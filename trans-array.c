/* Array transtation routines
   Copyright (C) 2000, 2001, 2002 Free Software Foundation, Inc.
   Contributed by Paul Brook <paul@nowt.org>
   and Steven Bosscher <s.bosscher@student.tudelft.nl>

This file is part of GNU G95.

GNU G95 is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU G95 is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU G95; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

/* trans-array.c-- Various array related code, including scalarization,
                   allocation, initialization and other support routines.  */

/* How the scalarizer works.
   In g95, array expressions use the same routines as scalar expressions.
   First, a Scalarization State (SS) chain is built.  This is done by walking
   the expression tree, and building a linear list of the terms in the
   expression.  As the tree is walked, scalar subexpressions are translated.
   The SS chain is built in reverse order, so must be reversed before being
   passed to the rest of the scalarizer.

   The scalarization parameters are stored in a g95_loopinfo structure.
   First the start and stride of each term is calculated by
   g95_conv_ss_startstride.  During this process the expressions for the array
   descriptors and data pointers are also translated.

   If the expression is an assignment, we must then resolve and dependencies.
   In fortran all the rhs values of an assignment must be evaluated before
   any assignments take place.  This can require a temporary array to store the
   values.  We also require a temporary when we are passing array expressions
   or vector subecripts as procedure parameters.

   Array sections are passed without copying to a temporary.  These use the
   scalarizer to determine the shape of the section.  The flag
   loop->array_parameter tells the scalarizer that the actual values and loop
   variables will not be required.

   The function g95_conv_loop_setup generates the scalarization setup code.
   It determines the range of the scalarizing loop variables.  If a temporary
   is required, this is created and initialized.  Code for scalar expressions
   taken outside the loop is also generated at this time.  Next the offset and
   scaling required to translate from loop variables to array indices for each
   term is calculated.

   A call to g95_start_scalarized_body marks the start of the scalarized
   expression.  This creates a scope and declares the loop variables.  Before
   calling this g95_make_ss_chain_used must be used to indicate which terms
   will be used inside this loop.

   The scalar g95_conv_* functions are then used to build the main body of the
   scalarization loop.  Scalarization loop variables and precalculated scalar
   values are automaticaly substituted.  Note that g95_advance_se_ss_chain
   must be used, rather than changing the se->ss directly.

   For assignment expressions requiring a temporary two sub loops are
   generated.  The first stores the result of the expression in the temporary,
   the second copies it to the result.  A Call to
   g95_trans_scalarized_loop_boundary marks the end of the main loop code and
   the start of the copying loop.  The temporary may be less than full rank.

   Finally g95_trans_scalarizing_loops is called to generate the impicit do
   loops. The loops are added to the pre chain of the loopinfo.  */

#include "config.h"
#include "system.h"
#include "tree.h"
#include "tree-simple.h"
#include <stdio.h>
#include "c-common.h"
#include "ggc.h"
#include "rtl.h"
#include "toplev.h"
#include "function.h"
#include "expr.h"
#include "real.h"
#include "flags.h"
#include <assert.h>
#include <gmp.h>
#define BACKEND_CODE
#include "g95.h"
#include "trans.h"
#include "trans-stmt.h"
#include "trans-types.h"
#include "trans-array.h"
#include "trans-const.h"

/* The contents of this structure aren't actualy used, just the address.  */
static g95_ss g95_ss_terminator_var;
g95_ss* g95_ss_terminator = &g95_ss_terminator_var;

unsigned HOST_WIDE_INT g95_stack_space_left;

/* Returns true if a variable of specified size should go on the stack.  */
int
g95_can_put_var_on_stack (tree size)
{
  unsigned HOST_WIDE_INT low;

  if (! INTEGER_CST_P (size))
    return 0;

  if (g95_option.max_stack_var_size < 0)
    return 1;

  if (TREE_INT_CST_HIGH (size) != 0)
    return 0;

  low = TREE_INT_CST_LOW (size);
  if (low > (unsigned HOST_WIDE_INT) g95_option.max_stack_var_size)
    return 0;

/* TODO: Set a per-function stack size limit.  */
#if 0
  /* We should be a bit more clever with array temps.  */
  if (g95_option.max_function_vars_size >= 0)
    {
      if (low > g95_stack_space_left)
        return 0;

      g95_stack_space_left -= low;
    }
#endif

  return 1;
}

static tree
g95_array_dataptr_type (tree desc)
{
  return (G95_TYPE_ARRAY_DATAPTR_TYPE (TREE_TYPE (desc)));
}

/* Build expressions to access the members of an array descriptor.
   It's surprisingly easy to mess up here, so never access
   an array descriptor by "brute force", always use these
   functions.  This also avoids problems if we change the format
   of an array descriptor.

   To understand these magic numbers, look at the comments
   before g95_build_array_type() in trans-types.c.

   The code within these defines should be the only code which knows the format
   of an array descriptor.

   Any code just needing to read obtain the bounds of an array should use
   g95_conv_array_* rather than the following functions as these will return
   know constant values, and work with arrays which do not have descriptors.

   Don't forget to #undef these!  */

#define DATA_FIELD 0
#define BASE_FIELD 1
#define DTYPE_FIELD 2
#define DIMENSION_FIELD 3

#define STRIDE_SUBFIELD 0
#define LBOUND_SUBFIELD 1
#define UBOUND_SUBFIELD 2

tree
g95_conv_descriptor_data (tree desc)
{
  tree field;
  tree type;

  type = TREE_TYPE (desc);
  assert (G95_DESCRIPTOR_TYPE_P (type));

  field = TYPE_FIELDS (type);
  assert (DATA_FIELD == 0);
  assert (field != NULL_TREE
          && TREE_CODE (TREE_TYPE (field)) == POINTER_TYPE
          && TREE_CODE (TREE_TYPE (TREE_TYPE (field))) == ARRAY_TYPE);

  return build (COMPONENT_REF, TREE_TYPE (field), desc, field);
}

tree
g95_conv_descriptor_base (tree desc)
{
  tree field;
  tree type;

  type = TREE_TYPE (desc);
  assert (G95_DESCRIPTOR_TYPE_P (type));

  field = g95_advance_chain (TYPE_FIELDS (type), BASE_FIELD);
  assert (field != NULL_TREE
          && TREE_CODE (TREE_TYPE (field)) == POINTER_TYPE
          && TREE_CODE (TREE_TYPE (TREE_TYPE (field))) == ARRAY_TYPE);

  return build (COMPONENT_REF, TREE_TYPE (field), desc, field);
}

tree
g95_conv_descriptor_dtype (tree desc)
{
  tree field;
  tree type;

  type = TREE_TYPE (desc);
  assert (G95_DESCRIPTOR_TYPE_P (type));

  field = g95_advance_chain (TYPE_FIELDS (type), DTYPE_FIELD);
  assert (field != NULL_TREE
          && TREE_TYPE (field) == g95_array_index_type);

  return build (COMPONENT_REF, TREE_TYPE (field), desc, field);
}

static tree
g95_conv_descriptor_dimension (tree desc, tree dim)
{
  tree field;
  tree type;
  tree tmp;

  assert (is_simple_val (dim));

  type = TREE_TYPE (desc);
  assert (G95_DESCRIPTOR_TYPE_P (type));

  field = g95_advance_chain (TYPE_FIELDS (type), DIMENSION_FIELD);
  assert (field != NULL_TREE
          && TREE_CODE (TREE_TYPE (field)) == ARRAY_TYPE
          && TREE_CODE (TREE_TYPE (TREE_TYPE (field))) == RECORD_TYPE);

  tmp = build (COMPONENT_REF, TREE_TYPE (field), desc, field);
  tmp = build (ARRAY_REF, TREE_TYPE (TREE_TYPE (tmp)), tmp, dim);
  return tmp;
}

tree
g95_conv_descriptor_stride (tree desc, tree dim)
{
  tree tmp;
  tree field;

  tmp = g95_conv_descriptor_dimension (desc, dim);
  field = TYPE_FIELDS (TREE_TYPE (tmp));
  field = g95_advance_chain (field, STRIDE_SUBFIELD);
  assert (field != NULL_TREE
          && TREE_TYPE (field) == g95_array_index_type);

  tmp = build (COMPONENT_REF, TREE_TYPE (field), tmp, field);
  return tmp;
}

tree
g95_conv_descriptor_lbound (tree desc, tree dim)
{
  tree tmp;
  tree field;

  tmp = g95_conv_descriptor_dimension (desc, dim);
  field = TYPE_FIELDS (TREE_TYPE (tmp));
  field = g95_advance_chain (field, LBOUND_SUBFIELD);
  assert (field != NULL_TREE
          && TREE_TYPE (field) == g95_array_index_type);

  tmp = build (COMPONENT_REF, TREE_TYPE (field), tmp, field);
  return tmp;
}

tree
g95_conv_descriptor_ubound (tree desc, tree dim)
{
  tree tmp;
  tree field;

  tmp = g95_conv_descriptor_dimension (desc, dim);
  field = TYPE_FIELDS (TREE_TYPE (tmp));
  field = g95_advance_chain (field, UBOUND_SUBFIELD);
  assert (field != NULL_TREE
          && TREE_TYPE (field) == g95_array_index_type);

  tmp = build (COMPONENT_REF, TREE_TYPE (field), tmp, field);
  return tmp;
}

/* Build a static constructor for an array descriptor.  Also allocates storage
   for the array data.  Return the offset.  */
static tree
g95_build_array_initializer (g95_symbol * sym)
{
  tree type;
  tree field;
  tree list;
  tree init;
  tree tmp;
  tree dim;
  tree data;
  mpz_t val;
  mpz_t s;
  mpz_t offset;
  int n;

  mpz_init (val);
  mpz_init_set_ui (s, 1);
  mpz_init_set_ui (offset, 0);

  dim = TYPE_FIELDS (TREE_TYPE (sym->backend_decl));
  dim = g95_advance_chain (dim, DIMENSION_FIELD);
  assert (TREE_CODE (TREE_TYPE (dim)) == ARRAY_TYPE);
  type = TREE_TYPE (TREE_TYPE (dim));
  init = NULL_TREE;
  /* All the dimension fields.  */
  for (n = 0; n < sym->as->rank; n++)
    {
      /* The list of fields is build reversed.  */
      field = TYPE_FIELDS (type);
      assert (STRIDE_SUBFIELD == 0 && LBOUND_SUBFIELD == 1
              && UBOUND_SUBFIELD == 2);
      list = tree_cons (field, g95_conv_mpz_to_tree (s, g95_array_index_kind),
                        NULL_TREE);
      assert (sym->as->lower[n]->expr_type == EXPR_CONSTANT);
      field = TREE_CHAIN (field);
      list = tree_cons (field, g95_conv_mpz_to_tree (
                sym->as->lower[n]->value.integer, g95_array_index_kind),
              list);
      field = TREE_CHAIN (field);
      assert (sym->as->upper[n]->expr_type == EXPR_CONSTANT);
      list = tree_cons (field, g95_conv_mpz_to_tree (
                sym->as->upper[n]->value.integer, g95_array_index_kind),
              list);

      list = build (CONSTRUCTOR, type, NULL_TREE, nreverse (list));
      init = tree_cons (NULL_TREE, list, init);


      mpz_mul (val, sym->as->lower[n]->value.integer, s);
      mpz_add (offset, offset, val);
      mpz_sub (val, sym->as->upper[n]->value.integer,
               sym->as->lower[n]->value.integer);
      mpz_add_ui (val, val, 1);
      mpz_mul (s, s, val);
    }
  init = build (CONSTRUCTOR, TREE_TYPE (dim), NULL_TREE, nreverse (init));
  list = tree_cons (dim, init, NULL_TREE);

  mpz_neg (offset, offset);
  /* Now allocate the data for the array.  */
  if (! sym->attr.use_assoc)
    {
      mpz_sub_ui (s, s, 1);
      tmp = g95_conv_mpz_to_tree (s, g95_array_index_kind);
      type = build_range_type (g95_array_index_type, integer_zero_node, tmp);
      type = build_array_type (g95_get_element_type (
              TREE_TYPE (sym->backend_decl)), type);
      data = create_tmp_var (type, "A");
      TREE_STATIC (data) = 1;
      TREE_ADDRESSABLE (data) = 1;
  /* TODO: array initializers.  */

  field = TYPE_FIELDS (TREE_TYPE (sym->backend_decl));
  field = g95_advance_chain (field, DTYPE_FIELD);
  init = G95_TYPE_ARRAY_DTYPE (TREE_TYPE (sym->backend_decl));
  list = tree_cons (field, init, list);

  /* We can't set the base member at compile time because it may not point to
     a valid address.  It may work with -O0, by really confuses -O2.  */
  field = TYPE_FIELDS (TREE_TYPE (sym->backend_decl));
  field = g95_advance_chain (field, BASE_FIELD);
  list = tree_cons (field, integer_zero_node, list);

  field = TYPE_FIELDS (TREE_TYPE (sym->backend_decl));
  assert (DATA_FIELD == 0);
  init = build1 (ADDR_EXPR, TREE_TYPE (field), data);
  list = tree_cons (field, init, list);

  init = build (CONSTRUCTOR, TREE_TYPE (sym->backend_decl), NULL_TREE, list);
  TREE_CONSTANT (init) = 1;
  DECL_INITIAL (sym->backend_decl) = init;
    }

  tmp = g95_conv_mpz_to_tree (offset, g95_array_index_kind);

  mpz_clear (val);
  mpz_clear (s);
  mpz_clear (offset);

  return tmp;
}

/* Cleanup those #defines.  */
#undef DATA_FIELD
#undef BASE_FIELD
#undef DTYPE_FIELD
#undef DIMENSION_FIELD
#undef STRIDE_SUBFIELD
#undef LBOUND_SUBFIELD
#undef UBOUND_SUBFIELD

/* Mark a SS chain as used.  Flags specifies in which loops the SS is used.
   flags & 1 = Main loop body.
   flags & 2 = temp copy loop.  */
void
g95_mark_ss_chain_used (g95_ss * ss, unsigned flags)
{
  for (; ss != g95_ss_terminator; ss = ss->next)
    ss->useflags = flags;
}

static void g95_free_ss (g95_ss *);

/* Free a g95_ss chain.  */
static void
g95_free_ss_chain (g95_ss * ss)
{
  g95_ss *next;

  while (ss != g95_ss_terminator)
    {
      assert (ss != NULL);
      next = ss->next;
      g95_free_ss (ss);
      ss = next;
    }
}

/* Free a SS.  */
static void
g95_free_ss (g95_ss * ss)
{
  int n;

  switch (ss->type)
    {
    case G95_SS_SECTION:
    case G95_SS_VECTOR:
      for (n = 0; n < G95_MAX_DIMENSIONS; n++)
        {
          if (ss->data.info.subscript[n])
            g95_free_ss_chain (ss->data.info.subscript[n]);
        }
      break;

    default:
      break;
    }

  g95_free (ss);
}

/* Free all the SS associated with a loop.  */
void
g95_cleanup_loop (g95_loopinfo * loop)
{
  g95_ss *ss;
  g95_ss *next;

  ss = loop->ss;
  while (ss != g95_ss_terminator)
    {
      assert (ss != NULL);
      next = ss->loop_chain;
      g95_free_ss (ss);
      ss = next;
    }
}

/* Associate a SS chain with a loop.  */
void
g95_add_ss_to_loop (g95_loopinfo * loop, g95_ss * head)
{
  g95_ss *ss;

  if (head == g95_ss_terminator)
    return;

  ss = head;
  for (; ss && ss != g95_ss_terminator; ss = ss->next)
    {
      if (ss->next == g95_ss_terminator)
        ss->loop_chain = loop->ss;
      else
        ss->loop_chain = ss->next;
    }
  assert (ss == g95_ss_terminator);
  loop->ss = head;
}

/* Initialize an array of character strings.  */
static tree
g95_trans_init_character_array (tree descriptor, tree pointer, tree count)
{
  tree head;
  tree tail;
  tree pstr;
  tree stype;
  tree loopvar;
  tree len;
  tree tmp;
  tree stmt;
  tree body;
  tree body_tail;
  tree init;
  tree cond;
  tree inc;

  assert (TREE_CODE (descriptor) == VAR_DECL);
  g95_start_stmt();
  head = tail = NULL_TREE;

  /* The character string type. Should be a pointer to an array of chars.
     The passed pointer is a pointer to an array of character strings.  */
  stype = TREE_TYPE (TREE_TYPE (TREE_TYPE (pointer)));
  pstr = create_tmp_var (stype, "pstr");
  loopvar = create_tmp_var (g95_array_index_type, "loop");

  /* Character strings are one-based, so the next string starts at len+1.  */
  len = G95_DECL_STRING_LENGTH (descriptor);
  len = build (PLUS_EXPR, TREE_TYPE (len), len, integer_one_node);
  len = g95_simple_fold (len, &head, &tail, NULL);

  /* The First string starts immediately after the block of pointers.  */
  tmp = build1 (INDIRECT_REF, TREE_TYPE (TREE_TYPE (pointer)), pointer);
  tmp = build (ARRAY_REF, stype, tmp, count);
  tmp = build1 (ADDR_EXPR, stype, tmp);
  tmp = build (MODIFY_EXPR, stype, pstr, tmp);
  stmt = build_stmt (EXPR_STMT, tmp);
  g95_add_stmt_to_list (&head, &tail, stmt, stmt);

  /* The body of the loop.  */
  g95_start_stmt ();
  body = body_tail = NULL_TREE;

  tmp = build1 (INDIRECT_REF, TREE_TYPE (TREE_TYPE (pointer)), pointer);
  tmp = build (ARRAY_REF, stype, tmp, loopvar);
  tmp = build (MODIFY_EXPR, stype, tmp, pstr);
  stmt = build_stmt (EXPR_STMT, tmp);
  g95_add_stmt_to_list (&body, &body_tail, stmt, stmt);

  tmp = build1 (INDIRECT_REF, TREE_TYPE (stype), pstr);
  tmp = build (ARRAY_REF, TREE_TYPE (TREE_TYPE (tmp)), tmp, len);
  tmp = build1 (ADDR_EXPR, stype, tmp);
  tmp = build (MODIFY_EXPR, stype, pstr, tmp);
  stmt = build_stmt (EXPR_STMT, tmp);
  g95_add_stmt_to_list (&body, &body_tail, stmt, stmt);
  body = g95_finish_stmt (body, body_tail);

  /* Build the loop.  */
  init = build (MODIFY_EXPR, TREE_TYPE (loopvar), loopvar, integer_zero_node);
  init = build_stmt (EXPR_STMT, init);
  cond = build (LT_EXPR, boolean_type_node, loopvar, count);
  inc = build (PLUS_EXPR, TREE_TYPE (loopvar), loopvar, integer_one_node);
  inc = build (MODIFY_EXPR, TREE_TYPE (inc), loopvar, inc);

  stmt = build_stmt (FOR_STMT, init, cond, inc, body);
  g95_add_stmt_to_list (&head, &tail, stmt, stmt);

  stmt = g95_finish_stmt (head, tail);
  return stmt;
}

/* Generate code to allocate an array temporary, or create a variable to
   hold the data.  */
static void
g95_trans_allocate_array_storage (g95_loopinfo * loop, g95_ss_info * info,
                                  tree size, tree nelem)
{
  tree tmp;
  tree stmt;
  tree args;
  tree desc;
  tree data;

  desc = info->descriptor;
  data = g95_conv_descriptor_data (desc);
  if (g95_can_put_var_on_stack (size))
    {
      /* Make a temporary variable to hold the data.  */
      tmp = g95_get_stack_array_type (size);
      tmp = create_tmp_var (tmp, "A");
      tmp = build1 (ADDR_EXPR, TREE_TYPE (data), tmp);
      info->data = g95_simple_fold (tmp, &loop->pre, &loop->pre_tail, NULL);

      tmp = build (MODIFY_EXPR, TREE_TYPE (data), data, info->data);
      stmt = build_stmt (EXPR_STMT, tmp);
      g95_add_stmt_to_pre (loop, stmt, stmt);
      info->pdata = NULL_TREE;
    }
  else
    {
      /* Allocate memory to hold the data.  */
      tmp = build1 (ADDR_EXPR, ppvoid_type_node, data);
      info->pdata = create_tmp_var (ppvoid_type_node, "ptr");
      tmp = build (MODIFY_EXPR, TREE_TYPE (info->pdata), info->pdata, tmp);
      stmt = build_stmt (EXPR_STMT, tmp);
      g95_add_stmt_to_pre (loop, stmt, stmt);

      /* Build a call to allocate storage.  */
      args = g95_chainon_list (NULL_TREE, info->pdata);
      args = g95_chainon_list (args, size);

      if (g95_array_index_kind == 4)
        tmp = gfor_fndecl_internal_malloc;
      else if (g95_array_index_kind == 8)
        tmp = gfor_fndecl_internal_malloc64;
      else
        abort();
      tmp = g95_build_function_call (tmp, args);
      stmt = build_stmt (EXPR_STMT, tmp);
      g95_add_stmt_to_pre (loop, stmt, stmt);

      info->data = g95_simple_fold (data, &loop->pre, &loop->pre_tail, NULL);
    }

  /* The offset is zero because we create temporaries with a zero
     lower bound.  */
  tmp = g95_conv_descriptor_base (desc);
  tmp = build (MODIFY_EXPR, TREE_TYPE (tmp), tmp, info->data);
  stmt = build_stmt (EXPR_STMT, tmp);
  g95_add_stmt_to_pre (loop, stmt, stmt);

  if (nelem)
    {
      stmt = g95_trans_init_character_array (desc, info->data, nelem);
      g95_add_stmt_to_pre (loop, stmt, NULL_TREE);
    }

  if (info->pdata != NULL_TREE)
    {
      /* Free the temporary.  */
      tmp = g95_chainon_list (NULL_TREE, info->pdata);
      tmp = g95_build_function_call (gfor_fndecl_internal_free, tmp);
      stmt = build_stmt (EXPR_STMT, tmp);
      g95_add_stmt_to_post (loop, stmt, stmt);
    }
}

/* Generate code to allocate and initialize the descriptor for a temporary
   array.  Fills in info->descriptor and info->data.  Also adjusts the loop
   variables to be zero-based.  Returns the size of the array.  */
/* TODO: Use destination for simple assignments.  */
tree
g95_trans_allocate_temp_array (g95_loopinfo * loop, g95_ss_info * info,
                               tree eltype, tree string_length)
{
  tree type;
  tree desc;
  tree stmt;
  tree tmp;
  tree tmpvar;
  tree size;
  tree sizevar;
  tree nelem;
  int n;
  int dim;

  assert (info->dimen > 0);
  /* Set the lower bound to zero.  */
  for (dim = 0; dim < info->dimen; dim++)
    {
      n = loop->order[dim];
      if (n < loop->temp_dim)
        assert (integer_zerop (loop->from[n]));
      else
        {
          tmp = build (MINUS_EXPR, g95_array_index_type, loop->to[n],
                     loop->from[n]);
          loop->to[n] =
            g95_simple_fold (tmp, &loop->pre, &loop->pre_tail, NULL);
          loop->from[n] = integer_zero_node;
        }

      info->delta[dim] = integer_zero_node;
      info->start[dim] = integer_zero_node;
      info->stride[dim] = integer_one_node;
      info->dim[dim] = dim;
    }

  /* Initialise the descriptor.  */
  type = g95_get_array_type_bounds(eltype, info->dimen, loop->from, loop->to);
  desc = create_tmp_var (type, "atmp");
  G95_DECL_PACKED_ARRAY (desc) = 1;
  if (string_length)
    {
      g95_allocate_lang_decl (desc);
      G95_DECL_STRING (desc) = 1;
      G95_DECL_STRING_LENGTH (desc) = string_length;
    }

  info->descriptor = desc;
  tmpvar = NULL_TREE;
  sizevar = NULL_TREE;
  size = integer_one_node;

  /* Fill in the array dtype.  */
  tmp = g95_conv_descriptor_dtype (desc);
  tmp = build (MODIFY_EXPR, TREE_TYPE (tmp), tmp,
               G95_TYPE_ARRAY_DTYPE (TREE_TYPE (desc)));
  stmt = build_stmt (EXPR_STMT, tmp);
  g95_add_stmt_to_pre (loop, stmt, stmt);

  /* Fill in the bounds and stride.  This is a packed array, so:
      size = 1;
      for (n = 0; n < rank; n++)
        {
          stride[n] = size
          delta = ubound[n] + 1 - lbound[n];
          size = size * delta;
        }
        size = size * sizeof(element);  */
  for (n = 0; n < info->dimen; n++)
    {
      tmp = g95_conv_descriptor_stride (desc, g95_rank_cst[n]);
      tmp = build (MODIFY_EXPR, TREE_TYPE (tmp), tmp, size);
      stmt = build_stmt (EXPR_STMT, tmp);
      g95_add_stmt_to_pre (loop, stmt, stmt);

      tmp = g95_conv_descriptor_lbound (desc, g95_rank_cst[n]);
      tmp = build (MODIFY_EXPR, TREE_TYPE (tmp), tmp, integer_zero_node);
      stmt = build_stmt (EXPR_STMT, tmp);
      g95_add_stmt_to_pre (loop, stmt, stmt);

      tmp = g95_conv_descriptor_ubound (desc, g95_rank_cst[n]);
      tmp = build (MODIFY_EXPR, TREE_TYPE (tmp), tmp, loop->to[n]);
      stmt = build_stmt (EXPR_STMT, tmp);
      g95_add_stmt_to_pre (loop, stmt, stmt);

      tmp = build (PLUS_EXPR, g95_array_index_type,
          loop->to[n], integer_one_node);
      tmp = g95_simple_fold (tmp, &loop->pre, &loop->pre_tail, &tmpvar);

      tmp = build (MULT_EXPR, g95_array_index_type, size, tmp);
      size = g95_simple_fold_tmp (tmp, &loop->pre, &loop->pre_tail,
                                 &sizevar);
    }

  if (string_length)
    {
      nelem = g95_simple_fold_tmp (size, &loop->pre, &loop->pre_tail, NULL);
      tmp = build (MULT_EXPR, g95_array_index_type, nelem, string_length);
      tmp = g95_simple_fold (tmp, &loop->pre, &loop->pre_tail, NULL);
      size = build (PLUS_EXPR, g95_array_index_type, size, tmp);
      size = g95_simple_fold (size, &loop->pre, &loop->pre_tail, &sizevar);
    }
  else
    nelem = NULL_TREE;

  /* Get the size of the array.  */
  tmp = build (MULT_EXPR, g95_array_index_type, size,
      TYPE_SIZE_UNIT (g95_get_element_type (type)));
  size = g95_simple_fold (tmp, &loop->pre, &loop->pre_tail, &sizevar);

  g95_trans_allocate_array_storage (loop, info, size, nelem);

  if (info->dimen > loop->temp_dim)
    loop->temp_dim = info->dimen;

  return size;
}

/* Assign the values to the elements of an array constructor.  */
static void
g95_trans_array_constructor_value (tree * phead, tree * ptail, tree type,
    tree pointer, g95_constructor * c, tree * poffset, tree * offsetvar)
{
  tree tmp;
  tree stmt;
  tree ref;
  tree body;
  tree body_tail;
  g95_se se;

  for (; c; c = c->next)
    {
      /* If this is an iterator, the offset must be a variable.  */
      if (c->iterator && INTEGER_CST_P (*poffset))
        {
          /* We should have already created the offset variable.  We cannot
             create it here because we may be in an inner scopde.  */
          assert (*offsetvar != NULL_TREE);
          tmp = build (MODIFY_EXPR, g95_array_index_type, *offsetvar, *poffset);
          stmt = build_stmt (EXPR_STMT, tmp);
          g95_add_stmt_to_list (phead, ptail, stmt, stmt);
          *poffset = *offsetvar;
          TREE_USED (*offsetvar) = 1;
        }

      g95_start_stmt();

      body = body_tail = NULL_TREE;

      if (c->expr->expr_type == EXPR_ARRAY)
        {
          /* Array constructors can be nested.  */
          g95_trans_array_constructor_value (&body, &body_tail, type, pointer,
              c->expr->value.constructor.head, poffset, offsetvar);
        }
      else
        {
          g95_constructor *p;
          HOST_WIDE_INT n;

          p = c;
          n = 0;
          while (p && ! (p->iterator || p->expr->expr_type != EXPR_CONSTANT))
            {
              if (p->expr->rank > 0)
                g95_todo_error ("Array expressions in constructors");
              p = p->next;
              n++;
            }
          if (n < 4)
            {
              /* A single scalar value.  */
              g95_init_se (&se, NULL);
              g95_conv_simple_rhs (&se, c->expr);
              g95_add_stmt_to_list (&body, &body_tail, se.pre, se.pre_tail);

              ref = build1 (INDIRECT_REF, TREE_TYPE (TREE_TYPE (pointer)),
                            pointer);
              ref = build (ARRAY_REF, type, ref, *poffset);
              tmp = build (MODIFY_EXPR, TREE_TYPE (ref), ref, se.expr);
              stmt = build_stmt (EXPR_STMT, tmp);
              g95_add_stmt_to_list (&body, &body_tail, stmt, stmt);
              g95_add_stmt_to_list (&body, &body_tail, se.post, se.post_tail);

              *poffset = build (PLUS_EXPR, g95_array_index_type, *poffset,
                              integer_one_node);
              *poffset = g95_simple_fold_tmp (*poffset, &body, &body_tail,
                                              offsetvar);
            }
          else
            {
              /* Collect multiple scalar constants into a constructor.  */
              tree list;
              tree init;
              tree bound;
              tree tmptype;

              p = c;
              list = NULL_TREE;
              while (p && ! (p->iterator
                             || p->expr->expr_type != EXPR_CONSTANT))
                {
                  g95_init_se (&se, NULL);
                  g95_conv_constant (&se, p->expr);
                  list = tree_cons (NULL_TREE, se.expr, list);
                  c = p;
                  p = p->next;
                }

              bound = build_int_2 (n - 1, 0);
              tmptype = build_range_type (g95_array_index_type,
                                          integer_zero_node, bound);
              tmptype = build_array_type (type, tmptype);

              init = build (CONSTRUCTOR, tmptype, NULL_TREE, nreverse (list));
              TREE_CONSTANT (init) = 1;
              TREE_STATIC (init) = 1;

              /* Assign the values.  */
              tmp = build1 (INDIRECT_REF, TREE_TYPE (TREE_TYPE (pointer)),
                            pointer);
              tmp = build (ARRAY_REF, type, tmp, *poffset);
              tmp = build1 (ADDR_EXPR, build_pointer_type (tmptype), tmp);
              tmp = g95_simple_fold (tmp, &body, &body_tail, NULL);
              tmp = build1 (INDIRECT_REF, tmptype, tmp);
              tmp = build (MODIFY_EXPR, tmptype, tmp, init);
              stmt = build_stmt (EXPR_STMT, tmp);
              g95_add_stmt_to_list (&body, &body_tail, stmt, stmt);

              bound = build_int_2 (n, 0);
              tmp = build (PLUS_EXPR, g95_array_index_type, *poffset, bound);
              *poffset = g95_simple_fold (tmp, &body, &body_tail, offsetvar);
            }
        }

      /* The frontend will already have done any expansions.  */
      if (c->iterator)
        {
          tree start;
          tree end;
          tree step;
          tree loopvar;

          body = g95_finish_stmt (body, body_tail);

          g95_init_se (&se, NULL);
          g95_conv_simple_lhs (&se, c->iterator->var);
          g95_add_stmt_to_list (phead, ptail, se.pre, se.pre_tail);
          assert (se.post == NULL_TREE);
          loopvar = se.expr;

          g95_init_se (&se, NULL);
          g95_conv_simple_rhs (&se, c->iterator->start);
          g95_add_stmt_to_list (phead, ptail, se.pre, se.pre_tail);
          start = build (MODIFY_EXPR, TREE_TYPE (loopvar), loopvar, se.expr);
          start = build_stmt (EXPR_STMT, start);

          g95_init_se (&se, NULL);
          g95_conv_simple_val (&se, c->iterator->end);
          g95_add_stmt_to_list (phead, ptail, se.pre, se.pre_tail);
          end = build (LE_EXPR, boolean_type_node, loopvar, se.expr);

          g95_init_se (&se, NULL);
          g95_conv_simple_val (&se, c->iterator->step);
          g95_add_stmt_to_list (phead, ptail, se.pre, se.pre_tail);
          step = build (PLUS_EXPR, TREE_TYPE (loopvar), loopvar, se.expr);
          step = build (MODIFY_EXPR, TREE_TYPE (loopvar), loopvar, step);

          body = build_stmt (FOR_STMT, start, end, step, body);
          body_tail = body;
        }
      else if (getdecls () == NULL_TREE)
        {
          g95_merge_stmt ();
        }
      else
        {
          body = g95_finish_stmt (body, body_tail);
          body_tail = NULL_TREE;
        }
      g95_add_stmt_to_list (phead, ptail, body, body_tail);
    }
}

/* Get the size of an expression.  Returns -1 if the size isn't constant.
   Implied do loops with non-constant bounds are tricky because we must only
   evaluate the bounds once.  */
static void
g95_get_array_cons_size (mpz_t *size, g95_constructor * c)
{
  g95_iterator *i;
  mpz_t val;
  mpz_t len;

  mpz_set_ui (*size, 0);
  mpz_init (len);
  mpz_init (val);

  for (; c; c = c->next)
    {
      if (c->expr->expr_type == EXPR_ARRAY)
        {
          g95_get_array_cons_size (&len, c->expr->value.constructor.head);
          if (mpz_sgn (len) < 0)
            {
              mpz_set (*size, len);
              mpz_clear (len);
              mpz_clear (val);
              return;
            }
        }
      else
        {
          if (c->expr->rank > 0)
            g95_todo_error ("Array expressions in constructors");
          mpz_set_ui (len, 1);
        }

      if (c->iterator)
        {
          i = c->iterator;

          if (i->start->expr_type != EXPR_CONSTANT
              || i->end->expr_type != EXPR_CONSTANT
              || i->step->expr_type != EXPR_CONSTANT)
            {
              mpz_set_si (*size, -1);
              mpz_clear (len);
              mpz_clear (val);
              return;
            }

          mpz_add (val, i->end->value.integer, i->start->value.integer);
          mpz_tdiv_q (val, val, i->step->value.integer);
          mpz_add_ui (val, val, 1);
          mpz_mul (len, len, val);
        }
      mpz_add (*size, *size, len);
    }
  mpz_clear (len);
  mpz_clear (val);
}

/* Array constructors are handled by constructing a temporary, then using that
   within the scalarization loop.  This is not optimal, seems by far the
   simplest method.  */
static void
g95_trans_array_constructor (g95_loopinfo * loop, g95_ss * ss)
{
  tree offset;
  tree offsetvar;
  tree desc;
  tree size;
  tree type;

  if (ss->expr->ts.type == BT_CHARACTER)
    g95_todo_error ("Character string array constructors");
  type = g95_typenode_for_spec (&ss->expr->ts);
  ss->data.info.dimen = loop->dimen;
  size = g95_trans_allocate_temp_array (loop, &ss->data.info, type, NULL_TREE);

  desc = ss->data.info.descriptor;
  offset = integer_zero_node;
  offsetvar = create_tmp_alias_var (g95_array_index_type, "offset");
  TREE_USED (offsetvar) = 0;
  g95_trans_array_constructor_value (&loop->pre, &loop->pre_tail, type,
      ss->data.info.data, ss->expr->value.constructor.head, &offset,
      &offsetvar);

  if (TREE_USED (offsetvar))
    pushdecl (offsetvar);
  else
    assert (INTEGER_CST_P (offset));
#if 0
  /* Disable bound checking for now cos it's probably broken.  */
  if (flag_bounds_check)
    {
      tree tmp;

      size = build (TRUNC_DIV_EXPR, g95_array_index_type, size,
                    TYPE_SIZE_UNIT (type));
      size = g95_simple_fold (size, &loop->pre, &loop->pre_tail, NULL);
      tmp = build (NE_EXPR, boolean_type_node, size, offset);
      g95_trans_runtime_check (tmp, g95_strconst_bounds,
                               &loop->pre, &loop->pre_tail);
    }
#endif
}

/* Add the pre and post chains for all the scalar expressions in a SS chain
   to loop.  This is called after the loop parameters have been calculated,
   but before the actual scalarizing loops.  */
/*GCC ARRAYS*/
static void
g95_add_loop_ss_code (g95_loopinfo * loop, g95_ss * ss)
{
  g95_se se;
  int n;

  assert (ss != NULL);

  for (; ss != g95_ss_terminator; ss = ss->loop_chain)
    {
      assert (ss);

      switch (ss->type)
        {
        case G95_SS_SCALAR:
          /* Scalar expression.  Evaluate this now.  This includes elemental
             dimension indices, but not array section bounds.  */
          g95_init_se (&se, NULL);
          g95_conv_simple_val (&se, ss->expr);
          g95_add_stmt_to_pre (loop, se.pre, se.pre_tail);
          g95_add_stmt_to_post (loop, se.post, se.post_tail);

          ss->data.scalar.expr = se.expr;
          ss->data.scalar.string_length = se.string_length;
          break;

        case G95_SS_REFERENCE:
          /* Scalar reference.  Evaluate this now.  This includes elemental
             dimension indices, but not array section bounds.  */
          g95_init_se (&se, NULL);
          g95_conv_simple_reference (&se, ss->expr);
          g95_add_stmt_to_pre (loop, se.pre, se.pre_tail);
          g95_add_stmt_to_post (loop, se.post, se.post_tail);

          ss->data.scalar.expr = se.expr;
          ss->data.scalar.string_length = se.string_length;
          break;

        case G95_SS_SECTION:
        case G95_SS_VECTOR:
          /* Scalarized expression.  Evaluate any scalar subscripts.  */
          for (n = 0; n < G95_MAX_DIMENSIONS; n++)
            {
              /* Add the expressions for scalar subscripts.  */
              if (ss->data.info.subscript[n])
                g95_add_loop_ss_code (loop, ss->data.info.subscript[n]);
            }
          break;

        case G95_SS_INTRINSIC:
          g95_add_intrinsic_ss_code (loop, ss);
          break;

        case G95_SS_FUNCTION:
          /* Array function return value.  We call the function and save its
             result in a temporary for use inside the loop.  */
          g95_init_se (&se, NULL);
          se.loop = loop;
          se.ss = ss;
          g95_conv_simple_rhs (&se, ss->expr);
          g95_add_stmt_to_pre (loop, se.pre, se.pre_tail);
          g95_add_stmt_to_post (loop, se.post, se.post_tail);
          break;

        case G95_SS_CONSTRUCTOR:
          g95_trans_array_constructor(loop, ss);
          break;

        default:
          abort();
        }
    }
}

/* Reverse a SS chain.  */
g95_ss *
g95_reverse_ss (g95_ss *ss)
{
  g95_ss *next;
  g95_ss *head;

  assert (ss != NULL);

  head = g95_ss_terminator;
  while (ss != g95_ss_terminator)
    {
      next = ss->next;
      assert (next != NULL); /* Check we didn't somehow break the chain.  */
      ss->next = head;
      head = ss;
      ss = next;
    }

  return (head);
}

/* Translate expressions for the descriptor and data pointer of a SS.  */
/*GCC ARRAYS*/
static void
g95_conv_ss_descriptor (g95_loopinfo * loop, g95_ss * ss)
{
  g95_se se;

  /* Get the descriptor for the array to be scalarized.  */
  assert (ss->expr->expr_type == EXPR_VARIABLE);
  g95_init_se (&se, NULL);
  se.descriptor_only = 1;
  g95_conv_simple_rhs (&se, ss->expr);
  assert (se.post == NULL_TREE);
  g95_add_stmt_to_pre (loop, se.pre, se.pre_tail);
  ss->data.info.descriptor = se.expr;

  if (! loop->array_parameter)
    {
      /* Also the data pointer.  */
      se.expr = g95_conv_array_base (se.expr);
      ss->data.info.data =
        g95_simple_fold (se.expr, &loop->pre, &loop->pre_tail, NULL);
    }
}

/* Initialise a g95_loopinfo structure.  */
void
g95_init_loopinfo (g95_loopinfo * loop)
{
  int n;

  memset (loop, 0, sizeof(g95_loopinfo));

  /* Initialy scalarize in order.  */
  for (n = 0; n < G95_MAX_DIMENSIONS; n++)
    loop->order[n] = n;

  loop->ss = g95_ss_terminator;
}

/* Copies the loop variable info to a g95_se sructure. Does not copy the SS
   chain.  */
void
g95_copy_loopinfo_to_se (g95_se * se, g95_loopinfo * loop)
{
  se->loop = loop;
}

/* Returns 2 if an array is packed, 1 if the first dimension of an array is
   packed and 0 is an array is not packed.  If the state of the array data
   cannot be determined at compile time (eg. pointers) this will return 0.  */
static int
g95_array_is_packed (tree descriptor)
{
  tree decl;

  decl = descriptor;

  /* Arrays without descriptors are always packed.  */
  if (G95_ARRAY_TYPE_P (TREE_TYPE (decl)))
    return 2;

  assert (G95_DESCRIPTOR_TYPE_P (TREE_TYPE (decl)));

  /* Derived type array components are always packed.  */
  if (TREE_CODE (decl) == COMPONENT_REF)
    return 2;

  /* If this is a pointer dereference then get the pointer decl.  */
  if (TREE_CODE (decl) == INDIRECT_REF)
    decl = TREE_OPERAND (decl, 0);

  assert (TREE_CODE (decl) == VAR_DECL
          || TREE_CODE (decl) == PARM_DECL);

  if (G95_DECL_PACKED_ARRAY (decl))
    return 2;
  else if (G95_DECL_PARTIAL_PACKED_ARRAY (decl))
    return 1;

  return 0;
}

tree
g95_conv_array_data (tree descriptor)
{
  if (G95_ARRAY_TYPE_P (TREE_TYPE (descriptor)))
    {
      if (TREE_CODE (descriptor) == VAR_DECL)
        TREE_ADDRESSABLE (descriptor) = 1;
      if (TREE_CODE (descriptor) == INDIRECT_REF)
        return TREE_OPERAND (descriptor, 0);
      return build1 (ADDR_EXPR, build_pointer_type (TREE_TYPE (descriptor)),
                     descriptor);
    }
  else
    return g95_conv_descriptor_data (descriptor);
}

tree
g95_conv_array_base (tree descriptor)
{
  tree tmp;
  tree type;

  if (G95_ARRAY_TYPE_P (TREE_TYPE (descriptor)))
    {
      TREE_ADDRESSABLE (descriptor) = 1;
      type = TREE_TYPE (descriptor);
      tmp = build (ARRAY_REF, g95_get_element_type (type), descriptor,
                   G95_TYPE_ARRAY_OFFSET (type));
      tmp = build1 (ADDR_EXPR, g95_array_dataptr_type (descriptor), tmp);
      return tmp;
    }
  else
    return g95_conv_descriptor_base (descriptor);
}

/* Get an expression for the array stride.  Either a constant or a
   COMPONENT_REF.  */
tree
g95_conv_array_stride (tree descriptor, int dim)
{
  tree tmp;
  tree type;
  int packed;

  type = TREE_TYPE (descriptor);
  packed = g95_array_is_packed (descriptor);
  if (dim == 0 && packed > 0)
    return integer_one_node;

  /* Use the array size, if known.  */
  if (packed == 2)
    {
      tmp = G95_TYPE_ARRAY_STRIDE (type, dim);
      if (tmp != NULL_TREE)
        {
          assert (INTEGER_CST_P (tmp));
          return tmp;
        }
    }

  tmp = g95_conv_descriptor_stride (descriptor, g95_rank_cst[dim]);
  return tmp;
}

/* Like g95_conv_array_stride, but for the lower bound.  */
tree
g95_conv_array_lbound (tree descriptor, int dim)
{
  tree tmp;
  tree type;

  type = TREE_TYPE (descriptor);

  tmp = G95_TYPE_ARRAY_LBOUND (type, dim);
  if (tmp != NULL_TREE)
    {
      assert (INTEGER_CST_P (tmp));
      return tmp;
    }

  tmp = g95_conv_descriptor_lbound (descriptor, g95_rank_cst[dim]);
  return tmp;
}

/* Like g95_conv_array_stride, but for the upper bound.  */
tree
g95_conv_array_ubound (tree descriptor, int dim)
{
  tree tmp;
  tree type;

  type = TREE_TYPE (descriptor);

  tmp = G95_TYPE_ARRAY_UBOUND (type, dim);
  if (tmp != NULL_TREE)
    {
      assert (INTEGER_CST_P (tmp));
      return tmp;
    }

  tmp = g95_conv_descriptor_ubound (descriptor, g95_rank_cst[dim]);
  return tmp;
}

/* Translate an array reference.  The descriptor should be in se->expr.
   Do not use this function, it wil be removed soon.  */
/*GCC ARRAYS*/
static void
g95_conv_array_index_ref (g95_se * se, tree pointer, tree * indices,
                          int dimen)
{
  tree array;
  tree tmp;
  tree index;
  tree tmpvar;
  tree indexvar;
  tree fault;
  tree boundvar;
  tree condvar;
  tree faultvar;
  int n;

  array = build1 (INDIRECT_REF, TREE_TYPE (TREE_TYPE (pointer)), pointer);

  tmpvar = NULL_TREE;
  indexvar = NULL_TREE;

  fault = integer_zero_node;
  faultvar = NULL_TREE;
  boundvar = NULL_TREE;
  condvar = NULL_TREE;
  index = integer_zero_node;
  for (n = 0; n < dimen; n++)
    {
      /* index = index + stride[n]*indices[n] */
      tmp = g95_conv_array_stride (se->expr, n);
      tmp = g95_simple_fold (tmp, &se->pre, &se->pre_tail, &tmpvar);
      tmp = build (MULT_EXPR, g95_array_index_type, indices[n], tmp);
      tmp = g95_simple_fold (tmp, &se->pre, &se->pre_tail, &tmpvar);

      index = build (PLUS_EXPR, g95_array_index_type, index, tmp);
      if (n == dimen - 1)
        index = g95_simple_fold (index, &se->pre, &se->pre_tail, &indexvar);
      else
        index = g95_simple_fold_tmp (index, &se->pre, &se->pre_tail, &indexvar);
    }

  /* Result = data[index].  */
  assert (TREE_CODE (TREE_TYPE (array)) == ARRAY_TYPE);
  tmp = build (ARRAY_REF, TREE_TYPE( TREE_TYPE (array)), array, index);

  /* Check we've used the correct number of dimensions.  */
  assert (TREE_CODE (TREE_TYPE (tmp)) != ARRAY_TYPE);

  se->expr = tmp;
}

/* Generate code to perform an array index bound check.  */
static void
g95_trans_array_bound_check (g95_se * se, tree descriptor, tree index, int n)
{
  tree cond;
  tree condvar;
  tree fault;
  tree faultvar;
  tree tmp;
  tree tmpvar;

  if (! flag_bounds_check)
    return;

  fault = integer_zero_node;
  faultvar = NULL_TREE;
  tmpvar = NULL_TREE;
  condvar = NULL_TREE;

  /* Check lower bound.  */
  tmp = g95_conv_array_lbound (descriptor, n);
  tmp = g95_simple_fold (tmp, &se->pre, &se->pre_tail, &tmpvar);
  cond = build (LT_EXPR, boolean_type_node, index, tmp);
  cond = g95_simple_fold (cond, &se->pre, &se->pre_tail, &condvar);
  fault = build (TRUTH_OR_EXPR, boolean_type_node, fault, cond);
  fault = g95_simple_fold_tmp (fault, &se->pre, &se->pre_tail, &faultvar);
  /* Check upper bound.  */
  tmp = g95_conv_array_ubound (descriptor, n);
  tmp = g95_simple_fold (tmp, &se->pre, &se->pre_tail, &tmpvar);
  cond = build (GT_EXPR, boolean_type_node, index, tmp);
  cond = g95_simple_fold (cond, &se->pre, &se->pre_tail, &condvar);
  fault = build (TRUTH_OR_EXPR, boolean_type_node, fault, cond);
  fault = g95_simple_fold (fault, &se->pre, &se->pre_tail, &faultvar);

  g95_trans_runtime_check (fault, g95_strconst_fault, &se->pre, &se->pre_tail);
}

/* A reference to an array vector subscript.  Uses recursion to handle nested
   vector subscripts.  */
static tree
g95_conv_vector_array_index (g95_se * se, tree index, g95_ss * ss, tree * pvar)
{
  tree descsave;
  tree indices[G95_MAX_DIMENSIONS];
  g95_array_ref *ar;
  g95_ss_info *info;
  int n;

  assert (ss && ss->type == G95_SS_VECTOR);

  /* Save the descriptor.  */
  descsave = se->expr;
  info = &ss->data.info;
  se->expr = info->descriptor;

  ar = &info->ref->u.ar;
  for (n = 0; n < ar->dimen; n++)
   {
     switch (ar->dimen_type[n])
       {
       case DIMEN_ELEMENT:
        assert (info->subscript[n] != g95_ss_terminator
                && info->subscript[n]->type == G95_SS_SCALAR);
        indices[n] = info->subscript[n]->data.scalar.expr;
        break;

      case DIMEN_RANGE:
        indices[n] = index;
        break;

      case DIMEN_VECTOR:
        indices[n] =
          g95_conv_vector_array_index (se, index, info->subscript[n], pvar);

        g95_trans_array_bound_check (se, info->descriptor, index, n);
        break;

      default:
        abort();
      }
   }
  /* Get the index from the vector.  */
  g95_conv_array_index_ref (se, info->data, indices, ar->dimen);
  index = g95_simple_fold (se->expr, &se->pre, &se->pre_tail, pvar);
  /* Put the descriptor back.  */
  se->expr = descsave;

  return index;
}

/* Return the offset for an index.  Performs bound checking for elemental
   dimensions.  Single element references are processed seperately.  */
static tree
g95_conv_array_index_offset (g95_se * se, g95_ss_info * info, int dim, int i,
                             g95_array_ref * ar, tree stride, tree * tmpvar)
{
  tree index;

  /* Get the index into the array for this dimension.  */
  if (ar)
    {
      assert (ar->type != AR_ELEMENT);
      if (ar->dimen_type[dim] == DIMEN_ELEMENT)
        {
          assert (i == -1);
          /* Elemental dimension.  */
          assert (info->subscript[dim]
                  && info->subscript[dim]->type == G95_SS_SCALAR);
          /* We've already translated this value outside the loop.  */
          index = info->subscript[dim]->data.scalar.expr;

          g95_trans_array_bound_check (se, info->descriptor, index, dim);
        }
      else
        {
          /* Scalarized dimension.  */
          assert (info && se->loop);

          index = se->loop->loopvar[i];
          index = build (MULT_EXPR, g95_array_index_type, index,
                         info->stride[i]);
          index = g95_simple_fold (index, &se->pre, &se->pre_tail, tmpvar);

          index = build (PLUS_EXPR, g95_array_index_type, index,
                         info->delta[i]);
          index = g95_simple_fold (index, &se->pre, &se->pre_tail, tmpvar);

          if (ar->dimen_type[dim] == DIMEN_VECTOR)
            {
              index = g95_conv_vector_array_index (se, index,
                  info->subscript[dim], tmpvar);
              g95_trans_array_bound_check (se, info->descriptor, index, dim);
            }
          else
            assert (ar->dimen_type[dim] == DIMEN_RANGE);
        }
    }
  else
    {
      /* Temporary array.  */
      assert (se->loop);
      index = se->loop->loopvar[se->loop->order[i]];
    }

  /* Multiply by the stride.  */
  index = build (MULT_EXPR, g95_array_index_type, index, stride);
  index = g95_simple_fold_tmp (index, &se->pre, &se->pre_tail, tmpvar);

  return index;
}

/* Build a scalarized reference to an array.  */
static void
g95_conv_scalarized_array_ref (g95_se * se, g95_array_ref * ar)
{
  g95_ss_info * info;
  tree index;
  tree tmp;
  tree tmpvar;
  int n;

  info = &se->ss->data.info;
  tmpvar = NULL_TREE;
  if (ar)
    n = se->loop->order[0];
  else
    n = 0;

  index = g95_conv_array_index_offset (se, info, info->dim[n], n, ar,
                                       info->stride0, &tmpvar);
  index = build (PLUS_EXPR, g95_array_index_type, index, info->offset);
  index = g95_simple_fold (index, &se->pre, &se->pre_tail, &tmpvar);

  tmp = build1 (INDIRECT_REF, TREE_TYPE (TREE_TYPE (info->data)), info->data);
  se->expr = build (ARRAY_REF, TREE_TYPE (TREE_TYPE (tmp)), tmp, index);
}

void
g95_conv_tmp_array_ref (g95_se * se)
{
  tree desc;

  desc = se->ss->data.info.descriptor;
  if (G95_DECL_STRING (desc))
    se->string_length = G95_DECL_STRING_LENGTH (desc);

  g95_conv_scalarized_array_ref (se, NULL);
}

/* Build an array reference. se->expr already holds the array descriptor.
   This should be either a variable, indirect variable reference or component
   reference.  For arrays which do not have a descriptor, se->expr will be
   the data pointer.  */
void
g95_conv_array_ref (g95_se * se, g95_array_ref * ar)
{
  int n;
  tree index;
  tree tmp;
  tree tmpvar;
  tree stride;
  tree stridevar;
  tree indexvar;
  tree fault;
  tree faultvar;
  tree condvar;
  g95_se indexse;

  /* Handle scalarized references seperately.  */
  if (ar->type != AR_ELEMENT)
    {
      g95_conv_scalarized_array_ref (se, ar);
      return;
    }

  index = integer_zero_node;
  tmpvar = NULL_TREE;
  stridevar = NULL_TREE;
  indexvar = NULL_TREE;

  fault = integer_zero_node;
  faultvar = NULL_TREE;
  condvar = NULL_TREE;

  for (n = 0; n < ar->dimen; n++)
    {
      g95_init_se (&indexse, NULL);
      g95_conv_simple_val_type (&indexse, ar->start[n],
                                g95_array_index_type);
      g95_add_stmt_to_pre (se, indexse.pre, indexse.pre_tail);

      if (flag_bounds_check)
        {
          /* Check array bounds.  */
          tree cond;

          tmp = g95_conv_array_lbound (se->expr, n);
          tmp = g95_simple_fold (tmp, &se->pre, &se->pre_tail, &tmpvar);
          cond = build (LT_EXPR, boolean_type_node, indexse.expr, tmp);
          cond = g95_simple_fold (cond, &se->pre, &se->pre_tail, &condvar);
          fault = build (TRUTH_OR_EXPR, boolean_type_node, fault, cond);
          fault =
            g95_simple_fold_tmp (fault, &se->pre, &se->pre_tail, &faultvar);

          tmp = g95_conv_array_ubound (se->expr, n);
          tmp = g95_simple_fold (tmp, &se->pre, &se->pre_tail, &tmpvar);
          cond = build (GT_EXPR, boolean_type_node, indexse.expr, tmp);
          cond = g95_simple_fold (cond, &se->pre, &se->pre_tail, &condvar);
          fault = build (TRUTH_OR_EXPR, boolean_type_node, fault, cond);
          fault =
            g95_simple_fold_tmp (fault, &se->pre, &se->pre_tail, &faultvar);
        }

      /* Multiply the index by the stride.  */
      stride = g95_conv_array_stride (se->expr, n);
      stride = g95_simple_fold (stride, &se->pre, &se->pre_tail, &tmpvar);
      tmp = build (MULT_EXPR, g95_array_index_type, indexse.expr, stride);
      tmp = g95_simple_fold (tmp, &se->pre, &se->pre_tail, &tmpvar);

      index = build (PLUS_EXPR, g95_array_index_type, index, tmp);
      index = g95_simple_fold_tmp (index, &se->pre, &se->pre_tail, &indexvar);
    }

  if (flag_bounds_check)
    {
      g95_trans_runtime_check (fault, g95_strconst_fault, &se->pre,
          &se->pre_tail);
    }

  tmp = g95_conv_array_base (se->expr);
  tmp = g95_simple_fold (tmp, &se->pre, &se->pre_tail, NULL);
  tmp = build1 (INDIRECT_REF, TREE_TYPE (TREE_TYPE (tmp)), tmp);
  se->expr = build (ARRAY_REF, TREE_TYPE (TREE_TYPE (tmp)), tmp, index);
}

/* Generate the code to be executed immediately before entering a
   scalarization loop.  */
static void
g95_trans_preloop_setup (g95_loopinfo * loop, int dim, int flag,
    tree * phead, tree * ptail, tree * ptmpvar, tree * pstridevar)
{
  tree index;
  tree stride;
  g95_ss_info *info;
  g95_ss *ss;
  g95_se se;
  int i;

  /* This code will be executed before entering the scalarization loop
     for this dimension.  */
  for (ss = loop->ss; ss != g95_ss_terminator; ss = ss->loop_chain)
    {
      if ((ss->useflags & flag) == 0)
        continue;

      if (ss->type != G95_SS_SECTION
          && ss->type != G95_SS_FUNCTION
          && ss->type != G95_SS_CONSTRUCTOR)
        continue;

      info = &ss->data.info;

      if (dim >= info->dimen)
        continue;

      if (dim == info->dimen - 1)
        {
          /* For the outermost loop calculate the offset due to any
             elemental dimensions.  */
          info->offset = integer_zero_node;
          if (info->ref)
            {
              for (i = 0; i < info->ref->u.ar.dimen; i ++)
                {
                  if (info->ref->u.ar.dimen_type[i] != DIMEN_ELEMENT)
                    continue;

                  g95_init_se (&se, NULL);
                  se.loop = loop;
                  se.expr = info->descriptor;
                  stride = g95_conv_array_stride (info->descriptor, i);
                  stride = g95_simple_fold (stride, phead, ptail,
                                            pstridevar);
                  index = g95_conv_array_index_offset (&se, info, i, -1,
                      &info->ref->u.ar, stride, ptmpvar);
                  g95_add_stmt_to_list (phead, ptail, se.pre, se.pre_tail);
                  assert (se.post == NULL);

                  info->offset = build (PLUS_EXPR, g95_array_index_type,
                                        info->offset, index);
                  info->offset =
                    g95_simple_fold_tmp (info->offset, phead, ptail, NULL);
                }

              i = loop->order[0];
              stride = g95_conv_array_stride (info->descriptor, info->dim[i]);
            }
          else
            stride = g95_conv_array_stride (info->descriptor, 0);

          /* Calculate the stride of the innermost loop.  */
          info->stride0 = g95_simple_fold (stride, phead, ptail, NULL);
        }
      else
        {
          /* Add the offset for the previous loop dimension.  */
          g95_array_ref *ar;

          if (info->ref)
            {
              ar = &info->ref->u.ar;
              i = loop->order[dim + 1];
            }
          else
            {
              ar = NULL;
              i = dim + 1;
            }

          g95_init_se (&se, NULL);
          se.loop = loop;
          se.expr = info->descriptor;
          stride = g95_conv_array_stride (info->descriptor, i);
          stride = g95_simple_fold (stride, phead, ptail, pstridevar);
          index = g95_conv_array_index_offset (&se, info, info->dim[i], i,
              ar, stride, ptmpvar);
          g95_add_stmt_to_list (phead, ptail, se.pre, se.pre_tail);
          assert (se.post == NULL);
          info->offset = build (PLUS_EXPR, g95_array_index_type,
                                info->offset, index);
          info->offset = g95_simple_fold_tmp (info->offset, phead, ptail,
                                              NULL);
        }
    }
}

/* Start a scalarized expression.  Creates a scope and declares loop
   variables.  */
void
g95_start_scalarized_body (g95_loopinfo * loop)
{
  int dim;
  int n;
  tree tmpvar;
  tree stridevar;
  int flags;

  assert (! loop->array_parameter);

  tmpvar = NULL_TREE;
  stridevar = NULL_TREE;

  for (dim = loop->dimen - 1; dim >= 0; dim--)
    {
      n = loop->order[dim];

      g95_start_stmt ();
      loop->code[n] = loop->code_tail[n] = NULL_TREE;

      /* Create the loop variable.  */
      loop->loopvar[n] = create_tmp_var (g95_array_index_type, "S");

      if (dim < loop->temp_dim)
        flags = 3;
      else
        flags = 1;
      g95_trans_preloop_setup (loop, dim, flags, &loop->code[n],
                               &loop->code_tail[n], &tmpvar, &stridevar);
    }
  g95_start_stmt ();
}

static void
g95_trans_scalarized_loop_end (g95_loopinfo * loop, int n, tree head, tree tail)
{
  tree init;
  tree cond;
  tree inc;
  tree tmp;
  tree body;
  tree stmt;

  body = g95_finish_stmt (head, tail);

  tmp = build (MODIFY_EXPR, g95_array_index_type,
              loop->loopvar[n], loop->from[n]);
  init = build_stmt (EXPR_STMT, tmp);
  cond = build (LE_EXPR, g95_array_index_type,
               loop->loopvar[n], loop->to[n]);
  tmp = build (PLUS_EXPR, g95_array_index_type,
              loop->loopvar[n], integer_one_node);
  inc = build (MODIFY_EXPR, g95_array_index_type,
              loop->loopvar[n], tmp);
  stmt = build_stmt (FOR_STMT, init, cond, inc, body);

  g95_add_stmt_to_list (&loop->code[n], &loop->code_tail[n], stmt, stmt);
}

/* Generates the actual loops for a scalarized expression.  */
void
g95_trans_scalarizing_loops (g95_loopinfo * loop, tree head, tree tail)
{
  int dim;
  int n;
  g95_ss *ss;

  /* Generate the loops.  */
  for (dim = 0; dim < loop->dimen; dim++)
    {
      n = loop->order[dim];
      g95_trans_scalarized_loop_end (loop, n, head, tail);
      loop->loopvar[n] = NULL_TREE;
      head = loop->code[n];
      tail = loop->code_tail[n];
    }

  head = g95_finish_stmt (head, tail);
  g95_add_stmt_to_pre (loop, head, NULL_TREE);

  /* Clear all the used flags.  */
  for (ss = loop->ss; ss; ss = ss->loop_chain)
    ss->useflags = 0;
}

/* Finish the main body of a scalarized expression.  */
void
g95_trans_scalarized_loop_boundary (g95_loopinfo * loop, tree head, tree tail)
{
  int dim;
  int n;
  tree tmpvar;
  tree stridevar;

  /* We finish and restart as many loops as are used by the temporary.  */
  for (dim = 0; dim < loop->temp_dim - 1; dim++)
    {
      n = loop->order[dim];
      g95_trans_scalarized_loop_end (loop, n, head, tail);
      head = loop->code[n];
      tail = loop->code_tail[n];
      loop->loopvar[n] = NULL_TREE;
    }

  n = loop->order[loop->temp_dim - 1];
  g95_trans_scalarized_loop_end (loop, n, head, tail);

  tmpvar = NULL_TREE;
  stridevar = NULL_TREE;

  for (dim = loop->temp_dim - 2; dim >= 0; dim--)
    {
      n = loop->order[dim];

      g95_start_stmt ();
      loop->code[n] = loop->code_tail[n] = NULL_TREE;

      loop->loopvar[n] = create_tmp_var (g95_array_index_type, "Q");

      g95_trans_preloop_setup (loop, dim, 2, &loop->code[n],
                               &loop->code_tail[n], &tmpvar, &stridevar);
    }

  g95_start_stmt ();
}

/* Calculate the upper bound of an array section.  */
static tree
g95_conv_section_upper_bound (g95_ss * ss, int n, tree * phead, tree * ptail,
                              tree * tmpvar)
{
  int dim;
  g95_ss *vecss;
  g95_expr *end;
  tree desc;
  tree bound;
  g95_se se;

  assert (ss->type == G95_SS_SECTION);

  /* For vector array subscripts we want the size of the vector.  */
  dim = ss->data.info.dim[n];
  vecss = ss;
  while (vecss->data.info.ref->u.ar.dimen_type[dim] == DIMEN_VECTOR)
    {
      vecss = vecss->data.info.subscript[dim];
      assert (vecss && vecss->type == G95_SS_VECTOR);
      dim = vecss->data.info.dim[0];
    }

  assert (vecss->data.info.ref->u.ar.dimen_type[dim] == DIMEN_RANGE);
  end = vecss->data.info.ref->u.ar.end[dim];
  desc = vecss->data.info.descriptor;

  if (end)
    {
      /* The upper bound was specified.  */
      g95_init_se (&se, NULL);
      g95_conv_simple_val_type (&se, end, g95_array_index_type);
      bound = se.expr;
    }
  else
    {
      /* No upper bound was specified, so use the bound of the array. */
      bound = G95_TYPE_ARRAY_UBOUND (TREE_TYPE (desc), dim);

      if (bound == NULL_TREE)
        {
          bound = g95_conv_descriptor_ubound (desc, g95_rank_cst[dim]);
          bound = g95_simple_fold (bound, phead, ptail, tmpvar);
        }
    }

  return bound;
}

/* Calculate the lower bound of an array section.  */
static void
g95_conv_section_startstride (g95_loopinfo * loop, g95_ss * ss, int n)
{
  g95_expr *start;
  g95_expr *stride;
  g95_ss *vecss;
  tree desc;
  g95_se se;
  tree tmp;
  g95_ss_info *info;
  int dim;

  info = &ss->data.info;

  dim = info->dim[n];

  /* For vector array subscripts we want the size of the vector.  */
  vecss = ss;
  while (vecss->data.info.ref->u.ar.dimen_type[dim] == DIMEN_VECTOR)
    {
      vecss = vecss->data.info.subscript[dim];
      assert (vecss && vecss->type == G95_SS_VECTOR);
      /* Get the descriptors for the vector subscripts as well.  */
      g95_conv_ss_descriptor (loop, vecss);
      dim = vecss->data.info.dim[0];
    }

  assert (vecss->data.info.ref->u.ar.dimen_type[dim] == DIMEN_RANGE);
  start = vecss->data.info.ref->u.ar.start[dim];
  stride = vecss->data.info.ref->u.ar.stride[dim];
  desc = vecss->data.info.descriptor;

  /* Calculate the start of the range.  For vector subscripts this will
     be the range of the vector.  */
  if (start)
    {
      /* Specified section start.  */
      g95_init_se (&se, NULL);
      g95_conv_simple_val_type (&se, start, g95_array_index_type);
      info->start[n] = se.expr;
      g95_add_stmt_to_pre (loop, se.pre, se.pre_tail);
    }
  else
    {
      /* No lower bound specified so use the bound of the array.  */
      info->start[n] = G95_TYPE_ARRAY_LBOUND (TREE_TYPE (desc), dim);

      if (info->start[n] == NULL_TREE)
        {
          tmp = g95_conv_descriptor_lbound (desc, g95_rank_cst[dim]);
          info->start[n] = g95_simple_fold (tmp, &loop->pre,
                                            &loop->pre_tail, NULL);
        }
    }

  /* Calculate the stride.  */
  if (stride == NULL)
      info->stride[n] = integer_one_node;
  else
    {
      g95_init_se (&se, NULL);
      g95_conv_simple_val_type (&se, stride, g95_array_index_type);
      info->stride[n] = se.expr;
      g95_add_stmt_to_pre (loop, se.pre, se.pre_tail);
    }
}

/* Calculates the range start and stride for a SS chain.  Also gets the
   descriptor and data pointer.  The range of vector subscripts is the size
   of the vector.  Array bounds are also checked.  */
void
g95_conv_ss_startstride (g95_loopinfo * loop)
{
  int n;
  tree tmp;
  g95_ss *ss;
  g95_ss *vecss;
  tree desc;

  loop->dimen = 0;
  /* Determine the rank of the loop.  */
  for (ss = loop->ss;
       ss != g95_ss_terminator && loop->dimen == 0;
       ss = ss->loop_chain)
    {
      switch (ss->type)
        {
        case G95_SS_SECTION:
          loop->dimen = ss->data.info.dimen;
          break;

        case G95_SS_CONSTRUCTOR:
          loop->dimen = ss->data.info.dimen;
          break;

        case G95_SS_FUNCTION:
          break;

        default:
          break;
        }
    }

  if (loop->dimen == 0)
    g95_todo_error ("Unable to determine rank of expression");


  /* loop over all the SS in the chain.  */
  for (ss = loop->ss; ss != g95_ss_terminator; ss = ss->loop_chain)
    {
      switch (ss->type)
        {
        case G95_SS_SECTION:
          /* Get the descriptor for the array.  */
          g95_conv_ss_descriptor (loop, ss);

          for (n = 0; n < ss->data.info.dimen; n++)
            g95_conv_section_startstride (loop, ss, n);
          break;

        case G95_SS_CONSTRUCTOR:
          for (n = 0; n < ss->data.info.dimen; n++)
            {
              ss->data.info.start[n] = integer_zero_node;
              ss->data.info.stride[n] = integer_one_node;
            }
          break;

        default:
          break;
        }
    }

  if (flag_bounds_check)
    {
      tree head;
      tree tail;
      tree fault;
      tree faultvar;
      tree condvar;
      tree bound;
      tree boundvar;
      tree tmpvar;
      tree end;
      tree size[G95_MAX_DIMENSIONS];
      g95_ss_info *info;
      int dim;

      g95_start_stmt ();
      head = tail = NULL_TREE;

      fault = integer_zero_node;
      faultvar = NULL_TREE;
      condvar = NULL_TREE;
      boundvar = NULL_TREE;
      tmpvar = NULL_TREE;
      for (n = 0; n < loop->dimen; n++)
        size[n] = NULL_TREE;

      for (ss = loop->ss; ss != g95_ss_terminator; ss = ss->loop_chain)
        {
          if (ss->type != G95_SS_SECTION)
            continue;

          /* TODO: range checking for mapped dimensions.  */
          info = &ss->data.info;

          /* This only checks scalarized dimensions, elemental dimensions are
             checked later.  */
          for (n = 0; n < loop->dimen; n++)
            {
              dim = info->dim[n];
              vecss = ss;
              while (vecss->data.info.ref->u.ar.dimen_type[dim]
                     == DIMEN_VECTOR)
                {
                  vecss = vecss->data.info.subscript[dim];
                  assert (vecss && vecss->type == G95_SS_VECTOR);
                  dim = vecss->data.info.dim[0];
                }
              assert (vecss->data.info.ref->u.ar.dimen_type[dim]
                      == DIMEN_RANGE);
              desc = vecss->data.info.descriptor;

              /* Check lower bound.  */
              bound = g95_conv_array_lbound (desc, dim);
              bound = g95_simple_fold (bound, &head, &tail, &boundvar);
              tmp = info->start[n];
              tmp = build (LT_EXPR, boolean_type_node, tmp, bound);
              tmp = g95_simple_fold (tmp, &head, &tail, &condvar);

              fault = build (TRUTH_OR_EXPR, boolean_type_node, fault, tmp);
              fault = g95_simple_fold_tmp (fault, &head, &tail, &faultvar);

              /* Check the upper bound.  */
              bound = g95_conv_array_ubound (desc, dim);
              bound = g95_simple_fold (bound, &head, &tail, &boundvar);
              end = g95_conv_section_upper_bound (ss, n, &head, &tail,
                                                  &tmpvar);
              tmp = build (GT_EXPR, boolean_type_node, end, bound);
              tmp = g95_simple_fold (tmp, &head, &tail, &condvar);

              fault = build (TRUTH_OR_EXPR, boolean_type_node, fault, tmp);
              fault = g95_simple_fold_tmp (fault, &head, &tail, &faultvar);

              /* Check the section sizes match.  */
              tmp = build (MINUS_EXPR, g95_array_index_type, end,
                           info->start[n]);
              tmp = g95_simple_fold (tmp, &head, &tail, &tmpvar);
              tmp = build (FLOOR_DIV_EXPR, g95_array_index_type, tmp,
                           info->stride[n]);
              if (size[n])
                {
                  tmp = g95_simple_fold (tmp, &head, &tail, &tmpvar);
                  tmp = build (NE_EXPR, boolean_type_node, tmp, size[n]);
                  tmp = g95_simple_fold (tmp, &head, &tail, &condvar);

                  fault = build (TRUTH_OR_EXPR, boolean_type_node, fault, tmp);
                  fault = g95_simple_fold_tmp (fault, &head, &tail, &faultvar);
                }
              else
                size[n] = g95_simple_fold_tmp (tmp, &head, &tail, NULL);
            }
        }
      g95_trans_runtime_check (fault, g95_strconst_bounds, &head, &tail);

      tmp = g95_finish_stmt (head, tail);
      g95_add_stmt_to_pre (loop, tmp, NULL_TREE);
    }
}

/* Return true if both symbols could refer to the same data object.  Does not
   take account of aliasing due to equivalence statements.  */
static int
g95_symbols_could_alias (g95_symbol *lsym, g95_symbol *rsym)
{
  /* Aliasing isn't possible if the symbols have different base types.  */
  if (g95_compare_types (&lsym->ts, &rsym->ts) == 0)
    return 0;

  /* Pointers can point to other pointers, target objects and allocatable
     objects.  Two allocatable objects cannot share the same storage.  */
  if (lsym->attr.pointer
      && (rsym->attr.pointer || rsym->attr.allocatable || rsym->attr.target))
    return 1;
  if (lsym->attr.target
      && rsym->attr.pointer)
    return 1;
  if (lsym->attr.allocatable
      && rsym->attr.pointer)
    return 1;

  return 0;
}

/* Return true if the two SS could be aliased, ie. both point to the same data
   object.  */
static int
g95_could_be_alias (g95_ss * lss, g95_ss * rss)
{
  g95_ref *lref;
  g95_ref *rref;

/*  if (g95_option.no_pointer_alias)
    return 0;*/

  if (g95_symbols_could_alias (lss->expr->symbol, rss->expr->symbol))
    return 1;

  if (rss->expr->symbol->ts.type != BT_DERIVED
      && lss->expr->symbol->ts.type != BT_DERIVED)
    return 0;

  /* For Derived types we must check all the component types.  We can ignore
     array references as these will have the same base type as the previous
     component ref.  */
  for (lref = lss->expr->ref; lref != lss->data.info.ref; lref = lref->next)
    {
      if (lref->type != COMPONENT_REF)
        continue;

      if (g95_symbols_could_alias (lref->u.c.sym, rss->expr->symbol))
        return 1;

      for (rref = rss->expr->ref; rref != rss->data.info.ref; rref = rref->next)
        {
          if (rref->type != COMPONENT_REF)
            continue;

          if (g95_symbols_could_alias (lref->u.c.sym, rref->u.c.sym))
            return 1;
        }
    }

  for (rref = rss->expr->ref; rref != rss->data.info.ref; rref = rref->next)
    {
      if (rref->type != COMPONENT_REF)
        break;

      if (g95_symbols_could_alias (rref->u.c.sym, lss->expr->symbol))
        return 1;
    }

  return 0;
}

/* Returns 1 if the expr is an integer constant value 1, 0 if it is not or
   def if the value could not be determined.  */
static int
g95_expr_is_one (g95_expr * expr, int def)
{
  assert (expr != NULL);

  if (expr->expr_type != EXPR_CONSTANT)
    return def;

  if (expr->ts.type != BT_INTEGER)
    return def;

  return mpz_cmp_si (expr->value.integer, 1) == 0;
}

/* Compare two values.  Returns 0 if e1 == e2, -1 if e1 < e2, +1 if e1 > e2,
   and -2 if the relationship could not be determined.  */
static int g95_dep_compare_expr (g95_expr * e1, g95_expr * e2)
{
  int i;

  if (e1->expr_type != e2->expr_type)
    return -2;

  switch (e1->expr_type)
    {
    case EXPR_CONSTANT:
      if (e1->ts.type != BT_INTEGER || e2->ts.type != BT_INTEGER)
        return -2;

      i = mpz_cmp (e1->value.integer, e2->value.integer);
      if (i == 0)
        return 0;
      else if (i < 0)
        return -1;
      return 1;

    case EXPR_VARIABLE:
      if (e1->ref || e2->ref)
        return -2;
      if (e1->symbol == e2->symbol)
        return 0;
      return -2;

    default:
      return -2;
    }
}

/* Returns 1 if the two ranges are the same, 0 if they are not, and def
   if the results are indeterminate.  N is the dimension to compare.  */
static int
g95_is_same_range (g95_array_ref * ar1, g95_array_ref * ar2, int n, int def)
{
  g95_expr *e1;
  g95_expr *e2;
  int i;

  /* TODO: More sophisticated range comparison.  */
  assert (ar1 && ar2);

  assert (ar1->dimen_type[n] == ar2->dimen_type[n]);

  e1 = ar1->stride[n];
  e2 = ar2->stride[n];
  /* Check for mismatching strides.  A NULL stride means a stride of 1.  */
  if (e1 && ! e2)
    {
      i = g95_expr_is_one (e1, -1);
      if (i == -1)
        return def;
      else if (i == 0)
        return 0;
    }
  else if (e2 && ! e1)
    {
      i = g95_expr_is_one (e2, -1);
      if (i == -1)
        return def;
      else if (i == 0)
        return 0;
    }
  else if (e1 && e2)
    {
      i = g95_dep_compare_expr (e1, e2);
      if (i == -2)
        return def;
      else if (i != 0)
        return 0;
    }
  /* The strides match.  */

  /* Check the range start.  */
  e1 = ar1->start[n];
  e2 = ar2->start[n];

  if (! (e1 || e2))
    return 1;

  /* Use the bound of the array if no bound is specified.  */
  if (ar1->as && ! e1)
    e1 = ar1->as->lower[n];

  if (ar2->as && ! e2)
    e2 = ar2->as->upper[n];

  /* Check we have values for both.  */
  if (! (e1 && e2))
    return def;

  i = g95_dep_compare_expr (e1, e2);

  if (i == -2)
    return def;
  else if (i == 0)
    return 1;
  return 0;
}

/* Dependency checking for direct function return by reference.  Returns true
   if the arguments of the function depend on the destination.  This is
   considerably less conservative than other dependencies because many
   function arguments will already be copied into a temporary.  */
int
g95_check_fncall_dependency (g95_expr * dest, g95_expr * fncall)
{
  g95_actual_arglist *actual;
  g95_ref *ref;
  g95_expr *expr;
  int n;

  assert (dest->expr_type == EXPR_VARIABLE
          && fncall->expr_type == EXPR_FUNCTION);
  assert (fncall->rank > 0);

  for (actual = fncall->value.function.actual; actual; actual = actual->next)
    {
      expr = actual->expr;

      /* Skip args which are not present.  */
      if (! expr)
        continue;

      /* Non-variable expressions will be allocated temporaries anyway.  */
      switch (expr->expr_type)
        {
        case EXPR_VARIABLE:
          if (expr->rank > 1)
            {
              /* This is an array section.  */
              for (ref = expr->ref; ref; ref = ref->next)
                {
                  if (ref->type == REF_ARRAY
                      && ref->u.ar.type != AR_ELEMENT)
                    break;
                }
              assert (ref);
              /* AR_FULL can't contain vector subscripts.  */
              if (ref->u.ar.type == AR_SECTION)
                {
                  for (n = 0; n < ref->u.ar.dimen; n++)
                    {
                      if (ref->u.ar.dimen_type[n] == DIMEN_VECTOR)
                        break;
                    }
                  /* Vector subscript array sections will be copied to a
                     temporary.  */
                  if (n != ref->u.ar.dimen)
                    continue;
                }
            }

          if (g95_check_dependency (dest, actual->expr, NULL, 0))
            return 1;
          break;

        case EXPR_ARRAY:
          if (g95_check_dependency (dest, expr, NULL, 0))
            return 1;
          break;

        default:
          break;
        }
    }

  return 0;
}

/* Return true if the statement body redefines the condition.  Returns true if
   expr2 depends on expr1.  expr1 should be a single term suitable for the lhs
   of an assignment.  The symbols listed in VARS must be considered to have
   all possible values. All other scalar variables may be considered constant.
   Used for forall and where statements.  Also used with functions returning
   arrays without a temporary.  */
int
g95_check_dependency (g95_expr * expr1, g95_expr * expr2, g95_expr ** vars,
                      int nvars)
{
  g95_ref *ref;
  int n;
  g95_actual_arglist *actual;

  assert (expr1->expr_type == EXPR_VARIABLE);
  /* TODO: -fassume-no-pointer-aliasing */
  if (expr1->symbol->attr.pointer)
    return 1;
  for (ref = expr1->ref; ref; ref = ref->next)
    {
      if (ref->type == COMPONENT_REF
          && ref->u.c.component->pointer)
        return 1;
    }

  switch (expr2->expr_type)
    {
    case EXPR_OP:
      n = g95_check_dependency (expr1, expr2->op1, vars, nvars);
      if (n)
        return n;
      if (expr2->op2)
        return g95_check_dependency (expr1, expr2->op2, vars, nvars);
      return 0;

    case EXPR_VARIABLE:
      if (expr2->symbol->attr.pointer)
        return 1;

      for (ref = expr2->ref; ref; ref = ref->next)
        {
          if (ref->type == COMPONENT_REF
              && ref->u.c.component->pointer)
                return 1;
        }

      if (expr1->symbol != expr2->symbol)
        return 0;

      for (ref = expr2->ref; ref; ref = ref->next)
        {
          /* Identical ranges return 0, overlapping ranges return 1.  */
          if (ref->type == ARRAY_REF)
            return 1;
        }
      return 1;

    case EXPR_FUNCTION:
      /* Remember possible differences betweeen elemental and transformational
         functions.  All functions inside a forall will be pure.  */
      for (actual = expr2->value.function.actual;
           actual;
           actual = actual->next)
        {
          if (! actual->expr)
            continue;
          n = g95_check_dependency (expr1, actual->expr, vars, nvars);
          if (n)
            return n;
        }
      return 0;

    case EXPR_CONSTANT:
      return 0;

    case EXPR_ARRAY:
      /* Probably ok in the majority of (constant) cases.  */
      return 1;

    default:
      return 1;
    }
}

/* Resolve array data dependencies.  This will eventually do dependency
   analysis and maybe loop shifting, etc.  Currently it allocates a
   temporary if there are any possible dependencies.  */
void
g95_conv_resolve_dependencies (g95_loopinfo * loop, g95_ss * dest,
                               g95_ss * rss)
{
  g95_ss *ss;
  int temp_dim;
  int same;
  g95_ref *lref;
  g95_ref *rref;
  g95_ref *aref;
  int depends[G95_MAX_DIMENSIONS];
  int n;
  int dim;

  loop->temp_ss = NULL;
  aref = dest->data.info.ref;
  temp_dim = 0;
  for (n = 0; n < loop->dimen; n++)
    depends[n] = 0;

  for (ss = rss; ss != g95_ss_terminator; ss = ss->next)
    {
      if (ss->type != G95_SS_SECTION)
        continue;

      if (g95_could_be_alias (dest, ss))
        temp_dim = -1;

      if (temp_dim == -1)
        break;

      if (dest->expr->symbol == ss->expr->symbol)
        {
          lref = dest->expr->ref;
          rref = ss->expr->ref;

          /* Same specifies if both SS refer to the same array.  */
          same = 1;
          while (same && lref != aref)
            {
              assert (lref->type == rref->type);

              switch (lref->type)
                {
                case COMPONENT_REF:
                  if (lref->u.c.component != rref->u.c.component)
                    same = 0;
                  else
                    lref = aref->next;
                  break;

                case ARRAY_REF:
                  assert (lref->u.ar.type == AR_ELEMENT);
                  /* TODO: Not all elmental array refs conflict.  */
                  /* We have a potential dependency.  */
                  temp_dim = -1;
                  same = 0;
                  break;

                default:
                  abort();
                }
              lref = lref->next;
              rref = rref->next;
            }

          /* Check the elemental dimensions.  */
          if (same)
            {
              assert (lref->u.ar.dimen == rref->u.ar.dimen);
              for (n = 0; n < dest->data.info.dimen; n++)
                {
                  /* eg. a(:, 1) = a(2, :).  */
                  /* TODO: check dependencies of elemental vs section.  */
                  if (lref->u.ar.dimen_type[n] != rref->u.ar.dimen_type[n])
                    {
                      temp_dim = -1;
                      same = 0;
                      break;
                    }

                  if (lref->u.ar.dimen_type[n] != DIMEN_ELEMENT)
                    continue;

                  /* If the elemental indices are different, there is no
                     dependency.  */
                  dim = g95_dep_compare_expr (lref->u.ar.start[n],
                                              rref->u.ar.start[n]);
                  if (dim == -1 || dim == 1)
                    {
                      same = 0;
                      break;
                    }
                }
            }

          if (same)
            {
              for (n = 0; n < loop->dimen; n++)
                {
                  int dim;
                  dim = dest->data.info.dim[n];
                  if (lref->u.ar.dimen_type[dim] == DIMEN_VECTOR)
                    depends[n] = 2;
                  else if (! g95_is_same_range (&lref->u.ar,
                                                &rref->u.ar, dim, 0))
                    depends[n] = 1;
                }
            }
        }
    }

  if (temp_dim == 0)
    {
      /* Put all the dimensions with dependencies in the innermost loops.  */
      dim = 0;
      for (n = 0; n < loop->dimen; n++)
        {
          assert (loop->order[n] == n);

          if (depends[n])
            loop->order[dim++] = n;
        }
      temp_dim = dim;
      for (n = 0; n < loop->dimen; n++)
        {
          if (! depends[n])
            loop->order[dim++] = n;
        }
      assert (dim == loop->dimen);
    }
  else
    temp_dim = loop->dimen;

  if (temp_dim)
    {
      loop->temp_ss = g95_get_ss();
      loop->temp_ss->type = G95_SS_TEMP;
      loop->temp_ss->data.temp.type =
        g95_get_element_type (TREE_TYPE (dest->data.info.descriptor));
      loop->temp_ss->data.temp.string_length =
        g95_conv_string_length (dest->data.info.descriptor);
      loop->temp_ss->data.temp.dimen = temp_dim;
      loop->temp_ss->next = g95_ss_terminator;
      g95_add_ss_to_loop (loop, loop->temp_ss);
    }
  else
    loop->temp_ss = NULL;
}

/* Initialise the scalarization loop.  Creates the loop variables.  Determines
   the range of the loop variables.  Creates a temporary if required.
   Calculates how to transform from loop variables to array indices for each
   expression.  Also generates code for scalar expressions which have been
   moved outside the loop. */
/*GCC ARRAYS*/
void
g95_conv_loop_setup (g95_loopinfo * loop)
{
  int n;
  int dim;
  g95_ss_info *info;
  g95_ss_info *specinfo;
  g95_ss *ss;
  tree tmp;
  tree var;
  tree len;
  g95_ss *loopspec[G95_MAX_DIMENSIONS];
  mpz_t *cshape;
  mpz_t i;

  mpz_init (i);
  for (n = 0; n < loop->dimen; n++)
    {
      loopspec[n] = NULL;
      /* We use one SS term, and use that to determine the bounds of the
         loop for this dimension.  We try to pick the simplest term.  */
      for (ss = loop->ss ; ss != g95_ss_terminator ; ss = ss->loop_chain)
        {
          if (ss->type == G95_SS_CONSTRUCTOR)
            {
              if (ss->expr->value.constructor.shape)
                {
                  /* The frontend has worked out the size for us.  */
                  loopspec[n] = ss;
                }
              else
                {
                  /* Try to figure out the size of the constructior.  */
                  g95_get_array_cons_size (&i,
                      ss->expr->value.constructor.head);
                  /* A negative value meens we failed. */
                  if (mpz_sgn (i) > 0)
                    {
                      mpz_sub_ui (i, i, 1);
                      loop->to[n] =
                        g95_conv_mpz_to_tree (i, g95_array_index_kind);
                      loopspec[n] = ss;
                    }
                }
              continue;
            }

          /* We don't know how to handle functions yet.  */
          if (ss->type != G95_SS_SECTION)
            continue;

          info = &ss->data.info;

          if (loopspec[n])
            specinfo = &loopspec[n]->data.info;
          else
            specinfo = NULL;
          info = &ss->data.info;

          /* Criteria for choosing a loop specifier (most important first):
              array constructor
              stride of one
              known stride
              known lower bound
              known upper bound
           */
          if (! specinfo)
            loopspec[n] = ss;
          else if (loopspec[n]->type != G95_SS_CONSTRUCTOR)
            {
              if (integer_onep (info->stride[n])
                        && ! integer_onep (specinfo->stride[n]))
                loopspec[n] = ss;
              else if (INTEGER_CST_P (info->stride[n])
                        && ! INTEGER_CST_P (specinfo->stride[n]))
                loopspec[n] = ss;
              else if (INTEGER_CST_P (info->start[n])
                        && ! INTEGER_CST_P (specinfo->start[n]))
                loopspec[n] = ss;
              /* We don't work out the upper bound.
              else if (INTEGER_CST_P (info->finish[n])
                        && ! INTEGER_CST_P (specinfo->finish[n]))
                loopspec[n] = ss;*/
            }
        }

      if (! loopspec[n])
        g95_todo_error ("Unable to find scalarization loop specifier");

      info = &loopspec[n]->data.info;

      /* Set the extents of this range.  */
      loop->from[n] = info->start[n];
      switch (loopspec[n]->type)
        {
        case G95_SS_CONSTRUCTOR:
          cshape = loopspec[n]->expr->value.constructor.shape;
          if (cshape)
            {
              mpz_set (i, cshape[n]);
              mpz_sub_ui (i, i, 1);
              loop->to[n] = g95_conv_mpz_to_tree (i, g95_array_index_kind);
            }
          else
            {
              assert (info->dimen == 1);
              assert (loop->to[n]);
            }
          break;

        case G95_SS_SECTION:
          loop->to[n] = g95_conv_section_upper_bound (loopspec[n], n,
              &loop->pre, &loop->pre_tail, NULL);
          break;

        default:
          abort ();
        }
      info->delta[n] = integer_zero_node;

      if (! integer_onep(info->stride[n]))
        {
          /* We don't have a convenient unit stride section.  */
          var = NULL_TREE;
          /* Set the delta for this section.  */
          info->delta[n] = loop->from[n];
          /* Make the loop variable start at 0.  */
          tmp = build (MINUS_EXPR, g95_array_index_type, loop->to[n],
                      loop->from[n]);
          loop->to[n] =
            g95_simple_fold (tmp, &loop->pre, &loop->pre_tail, &var);
          loop->from[n] = integer_zero_node;

          /* Number of iterations is (end - start + step) / step.
             with start = 0, this simplifies to
              last = end / step;
              for (i = 0; i<=last; i++){...};  */
          tmp = build (TRUNC_DIV_EXPR, g95_array_index_type, loop->to[n],
                      info->stride[n]);
          loop->to[n] =
            g95_simple_fold (tmp, &loop->pre, &loop->pre_tail, &var);
        }
    }

  /* If we want a temporary then create it.  */
  if (loop->temp_ss != NULL)
    {
      assert (loop->temp_ss->type == G95_SS_TEMP);
      tmp = loop->temp_ss->data.temp.type;
      len = loop->temp_ss->data.temp.string_length;
      n = loop->temp_ss->data.temp.dimen;
      memset (&loop->temp_ss->data.info, 0, sizeof(g95_ss_info));
      loop->temp_ss->type = G95_SS_SECTION;
      loop->temp_ss->data.info.dimen = n;
      g95_trans_allocate_temp_array (loop, &loop->temp_ss->data.info,
                                     tmp, len);
    }

  g95_add_loop_ss_code (loop, loop->ss);

  for (n = 0; n < loop->temp_dim; n++)
    loopspec[loop->order[n]] = NULL;

  mpz_clear (i);

  /* For array parameters we don't have loop variables, so don't calculate the
     translations.  */
  if (loop->array_parameter)
    return;

  /* Calculate the translation from loop variables to array indices.  */
  for (ss = loop->ss; ss != g95_ss_terminator; ss = ss->loop_chain)
    {
      if (ss->type != G95_SS_SECTION)
        continue;

      info = &ss->data.info;

      for (n = 0; n < info->dimen; n++)
        {
          dim = info->dim[n];

          /* If we are specifying the range the delta are already set.  */
          if (ss != loopspec[n])
            {
              /* Calculate the offset relative to the loop variable.  */
              var = NULL_TREE;
              /* First multiply by the stride.  */
              tmp = build (MULT_EXPR, g95_array_index_type, loop->from[n],
                  info->stride[n]);
              tmp = g95_simple_fold (tmp, &loop->pre, &loop->pre_tail, &var);

              /* Then subtract this from our starting value.  */
              tmp = build (MINUS_EXPR, g95_array_index_type,
                           info->start[n], tmp);

              info->delta[n] = g95_simple_fold (tmp, &loop->pre,
                                                &loop->pre_tail, &var);
            }
        }
    }
}

/* Fills in an array descriptor, and returns the size of the array.  The size
   will be a simple_val, ie a variable or a constant.  Also calculates the
   offset of the base.
   {
    stride = 1;
    offset = 0;
    for (n = 0; n < rank; n++)
      {
        a.lbound[n] = specified_lower_bound;
        offset = offset + a.lbond[n] * stride;
        size = 1 - lbound;
        a.ubound[n] = specified_upper_bound;
        a.stride[n] = stride;
        size = ubound + size; //size = ubound + 1 - lbound
        stride = stride * size;
      }
    size = stride * sizeof (element);
   }  */
/*GCC ARRAYS*/
static tree
g95_array_init_size (tree descriptor, int rank, tree * poffset,
                     g95_expr ** lower, g95_expr ** upper,
                     tree * phead, tree * ptail, tree * pstring)
{
  tree type;
  tree tmp;
  tree tmpvar;
  tree size;
  tree offset;
  tree offsetvar;
  tree stride;
  tree stridevar;
  tree stmt;
  tree string_len;
  g95_se se;
  int n;

  type = TREE_TYPE (descriptor);

  tmpvar = NULL_TREE;
  stridevar = NULL_TREE;
  stride = integer_one_node;
  offsetvar = NULL_TREE;
  offset = integer_zero_node;

  /* Set the dtype.  */
  tmp = g95_conv_descriptor_dtype (descriptor);
  tmp = build (MODIFY_EXPR, TREE_TYPE (tmp), tmp,
               G95_TYPE_ARRAY_DTYPE (TREE_TYPE (descriptor)));
  stmt = build_stmt (EXPR_STMT, tmp);
  g95_add_stmt_to_list (phead, ptail, stmt, stmt);

  for (n = 0; n < rank; n++)
    {
      /* Set lower bound.  */
      g95_init_se (&se, NULL);
      if (lower == NULL)
        se.expr = integer_one_node;
      else
        {
          assert (lower[n]);
          g95_conv_simple_val_type (&se, lower[n], g95_array_index_type);
        }
      g95_add_stmt_to_list (phead, ptail, se.pre, se.pre_tail);
      tmp = g95_conv_descriptor_lbound (descriptor, g95_rank_cst[n]);
      tmp = build (MODIFY_EXPR, TREE_TYPE (tmp), tmp, se.expr);
      stmt = build_stmt (EXPR_STMT, tmp);
      g95_add_stmt_to_list (phead, ptail, stmt, stmt);

      /* Work out the offset for this component.  */
      tmp = build (MULT_EXPR, g95_array_index_type, se.expr, stride);
      tmp = g95_simple_fold (tmp, phead, ptail, &tmpvar);

      offset = build (MINUS_EXPR, g95_array_index_type, offset, tmp);
      offset = g95_simple_fold_tmp (offset, phead, ptail, &offsetvar);

      /* Start the calculation for the size of this dimension.  */
      size = build (MINUS_EXPR, g95_array_index_type,
                   integer_one_node, se.expr);
      size = g95_simple_fold (size, phead, ptail, &tmpvar);

      /* Set upper bound.  */
      g95_init_se (&se, NULL);
      assert (upper[n]);
      g95_conv_simple_val_type (&se, upper[n], g95_array_index_type);
      g95_add_stmt_to_list (phead, ptail, se.pre, se.pre_tail);

      tmp = g95_conv_descriptor_ubound (descriptor, g95_rank_cst[n]);
      tmp = build (MODIFY_EXPR, TREE_TYPE (tmp), tmp, se.expr);
      stmt = build_stmt (EXPR_STMT, tmp);
      g95_add_stmt_to_list (phead, ptail, stmt, stmt);

      /* Store the stride.  */
      tmp = g95_conv_descriptor_stride (descriptor, g95_rank_cst[n]);
      tmp = build (MODIFY_EXPR, TREE_TYPE (tmp), tmp, stride);
      stmt = build_stmt (EXPR_STMT, tmp);
      g95_add_stmt_to_list (phead, ptail, stmt, stmt);

      /* Calculate the size of this dimension.  */
      size = build (PLUS_EXPR, g95_array_index_type, se.expr, size);
      size = g95_simple_fold (size, phead, ptail, &tmpvar);

      /* Multiply the stride by the number of elements in this dimension.  */
      stride = build (MULT_EXPR, g95_array_index_type, stride, size);
      stride = g95_simple_fold_tmp (stride, phead, ptail, &stridevar);
    }

  if (pstring && *pstring)
    {
      string_len = *pstring;
      *pstring = g95_simple_fold_tmp (stride, phead, ptail, NULL);

      string_len = build (MULT_EXPR, g95_array_index_type, stride, string_len);
      string_len = g95_simple_fold (string_len, phead, ptail, NULL);
    }
  else
    string_len = NULL_TREE;

  /* The stride is the number of elements in the array, so multiply by the
     size of an element to get the total size.  */
  tmp = TYPE_SIZE_UNIT (g95_get_element_type (type));
  stride = build (MULT_EXPR, g95_array_index_type, stride, tmp);
  stride = g95_simple_fold_tmp (stride, phead, ptail, &stridevar);

  if (string_len)
    {
      stride = build (PLUS_EXPR, g95_array_index_type, stride, string_len);
      stride = g95_simple_fold (stride, phead, ptail, &stridevar);
    }

  if (poffset != NULL )
    *poffset = offset;

  return stride;
}

/* Initialises the descriptor and generates a call to __g95_allocate.  Does
   the work for an ALLOCATE statement.  */
/*GCC ARRAYS*/
void
g95_array_allocate (g95_se * se, g95_ref * ref, tree pstat)
{
  tree tmp;
  tree base;
  tree stmt;
  tree pointer;
  tree allocate;
  tree offset;
  tree size;
  tree len;
  g95_expr **lower;
  g95_expr **upper;

  /* Figure out the size of the array.  */
  switch (ref->u.ar.type)
    {
    case AR_ELEMENT:
      lower = NULL;
      upper = ref->u.ar.start;
      break;

    case AR_FULL:
      assert (ref->u.ar.as->type == AS_EXPLICIT);

      lower = ref->u.ar.as->lower;
      upper = ref->u.ar.as->upper;
      break;

    case AR_SECTION:
      lower = ref->u.ar.start;
      upper = ref->u.ar.end;
      break;

    default:
      abort();
      break;
    }

  len = se->string_length;
  size = g95_array_init_size (se->expr, ref->u.ar.as->rank, &offset,
      lower, upper, &se->pre, &se->pre_tail, &len);

  /* Allocate memory to store the data.  */
  tmp = g95_conv_descriptor_data (se->expr);
  tmp = build1 (ADDR_EXPR, build_pointer_type (TREE_TYPE (tmp)), tmp);
  pointer = g95_simple_fold (tmp, &se->pre, &se->pre_tail, NULL);

  if (g95_array_index_type == g95_int4_type_node)
    allocate = gfor_fndecl_allocate;
  else if (g95_array_index_type == g95_int8_type_node)
    allocate = gfor_fndecl_allocate64;
  else
    abort();

  tmp = g95_chainon_list (NULL_TREE, pointer);
  tmp = g95_chainon_list (tmp, size);
  tmp = g95_chainon_list (tmp, pstat);
  tmp = g95_build_function_call (allocate, tmp);
  stmt = build_stmt (EXPR_STMT, tmp);
  g95_add_stmt_to_pre (se, stmt, stmt);

  /* Set base = &data[offset].  */
  pointer = g95_conv_descriptor_data (se->expr);
  pointer = g95_simple_fold (pointer, &se->pre, &se->pre_tail, NULL);
  if (! integer_zerop (offset))
    {
      tmp = build1 (INDIRECT_REF, TREE_TYPE (TREE_TYPE (pointer)), pointer);
      tmp = build (ARRAY_REF, TREE_TYPE (TREE_TYPE (tmp)), tmp, offset);
      tmp = build1 (ADDR_EXPR, TREE_TYPE (pointer), tmp);
    }
  base = g95_conv_descriptor_base (se->expr);

  tmp = build (MODIFY_EXPR, TREE_TYPE (base), base, tmp);
  stmt = build_stmt (EXPR_STMT, tmp);
  g95_add_stmt_to_pre (se, stmt, stmt);

  /* Initialize the pointers for a character array.  */
  if (len)
    {
      stmt = g95_trans_init_character_array (se->expr, pointer, len);
      g95_add_stmt_to_pre (se, stmt, NULL_TREE);
    }
}

/* Deallocate an array variable.  Also called when an allocated variable goes
   out of scope.  Does not create a new scope for the temporary or wrap in
   a COMPOUND_STMT as this can be done more effectively by the caller.  */
/*GCC ARRAYS*/
tree
g95_array_deallocate (tree descriptor)
{
  tree var;
  tree tmp;
  tree stmt;

  /* Get a pointer to the data.  */
  tmp = g95_conv_descriptor_data (descriptor);
  tmp = build1 (ADDR_EXPR, build_pointer_type (TREE_TYPE (tmp)), tmp);
  var = create_tmp_var (TREE_TYPE (tmp), "ptr");
  tmp = build (MODIFY_EXPR, TREE_TYPE (tmp), var, tmp);
  stmt = build_stmt (EXPR_STMT, tmp);

  /* Parameter is the address of the data component.  */
  tmp = g95_chainon_list (NULL_TREE, var);
  tmp = g95_chainon_list (tmp, integer_zero_node);
  tmp = g95_build_function_call (gfor_fndecl_deallocate, tmp);
  stmt = chainon (stmt, build_stmt (EXPR_STMT, tmp));

  return stmt;
}

/* Generate code to initialize the descriptor for an array variable.  */
/*GCC ARRAYS*/
tree
g95_trans_auto_array_allocation (tree descriptor, g95_symbol * sym)
{
  tree offset;
  tree size;
  tree pointer;
  tree tmp;
  tree ref;
  tree stmt;
  tree tmpvar;
  tree head;
  tree tail;
  tree len;
  g95_array_spec *as;

  assert (! sym->attr.pointer || sym->attr.allocatable);

  if (G95_ARRAY_TYPE_P (TREE_TYPE (descriptor)))
    {
      if (sym->value)
        g95_todo_error ("Initialization of arrays");
      if (sym->ts.type == BT_CHARACTER)
        g95_todo_error ("arrays of strings");
      return NULL_TREE;
    }

  /* TODO: initialization of descriptorless arrays.  */
  if (TREE_STATIC (descriptor) || sym->attr.use_assoc)
    {
      /* These should all be descriptorless arrays.  */
      abort ();

      assert (G95_DESCRIPTOR_TYPE_P (TREE_TYPE (descriptor)));
      offset = g95_build_array_initializer (sym);

      if (sym->attr.use_assoc || sym->module[0] == 0)
        {
          g95_start_stmt();
          head = tail = NULL_TREE;

          /* We still need to set the base component.  */
          tmp = g95_conv_descriptor_data (descriptor);
          ref = g95_conv_descriptor_base (descriptor);
          tmp = g95_simple_fold (tmp, &head, &tail, NULL);
          tmp = build1 (INDIRECT_REF, TREE_TYPE (TREE_TYPE (tmp)), tmp);
          tmp = build (ARRAY_REF, TREE_TYPE (TREE_TYPE (tmp)), tmp, offset);
          tmp = build1 (ADDR_EXPR, TREE_TYPE (ref), tmp);
          tmp = build (MODIFY_EXPR, TREE_TYPE (ref), ref, tmp);
          stmt = build_stmt (EXPR_STMT, tmp);
          g95_add_stmt_to_list (&head, &tail, stmt, stmt);

          stmt = g95_finish_stmt (head, tail);
          return stmt;
        }
      return NULL_TREE;
    }

  assert (! sym->module[0]);

  g95_start_stmt();
  head = tail = NULL_TREE;

  as = sym->as;

  if (sym->ts.type == BT_CHARACTER)
    {
      /* TODO: Derived types.  */
      assert (TREE_CODE (sym->backend_decl) == VAR_DECL);
      len = G95_DECL_STRING_LENGTH (sym->backend_decl);
      if (! INTEGER_CST_P (len))
        len = g95_conv_init_string_length (sym, &head, &tail);
    }
  else
    len = NULL_TREE;

  size = g95_array_init_size (descriptor, as->rank, &offset,
                              as->lower, as->upper, &head, &tail, &len);


  /* Allocate the array.  */
  if (g95_can_put_var_on_stack (size))
    {
      /* Create a temporary variable to hold the data.  */
      tmp = g95_get_stack_array_type (size);
      tmpvar = create_tmp_alias_var (tmp, "A");
      TREE_STATIC (tmpvar) = TREE_STATIC (descriptor);

      /* Store the address.  */
      tmp = g95_conv_descriptor_data (descriptor);
      pointer = build1 (ADDR_EXPR, TREE_TYPE (tmp), tmpvar);
      pointer = g95_simple_fold (pointer, &head, &tail, NULL);
      tmp = build (MODIFY_EXPR, TREE_TYPE (tmp), tmp, pointer);
      stmt = build_stmt (EXPR_STMT, tmp);
      g95_add_stmt_to_list (&head, &tail, stmt, stmt);
    }
  else
    {
      /* Allocate memory to hold the data.  */
      /* Get the address of the data component.  */
      tmp = g95_conv_descriptor_data (descriptor);
      tmp = build1 (ADDR_EXPR, build_pointer_type (TREE_TYPE (tmp)), tmp);
      pointer = create_tmp_var (TREE_TYPE (tmp), "ptr");
      tmp = build (MODIFY_EXPR, TREE_TYPE (pointer), pointer, tmp);
      stmt = build_stmt (EXPR_STMT, tmp);
      g95_add_stmt_to_list (&head, &tail, stmt, stmt);

      /* Now allocate the memory.  */
      tmp = g95_chainon_list (NULL_TREE, pointer);
      tmp = g95_chainon_list (tmp, size);
      if (g95_array_index_kind == 4)
        ref = gfor_fndecl_internal_malloc;
      else if (g95_array_index_kind == 8)
        ref = gfor_fndecl_internal_malloc64;
      else
        abort();

      tmp = g95_build_function_call (ref, tmp);
      stmt = build_stmt (EXPR_STMT, tmp);
      g95_add_stmt_to_list (&head, &tail, stmt, stmt);

      pointer = g95_conv_descriptor_data (descriptor);
      pointer = g95_simple_fold (pointer, &head, &tail, NULL);

      tmpvar = NULL_TREE;
    }

  /* Set the base of the array.  */
  if (! integer_zerop (offset))
    {
      tmp = build1 (INDIRECT_REF, TREE_TYPE (TREE_TYPE (pointer)), pointer);
      tmp = build (ARRAY_REF, TREE_TYPE (TREE_TYPE (tmp)), tmp, offset);
      tmp = build1 (ADDR_EXPR, TREE_TYPE(pointer), tmp);
    }
  ref = g95_conv_descriptor_base (descriptor);
  tmp = build (MODIFY_EXPR, TREE_TYPE(ref), ref, tmp);
  stmt = build_stmt (EXPR_STMT, tmp);
  g95_add_stmt_to_list (&head, &tail, stmt, stmt);

  /* Initialize the pointers for an array of character strings.  */
  if (len)
    {
      stmt = g95_trans_init_character_array (descriptor, pointer, len);
      g95_add_stmt_to_list (&head, &tail, stmt, NULL_TREE);
    }

  head = g95_finish_stmt (head, tail);

  if (tmpvar != NULL_TREE)
    pushdecl (tmpvar);

  if (sym->value)
    internal_error ("non-static array with initializer");

  return head;
}

/* Generate code to repack an array.  */
/*GCC ARRAYS*/
static void
g95_trans_repack_array (tree * phead, tree * ptail, tree dest, tree src,
                       int dimen)
{
  int n;
  g95_loopinfo loop;
  g95_se lse;
  g95_se rse;
  tree tmp;
  tree stmt;
  g95_ss *lss;
  g95_ss *rss;

  /* We can either repack arrays inline or use a library function to do it.
     inline is faster, but generates larger code.  */
  if (! g95_option.inline_repack_arrays)
    {
      tree args;

      /* TODO: Write the library repack functions.  */
      args = NULL_TREE;
      if (TREE_CODE (dest) == INDIRECT_REF)
        tmp = TREE_OPERAND (dest, 0);
      else
        {
          tmp = build1 (ADDR_EXPR, build_pointer_type (TREE_TYPE (dest)), dest);
          tmp = g95_simple_fold (tmp, phead, ptail, NULL);
        }
      args = g95_chainon_list (args, tmp);
      if (TREE_CODE (src) == INDIRECT_REF)
        tmp = TREE_OPERAND (src, 0);
      else
        {
          tmp = build1 (ADDR_EXPR, build_pointer_type (TREE_TYPE (src)), src);
          tmp = g95_simple_fold (tmp, phead, ptail, NULL);
        }
      args = g95_chainon_list (args, tmp);
      tmp = TYPE_SIZE_UNIT (g95_get_element_type (TREE_TYPE (dest)));
      args = g95_chainon_list (args, tmp);

      tmp = g95_build_function_call (gfor_fndecl_repack[dimen - 1], args);
      stmt = build_stmt (EXPR_STMT, tmp);
      g95_add_stmt_to_list (phead, ptail, stmt, stmt);

      return;
    }

  /* Repack the array inline.  */
  g95_init_loopinfo (&loop);
  loop.dimen = dimen;
  lss = g95_get_ss ();
  rss = g95_get_ss ();

  /* Work out the bounds and initialize the scalarizer parameters.  */
  if (TREE_CODE (dest) == INDIRECT_REF)
    tmp = src;
  else
    tmp = dest;
  for (n = 0; n < dimen; n++)
    {
      loop.loopvar[n] = create_tmp_var (g95_array_index_type, "S");
      loop.from[n] = g95_conv_array_lbound (tmp, n);
      loop.to[n] = g95_conv_array_ubound (tmp, n);
      loop.to[n] = g95_simple_fold (loop.to[n], phead, ptail, NULL);

      lss->data.info.start[n] = loop.from[n];
      lss->data.info.delta[n] = integer_zero_node;
      lss->data.info.stride[n] = integer_one_node;
      lss->data.info.dim[n] = n;

      rss->data.info.start[n] = loop.from[n];
      rss->data.info.delta[n] = integer_zero_node;
      rss->data.info.stride[n] = integer_one_node;
      rss->data.info.dim[n] = n;
    }

  /* Setup the SS.  */
  tmp = g95_conv_array_base (dest);
  lss->data.info.data = g95_simple_fold (tmp, phead, ptail, NULL);
  lss->data.info.descriptor = dest;
  lss->type = G95_SS_SECTION;
  lss->next = g95_ss_terminator;
  lss->data.info.dimen = dimen;

  tmp = g95_conv_array_base (src);
  rss->data.info.data = g95_simple_fold (tmp, phead, ptail, NULL);
  rss->data.info.descriptor = src;
  rss->type = G95_SS_SECTION;
  rss->next = g95_ss_terminator;
  rss->data.info.dimen = dimen;

  g95_add_ss_to_loop (&loop, lss);
  g95_add_ss_to_loop (&loop, rss);

  /* The loop body.  */
  g95_mark_ss_chain_used (lss, 1);
  g95_mark_ss_chain_used (rss, 1);
  g95_start_scalarized_body (&loop);

  g95_init_se (&lse, NULL);
  g95_init_se (&rse, NULL);
  lse.ss = lss;
  lse.loop = &loop;
  rse.ss = rss;
  rse.loop = &loop;

  g95_conv_scalarized_array_ref (&lse, NULL);
  g95_conv_scalarized_array_ref (&rse, NULL);

  tmp = build (MODIFY_EXPR, TREE_TYPE (lse.expr), lse.expr, rse.expr);
  stmt = build_stmt (EXPR_STMT, tmp);

  /* Piece the loop body together.  */
  g95_add_stmt_to_pre (&lse, rse.pre, rse.pre_tail);
  g95_add_stmt_to_pre (&lse, stmt, stmt);
  g95_add_stmt_to_pre (&lse, rse.post, rse.post_tail);
  g95_add_stmt_to_pre (&lse, lse.post, lse.post_tail);

  g95_trans_scalarizing_loops (&loop, lse.pre, lse.pre_tail);

  g95_add_stmt_to_list (phead, ptail, loop.pre, loop.pre_tail);
  g95_add_stmt_to_list (phead, ptail, loop.post, loop.post_tail);
}

/* Modify the descriptor of an array parameter so that it has the
   correct lower bound.  Also move the upper bound accordingly.
   If the array is not packed, it will be copied into a temporary.
   For each dimension we set the new lower and upper bounds.  Then we copy the
   stride and calculate the offset for this dimension.  We also work out
   what the stride of a packed array would be, and see it the two match.
   If the array need repacking, we set the stride to the values we just
   calculated, recalculate the offset and copy the array data.
   Code is also added to copy the data back at the end of the function.
   */
/*GCC ARRAYS*/
tree
g95_trans_dummy_array_bias (g95_symbol * sym, tree tmpdesc, tree body)
{
  tree type;
  tree dumdesc;
  tree head;
  tree tail;
  tree stmt;
  g95_se se;
  int n;
  int repack;
  tree tmp;
  tree tmpvar;
  tree stride;
  tree stridevar;
  tree strides[G95_MAX_DIMENSIONS];
  tree offset;
  tree offsetvar;
  tree ref;
  tree lbound[G95_MAX_DIMENSIONS];
  tree ubound;
  tree uboundvar;
  tree lboundvar;
  tree args;
  tree needpack;
  tree packedvar;
  tree eqvar;
  tree repack_stmt;
  tree nopack_stmt;
  tree unpack;
  tree pack_tail;
  tree oldstride;
  tree base;
  int checkparm;
  locus loc;

  if (sym->attr.pointer || sym->attr.allocatable)
    abort();

  if (sym->ts.type == BT_CHARACTER)
    g95_todo_error ("Character string array dummy parameters");

  g95_get_backend_locus (&loc);
  g95_set_backend_locus (&sym->declared_at);
  /* Descriptor type.  */
  type = TREE_TYPE (tmpdesc);
  assert (G95_DESCRIPTOR_TYPE_P (type));

  /* The actual argument descriptor.  */
  dumdesc = build1 (INDIRECT_REF, type, G95_DECL_SAVED_DESCRIPTOR (tmpdesc));

  tmpvar = NULL_TREE;
  offset = integer_zero_node;
  offsetvar = NULL_TREE;
  stride = integer_one_node;
  stridevar = NULL_TREE;
  uboundvar = NULL_TREE;
  lboundvar = NULL_TREE;
  eqvar = NULL_TREE;

  checkparm = (sym->as->type == AS_EXPLICIT && flag_bounds_check);

  if (g95_option.no_repack_arrays)
    {
      repack = 0;
      unpack = NULL_TREE;
      base = NULL_TREE;
    }
  else
    {
      repack = 1;
      unpack = create_tmp_var (boolean_type_node, "unpack");
      /* We need to save the passed descriptor base.  */
      base = create_tmp_var (g95_array_dataptr_type (dumdesc), "base");
      //base = create_tmp_var (integer_type_node, "base");
    }

  g95_start_stmt ();

  head = tail = NULL_TREE;

  if (repack)
    {
      /* Save the old base.  */
      tmp = g95_conv_descriptor_base (dumdesc);
      tmp = build (MODIFY_EXPR, TREE_TYPE (base), base, tmp);
      stmt = build_stmt (EXPR_STMT, tmp);
      g95_add_stmt_to_list (&head, &tail, stmt, stmt);

      needpack = packedvar = create_tmp_var (boolean_type_node, "repack");
    }
  else
    needpack = packedvar = NULL_TREE;

  /* Set the dtype.  */
  tmp = g95_conv_descriptor_dtype (tmpdesc);
  tmp = build (MODIFY_EXPR, TREE_TYPE (tmp), tmp,
               G95_TYPE_ARRAY_DTYPE(TREE_TYPE (tmpdesc)));
  stmt = build_stmt (EXPR_STMT, tmp);
  g95_add_stmt_to_list (&head, &tail, stmt, stmt);
  oldstride = create_tmp_var (g95_array_index_type, "stride");

  for (n = 0; n < sym->as->rank; n++)
    {
      /* Set the desired lower bound.  */
      assert (sym->as->lower[n]);
      g95_init_se (&se, NULL);
      g95_conv_simple_val_type (&se, sym->as->lower[n], g95_array_index_type);
      g95_add_stmt_to_list (&head, &tail, se.pre, se.pre_tail);
      lbound[n] = se.expr;

      tmp = g95_conv_descriptor_lbound (tmpdesc, g95_rank_cst[n]);
      tmp = build (MODIFY_EXPR, TREE_TYPE (tmp), tmp, lbound[n]);
      stmt = build_stmt (EXPR_STMT, tmp);
      g95_add_stmt_to_list (&head, &tail, stmt, stmt);

      if (checkparm || ! sym->as->upper[n])
        {
          /* Get the upper bound of the actual parameter.  */
          ubound = g95_conv_descriptor_ubound (dumdesc, g95_rank_cst[n]);
          ubound = g95_simple_fold (ubound, &head, &tail, &uboundvar);
        }
      else
        ubound = NULL_TREE;

      if (checkparm || ! sym->as->upper[n])
        {
          /* Get the lower bound of the actual parameter.  */
          ref = g95_conv_descriptor_lbound (dumdesc, g95_rank_cst[n]);
          ref = g95_simple_fold_tmp (ref, &head, &tail, &lboundvar);
        }
      else
        ref = NULL_TREE;

      /* Set the desired upper bound.  */
      if (sym->as->upper[n])
        {
          /* We know what we want the upper bound to be.  */
          g95_init_se (&se, NULL);
          g95_conv_simple_val_type (&se, sym->as->upper[n],
                                   g95_array_index_type);
          g95_add_stmt_to_list (&head, &tail, se.pre, se.pre_tail);

          /* Check the sizes match.  */
          if (checkparm)
            {
              /* Check (ubound(a) - lbound(a) == ubound(b) - lbound(b)).
                       (ubound(a) - lbound(a) + lbound(b) == ubound(b)).  */

              tmp = build (MINUS_EXPR, g95_array_index_type, se.expr,
                          lbound[n]);
              tmp = g95_simple_fold (tmp, &head, &tail, &tmpvar);
              tmp = build (PLUS_EXPR, g95_array_index_type, tmp, ref);
              tmp = g95_simple_fold (tmp, &head, &tail, &tmpvar);

              tmp = build (NE_EXPR, boolean_type_node, tmp, ubound);
              g95_trans_runtime_check (tmp, g95_strconst_bounds, &head, &tail);
            }
          ubound = se.expr;
        }
      else
        {
          /* For assumed shape arrays move the upper bound by the same amount
             as the lower bound.  */
          tmp = build (MINUS_EXPR, g95_array_index_type, se.expr, ref);
          tmp = g95_simple_fold (tmp, &head, &tail, &tmpvar);

          ubound = build (PLUS_EXPR, g95_array_index_type, ubound, tmp);
          ubound = g95_simple_fold_tmp (ubound, &head, &tail, &uboundvar);
        }

      /* ubound is now the upper bound of the temporary.  */
      /* Store the new upper bound.  */
      tmp = g95_conv_descriptor_ubound (tmpdesc, g95_rank_cst[n]);
      tmp = build (MODIFY_EXPR, TREE_TYPE (tmp), tmp, ubound);
      stmt = build_stmt (EXPR_STMT, tmp);
      g95_add_stmt_to_list (&head, &tail, stmt, stmt);

      /* Get the passed stride.  */
      tmp = g95_conv_descriptor_stride (dumdesc, g95_rank_cst[n]);
      tmp = build (MODIFY_EXPR, TREE_TYPE (oldstride), oldstride, tmp);
      stmt = build_stmt (EXPR_STMT, tmp);
      g95_add_stmt_to_list (&head, &tail, stmt, stmt);


      /* Now we check the stride to see if the array is packed.  */
      if (repack)
        {
          /* We save the packed strides, they may be needed later.  */
          strides[n] = stride;
          stridevar = NULL_TREE;

          if (n == 0)
            {
              /* The first dimension gets special handling.  */
              /* A stride of 0 means were passed a disposable temporary.
                 Otherwise we were passed an array section, so must
                 copy the result back.  The unpack variable needs function
                 scope.  */
              tmp = build (NE_EXPR, boolean_type_node, oldstride,
                          integer_zero_node);
              tmp = build (MODIFY_EXPR, TREE_TYPE (unpack), unpack, tmp);
              stmt = build_stmt (EXPR_STMT, tmp);
              g95_add_stmt_to_list (&head, &tail, stmt, stmt);

              /* Block to execute if we are passed a temporary.  */
              g95_start_stmt ();

              nopack_stmt = pack_tail = NULL_TREE;

              /* Set both strides to 1.  */
              tmp = g95_conv_descriptor_stride (dumdesc, g95_rank_cst[n]);
              tmp = build (MODIFY_EXPR, TREE_TYPE (tmp), tmp, integer_one_node);
              stmt = build_stmt (EXPR_STMT, tmp);
              g95_add_stmt_to_list (&nopack_stmt, &pack_tail, stmt, stmt);

              tmp = build (MODIFY_EXPR, TREE_TYPE (oldstride), oldstride,
                          integer_one_node);
              stmt = build_stmt (EXPR_STMT, tmp);
              g95_add_stmt_to_list (&nopack_stmt, &pack_tail, stmt, stmt);


              /* It must be a packed array.  */
              tmp = build (MODIFY_EXPR, TREE_TYPE (needpack), needpack,
                          integer_zero_node);
              stmt = build_stmt (EXPR_STMT, tmp);
              g95_add_stmt_to_list (&nopack_stmt, &pack_tail, stmt, stmt);

              nopack_stmt = g95_finish_stmt (nopack_stmt, pack_tail);
              /* End of block.  */

              /* Block to execute if we are passed an array section.  */
              g95_start_stmt ();

              repack_stmt = pack_tail = NULL_TREE;

              /* Test if the array is already packed.  */
              needpack = build (NE_EXPR, boolean_type_node, stride, oldstride);
              needpack = g95_simple_fold_tmp (needpack, &repack_stmt,
                                             &pack_tail, &packedvar);

              repack_stmt = g95_finish_stmt (repack_stmt, pack_tail);
              /* End of block.  */

              stmt = build_stmt (IF_STMT, unpack, repack_stmt, nopack_stmt);
              g95_add_stmt_to_list (&head, &tail, stmt, stmt);
            }
          else if (G95_DECL_PACKED_ARRAY (tmpdesc))
            {
              /* Test if the array is already packed.  */
              tmp = build (NE_EXPR, boolean_type_node, oldstride, stride);
              tmp = g95_simple_fold (tmp, &head, &tail, &eqvar);

              needpack =
                g95_simple_fold_tmp (needpack, &head, &tail, &packedvar);
              tmp = build (TRUTH_OR_EXPR, TREE_TYPE (tmp), needpack, tmp);
              tmp = build (MODIFY_EXPR, TREE_TYPE (tmp), needpack, tmp);
              stmt = build_stmt (EXPR_STMT, tmp);
              g95_add_stmt_to_list (&head, &tail, stmt, stmt);
            }
          /* For partial packed arrays we just test the first stride.  */
        }
      else if (n == 0)
        {
          /* Even if we never repack the array, we still need to set the
             stride of passed temporaries to 1.  */
          g95_start_stmt ();
          repack_stmt = pack_tail = NULL_TREE;

          tmp = g95_conv_descriptor_stride (dumdesc, g95_rank_cst[n]);
          tmp = build (MODIFY_EXPR, TREE_TYPE (tmp), tmp, integer_one_node);
          stmt = build_stmt (EXPR_STMT, tmp);
          g95_add_stmt_to_list (&repack_stmt, &pack_tail, stmt, stmt);

          tmp = build (MODIFY_EXPR, g95_array_index_type, oldstride,
                      integer_one_node);
          stmt = build_stmt (EXPR_STMT, tmp);
          g95_add_stmt_to_list (&repack_stmt, &pack_tail, stmt, stmt);


          repack_stmt = g95_finish_stmt (repack_stmt, pack_tail);

          tmp = build (EQ_EXPR, g95_array_index_type, oldstride,
                      integer_zero_node);
          stmt = build_stmt (IF_STMT, tmp, repack_stmt, NULL_TREE);
          g95_add_stmt_to_list (&head, &tail, stmt, stmt);
        }

      /* Store the passed stride.  Will be overwritten if array is repacked.  */
      ref = g95_conv_descriptor_stride (tmpdesc, g95_rank_cst[n]);
      ref = build (MODIFY_EXPR, TREE_TYPE (ref), ref, oldstride);
      stmt = build_stmt (EXPR_STMT, ref);
      g95_add_stmt_to_list (&head, &tail, stmt, stmt);

      /* Calculate the offset of the unpacked array.  */
      tmp = build (MULT_EXPR, g95_array_index_type, oldstride, lbound[n]);
      tmp = g95_simple_fold (tmp, &head, &tail, &tmpvar);

      offset = build (MINUS_EXPR, TREE_TYPE (tmp), offset, tmp);
      offset = g95_simple_fold_tmp (offset, &head, &tail, &offsetvar);

      /* Calculate the next packed stride.  */
      tmp = build (MINUS_EXPR, g95_array_index_type, integer_one_node,
                   lbound[n]);
      tmp = g95_simple_fold (tmp, &head, &tail, &tmpvar);
      tmp = build (PLUS_EXPR, TREE_TYPE (tmp), ubound, tmp);
      tmp = g95_simple_fold (tmp, &head, &tail, &tmpvar);
      stride = build (MULT_EXPR, TREE_TYPE (tmp), stride, tmp);
      stride = g95_simple_fold_tmp (stride, &head, &tail, &stridevar);
    }

  if (repack)
    {
      tree packoffset;
      tree pointervar;
      /* We will do this if the array needs repacking.  */
      g95_start_stmt ();

      repack_stmt = pack_tail = NULL_TREE;
      pointervar = NULL_TREE;

      /* Allocate storage for the array.  */
      ref = g95_conv_descriptor_data (tmpdesc);
      ref = build1 (ADDR_EXPR, ppvoid_type_node, ref);
      ref = g95_simple_fold (ref, &repack_stmt, &pack_tail, NULL);

      tmp = TYPE_SIZE_UNIT (g95_get_element_type (type));
      stride = build (MULT_EXPR, g95_array_index_type, stride, tmp);
      stride = g95_simple_fold (stride, &repack_stmt, &pack_tail, &stridevar);

      args = g95_chainon_list (NULL_TREE, ref);
      args = g95_chainon_list (args, stride);

      /* Allocate memory to hold the packed array.  This memory will be freed
         automatically when the procedure exits.  */
      if (g95_array_index_kind == 4)
        tmp = gfor_fndecl_internal_malloc;
      else if (g95_array_index_kind == 8)
        tmp = gfor_fndecl_internal_malloc64;
      else
        abort();
      tmp = g95_build_function_call (tmp, args);
      stmt = build_stmt (EXPR_STMT, tmp);
      g95_add_stmt_to_list (&repack_stmt, &pack_tail, stmt, stmt);

      /* Calculate the base pointer for the parameter.  */
      tmp = g95_conv_descriptor_data (dumdesc);
      tmp = g95_simple_fold (tmp, &repack_stmt, &pack_tail, &pointervar);
      tmp = build1 (INDIRECT_REF, TREE_TYPE (TREE_TYPE (tmp)), tmp);
      tmp = build (ARRAY_REF, TREE_TYPE (TREE_TYPE (tmp)), tmp, offset);
      tmp = build1 (ADDR_EXPR, g95_array_dataptr_type (dumdesc), tmp);

      ref = g95_conv_descriptor_base (dumdesc);
      tmp = build (MODIFY_EXPR, TREE_TYPE (ref), ref, tmp);
      stmt = build_stmt (EXPR_STMT, tmp);
      g95_add_stmt_to_list (&nopack_stmt, &pack_tail, stmt, stmt);

      packoffset = integer_zero_node;
      /* Set the stride, and calculate the offset.  */
      for (n = 0; n < sym->as->rank; n++)
        {
          ref = g95_conv_descriptor_stride (tmpdesc, g95_rank_cst[n]);
          tmp = build (MODIFY_EXPR, g95_array_index_type, ref, strides[n]);
          stmt = build_stmt (EXPR_STMT, tmp);
          g95_add_stmt_to_list (&repack_stmt, &pack_tail, stmt, stmt);

          if (n == 0)
            tmp = integer_one_node;
          else
            tmp = strides[n];
          tmp = build (MULT_EXPR, g95_array_index_type, tmp, lbound[n]);
          tmp = g95_simple_fold (tmp, &repack_stmt, &pack_tail,
                                (n == 0) ? &offsetvar : &tmpvar);

          packoffset = build (MINUS_EXPR, g95_array_index_type, packoffset,
                             tmp);
          packoffset = g95_simple_fold_tmp (packoffset, &repack_stmt,
                                           &pack_tail, &offsetvar);
        }

      /* Calculate the base pointer for the temp.  */
      tmp = g95_conv_descriptor_data (tmpdesc);
      tmp = g95_simple_fold (tmp, &nopack_stmt, &pack_tail, &pointervar);
      tmp = build1 (INDIRECT_REF, TREE_TYPE (TREE_TYPE (tmp)), tmp);
      tmp = build (ARRAY_REF, TREE_TYPE (TREE_TYPE (tmp)), tmp, packoffset);
      tmp = build1 (ADDR_EXPR, g95_array_dataptr_type (tmpdesc), tmp);

      ref = g95_conv_descriptor_base (tmpdesc);
      tmp = build (MODIFY_EXPR, TREE_TYPE (ref), ref, tmp);
      stmt = build_stmt (EXPR_STMT, tmp);
      g95_add_stmt_to_list (&nopack_stmt, &pack_tail, stmt, stmt);

      if (sym->attr.dummy && sym->attr.intent != INTENT_OUT)
        {
          /* Copy the array data.  */
          g95_trans_repack_array (&repack_stmt, &pack_tail, tmpdesc,
                                 dumdesc, sym->as->rank);
        }

      repack_stmt = g95_finish_stmt (repack_stmt, pack_tail);
      /* End of code to repack the array.  */

      /* Create a block containing code for when the data is already
         packed.  */
      g95_start_stmt ();
    }

  /* This will be executed if we don't repack the array.  */
  /* Set the data pointer.  */
  nopack_stmt = pack_tail = NULL_TREE;
  tmp = g95_conv_descriptor_data (dumdesc);
  ref = g95_conv_descriptor_data (tmpdesc);
  tmp = build (MODIFY_EXPR, TREE_TYPE (ref), ref, tmp);
  stmt = build_stmt (EXPR_STMT, tmp);
  g95_add_stmt_to_list (&nopack_stmt, &pack_tail, stmt, stmt);

  /* Calculate the base pointer.  Use the offset we calculated earlier.  */
  tmp = g95_conv_descriptor_data (tmpdesc);
  tmp = g95_simple_fold (tmp, &nopack_stmt, &pack_tail, NULL);
  tmp = build1 (INDIRECT_REF, TREE_TYPE (TREE_TYPE (tmp)), tmp);
  tmp = build (ARRAY_REF, TREE_TYPE (TREE_TYPE (tmp)), tmp, offset);
  tmp = build1 (ADDR_EXPR, g95_array_dataptr_type (tmpdesc), tmp);

  ref = g95_conv_descriptor_base (tmpdesc);
  tmp = build (MODIFY_EXPR, TREE_TYPE (ref), ref, tmp);
  stmt = build_stmt (EXPR_STMT, tmp);
  g95_add_stmt_to_list (&nopack_stmt, &pack_tail, stmt, stmt);

  if (repack)
    {
      /* If the array wan't repacked, we don't need to unpack it afterwards.  */
      tmp = build (MODIFY_EXPR, TREE_TYPE (unpack), unpack, integer_zero_node);
      stmt = build_stmt (EXPR_STMT, tmp);
      g95_add_stmt_to_list (&nopack_stmt, &pack_tail, stmt, stmt);

      nopack_stmt = g95_finish_stmt (nopack_stmt, pack_tail);
      /* End of no-repack block.  */

      /* Choose which block.  */
      stmt = build_stmt (IF_STMT, needpack, repack_stmt, nopack_stmt);

      g95_add_stmt_to_list (&head, &tail, stmt, stmt);
    }
  else
    {
      /* We always want the no-repack code.  */
      g95_add_stmt_to_list (&head, &tail, nopack_stmt, pack_tail);
    }

  stmt = g95_finish_stmt (head, tail);

  /* Add to start of function body.  */
  body = chainon (stmt, body);

  g95_set_backend_locus (&loc);
  if (repack
      && ! (sym->attr.dummy && sym->attr.intent == INTENT_IN))
    {
      /* Build code to unpack the array data.  */
      g95_start_stmt ();
      head = tail = NULL_TREE;

      g95_trans_repack_array (&head, &tail, dumdesc, tmpdesc, sym->as->rank);

      stmt = g95_finish_stmt (head, tail);

      /* If unpack is set, we must copy the array data back to
         the passed parameter.  */
      stmt = build_stmt (IF_STMT, unpack, stmt, NULL_TREE);

      /* Add to end of function body.  */
      body = chainon (body, stmt);
    }

  if (repack)
    {
      /* Restore the original base.  */
      tmp = g95_conv_descriptor_base (dumdesc);
      tmp = build (MODIFY_EXPR, TREE_TYPE (base), tmp, base);
      stmt = build_stmt (EXPR_STMT, tmp);
      body = chainon (body, stmt);
    }

  return body;
}

/* Convert an array for passing as an actual function parameter.  Expressions
   and vector subscripts are evaluated and stored in a teporary, which is then
   passed.  For whole arrays the descriptor is passed.  For array sections
   a modified copy of the descriptor is passed, but using the original data. */
void
g95_conv_array_parameter (g95_se * se, g95_expr * expr, g95_ss * ss)
{
  g95_loopinfo loop;
  g95_ss * secss;
  int need_tmp;
  int n;
  tree tmp;
  tree stmt;
  tree desc;

  assert (ss != g95_ss_terminator);

  if (expr->ts.type == BT_CHARACTER)
    g95_todo_error ("Character string array actual parameters");

  g95_init_loopinfo (&loop);

  ss = g95_reverse_ss (ss);

  /* Associate the SS with the loop.  */
  g95_add_ss_to_loop (&loop, ss);

  /* TODO: Pass constant array constructors without a temporary.  */
  /* If we have a linear array section, we can pass it directly.  Otherwise
     we need to copy it into a temporary.  */
  if (expr->expr_type == EXPR_VARIABLE)
    {
      g95_ss *vss;

      /* Find the SS for the array section.  */
      secss = ss;
      while (secss != g95_ss_terminator
          && secss->type != G95_SS_SECTION)
        secss = secss->next;

      assert (secss != g95_ss_terminator);

      need_tmp = 0;
      for (n = 0; n < secss->data.info.dimen; n++)
        {
          vss = secss->data.info.subscript[secss->data.info.dim[n]];
          if (vss && vss->type == G95_SS_VECTOR)
            need_tmp = 1;
        }
    }
  else
    {
      need_tmp = 1;
      secss = NULL;
    }

  /* Tell the scalarizer not to bother creating loop varliables, etc.  */
  if (! need_tmp)
    loop.array_parameter = 1;

  /* Setup the scalarizing loops and bounds.  */
  g95_conv_ss_startstride (&loop);

  if (need_tmp)
    {
      /* Tell the scalarizer to make a temporary.  */
      if (expr->ts.type == BT_CHARACTER)
        g95_todo_error ("Passing character string expressions");
      loop.temp_ss = g95_get_ss ();
      loop.temp_ss->type = G95_SS_TEMP;
      loop.temp_ss->next = g95_ss_terminator;
      loop.temp_ss->data.temp.type = g95_typenode_for_spec (&expr->ts);
      loop.temp_ss->data.temp.string_length = NULL;
      loop.temp_ss->data.temp.dimen = loop.dimen;
      g95_add_ss_to_loop (&loop, loop.temp_ss);
    }

  g95_conv_loop_setup (&loop);

  if (need_tmp)
    {
      /* Copy into a temporary and pass that.  We don't need to copy the data
         back because expressions and vector subscripts must be INTENT_IN.  */
      g95_se lse;
      g95_se rse;

      /* TODO: Optimize passing function return values.  */
      g95_mark_ss_chain_used (loop.temp_ss, 1);
      g95_mark_ss_chain_used (ss, 1);
      g95_start_scalarized_body (&loop);

      g95_init_se (&lse, NULL);
      g95_init_se (&rse, NULL);
      g95_copy_loopinfo_to_se (&lse, &loop);
      g95_copy_loopinfo_to_se (&rse, &loop);
      lse.ss = loop.temp_ss;
      rse.ss = ss;

      g95_conv_scalarized_array_ref (&lse, NULL);
      g95_conv_simple_rhs (&rse, expr);

      tmp = build (MODIFY_EXPR, TREE_TYPE (rse.expr), lse.expr, rse.expr);
      stmt = build_stmt (EXPR_STMT, tmp);

      g95_add_stmt_to_pre (&lse, rse.pre, rse.pre_tail);
      g95_add_stmt_to_pre (&lse, stmt, stmt);
      g95_add_stmt_to_pre (&lse, rse.post, rse.post_tail);
      g95_add_stmt_to_pre (&lse, lse.post, lse.post_tail);

      g95_trans_scalarizing_loops (&loop, lse.pre, lse.pre_tail);

      /* Set the first stride component to zero to indicate a temporary.  */
      desc = loop.temp_ss->data.info.descriptor;
      tmp = g95_conv_descriptor_stride (desc, g95_rank_cst[0]);
      tmp = build (MODIFY_EXPR, TREE_TYPE (tmp), tmp, integer_zero_node);
      stmt = build_stmt (EXPR_STMT, tmp);
      g95_add_stmt_to_pre (&loop, stmt, stmt);

      assert (is_simple_varname (desc));
      TREE_ADDRESSABLE (desc) = 1;
      se->expr = build1 (ADDR_EXPR, build_pointer_type (TREE_TYPE (desc)),
                        desc);
      se->expr = g95_simple_fold (se->expr, &loop.pre, &loop.pre_tail, NULL);
    }
  else
    {
      /* We pass sections without copying to a temporary.  A function may
         decide to repack the array to speed up access, but we're not
         bothered about that here.  */
      int dim;
      tree type;
      tree parm;
      tree parmtype;
      tree offset;
      tree offsetvar;
      tree tmpvar;
      tree stride;
      tree stridevar;
      g95_ss_info *info;
      int full;

      info = &secss->data.info;
      type = TREE_TYPE (info->descriptor);
      desc = info->descriptor;

      full = 1;
      if (G95_ARRAY_TYPE_P(TREE_TYPE (desc)))
        {
          /* Create a descriptor if the array doesn't have one.  */
          full = 0;
        }
      else if (info->ref->u.ar.type != AR_FULL)
        {
          assert (info->ref->u.ar.type == AR_SECTION);

          for (n = 0; n < info->ref->u.ar.dimen; n++)
            {
              /* Detect passing the full array as a section.  This could do
                 even more checking, but it doesn't seem worth it.  */
              if (info->ref->u.ar.start[n]
                  || info->ref->u.ar.end[n]
                  || ! g95_expr_is_one (info->ref->u.ar.stride[n], 0))
                {
                  full = 0;
                  break;
                }
            }
        }
      if (full)
        {
          /* We pass full arrays directly.  This means that pointers and
             allocatable arrays should work.  */
          if (TREE_CODE (desc) == INDIRECT_REF)
            se->expr = TREE_OPERAND (desc, 0);
          else
            {
              assert (is_simple_varname (desc));
              if (is_simple_id (desc))
                TREE_ADDRESSABLE (desc) = 1;
              se->expr = build1 (ADDR_EXPR, build_pointer_type (type), desc);
              se->expr = g95_simple_fold (se->expr, &loop.pre,
                                         &loop.pre_tail, NULL);
            }
        }
      else
        {
          /* Otherwise make a copy of the descriptor and point it at
             the section we want.  The loop variable limits will be the limits
             of the section.  */
          assert (secss && secss != g95_ss_terminator);
          parmtype = g95_get_element_type (type);
          parmtype = g95_get_array_type_bounds (parmtype, loop.dimen,
                                               loop.from, loop.to);
          parm = create_tmp_var (parmtype, "parm");

          offset = integer_zero_node;
          offsetvar = NULL_TREE;
          stridevar = NULL_TREE;
          tmpvar = NULL_TREE;
          dim = 0;

          /* The following can be somewhat confusing.  We have two
             descriptors, a temporary and the original array.
             {parm, parmtype, dim} refer to the temporary.
             {desc, type, n, secss, loop} refer to the original.
             The bounds of the scaralization are the bounds of the section.
             We don't have to worry about numeric overflows when calculating
             the offsets because all elements are within the array data.  */

          /* Set the dtype.  */
          tmp = g95_conv_descriptor_dtype (parm);
          tmp = build (MODIFY_EXPR, TREE_TYPE (tmp), tmp,
                       G95_TYPE_ARRAY_DTYPE (parmtype));
          stmt = build_stmt (EXPR_STMT, tmp);
          g95_add_stmt_to_pre (&loop, stmt, stmt);

          for (n = 0; n < info->ref->u.ar.dimen; n++)
            {
              /* Work out the offset.  */
              tmp = g95_conv_array_lbound (desc, n);
              tmp = g95_simple_fold (tmp, &loop.pre, &loop.pre_tail, &tmpvar);

              tmp = build (MINUS_EXPR, TREE_TYPE (tmp), info->start[dim], tmp);
              tmp = g95_simple_fold (tmp, &loop.pre, &loop.pre_tail, &tmpvar);

              stride = g95_conv_array_stride (desc, n);
              stride = g95_simple_fold_tmp (stride, &loop.pre, &loop.pre_tail,
                                       &stridevar);

              tmp = build (MULT_EXPR, TREE_TYPE (tmp), tmp, stride);
              tmp = g95_simple_fold (tmp, &loop.pre, &loop.pre_tail,
                                    &tmpvar);

              offset = build (PLUS_EXPR, TREE_TYPE (tmp), offset, tmp);
              offset = g95_simple_fold_tmp (offset, &loop.pre, &loop.pre_tail,
                                       &offsetvar);

              if (info->ref->u.ar.dimen_type[n] == DIMEN_ELEMENT)
                {
                  /* For elemental dimensions, we only need the offset.  */
                  continue;
                }

              /* Check we haven't somehow got out of sync.  */
              assert (info->dim[dim] == n);

              /* Vector subscripts need copying and are handled elsewhere.  */
              assert (info->ref->u.ar.dimen_type[n] == DIMEN_RANGE);

              /* Set the new lower bound.  */
              tmp = g95_conv_descriptor_lbound (parm, g95_rank_cst[dim]);
              tmp = build (MODIFY_EXPR, TREE_TYPE (tmp), tmp, loop.from[dim]);
              stmt = build_stmt (EXPR_STMT, tmp);
              g95_add_stmt_to_pre (&loop, stmt, stmt);

              /* Set the new upper bound.  */
              tmp = g95_conv_descriptor_ubound (parm, g95_rank_cst[dim]);
              tmp = build (MODIFY_EXPR, TREE_TYPE (tmp), tmp, loop.to[dim]);
              stmt = build_stmt (EXPR_STMT, tmp);
              g95_add_stmt_to_pre (&loop, stmt, stmt);

              /* Multiply the stride by the section stride to get the
                 total stride.  */
              stride = fold (build (MULT_EXPR, g95_array_index_type, stride,
                             info->stride[n]));
              tmp = g95_conv_descriptor_stride (parm, g95_rank_cst[dim]);
              tmp = build (MODIFY_EXPR, TREE_TYPE (tmp), tmp, stride);
              stmt = build_stmt (EXPR_STMT, tmp);
              g95_add_stmt_to_pre (&loop, stmt, stmt);

              dim++;
            }

          /* Point the data pointer at the first element in the section.  */
          tmp = g95_conv_array_data (desc);
          if (TREE_CODE (tmp) == INDIRECT_REF)
            tmp = TREE_OPERAND (tmp, 0);
          else
            {
              tmp = g95_simple_fold (tmp, &loop.pre, &loop.pre_tail, NULL);
              tmp = build1 (INDIRECT_REF, TREE_TYPE (TREE_TYPE (tmp)), tmp);
            }
          tmp = build (ARRAY_REF, TREE_TYPE (TREE_TYPE (tmp)), tmp, offset);
          offset = build1 (ADDR_EXPR, g95_array_dataptr_type (desc), tmp);

          tmp = g95_conv_descriptor_data (parm);
          tmp = build (MODIFY_EXPR, TREE_TYPE (tmp), tmp, offset);
          stmt = build_stmt (EXPR_STMT, tmp);
          g95_add_stmt_to_pre (&loop, stmt, stmt);

          /* Invaidate the base pointer.  */
          tmp = g95_conv_descriptor_base (parm);
          tmp = build (MODIFY_EXPR, TREE_TYPE (tmp), tmp, integer_zero_node);
          stmt = build_stmt (EXPR_STMT, tmp);
          g95_add_stmt_to_pre (&loop, stmt, stmt);

          /* Get a pointer to the new descriptor.  */
          se->expr = build1 (ADDR_EXPR, build_pointer_type (parmtype), parm);
          se->expr =
            g95_simple_fold (se->expr, &loop.pre, &loop.pre_tail, NULL);
          TREE_ADDRESSABLE (parm) = 1;
        }
    }

  g95_add_stmt_to_pre (se, loop.pre, loop.pre_tail);
  g95_add_stmt_to_post (se, loop.post, loop.post_tail);

  /* Cleanup the scalarizer.  */
  g95_cleanup_loop (&loop);
}

/* NULLIFY an allocated array on function entry, free it on exit.  */
tree
g95_trans_deferred_array (g95_symbol * sym, tree body)
{
  tree type;
  tree var;
  tree tmp;
  tree descriptor;
  tree stmt;
  tree deallocate;
  tree head;
  tree tail;
  locus loc;

  /* Make sure the frontend gets these right.  */
  if (! (sym->attr.pointer || sym->attr.allocatable))
    fatal_error ("Possible frontend bug: Deferred array size without pointer or allocatable attribute.");

  assert (TREE_CODE (sym->backend_decl) == VAR_DECL);
  if (G95_DECL_STRING (sym->backend_decl))
    {
      tmp = G95_DECL_STRING_LENGTH (sym->backend_decl);
      if (! INTEGER_CST_P (tmp))
        {
          head = tail = NULL_TREE;
          g95_conv_init_string_length (sym, &head, &tail);
          body = chainon (head, body);
        }
    }

  /* Parameter variables don't need anything special.  */
  if (sym->attr.dummy)
    return body;

  g95_get_backend_locus (&loc);
  g95_set_backend_locus (&sym->declared_at);
  descriptor = sym->backend_decl;

  if (TREE_STATIC (descriptor))
    {
      g95_todo_error ("static deferred shape array");
      return body;
    }

  /* Get the descriptor type.  */
  type = TREE_TYPE (sym->backend_decl);
  assert (G95_DESCRIPTOR_TYPE_P (type));

  /* NULLIFY the data pointer.  */
  tmp = g95_conv_descriptor_data (descriptor);
  tmp = build (MODIFY_EXPR, TREE_TYPE(tmp), tmp, integer_zero_node);
  stmt = build_stmt (EXPR_STMT, tmp);
  body = chainon (stmt, body);

  g95_set_backend_locus (&loc);
  /* Allocatable arrays need to be freed when they go out of scope.  */
  if (sym->attr.allocatable)
    {
      g95_start_stmt ();

      /* Deallocate if still allocated at the end of the procedure.  */
      g95_start_stmt ();
      deallocate = g95_array_deallocate (descriptor);
      deallocate = g95_finish_stmt (deallocate, NULL_TREE);

      tmp = g95_conv_descriptor_data (descriptor);
      var = create_tmp_var (TREE_TYPE (tmp), "ptr");
      tmp = build (MODIFY_EXPR, TREE_TYPE (tmp), var, tmp);
      stmt = build_stmt (EXPR_STMT, tmp);

      tmp = build (NE_EXPR, boolean_type_node, var, integer_zero_node);
      stmt = chainon (stmt, build_stmt (IF_STMT, tmp, deallocate, NULL_TREE));

      stmt = g95_finish_stmt (stmt, NULL_TREE);
      body = chainon (body, stmt);
    }

  return body;
}

/* Functions for walking an expression tree.  As the tree is traversed,
   scalar expressions are translated, array sections are remembered.  The list
   is created in reverse order, and must be reversed before passing to
   g95_conv_*.
   Scalar expressions are converted as they are seen, then substitited later.
   This has the added effect of moving scalar expressions outside the
   scalarization loop.  A return value equal to the passed chain means this is
   a scalar expression.  It is up to the caller to take whatever action is
   neccessary.

   Possible extension - multiple component subscripts.
    x(:,:) = foo%a(:)%b(:)
   Transforms to
    forall (i=..., j=...)
      x(i,j) = foo%a(j)%b(i)
    end forall
   This adds a fair amout of complexity because you need to deal with more
   than one ref.  Maybe handle in a similar manner to vector subscripts.  */

/* Walk a variable reference.  */
static g95_ss *
g95_walk_variable_expr (g95_ss * ss, g95_expr * expr)
{
  g95_ref *ref;
  g95_array_ref *ar;
  g95_ss *newss;
  g95_ss *head;
  int n;

  /* This is a scalar variable, or a scalar array reference.  Pass it back.  */
  if (!expr->symbol->attr.dimension)
    return ss;

  for (ref = expr->ref ; ref ; ref = ref->next)
    {
      if (ref->type != REF_ARRAY)
        continue;

      ar = &ref->u.ar;
      switch (ar->type)
        {
        case AR_ELEMENT:
          break;
        case AR_FULL:
          newss = g95_get_ss ();
          newss->type = G95_SS_SECTION;
          newss->expr = expr;
          newss->next = ss;
          newss->data.info.dimen = ar->as->rank;
          newss->data.info.ref = ref;

          /* Make sure array is the same as array(:,:), this way
             we don't need to special case all the time.  */
          ar->dimen = ar->as->rank;
          for (n = 0 ; n < ar->dimen ; n++)
          {
            newss->data.info.dim[n] = n;
            ar->dimen_type[n] = DIMEN_RANGE;

            assert (ar->start[n] == NULL);
            assert (ar->end[n] == NULL);
            assert (ar->stride[n] == NULL);
          }
          return newss;
        case AR_SECTION:
          newss = g95_get_ss ();
          newss->type = G95_SS_SECTION;
          newss->expr = expr;
          newss->next = ss;
          newss->data.info.dimen = 0;
          newss->data.info.ref = ref;

          head = newss;

          for (n = 0 ; n < ar->dimen ; n++)
            {
              g95_ss *indexss;

              switch (ar->dimen_type[n])
                {
                case DIMEN_ELEMENT:
                  /* Add SS for elemental (scalar) subscripts.  */
                  assert (ar->start[n]);
                  indexss = g95_get_ss();
                  indexss->type = G95_SS_SCALAR;
                  indexss->expr = ar->start[n];
                  indexss->next = g95_ss_terminator;
                  indexss->loop_chain = g95_ss_terminator;
                  newss->data.info.subscript[n] = indexss;
                  break;

                case DIMEN_RANGE:
                  newss->data.info.dim[newss->data.info.dimen] = n;
                  newss->data.info.dimen++;
                  break;

                case DIMEN_VECTOR:
                  /* Get a SS for the vector.  This will not be added to the
                      chain directly.  */
                  indexss =
                    g95_walk_expr (g95_ss_terminator, ar->start[n]);
                  if (indexss == g95_ss_terminator)
                    internal_error("scalar vector subscript???");

                  if (indexss->next != g95_ss_terminator)
                    g95_todo_error ("vector subscript expressions");
                  indexss->loop_chain = g95_ss_terminator;

                  /* Mark this as a vector subscript, and add it to the
                     existing SS for this term.  */
                  indexss->type = G95_SS_VECTOR;
                  newss->data.info.subscript[n] = indexss;
                  newss->data.info.dim[newss->data.info.dimen] = n;
                  newss->data.info.dimen++;
                  break;

                default:
                  /* We should know what sort of section it is by now.  */
                  abort ();
                }
            }
          /* We should have at least one non-elemental dimension.  */
          assert (newss->data.info.dimen > 0);
          return head;
        break;

        default:
          /* We should know what sort of section it is by now.  */
          abort();
        }

    }
  return ss;
}

/* Walk an expression operator. First walk the parameters.  Unary operators
   we pass back.  If only one operand of a binary expression is scalar, we must
   translate that expression, and add it to the SS chain.  */
static g95_ss *
g95_walk_op_expr (g95_ss * ss, g95_expr *expr)
{
  g95_ss *head;
  g95_ss *head2;
  g95_ss *newss;

  head = g95_walk_expr (ss, expr->op1);
  if (expr->op2 == NULL)
    head2 = head;
  else
    head2 = g95_walk_expr (head, expr->op2);

  /* All operands are scalar.  Pass back and let the caller deal with it.  */
  if (head2 == ss)
    return head2;

  /* All operands require scalarization. */
  if (head != ss
      && (expr->op2 == NULL || head2 != head))
    return head2;

  /* One of the operands needs scalarization, the other is scalar.  */
  /* Create a g95_ss for the scalar expression.  */
  newss = g95_get_ss ();
  if (head == ss)
    {
      /* First operand is scalar.  We build the chain in reverse order, so
         add the scarar SS after the second operand.  */
      head = head2;
      while (head && head->next != ss)
        head = head->next;
      /* Check we haven't somehow broken the chain.  */
      assert (head);
      newss->next = ss;
      head->next = newss;
      newss->expr = expr->op1;
    }
  else /* head2 == head */
    {
      assert (head2 == head);
      /* Second operand is scalar.  */
      newss->next = head2;
      head2 = newss;
      newss->expr = expr->op2;
    }

  return head2;
}

/* Walk the arguments of an intrinsic function.  */
g95_ss *
g95_walk_elemental_function_args (g95_ss * ss, g95_expr * expr,
                                  g95_ss_type type)
{
  g95_actual_arglist *arg;
  int scalar;
  g95_ss *head;
  g95_ss *tail;
  g95_ss *newss;

  head = ss;
  tail = NULL;
  scalar = 1;
  for (arg = expr->value.function.actual; arg; arg = arg->next)
    {
      if (! arg->expr)
        continue;

      newss = g95_walk_expr (ss, arg->expr);
      if (newss == ss)
        {
          /* Scalar argumet.  */
          newss = g95_get_ss ();
          newss->type = type;
          newss->expr = arg->expr;
          newss->next = head;
        }
      else
        scalar = 0;

      if (! tail)
        tail = newss;
      head = newss;
    }

  if (scalar && tail)
    {
      /* If all the arguments are scalar we don't need the argument SS.  */
      tail->next = g95_ss_terminator;
      g95_free_ss_chain (head);
      /* Pass it back.  */
      return ss;
    }

  return head;
}
/* Walk a function call.  Scalar functions are passed back, and taken out of
   scalarization loops.  For elemental functions we walk their arguments.
   The result of functions returning arrays is stored in a temporary outside
   the loop, so that the function is only called once.  */
static g95_ss *
g95_walk_function_expr (g95_ss * ss, g95_expr * expr)
{
  g95_ss *newss;
  g95_intrinsic_sym *isym;
  g95_symbol *sym;

  sym = expr->value.function.esym;
  isym = expr->value.function.isym;

  assert ((sym || isym) && ! (sym && isym));
  if (isym)
    return g95_walk_intrinsic_function (ss, expr, isym);

  /* A function that returns arrays.  */
  if (g95_return_by_reference (sym))
    {
      newss = g95_get_ss();
      newss->type = G95_SS_FUNCTION;
      newss->expr = expr;
      newss->next = ss;
      return newss;
    }

  /* Walk the parameters of an elemental function.  */
  if (sym->attr.elemental)
    return g95_walk_elemental_function_args (ss, expr, G95_SS_REFERENCE);

  /* Scalar functions are OK as these are evaluated outside the scalarisation
     loop.  Pass back and let the caller deal with it.  */
  return ss;
}

/* An array temporary is constructed for array constructors.  */
static g95_ss *
g95_walk_array_constructor (g95_ss * ss, g95_expr * expr)
{
  g95_ss *newss;
  int n;

  newss = g95_get_ss ();
  newss->type = G95_SS_CONSTRUCTOR;
  newss->expr = expr;
  newss->next = ss;
  newss->data.info.dimen = expr->rank;
  for (n = 0; n < expr->rank; n++)
    newss->data.info.dim[n] = n;

  return newss;
}

/* Walk an expresson.  Add walked expressions to the head of the SS chain.
   A wholy scalar expression will not be added.  */
g95_ss *
g95_walk_expr (g95_ss * ss, g95_expr * expr)
{
  g95_ss *head;

  switch (expr->expr_type)
    {
    case EXPR_VARIABLE:
      head = g95_walk_variable_expr (ss, expr);
      return head;

    case EXPR_OP:
      head = g95_walk_op_expr (ss, expr);
      return head;

    case EXPR_FUNCTION:
      head = g95_walk_function_expr (ss, expr);
      return head;

    case EXPR_CONSTANT:
    case EXPR_NULL:
    case EXPR_STRUCTURE:
      /* Pass back and let the caller deal with it.  */
      break;

    case EXPR_ARRAY:
      head = g95_walk_array_constructor (ss, expr);
      return head;

    case EXPR_SUBSTRING:
      /* Pass back and let the caller deal with it.  */
      break;

    default:
      internal_error("bad expression type during walk (%d)", expr->expr_type);
    }
  return ss;
}

