/* Array translation routines
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
#include "toplev.h"
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
#include "dependency.h"

static g95_ss * g95_walk_subexpr (g95_ss *, g95_expr *);
/* The contents of this structure aren't actualy used, just the address.  */
static g95_ss g95_ss_terminator_var;
g95_ss *g95_ss_terminator = &g95_ss_terminator_var;

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

/* Generate an initializer for a static pointer or allocatable array.  */
void
g95_trans_static_array_pointer (g95_symbol * sym)
{
  tree tmp;
  tree field;
  tree type;

  assert (TREE_STATIC (sym->backend_decl));
  /* Just zero the data member.  */
  type = TREE_TYPE (sym->backend_decl);
  assert (G95_DESCRIPTOR_TYPE_P (type));
  assert (DATA_FIELD == 0);
  field = TYPE_FIELDS (type);
  tmp = build_int_2 (0, 0);
  TREE_TYPE (tmp) = TREE_TYPE (field);
  tmp = tree_cons (field, tmp, NULL_TREE);
  tmp = build (CONSTRUCTOR, type, NULL_TREE, tmp);
  TREE_CONSTANT (tmp) = 1;
  DECL_INITIAL (sym->backend_decl) = tmp;
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

/* Generate code to allocate an array temporary, or create a variable to
   hold the data.  */
static void
g95_trans_allocate_array_storage (g95_loopinfo * loop, g95_ss_info * info,
                                  tree size, tree nelem)
{
  tree tmp;
  tree args;
  tree desc;
  tree data;

  desc = info->descriptor;
  data = g95_conv_descriptor_data (desc);
  if (g95_can_put_var_on_stack (size))
    {
      /* Make a temporary variable to hold the data.  */
      tmp = fold (build (MINUS_EXPR, TREE_TYPE (nelem), nelem,
                         integer_one_node));
      tmp = build_range_type (g95_array_index_type, integer_zero_node, tmp);
      tmp = build_array_type (g95_get_element_type (TREE_TYPE (desc)), tmp);
      tmp = g95_create_var (tmp, "A");
      TREE_ADDRESSABLE (tmp) = 1;
      tmp = build1 (ADDR_EXPR, TREE_TYPE (data), tmp);
      info->data = g95_evaluate_now (tmp, &loop->pre);

      tmp = build (MODIFY_EXPR, TREE_TYPE (data), data, info->data);
      g95_add_expr_to_block (&loop->pre, tmp);
      info->pdata = NULL_TREE;
    }
  else
    {
      /* Allocate memory to hold the data.  */
      info->pdata = build1 (ADDR_EXPR, ppvoid_type_node, data);

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
      g95_add_expr_to_block (&loop->pre, tmp);

      info->data = g95_evaluate_now (data, &loop->pre);
    }

  /* The offset is zero because we create temporaries with a zero
     lower bound.  */
  tmp = g95_conv_descriptor_base (desc);
  tmp = build_v (MODIFY_EXPR, tmp, info->data);
  g95_add_expr_to_block (&loop->pre, tmp);

  if (info->pdata != NULL_TREE)
    {
      /* Free the temporary.  */
      tmp = g95_chainon_list (NULL_TREE, info->pdata);
      tmp = g95_build_function_call (gfor_fndecl_internal_free, tmp);
      g95_add_expr_to_block (&loop->post, tmp);
    }
}

/* Generate code to allocate and initialize the descriptor for a temporary
   array.  Fills in info->descriptor and info->data.  Also adjusts the loop
   variables to be zero-based.  Returns the size of the array.  */
tree
g95_trans_allocate_temp_array (g95_loopinfo * loop, g95_ss_info * info,
                               tree eltype, tree string_length)
{
  tree type;
  tree desc;
  tree tmp;
  tree size;
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
          loop->to[n] = fold (build (MINUS_EXPR, g95_array_index_type,
                                     loop->to[n], loop->from[n]));
          loop->from[n] = integer_zero_node;
        }

      info->delta[dim] = integer_zero_node;
      info->start[dim] = integer_zero_node;
      info->stride[dim] = integer_one_node;
      info->dim[dim] = dim;
    }

  /* Initialise the descriptor.  */
  type = g95_get_array_type_bounds(eltype, info->dimen, loop->from, loop->to);
  desc = g95_create_var (type, "atmp");
  G95_DECL_PACKED_ARRAY (desc) = 1;
  if (string_length)
    {
      g95_allocate_lang_decl (desc);
      G95_DECL_STRING (desc) = 1;
      G95_DECL_STRING_LENGTH (desc) = string_length;
    }

  info->descriptor = desc;
  size = integer_one_node;

  /* Fill in the array dtype.  */
  tmp = g95_conv_descriptor_dtype (desc);
  g95_add_modify_expr (&loop->pre, tmp,
                       G95_TYPE_ARRAY_DTYPE (TREE_TYPE (desc)));

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
      /* Store the stride and bound components in the descriptor.  */
      tmp = g95_conv_descriptor_stride (desc, g95_rank_cst[n]);
      g95_add_modify_expr (&loop->pre, tmp, size);

      tmp = g95_conv_descriptor_lbound (desc, g95_rank_cst[n]);
      g95_add_modify_expr (&loop->pre, tmp, integer_zero_node);

      tmp = g95_conv_descriptor_ubound (desc, g95_rank_cst[n]);
      g95_add_modify_expr (&loop->pre, tmp, loop->to[n]);

      tmp = fold (build (PLUS_EXPR, g95_array_index_type,
                         loop->to[n], integer_one_node));

      size = fold (build (MULT_EXPR, g95_array_index_type, size, tmp));
      size = g95_evaluate_now (size, &loop->pre);
    }

  if (string_length)
    g95_todo_error ("Arrays of strings");

  /* Get the size of the array.  */
  nelem = size;
  size = fold (build (MULT_EXPR, g95_array_index_type, size,
                      TYPE_SIZE_UNIT (g95_get_element_type (type))));

  g95_trans_allocate_array_storage (loop, info, size, nelem);

  if (info->dimen > loop->temp_dim)
    loop->temp_dim = info->dimen;

  return size;
}

/* Make sure offset is a variable.  */
static void
g95_put_offset_into_var (stmtblock_t * pblock, tree * poffset,
                         tree * offsetvar)
{
  tree tmp;

  /* We should have already created the offset variable.  We cannot
     create it here because we may be in an inner scopde.  */
  assert (*offsetvar != NULL_TREE);
  tmp = build (MODIFY_EXPR, g95_array_index_type, *offsetvar, *poffset);
  g95_add_expr_to_block (pblock, tmp);
  *poffset = *offsetvar;
  TREE_USED (*offsetvar) = 1;
}

/* Add the contents of an array to the constructor.  */
static void
g95_trans_array_constructor_subarray (stmtblock_t * pblock, tree type,
    tree pointer, g95_expr * expr, tree * poffset, tree * offsetvar)
{
  g95_se se;
  g95_ss *ss;
  g95_loopinfo loop;
  stmtblock_t body;
  tree tmp;

  /* We need this to be a variable so we can increment it.  */
  g95_put_offset_into_var (pblock, poffset, offsetvar);

  g95_init_se (&se, NULL);

  /* Walk the array expression.  */
  ss = g95_walk_expr (g95_ss_terminator, expr);
  assert (ss != g95_ss_terminator);

  /* Initialize the scalarizer.  */
  g95_init_loopinfo (&loop);
  g95_add_ss_to_loop (&loop, ss);

  /* Initialize the loop.  */
  g95_conv_ss_startstride (&loop);
  g95_conv_loop_setup (&loop);

  /* Make the loop body.  */
  g95_mark_ss_chain_used (ss, 1);
  g95_start_scalarized_body (&loop, &body);
  g95_copy_loopinfo_to_se (&se, &loop);
  se.ss = ss;

  g95_conv_expr (&se, expr);
  g95_add_block_to_block (&body, &se.pre);

  /* Store the value.  */
  tmp = build1 (INDIRECT_REF, TREE_TYPE (TREE_TYPE (pointer)), pointer);
  tmp = build (ARRAY_REF, type, tmp, *poffset);
  g95_add_modify_expr (&body, tmp, se.expr);

  /* Increment the offset.  */
  tmp = build (PLUS_EXPR, g95_array_index_type, *poffset, integer_one_node);
  g95_add_modify_expr (&body, *poffset, tmp);

  /* Finish the loop.  */
  g95_add_block_to_block (&body, &se.post);
  assert (se.ss == g95_ss_terminator);
  g95_trans_scalarizing_loops (&loop, &body);
  g95_add_block_to_block (&loop.pre, &loop.post);
  tmp = g95_finish_block (&loop.pre);
  g95_add_expr_to_block (pblock, tmp);
}

/* Assign the values to the elements of an array constructor.  */
static void
g95_trans_array_constructor_value (stmtblock_t * pblock, tree type,
    tree pointer, g95_constructor * c, tree * poffset, tree * offsetvar)
{
  tree tmp;
  tree ref;
  stmtblock_t body;
  tree loopbody;
  g95_se se;

  for (; c; c = c->next)
    {
      /* If this is an iterator or an array, the offset must be a variable.  */
      if ((c->iterator || c->expr->rank > 0)
          && INTEGER_CST_P (*poffset))
        g95_put_offset_into_var (pblock, poffset, offsetvar);

      g95_start_block (&body);

      if (c->expr->expr_type == EXPR_ARRAY)
        {
          /* Array constructors can be nested.  */
          g95_trans_array_constructor_value (&body, type, pointer,
              c->expr->value.constructor, poffset, offsetvar);
        }
      else if (c->expr->rank > 0)
        {
          g95_trans_array_constructor_subarray (&body, type, pointer,
              c->expr, poffset, offsetvar);
        }
      else
        {
          g95_constructor *p;
          HOST_WIDE_INT n;

          p = c;
          n = 0;
          while (p && ! (p->iterator || p->expr->expr_type != EXPR_CONSTANT))
            {
              p = p->next;
              n++;
            }
          if (n < 4)
            {
              /* A single scalar value.  */
              g95_init_se (&se, NULL);
              g95_conv_expr (&se, c->expr);
              g95_add_block_to_block (&body, &se.pre);

              ref = build1 (INDIRECT_REF, TREE_TYPE (TREE_TYPE (pointer)),
                            pointer);
              ref = build (ARRAY_REF, type, ref, *poffset);
              g95_add_modify_expr (&body, ref, se.expr);
              g95_add_block_to_block (&body, &se.post);

              *poffset = fold (build (PLUS_EXPR, g95_array_index_type,
                                       *poffset, integer_one_node));
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
              tmp = build1 (ADDR_EXPR, build_pointer_type (TREE_TYPE (tmp)),
                            tmp);
              tmp = build1 (NOP_EXPR, build_pointer_type (tmptype), tmp);
              tmp = build1 (INDIRECT_REF, tmptype, tmp);
              tmp = build_v (MODIFY_EXPR, tmp, init);
              g95_add_expr_to_block (&body, tmp);

              bound = build_int_2 (n, 0);
              *poffset = fold (build (PLUS_EXPR, g95_array_index_type,
                                      *poffset, bound));
            }
          if (! INTEGER_CST_P (*poffset))
            {
              tmp = build_v (MODIFY_EXPR, *offsetvar, *poffset);
              g95_add_expr_to_block (&body, tmp);
            }
        }

      /* The frontend should already have done any expansions.  */
      if (c->iterator)
        {
          tree end;
          tree step;
          tree loopvar;
          tree exit_label;

          loopbody = g95_finish_block (&body);

          g95_init_se (&se, NULL);
          g95_conv_expr (&se, c->iterator->var);
          g95_add_block_to_block (pblock, &se.pre);
          loopvar = se.expr;

          /* Initialize thie loop.  */
          g95_init_se (&se, NULL);
          g95_conv_expr_val (&se, c->iterator->start);
          g95_add_block_to_block (pblock, &se.pre);
          tmp = build_v (MODIFY_EXPR, loopvar, se.expr);
          g95_add_expr_to_block (pblock, tmp);

          g95_init_se (&se, NULL);
          g95_conv_expr_val (&se, c->iterator->end);
          g95_add_block_to_block (pblock, &se.pre);
          end = g95_evaluate_now (se.expr, pblock);

          g95_init_se (&se, NULL);
          g95_conv_expr_val (&se, c->iterator->step);
          g95_add_block_to_block (pblock, &se.pre);
          step = g95_evaluate_now (se.expr, pblock);

          /* Generate the loop body.  */
          exit_label = g95_build_label_decl (NULL_TREE);
          g95_start_block (&body);

          /* Generate the exit condition.  */
          end = build (GT_EXPR, boolean_type_node, loopvar, end);
          tmp = build_v (GOTO_EXPR, exit_label);
          TREE_USED (exit_label) = 1;
          tmp = build_v (COND_EXPR, end, tmp, empty_stmt_node);
          g95_add_expr_to_block (&body, tmp);

          /* The main loop body.  */
          g95_add_expr_to_block (&body, loopbody);

          /* Increment the loop variable.  */
          tmp = build (PLUS_EXPR, TREE_TYPE (loopvar), loopvar, step);
          tmp = build_v (MODIFY_EXPR, loopvar, tmp);
          g95_add_expr_to_block (&body, tmp);

          /* Finish the loop.  */
          tmp = g95_finish_block (&body);
          tmp = build_v (LOOP_EXPR, tmp);
          g95_add_expr_to_block (pblock, tmp);

          /* Add the exit label.  */
          tmp = build_v (LABEL_EXPR, exit_label);
          g95_add_expr_to_block (pblock, tmp);
        }
      else
        {
          /* Ass the code as is.  */
          tmp = g95_finish_block (&body);
          g95_add_expr_to_block (pblock, tmp);
        }
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
          g95_get_array_cons_size (&len, c->expr->value.constructor);
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
            {
              mpz_set_si (*size, -1);
              mpz_clear (len);
              mpz_clear (val);
              return;
            }
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
   within the scalarization loop.  This is not optimal, but seems by far the
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
  offsetvar = g95_create_var_np (g95_array_index_type, "offset");
  TREE_USED (offsetvar) = 0;
  g95_trans_array_constructor_value (&loop->pre, type,
      ss->data.info.data, ss->expr->value.constructor, &offset,
      &offsetvar);

  if (TREE_USED (offsetvar))
    pushdecl (offsetvar);
  else
    assert (INTEGER_CST_P (offset));
#if 0
  /* Disable bound checking for now cos it's probably broken.  */
  if (flag_bounds_check)
    {
      abort ();
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
          g95_conv_expr (&se, ss->expr);
          g95_add_block_to_block (&loop->pre, &se.pre);

          if (ss->expr->ts.type != BT_CHARACTER)
            {
              /* Move the evaluation of scalar expressions outside the
                 scalarization loop.  */
              se.expr = g95_evaluate_now (se.expr, &loop->pre);
              g95_add_block_to_block (&loop->pre, &se.post);
            }
          else
            g95_add_block_to_block (&loop->post, &se.post);

          ss->data.scalar.expr = se.expr;
          ss->data.scalar.string_length = se.string_length;
          break;

        case G95_SS_REFERENCE:
          /* Scalar reference.  Evaluate this now.  */
          g95_init_se (&se, NULL);
          g95_conv_expr_reference (&se, ss->expr);
          g95_add_block_to_block (&loop->pre, &se.pre);
          g95_add_block_to_block (&loop->post, &se.post);

          ss->data.scalar.expr = g95_evaluate_now (se.expr, &se.pre);
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
          g95_conv_expr (&se, ss->expr);
          g95_add_block_to_block (&loop->pre, &se.pre);
          g95_add_block_to_block (&loop->post, &se.post);
          break;

        case G95_SS_CONSTRUCTOR:
          g95_trans_array_constructor(loop, ss);
          break;

        default:
          abort();
        }
    }
}

/* Translate expressions for the descriptor and data pointer of a SS.  */
/*GCC ARRAYS*/
static void
g95_conv_ss_descriptor (stmtblock_t * block, g95_ss * ss, int base)
{
  g95_se se;

  /* Get the descriptor for the array to be scalarized.  */
  assert (ss->expr->expr_type == EXPR_VARIABLE);
  g95_init_se (&se, NULL);
  se.descriptor_only = 1;
  g95_conv_expr_lhs (&se, ss->expr);
  g95_add_block_to_block (block, &se.pre);
  ss->data.info.descriptor = se.expr;

  if (base)
    {
      /* Also the data pointer.  */
      se.expr = g95_conv_array_base (se.expr);
      ss->data.info.data = g95_evaluate_now (se.expr, block);
    }
}

/* Initialise a g95_loopinfo structure.  */
void
g95_init_loopinfo (g95_loopinfo * loop)
{
  int n;

  memset (loop, 0, sizeof(g95_loopinfo));
  g95_init_block (&loop->pre);
  g95_init_block (&loop->post);

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
      TREE_ADDRESSABLE (descriptor) = 1;
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
  int n;

  array = build1 (INDIRECT_REF, TREE_TYPE (TREE_TYPE (pointer)), pointer);

  index = integer_zero_node;
  for (n = 0; n < dimen; n++)
    {
      /* index = index + stride[n]*indices[n] */
      tmp = g95_conv_array_stride (se->expr, n);
      tmp = fold (build (MULT_EXPR, g95_array_index_type, indices[n], tmp));

      index = fold (build (PLUS_EXPR, g95_array_index_type, index, tmp));
    }

  /* Result = data[index].  */
  assert (TREE_CODE (TREE_TYPE (array)) == ARRAY_TYPE);
  tmp = build (ARRAY_REF, TREE_TYPE( TREE_TYPE (array)), array, index);

  /* Check we've used the correct number of dimensions.  */
  assert (TREE_CODE (TREE_TYPE (tmp)) != ARRAY_TYPE);

  se->expr = tmp;
}

/* Generate code to perform an array index bound check.  */
static tree
g95_trans_array_bound_check (g95_se * se, tree descriptor, tree index, int n)
{
  tree cond;
  tree fault;
  tree tmp;

  if (! flag_bounds_check)
    return index;

  index = g95_evaluate_now (index, &se->pre);
  /* Check lower bound.  */
  tmp = g95_conv_array_lbound (descriptor, n);
  fault = fold (build (LT_EXPR, boolean_type_node, index, tmp));
  /* Check upper bound.  */
  tmp = g95_conv_array_ubound (descriptor, n);
  cond = fold (build (GT_EXPR, boolean_type_node, index, tmp));
  fault = fold (build (TRUTH_OR_EXPR, boolean_type_node, fault, cond));

  g95_trans_runtime_check (fault, g95_strconst_fault, &se->pre);

  return index;
}

/* A reference to an array vector subscript.  Uses recursion to handle nested
   vector subscripts.  */
static tree
g95_conv_vector_array_index (g95_se * se, tree index, g95_ss * ss)
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
        index =
          g95_conv_vector_array_index (se, index, info->subscript[n]);

        indices[n] =
          g95_trans_array_bound_check (se, info->descriptor, index, n);
        break;

      default:
        abort();
      }
   }
  /* Get the index from the vector.  */
  g95_conv_array_index_ref (se, info->data, indices, ar->dimen);
  index = se->expr;
  /* Put the descriptor back.  */
  se->expr = descsave;

  return index;
}

/* Return the offset for an index.  Performs bound checking for elemental
   dimensions.  Single element references are processed seperately.  */
static tree
g95_conv_array_index_offset (g95_se * se, g95_ss_info * info, int dim, int i,
                             g95_array_ref * ar, tree stride)
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

          index =
            g95_trans_array_bound_check (se, info->descriptor, index, dim);
        }
      else
        {
          /* Scalarized dimension.  */
          assert (info && se->loop);

          index = se->loop->loopvar[i];
          index = fold (build (MULT_EXPR, g95_array_index_type, index,
                               info->stride[i]));
          index = fold (build (PLUS_EXPR, g95_array_index_type, index,
                               info->delta[i]));

          if (ar->dimen_type[dim] == DIMEN_VECTOR)
            {
              index = g95_conv_vector_array_index (se, index,
                                                   info->subscript[dim]);
              index =
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
  index = fold (build (MULT_EXPR, g95_array_index_type, index, stride));

  return index;
}

/* Build a scalarized reference to an array.  */
static void
g95_conv_scalarized_array_ref (g95_se * se, g95_array_ref * ar)
{
  g95_ss_info * info;
  tree index;
  tree tmp;
  int n;

  info = &se->ss->data.info;
  if (ar)
    n = se->loop->order[0];
  else
    n = 0;

  index = g95_conv_array_index_offset (se, info, info->dim[n], n, ar,
                                       info->stride0);
  index = fold (build (PLUS_EXPR, g95_array_index_type, index, info->offset));

  tmp = build1 (INDIRECT_REF, TREE_TYPE (TREE_TYPE (info->data)), info->data);
  se->expr = build (ARRAY_REF, TREE_TYPE (TREE_TYPE (tmp)), tmp, index);
}

/* Translate access of temporary array.  */
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
  tree stride;
  tree fault;
  g95_se indexse;

  /* Handle scalarized references seperately.  */
  if (ar->type != AR_ELEMENT)
    {
      g95_conv_scalarized_array_ref (se, ar);
      return;
    }

  index = integer_zero_node;

  fault = integer_zero_node;

  for (n = 0; n < ar->dimen; n++)
    {
      g95_init_se (&indexse, NULL);
      g95_conv_expr_type (&indexse, ar->start[n], g95_array_index_type);
      g95_add_block_to_block (&se->pre, &indexse.pre);

      if (flag_bounds_check)
        {
          /* Check array bounds.  */
          tree cond;

          indexse.expr = g95_evaluate_now (indexse.expr, &se->pre);

          tmp = g95_conv_array_lbound (se->expr, n);
          cond = fold (build (LT_EXPR, boolean_type_node, indexse.expr, tmp));
          fault = fold (build (TRUTH_OR_EXPR, boolean_type_node, fault, cond));

          tmp = g95_conv_array_ubound (se->expr, n);
          cond = fold (build (GT_EXPR, boolean_type_node, indexse.expr, tmp));
          fault = fold (build (TRUTH_OR_EXPR, boolean_type_node, fault, cond));
        }

      /* Multiply the index by the stride.  */
      stride = g95_conv_array_stride (se->expr, n);
      tmp = fold (build (MULT_EXPR, g95_array_index_type, indexse.expr,
                         stride));

      index = fold (build (PLUS_EXPR, g95_array_index_type, index, tmp));
    }

  if (flag_bounds_check)
    g95_trans_runtime_check (fault, g95_strconst_fault, &se->pre);

  tmp = g95_conv_array_base (se->expr);
  tmp = build1 (INDIRECT_REF, TREE_TYPE (TREE_TYPE (tmp)), tmp);
  se->expr = build (ARRAY_REF, TREE_TYPE (TREE_TYPE (tmp)), tmp, index);
}

/* Generate the code to be executed immediately before entering a
   scalarization loop.  */
static void
g95_trans_preloop_setup (g95_loopinfo * loop, int dim, int flag,
                         stmtblock_t * pblock)
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
                  index = g95_conv_array_index_offset (&se, info, i, -1,
                      &info->ref->u.ar, stride);
                  g95_add_block_to_block (pblock, &se.pre);

                  info->offset = fold (build (PLUS_EXPR, g95_array_index_type,
                                              info->offset, index));
                  info->offset = g95_evaluate_now (info->offset, pblock);
                }

              i = loop->order[0];
              stride = g95_conv_array_stride (info->descriptor, info->dim[i]);
            }
          else
            stride = g95_conv_array_stride (info->descriptor, 0);

          /* Calculate the stride of the innermost loop.  */
          info->stride0 = g95_evaluate_now (stride, pblock);
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
          index = g95_conv_array_index_offset (&se, info, info->dim[i], i,
              ar, stride);
          g95_add_block_to_block (pblock, &se.pre);
          info->offset = fold (build (PLUS_EXPR, g95_array_index_type,
                                      info->offset, index));
          info->offset = g95_evaluate_now (info->offset, pblock);
        }
    }
}

/* Start a scalarized expression.  Creates a scope and declares loop
   variables.  */
void
g95_start_scalarized_body (g95_loopinfo * loop, stmtblock_t * pbody)
{
  int dim;
  int n;
  int flags;

  assert (! loop->array_parameter);

  for (dim = loop->dimen - 1; dim >= 0; dim--)
    {
      n = loop->order[dim];

      g95_start_block (&loop->code[n]);

      /* Create the loop variable.  */
      loop->loopvar[n] = g95_create_var (g95_array_index_type, "S");

      if (dim < loop->temp_dim)
        flags = 3;
      else
        flags = 1;
      g95_trans_preloop_setup (loop, dim, flags, &loop->code[n]);
    }
  g95_start_block (pbody);
}

/* Finish off a scalarized loop.  Generates the loop code.  */
static void
g95_trans_scalarized_loop_end (g95_loopinfo * loop, int n, stmtblock_t * pbody)
{
  stmtblock_t block;
  tree cond;
  tree tmp;
  tree loopbody;
  tree exit_label;

  loopbody = g95_finish_block (pbody);

  /* Initialize the loopvar.  */
  tmp = build_v (MODIFY_EXPR, loop->loopvar[n], loop->from[n]);
  g95_add_expr_to_block (&loop->code[n], tmp);

  exit_label = g95_build_label_decl (NULL_TREE);

  /* Generate the loop body.  */
  g95_init_block (&block);

  /* The exit condition.  */
  cond = build (GT_EXPR, boolean_type_node, loop->loopvar[n], loop->to[n]);
  tmp = build_v (GOTO_EXPR, exit_label);
  TREE_USED (exit_label) = 1;
  tmp = build_v (COND_EXPR, cond, tmp, empty_stmt_node);
  g95_add_expr_to_block (&block, tmp);

  /* The main body.  */
  g95_add_expr_to_block (&block, loopbody);

  /* Increment the loopvar.  */
  tmp = build (PLUS_EXPR, g95_array_index_type,
               loop->loopvar[n], integer_one_node);
  tmp = build_v (MODIFY_EXPR, loop->loopvar[n], tmp);
  g95_add_expr_to_block (&block, tmp);

  /* Build the loop.  */
  tmp = g95_finish_block (&block);
  tmp = build_v (LOOP_EXPR, tmp);
  g95_add_expr_to_block (&loop->code[n], tmp);

  /* Add the exit label.  */
  tmp = build_v (LABEL_EXPR, exit_label);
  g95_add_expr_to_block (&loop->code[n], tmp);
}

/* Generates the actual loops for a scalarized expression.  */
void
g95_trans_scalarizing_loops (g95_loopinfo * loop, stmtblock_t * body)
{
  int dim;
  int n;
  g95_ss *ss;
  stmtblock_t * pblock;
  tree tmp;

  pblock = body;
  /* Generate the loops.  */
  for (dim = 0; dim < loop->dimen; dim++)
    {
      n = loop->order[dim];
      g95_trans_scalarized_loop_end (loop, n, pblock);
      loop->loopvar[n] = NULL_TREE;
      pblock = &loop->code[n];
    }

  tmp = g95_finish_block (pblock);
  g95_add_expr_to_block (&loop->pre, tmp);

  /* Clear all the used flags.  */
  for (ss = loop->ss; ss; ss = ss->loop_chain)
    ss->useflags = 0;
}

/* Finish the main body of a scalarized expression, and start the secondary
   copying body.  */
void
g95_trans_scalarized_loop_boundary (g95_loopinfo * loop, stmtblock_t * body)
{
  int dim;
  int n;
  stmtblock_t * pblock;

  pblock = body;
  /* We finish and restart as many loops as are used by the temporary.  */
  for (dim = 0; dim < loop->temp_dim - 1; dim++)
    {
      n = loop->order[dim];
      g95_trans_scalarized_loop_end (loop, n, pblock);
      loop->loopvar[n] = NULL_TREE;
      pblock = &loop->code[n];
    }

  n = loop->order[loop->temp_dim - 1];
  g95_trans_scalarized_loop_end (loop, n, pblock);

  for (dim = loop->temp_dim - 2; dim >= 0; dim--)
    {
      n = loop->order[dim];

      g95_start_block (&loop->code[n]);

      loop->loopvar[n] = g95_create_var (g95_array_index_type, "Q");

      g95_trans_preloop_setup (loop, dim, 2, &loop->code[n]);
    }

  g95_start_block (body);
}

/* Calculate the upper bound of an array section.  */
static tree
g95_conv_section_upper_bound (g95_ss * ss, int n, stmtblock_t * pblock)
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
      g95_conv_expr_type (&se, end, g95_array_index_type);
      g95_add_block_to_block (pblock, &se.pre);
      bound = se.expr;
    }
  else
    {
      /* No upper bound was specified, so use the bound of the array. */
      bound = G95_TYPE_ARRAY_UBOUND (TREE_TYPE (desc), dim);

      if (bound == NULL_TREE)
        {
          bound = g95_conv_descriptor_ubound (desc, g95_rank_cst[dim]);
          bound = g95_evaluate_now (bound, pblock);
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
      if (! vecss->data.info.descriptor)
        g95_conv_ss_descriptor (&loop->pre, vecss, ! loop->array_parameter);
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
      g95_conv_expr_type (&se, start, g95_array_index_type);
      g95_add_block_to_block (&loop->pre, &se.pre);
      info->start[n] = se.expr;
    }
  else
    {
      /* No lower bound specified so use the bound of the array.  */
      info->start[n] = G95_TYPE_ARRAY_LBOUND (TREE_TYPE (desc), dim);

      if (info->start[n] == NULL_TREE)
        {
          info->start[n] =
            g95_conv_descriptor_lbound (desc, g95_rank_cst[dim]);
        }
    }
  info->start[n] = g95_evaluate_now (info->start[n], &loop->pre);

  /* Calculate the stride.  */
  if (stride == NULL)
      info->stride[n] = integer_one_node;
  else
    {
      g95_init_se (&se, NULL);
      g95_conv_expr_type (&se, stride, g95_array_index_type);
      g95_add_block_to_block (&loop->pre, &se.pre);
      info->stride[n] = g95_evaluate_now (se.expr, &loop->pre);
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
          g95_conv_ss_descriptor (&loop->pre, ss, ! loop->array_parameter);

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
      stmtblock_t block;
      tree fault;
      tree bound;
      tree end;
      tree size[G95_MAX_DIMENSIONS];
      g95_ss_info *info;
      int dim;

      g95_start_block (&block);

      fault = integer_zero_node;
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
              tmp = info->start[n];
              tmp = fold (build (LT_EXPR, boolean_type_node, tmp, bound));
              fault = fold (build (TRUTH_OR_EXPR, boolean_type_node, fault,
                                   tmp));

              /* Check the upper bound.  */
              bound = g95_conv_array_ubound (desc, dim);
              end = g95_conv_section_upper_bound (ss, n, &block);
              tmp = fold (build (GT_EXPR, boolean_type_node, end, bound));
              fault = fold (build (TRUTH_OR_EXPR, boolean_type_node, fault,
                                   tmp));

              /* Check the section sizes match.  */
              tmp = fold (build (MINUS_EXPR, g95_array_index_type, end,
                                 info->start[n]));
              tmp = fold (build (FLOOR_DIV_EXPR, g95_array_index_type, tmp,
                                 info->stride[n]));
              /* We remember the size of the first section, and check all the
                 others against this.  */
              if (size[n])
                {
                  tmp = fold (build (NE_EXPR, boolean_type_node, tmp, size[n]));
                  fault = build (TRUTH_OR_EXPR, boolean_type_node, fault, tmp);
                }
              else
                size[n] = g95_evaluate_now (tmp, &block);
            }
        }
      g95_trans_runtime_check (fault, g95_strconst_bounds, &block);

      tmp = g95_finish_block (&block);
      g95_add_expr_to_block (&loop->pre, tmp);
    }
}

/* Return true if the two SS could be aliased, ie. both point to the same data
   object.  */
/* TODO: resolve aliases based on frontend expressions.  */
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
      if (lref->type != REF_COMPONENT)
        continue;

      if (g95_symbols_could_alias (lref->u.c.sym, rss->expr->symbol))
        return 1;

      for (rref = rss->expr->ref; rref != rss->data.info.ref; rref = rref->next)
        {
          if (rref->type != REF_COMPONENT)
            continue;

          if (g95_symbols_could_alias (lref->u.c.sym, rref->u.c.sym))
            return 1;
        }
    }

  for (rref = rss->expr->ref; rref != rss->data.info.ref; rref = rref->next)
    {
      if (rref->type != REF_COMPONENT)
        break;

      if (g95_symbols_could_alias (rref->u.c.sym, lss->expr->symbol))
        return 1;
    }

  return 0;
}

/* Resolve array data dependencies.  Creates a temporary if required.  */
/* Calc dependencies with g95_expr rather than g95_ss, and move to
   dependency.c.  */
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
                case REF_COMPONENT:
                  if (lref->u.c.component != rref->u.c.component)
                    same = 0;
                  break;

                case REF_ARRAY:
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
              if (ss->expr->shape)
                {
                  /* The frontend has worked out the size for us.  */
                  loopspec[n] = ss;
                }
              else
                {
                  /* Try to figure out the size of the constructior.  */
                  g95_get_array_cons_size (&i,
                      ss->expr->value.constructor);
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

          /* We don't know how to handle functions yet.
             This may not be possible in all cases.  */
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
          cshape = loopspec[n]->expr->shape;
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
              &loop->pre);
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
          info->delta[n] = g95_evaluate_now (loop->from[n], &loop->pre);
          /* Number of iterations is (end - start + step) / step.
             with start = 0, this simplifies to
              last = end / step;
              for (i = 0; i<=last; i++){...};  */
          tmp = fold (build (MINUS_EXPR, g95_array_index_type, loop->to[n],
                             loop->from[n]));
          tmp = fold (build (TRUNC_DIV_EXPR, g95_array_index_type, tmp,
                             info->stride[n]));
          loop->to[n] = g95_evaluate_now (tmp, &loop->pre);
          /* Make the loop variable start at 0.  */
          loop->from[n] = integer_zero_node;
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
              tmp = fold (build (MULT_EXPR, g95_array_index_type,
                                 loop->from[n], info->stride[n]));

              /* Then subtract this from our starting value.  */
              tmp = fold (build (MINUS_EXPR, g95_array_index_type,
                                 info->start[n], tmp));

              info->delta[n] = g95_evaluate_now (tmp, &loop->pre);
            }
        }
    }
}

/* Fills in an array descriptor, and returns the size of the array.  The size
   will be a simple_val, ie a variable or a constant.  Also calculates the
   offset of the base.  Returns the number of elements in the arrary.
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
    return (stride);
   }  */
/*GCC ARRAYS*/
static tree
g95_array_init_size (tree descriptor, int rank, tree * poffset,
                     g95_expr ** lower, g95_expr ** upper,
                     stmtblock_t * pblock, tree * pstring)
{
  tree type;
  tree tmp;
  tree size;
  tree offset;
  tree stride;
  tree string_len;
  g95_se se;
  int n;

  type = TREE_TYPE (descriptor);

  stride = integer_one_node;
  offset = integer_zero_node;

  /* Set the dtype.  */
  tmp = g95_conv_descriptor_dtype (descriptor);
  tmp = build_v (MODIFY_EXPR, tmp,
                 G95_TYPE_ARRAY_DTYPE (TREE_TYPE (descriptor)));
  g95_add_expr_to_block (pblock, tmp);

  for (n = 0; n < rank; n++)
    {
      /* Set lower bound.  */
      g95_init_se (&se, NULL);
      if (lower == NULL)
        se.expr = integer_one_node;
      else
        {
          assert (lower[n]);
          g95_conv_expr_type (&se, lower[n], g95_array_index_type);
          g95_add_block_to_block (pblock, &se.pre);
        }
      tmp = g95_conv_descriptor_lbound (descriptor, g95_rank_cst[n]);
      tmp = build_v (MODIFY_EXPR, tmp, se.expr);
      g95_add_expr_to_block (pblock, tmp);

      /* Work out the offset for this component.  */
      tmp = fold (build (MULT_EXPR, g95_array_index_type, se.expr, stride));
      offset = fold (build (MINUS_EXPR, g95_array_index_type, offset, tmp));

      /* Start the calculation for the size of this dimension.  */
      size = build (MINUS_EXPR, g95_array_index_type,
                    integer_one_node, se.expr);

      /* Set upper bound.  */
      g95_init_se (&se, NULL);
      assert (upper[n]);
      g95_conv_expr_type (&se, upper[n], g95_array_index_type);
      g95_add_block_to_block (pblock, &se.pre);

      tmp = g95_conv_descriptor_ubound (descriptor, g95_rank_cst[n]);
      g95_add_modify_expr (pblock, tmp, se.expr);

      /* Store the stride.  */
      tmp = g95_conv_descriptor_stride (descriptor, g95_rank_cst[n]);
      g95_add_modify_expr (pblock, tmp, stride);

      /* Calculate the size of this dimension.  */
      size = fold (build (PLUS_EXPR, g95_array_index_type, se.expr, size));

      /* Multiply the stride by the number of elements in this dimension.  */
      stride = fold (build (MULT_EXPR, g95_array_index_type, stride, size));
      size = g95_evaluate_now (stride, pblock);
    }

  if (pstring && *pstring)
    {
      string_len = *pstring;
      string_len = fold (build (MULT_EXPR, g95_array_index_type, stride,
                               string_len));
    }
  else
    string_len = NULL_TREE;

  /* The stride is the number of elements in the array, so multiply by the
     size of an element to get the total size.  */
  tmp = TYPE_SIZE_UNIT (g95_get_element_type (type));
  stride = fold (build (MULT_EXPR, g95_array_index_type, stride, tmp));

  if (string_len)
    stride = fold (build (PLUS_EXPR, g95_array_index_type, stride,
                          string_len));

  if (poffset != NULL )
    {
      offset = g95_evaluate_now (offset, pblock);
      *poffset = offset;
    }

  return stride;
}

/* Initialises the descriptor and generates a call to _gfor_allocate.  Does
   the work for an ALLOCATE statement.  */
/*GCC ARRAYS*/
void
g95_array_allocate (g95_se * se, g95_ref * ref, tree pstat)
{
  tree tmp;
  tree base;
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
      lower, upper, &se->pre, &len);

  /* The size is the number of elements in the array, so multiply by the
     size of an element to get the total size.  */
  tmp = TYPE_SIZE_UNIT (g95_get_element_type (TREE_TYPE (se->expr)));
  size = fold (build (MULT_EXPR, g95_array_index_type, size, tmp));

  /* Allocate memory to store the data.  */
  tmp = g95_conv_descriptor_data (se->expr);
  pointer = build1 (ADDR_EXPR, build_pointer_type (TREE_TYPE (tmp)), tmp);
  pointer = g95_evaluate_now (pointer, &se->pre);

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
  g95_add_expr_to_block (&se->pre, tmp);

  pointer = g95_conv_descriptor_data (se->expr);
  /* Set base = &data[offset].  */
  if (! integer_zerop (offset))
    {
      tmp = build1 (INDIRECT_REF, TREE_TYPE (TREE_TYPE (pointer)), pointer);
      tmp = build (ARRAY_REF, TREE_TYPE (TREE_TYPE (tmp)), tmp, offset);
      pointer = build1 (ADDR_EXPR, TREE_TYPE (pointer), tmp);
    }
  base = g95_conv_descriptor_base (se->expr);
  tmp = build (MODIFY_EXPR, TREE_TYPE (base), base, pointer);
  g95_add_expr_to_block (&se->pre, tmp);

  /* Initialize the pointers for a character array.  */
  if (len)
    {
      g95_todo_error ("arrays of strings");
    }
}

/* Deallocate an array variable.  Also used when an allocated variable goes
   out of scope.  */
/*GCC ARRAYS*/
tree
g95_array_deallocate (tree descriptor)
{
  tree var;
  tree tmp;
  stmtblock_t block;

  g95_start_block (&block);
  /* Get a pointer to the data.  */
  tmp = g95_conv_descriptor_data (descriptor);
  tmp = build1 (ADDR_EXPR, build_pointer_type (TREE_TYPE (tmp)), tmp);
  var = g95_create_var (TREE_TYPE (tmp), "ptr");
  tmp = build_v (MODIFY_EXPR, var, tmp);
  g95_add_expr_to_block (&block, tmp);

  /* Parameter is the address of the data component.  */
  tmp = g95_chainon_list (NULL_TREE, var);
  tmp = g95_chainon_list (tmp, integer_zero_node);
  tmp = g95_build_function_call (gfor_fndecl_deallocate, tmp);
  g95_add_expr_to_block (&block, tmp);

  return g95_finish_block (&block);
}

/* Create an array constructor from an initialization expression.  */
static tree
g95_conv_array_initializer (tree type, g95_expr * expr)
{
  g95_constructor *c;
  tree list;
  tree cons;
  g95_se se;

  list = NULL_TREE;
  /* We assume the frontend already did any expansions and conversions.  */
  for (c = expr->value.constructor; c; c = c->next)
    {
      if (c->iterator)
        {
          internal_error (
            "Possible frontend bug: array constructor not expanded");
        }
      assert (c->expr->expr_type == EXPR_CONSTANT);

      g95_init_se (&se, NULL);
      g95_conv_constant (&se, c->expr);
      list = tree_cons (NULL_TREE, se.expr, list);
    }
  list = nreverse (list);
  cons = build (CONSTRUCTOR, type, NULL_TREE, list);
  TREE_CONSTANT (cons) = 1;
  return cons;
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
  tree tmpvar;
  tree ref;
  tree len;
  tree nelemen;
  stmtblock_t block;
  g95_array_spec *as;

  assert (! (sym->attr.pointer || sym->attr.allocatable));

  if (G95_ARRAY_TYPE_P (TREE_TYPE (descriptor)))
    {
      /* TODO: Put large arrays on the heap.  */
      if (sym->ts.type == BT_CHARACTER)
        g95_todo_error ("arrays of strings");

      if (sym->value && ! sym->attr.use_assoc)
        {
          DECL_INITIAL (descriptor) =
            g95_conv_array_initializer (TREE_TYPE (descriptor), sym->value);
        }

      return NULL_TREE;
    }

  assert (! sym->attr.use_assoc);
  assert (! TREE_STATIC (descriptor));
  assert (! sym->module[0]);

  g95_start_block (&block);

  as = sym->as;

  if (sym->ts.type == BT_CHARACTER)
    {
      /* TODO: Derived types.  */
      assert (TREE_CODE (descriptor) == VAR_DECL);
      len = G95_DECL_STRING_LENGTH (descriptor);
      if (! INTEGER_CST_P (len))
        len = g95_conv_init_string_length (sym, &block);
    }
  else
    len = NULL_TREE;

  nelemen = g95_array_init_size (descriptor, as->rank, &offset,
                              as->lower, as->upper, &block, &len);

  /* The size is the number of elements in the array, so multiply by the
     size of an element to get the total size.  */
  tmp = TYPE_SIZE_UNIT (g95_get_element_type (TREE_TYPE (descriptor)));
  size = fold (build (MULT_EXPR, g95_array_index_type, nelemen, tmp));

  /* Allocate the array.  */
  if (g95_can_put_var_on_stack (size))
    {
      /* Create a temporary variable to hold the data.  */
      tmp = fold (build (MINUS_EXPR, g95_array_index_type, size,
                         integer_one_node));
      tmp = build_range_type (g95_array_index_type, integer_zero_node, tmp);
      tmp = build_array_type (g95_get_element_type (TREE_TYPE (descriptor)),
                              tmp);
      tmpvar = g95_create_var_np (tmp, "A");
      TREE_ADDRESSABLE (tmpvar) = 1;
      TREE_STATIC (tmpvar) = TREE_STATIC (descriptor);

      /* Store the address.  */
      tmp = g95_conv_descriptor_data (descriptor);
      pointer = build1 (ADDR_EXPR, TREE_TYPE (tmp), tmpvar);
      tmp = build_v (MODIFY_EXPR, tmp, pointer);
      g95_add_expr_to_block (&block, tmp);
    }
  else
    {
      /* Allocate memory to hold the data.  */
      /* Get the address of the data component.  */
      tmp = g95_conv_descriptor_data (descriptor);
      tmp = build1 (ADDR_EXPR, build_pointer_type (TREE_TYPE (tmp)), tmp);
      pointer = g95_create_var (TREE_TYPE (tmp), "ptr");
      tmp = build_v (MODIFY_EXPR, pointer, tmp);
      g95_add_expr_to_block (&block, tmp);

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
      g95_add_expr_to_block (&block, tmp);

      pointer = g95_conv_descriptor_data (descriptor);
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
  tmp = build_v (MODIFY_EXPR, ref, tmp);
  g95_add_expr_to_block (&block, tmp);

  /* Initialize the pointers for an array of character strings.  */
  if (len)
    {
      g95_todo_error ("arrays of strings");
    }

  tmp = g95_finish_block (&block);

  if (tmpvar != NULL_TREE)
    pushdecl (tmpvar);

  if (sym->value)
    internal_error ("non-static array with initializer");

  return tmp;
}

/* Generate code to repack an array.  */
/*GCC ARRAYS*/
static void
g95_trans_repack_array (stmtblock_t * pblock, tree dest, tree src, int dimen)
{
  int n;
  g95_loopinfo loop;
  g95_se lse;
  g95_se rse;
  tree tmp;
  g95_ss *lss;
  g95_ss *rss;
  stmtblock_t body;

  /* We can either repack arrays inline or use a library function to do it.
     inline is probably faster, but generates larger code.  */
  if (! g95_option.inline_repack_arrays)
    {
      tree args;

      /* TODO: Write the library repack functions.  */
      args = NULL_TREE;
      if (TREE_CODE (dest) == INDIRECT_REF)
        tmp = TREE_OPERAND (dest, 0);
      else
        tmp = build1 (ADDR_EXPR, build_pointer_type (TREE_TYPE (dest)), dest);
      args = g95_chainon_list (args, tmp);
      if (TREE_CODE (src) == INDIRECT_REF)
        tmp = TREE_OPERAND (src, 0);
      else
        tmp = build1 (ADDR_EXPR, build_pointer_type (TREE_TYPE (src)), src);

      args = g95_chainon_list (args, tmp);
      tmp = TYPE_SIZE_UNIT (g95_get_element_type (TREE_TYPE (dest)));
      args = g95_chainon_list (args, tmp);

      tmp = g95_build_function_call (gfor_fndecl_repack[dimen - 1], args);
      g95_add_expr_to_block (pblock, tmp);

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
      loop.loopvar[n] = g95_create_var (g95_array_index_type, "S");
      loop.from[n] =
        g95_evaluate_now (g95_conv_array_lbound (tmp, n), &loop.pre);
      loop.to[n] =
        g95_evaluate_now (g95_conv_array_ubound (tmp, n), &loop.pre);

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
  lss->data.info.data = g95_evaluate_now (tmp, &loop.pre);
  lss->data.info.descriptor = dest;
  lss->type = G95_SS_SECTION;
  lss->next = g95_ss_terminator;
  lss->data.info.dimen = dimen;

  tmp = g95_conv_array_base (src);
  rss->data.info.data = g95_evaluate_now (tmp, &loop.pre);
  rss->data.info.descriptor = src;
  rss->type = G95_SS_SECTION;
  rss->next = g95_ss_terminator;
  rss->data.info.dimen = dimen;

  g95_add_ss_to_loop (&loop, lss);
  g95_add_ss_to_loop (&loop, rss);

  /* The loop body.  */
  g95_mark_ss_chain_used (lss, 1);
  g95_mark_ss_chain_used (rss, 1);
  g95_start_scalarized_body (&loop, &body);

  g95_init_se (&lse, NULL);
  g95_init_se (&rse, NULL);
  lse.ss = lss;
  lse.loop = &loop;
  rse.ss = rss;
  rse.loop = &loop;

  g95_conv_scalarized_array_ref (&lse, NULL);
  g95_add_block_to_block (&body, &lse.pre);
  g95_conv_scalarized_array_ref (&rse, NULL);
  g95_add_block_to_block (&body, &rse.pre);

  tmp = build (MODIFY_EXPR, TREE_TYPE (lse.expr), lse.expr, rse.expr);
  g95_add_expr_to_block (&body, tmp);

  g95_add_block_to_block (&body, &rse.post);
  g95_add_block_to_block (&body, &lse.post);

  g95_trans_scalarizing_loops (&loop, &body);

  g95_add_block_to_block (pblock, &loop.pre);
  g95_add_block_to_block (pblock, &loop.post);
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
  g95_se se;
  int n;
  int repack;
  tree tmp;
  tree tmpvar;
  tree stride;
  tree strides[G95_MAX_DIMENSIONS];
  tree offset;
  tree ref;
  tree lbound[G95_MAX_DIMENSIONS];
  tree ubound;
  tree args;
  tree needpack;
  tree eqvar;
  tree repack_stmt;
  tree nopack_stmt;
  tree unpack;
  tree oldstride;
  tree base;
  int checkparm;
  locus loc;
  stmtblock_t block;
  stmtblock_t subblock;

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
  stride = integer_one_node;
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
      unpack = g95_create_var (boolean_type_node, "unpack");
      /* We need to save the passed descriptor base.  */
      base = g95_create_var (g95_array_dataptr_type (dumdesc), "base");
    }

  g95_start_block (&block);

  if (repack)
    {
      /* Save the old base.  */
      tmp = g95_conv_descriptor_base (dumdesc);
      tmp = build (MODIFY_EXPR, TREE_TYPE (base), base, tmp);
      g95_add_expr_to_block (&block, tmp);
      needpack = g95_create_var (boolean_type_node, "repack");
    }
  else
    needpack = NULL_TREE;

  /* Set the dtype.  */
  tmp = g95_conv_descriptor_dtype (tmpdesc);
  tmp = build_v (MODIFY_EXPR, tmp,
               G95_TYPE_ARRAY_DTYPE(TREE_TYPE (tmpdesc)));
  g95_add_expr_to_block (&block, tmp);

  oldstride = g95_create_var (g95_array_index_type, "stride");

  for (n = 0; n < sym->as->rank; n++)
    {
      /* Set the desired lower bound.  */
      assert (sym->as->lower[n]);
      g95_init_se (&se, NULL);
      g95_conv_expr_type (&se, sym->as->lower[n], g95_array_index_type);
      g95_add_block_to_block (&block, &se.pre);
      lbound[n] = g95_evaluate_now (se.expr, &block);

      tmp = g95_conv_descriptor_lbound (tmpdesc, g95_rank_cst[n]);
      tmp = build_v (MODIFY_EXPR, tmp, lbound[n]);
      g95_add_expr_to_block (&block, tmp);

      if (checkparm || ! sym->as->upper[n])
        {
          /* Get the upper bound of the actual parameter.  */
          ubound = g95_conv_descriptor_ubound (dumdesc, g95_rank_cst[n]);
        }
      else
        ubound = NULL_TREE;

      if (checkparm || ! sym->as->upper[n])
        {
          /* Get the lower bound of the actual parameter.  */
          ref = g95_conv_descriptor_lbound (dumdesc, g95_rank_cst[n]);
        }
      else
        ref = NULL_TREE;

      /* Set the desired upper bound.  */
      if (sym->as->upper[n])
        {
          /* We know what we want the upper bound to be.  */
          g95_init_se (&se, NULL);
          g95_conv_expr_type (&se, sym->as->upper[n], g95_array_index_type);
          g95_add_block_to_block (&block, &se.pre);

          /* Check the sizes match.  */
          if (checkparm)
            {
              se.expr = g95_evaluate_now (se.expr, &block);

              /* Check (ubound(a) - lbound(a) == ubound(b) - lbound(b)).
                       (ubound(a) - lbound(a) + lbound(b) == ubound(b)).  */

              tmp = fold (build (MINUS_EXPR, g95_array_index_type, se.expr,
                                 lbound[n]));
              tmp = fold (build (PLUS_EXPR, g95_array_index_type, tmp, ref));
              tmp = fold (build (NE_EXPR, boolean_type_node, tmp, ubound));
              g95_trans_runtime_check (tmp, g95_strconst_bounds, &block);
            }
          ubound = se.expr;
        }
      else
        {
          /* For assumed shape arrays move the upper bound by the same amount
             as the lower bound.  */
          tmp = build (MINUS_EXPR, g95_array_index_type, se.expr, ref);
          ubound = fold (build (PLUS_EXPR, g95_array_index_type, ubound, tmp));
        }

      /* ubound is now the upper bound of the temporary.  */
      /* Store the new upper bound.  */
      tmp = g95_conv_descriptor_ubound (tmpdesc, g95_rank_cst[n]);
      tmp = build_v (MODIFY_EXPR, tmp, ubound);
      g95_add_expr_to_block (&block, tmp);

      /* Get the passed stride.  */
      tmp = g95_conv_descriptor_stride (dumdesc, g95_rank_cst[n]);
      tmp = build_v (MODIFY_EXPR, oldstride, tmp);
      g95_add_expr_to_block (&block, tmp);

      /* Now we check the stride to see if the array is packed.  */
      if (repack)
        {
          /* We save the packed strides, they may be needed later.  */
          strides[n] = g95_evaluate_now (stride, &block);

          if (n == 0)
            {
              /* The first dimension gets special handling.  */
              /* A stride of 0 means were passed a disposable temporary.
                 Otherwise we were passed an array section, so must
                 copy the result back.  The unpack variable needs function
                 scope.  */
              tmp = build (NE_EXPR, boolean_type_node, oldstride,
                          integer_zero_node);
              tmp = build_v (MODIFY_EXPR, unpack, tmp);
              g95_add_expr_to_block (&block, tmp);

              /* Block to execute if we are passed a temporary.  */
              g95_start_block (&subblock);

              /* Set both strides to 1.  */
              tmp = g95_conv_descriptor_stride (dumdesc, g95_rank_cst[n]);
              tmp = build_v (MODIFY_EXPR, tmp, integer_one_node);
              g95_add_expr_to_block (&subblock, tmp);

              tmp = build_v (MODIFY_EXPR, oldstride, integer_one_node);
              g95_add_expr_to_block (&subblock, tmp);

              /* It must be a packed array.  */
              tmp = build_v (MODIFY_EXPR, needpack, integer_zero_node);
              g95_add_expr_to_block (&subblock, tmp);

              nopack_stmt = g95_finish_block (&subblock);
              /* End of block.  */

              /* If we are passed an arry section then test if the array is
                 already packed.  */
              tmp = build (NE_EXPR, boolean_type_node, stride, oldstride);
              tmp = build_v (MODIFY_EXPR, needpack, tmp);

              tmp = build_v (COND_EXPR, unpack, tmp, nopack_stmt);
              g95_add_expr_to_block (&block, tmp);
            }
          else if (G95_DECL_PACKED_ARRAY (tmpdesc))
            {
              /* Test if the array is already packed.  */
              tmp = build (NE_EXPR, boolean_type_node, oldstride, stride);

              tmp = build (TRUTH_OR_EXPR, TREE_TYPE (tmp), needpack, tmp);
              tmp = build_v (MODIFY_EXPR, needpack, tmp);
              g95_add_expr_to_block (&block, tmp);
            }
          /* For partial packed arrays we just test the first stride.  */
        }
      else if (n == 0)
        {
          /* Even if we never repack the array, we still need to set the
             stride of passed temporaries to 1.  */
          g95_start_block (&subblock);

          tmp = g95_conv_descriptor_stride (dumdesc, g95_rank_cst[n]);
          tmp = build_v (MODIFY_EXPR, tmp, integer_one_node);
          g95_add_expr_to_block (&subblock, tmp);

          tmp = build_v (MODIFY_EXPR, oldstride, integer_one_node);
          g95_add_expr_to_block (&subblock, tmp);

          repack_stmt = g95_finish_block (&subblock);

          tmp = build (EQ_EXPR, g95_array_index_type, oldstride,
                       integer_zero_node);
          tmp = build_v (COND_EXPR, tmp, repack_stmt, empty_stmt_node);
          g95_add_expr_to_block (&block, tmp);
        }

      /* Store the passed stride.  Will be overwritten if array is repacked.  */
      tmp = g95_conv_descriptor_stride (tmpdesc, g95_rank_cst[n]);
      tmp = build_v (MODIFY_EXPR, tmp, oldstride);
      g95_add_expr_to_block (&block, tmp);

      /* Calculate the offset of the unpacked array.  */
      tmp = fold (build (MULT_EXPR, g95_array_index_type, oldstride,
                         lbound[n]));
      offset = fold (build (MINUS_EXPR, TREE_TYPE (tmp), offset, tmp));
      /* TODO: Only create one offset var.  */
      /* We need to evaluate this now because the stride variable will
         change.  */
      offset = g95_evaluate_now (offset, &block);

      /* Calculate the next packed stride.  */
      tmp = fold (build (MINUS_EXPR, g95_array_index_type, integer_one_node,
                         lbound[n]));
      tmp = fold (build (PLUS_EXPR, TREE_TYPE (tmp), ubound, tmp));
      stride = fold (build (MULT_EXPR, TREE_TYPE (tmp), stride, tmp));
    }

  if (repack)
    {
      tree packoffset;
      /* We will do this if the array needs repacking.  */
      g95_start_block (&subblock);

      /* Allocate storage for the array.  */
      ref = g95_conv_descriptor_data (tmpdesc);
      ref = build1 (ADDR_EXPR, ppvoid_type_node, ref);

      tmp = TYPE_SIZE_UNIT (g95_get_element_type (type));
      stride = build (MULT_EXPR, g95_array_index_type, stride, tmp);

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
      g95_add_expr_to_block (&subblock, tmp);

      /* Calculate the base pointer for the parameter.  */
      tmp = g95_conv_descriptor_data (dumdesc);
      tmp = build1 (INDIRECT_REF, TREE_TYPE (TREE_TYPE (tmp)), tmp);
      tmp = build (ARRAY_REF, TREE_TYPE (TREE_TYPE (tmp)), tmp, offset);
      tmp = build1 (ADDR_EXPR, g95_array_dataptr_type (dumdesc), tmp);

      ref = g95_conv_descriptor_base (dumdesc);
      tmp = build_v (MODIFY_EXPR, ref, tmp);
      g95_add_expr_to_block (&subblock, tmp);

      packoffset = integer_zero_node;
      /* Set the stride, and calculate the offset.  */
      for (n = 0; n < sym->as->rank; n++)
        {
          ref = g95_conv_descriptor_stride (tmpdesc, g95_rank_cst[n]);
          tmp = build_v (MODIFY_EXPR, ref, strides[n]);
          g95_add_expr_to_block (&subblock, tmp);

          if (n == 0)
            tmp = integer_one_node;
          else
            tmp = strides[n];
          tmp = fold (build (MULT_EXPR, g95_array_index_type, tmp, lbound[n]));

          packoffset = fold (build (MINUS_EXPR, g95_array_index_type,
                                    packoffset, tmp));
        }

      /* Calculate the base pointer for the temp.  */
      tmp = g95_conv_descriptor_data (tmpdesc);
      tmp = build1 (INDIRECT_REF, TREE_TYPE (TREE_TYPE (tmp)), tmp);
      tmp = build (ARRAY_REF, TREE_TYPE (TREE_TYPE (tmp)), tmp, packoffset);
      tmp = build1 (ADDR_EXPR, g95_array_dataptr_type (tmpdesc), tmp);

      ref = g95_conv_descriptor_base (tmpdesc);
      tmp = build_v (MODIFY_EXPR, ref, tmp);
      g95_add_expr_to_block (&subblock, tmp);

      if (sym->attr.dummy && sym->attr.intent != INTENT_OUT)
        {
          /* Copy the array data.  */
          g95_trans_repack_array (&subblock, tmpdesc,
                                 dumdesc, sym->as->rank);
        }

      repack_stmt = g95_finish_block (&subblock);
      /* End of code to repack the array.  */

      /* Create a block containing code for when the data is already
         packed.  */
      g95_start_block (&subblock);
    }
  else
    {
      g95_init_block (&subblock);
      repack_stmt = NULL_TREE;
    }

  /* This will be executed if we don't repack the array.  */
  /* Set the data pointer.  */
  tmp = g95_conv_descriptor_data (dumdesc);
  ref = g95_conv_descriptor_data (tmpdesc);
  tmp = build_v (MODIFY_EXPR, ref, tmp);
  g95_add_expr_to_block (&subblock, tmp);

  /* Calculate the base pointer.  Use the offset we calculated earlier.  */
  tmp = g95_conv_descriptor_data (tmpdesc);
  tmp = build1 (INDIRECT_REF, TREE_TYPE (TREE_TYPE (tmp)), tmp);
  tmp = build (ARRAY_REF, TREE_TYPE (TREE_TYPE (tmp)), tmp, offset);
  tmp = build1 (ADDR_EXPR, g95_array_dataptr_type (tmpdesc), tmp);

  ref = g95_conv_descriptor_base (tmpdesc);
  tmp = build_v (MODIFY_EXPR, ref, tmp);
  g95_add_expr_to_block (&subblock, tmp);

  if (repack)
    {
      /* If the array wan't repacked, we don't need to unpack it afterwards.  */
      tmp = build_v (MODIFY_EXPR, unpack, integer_zero_node);
      g95_add_expr_to_block (&subblock, tmp);

      nopack_stmt = g95_finish_block (&subblock);
      /* End of no-repack block.  */

      /* Choose which block.  */
      tmp = build_v (COND_EXPR, needpack, repack_stmt, nopack_stmt);
      g95_add_expr_to_block (&block, tmp);
    }
  else
    {
      /* We always want the no-repack code.  */
      tmp = g95_finish_block (&subblock);
      g95_add_expr_to_block (&block, tmp);
    }

  /* Add to the start of the function body.  */
  tmp = g95_finish_block (&block);

  g95_init_block (&block);
  g95_add_expr_to_block (&block, tmp);
  g95_add_expr_to_block (&block, body);


  g95_set_backend_locus (&loc);
  if (repack
      && ! (sym->attr.dummy && sym->attr.intent == INTENT_IN))
    {
      /* Build code to unpack the array data.  */
      g95_start_block (&subblock);

      g95_trans_repack_array (&subblock, dumdesc, tmpdesc, sym->as->rank);

      tmp = g95_finish_block (&subblock);

      /* If unpack is set, we must copy the array data back to
         the passed parameter.  */
      tmp = build_v (COND_EXPR, unpack, tmp, empty_stmt_node);

      g95_add_expr_to_block (&block, tmp);
    }

  if (repack)
    {
      /* Restore the original base.  */
      tmp = g95_conv_descriptor_base (dumdesc);
      tmp = build_v (MODIFY_EXPR, tmp, base);
      g95_add_expr_to_block (&block, tmp);
    }

  return g95_finish_block (&block);
}

/* Convert an array for passing as an actual function parameter.  Expressions
   and vector subscripts are evaluated and stored in a teporary, which is then
   passed.  For whole arrays the descriptor is passed.  For array sections
   a modified copy of the descriptor is passed, but using the original data.
   Also used for array pointer assignments by setting se->direct_byref.  */
void
g95_conv_array_parameter (g95_se * se, g95_expr * expr, g95_ss * ss)
{
  g95_loopinfo loop;
  g95_ss * secss;
  g95_ss_info *info;
  int need_tmp;
  int n;
  tree tmp;
  tree desc;
  stmtblock_t block;
  tree start;
  tree offset;
  int full;

  assert (ss != g95_ss_terminator);

  if (expr->ts.type == BT_CHARACTER)
    g95_todo_error ("Character string array actual parameters");

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

      info = &secss->data.info;

      g95_conv_ss_descriptor (&se->pre, secss, 0);
      desc = info->descriptor;
      if (G95_ARRAY_TYPE_P(TREE_TYPE (desc)))
        {
          /* Create a new descriptor if the array doesn't have one.  */
          full = 0;
        }
      else if (info->ref->u.ar.type == AR_FULL)
        full = 1;
      else if (se->direct_byref)
        full = 0;
      else
        {
          assert (info->ref->u.ar.type == AR_SECTION);

          full = 1;
          for (n = 0; n < info->ref->u.ar.dimen; n++)
            {
              /* Detect passing the full array as a section.  This could do
                 even more checking, but it doesn't seem worth it.  */
              if (info->ref->u.ar.start[n]
                  || info->ref->u.ar.end[n]
                  || (info->ref->u.ar.stride[n]
                      && ! g95_expr_is_one (info->ref->u.ar.stride[n], 0)))
                {
                  full = 0;
                  break;
                }
            }
        }
      if (full)
        {
          if (se->direct_byref)
            {
              /* Copy the descriptor for pointer assignments.  */
              g95_add_modify_expr (&se->pre, se->expr, desc);
            }
          else if (se->want_pointer)
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
                  se->expr = build1 (ADDR_EXPR,
                      build_pointer_type (TREE_TYPE (desc)), desc);
                }
            }
          else
            {
              se->expr = desc;
            }
          return;
        }
    }
  else
    {
      need_tmp = 1;
      secss = NULL;
      info = NULL;
    }

  g95_init_loopinfo (&loop);

  /* Associate the SS with the loop.  */
  g95_add_ss_to_loop (&loop, ss);

  /* Tell the scalarizer not to bother creating loop varliables, etc.  */
  if (! need_tmp)
    loop.array_parameter = 1;
  else
    assert (se->want_pointer && ! se->direct_byref);

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
      g95_start_scalarized_body (&loop, &block);

      g95_init_se (&lse, NULL);
      g95_add_block_to_block (&block, &lse.pre);
      g95_copy_loopinfo_to_se (&lse, &loop);

      g95_init_se (&rse, NULL);
      g95_add_block_to_block (&block, &rse.pre);
      g95_copy_loopinfo_to_se (&rse, &loop);

      lse.ss = loop.temp_ss;
      rse.ss = ss;

      g95_conv_scalarized_array_ref (&lse, NULL);
      g95_conv_expr_val (&rse, expr);

      g95_add_block_to_block (&block, &rse.pre);
      g95_add_block_to_block (&block, &lse.pre);

      tmp = build_v (MODIFY_EXPR, lse.expr, rse.expr);
      g95_add_expr_to_block (&block, tmp);

      g95_trans_scalarizing_loops (&loop, &block);

      /* Set the first stride component to zero to indicate a temporary.  */
      desc = loop.temp_ss->data.info.descriptor;
      tmp = g95_conv_descriptor_stride (desc, g95_rank_cst[0]);
      tmp = build_v (MODIFY_EXPR, tmp, integer_zero_node);
      g95_add_expr_to_block (&loop.pre, tmp);

      assert (is_simple_varname (desc));
      TREE_ADDRESSABLE (desc) = 1;
      se->expr = build1 (ADDR_EXPR, build_pointer_type (TREE_TYPE (desc)),
                        desc);
    }
  else
    {
      /* We pass sections without copying to a temporary.  A function may
         decide to repack the array to speed up access, but we're not
         bothered about that here.  */
      int dim;
      tree parm;
      tree parmtype;
      tree stride;
      tree from;
      tree to;
      tree base;

      /* Otherwise make a new descriptor and point it at the section we
         want.  The loop variable limits will be the limits of the section.
       */
      desc = info->descriptor;
      assert (secss && secss != g95_ss_terminator);
      if (se->direct_byref)
        {
          /* For pointer assignments we fill in the destination.  */
          parm = se->expr;
          parmtype = TREE_TYPE (parm);
        }
      else
        {
          /* Otherwise make a new one.  */
          parmtype = g95_get_element_type (TREE_TYPE (desc));
          parmtype = g95_get_array_type_bounds (parmtype, loop.dimen,
                                               loop.from, loop.to);
          parm = g95_create_var (parmtype, "parm");
        }

      offset = integer_zero_node;
      dim = 0;

      /* The following can be somewhat confusing.  We have two
         descriptors, a new one and the original array.
         {parm, parmtype, dim} refer to the new one.
         {desc, type, n, secss, loop} refer to the original.
         The bounds of the scaralization are the bounds of the section.
         We don't have to worry about numeric overflows when calculating
         the offsets because all elements are within the array data.  */

      /* Set the dtype.  */
      tmp = g95_conv_descriptor_dtype (parm);
      tmp = build_v (MODIFY_EXPR, tmp,
                   G95_TYPE_ARRAY_DTYPE (parmtype));
      g95_add_expr_to_block (&loop.pre, tmp);

      if (se->direct_byref)
        base = integer_zero_node;
      else
        base = NULL_TREE;

      for (n = 0; n < info->ref->u.ar.dimen; n++)
        {
          stride = g95_conv_array_stride (desc, n);

          /* Work out the offset.  */
          if (info->ref->u.ar.dimen_type[n] == DIMEN_ELEMENT)
            {
              assert (info->subscript[n]
                      && info->subscript[n]->type == G95_SS_SCALAR);
              start = info->subscript[n]->data.scalar.expr;
            }
          else
            {
              /* Check we haven't somehow got out of sync.  */
              assert (info->dim[dim] == n);

              /* Evaluate and remember the start of the section.  */
              start = info->start[dim];
              stride = g95_evaluate_now (stride, &loop.pre);
            }

          tmp = g95_conv_array_lbound (desc, n);
          tmp = fold (build (MINUS_EXPR, TREE_TYPE (tmp), start, tmp));

          tmp = fold (build (MULT_EXPR, TREE_TYPE (tmp), tmp, stride));
          offset = fold (build (PLUS_EXPR, TREE_TYPE (tmp), offset, tmp));

          if (info->ref->u.ar.dimen_type[n] == DIMEN_ELEMENT)
            {
              /* For elemental dimensions, we only need the offset.  */
              continue;
            }

          /* Vector subscripts need copying and are handled elsewhere.  */
          assert (info->ref->u.ar.dimen_type[n] == DIMEN_RANGE);

          /* Set the new lower bound.  */
          from = loop.from[dim];
          to = loop.to[dim];
          if (se->direct_byref && ! integer_onep (from))
            {
              /* Make sure the new section starts at 1.  */
              tmp = fold (build (MINUS_EXPR, TREE_TYPE (from),
                                 integer_one_node, from));
              to = fold (build (PLUS_EXPR, TREE_TYPE (to), to, tmp));
              from = integer_one_node;
            }
          tmp = g95_conv_descriptor_lbound (parm, g95_rank_cst[dim]);
          g95_add_modify_expr (&loop.pre, tmp, from);

          /* Set the new upper bound.  */
          tmp = g95_conv_descriptor_ubound (parm, g95_rank_cst[dim]);
          g95_add_modify_expr (&loop.pre, tmp, to);

          /* Multiply the stride by the section stride to get the
             total stride.  */
          stride = fold (build (MULT_EXPR, g95_array_index_type, stride,
                         info->stride[dim]));

          if (se->direct_byref)
            {
              base = fold (build (MINUS_EXPR, TREE_TYPE (base),
                                  base, stride));
            }

          /* Store the new stride.  */
          tmp = g95_conv_descriptor_stride (parm, g95_rank_cst[dim]);
          g95_add_modify_expr (&loop.pre, tmp, stride);


          dim++;
        }

      /* Point the data pointer at the first element in the section.  */
      tmp = g95_conv_array_data (desc);
      if (TREE_CODE (tmp) == INDIRECT_REF)
        tmp = TREE_OPERAND (tmp, 0);
      else
        tmp = build1 (INDIRECT_REF, TREE_TYPE (TREE_TYPE (tmp)), tmp);
      tmp = build (ARRAY_REF, TREE_TYPE (TREE_TYPE (tmp)), tmp, offset);
      offset = build1 (ADDR_EXPR, g95_array_dataptr_type (desc), tmp);

      tmp = g95_conv_descriptor_data (parm);
      g95_add_modify_expr (&loop.pre, tmp, offset);

      if (se->direct_byref)
        {
          /* Set the base pointer.  */
          tmp = g95_conv_array_data (desc);
          if (TREE_CODE (tmp) == INDIRECT_REF)
            tmp = TREE_OPERAND (tmp, 0);
          else
            tmp = build1 (INDIRECT_REF, TREE_TYPE (TREE_TYPE (tmp)), tmp);
          tmp = build (ARRAY_REF, TREE_TYPE (TREE_TYPE (tmp)), tmp, base);
          base = build1 (ADDR_EXPR, g95_array_dataptr_type (desc), tmp);

          tmp = g95_conv_descriptor_base (parm);
          g95_add_modify_expr (&loop.pre, tmp, base);
        }
      else
        {
          /* Invaidate the base pointer.  */
          tmp = g95_conv_descriptor_base (parm);
          tmp = build_v (MODIFY_EXPR, tmp, integer_zero_node);
          g95_add_expr_to_block (&loop.pre, tmp);
        }

      if (se->want_pointer)
        {
          /* Get a pointer to the new descriptor.  */
          se->expr = build1 (ADDR_EXPR, build_pointer_type (parmtype),
                             parm);
          TREE_ADDRESSABLE (parm) = 1;
        }
    }

  g95_add_block_to_block (&se->pre, &loop.pre);
  g95_add_block_to_block (&se->post, &loop.post);

  /* Cleanup the scalarizer.  */
  g95_cleanup_loop (&loop);
}

/* NULLIFY an allocated/pointer array on function entry, free it on exit.  */
tree
g95_trans_deferred_array (g95_symbol * sym, tree body)
{
  tree type;
  tree tmp;
  tree descriptor;
  tree deallocate;
  stmtblock_t block;
  stmtblock_t fnblock;
  locus loc;

  /* Make sure the frontend gets these right.  */
  if (! (sym->attr.pointer || sym->attr.allocatable))
    fatal_error ("Possible frontend bug: Deferred array size without pointer or allocatable attribute.");

  g95_init_block (&fnblock);

  assert (TREE_CODE (sym->backend_decl) == VAR_DECL);
  if (G95_DECL_STRING (sym->backend_decl))
    {
      tmp = G95_DECL_STRING_LENGTH (sym->backend_decl);
      if (! INTEGER_CST_P (tmp))
        g95_conv_init_string_length (sym, &fnblock);
    }


  /* Parameter variables don't need anything special.  */
  if (sym->attr.dummy)
    {
      g95_add_expr_to_block (&fnblock, body);

      return g95_finish_block (&fnblock);
    }

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
  tmp = build_v (MODIFY_EXPR, tmp, integer_zero_node);
  g95_add_expr_to_block (&fnblock, tmp);

  g95_add_expr_to_block (&fnblock, body);

  g95_set_backend_locus (&loc);
  /* Allocatable arrays need to be freed when they go out of scope.  */
  if (sym->attr.allocatable)
    {
      g95_start_block (&block);

      /* Deallocate if still allocated at the end of the procedure.  */
      deallocate = g95_array_deallocate (descriptor);

      tmp = g95_conv_descriptor_data (descriptor);
      tmp = build (NE_EXPR, boolean_type_node, tmp, integer_zero_node);
      tmp = build_v (COND_EXPR, tmp, deallocate, empty_stmt_node);
      g95_add_expr_to_block (&block, tmp);

      tmp = g95_finish_block (&block);
      g95_add_expr_to_block (&fnblock, tmp);
    }

  return g95_finish_block (&fnblock);
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

  head = g95_walk_subexpr (ss, expr->op1);
  if (expr->op2 == NULL)
    head2 = head;
  else
    head2 = g95_walk_subexpr (head, expr->op2);

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

      newss = g95_walk_subexpr (ss, arg->expr);
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
  if (g95_return_by_reference (sym) && sym->attr.dimension)
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
static g95_ss *
g95_walk_subexpr (g95_ss * ss, g95_expr * expr)
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

/* Reverse a SS chain.  */
static g95_ss *
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

/* Entry point for expression walking.  */
g95_ss *
g95_walk_expr (g95_ss * ss, g95_expr * expr)
{
  g95_ss *res;

  res = g95_walk_subexpr (ss, expr);
  return g95_reverse_ss (res);
}

