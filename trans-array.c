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

   If the expression is an assignment, we must then resolve and dependancies.
   In fortran all the rhs values of an assignment mush be evaluated before
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

   Finally g95_trans_scalarizing_loops is called to generate the impicit do
   loops. The loops are added to the pre chain of the loopinfo.

   For assignment expressions requiring a temporary two loops are
   generated.  The first stores the result of the expression in the temporary,
   the second copies it to the result.  */

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

void
g95_mark_ss_chain_used (g95_ss * ss)
{
  for (; ss != g95_ss_terminator; ss = ss->next)
    ss->used = 1;
}

/* Free a g95_ss chain.  */
static void
g95_free_ss_chain (g95_ss * ss)
{
  g95_ss *next;
  int n;

  while (ss != g95_ss_terminator)
    {
      assert (ss != NULL);
      next = ss->next;
      if (ss->type == G95_SS_SECTION || ss->type == G95_SS_VECTOR)
        {
          for (n = 0; n < G95_MAX_DIMENSIONS; n++)
            {
              if (ss->data.info.subscript[n])
                g95_free_ss_chain (ss->data.info.subscript[n]);
            }
        }

      g95_free (ss);
      ss = next;
    }
}

/* Free all the SS associated with a loop.  */
void
g95_cleanup_loop (g95_loopinfo * loop)
{
  g95_ss *ss;
  g95_ss *next;
  int n;

  ss = loop->ss;
  while (ss != g95_ss_terminator)
    {
      assert (ss != NULL);
      next = ss->loop_chain;
      if (ss->type == G95_SS_SECTION || ss->type == G95_SS_VECTOR)
        {
          for (n = 0; n < G95_MAX_DIMENSIONS; n++)
            {
              if (ss->data.info.subscript[n])
                g95_free_ss_chain (ss->data.info.subscript[n]);
            }
        }
      g95_free (ss);
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
                                  tree size)
{
  tree field;
  tree tmp;
  tree stmt;
  tree args;
  tree desc;

  desc = info->descriptor;
  field = g95_get_data_component (TREE_TYPE (desc));
  if (g95_can_put_var_on_stack (size))
    {
      /* Make a temporary variable to hold the data.  */
      tmp = g95_get_stack_array_type (size);
      tmp = create_tmp_var (tmp, "A");
      tmp = build1 (ADDR_EXPR, TREE_TYPE (field), tmp);
      info->data =
        g95_simple_fold (tmp, &loop->pre, &loop->pre_tail, NULL);

      tmp = build (COMPONENT_REF, TREE_TYPE (field), desc, field);
      tmp = build (MODIFY_EXPR, TREE_TYPE (tmp), tmp, info->data);
      stmt = build_stmt (EXPR_STMT, tmp);
      g95_add_stmt_to_pre (loop, stmt, stmt);
      info->pdata = NULL_TREE;
    }
  else
    {
      /* Allocate memory to hold the data.  */
      tmp = build (COMPONENT_REF, TREE_TYPE (field), desc, field);
      tmp = build1 (ADDR_EXPR, ppvoid_type_node, tmp);

      info->pdata = g95_create_tmp_var (ppvoid_type_node);
      tmp = build (MODIFY_EXPR, TREE_TYPE (info->pdata), info->pdata, tmp);
      stmt = build_stmt (EXPR_STMT, tmp);
      g95_add_stmt_to_pre (loop, stmt, stmt);

      /* Build a call to allocate storage.  */
      args = g95_chainon_list (NULL_TREE, info->pdata);
      args = g95_chainon_list (args, size);

      if (g95_array_index_kind == 4)
        tmp = g95_fndecl_internal_malloc;
      else if (g95_array_index_kind == 8)
        tmp = g95_fndecl_internal_malloc64;
      else
        abort();
      tmp = g95_build_function_call (tmp, args);
      stmt = build_stmt (EXPR_STMT, tmp);
      g95_add_stmt_to_pre (loop, stmt, stmt);

      tmp = build (COMPONENT_REF, TREE_TYPE (field), desc, field);
      info->data = g95_simple_fold (tmp, &loop->pre, &loop->pre_tail, NULL);
    }

  /* The offset is zero because we create temporaries with a zero
     lower bound.  */
  field = g95_get_base_component (TREE_TYPE (desc));
  tmp = build (COMPONENT_REF, TREE_TYPE (field), desc, field);
  tmp = build (MODIFY_EXPR, TREE_TYPE (field), tmp, info->data);
  stmt = build_stmt (EXPR_STMT, tmp);
  g95_add_stmt_to_pre (loop, stmt, stmt);

  if (info->pdata != NULL_TREE)
    {
      /* Free the temporary.  */
      tmp = g95_chainon_list (NULL_TREE, info->pdata);
      tmp = g95_build_function_call (g95_fndecl_internal_free, tmp);
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
                               tree eltype)
{
  tree type;
  tree field;
  tree desc;
  tree stmt;
  tree tmp;
  tree tmpvar;
  tree size;
  tree sizevar;
  int n;

  /* Set the lower bound to zero.  */
  for (n = 0; n < loop->dimen; n++)
    {
      if (! loop->temp_used)
        {
          tmp = build (MINUS_EXPR, g95_array_index_type,
                       loop->to[n], loop->from[n]);
          loop->to[n] =
            g95_simple_fold (tmp, &loop->pre, &loop->pre_tail, NULL);
          loop->from[n] = integer_zero_node;
        }
      else
        assert (integer_zerop (loop->from[n]));

      info->delta[n] = integer_zero_node;
      info->start[n] = integer_zero_node;
      info->stride[n] = integer_one_node;
    }
  info->dimen = loop->dimen;

  /* Initialise the descriptor.  */
  type = g95_get_array_type_bounds(eltype, loop->dimen, loop->from, loop->to);
  desc = g95_create_tmp_var (type);
  G95_DECL_PACKED_ARRAY (desc) = 1;
  info->descriptor = desc;
  tmpvar = NULL_TREE;
  sizevar = NULL_TREE;
  size = integer_one_node;
  /* Fill in the bounds and stride.  This is a packed array, so:
      size = 1;
      for (n = 0; n < rank; n++)
        {
          stride[n] = size
          delta = ubound[n] + 1 - lbound[n];
          size = size * delta;
        }
        size = size * sizeof(element);  */
  for (n = 0; n < loop->dimen; n++)
    {
      field = g95_get_stride_component (type, n);
      tmp = build (COMPONENT_REF, TREE_TYPE (field), desc, field);
      tmp = build (MODIFY_EXPR, TREE_TYPE (tmp), tmp, size);
      stmt = build_stmt (EXPR_STMT, tmp);
      g95_add_stmt_to_pre (loop, stmt, stmt);

      field = g95_get_lbound_component (type, n);
      tmp = build (COMPONENT_REF, TREE_TYPE (field), desc, field);
      tmp = build (MODIFY_EXPR, TREE_TYPE (tmp), tmp, integer_zero_node);
      stmt = build_stmt (EXPR_STMT, tmp);
      g95_add_stmt_to_pre (loop, stmt, stmt);

      field = g95_get_ubound_component (type, n);
      tmp = build (COMPONENT_REF, TREE_TYPE (field), desc, field);
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

  /* Get the size of the array.  */
  tmp = build (MULT_EXPR, g95_array_index_type, size,
      TYPE_SIZE_UNIT (g95_get_element_type (type)));
  size = g95_simple_fold (tmp, &loop->pre, &loop->pre_tail, &sizevar);

  g95_trans_allocate_array_storage (loop, info, size);

  loop->temp_used = 1;

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
              c->expr->value.constructor, poffset, offsetvar);
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

/* Array constructors are handled by constructing a temporary, then using that
   within the scalarization loop.  This is not optimal, seems by far the
   simplest method.  */
static void
g95_trans_array_constructor (g95_loopinfo * loop, g95_ss * ss)
{
  tree offset;
  tree offsetvar;
  tree type;
  tree desc;
  tree size;

  type = g95_typenode_for_spec (&ss->expr->ts);
  size = g95_trans_allocate_temp_array (loop, &ss->data.info, type);

  desc = ss->data.info.descriptor;
  offset = integer_zero_node;
  offsetvar = create_tmp_alias_var (g95_array_index_type, "offset");
  TREE_USED (offsetvar) = 0;
  g95_trans_array_constructor_value (&loop->pre, &loop->pre_tail, type,
      ss->data.info.data, ss->expr->value.constructor, &offset, &offsetvar);

  if (TREE_USED (offsetvar))
    pushdecl (offsetvar);
  else
    assert (INTEGER_CST_P (offset));

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
      switch (ss->type)
        {
        case G95_SS_SCALAR:
          /* Scalar expression.  Evaluate this now.  This includes elemental
             dimension indices, but not array section bounds.  */
          g95_init_se (&se, NULL);
          g95_conv_simple_val (&se, ss->expr);
          g95_add_stmt_to_pre (loop, se.pre, se.pre_tail);
          g95_add_stmt_to_post (loop, se.post, se.post_tail);

          ss->data.scalar = se.expr;
          break;

        case G95_SS_REFERENCE:
          /* Scalar reference.  Evaluate this now.  This includes elemental
             dimension indices, but not array section bounds.  */
          g95_init_se (&se, NULL);
          g95_conv_simple_reference (&se, ss->expr);
          g95_add_stmt_to_pre (loop, se.pre, se.pre_tail);
          g95_add_stmt_to_post (loop, se.post, se.post_tail);

          ss->data.scalar = se.expr;
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
  tree field;

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
      field = g95_get_base_component (TREE_TYPE (se.expr));

      se.expr = build (COMPONENT_REF, TREE_TYPE (field), se.expr, field);
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
   packed and 0 is an array is not packed.  */
static int
g95_array_is_packed (tree descriptor)
{
  tree decl;

  decl = descriptor;

  assert (G95_DESCRIPTOR_TYPE_P (TREE_TYPE (descriptor)));

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
      tmp = G95_TYPE_DESCRIPTOR_STRIDE (type, dim);
      if (tmp != NULL_TREE)
        {
          assert (INTEGER_CST_P (tmp));
          return tmp;
        }
    }

  tmp = g95_get_stride_component (type, dim);
  tmp = build (COMPONENT_REF, TREE_TYPE (tmp), descriptor, tmp);

  return tmp;
}

/* Like g95_conv_array_stride, but for the lower bound.  */
tree
g95_conv_array_lbound (tree descriptor, int dim)
{
  tree tmp;
  tree type;

  type = TREE_TYPE (descriptor);

  tmp = G95_TYPE_DESCRIPTOR_LBOUND (type, dim);
  if (tmp != NULL_TREE)
    {
      assert (INTEGER_CST_P (tmp));
      return tmp;
    }

  tmp = g95_get_lbound_component (type, dim);
  tmp = build (COMPONENT_REF, TREE_TYPE (tmp), descriptor, tmp);

  return tmp;
}

/* Like g95_conv_array_stride, but for the upper bound.  */
tree
g95_conv_array_ubound (tree descriptor, int dim)
{
  tree tmp;
  tree type;

  type = TREE_TYPE (descriptor);

  tmp = G95_TYPE_DESCRIPTOR_UBOUND (type, dim);
  if (tmp != NULL_TREE)
    {
      assert (INTEGER_CST_P (tmp));
      return tmp;
    }

  tmp = g95_get_ubound_component (type, dim);
  tmp = build (COMPONENT_REF, TREE_TYPE (tmp), descriptor, tmp);

  return tmp;
}

/* Translate an array reference.  The descriptor should be in se->expr.  */
/*GCC ARRAYS*/
static void
g95_conv_array_index_ref (g95_se * se, tree pointer, tree * indices,
                          int dimen, int safe)
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
      if (flag_bounds_check && ! safe)
        {
          /* Check array bounds.  */
          tree cond;
          tree bound;

          bound = g95_conv_array_lbound (se->expr, n);
          bound = g95_simple_fold (bound, &se->pre, &se->pre_tail, &boundvar);
          cond = build (LT_EXPR, boolean_type_node, indices[n], bound);
          cond = g95_simple_fold (cond, &se->pre, &se->pre_tail, &condvar);
          fault = build (TRUTH_OR_EXPR, boolean_type_node, fault, cond);
          fault =
            g95_simple_fold_tmp (fault, &se->pre, &se->pre_tail, &faultvar);

          bound = g95_conv_array_ubound (se->expr, n);
          bound = g95_simple_fold (bound, &se->pre, &se->pre_tail, &boundvar);
          cond = build (GT_EXPR, boolean_type_node, indices[n], bound);
          cond = g95_simple_fold (cond, &se->pre, &se->pre_tail, &condvar);
          fault = build (TRUTH_OR_EXPR, boolean_type_node, fault, cond);
          fault =
            g95_simple_fold_tmp (fault, &se->pre, &se->pre_tail, &faultvar);
        }

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

  if (flag_bounds_check && ! safe)
    {
      g95_trans_runtime_check (fault, g95_strconst_fault, &se->pre,
          &se->pre_tail);
    }

  /* Result = data[index].  */
  assert (TREE_CODE (TREE_TYPE (array)) == ARRAY_TYPE);
  tmp = build (ARRAY_REF, TREE_TYPE( TREE_TYPE (array)), array, index);

  /* Check we've used the correct number of dimensions.  */
  assert (TREE_CODE (TREE_TYPE (tmp)) != ARRAY_TYPE);

  se->expr = tmp;
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
        indices[n] = info->subscript[n]->data.scalar;
        break;

      case DIMEN_RANGE:
        indices[n] = index;
        break;

      case DIMEN_VECTOR:
        indices[n] =
          g95_conv_vector_array_index (se, index,
                                       info->subscript[n], pvar);
        break;

      default:
        abort();
      }
   }
  /* Get the index from the vector.  */
  g95_conv_array_index_ref (se, info->data, indices, ar->dimen, 0);
  index = g95_simple_fold (se->expr, &se->pre, &se->pre_tail, pvar);
  /* Put the descriptor back.  */
  se->expr = descsave;

  return index;
}

/* Return the offset for an index.  */
static tree
g95_conv_array_index_offset (g95_se * se, g95_ss_info * info, int n,
                             g95_array_ref * ar, tree stride, tree * tmpvar)
{
  tree index;
  int dim;

  /* Get the index into the array for this dimension.  */
  if (ar)
    {
      assert (ar->type != AR_ELEMENT);
      if (ar->dimen_type[n] == DIMEN_ELEMENT)
        {
          /* Elemental dimension.  */
          assert (info->subscript[n]
                  && info->subscript[n]->type == G95_SS_SCALAR);
          /* We've already translated this value outside the loop.  */
          index = info->subscript[n]->data.scalar;
        }
      else
        {
          /* Scalarized dimension.  */
          assert (info && se->loop);
          /* Find the scalarizer variable for this loop.  */
          for (dim = 0; dim < info->dimen; dim++)
            {
              if (info->dim[dim] == n)
                break;
            }
          assert (dim < info->dimen);

          index = se->loop->loopvar[dim];
          index = build (MULT_EXPR, g95_array_index_type, index,
                         info->stride[dim]);
          index = g95_simple_fold (index, &se->pre, &se->pre_tail, tmpvar);

          index = build (PLUS_EXPR, g95_array_index_type, index,
                         info->delta[dim]);
          index = g95_simple_fold (index, &se->pre, &se->pre_tail, tmpvar);

          if (ar->dimen_type[n] == DIMEN_VECTOR)
            {
              index = g95_conv_vector_array_index (se, index,
                  info->subscript[n], tmpvar);
            }
          else
            assert (ar->dimen_type[n] == DIMEN_RANGE);
        }
    }
  else
    {
      /* Temporary array.  */
      assert (se->loop);
      index = se->loop->loopvar[n];
    }

  /* Multiply by the stride.  */
  index = build (MULT_EXPR, g95_array_index_type, index, stride);
  index = g95_simple_fold_tmp (index, &se->pre, &se->pre_tail, tmpvar);

  return index;
}

/* Build a reference to a scalarized array.  */
void
g95_conv_scalarized_array_ref (g95_se * se, g95_array_ref * ar)
{
  g95_ss_info * info;
  tree index;
  tree tmp;
  tree tmpvar;
  int n;

  info = &se->ss->data.info;
  tmpvar = NULL_TREE;
  n = info->dim[se->loop->order[0]];
  index = g95_conv_array_index_offset (se, info, n, ar, info->stride0, &tmpvar);
  index = build (PLUS_EXPR, g95_array_index_type, index, info->offset);
  index = g95_simple_fold (index, &se->pre, &se->pre_tail, &tmpvar);

  tmp = build1 (INDIRECT_REF, TREE_TYPE (TREE_TYPE (info->data)), info->data);
  se->expr = build (ARRAY_REF, TREE_TYPE (TREE_TYPE (tmp)), tmp, index);
}

/* Build an array reference. se->expr already holds the array descriptor.
   This should be either a variable, indirect variable reference or component
   reference.  */
void
g95_conv_array_ref (g95_se * se, g95_array_ref * ar)
{
  int n;
  tree index;
  tree field;
  tree tmp;
  tree tmpvar;
  tree stride;
  tree stridevar;
  tree indexvar;
  tree fault;
  tree faultvar;
  tree condvar;
  g95_se indexse;

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

  field = g95_get_base_component (TREE_TYPE (se->expr));
  tmp = build (COMPONENT_REF, TREE_TYPE (field), se->expr, field);
  tmp = g95_simple_fold (tmp, &se->pre, &se->pre_tail, NULL);
  tmp = build1 (INDIRECT_REF, TREE_TYPE (TREE_TYPE (tmp)), tmp);
  se->expr = build (ARRAY_REF, TREE_TYPE (TREE_TYPE (tmp)), tmp, index);
}

/* Start a scalarized expression.  Creates a scope and declares loop
   variables.  */
void
g95_start_scalarized_body (g95_loopinfo * loop)
{
  int n;
  int i;
  int dim;
  tree head;
  tree tail;
  tree index;
  tree tmpvar;
  tree stridevar;
  tree stride;
  g95_ss *ss;
  g95_ss_info *info;
  g95_se se;

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

      /* This code will be executed before entering the scalarization loop
         for this dimension.  */
      head = tail = NULL_TREE;

      for (ss = loop->ss; ss != g95_ss_terminator; ss = ss->loop_chain)
        {
          if (! ss->used)
            continue;

          if (ss->type != G95_SS_SECTION
              && ss->type != G95_SS_FUNCTION
              && ss->type != G95_SS_CONSTRUCTOR)
            continue;

          info = &ss->data.info;

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
                      stride = g95_simple_fold (stride, &head, &tail,
                                                &stridevar);
                      index = g95_conv_array_index_offset (&se, info,
                          i, &info->ref->u.ar, stride, &tmpvar);
                      g95_add_stmt_to_list (&head, &tail, se.pre, se.pre_tail);
                      assert (se.post == NULL);

                      info->offset = build (PLUS_EXPR, g95_array_index_type,
                                            info->offset, index);
                      info->offset =
                        g95_simple_fold_tmp (info->offset, &head, &tail, NULL);
                    }
                }
              assert (info->dimen == loop->dimen);
              stride = g95_conv_array_stride (info->descriptor, info->dim[0]);
              info->stride0 = g95_simple_fold (stride, &head, &tail, NULL);
            }
          else
            {
              /* Add the offset for the previous loop dimension.  */
              g95_array_ref *ar;

              if (info->ref)
                ar = &info->ref->u.ar;
              else
                ar = NULL;

              i = loop->order[dim + 1];
              g95_init_se (&se, NULL);
              se.loop = loop;
              se.expr = info->descriptor;
              stride = g95_conv_array_stride (info->descriptor, i);
              stride = g95_simple_fold (stride, &head, &tail, &stridevar);
              index = g95_conv_array_index_offset (&se, info, info->dim[i],
                  ar, stride, &tmpvar);
              g95_add_stmt_to_list (&head, &tail, se.pre, se.pre_tail);
              assert (se.post == NULL);
              info->offset = build (PLUS_EXPR, g95_array_index_type,
                                    info->offset, index);
              info->offset = g95_simple_fold_tmp (info->offset, &head, &tail,
                                                  NULL);
            }
        }

      loop->code[n] = head;
      loop->code_tail[n] = tail;
    }
  g95_start_stmt ();
}

/* Generates the actual loops for a scalarized expression.  */
void
g95_trans_scalarizing_loops (g95_loopinfo * loop, tree head, tree tail)
{
  tree init;
  tree cond;
  tree inc;
  tree tmp;
  tree stmt;
  int n;
  int dim;
  g95_ss *ss;

  stmt = g95_finish_stmt (head, tail);

  g95_create_tmp_var (g95_real4_type_node);
  for (dim = 0; dim < loop->dimen; dim++)
    {
      /* We may have reordered the loops during scalarization.  */
      n = loop->order[dim];

      head = loop->code[n];
      tail = loop->code_tail[n];

      tmp = build (MODIFY_EXPR, g95_array_index_type,
                  loop->loopvar[n], loop->from[n]);
      init = build_stmt (EXPR_STMT, tmp);
      cond = build (LE_EXPR, g95_array_index_type,
                   loop->loopvar[n], loop->to[n]);
      tmp = build (PLUS_EXPR, g95_array_index_type,
                  loop->loopvar[n], integer_one_node);
      inc = build (MODIFY_EXPR, g95_array_index_type,
                  loop->loopvar[n], tmp);
      stmt = build_stmt (FOR_STMT, init, cond, inc, stmt);

      g95_add_stmt_to_list (&head, &tail, stmt, stmt);
      stmt = g95_finish_stmt (head, tail);
      loop->loopvar[n] = NULL_TREE;
      loop->code[n] = loop->code_tail[n] = NULL_TREE;
    }

  g95_add_stmt_to_pre (loop, stmt, NULL_TREE);

  for (ss = loop->ss; ss; ss = ss->loop_chain)
    ss->used = 0;
}

/* Calculates the range start and stride for a SS chain.  Also gets the
   descriptor and data pointer.  The range of vector subscripts is the size
   of the vector.  */
void
g95_conv_ss_startstride (g95_loopinfo * loop)
{
  int n;
  int dim;
  g95_ss_info *info;
  tree tmp;
  tree field;
  g95_ss *ss;
  g95_ss *vecss;
  g95_se se;
  g95_expr *start;
  g95_expr *stride;
  tree desc;

  loop->dimen = 0;
  /* Determine the rank of the loop.  */
  for (ss = loop->ss; ss != g95_ss_terminator; ss = ss->loop_chain)
    {
      if (ss->type == G95_SS_SECTION)
        {
          loop->dimen = ss->data.info.dimen;
          break;
        }
    }

  if (loop->dimen == 0)
    internal_error ("Unable to determine rank of expression");

  /* loop over all the SS in the chain.  */
  for (ss = loop->ss; ss != g95_ss_terminator; ss = ss->loop_chain)
    {
      if (ss->type != G95_SS_SECTION)
        continue;

      info = &ss->data.info;

      /* Get the descriptor for the array.  */
      g95_conv_ss_descriptor (loop, ss);

      for (n = 0; n < loop->dimen; n++)
        {
          /* For vector array subscripts we want the size of the vector.  */
          dim = info->dim[n];
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
            }
          else
            {
              /* No lower bound specified so use the bound of the array.  */
              info->start[n] =
                G95_TYPE_DESCRIPTOR_LBOUND (TREE_TYPE (desc), dim);

              if (info->start[n] == NULL_TREE)
                {
                  info->start[n] = g95_create_tmp_var (g95_array_index_type);
                  field = g95_get_lbound_component (TREE_TYPE (desc), dim);
                  tmp = build (COMPONENT_REF, g95_array_index_type,
                              desc, field);
                  tmp = build (MODIFY_EXPR, g95_array_index_type,
                              info->start[n], tmp);
                  tmp = build_stmt (EXPR_STMT, tmp);
                  g95_add_stmt_to_pre (loop, tmp, tmp);
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
            }
        }
    }
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
  g95_se se;
  g95_ss_info *info;
  g95_ss_info *specinfo;
  g95_ss *ss;
  g95_ss *vecss;
  tree tmp;
  tree field;
  tree var;
  tree desc;
  g95_ss *loopspec[G95_MAX_DIMENSIONS];
  g95_expr *end;

  for (n = 0; n < loop->dimen; n++)
    {
      loopspec[n] = NULL;
      /* We use one SS term, and use that to determine the bounds of the
         loop for this dimension.  We try to pick the simplest term.  */
      for (ss = loop->ss ; ss != g95_ss_terminator ; ss = ss->loop_chain)
        {
          if (ss->type != G95_SS_SECTION)
            continue;

          info = &ss->data.info;
          if (loopspec[n])
            specinfo = &loopspec[n]->data.info;
          else
            specinfo = NULL;
          info = &ss->data.info;

          /* Criteria for choosing a loop specifier (most important first):
              stride of one
              known stride
              known lower bound
              known upper bound
           */
          if (! specinfo)
            loopspec[n] = ss;
          else if (integer_onep (info->stride[n])
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

      if (! loopspec[n])
        internal_error ("Unable to find scalarization loop specifier");

      info = &loopspec[n]->data.info;

      /* Set the extents of this range.  */
      loop->from[n] = info->start[n];
      info->delta[n] = integer_zero_node;

      /* For vector array subscripts we want the size of the vector.  */
      dim = info->dim[n];
      vecss = loopspec[n];
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
          loop->to[n] = se.expr;
        }
      else
        {
          /* No upper bound was specified, so use the bound of the array. */
          loop->to[n] = G95_TYPE_DESCRIPTOR_UBOUND (TREE_TYPE (desc), dim);

          if (loop->to[n] == NULL_TREE)
            {
              field = g95_get_ubound_component (TREE_TYPE (desc), dim);
              tmp = build (COMPONENT_REF, g95_array_index_type,
                          desc, field);
              loop->to[n] = g95_simple_fold (tmp, &loop->pre, &loop->pre_tail,
                                            NULL);
            }
        }

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
      /* TODO: better way of allocating temporaries.  */
      assert (loop->temp_ss->type == G95_SS_TEMP);
      g95_trans_allocate_temp_array (loop, &loop->temp_ss->data.info,
                                     loop->temp_ss->data.info.descriptor);
      loop->temp_ss->type = G95_SS_SECTION;
    }

  g95_add_loop_ss_code (loop, loop->ss);

  if (loop->temp_used)
    {
      for (n = 0; n < loop->dimen; n++)
        loopspec[n] = NULL;
    }

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

      for (n = 0; n < loop->dimen; n++)
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
tree
g95_array_init_size (tree descriptor, int rank, tree * poffset,
                    g95_expr ** lower, g95_expr ** upper,
                    tree * phead, tree * ptail)
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
  tree field;
  g95_se se;
  int n;

  type = TREE_TYPE (descriptor);

  tmpvar = NULL_TREE;
  stridevar = NULL_TREE;
  stride = integer_one_node;
  offsetvar = NULL_TREE;
  offset = integer_zero_node;

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
      field = g95_get_lbound_component (type, n);
      tmp = build (COMPONENT_REF, TREE_TYPE (field), descriptor, field);
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

      field = g95_get_ubound_component (type, n);
      tmp = build (COMPONENT_REF, TREE_TYPE (field), descriptor, field);
      tmp = build (MODIFY_EXPR, TREE_TYPE (tmp), tmp, se.expr);
      stmt = build_stmt (EXPR_STMT, tmp);
      g95_add_stmt_to_list (phead, ptail, stmt, stmt);

      /* Store the stride.  */
      field = g95_get_stride_component (type, n);
      tmp = build (COMPONENT_REF, TREE_TYPE (field), descriptor, field);
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

  /* The stride is the number of elements in the array, so multiply by the
     size of an element to get the total size.  */
  tmp = TYPE_SIZE_UNIT (g95_get_element_type (type));
  stride = build (MULT_EXPR, g95_array_index_type, stride, tmp);
  stride = g95_simple_fold_tmp (stride, phead, ptail, &stridevar);

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
  tree field;
  tree tmp;
  tree stmt;
  tree pointer;
  tree allocate;
  tree offset;
  tree size;
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

  size = g95_array_init_size (se->expr, ref->u.ar.as->rank, &offset,
      lower, upper, &se->pre, &se->pre_tail);

  /* Allocate memory to store the data.  */
  field = g95_get_data_component (TREE_TYPE (se->expr));
  tmp = build (COMPONENT_REF, TREE_TYPE (field), se->expr, field);
  tmp = build1 (ADDR_EXPR, build_pointer_type (TREE_TYPE (field)), tmp);
  pointer = g95_simple_fold (tmp, &se->pre, &se->pre_tail, NULL);

  if (g95_array_index_type == g95_int4_type_node)
    allocate = g95_fndecl_allocate;
  else if (g95_array_index_type == g95_int8_type_node)
    allocate = g95_fndecl_allocate64;
  else
    abort();

  tmp = g95_chainon_list (NULL_TREE, pointer);
  tmp = g95_chainon_list (tmp, size);
  tmp = g95_chainon_list (tmp, pstat);
  tmp = g95_build_function_call (allocate, tmp);
  stmt = build_stmt (EXPR_STMT, tmp);
  g95_add_stmt_to_pre (se, stmt, stmt);

  /* Set base = &data[offset].  */
  tmp = build (COMPONENT_REF, TREE_TYPE (field), se->expr, field);
  if (! integer_zerop (offset))
    {
      pointer = g95_create_tmp_var (TREE_TYPE (field));
      tmp = build (MODIFY_EXPR, TREE_TYPE (tmp), pointer, tmp);
      stmt = build_stmt (EXPR_STMT, tmp);
      g95_add_stmt_to_pre (se, stmt, stmt);

      tmp = build1 (INDIRECT_REF, TREE_TYPE (TREE_TYPE (pointer)), pointer);
      tmp = build (ARRAY_REF, TREE_TYPE (TREE_TYPE (tmp)), tmp, offset);
      tmp = build1 (ADDR_EXPR, TREE_TYPE (pointer), tmp);
    }
  field = g95_get_base_component(TREE_TYPE (se->expr));
  field = build (COMPONENT_REF, TREE_TYPE (field), se->expr, field);
  tmp = build (MODIFY_EXPR, TREE_TYPE (field), field, tmp);
  stmt = build_stmt (EXPR_STMT, tmp);
  g95_add_stmt_to_pre (se, stmt, stmt);
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
  tree field;
  tree stmt;

  field = g95_get_data_component (TREE_TYPE (descriptor));

  /* Get a pointer to the data.  */
  tmp = build (COMPONENT_REF, TREE_TYPE (field), descriptor, field);
  tmp = build1 (ADDR_EXPR, build_pointer_type (TREE_TYPE (field)), tmp);
  var = g95_create_tmp_var (TREE_TYPE (tmp));
  tmp = build (MODIFY_EXPR, TREE_TYPE (tmp), var, tmp);
  stmt = build_stmt (EXPR_STMT, tmp);

  /* Parameter is the address of the data component.  */
  tmp = g95_chainon_list (NULL_TREE, var);
  tmp = g95_chainon_list (tmp, integer_zero_node);
  tmp = g95_build_function_call (g95_fndecl_deallocate, tmp);
  stmt = chainon (stmt, build_stmt (EXPR_STMT, tmp));

  return stmt;
}

/* Generate code to initialize the descriptor for an array variable.  */
/*GCC ARRAYS*/
tree
g95_trans_auto_array_allocation (tree descriptor, g95_array_spec * as)
{
  tree offset;
  tree size;
  tree pointer;
  tree field;
  tree tmp;
  tree ref;
  tree stmt;
  tree tmpvar;
  tree head;
  tree tail;

  g95_start_stmt ();

  head = tail = NULL_TREE;

  size = g95_array_init_size (descriptor, as->rank, &offset,
                              as->lower, as->upper, &head, &tail);

  /* Allocate the array.  */
  field = g95_get_data_component (TREE_TYPE (descriptor));

  if (g95_can_put_var_on_stack (size))
    {
      /* Create a temporary variable to hold the data.  */
      tmp = g95_get_stack_array_type (size);
      tmpvar = create_tmp_alias_var (tmp, "A");

      /* Store the address.  */
      pointer = build1 (ADDR_EXPR, TREE_TYPE (field), tmpvar);
      pointer = g95_simple_fold (pointer, &head, &tail, NULL);
      tmp = build (COMPONENT_REF, TREE_TYPE (field), descriptor, field);
      tmp = build (MODIFY_EXPR, TREE_TYPE (field), tmp, pointer);
      stmt = build_stmt (EXPR_STMT, tmp);
      g95_add_stmt_to_list (&head, &tail, stmt, stmt);
    }
  else
    {
      /* Allocate memory to hold the data.  */
      /* Get the address of the data component.  */
      tmp = build (COMPONENT_REF, TREE_TYPE (field), descriptor, field);
      tmp = build1 (ADDR_EXPR, build_pointer_type (TREE_TYPE (tmp)), tmp);
      pointer = g95_create_tmp_var (TREE_TYPE (tmp));
      tmp = build (MODIFY_EXPR, TREE_TYPE (pointer), pointer, tmp);
      stmt = build_stmt (EXPR_STMT, tmp);
      g95_add_stmt_to_list (&head, &tail, stmt, stmt);

      /* Now allocate the memory.  */
      tmp = g95_chainon_list (NULL_TREE, pointer);
      tmp = g95_chainon_list (tmp, size);
      if (g95_array_index_kind == 4)
        ref = g95_fndecl_internal_malloc;
      else if (g95_array_index_kind == 8)
        ref = g95_fndecl_internal_malloc64;
      else
        abort();

      tmp = g95_build_function_call (ref, tmp);
      stmt = build_stmt (EXPR_STMT, tmp);
      g95_add_stmt_to_list (&head, &tail, stmt, stmt);

      field = g95_get_data_component (TREE_TYPE (descriptor));
      pointer = build (COMPONENT_REF, TREE_TYPE (field), descriptor, field);
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
  field = g95_get_base_component (TREE_TYPE (descriptor));
  ref = build (COMPONENT_REF, TREE_TYPE (field), descriptor, field);
  tmp = build (MODIFY_EXPR, TREE_TYPE(ref), ref, tmp);
  stmt = build_stmt (EXPR_STMT, tmp);
  g95_add_stmt_to_list (&head, &tail, stmt, stmt);

  head = g95_finish_stmt (head, tail);

  if (tmpvar != NULL_TREE)
    pushdecl (tmpvar);

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
  tree field;
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

      tmp = g95_build_function_call (g95_fndecl_repack[dimen - 1], args);
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
      loop.loopvar[n] = g95_create_tmp_var (g95_array_index_type);
      loop.from[n] = g95_conv_array_lbound (tmp, n);
      loop.to[n] = g95_conv_array_ubound (tmp, n);
      loop.to[n] = g95_simple_fold (loop.to[n], phead, ptail, NULL);
      loop.loopvar[n] = g95_create_tmp_var (g95_array_index_type);

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
  field = g95_get_base_component (TREE_TYPE (dest));
  tmp = build (COMPONENT_REF, TREE_TYPE (field), dest, field);
  lss->data.info.data = g95_simple_fold (tmp, phead, ptail, NULL);
  lss->data.info.descriptor = dest;
  lss->type = G95_SS_SECTION;
  lss->next = g95_ss_terminator;
  lss->data.info.dimen = dimen;

  field = g95_get_base_component (TREE_TYPE (src));
  tmp = build (COMPONENT_REF, TREE_TYPE (field), src, field);
  rss->data.info.data = g95_simple_fold (tmp, phead, ptail, NULL);
  rss->data.info.descriptor = src;
  rss->type = G95_SS_SECTION;
  rss->next = g95_ss_terminator;
  rss->data.info.dimen = dimen;

  g95_add_ss_to_loop (&loop, lss);
  g95_add_ss_to_loop (&loop, rss);

  /* The loop body.  */
  g95_mark_ss_chain_used (lss);
  g95_mark_ss_chain_used (rss);
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
  tree field;
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
      unpack = g95_create_tmp_var (boolean_type_node);
      /* We need to save the passed descriptor base.  */
      field = g95_get_base_component (type);
      base = g95_create_tmp_var (TREE_TYPE (field));
    }

  g95_start_stmt ();

  head = tail = NULL_TREE;

  if (repack)
    {
      /* Save the old base.  */
      field = g95_get_base_component (type);
      tmp = build (COMPONENT_REF, TREE_TYPE (field), dumdesc, field);
      tmp = build (MODIFY_EXPR, TREE_TYPE (base), base, tmp);
      stmt = build_stmt (EXPR_STMT, tmp);
      g95_add_stmt_to_list (&head, &tail, stmt, stmt);

      needpack = packedvar = g95_create_tmp_var (boolean_type_node);
    }
  else
    needpack = packedvar = NULL_TREE;

  oldstride = g95_create_tmp_var (g95_array_index_type);

  for (n = 0; n < sym->as->rank; n++)
    {
      /* Set the desired lower bound.  */
      field = g95_get_lbound_component (type, n);

      assert (sym->as->lower[n]);
      g95_init_se (&se, NULL);
      g95_conv_simple_val_type (&se, sym->as->lower[n], g95_array_index_type);
      g95_add_stmt_to_list (&head, &tail, se.pre, se.pre_tail);
      lbound[n] = se.expr;

      tmp = build (COMPONENT_REF, TREE_TYPE (field), tmpdesc, field);
      tmp = build (MODIFY_EXPR, TREE_TYPE (tmp), tmp, lbound[n]);
      stmt = build_stmt (EXPR_STMT, tmp);
      g95_add_stmt_to_list (&head, &tail, stmt, stmt);

      if (checkparm || ! sym->as->upper[n])
        {
          /* Get the upper bound of the actual parameter.  */
          field = g95_get_ubound_component (type, n);
          ubound = build (COMPONENT_REF, TREE_TYPE (field), dumdesc, field);
          ubound = g95_simple_fold (ubound, &head, &tail, &uboundvar);
        }
      else
        ubound = NULL_TREE;

      if (checkparm || ! sym->as->upper[n])
        {
          /* Get the lower bound of the actual parameter.  */
          field = g95_get_lbound_component (type, n);
          ref = build (COMPONENT_REF, TREE_TYPE (field), dumdesc, field);
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
      field = g95_get_ubound_component (type, n);
      tmp = build (COMPONENT_REF, g95_array_index_type, tmpdesc, field);
      tmp = build (MODIFY_EXPR, TREE_TYPE (tmp), tmp, ubound);
      stmt = build_stmt (EXPR_STMT, tmp);
      g95_add_stmt_to_list (&head, &tail, stmt, stmt);

      /* Get the passed stride.  */
      field = g95_get_stride_component (type, n);
      tmp = build (COMPONENT_REF, TREE_TYPE (field), dumdesc, field);
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
              tmp = build (COMPONENT_REF, TREE_TYPE (field), dumdesc, field);
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

          tmp = build (COMPONENT_REF, TREE_TYPE (field), dumdesc, field);
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
      ref = build (COMPONENT_REF, TREE_TYPE (field), tmpdesc, field);
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
      field = g95_get_data_component (type);
      ref = build (COMPONENT_REF, TREE_TYPE (field), tmpdesc, field);
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
        tmp = g95_fndecl_internal_malloc;
      else if (g95_array_index_kind == 8)
        tmp = g95_fndecl_internal_malloc64;
      else
        abort();
      tmp = g95_build_function_call (tmp, args);
      stmt = build_stmt (EXPR_STMT, tmp);
      g95_add_stmt_to_list (&repack_stmt, &pack_tail, stmt, stmt);

      /* Calculate the base pointer for the parameter.  */
      field = g95_get_data_component (type);
      tmp = build (COMPONENT_REF, TREE_TYPE (field), dumdesc, field);
      tmp = g95_simple_fold (tmp, &repack_stmt, &pack_tail, &pointervar);
      tmp = build1 (INDIRECT_REF, TREE_TYPE (TREE_TYPE (tmp)), tmp);
      tmp = build (ARRAY_REF, TREE_TYPE (TREE_TYPE (tmp)), tmp, offset);
      tmp = build1 (ADDR_EXPR, TREE_TYPE (field), tmp);

      field = g95_get_base_component (type);
      ref = build (COMPONENT_REF, TREE_TYPE (field), dumdesc, field);
      tmp = build (MODIFY_EXPR, TREE_TYPE (ref), ref, tmp);
      stmt = build_stmt (EXPR_STMT, tmp);
      g95_add_stmt_to_list (&nopack_stmt, &pack_tail, stmt, stmt);

      packoffset = integer_zero_node;
      /* Set the stride, and calculate the offset.  */
      for (n = 0; n < sym->as->rank; n++)
        {
          field = g95_get_stride_component (type, n);
          ref = build (COMPONENT_REF, TREE_TYPE (field), tmpdesc, field);
          tmp = build (MODIFY_EXPR, g95_array_index_type, ref, strides[n]);
          stmt = build_stmt (EXPR_STMT, tmp);
          g95_add_stmt_to_list (&repack_stmt, &pack_tail, stmt, stmt);

          if (n == 0)
            tmp = integer_one_node;
          else
            tmp = strides[n - 1];
          tmp = build (MULT_EXPR, g95_array_index_type, tmp, lbound[n]);
          tmp = g95_simple_fold (tmp, &repack_stmt, &pack_tail,
                                (n == 0) ? &offsetvar : &tmpvar);

          packoffset = build (MINUS_EXPR, g95_array_index_type, packoffset,
                             tmp);
          packoffset = g95_simple_fold_tmp (packoffset, &repack_stmt,
                                           &pack_tail, &offsetvar);
        }

      /* Calculate the base pointer for the temp.  */
      field = g95_get_data_component (type);
      tmp = build (COMPONENT_REF, TREE_TYPE (field), tmpdesc, field);
      tmp = g95_simple_fold (tmp, &nopack_stmt, &pack_tail, &pointervar);
      tmp = build1 (INDIRECT_REF, TREE_TYPE (TREE_TYPE (tmp)), tmp);
      tmp = build (ARRAY_REF, TREE_TYPE (TREE_TYPE (tmp)), tmp, packoffset);
      tmp = build1 (ADDR_EXPR, TREE_TYPE (field), tmp);

      field = g95_get_base_component (type);
      ref = build (COMPONENT_REF, TREE_TYPE (field), tmpdesc, field);
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
  field = g95_get_data_component (type);
  tmp = build (COMPONENT_REF, TREE_TYPE (field), dumdesc, field);
  ref = build (COMPONENT_REF, TREE_TYPE (field), tmpdesc, field);
  tmp = build (MODIFY_EXPR, TREE_TYPE (ref), ref, tmp);
  stmt = build_stmt (EXPR_STMT, tmp);
  g95_add_stmt_to_list (&nopack_stmt, &pack_tail, stmt, stmt);

  /* Calculate the base pointer.  Use the offset we calculated earlier.  */
  field = g95_get_data_component (type);
  tmp = build (COMPONENT_REF, TREE_TYPE (field), tmpdesc, field);
  tmp = g95_simple_fold (tmp, &nopack_stmt, &pack_tail, NULL);
  tmp = build1 (INDIRECT_REF, TREE_TYPE (TREE_TYPE (tmp)), tmp);
  tmp = build (ARRAY_REF, TREE_TYPE (TREE_TYPE (tmp)), tmp, offset);
  tmp = build1 (ADDR_EXPR, TREE_TYPE (field), tmp);

  field = g95_get_base_component (type);
  ref = build (COMPONENT_REF, TREE_TYPE (field), tmpdesc, field);
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
      field = g95_get_base_component (type);
      tmp = build (COMPONENT_REF, TREE_TYPE (field), dumdesc, field);
      tmp = build (MODIFY_EXPR, TREE_TYPE (base), tmp, base);
      stmt = build_stmt (EXPR_STMT, tmp);
      body = chainon (body, stmt);
    }

  return body;
}

/* Retirns true if the expr is an integer constant value 1.  */
static int
g95_expr_is_one (g95_expr * expr)
{
  assert (expr != NULL);

  if (expr->expr_type != EXPR_CONSTANT)
    return 0;

  if (expr->ts.type != BT_INTEGER)
    return 0;

  return mpz_cmp_si (expr->value.integer, 1) == 0;
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
  /* Find the first array section.  */
  secss = ss;
  while (secss != g95_ss_terminator
         && secss->type != G95_SS_SECTION)
    secss = secss->next;

  /* TODO: Passing function array return values and array constructors as
     actual parameters.  */
  assert (secss != g95_ss_terminator);

  ss = g95_reverse_ss (ss);

  /* Associate the SS with the loop.  */
  g95_add_ss_to_loop (&loop, ss);

  /* If we have a single array section, we can pass it directly.  If we have an
     expression or vector subscripts we need to copy it into a temporary.  */
  if (expr->expr_type == EXPR_VARIABLE)
    {
      g95_ss *vss;

      need_tmp = 0;
      for (n = 0; n < secss->data.info.dimen; n++)
        {
          vss = secss->data.info.subscript[secss->data.info.dim[n]];
          if (vss && vss->type == G95_SS_VECTOR)
            need_tmp = 1;
        }
    }
  else
    need_tmp = 1;

  /* Tell the scalarizer not to bother creating loop varliables, etc.  */
  if (! need_tmp)
    loop.array_parameter = 1;

  /* Setup the scalarizing loops and bounds.  */
  g95_conv_ss_startstride (&loop);

  if (need_tmp)
    {
      /* Tell the scalarizer to make a temporary.  */
      loop.temp_ss = g95_get_ss ();
      loop.temp_ss->type = G95_SS_TEMP;
      loop.temp_ss->next = g95_ss_terminator;
      loop.temp_ss->data.info.descriptor =
        g95_get_element_type (TREE_TYPE (secss->data.info.descriptor));
      loop.temp_ss->data.info.dimen = loop.dimen;
      g95_add_ss_to_loop (&loop, loop.temp_ss);
    }

  g95_conv_loop_setup (&loop);

  if (need_tmp)
    {
      /* Copy into a temporary and pass that.  We don't need to copy the data
         back because expressions and vector subscripts must be INTENT_IN.  */
      g95_se lse;
      g95_se rse;

      g95_mark_ss_chain_used (loop.temp_ss);
      g95_mark_ss_chain_used (ss);
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
      tmp = g95_get_stride_component (TREE_TYPE (desc), 0);
      tmp = build (COMPONENT_REF, TREE_TYPE (tmp), desc, tmp);
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
      tree field;
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
      if (info->ref->u.ar.type != AR_FULL)
        {
          assert (info->ref->u.ar.type == AR_SECTION);

          for (n = 0; n < info->ref->u.ar.dimen; n++)
            {
              /* Detect passing the full array as a section.  This could do
                 even more checking, but it doesn't seem worth it.  */
              if (info->ref->u.ar.start[n]
                  || info->ref->u.ar.end[n]
                  || ! g95_expr_is_one (info->ref->u.ar.stride[n]))
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
          parmtype = g95_get_element_type (type);
          parmtype = g95_get_array_type_bounds (parmtype, loop.dimen,
                                               loop.from, loop.to);
          parm = g95_create_tmp_var (parmtype);

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
              field = g95_get_lbound_component (parmtype, dim);
              tmp = build (COMPONENT_REF, TREE_TYPE (field), parm, field);
              tmp = build (MODIFY_EXPR, TREE_TYPE (tmp), tmp, loop.from[dim]);
              stmt = build_stmt (EXPR_STMT, tmp);
              g95_add_stmt_to_pre (&loop, stmt, stmt);

              /* Set the new upper bound.  */
              field = g95_get_ubound_component (parmtype, dim);
              tmp = build (COMPONENT_REF, TREE_TYPE (field), parm, field);
              tmp = build (MODIFY_EXPR, TREE_TYPE (tmp), tmp, loop.to[dim]);
              stmt = build_stmt (EXPR_STMT, tmp);
              g95_add_stmt_to_pre (&loop, stmt, stmt);

              /* Multiply the stride by the section stride to get the
                 total stride.  */
              stride = fold (build (MULT_EXPR, g95_array_index_type, stride,
                             info->stride[n]));
              field = g95_get_stride_component (type, n);
              tmp = build (COMPONENT_REF, TREE_TYPE (field), parm, field);
              tmp = build (MODIFY_EXPR, TREE_TYPE (tmp), tmp, stride);
              stmt = build_stmt (EXPR_STMT, tmp);
              g95_add_stmt_to_pre (&loop, stmt, stmt);

              dim++;
            }

          /* Point the data pointer at the first element in the section.  */
          field = g95_get_data_component (type);
          tmp = build (COMPONENT_REF, TREE_TYPE (field), desc, field);
          tmp = g95_simple_fold (tmp, &loop.pre, &loop.pre_tail, NULL);
          tmp = build1 (INDIRECT_REF, TREE_TYPE (TREE_TYPE (tmp)), tmp);
          tmp = build (ARRAY_REF, TREE_TYPE (TREE_TYPE (tmp)), tmp, offset);
          offset = build1 (ADDR_EXPR, TREE_TYPE (field), tmp);

          tmp = build (COMPONENT_REF, TREE_TYPE (field), parm, field);
          tmp = build (MODIFY_EXPR, TREE_TYPE (tmp), tmp, offset);
          stmt = build_stmt (EXPR_STMT, tmp);
          g95_add_stmt_to_pre (&loop, stmt, stmt);

          /* Invaidate the base pointer.  */
          field = g95_get_base_component (type);
          tmp = build (COMPONENT_REF, TREE_TYPE (field), parm, field);
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
  tree field;
  tree stmt;
  tree deallocate;
  locus loc;

  /* Make sure the frontend gets these right.  */
  if (! (sym->attr.pointer || sym->attr.allocatable))
    fatal_error ("Possible frontend bug: Deferred array size without pointer or allocatable attribute.");

  /* Parameter variables don't need anything special.  */
  if (sym->attr.dummy)
    return body;

  if (sym->attr.save)
    return body;

  g95_get_backend_locus (&loc);
  g95_set_backend_locus (&sym->declared_at);
  /* Get the descriptor type.  */
  descriptor = sym->backend_decl;
  type = TREE_TYPE (sym->backend_decl);
  assert (G95_DESCRIPTOR_TYPE_P (type));

  /* NULLIFY the data pointer.  */
  field = g95_get_data_component (type);
  tmp = build (COMPONENT_REF, TREE_TYPE(field), descriptor, field);
  tmp = build (MODIFY_EXPR, TREE_TYPE(tmp), tmp, integer_zero_node);
  stmt = build_stmt (EXPR_STMT, tmp);
  body = chainon (stmt, body);

  g95_set_backend_locus (&loc);
  /* Allocatable arrays meed to be free when they go out of scope.  */
  if (sym->attr.allocatable && ! sym->attr.save)
    {
      g95_start_stmt ();

      /* Deallocate if still allocated at the end of the procedure.  */
      g95_start_stmt ();
      deallocate = g95_array_deallocate (descriptor);
      deallocate = g95_finish_stmt (deallocate, NULL_TREE);

      var = g95_create_tmp_var (TREE_TYPE (field));
      tmp = build (COMPONENT_REF, TREE_TYPE(field), descriptor, field);
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

   TODO:Extension - multiple component subscripts.
    x(:,:) = foo%a(:)%b(:)
   Transforms to
    forall (i=..., j=...)
      x(i,j) = foo%a(j)%b(i)
    end forall
   This adds a fair amout of complexity because you need to deal with more
   than one ref.  Could be handled in a similar manner to vector subscripts.  */

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
                  indexss = g95_walk_expr (g95_ss_terminator, ar->start[n]);
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
g95_walk_array_constructor (g95_ss * ss, g95_expr *expr)
{
  g95_ss *newss;

  newss = g95_get_ss ();
  newss->type = G95_SS_CONSTRUCTOR;
  newss->expr = expr;
  newss->next = ss;

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

