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

/* Add the pre chains for all the scalar expressions in a SS chain.  */
void
g95_add_ss_stmts (g95_loopinfo * loop, g95_ss * ss)
{
  g95_se *se;
  int n;

  assert (ss != NULL);

  while (ss != g95_ss_terminator)
    {
      if (ss->dimen == 0)
        {
          se = &ss->data.se;

          g95_add_stmt_to_pre (loop, se->pre, se->pre_tail);
          g95_add_stmt_to_post (loop, se->post, se->post_tail);
          se->pre = se->pre_tail = NULL_TREE;
          se->post = se->post_tail = NULL_TREE;
        }
      else if (ss->dimen > 0)
        {
          for (n = 0; n < ss->dimen; n++)
            {
              /* Add the scalar expressions for a vector subscript.  */
              if (ss->data.info.vector[n])
                g95_add_ss_stmts (loop, ss->data.info.vector[n]->next);
            }
        }
      else /* A vector SS should never be in a normal chain.  */
        abort();
      ss = ss->next;
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

  for (n = 0; n < G95_MAX_DIMENSIONS; n++)
    loop->order[n] = n;
}

/* Copies the loop variable info to a g95_se sructure. Does not copy the SS
   chain.  */
void
g95_copy_loopinfo_to_se (g95_se * se, g95_loopinfo * loop)
{
  int n;

  for (n = 0; n < loop->dimen; n++)
    {
      se->loopvar[n] = loop->loopvar[n];
    }
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

/* Generates the actual loops for a scalarized expression.  
   There are several optimizations that could be done (eg. loop unrolling),
   common expression extraction, however they are not beneficial in all cases,
   so I'm leaving that to the backend.  If seems to make a fairly good job of
   it.  */
tree
g95_trans_scalarizing_loops (g95_loopinfo * loop, tree body)
{
  tree init;
  tree cond;
  tree inc;
  tree tmp;
  int n;
  int dim;

  for (dim = 0; dim < loop->dimen; dim++)
    {
      n = loop->order[dim];
      tmp = build (MODIFY_EXPR, g95_array_index_type,
                  loop->loopvar[n], loop->from[n]);
      init = build_stmt (EXPR_STMT, tmp);
      cond = build (LE_EXPR, g95_array_index_type,
                   loop->loopvar[n], loop->to[n]);
      tmp = build (PLUS_EXPR, g95_array_index_type,
                  loop->loopvar[n], integer_one_node);
      inc = build (MODIFY_EXPR, g95_array_index_type,
                  loop->loopvar[n], tmp);
      body = build_stmt (FOR_STMT, init, cond, inc, body);
    }

  return body;
}

/* Calculates the range start and stride for a SS chain.  Also gets the
   descriptor and data pointer.  */
void
g95_conv_ss_startstride (g95_loopinfo * loop)
{
  int n;
  int dim;
  int vecn;
  g95_ss_info *info;
  tree tmp;
  tree field;
  g95_ss *ss;
  g95_ss *vecss;
  g95_se se;
  g95_expr *start;
  g95_expr *stride;
  tree desc;

  for (ss = loop->ss; ss != g95_ss_terminator; ss = ss->next)
    {
      if (ss->dimen <= 0)
          continue;

      info = &ss->data.info;

      g95_conv_ss_descriptor (loop, ss);

      for (n = 0; n < ss->dimen; n++)
        {
          /* For vector array subscripts we want the size of the vector.  */
          dim = info->dim[n];
          vecss = ss;
          vecn = n;
          while (vecss->data.info.ref->u.ar.dimen_type[dim] == DIMEN_VECTOR)
            {
              vecss = vecss->data.info.vector[vecn];
              assert (vecss && vecss->dimen == -1);
              g95_conv_ss_descriptor (loop, vecss);
              dim = vecss->data.info.dim[0];
              vecn = 0;
            }

          assert (vecss->data.info.ref->u.ar.dimen_type[dim] == DIMEN_RANGE);
          start = vecss->data.info.ref->u.ar.start[dim];
          stride = vecss->data.info.ref->u.ar.stride[dim];
          desc = vecss->data.info.descriptor;

          /* Calculate the start of the range.  */
          if (start)
            {
              g95_init_se (&se, NULL);
              g95_conv_simple_val_type (&se, start, g95_array_index_type);
              info->start[n] = se.expr;
            }
          else
            {
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

/* Initialise the scalarization loop parameters.  */
/*GCC ARRAYS*/
void
g95_conv_loopvars (g95_loopinfo * loop)
{
  int n;
  int dim;
  int vecn;
  g95_se se;
  g95_ss_info *info;
  g95_ss_info *specinfo;
  g95_ss *ss;
  g95_ss *vecss;
  tree tmp;
  tree stmt;
  tree field;
  tree var;
  tree type;
  tree desc;
  tree size;
  tree sizevar;
  tree tmpvar;
  g95_ss *loopspec[G95_MAX_DIMENSIONS];
  g95_expr *end;

  for (n = 0; n < loop->dimen; n++)
    {
      loopspec[n] = NULL;
      /* Find a loop to get the range from.  */
      for (ss = loop->ss ; ss != g95_ss_terminator ; ss = ss->next)
        {
          if (ss->dimen <= 0)
            continue;

          info = &ss->data.info;
          if (loopspec[n])
            specinfo = &loopspec[n]->data.info;
          else
            specinfo = NULL;
          info = &ss->data.info;

          /* Criteria for choosing a loop specifier (in order of importance):
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

      info = &loopspec[n]->data.info;

      /* Set the extents of this range.  */
      loop->from[n] = info->start[n];
      info->delta[n] = integer_zero_node;

      /* For vector array subscripts we want the size of the vector.  */
      dim = info->dim[n];
      vecss = loopspec[n];
      vecn = n;
      while (vecss->data.info.ref->u.ar.dimen_type[dim] == DIMEN_VECTOR)
        {
          vecss = vecss->data.info.vector[vecn];
          assert (vecss && vecss->dimen == -1);
          dim = vecss->data.info.dim[0];
          vecn = 0;
        }

      assert (vecss->data.info.ref->u.ar.dimen_type[dim] == DIMEN_RANGE);
      end = vecss->data.info.ref->u.ar.end[dim];
      desc = vecss->data.info.descriptor;

      if (end)
        {
          g95_init_se (&se, NULL);
          g95_conv_simple_val_type (&se, end, g95_array_index_type);
          loop->to[n] = se.expr;
        }
      else
        {
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

          /* Now divide by the stride.  */
          tmp = build (TRUNC_DIV_EXPR, g95_array_index_type, loop->to[n],
                      info->stride[n]);
          loop->to[n] =
            g95_simple_fold (tmp, &loop->pre, &loop->pre_tail, &var);
        }

      /* Create the loop variable.  */
      if (! loop->array_parameter)
        loop->loopvar[n] = g95_create_tmp_var (g95_array_index_type);
    }

  /* If we're using a temporary create it.  */
  if (loop->temp_ss != NULL)
    {
      info = &loop->temp_ss->data.info;
      /* Set the lower bound to zero.  */
      for (n = 0; n < loop->dimen; n++)
        {
          tmp = build (MINUS_EXPR, g95_array_index_type,
                      loop->to[n], loop->from[n]);
          loop->to[n] =
            g95_simple_fold (tmp, &loop->pre, &loop->pre_tail, NULL);
          loop->from[n] = integer_zero_node;
          loopspec[n] = loop->temp_ss;

          info->delta[n] = integer_zero_node;
        }

      /* Initialise the descriptor.  */
      type = info->descriptor;
      type = g95_get_array_type_bounds(type, loop->dimen, loop->from, loop->to);
      desc = g95_create_tmp_var (type);
      G95_DECL_PACKED_ARRAY (desc) = 1;
      info->descriptor = desc;
      tmpvar = NULL_TREE;
      sizevar = NULL_TREE;
      size = integer_one_node;
      for (n = 0; n < loop->dimen ; n++)
        {
          /* Fill in the bounds and stride.  */
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

      field = g95_get_data_component (type);
      if (g95_can_put_var_on_stack (size))
        {
          /* Make a temporary variable to hold the result.  */
          tmp = g95_get_stack_array_type (size);
          tmp = g95_create_tmp_var (tmp);
          tmp = build1 (ADDR_EXPR, TREE_TYPE (field), tmp);
          info->data =
            g95_simple_fold (tmp, &loop->pre, &loop->pre_tail, NULL);

          tmp = build (COMPONENT_REF, TREE_TYPE (field), desc, field);
          tmp = build (MODIFY_EXPR, TREE_TYPE (tmp), tmp, info->data);
          stmt = build_stmt (EXPR_STMT, tmp);
          g95_add_stmt_to_pre (loop, stmt, stmt);
        }
      else
        {
          tmp = build (COMPONENT_REF, TREE_TYPE (field), desc, field);
          tmp = build1 (ADDR_EXPR, ppvoid_type_node, tmp);

          info->pdata = g95_create_tmp_var (ppvoid_type_node);
          tmp = build (MODIFY_EXPR, TREE_TYPE (info->pdata), info->pdata, tmp);
          stmt = build_stmt (EXPR_STMT, tmp);
          g95_add_stmt_to_pre (loop, stmt, stmt);

          /* Build a call to allocate storage.  */
          tmp = g95_chainon_list (NULL_TREE, info->pdata);
          tmp = g95_chainon_list (tmp, size);

          if (g95_array_index_kind == 4)
            var = g95_fndecl_internal_malloc;
          else if (g95_array_index_kind == 8)
            var = g95_fndecl_internal_malloc64;
          else
            abort();
          tmp = g95_build_function_call (var, tmp);
          stmt = build_stmt (EXPR_STMT, tmp);
          g95_add_stmt_to_pre (loop, stmt, stmt);

          tmp = build (COMPONENT_REF, TREE_TYPE (field), desc, field);
          info->data = g95_simple_fold (tmp, &loop->pre, &loop->pre_tail, NULL);
        }

      /* The offset is zero because we create temporaries with a zero
         lower bound.  */
      field = g95_get_base_component (type);
      tmp = build (COMPONENT_REF, TREE_TYPE (field), desc, field);
      tmp = build (MODIFY_EXPR, TREE_TYPE (field), tmp, info->data);
      stmt = build_stmt (EXPR_STMT, tmp);
      g95_add_stmt_to_pre (loop, stmt, stmt);
    }

  if (loop->array_parameter)
    return;

  /* Calculate the offsets of the loops.  */
  for (ss = loop->ss; ss != g95_ss_terminator; ss = ss->next)
    {
      if (ss->dimen <= 0)
        continue;

      info = &ss->data.info;

      for (n = 0; n < ss->dimen; n++)
        {
          dim = info->dim[n];

          /* If we are specifying the range the delta should already be set.  */
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
   will be a simple_val, ie a variable or a constant.  */
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
      /* Lower bound.  */
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

      size = build (MINUS_EXPR, g95_array_index_type,
                   integer_one_node, se.expr);
      size = g95_simple_fold (size, phead, ptail, &tmpvar);

      /* Upper bound.  */
      g95_init_se (&se, NULL);
      assert (upper[n]);
      g95_conv_simple_val_type (&se, upper[n], g95_array_index_type);
      g95_add_stmt_to_list (phead, ptail, se.pre, se.pre_tail);

      field = g95_get_ubound_component (type, n);
      tmp = build (COMPONENT_REF, TREE_TYPE (field), descriptor, field);
      tmp = build (MODIFY_EXPR, TREE_TYPE (tmp), tmp, se.expr);
      stmt = build_stmt (EXPR_STMT, tmp);
      g95_add_stmt_to_list (phead, ptail, stmt, stmt);

      /* Store the stride.  This is zero for Type A arrays.  We still need to
         calculate the stride to get the size of the array.  */
      field = g95_get_stride_component (type, n);
      tmp = build (COMPONENT_REF, TREE_TYPE (field), descriptor, field);
      tmp = build (MODIFY_EXPR, TREE_TYPE (tmp), tmp, stride);
      stmt = build_stmt (EXPR_STMT, tmp);
      g95_add_stmt_to_list (phead, ptail, stmt, stmt);

      /* size = ubound + 1 - lbound.  */
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

/* Initialises the descriptor and generates a call to __g95_allocate.  */
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

  tmp = build (COMPONENT_REF, TREE_TYPE (field), descriptor, field);
  tmp = build1 (ADDR_EXPR, build_pointer_type (TREE_TYPE (field)), tmp);
  var = g95_create_tmp_var (TREE_TYPE (tmp));
  tmp = build (MODIFY_EXPR, TREE_TYPE (tmp), var, tmp);
  stmt = build_stmt (EXPR_STMT, tmp);

  /* Parameter is address of the data component.  */
  tmp = g95_chainon_list (NULL_TREE, var);
  tmp = g95_chainon_list (tmp, integer_zero_node);
  tmp = g95_build_function_call (g95_fndecl_deallocate, tmp);
  stmt = chainon (stmt, build_stmt (EXPR_STMT, tmp));

  return stmt;
}

/* Helper Macro. Adds a STMT_EXPR to list(head, tail).  */
#define ADD_EXPR_STMT(expr) {tree stmt__tmp = build_stmt (EXPR_STMT, expr); \
                    g95_add_stmt_to_list (&head, &tail, stmt__tmp, stmt__tmp);}

/* Generates setup and cleanup code for arrays, and adds it to function body.
   End result is something like:

    subroutine foo (...)
    {
      __g95_push_context ()
      foreach (local_array)
        {
          <initialise descriptor>
          <allocate data>
          array.base = &array.data[-sum(array.lbound[]*array.stride[])]
        }
      foreach (non-pointer array parameter)
        {
          <adjust bounds to match assumed lower bound>
        }

      <function body code>
    __return_foo:
      foreach (non-pointer array parameter)
        {
          <restore bounds>
        }
      __g95_pop_context ()
    }
 */
/*GCC ARRAYS*/
tree
g95_trans_auto_array_allocation (g95_symbol * sym)
{
  g95_expr **lower;
  g95_expr **upper;
  tree offset;
  tree size;
  tree head;
  tree tail;
  tree pointer;
  tree field;
  tree tmp;
  tree ref;
  tree assign;

  g95_start_stmt ();

  head = tail = NULL_TREE;

  lower = sym->as->lower;
  upper = sym->as->upper;

  size = g95_array_init_size (sym->backend_decl, sym->as->rank, &offset,
      lower, upper, &head, &tail);

  field = g95_get_data_component (TREE_TYPE (sym->backend_decl));

  if (g95_can_put_var_on_stack (size))
    {
      tmp = build (COMPONENT_REF, TREE_TYPE (field), sym->backend_decl, field);
      /* The address will be fixed up later.  */
      assign = build (MODIFY_EXPR, TREE_TYPE (field), tmp, NULL_TREE);
      ADD_EXPR_STMT (assign);
    }
  else
    {
      tmp = build (COMPONENT_REF, TREE_TYPE (field), sym->backend_decl, field);
      tmp = build1 (ADDR_EXPR, build_pointer_type (TREE_TYPE (tmp)), tmp);
      pointer = g95_create_tmp_var (TREE_TYPE (tmp));
      tmp = build (MODIFY_EXPR, TREE_TYPE (pointer), pointer, tmp);
      ADD_EXPR_STMT (tmp);

      /* Now allocate the memory.  Parameters to __g95_internal_alloc
         are address of data pointer and size of block.  */
      tmp = g95_chainon_list (NULL_TREE, size);
      tmp = g95_chainon_list (tmp, pointer);
      if (g95_array_index_kind == 4)
        ref = g95_fndecl_internal_malloc;
      else if (g95_array_index_kind == 8)
        ref = g95_fndecl_internal_malloc64;
      else
        abort();

      tmp = g95_build_function_call (ref, tmp);
      ADD_EXPR_STMT (tmp);
      assign = NULL_TREE;
    }

  tmp = build (COMPONENT_REF, TREE_TYPE (field), sym->backend_decl, field);

  if (! integer_zerop (offset))
    {
      /* pointer = array.data */
      pointer = g95_create_tmp_var (TREE_TYPE (field));
      tmp = build (MODIFY_EXPR, TREE_TYPE (field), pointer, tmp);
      ADD_EXPR_STMT (tmp);

      /* array.base = &pointer[offset] */
      tmp = build1 (INDIRECT_REF, TREE_TYPE (TREE_TYPE (pointer)), pointer);
      tmp = build (ARRAY_REF, TREE_TYPE (TREE_TYPE (tmp)), tmp, offset);
      tmp = build1 (ADDR_EXPR, TREE_TYPE(pointer), tmp);
    }
  field = g95_get_base_component (TREE_TYPE (sym->backend_decl));
  ref = build (COMPONENT_REF, TREE_TYPE (field), sym->backend_decl, field);
  tmp = build (MODIFY_EXPR, TREE_TYPE(ref), ref, tmp);
  ADD_EXPR_STMT (tmp);

  head = g95_finish_stmt (head, tail);

  if (assign != NULL_TREE)
    {
      /* Fixup the address of the data.  The temp must be created with function
       * scope.  */
      tmp = g95_get_stack_array_type (size);
      ref = g95_create_tmp_var (tmp);
      ref = build1 (ADDR_EXPR, build_pointer_type (tmp), ref);
      TREE_OPERAND (assign, 1) = ref;
    }

  return head;
}

/* Translate an array reference.  The descriptor should be in se->expr.  */
/*GCC ARRAYS*/
void
g95_conv_array_index_ref (g95_se * se, tree pointer, tree * indices, int dimen)
{
  tree array;
  tree tmp;
  tree index;
  tree tmpvar;
  tree indexvar;
  int n;

  array = build1 (INDIRECT_REF, TREE_TYPE (TREE_TYPE (pointer)), pointer);

  tmpvar = NULL_TREE;
  indexvar = NULL_TREE;

  index = integer_zero_node;
  for (n = 0; n < dimen; n++)
    {
      /* index = index + stride[n]*index[n] */
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

  assert (TREE_CODE (TREE_TYPE (array)) == ARRAY_TYPE);
  tmp = build (ARRAY_REF, TREE_TYPE( TREE_TYPE (array)), array, index);

  /* Check we've used the correct number of dimensions.  */
  assert (TREE_CODE (TREE_TYPE (tmp)) != ARRAY_TYPE);

  se->expr = tmp;
}

/* Generate code to repack an array.  */
/*GCC ARRAYS*/
static void
g95_trans_repack_array (tree * phead, tree * ptail, tree dest, tree src,
                       int dimen)
{
  int n;
  tree loopvar[G95_MAX_DIMENSIONS];
  tree from[G95_MAX_DIMENSIONS];
  tree to[G95_MAX_DIMENSIONS];
  g95_se lse;
  g95_se rse;
  tree ldata;
  tree rdata;
  tree tmp;
  tree field;
  tree stmt;
  tree cond;
  tree init;
  tree inc;

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
  /* Get the data pointers.  */
  field = g95_get_base_component (TREE_TYPE (dest));
  ldata = build (COMPONENT_REF, TREE_TYPE (field), dest, field);
  ldata = g95_simple_fold (ldata, phead, ptail, NULL);
  field = g95_get_base_component (TREE_TYPE (src));
  rdata = build (COMPONENT_REF, TREE_TYPE (field), src, field);
  rdata = g95_simple_fold (rdata, phead, ptail, NULL);

  for (n = 0; n < dimen; n++)
    {
      loopvar[n] = g95_create_tmp_var (g95_array_index_type);
      from[n] = g95_conv_array_lbound (dest, n);
      to[n] = g95_conv_array_ubound (dest, n);
      to[n] = g95_simple_fold (to[n], phead, ptail, NULL);
    }

  /* The loop body.  */
  g95_start_stmt ();

  g95_init_se (&lse, NULL);
  g95_init_se (&rse, NULL);
  lse.expr = dest;
  rse.expr = src;

  g95_conv_array_index_ref (&lse, ldata, loopvar, dimen);
  g95_conv_array_index_ref (&rse, rdata, loopvar, dimen);

  tmp = build (MODIFY_EXPR, TREE_TYPE (lse.expr), lse.expr, rse.expr);
  stmt = build_stmt (EXPR_STMT, tmp);

  /* Piece the loop body together.  */
  g95_add_stmt_to_pre (&lse, rse.pre, rse.pre_tail);
  g95_add_stmt_to_pre (&lse, stmt, stmt);
  g95_add_stmt_to_pre (&lse, rse.post, rse.post_tail);
  g95_add_stmt_to_pre (&lse, lse.post, lse.post_tail);

  stmt = g95_finish_stmt (lse.pre, lse.pre_tail);

  /* Now construct the loops.  */
  for (n = 0; n < dimen; n++)
    {
      init = build (MODIFY_EXPR, TREE_TYPE (loopvar[n]), loopvar[n], from[n]);
      init = build_stmt (EXPR_STMT, init);
      cond = build (LE_EXPR, boolean_type_node, loopvar[n], to[n]);
      inc = build (PLUS_EXPR, TREE_TYPE (loopvar[n]), loopvar[n],
                  integer_one_node);
      inc = build (MODIFY_EXPR, TREE_TYPE (inc), loopvar[n], inc);

      stmt = build_stmt (FOR_STMT, init, cond, inc, stmt);
    }

  g95_add_stmt_to_list (phead, ptail, stmt, NULL_TREE);
}

/* Modify the descriptor of an array parameter so that it has the
   correct lower bound.  Also move the upper bound accordingly.
   If the array is not packed, it will be copied into a temporary.  */
/* TODO: Check that the bounds match for AS_EXPLICIT parameters.  */
/*GCC ARRAYS*/
tree
g95_trans_dummy_array_bias (g95_symbol * sym, tree body)
{
  tree type;
  tree tmpdesc;
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
  int checkparm;

  /* Descriptor type.  */
  type = TREE_TYPE (sym->backend_decl);
  assert (G95_DESCRIPTOR_TYPE_P (type));

  /* The descriptor used in the function.  */
  tmpdesc = sym->backend_decl;
  /* The actual argument descriptor.  */
  dumdesc = build1 (INDIRECT_REF, type, G95_DECL_SAVED_DESCRIPTOR (tmpdesc));

  head = tail = NULL_TREE;

  checkparm = sym->as->type == AS_EXPLICIT
              && g95_option.check_array_bounds;

  if (g95_option.no_repack_arrays)
    {
      repack = 0;
      unpack = NULL_TREE;
    }
  else
    {
      repack = 1;
      unpack = g95_create_tmp_var (boolean_type_node);
    }

  g95_start_stmt ();

  tmpvar = NULL_TREE;
  offset = integer_zero_node;
  offsetvar = NULL_TREE;
  stride = integer_one_node;
  stridevar = NULL_TREE;
  uboundvar = NULL_TREE;
  lboundvar = NULL_TREE;
  eqvar = NULL_TREE;

  if (repack)
    {
      needpack = packedvar = g95_create_tmp_var (boolean_type_node);
    }
  else
    needpack = packedvar = NULL_TREE;

  oldstride = g95_create_tmp_var (g95_array_index_type);

  for (n = 0; n < sym->as->rank; n++)
    {
      /* Lower bound.  */
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

      field = g95_get_lbound_component (type, n);
      ref = build (COMPONENT_REF, TREE_TYPE (field), dumdesc, field);
      ref = g95_simple_fold_tmp (ref, &head, &tail, &lboundvar);

      /* Upper bound.  */
      if (sym->as->upper[n])
        {
          g95_init_se (&se, NULL);
          g95_conv_simple_val_type (&se, sym->as->upper[n],
                                   g95_array_index_type);
          g95_add_stmt_to_list (&head, &tail, se.pre, se.pre_tail);

          /* Check the sizes match.  */
          if (checkparm)
            {
              /* Call __g95_runtine_error.  */
              g95_start_stmt ();

              repack_stmt = pack_tail = NULL_TREE;
              
              tmp = build1 (ADDR_EXPR, pchar_type_node, g95_strconst_bounds);
              tmp = g95_simple_fold (tmp, &repack_stmt, &pack_tail, NULL);
              tmp = g95_chainon_list (NULL_TREE, tmp);
              tmp = g95_build_function_call (g95_fndecl_runtime_error, tmp);
              stmt = build_stmt (EXPR_STMT, tmp);
              g95_add_stmt_to_list (&repack_stmt, &pack_tail, stmt, stmt);

              stmt = g95_finish_stmt (repack_stmt, pack_tail);

              /* Check (ubound(a) - lbound(a) == ubound(b) - lbound(b)).
                       (ubound(a) - lbound(a) + lbound(b) == ubound(b)).  */

              tmp = build (MINUS_EXPR, g95_array_index_type, se.expr,
                          lbound[n]);
              tmp = g95_simple_fold (tmp, &head, &tail, &tmpvar);
              tmp = build (PLUS_EXPR, g95_array_index_type, tmp, ref);
              tmp = g95_simple_fold (tmp, &head, &tail, &tmpvar);

              tmp = build (NE_EXPR, boolean_type_node, tmp, ubound);

              stmt = build_stmt (IF_STMT, tmp, stmt, NULL_TREE);
              g95_add_stmt_to_list (&head, &tail, stmt, stmt);
            }
          ubound = se.expr;
        }
      else
        {
          /* Move the upper bound by the same amount as the lower bound.  */
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


      /* This checks the stride and sets tmp to the value to be stored as
         the new stride.  */
      if (repack)
        {
          /* We save the packed strides, they may be needed later.  */
          strides[n] = stride;
          stridevar = NULL_TREE;

          if (n == 0)
            {
              tree stridetmp;

              /* A stride of 0 means were passed a disposable temporary.
                 Otherwise we were passed an array section, so must
                 copy the result back.  */
              /* unpack has function scope.  */
              tmp = build (NE_EXPR, boolean_type_node, oldstride,
                          integer_zero_node);
              tmp = build (MODIFY_EXPR, TREE_TYPE (unpack), unpack, tmp);
              stmt = build_stmt (EXPR_STMT, tmp);
              g95_add_stmt_to_list (&head, &tail, stmt, stmt);

              /* A variable to hold the stride.  */
              stridetmp = g95_create_tmp_var (g95_array_index_type);
              /* If we are passed a temporary.  */
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

              /* If we are passed an array section.  */
              g95_start_stmt ();

              repack_stmt = pack_tail = NULL_TREE;

              /* Test if the array is already packed.  */
              needpack = build (NE_EXPR, boolean_type_node, stride, oldstride);
              needpack = g95_simple_fold_tmp (needpack, &repack_stmt,
                                             &pack_tail, &packedvar);

              repack_stmt = g95_finish_stmt (repack_stmt, pack_tail);

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
          tmp = build (MODIFY_EXPR, g95_array_index_type, oldstride,
                      integer_one_node);
          stmt = build_stmt (EXPR_STMT, tmp);

          tmp = build (EQ_EXPR, g95_array_index_type, oldstride,
                      integer_zero_node);
          stmt = build_stmt (IF_STMT, tmp, stmt, NULL_TREE);
          g95_add_stmt_to_list (&head, &tail, stmt, stmt);
        }

      /* Store the stride.  May be overwritten if array is repacked.  */
      ref = build (COMPONENT_REF, TREE_TYPE (field), tmpdesc, field);
      ref = build (MODIFY_EXPR, TREE_TYPE (ref), ref, oldstride);
      stmt = build_stmt (EXPR_STMT, ref);
      g95_add_stmt_to_list (&head, &tail, stmt, stmt);

      /* Calculate the offset.  */
      tmp = build (MULT_EXPR, g95_array_index_type, oldstride, lbound[n]);
      tmp = g95_simple_fold (tmp, &head, &tail, &tmpvar);

      offset = build (MINUS_EXPR, TREE_TYPE (tmp), offset, tmp);
      offset = g95_simple_fold_tmp (offset, &head, &tail, &offsetvar);

      /* Calculate the next stride.  */
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
      /* We will do this if the array needs repacking.  */
      g95_start_stmt ();

      repack_stmt = pack_tail = NULL_TREE;

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

      /* Calculate the base pointer.  */
      field = g95_get_data_component (type);
      tmp = build (COMPONENT_REF, TREE_TYPE (field), dumdesc, field);
      tmp = g95_simple_fold (tmp, &repack_stmt, &pack_tail, NULL);
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

      /* Calculate the base component.  */
      field = g95_get_data_component (type);
      tmp = build (COMPONENT_REF, TREE_TYPE (field), tmpdesc, field);
      tmp = g95_simple_fold (tmp, &nopack_stmt, &pack_tail, NULL);
      tmp = build1 (INDIRECT_REF, TREE_TYPE (TREE_TYPE (tmp)), tmp);
      tmp = build (ARRAY_REF, TREE_TYPE (TREE_TYPE (tmp)), tmp, packoffset);
      tmp = build1 (ADDR_EXPR, TREE_TYPE (field), tmp);

      field = g95_get_base_component (type);
      ref = build (COMPONENT_REF, TREE_TYPE (field), tmpdesc, field);
      tmp = build (MODIFY_EXPR, TREE_TYPE (ref), ref, tmp);
      stmt = build_stmt (EXPR_STMT, tmp);
      g95_add_stmt_to_list (&nopack_stmt, &pack_tail, stmt, stmt);

      g95_trans_repack_array (&repack_stmt, &pack_tail, tmpdesc,
                                        dumdesc, sym->as->rank);

      repack_stmt = g95_finish_stmt (repack_stmt, pack_tail);

      g95_start_stmt ();
    }

  /* This will be done if the array does not need repacking.  */
  /* Set the data pointer.  */
  nopack_stmt = pack_tail = NULL_TREE;
  field = g95_get_data_component (type);
  tmp = build (COMPONENT_REF, TREE_TYPE (field), dumdesc, field);
  ref = build (COMPONENT_REF, TREE_TYPE (field), tmpdesc, field);
  tmp = build (MODIFY_EXPR, TREE_TYPE (ref), ref, tmp);
  stmt = build_stmt (EXPR_STMT, tmp);
  g95_add_stmt_to_list (&nopack_stmt, &pack_tail, stmt, stmt);

  /* Calculate the base pointer.  */
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

      stmt = build_stmt (IF_STMT, needpack, repack_stmt, nopack_stmt);

      g95_add_stmt_to_list (&head, &tail, stmt, stmt);
    }
  else
    {
      g95_add_stmt_to_list (&head, &tail, nopack_stmt, pack_tail);
    }

  stmt = g95_finish_stmt (head, tail);

  /* Add to start of function body.  */
  body = chainon (stmt, body);

  if (repack)
    {
      /* Build code to unpack the array.  */
      g95_start_stmt ();
      head = tail = NULL_TREE;

      g95_trans_repack_array (&head, &tail, dumdesc, tmpdesc, sym->as->rank);

      stmt = g95_finish_stmt (head, tail);

      /* If unpack is set, we must copy the results of the array back to
         the original.  */
      stmt = build_stmt (IF_STMT, unpack, stmt, NULL_TREE);

      /* Add to end of function body.  */
      body = chainon (body, stmt);
    }
  return body;
}

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

  /* Make sure the frontend gets these right.  */
  if (! (sym->attr.pointer || sym->attr.allocatable))
    fatal_error ("Possible frontend bug: Deferred array size without pointer or allocatable attribute.");

  /* Parameter variables don't need anything special.  */
  if (sym->attr.dummy)
    return body;

  if (sym->attr.save)
    g95_todo_error ("save attribute on deferred size arrays");

  /* Nullify array variables.  */
  /* Descriptor type.  */
  descriptor = sym->backend_decl;
  type = TREE_TYPE (sym->backend_decl);
  assert (TREE_CODE (type) == RECORD_TYPE);

  /* NULLIFY the data pointer.  */
  field = g95_get_data_component (type);
  tmp = build (COMPONENT_REF, TREE_TYPE(field), descriptor, field);
  tmp = build (MODIFY_EXPR, TREE_TYPE(tmp), tmp, integer_zero_node);
  stmt = build_stmt (EXPR_STMT, tmp);
  body = chainon (stmt, body);

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
   a scalar expression.  The caller should then take whatever action is
   neccessary.

   TODO:Extension - multiple component subscripts.
    x(:,:) = foo%a(:)%b(:)
   Transforms to
    forall (i=..., j=...)
      x(i,j) = foo%a(j)%b(i)
    end forall
   This adds a fair amout of complexity because you need to deal with more
   than one ref.  */

static g95_ss *
g95_walk_variable_expr (g95_ss * ss, g95_expr * expr)
{
  g95_ref *ref;
  g95_array_ref *ar;
  g95_ss *newss;
  g95_ss *head;
  g95_ss *tmp;
  g95_ss *last;
  int n;

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
          newss->expr = expr;
          newss->next = ss;
          newss->dimen = ar->as->rank;
          newss->data.info.ref = ref;

          /* Make sure array is the same as array(:,:), this way
             we don't need to special case all the time.  */
          ar->dimen = ar->as->rank;
          for (n = 0 ; n < newss->dimen ; n++)
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
          newss->expr = expr;
          newss->next = ss;
          newss->dimen = 0;
          newss->data.info.ref = ref;

          head = newss;

          for (n = 0 ; n < ar->dimen ; n++)
            {
              g95_ss *indexss;

              switch (ar->dimen_type[n])
                {
                case DIMEN_ELEMENT:
                  indexss = g95_get_ss();
                  indexss->dimen = 0;
                  indexss->expr = ar->start[n];
                  indexss->next = head;
                  head = indexss;

                  g95_init_se (&head->data.se, NULL);
                  g95_conv_simple_val_type (&head->data.se, head->expr,
                                           g95_array_index_type);
                  break;

                case DIMEN_RANGE:
                  newss->data.info.dim[newss->dimen] = n;
                  newss->dimen++;
                  break;

                case DIMEN_VECTOR:
                  indexss = g95_walk_expr (g95_ss_terminator, ar->start[n]);
                  if (indexss == g95_ss_terminator)
                    internal_error("scalar vector reference???");

                  for (tmp = indexss, last = NULL;
                       tmp != g95_ss_terminator;
                       tmp = tmp->next)
                    {
                      if (tmp->dimen != 0)
                        {
                          assert (tmp->dimen == 1);

                          if (last)
                            {
                              last->next = tmp->next;
                              tmp->next = indexss;
                              indexss = tmp;
                            }
                          break;
                        }
                      last = tmp;
                    }
                  if (tmp == g95_ss_terminator)
                    abort();
                  assert (indexss->dimen == 1);
                  indexss->dimen = -1;
                  newss->data.info.vector[newss->dimen] = indexss;
                  newss->data.info.dim[newss->dimen] = n;
                  newss->dimen++;
                  break;

                default:
                  /* We should know what sort of section it is by now.  */
                  abort ();
                }
            }
          assert (newss->dimen > 0);
          return head;
        break;

        default:
          /* We should know what sort of section it is by now.  */
          abort();
        }

    }
  return ss;
}

static g95_ss *
g95_walk_op_expr (g95_ss * ss, g95_expr *expr)
{
  g95_ss *head;
  g95_ss *head2;
  g95_expr *op;
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
  /* Create a g95_ss to hold the scalar expression.  */
  newss = g95_get_ss ();
  if (head == ss)
    {
      /* First operand is scalar.  */
      head = head2;
      while (head && head->next != ss)
        head = head->next;
      /* Check we haven't somehow broken the chain.  */
      assert (head);
      newss->next = ss;
      head->next = newss;
      op = expr->op1;
    }
  else /* head2 == head */
    {
      assert (head2 == head);
      /* Second operand is scalar.  */
      newss->next = head2;
      head2 = newss;
      op = expr->op2;
    }
  /* Mainly for consistancy checking.  */
  newss->expr = op;

  /* Translate the code for the static expression.  */
  g95_init_se (&newss->data.se, NULL);
  g95_conv_simple_val (&newss->data.se, op);

  return head2;
}

static g95_ss *
g95_walk_function_expr (g95_ss * ss, g95_expr *expr)
{
  g95_actual_arglist *arg;
  g95_ss *argss;

  /* These should be evaluated outside the loop, and the stored result used
     inside the loop.  */
  if (expr->symbol->attr.dimension)
    g95_todo_error ("function returning array during walk");

  /* We need to walk all the parameters for an elemental function.  */
  if (expr->symbol->attr.elemental)
    {
      for (arg = expr->value.function.actual ; arg != NULL ; arg = arg->next)
        {
          argss = g95_walk_expr (ss, arg->expr);
          if (argss == ss)
            {
              /* Scalar argument.  */
              argss = g95_get_ss ();
              argss->dimen = 0;
              argss->expr = arg->expr;
              argss->next = ss;
              g95_conv_simple_reference (&argss->data.se, arg->expr);
            }
          ss = argss;
        }
      return ss;
    }

  /* Scalar functions are OK as these are evaluated outside the scalarisation
     loop.  Pass back and let the caller deal with it.  */
  return ss;
}

static g95_ss *
g95_walk_array_constructor (g95_ss * ss ATTRIBUTE_UNUSED, g95_expr *expr ATTRIBUTE_UNUSED)
{
  g95_todo_error ("array constructors");
}

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

