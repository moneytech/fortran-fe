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

/* The contents of this structure aren't actualy used, just the address.  */
static g95_ss g95_ss_terminator_var;
g95_ss* g95_ss_terminator = &g95_ss_terminator_var;

/* Fills in an array descriptor, and returns the size of the array.  The size
   will be a simple_val, ie a variable or a constant.  Also gets the offset
   for Type B arrays.  */
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
  /* Only Type B arrays use offset.  */
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

      if (! g95_use_gcc_arrays)
        {
          /* Work out the offset for this component.  */
          tmp = build (MULT_EXPR, g95_array_index_type, se.expr, stride);
          tmp = g95_simple_fold (tmp, phead, ptail, &tmpvar);

          offset = build (PLUS_EXPR, g95_array_index_type, offset, tmp);
          offset = g95_simple_fold (offset, phead, ptail, &offsetvar);
        }

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
      tmp = build (MODIFY_EXPR, TREE_TYPE (tmp), tmp,
                    g95_use_gcc_arrays ? integer_zero_node : stride);
      stmt = build_stmt (EXPR_STMT, tmp);
      g95_add_stmt_to_list (phead, ptail, stmt, stmt);

      /* size = ubound + 1 - lbound.  */
      size = build (PLUS_EXPR, g95_array_index_type, se.expr, size);
      size = g95_simple_fold (size, phead, ptail, &tmpvar);

      /* Multiply the stride by the number of elements in this dimension.  */
      stride = build (MULT_EXPR, g95_array_index_type, stride, size);
      stride = g95_simple_fold (stride, phead, ptail, &stridevar);
    }

  if (! g95_use_gcc_arrays)
    {
      /* We need to negate the offset.  */
      offset = build1 (NEGATE_EXPR, g95_array_index_type, offset);
      offset = g95_simple_fold (offset, phead, ptail, &offsetvar);
    }

  /* The stride is the number of elements in the array, so multiply by the
     size of an element to get the total size.  */
  tmp = TYPE_SIZE_UNIT (g95_get_element_type (type));
  stride = build (MULT_EXPR, g95_array_index_type, stride, tmp);
  stride = g95_simple_fold (stride, phead, ptail, &stridevar);

  if (poffset != NULL && ! g95_use_gcc_arrays)
    {
      *poffset = offset;
    }
  else
    assert (g95_use_gcc_arrays);

  return stride;
}

/* Initialises the descriptor and generates a call to __g95_allocate.  */
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

  if (! g95_use_gcc_arrays)
    {
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
}

/* Deallocate an array variable.  Also called when an allocated variable goes
   out of scope.  Does not create a new scope for the temporary or wrap in
   a COMPOUND_STMT as this can be done more effectively by the caller.  */
tree
g95_array_deallocate (tree descriptor)
{
  tree var;
  tree tmp;
  tree field;
  tree stmt;

  field = g95_get_data_component (TREE_TYPE (descriptor));

  /* TODO: check array type? */
  tmp = build (COMPONENT_REF, TREE_TYPE (field), descriptor, field);
  tmp = build1 (ADDR_EXPR, build_pointer_type (TREE_TYPE (field)), tmp);
  var = g95_create_tmp_var (TREE_TYPE (tmp));
  tmp = build (MODIFY_EXPR, TREE_TYPE (tmp), var, tmp);
  stmt = build_stmt (EXPR_STMT, tmp);

  /* Parameter is address of the block/data component.  */
  tmp = tree_cons (NULL_TREE, var, NULL_TREE);
  tmp = g95_build_function_call (g95_fndecl_deallocate, tmp);
  stmt = chainon (stmt, build_stmt (EXPR_STMT, tmp));

  /* We still need to NULLIFY the data pointer for Type B arrays.  This is
     done by __g95_free for Type A arrays.  */
  if (! g95_use_gcc_arrays)
    {
      field = g95_get_base_component (TREE_TYPE (descriptor));
      tmp = build (COMPONENT_REF, TREE_TYPE (field), descriptor, field);
      tmp = build (MODIFY_EXPR, TREE_TYPE (tmp), tmp, integer_zero_node);
      stmt = chainon (stmt, build_stmt (EXPR_STMT, tmp));
    }

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
    #ifndef g95_use_gcc_arrays
          array.data = &array.block[-sum(array.lbound[]*array.stride[])]
    #endif
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

  if (INTEGER_CST_P (size)
      && (g95_option.max_stack_var_size < 0
        || (TREE_INT_CST_HIGH (size) == 0
          && TREE_INT_CST_LOW (size) <=
            (unsigned HOST_WIDE_INT)g95_option.max_stack_var_size)))
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
      tmp = tree_cons (NULL_TREE, size, NULL_TREE);
      tmp = tree_cons (NULL_TREE, pointer, tmp);
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

  /* For Type B arrays we need to bias the data pointer.  */
  if (! g95_use_gcc_arrays)
    {
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
    }

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

/* Modify the descriptor of an array parameter so that it has the
   correct lower bound.  Also move the upper bound accordingly
   Bias the data pointer for Type B arrays.  */
/* TODO: Check that the bounds match for AS_EXPLICIT parameters.  */
tree
g95_trans_dummy_array_bias (g95_symbol * sym, tree body)
{
  tree stmt;
  tree saved;
  tree type;
  tree descriptor;
  tree tmp;
  tree offset;
  tree num;
  tree delta;
  tree head;
  tree tail;
  tree field;
  tree saved_field;
  tree dest;
  tree offsetvar;
  g95_se lbound;
  int n;

  assert (TREE_CODE (TREE_TYPE (sym->backend_decl)) = REFERENCE_TYPE);
  /* Descriptor type.  */
  type = TREE_TYPE (TREE_TYPE (sym->backend_decl));
  assert (TREE_CODE (type) == RECORD_TYPE);

  saved = g95_create_tmp_var (g95_get_descriptorsave_type (sym->as->rank));
  saved_field = TYPE_FIELDS (TREE_TYPE (saved));
  descriptor = build1 (INDIRECT_REF, type, sym->backend_decl);

  /* Wrap up the initialisation in its own scope.  Note that saved must be
     created outside this block so that it has function scope.  */
  g95_start_stmt ();

  head = tail = NULL_TREE;

  num = g95_create_tmp_var (g95_array_index_type);
  delta = g95_create_tmp_var (g95_array_index_type);
  if (! g95_use_gcc_arrays)
    offset = g95_create_tmp_var (g95_array_index_type);
  else
    offset = NULL_TREE;

  /* First check that it's the correct type of descriptor.  If we will  call
     __g95_array_mismatch () which throws a runtime error.  */
  /* num = descriptor.stride00 */
  field = g95_get_stride_component(type, 0);
  tmp = build (COMPONENT_REF, g95_array_index_type, descriptor, field);
  tmp = build (MODIFY_EXPR, g95_array_index_type, num, tmp);
  ADD_EXPR_STMT (tmp);

  tmp = g95_build_function_call (g95_fndecl_array_mismatch, NULL_TREE);
  stmt = build_stmt (EXPR_STMT, tmp);

  tmp = build (NE_EXPR, boolean_type_node, num,
                g95_use_gcc_arrays ? integer_zero_node : integer_one_node);
  stmt = build_stmt (IF_STMT, tmp, stmt, NULL_TREE);
  g95_add_stmt_to_list (&head, &tail, stmt, stmt);

  if (! g95_use_gcc_arrays)
    {
      /* Save the base data pointer.  */
      field = g95_get_base_component (type);
      assert (TREE_CODE (TREE_TYPE (saved_field)) == POINTER_TYPE);

      tmp = build (COMPONENT_REF, TREE_TYPE (field), descriptor, field);
      dest = build (COMPONENT_REF, TREE_TYPE (field), saved, saved_field);
      tmp = build (MODIFY_EXPR, TREE_TYPE (field), dest, tmp);
      ADD_EXPR_STMT (tmp);

      saved_field = TREE_CHAIN (saved_field);
    }

  offsetvar = NULL_TREE;
  for (n = 0; n < sym->as->rank; n++)
    {
      g95_init_se (&lbound, NULL);

      g95_conv_simple_val_type (&lbound, sym->as->lower[n],
                               g95_array_index_type);
      g95_make_safe_expr (&lbound);
      g95_add_stmt_to_list (&head, &tail, lbound.pre, lbound.pre_tail);

      /* Get passed lower bound.  */
      field = g95_get_lbound_component (type, n);
      tmp = build (COMPONENT_REF, g95_array_index_type, descriptor, field);
      tmp = build (MODIFY_EXPR, g95_array_index_type, num, tmp);
      ADD_EXPR_STMT (tmp);

      /* How much do we need to change the bound, store in delta.  */
      tmp = build (MINUS_EXPR, g95_array_index_type, lbound.expr, num);
      tmp = build (MODIFY_EXPR, g95_array_index_type, delta, tmp);
      ADD_EXPR_STMT (tmp);

      /* Save this.  */
      tmp = build (COMPONENT_REF, g95_array_index_type, saved, saved_field);
      tmp = build (MODIFY_EXPR, g95_array_index_type, tmp, delta);
      ADD_EXPR_STMT (tmp);

      saved_field = TREE_CHAIN (saved_field);

      /* Store the new bound.  */
      field = g95_get_lbound_component (type, n);
      tmp = build (COMPONENT_REF, g95_array_index_type, descriptor, field);
      tmp = build (MODIFY_EXPR, g95_array_index_type, tmp, lbound.expr);
      ADD_EXPR_STMT (tmp);

      /* Get the upper bound.  */
      field = g95_get_ubound_component (type, n);
      tmp = build (COMPONENT_REF, g95_array_index_type, descriptor, field);
      tmp = build (MODIFY_EXPR, g95_array_index_type, num, tmp);
      ADD_EXPR_STMT (tmp);

      /* Adjust and store back.  */
      tmp = build (PLUS_EXPR, g95_array_index_type, num, delta);
      dest = build (COMPONENT_REF, g95_array_index_type, descriptor, field);
      tmp = build (MODIFY_EXPR, g95_array_index_type, dest, tmp);
      ADD_EXPR_STMT (tmp);


      /* For Type A arrays data is correct.  For Type B arrays we need to bias
         the data pointer.  */
      if (! g95_use_gcc_arrays)
        {
          /* Stride == 1 for first dimension.  */
          if (n > 0)
            {
              /* Get the stide.  */
              field = g95_get_stride_component (type, n);
              tmp = build (COMPONENT_REF, g95_array_index_type,
                            descriptor, field);
              tmp = build (MODIFY_EXPR, g95_array_index_type, num, tmp);
              ADD_EXPR_STMT (tmp);

              /* Multiply by delta */
              tmp = build (MULT_EXPR, g95_array_index_type, delta, num);
              tmp = build (MODIFY_EXPR, g95_array_index_type, num, tmp);
              ADD_EXPR_STMT (tmp);

              /* Add to total offset.  */
              tmp = build (PLUS_EXPR, g95_array_index_type, offset, num);
            }
          else
            tmp = delta;
          /* Store offset.  */
          offset = g95_simple_fold(tmp, &head, &tail, &offsetvar);
        }
    }

  if (! g95_use_gcc_arrays)
    {
      tree pointer;

      /* Get the data component.  */
      field = g95_get_base_component (type);
      pointer = g95_create_tmp_var (TREE_TYPE (field));

      tmp = build (COMPONENT_REF, TREE_TYPE (field), descriptor, field);
      tmp = build (MODIFY_EXPR, TREE_TYPE (field), pointer, tmp);
      ADD_EXPR_STMT (tmp);

      /* array->base = &array->base[offset] */
      tmp = build1 (INDIRECT_REF, TREE_TYPE (TREE_TYPE (pointer)), pointer);
      tmp = build (ARRAY_REF, TREE_TYPE (TREE_TYPE (tmp)), tmp, offset);
      tmp = build1 (ADDR_EXPR, TREE_TYPE (pointer), tmp);
      dest = build (COMPONENT_REF, TREE_TYPE (field), descriptor, field);
      tmp = build (MODIFY_EXPR, TREE_TYPE (field), dest, tmp);
      ADD_EXPR_STMT (tmp);
    }

  /* Add code to start of function.  */
  head = g95_finish_stmt (head, tail);
  body = chainon (head, body);

  /* Cleanup.  */
  g95_start_stmt ();

  head = tail = NULL_TREE;
  num = g95_create_tmp_var (g95_array_index_type);
  delta = g95_create_tmp_var (g95_array_index_type);

  saved_field = TYPE_FIELDS (TREE_TYPE (saved));

  if (! g95_use_gcc_arrays)
    {
      /* Restore the data pointer.  */
      field = g95_get_base_component (type);
      dest = build (COMPONENT_REF, TREE_TYPE (field), descriptor, field);
      tmp = build (COMPONENT_REF, TREE_TYPE (field), saved, saved_field);
      tmp = build (MODIFY_EXPR, TREE_TYPE (field), dest, tmp);
      ADD_EXPR_STMT (tmp);

      saved_field = TREE_CHAIN (saved_field);
    }

  /* Restore the bounds.  */
  for (n = 0 ; n < sym->as->rank ; n++)
    {
      tmp = build (COMPONENT_REF, TREE_TYPE (field), saved, saved_field);
      tmp = build (MODIFY_EXPR, TREE_TYPE (field), delta, tmp);
      ADD_EXPR_STMT (tmp);

      /* Get lower bound.  */
      field = g95_get_lbound_component (type, n);
      tmp = build (COMPONENT_REF, TREE_TYPE (field), descriptor, field);
      tmp = build (MODIFY_EXPR, TREE_TYPE (field), num, tmp);
      ADD_EXPR_STMT (tmp);
      /* Adjust and store back.  */
      tmp = build (MINUS_EXPR, TREE_TYPE (field), num, delta);
      dest = build (COMPONENT_REF, TREE_TYPE (field), descriptor, field);
      tmp = build (MODIFY_EXPR, TREE_TYPE (field), dest, tmp);
      ADD_EXPR_STMT (tmp);

      /* Same for upper bound.  */
      field = g95_get_ubound_component (type, n);
      tmp = build (COMPONENT_REF, TREE_TYPE (field), descriptor, field);
      tmp = build (MODIFY_EXPR, TREE_TYPE (field), num, tmp);
      ADD_EXPR_STMT (tmp);
      tmp = build (MINUS_EXPR, TREE_TYPE (field), num, delta);
      dest = build (COMPONENT_REF, TREE_TYPE (field), descriptor, field);
      tmp = build (MODIFY_EXPR, TREE_TYPE (field), dest, tmp);
      ADD_EXPR_STMT (tmp);

      saved_field = TREE_CHAIN (saved_field);
    }

  /* Add cleanup code to end of function.  */
  head = g95_finish_stmt (head, tail);
  body = chainon (body, head);

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
                  g95_todo_error ("vector array subscripts");

                default:
                  /* We should know what sort of section it is by now.  */
                  abort();
                  break;
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
      head = g95_walk_function_expr(ss, expr);
      return head;

    case EXPR_CONSTANT:
    case EXPR_NULL:
    case EXPR_STRUCTURE:
      /* Pass back and let the caller deal with it.  */
      break;

    /* TODO: EXPR_SUBSTRING, EXPR_ARRAY.  */
    default:
      g95_todo_error("expr_type %d during walk", expr->expr_type);
      break;
    }
  return ss;
}

