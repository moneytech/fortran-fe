/* Expression translation
   Copyright (C) 2000, 2001, 2002 Free Software Foundation, Inc.
   Contributed by Paul Brook <paul@nowt.org> and
                  Steven Bosscher <s.bosscher@student.tudelft.nl>

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

/* trans-expr.c-- generate SIMPLE trees for g95_expr.  */

#include "config.h"
#include "system.h"
#include "tree.h"
#include <stdio.h>
#include "c-common.h"
#include "ggc.h"
#include "rtl.h"
#include "toplev.h"
#include "function.h"
#include "expr.h"
#include "real.h"
#include "tree-simple.h"
#include <gmp.h>
#include <assert.h>
#define BACKEND_CODE
#include "g95.h"
#include "trans.h"
#include "trans-const.h"
#include "trans-types.h"
#include "trans-array.h"
/* Only for g95_trans_assign and g95_trans_pointer_assign.  */
#include "trans-stmt.h"

/* Copy the scalarization loop variables.  */
static void
g95_copy_se_loopvars (g95_se * dest, g95_se * src)
{
  int n;

  dest->ss = src->ss;
  for (n = 0; n < G95_MAX_DIMENSIONS; n++)
    {
      dest->loopvar[n] = src->loopvar[n];
    }
}

/* Initialise a simple expression holder.

   Care must be taken when multiple se are created with the same parent.
   The child se must be kept in sync.  The easiest way is to delay creation
   of a child se until after after the previous se has been translated.  */
void
g95_init_se(g95_se * se, g95_se * parent)
{
  memset(se, 0, sizeof(g95_se));

  se->parent = parent;

  if (parent)
    g95_copy_se_loopvars(se, parent);
}

/* Advances to the next SS in the chain.  Use this rather than setting
   se->ss = se->ss->next because all the parent needs to be kept in sync.
   See g95_init_se.  */
void
g95_advance_se_ss_chain (g95_se * se)
{
  g95_se * p;

  assert (se != NULL && se->ss != NULL
            && se->ss != g95_ss_terminator);

  p = se;
  /* Walk down the parent chain.  */
  while (p != NULL)
    {
      /* Simple consistancy check.  */
      assert (p->parent == NULL || p->parent->ss == p->ss);

      p->ss = p->ss->next;

      p = p->parent;
    }
}

/* This probably doesn't work since the push/pop_scope changes.
   Should be safe, just will create a temp in all circumstances.  */
static bool
is_my_tmp_var(g95_se * se)
{
  tree  stmt;

  for (stmt = se->pre; stmt; stmt = TREE_CHAIN(stmt))
    {
      if ((TREE_CODE(stmt) == DECL_STMT) && (DECL_STMT_DECL(stmt) == se->expr))
        return(true);
    }
  return(false);
}

/* Ensures the result of the expression as either a temporary variable
   or a constant so that it can be used repeatedly.  */
void
g95_make_safe_expr(g95_se * se)
{
  tree  tmp;
  tree  stmt;
  /* we shouldn't call this with an empty expr */
  assert ((se != NULL) && (se->expr != NULL_TREE));

  if (is_my_tmp_var (se))
    return;

  if (TREE_CODE_CLASS (TREE_CODE (se->expr)) == 'c')
    return;

  /* we need a temporary for this result */
  tmp = g95_create_tmp_var (TREE_TYPE(se->expr));
  stmt = build_stmt (EXPR_STMT,
              build (MODIFY_EXPR, TREE_TYPE (se->expr), tmp, se->expr));
  g95_add_stmt_to_pre (se, stmt, NULL);
  se->expr = tmp;
}

/* A reference to an array vector subscript.  Uses recursion to handle nested
   vector subscripts.  */
static tree
g95_conv_vector_array_index (g95_se * se, tree index, g95_ss * ss, tree * pvar)
{
  tree descsave;
  tree indices[G95_MAX_DIMENSIONS];
  g95_ss *indexss;
  g95_array_ref *ar;
  int n;

  assert (ss && ss->type == G95_SS_VECTOR);

  /* Save the descriptor.  */
  descsave = se->expr;
  se->expr = ss->data.info.descriptor;

  indexss = ss->next;
  ar = &ss->data.info.ref->u.ar;
  for (n = 0; n < ar->dimen; n++)
   {
     switch (ar->dimen_type[n])
       {
       case DIMEN_ELEMENT:
        assert (indexss != g95_ss_terminator
                && indexss->type == G95_SS_SCALAR);
        indices[n] = indexss->data.scalar;
        indexss = indexss->next;
        break;

      case DIMEN_RANGE:
        indices[n] = index;
        break;

      case DIMEN_VECTOR:
        indices[n] =
          g95_conv_vector_array_index (se, index,
                                      ss->data.info.vector[0], pvar);
        break;

      default:
        abort();
      }
   }
  /* Get the index from the vector.  */
  g95_conv_array_index_ref (se, ss->data.info.data, indices, ar->dimen);
  index = g95_simple_fold (se->expr, &se->pre, &se->pre_tail, pvar);
  /* Put the descriptor back.  */
  se->expr = descsave;

  return index;
}

/* Build an array reference. se->expr already holds the array descriptor.
   This should be either a variable, indirect variable reference or component
   reference.  */
/*GCC ARRAYS*/
static void
g95_conv_array_ref (g95_se * se, g95_array_ref * ar)
{
  tree index;
  tree tmp;
  tree var;
  tree pointer;
  tree stmt;
  tree field;
  tree indices[G95_MAX_DIMENSIONS];
  g95_se indexse;
  g95_ss *ss;
  g95_ss_info *info;
  int n;
  int dim;
  int dimen;

  /* This has already been validated by g95_conv_variable.  */
  ss = se->ss;
  /* This must be done before translating indices.  */
  if (ss != NULL)
    {
      g95_advance_se_ss_chain (se);
      info = &ss->data.info;
    }
  else
    {
      assert (ar->type == AR_ELEMENT);
      info = NULL;
    }

  dimen = ar->dimen;
  dim = 0;
  for (n = 0; n < dimen; n++)
    {
      var = NULL_TREE;
      if (ar->type == AR_ELEMENT)
        {
          g95_init_se (&indexse, se);
          g95_conv_simple_val_type (&indexse, ar->start[n],
                                   g95_array_index_type);
          g95_add_stmt_to_pre (se, indexse.pre, indexse.pre_tail);
          indices[n] = indexse.expr;
        }
      else
        {
          if (ar->dimen_type[n] == DIMEN_ELEMENT)
            {
              assert (se->ss != g95_ss_terminator
                      && se->ss->type == G95_SS_SCALAR);
              /* We've already translated this value outside the loop.  */
              indices[n] = se->ss->data.scalar;

              g95_advance_se_ss_chain (se);
            }
          else
            {
              assert (ss != g95_ss_terminator);
              /* Substutute a scalarizing loop variable.  */
              assert (ss != NULL && dim < ss->data.info.dimen);
              assert (info->dim[dim] == n);

              index = se->loopvar[dim];
              tmp = build (MULT_EXPR, g95_array_index_type,
                            se->loopvar[dim], info->stride[dim]);
              index = g95_simple_fold (tmp, &se->pre, &se->pre_tail, &var);

              tmp = build (PLUS_EXPR, g95_array_index_type,
                            index, info->delta[dim]);
              index = g95_simple_fold (tmp, &se->pre, &se->pre_tail, &var);

              /* Handle vector references.  */
              if (ar->dimen_type[n] == DIMEN_VECTOR)
                {
                  index = g95_conv_vector_array_index (se, index,
                      ss->data.info.vector[dim], &var);
                }
              else
                assert (ar->dimen_type[n] == DIMEN_RANGE);

              indices[n] = index;
              dim++;
            }
        }
    }

  field = g95_get_base_component (TREE_TYPE (se->expr));

  /* Get a pointer to the array data.  */
  if (ss == NULL)
    {
      pointer = g95_create_tmp_var (TREE_TYPE (field));
      tmp = build (COMPONENT_REF, TREE_TYPE (field), se->expr, field);
      tmp = build (MODIFY_EXPR, TREE_TYPE (pointer), pointer, tmp);
      stmt = build_stmt (EXPR_STMT, tmp);
      g95_add_stmt_to_pre(se, stmt, stmt);
    }
  else
    pointer = info->data;

  g95_conv_array_index_ref (se, pointer, indices, dimen);
}

static void
g95_conv_substring (g95_se * se, g95_ref * ref, int kind)
{
  tree tmp;
  tree type;
  tree var;
  g95_se start;
  g95_se end;

  type = g95_get_character_type (kind, ref->u.ss.length);
  if (G95_SIZE_STRING_TYPE_P (type))
    type = build_pointer_type (type);

  var = NULL_TREE;
  g95_init_se (&start, se);
  g95_conv_simple_val_type (&start, ref->u.ss.start, g95_strlen_type_node);
  g95_add_stmt_to_pre (se, start.pre, start.pre_tail);

  if (integer_onep (start.expr))
    {
      g95_conv_string_parameter (se);
    }
  else
    {
      /* Change the start of the string.  */
      if (G95_SIZE_STRING_TYPE_P (TREE_TYPE (se->expr)))
        tmp = se->expr;
      else
        tmp = build1 (INDIRECT_REF, TREE_TYPE (TREE_TYPE (se->expr)), se->expr);
      tmp = build (ARRAY_REF, TREE_TYPE (TREE_TYPE (tmp)), tmp, start.expr);
      tmp = build1 (ADDR_EXPR, type, tmp);
      se->expr = g95_simple_fold (tmp, &se->pre, &se->pre_tail, NULL);
    }

  /* Length = end + 1 - start.  */
  g95_init_se (&end, se);
  g95_conv_simple_val_type (&end, ref->u.ss.end, g95_strlen_type_node);
  g95_add_stmt_to_pre (se, end.pre, end.pre_tail);

  tmp = build (MINUS_EXPR, g95_strlen_type_node, integer_one_node, start.expr);
  tmp = g95_simple_fold (tmp, &se->pre, &se->pre_tail, &var);
  tmp = build (PLUS_EXPR, g95_strlen_type_node, end.expr, tmp);
  se->string_length = g95_simple_fold (tmp, &se->pre, &se->pre_tail, &var);
}

/* Return the contents of a variable. Also handles refrence/pointer
   variables (all Fortran pointer refrences are implicit) */
static void
g95_conv_variable (g95_se * se, g95_expr * expr)
{
  g95_ref *ref;

  if (se->ss != NULL)
    {
      /* Check that something hasn't gone horribly wrong.  */
      assert (se->ss != g95_ss_terminator);
      assert (se->ss->expr == expr);

      if (se->ss->type == G95_SS_SCALAR)
        {
          /* This is a scalar term so we already have a value.  */
          se->expr = se->ss->data.scalar;
          g95_advance_se_ss_chain (se);
          return;
        }
      /* A scalarized term.  We already know the descriptor.  */
      se->expr = se->ss->data.info.descriptor;
      ref = se->ss->data.info.ref;
    }
  else
    {
      se->expr = g95_get_symbol_decl (expr->symbol);

      /* Special case for assigning the return value of a function.
         Self recursive functions must have an explicit return value.  */
      if (se->expr == current_function_decl && expr->symbol->attr.function
            && (expr->symbol->result == expr->symbol))
       {
         se->expr = g95_get_fake_result_decl(expr->symbol);
         assert (expr->ref == NULL || expr->symbol->attr.dimension);
       }

      /* RHS of pointer assignment, or allocation statement.
         Arrays and derived type components are handled seperately.  */
      if (se->want_pointer && expr->ref == NULL)
        {
          assert (expr->symbol->attr.pointer || expr->symbol->attr.allocatable);
          return;
        }

      /* Dereference scalar dummy variables.  */
      if (expr->symbol->attr.dummy
          && ! (G95_DECL_STRING (se->expr)
                || expr->symbol->attr.dimension))
        {
          se->expr = build1 (INDIRECT_REF, TREE_TYPE(TREE_TYPE (se->expr)),
                            se->expr);
        }

      /* Dereference pointer variables.  */
      if ((expr->symbol->attr.pointer || expr->symbol->attr.allocatable)
          && (expr->symbol->attr.dummy
              || expr->symbol->attr.result
              || expr->symbol->attr.function
              || ! expr->symbol->attr.dimension))
        {
          se->expr = build1 (INDIRECT_REF, TREE_TYPE(TREE_TYPE (se->expr)),
                            se->expr);
        }

      /* For character variables, also get the length.  */
      if (expr->symbol->ts.type == BT_CHARACTER)
        {
          assert (G95_DECL_STRING (se->expr));
          se->string_length = G95_DECL_STRING_LENGTH (se->expr);
        }

      ref = expr->ref;
    }

  while (ref)
    {
      switch (ref->type)
        {
        case REF_ARRAY:
          /* Return the descriptor if that's what we want and this is an array
             section reference.  */
          if (se->descriptor_only && ref->u.ar.type != AR_ELEMENT)
            return;
          /* Return the descriptor for array pointers.  */
          if (se->want_pointer && ref->next == NULL)
            return;
          g95_conv_array_ref(se, &ref->u.ar);
          break;

        case REF_COMPONENT:
          g95_todo_error("component ref");
          break;

        case REF_SUBSTRING:
          g95_conv_substring (se, ref, expr->symbol->ts.kind);
          break;

        default:
          abort();
          break;
        }
      ref = ref->next;
    }
}

void
g95_conv_simple_val_type(g95_se * se, g95_expr * expr, tree type)
{
  tree tmp;
  tree assign;

  g95_start_stmt ();
  g95_start_stmt ();

  assert (expr->ts.type != BT_CHARACTER);

  g95_conv_simple_rhs (se, expr);

  if (!is_simple_val (se->expr))
    {
      /* The target variable will be fixed up later.  */
      assign = build (MODIFY_EXPR, TREE_TYPE (se->expr), NULL_TREE, se->expr);
      g95_add_stmt_to_pre (se, build_stmt (EXPR_STMT, assign), NULL);
      g95_add_stmt_to_pre (se, se->post, se->post_tail);
      se->post = se->post_tail = NULL_TREE;

      /* Wrap everything up in its own scope if neccessary.  */
      g95_finish_se_stmt (se);

      /* The result variable must be created outside the new scope.  */
      tmp = g95_create_tmp_var (TREE_TYPE (se->expr));
      TREE_OPERAND (assign, 0) = tmp;
      se->expr = tmp;
    }
  else
    g95_merge_stmt();

  tmp = fold (convert (type, se->expr));
  if (! is_simple_val (tmp))
    {
      assign = build (MODIFY_EXPR, type, NULL_TREE, tmp);
      g95_add_stmt_to_pre (se, build_stmt (EXPR_STMT, assign), NULL);
      g95_add_stmt_to_pre (se, se->post, se->post_tail);
      se->post = se->post_tail = NULL_TREE;

      /* Wrap everything up in its own scope if neccessary.  */
      g95_finish_se_stmt (se);

      /* The result variable must be created outside the new scope.  */
      tmp = g95_create_tmp_var (type);
      TREE_OPERAND (assign, 0) = tmp;
      se->expr = tmp;
    }
  else
    g95_merge_stmt();
}

/* Return an expr which is a single variable/value, suitable for
   function parameters and array indices.  */
void
g95_conv_simple_val(g95_se * se, g95_expr * expr)
{
  tree  tmp;
  tree  assign;
  tree  type;

  g95_start_stmt ();

  g95_conv_simple_rhs (se, expr);

  /* Special handling for character variables.  */
  if (expr->ts.type == BT_CHARACTER)
    {
      g95_conv_string_parameter (se);
      g95_merge_stmt ();
      return;
    }

  if (!is_simple_val (se->expr))
    {
      type = TREE_TYPE (se->expr);
      /* The target variable will be fixed up later.  */
      assign = build (MODIFY_EXPR, type, NULL_TREE, se->expr);
      g95_add_stmt_to_pre (se, build_stmt (EXPR_STMT, assign), NULL);
      g95_add_stmt_to_pre (se, se->post, se->post_tail);
      se->post = se->post_tail = NULL_TREE;

      /* Wrap everything up in its own scope if neccessary.  */
      g95_finish_se_stmt (se);
      /* The result variable must be created outside the new scope.  */
      tmp = g95_create_tmp_var (type);
      TREE_OPERAND (assign, 0) = tmp;
      se->expr = tmp;
    }
  else
    {
      g95_merge_stmt ();
      assert (se->post == NULL_TREE);
    }
}

void
g95_conv_simple_reference (g95_se * se, g95_expr * expr)
{
  assert (! se->ss);

  g95_conv_simple_rhs (se, expr);

  if (expr->ts.type == BT_CHARACTER)
    {
      g95_conv_string_parameter (se);
      return;
    }

  if (TREE_CODE (se->expr) == INDIRECT_REF)
    {
      /* If this is a dereference operation, just take the subject.  */
      se->expr = TREE_OPERAND (se->expr, 0);

      assert (is_simple_id (se->expr));
    }
  else
    {
      tree var;
      tree tmp;
      tree stmt;

      if (is_simple_varname (se->expr))
        {
          /* Pass this variable by reference.  */
          var = se->expr;
        }
      else
        {
          /* Create a temporary to hold the value.  */
          var = g95_create_tmp_var (TREE_TYPE (se->expr));
          TREE_ADDRESSABLE (var) = 1;
          tmp = build (MODIFY_EXPR, TREE_TYPE (var), var, se->expr);
          stmt = build_stmt (EXPR_STMT, tmp);
          g95_add_stmt_to_pre (se, stmt, stmt);
        }

      /* Take the address of this variable.  */
      tmp = build1 (ADDR_EXPR, build_pointer_type (TREE_TYPE (var)), var);
      se->expr = g95_create_tmp_var (TREE_TYPE (tmp));
      tmp = build (MODIFY_EXPR, TREE_TYPE (tmp), se->expr, tmp);
      stmt = build_stmt (EXPR_STMT, tmp);
      g95_add_stmt_to_pre (se, stmt, stmt);

      /* Add the post chain to the pre chain.  */
      g95_add_stmt_to_pre (se, se->post, se->post_tail);
      se->post = se->post_tail = NULL_TREE;
    }
}


/* Returns an lvalue, throws error if not possble.
   Guaranteed to return with se.post == NULL.  */
void
g95_conv_simple_lhs (g95_se * se, g95_expr * expr)
{
  g95_conv_simple_rhs(se, expr);

  assert (is_simple_modify_expr_lhs (se->expr));
  assert (se->post == NULL_TREE);
}

/* Return an expr suitable for a logic test, eg. an if condition.  */
void
g95_conv_simple_cond (g95_se * se, g95_expr * expr)
{
  tree  tmp;
  tree  assign;
  tree  type;

  assert (! se->ss);

  g95_conv_simple_rhs (se, expr);

  if (!is_simple_condexpr (se->expr))
    {
      type = TREE_TYPE (se->expr);
      tmp = g95_create_tmp_var(type);
      assign = build (MODIFY_EXPR, type, tmp, se->expr);
      g95_add_stmt_to_pre(se, build_stmt (EXPR_STMT, assign), NULL);
      se->expr=tmp;
      g95_add_stmt_to_pre (se, se->post, se->post_tail);
      se->post = se->post_tail = NULL_TREE;
    }
  assert (se->post == NULL_TREE);
}

/* Unary ops are easy... Or they would be if ! was a valid op.  */
static void
g95_conv_unary_op (enum tree_code code, g95_se * se, g95_expr * expr)
{
  g95_se operand;
  tree type;

  /* Initialize the operand.  */
  g95_init_se (&operand, se);
  g95_conv_simple_val (&operand, expr->op1);

  type = g95_typenode_for_spec(&expr->ts);

 /* TRUTH_NOT_EXPR is not a "true" unary operator in GCC.
    We must simplify it to a compare to 0 (e.g. EQ_EXPR (op1, 0)).
    All other unary operators have an equivalent SIMPLE unary operator  */
 if (code == TRUTH_NOT_EXPR)
   se->expr = build (EQ_EXPR, type, operand.expr, integer_zero_node);
 else
   se->expr = build1 (code, type, operand.expr);

  /* combine the pre and post stmts */
  g95_add_stmt_to_pre (se, operand.pre, operand.pre_tail);
  g95_add_stmt_to_post (se, operand.post, operand.post_tail);
}

static void
g95_conv_power_op (g95_se * se ATTRIBUTE_UNUSED, g95_expr * expr ATTRIBUTE_UNUSED)
{
  g95_todo_error ("power op");
}

/* Handle a string concatenation operation.  A temporary will be allocated to
   hold the result.  */
static void
g95_conv_concat_op (g95_se * se, g95_expr * expr)
{
  g95_se lse;
  g95_se rse;
  tree len;
  tree type;
  tree var;
  tree args;
  tree tmp;
  tree stmt;

  assert (expr->op1->ts.type == BT_CHARACTER
      && expr->op2->ts.type == BT_CHARACTER);

  g95_init_se (&lse, se);
  g95_conv_simple_val (&lse, expr->op1);
  g95_init_se (&rse, se);
  g95_conv_simple_val (&rse, expr->op2);

  g95_add_stmt_to_pre (se, lse.pre, lse.pre_tail);
  g95_add_stmt_to_pre (se, rse.pre, rse.pre_tail);

  type = g95_get_character_type (expr->ts.kind, expr->ts.cl);
  if (G95_SIZE_STRING_TYPE_P (type))
    tmp = build_pointer_type (type);
  else
    tmp = type;
  var = g95_create_tmp_var (tmp);

  if (G95_SIZE_STRING_TYPE_P (type))
    {
      len = TYPE_MAX_VALUE (TYPE_DOMAIN (type));
    }
  else
    {
      len = build (PLUS_EXPR, TREE_TYPE (lse.string_length), lse.string_length,
                   rse.string_length);
      len = g95_simple_fold (len, &se->pre, &se->pre_tail, NULL);
    }

  if (g95_can_put_var_on_stack (len))
    {
      /* Create a temporary (stack) array to hold the result.  */
      type = g95_get_stack_array_type (len);
      tmp = g95_create_tmp_var (type);
      TREE_ADDRESSABLE (tmp) = 1;
      tmp = build1 (ADDR_EXPR, TREE_TYPE (var), tmp);
      tmp = build (MODIFY_EXPR, TREE_TYPE (var), var, tmp);
      stmt = build_stmt (EXPR_STMT, tmp);
      g95_add_stmt_to_pre (se, stmt, stmt);
    }
  else
    {
      tree addr;

      TREE_ADDRESSABLE (var) = 1;

      /* Allocate a temporary to hold the result.  */
      tmp = build1 (ADDR_EXPR, ppvoid_type_node, var);
      addr = g95_simple_fold (tmp, &se->pre, &se->pre_tail, NULL);

      args = NULL_TREE;
      args = g95_chainon_list (args, addr);
      args = g95_chainon_list (args, len);
      tmp = g95_build_function_call (g95_fndecl_internal_malloc, args);
      stmt = build_stmt (EXPR_STMT, tmp);
      g95_add_stmt_to_pre (se, stmt, stmt);

      /* Free the temporary afterwards.  */
      args = g95_chainon_list (NULL_TREE, addr);
      tmp = g95_build_function_call (g95_fndecl_internal_free, args);
      stmt = build_stmt (EXPR_STMT, tmp);
      g95_add_stmt_to_post (se, stmt, stmt);
    }

  /* Do the actual concatenation.  */
  args = NULL_TREE;
  args = g95_chainon_list (args, len);
  args = g95_chainon_list (args, var);
  args = g95_chainon_list (args, lse.string_length);
  args = g95_chainon_list (args, lse.expr);
  args = g95_chainon_list (args, rse.string_length);
  args = g95_chainon_list (args, rse.expr);
  tmp = g95_build_function_call (g95_fndecl_concat_string, args);
  stmt = build_stmt (EXPR_STMT, tmp);
  g95_add_stmt_to_pre (se, stmt, stmt);

  /* Add the cleanup for the operands.  */
  g95_add_stmt_to_pre (se, rse.post, rse.post_tail);
  g95_add_stmt_to_pre (se, lse.post, lse.post_tail);

  se->expr = var;
  se->string_length = len;
}

/* Translates an op expression. Common (binary) cases are handled by this
   function, others are passed on. Recursion is used in either case.
   We use the fact that (op1.ts == op2.ts) (except for the power
   operand **).
   Operators need no special handling for scalarized expressions as long as
   they call g95_conv_siple_val to get their operands.
   Character strings get special handling.  */
static void
g95_conv_expr_op (g95_se * se, g95_expr * expr)
{
  enum tree_code code;
  g95_se lse;
  g95_se rse;
  tree type;
  int checkstring;

  checkstring = 0;
  switch (expr->operator)
    {
    case INTRINSIC_UPLUS:
      g95_conv_simple_val (se, expr->op1);
      return;

    case INTRINSIC_UMINUS:
      g95_conv_unary_op (NEGATE_EXPR, se, expr);
      return;

    case INTRINSIC_NOT:
      g95_conv_unary_op (TRUTH_NOT_EXPR, se, expr);
      return;

    case INTRINSIC_PLUS:
      code = PLUS_EXPR;
      break;

    case INTRINSIC_MINUS:
      code = MINUS_EXPR;
      break;

    case INTRINSIC_TIMES:
      code = MULT_EXPR;
      break;

    case INTRINSIC_DIVIDE:
      /* If expr is a real or complex expr, use an RDIV_EXPR. If op1 is
         an integer, we must round towards zero, so we use a
         TRUNC_DIV_EXPR.  */
      if (expr->ts.type == BT_INTEGER)
        code = TRUNC_DIV_EXPR;
      else
        code = RDIV_EXPR;
      break;

    case INTRINSIC_POWER:
      g95_conv_power_op (se, expr);
      return;

    case INTRINSIC_CONCAT:
      g95_conv_concat_op (se, expr);
      return;

    case INTRINSIC_AND:
      code = BIT_AND_EXPR;
      break;

    case INTRINSIC_OR:
      code = BIT_IOR_EXPR;
      break;

    /* EQV and NEQV only work on logicals, but since we represent them
       as integers, we can use EQ_EXPR and NE_EXPR for them in SIMPLE.  */
    case INTRINSIC_EQ:
    case INTRINSIC_EQV:
      code = EQ_EXPR;
      checkstring = 1;
      break;

    case INTRINSIC_NE:
    case INTRINSIC_NEQV:
      code = NE_EXPR;
      checkstring = 1;
      break;

    case INTRINSIC_GT:
      code = GT_EXPR;
      checkstring = 1;
      break;

    case INTRINSIC_GE:
      code = GE_EXPR;
      checkstring = 1;
      break;

    case INTRINSIC_LT:
      code = LT_EXPR;
      checkstring = 1;
      break;

    case INTRINSIC_LE:
      code = LE_EXPR;
      checkstring = 1;
      break;

    case INTRINSIC_USER:
      g95_todo_error ("User operatior");
      return;

    case INTRINSIC_ASSIGN:
      g95_todo_error ("intrinsic assignment expr");
      return;

    default:
      fatal_error ("Unknown intrinsic op");
      return;
    }

  /* The only exception to this is **, which is handled seperately anyway.  */
  assert (expr->op1->ts.type == expr->op2->ts.type);

  if (checkstring && expr->op1->ts.type == BT_CHARACTER)
    g95_start_stmt ();
  else
    checkstring = 0;

  /* lhs */
  g95_init_se (&lse, se);
  g95_conv_simple_val (&lse, expr->op1);

  /* rhs */
  g95_init_se (&rse, se);
  g95_conv_simple_val (&rse, expr->op2);

  if (checkstring)
    {
      tree assign;
      tree stmt;
      tree args;

      args = NULL_TREE;
      args = g95_chainon_list (args, lse.string_length);
      args = g95_chainon_list (args, lse.expr);
      args = g95_chainon_list (args, rse.string_length);
      args = g95_chainon_list (args, rse.expr);

      assign = g95_build_function_call (g95_fndecl_compare_string, args);
      assign = build (MODIFY_EXPR, g95_int4_type_node, NULL_TREE, assign);
      stmt = build_stmt (EXPR_STMT, assign);

      g95_add_stmt_to_pre (&lse, rse.pre, rse.pre_tail);
      g95_add_stmt_to_pre (&lse, stmt, stmt);
      g95_add_stmt_to_pre (&lse, rse.post, rse.post_tail);
      g95_add_stmt_to_pre (&lse, lse.post, lse.post_tail);
      lse.post = lse.post_tail = NULL_TREE;
      rse.pre = rse.pre_tail = NULL_TREE;
      rse.post = rse.post_tail = NULL_TREE;

      g95_finish_se_stmt (&lse);

      /* Create the result variable outside the containing scope.  */
      lse.expr = g95_create_tmp_var (g95_int4_type_node);
      TREE_OPERAND (assign, 0) = lse.expr;
      rse.expr = integer_zero_node;
    }

  type = g95_typenode_for_spec (&expr->ts);

  se->expr = build (code, type, lse.expr, rse.expr);

  /* Combine the pre and post stmts.  */
  g95_add_stmt_to_pre (se, lse.pre, lse.pre_tail);
  g95_add_stmt_to_pre (se, rse.pre, rse.pre_tail);
  assert (lse.post == NULL_TREE);
  assert (rse.post == NULL_TREE);
}

static void
g95_conv_function_val (g95_se * se, g95_symbol * sym)
{
  tree tmp;

  if (sym->attr.dummy)
    {
      tmp = g95_get_symbol_decl (sym);
      assert (TREE_CODE (tmp) == POINTER_TYPE
             && TREE_CODE (TREE_TYPE (tmp)) == FUNCTION_DECL);

      se->expr = g95_create_tmp_var (TREE_TYPE (tmp));

      tmp = build (MODIFY_EXPR, TREE_TYPE (tmp), se->expr, tmp);
      tmp = build_stmt (EXPR_STMT, tmp);
      g95_add_stmt_to_pre (se, tmp, tmp);
    }
  else
    {
      /* TODO: contained procedures.  */
      tmp = g95_get_extern_function_decl (sym);

      assert (TREE_CODE (tmp) == FUNCTION_DECL);
      se->expr = build1 (ADDR_EXPR, build_pointer_type (TREE_TYPE (tmp)), tmp);
    }
}

/* Generate code for a procedure call.  Note can return se->post != NULL.  */
void
g95_conv_function_call (g95_se * se, g95_symbol * sym,
                       g95_actual_arglist * arg)
{
  tree arglist;
  tree tmp;
  tree fntype;
  tree stmt;
  g95_se parmse;
  g95_ss *argss;
  g95_ss_info info;

  arglist = NULL_TREE;

  if (se->ss != NULL)
    {
      if (! sym->attr.elemental)
        {
          switch (se->ss->type)
            {
            case G95_SS_SCALAR:
              /* We've already got the return value of this function.  */
              se->expr = se->ss->data.scalar;
              g95_advance_se_ss_chain (se);
              return;

            case G95_SS_FUNCTION:
              assert (g95_return_by_reference (sym));
              assert (se->loop != NULL);

              /* Access the previously obtained result.  */
              g95_conv_tmp_ref (se);
              g95_advance_se_ss_chain (se);
              return;

            default:
              abort();
            }
        }
    }

  if (g95_return_by_reference (sym))
    {
      /* Currently we only return arrays be reference, but we may
         need to do derived types as well.  */
      if (! sym->attr.dimension)
        abort();
      assert (se->loop != NULL);
      /* Set the type of the array.  */
      info.descriptor = g95_typenode_for_spec (&sym->ts);
      /* Allocate a temporary to store the result.  */
      g95_trans_allocate_temp_array (se->loop, &info);

      /* Zero the forst stride to indicate a temporary.  */
      tmp = g95_get_stride_component (TREE_TYPE (info.descriptor), 0);
      tmp = build (COMPONENT_REF, TREE_TYPE (tmp), info.descriptor, tmp);
      tmp = build (MODIFY_EXPR, TREE_TYPE (tmp), tmp, integer_zero_node);
      stmt = build_stmt (EXPR_STMT, tmp);
      g95_add_stmt_to_pre (se, stmt, stmt);
      /* Pass the temporary as the first argument.  */
      tmp = info.descriptor;
      tmp = build1 (ADDR_EXPR, build_pointer_type (TREE_TYPE (tmp)), tmp);
      tmp = g95_simple_fold (tmp, &se->pre, &se->pre_tail, NULL);
      arglist = g95_chainon_list (arglist, tmp);
      /* TODO: Check the function returned the array properly.  Verify the data
         pointer and stride0==1  */
    }

  /* Evaluate the arguments.  */
  for (; arg != NULL; arg = arg->next)
    {
      /* We don't do alternate returns.  */
      assert (arg->expr != NULL);

      g95_init_se (&parmse, se);

      if (se->ss == NULL)
        {
          /* A scalar function.  */
          argss = g95_walk_expr (g95_ss_terminator, arg->expr);

          if (argss == g95_ss_terminator)
            g95_conv_simple_reference (&parmse, arg->expr);
          else
            g95_conv_array_parameter (&parmse, arg->expr, argss);
        }
      else
        {
          /* An elemental function inside a scalarized loop.  */
          g95_conv_simple_val (&parmse, arg->expr);
        }

      g95_add_stmt_to_pre (se, parmse.pre, parmse.pre_tail);
      g95_add_stmt_to_post (se, parmse.post, parmse.post_tail);

      /* Character strings are passed as two paramarers, a length and a
         pointer.  */
      if (parmse.string_length != NULL_TREE)
        arglist = g95_chainon_list (arglist, parmse.string_length);

      arglist = g95_chainon_list (arglist, parmse.expr);
    }

  /* Generate the actual call.  */
  g95_conv_function_val (se, sym);
  fntype =TREE_TYPE (TREE_TYPE (se->expr));
  se->expr = build (CALL_EXPR, TREE_TYPE (fntype), se->expr, arglist);

  if (! sym->attr.pure)
    TREE_SIDE_EFFECTS (se->expr) = 1;

  if (g95_return_by_reference (sym))
    {
      stmt = build_stmt (EXPR_STMT, se->expr);
      g95_add_stmt_to_pre (se, stmt, stmt);
      se->expr = info.descriptor;
      se->data_pointer = info.data;
    }
}

/* Translate a function expression.  */
static void
g95_conv_function_expr (g95_se * se, g95_expr * expr)
{
  g95_symbol sym;
  g95_symbol *psym;

  if (expr->value.function.isym != NULL)
    {
      g95_conv_intrinsic_function (se, expr);
      return;
    }

  psym = expr->value.function.esym;
  if (psym == NULL)
    {
      /* The frontend does not always create a symbol for implicitly declared
         functions, so we create one now.  */
      memset (&sym, 0, sizeof (g95_symbol));

      sym.ts = expr->ts;
      strcpy (sym.name, expr->value.function.name);
      sym.attr.external = 1;
      sym.attr.function = 1;
      sym.attr.proc = PROC_EXTERNAL;
      sym.attr.flavor = FL_PROCEDURE;

      psym = &sym;
    }

  g95_conv_function_call (se, psym, expr->value.function.actual);
}

/* Return a SIMPLE expression suitable for the RHS of an assignment.  */
void
g95_conv_simple_rhs (g95_se * se, g95_expr * expr)
{
  switch (expr->expr_type)
    {
    case EXPR_VARIABLE:
      g95_conv_variable (se, expr);
      break;

    case EXPR_CONSTANT:
      g95_conv_constant (se, expr);
      break;

    case EXPR_OP:
      g95_conv_expr_op (se, expr);
      break;

    case EXPR_FUNCTION:
      g95_conv_function_expr (se, expr);
      break;

    default:
      g95_todo_error ("scalar expr type %d", expr->expr_type);
      break;
    }
}

tree
g95_trans_pointer_assign (g95_code * code ATTRIBUTE_UNUSED)
{
  g95_internal_error ("pointer assignment not implemented");
}

/* Resolve array data dependancies.  This will eventualy do dependancy analysis
   and maybe loop shifting, etc.  Currently it allocates a temporary if
   there are any possible dependancies.  */
static void
g95_conv_resolve_dependencies (g95_loopinfo * loop, g95_ss * dest,
                              g95_ss * rss)
{
  g95_ss *ss;
  int need_temp;

  loop->temp_ss = NULL;
  need_temp = 0;
  for (ss = rss; ss != g95_ss_terminator; ss = ss->next)
    {
      if (ss->type != G95_SS_SECTION)
        continue;
      if (ss->expr->symbol == dest->expr->symbol)
        need_temp = 1;
    }

  if (need_temp)
    {
      loop->temp_ss = g95_get_ss();
      loop->temp_ss->type = G95_SS_SECTION;
      loop->temp_ss->data.info.descriptor =
        g95_get_element_type (TREE_TYPE (dest->data.info.descriptor));
      loop->temp_ss->data.info.dimen = loop->dimen;
      loop->temp_ss->next = g95_ss_terminator;
    }
  else
    loop->temp_ss = NULL;
}

/* Makes sure se is suitable for passing as a function string parameter.  */
void
g95_conv_string_parameter (g95_se *se)
{
  if (TREE_CODE (se->expr) == STRING_CST)
    {
      se->expr = build1 (ADDR_EXPR, pchar_type_node, se->expr);
      return;
    }

  se->string_length =
    g95_simple_fold (se->string_length, &se->pre, &se->pre_tail, NULL);
  if (G95_SIZE_STRING_TYPE_P (TREE_TYPE (se->expr)))
    se->expr = build1 (ADDR_EXPR, pchar_type_node, se->expr);

  assert (TREE_CODE (TREE_TYPE (se->expr)) == POINTER_TYPE);

  se->expr = g95_simple_fold (se->expr, &se->pre, &se->pre_tail, NULL);
}

/* Translate a string assignment.  */
static tree
g95_trans_string_assign (g95_se * lse, g95_se * rse)
{
  tree tmp;
  tree args;

  args = NULL_TREE;

  assert (lse->string_length != NULL_TREE && rse->string_length != NULL_TREE);

  g95_conv_string_parameter (lse);
  g95_conv_string_parameter (rse);

  args = g95_chainon_list (args, lse->string_length);
  args = g95_chainon_list (args, lse->expr);
  args = g95_chainon_list (args, rse->string_length);
  args = g95_chainon_list (args, rse->expr);

  tmp = g95_build_function_call (g95_fndecl_copy_string, args);
  tmp = build_stmt (EXPR_STMT, tmp);

  return tmp;
}

/* Translate an assignment statement.  Most of the code is concerned with
   setting up the scalarizer.  */
tree
g95_trans_assign (g95_code * code)
{
  g95_se lse;
  g95_se rse;
  g95_ss *lss;
  g95_ss *lss_tail;
  g95_ss *lss_section;
  g95_ss *rss;
  g95_loopinfo loop;
  tree assign;
  tree tmp;
  tree body;

  /* Assignment of the form lhs = rhs.  */
  g95_start_stmt ();

  g95_init_se (&lse, NULL);
  g95_init_se (&rse, NULL);

  /* Walk the lhs.  */
  lss = g95_walk_expr (g95_ss_terminator, code->expr);
  rss = NULL;
  if (lss != g95_ss_terminator)
    {
      /* The assignment needs scalarization.  */
      lss_section = lss;

      if (code->expr->ts.type == BT_CHARACTER)
        g95_todo_error ("Character array assignments");

      /* Find a non-scalar SS from the lhs.  */
      while (lss_section != g95_ss_terminator
             && lss_section->type != G95_SS_SECTION)
        lss_section = lss_section->next;

      assert (lss_section != g95_ss_terminator);

      /* Initialize the scalarizer.  */
      g95_init_loopinfo (&loop);
      loop.dimen = lss_section->data.info.dimen;

      /* Walk the lhs.  */
      rss = g95_walk_expr (g95_ss_terminator, code->expr2);
      if (rss == g95_ss_terminator)
        {
          /* The rhs is scalar.  Add a ss for the expression.  */
          rss = g95_get_ss ();
          rss->next = g95_ss_terminator;
          rss->type = G95_SS_SCALAR;
          rss->expr = code->expr2;
        }
      /* The SS chains are built in reverse order, so reverse them.  */
      rss = g95_reverse_ss (rss);
      lss = g95_reverse_ss (lss);

      /* Find the last SS in the lhs chain.  */
      lss_tail = lss_section;
      while (lss_tail->next != g95_ss_terminator)
        lss_tail = lss_tail->next;

      /* Combine the lhs and rhs SS chains for loop generation.  This is
         needed because we're scalarizing two expressions at once.  */
      lss_tail->next = rss;
      loop.ss = lss;

      /* Calculate the bounds of the scalarization.  */
      g95_conv_ss_startstride (&loop);
      /* Resolve any data dependancies in the statement.  */
      g95_conv_resolve_dependencies (&loop, lss_section, rss);
      /* Setup the scalarizing loops.  */
      g95_conv_loopvars (&loop);
      /* Break the lhs and rhs chains apart, otherwise we'll confuse the
         expression translator.  */
      lss_tail->next = g95_ss_terminator;

      /* Setup the g95_se structures.  */
      g95_copy_loopinfo_to_se (&lse, &loop);
      g95_copy_loopinfo_to_se (&rse, &loop);
      rse.ss = rss;

      if (loop.temp_ss == NULL)
        lse.ss = lss;
      else
        lse.ss = loop.temp_ss;

      /* Enclose the loop body in it's own scope.  */
      g95_start_stmt ();
    }

  /* Translate the expression.  */
  g95_conv_simple_rhs (&rse, code->expr2);

  if (lss != g95_ss_terminator && loop.temp_ss != NULL)
    g95_conv_tmp_ref (&lse);
  else
    g95_conv_simple_lhs (&lse, code->expr);

  if (code->expr->ts.type == BT_CHARACTER)
    {
      /* String assignments are more complicated, so are handled seperately. */
      assign = g95_trans_string_assign (&lse, &rse);
    }
  else
    {
      tmp = build (MODIFY_EXPR, TREE_TYPE (lse.expr), lse.expr, rse.expr);
      assign = build_stmt (EXPR_STMT, tmp);
    }

  /* Chain all parts of the loop body together.  */
  g95_add_stmt_to_pre (&lse, rse.pre, rse.pre_tail);
  g95_add_stmt_to_pre (&lse, assign, NULL_TREE);
  g95_add_stmt_to_pre (&lse, rse.post, rse.post_tail);
  g95_add_stmt_to_pre (&lse, lse.post, lse.post_tail);
  /* The whole statement in now held by lse.pre.  */
  body = g95_finish_stmt (lse.pre, lse.pre_tail);

  if (lss != g95_ss_terminator)
    {
      /* Generate the scalarizer loop code.  */
      body = g95_trans_scalarizing_loops (&loop, body);
      g95_add_stmt_to_pre (&loop, body, NULL_TREE);

      if (loop.temp_ss != NULL)
        {
          /* We need to copy the temporary to the actual lhs.  */
          /* TODO: For unit stride packed arrays we could use memcpy.  */
          /* Generate code to copy the elements.  */
          g95_start_stmt ();
          g95_init_se (&lse, NULL);
          g95_init_se (&rse, NULL);
          g95_copy_loopinfo_to_se (&lse, &loop);
          g95_copy_loopinfo_to_se (&rse, &loop);

          rse.ss = loop.temp_ss;
          lse.ss = lss;

          g95_conv_tmp_ref (&rse);
          g95_conv_simple_lhs (&lse, code->expr);

          tmp = build (MODIFY_EXPR, TREE_TYPE (lse.expr), lse.expr, rse.expr);
          assign = build_stmt (EXPR_STMT, tmp);

          g95_add_stmt_to_pre (&lse, rse.pre, rse.pre_tail);
          g95_add_stmt_to_pre (&lse, assign, NULL_TREE);
          g95_add_stmt_to_pre (&lse, rse.post, rse.post_tail);
          g95_add_stmt_to_pre (&lse, lse.post, lse.post_tail);

          body = g95_finish_stmt (lse.pre, lse.pre_tail);
          /* Generate the copying loops.  */
          body = g95_trans_scalarizing_loops (&loop, body);
          g95_add_stmt_to_pre (&loop, body, NULL_TREE);
        }

      /* Wrap the whole thing up.  */
      g95_add_stmt_to_pre (&loop, loop.post, loop.post_tail);
      body = g95_finish_stmt (loop.pre, loop.pre_tail);

      g95_free_ss (lss);
      g95_free_ss (rss);
      if (loop.temp_ss)
        g95_free_ss (loop.temp_ss);
    }
  return body;
}

