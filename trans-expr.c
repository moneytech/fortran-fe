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

/* Free a g95_ss chain.  */
static void
g95_free_ss (g95_ss * ss)
{
  g95_ss *next;
  int n;

  while (ss != g95_ss_terminator)
    {
      assert (ss != NULL);
      next = ss->next;
      if (ss->dimen > 0)
        {
          for (n = 0; n < ss->dimen; n++)
            {
              if (ss->data.info.vector[n])
                g95_free (ss->data.info.vector[n]);
            }
        }
      else if (ss->dimen == -1)
        {
          if (ss->data.info.vector[0])
            g95_free (ss->data.info.vector[0]);
        }
      g95_free (ss);
      ss = next;
    }
}

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
  /* Skip vector SS.  */
  if (se->ss != g95_ss_terminator && se->ss->dimen == -1)
    g95_advance_se_ss_chain (se);
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

/* A reference to an array temporary.  The descriptor is in se->expr.  */
static void
g95_conv_tmp_ref (g95_se * se)
{
  tree pointer;
  tree indices[G95_MAX_DIMENSIONS];
  g95_ss_info *info;
  int n;

  assert (se->ss != NULL);

  info = &se->ss->data.info;
  pointer = info->data;
  se->expr = info->descriptor;
  for (n = 0; n < se->ss->dimen; n++)
    indices[n] = se->loopvar[n];

  g95_conv_array_index_ref (se, pointer, indices, se->ss->dimen);
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

  assert (ss && ss->dimen == -1);

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
        assert (indexss != g95_ss_terminator && indexss->dimen == 0);
        indices[n] = indexss->data.se.expr;
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
  //index = se->expr;
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
              assert (se->ss != g95_ss_terminator && se->ss->dimen == 0);
              /* We've already translated this value outside the loop.  */
              indices[n] = se->ss->data.se.expr;

              g95_advance_se_ss_chain (se);
            }
          else
            {
              assert (ss != g95_ss_terminator);
              /* Substutute a scalarizing loop variable.  */
              assert (ss != NULL && dim < ss->dimen);
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

      if (se->ss->dimen == 0)
        {
          /* This is a scalar term so we already have a value.  */
          se->expr = se->ss->data.se.expr;
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
         se->expr = g95_get_fake_result_decl();
         return;
       }

      /* RHS of pointer assignment, or allocation statement.
         Arrays and derived type components are handled seperately.  */
      if (se->want_pointer && expr->ref == NULL)
        {
          assert (expr->symbol->attr.pointer || expr->symbol->attr.allocatable);
          return;
        }

      if (expr->symbol->attr.dummy
          && ! (G95_DECL_STRING (se->expr)
                || expr->symbol->attr.dimension))
        {
          se->expr = build1 (INDIRECT_REF, TREE_TYPE(TREE_TYPE (se->expr)),
                            se->expr);
        }

      if ((expr->symbol->attr.pointer || expr->symbol->attr.allocatable)
          && (expr->symbol->attr.dummy || ! expr->symbol->attr.dimension))
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

/* Convert an array for passing as an actual function parameter.  */
static void
g95_conv_array_parameter (g95_se * se, g95_expr * expr, g95_ss * ss)
{
  g95_loopinfo loop;
  g95_ss * tmpss;
  int need_tmp;
  int n;
  tree tmp;
  tree stmt;
  tree desc;

  assert (ss != g95_ss_terminator);

  if (expr->ts.type == BT_CHARACTER)
    g95_todo_error ("Character string array actual parameters");

  g95_init_loopinfo (&loop);
  tmpss = ss;
  while (tmpss->dimen <= 0)
    tmpss = tmpss->next;

  loop.dimen = tmpss->dimen;

  /* Add in the pre code for scalar subexpressions.  */
  g95_add_ss_stmts (&loop, ss);

  ss = g95_reverse_ss (ss);

  loop.ss = ss;

  /* If we have a single array section, we can pass it directly.  If we have an
     expression or vector subscripts we need to copy it into a temporary.  */
  if (expr->expr_type == EXPR_VARIABLE)
    {
      need_tmp = 0;
      for (n = 0; n < loop.dimen; n++)
        {
          if (tmpss->data.info.vector[n])
            need_tmp = 1;
        }
    }
  else
    need_tmp = 2;

  if (need_tmp)
   {
     loop.temp_ss = g95_get_ss ();
     loop.temp_ss->next = g95_ss_terminator;
     loop.temp_ss->data.info.descriptor =
       g95_get_element_type (TREE_TYPE (tmpss->data.info.descriptor));
     loop.temp_ss->dimen = loop.dimen;
   }
  else
    loop.array_parameter = 1;

  /* Setup the scalarizing loops and bounds.  */
  g95_conv_ss_startstride (&loop);
  g95_conv_loopvars (&loop);

  if (need_tmp)
    {
      g95_se lse;
      g95_se rse;
      /* Copy into a temporary and pass that.  */
      /* TODO: work out intent of function arguments.  */
      /*if (arg->intent != INTENT_OUT)*/
        {
          tree descriptor;

          g95_start_stmt ();

          g95_init_se (&lse, NULL);
          g95_init_se (&rse, NULL);
          g95_copy_loopinfo_to_se (&lse, &loop);
          g95_copy_loopinfo_to_se (&rse, &loop);
          lse.ss = loop.temp_ss;
          rse.ss = ss;

          g95_conv_tmp_ref (&lse);
          g95_conv_simple_rhs (&rse, expr);

          tmp = build (MODIFY_EXPR, TREE_TYPE (rse.expr), lse.expr, rse.expr);
          stmt = build_stmt (EXPR_STMT, tmp);

          g95_add_stmt_to_pre (&lse, rse.pre, rse.pre_tail);
          g95_add_stmt_to_pre (&lse, stmt, stmt);
          g95_add_stmt_to_pre (&lse, rse.post, rse.post_tail);
          g95_add_stmt_to_pre (&lse, lse.post, lse.post_tail);

          stmt = g95_finish_stmt (lse.pre, lse.pre_tail);
          stmt = g95_trans_scalarizing_loops (&loop, stmt);

          g95_add_stmt_to_pre (&loop, stmt, NULL_TREE);

          /* Set the first stride component to zero to indicate a temporary.  */
          descriptor = loop.temp_ss->data.info.descriptor;
          tmp = g95_get_stride_component (TREE_TYPE (descriptor), 0);
          tmp = build (COMPONENT_REF, TREE_TYPE (tmp), descriptor, tmp);
          tmp = build (MODIFY_EXPR, TREE_TYPE (tmp), tmp, integer_zero_node);
          stmt = build_stmt (EXPR_STMT, tmp);
          g95_add_stmt_to_pre (&loop, stmt, stmt);
        }
      /* TODO: Copy back for vector subscripts.  */
#if 0
      /* Copy the result back.  */
      if (arg->intent != INTENT_IN && need_tmp == 1)
        {
          g95_start_stmt();

          g95_init_se (&lse, NULL);
          g95_init_se (&rse, NULL);
          g95_copy_loopinfo_to_se (&lse, &loop);
          g95_copy_loopinfo_to_se (&rse, &loop);
          lse.ss = loop->temp_ss;
          rse.ss = ss;

          g95_conv_simple_lhs (&lse, expr);
          g95_conv_temp_ref (&rse);

          tmp = build (MODIFY_EXPR, TREE_TYPE (lse.expr), lse.expr, rse.expr);
          stmt = build_stmt (EXPR_STMT, tmp);

          g95_add_stmt_to_pre (&lse, rse.pre, rse.pre_tail);
          g95_add_stmt_to_pre (&lse, stmt, stmt);
          g95_add_stmt_to_pre (&lse, rse.post, rse.post_tail);
          g95_add_stmt_to_pre (&lse, lse.post, lse.post_tail);

          stmt = g95_finish_stmt (lse.pre, lse.pre_tail);
          stmt = g95_trans_scalarizing_loops (&loop, stmt);

          g95_add_stmt_to_post (&loop, stmt, NULL_TREE);
        }
#endif
      desc = loop.temp_ss->data.info.descriptor;
      assert (is_simple_varname (desc));
      TREE_ADDRESSABLE (desc) = 1;
      se->expr = build1 (ADDR_EXPR, build_pointer_type (TREE_TYPE (desc)),
                        desc);
      se->expr = g95_simple_fold (se->expr, &loop.pre, &loop.pre_tail, NULL);

      /* Free the temporary.  */
      if (loop.temp_ss->data.info.pdata != NULL_TREE)
        {
          tmp = tree_cons (NULL_TREE, loop.temp_ss->data.info.pdata,
              NULL_TREE);
          tmp = g95_build_function_call (g95_fndecl_internal_free, tmp);
          stmt = build_stmt (EXPR_STMT, tmp);
          g95_add_stmt_to_post (&loop, stmt, stmt);
        }
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

      info = &tmpss->data.info;
      type = TREE_TYPE (info->descriptor);
      desc = info->descriptor;
      
      full = 1;
      if (info->ref->u.ar.type != AR_FULL)
        {
          assert (info->ref->u.ar.type == AR_SECTION);
          
          for (n = 0; n < info->ref->u.ar.dimen; n++)
            {
              /* Detect passing the full array as a section.  */
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
          parm = g95_create_tmp_var (type);

          offset = integer_zero_node;
          offsetvar = NULL_TREE;
          stridevar = NULL_TREE;
          tmpvar = NULL_TREE;
          dim = 0;

          /* The following can be somewhat confusing.  We have two
             descriptors, a temporary and the original array.
             {parm, parmtype, dim} refer to the temporary.
             {desc, type, n, tmpss, loop} refer to the original.
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
          tmp = build (COMPONENT_REF, TREE_TYPE (field), desc, field);
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

  g95_free_ss (ss);
  if (loop.temp_ss)
    g95_free_ss (loop.temp_ss);
}

/* TODO: remove limit on function parameters.  */
#define G95_MAX_FUNCTION_ARGS 32

/* Translate a function call.  Note can return se->post != NULL.  */
void
g95_conv_function_call (g95_se * se, g95_symbol * sym,
                       g95_actual_arglist * arg)
{
  tree parms[G95_MAX_FUNCTION_ARGS];
  tree arglist;
  tree tmp;
  tree fntype;
  g95_se parmse;
  g95_ss *ss;
  int nargs;
  int n;

  if (se->ss != NULL)
    {
      if (! sym->attr.elemental)
        {
          /* Non-elemental functions should already have been evaluated.  */
          se->expr = se->ss->data.se.expr;
          g95_advance_se_ss_chain (se);
          return;
        }
    }

  /* Evaluate the arguments.  */
  for (nargs = 0; arg != NULL; arg = arg->next)
    {
      /* We don't do alternate returns.  */
      assert (arg->expr != NULL);
      if (nargs >= G95_MAX_FUNCTION_ARGS)
        fatal_error ("Too many function args");

      g95_init_se (&parmse, se);

      if (se->ss == NULL)
        {
          /* A scalar function.  */
          ss = g95_walk_expr (g95_ss_terminator, arg->expr);

          if (ss == g95_ss_terminator)
            g95_conv_simple_reference (&parmse, arg->expr);
          else
            g95_conv_array_parameter (&parmse, arg->expr, ss);
        }
      else
        {
          /* An elemental function inside a scalarized loop.  */
          g95_conv_simple_val (&parmse, arg->expr);
        }

      g95_add_stmt_to_pre (se, parmse.pre, parmse.pre_tail);
      g95_add_stmt_to_post (se, parmse.post, parmse.post_tail);

      if (parmse.string_length != NULL_TREE)
        parms[nargs++] = parmse.string_length;
      parms[nargs++] = parmse.expr;
    }

  /* Build the argument list.  */
  arglist = NULL_TREE;
  for (n = nargs -1; n >= 0; n--)
    {
      arglist = tree_cons (NULL_TREE, parms[n], arglist);
    }

  g95_conv_function_val (se, sym);
  fntype =TREE_TYPE (TREE_TYPE (se->expr));
  tmp = build (CALL_EXPR, TREE_TYPE (fntype), se->expr, arglist);
  se->expr = tmp;

  if (! sym->attr.pure)
    TREE_SIDE_EFFECTS (se->expr) = 1;
}

static void
g95_conv_function (g95_se * se, g95_expr * expr)
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
      /* The frontend does not create a symbol for implicitly declared
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
      g95_conv_function (se, expr);
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

/* Resolve array data dependancies.  */
static void
g95_conv_resolve_dependencies (g95_loopinfo * loop, g95_ss * dest,
                              g95_ss * rss)
{
  g95_ss *ss;
  int n;
  int need_temp;

  for (n = 0; n < loop->dimen; n++)
      loop->order[n] = n;

  loop->temp_ss = NULL;
  need_temp = 0;
  for (ss = rss; ss != g95_ss_terminator; ss = ss->next)
    {
      if (ss->dimen <= 0)
        continue;
      if (ss->expr->symbol == dest->expr->symbol)
        need_temp = 1;
    }

  if (need_temp)
    {
      loop->temp_ss = g95_get_ss();
      loop->temp_ss->data.info.descriptor =
        g95_get_element_type (TREE_TYPE (dest->data.info.descriptor));
      loop->temp_ss->dimen = loop->dimen;
      loop->temp_ss->next = g95_ss_terminator;
    }
  else
    loop->temp_ss = NULL;
}

/* Like chainon(list, listify(add)) except it ignores TREE_CHAIN(add).  */
tree
g95_chainon_list (tree list, tree add)
{
  tree l;

  l = tree_cons (NULL_TREE, add, NULL_TREE);

  return chainon (list, l);
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

/* Translate an assign statement.  */
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

  g95_start_stmt ();

  g95_init_se (&lse, NULL);
  g95_init_se (&rse, NULL);

  /* Setup the scalarization loop.  */
  lss = g95_walk_expr (g95_ss_terminator, code->expr);
  if (lss != g95_ss_terminator)
    {
      lss_section = lss;

      if (code->expr->ts.type == BT_CHARACTER)
        g95_todo_error ("Character array assignments");

      while (lss_section != g95_ss_terminator && lss_section->dimen <= 0)
        lss_section = lss_section->next;

      assert (lss_section != g95_ss_terminator);

      g95_init_loopinfo (&loop);
      loop.dimen = lss_section->dimen;

      rss = g95_walk_expr (g95_ss_terminator, code->expr2);
      if (rss == g95_ss_terminator)
        {
          /* RHS is scalar.  */
          rss = g95_get_ss ();
          rss->next = g95_ss_terminator;
          rss->dimen = 0;
          rss->expr = code->expr2;
          g95_init_se (&rss->data.se, NULL);
          g95_conv_simple_val (&rss->data.se, code->expr2);
        }
      /* Add in the pre code for scalar subexpressions.  */
      g95_add_ss_stmts (&loop, lss);
      g95_add_ss_stmts (&loop, rss);

      rss = g95_reverse_ss (rss);
      lss = g95_reverse_ss (lss);

      lss_tail = lss_section;
      while (lss_tail->next != g95_ss_terminator)
        lss_tail = lss_tail->next;

      /* Chain the lhs and rhs SS together for loop generation.  This is
         needed because we're scalarizing two expressions at once.  */
      lss_tail->next = rss;
      loop.ss = lss;

      /* Setup the scalarizing loops.  */
      g95_conv_ss_startstride (&loop);
      g95_conv_resolve_dependencies (&loop, lss_section, rss);
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
  else
    rss = NULL;

  /* Translate the expression.  */
  g95_conv_simple_rhs (&rse, code->expr2);

  if (lss != g95_ss_terminator && loop.temp_ss != NULL)
    g95_conv_tmp_ref (&lse);
  else
    g95_conv_simple_lhs (&lse, code->expr);

  if (code->expr->ts.type == BT_CHARACTER)
    {
      assign = g95_trans_string_assign (&lse, &rse);
    }
  else
    {
      tmp = build (MODIFY_EXPR, TREE_TYPE (lse.expr), lse.expr, rse.expr);
      assign = build_stmt (EXPR_STMT, tmp);
    }

  /* Chain all the stmts together.  */
  g95_add_stmt_to_pre (&lse, rse.pre, rse.pre_tail);
  g95_add_stmt_to_pre (&lse, assign, NULL_TREE);
  g95_add_stmt_to_pre (&lse, rse.post, rse.post_tail);
  g95_add_stmt_to_pre (&lse, lse.post, lse.post_tail);
  /* The whole statement in now held by lse.pre.  */
  body = g95_finish_stmt (lse.pre, lse.pre_tail);

  if (lss != g95_ss_terminator)
    {
      body = g95_trans_scalarizing_loops (&loop, body);
      g95_add_stmt_to_pre (&loop, body, NULL_TREE);

      if (loop.temp_ss != NULL)
        {
          /* Create loops to copy the temporary.  */
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
          body = g95_trans_scalarizing_loops (&loop, body);
          g95_add_stmt_to_pre (&loop, body, NULL_TREE);

          /* Free the temporary.  */
          if (loop.temp_ss->data.info.pdata != NULL_TREE)
            {
              tmp = tree_cons (NULL_TREE, loop.temp_ss->data.info.pdata,
                  NULL_TREE);
              tmp = g95_build_function_call (g95_fndecl_internal_free, tmp);
              body = build_stmt (EXPR_STMT, tmp);
              g95_add_stmt_to_pre (&loop, body, body);
            }
        }

      /* Wrap the whole thing up.  */
      body = g95_finish_stmt (loop.pre, loop.pre_tail);

      g95_free_ss (lss);
      g95_free_ss (rss);
      if (loop.temp_ss)
        g95_free_ss (loop.temp_ss);
    }
  return body;
}

