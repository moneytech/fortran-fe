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
/* Only for g95_trans_assign and g95_trans_pointer_assign.  */
#include "trans-stmt.h"

/* Initialise a simple expression holder.  Currently this just zeros
   the contents, but it could do some copying once the scalariser is
   implemented.  */
void
g95_init_se(g95_se * to, g95_se * from ATTRIBUTE_UNUSED)
{
  memset(to, 0, sizeof(g95_se));
}

/* This probably doesn't work since the push/pop_scope changes.
   Should be safe, just will create a temp in all circumstances.  */
static bool
is_my_tmp_var(g95_se * se)
{
  tree  stmt;

  for (stmt = se->pre ; stmt ; stmt = TREE_CHAIN(stmt))
    {
      if ((TREE_CODE(stmt) == DECL_STMT) && (DECL_STMT_DECL(stmt) == se->expr))
        return(true);
    }
  return(false);
}

/* Ensures the result of the expression is either a temporary variable
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

/* Build an array reference. se->expr already holds the array descriptor.
   This should be either a variable, indirect variable reference or component
   reference.  */
/* TODO: Handle scalarized expressions.  */
static void
g95_conv_array_ref (g95_se * se, g95_array_ref * ar)
{
  tree ref;
  tree pointer;
  tree stmt;
  tree field;
  tree array;
  g95_se indices[G95_MAX_DIMENSIONS];
  int n;

  /* Scararization and array sections aren't implemented yet.  */
  assert (ar->type == AR_ELEMENT);
  assert (ar->dimen > 0);

  for (n = 0 ; n < ar->dimen ; n++)
    {
      /* We can only do single emenents without scalarization.  */
      assert (ar->dimen_type[n] == DIMEN_ELEMENT);

      g95_init_se (&indices[n], se);
      /* Calculate the index for this dimension.  For scalarized expressions we
         would insert the implicit loop variable here.  */
      g95_conv_simple_val (&indices[n], ar->start[n]);
      g95_add_stmt_to_pre (se, indices[n].pre, indices[n].pre_tail);
    }

  field = g95_get_data_component (TREE_TYPE (se->expr));

  /* Get a pointer to the array data.  */
  pointer = g95_create_tmp_var (TREE_TYPE (field));
  ref = build (COMPONENT_REF, TREE_TYPE (field), se->expr, field);
  ref = build (MODIFY_EXPR, TREE_TYPE (pointer), pointer, ref);
  stmt = build_stmt (EXPR_STMT, ref);
  g95_add_stmt_to_pre(se, stmt, stmt);

  array = build1 (INDIRECT_REF, TREE_TYPE (TREE_TYPE (pointer)), pointer);

  if (g95_use_gcc_arrays)
    {
      /* Build the reference backwards - Fortran uses column major ordering.  */
      for ( n = ar->dimen - 1 ; n >= 0 ; n--)
        {
          assert (TREE_CODE (TREE_TYPE (array)) == ARRAY_TYPE);
          array = build (ARRAY_REF, TREE_TYPE (TREE_TYPE (array)),
                          array, indices[n].expr);
        }
      /* Tell the backend where to find the array descriptor.  */
      ref = build (WITH_RECORD_EXPR, TREE_TYPE (array), array, se->expr);
    }
  else
    {
      tree index;
      tree num;
      tree prev;

      if (ar->dimen > 1)
        {
          index = g95_create_tmp_var (g95_array_index_type);
          num = g95_create_tmp_var (g95_array_index_type);
        }
      else
        {
          num = NULL_TREE;
          index = indices[0].expr;
        }

      for ( n = 1 ; n < ar->dimen ; n++)
        {
          field = g95_get_stride_component (TREE_TYPE (se->expr), n);

          /* num = index[n]*stride[n] */
          ref = build (COMPONENT_REF, TREE_TYPE (field), se->expr, field);
          ref = build (MULT_EXPR, g95_array_index_type, ref, indices[n].expr);
          ref = build (MODIFY_EXPR, g95_array_index_type, num, ref);
          stmt = build_stmt (EXPR_STMT, ref);
          g95_add_stmt_to_pre (se, stmt, stmt);
          if (n == 1)
            prev = indices[0].expr;
          else
            prev=index;
          /* index = index + num */
          ref = build (PLUS_EXPR, g95_array_index_type, prev, num);
          ref = build (MODIFY_EXPR, g95_array_index_type, index, ref);
          stmt  = build_stmt (EXPR_STMT, ref);
          g95_add_stmt_to_pre (se, stmt, stmt);
        }
      assert (TREE_CODE (TREE_TYPE (array)) == ARRAY_TYPE);
      ref = build (ARRAY_REF, TREE_TYPE( TREE_TYPE (array)), array, index);
    }

  /* Check we've used the correct number of dimensions.  */
  assert (TREE_CODE (TREE_TYPE (ref)) != ARRAY_TYPE);

  se->expr = ref;
}

/* Return the contents of a variable. Also handles refrence/pointer
   variables (all Fortran pointer refrences are implicit) */
/* TODO: Handle scalarized expressions.  */
static void
g95_conv_variable (g95_se * se, g95_expr * expr)
{
  g95_ref * ref;

  se->expr = g95_get_symbol_decl (expr->symbol);

  /* Special case for assigning the return value of a function.
     Self recursive functions must have an explicit return value.  */
  if (se->expr == current_function_decl && expr->symbol->attr.function
        && (expr->symbol->result == expr->symbol))
   {
     se->expr = g95_get_fake_result_decl();
     return;
   }

  /* This will also neet to be done for component references.  */
  if (expr->symbol->attr.pointer)
    g95_todo_error("Pointer variable");
  /*TODO: RHS of pointer assignments.  */


  if (expr->symbol->attr.dummy)
    se->expr = build1 (INDIRECT_REF, TREE_TYPE(TREE_TYPE (se->expr)), se->expr);

  ref = expr->ref;
  while (ref)
    {
      switch (ref->type)
        {
        case REF_ARRAY:
          g95_conv_array_ref(se, &ref->u.ar);
          break;

        default:
          g95_todo_error("component or substring ref");
          break;
        }
      ref = ref->next;
    }
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

  g95_conv_simple_rhs(se, expr);

  if (!is_simple_val(se->expr))
    {
      type = TREE_TYPE(se->expr);
      /* The target variable will be fixed up later.  */
      assign = build (MODIFY_EXPR, type, NULL_TREE, se->expr);
      g95_add_stmt_to_pre (se, build_stmt(EXPR_STMT, assign), NULL);
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

/* Returns an lvalue, throws error if not possble.
   Guaranteed to return with se.post == NULL.  */
void
g95_conv_simple_lhs(g95_se * se, g95_expr * expr)
{
  g95_conv_simple_rhs(se, expr);

  assert (is_simple_modify_expr_lhs(se->expr));
}

/* Return an expr suitable for a logic test, eg. an if condition.  */
void
g95_conv_simple_cond (g95_se *se, g95_expr *expr)
{
  tree  tmp;
  tree  assign;
  tree  type;

  g95_conv_simple_rhs(se, expr);

  if (!is_simple_condexpr(se->expr))
    {
      type=TREE_TYPE(se->expr);
      tmp=g95_create_tmp_var(type);
      assign=build(MODIFY_EXPR, type, tmp, se->expr);
      g95_add_stmt_to_pre(se, build_stmt(EXPR_STMT, assign), NULL);
      se->expr=tmp;
      g95_add_stmt_to_pre (se, se->post, se->post_tail);
      se->post = se->post_tail = NULL_TREE;
    }
}

/* Unary ops are easy... */
static void
g95_conv_unary_op(enum tree_code code, g95_se * se, g95_expr * expr)
{
  g95_se operand;
  tree type;

  /* Initialize the operand.  */
  g95_init_se(&operand, se);
  g95_conv_simple_val(&operand, expr->op1);

  /*SCALARIZE*/
  type = g95_typenode_for_spec(&expr->ts);

  se->expr = build1(code, type, operand.expr);

  /* combine the pre and post stmts */
  g95_add_stmt_to_pre(se, operand.pre, operand.pre_tail);
  g95_add_stmt_to_post(se, operand.post, operand.post_tail);
}

static void
g95_conv_power_op(g95_se * se ATTRIBUTE_UNUSED, g95_expr * expr ATTRIBUTE_UNUSED)
{
  g95_todo_error("power op");
}

/* This operator need to be replaced with function calls as well, but
   in this case, we may be able to use a (combination of) builtin
   function(s) instead.  */
static void
g95_conv_concat_op(g95_se * se ATTRIBUTE_UNUSED, g95_expr * expr ATTRIBUTE_UNUSED)
{
  g95_todo_error("concat op");
}

/* Translates an op expression. Common (binary) cases are handled by this
   function, others are passed on. Recursion is used in either case.
   We use the fact that (op1.ts == op2.ts) (except for the power
   operand **).  */
static void
g95_conv_expr_op (g95_se * se, g95_expr * expr)
{
  enum tree_code code;
  g95_se lse;
  g95_se rse;
  tree type;

  switch (expr->operator)
    {
    case INTRINSIC_UPLUS: /* Has no semantics in SIMPLE. Ignore.  */
      return;

    case INTRINSIC_UMINUS:
      g95_conv_unary_op (NEGATE_EXPR, se, expr);
      return;

    case INTRINSIC_NOT:
      g95_conv_unary_op(TRUTH_NOT_EXPR, se, expr);
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
      g95_conv_power_op(se, expr);
      return;

    case INTRINSIC_CONCAT:
      g95_conv_concat_op(se, expr);
      return;

    case INTRINSIC_AND:
      code = BIT_AND_EXPR;
      break;

    case INTRINSIC_OR:
      code = BIT_IOR_EXPR;
      break;

    /* EQV and NEQV only work on logicals, but since we represent them
       as integers, we can use QE_EXPR and NE_EXPR for them in SIMPLE.  */
    case INTRINSIC_EQ:
    case INTRINSIC_EQV:
      code = EQ_EXPR;
      break;

    case INTRINSIC_NE:
    case INTRINSIC_NEQV:
      code = NE_EXPR;
      break;

    case INTRINSIC_GT:
      code = GT_EXPR;
      break;

    case INTRINSIC_GE:
      code = GE_EXPR;
      break;

    case INTRINSIC_LT:
      code = LT_EXPR;
      break;

    case INTRINSIC_LE:
      code = LE_EXPR;
      break;

    case INTRINSIC_USER:
      g95_todo_error("User operation");
      return;

    case INTRINSIC_ASSIGN:
      g95_todo_error("intrinsic assign expr");
      return;

    default:
      g95_todo_error("unknown intrinsic op");
      return;
    }
  /* lhs */
  g95_init_se(&lse, se);
  g95_conv_simple_val(&lse, expr->op1);
  /* rhs */
  g95_init_se(&rse, se);
  g95_conv_simple_val(&rse, expr->op2);

  /*SCALARIZE*/
  type = g95_typenode_for_spec(&expr->ts);

  se->expr = build(code, type, lse.expr, rse.expr);

  /* Combine the pre and post stmts.  */
  g95_add_stmt_to_pre(se, lse.pre, lse.pre_tail);
  g95_add_stmt_to_pre(se, rse.pre, rse.pre_tail);
  g95_add_stmt_to_post(se, rse.post, rse.post_tail);
  g95_add_stmt_to_post(se, lse.post, lse.post_tail);
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
    default:
      g95_todo_error ("scalar expr type %d", expr->expr_type);
      break;
    }
}

tree
g95_trans_pointer_assign (g95_code * code ATTRIBUTE_UNUSED)
{
  g95_internal_error("pointer assignment not implemented");
}

#if 0
/* Work in progress... */
typedef struct g95_ss
{
  int dimen;
  int count;
  tree loopvar[G95_MAX_DIMENSIONS];
  g95_expr *lower[G95_MAX_DIMENSIONS];
  g95_expr *upper[G95_MAX_DIMENSIONS];
} g95_ss;

static void
g95_walk_expr_variable (g95_ss * ss, g95_expr * expr)
{
  g95_ref *ref;
  int n;

  if (!expr->sym->attr.dimension)
    return;

  for (ref = expr->ref ; ref ; ref = ref->next)
    {
      if (ref->type != REF_ARRAY)
        continue;

      switch (ar->type)
        {
        case AR_EMELENT:
          break;
        case AR_FULL:
          ss->dimen = ref->dimen;
          for (n = 0 ; n < ss->dimen ; n++)
            {

            }
          /* Add all dimensions.  */
          break;
        case AR_SECTION:
          for (n = 0 ; n < ref->dimen ; n++)
            {
              if (ref->dimen_type != DIMEN_ELEMENT)
                {
                  g95_walk_dimension()
                }
            }
          break;
        }
    }
}

static void
g95_walk_expr (g95_ss * ss, g95_expr * expr)
{
  switch (expr->expr_type)
    {
      case EXPR_VARIABLE:
        g95_walk_expr_variable (ss, expr);
        break;
    }


}
#endif

/* Translate an assign statement.  */
tree
g95_trans_assign (g95_code * code)
{
  g95_se lse;
  g95_se rse;
  tree assign;

  g95_start_stmt();

  g95_init_se(&lse, NULL);
  g95_conv_simple_lhs(&lse, code->expr);

  g95_init_se(&rse, NULL);
  g95_conv_simple_rhs(&rse, code->expr2);

  assign=build_stmt(EXPR_STMT, build(MODIFY_EXPR, TREE_TYPE(lse.expr),
                      lse.expr, rse.expr));

  /* chain all the stmts together */
  /* could use chainon, but that would be slow */
  g95_add_stmt_to_pre(&lse, rse.pre, rse.pre_tail);
  g95_add_stmt_to_pre(&lse, assign, NULL);
  g95_add_stmt_to_pre(&lse, rse.post, rse.post_tail);
  g95_add_stmt_to_pre(&lse, lse.post, lse.post_tail);
  /* lse.pre now holds the whole statement */

  return g95_finish_stmt (lse.pre, lse.pre_tail);
}

