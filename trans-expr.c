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
#define BACKEND_CODE
#include "g95.h"
#include "trans.h"
#include "trans-const.h"

/* Initialise a simple expression holder.  Currently this just zeros
   the contents, but it could do some copying once the scalariser is
   implemented.  */
void
g95_init_se(g95_se * to, g95_se * from)
{
  memset(to, 0, sizeof(g95_se));
}

/* This probably doesn't work since the push/pop_scope changes.
   Should be safe, just may create a temp in all circumstances.  */
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
   or a constnt so that it can be used repeatedly.  */
void
g95_make_tmp_expr(g95_se * se)
{
  tree  tmp;
  tree  stmt;
  /* we shouldn't call this with an empty expr */
  if (!(se && se->expr))
    abort();

  if (is_my_tmp_var(se))
    return;

  if (TREE_TYPE(se->expr) == 'c')
    return;

  /* we need a temporary for this result */
  tmp=g95_create_tmp_var(TREE_TYPE(se->expr));
  stmt=build_stmt(EXPR_STMT,
      build(MODIFY_EXPR, TREE_TYPE(se->expr), tmp, se->expr));
  g95_add_stmt_to_pre(se, stmt, NULL);
  se->expr=tmp;
}

/* Return the contents of a variable. Also handles refrence/pointer
   variables (all Fortran pointers refrences are implicit) */
/*SCALARIZE*/
static void
g95_conv_variable (g95_se * se, g95_expr * expr)
{
  if (expr->ref)
    g95_todo_error ("valiable vith refs");

  se->expr = g95_get_symbol_decl (expr->symbol);

  /* Special case for assigning the return value of a function.
     Self recursive functions must have an explicit return value.  */
  if (se->expr == current_function_decl && expr->symbol->attr.function
        && (expr->symbol->result == expr->symbol))
   {
     se->expr = g95_get_fake_result_decl();
     return;
   }
  if (expr->symbol->attr.dummy)
    se->expr = build1 (INDIRECT_REF, TREE_TYPE(TREE_TYPE (se->expr)), se->expr);
}

/* Return and expr which is a single variable/value, suitable for
   function parameters and array indices.  */
void
g95_conv_simple_val(g95_se * se, g95_expr * expr)
{
  tree  tmp;
  tree  assign;
  tree  type;

  g95_conv_simple_rhs(se, expr);

  if (!is_simple_val(se->expr))
  {
    type=TREE_TYPE(se->expr);
    tmp=g95_create_tmp_var(type);
    assign=build(MODIFY_EXPR, type, tmp, se->expr);
    g95_add_stmt_to_pre(se, build_stmt(EXPR_STMT, assign), NULL);
    se->expr=tmp;
  }
}

/* Returns an lvalue, throws error if not possble.
   Guaranteed to return with se.post == NULL.  */
void
g95_conv_simple_lhs(g95_se * se, g95_expr * expr)
{
  tree  tmp;
  tree  assign;
  tree  type;

  g95_conv_simple_rhs(se, expr);

  if (! (is_simple_modify_expr_lhs(se->expr)
          || (TREE_CODE(se->expr) == RESULT_DECL)) )
    abort();
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
g95_conv_power_op(g95_se * se, g95_expr * expr)
{
  g95_todo_error("power op");
}

/* This operator need to be replaced with function calls as well, but
   in this case, we may be able to use a (combination of) builtin
   function(s) instead.  */
static void
g95_conv_concat_op(g95_se * se, g95_expr * expr)
{
  g95_todo_error("concat op");
}

/* Translates an expression. Common (binary) cases are handled by this
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

  /* combine the pre and post stmts */
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
g95_trans_pointer_assign (g95_code * code)
{
  g95_internal_error("pointer assignment not implemented");
}

/* Translate an assign statement.  */
tree
g95_trans_assign (g95_code * code)
{
  g95_se lse;
  g95_se rse;
  tree assign;

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

  return build_stmt(COMPOUND_STMT, lse.pre);
}

