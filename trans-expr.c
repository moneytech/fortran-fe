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

/* TODO: intinsic functions - move to their own file.  */
static void
g95_conv_intrinsic_function (g95_se * se ATTRIBUTE_UNUSED, g95_expr * expr ATTRIBUTE_UNUSED)
{
  warning ("intrinsic function");
  se->expr = integer_zero_node;
}

/* Copy the scalarization loop variables.  */
static void
g95_copy_se_loopvars (g95_se * dest, g95_se * src)
{
  int n;

  dest->dimen = src->dimen;
  dest->ss = src->ss;
  for (n = 0 ; n < src->dimen ; n++)
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
  assert (se != NULL && se->ss != NULL
            && se->ss != g95_ss_terminator);

  /* Walk down the parent chain.  */
  while (se != NULL)
    {
      /* Simple consistancy check.  */
      assert (se->parent == NULL || se->parent->ss == se->ss);

      se->ss = se->ss->next;

      se = se->parent;
    }
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

/* TODO: precalculate the stride for known size arrays.  */
static tree
g95_conv_array_stride (tree descriptor, int dim, g95_array_ref * ar ATTRIBUTE_UNUSED)
{
  tree tmp;
  tree field;

  field = g95_get_stride_component (TREE_TYPE (descriptor), dim);
  tmp = build (COMPONENT_REF, TREE_TYPE (field), descriptor, field);
  return tmp;
}

/* Build an array reference. se->expr already holds the array descriptor.
   This should be either a variable, indirect variable reference or component
   reference.  */
static void
g95_conv_array_ref (g95_se * se, g95_array_ref * ar)
{
  tree index;
  tree tmp;
  tree var;
  tree pointer;
  tree stmt;
  tree field;
  tree array;
  g95_se indices[G95_MAX_DIMENSIONS];
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
      dimen = ss->dimen;
      info = &ss->data.info;
    }
  else
    {
      assert (ar->type == AR_ELEMENT);
      dimen = ar->dimen;
      info = NULL;
    }

  /* TODO: Temporaries with rank != rank of original array. */
  for (n = 0 ; n < dimen ; n++)
    {
      g95_init_se (&indices[n], se);
      var = NULL_TREE;
      if (ar->type == AR_ELEMENT)
        {
          g95_conv_simple_val (&indices[n], ar->start[n]);
          g95_add_stmt_to_pre (se, indices[n].pre, indices[n].pre_tail);
        }
      else
        {
          if (ar->dimen_type[n] == DIMEN_ELEMENT)
            {
              assert (se->ss != g95_ss_terminator);
              /* We've already translated this value outside the loop.  */
              indices[n].expr = se->ss->data.se.expr;
            }
          else
            {
              assert (ar->dimen_type[n] == DIMEN_RANGE);
              assert (ss != g95_ss_terminator);
              /* Substutute a scalarizing loop variable.  */
              dim = info->dim[n];
              assert (ss != NULL && dim < se->dimen);

              index = se->loopvar[dim];
              tmp = build (MULT_EXPR, g95_array_index_type,
                            se->loopvar[dim], info->stride[dim]);
              index = g95_simple_fold (tmp, &se->pre, &se->pre_tail, &var);

              tmp = build (PLUS_EXPR, g95_array_index_type,
                            index, info->delta[dim]);
              index = g95_simple_fold (tmp, &se->pre, &se->pre_tail, &var);
              indices[n].expr = index;
              dim++;
            }
        }
    }

  field = g95_get_data_component (TREE_TYPE (se->expr));

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

  array = build1 (INDIRECT_REF, TREE_TYPE (TREE_TYPE (pointer)), pointer);

  if (g95_use_gcc_arrays)
    {
      /* Build the reference backwards - Fortran uses column major ordering.  */
      for ( n = dimen - 1 ; n >= 0 ; n--)
        {
          assert (TREE_CODE (TREE_TYPE (array)) == ARRAY_TYPE);
          array = build (ARRAY_REF, TREE_TYPE (TREE_TYPE (array)),
                          array, indices[n].expr);
        }
      /* Tell the backend where to find the array descriptor.  */
      tmp = build (WITH_RECORD_EXPR, TREE_TYPE (array), array, se->expr);
    }
  else
    {
      tree num;
      tree prev;

      if (dimen > 1)
        {
          index = g95_create_tmp_var (g95_array_index_type);
          num = g95_create_tmp_var (g95_array_index_type);
        }
      else
        {
          num = NULL_TREE;
          index = indices[0].expr;
        }

      for ( n = 1 ; n < dimen ; n++)
        {
          /* num = index[n]*stride[n] */
          tmp = g95_conv_array_stride (se->expr, n, ar);
          tmp = build (MODIFY_EXPR, g95_array_index_type, num, tmp);
          stmt = build_stmt (EXPR_STMT, tmp);
          g95_add_stmt_to_pre (se, stmt, stmt);

          tmp = build (MULT_EXPR, g95_array_index_type, num, indices[n].expr);
          tmp = build (MODIFY_EXPR, g95_array_index_type, num, tmp);
          stmt = build_stmt (EXPR_STMT, tmp);
          g95_add_stmt_to_pre (se, stmt, stmt);
          if (n == 1)
            prev = indices[0].expr;
          else
            prev=index;
          /* index = index + num */
          tmp = build (PLUS_EXPR, g95_array_index_type, prev, num);
          tmp = build (MODIFY_EXPR, g95_array_index_type, index, tmp);
          stmt  = build_stmt (EXPR_STMT, tmp);
          g95_add_stmt_to_pre (se, stmt, stmt);
        }
      assert (TREE_CODE (TREE_TYPE (array)) == ARRAY_TYPE);
      tmp = build (ARRAY_REF, TREE_TYPE( TREE_TYPE (array)), array, index);
    }

  /* Check we've used the correct number of dimensions.  */
  assert (TREE_CODE (TREE_TYPE (tmp)) != ARRAY_TYPE);

  se->expr = tmp;
}

/* Return the contents of a variable. Also handles refrence/pointer
   variables (all Fortran pointer refrences are implicit) */
static void
g95_conv_variable (g95_se * se, g95_expr * expr)
{
  g95_ref * ref;

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

      if (expr->symbol->attr.dummy)
        {
          se->expr = build1 (INDIRECT_REF, TREE_TYPE(TREE_TYPE (se->expr)),
                            se->expr);
        }

      if ((expr->symbol->attr.pointer || expr->symbol->attr.allocatable)
            && ! expr->symbol->attr.dimension)
        {
          se->expr = build1 (INDIRECT_REF, TREE_TYPE(TREE_TYPE (se->expr)),
                            se->expr);
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

  assert (! se->ss);

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

void
g95_conv_simple_reference (g95_se * se, g95_expr * expr)
{
  assert (! se->ss);

  g95_conv_simple_rhs (se, expr);

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
g95_conv_simple_lhs(g95_se * se, g95_expr * expr)
{
  g95_conv_simple_rhs(se, expr);

  assert (is_simple_modify_expr_lhs(se->expr));
  assert (se->post == NULL_TREE);
}

/* Return an expr suitable for a logic test, eg. an if condition.  */
void
g95_conv_simple_cond (g95_se *se, g95_expr *expr)
{
  tree  tmp;
  tree  assign;
  tree  type;

  assert (! se->ss);

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
g95_conv_power_op(g95_se * se, g95_expr * expr)
{
  g95_todo_error("concat op");
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
   operand **).
   Operators need no special handling for scalarized expressions as long as
   they call g95_conv_siple_val to get their operands.  */
static void
g95_conv_expr_op (g95_se * se, g95_expr * expr)
{
  enum tree_code code;
  g95_se lse;
  g95_se rse;
  tree type;

  switch (expr->operator)
    {
    case INTRINSIC_UPLUS:
      g95_conv_simple_val(se, expr->op1);
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
       as integers, we can use EQ_EXPR and NE_EXPR for them in SIMPLE.  */
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
      g95_todo_error("User operatior");
      return;

    case INTRINSIC_ASSIGN:
      g95_todo_error("intrinsic assignment expr");
      return;

    default:
      fatal_error("Unknown intrinsic op");
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

/* TODO: remove limit of function parameters.  */
#define G95_MAX_FUNCTION_ARGS 32

/* Translate a function call.  Note can return se->post != NULL.  */
void
g95_conv_function_call (g95_se * se, g95_symbol * sym, g95_actual_arglist * arg)
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
  for (nargs = 0 ; arg != NULL ; arg = arg->next, nargs++)
    {
      /* We don't do alternate returns.  */
      assert (arg->expr != NULL);
      if (nargs > G95_MAX_FUNCTION_ARGS)
        fatal_error ("Too many function args");

      g95_init_se (&parmse, se);

      if (se->ss == NULL)
        {
          /* A scalar function.  */
          ss = g95_walk_expr (g95_ss_terminator, arg->expr);

          if (ss == g95_ss_terminator)
            {
              /* TODO: passing by value when possible.  */
              g95_conv_simple_reference (&parmse, arg->expr);
            }
          else
            {
              g95_todo_error ("Function array parameters");
            }
        }
      else
        {
          /* An elemental function inside a scalarized loop.  */
          g95_conv_simple_val (&parmse, arg->expr);
        }

      g95_add_stmt_to_pre (se, parmse.pre, parmse.pre_tail);
      g95_add_stmt_to_pre (se, parmse.post, parmse.post);

      parms[nargs] = parmse.expr;
    }

  /* Build the argument list.  */
  arglist = NULL_TREE;
  for (n = nargs -1 ; n >= 0 ; n--)
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

  g95_conv_function_call (se, psym,
      expr->value.function.actual);
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
  g95_internal_error("pointer assignment not implemented");
}

/* Free a g95_ss chain.  */
static void
g95_free_ss (g95_ss *ss)
{
  g95_ss *next;

  while (ss != NULL && ss != g95_ss_terminator)
    {
      next = ss->next;
      g95_free (ss);
      ss = next;
    }
  assert (ss == g95_ss_terminator);
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

static void
g95_add_ss_stmts (g95_loopinfo * loop, g95_ss *ss)
{
  g95_se *se;

  assert (ss != NULL);

  while (ss != g95_ss_terminator)
    {
      if (ss->dimen == 0)
        {
          se = &ss->data.se;

          g95_add_stmt_to_pre (loop, se->pre, se->pre_tail);
          se->pre = se->pre_tail = NULL_TREE;
        }
      ss = ss->next;
    }
}

/* Calculates the range start and stride for a SS chain.  */
static void
g95_conv_ss_startstride (g95_loopinfo * loop)
{
  int n;
  int dim;
  g95_ss_info *info;
  tree tmp;
  tree field;
  g95_ss *ss;
  g95_expr *expr;
  g95_se se;


  for (ss = loop->ss ; ss != g95_ss_terminator ; ss = ss->next)
    {
      if (ss->dimen <= 0)
          continue;

      info = &ss->data.info;

      /* Get the descriptor for the array to be scalarized.  */
      assert (ss->expr->expr_type == EXPR_VARIABLE);
      g95_init_se (&se, NULL);
      se.descriptor_only = 1;
      g95_conv_simple_rhs (&se, ss->expr);
      assert (se.post == NULL_TREE);
      g95_add_stmt_to_pre (loop, se.pre, se.pre_tail);
      info->descriptor = se.expr;

      /* Also the data pointer.  */
      field = g95_get_data_component (TREE_TYPE (info->descriptor));
      info->data = g95_create_tmp_var (TREE_TYPE (field));
      tmp = build (COMPONENT_REF, TREE_TYPE (field), info->descriptor, field);
      tmp = build (MODIFY_EXPR, TREE_TYPE (field), info->data, tmp);
      tmp = build_stmt (EXPR_STMT, tmp);
      g95_add_stmt_to_pre (loop, tmp, tmp);

      for ( n = 0 ; n < ss->dimen ; n++)
        {
          dim = info->dim[n];

          /* Calculate the start of the range.  */
          if (info->ref->u.ar.start[dim]
                && info->ref->u.ar.start[dim]->expr_type == EXPR_CONSTANT)
            {
              g95_init_se (&se, NULL);
              g95_conv_simple_val (&se, info->ref->u.ar.start[dim]);
              info->start[n] = se.expr;
            }
          else
            {
              info->start[n] = NULL_TREE;
              if (info->ref->u.ar.as->type != AS_DEFERRED)
                {
                  /* We may know the lower bound for assumed shape/size
                     arrays and explicit shape arrays.  */
                  expr = info->ref->u.ar.as->lower[dim];
                  if (expr != NULL && expr->expr_type == EXPR_CONSTANT)
                    {
                      g95_init_se (&se, NULL);
                      g95_conv_constant (&se, expr);
                      info->start[n] = se.expr;
                    }
                }

              if (info->start[n] == NULL_TREE)
                {
                  info->start[n] = g95_create_tmp_var (g95_array_index_type);
                  field = g95_get_lbound_component (
                                  TREE_TYPE (info->descriptor), dim);
                  tmp = build (COMPONENT_REF, g95_array_index_type,
                                  info->descriptor, field);
                  tmp = build (MODIFY_EXPR, g95_array_index_type,
                                  info->start[n], tmp);
                  tmp = build_stmt (EXPR_STMT, tmp);
                  g95_add_stmt_to_pre (loop, tmp, tmp);
                }
            }

          /* Calculate the stride.  */
          if (info->ref->u.ar.stride[dim] == NULL)
              info->stride[n] = integer_one_node;
          else
            {
              g95_init_se (&se, NULL);
              g95_conv_simple_val (&se, info->ref->u.ar.stride[dim]);
              info->stride[n] = se.expr;
            }
        }
    }
}

/* Initialise the scalarization loop parameters.  */
static void
g95_conv_loopvars (g95_loopinfo * loop)
{
  int n;
  int dim;
  g95_se se;
  g95_ss_info *info;
  g95_ss_info *specinfo;
  g95_ss *ss;
  g95_expr *expr;
  tree tmp;
  tree field;
  tree var;

  for ( n = 0 ; n < loop->dimen ; n++)
    {
      loop->loopspec[n] = NULL;
      /* Find a loop to get the range from.  */
      for (ss = loop->ss ; ss != g95_ss_terminator ; ss = ss->next)
        {
          if (ss->dimen <= 0)
            continue;

          /* Criteria for choosing a loop specifier (in order of inportance):
              stride of one
              known stride
              known lower bound
             Failure to meet this criteria will just mean that more setup
             code is needed for the loop.  */

          info = &ss->data.info;
          if (loop->loopspec[n])
            specinfo = &loop->loopspec[n]->data.info;
          else
            specinfo = NULL;
          info = &ss->data.info;

          if (! specinfo)
            loop->loopspec[n] = ss;
          else if (integer_onep (info->stride[n])
                    && ! integer_onep (specinfo->stride[n]))
            loop->loopspec[n] = ss;
          else if (INTEGER_CST_P (info->stride[n])
                    && ! INTEGER_CST_P (specinfo->stride[n]))
            loop->loopspec[n] = ss;
          else if (INTEGER_CST_P (info->start[n])
                    && ! INTEGER_CST_P (specinfo->stride[n]))
            loop->loopspec[n] = ss;
        }

      info = &loop->loopspec[n]->data.info;

      dim = info->dim[n];

      /* Set the extents of this range.  */
      loop->from[n] = info->start[n];
      info->delta[n] = integer_zero_node;
      if (info->ref->u.ar.end[dim])
        {
          g95_init_se (&se, NULL);
          g95_conv_simple_val (&se, info->ref->u.ar.end[dim]);
          loop->to[n] = se.expr;
        }
      else
        {
          loop->to[n] = NULL_TREE;
          if (info->ref->u.ar.as->type != AS_DEFERRED)
            {
              /* We may know the lower bound for assumed shape/size
                 arrays and explicit shape arrays.  */
              expr = info->ref->u.ar.as->upper[dim];
              if (expr != NULL && expr->expr_type == EXPR_CONSTANT)
                {
                  g95_init_se (&se, NULL);
                  g95_conv_constant (&se, expr);
                  loop->to[n] = se.expr;
                }
            }

          if (loop->to[n] == NULL_TREE)
            {
              loop->to[n] = g95_create_tmp_var (g95_array_index_type);
              field = g95_get_ubound_component (
                             TREE_TYPE (info->descriptor), dim);
              tmp = build (COMPONENT_REF, g95_array_index_type,
                             info->descriptor, field);
              tmp = build (MODIFY_EXPR, g95_array_index_type,
                              loop->to[n], tmp);
              tmp = build_stmt (EXPR_STMT, tmp);
              g95_add_stmt_to_pre (loop, tmp, tmp);
            }
        }

      /* We will want to divide by step.  However this complicates things.  */
      if (! integer_onep(info->stride[n]))
          g95_todo_error ("expression with no unit stride ranges");

      /* Create the loop variable.  */
      loop->loopvar[n] = g95_create_tmp_var (g95_array_index_type);
    }

  for (ss = loop->ss ; ss != g95_ss_terminator ; ss = ss->next)
    {
      if (ss->dimen <= 0)
        continue;

      info = &ss->data.info;

      for ( n = 0 ; n < ss->dimen ; n++)
        {
          dim = info->dim[n];

          var = NULL_TREE;
          /* If we are specifying the range the dela should already be set.  */
          if (ss != loop->loopspec[n])
            {
              /* Calculate the offset relative to the loop variable.  */
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

/* Resolve array data dependancies.  */
static void
g95_conv_resolve_dependencies (g95_loopinfo *loop, g95_ss * dest, g95_ss * rss)
{
  g95_ss *ss;
  int n;

  for (n = 0 ; n < loop->dimen ; n++)
    {
      loop->order[n] = n;
    }

  for (ss = rss ; ss != g95_ss_terminator ; ss = ss->next)
    {
      if (ss->dimen <= 0)
        continue;
      if (ss->expr->symbol == dest->expr->symbol)
        g95_todo_error ("array dependencies");
    }
}

/* Copies the loop variable info to a g95_se sructure. Does not copy the SS
   chain.  */
static void
g95_copy_loopinfo_to_se (g95_se * se, g95_loopinfo * loop)
{
  int n;

  se->dimen = loop->dimen;
  for (n = 0 ; n < loop->dimen ; n++)
    {
      se->loopvar[n] = loop->loopvar[n];
    }
}

/* Generates the actual loops for a scalarized expression.  */
static tree
g95_trans_scalarizing_loops (g95_loopinfo *loop, tree body)
{
  tree init;
  tree cond;
  tree inc;
  tree tmp;
  int n;
  int dim;

  for (dim = 0 ; dim < loop->dimen ; dim++)
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

/* Initialise a g95_loopinfo structure.  */
static void
g95_init_loopinfo (g95_loopinfo * loop)
{
  memset(loop, 0, sizeof(g95_loopinfo));
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
  tree body;

  g95_start_stmt();

  g95_init_se(&lse, NULL);
  g95_init_se(&rse, NULL);

  /* Setup the scalarization loop.  */
  lss = g95_walk_expr (g95_ss_terminator, code->expr);
  if (lss != g95_ss_terminator)
    {
      lss_section = lss;

      while (lss_section != g95_ss_terminator && lss_section->dimen <= 0)
        lss_section = lss_section->next;

      assert (lss_section != g95_ss_terminator);

      g95_init_loopinfo(&loop);
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

      lss_tail = lss_section;
      while (lss_tail->next != g95_ss_terminator)
        lss_tail = lss_tail->next;

      /* Chain the lhs and rhs SS together for loop generation.  This is
         needed because we're scalarizing two expressions at once.  */
      lss_tail->next = rss;
      loop.ss = lss;

      /* Setup the scalarizing loops.  */
      g95_conv_ss_startstride (&loop);
      g95_conv_resolve_dependencies(&loop, lss_section, rss);
      g95_conv_loopvars(&loop);
      /* Break the lhs and rhs chains apart, otherwise we'll confuse the
         expression translator.  */
      lss_tail->next = g95_ss_terminator;

      /* Setup the g95_se structures.  */
      g95_copy_loopinfo_to_se (&lse, &loop);
      g95_copy_loopinfo_to_se (&rse, &loop);
      lse.ss = lss;
      rse.ss = rss;

      /* Enclose the loop body in it's own scope.  */
      g95_start_stmt ();
    }
  else
    rss = NULL;

  /* Translate the expression.  */
  g95_conv_simple_lhs(&lse, code->expr);
  g95_conv_simple_rhs(&rse, code->expr2);

  assign=build_stmt(EXPR_STMT, build(MODIFY_EXPR, TREE_TYPE(lse.expr),
                      lse.expr, rse.expr));

  /* Chain all the stmts together.  */
  g95_add_stmt_to_pre(&lse, rse.pre, rse.pre_tail);
  g95_add_stmt_to_pre(&lse, assign, NULL);
  g95_add_stmt_to_pre(&lse, rse.post, rse.post_tail);
  g95_add_stmt_to_pre(&lse, lse.post, lse.post_tail);
  /* The whole statement in now held by lse.pre.  */
  body = g95_finish_stmt (lse.pre, lse.pre_tail);

  if (lss != g95_ss_terminator)
    {
      body = g95_trans_scalarizing_loops (&loop, body);

      /* Add the loops to the end of pre chain and wrap up.  */
      g95_add_stmt_to_pre (&loop, body, NULL_TREE);
      body = g95_finish_stmt (loop.pre, loop.pre_tail);

      g95_free_ss (lss);
      g95_free_ss (rss);
    }
  return body;
}

