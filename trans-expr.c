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
#include "coretypes.h"
#include "tree.h"
#include <stdio.h>
#include "c-common.h"
#include "ggc.h"
#include "toplev.h"
#include "real.h"
#include "tree-simple.h"
#include "flags.h"
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
  dest->ss = src->ss;
  dest->loop = src->loop;
}

/* Initialise a simple expression holder.

   Care must be taken when multiple se are created with the same parent.
   The child se must be kept in sync.  The easiest way is to delay creation
   of a child se until after after the previous se has been translated.  */
void
g95_init_se(g95_se * se, g95_se * parent)
{
  memset(se, 0, sizeof(g95_se));
  g95_init_block (&se->pre);
  g95_init_block (&se->post);

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

/* Ensures the result of the expression as either a temporary variable
   or a constant so that it can be used repeatedly.  */
void
g95_make_safe_expr(g95_se * se)
{
  tree tmp;
  tree var;

  if (TREE_CODE_CLASS (TREE_CODE (se->expr)) == 'c')
    return;

  /* we need a temporary for this result */
  var = g95_create_var (TREE_TYPE(se->expr), NULL);
  tmp = build_v (MODIFY_EXPR, var, se->expr);
  g95_add_expr_to_block (&se->pre, tmp);
  se->expr = var;
}

/* Generate code to initialize a string length variable. Returns the
   value.  */
tree
g95_conv_init_string_length (g95_symbol * sym, stmtblock_t * pblock)
{
  g95_se se;
  tree tmp;

  g95_init_se (&se, NULL);
  g95_conv_expr_type (&se, sym->ts.cl->length, g95_strlen_type_node);
  g95_add_block_to_block (pblock, &se.pre);

  tmp = G95_DECL_STRING_LENGTH (sym->backend_decl);
  tmp = build_v (MODIFY_EXPR, tmp, se.expr);
  g95_add_expr_to_block (pblock, tmp);

  return se.expr;
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
  type = build_pointer_type (type);

  var = NULL_TREE;
  g95_init_se (&start, se);
  g95_conv_expr_type (&start, ref->u.ss.start, g95_strlen_type_node);
  g95_add_block_to_block (&se->pre, &start.pre);

  if (integer_onep (start.expr))
    {
      g95_conv_string_parameter (se);
    }
  else
    {
      /* Change the start of the string.  */
      if (TYPE_STRING_FLAG (TREE_TYPE (se->expr)))
        tmp = se->expr;
      else
        tmp = build1 (INDIRECT_REF, TREE_TYPE (TREE_TYPE (se->expr)), se->expr);
      tmp = build (ARRAY_REF, TREE_TYPE (TREE_TYPE (tmp)), tmp, start.expr);
      se->expr = build1 (ADDR_EXPR, type, tmp);
    }

  /* Length = end + 1 - start.  */
  g95_init_se (&end, se);
  g95_conv_expr_type (&end, ref->u.ss.end, g95_strlen_type_node);
  g95_add_block_to_block (&se->pre, &end.pre);

  tmp = build (MINUS_EXPR, g95_strlen_type_node, integer_one_node, start.expr);
  tmp = build (PLUS_EXPR, g95_strlen_type_node, end.expr, tmp);
  se->string_length = fold (tmp);
}

/* Convert a derived type component reference.  */
static void
g95_conv_component_ref (g95_se * se, g95_ref * ref)
{
  g95_component *c;
  tree tmp;
  tree decl;
  tree field;

  c = ref->u.c.component;

  assert (c->backend_decl);

  field = c->backend_decl;
  assert (TREE_CODE (field) == FIELD_DECL);
  decl= se->expr;
  tmp = build (COMPONENT_REF, TREE_TYPE (field), decl, field);

  if (! c->dimension)
    {
      if (se->want_pointer && ! ref->next)
        {
          if (! c->pointer)
            {
              tmp = build1 (ADDR_EXPR, build_pointer_type (TREE_TYPE (tmp)),
                            tmp);
            }
        }
      else if (c->pointer)
        tmp = build1 (INDIRECT_REF, TREE_TYPE (TREE_TYPE (tmp)), tmp);
    }
  se->expr = tmp;

  if (c->ts.type == BT_CHARACTER)
    {
      tmp = G95_DECL_STRING_LENGTH (field);
      assert (tmp);
      if (! INTEGER_CST_P (tmp))
        g95_todo_error ("Unknown length character component");
      se->string_length = tmp;
    }
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
          se->expr = se->ss->data.scalar.expr;
          se->string_length = se->ss->data.scalar.string_length;
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

      /* Procedure actual arguments.  */
      if (expr->symbol->attr.flavor == FL_PROCEDURE
          && se->expr != current_function_decl)
        {
          assert (se->want_pointer);
          if (! expr->symbol->attr.dummy)
            {
              assert (TREE_CODE (se->expr) == FUNCTION_DECL
                      && DECL_EXTERNAL (se->expr));
              se->expr = build1 (ADDR_EXPR,
                  build_pointer_type (TREE_TYPE (se->expr)), se->expr);
            }
          return;
        }

      /* Special case for assigning the return value of a function.
         Self recursive functions must have an explicit return value.  */
      if (se->expr == current_function_decl && expr->symbol->attr.function
            && (expr->symbol->result == expr->symbol))
       {
         se->expr = g95_get_fake_result_decl(expr->symbol);
         assert (expr->ref == NULL || expr->symbol->attr.dimension);
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

      ref = expr->ref;
    }

  /* For character variables, also get the length.  */
  if (expr->symbol->ts.type == BT_CHARACTER)
    {
      assert (G95_DECL_STRING (se->expr));
      se->string_length = G95_DECL_STRING_LENGTH (se->expr);
      assert (se->string_length);
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
          /* Return the descriptor for array pointers and allocations.  */
          if (se->want_pointer
              && ref->next == NULL
              && (se->descriptor_only || expr->rank > 0))
            return;

          g95_conv_array_ref(se, &ref->u.ar);
          /* Return a pointer to an element.  */
          break;

        case REF_COMPONENT:
          g95_conv_component_ref (se, ref);
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
  /* Pointer assignment, allocation or pass by reference.  Arrays are handled
     seperately.  */
  if (se->want_pointer)
    {
      if (expr->ts.type == BT_CHARACTER)
        g95_conv_string_parameter (se);
      else if (TREE_CODE (se->expr) == INDIRECT_REF)
        se->expr = TREE_OPERAND (se->expr, 0);
      else
        {
          TREE_ADDRESSABLE (se->expr) = 1;
          se->expr = build1 (ADDR_EXPR,
                             build_pointer_type (TREE_TYPE (se->expr)),
                             se->expr);
        }
    }
  if (se->ss != NULL)
    g95_advance_se_ss_chain (se);
}

/* Unary ops are easy... Or they would be if ! was a valid op.  */
static void
g95_conv_unary_op (enum tree_code code, g95_se * se, g95_expr * expr)
{
  g95_se operand;
  tree type;

  assert (expr->ts.type != BT_CHARACTER);
  /* Initialize the operand.  */
  g95_init_se (&operand, se);
  g95_conv_expr_val (&operand, expr->op1);
  g95_add_block_to_block (&se->pre, &operand.pre);

  type = g95_typenode_for_spec(&expr->ts);

 /* TRUTH_NOT_EXPR is not a "true" unary operator in GCC.
    We must convert it to a compare to 0 (e.g. EQ_EXPR (op1, 0)).
    All other unary operators have an equivalent SIMPLE unary operator  */
 if (code == TRUTH_NOT_EXPR)
   se->expr = build (EQ_EXPR, type, operand.expr, integer_zero_node);
 else
   se->expr = build1 (code, type, operand.expr);

}

/* For power op (lhs ** rhs) We generate:
    m = lhs
    if (rhs > 0)
      count = rhs
    else if (rhs == 0)
      {
        count = 0
        m = 1
      }
    else // (rhs < 0)
      {
        count = -rhs
        m = 1 / m;
      }
    // for constant rhs we do the above at compile time
    val = m;
    for (n = 1; n < count; n++)
      val = val * m;
 */
static void
g95_conv_integer_power (g95_se *se, tree lhs, tree rhs)
{
  tree count;
  tree result;
  tree cond;
  tree neg_stmt;
  tree pos_stmt;
  tree tmp;
  tree var;
  tree type;
  stmtblock_t block;
  tree exit_label;

  type = TREE_TYPE (lhs);

  if (INTEGER_CST_P (rhs))
    {
      if (integer_zerop (rhs))
        {
          se->expr = g95_build_const (type, integer_one_node);
          return;
        }
      /* Special cases for constant values.  */
      if (TREE_INT_CST_HIGH (rhs) == -1)
        {
          /* x ** (-y) == 1 / (x ** y).  */
          if (TREE_CODE (type) == INTEGER_TYPE)
            {
              se->expr = integer_zero_node;
              return;
            }

          tmp = g95_build_const (type, integer_one_node);
          lhs = fold (build (RDIV_EXPR, type, tmp, lhs));

          rhs = fold (build1 (NEGATE_EXPR, TREE_TYPE (rhs), rhs));
          assert (INTEGER_CST_P (rhs));
        }
      else
        {
          /* TODO: really big integer powers.  */
          assert (TREE_INT_CST_HIGH (rhs) == 0);
        }

      if (integer_onep (rhs))
        {
          se->expr = lhs;
          return;
        }
      if (TREE_INT_CST_LOW (rhs) == 2)
        {
          se->expr = build (MULT_EXPR, type, lhs, lhs);
          return;
        }
      if (TREE_INT_CST_LOW (rhs) == 3)
        {
          tmp = build (MULT_EXPR, type, lhs, lhs);
          se->expr = fold (build (MULT_EXPR, type, tmp, lhs));
          return;
        }

      /* Create the loop count variable.  */
      count = g95_create_var (TREE_TYPE (rhs), "count");
      g95_add_modify_expr (&se->pre, count, rhs);
    }
  else
    {
      /* Put the lhs into a temporary variable.  */
      var = g95_create_var (type, "val");
      count = g95_create_var (TREE_TYPE (rhs), "count");
      g95_add_modify_expr (&se->pre, var, lhs);
      lhs = var;

      /* Generate code for negative rhs.  */
      g95_start_block (&block);

      if (TREE_CODE (TREE_TYPE (lhs)) == INTEGER_TYPE)
        {
          g95_add_modify_expr (&block, lhs, integer_zero_node);
          g95_add_modify_expr (&block, count, integer_zero_node);
        }
      else
        {
          tmp = g95_build_const (type, integer_one_node);
          tmp = build (RDIV_EXPR, type, tmp, lhs);
          g95_add_modify_expr (&block, var, tmp);

          tmp = build1 (NEGATE_EXPR, TREE_TYPE (rhs), rhs);
          g95_add_modify_expr (&block, count, tmp);
        }
      neg_stmt = g95_finish_block (&block);

      pos_stmt = build_v (MODIFY_EXPR, count, rhs);

      /* Code for rhs == 0.  */
      g95_start_block (&block);

      g95_add_modify_expr (&block, count, integer_zero_node);
      tmp = g95_build_const (type, integer_one_node);
      g95_add_modify_expr (&block, lhs, tmp);

      tmp = g95_finish_block (&block);

      /* Select the appropriate action.  */
      cond = build (EQ_EXPR, TREE_TYPE (rhs), rhs, integer_zero_node);
      tmp = build_v (COND_EXPR, cond, tmp, neg_stmt);

      cond = build (GT_EXPR, TREE_TYPE (rhs), rhs, integer_zero_node);
      tmp = build_v (COND_EXPR, cond, pos_stmt, tmp);
      g95_add_expr_to_block (&se->pre, tmp);
    }

  /* Create a variable for the result.  */
  result = g95_create_var (type, "pow");
  g95_add_modify_expr (&se->pre, result, lhs);

  exit_label = g95_build_label_decl (NULL_TREE);
  TREE_USED (exit_label) = 1;

  /* Create the loop body.  */
  g95_start_block (&block);

  /* First the exit condition (until count <= 1).  */
  tmp = build_v (GOTO_EXPR, exit_label);
  cond = build (LE_EXPR, TREE_TYPE (count), count, integer_one_node);
  tmp = build_v (COND_EXPR, cond, tmp, empty_stmt_node);
  g95_add_expr_to_block (&block, tmp);

  /* Multiply by the lhs.  */
  tmp = build (MULT_EXPR, type, result, lhs);
  tmp = build (MODIFY_EXPR, type, result, tmp);
  g95_add_expr_to_block (&block, tmp);

  /* Adjust the loop count.  */
  tmp = build (MINUS_EXPR, TREE_TYPE (count), count, integer_one_node);
  g95_add_modify_expr (&block, count, tmp);

  tmp = g95_finish_block (&block);

  /* Create the the loop.  */
  tmp = build_v (LOOP_EXPR, tmp);
  g95_add_expr_to_block (&se->pre, tmp);

  /* Add the exit label.  */
  tmp = build_v (LABEL_EXPR, exit_label);
  g95_add_expr_to_block (&se->pre, tmp);

  se->expr = result;
}

/* Power op (**).  Integer rhs has special handling.  */
static void
g95_conv_power_op (g95_se * se, g95_expr * expr)
{
  int kind;
  g95_se lse;
  g95_se rse;
  tree fndecl;
  tree tmp;
  tree type;

  g95_init_se (&lse, se);
  g95_conv_expr_val (&lse, expr->op1);
  g95_add_block_to_block (&se->pre, &lse.pre);

  g95_init_se (&rse, se);
  g95_conv_expr_val (&rse, expr->op2);
  g95_add_block_to_block (&se->pre, &rse.pre);

  type = TREE_TYPE (lse.expr);

  kind = expr->op1->ts.kind;
  switch (expr->op2->ts.type)
    {
    case BT_INTEGER:
      /* Integer powers are expanded inline as multiplications.  */
      g95_conv_integer_power (se, lse.expr, rse.expr);
      return;

    case BT_REAL:
      switch (kind)
        {
        case 4: fndecl = gfor_fndecl_math_powf; break;
        case 8: fndecl = gfor_fndecl_math_pow; break;
        default: abort();
        }
      break;

    case BT_COMPLEX:
      switch (kind)
        {
        case 4: fndecl = gfor_fndecl_math_cpowf; break;
        case 8: fndecl = gfor_fndecl_math_cpow; break;
        default: abort();
        }
      break;

    default:
      abort();
      break;
    }

  tmp = g95_chainon_list (NULL_TREE, lse.expr);
  tmp = g95_chainon_list (tmp, rse.expr);
  se->expr = g95_build_function_call (fndecl, tmp);
}

/* Generate code to allocate a string temporary.  */
tree
g95_conv_string_tmp (g95_se * se, tree type, tree len)
{
  tree var;
  tree tmp;
  tree args;
  tree addr;

  if (g95_can_put_var_on_stack (len))
    {
      /* Create a temporary variable to hold the result.  */
      tmp = fold (build (MINUS_EXPR, TREE_TYPE (len), len, integer_one_node));
      tmp = build_range_type (g95_array_index_type, integer_zero_node, tmp);
      tmp = build_array_type (g95_character1_type_node, tmp);
      var = g95_create_var (tmp, "str");
      TREE_ADDRESSABLE (var) = 1;
      var = build1 (ADDR_EXPR, type, var);
    }
  else
    {
      var = g95_create_var (type, "pstr");

      TREE_ADDRESSABLE (var) = 1;

      /* Allocate a temporary to hold the result.  */
      addr = build1 (ADDR_EXPR, ppvoid_type_node, var);

      args = NULL_TREE;
      args = g95_chainon_list (args, addr);
      args = g95_chainon_list (args, len);
      tmp = g95_build_function_call (gfor_fndecl_internal_malloc, args);
      g95_add_expr_to_block (&se->pre, tmp);

      /* Free the temporary afterwards.  */
      args = g95_chainon_list (NULL_TREE, addr);
      tmp = g95_build_function_call (gfor_fndecl_internal_free, args);
      g95_add_expr_to_block (&se->post, tmp);
    }

  return var;
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

  assert (expr->op1->ts.type == BT_CHARACTER
      && expr->op2->ts.type == BT_CHARACTER);

  g95_init_se (&lse, se);
  g95_conv_expr (&lse, expr->op1);
  g95_conv_string_parameter (&lse);
  g95_init_se (&rse, se);
  g95_conv_expr (&rse, expr->op2);
  g95_conv_string_parameter (&rse);

  g95_add_block_to_block (&se->pre, &lse.pre);
  g95_add_block_to_block (&se->pre, &rse.pre);

  type = g95_get_character_type (expr->ts.kind, expr->ts.cl);
  if (G95_KNOWN_SIZE_STRING_TYPE (type))
    {
      len = TYPE_MAX_VALUE (TYPE_DOMAIN (type));
    }
  else
    {
      len = fold (build (PLUS_EXPR, TREE_TYPE (lse.string_length),
                         lse.string_length, rse.string_length));
    }

  type = build_pointer_type (type);

  var = g95_conv_string_tmp (se, type, len);

  /* Do the actual concatenation.  */
  args = NULL_TREE;
  args = g95_chainon_list (args, len);
  args = g95_chainon_list (args, var);
  args = g95_chainon_list (args, lse.string_length);
  args = g95_chainon_list (args, lse.expr);
  args = g95_chainon_list (args, rse.string_length);
  args = g95_chainon_list (args, rse.expr);
  tmp = g95_build_function_call (gfor_fndecl_concat_string, args);
  g95_add_expr_to_block (&se->pre, tmp);

  /* Add the cleanup for the operands.  */
  g95_add_block_to_block (&se->pre, &rse.post);
  g95_add_block_to_block (&se->pre, &lse.post);

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
  tree tmp;
  int lop;
  int checkstring;

  checkstring = 0;
  lop = 0;
  switch (expr->operator)
    {
    case INTRINSIC_UPLUS:
      g95_conv_expr (se, expr->op1);
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
      lop = 1;
      break;

    case INTRINSIC_OR:
      code = BIT_IOR_EXPR;
      lop = 1;
      break;

    /* EQV and NEQV only work on logicals, but since we represent them
       as integers, we can use EQ_EXPR and NE_EXPR for them in SIMPLE.  */
    case INTRINSIC_EQ:
    case INTRINSIC_EQV:
      code = EQ_EXPR;
      checkstring = 1;
      lop = 1;
      break;

    case INTRINSIC_NE:
    case INTRINSIC_NEQV:
      code = NE_EXPR;
      checkstring = 1;
      lop = 1;
      break;

    case INTRINSIC_GT:
      code = GT_EXPR;
      checkstring = 1;
      lop = 1;
      break;

    case INTRINSIC_GE:
      code = GE_EXPR;
      checkstring = 1;
      lop = 1;
      break;

    case INTRINSIC_LT:
      code = LT_EXPR;
      checkstring = 1;
      lop = 1;
      break;

    case INTRINSIC_LE:
      code = LE_EXPR;
      checkstring = 1;
      lop = 1;
      break;

    case INTRINSIC_USER:
    case INTRINSIC_ASSIGN:
      /* These should be converted into function calls by the frontend.  */
      abort ();
      return;

    default:
      fatal_error ("Unknown intrinsic op");
      return;
    }

  /* The only exception to this is **, which is handled seperately anyway.  */
  assert (expr->op1->ts.type == expr->op2->ts.type);

  if (checkstring && expr->op1->ts.type != BT_CHARACTER)
    checkstring = 0;

  /* lhs */
  g95_init_se (&lse, se);
  g95_conv_expr (&lse, expr->op1);
  g95_add_block_to_block (&se->pre, &lse.pre);

  /* rhs */
  g95_init_se (&rse, se);
  g95_conv_expr (&rse, expr->op2);
  g95_add_block_to_block (&se->pre, &rse.pre);

  /* For string comparisons we generate a library call, and compare the return
     value with 0.  */
  if (checkstring)
    {
      g95_conv_string_parameter (&lse);
      g95_conv_string_parameter (&rse);
      tmp = NULL_TREE;
      tmp = g95_chainon_list (tmp, lse.string_length);
      tmp = g95_chainon_list (tmp, lse.expr);
      tmp = g95_chainon_list (tmp, rse.string_length);
      tmp = g95_chainon_list (tmp, rse.expr);

      /* Build a call for the comparison.  */
      lse.expr = g95_build_function_call (gfor_fndecl_compare_string, tmp);
      g95_add_block_to_block (&lse.post, &rse.post);

      rse.expr = integer_zero_node;
    }

  type = g95_typenode_for_spec (&expr->ts);

  if (lop)
    {
      /* The result of logical ops is always boolean_type_node.  */
      tmp = build (code, type, lse.expr, rse.expr);
      se->expr = convert (type, tmp);
    }
  else
    se->expr = build (code, type, lse.expr, rse.expr);


  /* Add the post blocks.  */
  g95_add_block_to_block (&se->post, &rse.post);
  g95_add_block_to_block (&se->post, &lse.post);
}

static void
g95_conv_function_val (g95_se * se, g95_symbol * sym)
{
  tree tmp;

  if (sym->attr.dummy)
    {
      tmp = g95_get_symbol_decl (sym);
      assert (TREE_CODE (TREE_TYPE (tmp)) == POINTER_TYPE
             && TREE_CODE (TREE_TYPE (TREE_TYPE (tmp))) == FUNCTION_TYPE);

      se->expr = tmp;
    }
  else
    {
      if (! sym->backend_decl)
        sym->backend_decl = g95_get_extern_function_decl (sym);

      tmp = sym->backend_decl;
      assert (TREE_CODE (tmp) == FUNCTION_DECL);
      se->expr = build1 (ADDR_EXPR, build_pointer_type (TREE_TYPE (tmp)), tmp);
    }
}

/* Generate code for a procedure call.  Note can return se->post != NULL.
   If se->direct_byref is set then se->expr contains the return parameter.  */
void
g95_conv_function_call (g95_se * se, g95_symbol * sym,
                        g95_actual_arglist * arg)
{
  tree arglist;
  tree tmp;
  tree fntype;
  g95_se parmse;
  g95_ss *argss;
  g95_ss_info *info;
  int byref;
  tree type;
  tree var;
  tree len;

  arglist = NULL_TREE;
  var = NULL_TREE;
  len = NULL_TREE;

  if (se->ss != NULL)
    {
      if (! sym->attr.elemental)
        {
          switch (se->ss->type)
            {
            case G95_SS_SCALAR:
              /* We've already got the return value of this function.  */
              se->expr = se->ss->data.scalar.expr;
              se->string_length = se->ss->data.scalar.string_length;
              g95_advance_se_ss_chain (se);
              return;

            case G95_SS_FUNCTION:
              if (se->ss->useflags)
                {
                  assert (g95_return_by_reference (sym) && sym->attr.dimension);
                  assert (se->loop != NULL);

                  /* Access the previously obtained result.  */
                  g95_conv_tmp_array_ref (se);
                  g95_advance_se_ss_chain (se);
                  return;
                }
              break;

            default:
              abort();
            }
        }
      info = &se->ss->data.info;
    }
  else
    info = NULL;

  byref = g95_return_by_reference (sym);
  if (byref)
    {
      if (se->direct_byref)
        arglist = g95_chainon_list (arglist, se->expr);
      else if (sym->attr.dimension)
        {
          assert (se->loop && se->ss);
          /* Set the type of the array.  */
          tmp = g95_typenode_for_spec (&sym->ts);
          info->dimen = se->loop->dimen;
          /* Allocate a temporary to store the result.  */
          g95_trans_allocate_temp_array (se->loop, info, tmp, NULL_TREE);

          /* Zero the first stride to indicate a temporary.  */
          tmp = g95_conv_descriptor_stride (info->descriptor, g95_rank_cst[0]);
          tmp = build (MODIFY_EXPR, TREE_TYPE (tmp), tmp, integer_zero_node);
          g95_add_expr_to_block (&se->pre, tmp);
          /* Pass the temporary as the first argument.  */
          tmp = info->descriptor;
          tmp = build1 (ADDR_EXPR, build_pointer_type (TREE_TYPE (tmp)), tmp);
          arglist = g95_chainon_list (arglist, tmp);
        }
      else if (sym->ts.type == BT_CHARACTER)
        {
          type = g95_get_character_type (sym->ts.kind, sym->ts.cl);
          assert (G95_KNOWN_SIZE_STRING_TYPE (type));
          len = TYPE_MAX_VALUE (TYPE_DOMAIN (type));
          type = build_pointer_type (type);

          if (g95_can_put_var_on_stack (len))
            {
              /* Create a temporary variable to hold the result.  */
              tmp = fold (build (MINUS_EXPR, TREE_TYPE (len), len,
                          integer_one_node));
              tmp = build_range_type (g95_array_index_type, integer_zero_node,
                                      tmp);
              type = build_array_type (g95_character1_type_node, tmp);
              tmp = g95_create_var (type, "str");
              TREE_ADDRESSABLE (tmp) = 1;
              var = build1 (ADDR_EXPR, build_pointer_type(TREE_TYPE (tmp)),
                            tmp);
            }
          else
            {
              tree addr;
              tree args;

              var = g95_create_var (type, "pstr");
              TREE_ADDRESSABLE (var) = 1;

              /* Allocate a temporary to hold the result.  */
              addr = build1 (ADDR_EXPR, ppvoid_type_node, var);

              args = NULL_TREE;
              args = g95_chainon_list (args, addr);
              args = g95_chainon_list (args, len);
              tmp = g95_build_function_call (gfor_fndecl_internal_malloc, args);
              g95_add_expr_to_block (&se->pre, tmp);

              /* Free the temporary afterwards.  */
              args = g95_chainon_list (NULL_TREE, addr);
              tmp = g95_build_function_call (gfor_fndecl_internal_free, args);
              g95_add_expr_to_block (&se->post, tmp);
            }
          arglist = g95_chainon_list (arglist, var);
        }
      else /* TODO: derived type function return values.  */
        abort ();
    }

  /* Evaluate the arguments.  */
  for (; arg != NULL; arg = arg->next)
    {
      if (arg->expr == NULL)
        {

          if (se->ignore_optional)
            {
              /* Some intrinsics have already been resolved to the correct
                 parameters.  */
              continue;
            }
          else if (arg->label)
            {
              /* We don't do alternate returns yet.  */
              abort ();
            }
          else
            {
              /* Pass a NULL pointer for an absent arg.  */
              g95_init_se (&parmse, NULL);
              parmse.expr = null_pointer_node;
              /* TODO: passing optional character type parameters.  */
            }
        }
      else if (se->ss && se->ss->useflags)
        {
          g95_init_se (&parmse, se);

          /* An elemental function inside a scalarized loop.  */
          g95_conv_expr (&parmse, arg->expr);
        }
      else
        {
          /* A scalar or transformational function.  */
          g95_init_se (&parmse, NULL);
          argss = g95_walk_expr (g95_ss_terminator, arg->expr);

          if (argss == g95_ss_terminator)
            g95_conv_expr_reference (&parmse, arg->expr);
          else
            {
              parmse.want_pointer = 1;
              g95_conv_array_parameter (&parmse, arg->expr, argss);
            }
        }

      g95_add_block_to_block (&se->pre, &parmse.pre);
      g95_add_block_to_block (&se->post, &parmse.post);

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

/* A pure function may still have side-effects - it may modify its
   parameters.  */
  TREE_SIDE_EFFECTS (se->expr) = 1;
#if 0
  if (! sym->attr.pure)
    TREE_SIDE_EFFECTS (se->expr) = 1;
#endif

  if (byref && ! se->direct_byref)
    {
      g95_add_expr_to_block (&se->pre, se->expr);

      if (sym->attr.dimension)
        {
          if (flag_bounds_check)
            {
              /* Check the data pointer hasn't been modified.  This would happen
                 in a function returning a pointer.  */
              tmp = g95_conv_descriptor_data (info->descriptor);
              tmp = build (NE_EXPR, boolean_type_node, tmp, info->data);
              g95_trans_runtime_check (tmp, g95_strconst_fault, &se->pre);
            }
          se->expr = info->descriptor;
        }
      else if (sym->ts.type == BT_CHARACTER)
        {
          se->expr = var;
          se->string_length = len;
        }
      else
        abort ();
    }
}

/* Translate a function expression.  */
static void
g95_conv_function_expr (g95_se * se, g95_expr * expr)
{
  if (expr->value.function.isym)
    {
      g95_conv_intrinsic_function (se, expr);
      return;
    }

  g95_conv_function_call (se, expr->symbol, expr->value.function.actual);
}

static void
g95_conv_array_constructor_expr (g95_se * se, g95_expr * expr)
{
  assert (se->ss != NULL && se->ss != g95_ss_terminator);
  assert (se->ss->expr == expr && se->ss->type == G95_SS_CONSTRUCTOR);

  g95_conv_tmp_array_ref (se);
  g95_advance_se_ss_chain (se);
}

/* Entry point for expression translation.  */
void
g95_conv_expr (g95_se * se, g95_expr * expr)
{
  switch (expr->expr_type)
    {
    case EXPR_OP:
      g95_conv_expr_op (se, expr);
      break;

    case EXPR_FUNCTION:
      g95_conv_function_expr (se, expr);
      break;

    case EXPR_CONSTANT:
      g95_conv_constant (se, expr);
      break;

    case EXPR_VARIABLE:
      g95_conv_variable (se, expr);
      break;

    case EXPR_NULL:
      g95_todo_error ("EXPR_NULL");
      break;

    case EXPR_SUBSTRING:
      g95_todo_error ("EXPR_SUBSTRING");
      break;

    case EXPR_STRUCTURE:
      g95_todo_error ("EXPR_STRUCTURE");
      break;

    case EXPR_ARRAY:
      g95_conv_array_constructor_expr (se, expr);
      break;

    default:
      abort();
      break;
    }
}

void
g95_conv_expr_lhs (g95_se * se, g95_expr * expr)
{
  g95_conv_expr (se, expr);
  /* AFAICS all numeric lvalues have empty post chains.  If not we need to
     figure out a way of rewriting an lvalue so that it has no post chain.  */
  if (se->post.head && expr->ts.type != BT_CHARACTER)
    g95_todo_error ("LHS with post chain");
}

void
g95_conv_expr_val (g95_se * se, g95_expr * expr)
{
  tree val;
  tree tmp;

  assert (expr->ts.type != BT_CHARACTER);
  g95_conv_expr (se, expr);
  if (se->post.head)
    {
      val = g95_create_var (TREE_TYPE (se->expr), NULL);
      tmp = build_v (MODIFY_EXPR, val, se->expr);
      g95_add_expr_to_block (&se->pre, tmp);
    }
}

void
g95_conv_expr_type (g95_se * se, g95_expr * expr, tree type)
{
  g95_conv_expr_val (se, expr);
  se->expr = convert (type, se->expr);
}

void
g95_conv_expr_reference (g95_se * se, g95_expr * expr)
{
  tree var;
  tree tmp;

  if (expr->ts.type == BT_CHARACTER)
    {
      g95_conv_expr (se, expr);
      g95_conv_string_parameter (se);
      return;
    }

  if (expr->expr_type == EXPR_VARIABLE)
    {
      se->want_pointer = 1;
      g95_conv_expr (se, expr);
      if (se->post.head)
        {
          var = g95_create_var (TREE_TYPE (se->expr), NULL);
          tmp = build_v (MODIFY_EXPR, var, se->expr);
          g95_add_expr_to_block (&se->pre, tmp);
          g95_add_block_to_block (&se->pre, &se->post);
          se->expr = var;
        }
      return;
    }

  g95_conv_expr (se, expr);

  /* Create a temporary var to hold the value.  */
  var = g95_create_var (TREE_TYPE (se->expr), NULL);
  tmp = build_v (MODIFY_EXPR, var, se->expr);
  g95_add_expr_to_block (&se->pre, tmp);
  g95_add_block_to_block (&se->pre, &se->post);

  /* Take the address of that value.  */
  se->expr = build1 (ADDR_EXPR, build_pointer_type (TREE_TYPE (var)), var);
}

tree
g95_trans_pointer_assign (g95_code * code)
{
  g95_se lse;
  g95_se rse;
  g95_ss *lss;
  g95_ss *rss;
  stmtblock_t block;

  g95_start_block (&block);

  g95_init_se (&lse, NULL);

  lss = g95_walk_expr (g95_ss_terminator, code->expr);
  rss = g95_walk_expr (g95_ss_terminator, code->expr2);
  if (lss == g95_ss_terminator)
    {
      lse.want_pointer = 1;
      g95_conv_expr (&lse, code->expr);
      assert (rss == g95_ss_terminator);
      g95_init_se (&rse, NULL);
      rse.want_pointer = 1;
      g95_conv_expr (&rse, code->expr2);
      g95_add_block_to_block (&block, &lse.pre);
      g95_add_block_to_block (&block, &rse.pre);
      g95_add_modify_expr (&block, lse.expr, rse.expr);
      g95_add_block_to_block (&block, &rse.post);
      g95_add_block_to_block (&block, &lse.post);
    }
  else
    {
      g95_conv_array_parameter (&lse, code->expr2, lss);
      lse.direct_byref = 1;
      g95_conv_array_parameter (&lse, code->expr2, rss);
      g95_add_block_to_block (&block, &lse.pre);
      g95_add_block_to_block (&block, &lse.post);
    }
  return g95_finish_block (&block);
}

/* Get the decl for the length of a string from an expression.  */
tree
g95_conv_string_length (tree expr)
{
  /* TODO: string lengths of components.  */
  while (TREE_CODE (expr) == INDIRECT_REF)
    expr = TREE_OPERAND (expr, 0);

  if (!(DECL_P (expr) && G95_DECL_STRING (expr)))
    return NULL_TREE;

  return G95_DECL_STRING_LENGTH (expr);
}

/* Makes sure se is suitable for passing as a function string parameter.  */
void
g95_conv_string_parameter (g95_se * se)
{
  tree type;

  if (TREE_CODE (se->expr) == STRING_CST)
    {
      se->expr = build1 (ADDR_EXPR, pchar_type_node, se->expr);
      return;
    }

  type = TREE_TYPE (se->expr);
  if (TYPE_STRING_FLAG (type))
    {
      assert (TREE_CODE (se->expr) == VAR_DECL
              || TREE_CODE (se->expr) == COMPONENT_REF);
      TREE_ADDRESSABLE (se->expr) = 1;
      se->expr = build1 (ADDR_EXPR, build_pointer_type (type), se->expr);
      se->expr = fold (convert (pchar_type_node, se->expr));
    }

  assert (POINTER_TYPE_P (TREE_TYPE (se->expr)));
  assert (se->string_length
          && TREE_CODE (TREE_TYPE (se->string_length)) == INTEGER_TYPE);
}

/* Generate code for assignment of scalar variables.  Includes character
   strings.  */
tree
g95_trans_scalar_assign (g95_se * lse, g95_se * rse, bt type)
{
  tree tmp;
  tree args;
  stmtblock_t block;

  g95_init_block (&block);


  if (type == BT_CHARACTER)
    {
      args = NULL_TREE;

      assert (lse->string_length != NULL_TREE
              && rse->string_length != NULL_TREE);

      g95_conv_string_parameter (lse);
      g95_conv_string_parameter (rse);

      g95_add_block_to_block (&block, &lse->pre);
      g95_add_block_to_block (&block, &rse->pre);

      args = g95_chainon_list (args, lse->string_length);
      args = g95_chainon_list (args, lse->expr);
      args = g95_chainon_list (args, rse->string_length);
      args = g95_chainon_list (args, rse->expr);

      tmp = g95_build_function_call (gfor_fndecl_copy_string, args);
      g95_add_expr_to_block (&block, tmp);
    }
  else
    {
      g95_add_block_to_block (&block, &lse->pre);
      g95_add_block_to_block (&block, &rse->pre);

      g95_add_modify_expr (&block, lse->expr, rse->expr);
    }

  g95_add_block_to_block (&block, &lse->post);
  g95_add_block_to_block (&block, &rse->post);

  return g95_finish_block (&block);
}

/* Try to translate array(:) = func (...), where func is a transformational
   array function, without using a temporary.  Returns NULL is this isn't the
   case.  */
static tree
g95_trans_arrayfunc_assign (g95_expr * expr1, g95_expr * expr2)
{
  g95_se se;
  g95_ss *ss;
  /* The caller has already checked rank>0 and expr_type == EXPR_FUNCTION.  */

  /* Elemental functions don't need a temporary anyway.  */
  if (expr2->symbol->attr.elemental)
    return NULL;

  if (expr2->value.function.isym
      && ! g95_is_intrinsic_libcall (expr2))
    return NULL;

  /* Check for a dependency.  */
  if (g95_check_fncall_dependency (expr1, expr2))
    return NULL;

  /* The frontend doesn't seem to bother filling in expr->symbol for intrinsic
     functions.  */
  assert (expr2->value.function.isym ||
          (g95_return_by_reference (expr2->symbol)
           && expr2->symbol->attr.dimension));

  ss = g95_walk_expr (g95_ss_terminator, expr1);
  assert (ss != g95_ss_terminator);
  g95_init_se (&se, NULL);
  g95_start_block (&se.pre);
  se.want_pointer = 1;

  g95_conv_array_parameter (&se, expr1, ss);

  se.direct_byref = 1;
  se.ss = g95_walk_expr (g95_ss_terminator, expr2);
  assert (se.ss != g95_ss_terminator);
  g95_conv_function_expr (&se, expr2);
  g95_add_expr_to_block (&se.pre, se.expr);
  g95_add_block_to_block (&se.pre, &se.post);

  return g95_finish_block (&se.pre);
}

/* Translate an assignment.  Most of the code is concerned with
   setting up the scalarizer.  */
tree
g95_trans_assignment (g95_expr * expr1, g95_expr * expr2)
{
  g95_se lse;
  g95_se rse;
  g95_ss *lss;
  g95_ss *lss_section;
  g95_ss *rss;
  g95_loopinfo loop;
  tree tmp;
  stmtblock_t block;
  stmtblock_t body;

  /* Special case a single function returning an array.  */
  if (expr2->expr_type == EXPR_FUNCTION && expr2->rank > 0)
    {
      tmp = g95_trans_arrayfunc_assign (expr1, expr2);
      if (tmp)
        return tmp;
    }

  /* Assignment of the form lhs = rhs.  */
  g95_start_block (&block);

  g95_init_se (&lse, NULL);
  g95_init_se (&rse, NULL);

  /* Walk the lhs.  */
  lss = g95_walk_expr (g95_ss_terminator, expr1);
  rss = NULL;
  if (lss != g95_ss_terminator)
    {
      /* The assignment needs scalarization.  */
      lss_section = lss;

      /* Find a non-scalar SS from the lhs.  */
      while (lss_section != g95_ss_terminator
             && lss_section->type != G95_SS_SECTION)
        lss_section = lss_section->next;

      assert (lss_section != g95_ss_terminator);

      /* Initialize the scalarizer.  */
      g95_init_loopinfo (&loop);

      /* Walk the rhs.  */
      rss = g95_walk_expr (g95_ss_terminator, expr2);
      if (rss == g95_ss_terminator)
        {
          /* The rhs is scalar.  Add a ss for the expression.  */
          rss = g95_get_ss ();
          rss->next = g95_ss_terminator;
          rss->type = G95_SS_SCALAR;
          rss->expr = expr2;
        }
      /* Associate the SS with the loop.  */
      g95_add_ss_to_loop (&loop, lss);
      g95_add_ss_to_loop (&loop, rss);

      /* Calculate the bounds of the scalarization.  */
      g95_conv_ss_startstride (&loop);
      /* Resolve any data dependencies in the statement.  */
      g95_conv_resolve_dependencies (&loop, lss_section, rss);
      /* Setup the scalarizing loops.  */
      g95_conv_loop_setup (&loop);

      /* Setup the g95_se structures.  */
      g95_copy_loopinfo_to_se (&lse, &loop);
      g95_copy_loopinfo_to_se (&rse, &loop);

      rse.ss = rss;
      g95_mark_ss_chain_used (rss, 1);
      if (loop.temp_ss == NULL)
        {
          lse.ss = lss;
          g95_mark_ss_chain_used (lss, 1);
        }
      else
        {
          lse.ss = loop.temp_ss;
          g95_mark_ss_chain_used (lss, 3);
          g95_mark_ss_chain_used (loop.temp_ss, 3);
        }

      /* Start the scalarized loop body.  */
      g95_start_scalarized_body (&loop, &body);
    }
  else
    g95_init_block (&body);

  /* Translate the expression.  */
  g95_conv_expr (&rse, expr2);

  if (lss != g95_ss_terminator && loop.temp_ss != NULL)
    {
      g95_conv_tmp_array_ref (&lse);
      g95_advance_se_ss_chain (&lse);
    }
  else
    g95_conv_expr (&lse, expr1);

  tmp = g95_trans_scalar_assign (&lse, &rse, expr1->ts.type);
  g95_add_expr_to_block (&body, tmp);

  if (lss == g95_ss_terminator)
    {
      /* Use the scalar assignment as is.  */
      g95_add_block_to_block (&block, &body);
    }
  else
    {
      if (lse.ss != g95_ss_terminator)
        abort ();
      if (rse.ss != g95_ss_terminator)
        abort ();

      if (loop.temp_ss != NULL)
        {
          g95_trans_scalarized_loop_boundary (&loop, &body);

          /* We need to copy the temporary to the actual lhs.  */
          g95_init_se (&lse, NULL);
          g95_init_se (&rse, NULL);
          g95_copy_loopinfo_to_se (&lse, &loop);
          g95_copy_loopinfo_to_se (&rse, &loop);

          rse.ss = loop.temp_ss;
          lse.ss = lss;

          g95_conv_tmp_array_ref (&rse);
          g95_advance_se_ss_chain (&rse);
          g95_conv_expr (&lse, expr1);

          if (lse.ss != g95_ss_terminator)
            abort ();

          if (rse.ss != g95_ss_terminator)
            abort ();

          tmp = g95_trans_scalar_assign (&lse, &rse, expr1->ts.type);
          g95_add_expr_to_block (&body, tmp);
        }
      /* Generate the copying loops.  */
      g95_trans_scalarizing_loops (&loop, &body);

      /* Wrap the whole thing up.  */
      g95_add_block_to_block (&block, &loop.pre);
      g95_add_block_to_block (&block, &loop.post);

      g95_cleanup_loop (&loop);
    }

  return g95_finish_block (&block);
}

tree
g95_trans_assign (g95_code * code)
{
  return g95_trans_assignment (code->expr, code->expr2);
}

