/* Statement translation
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

/* trans-stmt.c-- generate GCC trees from g95_code */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tree.h"
#include "tree-simple.h"
#include <stdio.h>
#include "c-common.h"
#include "ggc.h"
#include "toplev.h"
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

tree
g95_trans_label_here (g95_code *code)
{
  return build_v (LABEL_EXPR, g95_get_label_decl (code->here));
}

tree
g95_trans_goto (g95_code *code)
{
  return build_v (GOTO_EXPR, g95_get_label_decl (code->label));
}

tree
g95_trans_call (g95_code *code)
{
  g95_se se;
  g95_symbol sym;
  g95_symbol *psym;

  /* IO functions need special handling.  */
  if (strncmp (code->sub_name, "_io_", 4) == 0)
    return g95_trans_io_call (code);

  if (strncmp (code->sub_name, "_gforio_", 8) == 0)
    return g95_trans_iostate_call (code);

  g95_init_se (&se, NULL);
  g95_start_block (&se.pre);

  psym = code->sym;
  if (psym == NULL)
    {
      /* The frontend does not create a symbol for some functions, so we
         create one now.  */
      memset (&sym, 0, sizeof (g95_symbol));

      sym.ts.type = BT_PROCEDURE;
      strcpy (sym.name, code->sub_name);
      sym.attr.external = 1;
      sym.attr.subroutine = 1;
      sym.attr.proc = PROC_EXTERNAL;
      sym.attr.flavor = FL_PROCEDURE;

      psym = &sym;
    }
  else if (strcmp (psym->name, code->sub_name) != 0)
    {
      sym = *psym;
      psym = &sym;
      strcpy (psym->name, code->sub_name);
      psym->backend_decl = NULL_TREE;
    }

  g95_conv_function_call (&se, psym, code->ext.actual);

  TREE_SIDE_EFFECTS (se.expr) = 1;
  g95_add_expr_to_block (&se.pre, se.expr);
  g95_add_block_to_block (&se.pre, &se.post);

  return g95_finish_block (&se.pre);
}

tree
g95_trans_return (g95_code *code ATTRIBUTE_UNUSED)
{
  return build_v (GOTO_EXPR, g95_get_return_label ());
}

tree
g95_trans_stop (g95_code *code)
{
  g95_se se;
  tree arg;
  tree tmp;

  g95_init_se (&se, NULL);
  g95_start_block (&se.pre);

  if (code->label != NULL)
    {
      if (code->expr != NULL)
        {
          g95_conv_expr_type (&se, code->expr, g95_int4_type_node);
          arg = se.expr;
        }
      else
        arg = build_int_2 (code->label->value, 0);
    }
  else
    arg = integer_zero_node;

  arg = tree_cons (NULL_TREE, arg, NULL_TREE);
  tmp = g95_build_function_call (gfor_fndecl_stop, arg);
  g95_add_expr_to_block (&se.pre, tmp);

  tmp = g95_finish_block (&se.pre);

  return tmp;
}


/* Generate SIMPLE for the IF construct. This function also deals with
   the simple IF statement, because the front end translates the IF
   statement into an IF construct.

   We translate:

        IF (cond) THEN
           then_clause
        ELSEIF (cond2)
           elseif_clause
        ELSE
           else_clause
        ENDIF

   into:

        pre_cond_s;
        if (cond_s)
          {
            then_clause;
          }
        else
          {
            pre_cond_s
            if (cond_s)
              {
                elseif_clause
              }
            else
              {
                else_clause;
              }
          }

   where COND_S is the simplified version of the predicate. PRE_COND_S
   are the pre side-effects produced by the translation of the
   conditional.  */
tree
g95_trans_if (g95_code * code)
{
  g95_se if_se;
  tree top, stmt, tail, ifstmt;

  top = tail = NULL_TREE;

  /* Ignore the top EXEC_IF, it only announces an IF construct. The
     actual code we must translate is in code->block.  */
  code = code->block;

  /* If code->expr != NULL, then we need to build a condition
     expression. This is true for IF(cond) and ELSEIF(cond). The
     final, unconditional ELSE is chained at the bottom of this
     function.  */

  while (code && code->expr)
    {
      /* Initialize the statement builder. Puts in NULL_TREEs.  */
      g95_init_se (&if_se, NULL);
      g95_start_block (&if_se.pre);

      /* SIMPLEfy the IF condition expression.  */
      g95_conv_expr_val (&if_se, code->expr);

      /* Translate the THEN clause.  */
      stmt = g95_trans_code (code->next);

      /* Build the condition expression and add it to the condition block.  */
      ifstmt = build_v (COND_EXPR, if_se.expr, stmt, empty_stmt_node);
      g95_add_expr_to_block (&if_se.pre, ifstmt);

      /* Finish off this statement.  */
      stmt = g95_finish_block (&if_se.pre);

      /* If this is an elseif, insert it into the else of the previous
         condition.  */
      if (tail)
        TREE_OPERAND (tail, 2) = stmt;
      tail = ifstmt;

      /* Store in TOP if this is the first translated IF block of this
         construct.  */
      if (! top)
        top = stmt;

      /* Advance to the next block, if there is one.  */
      code = code->block;
    }

  /* See about the unconditional ELSE.  */
  if (code)
    TREE_OPERAND (tail, 2) = g95_trans_code (code->next);

  return top;
}

/* An Arithmetic if statement. IF (cond) label1, label2, label3 translates to
    if (cond <= 0)
      {
        if (cond < 0)
          goto label1;
        else // cond == 0
          goto label2;
      }
    else // cond > 0
      goto label3;
 */
tree
g95_trans_arithmetic_if (g95_code *code)
{
  g95_se se;
  tree tmp;
  tree branch1;
  tree branch2;
  tree zero;


  g95_init_se (&se, NULL);
  g95_start_block (&se.pre);

  g95_conv_expr_val (&se, code->expr);

  zero = g95_build_const (TREE_TYPE (se.expr), integer_zero_node);
  branch1 = build_v (GOTO_EXPR, g95_get_label_decl (code->label));
  branch2 = build_v (GOTO_EXPR, g95_get_label_decl (code->label2));
  tmp = build (LT_EXPR, boolean_type_node, se.expr, zero);
  branch1 = build_v (COND_EXPR, tmp, branch1, branch2);
  branch2 = build_v (GOTO_EXPR, g95_get_label_decl (code->label3));
  tmp = build (LE_EXPR, boolean_type_node, se.expr, zero);
  branch1 = build_v (COND_EXPR, tmp, branch1, branch2);

  g95_add_expr_to_block (&se.pre, branch1);

  return g95_finish_block (&se.pre);
}

/* Currently calculates the loop count before entering the loop, but
   it may be possible to optimize if step is a constant. The main
   advantage is that the loop test is a single SIMPLE node

   We translate a do loop from:

   DO dovar = from, to, step
      body
   END DO

   to:

   pre_dovar;
   pre_from;
   pre_to;
   pre_step;
   temp1=to_expr-from_expr;
   step_temp=step_expr;
   range_temp=step_tmp/range_temp;
   for ( ; range_temp > 0 ; range_temp = range_temp - 1)
     {
       body;
cycle_label:
       dovar_temp = dovar
       dovar=dovar_temp + step_temp;
     }
exit_label:

   Some optimization is done for empty do loops. We can't just let
   dovar=to because it's possible for from+range*loopcount!=to.  Anyone
   who writes empty DO deserves sub-optimal (but correct) code anyway.

   TODO: Large loop counts
   Does not work loop counts which do not fit into a signed integer kind,
   ie. Does not work for loop counts > 2^31 for integer(kind=4) variables
   We must support the full range.  */
tree
g95_trans_do (g95_code * code)
{
  g95_se se;
  tree dovar;
  tree from;
  tree to;
  tree step;
  tree count;
  tree type;
  tree cond;
  tree cycle_label;
  tree exit_label;
  tree tmp;
  stmtblock_t block;
  stmtblock_t body;

  g95_start_block (&block);

  /* Create SIMPLE versions of all expressions.  */
  g95_init_se (&se, NULL);
  g95_conv_expr_lhs (&se, code->ext.iterator->var);
  g95_add_block_to_block (&block, &se.pre);
  dovar = se.expr;
  type = TREE_TYPE (dovar);

  g95_init_se (&se, NULL);
  g95_conv_expr_type (&se, code->ext.iterator->start, type);
  g95_add_block_to_block (&block, &se.pre);
  from = se.expr;

  g95_init_se (&se, NULL);
  g95_conv_expr_type (&se, code->ext.iterator->end, type);
  g95_add_block_to_block (&block, &se.pre);
  to = se.expr;

  g95_init_se (&se, NULL);
  g95_conv_expr_type (&se, code->ext.iterator->step, type);
  /* We don't want this changing part way through.  */
  g95_make_safe_expr (&se);
  g95_add_block_to_block (&block, &se.pre);
  step = se.expr;

  /* Initialise loop count. This code is executed before we enter the
     loop body. We generate: count = (to + step - from) / step.  */

  tmp = fold (build (MINUS_EXPR, type, step, from));
  tmp = fold (build (PLUS_EXPR, type, to, tmp));
  tmp = fold (build (TRUNC_DIV_EXPR, type, tmp, step));

  count = g95_create_var (type, "count");
  g95_add_modify_expr (&block, count, tmp);

  /* Initialise the DO variable: dovar = from.  */
  g95_add_modify_expr (&block, dovar, from);

  /* Loop body.  */
  g95_start_block (&body);

  /* Cycle and exit statements are implemented with gotos.  */
  cycle_label = g95_build_label_decl (NULL_TREE);
  exit_label = g95_build_label_decl (NULL_TREE);

  /* Start with the loop condition.  Loop until count <= 0.  */
  cond = build (LE_EXPR, boolean_type_node, count, integer_zero_node);
  tmp = build_v (GOTO_EXPR, exit_label);
  TREE_USED (exit_label) = 1;
  tmp = build_v (COND_EXPR, cond, tmp, empty_stmt_node);
  g95_add_expr_to_block (&body, tmp);

  /* Put these labels where they can be found later. We put the
     labels in a TREE_LIST node (because TREE_CHAIN is already
     used). cycle_label goes in TREE_PURPOSE (backend_decl), exit
     label in TREE_VALUE (backend_decl).  */
  code->block->backend_decl = tree_cons (cycle_label, exit_label, NULL);

  /* Main loop body.  */
  tmp = g95_trans_code (code->block->next);
  g95_add_expr_to_block (&body, tmp);

  /* Label for cycle statements (if needed).  */
  if (TREE_USED (cycle_label))
    {
      tmp = build_v (LABEL_EXPR, cycle_label);
      g95_add_expr_to_block (&body, tmp);
    }

  /* Increment the loop variable.  */
  tmp = build (PLUS_EXPR, type, dovar, step);
  g95_add_modify_expr (&body, dovar, tmp);
  /* Decrement the loop count.  */
  tmp = build (MINUS_EXPR, type, count, integer_one_node);
  g95_add_modify_expr (&body, count, tmp);

  /* End of loop body.  */
  tmp = g95_finish_block (&body);

  /* The for loop itself.  */
  tmp = build_v (LOOP_EXPR, tmp);
  g95_add_expr_to_block (&block, tmp);

  /* Add the exit label.  */
  tmp = build_v (LABEL_EXPR, exit_label);
  g95_add_expr_to_block (&block, tmp);

  return g95_finish_block (&block);
}

/* We translate

   DO WHILE (cond)
      body
   END DO

   to:

   for ( ; ; )
     {
       pre_cond;
       if (! cond) goto exit_label;
       body;
cycle_label:
     }
exit_label:

   Because the evaluation of the exit condition `cond' may have side
   effects, we can't do much for empty loop bodies. Future
   improvements to the SIMPLEfying process, however, could make
   possible things such as a warning if we can prove that cond doesn't
   have side effects, and the loop body is empty.  */
tree
g95_trans_do_while (g95_code * code)
{
  g95_se cond;
  tree tmp;
  tree cycle_label;
  tree exit_label;
  stmtblock_t block;

  /* Everything we build here is part of the loop body.  */
  g95_start_block (&block);

  /* Cycle and exit statements are implemented with gotos.  */
  cycle_label = g95_build_label_decl (NULL_TREE);
  exit_label = g95_build_label_decl (NULL_TREE);

  /* Put the labels where they can be found later. See g95_trans_do().  */
  code->block->backend_decl = tree_cons (cycle_label, exit_label, NULL);

  /* Create a SIMPLE version of the exit condition.  */
  g95_init_se (&cond, NULL);
  g95_conv_expr_val (&cond, code->expr);
  g95_add_block_to_block (&block, &cond.pre);
  cond.expr = build (EQ_EXPR, TREE_TYPE (cond.expr), cond.expr,
                    integer_zero_node);

  /* Build `IF (! cond) GOTO exit_label.  */
  tmp = build_v (GOTO_EXPR, exit_label);
  TREE_USED (exit_label) = 1;
  tmp = build_v (COND_EXPR, cond.expr, tmp, empty_stmt_node);
  g95_add_expr_to_block (&block, tmp);

  /* The main body of the loop.  */
  tmp = g95_trans_code (code->block->next);
  g95_add_expr_to_block (&block, tmp);

  /* Label for cycle statements (if needed).  */
  if (TREE_USED (cycle_label))
    {
      tmp = build_v (LABEL_EXPR, cycle_label);
      g95_add_expr_to_block (&block, tmp);
    }

  /* End of loop body.  */
  tmp = g95_finish_block (&block);

  g95_init_block (&block);
  /* Build the loop.  */
  tmp = build_v (LOOP_EXPR, tmp);
  g95_add_expr_to_block (&block, tmp);

  /* Add the exit label.  */
  tmp = build_v (LABEL_EXPR, exit_label);
  g95_add_expr_to_block (&block, tmp);

  return g95_finish_block (&block);
}

tree
g95_trans_select (g95_code * code)
{
  g95_code *c;
  g95_case *cp;
  tree end_label;
  tree tmp;
  tree low;
  tree high;
  g95_se se;
  stmtblock_t block;
  stmtblock_t body;
  g95_expr *expr;
  int kind;

  /* Normal select statements put the condition in expr, computed GOTO
     statements put it in expr2.  */
  if (code->expr)
    expr = code->expr;
  else
    expr = code->expr2;

  if (expr->ts.type != BT_INTEGER)
    g95_todo_error ("non-integer switch statements");

  g95_start_block (&block);

  g95_init_se (&se, NULL);
  g95_conv_expr_val (&se, expr);
  g95_add_block_to_block (&block, &se.pre);

  kind = expr->ts.kind;

  end_label = g95_build_label_decl (NULL_TREE);

  g95_init_block (&body);

  for (c = code->block; c; c = c->block)
    {
      for (cp = c->ext.case_list; cp; cp = cp->next)
        {
          if (! (cp->low || cp->high))
            {
              /* Case DEFAULT.  */
              low = high = NULL_TREE;
            }
          else
            {
              if (cp->low)
                low = g95_conv_mpz_to_tree (cp->low->value.integer, kind);
              else
                g95_todo_error ("unbounded case ranges");

              if (cp->high)
                {
                  if (mpz_cmp (cp->low->value.integer,
                               cp->high->value.integer) != 0)
                    {
                      high = g95_conv_mpz_to_tree (cp->high->value.integer,
                                                   kind);
                    }
                  else
                    high = NULL_TREE;
                }
              else
                g95_todo_error ("unbounded case ranges");
            }

          /* Add this case label.  */
          tmp = build_v (CASE_LABEL_EXPR, low, high);
          g95_add_expr_to_block (&body, tmp);
        }

      /* Add the statements for this case.  */
      tmp = g95_trans_code (c->next);
      g95_add_expr_to_block (&body, tmp);

      /* Break to the end of the loop.  */
      tmp = build_v (GOTO_EXPR, end_label);
      g95_add_expr_to_block (&body, tmp);
    }

  tmp = g95_finish_block (&body);
  tmp = build_v (SWITCH_EXPR, se.expr, tmp, NULL_TREE);
  g95_add_expr_to_block (&block, tmp);

  tmp = build_v (LABEL_EXPR, end_label);
  g95_add_expr_to_block (&block, tmp);

  return g95_finish_block (&block);
}

static tree
g95_trans_forall_loop (tree * var, tree * start, tree * end, tree * step,
                       int nvar, tree body)
{
  int n;
  tree tmp;
  tree cond;
  stmtblock_t block;
  tree exit_label;

  for (n = 0; n < nvar; n++)
    {
      exit_label = g95_build_label_decl (NULL_TREE);
      TREE_USED (exit_label) = 1;

      /* The body of the loop.  */
      g95_init_block (&block);

      /* The exit condition.  */
      cond = build (GT_EXPR, boolean_type_node, var[n], end[n]);
      tmp = build_v (GOTO_EXPR, exit_label);
      tmp = build_v (COND_EXPR, cond, tmp, empty_stmt_node);
      g95_add_expr_to_block (&block, tmp);

      /* The main loop body.  */
      g95_add_expr_to_block (&block, body);

      /* Increment the loop variable.  */
      tmp = build (PLUS_EXPR, TREE_TYPE (var[n]), var[n], step[n]);
      g95_add_modify_expr (&block, var[n], tmp);

      body = g95_finish_block (&block);

      /* Loop var initialization.  */
      g95_init_block (&block);
      g95_add_modify_expr (&block, var[n], start[n]);

      /* The loop expression.  */
      tmp = build_v (LOOP_EXPR, body);
      g95_add_expr_to_block (&block, tmp);

      /* The exit label.  */
      tmp = build_v (LABEL_EXPR, exit_label);
      g95_add_expr_to_block (&block, tmp);

      body = g95_finish_block (&block);
    }
  return body;
}

/* Allocate data for holding a temporary array.  Returns either a local
   temporary array or a pointer variable.  */
static tree
g95_do_allocate (tree bytesize, tree size, tree * pdata, stmtblock_t * pblock)
{
  tree tmpvar;
  tree pointer;
  tree type;
  tree tmp;
  tree args;

  if (INTEGER_CST_P (size))
    {
      tmp = fold (build (MINUS_EXPR, g95_array_index_type, size,
                  integer_one_node));
    }
  else
    tmp = NULL_TREE;

  type = build_range_type (g95_array_index_type, integer_zero_node, tmp);
  type = build_array_type (boolean_type_node, type);
  if (g95_can_put_var_on_stack (bytesize))
    {
      assert (INTEGER_CST_P (size));
      tmpvar = g95_create_var (type, "mask");
      pointer = NULL_TREE;
    }
  else
    {
      tmpvar = g95_create_var (build_pointer_type (type), "mask");
      pointer = build1 (ADDR_EXPR, ppvoid_type_node, tmpvar);

      args = g95_chainon_list (NULL_TREE, pointer);
      args = g95_chainon_list (args, bytesize);

      if (g95_array_index_kind == 4)
        tmp = gfor_fndecl_internal_malloc;
      else if (g95_array_index_kind == 8)
        tmp = gfor_fndecl_internal_malloc64;
      else
        abort();
      tmp = g95_build_function_call (tmp, args);
      g95_add_expr_to_block (pblock, tmp);
    }
  *pdata = pointer;
  return tmpvar;
}

/* FORALL and WHERE statements are really nasty, especially when you nest them.
   All the rhs of a forall assignment must be evaluated before the actual
   assignments are performed. Presumably this also applies to all the
   assignments in an inner where statement.  */
/* It is possible to want more than G95_MAX_DIMENSIONS vars, but unlikley.  */
#define MAX_FORALL_VARS G95_MAX_DIMENSIONS
/* Generate code for a FORALL statement.  Any temporaries are allocated as a
   linear array, relying on the fact that we process in the same order in all
   loops.
 */
tree
g95_trans_forall (g95_code * code)
{
  stmtblock_t block;
  stmtblock_t body;
  tree var[MAX_FORALL_VARS];
  tree start[MAX_FORALL_VARS];
  tree end[MAX_FORALL_VARS];
  tree step[MAX_FORALL_VARS];
  g95_expr *varexpr[MAX_FORALL_VARS];
  tree tmp;
  tree assign;
  tree size;
  tree bytesize;
  tree tmpvar;
  tree sizevar;
  tree lenvar;
  tree maskindex;
  tree mask;
  tree pmask;
  int n;
  int nvar;
  int need_temp;
  g95_forall_iterator *fa;
  g95_se se;
  g95_code *c;

  g95_start_block (&block);

  n = 0;
  for (fa = code->ext.forall_iterator; fa; fa = fa->next)
    {
      if (n == MAX_FORALL_VARS)
        fatal_error ("too many variables in FORALL statement");

      /* TODO: don't use actual variables in forall.  */
      g95_init_se (&se, NULL);
      g95_conv_expr_lhs (&se, fa->var);
      assert (is_simple_id (se.expr));
      /* se.pre should be empty anyway.  */
      g95_add_block_to_block (&block, &se.pre);
      var[n] = se.expr;

      g95_init_se (&se, NULL);
      g95_conv_expr_val (&se, fa->start);
      g95_add_block_to_block (&block, &se.pre);
      start[n] = se.expr;

      g95_init_se (&se, NULL);
      g95_conv_expr_val (&se, fa->end);
      g95_make_safe_expr (&se);
      g95_add_block_to_block (&block, &se.pre);
      end[n] = se.expr;

      g95_init_se (&se, NULL);
      g95_conv_expr_val (&se, fa->stride);
      g95_make_safe_expr (&se);
      g95_add_block_to_block (&block, &se.pre);
      step[n] = se.expr;

      n++;
    }
  nvar = n;

  assert (nvar <= G95_MAX_DIMENSIONS);

  tmpvar = NULL_TREE;
  lenvar = NULL_TREE;
  size = integer_one_node;
  sizevar = NULL_TREE;
  for (n = 0; n < nvar; n++)
    {
      if (lenvar && TREE_TYPE (lenvar) != TREE_TYPE (start[n]))
        lenvar = NULL_TREE;
      /* size = (end + step - start) / step.  */
      tmp = build (MINUS_EXPR, TREE_TYPE (start[n]), step[n], start[n]);
      tmp = build (PLUS_EXPR, TREE_TYPE (end[n]), end[n], tmp);

      tmp = build (FLOOR_DIV_EXPR, TREE_TYPE (tmp), tmp, step[n]);
      tmp = convert (g95_array_index_type, tmp);

      size = fold (build (MULT_EXPR, g95_array_index_type, size, tmp));
    }

  /* Copy the mask into a temporary variable if required.  */
  /* For now we assume a mask temporary is needed. */
  if (code->expr)
    {
      bytesize = fold (build (MULT_EXPR, g95_array_index_type, size,
                              TYPE_SIZE_UNIT (boolean_type_node)));

      mask = g95_do_allocate (bytesize, size, &pmask, &block);

      maskindex = g95_create_var_np (g95_array_index_type, "mi");
      g95_add_modify_expr (&block, maskindex, integer_zero_node);

      /* Start of mask assignment loop body.  */
      g95_start_block (&body);

      /* Evaluate the mask expression.  */
      g95_init_se (&se, NULL);
      g95_conv_expr_val (&se, code->expr);
      g95_add_block_to_block (&body, &se.pre);

      /* Store the mask.  */
      se.expr = convert (boolean_type_node, se.expr);

      if (pmask)
        tmp = build1 (INDIRECT_REF, TREE_TYPE (TREE_TYPE (mask)), mask);
      else
        tmp = mask;
      tmp = build (ARRAY_REF, boolean_type_node, tmp, maskindex);
      g95_add_modify_expr (&body, tmp, se.expr);

      /* Advance to the next mask element.  */
      tmp = build (PLUS_EXPR, g95_array_index_type, maskindex,
                   integer_one_node);
      g95_add_modify_expr (&body, maskindex, tmp);

      /* Generate the loops.  */
      tmp = g95_finish_block (&body);
      tmp = g95_trans_forall_loop (var, start, end, step, nvar, tmp);
      g95_add_expr_to_block (&block, tmp);
    }
  else
   {
     /* No mask was specified.  */
     maskindex = NULL_TREE;
     mask = pmask = NULL_TREE;
   }

  c = code->block->next;

  /* TODO: loop merging in FORALL statements.  */
  while (c)
    {
      switch (c->op)
        {
        case EXEC_ASSIGN:
          need_temp = g95_check_dependency (c->expr, c->expr2,
                                            varexpr, nvar);
          if (need_temp)
            g95_todo_error ("Forall with temporary");

          if (mask)
            g95_add_modify_expr (&block, maskindex, integer_zero_node);

          g95_start_block (&body);

          assign = g95_trans_assignment (c->expr, c->expr2);

          if (mask)
            {
              /* If a mask was specified make the assignment contitional.  */
              if (pmask)
                tmp = build1 (INDIRECT_REF, TREE_TYPE (TREE_TYPE (mask)), mask);
              else
                tmp = mask;
              tmp = build (ARRAY_REF, boolean_type_node, tmp, maskindex);

              tmp = build_v (COND_EXPR, tmp, assign, empty_stmt_node);
              g95_add_expr_to_block (&body, tmp);

              /* Advance to the next element.  */
              tmp = build (PLUS_EXPR, g95_array_index_type, maskindex,
                           integer_one_node);
              g95_add_modify_expr (&body, maskindex, tmp);
            }
          else
            g95_add_expr_to_block (&body, assign);

          /* Finish the loop.  */
          tmp = g95_finish_block (&body);
          tmp = g95_trans_forall_loop (var, start, end, step, nvar, tmp);
          g95_add_expr_to_block (&block, tmp);
          break;

        case EXEC_WHERE:
          g95_todo_error ("WHERE inside FORALL");
          break;

        case EXEC_POINTER_ASSIGN:
          g95_todo_error ("Pointer assignment inside FORALL");
          break;

        case EXEC_FORALL:
          g95_todo_error ("Nested FORALL");
          break;

        default:
          abort ();
          break;
        }

      c = c->next;
    }

  if (pmask)
    {
      /* Free the temporary for the mask.  */
      tmp = g95_chainon_list (NULL_TREE, pmask);
      tmp = g95_build_function_call (gfor_fndecl_internal_free, tmp);
      g95_add_expr_to_block (&block, tmp);
    }
  if (maskindex)
    pushdecl (maskindex);

  return g95_finish_block (&block);
}

tree
g95_trans_where (g95_code * code ATTRIBUTE_UNUSED)
{
  g95_todo_error ("Statement not implemented: WHERE");
}

/* CYCLE a DO loop. The label decl has already been created by
   g95_trans_do(), it's in TREE_PURPOSE (backend_decl) of the g95_code
   node at the head of the loop. We must mark the label as used.  */
tree
g95_trans_cycle (g95_code * code)
{
  tree cycle_label;

  cycle_label = TREE_PURPOSE (code->ext.whichloop->backend_decl);
  TREE_USED (cycle_label) = 1;
  return build_v (GOTO_EXPR, cycle_label);
}

/* EXIT a DO loop. Similair to CYCLE, but now the label is in
   TREE_VALUE (backend_decl) of the g95_code node at the head of the
   loop.  */
tree
g95_trans_exit (g95_code * code)
{
  tree exit_label;

  exit_label = TREE_VALUE (code->ext.whichloop->backend_decl);
  TREE_USED (exit_label) = 1;
  return build_v (GOTO_EXPR, exit_label);
}

tree
g95_trans_allocate (g95_code * code)
{
  g95_alloc *al;
  g95_expr *expr;
  g95_se se;
  tree tmp;
  tree parm;
  g95_ref *ref;
  tree stat;
  tree pstat;
  tree error_label;
  stmtblock_t block;

  if (! code->ext.alloc_list)
    return NULL_TREE;

  g95_start_block (&block);

  if (code->expr)
    {
      stat = g95_create_var (g95_int4_type_node, "stat");
      TREE_ADDRESSABLE (stat) = 1;
      pstat = build1 (ADDR_EXPR, build_pointer_type (TREE_TYPE (stat)), stat);

      error_label = g95_build_label_decl (NULL_TREE);
      TREE_USED (error_label) = 1;
    }
  else
    {
      pstat = integer_zero_node;
      stat = error_label = NULL_TREE;
    }


  for (al = code->ext.alloc_list; al != NULL; al = al->next)
    {
      expr = al->expr;

      g95_init_se (&se, NULL);
      g95_start_block (&se.pre);

      se.want_pointer = 1;
      se.descriptor_only = 1;
      g95_conv_expr (&se, expr);

      ref = expr->ref;

      if (ref != NULL)
        {
         /* Find the last reference in the chain.  */
         while (ref->next != NULL)
           {
             assert (ref->type != REF_ARRAY || ref->u.ar.type == AR_ELEMENT);
             ref = ref->next;
           }

         if (ref->type != REF_ARRAY)
           ref = NULL;

          /* An array.  */
          g95_array_allocate (&se, ref, pstat);
        }
      else
        {
          /* A scalar or derived type.  */
          /*TODO: allocation of derived types containing arrays.  */
          tree val;

          val = g95_create_var (ppvoid_type_node, "ptr");
          tmp = build1 (ADDR_EXPR, TREE_TYPE (val), se.expr);
          g95_add_modify_expr (&se.pre, val, tmp);

          tmp = TYPE_SIZE_UNIT (TREE_TYPE (se.expr));
          parm = g95_chainon_list (NULL_TREE, val);
          parm = g95_chainon_list (parm, tmp);
          parm = g95_chainon_list (parm, pstat);
          tmp = g95_build_function_call (gfor_fndecl_allocate, parm);
          g95_add_expr_to_block (&se.pre, tmp);

          if (code->expr)
            {
              tmp = build_v (GOTO_EXPR, error_label);
              parm =
                build (NE_EXPR, boolean_type_node, stat, integer_zero_node);
              tmp = build_v (COND_EXPR, parm, tmp, empty_stmt_node);
              g95_add_expr_to_block (&se.pre, tmp);
            }
        }

      tmp = g95_finish_block (&se.pre);
      g95_add_expr_to_block (&block, tmp);
    }

  /* Assign the value to the status variable.  */
  if (code->expr)
    {
      tmp = build_v (LABEL_EXPR, error_label);
      g95_add_expr_to_block (&block, tmp);

      g95_init_se (&se, NULL);
      g95_conv_expr_lhs (&se, code->expr);
      tmp = convert (TREE_TYPE (se.expr), stat);
      g95_add_modify_expr (&block, se.expr, tmp);
    }

  return g95_finish_block (&block);
}

tree
g95_trans_deallocate (g95_code * code)
{
  g95_se se;
  g95_alloc *al;
  g95_expr *expr;
  tree var;
  tree tmp;
  tree type;
  stmtblock_t block;

  g95_start_block (&block);

  for (al = code->ext.alloc_list; al != NULL; al = al->next)
    {
      expr = al->expr;
      assert (expr->expr_type == EXPR_VARIABLE);

      g95_init_se (&se, NULL);
      g95_start_block (&se.pre);

      se.want_pointer = 1;
      g95_conv_expr (&se, expr);

      if (expr->symbol->attr.dimension)
        {
          tmp = g95_array_deallocate (se.expr);
          g95_add_expr_to_block (&se.pre, tmp);
        }
      else
        {
          type = build_pointer_type (TREE_TYPE (se.expr));
          var = g95_create_var (type, "ptr");
          tmp = build1 (ADDR_EXPR, type, se.expr);
          g95_add_modify_expr (&se.pre, var, tmp);

          tmp = g95_chainon_list (NULL_TREE, var);
          tmp = g95_chainon_list (tmp, integer_zero_node);
          tmp = g95_build_function_call (gfor_fndecl_deallocate, tmp);
          g95_add_expr_to_block (&se.pre, tmp);
        }
      tmp = g95_finish_block (&se.pre);
      g95_add_expr_to_block (&block, tmp);
    }

  return g95_finish_block (&block);
}

