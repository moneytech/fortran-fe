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

tree
g95_trans_label_here (g95_code *code)
{
  return build_stmt (LABEL_STMT, g95_get_label_decl (code->here));
}

tree
g95_trans_goto (g95_code *code)
{
  return build_stmt (GOTO_STMT, g95_get_label_decl (code->label));
}

tree
g95_trans_call (g95_code *code)
{
  g95_se se;
  tree stmt;
  g95_symbol sym;
  g95_symbol *psym;

  /* IO functions need special handling.  */
  if (strncmp (code->sub_name, "_io_", 4) == 0)
    return g95_trans_io_call (code);

  g95_start_stmt ();

  g95_init_se (&se, NULL);

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
  g95_conv_function_call (&se, psym, code->ext.actual);

  TREE_SIDE_EFFECTS (se.expr) = 1;
  stmt = build_stmt (EXPR_STMT, se.expr);
  g95_add_stmt_to_pre (&se, stmt, stmt);
  g95_add_stmt_to_pre (&se, se.post, se.post_tail);

  return g95_finish_stmt (se.pre, se.pre_tail);
}

tree
g95_trans_return (g95_code *code ATTRIBUTE_UNUSED)
{
  tree stmt;

  stmt = build_stmt (GOTO_STMT, g95_get_return_label ());

  return stmt;
}

tree
g95_trans_stop (g95_code *code)
{
  g95_se se;
  tree arg;
  tree tmp;

  g95_start_stmt ();

  g95_init_se (&se, NULL);
  if (code->label != NULL)
    {
      if (code->expr != NULL)
        {
          g95_conv_simple_val_type (&se, code->expr, g95_int4_type_node);
          arg = se.expr;
        }
      else
        arg = build_int_2 (code->label->value, 0);
    }
  else
    arg = integer_zero_node;

  arg = tree_cons (NULL_TREE, arg, NULL_TREE);
  tmp = g95_build_function_call (gfor_fndecl_stop, arg);
  tmp = build_stmt (EXPR_STMT, tmp);
  g95_add_stmt_to_pre (&se, tmp, tmp);

  tmp = g95_finish_stmt (se.pre, se.pre_tail);

  return tmp;
}


/* Generate SIMPLE for the IF construct. This function also deals with
   the simple IF statement, because the front end translates the IF
   statement into an IF construct.

   We translate:

        IF (cond) THEN
           then_clause
        ELSEIF (cond2)
           ...
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
              ...
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
  g95_se if_stmt;
  tree top, stmt, tail, then_clause;

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
      /* Enter a new scope for each block.  */
      g95_start_stmt ();

      /* Initialize the statement builder. Puts in NULL_TREEs.  */
      g95_init_se (&if_stmt, NULL);

      /* SIMPLEfy the IF condition expression.  */
      g95_conv_simple_cond (&if_stmt, code->expr);

      /* Translate the THEN clause. g95_trans_code() wraps it in a
         COMPOUND_STMT for us.  */
      then_clause = g95_trans_code (code->next);

      /* Build the IF_STMT node and add it to the POST chain of the
         SIMPLEfied condition expression.  */
      stmt = build_stmt (IF_STMT,
                         if_stmt.expr,
                         then_clause,
                         NULL_TREE);
      g95_add_stmt_to_pre (&if_stmt, stmt, NULL_TREE);

      /* Finish off this statement.  */
      if_stmt.pre = g95_finish_stmt (if_stmt.pre, NULL_TREE);

      /* Chain the block to the ELSE_CLAUSE of a previously translated
         IF blocks, and move the tail to the current IF block.  */
      if (tail)
	ELSE_CLAUSE (tail) = if_stmt.pre;
      tail = stmt;

      /* Store in TOP if this is the first translated IF block of this
         construct.  */
      if (! top)
	top = if_stmt.pre;

      /* Advance to the next block, if there is one.  */
      code = code->block;
    }

  /* See about the unconditional ELSE.  */
  if (code)
    ELSE_CLAUSE (tail) = g95_trans_code (code->next);

  return top;
}

tree
g95_trans_arithmetic_if (g95_code *code ATTRIBUTE_UNUSED)
{
  g95_todo_error ("Statement not implemented: ARITHMETIC IF");
}

/* Compare two values.  Return true if they are the same variable.  */
static int
g95_is_same_var (tree val1, tree val2)
{
  if (val1 == val2)
    return 1;

  if (val1 == NULL_TREE || val2 == NULL_TREE)
    return 0;

  /* ignore NON_LAVALUE_EXPR.  */
  if (TREE_CODE (val1) == NON_LVALUE_EXPR)
    val1 = TREE_OPERAND (val1, 0);
  if (TREE_CODE (val1) == NON_LVALUE_EXPR)
    val1 = TREE_OPERAND (val1, 0);

  if (TREE_CODE (val1) != VAR_DECL || TREE_CODE (val2) != VAR_DECL)
    return 0;

  return (val1 == val2);
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
  g95_se dovar;
  g95_se from;
  g95_se to;
  g95_se step;
  tree body;
  tree expr;
  tree count;
  tree countvar;
  tree init;
  tree init_tail;
  tree type;
  tree cond;
  tree stmt;
  tree cycle_label;
  tree exit_label;
  tree for_init;
  tree tmp;

  g95_start_stmt ();

  /* Create SIMPLE versions of all expressions.  */
  g95_init_se (&dovar, NULL);
  g95_conv_simple_lhs (&dovar, code->ext.iterator->var);
  type = TREE_TYPE (dovar.expr);
  g95_init_se (&from, NULL);
  g95_conv_simple_val_type (&from, code->ext.iterator->start, type);
  g95_init_se (&to, NULL);
  g95_conv_simple_val_type (&to, code->ext.iterator->end, type);
  g95_init_se (&step, NULL);
  g95_conv_simple_val_type (&step, code->ext.iterator->step, type);

  /* We don't want this changing part way through.  */
  g95_make_safe_expr (&step);

  countvar = NULL_TREE;
  init = init_tail = NULL_TREE;

  /* Initialise loop count. This code is executed before we enter the
     loop body. We generate: count = (to + step - from)  */

  count = build (MINUS_EXPR, type, step.expr, from.expr);
  count = g95_simple_fold (count, &init, &init_tail, &countvar);

  count = build (PLUS_EXPR, type, to.expr, count);
  count = g95_simple_fold (count, &init, &init_tail, &countvar);

  for_init = tmp = NULL_TREE;
  count = build (TRUNC_DIV_EXPR, type, count, step.expr);
  count = g95_simple_fold (count, &init, &init_tail, &countvar);

  /* We use the last assignment as the initial expression for the for loop.  */
  if (! g95_is_same_var (count,  countvar))
    {
      /* Count needs to be a temporary variable. */
      if (countvar == NULL_TREE)
        countvar = create_tmp_var (type, "count");
      tmp = build (MODIFY_EXPR, type, countvar, count);
      for_init = build_stmt (EXPR_STMT, tmp);
      count = countvar;
    }
  else
    {
      assert (init != NULL_TREE);
      /* The last assignment is the for loop init statement.  */

      for_init = init_tail;
      if (init == for_init)
        init = init_tail = NULL_TREE;
      else
        {
          init_tail = init;
          while (TREE_CHAIN (init_tail) != for_init)
            init_tail = TREE_CHAIN (init_tail);
          TREE_CHAIN (init_tail) = NULL_TREE;
        }
    }

  /* Initialise the DO variable: dovar = from.  */
  tmp = build (MODIFY_EXPR, type, dovar.expr, from.expr);
  stmt = build_stmt (EXPR_STMT, tmp);
  g95_add_stmt_to_list (&init, &init_tail, stmt, stmt);

  /* Loop until count <= 0.  */
  cond = build (GT_EXPR, boolean_type_node, count, integer_zero_node);

  /* count = count - 1  */
  tmp = build (MINUS_EXPR, type, count, integer_one_node);
  expr = build (MODIFY_EXPR, type, count, tmp);

  /* Loop body.  */
  g95_start_stmt ();

  /* Only translate the DO block if it is't empty.  */
  if (code->block->next != NULL)
    {
      /* Cycle and exit statements are implemented with gotos.  */
      cycle_label = g95_build_label_decl (NULL_TREE);
      exit_label = g95_build_label_decl (NULL_TREE);

      /* Put these labels where they can be found later. We put the
         labels in a TREE_LIST node (because TREE_CHAIN is already
         used). cycle_label goes in TREE_PURPOSE (backend_decl), exit
         label in TREE_VALUE (backend_decl).  */
      code->block->backend_decl = tree_cons (cycle_label, exit_label, NULL);

      /* Main loop body.  */
      body = g95_trans_code (code->block->next);

      /* Label for cycle statements (if needed).  */
      if (TREE_USED (cycle_label))
        {
          body = chainon (body, build_stmt (LABEL_STMT, cycle_label));
        }
    }
  else
    {
      body = NULL_TREE;
      exit_label = NULL_TREE;
    }

  /* Increment the loop variable.  */
  if (! is_simple_val (dovar.expr))
    {
      /* Create a temporary if necessary.  */
      tmp = create_tmp_var (type, "dovar");
      build_stmt (EXPR_STMT, build (MODIFY_EXPR, type, tmp, dovar.expr));
    }
  else
    tmp = dovar.expr;

  /* dovar = SIMPLEfied dovar + step.  */
  tmp = build (PLUS_EXPR, type, tmp, step.expr);
  tmp = build (MODIFY_EXPR, type, dovar.expr, tmp);
  body = chainon (body, build_stmt (EXPR_STMT, tmp));

  /* End of loop body.  */
  body = g95_finish_stmt (body, NULL_TREE);

  /* The for loop itself.  */
  stmt = build_stmt (FOR_STMT, for_init, cond, expr, body);

  /* Chain all the bits together.  */
  /* We know that to, from and step all have empty post trees.  */
  /* Collate all the stmts in dovar.pre.  */
  g95_add_stmt_to_pre (&dovar, from.pre, from.pre_tail);
  g95_add_stmt_to_pre (&dovar, to.pre, to.pre_tail);
  g95_add_stmt_to_pre (&dovar, step.pre, step.pre_tail);
  g95_add_stmt_to_pre (&dovar, init, init_tail);
  g95_add_stmt_to_pre (&dovar, stmt, NULL_TREE);

  /* Add label for exit if one exists.  */
  if (code->block->next != NULL && TREE_USED (exit_label))
    {
      TREE_CHAIN (dovar.pre_tail) = build_stmt (LABEL_STMT, exit_label);
      dovar.pre_tail = TREE_CHAIN (dovar.pre_tail);
    }

  stmt = g95_finish_stmt (dovar.pre, dovar.pre_tail);

  return stmt;
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
  tree body;
  tree stmt;
  tree cycle_label;
  tree exit_label;

  /* Everything we build here is part of the loop body.  */
  g95_start_stmt ();

  /* Cycle and exit statements are implemented with gotos. For DO
     WHILE, we need at least the exit label.  */
  cycle_label = g95_build_label_decl (NULL_TREE);
  exit_label = g95_build_label_decl (NULL_TREE);

  /* Put the labels where they can be found later. See g95_trans_do().  */
  code->block->backend_decl = tree_cons (cycle_label, exit_label, NULL);

  /* Create a SIMPLE version of the exit condition.  */
  g95_init_se (&cond, NULL);
  {
    g95_conv_simple_val (&cond, code->expr);
    cond.expr = build (EQ_EXPR, TREE_TYPE (cond.expr), cond.expr,
                      integer_zero_node);
  }

  /* Build `IF (! cond) GOTO exit_label.  */
  stmt = build_stmt (IF_STMT,
		     cond.expr,
		     build_stmt (GOTO_STMT, exit_label),
		     NULL_TREE);

  /* Only translate the DO WHILE body block if it is't empty.  */
  if (code->block->next != NULL)
    {
      body = g95_trans_code (code->block->next);

      /* Label for cycle statements (if needed).  */
      if (TREE_USED (cycle_label))
        body = chainon (body, build_stmt (LABEL_STMT, cycle_label));
    }
  else
    body = NULL_TREE;

  /* Put the IF (..) GOTO expression on top of the body.  */
  body = chainon (stmt, body);

  /* Chain the body and the SIMPLEfied expression together.  */
  g95_add_stmt_to_pre (&cond, body, NULL_TREE);

  /* End of loop body.  */
  body = g95_finish_stmt (cond.pre, cond.pre_tail);

  /* Build the SIMPLE `for'-loop itself. Oddly the FOR_INIT_EXPR
     cannot be a NULL_TREE, instead it must be an empty EXPR_STMT...  */
  stmt = build_stmt (EXPR_STMT, NULL_TREE);
  stmt = build_stmt (FOR_STMT, stmt, NULL_TREE, NULL_TREE, body);

  /* Add label for exit. This time it always exists.  */
  TREE_USED (exit_label) = 1;
  chainon (stmt, build_stmt (LABEL_STMT, exit_label));

  return stmt;
}

tree
g95_trans_select (g95_code * code ATTRIBUTE_UNUSED)
{
  g95_todo_error ("Statement not implemented: SELECT");
}

static tree
g95_trans_forall_loop (tree * var, tree * start, tree * end, tree * step,
                       int nvar, tree body)
{
  tree init;
  tree cond;
  tree inc;
  int n;

  for (n = 0; n < nvar; n++)
    {
      init = build (MODIFY_EXPR, TREE_TYPE (var[n]), var[n], start[n]);
      init = build_stmt (EXPR_STMT, init);
      cond = build (LE_EXPR, boolean_type_node, var[n], end[n]);
      inc = build (PLUS_EXPR, TREE_TYPE (var[n]), var[n], step[n]);
      inc = build (MODIFY_EXPR, TREE_TYPE (var[n]), var[n], inc);

      body = build_stmt (FOR_STMT, init, cond, inc, body);
    }
  return body;
}

/* Allocate data for holding a temporary array.  Returns either a local
   temporary array or a pointer variable.  */
static tree
g95_do_allocate (tree bytesize, tree size, tree * pdata,
                 tree * phead, tree * ptail)
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
      tmpvar = create_tmp_var (type, "mask");
      pointer = NULL_TREE;
    }
  else
    {
      tmpvar = create_tmp_var (build_pointer_type (type), "mask");
      pointer = build1 (ADDR_EXPR, ppvoid_type_node, tmpvar);
      pointer = g95_simple_fold (pointer, phead, ptail, NULL);

      args = g95_chainon_list (NULL_TREE, pointer);
      args = g95_chainon_list (args, bytesize);

      if (g95_array_index_kind == 4)
        tmp = gfor_fndecl_internal_malloc;
      else if (g95_array_index_kind == 8)
        tmp = gfor_fndecl_internal_malloc64;
      else
        abort();
      tmp = g95_build_function_call (tmp, args);
      tmp = build_stmt (EXPR_STMT, tmp);
      g95_add_stmt_to_list (phead, ptail, tmp, tmp);
    }
  *pdata = pointer;
  return tmpvar;
}

/* FORALL and WHERE statements are really nasty, especially when you nest them.
   All the rhs of a forall assignment must be evaluated before the actual
   assignments are performaed. Presumably this alos applies to all the
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
  tree head;
  tree tail;
  tree body;
  tree body_tail;
  tree var[MAX_FORALL_VARS];
  tree start[MAX_FORALL_VARS];
  tree end[MAX_FORALL_VARS];
  tree step[MAX_FORALL_VARS];
  g95_expr *varexpr[MAX_FORALL_VARS];
  tree tmp;
  tree size;
  tree bytesize;
  tree tmpvar;
  tree sizevar;
  tree lenvar;
  tree maskindex;
  tree mask;
  tree pmask;
  tree stmt;
  int n;
  int nvar;
  int need_temp;
  g95_forall_iterator *fa;
  g95_se se;
  g95_code *block;

  g95_start_stmt ();
  head = tail = NULL_TREE;

  n = 0;
  for (fa = code->ext.forall_iterator; fa; fa = fa->next)
    {
      if (n == MAX_FORALL_VARS)
        fatal_error ("too many variables in FORALL statement");

      /* TODO: don't use actual variables in forall.  */
      g95_init_se (&se, NULL);
      g95_conv_simple_lhs (&se, fa->var);
      assert (is_simple_id (se.expr));
      assert (se.pre == NULL_TREE && se.post == NULL_TREE);
      var[n] = se.expr;

      g95_init_se (&se, NULL);
      g95_conv_simple_val (&se, fa->start);
      g95_add_stmt_to_list (&head, &tail, se.pre, se.pre_tail);
      assert (se.post == NULL_TREE);
      start[n] = se.expr;

      g95_init_se (&se, NULL);
      g95_conv_simple_val (&se, fa->end);
      g95_make_safe_expr (&se);
      g95_add_stmt_to_list (&head, &tail, se.pre, se.pre_tail);
      assert (se.post == NULL_TREE);
      end[n] = se.expr;

      g95_init_se (&se, NULL);
      g95_conv_simple_val (&se, fa->stride);
      g95_make_safe_expr (&se);
      g95_add_stmt_to_list (&head, &tail, se.pre, se.pre_tail);
      assert (se.post == NULL_TREE);
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
      tmp = g95_simple_fold (tmp, &head, &tail, &lenvar);
      tmp = build (PLUS_EXPR, TREE_TYPE (end[n]), end[n], tmp);
      tmp = g95_simple_fold (tmp, &head, &tail, &lenvar);

      tmp = build (FLOOR_DIV_EXPR, TREE_TYPE (tmp), tmp, step[n]);
      tmp = g95_simple_fold (tmp, &head, &tail, &lenvar);

      tmp = g95_simple_convert (g95_array_index_type, tmp);
      tmp = g95_simple_fold (tmp, &head, &tail, &tmpvar);

      tmp = build (MULT_EXPR, g95_array_index_type, size, tmp);
      size = g95_simple_fold_tmp (tmp, &head, &tail, &sizevar);
    }

  /* Copy the mask into a temporary variable if required.  */
  /* For now we assume a mask temporary is needed. */
  if (code->expr)
    {
      bytesize = build (MULT_EXPR, g95_array_index_type, size,
                        TYPE_SIZE_UNIT (boolean_type_node));
      bytesize = g95_simple_fold (bytesize, &head, &tail, NULL);

      mask = g95_do_allocate (bytesize, size, &pmask, &head, &tail);

      maskindex = create_tmp_alias_var (g95_array_index_type, "mi");
      tmp = build (MODIFY_EXPR, g95_array_index_type, maskindex,
                   integer_zero_node);
      tmp = build_stmt (EXPR_STMT, tmp);
      g95_add_stmt_to_list (&head, &tail, tmp, tmp);

      /* Start of mask assignment loop body.  */
      g95_start_stmt ();
      body = body_tail = NULL_TREE;

      /* Evaluate the mask expression.  */
      g95_init_se (&se, NULL);
      g95_conv_simple_cond (&se, code->expr);
      g95_add_stmt_to_list (&body, &body_tail, se.pre, se.pre_tail);
      assert (se.post == NULL_TREE);

      /* Store the mask.  */
      if (TREE_TYPE (se.expr) != boolean_type_node)
        {
          se.expr = g95_simple_fold (se.expr, &body, &body_tail, NULL);
          se.expr = g95_simple_convert (boolean_type_node, se.expr);
        }

      if (pmask)
        tmp = build1 (INDIRECT_REF, TREE_TYPE (TREE_TYPE (mask)), mask);
      else
        tmp = mask;
      tmp = build (ARRAY_REF, boolean_type_node, tmp, maskindex);
      tmp = build (MODIFY_EXPR, boolean_type_node, tmp, se.expr);
      tmp = build_stmt (EXPR_STMT, tmp);
      g95_add_stmt_to_list (&body, &body_tail, tmp, tmp);

      /* Advance to the next mask element.  */
      tmp = build (PLUS_EXPR, g95_array_index_type, maskindex,
                   integer_one_node);
      tmp = build (MODIFY_EXPR, g95_array_index_type, maskindex, tmp);
      tmp = build_stmt (EXPR_STMT, tmp);
      g95_add_stmt_to_list (&body, &body_tail, tmp, tmp);

      /* Generate the loops.  */
      stmt = g95_finish_stmt (body, body_tail);
      stmt = g95_trans_forall_loop (var, start, end, step, nvar, stmt);
      g95_add_stmt_to_list (&head, &tail, stmt, NULL_TREE);
    }
  else
   {
     /* No mask was specified.  */
     maskindex = NULL_TREE;
     mask = pmask = NULL_TREE;
   }

  block = code->block->next;

  /* TODO: loop merging in FORALL statements.  */
  while (block)
    {
      switch (block->op)
        {
        case EXEC_ASSIGN:
          need_temp = g95_check_dependancy (block->expr, block->expr2,
                                            varexpr, nvar);
          if (need_temp)
            g95_todo_error ("Forall with temporary");

          if (mask)
            {
              tmp = build (MODIFY_EXPR, g95_array_index_type, maskindex,
                           integer_zero_node);
              tmp = build_stmt (EXPR_STMT, tmp);
              g95_add_stmt_to_list (&head, &tail, tmp, tmp);
            }

          g95_start_stmt ();
          body = body_tail = NULL_TREE;
          if (mask)
            g95_start_stmt ();

          tmp = g95_trans_assignment (block->expr, block->expr2);
          g95_add_stmt_to_list (&body, &body_tail, tmp, NULL_TREE);

          if (mask)
            {
              /* If a mask was specified make the assignment contitional.  */
              stmt = g95_finish_stmt (body, body_tail);
              body = body_tail = NULL_TREE;

              if (pmask)
                tmp = build1 (INDIRECT_REF, TREE_TYPE (TREE_TYPE (mask)), mask);
              else
                tmp = mask;
              tmp = build (ARRAY_REF, boolean_type_node, tmp, maskindex);
              tmp = g95_simple_fold (tmp, &body, &body_tail, NULL);

              stmt = build_stmt (IF_STMT, tmp, stmt);
              g95_add_stmt_to_list (&body, &body_tail, stmt, stmt);
            }

          if (mask)
            {
              /* Advance to the next element.  */
              tmp = build (PLUS_EXPR, g95_array_index_type, maskindex,
                           integer_one_node);
              tmp = build (MODIFY_EXPR, g95_array_index_type, maskindex, tmp);
              stmt = build_stmt (EXPR_STMT, tmp);
              g95_add_stmt_to_list (&body, &body_tail, stmt, stmt);
            }

          /* Finish the loop.  */
          stmt = g95_finish_stmt (body, body_tail);
          stmt = g95_trans_forall_loop (var, start, end, step, nvar, stmt);
          g95_add_stmt_to_list (&head, &tail, stmt, NULL_TREE);
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

      block = block->next;
    }

  if (pmask)
    {
      /* Free the temporary for the mask.  */
      tmp = g95_chainon_list (NULL_TREE, pmask);
      tmp = g95_build_function_call (gfor_fndecl_internal_free, tmp);
      stmt = build_stmt (EXPR_STMT, tmp);
      g95_add_stmt_to_list (&head, &tail, stmt, stmt);
    }
  if (maskindex)
    pushdecl (maskindex);

  head = g95_finish_stmt (head, tail);
  return head;
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
  return build_stmt (GOTO_STMT, cycle_label);
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
  return build_stmt (GOTO_STMT, exit_label);
}

tree
g95_trans_allocate (g95_code * code)
{
  g95_alloc *al;
  g95_expr *expr;
  g95_se se;
  tree tmp;
  tree parm;
  tree stmt;
  g95_ref *ref;
  tree head;
  tree stat;
  tree pstat;
  tree error_label;

  if (! code->ext.alloc_list)
    return NULL_TREE;

  g95_start_stmt ();

  head = NULL_TREE;

  if (code->expr)
    {
      stat = create_tmp_var (g95_int4_type_node, "stat");
      TREE_ADDRESSABLE (stat) = 1;
      pstat = build1 (ADDR_EXPR, build_pointer_type (TREE_TYPE (stat)), stat);
      pstat = g95_simple_fold (pstat, &head, &tmp, NULL);

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
      g95_start_stmt ();

      expr = al->expr;

      g95_init_se (&se, NULL);
      se.want_pointer = 1;
      se.descriptor_only = 1;
      g95_conv_simple_rhs (&se, expr);
      assert (se.post == NULL_TREE);

      ref = expr->ref;

      /* Find the last reference in the chain.  */
      while (ref->next != NULL)
        {
          assert (ref->type != REF_ARRAY || ref->u.ar.type == AR_ELEMENT);
          ref = ref->next;
        }

      if (ref->type != REF_ARRAY)
        ref = NULL;

      if (ref != NULL)
        {
          /* An array.  */
          g95_array_allocate (&se, ref, pstat);
        }
      else
        {
          /* A scalar or derived type.  */
          /*TODO: allocation of derived types containing arrays.  */
          tree val;

          val = create_tmp_var (ppvoid_type_node, "ptr");
          tmp = build1 (ADDR_EXPR, TREE_TYPE (val), se.expr);
          tmp = build (MODIFY_EXPR, TREE_TYPE (val), val, tmp);
          stmt = build_stmt (EXPR_STMT, tmp);
          g95_add_stmt_to_pre (&se, stmt, stmt);

          tmp = TYPE_SIZE_UNIT (TREE_TYPE (se.expr));
          parm = g95_chainon_list (NULL_TREE, val);
          parm = g95_chainon_list (parm, tmp);
          parm = g95_chainon_list (parm, pstat);
          tmp = g95_build_function_call (gfor_fndecl_allocate, parm);
          stmt = build_stmt (EXPR_STMT, tmp);
          g95_add_stmt_to_pre (&se, stmt, stmt);

          if (code->expr)
            {
              stmt = build_stmt (GOTO_STMT, error_label);
              tmp =
                build (NE_EXPR, boolean_type_node, stat, integer_zero_node);
              stmt = build_stmt (IF_STMT, tmp, stmt, NULL_TREE);
              g95_add_stmt_to_pre (&se, stmt, stmt);
            }
        }

      stmt = g95_finish_stmt (se.pre, se.pre_tail);
      head = chainon (head, stmt);
    }

  /* Assign the value to the status variable.  */
  if (code->expr)
    {
      stmt = build_stmt (LABEL_STMT, error_label);

      g95_init_se (&se, NULL);
      g95_conv_simple_lhs (&se, code->expr);
      tmp = g95_simple_convert (TREE_TYPE (se.expr), stat);
      tmp = build (MODIFY_EXPR, TREE_TYPE (se.expr), se.expr, tmp);
      stmt = chainon (stmt, build_stmt (EXPR_STMT, tmp));

      head = chainon (head, stmt);
    }

  /* Don't add the extra scope if it's not needed.  */
  if (! (code->ext.alloc_list->next || code->expr))
    g95_merge_stmt ();
  else
    head = g95_finish_stmt (head, NULL_TREE);

  return head;
}

tree
g95_trans_deallocate (g95_code * code)
{
  g95_se se;
  g95_alloc *al;
  g95_expr *expr;
  tree stmt;
  tree head;
  tree var;
  tree tmp;
  tree type;

  g95_start_stmt ();
  head = NULL_TREE;

  for (al = code->ext.alloc_list; al != NULL; al = al->next)
    {
      g95_start_stmt ();

      expr = al->expr;
      assert (expr->expr_type == EXPR_VARIABLE);

      g95_init_se (&se, NULL);
      se.want_pointer = 1;
      g95_conv_simple_rhs (&se, expr);
      assert (se.post == NULL_TREE);

      if (expr->symbol->attr.dimension)
        {
          stmt = g95_array_deallocate (se.expr);
          g95_add_stmt_to_pre (&se, stmt, NULL_TREE);
        }
      else
        {
          type = build_pointer_type (TREE_TYPE (se.expr));
          var = create_tmp_var (type, "ptr");
          tmp = build1 (ADDR_EXPR, type, se.expr);
          tmp = build (MODIFY_EXPR, type, var, tmp);
          stmt = build_stmt (EXPR_STMT, tmp);
          g95_add_stmt_to_pre (&se, stmt, stmt);

          tmp = g95_chainon_list (NULL_TREE, var);
          tmp = g95_chainon_list (tmp, integer_zero_node);
          tmp = g95_build_function_call (gfor_fndecl_deallocate, tmp);
          stmt = build_stmt (EXPR_STMT, tmp);
          g95_add_stmt_to_pre (&se, stmt, stmt);
        }
      stmt = g95_finish_stmt (se.pre, se.pre_tail);
      head = chainon (head, stmt);
    }

  /* Only create the outer scope if there's more than one variable.  */
  if (TREE_CHAIN (head) == NULL_TREE)
    {
      assert (TREE_CODE (head) == COMPOUND_STMT);
      g95_merge_stmt ();
    }
  else
    head = g95_finish_stmt (head, NULL_TREE);

  return head;
}

