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
#include <gmp.h>
#define BACKEND_CODE
#include "g95.h"
#include "trans.h"
#include "trans-stmt.h"

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
g95_trans_call (g95_code *code ATTRIBUTE_UNUSED)
{
  g95_todo_error ("Statement not implemented: CALL");
}

tree
g95_trans_return (g95_code *code ATTRIBUTE_UNUSED)
{
  tree stmt;

  stmt = build_stmt (GOTO_STMT, g95_get_return_label ());

  return stmt;
}

tree
g95_trans_stop (g95_code *code ATTRIBUTE_UNUSED)
{
  g95_todo_error ("Statement not implemented: STOP");
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
   dovar=to because it's possible for to+range*(to-from)!=to.  Anyone
   who writes empty DO deserves sub-optimal (but correct) code anyway.

   TODO:
   Does not work loop counts which do not fit into a signed integer kind,
   ie. Does not work for loop counts > 2^31 for integer(kind=4) variables
   We must support the full range.  */
tree
g95_trans_do (g95_code * code)
{
  g95_se  dovar;
  g95_se  from;
  g95_se  to;
  g95_se  step;
  tree body;
  tree expr;
  tree count;
  tree count1;
  tree range;
  tree init;
  tree type;
  tree cond;
  tree stmt;
  tree cycle_label;
  tree exit_label;
  tree for_init;
  tree tmp;

  g95_start_stmt ();

  /* Create SIMPLE versions of all expressions.  */
  g95_init_se (&from, NULL);
  g95_conv_simple_val (&from, code->ext.iterator->start);
  g95_init_se (&to, NULL);
  g95_conv_simple_val (&to, code->ext.iterator->end);
  g95_init_se (&dovar, NULL);
  g95_conv_simple_lhs (&dovar, code->ext.iterator->var);
  g95_init_se (&step, NULL);
  g95_conv_simple_val (&step, code->ext.iterator->step);

  /* We don't want this changing half way through the loop.  */
  g95_make_tmp_expr (&step);

  type = TREE_TYPE (dovar.expr);

  range = g95_create_tmp_var (type);
  count = g95_create_tmp_var (type);

  /* Initialise loop count. This code is executed before we enter the
     loop body. We generate:

     range = to - from;
     range = range + step;
     count = range / step;  */
  tmp = build (MINUS_EXPR, type, to.expr, from.expr);
  tmp = build (MODIFY_EXPR, type, range, tmp);
  init = build_stmt (EXPR_STMT, tmp);

  tmp = build (PLUS_EXPR, type, range, step.expr);
  tmp = build (MODIFY_EXPR, type, range, tmp);
  init = chainon (init, build_stmt (EXPR_STMT, tmp));

  tmp = build (TRUNC_DIV_EXPR, type, range, step.expr);
  tmp = build (MODIFY_EXPR, type, count, tmp);
  init = chainon (init, build_stmt (EXPR_STMT, tmp));

  /* Initialise the DO variable: just dovar = from.  */
  for_init = build_stmt (EXPR_STMT,
			build (MODIFY_EXPR, type, dovar.expr, from.expr));

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
         used?). cycle_label goes in TREE_PURPOSE (backend_decl), exit
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
      tmp = g95_create_tmp_var (type);
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
  g95_add_stmt_to_pre (&dovar, step.pre, to.pre_tail);
  g95_add_stmt_to_pre (&dovar, init, NULL_TREE);
  g95_add_stmt_to_pre (&dovar, stmt, NULL_TREE);

  /* Add label for exit if one exists.  */
  if ((code->block->next != NULL)
      && exit_label
      && TREE_USED (exit_label))
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
  g95_conv_simple_cond (&cond, code->expr);

  /* Build `IF (cond) GOTO exit_label.  */
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
g95_trans_select (g95_code *code ATTRIBUTE_UNUSED)
{
  g95_todo_error ("Statement not implemented: SELECT");
}

tree
g95_trans_forall (g95_code *code ATTRIBUTE_UNUSED)
{
  g95_todo_error ("Statement not implemented: FORALL");
}

tree
g95_trans_where (g95_code *code ATTRIBUTE_UNUSED)
{
  g95_todo_error ("Statement not implemented: WHERE");
}

/* CYCLE a DO loop. The label decl has already been created by
   g95_trans_do(), it's in TREE_PURPOSE (backend_decl) of the g95_code
   node at the head of the loop. We must mark the label as used.  */
tree
g95_trans_cycle (g95_code *code)
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
g95_trans_exit (g95_code *code)
{
  tree exit_label;

  exit_label = TREE_VALUE (code->ext.whichloop->backend_decl);
  TREE_USED (exit_label) = 1;
  return build_stmt (GOTO_STMT, exit_label);
}

tree
g95_trans_allocate (g95_code *code ATTRIBUTE_UNUSED)
{
  g95_todo_error ("Statement not implemented: ALLOCATE");
}

tree
g95_trans_deallocate (g95_code *code ATTRIBUTE_UNUSED)
{
  g95_todo_error ("Statement not implemented: DEALLOCATE");
}

