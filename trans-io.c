/* IO Code translation/library interface
   Copyright (C) 2000, 2001, 2002 Free Software Foundation, Inc.
   Contributed by Paul Brook

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

/* trans-io.c-- generate GCC trees from g95_code */

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
#include <assert.h>
#include <gmp.h>
#define BACKEND_CODE
#include "g95.h"
#include "trans.h"
#include "trans-stmt.h"
#include "trans-array.h"
#include "trans-types.h"

/* This is a copy of the flags from gforio/write.h.  Duplicated because
   including all the gforio header files causes a whole load of other
   problems.  */
#include "gforio-flags.h"

tree
g95_trans_open (g95_code *code ATTRIBUTE_UNUSED)
{
  g95_todo_error ("IO statement not implemented: OPEN");
}

tree
g95_trans_close (g95_code *code ATTRIBUTE_UNUSED)
{
  g95_todo_error ("IO statement not implemented: CLOSE");
}

tree
g95_trans_read (g95_code *code ATTRIBUTE_UNUSED)
{
  g95_todo_error ("IO statement not implemented: READ");
}

tree
g95_trans_write (g95_code * code)
{
  /* The interface for the IO library isn't fixed, so this code will probably
     change a lot.  It was only written so that I could write a "Hello World"
     program :-)  */
  tree args;
  unsigned long flags;
  g95_dt *dt;
  g95_se se;
  tree head;
  tree tail;
  tree post;
  tree post_tail;
  tree tmp;

  g95_start_stmt ();

  dt = code->ext.dt;
  args = NULL_TREE;
  head = tail = NULL_TREE;
  post = post_tail = NULL_TREE;
  flags = 0;

/* TODO: error status codes.  */
  if (dt->io_unit)
    {
      g95_init_se (&se, NULL);
      g95_conv_simple_val_type (&se, dt->io_unit, g95_int4_type_node);
      g95_add_stmt_to_list (&head, &tail, se.pre, se.pre_tail);
      args = chainon (args, tree_cons (NULL_TREE, se.expr, NULL_TREE));
      flags |= GFORIO_WRITE_P_UNIT;
    }

  if (dt->advance)
    {
      g95_init_se (&se, NULL);
      g95_conv_simple_val_type (&se, dt->advance, g95_int4_type_node);
      g95_add_stmt_to_list (&head, &tail, se.pre, se.pre_tail);
      g95_add_stmt_to_list (&post, &post_tail, se.post, se.post_tail);
      args = g95_chainon_list (args, se.string_length);
      args = g95_chainon_list (args, se.expr);
      flags |= GFORIO_WRITE_P_ADVANCE;
    }

  if (dt->size)
    {
      g95_init_se (&se, NULL);
      g95_conv_simple_val_type (&se, dt->size, g95_int4_type_node);
      g95_add_stmt_to_list (&head, &tail, se.pre, se.pre_tail);
      args = chainon (args, tree_cons (NULL_TREE, se.expr, NULL_TREE));
      flags |= GFORIO_WRITE_P_REC;
    }

  if (dt->format_expr)
    {
      g95_init_se (&se, NULL);
      g95_conv_simple_val_type (&se, dt->format_expr, g95_int4_type_node);
      g95_add_stmt_to_list (&head, &tail, se.pre, se.pre_tail);
      g95_add_stmt_to_list (&post, &post_tail, se.post, se.post_tail);
      args = g95_chainon_list (args, se.string_length);
      args = g95_chainon_list (args, se.expr);
      flags |= GFORIO_WRITE_P_FMT;
    }

  args = tree_cons (NULL_TREE, build_int_2 (flags, 0), args);
  tmp = g95_build_function_call (g95_fndecl_write_begin, args);
  tmp = build_stmt (EXPR_STMT, tmp);
  g95_add_stmt_to_list (&head, &tail, tmp, tmp);

  g95_add_stmt_to_list (&head, &tail, post, post_tail);

  tmp = g95_finish_stmt (head, tail);

  return tmp;
}

tree
g95_trans_iolength (g95_code *code ATTRIBUTE_UNUSED)
{
  g95_todo_error ("IO statement not implemented: IOLENGTH");
}

tree
g95_trans_backspace (g95_code *code ATTRIBUTE_UNUSED)
{
  g95_todo_error ("IO statement not implemented: BACKSPACE");
}

tree
g95_trans_endfile (g95_code *code ATTRIBUTE_UNUSED)
{
  g95_todo_error ("IO statement not implemented: ENDFILE");
}

tree
g95_trans_inquire (g95_code *code ATTRIBUTE_UNUSED)
{
  g95_todo_error ("IO statement not implemented: INQUIRE");
}

tree
g95_trans_rewind (g95_code *code ATTRIBUTE_UNUSED)
{
  g95_todo_error ("IO statement not implemented: REWIND");
}

tree
g95_trans_io_call (g95_code * code)
{
  g95_ss *ss;
  g95_se se;
  g95_expr *expr;
  tree tmp;
  tree fndecl;
  int kind;
  g95_io_fndecl_enum fn;

  g95_start_stmt ();

  g95_init_se (&se, NULL);

  expr = code->ext.actual->expr;
  ss = g95_walk_expr (g95_ss_terminator, expr);
  if (ss != g95_ss_terminator)
    g95_todo_error ("array IO");

  g95_conv_simple_val (&se, expr);
  kind = expr->ts.kind;
  fndecl = NULL_TREE;
  fn = GFORIO_NUM_FNDECLS;
  switch (expr->ts.type)
    {
    case BT_INTEGER:
      if (kind == 4)
        fn = GFORIO_FNDECL_INT4;
      else if (kind == 8)
        fn = GFORIO_FNDECL_INT8;
      else
        fatal_error ("Can't do integer kind=%d IO", kind);
      break;

    case BT_REAL:
      if (kind == 4)
        fn = GFORIO_FNDECL_REAL4;
      else if (kind == 8)
        fn = GFORIO_FNDECL_REAL8;
      else
        fatal_error ("Can't do real kind=%d IO", kind);
      break;

    case BT_COMPLEX:
      if (kind == 4)
        fn = GFORIO_FNDECL_COMPLEX4;
      else if (kind == 8)
        fn = GFORIO_FNDECL_COMPLEX8;
      else
        fatal_error ("Can't do complex kind=%d IO", kind);
      break;

    case BT_LOGICAL:
      fn = GFORIO_FNDECL_LOGICAL;
      break;

    case BT_CHARACTER:
      if (kind == 1)
        fndecl = g95_fndecl_write_character;
      else
        fatal_error ("Can't do character kind=%d IO", kind);
      break;

    default:
      internal_error ("Bad IO basetype (%d)", expr->ts.type);
    }

  if (fndecl == NULL_TREE)
    {
      if (code->sub_name[4] == 'r')
        fndecl = g95_io_fndecls[fn].read;
      else if (code->sub_name[4] == 'w')
        fndecl = g95_io_fndecls[fn].write;
      else
        g95_todo_error ("%s", code->sub_name);
    }

  switch (expr->ts.type)
    {
    case BT_CHARACTER:
      g95_conv_string_parameter (&se);

      tmp = NULL_TREE;
      tmp = g95_chainon_list (tmp, se.string_length);
      tmp = g95_chainon_list (tmp, se.expr);
      break;

    case BT_LOGICAL:
      if (kind != 4)
        {
          tmp = build1 (NOP_EXPR, g95_logical4_type_node, se.expr);
          se.expr = g95_simple_fold (tmp, &se.pre, &se.pre_tail, NULL);
        }

      /* Fall through.  */

    default:
      tmp = tree_cons (NULL_TREE, se.expr, NULL_TREE);
      break;
    }

  tmp = g95_build_function_call (fndecl, tmp);
  tmp = build_stmt (EXPR_STMT, tmp);
  g95_add_stmt_to_pre (&se, tmp, tmp);
  g95_add_stmt_to_pre (&se, se.post, se.post_tail);

  tmp = g95_finish_stmt (se.pre, se.pre_tail);

  return tmp;
}

