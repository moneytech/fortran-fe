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
#include "coretypes.h"
#include "tree.h"
#include "tree-simple.h"
#include <stdio.h>
#include "ggc.h"
#include "toplev.h"
#include "real.h"
#include <assert.h>
#include <gmp.h>
#define BACKEND_CODE
#include "g95.h"
#include "trans.h"
#include "trans-stmt.h"
#include "trans-array.h"
#include "trans-types.h"

static GTY(()) tree gforio_pstate_type_node;
static GTY(()) tree gforio_state_unit;
static GTY(()) tree gforio_state_adv;
static GTY(()) tree gforio_state_advlen;
static GTY(()) tree gforio_state_fmt;
static GTY(()) tree gforio_state_fmtlen;
static GTY(()) tree gforio_state_rec;
static GTY(()) tree gforio_state_err;
static GTY(()) tree gforio_state_iostat;

tree g95_current_io_state = NULL_TREE;

/* External IO function decls.  */
struct GTY(()) gforio_fndecl_t
{
  char *name;
  tree *ptype;
  tree GTY(()) write;
  tree GTY(()) read;
};

typedef struct gforio_fndecl_t gforio_fndecl_t;

/* This must be consistent with gforio_fndecls.  */
typedef enum
{
  GFORIO_FNDECL_INT4=0,
  GFORIO_FNDECL_INT8,
  GFORIO_FNDECL_REAL4,
  GFORIO_FNDECL_REAL8,
  GFORIO_FNDECL_COMPLEX4,
  GFORIO_FNDECL_COMPLEX8,
  /* We convert all logical values to kind=4 before passing to IO.  */
  GFORIO_FNDECL_LOGICAL,
  GFORIO_NUM_FNDECLS
} gforio_fndecl_enum;

/* These must be consistent with gforio_fndecl_enum.  */
static GTY(()) gforio_fndecl_t gforio_fndecls[GFORIO_NUM_FNDECLS] =
{
  {"int4", &g95_int4_type_node, NULL_TREE, NULL_TREE},
  {"int8", &g95_int8_type_node, NULL_TREE, NULL_TREE},
  {"real4", &g95_real4_type_node, NULL_TREE, NULL_TREE},
  {"real8", &g95_real8_type_node, NULL_TREE, NULL_TREE},
  {"complex4", &g95_complex4_type_node, NULL_TREE, NULL_TREE},
  {"complex8", &g95_complex8_type_node, NULL_TREE, NULL_TREE},
  {"logical4", &g95_logical4_type_node, NULL_TREE, NULL_TREE}
};

/* IO library decls.  */
static GTY(()) tree gforio_fndecl_state_get;
static GTY(()) tree gforio_fndecl_write_begin;
static GTY(()) tree gforio_fndecl_write_end;
static GTY(()) tree gforio_fndecl_write_character;

static tree
gforio_addfield (tree stype, char * name, tree type)
{
  tree decl;

  decl = build_decl (FIELD_DECL, get_identifier (name), type);
  DECL_CONTEXT (decl) = stype;
  DECL_INITIAL (decl) = 0;
  DECL_ALIGN (decl) = 0;
  DECL_USER_ALIGN (decl) = 0;
  TYPE_FIELDS (stype) = chainon (TYPE_FIELDS (stype), decl);

  return decl;
}

static void
g95_init_io_state_type (void)
{
  tree type;

  type = make_node (RECORD_TYPE);
  TYPE_NAME (type) = get_identifier ("gforio_state");

  gforio_state_unit = gforio_addfield (type, "unit", g95_int4_type_node);
  gforio_state_adv = gforio_addfield (type, "advance", pchar_type_node);
  gforio_state_advlen =
    gforio_addfield (type, "advance_len", g95_strlen_type_node);
  gforio_state_fmt = gforio_addfield (type, "format", pchar_type_node);
  gforio_state_fmtlen =
    gforio_addfield (type, "format_len", g95_strlen_type_node);
  gforio_state_rec = gforio_addfield (type, "rec", g95_array_index_type);
  gforio_state_err = gforio_addfield (type, "err", g95_int4_type_node);
  gforio_state_iostat = gforio_addfield (type, "iostat", g95_int4_type_node);

  g95_finish_type (type);

  gforio_pstate_type_node = build_pointer_type (type);
}

/* Create function decls for IO library functions.  */
void
g95_build_io_library_fndecls (void)
{
  int i;
  char name[G95_MAX_SYMBOL_LEN+1];

  g95_init_io_state_type ();

  for (i = 0; i < GFORIO_NUM_FNDECLS; i++)
    {
      sprintf (name, "_gforio_write_%s", gforio_fndecls[i].name);
      gforio_fndecls[i].write =
        g95_build_library_function_decl (get_identifier (name),
                                        void_type_node,
                                        2, gforio_pstate_type_node,
                                        *gforio_fndecls[i].ptype);
      sprintf (name, "_gforio_read_%s", gforio_fndecls[i].name);
      gforio_fndecls[i].read =
        g95_build_library_function_decl (get_identifier (name),
                                        void_type_node,
                                        2, gforio_pstate_type_node,
                                        *gforio_fndecls[i].ptype);
    }
  gforio_fndecl_state_get =
    g95_build_library_function_decl (get_identifier ("_gforio_state_get"),
                                    gforio_pstate_type_node, 0);
  gforio_fndecl_write_begin =
    g95_build_library_function_decl (get_identifier ("_gforio_write_begin"),
                                    void_type_node,
                                    1, gforio_pstate_type_node);
  gforio_fndecl_write_end =
    g95_build_library_function_decl (get_identifier ("_gforio_write_end"),
                                    void_type_node,
                                    1, gforio_pstate_type_node);
  gforio_fndecl_write_character =
    g95_build_library_function_decl (get_identifier ("_gforio_write_character"),
                                    void_type_node,
                                    2, gforio_pstate_type_node,
                                    g95_strlen_type_node, pchar_type_node);
}

tree
g95_trans_open (g95_code * code ATTRIBUTE_UNUSED)
{
  g95_todo_error ("IO statement not implemented: OPEN");
}

tree
g95_trans_close (g95_code * code ATTRIBUTE_UNUSED)
{
  g95_todo_error ("IO statement not implemented: CLOSE");
}

tree
g95_trans_read (g95_code * code ATTRIBUTE_UNUSED)
{
  g95_todo_error ("IO statement not implemented: READ");
}

tree
g95_trans_write (g95_code * code)
{
  /* The IO library is still in the very early stages of development.  */
  tree args;
  unsigned long flags;
  g95_dt *dt;
  g95_se se;
  stmtblock_t block;
  stmtblock_t postblock;
  tree tmp;
  tree pstate;
  tree state;
  tree field;

  g95_start_block (&block);
  g95_init_block (&postblock);

  dt = code->ext.dt;
  args = NULL_TREE;
  flags = 0;

  tmp = g95_build_function_call (gforio_fndecl_state_get, NULL_TREE);
  if (! g95_current_io_state)
    g95_current_io_state = g95_create_var_np (TREE_TYPE (tmp), "iostate");

  pstate = g95_current_io_state;
  tmp = build (MODIFY_EXPR, TREE_TYPE (tmp), pstate, tmp);
  g95_add_expr_to_block (&block, tmp);

  state = build1 (INDIRECT_REF, TREE_TYPE (TREE_TYPE (pstate)), pstate);
/* TODO: IO error status codes.  */
  if (dt->io_unit)
    {
      field = gforio_state_unit;
      g95_init_se (&se, NULL);
      g95_conv_expr_type (&se, dt->io_unit, TREE_TYPE (field));
      g95_add_block_to_block (&block, &se.pre);

      tmp = build (COMPONENT_REF, TREE_TYPE (field), state, field);
      tmp = build (MODIFY_EXPR, TREE_TYPE (tmp), tmp, se.expr);
      g95_add_expr_to_block (&block, tmp);
    }

  if (dt->rec)
    {
      field = gforio_state_rec;
      g95_init_se (&se, NULL);
      g95_conv_expr_type (&se, dt->rec, TREE_TYPE (field));
      g95_add_block_to_block (&block, &se.pre);

      tmp = build (COMPONENT_REF, TREE_TYPE (field), state, field);
      tmp = build (MODIFY_EXPR, TREE_TYPE (tmp), tmp, se.expr);
      g95_add_expr_to_block (&block, tmp);
    }

  if (dt->advance)
    {
      field = gforio_state_adv;
      g95_init_se (&se, NULL);
      g95_conv_expr (&se, dt->advance);
      g95_conv_string_parameter (&se);
      g95_add_block_to_block (&block, &se.pre);
      g95_add_block_to_block (&postblock, &se.post);

      tmp = build (COMPONENT_REF, TREE_TYPE (field), state, field);
      tmp = build (MODIFY_EXPR, TREE_TYPE (tmp), tmp, se.expr);
      g95_add_expr_to_block (&block, tmp);

      field = gforio_state_advlen;
      tmp = build (COMPONENT_REF, TREE_TYPE (field), state, field);
      tmp = build (MODIFY_EXPR, TREE_TYPE (tmp), tmp, se.string_length);
      g95_add_expr_to_block (&block, tmp);
    }

  if (dt->format_expr)
    {
      field = gforio_state_fmt;
      g95_init_se (&se, NULL);
      g95_conv_expr (&se, dt->format_expr);
      g95_conv_string_parameter (&se);
      g95_add_block_to_block (&block, &se.pre);
      g95_add_block_to_block (&postblock, &se.post);

      tmp = build (COMPONENT_REF, TREE_TYPE (field), state, field);
      tmp = build (MODIFY_EXPR, TREE_TYPE (tmp), tmp, se.expr);
      g95_add_expr_to_block (&block, tmp);

      field = gforio_state_fmtlen;
      tmp = build (COMPONENT_REF, TREE_TYPE (field), state, field);
      tmp = build (MODIFY_EXPR, TREE_TYPE (tmp), tmp, se.string_length);
      g95_add_expr_to_block (&block, tmp);
    }

  if (dt->size)
    g95_todo_error ("SIZE qualifier on write statement");

  args = g95_chainon_list (NULL_TREE, pstate);
  tmp = g95_build_function_call (gforio_fndecl_write_begin, args);
  g95_add_expr_to_block (&block, tmp);

  g95_add_block_to_block (&block, &postblock);

  return g95_finish_block (&block);
}

tree
g95_trans_iolength (g95_code * code ATTRIBUTE_UNUSED)
{
  g95_todo_error ("IO statement not implemented: IOLENGTH");
}

tree
g95_trans_backspace (g95_code * code ATTRIBUTE_UNUSED)
{
  g95_todo_error ("IO statement not implemented: BACKSPACE");
}

tree
g95_trans_endfile (g95_code * code ATTRIBUTE_UNUSED)
{
  g95_todo_error ("IO statement not implemented: ENDFILE");
}

tree
g95_trans_inquire (g95_code * code ATTRIBUTE_UNUSED)
{
  g95_todo_error ("IO statement not implemented: INQUIRE");
}

tree
g95_trans_rewind (g95_code * code ATTRIBUTE_UNUSED)
{
  g95_todo_error ("IO statement not implemented: REWIND");
}

tree
g95_trans_iostate_call (g95_code * code)
{
  tree tmp;

  if (strcmp (code->sub_name, "_gforio_write_end") == 0)
    {
      assert (g95_current_io_state);
      tmp = g95_chainon_list (NULL_TREE, g95_current_io_state);
      tmp = g95_build_function_call (gforio_fndecl_write_end, tmp);
      return tmp;
    }
  else
    internal_error ("Unknown IO state call %s\n", code->sub_name);
}

/* Generate the IO statements for an expression.  */
static void
g95_trans_expr_io (g95_se * se, g95_typespec * ts, int mode)
{
  tree args;
  tree tmp;
  int kind;
  tree fndecl;
  gforio_fndecl_enum fn;

  fndecl = NULL_TREE;
  fn = GFORIO_NUM_FNDECLS;
  kind = ts->kind;
  switch (ts->type)
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
        fndecl = gforio_fndecl_write_character;
      else
        fatal_error ("Can't do character kind=%d IO", kind);
      break;

    case BT_DERIVED:
      g95_todo_error ("IO of derived types");
      break;

    default:
      internal_error ("Bad IO basetype (%d)", ts->type);
    }

  if (fndecl == NULL_TREE)
    {
      if (mode == 'r')
        fndecl = gforio_fndecls[fn].read;
      else if (mode == 'w')
        fndecl = gforio_fndecls[fn].write;
      else
        g95_todo_error ("IO mode %c", mode);
    }

  assert (g95_current_io_state);
  args = g95_chainon_list (NULL_TREE, g95_current_io_state);
  switch (ts->type)
    {
    case BT_CHARACTER:
      g95_conv_string_parameter (se);

      args = g95_chainon_list (args, se->string_length);
      args = g95_chainon_list (args, se->expr);
      break;

    case BT_LOGICAL:
      /* Logical kinds are always passed as kind=4.  */
      if (kind != 4)
        se->expr = convert (g95_logical4_type_node, se->expr);

      /* Fall through.  */

    default:
      args = g95_chainon_list (args, se->expr);
      break;
    }

  tmp = g95_build_function_call (fndecl, args);
  g95_add_expr_to_block (&se->pre, tmp);
  g95_add_block_to_block (&se->pre, &se->post);
}

tree
g95_trans_io_call (g95_code * code)
{
  g95_ss *ss;
  g95_se se;
  g95_expr *expr;
  g95_loopinfo loop;
  tree tmp;
  stmtblock_t block;
  stmtblock_t body;

  g95_start_block (&block);

  expr = code->ext.actual->expr;
  ss = g95_walk_expr (g95_ss_terminator, expr);

  g95_init_se (&se, NULL);

  if (ss != g95_ss_terminator)
    {
      /* Initialize the scalarizer.  */
      g95_init_loopinfo (&loop);
      g95_add_ss_to_loop (&loop, ss);

      /* Initialize the loop.  */
      g95_conv_ss_startstride (&loop);
      g95_conv_loop_setup (&loop);

      /* The main loop body.  */
      g95_mark_ss_chain_used (ss, 1);
      g95_start_scalarized_body (&loop, &body);

      g95_copy_loopinfo_to_se (&se, &loop);
      se.ss = ss;
    }
  else
    g95_init_block (&body);

  g95_conv_expr (&se, expr);

  g95_trans_expr_io (&se, &expr->ts, code->sub_name[4]);

  g95_add_block_to_block (&body, &se.pre);
  g95_add_block_to_block (&body, &se.post);

  if (se.ss)
    {
      assert (se.ss == g95_ss_terminator);
      g95_trans_scalarizing_loops (&loop, &body);

      g95_add_block_to_block (&loop.pre, &loop.post);

      tmp = g95_finish_block (&loop.pre);
      g95_add_expr_to_block (&block, tmp);
    }
  else
    {
      tmp = g95_finish_block (&body);
      g95_add_expr_to_block (&block, tmp);
    }

  return g95_finish_block (&block);;
}

#include "gt-f95-trans-io.h"
