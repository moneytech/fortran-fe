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
#include <string.h>
#include "c-common.h"
#include "ggc.h"
#include "rtl.h"
#include "toplev.h"
#include "function.h"
#include "expr.h"
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

/* Conversions between different types are outputed by the frontend as
   intrinsic functions.  We implement these directly.  */
static void
g95_conv_intrinsic_conversion (g95_se * se, g95_expr * expr,
                              g95_intrinsic_sym * isym)
{
  tree type;

  /* Evaluate the argument.  */
  type = g95_typenode_for_spec (&isym->ts);
  assert (expr->value.function.actual->expr);
  g95_conv_simple_rhs (se, expr->value.function.actual->expr);

  /* Conversion from complex to real/integer involves taking the
     real component.  */
  if (isym->ts.type == BT_COMPLEX)
    {
      se->expr = g95_simple_fold (se->expr, &se->pre, &se->pre_tail, NULL);
      se->expr = build1 (REALPART_EXPR, TREE_TYPE (TREE_TYPE (se->expr)),
                         se->expr);
    }

  /* Convert it to the required type.  */
  if (TREE_TYPE (se->expr) != type)
    {
      se->expr = g95_simple_fold (se->expr, &se->pre, &se->pre_tail, NULL);
      se->expr = g95_simple_convert (type, se->expr);
    }
}

/* Get the imaginary component of a value.  */
static void
g95_conv_intrinsic_imagpart (g95_se * se, g95_expr * expr,
                             g95_intrinsic_sym * isym)
{
  assert (expr->ts.kind == isym->ts.kind);

  g95_conv_simple_val (se, expr->value.function.actual->expr);
  se->expr =
    build1 (IMAGPART_EXPR, TREE_TYPE (TREE_TYPE (se->expr)), se->expr);
}

/* Evaluate the arguments to an intrinsic function.  */
static tree
g95_conv_intrinsic_function_args (g95_se * se, g95_expr * expr)
{
  g95_actual_arglist *actual;
  tree args;
  g95_se argse;

  args = NULL_TREE;
  for (actual = expr->value.function.actual; actual; actual = actual->next)
    {
      if (se->ss)
        {
          /* Substitute a scalar that has been taken out of the loop.  */
          assert (se->ss->type == G95_SS_SCALAR);
          args = g95_chainon_list (args, se->ss->data.scalar);
          g95_advance_se_ss_chain (se);
        }
      else
        {
          /* Evaluate the parameter.  */
          g95_init_se (&argse, se);
          g95_conv_simple_val (&argse, actual->expr);
          g95_add_stmt_to_pre (se, argse.pre, argse.pre_tail);
          g95_add_stmt_to_post (se, argse.post, argse.post_tail);

          args = g95_chainon_list (args, argse.expr);
        }
    }
  return args;
}

/* This maps fortran intrinsic functions to external library or GCC builtin
   functions.  */
typedef struct GTY(())
{
  const char *fe_name;
  tree GTY(()) fe_id;
  const char *lib_name;
  int code;
  tree GTY(()) fndecl;
} g95_intrinsic_map_t;

#define I_LIB(fe, lib) {fe, NULL_TREE, lib, 0, NULL_TREE},
#define I_BUILTIN(fe, code) {fe, NULL_TREE, NULL, code, NULL_TREE},
static GTY(()) g95_intrinsic_map_t g95_intrinsic_map[] =
{
  /* Math functions.  These are in libm.  */
I_BUILTIN ("sin", BUILT_IN_SINF)
I_BUILTIN ("dsin", BUILT_IN_SIN)
I_BUILTIN ("cos", BUILT_IN_COS)
I_BUILTIN ("dcos", BUILT_IN_COSF)
I_BUILTIN ("sqrt", BUILT_IN_SQRT)
I_BUILTIN ("dsqrt", BUILT_IN_SQRTF)

I_LIB("csin", "csinf")
I_LIB("zsin", "csin")
I_LIB("ccos", "ccosf")
I_LIB("zcos", "ccos")
I_LIB("csqrt", "csqrtf")
I_LIB("zsqrt", "csqrt")

I_LIB("tan", "tanf")
I_LIB("dtan", "tan")
I_LIB("ctan", "ctanf")
I_LIB("ztan", "ctan")

I_LIB("asin", "asinf")
I_LIB("dasin", "asin")
I_LIB("acos", "acosf")
I_LIB("dacos", "acos")
I_LIB("atan", "atanf")
I_LIB("datan", "atan")
I_LIB("atan2", "atan2f")
I_LIB("datan2", "atan2")

I_LIB("sinh", "sinhf")
I_LIB("dsinh", "sinh")
I_LIB("cosh", "coshf")
I_LIB("dcosh", "cosh")
I_LIB("tanh", "tanhf")
I_LIB("dtanh", "tanh")

I_LIB("exp", "expf")
I_LIB("dexp", "exp")
I_LIB("cexp", "cexpf")
I_LIB("zexp", "cexp")

I_LIB("alog", "logf")
I_LIB("dlog", "log")
I_LIB("clog", "clogf")
I_LIB("zlog", "clog")

I_LIB("alog10", "log10f")
I_LIB("dlog10", "log10")
I_LIB("clog10", "clog10f")
I_LIB("zlog10", "clog10")

  {NULL, NULL_TREE, NULL, 0, NULL_TREE}
};
#undef I_LIB
#undef I_BUILTIN

/* Initialize function decls for library functions.  Identifier nodes are used
   to speed up the search.  The actual decls are not created until the
   function is used.  */
void
g95_build_intrinsic_lib_fndecls ()
{
  g95_intrinsic_map_t *m;

  for (m = g95_intrinsic_map; m->fe_name; m++)
    {
      m->fe_id = get_identifier (m->fe_name);
    }
}

/* Create a fndecl for a simple intrinsic library function.  */
static tree
g95_get_intrinsic_lib_fndecl (g95_intrinsic_map_t * m,
                              g95_intrinsic_sym * isym)
{
  tree type;
  tree argtypes;
  tree fndecl;
  g95_intrinsic_arg *formal;

  if (m->fndecl)
    return m->fndecl;

  if (! m->lib_name)
    return built_in_decls[m->code];

  argtypes = NULL_TREE;
  for (formal = isym->formal; formal; formal = formal->next)
    {
      type = g95_typenode_for_spec (&formal->ts);
      argtypes= g95_chainon_list (argtypes, type);
      assert (! formal->optional);
    }
  argtypes = g95_chainon_list (argtypes, void_type_node);
  type = g95_typenode_for_spec (&isym->ts);
  type = build_function_type (type, argtypes);
  fndecl = build_decl (FUNCTION_DECL, get_identifier (m->lib_name), type);

  /* Mark the decl as external.  */
  DECL_EXTERNAL (fndecl) = 1;
  TREE_PUBLIC (fndecl) = 1;

  rest_of_decl_compilation (fndecl, NULL, 1, 0);

  m->fndecl = fndecl;
  return fndecl;
}

/* Convert an intrinsic function into an external or builtin call.  */
static void
g95_conv_intrinsic_lib_function (g95_se * se, g95_expr * expr,
                                 g95_intrinsic_sym * isym)
{
  g95_intrinsic_map_t *m;
  tree args;
  tree fndecl;
  tree id;

  /* Find the entry for this function.  */
  id = get_identifier (&isym->lib_name[2]);
  for (m = g95_intrinsic_map; m->fe_name; m++)
    {
      if (m->fe_id == id)
        break;
    }

  if (! m->fe_name)
    {
      internal_error ("Intrinsic function %s(%s) not implemented",
                      isym->name, isym->lib_name);
    }

  /* Get the decl and generate the call.  */
  args = g95_conv_intrinsic_function_args (se, expr);
  fndecl = g95_get_intrinsic_lib_fndecl (m, isym);
  se->expr = g95_build_function_call (fndecl, args);
}

/* UBOUND and LBOUND.  A known second parameter is handled directly, otherwise
   a switch statement is generated.  */
static void
g95_conv_intrinsic_bound (g95_se * se, g95_expr * expr, int upper)
{
  g95_actual_arglist *arg;
  g95_actual_arglist *arg2;
  g95_se argse;
  tree type;
  tree bound;
  tree desc;
  tree stmt;
  tree head;
  tree tail;
  tree res;
  tree tmp;
  tree label;
  int n;

  arg = expr->value.function.actual;
  arg2 = arg->next;

  assert (arg->expr->expr_type == EXPR_VARIABLE);
  assert (arg->expr->symbol->attr.dimension);

  if (se->ss)
    {
      /* Make a second argument from the implicit loop variable.  */
      assert (! arg2->expr);
      assert (se->loop->dimen == 1);
      assert (se->ss->expr == expr);
      g95_advance_se_ss_chain (se);

      bound = se->loop->loopvar[0];
      bound = build (MINUS_EXPR, g95_array_index_type, bound,
                     se->loop->from[0]);
      bound = g95_simple_fold (bound, &se->pre, &se->pre_tail, NULL);
    }
  else
    {
      /* Convert the second argument.  */
      assert (arg2->expr);
      g95_init_se (&argse, NULL);
      g95_conv_simple_val_type (&argse, arg2->expr, g95_array_index_type);
      g95_add_stmt_to_pre (se, argse.pre, argse.pre_tail);
      /* Convert from one based to zero based.  */
      bound = build (MINUS_EXPR, g95_array_index_type, argse.expr,
                     integer_one_node);
      bound = g95_simple_fold (bound, &se->pre, &se->pre_tail, NULL);
    }

  /* Get the descriptor of the first argument.  */
  g95_init_se (&argse, NULL);
  argse.want_pointer = 1;
  g95_conv_simple_rhs (&argse, arg->expr);
  g95_add_stmt_to_pre (se, argse.pre, argse.pre_tail);
  g95_add_stmt_to_post (se, argse.post, argse.post_tail);
  desc = argse.expr;
  assert (G95_DESCRIPTOR_TYPE_P (TREE_TYPE (desc)));

  if (INTEGER_CST_P (bound))
    {
      /* We know the second argument, so supply the bound directly.  */
      assert (TREE_INT_CST_HIGH (bound) == 0);
      n = TREE_INT_CST_LOW (bound);
      assert (n >= 0 && n < G95_TYPE_DESCRIPTOR_RANK (TREE_TYPE (desc)));

      if (upper)
        se->expr = g95_conv_array_ubound (desc, n);
      else
        se->expr = g95_conv_array_lbound (desc, n);
      se->expr = g95_simple_fold (se->expr, &se->pre, &se->pre_tail, NULL);

      /* Convert to the correct type.  */
      type = g95_typenode_for_spec (&expr->ts);
      se->expr = g95_simple_convert (type, se->expr);
    }
  else
    {
      /* Implement an array bound query as a switch statement.  */
      /* Create a variable to hold the result.  */
      res = g95_create_tmp_var (g95_array_index_type);

      /* Create the body of the switch statement.  */
      g95_start_stmt ();
      head = tail = NULL_TREE;
      for (n = 0; n < G95_TYPE_DESCRIPTOR_RANK (TREE_TYPE (desc)); n++)
        {
          /* Create a label for this case.  */
          label = build_decl (LABEL_DECL, NULL_TREE, NULL_TREE);

          /* Build the case marker.  */
          tmp = build_int_2 (n, 0);
          stmt = build_stmt (CASE_LABEL, tmp, NULL_TREE, label);
          g95_add_stmt_to_list (&head, &tail, stmt, stmt);

          if (upper)
            tmp = g95_conv_array_ubound (desc, n);
          else
            tmp = g95_conv_array_lbound(desc, n);

          /* Get the bound.  */
          tmp = build (MODIFY_EXPR, g95_array_index_type, res, tmp);
          stmt = build_stmt (EXPR_STMT, tmp);
          g95_add_stmt_to_list (&head, &tail, stmt, stmt);

          stmt = build_stmt (BREAK_STMT);
          g95_add_stmt_to_list (&head, &tail, stmt, stmt);
        }

      if (flag_bounds_check)
        {
          /* Create a default case to generate an bound error.  */
          label = build_decl (LABEL_DECL, NULL_TREE, NULL_TREE);
          stmt = build_stmt (CASE_LABEL, NULL_TREE, NULL_TREE, label);
          g95_add_stmt_to_list (&head, &tail, stmt, stmt);

          g95_trans_runtime_check (integer_one_node, g95_strconst_fault,
                                   &head, &tail);
        }
      /* Construct the case statement.  */
      stmt = g95_finish_stmt (head, tail);
      stmt = build_stmt (SWITCH_STMT, bound, stmt);
      g95_add_stmt_to_pre (se, stmt, stmt);

      /* Convert the value to the required type.  */
      tmp = g95_typenode_for_spec (&expr->ts);
      se->expr = g95_simple_convert (tmp, res);
    }
}

/* Generate code for an intrinsic function.  Some map directly to library
   calls, others get special handling.  */
void
g95_conv_intrinsic_function (g95_se * se, g95_expr * expr)
{
  g95_intrinsic_sym *isym;
  char *name;

  if (se->ss && se->ss->type == G95_SS_SCALAR)
    {
      se->expr = se->ss->data.scalar;
      return;
    }

  isym = expr->value.function.isym;

  assert (strncmp (isym->lib_name, "__", 2) == 0);
  name = &isym->lib_name[2];

  if (strncmp (name, "convert_", 7) == 0
      || strcmp (name, "real") == 0
      || strcmp (name, "sngl") == 0
      || strcmp (name, "float") == 0
      || strcmp (name, "int") == 0
      || strcmp (name, "ifix") == 0
      || strcmp (name, "idint") == 0)
    g95_conv_intrinsic_conversion (se, expr, isym);
  else if (strcmp (name, "aimag") == 0
           || strcmp (name, "dimag") == 0)
    g95_conv_intrinsic_imagpart (se, expr, isym);
  else if (strcmp (name, "ubound") == 0
           || strcmp (name, "lbound") == 0)
    g95_conv_intrinsic_bound (se, expr, name[0] == 'u');
  else
    g95_conv_intrinsic_lib_function (se, expr, isym);
}

/* This generates code to execute before entering the scalarization loop.
   Currently does nothing.  */
void
g95_add_intrinsic_ss_code (g95_loopinfo * loop ATTRIBUTE_UNUSED, g95_ss * ss ATTRIBUTE_UNUSED)
{
  return;
}

/* UBOUND and LBOUND intrinsics with one parameter are expanded into a case
   statement inside the scalarization loop.  */
static g95_ss *
g95_walk_intrinsic_bound (g95_ss * ss, g95_expr * expr)
{
  g95_ss *newss;

  if (expr->value.function.actual->next->expr)
    return ss;

  newss = g95_get_ss ();
  newss->type = G95_SS_INTRINSIC;
  newss->expr = expr;
  newss->next = ss;

  return newss;
}

/* Walk an intrinsic function.  */
g95_ss *
g95_walk_intrinsic_function (g95_ss * ss, g95_expr * expr,
                             g95_intrinsic_sym * isym)
{
  if (! isym)
    fatal_error ("no isym for %s", expr->value.function.name);

  if (isym->elemental)
    return g95_walk_elemental_function_args (ss, expr, G95_SS_SCALAR);

  /* Special cases.  */
  if (strcmp (isym->name, "ubound") == 0
      || strcmp (isym->name, "lbound") == 0)
    return g95_walk_intrinsic_bound (ss, expr);

  /* These can probably be handles in the same way as normal functions.  */
  g95_todo_error ("Scalarization of non-elemental intrinsic: %s(%s)",
                  isym->name, isym->lib_name);
}

