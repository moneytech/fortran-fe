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

/* Intrinsic functions still to implement:
    Numeric inquiry functions (eg. KIND, HUGE) should be handled by the
    frontend.
   ceiling
   floor
   mod
   modulo
   [a]ninit

   All the character functions

   logical

   btest
   ibtclr
   ibits
   ibset
   ieor
   ior
   ishift
   ishiftc
   not
   transfer

   exponent
   fraction
   nearest
   rrspacing
   scale
   set_exponent
   spacing

   dot_product
   matmul
   all
   any
   count
   maxval
   minval
   product
   sum
   allocated
   shape
   size
   merge
   pack
   spread
   unpack
   reshape
   cshift
   eoshift
   transpose
   maxloc
   minloc

   associated

   cpu_time
   date_and_time
   mvbits
   random_number
   random_seed
   system_clock

   sum is partialy implemented
 */

/* This maps fortran intrinsic math functions to external library or GCC
   builtin functions.  */
typedef struct GTY(())
{
  const char *name;
  int len;
  tree GTY(()) real4_decl;
  tree GTY(()) real8_decl;
  tree GTY(()) complex4_decl;
  tree GTY(()) complex8_decl;
} g95_intrinsic_map_t;

#define I_LIB(fe) {fe, -1, NULL_TREE, NULL_TREE, NULL_TREE, NULL_TREE},
static GTY(()) g95_intrinsic_map_t g95_intrinsic_map[] =
{
  /* Math functions.  These are in libm.  */
I_LIB("sin")
I_LIB("cos")
I_LIB("sqrt")
I_LIB("tan")

I_LIB("asin")
I_LIB("acos")
I_LIB("atan")
I_LIB("atan2")

I_LIB("sinh")
I_LIB("cosh")
I_LIB("tanh")

I_LIB("exp")
I_LIB("log")
I_LIB("log10")

  {NULL, -1, NULL_TREE, NULL_TREE, NULL_TREE, NULL_TREE}
};
#undef I_LIB

typedef struct
{
  const char *name;
  const int code4;
  const int code8;
} g95_builtin_intrinsic_t;

static const g95_builtin_intrinsic_t g95_builtin_intrinsics[]=
{
  {"sin", BUILT_IN_SINF, BUILT_IN_SIN},
  {"cos", BUILT_IN_COSF, BUILT_IN_COS},
  {"sqrt", BUILT_IN_SQRTF, BUILT_IN_SQRT},
  {NULL, 0, 0}
};

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
      /* Skip ommitted optional arguments.  */
      if (! actual->expr)
        continue;

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

/* Conversions between different types are outputed by the frontend as
   intrinsic functions.  We implement these directly.  */
static void
g95_conv_intrinsic_conversion (g95_se * se, g95_expr * expr)
{
  tree type;
  tree arg;

  /* Evaluate the argument.  */
  type = g95_typenode_for_spec (&expr->ts);
  assert (expr->value.function.actual->expr);
  arg = g95_conv_intrinsic_function_args (se, expr);
  arg = TREE_VALUE (arg);

  /* Conversion of complex types needs special handling.  */
  if (TREE_CODE (TREE_TYPE (arg)) == COMPLEX_TYPE)
    {
      tree real;
      tree imag;
      tree artype;

      artype = TREE_TYPE (TREE_TYPE (arg));
      if (expr->ts.type == BT_COMPLEX)
        {
          /* The standard convert() routine does not produce a SIMPLE
             expression for conversion of complex values.  */
          real = build1 (REALPART_EXPR, artype, arg);
          real = convert (TREE_TYPE (type), real);
          real = g95_simple_fold (real, &se->pre, &se->pre_tail, NULL);
          imag = build1 (IMAGPART_EXPR, artype, arg);
          imag = convert (TREE_TYPE (type), imag);
          imag = g95_simple_fold (imag, &se->pre, &se->pre_tail, NULL);
          se->expr = build (COMPLEX_EXPR, type, real, imag);
          return;
        }
      else
        {
          /* Conversion from complex to real/integer involves taking the
             real component.  */
          arg = build1 (REALPART_EXPR, artype, arg);
        }
    }

  /* Convert it to the required type.  */
  if (TREE_TYPE (arg) != type)
    {
      arg = g95_simple_fold (arg, &se->pre, &se->pre_tail, NULL);
      arg = g95_simple_convert (type, arg);
    }

  se->expr = arg;
}

/* Get the imaginary component of a value.  */
static void
g95_conv_intrinsic_imagpart (g95_se * se, g95_expr * expr)
{
  tree arg;

  arg = g95_conv_intrinsic_function_args (se, expr);
  arg = TREE_VALUE (arg);
  se->expr = build1 (IMAGPART_EXPR, TREE_TYPE (TREE_TYPE (arg)), arg);
}

/* Get the complex conjugate of a value.  */
static void
g95_conv_intrinsic_conjg (g95_se * se, g95_expr * expr)
{
  tree arg;

  arg = g95_conv_intrinsic_function_args (se, expr);
  arg = TREE_VALUE (arg);
  se->expr = build1 (CONJ_EXPR, TREE_TYPE (arg), arg);
}

/* Initialize function decls for library functions.  The external functions
   are created as required.  Builtin functions are added here.  */
void
g95_build_intrinsic_lib_fndecls ()
{
  const g95_builtin_intrinsic_t *i;
  g95_intrinsic_map_t *m;

  /* Fill in the string lengths.  */
  for (m = g95_intrinsic_map; m->name; m++)
    m->len = strlen (m->name);

  /* Add GCC builting functions.  */
  for (i = g95_builtin_intrinsics; i->name; i++)
    {
      for (m = g95_intrinsic_map; m->name; m++)
        {
          if (strcmp (m->name, i->name) == 0)
            break;
        }

      assert (m->name);

      m->real4_decl = built_in_decls[i->code4];
      m->real8_decl = built_in_decls[i->code8];
    }
}

/* Create a fndecl for a simple intrinsic library function.  */
static tree
g95_get_intrinsic_lib_fndecl (g95_intrinsic_map_t * m, g95_expr * expr)
{
  tree type;
  tree argtypes;
  tree fndecl;
  g95_actual_arglist *actual;
  tree *pdecl;
  g95_typespec *ts;
  char name[G95_MAX_SYMBOL_LEN+3];

  ts = &expr->ts;
  name[0] = 0;
  if (ts->type == BT_REAL)
    {
      switch (ts->kind)
        {
        case 4: pdecl = &m->real4_decl; break;
        case 8: pdecl = &m->real8_decl; break;
        default: abort();
        }
    }
  else if (ts->type == BT_COMPLEX)
    {
      name[0] = 'c';
      name[1] = 0;
      switch (ts->kind)
        {
        case 4: pdecl = &m->complex4_decl; break;
        case 8: pdecl = &m->complex8_decl; break;
        default: abort();
        }
    }
  else
    abort ();

  if (*pdecl)
    return *pdecl;

  type = g95_typenode_for_spec (ts);
  argtypes = NULL_TREE;

  for (actual = expr->value.function.actual; actual; actual = actual->next)
    {
      if (! g95_compare_types (&actual->expr->ts, ts))
        {
          internal_error ("arg types for intrinsic %s do not match",
                          expr->value.function.name);
        }
      argtypes= g95_chainon_list (argtypes, type);
    }

  strcat (name, m->name);
  if (ts->kind == 4)
    strcat (name, "f");
  else
    assert (ts->kind == 8);

  argtypes = g95_chainon_list (argtypes, void_type_node);
  type = build_function_type (type, argtypes);
  fndecl = build_decl (FUNCTION_DECL, get_identifier (name), type);

  /* Mark the decl as external.  */
  DECL_EXTERNAL (fndecl) = 1;
  TREE_PUBLIC (fndecl) = 1;

  rest_of_decl_compilation (fndecl, NULL, 1, 0);

  (*pdecl) = fndecl;
  return fndecl;
}

/* Convert an intrinsic function into an external or builtin call.  */
static void
g95_conv_intrinsic_lib_function (g95_se * se, g95_expr * expr,
                                 const char *name)
{
  g95_intrinsic_map_t *m;
  tree args;
  tree fndecl;

  /* Find the entry for this function.  */
  for (m = g95_intrinsic_map; m->name; m++)
    {
      if (strncmp (m->name, name, m->len) == 0
          && name[m->len] == '_')
        break;
    }

  if (! m->name)
    {
      internal_error ("Intrinsic function %s not implemented",
                      expr->value.function.name);
    }

  /* Get the decl and generate the call.  */
  args = g95_conv_intrinsic_function_args (se, expr);
  fndecl = g95_get_intrinsic_lib_fndecl (m, expr);
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
      res = create_tmp_var (g95_array_index_type, "bound");

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

static void
g95_conv_intrinsic_abs (g95_se * se, g95_expr * expr)
{
  tree args;
  tree val;
  tree fndecl;

  args = g95_conv_intrinsic_function_args (se, expr);
  assert (args && TREE_CHAIN (args) == NULL_TREE);
  val = TREE_VALUE (args);

  switch (expr->ts.type)
    {
    case BT_INTEGER:
    case BT_REAL:
      se->expr = build1 (ABS_EXPR, TREE_TYPE (val), val);
      break;

    case BT_COMPLEX:
      switch (expr->ts.kind)
        {
        case 4: fndecl = g95_fndecl_math_cabsf; break;
        case 8: fndecl = g95_fndecl_math_cabs; break;
        default: abort();
        }
      se->expr = g95_build_function_call (fndecl, args);
      break;

    default:
      abort();
    }
}

/* Create a complex value from one or two real components.  */
static void
g95_conv_intrinsic_cmplx (g95_se * se, g95_expr * expr, int both)
{
  tree arg;
  tree real;
  tree imag;
  tree type;

  type = g95_typenode_for_spec (&expr->ts);
  arg = g95_conv_intrinsic_function_args (se, expr);
  real = g95_simple_convert (TREE_TYPE (type), TREE_VALUE (arg));
  real = g95_simple_fold (real, &se->pre, &se->pre_tail, NULL);
  arg = TREE_CHAIN (arg);
  if (both)
    {
      imag = g95_simple_convert (TREE_TYPE (type), TREE_VALUE (arg));
      imag = g95_simple_fold (imag, &se->pre, &se->pre_tail, NULL);
    }
  else
    imag = build_real_from_int_cst (TREE_TYPE (type), integer_zero_node);

  se->expr = fold (build (COMPLEX_EXPR, type, real, imag));
}

/* Positive difference DIM (x, y) = (x > y) ? x - y : 0.  */
static void
g95_conv_intrinsic_dim (g95_se * se, g95_expr * expr)
{
  tree arg;
  tree arg2;
  tree pstmt;
  tree zstmt;
  tree val;
  tree tmp;
  tree type;

  arg = g95_conv_intrinsic_function_args (se, expr);
  arg2 = TREE_VALUE (TREE_CHAIN (arg));
  arg = TREE_VALUE (arg);
  type = TREE_TYPE (arg);
  val = create_tmp_var (type, "dim");

  tmp = build (MINUS_EXPR, type, arg, arg2);
  tmp = build (MODIFY_EXPR, type, val, tmp);
  pstmt = build_stmt (EXPR_STMT, tmp);

  tmp = g95_build_const (type, integer_zero_node);
  tmp = build (MODIFY_EXPR, type, val, tmp);
  zstmt = build_stmt (EXPR_STMT, tmp);

  tmp = build (GT_EXPR, type, arg, arg2);
  tmp = build_stmt (IF_STMT, tmp, pstmt, zstmt);
  g95_add_stmt_to_pre (se, tmp, tmp);

  se->expr = val;
}

/* SIGN(A, B) is absolute value of A times sign of B.  Implement this by
   negating A if A and B have differing signs.
    sign (a, b)
    {
      cond = (a >= 0);
      tmp = (b < 0);
      cond = cond && tmp;
      cond2 = (a < 0);
      tmp = (b >= 0);
      cond2 = cond2 && tmp;
      if (cond || cond2)
        val = -a;
      else
        val = a;
      return val;
    }
   This should work for negative zero provided -0.0 < 0.0
 */
static void
g95_conv_intrinsic_sign (g95_se * se, g95_expr * expr)
{
  tree arg;
  tree arg2;
  tree type;
  tree zero;
  tree tmp;
  tree tmpvar;
  tree cond;
  tree cond2;
  tree condvar;
  tree condvar2;
  tree pstmt;
  tree nstmt;
  tree stmt;
  tree val;

  arg = g95_conv_intrinsic_function_args (se, expr);
  arg2 = TREE_VALUE (TREE_CHAIN (arg));
  arg = TREE_VALUE (arg);
  type = TREE_TYPE (arg);
  zero = g95_build_const (type, integer_zero_node);

  condvar = NULL_TREE;
  condvar2 = NULL_TREE;
  tmpvar = NULL_TREE;
  /* cond = (A >= 0);
     if (B >= 0)
       cond = 0;  */
  cond = create_tmp_var (boolean_type_node, "neg");
  tmp = fold (build (GE_EXPR, boolean_type_node, arg, zero));
  tmp = build (MODIFY_EXPR, boolean_type_node, cond, tmp);
  stmt = build_stmt (EXPR_STMT, tmp);
  g95_add_stmt_to_pre (se, stmt, stmt);

  tmp = build (MODIFY_EXPR, boolean_type_node, cond, integer_zero_node);
  stmt = build_stmt (EXPR_STMT, tmp);
  tmp = fold (build (GE_EXPR, boolean_type_node, arg2, zero));
  stmt = build_stmt (IF_STMT, tmp, stmt, NULL_TREE);
  g95_add_stmt_to_pre (se, stmt, stmt);

  /* cond2 = (A < 0);
     if (B < 0)
       cond2 = 0;  */
  cond2 = create_tmp_var (boolean_type_node, "neg");
  tmp = fold (build (LT_EXPR, boolean_type_node, arg, zero));
  tmp = build (MODIFY_EXPR, boolean_type_node, cond2, tmp);
  stmt = build_stmt (EXPR_STMT, tmp);
  g95_add_stmt_to_pre (se, stmt, stmt);

  tmp = build (MODIFY_EXPR, boolean_type_node, cond2, integer_zero_node);
  stmt = build_stmt (EXPR_STMT, tmp);
  tmp = fold (build (LT_EXPR, boolean_type_node, arg2, zero));
  stmt = build_stmt (IF_STMT, tmp, stmt, NULL_TREE);
  g95_add_stmt_to_pre (se, stmt, stmt);

  /* if (cond2)
       cond = 1;  */
  tmp = build (MODIFY_EXPR, boolean_type_node, cond, integer_one_node);
  stmt = build_stmt (EXPR_STMT, tmp);
  stmt = build_stmt (IF_STMT, cond2, stmt, NULL_TREE);
  g95_add_stmt_to_pre (se, stmt, stmt);

  val = create_tmp_var (type, "sign");
  /* Negate A.  */
  tmp = build1 (NEGATE_EXPR, type, arg);
  tmp = build (MODIFY_EXPR, type, val, tmp);
  nstmt = build_stmt (EXPR_STMT, tmp);

  /* Don't negate A.  */
  tmp = build (MODIFY_EXPR, type, val, arg);
  pstmt = build_stmt (EXPR_STMT, tmp);

  pstmt = build_stmt (IF_STMT, cond, nstmt, pstmt);
  g95_add_stmt_to_pre (se, pstmt, pstmt);

  se->expr = val;
}

/* Calculate the double precision product of two single precision values.  */
static void
g95_conv_intrinsic_dprod (g95_se * se, g95_expr * expr)
{
  tree arg;
  tree arg2;
  tree type;

  arg = g95_conv_intrinsic_function_args (se, expr);
  arg2 = TREE_VALUE (TREE_CHAIN (arg));
  arg = TREE_VALUE (arg);

  /* Convert the args to double precision before multiplying.  */
  type = g95_typenode_for_spec (&expr->ts);
  arg = g95_simple_convert (type, arg);
  arg = g95_simple_fold (arg, &se->pre, &se->pre_tail, NULL);
  arg2 = g95_simple_convert (type, arg2);
  arg2 = g95_simple_fold (arg2, &se->pre, &se->pre_tail, NULL);

  se->expr = build (MULT_EXPR, type, arg, arg2);
}

/* Get the minimum/maximum value of all the parameters.
    minmax (a1, a2, a3, ...)
    {
      if (a2 .op. a1)
        mvar = a2;
      else
        mvar = a1;
      if (a3 .op. mvar)
        mvar = a3;
      ...
      return mvar
    }
 */
static void
g95_conv_intrinsic_minmax (g95_se * se, g95_expr * expr, int op)
{
  tree limit;
  tree tmp;
  tree mvar;
  tree val;
  tree stmt;
  tree elsestmt;
  tree arg;
  tree type;

  arg = g95_conv_intrinsic_function_args (se, expr);
  type = g95_typenode_for_spec (&expr->ts);

  limit = TREE_VALUE (arg);
  mvar = create_tmp_var (type, "M");
  tmp = build (MODIFY_EXPR, type, mvar, limit);
  elsestmt = build_stmt (EXPR_STMT, tmp);
  for (arg = TREE_CHAIN (arg); arg != NULL_TREE; arg = TREE_CHAIN (arg))
    {
      val = TREE_VALUE (arg);
      tmp = build (MODIFY_EXPR, type, mvar, val);
      stmt = build_stmt (EXPR_STMT, tmp);

      tmp = build (op, type, val, limit);
      stmt = build_stmt (IF_STMT, tmp, stmt, elsestmt);
      g95_add_stmt_to_pre (se, stmt, stmt);
      elsestmt = NULL_TREE;
      limit = mvar;
    }
  se->expr = mvar;
}

static void
g95_conv_intrinsic_sum (g95_se * se, g95_expr * expr)
{
  tree sum;
  tree type;
  tree head;
  tree tail;
  tree tmp;
  tree stmt;
  g95_loopinfo loop;
  g95_actual_arglist *actual;
  g95_ss *arrayss;
  g95_ss *maskss;
  g95_se arrayse;
  g95_se maskse;
  g95_expr *arrayexpr;
  g95_expr *maskexpr;

  if (se->ss)
    g95_todo_error ("sum intrinsic with DIM parameter");

  type = g95_typenode_for_spec (&expr->ts);
  /* Initialize the result.  */
  sum = create_tmp_var (type, "sum");
  tmp = g95_build_const (type, integer_zero_node);
  tmp = build (MODIFY_EXPR, type, sum, tmp);
  stmt = build (EXPR_STMT, tmp);
  g95_add_stmt_to_pre (se, stmt, stmt);

  /* Walk the arguments.  */
  actual = expr->value.function.actual;
  arrayexpr = actual->expr;
  arrayss = g95_walk_expr (g95_ss_terminator, arrayexpr);
  assert (arrayss != g95_ss_terminator);
  arrayss = g95_reverse_ss (arrayss);

  actual = actual->next->next;
  assert (actual);
  maskexpr = actual->expr;
  if (maskexpr)
    {
      maskss = g95_walk_expr (g95_ss_terminator, maskexpr);
      assert (maskss != g95_ss_terminator);
      maskss = g95_reverse_ss (maskss);
    }
  else
    maskss = NULL;

  /* Initialize the scalarizer.  */
  g95_init_loopinfo (&loop);
  g95_add_ss_to_loop (&loop, arrayss);
  if (maskss)
    g95_add_ss_to_loop (&loop, maskss);

  /* Initialize the loop.  */
  g95_conv_ss_startstride (&loop);
  g95_conv_loop_setup (&loop);

  /* Generate the loop body.  */
  g95_start_scalarized_body (&loop);
  head = tail = NULL_TREE;

  /* If we have a mask, only add this element if the mask is set.  */
  if (maskss)
    {
      g95_init_se (&maskse, NULL);
      g95_copy_loopinfo_to_se (&maskse, &loop);
      maskse.ss = maskss;
      g95_conv_simple_cond (&maskse, maskexpr);

      g95_start_stmt();
    }

  g95_init_se (&arrayse, NULL);
  g95_copy_loopinfo_to_se (&arrayse, &loop);
  arrayse.ss = arrayss;
  g95_conv_simple_rhs (&arrayse, arrayexpr);
  g95_add_stmt_to_list (&head, &tail, arrayse.pre, arrayse.pre_tail);
  tmp = build (PLUS_EXPR, type, sum, arrayse.expr);
  tmp = build (MODIFY_EXPR, type, sum, tmp);
  stmt = build_stmt (EXPR_STMT, tmp);
  g95_add_stmt_to_list (&head, &tail, stmt, stmt);
  g95_add_stmt_to_list (&head, &tail, arrayse.post, arrayse.post_tail);

  if (maskss)
    {
      /* We enclose the above in if (mask) .  */
      stmt = g95_finish_stmt (head, tail);
      /* The actual body of the loop.  */
      head = tail = NULL_TREE;
      g95_add_stmt_to_list (&head, &tail, maskse.pre, maskse.pre_tail);
      assert (maskse.post = NULL_TREE);

      stmt = build_stmt (IF_STMT, maskse.expr, stmt, NULL_TREE);
      g95_add_stmt_to_list (&head, &tail, stmt, stmt);
    }

  g95_trans_scalarizing_loops (&loop, head, tail);

  g95_add_stmt_to_pre (se, loop.pre, loop.pre_tail);
  g95_add_stmt_to_pre (se, loop.post, loop.post_tail);
  se->expr = sum;
}

/* Generate code for an intrinsic function.  Some map directly to library
   calls, others get special handling.  In some cases the name of the function
   used depends on the type specifiers.  */
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

  assert (strncmp (expr->value.function.name, "__", 2) == 0);
  name = &expr->value.function.name[2];

  if (strncmp (name, "convert_", 7) == 0
      || strncmp (name, "real_", 5) == 0
      || strncmp (name, "int_", 4) == 0
      || strcmp (name, "ifix") == 0
      || strcmp (name, "idint") == 0
      || strcmp (name, "float") == 0
      || strcmp (name, "sngl") == 0)
    g95_conv_intrinsic_conversion (se, expr);
  else if (strncmp (name, "conjg_", 6) == 0)
    g95_conv_intrinsic_conjg (se, expr);
  else if (strncmp (name, "aimag_", 6) == 0)
    g95_conv_intrinsic_imagpart (se, expr);
  else if (strncmp (name, "abs_", 4) == 0)
    g95_conv_intrinsic_abs (se, expr);
  else if (strncmp (name, "cmplx", 5) == 0)
    g95_conv_intrinsic_cmplx (se, expr, name[5] == '1');
  else if (strncmp (name, "dim_", 4) == 0)
    g95_conv_intrinsic_dim (se, expr);
  else if (strncmp (name, "sign_", 5) == 0)
    g95_conv_intrinsic_sign (se, expr);
  else if (strcmp (name, "dprod") == 0)
    g95_conv_intrinsic_dprod (se, expr);
  else if (strncmp (name, "min_", 4) == 0)
    g95_conv_intrinsic_minmax(se, expr, LT_EXPR);
  else if (strncmp (name, "max_", 4) == 0)
    g95_conv_intrinsic_minmax(se, expr, GT_EXPR);
  else if (strncmp (name, "sum_", 4) == 0)
    g95_conv_intrinsic_sum (se, expr);
  else if (strncmp (name, "ubound", 6) == 0
           || strncmp (name, "lbound", 6) == 0)
    g95_conv_intrinsic_bound (se, expr, name[0] == 'u');
  else
    g95_conv_intrinsic_lib_function (se, expr, name);
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
  assert(isym);

  if (isym->elemental)
    return g95_walk_elemental_function_args (ss, expr, G95_SS_SCALAR);

  /* Special cases.  */
  if (strcmp (isym->name, "ubound") == 0
      || strcmp (isym->name, "lbound") == 0)
    return g95_walk_intrinsic_bound (ss, expr);

  /* These can probably be handles in the same way as normal functions.  */
  g95_todo_error ("Scalarization of non-elemental intrinsic: %s",
                  expr->value.function.name);
}

