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
#include "intrinsic.h"
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
typedef struct g95_intrinsic_map_t GTY(())
{
  const int id;
  const char *name;
  tree GTY(()) real4_decl;
  tree GTY(()) real8_decl;
  tree GTY(()) complex4_decl;
  tree GTY(()) complex8_decl;
} g95_intrinsic_map_t;

#define I_LIB(id, name) {id, name, NULL_TREE, NULL_TREE, NULL_TREE, NULL_TREE},
static GTY(()) g95_intrinsic_map_t g95_intrinsic_map[] =
{
  /* Math functions.  These are in libm.  */
I_LIB(G95_ISYM_SIN, "sin")
I_LIB(G95_ISYM_COS, "cos")
I_LIB(G95_ISYM_SQRT, "sqrt")
I_LIB(G95_ISYM_TAN, "tan")

I_LIB(G95_ISYM_ASIN, "asin")
I_LIB(G95_ISYM_ACOS, "acos")
I_LIB(G95_ISYM_ATAN, "atan")
I_LIB(G95_ISYM_ATAN2, "atan2")

I_LIB(G95_ISYM_SINH, "sinh")
I_LIB(G95_ISYM_COSH, "cosh")
I_LIB(G95_ISYM_TANH, "tanh")

I_LIB(G95_ISYM_EXP, "exp")
I_LIB(G95_ISYM_LOG, "log")
I_LIB(G95_ISYM_LOG10, "log10")

I_LIB(G95_ISYM_NONE, NULL)
};
#undef I_LIB

typedef struct
{
  const int id;
  const int code4;
  const int code8;
} g95_builtin_intrinsic_t;

static const g95_builtin_intrinsic_t g95_builtin_intrinsics[]=
{
  {G95_ISYM_SIN, BUILT_IN_SINF, BUILT_IN_SIN},
  {G95_ISYM_COS, BUILT_IN_COSF, BUILT_IN_COS},
  {G95_ISYM_SQRT, BUILT_IN_SQRTF, BUILT_IN_SQRT},
  {G95_ISYM_NONE, 0, 0}
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

      /* Evaluate the parameter.  This will substitute scalarized
         references automatically. */
      g95_init_se (&argse, se);
      g95_conv_simple_val (&argse, actual->expr);
      g95_add_stmt_to_pre (se, argse.pre, argse.pre_tail);
      g95_add_stmt_to_post (se, argse.post, argse.post_tail);

      if (actual->expr->ts.type == BT_CHARACTER)
        {
          g95_conv_string_parameter (&argse);
          args = g95_chainon_list (args, argse.string_length);
        }
      args = g95_chainon_list (args, argse.expr);
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

  /* Add GCC builtin functions.  */
  for (i = g95_builtin_intrinsics; i->id != G95_ISYM_NONE; i++)
    {
      for (m = g95_intrinsic_map; m->id != G95_ISYM_NONE; m++)
        {
          if (m->id == i->id)
            break;
        }
      assert (m->id != G95_ISYM_NONE);

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
g95_conv_intrinsic_lib_function (g95_se * se, g95_expr * expr)
{
  g95_intrinsic_map_t *m;
  tree args;
  tree fndecl;
  int id;

  id = expr->value.function.isym->generic_id;
  /* Find the entry for this function.  */
  for (m = g95_intrinsic_map; m->id != G95_ISYM_NONE; m++)
    {
      if (id == m->id)
        break;
    }

  if (m->id == G95_ISYM_NONE)
    {
      internal_error ("Intrinsic function %s(%d) not recognized",
                      expr->value.function.name, id);
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
        case 4: fndecl = gfor_fndecl_math_cabsf; break;
        case 8: fndecl = gfor_fndecl_math_cabs; break;
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
   negating A if A and B have differing signs.  The real value versions
   use library functions to ensure the correct handling of negative zero.
    sign (int a, int b)
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
  if (expr->ts.type == BT_REAL)
    {
      switch (expr->ts.kind)
        {
        case 4: tmp = gfor_fndecl_math_sign4; break;
        case 8: tmp = gfor_fndecl_math_sign8; break;
        default: abort ();
        }
      se->expr = g95_build_function_call (tmp, arg);
      return;
    }

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

  g95_mark_ss_chain_used (arrayss, 1);
  if (maskss)
      g95_mark_ss_chain_used (maskss, 1);
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

/* BTEST (i, pos) = (i & (1 << pos)) != 0.  */
static void
g95_conv_intrinsic_btest (g95_se * se, g95_expr * expr)
{
  tree arg;
  tree arg2;
  tree type;
  tree tmp;
  tree tmpvar;

  arg = g95_conv_intrinsic_function_args (se, expr);
  arg2 = TREE_VALUE (TREE_CHAIN (arg));
  arg = TREE_VALUE (arg);
  type = TREE_TYPE (arg);

  tmpvar = NULL_TREE;
  tmp = build (LSHIFT_EXPR, type, integer_one_node, arg2);
  tmp = g95_simple_fold (tmp, &se->pre, &se->pre_tail, &tmpvar);
  tmp = build (BIT_AND_EXPR, type, arg, tmp);
  tmp = g95_simple_fold (tmp, &se->pre, &se->pre_tail, &tmpvar);

  type = g95_typenode_for_spec (&expr->ts);
  se->expr = fold (build (NE_EXPR, type, tmp, integer_zero_node));
}

/* Generate code to perform the specified operation.  */
static void
g95_conv_intrinsic_bitop (g95_se * se, g95_expr * expr, int op)
{
  tree arg;
  tree arg2;
  tree type;

  arg = g95_conv_intrinsic_function_args (se, expr);
  arg2 = TREE_VALUE (TREE_CHAIN (arg));
  arg = TREE_VALUE (arg);
  type = TREE_TYPE (arg);

  se->expr = fold (build (op, type, arg, arg2));
}

/* Bitwise not.  */
static void
g95_conv_intrinsic_not (g95_se * se, g95_expr * expr)
{
  tree arg;

  arg = g95_conv_intrinsic_function_args (se, expr);
  arg = TREE_VALUE (arg);

  se->expr = build1 (BIT_NOT_EXPR, TREE_TYPE (arg), arg);
}

/* Set or clear a single bit.  */
static void
g95_conv_intrinsic_singlebitop (g95_se * se, g95_expr * expr, int set)
{
  tree arg;
  tree arg2;
  tree type;
  tree tmp;
  tree tmpvar;
  int op;

  arg = g95_conv_intrinsic_function_args (se, expr);
  arg2 = TREE_VALUE (TREE_CHAIN (arg));
  arg = TREE_VALUE (arg);
  type = TREE_TYPE (arg);

  tmpvar = NULL_TREE;
  tmp = build (LSHIFT_EXPR, type, integer_one_node, arg2);
  tmp = g95_simple_fold (tmp, &se->pre, &se->pre_tail, &tmpvar);
  if (set)
    op = BIT_IOR_EXPR;
  else
    {
      op = BIT_AND_EXPR;
      tmp = build1 (BIT_NOT_EXPR, type, tmp);
      tmp = g95_simple_fold (tmp, &se->pre, &se->pre_tail, &tmpvar);
    }
  se->expr = fold (build (op, type, arg, tmp));
}

/* Extract a sequence of bits.
    IBITS(I, POS, LEN) = (I >> POS) & ~(-1 << LEN).  */
static void
g95_conv_intrinsic_ibits (g95_se * se, g95_expr * expr)
{
  tree arg;
  tree arg2;
  tree arg3;
  tree type;
  tree tmp;
  tree tmpvar;
  tree mask;
  tree maskvar;

  arg = g95_conv_intrinsic_function_args (se, expr);
  arg2 = TREE_CHAIN (arg);
  arg3 = TREE_VALUE (TREE_CHAIN (arg2));
  arg = TREE_VALUE (arg);
  arg2 = TREE_VALUE (arg2);
  type = TREE_TYPE (arg);

  maskvar = NULL_TREE;
  mask = build_int_2 (-1, ~(unsigned HOST_WIDE_INT) 0);
  mask = build (LSHIFT_EXPR, type, mask, arg3);
  mask = g95_simple_fold (mask, &se->pre, &se->pre_tail, &maskvar);
  mask = build1 (BIT_NOT_EXPR, type, mask);
  mask = g95_simple_fold (mask, &se->pre, &se->pre_tail, &maskvar);

  tmpvar = NULL_TREE;
  tmp = build (RSHIFT_EXPR, type, arg, arg2);
  tmp = g95_simple_fold (tmp, &se->pre, &se->pre_tail, &tmpvar);

  se->expr = fold (build (BIT_AND_EXPR, type, tmp, mask));
}

/* ISHFT (I, SHIFT) = (shift >= 0) ? i << shift : i >> -shift.
   It would probably be best to implement this as a case statement.  */
static void
g95_conv_intrinsic_ishft (g95_se * se, g95_expr * expr)
{
  tree arg;
  tree arg2;
  tree type;
  tree tmp;
  tree val;
  tree lstmt;
  tree rstmt;
  tree head;
  tree tail;

  arg = g95_conv_intrinsic_function_args (se, expr);
  arg2 = TREE_VALUE (TREE_CHAIN (arg));
  arg = TREE_VALUE (arg);
  type = TREE_TYPE (arg);

  val = create_tmp_var (type, "ishft");

  /* Left shift if positive.  */
  tmp = build (LSHIFT_EXPR, type, arg, arg2);
  tmp = build (MODIFY_EXPR, type, val, tmp);
  lstmt = build_stmt (EXPR_STMT, tmp);

  /* Right shift if negative.  This will perform an arithmetic shift as
     we are dealing with signed integers.  Section 13.5.7 allows this.  */
  g95_start_stmt ();
  head = tail = NULL_TREE;

  tmp = build1 (NEGATE_EXPR, TREE_TYPE (arg2), arg2);
  tmp = g95_simple_fold (tmp, &head, &tail, NULL);

  tmp = build (RSHIFT_EXPR, type, arg, tmp);
  tmp = build (MODIFY_EXPR, type, val, tmp);
  rstmt = build_stmt (EXPR_STMT, tmp);
  g95_add_stmt_to_list (&head, &tail, rstmt, rstmt);

  rstmt = g95_finish_stmt (head, tail);

  tmp = build (GT_EXPR, boolean_type_node, arg2, integer_zero_node);
  rstmt = build_stmt (IF_STMT, tmp, lstmt, rstmt);

  /* Do nothing if shift == 0.  */
  tmp = build (MODIFY_EXPR, type, val, integer_zero_node);
  lstmt = build_stmt (EXPR_STMT, tmp);

  tmp = build (EQ_EXPR, boolean_type_node, arg2, integer_zero_node);
  tmp = build_stmt (IF_STMT, tmp, lstmt, rstmt);
  g95_add_stmt_to_pre (se, tmp, tmp);

  se->expr = val;
}

static void
g95_conv_intrinsic_ishftc (g95_se * se, g95_expr * expr)
{
  tree arg;
  tree arg2;
  tree arg3;
  tree type;
  tree tmp;
  tree val;
  tree lstmt;
  tree rstmt;
  tree head;
  tree tail;

  arg = g95_conv_intrinsic_function_args (se, expr);
  arg2 = TREE_CHAIN (arg);
  arg3 = TREE_CHAIN (arg2);
  if (arg3)
    {
      /* Use a library function for the 3 parameter version.  */
      type = TREE_TYPE (TREE_VALUE (arg));
      /* Convert all args to the same type otherwise we need loads of library
         functions.  SIZE and SHIFT cannot have values > BIT_SIZE (I) so the
         conversion is safe.  */
      tmp = g95_simple_convert (type, TREE_VALUE (arg2));
      tmp = g95_simple_fold (tmp, &se->pre, &se->pre_tail, NULL);
      TREE_VALUE (arg2) = tmp;
      tmp = g95_simple_convert (type, TREE_VALUE (arg3));
      tmp = g95_simple_fold (tmp, &se->pre, &se->pre_tail, NULL);
      TREE_VALUE (arg3) = tmp;

      switch (expr->ts.kind)
        {
        case 4: tmp = gfor_fndecl_math_ishftc4; break;
        case 8: tmp = gfor_fndecl_math_ishftc8; break;
        default: abort();
        }
      se->expr = g95_build_function_call (tmp, arg);
      return;
    }
  arg = TREE_VALUE (arg);
  arg2 = TREE_VALUE (arg2);
  type = TREE_TYPE (arg);
  val = create_tmp_var (type, "ishftc");

  /* Left shift if positive.  */
  tmp = build (LROTATE_EXPR, type, arg, arg2);
  tmp = build (MODIFY_EXPR, type, val, tmp);
  lstmt = build_stmt (EXPR_STMT, tmp);

  /* Right shift if negative.  */
  g95_start_stmt ();
  head = tail = NULL_TREE;

  tmp = build1 (NEGATE_EXPR, TREE_TYPE (arg2), arg2);
  tmp = g95_simple_fold (tmp, &head, &tail, NULL);

  tmp = build (RROTATE_EXPR, type, arg, tmp);
  tmp = build (MODIFY_EXPR, type, val, tmp);
  rstmt = build_stmt (EXPR_STMT, tmp);
  g95_add_stmt_to_list (&head, &tail, rstmt, rstmt);

  rstmt = g95_finish_stmt (head, tail);

  tmp = build (GT_EXPR, boolean_type_node, arg2, integer_zero_node);
  rstmt = build_stmt (IF_STMT, tmp, lstmt, rstmt);

  /* Do nothing if shift == 0.  */
  tmp = build (MODIFY_EXPR, type, val, integer_zero_node);
  lstmt = build_stmt (EXPR_STMT, tmp);

  tmp = build (EQ_EXPR, boolean_type_node, arg2, integer_zero_node);
  tmp = build_stmt (IF_STMT, tmp, lstmt, rstmt);
  g95_add_stmt_to_pre (se, tmp, tmp);

  se->expr = val;
}

/* The length of a character string.  */
static void
g95_conv_intrinsic_len (g95_se * se, g95_expr * expr)
{
  tree len;
  tree type;
  g95_se argse;

  assert (!se->ss);

  g95_init_se (&argse, se);
  g95_conv_simple_rhs (&argse, expr);
  g95_add_stmt_to_pre (se, argse.pre, argse.pre_tail);
  g95_add_stmt_to_post (se, argse.post, argse.post_tail);
  len = argse.string_length;

  type = g95_typenode_for_spec (&expr->ts);
  se->expr = g95_simple_convert (type, len);
}

/* The length of a character string not including trailing blanks.  */
static void
g95_conv_intrinsic_len_trim (g95_se * se, g95_expr * expr)
{
  tree args;
  tree type;

  args = g95_conv_intrinsic_function_args (se, expr);
  type = g95_typenode_for_spec (&expr->ts);
  se->expr = g95_build_function_call (gfor_fndecl_string_len_trim, args);
  if (TREE_TYPE (se->expr) != type)
    {
      se->expr = g95_simple_fold (se->expr, &se->pre, &se->pre_tail, NULL);
      se->expr = g95_simple_convert (type, se->expr);
    }
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
      se->expr = se->ss->data.scalar.expr;
      se->string_length = se->ss->data.scalar.string_length;
      g95_advance_se_ss_chain (se);
      return;
    }

  isym = expr->value.function.isym;

  assert (strncmp (expr->value.function.name, "__", 2) == 0);
  name = &expr->value.function.name[2];

  switch (expr->value.function.isym->generic_id)
    {
    case G95_ISYM_NONE:
      abort ();

    case G95_ISYM_ACHAR:
    case G95_ISYM_ADJUSTL:
    case G95_ISYM_ADJUSTR:
    case G95_ISYM_AINT:
    case G95_ISYM_ANINT:
    case G95_ISYM_ALL:
    case G95_ISYM_ALLOCATED:
    case G95_ISYM_ANINIT:
    case G95_ISYM_ANY:
    case G95_ISYM_ASSOCIATED:
    case G95_ISYM_CEILING:
    case G95_ISYM_CHAR:
    case G95_ISYM_COUNT:
    case G95_ISYM_CPU_TIME:
    case G95_ISYM_CSHIFT:
    case G95_ISYM_DATE_AND_TIME:
    case G95_ISYM_DOT_PRODUCT:
    case G95_ISYM_EOSHIFT:
    case G95_ISYM_EXPONENT:
    case G95_ISYM_FLOOR:
    case G95_ISYM_FRACTION:
    case G95_ISYM_IACHAR:
    case G95_ISYM_ICHAR:
    case G95_ISYM_INDEX:
    case G95_ISYM_LGE:
    case G95_ISYM_LGT:
    case G95_ISYM_LLE:
    case G95_ISYM_LLT:
    case G95_ISYM_MATMUL:
    case G95_ISYM_MAXLOC:
    case G95_ISYM_MAXVAL:
    case G95_ISYM_MERGE:
    case G95_ISYM_MINLOC:
    case G95_ISYM_MINVAL:
    case G95_ISYM_MOD:
    case G95_ISYM_MODULO:
    case G95_ISYM_MVBITS:
    case G95_ISYM_NEAREST:
    case G95_ISYM_NINT:
    case G95_ISYM_PACK:
    case G95_ISYM_PRESENT:
    case G95_ISYM_PRODUCT:
    case G95_ISYM_RANDOM_NUMBER:
    case G95_ISYM_RANDOM_SEED:
    case G95_ISYM_REPEAT:
    case G95_ISYM_RESHAPE:
    case G95_ISYM_SCAN:
    case G95_ISYM_SET_EXPONENT:
    case G95_ISYM_SHAPE:
    case G95_ISYM_SIZE:
    case G95_ISYM_SPREAD:
    case G95_ISYM_SYSTEM_CLOCK:
    case G95_ISYM_TRANSFER:
    case G95_ISYM_TRANSPOSE:
    case G95_ISYM_TRIM:
    case G95_ISYM_UNPACK:
    case G95_ISYM_VERIFY:
      g95_todo_error ("Intrinsic %s", expr->value.function.name);

    case G95_ISYM_ABS:
      g95_conv_intrinsic_abs (se, expr);
      break;

    case G95_ISYM_AIMAG:
      g95_conv_intrinsic_imagpart (se, expr);
      break;

    case G95_ISYM_BTEST:
      g95_conv_intrinsic_btest (se, expr);
      break;

    case G95_ISYM_CONVERSION:
    case G95_ISYM_REAL:
    case G95_ISYM_INT:
    case G95_ISYM_LOGICAL:
    case G95_ISYM_DBLE:
      g95_conv_intrinsic_conversion (se, expr);
      break;

    case G95_ISYM_CMPLX:
      g95_conv_intrinsic_cmplx (se, expr, name[5] == '1');
      break;

    case G95_ISYM_CONJG:
      g95_conv_intrinsic_conjg (se, expr);
      break;

    case G95_ISYM_DIM:
      g95_conv_intrinsic_dim (se, expr);
      break;

    case G95_ISYM_DPROD:
      g95_conv_intrinsic_dprod (se, expr);
      break;

    case G95_ISYM_IAND:
      g95_conv_intrinsic_bitop (se, expr, BIT_AND_EXPR);
      break;

    case G95_ISYM_IBCLR:
      g95_conv_intrinsic_singlebitop (se, expr, 0);
      break;

    case G95_ISYM_IBITS:
      g95_conv_intrinsic_ibits (se, expr);
      break;

    case G95_ISYM_IBSET:
      g95_conv_intrinsic_singlebitop (se, expr, 1);
      break;

    case G95_ISYM_IEOR:
      g95_conv_intrinsic_bitop (se, expr, BIT_XOR_EXPR);
      break;

    case G95_ISYM_IOR:
      g95_conv_intrinsic_bitop (se, expr, BIT_IOR_EXPR);
      break;

    case G95_ISYM_ISHFT:
      g95_conv_intrinsic_ishft (se, expr);
      break;

    case G95_ISYM_ISHFTC:
      g95_conv_intrinsic_ishftc (se, expr);
      break;

    case G95_ISYM_LBOUND:
      g95_conv_intrinsic_bound (se, expr, 0);
      break;

    case G95_ISYM_LEN:
      g95_conv_intrinsic_len (se, expr);
      break;

    case G95_ISYM_LEN_TRIM:
      g95_conv_intrinsic_len_trim (se, expr);
      break;

    case G95_ISYM_MAX:
      g95_conv_intrinsic_minmax(se, expr, GT_EXPR);
      break;

    case G95_ISYM_MIN:
      g95_conv_intrinsic_minmax(se, expr, LT_EXPR);
      break;

    case G95_ISYM_NOT:
      g95_conv_intrinsic_not (se, expr);
      break;

    case G95_ISYM_SIGN:
      g95_conv_intrinsic_sign (se, expr);
      break;

    case G95_ISYM_SUM:
      g95_conv_intrinsic_sum (se, expr);
      break;

    case G95_ISYM_UBOUND:
      g95_conv_intrinsic_bound (se, expr, 1);
      break;

    default:
      g95_conv_intrinsic_lib_function (se, expr);
      break;
    }
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
  switch (isym->generic_id)
    {
    case G95_ISYM_LBOUND:
    case G95_ISYM_UBOUND:
      return g95_walk_intrinsic_bound (ss, expr);

    case G95_ISYM_LEN:
      /* Returns a single scalar value.  Pass it back.  */
      return ss;

    default:
      /* Many of these can probably be handled in the same way as normal
         functions.  */
      g95_todo_error ("Scalarization of non-elemental intrinsic: %s",
                      expr->value.function.name);
    }
}

#include "gt-f95-trans-intrinsic.h"
