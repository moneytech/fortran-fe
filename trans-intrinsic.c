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
#include <string.h>
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
#include "intrinsic.h"
#include "trans.h"
#include "trans-const.h"
#include "trans-types.h"
#include "trans-array.h"
/* Only for g95_trans_assign and g95_trans_pointer_assign.  */
#include "trans-stmt.h"

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
      if (actual->expr->ts.type == BT_CHARACTER)
        g95_conv_expr (&argse, actual->expr);
      else
        g95_conv_expr_val (&argse, actual->expr);

      if (actual->expr->ts.type == BT_CHARACTER)
        {
          g95_conv_string_parameter (&argse);
          args = g95_chainon_list (args, argse.string_length);
        }
      g95_add_block_to_block (&se->pre, &argse.pre);
      g95_add_block_to_block (&se->post, &argse.post);
      args = g95_chainon_list (args, argse.expr);
    }
  return args;
}

/* Conversions between different types are output by the frontend as
   intrinsic functions.  We implement these directly with inline code.  */
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

  /* Conversion from complex to non-complex involves taking the real
     component of the value.  */
  if (TREE_CODE (TREE_TYPE (arg)) == COMPLEX_TYPE
      && expr->ts.type != BT_COMPLEX)
    {
      tree artype;

      artype = TREE_TYPE (TREE_TYPE (arg));
      arg = build1 (REALPART_EXPR, artype, arg);
    }

  se->expr = convert (type, arg);
}

/* This is needed because the gcc backend only implements FIX_TRUNC_EXPR
   NINT(x) = INT(x + ((x > 0) ? 0.5 : -0.5)).  */
static void g95_conv_intrinsic_round(g95_se * se, tree arg, tree type)
{
  tree tmp;
  tree cond;
  tree neg;
  tree pos;
  tree argtype;
  REAL_VALUE_TYPE r;

  argtype = TREE_TYPE (arg);
  arg = g95_evaluate_now(arg, &se->pre);

  real_from_string(&r, "0.5");
  pos = build_real(argtype, r);

  real_from_string(&r, "-0.5");
  neg = build_real(argtype, r);

  tmp = g95_build_const (argtype, integer_zero_node);
  cond = fold (build (GT_EXPR, boolean_type_node, arg, tmp));

  tmp = fold (build (COND_EXPR, argtype, cond, pos, neg));
  tmp = fold (build (PLUS_EXPR, argtype, arg, tmp));
  se->expr = fold (build1 (FIX_TRUNC_EXPR, type, tmp));
}

/* Convert to an integer using the specified rounding mode.  */
static void
g95_conv_intrinsic_int (g95_se * se, g95_expr * expr, int op)
{
  tree type;
  tree arg;

  /* Evaluate the argument.  */
  type = g95_typenode_for_spec (&expr->ts);
  assert (expr->value.function.actual->expr);
  arg = g95_conv_intrinsic_function_args (se, expr);
  arg = TREE_VALUE (arg);

  if (TREE_CODE (TREE_TYPE (arg)) == INTEGER_TYPE)
    {
      /* Conversion to a different integer kind.  */
      se->expr = convert(type, arg);
    }
  else
    {
      /* Conversion from complex to non-complex involves taking the real
         component of the value.  */
      if (TREE_CODE (TREE_TYPE (arg)) == COMPLEX_TYPE
          && expr->ts.type != BT_COMPLEX)
        {
          tree artype;

          artype = TREE_TYPE (TREE_TYPE (arg));
          arg = build1 (REALPART_EXPR, artype, arg);
        }

      /* FIX_ROUND_EXPR isn't implemented in the gcc backend, so we
         must do it ourselves.  */
      switch (op)
        {
        case FIX_ROUND_EXPR:
          g95_conv_intrinsic_round (se, arg, type);
          break;

        default:
          se->expr = build1 (op, type, arg);
          break;
        }
    }
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

/* UBOUND and LBOUND.  This should probably just be a library call as
   constant cases will have been substituted by the forntend.  */
static void
g95_conv_intrinsic_bound (g95_se * se, g95_expr * expr, int upper)
{
  g95_actual_arglist *arg;
  g95_actual_arglist *arg2;
  g95_se argse;
  tree bound;
  tree desc;
  tree tmp;
  tree cond;
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
    }
  else
    {
      /* Convert the second argument.  */
      assert (arg2->expr);
      g95_init_se (&argse, NULL);
      g95_conv_expr_type (&argse, arg2->expr, g95_array_index_type);
      g95_add_block_to_block (&se->pre, &argse.pre);
      /* Convert from one based to zero based.  */
      bound = fold (build (MINUS_EXPR, g95_array_index_type, argse.expr,
                           integer_one_node));
    }

  /* Get the descriptor of the first argument.  */
  g95_init_se (&argse, NULL);
  argse.want_pointer = 1;
  g95_conv_expr (&argse, arg->expr);
  g95_add_block_to_block (&se->pre, &argse.pre);
  g95_add_block_to_block (&se->post, &argse.post);
  desc = argse.expr;
  /* TODO: bounds of arrays without descriptors.  */
  assert (G95_DESCRIPTOR_TYPE_P (TREE_TYPE (desc)));

  if (INTEGER_CST_P (bound))
    {
      /* We know the second argument, so supply the bound directly.  */
      assert (TREE_INT_CST_HIGH (bound) == 0);
      n = TREE_INT_CST_LOW (bound);
      assert (n >= 0 && n < G95_TYPE_ARRAY_RANK (TREE_TYPE (desc)));

      if (upper)
        se->expr = g95_conv_array_ubound (desc, n);
      else
        se->expr = g95_conv_array_lbound (desc, n);
    }
  else
    {
      bound = convert (g95_array_index_type, bound);
      bound = g95_evaluate_now (bound, &se->pre);

      if (flag_bounds_check)
        {
          cond = build (LT_EXPR, boolean_type_node, bound, integer_zero_node);
          g95_trans_runtime_check (cond, g95_strconst_fault, &se->pre);
          cond = build (GT_EXPR, boolean_type_node, bound,
              g95_rank_cst[G95_TYPE_ARRAY_RANK (TREE_TYPE (desc))]);
          g95_trans_runtime_check (cond, g95_strconst_fault, &se->pre);
        }

      if (upper)
        se->expr = g95_conv_descriptor_ubound (desc, bound);
      else
        se->expr = g95_conv_descriptor_lbound (desc, bound);
    }
  /* Convert the value to the required type.  */
  tmp = g95_typenode_for_spec (&expr->ts);
  se->expr = convert (tmp, se->expr);
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

  switch (expr->value.function.actual->expr->ts.type)
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
  real = convert (TREE_TYPE (type), TREE_VALUE (arg));
  arg = TREE_CHAIN (arg);
  if (both)
    imag = convert (TREE_TYPE (type), TREE_VALUE (arg));
  else
    imag = build_real_from_int_cst (TREE_TYPE (type), integer_zero_node);

  se->expr = fold (build (COMPLEX_EXPR, type, real, imag));
}

/* Remainder function MOD(A, P) = A - INT(A / P) * P.  */
/* TODO: MOD(x, 0)  */
static void
g95_conv_intrinsic_mod (g95_se * se, g95_expr * expr)
{
  tree arg;
  tree arg2;
  tree type;
  tree itype;
  tree tmp;

  arg = g95_conv_intrinsic_function_args (se, expr);
  arg2 = TREE_VALUE (TREE_CHAIN (arg));
  arg = TREE_VALUE (arg);
  type = TREE_TYPE (arg);

  switch (expr->ts.type)
    {
    case BT_INTEGER:
      /* Integer case is easy, we've got a builtin op.  */
      se->expr = build (TRUNC_MOD_EXPR, type, arg, arg2);
      break;

    case BT_REAL:
      /* Real values we have to do the hard way.  */
      arg = g95_evaluate_now(arg, &se->pre);
      arg2 = g95_evaluate_now(arg2, &se->pre);

      itype = g95_get_int_type (expr->ts.kind);
      tmp = fold(build (RDIV_EXPR, type, arg, arg2));
      tmp = fold(build1 (FIX_TRUNC_EXPR, itype, tmp));
      tmp = convert (type, tmp);
      tmp = fold(build (MULT_EXPR, type, tmp, arg2));
      se->expr = fold(build (MINUS_EXPR, type, arg, tmp));
      break;

    default:
      abort();
    }
}

/* Positive difference DIM (x, y) = ((x - y) < 0) ? 0 : x - y.  */
static void
g95_conv_intrinsic_dim (g95_se * se, g95_expr * expr)
{
  tree arg;
  tree arg2;
  tree val;
  tree tmp;
  tree type;
  tree zero;

  arg = g95_conv_intrinsic_function_args (se, expr);
  arg2 = TREE_VALUE (TREE_CHAIN (arg));
  arg = TREE_VALUE (arg);
  type = TREE_TYPE (arg);

  val = build (MINUS_EXPR, type, arg, arg2);
  val = g95_evaluate_now (val, &se->pre);

  zero = g95_build_const (type, integer_zero_node);
  tmp = build (LE_EXPR, boolean_type_node, val, zero);
  se->expr = build (COND_EXPR, type, tmp, zero, val);
}

/* SIGN(A, B) is absolute value of A times sign of B.
   The real value versions use library functions to ensure the correct
   handling of negative zero.  Integer case implemented as:
   SIGN(A, B) = ((a >= 0) .xor. (b >= 0)) ? a : -a
  */
static void
g95_conv_intrinsic_sign (g95_se * se, g95_expr * expr)
{
  tree tmp;
  tree arg;
  tree arg2;
  tree type;
  tree zero;
  tree testa;
  tree testb;


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

  testa = build (GE_EXPR, boolean_type_node, arg, zero);
  testb = build (GE_EXPR, boolean_type_node, arg2, zero);
  tmp = build (TRUTH_XOR_EXPR, boolean_type_node, testa, testb);
  se->expr = build (COND_EXPR, type, tmp,
                    build1 (NEGATE_EXPR, type, arg), arg);
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
  arg = convert (type, arg);
  arg2 = convert (type, arg2);
  se->expr = build (MULT_EXPR, type, arg, arg2);
}

/* Return a length one character string containing an ascii character.  */
static void
g95_conv_intrinsic_char (g95_se * se, g95_expr * expr)
{
  tree arg;
  tree var;
  tree type;

  arg = g95_conv_intrinsic_function_args (se, expr);
  arg = TREE_VALUE (arg);

  /* We currently don't support character types != 1.  */
  assert (expr->ts.kind == 1);
  type = g95_get_character_type (expr->ts.kind, expr->ts.cl);
  var = g95_create_var (type, "char");
  TREE_ADDRESSABLE (var) = 1;

  arg = convert (g95_character1_type_node, arg);
  var = build (ARRAY_REF, g95_character1_type_node, var, integer_one_node);
  g95_add_modify_expr (&se->pre, var, arg);
  se->expr = build1 (ADDR_EXPR, build_pointer_type (type), var);
  se->string_length = integer_one_node;
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
  tree thencase;
  tree elsecase;
  tree arg;
  tree type;

  arg = g95_conv_intrinsic_function_args (se, expr);
  type = g95_typenode_for_spec (&expr->ts);

  limit = TREE_VALUE (arg);
  mvar = g95_create_var (type, "M");
  elsecase = build_v (MODIFY_EXPR, mvar, limit);
  for (arg = TREE_CHAIN (arg); arg != NULL_TREE; arg = TREE_CHAIN (arg))
    {
      val = TREE_VALUE (arg);
      thencase = build_v (MODIFY_EXPR, mvar, val);

      tmp = build (op, boolean_type_node, val, limit);
      tmp = build_v (COND_EXPR, tmp, thencase, elsecase);
      g95_add_expr_to_block (&se->pre, tmp);
      elsecase = empty_stmt_node;
      limit = mvar;
    }
  se->expr = mvar;
}

static g95_symbol *
g95_get_symbol_for_expr (g95_expr * expr)
{
  g95_symbol *sym;

  assert (strlen (expr->value.function.name) <= G95_MAX_SYMBOL_LEN - 5);
  sym = g95_new_symbol (expr->value.function.name, NULL);

  sym->ts = expr->ts;
  sym->attr.external = 1;
  sym->attr.function = 1;
  sym->attr.proc = PROC_INTRINSIC;
  sym->attr.flavor = FL_PROCEDURE;
  if (expr->rank > 0)
    {
      sym->attr.dimension = 1;
      sym->as = g95_get_array_spec ();
      sym->as->type = AS_ASSUMED_SHAPE;
      sym->as->rank = expr->rank;
    }

  /* TODO: proper argument lists for external intrinsics.  */
  return sym;
}

/* Generate a call to an external intrinsic function.  */
static void
g95_conv_intrinsic_funcall (g95_se * se, g95_expr * expr)
{
  g95_symbol *sym;

  assert (! se->ss
          || se->ss->expr == expr);

  if (se->ss)
    assert (expr->rank > 0);
  else
    assert (expr->rank == 0);

  sym = g95_get_symbol_for_expr (expr);
  g95_conv_function_call (se, sym, expr->value.function.actual);
  g95_free (sym);
}

/* ANY and ALL intrinsics. ANY->op == NE_EXPR, ALL->op == EQ_EXPR.
   Implemented as
    any(a)
    {
      forall (i=...)
        if (a[i] != 0)
          return 1
      end forall
      return 0
    }
    all(a)
    {
      forall (i=...)
        if (a[i] == 0)
          return 0
      end forall
      return 1
    }
 */
static void
g95_conv_intrinsic_anyall (g95_se * se, g95_expr * expr, int op)
{
  tree resvar;
  stmtblock_t block;
  stmtblock_t body;
  tree type;
  tree tmp;
  tree found;
  g95_loopinfo loop;
  g95_actual_arglist *actual;
  g95_ss *arrayss;
  g95_se arrayse;
  tree exit_label;

  if (se->ss)
    {
      g95_conv_intrinsic_funcall (se, expr);
      return;
    }

  actual = expr->value.function.actual;
  type = g95_typenode_for_spec (&expr->ts);
  /* Initialize the result.  */
  resvar = g95_create_var (type, "test");
  if (op == EQ_EXPR)
    tmp = g95_build_const (type, integer_one_node);
  else
    tmp = g95_build_const (type, integer_zero_node);
  tmp = build (MODIFY_EXPR, type, resvar, tmp);
  g95_add_expr_to_block (&se->pre, tmp);

  /* Walk the arguments.  */
  arrayss = g95_walk_expr (g95_ss_terminator, actual->expr);
  assert (arrayss != g95_ss_terminator);
  arrayss = g95_reverse_ss (arrayss);

  /* Initialize the scalarizer.  */
  g95_init_loopinfo (&loop);
  exit_label = g95_build_label_decl (NULL_TREE);
  TREE_USED (exit_label) = 1;
  g95_add_ss_to_loop (&loop, arrayss);

  /* Initialize the loop.  */
  g95_conv_ss_startstride (&loop);
  g95_conv_loop_setup (&loop);

  g95_mark_ss_chain_used (arrayss, 1);
  /* Generate the loop body.  */
  g95_start_scalarized_body (&loop, &body);

  /* If the condition matches then set the return value.  */
  g95_start_block (&block);
  if (op == EQ_EXPR)
    tmp = g95_build_const (type, integer_zero_node);
  else
    tmp = g95_build_const (type, integer_one_node);
  tmp = build_v (MODIFY_EXPR, resvar, tmp);
  g95_add_expr_to_block (&block, tmp);

  /* And break out of the loop.  */
  tmp = build_v (GOTO_EXPR, exit_label);
  g95_add_expr_to_block (&block, tmp);

  found = g95_finish_block (&block);

  /* Check this element.  */
  g95_init_se (&arrayse, NULL);
  g95_copy_loopinfo_to_se (&arrayse, &loop);
  arrayse.ss = arrayss;
  g95_conv_expr_val (&arrayse, actual->expr);

  g95_add_block_to_block (&body, &arrayse.pre);
  tmp = build (op, boolean_type_node, arrayse.expr, integer_zero_node);
  tmp = build_v (COND_EXPR, tmp, found, empty_stmt_node);
  g95_add_expr_to_block (&body, tmp);
  g95_add_block_to_block (&body, &arrayse.post);

  g95_trans_scalarizing_loops (&loop, &body);

  /* Add the exit label.  */
  tmp = build_v (LABEL_EXPR, exit_label);
  g95_add_expr_to_block (&loop.pre, tmp);

  g95_add_block_to_block (&se->pre, &loop.pre);
  g95_add_block_to_block (&se->pre, &loop.post);
  se->expr = resvar;
}

/* COUNT(A) = Number of true elements in A.  */
static void
g95_conv_intrinsic_count (g95_se * se, g95_expr * expr)
{
  tree resvar;
  tree type;
  stmtblock_t body;
  tree tmp;
  g95_loopinfo loop;
  g95_actual_arglist *actual;
  g95_ss *arrayss;
  g95_se arrayse;

  if (se->ss)
    {
      g95_conv_intrinsic_funcall (se, expr);
      return;
    }

  actual = expr->value.function.actual;

  type = g95_typenode_for_spec (&expr->ts);
  /* Initialize the result.  */
  resvar = g95_create_var (type, "count");
  tmp = build_v (MODIFY_EXPR, resvar, integer_zero_node);
  g95_add_expr_to_block (&se->pre, tmp);

  /* Walk the arguments.  */
  arrayss = g95_walk_expr (g95_ss_terminator, actual->expr);
  assert (arrayss != g95_ss_terminator);
  arrayss = g95_reverse_ss (arrayss);

  /* Initialize the scalarizer.  */
  g95_init_loopinfo (&loop);
  g95_add_ss_to_loop (&loop, arrayss);

  /* Initialize the loop.  */
  g95_conv_ss_startstride (&loop);
  g95_conv_loop_setup (&loop);

  g95_mark_ss_chain_used (arrayss, 1);
  /* Generate the loop body.  */
  g95_start_scalarized_body (&loop, &body);

  tmp = build (PLUS_EXPR, TREE_TYPE (resvar), resvar, integer_one_node);
  tmp = build_v (MODIFY_EXPR, resvar, tmp);

  g95_init_se (&arrayse, NULL);
  g95_copy_loopinfo_to_se (&arrayse, &loop);
  arrayse.ss = arrayss;
  g95_conv_expr_val (&arrayse, actual->expr);
  tmp = build_v (COND_EXPR, arrayse.expr,tmp, empty_stmt_node);

  g95_add_block_to_block (&body, &arrayse.pre);
  g95_add_expr_to_block (&body, tmp);
  g95_add_block_to_block (&body, &arrayse.post);

  g95_trans_scalarizing_loops (&loop, &body);

  g95_add_block_to_block (&se->pre, &loop.pre);
  g95_add_block_to_block (&se->pre, &loop.post);
  se->expr = resvar;
}

/* Inline implementation of the sum and product intrinsics.  */
static void
g95_conv_intrinsic_arith (g95_se * se, g95_expr * expr, int op)
{
  tree resvar;
  tree type;
  stmtblock_t body;
  stmtblock_t block;
  tree tmp;
  g95_loopinfo loop;
  g95_actual_arglist *actual;
  g95_ss *arrayss;
  g95_ss *maskss;
  g95_se arrayse;
  g95_se maskse;
  g95_expr *arrayexpr;
  g95_expr *maskexpr;

  if (se->ss)
    {
      g95_conv_intrinsic_funcall (se, expr);
      return;
    }

  type = g95_typenode_for_spec (&expr->ts);
  /* Initialize the result.  */
  resvar = g95_create_var (type, "val");
  if (op == PLUS_EXPR)
    tmp = g95_build_const (type, integer_zero_node);
  else
    tmp = g95_build_const (type, integer_one_node);

  tmp = build (MODIFY_EXPR, type, resvar, tmp);
  g95_add_expr_to_block (&se->pre, tmp);

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
  g95_start_scalarized_body (&loop, &body);

  /* If we have a mask, only add this element if the mask is set.  */
  if (maskss)
    {
      g95_init_se (&maskse, NULL);
      g95_copy_loopinfo_to_se (&maskse, &loop);
      maskse.ss = maskss;
      g95_conv_expr_val (&maskse, maskexpr);
      g95_add_block_to_block (&body, &maskse.pre);

      g95_start_block (&block);
    }
  else
    g95_init_block (&block);

  /* Do the actual summation/product.  */
  g95_init_se (&arrayse, NULL);
  g95_copy_loopinfo_to_se (&arrayse, &loop);
  arrayse.ss = arrayss;
  g95_conv_expr_val (&arrayse, arrayexpr);
  g95_add_block_to_block (&block, &arrayse.pre);

  tmp = build (op, type, resvar, arrayse.expr);
  tmp = build_v (MODIFY_EXPR, resvar, tmp);
  g95_add_expr_to_block (&block, tmp);
  g95_add_block_to_block (&block, &arrayse.post);

  if (maskss)
    {
      /* We enclose the above in if (mask) {...} .  */
      tmp = g95_finish_block (&block);

      tmp = build_v (COND_EXPR, maskse.expr, tmp, empty_stmt_node);
    }
  else
    tmp = g95_finish_block (&block);
  g95_add_expr_to_block (&body, tmp);

  g95_trans_scalarizing_loops (&loop, &body);

  g95_add_block_to_block (&se->pre, &loop.pre);
  g95_add_block_to_block (&se->pre, &loop.post);

  se->expr = resvar;
}

static void
g95_conv_intrinsic_minmaxloc (g95_se * se, g95_expr * expr, int op)
{
  stmtblock_t body;
  stmtblock_t block;
  stmtblock_t ifblock;
  tree limit;
  tree type;
  tree tmp;
  tree ifbody;
  g95_loopinfo loop;
  g95_actual_arglist *actual;
  g95_ss *arrayss;
  g95_ss *maskss;
  g95_se arrayse;
  g95_se maskse;
  g95_expr *arrayexpr;
  g95_expr *maskexpr;
  tree pos;
  int n;

  if (se->ss)
    {
      g95_conv_intrinsic_funcall (se, expr);
      return;
    }

  type = g95_typenode_for_spec (&expr->ts);
  /* Initialize the result.  */
  limit = g95_create_var (type, "limit");
  pos = g95_create_var (g95_array_index_type, "pos");
  n = g95_validate_kind (expr->ts.type, expr->ts.kind);
  switch (expr->ts.type)
    {
    case BT_REAL:
      tmp = g95_conv_mpf_to_tree (g95_real_kinds[n].huge, expr->ts.kind);
      break;

    case BT_INTEGER:
      tmp = g95_conv_mpz_to_tree (g95_integer_kinds[n].huge, expr->ts.kind);
      break;

    default:
      abort ();
    }

  /* Most negative(+HUGE) for maxval, most negative (-HUGE) for minval.  */
  if (op == GT_EXPR)
    tmp = fold (build1(NEGATE_EXPR, TREE_TYPE (tmp), tmp));
  tmp = build (MODIFY_EXPR, type, limit, tmp);
  g95_add_expr_to_block (&se->pre, tmp);

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

  assert (loop.dimen == 1);

  g95_mark_ss_chain_used (arrayss, 1);
  if (maskss)
      g95_mark_ss_chain_used (maskss, 1);
  /* Generate the loop body.  */
  g95_start_scalarized_body (&loop, &body);

  /* If we have a mask, only check this element if the mask is set.  */
  if (maskss)
    {
      g95_init_se (&maskse, NULL);
      g95_copy_loopinfo_to_se (&maskse, &loop);
      maskse.ss = maskss;
      g95_conv_expr_val (&maskse, maskexpr);
      g95_add_block_to_block (&body, &maskse.pre);

      g95_start_block (&block);
    }
  else
    g95_init_block (&block);

  /* Compare with the current limit.  */
  g95_init_se (&arrayse, NULL);
  g95_copy_loopinfo_to_se (&arrayse, &loop);
  arrayse.ss = arrayss;
  g95_conv_expr_val (&arrayse, arrayexpr);
  g95_add_block_to_block (&block, &arrayse.pre);

  /* We do the following if this is a more extreme value.  */
  g95_start_block (&ifblock);

  /* Assign the value to the limit...  */
  tmp = build_v (MODIFY_EXPR, limit, arrayse.expr);
  g95_add_expr_to_block (&ifblock, tmp);

  /* Remember where we are.  */
  tmp = build_v (MODIFY_EXPR, pos, loop.loopvar[0]);
  g95_add_expr_to_block (&ifblock, tmp);

  ifbody = g95_finish_block (&ifblock);

  /* If it is a more extreme value.  */
  tmp = build (op, boolean_type_node, arrayse.expr, limit);
  tmp = build_v (COND_EXPR, tmp, ifbody, empty_stmt_node);
  g95_add_expr_to_block (&block, tmp);

  if (maskss)
    {
      /* We enclose the above in if (mask) {...}.  */
      tmp = g95_finish_block (&block);

      tmp = build_v (COND_EXPR, maskse.expr, tmp, empty_stmt_node);
    }
  else
    tmp = g95_finish_block (&block);
  g95_add_expr_to_block (&body, tmp);

  g95_trans_scalarizing_loops (&loop, &body);

  g95_add_block_to_block (&se->pre, &loop.pre);
  g95_add_block_to_block (&se->pre, &loop.post);

  /* Return a value in the range 1..SIZE(array).  */
  tmp = fold (build (MINUS_EXPR, g95_array_index_type, loop.from[0],
                     integer_one_node));
  tmp = fold (build (MINUS_EXPR, g95_array_index_type, pos, tmp));
  /* And convert to the required type.  */
  se->expr= convert (type, pos);
}

static void
g95_conv_intrinsic_minmaxval (g95_se * se, g95_expr * expr, int op)
{
  tree limit;
  tree type;
  tree tmp;
  tree ifbody;
  stmtblock_t body;
  stmtblock_t block;
  g95_loopinfo loop;
  g95_actual_arglist *actual;
  g95_ss *arrayss;
  g95_ss *maskss;
  g95_se arrayse;
  g95_se maskse;
  g95_expr *arrayexpr;
  g95_expr *maskexpr;
  int n;

  if (se->ss)
    {
      g95_conv_intrinsic_funcall (se, expr);
      return;
    }

  type = g95_typenode_for_spec (&expr->ts);
  /* Initialize the result.  */
  limit = g95_create_var (type, "limit");
  n = g95_validate_kind (expr->ts.type, expr->ts.kind);
  switch (expr->ts.type)
    {
    case BT_REAL:
      tmp = g95_conv_mpf_to_tree (g95_real_kinds[n].huge, expr->ts.kind);
      break;

    case BT_INTEGER:
      tmp = g95_conv_mpz_to_tree (g95_integer_kinds[n].huge, expr->ts.kind);
      break;

    default:
      abort ();
    }

  /* Most negative(-HUGE) for maxval, most positive (-HUGE) for minval.  */
  if (op == GT_EXPR)
    tmp = fold (build1(NEGATE_EXPR, TREE_TYPE (tmp), tmp));
  tmp = build_v (MODIFY_EXPR, limit, tmp);
  g95_add_expr_to_block (&se->pre, tmp);

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
  g95_start_scalarized_body (&loop, &body);

  /* If we have a mask, only add this element if the mask is set.  */
  if (maskss)
    {
      g95_init_se (&maskse, NULL);
      g95_copy_loopinfo_to_se (&maskse, &loop);
      maskse.ss = maskss;
      g95_conv_expr_val (&maskse, maskexpr);
      g95_add_block_to_block (&body, &maskse.pre);

      g95_start_block (&block);
    }
  else
    g95_init_block (&block);

  /* Compare with the current limit.  */
  g95_init_se (&arrayse, NULL);
  g95_copy_loopinfo_to_se (&arrayse, &loop);
  arrayse.ss = arrayss;
  g95_conv_expr_val (&arrayse, arrayexpr);
  g95_add_block_to_block (&block, &arrayse.pre);

  /* Assign the value to the limit...  */
  ifbody = build_v (MODIFY_EXPR, limit, arrayse.expr);

  /* If it is a more extreme value.  */
  tmp = build (op, boolean_type_node, arrayse.expr, limit);
  tmp = build_v (COND_EXPR, tmp, ifbody, empty_stmt_node);
  g95_add_expr_to_block (&block, tmp);
  g95_add_block_to_block (&block, &arrayse.post);

  tmp = g95_finish_block (&block);
  if (maskss)
    {
      /* We enclose the above in if (mask) {...}.  */
      tmp = build (COND_EXPR, maskse.expr, tmp, empty_stmt_node);
    }
  g95_add_expr_to_block (&body, tmp);

  g95_trans_scalarizing_loops (&loop, &body);

  g95_add_block_to_block (&se->pre, &loop.pre);
  g95_add_block_to_block (&se->pre, &loop.post);

  se->expr = limit;
}

/* BTEST (i, pos) = (i & (1 << pos)) != 0.  */
static void
g95_conv_intrinsic_btest (g95_se * se, g95_expr * expr)
{
  tree arg;
  tree arg2;
  tree type;
  tree tmp;

  arg = g95_conv_intrinsic_function_args (se, expr);
  arg2 = TREE_VALUE (TREE_CHAIN (arg));
  arg = TREE_VALUE (arg);
  type = TREE_TYPE (arg);

  tmp = build (LSHIFT_EXPR, type, integer_one_node, arg2);
  tmp = build (BIT_AND_EXPR, type, arg, tmp);
  tmp = fold (build (NE_EXPR, boolean_type_node, tmp, integer_zero_node));
  type = g95_typenode_for_spec (&expr->ts);
  se->expr = convert (type, tmp);
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
  int op;

  arg = g95_conv_intrinsic_function_args (se, expr);
  arg2 = TREE_VALUE (TREE_CHAIN (arg));
  arg = TREE_VALUE (arg);
  type = TREE_TYPE (arg);

  tmp = fold (build (LSHIFT_EXPR, type, integer_one_node, arg2));
  if (set)
    op = BIT_IOR_EXPR;
  else
    {
      op = BIT_AND_EXPR;
      tmp = fold (build1 (BIT_NOT_EXPR, type, tmp));
    }
  se->expr = fold (build (op, type, arg, tmp));
}

/* Extract a sequence of bits.
    IBITS(I, POS, LEN) = (I >> POS) & ~((~0) << LEN).  */
static void
g95_conv_intrinsic_ibits (g95_se * se, g95_expr * expr)
{
  tree arg;
  tree arg2;
  tree arg3;
  tree type;
  tree tmp;
  tree mask;

  arg = g95_conv_intrinsic_function_args (se, expr);
  arg2 = TREE_CHAIN (arg);
  arg3 = TREE_VALUE (TREE_CHAIN (arg2));
  arg = TREE_VALUE (arg);
  arg2 = TREE_VALUE (arg2);
  type = TREE_TYPE (arg);

  mask = build_int_2 (-1, ~(unsigned HOST_WIDE_INT) 0);
  mask = build (LSHIFT_EXPR, type, mask, arg3);
  mask = build1 (BIT_NOT_EXPR, type, mask);

  tmp = build (RSHIFT_EXPR, type, arg, arg2);

  se->expr = fold (build (BIT_AND_EXPR, type, tmp, mask));
}

/* ISHFT (I, SHIFT) = (shift >= 0) ? i << shift : i >> -shift.  */
static void
g95_conv_intrinsic_ishft (g95_se * se, g95_expr * expr)
{
  tree arg;
  tree arg2;
  tree type;
  tree tmp;
  tree lshift;
  tree rshift;

  arg = g95_conv_intrinsic_function_args (se, expr);
  arg2 = TREE_VALUE (TREE_CHAIN (arg));
  arg = TREE_VALUE (arg);
  type = TREE_TYPE (arg);

  /* Left shift if positive.  */
  lshift = build (LSHIFT_EXPR, type, arg, arg2);

  /* Right shift if negative.  This will perform an arithmetic shift as
     we are dealing with signed integers.  Section 13.5.7 allows this.  */
  tmp = build1 (NEGATE_EXPR, TREE_TYPE (arg2), arg2);
  rshift = build (RSHIFT_EXPR, type, arg, tmp);

  tmp = build (GT_EXPR, boolean_type_node, arg2, integer_zero_node);
  rshift = build (COND_EXPR, type, tmp, lshift, rshift);

  /* Do nothing if shift == 0.  */
  tmp = build (EQ_EXPR, boolean_type_node, arg2, integer_zero_node);
  se->expr = build (COND_EXPR, type, tmp, arg, rshift);
}

/* Circular shift.  AKA rotate or barrel shift.  */
static void
g95_conv_intrinsic_ishftc (g95_se * se, g95_expr * expr)
{
  tree arg;
  tree arg2;
  tree arg3;
  tree type;
  tree tmp;
  tree lrot;
  tree rrot;

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
      tmp = convert (type, TREE_VALUE (arg2));
      TREE_VALUE (arg2) = tmp;
      tmp = convert (type, TREE_VALUE (arg3));
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

  /* Rotate left if positive.  */
  lrot = build (LROTATE_EXPR, type, arg, arg2);

  /* Rotate right if negative.  */
  tmp = build1 (NEGATE_EXPR, TREE_TYPE (arg2), arg2);
  rrot = build (RROTATE_EXPR, type, arg, tmp);

  tmp = build (GT_EXPR, boolean_type_node, arg2, integer_zero_node);
  rrot = build (COND_EXPR, type, tmp, lrot, rrot);

  /* Do nothing if shift == 0.  */
  tmp = build (EQ_EXPR, boolean_type_node, arg2, integer_zero_node);
  se->expr = build (COND_EXPR, type, tmp, arg, rrot);
}

/* The length of a character string.  */
static void
g95_conv_intrinsic_len (g95_se * se, g95_expr * expr)
{
  tree len;
  tree type;
  tree decl;
  g95_se argse;
  g95_expr *arg;

  assert (!se->ss);

  arg = expr->value.function.actual->expr;

  type = g95_typenode_for_spec (&expr->ts);
  switch (arg->expr_type)
    {
    case EXPR_VARIABLE:
      decl = g95_get_symbol_decl (arg->symbol);
      assert (G95_DECL_STRING (decl));
      len = G95_DECL_STRING_LENGTH (decl);
      assert (len);
      break;

    case EXPR_CONSTANT:
      len = build_int_2 (arg->value.character.length, 0);
      break;

    default:
      /* Anybody stupid enough to do this deserves inefficient code.  */
      g95_init_se (&argse, se);
      g95_conv_expr (&argse, expr->value.function.actual->expr);
      g95_add_block_to_block (&se->pre, &argse.pre);
      g95_add_block_to_block (&se->post, &argse.post);
      len = argse.string_length;
      break;
    }
  se->expr = convert (type, len);
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
  se->expr = convert (type, se->expr);
}

/* The ascii value for a single character.  */
static void
g95_conv_intrinsic_ichar (g95_se * se, g95_expr * expr)
{
  tree arg;
  tree type;

  arg = g95_conv_intrinsic_function_args (se, expr);
  arg = TREE_VALUE (TREE_CHAIN(arg));
  assert (POINTER_TYPE_P (TREE_TYPE(arg)));
  arg = build1 (NOP_EXPR, pchar_type_node, arg);
  type = g95_typenode_for_spec (&expr->ts);

  se->expr = build1 (INDIRECT_REF, TREE_TYPE(TREE_TYPE (arg)), arg);
  se->expr = convert(type, se->expr);
}

static void
g95_conv_intrinsic_size (g95_se * se, g95_expr * expr)
{
  g95_actual_arglist *actual;
  tree args;
  tree type;
  tree fndecl;
  g95_se argse;
  g95_ss *ss;

  g95_init_se (&argse, NULL);
  actual =expr->value.function.actual;

  ss = g95_walk_expr (g95_ss_terminator, actual->expr);
  assert (ss != g95_ss_terminator);
  g95_conv_array_parameter (&argse, actual->expr, ss);
  g95_add_block_to_block (&se->pre, &argse.pre);
  g95_add_block_to_block (&se->post, &argse.post);
  args = g95_chainon_list (NULL_TREE, argse.expr);

  actual = actual->next;
  if (actual->expr)
    {
      g95_init_se (&argse, NULL);
      g95_conv_expr_type (&argse, actual->expr, g95_array_index_type);
      g95_add_block_to_block (&se->pre, &argse.pre);
      args = g95_chainon_list (args, argse.expr);
      fndecl = gfor_fndecl_size1;
    }
  else
    fndecl = gfor_fndecl_size0;

  se->expr = g95_build_function_call (fndecl, args);
  type = g95_typenode_for_spec (&expr->ts);
  se->expr = convert (type, se->expr);
}

/* Intrinsic string comarison functions.  */
static void
g95_conv_intrinsic_strcmp (g95_se * se, g95_expr * expr, int op)
{
  tree type;
  tree args;

  args = g95_conv_intrinsic_function_args (se, expr);
  /* Build a call for the comparison.  */
  se->expr = g95_build_function_call (gfor_fndecl_compare_string, args);

  type = g95_typenode_for_spec (&expr->ts);
  se->expr = build (op, type, se->expr, integer_zero_node);
}

/* Generate a call to the adjustl/adjustr library function.  */
static void
g95_conv_intrinsic_adjust (g95_se * se, g95_expr * expr, tree fndecl)
{
  tree args;
  tree len;
  tree type;
  tree var;
  tree tmp;

  args = g95_conv_intrinsic_function_args (se, expr);
  len = TREE_VALUE (args);

  type = TREE_TYPE (TREE_VALUE (TREE_CHAIN (args)));
  var = g95_conv_string_tmp (se, type, len);
  args = tree_cons (NULL_TREE, var, args);

  tmp = g95_build_function_call (fndecl, args);
  g95_add_expr_to_block (&se->pre, tmp);
  se->expr = var;
  se->string_length = len;
}

/* Generate code for an intrinsic function.  Some map directly to library
   calls, others get special handling.  In some cases the name of the function
   used depends on the type specifiers.  */
void
g95_conv_intrinsic_function (g95_se * se, g95_expr * expr)
{
  g95_intrinsic_sym *isym;
  char *name;
  int lib;

  isym = expr->value.function.isym;

  if (se->ss && se->ss->type == G95_SS_SCALAR
      && ! isym->elemental)
    {
      assert (se->ss->expr == expr);
      se->expr = se->ss->data.scalar.expr;
      se->string_length = se->ss->data.scalar.string_length;
      g95_advance_se_ss_chain (se);
      return;
    }

  name = &expr->value.function.name[2];

  if (expr->rank > 0)
    {
      lib = g95_is_intrinsic_libcall (expr);
      if (lib != 0)
        {
          if (lib == 1)
            se->ignore_optional = 1;
          g95_conv_intrinsic_funcall (se, expr);
          return;
        }
    }

  switch (expr->value.function.isym->generic_id)
    {
    case G95_ISYM_NONE:
      abort ();

    case G95_ISYM_ALLOCATED:
    case G95_ISYM_ANINIT:
    case G95_ISYM_ASSOCIATED:
    case G95_ISYM_CEILING:
    case G95_ISYM_CSHIFT:
    case G95_ISYM_EOSHIFT:
    case G95_ISYM_EXPONENT:
    case G95_ISYM_FLOOR:
    case G95_ISYM_FRACTION:
    case G95_ISYM_INDEX:
    case G95_ISYM_MERGE:
    case G95_ISYM_MODULO:
    case G95_ISYM_NEAREST:
    case G95_ISYM_PACK:
    case G95_ISYM_PRESENT:
    case G95_ISYM_REPEAT:
    case G95_ISYM_SCAN:
    case G95_ISYM_SET_EXPONENT:
    case G95_ISYM_SPREAD:
    case G95_ISYM_TRANSFER:
    case G95_ISYM_TRANSPOSE:
    case G95_ISYM_TRIM:
    case G95_ISYM_UNPACK:
    case G95_ISYM_VERIFY:
      g95_todo_error ("Intrinsic %s", expr->value.function.name);

    case G95_ISYM_ABS:
      g95_conv_intrinsic_abs (se, expr);
      break;

    case G95_ISYM_ADJUSTL:
      g95_conv_intrinsic_adjust (se, expr, gfor_fndecl_adjustl);
      break;

    case G95_ISYM_ADJUSTR:
      g95_conv_intrinsic_adjust (se, expr, gfor_fndecl_adjustr);
      break;

    case G95_ISYM_AIMAG:
      g95_conv_intrinsic_imagpart (se, expr);
      break;

    case G95_ISYM_ALL:
      g95_conv_intrinsic_anyall (se, expr, EQ_EXPR);
      break;

    case G95_ISYM_ANY:
      g95_conv_intrinsic_anyall (se, expr, NE_EXPR);
      break;

    case G95_ISYM_BTEST:
      g95_conv_intrinsic_btest (se, expr);
      break;

    case G95_ISYM_ACHAR:
    case G95_ISYM_CHAR:
      g95_conv_intrinsic_char (se, expr);
      break;

    case G95_ISYM_CONVERSION:
    case G95_ISYM_REAL:
    case G95_ISYM_LOGICAL:
    case G95_ISYM_DBLE:
      g95_conv_intrinsic_conversion (se, expr);
      break;

    /* Integer conversions are handled seperately to make sure we get the
       correct rounding mode.  */
    case G95_ISYM_AINT:
    case G95_ISYM_INT:
      g95_conv_intrinsic_int (se, expr, FIX_TRUNC_EXPR);
      break;

    case G95_ISYM_ANINT:
    case G95_ISYM_NINT:
      g95_conv_intrinsic_int (se, expr, FIX_ROUND_EXPR);
      break;

    case G95_ISYM_MOD:
      g95_conv_intrinsic_mod (se, expr);
      break;

    case G95_ISYM_CMPLX:
      g95_conv_intrinsic_cmplx (se, expr, name[5] == '1');
      break;

    case G95_ISYM_CONJG:
      g95_conv_intrinsic_conjg (se, expr);
      break;

    case G95_ISYM_COUNT:
      g95_conv_intrinsic_count (se, expr);
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

    case G95_ISYM_IACHAR:
    case G95_ISYM_ICHAR:
      /* We assume ASCII character sequence.  */
      g95_conv_intrinsic_ichar (se, expr);
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

    case G95_ISYM_LGE:
      g95_conv_intrinsic_strcmp (se, expr, GE_EXPR);
      break;

    case G95_ISYM_LGT:
      g95_conv_intrinsic_strcmp (se, expr, GT_EXPR);
      break;

    case G95_ISYM_LLE:
      g95_conv_intrinsic_strcmp (se, expr, LE_EXPR);
      break;

    case G95_ISYM_LLT:
      g95_conv_intrinsic_strcmp (se, expr, LT_EXPR);
      break;

    case G95_ISYM_MAX:
      g95_conv_intrinsic_minmax(se, expr, GT_EXPR);
      break;

    case G95_ISYM_MAXLOC:
      g95_conv_intrinsic_minmaxloc (se, expr, GT_EXPR);
      break;

    case G95_ISYM_MAXVAL:
      g95_conv_intrinsic_minmaxval (se, expr, GT_EXPR);
      break;

    case G95_ISYM_MIN:
      g95_conv_intrinsic_minmax(se, expr, LT_EXPR);
      break;

    case G95_ISYM_MINLOC:
      g95_conv_intrinsic_minmaxloc (se, expr, LT_EXPR);
      break;

    case G95_ISYM_MINVAL:
      g95_conv_intrinsic_minmaxval (se, expr, LT_EXPR);
      break;

    case G95_ISYM_NOT:
      g95_conv_intrinsic_not (se, expr);
      break;

    case G95_ISYM_PRODUCT:
      g95_conv_intrinsic_arith (se, expr, MULT_EXPR);
      break;

    case G95_ISYM_SIGN:
      g95_conv_intrinsic_sign (se, expr);
      break;

    case G95_ISYM_SIZE:
      g95_conv_intrinsic_size (se, expr);
      break;

    case G95_ISYM_SUM:
      g95_conv_intrinsic_arith (se, expr, PLUS_EXPR);
      break;

    case G95_ISYM_UBOUND:
      g95_conv_intrinsic_bound (se, expr, 1);
      break;

    case G95_ISYM_DOT_PRODUCT:
    case G95_ISYM_MATMUL:
      g95_conv_intrinsic_funcall (se, expr);
      break;

    default:
      g95_conv_intrinsic_lib_function (se, expr);
      break;
    }
}

/* This generates code to execute before entering the scalarization loop.
   Currently does nothing.  */
void
g95_add_intrinsic_ss_code (g95_loopinfo * loop ATTRIBUTE_UNUSED, g95_ss * ss)
{
  switch (ss->expr->value.function.isym->generic_id)
    {
    case G95_ISYM_UBOUND:
    case G95_ISYM_LBOUND:
      break;

    default:
      abort ();
      break;
    }

  return;
}

/* UBOUND and LBOUND intrinsics with one parameter are expanded into code
   inside the scalarization loop.  */
static g95_ss *
g95_walk_intrinsic_bound (g95_ss * ss, g95_expr * expr)
{
  g95_ss *newss;

  /* The two argument version returns a scalar.  */
  if (expr->value.function.actual->next->expr)
    return ss;

  newss = g95_get_ss ();
  newss->type = G95_SS_INTRINSIC;
  newss->expr = expr;
  newss->next = ss;

  return newss;
}

/* Walk an intrinsic array libcall.  */
static g95_ss *
g95_walk_intrinsic_libfunc (g95_ss * ss, g95_expr * expr)
{
  g95_ss *newss;

  assert (expr->rank > 0);

  newss = g95_get_ss ();
  newss->type = G95_SS_FUNCTION;
  newss->expr = expr;
  newss->next = ss;

  return newss;
}

/* Returns nonzero if the specified intrinsic function call maps directly to a
   an external library call.  Should only be used for functions that return
   arrays.  */
int
g95_is_intrinsic_libcall (g95_expr * expr)
{
  assert (expr->expr_type == EXPR_FUNCTION
          && expr->value.function.isym);
  assert (expr->rank > 0);

  switch (expr->value.function.isym->generic_id)
    {
    case G95_ISYM_ALL:
    case G95_ISYM_ANY:
    case G95_ISYM_COUNT:
    case G95_ISYM_MATMUL:
    case G95_ISYM_MAXLOC:
    case G95_ISYM_MAXVAL:
    case G95_ISYM_MINLOC:
    case G95_ISYM_MINVAL:
    case G95_ISYM_PRODUCT:
    case G95_ISYM_SUM:
    case G95_ISYM_SHAPE:
      /* Ignore absent optional parameters.  */
      return 1;
      
    case G95_ISYM_RESHAPE:
      /* Pass absent optional parameters.  */
      return 2;

    default:
      return 0;
    }
}

/* Walk an intrinsic function.  */
g95_ss *
g95_walk_intrinsic_function (g95_ss * ss, g95_expr * expr,
                             g95_intrinsic_sym * isym)
{
  assert(isym);

  if (isym->elemental)
    return g95_walk_elemental_function_args (ss, expr, G95_SS_SCALAR);

  if (expr->rank == 0)
    return ss;

  if (g95_is_intrinsic_libcall (expr))
    return g95_walk_intrinsic_libfunc (ss, expr);

  /* Special cases.  */
  switch (isym->generic_id)
    {
    case G95_ISYM_LBOUND:
    case G95_ISYM_UBOUND:
      return g95_walk_intrinsic_bound (ss, expr);

    default:
      g95_todo_error ("Scalarization of non-elemental intrinsic: %s",
                      expr->value.function.name);
    }
}

#include "gt-f95-trans-intrinsic.h"
