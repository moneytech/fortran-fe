/* Translation of constants
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

/* trans_code.c -- convert constant values */

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
#include <gmp.h>
#include <assert.h>
#include <math.h>
#define BACKEND_CODE
#include "g95.h"
#include "trans.h"
#include "trans-const.h"
#include "trans-types.h"

/* Strinng constants.  */
tree g95_strconst_bounds;
tree g95_strconst_fault;
tree g95_strconst_wrong_return;
tree g95_strconst_current_filename;

/* Build a constant with given type from an int_cst.  */
tree
g95_build_const (tree type, tree intval)
{
  tree val;
  tree zero;

  switch (TREE_CODE (type))
    {
    case INTEGER_TYPE:
      val = convert (type, intval);
      break;

    case REAL_TYPE:
      val = build_real_from_int_cst (type, intval);
      break;

    case COMPLEX_TYPE:
      val = build_real_from_int_cst (TREE_TYPE (type), intval);
      zero = build_real_from_int_cst (TREE_TYPE (type), integer_zero_node);
      val = build_complex (type, val, zero);
      break;

    default:
      abort();
    }
  return val;
}

static tree
g95_build_string_const(int length, char *s)
{
  tree str;
  tree len;

  str = build_string (length, s);
  len = build_int_2 (length, 0);
  TREE_TYPE (str) =
    build_array_type (g95_character1_type_node,
                     build_range_type (g95_strlen_type_node,
                                      integer_one_node, len));
  return str;
}

void
g95_init_string_constants ()
{
  g95_strconst_bounds =
    g95_build_string_const (21, "Array bound mismatch");

  g95_strconst_fault =
    g95_build_string_const (30, "Array reference out of bounds");

  g95_strconst_wrong_return =
    g95_build_string_const (32, "Incorrect function return value");

  g95_strconst_current_filename =
    g95_build_string_const (strlen (g95_option.source)+1, g95_option.source);
}

/*TODO: Maybe get values > 2^31 working.  */
tree
g95_conv_mpz_to_tree (mpz_t i, int kind)
{
  int val;
  tree res;

  if (!mpz_fits_slong_p (i))
    g95_todo_error ("integer constant does not fit in a signed int");
  val = mpz_get_si (i);
  res = build_int_2 (val, (val < 0) ? -1 : 0);
  TREE_TYPE (res) = g95_get_int_type (kind);
  return (res);
}

#define foo
/* Converts a real constant into backend form.  Uses an intermediate string
   representation.  */
static tree
convert_mpf_to_tree (mpf_t f, int kind)
{
  tree res;
  tree type;
  mp_exp_t exp;
  char buff[128];
  char *p;
  int n;
  int digits;
  int edigits;

  for (n = 0; g95_real_kinds[n].kind != 0; n++)
    {
      if (g95_real_kinds[n].kind == kind)
        break;
    }
  assert (g95_real_kinds[n].kind);

  digits = g95_real_kinds[n].precision + 1;
  assert (g95_real_kinds[n].radix == 2);

  n = MAX (abs(g95_real_kinds[n].min_exponent),
           abs(g95_real_kinds[n].min_exponent));
#if 0
  edigits = 2 + (int)(log (n) /
                      log (g95_real_kinds[n].radix));
#endif
  edigits = 1;
  while (n > 0)
    {
      n = n / 10;
      edigits += 3;
    }

  /* We also have two mins signs, 'e', '.' and a null terminator.  */
  if (digits + edigits + 5 > 128)
    p = (char *)g95_getmem (digits + edigits + 3);
  else
    p = buff;

  mpf_get_str (&buff[1], &exp, 10, digits , f);
  if (buff[1])
    {
      buff[0] = '.';
      strcat (buff, "e");
      sprintf (&buff[strlen (buff)], "%d", (int) exp);
    }
  else
    {
      strcpy (buff, "0");
    }

  type = g95_get_real_type (kind);
  res = build_real (type, REAL_VALUE_ATOF (buff, TYPE_MODE (type)));

  if (p != buff)
    g95_free (buff);

  return (res);
}

/* Translate a scalar constant.  Constants never have pre or post chains.  */
void
g95_conv_constant (g95_se * se, g95_expr * expr)
{
  tree real;
  tree imag;

  assert (expr->expr_type == EXPR_CONSTANT);

  if (se->ss != NULL)
    {
      assert (se->ss != g95_ss_terminator);
      assert (se->ss->type == G95_SS_SCALAR);
      assert (se->ss->expr == expr);

      se->expr = se->ss->data.scalar;
      g95_advance_se_ss_chain (se);
      return;
    }

  switch (expr->ts.type)
    {
    case BT_INTEGER:
      se->expr = g95_conv_mpz_to_tree (expr->value.integer, expr->ts.kind);
      break;

    case BT_REAL:
      se->expr = convert_mpf_to_tree (expr->value.real, expr->ts.kind);
      break;

    case BT_LOGICAL:
      se->expr = build_int_2 (expr->value.logical, 0);
      break;

    case BT_COMPLEX:
      real = convert_mpf_to_tree (expr->value.complex.r, expr->ts.kind);
      imag = convert_mpf_to_tree (expr->value.complex.i, expr->ts.kind);
      se->expr = build_complex (NULL_TREE, real, imag);
      break;

    case BT_CHARACTER:
      se->expr = g95_build_string_const (expr->value.character.length,
                                        expr->value.character.string);
      se->string_length = TYPE_MAX_VALUE (TYPE_DOMAIN (TREE_TYPE (se->expr)));
      break;

    default:
      fatal_error ("invalid constant: type %d", expr->ts.type);
      break;
    }
}

