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
#include "ggc.h"
#include "toplev.h"
#include "real.h"
#include <gmp.h>
#include <assert.h>
#include <math.h>
#define BACKEND_CODE
#include "g95.h"
#include "trans.h"
#include "trans-const.h"
#include "trans-types.h"

/* String constants.  */
tree g95_strconst_bounds;
tree g95_strconst_fault;
tree g95_strconst_wrong_return;
tree g95_strconst_current_filename;

tree g95_rank_cst[G95_MAX_DIMENSIONS + 1];

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

tree
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

/* Return a string constant with the given length.  Used for static
   initializers.  The constant will be padded to the full length.  */
tree
g95_conv_string_init (tree length, g95_expr * expr)
{
  char *s;
  HOST_WIDE_INT len;
  int slen;
  tree str;

  assert (expr->expr_type == EXPR_CONSTANT);
  assert (expr->ts.type == BT_CHARACTER && expr->ts.kind == 1);
  assert (INTEGER_CST_P (length));
  assert (TREE_INT_CST_HIGH (length) == 0);

  len = TREE_INT_CST_LOW (length);
  slen = expr->value.character.length;
  assert (len >= slen);
  if (len != slen)
    {
      s = g95_getmem (len);
      memcpy (s, expr->value.character.string, slen);
      memset (&s[slen], ' ', len - slen);
      str = g95_build_string_const (len, s);
      g95_free (s);
    }
  else
    str = g95_build_string_const (len, expr->value.character.string);

  return str;
}

void
g95_init_constants ()
{
  int n;

  for (n = 0; n <= G95_MAX_DIMENSIONS; n++)
    {
      g95_rank_cst[n] = build_int_2 (n, 0);
      TREE_TYPE (g95_rank_cst[n]) = g95_array_index_type;
    }

  g95_strconst_bounds =
    g95_build_string_const (21, "Array bound mismatch");

  g95_strconst_fault =
    g95_build_string_const (30, "Array reference out of bounds");

  g95_strconst_wrong_return =
    g95_build_string_const (32, "Incorrect function return value");

  g95_strconst_current_filename =
    g95_build_string_const (strlen (g95_option.source)+1, g95_option.source);
}

#define BITS_PER_HOST_WIDE_INT (8 * sizeof (HOST_WIDE_INT))
/* Converts a GMP integer into a backend tree node.  */
tree
g95_conv_mpz_to_tree (mpz_t i, int kind)
{
  int val;
  tree res;
  HOST_WIDE_INT high;
  unsigned HOST_WIDE_INT low;
  int negate;
  char buff[10];
  char *p;
  char *q;
  int n;

  if (mpz_fits_slong_p (i))
    {
      val = mpz_get_si (i);
      res = build_int_2 (val, (val < 0) ? -1 : 0);
      TREE_TYPE (res) = g95_get_int_type (kind);
      return (res);
    }

  n = mpz_sizeinbase (i, 16);
  if (n > 8)
    q = g95_getmem (n + 2);
  else
    q = buff;

  low = 0;
  high = 0;
  p = mpz_get_str (q, 16, i);
  if (p[0] == '-')
    {
      negate = 1;
      p++;
    }
  else
    negate = 0;

  while (*p)
    {
      n = *(p++);
      if (n >= '0' && n <= '9')
        n = n - '0';
      else if (n >= 'a' && n <= 'z')
        n = n + 10 - 'a';
      else if (n >= 'A' && n <= 'Z')
        n = n + 10 - 'A';
      else
        abort ();

      assert (n >= 0 && n < 16);
      high = (high << 4) + (low >> (BITS_PER_HOST_WIDE_INT - 4));
      low = (low << 4) + n;
    }
  res = build_int_2 (high, low);
  TREE_TYPE (res) = g95_get_int_type (kind);
  if (negate)
    res = fold (build1 (NEGATE_EXPR, TREE_TYPE (res), res));

  if (q != buff)
    g95_free (q);

  return res;
}

/* Converts a real constant into backend form.  Uses an intermediate string
   representation.  */
tree
g95_conv_mpf_to_tree (mpf_t f, int kind)
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

  /* We also have two minus signs, "e", "." and a null terminator.  */
  if (digits + edigits + 5 > 128)
    p = (char *)g95_getmem (digits + edigits + 5);
  else
    p = buff;

  mpf_get_str (&p[1], &exp, 10, digits , f);
  if (p[1])
    {
      if (p[1] == '-')
        {
          p[0] = '-';
          p[1] = '.';
        }
      else
        {
          p[0] = '.';
        }
      strcat (p, "e");
      sprintf (&p[strlen (p)], "%d", (int) exp);
    }
  else
    {
      strcpy (p, "0");
    }

  type = g95_get_real_type (kind);
  res = build_real (type, REAL_VALUE_ATOF (p, TYPE_MODE (type)));
  if (p != buff)
    g95_free (p);

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

      se->expr = se->ss->data.scalar.expr;
      se->string_length = se->ss->data.scalar.string_length;
      g95_advance_se_ss_chain (se);
      return;
    }

  switch (expr->ts.type)
    {
    case BT_INTEGER:
      se->expr = g95_conv_mpz_to_tree (expr->value.integer, expr->ts.kind);
      break;

    case BT_REAL:
      se->expr = g95_conv_mpf_to_tree (expr->value.real, expr->ts.kind);
      break;

    case BT_LOGICAL:
      se->expr = build_int_2 (expr->value.logical, 0);
      break;

    case BT_COMPLEX:
      real = g95_conv_mpf_to_tree (expr->value.complex.r, expr->ts.kind);
      imag = g95_conv_mpf_to_tree (expr->value.complex.i, expr->ts.kind);
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

