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
#define BACKEND_CODE
#include "g95.h"
#include "trans.h"
#include "trans-const.h"
#include "trans-types.h"

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

/*TODO: Work out the required precision from the kind.  */
static tree
convert_mpf_to_tree (mpf_t f, int kind)
{
  tree res;
  tree type;
  mp_exp_t exp;
  char buff[53];

  /* sonvert via. a string. This should have enough space for 128 bit
   * mantissa+32bit exponent */
  mpf_get_str (&buff[1], &exp, 10, 39, f);
  if (exp == 0)
    strcpy (buff, "0");
  else
    {
      buff[0] = '.';
      strcat (buff, "e");
      sprintf (&buff[strlen (buff)], "%d", (int) exp);
    }
  type = g95_get_real_type (kind);
  res = build_real (type, REAL_VALUE_ATOF (buff, TYPE_MODE (type)));
  return (res);
}

/* Translate a scalar constant.  Constants never have pre or post chains.  */
void
g95_conv_constant (g95_se * se, g95_expr * expr)
{
  assert (expr->expr_type == EXPR_CONSTANT);

  if (se->ss != NULL)
    {
      assert (se->ss != g95_ss_terminator);
      assert (se->ss->dimen == 0);
      assert (se->ss->expr == expr);

      se->expr = se->ss->data.se.expr;
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

    default:
      g95_todo_error ("can't handle constant type %d", expr->ts.type);
      break;
    }
}

