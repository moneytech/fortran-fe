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

static void
g95_conv_intrinsic_conversion (g95_se * se, g95_expr * expr,
                              g95_intrinsic_sym * isym)
{
  tree type;

  type = g95_typenode_for_spec (&isym->ts);

  assert (expr->value.function.actual->expr);
  g95_conv_simple_val_type (se, expr->value.function.actual->expr, type);
}


void
g95_conv_intrinsic_function (g95_se * se, g95_expr * expr)
{
  g95_intrinsic_sym *isym;
  isym = expr->value.function.isym;

  if (strncmp (isym->name, "__convert_", 9) == 0)
    {
      g95_conv_intrinsic_conversion (se, expr, isym);
      return;
    }

  g95_todo_error ("Intrinsic function: %s", isym->name);
}

