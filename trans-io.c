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
#include <gmp.h>
#define BACKEND_CODE
#include "g95.h"
#include "trans.h"
#include "trans-stmt.h"

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
g95_trans_write (g95_code *code ATTRIBUTE_UNUSED)
{
  g95_todo_error ("IO statement not implemented: WRITE");
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

