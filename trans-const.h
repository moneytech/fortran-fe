/* Header for code constant translation functions
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

/* Returns an INT_CST.  */
tree g95_conv_mpz_to_tree (mpz_t i, int kind);

/* only really for use by g95_trans_simple_expr */
void g95_conv_constant (g95_se *, g95_expr *);

/* Initialise the string constants.  */
void g95_init_string_constants (void);

/* Strinng constants.  */
extern GTY(()) tree g95_strconst_current_filename;
extern GTY(()) tree g95_strconst_bounds;
extern GTY(()) tree g95_strconst_fault;
extern GTY(()) tree g95_strconst_wrong_return;
