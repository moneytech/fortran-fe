/* Header for dependency analysis
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

int g95_check_fncall_dependency (g95_expr *, g95_expr *);
int g95_check_dependency (g95_expr *, g95_expr *, g95_expr **, int);
int g95_is_same_range (g95_array_ref *, g95_array_ref *, int, int);
int g95_dep_compare_expr (g95_expr *, g95_expr *);
int g95_expr_is_one (g95_expr *, int);
int g95_symbols_could_alias (g95_symbol *, g95_symbol *);

