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

/* Enums  */
enum range {LHS,RHS,MID};

/* Macros */
#define IS_ARRAY_EXPLICIT(as) ( ( as->type == AS_EXPLICIT ? 1: 0 ) )


int g95_check_fncall_dependency (g95_expr *, g95_expr *);
int g95_check_dependency (g95_expr *, g95_expr *, g95_expr **, int);
int g95_is_same_range (g95_array_ref *, g95_array_ref *, int, int);
int g95_dep_compare_expr (g95_expr *, g95_expr *);
int g95_expr_is_one (g95_expr *, int);
int g95_symbols_could_alias (g95_symbol *, g95_symbol *);

int g95_dep_resolver(g95_ref* lref, g95_ref *rref, int* dep);
int g95_check_range_range(g95_ref* lref, g95_ref* rref, int n);
int g95_check_element_vs_section( g95_ref *lref, g95_ref *rref ,int n );
int g95_check_element_vs_element( g95_ref *lref, g95_ref *rref ,int n );
int g95_is_inside_range( g95_expr *chk, g95_expr *start, g95_expr *end);

int get_no_of_elements(g95_expr *, g95_expr *, g95_expr *);
enum range get_range(mpf_t, int);
int get_deps( enum range x1,enum range x2,mpf_t X1,mpf_t X2,int N);
g95_ref * g95_get_array_from_component(g95_ref *ref);
int g95_check_another_arrayref(g95_ref * ref);
g95_ref * g95_get_array_from_component(g95_ref *ref);
