/* Set up intrinsic functions
   Copyright (C) 2000 Free Software Foundation, Inc.
   Contributed by Andy Vaught & Katherine Holcomb

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


/* intrinsic.c-- Build up a list of intrinsic subroutines and
 * functions for the name-resolution stage. */

#include <stdio.h>
#include <stdarg.h>
#include <string.h>
#include <gmp.h>

#include "g95.h"
#include "intrinsic.h"


extern g95_integer_info g95_integer_kinds[];
extern g95_real_info g95_real_kinds[];

int g95_intrinsic_extension;

/* Pointers to a intrinsic function and it's argument names being checked.
 * The mvbits() subroutine requires the most arguments-- five. */

#define MAX_INTRINSIC_ARGS 5

char *g95_current_intrinsic, *g95_intrinsic_arg[MAX_INTRINSIC_ARGS];
locus *g95_current_intrinsic_where;

typedef struct intrinsic_arg {
  char name[G95_MAX_SYMBOL_LEN+1];

  g95_typespec ts;
  int optional;
  g95_actual_arglist *actual;

  struct intrinsic_arg *next;

} intrinsic_arg;


typedef struct intrinsic_sym {
  char name[G95_MAX_SYMBOL_LEN+1], lib_name[G95_MAX_SYMBOL_LEN+1];
  intrinsic_arg *arg;
  g95_typespec ts;
  int elemental, generic, specific, actual_ok;

  g95_expr *(*simplify)();
  try (*check)();
  void (*resolve)();
  struct intrinsic_sym *specific_head, *next;

} intrinsic_sym;


static intrinsic_sym *functions, *subroutines, *conversion, *next_sym;
static intrinsic_arg *next_arg;

static int nfunc, nsub, nargs, nconv;

enum { SZ_NOTHING=0, SZ_SUBS, SZ_FUNCS, SZ_CONVS } sizing;

/* g95_type_letter()-- Return a letter based on the passed type.  Used
 * to construct the name of a type-dependent subroutine. */

char g95_type_letter(bt type) {
char c;

  switch(type) {
  case BT_LOGICAL:    c = 'l';  break;
  case BT_CHARACTER:  c = 'c';  break;
  case BT_INTEGER:    c = 'i';  break;
  case BT_REAL:       c = 'r';  break;
  case BT_COMPLEX:    c = 'z';  break;

  default:            c = 'u';  break;
  }

  return c;
}


/* conv_name()-- Return a pointer to the name of a conversion function
 * given two typespecs. */

static char *conv_name(g95_typespec *from, g95_typespec *to) {
static char name[30];

  sprintf(name, "__convert_%c%d_%c%d", g95_type_letter(from->type), from->kind,
	  g95_type_letter(to->type), to->kind);

  return name;
}


/* find_conv()-- Given a pair of typespecs, find the intrinsic_sym
 * node that corresponds to the conversion.  Returns NULL if the
 * conversion isn't found. */

static intrinsic_sym *find_conv(g95_typespec *from, g95_typespec *to) {
intrinsic_sym *sym;
char *target;
int i;

  target = conv_name(from, to);
  sym = conversion;

  for(i=0; i<nconv; i++, sym++)
    if (strcmp(target, sym->name) == 0) return sym;

  return NULL;
}


/* convert_constant()-- Master function to convert one constant to
 * another.  While this is used as a simplification function, it
 * requires the destination type and kind information which is
 * supplied by a special case in do_simplify(). */

static g95_expr *convert_constant(g95_expr *e, bt type, int kind) {
g95_expr *result, *(*f)(g95_expr *, int);
g95_constructor *head, *c, *tail;

  switch(e->ts.type) {
  case BT_INTEGER:
    switch(type) {
    case BT_INTEGER:  f = g95_int2int;          break;
    case BT_REAL:     f = g95_int2real;         break;
    case BT_COMPLEX:  f = g95_int2complex;      break;
    default: goto oops;
    }
    break;

  case BT_REAL:
    switch(type) {
    case BT_INTEGER:  f = g95_real2int;         break;
    case BT_REAL:     f = g95_real2real;        break;
    case BT_COMPLEX:  f = g95_real2complex;     break;
    default: goto oops;
    }
    break;

  case BT_COMPLEX:
    switch(type) {
    case BT_INTEGER:  f = g95_complex2int;      break;
    case BT_REAL:     f = g95_complex2real;     break;
    case BT_COMPLEX:  f = g95_complex2complex;  break;

    default: goto oops;
    }
    break;

  default: oops:
    g95_internal_error("convert_constant(): Unexpected type");
  }

  result = NULL;

  /* Convert a constant */

  switch(e->expr_type) {
  case EXPR_CONSTANT:
    result = f(e, kind);
    if (result == NULL) return &g95_bad_expr;
    break;

  case EXPR_ARRAY:
    if (!g95_is_constant_expr(e)) break;

    head = NULL;

    for(c=e->value.constructor; c; c=c->next) {
      if (c->iterator != NULL)
	g95_internal_error("convert_constant(): Iterator present");

      if (head == NULL)
	head = tail = g95_get_constructor();
      else {
	tail->next = g95_get_constructor();
	tail = tail->next;
      }

      tail->where = c->where;
      tail->expr = f(c->expr, kind);
      if (tail->expr == NULL) {
	g95_free_constructor(head);
	return &g95_bad_expr;
      }

    }

    result = g95_get_expr();
    result->ts.type = type;
    result->ts.kind = kind;
    result->expr_type = EXPR_ARRAY;
    result->value.constructor = head;
    result->where = e->where;
    break;

  default:
    break;
  }

  return result;
}


/* do_check()-- Interface to the check functions.  We break apart an
 * argument list and call the proper check function rather than
 * forcing each function to manipulate the argument list */

static try do_check(intrinsic_sym *specific, g95_actual_arglist *arg) {
g95_expr *a1, *a2, *a3, *a4, *a5;
try t;

  a1 = arg->expr;
  arg = arg->next;

  if (arg == NULL) 
    t = (*specific->check)(a1);
  else {
    a2 = arg->expr;
    arg = arg->next;

    if (arg == NULL) 
      t = (*specific->check)(a1, a2);
    else {
      a3 = arg->expr;
      arg = arg->next;
      
      if (arg == NULL)
	t = (*specific->check)(a1, a2, a3);
      else {
	a4 = arg->expr;
	arg = arg->next;

	if (arg == NULL)
	  t = (*specific->check)(a1, a2, a3, a4);
	else {
	  a5 = arg->expr;
	  arg = arg->next;

	  if (arg == NULL)
	    t = (*specific->check)(a1, a2, a3, a4, a5);
	  else {
	    g95_internal_error("do_check(): too many args");
	  }
	}
      }
    }
  }

  return t;
}


/*********** Subroutines to build the intrinsic list ****************/

/* add_sym()-- Add a single intrinsic symbol to the current list. 
 * Argument list:
 *    char *     name of function
 *    int        whether function is elemental
 *    int        If the function can be used as an actual argument
 *    bt         return type of function
 *    int        kind of return type of function
 *    check      pointer to check function
 *    simplify   pointer to simplification function
 *    resolve    pointer to resolution function
 * Optional arguments come in multiples of four:
 *    char *    name of argument
 *    bt        type of argument
 *    int       kind of argument
 *    int       arg optional flag (1=optional, 0=required)
 *
 * the sequence is terminated by a NULL name. */

static void add_sym(const char *name, int elemental, int actual_ok, bt type,
		    int kind, try (*check)(), g95_expr *(*simplify)(),
		    void (*resolve)(), ...) {

int optional, first_flag;
va_list argp;

  switch(sizing) {
  case SZ_SUBS:
    nsub++;
    break;

  case SZ_FUNCS:
    nfunc++;
    break;

  case SZ_NOTHING:
    strcpy(next_sym->name, name);

    strcpy(next_sym->lib_name, "__");
    strcat(next_sym->lib_name, name);

    next_sym->elemental = elemental;
    next_sym->ts.type = type;
    next_sym->ts.kind = kind;
    next_sym->simplify = simplify;
    next_sym->check = check;
    next_sym->resolve = resolve;
    next_sym->specific = 0;
    next_sym->generic = 0;
    break;

  default:
    g95_internal_error("add_sym(): Bad sizing mode");
  }

  va_start(argp, resolve);

  first_flag = 1;

  for(;;) {
    name = va_arg(argp, char *);
    if (name == NULL) break;

    type = va_arg(argp, bt);
    kind = va_arg(argp, int);
    optional = va_arg(argp, int);

    if (sizing != SZ_NOTHING)
      nargs++;
    else {
      next_arg++;

      if (first_flag)
	next_sym->arg = next_arg;
      else
	(next_arg-1)->next = next_arg;

      first_flag = 0;

      strcpy(next_arg->name, name);
      next_arg->ts.type = type;
      next_arg->ts.kind = kind;
      next_arg->optional = optional;
    }
  }

  va_end(argp);

  next_sym++;
}



/* find_sym()-- Locate an intrinsic symbol given a base pointer,
 * number of elements in the table and a pointer to a name.  Returns
 * the NULL pointer if a name is not found. */

static intrinsic_sym *find_sym(intrinsic_sym *start, int n, const char *name) {

  while(n > 0) {
    if (strcmp(name, start->name) == 0) return start;

    start++;
    n--;
  }

  return NULL;
}


/* find_function()-- Given a name, find a function in the intrinsic
 * function table.  Returns NULL if not found. */

static intrinsic_sym *find_function(const char *name) {

  return find_sym(functions, nfunc, name);
}


/* find_subroutine()-- Given a name, find a function in the intrinsic
 * subroutine table.  Returns NULL if not found. */

static intrinsic_sym *find_subroutine(const char *name) {

  return find_sym(subroutines, nsub, name);
}


/* g95_generic_intrinsic()-- Given a string, figure out if it is
 * the name of a generic intrinsic function or not. */

int g95_generic_intrinsic(char *name) {
intrinsic_sym *sym;

  sym = find_function(name);
  return (sym == NULL) ? 0 : sym->generic;
}


/* g95_specific_intrinsic()-- Given a string, figure out if it is the
 * name of a specific intrinsic function or not. */

int g95_specific_intrinsic(char *name) {
intrinsic_sym *sym;

  sym = find_function(name);
  return (sym == NULL) ? 0 : sym->specific;
}


/* g95_intrinsic_name()-- Given a string, figure out if it is the name
 * of an intrinsic subroutine or function.  There are no generic
 * intrinsic subroutines, they are all specific. */

int g95_intrinsic_name(char *name, int subroutine_flag) {

  return subroutine_flag ?
    find_subroutine(name) != NULL :
    find_function(name) != NULL;
}


/* make_generic()-- Collect a set of intrinsic functions into a
 * generic collection.  The first argument is the name of the generic
 * function, which is also the name of a specific function.  The rest
 * of the specifics currently in the table are placed into the list of
 * specific functions associated with that generic.  */

static void make_generic(const char *name) {
intrinsic_sym *g;

  if (sizing != SZ_NOTHING) return; 

  g = find_function(name);
  if (g == NULL)
    g95_internal_error("make_generic(): Can't find generic symbol '%s'", name);

  g->generic = 1;
  g->specific = 1;
  if ((g+1)->name[0] != '\0') g->specific_head = g + 1;
  g++;
  
  while(g->name[0] != '\0') {
    g->next = g + 1;
    g->specific = 1;
    g++;
  }

  g--;
  g->next = NULL;
}


/* make_alias()-- Create a duplicate intrinsic function entry for the
 * current function, the only difference being the alternate name.
 * Note that we use argument lists more than once, but all argument
 * lists are freed as a single block.  */

static void make_alias(const char *name) {

  switch(sizing) {
  case SZ_FUNCS:
    nfunc++;
    break;

  case SZ_SUBS:
    nsub++;
    break;

  case SZ_NOTHING:
    next_sym[0] = next_sym[-1];
    strcpy(next_sym->name, name);
    next_sym++;
    break;

  default:
    break;
  }
}


/* add_functions()-- Add intrinsic functions */

static void add_functions(void) {

/* Argument names as in the standard (to be used as argument keywords) */

char   a[] = "a",  f[] = "field",      pt[] = "pointer",   tg[] = "target",
       b[] = "b",  m[] = "matrix",     ma[] = "matrix_a",  mb[] = "matrix_b",
       c[] = "c",  n[] = "ncopies",   pos[] = "pos",      bck[] = "back",
       i[] = "i",  v[] = "vector",     va[] = "vector_a",  vb[] = "vector_b",
       j[] = "j", a1[] = "a1",         fs[] = "fsource",   ts[] = "tsource",
       l[] = "l", a2[] = "a2",         mo[] = "mold",     ord[] = "order",
       p[] = "p", ar[] = "array",     shp[] = "shape",    src[] = "source",
       r[] = "r", bd[] = "boundary",  pad[] = "pad",      set[] = "set",
       s[] = "s", dm[] = "dim",      kind[] = "kind",     msk[] = "mask",
       x[] = "x", sh[] = "shift",     stg[] = "string",   ssg[] = "substring",
       y[] = "y", sz[] = "size",      sta[] = "string_a", stb[] = "string_b",
       z[] = "z", ln[] = "len";

int di, dr, dd, dl, dc, dz;

  di = g95_default_integer_kind();
  dr = g95_default_real_kind();
  dd = g95_default_double_kind();
  dl = g95_default_logical_kind();
  dc = g95_default_character_kind();
  dz = g95_default_complex_kind();

  add_sym("abs", 1, 1, BT_REAL, dr,
	  g95_check_abs, g95_simplify_abs, g95_resolve_abs,
	  a, BT_REAL, dr, 0, NULL);

  add_sym("iabs", 1, 1, BT_INTEGER, di,
	  NULL, g95_simplify_iabs, g95_resolve_abs,
	  a, BT_INTEGER, di, 0, NULL);

  add_sym("dabs", 1, 1, BT_REAL, dd,
	  NULL, g95_simplify_abs, g95_resolve_abs,
	  a, BT_REAL, dd, 0, NULL);

  add_sym("cabs", 1, 1, BT_REAL, dr,
	  NULL, g95_simplify_cabs, g95_resolve_abs,
	  a, BT_COMPLEX, dz, 0, NULL);

  add_sym("zabs", 1, 1, BT_REAL, dd,
	  NULL, g95_simplify_cabs, g95_resolve_abs,
	  a, BT_COMPLEX, dd, 0, NULL);   /* Extension */

  make_alias("cdabs");

  make_generic("abs");

  add_sym("achar", 1, 1, BT_CHARACTER, dc,
	  NULL, g95_simplify_achar, NULL,
	  i, BT_INTEGER, di, 0, NULL);

  add_sym("acos", 1, 1, BT_REAL, dr,
	  NULL, g95_simplify_acos, NULL,
	  x, BT_REAL, dr, 0, NULL);

  add_sym("dacos", 1, 1, BT_REAL, dd,
	  NULL, g95_simplify_acos, NULL,
	  x, BT_REAL, dd, 0, NULL);

  make_generic("acos");

  add_sym("adjustl", 1, 1, BT_CHARACTER, dc,
	  NULL, g95_simplify_adjustl, NULL,
	  stg, BT_CHARACTER, dc, 0, NULL);

  add_sym("adjustr", 1, 1, BT_CHARACTER, dc,
	  NULL, g95_simplify_adjustr, NULL,
	  stg, BT_CHARACTER, dc, 0, NULL);

  add_sym("aimag", 1, 1, BT_REAL, dr,
	  NULL, g95_simplify_aimag, NULL,
	  z, BT_COMPLEX, dz, 0, NULL);

  add_sym("dimag", 1, 1, BT_REAL, dd,
	  NULL, g95_simplify_aimag, NULL,
	  z, BT_COMPLEX, dd, 0, NULL);    /* Extension */

  make_generic("aimag");

  add_sym("aint", 1, 1, BT_REAL, dr,
	  g95_check_a_kind, g95_simplify_aint, g95_resolve_aint,
	  a, BT_REAL, dr, 0,   kind, BT_INTEGER, di, 1, NULL);

  add_sym("dint", 1, 1, BT_REAL, dd,
	  NULL, g95_simplify_dint, NULL,
	  a, BT_REAL, dd, 0, NULL);

  make_generic("aint");

  add_sym("all", 0, 1, BT_UNKNOWN, 0,
	  g95_check_all_any, NULL, g95_resolve_all,
	  msk, BT_LOGICAL, dl, 0,   dm, BT_INTEGER, di, 1, NULL);

  add_sym("allocated", 0, 1, BT_LOGICAL, dl,
	  g95_check_allocated, NULL, NULL,
	  ar, BT_UNKNOWN, 0, 0, NULL);

  add_sym("anint", 1, 1, BT_REAL, dr,
	  g95_check_a_kind, g95_simplify_anint, g95_resolve_anint,
	  a, BT_REAL, dr, 0,   kind, BT_INTEGER, di, 1, NULL);

  add_sym("dnint", 1, 1, BT_REAL, dd,
	  NULL, g95_simplify_dnint, NULL,
	  a, BT_REAL, dd, 0, NULL);

  make_generic("anint");

  add_sym("any", 0, 1, BT_UNKNOWN, 0,
	  g95_check_all_any, NULL, g95_resolve_any,
	  msk, BT_LOGICAL, dl, 0, dm, BT_INTEGER, di, 1, NULL);

  add_sym("asin", 1, 1, BT_REAL, dr,
	  NULL, g95_simplify_asin, NULL,
	  x, BT_REAL, dr, 0, NULL);

  add_sym("dasin", 1, 1, BT_REAL, dd,
	  NULL, g95_simplify_asin, NULL,
	  x, BT_REAL, dd, 0, NULL);

  make_generic("asin");

  add_sym("associated", 0, 1, BT_LOGICAL, dl,
	  g95_check_associated, NULL, NULL,
	  pt, BT_UNKNOWN, 0, 0,   tg, BT_INTEGER, di, 1, NULL);

  add_sym("atan", 1, 1, BT_REAL, dr,
	  NULL, g95_simplify_atan, NULL,
	  x, BT_REAL, dr, 0, NULL);

  add_sym("datan", 1, 1, BT_REAL, dd,
	  NULL, g95_simplify_atan, NULL,
	  x, BT_REAL, dd, 0, NULL);

  make_generic("atan");

  add_sym("atan2", 1, 1, BT_REAL, dr,
	  g95_check_atan2, g95_simplify_atan2, NULL,
	  y, BT_REAL, dr, 0,   x, BT_REAL, dr, 0, NULL);

  add_sym("datan2", 1, 1, BT_REAL, dd,
	  g95_check_atan2, g95_simplify_atan2, NULL,
	  y, BT_REAL, dd, 0,   x, BT_REAL, dd, 0, NULL);

  make_generic("atan2");

  add_sym("bit_size", 0, 1, BT_INTEGER, di,
	  g95_check_i, g95_simplify_bit_size, NULL,
	  i, BT_INTEGER, di, 0, NULL);

  add_sym("btest", 1, 1, BT_LOGICAL, dl,
	  g95_check_btest, g95_simplify_btest, g95_resolve_btest,
	  i, BT_INTEGER, di, 0,   pos, BT_INTEGER, di, 0, NULL);

  add_sym("ceiling", 1, 1, BT_INTEGER, di,
	  g95_check_a_kind, g95_simplify_ceiling, g95_resolve_ceiling,
	  a, BT_REAL, dr, 0,   kind, BT_INTEGER, di, 1, NULL);

  add_sym("char", 1, 0, BT_CHARACTER, dc,
	  g95_check_char, g95_simplify_char, NULL,
	  i, BT_INTEGER, di, 0,   kind, BT_INTEGER, di, 1, NULL);

  make_generic("char");

  add_sym("cmplx", 1, 1, BT_COMPLEX, dz,
	  g95_check_cmplx, g95_simplify_cmplx, g95_resolve_cmplx,
	  x, BT_UNKNOWN, dr, 0,   y, BT_UNKNOWN, dr, 1,
	  kind, BT_INTEGER, di, 1, NULL);

  make_generic("cmplx");

  /* Making dcmplx a specific of cmplx causes cmplx to return a double
   * complex instead of the default complex.  */

  add_sym("dcmplx", 1, 1, BT_COMPLEX, dd,
	  g95_check_dcmplx, g95_simplify_dcmplx, NULL,
	  x, BT_REAL, dd, 0,   y, BT_REAL, dd, 1, NULL);  /* Extension */

  add_sym("conjg", 1, 1, BT_COMPLEX, dz,
	  NULL, g95_simplify_conjg, NULL,
	  z, BT_COMPLEX, dz, 0, NULL);

  add_sym("dconjg", 1, 1, BT_COMPLEX, dd,
	  NULL, g95_simplify_conjg, NULL,
	  z, BT_COMPLEX, dd, 0, NULL);   /* Extension */

  make_generic("conjg");

  add_sym("cos", 1, 1, BT_REAL, dr,
	  NULL, g95_simplify_cos, NULL,
	  x, BT_REAL, dr, 0, NULL);

  add_sym("dcos", 1, 1, BT_REAL, dd,
	  NULL, g95_simplify_cos, NULL,
	  x, BT_REAL, dd, 0, NULL);

  add_sym("ccos", 1, 1, BT_COMPLEX, dz,
	  NULL, g95_simplify_cos, NULL,
	  x, BT_COMPLEX, dz, 0, NULL);

  add_sym("zcos", 1, 1, BT_COMPLEX, dd,
	  NULL, g95_simplify_cos, NULL,
	  x, BT_COMPLEX, dd, 0, NULL);   /* Extension */

  make_alias("cdcos");

  make_generic("cos");

  add_sym("cosh", 1, 1, BT_REAL, dr,
	  NULL, g95_simplify_cosh, NULL,
	  x, BT_REAL, dr, 0, NULL);

  add_sym("dcosh", 1, 1, BT_REAL, dd,
	  NULL, g95_simplify_cosh, NULL,
	  x, BT_REAL, dd, 0, NULL);

  make_generic("cosh");

  add_sym("count", 0, 1, BT_INTEGER, di,
	  g95_check_count, NULL, NULL,
	  msk, BT_LOGICAL, dl, 0,   dm, BT_INTEGER, di, 1, NULL);

  add_sym("cshift", 0, 1, BT_REAL, dr,
	  g95_check_cshift, NULL, NULL,
	  ar, BT_REAL, dr, 0,   sh, BT_INTEGER, di, 0,
	  dm, BT_INTEGER, di, 1, NULL);

  add_sym("dble", 1, 1, BT_REAL, dd,
	  g95_check_dble, g95_simplify_dble, NULL,
	  a, BT_REAL, dr, 0, NULL);

  add_sym("digits", 0, 1, BT_INTEGER, di,
	  g95_check_digits, g95_simplify_digits, NULL,
	  x, BT_UNKNOWN, dr, 0, NULL);

  add_sym("dim", 1, 1, BT_REAL, dr,
	  g95_check_a_p, g95_simplify_dim, g95_resolve_dim,
	  x, BT_UNKNOWN, dr, 0,   y, BT_UNKNOWN, dr, 0, NULL);

  add_sym("idim", 1, 1, BT_INTEGER, di,
	  NULL, g95_simplify_dim, g95_resolve_dim,
	  x, BT_INTEGER, di, 0,   y, BT_INTEGER, di, 0, NULL);

  add_sym("ddim", 1, 1, BT_REAL, dd,
	  NULL, g95_simplify_dim, g95_resolve_dim,
	  x, BT_REAL, dd, 0,   y, BT_REAL, dd, 0, NULL);

  make_generic("dim");

  add_sym("dot_product", 0, 1, BT_UNKNOWN, 0,
	  g95_check_dot_product, NULL, g95_resolve_dot_product,
	  va, BT_REAL, dr, 0,   vb, BT_REAL, dr, 0, NULL);

  add_sym("dprod", 1, 1, BT_REAL, dd,
	  NULL, g95_simplify_dprod, NULL,
	  x, BT_REAL, dr, 0,   y, BT_REAL, dr, 0, NULL);

  make_generic("dprod");

  add_sym("dreal", 1, 0, BT_REAL, dd,
	  NULL, NULL, NULL,
	  a, BT_COMPLEX, dd, 0,  NULL);    /* Extension */

  add_sym("eoshift", 0, 1, BT_REAL, dr,
	  g95_check_eoshift, NULL, NULL,
	  ar, BT_REAL, dr, 0,   sh, BT_INTEGER, di, 0,
	  bd, BT_REAL, dr, 1,   dm, BT_INTEGER, di, 1, NULL);

  add_sym("epsilon", 0, 1, BT_REAL, dr,
	  g95_check_x, g95_simplify_epsilon, NULL,
	  x, BT_REAL, dr, 0, NULL);

  add_sym("exp", 1, 1, BT_REAL, dr,
	  NULL, g95_simplify_exp, NULL,
	  x, BT_REAL, dr, 0, NULL);

  add_sym("dexp", 1, 1, BT_REAL, dd,
	  NULL, g95_simplify_exp, NULL,
	  x, BT_REAL, dd, 0, NULL);

  add_sym("cexp", 1, 1, BT_COMPLEX, dz,
	  NULL, g95_simplify_exp, NULL,
	  x, BT_COMPLEX, dz, 0, NULL);

  add_sym("zexp", 1, 1, BT_COMPLEX, dd,
	  NULL, g95_simplify_exp, NULL,
	  x, BT_COMPLEX, dd, 0, NULL);   /* Extension */

  make_alias("cdexp");

  make_generic("exp");

  add_sym("exponent", 1, 1, BT_INTEGER, di,
	  g95_check_x, g95_simplify_exponent, g95_resolve_exponent,
	  x, BT_REAL, dr, 0, NULL);

  add_sym("floor", 1, 1, BT_INTEGER, di,
	  g95_check_a_kind, g95_simplify_floor, g95_resolve_floor,
	  a, BT_REAL, dr, 0,   kind, BT_INTEGER, di, 1, NULL);

  add_sym("fraction", 1, 1, BT_REAL, dr,
	  g95_check_x, g95_simplify_fraction, g95_resolve_fraction,
	  x, BT_REAL, dr, 0, NULL);

  add_sym("huge", 0, 1, BT_REAL, dr,
	  g95_check_huge, g95_simplify_huge, NULL,
	  x, BT_UNKNOWN, dr, 0,  NULL);

  add_sym("iachar", 1, 1, BT_INTEGER, di,
	  NULL, g95_simplify_iachar, NULL,
	  c, BT_CHARACTER, dc, 0, NULL);

  add_sym("iand", 1, 1, BT_INTEGER, di,
	  g95_check_iand, g95_simplify_iand, NULL,
	  i, BT_INTEGER, di, 0,   j, BT_INTEGER, di, 0, NULL);

  add_sym("ibclr", 1, 1, BT_INTEGER, di,
	  g95_check_ibclr, g95_simplify_ibclr, NULL,
	  i, BT_INTEGER, di, 0,   pos, BT_INTEGER, di, 0, NULL);

  add_sym("ibits", 1, 1, BT_INTEGER, di,
	  g95_check_ibits, g95_simplify_ibits, NULL,
	  i, BT_INTEGER, di, 0,   pos, BT_INTEGER, di, 0,
	  ln, BT_INTEGER, di, 0, NULL);

  add_sym("ibset", 1, 1, BT_INTEGER, di,
	  g95_check_ibset, g95_simplify_ibset, NULL,
	  i, BT_INTEGER, di, 0, pos,   BT_INTEGER, di, 0, NULL);

  add_sym("ichar", 1, 0, BT_INTEGER, di,
	  NULL, g95_simplify_ichar, NULL,
	  c, BT_CHARACTER, dc, 0, NULL);

  make_generic("ichar");

  add_sym("ieor", 1, 1, BT_INTEGER, di,
	  g95_check_ieor, g95_simplify_ieor, NULL,
	  i, BT_INTEGER, di, 0,   j, BT_INTEGER, di, 0, NULL);

  add_sym("index", 1, 1, BT_INTEGER, di,
	  g95_check_index, g95_simplify_index, NULL,
	  stg, BT_CHARACTER, dc, 0,   ssg, BT_CHARACTER, dc, 0,
	  bck, BT_LOGICAL, dl, 1, NULL);

  make_generic("index");

  add_sym("int", 1, 1, BT_INTEGER, di,
	  g95_check_int, g95_simplify_int, NULL,
	  a, BT_REAL, dr, 0,   kind, BT_INTEGER, di, 1, NULL);

  add_sym("ifix", 1, 0, BT_INTEGER, di,
	  NULL, g95_simplify_ifix, NULL,
	  a, BT_REAL, dr, 0, NULL);

  add_sym("idint", 1, 0, BT_INTEGER, di,
	  NULL, g95_simplify_idint, NULL,
	  a, BT_REAL, dd, 0, NULL);

  make_generic("int");

  add_sym("ior", 1, 1, BT_INTEGER, di,
	  g95_check_ior, g95_simplify_ior, NULL,
	  i, BT_INTEGER, di, 0, j,   BT_INTEGER, di, 0, NULL);

  add_sym("ishft", 1, 1, BT_INTEGER, di,
	  g95_check_ishft, g95_simplify_ishft, g95_resolve_ishft,
	  i, BT_INTEGER, di, 0,   sh, BT_INTEGER, di, 0, NULL);

  add_sym("ishftc", 1, 1, BT_INTEGER, di,
	  g95_check_ishftc, g95_simplify_ishftc, g95_resolve_ishftc,
	  i, BT_INTEGER, di, 0,   sh, BT_INTEGER, di, 0,
	  sz, BT_INTEGER, di, 1, NULL);

  add_sym("kind", 0, 1, BT_INTEGER, di,
	  g95_check_kind, g95_simplify_kind, NULL,
	  x, BT_REAL, dr, 0, NULL);

  add_sym("lbound", 0, 1, BT_INTEGER, di,
	  g95_check_lbound, NULL, NULL,
	  ar, BT_REAL, dr, 0,   dm, BT_INTEGER, di, 1, NULL);

  add_sym("len", 0, 1, BT_INTEGER, di,
	  NULL, g95_simplify_len, NULL,
	  stg, BT_CHARACTER, dc, 0, NULL);

  make_generic("len");

  add_sym("len_trim", 1, 1, BT_INTEGER, di,
	  NULL, g95_simplify_len_trim, NULL,
	  stg, BT_CHARACTER, dc, 0, NULL);

  add_sym("lge", 1, 0, BT_LOGICAL, dl,
	  NULL, g95_simplify_lge, NULL,
	  sta, BT_CHARACTER, dc, 0,   stb, BT_CHARACTER, dc, 0, NULL);

  make_generic("lge");

  add_sym("lgt", 1, 0, BT_LOGICAL, dl,
	  NULL, g95_simplify_lgt, NULL,
	  sta, BT_CHARACTER, dc, 0,   stb, BT_CHARACTER, dc, 0, NULL);

  make_generic("lgt");

  add_sym("lle", 1, 0, BT_LOGICAL, dl,
	  NULL, g95_simplify_lle, NULL,
	  sta, BT_CHARACTER, dc, 0,   stb, BT_CHARACTER, dc, 0, NULL);

  make_generic("lle");

  add_sym("llt", 1, 0, BT_LOGICAL, dl,
	  NULL, g95_simplify_llt, NULL,
	  sta, BT_CHARACTER, dc, 0,   stb, BT_CHARACTER, dc, 0, NULL);

  make_generic("llt");

  add_sym("log", 1, 1, BT_REAL, dr,
	  NULL, g95_simplify_log, NULL,
	  x, BT_REAL, dr, 0, NULL);

  add_sym("alog", 1, 1, BT_REAL, dr,
	  NULL, g95_simplify_log, NULL,
	  x, BT_REAL, dr, 0, NULL);

  add_sym("dlog", 1, 1, BT_REAL, dd,
	  NULL, g95_simplify_log, NULL,
	  x, BT_REAL, dd, 0, NULL);

  add_sym("clog", 1, 1, BT_COMPLEX, dz,
	  NULL, g95_simplify_log, NULL,
	  x, BT_COMPLEX, dz, 0, NULL);

  add_sym("zlog", 1, 1, BT_COMPLEX, dd,
	  NULL, g95_simplify_log, NULL,
	  x, BT_COMPLEX, dd, 0, NULL);   /* Extension */

  make_alias("cdlog");

  make_generic("log");

  add_sym("log10", 1, 1, BT_REAL, dr,
	  NULL, g95_simplify_log10, NULL,
	  x, BT_REAL, dr, 0, NULL);

  add_sym("alog10", 1, 1, BT_REAL, dr,
	  NULL, g95_simplify_log10, NULL,
	  x, BT_REAL, dr, 0, NULL);

  add_sym("dlog10", 1, 1, BT_REAL, dd,
	  NULL, g95_simplify_log10, NULL,
	  x, BT_REAL, dd, 0, NULL);

  make_generic("log10");

  add_sym("logical", 0, 1, BT_LOGICAL, dl,
	  g95_check_logical, g95_simplify_logical, g95_resolve_logical,
	  l, BT_LOGICAL, dl, 0,   kind, BT_INTEGER, di, 1, NULL);

  add_sym("matmul", 0, 1, BT_REAL, dr,
	  g95_check_matmul, NULL, NULL,
	  ma, BT_REAL, dr, 0,   mb, BT_REAL, dr, 0, NULL);

/* Note: amax0 is equivalent to real(max), max1 is equivalent to
 * int(max).  The max function must take at least two arguments. */

  add_sym("max", 1, 0, BT_UNKNOWN, 0,
	  g95_check_min_max, g95_simplify_max, g95_resolve_max,
	  a1, BT_UNKNOWN, dr, 0,   a2, BT_UNKNOWN, dr, 0, NULL);

  add_sym("max0", 1, 0, BT_INTEGER, di,
	  g95_check_min_max_integer, g95_simplify_max, NULL,
	  a1, BT_INTEGER, di, 0,   a2, BT_INTEGER, di, 0, NULL);

  add_sym("amax0", 1, 0, BT_REAL, dr,
	  g95_check_min_max_integer, g95_simplify_max, NULL,
	  a1, BT_INTEGER, di, 0,   a2, BT_INTEGER, di, 0, NULL);

  add_sym("amax1", 1, 0, BT_REAL, dr,
	  g95_check_min_max_real, g95_simplify_max, NULL,
	  a1, BT_REAL, dr, 0,   a2, BT_REAL, dr, 0, NULL);

  add_sym("max1", 1, 0, BT_INTEGER, di,
	  g95_check_min_max_real, g95_simplify_max, NULL,
	  a1, BT_REAL, dr, 0,   a2, BT_REAL, dr, 0, NULL);

  add_sym("dmax1", 1, 0, BT_REAL, dd,
	  g95_check_min_max_double, g95_simplify_max, NULL,
	  a1, BT_REAL, dd, 0,   a2, BT_REAL, dd, 0, NULL);

  make_generic("max");

  add_sym("maxexponent", 0, 1, BT_INTEGER, di,
	  g95_check_x, g95_simplify_maxexponent, NULL,
	  x, BT_UNKNOWN, dr, 0, NULL);

  add_sym("maxloc", 0, 1, BT_INTEGER, di,
	  g95_check_minloc_maxloc, NULL, NULL,
	  ar, BT_REAL, dr, 0,   dm, BT_INTEGER, di, 1,
	  msk, BT_LOGICAL, dl, 1, NULL);

  add_sym("maxval", 0, 1, BT_REAL, dr,
	  g95_check_minval_maxval, NULL, g95_resolve_maxval,
	  ar, BT_REAL, dr, 0,   dm, BT_INTEGER, di, 1,
	  msk, BT_LOGICAL, dl, 1, NULL);

  add_sym("merge", 1, 1, BT_REAL, dr,
	  g95_check_merge, NULL, NULL,
	  ts, BT_REAL, dr, 0,	  fs, BT_REAL, dr, 0,
	  msk, BT_LOGICAL, dl, 0, NULL);

/* Note: amin0 is equivalent to real(min), min1 is equivalent to int(min). */

  add_sym("min", 1, 0, BT_UNKNOWN, 0,
	  g95_check_min_max, g95_simplify_min, g95_resolve_min,
	  a1, BT_REAL, dr, 0,   a2, BT_REAL, dr, 0, NULL);

  add_sym("min0", 1, 0, BT_INTEGER, di,
	  g95_check_min_max_integer, g95_simplify_min, NULL,
	  a1, BT_INTEGER, di, 0,   a2, BT_INTEGER, di, 0, NULL);

  add_sym("amin0", 1, 0, BT_REAL, dr,
	  g95_check_min_max_integer, g95_simplify_min, NULL,
	  a1, BT_INTEGER, di, 0,   a2, BT_INTEGER, di, 0, NULL);

  add_sym("amin1", 1, 0, BT_REAL, dr,
	  g95_check_min_max_real, g95_simplify_min, NULL,
	  a1, BT_REAL, dr, 0,   a2, BT_REAL, dr, 0, NULL);

  add_sym("min1", 1, 0, BT_INTEGER, di,
	  g95_check_min_max_real, g95_simplify_min, NULL,
	  a1, BT_REAL, dr, 0,   a2, BT_REAL, dr, 0, NULL);

  add_sym("dmin1", 1, 0, BT_REAL, dd,
	  g95_check_min_max_double, g95_simplify_min, NULL,
	  a1, BT_REAL, dd, 0,   a2, BT_REAL, dd, 0, NULL);

  make_generic("min");

  add_sym("minexponent", 0, 1, BT_INTEGER, di,
	  g95_check_x, g95_simplify_minexponent, NULL,
	  x, BT_UNKNOWN, dr, 0, NULL);

  add_sym("minloc", 0, 1, BT_INTEGER, di,
	  g95_check_minloc_maxloc, NULL, NULL,
	  ar, BT_REAL, dr, 0,   dm, BT_INTEGER, di, 1,
	  msk, BT_LOGICAL, dl, 1, NULL);

  add_sym("minval", 0, 1, BT_REAL, dr,
	  g95_check_minval_maxval, NULL, g95_resolve_minval,
	  ar, BT_REAL, dr, 0,   dm, BT_INTEGER, di, 1,
	  msk, BT_LOGICAL, dl, 1, NULL);

  add_sym("mod", 1, 1, BT_INTEGER, di,
	  g95_check_a_p, g95_simplify_mod, g95_resolve_mod,
	  a, BT_INTEGER, di, 0,   p, BT_INTEGER, di, 0, NULL);

  add_sym("amod", 1, 1, BT_REAL, dr,
	  NULL, g95_simplify_mod, g95_resolve_mod,
	  a, BT_REAL, dr, 0,   p, BT_REAL, dr, 0, NULL);

  add_sym("dmod", 1, 1, BT_REAL, dd,
	  NULL, g95_simplify_mod, g95_resolve_mod,
	  a, BT_REAL, dd, 0,   p, BT_REAL, dd, 0, NULL);

  make_generic("mod");

  add_sym("modulo", 1, 1, BT_REAL, di,
	  g95_check_a_p, g95_simplify_modulo, g95_resolve_modulo,
	  a, BT_REAL, di, 0,   p, BT_REAL, di, 0, NULL);

  add_sym("nearest", 1, 1, BT_REAL, dr,
	  g95_check_nearest, g95_simplify_nearest, NULL,
	  x, BT_REAL, dr, 0,   s, BT_REAL, dr, 0, NULL);

  add_sym("nint", 1, 1, BT_INTEGER, di,
	  g95_check_a_kind, g95_simplify_nint, g95_resolve_nint,
	  a, BT_REAL, dr, 0,   kind, BT_INTEGER, di, 1, NULL);

  add_sym("idnint", 1, 1, BT_INTEGER, di,
	  g95_check_idnint, g95_simplify_idnint, NULL,
	  a, BT_REAL, dd, 0,   kind, BT_INTEGER, di, 1, NULL);

  make_generic("nint");

  add_sym("not", 1, 1, BT_INTEGER, di,
	  g95_check_i, g95_simplify_not, g95_resolve_not,
	  i, BT_INTEGER, di, 0, NULL);

  add_sym("null", 0, 1, BT_INTEGER, di,
	  g95_check_null, g95_simplify_null, NULL,
	  mo, BT_INTEGER, di, 1, NULL);

  add_sym("pack", 0, 1, BT_REAL, dr,
	  g95_check_pack, NULL, NULL,
	  ar, BT_REAL, dr, 0,   msk, BT_LOGICAL, dl, 0,
	  v, BT_REAL, dr, 1, NULL);

  add_sym("precision", 0, 1, BT_INTEGER, di,
	  g95_check_precision, g95_simplify_precision, NULL,
	  x, BT_UNKNOWN, 0, 0, NULL);

  add_sym("present", 0, 1, BT_LOGICAL, dl,
	  g95_check_present, NULL, NULL,
	  a, BT_REAL, dr, 0, NULL);

  add_sym("product", 0, 1, BT_REAL, dr,
	  g95_check_product, NULL, g95_resolve_product,
	  ar, BT_REAL, dr, 0,   dm, BT_INTEGER, di, 1,
	  msk, BT_LOGICAL, dl, 1, NULL);

  add_sym("radix", 0, 1, BT_INTEGER, di,
	  g95_check_radix, g95_simplify_radix, NULL,
	  x, BT_UNKNOWN, 0, 0, NULL);

  add_sym("range", 0, 1, BT_INTEGER, di,
	  g95_check_range, g95_simplify_range, NULL,
	  x, BT_REAL, dr, 0, NULL);

  add_sym("real", 1, 0, BT_REAL, dr,
	  g95_check_real, g95_simplify_real, g95_resolve_real,
	  a, BT_INTEGER, di, 0,   kind, BT_INTEGER, di, 1, NULL);

  add_sym("float", 1, 0, BT_REAL, dr,
	  NULL, g95_simplify_float, NULL,
	  a, BT_INTEGER, di, 0, NULL);

  add_sym("sngl", 1, 0, BT_REAL, dr,
	  NULL, g95_simplify_sngl, NULL,
	  a, BT_REAL, dd, 0, NULL);

  make_generic("real");

  add_sym("repeat", 0, 1, BT_CHARACTER, dc,
	  g95_check_repeat, g95_simplify_repeat, NULL,
	  stg, BT_CHARACTER, dc, 0,   n, BT_INTEGER, di, 0, NULL);

  add_sym("reshape", 0, 1, BT_REAL, dr,
	  g95_check_reshape, g95_simplify_reshape, g95_resolve_reshape,
	  src, BT_REAL, dr, 0,   shp, BT_INTEGER, di, 0,
	  pad, BT_REAL, dr, 1,   ord, BT_INTEGER, di, 1, NULL);

  add_sym("rrspacing", 1, 1, BT_REAL, dr,
	  g95_check_x, g95_simplify_rrspacing, g95_resolve_rrspacing,
	  x, BT_REAL, dr, 0, NULL);

  add_sym("scale", 1, 1, BT_REAL, dr,
	  g95_check_scale, NULL, g95_resolve_scale,
	  x, BT_REAL, dr, 0,   i, BT_INTEGER, di, 0, NULL);

  make_generic("scale");

  add_sym("scan", 1, 1, BT_INTEGER, di,
	  g95_check_scan, g95_simplify_scan, NULL,
	  stg, BT_CHARACTER, dc, 0,  set, BT_CHARACTER, dc, 0,
	  bck, BT_LOGICAL, dl, 1, NULL);

  add_sym("selected_int_kind", 0, 1, BT_INTEGER, di,
	  NULL, g95_simplify_selected_int_kind, NULL,
	  r, BT_INTEGER, di, 0, NULL);

  add_sym("selected_real_kind", 0, 1, BT_INTEGER, di,
	  g95_check_selected_real_kind, g95_simplify_selected_real_kind, NULL,
	  p, BT_INTEGER, di, 1,   r, BT_INTEGER, di, 1, NULL);

  add_sym("set_exponent", 1, 1, BT_REAL, dr,
	  g95_check_set_exponent, g95_simplify_set_exponent,
	  g95_resolve_set_exponent,
	  x, BT_REAL, dr, 0,   i, BT_INTEGER, di, 0, NULL);

  add_sym("shape", 0, 1, BT_INTEGER, di,
	  g95_check_shape, NULL, g95_resolve_shape,
	  src, BT_REAL, dr, 0, NULL);

  add_sym("sign", 1, 1, BT_REAL, dr,
	  g95_check_sign, g95_simplify_sign, NULL,
	  a, BT_REAL, dr, 0,   b, BT_REAL, dr, 0, NULL);

  add_sym("isign", 1, 1, BT_INTEGER, di,
	  NULL, g95_simplify_sign, NULL,
	  a, BT_INTEGER, di, 0,   b, BT_INTEGER, di, 0, NULL);

  add_sym("dsign", 1, 1, BT_REAL, dd,
	  NULL, g95_simplify_sign, NULL,
	  a, BT_REAL, dd, 0,   b, BT_REAL, dd, 0, NULL);

  make_generic("sign");

  add_sym("sin", 1, 1, BT_REAL, dr,
	  NULL, g95_simplify_sin, NULL,
	  x, BT_REAL, dr, 0, NULL);

  add_sym("dsin", 1, 1, BT_REAL, dd,
	  NULL, g95_simplify_sin, NULL,
	  x, BT_REAL, dd, 0, NULL);

  add_sym("csin", 1, 1, BT_COMPLEX, dz,
	  NULL, g95_simplify_sin, NULL,
	  x, BT_COMPLEX, dz, 0, NULL);

  add_sym("zsin", 1, 1, BT_COMPLEX, dd,
	  NULL, g95_simplify_sin, NULL,
	  x, BT_COMPLEX, dd, 0, NULL);   /* Extension */

  make_alias("cdsin");

  make_generic("sin");

  add_sym("sinh", 1, 1, BT_REAL, dr,
	  NULL, g95_simplify_sinh, NULL,
	  x, BT_REAL, dr, 0, NULL);

  add_sym("dsinh", 1, 1, BT_REAL, dd,
	  NULL, g95_simplify_sinh, NULL,
	  x, BT_REAL, dd, 0, NULL);

  make_generic("sinh");

  add_sym("size", 0, 1, BT_INTEGER, di,
	  g95_check_size, NULL, NULL,
	  ar, BT_REAL, dr, 0,   dm, BT_INTEGER, di, 1, NULL);

  add_sym("spacing", 1, 1, BT_REAL, dr,
	  g95_check_x, g95_simplify_spacing, g95_resolve_spacing,
	  x, BT_REAL, dr, 0, NULL);

  add_sym("spread", 0, 1, BT_REAL, dr,
	  g95_check_spread, NULL, NULL,
	  src, BT_REAL, dr, 0,   dm, BT_INTEGER, di, 0,
	  n, BT_INTEGER, di, 0, NULL);

  add_sym("sqrt", 1, 1, BT_REAL, dr,
	  NULL, g95_simplify_sqrt, NULL,
	  x, BT_REAL, dr, 0, NULL);

  add_sym("dsqrt", 1, 1, BT_REAL, dd,
	  NULL, g95_simplify_sqrt, NULL,
	  x, BT_REAL, dd, 0, NULL);

  add_sym("csqrt", 1, 1, BT_COMPLEX, dz,
	  NULL, g95_simplify_sqrt, NULL,
	  x, BT_COMPLEX, dz, 0, NULL);

  add_sym("zsqrt", 1, 1, BT_COMPLEX, dd,
	  NULL, g95_simplify_sqrt, NULL,
	  x, BT_COMPLEX, dd, 0, NULL);   /* Extension */

  make_alias("cdsqrt");

  make_generic("sqrt");

  add_sym("sum", 0, 1, BT_UNKNOWN, 0,
	  g95_check_sum, NULL, g95_resolve_sum,
	  ar, BT_REAL, dr, 0,   dm, BT_INTEGER, di, 1,
	  msk, BT_LOGICAL, dl, 1, NULL);

  add_sym("tan", 1, 1, BT_REAL, dr,
	  NULL, g95_simplify_tan, NULL,
	  x, BT_REAL, dr, 0, NULL);

  add_sym("dtan", 1, 1, BT_REAL, dd,
	  NULL, g95_simplify_tan, NULL,
	  x, BT_REAL, dd, 0, NULL);

  make_generic("tan");

  add_sym("tanh", 1, 1, BT_REAL, dr,
	  NULL, g95_simplify_tanh, NULL,
	  x, BT_REAL, dr, 0, NULL);

  add_sym("dtanh", 1, 1, BT_REAL, dd,
	  NULL, g95_simplify_tanh, NULL,
	  x, BT_REAL, dd, 0, NULL);
 
  make_generic("tanh");

  add_sym("tiny", 0, 1, BT_REAL, dr,
	  g95_check_x, g95_simplify_tiny, NULL,
	  x, BT_REAL, dr, 0, NULL);

  add_sym("transfer", 0, 1, BT_REAL, dr,
	  g95_check_transfer, NULL, NULL,
	  src, BT_REAL, dr, 0,    mo, BT_REAL, dr, 0,
	  sz, BT_INTEGER, di, 1,  NULL);

  add_sym("transpose", 0, 1, BT_REAL, dr,
	  g95_check_transpose, NULL, g95_resolve_transpose,
	  m, BT_REAL, dr, 0, NULL);

  add_sym("trim", 0, 1, BT_CHARACTER, dc,
	  NULL, g95_simplify_trim, NULL,
	  stg, BT_CHARACTER, dc, 0, NULL);

  add_sym("ubound", 0, 1, BT_INTEGER, di,
	  g95_check_ubound, NULL, NULL,
	  ar, BT_REAL, dr, 0,   dm, BT_INTEGER, di, 1, NULL);

  add_sym("unpack", 0, 1, BT_REAL, dr,
	  g95_check_unpack, NULL, NULL,
	  v, BT_REAL, dr, 0,   msk, BT_LOGICAL, dl, 0,
	  f, BT_REAL, dr, 0, NULL);

  add_sym("verify", 1, 1, BT_INTEGER, di,
	  g95_check_verify, g95_simplify_verify, NULL,
	  stg, BT_CHARACTER, dc, 0,   set, BT_CHARACTER, dc, 0,
	  bck, BT_LOGICAL, dl, 1, NULL);
}



/* add_subroutines()-- Add intrinsic subroutines */

static void add_subroutines(void) {

/* Argument names as in the standard (to be used as argument keywords) */
char   h[] = "harvest", dt[] = "date", vl[] = "values",  pt[] = "put",
       c[] = "count",   tm[] = "time", tp[] = "topos",   gt[] = "get",
       t[] = "to",      zn[] = "zone", fp[] = "frompos", cm[] = "count_max",
       f[] = "from",    sz[] = "size", ln[] = "len",     cr[] = "count_rate";

int di, dr, dc;

  di = g95_default_integer_kind(); 
  dr = g95_default_real_kind();
  dc = g95_default_character_kind();

  add_sym("cpu_time", 0, 1, BT_UNKNOWN, 0,
	  NULL, NULL, NULL,
	  tm, BT_REAL, dr, 0, NULL);

  add_sym("date_and_time", 0, 1, BT_UNKNOWN, 0,
	  g95_check_date_and_time, NULL, NULL,
	  dt, BT_CHARACTER, dc, 1,   tm, BT_CHARACTER, dc, 1,
	  zn, BT_CHARACTER, dc, 1,   vl, BT_INTEGER,   di, 1, NULL);

  add_sym("mvbits", 1, 1, BT_UNKNOWN, 0,
	  g95_check_mvbits, g95_simplify_mvbits, NULL,
	  f, BT_INTEGER, di, 0,   fp, BT_INTEGER, di, 0,
	  ln, BT_INTEGER, di, 0,   t, BT_INTEGER, di, 0,
	  tp, BT_INTEGER, di, 0, NULL);

  add_sym("random_number", 0, 1, BT_UNKNOWN, 0,
	  g95_check_random_number, NULL, NULL,
	  h, BT_REAL, dr, 0, NULL);

  add_sym("random_seed", 0, 1, BT_UNKNOWN, 0,
	  g95_check_random_seed, NULL, NULL,
	  sz, BT_INTEGER, di, 1,   pt, BT_INTEGER, di, 1,
	  gt, BT_INTEGER, di, 1, NULL);

  add_sym("system_clock", 0, 1, BT_UNKNOWN, 0,
	  NULL, NULL, NULL,
	  c,  BT_INTEGER, di, 1,   cr, BT_INTEGER, di, 1,
	  cm, BT_INTEGER, di, 1, NULL);
}


/* add_conv()-- Add a function to the list of conversion symbols */

static void add_conv(bt from_type, int from_kind, bt to_type, int to_kind,
		     g95_expr *(*simplify)()) {

g95_typespec from, to;
intrinsic_sym *sym;

  if (sizing == SZ_CONVS) {
    nconv++;
    return;
  }

  g95_clear_ts(&from);
  from.type = from_type;
  from.kind = from_kind;

  g95_clear_ts(&to);
  to.type = to_type;
  to.kind = to_kind;

  sym = conversion + nconv;

  strcpy(sym->name, conv_name(&from, &to));
  strcpy(sym->lib_name, sym->name);
  sym->simplify = simplify;
  sym->elemental = 1;
  sym->ts = to;

  nconv++;
}


/* add_conversions()-- Create intrinsic_sym nodes for all intrinsic
 * conversion functions by looping over the kind tables. */

static void add_conversions(void) {
int i, j;

  /* Integer-Integer conversions */

  for(i=0; g95_integer_kinds[i].kind != 0; i++)
    for(j=0; g95_integer_kinds[j].kind != 0; j++) {
      if (i == j) continue;

      add_conv(BT_INTEGER, g95_integer_kinds[i].kind,
	       BT_INTEGER, g95_integer_kinds[j].kind, convert_constant);
    }

  /* Integer-Real/Complex conversions */

  for(i=0; g95_integer_kinds[i].kind != 0; i++)
    for(j=0; g95_real_kinds[j].kind != 0; j++) {
      add_conv(BT_INTEGER, g95_integer_kinds[i].kind,
	       BT_REAL,    g95_real_kinds[j].kind, convert_constant);

      add_conv(BT_REAL,    g95_real_kinds[j].kind,
	       BT_INTEGER, g95_integer_kinds[i].kind, convert_constant);

      add_conv(BT_INTEGER, g95_integer_kinds[i].kind,
	       BT_COMPLEX, g95_real_kinds[j].kind, convert_constant);

      add_conv(BT_COMPLEX, g95_real_kinds[j].kind,
	       BT_INTEGER, g95_integer_kinds[i].kind, convert_constant);
    }

  /* Real/Complex - Real/Complex conversions */

  for(i=0; g95_real_kinds[i].kind != 0; i++)
    for(j=0; g95_real_kinds[j].kind != 0; j++) {
      if (i != j) {
	add_conv(BT_REAL, g95_real_kinds[i].kind,
		 BT_REAL, g95_real_kinds[j].kind, convert_constant);

	add_conv(BT_COMPLEX, g95_real_kinds[i].kind,
		 BT_COMPLEX, g95_real_kinds[j].kind, convert_constant);
      }

      add_conv(BT_REAL,    g95_real_kinds[i].kind,
	       BT_COMPLEX, g95_real_kinds[j].kind, convert_constant);

      add_conv(BT_COMPLEX, g95_real_kinds[i].kind,
	       BT_REAL,    g95_real_kinds[j].kind, convert_constant);
    }

  /* Logical/Logical kind conversion */

  for(i=0; g95_logical_kinds[i].kind; i++)
    for(j=0; g95_logical_kinds[j].kind; j++) {
      if (i == j) continue;

      add_conv(BT_LOGICAL, g95_real_kinds[i].kind,
	       BT_LOGICAL, g95_real_kinds[j].kind, convert_constant);
    }
}


/* g95_intrinsic_init_1()-- Initialize the table of intrinsics */

void g95_intrinsic_init_1(void) {

  nargs = nfunc = nsub = nconv = 0;

  sizing = SZ_FUNCS;
  add_functions();
  sizing = SZ_SUBS;
  add_subroutines();
  sizing = SZ_CONVS;
  add_conversions();

  functions = g95_getmem(sizeof(intrinsic_sym)*(nfunc+nsub)
			 + sizeof(intrinsic_arg)*nargs);

  next_sym = functions;
  subroutines = functions + nfunc;

  conversion = g95_getmem(sizeof(intrinsic_sym)*nconv);

  next_arg = ((intrinsic_arg *) (subroutines + nsub)) - 1;

  sizing = SZ_NOTHING;
  nconv = 0;

  add_functions();
  add_subroutines();
  add_conversions();
}


void g95_intrinsic_done_1(void) {

  g95_free(functions);
  g95_free(conversion);
}


/* g95_intrinsic_symbol()-- Given a symbol that we have decided is
 * intrinsic, mark it as such by placing it into a special module that
 * is otherwise impossible to read or write. */

void g95_intrinsic_symbol(g95_symbol *sym) {

  strcpy(sym->module, "(intrinsic)");
}


/******** Subroutines to check intrinsic interfaces ***********/

/* remove_nullargs()-- Given a formal argument list, remove any
 * NULL arguments that may have been left behind by a sort against
 * some formal argument list. */

static void remove_nullargs(g95_actual_arglist **ap) {
g95_actual_arglist *head, *tail, *next;

  tail = NULL;

  for(head=*ap; head; head=next) {
    next = head->next;

    if (head->expr == NULL) {
      head->next = NULL;
      g95_free_actual_arglist(head);
    } else {
      if (tail == NULL)
	*ap = head;
      else
	tail->next = head;

      tail = head;
      tail->next = NULL;
    }
  }

  if (tail == NULL) *ap = NULL;
}


/* sort_actual()-- Given an actual arglist and a formal arglist, sort
 * the actual arglist so that its arguments are in a one-to-one
 * correspondence with the format arglist.  Arguments that are not
 * present are given a blank g95_actual_arglist structure.  If
 * something is obviously wrong (say, a missing required argument) we
 * abort sorting and return FAILURE. */

static try sort_actual(const char *name, g95_actual_arglist **ap,
		       intrinsic_arg *formal, locus *where) {

g95_actual_arglist *actual, *a;
intrinsic_arg *f;
int i;

  remove_nullargs(ap);
  actual = *ap;

  for(f=formal; f; f=f->next)
    f->actual = NULL;

  f = formal;
  a = actual;

  for(;;) {     /* Put the nonkeyword arguments in a 1:1 correspondence */
    if (f == NULL) break;
    if (a == NULL) goto optional;

    if (a->name[0] != '\0') goto keywords;

    f->actual = a;

    f = f->next;
    a = a->next;
  }

  if (a == NULL) goto do_sort;

  g95_error("Too many arguments in call to '%s' at %L", name, where);
  return FAILURE;

/* Associate the remaining actual arguments, all of which have to be
 * keyword arguments. */

keywords:
  for(; a; a=a->next) {
    for(f=formal, i=0; f; f=f->next, i++)
      if (strcmp(a->name, f->name) == 0) break;

    if (f == NULL) {
      g95_error("Can't find keyword named '%s' in call to '%s' at %L",
		a->name, name, where);
      return FAILURE;
    }

    if (f->actual != NULL) {
      g95_error("Argument '%s' is appears twice in call to '%s' at %L",
		f->name, name, where);
      return FAILURE;
    }

    f->actual = a;
  }

/* At this point, all unmatched formal args must be optional */

optional:
  for(f=formal; f; f=f->next) {
    if (f->actual == NULL && f->optional == 0) {
      g95_error("Missing actual argument '%s' in call to '%s' at %L",
		f->name, name, where);
      return FAILURE;
    }
  }

/* Using the formal argument list, string the actual argument list
 * together in a way that corresponds with the formal list. */

do_sort:
  actual = NULL;

  for(f=formal; f; f=f->next) {
    a = (f->actual == NULL) ? g95_get_actual_arglist() : f->actual;

    if (actual == NULL)
      *ap = a;
    else
      actual->next = a;

    actual = a;
  }
  actual->next = NULL;  /* End the sorted argument list. */
      
  return SUCCESS;
}


/* check_arglist()-- Compare an actual argument list with an
 * intrinsic's formal argument list.  The lists are checked for
 * agreement of type.  We don't check for arrayness here. */

static try check_arglist(g95_actual_arglist **ap, intrinsic_sym *sym,
			 int error_flag) {
g95_actual_arglist *actual;
intrinsic_arg *formal;
int i;

  formal = sym->arg;
  actual = *ap;

  i = 0;
  for(; formal; formal=formal->next, actual=actual->next, i++) {
    if (actual->expr == NULL) continue;

    if (!g95_compare_types(&formal->ts, &actual->expr->ts)) {
      if (error_flag)
	g95_error("Type of argument '%s' in call to '%s' at %L should be "
		  "%s, not %s", g95_intrinsic_arg[i],
		  g95_current_intrinsic, &actual->expr->where,
		  g95_typename(&formal->ts), g95_typename(&actual->expr->ts));
      return FAILURE;
    }
  }

  return SUCCESS;
}


/* resolve_intrinsic()-- Given a pointer to an intrinsic symbol and an
 * expression node that represent the function call to that
 * subroutine, figure out the type of the result.  This may involve
 * calling a resolution subroutine */

static void resolve_intrinsic(intrinsic_sym *specific, g95_expr *e) {
g95_expr *a1, *a2, *a3, *a4, *a5;
g95_actual_arglist *arg;

  if (specific->elemental) e->rank = e->value.function.actual->expr->rank;

  if (specific->resolve == NULL) {
    if (e->value.function.name == NULL)
      e->value.function.name = specific->lib_name;

    if (e->ts.type == BT_UNKNOWN) e->ts = specific->ts;
    return;
  }

  arg = e->value.function.actual;

  a1 = arg->expr;
  arg = arg->next;

  if (arg == NULL || specific->resolve == g95_resolve_max ||
      specific->resolve == g95_resolve_min) {

    (*specific->resolve)(e, a1);
    return;
  }

  a2 = arg->expr;
  arg = arg->next;

  if (arg == NULL) {
    (*specific->resolve)(e, a1, a2);
    return;
  }

  a3 = arg->expr;
  arg = arg->next;
      
  if (arg == NULL) {
    (*specific->resolve)(e, a1, a2, a3);
    return;
  }

  a4 = arg->expr;
  arg = arg->next;

  if (arg == NULL) {
    (*specific->resolve)(e, a1, a2, a3, a4);
    return;
  }

  a5 = arg->expr;
  arg = arg->next;

  if (arg == NULL) {
    (*specific->resolve)(e, a1, a2, a3, a4, a5);
    return;
  }

  g95_internal_error("resolve_intrinsic(): Too many args for intrinsic");
}


/* do_simplify()-- Given an intrinsic symbol node and an expression
 * node, call the simplification function (if there is one), perhaps
 * replacing the expression with something simpler.  We return FAILURE
 * on an error of the simplification, SUCCESS if the simplification
 * worked, even if nothing has changed in the expression itself */

static try do_simplify(intrinsic_sym *specific, g95_expr *e) {
g95_expr *result, *a1, *a2, *a3, *a4, *a5;
g95_actual_arglist *arg;

/* Max and min require special handling due to the variable number of args */

  if (specific->simplify == g95_simplify_min) {
    result = g95_simplify_min(e);
    goto finish;
  }

  if (specific->simplify == g95_simplify_max) {
    result = g95_simplify_max(e);
    goto finish;
  }

  if (specific->simplify == NULL) {
    result = NULL;
    goto finish;
  }

  arg = e->value.function.actual;

  a1 = arg->expr;
  arg = arg->next;

  if (specific->simplify == convert_constant) {
    result = convert_constant(a1, specific->ts.type, specific->ts.kind);
    goto finish;
  }

  /* TODO: Warn if -pedantic and initialization expression and arg
   * types not integer or character */

  if (arg == NULL)
    result = (*specific->simplify)(a1);
  else {
    a2 = arg->expr;
    arg = arg->next;

    if (arg == NULL)
      result = (*specific->simplify)(a1, a2);
    else {
      a3 = arg->expr;
      arg = arg->next;
      
      if (arg == NULL)
	result = (*specific->simplify)(a1, a2, a3);
      else {
	a4 = arg->expr;
	arg = arg->next;

	if (arg == NULL)
	  result = (*specific->simplify)(a1, a2, a3, a4);
  	else {
	  a5 = arg->expr;
	  arg = arg->next;

	  if (arg == NULL)
	    result = (*specific->simplify)(a1, a2, a3, a4, a5);
	  else 
	    g95_internal_error("do_simplify(): Too many args for intrinsic");
	}
      }
    }
  }

 finish:
  if (result == &g95_bad_expr) return FAILURE;

  if (result == NULL)
    resolve_intrinsic(specific, e);         /* Must call at run-time */
  else {
    result->where = e->where;
    g95_replace_expr(e, result);
  }

  return SUCCESS;
}


/* check_specific()-- Given a pointer to an intrinsic symbol and an
 * expression consisting of a function call, see if the function call
 * is consistent with the intrinsic's formal argument list.  Return
 * SUCCESS if the expression and intrinsic match, FAILURE otherwise.  */

static try check_specific(intrinsic_sym *specific, g95_expr *expr,
			  int error_flag) {
g95_actual_arglist **ap;
intrinsic_arg *formal;
try t;
int i;

  ap = &expr->value.function.actual;

/* Don't attempt to sort the argument list for min or max */

  if (specific->check == g95_check_min_max ||
      specific->check == g95_check_min_max_integer ||
      specific->check == g95_check_min_max_real ||
      specific->check == g95_check_min_max_double)
    return (*specific->check)(*ap);

  i = 0; 
  for(formal=specific->arg; formal; formal=formal->next) {
    if (i >= MAX_INTRINSIC_ARGS)
      g95_internal_error("check_arglist(): MAX_INTRINSICS_ARGS too small");

    g95_intrinsic_arg[i++] = formal->name;
  }

  if (sort_actual(specific->name, ap, specific->arg, &expr->where) == FAILURE)
    return FAILURE;

  if (specific->check == NULL) {
    t = check_arglist(ap, specific, error_flag);
    if (t == SUCCESS) expr->ts = specific->ts;
  } else
    t = do_check(specific, *ap);

  if (t == FAILURE) remove_nullargs(ap);

  return t;
}


/* g95_intrinsic_func_interface()-- see if a function call corresponds
 * to an intrinsic function call.  We return:
 *  MATCH_YES    if the call corresponds to an intrinsic, simplification
 *               is done if possible.
 *
 *  MATCH_NO     if the call does not correspond to an intrinsic
 *
 *  MATCH_ERROR  if the call corresponds to an intrinsic but there was an
 *               error during the simplification process.
 *
 * The error_flag parameter enables an error reporting.
 */

match g95_intrinsic_func_interface(g95_expr *expr, int error_flag) {
intrinsic_sym *isym, *specific;
g95_actual_arglist *actual;
const char *name;
int flag;

  if (expr->value.function.isym != NULL)
    return (do_simplify(expr->value.function.isym, expr) == FAILURE)
      ? MATCH_ERROR : MATCH_YES;

  g95_suppress_error = !error_flag;
  g95_intrinsic_extension = 1;
  flag = 0;

  for(actual=expr->value.function.actual; actual; actual=actual->next)
    if (actual->expr != NULL)
      flag |= (actual->expr->ts.type != BT_INTEGER &&
	       actual->expr->ts.type != BT_CHARACTER);

  name = expr->symbol->name;

  isym = specific = find_function(name);
  if (isym == NULL) {
    g95_suppress_error = 0;
    return MATCH_NO;
  }

  g95_current_intrinsic = isym->name;
  g95_current_intrinsic_where = &expr->where;

/* Bypass the generic list for min and max */

  if (isym->check == g95_check_min_max) {
    if (g95_check_min_max(expr->value.function.actual) == SUCCESS)
      goto got_specific;

    g95_suppress_error = 0;
    return MATCH_NO;
  }

/* If the function is generic, check all of its specific incarnations.
 * If the generic name is also a specific, we check that name last, so
 * that any error message will correspond to the specific */

  g95_suppress_error = 1;

  if (isym->specific_head != NULL) {
    for(specific=isym->specific_head; specific; specific=specific->next) {
      if (specific == isym) continue;
      if (check_specific(specific, expr, 0) == SUCCESS) goto got_specific;
    }
  }

  g95_suppress_error = !error_flag;

  if (check_specific(isym, expr, error_flag) == FAILURE) {
    g95_suppress_error = 0;
    return MATCH_NO;
  }

  specific = isym;

got_specific:
  expr->value.function.isym = specific;
  g95_intrinsic_symbol(expr->symbol);

  if (do_simplify(specific, expr) == FAILURE) {
    g95_suppress_error = 0;
    return MATCH_ERROR;
  }

  flag |= (expr->ts.type != BT_INTEGER && expr->ts.type != BT_CHARACTER);

  if (flag && g95_intrinsic_extension && g95_option.pedantic)
    g95_warning("Evaluation of initialization expression at %L is nonstandard",
		&expr->where);

  g95_suppress_error = 0;
  return MATCH_YES;
}


/* g95_intrinsic_sub_interface()-- see if a CALL statement corresponds
 * to an intrinsic subroutine */

try g95_intrinsic_sub_interface(g95_code *c) {
g95_actual_arglist **argp;
intrinsic_sym *isym;
char *name;

  name = c->sym->name;
  argp = (g95_actual_arglist **) &c->ext;

  isym = find_subroutine(name);
  if (isym == NULL) {
    g95_error("The subroutine '%s' at %L is not a valid intrinsic", name,
	      &c->loc);
    return FAILURE;
  }

  if (isym->check != NULL) return do_check(isym, *argp);

  return check_arglist(argp, isym, 1);
}


/* g95_convert_type()-- Tries to convert an expression (in place) from
 * one type to another.  The eflag control the behavior on error.
 * The possible values are:
 *   1    Generate a g95_error()
 *   2    Generate a g95_internal_error() 
 */

try g95_convert_type(g95_expr *expr, g95_typespec *ts, int eflag) {
intrinsic_sym *sym;
locus old_where;
g95_expr *new;

  if (ts->type == BT_UNKNOWN) goto bad;

  if (expr->expr_type == EXPR_NULL) { /* NULL pointers aquire types easily */
    expr->ts = *ts;
    return SUCCESS;
  }

  if (expr->ts.type == BT_UNKNOWN) goto bad;

  if (expr->ts.type == BT_DERIVED && ts->type == BT_DERIVED &&
      expr->ts.derived == ts->derived) return SUCCESS;

  sym = find_conv(&expr->ts, ts);
  if (sym == NULL) goto bad;

/* Insert a pre-resolved function call to the right function */

  old_where = expr->where;
  new = g95_get_expr();
  *new = *expr;

  new = g95_build_funcall(NULL, new, NULL);
  new->value.function.name = sym->lib_name;
  new->value.function.isym = sym;
  new->where = old_where;

  *expr = *new;

  g95_free(new);
  expr->ts = *ts;

  if (g95_is_constant_expr(expr->value.function.actual->expr) &&
      do_simplify(sym, expr) == FAILURE) {

    if (eflag == 2) goto bad;
    return FAILURE;   /* Error already generated in do_simplify() */
  }

  return SUCCESS;  

bad:
  if (eflag == 1) {
    g95_error("Can't convert %s to %s at %L",
	      g95_typename(&expr->ts), g95_typename(ts), &expr->where);
    return FAILURE;
  }

  g95_internal_error("Can't convert %s to %s at %L",
		     g95_typename(&expr->ts), g95_typename(ts), &expr->where);

  return FAILURE;
}

