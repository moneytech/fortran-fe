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
#include "simplify.h"


static char *lib_name;                /* Override a library name */

extern g95_integer_info g95_integer_kinds[];
extern g95_real_info g95_real_kinds[];
extern g95_expr g95_bad_expr;

/* If a validation of an intrinsic symbol/interface fails for some
 * reason, the text of the reason is here. */

char g95_intrinsic_diagnostic[120];

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
  int elemental;

  g95_expr *(*simplify)();
  try (*check_function)();
  struct intrinsic_sym *specific, *next;

} intrinsic_sym;


static intrinsic_sym *functions, *subroutines, *conversion, *next_sym;
static intrinsic_arg *next_arg;

static int nfunc, nsub, nargs, nconv, sizing;


/* intrinsic_error()-- write an error message into static memory in
 * case a caller is interested in why something failed. */

static void intrinsic_error(char *format, ...) {
va_list argp;

  va_start(argp, format);
  vsprintf(g95_intrinsic_diagnostic, format, argp);
  va_end(argp);
}


/* type_letter()-- Return a letter based on the passed type.  Used to
 * construct the name of a type conversion subroutine. */

static char type_letter(bt type) {
char c;

  switch(type) {
  case BT_LOGICAL:    c = 'l';  break;
  case BT_CHARACTER:  c = 'c';  break;
  case BT_INTEGER:    c = 'i';  break;
  case BT_REAL:       c = 'r';  break;
  case BT_COMPLEX:    c = 'z';  break;

  case BT_DERIVED:
  case BT_UNKNOWN:
  default:            c = 'u';  break;
  }

  return c;
}


/* conv_name()-- Return a pointer to the name of a conversion function
 * given two typespecs. */

static char *conv_name(g95_typespec *from, g95_typespec *to) {
static char name[30];

  sprintf(name, "__convert_%c%d_%c%d", type_letter(from->type), from->kind,
	  type_letter(to->type), to->kind);

  return name;
}


/* find_conv()-- Given a pair of typespecs, find the intrinsic_sym
 * node that corresponds to the conversion.  Returns NULL if the
 * conversion isn't found. */

intrinsic_sym *find_conv(g95_typespec *from, g95_typespec *to) {
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
 * requires the destination type and kind information with is supplied
 * by a special case in do_simplify(). */

static g95_expr *convert_constant(g95_expr *e, bt type, int kind) {
g95_expr *result;

  if (e->expr_type != EXPR_CONSTANT) return NULL;

  switch(e->ts.type) {
  case BT_INTEGER:
    switch(type) {
    case BT_INTEGER:  result = g95_int2int(e, kind);          break;
    case BT_REAL:     result = g95_int2real(e, kind);         break;
    case BT_COMPLEX:  result = g95_int2complex(e, kind);      break;
    default: goto oops;
    }
    break;

  case BT_REAL:
    switch(type) {
    case BT_INTEGER:  result = g95_real2int(e, kind);         break;
    case BT_REAL:     result = g95_real2real(e, kind);        break;
    case BT_COMPLEX:  result = g95_real2complex(e, kind);     break;
    default: goto oops;
    }
    break;

  case BT_COMPLEX:
    switch(type) {
    case BT_INTEGER:  result = g95_complex2int(e, kind);      break;
    case BT_REAL:     result = g95_complex2real(e, kind);     break;
    case BT_COMPLEX:  result = g95_complex2complex(e, kind);  break;

    default: goto oops;
    }
    break;

  default: oops:
    g95_internal_error("convert_constant(): Unexpected type");
  }

  if (result == NULL) result = &g95_bad_expr;
  return result;
}



/***** Check functions *****/

static try check_bit_size(g95_expr *x) {

  if (x->ts.type != BT_INTEGER) return FAILURE;

  return SUCCESS;
}


static try check_cmplx(g95_expr *x, g95_expr *y, g95_expr *kind) {

  if (!g95_numeric_ts(&x->ts)) return FAILURE;

  if (y != NULL) {
    if (!g95_numeric_ts(&y->ts)) return FAILURE;
    if (x->ts.type == BT_COMPLEX) return FAILURE;
  }

  if (kind != NULL &&
      (kind->ts.type != BT_INTEGER || kind->expr_type != EXPR_CONSTANT))
    return FAILURE;

  return SUCCESS;
}


static try check_digits(g95_expr *x) {

  if (!g95_numeric_ts(&x->ts)) return FAILURE;

  return SUCCESS;
}


static try check_dim(g95_expr *x, g95_expr *y) {

  if ((x->ts.type != BT_INTEGER && x->ts.type != BT_REAL) ||
      (y->ts.type != BT_INTEGER && y->ts.type != BT_REAL) ||
      x->ts.type != y->ts.type || x->ts.kind != y->ts.kind) return FAILURE;

  return SUCCESS;
}


static try check_epsilon(g95_expr *x) {

  if (x->ts.type != BT_REAL) return FAILURE;

  return SUCCESS;
}


static try check_float(g95_expr *a, g95_expr *kind) {

  if (a->ts.type != BT_INTEGER || a->ts.kind != g95_default_integer_kind())
    return FAILURE;

  if (kind != NULL &&
      (kind->ts.type != BT_INTEGER || kind->expr_type != EXPR_CONSTANT))
    return FAILURE;

  return SUCCESS;
}


static try check_huge(g95_expr *x) {

  if (x->ts.type != BT_INTEGER && x->ts.type != BT_REAL) return FAILURE;

  return SUCCESS;
}


static try check_int(g95_expr *x, g95_expr *kind) {

  if (!g95_numeric_ts(&x->ts)) return FAILURE;

  if (kind != NULL &&
      (kind->ts.type != BT_INTEGER || kind->expr_type != EXPR_CONSTANT))
    return FAILURE;

  return SUCCESS;
}


static try check_kind(g95_expr *x) {

  if (x->ts.type == BT_DERIVED) return FAILURE;

  return SUCCESS;
}


static try check_min_max(g95_actual_arglist *arg) {
g95_expr *x, *y;

  x=arg->expr;
  arg = arg->next;

  if ( arg == NULL ) {
    g95_error("Too few arguments to intrinsic at %L",x->where);
    return FAILURE;
  }

  while ( arg != NULL ) {
    y=arg->expr;
    if ((x->ts.type != BT_INTEGER && x->ts.type != BT_REAL) ||
         x->ts.type != y->ts.type || x->ts.kind != y->ts.kind) return FAILURE;
    x=y;
    arg=arg->next;
  }

  return SUCCESS;
}


static try check_min0_max0(g95_actual_arglist *arg) {
g95_expr *x, *y;

  x=arg->expr;
  arg = arg->next;

  if ( arg == NULL ) {
    g95_error("Too few arguments to intrinsic at %L",x->where);
    return FAILURE;
  }

  while ( arg != NULL ) {
    y=arg->expr;
    if (x->ts.type != BT_INTEGER  ||
        x->ts.type != y->ts.type  || x->ts.kind != y->ts.kind) return FAILURE;
    x=y;
    arg=arg->next;
  }

  return SUCCESS;
}


static try check_min1_max1(g95_actual_arglist *arg) {
g95_expr *x, *y;

  x=arg->expr;
  arg = arg->next;

  if ( arg == NULL ) {
    g95_error("Too few arguments to intrinsic at %L",x->where);
    return FAILURE;
  }

  while ( arg != NULL ) {
    y=arg->expr;
    if (x->ts.type != BT_REAL ||
        x->ts.type != y->ts.type || x->ts.kind != y->ts.kind) return FAILURE;
    x=y;
    arg=arg->next;
  }

  return SUCCESS;
}


static try check_min_max_exponent(g95_expr *x) {

  if (x->ts.type != BT_REAL) return FAILURE;

  return SUCCESS;
}


static try check_modulo(g95_expr *a, g95_expr *p) {

  if ((a->ts.type != BT_INTEGER && a->ts.type != BT_REAL) ||
      a->ts.type != p->ts.type || a->ts.kind != p->ts.kind) return FAILURE;

  return SUCCESS;
}


static try check_precision(g95_expr *x) {

  if (x->ts.type != BT_REAL && x->ts.type != BT_COMPLEX) return FAILURE;

  return SUCCESS;
}


static try check_radix(g95_expr *x) {

  if (x->ts.type != BT_INTEGER && x->ts.type != BT_REAL) return FAILURE;

  return SUCCESS;
}


static try check_range(g95_expr *x) {

  if (!g95_numeric_ts(&x->ts)) return FAILURE;

  return SUCCESS;
}


static try check_real(g95_expr *a, g95_expr *kind) {

  if (!g95_numeric_ts(&a->ts)) return FAILURE;

  if (kind->ts.type != BT_INTEGER || kind->expr_type != EXPR_CONSTANT)
    return FAILURE;

  return SUCCESS;
}


static try check_selected_real_kind(g95_expr *p, g95_expr *r) {

  if (p == NULL && r == NULL) return FAILURE;

  return SUCCESS;
}


static try check_sngl(g95_expr *a, g95_expr *kind) {

  if (a->ts.type != BT_REAL || a->ts.kind != g95_default_double_kind())
    return FAILURE;

  if (kind->ts.type != BT_INTEGER || kind->expr_type != EXPR_CONSTANT)
    return FAILURE;

  return SUCCESS;
}


static try check_tiny(g95_expr *x) {

  if (x->ts.type != BT_REAL) return FAILURE;

  return SUCCESS;
}



/* do_check()-- Interface to the check functions.  We break apart an
 * argument list and call the proper check function rather than
 * forcing each function to manipulate the argument list */

static try do_check(intrinsic_sym *specific, g95_actual_arglist *arg) {
g95_expr *a1, *a2, *a3, *a4;
try t;

/* Max and min require special handling due to the variable number of args */
  if ( specific->check_function == check_min_max    ||
       specific->check_function == check_min0_max0  ||
       specific->check_function == check_min1_max1 ) {
    t = (*specific->check_function)(arg);
    return t;
  }

  a1 = arg->expr;
  arg = arg->next;

  if (arg == NULL) 
    t = (*specific->check_function)(a1);
  else {
    a2 = arg->expr;
    arg = arg->next;

    if (arg == NULL)
      t = (*specific->check_function)(a1, a2);
    else {
      a3 = arg->expr;
      arg = arg->next;
      
      if (arg == NULL)
	t = (*specific->check_function)(a1, a2, a3);
      else {
	a4 = arg->expr;
	arg = arg->next;

	if (arg == NULL)
	  t = (*specific->check_function)(a1, a2, a3, a4);
	else {
	  g95_internal_error("do_check(): Too many args");
	}
      }
    }
  }

  return t;
}


/*********** Subroutines to build the intrinsic list ****************/

/* add_sym()-- Add a single intrinsic symbol to the current list. 
 * Argument list:
 *    char *    Name of function
 *    int       Whether function is elemental (1=Non-elemental, 0=elemental)
 *    bt        Return type of function
 *    int       Kind of return type of function
 *    simplify
 *    cfunction
 * optional arguments come in multiples of four:
 *    char *    Name of argument
 *    bt        Type of argument
 *    int       Kind of argument
 *    int       Arg optional flag (1=Optional, 0=Required)
 *
 * The sequence is terminated by a NULL name. */

static void add_sym(char *name, int elemental, bt type, int kind,
		    g95_expr *(*simplify)(),
		    try (*cfunction)(), ...) {
int optional, first_flag;
va_list argp;

  if (sizing) {
    if (type == BT_UNKNOWN)
      nsub++;
    else
      nfunc++;
  } else {
    strcpy(next_sym->name, name);

    strcpy(next_sym->lib_name, "__");
    strcat(next_sym->lib_name, name);

    next_sym->elemental = elemental;
    next_sym->ts.type = type;
    next_sym->ts.kind = kind;
    next_sym->simplify = simplify;
    next_sym->check_function = cfunction;
  }

  va_start(argp, cfunction);

  first_flag = 1;

  for(;;) {
    name = va_arg(argp, char *);
    if (name == NULL) break;

    type = va_arg(argp, bt);
    kind = va_arg(argp, int);
    optional = va_arg(argp, int);

    if (sizing)
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

  next_sym++;
  va_end(argp);  
}


/* find_sym()-- Locate an intrinsic symbol given a base pointer,
 * number of elements in the table and a pointer to a name.  Returns
 * the NULL pointer if a name is not found. */

static intrinsic_sym *find_sym(intrinsic_sym *start, int n, char *name) {

  while(n > 0) {
    if (strcmp(name, start->name) == 0) return start;

    start++;
    n--;
  }

  return NULL;
}


/* find_function()-- Given a name, find a function in the intrinsic
 * function table.  Returns NULL if not found. */

static intrinsic_sym *find_function(char *name) {

  return find_sym(functions, nfunc, name);
}


/* find_subroutine()-- Given a name, find a function in the intrinsic
 * subroutine table.  Returns NULL if not found. */

static intrinsic_sym *find_subroutine(char *name) {

  return find_sym(subroutines, nsub, name);
}


/* make_generic()-- Collect a set of intrinsic functions into a
 * generic collection.  The first argument is the name of the generic
 * function, which is also the name of a specific function.  The rest
 * of the specifics currently in the table are placed into the list of
 * specific functions associated with that generic.  */

static void make_generic(char *name) {
intrinsic_sym *generic;

  if (sizing) return; 

  generic = find_function(name);
  if (generic == NULL)
    g95_internal_error("make_generic(): Can't find generic symbol '%s'", name);

  generic->specific = generic + 1;
  generic++;
  
  while(generic->name[0] != '\0') {
    generic->next = generic + 1;
    generic++;
  }

  generic--;
  generic->next = NULL;
}


/******* Check functions ********/

/* These functions check to see if an argument list is compatible with
 * a particular intrinsic function or subroutine.  Presence of
 * required arguments has already been established, and the argument
 * list has NULL arguments in the correct places.  */

static try not_ready(g95_actual_arglist *dummy) {

  g95_warning("Intrinsic checker not ready");
  return FAILURE;
} 


/* add_functions()-- Add intrinsic functions */

static void add_functions(void) {

/* Argument names as in the standard (to be used as argument keywords) */
char   a[] = "a",  f[] = "field",     pt[] = "pointer",   tg[] = "target",
       b[] = "b",  m[] = "matrix",    ma[] = "matrix_a",  mb[] = "matrix_b",
       c[] = "c",  n[] = "ncopies",  pos[] = "pos",      bck[] = "back",
       i[] = "i",  v[] = "vector",    va[] = "vector_a",  vb[] = "vector_b",
       j[] = "j", a1[] = "a1",        fs[] = "fsource",   ts[] = "tsource",
       l[] = "l", a2[] = "a2",        mo[] = "mold",     ord[] = "order",
       p[] = "p", ar[] = "array",    shp[] = "shape",    src[] = "source",
       r[] = "r", bd[] = "boundary", pad[] = "pad",      set[] = "set",
       s[] = "s", dm[] = "dim",      knd[] = "kind",     msk[] = "mask",
       x[] = "x", sh[] = "shift",    stg[] = "string",   ssg[] = "substring",
       y[] = "y", sz[] = "size",     sta[] = "string_a", stb[] = "string_b",
       z[] = "z", ln[] = "len";

int di, dr, dd, dl, dc, dz;

  di = g95_default_integer_kind();
  dr = g95_default_real_kind();
  dd = g95_default_double_kind();
  dl = g95_default_logical_kind();
  dc = g95_default_character_kind();
  dz = g95_default_complex_kind();

  add_sym("abs",  0, BT_REAL,    dr, g95_simplify_rabs, NULL,
	  a, BT_REAL, dr, 0, NULL);
  add_sym("iabs", 0, BT_INTEGER, di, g95_simplify_iabs, NULL,
	  a, BT_INTEGER, di, 0, NULL);
  add_sym("dabs", 0, BT_REAL,    dd, g95_simplify_rabs, NULL,
	  a, BT_REAL, dd, 0, NULL);
  add_sym("cabs", 0, BT_REAL,    dr, g95_simplify_cabs, NULL,
	  a, BT_COMPLEX, dz, 0, NULL);
  make_generic("abs");

  add_sym("achar", 0, BT_CHARACTER, dc, g95_simplify_achar, NULL,
	  i, BT_INTEGER, di, 0, NULL);
  
  add_sym("acos",  0, BT_REAL, dr, g95_simplify_acos, NULL,
	  x, BT_REAL, dr, 0, NULL);
  add_sym("dacos", 0, BT_REAL, dd, g95_simplify_acos, NULL,
	  x, BT_REAL, dd, 0, NULL);
  make_generic("acos");

  add_sym("adjustl", 0, BT_CHARACTER, dc, g95_simplify_adjustl, NULL,
	  stg, BT_CHARACTER, dc, 0, NULL);

  add_sym("adjustr", 0, BT_CHARACTER, dc, g95_simplify_adjustr, NULL,
	  stg, BT_CHARACTER, dc, 0, NULL);

  add_sym("aimag", 0, BT_REAL, dr, g95_simplify_aimag, NULL,
	  z, BT_COMPLEX, dz, 0, NULL);

  add_sym("aint", 0, BT_REAL, dr, g95_simplify_aint, NULL,
	  a, BT_REAL, dr, 0,   knd, BT_INTEGER, di, 1, NULL);
  add_sym("dint", 0, BT_REAL, dd, g95_simplify_aint, NULL, a,
	  BT_REAL, dd, 0, NULL);
  make_generic("aint");

/* KAH Takes logical array input */
  add_sym("all", 1, BT_LOGICAL, dl, NULL, not_ready,
	  msk, BT_LOGICAL, dl, 0, dm, BT_INTEGER, di, 1, NULL);

/* KAH Takes an allocatable array argument */
  add_sym("allocated", 1, BT_LOGICAL, dl, NULL, not_ready,
	  ar, BT_REAL, dr, 0, NULL);

  add_sym("anint", 0, BT_REAL, dr, g95_simplify_anint, NULL,
	  a, BT_REAL, dr, 0,  knd, BT_INTEGER, di, 1, NULL);
  add_sym("dnint", 0, BT_REAL, dd, g95_simplify_anint, NULL,
	  a, BT_REAL, dd, 0, NULL);
  make_generic("anint");

/* KAH Takes logical array input */
  add_sym("any", 1, BT_LOGICAL, dl, NULL, not_ready,
	  msk, BT_LOGICAL, dl, 0, dm, BT_INTEGER, di, 1, NULL);

  add_sym("asin",  0, BT_REAL, dr, g95_simplify_asin, NULL,
	  x, BT_REAL, dr, 0, NULL);
  add_sym("dasin", 0, BT_REAL, dd, g95_simplify_asin, NULL,
	  x, BT_REAL, dd, 0, NULL);
  make_generic("asin");

/* KAH Takes pointer and target types-- BT_INTEGER used as a placeholder */
  add_sym("associated", 1, BT_LOGICAL, dl, NULL, not_ready,
	  pt, BT_INTEGER, di, 0, tg, BT_INTEGER, di, 1, NULL);

  add_sym("atan",  0, BT_REAL, dr, NULL, NULL, x, BT_REAL, dr, 0, NULL);
  add_sym("datan", 0, BT_REAL, dd, NULL, NULL, x, BT_REAL, dd, 0, NULL);
  make_generic("atan");

  add_sym("atan2",  0, BT_REAL, dr, g95_simplify_atan2, NULL,
	  y, BT_REAL, dr, 0, x, BT_REAL, dr, 0, NULL);
  add_sym("datan2", 0, BT_REAL, dd, g95_simplify_atan2, NULL,
	  y, BT_REAL, dd, 0, x, BT_REAL, dd, 0, NULL);
  make_generic("atan2");

  add_sym("bit_size", 1, BT_INTEGER, di, g95_simplify_bit_size, check_bit_size,
	  i, BT_INTEGER, di, 0, NULL);

  add_sym("btest", 0, BT_LOGICAL, dl, g95_simplify_btest, NULL,
	  i, BT_INTEGER, di, 0, pos, BT_INTEGER, di, 0, NULL);

  add_sym("ceiling", 0, BT_INTEGER, di, g95_simplify_ceiling, NULL,
	  a, BT_REAL, dr, 0,   knd, BT_INTEGER, di, 1, NULL);

  add_sym("char", 0, BT_CHARACTER, dc, g95_simplify_char, NULL,
	  i, BT_INTEGER, di, 0,   knd, BT_INTEGER, di, 1, NULL);

  add_sym("cmplx", 0, BT_COMPLEX, dz, g95_simplify_cmplx, check_cmplx,
	  x, BT_UNKNOWN, dr, 0, y, BT_UNKNOWN, dr, 1, knd, BT_INTEGER, 
          di, 1, NULL);

  add_sym("conjg", 0, BT_COMPLEX, dz, g95_simplify_conjg, NULL,
	  z, BT_COMPLEX, dz, 0, NULL);

  add_sym("cos",  0, BT_REAL,    dr, NULL, not_ready,
	  x, BT_REAL,    dr, 0, NULL);
  add_sym("dcos", 0, BT_REAL,    dd, NULL, not_ready,
	  x, BT_REAL,    dd, 0, NULL);
  add_sym("ccos", 0, BT_COMPLEX, dz, NULL, not_ready,
	  x, BT_COMPLEX, dz, 0, NULL);
  make_generic("cos");

  add_sym("cosh",  0, BT_REAL, dr, NULL, NULL, x, BT_REAL, dr, 0, NULL);
  add_sym("dcosh", 0, BT_REAL, dd, NULL, NULL, x, BT_REAL, dd, 0, NULL);
  make_generic("cosh");

/* KAH Takes logical array input */
  add_sym("count", 1, BT_INTEGER, di, NULL, not_ready,
	  msk, BT_LOGICAL, dl, 0, dm, BT_INTEGER, di, 1, NULL);

/* KAH Takes an array argument of any type and returns an array */
  add_sym("cshift", 1, BT_REAL, dr, NULL, not_ready, ar, BT_REAL, dr, 0,
	  sh, BT_INTEGER, di, 0, dm, BT_INTEGER, di, 1, NULL);

  add_sym("dble", 0, BT_REAL, dd, g95_simplify_dble, not_ready,
	  a, BT_REAL, dr, 0, NULL);

  add_sym("digits", 1, BT_INTEGER, di, g95_simplify_digits, check_digits,
	  x, BT_UNKNOWN, dr, 0, NULL);

  add_sym("dim",  0, BT_REAL,    dr, g95_simplify_dim, check_dim,
	  x, BT_UNKNOWN,    dr, 0, y, BT_UNKNOWN,    dr, 0, NULL);
  add_sym("idim", 0, BT_INTEGER, di, g95_simplify_dim, NULL,
	  x, BT_INTEGER, di, 0, y, BT_INTEGER, di, 0, NULL);
  add_sym("ddim", 0, BT_REAL,    dd, g95_simplify_dim, NULL,
	  x, BT_REAL,    dd, 0, y, BT_REAL,    dd, 0, NULL);
  make_generic("dim");

/* KAH Takes vector input, returns scalar.  Vectors can be integer,
 * real, complex, or logical */
  add_sym("dot_product", 1, BT_REAL, dr, NULL, not_ready,
	  va, BT_REAL, dr, 0, vb, BT_REAL, dr, 0, NULL);

  add_sym("dprod", 0, BT_REAL, dd, g95_simplify_dprod, NULL,
	  x, BT_REAL, dr, 0, y, BT_REAL, dr, 0, NULL);

/* KAH Takes array argument */
  add_sym("eoshift", 1, BT_REAL, dr, NULL, not_ready,
	  ar, BT_REAL, dr, 0, sh, BT_INTEGER, di, 0,
	  bd, BT_REAL, dr, 1, dm, BT_INTEGER, di, 1, NULL);

  add_sym("epsilon", 1, BT_REAL, dr, g95_simplify_epsilon, check_epsilon,
	  x, BT_UNKNOWN, dr, 0, NULL);

  add_sym("exp",  0, BT_REAL,    dr, g95_simplify_exp, NULL,
	  x, BT_REAL, dr, 0, NULL);

  add_sym("dexp", 0, BT_REAL,    dd, NULL, NULL, x, BT_REAL,    dd, 0, NULL);

  add_sym("cexp", 0, BT_COMPLEX, dz, NULL, NULL, x, BT_COMPLEX, dz, 0, NULL);

  make_generic("exp");

  add_sym("exponent", 0, BT_INTEGER, di, g95_simplify_exponent, NULL,
	  x, BT_REAL, dr, 0, NULL);

  add_sym("floor", 0, BT_INTEGER, di, g95_simplify_floor, NULL,
	  a, BT_REAL, dr, 0, knd, BT_INTEGER, di, 1, NULL);

  add_sym("fraction", 0, BT_REAL, dr, g95_simplify_fraction, NULL,
	  x, BT_REAL, dr, 0, NULL);

  add_sym("huge", 1, BT_REAL, dr, g95_simplify_huge, check_huge,
	  x, BT_UNKNOWN, dr, 0,  NULL);

  add_sym("iachar", 0, BT_INTEGER, di, g95_simplify_iachar, NULL,
	  c, BT_CHARACTER, dc, 0, NULL);

  add_sym("iand", 0, BT_INTEGER, di, g95_simplify_iand, NULL,
	  i, BT_INTEGER, di, 0, j, BT_INTEGER, di, 0, NULL);

  add_sym("ibclr", 0, BT_INTEGER, di, g95_simplify_ibclr, NULL,
	  i, BT_INTEGER, di, 0, pos, BT_INTEGER, di, 0, NULL);

  add_sym("ibits", 0, BT_INTEGER, di, g95_simplify_ibits, NULL,
	  i, BT_INTEGER, di, 0,   pos, BT_INTEGER, di, 0,
	  ln, BT_INTEGER, di, 0, NULL);

  add_sym("ibset", 0, BT_INTEGER, di, g95_simplify_ibset, NULL,
	  i, BT_INTEGER, di, 0, pos, BT_INTEGER, di, 0, NULL);

  add_sym("ichar", 0, BT_INTEGER, di, g95_simplify_ichar, NULL,
	  c, BT_CHARACTER, dc, 0, NULL);

  add_sym("ieor", 0, BT_INTEGER, di, g95_simplify_ieor, NULL,
	  i, BT_INTEGER, di, 0, j, BT_INTEGER, di, 0, NULL);

  add_sym("index", 0, BT_INTEGER, di, g95_simplify_index, NULL,
	  stg, BT_CHARACTER, dc, 0,   ssg, BT_CHARACTER, dc, 0,
	  bck, BT_LOGICAL, dl, 1, NULL);

  add_sym("int",   0, BT_INTEGER, di, g95_simplify_int, check_int,
	  a, BT_REAL, dr, 0, knd, BT_INTEGER, di, 1, NULL);
  add_sym("ifix",  0, BT_INTEGER, di, g95_simplify_int, NULL,
	  a, BT_REAL, dr, 0, NULL);
  add_sym("idint", 0, BT_INTEGER, di, g95_simplify_int, NULL,
	  a, BT_REAL, dd, 0, NULL);
  make_generic("int");

  add_sym("ior", 0, BT_INTEGER, di, g95_simplify_ior, NULL,
	  i, BT_INTEGER, di, 0, j, BT_INTEGER, di, 0, NULL);

  add_sym("ishft", 0, BT_INTEGER, di, g95_simplify_ishft, NULL,
	  i, BT_INTEGER, di, 0, sh, BT_INTEGER, di, 0, NULL);

  add_sym("ishftc", 0, BT_INTEGER, di, g95_simplify_ishftc, NULL,
	  i, BT_INTEGER, di, 0,  sh, BT_INTEGER, di, 0,
	  sz, BT_INTEGER, di, 1, NULL);

  add_sym("kind", 1, BT_INTEGER, di, g95_simplify_kind, check_kind,
	  x, BT_REAL, dr, 0, NULL);

/* KAH Array input, output can be an array */
  add_sym("lbound", 1, BT_INTEGER, di, NULL, not_ready,
	  ar, BT_REAL, dr, 0, dm, BT_INTEGER, di, 1, NULL);

  add_sym("len", 1, BT_INTEGER, di, g95_simplify_len, NULL,
	  stg, BT_CHARACTER, dc, 0, NULL);

  add_sym("len_trim", 0, BT_INTEGER, di, g95_simplify_len_trim, NULL,
	  stg, BT_CHARACTER, dc, 0, NULL);

  add_sym("lge", 0, BT_LOGICAL, dl, g95_simplify_lge, NULL,
	  sta, BT_CHARACTER, dc, 0, stb, BT_CHARACTER, dc, 0, NULL);

  add_sym("lgt", 0, BT_LOGICAL, dl, g95_simplify_lgt, NULL,
	  sta, BT_CHARACTER, dc, 0, stb, BT_CHARACTER, dc, 0, NULL);

  add_sym("lle", 0, BT_LOGICAL, dl, g95_simplify_lle, NULL,
	  sta, BT_CHARACTER, dc, 0, stb, BT_CHARACTER, dc, 0, NULL);

  add_sym("llt", 0, BT_LOGICAL, dl, g95_simplify_llt, NULL,
	  sta, BT_CHARACTER, dc, 0, stb, BT_CHARACTER, dc, 0, NULL);

  add_sym("log",  0, BT_REAL,    dr, g95_simplify_log, not_ready,
	  x, BT_REAL,    dr, 0, NULL);
  add_sym("alog", 0, BT_REAL,    dr, g95_simplify_log, not_ready,
	  x, BT_REAL,    dr, 0, NULL);
  add_sym("dlog", 0, BT_REAL,    dd, g95_simplify_log, not_ready,
	  x, BT_REAL,    dd, 0, NULL);
  add_sym("clog", 0, BT_COMPLEX, dz, g95_simplify_log, not_ready,
	  x, BT_COMPLEX, dz, 0, NULL);
  make_generic("log");

  add_sym("log10",  0, BT_REAL, dr, g95_simplify_log10, not_ready,
	  x, BT_REAL, dr, 0, NULL);
  add_sym("alog10", 0, BT_REAL, dr, g95_simplify_log10, not_ready,
	  x, BT_REAL, dr, 0, NULL);
  add_sym("dlog10", 0, BT_REAL, dd, g95_simplify_log10, not_ready,
	  x, BT_REAL, dd, 0, NULL);
  make_generic("log10");

  add_sym("logical", 0, BT_LOGICAL, dl, g95_simplify_logical, NULL,
	  l, BT_LOGICAL, dl, 0,	  knd, BT_INTEGER, di, 1, NULL);

/* KAH Takes and returns arrays of any numeric or logical type */
  add_sym("matmul", 1, BT_REAL, dr, NULL, not_ready,
	  ma, BT_REAL, dr, 0, mb, BT_REAL, dr, 0, NULL);

/* Note: amax0 is equivalent to real(max), max1 is equivalent to int(max) 
 * Max function must take at least two arguments                        */
  add_sym("max",   0, BT_REAL,    dr, g95_simplify_max, check_min_max,
	  a1, BT_UNKNOWN,    dr, 0, a2, BT_UNKNOWN,    dr, 0, NULL);
  add_sym("max0",  0, BT_INTEGER, di, g95_simplify_max, check_min0_max0,
	  a1, BT_INTEGER, di, 0, a2, BT_INTEGER, di, 0, NULL);
  add_sym("amax1", 0, BT_REAL,    dr, g95_simplify_max, check_min1_max1,
	  a1, BT_REAL,    dr, 0, a2, BT_REAL,    dr, 0, NULL);
  add_sym("dmax1", 0, BT_REAL,    dd, g95_simplify_max, check_min1_max1,
	  a1, BT_REAL,    dd, 0, a2, BT_REAL,    dd, 0, NULL);
  add_sym("amax0", 0, BT_REAL,    dr, g95_simplify_amax0, check_min0_max0,
	  a1, BT_INTEGER, di, 0, a2, BT_INTEGER, di, 0, NULL);
  add_sym("max1",  0, BT_INTEGER, di, g95_simplify_max1, check_min1_max1,
	  a1, BT_REAL,    dr, 0, a2, BT_REAL,    dr, 0, NULL);
  make_generic("max");

  add_sym("maxexponent", 1, BT_INTEGER, di, g95_simplify_maxexponent,
	  check_min_max_exponent, x, BT_UNKNOWN, dr, 0, NULL);

/* KAH Takes array argument of type integer or real.  The type of the
 * second argument must be checked to decide if it's dm or msk if called
 * with two arguments. */
  add_sym("maxloc", 1, BT_INTEGER, di, NULL, not_ready, ar, BT_REAL, dr, 0,
	  dm, BT_INTEGER, di, 1, msk, BT_LOGICAL, dl, 1, NULL);

/* KAH Takes array argument of type integer or real, returns same type.
 * The type of the second argument must be checked to decide if it's dm
 * or msk if called with two arguments. */
  add_sym("maxval", 1, BT_REAL, dr, NULL, not_ready, ar, BT_REAL, dr, 0,
	  dm, BT_INTEGER, di, 1, msk, BT_LOGICAL, dl, 1, NULL);

/* KAH Takes any type for ts and fs */
  add_sym("merge", 0, BT_REAL, dr, NULL, not_ready, ts, BT_REAL, dr, 0,
	  fs, BT_REAL, dr, 0, msk, BT_LOGICAL, dl, 0, NULL);

/* Note: amin0 is equivalent to real(min), min1 is equivalent to int(min) */
  add_sym("min",   0, BT_REAL,    dr, g95_simplify_min, check_min_max,
	  a1, BT_REAL,    dr, 0, a2, BT_REAL,    dr, 0, NULL);
  add_sym("min0",  0, BT_INTEGER, di, g95_simplify_min, check_min0_max0,
	  a1, BT_INTEGER, di, 0, a2, BT_INTEGER, di, 0, NULL);
  add_sym("amin1", 0, BT_REAL,    dr, g95_simplify_min, check_min1_max1,
	  a1, BT_REAL,    dr, 0, a2, BT_REAL,    dr, 0, NULL);
  add_sym("dmin1", 0, BT_REAL,    dd, g95_simplify_min, check_min1_max1,
	  a1, BT_REAL,    dd, 0, a2, BT_REAL,    dd, 0, NULL);
  add_sym("amin0", 0, BT_REAL,    dr, g95_simplify_amin0, check_min0_max0,
	  a1, BT_INTEGER, di, 0, a2, BT_INTEGER, di, 0, NULL);
  add_sym("min1",  0, BT_INTEGER, di, g95_simplify_min1, check_min1_max1,
	  a1, BT_REAL,    dr, 0, a2, BT_REAL,    dr, 0, NULL);
  make_generic("min");

  add_sym("minexponent", 1, BT_INTEGER, di, g95_simplify_minexponent,
	  check_min_max_exponent, x, BT_UNKNOWN, dr, 0, NULL);

/* KAH Takes array argument of type integer or real.  The type of the
 * second argument must be checked to decide if it's dm or msk if called
 * with two arguments. */
  add_sym("minloc", 1, BT_INTEGER, di, NULL, not_ready, ar, BT_REAL, dr, 0,
	  dm, BT_INTEGER, di, 1, msk, BT_LOGICAL, dl, 1, NULL);

/* KAH Takes array argument of type integer or real, returns same type.
 * The type of the second argument must be checked to decide if it's dm
 * or msk if called with two arguments. */
  add_sym("minval", 1, BT_REAL, dr, NULL, not_ready, ar, BT_REAL, dr, 0,
	  dm, BT_INTEGER, di, 1, msk, BT_LOGICAL, dl, 1, NULL);

  add_sym("mod",  0, BT_INTEGER, di, g95_simplify_mod, NULL,
	  a, BT_INTEGER, di, 0, p, BT_INTEGER, di, 0, NULL);
  add_sym("amod", 0, BT_REAL,    dr, g95_simplify_mod, NULL,
	  a, BT_REAL,    dr, 0, p, BT_REAL,    dr, 0, NULL);
  add_sym("dmod", 0, BT_REAL,    dd, g95_simplify_mod, NULL,
	  a, BT_REAL,    dd, 0, p, BT_REAL,    dd, 0, NULL);
  make_generic("mod");

  add_sym("modulo", 0, BT_REAL, di, g95_simplify_modulo, check_modulo,
	  a, BT_REAL, di, 0, p, BT_REAL, di, 0, NULL);

  add_sym("nearest", 0, BT_REAL, dr, g95_simplify_nearest, NULL,
	  x, BT_REAL, dr, 0, s, BT_REAL, dr, 0, NULL);

  add_sym("nint",   0, BT_INTEGER, di, g95_simplify_nint, NULL,
	  a, BT_REAL, dr, 0,   knd, BT_INTEGER, di, 1, NULL);
  add_sym("idnint", 0, BT_INTEGER, di, g95_simplify_nint, NULL,
	  a, BT_REAL, dd, 0, NULL);
  make_generic("nint");

  add_sym("not", 0, BT_INTEGER, di, g95_simplify_not, NULL,
	  i, BT_INTEGER, di, 0, NULL);

/* KAH Takes and returns pointers-- using BT_INTEGER as a placeholder */
  add_sym("null", 1, BT_INTEGER, di, NULL, not_ready,
	  mo, BT_INTEGER, di, 1, NULL);

/* KAH Takes arrays and an optional vector and returns a vector */
  add_sym("pack", 1, BT_REAL, dr, NULL, not_ready, ar, BT_REAL, dr, 0,
	  msk, BT_LOGICAL, dl, 0, v, BT_REAL, dr, 1, NULL);

  add_sym("precision",1, BT_INTEGER, di, g95_simplify_precision,
	  check_precision,  x, BT_UNKNOWN, 0, 0, NULL);

/* KAH Takes any type, including pointer */
  add_sym("present", 1, BT_LOGICAL, dl, NULL, not_ready,
	  a, BT_REAL, dr, 0, NULL);

  add_sym("product", 1, BT_REAL, dr, NULL, not_ready, ar, BT_REAL, dr, 0,
	  dm, BT_INTEGER, di, 1, msk, BT_LOGICAL, dl, 1, NULL);

  add_sym("radix", 1, BT_INTEGER, di, g95_simplify_radix, check_radix,
	  x, BT_UNKNOWN, 0, 0, NULL);

  add_sym("range", 1, BT_INTEGER, di, g95_simplify_range, check_range,
	  x, BT_REAL, dr, 0, NULL);

  add_sym("real",  1, BT_REAL, dr, g95_simplify_real, check_real,
	  a, BT_INTEGER, di, 0, knd, BT_INTEGER, di, 1, NULL);

  add_sym("float", 1, BT_REAL, dr, g95_simplify_real, check_float,
	  a, BT_INTEGER, di, 0, knd, BT_INTEGER, di, 1, NULL);

  add_sym("sngl",  1, BT_REAL, dr, g95_simplify_real, check_sngl,
	  a, BT_REAL,    dd, 0, knd, BT_INTEGER, di, 1, NULL);

  add_sym("repeat", 1, BT_CHARACTER, dc, g95_simplify_repeat, NULL,
	  stg, BT_CHARACTER, dc, 0, n, BT_INTEGER, di, 0, NULL);

/* KAH Takes any type array, integer arrays, returns array */
  add_sym("reshape", 1, BT_REAL, dr, NULL, not_ready,
	  src, BT_REAL, dr, 0, shp, BT_INTEGER, di, 0,
	  pad, BT_REAL, dr, 1, ord, BT_INTEGER, di, 1, NULL);

  add_sym("rrspacing",0, BT_REAL, dr, g95_simplify_rrspacing, NULL,
	  x, BT_REAL, dr, 0, NULL);

  add_sym("scale", 0, BT_REAL, dr, g95_simplify_scale, NULL,
	  x, BT_REAL, dr, 0, i, BT_INTEGER, di, 0, NULL);

  add_sym("scan", 0, BT_INTEGER, di, g95_simplify_scan, NULL,
	  stg, BT_CHARACTER, dc, 0,  set, BT_CHARACTER, dc, 0,
	  bck, BT_LOGICAL, dl, 1, NULL);

  add_sym("selected_int_kind", 0, BT_INTEGER, di,
	  g95_simplify_selected_int_kind, NULL,
	  r, BT_INTEGER, di, 0, NULL);

  add_sym("selected_real_kind", 0, BT_INTEGER, di,
	  g95_simplify_selected_real_kind, check_selected_real_kind,
	  p, BT_INTEGER, di, 1, r, BT_INTEGER, di, 1, NULL);

  add_sym("set_exponent", 0, BT_REAL, dr, g95_simplify_set_exponent, NULL,
	  x, BT_REAL, dr, 0, i, BT_INTEGER, di, 0, NULL);

/* KAH Takes array of any type, returns integer array */
  add_sym("shape", 1, BT_INTEGER, di, NULL, not_ready,
	  src, BT_REAL, dr, 0, NULL);

  add_sym("sign",  0, BT_REAL,    dr, g95_simplify_sign, NULL,
	  a, BT_REAL,    dr, 0, b, BT_REAL,    dr, 0, NULL);
  add_sym("isign", 0, BT_INTEGER, di, g95_simplify_sign, NULL,
	  a, BT_INTEGER, di, 0, b, BT_INTEGER, di, 0, NULL);
  add_sym("dsign", 0, BT_REAL,    dd, g95_simplify_sign, NULL,
	  a, BT_REAL,    dd, 0, b, BT_REAL,    dd, 0, NULL);
  make_generic("sign");

  add_sym("sin",  1, BT_REAL,    dr, NULL, NULL,   x, BT_REAL, dr, 0, NULL);
  add_sym("dsin", 1, BT_REAL,    dd, NULL, NULL,   x, BT_REAL, dd, 0, NULL);
  add_sym("csin", 1, BT_COMPLEX, dz, NULL, NULL,   x, BT_COMPLEX, dz, 0, NULL);
  make_generic("sin");

  add_sym("sinh",  1, BT_REAL, dr, NULL, NULL, x, BT_REAL, dr, 0, NULL);
  add_sym("dsinh", 1, BT_REAL, dd, NULL, NULL, x, BT_REAL, dd, 0, NULL);
  make_generic("sinh");

/* KAH Takes array of any type */
  add_sym("size", 1, BT_INTEGER, di, NULL, not_ready,
	  ar, BT_REAL, dr, 0, dm, BT_INTEGER, di, 1, NULL);

  add_sym("spacing", 0, BT_REAL, dr, g95_simplify_spacing, NULL,
	  x, BT_REAL, dr, 0, NULL);

/* KAH Takes array of any type, returns array */
  add_sym("spread", 1, BT_REAL, dr, NULL, not_ready, src, BT_REAL, dr, 0,
	  dm, BT_INTEGER, di, 0, n, BT_INTEGER, di, 0, NULL);

  add_sym("sqrt",  1, BT_REAL,    dr, g95_simplify_sqrt, NULL,
	  x, BT_REAL,    dr, 0, NULL);
  add_sym("dsqrt", 1, BT_REAL,    dd, g95_simplify_sqrt, NULL,
	  x, BT_REAL,    dd, 0, NULL);
  add_sym("csqrt", 1, BT_COMPLEX, dz, NULL, NULL,
	  x, BT_COMPLEX, dz, 0, NULL);
  make_generic("sqrt");

/* KAH Takes array argument of type integer or real, returns array of
 * same type.  The type of the second argument must be checked to decide
 * if it's dm or msk if called with two arguments. */
  add_sym("sum", 1, BT_REAL, dr, NULL, not_ready, ar, BT_REAL, dr, 0,
	  dm, BT_INTEGER, di, 1, msk, BT_LOGICAL, dl, 1, NULL);

  add_sym("tan",  1, BT_REAL, dr, NULL, NULL, x, BT_REAL, dr, 0,
	  NULL);
  add_sym("dtan", 1, BT_REAL, dd, NULL, NULL, x, BT_REAL, dd, 0,
	  NULL);
  make_generic("tan");

  add_sym("tanh",  1, BT_REAL, dr, NULL, NULL,  x, BT_REAL, dr, 0, NULL);
  add_sym("dtanh", 1, BT_REAL, dd, NULL, NULL,  x, BT_REAL, dd, 0, NULL);
  make_generic("tanh");

  add_sym("tiny", 0, BT_REAL, dr, g95_simplify_tiny, check_tiny,
	  x, BT_UNKNOWN, dr, 0, NULL);

/* KAH Array function */
  add_sym("transfer", 0, BT_REAL, dr, NULL, not_ready, src, BT_REAL, dr, 0,
	  mo, BT_REAL, dr, 0, sz, BT_INTEGER, di, 1, NULL);

/* KAH Array function */
  add_sym("transpose", 0, BT_REAL, dr, NULL, not_ready,
	  m, BT_REAL, dr, 0, NULL);

  add_sym("trim", 1, BT_CHARACTER, dc, g95_simplify_trim, NULL,
	  stg, BT_CHARACTER, dc, 0, NULL);

/* KAH Array function */
  add_sym("ubound", 1, BT_INTEGER, di, NULL, not_ready,
	  ar, BT_REAL, dr, 0, dm, BT_INTEGER, di, 1, NULL);

/* KAH Takes a vector and an array and returns a vector */
  add_sym("unpack", 1, BT_REAL, dr, NULL, not_ready, v, BT_REAL, dr, 0,
	  msk, BT_LOGICAL, dl, 0, f, BT_REAL, dr, 0, NULL);

  add_sym("verify", 0, BT_INTEGER, di, g95_simplify_verify, NULL,
	  stg, BT_CHARACTER, dc, 0,  set, BT_CHARACTER, dc, 0,
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

  add_sym("cpu_time", 1, BT_UNKNOWN, 0, NULL, NULL, tm, BT_REAL, dr, 0, NULL);

/* KAH Last argument is a vector */
  add_sym("date_and_time", 1, BT_UNKNOWN, 0, NULL, not_ready,
	  dt, BT_CHARACTER, dc, 1, tm, BT_CHARACTER, dc, 1,
	  zn, BT_CHARACTER, dc, 1, vl, BT_INTEGER,   di, 1, NULL);

/* KAH j, ln and pt must be non-negative. i and t must have same kind
 * parameter. */
  add_sym("mvbits", 0, BT_UNKNOWN, 0, g95_simplify_mvbits, not_ready,
	  f, BT_INTEGER, di, 0, fp, BT_INTEGER, di, 0, ln, BT_INTEGER, di, 0,
	  t, BT_INTEGER, di, 0, tp, BT_INTEGER, di, 0, NULL);

/* KAH Can take an array */
  add_sym("random_number", 1, BT_UNKNOWN, 0, NULL, not_ready,
	  h, BT_REAL, dr, 0, NULL);

/* KAH Second two possible arguments are integer arrays */
  add_sym("random_seed", 1, BT_UNKNOWN, 0, NULL, not_ready,
	  sz, BT_INTEGER, di, 1, pt, BT_INTEGER, di, 1,
	  gt, BT_INTEGER, di, 1, NULL);

  add_sym("system_clock", 1, BT_UNKNOWN, 0, NULL, NULL,
          c,  BT_INTEGER, di, 1, cr, BT_INTEGER, di, 1,
          cm, BT_INTEGER, di, 1, NULL);
}



/* add_conv()-- Add a function to the list of conversion symbols */

static void add_conv(bt from_type, int from_kind, bt to_type, int to_kind,
		     g95_expr * (*simplify)()) {

g95_typespec from, to;
intrinsic_sym *sym;

  if (sizing) {
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
}


/* g95_intrinsic_init_1()-- Initialize the table of intrinsics */

void g95_intrinsic_init_1(void) {

  nargs = nfunc = nsub = nconv = 0;
  sizing = 1;

  add_functions();
  add_subroutines();
  add_conversions();

  functions = g95_getmem(sizeof(intrinsic_sym)*(nfunc+nsub)
			 + sizeof(intrinsic_arg)*nargs);

  next_sym = functions;
  subroutines = functions + nfunc;

  conversion = g95_getmem(sizeof(intrinsic_sym)*nconv);

  next_arg = ((intrinsic_arg *) (subroutines + nsub)) - 1;

  sizing = 0;
  nconv = 0;

  add_functions();
  add_subroutines();
  add_conversions();
}


void g95_intrinsic_done_1(void) {

  g95_free(functions);
  g95_free(conversion);
}


/******** Subroutines to check intrinsic interfaces ***********/


/* g95_remove_nullargs()-- Given a formal argument list, remove any
 * null arguments that may have been left behind by a sort against
 * some formal argument list. */

static void g95_remove_nullargs(g95_actual_arglist **ap) {
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

static try sort_actual(char *name, g95_actual_arglist **ap,
		       intrinsic_arg *formal) {

g95_actual_arglist *actual, *a;
intrinsic_arg *f;

  g95_remove_nullargs(ap);
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

  intrinsic_error("Too many arguments in call to '%s' at %%L", name);
  return FAILURE;

/* Associate the remaining actual arguments, all of which have to be
 * keyword arguments. */

keywords:
  for(; a; a=a->next) {
    for(f=formal; f; f=f->next)
      if (strcmp(a->name, f->name) == 0) break;

    if (f == NULL) {
      intrinsic_error("Can't find keyword named '%s' in call to '%s' at %%L",
		      a->name, name);
      return FAILURE;
    }

    if (f->actual != NULL) {
      intrinsic_error("Argument '%s' is associated twice in call to "
		      "'%s' at %%L", f->name, name);
      return FAILURE;
    }

    f->actual = a;
  }

/* At this point, all unmatched formal args must be optional */

optional:
  for(f=formal; f; f=f->next) {
    if (f->actual == NULL && f->optional == 0) {
      intrinsic_error("Missing actual argument for formal argument "
		      "'%s' in call to '%s' at %%L", f->name, name);
      return FAILURE;
    }
  }

/* Using the formal argument list, string the actual argument list
 * together in a way that corresponds with the formal list. */

do_sort:
  actual = NULL;

  for(f=formal; f; f=f->next) {
    a = (f->actual == NULL) ? g95_get_actual_arglist() : f->actual;

    if (actual == NULL) *ap = a;
    else actual->next = a;

    actual = a;
  }
  actual->next = NULL;  /* End the sorted argument list. */
      
  return SUCCESS;
}


/* check_arglist()-- Compare an actual argument list with an
 * intrinsic's formal argument list.  The lists are checked for
 * agreement of type and kind.  We don't check for arrayness here. */

static try check_arglist(g95_actual_arglist **ap, intrinsic_sym *sym) {
g95_actual_arglist *actual;
intrinsic_arg *formal;

  formal = sym->arg;
  actual = *ap;

  for(; formal; formal=formal->next, actual=actual->next) {
    if (actual->expr == NULL) continue;

    if (formal->ts.type != actual->expr->ts.type) {
      intrinsic_error("Type of argument %d in call to %s at %%L should "
		      "be %s, not %s", actual->arg_number, sym->name,
		      g95_typename(formal->ts.type),
		      g95_typename(actual->expr->ts.type));
      return FAILURE;
    }

    if (formal->ts.kind != actual->expr->ts.kind) {
      intrinsic_error("Kind of argument %d in call to %s at %%L should "
		      "be %d, not %d", actual->arg_number, sym->name,
		      formal->ts.kind,
		      actual->expr->ts.kind);
      return FAILURE;
    }
  }

  return SUCCESS;
}


/* do_simplify()-- Given an intrinsic symbol node and an expression
 * node, call the simplification function (if there is one), perhaps
 * replacing the expression with something simpler.  We return FAILURE
 * on an error of the simplification, SUCCESS if the simplification
 * worked, even if nothing has changed in the expression itself */

try do_simplify(intrinsic_sym *specific, g95_expr *e) {
g95_expr *result, *a1, *a2, *a3, *a4;
g95_actual_arglist *arg;

  if (specific->simplify == NULL) {
    result = NULL;
    goto finish;
  }

  arg = e->value.function.actual;

/* Max and min require special handling due to the variable number of args */
  if ( (specific->simplify == g95_simplify_max)   || 
       (specific->simplify == g95_simplify_amax0) || 
       (specific->simplify == g95_simplify_max1)  || 
       (specific->simplify == g95_simplify_min)   || 
       (specific->simplify == g95_simplify_amin0) || 
       (specific->simplify == g95_simplify_min1) ) {
    result = (*specific->simplify)(arg);
    goto finish;
  }

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
	  g95_internal_error("do_simplify(): Too many args for intrinsic");
	}
      }
    }
  }


 finish:
  if (result == &g95_bad_expr) return FAILURE;

  if (result == NULL) {     /* Must call at run-time */
    e->value.function.name =
      (lib_name != NULL) ? lib_name : specific->lib_name;

    e->ts = specific->ts;

  } else
    g95_replace_expr(e, result);


  return SUCCESS;
}


/* check_specific()-- Given a pointer to an intrinsic symbol and an
 * expression consisting of a function call, see if the function call
 * is consistent with the intrinsic's formal argument list.  Return
 * SUCCESS if the expression and intrinsic match, FAILURE otherwise.
 */

static try check_specific(intrinsic_sym *specific, g95_expr *expr) {
g95_actual_arglist **ap;
try t;

  ap = &expr->value.function.actual;
  lib_name = NULL;

/* Don't attempt to sort the argument list for min or max */
  if ( specific->check_function == check_min_max    ||
       specific->check_function == check_min0_max0  ||
       specific->check_function == check_min1_max1 ) {
    t=do_check(specific, *ap);
    return t;
  } 

  if (sort_actual(specific->name, ap, specific->arg) == FAILURE)
    return FAILURE;

  if (specific->check_function == NULL) {
    t = check_arglist(ap, specific);
    if (t == SUCCESS) expr->ts = specific->ts;
  } else
    t = do_check(specific, *ap);

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
 */

match g95_intrinsic_func_interface(g95_expr *expr) {
intrinsic_sym *isym, *specific;

  isym = find_function(expr->symbol->name);
  if (isym == NULL) return MATCH_NO;

/* If the function is generic, check all of its specific incarnations.
 * If the generic name is also a specific, we check that name last, so
 * that any error message will correspond to the specific */

  if (isym->specific) {
    for(specific=isym->specific; specific; specific=specific->next) {
      if (specific == isym) continue;
      if (check_specific(specific, expr) == SUCCESS) goto got_specific;
    }
  }

  if (check_specific(isym, expr) == FAILURE) return MATCH_NO;
  specific = isym;

 got_specific:
  if (do_simplify(specific, expr) == FAILURE) return MATCH_ERROR;
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
    intrinsic_error("The subroutine '%s' at %%L is not a valid intrinsic",
		    name);
    return FAILURE;
  }

  if (isym->check_function != NULL) return do_check(isym, *argp);

  return check_arglist(argp, isym);
}


/* g95_check_intrinsic()-- Given a name, search for it in the proper
 * table and return the following values:
 *
 *    0   Name not found
 *    1   Name is a generic intrinsic name
 *    2   Name is a specific intrinsic name
 */

int g95_check_intrinsic(char *name, int sub_flag) {
intrinsic_sym *sym;

  if (sub_flag)
    sym = find_function(name);
  else
    sym = find_subroutine(name);

  if (sym == NULL) return 0;

  return (sym->specific != NULL) ? 1 : 2;
}


/* g95_convert_type()-- Tries to convert an expression (in place) from
 * one type to another.  The eflag control the behavior on error.
 * The possible values are:
 *   1    Generate a g95_error()
 *   2    Generate a g95_internal_error() 
 */

try g95_convert_type(g95_expr *expr, g95_typespec *ts, int eflag) {
intrinsic_sym *sym;
g95_expr *new;

  if (expr->ts.type == BT_DERIVED && ts->type == BT_DERIVED &&
      expr->ts.derived == ts->derived) return SUCCESS;

  sym = find_conv(&expr->ts, ts);
  if (sym == NULL) goto bad;

/* Insert a pre-resolved function call to the right function */

  new = g95_get_expr();
  *new = *expr;

  new = g95_build_funcall(NULL, new, NULL);
  new->value.function.name = sym->lib_name;

  *expr = *new;

  g95_free(new);
  expr->ts = *ts;

  if (expr->value.function.actual->expr->expr_type == EXPR_CONSTANT &&
      do_simplify(sym, expr) == FAILURE) {

    if (eflag == 2) goto bad;
    return FAILURE;   /* Error already generated in do_simplify() */
  }

  return SUCCESS;  

bad:
  if (eflag == 1) {
    g95_error("Can't convert %s(%d) to %s(%d) at %L",
	      g95_typename(expr->ts.type), expr->ts.kind,
	      g95_typename(ts->type), ts->kind,
	      &expr->where);
    return FAILURE;
  }

  g95_internal_error("Can't convert %s(%d) to %s(%d) at %L",
		     g95_typename(expr->ts.type), expr->ts.kind,
		     g95_typename(ts->type), ts->kind,
		     &expr->where);

  return FAILURE;
}


/* g95_simplify_intrinsic()-- Try to simplify an expression node that
 * (might) represent an intrinsic function call.  Most intrinsic
 * function calls are not simplified here-- most are simplified as
 * soon as the function is resolved as an intrinsic, but sometimes a
 * type conversion intrinsic is inserted before resolution and must be
 * simplified at some point.
 *
 * This subroutine works by making sure the function's library name
 * points to the lib_name member of an intrinsic_sym structure.  In
 * other words, it has to have already been decided that an intrinsic
 * is called for. */

void g95_simplify_intrinsic(g95_expr *expr) {
intrinsic_sym *sym;
char *lib_name;
int i;

  if (expr->expr_type != EXPR_FUNCTION) return;

  lib_name = expr->value.function.name;

  if (lib_name >= ((char *) functions) && 
      lib_name <= ((char *) (functions+nfunc))) {
    sym = functions; 

    for(i=0; i<nfunc; i++, sym++)
      if (lib_name == sym->lib_name) {
	if (sym->simplify != NULL) do_simplify(sym, expr);
	break;
      }
  }

  if (lib_name >= ((char *) conversion) && 
      lib_name <= ((char *) (conversion+nconv))) {
    sym = conversion;

    for(i=0; i<nconv; i++, sym++)
      if (lib_name == sym->lib_name) {
	if (sym->simplify != NULL) do_simplify(sym, expr);
	break;
      }
  }
}
