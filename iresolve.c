/* Intrinsic function resolution
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


/* iresolve.c-- assign name and types to intrinsic procedures.  The
 * first argument to a resolution function is an expression pointer to
 * the original function node and the rest are pointers to the
 * arguments of the function call.  The result type and library
 * subroutine name are generally set accoring to the function
 * arguments. */

#include <string.h>
#include <stdarg.h>

#include "g95.h"
#include "intrinsic.h"


/* String pool subroutines.  This are used to provide static locations
 * for the string constants that represent library function names. */

typedef struct string_node {
  struct string_node *next;
  char string[1];
} string_node;

#define HASH_SIZE 13

static string_node *string_head[HASH_SIZE];


/* hash()-- Return a hash code based on the name */

static int hash(char *name) {
int h;

  h = 1;
  while(*name)
    h = 5311966*h + *name++;

  if (h < 0) h = -h;
  return h % HASH_SIZE;
}


/* get_string()-- Given printf-like arguments, return a static address
 * of the resulting string.  If the name is not in the table, it is
 * added. */

static char *get_string(const char *format, ...) {
char temp_name[50];
string_node *p;
va_list ap;
int h;

  va_start(ap, format); 
  vsprintf(temp_name, format, ap);
  va_end(ap);

  h = hash(temp_name);

  /* Search */

  for(p=string_head[h]; p; p=p->next)
    if (strcmp(p->string, temp_name) == 0) return p->string;

  /* Add */

  p = g95_getmem(sizeof(string_node) + strlen(temp_name));

  strcpy(p->string, temp_name);

  p->next = string_head[h];
  string_head[h] = p;

  return p->string;
}



static void free_strings(void) {
string_node *p, *q;
int h;

  for(h=0; h<HASH_SIZE; h++) {
    for(p=string_head[h]; p; p=q) {
      q = p->next;
      g95_free(p);
    }
  }
}


/********************** Resolution functions **********************/


void g95_resolve_abs(g95_expr *f, g95_expr *a) {

  f->ts = a->ts;
  if (f->ts.type == BT_COMPLEX) f->ts.type = BT_REAL;

  f->value.function.name =
    get_string("__abs_%c_%d", g95_type_letter(a->ts.type), a->ts.kind);
}


void g95_resolve_aint(g95_expr *f, g95_expr *a, g95_expr *kind) {

  f->ts.type = a->ts.type;
  f->ts.kind = (kind == NULL) ? a->ts.kind
    : mpz_get_ui(kind->value.integer);

  f->value.function.name =
    get_string("__aint%d_%c%d", f->ts.kind, g95_type_letter(a->ts.type),
	       a->ts.kind);
}


void g95_resolve_all(g95_expr *f, g95_expr *mask, g95_expr *dim) {
static char all0[] = "__all0", all1[] = "__all1";

  f->ts = mask->ts;

  if (dim == NULL || mask->rank == 1)
    f->value.function.name = all0;
  else {
    f->value.function.name = all1;
    f->rank = mask->rank - 1;
  }
}


void g95_resolve_anint(g95_expr *f, g95_expr *a, g95_expr *kind) {

  f->ts.type = a->ts.type;
  f->ts.kind = (kind == NULL) ? a->ts.kind
    : mpz_get_ui(kind->value.integer);

  f->value.function.name =
    get_string("__anint%d_%c%d", f->ts.kind, g95_type_letter(a->ts.type),
	       a->ts.kind);
}


void g95_resolve_any(g95_expr *f, g95_expr *mask, g95_expr *dim) {
static char any0[] = "__any0", any1[] = "__any1";

  f->ts = mask->ts;

  if (dim == NULL || mask->rank == 1)
    f->value.function.name = any0;
  else {
    f->value.function.name = any1;
    f->rank = mask->rank - 1;
  }
}


void g95_resolve_btest(g95_expr *f, g95_expr *i, g95_expr *pos) {

  f->ts.type = BT_LOGICAL;
  f->ts.kind = g95_default_logical_kind();

  f->value.function.name = get_string("__btest_%d_%d", i->ts.kind,
				      pos->ts.kind);
}


void g95_resolve_ceiling(g95_expr *f, g95_expr *a, g95_expr *kind) {

  f->ts.type = BT_INTEGER;
  f->ts.kind = (kind == NULL) ? g95_default_integer_kind()
    : mpz_get_ui(kind->value.integer);

  f->value.function.name =
    get_string("__ceiling%d_%c%d", f->ts.kind, g95_type_letter(a->ts.type),
	       a->ts.kind);
}


void g95_resolve_char(g95_expr *f, g95_expr *a, g95_expr *kind) {

  f->ts.type = BT_CHARACTER;
  f->ts.kind = (kind == NULL) ? g95_default_character_kind()
    : mpz_get_ui(kind->value.integer);

  f->value.function.name =
    get_string("__char%d_%c%d", f->ts.kind, g95_type_letter(a->ts.type),
	       a->ts.kind);
}


void g95_resolve_cmplx(g95_expr *f, g95_expr *x, g95_expr *y, g95_expr *kind) {

  f->ts.type = BT_COMPLEX;
  f->ts.kind = (kind == NULL) ? g95_default_real_kind()
    : mpz_get_ui(kind->value.integer);

  if (y == NULL)
    f->value.function.name =
      get_string("__cmplx0_%d_%c%d", f->ts.kind,
		 g95_type_letter(x->ts.type), x->ts.kind);
  else
    f->value.function.name =
      get_string("__cmplx1_%d_%c%d_%c%d", f->ts.kind,
		 g95_type_letter(x->ts.type), x->ts.kind,
		 g95_type_letter(y->ts.type), y->ts.kind);
}


void g95_resolve_dim(g95_expr *f, g95_expr *x, g95_expr *y) {

  f->ts = x->ts;
  f->value.function.name =
    get_string("__dim_%c_%d", g95_type_letter(x->ts.type), x->ts.kind);
}


void g95_resolve_dot_product(g95_expr *f, g95_expr *a, g95_expr *b) {
g95_expr temp;

  if (a->ts.type == BT_LOGICAL && b->ts.type == BT_LOGICAL) {
    f->ts.type = BT_LOGICAL;
    f->ts.kind = g95_default_logical_kind();
  } else {
    temp.op1 = a;
    temp.op2 = b;
    g95_type_convert_binary(&temp);
    f->ts = temp.ts;
  }

  f->value.function.name = get_string("__dot_product_%c%d",
				      g95_type_letter(f->ts.type), f->ts.kind);
}


void g95_resolve_exponent(g95_expr *f, g95_expr *x) {

  f->ts.type = BT_INTEGER;
  f->ts.kind = g95_default_integer_kind();

  f->value.function.name = get_string("__exponent_%d", x->ts.kind);
}


void g95_resolve_floor(g95_expr *f, g95_expr *a, g95_expr *kind) {

  f->ts.type = BT_INTEGER;
  f->ts.kind = (kind == NULL) ? g95_default_integer_kind()
    : mpz_get_ui(kind->value.integer);

  f->value.function.name =
    get_string("__floor%d_%c%d", f->ts.kind, g95_type_letter(a->ts.type),
	       a->ts.kind);
}


void g95_resolve_fraction(g95_expr *f, g95_expr *x) {

  f->ts = x->ts;
  f->value.function.name = get_string("__fraction_%d", x->ts.kind);
}


void g95_resolve_int(g95_expr *f, g95_expr *a, g95_expr *kind) {

  f->ts.type = BT_INTEGER;
  f->ts.kind = (kind == NULL) ? g95_default_integer_kind()
    : mpz_get_ui(kind->value.integer);

  f->value.function.name =
    get_string("__int%d_%c%d", f->ts.kind, g95_type_letter(a->ts.type),
	       a->ts.kind);
}


void g95_resolve_ishft(g95_expr *f, g95_expr *i, g95_expr *shift) {

  f->ts = i->ts;
  f->value.function.name =
    get_string("__ishft_%d_%d", i->ts.kind, shift->ts.kind);
}


void g95_resolve_ishftc(g95_expr *f, g95_expr *i, g95_expr *shift,
			g95_expr *size) {
int s_kind;

  s_kind = (size == NULL) ? g95_default_integer_kind() : shift->ts.kind;

  f->ts = i->ts;
  f->value.function.name =
    get_string("__ishftc_%d_%d_%d", i->ts.kind, shift->ts.kind, s_kind);
}


void g95_resolve_logical(g95_expr *f, g95_expr *a, g95_expr *kind) {

  f->ts.type = BT_LOGICAL;
  f->ts.kind = (kind == NULL) ? g95_default_logical_kind()
    : mpz_get_ui(kind->value.integer);

  f->value.function.name =
    get_string("__logical%d_%c%d", f->ts.kind, g95_type_letter(a->ts.type),
	       a->ts.kind);
}


static char *max_name(bt type, int kind) {
static char max0[] = "__max0", amax1[] = "__amax1", dmax1[] = "__dmax1";

  if (type == BT_INTEGER && kind == g95_default_integer_kind()) return max0;
  if (type == BT_REAL && kind == g95_default_real_kind()) return amax1;
  if (type == BT_REAL && kind == g95_default_double_kind()) return dmax1;

  return get_string("__max_%c%d", g95_type_letter(type), kind);
}


void g95_resolve_max(g95_expr *f, g95_expr *a1) {

  f->ts = a1->ts;
  f->value.function.name = get_string(max_name(a1->ts.type, a1->ts.kind));
}


void g95_resolve_maxval(g95_expr *f, g95_expr *array, g95_expr *dim,
			g95_expr *mask) {

  f->ts = array->ts;
  if (dim != NULL && array->rank != 1)
    f->rank = array->rank - 1;

  f->value.function.name =
    get_string("__maxval_%c%d", g95_type_letter(array->ts.type),
	       array->ts.kind);
}


static char *min_name(bt type, int kind) {
static char min0[] = "__min0", amin1[] = "__amin1", dmin1[] = "__dmin1";

  if (type == BT_INTEGER && kind == g95_default_integer_kind()) return min0;
  if (type == BT_REAL && kind == g95_default_real_kind()) return amin1;
  if (type == BT_REAL && kind == g95_default_double_kind()) return dmin1;

  return get_string("__min_%c%d", g95_type_letter(type), kind);
}


void g95_resolve_min(g95_expr *f, g95_expr *a1) {

  f->ts = a1->ts;
  f->value.function.name = get_string(min_name(a1->ts.type, a1->ts.kind));
}


void g95_resolve_minval(g95_expr *f, g95_expr *array, g95_expr *dim,
			     g95_expr *mask) {
  f->ts = array->ts;

  if (dim != NULL && array->rank != 1)
    f->rank = array->rank - 1;

  f->value.function.name =
    get_string("__minval_%c%d", g95_type_letter(array->ts.type),
	       array->ts.kind);
}


void g95_resolve_mod(g95_expr *f, g95_expr *a, g95_expr *p) {

  f->ts = a->ts;
  f->value.function.name =
    get_string("__mod_%c_%d", g95_type_letter(a->ts.type), a->ts.kind);
}


void g95_resolve_modulo(g95_expr *f, g95_expr *a, g95_expr *p) {

  f->ts = a->ts;
  f->value.function.name =
    get_string("__modulo_%c_%d", g95_type_letter(a->ts.type), a->ts.kind);
}


void g95_resolve_nint(g95_expr *f, g95_expr *a, g95_expr *kind) {

  f->ts.type = BT_INTEGER;
  f->ts.kind = (kind == NULL) ? g95_default_integer_kind()
    : mpz_get_ui(kind->value.integer);

  f->value.function.name =
    get_string("__nint%d_%c%d", f->ts.kind, g95_type_letter(a->ts.type),
	       a->ts.kind);
}


void g95_resolve_not(g95_expr *f, g95_expr *i) {

  f->ts = i->ts;
  f->value.function.name = get_string("__not_%d", i->ts.kind);
}


void g95_resolve_product(g95_expr *f, g95_expr *array, g95_expr *dim,
			 g95_expr *mask) {

  f->ts = array->ts;
  f->value.function.name =
    get_string("__product_%c_%d", g95_type_letter(array->ts.type),
	       array->ts.kind);
}


void g95_resolve_real(g95_expr *f, g95_expr *a, g95_expr *kind) {

  f->ts.type = BT_REAL;
  f->ts.kind = (kind == NULL) ? g95_default_real_kind()
    : mpz_get_ui(kind->value.integer);

  f->value.function.name =
    get_string("__real%d_%c%d", f->ts.kind, g95_type_letter(a->ts.type),
	       a->ts.kind);
}


void g95_resolve_reshape(g95_expr *f, g95_expr *source, g95_expr *shape,
			 g95_expr *pad, g95_expr *order) {

  f->ts = source->ts;
}


void g95_resolve_rrspacing(g95_expr *f, g95_expr *x) {

  f->ts = x->ts;
  f->value.function.name = get_string("__rrspacing_%d", x->ts.kind);
}


void g95_resolve_scale(g95_expr *f, g95_expr *x, g95_expr *y) {

  f->ts = x->ts;
  f->value.function.name = get_string("__scale_%d_%d", x->ts.kind, x->ts.kind);
}


void g95_resolve_set_exponent(g95_expr *f, g95_expr *x, g95_expr *i) {

  f->ts = x->ts;
  f->value.function.name = get_string("__set_exponent_%d_%d",
				      x->ts.kind, i->ts.kind);
}


void g95_resolve_shape(g95_expr *f, g95_expr *source) {

  f->ts.type = BT_INTEGER;
  f->ts.kind = g95_default_integer_kind();
  f->rank = 1;
}


void g95_resolve_spacing(g95_expr *f, g95_expr *x) {

  f->ts = x->ts;
  f->value.function.name = get_string("__spacing_%d", x->ts.kind);
}


void g95_resolve_sum(g95_expr *f, g95_expr *array, g95_expr *dim,
		     g95_expr *mask) {

  f->ts = array->ts;

  if (dim != NULL && array->rank != 1) f->rank = array->rank - 1;

  f->value.function.name =
    get_string("__sum_%c%d", g95_type_letter(array->ts.type), array->ts.kind);
}


void g95_resolve_transpose(g95_expr *f, g95_expr *matrix) {

  f->ts = matrix->ts;
  f->rank = 2;

  f->value.function.name =
    get_string("__transpose_%c%d", g95_type_letter(matrix->ts.type),
	       matrix->ts.kind);
}


void g95_iresolve_init_1(void) {
int i;

  for(i=0; i<HASH_SIZE; i++)
    string_head[i] = NULL;
}


void g95_iresolve_done_1(void) {

  free_strings();
}
