/* Intrinsic function resolution
   Copyright (C) 2000, 2001, 2002 Free Software Foundation, Inc.
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


/* iresolve.c-- assign name and types to intrinsic procedures.  For
 * functions, the first argument to a resolution function is an
 * expression pointer to the original function node and the rest are
 * pointers to the arguments of the function call.  For subroutines,
 * a pointer to the code node is passed.
 *
 * The result type and library subroutine name are generally set
 * according to the function arguments. */

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


/* g95_get_string()-- Given printf-like arguments, return a static
 * address of the resulting string.  If the name is not in the table,
 * it is added. */

char *g95_get_string(char *format, ...) {
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
    g95_get_string(PREFIX "abs_%c%d", g95_type_letter(a->ts.type), a->ts.kind);
}


void g95_resolve_acos(g95_expr *f, g95_expr *x) {

  f->ts = x->ts;
  f->value.function.name =
    g95_get_string(PREFIX "acos_%c%d", g95_type_letter(x->ts.type),
		   x->ts.kind);
}


void g95_resolve_aimag(g95_expr *f, g95_expr *x) {

  f->ts.type = BT_REAL;
  f->ts.kind = x->ts.kind;
  f->value.function.name =
    g95_get_string(PREFIX "aimag_%c%d", g95_type_letter(x->ts.type),
		   x->ts.kind);
}


void g95_resolve_aint(g95_expr *f, g95_expr *a, g95_expr *kind) {

  f->ts.type = a->ts.type;
  f->ts.kind = (kind == NULL) ? a->ts.kind
    : mpz_get_si(kind->value.integer);

  f->value.function.name =
    g95_get_string(PREFIX "aint_%d_%c%d", f->ts.kind,
		   g95_type_letter(a->ts.type), a->ts.kind);
}


void g95_resolve_all(g95_expr *f, g95_expr *mask, g95_expr *dim) {

  f->ts = mask->ts;

  if (dim != NULL && mask->rank != 1) f->rank = mask->rank - 1;

  f->value.function.name =
    g95_get_string(PREFIX "all_%c%d", g95_type_letter(mask->ts.type),
                   mask->ts.kind);
}


void g95_resolve_anint(g95_expr *f, g95_expr *a, g95_expr *kind) {

  f->ts.type = a->ts.type;
  f->ts.kind = (kind == NULL) ? a->ts.kind
    : mpz_get_si(kind->value.integer);

  f->value.function.name =
    g95_get_string(PREFIX "anint_%d_%c%d", f->ts.kind,
		   g95_type_letter(a->ts.type), a->ts.kind);
}


void g95_resolve_any(g95_expr *f, g95_expr *mask, g95_expr *dim) {

  f->ts = mask->ts;

  if (dim != NULL && mask->rank != 1) f->rank = mask->rank - 1;

  f->value.function.name =
    g95_get_string(PREFIX "any_%c%d", g95_type_letter(mask->ts.type),
                   mask->ts.kind);
}


void g95_resolve_asin(g95_expr *f, g95_expr *x) {

  f->ts = x->ts;
  f->value.function.name =
    g95_get_string(PREFIX "asin_%c%d", g95_type_letter(x->ts.type),
		   x->ts.kind);
}


void g95_resolve_atan(g95_expr *f, g95_expr *x) {

  f->ts = x->ts;
  f->value.function.name =
    g95_get_string(PREFIX "atan_%c%d", g95_type_letter(x->ts.type),
		   x->ts.kind);
}


void g95_resolve_atan2(g95_expr *f, g95_expr *x, g95_expr *y) {

  f->ts = x->ts;
  f->value.function.name =
    g95_get_string(PREFIX "atan2_%c%d", g95_type_letter(x->ts.type),
		   x->ts.kind);
}


void g95_resolve_btest(g95_expr *f, g95_expr *i, g95_expr *pos) {

  f->ts.type = BT_LOGICAL;
  f->ts.kind = g95_default_logical_kind();

  f->value.function.name = g95_get_string(PREFIX "btest_%d_%d", i->ts.kind,
					  pos->ts.kind);
}


void g95_resolve_ceiling(g95_expr *f, g95_expr *a, g95_expr *kind) {

  f->ts.type = BT_INTEGER;
  f->ts.kind = (kind == NULL) ? g95_default_integer_kind()
    : mpz_get_si(kind->value.integer);

  f->value.function.name =
    g95_get_string(PREFIX "ceiling_%d_%c%d", f->ts.kind,
		   g95_type_letter(a->ts.type), a->ts.kind);
}


void g95_resolve_char(g95_expr *f, g95_expr *a, g95_expr *kind) {

  f->ts.type = BT_CHARACTER;
  f->ts.kind = (kind == NULL) ? g95_default_character_kind()
    : mpz_get_si(kind->value.integer);

  f->value.function.name =
    g95_get_string(PREFIX "char_%d_%c%d", f->ts.kind,
		   g95_type_letter(a->ts.type), a->ts.kind);
}


void g95_resolve_cmplx(g95_expr *f, g95_expr *x, g95_expr *y, g95_expr *kind) {

  f->ts.type = BT_COMPLEX;
  f->ts.kind = (kind == NULL) ? g95_default_real_kind()
    : mpz_get_si(kind->value.integer);

  if (y == NULL)
    f->value.function.name =
      g95_get_string(PREFIX "cmplx0_%d_%c%d", f->ts.kind,
		     g95_type_letter(x->ts.type), x->ts.kind);
  else
    f->value.function.name =
      g95_get_string(PREFIX "cmplx1_%d_%c%d_%c%d", f->ts.kind,
		     g95_type_letter(x->ts.type), x->ts.kind,
		     g95_type_letter(y->ts.type), y->ts.kind);
}


void g95_resolve_conjg(g95_expr *f, g95_expr *x) {

  f->ts = x->ts;
  f->value.function.name = g95_get_string(PREFIX "conjg_%d", x->ts.kind);
}


void g95_resolve_cos(g95_expr *f, g95_expr *x) {

  f->ts = x->ts;
  f->value.function.name =
    g95_get_string(PREFIX "cos_%c%d", g95_type_letter(x->ts.type), x->ts.kind);
}


void g95_resolve_cosh(g95_expr *f, g95_expr *x) {

  f->ts = x->ts;
  f->value.function.name =
    g95_get_string(PREFIX "cosh_%c%d", g95_type_letter(x->ts.type),
		   x->ts.kind);
}


void g95_resolve_count(g95_expr *f, g95_expr *mask, g95_expr *dim) {

  f->ts.type = BT_INTEGER;
  f->ts.kind = g95_default_integer_kind();

  if (dim != NULL) f->rank = mask->rank - 1;

  f->value.function.name =
    g95_get_string(PREFIX "count_%d_%c%d", f->ts.kind,
                   g95_type_letter(mask->ts.type), mask->ts.kind);
}


void g95_resolve_cshift(g95_expr *f, g95_expr *array, g95_expr *shift,
			g95_expr *dim) {
static char cshift0[] = PREFIX "cshift0", cshift1[] = PREFIX "cshift1";

  f->ts = array->ts;
  f->rank = array->rank;

  if (dim == NULL)
    f->value.function.name = cshift0;
  else {
    f->value.function.name = cshift1;
  }
}


void g95_resolve_dble(g95_expr *f, g95_expr *a) {

  f->ts.type = BT_REAL;
  f->ts.kind = g95_default_double_kind();
  f->value.function.name =
    g95_get_string(PREFIX "dble_%c%d", g95_type_letter(a->ts.type),
		   a->ts.kind);
}


void g95_resolve_dim(g95_expr *f, g95_expr *x, g95_expr *y) {

  f->ts = x->ts;
  f->value.function.name =
    g95_get_string(PREFIX "dim_%c%d", g95_type_letter(x->ts.type), x->ts.kind);
}


void g95_resolve_dot_product(g95_expr *f, g95_expr *a, g95_expr *b) {
g95_expr temp;

  if (a->ts.type == BT_LOGICAL && b->ts.type == BT_LOGICAL) {
    f->ts.type = BT_LOGICAL;
    f->ts.kind = g95_default_logical_kind();
  } else {
    temp.expr_type = EXPR_OP;
    g95_clear_ts(&temp.ts);
    temp.operator = INTRINSIC_NONE;
    temp.op1 = a;
    temp.op2 = b;
    g95_type_convert_binary(&temp);
    f->ts = temp.ts;
  }

  f->value.function.name =
    g95_get_string(PREFIX "dot_product_%c%d", g95_type_letter(f->ts.type),
		   f->ts.kind);
}


void g95_resolve_eoshift(g95_expr *f, g95_expr *array, g95_expr *shift,
      		                   g95_expr *boundary, g95_expr *dim) {

  f->ts = array->ts;
  f->rank = array->rank;

  f->value.function.name =
    g95_get_string(PREFIX "eoshift_%c%d", g95_type_letter(array->ts.type),
		   array->ts.kind);
}


void g95_resolve_exp(g95_expr *f, g95_expr *x) {

  f->ts = x->ts;
  f->value.function.name =
    g95_get_string(PREFIX "exp_%c%d", g95_type_letter(x->ts.type), x->ts.kind);
}


void g95_resolve_exponent(g95_expr *f, g95_expr *x) {

  f->ts.type = BT_INTEGER;
  f->ts.kind = g95_default_integer_kind();

  f->value.function.name = g95_get_string(PREFIX "exponent_%d", x->ts.kind);
}


void g95_resolve_floor(g95_expr *f, g95_expr *a, g95_expr *kind) {

  f->ts.type = BT_INTEGER;
  f->ts.kind = (kind == NULL) ? g95_default_integer_kind()
    : mpz_get_si(kind->value.integer);

  f->value.function.name =
    g95_get_string(PREFIX "floor%d_%c%d", f->ts.kind,
		   g95_type_letter(a->ts.type), a->ts.kind);
}


void g95_resolve_fraction(g95_expr *f, g95_expr *x) {

  f->ts = x->ts;
  f->value.function.name = g95_get_string(PREFIX "fraction_%d", x->ts.kind);
}


void g95_resolve_iand(g95_expr *f, g95_expr *i, g95_expr *j) {

  f->ts = i->ts;
  f->value.function.name = g95_get_string(PREFIX "iand_%d", i->ts.kind);
}


void g95_resolve_ibclr(g95_expr *f, g95_expr *i, g95_expr *pos) {

  f->ts = i->ts;
  f->value.function.name = g95_get_string(PREFIX "ibclr_%d", i->ts.kind);
}


void g95_resolve_ibits(g95_expr *f, g95_expr *i, g95_expr *pos,
		       g95_expr *len) {

  f->ts = i->ts;
  f->value.function.name = g95_get_string(PREFIX "ibits_%d", i->ts.kind);
}


void g95_resolve_ibset(g95_expr *f, g95_expr *i, g95_expr *pos) {

  f->ts = i->ts;
  f->value.function.name = g95_get_string(PREFIX "ibset_%d", i->ts.kind);
}


void g95_resolve_ichar(g95_expr *f, g95_expr *c) {

  f->ts.type = BT_INTEGER;
  f->ts.kind = g95_default_integer_kind();

  f->value.function.name = g95_get_string(PREFIX "ichar_%d", c->ts.kind);
}


void g95_resolve_idnint(g95_expr *f, g95_expr *a) {
  g95_resolve_nint(f, a, NULL);
}


void g95_resolve_ieor(g95_expr *f, g95_expr *i, g95_expr *j) {

  f->ts = i->ts;
  f->value.function.name = g95_get_string(PREFIX "ieor_%d", i->ts.kind);
}


void g95_resolve_ior(g95_expr *f, g95_expr *i, g95_expr *j) {

  f->ts = i->ts;
  f->value.function.name = g95_get_string(PREFIX "ior_%d", i->ts.kind);
}


void g95_resolve_int(g95_expr *f, g95_expr *a, g95_expr *kind) {

  f->ts.type = BT_INTEGER;
  f->ts.kind = (kind == NULL) ? g95_default_integer_kind()
    : mpz_get_si(kind->value.integer);

  f->value.function.name =
    g95_get_string(PREFIX "int_%d_%c%d", f->ts.kind,
		   g95_type_letter(a->ts.type), a->ts.kind);
}


void g95_resolve_ishft(g95_expr *f, g95_expr *i, g95_expr *shift) {

  f->ts = i->ts;
  f->value.function.name =
    g95_get_string(PREFIX "ishft_%d_%d", i->ts.kind, shift->ts.kind);
}


void g95_resolve_ishftc(g95_expr *f, g95_expr *i, g95_expr *shift,
			g95_expr *size) {
int s_kind;

  s_kind = (size == NULL) ? g95_default_integer_kind() : shift->ts.kind;

  f->ts = i->ts;
  f->value.function.name =
    g95_get_string(PREFIX "ishftc_%d_%d_%d", i->ts.kind, shift->ts.kind,
		   s_kind);
}


void g95_resolve_lbound(g95_expr *f, g95_expr *array, g95_expr *dim) {
static char lbound0[] = PREFIX "lbound0", lbound1[] = PREFIX "lbound1";

  f->ts.type = BT_INTEGER;
  f->ts.kind = g95_default_integer_kind();

  if (dim != NULL)
    f->value.function.name = lbound0;
  else {
    f->value.function.name = lbound1;
    f->rank = 1;
  }
}


void g95_resolve_len(g95_expr *f, g95_expr *string) {

  f->ts.type = BT_INTEGER;
  f->ts.kind = g95_default_integer_kind();
  f->value.function.name = g95_get_string(PREFIX "len_%d", string->ts.kind);
}


void g95_resolve_len_trim(g95_expr *f, g95_expr *string) {

  f->ts.type = BT_INTEGER;
  f->ts.kind = g95_default_integer_kind();
  f->value.function.name = g95_get_string(PREFIX "len_trim%d",
					  string->ts.kind);
}


void g95_resolve_log(g95_expr *f, g95_expr *x) {

  f->ts = x->ts;
  f->value.function.name =
    g95_get_string(PREFIX "log_%c%d", g95_type_letter(x->ts.type), x->ts.kind);
}


void g95_resolve_log10(g95_expr *f, g95_expr *x) {

  f->ts = x->ts;
  f->value.function.name =
    g95_get_string(PREFIX "log10_%c%d", g95_type_letter(x->ts.type),
		   x->ts.kind);
}


void g95_resolve_logical(g95_expr *f, g95_expr *a, g95_expr *kind) {

  f->ts.type = BT_LOGICAL;
  f->ts.kind = (kind == NULL) ? g95_default_logical_kind()
    : mpz_get_si(kind->value.integer);
  f->rank = a->rank;

  f->value.function.name =
    g95_get_string(PREFIX "logical_%d_%c%d", f->ts.kind,
		   g95_type_letter(a->ts.type), a->ts.kind);
}


void g95_resolve_matmul(g95_expr *f, g95_expr *a, g95_expr *b) {
g95_expr temp;

  if (a->ts.type == BT_LOGICAL && b->ts.type == BT_LOGICAL) {
    f->ts.type = BT_LOGICAL;
    f->ts.kind = g95_default_logical_kind();
  } else {
    temp.expr_type = EXPR_OP;
    g95_clear_ts(&temp.ts);
    temp.operator = INTRINSIC_NONE;
    temp.op1 = a;
    temp.op2 = b;
    g95_type_convert_binary(&temp);
    f->ts = temp.ts;
  }

  f->rank = (a->rank == 2 && b->rank == 2) ? 2 : 1;

  f->value.function.name =
    g95_get_string(PREFIX "matmul_%c%d", g95_type_letter(f->ts.type),
		   f->ts.kind);
}


void g95_resolve_max(g95_expr *f, g95_expr *a1) {

  f->ts = a1->ts;
  f->value.function.name =
    g95_get_string(PREFIX "max_%c%d", g95_type_letter(a1->ts.type),
		   a1->ts.kind);
}


void g95_resolve_maxloc(g95_expr *f, g95_expr *array, g95_expr *dim,
			g95_expr *mask) {
char *name;

  f->ts = array->ts;

  if (dim == NULL)
    f->rank = 1;
  else
    f->rank = array->rank - 1;

  name = mask ? "mmaxloc" : "maxloc";
  f->value.function.name =
    g95_get_string(PREFIX "%s%d_%d_%c%d", name, dim != NULL, f->ts.kind,
                   g95_type_letter(array->ts.type), array->ts.kind);
}


void g95_resolve_maxval(g95_expr *f, g95_expr *array, g95_expr *dim,
			g95_expr *mask) {

  f->ts = array->ts;

  if (dim != NULL && array->rank != 1)
    f->rank = array->rank - 1;

  f->value.function.name =
    g95_get_string(PREFIX "%s_%c%d", mask ? "mmaxval" : "maxval",
                   g95_type_letter(array->ts.type), array->ts.kind);
}


void g95_resolve_merge(g95_expr *f, g95_expr *tsource, g95_expr *fsource,
		       g95_expr *mask) {

  f->ts = tsource->ts;
  f->value.function.name =
    g95_get_string(PREFIX "merge_%c%d", g95_type_letter(tsource->ts.type),
		   tsource->ts.kind);
}


void g95_resolve_min(g95_expr *f, g95_expr *a1) {

  f->ts = a1->ts;
  f->value.function.name =
    g95_get_string(PREFIX "min_%c%d", g95_type_letter(a1->ts.type),
		   a1->ts.kind);
}


void g95_resolve_minloc(g95_expr *f, g95_expr *array, g95_expr *dim,
			g95_expr *mask) {
char *name;

  f->ts = array->ts;

  if (dim == NULL)
    f->rank = 1;
  else
    f->rank = array->rank - 1;

  name = mask ? "mminloc" : "minloc";
  f->value.function.name =
    g95_get_string(PREFIX "%s%d_%d_%c%d", name, dim != NULL, f->ts.kind,
                   g95_type_letter(array->ts.type), array->ts.kind);
}

void g95_resolve_minval(g95_expr *f, g95_expr *array, g95_expr *dim,
			     g95_expr *mask) {

  f->ts = array->ts;

  if (dim != NULL && array->rank != 1)
    f->rank = array->rank - 1;

  f->value.function.name =
    g95_get_string(PREFIX "%s_%c%d", mask ? "mminval" : "minval",
                   g95_type_letter(array->ts.type), array->ts.kind);
}


void g95_resolve_mod(g95_expr *f, g95_expr *a, g95_expr *p) {

  f->ts = a->ts;
  f->value.function.name =
    g95_get_string(PREFIX "mod_%c%d", g95_type_letter(a->ts.type), a->ts.kind);
}


void g95_resolve_modulo(g95_expr *f, g95_expr *a, g95_expr *p) {

  f->ts = a->ts;
  f->value.function.name =
    g95_get_string(PREFIX "modulo_%c%d", g95_type_letter(a->ts.type),
		   a->ts.kind);
}


void g95_resolve_nint(g95_expr *f, g95_expr *a, g95_expr *kind) {

  f->ts.type = BT_INTEGER;
  f->ts.kind = (kind == NULL) ? g95_default_integer_kind()
    : mpz_get_si(kind->value.integer);

  f->value.function.name =
    g95_get_string(PREFIX "nint_%d_%d", f->ts.kind, a->ts.kind);
}


void g95_resolve_not(g95_expr *f, g95_expr *i) {

  f->ts = i->ts;
  f->value.function.name = g95_get_string(PREFIX "not_%d", i->ts.kind);
}


void g95_resolve_pack(g95_expr *f, g95_expr *array, g95_expr *mask,
		      g95_expr *vector) {

  f->ts = array->ts;
  f->rank = 1;

  f->value.function.name =
    g95_get_string(PREFIX "pack_%c%d", g95_type_letter(array->ts.type),
		   array->ts.kind);
}


void g95_resolve_product(g95_expr *f, g95_expr *array, g95_expr *dim,
			 g95_expr *mask) {

  f->ts = array->ts;

  if (dim != NULL && array->rank != 1) f->rank = array->rank - 1;

  f->value.function.name =
    g95_get_string(PREFIX "%s_%c%d", mask ? "mproduct" : "product",
                   g95_type_letter(array->ts.type), array->ts.kind);
}


void g95_resolve_real(g95_expr *f, g95_expr *a, g95_expr *kind) {

  f->ts.type = BT_REAL;

  if (kind != NULL)
    f->ts.kind = mpz_get_si(kind->value.integer);
  else
    f->ts.kind = (a->ts.type == BT_COMPLEX) ?
      a->ts.kind : g95_default_real_kind();

  f->value.function.name =
    g95_get_string(PREFIX "real_%d_%c%d", f->ts.kind,
		   g95_type_letter(a->ts.type), a->ts.kind);
}


void g95_resolve_repeat(g95_expr *f, g95_expr *string, g95_expr *ncopies) {

  f->ts.type = BT_CHARACTER;
  f->ts.kind = string->ts.kind;
  f->value.function.name = g95_get_string(PREFIX "repeat_%d", string->ts.kind);
}


void g95_resolve_reshape(g95_expr *f, g95_expr *source, g95_expr *shape,
			 g95_expr *pad, g95_expr *order) {
static char reshape0[] = PREFIX "reshape";
mpz_t rank;
int kind;

  f->ts = source->ts;

  g95_array_size(shape, &rank);
  f->rank = mpz_get_si(rank);
  mpz_clear(rank);
  if (source->ts.type == BT_DERIVED)
    kind = 0;
  else
    kind = source->ts.kind;

  switch (kind) {
  case 4:
  case 8:
    /* case 16: */
    f->value.function.name =
      g95_get_string(PREFIX "reshape_%d", source->ts.kind);
    break;
      
  default:
    f->value.function.name = reshape0;
    break;
  }

}


void g95_resolve_rrspacing(g95_expr *f, g95_expr *x) {

  f->ts = x->ts;
  f->value.function.name = g95_get_string(PREFIX "rrspacing_%d", x->ts.kind);
}


void g95_resolve_scale(g95_expr *f, g95_expr *x, g95_expr *y) {

  f->ts = x->ts;
  f->value.function.name = g95_get_string(PREFIX "scale_%d_%d", x->ts.kind,
					  x->ts.kind);
}


void g95_resolve_scan(g95_expr *f, g95_expr *string, g95_expr *set,
		      g95_expr *back) {

  f->ts.type = BT_INTEGER;
  f->ts.kind = g95_default_integer_kind();
  f->value.function.name = g95_get_string(PREFIX "scan_%d", string->ts.kind);
}


void g95_resolve_set_exponent(g95_expr *f, g95_expr *x, g95_expr *i) {

  f->ts = x->ts;
  f->value.function.name =
    g95_get_string(PREFIX "set_exponent_%d_%d", x->ts.kind, i->ts.kind);
}


void g95_resolve_shape(g95_expr *f, g95_expr * array) {

  f->ts.type = BT_INTEGER;
  f->ts.kind = g95_default_integer_kind();
  f->rank = 1;
  f->value.function.name = g95_get_string(PREFIX "shape_%d", f->ts.kind);
}


void g95_resolve_sign(g95_expr *f, g95_expr *a, g95_expr *b) {

  f->ts = a->ts;
  f->value.function.name =
    g95_get_string(PREFIX "sign_%c%d", g95_type_letter(a->ts.type),
		   a->ts.kind);
}


void g95_resolve_sin(g95_expr *f, g95_expr *x) {

  f->ts = x->ts;
  f->value.function.name =
    g95_get_string(PREFIX "sin_%c%d", g95_type_letter(x->ts.type), x->ts.kind);
}


void g95_resolve_sinh(g95_expr *f, g95_expr *x) {

  f->ts = x->ts;
  f->value.function.name =
    g95_get_string(PREFIX "sinh_%c%d", g95_type_letter(x->ts.type),
		   x->ts.kind);
}


void g95_resolve_spacing(g95_expr *f, g95_expr *x) {

  f->ts = x->ts;
  f->value.function.name = g95_get_string(PREFIX "spacing_%d", x->ts.kind);
}


void g95_resolve_spread(g95_expr *f, g95_expr *source, g95_expr *dim,
			g95_expr *ncopies) {

  f->ts.type = source->ts.type;
  f->ts.kind = source->ts.kind;
  f->rank = source->rank + 1;
  f->value.function.name =
    g95_get_string(PREFIX "spread_%c%d", g95_type_letter(source->ts.type),
		   source->ts.kind);
}


void g95_resolve_sqrt(g95_expr *f, g95_expr *x) {

  f->ts = x->ts;
  f->value.function.name =
    g95_get_string(PREFIX "sqrt_%c%d", g95_type_letter(x->ts.type),
		   x->ts.kind);
}


void g95_resolve_sum(g95_expr *f, g95_expr *array, g95_expr *dim,
		     g95_expr *mask) {

  f->ts = array->ts;

  if (dim != NULL && array->rank != 1) f->rank = array->rank - 1;

  f->value.function.name =
    g95_get_string(PREFIX "%s_%c%d", mask ? "msum" : "sum",
                   g95_type_letter(array->ts.type), array->ts.kind);
}


void g95_resolve_tan(g95_expr *f, g95_expr *x) {

  f->ts = x->ts;
  f->value.function.name =
    g95_get_string(PREFIX "tan_%c%d", g95_type_letter(x->ts.type), x->ts.kind);
}


void g95_resolve_tanh(g95_expr *f, g95_expr *x) {

  f->ts = x->ts;
  f->value.function.name =
    g95_get_string(PREFIX "tanh_%c%d", g95_type_letter(x->ts.type),
		   x->ts.kind);
}


void g95_resolve_transfer(g95_expr *f, g95_expr *source, g95_expr *mold,
			                                 g95_expr *size) {
static char transfer0[] = PREFIX "transfer0", transfer1[] = PREFIX "transfer1";

  f->ts = mold->ts;

  if (size == NULL && mold->rank == 0) {
    f->rank = 0;
    f->value.function.name = transfer0;
  } else {
    f->rank = 1;
    f->value.function.name = transfer1;
  }
}


void g95_resolve_transpose(g95_expr *f, g95_expr *matrix) {

  f->ts = matrix->ts;
  f->rank = 2;

  f->value.function.name =
    g95_get_string(PREFIX "transpose_%c%d", g95_type_letter(matrix->ts.type),
		   matrix->ts.kind);
}


void g95_resolve_trim(g95_expr *f, g95_expr *string) {

  f->ts.type = BT_CHARACTER;
  f->ts.kind = string->ts.kind;
  f->value.function.name = g95_get_string(PREFIX "trim_%d", string->ts.kind);
}


void g95_resolve_ubound(g95_expr *f, g95_expr *array, g95_expr *dim) {
static char ubound0[] = PREFIX "ubound0", ubound1[] = PREFIX "ubound1";

  f->ts.type = BT_INTEGER;
  f->ts.kind = g95_default_integer_kind();

  if (dim != NULL)
    f->value.function.name = ubound0;
  else {
    f->value.function.name = ubound1;
    f->rank = 1;
  }
}


void g95_resolve_unpack(g95_expr *f, g95_expr *vector, g95_expr *mask,
			                              g95_expr *field) {

  f->ts.type = vector->ts.type;
  f->ts.kind = vector->ts.kind;
  f->rank = mask->rank;

  f->value.function.name =
    g95_get_string(PREFIX "unpack_%c%d", g95_type_letter(vector->ts.type),
		   vector->ts.kind);
}


void g95_resolve_verify(g95_expr *f, g95_expr *string, g95_expr *set, 
	                                       	     g95_expr *back) {

  f->ts.type = BT_INTEGER;
  f->ts.kind = g95_default_integer_kind();
  f->value.function.name = g95_get_string(PREFIX "verify_%d", string->ts.kind);
}


/* Intrinsic subroutine resolution.  */

void g95_resolve_cpu_time(g95_code *c) {

  c->sub_name = g95_get_string(PREFIX "cpu_time_%d",
			       c->ext.actual->expr->ts.kind);
}


void g95_resolve_random_number(g95_code *c) {
int kind;

  kind = c->ext.actual->expr->ts.kind;

  c->sub_name = g95_get_string((c->ext.actual->expr->rank == 0) ?
			       PREFIX "random_%d" : PREFIX "arandom_%d", kind);
}


void g95_iresolve_init_1(void) {
int i;

  for(i=0; i<HASH_SIZE; i++)
    string_head[i] = NULL;
}


void g95_iresolve_done_1(void) {

  free_strings();
}
