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


/* iresolve.c-- assign name and types to intrinsic procedures */

#include <string.h>

#include "g95.h"
#include "intrinsic.h"

static char temp_name[30];


/* String Pool.  This is used to provide a stable location for various
 * string constants. */

#define G95_STRINGPOOL_SIZE 800

static char *pool_base, *pool_top;



/* add_string()-- Add a string to the string pool */

static void add_string(char *string) {
int len;

  len = strlen(string);

  if ((pool_top - pool_base) + len + 2 >= G95_STRINGPOOL_SIZE)
    g95_internal_error("add_string(): string pool size too small");

  strcpy(pool_top, string);

  pool_top = strchr(pool_top, '\0') + 1;
  *pool_top = '\0';
}


/* get_string()-- Find a string in the string pool */

static char *get_string(char *string) {
char *p;

  p = pool_base;

  while(*p) {
    if (strcmp(p, string) == 0) return p;
    p = strchr(p, '\0') + 1;
  }

  g95_internal_error("get_string(): string '%s' not found", string);
  return NULL;
}



/********************** Resolution functions **********************/


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


/* dot_name()-- Given a type and kind, return a static pointer to the
 * name of the appropriate dot_product intrinsic. */

static char *dot_name(bt type, int kind) {

  sprintf(temp_name, "__dot_product_%c%d", g95_type_letter(type), kind);
  return temp_name;
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

  f->value.function.name = get_string(dot_name(f->ts.type, f->ts.kind));
}


static char *btest_name(int k1, int k2) {

  sprintf(temp_name, "__btest_%d_%d", k1, k2);
  return temp_name;
}


void g95_resolve_btest(g95_expr *f, g95_expr *i, g95_expr *pos) {

  f->ts.type = BT_LOGICAL;
  f->ts.kind = g95_default_logical_kind();

  f->value.function.name = get_string(btest_name(i->ts.kind, pos->ts.kind));
}


static char *max_name(bt type, int kind) {
static char max0[] = "__max0", amax1[] = "__amax1", dmax1[] = "__dmax1";

  if (type == BT_INTEGER && kind == g95_default_integer_kind()) return max0;
  if (type == BT_REAL && kind == g95_default_real_kind()) return amax1;
  if (type == BT_REAL && kind == g95_default_double_kind()) return dmax1;

  sprintf(temp_name, "__max_%c%d", g95_type_letter(type), kind);
  return temp_name;
}


void g95_resolve_max(g95_expr *f, g95_expr *a1) {

  f->ts = a1->ts;
  f->value.function.name = get_string(max_name(a1->ts.type, a1->ts.kind));
}


static char *maxval_name(bt type, int kind) {

  sprintf(temp_name, "__maxval_%c%d", g95_type_letter(type), kind);
  return temp_name;
}


void g95_resolve_maxval(g95_expr *f, g95_expr *array, g95_expr *dim,
			g95_expr *mask) {

  f->ts = array->ts;

  f->value.function.name =
    get_string(maxval_name(array->ts.type, array->ts.kind));

  if (dim != NULL && array->rank != 1)
    f->rank = array->rank - 1;
}


static char *min_name(bt type, int kind) {
static char min0[] = "__min0", amin1[] = "__amin1", dmin1[] = "__dmin1";

  if (type == BT_INTEGER && kind == g95_default_integer_kind()) return min0;
  if (type == BT_REAL && kind == g95_default_real_kind()) return amin1;
  if (type == BT_REAL && kind == g95_default_double_kind()) return dmin1;

  sprintf(temp_name, "__min_%c%d", g95_type_letter(type), kind);
  return temp_name;
}


void g95_resolve_min(g95_expr *f, g95_expr *a1) {

  f->ts = a1->ts;
  f->value.function.name = get_string(min_name(a1->ts.type, a1->ts.kind));
}



static char *minval_name(bt type, int kind) {

  sprintf(temp_name, "__minval_%c%d", g95_type_letter(type), kind);
  return temp_name;
}


void g95_resolve_minval(g95_expr *f, g95_expr *array, g95_expr *dim,
			     g95_expr *mask) {
  f->ts = array->ts;

  f->value.function.name =
    get_string(minval_name(array->ts.type, array->ts.kind));

  if (dim != NULL && array->rank != 1)
    f->rank = array->rank - 1;
}



void g95_resolve_reshape(g95_expr *f) {


}



static char *scale_name(int real_kind, int int_kind) {

  sprintf(temp_name, "__scale%d_%d", real_kind, int_kind);
  return temp_name;
}


void g95_resolve_scale(g95_expr *f, g95_expr *x, g95_expr *y) {

  f->ts = x->ts;
  f->value.function.name = get_string(scale_name(x->ts.kind, x->ts.kind));
}


void g95_resolve_shape(g95_expr *f, g95_expr *source) {

  f->ts.type = BT_INTEGER;
  f->ts.kind = g95_default_integer_kind();
  f->rank = 1;
}


static char *sum_name(bt type, int kind) {

  sprintf(temp_name, "__sum_%c%d", g95_type_letter(type), kind);
  return temp_name;
}


void g95_resolve_sum(g95_expr *f, g95_expr *array, g95_expr *dim,
		     g95_expr *mask) {

  f->ts = array->ts;

  if (dim != NULL && array->rank != 1) f->rank = array->rank - 1;

  f->value.function.name =
    get_string(sum_name(array->ts.type, array->ts.kind));
}



void g95_iresolve_init_1(void) {
int i, j, k, ik, rk;

  pool_base = pool_top = g95_getmem(G95_STRINGPOOL_SIZE);
  *pool_base = '\0';

  add_string(dot_name(BT_LOGICAL, g95_default_logical_kind()));

  /* Generate names of integer subroutines */

  for(i=0; g95_integer_kinds[i].kind; i++) {
    k = g95_integer_kinds[i].kind;

    add_string(dot_name(BT_INTEGER, k));
    add_string(sum_name(BT_INTEGER, k));

    add_string(maxval_name(BT_INTEGER, k));
    add_string(minval_name(BT_INTEGER, k));

    add_string(min_name(BT_INTEGER, k));
    add_string(max_name(BT_INTEGER, k));


    for(j=0; g95_integer_kinds[j].kind; j++) {
      ik = g95_integer_kinds[j].kind;

      add_string(btest_name(k, ik));
    }
  }

  /* Generate names of real and complex names */

  for(i=0; g95_real_kinds[i].kind; i++) { 
    k = g95_integer_kinds[i].kind;

    add_string(dot_name(BT_REAL, k));
    add_string(dot_name(BT_COMPLEX, k));

    add_string(sum_name(BT_REAL, k));
    add_string(sum_name(BT_COMPLEX, k));

    add_string(maxval_name(BT_REAL, k));
    add_string(minval_name(BT_REAL, k));

    add_string(min_name(BT_REAL, k));
    add_string(max_name(BT_REAL, k));
  }

  /* Generate a Cartesian product of read and integer kinds */

  for(i=0; g95_real_kinds[i].kind; i++) {
    rk = g95_real_kinds[i].kind;
    for(j=0; g95_integer_kinds[j].kind; j++) {
      ik = g95_integer_kinds[j].kind;

      add_string(scale_name(rk, ik));
    }
  }
}



void g95_iresolve_done_1(void) {

  g95_free(pool_base);
}
