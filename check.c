/* Check functions
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


/* check.c-- check that actual arguments are consistent with an
 * intrinsic interface. */

#include <stdarg.h>

#include "g95.h"
#include "intrinsic.h"


extern int g95_intrinsic_extension;


/***** Functions to store error messages with reason for failure of 
 *      intrinsic resolution                                      ***/
/* TODO Make these better, add more */

/* intrinsic_error()-- write an error message into static memory in
 * case a caller is interested in why something failed. */

static void intrinsic_error(const char *format, ...) {
va_list argp;

  va_start(argp, format);
  vsprintf(g95_intrinsic_diagnostic, format, argp);
  va_end(argp);
}


static void type_error(g95_expr *arg) {
  intrinsic_error("Incorrect argument type in call to intrinsic at %%L",
		  &arg->where);
}

static void kind_error(g95_expr *arg) {
  intrinsic_error("Incorrect argument kind in call to intrinsic at %%L",
		  &arg->where);
}


/***** Functions to check specific arguments *****/

static try check_arg_dim(g95_expr *arg, g95_expr *dim, int optional) {

  if (optional) {
    if (dim == NULL) return SUCCESS;

    if (dim->expr_type == EXPR_VARIABLE && dim->symbol->attr.optional)
      return FAILURE;
  }

  if (dim == NULL) {
    intrinsic_error("Missing DIM parameter at %%L",&arg->where);
    return FAILURE;
  }

  if (dim->ts.type != BT_INTEGER) {
    intrinsic_error("DIM parameter at %%L must be of type integer",
                    &dim->where);
    return FAILURE;
  }

  if (dim->rank != 0) {
    intrinsic_error("DIM parameter at %%L must be of scalar type",
                    &dim->where);
    return FAILURE;
  }

  return SUCCESS;
}


/***** Check functions *****/

try g95_check_all_any(g95_expr *mask, g95_expr *dim) {

  if (mask->ts.type != BT_LOGICAL || mask->rank == 0) return FAILURE;

  if (check_arg_dim(mask, dim, 1) == FAILURE) return FAILURE;

  return SUCCESS;
}


try g95_check_allocated(g95_expr *array) {

  if (array->expr_type != EXPR_VARIABLE) return FAILURE;

  if (array->rank == 0) return FAILURE;

  return (array->symbol->attr.allocatable) ? SUCCESS : FAILURE;
}


try g95_check_associated(g95_expr *pointer, g95_expr *target) {
symbol_attribute attr;

  if (pointer->expr_type != EXPR_VARIABLE) return FAILURE;

  attr = g95_variable_attr(pointer, NULL);
  if (!attr.pointer) return FAILURE;

  if (target == NULL) return SUCCESS;

  /* Target argument is optional */

  attr = g95_variable_attr(target, NULL);
  if (!attr.pointer && !attr.target) return FAILURE;

  return SUCCESS;
}


try g95_check_aint(g95_expr *a, g95_expr *kind) {

  if (a->ts.type != BT_REAL) {
    type_error(a);
    return FAILURE;
  }

  if (kind != NULL &&
      (kind->ts.type != BT_INTEGER || kind->expr_type != EXPR_CONSTANT)) {
    kind_error(kind);
    return FAILURE;
  }

  return SUCCESS;
}


try g95_check_anint(g95_expr *a, g95_expr *kind) {

  if (a->ts.type != BT_REAL) {
    type_error(a);
    return FAILURE;
  }

  if (kind != NULL &&
      (kind->ts.type != BT_INTEGER || kind->expr_type != EXPR_CONSTANT)) {
    kind_error(kind);
    return FAILURE;
  }

  return SUCCESS;
}

/* Atan2 family */

try g95_check_atan2(g95_expr *y, g95_expr *x) {

  if (x == NULL) {
    intrinsic_error("Second argument missing at %%L");
    return FAILURE;
  }

  if (x->ts.type != BT_REAL) {
    type_error(x);
    return FAILURE;
  }

  if (y->ts.type != BT_REAL) {
    type_error(y);
    return FAILURE;
  }

  if (x->ts.kind != y->ts.kind) {
    intrinsic_error("Kinds of arguments to intrinsic at %%L must agree");
    return FAILURE;
  }

  return SUCCESS;
}

try g95_check_datan2(g95_expr *y, g95_expr *x) {

  if (x == NULL) {
    intrinsic_error("Second argument missing at %%L");
    return FAILURE;
  }

  if (x->ts.type != BT_REAL || x->ts.kind != g95_default_double_kind()) {
    type_error(x);
    return FAILURE;
  }

  if (y->ts.type != BT_REAL || y->ts.kind != g95_default_double_kind()) {
    type_error(y);
    return FAILURE;
  }

  return SUCCESS;

}

/* end atan2 */


try g95_check_ceiling(g95_expr *a, g95_expr *kind) {

  if (a->ts.type != BT_REAL) {
    type_error(a);
    return FAILURE;
  }

  if (kind != NULL &&
      (kind->ts.type != BT_INTEGER || kind->expr_type != EXPR_CONSTANT)) {
    kind_error(kind);
    return FAILURE;
  }

  return SUCCESS;
}


try g95_check_char(g95_expr *a, g95_expr *kind) {

  if (a->ts.type != BT_INTEGER) {
    type_error(a);
    return FAILURE;
  }

  if (kind != NULL &&
      (kind->ts.type != BT_INTEGER || kind->expr_type != EXPR_CONSTANT)) {
    kind_error(kind);
    return FAILURE;
  }

  return SUCCESS;
}


try g95_check_cmplx(g95_expr *x, g95_expr *y, g95_expr *kind) {

  if (!g95_numeric_ts(&x->ts)) {
    type_error(x);
    return FAILURE;
  }

  if (y != NULL) {
    if (!g95_numeric_ts(&y->ts)) {
      type_error(y);
      return FAILURE;
    }
    if (x->ts.type == BT_COMPLEX) {
      intrinsic_error("Second argument to cmplx at %%L must not be present "
		      "if first argument is complex");
      return FAILURE;
    }
  }

  if (kind != NULL &&
      (kind->ts.type != BT_INTEGER || kind->expr_type != EXPR_CONSTANT)) {
    kind_error(kind);
    return FAILURE;
  }

  return SUCCESS;
}


try g95_check_cos(g95_expr *x) {

  if (x->ts.type != BT_REAL && x->ts.type != BT_COMPLEX) {
    type_error(x);
    return FAILURE;
  }

  return SUCCESS;
}


try g95_check_count(g95_expr *mask, g95_expr *dim) {

  if (mask->ts.type != BT_LOGICAL || mask->rank == 0) return FAILURE;

  if (check_arg_dim(mask, dim, 1) == FAILURE) return FAILURE;

  return SUCCESS;
}


try g95_check_cshift(g95_expr *array, g95_expr *shift, g95_expr *dim) {

  if (array->rank == 0) return FAILURE;

  if (array->rank == 1) {
    if (shift->rank != 0) return FAILURE;
  } else {
    /* TODO: more requirements on shift parameter */
  }

  if (check_arg_dim(shift,dim,1) == FAILURE) {
    return FAILURE;
  }

  return SUCCESS;
}


try g95_check_dble(g95_expr *x) {

  if (!g95_numeric_ts(&x->ts)) {
    type_error(x);
    return FAILURE;
  }

  return SUCCESS;
}


try g95_check_digits(g95_expr *x) {

  if (x->ts.type != BT_INTEGER && x->ts.type != BT_REAL) {
    type_error(x);
    return FAILURE;
  }

  g95_intrinsic_extension = 0;

  return SUCCESS;
}


try g95_check_dim(g95_expr *x, g95_expr *y) {

  if (y == NULL) {
    intrinsic_error("Second argument missing at %%L");
    return FAILURE;
  }

  if (x->ts.type != BT_INTEGER && x->ts.type != BT_REAL) {
    type_error(x);
    return FAILURE;
  }

  if (y->ts.type != BT_INTEGER && y->ts.type != BT_REAL) {
    type_error(y);
    return FAILURE;
  }

  if (x->ts.type != y->ts.type) {
    intrinsic_error("Types of arguments to intrinsic at %%L must agree");
    return FAILURE;
  }

  if (x->ts.kind != y->ts.kind) {
    intrinsic_error("Kinds of arguments to intrinsic at %%L must agree");
    return FAILURE;
  }

  return SUCCESS;
}


try g95_check_dot_product(g95_expr *vector_a, g95_expr *vector_b) {

  if ((vector_a->ts.type != BT_LOGICAL || vector_b->ts.type != BT_LOGICAL) &&
      (!g95_numeric_ts(&vector_a->ts) || !g95_numeric_ts(&vector_b->ts)))
    return FAILURE;

  if (vector_a->rank != 1) return FAILURE;
  if (vector_b->rank != 1) return FAILURE;

  return SUCCESS;
}


try g95_check_dprod(g95_expr *x, g95_expr *y) {

  if (y == NULL) {
    intrinsic_error("Second argument missing at %%L");
    return FAILURE;
  }

  if (x->ts.type != BT_REAL) {
    type_error(x);
    return FAILURE;
  }

  if (y->ts.type != BT_REAL) {
    type_error(y);
    return FAILURE;
  }

  if (x->ts.kind != g95_default_real_kind() ||
                y->ts.kind != g95_default_real_kind()) {
    kind_error(x);
    return FAILURE;
  }

  return SUCCESS;
}


try g95_check_epsilon(g95_expr *x) {

  if (x->ts.type != BT_REAL) {
    type_error(x);
    return FAILURE;
  }

  g95_intrinsic_extension = 0;

  return SUCCESS;
}


try g95_check_eoshift(g95_expr *array, g95_expr *shift, g95_expr *boundary,
		      g95_expr *dim) {

  if (array->rank == 0) return FAILURE;

  if (shift->ts.type != BT_INTEGER) return FAILURE;

  if (array->rank == 1) {
    if (shift->rank != 0) return FAILURE;
  } else {
    /* TODO: more weird restrictions on shift */
  }

  if (boundary != NULL) {
    if (!g95_compare_types(&array->ts, &boundary->ts)) return FAILURE;

    /* TODO: more restrictions on boundary */
  }

  if (check_arg_dim(shift,dim,1) == FAILURE) {
    return FAILURE;
  }

  return SUCCESS;
}


try g95_check_exp(g95_expr *x) {

  if (x->ts.type != BT_REAL && x->ts.type != BT_COMPLEX) {
    type_error(x);
    return FAILURE;
  }

  return SUCCESS;
}


try g95_check_floor(g95_expr *a, g95_expr *kind) {

  if (a->ts.type != BT_REAL) {
    type_error(a);
    return FAILURE;
  }

  if (kind != NULL &&
      (kind->ts.type != BT_INTEGER || kind->expr_type != EXPR_CONSTANT)) {
    kind_error(kind);
    return FAILURE;
  }

  return SUCCESS;
}


try g95_check_huge(g95_expr *x) {

  if (x->ts.type != BT_INTEGER && x->ts.type != BT_REAL) {
    type_error(x);
    return FAILURE;
  }

  g95_intrinsic_extension = 0;

  return SUCCESS;
}


try g95_check_iand(g95_expr *i, g95_expr *j) {

  if (j == NULL) {
    intrinsic_error("Second argument missing at %%L");
    return FAILURE;
  }

  if (i->ts.type != BT_INTEGER) {
    type_error(i);
    return FAILURE;
  }

  if (j->ts.type != BT_INTEGER) {
    type_error(j);
    return FAILURE;
  }

  if (i->ts.kind != j->ts.kind) {
    intrinsic_error("Kinds of arguments to intrinsic at %%L must agree");
    return FAILURE;
  }

  return SUCCESS;

}


try g95_check_ibclr(g95_expr *i, g95_expr *j) {

  if (j == NULL) {
    intrinsic_error("Second argument missing at %%L");
    return FAILURE;
  }

  if (i->ts.type != BT_INTEGER) {
    type_error(i);
    return FAILURE;
  }

  if (j->ts.type != BT_INTEGER) {
    type_error(j);
    return FAILURE;
  }

  return SUCCESS;
}


try g95_check_ibits(g95_expr *i, g95_expr *j, g95_expr *k) {

  if (j == NULL) {
    intrinsic_error("Second argument missing at %%L");
    return FAILURE;
  }

  if (k == NULL) {
    intrinsic_error("Third argument missing at %%L");
    return FAILURE;
  }

  if (i->ts.type != BT_INTEGER) {
    type_error(i);
    return FAILURE;
  }

  if (j->ts.type != BT_INTEGER) {
    type_error(j);
    return FAILURE;
  }

  if (k->ts.type != BT_INTEGER) {
    type_error(k);
    return FAILURE;
  }

  return SUCCESS;
}


try g95_check_ibset(g95_expr *i, g95_expr *j) {

  if (j == NULL) {
    intrinsic_error("Second argument missing at %%L");
    return FAILURE;
  }

  if (i->ts.type != BT_INTEGER) {
    type_error(i);
    return FAILURE;
  }

  if (j->ts.type != BT_INTEGER) {
    type_error(j);
    return FAILURE;
  }

  return SUCCESS;
}


try g95_check_ieor(g95_expr *i, g95_expr *j) {

  if (j == NULL) {
    intrinsic_error("Second argument missing at %%L");
    return FAILURE;
  }

  if (i->ts.type != BT_INTEGER) {
    type_error(i);
    return FAILURE;
  }

  if (j->ts.type != BT_INTEGER) {
    type_error(j);
    return FAILURE;
  }

  if (i->ts.kind != j->ts.kind) {
    intrinsic_error("Kinds of arguments to intrinsic at %%L must agree");
    return FAILURE;
  }

  return SUCCESS;
}


try g95_check_index(g95_expr *i, g95_expr *j, g95_expr *k) {

  if (j == NULL) {
    intrinsic_error("Second argument missing at %%L");
    return FAILURE;
  }

  if (i->ts.type != BT_CHARACTER) {
    type_error(i);
    return FAILURE;
  }

  if (j->ts.type != BT_CHARACTER) {
    type_error(j);
    return FAILURE;
  }

  if (k!=NULL && k->ts.type != BT_LOGICAL) {
    type_error(k);
    return FAILURE;
  }

  if (i->ts.kind != j->ts.kind) {
    intrinsic_error("Kinds of arguments to intrinsic at %%L must agree");
    return FAILURE;
  }

  return SUCCESS;
}


try g95_check_int(g95_expr *x, g95_expr *kind) {

  if (!g95_numeric_ts(&x->ts)) {
    type_error(x);
    return FAILURE;
  }

  if (kind != NULL &&
      (kind->ts.type != BT_INTEGER || kind->expr_type != EXPR_CONSTANT)) {
    kind_error(kind);
    return FAILURE;
  }

  return SUCCESS;
}


try g95_check_ior(g95_expr *i, g95_expr *j) {

  if (j == NULL) {
    intrinsic_error("Second argument missing at %%L");
    return FAILURE;
  }

  if (i->ts.type != BT_INTEGER) {
    type_error(i);
    return FAILURE;
  }

  if (j->ts.type != BT_INTEGER) {
    type_error(j);
    return FAILURE;
  }

  if (i->ts.kind != j->ts.kind) {
    intrinsic_error("Kinds of arguments to intrinsic at %%L must agree");
    return FAILURE;
  }

  return SUCCESS;
}


try g95_check_kind(g95_expr *x) {

  if (x->ts.type == BT_DERIVED) return FAILURE;

  g95_intrinsic_extension = 0;

  return SUCCESS;
}


try g95_check_lbound(g95_expr *array, g95_expr *dim) {

  if (array->rank == 0) return FAILURE;

  if (check_arg_dim(array, dim, 1) == FAILURE) return FAILURE;

  return SUCCESS;
}


try g95_check_log(g95_expr *x) {

  if (x->ts.type != BT_REAL && x->ts.type != BT_COMPLEX) {
    type_error(x);
    return FAILURE;
  }

  return SUCCESS;
}


try g95_check_log10(g95_expr *x) {

  if (x->ts.type != BT_REAL) {
    type_error(x);
    return FAILURE;
  }

  return SUCCESS;
}


try g95_check_logical(g95_expr *a, g95_expr *kind) {

  if (a->ts.type != BT_LOGICAL) {
    type_error(a);
    return FAILURE;
  }

  if (kind != NULL &&
      (kind->ts.type != BT_INTEGER || kind->expr_type != EXPR_CONSTANT)) {
    kind_error(kind);
    return FAILURE;
  }

  return SUCCESS;
}


/* Min/max family.  Even though several of the check functions could
 * be combined in various ways, the separate check functions allow us
 * to determine the exact intrinsic from the check function. */

static try check_rest(bt type, int kind, g95_actual_arglist *arg) {
g95_expr *x;
int n;

  n = 0; 

  for(; arg; arg=arg->next, n++) {
    x = arg->expr;
    if (x->ts.type != type || x->ts.kind != kind) {
     type_error(x); 
     return FAILURE;
    }
  }

  if (n < 2) return FAILURE;

  return SUCCESS;
}


try g95_check_min(g95_actual_arglist *arg) {
g95_expr *x;

  if (arg == NULL) return FAILURE;

  x = arg->expr;
  if (!g95_numeric_ts(&x->ts)) return FAILURE;

  return check_rest(x->ts.type, x->ts.kind, arg);
}


try g95_check_max(g95_actual_arglist *arg) {

  return g95_check_min(arg);
}


try g95_check_min0(g95_actual_arglist *arg) {
g95_expr *x;

  if (arg == NULL) return FAILURE; 

  x = arg->expr;
  if (!g95_numeric_ts(&x->ts)) return FAILURE;

  return check_rest(BT_INTEGER, g95_default_integer_kind(), arg);
}


try g95_check_max0(g95_actual_arglist *arg) {

  return g95_check_min0(arg);
}


try g95_check_min1(g95_actual_arglist *arg) {
g95_expr *x;

  if (arg == NULL) return FAILURE;

  x = arg->expr;
  if (!g95_numeric_ts(&x->ts)) return FAILURE;

  return check_rest(BT_REAL, g95_default_real_kind(), arg);
}


try g95_check_max1(g95_actual_arglist *arg) {

  return g95_check_min1(arg);
}


try g95_check_amin0(g95_actual_arglist *arg) {

  if (arg == NULL) return FAILURE; 

  return check_rest(BT_INTEGER, g95_default_integer_kind(), arg);
}


try g95_check_amax0(g95_actual_arglist *arg) {

  return g95_check_amin0(arg);
}


try g95_check_amin1(g95_actual_arglist *arg) {

  if (arg == NULL) return FAILURE; 

  return check_rest(BT_REAL, g95_default_real_kind(), arg);
}


try g95_check_amax1(g95_actual_arglist *arg) {

  return g95_check_amin1(arg);
}


try g95_check_dmin1(g95_actual_arglist *arg) {

  if (arg == NULL) return FAILURE; 

  return check_rest(BT_REAL, g95_default_double_kind(), arg); 
}


try g95_check_dmax1(g95_actual_arglist *arg) {

  return g95_check_dmin1(arg);
}


/* End of min/max family */


try g95_check_min_max_exponent(g95_expr *x) {

  if (x->ts.type != BT_REAL) {
    type_error(x);
    return FAILURE;
  }

  g95_intrinsic_extension = 0;

  return SUCCESS;
}


try g95_check_matmul(g95_expr *matrix_a, g95_expr *matrix_b) {

  if ((matrix_a->ts.type != BT_LOGICAL) && !g95_numeric_ts(&matrix_b->ts)) {
    type_error(matrix_a);
    return FAILURE;
  }

  if ((matrix_b->ts.type != BT_LOGICAL) && !g95_numeric_ts(&matrix_a->ts)) {
    type_error(matrix_b);
    return FAILURE;
  }

  if (matrix_a->rank == 0 || matrix_b->rank == 0) return FAILURE;

  if ((matrix_a->rank != 1 || matrix_b->rank != 2) &&
      (matrix_a->rank != 2 || matrix_b->rank != 1))
    return FAILURE;

  return SUCCESS;
}


try g95_check_maxloc(g95_expr *array, g95_expr *dim, g95_expr *mask) {

  if (array->ts.type != BT_INTEGER && array->ts.type != BT_REAL) {
    type_error(array);
    return FAILURE;
  }

  if (check_arg_dim(array, dim, 0) == FAILURE) return FAILURE;

  if (mask != NULL && (mask->rank != 0 || mask->ts.type != BT_LOGICAL))
    return FAILURE;

  return SUCCESS;
}


try g95_check_maxval(g95_expr *array, g95_expr *dim, g95_expr *mask) {

  if (array->rank == 0) return FAILURE;

  if (array->ts.type != BT_INTEGER && array->ts.type != BT_REAL) {
    type_error(array);
    return FAILURE;
  }

  if (check_arg_dim(array, dim, 1) == FAILURE) return FAILURE;

  if (mask != NULL && (mask->ts.type != BT_LOGICAL || mask->rank == 0))
    return FAILURE;

  return SUCCESS;
}


try g95_check_merge(g95_expr *tsource, g95_expr *fsource, g95_expr *mask) {

  if (!g95_compare_types(&tsource->ts, &fsource->ts)) return FAILURE;

  if (mask->ts.type != BT_LOGICAL) return FAILURE;

  return SUCCESS;
}


try g95_check_minloc(g95_expr *array, g95_expr *dim, g95_expr *mask) {

  if (array->ts.type != BT_INTEGER && array->ts.type != BT_REAL) {
    type_error(array);
    return FAILURE;
  }

  if (check_arg_dim(array, dim, 0) == FAILURE) return FAILURE;

  if (mask != NULL && (mask->rank != 0 || mask->ts.type != BT_LOGICAL))
    return FAILURE;

  return SUCCESS;
}


try g95_check_minval(g95_expr *array, g95_expr *dim, g95_expr *mask) {

  if (array->rank == 0) return FAILURE;

  if (array->ts.type != BT_INTEGER && array->ts.type != BT_REAL) {
    type_error(array);
    return FAILURE;
  }

  if (check_arg_dim(array, dim, 1) == FAILURE) return FAILURE;

  if (mask != NULL && (mask->ts.type != BT_LOGICAL || mask->rank == 0))
    return FAILURE;

  return SUCCESS;
}


try g95_check_mod(g95_expr *a, g95_expr *p) {

  if ((a->ts.type != BT_INTEGER && a->ts.type != BT_REAL)) {
    type_error(a);
    return FAILURE;
  }

  if (a->ts.type != p->ts.type) {
    intrinsic_error("Types of arguments to intrinsic at %%L must agree"); 
    return FAILURE;
  }
  
  if (a->ts.kind != p->ts.kind) {
    intrinsic_error("Kinds of arguments to intrinsic at %%L must agree");
    return FAILURE;
  }

  return SUCCESS;
}


try g95_check_modulo(g95_expr *a, g95_expr *p) {

  if ((a->ts.type != BT_INTEGER && a->ts.type != BT_REAL)) {
    type_error(a);
    return FAILURE;
  }

  if (a->ts.type != p->ts.type) {
    intrinsic_error("Types of arguments to intrinsic at %%L must agree");
    return FAILURE;
  }

  if (a->ts.kind != p->ts.kind) {
    intrinsic_error("Kinds of arguments to intrinsic at %%L must agree");
    return FAILURE;
  }

  return SUCCESS;
}


try g95_check_nearest(g95_expr *x, g95_expr *s) {

  if (x->ts.type != BT_REAL) {
    type_error(x);
    return FAILURE;
  }

  if (s->ts.type != BT_REAL) {
    type_error(s);
    return FAILURE;
  }

  return SUCCESS;
}


try g95_check_nint(g95_expr *x, g95_expr *kind) {

  if (x->ts.type != BT_REAL) {
    type_error(x);
    return FAILURE;
  }

  if (kind != NULL &&
      (kind->ts.type != BT_INTEGER || kind->expr_type != EXPR_CONSTANT)) {
    kind_error(kind);
    return FAILURE;
  }

  return SUCCESS;
}


try g95_check_idnint(g95_expr *x) {

  if (x->ts.type != BT_REAL) {
    type_error(x);
    return FAILURE;
  }

  if (x->ts.kind != g95_default_double_kind()) {
    kind_error(x);
    return FAILURE;
  }

  return SUCCESS;
}


try g95_check_null(g95_expr *mold) {
g95_ref *ref;

  if (mold == NULL) return SUCCESS;

  if (mold->ref == NULL) {
    if (mold->symbol->attr.pointer == 0) return FAILURE;
  } else {
    for(ref=mold->ref; ref->next;)
      ref = ref->next;

    if (ref->component == NULL || ref->component->pointer == 0) return FAILURE;
  }

  return SUCCESS;
}


try g95_check_pack(g95_expr *array, g95_expr *mask, g95_expr *vector) {

  if (array->rank == 0) return FAILURE;

  if (mask->rank == 0 || mask->ts.type != BT_INTEGER) return FAILURE;

  if (vector != NULL) {

    if (!g95_compare_types(&array->ts, &vector->ts)) return FAILURE;

    if (vector->rank != 1) return FAILURE;

    /* TODO: More constraints here */
  }

  return SUCCESS;
}


try g95_check_precision(g95_expr *x) {

  if (x->ts.type != BT_REAL && x->ts.type != BT_COMPLEX) {
    type_error(x);
    return FAILURE;
  }

  g95_intrinsic_extension = 0;

  return SUCCESS;
}


try g95_check_present(g95_expr *a) {

  if (a->expr_type != EXPR_VARIABLE) return FAILURE;

  if (a->symbol->attr.dummy == 0 || a->symbol->attr.optional == 0)
    return FAILURE;

  return SUCCESS;
}


try g95_check_product(g95_expr *array, g95_expr *dim, g95_expr *mask) {

  if (array->rank == 0) return FAILURE;

  if (!g95_numeric_ts(&array->ts)) {
    type_error(array);
    return FAILURE;
  }

  if (check_arg_dim(array, dim, 0) == FAILURE) return FAILURE;

  if (mask != NULL && (mask->ts.type != BT_LOGICAL || mask->rank == 0))
    return FAILURE;

  return SUCCESS;
}


try g95_check_radix(g95_expr *x) {

  if (x->ts.type != BT_INTEGER && x->ts.type != BT_REAL) {
    type_error(x);
    return FAILURE;
  }

  g95_intrinsic_extension = 0;

  return SUCCESS;
}


try g95_check_range(g95_expr *x) {

  if (!g95_numeric_ts(&x->ts)) {
    type_error(x);
    return FAILURE;
  }

  g95_intrinsic_extension = 0;

  return SUCCESS;
}

/* real, float, sngl */
try g95_check_real(g95_expr *a, g95_expr *kind) {

  if (!g95_numeric_ts(&a->ts)) {
    type_error(a);
    return FAILURE;
  }

  if (kind != NULL && (kind->ts.type != BT_INTEGER ||
		       kind->expr_type != EXPR_CONSTANT)) {
    kind_error(kind);
    return FAILURE;
  }

  return SUCCESS;
}


try g95_check_repeat(g95_expr *x, g95_expr *y) {

  if (y == NULL) {
    intrinsic_error("Second argument missing at %%L");
    return FAILURE;
  }

  if (x->ts.type != BT_CHARACTER) {
    type_error(x);
    return FAILURE;
  }

  if (y->ts.type != BT_INTEGER) {
    type_error(y);
    return FAILURE;
  }

  return SUCCESS;
}


try g95_check_reshape(g95_expr *source, g95_expr *shape,
		      g95_expr *pad, g95_expr *order) {

  if (source->rank == 0) return FAILURE;

  if (shape->rank == 0) return FAILURE;

  if (shape->ts.type != BT_INTEGER) {
    type_error(shape);
    return FAILURE;
  }

  if (shape->ts.type != BT_INTEGER) {
    type_error(shape);
    return FAILURE;
  }

  if (shape->rank != 1) return FAILURE;

  if (pad != NULL) {
    if (!g95_compare_types(&source->ts, &pad->ts)) return FAILURE;
    if (pad->rank == 0) return FAILURE;
  }

  if (order != NULL && order->rank == 0) return FAILURE;

  return SUCCESS;
}


try g95_check_scale(g95_expr *x, g95_expr *i) {

  if (x->ts.type != BT_REAL || i->ts.type != BT_INTEGER) return FAILURE;

  return SUCCESS;
}


try g95_check_scan(g95_expr *x, g95_expr *y, g95_expr *z) {

  if (y == NULL) {
    intrinsic_error("Second argument missing at %%L");
    return FAILURE;
  }

  if (x->ts.type != BT_CHARACTER) {
    type_error(x);
    return FAILURE;
  }

  if (y->ts.type != BT_CHARACTER) {
    type_error(y);
    return FAILURE;
  }

  if (z!=NULL && z->ts.type != BT_LOGICAL) {
    type_error(z);
    return FAILURE;
  }

  if (x->ts.kind != y->ts.kind) {
    intrinsic_error("Kinds of arguments to intrinsic at %%L must agree");
    return FAILURE;
  }

  return SUCCESS;
}


try g95_check_selected_real_kind(g95_expr *p, g95_expr *r) {

  if (p == NULL && r == NULL) {
    return FAILURE;
  }

  if (p != NULL && p->ts.type != BT_INTEGER) {
    type_error(p);
    return FAILURE;
  } 

  if (r != NULL && r->ts.type != BT_INTEGER) {
    type_error(r);
    return FAILURE;
  }

  g95_intrinsic_extension = 0;

  return SUCCESS;
}


try g95_check_shape(g95_expr *source) {

  return SUCCESS;
}


try g95_check_sin(g95_expr *x) {

  if (x->ts.type != BT_REAL && x->ts.type != BT_COMPLEX) {
    type_error(x);
    return FAILURE;
  }

  return SUCCESS;
}


try g95_check_size(g95_expr *array, g95_expr *dim) {

  if (array->rank == 0) return FAILURE;

  if (check_arg_dim(array, dim, 1) == FAILURE) return FAILURE;

  return SUCCESS;
}


try g95_check_sign(g95_expr *a, g95_expr *b) {

  if (a->ts.type != BT_INTEGER && a->ts.type != BT_REAL) {
    type_error(a);
    return FAILURE;
  }

  if (b->ts.type != BT_INTEGER && b->ts.type != BT_REAL) {
    type_error(b);
    return FAILURE;
  }

  if (a->ts.type != b->ts.type) {
    intrinsic_error("Types of arguments to intrinsic at %%L must agree");
    return FAILURE;
  }  
  
  if (a->ts.kind != b->ts.kind) {
    intrinsic_error("Kinds of arguments to intrinsic at %%L must agree");
    return FAILURE;
  }

  return SUCCESS;
}


try g95_check_spread(g95_expr *source, g95_expr *dim, g95_expr *ncopies) {

  if (source->rank >= G95_MAX_DIMENSIONS) return FAILURE;

  if (check_arg_dim(source, dim, 0) == FAILURE) return FAILURE;

  if (ncopies->ts.type != BT_INTEGER || ncopies->rank != 0) return FAILURE;

  return SUCCESS;
}


try g95_check_sqrt(g95_expr *x) {

  if (x->ts.type != BT_REAL && x->ts.type != BT_COMPLEX) {
    type_error(x);
    return FAILURE;
  }

  return SUCCESS;
}


try g95_check_sum(g95_expr *array, g95_expr *dim, g95_expr *mask) {

  if (array->rank == 0) return FAILURE;

  if (!g95_numeric_ts(&array->ts)) {
    type_error(array);
    return FAILURE;
  }

  if (check_arg_dim(array, dim, 1) == FAILURE) return FAILURE;

  if (mask != NULL && (mask->ts.type != BT_LOGICAL || mask->rank == 0))
    return FAILURE;

  return SUCCESS;
}

/* Tangent family */

try g95_check_tan(g95_expr *x) {

  if (x->ts.type != BT_REAL && x->ts.type != BT_COMPLEX) {
    type_error(x);
    return FAILURE;
  }

  return SUCCESS;
}


try g95_check_dtan(g95_expr *x) {

  if (x->ts.type != BT_REAL || x->ts.kind != g95_default_double_kind()) {
    type_error(x);
    return FAILURE;
  }

  return SUCCESS;
}
/* end of tangents */


try g95_check_transfer(g95_expr *source, g95_expr *mold, g95_expr *size) {

  if (size != NULL && (size->ts.type != BT_INTEGER || size->rank == 0))
    return FAILURE;

  return SUCCESS;
}


try g95_check_transpose(g95_expr *matrix) {

  if (matrix->rank != 2) return FAILURE;

  return SUCCESS;
}


try g95_check_tiny(g95_expr *x) {

  if (x->ts.type != BT_REAL) {
    type_error(x);
    return FAILURE;
  }

  g95_intrinsic_extension = 0;

  return SUCCESS;
}


try g95_check_ubound(g95_expr *array, g95_expr *dim) {

  if (array->rank == 0) return FAILURE;

  if (check_arg_dim(array, dim, 1) == FAILURE) {
    return FAILURE;
  }

  return SUCCESS;
}


try g95_check_unpack(g95_expr *vector, g95_expr *mask, g95_expr *field) {

  if (vector->rank == 0 || vector->rank != 1) return FAILURE;

  if (mask->rank == 0 || mask->ts.type != BT_LOGICAL) return FAILURE;

  if (!g95_compare_types(&vector->ts, &field->ts)) return FAILURE;

  return SUCCESS;
}


try g95_check_verify(g95_expr *x, g95_expr *y, g95_expr *z) {

  if (y == NULL) {
    intrinsic_error("Second argument missing at %%L");
    return FAILURE;
  }

  if (x->ts.type != BT_CHARACTER) {
    type_error(x);
    return FAILURE;
  }

  if (y->ts.type != BT_CHARACTER) {
    type_error(y);
    return FAILURE;
  }

  if (z!=NULL && z->ts.type != BT_LOGICAL) {
    type_error(z);
    return FAILURE;
  }

  if (x->ts.kind != y->ts.kind) {
    intrinsic_error("Kinds of arguments to intrinsic at %%L must agree");
    return FAILURE;
  }

  return SUCCESS;
}





/************* Check functions for intrinsic subroutines *************/


try g95_check_date_and_time(g95_expr *date, g95_expr *time,
			    g95_expr *zone, g95_expr *values) {

  if (date != NULL && (date->ts.type != BT_CHARACTER || date->rank != 0))
    return FAILURE;

  if (time != NULL && (time->ts.type != BT_CHARACTER || time->rank != 0))
    return FAILURE;

  if (zone != NULL && (zone->ts.type != BT_CHARACTER || zone->rank != 0))
    return FAILURE;

  if (values != NULL && (values->ts.type != BT_INTEGER || values->rank == 0))
    return FAILURE;

  return SUCCESS;
}


try g95_check_mvbits(g95_expr *from, g95_expr *frompos, g95_expr *len,
		     g95_expr *to, g95_expr *topos) {

  if (from->ts.type != BT_INTEGER) return FAILURE;
  if (frompos->ts.type != BT_INTEGER) return FAILURE;

  if (len->ts.type != BT_INTEGER) return FAILURE;
  if (to->ts.type != BT_INTEGER || to->ts.kind != from->ts.kind)
    return FAILURE;

  if (topos->ts.type != BT_INTEGER) return FAILURE;

  return SUCCESS;
}


try g95_check_random_number(g95_expr *size, g95_expr *put, g95_expr *get) {

  if (size->rank != 0 || size->ts.type != BT_INTEGER ||
      size->ts.kind != g95_default_integer_kind()) return FAILURE;

  if (put->rank == 0 || put->ts.type != BT_INTEGER ||
      put->ts.kind != g95_default_integer_kind()) return FAILURE;

  if (get->rank == 0 || get->ts.type != BT_INTEGER ||
      get->ts.kind != g95_default_integer_kind()) return FAILURE;

  return SUCCESS;
}


try g95_check_random_seed(g95_expr *harvest) {

  if (harvest->ts.type != BT_REAL) return FAILURE;

  return SUCCESS;
}

