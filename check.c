/* Check functions
   Copyright (C) 2002 Free Software Foundation, Inc.
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


/* check.c-- These functions check to see if an argument list is
 * compatible with a particular intrinsic function or subroutine.
 * Presence of required arguments has already been established, the
 * argument list has been sorted into the right order and has NULL
 * arguments in the correct places for missing optional arguments.  */


#include <stdlib.h>
#include <stdarg.h>

#include "g95.h"
#include "intrinsic.h"


/* must_be()-- The fundamental complaint function of this source file.
 * This function can be called in all kinds of ways. */

static void must_be(g95_expr *e, int n, const char *thing) {

  g95_error("'%s' argument of '%s' intrinsic at %L must be %s",
	    g95_current_intrinsic_arg[n], g95_current_intrinsic, &e->where,
	    thing);
}


/* type_check()-- Check the type of an expression */

static try type_check(g95_expr *e, int n, bt type) {

  if (e->ts.type == type) return SUCCESS;

  must_be(e, n, g95_basic_typename(type));

  return FAILURE;
}


/* numeric_check()-- Check that the expression is a numeric type */

static try numeric_check(g95_expr *e, int n) {

  if (g95_numeric_ts(&e->ts)) return SUCCESS;

  must_be(e, n, "a numeric type");

  return FAILURE;
}


/* int_or_real_check()-- Check that an expression is integer or real */

static try int_or_real_check(g95_expr *e, int n) {

  if (e->ts.type != BT_INTEGER && e->ts.type != BT_REAL) {
    must_be(e, n, "INTEGER or REAL");
    return FAILURE;
  }

  return SUCCESS;
}


/* kind_check()-- that the expression is an optional constant integer
 * and that it specifies a valid kind for that type. */

static try kind_check(g95_expr *k, int n, bt type) {
int kind;

  if (k == NULL) return SUCCESS;

  if (type_check(k, n, BT_INTEGER) == FAILURE) return FAILURE;

  if (k->expr_type != EXPR_CONSTANT) {
    must_be(k, n, "a constant");
    return FAILURE;
  }

  if (g95_extract_int(k, &kind) != NULL ||
      g95_validate_kind(type, kind) == -1) {
    g95_error("Invalid kind for %s at %L", g95_basic_typename(type),
	      &k->where);
    return FAILURE;
  }

  return SUCCESS;
}


/* double_check()-- Make sure the expression is a double precision real */

static try double_check(g95_expr *d, int n) {

  if (type_check(d, n, BT_REAL) == FAILURE) return FAILURE;

  if (d->ts.kind != g95_default_double_kind()) {
    must_be(d, n, "double precision");
    return FAILURE;
  }

  return SUCCESS;
}


/* logical_array_check()-- Make sure the expression is a logical array */

static try logical_array_check(g95_expr *array, int n) {

  if (array->ts.type != BT_LOGICAL || array->rank == 0) {
    must_be(array, 0, "a logical array");
    return FAILURE;
  }

  return SUCCESS;
}


/* array_check()-- Make sure an expression is an array */

static try array_check(g95_expr *e, int n) {

  if (e->rank != 0) return SUCCESS;

  must_be(e, n, "an array");

  return FAILURE;
}


/* scalar_check()-- Make sure an expression is a scalar */

static try scalar_check(g95_expr *e, int n) {

  if (e->rank == 0) return SUCCESS;

  must_be(e, n, "a scalar");

  return FAILURE;
}


/* same_type_check()-- Make sure two expression have the same type */

static try same_type_check(g95_expr *e, int n, g95_expr *f, int m) {
char message[100];

  if (g95_compare_types(&e->ts, &f->ts)) return SUCCESS;

  sprintf(message, "the same type and kind as '%s'",
	  g95_current_intrinsic_arg[n]);

  must_be(f, m, message);

  return FAILURE;
}


/* rank_check()-- Make sure that an expression has a certain (nonzero) rank */

static try rank_check(g95_expr *e, int n, int rank) {
char *message;

  if (e->rank == rank) return SUCCESS;

  message = alloca(100);
  sprintf(message, "of rank %d", rank);

  must_be(e, n, message);

  return FAILURE;
}


/* nonoptional_check()-- Make sure a variable expression is not an
 * optional dummy argument */

static try nonoptional_check(g95_expr *e, int n) {

  if (e->expr_type == EXPR_VARIABLE && e->symbol->attr.optional) {
    g95_error("'%s' argument of '%s' intrinsic at %L must not be OPTIONAL",
	      g95_current_intrinsic_arg[n], g95_current_intrinsic, &e->where);

  }

  /* TODO: Recursive check on nonoptional variables? */

  return SUCCESS;
}


/* kind_value_check()-- Check that an expression has a particular kind */

static try kind_value_check(g95_expr *e, int n, int k) {
char *message;

  if (e->ts.kind == k) return SUCCESS;

  message = alloca(100);
  sprintf(message, "of kind %d", k);

  must_be(e, n, message);
  return FAILURE;
}


/* variable_check()-- Make sure an expression is a variable. */

static try variable_check(g95_expr *e, int n) {

  if ((e->expr_type == EXPR_VARIABLE &&
       e->symbol->attr.flavor != FL_PARAMETER) ||
      (e->expr_type == EXPR_FUNCTION && e->symbol->result == e->symbol))
    return SUCCESS;

  if (e->expr_type == EXPR_VARIABLE && e->symbol->attr.intent == INTENT_IN) {
    g95_error("'%s' argument of '%s' intrinsic at %L cannot be INTENT(IN)",
	      g95_current_intrinsic_arg[n], g95_current_intrinsic, &e->where);
    return FAILURE;
  }

  must_be(e, n, "a variable");

  return FAILURE;
}


/* dim_check()-- Check the common DIM parameter for correctness */

static try dim_check(g95_expr *dim, int n, int optional) {

  if (optional) {
    if (dim == NULL) return SUCCESS;

    if (nonoptional_check(dim, n) == FAILURE) return FAILURE;

    return SUCCESS;
  }

  if (dim == NULL) {
    g95_error("Missing DIM parameter in intrinsic '%s' at %L",
	      g95_current_intrinsic, g95_current_intrinsic_where);
    return FAILURE;
  }

  if (type_check(dim, n, BT_INTEGER) == FAILURE) return FAILURE;

  if (scalar_check(dim, n) == FAILURE) return FAILURE;

  return SUCCESS;
}


/* dim_rank_check()-- If a DIM parameter is a constant, make sure that
 * it is greater than zero and less than the rank of the given
 * array. */

static try dim_rank_check(g95_expr *dim, g95_expr *array) {
g95_array_ref *ar;
int rank;

  if (dim->expr_type != EXPR_CONSTANT) return SUCCESS;

  ar = g95_find_array_ref(array);
  rank = array->rank;
  if (ar->as->type == AS_ASSUMED_SIZE) rank--;

  if (mpz_cmp_ui(dim->value.integer, 1) < 0 ||
      mpz_cmp_ui(dim->value.integer, rank) > 0) {
    g95_error("'dim' argument of '%s' intrinsic at %L is not a valid "
	      "dimension index", &dim->where);

    return FAILURE;
  }

  return SUCCESS;
}


/***** Check functions *****/

try g95_check_abs(g95_expr *a) {

  if (numeric_check(a, 0) == FAILURE) return FAILURE;

  return SUCCESS;
}


/* g95_check_a_kind()-- Check subroutine suitable for aint, anint,
 * ceiling, floor and nint. */

try g95_check_a_kind(g95_expr *a, g95_expr *kind) {

  if (type_check(a, 0, BT_REAL) == FAILURE) return FAILURE;
  if (kind_check(kind, 1, BT_REAL) == FAILURE) return FAILURE;

  return SUCCESS;
}


try g95_check_all_any(g95_expr *mask, g95_expr *dim) {

  if (logical_array_check(mask, 0) == FAILURE) return FAILURE;

  if (dim_check(dim, 1, 1) == FAILURE) return FAILURE;

  return SUCCESS;
}


try g95_check_allocated(g95_expr *array) {

  if (variable_check(array, 0) == FAILURE) return FAILURE;

  if (array_check(array, 0) == FAILURE) return FAILURE;

  if (!array->symbol->attr.allocatable) {
    must_be(array, 0, "ALLOCATABLE");
    return FAILURE;
  }

  return SUCCESS;
}


/* Common check function where the first arugment must be real or
 * integer and the second argument must be the same as the first. */

try g95_check_a_p(g95_expr *a, g95_expr *p) {

  if (int_or_real_check(a, 0) == FAILURE) return FAILURE;

  if (same_type_check(a, 0, p, 1) == FAILURE) return FAILURE;

  return SUCCESS;
}


try g95_check_associated(g95_expr *pointer, g95_expr *target) {
symbol_attribute attr;

  if (variable_check(pointer, 0) == FAILURE) return FAILURE;

  attr = g95_variable_attr(pointer, NULL);
  if (!attr.pointer) {
    must_be(pointer, 0, "a POINTER");
    return FAILURE;
  }

  if (target == NULL) return SUCCESS;

  /* Target argument is optional */

  attr = g95_variable_attr(target, NULL);
  if (!attr.pointer && !attr.target) {
    must_be(target, 1, "a POINTER or a TARGET");
    return FAILURE;
  }

  return SUCCESS;
}


try g95_check_btest(g95_expr *i, g95_expr *pos) {

  if (type_check(i, 0, BT_INTEGER) == FAILURE) return FAILURE;
  if (type_check(pos, 1, BT_INTEGER) == FAILURE) return FAILURE;

  return SUCCESS;
}


try g95_check_char(g95_expr *i, g95_expr *kind) {

  if (type_check(i, 0, BT_INTEGER) == FAILURE) return FAILURE;
  if (kind_check(kind, 1, BT_CHARACTER) == FAILURE) return FAILURE;

  return SUCCESS;
}


try g95_check_cmplx(g95_expr *x, g95_expr *y, g95_expr *kind) {

  if (numeric_check(x, 0) == FAILURE) return FAILURE;

  if (y != NULL) {
    if (numeric_check(y, 1) == FAILURE) return FAILURE;

    if (x->ts.type == BT_COMPLEX) {
      must_be(y, 1, "not be present if 'x' is COMPLEX");
      return FAILURE;
    }
  }

  if (kind_check(kind, 2, BT_COMPLEX) == FAILURE) return FAILURE;

  return SUCCESS;
}


try g95_check_count(g95_expr *mask, g95_expr *dim) {

  if (logical_array_check(mask, 0) == FAILURE) return FAILURE;
  if (dim_check(dim, 1, 1) == FAILURE) return FAILURE;

  return SUCCESS;
}


try g95_check_cshift(g95_expr *array, g95_expr *shift, g95_expr *dim) {

  if (array_check(array, 0) == FAILURE) return FAILURE;

  if (array->rank == 1) {
    if (scalar_check(shift, 1) == FAILURE) return FAILURE;
  } else {
    /* TODO: more requirements on shift parameter */
  }

  if (dim_check(dim, 2, 1) == FAILURE) return FAILURE;

  return SUCCESS;
}


try g95_check_dcmplx(g95_expr *x, g95_expr *y) {

  if (numeric_check(x, 0) == FAILURE) return FAILURE;

  if (y != NULL) {
    if (numeric_check(y, 1) == FAILURE) return FAILURE;

    if (x->ts.type == BT_COMPLEX) {
      must_be(y, 1, "not be present if 'x' is COMPLEX");
      return FAILURE;
    }
  }

  return SUCCESS;
}


try g95_check_dble(g95_expr *x) {

  if (numeric_check(x, 0) == FAILURE) return FAILURE;

  return SUCCESS;
}


try g95_check_digits(g95_expr *x) {

  if (int_or_real_check(x, 0) == FAILURE) return FAILURE;

  g95_intrinsic_extension = 0;

  return SUCCESS;
}


try g95_check_dot_product(g95_expr *vector_a, g95_expr *vector_b) {

  switch(vector_a->ts.type) {
  case BT_LOGICAL:
    if (type_check(vector_b, 1, BT_LOGICAL) == FAILURE) return FAILURE;
    break;

  case BT_INTEGER:
  case BT_REAL:
  case BT_COMPLEX:
    if (numeric_check(vector_b, 1) == FAILURE) return FAILURE;
    break;

  default:
    must_be(vector_a, 0, "numeric or LOGICAL");
    return FAILURE;
  }

  if (rank_check(vector_a, 0, 1) == FAILURE) return FAILURE;

  if (rank_check(vector_b, 1, 1) == FAILURE) return FAILURE;

  return SUCCESS;
}


try g95_check_eoshift(g95_expr *array, g95_expr *shift, g95_expr *boundary,
		      g95_expr *dim) {

  if (array_check(array, 0) == FAILURE) return FAILURE;

  if (type_check(shift, 1, BT_INTEGER) == FAILURE) return FAILURE;

  if (array->rank == 1) {
    if (scalar_check(shift, 2) == FAILURE) return FAILURE;
  } else {
    /* TODO: more weird restrictions on shift */
  }

  if (boundary != NULL) {
    if (same_type_check(array, 0, boundary, 2) == FAILURE) return FAILURE;

    /* TODO: more restrictions on boundary */
  }

  if (dim_check(dim, 1, 1) == FAILURE) return FAILURE;

  return SUCCESS;
}



try g95_check_huge(g95_expr *x) {

  if (int_or_real_check(x, 0) == FAILURE) return FAILURE;

  g95_intrinsic_extension = 0;

  return SUCCESS;
}


/* g95_check_i()-- Check that the single argument is an integer */

try g95_check_i(g95_expr *i) {

  if (type_check(i, 0, BT_INTEGER) == FAILURE) return FAILURE;

  return SUCCESS;
}


try g95_check_iand(g95_expr *i, g95_expr *j) {

  if (type_check(i, 0, BT_INTEGER) == FAILURE ||
      type_check(j, 1, BT_INTEGER) == FAILURE) return FAILURE;

  if (same_type_check(i, 0, j, 1) == FAILURE) return FAILURE;

  return SUCCESS;
}


try g95_check_ibclr(g95_expr *i, g95_expr *pos) {

  if (type_check(i,   0, BT_INTEGER) == FAILURE ||
      type_check(pos, 1, BT_INTEGER) == FAILURE ||
      kind_value_check(pos, 1, g95_default_integer_kind()) == FAILURE)
    return FAILURE;

  return SUCCESS;
}


try g95_check_ibits(g95_expr *i, g95_expr *pos, g95_expr *len) {

  if (type_check(i,   0, BT_INTEGER) == FAILURE ||
      type_check(pos, 1, BT_INTEGER) == FAILURE ||
      kind_value_check(pos, 1, g95_default_integer_kind()) == FAILURE ||
      type_check(len, 2, BT_INTEGER) == FAILURE)
    return FAILURE;

  return SUCCESS;
}


try g95_check_ibset(g95_expr *i, g95_expr *pos) {

  if (type_check(i,   0, BT_INTEGER) == FAILURE ||
      type_check(pos, 1, BT_INTEGER) == FAILURE ||
      kind_value_check(pos, 1, g95_default_integer_kind()) == FAILURE)
    return FAILURE;

  return SUCCESS;
}


try g95_check_idnint(g95_expr *a, g95_expr *kind) {

  if (double_check(a, 0) == FAILURE) return FAILURE;

  if (kind_check(kind, 1, BT_INTEGER) == FAILURE) return FAILURE;

  return SUCCESS;
}


try g95_check_ieor(g95_expr *i, g95_expr *j) {

  if (type_check(i, 0, BT_INTEGER) == FAILURE ||
      type_check(j, 1, BT_INTEGER) == FAILURE) return FAILURE;

  if (same_type_check(i, 0, j, 1) == FAILURE) return FAILURE;

  return SUCCESS;
}


try g95_check_index(g95_expr *string, g95_expr *substring, g95_expr *back) {

  if (type_check(string,    0, BT_CHARACTER) == FAILURE ||
      type_check(substring, 1, BT_CHARACTER) == FAILURE) return FAILURE;


  if (back != NULL && type_check(back, 2, BT_LOGICAL) == FAILURE)
    return FAILURE;

  if (string->ts.kind != substring->ts.kind) {
    must_be(substring, 1, "the same kind as 'string'");
    return FAILURE;
  }

  return SUCCESS;
}


try g95_check_int(g95_expr *x, g95_expr *kind) {

  if (numeric_check(x, 0) == FAILURE ||
      kind_check(kind, 1, BT_INTEGER) == FAILURE) return FAILURE;

  return SUCCESS;
}


try g95_check_ior(g95_expr *i, g95_expr *j) {

  if (type_check(i, 0, BT_INTEGER) == FAILURE ||
      type_check(j, 1, BT_INTEGER) == FAILURE) return FAILURE;

  if (same_type_check(i, 0, j, 1) == FAILURE) return FAILURE;

  return SUCCESS;
}


try g95_check_ishft(g95_expr *i, g95_expr *shift) {

  if (type_check(i, 0, BT_INTEGER) == FAILURE ||
      type_check(shift, 1, BT_INTEGER) == FAILURE) return FAILURE;

  return SUCCESS;
}


try g95_check_ishftc(g95_expr *i, g95_expr *shift, g95_expr *size) {

  if (type_check(i, 0, BT_INTEGER) == FAILURE ||
      type_check(shift, 1, BT_INTEGER) == FAILURE) return FAILURE;

  if (size != NULL &&
      type_check(size, 2, BT_INTEGER) == FAILURE) return FAILURE;

  return SUCCESS;
}


try g95_check_kind(g95_expr *x) {

  if (x->ts.type == BT_DERIVED) {
    must_be(x, 0, "a non-derived type");
    return FAILURE;
  }

  g95_intrinsic_extension = 0;

  return SUCCESS;
}


try g95_check_lbound(g95_expr *array, g95_expr *dim) {

  if (array_check(array, 0) == FAILURE) return FAILURE;
  if (dim_check(dim, 1, 1) == FAILURE) return FAILURE;

  return SUCCESS;
}


try g95_check_logical(g95_expr *a, g95_expr *kind) {

  if (type_check(a, 0, BT_LOGICAL) == FAILURE) return FAILURE;
  if (kind_check(kind, 1, BT_LOGICAL) == FAILURE) return FAILURE;

  return SUCCESS;
}


/* Min/max family.  */

static try min_max_args(g95_actual_arglist *arg) {

  if (arg == NULL || arg->next == NULL) {
    g95_error("Intrinsic '%s' at %L must have at least two arguments",
	      g95_current_intrinsic, g95_current_intrinsic_where);
    return FAILURE;
  }

  return SUCCESS;
}


static try check_rest(bt type, int kind, g95_actual_arglist *arg) {
g95_expr *x;
int n;

  if (min_max_args(arg) == FAILURE) return FAILURE; 

  n = 1;

  for(; arg; arg=arg->next, n++) {
    x = arg->expr;
    if (x->ts.type != type || x->ts.kind != kind) {
      g95_error("'a%d' argument of '%s' intrinsic at %L must be %s(%d)",
		n, g95_current_intrinsic, &x->where,
		g95_basic_typename(type), kind);
      return FAILURE;
    }
  }

  return SUCCESS;
}


try g95_check_min_max(g95_actual_arglist *arg) {
g95_expr *x;

  if (min_max_args(arg) == FAILURE) return FAILURE; 

  x = arg->expr;

  if (x->ts.type != BT_INTEGER && x->ts.type != BT_REAL) {
    g95_error("'a1' argument of '%s' intrinsic at %L must be INTEGER or REAL",
	      g95_current_intrinsic, &x->where);
    return FAILURE;
  }

  return check_rest(x->ts.type, x->ts.kind, arg);
}


try g95_check_min_max_integer(g95_actual_arglist *arg) {

  return check_rest(BT_INTEGER, g95_default_integer_kind(), arg);
}


try g95_check_min_max_real(g95_actual_arglist *arg) {

  return check_rest(BT_REAL, g95_default_real_kind(), arg);
}


try g95_check_min_max_double(g95_actual_arglist *arg) {

  return check_rest(BT_REAL, g95_default_double_kind(), arg);
}


/* End of min/max family */


try g95_check_matmul(g95_expr *matrix_a, g95_expr *matrix_b) {

  if ((matrix_a->ts.type != BT_LOGICAL) && !g95_numeric_ts(&matrix_b->ts)) {
    must_be(matrix_a, 0, "numeric or LOGICAL");
    return FAILURE;
  }

  if ((matrix_b->ts.type != BT_LOGICAL) && !g95_numeric_ts(&matrix_a->ts)) {
    must_be(matrix_b, 0, "numeric or LOGICAL");
    return FAILURE;
  }

  switch(matrix_a->rank) {
  case 1:
    if (rank_check(matrix_b, 1, 2) == FAILURE) return FAILURE;
    break;

  case 2:
    if (matrix_b->rank == 2) break;
    if (rank_check(matrix_b, 1, 1) == FAILURE) return FAILURE;
    break;

  default:
    must_be(matrix_a, 0, "of rank 1 or 2");
    return FAILURE;
  }

  return SUCCESS;
}


/* minloc/maxloc().  Whoever came up with this interface was probably
 * on something.  The possibilities for the occupation of the second
 * and third parameters are:
 *       Arg #2     Arg #3
 *       NULL       NULL
 *       DIM        NULL
 *       MASK       NULL
 *       NULL       MASK             minloc(array, mask=m)
 *       DIM        MASK
 */

try g95_check_minloc_maxloc(g95_expr *array, g95_expr *a2, g95_expr *a3) {

  if (int_or_real_check(array, 0) == FAILURE) return FAILURE;

  if (array_check(array, 0) == FAILURE) return FAILURE;

  if (a3 != NULL) {
    if (logical_array_check(a3, 2) == FAILURE) return FAILURE;

    if (a2 != NULL) {
      if (scalar_check(a2, 1) == FAILURE) return FAILURE;
      if (type_check(a2, 1, BT_INTEGER) == FAILURE) return FAILURE;
    }
  } else {
    if (a2 != NULL) {
      switch(a2->ts.type) {
      case BT_INTEGER:
	if (scalar_check(a2, 1) == FAILURE) return FAILURE;
	break;

      case BT_LOGICAL:  /* The '2' makes the error message correct */
	if (logical_array_check(a2, 2) == FAILURE) return FAILURE;
	break;

      default:
	type_check(a2, 1, BT_INTEGER);  /* Guaranteed to fail */
	return FAILURE;
      }
    }
  }

  return SUCCESS;
}


try g95_check_minval_maxval(g95_expr *array, g95_expr *dim, g95_expr *mask) {

  if (array_check(array, 0) == FAILURE) return FAILURE;

  if (int_or_real_check(array, 0) == FAILURE) return FAILURE;

  if (dim_check(dim, 1, 1) == FAILURE) return FAILURE;

  if (mask != NULL && logical_array_check(mask, 2) == FAILURE) return FAILURE;

  return SUCCESS;
}


try g95_check_merge(g95_expr *tsource, g95_expr *fsource, g95_expr *mask) {

  if (same_type_check(tsource, 0, fsource, 1) == FAILURE) return FAILURE;

  if (type_check(mask, 2, BT_LOGICAL) == FAILURE) return FAILURE;

  return SUCCESS;
}


try g95_check_nearest(g95_expr *x, g95_expr *s) {

  if (type_check(x, 0, BT_REAL) == FAILURE) return FAILURE;

  if (type_check(s, 1, BT_REAL) == FAILURE) return FAILURE;

  return SUCCESS;
}


try g95_check_null(g95_expr *mold) {
symbol_attribute attr;

  if (mold == NULL) return SUCCESS;

  if (variable_check(mold, 0) == FAILURE) return FAILURE;

  attr = g95_variable_attr(mold, NULL);

  if (!attr.pointer) {
    must_be(mold, 0, "a POINTER");
    return FAILURE;
  }

  return SUCCESS;
}


try g95_check_pack(g95_expr *array, g95_expr *mask, g95_expr *vector) {

  if (array_check(array, 0) == FAILURE) return FAILURE;

  if (type_check(mask, 1, BT_LOGICAL) == FAILURE) return FAILURE;

  if (mask->rank != 0 && mask->rank != array->rank) {
    must_be(array, 0, "conformable with 'mask' argument");
    return FAILURE;
  }

  if (vector != NULL) {
    if (same_type_check(array, 0, vector, 2) == FAILURE) return FAILURE;

    if (rank_check(vector, 2, 1) == FAILURE) return FAILURE;

    /* TODO: More constraints here */
  }

  return SUCCESS;
}


try g95_check_precision(g95_expr *x) {

  if (x->ts.type != BT_REAL && x->ts.type != BT_COMPLEX) {
    must_be(x, 0, "of type REAL or COMPLEX");
    return FAILURE;
  }

  g95_intrinsic_extension = 0;

  return SUCCESS;
}


try g95_check_present(g95_expr *a) {

  if (variable_check(a, 0) == FAILURE) return FAILURE;

  if (!a->symbol->attr.dummy) {
    must_be(a, 0, "a dummy variable");
    return FAILURE;
  }

  if (!a->symbol->attr.optional) {
    must_be(a, 0, "an OPTIONAL dummy variable");
    return FAILURE;
  }

  g95_intrinsic_extension = 0;

  return SUCCESS;
}


try g95_check_product(g95_expr *array, g95_expr *dim, g95_expr *mask) {

  if (array_check(array, 0) == FAILURE) return FAILURE;

  if (numeric_check(array, 0) == FAILURE) return FAILURE;

  if (dim_check(dim, 1, 1) == FAILURE) return FAILURE;

  if (mask != NULL && logical_array_check(mask, 2) == FAILURE) return FAILURE;

  return SUCCESS;
}


try g95_check_radix(g95_expr *x) {

  if (int_or_real_check(x, 0) == FAILURE) return FAILURE;

  g95_intrinsic_extension = 0;

  return SUCCESS;
}


try g95_check_range(g95_expr *x) {

  if (numeric_check(x, 0) == FAILURE) return FAILURE;

  g95_intrinsic_extension = 0;

  return SUCCESS;
}


/* real, float, sngl */
try g95_check_real(g95_expr *a, g95_expr *kind) {

  if (numeric_check(a, 0) == FAILURE) return FAILURE;

  if (kind_check(kind, 1, BT_REAL) == FAILURE) return FAILURE;

  return SUCCESS;
}


try g95_check_repeat(g95_expr *x, g95_expr *y) {

  if (type_check(x, 0, BT_CHARACTER) == FAILURE) return FAILURE;

  if (type_check(y, 0, BT_INTEGER) == FAILURE) return FAILURE;

  return SUCCESS;
}


try g95_check_reshape(g95_expr *source, g95_expr *shape,
		      g95_expr *pad, g95_expr *order) {
mpz_t size;
int m;

  if (array_check(source, 0) == FAILURE) return FAILURE;

  if (rank_check(shape, 1, 1) == FAILURE) return FAILURE;

  if (type_check(shape, 1, BT_INTEGER) == FAILURE) return FAILURE;

  if (g95_array_size(shape, &size) != SUCCESS) {
    g95_error("'shape' argument of 'reshape' intrinsic at %L must be an "
	      "array of constant size", &shape->where);
    return FAILURE;
  }

  m = mpz_cmp_ui(size, G95_MAX_DIMENSIONS);
  mpz_clear(size);

  if (m > 0) {
    g95_error("'shape' argument of 'reshape' intrinsic at %L has more than "
	      stringize(G95_MAX_DIMENSIONS) " elements");
    return FAILURE;
  }

  if (pad != NULL) {
    if (same_type_check(source, 0, pad, 2) == FAILURE) return FAILURE;
    if (array_check(pad, 2) == FAILURE) return FAILURE;
  }

  if (order != NULL && array_check(order, 3) == FAILURE) return FAILURE;

  return SUCCESS;
}


try g95_check_scale(g95_expr *x, g95_expr *i) {

  if (type_check(x, 0, BT_REAL) == FAILURE) return FAILURE;

  if (type_check(i, 1, BT_INTEGER) == FAILURE) return FAILURE;

  return SUCCESS;
}


try g95_check_scan(g95_expr *x, g95_expr *y, g95_expr *z) {

  if (type_check(x, 0, BT_CHARACTER) == FAILURE) return FAILURE;

  if (type_check(y, 1, BT_CHARACTER) == FAILURE) return FAILURE;

  if (z != NULL && type_check(z, 2, BT_LOGICAL) == FAILURE) return FAILURE;

  if (same_type_check(x, 0, y, 1) == FAILURE) return FAILURE;

  return SUCCESS;
}


try g95_check_selected_real_kind(g95_expr *p, g95_expr *r) {

  if (p == NULL && r == NULL) {
    g95_error("Missing arguments to %s intrinsic at %L", g95_current_intrinsic,
	      g95_current_intrinsic_where);

    return FAILURE;
  }

  if (p != NULL && type_check(p, 0, BT_INTEGER) == FAILURE) return FAILURE;

  if (r != NULL && type_check(r, 1, BT_INTEGER) == FAILURE) return FAILURE;

  g95_intrinsic_extension = 0;

  return SUCCESS;
}


try g95_check_set_exponent(g95_expr *x, g95_expr *i) {

  if (type_check(x, 0, BT_REAL) == FAILURE) return FAILURE;

  if (type_check(i, 1, BT_INTEGER) == FAILURE) return FAILURE;

  return SUCCESS;
}


try g95_check_shape(g95_expr *source) {
g95_array_ref *ar;

  if (source->rank == 0) return SUCCESS;

  ar = g95_find_array_ref(source);

  if (ar->type == AS_ASSUMED_SIZE) {
    g95_error("'source' argument of 'shape' intrinsic at %L must not be "
	      "an assumed size array", &source->where);
    return FAILURE;
  }

  return SUCCESS;
}


try g95_check_size(g95_expr *array, g95_expr *dim) {

  if (array_check(array, 0) == FAILURE) return FAILURE;

  if (dim != NULL) {
    if (type_check(dim, 1, BT_INTEGER) == FAILURE) return FAILURE;

    if (kind_value_check(dim, 1, g95_default_integer_kind()) == FAILURE)
      return FAILURE;

    if (dim_rank_check(dim, array) == FAILURE) return FAILURE;
  }

  return SUCCESS;
}


try g95_check_sign(g95_expr *a, g95_expr *b) {

  if (int_or_real_check(a, 0) == FAILURE) return FAILURE;

  if (same_type_check(a, 0, b, 1) == FAILURE) return FAILURE;

  return SUCCESS;
}


try g95_check_spread(g95_expr *source, g95_expr *dim, g95_expr *ncopies) {

  if (source->rank >= G95_MAX_DIMENSIONS) {
    must_be(source, 0, "less than rank " stringize(G95_MAX_DIMENSIONS));
    return FAILURE;
  }

  if (dim_check(dim, 1, 0) == FAILURE) return FAILURE;

  if (type_check(ncopies, 2, BT_INTEGER) == FAILURE) return FAILURE;
  if (scalar_check(ncopies, 2) == FAILURE) return FAILURE;

  return SUCCESS;
}


try g95_check_sum(g95_expr *array, g95_expr *dim, g95_expr *mask) {

  if (array_check(array, 0) == FAILURE) return FAILURE;

  if (numeric_check(array, 0) == FAILURE) return FAILURE;

  if (dim_check(dim, 1, 1) == FAILURE) return FAILURE;

  if (mask != NULL && logical_array_check(mask, 2) == FAILURE) return FAILURE;

  return SUCCESS;
}


try g95_check_transfer(g95_expr *source, g95_expr *mold, g95_expr *size) {

  if (size != NULL) {
    if (type_check(size, 2, BT_INTEGER) == FAILURE) return FAILURE;

    if (scalar_check(size, 2) == FAILURE) return FAILURE;

    if (nonoptional_check(size, 2) == FAILURE) return FAILURE;
  }

  return SUCCESS;
}


try g95_check_transpose(g95_expr *matrix) {

  if (rank_check(matrix, 0, 2) == FAILURE) return FAILURE;

  return SUCCESS;
}


try g95_check_ubound(g95_expr *array, g95_expr *dim) {

  if (array_check(array, 0) == FAILURE) return FAILURE;

  if (dim_check(dim, 1, 1) == FAILURE) return FAILURE;

  return SUCCESS;
}


try g95_check_unpack(g95_expr *vector, g95_expr *mask, g95_expr *field) {

  if (rank_check(vector, 0, 1) == FAILURE) return FAILURE;

  if (array_check(mask, 1) == FAILURE) return FAILURE;

  if (type_check(mask, 1, BT_LOGICAL) == FAILURE) return FAILURE;

  if (same_type_check(vector, 0, field, 2) == FAILURE) return FAILURE;

  return SUCCESS;
}


try g95_check_verify(g95_expr *x, g95_expr *y, g95_expr *z) {

  if (type_check(x, 0, BT_CHARACTER) == FAILURE) return FAILURE;

  if (same_type_check(x, 0, y, 1) == FAILURE) return FAILURE;

  if (z != NULL && type_check(z, 2, BT_LOGICAL) == FAILURE) return FAILURE;

  return SUCCESS;
}


/* g95_check_x()-- Common check function for the half a dozen
 * intrinsics that have a single real argument */

try g95_check_x(g95_expr *x) {

  if (type_check(x, 0, BT_REAL) == FAILURE) return FAILURE;

  return SUCCESS;
}

/* g95_check_x_ni()-- Common check functions for numeric inquiry
 * functions that have a single real argument. */

try g95_check_x_ni(g95_expr *x) {

  if (type_check(x, 0, BT_REAL) == FAILURE) return FAILURE;

  g95_intrinsic_extension = 0;

  return SUCCESS;
}


/************* Check functions for intrinsic subroutines *************/


try g95_check_cpu_time(g95_expr *time) {

  if (scalar_check(time, 0) == FAILURE) return FAILURE;

  if (type_check(time, 0, BT_REAL) == FAILURE) return FAILURE;

  if (variable_check(time, 0) == FAILURE) return FAILURE;

  return SUCCESS;
}


try g95_check_date_and_time(g95_expr *date, g95_expr *time,
			    g95_expr *zone, g95_expr *values) {

  if (date != NULL) {
    if (type_check(date, 0, BT_CHARACTER) == FAILURE) return FAILURE;
    if (scalar_check(date, 0) == FAILURE) return FAILURE;
    if (variable_check(date, 0) == FAILURE) return FAILURE;
  }

  if (time != NULL) {
    if (type_check(time, 1, BT_CHARACTER) == FAILURE) return FAILURE;
    if (scalar_check(time, 1) == FAILURE) return FAILURE;
    if (variable_check(time, 1) == FAILURE) return FAILURE;
  }

  if (zone != NULL) {
    if (type_check(zone, 2, BT_CHARACTER) == FAILURE) return FAILURE;
    if (scalar_check(zone, 2) == FAILURE) return FAILURE;
    if (variable_check(zone, 2) == FAILURE) return FAILURE;
  }

  if (values != NULL) {
    if (type_check(values, 3, BT_INTEGER) == FAILURE) return FAILURE;
    if (array_check(values, 3) == FAILURE) return FAILURE;
    if (rank_check(values, 3, 1) == FAILURE) return FAILURE;
    if (variable_check(values, 3) == FAILURE) return FAILURE;
  }

  return SUCCESS;
}


try g95_check_mvbits(g95_expr *from, g95_expr *frompos, g95_expr *len,
		     g95_expr *to, g95_expr *topos) {

  if (type_check(from, 0, BT_INTEGER) == FAILURE) return FAILURE;

  if (type_check(frompos, 1, BT_INTEGER) == FAILURE) return FAILURE;

  if (type_check(len, 2, BT_INTEGER) == FAILURE) return FAILURE;

  if (same_type_check(from, 0, to, 3) == FAILURE) return FAILURE;

  if (variable_check(to, 3) == FAILURE) return FAILURE;

  if (type_check(topos, 4, BT_INTEGER) == FAILURE) return FAILURE;

  return SUCCESS;
}


try g95_check_random_number(g95_expr *harvest) {

  if (type_check(harvest, 0, BT_REAL) == FAILURE) return FAILURE;

  if (variable_check(harvest, 0) == FAILURE) return FAILURE;

  return SUCCESS;
}


try g95_check_random_seed(g95_expr *size, g95_expr *put, g95_expr *get) {

  if (size != NULL) {
    if (scalar_check(size, 0) == FAILURE) return FAILURE;

    if (type_check(size, 0, BT_INTEGER) == FAILURE) return FAILURE;

    if (variable_check(size, 0) == FAILURE) return FAILURE;

    if (kind_value_check(size, 0, g95_default_integer_kind()) == FAILURE)
      return FAILURE;
  }

  if (put != NULL) {
    if (array_check(put, 1) == FAILURE) return FAILURE;
    if (rank_check(put, 1, 1) == FAILURE) return FAILURE;

    if (type_check(put, 1, BT_INTEGER) == FAILURE) return FAILURE;

    if (kind_value_check(put, 1, g95_default_integer_kind()) == FAILURE)
      return FAILURE;
  }

  if (get != NULL) {
    if (array_check(get, 2) == FAILURE) return FAILURE;
    if (rank_check(get, 2, 1) == FAILURE) return FAILURE;

    if (type_check(get, 2, BT_INTEGER) == FAILURE) return FAILURE;

    if (variable_check(get, 2) == FAILURE) return FAILURE;

    if (kind_value_check(get, 2, g95_default_integer_kind()) == FAILURE)
      return FAILURE;
  }

  return SUCCESS;
}
