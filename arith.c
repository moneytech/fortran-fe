/* Compiler arithmetic
   Copyright (C) 2000 Free Software Foundation, Inc.
   Contributed by Andy Vaught

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

/* arith.c-- Since target arithmetic must be done on the host, there
 * has to be some way of evaluating arithmetic expressions as the host
 * would evaluate them.  We use the Gnu MP library to do arithmetic,
 * and this file provides the interface. */

#include "g95.h"
#include <string.h>

/* The g95_integer_kinds[] structure has everything the front end
 * needs to know about integers on the target.  The other members of
 * the structure are calculated.  The first entry is the default kind,
 * the second entry of the real structure is the default double kind. */

struct {
  int kind;
  char *max;
  int range;
  mpz_t maxval, minval; } g95_integer_kinds[] = {
    { 4, "2147483647", 9 },   /* Default kind is first */
    { 8, "9223372036854775807", 18 },
    { 2, "32767", 4 }, 
    { 1, "127", 2 },
    { 0, NULL, 0 } };

struct {
  int kind;
  char *max, *eps;
  int precision, range; /* decimal digits, decimal exponent range */
  mpf_t maxval, epsilon; } g95_real_kinds[] = {
    /*   max = 2**(128) - 2**(104),  eps = 2**(-149)           */
    { 4, "3.40282346638528860e+38",  "1.40129846432481707e-45", 6, 37 },
    /*   max = 2**(1024) - 2**(971), eps = 2**(-1074)          */
    { 8, "1.79769313486231571e+308", "4.94065645841246544e-324", 15, 307 },
    { 0, NULL, NULL } };

/* g95_arith_error()-- Given an arithmetic error code, return a
 * pointer to a string that explains the error. */

char *g95_arith_error(arith code) {
char *p;

  switch(code) {
    case ARITH_OK:         p = "Arithmetic OK"; break;
    case ARITH_OVERFLOW:   p = "Arithmetic overflow"; break;
    case ARITH_UNDERFLOW:  p = "Arithmetic underflow"; break;
    case ARITH_DIV0:       p = "Division by zero"; break;
    case ARITH_0TO0:       p = "Indeterminate form 0 ** 0"; break;
    default: g95_internal_error("g95_arith_error(): Bad error code");
  }

  return p;
}


/* g95_arith_init_1()-- Get things ready to do math. */

void g95_arith_init_1(void) {
int i;

/* Convert the minimum/maximum values for each kind into their Gnu MP
 * representation. */

  for(i=0; g95_integer_kinds[i].kind != 0; i++) {
    mpz_init_set_str(g95_integer_kinds[i].maxval,
		     g95_integer_kinds[i].max, 10);

    mpz_init(g95_integer_kinds[i].minval);
    mpz_neg(g95_integer_kinds[i].minval, g95_integer_kinds[i].maxval);
  }

  mpf_set_default_prec(G95_REAL_BITS);

  for(i=0; g95_real_kinds[i].kind != 0; i++) {
    mpf_init_set_str(g95_real_kinds[i].maxval, g95_real_kinds[i].max, 10);
    mpf_init_set_str(g95_real_kinds[i].epsilon, g95_real_kinds[i].eps, 10);
  }
}


/* g95_default_*_kind()-- Return default kinds */

int g95_default_integer_kind(void)   { return g95_integer_kinds[0].kind; }

int g95_default_real_kind(void)      { return g95_real_kinds[0].kind; }

int g95_default_double_kind(void)    { return g95_real_kinds[1].kind; }

int g95_default_character_kind(void) { return 1; }

int g95_default_logical_kind(void)   { return 4; }

int g95_default_complex_kind(void)   { return g95_default_real_kind(); }



/* validate_integer()-- Make sure that a valid kind is present.
 * Returns an index into the g95_integer_kinds array, -1 if the kind
 * is not present. */

static int validate_integer(int kind) {
int i;

  for(i=0;; i++) {
    if (g95_integer_kinds[i].kind == 0) { i = -1; break; }
    if (g95_integer_kinds[i].kind == kind) break;
  }

  return i;
}


static int validate_real(int kind) {
int i;

  for(i=0;; i++) {
    if (g95_real_kinds[i].kind == 0) { i = -1; break; }
    if (g95_real_kinds[i].kind == kind) break;
  }

  return i;
}


static int validate_logical(int kind) {

  if (kind == 1) return 0;
  return -1;
}


static int validate_character(int kind) {

  if (kind == 1) return 0;
  return -1;
}


/* g95_validate_kind()-- Validate a kind given a basic type.  The
 * return value is the same for the child functions, with -1
 * indicating nonexistence of the type */

int g95_validate_kind(bt type, int kind) {
int rc;

  switch(type) {
  case BT_REAL:     /* Fall through */
  case BT_COMPLEX:    rc = validate_real(kind);      break;
  case BT_INTEGER:    rc = validate_integer(kind);   break;
  case BT_LOGICAL:    rc = validate_logical(kind);   break;
  case BT_CHARACTER:  rc = validate_character(kind); break;

  default:
    g95_internal_error("g95_validate_kind(): Got bad type");
  }

  return rc;
}


/* g95_check_integer_range()-- Given an integer and a kind, make sure
 * that the integer lies within the range of the kind.  Returns
 * ARITH_OK or ARITH_OVERFLOW. */

arith g95_check_integer_range(mpz_t p, int kind) {
int i;

  i = validate_integer(kind);
  if (i == -1) g95_internal_error("g95_check_integer_range(): Bad kind");

  if ((mpz_cmp(p, g95_integer_kinds[i].maxval) == 1) ||
      (mpz_cmp(p, g95_integer_kinds[i].minval) == -1)) return ARITH_OVERFLOW;

  return ARITH_OK;
}


/* g95_check_real_range()-- Given a real and a kind, make sure that
 * the real lies within the range of the kind.  Returns ARITH_OK,
 * ARITH_OVERFLOW or ARITH_UNDERFLOW. */

arith g95_check_real_range(mpf_t p, int kind) {
arith retval;
mpf_t q;
int i;

  mpf_init(q);

  if (mpf_sgn(p) >= 0) mpf_set(q, p);
   else mpf_neg(q, p);

  i = validate_real(kind);
  if (i == -1) g95_internal_error("g95_check_real_range(): Bad kind");

  retval = ARITH_OK;
  if (mpf_sgn(q) == 0) goto done;

  if (mpf_cmp(q, g95_real_kinds[i].maxval) == 1) {
    retval = ARITH_OVERFLOW;
    goto done;
  }

  if (mpf_cmp(q, g95_real_kinds[i].epsilon) == -1) retval = ARITH_UNDERFLOW;

done:
  mpf_clear(q);

  return retval;
}



/* Generic functions that call more specific ones based on the type. */

arith g95_arith_uminus(g95_expr *op1, g95_expr *result) {
arith rc;

  switch(op1->ts.type) {
  case BT_INTEGER:
    mpz_init(result->value.integer);
    mpz_neg(result->value.integer, op1->value.integer);
    rc = ARITH_OK;
    break;

  case BT_REAL:
    mpf_init(result->value.real);
    mpf_neg(result->value.real, op1->value.real);
    rc = ARITH_OK;
    break;

  case BT_COMPLEX:
    mpf_init(result->value.complex.r);
    mpf_neg(result->value.complex.r, op1->value.complex.r);

    mpf_init(result->value.complex.i);
    mpf_neg(result->value.complex.i, op1->value.complex.i);
    break;

  default:
    g95_internal_error("g95_arith_uminus(): Bad basic type");
  }

  return rc;
}


arith g95_arith_plus(g95_expr *op1, g95_expr *op2, g95_expr *result) {
arith rc;

  switch(op1->ts.type) {
  case BT_INTEGER:
    mpz_init(result->value.integer);
    mpz_add(result->value.integer, op1->value.integer, op2->value.integer);
    rc = g95_check_integer_range(result->value.integer, result->ts.kind);
    break;

  case BT_REAL:
    mpf_init(result->value.real);
    mpf_add(result->value.real, op1->value.real, op2->value.real);
    rc = g95_check_real_range(result->value.real, result->ts.kind);
    break;

  case BT_COMPLEX:
    mpf_init(result->value.complex.r);
    mpf_init(result->value.complex.i);

    mpf_add(result->value.complex.r, op1->value.complex.r,
	    op2->value.complex.r);

    mpf_add(result->value.complex.i, op1->value.complex.i,
	    op2->value.complex.i);

    rc = g95_check_real_range(result->value.complex.r, result->ts.kind);
    if (rc == ARITH_OK) 
      rc = g95_check_real_range(result->value.complex.i, result->ts.kind);

    break;

  default:
    g95_internal_error("g95_arith_plus(): Bad basic type");
  }

  return rc;
}


arith g95_arith_minus(g95_expr *op1, g95_expr *op2, g95_expr *result) {
arith rc;

  switch(op1->ts.type) {
  case BT_INTEGER:
    mpz_init(result->value.integer);
    mpz_sub(result->value.integer, op1->value.integer, op2->value.integer);
    rc = g95_check_integer_range(result->value.integer, result->ts.kind);
    break;

  case BT_REAL:
    mpf_init(result->value.real);
    mpf_sub(result->value.real, op1->value.real, op2->value.real);
    rc = g95_check_real_range(result->value.real, result->ts.kind);
    break;

  case BT_COMPLEX:
    mpf_init(result->value.complex.r);
    mpf_init(result->value.complex.i);

    mpf_sub(result->value.complex.r, op1->value.complex.r,
	    op2->value.complex.r);

    mpf_sub(result->value.complex.i, op1->value.complex.i,
	    op2->value.complex.i);

    rc = g95_check_real_range(result->value.complex.r, result->ts.kind);
    if (rc == ARITH_OK)
      rc = g95_check_real_range(result->value.complex.i, result->ts.kind);

    break;

  default:
    g95_internal_error("g95_arith_minus(): Bad basic type");
  }

  return rc;
}


arith g95_arith_times(g95_expr *op1, g95_expr *op2, g95_expr *result) {
mpf_t x, y;
arith rc;

  switch(op1->ts.type) {
  case BT_INTEGER:
    mpz_init(result->value.integer);
    mpz_mul(result->value.integer, op1->value.integer, op2->value.integer);
    rc = g95_check_integer_range(result->value.integer, result->ts.kind);
    break;

  case BT_REAL:
    mpf_init(result->value.real);
    mpf_mul(result->value.real, op1->value.real, op2->value.real);
    rc = g95_check_real_range(result->value.real, result->ts.kind);
    break;

  case BT_COMPLEX:
    mpf_init(result->value.complex.r);
    mpf_init(result->value.complex.i);

    mpf_init(x);
    mpf_init(y);

    mpf_mul(x, op1->value.complex.r, op2->value.complex.r);
    mpf_mul(y, op1->value.complex.i, op2->value.complex.i);
    mpf_sub(result->value.complex.r, x, y);

    mpf_mul(x, op1->value.complex.r, op2->value.complex.i);
    mpf_mul(y, op1->value.complex.i, op2->value.complex.r);
    mpf_add(result->value.complex.i, x, y);

    mpf_clear(x);
    mpf_clear(y);

    rc = g95_check_real_range(result->value.complex.r, result->ts.kind);
    if (rc == ARITH_OK)
      rc = g95_check_real_range(result->value.complex.i, result->ts.kind);

    break;

  default:
    g95_internal_error("g95_arith_times(): Bad basic type");
  }

  return rc;
}


arith g95_arith_divide(g95_expr *op1, g95_expr *op2, g95_expr *result) {
mpf_t x, y, div;
arith rc;

  switch(op1->ts.type) {
  case BT_INTEGER:
    if (mpz_sgn(op2->value.integer) == 0) {
      rc = ARITH_DIV0;
      break;
    }

    mpz_init(result->value.integer);
    mpz_tdiv_q(result->value.integer, op1->value.integer,
	       op2->value.integer);

    rc = g95_check_integer_range(result->value.integer, result->ts.kind);
    break;

  case BT_REAL:
    if (mpf_sgn(op2->value.real) == 0) {
      rc = ARITH_DIV0;
      break;
    }

    mpf_init(result->value.real);
    mpf_div(result->value.real, op1->value.real, op2->value.real);
    rc = g95_check_real_range(result->value.real, result->ts.kind);
    break;

  case BT_COMPLEX:
    if (mpf_sgn(op2->value.complex.r) == 0 &&
	mpf_sgn(op2->value.complex.i) == 0) {
      rc = ARITH_DIV0;
      break;
    }

    mpf_init(result->value.complex.r);
    mpf_init(result->value.complex.i);

    mpf_init(x);
    mpf_init(y);
    mpf_init(div);

    mpf_mul(x, op2->value.complex.r, op2->value.complex.r);
    mpf_mul(y, op2->value.complex.i, op2->value.complex.i);
    mpf_add(div, x, y);

    mpf_mul(x, op1->value.complex.r, op2->value.complex.r);
    mpf_mul(y, op1->value.complex.i, op2->value.complex.i);
    mpf_add(result->value.complex.r, x, y);
    mpf_div(result->value.complex.r, result->value.complex.r, div);

    mpf_mul(x, op1->value.complex.i, op2->value.complex.r);
    mpf_mul(y, op1->value.complex.r, op2->value.complex.i);
    mpf_sub(result->value.complex.r, x, y);
    mpf_div(result->value.complex.r, result->value.complex.r, div);

    mpf_clear(x);
    mpf_clear(y);
    mpf_clear(div);

    break;

  default:
    g95_internal_error("g95_arith_divide(): Bad basic type");
  }

  return rc;
}


/* g95_arith_power()-- Raise a number to an integer power */

arith g95_arith_power(g95_expr *op1, g95_expr *op2, g95_expr *result) {
//g95_expr *prod;
int ipower;
arith rc;

  rc = ARITH_OK;

#if 0
  if (g95_extract_int(op2, &power) != NULL) return ARITH0TO0;  /* Fix */

  if (prod == 0) {
  }

  prod = g95_copy_expr(op1);

  if (prod > 0) {
  }
#endif

  switch(mpz_sgn(op2->value.integer)) {
  case -1:
    mpz_init_set_ui(result->value.integer, 0);
    break;

  case 0:
    if (mpz_sgn(op1->value.integer) == 0) rc = ARITH_0TO0;
    break;

  case 1:   /* Doesn't handle a**b  for 0<=a<=1 and b > 100000 correctly */
    if (mpz_cmp_si(op2->value.integer, 100000) == 1)
      rc = ARITH_OVERFLOW;
    else {
      ipower = mpz_get_si(op2->value.integer);
      mpz_init(result->value.integer);
      mpz_pow_ui(result->value.integer, op1->value.integer, ipower);
      rc = g95_check_integer_range(result->value.integer, result->ts.kind);
    }

    break;
  }

  return rc;
}


/* g95_arith_concat()-- Concatenate two string constants */

arith g95_arith_concat(g95_expr *op1, g95_expr *op2, g95_expr *result) {
int len;

  len = op1->value.character.length + op2->value.character.length;

  result->value.character.string = g95_getmem(len+1);
  result->value.character.length = len;

  memcpy(result->value.character.string, op1->value.character.string,
	 op1->value.character.length);

  memcpy(result->value.character.string + op1->value.character.length,
	 op2->value.character.string, op2->value.character.length);

  result->value.character.string[len] = '\0';

  return ARITH_OK;
}



/* g95_compare_expr()-- Comparison operators.  Assumes that the two
 * expression nodes contain two constants of the same type. */

int g95_compare_expr(g95_expr *op1, g95_expr *op2) {
int rc;

  switch(op1->ts.type) {
  case BT_INTEGER:
    rc = mpz_cmp(op1->value.integer, op2->value.integer);
    break;

  case BT_REAL:
    rc = mpf_cmp(op1->value.real, op2->value.real);
    break;
    
  default: g95_internal_error("g95_compare_expr(): Bad basic type");
  }

  return rc;
}


/* compare_complex()-- Compare a pair of complex numbers.  Naturally,
 * this is only for equality/nonequality. */

static int compare_complex(g95_expr *op1, g95_expr *op2) {

  return (mpf_cmp(op1->value.complex.r, op2->value.complex.r) == 0 &&
	  mpf_cmp(op1->value.complex.i, op2->value.complex.i) == 0);
}


/* Specific comparison subroutines */

void g95_arith_eq(g95_expr *op1, g95_expr *op2, g95_expr *result) {

  result->value.logical = (op1->ts.type == BT_COMPLEX) ?
    compare_complex(op1, op2) : (g95_compare_expr(op1, op2) == 0);
}


void g95_arith_ne(g95_expr *op1, g95_expr *op2, g95_expr *result) {

  result->value.logical = (op1->ts.type == BT_COMPLEX) ?
    !compare_complex(op1, op2) : (g95_compare_expr(op1, op2) != 0);
}


void g95_arith_gt(g95_expr *op1, g95_expr *op2, g95_expr *result) {

  result->value.logical = (g95_compare_expr(op1, op2) > 0);
}


void g95_arith_ge(g95_expr *op1, g95_expr *op2, g95_expr *result) {

  result->value.logical = (g95_compare_expr(op1, op2) >= 0);
}


void g95_arith_lt(g95_expr *op1, g95_expr *op2, g95_expr *result) {

  result->value.logical = (g95_compare_expr(op1, op2) < 0);
}


void g95_arith_le(g95_expr *op1, g95_expr *op2, g95_expr *result) {

  result->value.logical = (g95_compare_expr(op1, op2) <= 0);
}


/* g95_convert_integer()-- Convert an integer string to an expression
 * node */

g95_expr *g95_convert_integer(char *buffer, int kind, int radix) {
g95_expr *e;

  e = g95_get_expr();

  e->expr_type = EXPR_CONSTANT;
  e->rank = 0;
  e->ts.type = BT_INTEGER;
  e->ts.kind = kind;

  mpz_init_set_str(e->value.integer, buffer, radix);

  return e;
}


/* g95_convert_real()-- Convert a real string to an expression node. */

g95_expr *g95_convert_real(char *buffer, int kind) {
g95_expr *e;

  e = g95_get_expr();

  e->expr_type = EXPR_CONSTANT;
  e->rank = 0;
  e->ts.type = BT_REAL;
  e->ts.kind = kind;

  mpf_init_set_str(e->value.real, buffer, 10);

  return e;
}


/* g95_convert_complex()-- Convert a pair of real, constant expression
 * nodes to a single complex expression node. */

g95_expr *g95_convert_complex(g95_expr *real, g95_expr *imag, int kind) {
g95_expr *e;

  e = g95_get_expr();

  e->expr_type = EXPR_CONSTANT;
  e->rank = 0;
  e->ts.type = BT_COMPLEX;
  e->ts.kind = kind;

  mpf_init_set(e->value.complex.r, real->value.real);
  mpf_init_set(e->value.complex.i, imag->value.real);

  return e;
}


/******* Simplification of intrinsic functions with constant arguments *****/

/* g95_int2real()-- Convert default integer to default real */

arith g95_int2real(g95_expr **dest, g95_expr *src) {
g95_expr *e;
arith rv;

  e = g95_get_expr();

  e->expr_type = EXPR_CONSTANT;
  e->where = src->where;

  e->ts.type = BT_REAL;
  e->ts.kind = g95_default_real_kind();

  mpf_init(e->value.real);
  mpf_set_z(e->value.real, src->value.integer);

  rv = g95_check_real_range(e->value.real, e->ts.kind);

  if (rv == ARITH_OK)
    *dest = e;
  else {
    g95_error("Overflow converting INTEGER to REAL at %L", e->where);
    g95_free_expr(e);
  }

  return rv;
}


/* g95_int2complex()-- Convert default integer to default complex */

arith g95_int2complex(g95_expr **dest, g95_expr *src) {
g95_expr *e;
arith rv;

  e = g95_get_expr(); 

  e->expr_type = EXPR_CONSTANT;
  e->where = src->where;

  e->ts.type = BT_COMPLEX;
  e->ts.kind = g95_default_complex_kind();

  mpf_init(e->value.real);
  mpf_set_z(e->value.complex.r, src->value.integer);
  mpf_init_set_ui(e->value.complex.i, 0L);

  rv = g95_check_real_range(e->value.complex.i, e->ts.kind);

  if (rv == ARITH_OK)
    *dest = e;
  else {
    g95_error("Overflow converting INTEGER to COMPLEX at %L", e->where);
    g95_free_expr(e);
  }

  return rv;
}


/* g95_real2int()-- Convert default real to default integer */

arith g95_real2int(g95_expr **dest, g95_expr *src) {
g95_expr *e;
arith rv;

  e = g95_get_expr(); 

  e->expr_type = EXPR_CONSTANT;
  e->where = src->where;

  e->ts.type = BT_INTEGER;
  e->ts.kind = g95_default_integer_kind();

  mpz_init(e->value.integer);
  mpz_set_f(e->value.integer, src->value.real);

  rv = g95_check_integer_range(e->value.integer, e->ts.kind);

  if (rv == ARITH_OK)
    *dest = e;
  else {
    g95_error("Overflow converting REAL to INTEGER at %L", e->where);
    g95_free_expr(e);
  }

  return rv;
}


/* g95_real2complex()-- Convert default real to default complex.
 * Because complex components are real numbers, this can't fail.  */

arith g95_real2complex(g95_expr **dest, g95_expr *src) {
g95_expr *e;

  e = g95_get_expr(); 

  e->expr_type = EXPR_CONSTANT;
  e->where = src->where;

  e->ts.type = BT_COMPLEX;
  e->ts.kind = g95_default_complex_kind();

  mpf_init(e->value.real);
  mpf_init_set(e->value.complex.r, src->value.real);
  mpf_init_set_ui(e->value.complex.i, 0);

  *dest = e;
  return ARITH_OK;
}


/* g95_complex2int()-- Convert default complex to default integer */

arith g95_complex2int(g95_expr **dest, g95_expr *src) {
g95_expr *e;
arith rv;

  e = g95_get_expr(); 

  e->expr_type = EXPR_CONSTANT;
  e->where = src->where;

  e->ts.type = BT_INTEGER;
  e->ts.kind = g95_default_integer_kind();

  mpz_init(e->value.integer);
  mpz_set_f(e->value.integer, src->value.complex.r);

  rv = g95_check_integer_range(e->value.integer, e->ts.kind);

  if (rv == ARITH_OK)
    *dest = e;
  else {
    g95_error("Overflow converting COMPLEX to INTEGER at %L", e->where);
    g95_free_expr(e);
  }

  return rv;
}


/* g95_complex2real()-- Convert default complex to default real.
 * Because complex components are real numbers, this can't fail. */

arith g95_complex2real(g95_expr **dest, g95_expr *src) {
g95_expr *e;

  e = g95_get_expr(); 

  e->expr_type = EXPR_CONSTANT;
  e->where = src->where;

  e->ts.type = BT_REAL;
  e->ts.kind = g95_default_real_kind();

  mpf_init_set(e->value.real, src->value.complex.r);

  *dest = e;
  return ARITH_OK;
}


/* g95_double2real()-- Convert the double kind to default real kind */

arith g95_double2real(g95_expr **dest, g95_expr *src) {
g95_expr *e;
arith rv;

  e = g95_get_expr();

  e->expr_type = EXPR_CONSTANT;
  e->where = src->where;

  e->ts.type = BT_REAL;
  e->ts.kind = g95_default_real_kind();

  mpf_init_set(e->value.real, src->value.real);

  rv = g95_check_real_range(e->value.real, e->ts.kind);

  if (rv == ARITH_OK)
    *dest = e;
  else {
    g95_error("Overflow converting DOUBLE to REAL at %L", e->where);
    g95_free_expr(e);
  }

  return rv;
}


/* g95_real2double()-- Convert the double kind to default real kind */

arith g95_real2double(g95_expr **dest, g95_expr *src) {
g95_expr *e;

  e = g95_get_expr();

  e->expr_type = EXPR_CONSTANT;
  e->where = src->where;

  e->ts.type = BT_REAL;
  e->ts.kind = g95_default_real_kind();

  mpf_init_set(e->value.real, src->value.real);

  return ARITH_OK;
}


#define FIRST_ARG(e) (e->value.function.actual->expr)
#define SECOND_ARG(e) (e->value.function.actual->next->expr)

try g95_simplify_selected_int_kind(g95_expr *e) {
int i, kind, range;
g95_expr *arg;

  arg = FIRST_ARG(e);

  if (arg->expr_type != EXPR_CONSTANT) return FAILURE;

  if (g95_extract_int(arg, &range) != NULL) return FAILURE;

  kind = INT_MAX;
  for (i=0; g95_integer_kinds[i].kind!=0; i++) {
    if (g95_integer_kinds[i].range >= range && 
	g95_integer_kinds[i].kind < kind) {
      kind = g95_integer_kinds[i].kind;
    }
  }

  if (kind == INT_MAX) {
    kind = -1;
    g95_warning("Range %d exceeds all integer kinds at %L", 
		range, &arg->where);
  }

  g95_replace_expr(e, g95_constant_expr(BT_INTEGER, kind, NULL));
  return SUCCESS;
}


try g95_simplify_selected_real_kind(g95_expr *e) {
int range, precision, i, kind, foundprecision, foundrange;
g95_expr *arg1, *arg2;

  arg1 = FIRST_ARG(e);
  arg2 = SECOND_ARG(e);

  if (arg1 == NULL && arg2 == NULL)
    g95_internal_error("SELECTED_REAL_KIND without argument at %L should not "
		       "go through g95_simplify_selected_real_kind",
		       &e->where);

  if (arg1 != NULL) {
    if (arg1->expr_type != EXPR_CONSTANT) return FAILURE;
    precision = mpz_get_si(arg1->value.integer);
  } else
    precision = 0;

  if (arg2 != NULL) {
    if (arg2->expr_type != EXPR_CONSTANT) return FAILURE;
    range = mpz_get_si(arg2->value.integer);
  } else
    range = 0;

  kind = INT_MAX;
  foundprecision = 0;
  foundrange = 0;

  for (i=0; g95_real_kinds[i].kind!=0; i++) {
    if (g95_real_kinds[i].precision >= precision)
      foundprecision = 1;
    if (g95_real_kinds[i].range >= range) 
      foundrange = 1;
    if (g95_real_kinds[i].precision >= precision &&
	g95_real_kinds[i].range >= range &&
	g95_real_kinds[i].kind < kind)
      kind = g95_real_kinds[i].kind;
  }
  
  if (kind == INT_MAX) {
    kind = 0;
    if (!foundprecision) {
      g95_warning("Specified precision %d exceeds all REAL kinds at %L",
		  precision, &arg1->where);
      kind = -1;
    }

    if (!foundrange) {
      g95_warning("Specified exponent range %d exceeds all REAL kinds at %L", 
		  range, &arg2->where);
      kind -= 2;
    }
  }

  g95_replace_expr(e, g95_constant_expr(BT_INTEGER, kind, NULL));
  return SUCCESS;
}
