/* Compiler arithmetic
   Copyright (C) 2000, 2001. 2002 Free Software Foundation, Inc.
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

mpf_t mpf_pi, mpf_hpi, mpf_nhpi, mpf_tpi;

static mpf_t e;


/* The g95_(integer|real)_kinds[] structures have everything the front
 * end needs to know about integers and real numbers on the target.
 * Other entries of the structure are calculated from these values.
 * The first entry is the default kind, the second entry of the real
 * structure is the default double kind. */

g95_integer_info g95_integer_kinds[] = {
  { 4,  2,  31,  32 },
  { 8,  2,  63,  64 },
  { 2,  2,  15,  16 },
  { 1,  2,   7,   8 },
  { 0,  0,   0,   0 }
};

g95_logical_info g95_logical_kinds[] = {
  { 4,  32 },
  { 8,  64 },
  { 0,   0 }
};

g95_real_info g95_real_kinds[] = {
  { 4,  2,  24,  -148,  128 },
  { 8,  2,  53, -1073, 1024 },
  { 0,  0,   0,     0,    0 }
};


/* natural_logarithm()-- Compute a natural logarithm */

void natural_logarithm(mpf_t *arg, mpf_t *result) {
mpf_t x, xp, t, log;
int i, p;

  mpf_init_set(x, *arg);
  mpf_init(t);

  p = 0;

  /* Get the argument into the range 0.5 to 1.5 by successive
   * multiplications or divisions by e. */

  mpf_set_str(t, "0.5", 10);
  while(mpf_cmp(x, t) < 0) {
    mpf_mul(x, x, e);
    p--;
  }

  mpf_set_str(t, "1.5", 10);
  while(mpf_cmp(x, t) > 0) {
    mpf_div(x, x, e);
    p++;
  }

  /* Compute the natural log of x.  We use the series:
   *    ln(x) = (x-1) - (x-1)^/2 + (x-1)^3/3 - (x-1)^4/4 + ...
   *
   * Because we are expanding in powers of (x-1), and 0.5 < x < 1.5,
   * we have -0.5 < (x-1) < 0.5.  Ignoring the harmonic term, this
   * means that each term is at most 1/(2^i), meaning one bit is
   * gained per iteration.
   *
   * Not very efficient, but it doesn't have to be. */

  mpf_sub_ui(x, x, 1);
  mpf_init_set_ui(log, 0);
  mpf_init_set_ui(xp, 1);

  for(i=1; i<G95_REAL_BITS; i++) {
    mpf_mul(xp, xp, x);
    mpf_div_ui(t, xp, i);

    if (i % 2 == 0)
      mpf_sub(log, log, t);
    else
      mpf_add(log, log, t);
  }

  /* Add in the log (e^p) = p */

  if (p < 0)
    mpf_sub_ui(log, log, -p);
  else
    mpf_add_ui(log, log, p);

  mpf_clear(x);
  mpf_clear(xp);
  mpf_clear(t);

  mpf_set(*result, log);
  mpf_clear(log);
}


void common_logarithm(mpf_t *arg, mpf_t *result) {
mpf_t i10, log10;

  natural_logarithm(arg, result);

  mpf_init_set_ui(i10, 10);
  mpf_init(log10);
  natural_logarithm(&i10, &log10);

  mpf_div(*result, *result, log10);
  mpf_clear(i10);
  mpf_clear(log10);
}


void exponential(mpf_t *arg, mpf_t *result) {
mpf_t two, ln2, power, q, r, num, denom, term, x, xp;
int i;
long n;
unsigned long p, mp;

/* We use a reduction of the form
 *   x= Nln2 + r
 *   then obtain exp(r) from the McLaurin series.
 *   exp(x) is then recovered from the identity exp(x) = 2^N*exp(r) */

  mpf_init_set(x, *arg);

  if ( mpf_cmp_ui(x,0) == 0) {
    mpf_set_ui(*result,1);
  }

  else if ( mpf_cmp_ui(x,1) == 0) {
    mpf_set(*result,e);
  }

  else {
    mpf_init_set_ui(two,2);
    mpf_init(ln2);
    mpf_init(q);
    mpf_init(r);
    mpf_init(power);
    mpf_init(num);
    mpf_init(term);

    natural_logarithm(&two, &ln2);

    mpf_div(q,x,ln2);
    mpf_floor(power,q);
    mpf_mul(q,power,ln2);
    mpf_sub(r,x,q);

    mpf_init_set_ui(xp, 1);
    mpf_init_set_ui(num, 1);
    mpf_init_set_ui(denom, 1);

    for(i=1; i<=G95_REAL_BITS+10; i++) {
      mpf_mul(num, num, r);
      mpf_mul_ui(denom,denom,i);
      mpf_div(term,num,denom);
      mpf_add(xp,xp,term);
    }

    /* Reconstruction step */
    n = (long) mpf_get_d(power);

    if ( n > 0 ) {
      p = (unsigned int) n;
      mpf_mul_2exp(*result,xp,p);
    }
    else {
      mp = (unsigned int) (-n);
      mpf_div_2exp(*result,xp,mp);
    }

    mpf_clear(two);
    mpf_clear(ln2);
    mpf_clear(q);
    mpf_clear(r);
    mpf_clear(power);
    mpf_clear(num);
    mpf_clear(denom);
    mpf_clear(term);
    mpf_clear(xp);
  }
}

void sine(mpf_t *arg, mpf_t *result) {
mpf_t factor, q, r, num, denom, term, x, xp;
int i, sign;

/* We use a reduction of the form
 *   x= N*2pi + r
 *   then obtain sin(r) from the McLaurin series. */

  mpf_init_set(x, *arg);

/* Special case (we do not treat multiples of pi due to roundoff issues) */
  if (mpf_cmp_ui(x,0) ==  0) {
    mpf_set_ui(*result,0);
  }

  else {
    mpf_init(q);
    mpf_init(r);
    mpf_init(factor);
    mpf_init(num);
    mpf_init(denom);
    mpf_init(term);

    mpf_div(q, x, mpf_tpi);
    mpf_floor(factor, q);
    mpf_mul(q, factor, mpf_tpi);
    mpf_sub(r, x, q);

    mpf_init_set_ui(xp, 0);
    mpf_init_set_ui(num, 1);
    mpf_init_set_ui(denom, 1);

    sign = -1;
    for(i=1; i<G95_REAL_BITS+10; i++) {
      mpf_mul(num, num, r);
      mpf_mul_ui(denom, denom, i);
      if (i%2 == 0) continue;

      sign = -sign;
      mpf_div(term, num, denom);
      if (sign > 0)
	mpf_add(xp, xp, term);
      else
	mpf_sub(xp,xp,term);
    }

    mpf_set(*result, xp);

    mpf_clear(q);
    mpf_clear(r);
    mpf_clear(factor);
    mpf_clear(num);
    mpf_clear(denom);
    mpf_clear(term);
    mpf_clear(x);
    mpf_clear(xp);
  }
}

void cosine(mpf_t *arg, mpf_t *result) {
mpf_t factor, q, r, num, denom, term, x, xp;
int i, sign;

/* Similar to sine routine */

  mpf_init_set(x, *arg);

/* Special case (we do not treat multiples of pi due to roundoff issues) */
  if (mpf_cmp_ui(x,0) == 0) {
    mpf_set_ui(*result, 1);
  } else {
    mpf_init(q);
    mpf_init(r);
    mpf_init(factor);
    mpf_init(num);
    mpf_init(denom);
    mpf_init(term);

    mpf_div(q, x, mpf_tpi);
    mpf_floor(factor, q);
    mpf_mul(q, factor, mpf_tpi);
    mpf_sub(r, x, q);

    mpf_init_set_ui(xp, 1);
    mpf_init_set_ui(num, 1);
    mpf_init_set_ui(denom, 1);

    sign = 1;
    for(i=1; i<G95_REAL_BITS+10; i++) {
      mpf_mul(num, num, r);
      mpf_mul_ui(denom, denom, i);
      if (i%2 != 0) continue;

      sign = -sign;
      mpf_div(term, num, denom);
      if (sign > 0)
	mpf_add(xp, xp, term);
      else
	mpf_sub(xp, xp, term);
    }
    mpf_set(*result, xp);

    mpf_clear(q);
    mpf_clear(r);
    mpf_clear(factor);
    mpf_clear(num);
    mpf_clear(denom);
    mpf_clear(term);
    mpf_clear(x);
    mpf_clear(xp);
  }
}


void arctangent(mpf_t *arg, mpf_t *result) {
mpf_t absval, convgu, convgl, num, term, x, xp;
int i, sign;

/* Similar to sine routine but requires special handling for x near 1 */

  mpf_init_set(x, *arg);

/* Special cases */
  if (mpf_cmp_ui(x, 0) == 0) {
    mpf_set_ui(*result, 0);
  } else if (mpf_cmp_ui(x,1) == 0) {
    mpf_init(num);
    mpf_div_ui(num, mpf_hpi, 2);
    mpf_set(*result, num);
    mpf_clear(num);
  } else if (mpf_cmp_si(x,-1) == 0) {
    mpf_init(num);
    mpf_div_ui(num, mpf_hpi, 2);
    mpf_neg(*result, num);
    mpf_clear(num);
  } else { /* General cases */

    mpf_init(absval);
    mpf_abs(absval, x);

    mpf_init_set_d(convgu, 1.5);
    mpf_init_set_d(convgl, 0.5);
    mpf_init_set_ui(num, 1);
    mpf_init(term);

    if (mpf_cmp(absval, convgl) < 0) {
      mpf_init_set_ui(xp, 0);
      sign = -1;
      for(i=1; i<G95_REAL_BITS+10; i++) {
        mpf_mul(num, num, absval);
	if (i%2 == 0) continue;

	sign = -sign;
	mpf_div_ui(term, num, i);
	if (sign > 0)
	  mpf_add(xp, xp, term);
	else
	  mpf_sub(xp, xp, term);
      }
    } else if (mpf_cmp(absval, convgu) >= 0) {
      mpf_init_set(xp, mpf_hpi);
      sign = 1;
      for(i=1; i<G95_REAL_BITS+10; i++) {
        mpf_div(num, num, absval);
	if (i%2 == 0) continue;

	sign = -sign;
	mpf_div_ui(term, num, i);
	if (sign > 0)
	  mpf_add(xp, xp, term);
	else
	  mpf_sub(xp, xp, term);
      }
    } else {
      mpf_init_set_ui(xp, 0);

      mpf_sub_ui(num, absval, 1);
      mpf_add_ui(term, absval, 1);
      mpf_div(absval, num, term);

      mpf_set_ui(num, 1);

      sign = -1;
      for(i=1; i<G95_REAL_BITS+10; i++) {
        mpf_mul(num, num, absval);
	if (i%2 == 0) continue;
	sign = -sign;
	mpf_div_ui(term, num, i);
	if (sign > 0)
	  mpf_add(xp, xp, term);
	else
	  mpf_sub(xp, xp, term);
      }

      mpf_div_ui(term, mpf_hpi, 2);
      mpf_add(xp, term, xp);
    }

  /* This makes sure to preserve the identity arctan(-x) = -arctan(x) */
  /* and improves accuracy to boot                                    */

    if (mpf_cmp_ui(x, 0) > 0)
      mpf_set(*result, xp);
    else
      mpf_neg(*result, xp);

    mpf_clear(absval);
    mpf_clear(convgl);
    mpf_clear(convgu);
    mpf_clear(num);
    mpf_clear(term);
    mpf_clear(xp);
  }
  mpf_clear(x);
}


void hypercos(mpf_t *arg, mpf_t *result) {
mpf_t neg, term1, term2, x, xp;

  mpf_init_set(x, *arg);

  mpf_init(neg);
  mpf_init(term1);
  mpf_init(term2);
  mpf_init(xp);

  mpf_neg(neg, x);

  exponential(&x, &term1);
  exponential(&neg, &term2);

  mpf_add(xp, term1, term2);
  mpf_div_ui(*result, xp, 2);

  mpf_clear(neg);
  mpf_clear(term1);
  mpf_clear(term2);
  mpf_clear(x);
  mpf_clear(xp);
}


void hypersine(mpf_t *arg, mpf_t *result) {
mpf_t neg, term1, term2, x, xp;

  mpf_init_set(x, *arg);

  mpf_init(neg);
  mpf_init(term1);
  mpf_init(term2);
  mpf_init(xp);

  mpf_neg(neg, x);

  exponential(&x, &term1);
  exponential(&neg, &term2);

  mpf_sub(xp, term1, term2);
  mpf_div_ui(*result, xp, 2);

  mpf_clear(neg);
  mpf_clear(term1);
  mpf_clear(term2);
  mpf_clear(x);
  mpf_clear(xp);
}


/* g95_arith_error()-- Given an arithmetic error code, return a
 * pointer to a string that explains the error. */

char *g95_arith_error(arith code) {
char *p;

  switch(code) {
  case ARITH_OK:              p = "Arithmetic OK"; break;
  case ARITH_OVERFLOW:        p = "Arithmetic overflow"; break;
  case ARITH_UNDERFLOW:       p = "Arithmetic underflow"; break;
  case ARITH_DIV0:            p = "Division by zero"; break;
  case ARITH_0TO0:            p = "Indeterminate form 0 ** 0"; break;
  case ARITH_INCOMMENSURATE:  p = "Array operands are incommensurate"; break;
  default: g95_internal_error("g95_arith_error(): Bad error code");
  }

  return p;
}


/* g95_arith_init_1()-- Get things ready to do math. */

void g95_arith_init_1(void) {
g95_integer_info *int_info;
g95_real_info *real_info;
mpf_t a, b;
mpz_t r;
int i, n, limit;

/* Set the default precision for GMP computations */
  mpf_set_default_prec(G95_REAL_BITS+30);

/* Calculate e, needed by the natural_logarithm() subroutine. */

  mpf_init(b);
  mpf_init_set_ui(e, 0);
  mpf_init_set_ui(a, 1);

  for(i=1; i<100; i++) {
    mpf_add(e, e, a);
    mpf_div_ui(a, a, i);   /* 1/(i!) */
  }

/* Calculate pi, 2pi, pi/2, and -pi/2, needed for trigonometric functions
 * We use the Bailey, Borwein and Plouffe formula:
 *
 * pi = \sum{n=0}^\infty (1/16)^n [4/(8n+1) - 2/(8n+4) - 1/(8n+5) - 1/(8n+6)]
 *
 * which gives about four bits per iteration.
 */


  mpf_init_set_ui(mpf_pi,0);

  mpf_init(mpf_tpi);
  mpf_init(mpf_hpi);
  mpf_init(mpf_nhpi);

  limit = (G95_REAL_BITS / 4) + 10;  /* (1/16)^n gives 4 bits per iteration */

  for(n=0; n<limit; n++) {
    mpf_set_ui(b, 4);
    mpf_div_ui(b, b, 8*n+1);  /* 4/(8n+1) */

    mpf_set_ui(a, 2);
    mpf_div_ui(a, a, 8*n+4);  /* 2/(8n+4) */
    mpf_sub(b, b, a);

    mpf_set_ui(a, 1);
    mpf_div_ui(a, a, 8*n+5);  /* 1/(8n+5) */
    mpf_sub(b, b, a);

    mpf_set_ui(a, 1);
    mpf_div_ui(a, a, 8*n+6);  /* 1/(8n+6) */
    mpf_sub(b, b, a);

    mpf_set_ui(a, 16);
    mpf_pow_ui(a, a, n);      /* 16^n */

    mpf_div(b, b, a);

    mpf_add(mpf_pi, mpf_pi, b);
  }

  mpf_mul_ui(mpf_tpi,mpf_pi,2);
  mpf_div_ui(mpf_hpi,mpf_pi,2);
  mpf_neg(mpf_nhpi, mpf_hpi);

/* Convert the minimum/maximum values for each kind into their Gnu MP
 * representation. */

  mpz_init(r);

  for(int_info=g95_integer_kinds; int_info->kind != 0; int_info++) {
    /* Huge */

    mpz_set_ui(r, int_info->radix);
    mpz_pow_ui(r, r, int_info->digits);

    mpz_init(int_info->huge);
    mpz_sub_ui(int_info->huge, r, 1);

    /* These are the numbers that are actually representable by the
     * target.  For bases other than two, this needs to be changed. */

    if (int_info->radix != 2)
      g95_internal_error("Fix min_int, max_int calculation");

    mpz_init(int_info->min_int);
    mpz_neg(int_info->min_int, int_info->huge);

    mpz_init(int_info->max_int);
    mpz_add(int_info->max_int, int_info->huge, int_info->huge); 
    mpz_add_ui(int_info->max_int, int_info->max_int, 1);

    /* Range */

    mpf_set_z(a, int_info->huge);
    common_logarithm(&a, &a);
    mpf_trunc(a, a);
    mpz_set_f(r, a);
    int_info->range = mpz_get_si(r);
  }

/*  mpf_set_default_prec(G95_REAL_BITS); */

  for(real_info=g95_real_kinds; real_info->kind != 0; real_info++) {
    /* Huge */

    mpf_set_ui(a, real_info->radix);
    mpf_set_ui(b, real_info->radix);

    mpf_pow_ui(a, a, real_info->max_exponent);
    mpf_pow_ui(b, b, real_info->max_exponent - real_info->digits);

    mpf_init(real_info->huge);
    mpf_sub(real_info->huge, a, b);

    /* Tiny */

    mpf_set_ui(b, real_info->radix);
    mpf_pow_ui(b, b, 1-real_info->min_exponent);

    mpf_init(real_info->tiny);
    mpf_ui_div(real_info->tiny, 1, b);

    /* Epsilon */

    mpf_set_ui(b, real_info->radix);
    mpf_pow_ui(b, b, real_info->digits - 1);

    mpf_init(real_info->epsilon);
    mpf_ui_div(real_info->epsilon, 1, b);

    /* Range */

    common_logarithm(&real_info->huge, &a);
    common_logarithm(&real_info->tiny, &b);
    mpf_neg(b, b);

    if (mpf_cmp(a, b) > 0) mpf_set(a, b);  /* a = min(a, b) */

    mpf_trunc(a, a);
    mpz_set_f(r, a);
    real_info->range = mpz_get_si(r);

    /* Precision */

    mpf_set_ui(a, real_info->radix);
    common_logarithm(&a, &a);

    mpf_mul_ui(a, a, real_info->digits-1);
    mpf_trunc(a, a);
    mpz_set_f(r, a);
    real_info->precision = mpz_get_si(r);

    /* If the radix is an integral power of 10, add one to the precision. */

    for(i=10; i<=real_info->radix; i*=10)
      if (i == real_info->radix) real_info->precision++;
  }

  mpz_clear(r);
  mpf_clear(a);
  mpf_clear(b);
}


/* g95_default_*_kind()-- Return default kinds */

int g95_default_integer_kind(void) {
  return g95_integer_kinds[g95_option.i8 ? 1 : 0].kind;
}

int g95_default_real_kind(void) {
  return g95_real_kinds[g95_option.r8 ? 1 : 0].kind;
}

int g95_default_double_kind(void)    { return g95_real_kinds[1].kind; }

int g95_default_character_kind(void) { return 1; }

int g95_default_logical_kind(void) {
  return g95_logical_kinds[g95_option.i8 ? 1 : 0].kind;
}

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
int i;

  if ( g95_option.l1 == 1 ) {
    if ( kind == 1 ) {
      i = 0;
      return i;
    }
  }

  for(i=0;; i++) {
    if (g95_logical_kinds[i].kind == 0) { i = -1; break; }
    if (g95_logical_kinds[i].kind == kind) break;
  }

  return i;
}


static int validate_character(int kind) {

  if (kind == g95_default_character_kind()) return 0;
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

static arith g95_check_integer_range(mpz_t p, int kind) {
arith result;
int i;

  i = validate_integer(kind);
  if (i == -1) g95_internal_error("g95_check_integer_range(): Bad kind");

  result = ARITH_OK;

  if (mpz_cmp(p, g95_integer_kinds[i].min_int) < 0 ||
      mpz_cmp(p, g95_integer_kinds[i].max_int) > 0) result = ARITH_OVERFLOW;

  return result;
}


/* g95_check_real_range()-- Given a real and a kind, make sure that
 * the real lies within the range of the kind.  Returns ARITH_OK,
 * ARITH_OVERFLOW or ARITH_UNDERFLOW. */

static arith g95_check_real_range(mpf_t p, int kind) {
arith retval;
mpf_t q;
int i;

  mpf_init(q);
  mpf_abs(q, p);

  i = validate_real(kind);
  if (i == -1) g95_internal_error("g95_check_real_range(): Bad kind");

  retval = ARITH_OK;
  if (mpf_sgn(q) == 0) goto done;

  if (mpf_cmp(q, g95_real_kinds[i].huge) == 1) {
    retval = ARITH_OVERFLOW;
    goto done;
  }

  if (mpf_cmp(q, g95_real_kinds[i].tiny) == -1) retval = ARITH_UNDERFLOW;

done:
  mpf_clear(q);

  return retval;
}


/* g95_constant_result()-- Function to return a constant expression node
 * of a given type and kind. */

g95_expr *g95_constant_result(bt type, int kind) {
g95_expr *result;

  result = g95_get_expr();

  result->expr_type = EXPR_CONSTANT;
  result->ts.type = type;
  result->ts.kind = kind;

  switch(type) {
  case BT_INTEGER:
    mpz_init(result->value.integer);
    break;

  case BT_REAL:
    mpf_init(result->value.real);
    break;

  case BT_COMPLEX:
    mpf_init(result->value.complex.r);
    mpf_init(result->value.complex.i);
    break;

  default:
    break;
  }

  return result;
}


/* Low-level arithmetic functions.  All of these subroutines assume
 * that all operands are of the same type and return an operand of the
 * same type.  The other thing about these subroutines is that they
 * can fail in various ways-- overflow, underflow, division by zero,
 * zero raised to the zero, etc.  */

arith g95_arith_not(g95_expr *op1, g95_expr **resultp) {
g95_expr *result;

  result = g95_constant_result(BT_LOGICAL, op1->ts.kind);
  result->value.logical = !op1->value.logical;
  *resultp = result;

  return ARITH_OK;
}


arith g95_arith_and(g95_expr *op1, g95_expr *op2, g95_expr **resultp) {
g95_expr *result;

  result = g95_constant_result(BT_LOGICAL, g95_kind_max(op1, op2));
  result->value.logical = op1->value.logical && op2->value.logical;
  *resultp = result;

  return ARITH_OK;
}


arith g95_arith_or(g95_expr *op1, g95_expr *op2, g95_expr **resultp) {
g95_expr *result;

  result = g95_constant_result(BT_LOGICAL, g95_kind_max(op1, op2));
  result->value.logical = op1->value.logical || op2->value.logical;
  *resultp = result;

  return ARITH_OK;
}


arith g95_arith_eqv(g95_expr *op1, g95_expr *op2, g95_expr **resultp) {
g95_expr *result;

  result = g95_constant_result(BT_LOGICAL, g95_kind_max(op1, op2));
  result->value.logical = op1->value.logical == op2->value.logical;
  *resultp = result;

  return ARITH_OK;
}


arith g95_arith_neqv(g95_expr *op1, g95_expr *op2, g95_expr **resultp) {
g95_expr *result;

  result = g95_constant_result(BT_LOGICAL, g95_kind_max(op1, op2));
  result->value.logical = op1->value.logical != op2->value.logical;
  *resultp = result;

  return ARITH_OK;
}


/* g95_range_check()-- Make sure a constant numeric expression is
 * within the range for it's type and kind.
 * Note that there's also a g95_check_range(), but that one deals
 * with the intrinsic RANGE function. */

arith g95_range_check(g95_expr *e) {
arith rc;

  switch(e->ts.type) {
  case BT_INTEGER:
    rc = g95_check_integer_range(e->value.integer, e->ts.kind);
    break;

  case BT_REAL:
    rc = g95_check_real_range(e->value.real, e->ts.kind);
    break;

  case BT_COMPLEX:
    rc = g95_check_real_range(e->value.complex.r, e->ts.kind);
    if (rc != ARITH_OK)
      rc = g95_check_real_range(e->value.complex.i, e->ts.kind);

    break;

  default:
    g95_internal_error("g95_range_check(): Bad type");
  }

  return rc;
}


/* g95_arith_uplus()-- It may seem silly to have a subroutine that
 * actually computes the unary plus of a constant, but it prevents us
 * from making exceptions in the code elsewhere. */

arith g95_arith_uplus(g95_expr *op1, g95_expr **resultp) {

  *resultp = g95_copy_expr(op1);
  return ARITH_OK;
}


arith g95_arith_uminus(g95_expr *op1, g95_expr **resultp) {
g95_expr *result;
arith rc;

  result = g95_constant_result(op1->ts.type, op1->ts.kind);
  result->where = op1->where;

  switch(op1->ts.type) {
  case BT_INTEGER:
    mpz_neg(result->value.integer, op1->value.integer);
    break;

  case BT_REAL:
    mpf_neg(result->value.real, op1->value.real);
    break;

  case BT_COMPLEX:
    mpf_neg(result->value.complex.r, op1->value.complex.r);
    mpf_neg(result->value.complex.i, op1->value.complex.i);
    break;

  default:
    g95_internal_error("g95_arith_uminus(): Bad basic type");
  }

  rc = g95_range_check(result);

  if (rc != ARITH_OK)
    g95_free_expr(result);
  else
    *resultp = result;

  return rc;
}


arith g95_arith_plus(g95_expr *op1, g95_expr *op2, g95_expr **resultp) {
g95_expr *result;
arith rc;

  result = g95_constant_result(op1->ts.type, op1->ts.kind);
  result->where = op1->where;

  switch(op1->ts.type) {
  case BT_INTEGER:
    mpz_add(result->value.integer, op1->value.integer, op2->value.integer);
    break;

  case BT_REAL:
    mpf_add(result->value.real, op1->value.real, op2->value.real);
    break;

  case BT_COMPLEX:
    mpf_add(result->value.complex.r, op1->value.complex.r,
	    op2->value.complex.r);

    mpf_add(result->value.complex.i, op1->value.complex.i,
	    op2->value.complex.i);
    break;

  default:
    g95_internal_error("g95_arith_plus(): Bad basic type");
  }

  rc = g95_range_check(result);

  if (rc != ARITH_OK)
    g95_free_expr(result);
  else
    *resultp = result;

  return rc;
}


arith g95_arith_minus(g95_expr *op1, g95_expr *op2, g95_expr **resultp) {
g95_expr *result;
arith rc;

  result = g95_constant_result(op1->ts.type, op1->ts.kind);
  result->where = op1->where;

  switch(op1->ts.type) {
  case BT_INTEGER:
    mpz_sub(result->value.integer, op1->value.integer, op2->value.integer);
    break;

  case BT_REAL:
    mpf_sub(result->value.real, op1->value.real, op2->value.real);
    break;

  case BT_COMPLEX:
    mpf_sub(result->value.complex.r, op1->value.complex.r,
	    op2->value.complex.r);

    mpf_sub(result->value.complex.i, op1->value.complex.i,
	    op2->value.complex.i);

    break;

  default:
    g95_internal_error("g95_arith_minus(): Bad basic type");
  }

  rc = g95_range_check(result);

  if (rc != ARITH_OK)
    g95_free_expr(result);
  else
    *resultp = result;

  return rc;
}


arith g95_arith_times(g95_expr *op1, g95_expr *op2, g95_expr **resultp) {
g95_expr *result;
mpf_t x, y;
arith rc;

  result = g95_constant_result(op1->ts.type, op1->ts.kind);
  result->where = op1->where;

  switch(op1->ts.type) {
  case BT_INTEGER:
    mpz_mul(result->value.integer, op1->value.integer, op2->value.integer);
    break;

  case BT_REAL:
    mpf_mul(result->value.real, op1->value.real, op2->value.real);
    break;

  case BT_COMPLEX:
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

    break;

  default:
    g95_internal_error("g95_arith_times(): Bad basic type");
  }

  rc = g95_range_check(result);

  if (rc != ARITH_OK)
    g95_free_expr(result);
  else
    *resultp = result;

  return rc;
}


arith g95_arith_divide(g95_expr *op1, g95_expr *op2, g95_expr **resultp) {
g95_expr *result;
mpf_t x, y, div;
arith rc;

  rc = ARITH_OK;

  result = g95_constant_result(op1->ts.type, op1->ts.kind);
  result->where = op1->where;

  switch(op1->ts.type) {
  case BT_INTEGER:
    if (mpz_sgn(op2->value.integer) == 0) {
      rc = ARITH_DIV0;
      break;
    }

    mpz_tdiv_q(result->value.integer, op1->value.integer,
	       op2->value.integer);
    break;

  case BT_REAL:
    if (mpf_sgn(op2->value.real) == 0) {
      rc = ARITH_DIV0;
      break;
    }

    mpf_div(result->value.real, op1->value.real, op2->value.real);
    break;

  case BT_COMPLEX:
    if (mpf_sgn(op2->value.complex.r) == 0 &&
	mpf_sgn(op2->value.complex.i) == 0) {
      rc = ARITH_DIV0;
      break;
    }

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

  if (rc == ARITH_OK) rc = g95_range_check(result);

  if (rc != ARITH_OK)
    g95_free_expr(result);
  else
    *resultp = result;

  return rc;
}


/* complex_reciprocal()-- Compute the reciprocal of a complex number
 * (guaranteed nonzero) */

static void complex_reciprocal(g95_expr *op) {
mpf_t mod, a, result_r, result_i;

  mpf_init(mod);
  mpf_init(a);

  mpf_mul(mod, op->value.complex.r, op->value.complex.r);
  mpf_mul(a, op->value.complex.i, op->value.complex.i);
  mpf_add(mod, mod, a);

  mpf_init(result_r);
  mpf_div(result_r, op->value.complex.r, mod);

  mpf_init(result_i);
  mpf_neg(result_i, op->value.complex.i);
  mpf_div(result_i, result_i, mod);

  mpf_set(op->value.complex.r, result_r);
  mpf_set(op->value.complex.i, result_i);

  mpf_clear(result_r);
  mpf_clear(result_i);

  mpf_clear(mod);
  mpf_clear(a);
}


/* complex_pow_ui()-- Raise a complex number to positive power */

static void complex_pow_ui(g95_expr *base, int power, g95_expr *result) {
mpf_t temp_r, temp_i, a;

  mpf_set_ui(result->value.complex.r, 1);
  mpf_set_ui(result->value.complex.i, 0);

  mpf_init(temp_r);
  mpf_init(temp_i);
  mpf_init(a);

  for(; power>0; power--) {
    mpf_mul(temp_r, base->value.complex.r, result->value.complex.r);
    mpf_mul(a,      base->value.complex.i, result->value.complex.i);
    mpf_sub(temp_r, temp_r, a);

    mpf_mul(temp_i, base->value.complex.r, result->value.complex.i);
    mpf_mul(a,      base->value.complex.i, result->value.complex.r);
    mpf_add(temp_i, temp_i, a);

    mpf_set(result->value.complex.r, temp_r);
    mpf_set(result->value.complex.i, temp_i);
  }

  mpf_clear(temp_r);
  mpf_clear(temp_i);
  mpf_clear(a);
}


/* g95_arith_power()-- Raise a number to an integer power */

arith g95_arith_power(g95_expr *op1, g95_expr *op2, g95_expr **resultp) {
int power, apower;
g95_expr *result;
mpz_t unity_z;
mpf_t unity_f;
arith rc;

  rc = ARITH_OK;

  if (g95_extract_int(op2, &power) != NULL)
    g95_internal_error("g95_arith_power(): Bad exponent");

  result = g95_constant_result(op1->ts.type, op1->ts.kind);
  result->where = op1->where;

  if (power == 0) {     /* Handle something to the zeroth power */
    switch(op1->ts.type) {
    case BT_INTEGER:
      if (mpz_sgn(op1->value.integer) == 0)
	rc = ARITH_0TO0;
      else
	mpz_set_ui(result->value.integer, 1);
	
      break;

    case BT_REAL:
      if (mpf_sgn(op1->value.real) == 0)
	rc = ARITH_0TO0;
      else
	mpf_set_ui(result->value.real, 1);

      break;

    case BT_COMPLEX:
      if (mpf_sgn(op1->value.complex.r) == 0 &&
	  mpf_sgn(op1->value.complex.i) == 0)
	rc = ARITH_0TO0;
      else {
	mpf_set_ui(result->value.complex.r, 1);
	mpf_set_ui(result->value.complex.r, 0);
      }

      break;

    default:
      g95_internal_error("g95_arith_power(): Bad base");
    }
  }

  if (power != 0) {
    apower = power;
    if (power < 0) apower = -power;

    switch(op1->ts.type) {
    case BT_INTEGER:
      mpz_pow_ui(result->value.integer, op1->value.integer, apower);

      if (power < 0) {
	mpz_init_set_ui(unity_z, 1);
	mpz_tdiv_q(result->value.integer, unity_z, result->value.integer);
	mpz_clear(unity_z);
      }

      break;

    case BT_REAL:
      mpf_pow_ui(result->value.real, op1->value.real, apower);

      if (power < 0) {
	mpf_init_set_ui(unity_f, 1);
	mpf_div(result->value.real, unity_f, result->value.real);
	mpf_clear(unity_f);
      }

      break;

    case BT_COMPLEX:
      complex_pow_ui(op1, apower, result);
      if (power < 0) complex_reciprocal(result);

      break;

    default:
      break;
    }
  }

  if (rc == ARITH_OK) rc = g95_range_check(result);

  if (rc != ARITH_OK)
    g95_free_expr(result);
  else
    *resultp = result;

  return rc;
}


/* g95_arith_concat()-- Concatenate two string constants */

arith g95_arith_concat(g95_expr *op1, g95_expr *op2, g95_expr **resultp) {
g95_expr *result;
int len;

  result = g95_constant_result(BT_CHARACTER, g95_default_character_kind());

  len = op1->value.character.length + op2->value.character.length;

  result->value.character.string = g95_getmem(len+1);
  result->value.character.length = len;

  memcpy(result->value.character.string, op1->value.character.string,
	 op1->value.character.length);

  memcpy(result->value.character.string + op1->value.character.length,
	 op2->value.character.string, op2->value.character.length);

  result->value.character.string[len] = '\0';

  *resultp = result;

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

  case BT_CHARACTER:
    rc = g95_compare_string(op1, op2, NULL);
    break;

  case BT_LOGICAL:
    rc = ((!op1->value.logical && op2->value.logical) ||
	  (op1->value.logical && !op2->value.logical));
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


/* g95_compare_string()-- Given two constant strings and the inverse
 * collating sequence, compare the strings.  We return -1 for a<b,
 * 0 for a==b and 1 for a>b.  If the xcoll_table is NULL, we use the
 * processor's default collating sequence. */

int g95_compare_string(g95_expr *a, g95_expr *b, int *xcoll_table) {
int len, alen, blen, i, ac, bc;

  alen = a->value.character.length;
  blen = b->value.character.length;

  len = (alen > blen) ? alen : blen;

  for(i=0; i<len; i++) {
    ac = (i < alen) ? a->value.character.string[i] : ' ';
    bc = (i < blen) ? b->value.character.string[i] : ' ';

    if (xcoll_table != NULL) {
      ac = xcoll_table[ac];
      bc = xcoll_table[bc];
    }

    if (ac < bc) return -1;
    if (ac > bc) return 1;
  }

  /* Strings are equal */

  return 0;
}


/* Specific comparison subroutines */

arith g95_arith_eq(g95_expr *op1, g95_expr *op2, g95_expr **resultp) {
g95_expr *result;

  result = g95_constant_result(BT_LOGICAL, g95_default_logical_kind());
  result->value.logical = (op1->ts.type == BT_COMPLEX) ?
    compare_complex(op1, op2) : (g95_compare_expr(op1, op2) == 0);

  *resultp = result;
  return ARITH_OK;
}


arith g95_arith_ne(g95_expr *op1, g95_expr *op2, g95_expr **resultp) {
g95_expr *result;

  result = g95_constant_result(BT_LOGICAL, g95_default_logical_kind());
  result->value.logical = (op1->ts.type == BT_COMPLEX) ?
    !compare_complex(op1, op2) : (g95_compare_expr(op1, op2) != 0);

  *resultp = result;
  return ARITH_OK;
}


arith g95_arith_gt(g95_expr *op1, g95_expr *op2, g95_expr **resultp) {
g95_expr *result;

  result = g95_constant_result(BT_LOGICAL, g95_default_logical_kind());
  result->value.logical = (g95_compare_expr(op1, op2) > 0);
  *resultp = result;

  return ARITH_OK;
}


arith g95_arith_ge(g95_expr *op1, g95_expr *op2, g95_expr **resultp) {
g95_expr *result;

  result = g95_constant_result(BT_LOGICAL, g95_default_logical_kind());
  result->value.logical = (g95_compare_expr(op1, op2) >= 0);
  *resultp = result;

  return ARITH_OK;
}


arith g95_arith_lt(g95_expr *op1, g95_expr *op2, g95_expr **resultp) {
g95_expr *result;

  result = g95_constant_result(BT_LOGICAL, g95_default_logical_kind());
  result->value.logical = (g95_compare_expr(op1, op2) < 0);
  *resultp = result;

  return ARITH_OK;
}


arith g95_arith_le(g95_expr *op1, g95_expr *op2, g95_expr **resultp) {
g95_expr *result;

  result = g95_constant_result(BT_LOGICAL, g95_default_logical_kind());
  result->value.logical = (g95_compare_expr(op1, op2) <= 0);
  *resultp = result;

  return ARITH_OK;
}




static arith reduce_unary(arith (*eval)(), g95_expr *op, g95_expr **result) {
g95_constructor *c, *head;
g95_expr *r;
arith rc;

  if (op->expr_type == EXPR_CONSTANT) return eval(op, result);

  rc = ARITH_OK;

  head = op->value.constructor;
  op->value.constructor = NULL;   /* Reuse the constructor */

  for(c=head; c; c=c->next) {
    rc = eval(c->expr, &r);
    if (rc != ARITH_OK) break;

    g95_replace_expr(c->expr, r);
  }

  if (rc != ARITH_OK)
    g95_free_constructor(head);
  else {
    r = g95_get_expr();
    r->expr_type = EXPR_ARRAY;
    r->value.constructor = head;
    r->ts = op->ts;

    *result = r;
  }

  return rc;
}




static arith reduce_binary_ac(arith (*eval)(), g95_expr *op1, g95_expr *op2,
			      g95_expr **result) {
g95_constructor *c, *head;
g95_expr *r;
arith rc;

  head = op1->value.constructor;   /* Re-use constructor list */ 
  op1->value.constructor = NULL;

  rc = ARITH_OK;

  for(c=head; c; c=c->next) {
    rc = eval(c->expr, op2, &r);
    if (rc != ARITH_OK) break;

    g95_replace_expr(c->expr, r);
  }

  if (rc != ARITH_OK)
    g95_free_constructor(head);
  else {
    r = g95_get_expr();
    r->expr_type = EXPR_ARRAY;
    r->value.constructor = head;
    r->ts = op1->ts;

    *result = r;
  }

  return rc;
}


static arith reduce_binary_ca(arith (*eval)(), g95_expr *op1, g95_expr *op2,
			      g95_expr **result) {
g95_constructor *c, *head;
g95_expr *r;
arith rc;

  head = op2->value.constructor;   /* Re-use constructor list */ 
  op2->value.constructor = NULL;

  rc = ARITH_OK;

  for(c=head; c; c=c->next) {
    rc = eval(c->expr, op1, &r);
    if (rc != ARITH_OK) break;

    g95_replace_expr(c->expr, r);
  }

  if (rc != ARITH_OK)
    g95_free_constructor(head);
  else {
    r = g95_get_expr();
    r->expr_type = EXPR_ARRAY;
    r->value.constructor = head;
    r->ts = op2->ts;

    *result = r;
  }

  return rc;
}



static arith reduce_binary_aa(arith (*eval)(), g95_expr *op1, g95_expr *op2,
			      g95_expr **result) {
g95_constructor *c, *d, *head;
g95_expr *r;
arith rc;

  head = op1->value.constructor;   /* Re-use constructor list */ 
  op1->value.constructor = NULL;

  rc = ARITH_OK;
  d = op2->value.constructor;

  for(c=head; c; c=c->next, d=d->next) {
    if (d == NULL) {
      rc = ARITH_INCOMMENSURATE;
      break;
    }

    rc = eval(c->expr, d->expr, &r);
    if (rc != ARITH_OK) break;

    g95_replace_expr(c->expr, r);
  }

  if (d != NULL) rc = ARITH_INCOMMENSURATE;

  if (rc != ARITH_OK)
    g95_free_constructor(head);
  else {
    r = g95_get_expr();
    r->expr_type = EXPR_ARRAY;
    r->value.constructor = head;
    r->ts = op1->ts;

    *result = r;
  }

  return rc;
}




static arith reduce_binary(arith (*eval)(), g95_expr *op1, g95_expr *op2,
			   g95_expr **result) {

  if (op1->expr_type == EXPR_CONSTANT && op2->expr_type == EXPR_CONSTANT)
    return eval(op1, op2, result);

  if (op1->expr_type == EXPR_CONSTANT && op2->expr_type == EXPR_ARRAY)
    return reduce_binary_ca(eval, op1, op2, result);

  if (op1->expr_type == EXPR_ARRAY && op2->expr_type == EXPR_CONSTANT)
    return reduce_binary_ac(eval, op1, op2, result);

  return reduce_binary_aa(eval, op1, op2, result);
}


/* High level arithmetic subroutines.  These subroutines go into
 * eval_intrinsic(), which can do one of several things to it's
 * operands.  If the operands are incompatible with the intrinsic
 * operation, we return a node pointing to the operands and hope that
 * an operator interface is found during resolution.
 *
 * If the operands are compatible and are constants, then we try doing
 * the arithmetic.  We also handle the cases where either or both
 * operands are array constructors. */

static g95_expr *eval_intrinsic(g95_intrinsic_op operator,
				arith (*eval)(),
				g95_expr *op1, g95_expr *op2) {
g95_expr temp, *result;
int unary;
arith rc;

  g95_clear_ts(&temp.ts);

  switch(operator) {
  case INTRINSIC_NOT:    /* Logical unary */
    if (op1->ts.type != BT_LOGICAL) goto incompatible;

    temp.ts.type = BT_LOGICAL;
    temp.ts.kind = g95_default_logical_kind();

    unary = 1;
    break;

    /* Logical binary operators */
  case INTRINSIC_OR:     case INTRINSIC_AND:
  case INTRINSIC_NEQV:   case INTRINSIC_EQV:
    if (op1->ts.type != BT_LOGICAL || op2->ts.type != BT_LOGICAL)
      goto incompatible;

    temp.ts.type = BT_LOGICAL;
    temp.ts.kind = g95_default_logical_kind();

    unary = 0;
    break;

  case INTRINSIC_UPLUS:   case INTRINSIC_UMINUS:  /* Numeric unary */
    if (!g95_numeric_ts(&op1->ts)) goto incompatible;

    temp.ts = op1->ts;

    unary = 1;
    break;

  case INTRINSIC_GE:  case INTRINSIC_LT:  /* Additional restrictions */
  case INTRINSIC_LE:  case INTRINSIC_GT:  /* for ordering relations */
    if (op1->ts.type == BT_COMPLEX || op2->ts.type == BT_COMPLEX)
      goto incompatible;

    /* Fall through */

  case INTRINSIC_EQ:      case INTRINSIC_NE:
    if (op1->ts.type == BT_CHARACTER && op2->ts.type == BT_CHARACTER) {
      unary = 0;
      break;
    }

    /* Fall through */

  case INTRINSIC_PLUS:    case INTRINSIC_MINUS:   case INTRINSIC_TIMES:
  case INTRINSIC_DIVIDE:  case INTRINSIC_POWER:   /* Numeric binary */
    if (!g95_numeric_ts(&op1->ts) || !g95_numeric_ts(&op2->ts))
      goto incompatible;

    /* Insert any necessary type conversions to make the operands compatible */

    temp.expr_type = EXPR_OP;
    g95_clear_ts(&temp.ts);
    temp.operator = operator;

    temp.op1 = op1;
    temp.op2 = op2;

    g95_type_convert_binary(&temp);

    if (operator == INTRINSIC_EQ || operator == INTRINSIC_NE ||
	operator == INTRINSIC_GE || operator == INTRINSIC_GT ||
	operator == INTRINSIC_LE || operator == INTRINSIC_LT) {
      temp.ts.type = BT_LOGICAL;
      temp.ts.kind = g95_default_integer_kind();
    }

    unary = 0;
    break;

  case INTRINSIC_CONCAT:   /* Character binary */
    if (op1->ts.type != BT_CHARACTER || op2->ts.type != BT_CHARACTER)
      goto incompatible;

    temp.ts.type = BT_CHARACTER;
    temp.ts.kind = g95_default_character_kind();

    unary = 0;
    break;

  case INTRINSIC_USER:
    goto incompatible;

  default:
    g95_internal_error("eval_intrinsic(): Bad operator");
  }

  /* Try to combine the operators */

  if (operator == INTRINSIC_POWER && op2->ts.type != BT_INTEGER)
    goto incompatible;


  if (op1->expr_type != EXPR_CONSTANT &&
      (op1->expr_type != EXPR_ARRAY || !g95_is_constant_expr(op1)))
    goto incompatible;

  if (op2 != NULL && op2->expr_type != EXPR_CONSTANT &&
      (op2->expr_type != EXPR_ARRAY || !g95_is_constant_expr(op2)))
    goto incompatible;

  if (unary)
    rc = reduce_unary(eval, op1, &result);
  else
    rc = reduce_binary(eval, op1, op2, &result);

  if (rc != ARITH_OK) {     /* Something went wrong */
    g95_error("%s at %L", g95_arith_error(rc), &op1->where);
    return NULL;
  }

  g95_free_expr(op1);
  g95_free_expr(op2);
  return result;

  /* Create a run-time expression */

incompatible:
  result = g95_get_expr();
  result->ts = temp.ts;

  result->expr_type = EXPR_OP;
  result->operator = operator;

  result->op1 = op1;
  result->op2 = op2;

  result->where = op1->where;

  return result;
}

static g95_expr *eval_intrinsic_f2(g95_intrinsic_op operator,
				   arith (*eval)(g95_expr *, g95_expr **),
				   g95_expr *op1, g95_expr *op2) {
  return eval_intrinsic(operator, eval, op1, op2);
}

static g95_expr *eval_intrinsic_f3(g95_intrinsic_op operator,
				   arith (*eval)(g95_expr *, g95_expr *,
						 g95_expr **),
				   g95_expr *op1, g95_expr *op2) {

  return eval_intrinsic(operator, eval, op1, op2);
}


g95_expr *g95_uplus(g95_expr *op) {
  return eval_intrinsic_f2(INTRINSIC_UPLUS, g95_arith_uplus, op, NULL);
}

g95_expr *g95_uminus(g95_expr *op) {
  return eval_intrinsic_f2(INTRINSIC_UMINUS, g95_arith_uminus, op, NULL);
}

g95_expr *g95_add(g95_expr *op1, g95_expr *op2) {
  return eval_intrinsic_f3(INTRINSIC_PLUS, g95_arith_plus, op1, op2);
}

g95_expr *g95_subtract(g95_expr *op1, g95_expr *op2) {
  return eval_intrinsic_f3(INTRINSIC_MINUS, g95_arith_minus, op1, op2);
}

g95_expr *g95_multiply(g95_expr *op1, g95_expr *op2) {
  return eval_intrinsic_f3(INTRINSIC_TIMES, g95_arith_times, op1, op2);
}

g95_expr *g95_divide(g95_expr *op1, g95_expr *op2) {
  return eval_intrinsic_f3(INTRINSIC_DIVIDE, g95_arith_divide, op1, op2);
}

g95_expr *g95_power(g95_expr *op1, g95_expr *op2) {
  return eval_intrinsic_f3(INTRINSIC_POWER, g95_arith_power, op1, op2);
}

g95_expr *g95_concat(g95_expr *op1, g95_expr *op2) {
  return eval_intrinsic_f3(INTRINSIC_CONCAT, g95_arith_concat, op1, op2);
}

g95_expr *g95_and(g95_expr *op1, g95_expr *op2) {
  return eval_intrinsic_f3(INTRINSIC_AND, g95_arith_and, op1, op2);
}

g95_expr *g95_or(g95_expr *op1, g95_expr *op2) {
  return eval_intrinsic_f3(INTRINSIC_OR, g95_arith_or, op1, op2);
}

g95_expr *g95_not(g95_expr *op1) {
  return eval_intrinsic_f2(INTRINSIC_NOT, g95_arith_not, op1, NULL);
}

g95_expr *g95_eqv(g95_expr *op1, g95_expr *op2) {
  return eval_intrinsic_f3(INTRINSIC_EQV, g95_arith_eqv, op1, op2);
}

g95_expr *g95_neqv(g95_expr *op1, g95_expr *op2) {
  return eval_intrinsic_f3(INTRINSIC_NEQV, g95_arith_neqv, op1, op2);
}

g95_expr *g95_eq(g95_expr *op1, g95_expr *op2) {
  return eval_intrinsic_f3(INTRINSIC_EQ, g95_arith_eq, op1, op2);
}

g95_expr *g95_ne(g95_expr *op1, g95_expr *op2) {
  return eval_intrinsic_f3(INTRINSIC_NE, g95_arith_ne, op1, op2);
}

g95_expr *g95_gt(g95_expr *op1, g95_expr *op2) {
  return eval_intrinsic_f3(INTRINSIC_GT, g95_arith_gt, op1, op2);
}

g95_expr *g95_ge(g95_expr *op1, g95_expr *op2) {
  return eval_intrinsic_f3(INTRINSIC_GE, g95_arith_ge, op1, op2);
}

g95_expr *g95_lt(g95_expr *op1, g95_expr *op2) {
  return eval_intrinsic_f3(INTRINSIC_LT, g95_arith_lt, op1, op2);
}

g95_expr *g95_le(g95_expr *op1, g95_expr *op2) {
  return eval_intrinsic_f3(INTRINSIC_LE, g95_arith_le, op1, op2);
}

g95_expr *g95_unary_user(g95_expr *op1, g95_expr *op2) {
  return eval_intrinsic_f2(INTRINSIC_USER, NULL, op1, NULL);
}

g95_expr *g95_user(g95_expr *op1, g95_expr *op2) {
  return eval_intrinsic_f2(INTRINSIC_USER, NULL, op1, op2);
}


/* g95_convert_integer()-- Convert an integer string to an expression
 * node */

g95_expr *g95_convert_integer(const char *buffer, int kind, int radix) {
g95_expr *e;

  e = g95_constant_result(BT_INTEGER, kind);
  mpz_set_str(e->value.integer, buffer, radix);

  return e;
}


/* g95_convert_real()-- Convert a real string to an expression node. */

g95_expr *g95_convert_real(const char *buffer, int kind) {
g95_expr *e;

  e = g95_constant_result(BT_REAL, kind);
  mpf_set_str(e->value.real, buffer, 10);

  return e;
}


/* g95_convert_complex()-- Convert a pair of real, constant expression
 * nodes to a single complex expression node. */

g95_expr *g95_convert_complex(g95_expr *real, g95_expr *imag, int kind) {
g95_expr *e;

  e = g95_constant_result(BT_COMPLEX, kind);
  mpf_set(e->value.complex.r, real->value.real);
  mpf_set(e->value.complex.i, imag->value.real);

  return e;
}


/******* Simplification of intrinsic functions with constant arguments *****/


/* arith_error()-- Deal with an arithmetic error. */

static void arith_error(arith rc, g95_typespec *from, g95_typespec *to,
			locus *where) {

  g95_error("%s converting %s to %s at %L", g95_arith_error(rc),
	    g95_typename(from), g95_typename(to), where);

  /* TODO: Do something about the error, ie underflow rounds to 0,
   * throw exception, return NaN, etc. */

}

/* g95_int2int()-- Convert integers to integers */

g95_expr *g95_int2int(g95_expr *src, int kind) {
g95_expr *result;
arith rc;

  result = g95_constant_result(BT_INTEGER, kind);
  result->where = src->where;

  mpz_set(result->value.integer, src->value.integer);

  if ((rc = g95_check_integer_range(result->value.integer, kind))
      != ARITH_OK) {
    arith_error(rc, &src->ts, &result->ts, &src->where);
    g95_free_expr(result);
    return NULL;
  }

  return result;
}


/* g95_int2real()-- Convert integers to reals */

g95_expr *g95_int2real(g95_expr *src, int kind) {
g95_expr *result;
arith rc;

  result = g95_constant_result(BT_REAL, kind);
  result->where = src->where;

  mpf_set_z(result->value.real, src->value.integer);

  if ((rc = g95_check_real_range(result->value.real, kind)) != ARITH_OK) {
    arith_error(rc, &src->ts, &result->ts, &src->where);
    g95_free_expr(result);
    return NULL;
  }

  return result;
}


/* g95_int2complex()-- Convert default integer to default complex */

g95_expr *g95_int2complex(g95_expr *src, int kind) {
g95_expr *result;
arith rc;

  result = g95_constant_result(BT_COMPLEX, kind);
  result->where = src->where;

  mpf_set_z(result->value.complex.r, src->value.integer);
  mpf_set_ui(result->value.complex.i, 0);

  if ((rc = g95_check_real_range(result->value.complex.i, kind)) != ARITH_OK) {
    arith_error(rc, &src->ts, &result->ts, &src->where);
    g95_free_expr(result);
    return NULL;
  }

  return result;
}


/* g95_real2int()-- Convert default real to default integer */

g95_expr *g95_real2int(g95_expr *src, int kind) {
g95_expr *result;
arith rc;

  result = g95_constant_result(BT_INTEGER, kind);
  result->where = src->where;

  mpz_set_f(result->value.integer, src->value.real);

  if ((rc = g95_check_integer_range(result->value.integer, kind))
      != ARITH_OK) {
    arith_error(rc, &src->ts, &result->ts, &src->where);
    g95_free_expr(result);
    return NULL;
  }

  return result;
}


/* g95_real2real()-- Convert real to real */

g95_expr *g95_real2real(g95_expr *src, int kind) {
g95_expr *result;
arith rc;

  result = g95_constant_result(BT_REAL, kind);
  result->where = src->where;

  mpf_set(result->value.real, src->value.real);

  if ((rc = g95_check_real_range(result->value.real, kind)) != ARITH_OK) {
    arith_error(rc, &src->ts, &result->ts, &src->where);
    g95_free_expr(result);
    return NULL;
  }

  return result;
}


/* g95_real2complex()-- Convert real to complex. */

g95_expr *g95_real2complex(g95_expr *src, int kind) {
g95_expr *result;
arith rc;

  result = g95_constant_result(BT_COMPLEX, kind);
  result->where = src->where;

  mpf_set(result->value.complex.r, src->value.real);
  mpf_set_ui(result->value.complex.i, 0);

  if ((rc = g95_check_real_range(result->value.complex.i, kind)) != ARITH_OK) {
    arith_error(rc, &src->ts, &result->ts, &src->where);
    g95_free_expr(result);
    return NULL;
  }

  return result;
}


/* g95_complex2int()-- Convert complex to integer */

g95_expr *g95_complex2int(g95_expr *src, int kind) {
g95_expr *result;
arith rc;

  result = g95_constant_result(BT_INTEGER, kind);
  result->where = src->where;

  mpz_set_f(result->value.integer, src->value.complex.r);

  if ((rc = g95_check_integer_range(result->value.integer, kind))
      != ARITH_OK) {
    arith_error(rc, &src->ts, &result->ts, &src->where);
    g95_free_expr(result);
    return NULL;
  }

  return result;
}


/* g95_complex2real()-- Convert complex to real */

g95_expr *g95_complex2real(g95_expr *src, int kind) {
g95_expr *result;
arith rc;

  result = g95_constant_result(BT_REAL, kind);
  result->where = src->where;

  mpf_set(result->value.real, src->value.complex.r);

  if ((rc = g95_check_real_range(result->value.real, kind)) != ARITH_OK) {
    arith_error(rc, &src->ts, &result->ts, &src->where);
    g95_free_expr(result);
    return NULL;
  }

  return result;
}


/* g95_complex2complex()-- Convert complex to complex */

g95_expr *g95_complex2complex(g95_expr *src, int kind) {
g95_expr *result;
arith rc;

  result = g95_constant_result(BT_COMPLEX, kind);
  result->where = src->where;

  mpf_set(result->value.complex.r, src->value.complex.r);
  mpf_set(result->value.complex.i, src->value.complex.i);

  if ((rc = g95_check_real_range(result->value.complex.r, kind)) != ARITH_OK ||
      (rc = g95_check_real_range(result->value.complex.i, kind)) != ARITH_OK) {
    arith_error(rc, &src->ts, &result->ts, &src->where);
    g95_free_expr(result);
    return NULL;
  }

  return result;
}
