/* Simplify intrinsic functions at compile-time
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

#include "g95.h"
#include "intrinsic.h"

#include <ctype.h>
#include <string.h>


extern mpf_t mpf_pi, mpf_hpi, mpf_nhpi, mpf_tpi;

static g95_expr *integer_zero, *real_zero;
static mpf_t mpf_zero, mpf_half, mpf_one;
static mpz_t mpz_zero;

g95_expr g95_bad_expr;


/* Note that 'simplification' is not just transforming expressions.
 * For functions that are not simplified at compile time,
 * range checking is done if possible.
 *
 * The return convention is that each simplification function returns:
 *
 *   A new expression node corresponding to the simplified arguments.
 *   The original arguments are destroyed by the caller, and must not
 *   be a part of the new expression.
 *
 *   NULL pointer indicating that no simplification was possible and
 *   the original expression should remain intact.  If the
 *   simplification function sets the type and/or the function name
 *   via the pointer g95_simple_expression, then this type is
 *   retained.
 *
 *   An expression pointer to g95_bad_expr (a static placeholder)
 *   indicating that some error has prevented simplification.  For
 *   example, sqrt(-1.0).  The error is generated within the function
 *   and should be propagated upwards
 *
 * By the time a simplification function gets control, it has been
 * decided that the function call is really supposed to be the
 * intrinsic.  No type checking is strictly necessary, since only
 * valid types will be passed on.  On the other hand, a simplification
 * subroutine may have to look at the type of an argument as part of
 * its processing.
 *
 * Array arguments are never passed to these subroutines.  */

/* Static table for converting non-ascii character sets to ascii.
 * The xascii_table[] is the inverse table. */

static int ascii_table[256] = {
   '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0',
   '\b', '\t', '\n', '\v', '\0', '\r', '\0', '\0',
   '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0',
   '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0',
   ' ',  '!',  '\'', '#',  '$',  '%',  '&',  '\'',
   '(',  ')',  '*',  '+',  ',',  '-',  '.',  '/',
   '0',  '1',  '2',  '3',  '4',  '5',  '6',  '7',
   '8',  '9',  ':',  ';',  '<',  '=',  '>',  '?',
   '@',  'A',  'B',  'C',  'D',  'E',  'F',  'G',
   'H',  'I',  'J',  'K',  'L',  'M',  'N',  'O',
   'P',  'Q',  'R',  'S',  'T',  'U',  'V',  'W',
   'X',  'Y',  'Z',  '[', '\\',  ']',  '^',  '_',
   '`',  'a',  'b',  'c',  'd',  'e',  'f',  'g',
   'h',  'i',  'j',  'k',  'l',  'm',  'n',  'o',
   'p',  'q',  'r',  's',  't',  'u',  'v',  'w',
   'x',  'y',  'z',  '{',  '|',  '}',  '~',  '\?'
};

static int xascii_table[256];


/* range_check()-- Range checks an expression node.  If all goes well,
 * returns the node, otherwise returns &g95_bad_expr and frees the node. */

static g95_expr *range_check(g95_expr *result, const char *name) {

  switch(result->ts.type) {
  case BT_REAL:
    if (g95_check_real_range(result->value.real, result->ts.kind) == ARITH_OK)
      return result;
    break;

  case BT_INTEGER:
    if (g95_check_integer_range(result->value.integer, result->ts.kind)
	== ARITH_OK) return result;
    break;

  case BT_COMPLEX:
    if (g95_check_real_range(result->value.complex.r,
			     result->ts.kind) == ARITH_OK &&
	g95_check_real_range(result->value.complex.i,
			     result->ts.kind)== ARITH_OK) return result;
    break;

  default:
    g95_internal_error("range_check(): Bad type");
  }

  g95_error("Result of %s overflows its kind at %L", name, &result->where);

  g95_free_expr(result);
  return &g95_bad_expr;
}


/* get_kind()-- A helper function that gets an optional and possibly
 * missing kind parameter.  Returns the kind, -1 if something went
 * wrong. */

static int get_kind(bt type, g95_expr *k, const char *name, int default_kind) {
int kind;

  if (k == NULL) return default_kind;

  if (k->expr_type != EXPR_CONSTANT) {
    g95_error("KIND parameter of %s at %L must be an initialization "
	      "expression", name, &k->where);

    return -1;
  }

  if (g95_extract_int(k, &kind) != NULL ||
      g95_validate_kind(type, kind) < 0) {

    g95_error("Invalid KIND parameter of %s at %L", name, &k->where);
    return -1;
  }

  return kind;
}


/*********************** Simplification functions ******************************/


/* The abs family*/


g95_expr *g95_simplify_abs(g95_expr *e) {
g95_expr *result;

  if (e->expr_type != EXPR_CONSTANT) return NULL;

  result = g95_constant_result(BT_REAL, e->ts.kind);
  result->where = e->where;

  mpf_abs(result->value.real, e->value.real);

  return range_check(result, "ABS");
}

g95_expr *g95_simplify_iabs(g95_expr *e) {
g95_expr *result;

  if (e->expr_type != EXPR_CONSTANT) return NULL;

  result = g95_constant_result(BT_INTEGER, e->ts.kind);
  result->where = e->where;

  mpz_abs(result->value.integer, e->value.integer);

  return range_check(result, "IABS");
}


g95_expr *g95_simplify_cabs(g95_expr *e) {
g95_expr *result;
mpf_t a, b;

  if (e->expr_type != EXPR_CONSTANT) return NULL;

  result = g95_constant_result(BT_COMPLEX, e->ts.kind);
  result->where = e->where;

  mpf_init(a);
  mpf_mul(a, e->value.complex.r, e->value.complex.r);

  mpf_init(b);
  mpf_mul(b, e->value.complex.i, e->value.complex.i);

  mpf_add(a, a, b);
  mpf_sqrt(result->value.real, a);

  mpf_clear(a);
  mpf_clear(b);

  return range_check(result, "CABS");
}
/* end of abs family */


g95_expr *g95_simplify_achar(g95_expr *e) {
g95_expr *result;
int index;

  if (e->expr_type != EXPR_CONSTANT) return NULL;

/* We cannot assume that the native character set is ASCII in this function */

  if (g95_extract_int(e, &index) != NULL  || index < 0 || index > 127) {
      g95_error("Extended ASCII not implemented: argument of ACHAR at %L "
		"must be between 0 and 127", &e->where);
      return &g95_bad_expr;
  }

  result = g95_constant_result(BT_CHARACTER, g95_default_character_kind());
  result->where = e->where;

  result->value.character.string = g95_getmem(2);

  result->value.character.length = 1;
  result->value.character.string[0] = ascii_table[index];
  result->value.character.string[1] = '\0';   /* For debugger */
  return result;
}


g95_expr *g95_simplify_acos(g95_expr *x) {
g95_expr *result;
mpf_t negative, square, term;

  if (x->expr_type != EXPR_CONSTANT) return NULL;

  if (mpf_cmp_si(x->value.real, 1) > 0 || mpf_cmp_si(x->value.real, -1) < 0) {
    g95_error("Argument of ACOS at %L must be between -1 and 1", &x->where);
    return &g95_bad_expr;
  }

  result = g95_constant_result(x->ts.type, x->ts.kind);
  result->where = x->where;

  if (mpf_cmp_si(x->value.real, 1) == 0) {
    mpf_set_ui(result->value.real, 0);
    return range_check(result, "ACOS");
  }

  mpf_init(negative);

  if (mpf_cmp_si(x->value.real, -1) == 0) {
    mpf_set(result->value.real, mpf_pi);
    return range_check(result, "ACOS");
  }

  mpf_init(square);
  mpf_init(term);

  mpf_pow_ui(square, x->value.real, 2);
  mpf_ui_sub(term, 1, square);
  mpf_sqrt(term, term);
  mpf_div(term, x->value.real, term);
  mpf_neg(term, term);
  arctangent(&term, &negative);
  mpf_add(result->value.real, mpf_hpi, negative);

  mpf_clear(negative);
  mpf_clear(square);
  mpf_clear(term);

  return range_check(result, "ACOS");
}


g95_expr *g95_simplify_adjustl(g95_expr *e) {
g95_expr *result;
int count, i, len;
char ch;

  if (e->expr_type != EXPR_CONSTANT) return NULL;

  len = e->value.character.length;

  result = g95_constant_result(BT_CHARACTER, e->ts.kind);
  result->where = e->where;

  result->value.character.length = len;
  result->value.character.string = g95_getmem(len+1);

  for (count=0, i=0; i<len; ++i) {
    ch = e->value.character.string[i];
    if (ch != ' ') break;
    ++count;
  }

  for (i=0; i<len-count; ++i) {
    result->value.character.string[i] = e->value.character.string[count+i];
  }

  for (i=len-count; i<len; ++i) {
    result->value.character.string[i] = ' ';
  }

  result->value.character.string[len] = '\0';   /* For debugger */

  return result;
}


g95_expr *g95_simplify_adjustr(g95_expr *e) {
g95_expr *result;
int count, i, len;
char ch;

  if (e->expr_type != EXPR_CONSTANT) return NULL;

  len = e->value.character.length;

  result = g95_constant_result(BT_CHARACTER, e->ts.kind);
  result->where = e->where;

  result->value.character.length = len;
  result->value.character.string = g95_getmem(len+1);

  for (count=0, i=len-1; i>=0; --i) {
    ch = e->value.character.string[i];
    if (ch != ' ') break;
    ++count;
  }

  for (i=0; i<count; ++i) {
    result->value.character.string[i] = ' ';
  }

  for (i=count; i<len; ++i) {
    result->value.character.string[i] = e->value.character.string[i-count];
  }

  result->value.character.string[len] = '\0';   /* For debugger */

  return result;
}


g95_expr *g95_simplify_aimag(g95_expr *e) {
g95_expr *result;

  if (e->expr_type != EXPR_CONSTANT) return NULL;

  result = g95_constant_result(BT_REAL, e->ts.kind);
  result->where = e->where;
  mpf_set(result->value.real, e->value.complex.i);

  return range_check(result, "AIMAG");
}


g95_expr *g95_simplify_aint(g95_expr *e, g95_expr *k) {
g95_expr *rtrunc, *result;
int kind;

  kind = get_kind(BT_REAL, k, "AINT", e->ts.kind);
  if (kind == -1) return &g95_bad_expr;

  if (e->expr_type != EXPR_CONSTANT) return NULL;

  rtrunc = g95_copy_expr(e);

  mpf_trunc(rtrunc->value.real, e->value.real);

  result = g95_real2real(rtrunc, kind);
  g95_free_expr(rtrunc);

  return range_check(result, "AINT");
}


g95_expr *g95_simplify_dint(g95_expr *e) {
g95_expr *rtrunc, *result;

  if (e->expr_type != EXPR_CONSTANT) return NULL;

  rtrunc = g95_copy_expr(e);

  mpf_trunc(rtrunc->value.real, e->value.real);

  result = g95_real2real(rtrunc, g95_default_double_kind());
  g95_free_expr(rtrunc);

  return range_check(result, "DINT");

}


g95_expr *g95_simplify_anint(g95_expr *e, g95_expr *k) {
g95_expr *rtrunc, *result;
int kind, cmp;

  kind = get_kind(BT_REAL, k, "ANINT", e->ts.kind);
  if (kind == -1) return &g95_bad_expr;

  if (e->expr_type != EXPR_CONSTANT) return NULL;

  result = g95_constant_result(e->ts.type, kind);
  result->where = e->where;

  rtrunc = g95_copy_expr(e);

  cmp = mpf_cmp_ui(e->value.real, 0);

  if (cmp > 0) {
    mpf_add(rtrunc->value.real, e->value.real,mpf_half);
    mpf_trunc(result->value.real, rtrunc->value.real);
  } else if (cmp < 0) {
    mpf_sub(rtrunc->value.real, e->value.real,mpf_half);
    mpf_trunc(result->value.real, rtrunc->value.real);
  } else
    mpf_set_ui(result->value.real, 0);

  g95_free_expr(rtrunc);

  return range_check(result, "ANINT");
}


g95_expr *g95_simplify_dnint(g95_expr *e) {
g95_expr *rtrunc, *result;
int cmp;

  if (e->expr_type != EXPR_CONSTANT) return NULL;

  result = g95_constant_result(BT_REAL, g95_default_double_kind());
  result->where = e->where;

  rtrunc = g95_copy_expr(e);

  cmp = mpf_cmp_ui(e->value.real, 0);

  if (cmp > 0) {
    mpf_add(rtrunc->value.real, e->value.real, mpf_half);
    mpf_trunc(result->value.real, rtrunc->value.real);
  } else if (cmp < 0) {
    mpf_sub(rtrunc->value.real, e->value.real, mpf_half);
    mpf_trunc(result->value.real, rtrunc->value.real);
  } else 
    mpf_set_ui(result->value.real, 0);

  g95_free_expr(rtrunc);

  return range_check(result, "DNINT");
}


g95_expr *g95_simplify_asin(g95_expr *x) {
g95_expr *result;
mpf_t negative, square, term;

  if (x->expr_type != EXPR_CONSTANT) return NULL;

  if (mpf_cmp_si(x->value.real, 1) > 0 || mpf_cmp_si(x->value.real, -1) < 0) {
    g95_error("Argument of ASIN at %L must be between -1 and 1", &x->where);
    return &g95_bad_expr;
  }

  result = g95_constant_result(x->ts.type, x->ts.kind);
  result->where = x->where;

  if (mpf_cmp_si(x->value.real, 1) == 0) {
    mpf_set(result->value.real, mpf_hpi);
    return range_check(result, "ASIN");
  }

  mpf_init(negative);

  if (mpf_cmp_si(x->value.real, -1) == 0) {
    mpf_neg(negative, mpf_hpi);
    mpf_set(result->value.real, negative);
    mpf_clear(negative);
    return range_check(result, "ASIN");
  }

  mpf_init(square);
  mpf_init(term);

  mpf_pow_ui(square, x->value.real, 2);
  mpf_ui_sub(term, 1, square);
  mpf_sqrt(term, term);
  mpf_div(term, x->value.real, term);
  arctangent(&term, &result->value.real);

  mpf_clear(negative);
  mpf_clear(square);
  mpf_clear(term);

  return range_check(result, "ASIN");
}


g95_expr *g95_simplify_atan(g95_expr *x) {
g95_expr *result;

  if (x->expr_type != EXPR_CONSTANT) return NULL;

  result = g95_constant_result(x->ts.type, x->ts.kind);
  result->where = x->where;

  arctangent(&x->value.real, &result->value.real);

  return range_check(result, "ATAN");

}


g95_expr *g95_simplify_atan2(g95_expr *y, g95_expr *x) {
g95_expr *result;
mpf_t term;

  if (x->expr_type != EXPR_CONSTANT || y->expr_type != EXPR_CONSTANT)
    return NULL;

  result = g95_constant_result(x->ts.type, x->ts.kind);
  result->where = x->where;

  mpf_init(term);

  if (mpf_cmp_ui(y->value.real, 0) == 0) {
    if (mpf_cmp_ui(x->value.real, 0) == 0) {
      mpf_clear(term);
      g95_error("If first argument of ATAN2 %L is zero, the second argument "
		"must not be zero", &x->where);
      g95_free_expr(result);
      return &g95_bad_expr;
    }
    else if (mpf_cmp_si(x->value.real, 0) < 0) {
      mpf_set(result->value.real, mpf_pi);
      mpf_clear(term);
      return result;
    }
    else if (mpf_cmp_si(x->value.real, -1)== 0) {
      mpf_set_ui(result->value.real, 0);
      mpf_clear(term);
      return range_check(result, "ATAN2");
    }
  }

  if (mpf_cmp_ui(x->value.real, 0) == 0) {
    if (mpf_cmp_si(y->value.real, 0) < 0) {
      mpf_neg(term, mpf_hpi);
      mpf_set(result->value.real, term);
      mpf_clear(term);
      return range_check(result, "ATAN2");
    }
    else if (mpf_cmp_si(y->value.real, 0) > 0) {
      mpf_set(result->value.real, mpf_hpi);
      mpf_clear(term);
      return range_check(result, "ATAN2");
    }
  }

  mpf_div(term, y->value.real, x->value.real);
  arctangent(&term, &result->value.real);

  mpf_clear(term);

  return range_check(result, "ATAN2");

}


g95_expr *g95_simplify_bit_size(g95_expr *e) {
g95_expr *result;
int i;

  i = g95_validate_kind(e->ts.type, e->ts.kind);
  if (i < 0) g95_internal_error("In g95_simplify_bit_size(): bad kind");

  result = g95_constant_result(BT_INTEGER, e->ts.kind);
  mpz_set_ui(result->value.integer, g95_integer_kinds[i].bit_size);
  result->where = e->where;

  return result;
}


g95_expr *g95_simplify_btest(g95_expr *e, g95_expr *bit) {
int b;

  if (e->expr_type != EXPR_CONSTANT || bit->expr_type != EXPR_CONSTANT)
    return NULL;

  if (g95_extract_int(bit, &b) != NULL || b < 0)
    return g95_logical_expr(0, &e->where);

  return g95_logical_expr(mpz_tstbit(e->value.integer, b), &e->where);
}


g95_expr *g95_simplify_ceiling(g95_expr *e, g95_expr *k) {
g95_expr *ceil, *result;
int kind;

  kind = get_kind(BT_REAL, k, "CEILING", g95_default_integer_kind());
  if (kind == -1) return &g95_bad_expr;

  if (e->expr_type != EXPR_CONSTANT) return NULL;

  result = g95_constant_result(BT_INTEGER, kind);
  result->where = e->where;

  ceil = g95_copy_expr(e);

  mpf_ceil(ceil->value.real, e->value.real);
  mpz_set_f(result->value.integer, ceil->value.real);

  g95_free_expr(ceil);

  return range_check(result, "CEILING");
}


g95_expr *g95_simplify_char(g95_expr *e, g95_expr *k) {
g95_expr *result;
int c, kind;

  kind = get_kind(BT_CHARACTER, k, "CHAR", g95_default_character_kind());
  if (kind == -1) return &g95_bad_expr;

  if (e->expr_type != EXPR_CONSTANT) return NULL;

  if (g95_extract_int(e, &c) != NULL || c < 0 || c > 255) {
    g95_error("Bad character in CHAR function at %L", &e->where);
    return &g95_bad_expr;
  }

  result = g95_constant_result(BT_CHARACTER, kind);
  result->where = e->where;

  result->value.character.length = 1;
  result->value.character.string = g95_getmem(2);

  result->value.character.string[0] = c;
  result->value.character.string[1] = '\0';   /* For debugger */

  return result;
}


/* simplify_cmplx()-- Common subroutine for simplifying CMPLX and DCMPLX */

static g95_expr *simplify_cmplx(g95_expr *x, g95_expr *y, int kind,
				const char *name) {
g95_expr *result;

  result = g95_constant_result(BT_COMPLEX, kind);
  result->where = x->where;

  mpf_set_ui(result->value.complex.i, 0);

  switch(x->ts.type) {
  case BT_INTEGER:
    mpf_set_z(result->value.complex.r, x->value.integer);
    break;

  case BT_REAL:
    mpf_set(result->value.complex.r, x->value.real);
    break;

  case BT_COMPLEX:
    mpf_set(result->value.complex.r, x->value.complex.r);
    mpf_set(result->value.complex.i, x->value.complex.i);
    break;

  default:
    g95_internal_error("g95_simplify_dcmplx(): Bad type (x)");
  }

  if (y != NULL) {
    switch(y->ts.type) {
    case BT_INTEGER:
      mpf_set_z(result->value.complex.i, y->value.integer);
      break;

    case BT_REAL:
      mpf_set(result->value.complex.i, y->value.real);
      break;

    default:
      g95_internal_error("g95_simplify_dcmplx(): Bad type (y)");
    }
  }

  return range_check(result, name);
}


g95_expr *g95_simplify_cmplx(g95_expr *x, g95_expr *y, g95_expr *k) {
int kind;

  if (x->expr_type != EXPR_CONSTANT ||
      (y != NULL && y->expr_type != EXPR_CONSTANT)) return NULL;

  kind = get_kind(BT_REAL, k, "CMPLX", g95_default_real_kind());
  if (kind == -1) return &g95_bad_expr;

  return simplify_cmplx(x, y, kind, "CMPLX");
}


g95_expr *g95_simplify_conjg(g95_expr *e) {
g95_expr *result;

  if (e->expr_type != EXPR_CONSTANT) return NULL;

  result = g95_copy_expr(e);
  mpf_neg(result->value.complex.i, result->value.complex.i);

  return range_check(result, "CONJG");
}


g95_expr *g95_simplify_cos(g95_expr *x) {
g95_expr *result;
mpf_t xp, xq;

  if (x->expr_type != EXPR_CONSTANT) return NULL;

  result = g95_constant_result(x->ts.type, x->ts.kind);
  result->where = x->where;

  switch (x->ts.type) {
  case BT_REAL:
    cosine(&x->value.real, &result->value.real);
    break;
  case BT_COMPLEX:
    mpf_init(xp);
    mpf_init(xq);

    cosine(&x->value.complex.r, &xp);
    hypercos(&x->value.complex.i, &xq);
    mpf_mul(result->value.complex.r, xp, xq);

    sine(&x->value.complex.r, &xp);
    hypersine(&x->value.complex.i, &xq);
    mpf_mul(xp, xp, xq);
    mpf_neg(result->value.complex.i, xp);

    mpf_clear(xp);
    mpf_clear(xq);
    break;
  default:
    g95_internal_error("in g95_simplify_cos(): Bad type");
  }

  return range_check(result, "COS");

}


g95_expr *g95_simplify_cosh(g95_expr *x) {
g95_expr *result;

  if (x->expr_type != EXPR_CONSTANT) return NULL;

  result = g95_constant_result(x->ts.type, x->ts.kind);
  result->where = x->where;

  hypercos(&x->value.real, &result->value.real);

  return range_check(result, "COSH");

}


g95_expr *g95_simplify_dcmplx(g95_expr *x, g95_expr *y) {

  if (x->expr_type != EXPR_CONSTANT ||
      (y != NULL && y->expr_type != EXPR_CONSTANT))
    return NULL;

  return simplify_cmplx(x, y, g95_default_double_kind(), "DCMPLX");
}


g95_expr *g95_simplify_dble(g95_expr *e) {
g95_expr *result;

  if (e->expr_type != EXPR_CONSTANT) return NULL;

  switch (e->ts.type) {
  case BT_INTEGER:
    result = g95_int2real(e, g95_default_double_kind());
    break;

  case BT_REAL:
    result = g95_real2real(e, g95_default_double_kind());
    break;

  case BT_COMPLEX:
    result = g95_complex2real(e, g95_default_double_kind());
    break;

  default:
    g95_internal_error("bad type in g95_simplify_dble", &e->where);
  }

  return range_check(result, "DBLE");
}


g95_expr *g95_simplify_digits(g95_expr *x) {
int i, digits;

  i = g95_validate_kind(x->ts.type, x->ts.kind);
  if (i < 0) goto bad;

  switch(x->ts.type) {
  case BT_INTEGER:
    digits = g95_integer_kinds[i].digits;
    break;

  case BT_REAL:
  case BT_COMPLEX:
    digits = g95_real_kinds[i].digits;
    break;

  default:
  bad:
    g95_internal_error("g95_simplify_digits(): Bad type");
  }

  return g95_int_expr(digits);
}


g95_expr *g95_simplify_dim(g95_expr *x, g95_expr *y) {
g95_expr *result;

  if (x->expr_type != EXPR_CONSTANT || y->expr_type != EXPR_CONSTANT)
    return NULL;

  result = g95_constant_result(x->ts.type, x->ts.kind);
  result->where = x->where;

  switch (x->ts.type) {
  case BT_INTEGER:
    if (mpz_cmp(x->value.integer, y->value.integer) > 0)
      mpz_sub(result->value.integer, x->value.integer, y->value.integer);
    else
      mpz_set(result->value.integer, mpz_zero);

    break;

  case BT_REAL:
    if (mpf_cmp(x->value.real, y->value.real) > 0)
      mpf_sub(result->value.real, x->value.real, y->value.real);
    else
      mpf_set(result->value.real, mpf_zero);

    break;

  default:
    g95_internal_error("g95_simplify_dim(): Bad type");
  }

  return range_check(result, "DIM");
}


g95_expr *g95_simplify_dprod(g95_expr *x, g95_expr *y) {
g95_expr *mult1, *mult2, *result;

  if (x->expr_type != EXPR_CONSTANT || y->expr_type != EXPR_CONSTANT)
    return NULL;

  result = g95_constant_result(BT_REAL, g95_default_double_kind());
  result->where = x->where;

  mult1 = g95_real2real(x, g95_default_double_kind());
  mult2 = g95_real2real(y, g95_default_double_kind());

  mpf_mul(result->value.real, mult1->value.real, mult2->value.real);

  g95_free_expr(mult1);
  g95_free_expr(mult2);

  return range_check(result, "DPROD");
}


g95_expr *g95_simplify_epsilon(g95_expr *e) {
g95_expr *result;
int i;

  i = g95_validate_kind(e->ts.type, e->ts.kind);
  if (i == -1) g95_internal_error("g95_simplify_epsilon(): Bad kind");

  result = g95_constant_result(BT_REAL, e->ts.kind);
  result->where = e->where;

  mpf_set(result->value.real, g95_real_kinds[i].epsilon);

  return range_check(result, "EPSILON");
}


g95_expr *g95_simplify_exp(g95_expr *x) {
g95_expr *result;
mpf_t xp, xq;
double ln2, absval, rhuge;

  if (x->expr_type != EXPR_CONSTANT) return NULL;

  result = g95_constant_result(x->ts.type, x->ts.kind);
  result->where = x->where;

  /* Exactitude doesn't matter here */
  ln2 = .6931472;
  rhuge  = ln2*mpz_get_d(g95_integer_kinds[0].huge);

  switch (x->ts.type) {
  case BT_REAL:
    absval = mpf_get_d(x->value.real);
    if (absval < 0) absval = -absval;
    if (absval > rhuge) {
      /* Underflow (set arg to zero) if x is negative and its magnitude is
       * greater than the maximum C long int times ln2, because the exponential
       * method in arith.c will fail for such values */

      if (mpf_cmp_ui(x->value.real, 0) < 0) {
        if (g95_option.pedantic == 1) 
            g95_warning_now("Argument of EXP at %L is negative and too large, "
			    "setting result to zero", &x->where);
        mpf_set_ui(result->value.real, 0);
        return range_check(result, "EXP");
      }
    /* Overflow if magnitude of x is greater than C long int huge times ln2. */
      else {
        g95_error("Argument of EXP at %L too large", &x->where);
        g95_free_expr(result);
        return &g95_bad_expr;
      }
    }
    exponential(&x->value.real, &result->value.real);
    break;
  case BT_COMPLEX:
    /* Using Euler's formula */
    absval = mpf_get_d(x->value.complex.r);
    if (absval < 0) absval = -absval;
    if (absval > rhuge) {
      if (mpf_cmp_ui(x->value.complex.r, 0) < 0) {
        if (g95_option.pedantic == 1)
            g95_warning_now("Real part of argument of EXP at %L is negative "
			    "and too large, setting result to zero",
			    &x->where);

        mpf_set_ui(result->value.complex.r, 0);
        mpf_set_ui(result->value.complex.i, 0);
        return range_check(result, "EXP");
      } else {
        g95_error("Real part of argument of EXP at %L too large", &x->where);
        g95_free_expr(result);
        return &g95_bad_expr;
      }
    }
    mpf_init(xp);
    mpf_init(xq);
    exponential(&x->value.complex.r, &xq);
    cosine(&x->value.complex.i, &xp);
    mpf_mul(result->value.complex.r, xq, xp);
    sine(&x->value.complex.i, &xp);
    mpf_mul(result->value.complex.i, xq, xp);
    mpf_clear(xp);
    mpf_clear(xq);
    break;
  default:
    g95_internal_error("in g95_simplify_exp(): Bad type");
  }

  return range_check(result, "EXP");
}


g95_expr *g95_simplify_exponent(g95_expr *x) {
mpf_t i2, absv, ln2, lnx;
g95_expr *result;

  if (x->expr_type != EXPR_CONSTANT) return NULL;

  result=g95_constant_result(BT_INTEGER, g95_default_integer_kind());
  result->where = x->where;

  if (mpf_cmp(x->value.real, mpf_zero) == 0) {
    mpz_set_ui(result->value.integer, 0);
    return result;
  }

  mpf_init_set_ui(i2, 2);
  mpf_init(absv);
  mpf_init(ln2);
  mpf_init(lnx);

  natural_logarithm(&i2, &ln2);

  mpf_abs(absv, x->value.real);
  natural_logarithm(&absv, &lnx);

  mpf_div(lnx, lnx, ln2);
  mpf_trunc(lnx, lnx);
  mpf_add_ui(lnx, lnx, 1);
  mpz_set_f(result->value.integer, lnx);

  mpf_clear(i2);
  mpf_clear(ln2);
  mpf_clear(lnx);
  mpf_clear(absv);

  return range_check(result, "EXPONENT");
}


g95_expr *g95_simplify_float(g95_expr *a) {
g95_expr *result;

  if (a->expr_type != EXPR_CONSTANT) return NULL;

  result = g95_int2real(a, g95_default_real_kind());
  return range_check(result, "FLOAT");
}


g95_expr *g95_simplify_floor(g95_expr *e, g95_expr *k) {
g95_expr *result;
mpf_t floor;
int kind;

  kind = get_kind(BT_REAL, k, "FLOOR", g95_default_integer_kind());
  if (kind == -1) g95_internal_error("g95_simplify_floor(): Bad kind");

  if (e->expr_type != EXPR_CONSTANT) return NULL;

  result = g95_constant_result(BT_INTEGER, kind);
  result->where = e->where;

  mpf_init(floor);
  mpf_floor(floor, e->value.real);
  mpz_set_f(result->value.integer, floor);
  mpf_clear(floor);

  return range_check(result, "FLOOR");
}


g95_expr *g95_simplify_fraction(g95_expr *x) {
g95_expr *result;
mpf_t i2, absv, ln2, lnx, pow2;
unsigned long exp2;

  if (x->expr_type != EXPR_CONSTANT) return NULL;

  result = g95_constant_result(BT_REAL, x->ts.kind);
  result->where = x->where;

  if (mpf_cmp(x->value.real, mpf_zero) == 0) {
    mpf_set(result->value.real, mpf_zero);
    return result;
  }

  mpf_init_set_ui(i2, 2);
  mpf_init(absv);
  mpf_init(ln2);
  mpf_init(lnx);
  mpf_init(pow2);

  natural_logarithm(&i2, &ln2);

  mpf_abs(absv, x->value.real);
  natural_logarithm(&absv, &lnx);

  mpf_div(lnx, lnx, ln2);
  mpf_trunc(lnx, lnx);
  mpf_add_ui(lnx, lnx, 1);

  exp2 = (unsigned long) mpf_get_d(lnx);
  mpf_pow_ui(pow2, i2, exp2);

  mpf_div(result->value.real, absv, pow2);

  mpf_clear(i2);
  mpf_clear(ln2);
  mpf_clear(absv);
  mpf_clear(lnx);
  mpf_clear(pow2);

  return range_check(result, "FRACTION");
}


g95_expr *g95_simplify_huge(g95_expr *e) {
g95_expr *result;
int i;

  i = g95_validate_kind(e->ts.type, e->ts.kind);
  if (i == -1) goto bad_type;

  result = g95_constant_result(e->ts.type, e->ts.kind);
  result->where = e->where;

  switch(e->ts.type) {
  case BT_INTEGER:
    mpz_init_set(result->value.integer, g95_integer_kinds[i].huge);
    break;

  case BT_REAL:
    mpf_init_set(result->value.real, g95_real_kinds[i].huge);
    break;

  bad_type:
  default:
    g95_internal_error("g95_simplify_huge(): Bad type");
  }

  return result;
}


g95_expr *g95_simplify_iachar(g95_expr *e) {
g95_expr *result;
int index;

  if (e->expr_type != EXPR_CONSTANT) return NULL;

  if (e->value.character.length != 1) {
    g95_error("Argument of IACHAR at %L must be of length one", &e->where);
    return &g95_bad_expr;
  }

  index = xascii_table[(int) e->value.character.string[0] & 0xFF];

  result = g95_int_expr(index);
  result->where = e->where;

  return range_check(result, "IACHAR");
}


g95_expr *g95_simplify_iand(g95_expr *x, g95_expr *y) {
g95_expr *result;

  if (x->expr_type != EXPR_CONSTANT || y->expr_type != EXPR_CONSTANT)
    return NULL;

  result = g95_constant_result(BT_INTEGER, x->ts.kind);
  result->where = x->where;

  mpz_and(result->value.integer, x->value.integer, y->value.integer);

  return range_check(result, "IAND");
}


g95_expr *g95_simplify_ibclr(g95_expr *x, g95_expr *y) {
g95_expr *result;
int k, pos;

  if (x->expr_type != EXPR_CONSTANT || y->expr_type != EXPR_CONSTANT)
    return NULL;

  if (g95_extract_int(y, &pos) != NULL || pos < 0) {
    g95_error("Invalid second argument of IBCLR at %L", &y->where);
    return &g95_bad_expr;
  }

  k = g95_validate_kind(x->ts.type, x->ts.kind);
  if (k == -1) g95_internal_error("In g95_simplify_ibclr: bad kind");

  if (pos > g95_integer_kinds[k].bit_size) {
    g95_error("Second argument of IBCLR exceeds bit size at %L", &y->where);
    return &g95_bad_expr;
  }

  result=g95_copy_expr(x);

  mpz_clrbit(result->value.integer, pos);
  return range_check(result, "IBCLR");
}


g95_expr *g95_simplify_ibits(g95_expr *x, g95_expr *y, g95_expr *z) {
g95_expr *result;
int pos, len;
int i, k, bitsize;
int *bits;

  if (x->expr_type != EXPR_CONSTANT || y->expr_type != EXPR_CONSTANT ||
      z->expr_type != EXPR_CONSTANT) return NULL;

  if (g95_extract_int(y, &pos) != NULL || pos < 0) {
    g95_error("Invalid second argument of IBITS at %L", &y->where);
    return &g95_bad_expr;
  }

  if (g95_extract_int(z, &len) != NULL || len < 0) {
    g95_error("Invalid third argument of IBITS at %L", &z->where);
    return &g95_bad_expr;
  }

  k = g95_validate_kind(BT_INTEGER, x->ts.kind);
  if (k == -1) g95_internal_error("In g95_simplify_ibits: bad kind");

  bitsize = g95_integer_kinds[k].bit_size;

  if (pos+len > bitsize) {
    g95_error("Sum of second and third arguments of IBITS exceeds bit size "
	      "at %L", &y->where);
    return &g95_bad_expr;
  }

  result = g95_constant_result(x->ts.type, x->ts.kind);
  result->where = x->where;

  bits = g95_getmem(bitsize*sizeof(int));

  for(i=0; i<bitsize; i++)
    bits[i] = 0;

  for(i=0; i<len; i++)
    bits[i] = mpz_tstbit(x->value.integer, i+pos);

  for(i=0; i<bitsize; i++) {
    if (bits[i] == 0) {
      mpz_clrbit(result->value.integer, i);
    } else if (bits[i] == 1) {
      mpz_setbit(result->value.integer, i);
    } else {
      g95_internal_error("IBITS: Bad bit");
    }
  }

  g95_free(bits);

  return range_check(result, "IBITS");
}


g95_expr *g95_simplify_ibset(g95_expr *x, g95_expr *y) {
g95_expr *result;
int k, pos;

  if (x->expr_type != EXPR_CONSTANT || y->expr_type != EXPR_CONSTANT)
    return NULL;

  if (g95_extract_int(y, &pos) != NULL || pos < 0) {
    g95_error("Invalid second argument of IBSET at %L", &y->where);
    return &g95_bad_expr;
  }

  k = g95_validate_kind(x->ts.type, x->ts.kind);
  if (k == -1) g95_internal_error("In g95_simplify_ibset: bad kind");

  if (pos > g95_integer_kinds[k].bit_size) {
    g95_error("Second argument of IBSET exceeds bit size at %L", &y->where);
    return &g95_bad_expr;
  }

  result = g95_copy_expr(x);

  mpz_setbit(result->value.integer, pos);
  return range_check(result, "IBSET");
}


g95_expr *g95_simplify_ichar(g95_expr *e) {
g95_expr *result;
int index;

  if (e->expr_type != EXPR_CONSTANT) return NULL;

  if (e->value.character.length != 1) {
    g95_error("Argument of ICHAR at %L must be of length one", &e->where);
    return &g95_bad_expr;
  }

  index = (int) e->value.character.string[0];

  if (index < CHAR_MIN || index > CHAR_MAX) {
    g95_error("Argument of ICHAR at %L out of range of this processor",
	      &e->where);
    return &g95_bad_expr;
  }

  result = g95_int_expr(index);
  result->where = e->where;
  return range_check(result, "ICHAR");
}


g95_expr *g95_simplify_ieor(g95_expr *x, g95_expr *y) {
g95_expr *result;

  if (x->expr_type != EXPR_CONSTANT || y->expr_type != EXPR_CONSTANT)
    return NULL;

  result = g95_constant_result(BT_INTEGER, x->ts.kind);
  result->where = x->where;

  mpz_xor(result->value.integer, x->value.integer, y->value.integer);

  return range_check(result, "IEOR");
}


g95_expr *g95_simplify_index(g95_expr *x, g95_expr *y, g95_expr *b) {
g95_expr *result;
int back, len, lensub;
int i, j, k, count, index=0, start;

  if (x->expr_type != EXPR_CONSTANT || y->expr_type != EXPR_CONSTANT)
    return NULL;

  if (b != NULL && b->value.logical != 0)
    back = 1;
  else
    back = 0;

  result = g95_constant_result(BT_INTEGER, g95_default_integer_kind());
  result->where = x->where;

  len    = x->value.character.length;
  lensub = y->value.character.length;

  if (len < lensub) {
    mpz_set_si(result->value.integer, 0);
    return result;
  }

  if (back == 0) {

    if (lensub == 0) {
      mpz_set_si(result->value.integer, 1);
      return result;
    } else if (lensub == 1) {
      for(i=0; i<len; i++) {
        for(j=0; j<lensub; j++) {
    	  if (y->value.character.string[j] == x->value.character.string[i]) {
	    index = i+1;
	    goto done;
	  }
	}
      }
    } else {
      for(i=0; i<len; i++) {
        for(j=0; j<lensub; j++) {
	  if (y->value.character.string[j] == x->value.character.string[i]) {
	    start = i;
	    count = 0;

	    for(k=0; k<lensub; k++) {
    	      if (y->value.character.string[k] ==
		  x->value.character.string[k+start]) count++;
	    }

	    if (count == lensub) {
	      index = start+1;
	      goto done;
	    }
	  }
	}
      }
    }

  } else {

    if (lensub == 0) {
      mpz_set_si(result->value.integer, len+1);
      return result;
    }
    else if (lensub == 1) {
      for(i=0; i<len; i++) {
        for(j=0; j<lensub; j++) {
	  if (y->value.character.string[j]==x->value.character.string[len-i]) {
	    index = len-i+1;
	    goto done;
	  }
	}
      }
    } else {
      for(i=0; i<len; i++) {
        for(j=0; j<lensub; j++) {
	  if (y->value.character.string[j]==x->value.character.string[len-i]) {
	    start = len-i;
	    if (start <= len-lensub) {
	      count = 0;
	      for(k=0; k<lensub; k++)
    	        if (y->value.character.string[k] ==
		    x->value.character.string[k+start]) count++;

	      if (count == lensub) {
	        index = start+1;
	        goto done;
	      }
	    } else {
	      continue;
	    }
	  }
	}
      }
    }
  }

done:
  mpz_set_si(result->value.integer, index);
  return range_check(result, "INDEX");
}


g95_expr *g95_simplify_int(g95_expr *e, g95_expr *k) {
g95_expr *rpart, *rtrunc, *result;
int kind;

  kind = get_kind(BT_REAL, k, "INT", g95_default_integer_kind());
  if (kind == -1) return &g95_bad_expr;

  if (e->expr_type != EXPR_CONSTANT) return NULL;

  result = g95_constant_result(BT_INTEGER, kind);
  result->where = e->where;

  switch(e->ts.type) {
  case BT_INTEGER:
    mpz_set(result->value.integer, e->value.integer);
    break;

  case BT_REAL:
    rtrunc = g95_copy_expr(e);
    mpf_trunc(rtrunc->value.real, e->value.real);
    mpz_set_f(result->value.integer, rtrunc->value.real);
    g95_free_expr(rtrunc);
    break;

  case BT_COMPLEX:
    rpart = g95_complex2real(e, kind);
    rtrunc = g95_copy_expr(rpart);
    mpf_trunc(rtrunc->value.real, rpart->value.real);
    mpz_set_f(result->value.integer, rtrunc->value.real);
    g95_free_expr(rpart);
    g95_free_expr(rtrunc);
    break;

  default:
    g95_error("Argument of INT at %L is not a valid type", &e->where);
    g95_free_expr(result);
    return &g95_bad_expr;
  }

  return range_check(result, "INT");
}


g95_expr *g95_simplify_ifix(g95_expr *e) {
g95_expr *rtrunc, *result;

  if (e->expr_type != EXPR_CONSTANT) return NULL;

  result = g95_constant_result(BT_INTEGER, g95_default_integer_kind());
  result->where = e->where;

  rtrunc = g95_copy_expr(e);

  mpf_trunc(rtrunc->value.real, e->value.real);
  mpz_set_f(result->value.integer, rtrunc->value.real);

  g95_free_expr(rtrunc);
  return range_check(result, "IFIX");
}


g95_expr *g95_simplify_idint(g95_expr *e) {
g95_expr *rtrunc, *result;

  if (e->expr_type != EXPR_CONSTANT) return NULL;

  result = g95_constant_result(BT_INTEGER, g95_default_integer_kind());
  result->where = e->where;

  rtrunc = g95_copy_expr(e);

  mpf_trunc(rtrunc->value.real, e->value.real);
  mpz_set_f(result->value.integer, rtrunc->value.real);

  g95_free_expr(rtrunc);
  return range_check(result, "IDINT");
}


g95_expr *g95_simplify_ior(g95_expr *x, g95_expr *y) {
g95_expr *result;

  if (x->expr_type != EXPR_CONSTANT || y->expr_type != EXPR_CONSTANT)
    return NULL;

  result = g95_constant_result(BT_INTEGER, x->ts.kind);
  result->where = x->where;

  mpz_ior(result->value.integer, x->value.integer, y->value.integer);
  return range_check(result, "IOR");
}


g95_expr *g95_simplify_ishft(g95_expr *e, g95_expr *s) {
g95_expr *result;
int shift, ashift, isize, k;
long e_int;

  if (e->expr_type != EXPR_CONSTANT || s->expr_type != EXPR_CONSTANT)
    return NULL;

  if (g95_extract_int(s, &shift) != NULL) {
    g95_error("Invalid second argument of ISHFT at %L", &s->where);
    return &g95_bad_expr;
  }

  k = g95_validate_kind(BT_INTEGER, e->ts.kind);
  if (k == -1) g95_internal_error("In g95_simplify_ishft: bad kind");

  isize = g95_integer_kinds[k].bit_size;

  if (shift >= 0) ashift = shift;
  else ashift = -shift;

  if (ashift > isize) {
    g95_error("Magnitude of second argument of ISHFT exceeds bit size at %L", &s->where);
    return &g95_bad_expr;
  }

  e_int = mpz_get_si(e->value.integer);
  if (e_int > INT_MAX || e_int < INT_MIN) {
    g95_internal_error("ISHFT: unable to extract integer");
    return &g95_bad_expr;
  }

  result = g95_constant_result(e->ts.type, e->ts.kind);
  result->where = e->where;

  if (shift == 0) {
    mpz_set(result->value.integer, e->value.integer);
    return range_check(result, "ISHFT");
  }

  if (shift > 0)
    mpz_set_si(result->value.integer, e_int << shift);
  else
    mpz_set_si(result->value.integer, e_int >> ashift);

  return range_check(result, "ISHFT");
}


g95_expr *g95_simplify_ishftc(g95_expr *e, g95_expr *s, g95_expr *sz) {
g95_expr *result;
int shift, ashift, isize, delta, k;
int i, *bits;

  if (e->expr_type != EXPR_CONSTANT || s->expr_type != EXPR_CONSTANT)
    return NULL;

  if (g95_extract_int(s, &shift) != NULL) {
    g95_error("Invalid second argument of ISHFTC at %L", &s->where);
    return &g95_bad_expr;
  }

  k = g95_validate_kind(e->ts.type, e->ts.kind);
  if (k == -1) g95_internal_error("In g95_simplify_ishftc: bad kind");

  if (sz !=NULL) {
    if (g95_extract_int(sz, &isize) != NULL || isize < 0) {
      g95_error("Invalid third argument of ISHFTC at %L", &sz->where);
      return &g95_bad_expr;
    }
  } else
    isize = g95_integer_kinds[k].bit_size;

  if (shift >= 0) ashift = shift;
  else ashift = -shift;

  if (ashift > isize) {
    g95_error("Magnitude of second argument of ISHFTC exceeds third argument at %L", &s->where);
    return &g95_bad_expr;
  }

  result = g95_constant_result(e->ts.type, e->ts.kind);
  result->where = e->where;

  bits = g95_getmem(isize*sizeof(int));

  for(i=0; i<isize; i++)
    bits[i] = mpz_tstbit(e->value.integer, i);

  delta = isize-ashift;

  if (shift == 0) {
    mpz_set(result->value.integer, e->value.integer);
    g95_free(bits);
    return range_check(result, "ISHFTC");
  }

  else if (shift > 0) {
    for(i=0; i<delta; i++) {
      if (bits[i] == 0) mpz_clrbit(result->value.integer, i+shift);
      if (bits[i] == 1) mpz_setbit(result->value.integer, i+shift);
    }

    for(i=delta; i<isize; i++) {
      if (bits[i] == 0) mpz_clrbit(result->value.integer, i-delta);
      if (bits[i] == 1) mpz_setbit(result->value.integer, i-delta);
    }

    g95_free(bits);
    return range_check(result, "ISHFTC");
  } else {
    for(i=0; i<ashift; i++) {
      if (bits[i] == 0) mpz_clrbit(result->value.integer, i+delta);
      if (bits[i] == 1) mpz_setbit(result->value.integer, i+delta);
    }

    for(i=ashift; i<isize; i++) {
      if (bits[i] == 0) mpz_clrbit(result->value.integer, i+shift);
      if (bits[i] == 1) mpz_setbit(result->value.integer, i+shift);
    }

    g95_free(bits);
    return range_check(result, "ISHFTC");
  }
}


g95_expr *g95_simplify_kind(g95_expr *e) {

  if (e->ts.type == BT_DERIVED) {
    g95_error("Argument of KIND at %L is a DERIVED type", &e->where);
    return &g95_bad_expr;
  }

  return g95_int_expr(e->ts.kind);
}


g95_expr *g95_simplify_len(g95_expr *e) {
g95_expr *result;

  if (e->expr_type != EXPR_CONSTANT) return NULL;

  result = g95_constant_result(BT_INTEGER, g95_default_integer_kind());
  result->where = e->where;

  mpz_set_si(result->value.integer, e->value.character.length);
  return range_check(result, "LEN");
}


g95_expr *g95_simplify_len_trim(g95_expr *e) {
g95_expr *result;
int count, len, lentrim, i;

  if (e->expr_type != EXPR_CONSTANT) return NULL;

  result = g95_constant_result(BT_INTEGER, g95_default_integer_kind());
  result->where = e->where;

  len = e->value.character.length;

  for(count=0, i=1; i<=len; i++)
    if (e->value.character.string[len-i] == ' ')
      count++;
    else
      break;

  lentrim = len-count;

  mpz_set_si(result->value.integer, lentrim);
  return range_check(result, "LEN_TRIM");
}


g95_expr *g95_simplify_lge(g95_expr *a, g95_expr *b) {

  if (a->expr_type != EXPR_CONSTANT || b->expr_type != EXPR_CONSTANT)
    return NULL;

  return g95_logical_expr(g95_compare_string(a, b, xascii_table) >= 0,
			  &a->where);
}


g95_expr *g95_simplify_lgt(g95_expr *a, g95_expr *b) {

  if (a->expr_type != EXPR_CONSTANT || b->expr_type != EXPR_CONSTANT)
    return NULL;

  return g95_logical_expr(g95_compare_string(a, b, xascii_table) > 0,
			  &a->where);
}


g95_expr *g95_simplify_lle(g95_expr *a, g95_expr *b) {

  if (a->expr_type != EXPR_CONSTANT || b->expr_type != EXPR_CONSTANT)
    return NULL;

  return g95_logical_expr(g95_compare_string(a, b, xascii_table) <= 0,
			  &a->where);
}


g95_expr *g95_simplify_llt(g95_expr *a, g95_expr *b) {

  if (a->expr_type != EXPR_CONSTANT || b->expr_type != EXPR_CONSTANT)
    return NULL;

  return g95_logical_expr(g95_compare_string(a, b, xascii_table) < 0,
			  &a->where);
}


g95_expr *g95_simplify_log(g95_expr *x) {
g95_expr *result;
mpf_t xr, xi;

  if (x->expr_type != EXPR_CONSTANT) return NULL;

  result = g95_constant_result(x->ts.type, x->ts.kind);
  result->where = x->where;

  switch(x->ts.type) {
  case BT_REAL:
    if (mpf_cmp(x->value.real, mpf_zero) <= 0) {
      g95_error("Argument of LOG at %L cannot be less than or equal to zero",
		&x->where);
      g95_free_expr(result);
      return &g95_bad_expr;
    }

    natural_logarithm(&x->value.real, &result->value.real);
    break;

  case BT_COMPLEX:
    if ((mpf_cmp(x->value.complex.r, mpf_zero) == 0) &&
	(mpf_cmp(x->value.complex.i, mpf_zero) == 0)) {
      g95_error("Complex argument of LOG at %L cannot be zero",
		&x->where);
      g95_free_expr(result);
      return &g95_bad_expr;
    }

    mpf_init(xr);
    mpf_init(xi);

    mpf_div(xr, x->value.complex.i, x->value.complex.r);
    arctangent(&xr, &result->value.complex.i);

    mpf_mul(xr, x->value.complex.r, x->value.complex.r);
    mpf_mul(xi, x->value.complex.i, x->value.complex.i);
    mpf_add(xr, xr, xi);
    mpf_sqrt(xr, xr);
    natural_logarithm(&xr, &result->value.complex.r);

    mpf_clear(xr);
    mpf_clear(xi);

    break;

  default:
    g95_internal_error("g95_simplify_log: bad type");
  }

  return range_check(result, "LOG");
}


g95_expr *g95_simplify_log10(g95_expr *x) {
g95_expr *result;

  if (x->expr_type != EXPR_CONSTANT) return NULL;

  if (mpf_cmp(x->value.real, mpf_zero) <= 0) {
    g95_error("Argument of LOG10 at %L cannot be less than or equal to zero",
              &x->where);
    return &g95_bad_expr;
  }

  result = g95_constant_result(x->ts.type, x->ts.kind);
  result->where = x->where;

  common_logarithm(&x->value.real, &result->value.real);

  return range_check(result, "LOG10");
}


g95_expr *g95_simplify_logical(g95_expr *e, g95_expr *k) {
g95_expr *result;
int kind;

  kind = get_kind(BT_LOGICAL, k, "LOGICAL", g95_default_logical_kind());
  if (kind < 0) return &g95_bad_expr;

  if (e->expr_type != EXPR_CONSTANT) return NULL;

  result = g95_constant_result(BT_LOGICAL, kind);
  result->where = e->where;

  result->value.logical = e->value.logical;

  return result;
}


/* simplify_min_max()-- This function is special since MAX() can take
 * any number of arguments.  The simplified expression is a rewritten
 * version of the argument list containing at most one constant
 * element.  Other constant elements are deleted.  Because the
 * argument list has already been checked, this function always
 * succeeds.  sign is 1 for MAX(), -1 for MIN(). */

static g95_expr *simplify_min_max(g95_expr *expr, int sign) {
g95_actual_arglist *arg, *last, *extremum=NULL;

  last = NULL;
  extremum = NULL;

  arg = expr->value.function.actual;

  for(; arg; last=arg, arg=arg->next) {
    if (arg->expr->expr_type != EXPR_CONSTANT) continue;

    if (extremum == NULL) {
      extremum = arg;
      continue;
    }

    switch(arg->expr->ts.type) {
    case BT_INTEGER:
      if (mpz_cmp(arg->expr->value.integer,
		  extremum->expr->value.integer)*sign > 0)
	mpz_set(extremum->expr->value.integer, arg->expr->value.integer);

      break;

    case BT_REAL:
      if (mpf_cmp(arg->expr->value.real, extremum->expr->value.real)*sign > 0)
	mpf_set(extremum->expr->value.real, arg->expr->value.real);

      break;

    default:
      g95_internal_error("g95_simplify_max(): Bad type in arglist");
    }

    /* Delete the extra constant argument */

    if (last == NULL)
      expr->value.function.actual = arg->next;
    else
      last->next = arg->next;

    arg->next = NULL;
    g95_free_actual_arglist(arg);
    arg = last;
  }

  /* If there is one value left, replace the function call with the
   * expression */

  if (expr->value.function.actual->next != NULL) return NULL;

  return g95_copy_expr(expr->value.function.actual->expr);
}


g95_expr *g95_simplify_min(g95_expr *e) {

  return simplify_min_max(e, -1);
}


g95_expr *g95_simplify_max(g95_expr *e) {

  return simplify_min_max(e, 1);
}


g95_expr *g95_simplify_maxexponent(g95_expr *x) {
g95_expr *result;
int i;

  i = g95_validate_kind(BT_REAL, x->ts.kind);
  if (i < 0) g95_internal_error("g95_simplify_maxexponent(): Bad kind");

  result = g95_int_expr(g95_real_kinds[i].max_exponent);
  result->where = x->where;

  return result;
}


g95_expr *g95_simplify_minexponent(g95_expr *x) {
g95_expr *result;
int i;

  i = g95_validate_kind(BT_REAL, x->ts.kind);
  if (i < 0) g95_internal_error("g95_simplify_minexponent(): Bad kind");

  result = g95_int_expr(g95_real_kinds[i].min_exponent);
  result->where = x->where;

  return result;
}


g95_expr *g95_simplify_mod(g95_expr *a, g95_expr *p) {
g95_expr *result;
mpf_t quot, iquot, term;

  if (a->expr_type != EXPR_CONSTANT || p->expr_type != EXPR_CONSTANT)
    return NULL;

  result = g95_constant_result(a->ts.type, a->ts.kind);
  result->where = a->where;

  switch (a->ts.type) {
  case BT_INTEGER:
    if (mpz_cmp_ui(p->value.integer, 0) == 0) {
      /* Result is processor-dependent */
      g95_error("Second argument MOD at %L is zero", &a->where);
      g95_free_expr(result);
      return &g95_bad_expr;
    }
    mpz_tdiv_r(result->value.integer, a->value.integer, p->value.integer);
    break;

  case BT_REAL:
    if (mpf_cmp_ui(p->value.real, 0) == 0) {
      /* Result is processor-dependent */

      g95_error("Second argument of MOD at %L is zero", &p->where);
      g95_free_expr(result);
      return &g95_bad_expr;
    }

    mpf_init(quot);
    mpf_init(iquot);
    mpf_init(term);

    mpf_div(quot, a->value.real, p->value.real);
    mpf_trunc(iquot, quot);
    mpf_mul(term, iquot, p->value.real);
    mpf_sub(result->value.real, a->value.real, term);

    mpf_clear(quot);
    mpf_clear(iquot);
    mpf_clear(term);
    break;

  default:
    g95_internal_error("g95_simplify_mod(): Bad arguments");
  }

  return range_check(result, "MOD");
}


g95_expr *g95_simplify_modulo(g95_expr *a, g95_expr *p) {
g95_expr *result;
mpf_t quot, iquot, term;

  if (a->expr_type != EXPR_CONSTANT || p->expr_type != EXPR_CONSTANT)
    return NULL;

  result = g95_constant_result(a->ts.type, a->ts.kind);
  result->where = a->where;

  switch (a->ts.type) {
  case BT_INTEGER:
    if (mpz_cmp_ui(p->value.integer, 0) == 0) {
      /* Result is processor-dependent, and this processor doesn't handle it */
      g95_error("Second argument of MODULO at %L is zero", &a->where);
      g95_free_expr(result);
      return &g95_bad_expr;
    }
    mpz_fdiv_r(result->value.integer, a->value.integer, p->value.integer);

    break;

  case BT_REAL:
    if (mpf_cmp_ui(p->value.real, 0) == 0) {
      /* Result is processor-dependent */
      g95_error("Second argument of MODULO at %L is zero", &p->where);
      g95_free_expr(result);
      return &g95_bad_expr;
    }

    mpf_init(quot);
    mpf_init(iquot);
    mpf_init(term);

    mpf_div(quot, a->value.real, p->value.real);
    mpf_floor(iquot, quot);
    mpf_mul(term, iquot, p->value.real);

    mpf_clear(quot);
    mpf_clear(iquot);
    mpf_clear(term);

    mpf_sub(result->value.real, a->value.real, term);
    break;

  default:
    g95_internal_error("g95_simplify_modulo(): Bad arguments");
  }

  return range_check(result, "MODULO");
}


g95_expr *g95_simplify_mvbits(g95_expr *f, g95_expr *fp, g95_expr *l,
			      g95_expr *to, g95_expr *tp) {
  return NULL;
}


g95_expr *g95_simplify_nearest(g95_expr *x, g95_expr *s) {
g95_expr *result;
float rval;
double val, eps;
int p, i, k;

/* FIXME */
/*This implementation is dopey and probably not quite right, but it's a start.*/

  if (x->expr_type != EXPR_CONSTANT) return NULL;

  k = g95_validate_kind(x->ts.type, x->ts.kind);
  if (k == -1) g95_internal_error("g95_simplify_precision(): Bad kind");

  result = g95_constant_result(x->ts.type, x->ts.kind);
  result->where = x->where;

  val  = mpf_get_d(x->value.real);
  p    = g95_real_kinds[k].digits;

  eps = 1.;
  for (i=1;i<p;++i) {
    eps = eps/2.;
  }

  if (mpf_cmp_ui(s->value.real, 0) > 0) {
    if (k == g95_default_real_kind()) {
      rval = (float) val;
      rval = rval + eps;
      mpf_set_d(result->value.real, rval);
    }
    else {
      val = val + eps;
      mpf_set_d(result->value.real, val);
    }
  }
  else if (mpf_cmp_ui(s->value.real, 0) < 0) {
    if (k == g95_default_real_kind()) {
      rval = (float) val;
      rval = rval - eps;
      mpf_set_d(result->value.real, rval);
    }
    else {
      val = val - eps;
      mpf_set_d(result->value.real, val);
    }
  }
  else {
    g95_error("Invalid second argument of NEAREST at %L", &s->where);
    g95_free(result);
    return &g95_bad_expr;
  }

  return range_check(result, "NEAREST");

}


g95_expr *g95_simplify_nint(g95_expr *e, g95_expr *k) {
g95_expr *rtrunc, *itrunc, *result;
int kind, cmp;

  kind = get_kind(BT_REAL, k, "NINT", g95_default_integer_kind());
  if (kind == -1) return &g95_bad_expr;

  if (e->expr_type != EXPR_CONSTANT) return NULL;

  result = g95_constant_result(BT_INTEGER, kind);
  result->where = e->where;

  rtrunc = g95_copy_expr(e);
  itrunc = g95_copy_expr(e);

  cmp = mpf_cmp_ui(e->value.real, 0);

  if (cmp > 0) {
    mpf_add(rtrunc->value.real, e->value.real, mpf_half);
    mpf_trunc(itrunc->value.real, rtrunc->value.real);
  } else if (cmp < 0) {
    mpf_sub(rtrunc->value.real, e->value.real, mpf_half);
    mpf_trunc(itrunc->value.real, rtrunc->value.real);
  } else
    mpf_set_ui(itrunc->value.real, 0);

  mpz_set_f(result->value.integer, itrunc->value.real);

  g95_free_expr(itrunc);
  g95_free_expr(rtrunc);

  return range_check(result, "NINT");
}


g95_expr *g95_simplify_idnint(g95_expr *e) {
g95_expr *rtrunc, *itrunc, *result;
int cmp;

  if (e->expr_type != EXPR_CONSTANT) return NULL;

  result = g95_constant_result(BT_INTEGER, g95_default_integer_kind());
  result->where = e->where;

  rtrunc = g95_copy_expr(e);
  itrunc = g95_copy_expr(e);

  cmp = mpf_cmp_ui(e->value.real, 0);

  if (cmp > 0) {
    mpf_add(rtrunc->value.real, e->value.real, mpf_half);
    mpf_trunc(itrunc->value.real, rtrunc->value.real);
  } else if (cmp < 0) {
    mpf_sub(rtrunc->value.real, e->value.real, mpf_half);
    mpf_trunc(itrunc->value.real, rtrunc->value.real);
  } else
    mpf_set_ui(itrunc->value.real, 0);

  mpz_set_f(result->value.integer, itrunc->value.real);

  g95_free_expr(itrunc);
  g95_free_expr(rtrunc);

  return range_check(result, "IDNINT");
}


g95_expr *g95_simplify_not(g95_expr *e) {
g95_expr *result;

  if (e->expr_type != EXPR_CONSTANT) return NULL;

  result = g95_constant_result(e->ts.type, e->ts.kind);
  result->where = e->where;

  mpz_com(result->value.integer, e->value.integer);

  return range_check(result, "NOT");
}


g95_expr *g95_simplify_null(g95_expr *mold) {
g95_expr *result;

  result = g95_get_expr();
  result->expr_type = EXPR_NULL;

  if (mold == NULL)
    result->ts.type = BT_UNKNOWN;
  else
    result->ts = mold->ts;

  return result;
}


g95_expr *g95_simplify_precision(g95_expr *e) {
g95_expr *result;
int i;

  i = g95_validate_kind(e->ts.type, e->ts.kind);
  if (i == -1) g95_internal_error("g95_simplify_precision(): Bad kind");

  result = g95_int_expr(g95_real_kinds[i].precision);
  result->where = e->where;

  return result;
}


g95_expr *g95_simplify_radix(g95_expr *e) {
g95_expr *result;
int i;

  i = g95_validate_kind(e->ts.type, e->ts.kind);
  if (i < 0) goto bad;

  switch(e->ts.type) {
  case BT_INTEGER:
    i = g95_integer_kinds[i].radix;
    break;

  case BT_REAL:
    i = g95_real_kinds[i].radix;
    break;

  default: bad:
    g95_internal_error("g95_simplify_radix(): Bad type");
  }

  result = g95_int_expr(i);
  result->where = e->where;

  return result;
}


g95_expr *g95_simplify_range(g95_expr *e) {
g95_expr *result;
int i;
long j;

  i = g95_validate_kind(e->ts.type, e->ts.kind);
  if (i < 0) goto bad_type;

  switch(e->ts.type) {
  case BT_INTEGER:
    j = g95_integer_kinds[i].range;
    break;

  case BT_REAL:
  case BT_COMPLEX:
    j = g95_real_kinds[i].range;
    break;

  bad_type:
  default:
    g95_internal_error("g95_simplify_range(): Bad kind");
  }

  result = g95_int_expr(j);
  result->where = e->where;

  return result;
}


g95_expr *g95_simplify_real(g95_expr *e, g95_expr *k) {
g95_expr *result;
int kind;

  kind = get_kind(BT_REAL, k, "REAL", g95_default_real_kind());
  if (kind == -1) return &g95_bad_expr;

  if (e->expr_type != EXPR_CONSTANT) return NULL;

  switch (e->ts.type) {
  case BT_INTEGER:
    result = g95_int2real(e, kind);
    break;

  case BT_REAL:
    result = g95_real2real(e, kind);
    break;

  case BT_COMPLEX:
    result = g95_complex2real(e, kind);
    break;

  default:
    g95_internal_error("bad type in REAL");
    return &g95_bad_expr;
  }

  return range_check(result, "REAL");
}

g95_expr *g95_simplify_repeat(g95_expr *e, g95_expr *n) {
g95_expr *result;
int i, j, len, ncopies, nlen;

  if (e->expr_type != EXPR_CONSTANT || n->expr_type != EXPR_CONSTANT)
    return NULL;

  if (n !=NULL && (g95_extract_int(n, &ncopies) != NULL || ncopies < 0)) {
    g95_error("Invalid second argument of REPEAT at %L", &n->where);
    return &g95_bad_expr;
  }

  len    = e->value.character.length;
  nlen   = ncopies*len;

  result = g95_constant_result(BT_CHARACTER, e->ts.kind);

  if (ncopies == 0) {
    result->value.character.string=g95_getmem(1);
    result->value.character.length=0;
    result->value.character.string='\0';
    return result;
  }

  result->value.character.length=nlen;
  result->value.character.string=g95_getmem(nlen+1);

  for(i=0; i<ncopies; i++)
    for(j=0; j<len; j++)
      result->value.character.string[j+i*len] = e->value.character.string[j];

  result->value.character.string[nlen] = '\0';  /* For debugger */
  return result;
}


/* g95_simplify_reshape()-- This one is a bear, but mainly has to do
 * with shuffling elements. */

g95_expr *g95_simplify_reshape(g95_expr *source, g95_expr *shape_exp,
			       g95_expr *pad, g95_expr *order_exp) {

unsigned long j;
size_t nsource;
int i, rank, npad, x[G95_MAX_DIMENSIONS];
int order[G95_MAX_DIMENSIONS], shape[G95_MAX_DIMENSIONS];
g95_constructor *head, *tail;
g95_array_shape *s;
mpz_t index;
g95_expr *e;

/* Unpack the shape array */

  if (source->expr_type != EXPR_ARRAY || shape_exp->expr_type != EXPR_ARRAY ||
      (pad != NULL && pad->expr_type != EXPR_ARRAY) ||
      (order_exp != NULL && order_exp->expr_type != EXPR_ARRAY))
    return NULL;

  mpz_init(index);
  rank = 0;

  for(;;) {
    e = g95_get_array_element(shape_exp, rank);
    if (e == NULL) break;

    if (rank >= G95_MAX_DIMENSIONS) {
      g95_error("Too many dimensions in shape specification for RESHAPE "
		"at %L", &e->where);

      goto done;
    }

    if (g95_extract_int(e, &shape[rank]) != NULL) {
      g95_error("Integer too large in shape specification at %L",
		&e->where);
      goto done;
    }

    if (shape[rank] < 0) {
      g95_error("Shape specification at %L cannot be negative", &e->where);
      goto done;
    }

    rank++;
  }

  if (rank == 0) {
    g95_error("Shape specification at %L cannot be the null array",
	      &shape_exp->where);
    goto done;
  }

  /* Now unpack the order array if present */

  if (order_exp == NULL) {
    for(i=0; i<rank; i++)
      order[i] = i;

  } else {

    for(i=0; i<rank; i++)
      x[i] = 0;

    for(i=0; i<rank; i++) {
      e = g95_get_array_element(order_exp, i);
      if (e == NULL) {
	g95_error("ORDER parameter of RESHAPE at %L is not the same size "
		  "as SHAPE parameter", &e->where);
	goto done;
      }

      if (g95_extract_int(e, &order[i]) != NULL) {
	g95_error("Error in ORDER parameter of RESHAPE at %L", &e->where);
	goto done;
      }

      if (order[i] < 1 || order[i] > rank) {
	g95_error("ORDER parameter of RESHAPE at %L is out of range",
		  &e->where);
	goto done;
      }

      order[i]--;

      if (x[order[i]]) {
	g95_error("Invalid permutation in ORDER parameter at %L", &e->where);
	goto done;
      }

      x[order[i]] = 1;
    }
  }

  /* Count the elements in the source and padding arrays */

  npad = 0;

  if (pad != NULL)
    for(;; npad++)
      if (g95_get_array_element(pad, npad) == NULL) break;

  nsource = 0;

  head = tail = NULL;

  for(;; nsource++)
    if (g95_get_array_element(source, nsource) == NULL) break;

  /* If it weren't for that pesky permutation we could just loop
   * through the source and round out any shortage with pad elements.
   * But no, someone just had to have the compiler do something the
   * user should be doing. */

  for(i=0; i<rank; i++)
    x[i] = 0;

  for(;;) {

    /* Figure out which element to extract */

    mpz_set_ui(index, 0);

    for(i=rank-1; i>=0; i--) {
      mpz_add_ui(index, index, x[order[i]]);
      if (i != 0) mpz_mul_ui(index, index, shape[order[i-1]]);
    }

    if (mpz_cmp_ui(index, INT_MAX) > 0) {
      g95_internal_error("Reshaped array too large at %L", &e->where);
      goto done;
    }

    j = mpz_get_ui(index);

    if (j < nsource)
      e = g95_get_array_element(source, j);
    else {
      j = j - nsource;

      if (npad == 0) {
	g95_error("PAD parameter required for short SOURCE parameter at %L",
		  &source->where);
	goto done;
      }

      j = j % npad;
      e = g95_get_array_element(pad, j);
    }

    if (head == NULL)
      head = tail = g95_get_constructor();
    else {
      tail->next = g95_get_constructor();
      tail = tail->next;
    }

    tail->where = e->where;
    tail->expr = g95_copy_expr(e);

    /* Calculate the next element */

    i = 0;
  inc:
    if (++x[i] < shape[i]) continue;
    x[i++] = 0;
    if (i < rank) goto inc;

    break;
  }

  mpz_clear(index);

  e = g95_get_expr();
  e->where = source->where;
  e->expr_type = EXPR_ARRAY;
  e->value.constructor = head;
  e->ts = head->expr->ts;

  e->rank = rank;

  /* TODO: Reshape is broken-- need to be able to specify the shape of
   * array constants */

  s = NULL;

#if 0
  for(i=0; i<rank; i++)
    s->shape[i] = g95_int_expr(shape[i]);
#endif


  return e;

done:
  mpz_clear(index);
  return &g95_bad_expr;
}


g95_expr *g95_simplify_rrspacing(g95_expr *x) {
g95_expr *result;
mpf_t i2, absv, ln2, lnx, frac, pow2;
unsigned long exp2;
int i, p;

  if (x->expr_type != EXPR_CONSTANT) return NULL;

  i = g95_validate_kind(x->ts.type, x->ts.kind);
  if (i < 0) g95_internal_error("g95_simplify_rrspacing(): bad kind");

  result=g95_constant_result(BT_REAL, x->ts.kind);
  result->where = x->where;

  p = g95_real_kinds[i].digits;

  if (mpf_cmp(x->value.real, mpf_zero) == 0) {
    mpf_ui_div(result->value.real, 1, g95_real_kinds[i].tiny);
    return result;
  }

  mpf_init_set_ui(i2, 2);
  mpf_init(ln2);
  mpf_init(absv);
  mpf_init(lnx);
  mpf_init(frac);
  mpf_init(pow2);

  natural_logarithm(&i2, &ln2);

  mpf_abs(absv, x->value.real);
  natural_logarithm(&absv, &lnx);

  mpf_div(lnx, lnx, ln2);
  mpf_trunc(lnx, lnx);
  mpf_add_ui(lnx, lnx, 1);

  exp2 = (unsigned long) mpf_get_d(lnx);
  mpf_pow_ui(pow2, i2, exp2);
  mpf_div(frac, absv, pow2);

  exp2 = (unsigned long) p;
  mpf_mul_2exp(result->value.real, frac, exp2);

  mpf_clear(i2);
  mpf_clear(ln2);
  mpf_clear(absv);
  mpf_clear(lnx);
  mpf_clear(frac);
  mpf_clear(pow2);

  return range_check(result, "RRSPACING");
}


g95_expr *g95_simplify_scale(g95_expr *x, g95_expr *i) {
int k, neg_flag, power, exp_range;
mpf_t scale, radix;
g95_expr *result;

  if (x->expr_type != EXPR_CONSTANT || i->expr_type != EXPR_CONSTANT)
    return NULL;

  result = g95_constant_result(BT_REAL, x->ts.kind);
  result->where = x->where;

  if (mpf_sgn(x->value.real) == 0) {
    mpf_set_ui(result->value.real, 0);
    return result;
  }

  k = g95_validate_kind(BT_REAL, x->ts.kind);
  exp_range = g95_real_kinds[k].max_exponent - g95_real_kinds[k].min_exponent;

  /* This check filters out values of i that would overflow an int */

  if (mpz_cmp_si(i->value.integer, exp_range+2) > 0 ||
      mpz_cmp_si(i->value.integer, -exp_range-2) < 0) {
    g95_error("Result of SCALE overflows its kind at %L", &result->where);
    return &g95_bad_expr;
  }

  /* Compute scale = radix ** power */

  power = mpz_get_si(i->value.integer);

  if (power >= 0)
    neg_flag = 0;
  else {
    neg_flag = 1;
    power = -power;
  }

  mpf_init_set_ui(radix, g95_real_kinds[k].radix);
  mpf_init(scale);
  mpf_pow_ui(scale, radix, power);

  if (neg_flag)
    mpf_div(result->value.real, x->value.real, scale);
  else
    mpf_mul(result->value.real, x->value.real, scale);

  mpf_clear(scale);
  mpf_clear(radix);

  return range_check(result, "SCALE");
}


g95_expr *g95_simplify_scan(g95_expr *e, g95_expr *c, g95_expr *b) {
g95_expr *result;
int back;
size_t indx, len, lenc;

  if (e->expr_type != EXPR_CONSTANT || c->expr_type != EXPR_CONSTANT)
    return NULL;

  if (b != NULL && b->value.logical != 0)
    back = 1;
  else
    back = 0;

  result = g95_constant_result(BT_INTEGER, g95_default_integer_kind());
  result->where = e->where;

  len  = e->value.character.length;
  lenc = c->value.character.length;

  if (len == 0 || lenc == 0) {
    indx = 0;
  } else {
    indx = strcspn(e->value.character.string, c->value.character.string) + 1;
    if (indx > len) indx=0;
    if (back != 0 && indx != 0) indx = len - indx + 1;
  }

  mpz_set_ui(result->value.integer, indx);
  return range_check(result, "SCAN");
}


g95_expr *g95_simplify_selected_int_kind(g95_expr *e) {
int i, kind, range;
g95_expr *result;

  if (e->expr_type != EXPR_CONSTANT || g95_extract_int(e, &range) != NULL)
    return NULL;

  kind = INT_MAX;

  for(i=0; g95_integer_kinds[i].kind!=0; i++)
    if (g95_integer_kinds[i].range >= range &&
	g95_integer_kinds[i].kind < kind) kind = g95_integer_kinds[i].kind;

  if (kind == INT_MAX) kind = -1;

  result = g95_int_expr(kind);
  result->where = e->where;

  return result;
}


g95_expr *g95_simplify_selected_real_kind(g95_expr *p, g95_expr *q) {
int range, precision, i, kind, found_precision, found_range;
g95_expr *result;

  if (p == NULL)
    precision = 0;
  else {
    if (p->expr_type != EXPR_CONSTANT ||
	g95_extract_int(p, &precision) != NULL) return NULL;
  }

  if (q == NULL)
    range = 0;
  else {
    if (q->expr_type != EXPR_CONSTANT ||
	g95_extract_int(q, &range) != NULL) return NULL;
  }

  kind = INT_MAX;
  found_precision = 0;
  found_range = 0;

  for(i=0; g95_real_kinds[i].kind!=0; i++) {
    if (g95_real_kinds[i].precision >= precision) found_precision = 1;

    if (g95_real_kinds[i].range >= range) found_range = 1;

    if (g95_real_kinds[i].precision >= precision &&
	g95_real_kinds[i].range >= range &&
	g95_real_kinds[i].kind < kind)
      kind = g95_real_kinds[i].kind;
  }

  if (kind == INT_MAX) {
    kind = 0;

    if (!found_precision) kind = -1;
    if (!found_range) kind -= 2;
  }

  result = g95_int_expr(kind);
  result->where = (p != NULL) ? p->where : q->where;

  return g95_int_expr(kind);
}


g95_expr *g95_simplify_set_exponent(g95_expr *x, g95_expr *i) {
g95_expr *result;
mpf_t i2, ln2, absv, lnx, pow2, frac;
unsigned long exp2;

  if (x->expr_type != EXPR_CONSTANT || i->expr_type != EXPR_CONSTANT)
    return NULL;

  result=g95_constant_result(BT_REAL, x->ts.kind);
  result->where = x->where;

  if (mpf_cmp(x->value.real, mpf_zero) == 0) {
    mpf_set(result->value.real, mpf_zero);
    return result;
  }

  mpf_init_set_ui(i2, 2);
  mpf_init(ln2);
  mpf_init(absv);
  mpf_init(lnx);
  mpf_init(pow2);
  mpf_init(frac);

  natural_logarithm(&i2, &ln2);

  mpf_abs(absv, x->value.real);
  natural_logarithm(&absv, &lnx);

  mpf_div(lnx, lnx, ln2);
  mpf_trunc(lnx, lnx);
  mpf_add_ui(lnx, lnx, 1);

/* old exponent value, and fraction */
  exp2 = (unsigned long) mpf_get_d(lnx);
  mpf_pow_ui(pow2, i2, exp2);

  mpf_div(frac, absv, pow2);

/* New exponent */
  exp2 = (unsigned long) mpz_get_d(i->value.integer);
  mpf_mul_2exp(result->value.real, frac, exp2);

  mpf_clear(i2);
  mpf_clear(ln2);
  mpf_clear(absv);
  mpf_clear(lnx);
  mpf_clear(pow2);
  mpf_clear(frac);

  return range_check(result, "SET_EXPONENT");
}


g95_expr *g95_simplify_sign(g95_expr *x, g95_expr *y) {
g95_expr *absv, *result;
mpz_t sgnz;
mpf_t sgnf;
int sgn;

  if (x->expr_type != EXPR_CONSTANT || y->expr_type != EXPR_CONSTANT)
    return NULL;

  absv = g95_copy_expr(x);

  result = g95_constant_result(x->ts.type, x->ts.kind);
  result->where = x->where;

  switch(x->ts.type) {
  case BT_INTEGER:
    sgn = mpz_sgn(y->value.integer);
    mpz_init_set_si(sgnz, sgn);
    mpz_abs(absv->value.integer, x->value.integer);
    mpz_mul(result->value.integer, absv->value.integer, sgnz);
    mpz_clear(sgnz);
    break;

  case BT_REAL:
    sgn = mpf_sgn(y->value.real);
    mpf_abs(absv->value.real, x->value.real);
    mpf_init_set_si(sgnf, sgn);
    mpf_mul(result->value.real, absv->value.real, sgnf);
    mpf_clear(sgnf);
    break;

  default:
    g95_internal_error("Bad type in g95_simplify_sign");
    g95_free_expr(result);
    return &g95_bad_expr;
  }

  g95_free_expr(absv);
  return result;
}


g95_expr *g95_simplify_sin(g95_expr *x) {
g95_expr *result;
mpf_t xp, xq;

  if (x->expr_type != EXPR_CONSTANT) return NULL;

  result = g95_constant_result(x->ts.type, x->ts.kind);
  result->where = x->where;

  switch (x->ts.type) {
  case BT_REAL: 
    sine(&x->value.real, &result->value.real);
    break;

  case BT_COMPLEX: 
    mpf_init(xp);
    mpf_init(xq);

    sine(&x->value.complex.r, &xp);
    hypercos(&x->value.complex.i, &xq);
    mpf_mul(result->value.complex.r, xp, xq);

    cosine(&x->value.complex.r, &xp);
    hypersine(&x->value.complex.i, &xq);
    mpf_mul(result->value.complex.i, xp, xq);

    mpf_clear(xp);
    mpf_clear(xq);
    break;

  default:
    g95_internal_error("in g95_simplify_sin(): Bad type");
  }

  return range_check(result, "SIN");
}


g95_expr *g95_simplify_sinh(g95_expr *x) {
g95_expr *result;

  if (x->expr_type != EXPR_CONSTANT) return NULL;

  result = g95_constant_result(x->ts.type, x->ts.kind);
  result->where = x->where;

  hypersine(&x->value.real, &result->value.real);

  return range_check(result, "SINH");
}


/* simplify_sngl()-- The argument is always a double precision real
 * that is converted to single precision.  TODO: Rounding! */

g95_expr *g95_simplify_sngl(g95_expr *a) {
g95_expr *result;

  if (a->expr_type != EXPR_CONSTANT) return NULL;

  result=g95_real2real(a, g95_default_real_kind());
  return range_check(result, "SNGL");
}


g95_expr *g95_simplify_spacing(g95_expr *x) {
g95_expr *result;
mpf_t i1, i2, ln2, absv, lnx;
long diff;
unsigned long exp2;
int i, p;

  if (x->expr_type != EXPR_CONSTANT) return NULL;

  i = g95_validate_kind(x->ts.type, x->ts.kind);
  if (i < 0) g95_internal_error("g95_simplify_spacing(): bad kind");

  p = g95_real_kinds[i].digits;

  result=g95_constant_result(BT_REAL, x->ts.kind);
  result->where = x->where;

  if (mpf_cmp(x->value.real, mpf_zero) == 0) {
    mpf_set(result->value.real, g95_real_kinds[i].tiny);
    return result;
  }

  mpf_init_set_ui(i1, 1);
  mpf_init_set_ui(i2, 2);
  mpf_init(ln2);
  mpf_init(absv);
  mpf_init(lnx);

  natural_logarithm(&i2, &ln2);

  mpf_abs(absv, x->value.real);
  natural_logarithm(&absv, &lnx);

  mpf_div(lnx, lnx, ln2);
  mpf_trunc(lnx, lnx);
  mpf_add_ui(lnx, lnx, 1);

  diff = (long) mpf_get_d(lnx) - (long) p;
  if (diff >= 0) {
    exp2 = (unsigned) diff;
    mpf_mul_2exp(result->value.real, i1, exp2);
  } else {
    diff = -diff;
    exp2 = (unsigned) diff;
    mpf_div_2exp(result->value.real, i1, exp2);
  }

  mpf_clear(i1);
  mpf_clear(i2);
  mpf_clear(ln2);
  mpf_clear(absv);
  mpf_clear(lnx);

  if (mpf_cmp(result->value.real, g95_real_kinds[i].tiny) < 0)
    mpf_set(result->value.real, g95_real_kinds[i].tiny);

  return range_check(result, "SPACING");
}


g95_expr *g95_simplify_sqrt(g95_expr *e) {
g95_expr *result;
mpf_t ac, ad, s, t, w;

  if (e->expr_type != EXPR_CONSTANT) return NULL;

  result = g95_constant_result(e->ts.type, e->ts.kind);
  result->where = e->where;

  switch (e->ts.type) {
  case BT_REAL:
    if (mpf_cmp_si(e->value.real, 0) < 0) goto negative_arg;
    mpf_sqrt(result->value.real, e->value.real);

    break;

  case BT_COMPLEX:
    /*Formula taken from Numerical Recipes to avoid over- and underflow*/

    mpf_init(ac);
    mpf_init(ad);
    mpf_init(s);
    mpf_init(t);
    mpf_init(w);

    if (mpf_cmp_ui(e->value.complex.r, 0) == 0 &&
	mpf_cmp_ui(e->value.complex.i, 0) == 0) {

      mpf_set_ui(result->value.complex.r, 0);
      mpf_set_ui(result->value.complex.i, 0);
      break;
    }

    mpf_abs(ac, e->value.complex.r);
    mpf_abs(ad, e->value.complex.i);

    if (mpf_cmp(ac, ad) >= 0) {
      mpf_div(t, e->value.complex.i, e->value.complex.r);
      mpf_mul(t, t, t);
      mpf_add_ui(t, t, 1);
      mpf_sqrt(t, t);
      mpf_add_ui(t, t, 1);
      mpf_div_ui(t, t, 2);
      mpf_sqrt(t, t);
      mpf_sqrt(s, ac);
      mpf_mul(w, s, t);
    } else {
      mpf_div(s, e->value.complex.r, e->value.complex.i);
      mpf_mul(t, s, s);
      mpf_add_ui(t, t, 1);
      mpf_sqrt(t, t);
      mpf_abs(s, s);
      mpf_add(t, t, s);
      mpf_div_ui(t, t, 2);
      mpf_sqrt(t, t);
      mpf_sqrt(s, ad);
      mpf_mul(w, s, t);
    }

    if (mpf_cmp_ui(w, 0) !=0 && mpf_cmp_ui(e->value.complex.r, 0) >=0) {
      mpf_mul_ui(t, w, 2);
      mpf_div(result->value.complex.i, e->value.complex.i, t);
      mpf_set(result->value.complex.r, w);
    } else if (mpf_cmp_ui(w, 0) !=0 && mpf_cmp_ui(e->value.complex.r, 0) < 0 &&
	       mpf_cmp_ui(e->value.complex.i, 0) >= 0) {
      mpf_mul_ui(t, w, 2);
      mpf_div(result->value.complex.r, e->value.complex.i, t);
      mpf_set(result->value.complex.i, w);
    } else if (mpf_cmp_ui(w, 0) !=0 && mpf_cmp_ui(e->value.complex.r, 0) < 0 &&
	       mpf_cmp_ui(e->value.complex.i, 0) < 0) {
      mpf_mul_ui(t, w, 2);
      mpf_div(result->value.complex.r, ad, t);
      mpf_neg(w, w);
      mpf_set(result->value.complex.i, w);
    } else {
      g95_internal_error("invalid complex argument of SQRT at %L",
			 &e->where);
      mpf_clear(s);  mpf_clear(t); mpf_clear(ac);
      mpf_clear(ad); mpf_clear(w);
      g95_free_expr(result);
      return &g95_bad_expr;
    }

    mpf_clear(s);
    mpf_clear(t);
    mpf_clear(ac);
    mpf_clear(ad);
    mpf_clear(w);

    break;

  default:
    g95_internal_error("invalid argument of SQRT at %L", &e->where);
    g95_free_expr(result);
    return &g95_bad_expr;
  }

  return range_check(result, "SQRT");

 negative_arg:
  g95_free_expr(result);
  g95_error("Argument of SQRT at %L has a negative value", &e->where);
  return &g95_bad_expr;
}


g95_expr *g95_simplify_tan(g95_expr *x) {
g95_expr *result;
mpf_t mpf_sin, mpf_cos, mag_cos;
int i;

  if (x->expr_type != EXPR_CONSTANT) return NULL;

  i = g95_validate_kind(BT_REAL, x->ts.kind);
  if (i == -1) g95_internal_error("g95_simplify_tan(): bad kind");

  result = g95_constant_result(x->ts.type, x->ts.kind);
  result->where = x->where;

  mpf_init(mpf_sin);
  mpf_init(mpf_cos);
  mpf_init(mag_cos);
  sine(&x->value.real, &mpf_sin);
  cosine(&x->value.real, &mpf_cos);
  mpf_abs(mag_cos, mpf_cos);
  if (mpf_cmp_ui(mag_cos, 0) == 0) {
    g95_error("Tangent undefined at %L", &x->where);
    mpf_clear(mpf_sin);
    mpf_clear(mpf_cos);
    mpf_clear(mag_cos);
    g95_free_expr(result);
    return &g95_bad_expr;
  } else if (mpf_cmp(mag_cos, g95_real_kinds[i].tiny) < 0) {
    g95_error("Tangent cannot be accurately evaluated at %L", &x->where);
    mpf_clear(mpf_sin);
    mpf_clear(mpf_cos);
    mpf_clear(mag_cos);
    g95_free_expr(result);
    return &g95_bad_expr;
  } else {
    mpf_div(result->value.real, mpf_sin, mpf_cos);
    mpf_clear(mpf_sin);
    mpf_clear(mpf_cos);
    mpf_clear(mag_cos);
  }

  return range_check(result, "TAN");
}


g95_expr *g95_simplify_tanh(g95_expr *x) {
g95_expr *result;
mpf_t xp, xq;

  if (x->expr_type != EXPR_CONSTANT) return NULL;

  result = g95_constant_result(x->ts.type, x->ts.kind);
  result->where = x->where;

  mpf_init(xp);
  mpf_init(xq);

  hypersine(&x->value.real, &xq);
  hypercos(&x->value.real, &xp);

  mpf_div(result->value.real, xq, xp);

  mpf_clear(xp);
  mpf_clear(xq);

  return range_check(result, "TANH");

}


g95_expr *g95_simplify_tiny(g95_expr *e) {
g95_expr *result;
int i;

  i = g95_validate_kind(BT_REAL, e->ts.kind);
  if (i < 0) g95_internal_error("g95_simplify_error(): bad kind");

  result = g95_constant_result(BT_REAL, e->ts.kind);
  mpf_init_set(result->value.real, g95_real_kinds[i].tiny);
  result->where = e->where;

  return result;
}


g95_expr *g95_simplify_trim(g95_expr *e) {
g95_expr *result;
int count, i, len, lentrim;

  if (e->expr_type != EXPR_CONSTANT) return NULL;

  len = e->value.character.length;

  result = g95_constant_result(BT_CHARACTER, e->ts.kind);
  result->where = e->where;

  for (count=0, i=1; i<=len; ++i) {
    if (e->value.character.string[len-i] == ' ')
      count++;
    else
      break;
  }

  lentrim = len-count;

  result->value.character.length = lentrim;
  result->value.character.string = g95_getmem(lentrim+1);

  for(i=0; i<lentrim; i++)
    result->value.character.string[i] = e->value.character.string[i];

  result->value.character.string[lentrim] = '\0';   /* For debugger */

  return result;
}


g95_expr *g95_simplify_verify(g95_expr *s, g95_expr *set, g95_expr *b) {
g95_expr *result;
int back;
size_t index=0, len, lenset;

  if (s->expr_type != EXPR_CONSTANT || set->expr_type != EXPR_CONSTANT)
    return NULL;

  if (b != NULL && b->value.logical != 0)
    back = 1;
  else
    back = 0;

  result = g95_constant_result(BT_INTEGER, g95_default_integer_kind());
  result->where = s->where;

  len    = s->value.character.length;
  lenset = set->value.character.length;

  if (len == 0) {
    mpz_set_ui(result->value.integer, 0);
    return result;
  }

  if (back == 0) {
    if (lenset == 0) {
      mpz_set_ui(result->value.integer, len);
      return result;
    }

    index = strspn(s->value.character.string, set->value.character.string) + 1;
    if (index > len) index = 0;

  } else {
    if (lenset == 0) {
      mpz_set_ui(result->value.integer, 1);
      return result;
    }

    index = len-strspn(s->value.character.string, set->value.character.string);
  }

  mpz_set_ui(result->value.integer, index);
  return result;
}


/****************************************************************************
 * Helper functions
 */


/* invert_table()-- Given a collating table, create the inverse table */

static void invert_table(int *table, int *xtable) {
int i;

  for(i=0; i<256; i++)
    xtable[i] = 0;

  for(i=0; i<256; i++)
    xtable[table[i]] = i;
}


void g95_simplify_init_1(void) {

  integer_zero = g95_convert_integer("0", g95_default_integer_kind(), 10);
  real_zero = g95_convert_real("0.0", g95_default_real_kind());

  mpf_init_set_str(mpf_zero, "0.0", 10);
  mpf_init_set_str(mpf_half, "0.5", 10);
  mpf_init_set_str(mpf_one,  "1.0", 10);
  mpz_init_set_str(mpz_zero,   "0", 10);

  invert_table(ascii_table, xascii_table);
}


void g95_simplify_done_1(void) {

  mpf_clear(mpf_pi);
  mpf_clear(mpf_hpi);
  mpf_clear(mpf_nhpi);

  mpf_clear(mpf_zero);
  mpf_clear(mpf_half);
  mpf_clear(mpf_one);
  mpz_clear(mpz_zero);

  g95_free_expr(integer_zero);
  g95_free_expr(real_zero);
}
