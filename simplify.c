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
#include "simplify.h"

#include <ctype.h>
#include <string.h>


extern g95_integer_info g95_integer_kinds[];
extern g95_real_info g95_real_kinds[];

#define FIRST_ARG(e) (e->value.function.actual->expr)
#define SECOND_ARG(e) (e->value.function.actual->next->expr)
#define THIRD_ARG(e) (e->value.function.actual->next->next->expr)

static g95_expr *integer_zero, *real_zero;
static mpf_t mpf_zero, mpf_half, mpf_one, mpf_pi, mpf_hpi, mpf_nhpi;
static mpz_t mpz_zero;

g95_expr g95_bad_expr;


/* Note that 'simplification' is not just transforming expressions.
 * For functions that are not simplified at compile-time like asin(),
 * range checking is done if possible.
 *
 * The return convention is that each simplification function returns:
 *
 *   A new expression node corresponding to the simplified arguments.
 *   The original arguments are destroyed by the caller, and must not
 *   be a part of the new expression.  Use g95_copy_expr() if
 *   necessary.
 *
 *   NULL pointer indicating that no simplifcation was possible and
 *   the original expression should remain intact.  For example,
 *   sqrt(1.0).
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
 * Array arguments are never passed to these subroutines.
 */

/* FIXME -- a static table is a really stupid implementation of achar but
 * it should work for a start */

static char ascii_table[128] = {
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


/* range_check()-- Range checks an expression node.  If all goes well,
 * returns the node, otherwise returns &g95_bad_expr and frees the node.  */

static g95_expr *range_check(g95_expr *result, char *name) {

  switch(result->ts.type) {
  case BT_REAL:
    if (g95_check_real_range(result->value.real, result->ts.kind) == ARITH_OK)
      return result;
    break;

  case BT_INTEGER:
    if (g95_check_integer_range(result->value.integer, result->ts.kind)
	== ARITH_OK) return result;
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

static int get_kind(g95_expr *k, char *name, int default_kind) {
int kind;

  if (k == NULL) return default_kind;

  if (k->expr_type != EXPR_CONSTANT) {
    g95_error("KIND parameter of %s at %L must be an initialization "
	      "expression", name, &k->where);

    return -1;
  }

  if (g95_extract_int(k, &kind) != NULL ||
      g95_validate_kind(BT_REAL, kind) < 0) {

    g95_error("Invalid KIND parameter of %s at %L", name, &k->where);
    return -1;
  }

  return kind;
}

/*********************** Simplification functions ******************************/


/* The abs family*/

g95_expr *g95_simplify_iabs(g95_expr *e) {
g95_expr *result;

  if (e->expr_type != EXPR_CONSTANT) return NULL;

  result = g95_copy_expr(e);

  mpz_abs(result->value.integer, e->value.integer);

  if (g95_check_integer_range(result->value.integer, result->ts.kind)
      != ARITH_OK) {
    g95_error("Result of ABS() overflows its kind at %L", &e->where);

    g95_free_expr(result);
    return &g95_bad_expr;
  }

  return result;
}


g95_expr *g95_simplify_rabs(g95_expr *e) {
g95_expr *result;

  if (e->expr_type != EXPR_CONSTANT) return NULL;

  result = g95_copy_expr(e);

  mpf_abs(result->value.real, e->value.real);

  return range_check(result, "ABS");
}


g95_expr *g95_simplify_cabs(g95_expr *e) {
g95_expr *result;
mpf_t a, b;

  if (e->expr_type != EXPR_CONSTANT) return NULL;

  result = g95_constant_result(BT_REAL, e->ts.kind);
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

  if (g95_extract_int(e, &index) != NULL  || index < 0 || index > 127  ) {
      g95_error("Extended ASCII not implemented: argument of ACHAR at %L must be between 0 and 127", &e->where);
      return &g95_bad_expr;
  }

/* FIXME This is a limited and stupid implementation, but it's a start. */

    result = g95_constant_result(BT_CHARACTER, g95_default_character_kind());
    result->where = e->where;

    result->value.character.string = g95_getmem(2);

    result->value.character.length = 1;
    result->value.character.string[0] = ascii_table[index];
    result->value.character.string[1] = '\0';   /* For debugger */
    return result;

}


g95_expr *g95_simplify_acos(g95_expr *e) {

  if (e->expr_type != EXPR_CONSTANT) return NULL; 

/* Range checking--  Must have abs(x)<=1 */
 
  if (mpf_cmp_si(e->value.real, 1) > 0 || mpf_cmp_si(e->value.real, -1) < 0) {
    g95_error("Argument of ACOS at %L must be between -1 and 1", &e->where);
    return &g95_bad_expr;
  }

  return NULL;
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

  for ( count=0, i=0; i<len; ++i ) {
    ch = e->value.character.string[i];
    if ( ch != ' ' ) break;
    ++count;
  }

  for ( i=0; i<len-count; ++i) {
    result->value.character.string[i] = e->value.character.string[count+i];
  }

  for ( i=len-count; i<len; ++i) {
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

  for ( count=0, i=len-1; i>=0; --i ) {
    ch = e->value.character.string[i];
    if ( ch != ' ' ) break;
    ++count;
  }

  for ( i=0; i<count; ++i) {
    result->value.character.string[i] = ' ';
  }

  for ( i=count; i<len; ++i) {
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

  return result;
}


g95_expr *g95_simplify_aint(g95_expr *e, g95_expr *k) {
g95_expr *rtrunc, *result;
int kind;

/* Because the kind parameter has to be known at compile-time, we make
 * sure this is so before seeing if e is non-constant. */

  kind = get_kind(k, "AINT", e->ts.kind);
  if (kind == -1) return &g95_bad_expr;

  if (e->expr_type != EXPR_CONSTANT) return NULL;

  rtrunc = g95_constant_result(BT_REAL, kind);
  rtrunc->where = e->where;

  mpf_trunc(rtrunc->value.real, e->value.real);

  result=g95_real2real(rtrunc,kind);
  if ( result == NULL ) {
    g95_error("Result of AINT() overflows its kind at %L", &e->where);
    g95_free_expr(rtrunc);
    return &g95_bad_expr;
  }
  else {
    g95_free_expr(rtrunc);
    return range_check(result,"AINT");
  }
}


g95_expr *g95_simplify_anint(g95_expr *e, g95_expr *k) {
g95_expr *rtrunc, *result;
int kind;

  kind = get_kind(k, "ANINT", e->ts.kind);
  if (kind == -1) return &g95_bad_expr;

  if (e->expr_type != EXPR_CONSTANT) return NULL;

  rtrunc = g95_copy_expr(e);

  if ( mpf_cmp_si(e->value.real,0) > 0 ) {
    mpf_ceil(rtrunc->value.real,e->value.real);
    }
  else if ( mpf_cmp_si(e->value.real,0) < 0 ) {
    mpf_floor(rtrunc->value.real,e->value.real);
  }
  else 
    mpf_set_si(rtrunc->value.real,0);

  result=g95_real2real(rtrunc,kind);
  if ( result == NULL ) {
    g95_error("Result of ANINT() overflows its kind at %L", &e->where);
    g95_free_expr(rtrunc);
    return &g95_bad_expr;
  }
  else {
    g95_free_expr(rtrunc);
    return range_check(result,"ANINT");
  }
}


g95_expr *g95_simplify_asin(g95_expr *e) {

  return NULL;

/* Range checking--  Must have abs(x)<=1 */
 
  /* Evaluation of transcendentals not implemented yet 
  if (e->expr_type != EXPR_CONSTANT) return NULL; 
  if (mpf_cmp_si(e->value.real, 1) > 0 || mpf_cmp_si(e->value.real, -1) < 0) {
    g95_error("Argument of ASIN at %L must be between -1 and 1", &e->where);
    return &g95_bad_expr;
  }
  */
}


g95_expr *g95_simplify_atan2(g95_expr *y, g95_expr *x) {

  return NULL;

  /* Evaluation of transcendentals not implemented yet 
  if ( x == NULL ) {
    g95_error("Second argument of ATAN2 missing at %L", &x->where);
    return &g95_bad_expr;
  }

  if (x->expr_type != EXPR_CONSTANT || y->expr_type != EXPR_CONSTANT) return NULL; 
  if (x->ts.kind != y->ts.kind) {
    g95_error("KIND of arguments of ATAN2 at %L must agree", &x->where);
    return &g95_bad_expr;
  }
  */

}


g95_expr *g95_simplify_bit_size(g95_expr *e) {
int i;

  i = g95_validate_kind(BT_INTEGER, e->ts.kind);
  if (i < 0) g95_internal_error("Bad integer kind in g95_simplify_bit_size()");

  return g95_int_expr(g95_integer_kinds[i].bit_size);
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

  kind = get_kind(k, "CEILING", g95_default_real_kind());
  if (kind == -1) return &g95_bad_expr;

  if (e->ts.type != EXPR_CONSTANT) return NULL;

  result = g95_constant_result(BT_INTEGER, kind);
  result->where = e->where;
  
  ceil = g95_copy_expr(e);
  mpf_ceil(ceil->value.real, e->value.real);
  result=g95_real2int(ceil, kind);

  g95_free_expr(ceil);

  return range_check(result, "CEILING");
}


g95_expr *g95_simplify_char(g95_expr *e, g95_expr *k) {
g95_expr *result;
int c, kind; 

  kind = g95_default_character_kind();

  if (k != NULL) {
    if (k->expr_type != EXPR_CONSTANT) {
      g95_error("KIND parameter of CHAR at %L must be an initialization "
		"expression", &k->where);

      return &g95_bad_expr;
    }

    if (g95_extract_int(k, &kind) != NULL ||
	g95_validate_kind(BT_CHARACTER, kind) < 0) {

      g95_error("Invalid KIND parameter of CHAR at %L", &k->where);
      return &g95_bad_expr;
    }
  }

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


g95_expr *g95_simplify_cmplx(g95_expr *x, g95_expr *y, g95_expr *k) {
g95_expr *result;
int kind;

  kind = get_kind(k, "CMPLX", g95_default_real_kind()); 
  if (kind == -1) return &g95_bad_expr;

  if (x->expr_type != EXPR_CONSTANT) return NULL; 
  if ( y!= NULL && y->expr_type != EXPR_CONSTANT) return NULL; 

  if (y == NULL) {
  switch (x->ts.type) {
	case BT_COMPLEX:
	      result = g95_complex2complex(x,kind); 
              return result;
	case BT_INTEGER:
	      result = g95_int2complex(x,kind);
              return result;
	case BT_REAL:
	      result = g95_real2complex(x, kind);
              return result;
	default :
	      g95_error("Argument of CMPLX at %L is not a valid type",
			      &x->where);
	      return &g95_bad_expr;
  }
  }
  else {
  switch (x->ts.type) {
	case BT_COMPLEX:
      	      g95_error("CMPLX at %L cannot take two arguments if the first is complex",
  	 	  &x->where);
              return &g95_bad_expr;
	case BT_INTEGER:
	      result = g95_constant_result(BT_COMPLEX, kind);
  	      result->where = x->where;
	      mpf_set_z(result->value.complex.r, x->value.integer);
	      if ( y->ts.type == BT_INTEGER ) 
      	        mpf_set_z(result->value.complex.i, y->value.integer);
	      if ( y->ts.type == BT_REAL )
      	        mpf_set(result->value.complex.i, y->value.real);
              return result;
	case BT_REAL:
	      result = g95_constant_result(BT_COMPLEX, kind);
  	      result->where = x->where;
	      mpf_set(result->value.complex.r, x->value.real);
	      if ( y->ts.type == BT_INTEGER ) 
      	        mpf_set_z(result->value.complex.i, y->value.integer);
	      if (y->ts.type == BT_REAL )
      	        mpf_set(result->value.complex.i, y->value.real);
              return result;
	default :
	      g95_error("Argument of CMPLX at %L is not a valid type",
			      &x->where);
	      return &g95_bad_expr;
  }
  }

}


g95_expr *g95_simplify_conjg(g95_expr *e) {
g95_expr *result;

  if (e->expr_type != EXPR_CONSTANT) return NULL;

  result = g95_copy_expr(e);
  mpf_neg(result->value.complex.i, result->value.complex.i);

  return result;
}


g95_expr *g95_simplify_cos(g95_expr *e) {
/* We don't have an extension to return mathematical functions for constant
 * arguments yet */

  return NULL;
}


g95_expr *g95_simplify_cosh(g95_expr *e) {
  /* Ditto */

  return NULL;
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
	          g95_error("Argument of DBLE at %L is not a valid type",
			      &e->where);
		  return &g95_bad_expr;
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

  if (x->expr_type != EXPR_CONSTANT || y->expr_type != EXPR_CONSTANT) return NULL;

  if ( y == NULL ) {
    g95_error("Second argument of DIM missing at %L", &x->where);
    return &g95_bad_expr;
  }

  result = g95_constant_result(x->ts.type, x->ts.kind);
  result->where = x->where;

  switch (x->ts.type) {
  case BT_INTEGER: 
    if (mpz_cmp(x->value.integer, y->value.integer) > 0) {
      mpz_sub(result->value.integer, x->value.integer, y->value.integer);
    }
    else {
      mpz_set(result->value.integer, mpz_zero);
    }

    break;

  case BT_REAL: 
    if (mpf_cmp(x->value.real, y->value.real) > 0)
      mpf_sub(result->value.real, x->value.real, y->value.real);
    else
      mpf_set(result->value.real,mpf_zero);

    break;

  default:
    g95_internal_error("g95_simplify_dim(): Bad type");
  }

  return range_check(result, "DIM");
}


g95_expr *g95_simplify_dprod(g95_expr *x, g95_expr *y) {
g95_expr *mult1, *mult2, *result;
int kx, ky;

  if (x->expr_type != EXPR_CONSTANT || y->expr_type != EXPR_CONSTANT) return NULL;

  if ( y == NULL ) {
    g95_error("Second argument of DPROD missing at %L", &x->where);
    return &g95_bad_expr;
  }

  kx = g95_validate_kind(x->ts.type, x->ts.kind);
  ky = g95_validate_kind(y->ts.type, y->ts.kind);
  if ( kx != g95_default_real_kind() || ky != g95_default_real_kind() ) {
    g95_error("Arguments of DPROD at %L must be of default real kind",
		    x->where);
  }

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

  return result;
}


/* simplify_exp */
g95_expr *g95_simplify_exp(g95_expr *e) {

  return NULL; 

/* We haven't implemented the extension of evaluation of transcendentals yet 
  //  if (e->expr_type != EXPR_CONSTANT) return NULL;
*/
}



g95_expr *g95_simplify_exponent(g95_expr *e) {

  return NULL; 

//  if (e->expr_type != EXPR_CONSTANT) return NULL;

/* Not done yet 

  if (arg->ts.type != BT_REAL) {
    g95_warning("Argument of EXPONENT at %L must be real", &e->where);
    return &g95_bad_expr;
  }
*/

}


/* simplify_floor */
g95_expr *g95_simplify_floor(g95_expr *e, g95_expr *k) {
g95_expr *floor, *result;
int kind;

  kind = get_kind(k, "FLOOR", g95_default_real_kind());
  if (kind == -1) return &g95_bad_expr;

  if (e->ts.type != EXPR_CONSTANT) return NULL;

  result = g95_constant_result(BT_INTEGER, kind);
  result->where = e->where;
  
  floor = g95_copy_expr(e);
  mpf_floor(result->value.real, e->value.real);
  result=g95_real2int(floor, kind);

  g95_free_expr(floor);

  return range_check(result, "FLOOR");

}



g95_expr *g95_simplify_fraction(g95_expr *e) {

  return NULL; 

  //  if (arg->expr_type != EXPR_CONSTANT) return NULL;

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
int i, index;
char c;

  if (e->expr_type != EXPR_CONSTANT) return NULL;

  if (e->value.character.length != 1 ) {
    g95_error("Argument of IACHAR at %L must be of length one", &e->where);
    return &g95_bad_expr;
  }

/* Table lookup -- can (should) be replaced with something smarter */

  c = e->value.character.string[0];

  if ( iscntrl(c) ) {
    if ( c == '\?' ) index = 127;
    else {
      for ( i=0; i<=31; ++i ) {
	if ( c == ascii_table[i] ) index = i;
	else {
	  g95_error("Argument of IACHAR at %L cannot be represented by this processor or is not implemented", e->where);
	  return &g95_bad_expr;
	}
      }
    }
  }
  else if ( isspace(c) ) index = 32;
  else if ( ispunct(c) ) {
    for ( i=33; i<=47; ++i ) { 
      if ( c == ascii_table[i] ) index = i;
    }
    for ( i=58; i<=64; ++i ) { 
      if ( c == ascii_table[i] ) index = i;
    }
    for ( i=91; i<=96; ++i ) { 
      if ( c == ascii_table[i] ) index = i;
    }
    for ( i=123; i<=126; ++i ) { 
      if ( c == ascii_table[i] ) index = i;
    }
  }
  else if ( isdigit(c) ) {
    for ( i=48; i<=57; ++i ) {
      if ( c == ascii_table[i] ) index = i;
    }
  }
  else if ( isupper(c) ) {
    for ( i=65; i<=90; ++i ) {
      if ( c == ascii_table[i] ) index = i;
    }
  }
  else if ( islower(c) ) {
    for ( i=97; i<=122; ++i ) {
      if ( c == ascii_table[i] ) index = i;
    }
  }
  else {
    g95_error("Argument of IACHAR at %L cannot be represented by this processor", e->where);
    return &g95_bad_expr;
  }
/* End of lookup */


  if ( index < CHAR_MIN || index > CHAR_MAX ) {
    g95_error("Argument of IACHAR at %L out of range of this processor", &e->where);
    return &g95_bad_expr;
  }

  result = g95_constant_result(BT_REAL, e->ts.kind);
  result->where = e->where;

  mpz_set_si(result->value.integer, index);
  return result;
}


g95_expr *g95_simplify_iand(g95_expr *x, g95_expr *y) {
g95_expr *result;

  if (x->ts.kind != y->ts.kind) {
    g95_error("KIND of arguments of IAND at %L must agree", &x->where);
    return &g95_bad_expr;
  }

  if (x->expr_type != EXPR_CONSTANT || y->expr_type != EXPR_CONSTANT)  return NULL;

  result = g95_constant_result(BT_REAL, x->ts.kind);
  result->where = x->where;

  mpz_and(result->value.integer, x->value.integer, y->value.integer);

  return range_check(result,"IAND");

}


g95_expr *g95_simplify_ibclr(g95_expr *x, g95_expr *y) {
g95_expr *result;
int k, pos;

  if (x->expr_type != EXPR_CONSTANT || y->expr_type != EXPR_CONSTANT)  return NULL;

  result = g95_copy_expr(x);
  result->where = x->where;

  if (g95_extract_int(y, &pos) != NULL || pos < 0 ) {
    g95_error("Invalid second argument of IBCLR at %L", &y->where);
    return &g95_bad_expr;
  }

  k = g95_validate_kind(BT_INTEGER, x->ts.kind);

  if ( pos > g95_integer_kinds[k].bit_size ) {
    g95_error("Second argument of IBCLR exceeds bit size at %L", &y->where);
    return &g95_bad_expr;
  }

  mpz_clrbit(result->value.integer,pos);
  return range_check(result,"IBCLR");
}


g95_expr *g95_simplify_ibits(g95_expr *x, g95_expr *y, g95_expr *z) {
g95_expr *result;
int pos, len;
int i, k, bitsize;
int *bits;

   if (x->expr_type != EXPR_CONSTANT || y->expr_type != EXPR_CONSTANT ||  
		   z->expr_type != EXPR_CONSTANT ) return NULL;

  result = g95_constant_result(BT_INTEGER, x->ts.kind);
  result->where = x->where;

  if (g95_extract_int(y, &pos) != NULL || pos < 0 ) {
    g95_error("Invalid second argument of IBITS at %L", &y->where);
    return &g95_bad_expr;
  }

  if (g95_extract_int(z, &len) != NULL || len < 0 ) {
    g95_error("Invalid third argument of IBITS at %L", &z->where);
    return &g95_bad_expr;
  }

  k = g95_validate_kind(BT_INTEGER, x->ts.kind);

  bitsize = g95_integer_kinds[k].bit_size;

  if ( pos+len > bitsize ) {
    g95_error("Sum of second and third arguments of IBITS exceeds bit size at %L", 
		    &y->where);
    return &g95_bad_expr;
  }

  bits = g95_getmem(bitsize);

  for ( i=0; i<bitsize; ++i ) {
    bits[i] = 0;
  }

  for ( i=0; i<len; ++i ) {
    bits[i] = mpz_tstbit(x->value.integer,i+pos);
  }

  for ( i=0; i<bitsize; ++i ) {
    if ( bits[i] == 0 ) {
      mpz_clrbit(result->value.integer,i);
    }
    else if ( bits[i] == 1) {
      mpz_setbit(result->value.integer,i);
    }
    else {
      g95_internal_error("IBITS: Bad bit");
    }
  }

  g95_free(bits);

  return range_check(result,"IBITS");

}


g95_expr *g95_simplify_ibset(g95_expr *x, g95_expr *y) {
g95_expr *result;
int k, pos;

  if (x->expr_type != EXPR_CONSTANT || y->expr_type != EXPR_CONSTANT)  return NULL;

  result = g95_copy_expr(x);

  if (g95_extract_int(y, &pos) != NULL || pos < 0 ) {
    g95_error("Invalid second argument of IBSET at %L", &y->where);
    return &g95_bad_expr;
  }

  k = g95_validate_kind(BT_INTEGER, x->ts.kind);

  if ( pos > g95_integer_kinds[k].bit_size ) {
    g95_error("Second argument of IBSET exceeds bit size at %L", &y->where);
    return &g95_bad_expr;
  }

  mpz_setbit(result->value.integer,pos);
  return range_check(result,"IBSET");
}


g95_expr *g95_simplify_ichar(g95_expr *e) {
g95_expr *result;
int index;

  if (e->expr_type != EXPR_CONSTANT) return NULL;

  if (e->value.character.length != 1) {
    g95_error("Argument of ICHAR at %L must be of length one", &e->where);
    return &g95_bad_expr;
  }

  index = (unsigned) e->value.character.string[0];

  if (index < CHAR_MIN || index > CHAR_MAX) {
    g95_error("Argument of ICHAR at %L out of range of this processor",
	      &e->where);
    return &g95_bad_expr;
  }

  result = g95_int_expr(index);
  result->where = e->where;
  return result;
}


g95_expr *g95_simplify_ieor(g95_expr *x, g95_expr *y) {
g95_expr *result;

  if (x->ts.kind != y->ts.kind) {
    g95_error("KIND of arguments of IEOR at %L must agree", &x->where);
    return &g95_bad_expr;
  }

  if (x->expr_type != EXPR_CONSTANT || y->expr_type != EXPR_CONSTANT)  return NULL;

  result = g95_constant_result(BT_INTEGER, x->ts.kind);
  result->where = x->where;

  mpz_xor(result->value.integer, x->value.integer, y->value.integer);

  return range_check(result,"IEOR");
}


g95_expr *g95_simplify_index(g95_expr *x, g95_expr *y, g95_expr *b) {
g95_expr *result;
int back, len, lensub;
int i, j, k, count, index, start;
  
  if (x->ts.kind != y->ts.kind) {
    g95_error("KIND of arguments of INDEX at %L must agree", &x->where);
    return &g95_bad_expr;
  }

  if (x->expr_type != EXPR_CONSTANT || y->expr_type != EXPR_CONSTANT)  
    return NULL;

  if ( b != NULL && b->ts.type != BT_LOGICAL) {
    g95_error("Optional argument of INDEX at %L must be logical", &b->where);
    return &g95_bad_expr;
  }

  if ( b != NULL && b->value.logical != 0  ) back = 1;
  else back = 0;

  result = g95_constant_result(BT_INTEGER, g95_default_integer_kind());
  result->where = x->where;

  len    = x->value.character.length;
  lensub = y->value.character.length;

  if ( len < lensub ) {
    mpz_set_si(result->value.integer,0);
    return result;
  }

  if ( back == 0  ) {

    if (lensub == 0) {
      mpz_set_si(result->value.integer,1);
      return result;
    }

    else if (lensub == 1) {
      for ( i=0; i<len; ++i ) {
        for ( j=0; j<lensub; ++j ) {
    	  if ( y->value.character.string[j] == x->value.character.string[i] ) {
	    index = i+1;
	    goto done;
	  }
	}
      }
    }

    else {
      index = 0;
      for ( i=0; i<len; ++i ) {
        for ( j=0; j<lensub; ++j ) {
	  if (y->value.character.string[j] == x->value.character.string[i] ) {
	    start = i;
	    count = 0;
	    for (k=0; k<lensub; ++k) {
    	      if (y->value.character.string[k] ==
			      x->value.character.string[k+start] ) {
		  ++count;
	      }
	    }
	    if (count == lensub ) {
	      index = start+1;
	      goto done;
	    }
	  }
	}
      }
    }
  }

  else {

    if (lensub == 0) {
      mpz_set_si(result->value.integer,len+1);
      return result;
    }

    else if (lensub == 1) {
      for ( i=index; i<len; ++i ) {
        for ( j=0; j<lensub; ++j ) {
	  if (y->value.character.string[j]==x->value.character.string[len-i]) {
	    index = len-i+1;
	    goto done;
	  }
	}
      }
    }

    else {

      index = 0;
      for ( i=0; i<len; ++i ) {
        for ( j=0; j<lensub; ++j ) {
	  if (y->value.character.string[j]==x->value.character.string[len-i]) {
	    start = len-i;
	    if ( start <= len-lensub ) {
	      count = 0;
	      for (k=0; k<lensub; ++k) {
    	        if (y->value.character.string[k] ==
			      x->value.character.string[k+start] ) {
	  	  ++count;
	        }
	      }
	      if (count == lensub ) {
	        index = start+1;
	        goto done;
	      }
	    }
	    else {
	      continue;
	    }
	  }
	}
      }
    }
  }

done:
    mpz_set_si(result->value.integer,index);
    return result;

}


g95_expr *g95_simplify_int(g95_expr *e, g95_expr *k) {
g95_expr *rpart, *rtrunc, *result;
int kind;

  kind = get_kind(k, "INT", g95_default_real_kind()); 
  if (kind == -1) return &g95_bad_expr;

  if (e->expr_type != EXPR_CONSTANT) return NULL;

  switch ( e->ts.type ) {
          case BT_INTEGER:
  		result = g95_int2int(e, kind);
		return range_check(result,"INT");
          case BT_REAL:
                rtrunc = g95_copy_expr(e);
		mpf_trunc(rtrunc->value.real, e->value.real);
                result = g95_real2int(rtrunc, kind);
                g95_free_expr(rtrunc);
		return range_check(result,"INT");
          case BT_COMPLEX:
  		rpart = g95_complex2real(e, kind);
                rtrunc = g95_copy_expr(rpart);
		mpf_trunc(rtrunc->value.real, rpart->value.real);
                result = g95_real2int(rtrunc, kind);
                g95_free_expr(rtrunc);
		return range_check(result,"INT");
	  default:
	        g95_error("Argument of INT at %L is not a valid type",
			      &e->where);
		return &g95_bad_expr;
  }

}


g95_expr *g95_simplify_ior(g95_expr *x, g95_expr *y) {
g95_expr *result;

  if (x->ts.kind != y->ts.kind) {
    g95_error("KIND of arguments of IEOR at %L must agree", &x->where);
    return &g95_bad_expr;
  }

  if (x->expr_type != EXPR_CONSTANT || y->expr_type != EXPR_CONSTANT)  return NULL;

  if (x->ts.type != BT_INTEGER || x->ts.type != BT_INTEGER) {
    g95_error("Arguments of IEOR at %L must be integer", &x->where);
    return &g95_bad_expr;
  }

  result = g95_constant_result(BT_INTEGER, x->ts.kind);
  result->where = x->where;

  mpz_ior(result->value.integer, x->value.integer, y->value.integer);

  return range_check(result,"IOR");
}


g95_expr *g95_simplify_ishft(g95_expr *e, g95_expr *s) {
g95_expr *result;
int shift;
int isize, k;
long e_int, new_int;

  if (e->expr_type != EXPR_CONSTANT || s->expr_type != EXPR_CONSTANT)  
    return NULL;

  if (g95_extract_int(s, &shift) != NULL || shift < 0 ) {
    g95_error("Invalid second argument of ISHFT at %L", &s->where);
    return &g95_bad_expr;
  }

  k = g95_validate_kind(BT_INTEGER, e->ts.kind);

  isize = g95_integer_kinds[k].bit_size;

  if ( shift > isize ) {
    g95_error("Second argument of ISHFT exceeds bit size at %L", &s->where);
    return &g95_bad_expr;
  }

  result = g95_copy_expr(e);

  if ( shift == 0 ) {
    return result;
  }
  else {
    e_int = mpz_get_si(e->value.integer);
    if (e_int > INT_MAX || e_int < INT_MIN) {
      g95_internal_error("ISHFT: unable to extract integer");
      return &g95_bad_expr;
    }
    else if ( shift > 0 ) {
      new_int = e_int << shift;
      mpz_set_si(result->value.integer, new_int);
      return range_check(result,"ISHFT");
    }
    else
      new_int = e_int >> shift;
      mpz_set_si(result->value.integer, new_int);
      return range_check(result,"ISHFT");
  }

}


g95_expr *g95_simplify_ishftc(g95_expr *e, g95_expr *s, g95_expr *sz) {
g95_expr *result;
int shift, isize, delta, k;
int i;
int *bits;

  if (e->expr_type != EXPR_CONSTANT || s->expr_type != EXPR_CONSTANT)  
    return NULL;

  if (g95_extract_int(s, &shift) != NULL || shift < 0 ) {
    g95_error("Invalid second argument of ISHFTC at %L", &s->where);
    return &g95_bad_expr;
  }

  k = g95_validate_kind(BT_INTEGER, e->ts.kind);

  if (sz !=NULL ) {
    if (g95_extract_int(sz, &isize) != NULL || isize < 0 ) {
      g95_error("Invalid third argument of ISHFTC at %L", &sz->where);
      return &g95_bad_expr;
    }
  }
  else
    isize = g95_integer_kinds[k].bit_size;

  if ( shift > isize ) {
    g95_error("Second argument of ISHFTC exceeds bit size at %L", &s->where);
    return &g95_bad_expr;
  }

  result = g95_copy_expr(e);

  if ( shift == 0 ) {
    return result;
  }

  bits = g95_getmem(isize);

  if (bits == NULL) {
    g95_internal_error("ISHFTC: Unable to allocate memory");
  }

  for ( i=0; i<isize; ++i ) {
    bits[i] = mpz_tstbit(e->value.integer,i);
  }

  delta = isize-shift;

  if (shift == 0 ) {
    return result;
  }
  else if ( shift > 0 ) {
    for ( i=0; i<delta; ++i ) {
      if ( bits[i] == 0 ) mpz_clrbit(result->value.integer, i+shift);
      if ( bits[i] == 1)  mpz_setbit(result->value.integer, i+shift);
    }
    for ( i=delta; i<isize; ++i ) {
      if ( bits[i] == 0 ) mpz_clrbit(result->value.integer, i-delta);
      if ( bits[i] == 1 ) mpz_setbit(result->value.integer, i-delta);
    }
    return range_check(result,"ISHFTC");
  }
  else {
    for ( i=shift; i<isize; ++i ) {
      if ( bits[i] == 0 ) mpz_clrbit(result->value.integer, i-shift);
      if ( bits[i] == 1 ) mpz_setbit(result->value.integer, i-shift);
    }
    for ( i=0; i<shift; ++i) {
      if ( bits[i] == 0 ) mpz_clrbit(result->value.integer, i+delta);
      if ( bits[i] == 1 ) mpz_setbit(result->value.integer, i+delta);
    }
    return range_check(result,"ISHFTC");
  }

  g95_free(bits);

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

  mpz_set_si(result->value.integer,e->value.character.length);
  return result;
}


g95_expr *g95_simplify_len_trim(g95_expr *e) {
g95_expr *result;
int count, len, lentrim;
int i;

  if (e->expr_type != EXPR_CONSTANT) return NULL;

  result = g95_constant_result(BT_INTEGER, g95_default_integer_kind());
  result->where = e->where;

  len = e->value.character.length;

  for (count=0, i=1; i<=len; ++i) {
    if (e->value.character.string[len-i] == ' ') ++count;
    else break; 
  }

  lentrim = len-count;

  mpz_set_si(result->value.integer,lentrim);
  return result;

}

/* FIXME */
/* Caveat: I have not accounted for possibility of non-ASCII character sets
 * in lge, lgt, lle, llt */

g95_expr *g95_simplify_lge(g95_expr *a, g95_expr *b) {
int tv;

  if (a->expr_type != EXPR_CONSTANT || b->expr_type != EXPR_CONSTANT)
    return NULL;

  if (a->value.character.length == 0 && b->value.character.length == 0 ) {
    return g95_logical_expr( 1, &a->where);
  }

  if ( strcmp(a->value.character.string, b->value.character.string) >= 0 ) 
    tv = 1;
  else tv = 0;

  return g95_logical_expr( tv, &a->where);
}


g95_expr *g95_simplify_lgt(g95_expr *a, g95_expr *b) {
int tv;

  if (a->expr_type != EXPR_CONSTANT || b->expr_type != EXPR_CONSTANT)
    return NULL;

  if (a->value.character.length == 0 && b->value.character.length == 0 ) {
    return g95_logical_expr( 0, &a->where);
  }

  if ( strcmp(a->value.character.string, b->value.character.string) > 0 )
    tv = 1;
  else tv = 0;

  return g95_logical_expr( tv, &a->where);
}


g95_expr *g95_simplify_lle(g95_expr *a, g95_expr *b) {
int tv;

  if (a->expr_type != EXPR_CONSTANT || b->expr_type != EXPR_CONSTANT)
    return NULL;

  if (a->value.character.length == 0 && b->value.character.length == 0 ) {
    return g95_logical_expr( 1, &a->where);
  }

  if ( strcmp(a->value.character.string, b->value.character.string) <= 0 )
    tv = 1;
  else tv = 0;

  return g95_logical_expr( tv, &a->where);
}


g95_expr *g95_simplify_llt(g95_expr *a, g95_expr *b) {
int tv;

  if (a->expr_type != EXPR_CONSTANT || b->expr_type != EXPR_CONSTANT)
    return NULL;

  if (a->value.character.length == 0 && b->value.character.length == 0 ) {
    return g95_logical_expr( 0, &a->where);
  }

  if ( strcmp(a->value.character.string, b->value.character.string) < 0 )
    tv = 1;
  else tv = 0;

  return g95_logical_expr( tv, &a->where);
}


g95_expr *g95_simplify_log(g95_expr *e) {

  return NULL;

/*
  if (e->ts.type == BT_REAL ) {
    if ( g95_compare_expr(e, real_zero) <= 0 ) {
      g95_error("Argument of LOG at %L cannot be less than or equal to zero",
  		  &e->where);
      return &g95_bad_expr;
    }
  }

  if (e->ts.type == BT_COMPLEX ) {
    if ( (mpf_cmp(e->value.complex.r, mpf_zero) == 0) && 
         (mpf_cmp(e->value.complex.i, mpf_zero) == 0) ) {
      g95_error("Complex argument of LOG at %L cannot be zero",
  		  &e->where);
      return &g95_bad_expr;
    }
  }
*/

}


g95_expr *g95_simplify_log10(g95_expr *e) {

  return NULL;

/* Transcendentals not yet implemented as extension 
  if (e->expr_type != EXPR_CONSTANT) return NULL;

  if ( g95_compare_expr(e, real_zero) <= 0 ) {
    g95_error("Argument of LOG10 at %L cannot be less than or equal to zero",
    	        &e->where);
    return &g95_bad_expr;
  }
*/

}


g95_expr *g95_simplify_logical(g95_expr *e, g95_expr *k) {
g95_expr *result;
int kind;

  kind = get_kind(k, "LOGICAL", g95_default_logical_kind()); 
  if (kind == -1) return &g95_bad_expr;

  if (e->expr_type != EXPR_CONSTANT) return NULL;

  result = g95_constant_result(BT_LOGICAL, kind);
  result->where = e->where; 

  result->value.logical = e->value.logical;

  return result;

}

/* Max family */

g95_expr *g95_simplify_max(g95_actual_arglist *arg) {
g95_expr *x, *y, *result;
mpz_t max_val;
mpf_t rmax_val;
bt my_type;
int my_kind;

  x   = arg->expr;
  arg = arg->next;

  if ( x->expr_type != EXPR_CONSTANT ) { /* Skip nonconstant argument */
    x   = arg->expr;
    arg = arg->next;
  }

  if ( x->ts.type == BT_INTEGER ) {
    mpz_init_set(max_val, x->value.integer);
    my_type = BT_INTEGER;
    my_kind = x->ts.kind;
  }
  else if ( x->ts.type == BT_REAL ) {
    mpf_init_set(rmax_val, x->value.real);
    my_type = BT_REAL;
    my_kind = x->ts.kind;
  }
  else {
    g95_internal_error("g95_simplify_max: bad type");
    return &g95_bad_expr;
  }

  while ( arg != NULL ) {
    y=arg->expr;
    if ( y->expr_type != EXPR_CONSTANT ) {
      x=y;
      arg=arg->next;
    }
    else {
      if ( my_type == BT_INTEGER ) {
        if ( mpz_cmp(y->value.integer, max_val) > 0 ) 
       	  mpz_set(max_val, y->value.integer);
      }
      if ( my_type == BT_REAL ) {
        if ( mpf_cmp(x->value.real, rmax_val) > 0 )
 	  mpf_set(rmax_val, y->value.real);
      }
      x=y;
      arg=arg->next;
    }
  }

  if ( my_type == BT_INTEGER ) {
    result=g95_constant_result(BT_INTEGER,my_kind);
    mpz_set(result->value.integer, max_val);
    mpz_clear(max_val);
  }
  else if ( my_type == BT_REAL ) {
    result=g95_constant_result(BT_REAL,my_kind);
    mpf_set(result->value.real, rmax_val);
    mpf_clear(rmax_val);
  }

  return result;

}


g95_expr *g95_simplify_amax0(g95_actual_arglist *arg) {
g95_expr *x, *y, *r, *result;
mpz_t max_val;
int i, kind;

  x   = arg->expr;
  arg = arg->next;

  kind = x->ts.kind;
  i = g95_validate_kind(BT_INTEGER, kind);
  if (i < 0) g95_internal_error("g95_simplify_amax0(): Bad kind");

  if ( x->expr_type != EXPR_CONSTANT ) { /* Skip nonconstant argument */
    x   = arg->expr;
    arg = arg->next;
  }

  mpz_init_set(max_val, x->value.integer);

  while ( arg != NULL ) {
    y=arg->expr;
    if ( y->expr_type != EXPR_CONSTANT ) {
      x=y;
      arg=arg->next;
    }
    else {
      if ( mpz_cmp(y->value.integer, max_val) > 0 ) {
        mpz_set(max_val, y->value.integer);
      }
      x=y;
      arg=arg->next;
    }
  }

  r = g95_constant_result(BT_INTEGER,kind);
  mpz_set(r->value.integer, max_val);
  mpz_clear(max_val);

  result = g95_int2real(r, kind);

  g95_free_expr(r);

  return result;

}


g95_expr *g95_simplify_max1(g95_actual_arglist *arg) {
g95_expr *x, *y, *r, *result;
mpf_t max_val;
int i, kind;

  x   = arg->expr;
  arg = arg->next;

  kind = x->ts.kind;
  i    = g95_validate_kind(BT_REAL, kind);
  if (i < 0) g95_internal_error("g95_simplify_max1(): Bad kind");

  if ( x->expr_type != EXPR_CONSTANT ) { /* Skip nonconstant argument */
    x   = arg->expr;
    arg = arg->next;
  }

  mpf_init_set(max_val, x->value.real);

  while ( arg != NULL ) {
    y=arg->expr;
    if ( y->expr_type != EXPR_CONSTANT ) {
      x=y;
      arg=arg->next;
    }
    else {
      if ( mpf_cmp(y->value.real, max_val) > 0 ) {
        mpf_set(max_val, y->value.real);
      }
      x=y;
      arg=arg->next;
    }
  }

  r = g95_constant_result(BT_REAL,kind);
  mpf_set(r->value.real, max_val);
  mpf_clear(max_val);

  result = g95_real2int(r, kind);

  g95_free_expr(r);

  return result;

}

/* End of max functions */


g95_expr *g95_simplify_maxexponent(g95_expr *x) {
g95_expr *result;
int i;

  i = g95_validate_kind(BT_REAL, x->ts.kind);
  if (i < 0) g95_internal_error("g95_simplify_maxexponent(): Bad kind");

  result = g95_int_expr(g95_real_kinds[i].max_exponent);
  result->where = x->where;

  return result;
}


/* Min family */

g95_expr *g95_simplify_min(g95_actual_arglist *arg) {
g95_expr *x, *y, *result;
mpz_t min_val;
mpf_t rmin_val;
bt my_type;
int my_kind;

  x   = arg->expr;
  arg = arg->next;

  if ( x->expr_type != EXPR_CONSTANT ) { /* Skip nonconstant argument */
    x   = arg->expr;
    arg = arg->next;
  }

  if ( x->ts.type == BT_INTEGER ) {
    mpz_init_set(min_val, x->value.integer);
    my_type = BT_INTEGER;
    my_kind = x->ts.kind;
  }
  else if ( x->ts.type == BT_REAL ) {
    mpf_init_set(rmin_val, x->value.real);
    my_type = BT_REAL;
    my_kind = x->ts.kind;
  }
  else {
    g95_internal_error("g95_simplify_min: bad type");
    return &g95_bad_expr;
  }

  while ( arg != NULL ) {
    y=arg->expr;
    if ( y->expr_type != EXPR_CONSTANT ) {
      x=y;
      arg=arg->next;
    }
    else {
      if ( my_type == BT_INTEGER ) {
        if ( mpz_cmp(y->value.integer, min_val) < 0 ) 
       	  mpz_set(min_val, y->value.integer);
      }
      if ( my_type == BT_REAL ) {
        if ( mpf_cmp(x->value.real, rmin_val) < 0 )
 	  mpf_set(rmin_val, y->value.real);
      }
      x=y;
      arg=arg->next;
    }
  }

  if ( my_type == BT_INTEGER ) {
    result=g95_constant_result(BT_INTEGER,my_kind);
    mpz_set(result->value.integer, min_val);
    mpz_clear(min_val);
  }
  else if ( my_type == BT_REAL ) {
    result=g95_constant_result(BT_REAL,my_kind);
    mpf_set(result->value.real, rmin_val);
    mpf_clear(rmin_val);
  }

  return result;

}


g95_expr *g95_simplify_amin0(g95_actual_arglist *arg) {
g95_expr *x, *y, *r, *result;
mpz_t min_val;
int i, kind;

  x   = arg->expr;
  arg = arg->next;
  
  kind = x->ts.kind;
  i = g95_validate_kind(BT_INTEGER, x->ts.kind);
  if (i < 0) g95_internal_error("g95_simplify_amin0(): Bad kind");

  if ( x->expr_type != EXPR_CONSTANT ) { /* Skip nonconstant argument */
    x   = arg->expr;
    arg = arg->next;
  }

  mpz_init_set(min_val, x->value.integer);

  while ( arg != NULL ) {
    y=arg->expr;
    if ( y->expr_type != EXPR_CONSTANT ) {
      x=y;
      arg=arg->next;
    }
    else {
      if ( mpz_cmp(y->value.integer, min_val) < 0 ) {
        mpz_set(min_val, y->value.integer);
      }
      x=y;
      arg=arg->next;
    }
  }

  r = g95_constant_result(BT_INTEGER,kind);
  mpz_set(r->value.integer, min_val);
  mpz_clear(min_val);

  result = g95_int2real(r, kind);

  g95_free_expr(r);

  return result;

}


g95_expr *g95_simplify_min1(g95_actual_arglist *arg) {
g95_expr *x, *y, *r, *result;
mpf_t min_val;
int i, kind;

  x   = arg->expr;
  arg = arg->next;

  kind = x->ts.kind;
  i = g95_validate_kind(BT_REAL, x->ts.kind);
  if (i < 0) g95_internal_error("g95_simplify_min1(): Bad kind");

  if ( x->expr_type != EXPR_CONSTANT ) { /* Skip nonconstant argument */
    x   = arg->expr;
    arg = arg->next;
  }

  mpf_init_set(min_val, x->value.real);

  while ( arg != NULL ) {
    y=arg->expr;
    if ( y->expr_type != EXPR_CONSTANT ) {
      x=y;
      arg=arg->next;
    }
    else {
      if ( mpf_cmp(y->value.real, min_val) < 0 ) {
        mpf_set(min_val, y->value.real);
      }
      x=y;
      arg=arg->next;
    }
  }

  r = g95_constant_result(BT_REAL,kind);
  mpf_set(r->value.real, min_val);
  mpf_clear(min_val);

  result = g95_real2int(r, kind);

  g95_free_expr(r);

  return result;

}

/* End of min functions */



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

  if (a->expr_type != EXPR_CONSTANT && p->expr_type != EXPR_CONSTANT) 
    return NULL;

  switch (a->ts.type) {
    	case BT_INTEGER:
	    	if (p->ts.type != BT_INTEGER ) {
	     	        g95_error("Type of arguments of MOD at %L must agree",
  				&a->where);
			return &g95_bad_expr;
    		}
    		else {
	      	  if (g95_compare_expr(p, integer_zero) != 0) {
			result =g95_constant_result(BT_INTEGER,a->ts.kind);
			result->where = a->where;
       		 	mpz_tdiv_r(result->value.integer, a->value.integer, 
                                                       p->value.integer);
			return range_check(result, "MOD");
      		  }
      		  else {
		  /* Result is processor-dependent */
       		 	g95_error("Second argument MOD at %L is zero",
  		       		  &a->where);
			return &g95_bad_expr;
      		  }
		}
    	case BT_REAL:
	    	if (p->ts.type != BT_REAL ) {
		      g95_error("Type of arguments of MOD at %L must agree",
  				&a->where);
		      return &g95_bad_expr;
    		}
	    	else {
      		  if (g95_compare_expr(p, real_zero) != 0) {
			result =g95_constant_result(BT_REAL,a->ts.kind);
			result->where = a->where;
			mpf_init(quot);
			mpf_init(iquot);
			mpf_init(term);
			mpf_div(quot, a->value.real,p->value.real);
			mpf_trunc(iquot, quot);
			mpf_mul(term, iquot, p->value.real);
			mpf_sub(result->value.real, a->value.real, term);
			mpf_clear(quot);
			mpf_clear(iquot);
			mpf_clear(term);
			return range_check(result, "MOD");
      		  }
      		  else {
		  /* Result is processor-dependent */
       		 	g95_error("Second argument of MOD at %L is zero",
  	       			  &p->where);
			return &g95_bad_expr;
      		  }
		}
    		break;
	    default:
       		g95_warning("Type of arguments of MOD at %L must be real or integer",
	 	       	&a->where);
		return &g95_bad_expr;
  }

}


g95_expr *g95_simplify_modulo(g95_expr *a, g95_expr *p) {
g95_expr *result;
mpf_t quot, iquot, term;

  if (a->expr_type != EXPR_CONSTANT && p->expr_type != EXPR_CONSTANT) 
    return NULL;

  switch (a->ts.type) {
    	case BT_INTEGER:
	    	if (p->ts.type != BT_INTEGER ) {
	     	        g95_error("Type of arguments of MODULO at %L must agree",
  				&a->where);
			return &g95_bad_expr;
    		}
    		else {
	      	  if (g95_compare_expr(p, integer_zero) != 0) {
			result =g95_constant_result(BT_INTEGER,a->ts.kind);
			result->where = a->where;
			mpz_fdiv_r(result->value.integer, a->value.integer, 
                                                       p->value.integer);
			return range_check(result, "MODULO");
      		  }
      		  else {
		  /* Result is processor-dependent */
       		 	g95_error("Second argument MODULO at %L is zero",
  		       		  &a->where);
			return &g95_bad_expr;
      		  }
		}
    	case BT_REAL:
	    	if (p->ts.type != BT_REAL ) {
		      g95_error("Type of arguments of MODULO at %L must agree",
  				&a->where);
		      return &g95_bad_expr;
    		}
	    	else {
      		  if (g95_compare_expr(p, real_zero) != 0) {
			result =g95_constant_result(BT_REAL,a->ts.kind);
			result->where = a->where;
			mpf_init(quot);
			mpf_init(iquot);
			mpf_init(term);
			mpf_div(quot, a->value.real,p->value.real);
			mpf_floor(iquot, quot);
			mpf_mul(term, iquot, p->value.real);
			mpf_clear(quot);
			mpf_clear(iquot);
			mpf_clear(term);
			mpf_sub(result->value.real, a->value.real, term);
			return range_check(result, "MODULO");
      		  }
      		  else {
		  /* Result is processor-dependent */
       		 	g95_error("Second argument of MODULO at %L is zero",
  	       			  &p->where);
			return &g95_bad_expr;
      		  }
		}
    		break;
	    default:
       		g95_warning("Type of arguments of MODULO at %L must be real or integer",
	 	       	&a->where);
		return &g95_bad_expr;
  }

}

/* simplify_mvbits() */
g95_expr *g95_simplify_mvbits(g95_expr *f, g95_expr *fp, g95_expr *l, g95_expr *to, g95_expr *tp) {
  return NULL;
}

g95_expr *g95_simplify_nearest(g95_expr *e) {

  return NULL; 

}


g95_expr *g95_simplify_nint(g95_expr *e, g95_expr *k) {
g95_expr *rtrunc, *result;
int kind;

  kind = get_kind(k, "NINT", e->ts.kind);
  if (kind == -1) return &g95_bad_expr;

  if (e->expr_type != EXPR_CONSTANT ) return NULL;

  rtrunc = g95_copy_expr(e);

  if ( mpf_cmp_si(e->value.real,0) > 0 ) {
    mpf_ceil(rtrunc->value.real,e->value.real);
    }
  else if ( mpf_cmp_si(e->value.real,0) < 0 ) {
    mpf_floor(rtrunc->value.real,e->value.real);
  }
  else {
    mpf_set_si(rtrunc->value.real,0);
  }

  result = g95_real2int(rtrunc, kind);

  if ( result == NULL ) {
    g95_error("Result of NINT() overflows its kind at %L", &e->where);
    g95_free_expr(rtrunc);
    return &g95_bad_expr;
  }
  else {
    g95_free_expr(rtrunc);
    return range_check(result,"NINT");
  }

}


g95_expr *g95_simplify_not(g95_expr *e) {
g95_expr *result;
int k, isize;
int i;

  k = g95_validate_kind(BT_INTEGER, e->ts.kind);
  if (k == -1) g95_internal_error("g95_simplify_not(): Bad kind");

  if (e->expr_type != EXPR_CONSTANT) return NULL;

  isize = g95_integer_kinds[k].bit_size;

  result = g95_copy_expr(e);

  for ( i=0; i<isize; ++i ) {
    if (mpz_tstbit(e->value.integer,i) == 0) {
      mpz_setbit(result->value.integer,i);
    }
    else if (mpz_tstbit(e->value.integer,i) == 1) {
      mpz_clrbit(result->value.integer,i);
    }
    else {
      g95_internal_error("g95_simplify_not(): Bad bit in argument");
    }
  }

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

  i = g95_validate_kind(e->ts.type, e->ts.kind);
  if (i < 0) goto bad_type;

  switch(e->ts.type) {
  case BT_INTEGER:
    i = g95_integer_kinds[i].range;
    break;

  case BT_REAL:
    i = g95_real_kinds[i].range;
    break;

  bad_type:
  default:
    g95_internal_error("g95_simplify_range(): Bad kind");
  }

  result = g95_int_expr(i);
  result->where = e->where;

  return result;
}



g95_expr *g95_simplify_real(g95_expr *e, g95_expr *k) {
g95_expr *result;
int kind;

  kind = get_kind(k, "REAL", g95_default_real_kind()); 
  if (kind == -1) return &g95_bad_expr;

  if (e->expr_type != EXPR_CONSTANT) return NULL;

  switch (e->ts.type) {
	case BT_INTEGER:
	    result = g95_int2real(e, kind);
            return range_check(result, "REAL");
	case BT_REAL:
	    result = g95_real2real(e, kind);
            return range_check(result, "REAL");
	case BT_COMPLEX:
	    result = g95_complex2real(e, kind);
            return range_check(result, "REAL");
	    break;

        default:
       	    g95_warning("Invalid argument type in REAL at %L", e->where);
	    return &g95_bad_expr;
  }

}

g95_expr *g95_simplify_repeat(g95_expr *e, g95_expr *n) {
g95_expr *result;
int i, j, len, ncopies, nlen;

  if (e->expr_type != EXPR_CONSTANT || n->expr_type != EXPR_CONSTANT)  
    return NULL;

  if (n !=NULL ) {
    if (g95_extract_int(n, &ncopies) != NULL || ncopies < 0 ) {
      g95_error("Invalid second argument of REPEAT at %L", &n->where);
      return &g95_bad_expr;
    }
  }

  len    = e->value.character.length;
  nlen   = ncopies*len;

  result = g95_constant_result(BT_CHARACTER,e->ts.kind);

  if (ncopies == 0) {
    result->value.character.string=g95_getmem(1);
    result->value.character.length=0;
    result->value.character.string='\0';
    return result;
  }
  else {
    result->value.character.length=nlen;
    result->value.character.string=g95_getmem(nlen+1);
    for (i=0; i<ncopies; ++i) {
      for (j=0; j<len; ++j) {
	result->value.character.string[j+i*len] = e->value.character.string[j];
      }
    }
    result->value.character.string[nlen] = '\0';  /* For debugger */
    return result;
  }

}


g95_expr *g95_simplify_rrspacing(g95_expr *e) {
g95_expr *arg;

  return NULL; 

/* Type checking */

  arg = FIRST_ARG(e);

  if (arg->ts.type != BT_REAL) {
    g95_warning("Argument of RRSPACING at %L must be real",
		&FIRST_ARG(e)->where);
    //    return FAILURE;
  }

  //  if (arg->expr_type != EXPR_CONSTANT) return FAILURE;

  //  return SUCCESS;

}


g95_expr *g95_simplify_scale(g95_expr *e) {

  return NULL; 

}


g95_expr *g95_simplify_scan(g95_expr *e, g95_expr *c, g95_expr *b) {
g95_expr *result;
int back;
int indx, len, lenc;

  if (e->expr_type != EXPR_CONSTANT || c->expr_type != EXPR_CONSTANT)  
    return NULL;

  if (e->ts.kind != c->ts.kind) {
    g95_error("KIND of arguments of SCAN at %L must agree", &e->where);
    return &g95_bad_expr;
  }

  if ( b != NULL && b->value.logical != 0  ) back = 1;
  else back = 0;

  result = g95_constant_result(BT_INTEGER, g95_default_integer_kind());
  result->where = e->where;

  len  = e->value.character.length;
  lenc = c->value.character.length;

  if ( len == 0 || lenc == 0 ) {
    indx = 0;
  }
  else {
    indx = strcspn(e->value.character.string,c->value.character.string) + 1;
    if ( indx > len ) indx=0;
    if ( back != 0 && indx != 0 ) indx = len-indx+1;
  }

  mpz_set_si(result->value.integer,indx);
  return result;

}


g95_expr *g95_simplify_selected_int_kind(g95_expr *e) {
int i, kind, range;

  if (e->expr_type != EXPR_CONSTANT ||
      g95_extract_int(e, &range) != NULL) return NULL;

  kind = INT_MAX;
  for(i=0; g95_integer_kinds[i].kind!=0; i++) {
    if (g95_integer_kinds[i].range >= range && 
	g95_integer_kinds[i].kind < kind) {
      kind = g95_integer_kinds[i].kind;
    }
  }

  if (kind == INT_MAX) {
    g95_error("Range %d exceeds all integer kinds at %L", range, &e->where);
    return &g95_bad_expr;
  }

  return g95_int_expr(kind);
}



g95_expr *g95_simplify_selected_real_kind(g95_expr *p, g95_expr *q) {
int range, precision, i, kind, found_precision, found_range;

  if (p == NULL && q == NULL)
    g95_internal_error("No arguments to SELECTED_REAL_KIND()");

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
    if (g95_real_kinds[i].precision >= precision)
      found_precision = 1;
    if (g95_real_kinds[i].range >= range) 
      found_range = 1;
    if (g95_real_kinds[i].precision >= precision &&
	g95_real_kinds[i].range >= range &&
	g95_real_kinds[i].kind < kind)
      kind = g95_real_kinds[i].kind;
  }
  
  if (kind == INT_MAX) {
    kind = 0;
    if (!found_precision) {
      g95_warning("Specified precision %d exceeds all REAL kinds at %L",
		  precision, &p->where);
      kind = -1;
    }

    if (!found_range) {
      g95_warning("Specified exponent range %d exceeds all REAL kinds at %L", 
		  range, &q->where);
      kind -= 2;
    }
  }

  return g95_int_expr(kind);
}



g95_expr *g95_simplify_set_exponent(g95_expr *e) {

  return NULL; 

}


g95_expr *g95_simplify_sign(g95_expr *x, g95_expr *y) {
g95_expr *absv, *result;
mpz_t sgnz;
mpf_t sgnf;
int sgn;

  if (x->expr_type != EXPR_CONSTANT || y->expr_type != EXPR_CONSTANT)  
    return NULL;

  if ( (x->ts.type != y->ts.type) ) {
    g95_error("Type of arguments of SIGN at %L must agree", &x->where);
    return &g95_bad_expr;
  }

  if ( (x->ts.kind != y->ts.kind) ) {
    g95_error("Kind of arguments of SIGN at %L must agree", &x->where);
    return &g95_bad_expr;
  }

  absv   = g95_copy_expr(x);
  result = g95_copy_expr(x);

  switch (x->ts.type) {
	case BT_INTEGER:
        	sgn = mpz_sgn(y->value.integer);
		mpz_init_set_si(sgnz,sgn);
		mpz_abs(absv->value.integer,x->value.integer);
		mpz_mul(result->value.integer, absv->value.integer, sgnz);
		mpz_clear(sgnz);
		g95_free_expr(absv);
		return result;
	case BT_REAL:
        	sgn = mpf_sgn(y->value.real);
		mpf_abs(absv->value.real,x->value.real);
		mpf_init_set_si(sgnf,sgn);
		mpf_mul(result->value.real, absv->value.real, sgnf);
		mpf_clear(sgnf);
		g95_free_expr(absv);
		return result;
	  default:
	    ;;
	    g95_internal_error("Bad type in g95_simplify_sign");
	    return &g95_bad_expr;
  }
}


g95_expr *g95_simplify_sin(g95_expr *e) {
/* We don't have an extension to return transcendental functions for constant
 * arguments yet */

  return NULL;
}


g95_expr *g95_simplify_sinh(g95_expr *e) {
  /* Ditto */

  return NULL;
}


/* simplify_spacing */
g95_expr *g95_simplify_spacing(g95_expr *e) {

  return NULL; 

}



g95_expr *g95_simplify_sqrt(g95_expr *e) {
g95_expr *sroot, *result;

/* Evaluation of sqrt for constant arguments is an extension */
/* Will do complex case later */

  switch (e->ts.type) {
  	case BT_INTEGER:
	  if (mpz_cmp_si(e->value.integer,0) < 0) {
	    g95_error("Argument of SQRT at %L has a negative value", &e->where);
	    return &g95_bad_expr;
	  }
	  else {
	    sroot = g95_copy_expr(e);
            mpz_sqrt(sroot->value.integer, e->value.integer);
	    result = g95_int2real(sroot, g95_default_real_kind());
	    g95_free_expr(sroot);
	    return result;
	  }
  	case BT_REAL:
	  if (mpf_cmp_si(e->value.real,0) < 0) {
	    g95_error("Argument of SQRT at %L has a negative value", &e->where);
	    return &g95_bad_expr;
	  }
	  else {
	    result = g95_copy_expr(e);
            mpf_sqrt(result->value.real, e->value.real);
	    return result;
	  }
	case BT_COMPLEX:
	  // Need to compute principle value
	  g95_warning("Complex constant sqrt not implemented yet %L",&e->where);
	  return NULL;
	  break;

        default:
	    g95_warning("Invalid argument of SQRT at %L", &e->where);
	    return &g95_bad_expr;
  }
}


g95_expr *g95_simplify_tan(g95_expr *e) {
/* We don't have an extension to return transcendental functions for constant
 * arguments yet */

  return NULL;
}


g95_expr *g95_simplify_tanh(g95_expr *e) {
  /* Ditto */

  return NULL;
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

  if (e->ts.type != BT_CHARACTER) {
    g95_error("Argument of TRIM at %L must be character", &e->where);
    return &g95_bad_expr;
  }

  len = e->value.character.length;

  result = g95_constant_result(BT_CHARACTER, e->ts.kind);
  result->where = e->where;

  for (count=0, i=1; i<=len; ++i) {
    if (e->value.character.string[len-i] == ' ') ++count;
    else break; 
  }

  lentrim = len-count;

  result->value.character.length = lentrim;
  result->value.character.string = g95_getmem(lentrim+1);

  for ( i=0; i<lentrim; ++i) {
    result->value.character.string[i] = e->value.character.string[i];
  }

  result->value.character.string[lentrim] = '\0';   /* For debugger */

  return result;
}


g95_expr *g95_simplify_verify(g95_expr *s, g95_expr *set, g95_expr *b) {
g95_expr *result;
int back, len, lenset;
int index;
  
  if (s->ts.kind != set->ts.kind) {
    g95_error("KIND of arguments of VERIFY at %L must agree", &s->where);
    return &g95_bad_expr;
  }

  if (s->expr_type != EXPR_CONSTANT || set->expr_type != EXPR_CONSTANT)  
    return NULL;

  if ( b != NULL && b->ts.type != BT_LOGICAL) {
    g95_error("Optional argument of INDEX at %L must be logical", &b->where);
    return &g95_bad_expr;
  }

  if ( b != NULL && b->value.logical != 0  ) back = 1;
  else back = 0;

  result = g95_constant_result(BT_INTEGER, g95_default_integer_kind());
  result->where = s->where;

  len    = s->value.character.length;
  lenset = set->value.character.length;

  if ( len == 0 ) {
    mpz_set_si(result->value.integer,0);
    return result;
  }

  if ( back == 0 ) {
    if (lenset == 0) {
      mpz_set_si(result->value.integer,len);
      return result;
    }
    else {
      index = strspn(s->value.character.string,set->value.character.string)+1;
      if ( index > len ) index = 0;
    }
  }
  else {
    if ( lenset == 0 ) {
       mpz_set_si(result->value.integer,1);
    }
    else {
      index = len-strspn(s->value.character.string,set->value.character.string);
    }
  }

  mpz_set_si(result->value.integer,index);
  return result;

}



/* init_pi()-- Calculate pi.  We use the Bailey, Borwein and Plouffe formula:
 *
 * pi = \sum{n=0}^\infty (1/16)^n [4/(8n+1) - 2/(8n+4) - 1/(8n+5) - 1/(8n+6)]
 * 
 * which gives about four bits per iteration.
 */


static void init_pi(void) {
mpf_t s, t;
int n, limit;

  mpf_init(s);
  mpf_init(t);

  mpf_init(mpf_pi);
  mpf_set_ui(mpf_pi, 0);

  limit = (G95_REAL_BITS / 4) + 10;  /* (1/16)^n gives 4 bits per iteration */

  for(n=0; n<limit; n++) {
    mpf_set_ui(t, 4);
    mpf_div_ui(t, t, 8*n+1);  /* 4/(8n+1) */

    mpf_set_ui(s, 2);
    mpf_div_ui(s, s, 8*n+4);  /* 2/(8n+4) */
    mpf_sub(t, t, s);

    mpf_set_ui(s, 1);
    mpf_div_ui(s, s, 8*n+5);  /* 1/(8n+5) */
    mpf_sub(t, t, s);

    mpf_set_ui(s, 1);
    mpf_div_ui(s, s, 8*n+6);  /* 1/(8n+6) */
    mpf_sub(t, t, s);

    mpf_set_ui(s, 16);
    mpf_pow_ui(s, s, n);      /* 16^n */

    mpf_div(t, t, s);

    mpf_add(mpf_pi, mpf_pi, t);
  }

  mpf_clear(s);
  mpf_clear(t);

/* Compute multiples of pi */

  mpf_init(mpf_hpi);
  mpf_init(mpf_nhpi);

  mpf_div_ui(mpf_hpi, mpf_pi, 2);
  mpf_neg(mpf_nhpi, mpf_hpi);
}


void g95_simplify_init_1(void) {

  init_pi();

  integer_zero = g95_convert_integer("0", g95_default_integer_kind(), 10);
  real_zero = g95_convert_real("0.0", g95_default_real_kind());

  mpf_init_set_str(mpf_zero, "0.0", 10);
  mpf_init_set_str(mpf_half, "0.5", 10);
  mpf_init_set_str(mpf_one,  "1.0", 10);
  mpz_init_set_str(mpz_zero,   "0", 10);
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
