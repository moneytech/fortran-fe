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


extern g95_integer_info g95_integer_kinds[];
extern g95_real_info g95_real_kinds[];

#define FIRST_ARG(e) (e->value.function.actual->expr)
#define SECOND_ARG(e) (e->value.function.actual->next->expr)
#define THIRD_ARG(e) (e->value.function.actual->next->next->expr)

static g95_expr *integer_zero, *real_zero;
static mpf_t mpf_zero, mpf_half, mpf_one, mpf_pi, mpf_hpi, mpf_nhpi;

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


g95_expr *g95_simplify_iabs(g95_expr *e) {
g95_expr *result;

  if (e->expr_type != EXPR_CONSTANT) return NULL;

  result = g95_copy_expr(e);

  mpz_abs(result->value.integer, e->value.integer);

  if (g95_check_integer_range(result->value.integer, result->ts.kind)
      != ARITH_OK) {
    g95_error("Result of ABS() overflows it's kind at %L", &e->where);

    g95_free_expr(result);
    return &g95_bad_expr;
  }

  return result;
}


/* real_range_check()-- Range checks a real expression node.  If all
 * goes well, returns the node, otherwise returns &g95_bad_expr and
 * frees the node.  */

static g95_expr *real_range_check(g95_expr *result, char *name) {

  if (g95_check_real_range(result->value.real, result->ts.kind)	== ARITH_OK)
    return result;

  g95_error("Result of %s overflows it's kind at %L", name, &result->where);

  g95_free_expr(result);
  return &g95_bad_expr;
}


g95_expr *g95_simplify_rabs(g95_expr *e) {
g95_expr *result;

  if (e->expr_type != EXPR_CONSTANT) return NULL;

  result = g95_copy_expr(e);

  mpf_abs(result->value.real, e->value.real);

  return real_range_check(result, "ABS");
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
    
  mpf_sub(a, a, b);
  mpf_sqrt(result->value.real, a);

  mpf_clear(a);
  mpf_clear(b);

  return real_range_check(result, "CABS");
}


g95_expr *g95_simplify_achar(g95_expr *e) {

  if (e->expr_type != EXPR_CONSTANT) return NULL;

  /* Skeleton: Put meat here */

  return NULL;
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

  if (e->expr_type != EXPR_CONSTANT) return NULL;

  /* Skeleton: Put meat here */

  return NULL;
}


g95_expr *g95_simplify_adjustr(g95_expr *e) {

  if (e->expr_type != EXPR_CONSTANT) return NULL;

  /* Skeleton: Put meat here */

  return NULL;
}


g95_expr *g95_simplify_aimag(g95_expr *e) {
g95_expr *result;

  if (e->expr_type != EXPR_CONSTANT) return NULL;

  result = g95_constant_result(BT_REAL, e->ts.kind);
  mpf_set(result->value.real, e->value.complex.i);

  return result;
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


g95_expr *g95_simplify_aint(g95_expr *e, g95_expr *k) {
g95_expr *result;
int kind;

/* Because the kind parameter has to be known at compile-time, we make
 * sure this is so before seeing if e is non-constant. */

  kind = get_kind(k, "AINT", e->ts.kind);
  if (kind == -1) return &g95_bad_expr;

  if (e->expr_type != EXPR_CONSTANT) return NULL;

  result = g95_constant_result(BT_REAL, kind);

  mpf_trunc(result->value.real, e->value.real);

  if (g95_check_real_range(result->value.real, kind) != ARITH_OK) {
    g95_error("Result of AINT() overflows its kind at %L", &e->where);
    g95_free_expr(result);

    return &g95_bad_expr;
  }

  return result;
}


g95_expr *g95_simplify_anint(g95_expr *e, g95_expr *k) {
g95_expr *result;
int kind;

  kind = get_kind(k, "ANINT", e->ts.kind);
  if (kind == -1) return &g95_bad_expr;

  if (e->expr_type != EXPR_CONSTANT) return NULL;

  result = g95_constant_result(BT_REAL, kind);

  if (mpf_sgn(e->value.real) < 0)
    mpf_sub(result->value.real, e->value.real, mpf_half);
  else
    mpf_add(result->value.real, e->value.real, mpf_half);

  mpf_trunc(result->value.real, result->value.real);

  if (g95_check_real_range(result->value.real, kind) != ARITH_OK) {
    g95_error("Result of ANINT() overflows its kind at %L", &e->where);
    g95_free_expr(result);

    return &g95_bad_expr;
  }

  return result;
}


g95_expr *g95_simplify_asin(g95_expr *e) {

  if (e->expr_type != EXPR_CONSTANT) return NULL; 

/* Range checking--  Must have abs(x)<=1 */
 
  if (mpf_cmp_si(e->value.real, 1) > 0 || mpf_cmp_si(e->value.real, -1) < 0) {
    g95_error("Argument of ASIN at %L must be between -1 and 1", &e->where);
    return &g95_bad_expr;
  }

  return NULL;
}


g95_expr *g95_simplify_atan2(g95_expr *x, g95_expr *y) {

  if (x->ts.kind != y->ts.kind) {
    g95_warning("KIND of arguments of ATAN2 at %L must agree", &x->where);
    return &g95_bad_expr;
  }

  return NULL;
}


g95_expr *g95_simplify_bit_size(g95_expr *e) {

  return g95_int_expr(32);  /* TODO: Replace this with config info */
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
g95_expr *result;
int kind;

  kind = get_kind(k, "CEILING", g95_default_real_kind());
  if (kind == -1) return &g95_bad_expr;

  if (e->ts.type != EXPR_CONSTANT) return NULL;

  result = g95_constant_result(BT_REAL, kind);
  result->where = e->where;
  
  mpf_ceil(result->value.real, e->value.real);

  return real_range_check(result, "CEILING");
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

  result->value.character.length = 1;
  result->value.character.string = g95_getmem(2);

  result->value.character.string[0] = c;
  result->value.character.string[1] = '\0';   /* For debugger */

  return result;
}


g95_expr *g95_simplify_cmplx(g95_expr *x, g95_expr *y, g95_expr *k) {
g95_expr *result, *arg1, *arg2, *e;
int kind;

  kind = get_kind(k, "CMPLX", g95_default_real_kind()); 
  if (kind == -1) return &g95_bad_expr;

  return NULL;


/* NOT QUITE READY */
/* How to handle optional arguments? */

/* Takes integer, real, or (if only x is present) complex input.
 * knd (optional) must be a valid complex kind */

  arg1 = FIRST_ARG(e);
  arg2 = SECOND_ARG(e);

  //  if (arg1->expr_type != EXPR_CONSTANT || arg2->expr_type != EXPR_CONSTANT)     return FAILURE;

  if (arg2 == NULL) {
  switch (arg1->ts.type) {
	case BT_COMPLEX:
	      g95_replace_expr(e, arg1);
	      break;
	case BT_INTEGER:
              result = g95_copy_expr(arg1);
	      g95_int2complex(&result, arg1);
              g95_replace_expr(e, result);
	      break;
	case BT_REAL:
              result = g95_copy_expr(arg1);
	      g95_real2complex(&result, arg1);
	      g95_replace_expr(e, result);
	      break;
	default :
	      g95_warning("Argument of CMPLX at %L is not a valid type",
			      &FIRST_ARG(e)->where);
	      //	      return FAILURE;
  }
  }
  else {
  switch (arg1->ts.type) {
	case BT_COMPLEX:
      		g95_warning("CMPLX at %L cannot take two arguments if the first is complex",
  	 	  &FIRST_ARG(e)->where);
		//		return FAILURE;
	      break;
	case BT_INTEGER:
	      result = g95_copy_expr(e);
	      mpf_init(result->value.complex.r);
	      mpf_init(result->value.complex.i);
	      mpf_set_z(result->value.complex.r, arg1->value.integer);
	      if ( arg2->ts.type == BT_INTEGER ) 
      	        mpf_set_z(result->value.complex.i, arg2->value.integer);
	      if (arg2->ts.type == BT_REAL )
      	        mpf_set(result->value.complex.i, arg2->value.real);
	      break;
	case BT_REAL:
	      result = g95_copy_expr(e);
              mpf_init(result->value.complex.r);
	      mpf_init(result->value.complex.i);
	      if ( arg2->ts.type == BT_INTEGER ) 
      	        mpf_set_z(result->value.complex.i, arg2->value.integer);
	      if (arg2->ts.type == BT_REAL )
      	        mpf_set(result->value.complex.i, arg2->value.real);
	      break;
	default :
	      g95_warning("Argument of CMPLX at %L is not a valid type",
			      &FIRST_ARG(e)->where);
  }
  }

  //  return SUCCESS;
} /* end simplify_cmplx */



g95_expr *g95_simplify_conjg(g95_expr *e) {
g95_expr *result;

  if (e->expr_type != EXPR_CONSTANT) return NULL;

  result = g95_copy_expr(e);
  mpf_neg(result->value.complex.i, result->value.complex.i);

  return result;
}



/* simplify_dble */
g95_expr *g95_simplify_dble(g95_expr *e) {
g95_expr *arg, *rmid, *result;

  return NULL; 

  arg = FIRST_ARG(e);

  //  if (arg->expr_type != EXPR_CONSTANT) return FAILURE;

  rmid = g95_copy_expr(arg);

  switch (arg->ts.type) {
	  case BT_INTEGER:
		  g95_int2real(&rmid, arg);
		  g95_real2double(&result, rmid);
		  break;
	  case BT_REAL:
		  g95_real2double(&result, arg);
		  break;
	  case BT_COMPLEX:
		  g95_complex2real(&rmid, arg);
		  g95_real2double(&result, rmid);
		  break;
	  default:
	          g95_warning("Argument of DBLE at %L is not a valid type",
			      &FIRST_ARG(e)->where);
	          g95_free_expr(rmid);
		  //                  return FAILURE;
  }

  g95_free_expr(rmid);

  return NULL;
}



g95_expr *g95_simplify_dim(g95_expr *e) {
g95_expr *arg1, *arg2, *result;
int knd1, knd2;

  return NULL; 

  arg1 = FIRST_ARG(e);
  arg2 = SECOND_ARG(e);

  if ( arg1->ts.type != BT_INTEGER || arg1->ts.type != BT_REAL ) {
    g95_warning("Arguments of DIM at %L must be integer or real",
		&FIRST_ARG(e)->where);
    //    return FAILURE;
  }

  if ( (arg1->ts.type == BT_INTEGER && arg2->ts.type != BT_INTEGER) ||
       (arg1->ts.type == BT_REAL && arg2->ts.type != BT_REAL ) ) {
    g95_warning("Type of arguments of DIM at %L must agree",
		&FIRST_ARG(e)->where);
    //    return FAILURE;
  }

  if ( arg1->ts.type == BT_INTEGER ) {
    knd1 = g95_validate_kind(BT_INTEGER, arg1->ts.type);
    knd2 = g95_validate_kind(BT_INTEGER, arg2->ts.type);
    if (knd1 != knd2 ) {
      g95_warning("Kind of arguments of DIM at %L must agree",
		&FIRST_ARG(e)->where);
      //      return FAILURE;
    }
  }

  //  if (arg1->expr_type != EXPR_CONSTANT || arg2->expr_type != EXPR_CONSTANT)     return FAILURE;

  result = g95_get_expr();

  if (g95_compare_expr(arg1, arg2) > 0) {
    g95_arith_minus(arg1, arg2, &result);
  }
  else 
    result = g95_copy_expr(0);

  return NULL;
}



g95_expr *g95_simplify_dprod(g95_expr *e) {
g95_expr *arg1, *arg2, *dbl1, *dbl2, *result;
arith r1, r2, r;

  return NULL; 

  arg1 = FIRST_ARG(e);
  arg2 = SECOND_ARG(e);

  if (arg1->ts.type != BT_REAL || arg2->ts.type != BT_REAL ) {
     g95_warning("Arguments of DPROD at %L must be real",
        	    &FIRST_ARG(e)->where);
     //     return FAILURE;
  }

  //  if (arg1->expr_type != EXPR_CONSTANT) return FAILURE;

  r1 = g95_real2double(&dbl1, arg1);
  r2 = g95_real2double(&dbl2, arg2);

  if (r1 == ARITH_OK && r2 == ARITH_OK) {
    r = g95_arith_times(dbl1, dbl2, &result);
    if (r == ARITH_OK) {
      result = g95_get_expr();
      r = g95_arith_times(dbl1, dbl2, &result);
      if (r == ARITH_OK) {
	g95_replace_expr(e, result);
	g95_free_expr(dbl1);
	g95_free_expr(dbl2);
	//        return SUCCESS;
      }
      else {
        g95_warning("Invalid result in DPROD at %L",
               	      &FIRST_ARG(e)->where);
	g95_free_expr(dbl1);
	g95_free_expr(dbl2);
	g95_free_expr(result);
	//	return FAILURE;
      }
    }
    else {
     g95_warning("Invalid result in DPROD at %L",
        	    &FIRST_ARG(e)->where);
     g95_free_expr(dbl1);
     g95_free_expr(dbl2);
     //     return FAILURE;
    }
  } 
  else {
     g95_warning("Conversion failed in DPROD at %L",
        	    &FIRST_ARG(e)->where);
     g95_free_expr(dbl1);
     g95_free_expr(dbl2);
     //     return FAILURE;
  }

  return NULL;
}


g95_expr *g95_simplify_epsilon(g95_expr *e) {
g95_expr *arg, *result;
int i;

  return NULL; 

  arg = FIRST_ARG(e);
  i = g95_validate_kind(arg->ts.type, arg->ts.kind);
  if (i == -1) g95_internal_error("g95_simplify_epsilon(): Bad kind");

  result = g95_constant_result(BT_REAL, arg->ts.kind);
  mpf_init_set(result->value.real, g95_real_kinds[i].epsilon);

  g95_replace_expr(e, result);

  return NULL;
}


/* simplify_exp */
g95_expr *g95_simplify_exp(g95_expr *e) {
g95_expr *arg;

  return NULL; 

  arg = FIRST_ARG(e);

/* Type checking */

  if (arg->ts.type != BT_REAL) {
    g95_warning("Argument of EXP at %L must be real",
		&FIRST_ARG(e)->where);
    //    return FAILURE;
  }

  //  if (arg->expr_type != EXPR_CONSTANT) return FAILURE;

  //  return SUCCESS;
}



g95_expr *g95_simplify_exponent(g95_expr *e) {
g95_expr *arg;

  return NULL; 

  arg = FIRST_ARG(e);

/* Type checking */

  if (arg->ts.type != BT_REAL) {
    g95_warning("Argument of EXPONENT at %L must be real",
		&FIRST_ARG(e)->where);
    //    return FAILURE;
  }

  //  if (arg->expr_type != EXPR_CONSTANT) return FAILURE;

  //  return SUCCESS;
}


/* simplify_floor */
g95_expr *g95_simplify_floor(g95_expr *e) {
g95_expr *arg, *floor, *result;
int knd;
arith r;
/* Result needs to have correct KIND */
/* knd (optional) must be a valid integer kind */

  return NULL; 


  arg = FIRST_ARG(e);

  if (arg->ts.type != BT_REAL) {
    g95_warning("Argument of FLOOR at %L must be real",
		&SECOND_ARG(e)->where);
    //    return FAILURE;
  }

  knd = g95_validate_kind(BT_REAL, arg->ts.type);

  //  if (arg->expr_type != EXPR_CONSTANT) return FAILURE;

  floor = g95_copy_expr(arg);

  mpf_init(floor->value.real);
  mpf_floor(floor->value.real, arg->value.real);

  result = g95_get_expr();

  r = g95_real2int(&result, floor);

  g95_free_expr(floor);

  if ( r == ARITH_OK ) {
    g95_replace_expr(e, result);
    //    return SUCCESS;
  }
  else {
    g95_warning("Conversion in ANINT at %L failed",
		&SECOND_ARG(e)->where);
    g95_free_expr(result);
    // return FAILURE;
  }
}



g95_expr *g95_simplify_fraction(g95_expr *e) {
g95_expr *arg;

  return NULL; 

  arg = FIRST_ARG(e);

  if (arg->ts.type != BT_REAL) {
    g95_warning("Argument of FRACTION at %L must be real",
		&FIRST_ARG(e)->where);
    //    return FAILURE;
  }

  //  if (arg->expr_type != EXPR_CONSTANT) return FAILURE;

  //return SUCCESS;
}


g95_expr *g95_simplify_huge(g95_expr *e) {
g95_expr *arg, *result;
int i;

  return NULL; 

  arg = FIRST_ARG(e);
  i = g95_validate_kind(arg->ts.type, arg->ts.kind);
  if (i == -1) goto bad_type;

  result = g95_constant_result(arg->ts.type, arg->ts.kind);

  switch(arg->ts.type) {
  case BT_INTEGER:
    mpz_init_set(result->value.integer, g95_integer_kinds[i].maxval);
    break;

  case BT_REAL:
    mpf_init_set(result->value.real, g95_real_kinds[i].maxval);
    break;

  bad_type:
  default:
    g95_internal_error("g95_simplify_huge(): Bad type");
  }

  g95_replace_expr(e, result);
  //  return SUCCESS;
}



g95_expr *g95_simplify_iachar(g95_expr *e) {
g95_expr *arg;

  return NULL; 

/* Type checking */

  arg = FIRST_ARG(e);

  /* Need to check character length */
  if (arg->ts.type != BT_CHARACTER ) {
    g95_warning("Argument of IACHAR at %L must be character and of length one",
		&FIRST_ARG(e)->where);
    //    return FAILURE;
  }

  //  if (arg->expr_type != EXPR_CONSTANT) return FAILURE;

  // return SUCCESS;
}



g95_expr *g95_simplify_iand(g95_expr *e) {
g95_expr *arg1, *arg2;
int knd1, knd2;

  return NULL; 

/* Type checking */

  arg1 = FIRST_ARG(e);
  arg2 = SECOND_ARG(e);

  if (arg1->ts.type != BT_INTEGER || arg2->ts.type != BT_INTEGER) {
    g95_warning("Arguments of IAND at %L must be integer",
		&FIRST_ARG(e)->where);
    //    return FAILURE;
  }

  knd1 = g95_validate_kind(BT_INTEGER, arg1->ts.type);
  knd2 = g95_validate_kind(BT_INTEGER, arg2->ts.type);
  if (knd1 != knd2 ) {
    g95_warning("Kind of arguments of IAND at %L must agree",
                 &FIRST_ARG(e)->where);
    //    return FAILURE;
  }

  // if (arg1->expr_type != EXPR_CONSTANT || arg2->expr_type != EXPR_CONSTANT)     return FAILURE;

  //  return SUCCESS;
}


g95_expr *g95_simplify_ibclr(g95_expr *e) {
g95_expr *arg1, *arg2;

  return NULL; 

/* Type checking */

  arg1 = FIRST_ARG(e);
  arg2 = SECOND_ARG(e);

  if (arg1->ts.type != BT_INTEGER || arg2->ts.type != BT_INTEGER ) {
    g95_warning("Arguments of IBCLR at %L must be integer",
		&FIRST_ARG(e)->where);
    //    return FAILURE;
  }

/* Range checking */
/* Second argument must be nonnegative and less than bit_size(i) */

  if ( g95_compare_expr(arg2, integer_zero) < 0 ) {
    g95_warning("Last argument of IBCLR at %L must be nonnegative",
		&FIRST_ARG(e)->where);
    //    return FAILURE;
  }
/* Second argument must be less than BIT_SIZE(I), no check yet */

  //  if (arg1->expr_type != EXPR_CONSTANT || arg2->expr_type != EXPR_CONSTANT)     return FAILURE;

  //  return SUCCESS;
}


/* simplify_ibits() */
g95_expr *g95_simplify_ibits(g95_expr *e) {
g95_expr *arg1, *arg2, *arg3;

  return NULL; 

/* Type checking */

  arg1 = FIRST_ARG(e);
  arg2 = SECOND_ARG(e);
  arg3 = THIRD_ARG(e);

  if (arg1->ts.type != BT_INTEGER || arg2->ts.type != BT_INTEGER 
		  || arg3->ts.type != BT_INTEGER ) {
    g95_warning("Arguments of IBITS at %L must be integer",
		&FIRST_ARG(e)->where);
    //    return FAILURE;
  }

/* Range checking */
/* Second argument must be nonnegative and pos+ln must be less than
 * bit_size(i); third argument must be nonnegative */

  if ( g95_compare_expr(arg2, integer_zero) < 0 ) {
    g95_warning("Second argument of IBITS at %L must be nonnegative",
		&FIRST_ARG(e)->where);
    //    return FAILURE;
  }

  if ( g95_compare_expr(arg3, integer_zero) < 0 ) {
    g95_warning("Last argument of IBITS at %L must be nonnegative",
		&FIRST_ARG(e)->where);
    //    return FAILURE;
  }
/* Arg2+arg3 must be less than or equal to BIT_SIZE(I), no check yet */

  //  if (arg1->expr_type != EXPR_CONSTANT || arg2->expr_type != EXPR_CONSTANT ||	  arg3->expr_type != EXPR_CONSTANT ) return FAILURE;

  //  return SUCCESS;
}


g95_expr *g95_simplify_ibset(g95_expr *e) {
g95_expr *arg1, *arg2;

  return NULL; 

/* Type checking */

  arg1 = FIRST_ARG(e);
  arg2 = SECOND_ARG(e);

  if (arg1->ts.type != BT_INTEGER || arg2->ts.type != BT_INTEGER ) {
    g95_warning("Arguments of IBSET at %L must be integer",
		&FIRST_ARG(e)->where);
    //    return FAILURE;
  }

/* Range checking */
/* Second argument must be nonnegative and less than bit_size(i) */

  if ( g95_compare_expr(arg2, integer_zero) < 0 ) {
    g95_warning("Last argument of IBSET at %L must be nonnegative",
		&FIRST_ARG(e)->where);
    //    return FAILURE;
  }
/* Second argument must be less than BIT_SIZE(I), no check yet */

  //  return SUCCESS;
}


g95_expr *g95_simplify_ichar(g95_expr *e) {
g95_expr *arg;

  return NULL; 

/* Type checking */

/* Argument must be of length 1; its value must be representable by the 
 * processor */

  arg = FIRST_ARG(e);

  /* Need to check character length */
  if (arg->ts.type != BT_CHARACTER ) {
    g95_warning("Argument of IACHAR at %L must be character and of length one",
		&FIRST_ARG(e)->where);
    //    return FAILURE;
  }

  //  if (arg->expr_type != EXPR_CONSTANT) return FAILURE;

  //  return SUCCESS;
} /* end simplify_ichar */



g95_expr *g95_simplify_ieor(g95_expr *e) {
g95_expr *arg1, *arg2;
int knd1, knd2;

  return NULL; 

/* Type checking */

  arg1 = FIRST_ARG(e);
  arg2 = SECOND_ARG(e);

  if (arg1->ts.type != BT_INTEGER && arg2->ts.type != BT_INTEGER) {
    g95_warning("Arguments of IEOR at %L must be integer",
		&FIRST_ARG(e)->where);
    //    return FAILURE;
  }

  knd1 = g95_validate_kind(BT_INTEGER, arg1->ts.type);
  knd2 = g95_validate_kind(BT_INTEGER, arg2->ts.type);
  if (knd1 != knd2 ) {
    g95_warning("Kind of arguments of IEOR at %L must agree",
                 &FIRST_ARG(e)->where);
    //    return FAILURE;
  }

  //  if (arg1->expr_type != EXPR_CONSTANT || arg2->expr_type != EXPR_CONSTANT) return FAILURE;

  //  return SUCCESS;
}


g95_expr *g95_simplify_index(g95_expr *e) {
g95_expr *arg1, *arg2, *arg3;

  return NULL; 
  
/* Takes optional argument, not really implemented */

/* Type checking */

  arg1 = FIRST_ARG(e);
  arg2 = SECOND_ARG(e);

  if (arg1->ts.type != BT_CHARACTER || arg2->ts.type != BT_CHARACTER) {
    g95_warning("First two arguments of INDEX at %L must be character",
		&FIRST_ARG(e)->where);
    //    return FAILURE;
  }

  if (arg3->ts.type != BT_LOGICAL) {
    g95_warning("Optional argument of INDEX at %L must be logical",
		&FIRST_ARG(e)->where);
    //    return FAILURE;
  }

  //  if (arg1->expr_type != EXPR_CONSTANT || arg2->expr_type != EXPR_CONSTANT)     return FAILURE;

  //  return SUCCESS;
}


g95_expr *g95_simplify_int(g95_expr *e) {
g95_expr *arg, *rmid, *rtrunc, *result;
int knd;
arith r;

  return NULL; 

/* Result needs to have correct KIND */
/* knd (optional) must be a valid integer kind */

  arg = FIRST_ARG(e);

  knd = g95_validate_kind(BT_REAL, arg->ts.type);

  //  if (arg->expr_type != EXPR_CONSTANT) return FAILURE;

  result = g95_copy_expr(arg);

  switch ( arg->ts.type ) {
          case BT_INTEGER:
	          break;
          case BT_REAL:
		  rmid = g95_get_expr();
                  mpf_init(rmid->value.real);
                  mpf_trunc(rmid->value.real, arg->value.real);
                  r = g95_real2int(&result, rmid);
		  g95_free(rmid);
                  if ( r == ARITH_OK ) {
		    g95_replace_expr(e, result);
		    //		    return SUCCESS;
		  }
                  else {
                    g95_warning("Conversion in NINT at %L failed",
	               	        &SECOND_ARG(e)->where);
		    g95_free_expr(result);
		    //                    return FAILURE;
                  }
	          break;
          case BT_COMPLEX:
	          rmid   = g95_get_expr();
	          rtrunc = g95_get_expr();
		  g95_complex2real(&rmid,arg);
		  mpf_init(rtrunc->value.real);
	          mpf_trunc(rtrunc->value.real, rmid->value.real);
                  r = g95_real2int(&result, rtrunc);
		  g95_free_expr(rmid);
		  g95_free_expr(rtrunc);
                  if ( r == ARITH_OK ) {
		    g95_replace_expr(e, result);
		    //		    return SUCCESS;
		  }
                  else {
                    g95_warning("Conversion in NINT at %L failed",
	                	&SECOND_ARG(e)->where);
		    g95_free_expr(result);
		    //                    return FAILURE;
		  }
	          break;
	  default:
	          g95_warning("Argument of INT at %L is not a valid type",
			      &FIRST_ARG(e)->where);
		  //                  return FAILURE;
  }

  //  return SUCCESS;
}


g95_expr *g95_simplify_ior(g95_expr *e) {
g95_expr *arg1, *arg2;
int knd1, knd2;

  return NULL; 

/* Type checking */

  arg1 = FIRST_ARG(e);
  arg2 = SECOND_ARG(e);

  if (arg1->ts.type != BT_INTEGER && arg2->ts.type != BT_INTEGER) {
    g95_warning("Arguments of IOR at %L must be integer",
		&FIRST_ARG(e)->where);
    //    return FAILURE;
  }

  knd1 = g95_validate_kind(BT_INTEGER, arg1->ts.type);
  knd2 = g95_validate_kind(BT_INTEGER, arg2->ts.type);
  if (knd1 != knd2 ) {
    g95_warning("Kind of arguments of IOR at %L must agree",
                 &FIRST_ARG(e)->where);
    //    return FAILURE;
  }

  //  if (arg1->expr_type != EXPR_CONSTANT || arg2->expr_type != EXPR_CONSTANT)    return FAILURE;

  //  return SUCCESS;
}



g95_expr *g95_simplify_ishft(g95_expr *e) {
g95_expr *arg1, *arg2;

  return NULL; 

/* Type checking */

  arg1 = FIRST_ARG(e);
  arg2 = SECOND_ARG(e);

  if (arg1->ts.type != BT_INTEGER || arg2->ts.type != BT_INTEGER ) {
    g95_warning("Arguments of ISHFT at %L must be integer",
		&FIRST_ARG(e)->where);
    //    return FAILURE;
  }

/* Range checking */
/* Absolute value of second argument (the shift) must be <=bit_size(i) */

  if ( g95_compare_expr(arg2, integer_zero) < 0 ) {
    g95_warning("Last argument of ISHFT at %L must be nonnegative",
		&FIRST_ARG(e)->where);
    //    return FAILURE;
  }
/* Second argument must be less than or equal to BIT_SIZE(I), no check yet */

  //  if (arg1->expr_type != EXPR_CONSTANT || arg2->expr_type != EXPR_CONSTANT)     return FAILURE;

  //  return SUCCESS;
}


g95_expr *g95_simplify_ishftc(g95_expr *e) {
g95_expr *arg1, *arg2;

  return NULL; 

/* Takes optional argument, not implemented */

/* Type checking */

  arg1 = FIRST_ARG(e);
  arg2 = SECOND_ARG(e);

  if (arg1->ts.type != BT_INTEGER || arg2->ts.type != BT_INTEGER ) {
    g95_warning("Arguments of ISHFTC at %L must be integer",
		&FIRST_ARG(e)->where);
    //    return FAILURE;
  }

/* Range checking */

/* Optional argument must be positive and less than or equal to BIT_SIZE(I) */
/* If not present, it is treated as if equal to BIT_SIZE(I) */

/* no check yet on bit_size */

  //  if (arg1->expr_type != EXPR_CONSTANT || arg2->expr_type != EXPR_CONSTANT)     return FAILURE;

  //  return SUCCESS;
}



g95_expr *g95_simplify_kind(g95_expr *e) {

  if (e->ts.type == BT_DERIVED) {
    g95_error("Argument of KIND at %L is a DERIVED type", &e->where);
    return &g95_bad_expr;
  }

  return g95_int_expr(e->ts.kind);
}


g95_expr *g95_simplify_len_trim(g95_expr *e) {
g95_expr *arg;

  return NULL; 

/* Type checking */

  arg = FIRST_ARG(e);

  if (arg->ts.type != BT_CHARACTER ) {
    g95_warning("Argument of LEN_TRIM at %L must be character",
		&FIRST_ARG(e)->where);
    //    return FAILURE;
  }

  //  if (arg->expr_type != EXPR_CONSTANT) return FAILURE;

  //  return SUCCESS;
}


g95_expr *g95_simplify_lge(g95_expr *e) {
g95_expr *arg1, *arg2;

  return NULL; 

/* Type checking */

  arg1 = FIRST_ARG(e);
  arg2 = SECOND_ARG(e);

  if (arg1->ts.type != BT_CHARACTER || arg2->ts.type != BT_CHARACTER ) {
    g95_warning("Arguments of LGE at %L must be character",
		&FIRST_ARG(e)->where);
    //    return FAILURE;
  }

  //  if (arg1->expr_type != EXPR_CONSTANT || arg2->expr_type != EXPR_CONSTANT)     return FAILURE;

  //  return SUCCESS;
} /* end simplify_lge */


g95_expr *g95_simplify_lgt(g95_expr *e) {
g95_expr *arg1, *arg2;

  return NULL; 

/* Type checking */

  arg1 = FIRST_ARG(e);
  arg2 = SECOND_ARG(e);

  if (arg1->ts.type != BT_CHARACTER || arg2->ts.type != BT_CHARACTER ) {
    g95_warning("Arguments of LGT at %L must be character",
		&FIRST_ARG(e)->where);
    //    return FAILURE;
  }

  //  if (arg1->expr_type != EXPR_CONSTANT || arg2->expr_type != EXPR_CONSTANT)     return FAILURE;

  //  return SUCCESS;
}


g95_expr *g95_simplify_lle(g95_expr *e) {
g95_expr *arg1, *arg2;

  return NULL; 

/* Type checking */

  arg1 = FIRST_ARG(e);
  arg2 = SECOND_ARG(e);

  if (arg1->ts.type != BT_CHARACTER || arg2->ts.type != BT_CHARACTER ) {
    g95_warning("Arguments of LLE at %L must be character",
		&FIRST_ARG(e)->where);
    //    return FAILURE;
  }

  //  if (arg1->expr_type != EXPR_CONSTANT || arg2->expr_type != EXPR_CONSTANT)     return FAILURE;

  //  return SUCCESS;
}



g95_expr *g95_simplify_llt(g95_expr *e) {
g95_expr *arg1, *arg2;

  return NULL; 

/* Type checking */

  arg1 = FIRST_ARG(e);
  arg2 = SECOND_ARG(e);

  if (arg1->ts.type != BT_CHARACTER || arg2->ts.type != BT_CHARACTER ) {
    g95_warning("Arguments of LLE at %L must be character",
		&FIRST_ARG(e)->where);
    //    return FAILURE;
  }

  //  if (arg1->expr_type != EXPR_CONSTANT || arg2->expr_type != EXPR_CONSTANT)     return FAILURE;

  //  return SUCCESS;
}


g95_expr *g95_simplify_log(g95_expr *e) {
g95_expr *arg;

  return NULL; 

  arg = FIRST_ARG(e);

/* Type checking */

  if (arg->ts.type != BT_REAL || arg->ts.type != BT_COMPLEX) {
    g95_warning("Argument of LOG at %L must be real or complex",
		&FIRST_ARG(e)->where);
    //    return FAILURE;
  }

/* Range checking */
/* If x is real, x must be >zero. If complex, its value must not be zero */

  if (arg->ts.type == BT_REAL ) {
    if ( g95_compare_expr(arg, real_zero) <= 0 ) {
      g95_warning("Argument of LOG at %L cannot be less than or equal to zero",
  		  &FIRST_ARG(e)->where);
      //      return FAILURE;
    }
  }

  if (arg->ts.type == BT_COMPLEX ) {
    if ( (mpf_cmp(arg->value.complex.r, mpf_zero) == 0) && 
         (mpf_cmp(arg->value.complex.i, mpf_zero) == 0) ) {
      g95_warning("Complex argument of LOG at %L cannot be zero",
  		  &FIRST_ARG(e)->where);
      //      return FAILURE;
    }
  }

  //  if (arg->expr_type != EXPR_CONSTANT) return FAILURE;

  //  return SUCCESS;
} /* end simplify_log */



g95_expr *g95_simplify_log10(g95_expr *e) {
g95_expr *arg;

  return NULL; 

  arg = FIRST_ARG(e);

/* Type checking */

  if (arg->ts.type != BT_REAL) {
    g95_warning("Argument of LOG10 at %L must be real",
		&FIRST_ARG(e)->where);
    //    return FAILURE;
  }

/* Range checking */
/* Argument must be >zero */

  if ( g95_compare_expr(arg, real_zero) <= 0 ) {
    g95_warning("Argument of LOG10 at %L cannot be less than or equal to zero",
    	        &FIRST_ARG(e)->where);
    //    return FAILURE;
  }

  //  if (arg->expr_type != EXPR_CONSTANT) return FAILURE;

  //  return SUCCESS;
}


g95_expr *g95_simplify_logical(g95_expr *e) {
g95_expr *arg;
int knd;

  return NULL; 

/* Takes optional argument, not handled yet */

/* Type checking */

  arg = FIRST_ARG(e);

  if (arg->ts.type != BT_LOGICAL) {
    g95_warning("Argument of LOGICAL at %L must be logical",
		&FIRST_ARG(e)->where);
    //    return FAILURE;
  }

  knd = g95_validate_kind(BT_INTEGER, arg->ts.type);

  //  if (arg->expr_type != EXPR_CONSTANT) return FAILURE;

  //  return SUCCESS;
}


g95_expr *g95_simplify_max(g95_expr *e) {

  return NULL;
}



g95_expr *g95_simplify_maxexponent(g95_expr *e) {
g95_expr *arg;

  return NULL; 

/* Type checking */

  arg = FIRST_ARG(e);

  if (arg->ts.type != BT_REAL) {
    g95_warning("Argument of MAXEXPONENT at %L must be real",
		&FIRST_ARG(e)->where);
    //    return FAILURE;
  }

  //  if (arg->expr_type != EXPR_CONSTANT) return FAILURE;

  //  return SUCCESS;

}


g95_expr *g95_simplify_min(g95_expr *e) {

  return NULL; 
}


g95_expr *g95_simplify_minexponent(g95_expr *e) {
g95_expr *arg;

  return NULL; 

/* Type checking */

  arg = FIRST_ARG(e);

  if (arg->ts.type != BT_REAL) {
    g95_warning("Argument of MINEXPONENT at %L must be real",
		&FIRST_ARG(e)->where);
    //    return FAILURE;
  }

  //  if (arg->expr_type != EXPR_CONSTANT) return FAILURE;

  //  return SUCCESS;

}


g95_expr *g95_simplify_mod(g95_expr *e) {
g95_expr *arg1, *arg2, *rmid1, *rmid2, *result;

  return NULL; 

  arg1 = FIRST_ARG(e);
  arg2 = SECOND_ARG(e);

  //  if (arg1->expr_type != EXPR_CONSTANT && arg2->expr_type != EXPR_CONSTANT)     return FAILURE;

  switch (arg1->ts.type) {
    	case BT_INTEGER:
	    	if (arg2->ts.type != BT_INTEGER ) {
	     	        g95_warning("Type of arguments of MOD at %L must agree",
  				&FIRST_ARG(e)->where);
			//      		        return FAILURE;
    		}
    		else {
	      	  if (g95_compare_expr(arg2, integer_zero) != 0) {
			result = g95_copy_expr(e);
       		 	mpz_init(result->value.integer);
       		 	mpz_mod(result->value.integer, arg1->value.integer, 
                                                       arg2->value.integer);
			g95_replace_expr(e, result);
			//			return SUCCESS;
      		  }
      		  else {
		  /* Result is processor-dependent */
       		 	g95_warning("Second argument MOD at %L is zero",
  		       		  &FIRST_ARG(e)->where);
			//       		 	return FAILURE;
      		  }
		}
	    	break;
    	case BT_REAL:
	    	if (arg2->ts.type != BT_REAL ) {
		      g95_warning("Type of arguments of MOD at %L must agree",
  				&FIRST_ARG(e)->where);
		      //      		      return FAILURE;
    		}
	    	else {
      		  if (g95_compare_expr(arg2, real_zero) != 0) {
			rmid1 = g95_get_expr();
			rmid2 = g95_get_expr();
			result =g95_copy_expr(e);
			g95_real2int( &rmid1, arg1 );
			g95_real2int( &rmid2, arg2 );
 			mpz_init(result->value.integer);
                    	mpz_mod (result->value.integer, arg1->value.integer,
			                                arg2->value.integer);
			g95_replace_expr(e, result);
			g95_free_expr(rmid1);
			g95_free_expr(rmid2);
			//	                return SUCCESS;
      		  }
      		  else {
		  /* Result is processor-dependent */
       		 	g95_warning("Second argument of MOD at %L is zero",
  	       			  &FIRST_ARG(e)->where);
			//	        	return FAILURE;
      		  }
		}
    		break;
	    default:
       		g95_warning("Type of arguments of MOD at %L must be real or integer",
	 	       	&FIRST_ARG(e)->where);
		//		return FAILURE;
  }

/* If we get to here something went wrong */
  //  return FAILURE;

}


g95_expr *g95_simplify_modulo(g95_expr *e) {

  return NULL; 
}


/* simplify_mvbits() */
g95_expr *g95_simplify_mvbits(g95_expr *e) {
g95_expr *arg1, *arg2, *arg3, *arg4, *arg5;

  return NULL; 
 
/* Type checking */

  arg1 = FIRST_ARG(e);
  arg2 = SECOND_ARG(e);
  arg3 = THIRD_ARG(e);

  arg4 = e->value.function.actual->next->next->expr;
  arg5 = e->value.function.actual->next->next->expr;

  if (arg1->ts.type != BT_INTEGER || arg2->ts.type != BT_INTEGER 
		  || arg3->ts.type != BT_INTEGER 
		  || arg4->ts.type != BT_INTEGER 
		  || arg5->ts.type != BT_INTEGER ) {
    g95_warning("Arguments of MVBITS at %L must be integer",
		&FIRST_ARG(e)->where);
    //    return FAILURE;
  }

  if (g95_compare_expr(arg2, integer_zero) < 0) {
    g95_warning("Second argument of MVBITS at %L must be nonnegative",
		&FIRST_ARG(e)->where);
    //    return FAILURE;
  }

  if (g95_compare_expr(arg3, integer_zero) < 0) {
    g95_warning("Third argument of MVBITS at %L must be nonnegative",
		&FIRST_ARG(e)->where);
    //    return FAILURE;
  }

  if (g95_compare_expr(arg5, integer_zero) < 0) {
    g95_warning("Third argument of MVBITS at %L must be nonnegative",
		&FIRST_ARG(e)->where);
    //    return FAILURE;
  }

  //  return SUCCESS;

}


g95_expr *g95_simplify_nearest(g95_expr *e) {
g95_expr *arg1, *arg2;

  return NULL; 

/* Type checking */

  arg1 = FIRST_ARG(e);
  arg2 = SECOND_ARG(e);

  if (arg1->ts.type != BT_REAL && arg2->ts.type != BT_INTEGER) {
    g95_warning("Illegal type in NEAREST at %L",
		&FIRST_ARG(e)->where);
    //    return FAILURE;
  }

/* Range checking */
/* Second argument must not be equal to zero */

  if (g95_compare_expr(arg2, real_zero) == 0) {
    g95_warning("Second argument of NEAREST at %L is zero",
		&FIRST_ARG(e)->where);
    //    return FAILURE;
  }

  //  if (arg1->expr_type != EXPR_CONSTANT || arg2->expr_type != EXPR_CONSTANT )     return FAILURE;

  //  return SUCCESS;

}


g95_expr *g95_simplify_nint(g95_expr *e) {
g95_expr *arg, *rmid, *rtrunc, *result;
int knd;
arith r;

  return NULL; 

/* Result needs to have correct KIND */
/* Takes optional KIND argument, not implemented */

  arg = FIRST_ARG(e);

  if (arg->ts.type != BT_REAL) {
    g95_warning("Argument of NINT at %L must be real",
		&SECOND_ARG(e)->where);
    //    return FAILURE;
  }

  knd = g95_validate_kind(BT_REAL, arg->ts.type);

  //  if (arg->expr_type != EXPR_CONSTANT) return FAILURE;

  rmid = g95_copy_expr(arg);

  if ( g95_compare_expr(arg, real_zero) <= 0 ) {
    mpf_init(rmid->value.real);
    mpf_sub(rmid->value.real, arg->value.real, mpf_half);
  }
  else if ( g95_compare_expr(arg, real_zero) > 0 ) {
    mpf_init(rmid->value.real);
    mpf_add(rmid->value.real, arg->value.real, mpf_half);
  }

  rtrunc = g95_copy_expr(rmid);
  mpf_trunc(rtrunc->value.real,rmid->value.real);

  r = g95_real2int(&result, rtrunc);

  g95_free_expr(rmid);
  g95_free_expr(rtrunc);

  if ( r == ARITH_OK ) {
    g95_replace_expr(e, result);
    //    return SUCCESS;
  }
  else {
    g95_warning("Conversion in NINT at %L failed",
		&SECOND_ARG(e)->where);
    //    return FAILURE;
  }

}


g95_expr *g95_simplify_not(g95_expr *e) {
g95_expr *arg;
int knd;

  return NULL; 

/* Type checking */

  arg = FIRST_ARG(e);

  if (arg->ts.type != BT_INTEGER) {
    g95_warning("Argument of NOT at %L must be integer",
		&FIRST_ARG(e)->where);
    //    return FAILURE;
  }

  knd = g95_validate_kind(BT_INTEGER, arg->ts.type);

  //  if (arg->expr_type != EXPR_CONSTANT) return FAILURE;

  //  return SUCCESS;
} /* end simplify_not */


g95_expr *g95_simplify_precision(g95_expr *e) {
int i;

  i = g95_validate_kind(e->ts.type, e->ts.kind);
  if (i == -1) g95_internal_error("g95_simplify_precision(): Bad kind");

  return g95_int_expr(g95_real_kinds[i].precision);
}


g95_expr *g95_simplify_radix(g95_expr *e) {

  return g95_int_expr(2);  /* TODO: This needs to be set from target config */
}


g95_expr *g95_simplify_range(g95_expr *e) {
int i;

  i = g95_validate_kind(e->ts.type, e->ts.kind);
  if (i == -1) goto bad_type;

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

  return g95_int_expr(i);
}



g95_expr *g95_simplify_real(g95_expr *e) {
g95_expr *arg, *result;
arith r;

  return NULL; 

/* Needs KIND */
/* knd (optional) must be a valid real kind */

  arg = FIRST_ARG(e);

  //  if (arg->expr_type != EXPR_CONSTANT) return FAILURE;

  result = g95_copy_expr(arg);

  switch (arg->ts.type) {
	case BT_INTEGER:
            r = g95_int2real(&result, arg);
	    if ( r == ARITH_OK ) {
		g95_replace_expr(e, result);
		//	        return SUCCESS;
	    }
	    else {
	        g95_free_expr(result);
		//		return FAILURE;
	    }
	    break;

	case BT_REAL:
	    g95_replace_expr(e, result);
	    //	    return SUCCESS;
	    break;

	case BT_COMPLEX:
       	    mpf_init(result->value.real);
	    mpf_set(result->value.real, arg->value.complex.r);
            g95_replace_expr(e, result);
	    //	    return SUCCESS;
	    break;

        default:
       	    g95_warning("Invalid argument type in REAL at %L",
 	       	&FIRST_ARG(e)->where);
	    g95_free(result);
	    //	    return FAILURE;
  }

  //  return SUCCESS;

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
g95_expr *arg1, *arg2;

  return NULL; 

/* Type checking */

  arg1 = FIRST_ARG(e);
  arg2 = SECOND_ARG(e);

  if (arg1->ts.type != BT_REAL) {
    g95_warning("First argument of SCALE at %L must be real",
		&FIRST_ARG(e)->where);
    //    return FAILURE;
  }

  if (arg2->ts.type != BT_INTEGER) {
    g95_warning("Second argument of SCALE at %L must be integer",
		&FIRST_ARG(e)->where);
    //    return FAILURE;
  }

  //  if (arg1->expr_type != EXPR_CONSTANT || arg2->expr_type != EXPR_CONSTANT )     return FAILURE;

  //  return SUCCESS;
}


g95_expr *g95_simplify_scan(g95_expr *e) {
g95_expr *arg1, *arg2;
int knd1, knd2;

  return NULL; 

/* Takes optional argument, not implemented */

/* Type checking */

  arg1 = FIRST_ARG(e);
  arg2 = SECOND_ARG(e);

  if (arg1->ts.type != BT_CHARACTER || arg2->ts.type != BT_CHARACTER ) {
    g95_warning("Arguments of SCAN at %L must be character",
		&FIRST_ARG(e)->where);
    //    return FAILURE;
  }

  knd1 = g95_validate_kind(BT_INTEGER, arg1->ts.type);
  knd2 = g95_validate_kind(BT_INTEGER, arg2->ts.type);

  if (knd1 != knd2 ) {
    g95_warning("Kind of arguments of SCAN at %L must agree",
                 &FIRST_ARG(e)->where);
    //    return FAILURE;
  }

  //  if (arg1->expr_type != EXPR_CONSTANT || arg2->expr_type != EXPR_CONSTANT)     return FAILURE;

  //  return SUCCESS;
} /* end simplify_scan */



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

  if (q != NULL)
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
g95_expr *arg1, *arg2;

  return NULL; 

/* Type checking */

  arg1 = FIRST_ARG(e);
  arg2 = SECOND_ARG(e);

  if (arg1->ts.type != BT_REAL) {
    g95_warning("First argument of SET_EXPONENT at %L must be real",
		&FIRST_ARG(e)->where);
    //    return FAILURE;
  }

  if (arg2->ts.type != BT_INTEGER) {
    g95_warning("Second argument of SET_EXPONENT at %L must be real",
		&FIRST_ARG(e)->where);
    //    return FAILURE;
  }

  //  if (arg1->expr_type != EXPR_CONSTANT || arg2->expr_type != EXPR_CONSTANT)     return FAILURE;

  //  return SUCCESS;
}


g95_expr *g95_simplify_sign(g95_expr *e) {
g95_expr *arg1, *arg2, *rmid, *result;
int knd1, knd2;

  return NULL;

  arg1 = FIRST_ARG(e);
  arg2 = SECOND_ARG(e);

/* Type checking */

  if (arg1->ts.type != BT_REAL || arg1->ts.type != BT_INTEGER) {
    g95_warning("First argument of SIGN at %L must be real or integer",
		&FIRST_ARG(e)->where);
    //    return FAILURE;
  }

  if ( (arg2->ts.type != arg1->ts.type) ) {
    g95_warning("Type of arguments of SET_EXPONENT at %L must agree",
		&FIRST_ARG(e)->where);
    //    return FAILURE;
  }

  knd1 = g95_validate_kind(BT_INTEGER, arg1->ts.type);
  knd2 = g95_validate_kind(BT_INTEGER, arg2->ts.type);

  if (knd1 != knd2 ) {
    g95_warning("Kind of arguments of SIGN at %L must agree",
                 &FIRST_ARG(e)->where);
    //    return FAILURE;
  }

  //  if (arg1->expr_type != EXPR_CONSTANT || arg2->expr_type != EXPR_CONSTANT)     return FAILURE;

  result = g95_copy_expr(arg1);

  switch (arg1->ts.type) {
	case BT_INTEGER:
        	mpz_init(result->value.integer);
    		if ( g95_compare_expr(arg2, integer_zero) >= 0 ) {
		  mpz_abs(result->value.integer, arg1->value.integer);
		}
		else {
		  rmid = g95_get_expr();
        	  mpz_init(rmid->value.integer);
		  mpz_abs(rmid->value.integer, arg1->value.integer);
		  mpz_neg(result->value.integer, rmid->value.integer);
		  g95_free_expr(rmid);
		}
		break;
	case BT_REAL:
        	mpf_init(result->value.real);
    		if ( g95_compare_expr(arg2, real_zero) > 0 ) {
		  mpf_abs(result->value.real, arg1->value.real);
		}
		else if ( g95_compare_expr(arg2, real_zero) < 0 ) {
		  rmid = g95_get_expr();
        	  mpf_init(rmid->value.real);
		  mpf_abs(rmid->value.real, arg1->value.real);
		  mpf_neg(result->value.real, rmid->value.real);
		  g95_free_expr(rmid);
		}
		else
		  /* This is not making a distinction between pos and neg zero*/
		  mpf_init(result->value.real);
		break;
	  default:
	    ;;
	    //		return FAILURE;
  }

  g95_replace_expr(e, result);

  //  return SUCCESS;
}


/* simplify_spacing */
g95_expr *g95_simplify_spacing(g95_expr *e) {
g95_expr *arg;

  return NULL; 

/* Type checking */

  arg = FIRST_ARG(e);

  if (arg->ts.type != BT_REAL) {
    g95_warning("Argument of SPACING at %L must be real",
		&FIRST_ARG(e)->where);
    //    return FAILURE;
  }

  //  if (arg->expr_type != EXPR_CONSTANT) return FAILURE;

  //  return SUCCESS;

}



g95_expr *g95_simplify_sqrt(g95_expr *e) {
g95_expr *arg, *result;

  return NULL; 

  arg = FIRST_ARG(e);

  //  if (arg->expr_type != EXPR_CONSTANT) return FAILURE;

/* Type checking */

  switch (arg->ts.type) {
  	case BT_REAL:
	  if (g95_compare_expr(arg, real_zero) == -1) {
	    g95_warning("Argument of SQRT at %L has a negative value",
			&FIRST_ARG(e)->where);
	    //	    return FAILURE;
	  }
	  else {
	    result = g95_copy_expr(arg);
            mpf_sqrt(result->value.real, arg->value.real);
            g95_replace_expr(e,result);
	    //            return SUCCESS;
	  }
	  break;

	case BT_COMPLEX:
	  /* Need to compute principle value */
	  break;

        default:
	    g95_warning("Argument of SQRT at %L must be real or complex",
			&FIRST_ARG(e)->where);
	    //	    return FAILURE;
  }

/* If we reach here something went wrong */
  //  return FAILURE;

}


g95_expr *g95_simplify_verify(g95_expr *e) {
g95_expr *arg1, *arg2;
int knd1, knd2;

  return NULL; 

/* Takes optional argument, not implemented */

/* Type checking */

  arg1 = FIRST_ARG(e);
  arg2 = SECOND_ARG(e);

  if (arg1->ts.type != BT_CHARACTER || arg2->ts.type != BT_CHARACTER ) {
    g95_warning("Arguments of VERIFY at %L must be character",
		&FIRST_ARG(e)->where);
    //    return FAILURE;
  }

  knd1 = g95_validate_kind(BT_INTEGER, arg1->ts.type);
  knd2 = g95_validate_kind(BT_INTEGER, arg2->ts.type);

  if (knd1 != knd2 ) {
    g95_warning("Kind of arguments of VERIFY at %L must agree",
                 &FIRST_ARG(e)->where);
    //    return FAILURE;
  }

  //  if (arg1->expr_type != EXPR_CONSTANT || arg2->expr_type != EXPR_CONSTANT)     return FAILURE;

  //  return SUCCESS;
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
  mpf_init_set_str(mpf_one, "0.5", 10);
}


void g95_simplify_done_1(void) {

  mpf_clear(mpf_pi);
  mpf_clear(mpf_hpi);
  mpf_clear(mpf_nhpi);

  mpf_clear(mpf_zero);
  mpf_clear(mpf_half);
  mpf_clear(mpf_one);

  g95_free_expr(integer_zero);
  g95_free_expr(real_zero);
}
