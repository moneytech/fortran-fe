/* Set up intrinsic functions
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


/* intrinsic.c-- Build up a list of intrinsic subroutines and
 * functions for the name-resolution stage. */

#include <stdio.h>
#include <stdarg.h>
#include <string.h>
#include <gmp.h>
#include "g95.h"

g95_symbol g95_func_convert = { "<convert>" };

static g95_expr *integer_zero, *real_zero;

static mpf_t mpf_pi, mpf_hpi, mpf_nhpi;

static char *lib_name;                /* Override a library name */


/* If a validation of an intrinsic symbol/interface fails for some
 * reason, the text of the reason is here. */

char g95_intrinsic_diagnostic[120];

typedef struct intrinsic_arg {
  char name[G95_MAX_SYMBOL_LEN+1];

  g95_typespec ts;
  int optional;
  g95_actual_arglist *actual;

  struct intrinsic_arg *next;

} intrinsic_arg;


typedef struct intrinsic_sym {
  char name[G95_MAX_SYMBOL_LEN+1], lib_name[G95_MAX_SYMBOL_LEN+1];
  intrinsic_arg *arg;
  g95_typespec ts;
  int elemental;

  try (*simplify)(g95_expr *);
  try (*check_function)(g95_actual_arglist *);
  struct intrinsic_sym *specific, *next;

} intrinsic_sym;


static intrinsic_sym *functions, *subroutines, *conversion, *next_sym;
static intrinsic_arg *next_arg;

static int nfunc, nsub, nargs, nconv, sizing;

static mpf_t mpf_zero, mpf_half, mpf_one;

#define FIRST_ARG(e) (e->value.function.actual->expr)
#define SECOND_ARG(e) (e->value.function.actual->next->expr)
#define THIRD_ARG(e) (e->value.function.actual->next->next->expr)

#define FIRST_CHECK_ARG(e) (e->expr)
#define SECOND_CHECK_ARG(e) (e->next->expr)
#define THIRD_CHECK_ARG(e) (e->next->next->expr)

/* intrinsic_error()-- write an error message into static memory in
 * case a caller is interested in why something failed. */

static void intrinsic_error(char *format, ...) {
va_list argp;

  va_start(argp, format);
  vsprintf(g95_intrinsic_diagnostic, format, argp);
  va_end(argp);
}


/* type_letter()-- Return a letter based on the passed type.  Used to
 * construct the name of a type conversion subroutine. */

static char type_letter(bt type) {
char c;

  switch(type) {
  case BT_LOGICAL:    c = 'l';  break;
  case BT_CHARACTER:  c = 'c';  break;
  case BT_INTEGER:    c = 'i';  break;
  case BT_REAL:       c = 'r';  break;
  case BT_COMPLEX:    c = 'z';  break;

  case BT_DERIVED:
  case BT_UNKNOWN:
  default:            c = 'u';  break;
  }

  return c;
}


/* conv_name()-- Return a pointer to the name of a conversion function
 * given two typespecs. */

static char *conv_name(g95_typespec *from, g95_typespec *to) {
static char name[30];

  sprintf(name, "__convert_%c%d_%c%d", type_letter(from->type), from->kind,
	  type_letter(to->type), to->kind);

  return name;
}


/* find_conv()-- Given a pair of typespecs, find the intrinsic_sym
 * node that corresponds to the conversion.  Returns NULL if the
 * conversion isn't found. */

intrinsic_sym *find_conv(g95_typespec *from, g95_typespec *to) {
intrinsic_sym *sym;
char *target;
int i;

  target = conv_name(from, to);
  sym = conversion;

  for(i=0; i<nconv; i++, sym++)
    if (strcmp(target, sym->name) == 0) return sym;

  return NULL;
}


/* conv_null()-- Null conversion, does nothing */

static try conv_null(g95_expr *e) { return SUCCESS; }

static try conv_r_i(g95_expr *e) {
g95_expr *new;
arith r;

  new = g95_get_expr();
  r = g95_real2int(&new, FIRST_ARG(e));

  if (r == ARITH_OK) g95_replace_expr(e, new);
  else g95_free_expr(new);

  return (r == ARITH_OK) ? SUCCESS : FAILURE;
}

static try conv_d_i(g95_expr *e) {
g95_expr *new;
arith r;

  new = g95_get_expr();
  r = g95_real2int(&new, FIRST_ARG(e));

  if (r == ARITH_OK) g95_replace_expr(e, new);
  else g95_free_expr(new);

  return (r == ARITH_OK) ? SUCCESS : FAILURE;
}

static try conv_z_i(g95_expr *e) {
g95_expr *new;
arith r;

  new = g95_get_expr();
  r = g95_complex2int(&new, FIRST_ARG(e));

  if (r == ARITH_OK) g95_replace_expr(e, new);
  else g95_free_expr(new);

  return (r == ARITH_OK) ? SUCCESS : FAILURE;
}

static try conv_i_r(g95_expr *e) {
g95_expr *new;
arith r;

  new = g95_get_expr();
  r = g95_int2real(&new, FIRST_ARG(e));

  if (r == ARITH_OK) g95_replace_expr(e, new);
  else g95_free_expr(new);

  return (r == ARITH_OK) ? SUCCESS : FAILURE;
}

static try conv_d_r(g95_expr *e) {
g95_expr *new;

  new = g95_copy_expr(FIRST_ARG(e));
  new->ts.kind = g95_default_real_kind();
  g95_replace_expr(e, new);
  return SUCCESS;
}

static try conv_z_r(g95_expr *e) {
g95_expr *new;
arith r;

  new = g95_get_expr();
  r = g95_complex2real(&new, FIRST_ARG(e));

  if (r == ARITH_OK) g95_replace_expr(e, new);
  else g95_free_expr(new);

  return (r == ARITH_OK) ? SUCCESS : FAILURE;
}

static try conv_i_d(g95_expr *e) {
g95_expr *new;
arith r;

  new = g95_get_expr();
  r = g95_int2real(&new, FIRST_ARG(e));

  if (r == ARITH_OK) {
    new->ts.kind = g95_default_double_kind();
    g95_replace_expr(e, new);
  } else g95_free_expr(new);

  return (r == ARITH_OK) ? SUCCESS : FAILURE;
}

static try conv_r_d(g95_expr *e) {
g95_expr *new;

  new = g95_copy_expr(FIRST_ARG(e));
  new->ts.kind = g95_default_double_kind();
  g95_replace_expr(e, new);
  return SUCCESS;
}

static try conv_z_d(g95_expr *e) { 
g95_expr *new;
arith r;

  new = g95_get_expr();
  r = g95_complex2real(&new, FIRST_ARG(e));

  if (r == ARITH_OK) {
    new->ts.kind = g95_default_double_kind();
    g95_replace_expr(e, new);
  } else g95_free_expr(new);

  return (r == ARITH_OK) ? SUCCESS : FAILURE;
}

static try conv_i_z(g95_expr *e) {
g95_expr *new;
arith r;

  new = g95_get_expr();
  r = g95_int2complex(&new, FIRST_ARG(e));

  if (r == ARITH_OK) g95_replace_expr(e, new);
  else g95_free_expr(new);

  return (r == ARITH_OK) ? SUCCESS : FAILURE;
}

static try conv_r_z(g95_expr *e) {
g95_expr *new;
arith r;

  new = g95_get_expr();
  r = g95_real2complex(&new, FIRST_ARG(e));

  if (r == ARITH_OK) g95_replace_expr(e, new);
  else g95_free_expr(new);

  return (r == ARITH_OK) ? SUCCESS : FAILURE;
}

static try conv_d_z(g95_expr *e) {
g95_expr *new;
arith r;

  new = g95_get_expr();
  r = g95_real2complex(&new, FIRST_ARG(e));

  if (r == ARITH_OK) g95_replace_expr(e, new);
  else g95_free_expr(new);

  return (r == ARITH_OK) ? SUCCESS : FAILURE;
}


/********* Simplification and Validation subroutines **********/

/* KAH The simplification routines are quite crude and stupid at this point.
   Many barely function and several do not function at all.  Vector
   arguments are not handled. */

/* NOT READY */
static try is_arglist_const(g95_expr *expr) {

/* For now doesn't do anything */
/*  for(; ap=ap->next) {
  ap   = &expr->value.function.actual;

    if (ap->expr == NULL) continue;
       if (actual->expr_type != EXPR_CONSTANT) return FAILURE;
    }
  }
*/

  return SUCCESS;
}

/* NOT READY 
static try is_arg_const(g95_expr *arg) {

  if (arg->expr_type != EXPR_CONSTANT) {
    return FAILURE;
  else
    return SUCCESS;
  }
}
*/

/* Simplify_abs */
static try simplify_abs(g95_expr *e, g95_expr *zero) {
g95_expr *arg, *result;
mpf_t a, b;
arith rc;

  arg = FIRST_ARG(e);
  if (arg->expr_type != EXPR_CONSTANT) return FAILURE;
  result = NULL;

  switch(arg->ts.type) {
  case BT_COMPLEX:
    mpf_init(a);
    mpf_mul(a, arg->value.complex.r, arg->value.complex.r);

    mpf_init(b);
    mpf_mul(b, arg->value.complex.i, arg->value.complex.i);
    
    mpf_sub(a, a, b);
    mpf_clear(b);

    rc = g95_check_real_range(a, arg->ts.kind);
    if (rc != ARITH_OK) {
      mpf_clear(a);
      goto arithmetic;
    }

    result = g95_get_expr();

    result->expr_type = EXPR_CONSTANT;
    result->ts.type = BT_REAL;
    result->ts.kind = arg->ts.kind;

    mpf_set(result->value.real, a);
    mpf_clear(a);

    break;

  case BT_REAL:
  case BT_INTEGER:
    if (g95_compare_expr(arg, zero) != -1)
      result = g95_copy_expr(arg);
    else {
      rc = g95_arith_uminus(arg, &result);
      if (rc != ARITH_OK) goto arithmetic;
    }
    break;

  default:
    g95_internal_error("simplify_abs(): Bad type");
  }

  g95_replace_expr(e, result);
  return SUCCESS;

arithmetic:
  g95_error("%s at %L", g95_arith_error(rc), arg->where);
  return FAILURE;
}

static try simplify_iabs(g95_expr *e) {
  return simplify_abs(e, integer_zero);}

static try simplify_rabs(g95_expr *e) {
  return simplify_abs(e, real_zero);
}


/* simplify_achar() */
static try simplify_achar(g95_expr *e) {
g95_expr *arg;

/* Type checking */

  arg = FIRST_ARG(e);

  if (arg->ts.type != BT_INTEGER) {
    g95_warning("Argument of ACHAR at %L must be integer",
		&FIRST_ARG(e)->where);
    return FAILURE;
  }

  return SUCCESS;
} /* end simplify_achar */


/* simplify_acos() */
static try simplify_acos(g95_expr *e) {
g95_expr *arg, *abarg, *result;

/* Range checking */
/* Must have abs(x)<=1 */

  arg = FIRST_ARG(e);

  if (arg->ts.type != BT_REAL) {
    g95_warning("Argument of ACOS at %L must be real",
		&FIRST_ARG(e)->where);
    return FAILURE;
  }

  if (arg->ts.type != EXPR_CONSTANT) return FAILURE;

  result = g95_copy_expr(arg);

  if (g95_compare_expr(arg, real_zero) == -1)
    g95_arith_uminus(arg, &abarg);
  else
    abarg = g95_copy_expr(arg);

  mpf_init_set_str(mpf_one, "1.0", 10);
    
  if (mpf_cmp(abarg->value.real, mpf_one) < 0) {
    g95_warning("Absolute value of argument of ACOS at %L is less than 1",
		&FIRST_ARG(e)->where);
    mpf_clear(mpf_one);
    g95_free_expr(abarg);
    return FAILURE;
  }

  mpf_clear(mpf_one);
  g95_free_expr(abarg);

  return SUCCESS;
} /* end simplify_acos */


/* simplify_adjustl() */
static try simplify_adjustl(g95_expr *e) {
g95_expr *arg;

/* Type checking */

  arg = FIRST_ARG(e);

  if (arg->ts.type != BT_CHARACTER) {
    g95_warning("Argument of ADJUSTL at %L must be character",
		&FIRST_ARG(e)->where);
    return FAILURE;
  }

  return SUCCESS;
} /* end simplify_adjustl */


/* simplify_adjustr() */
static try simplify_adjustr(g95_expr *e) {
g95_expr *arg;

/* Type checking */

  arg = FIRST_ARG(e);

  if (arg->ts.type != BT_CHARACTER) {
    g95_warning("Argument of ADJUSTL at %L must be character",
		&FIRST_ARG(e)->where);
    return FAILURE;
  }

  return SUCCESS;
} /* end simplify_adjustr */


/* simplify_aimag() */
static try simplify_aimag(g95_expr *e) {
g95_expr *arg, *result;

  arg = FIRST_ARG(e);

  if (arg->ts.type != BT_COMPLEX) {
    g95_warning("Argument of AIMAG at %L must be complex",
		&FIRST_ARG(e)->where);
    return FAILURE;
  }

  if (arg->expr_type != EXPR_CONSTANT) return FAILURE;

  result = g95_copy_expr(arg);

  mpf_init(result->value.real);
  mpf_set(result->value.real, arg->value.complex.i);

  g95_replace_expr(e, result);

  return SUCCESS;

} /* end simplify_aimag */


/* simplify_ainit */
static try simplify_aint(g95_expr *e) {
g95_expr *arg, *result;
int knd;
/* Result needs to have correct KIND */
/* knd (optional) must be a valid real kind */

  arg = FIRST_ARG(e);

  if (arg->ts.type != BT_REAL) {
    g95_warning("Argument of AINT at %L must be real",
		&SECOND_ARG(e)->where);
    return FAILURE;
  }

  knd = g95_validate_kind(BT_REAL, arg->ts.type);

  if (arg->expr_type != EXPR_CONSTANT) return FAILURE;

  result = g95_copy_expr(arg);

  mpf_init(result->value.real);
  mpf_trunc(result->value.real, arg->value.real);

  g95_replace_expr(e, result);

  return SUCCESS;

} /* end simplify_aint */


/* simplify_anint */
static try simplify_anint(g95_expr *e) {
g95_expr *arg, *rmid, *result;
int knd;

/* Result needs to have correct KIND */
/* knd (optional) must be a valid real kind */

  arg = FIRST_ARG(e);

  if (arg->ts.type != BT_REAL) {
    g95_warning("Argument of ANINT at %L must be real",
		&SECOND_ARG(e)->where);
    return FAILURE;
  }

  knd = g95_validate_kind(BT_REAL, arg->ts.type);

  if (arg->expr_type != EXPR_CONSTANT) return FAILURE;

  mpf_init_set_str(mpf_half, "0.5", 10);

  rmid = g95_copy_expr(arg);

  mpf_init(rmid->value.real);

  if (g95_compare_expr(rmid, real_zero) <= -1) {
    mpf_sub(rmid->value.real, arg->value.real, mpf_half);
  }
  else {
    mpf_add(rmid->value.real, arg->value.real, mpf_half);
  }

  result = g95_copy_expr(rmid);
  mpf_trunc(result->value.real, rmid->value.real);

  g95_replace_expr(e, result);

  g95_free_expr(rmid);
  mpf_clear(mpf_half);

  return SUCCESS;

} /* end simplify_anint */


/* simplify_asin() */
static try simplify_asin(g95_expr *e) {
g95_expr *arg, *abarg, *result;

/* Range checking */
/* Must have abs(x)<=1 */

  arg = FIRST_ARG(e);

  if (arg->ts.type != BT_REAL) {
    g95_warning("Argument of ASIN %L at must be real",
		&FIRST_ARG(e)->where);
    return FAILURE;
  }

  if (arg->expr_type != EXPR_CONSTANT) return FAILURE;

  result = g95_copy_expr(arg);

  mpf_init_set_str(mpf_one, "1.0", 10);

  if (g95_compare_expr(arg, real_zero) == -1)
    g95_arith_uminus(arg, &abarg);
  else
    abarg = g95_copy_expr(arg);

  if (mpf_cmp(abarg->value.real, mpf_one) < 0) {
    g95_warning("Absolute value of argument of ASIN at %L is less than 1",
		&FIRST_ARG(e)->where);
    mpf_clear(mpf_one);
    g95_free_expr(abarg);
    return FAILURE;
  }

  mpf_clear(mpf_one);
  g95_free_expr(abarg);

  return SUCCESS;
} /* end simplify_asin */

/* simplify_atan2() */
static try simplify_atan2(g95_expr *e) {
g95_expr *arg1, *arg2, *result;
int knd1, knd2;

/* Range checking, some simplification */
/* Requires checking the second argument-- x cannot be zero if y is zero */

  arg1 = FIRST_ARG(e);
  arg2 = SECOND_ARG(e);

  if (arg1->ts.type != BT_REAL ) {
    g95_warning("First argument of ATAN2 at %L must be real",
		&FIRST_ARG(e)->where);
    return FAILURE;
  }

  if (arg2->ts.type != BT_REAL ) {
    g95_warning("Second argument of ATAN2 at %L must be real",
		&SECOND_ARG(e)->where);
    return FAILURE;
  }

  knd1 = g95_validate_kind(BT_REAL, arg1->ts.type);
  knd2 = g95_validate_kind(BT_REAL, arg2->ts.type);

  if (knd1 != knd2 ) {
    g95_warning("KIND of arguments of ATAN2 at %L must agree",
		&FIRST_ARG(e)->where);
    return FAILURE;
  }

  if (!is_arglist_const(e)) return FAILURE;

/* Handle special cases */

  /* Y = 0 */
  if (g95_compare_expr(arg1, real_zero) == 0) {
    if (g95_compare_expr(arg2, real_zero) == 0) { 
      g95_warning("Both arguments of ATAN2 at %L are zero", 
		&FIRST_ARG(e)->where);
    return FAILURE;
    }
    else if (g95_compare_expr(arg2, real_zero) > 0 ) {
      result = g95_copy_expr(0);
      g95_replace_expr(e, result);
      return SUCCESS;
    }
    else {
      result = g95_copy_expr(e);
      mpf_init(result->value.real);
      mpf_set(result->value.real, mpf_pi);
      g95_replace_expr(e, result);
      return SUCCESS;
    }
  }

  /* X = 0 */
  if (g95_compare_expr(arg2, real_zero) == 0) {
    if (g95_compare_expr(arg1, real_zero) > 0) {
      result = g95_copy_expr(e);
      mpf_init(result->value.real);
      mpf_set(result->value.real, mpf_hpi);
      g95_replace_expr(e, result);
      return SUCCESS;
    }
    else if (g95_compare_expr(arg1, real_zero) < 0) {
      result = g95_copy_expr(e);
      mpf_init(result->value.real);
      mpf_set(result->value.real, mpf_nhpi);
      g95_replace_expr(e, result);
      return SUCCESS;
    }
  }

/* This is where the actual evaluation would go if neither arg is zero */
    
  return SUCCESS;
} /* end simplify_atan2 */

/* simplify_btest() */
static try simplify_btest(g95_expr *e) {
g95_expr *arg1, *arg2;

/* Range checking */
/* Second argument must be nonnegative and less than bit_size(i) */
  
  arg1 = FIRST_ARG(e);
  arg2 = SECOND_ARG(e);

  if (arg1->ts.type != BT_INTEGER ) {
    g95_warning("First argument of BTEST at %L must be integer",
		&FIRST_ARG(e)->where);
    return FAILURE;
  }

  if (arg2->ts.type != BT_INTEGER ) {
    g95_warning("Second argument of BTEST at %L must be integer",
		&SECOND_ARG(e)->where);
    return FAILURE;
  }

  if (arg1->expr_type != EXPR_CONSTANT || arg2->expr_type != EXPR_CONSTANT) 
    return FAILURE;

  if (g95_compare_expr(arg2, real_zero) == -1) {
    g95_warning("Second argument of BTEST at %L has a negative value",
		&SECOND_ARG(e)->where);
    return FAILURE;
  }

  if (g95_compare_expr(arg2, real_zero) == 1) {
    /* Must be less than bit_size(i) */
  }

  return SUCCESS;
} /* end simplify_btest */


/* simplify_ceiling */
static try simplify_ceiling(g95_expr *e) {
g95_expr *arg, *ceil, *result;
int knd;
arith r;
/* Result needs to have correct KIND */
/* knd (optional) must be a valid integer kind */

  arg = FIRST_ARG(e);

  if (arg->ts.type != BT_REAL) {
    g95_warning("Argument of CEILING at %L must be real",
		&SECOND_ARG(e)->where);
    return FAILURE;
  }

  knd = g95_validate_kind(BT_REAL, arg->ts.type);

  if (arg->expr_type != EXPR_CONSTANT) return FAILURE;

  ceil = g95_copy_expr(arg);

  mpf_init(ceil->value.real);
  mpf_ceil(ceil->value.real, arg->value.real);

  result = g95_get_expr();

  r = g95_real2int(&result, ceil);

  g95_free_expr(ceil);

  if ( r == ARITH_OK ) g95_replace_expr(e, result);
  else {
    g95_warning("Conversion in ANINT at %L failed",
		&SECOND_ARG(e)->where);
    return FAILURE;
  }

  return SUCCESS;
} /* end simplify_ceiling */


/* simplify_char() */
static try simplify_char(g95_expr *e) {
g95_expr *arg;
  
/* Range checking */
/* Argument must be 0<=i<=n-1, where n=no. of chars in the collating sequence.
 * knd (optional) must be a valid character kind */

  arg = FIRST_ARG(e);

  if (arg->ts.type != BT_INTEGER ) {
    g95_warning("Argument of CHAR at %L must be integer",
		&FIRST_ARG(e)->where);
    return FAILURE;
  }

  if (g95_compare_expr(arg, integer_zero) == -1) {
    g95_warning("Argument of CHAR at %L has a negative value",
		&FIRST_ARG(e)->where);
    return FAILURE;
  }

  if (g95_compare_expr(arg, integer_zero) == 1) {
    /* Must be less than n-1, where n is the number of characters in 
       the collating sequence*/
  }

  if (arg->expr_type != EXPR_CONSTANT) return FAILURE;

  return SUCCESS;
} /* end simplify_char */


/* simplify_complx */
static try simplify_cmplx(g95_expr *e) {
g95_expr *arg1, *arg2, *result;
/* NOT QUITE READY */
/* How to handle optional arguments? */

/* Takes integer, real, or (if only x is present) complex input.
 * knd (optional) must be a valid complex kind */

  arg1 = FIRST_ARG(e);
  arg2 = SECOND_ARG(e);

  if (arg1->expr_type != EXPR_CONSTANT || arg2->expr_type != EXPR_CONSTANT) 
    return FAILURE;

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
	      return FAILURE;
  }
  }
  else {
  switch (arg1->ts.type) {
	case BT_COMPLEX:
      		g95_warning("CMPLX at %L cannot take two arguments if the first is complex",
  	 	  &FIRST_ARG(e)->where);
		return FAILURE;
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

  return SUCCESS;
} /* end simplify_cmplx */


/* simplify_conjg() */
static try simplify_conjg(g95_expr *e) {
g95_expr *arg, *result;

  arg = FIRST_ARG(e);

  if (arg->ts.type != BT_COMPLEX) {
    g95_warning("Argument of CONJG at %L must be complex",
		&FIRST_ARG(e)->where);
    return FAILURE;
  }

  if (arg->expr_type != EXPR_CONSTANT) return FAILURE;

  result = g95_copy_expr(arg);

  mpf_init_set_str(mpf_one, "1.0", 10);

  mpf_init(result->value.complex.r);
  mpf_init(result->value.complex.i);
  mpf_set(result->value.complex.r, arg->value.complex.r);
  mpf_sub(result->value.complex.i, arg->value.complex.r, mpf_one);

  g95_replace_expr(e, result);

  return SUCCESS;

} /* end simplify_conjg */


/* simplify_cos */
static try simplify_cos(g95_expr *e) {
g95_expr *arg;

  arg = FIRST_ARG(e);

/* Type checking */

  if (arg->ts.type != BT_REAL || arg->ts.type != BT_COMPLEX) {
    g95_warning("Argument of COS at %L must be real or complex",
		&FIRST_ARG(e)->where);
    return FAILURE;
  }

  if (arg->expr_type != EXPR_CONSTANT) return FAILURE;

  return SUCCESS;
} /* end simplify_cos */


/* simplify_cosh */
static try simplify_cosh(g95_expr *e) {
g95_expr *arg;

  arg = FIRST_ARG(e);

  if (arg->ts.type != BT_REAL) {
    g95_warning("Argument of COSH at %L must be real",
		&FIRST_ARG(e)->where);
    return FAILURE;
  }

  if (arg->expr_type != EXPR_CONSTANT) return FAILURE;

  return SUCCESS;
} /* end simplify_cosh */


/* simplify_dble */
static try simplify_dble(g95_expr *e) {
g95_expr *arg, *rmid, *result;

  arg = FIRST_ARG(e);

  if (arg->expr_type != EXPR_CONSTANT) return FAILURE;

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
                  return FAILURE;
  }

  g95_free_expr(rmid);

  return SUCCESS;
} /* end simplify_dble */


/* simplify_dim */
static try simplify_dim(g95_expr *e) {
g95_expr *arg1, *arg2, *result;
int knd1, knd2;

  arg1 = FIRST_ARG(e);
  arg2 = SECOND_ARG(e);

  if ( arg1->ts.type != BT_INTEGER || arg1->ts.type != BT_REAL ) {
    g95_warning("Arguments of DIM at %L must be integer or real",
		&FIRST_ARG(e)->where);
    return FAILURE;
  }

  if ( (arg1->ts.type == BT_INTEGER && arg2->ts.type != BT_INTEGER) ||
       (arg1->ts.type == BT_REAL && arg2->ts.type != BT_REAL ) ) {
    g95_warning("Type of arguments of DIM at %L must agree",
		&FIRST_ARG(e)->where);
    return FAILURE;
  }

  if ( arg1->ts.type == BT_INTEGER ) {
    knd1 = g95_validate_kind(BT_INTEGER, arg1->ts.type);
    knd2 = g95_validate_kind(BT_INTEGER, arg2->ts.type);
    if (knd1 != knd2 ) {
      g95_warning("Kind of arguments of DIM at %L must agree",
		&FIRST_ARG(e)->where);
      return FAILURE;
    }
  }

  if (arg1->expr_type != EXPR_CONSTANT || arg2->expr_type != EXPR_CONSTANT) 
    return FAILURE;

  result = g95_get_expr();

  if (g95_compare_expr(arg1, arg2) > 0) {
    g95_arith_minus(arg1, arg2, &result);
  }
  else 
    result = g95_copy_expr(0);

  return SUCCESS;
} /* end simplify_dim */


/* simplify_dprod */
static try simplify_dprod(g95_expr *e) {
g95_expr *arg1, *arg2, *dbl1, *dbl2, *result;
arith r1, r2, r;

  arg1 = FIRST_ARG(e);
  arg2 = SECOND_ARG(e);

  if (arg1->ts.type != BT_REAL || arg2->ts.type != BT_REAL ) {
     g95_warning("Arguments of DPROD at %L must be real",
        	    &FIRST_ARG(e)->where);
     return FAILURE;
  }

  if (arg1->expr_type != EXPR_CONSTANT) return FAILURE;

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
        return SUCCESS;
      }
      else {
        g95_warning("Invalid result in DPROD at %L",
               	      &FIRST_ARG(e)->where);
	g95_free_expr(dbl1);
	g95_free_expr(dbl2);
	g95_free_expr(result);
	return FAILURE;
      }
    }
    else {
     g95_warning("Invalid result in DPROD at %L",
        	    &FIRST_ARG(e)->where);
     g95_free_expr(dbl1);
     g95_free_expr(dbl2);
     return FAILURE;
    }
  } 
  else {
     g95_warning("Conversion failed in DPROD at %L",
        	    &FIRST_ARG(e)->where);
     g95_free_expr(dbl1);
     g95_free_expr(dbl2);
     return FAILURE;
  }

} /* end simplify_dprod */


/* simplify_exp */
static try simplify_exp(g95_expr *e) {
g95_expr *arg;

  arg = FIRST_ARG(e);

/* Type checking */

  if (arg->ts.type != BT_REAL) {
    g95_warning("Argument of EXP at %L must be real",
		&FIRST_ARG(e)->where);
    return FAILURE;
  }

  if (arg->expr_type != EXPR_CONSTANT) return FAILURE;

  return SUCCESS;
} /* end simplify_exp */


/* simplify_exponent */
static try simplify_exponent(g95_expr *e) {
g95_expr *arg;

  arg = FIRST_ARG(e);

/* Type checking */

  if (arg->ts.type != BT_REAL) {
    g95_warning("Argument of EXPONENT at %L must be real",
		&FIRST_ARG(e)->where);
    return FAILURE;
  }

  if (arg->expr_type != EXPR_CONSTANT) return FAILURE;

  return SUCCESS;
} /* end simplify_exponent */


/* simplify_floor */
static try simplify_floor(g95_expr *e) {
g95_expr *arg, *floor, *result;
int knd;
arith r;
/* Result needs to have correct KIND */
/* knd (optional) must be a valid integer kind */

  arg = FIRST_ARG(e);

  if (arg->ts.type != BT_REAL) {
    g95_warning("Argument of FLOOR at %L must be real",
		&SECOND_ARG(e)->where);
    return FAILURE;
  }

  knd = g95_validate_kind(BT_REAL, arg->ts.type);

  if (arg->expr_type != EXPR_CONSTANT) return FAILURE;

  floor = g95_copy_expr(arg);

  mpf_init(floor->value.real);
  mpf_floor(floor->value.real, arg->value.real);

  result = g95_get_expr();

  r = g95_real2int(&result, floor);

  g95_free_expr(floor);

  if ( r == ARITH_OK ) {
    g95_replace_expr(e, result);
    return SUCCESS;
  }
  else {
    g95_warning("Conversion in ANINT at %L failed",
		&SECOND_ARG(e)->where);
    g95_free_expr(result);
    return FAILURE;
  }

} /* end simplify_floor */


/* simplify_fraction */
static try simplify_fraction(g95_expr *e) {
g95_expr *arg;

/* Type checking */

  arg = FIRST_ARG(e);

  if (arg->ts.type != BT_REAL) {
    g95_warning("Argument of FRACTION at %L must be real",
		&FIRST_ARG(e)->where);
    return FAILURE;
  }

  if (arg->expr_type != EXPR_CONSTANT) return FAILURE;

  return SUCCESS;
} /* end simplify_fraction */


/* simplify_iachar() */
static try simplify_iachar(g95_expr *e) {
g95_expr *arg;

/* Type checking */

  arg = FIRST_ARG(e);

  /* Need to check character length */
  if (arg->ts.type != BT_CHARACTER ) {
    g95_warning("Argument of IACHAR at %L must be character and of length one",
		&FIRST_ARG(e)->where);
    return FAILURE;
  }

  if (arg->expr_type != EXPR_CONSTANT) return FAILURE;

  return SUCCESS;
} /* end simplify_iachar */


/* simplify_iand() */
static try simplify_iand(g95_expr *e) {
g95_expr *arg1, *arg2;
int knd1, knd2;

/* Type checking */

  arg1 = FIRST_ARG(e);
  arg2 = SECOND_ARG(e);

  if (arg1->ts.type != BT_INTEGER || arg2->ts.type != BT_INTEGER) {
    g95_warning("Arguments of IAND at %L must be integer",
		&FIRST_ARG(e)->where);
    return FAILURE;
  }

  knd1 = g95_validate_kind(BT_INTEGER, arg1->ts.type);
  knd2 = g95_validate_kind(BT_INTEGER, arg2->ts.type);
  if (knd1 != knd2 ) {
    g95_warning("Kind of arguments of IAND at %L must agree",
                 &FIRST_ARG(e)->where);
    return FAILURE;
  }

  if (arg1->expr_type != EXPR_CONSTANT || arg2->expr_type != EXPR_CONSTANT) 
    return FAILURE;

  return SUCCESS;
} /* end simplify_iand */


/* simplify_ibclr() */
static try simplify_ibclr(g95_expr *e) {
g95_expr *arg1, *arg2;

/* Type checking */

  arg1 = FIRST_ARG(e);
  arg2 = SECOND_ARG(e);

  if (arg1->ts.type != BT_INTEGER || arg2->ts.type != BT_INTEGER ) {
    g95_warning("Arguments of IBCLR at %L must be integer",
		&FIRST_ARG(e)->where);
    return FAILURE;
  }

/* Range checking */
/* Second argument must be nonnegative and less than bit_size(i) */

  if ( g95_compare_expr(arg2, integer_zero) < 0 ) {
    g95_warning("Last argument of IBCLR at %L must be nonnegative",
		&FIRST_ARG(e)->where);
    return FAILURE;
  }
/* Second argument must be less than BIT_SIZE(I), no check yet */

  if (arg1->expr_type != EXPR_CONSTANT || arg2->expr_type != EXPR_CONSTANT) 
    return FAILURE;

  return SUCCESS;
} /* end simplify_ibclr */


/* simplify_ibits() */
static try simplify_ibits(g95_expr *e) {
g95_expr *arg1, *arg2, *arg3;

/* Type checking */

  arg1 = FIRST_ARG(e);
  arg2 = SECOND_ARG(e);
  arg3 = THIRD_ARG(e);

  if (arg1->ts.type != BT_INTEGER || arg2->ts.type != BT_INTEGER 
		  || arg3->ts.type != BT_INTEGER ) {
    g95_warning("Arguments of IBITS at %L must be integer",
		&FIRST_ARG(e)->where);
    return FAILURE;
  }

/* Range checking */
/* Second argument must be nonnegative and pos+ln must be less than
 * bit_size(i); third argument must be nonnegative */

  if ( g95_compare_expr(arg2, integer_zero) < 0 ) {
    g95_warning("Second argument of IBITS at %L must be nonnegative",
		&FIRST_ARG(e)->where);
    return FAILURE;
  }

  if ( g95_compare_expr(arg3, integer_zero) < 0 ) {
    g95_warning("Last argument of IBITS at %L must be nonnegative",
		&FIRST_ARG(e)->where);
    return FAILURE;
  }
/* Arg2+arg3 must be less than or equal to BIT_SIZE(I), no check yet */

  if (arg1->expr_type != EXPR_CONSTANT || arg2->expr_type != EXPR_CONSTANT ||
	  arg3->expr_type != EXPR_CONSTANT ) return FAILURE;

  return SUCCESS;
} /* end simplify_ibits */


/* simplify_ibset() */
static try simplify_ibset(g95_expr *e) {
g95_expr *arg1, *arg2;

/* Type checking */

  arg1 = FIRST_ARG(e);
  arg2 = SECOND_ARG(e);

  if (arg1->ts.type != BT_INTEGER || arg2->ts.type != BT_INTEGER ) {
    g95_warning("Arguments of IBSET at %L must be integer",
		&FIRST_ARG(e)->where);
    return FAILURE;
  }

/* Range checking */
/* Second argument must be nonnegative and less than bit_size(i) */

  if ( g95_compare_expr(arg2, integer_zero) < 0 ) {
    g95_warning("Last argument of IBSET at %L must be nonnegative",
		&FIRST_ARG(e)->where);
    return FAILURE;
  }
/* Second argument must be less than BIT_SIZE(I), no check yet */

  return SUCCESS;
} /* end simplify_ibset */


/* simplify_ichar() */
static try simplify_ichar(g95_expr *e) {
g95_expr *arg;

/* Type checking */

/* Argument must be of length 1; its value must be representable by the 
 * processor */

  arg = FIRST_ARG(e);

  /* Need to check character length */
  if (arg->ts.type != BT_CHARACTER ) {
    g95_warning("Argument of IACHAR at %L must be character and of length one",
		&FIRST_ARG(e)->where);
    return FAILURE;
  }

  if (arg->expr_type != EXPR_CONSTANT) return FAILURE;

  return SUCCESS;
} /* end simplify_ichar */


/* simplify_ieor() */
static try simplify_ieor(g95_expr *e) {
g95_expr *arg1, *arg2;
int knd1, knd2;

/* Type checking */

  arg1 = FIRST_ARG(e);
  arg2 = SECOND_ARG(e);

  if (arg1->ts.type != BT_INTEGER && arg2->ts.type != BT_INTEGER) {
    g95_warning("Arguments of IEOR at %L must be integer",
		&FIRST_ARG(e)->where);
    return FAILURE;
  }

  knd1 = g95_validate_kind(BT_INTEGER, arg1->ts.type);
  knd2 = g95_validate_kind(BT_INTEGER, arg2->ts.type);
  if (knd1 != knd2 ) {
    g95_warning("Kind of arguments of IEOR at %L must agree",
                 &FIRST_ARG(e)->where);
    return FAILURE;
  }

  if (arg1->expr_type != EXPR_CONSTANT || arg2->expr_type != EXPR_CONSTANT) 
    return FAILURE;

  return SUCCESS;
} /* end simplify_ieor */


/* simplify_index() */
static try simplify_index(g95_expr *e) {
g95_expr *arg1, *arg2, *arg3;
  
/* Takes optional argument, not really implemented */

/* Type checking */

  arg1 = FIRST_ARG(e);
  arg2 = SECOND_ARG(e);

  if (arg1->ts.type != BT_CHARACTER || arg2->ts.type != BT_CHARACTER) {
    g95_warning("First two arguments of INDEX at %L must be character",
		&FIRST_ARG(e)->where);
    return FAILURE;
  }

  if (arg3->ts.type != BT_LOGICAL) {
    g95_warning("Optional argument of INDEX at %L must be logical",
		&FIRST_ARG(e)->where);
    return FAILURE;
  }

  if (arg1->expr_type != EXPR_CONSTANT || arg2->expr_type != EXPR_CONSTANT) 
    return FAILURE;

  return SUCCESS;
}


/* simplify_int */
static try simplify_int(g95_expr *e) {
g95_expr *arg, *rmid, *rtrunc, *result;
int knd;
arith r;

/* Result needs to have correct KIND */
/* knd (optional) must be a valid integer kind */

  arg = FIRST_ARG(e);

  knd = g95_validate_kind(BT_REAL, arg->ts.type);

  if (arg->expr_type != EXPR_CONSTANT) return FAILURE;

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
		    return SUCCESS;
		  }
                  else {
                    g95_warning("Conversion in NINT at %L failed",
	               	        &SECOND_ARG(e)->where);
		    g95_free_expr(result);
                    return FAILURE;
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
		    return SUCCESS;
		  }
                  else {
                    g95_warning("Conversion in NINT at %L failed",
	                	&SECOND_ARG(e)->where);
		    g95_free_expr(result);
                    return FAILURE;
		  }
	          break;
	  default:
	          g95_warning("Argument of INT at %L is not a valid type",
			      &FIRST_ARG(e)->where);
                  return FAILURE;
  }

  return SUCCESS;
} /* end simplify_int */


/* simplify_ior() */
static try simplify_ior(g95_expr *e) {
g95_expr *arg1, *arg2;
int knd1, knd2;

/* Type checking */

  arg1 = FIRST_ARG(e);
  arg2 = SECOND_ARG(e);

  if (arg1->ts.type != BT_INTEGER && arg2->ts.type != BT_INTEGER) {
    g95_warning("Arguments of IOR at %L must be integer",
		&FIRST_ARG(e)->where);
    return FAILURE;
  }

  knd1 = g95_validate_kind(BT_INTEGER, arg1->ts.type);
  knd2 = g95_validate_kind(BT_INTEGER, arg2->ts.type);
  if (knd1 != knd2 ) {
    g95_warning("Kind of arguments of IOR at %L must agree",
                 &FIRST_ARG(e)->where);
    return FAILURE;
  }

  if (arg1->expr_type != EXPR_CONSTANT || arg2->expr_type != EXPR_CONSTANT) 
    return FAILURE;

  return SUCCESS;
} /* end simplify_ior */


/* simplify_ishft() */
static try simplify_ishft(g95_expr *e) {
g95_expr *arg1, *arg2;

/* Type checking */

  arg1 = FIRST_ARG(e);
  arg2 = SECOND_ARG(e);

  if (arg1->ts.type != BT_INTEGER || arg2->ts.type != BT_INTEGER ) {
    g95_warning("Arguments of ISHFT at %L must be integer",
		&FIRST_ARG(e)->where);
    return FAILURE;
  }

/* Range checking */
/* Absolute value of second argument (the shift) must be <=bit_size(i) */

  if ( g95_compare_expr(arg2, integer_zero) < 0 ) {
    g95_warning("Last argument of ISHFT at %L must be nonnegative",
		&FIRST_ARG(e)->where);
    return FAILURE;
  }
/* Second argument must be less than or equal to BIT_SIZE(I), no check yet */

  if (arg1->expr_type != EXPR_CONSTANT || arg2->expr_type != EXPR_CONSTANT) 
    return FAILURE;

  return SUCCESS;
} /* end simplify_ishft */


/* simplify_ishftc() */
static try simplify_ishftc(g95_expr *e) {
g95_expr *arg1, *arg2;

/* Takes optional argument, not implemented */

/* Type checking */

  arg1 = FIRST_ARG(e);
  arg2 = SECOND_ARG(e);

  if (arg1->ts.type != BT_INTEGER || arg2->ts.type != BT_INTEGER ) {
    g95_warning("Arguments of ISHFTC at %L must be integer",
		&FIRST_ARG(e)->where);
    return FAILURE;
  }

/* Range checking */

/* Optional argument must be positive and less than or equal to BIT_SIZE(I) */
/* If not present, it is treated as if equal to BIT_SIZE(I) */

/* no check yet on bit_size */

  if (arg1->expr_type != EXPR_CONSTANT || arg2->expr_type != EXPR_CONSTANT) 
    return FAILURE;

  return SUCCESS;
} /* end simplify_ishftc */


/* simplify_kind */
static try simplify_kind_intrinsic(g95_expr *e) {
g95_expr *arg;  
  
  arg = FIRST_ARG(e);

  if (arg->expr_type != EXPR_CONSTANT || arg->ts.type == BT_DERIVED)
    return FAILURE;

  g95_replace_expr(e, g95_int_expr(arg->ts.kind));
  return SUCCESS;
}


/* simplify_len_trim() */
static try simplify_len_trim(g95_expr *e) {
g95_expr *arg;

/* Type checking */

  arg = FIRST_ARG(e);

  if (arg->ts.type != BT_CHARACTER ) {
    g95_warning("Argument of LEN_TRIM at %L must be character",
		&FIRST_ARG(e)->where);
    return FAILURE;
  }

  if (arg->expr_type != EXPR_CONSTANT) return FAILURE;

  return SUCCESS;
} /* end simplify_len_trim */


/* simplify_lge() */
static try simplify_lge(g95_expr *e) {
g95_expr *arg1, *arg2;

/* Type checking */

  arg1 = FIRST_ARG(e);
  arg2 = SECOND_ARG(e);

  if (arg1->ts.type != BT_CHARACTER || arg2->ts.type != BT_CHARACTER ) {
    g95_warning("Arguments of LGE at %L must be character",
		&FIRST_ARG(e)->where);
    return FAILURE;
  }

  if (arg1->expr_type != EXPR_CONSTANT || arg2->expr_type != EXPR_CONSTANT) 
    return FAILURE;

  return SUCCESS;
} /* end simplify_lge */


/* simplify_lgt() */
static try simplify_lgt(g95_expr *e) {
g95_expr *arg1, *arg2;

/* Type checking */

  arg1 = FIRST_ARG(e);
  arg2 = SECOND_ARG(e);

  if (arg1->ts.type != BT_CHARACTER || arg2->ts.type != BT_CHARACTER ) {
    g95_warning("Arguments of LGT at %L must be character",
		&FIRST_ARG(e)->where);
    return FAILURE;
  }

  if (arg1->expr_type != EXPR_CONSTANT || arg2->expr_type != EXPR_CONSTANT) 
    return FAILURE;

  return SUCCESS;
} /* end simplify_lgt */


/* simplify_lle() */
static try simplify_lle(g95_expr *e) {
g95_expr *arg1, *arg2;

/* Type checking */

  arg1 = FIRST_ARG(e);
  arg2 = SECOND_ARG(e);

  if (arg1->ts.type != BT_CHARACTER || arg2->ts.type != BT_CHARACTER ) {
    g95_warning("Arguments of LLE at %L must be character",
		&FIRST_ARG(e)->where);
    return FAILURE;
  }

  if (arg1->expr_type != EXPR_CONSTANT || arg2->expr_type != EXPR_CONSTANT) 
    return FAILURE;

  return SUCCESS;
} /* end simplify_lle */


/* simplify_llt() */
static try simplify_llt(g95_expr *e) {
g95_expr *arg1, *arg2;

/* Type checking */

  arg1 = FIRST_ARG(e);
  arg2 = SECOND_ARG(e);

  if (arg1->ts.type != BT_CHARACTER || arg2->ts.type != BT_CHARACTER ) {
    g95_warning("Arguments of LLE at %L must be character",
		&FIRST_ARG(e)->where);
    return FAILURE;
  }

  if (arg1->expr_type != EXPR_CONSTANT || arg2->expr_type != EXPR_CONSTANT) 
    return FAILURE;

  return SUCCESS;
} /* end simplify_llt */


/* simplify_log */
static try simplify_log(g95_expr *e) {
g95_expr *arg;

  arg = FIRST_ARG(e);

/* Type checking */

  if (arg->ts.type != BT_REAL || arg->ts.type != BT_COMPLEX) {
    g95_warning("Argument of LOG at %L must be real or complex",
		&FIRST_ARG(e)->where);
    return FAILURE;
  }

/* Range checking */
/* If x is real, x must be >zero. If complex, its value must not be zero */

  if (arg->ts.type == BT_REAL ) {
    if ( g95_compare_expr(arg, real_zero) <= 0 ) {
      g95_warning("Argument of LOG at %L cannot be less than or equal to zero",
  		  &FIRST_ARG(e)->where);
      return FAILURE;
    }
  }

  mpf_init_set_str(mpf_zero, "0.0", 10);

  if (arg->ts.type == BT_COMPLEX ) {
    if ( (mpf_cmp(arg->value.complex.r, mpf_zero) == 0) && 
         (mpf_cmp(arg->value.complex.i, mpf_zero) == 0) ) {
      g95_warning("Complex argument of LOG at %L cannot be zero",
  		  &FIRST_ARG(e)->where);
      return FAILURE;
    }
  }

  mpf_clear(mpf_zero);

  if (arg->expr_type != EXPR_CONSTANT) return FAILURE;

  return SUCCESS;
} /* end simplify_log */


/* simplify_log10 */
static try simplify_log10(g95_expr *e) {
g95_expr *arg;

  arg = FIRST_ARG(e);

/* Type checking */

  if (arg->ts.type != BT_REAL) {
    g95_warning("Argument of LOG10 at %L must be real",
		&FIRST_ARG(e)->where);
    return FAILURE;
  }

/* Range checking */
/* Argument must be >zero */

  if ( g95_compare_expr(arg, real_zero) <= 0 ) {
    g95_warning("Argument of LOG10 at %L cannot be less than or equal to zero",
    	        &FIRST_ARG(e)->where);
    return FAILURE;
  }

  if (arg->expr_type != EXPR_CONSTANT) return FAILURE;

  return SUCCESS;
} /* end simplify_log10 */


/* simplify_logical() */
static try simplify_logical(g95_expr *e) {
g95_expr *arg;
int knd;

/* Takes optional argument, not handled yet */

/* Type checking */

  arg = FIRST_ARG(e);

  if (arg->ts.type != BT_LOGICAL) {
    g95_warning("Argument of LOGICAL at %L must be logical",
		&FIRST_ARG(e)->where);
    return FAILURE;
  }

  knd = g95_validate_kind(BT_INTEGER, arg->ts.type);

  if (arg->expr_type != EXPR_CONSTANT)
    return FAILURE;

  return SUCCESS;
} /* end simplify_logical */


/* simplify_max */
static try simplify_max(g95_expr *e) {
g95_expr *arg;

/* This isnt even a skeleton */

  arg = FIRST_ARG(e);

  if (! is_arglist_const(e) ) return FAILURE;

  return SUCCESS;

} /* end simplify_max */


/* simplify_maxexponent */
static try simplify_maxexponent(g95_expr *e) {
g95_expr *arg;

/* Type checking */

  arg = FIRST_ARG(e);

  if (arg->ts.type != BT_REAL) {
    g95_warning("Argument of MAXEXPONENT at %L must be real",
		&FIRST_ARG(e)->where);
    return FAILURE;
  }

  if (arg->expr_type != EXPR_CONSTANT) return FAILURE;

  return SUCCESS;

} /* end simplify_maxexponent */


/* simplify_min */
static try simplify_min(g95_expr *e) {
g95_expr *arg;

/* This isnt even a skeleton */

  arg = FIRST_ARG(e);

  if (! is_arglist_const(e) ) return FAILURE;

  return SUCCESS;

} /* end simplify_min */


/* simplify_minexponent */
static try simplify_minexponent(g95_expr *e) {
g95_expr *arg;

/* Type checking */

  arg = FIRST_ARG(e);

  if (arg->ts.type != BT_REAL) {
    g95_warning("Argument of MINEXPONENT at %L must be real",
		&FIRST_ARG(e)->where);
    return FAILURE;
  }

  if (arg->expr_type != EXPR_CONSTANT) return FAILURE;

  return SUCCESS;

} /* end simplify_minexponent */


/* simplify_mod() */
static try simplify_mod(g95_expr *e) {
g95_expr *arg1, *arg2, *rmid1, *rmid2, *result;

  arg1 = FIRST_ARG(e);
  arg2 = SECOND_ARG(e);

  if (arg1->expr_type != EXPR_CONSTANT && arg2->expr_type != EXPR_CONSTANT) 
    return FAILURE;

  switch (arg1->ts.type) {
    	case BT_INTEGER:
	    	if (arg2->ts.type != BT_INTEGER ) {
	     	        g95_warning("Type of arguments of MOD at %L must agree",
  				&FIRST_ARG(e)->where);
      		        return FAILURE;
    		}
    		else {
	      	  if (g95_compare_expr(arg2, integer_zero) != 0) {
			result = g95_copy_expr(e);
       		 	mpz_init(result->value.integer);
       		 	mpz_mod(result->value.integer, arg1->value.integer, 
                                                       arg2->value.integer);
			g95_replace_expr(e, result);
			return SUCCESS;
      		  }
      		  else {
		  /* Result is processor-dependent */
       		 	g95_warning("Second argument MOD at %L is zero",
  		       		  &FIRST_ARG(e)->where);
       		 	return FAILURE;
      		  }
		}
	    	break;
    	case BT_REAL:
	    	if (arg2->ts.type != BT_REAL ) {
		      g95_warning("Type of arguments of MOD at %L must agree",
  				&FIRST_ARG(e)->where);
      		      return FAILURE;
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
	                return SUCCESS;
      		  }
      		  else {
		  /* Result is processor-dependent */
       		 	g95_warning("Second argument of MOD at %L is zero",
  	       			  &FIRST_ARG(e)->where);
	        	return FAILURE;
      		  }
		}
    		break;
	    default:
       		g95_warning("Type of arguments of MOD at %L must be real or integer",
	 	       	&FIRST_ARG(e)->where);
		return FAILURE;
  }

/* If we get to here something went wrong */
  return FAILURE;

} /* end simplify_mod */


/* simplify_modulo */
static try simplify_modulo(g95_expr *e) {
g95_expr *arg;

/* This isnt even a skeleton */

  arg = FIRST_ARG(e);

  if (arg->expr_type != EXPR_CONSTANT) return FAILURE;

  return SUCCESS;

} /* end simplify_modulo */


/* simplify_mvbits() */
static try simplify_mvbits(g95_expr *e) {
g95_expr *arg1, *arg2, *arg3, *arg4, *arg5;

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
    return FAILURE;
  }

  if (g95_compare_expr(arg2, integer_zero) < 0) {
    g95_warning("Second argument of MVBITS at %L must be nonnegative",
		&FIRST_ARG(e)->where);
    return FAILURE;
  }

  if (g95_compare_expr(arg3, integer_zero) < 0) {
    g95_warning("Third argument of MVBITS at %L must be nonnegative",
		&FIRST_ARG(e)->where);
    return FAILURE;
  }

  if (g95_compare_expr(arg5, integer_zero) < 0) {
    g95_warning("Third argument of MVBITS at %L must be nonnegative",
		&FIRST_ARG(e)->where);
    return FAILURE;
  }

  return SUCCESS;

} /* end simplify_mvbits */


/* simplify_nearest */
static try simplify_nearest(g95_expr *e) {
g95_expr *arg1, *arg2;

/* Type checking */

  arg1 = FIRST_ARG(e);
  arg2 = SECOND_ARG(e);

  if (arg1->ts.type != BT_REAL && arg2->ts.type != BT_INTEGER) {
    g95_warning("Illegal type in NEAREST at %L",
		&FIRST_ARG(e)->where);
    return FAILURE;
  }

/* Range checking */
/* Second argument must not be equal to zero */

  if (g95_compare_expr(arg2, real_zero) == 0) {
    g95_warning("Second argument of NEAREST at %L is zero",
		&FIRST_ARG(e)->where);
    return FAILURE;
  }

  if (arg1->expr_type != EXPR_CONSTANT || arg2->expr_type != EXPR_CONSTANT ) 
    return FAILURE;

  return SUCCESS;

} /* end simplify_nearest */


/* simplify_nint */
static try simplify_nint(g95_expr *e) {
g95_expr *arg, *rmid, *rtrunc, *result;
int knd;
arith r;
mpf_t mpf_half;

/* Result needs to have correct KIND */
/* Takes optional KIND argument, not implemented */

  arg = FIRST_ARG(e);

  if (arg->ts.type != BT_REAL) {
    g95_warning("Argument of NINT at %L must be real",
		&SECOND_ARG(e)->where);
    return FAILURE;
  }

  knd = g95_validate_kind(BT_REAL, arg->ts.type);

  if (arg->expr_type != EXPR_CONSTANT) return FAILURE;

  rmid = g95_copy_expr(arg);

  mpf_init_set_str(mpf_half, "0.5", 10);

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
    return SUCCESS;
  }
  else {
    g95_warning("Conversion in NINT at %L failed",
		&SECOND_ARG(e)->where);
    return FAILURE;
  }

} /* end simplify_nint */


/* simplify_not() */
static try simplify_not(g95_expr *e) {
g95_expr *arg;
int knd;

/* Type checking */

  arg = FIRST_ARG(e);

  if (arg->ts.type != BT_INTEGER) {
    g95_warning("Argument of NOT at %L must be integer",
		&FIRST_ARG(e)->where);
    return FAILURE;
  }

  knd = g95_validate_kind(BT_INTEGER, arg->ts.type);

  if (arg->expr_type != EXPR_CONSTANT) 
    return FAILURE;

  return SUCCESS;
} /* end simplify_not */


/* simplify_precision */
static try simplify_precision(g95_expr *e) {
g95_expr *arg;

/* Type checking */

  arg = FIRST_ARG(e);

  if (arg->ts.type != BT_REAL || arg->ts.type != BT_COMPLEX) {
    g95_warning("Argument of PRECISION at %L must be real or complex",
		&FIRST_ARG(e)->where);
    return FAILURE;
  }

  if (arg->expr_type != EXPR_CONSTANT) return FAILURE;

  return SUCCESS;
} /* end simplify_precision */


/* simplify_radix */
static try simplify_radix(g95_expr *e) {
g95_expr *arg;

/* Type checking */

  arg = FIRST_ARG(e);

  if (arg->ts.type != BT_INTEGER || arg->ts.type != BT_REAL) {
    g95_warning("Argument of PRECISION at %L must be integer or real",
		&FIRST_ARG(e)->where);
    return FAILURE;
  }

  if (arg->expr_type != EXPR_CONSTANT) return FAILURE;

  return SUCCESS;
} /* end simplify_radix */


/* simplify_range */
static try simplify_range(g95_expr *e) {
g95_expr *arg;

/* Type checking */

  arg = FIRST_ARG(e);

  if (arg->ts.type != BT_INTEGER || arg->ts.type != BT_REAL 
		  || arg->ts.type != BT_COMPLEX) {
    g95_warning("Argument of PRECISION at %L must be integer, real, or complex",
		&FIRST_ARG(e)->where);
    return FAILURE;
  }

  if (arg->expr_type != EXPR_CONSTANT) return FAILURE;

  return SUCCESS;
} /* end simplify_range */


/* simplify_real() */
static try simplify_real(g95_expr *e) {
g95_expr *arg, *result;
arith r;

/* Needs KIND */
/* knd (optional) must be a valid real kind */

  arg = FIRST_ARG(e);

  if (arg->expr_type != EXPR_CONSTANT) return FAILURE;

  result = g95_copy_expr(arg);

  switch (arg->ts.type) {
	case BT_INTEGER:
            r = g95_int2real(&result, arg);
	    if ( r == ARITH_OK ) {
		g95_replace_expr(e, result);
	        return SUCCESS;
	    }
	    else {
	        g95_free_expr(result);
		return FAILURE;
	    }
	    break;

	case BT_REAL:
	    g95_replace_expr(e, result);
	    return SUCCESS;
	    break;

	case BT_COMPLEX:
       	    mpf_init(result->value.real);
	    mpf_set(result->value.real, arg->value.complex.r);
            g95_replace_expr(e, result);
	    return SUCCESS;
	    break;

        default:
       	    g95_warning("Invalid argument type in REAL at %L",
 	       	&FIRST_ARG(e)->where);
	    g95_free(result);
	    return FAILURE;
  }

  return SUCCESS;

} /* end simplify_real */


/* simplify_rrspacing */
static try simplify_rrspacing(g95_expr *e) {
g95_expr *arg;

/* Type checking */

  arg = FIRST_ARG(e);

  if (arg->ts.type != BT_REAL) {
    g95_warning("Argument of RRSPACING at %L must be real",
		&FIRST_ARG(e)->where);
    return FAILURE;
  }

  if (arg->expr_type != EXPR_CONSTANT) return FAILURE;

  return SUCCESS;

} /* end simplify_rrspacing */


/* simplify_scale */
static try simplify_scale(g95_expr *e) {
g95_expr *arg1, *arg2;

/* Type checking */

  arg1 = FIRST_ARG(e);
  arg2 = SECOND_ARG(e);

  if (arg1->ts.type != BT_REAL) {
    g95_warning("First argument of SCALE at %L must be real",
		&FIRST_ARG(e)->where);
    return FAILURE;
  }

  if (arg2->ts.type != BT_INTEGER) {
    g95_warning("Second argument of SCALE at %L must be integer",
		&FIRST_ARG(e)->where);
    return FAILURE;
  }

  if (arg1->expr_type != EXPR_CONSTANT || arg2->expr_type != EXPR_CONSTANT ) 
    return FAILURE;

  return SUCCESS;
}


/* simplify_scan() */
static try simplify_scan(g95_expr *e) {
g95_expr *arg1, *arg2;
int knd1, knd2;

/* Takes optional argument, not implemented */

/* Type checking */

  arg1 = FIRST_ARG(e);
  arg2 = SECOND_ARG(e);

  if (arg1->ts.type != BT_CHARACTER || arg2->ts.type != BT_CHARACTER ) {
    g95_warning("Arguments of SCAN at %L must be character",
		&FIRST_ARG(e)->where);
    return FAILURE;
  }

  knd1 = g95_validate_kind(BT_INTEGER, arg1->ts.type);
  knd2 = g95_validate_kind(BT_INTEGER, arg2->ts.type);

  if (knd1 != knd2 ) {
    g95_warning("Kind of arguments of SCAN at %L must agree",
                 &FIRST_ARG(e)->where);
    return FAILURE;
  }

  if (arg1->expr_type != EXPR_CONSTANT || arg2->expr_type != EXPR_CONSTANT) 
    return FAILURE;

  return SUCCESS;
} /* end simplify_scan */


/* simplify_selected_int_kind */
static try simplify_selected_int_kind(g95_expr *e) {

  return g95_simplify_selected_int_kind(e);
}


/* simplify_selected_real_kind */
static try simplify_selected_real_kind(g95_expr *e) {

  return g95_simplify_selected_real_kind(e);
}


/* simplify_set_exponent */
static try simplify_set_exponent(g95_expr *e) {
g95_expr *arg1, *arg2;

/* Type checking */

  arg1 = FIRST_ARG(e);
  arg2 = SECOND_ARG(e);

  if (arg1->ts.type != BT_REAL) {
    g95_warning("First argument of SET_EXPONENT at %L must be real",
		&FIRST_ARG(e)->where);
    return FAILURE;
  }

  if (arg2->ts.type != BT_INTEGER) {
    g95_warning("Second argument of SET_EXPONENT at %L must be real",
		&FIRST_ARG(e)->where);
    return FAILURE;
  }

  if (arg1->expr_type != EXPR_CONSTANT || arg2->expr_type != EXPR_CONSTANT) 
    return FAILURE;

  return SUCCESS;
} /* end simplify_set_exponent */


/* simplify_sign */
static try simplify_sign(g95_expr *e) {
g95_expr *arg1, *arg2, *rmid, *result;
int knd1, knd2;

  arg1 = FIRST_ARG(e);
  arg2 = SECOND_ARG(e);

/* Type checking */

  if (arg1->ts.type != BT_REAL || arg1->ts.type != BT_INTEGER) {
    g95_warning("First argument of SIGN at %L must be real or integer",
		&FIRST_ARG(e)->where);
    return FAILURE;
  }

  if ( (arg2->ts.type != arg1->ts.type) ) {
    g95_warning("Type of arguments of SET_EXPONENT at %L must agree",
		&FIRST_ARG(e)->where);
    return FAILURE;
  }

  knd1 = g95_validate_kind(BT_INTEGER, arg1->ts.type);
  knd2 = g95_validate_kind(BT_INTEGER, arg2->ts.type);

  if (knd1 != knd2 ) {
    g95_warning("Kind of arguments of SIGN at %L must agree",
                 &FIRST_ARG(e)->where);
    return FAILURE;
  }

  if (arg1->expr_type != EXPR_CONSTANT || arg2->expr_type != EXPR_CONSTANT) 
    return FAILURE;

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
		return FAILURE;
  }

  g95_replace_expr(e, result);

  return SUCCESS;
} /* end simplify_sign */


/* simplify_sin */
static try simplify_sin(g95_expr *e) {
g95_expr *arg;

  arg = FIRST_ARG(e);

  if (arg->ts.type != BT_REAL || arg->ts.type != BT_COMPLEX) {
    g95_warning("Argument of SIN at %L must be real or complex",
		&FIRST_ARG(e)->where);
    return FAILURE;
  }

  return SUCCESS;
} /* end simplify_sin */


/* simplify_sinh */
static try simplify_sinh(g95_expr *e) {
g95_expr *arg;

  arg = FIRST_ARG(e);

  if (arg->ts.type != BT_REAL) {
    g95_warning("Argument of SINH at %L must be real",
		&FIRST_ARG(e)->where);
    return FAILURE;
  }

  return SUCCESS;
} /* end simplify_sinh */


/* simplify_spacing */
static try simplify_spacing(g95_expr *e) {
g95_expr *arg;

/* Type checking */

  arg = FIRST_ARG(e);

  if (arg->ts.type != BT_REAL) {
    g95_warning("Argument of SPACING at %L must be real",
		&FIRST_ARG(e)->where);
    return FAILURE;
  }

  if (arg->expr_type != EXPR_CONSTANT) return FAILURE;

  return SUCCESS;

} /* end simplify_spacing */


/* simplify_sqrt() */
static try simplify_sqrt(g95_expr *e) {
g95_expr *arg, *result;

  arg = FIRST_ARG(e);

  if (arg->expr_type != EXPR_CONSTANT) return FAILURE;

/* Type checking */

  switch (arg->ts.type) {
  	case BT_REAL:
	  if (g95_compare_expr(arg, real_zero) == -1) {
	    g95_warning("Argument of SQRT at %L has a negative value",
			&FIRST_ARG(e)->where);
	    return FAILURE;
	  }
	  else {
	    result = g95_copy_expr(arg);
            mpf_sqrt(result->value.real, arg->value.real);
            g95_replace_expr(e,result);
            return SUCCESS;
	  }
	  break;

	case BT_COMPLEX:
	  /* Need to compute principle value */
	  break;

        default:
	    g95_warning("Argument of SQRT at %L must be real or complex",
			&FIRST_ARG(e)->where);
	    return FAILURE;
  }

/* If we reach here something went wrong */
  return FAILURE;

} /* end simplify_sqrt */


/* simplify_tan */
static try simplify_tan(g95_expr *e) {
g95_expr *arg;

  arg = FIRST_ARG(e);

  if (arg->ts.type != BT_REAL) {
    g95_warning("Argument of TAN at %L must be real or complex",
		&FIRST_ARG(e)->where);
    return FAILURE;
  }

  return SUCCESS;
} /* end simplify_sin */


/* simplify_tanh */
static try simplify_tanh(g95_expr *e) {
g95_expr *arg;

  arg = FIRST_ARG(e);

  if (arg->ts.type != BT_REAL) {
    g95_warning("Argument of TANH at %L must be real",
		&FIRST_ARG(e)->where);
    return FAILURE;
  }

  return SUCCESS;
} /* end simplify_tanh */


/* simplify_verify() */
static try simplify_verify(g95_expr *e) {
g95_expr *arg1, *arg2;
int knd1, knd2;

/* Takes optional argument, not implemented */

/* Type checking */

  arg1 = FIRST_ARG(e);
  arg2 = SECOND_ARG(e);

  if (arg1->ts.type != BT_CHARACTER || arg2->ts.type != BT_CHARACTER ) {
    g95_warning("Arguments of VERIFY at %L must be character",
		&FIRST_ARG(e)->where);
    return FAILURE;
  }

  knd1 = g95_validate_kind(BT_INTEGER, arg1->ts.type);
  knd2 = g95_validate_kind(BT_INTEGER, arg2->ts.type);

  if (knd1 != knd2 ) {
    g95_warning("Kind of arguments of VERIFY at %L must agree",
                 &FIRST_ARG(e)->where);
    return FAILURE;
  }

  if (arg1->expr_type != EXPR_CONSTANT || arg2->expr_type != EXPR_CONSTANT) 
    return FAILURE;

  return SUCCESS;
} /* end simplify_verify */


/**************************************************************************
*                 End of simplification functions
***************************************************************************/

/* check_real() */
static try check_real(g95_actual_arglist *arg) {
g95_expr *first, *second;
intrinsic_sym *sym;
g95_typespec ts;
int kind;

/* Check the arguments to a REAL subroutine call.  If
 * the argument list is correct, we resolve it to one of the conv_?_r
 * functions. */

  first = FIRST_CHECK_ARG(arg);
  second = SECOND_CHECK_ARG(arg);

  if (second == NULL) {
    if (first->ts.type == BT_COMPLEX)
      kind = first->ts.kind;
    else
      kind = g95_default_real_kind();

  } else {
    if (g95_extract_int(second, &kind) != NULL ||
	g95_validate_kind(BT_REAL, kind) == -1) {
      intrinsic_error("KIND argument of REAL intrinsic at %L requires "
		      "a constant integer kind value");
      return FAILURE;
    }
  }

/* Now that we have the destination kind, figure out which function
 * this one resolves to. */

  ts.type = BT_REAL;
  ts.kind = kind;

  sym = find_conv(&first->ts, &ts);
  if (sym == NULL)
    g95_internal_error("Can't find internal conversion for REAL(), "
		       "%s(%d) to %s(%d)", 
		       g95_typename(first->ts.type), first->ts.kind, 
		       g95_typename(ts.type), ts.kind);

  lib_name = sym->lib_name;

  return SUCCESS;
}

static try check_kind_intrinsic(g95_actual_arglist *arg) {
g95_expr *first; 

  first = FIRST_CHECK_ARG(arg);
  if (first == NULL)
    return FAILURE;

  if (first->ts.type == BT_DERIVED) {
    intrinsic_error("X argument to KIND intrinsic must be of intrinsic type");
    return FAILURE;
  }

  return SUCCESS;
}

static try check_selected_int_kind(g95_actual_arglist *arg) {
g95_expr *first; 

  first = FIRST_CHECK_ARG(arg);
  if (first == NULL) 
    return FAILURE;

  if (first->ts.type != BT_INTEGER) {
    intrinsic_error("KIND argument of SELECTED_INT_KIND intrinsic at %L "
		    "requires an integer value");
    return FAILURE;
  }

  return SUCCESS;
}

static try check_selected_real_kind(g95_actual_arglist *arg) {
g95_expr *first, *second;

  first = FIRST_CHECK_ARG(arg);
  second = SECOND_CHECK_ARG(arg);

  if (first == NULL)
    return FAILURE;

  if (first->ts.type != BT_INTEGER) {
    intrinsic_error("P argument of SELECTED_REAL_KIND intrinsic at %L "
		    "requires an integer value");
    return FAILURE;
  }

  if (second != NULL) {
    if (second->ts.type != BT_INTEGER) {
      intrinsic_error("R argument of SELECTED_REAL_KIND intrinsic at %L "
		      "requires an integer value");
      return FAILURE;
    }
  }

  return SUCCESS;
}

/*********** Subroutines to build the intrinsic list ****************/

/* add_sym()-- Add a single intrinsic symbol to the current list. 
 * Argument list:
 *    char *    Name of function
 *    int       Whether function is elemental (1=Non-elemental, 0=elemental)
 *    bt        Return type of function
 *    int       Kind of return type of function
 *    simplify
 *    cfunction
 * optional arguments come in multiples of four:
 *    char *    Name of argument
 *    bt        Type of argument
 *    int       Kind of argument
 *    int       Arg optional flag (1=Optional, 0=Required)
 *
 * The sequence is terminated by a NULL name. */

static void add_sym(char *name, int elemental, bt type, int kind,
		    try (*simplify)(g95_expr *),
		    try (*cfunction)(g95_actual_arglist *), ...) {
int optional, first_flag;
va_list argp;

  if (sizing) {
    if (type == BT_UNKNOWN)
      nsub++;
    else
      nfunc++;
  } else {
    strcpy(next_sym->name, name);

    strcpy(next_sym->lib_name, "__");
    strcat(next_sym->lib_name, name);

    next_sym->elemental = elemental;
    next_sym->ts.type = type;
    next_sym->ts.kind = kind;
    next_sym->simplify = simplify;
    next_sym->check_function = cfunction;
  }

  va_start(argp, cfunction);

  first_flag = 1;

  for(;;) {
    name = va_arg(argp, char *);
    if (name == NULL) break;

    type = va_arg(argp, bt);
    kind = va_arg(argp, int);
    optional = va_arg(argp, int);

    if (sizing)
      nargs++;
    else {
      next_arg++;

      if (first_flag)
	next_sym->arg = next_arg;
      else
	(next_arg-1)->next = next_arg;

      first_flag = 0;

      strcpy(next_arg->name, name);
      next_arg->ts.type = type;
      next_arg->ts.kind = kind;
      next_arg->optional = optional;
    }
  }

  next_sym++;
  va_end(argp);  
}


/* find_sym()-- Locate an intrinsic symbol given a base pointer,
 * number of elements in the table and a pointer to a name.  Returns
 * the NULL pointer if a name is not found. */

static intrinsic_sym *find_sym(intrinsic_sym *start, int n, char *name) {

  while(n > 0) {
    if (strcmp(name, start->name) == 0) return start;

    start++;
    n--;
  }

  return NULL;
}


/* find_function()-- Given a name, find a function in the intrinsic
 * function table.  Returns NULL if not found. */

static intrinsic_sym *find_function(char *name) {

  return find_sym(functions, nfunc, name);
}


/* find_subroutine()-- Given a name, find a function in the intrinsic
 * subroutine table.  Returns NULL if not found. */

static intrinsic_sym *find_subroutine(char *name) {

  return find_sym(subroutines, nsub, name);
}


/* make_generic()-- Collect a set of intrinsic functions into a
 * generic collection.  The first argument is the name of the generic
 * function (which is probably also the name of a specific function)
 * and the rest of the arguments are the names of the specific
 * functions. */

void make_generic(char *name, ...) {
intrinsic_sym *generic, *new;
va_list argp;

  if (sizing) return; 

  generic = find_function(name);
  if (generic == NULL)
    g95_internal_error("make_generic(): Can't find generic symbol '%s'", name);

  va_start(argp, name);

  for(;;) {
    name = va_arg(argp, char *);
    if (name == NULL) break;

    new = find_function(name);

    if (new == NULL) 
      g95_internal_error("make_generic(): Can't find symbol '%s',", name);

    new->next = generic->specific;
    generic->specific = new;

  }

  va_end(argp);
}


/******* Check functions ********/

/* These functions check to see if an argument list is compatible with
 * a particular intrinsic function or subroutine.  Presence of
 * required arguments has already been established, and the argument
 * list has NULL arguments in the correct places.  */

static try not_ready(g95_actual_arglist *dummy) {

  g95_warning("Intrinsic checker not ready");
  return FAILURE;
} 


/* add_functions()-- Add intrinsic functions */

static void add_functions(void) {

/* Argument names as in the standard (to be used as argument keywords) */
char   a[] = "a",  f[] = "field",     pt[] = "pointer",   tg[] = "target",
       b[] = "b",  m[] = "matrix",    ma[] = "matrix_a",  mb[] = "matrix_b",
       c[] = "c",  n[] = "ncopies",  pos[] = "pos",      bck[] = "back",
       i[] = "i",  v[] = "vector",    va[] = "vector_a",  vb[] = "vector_b",
       j[] = "j", a1[] = "a1",        fs[] = "fsource",   ts[] = "tsource",
       l[] = "l", a2[] = "a2",        mo[] = "mold",     ord[] = "order",
       p[] = "p", ar[] = "array",    shp[] = "shape",    src[] = "source",
       r[] = "r", bd[] = "boundary", pad[] = "pad",      set[] = "set",
       s[] = "s", dm[] = "dim",      knd[] = "kind",     msk[] = "mask",
       x[] = "x", sh[] = "shift",    stg[] = "string",   ssg[] = "substring",
       y[] = "y", sz[] = "size",     sta[] = "string_a", stb[] = "string_b",
       z[] = "z", ln[] = "len";

int di, dr, dd, dl, dc, dz;

  di = g95_default_integer_kind();
  dr = g95_default_real_kind();
  dd = g95_default_double_kind();
  dl = g95_default_logical_kind();
  dc = g95_default_character_kind();
  dz = g95_default_complex_kind();

  add_sym("abs",  0, BT_REAL,    dr, simplify_rabs, NULL, a, BT_REAL,    dr, 0, NULL);
  add_sym("iabs", 0, BT_INTEGER, di, simplify_iabs, NULL, a, BT_INTEGER, di, 0, NULL);
  add_sym("dabs", 0, BT_REAL,    dd, simplify_rabs, NULL, a, BT_REAL,    dd, 0, NULL);
  add_sym("cabs", 0, BT_REAL,    dr, NULL, NULL, a, BT_COMPLEX, dz, 0, NULL);
  make_generic("abs", "abs", "iabs", "dabs", "cabs", NULL);
  
  add_sym("achar", 0, BT_CHARACTER, dc, simplify_achar, NULL,
	  i, BT_INTEGER, di, 0, NULL);
  
  add_sym("acos",  0, BT_REAL, dr, simplify_acos, not_ready, x, BT_REAL, dr, 0, NULL);
  add_sym("dacos", 0, BT_REAL, dd, simplify_acos, not_ready, x, BT_REAL, dd, 0, NULL);
  make_generic("acos", "acos", "dacos", NULL);

  add_sym("adjustl", 0, BT_CHARACTER, dc, simplify_adjustl, NULL,
	  stg, BT_CHARACTER, dc, 0, NULL);

  add_sym("adjustr", 0, BT_CHARACTER, dc, simplify_adjustr, NULL,
	  stg, BT_CHARACTER, dc, 0, NULL);

  add_sym("aimag", 0, BT_REAL, dr, simplify_aimag, NULL, z, BT_COMPLEX, dz, 0, NULL);

  add_sym("aint", 0, BT_REAL, dr, simplify_aint, not_ready, a, BT_REAL, dr, 0,
	  knd, BT_INTEGER, di, 1, NULL);
  add_sym("dint", 0, BT_REAL, dd, NULL, NULL, a, BT_REAL, dd, 0, NULL);
  make_generic("aint", "aint", "dint", NULL);

/* KAH Takes logical array input */
  add_sym("all", 1, BT_LOGICAL, dl, NULL, not_ready,
	  msk, BT_LOGICAL, dl, 0, dm, BT_INTEGER, di, 1, NULL);

/* KAH Takes an allocatable array argument */
  add_sym("allocated", 1, BT_LOGICAL, dl, NULL, not_ready,
	  ar, BT_REAL, dr, 0, NULL);

  add_sym("anint", 0, BT_REAL, dr, simplify_anint, not_ready, a, BT_REAL, dr, 0,
	  knd, BT_INTEGER, di, 1, NULL);
  add_sym("dnint", 0, BT_REAL, dd, simplify_anint, NULL, a, BT_REAL, dd, 0, NULL);
  make_generic("anint", "anint", "dnint", NULL);

/* KAH Takes logical array input */
  add_sym("any", 1, BT_LOGICAL, dl, NULL, not_ready,
	  msk, BT_LOGICAL, dl, 0, dm, BT_INTEGER, di, 1, NULL);

  add_sym("asin",  0, BT_REAL, dr, simplify_asin, not_ready, x, BT_REAL, dr, 0, NULL);
  add_sym("dasin", 0, BT_REAL, dd, simplify_asin, not_ready, x, BT_REAL, dd, 0, NULL);
  make_generic("asin", "asin", "dasin", NULL);

/* KAH Takes pointer and target types-- BT_INTEGER used as a placeholder */
  add_sym("associated", 1, BT_LOGICAL, dl, NULL, not_ready,
	  pt, BT_INTEGER, di, 0, tg, BT_INTEGER, di, 1, NULL);

  add_sym("atan",  0, BT_REAL, dr, NULL, NULL, x, BT_REAL, dr, 0, NULL);
  add_sym("datan", 0, BT_REAL, dd, NULL, NULL, x, BT_REAL, dd, 0, NULL);
  make_generic("atan", "atan", "datan", NULL);

  add_sym("atan2",  0, BT_REAL, dr, simplify_atan2, not_ready,
	  y, BT_REAL, dr, 0, x, BT_REAL, dr, 0, NULL);
  add_sym("datan2", 0, BT_REAL, dd, simplify_atan2, not_ready,
	  y, BT_REAL, dd, 0, x, BT_REAL, dd, 0, NULL);
  make_generic("atan2", "atan2", "datan2", NULL);

/* KAH Takes integer scalar or array */
  add_sym("bit_size", 1, BT_INTEGER, di, NULL, not_ready,
	  i, BT_INTEGER, di, 0, NULL);

  add_sym("btest", 0, BT_LOGICAL, dl, simplify_btest, not_ready,
	  i, BT_INTEGER, di, 0, pos, BT_INTEGER, di, 0, NULL);

  add_sym("ceiling", 0, BT_INTEGER, di, simplify_ceiling, not_ready, a, BT_REAL, dr, 0,
	  knd, BT_INTEGER, di, 1, NULL);

  add_sym("char", 0, BT_CHARACTER, dc, simplify_char, not_ready,
	  i, BT_INTEGER, di, 0, knd, BT_INTEGER, di, 1, NULL);

  add_sym("cmplx", 0, BT_COMPLEX, dz, simplify_cmplx, not_ready,
	  x, BT_REAL, dr, 0, y, BT_REAL, dr, 1, knd, BT_INTEGER, di, 1, NULL);

  add_sym("conjg", 0, BT_COMPLEX, dz, simplify_conjg, NULL, z, BT_COMPLEX, dz, 0, NULL);

  add_sym("cos",  0, BT_REAL,    dr, simplify_cos, NULL, x, BT_REAL,    dr, 0, NULL);
  add_sym("dcos", 0, BT_REAL,    dd, NULL, NULL, x, BT_REAL,    dd, 0, NULL);
  add_sym("ccos", 0, BT_COMPLEX, dz, NULL, NULL, x, BT_COMPLEX, dz, 0, NULL);
  make_generic("cos", "cos", "dcos", "ccos", NULL);

  add_sym("cosh",  0, BT_REAL, dr, simplify_cosh, NULL, x, BT_REAL, dr, 0, NULL);
  add_sym("dcosh", 0, BT_REAL, dd, NULL, NULL, x, BT_REAL, dd, 0, NULL);
  make_generic("cosh", "cosh", "dcosh", NULL);

/* KAH Takes logical array input */
  add_sym("count", 1, BT_INTEGER, di, NULL, not_ready,
	  msk, BT_LOGICAL, dl, 0, dm, BT_INTEGER, di, 1, NULL);

/* KAH Takes an array argument of any type and returns an array */
  add_sym("cshift", 1, BT_REAL, dr, NULL, not_ready, ar, BT_REAL, dr, 0,
	  sh, BT_INTEGER, di, 0, dm, BT_INTEGER, di, 1, NULL);

  add_sym("dble", 0, BT_REAL, dd, simplify_dble, not_ready, a, BT_REAL, dr, 0, NULL);

/* KAH Input can be integer or real, scalar or array */
  add_sym("digits", 1, BT_INTEGER, di, NULL, not_ready,
	  x, BT_REAL, dr, 0, NULL);

  add_sym("dim",  0, BT_REAL,    dr, simplify_dim, NULL,
	  x, BT_REAL,    dr, 0, y, BT_REAL,    dr, 0, NULL);
  add_sym("idim", 0, BT_INTEGER, di, simplify_dim, NULL,
	  x, BT_INTEGER, di, 0, y, BT_INTEGER, di, 0, NULL);
  add_sym("ddim", 0, BT_REAL,    dd, simplify_dim, NULL,
	  x, BT_REAL,    dd, 0, y, BT_REAL,    dd, 0, NULL);
  make_generic("dim", "dim", "idim", "ddim", NULL);

/* KAH Takes vector input, returns scalar.  Vectors can be integer,
 * real, complex, or logical */
  add_sym("dot_product", 1, BT_REAL, dr, NULL, not_ready,
	  va, BT_REAL, dr, 0, vb, BT_REAL, dr, 0, NULL);

  add_sym("dprod", 0, BT_REAL, dd, simplify_dprod, NULL,
	  x, BT_REAL, dr, 0, y, BT_REAL, dr, 0, NULL);

/* KAH Takes array argument */
  add_sym("eoshift", 1, BT_REAL, dr, NULL, not_ready,
	  ar, BT_REAL, dr, 0, sh, BT_INTEGER, di, 0,
	  bd, BT_REAL, dr, 1, dm, BT_INTEGER, di, 1, NULL);

/* KAH Takes and returns real scalar or array */
  add_sym("epsilon", 1, BT_REAL, dr, NULL, not_ready, x, BT_REAL, dr, 0, NULL);

  add_sym("exp",  0, BT_REAL,    dr, simplify_exp, NULL, x, BT_REAL,    dr, 0, NULL);
  add_sym("dexp", 0, BT_REAL,    dd, NULL, NULL, x, BT_REAL,    dd, 0, NULL);
  add_sym("cexp", 0, BT_COMPLEX, dz, NULL, NULL, x, BT_COMPLEX, dz, 0, NULL);
  make_generic("exp", "exp", "dexp", "cexp", NULL);

  add_sym("exponent", 0, BT_INTEGER, di, simplify_exponent, NULL, x, BT_REAL, dr, 0, NULL);

  add_sym("floor", 0, BT_INTEGER, di, simplify_floor, not_ready, a, BT_REAL, dr, 0,
	  knd, BT_INTEGER, di, 1, NULL);

  add_sym("fraction", 0, BT_REAL, dr, simplify_fraction, NULL, x, BT_REAL, dr, 0, NULL);

/* KAH May be integer or real, input may be scalar or array */
  add_sym("huge", 1, BT_REAL, dr, NULL, not_ready, x, BT_REAL, dr, 0, NULL);

  add_sym("iachar", 0, BT_INTEGER, di, simplify_iachar, NULL,
	  c, BT_CHARACTER, dc, 0, NULL);

  add_sym("iand", 0, BT_INTEGER, di, simplify_iand, NULL,
	  i, BT_INTEGER, di, 0, j, BT_INTEGER, di, 0, NULL);

  add_sym("ibclr", 0, BT_INTEGER, di, simplify_ibclr, not_ready,
	  i, BT_INTEGER, di, 0, pos, BT_INTEGER, di, 0, NULL);

  add_sym("ibits", 0, BT_INTEGER, di, simplify_ibits, not_ready, i, BT_INTEGER, di, 0,
	  pos, BT_INTEGER, di, 0, ln, BT_INTEGER, di, 0, NULL);

  add_sym("ibset", 0, BT_INTEGER, di, simplify_ibset, not_ready,
	  i, BT_INTEGER, di, 0, pos, BT_INTEGER, di, 0, NULL);

  add_sym("ichar", 0, BT_INTEGER, di, simplify_ichar, not_ready,
	  c, BT_CHARACTER, dc, 0, NULL);

  add_sym("ieor", 0, BT_INTEGER, di, simplify_ieor, NULL,
	  i, BT_INTEGER, di, 0, j, BT_INTEGER, di, 0, NULL);

  add_sym("index", 0, BT_INTEGER, di, simplify_index, NULL, stg, BT_CHARACTER, dc, 0,
	  ssg, BT_CHARACTER, dc, 0, bck, BT_LOGICAL, dl, 1, NULL);

  add_sym("int",   0, BT_INTEGER, di, simplify_int, not_ready,
	  a, BT_REAL, dr, 0, knd, BT_INTEGER, di, 1, NULL);
  add_sym("ifix",  0, BT_INTEGER, di, simplify_int, NULL, a, BT_REAL, dr, 0, NULL);
  add_sym("idint", 0, BT_INTEGER, di, simplify_int, NULL, a, BT_REAL, dd, 0, NULL);
  make_generic("int", "int", "ifix", "idint", NULL);

  add_sym("ior", 0, BT_INTEGER, di, simplify_ior, NULL,
	  i, BT_INTEGER, di, 0, j, BT_INTEGER, di, 0, NULL);

  add_sym("ishft", 0, BT_INTEGER, di, simplify_ishft, not_ready,
	  i, BT_INTEGER, di, 0, sh, BT_INTEGER, di, 0, NULL);

  add_sym("ishftc", 0, BT_INTEGER, di, simplify_ishftc, not_ready, i, BT_INTEGER, di, 0,
	  sh, BT_INTEGER, di, 0, sz, BT_INTEGER, di, 1, NULL);

/* KAH Input can be any type, or an array of any type */
  add_sym("kind", 1, BT_INTEGER, di, simplify_kind_intrinsic, 
	  check_kind_intrinsic, x, BT_REAL, dr, 0, NULL);

/* KAH Array input, output can be an array */
  add_sym("lbound", 1, BT_INTEGER, di, NULL, not_ready,
	  ar, BT_REAL, dr, 0, dm, BT_INTEGER, di, 1, NULL);

  add_sym("len", 1, BT_INTEGER, di, NULL, NULL,
	  stg, BT_CHARACTER, dc, 0, NULL);

  add_sym("len_trim", 0, BT_INTEGER, di, simplify_len_trim, NULL,
	  stg, BT_CHARACTER, dc, 0, NULL);

  add_sym("lge", 0, BT_LOGICAL, dl, simplify_lge, NULL,
	  sta, BT_CHARACTER, dc, 0, stb, BT_CHARACTER, dc, 0, NULL);

  add_sym("lgt", 0, BT_LOGICAL, dl, simplify_lgt, NULL,
	  sta, BT_CHARACTER, dc, 0, stb, BT_CHARACTER, dc, 0, NULL);

  add_sym("lle", 0, BT_LOGICAL, dl, simplify_lle, NULL,
	  sta, BT_CHARACTER, dc, 0, stb, BT_CHARACTER, dc, 0, NULL);

  add_sym("llt", 0, BT_LOGICAL, dl, simplify_llt, NULL,
	  sta, BT_CHARACTER, dc, 0, stb, BT_CHARACTER, dc, 0, NULL);

  add_sym("log",  0, BT_REAL,    dr, simplify_log, not_ready,
	  x, BT_REAL,    dr, 0, NULL);
  add_sym("alog", 0, BT_REAL,    dr, simplify_log, not_ready,
	  x, BT_REAL,    dr, 0, NULL);
  add_sym("dlog", 0, BT_REAL,    dd, simplify_log, not_ready,
	  x, BT_REAL,    dd, 0, NULL);
  add_sym("clog", 0, BT_COMPLEX, dz, simplify_log, not_ready,
	  x, BT_COMPLEX, dz, 0, NULL);
  make_generic("log", "log", "alog", "dlog", "clog", NULL);

  add_sym("log10",  0, BT_REAL, dr, simplify_log10, not_ready, x, BT_REAL, dr, 0, NULL);
  add_sym("alog10", 0, BT_REAL, dr, simplify_log10, not_ready, x, BT_REAL, dr, 0, NULL);
  add_sym("dlog10", 0, BT_REAL, dd, simplify_log10, not_ready, x, BT_REAL, dd, 0, NULL);
  make_generic("log10", "log10", "alog10", "dlog10", NULL);

/* KAH knd (optional) must be a valid logical kind */
  add_sym("logical", 0, BT_LOGICAL, dl, simplify_logical, not_ready, l, BT_LOGICAL, dl, 0,
	  knd, BT_INTEGER, di, 1, NULL);

/* KAH Takes and returns arrays of any numeric or logical type */
  add_sym("matmul", 1, BT_REAL, dr, NULL, not_ready,
	  ma, BT_REAL, dr, 0, mb, BT_REAL, dr, 0, NULL);

/* KAH Takes an indefinite number of arguments (but at least two), the
 * argument names are a1,a2[,a3,...]
 * Note: amax0 is equivalent to real(max), max1 is equivalent to int(max) */
  add_sym("max",   0, BT_REAL,    dr, simplify_max, not_ready,
	  a1, BT_REAL,    dr, 0, a2, BT_REAL,    dr, 0, NULL);
  add_sym("max0",  0, BT_INTEGER, di, NULL, not_ready,
	  a1, BT_INTEGER, di, 0, a2, BT_INTEGER, di, 0, NULL);
  add_sym("amax1", 0, BT_REAL,    dr, NULL, not_ready,
	  a1, BT_REAL,    dr, 0, a2, BT_REAL,    dr, 0, NULL);
  add_sym("dmax1", 0, BT_REAL,    dd, NULL, not_ready,
	  a1, BT_REAL,    dd, 0, a2, BT_REAL,    dd, 0, NULL);
  add_sym("amax0", 0, BT_REAL,    dr, NULL, not_ready,
	  a1, BT_INTEGER, di, 0, a2, BT_INTEGER, di, 0, NULL);
  add_sym("max1",  0, BT_INTEGER, di, NULL, not_ready,
	  a1, BT_REAL,    dr, 0, a2, BT_REAL,    dr, 0, NULL);
  make_generic("max", "max", "max0", "amax1", "dmax1", "amax0", "max1", NULL);

/* KAH Takes scalar or array input */
  add_sym("maxexponent", 1, BT_INTEGER, di, simplify_maxexponent, not_ready,
	  x, BT_REAL, dr, 0, NULL);

/* KAH Takes array argument of type integer or real.  The type of the
 * second argument must be checked to decide if it's dm or msk if called
 * with two arguments. */
  add_sym("maxloc", 1, BT_INTEGER, di, NULL, not_ready, ar, BT_REAL, dr, 0,
	  dm, BT_INTEGER, di, 1, msk, BT_LOGICAL, dl, 1, NULL);

/* KAH Takes array argument of type integer or real, returns same type.
 * The type of the second argument must be checked to decide if it's dm
 * or msk if called with two arguments. */
  add_sym("maxval", 1, BT_REAL, dr, NULL, not_ready, ar, BT_REAL, dr, 0,
	  dm, BT_INTEGER, di, 1, msk, BT_LOGICAL, dl, 1, NULL);

/* KAH Takes any type for ts and fs */
  add_sym("merge", 0, BT_REAL, dr, NULL, not_ready, ts, BT_REAL, dr, 0,
	  fs, BT_REAL, dr, 0, msk, BT_LOGICAL, dl, 0, NULL);

/* KAH Takes an indefinite number of arguments (but at least two).  The
 * argument names are a1,a2[,a3,...]
 * Note: amin0 is equivalent to real(min), min1 is equivalent to int(min) */
  add_sym("min",   0, BT_REAL,    dr, simplify_min, not_ready,
	  a1, BT_REAL,    dr, 0, a2, BT_REAL,    dr, 0, NULL);
  add_sym("min0",  0, BT_INTEGER, di, NULL, not_ready,
	  a1, BT_INTEGER, di, 0, a2, BT_INTEGER, di, 0, NULL);
  add_sym("amin1", 0, BT_REAL,    dr, NULL, not_ready,
	  a1, BT_REAL,    dr, 0, a2, BT_REAL,    dr, 0, NULL);
  add_sym("dmin1", 0, BT_REAL,    dd, NULL, not_ready,
	  a1, BT_REAL,    dd, 0, a2, BT_REAL,    dd, 0, NULL);
  add_sym("amin0", 0, BT_REAL,    dr, NULL, not_ready,
	  a1, BT_INTEGER, di, 0, a2, BT_INTEGER, di, 0, NULL);
  add_sym("min1",  0, BT_INTEGER, di, NULL, not_ready,
	  a1, BT_REAL,    dr, 0, a2, BT_REAL,    dr, 0, NULL);
  make_generic("min", "min", "min0", "amin1", "dmin1", "amin0", "min1", NULL);

  add_sym("minexponent", 1, BT_INTEGER, di, simplify_minexponent, not_ready,
	  x, BT_REAL, dr, 0, NULL);

/* KAH Takes array argument of type integer or real.  The type of the
 * second argument must be checked to decide if it's dm or msk if called
 * with two arguments. */
  add_sym("minloc", 1, BT_INTEGER, di, NULL, not_ready, ar, BT_REAL, dr, 0,
	  dm, BT_INTEGER, di, 1, msk, BT_LOGICAL, dl, 1, NULL);

/* KAH Takes array argument of type integer or real, returns same type.
 * The type of the second argument must be checked to decide if it's dm
 * or msk if called with two arguments. */
  add_sym("minval", 1, BT_REAL, dr, NULL, not_ready, ar, BT_REAL, dr, 0,
	  dm, BT_INTEGER, di, 1, msk, BT_LOGICAL, dl, 1, NULL);

  add_sym("mod",  0, BT_INTEGER, di, simplify_mod, NULL,
	  a, BT_INTEGER, di, 0, p, BT_INTEGER, di, 0, NULL);
  add_sym("amod", 0, BT_REAL,    dr, simplify_mod, NULL,
	  a, BT_REAL,    dr, 0, p, BT_REAL,    dr, 0, NULL);
  add_sym("dmod", 0, BT_REAL,    dd, simplify_mod, NULL,
	  a, BT_REAL,    dd, 0, p, BT_REAL,    dd, 0, NULL);
  make_generic("mod", "mod", "amod", "dmod", NULL);

  add_sym("modulo", 0, BT_INTEGER, di, simplify_modulo, not_ready,
	  a, BT_INTEGER, di, 0, p, BT_INTEGER, di, 0, NULL);

  add_sym("nearest", 0, BT_REAL, dr, simplify_nearest, not_ready,
	  x, BT_REAL, dr, 0, s, BT_REAL, dr, 0, NULL);

  add_sym("nint",   0, BT_INTEGER, di, simplify_nint, not_ready, a, BT_REAL, dr, 0,
	  knd, BT_INTEGER, di, 1, NULL);
  add_sym("idnint", 0, BT_INTEGER, di, simplify_nint, NULL, a, BT_REAL, dd, 0, NULL);
  make_generic("nint", "nint", "idnint", NULL);

  add_sym("not", 0, BT_INTEGER, di, simplify_not, NULL, i, BT_INTEGER, di, 0, NULL);

/* KAH Takes and returns pointers-- using BT_INTEGER as a placeholder */
  add_sym("null", 1, BT_INTEGER, di, NULL, not_ready,
	  mo, BT_INTEGER, di, 1, NULL);

/* KAH Takes arrays and an optional vector and returns a vector */
  add_sym("pack", 1, BT_REAL, dr, NULL, not_ready, ar, BT_REAL, dr, 0,
	  msk, BT_LOGICAL, dl, 0, v, BT_REAL, dr, 1, NULL);

  add_sym("precision",1, BT_INTEGER, di, simplify_precision, not_ready,
	  x, BT_REAL, dr, 0, NULL);

/* KAH Takes any type, including pointer */
  add_sym("present", 1, BT_LOGICAL, dl, NULL, not_ready,
	  a, BT_REAL, dr, 0, NULL);

/* KAH Takes array argument of type integer, real, or complex.  The type
 * of the second argument must be checked to decide if it's dm or msk if
 * called with two arguments. */
  add_sym("product", 1, BT_REAL, dr, NULL, not_ready, ar, BT_REAL, dr, 0,
	  dm, BT_INTEGER, di, 1, msk, BT_LOGICAL, dl, 1, NULL);

  add_sym("radix", 1, BT_INTEGER, di, simplify_radix, not_ready,
	  x, BT_REAL, dr, 0, NULL);

  add_sym("range", 1, BT_INTEGER, di, simplify_range, not_ready,
	  x, BT_REAL, dr, 0, NULL);

  add_sym("real",  0, BT_REAL, dr, simplify_real, check_real,
	  a, BT_INTEGER, di, 0, knd, BT_INTEGER, di, 1, NULL);
  add_sym("float", 0, BT_REAL, dr, simplify_real, NULL, a, BT_INTEGER, di, 0, NULL);
  add_sym("sngl",  0, BT_REAL, dr, NULL, NULL, a, BT_REAL,    dd, 0, NULL);
  make_generic("real", "real", "float", "sngl", NULL);

  add_sym("repeat", 1, BT_CHARACTER, dc, NULL, NULL,
	  stg, BT_CHARACTER, dc, 0, n, BT_INTEGER, di, 0, NULL);

/* KAH Takes any type array, integer arrays, returns array */
  add_sym("reshape", 1, BT_REAL, dr, NULL, not_ready,
	  src, BT_REAL, dr, 0, shp, BT_INTEGER, di, 0,
	  pad, BT_REAL, dr, 1, ord, BT_INTEGER, di, 1, NULL);

  add_sym("rrspacing",0, BT_REAL, dr, simplify_rrspacing, NULL, x, BT_REAL, dr, 0, NULL);

  add_sym("scale", 0, BT_REAL, dr, simplify_scale, NULL,
	  x, BT_REAL, dr, 0, i, BT_INTEGER, di, 0, NULL);

  add_sym("scan", 0, BT_INTEGER, di, simplify_scan, not_ready, stg, BT_CHARACTER, dc, 0,
	  set, BT_CHARACTER, dc, 0, bck, BT_LOGICAL, dl, 1, NULL);    

  add_sym("selected_int_kind", 1, BT_INTEGER, di, 
	  simplify_selected_int_kind, check_selected_int_kind,
	  r, BT_INTEGER, di, 0, NULL);

  add_sym("selected_real_kind", 1, BT_INTEGER, di, 
	  simplify_selected_real_kind, check_selected_real_kind,
	  p, BT_INTEGER, di, 1, r, BT_INTEGER, di, 1, NULL);

  add_sym("set_exponent", 0, BT_REAL, dr, simplify_set_exponent, NULL,
	  x, BT_REAL, dr, 0, i, BT_INTEGER, di, 0, NULL);

/* KAH Takes array of any type, returns integer array */
  add_sym("shape", 1, BT_INTEGER, di, NULL, not_ready,
	  src, BT_REAL, dr, 0, NULL);

  add_sym("sign",  0, BT_REAL,    dr, simplify_sign, NULL,
	  a, BT_REAL,    dr, 0, b, BT_REAL,    dr, 0, NULL);
  add_sym("isign", 0, BT_INTEGER, di, simplify_sign, NULL,
	  a, BT_INTEGER, di, 0, b, BT_INTEGER, di, 0, NULL);
  add_sym("dsign", 0, BT_REAL,    dd, simplify_sign, NULL,
	  a, BT_REAL,    dd, 0, b, BT_REAL,    dd, 0, NULL);
  make_generic("sign", "sign", "isign", "dsign", NULL);

  add_sym("sin",  0, BT_REAL,    dr, simplify_sin, NULL, x, BT_REAL,    dr, 0, NULL);
  add_sym("dsin", 0, BT_REAL,    dd, simplify_sin, NULL, x, BT_REAL,    dd, 0, NULL);
  add_sym("csin", 0, BT_COMPLEX, dz, simplify_sin, NULL, x, BT_COMPLEX, dz, 0, NULL);
  make_generic("sin", "sin", "dsin", "csin", NULL);

  add_sym("sinh",  0, BT_REAL, dr, simplify_sinh, NULL, x, BT_REAL, dr, 0, NULL);
  add_sym("dsinh", 0, BT_REAL, dd, simplify_sinh, NULL, x, BT_REAL, dd, 0, NULL);
  make_generic("sinh", "sinh", "dsinh", NULL);

/* KAH Takes array of any type */
  add_sym("size", 1, BT_INTEGER, di, NULL, not_ready,
	  ar, BT_REAL, dr, 0, dm, BT_INTEGER, di, 1, NULL);

  add_sym("spacing", 0, BT_REAL, dr, simplify_spacing, NULL, x, BT_REAL, dr, 0, NULL);

/* KAH Takes array of any type, returns array */
  add_sym("spread", 1, BT_REAL, dr, NULL, not_ready, src, BT_REAL, dr, 0,
	  dm, BT_INTEGER, di, 0, n, BT_INTEGER, di, 0, NULL);

/* KAH Unless the arg is complex it must be >=0 */
  add_sym("sqrt",  0, BT_REAL,    dr, simplify_sqrt, NULL,
	  x, BT_REAL,    dr, 0, NULL);
  add_sym("dsqrt", 0, BT_REAL,    dd, simplify_sqrt, NULL,
	  x, BT_REAL,    dd, 0, NULL);
  add_sym("csqrt", 0, BT_COMPLEX, dz, NULL, NULL,
	  x, BT_COMPLEX, dz, 0, NULL);
  make_generic("sqrt", "sqrt", "dsqrt", "csqrt", NULL);

/* KAH Takes array argument of type integer or real, returns array of
 * same type.  The type of the second argument must be checked to decide
 * if it's dm or msk if called with two arguments. */
  add_sym("sum", 1, BT_REAL, dr, NULL, not_ready, ar, BT_REAL, dr, 0,
	  dm, BT_INTEGER, di, 1, msk, BT_LOGICAL, dl, 1, NULL);

  add_sym("tan",  0, BT_REAL, dr, simplify_tan, NULL, x, BT_REAL, dr, 0, NULL);
  add_sym("dtan", 0, BT_REAL, dd, simplify_tan, NULL, x, BT_REAL, dd, 0, NULL);
  make_generic("tan", "tan", "dtan", NULL);

  add_sym("tanh",  0, BT_REAL, dr, simplify_tanh, NULL, x, BT_REAL, dr, 0, NULL);
  add_sym("dtanh", 0, BT_REAL, dd, simplify_tanh, NULL, x, BT_REAL, dd, 0, NULL);
  make_generic("tanh", "tanh", "dtanh", NULL);

/* KAH Input may be scalar or array */
  add_sym("tiny", 1, BT_REAL, dr, NULL, not_ready, x, BT_REAL, dr, 0, NULL);

/* KAH Array function */
  add_sym("transfer", 1, BT_REAL, dr, NULL, not_ready, src, BT_REAL, dr, 0,
	  mo, BT_REAL, dr, 0, sz, BT_INTEGER, di, 1, NULL);

/* KAH Array function */
  add_sym("transpose",1, BT_REAL, dr, NULL, not_ready,
	  m, BT_REAL, dr, 0, NULL);

  add_sym("trim", 1, BT_CHARACTER, dc, NULL, NULL,
	  stg, BT_CHARACTER, dc, 0, NULL);

/* KAH Array function */
  add_sym("ubound", 1, BT_INTEGER, di, NULL, not_ready,
	  ar, BT_REAL, dr, 0, dm, BT_INTEGER, di, 1, NULL);

/* KAH Takes a vector and an array and returns a vector */
  add_sym("unpack", 1, BT_REAL, dr, NULL, not_ready, v, BT_REAL, dr, 0,
	  msk, BT_LOGICAL, dl, 0, f, BT_REAL, dr, 0, NULL);

  add_sym("verify", 0, BT_INTEGER, di, simplify_verify, NULL, stg, BT_CHARACTER, dc, 0,
	  set, BT_CHARACTER, dc, 0, bck, BT_LOGICAL, dl, 1, NULL);
}


/* add_subroutines()-- Add intrinsic subroutines */

static void add_subroutines(void) {

/* Argument names as in the standard (to be used as argument keywords) */
char   h[] = "harvest", dt[] = "date", vl[] = "values",  pt[] = "put",
       c[] = "count",   tm[] = "time", tp[] = "topos",   gt[] = "get",
       t[] = "to",      zn[] = "zone", fp[] = "frompos", cm[] = "count_max",
       f[] = "from",    sz[] = "size", ln[] = "len",     cr[] = "count_rate";

int di, dr, dc;

  di = g95_default_integer_kind(); 
  dr = g95_default_real_kind();
  dc = g95_default_character_kind();

  add_sym("cpu_time", 1, BT_UNKNOWN, 0, NULL, NULL, tm, BT_REAL, dr, 0, NULL);

/* KAH Last argument is a vector */
  add_sym("date_and_time", 1, BT_UNKNOWN, 0, NULL, not_ready,
	  dt, BT_CHARACTER, dc, 1, tm, BT_CHARACTER, dc, 1,
	  zn, BT_CHARACTER, dc, 1, vl, BT_INTEGER,   di, 1, NULL);

/* KAH j, ln and pt must be non-negative. i and t must have same kind
 * parameter. */
  add_sym("mvbits", 0, BT_UNKNOWN, 0, simplify_mvbits, not_ready,
	  f, BT_INTEGER, di, 0, fp, BT_INTEGER, di, 0, ln, BT_INTEGER, di, 0,
	  t, BT_INTEGER, di, 0, tp, BT_INTEGER, di, 0, NULL);

/* KAH Can take an array */
  add_sym("random_number", 1, BT_UNKNOWN, 0, NULL, not_ready,
	  h, BT_REAL, dr, 0, NULL);

/* KAH Second two possible arguments are integer arrays */
  add_sym("random_seed", 1, BT_UNKNOWN, 0, NULL, not_ready,
	  sz, BT_INTEGER, di, 1, pt, BT_INTEGER, di, 1,
	  gt, BT_INTEGER, di, 1, NULL);

  add_sym("system_clock", 1, BT_UNKNOWN, 0, NULL, NULL,
          c,  BT_INTEGER, di, 1, cr, BT_INTEGER, di, 1,
          cm, BT_INTEGER, di, 1, NULL);
}



/* add_conv()-- Add a function to the list of conversion symbols */

static void add_conv(bt from_type, int from_kind, bt to_type, int to_kind,
		     try (*simplify)(g95_expr *)) {

g95_typespec from, to;
intrinsic_sym *sym;

  if (sizing) {
    nconv++;
    return;
  }

  from.type = from_type;
  from.kind = from_kind;

  to.type = to_type;
  to.kind = to_kind;

  sym = conversion + nconv;

  strcpy(sym->name, conv_name(&from, &to));
  strcpy(sym->lib_name, sym->name);
  sym->simplify = simplify;

  nconv++;
}


/* add_conversions()-- Create intrinsic_sym nodes for all intrinsic
 * conversion functions. */

static void add_conversions(void) {
int dr, di, dz, dd;

  di = g95_default_integer_kind();
  dr = g95_default_real_kind(); 
  dd = g95_default_double_kind();
  dz = g95_default_complex_kind();

  add_conv(BT_INTEGER, di,   BT_INTEGER, di,  conv_null);
  add_conv(BT_REAL, dr,      BT_INTEGER, di,  conv_r_i);
  add_conv(BT_REAL, dd,      BT_INTEGER, di,  conv_d_i);
  add_conv(BT_COMPLEX, dz,   BT_INTEGER, di,  conv_z_i);

  add_conv(BT_INTEGER, di,   BT_REAL, dr,     conv_i_r);
  add_conv(BT_REAL, dr,      BT_REAL, dr,     conv_null);
  add_conv(BT_REAL, dd,      BT_REAL, dr,     conv_d_r);
  add_conv(BT_COMPLEX, dz,   BT_REAL, dr,     conv_z_r);

  add_conv(BT_INTEGER, di,   BT_REAL, dd,     conv_i_d);
  add_conv(BT_REAL, dr,      BT_REAL, dd,     conv_r_d);
  add_conv(BT_REAL, dd,      BT_REAL, dd,     conv_null);
  add_conv(BT_COMPLEX, dz,   BT_REAL, dd,     conv_z_d);

  add_conv(BT_INTEGER, di,   BT_COMPLEX, dz,  conv_i_z);
  add_conv(BT_REAL, dr,      BT_COMPLEX, dz,  conv_r_z);
  add_conv(BT_REAL, dd,      BT_COMPLEX, dz,  conv_d_z);
  add_conv(BT_COMPLEX, dz,   BT_COMPLEX, dz,  conv_null);

}


/* init_pi()-- Calculate pi.  We use the Bailey, Borwein and Plouffe formula:
 *
 * pi = \sum{n=0}^\infty (1/16)^n [4/(8n+1) - 2/(8n+4) - 1/(8n+5) - 1/(8n+6)]
 * 
 * which converges pretty quickly.  Taking 100 terms of the series
 * gives about 120 digits of accuracy. */

static void init_pi(void) {
mpf_t s, t;
int n;

  mpf_init(s);
  mpf_init(t);

  mpf_init(mpf_pi);
  mpf_set_ui(mpf_pi, 0);

  for(n=0; n<100; n++) {
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


/* g95_intrinsic_init_1()-- Initialize the table of intrinsics */

void g95_intrinsic_init_1(void) {

  init_pi();

  nargs = nfunc = nsub = nconv = 0;
  sizing = 1;

  add_functions();
  add_subroutines();
  add_conversions();

  functions = g95_getmem(sizeof(intrinsic_sym)*(nfunc+nsub)
			 + sizeof(intrinsic_arg)*nargs);

  next_sym = functions;
  subroutines = functions + nfunc;

  conversion = g95_getmem(sizeof(intrinsic_sym)*nconv);

  next_arg = ((intrinsic_arg *) (subroutines + nsub)) - 1;

  sizing = 0;
  nconv = 0;

  add_functions();
  add_subroutines();
  add_conversions();

  integer_zero = g95_convert_integer("0", g95_default_integer_kind(), 10);
  real_zero = g95_convert_real("0.0", g95_default_real_kind());

}


void g95_intrinsic_done_1(void) {

  mpf_clear(mpf_pi);
  mpf_clear(mpf_hpi);
  mpf_clear(mpf_nhpi);

  g95_free(functions);
  g95_free(conversion);

  g95_free_expr(integer_zero);
  g95_free_expr(real_zero);
}


/******** Subroutines to check intrinsic interfaces ***********/


/* g95_remove_nullargs()-- Given a formal argument list, remove any
 * null arguments that may have been left behind by a sort against
 * some formal argument list. */

static void g95_remove_nullargs(g95_actual_arglist **ap) {
g95_actual_arglist *head, *tail, *next;

  tail = NULL;

  for(head=*ap; head; head=next) {
    next = head->next;

    if (head->expr == NULL) {
      head->next = NULL;
      g95_free_actual_arglist(head);
    } else {
      if (tail == NULL)
	*ap = head;
      else
	tail->next = head;

      tail = head;
    }
  }

  if (tail == NULL) *ap = NULL;
}


/* sort_actual()-- Given an actual arglist and a formal arglist, sort
 * the actual arglist so that its arguments are in a one-to-one
 * correspondance with the format arglist.  Arguments that are not
 * present are given a blank g95_actual_arglist structure.  If
 * something is obviously wrong (say, a missing required argument) we
 * abort sorting and return FAILURE. */

static try sort_actual(char *name, g95_actual_arglist **ap,
		       intrinsic_arg *formal) {

g95_actual_arglist *actual, *a;
intrinsic_arg *f;

  g95_remove_nullargs(ap);
  actual = *ap;

  for(f=formal; f; f=f->next)
    f->actual = NULL;

  f = formal;
  a = actual;

  for(;;) {     /* Put the nonkeyword arguments in a 1:1 correspondence */
    if (f == NULL) break;
    if (a == NULL) goto optional;

    if (a->name[0] != '\0') goto keywords;

    f->actual = a;

    f = f->next;
    a = a->next;
  }

  if (a == NULL) goto do_sort;

  intrinsic_error("Too many arguments in call to '%s' at %%L", name);
  return FAILURE;

/* Associate the remaining actual arguments, all of which have to be
 * keyword arguments. */

keywords:
  for(; a; a=a->next) {
    for(f=formal; f; f=f->next)
      if (strcmp(a->name, f->name) == 0) break;

    if (f == NULL) {
      intrinsic_error("Can't find keyword named '%s' in call to '%s' at %%L",
		      a->name, name);
      return FAILURE;
    }

    if (f->actual != NULL) {
      intrinsic_error("Argument '%s' is associated twice in call to "
		      "'%s' at %%L", f->name, name);
      return FAILURE;
    }

    f->actual = a;
  }

/* At this point, all unmatched formal args must be optional */

optional:
  for(f=formal; f; f=f->next) {
    if (f->actual == NULL && f->optional == 0) {
      intrinsic_error("Missing actual argument for formal argument "
		      "'%s' in call to '%s' at %%L", f->name, name);
      return FAILURE;
    }
  }

/* Using the formal argument list, string the actual argument list
 * together in a way that corresponds with the formal list. */

do_sort:
  actual = NULL;

  for(f=formal; f; f=f->next) {
    a = (f->actual == NULL) ? g95_get_actual_arglist() : f->actual;

    if (actual == NULL) *ap = a;
    else actual->next = a;

    actual = a;
  }
  actual->next = NULL;  /* End the sorted argument list. */
      
  return SUCCESS;
}


/* check_arglist()-- Compare an actual argument list with an
 * intrinsic's formal argument list.  The lists are checked for
 * agreement of type and kind.  We don't check for arrayness here. */

static try check_arglist(g95_actual_arglist **ap, intrinsic_sym *sym) {
g95_actual_arglist *actual;
intrinsic_arg *formal;

  formal = sym->arg;
  actual = *ap;

  for(; formal; formal=formal->next, actual=actual->next) {
    if (actual->expr == NULL) continue;

    if (formal->ts.type != actual->expr->ts.type) {
      intrinsic_error("Type of argument %d in call to %s at %%L should "
		      "be %s, not %s", actual->arg_number, sym->name,
		      g95_typename(formal->ts.type),
		      g95_typename(actual->expr->ts.type));
      return FAILURE;
    }

    if (formal->ts.kind != actual->expr->ts.kind) {
      intrinsic_error("Kind of argument %d in call to %s at %%L should "
		      "be %d, not %d", actual->arg_number, sym->name,
		      formal->ts.kind,
		      actual->expr->ts.kind);
      return FAILURE;
    }
  }

  return SUCCESS;
}


/* check_specific()-- Given a pointer to an intrinsic symbol and an
 * expression consisting of a function call, see if the function call
 * is consistent with the intrinsic's formal argument list.  If it is,
 * try to simplify the expression.  */

static try check_specific(intrinsic_sym *specific, g95_expr *expr) {
g95_actual_arglist **ap;
try t;

  ap = &expr->value.function.actual;
  lib_name = NULL;

  if (sort_actual(specific->name, ap, specific->arg) == FAILURE)
    return FAILURE;

  if (specific->check_function == NULL) {
    t = check_arglist(ap, specific);
    if (t == SUCCESS) expr->ts = specific->ts;
  } else
    t = (specific->check_function)(*ap);

  if (t == FAILURE) return FAILURE;

/* Try to simplify */

  if (specific->simplify == NULL || (specific->simplify)(expr) == FAILURE) {
    expr->value.function.name =
      (lib_name != NULL) ? lib_name : specific->lib_name;
  }

  return SUCCESS;
}


/* g95_intrinsic_func_interface()-- see if a function call corresponds
 * to an intrinsic function call.  If so, the expression node is
 * simplified or bound to the correct external name. */

try g95_intrinsic_func_interface(g95_expr *expr) {
intrinsic_sym *isym, *specific;

  isym = find_function(expr->symbol->name);
  if (isym == NULL) {
    intrinsic_error("The function '%s' at %%L is not a valid intrinsic",
		    expr->symbol->name);
    return FAILURE;
  }

/* If the function is generic, check all of its specific incarnations.
 * If the generic name is also a specific, we check that name last, so
 * that any error message will correspond to the specific */

  if (isym->specific) {
    for(specific=isym->specific; specific; specific=specific->next) {
      if (specific == isym) continue;
      if (check_specific(specific, expr) == SUCCESS) return SUCCESS;
    }
  }

  return check_specific(isym, expr);
}


/* g95_intrinsic_sub_interface()-- see if a CALL statement corresponds
 * to an intrinic subroutine */

try g95_intrinsic_sub_interface(g95_code *c) {
g95_actual_arglist **argp;
intrinsic_sym *isym;
char *name;

  name = c->sym->name;
  argp = (g95_actual_arglist **) &c->ext;

  isym = find_subroutine(name);
  if (isym == NULL) {
    intrinsic_error("The subroutine '%s' at %%L is not a valid intrinsic",
		    name);
    return FAILURE;
  }

  if (isym->check_function != NULL) return isym->check_function(*argp);

  return check_arglist(argp, isym);
}


/* g95_check_intrinsic()-- Given a name, search for it in the proper
 * table and return the following values:
 *
 *    0   Name not found
 *    1   Name is a generic intrinsic name
 *    2   Name is a specific intrinsic name
 */

int g95_check_intrinsic(char *name, int sub_flag) {
intrinsic_sym *sym;

  if (sub_flag)
    sym = find_function(name);
  else
    sym = find_subroutine(name);

  if (sym == NULL) return 0;

  return (sym->specific != NULL) ? 1 : 2;
}


/* g95_convert_type()-- Tries to convert an expression (in place) from
 * one type to another.  The eflag control the behavior on error.
 * The possible values are:
 *   0    Don't generate any errors
 *   1    Generate a g95_error()
 *   2    Generate a g95_internal_error() 
 */

try g95_convert_type(g95_expr *expr, g95_typespec *ts, int eflag) {
intrinsic_sym *sym;
g95_expr *new;

  if (expr->ts.type == BT_DERIVED && ts->type == BT_DERIVED &&
      expr->ts.derived == ts->derived) return SUCCESS;

  sym = find_conv(&expr->ts, ts);
  if (sym == NULL) goto bad;

/* Insert a pre-resolved function call to the right function */

  new = g95_get_expr();
  *new = *expr;

  new = g95_build_funcall(NULL, new, NULL);
  new->value.function.name = sym->lib_name;

  *expr = *new;

  g95_free(new);
  expr->ts = *ts;

  if (FIRST_ARG(expr)->expr_type == EXPR_CONSTANT) { /* Simplify constant */
    if ((sym->simplify)(expr) == FAILURE) goto bad;
    return SUCCESS;
  }

  return SUCCESS;  

bad:
  switch(eflag) {
  case 0:
    break;

  case 1:
    g95_error("Can't convert %s(%d) to %s(%d) at %L",
	      g95_typename(expr->ts.type), expr->ts.kind,
	      g95_typename(ts->type), ts->kind,
	      &expr->where);
    break;

  default:
    g95_internal_error("Can't convert %s(%d) to %s(%d) at %L",
		       g95_typename(expr->ts.type), expr->ts.kind,
		       g95_typename(ts->type), ts->kind,
		       &expr->where);
  }

  return FAILURE;
}



/* g95_simplify_intrinsic()-- Try to simplify an expression node that
 * (might) represent an intrinsic function call.  Most intrinsic
 * function calls are not simplified here-- most are simplified as
 * soon as the function is resolved as an intrinsic, but sometimes a
 * type conversion intrinsic is inserted before resolution and must be
 * simplified at some point.
 *
 * This subroutine works by making sure the function's library name
 * points to the lib_name member of an intrinsic_sym structure.  In
 * other words, it has to have already been decided that an intrinsic
 * is called for. */

void g95_simplify_intrinsic(g95_expr *expr) {
intrinsic_sym *sym;
char *lib_name;
int i;

  if (expr->expr_type != EXPR_FUNCTION) return;

  lib_name = expr->value.function.name;

  if (lib_name >= ((char *) functions) && 
      lib_name <= ((char *) (functions+nfunc))) {
    sym = functions; 

    for(i=0; i<nfunc; i++, sym++)
      if (lib_name == sym->lib_name) {
	if (sym->simplify != NULL) (sym->simplify)(expr);      
	break;
      }
  }

  if (lib_name >= ((char *) conversion) && 
      lib_name <= ((char *) (conversion+nconv))) {
    sym = conversion;

    for(i=0; i<nconv; i++, sym++)
      if (lib_name == sym->lib_name) {
	if (sym->simplify != NULL) (sym->simplify)(expr);      
	break;
      }
  }
}
