/* Compiler arithmetic
   Copyright (C) 2000, 2001, 2002, 2003 Free Software Foundation, Inc.
   Contributed by Andy Vaught

This file is part of G95.

G95 is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

G95 is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with G95; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */    
    
/* arith.c-- Since target arithmetic must be done on the host, there
 * has to be some way of evaluating arithmetic expressions as the host
 * would evaluate them.  We use the Gnu MP library to do arithmetic,
 * and this file provides the interface. */      
      
#include "g95.h"
#include <string.h>
      
mpf_t pi, half_pi, two_pi; 
 
static mpf_t e_value;      
      
      
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
  { 1,   8 },  
  { 0,   0 }        
}; 
 
g95_real_info g95_real_kinds[] = {       
  { 4,  2,  24,  -125,  128 },  
  { 8,  2,  53, -1021, 1024 }, 
  { 0,  0,   0,     0,    0 } 
};   
   
   
  
  
/* g95_arith_error()-- Given an arithmetic error code, return a
 * pointer to a string that explains the error. */        
        
char *g95_arith_error(arith code) {
char *z;     
     
  switch(code) {  
  case ARITH_OK:              z = "Arithmetic OK"; break; 
  case ARITH_OVERFLOW:        z = "Arithmetic overflow"; break;
  case ARITH_UNDERFLOW:       z = "Arithmetic underflow"; break;          
  case ARITH_DIV0:            z = "Division by zero"; break;    
  case ARITH_0TO0:            z = "Indeterminate form 0 ** 0"; break; 
  case ARITH_INCOMMENSURATE:  z = "Array operands are incommensurate"; break;          
  default: g95_internal_error("g95_arith_error(): Bad error code");    
  }

  return z;       
}     
     
     
     
     
void arctangent(mpf_t *arg, mpf_t *result) {        
mpf_t absval, convgu, convgl, num, term, y, xp;      
int a, sign;       
       
/* Similar to sine routine but requires special handling for x near 1 */         
         
  mpf_init_set(y, *arg);     
     
/* Special cases */     
  if (mpf_cmp_ui(y, 0) == 0) {  
    mpf_set_ui(*result, 0);    
  } else if (mpf_cmp_ui(y,1) == 0) {    
    mpf_init(num);     
    mpf_div_ui(num, half_pi, 2);          
    mpf_set(*result, num);  
    mpf_clear(num);       
  } else if (mpf_cmp_si(y,-1) == 0) {      
    mpf_init(num);     
    mpf_div_ui(num, half_pi, 2);    
    mpf_neg(*result, num);          
    mpf_clear(num);
  } else { /* General cases */    
    
    mpf_init(absval);          
    mpf_abs(absval, y);     
     
    mpf_init_set_d(convgu, 1.5);       
    mpf_init_set_d(convgl, 0.5);      
    mpf_init_set_ui(num, 1);       
    mpf_init(term);      
      
    if (mpf_cmp(absval, convgl) < 0) {     
      mpf_init_set_ui(xp, 0);
      sign = -1;       
      for(a=1; a<G95_REAL_BITS+10; a++) {          
        mpf_mul(num, num, absval);         
	if (a%2 == 0) continue;        
        
	sign = -sign;          
	mpf_div_ui(term, num, a);       
	if (sign > 0)
	  mpf_add(xp, xp, term);
	else     
	  mpf_sub(xp, xp, term);         
      }      
    } else if (mpf_cmp(absval, convgu) >= 0) { 
      mpf_init_set(xp, half_pi); 
      sign = 1;   
      for(a=1; a<G95_REAL_BITS+10; a++) {       
        mpf_div(num, num, absval); 
	if (a%2 == 0) continue;        
        
	sign = -sign;    
	mpf_div_ui(term, num, a);      
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
      for(a=1; a<G95_REAL_BITS+10; a++) {      
        mpf_mul(num, num, absval);   
	if (a%2 == 0) continue;          
	sign = -sign;        
	mpf_div_ui(term, num, a);        
	if (sign > 0)  
	  mpf_add(xp, xp, term);
	else     
	  mpf_sub(xp, xp, term);  
      }   
   
      mpf_div_ui(term, half_pi, 2);         
      mpf_add(xp, term, xp);    
    }   
   
  /* This makes sure to preserve the identity arctan(-x) = -arctan(x) */    
  /* and improves accuracy to boot */         
         
    if (mpf_cmp_ui(y, 0) > 0)          
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
  mpf_clear(y); 
}      
      
      
         
         
void common_logarithm(mpf_t *a, mpf_t *rslt) {
mpf_t i10, log10;

  natural_logarithm(a, rslt);     
     
  mpf_init_set_ui(i10, 10);         
  mpf_init(log10);    
  natural_logarithm(&i10, &log10);

  mpf_div(*rslt, *rslt, log10);     
  mpf_clear(i10);         
  mpf_clear(log10);  
}          
          
          
          
          
int g95_default_integer_kind(void) {        
        
  return g95_integer_kinds[g95_option.i8 ? 1 : 0].kind;      
}     
     
   
   
/* g95_arith_init_1()-- Get things ready to do math. */       
       
void g95_arith_init_1(void) {     
g95_integer_info *int_info;     
g95_real_info *real_info;     
mpf_t c, s;         
mpz_t x;        
int t, o, limit;    
    
/* Set the default precision for GMP computations */      
  mpf_set_default_prec(G95_REAL_BITS+30);         
         
/* Calculate e, needed by the natural_logarithm() subroutine. */    
    
  mpf_init(s);     
  mpf_init_set_ui(e_value, 0);    
  mpf_init_set_ui(c, 1);     
     
  for(t=1; t<100; t++) {  
    mpf_add(e_value, e_value, c);       
    mpf_div_ui(c, c, t);   /* 1/(i!) */    
  }      
      
/* Calculate pi, 2pi, pi/2, and -pi/2, needed for trigonometric functions
 * We use the Bailey, Borwein and Plouffe formula:
 *
 * pi = \sum_{n=0}^\infty (1/16)^n [4/(8n+1) - 2/(8n+4) - 1/(8n+5) - 1/(8n+6)]
 *
 * which gives about four bits per iteration.
 */        
        
  mpf_init_set_ui(pi,0);  
  
  mpf_init(two_pi);  
  mpf_init(half_pi);

  limit = (G95_REAL_BITS / 4) + 10;  /* (1/16)^n gives 4 bits per iteration */          
          
  for(o=0; o<limit; o++) {  
    mpf_set_ui(s, 4);       
    mpf_div_ui(s, s, 8*o+1);  /* 4/(8n+1) */

    mpf_set_ui(c, 2);       
    mpf_div_ui(c, c, 8*o+4);  /* 2/(8n+4) */         
    mpf_sub(s, s, c);    
    
    mpf_set_ui(c, 1); 
    mpf_div_ui(c, c, 8*o+5);  /* 1/(8n+5) */
    mpf_sub(s, s, c);     
     
    mpf_set_ui(c, 1);       
    mpf_div_ui(c, c, 8*o+6);  /* 1/(8n+6) */          
    mpf_sub(s, s, c);        
        
    mpf_set_ui(c, 16); 
    mpf_pow_ui(c, c, o);      /* 16^n */        
        
    mpf_div(s, s, c);          
          
    mpf_add(pi, pi, s);       
  }      
      
  mpf_mul_ui(two_pi, pi, 2);    
  mpf_div_ui(half_pi, pi, 2);       
       
/* Convert the minimum/maximum values for each kind into their Gnu MP
 * representation. */   
   
  mpz_init(x); 
 
  for(int_info=g95_integer_kinds; int_info->kind != 0; int_info++) {         
    /* Huge */        
        
    mpz_set_ui(x, int_info->radix);        
    mpz_pow_ui(x, x, int_info->digits);   
   
    mpz_init(int_info->huge);   
    mpz_sub_ui(int_info->huge, x, 1);         
         
    /* These are the numbers that are actually representable by the
     * target.  For bases other than two, this needs to be changed. */     
     
    if (int_info->radix != 2)          
      g95_internal_error("Fix min_int, max_int calculation");        
        
    mpz_init(int_info->min_int);       
    mpz_neg(int_info->min_int, int_info->huge);  
    if (!g95_option.pedantic)
      mpz_sub_ui(int_info->min_int, int_info->min_int, 1);         
         
    mpz_init(int_info->max_int);  
    mpz_add(int_info->max_int, int_info->huge, int_info->huge);    
    mpz_add_ui(int_info->max_int, int_info->max_int, 1); 
 
    /* Range */     
     
    mpf_set_z(c, int_info->huge); 
    common_logarithm(&c, &c);      
    mpf_trunc(c, c);  
    mpz_set_f(x, c);         
    int_info->range = mpz_get_si(x);        
  }       
       
/*  mpf_set_default_prec(G95_REAL_BITS); */     
     
  for(real_info=g95_real_kinds; real_info->kind != 0; real_info++) {    
    /* Huge */          
          
    mpf_set_ui(c, real_info->radix);      
    mpf_set_ui(s, real_info->radix);        
        
    mpf_pow_ui(c, c, real_info->max_exponent);       
    mpf_pow_ui(s, s, real_info->max_exponent - real_info->digits);  
  
    mpf_init(real_info->huge);         
    mpf_sub(real_info->huge, c, s); 
 
    /* Tiny */          
          
    mpf_set_ui(s, real_info->radix);         
    mpf_pow_ui(s, s, 1-real_info->min_exponent);       
       
    mpf_init(real_info->tiny);      
    mpf_ui_div(real_info->tiny, 1, s);

    /* Epsilon */  
  
    mpf_set_ui(s, real_info->radix);      
    mpf_pow_ui(s, s, real_info->digits - 1);         
         
    mpf_init(real_info->epsilon);  
    mpf_ui_div(real_info->epsilon, 1, s);     
     
    /* Range */     
     
    common_logarithm(&real_info->huge, &c);  
    common_logarithm(&real_info->tiny, &s);        
    mpf_neg(s, s);      
      
    if (mpf_cmp(c, s) > 0) mpf_set(c, s);  /* a = min(a, b) */

    mpf_trunc(c, c);       
    mpz_set_f(x, c); 
    real_info->range = mpz_get_si(x);       
       
    /* Precision */          
          
    mpf_set_ui(c, real_info->radix);       
    common_logarithm(&c, &c);     
     
    mpf_mul_ui(c, c, real_info->digits-1);          
    mpf_trunc(c, c);      
    mpz_set_f(x, c);        
    real_info->precision = mpz_get_si(x);   
   
    /* If the radix is an integral power of 10, add one to the precision. */     
     
    for(t=10; t<=real_info->radix; t*=10)        
      if (t == real_info->radix) real_info->precision++;   
  }

  mpz_clear(x);       
  mpf_clear(c);
  mpf_clear(s);        
}      
      
      
 
 
/* g95_arith_done_1()-- Get rid of numeric constants. */       
       
void g95_arith_done_1(void) {  
g95_integer_info *i;         
g95_real_info *rp; 
 
  mpf_clear(e_value);         
         
  mpf_clear(pi);        
  mpf_clear(half_pi);
  mpf_clear(two_pi);     
     
  for(i=g95_integer_kinds; i->kind; i++) { 
    mpz_clear(i->min_int);       
    mpz_clear(i->max_int);       
    mpz_clear(i->huge);      
  }

  for(rp=g95_real_kinds; rp->kind; rp++) {          
    mpf_clear(rp->epsilon);        
    mpf_clear(rp->huge);    
    mpf_clear(rp->tiny);       
  }   
}  
  
  
/* g95_default_*_kind()-- Return default kinds */       
       
      
      
/* reduce_binary0()-- Reduce a binary expression where at least one of
 * the operands involves a zero-length array.  Returns NULL if neither
 * of the operands is a zero-length array. */      
      
static g95_expr *reduce_binary0(g95_expr *op, g95_expr *op2) {   
   
  if (g95_zero_size_array(op)) {    
    g95_free_expr(op2); 
    return op;
  }       
       
  if (g95_zero_size_array(op2)) {         
    g95_free_expr(op);         
    return op2;     
  } 
 
  return NULL;
}      
      
      
        
        
/* g95_convert_integer()-- Convert an integer string to an expression
 * node */         
         
g95_expr *g95_convert_integer(char *b, int kind, int radix,        
			      locus *w) {         
g95_expr *x;   
   
  x = g95_constant_result(BT_INTEGER, kind, w);  
  if (b[0] == '+') b++;     
  mpz_set_str(x->value.integer, b, radix);        
        
  return x;          
}        
        
        
      
/******* Simplification of intrinsic functions with constant arguments *****/  
  
/* arith_error()-- Deal with an arithmetic error. */    
    
static void arith_error(arith rv, g95_typespec *start, g95_typespec *d,         
			locus *w) {    
    
  g95_error("%s converting %s to %s at %L", g95_arith_error(rv),          
	    g95_typename(start), g95_typename(d), w);          
          
  /* TODO: Do something about the error, ie underflow rounds to 0,
   * throw exception, return NaN, etc. */        
}  
  
  
       
       
/* g95_zero_size_array()-- Return nonzero if the expression is a zero
 * size array. */ 
 
int g95_zero_size_array(g95_expr *m) {         
         
  if (m->type != EXPR_ARRAY) return 0;    
    
  return m->value.constructor == NULL;  
}      
      
      
        
        
int g95_default_logical_kind(void) {  
  
  return g95_logical_kinds[g95_option.i8 ? 1 : 0].kind;    
}   
   
      
      
arith g95_arith_gt(g95_expr *op, g95_expr *op0, g95_expr **rp) {     
g95_expr *res;

  res = g95_constant_result(BT_LOGICAL, g95_default_logical_kind(),      
			       &op->where); 
  res->value.logical = (g95_compare_expr(op, op0) > 0);      
  *rp = res; 
 
  return ARITH_OK;       
}          
          
         
         
static int validate_real(int kind) {       
int b;      
      
  for(b=0;; b++) {  
    if (g95_real_kinds[b].kind == 0) { b = -1; break; }    
    if (g95_real_kinds[b].kind == kind) break;       
  }    
    
  return b;         
}      
      
      
   
   
/* natural_logarithm()-- Compute a natural logarithm */  
  
void natural_logarithm(mpf_t *ap, mpf_t *rslt) {        
mpf_t z, xp, a, log;       
int u, n;          
          
  mpf_init_set(z, *ap); 
  mpf_init(a);   
   
  n = 0;      
      
  /* Get the argument into the range 0.5 to 1.5 by successive
   * multiplications or divisions by e. */  
  
  mpf_set_str(a, "0.5", 10);   
  while(mpf_cmp(z, a) < 0) {
    mpf_mul(z, z, e_value);
    n--;         
  } 
 
  mpf_set_str(a, "1.5", 10);         
  while(mpf_cmp(z, a) > 0) {        
    mpf_div(z, z, e_value);        
    n++;      
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
          
  mpf_sub_ui(z, z, 1);     
  mpf_init_set_ui(log, 0);    
  mpf_init_set_ui(xp, 1);        
        
  for(u=1; u<G95_REAL_BITS; u++) {      
    mpf_mul(xp, xp, z);     
    mpf_div_ui(a, xp, u); 
 
    if (u % 2 == 0) 
      mpf_sub(log, log, a);       
    else    
      mpf_add(log, log, a);      
  }    
    
  /* Add in the log (e^p) = p */          
          
  if (n < 0)     
    mpf_sub_ui(log, log, -n);       
  else     
    mpf_add_ui(log, log, n);       
       
  mpf_clear(z);
  mpf_clear(xp);     
  mpf_clear(a);  
  
  mpf_set(*rslt, log);
  mpf_clear(log);        
}  
  
  
       
       
static arith reduce_binary_ac(arith (*eval)(), g95_expr *op1, g95_expr *op_2, 
			      g95_expr **rslt) { 
g95_constructor *t, *head;        
g95_expr *j;        
arith retval;      
      
  head = g95_copy_constructor(op1->value.constructor); 
  retval = ARITH_OK;   
   
  for(t=head; t; t=t->next) {    
    retval = eval(t->expr, op_2, &j);       
    if (retval != ARITH_OK) break;

    g95_replace_expr(t->expr, j);
  }        
        
  if (retval != ARITH_OK)          
    g95_free_constructor(head);         
  else { 
    j = g95_get_expr();
    j->type = EXPR_ARRAY;  
    j->value.constructor = head;         
    j->shape = g95_copy_shape(op1->shape, op1->rank);  
  
    j->ts = head->expr->ts;          
    j->where = op1->where;
    j->rank = op1->rank;      
      
    *rslt = j; 
  }        
        
  return retval;        
} 
 
 
         
         
/* g95_convert_complex()-- Convert a pair of real, constant expression
 * nodes to a single complex expression node. */   
   
g95_expr *g95_convert_complex(g95_expr *real, g95_expr *imag, int knd) {         
g95_expr *s;          
          
  s = g95_constant_result(BT_COMPLEX, knd, &real->where);         
  mpf_set(s->value.complex.r, real->value.real);        
  mpf_set(s->value.complex.i, imag->value.real);    
    
  return s;         
}      
      
      
 
 
int g95_default_complex_kind(void) {     
     
  return g95_default_real_kind(); 
}      
      
      


/* complex_reciprocal()-- Compute the reciprocal of a complex number
 * (guaranteed nonzero) */       
       
static void complex_reciprocal(g95_expr *op) {       
mpf_t mod, l, result_r, result_i;         
         
  mpf_init(mod);       
  mpf_init(l);      
      
  mpf_mul(mod, op->value.complex.r, op->value.complex.r);       
  mpf_mul(l, op->value.complex.i, op->value.complex.i);       
  mpf_add(mod, mod, l);         
         
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
  mpf_clear(l);    
} 
 
 
        
        
/* g95_arith_uplus()-- It may seem silly to have a subroutine that
 * actually computes the unary plus of a constant, but it prevents us
 * from making exceptions in the code elsewhere. */          
          
arith g95_arith_uplus(g95_expr *op_1, g95_expr **rp) {       
       
  *rp = g95_copy_expr(op_1);          
  return ARITH_OK;        
}      
      
      
        
        
int g95_default_character_kind(void) {        
        
  return 1;
}       
       
         
         
/* g95_check_real_range()-- Given a real and a kind, make sure that
 * the real lies within the range of the kind.  Returns ARITH_OK,
 * ARITH_OVERFLOW or ARITH_UNDERFLOW. */   
   
static arith g95_check_real_range(mpf_t p, int k0) {
arith retval;      
mpf_t o; 
int s;      
      
  mpf_init(o);
  mpf_abs(o, p);  
  
  s = validate_real(k0);  
  if (s == -1) g95_internal_error("g95_check_real_range(): Bad kind");     
     
  retval = ARITH_OK;  
  if (mpf_sgn(o) == 0) goto done; 
 
  if (mpf_cmp(o, g95_real_kinds[s].huge) == 1) {    
    retval = ARITH_OVERFLOW;         
    goto done; 
  }  
  
  if (mpf_cmp(o, g95_real_kinds[s].tiny) == -1) retval = ARITH_UNDERFLOW;     
     
done:    
  mpf_clear(o);   
   
  return retval;  
}    
    
    
         
         
/* Low-level arithmetic functions.  All of these subroutines assume
 * that all operands are of the same type and return an operand of the
 * same type.  The other thing about these subroutines is that they
 * can fail in various ways-- overflow, underflow, division by zero,
 * zero raised to the zero, etc.  */  
  
arith g95_arith_not(g95_expr *op_1, g95_expr **resp) {  
g95_expr *rslt;   
   
  rslt = g95_constant_result(BT_LOGICAL, op_1->ts.kind, &op_1->where);      
  rslt->value.logical = !op_1->value.logical;      
  *resp = rslt;          
          
  return ARITH_OK;     
}  
  
  
arith g95_arith_and(g95_expr *op_1, g95_expr *op, g95_expr **resp) { 
g95_expr *rslt;        
        
  rslt = g95_constant_result(BT_LOGICAL, g95_kind_max(op_1, op),          
			       &op_1->where);   
  rslt->value.logical = op_1->value.logical && op->value.logical;
  *resp = rslt;

  return ARITH_OK;       
}         
         
         
arith g95_arith_or(g95_expr *op_1, g95_expr *op, g95_expr **resp) { 
g95_expr *rslt;          
          
  rslt = g95_constant_result(BT_LOGICAL, g95_kind_max(op_1, op), 
			       &op_1->where); 
  rslt->value.logical = op_1->value.logical || op->value.logical;      
  *resp = rslt;         
         
  return ARITH_OK;      
}


arith g95_arith_eqv(g95_expr *op_1, g95_expr *op, g95_expr **resp) {      
g95_expr *rslt;     
     
  rslt = g95_constant_result(BT_LOGICAL, g95_kind_max(op_1, op),   
			       &op_1->where);          
  rslt->value.logical = op_1->value.logical == op->value.logical; 
  *resp = rslt;   
   
  return ARITH_OK;
}        
        
        
arith g95_arith_neqv(g95_expr *op_1, g95_expr *op, g95_expr **resp) {      
g95_expr *rslt;       
       
  rslt = g95_constant_result(BT_LOGICAL, g95_kind_max(op_1, op), 
			       &op_1->where);    
  rslt->value.logical = op_1->value.logical != op->value.logical;  
  *resp = rslt;

  return ARITH_OK;
}


     
     
arith g95_arith_minus(g95_expr *op0, g95_expr *op_2, g95_expr **resultp) {       
g95_expr *rslt;         
arith rv;     
     
  rslt = g95_constant_result(op0->ts.type, op0->ts.kind, &op0->where); 
 
  switch(op0->ts.type) {   
  case BT_INTEGER:  
    mpz_sub(rslt->value.integer, op0->value.integer, op_2->value.integer);         
    break;  
  
  case BT_REAL:   
    mpf_sub(rslt->value.real, op0->value.real, op_2->value.real);  
    break;      
      
  case BT_COMPLEX:         
    mpf_sub(rslt->value.complex.r, op0->value.complex.r,  
	    op_2->value.complex.r);        
        
    mpf_sub(rslt->value.complex.i, op0->value.complex.i,         
	    op_2->value.complex.i);      
      
    break;  
  
  default:        
    g95_internal_error("g95_arith_minus(): Bad basic type");  
  }          
          
  rv = g95_range_check(rslt);      
      
  if (rv != ARITH_OK) 
    g95_free_expr(rslt);       
  else     
    *resultp = rslt;

  return rv; 
}    
    
    
      
      
int g95_default_double_kind(void) {       
       
  return g95_real_kinds[1].kind;
}        
        
     
     
static arith reduce_unary(arith (*eval)(), g95_expr *op1, g95_expr **res) {          
g95_constructor *y, *h;
g95_expr *p;       
arith rc;        
        
  if (op1->type == EXPR_CONSTANT) return eval(op1, res); 
 
  rc = ARITH_OK;   
  h = g95_copy_constructor(op1->value.constructor);          
          
  for(y=h; y; y=y->next) {    
    rc = eval(y->expr, &p);       
    if (rc != ARITH_OK) break;   
   
    g95_replace_expr(y->expr, p);   
  }        
        
  if (rc != ARITH_OK)     
    g95_free_constructor(h);      
  else {     
    p = g95_get_expr();      
    p->type = EXPR_ARRAY;    
    p->value.constructor = h;       
    p->shape = g95_copy_shape(op1->shape, op1->rank);        
        
    p->ts = h->expr->ts;
    p->where = op1->where;         
    p->rank = op1->rank;    
    
    *res = p;       
  }          
          
  return rc;
}   
   
   
   
   
arith g95_arith_plus(g95_expr *op0, g95_expr *op_2, g95_expr **rp) {       
g95_expr *rslt;
arith rc;  
  
  rslt = g95_constant_result(op0->ts.type, op0->ts.kind, &op0->where);       
       
  switch(op0->ts.type) {   
  case BT_INTEGER: 
    mpz_add(rslt->value.integer, op0->value.integer, op_2->value.integer);      
    break;   
   
  case BT_REAL:          
    mpf_add(rslt->value.real, op0->value.real, op_2->value.real); 
    break;     
     
  case BT_COMPLEX:       
    mpf_add(rslt->value.complex.r, op0->value.complex.r,      
	    op_2->value.complex.r);     
     
    mpf_add(rslt->value.complex.i, op0->value.complex.i,
	    op_2->value.complex.i);        
    break;   
   
  default:      
    g95_internal_error("g95_arith_plus(): Bad basic type");    
  }    
    
  rc = g95_range_check(rslt);        
        
  if (rc != ARITH_OK)   
    g95_free_expr(rslt);        
  else    
    *rp = rslt;     
     
  return rc;
}         
         
         
     
     
static int validate_logical(int knd) {       
int q;    
    
  if ( g95_option.l1 == 1 ) {      
    if ( knd == 1 ) {      
      q = 0;      
      return q;      
    }  
  }    
    
  for(q=0;; q++) {      
    if (g95_logical_kinds[q].kind == 0) { q = -1; break; }          
    if (g95_logical_kinds[q].kind == knd) break;    
  }  
  
  return q;  
}  
  
  
     
     
void cosine(mpf_t *argum, mpf_t *result) {        
mpf_t factor, s, j, num, d, term, x, xp;          
int h, sign;       
       
/* Similar to sine routine */          
          
  mpf_init_set(x, *argum);       
       
/* Special case (we do not treat multiples of pi due to roundoff issues) */ 
  if (mpf_cmp_ui(x,0) == 0) {          
    mpf_set_ui(*result, 1);      
  } else {   
    mpf_init(s); 
    mpf_init(j);   
    mpf_init(factor);        
    mpf_init(term);        
        
    mpf_div(s, x, two_pi); 
    mpf_floor(factor, s);   
    mpf_mul(s, factor, two_pi);    
    mpf_sub(j, x, s); 
 
    mpf_init_set_ui(xp, 1);   
    mpf_init_set_ui(num, 1);    
    mpf_init_set_ui(d, 1);       
       
    sign = 1;
    for(h=1; h<G95_REAL_BITS+10; h++) {  
      mpf_mul(num, num, j);          
      mpf_mul_ui(d, d, h);    
      if (h%2 != 0) continue; 
 
      sign = -sign;     
      mpf_div(term, num, d);        
      if (sign > 0)  
	mpf_add(xp, xp, term);        
      else     
	mpf_sub(xp, xp, term);   
    }   
    mpf_set(*result, xp);   
   
    mpf_clear(s);  
    mpf_clear(j);        
    mpf_clear(factor);         
    mpf_clear(num);      
    mpf_clear(d);  
    mpf_clear(term);     
    mpf_clear(xp);       
  } 
 
  mpf_clear(x);
}   
   
   
    
    
/* g95_arith_concat()-- Concatenate two string constants */

arith g95_arith_concat(g95_expr *op1, g95_expr *op2, g95_expr **resultp) { 
g95_expr *r; 
int l;         
         
  r = g95_constant_result(BT_CHARACTER, g95_default_character_kind(),  
			       &op1->where);    
    
  l = op1->value.character.length + op2->value.character.length;      
      
  r->value.character.string = g95_getmem(l+1);      
  r->value.character.length = l;   
   
  memcpy(r->value.character.string, op1->value.character.string,          
	 op1->value.character.length);          
          
  memcpy(r->value.character.string + op1->value.character.length,  
	 op2->value.character.string, op2->value.character.length);      
      
  r->value.character.string[l] = '\0';  
  
  *resultp = r;

  return ARITH_OK;
}          
          
          
         
static int validate_character(int k0) {

  if (k0 == g95_default_character_kind()) return 0;   
  return -1;      
}   
   
   
  
  
/* validate_integer()-- Make sure that a valid kind is present.
 * Returns an index into the g95_integer_kinds array, -1 if the kind
 * is not present. */      
      
static int validate_integer(int knd) {  
int r; 
 
  for(r=0;; r++) {        
    if (g95_integer_kinds[r].kind == 0) { r = -1; break; }       
    if (g95_integer_kinds[r].kind == knd) break; 
  }      
      
  return r;    
}  
  
  
     
     
arith g95_arith_uminus(g95_expr *op1, g95_expr **resultp) {       
g95_expr *res;
arith retval;  
  
  res = g95_constant_result(op1->ts.type, op1->ts.kind, &op1->where);     
     
  switch(op1->ts.type) {    
  case BT_INTEGER:        
    mpz_neg(res->value.integer, op1->value.integer);          
    break;

  case BT_REAL: 
    mpf_neg(res->value.real, op1->value.real);       
    break;  
  
  case BT_COMPLEX:          
    mpf_neg(res->value.complex.r, op1->value.complex.r);       
    mpf_neg(res->value.complex.i, op1->value.complex.i);      
    break;          
          
  default: 
    g95_internal_error("g95_arith_uminus(): Bad basic type");          
  }   
   
  retval = g95_range_check(res); 
 
  if (retval != ARITH_OK)   
    g95_free_expr(res);         
  else
    *resultp = res;      
      
  return retval;      
}


 
 
arith g95_arith_ge(g95_expr *op0, g95_expr *op, g95_expr **resultp) {   
g95_expr *r;     
     
  r = g95_constant_result(BT_LOGICAL, g95_default_logical_kind(),   
			       &op0->where);  
  r->value.logical = (g95_compare_expr(op0, op) >= 0);  
  *resultp = r;      
      
  return ARITH_OK;        
}    
    
    
    
/* hyperbolic cosine */   
   
void hypercos(mpf_t *a, mpf_t *r) {          
mpf_t neg, term1, term2, l, xp;          
          
  mpf_init_set(l, *a);         
         
  mpf_init(neg);       
  mpf_init(term1); 
  mpf_init(term2);          
  mpf_init(xp);        
        
  mpf_neg(neg, l);     
     
  exponential(&l, &term1);
  exponential(&neg, &term2); 
 
  mpf_add(xp, term1, term2);        
  mpf_div_ui(*r, xp, 2);    
    
  mpf_clear(neg);          
  mpf_clear(term1);        
  mpf_clear(term2);    
  mpf_clear(l);          
  mpf_clear(xp);       
}  
  
  
void hypersine(mpf_t *a, mpf_t *r) {       
mpf_t neg, term1, term2, l, xp;    
    
  mpf_init_set(l, *a);        
        
  mpf_init(neg);         
  mpf_init(term1);   
  mpf_init(term2);   
  mpf_init(xp);   
   
  mpf_neg(neg, l);  
  
  exponential(&l, &term1);    
  exponential(&neg, &term2);       
       
  mpf_sub(xp, term1, term2);   
  mpf_div_ui(*r, xp, 2);        
        
  mpf_clear(neg);     
  mpf_clear(term1);    
  mpf_clear(term2);     
  mpf_clear(l);          
  mpf_clear(xp);  
}  
  
  
          
          
/* g95_complex2complex()-- Convert complex to complex */      
      
g95_expr *g95_complex2complex(g95_expr *src, int k) {      
g95_expr *res;          
arith retval;    
    
  res = g95_constant_result(BT_COMPLEX, k, &src->where);         
         
  mpf_set(res->value.complex.r, src->value.complex.r);  
  mpf_set(res->value.complex.i, src->value.complex.i);  
  
  if ((retval = g95_check_real_range(res->value.complex.r, k)) != ARITH_OK ||  
      (retval = g95_check_real_range(res->value.complex.i, k)) != ARITH_OK) {        
    arith_error(retval, &src->ts, &res->ts, &src->where);    
    g95_free_expr(res);          
    return NULL;        
  }      
      
  return res;        
}        
        
        
    
    
/* g95_real2real()-- Convert real to real */

g95_expr *g95_real2real(g95_expr *src, int k0) {
g95_expr *res;   
arith rc;        
        
  res = g95_constant_result(BT_REAL, k0, &src->where);   
   
  mpf_set(res->value.real, src->value.real);         
         
  if ((rc = g95_check_real_range(res->value.real, k0)) != ARITH_OK) {        
    arith_error(rc, &src->ts, &res->ts, &src->where);          
    g95_free_expr(res); 
    return NULL;        
  }      
      
  return res;    
}    
    
    
     
     
arith g95_arith_le(g95_expr *op, g95_expr *op0, g95_expr **resultp) {
g95_expr *result; 
 
  result = g95_constant_result(BT_LOGICAL, g95_default_logical_kind(),      
			       &op->where);     
  result->value.logical = (g95_compare_expr(op, op0) <= 0);   
  *resultp = result;         
         
  return ARITH_OK;  
}

         
         
/* g95_convert_real()-- Convert a real string to an expression node. */         
         
g95_expr *g95_convert_real(char *b, int knd, locus *where) {          
g95_expr *a;         
         
  a = g95_constant_result(BT_REAL, knd, where);       
  mpf_set_str(a->value.real, b, 10);         
         
  return a;       
}       
       
       
        
        
arith g95_arith_times(g95_expr *op1, g95_expr *op, g95_expr **resultp) {       
g95_expr *result;  
mpf_t h, v;
arith retval;

  result = g95_constant_result(op1->ts.type, op1->ts.kind, &op1->where); 
 
  switch(op1->ts.type) {     
  case BT_INTEGER:      
    mpz_mul(result->value.integer, op1->value.integer, op->value.integer);   
    break;          
          
  case BT_REAL:  
    mpf_mul(result->value.real, op1->value.real, op->value.real);         
    break;    
    
  case BT_COMPLEX:  
    mpf_init(h);   
    mpf_init(v); 
 
    mpf_mul(h, op1->value.complex.r, op->value.complex.r);
    mpf_mul(v, op1->value.complex.i, op->value.complex.i);
    mpf_sub(result->value.complex.r, h, v);   
   
    mpf_mul(h, op1->value.complex.r, op->value.complex.i);      
    mpf_mul(v, op1->value.complex.i, op->value.complex.r);          
    mpf_add(result->value.complex.i, h, v);      
      
    mpf_clear(h);   
    mpf_clear(v);          
          
    break;  
  
  default:       
    g95_internal_error("g95_arith_times(): Bad basic type");      
  } 
 
  retval = g95_range_check(result); 
 
  if (retval != ARITH_OK)      
    g95_free_expr(result); 
  else      
    *resultp = result;      
      
  return retval; 
}        
        
        
          
          
void exponential(mpf_t *ap, mpf_t *res) { 
mpf_t two, ln2, power, k, j, num, denom, u, c, xp;    
int b;        
long z;   
unsigned long a, mp;   
   
/* We use a reduction of the form
 *   x= Nln2 + r
 *   then obtain exp(r) from the McLaurin series.
 *   exp(x) is then recovered from the identity exp(x) = 2^N*exp(r) */      
      
  mpf_init_set(c, *ap);   
   
  if (mpf_cmp_ui(c, 0) == 0) {          
    mpf_set_ui(*res,1);     
  } else if (mpf_cmp_ui(c, 1) == 0) { 
    mpf_set(*res, e_value);
  } else {        
    mpf_init_set_ui(two,2);          
    mpf_init(ln2);          
    mpf_init(k);     
    mpf_init(j); 
    mpf_init(power);   
    mpf_init(u);    
    
    natural_logarithm(&two, &ln2);         
         
    mpf_div(k, c, ln2);  
    mpf_floor(power, k); 
    mpf_mul(k, power, ln2);
    mpf_sub(j, c, k);          
          
    mpf_init_set_ui(xp, 1);  
    mpf_init_set_ui(num, 1);    
    mpf_init_set_ui(denom, 1);     
     
    for(b=1; b<=G95_REAL_BITS+10; b++) {         
      mpf_mul(num, num, j);  
      mpf_mul_ui(denom, denom, b);  
      mpf_div(u, num, denom);         
      mpf_add(xp, xp, u);          
    }    
    
    /* Reconstruction step */   
    z = (long) mpf_get_d(power);          
          
    if (z > 0) {    
      a = (unsigned int) z;     
      mpf_mul_2exp(*res,xp,a);       
    } else {  
      mp = (unsigned int) (-z);  
      mpf_div_2exp(*res,xp,mp);     
    }      
      
    mpf_clear(two);         
    mpf_clear(ln2);     
    mpf_clear(k);  
    mpf_clear(j);         
    mpf_clear(power);      
    mpf_clear(num);          
    mpf_clear(denom);     
    mpf_clear(u);
    mpf_clear(xp);
  }        
        
  mpf_clear(c);      
}         
         
         
        
        
/* g95_int2complex()-- Convert default integer to default complex */      
      
g95_expr *g95_int2complex(g95_expr *src, int k0) {   
g95_expr *res;    
arith retval;   
   
  res = g95_constant_result(BT_COMPLEX, k0, &src->where);      
      
  mpf_set_z(res->value.complex.r, src->value.integer);          
  mpf_set_ui(res->value.complex.i, 0);     
     
  if ((retval = g95_check_real_range(res->value.complex.i, k0)) != ARITH_OK) {         
    arith_error(retval, &src->ts, &res->ts, &src->where);
    g95_free_expr(res);          
    return NULL;          
  }         
         
  return res;      
}         
         
         
      
      
/* g95_complex2real()-- Convert complex to real */     
     
g95_expr *g95_complex2real(g95_expr *s1, int k) {      
g95_expr *res;    
arith rc;   
   
  res = g95_constant_result(BT_REAL, k, &s1->where);          
          
  mpf_set(res->value.real, s1->value.complex.r);         
         
  if ((rc = g95_check_real_range(res->value.real, k)) != ARITH_OK) {
    arith_error(rc, &s1->ts, &res->ts, &s1->where);     
    g95_free_expr(res); 
    return NULL;     
  }   
   
  return res;          
}   
   
   
    
    
void sine(mpf_t *ap, mpf_t *rslt) {   
mpf_t factor, k, f, num, denom, t, h, xp;        
int p, sign;

/* We use a reduction of the form
 *   x= N*2pi + r
 *   then obtain sin(r) from the McLaurin series. */      
      
  mpf_init_set(h, *ap);    
    
/* Special case (we do not treat multiples of pi due to roundoff issues) */         
  if (mpf_cmp_ui(h,0) ==  0) {         
    mpf_set_ui(*rslt,0); 
  } else {  
    mpf_init(k);    
    mpf_init(f);
    mpf_init(factor);    
    mpf_init(t);          
          
    mpf_div(k, h, two_pi);     
    mpf_floor(factor, k); 
    mpf_mul(k, factor, two_pi);      
    mpf_sub(f, h, k);    
    
    mpf_init_set_ui(xp, 0);          
    mpf_init_set_ui(num, 1);
    mpf_init_set_ui(denom, 1);          
          
    sign = -1;  
    for(p=1; p<G95_REAL_BITS+10; p++) {          
      mpf_mul(num, num, f);          
      mpf_mul_ui(denom, denom, p);          
      if (p%2 == 0) continue;         
         
      sign = -sign;       
      mpf_div(t, num, denom);
      if (sign > 0)   
	mpf_add(xp, xp, t);   
      else        
	mpf_sub(xp,xp,t); 
    }

    mpf_set(*rslt, xp);       
       
    mpf_clear(k);  
    mpf_clear(f); 
    mpf_clear(factor);      
    mpf_clear(num);          
    mpf_clear(denom);          
    mpf_clear(t);          
    mpf_clear(xp); 
  }       
       
  mpf_clear(h);       
}


         
         
/* g95_int2real()-- Convert integers to reals */         
         
g95_expr *g95_int2real(g95_expr *source, int k0) {
g95_expr *result;   
arith rv; 
 
  result = g95_constant_result(BT_REAL, k0, &source->where);     
     
  mpf_set_z(result->value.real, source->value.integer);         
         
  if ((rv = g95_check_real_range(result->value.real, k0)) != ARITH_OK) {    
    arith_error(rv, &source->ts, &result->ts, &source->where);         
    g95_free_expr(result);      
    return NULL;
  }      
      
  return result;         
} 
 
 
  
  
arith g95_arith_lt(g95_expr *op0, g95_expr *op, g95_expr **resp) {  
g95_expr *r;     
     
  r = g95_constant_result(BT_LOGICAL, g95_default_logical_kind(),     
			       &op0->where);      
  r->value.logical = (g95_compare_expr(op0, op) < 0); 
  *resp = r;    
    
  return ARITH_OK;         
} 
 
       
       
/* complex_pow_ui()-- Raise a complex number to positive power */    
    
static void complex_pow_ui(g95_expr *bottom, int power, g95_expr *result) {        
mpf_t temp_r, temp_i, e;     
     
  mpf_set_ui(result->value.complex.r, 1);   
  mpf_set_ui(result->value.complex.i, 0);       
       
  mpf_init(temp_r); 
  mpf_init(temp_i);
  mpf_init(e);        
        
  for(; power>0; power--) { 
    mpf_mul(temp_r, bottom->value.complex.r, result->value.complex.r);    
    mpf_mul(e,      bottom->value.complex.i, result->value.complex.i);     
    mpf_sub(temp_r, temp_r, e);         
         
    mpf_mul(temp_i, bottom->value.complex.r, result->value.complex.i);     
    mpf_mul(e,      bottom->value.complex.i, result->value.complex.r);         
    mpf_add(temp_i, temp_i, e);   
   
    mpf_set(result->value.complex.r, temp_r);  
    mpf_set(result->value.complex.i, temp_i);   
  }    
    
  mpf_clear(temp_r);          
  mpf_clear(temp_i);     
  mpf_clear(e);
}


       
       
/* g95_compare_string()-- Given two constant strings and the inverse
 * collating sequence, compare the strings.  We return -1 for a<b,
 * 0 for a==b and 1 for a>b.  If the xcoll_table is NULL, we use the
 * processor's default collating sequence. */        
        
int g95_compare_string(g95_expr *j, g95_expr *u, int *xcoll_table) {       
int len, alen, blen, q, ac, bc;        
        
  alen = j->value.character.length;  
  blen = u->value.character.length;    
    
  len = (alen > blen) ? alen : blen;         
         
  for(q=0; q<len; q++) {   
    ac = (q < alen) ? j->value.character.string[q] : ' ';      
    bc = (q < blen) ? u->value.character.string[q] : ' '; 
 
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
          
          
     
     
/* g95_check_integer_range()-- Given an integer and a kind, make sure
 * that the integer lies within the range of the kind.  Returns
 * ARITH_OK or ARITH_OVERFLOW. */ 
 
static arith g95_check_integer_range(mpz_t e, int knd) {         
arith r;          
int x; 
 
  x = validate_integer(knd);        
  if (x == -1) g95_internal_error("g95_check_integer_range(): Bad kind"); 
 
  r = ARITH_OK;          
          
  if (mpz_cmp(e, g95_integer_kinds[x].min_int) < 0 || 
      mpz_cmp(e, g95_integer_kinds[x].max_int) > 0) r = ARITH_OVERFLOW;          
          
  return r;        
}  
  
  


/* eval_type_intrinsic0() -- modify type of expression for zero size array */         
         
static g95_expr *eval_type_intrinsic0(g95_intrinsic_op oper, g95_expr *op){    
    
  if (op == NULL) g95_internal_error("eval_type_intrinsic0(): op NULL");    
    
  switch(oper) { 
  case INTRINSIC_GE:  case INTRINSIC_LT: 
  case INTRINSIC_LE:  case INTRINSIC_GT:         
  case INTRINSIC_EQ:  case INTRINSIC_NE:  
    op->ts.type = BT_LOGICAL;       
    op->ts.kind = g95_default_logical_kind();     
    break;    
    
  default:
    break;          
  }    
    
  return op;   
} 
 
 
   
   
/* g95_constant_result()-- Function to return a constant expression node
 * of a given type and kind. */          
          
g95_expr *g95_constant_result(bt t, int k0, locus *w) {       
g95_expr *result;       
       
  if (! w)          
    g95_internal_error("g95_constant_result(): locus 'where' cannot be NULL");    
    
  result = g95_get_expr();  
  
  result->type = EXPR_CONSTANT;    
  result->ts.type = t;         
  result->ts.kind = k0;        
  result->where = *w;   
   
  switch(t) {       
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


       
       
static arith reduce_binary_aa(arith (*eval)(), g95_expr *op_1, g95_expr *op,     
			      g95_expr **result) { 
g95_constructor *x, *s, *start;
g95_expr *o;       
arith rv;       
       
  start = g95_copy_constructor(op_1->value.constructor);      
      
  rv = ARITH_OK;        
  s = op->value.constructor;  
  
  if (g95_check_conformance("Elemental binary operation", op_1, op)     
      != SUCCESS)       
    rv = ARITH_INCOMMENSURATE;          
  else {      
      
    for(x=start; x; x=x->next, s=s->next) {         
      if (s == NULL) {          
	rv = ARITH_INCOMMENSURATE;          
	break;         
      }          
          
      rv = eval(x->expr, s->expr, &o);          
      if (rv != ARITH_OK) break;        
              
      g95_replace_expr(x->expr, o);
    }   
   
    if (s != NULL) rv = ARITH_INCOMMENSURATE;   
  }       
       
  if (rv != ARITH_OK)      
    g95_free_constructor(start);       
  else {
    o = g95_get_expr();
    o->type = EXPR_ARRAY; 
    o->value.constructor = start;   
    o->shape = g95_copy_shape(op_1->shape, op_1->rank);

    o->ts = start->expr->ts; 
    o->where = op_1->where;         
    o->rank = op_1->rank;    
    
    *result = o; 
  }        
        
  return rv;        
}


   
   
/* g95_compare_expr()-- Comparison operators.  Assumes that the two
 * expression nodes contain two constants of the same type. */     
     
int g95_compare_expr(g95_expr *op_1, g95_expr *op) {       
int r;  
  
  switch(op_1->ts.type) { 
  case BT_INTEGER:   
    r = mpz_cmp(op_1->value.integer, op->value.integer);        
    break;  
  
  case BT_REAL: 
    r = mpf_cmp(op_1->value.real, op->value.real);   
    break;   
   
  case BT_CHARACTER:       
    r = g95_compare_string(op_1, op, NULL);      
    break;      
      
  case BT_LOGICAL:     
    r = (!!op_1->value.logical) - (!!op->value.logical);          
          
    if (r < 0) r = -1;       
    if (r > 0) r = 1; 
    break;     
     
  default: g95_internal_error("g95_compare_expr(): Bad basic type");         
  }   
   
  return r; 
}      
      
      
     
     
arith g95_arith_divide(g95_expr *op, g95_expr *op_2, g95_expr **rp) {  
g95_expr *res;     
mpf_t v, z, div; 
arith rc;          
          
  rc = ARITH_OK;   
   
  res = g95_constant_result(op->ts.type, op->ts.kind, &op->where);      
      
  switch(op->ts.type) { 
  case BT_INTEGER:         
    if (mpz_sgn(op_2->value.integer) == 0) {
      rc = ARITH_DIV0;  
      break;         
    }          
          
    mpz_tdiv_q(res->value.integer, op->value.integer,     
	       op_2->value.integer);         
    break;

  case BT_REAL:
    if (mpf_sgn(op_2->value.real) == 0) { 
      rc = ARITH_DIV0;       
      break; 
    }     
     
    mpf_div(res->value.real, op->value.real, op_2->value.real);          
    break; 
 
  case BT_COMPLEX:     
    if (mpf_sgn(op_2->value.complex.r) == 0 &&         
	mpf_sgn(op_2->value.complex.i) == 0) {     
      rc = ARITH_DIV0;    
      break;        
    }        
        
    mpf_init(v);
    mpf_init(z);         
    mpf_init(div);  
  
    mpf_mul(v, op_2->value.complex.r, op_2->value.complex.r);     
    mpf_mul(z, op_2->value.complex.i, op_2->value.complex.i);        
    mpf_add(div, v, z);     
     
    mpf_mul(v, op->value.complex.r, op_2->value.complex.r); 
    mpf_mul(z, op->value.complex.i, op_2->value.complex.i);    
    mpf_add(res->value.complex.r, v, z);     
    mpf_div(res->value.complex.r, res->value.complex.r, div);       
       
    mpf_mul(v, op->value.complex.i, op_2->value.complex.r);        
    mpf_mul(z, op->value.complex.r, op_2->value.complex.i);
    mpf_sub(res->value.complex.i, v, z);
    mpf_div(res->value.complex.i, res->value.complex.i, div);       
       
    mpf_clear(v);        
    mpf_clear(z);      
    mpf_clear(div); 
 
    break;  
  
  default:       
    g95_internal_error("g95_arith_divide(): Bad basic type");         
  }     
     
  if (rc == ARITH_OK) rc = g95_range_check(res);          
          
  if (rc != ARITH_OK)
    g95_free_expr(res);    
  else
    *rp = res;        
        
  return rc;   
}        
        
        
         
         
int g95_default_real_kind(void) {  
  
  return g95_real_kinds[g95_option.r8 ? 1 : 0].kind;      
}       
       
         
         
/* g95_complex2int()-- Convert complex to integer */         
         
g95_expr *g95_complex2int(g95_expr *s, int k) {       
g95_expr *res;          
arith retval;  
  
  res = g95_constant_result(BT_INTEGER, k, &s->where);         
         
  mpz_set_f(res->value.integer, s->value.complex.r);     
     
  if ((retval = g95_check_integer_range(res->value.integer, k))    
      != ARITH_OK) {      
    arith_error(retval, &s->ts, &res->ts, &s->where);     
    g95_free_expr(res); 
    return NULL;     
  }  
  
  return res;  
}    
    
    
        
        
/* g95_arith_power()-- Raise a number to an integer power */

arith g95_arith_power(g95_expr *op1, g95_expr *op_2, g95_expr **resp) {
int power, apower;   
g95_expr *result;
mpz_t unity_z;     
mpf_t unity_f;        
arith rc;      
      
  rc = ARITH_OK;        
        
  if (g95_extract_int(op_2, &power) != NULL)       
    g95_internal_error("g95_arith_power(): Bad exponent");       
       
  result = g95_constant_result(op1->ts.type, op1->ts.kind, &op1->where);         
         
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
    *resp = result;        
        
  return rc;          
}        
        
        
     
     
/* compare_complex()-- Compare a pair of complex numbers.  Naturally,
 * this is only for equality/nonequality. */ 
 
static int compare_complex(g95_expr *op1, g95_expr *op) {

  return (mpf_cmp(op1->value.complex.r, op->value.complex.r) == 0 &&        
	  mpf_cmp(op1->value.complex.i, op->value.complex.i) == 0);     
}       
       
       
          
          
/* g95_real2int()-- Convert default real to default integer */         
         
g95_expr *g95_real2int(g95_expr *source, int k) {   
g95_expr *res;         
arith rc;    
    
  res = g95_constant_result(BT_INTEGER, k, &source->where);      
      
  mpz_set_f(res->value.integer, source->value.real);        
        
  if ((rc = g95_check_integer_range(res->value.integer, k))      
      != ARITH_OK) {    
    arith_error(rc, &source->ts, &res->ts, &source->where);    
    g95_free_expr(res);  
    return NULL;     
  }        
        
  return res;        
}       
       
       
         
         
arith g95_arith_ne(g95_expr *op_1, g95_expr *op2, g95_expr **resp) {    
g95_expr *r;  
  
  r = g95_constant_result(BT_LOGICAL, g95_default_logical_kind(),         
			       &op_1->where);      
  r->value.logical = (op_1->ts.type == BT_COMPLEX) ?   
    !compare_complex(op_1, op2) : (g95_compare_expr(op_1, op2) != 0);     
     
  *resp = r;
  return ARITH_OK;          
}

         
         
/* g95_validate_kind()-- Validate a kind given a basic type.  The
 * return value is the same for the child functions, with -1
 * indicating nonexistence of the type */     
     
int g95_validate_kind(bt t, int k) {  
int rv;

  switch(t) {       
  case BT_REAL:     /* Fall through */        
  case BT_COMPLEX:    rv = validate_real(k);      break;      
  case BT_INTEGER:    rv = validate_integer(k);   break;     
  case BT_LOGICAL:    rv = validate_logical(k);   break;     
  case BT_CHARACTER:  rv = validate_character(k); break;       
       
  default:
    g95_internal_error("g95_validate_kind(): Got bad type");     
  }          
          
  return rv;   
}       
       
       
     
     
/* g95_log2log()-- Logical kind conversion. */     
     
g95_expr *g95_log2log(g95_expr *s, int k) {        
g95_expr *result;    
    
  result = g95_constant_result(BT_LOGICAL, k, &s->where);         
  result->value.logical = s->value.logical;      
      
  return result;        
}     
     
     
     
    
    
arith g95_arith_eq(g95_expr *op0, g95_expr *op_2, g95_expr **r) {         
g95_expr *rslt;   
   
  rslt = g95_constant_result(BT_LOGICAL, g95_default_logical_kind(),   
			       &op0->where);        
  rslt->value.logical = (op0->ts.type == BT_COMPLEX) ?
    compare_complex(op0, op_2) : (g95_compare_expr(op0, op_2) == 0);

  *r = rslt; 
  return ARITH_OK; 
} 
 
       
       
/* g95_int2int()-- Convert integers to integers */          
          
g95_expr *g95_int2int(g95_expr *s0, int kind) {         
g95_expr *rslt;         
arith r;          
          
  rslt = g95_constant_result(BT_INTEGER, kind, &s0->where);         
         
  mpz_set(rslt->value.integer, s0->value.integer);        
        
  if ((r = g95_check_integer_range(rslt->value.integer, kind))     
      != ARITH_OK) {         
    arith_error(r, &s0->ts, &rslt->ts, &s0->where);     
    g95_free_expr(rslt);      
    return NULL;      
  }  
  
  return rslt;          
}       
       
       


/* g95_range_check()-- Make sure a constant numeric expression is
 * within the range for it's type and kind.
 * Note that there's also a g95_check_range(), but that one deals
 * with the intrinsic RANGE function. */          
          
arith g95_range_check(g95_expr *g) {     
arith rv;      
      
  switch(g->ts.type) {  
  case BT_INTEGER:     
    rv = g95_check_integer_range(g->value.integer, g->ts.kind);  
    break;       
       
  case BT_REAL:  
    rv = g95_check_real_range(g->value.real, g->ts.kind);  
    break;   
   
  case BT_COMPLEX:      
    rv = g95_check_real_range(g->value.complex.r, g->ts.kind);         
    if (rv != ARITH_OK)   
      rv = g95_check_real_range(g->value.complex.i, g->ts.kind);        
        
    break;        
        
  default:     
    g95_internal_error("g95_range_check(): Bad type"); 
  }     
     
  return rv;         
}    
    
    
  
  
static arith reduce_binary_ca(arith (*eval)(), g95_expr *op1, g95_expr *op,        
			      g95_expr **result) {  
g95_constructor *s, *head;       
g95_expr *a;
arith rc;         
         
  head = g95_copy_constructor(op->value.constructor);      
  rc = ARITH_OK;

  for(s=head; s; s=s->next) {  
    rc = eval(op1, s->expr, &a);   
    if (rc != ARITH_OK) break;          
          
    g95_replace_expr(s->expr, a);          
  }     
     
  if (rc != ARITH_OK)     
    g95_free_constructor(head);    
  else {         
    a = g95_get_expr(); 
    a->type = EXPR_ARRAY;        
    a->value.constructor = head;        
    a->shape = g95_copy_shape(op->shape, op->rank);  
  
    a->ts = head->expr->ts;   
    a->where = op->where;  
    a->rank = op->rank;        
        
    *result = a;         
  }      
      
  return rc;   
}          
          
          
          
static arith reduce_binary(arith (*eval)(), g95_expr *op1, g95_expr *op0,        
			   g95_expr **res) {     
     
  if (op1->type == EXPR_CONSTANT && op0->type == EXPR_CONSTANT)  
    return eval(op1, op0, res);        
        
  if (op1->type == EXPR_CONSTANT && op0->type == EXPR_ARRAY)          
    return reduce_binary_ca(eval, op1, op0, res);      
      
  if (op1->type == EXPR_ARRAY && op0->type == EXPR_CONSTANT)         
    return reduce_binary_ac(eval, op1, op0, res);          
          
  return reduce_binary_aa(eval, op1, op0, res);
}


       
       
/* g95_real2complex()-- Convert real to complex. */      
      
g95_expr *g95_real2complex(g95_expr *src, int knd) {         
g95_expr *res;     
arith retval;          
          
  res = g95_constant_result(BT_COMPLEX, knd, &src->where);       
       
  mpf_set(res->value.complex.r, src->value.real);    
  mpf_set_ui(res->value.complex.i, 0);  
  
  if ((retval = g95_check_real_range(res->value.complex.i, knd)) != ARITH_OK) { 
    arith_error(retval, &src->ts, &res->ts, &src->where);          
    g95_free_expr(res);
    return NULL; 
  }  
  
  return res; 
}         
         
         
   
   
/* High level arithmetic subroutines.  These subroutines go into
 * eval_intrinsic(), which can do one of several things to its
 * operands.  If the operands are incompatible with the intrinsic
 * operation, we return a node pointing to the operands and hope that
 * an operator interface is found during resolution.
 *
 * If the operands are compatible and are constants, then we try doing
 * the arithmetic.  We also handle the cases where either or both
 * operands are array constructors. */        
        
static g95_expr *eval_intrinsic(g95_intrinsic_op oper,     
				arith (*eval)(),       
				g95_expr *op0, g95_expr *op_2) { 
g95_expr t0, *result;          
int unary;       
arith rv; 
 
  g95_clear_ts(&t0.ts);      
      
  switch(oper) {        
  case INTRINSIC_NOT:    /* Logical unary */  
    if (op0->ts.type != BT_LOGICAL) goto runtime;       
       
    t0.ts.type = BT_LOGICAL;    
    t0.ts.kind = g95_default_logical_kind();        
        
    unary = 1;   
    break;  
  
    /* Logical binary operators */      
  case INTRINSIC_OR:     case INTRINSIC_AND: 
  case INTRINSIC_NEQV:   case INTRINSIC_EQV:     
    if (op0->ts.type != BT_LOGICAL || op_2->ts.type != BT_LOGICAL)   
      goto runtime;   
   
    t0.ts.type = BT_LOGICAL;   
    t0.ts.kind = g95_default_logical_kind();       
       
    unary = 0;   
    break; 
 
  case INTRINSIC_UPLUS:   case INTRINSIC_UMINUS:  /* Numeric unary */   
    if (!g95_numeric_ts(&op0->ts)) goto runtime;

    t0.ts = op0->ts;         
         
    unary = 1;   
    break;     
     
  case INTRINSIC_GE:  case INTRINSIC_LT:  /* Additional restrictions */ 
  case INTRINSIC_LE:  case INTRINSIC_GT:  /* for ordering relations */    
    if (op0->ts.type == BT_COMPLEX || op_2->ts.type == BT_COMPLEX) {  
      t0.ts.type = BT_LOGICAL;        
      t0.ts.kind = g95_default_logical_kind();  
      goto runtime;
    }  
  
    /* Fall through */  
  
  case INTRINSIC_EQ:      case INTRINSIC_NE:   
    if (op0->ts.type == BT_CHARACTER && op_2->ts.type == BT_CHARACTER) {         
      unary = 0;     
      t0.ts.type = BT_LOGICAL;      
      t0.ts.kind = g95_default_logical_kind();       
      break;    
    }         
         
    /* Fall through */  
  
  case INTRINSIC_PLUS:    case INTRINSIC_MINUS:   case INTRINSIC_TIMES:      
  case INTRINSIC_DIVIDE:  case INTRINSIC_POWER:   /* Numeric binary */          
    if (!g95_numeric_ts(&op0->ts) || !g95_numeric_ts(&op_2->ts)) 
      goto runtime;       
       
    /* Insert any necessary type conversions to make the operands compatible */          
          
    t0.type = EXPR_OP;     
    g95_clear_ts(&t0.ts);   
    t0.operator = oper;     
     
    t0.op1 = op0;          
    t0.op2 = op_2;        
        
    g95_type_convert_binary(&t0);   
   
    if (oper == INTRINSIC_EQ || oper == INTRINSIC_NE ||       
	oper == INTRINSIC_GE || oper == INTRINSIC_GT ||          
	oper == INTRINSIC_LE || oper == INTRINSIC_LT) {      
      t0.ts.type = BT_LOGICAL;        
      t0.ts.kind = g95_default_logical_kind();        
    }       
       
    if (oper == INTRINSIC_DIVIDE && op_2->type == EXPR_CONSTANT &&      
	((op_2->ts.type == BT_INTEGER && 
	  mpz_cmp_ui(op_2->value.integer, 0) == 0) ||   
	 (op_2->ts.type == BT_REAL && 
	  mpf_cmp_ui(op_2->value.real, 0) == 0) ||          
	 (op_2->ts.type == BT_COMPLEX &&     
	  mpf_cmp_ui(op_2->value.complex.r, 0) == 0 &&    
	  mpf_cmp_ui(op_2->value.complex.i, 0) == 0)))        
      goto runtime;         
         
    unary = 0;       
    break;         
         
  case INTRINSIC_CONCAT:   /* Character binary */   
    if (op0->ts.type != BT_CHARACTER || op_2->ts.type != BT_CHARACTER)
      goto runtime;       
       
    t0.ts.type = BT_CHARACTER;       
    t0.ts.kind = g95_default_character_kind();  
  
    unary = 0;     
    break;    
    
  case INTRINSIC_USER: 
    goto runtime;        
        
  default:
    g95_internal_error("eval_intrinsic(): Bad operator"); 
  }         
         
  /* Try to combine the operators */        
        
  if (oper == INTRINSIC_POWER && op_2->ts.type != BT_INTEGER)
    goto runtime;    
    
  if (op0->type != EXPR_CONSTANT &&         
      (op0->type != EXPR_ARRAY || !g95_is_constant_expr(op0) ||     
       !g95_expanded_ac(op0)))       
    goto runtime;    
    
  if (op_2 != NULL && op_2->type != EXPR_CONSTANT &&      
      (op_2->type != EXPR_ARRAY || !g95_is_constant_expr(op_2) ||       
       !g95_expanded_ac(op_2))) 
    goto runtime;          
          
  if (unary)   
    rv = reduce_unary(eval, op0, &result);       
  else 
    rv = reduce_binary(eval, op0, op_2, &result);

  if (rv != ARITH_OK) {     /* Something went wrong */     
    g95_error("%s at %L", g95_arith_error(rv), &op0->where); 
    return NULL;        
  }  
  
  g95_free_expr(op0);    
  g95_free_expr(op_2);     
  return result;   
   
  /* Create a run-time expression */

runtime:      
  result = g95_get_expr();  
  result->ts = t0.ts;

  result->type = EXPR_OP;     
  result->operator = oper;         
         
  result->op1 = op0;       
  result->op2 = op_2;         
         
  result->where = op0->where;   
   
  return result;  
}         
         
         
     
     
static g95_expr *eval_intrinsic_f2(g95_intrinsic_op operator,       
				   arith (*eval)(g95_expr *, g95_expr **),          
				   g95_expr *op1, g95_expr *op_2) {   
g95_expr *res;       
       
  if (op_2 == NULL) {      
    if (g95_zero_size_array(op1)) return eval_type_intrinsic0(operator, op1);    
  } else {    
    res = reduce_binary0(op1, op_2);         
    if (res != NULL) return eval_type_intrinsic0(operator, res);
  }         
         
  return eval_intrinsic(operator, eval, op1, op_2);        
}        
        
        
        
        
g95_expr *g95_not(g95_expr *op_1) {  
  return eval_intrinsic_f2(INTRINSIC_NOT, g95_arith_not, op_1, NULL);         
}          
          
   
   
static g95_expr *eval_intrinsic_f3(g95_intrinsic_op o,         
				   arith (*eval)(g95_expr *, g95_expr *,       
						 g95_expr **),      
				   g95_expr *op0, g95_expr *op) {    
g95_expr *result;

  result = reduce_binary0(op0, op);         
  if (result != NULL) return eval_type_intrinsic0(o, result);    
    
  return eval_intrinsic(o, eval, op0, op);    
}        
        
        


g95_expr *g95_gt(g95_expr *op0, g95_expr *op) {     
  return eval_intrinsic_f3(INTRINSIC_GT, g95_arith_gt, op0, op);         
}        
        
  
  
g95_expr *g95_ne(g95_expr *op_1, g95_expr *op2) {    
  return eval_intrinsic_f3(INTRINSIC_NE, g95_arith_ne, op_1, op2); 
}       
       
  
  
g95_expr *g95_and(g95_expr *op1, g95_expr *op_2) {  
  return eval_intrinsic_f3(INTRINSIC_AND, g95_arith_and, op1, op_2);      
}          
          
        
        
g95_expr *g95_multiply(g95_expr *op1, g95_expr *op) {          
  return eval_intrinsic_f3(INTRINSIC_TIMES, g95_arith_times, op1, op);        
} 
 
    
    
g95_expr *g95_concat(g95_expr *op1, g95_expr *op) {     
  return eval_intrinsic_f3(INTRINSIC_CONCAT, g95_arith_concat, op1, op);    
}      
      
        
        
g95_expr *g95_lt(g95_expr *op_1, g95_expr *op0) {     
  return eval_intrinsic_f3(INTRINSIC_LT, g95_arith_lt, op_1, op0);  
}        
        
   
   
g95_expr *g95_le(g95_expr *op, g95_expr *op_2) { 
  return eval_intrinsic_f3(INTRINSIC_LE, g95_arith_le, op, op_2);   
}      
      
    
    
g95_expr *g95_or(g95_expr *op0, g95_expr *op2) {      
  return eval_intrinsic_f3(INTRINSIC_OR, g95_arith_or, op0, op2);      
}          
          
   
   
g95_expr *g95_eqv(g95_expr *op_1, g95_expr *op) {
  return eval_intrinsic_f3(INTRINSIC_EQV, g95_arith_eqv, op_1, op);      
}        
        
 
 
g95_expr *g95_divide(g95_expr *op_1, g95_expr *op0) {          
  return eval_intrinsic_f3(INTRINSIC_DIVIDE, g95_arith_divide, op_1, op0);        
}   
   
          
          
g95_expr *g95_uplus(g95_expr *op1) {      
  return eval_intrinsic_f2(INTRINSIC_UPLUS, g95_arith_uplus, op1, NULL);         
}   
   
      
      
g95_expr *g95_add(g95_expr *op0, g95_expr *op_2) {  
  return eval_intrinsic_f3(INTRINSIC_PLUS, g95_arith_plus, op0, op_2); 
}     
     
          
          
g95_expr *g95_power(g95_expr *op1, g95_expr *op2) {        
  return eval_intrinsic_f3(INTRINSIC_POWER, g95_arith_power, op1, op2);    
}       
       
 
 
g95_expr *g95_neqv(g95_expr *op1, g95_expr *op2) {       
  return eval_intrinsic_f3(INTRINSIC_NEQV, g95_arith_neqv, op1, op2);        
}        
         
         
g95_expr *g95_uminus(g95_expr *op) {     
  return eval_intrinsic_f2(INTRINSIC_UMINUS, g95_arith_uminus, op, NULL);         
}        
        
    
    
g95_expr *g95_ge(g95_expr *op, g95_expr *op_2) {          
  return eval_intrinsic_f3(INTRINSIC_GE, g95_arith_ge, op, op_2);        
}       
       
          
          
g95_expr *g95_eq(g95_expr *op1, g95_expr *op0) {       
  return eval_intrinsic_f3(INTRINSIC_EQ, g95_arith_eq, op1, op0);       
}       
       


g95_expr *g95_subtract(g95_expr *op1, g95_expr *op) {   
  return eval_intrinsic_f3(INTRINSIC_MINUS, g95_arith_minus, op1, op); 
}   
   
