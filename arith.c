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
  { 8,  64 },          
  { 0,   0 }  
};    
    
g95_real_info g95_real_kinds[] = {  
  { 4,  2,  24,  -125,  128 },         
  { 8,  2,  53, -1021, 1024 },    
  { 0,  0,   0,     0,    0 }         
};    
    
    
     
     
/* complex_reciprocal()-- Compute the reciprocal of a complex number
 * (guaranteed nonzero) */  
  
static void complex_reciprocal(g95_expr *op) {     
mpf_t mod, b, result_r, result_i;

  mpf_init(mod); 
  mpf_init(b);

  mpf_mul(mod, op->value.complex.r, op->value.complex.r);        
  mpf_mul(b, op->value.complex.i, op->value.complex.i);    
  mpf_add(mod, mod, b);      
      
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
  mpf_clear(b);
}         
         
         
  
  
arith g95_arith_le(g95_expr *op0, g95_expr *op2, g95_expr **r) {        
g95_expr *result;     
     
  result = g95_constant_result(BT_LOGICAL, g95_default_logical_kind(),          
			       &op0->where);    
  result->value.logical = (g95_compare_expr(op0, op2) <= 0);  
  *r = result;        
        
  return ARITH_OK;          
}     
     
        
        
/* g95_arith_init_1()-- Get things ready to do math. */

void g95_arith_init_1(void) {       
g95_integer_info *int_info;    
g95_real_info *real_info;
mpf_t a, o;       
mpz_t u;  
int f, j, l;    
    
/* Set the default precision for GMP computations */
  mpf_set_default_prec(G95_REAL_BITS+30);  
  
/* Calculate e, needed by the natural_logarithm() subroutine. */   
   
  mpf_init(o);     
  mpf_init_set_ui(e_value, 0); 
  mpf_init_set_ui(a, 1);  
  
  for(f=1; f<100; f++) {  
    mpf_add(e_value, e_value, a);      
    mpf_div_ui(a, a, f);   /* 1/(i!) */    
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
          
  l = (G95_REAL_BITS / 4) + 10;  /* (1/16)^n gives 4 bits per iteration */     
     
  for(j=0; j<l; j++) {  
    mpf_set_ui(o, 4);          
    mpf_div_ui(o, o, 8*j+1);  /* 4/(8n+1) */   
   
    mpf_set_ui(a, 2);   
    mpf_div_ui(a, a, 8*j+4);  /* 2/(8n+4) */  
    mpf_sub(o, o, a); 
 
    mpf_set_ui(a, 1);          
    mpf_div_ui(a, a, 8*j+5);  /* 1/(8n+5) */     
    mpf_sub(o, o, a);     
     
    mpf_set_ui(a, 1);    
    mpf_div_ui(a, a, 8*j+6);  /* 1/(8n+6) */      
    mpf_sub(o, o, a);        
        
    mpf_set_ui(a, 16);  
    mpf_pow_ui(a, a, j);      /* 16^n */     
     
    mpf_div(o, o, a);        
        
    mpf_add(pi, pi, o);         
  }   
   
  mpf_mul_ui(two_pi, pi, 2);      
  mpf_div_ui(half_pi, pi, 2);      
      
/* Convert the minimum/maximum values for each kind into their Gnu MP
 * representation. */    
    
  mpz_init(u);      
      
  for(int_info=g95_integer_kinds; int_info->kind != 0; int_info++) {   
    /* Huge */          
          
    mpz_set_ui(u, int_info->radix);       
    mpz_pow_ui(u, u, int_info->digits);   
   
    mpz_init(int_info->huge);        
    mpz_sub_ui(int_info->huge, u, 1);    
    
    /* These are the numbers that are actually representable by the
     * target.  For bases other than two, this needs to be changed. */   
   
    if (int_info->radix != 2) 
      g95_internal_error("Fix min_int, max_int calculation");          
          
    mpz_init(int_info->min_int);   
    mpz_neg(int_info->min_int, int_info->huge);   
    /* No -1 here, because the representation is symmetric */      
      
    mpz_init(int_info->max_int);       
    mpz_add(int_info->max_int, int_info->huge, int_info->huge);           
    mpz_add_ui(int_info->max_int, int_info->max_int, 1);     
     
    /* Range */      
      
    mpf_set_z(a, int_info->huge);
    common_logarithm(&a, &a);         
    mpf_trunc(a, a);         
    mpz_set_f(u, a);      
    int_info->range = mpz_get_si(u);    
  }  
  
/*  mpf_set_default_prec(G95_REAL_BITS); */        
        
  for(real_info=g95_real_kinds; real_info->kind != 0; real_info++) {        
    /* Huge */  
  
    mpf_set_ui(a, real_info->radix);         
    mpf_set_ui(o, real_info->radix);      
      
    mpf_pow_ui(a, a, real_info->max_exponent);        
    mpf_pow_ui(o, o, real_info->max_exponent - real_info->digits);    
    
    mpf_init(real_info->huge);  
    mpf_sub(real_info->huge, a, o);       
       
    /* Tiny */     
     
    mpf_set_ui(o, real_info->radix);    
    mpf_pow_ui(o, o, 1-real_info->min_exponent);  
  
    mpf_init(real_info->tiny);       
    mpf_ui_div(real_info->tiny, 1, o);  
  
    /* Epsilon */        
        
    mpf_set_ui(o, real_info->radix);   
    mpf_pow_ui(o, o, real_info->digits - 1);     
     
    mpf_init(real_info->epsilon); 
    mpf_ui_div(real_info->epsilon, 1, o);     
     
    /* Range */ 
 
    common_logarithm(&real_info->huge, &a);   
    common_logarithm(&real_info->tiny, &o);      
    mpf_neg(o, o);        
        
    if (mpf_cmp(a, o) > 0) mpf_set(a, o);  /* a = min(a, b) */       
       
    mpf_trunc(a, a);          
    mpz_set_f(u, a);
    real_info->range = mpz_get_si(u);

    /* Precision */    
    
    mpf_set_ui(a, real_info->radix);  
    common_logarithm(&a, &a);   
   
    mpf_mul_ui(a, a, real_info->digits-1);      
    mpf_trunc(a, a);     
    mpz_set_f(u, a);
    real_info->precision = mpz_get_si(u);       
       
    /* If the radix is an integral power of 10, add one to the precision. */    
    
    for(f=10; f<=real_info->radix; f*=10)         
      if (f == real_info->radix) real_info->precision++;          
  }       
       
  mpz_clear(u);       
  mpf_clear(a);      
  mpf_clear(o);
}    
    
    
         
         
/* g95_convert_integer()-- Convert an integer string to an expression
 * node */   
   
g95_expr *g95_convert_integer(char *buf, int kind, int radix,     
			      locus *pos) {          
g95_expr *r;         
         
  r = g95_constant_result(BT_INTEGER, kind, pos);         
  if (buf[0] == '+') buf++;  
  mpz_set_str(r->value.integer, buf, radix);        
        
  return r;        
}     
     
     
 
 
static int validate_real(int k0) {
int x;      
      
  for(x=0;; x++) {  
    if (g95_real_kinds[x].kind == 0) { x = -1; break; }       
    if (g95_real_kinds[x].kind == k0) break;     
  }     
     
  return x;  
}  
  
  
       
       
/* g95_arith_uplus()-- It may seem silly to have a subroutine that
 * actually computes the unary plus of a constant, but it prevents us
 * from making exceptions in the code elsewhere. */   
   
arith g95_arith_uplus(g95_expr *op0, g95_expr **r) {    
    
  *r = g95_copy_expr(op0); 
  return ARITH_OK;      
}     
     
     
   
   
/* validate_integer()-- Make sure that a valid kind is present.
 * Returns an index into the g95_integer_kinds array, -1 if the kind
 * is not present. */       
       
static int validate_integer(int kind) {     
int r; 
 
  for(r=0;; r++) {      
    if (g95_integer_kinds[r].kind == 0) { r = -1; break; }         
    if (g95_integer_kinds[r].kind == kind) break;       
  } 
 
  return r;   
}       
       
       
          
          
/* g95_constant_result()-- Function to return a constant expression node
 * of a given type and kind. */          
          
g95_expr *g95_constant_result(bt dtype, int k0, locus *pos) {    
g95_expr *rslt;   
   
  if (! pos)        
    g95_internal_error("g95_constant_result(): locus 'where' cannot be NULL");        
        
  rslt = g95_get_expr();  
  
  rslt->type = EXPR_CONSTANT; 
  rslt->ts.type = dtype;        
  rslt->ts.kind = k0;
  rslt->where = *pos;          
          
  switch(dtype) {      
  case BT_INTEGER:     
    mpz_init(rslt->value.integer);     
    break;        
        
  case BT_REAL:      
    mpf_init(rslt->value.real);       
    break;         
         
  case BT_COMPLEX:      
    mpf_init(rslt->value.complex.r);  
    mpf_init(rslt->value.complex.i); 
    break;    
    
  default: 
    break; 
  } 
 
  return rslt;   
}   
   
   
        
        
int g95_default_logical_kind(void) {       
       
  return g95_logical_kinds[g95_option.i8 ? 1 : 0].kind;          
}  
  
    
/******* Simplification of intrinsic functions with constant arguments *****/  
  
/* arith_error()-- Deal with an arithmetic error. */         
         
static void arith_error(arith retval, g95_typespec *t, g95_typespec *dest,      
			locus *w) {       
       
  g95_error("%s converting %s to %s at %L", g95_arith_error(retval),        
	    g95_typename(t), g95_typename(dest), w);  
  
  /* TODO: Do something about the error, ie underflow rounds to 0,
   * throw exception, return NaN, etc. */          
}  
  
  
         
         
int g95_default_character_kind(void) {

  return 1;   
}       
       
   
   
arith g95_arith_uminus(g95_expr *op, g95_expr **resp) {     
g95_expr *rslt;       
arith rv;   
   
  rslt = g95_constant_result(op->ts.type, op->ts.kind, &op->where);   
   
  switch(op->ts.type) {      
  case BT_INTEGER: 
    mpz_neg(rslt->value.integer, op->value.integer);   
    break;      
      
  case BT_REAL:      
    mpf_neg(rslt->value.real, op->value.real); 
    break;         
         
  case BT_COMPLEX:   
    mpf_neg(rslt->value.complex.r, op->value.complex.r);       
    mpf_neg(rslt->value.complex.i, op->value.complex.i);          
    break;        
        
  default:      
    g95_internal_error("g95_arith_uminus(): Bad basic type");  
  }          
          
  rv = g95_range_check(rslt);     
     
  if (rv != ARITH_OK)  
    g95_free_expr(rslt);  
  else        
    *resp = rslt;      
      
  return rv;
}         
         
         
          
          
int g95_default_double_kind(void) {   
   
  return g95_real_kinds[1].kind;
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
          
       
       
arith g95_arith_times(g95_expr *op1, g95_expr *op0, g95_expr **resp) {  
g95_expr *res;     
mpf_t g, k;         
arith rv;        
        
  res = g95_constant_result(op1->ts.type, op1->ts.kind, &op1->where);  
  
  switch(op1->ts.type) {     
  case BT_INTEGER:       
    mpz_mul(res->value.integer, op1->value.integer, op0->value.integer);        
    break;       
       
  case BT_REAL:      
    mpf_mul(res->value.real, op1->value.real, op0->value.real);   
    break;      
      
  case BT_COMPLEX:          
    mpf_init(g);        
    mpf_init(k);       
       
    mpf_mul(g, op1->value.complex.r, op0->value.complex.r);     
    mpf_mul(k, op1->value.complex.i, op0->value.complex.i);         
    mpf_sub(res->value.complex.r, g, k);         
         
    mpf_mul(g, op1->value.complex.r, op0->value.complex.i);          
    mpf_mul(k, op1->value.complex.i, op0->value.complex.r);   
    mpf_add(res->value.complex.i, g, k);    
    
    mpf_clear(g);  
    mpf_clear(k);  
  
    break;          
          
  default:   
    g95_internal_error("g95_arith_times(): Bad basic type");        
  } 
 
  rv = g95_range_check(res);     
     
  if (rv != ARITH_OK)         
    g95_free_expr(res); 
  else   
    *resp = res;    
    
  return rv;  
}          
          
          
         
         
/* hyperbolic cosine */         
         
void hypercos(mpf_t *a, mpf_t *res) {     
mpf_t neg, term1, term2, j, xp;

  mpf_init_set(j, *a);         
         
  mpf_init(neg);       
  mpf_init(term1);      
  mpf_init(term2);  
  mpf_init(xp); 
 
  mpf_neg(neg, j);        
        
  exponential(&j, &term1);
  exponential(&neg, &term2);    
    
  mpf_add(xp, term1, term2);         
  mpf_div_ui(*res, xp, 2);          
          
  mpf_clear(neg);        
  mpf_clear(term1);
  mpf_clear(term2); 
  mpf_clear(j); 
  mpf_clear(xp);        
}         
         
         
void hypersine(mpf_t *a, mpf_t *res) {       
mpf_t neg, term1, term2, j, xp;          
          
  mpf_init_set(j, *a);

  mpf_init(neg);   
  mpf_init(term1);        
  mpf_init(term2);     
  mpf_init(xp);       
       
  mpf_neg(neg, j);        
        
  exponential(&j, &term1);   
  exponential(&neg, &term2);        
        
  mpf_sub(xp, term1, term2);  
  mpf_div_ui(*res, xp, 2);        
        
  mpf_clear(neg);  
  mpf_clear(term1);     
  mpf_clear(term2);
  mpf_clear(j);
  mpf_clear(xp);    
}          
          
          
 
 
void sine(mpf_t *a, mpf_t *rslt) { 
mpf_t factor, z, p, num, d, t, b, xp;       
int v, sign;          
          
/* We use a reduction of the form
 *   x= N*2pi + r
 *   then obtain sin(r) from the McLaurin series. */     
     
  mpf_init_set(b, *a);  
  
/* Special case (we do not treat multiples of pi due to roundoff issues) */     
  if (mpf_cmp_ui(b,0) ==  0) {
    mpf_set_ui(*rslt,0);  
  } else { 
    mpf_init(z);   
    mpf_init(p);    
    mpf_init(factor);    
    mpf_init(t);       
       
    mpf_div(z, b, two_pi); 
    mpf_floor(factor, z);
    mpf_mul(z, factor, two_pi);     
    mpf_sub(p, b, z);       
       
    mpf_init_set_ui(xp, 0);        
    mpf_init_set_ui(num, 1);      
    mpf_init_set_ui(d, 1);   
   
    sign = -1;    
    for(v=1; v<G95_REAL_BITS+10; v++) {  
      mpf_mul(num, num, p);      
      mpf_mul_ui(d, d, v);   
      if (v%2 == 0) continue;  
  
      sign = -sign;          
      mpf_div(t, num, d);  
      if (sign > 0)
	mpf_add(xp, xp, t);    
      else        
	mpf_sub(xp,xp,t);      
    }          
          
    mpf_set(*rslt, xp);    
    
    mpf_clear(z);    
    mpf_clear(p);
    mpf_clear(factor);     
    mpf_clear(num);       
    mpf_clear(d);        
    mpf_clear(t);          
    mpf_clear(xp);        
  }  
  
  mpf_clear(b);
}     
     
     
  
  
void exponential(mpf_t *ap, mpf_t *res) {          
mpf_t two, ln2, power, l, b, num, d, t, c, xp;    
int i;
long h;
unsigned long j, mp;         
         
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
    mpf_init(l); 
    mpf_init(b);   
    mpf_init(power);          
    mpf_init(t);    
    
    natural_logarithm(&two, &ln2);       
       
    mpf_div(l, c, ln2);    
    mpf_floor(power, l); 
    mpf_mul(l, power, ln2);          
    mpf_sub(b, c, l);     
     
    mpf_init_set_ui(xp, 1);  
    mpf_init_set_ui(num, 1);        
    mpf_init_set_ui(d, 1);

    for(i=1; i<=G95_REAL_BITS+10; i++) {
      mpf_mul(num, num, b);          
      mpf_mul_ui(d, d, i); 
      mpf_div(t, num, d);         
      mpf_add(xp, xp, t);       
    }        
        
    /* Reconstruction step */       
    h = (long) mpf_get_d(power);          
          
    if (h > 0) {    
      j = (unsigned int) h;     
      mpf_mul_2exp(*res,xp,j);      
    } else {    
      mp = (unsigned int) (-h);   
      mpf_div_2exp(*res,xp,mp);   
    }      
      
    mpf_clear(two);  
    mpf_clear(ln2);   
    mpf_clear(l);
    mpf_clear(b);        
    mpf_clear(power);  
    mpf_clear(num);     
    mpf_clear(d);      
    mpf_clear(t);
    mpf_clear(xp); 
  }  
  
  mpf_clear(c);    
}    
    
    
 
 
static arith reduce_binary_aa(arith (*eval)(), g95_expr *op0, g95_expr *op2,
			      g95_expr **result) { 
g95_constructor *c, *n, *head;      
g95_expr *e;       
arith rc;   
   
  head = g95_copy_constructor(op0->value.constructor);   
   
  rc = ARITH_OK;   
  n = op2->value.constructor;          
          
  if (g95_check_conformance("Elemental binary operation", op0, op2)        
      != SUCCESS)  
    rc = ARITH_INCOMMENSURATE;        
  else {        
        
    for(c=head; c; c=c->next, n=n->next) {          
      if (n == NULL) {       
	rc = ARITH_INCOMMENSURATE;
	break;       
      }  
  
      rc = eval(c->expr, n->expr, &e);         
      if (rc != ARITH_OK) break;   
         
      g95_replace_expr(c->expr, e); 
    }   
   
    if (n != NULL) rc = ARITH_INCOMMENSURATE;     
  }       
       
  if (rc != ARITH_OK)        
    g95_free_constructor(head);         
  else {    
    e = g95_get_expr();  
    e->type = EXPR_ARRAY;          
    e->value.constructor = head;   
    e->shape = g95_copy_shape(op0->shape, op0->rank);      
      
    e->ts = head->expr->ts;
    e->where = op0->where;    
    e->rank = op0->rank;         
         
    *result = e;    
  }        
        
  return rc;       
}  
  
  
       
       
/* g95_compare_expr()-- Comparison operators.  Assumes that the two
 * expression nodes contain two constants of the same type. */  
  
int g95_compare_expr(g95_expr *op, g95_expr *op2) {  
int retval;          
          
  switch(op->ts.type) {        
  case BT_INTEGER:
    retval = mpz_cmp(op->value.integer, op2->value.integer);  
    break;

  case BT_REAL:      
    retval = mpf_cmp(op->value.real, op2->value.real);     
    break;  
  
  case BT_CHARACTER:       
    retval = g95_compare_string(op, op2, NULL);       
    break;          
          
  case BT_LOGICAL: 
    retval = (!!op->value.logical) - (!!op2->value.logical); 
 
    if (retval < 0) retval = -1;       
    if (retval > 0) retval = 1;   
    break;     
     
  default: g95_internal_error("g95_compare_expr(): Bad basic type");      
  }      
      
  return retval;
}   
   
   
         
         
/* g95_convert_real()-- Convert a real string to an expression node. */         
         
g95_expr *g95_convert_real(char *b, int k, locus *old_loc) {
g95_expr *v;       
       
  v = g95_constant_result(BT_REAL, k, old_loc);         
  mpf_set_str(v->value.real, b, 10);

  return v; 
}     
     
     
     
     
/* g95_zero_size_array()-- Return nonzero if the expression is a zero
 * size array. */

int g95_zero_size_array(g95_expr *r) {        
        
  if (r->type != EXPR_ARRAY) return 0;          
          
  return r->value.constructor == NULL;   
}          
          
          
       
       
arith g95_arith_minus(g95_expr *op_1, g95_expr *op0, g95_expr **resultp) {       
g95_expr *result;   
arith retval;       
       
  result = g95_constant_result(op_1->ts.type, op_1->ts.kind, &op_1->where);   
   
  switch(op_1->ts.type) {      
  case BT_INTEGER:     
    mpz_sub(result->value.integer, op_1->value.integer, op0->value.integer); 
    break;          
          
  case BT_REAL:
    mpf_sub(result->value.real, op_1->value.real, op0->value.real); 
    break;    
    
  case BT_COMPLEX:          
    mpf_sub(result->value.complex.r, op_1->value.complex.r,  
	    op0->value.complex.r);         
         
    mpf_sub(result->value.complex.i, op_1->value.complex.i,        
	    op0->value.complex.i);     
     
    break;        
        
  default:         
    g95_internal_error("g95_arith_minus(): Bad basic type");  
  }     
     
  retval = g95_range_check(result);         
         
  if (retval != ARITH_OK)  
    g95_free_expr(result);        
  else 
    *resultp = result; 
 
  return retval;
} 
 
 
       
       
int g95_default_integer_kind(void) { 
 
  return g95_integer_kinds[g95_option.i8 ? 1 : 0].kind;        
}         
         


arith g95_arith_gt(g95_expr *op0, g95_expr *op2, g95_expr **rp) {      
g95_expr *rslt;    
    
  rslt = g95_constant_result(BT_LOGICAL, g95_default_logical_kind(),
			       &op0->where);      
  rslt->value.logical = (g95_compare_expr(op0, op2) > 0);        
  *rp = rslt;    
    
  return ARITH_OK;     
}  
  
  
  
/* g95_check_integer_range()-- Given an integer and a kind, make sure
 * that the integer lies within the range of the kind.  Returns
 * ARITH_OK or ARITH_OVERFLOW. */

static arith g95_check_integer_range(mpz_t j, int k0) {     
arith res;
int x;     
     
  x = validate_integer(k0);   
  if (x == -1) g95_internal_error("g95_check_integer_range(): Bad kind");       
       
  res = ARITH_OK;          
          
  if (mpz_cmp(j, g95_integer_kinds[x].min_int) < 0 || 
      mpz_cmp(j, g95_integer_kinds[x].max_int) > 0) res = ARITH_OVERFLOW;        
        
  return res;       
}        
        
        
     
     
/* g95_int2int()-- Convert integers to integers */ 
 
g95_expr *g95_int2int(g95_expr *src, int kind) {   
g95_expr *r;  
arith rc;    
    
  r = g95_constant_result(BT_INTEGER, kind, &src->where);

  mpz_set(r->value.integer, src->value.integer);     
     
  if ((rc = g95_check_integer_range(r->value.integer, kind))     
      != ARITH_OK) {      
    arith_error(rc, &src->ts, &r->ts, &src->where); 
    g95_free_expr(r);
    return NULL;   
  }         
         
  return r;       
}   
   
   
      
      
/* compare_complex()-- Compare a pair of complex numbers.  Naturally,
 * this is only for equality/nonequality. */       
       
static int compare_complex(g95_expr *op1, g95_expr *op) {

  return (mpf_cmp(op1->value.complex.r, op->value.complex.r) == 0 && 
	  mpf_cmp(op1->value.complex.i, op->value.complex.i) == 0);         
}       
       
       
      
      
/* g95_real2int()-- Convert default real to default integer */          
          
g95_expr *g95_real2int(g95_expr *source, int knd) {        
g95_expr *result;  
arith r;        
        
  result = g95_constant_result(BT_INTEGER, knd, &source->where);  
  
  mpz_set_f(result->value.integer, source->value.real);    
    
  if ((r = g95_check_integer_range(result->value.integer, knd))   
      != ARITH_OK) {         
    arith_error(r, &source->ts, &result->ts, &source->where);       
    g95_free_expr(result);    
    return NULL;     
  }    
    
  return result;
}         
         
         
  
  
/* complex_pow_ui()-- Raise a complex number to positive power */     
     
static void complex_pow_ui(g95_expr *base, int power, g95_expr *res) {        
mpf_t temp_r, temp_i, d;          
          
  mpf_set_ui(res->value.complex.r, 1);       
  mpf_set_ui(res->value.complex.i, 0);  
  
  mpf_init(temp_r);        
  mpf_init(temp_i); 
  mpf_init(d);      
      
  for(; power>0; power--) {       
    mpf_mul(temp_r, base->value.complex.r, res->value.complex.r);      
    mpf_mul(d,      base->value.complex.i, res->value.complex.i); 
    mpf_sub(temp_r, temp_r, d);    
    
    mpf_mul(temp_i, base->value.complex.r, res->value.complex.i);      
    mpf_mul(d,      base->value.complex.i, res->value.complex.r);  
    mpf_add(temp_i, temp_i, d);   
   
    mpf_set(res->value.complex.r, temp_r);          
    mpf_set(res->value.complex.i, temp_i);   
  }        
        
  mpf_clear(temp_r);  
  mpf_clear(temp_i);  
  mpf_clear(d);          
}    
    
    
  
  
int g95_default_complex_kind(void) {          
          
  return g95_default_real_kind();   
}    
    
    
  
  
/* eval_type_intrinsic0() -- modify type of expression for zero size array */       
       
static g95_expr *eval_type_intrinsic0(g95_intrinsic_op operator, g95_expr *op1){ 
 
  if (op1 == NULL) g95_internal_error("eval_type_intrinsic0(): op NULL");      
      
  switch(operator) {      
  case INTRINSIC_GE:  case INTRINSIC_LT:         
  case INTRINSIC_LE:  case INTRINSIC_GT:       
  case INTRINSIC_EQ:  case INTRINSIC_NE:
    op1->ts.type = BT_LOGICAL;        
    op1->ts.kind = g95_default_logical_kind();    
    break;       
       
  default:   
    break;        
  }          
          
  return op1;          
}       
       
       
  
  
/* g95_check_real_range()-- Given a real and a kind, make sure that
 * the real lies within the range of the kind.  Returns ARITH_OK,
 * ARITH_OVERFLOW or ARITH_UNDERFLOW. */     
     
static arith g95_check_real_range(mpf_t n, int k0) {    
arith retval;  
mpf_t b;     
int r;          
          
  mpf_init(b);    
  mpf_abs(b, n);   
   
  r = validate_real(k0);      
  if (r == -1) g95_internal_error("g95_check_real_range(): Bad kind");   
   
  retval = ARITH_OK;     
  if (mpf_sgn(b) == 0) goto done;  
  
  if (mpf_cmp(b, g95_real_kinds[r].huge) == 1) {    
    retval = ARITH_OVERFLOW;     
    goto done;       
  }        
        
  if (mpf_cmp(b, g95_real_kinds[r].tiny) == -1) retval = ARITH_UNDERFLOW;   
   
done:  
  mpf_clear(b);    
    
  return retval;        
}   
   
   
  
  
/* g95_arith_power()-- Raise a number to an integer power */  
  
arith g95_arith_power(g95_expr *op, g95_expr *op0, g95_expr **resp) {        
int power, apower;   
g95_expr *rslt;   
mpz_t unity_z;         
mpf_t unity_f;      
arith rc;         
         
  rc = ARITH_OK;          
          
  if (g95_extract_int(op0, &power) != NULL)    
    g95_internal_error("g95_arith_power(): Bad exponent");     
     
  rslt = g95_constant_result(op->ts.type, op->ts.kind, &op->where);    
    
  if (power == 0) {     /* Handle something to the zeroth power */
    switch(op->ts.type) {
    case BT_INTEGER:     
      if (mpz_sgn(op->value.integer) == 0)          
	rc = ARITH_0TO0;       
      else        
	mpz_set_ui(rslt->value.integer, 1);   
	   
      break;   
   
    case BT_REAL:  
      if (mpf_sgn(op->value.real) == 0)    
	rc = ARITH_0TO0;       
      else   
	mpf_set_ui(rslt->value.real, 1);         
         
      break; 
 
    case BT_COMPLEX:       
      if (mpf_sgn(op->value.complex.r) == 0 &&          
	  mpf_sgn(op->value.complex.i) == 0) 
	rc = ARITH_0TO0;      
      else {          
	mpf_set_ui(rslt->value.complex.r, 1);          
	mpf_set_ui(rslt->value.complex.r, 0);      
      }    
    
      break;         
         
    default:   
      g95_internal_error("g95_arith_power(): Bad base");     
    }     
  }

  if (power != 0) {      
    apower = power;         
    if (power < 0) apower = -power;          
          
    switch(op->ts.type) {    
    case BT_INTEGER:       
      mpz_pow_ui(rslt->value.integer, op->value.integer, apower);   
   
      if (power < 0) {      
	mpz_init_set_ui(unity_z, 1);  
	mpz_tdiv_q(rslt->value.integer, unity_z, rslt->value.integer);  
	mpz_clear(unity_z);      
      }        
        
      break;  
  
    case BT_REAL:      
      mpf_pow_ui(rslt->value.real, op->value.real, apower);         
         
      if (power < 0) {  
	mpf_init_set_ui(unity_f, 1);       
	mpf_div(rslt->value.real, unity_f, rslt->value.real);   
	mpf_clear(unity_f);  
      }    
    
      break;     
     
    case BT_COMPLEX:   
      complex_pow_ui(op, apower, rslt);         
      if (power < 0) complex_reciprocal(rslt);          
          
      break;     
     
    default:
      break;       
    }        
  }        
        
  if (rc == ARITH_OK) rc = g95_range_check(rslt);      
      
  if (rc != ARITH_OK)    
    g95_free_expr(rslt);     
  else  
    *resp = rslt;       
       
  return rc;       
}      
      
      
   
   
/* g95_real2complex()-- Convert real to complex. */          
          
g95_expr *g95_real2complex(g95_expr *s1, int k0) {   
g95_expr *result;  
arith rv;      
      
  result = g95_constant_result(BT_COMPLEX, k0, &s1->where);      
      
  mpf_set(result->value.complex.r, s1->value.real);   
  mpf_set_ui(result->value.complex.i, 0); 
 
  if ((rv = g95_check_real_range(result->value.complex.i, k0)) != ARITH_OK) {         
    arith_error(rv, &s1->ts, &result->ts, &s1->where);       
    g95_free_expr(result);    
    return NULL; 
  }         
         
  return result; 
}         
         
         
          
          
/* g95_real2real()-- Convert real to real */  
  
g95_expr *g95_real2real(g95_expr *s0, int kind) {    
g95_expr *result;     
arith rc;          
          
  result = g95_constant_result(BT_REAL, kind, &s0->where);

  mpf_set(result->value.real, s0->value.real); 
 
  if ((rc = g95_check_real_range(result->value.real, kind)) != ARITH_OK) {      
    arith_error(rc, &s0->ts, &result->ts, &s0->where);      
    g95_free_expr(result); 
    return NULL;
  }   
   
  return result;       
}      
      
      
   
   
static int validate_logical(int k) {       
int l;   
   
  if ( g95_option.l1 == 1 ) {    
    if ( k == 1 ) {  
      l = 0;
      return l;       
    }      
  }         
         
  for(l=0;; l++) {
    if (g95_logical_kinds[l].kind == 0) { l = -1; break; }       
    if (g95_logical_kinds[l].kind == k) break;        
  }   
   
  return l;      
}         
         
         
          
          
/* reduce_binary0()-- Reduce a binary expression where at least one of
 * the operands involves a zero-length array.  Returns NULL if neither
 * of the operands is a zero-length array. */        
        
static g95_expr *reduce_binary0(g95_expr *op1, g95_expr *op_2) {    
    
  if (g95_zero_size_array(op1)) {  
    g95_free_expr(op_2); 
    return op1;   
  } 
 
  if (g95_zero_size_array(op_2)) {   
    g95_free_expr(op1);          
    return op_2;
  }  
  
  return NULL;    
}    
    
    
     
     
/* g95_convert_complex()-- Convert a pair of real, constant expression
 * nodes to a single complex expression node. */  
  
g95_expr *g95_convert_complex(g95_expr *real, g95_expr *imag, int k) {
g95_expr *t;     
     
  t = g95_constant_result(BT_COMPLEX, k, &real->where); 
  mpf_set(t->value.complex.r, real->value.real);        
  mpf_set(t->value.complex.i, imag->value.real);       
       
  return t;         
}    
    
    
  
  
/* g95_complex2complex()-- Convert complex to complex */       
       
g95_expr *g95_complex2complex(g95_expr *s1, int k) {        
g95_expr *res;     
arith rc;  
  
  res = g95_constant_result(BT_COMPLEX, k, &s1->where);

  mpf_set(res->value.complex.r, s1->value.complex.r);       
  mpf_set(res->value.complex.i, s1->value.complex.i);       
       
  if ((rc = g95_check_real_range(res->value.complex.r, k)) != ARITH_OK ||
      (rc = g95_check_real_range(res->value.complex.i, k)) != ARITH_OK) {       
    arith_error(rc, &s1->ts, &res->ts, &s1->where);        
    g95_free_expr(res);     
    return NULL;          
  }   
   
  return res;          
}  
  
  
       
       
arith g95_arith_plus(g95_expr *op, g95_expr *op0, g95_expr **rp) {
g95_expr *result; 
arith rv;    
    
  result = g95_constant_result(op->ts.type, op->ts.kind, &op->where);        
        
  switch(op->ts.type) {   
  case BT_INTEGER:         
    mpz_add(result->value.integer, op->value.integer, op0->value.integer);  
    break;

  case BT_REAL:       
    mpf_add(result->value.real, op->value.real, op0->value.real);   
    break;      
      
  case BT_COMPLEX:     
    mpf_add(result->value.complex.r, op->value.complex.r,     
	    op0->value.complex.r);    
    
    mpf_add(result->value.complex.i, op->value.complex.i,         
	    op0->value.complex.i);     
    break;     
     
  default:  
    g95_internal_error("g95_arith_plus(): Bad basic type");      
  }         
         
  rv = g95_range_check(result); 
 
  if (rv != ARITH_OK)          
    g95_free_expr(result);         
  else
    *rp = result;   
   
  return rv;
}          
          
          
      
      
void arctangent(mpf_t *argum, mpf_t *result) {         
mpf_t absval, convgu, convgl, num, t, u, xp;
int l, sign;    
    
/* Similar to sine routine but requires special handling for x near 1 */   
   
  mpf_init_set(u, *argum); 
 
/* Special cases */       
  if (mpf_cmp_ui(u, 0) == 0) {    
    mpf_set_ui(*result, 0);  
  } else if (mpf_cmp_ui(u,1) == 0) {         
    mpf_init(num);          
    mpf_div_ui(num, half_pi, 2);    
    mpf_set(*result, num);          
    mpf_clear(num);     
  } else if (mpf_cmp_si(u,-1) == 0) {          
    mpf_init(num);       
    mpf_div_ui(num, half_pi, 2);      
    mpf_neg(*result, num); 
    mpf_clear(num);
  } else { /* General cases */     
     
    mpf_init(absval);        
    mpf_abs(absval, u);  
  
    mpf_init_set_d(convgu, 1.5);          
    mpf_init_set_d(convgl, 0.5);  
    mpf_init_set_ui(num, 1);
    mpf_init(t);  
  
    if (mpf_cmp(absval, convgl) < 0) {          
      mpf_init_set_ui(xp, 0);
      sign = -1;  
      for(l=1; l<G95_REAL_BITS+10; l++) {  
        mpf_mul(num, num, absval);       
	if (l%2 == 0) continue;     
     
	sign = -sign;    
	mpf_div_ui(t, num, l);    
	if (sign > 0)    
	  mpf_add(xp, xp, t);         
	else      
	  mpf_sub(xp, xp, t);    
      }         
    } else if (mpf_cmp(absval, convgu) >= 0) {       
      mpf_init_set(xp, half_pi);      
      sign = 1;     
      for(l=1; l<G95_REAL_BITS+10; l++) {          
        mpf_div(num, num, absval);        
	if (l%2 == 0) continue;       
       
	sign = -sign;         
	mpf_div_ui(t, num, l);   
	if (sign > 0)  
	  mpf_add(xp, xp, t);          
	else     
	  mpf_sub(xp, xp, t);       
      }        
    } else {     
      mpf_init_set_ui(xp, 0);       
       
      mpf_sub_ui(num, absval, 1);        
      mpf_add_ui(t, absval, 1);         
      mpf_div(absval, num, t);      
      
      mpf_set_ui(num, 1);        
        
      sign = -1;    
      for(l=1; l<G95_REAL_BITS+10; l++) {  
        mpf_mul(num, num, absval);          
	if (l%2 == 0) continue;
	sign = -sign;  
	mpf_div_ui(t, num, l);    
	if (sign > 0)    
	  mpf_add(xp, xp, t);     
	else  
	  mpf_sub(xp, xp, t);   
      }       
       
      mpf_div_ui(t, half_pi, 2);
      mpf_add(xp, t, xp);          
    }         
         
  /* This makes sure to preserve the identity arctan(-x) = -arctan(x) */     
  /* and improves accuracy to boot */          
          
    if (mpf_cmp_ui(u, 0) > 0) 
      mpf_set(*result, xp);          
    else  
      mpf_neg(*result, xp); 
 
    mpf_clear(absval);      
    mpf_clear(convgl);          
    mpf_clear(convgu);       
    mpf_clear(num);       
    mpf_clear(t);      
    mpf_clear(xp);
  }          
  mpf_clear(u);
}     
     
     
  
  
/* g95_arith_concat()-- Concatenate two string constants */    
    
arith g95_arith_concat(g95_expr *op, g95_expr *op0, g95_expr **resp) {  
g95_expr *result;         
int len; 
 
  result = g95_constant_result(BT_CHARACTER, g95_default_character_kind(),         
			       &op->where);        
        
  len = op->value.character.length + op0->value.character.length; 
 
  result->value.character.string = g95_getmem(len+1); 
  result->value.character.length = len;     
     
  memcpy(result->value.character.string, op->value.character.string, 
	 op->value.character.length);   
   
  memcpy(result->value.character.string + op->value.character.length,         
	 op0->value.character.string, op0->value.character.length);       
       
  result->value.character.string[len] = '\0';

  *resp = result;       
       
  return ARITH_OK;         
}


 
 
arith g95_arith_divide(g95_expr *op1, g95_expr *op, g95_expr **resultp) {   
g95_expr *result;        
mpf_t p, k, div;  
arith rv;     
     
  rv = ARITH_OK;  
  
  result = g95_constant_result(op1->ts.type, op1->ts.kind, &op1->where);    
    
  switch(op1->ts.type) {   
  case BT_INTEGER:   
    if (mpz_sgn(op->value.integer) == 0) {   
      rv = ARITH_DIV0;       
      break;         
    }   
   
    mpz_tdiv_q(result->value.integer, op1->value.integer, 
	       op->value.integer); 
    break;

  case BT_REAL:
    if (mpf_sgn(op->value.real) == 0) {          
      rv = ARITH_DIV0;  
      break; 
    }       
       
    mpf_div(result->value.real, op1->value.real, op->value.real);         
    break;      
      
  case BT_COMPLEX:
    if (mpf_sgn(op->value.complex.r) == 0 &&          
	mpf_sgn(op->value.complex.i) == 0) {         
      rv = ARITH_DIV0;    
      break;        
    }         
         
    mpf_init(p);        
    mpf_init(k);          
    mpf_init(div);

    mpf_mul(p, op->value.complex.r, op->value.complex.r);         
    mpf_mul(k, op->value.complex.i, op->value.complex.i);         
    mpf_add(div, p, k);        
        
    mpf_mul(p, op1->value.complex.r, op->value.complex.r); 
    mpf_mul(k, op1->value.complex.i, op->value.complex.i);
    mpf_add(result->value.complex.r, p, k);  
    mpf_div(result->value.complex.r, result->value.complex.r, div);        
        
    mpf_mul(p, op1->value.complex.i, op->value.complex.r);      
    mpf_mul(k, op1->value.complex.r, op->value.complex.i);  
    mpf_sub(result->value.complex.r, p, k);  
    mpf_div(result->value.complex.r, result->value.complex.r, div);

    mpf_clear(p);  
    mpf_clear(k);
    mpf_clear(div);     
     
    break;     
     
  default:   
    g95_internal_error("g95_arith_divide(): Bad basic type");   
  }   
   
  if (rv == ARITH_OK) rv = g95_range_check(result);      
      
  if (rv != ARITH_OK)  
    g95_free_expr(result);   
  else         
    *resultp = result;         
         
  return rv;     
}  
  
  
 
 
int g95_default_real_kind(void) {        
        
  return g95_real_kinds[g95_option.r8 ? 1 : 0].kind; 
}      
      
     
     
/* g95_complex2int()-- Convert complex to integer */          
          
g95_expr *g95_complex2int(g95_expr *s1, int k) { 
g95_expr *res;        
arith retval; 
 
  res = g95_constant_result(BT_INTEGER, k, &s1->where);    
    
  mpz_set_f(res->value.integer, s1->value.complex.r);     
     
  if ((retval = g95_check_integer_range(res->value.integer, k))          
      != ARITH_OK) {        
    arith_error(retval, &s1->ts, &res->ts, &s1->where);         
    g95_free_expr(res);  
    return NULL; 
  }          
          
  return res;     
}         
         
         
          
          
arith g95_arith_ne(g95_expr *op, g95_expr *op2, g95_expr **resp) { 
g95_expr *rslt;       
       
  rslt = g95_constant_result(BT_LOGICAL, g95_default_logical_kind(),       
			       &op->where);    
  rslt->value.logical = (op->ts.type == BT_COMPLEX) ?       
    !compare_complex(op, op2) : (g95_compare_expr(op, op2) != 0);     
     
  *resp = rslt; 
  return ARITH_OK;         
}

       
       
void common_logarithm(mpf_t *ap, mpf_t *res) {    
mpf_t i10, log10; 
 
  natural_logarithm(ap, res);        
        
  mpf_init_set_ui(i10, 10);      
  mpf_init(log10);   
  natural_logarithm(&i10, &log10);   
   
  mpf_div(*res, *res, log10);          
  mpf_clear(i10);         
  mpf_clear(log10);          
}      
      
      


/* g95_arith_error()-- Given an arithmetic error code, return a
 * pointer to a string that explains the error. */ 
 
char *g95_arith_error(arith codep) {
char *c;   
   
  switch(codep) {   
  case ARITH_OK:              c = "Arithmetic OK"; break;         
  case ARITH_OVERFLOW:        c = "Arithmetic overflow"; break;    
  case ARITH_UNDERFLOW:       c = "Arithmetic underflow"; break;     
  case ARITH_DIV0:            c = "Division by zero"; break;  
  case ARITH_0TO0:            c = "Indeterminate form 0 ** 0"; break;
  case ARITH_INCOMMENSURATE:  c = "Array operands are incommensurate"; break;         
  default: g95_internal_error("g95_arith_error(): Bad error code");          
  }        
        
  return c;          
}        
        
        
          
          
/* g95_compare_string()-- Given two constant strings and the inverse
 * collating sequence, compare the strings.  We return -1 for a<b,
 * 0 for a==b and 1 for a>b.  If the xcoll_table is NULL, we use the
 * processor's default collating sequence. */   
   
int g95_compare_string(g95_expr *w, g95_expr *c, int *xcoll_table) {  
int len, alen, blen, e, ac, bc;      
      
  alen = w->value.character.length; 
  blen = c->value.character.length;       
       
  len = (alen > blen) ? alen : blen;     
     
  for(e=0; e<len; e++) {         
    ac = (e < alen) ? w->value.character.string[e] : ' ';      
    bc = (e < blen) ? c->value.character.string[e] : ' ';       
       
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
    
    
   
   
arith g95_arith_ge(g95_expr *op1, g95_expr *op_2, g95_expr **resp) {
g95_expr *r;     
     
  r = g95_constant_result(BT_LOGICAL, g95_default_logical_kind(),  
			       &op1->where);     
  r->value.logical = (g95_compare_expr(op1, op_2) >= 0); 
  *resp = r;          
          
  return ARITH_OK;          
}   
   
          
          
arith g95_arith_lt(g95_expr *op0, g95_expr *op2, g95_expr **resp) {          
g95_expr *rslt;     
     
  rslt = g95_constant_result(BT_LOGICAL, g95_default_logical_kind(),          
			       &op0->where);         
  rslt->value.logical = (g95_compare_expr(op0, op2) < 0);   
  *resp = rslt;       
       
  return ARITH_OK;   
}     
     
          
          
/* natural_logarithm()-- Compute a natural logarithm */   
   
void natural_logarithm(mpf_t *a, mpf_t *r) {        
mpf_t u, xp, q, log;
int j, g;        
        
  mpf_init_set(u, *a); 
  mpf_init(q); 
 
  g = 0; 
 
  /* Get the argument into the range 0.5 to 1.5 by successive
   * multiplications or divisions by e. */ 
 
  mpf_set_str(q, "0.5", 10);   
  while(mpf_cmp(u, q) < 0) {     
    mpf_mul(u, u, e_value); 
    g--; 
  }          
          
  mpf_set_str(q, "1.5", 10);  
  while(mpf_cmp(u, q) > 0) {
    mpf_div(u, u, e_value);       
    g++;        
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
   
  mpf_sub_ui(u, u, 1);       
  mpf_init_set_ui(log, 0);     
  mpf_init_set_ui(xp, 1);  
  
  for(j=1; j<G95_REAL_BITS; j++) {      
    mpf_mul(xp, xp, u);   
    mpf_div_ui(q, xp, j);       
       
    if (j % 2 == 0)          
      mpf_sub(log, log, q);    
    else 
      mpf_add(log, log, q);        
  }        
        
  /* Add in the log (e^p) = p */         
         
  if (g < 0)       
    mpf_sub_ui(log, log, -g);  
  else         
    mpf_add_ui(log, log, g);        
        
  mpf_clear(u); 
  mpf_clear(xp);   
  mpf_clear(q);   
   
  mpf_set(*r, log);          
  mpf_clear(log);       
}          
          
          
         
         
/* g95_complex2real()-- Convert complex to real */        
        
g95_expr *g95_complex2real(g95_expr *source, int knd) {         
g95_expr *result;          
arith rc;   
   
  result = g95_constant_result(BT_REAL, knd, &source->where);

  mpf_set(result->value.real, source->value.complex.r);   
   
  if ((rc = g95_check_real_range(result->value.real, knd)) != ARITH_OK) {     
    arith_error(rc, &source->ts, &result->ts, &source->where);   
    g95_free_expr(result);         
    return NULL;       
  }  
  
  return result;      
}    
    
    
 
 
/* g95_range_check()-- Make sure a constant numeric expression is
 * within the range for it's type and kind.
 * Note that there's also a g95_check_range(), but that one deals
 * with the intrinsic RANGE function. */     
     
arith g95_range_check(g95_expr *q) {       
arith retval;      
      
  switch(q->ts.type) {          
  case BT_INTEGER: 
    retval = g95_check_integer_range(q->value.integer, q->ts.kind);        
    break;      
      
  case BT_REAL:   
    retval = g95_check_real_range(q->value.real, q->ts.kind);         
    break;

  case BT_COMPLEX:      
    retval = g95_check_real_range(q->value.complex.r, q->ts.kind);          
    if (retval != ARITH_OK)        
      retval = g95_check_real_range(q->value.complex.i, q->ts.kind);       
       
    break;          
          
  default: 
    g95_internal_error("g95_range_check(): Bad type");  
  }       
       
  return retval; 
}  
  
  
       
       
/* g95_int2real()-- Convert integers to reals */       
       
g95_expr *g95_int2real(g95_expr *s, int kind) {    
g95_expr *res; 
arith r;      
      
  res = g95_constant_result(BT_REAL, kind, &s->where);  
  
  mpf_set_z(res->value.real, s->value.integer);       
       
  if ((r = g95_check_real_range(res->value.real, kind)) != ARITH_OK) {    
    arith_error(r, &s->ts, &res->ts, &s->where); 
    g95_free_expr(res);     
    return NULL;   
  }        
        
  return res;          
}     
     
     
        
        
/* Low-level arithmetic functions.  All of these subroutines assume
 * that all operands are of the same type and return an operand of the
 * same type.  The other thing about these subroutines is that they
 * can fail in various ways-- overflow, underflow, division by zero,
 * zero raised to the zero, etc.  */        
        
arith g95_arith_not(g95_expr *op_1, g95_expr **rp) {     
g95_expr *rslt;          
          
  rslt = g95_constant_result(BT_LOGICAL, op_1->ts.kind, &op_1->where);   
  rslt->value.logical = !op_1->value.logical;       
  *rp = rslt;        
        
  return ARITH_OK;      
}          
          
          
arith g95_arith_and(g95_expr *op_1, g95_expr *op, g95_expr **rp) {        
g95_expr *rslt;

  rslt = g95_constant_result(BT_LOGICAL, g95_kind_max(op_1, op),        
			       &op_1->where);  
  rslt->value.logical = op_1->value.logical && op->value.logical;       
  *rp = rslt; 
 
  return ARITH_OK;        
}   
   
   
arith g95_arith_or(g95_expr *op_1, g95_expr *op, g95_expr **rp) {      
g95_expr *rslt;          
          
  rslt = g95_constant_result(BT_LOGICAL, g95_kind_max(op_1, op),     
			       &op_1->where);      
  rslt->value.logical = op_1->value.logical || op->value.logical; 
  *rp = rslt;       
       
  return ARITH_OK;          
}    
    
    
arith g95_arith_eqv(g95_expr *op_1, g95_expr *op, g95_expr **rp) {    
g95_expr *rslt;       
       
  rslt = g95_constant_result(BT_LOGICAL, g95_kind_max(op_1, op),       
			       &op_1->where);        
  rslt->value.logical = op_1->value.logical == op->value.logical;     
  *rp = rslt;       
       
  return ARITH_OK;         
}      
      
      
arith g95_arith_neqv(g95_expr *op_1, g95_expr *op, g95_expr **rp) {       
g95_expr *rslt;          
          
  rslt = g95_constant_result(BT_LOGICAL, g95_kind_max(op_1, op),       
			       &op_1->where);  
  rslt->value.logical = op_1->value.logical != op->value.logical;
  *rp = rslt;      
      
  return ARITH_OK;  
}          
          
          

static int validate_character(int knd) {       
       
  if (knd == g95_default_character_kind()) return 0; 
  return -1;        
} 
 
 
       
       
static arith reduce_unary(arith (*eval)(), g95_expr *operand, g95_expr **rslt) {        
g95_constructor *a, *head;  
g95_expr *x;       
arith rc; 
 
  if (operand->type == EXPR_CONSTANT) return eval(operand, rslt); 
 
  rc = ARITH_OK;    
  head = g95_copy_constructor(operand->value.constructor);         
         
  for(a=head; a; a=a->next) {        
    rc = eval(a->expr, &x);       
    if (rc != ARITH_OK) break;    
    
    g95_replace_expr(a->expr, x);        
  }    
    
  if (rc != ARITH_OK)          
    g95_free_constructor(head);          
  else {        
    x = g95_get_expr();          
    x->type = EXPR_ARRAY;  
    x->value.constructor = head;   
    x->shape = g95_copy_shape(operand->shape, operand->rank);        
        
    x->ts = head->expr->ts;        
    x->where = operand->where;   
    x->rank = operand->rank;   
   
    *rslt = x;          
  }

  return rc;          
}        
        
        
    
    
arith g95_arith_eq(g95_expr *op, g95_expr *op2, g95_expr **resp) {   
g95_expr *r;

  r = g95_constant_result(BT_LOGICAL, g95_default_logical_kind(),          
			       &op->where);      
  r->value.logical = (op->ts.type == BT_COMPLEX) ?
    compare_complex(op, op2) : (g95_compare_expr(op, op2) == 0);      
      
  *resp = r;   
  return ARITH_OK; 
}     
     
       
       
/* g95_log2log()-- Logical kind conversion. */     
     
g95_expr *g95_log2log(g95_expr *src, int k) {          
g95_expr *rslt;

  rslt = g95_constant_result(BT_LOGICAL, k, &src->where);       
  rslt->value.logical = src->value.logical;    
    
  return rslt;    
}   
   
   
   
   
   
/* g95_validate_kind()-- Validate a kind given a basic type.  The
 * return value is the same for the child functions, with -1
 * indicating nonexistence of the type */     
     
int g95_validate_kind(bt dtype, int k0) {   
int rc;        
        
  switch(dtype) {        
  case BT_REAL:     /* Fall through */        
  case BT_COMPLEX:    rc = validate_real(k0);      break;     
  case BT_INTEGER:    rc = validate_integer(k0);   break; 
  case BT_LOGICAL:    rc = validate_logical(k0);   break;     
  case BT_CHARACTER:  rc = validate_character(k0); break;      
      
  default:    
    g95_internal_error("g95_validate_kind(): Got bad type");   
  }       
       
  return rc;        
}        
        
        
   
   
static arith reduce_binary_ca(arith (*eval)(), g95_expr *op_1, g95_expr *op_2,       
			      g95_expr **result) {         
g95_constructor *v, *head;   
g95_expr *w;       
arith rc;  
  
  head = g95_copy_constructor(op_2->value.constructor);      
  rc = ARITH_OK;      
      
  for(v=head; v; v=v->next) { 
    rc = eval(op_1, v->expr, &w);      
    if (rc != ARITH_OK) break;     
     
    g95_replace_expr(v->expr, w);       
  } 
 
  if (rc != ARITH_OK)
    g95_free_constructor(head);
  else {          
    w = g95_get_expr();      
    w->type = EXPR_ARRAY;       
    w->value.constructor = head;      
    w->shape = g95_copy_shape(op_2->shape, op_2->rank);   
   
    w->ts = head->expr->ts;    
    w->where = op_2->where;     
    w->rank = op_2->rank;          
          
    *result = w;         
  }     
     
  return rc;    
}          
          
     
     
/* g95_int2complex()-- Convert default integer to default complex */

g95_expr *g95_int2complex(g95_expr *s, int knd) {        
g95_expr *result;       
arith retval;       
       
  result = g95_constant_result(BT_COMPLEX, knd, &s->where);     
     
  mpf_set_z(result->value.complex.r, s->value.integer);       
  mpf_set_ui(result->value.complex.i, 0);     
     
  if ((retval = g95_check_real_range(result->value.complex.i, knd)) != ARITH_OK) {    
    arith_error(retval, &s->ts, &result->ts, &s->where);    
    g95_free_expr(result);
    return NULL;   
  }     
     
  return result; 
}     
     
     
  
  
void cosine(mpf_t *argum, mpf_t *res) {      
mpf_t factor, j, n, num, den, u, z, xp;    
int s, sign;    
    
/* Similar to sine routine */   
   
  mpf_init_set(z, *argum);     
     
/* Special case (we do not treat multiples of pi due to roundoff issues) */      
  if (mpf_cmp_ui(z,0) == 0) {         
    mpf_set_ui(*res, 1);       
  } else {   
    mpf_init(j);
    mpf_init(n);      
    mpf_init(factor);
    mpf_init(u);       
       
    mpf_div(j, z, two_pi);          
    mpf_floor(factor, j);      
    mpf_mul(j, factor, two_pi);       
    mpf_sub(n, z, j);        
        
    mpf_init_set_ui(xp, 1);
    mpf_init_set_ui(num, 1);         
    mpf_init_set_ui(den, 1);    
    
    sign = 1;   
    for(s=1; s<G95_REAL_BITS+10; s++) {         
      mpf_mul(num, num, n);        
      mpf_mul_ui(den, den, s);    
      if (s%2 != 0) continue;   
   
      sign = -sign; 
      mpf_div(u, num, den);          
      if (sign > 0)
	mpf_add(xp, xp, u);     
      else        
	mpf_sub(xp, xp, u);        
    }          
    mpf_set(*res, xp);        
        
    mpf_clear(j);     
    mpf_clear(n);       
    mpf_clear(factor);  
    mpf_clear(num);          
    mpf_clear(den);  
    mpf_clear(u);         
    mpf_clear(xp);   
  }     
     
  mpf_clear(z);        
}         
         
         
     
     
static arith reduce_binary_ac(arith (*eval)(), g95_expr *op1, g95_expr *op,
			      g95_expr **rslt) {          
g95_constructor *v, *h;     
g95_expr *x;      
arith rv;       
       
  h = g95_copy_constructor(op1->value.constructor); 
  rv = ARITH_OK;  
  
  for(v=h; v; v=v->next) {      
    rv = eval(v->expr, op, &x);       
    if (rv != ARITH_OK) break;

    g95_replace_expr(v->expr, x);
  }          
          
  if (rv != ARITH_OK) 
    g95_free_constructor(h);
  else {      
    x = g95_get_expr();          
    x->type = EXPR_ARRAY;     
    x->value.constructor = h;          
    x->shape = g95_copy_shape(op1->shape, op1->rank);         
         
    x->ts = h->expr->ts;    
    x->where = op1->where;
    x->rank = op1->rank;

    *rslt = x;        
  }  
  
  return rv;          
}          
          
          
  
  
static arith reduce_binary(arith (*eval)(), g95_expr *op1, g95_expr *op2,       
			   g95_expr **rslt) {     
     
  if (op1->type == EXPR_CONSTANT && op2->type == EXPR_CONSTANT)    
    return eval(op1, op2, rslt);      
      
  if (op1->type == EXPR_CONSTANT && op2->type == EXPR_ARRAY)    
    return reduce_binary_ca(eval, op1, op2, rslt);        
        
  if (op1->type == EXPR_ARRAY && op2->type == EXPR_CONSTANT)         
    return reduce_binary_ac(eval, op1, op2, rslt);  
  
  return reduce_binary_aa(eval, op1, op2, rslt); 
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
   
static g95_expr *eval_intrinsic(g95_intrinsic_op operator, 
				arith (*eval)(),   
				g95_expr *op0, g95_expr *op2) {   
g95_expr t0, *res;         
int unary;     
arith retval;        
        
  g95_clear_ts(&t0.ts);     
     
  switch(operator) {          
  case INTRINSIC_NOT:    /* Logical unary */          
    if (op0->ts.type != BT_LOGICAL) goto runtime;          
          
    t0.ts.type = BT_LOGICAL;       
    t0.ts.kind = g95_default_logical_kind();   
   
    unary = 1;   
    break;          
          
    /* Logical binary operators */      
  case INTRINSIC_OR:     case INTRINSIC_AND:  
  case INTRINSIC_NEQV:   case INTRINSIC_EQV: 
    if (op0->ts.type != BT_LOGICAL || op2->ts.type != BT_LOGICAL)      
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
    if (op0->ts.type == BT_COMPLEX || op2->ts.type == BT_COMPLEX) {          
      t0.ts.type = BT_LOGICAL;     
      t0.ts.kind = g95_default_logical_kind();   
      goto runtime;          
    }

    /* Fall through */  
  
  case INTRINSIC_EQ:      case INTRINSIC_NE:    
    if (op0->ts.type == BT_CHARACTER && op2->ts.type == BT_CHARACTER) {  
      unary = 0;    
      t0.ts.type = BT_LOGICAL;    
      t0.ts.kind = g95_default_logical_kind();     
      break;       
    }       
       
    /* Fall through */      
      
  case INTRINSIC_PLUS:    case INTRINSIC_MINUS:   case INTRINSIC_TIMES:     
  case INTRINSIC_DIVIDE:  case INTRINSIC_POWER:   /* Numeric binary */   
    if (!g95_numeric_ts(&op0->ts) || !g95_numeric_ts(&op2->ts)) 
      goto runtime;  
  
    /* Insert any necessary type conversions to make the operands compatible */    
    
    t0.type = EXPR_OP;      
    g95_clear_ts(&t0.ts);      
    t0.operator = operator;          
          
    t0.op1 = op0;     
    t0.op2 = op2;   
   
    g95_type_convert_binary(&t0);          
          
    if (operator == INTRINSIC_EQ || operator == INTRINSIC_NE ||  
	operator == INTRINSIC_GE || operator == INTRINSIC_GT ||       
	operator == INTRINSIC_LE || operator == INTRINSIC_LT) { 
      t0.ts.type = BT_LOGICAL;        
      t0.ts.kind = g95_default_logical_kind();  
    }    
    
    if (operator == INTRINSIC_DIVIDE && op2->type == EXPR_CONSTANT &&
	((op2->ts.type == BT_INTEGER && 
	  mpz_cmp_ui(op2->value.integer, 0) == 0) ||          
	 (op2->ts.type == BT_REAL &&          
	  mpf_cmp_ui(op2->value.real, 0) == 0) ||          
	 (op2->ts.type == BT_COMPLEX &&       
	  mpf_cmp_ui(op2->value.complex.r, 0) == 0 &&     
	  mpf_cmp_ui(op2->value.complex.i, 0) == 0)))     
      goto runtime;       
       
    unary = 0;          
    break;       
       
  case INTRINSIC_CONCAT:   /* Character binary */   
    if (op0->ts.type != BT_CHARACTER || op2->ts.type != BT_CHARACTER)  
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
       
  if (operator == INTRINSIC_POWER && op2->ts.type != BT_INTEGER)
    goto runtime;       
       
  if (op0->type != EXPR_CONSTANT &&          
      (op0->type != EXPR_ARRAY || !g95_is_constant_expr(op0) ||     
       !g95_expanded_ac(op0)))   
    goto runtime;        
        
  if (op2 != NULL && op2->type != EXPR_CONSTANT &&      
      (op2->type != EXPR_ARRAY || !g95_is_constant_expr(op2) ||         
       !g95_expanded_ac(op2)))        
    goto runtime;       
       
  if (unary)        
    retval = reduce_unary(eval, op0, &res);         
  else         
    retval = reduce_binary(eval, op0, op2, &res);     
     
  if (retval != ARITH_OK) {     /* Something went wrong */   
    g95_error("%s at %L", g95_arith_error(retval), &op0->where);
    return NULL;     
  }     
     
  g95_free_expr(op0);       
  g95_free_expr(op2);   
  return res;

  /* Create a run-time expression */       
       
runtime:          
  res = g95_get_expr();         
  res->ts = t0.ts;      
      
  res->type = EXPR_OP;    
  res->operator = operator;   
   
  res->op1 = op0;          
  res->op2 = op2;  
  
  res->where = op0->where;     
     
  return res;   
}


  
  
static g95_expr *eval_intrinsic_f3(g95_intrinsic_op operator,   
				   arith (*eval)(g95_expr *, g95_expr *,
						 g95_expr **),       
				   g95_expr *op_1, g95_expr *op2) {
g95_expr *r;         
         
  r = reduce_binary0(op_1, op2);     
  if (r != NULL) return eval_type_intrinsic0(operator, r);   
   
  return eval_intrinsic(operator, eval, op_1, op2);     
}   
   
   
   
   
g95_expr *g95_subtract(g95_expr *op0, g95_expr *op_2) {      
  return eval_intrinsic_f3(INTRINSIC_MINUS, g95_arith_minus, op0, op_2);
}    
    
     
     
g95_expr *g95_le(g95_expr *op1, g95_expr *op0) {        
  return eval_intrinsic_f3(INTRINSIC_LE, g95_arith_le, op1, op0);         
}  
  
  
  
g95_expr *g95_neqv(g95_expr *op1, g95_expr *op) {         
  return eval_intrinsic_f3(INTRINSIC_NEQV, g95_arith_neqv, op1, op);      
}      
         
         
g95_expr *g95_eq(g95_expr *op0, g95_expr *op2) {    
  return eval_intrinsic_f3(INTRINSIC_EQ, g95_arith_eq, op0, op2); 
} 
 
        
        
g95_expr *g95_lt(g95_expr *op, g95_expr *op2) {   
  return eval_intrinsic_f3(INTRINSIC_LT, g95_arith_lt, op, op2);  
}         
         
       
       
g95_expr *g95_concat(g95_expr *op_1, g95_expr *op) {   
  return eval_intrinsic_f3(INTRINSIC_CONCAT, g95_arith_concat, op_1, op);          
}        
        
     
     
static g95_expr *eval_intrinsic_f2(g95_intrinsic_op oper,     
				   arith (*eval)(g95_expr *, g95_expr **),        
				   g95_expr *op0, g95_expr *op) {
g95_expr *r;         
         
  if (op == NULL) {         
    if (g95_zero_size_array(op0)) return eval_type_intrinsic0(oper, op0);      
  } else {          
    r = reduce_binary0(op0, op);     
    if (r != NULL) return eval_type_intrinsic0(oper, r); 
  }

  return eval_intrinsic(oper, eval, op0, op);  
}   
   
   
        
        
g95_expr *g95_multiply(g95_expr *op1, g95_expr *op0) {        
  return eval_intrinsic_f3(INTRINSIC_TIMES, g95_arith_times, op1, op0);       
}    
    
         
         
g95_expr *g95_not(g95_expr *op1) {      
  return eval_intrinsic_f2(INTRINSIC_NOT, g95_arith_not, op1, NULL);       
}  
  
         
         
g95_expr *g95_uplus(g95_expr *o) { 
  return eval_intrinsic_f2(INTRINSIC_UPLUS, g95_arith_uplus, o, NULL);         
}          
          
      
      
g95_expr *g95_ge(g95_expr *op, g95_expr *op2) {          
  return eval_intrinsic_f3(INTRINSIC_GE, g95_arith_ge, op, op2);   
}         
         
 
 
g95_expr *g95_uminus(g95_expr *op) {     
  return eval_intrinsic_f2(INTRINSIC_UMINUS, g95_arith_uminus, op, NULL);   
} 
 
  
  
g95_expr *g95_divide(g95_expr *op1, g95_expr *op2) {
  return eval_intrinsic_f3(INTRINSIC_DIVIDE, g95_arith_divide, op1, op2);       
}   
   


g95_expr *g95_or(g95_expr *op1, g95_expr *op_2) { 
  return eval_intrinsic_f3(INTRINSIC_OR, g95_arith_or, op1, op_2);    
}      
      
         
         
g95_expr *g95_ne(g95_expr *op, g95_expr *op_2) { 
  return eval_intrinsic_f3(INTRINSIC_NE, g95_arith_ne, op, op_2);      
}       
       
   
   
g95_expr *g95_gt(g95_expr *op0, g95_expr *op2) {        
  return eval_intrinsic_f3(INTRINSIC_GT, g95_arith_gt, op0, op2);     
}        
        
 
 
g95_expr *g95_power(g95_expr *op0, g95_expr *op) {          
  return eval_intrinsic_f3(INTRINSIC_POWER, g95_arith_power, op0, op);         
}          
          
     
     
g95_expr *g95_eqv(g95_expr *op_1, g95_expr *op) {         
  return eval_intrinsic_f3(INTRINSIC_EQV, g95_arith_eqv, op_1, op); 
}  
  
       
       
g95_expr *g95_and(g95_expr *op0, g95_expr *op) {    
  return eval_intrinsic_f3(INTRINSIC_AND, g95_arith_and, op0, op); 
}         
         
       
       
g95_expr *g95_add(g95_expr *op0, g95_expr *op2) {   
  return eval_intrinsic_f3(INTRINSIC_PLUS, g95_arith_plus, op0, op2);       
}        
        
