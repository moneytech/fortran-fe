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
         
         
    
    
/* g95_arith_uplus()-- It may seem silly to have a subroutine that
 * actually computes the unary plus of a constant, but it prevents us
 * from making exceptions in the code elsewhere. */        
        
arith g95_arith_uplus(g95_expr *op1, g95_expr **resultp) {        
        
  *resultp = g95_copy_expr(op1);  
  return ARITH_OK;  
}          
          
          
     
     
void cosine(mpf_t *arg, mpf_t *result) {        
mpf_t factor, j, r, num, denom, term, w, xp;  
int i, sign;        
        
/* Similar to sine routine */       
       
  mpf_init_set(w, *arg);     
     
/* Special case (we do not treat multiples of pi due to roundoff issues) */   
  if (mpf_cmp_ui(w,0) == 0) {    
    mpf_set_ui(*result, 1);  
  } else {       
    mpf_init(j);  
    mpf_init(r);    
    mpf_init(factor);      
    mpf_init(term);          
          
    mpf_div(j, w, two_pi);
    mpf_floor(factor, j);       
    mpf_mul(j, factor, two_pi);   
    mpf_sub(r, w, j);

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
        
    mpf_clear(j);  
    mpf_clear(r);         
    mpf_clear(factor);    
    mpf_clear(num);      
    mpf_clear(denom);     
    mpf_clear(term);     
    mpf_clear(xp);     
  }    
    
  mpf_clear(w);     
}


   
   
void sine(mpf_t *arg, mpf_t *result) {   
mpf_t factor, m, r, num, denom, term, x, xp;    
int p, sign; 
 
/* We use a reduction of the form
 *   x= N*2pi + r
 *   then obtain sin(r) from the McLaurin series. */    
    
  mpf_init_set(x, *arg);          
          
/* Special case (we do not treat multiples of pi due to roundoff issues) */  
  if (mpf_cmp_ui(x,0) ==  0) {  
    mpf_set_ui(*result,0);   
  } else { 
    mpf_init(m);     
    mpf_init(r);      
    mpf_init(factor);     
    mpf_init(term); 
 
    mpf_div(m, x, two_pi);   
    mpf_floor(factor, m);       
    mpf_mul(m, factor, two_pi);  
    mpf_sub(r, x, m);          
          
    mpf_init_set_ui(xp, 0);        
    mpf_init_set_ui(num, 1);       
    mpf_init_set_ui(denom, 1);

    sign = -1;   
    for(p=1; p<G95_REAL_BITS+10; p++) {  
      mpf_mul(num, num, r);  
      mpf_mul_ui(denom, denom, p); 
      if (p%2 == 0) continue;      
      
      sign = -sign;     
      mpf_div(term, num, denom);     
      if (sign > 0)      
	mpf_add(xp, xp, term);
      else    
	mpf_sub(xp,xp,term); 
    }  
  
    mpf_set(*result, xp);   
   
    mpf_clear(m);      
    mpf_clear(r);    
    mpf_clear(factor);         
    mpf_clear(num);    
    mpf_clear(denom);  
    mpf_clear(term);        
    mpf_clear(xp);   
  }      
      
  mpf_clear(x);
}        
        
        
         
         
static arith reduce_binary_ca(arith (*eval)(), g95_expr *op1, g95_expr *op2,       
			      g95_expr **result) {      
g95_constructor *x, *head;      
g95_expr *b;   
arith rc; 
 
  head = g95_copy_constructor(op2->value.constructor); 
  rc = ARITH_OK;   
   
  for(x=head; x; x=x->next) {      
    rc = eval(op1, x->expr, &b);        
    if (rc != ARITH_OK) break;    
    
    g95_replace_expr(x->expr, b);         
  }        
        
  if (rc != ARITH_OK) 
    g95_free_constructor(head);       
  else {          
    b = g95_get_expr();      
    b->type = EXPR_ARRAY;   
    b->value.constructor = head;   
    b->shape = g95_copy_shape(op2->shape, op2->rank);        
        
    b->ts = head->expr->ts;  
    b->where = op2->where;  
    b->rank = op2->rank;       
       
    *result = b;        
  }   
   
  return rc;
}         
         
         
         
/* reduce_binary0()-- Reduce a binary expression where at least one of
 * the operands involves a zero-length array.  Returns NULL if neither
 * of the operands is a zero-length array. */     
     
static g95_expr *reduce_binary0(g95_expr *op1, g95_expr *op2) {          
          
  if (g95_zero_size_array(op1)) {
    g95_free_expr(op2);       
    return op1; 
  }    
    
  if (g95_zero_size_array(op2)) {      
    g95_free_expr(op1);
    return op2;  
  }     
     
  return NULL;     
}    
    
    
 
 
/* g95_convert_complex()-- Convert a pair of real, constant expression
 * nodes to a single complex expression node. */

g95_expr *g95_convert_complex(g95_expr *real, g95_expr *imag, int kind) {         
g95_expr *v;    
    
  v = g95_constant_result(BT_COMPLEX, kind, &real->where);      
  mpf_set(v->value.complex.r, real->value.real);
  mpf_set(v->value.complex.i, imag->value.real); 
 
  return v;    
}       
       
       
   
   
arith g95_arith_lt(g95_expr *op1, g95_expr *op2, g95_expr **resultp) {       
g95_expr *result;   
   
  result = g95_constant_result(BT_LOGICAL, g95_default_logical_kind(), 
			       &op1->where);  
  result->value.logical = (g95_compare_expr(op1, op2) < 0);
  *resultp = result;        
        
  return ARITH_OK;    
}          
          
 
 
/* natural_logarithm()-- Compute a natural logarithm */  
  
void natural_logarithm(mpf_t *arg, mpf_t *result) {       
mpf_t z, xp, v, log;      
int q, g;         
         
  mpf_init_set(z, *arg);
  mpf_init(v);      
      
  g = 0; 
 
  /* Get the argument into the range 0.5 to 1.5 by successive
   * multiplications or divisions by e. */        
        
  mpf_set_str(v, "0.5", 10);      
  while(mpf_cmp(z, v) < 0) {         
    mpf_mul(z, z, e_value);         
    g--;          
  }        
        
  mpf_set_str(v, "1.5", 10);      
  while(mpf_cmp(z, v) > 0) {
    mpf_div(z, z, e_value);     
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
       
  mpf_sub_ui(z, z, 1);    
  mpf_init_set_ui(log, 0);          
  mpf_init_set_ui(xp, 1);        
        
  for(q=1; q<G95_REAL_BITS; q++) {
    mpf_mul(xp, xp, z);    
    mpf_div_ui(v, xp, q);    
    
    if (q % 2 == 0)        
      mpf_sub(log, log, v);      
    else  
      mpf_add(log, log, v);     
  }      
      
  /* Add in the log (e^p) = p */  
  
  if (g < 0)   
    mpf_sub_ui(log, log, -g);  
  else       
    mpf_add_ui(log, log, g);     
     
  mpf_clear(z);       
  mpf_clear(xp);       
  mpf_clear(v);         
         
  mpf_set(*result, log);     
  mpf_clear(log);         
}       
       
       
      
      
/* g95_zero_size_array()-- Return nonzero if the expression is a zero
 * size array. */        
        
int g95_zero_size_array(g95_expr *s) {         
         
  if (s->type != EXPR_ARRAY) return 0;    
    
  return s->value.constructor == NULL; 
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
          
          
  
  
/* g95_arith_error()-- Given an arithmetic error code, return a
 * pointer to a string that explains the error. */     
     
char *g95_arith_error(arith code) {   
char *d;       
       
  switch(code) {          
  case ARITH_OK:              d = "Arithmetic OK"; break;  
  case ARITH_OVERFLOW:        d = "Arithmetic overflow"; break;  
  case ARITH_UNDERFLOW:       d = "Arithmetic underflow"; break;  
  case ARITH_DIV0:            d = "Division by zero"; break;
  case ARITH_0TO0:            d = "Indeterminate form 0 ** 0"; break;  
  case ARITH_INCOMMENSURATE:  d = "Array operands are incommensurate"; break;        
  default: g95_internal_error("g95_arith_error(): Bad error code");          
  }      
      
  return d; 
}


      
      
/* g95_constant_result()-- Function to return a constant expression node
 * of a given type and kind. */         
         
g95_expr *g95_constant_result(bt type, int kind, locus *where) {    
g95_expr *result;     
     
  if (! where)  
    g95_internal_error("g95_constant_result(): locus 'where' cannot be NULL");   
   
  result = g95_get_expr();  
  
  result->type = EXPR_CONSTANT;   
  result->ts.type = type;        
  result->ts.kind = kind;        
  result->where = *where;       
       
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
      
      
         
         
int g95_default_logical_kind(void) {        
        
  return g95_logical_kinds[g95_option.i8 ? 1 : 0].kind;      
}      
      


/* validate_integer()-- Make sure that a valid kind is present.
 * Returns an index into the g95_integer_kinds array, -1 if the kind
 * is not present. */ 
 
static int validate_integer(int kind) {       
int z;     
     
  for(z=0;; z++) {       
    if (g95_integer_kinds[z].kind == 0) { z = -1; break; }       
    if (g95_integer_kinds[z].kind == kind) break;      
  }    
    
  return z; 
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


          
          
static arith reduce_unary(arith (*eval)(), g95_expr *op, g95_expr **result) {  
g95_constructor *c, *head; 
g95_expr *e;  
arith rc;          
          
  if (op->type == EXPR_CONSTANT) return eval(op, result);   
   
  rc = ARITH_OK;   
  head = g95_copy_constructor(op->value.constructor);     
     
  for(c=head; c; c=c->next) {     
    rc = eval(c->expr, &e);         
    if (rc != ARITH_OK) break;      
      
    g95_replace_expr(c->expr, e);      
  }     
     
  if (rc != ARITH_OK)    
    g95_free_constructor(head);          
  else {         
    e = g95_get_expr();       
    e->type = EXPR_ARRAY;       
    e->value.constructor = head;  
    e->shape = g95_copy_shape(op->shape, op->rank);      
      
    e->ts = head->expr->ts;       
    e->where = op->where; 
    e->rank = op->rank;       
       
    *result = e;
  }         
         
  return rc; 
} 
 
 
        
        
int g95_default_complex_kind(void) {      
      
  return g95_default_real_kind();
}  
  
  
         
         
void exponential(mpf_t *arg, mpf_t *result) {     
mpf_t two, ln2, power, l, a, num, denom, term, d, xp; 
int i;         
long n;      
unsigned long f, mp;  
  
/* We use a reduction of the form
 *   x= Nln2 + r
 *   then obtain exp(r) from the McLaurin series.
 *   exp(x) is then recovered from the identity exp(x) = 2^N*exp(r) */        
        
  mpf_init_set(d, *arg);          
          
  if (mpf_cmp_ui(d, 0) == 0) {
    mpf_set_ui(*result,1);      
  } else if (mpf_cmp_ui(d, 1) == 0) {       
    mpf_set(*result, e_value);          
  } else {          
    mpf_init_set_ui(two,2);          
    mpf_init(ln2); 
    mpf_init(l);          
    mpf_init(a);   
    mpf_init(power);    
    mpf_init(term);  
  
    natural_logarithm(&two, &ln2);          
          
    mpf_div(l, d, ln2);
    mpf_floor(power, l);      
    mpf_mul(l, power, ln2);       
    mpf_sub(a, d, l);      
      
    mpf_init_set_ui(xp, 1);          
    mpf_init_set_ui(num, 1);          
    mpf_init_set_ui(denom, 1);       
       
    for(i=1; i<=G95_REAL_BITS+10; i++) {  
      mpf_mul(num, num, a);         
      mpf_mul_ui(denom, denom, i);   
      mpf_div(term, num, denom);  
      mpf_add(xp, xp, term);      
    }   
   
    /* Reconstruction step */       
    n = (long) mpf_get_d(power);        
        
    if (n > 0) {         
      f = (unsigned int) n;      
      mpf_mul_2exp(*result,xp,f);  
    } else {    
      mp = (unsigned int) (-n);        
      mpf_div_2exp(*result,xp,mp);          
    }          
          
    mpf_clear(two); 
    mpf_clear(ln2);          
    mpf_clear(l);        
    mpf_clear(a); 
    mpf_clear(power);         
    mpf_clear(num);         
    mpf_clear(denom);  
    mpf_clear(term);   
    mpf_clear(xp);   
  }     
     
  mpf_clear(d);   
} 
 
 
        
        
/* g95_arith_done_1()-- Get rid of numeric constants. */       
       
void g95_arith_done_1(void) {
g95_integer_info *ip;   
g95_real_info *rp;       
       
  mpf_clear(e_value);     
     
  mpf_clear(pi);    
  mpf_clear(half_pi);         
  mpf_clear(two_pi);      
      
  for(ip=g95_integer_kinds; ip->kind; ip++) {        
    mpz_clear(ip->min_int); 
    mpz_clear(ip->max_int);       
    mpz_clear(ip->huge);          
  }     
     
  for(rp=g95_real_kinds; rp->kind; rp++) {       
    mpf_clear(rp->epsilon);          
    mpf_clear(rp->huge);         
    mpf_clear(rp->tiny);
  }         
}     
     
     
/* g95_default_*_kind()-- Return default kinds */        
        

/******* Simplification of intrinsic functions with constant arguments *****/  
  
/* arith_error()-- Deal with an arithmetic error. */       
       
static void arith_error(arith rc, g95_typespec *from, g95_typespec *to,         
			locus *where) {       
       
  g95_error("%s converting %s to %s at %L", g95_arith_error(rc),      
	    g95_typename(from), g95_typename(to), where);          
          
  /* TODO: Do something about the error, ie underflow rounds to 0,
   * throw exception, return NaN, etc. */          
}


     
     
int g95_default_integer_kind(void) {  
  
  return g95_integer_kinds[g95_option.i8 ? 1 : 0].kind;    
}      
      
 
 
/* g95_check_integer_range()-- Given an integer and a kind, make sure
 * that the integer lies within the range of the kind.  Returns
 * ARITH_OK or ARITH_OVERFLOW. */         
         
static arith g95_check_integer_range(mpz_t h, int kind) { 
arith result;  
int l;        
        
  l = validate_integer(kind);       
  if (l == -1) g95_internal_error("g95_check_integer_range(): Bad kind");          
          
  result = ARITH_OK;      
      
  if (mpz_cmp(h, g95_integer_kinds[l].min_int) < 0 || 
      mpz_cmp(h, g95_integer_kinds[l].max_int) > 0) result = ARITH_OVERFLOW;       
       
  return result;    
}    
    
    
      
      
/* g95_compare_string()-- Given two constant strings and the inverse
 * collating sequence, compare the strings.  We return -1 for a<b,
 * 0 for a==b and 1 for a>b.  If the xcoll_table is NULL, we use the
 * processor's default collating sequence. */  
  
int g95_compare_string(g95_expr *r, g95_expr *b, int *xcoll_table) {          
int len, alen, blen, y, ac, bc;  
  
  alen = r->value.character.length;          
  blen = b->value.character.length;   
   
  len = (alen > blen) ? alen : blen;   
   
  for(y=0; y<len; y++) {        
    ac = (y < alen) ? r->value.character.string[y] : ' ';          
    bc = (y < blen) ? b->value.character.string[y] : ' ';    
    
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
  
  
          
          
/* g95_arith_init_1()-- Get things ready to do math. */ 
 
void g95_arith_init_1(void) {         
g95_integer_info *int_info;    
g95_real_info *real_info;    
mpf_t d, z;     
mpz_t x;          
int m, p, limit;   
   
/* Set the default precision for GMP computations */         
  mpf_set_default_prec(G95_REAL_BITS+30);  
  
/* Calculate e, needed by the natural_logarithm() subroutine. */    
    
  mpf_init(z);          
  mpf_init_set_ui(e_value, 0);     
  mpf_init_set_ui(d, 1);        
        
  for(m=1; m<100; m++) {         
    mpf_add(e_value, e_value, d);    
    mpf_div_ui(d, d, m);   /* 1/(i!) */       
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
        
  for(p=0; p<limit; p++) { 
    mpf_set_ui(z, 4); 
    mpf_div_ui(z, z, 8*p+1);  /* 4/(8n+1) */    
    
    mpf_set_ui(d, 2);          
    mpf_div_ui(d, d, 8*p+4);  /* 2/(8n+4) */ 
    mpf_sub(z, z, d);       
       
    mpf_set_ui(d, 1);   
    mpf_div_ui(d, d, 8*p+5);  /* 1/(8n+5) */  
    mpf_sub(z, z, d);  
  
    mpf_set_ui(d, 1);          
    mpf_div_ui(d, d, 8*p+6);  /* 1/(8n+6) */    
    mpf_sub(z, z, d);    
    
    mpf_set_ui(d, 16);       
    mpf_pow_ui(d, d, p);      /* 16^n */  
  
    mpf_div(z, z, d);      
      
    mpf_add(pi, pi, z);    
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
    /* No -1 here, because the representation is symmetric */        
        
    mpz_init(int_info->max_int); 
    mpz_add(int_info->max_int, int_info->huge, int_info->huge);  
    mpz_add_ui(int_info->max_int, int_info->max_int, 1);         
         
    /* Range */ 
 
    mpf_set_z(d, int_info->huge);         
    common_logarithm(&d, &d);          
    mpf_trunc(d, d);    
    mpz_set_f(x, d);          
    int_info->range = mpz_get_si(x);          
  }    
    
/*  mpf_set_default_prec(G95_REAL_BITS); */      
      
  for(real_info=g95_real_kinds; real_info->kind != 0; real_info++) {     
    /* Huge */         
         
    mpf_set_ui(d, real_info->radix);     
    mpf_set_ui(z, real_info->radix);       
       
    mpf_pow_ui(d, d, real_info->max_exponent);       
    mpf_pow_ui(z, z, real_info->max_exponent - real_info->digits);         
         
    mpf_init(real_info->huge);       
    mpf_sub(real_info->huge, d, z);          
          
    /* Tiny */  
  
    mpf_set_ui(z, real_info->radix);   
    mpf_pow_ui(z, z, 1-real_info->min_exponent);     
     
    mpf_init(real_info->tiny);  
    mpf_ui_div(real_info->tiny, 1, z);   
   
    /* Epsilon */

    mpf_set_ui(z, real_info->radix); 
    mpf_pow_ui(z, z, real_info->digits - 1); 
 
    mpf_init(real_info->epsilon); 
    mpf_ui_div(real_info->epsilon, 1, z);

    /* Range */

    common_logarithm(&real_info->huge, &d);
    common_logarithm(&real_info->tiny, &z);
    mpf_neg(z, z);         
         
    if (mpf_cmp(d, z) > 0) mpf_set(d, z);  /* a = min(a, b) */    
    
    mpf_trunc(d, d);         
    mpz_set_f(x, d);        
    real_info->range = mpz_get_si(x); 
 
    /* Precision */        
        
    mpf_set_ui(d, real_info->radix);          
    common_logarithm(&d, &d);

    mpf_mul_ui(d, d, real_info->digits-1);    
    mpf_trunc(d, d);   
    mpz_set_f(x, d);          
    real_info->precision = mpz_get_si(x);  
  
    /* If the radix is an integral power of 10, add one to the precision. */       
       
    for(m=10; m<=real_info->radix; m*=10)
      if (m == real_info->radix) real_info->precision++;          
  } 
 
  mpz_clear(x);          
  mpf_clear(d);       
  mpf_clear(z); 
}   
   
   
    
    
/* hyperbolic cosine */        
        
void hypercos(mpf_t *arg, mpf_t *result) {
mpf_t neg, term1, term2, g, xp;   
   
  mpf_init_set(g, *arg); 
 
  mpf_init(neg);         
  mpf_init(term1);        
  mpf_init(term2);   
  mpf_init(xp);    
    
  mpf_neg(neg, g);         
         
  exponential(&g, &term1);          
  exponential(&neg, &term2);

  mpf_add(xp, term1, term2);     
  mpf_div_ui(*result, xp, 2);      
      
  mpf_clear(neg);    
  mpf_clear(term1);
  mpf_clear(term2);        
  mpf_clear(g);       
  mpf_clear(xp);       
}    
    
    
void hypersine(mpf_t *arg, mpf_t *result) { 
mpf_t neg, term1, term2, g, xp;        
        
  mpf_init_set(g, *arg);   
   
  mpf_init(neg);  
  mpf_init(term1);          
  mpf_init(term2);      
  mpf_init(xp);

  mpf_neg(neg, g);    
    
  exponential(&g, &term1);       
  exponential(&neg, &term2);    
    
  mpf_sub(xp, term1, term2);       
  mpf_div_ui(*result, xp, 2);

  mpf_clear(neg); 
  mpf_clear(term1);         
  mpf_clear(term2);          
  mpf_clear(g);        
  mpf_clear(xp);         
}          
          
          
  
  
static arith reduce_binary_ac(arith (*eval)(), g95_expr *op1, g95_expr *op2,       
			      g95_expr **result) {   
g95_constructor *h, *head;        
g95_expr *e;      
arith rc;     
     
  head = g95_copy_constructor(op1->value.constructor);      
  rc = ARITH_OK;     
     
  for(h=head; h; h=h->next) {
    rc = eval(h->expr, op2, &e);        
    if (rc != ARITH_OK) break;   
   
    g95_replace_expr(h->expr, e);  
  }

  if (rc != ARITH_OK) 
    g95_free_constructor(head);         
  else {      
    e = g95_get_expr();      
    e->type = EXPR_ARRAY;       
    e->value.constructor = head;          
    e->shape = g95_copy_shape(op1->shape, op1->rank);          
          
    e->ts = head->expr->ts;     
    e->where = op1->where;         
    e->rank = op1->rank;        
        
    *result = e;    
  }          
          
  return rc;          
}     
     
     
         
         
/* g95_complex2int()-- Convert complex to integer */       
       
g95_expr *g95_complex2int(g95_expr *src, int kind) {        
g95_expr *result; 
arith rc;         
         
  result = g95_constant_result(BT_INTEGER, kind, &src->where);       
       
  mpz_set_f(result->value.integer, src->value.complex.r);  
  
  if ((rc = g95_check_integer_range(result->value.integer, kind))        
      != ARITH_OK) {        
    arith_error(rc, &src->ts, &result->ts, &src->where);   
    g95_free_expr(result);
    return NULL;  
  }   
   
  return result;         
} 
 
 
   
   
arith g95_arith_plus(g95_expr *op1, g95_expr *op2, g95_expr **resultp) { 
g95_expr *result;          
arith rc;        
        
  result = g95_constant_result(op1->ts.type, op1->ts.kind, &op1->where);

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
    
    
          
          
/* g95_real2int()-- Convert default real to default integer */    
    
g95_expr *g95_real2int(g95_expr *src, int kind) {      
g95_expr *result;   
arith rc;         
         
  result = g95_constant_result(BT_INTEGER, kind, &src->where);       
       
  mpz_set_f(result->value.integer, src->value.real);

  if ((rc = g95_check_integer_range(result->value.integer, kind)) 
      != ARITH_OK) {   
    arith_error(rc, &src->ts, &result->ts, &src->where);    
    g95_free_expr(result);          
    return NULL; 
  }      
      
  return result;         
}          
          
          


/* compare_complex()-- Compare a pair of complex numbers.  Naturally,
 * this is only for equality/nonequality. */   
   
static int compare_complex(g95_expr *op1, g95_expr *op2) {   
   
  return (mpf_cmp(op1->value.complex.r, op2->value.complex.r) == 0 &&     
	  mpf_cmp(op1->value.complex.i, op2->value.complex.i) == 0);       
}     
     
     
 
 
int g95_default_real_kind(void) {  
  
  return g95_real_kinds[g95_option.r8 ? 1 : 0].kind;       
}        
        
         
         
int g95_default_double_kind(void) {

  return g95_real_kinds[1].kind;
}       
       
  
  
/* eval_type_intrinsic0() -- modify type of expression for zero size array */   
   
static g95_expr *eval_type_intrinsic0(g95_intrinsic_op operator, g95_expr *op){          
          
  if (op == NULL) g95_internal_error("eval_type_intrinsic0(): op NULL");         
         
  switch(operator) {  
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
      
      
          
          
arith g95_arith_le(g95_expr *op1, g95_expr *op2, g95_expr **resultp) {        
g95_expr *result;       
       
  result = g95_constant_result(BT_LOGICAL, g95_default_logical_kind(), 
			       &op1->where);        
  result->value.logical = (g95_compare_expr(op1, op2) <= 0);   
  *resultp = result;

  return ARITH_OK;        
}          
          
     
     
void arctangent(mpf_t *arg, mpf_t *result) {    
mpf_t absval, convgu, convgl, num, term, z, xp;     
int g, sign;          
          
/* Similar to sine routine but requires special handling for x near 1 */

  mpf_init_set(z, *arg);         
         
/* Special cases */       
  if (mpf_cmp_ui(z, 0) == 0) {      
    mpf_set_ui(*result, 0);          
  } else if (mpf_cmp_ui(z,1) == 0) {          
    mpf_init(num);       
    mpf_div_ui(num, half_pi, 2);        
    mpf_set(*result, num);        
    mpf_clear(num);    
  } else if (mpf_cmp_si(z,-1) == 0) {          
    mpf_init(num);      
    mpf_div_ui(num, half_pi, 2);     
    mpf_neg(*result, num); 
    mpf_clear(num);     
  } else { /* General cases */

    mpf_init(absval);     
    mpf_abs(absval, z);     
     
    mpf_init_set_d(convgu, 1.5);      
    mpf_init_set_d(convgl, 0.5);  
    mpf_init_set_ui(num, 1);    
    mpf_init(term); 
 
    if (mpf_cmp(absval, convgl) < 0) { 
      mpf_init_set_ui(xp, 0);       
      sign = -1;  
      for(g=1; g<G95_REAL_BITS+10; g++) {      
        mpf_mul(num, num, absval);       
	if (g%2 == 0) continue;    
    
	sign = -sign;          
	mpf_div_ui(term, num, g);    
	if (sign > 0)
	  mpf_add(xp, xp, term);  
	else         
	  mpf_sub(xp, xp, term);     
      }       
    } else if (mpf_cmp(absval, convgu) >= 0) { 
      mpf_init_set(xp, half_pi);   
      sign = 1;          
      for(g=1; g<G95_REAL_BITS+10; g++) {   
        mpf_div(num, num, absval);   
	if (g%2 == 0) continue;

	sign = -sign;     
	mpf_div_ui(term, num, g);          
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
      for(g=1; g<G95_REAL_BITS+10; g++) {     
        mpf_mul(num, num, absval);   
	if (g%2 == 0) continue;   
	sign = -sign;     
	mpf_div_ui(term, num, g); 
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
 
    if (mpf_cmp_ui(z, 0) > 0)         
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
  mpf_clear(z); 
}        
        
        
      
      
static arith reduce_binary_aa(arith (*eval)(), g95_expr *op1, g95_expr *op2,          
			      g95_expr **result) {       
g95_constructor *x, *a, *head;     
g95_expr *t;
arith rc;     
     
  head = g95_copy_constructor(op1->value.constructor);

  rc = ARITH_OK;      
  a = op2->value.constructor;   
   
  if (g95_check_conformance("Elemental binary operation", op1, op2)          
      != SUCCESS)  
    rc = ARITH_INCOMMENSURATE; 
  else {         
         
    for(x=head; x; x=x->next, a=a->next) { 
      if (a == NULL) {  
	rc = ARITH_INCOMMENSURATE;        
	break;          
      }     
     
      rc = eval(x->expr, a->expr, &t);
      if (rc != ARITH_OK) break;     
           
      g95_replace_expr(x->expr, t);       
    }    
    
    if (a != NULL) rc = ARITH_INCOMMENSURATE;          
  }      
      
  if (rc != ARITH_OK) 
    g95_free_constructor(head);         
  else {          
    t = g95_get_expr(); 
    t->type = EXPR_ARRAY;       
    t->value.constructor = head; 
    t->shape = g95_copy_shape(op1->shape, op1->rank);  
  
    t->ts = head->expr->ts;     
    t->where = op1->where;    
    t->rank = op1->rank;      
      
    *result = t; 
  }       
       
  return rc;         
}    
    
    
    
static int validate_character(int kind) {  
  
  if (kind == g95_default_character_kind()) return 0;      
  return -1;      
}        
        
        
  
  
/* g95_convert_integer()-- Convert an integer string to an expression
 * node */

g95_expr *g95_convert_integer(char *buffer, int kind, int radix,          
			      locus *where) {        
g95_expr *u;          
          
  u = g95_constant_result(BT_INTEGER, kind, where);         
  if (buffer[0] == '+') buffer++;         
  mpz_set_str(u->value.integer, buffer, radix);         
         
  return u;  
}      
      
      
    
    
static int validate_logical(int kind) {     
int a;  
  
  if ( g95_option.l1 == 1 ) {       
    if ( kind == 1 ) {          
      a = 0;       
      return a;     
    }         
  }  
  
  for(a=0;; a++) {       
    if (g95_logical_kinds[a].kind == 0) { a = -1; break; }   
    if (g95_logical_kinds[a].kind == kind) break;       
  }   
   
  return a;        
}     
     
     
      
      
arith g95_arith_uminus(g95_expr *op1, g95_expr **resultp) {       
g95_expr *result;      
arith rc;       
       
  result = g95_constant_result(op1->ts.type, op1->ts.kind, &op1->where);      
      
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
 
 
      
      
static int validate_real(int kind) {     
int v;     
     
  for(v=0;; v++) {  
    if (g95_real_kinds[v].kind == 0) { v = -1; break; }   
    if (g95_real_kinds[v].kind == kind) break;    
  }  
  
  return v;     
}     
     
     
       
       
/* g95_check_real_range()-- Given a real and a kind, make sure that
 * the real lies within the range of the kind.  Returns ARITH_OK,
 * ARITH_OVERFLOW or ARITH_UNDERFLOW. */ 
 
static arith g95_check_real_range(mpf_t z, int kind) {
arith retval; 
mpf_t k;    
int w;      
      
  mpf_init(k);
  mpf_abs(k, z);      
      
  w = validate_real(kind);     
  if (w == -1) g95_internal_error("g95_check_real_range(): Bad kind");

  retval = ARITH_OK;   
  if (mpf_sgn(k) == 0) goto done;     
     
  if (mpf_cmp(k, g95_real_kinds[w].huge) == 1) {   
    retval = ARITH_OVERFLOW;       
    goto done;         
  }          
          
  if (mpf_cmp(k, g95_real_kinds[w].tiny) == -1) retval = ARITH_UNDERFLOW;     
     
done:        
  mpf_clear(k);   
   
  return retval;      
}          
          
          
  
  
/* g95_arith_concat()-- Concatenate two string constants */    
    
arith g95_arith_concat(g95_expr *op1, g95_expr *op2, g95_expr **resultp) {         
g95_expr *result;   
int len; 
 
  result = g95_constant_result(BT_CHARACTER, g95_default_character_kind(),  
			       &op1->where);         
         
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
    
    
 
 
/* g95_int2real()-- Convert integers to reals */    
    
g95_expr *g95_int2real(g95_expr *src, int kind) {         
g95_expr *result;          
arith rc;    
    
  result = g95_constant_result(BT_REAL, kind, &src->where);        
        
  mpf_set_z(result->value.real, src->value.integer); 
 
  if ((rc = g95_check_real_range(result->value.real, kind)) != ARITH_OK) {       
    arith_error(rc, &src->ts, &result->ts, &src->where);  
    g95_free_expr(result);       
    return NULL;     
  }          
          
  return result;         
}         
         
         
       
       
arith g95_arith_ne(g95_expr *op1, g95_expr *op2, g95_expr **resultp) {
g95_expr *result;

  result = g95_constant_result(BT_LOGICAL, g95_default_logical_kind(), 
			       &op1->where);
  result->value.logical = (op1->ts.type == BT_COMPLEX) ?  
    !compare_complex(op1, op2) : (g95_compare_expr(op1, op2) != 0);       
       
  *resultp = result;
  return ARITH_OK;  
}    
    
       
       
arith g95_arith_eq(g95_expr *op1, g95_expr *op2, g95_expr **resultp) {   
g95_expr *result;          
          
  result = g95_constant_result(BT_LOGICAL, g95_default_logical_kind(),
			       &op1->where);
  result->value.logical = (op1->ts.type == BT_COMPLEX) ?          
    compare_complex(op1, op2) : (g95_compare_expr(op1, op2) == 0);

  *resultp = result;       
  return ARITH_OK;          
}       
       
        
        
arith g95_arith_gt(g95_expr *op1, g95_expr *op2, g95_expr **resultp) {  
g95_expr *result;          
          
  result = g95_constant_result(BT_LOGICAL, g95_default_logical_kind(),        
			       &op1->where);     
  result->value.logical = (g95_compare_expr(op1, op2) > 0);  
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
    rc = (!!op1->value.logical) - (!!op2->value.logical);     
     
    if (rc < 0) rc = -1; 
    if (rc > 0) rc = 1;        
    break;

  default: g95_internal_error("g95_compare_expr(): Bad basic type");
  }        
        
  return rc;      
}         
         
         
      
      
/* complex_pow_ui()-- Raise a complex number to positive power */   
   
static void complex_pow_ui(g95_expr *base, int power, g95_expr *result) {          
mpf_t temp_r, temp_i, f;        
        
  mpf_set_ui(result->value.complex.r, 1);      
  mpf_set_ui(result->value.complex.i, 0);   
   
  mpf_init(temp_r);    
  mpf_init(temp_i);   
  mpf_init(f);  
  
  for(; power>0; power--) {
    mpf_mul(temp_r, base->value.complex.r, result->value.complex.r);   
    mpf_mul(f,      base->value.complex.i, result->value.complex.i);
    mpf_sub(temp_r, temp_r, f);      
      
    mpf_mul(temp_i, base->value.complex.r, result->value.complex.i);       
    mpf_mul(f,      base->value.complex.i, result->value.complex.r);          
    mpf_add(temp_i, temp_i, f);       
       
    mpf_set(result->value.complex.r, temp_r);
    mpf_set(result->value.complex.i, temp_i);       
  }  
  
  mpf_clear(temp_r);  
  mpf_clear(temp_i);   
  mpf_clear(f);    
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
    *resultp = result;     
     
  return rc;        
}         
         
         
   
   
/* g95_real2complex()-- Convert real to complex. */          
          
g95_expr *g95_real2complex(g95_expr *src, int kind) {  
g95_expr *result;         
arith rc;   
   
  result = g95_constant_result(BT_COMPLEX, kind, &src->where);    
    
  mpf_set(result->value.complex.r, src->value.real); 
  mpf_set_ui(result->value.complex.i, 0);      
      
  if ((rc = g95_check_real_range(result->value.complex.i, kind)) != ARITH_OK) {    
    arith_error(rc, &src->ts, &result->ts, &src->where);       
    g95_free_expr(result);     
    return NULL;         
  }       
       
  return result;    
}


         
         
arith g95_arith_minus(g95_expr *op1, g95_expr *op2, g95_expr **resultp) {         
g95_expr *result; 
arith rc;  
  
  result = g95_constant_result(op1->ts.type, op1->ts.kind, &op1->where);        
        
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
      
      
        
        
static arith reduce_binary(arith (*eval)(), g95_expr *op1, g95_expr *op2, 
			   g95_expr **result) {       
       
  if (op1->type == EXPR_CONSTANT && op2->type == EXPR_CONSTANT)    
    return eval(op1, op2, result);  
  
  if (op1->type == EXPR_CONSTANT && op2->type == EXPR_ARRAY)      
    return reduce_binary_ca(eval, op1, op2, result); 
 
  if (op1->type == EXPR_ARRAY && op2->type == EXPR_CONSTANT)    
    return reduce_binary_ac(eval, op1, op2, result);    
    
  return reduce_binary_aa(eval, op1, op2, result);
}       
       
       
       
       
/* Low-level arithmetic functions.  All of these subroutines assume
 * that all operands are of the same type and return an operand of the
 * same type.  The other thing about these subroutines is that they
 * can fail in various ways-- overflow, underflow, division by zero,
 * zero raised to the zero, etc.  */         
         
arith g95_arith_not(g95_expr *op1, g95_expr **resultp) {    
g95_expr *result;  
  
  result = g95_constant_result(BT_LOGICAL, op1->ts.kind, &op1->where); 
  result->value.logical = !op1->value.logical;   
  *resultp = result;         
         
  return ARITH_OK;         
}          
          
          
arith g95_arith_and(g95_expr *op1, g95_expr *op2, g95_expr **resultp) {    
g95_expr *result;   
   
  result = g95_constant_result(BT_LOGICAL, g95_kind_max(op1, op2), 
			       &op1->where);
  result->value.logical = op1->value.logical && op2->value.logical;        
  *resultp = result;   
   
  return ARITH_OK; 
}  
  
  
arith g95_arith_or(g95_expr *op1, g95_expr *op2, g95_expr **resultp) {   
g95_expr *result;

  result = g95_constant_result(BT_LOGICAL, g95_kind_max(op1, op2),        
			       &op1->where);      
  result->value.logical = op1->value.logical || op2->value.logical;   
  *resultp = result;    
    
  return ARITH_OK; 
}    
    
    
arith g95_arith_eqv(g95_expr *op1, g95_expr *op2, g95_expr **resultp) {
g95_expr *result;      
      
  result = g95_constant_result(BT_LOGICAL, g95_kind_max(op1, op2),       
			       &op1->where);         
  result->value.logical = op1->value.logical == op2->value.logical;       
  *resultp = result;

  return ARITH_OK;   
}


arith g95_arith_neqv(g95_expr *op1, g95_expr *op2, g95_expr **resultp) {         
g95_expr *result;          
          
  result = g95_constant_result(BT_LOGICAL, g95_kind_max(op1, op2),         
			       &op1->where);   
  result->value.logical = op1->value.logical != op2->value.logical;   
  *resultp = result; 
 
  return ARITH_OK;       
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
				g95_expr *op1, g95_expr *op2) {  
g95_expr temp, *result;  
int unary;
arith rc;          
          
  g95_clear_ts(&temp.ts);

  switch(operator) {       
  case INTRINSIC_NOT:    /* Logical unary */ 
    if (op1->ts.type != BT_LOGICAL) goto runtime;         
         
    temp.ts.type = BT_LOGICAL;   
    temp.ts.kind = g95_default_logical_kind();       
       
    unary = 1;          
    break; 
 
    /* Logical binary operators */          
  case INTRINSIC_OR:     case INTRINSIC_AND:   
  case INTRINSIC_NEQV:   case INTRINSIC_EQV:      
    if (op1->ts.type != BT_LOGICAL || op2->ts.type != BT_LOGICAL)    
      goto runtime; 
 
    temp.ts.type = BT_LOGICAL;   
    temp.ts.kind = g95_default_logical_kind(); 
 
    unary = 0;        
    break;        
        
  case INTRINSIC_UPLUS:   case INTRINSIC_UMINUS:  /* Numeric unary */      
    if (!g95_numeric_ts(&op1->ts)) goto runtime;      
      
    temp.ts = op1->ts;   
   
    unary = 1;          
    break; 
 
  case INTRINSIC_GE:  case INTRINSIC_LT:  /* Additional restrictions */    
  case INTRINSIC_LE:  case INTRINSIC_GT:  /* for ordering relations */          
    if (op1->ts.type == BT_COMPLEX || op2->ts.type == BT_COMPLEX) {         
      temp.ts.type = BT_LOGICAL;    
      temp.ts.kind = g95_default_logical_kind();        
      goto runtime;   
    }

    /* Fall through */   
   
  case INTRINSIC_EQ:      case INTRINSIC_NE:      
    if (op1->ts.type == BT_CHARACTER && op2->ts.type == BT_CHARACTER) {        
      unary = 0;          
      temp.ts.type = BT_LOGICAL;        
      temp.ts.kind = g95_default_logical_kind();        
      break; 
    }  
  
    /* Fall through */ 
 
  case INTRINSIC_PLUS:    case INTRINSIC_MINUS:   case INTRINSIC_TIMES:
  case INTRINSIC_DIVIDE:  case INTRINSIC_POWER:   /* Numeric binary */          
    if (!g95_numeric_ts(&op1->ts) || !g95_numeric_ts(&op2->ts))
      goto runtime; 
 
    /* Insert any necessary type conversions to make the operands compatible */       
       
    temp.type = EXPR_OP;        
    g95_clear_ts(&temp.ts);     
    temp.operator = operator;    
    
    temp.op1 = op1;
    temp.op2 = op2;     
     
    g95_type_convert_binary(&temp);         
         
    if (operator == INTRINSIC_EQ || operator == INTRINSIC_NE ||  
	operator == INTRINSIC_GE || operator == INTRINSIC_GT ||       
	operator == INTRINSIC_LE || operator == INTRINSIC_LT) {      
      temp.ts.type = BT_LOGICAL;        
      temp.ts.kind = g95_default_logical_kind(); 
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
    if (op1->ts.type != BT_CHARACTER || op2->ts.type != BT_CHARACTER)    
      goto runtime;          
          
    temp.ts.type = BT_CHARACTER;     
    temp.ts.kind = g95_default_character_kind();    
    
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
 
  if (op1->type != EXPR_CONSTANT &&          
      (op1->type != EXPR_ARRAY || !g95_is_constant_expr(op1) ||       
       !g95_expanded_ac(op1))) 
    goto runtime;  
  
  if (op2 != NULL && op2->type != EXPR_CONSTANT &&      
      (op2->type != EXPR_ARRAY || !g95_is_constant_expr(op2) ||         
       !g95_expanded_ac(op2)))        
    goto runtime;      
      
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

runtime:       
  result = g95_get_expr();      
  result->ts = temp.ts;      
      
  result->type = EXPR_OP;
  result->operator = operator;     
     
  result->op1 = op1;    
  result->op2 = op2;      
      
  result->where = op1->where;        
        
  return result;    
}    
    
    
     
     
arith g95_arith_divide(g95_expr *op1, g95_expr *op2, g95_expr **resultp) {      
g95_expr *result;
mpf_t n, k, div; 
arith rc;       
       
  rc = ARITH_OK;   
   
  result = g95_constant_result(op1->ts.type, op1->ts.kind, &op1->where);          
          
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
    
    mpf_init(n);      
    mpf_init(k);   
    mpf_init(div);      
      
    mpf_mul(n, op2->value.complex.r, op2->value.complex.r);    
    mpf_mul(k, op2->value.complex.i, op2->value.complex.i);        
    mpf_add(div, n, k);

    mpf_mul(n, op1->value.complex.r, op2->value.complex.r); 
    mpf_mul(k, op1->value.complex.i, op2->value.complex.i); 
    mpf_add(result->value.complex.r, n, k);    
    mpf_div(result->value.complex.r, result->value.complex.r, div);  
  
    mpf_mul(n, op1->value.complex.i, op2->value.complex.r); 
    mpf_mul(k, op1->value.complex.r, op2->value.complex.i);      
    mpf_sub(result->value.complex.r, n, k);      
    mpf_div(result->value.complex.r, result->value.complex.r, div);   
   
    mpf_clear(n);     
    mpf_clear(k);   
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
    
    
        
        
/* g95_int2int()-- Convert integers to integers */        
        
g95_expr *g95_int2int(g95_expr *src, int kind) {   
g95_expr *result;
arith rc;   
   
  result = g95_constant_result(BT_INTEGER, kind, &src->where);       
       
  mpz_set(result->value.integer, src->value.integer);    
    
  if ((rc = g95_check_integer_range(result->value.integer, kind))         
      != ARITH_OK) {   
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
    
  result = g95_constant_result(BT_COMPLEX, kind, &src->where);      
      
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
        
        
         
         
/* g95_log2log()-- Logical kind conversion. */   
   
g95_expr *g95_log2log(g95_expr *src, int kind) {   
g95_expr *result;      
      
  result = g95_constant_result(BT_LOGICAL, kind, &src->where);      
  result->value.logical = src->value.logical;       
       
  return result;    
}   
   
   
   
 
 
static g95_expr *eval_intrinsic_f2(g95_intrinsic_op operator,    
				   arith (*eval)(g95_expr *, g95_expr **),     
				   g95_expr *op1, g95_expr *op2) {        
g95_expr *result;    
    
  if (op2 == NULL) {
    if (g95_zero_size_array(op1)) return eval_type_intrinsic0(operator, op1);  
  } else {         
    result = reduce_binary0(op1, op2);     
    if (result != NULL) return eval_type_intrinsic0(operator, result); 
  } 
 
  return eval_intrinsic(operator, eval, op1, op2);   
}          
          
          
       
       
/* g95_convert_real()-- Convert a real string to an expression node. */ 
 
g95_expr *g95_convert_real(char *buffer, int kind, locus *where) {          
g95_expr *r;   
   
  r = g95_constant_result(BT_REAL, kind, where);
  mpf_set_str(r->value.real, buffer, 10);      
      
  return r; 
}


         
         
arith g95_arith_times(g95_expr *op1, g95_expr *op2, g95_expr **resultp) {    
g95_expr *result;          
mpf_t o, b;    
arith rc;       
       
  result = g95_constant_result(op1->ts.type, op1->ts.kind, &op1->where); 
 
  switch(op1->ts.type) {         
  case BT_INTEGER:     
    mpz_mul(result->value.integer, op1->value.integer, op2->value.integer);      
    break; 
 
  case BT_REAL:
    mpf_mul(result->value.real, op1->value.real, op2->value.real);
    break;     
     
  case BT_COMPLEX:         
    mpf_init(o);   
    mpf_init(b);          
          
    mpf_mul(o, op1->value.complex.r, op2->value.complex.r);  
    mpf_mul(b, op1->value.complex.i, op2->value.complex.i);         
    mpf_sub(result->value.complex.r, o, b);

    mpf_mul(o, op1->value.complex.r, op2->value.complex.i);       
    mpf_mul(b, op1->value.complex.i, op2->value.complex.r);          
    mpf_add(result->value.complex.i, o, b);          
          
    mpf_clear(o);    
    mpf_clear(b);     
     
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
     
     
       
       
arith g95_arith_ge(g95_expr *op1, g95_expr *op2, g95_expr **resultp) {   
g95_expr *result;      
      
  result = g95_constant_result(BT_LOGICAL, g95_default_logical_kind(),   
			       &op1->where);      
  result->value.logical = (g95_compare_expr(op1, op2) >= 0);  
  *resultp = result;    
    
  return ARITH_OK;     
}        
        
    
    
/* g95_int2complex()-- Convert default integer to default complex */     
     
g95_expr *g95_int2complex(g95_expr *src, int kind) {    
g95_expr *result;    
arith rc;   
   
  result = g95_constant_result(BT_COMPLEX, kind, &src->where);          
          
  mpf_set_z(result->value.complex.r, src->value.integer);   
  mpf_set_ui(result->value.complex.i, 0);    
    
  if ((rc = g95_check_real_range(result->value.complex.i, kind)) != ARITH_OK) {  
    arith_error(rc, &src->ts, &result->ts, &src->where);   
    g95_free_expr(result);       
    return NULL;         
  }         
         
  return result;          
}        
        
        
          
          
g95_expr *g95_uplus(g95_expr *op) {    
  return eval_intrinsic_f2(INTRINSIC_UPLUS, g95_arith_uplus, op, NULL);          
}     
     


/* g95_complex2real()-- Convert complex to real */      
      
g95_expr *g95_complex2real(g95_expr *src, int kind) { 
g95_expr *result;   
arith rc;   
   
  result = g95_constant_result(BT_REAL, kind, &src->where);         
         
  mpf_set(result->value.real, src->value.complex.r);         
         
  if ((rc = g95_check_real_range(result->value.real, kind)) != ARITH_OK) {      
    arith_error(rc, &src->ts, &result->ts, &src->where);        
    g95_free_expr(result);
    return NULL;          
  } 
 
  return result;    
}   
   
   
         
         
g95_expr *g95_uminus(g95_expr *op) {   
  return eval_intrinsic_f2(INTRINSIC_UMINUS, g95_arith_uminus, op, NULL);          
}     
     
 
 
int g95_default_character_kind(void) { 
 
  return 1;         
} 
 
         
         
/* g95_real2real()-- Convert real to real */          
          
g95_expr *g95_real2real(g95_expr *src, int kind) { 
g95_expr *result;    
arith rc;       
       
  result = g95_constant_result(BT_REAL, kind, &src->where); 
 
  mpf_set(result->value.real, src->value.real);        
        
  if ((rc = g95_check_real_range(result->value.real, kind)) != ARITH_OK) {          
    arith_error(rc, &src->ts, &result->ts, &src->where);       
    g95_free_expr(result);    
    return NULL;       
  } 
 
  return result;       
}   
   
   
        
        
static g95_expr *eval_intrinsic_f3(g95_intrinsic_op operator,          
				   arith (*eval)(g95_expr *, g95_expr *,  
						 g95_expr **),          
				   g95_expr *op1, g95_expr *op2) {          
g95_expr *result;     
     
  result = reduce_binary0(op1, op2);  
  if (result != NULL) return eval_type_intrinsic0(operator, result);        
        
  return eval_intrinsic(operator, eval, op1, op2);        
}       
       
       
 
 
g95_expr *g95_le(g95_expr *op1, g95_expr *op2) {        
  return eval_intrinsic_f3(INTRINSIC_LE, g95_arith_le, op1, op2);    
}

        
        
g95_expr *g95_gt(g95_expr *op1, g95_expr *op2) {         
  return eval_intrinsic_f3(INTRINSIC_GT, g95_arith_gt, op1, op2);      
}    
    
          
          
g95_expr *g95_or(g95_expr *op1, g95_expr *op2) {      
  return eval_intrinsic_f3(INTRINSIC_OR, g95_arith_or, op1, op2); 
}  
  
   
   
g95_expr *g95_ne(g95_expr *op1, g95_expr *op2) { 
  return eval_intrinsic_f3(INTRINSIC_NE, g95_arith_ne, op1, op2);       
}     
     
      
      
g95_expr *g95_multiply(g95_expr *op1, g95_expr *op2) {     
  return eval_intrinsic_f3(INTRINSIC_TIMES, g95_arith_times, op1, op2);   
}

 
 
g95_expr *g95_ge(g95_expr *op1, g95_expr *op2) {       
  return eval_intrinsic_f3(INTRINSIC_GE, g95_arith_ge, op1, op2);
}  
  
        
        
g95_expr *g95_and(g95_expr *op1, g95_expr *op2) {
  return eval_intrinsic_f3(INTRINSIC_AND, g95_arith_and, op1, op2); 
}       
       
       
       
g95_expr *g95_add(g95_expr *op1, g95_expr *op2) {          
  return eval_intrinsic_f3(INTRINSIC_PLUS, g95_arith_plus, op1, op2);   
}         
         
  
  
g95_expr *g95_eqv(g95_expr *op1, g95_expr *op2) {         
  return eval_intrinsic_f3(INTRINSIC_EQV, g95_arith_eqv, op1, op2);         
}          
          
        
        
g95_expr *g95_divide(g95_expr *op1, g95_expr *op2) {        
  return eval_intrinsic_f3(INTRINSIC_DIVIDE, g95_arith_divide, op1, op2);      
}        
        
 
 
g95_expr *g95_power(g95_expr *op1, g95_expr *op2) { 
  return eval_intrinsic_f3(INTRINSIC_POWER, g95_arith_power, op1, op2);
}   
   
     
     
g95_expr *g95_eq(g95_expr *op1, g95_expr *op2) {  
  return eval_intrinsic_f3(INTRINSIC_EQ, g95_arith_eq, op1, op2);    
}

  
  
g95_expr *g95_subtract(g95_expr *op1, g95_expr *op2) {      
  return eval_intrinsic_f3(INTRINSIC_MINUS, g95_arith_minus, op1, op2);     
} 
 
 
 
g95_expr *g95_neqv(g95_expr *op1, g95_expr *op2) { 
  return eval_intrinsic_f3(INTRINSIC_NEQV, g95_arith_neqv, op1, op2); 
}  
         
         
g95_expr *g95_not(g95_expr *op1) {     
  return eval_intrinsic_f2(INTRINSIC_NOT, g95_arith_not, op1, NULL);   
} 
 


g95_expr *g95_lt(g95_expr *op1, g95_expr *op2) {    
  return eval_intrinsic_f3(INTRINSIC_LT, g95_arith_lt, op1, op2);       
}       
       
        
        
g95_expr *g95_concat(g95_expr *op1, g95_expr *op2) {        
  return eval_intrinsic_f3(INTRINSIC_CONCAT, g95_arith_concat, op1, op2);         
}      
      
