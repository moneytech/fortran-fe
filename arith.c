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
     
     
     
     
/* eval_type_intrinsic0() -- modify type of expression for zero size array */    
    
static g95_expr *eval_type_intrinsic0(g95_intrinsic_op oper, g95_expr *operand){       
       
  if (operand == NULL) g95_internal_error("eval_type_intrinsic0(): op NULL");     
     
  switch(oper) {   
  case INTRINSIC_GE:  case INTRINSIC_LT: 
  case INTRINSIC_LE:  case INTRINSIC_GT:      
  case INTRINSIC_EQ:  case INTRINSIC_NE:    
    operand->ts.type = BT_LOGICAL;
    operand->ts.kind = g95_default_logical_kind();
    break;          
          
  default:     
    break;
  }       
       
  return operand;
}          
          
          
      
/******* Simplification of intrinsic functions with constant arguments *****/

/* arith_error()-- Deal with an arithmetic error. */  
  
static void arith_error(arith rc, g95_typespec *f, g95_typespec *d,  
			g95_locus *where) {          
          
  g95_error("%s converting %s to %s at %L", g95_arith_error(rc),
	    g95_typename(f), g95_typename(d), where);       
       
  /* TODO: Do something about the error, ie underflow rounds to 0,
   * throw exception, return NaN, etc. */
}    
    
    
         
         
/* g95_arith_error()-- Given an arithmetic error code, return a
 * pointer to a string that explains the error. */ 
 
char *g95_arith_error(arith codep) {        
char *q;        
        
  switch(codep) {          
  case ARITH_OK:              q = "Arithmetic OK"; break;   
  case ARITH_OVERFLOW:        q = "Arithmetic overflow"; break;          
  case ARITH_UNDERFLOW:       q = "Arithmetic underflow"; break;       
  case ARITH_DIV0:            q = "Division by zero"; break;          
  case ARITH_0TO0:            q = "Indeterminate form 0 ** 0"; break;    
  case ARITH_INCOMMENSURATE:  q = "Array operands are incommensurate"; break; 
  default: g95_internal_error("g95_arith_error(): Bad error code");      
  }          
          
  return q;    
}       
       
       
         
         
void common_logarithm(mpf_t *argum, mpf_t *result) {    
mpf_t i10, log10;        
        
  natural_logarithm(argum, result);        
        
  mpf_init_set_ui(i10, 10);  
  mpf_init(log10);   
  natural_logarithm(&i10, &log10);

  mpf_div(*result, *result, log10);        
  mpf_clear(i10);        
  mpf_clear(log10); 
}


   
   
/* g95_arith_done_1()-- Get rid of numeric constants. */        
        
void g95_arith_done_1(void) {       
g95_integer_info *interp;       
g95_real_info *rp; 
 
  mpf_clear(e_value);     
     
  mpf_clear(pi);    
  mpf_clear(half_pi);       
  mpf_clear(two_pi);

  for(interp=g95_integer_kinds; interp->kind; interp++) {     
    mpz_clear(interp->min_int);        
    mpz_clear(interp->max_int);          
    mpz_clear(interp->huge);         
  }      
      
  for(rp=g95_real_kinds; rp->kind; rp++) {         
    mpf_clear(rp->epsilon);       
    mpf_clear(rp->huge);  
    mpf_clear(rp->tiny);     
  }  
}         
         
         
/* g95_default_*_kind()-- Return default kinds */

         
         
arith g95_arith_lt(g95_expr *op0, g95_expr *op2, g95_expr **rp) {
g95_expr *rslt;     
     
  rslt = g95_constant_result(BT_LOGICAL, g95_default_logical_kind(),      
			       &op0->where);    
  rslt->value.logical = (g95_compare_expr(op0, op2) < 0);   
  *rp = rslt;    
    
  return ARITH_OK;    
}         
         


/* g95_convert_integer()-- Convert an integer string to an expression
 * node */    
    
g95_expr *g95_convert_integer(char *buffer, int kind, int radix,     
			      g95_locus *where) {       
g95_expr *a;   
   
  a = g95_constant_result(BT_INTEGER, kind, where);         
  if (*buffer == '+') buffer++;         
  mpz_set_str(a->value.integer, buffer, radix);         
         
  return a;
} 
 
 


arith g95_arith_ge(g95_expr *op_1, g95_expr *op, g95_expr **r) { 
g95_expr *rslt;     
     
  rslt = g95_constant_result(BT_LOGICAL, g95_default_logical_kind(),       
			       &op_1->where);      
  rslt->value.logical = (g95_compare_expr(op_1, op) >= 0);         
  *r = rslt;      
      
  return ARITH_OK;          
}

  
  
static int validate_logical(int k0) { 
int k;    
    
  if ( g95_option.l1 == 1 ) {      
    if ( k0 == 1 ) { 
      k = 0;      
      return k;    
    }       
  }   
   
  for(k=0;; k++) {       
    if (g95_logical_kinds[k].kind == 0) { k = -1; break; }         
    if (g95_logical_kinds[k].kind == k0) break;          
  } 
 
  return k;   
}    
    
    
 
 
int g95_default_complex_kind(void) {        
        
  return g95_default_real_kind();
}       
       
       
          
          
/* g95_constant_result()-- Function to return a constant expression node
 * of a given type and kind. */          
          
g95_expr *g95_constant_result(bt typ, int k0, g95_locus *where) {     
g95_expr *rslt;   
   
  if (! where)       
    g95_internal_error("g95_constant_result(): g95_locus 'where' cannot be NULL");      
      
  rslt = g95_get_expr();         
         
  rslt->type = EXPR_CONSTANT;     
  rslt->ts.type = typ;
  rslt->ts.kind = k0;     
  rslt->where = *where;          
          
  switch(typ) {    
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
       
       
   
   
arith g95_arith_times(g95_expr *op1, g95_expr *op2, g95_expr **rp) { 
g95_expr *rslt;       
mpf_t d, l;       
arith rv;         
         
  rslt = g95_constant_result(op1->ts.type, op1->ts.kind, &op1->where);         
         
  switch(op1->ts.type) {       
  case BT_INTEGER:         
    mpz_mul(rslt->value.integer, op1->value.integer, op2->value.integer);         
    break;       
       
  case BT_REAL:  
    mpf_mul(rslt->value.real, op1->value.real, op2->value.real);          
    break;    
    
  case BT_COMPLEX:      
    mpf_init(d);   
    mpf_init(l); 
 
    mpf_mul(d, op1->value.complex.r, op2->value.complex.r);  
    mpf_mul(l, op1->value.complex.i, op2->value.complex.i);
    mpf_sub(rslt->value.complex.r, d, l);     
     
    mpf_mul(d, op1->value.complex.r, op2->value.complex.i);          
    mpf_mul(l, op1->value.complex.i, op2->value.complex.r);         
    mpf_add(rslt->value.complex.i, d, l); 
 
    mpf_clear(d);          
    mpf_clear(l); 
 
    break;  
  
  default:   
    g95_internal_error("g95_arith_times(): Bad basic type");
  }    
    
  rv = g95_range_check(rslt);  
  
  if (rv != ARITH_OK)      
    g95_free_expr(rslt);       
  else          
    *rp = rslt;   
   
  return rv;  
}


  
  
arith g95_arith_le(g95_expr *op0, g95_expr *op, g95_expr **resultp) {          
g95_expr *res;          
          
  res = g95_constant_result(BT_LOGICAL, g95_default_logical_kind(),    
			       &op0->where);      
  res->value.logical = (g95_compare_expr(op0, op) <= 0);        
  *resultp = res;      
      
  return ARITH_OK;         
}          
          


/* validate_integer()-- Make sure that a valid kind is present.
 * Returns an index into the g95_integer_kinds array, -1 if the kind
 * is not present. */  
  
static int validate_integer(int k0) {    
int a;        
        
  for(a=0;; a++) {          
    if (g95_integer_kinds[a].kind == 0) { a = -1; break; }    
    if (g95_integer_kinds[a].kind == k0) break; 
  }    
    
  return a;         
}      
      
      
 
 
void exponential(mpf_t *ap, mpf_t *result) {    
mpf_t two, ln2, power, k, d, num, den, term, h, xp;      
int a;      
long n; 
unsigned long u, mp;         
         
/* We use a reduction of the form
 *   x= Nln2 + r
 *   then obtain exp(r) from the McLaurin series.
 *   exp(x) is then recovered from the identity exp(x) = 2^N*exp(r) */      
      
  mpf_init_set(h, *ap);    
    
  if (mpf_cmp_ui(h, 0) == 0) { 
    mpf_set_ui(*result,1);          
  } else if (mpf_cmp_ui(h, 1) == 0) {    
    mpf_set(*result, e_value);       
  } else {  
    mpf_init_set_ui(two,2);        
    mpf_init(ln2);       
    mpf_init(k);   
    mpf_init(d);     
    mpf_init(power); 
    mpf_init(term);      
      
    natural_logarithm(&two, &ln2);

    mpf_div(k, h, ln2);   
    mpf_floor(power, k);   
    mpf_mul(k, power, ln2);     
    mpf_sub(d, h, k); 
 
    mpf_init_set_ui(xp, 1);    
    mpf_init_set_ui(num, 1);
    mpf_init_set_ui(den, 1);

    for(a=1; a<=G95_REAL_BITS+10; a++) { 
      mpf_mul(num, num, d);     
      mpf_mul_ui(den, den, a);
      mpf_div(term, num, den);         
      mpf_add(xp, xp, term);        
    }        
        
    /* Reconstruction step */          
    n = (long) mpf_get_d(power);      
      
    if (n > 0) {    
      u = (unsigned int) n;      
      mpf_mul_2exp(*result,xp,u);     
    } else {        
      mp = (unsigned int) (-n);    
      mpf_div_2exp(*result,xp,mp); 
    }    
    
    mpf_clear(two);        
    mpf_clear(ln2);     
    mpf_clear(k);  
    mpf_clear(d);         
    mpf_clear(power);     
    mpf_clear(num);       
    mpf_clear(den);   
    mpf_clear(term);   
    mpf_clear(xp); 
  }       
       
  mpf_clear(h);
}   
   
   
     
     
static int validate_real(int kind) {      
int z;      
      
  for(z=0;; z++) {
    if (g95_real_kinds[z].kind == 0) { z = -1; break; }     
    if (g95_real_kinds[z].kind == kind) break;    
  }

  return z;    
}          
          
          
       
       
/* g95_convert_real()-- Convert a real string to an expression node. */        
        
g95_expr *g95_convert_real(char *buffer, int k, g95_locus *where) {        
g95_expr *r;       
       
  r = g95_constant_result(BT_REAL, k, where);       
  if (*buffer == '+') buffer++;         
  mpf_set_str(r->value.real, buffer, 10);          
          
  return r;     
}      
      
      
      
      
arith g95_arith_plus(g95_expr *op_1, g95_expr *op0, g95_expr **rp) {  
g95_expr *result; 
arith rc;       
       
  result = g95_constant_result(op_1->ts.type, op_1->ts.kind, &op_1->where);     
     
  switch(op_1->ts.type) {   
  case BT_INTEGER: 
    mpz_add(result->value.integer, op_1->value.integer, op0->value.integer);          
    break;    
    
  case BT_REAL:   
    mpf_add(result->value.real, op_1->value.real, op0->value.real);        
    break; 
 
  case BT_COMPLEX:        
    mpf_add(result->value.complex.r, op_1->value.complex.r,
	    op0->value.complex.r);    
    
    mpf_add(result->value.complex.i, op_1->value.complex.i,    
	    op0->value.complex.i);   
    break;          
          
  default:       
    g95_internal_error("g95_arith_plus(): Bad basic type");
  }          
          
  rc = g95_range_check(result);   
   
  if (rc != ARITH_OK)      
    g95_free_expr(result);  
  else   
    *rp = result; 
 
  return rc;
}        
        
        
  
  
/* hyperbolic cosine */         
         
void hypercos(mpf_t *ap, mpf_t *rslt) {          
mpf_t neg, term1, term2, b, xp;    
    
  mpf_init_set(b, *ap);  
  
  mpf_init(neg);     
  mpf_init(term1);    
  mpf_init(term2);    
  mpf_init(xp); 
 
  mpf_neg(neg, b);          
          
  exponential(&b, &term1);       
  exponential(&neg, &term2); 
 
  mpf_add(xp, term1, term2);
  mpf_div_ui(*rslt, xp, 2);      
      
  mpf_clear(neg);  
  mpf_clear(term1);          
  mpf_clear(term2);       
  mpf_clear(b);   
  mpf_clear(xp);         
}


void hypersine(mpf_t *ap, mpf_t *rslt) {          
mpf_t neg, term1, term2, b, xp;       
       
  mpf_init_set(b, *ap);        
        
  mpf_init(neg); 
  mpf_init(term1);       
  mpf_init(term2);         
  mpf_init(xp);      
      
  mpf_neg(neg, b);

  exponential(&b, &term1);
  exponential(&neg, &term2);         
         
  mpf_sub(xp, term1, term2);
  mpf_div_ui(*rslt, xp, 2);  
  
  mpf_clear(neg);
  mpf_clear(term1);       
  mpf_clear(term2);       
  mpf_clear(b);   
  mpf_clear(xp); 
}          
          
          
 
 
arith g95_arith_minus(g95_expr *op0, g95_expr *op, g95_expr **resp) {      
g95_expr *rslt;  
arith rv;   
   
  rslt = g95_constant_result(op0->ts.type, op0->ts.kind, &op0->where);    
    
  switch(op0->ts.type) {
  case BT_INTEGER:     
    mpz_sub(rslt->value.integer, op0->value.integer, op->value.integer);          
    break;     
     
  case BT_REAL:     
    mpf_sub(rslt->value.real, op0->value.real, op->value.real);        
    break;          
          
  case BT_COMPLEX:  
    mpf_sub(rslt->value.complex.r, op0->value.complex.r,       
	    op->value.complex.r);    
    
    mpf_sub(rslt->value.complex.i, op0->value.complex.i,        
	    op->value.complex.i);         
         
    break;         
         
  default:
    g95_internal_error("g95_arith_minus(): Bad basic type");     
  }    
    
  rv = g95_range_check(rslt);      
      
  if (rv != ARITH_OK)       
    g95_free_expr(rslt);         
  else 
    *resp = rslt; 
 
  return rv;
}    
    
    


void sine(mpf_t *a, mpf_t *result) {          
mpf_t factor, d, w, num, den, term0, h, xp;       
int f, sign;          
          
/* We use a reduction of the form
 *   x= N*2pi + r
 *   then obtain sin(r) from the McLaurin series. */          
          
  mpf_init_set(h, *a);         
         
/* Special case (we do not treat multiples of pi due to roundoff issues) */          
  if (mpf_cmp_ui(h,0) ==  0) {        
    mpf_set_ui(*result,0);     
  } else {   
    mpf_init(d);  
    mpf_init(w); 
    mpf_init(factor);
    mpf_init(term0);  
  
    mpf_div(d, h, two_pi);    
    mpf_floor(factor, d);  
    mpf_mul(d, factor, two_pi);    
    mpf_sub(w, h, d);  
  
    mpf_init_set_ui(xp, 0);       
    mpf_init_set_ui(num, 1);  
    mpf_init_set_ui(den, 1);

    sign = -1;       
    for(f=1; f<G95_REAL_BITS+10; f++) {    
      mpf_mul(num, num, w);         
      mpf_mul_ui(den, den, f);  
      if (f%2 == 0) continue;     
     
      sign = -sign;
      mpf_div(term0, num, den);  
      if (sign > 0)         
	mpf_add(xp, xp, term0);        
      else      
	mpf_sub(xp,xp,term0);      
    }          
          
    mpf_set(*result, xp);   
   
    mpf_clear(d);       
    mpf_clear(w);          
    mpf_clear(factor);         
    mpf_clear(num);       
    mpf_clear(den); 
    mpf_clear(term0);       
    mpf_clear(xp);   
  }

  mpf_clear(h);         
}        
        
        
         
         
/* g95_compare_string()-- Given two constant strings and the inverse
 * collating sequence, compare the strings.  We return -1 for a<b,
 * 0 for a==b and 1 for a>b.  If the xcoll_table is NULL, we use the
 * processor's default collating sequence. */  
  
int g95_compare_string(g95_expr *m, g95_expr *e, int *xcoll_table) {        
int leng, alen, blen, l, ac, bc;   
   
  alen = m->value.character.length;         
  blen = e->value.character.length;

  leng = (alen > blen) ? alen : blen;    
    
  for(l=0; l<leng; l++) {      
    ac = (l < alen) ? m->value.character.string[l] : ' ';         
    bc = (l < blen) ? e->value.character.string[l] : ' ';         
         
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
    
    
        
        
arith g95_arith_divide(g95_expr *op, g95_expr *op0, g95_expr **resultp) {         
g95_expr *res; 
mpf_t q, g, div;     
arith rv;         
         
  rv = ARITH_OK;  
  
  res = g95_constant_result(op->ts.type, op->ts.kind, &op->where);  
  
  switch(op->ts.type) {        
  case BT_INTEGER:         
    if (mpz_sgn(op0->value.integer) == 0) {
      rv = ARITH_DIV0;         
      break;  
    }      
      
    mpz_tdiv_q(res->value.integer, op->value.integer,         
	       op0->value.integer);  
    break;        
        
  case BT_REAL:       
    if (mpf_sgn(op0->value.real) == 0) {          
      rv = ARITH_DIV0; 
      break;  
    } 
 
    mpf_div(res->value.real, op->value.real, op0->value.real);        
    break;         
         
  case BT_COMPLEX:      
    if (mpf_sgn(op0->value.complex.r) == 0 &&   
	mpf_sgn(op0->value.complex.i) == 0) {      
      rv = ARITH_DIV0;         
      break;         
    }

    mpf_init(q);        
    mpf_init(g); 
    mpf_init(div); 
 
    mpf_mul(q, op0->value.complex.r, op0->value.complex.r);  
    mpf_mul(g, op0->value.complex.i, op0->value.complex.i); 
    mpf_add(div, q, g);

    mpf_mul(q, op->value.complex.r, op0->value.complex.r);        
    mpf_mul(g, op->value.complex.i, op0->value.complex.i); 
    mpf_add(res->value.complex.r, q, g);    
    mpf_div(res->value.complex.r, res->value.complex.r, div);     
     
    mpf_mul(q, op->value.complex.i, op0->value.complex.r);          
    mpf_mul(g, op->value.complex.r, op0->value.complex.i);    
    mpf_sub(res->value.complex.i, q, g);     
    mpf_div(res->value.complex.i, res->value.complex.i, div);      
      
    mpf_clear(q);          
    mpf_clear(g);        
    mpf_clear(div);   
   
    break;    
    
  default:    
    g95_internal_error("g95_arith_divide(): Bad basic type"); 
  }     
     
  if (rv == ARITH_OK) rv = g95_range_check(res);     
     
  if (rv != ARITH_OK)      
    g95_free_expr(res);  
  else      
    *resultp = res;    
    
  return rv;         
}


         
         
int g95_default_double_kind(void) {  
  
  return g95_real_kinds[1].kind;         
}   
   
        
        
/* g95_arith_uplus()-- It may seem silly to have a subroutine that
 * actually computes the unary plus of a constant, but it prevents us
 * from making exceptions in the code elsewhere. */       
       
arith g95_arith_uplus(g95_expr *op0, g95_expr **resp) {         
         
  *resp = g95_copy_expr(op0); 
  return ARITH_OK;         
}         
         
         
   
   
int g95_default_logical_kind(void) {       
       
  return g95_logical_kinds[g95_option.i8 ? 1 : 0].kind;   
}         
         
         
         
int g95_default_real_kind(void) {      
      
  return g95_real_kinds[g95_option.r8 ? 1 : 0].kind;       
}     
     


void arctangent(mpf_t *args, mpf_t *res) {     
mpf_t absval, convgu, convgl, num, u, a, xp;       
int l, sign;    
    
/* Similar to sine routine but requires special handling for x near 1 */ 
 
  mpf_init_set(a, *args);        
        
/* Special cases */         
  if (mpf_cmp_ui(a, 0) == 0) {      
    mpf_set_ui(*res, 0);         
  } else if (mpf_cmp_ui(a,1) == 0) {    
    mpf_init(num);       
    mpf_div_ui(num, half_pi, 2);    
    mpf_set(*res, num);          
    mpf_clear(num); 
  } else if (mpf_cmp_si(a,-1) == 0) {   
    mpf_init(num);      
    mpf_div_ui(num, half_pi, 2);  
    mpf_neg(*res, num);  
    mpf_clear(num);    
  } else { /* General cases */         
         
    mpf_init(absval);   
    mpf_abs(absval, a);         
         
    mpf_init_set_d(convgu, 1.5);      
    mpf_init_set_d(convgl, 0.5);   
    mpf_init_set_ui(num, 1);     
    mpf_init(u);   
   
    if (mpf_cmp(absval, convgl) < 0) {
      mpf_init_set_ui(xp, 0);          
      sign = -1;    
      for(l=1; l<G95_REAL_BITS+10; l++) {  
        mpf_mul(num, num, absval);
	if (l%2 == 0) continue;       
       
	sign = -sign;     
	mpf_div_ui(u, num, l);    
	if (sign > 0)       
	  mpf_add(xp, xp, u);      
	else   
	  mpf_sub(xp, xp, u);         
      }    
    } else if (mpf_cmp(absval, convgu) >= 0) {          
      mpf_init_set(xp, half_pi);     
      sign = 1;     
      for(l=1; l<G95_REAL_BITS+10; l++) {
        mpf_div(num, num, absval);
	if (l%2 == 0) continue;          
          
	sign = -sign;       
	mpf_div_ui(u, num, l);   
	if (sign > 0)     
	  mpf_add(xp, xp, u);    
	else 
	  mpf_sub(xp, xp, u);      
      } 
    } else { 
      mpf_init_set_ui(xp, 0);

      mpf_sub_ui(num, absval, 1);      
      mpf_add_ui(u, absval, 1);    
      mpf_div(absval, num, u);   
   
      mpf_set_ui(num, 1);          
          
      sign = -1;    
      for(l=1; l<G95_REAL_BITS+10; l++) {   
        mpf_mul(num, num, absval);    
	if (l%2 == 0) continue;      
	sign = -sign;         
	mpf_div_ui(u, num, l);  
	if (sign > 0)  
	  mpf_add(xp, xp, u);          
	else       
	  mpf_sub(xp, xp, u);          
      }      
      
      mpf_div_ui(u, half_pi, 2);
      mpf_add(xp, u, xp);         
    }   
   
  /* This makes sure to preserve the identity arctan(-x) = -arctan(x) */   
  /* and improves accuracy to boot */        
        
    if (mpf_cmp_ui(a, 0) > 0)          
      mpf_set(*res, xp);       
    else   
      mpf_neg(*res, xp);          
          
    mpf_clear(absval);  
    mpf_clear(convgl);          
    mpf_clear(convgu);  
    mpf_clear(num);  
    mpf_clear(u); 
    mpf_clear(xp);       
  }  
  mpf_clear(a);         
}     
     
     
    
    
void cosine(mpf_t *ap, mpf_t *result) {
mpf_t factor, u, m, num, denom, term0, o, xp;         
int g, sign;       
       
/* Similar to sine routine */       
       
  mpf_init_set(o, *ap);   
   
/* Special case (we do not treat multiples of pi due to roundoff issues) */         
  if (mpf_cmp_ui(o,0) == 0) { 
    mpf_set_ui(*result, 1);   
  } else {   
    mpf_init(u);
    mpf_init(m);      
    mpf_init(factor);   
    mpf_init(term0);    
    
    mpf_div(u, o, two_pi);    
    mpf_floor(factor, u);      
    mpf_mul(u, factor, two_pi);      
    mpf_sub(m, o, u); 
 
    mpf_init_set_ui(xp, 1);         
    mpf_init_set_ui(num, 1);      
    mpf_init_set_ui(denom, 1); 
 
    sign = 1;      
    for(g=1; g<G95_REAL_BITS+10; g++) {   
      mpf_mul(num, num, m);      
      mpf_mul_ui(denom, denom, g);      
      if (g%2 != 0) continue;  
  
      sign = -sign;   
      mpf_div(term0, num, denom);      
      if (sign > 0)   
	mpf_add(xp, xp, term0);          
      else          
	mpf_sub(xp, xp, term0);        
    }   
    mpf_set(*result, xp);   
   
    mpf_clear(u);     
    mpf_clear(m);
    mpf_clear(factor);
    mpf_clear(num);        
    mpf_clear(denom);  
    mpf_clear(term0);        
    mpf_clear(xp); 
  }    
    
  mpf_clear(o);        
}   
   
   
          
          
int g95_default_character_kind(void) { 
 
  return 1;   
} 
 
  
  
/* compare_complex()-- Compare a pair of complex numbers.  Naturally,
 * this is only for equality/nonequality. */      
      
static int compare_complex(g95_expr *op_1, g95_expr *op_2) {

  return (mpf_cmp(op_1->value.complex.r, op_2->value.complex.r) == 0 &&     
	  mpf_cmp(op_1->value.complex.i, op_2->value.complex.i) == 0);        
}  
  
  
    
static int validate_character(int kind) {     
     
  if (kind == g95_default_character_kind()) return 0;
  return -1;   
}      
      
      
   
   
/* complex_reciprocal()-- Compute the reciprocal of a complex number
 * (guaranteed nonzero) */     
     
static void complex_reciprocal(g95_expr *op) {        
mpf_t mod, n, result_r, result_i;        
        
  mpf_init(mod);         
  mpf_init(n);          
          
  mpf_mul(mod, op->value.complex.r, op->value.complex.r);          
  mpf_mul(n, op->value.complex.i, op->value.complex.i); 
  mpf_add(mod, mod, n);     
     
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
  mpf_clear(n);   
}   
   
   
          
          
/* g95_arith_init_1()-- Get things ready to do math. */       
       
void g95_arith_init_1(void) {      
g95_integer_info *int_info;          
g95_real_info *real_info;     
mpf_t q, z;    
mpz_t l;      
int k, d, lim;      
      
/* Set the default precision for GMP computations */   
  mpf_set_default_prec(G95_REAL_BITS+30);         
         
/* Calculate e, needed by the natural_logarithm() subroutine. */          
          
  mpf_init(z);     
  mpf_init_set_ui(e_value, 0); 
  mpf_init_set_ui(q, 1);   
   
  for(k=1; k<100; k++) {  
    mpf_add(e_value, e_value, q);        
    mpf_div_ui(q, q, k);   /* 1/(i!) */
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
  
  lim = (G95_REAL_BITS / 4) + 10;  /* (1/16)^n gives 4 bits per iteration */ 
 
  for(d=0; d<lim; d++) {          
    mpf_set_ui(z, 4);    
    mpf_div_ui(z, z, 8*d+1);  /* 4/(8n+1) */         
         
    mpf_set_ui(q, 2);     
    mpf_div_ui(q, q, 8*d+4);  /* 2/(8n+4) */          
    mpf_sub(z, z, q);          
          
    mpf_set_ui(q, 1);   
    mpf_div_ui(q, q, 8*d+5);  /* 1/(8n+5) */      
    mpf_sub(z, z, q);       
       
    mpf_set_ui(q, 1);          
    mpf_div_ui(q, q, 8*d+6);  /* 1/(8n+6) */  
    mpf_sub(z, z, q);  
  
    mpf_set_ui(q, 16);      
    mpf_pow_ui(q, q, d);      /* 16^n */          
          
    mpf_div(z, z, q);     
     
    mpf_add(pi, pi, z);         
  } 
 
  mpf_mul_ui(two_pi, pi, 2); 
  mpf_div_ui(half_pi, pi, 2);   
   
/* Convert the minimum/maximum values for each kind into their Gnu MP
 * representation. */

  mpz_init(l); 
 
  for(int_info=g95_integer_kinds; int_info->kind != 0; int_info++) {      
    /* Huge */   
   
    mpz_set_ui(l, int_info->radix);         
    mpz_pow_ui(l, l, int_info->digits);      
      
    mpz_init(int_info->huge);   
    mpz_sub_ui(int_info->huge, l, 1);   
   
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
     
    mpf_set_z(q, int_info->huge);          
    common_logarithm(&q, &q);        
    mpf_trunc(q, q);         
    mpz_set_f(l, q);     
    int_info->range = mpz_get_si(l);      
  }          
          
/*  mpf_set_default_prec(G95_REAL_BITS); */   
   
  for(real_info=g95_real_kinds; real_info->kind != 0; real_info++) {       
    /* Huge */  
  
    mpf_set_ui(q, real_info->radix);       
    mpf_set_ui(z, real_info->radix);       
       
    mpf_pow_ui(q, q, real_info->max_exponent);  
    mpf_pow_ui(z, z, real_info->max_exponent - real_info->digits);    
    
    mpf_init(real_info->huge);     
    mpf_sub(real_info->huge, q, z);       
       
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
        
    common_logarithm(&real_info->huge, &q);        
    common_logarithm(&real_info->tiny, &z);          
    mpf_neg(z, z);      
      
    if (mpf_cmp(q, z) > 0) mpf_set(q, z);  /* a = min(a, b) */  
  
    mpf_trunc(q, q);         
    mpz_set_f(l, q);
    real_info->range = mpz_get_si(l);        
        
    /* Precision */       
       
    mpf_set_ui(q, real_info->radix);          
    common_logarithm(&q, &q);    
    
    mpf_mul_ui(q, q, real_info->digits-1);    
    mpf_trunc(q, q);    
    mpz_set_f(l, q); 
    real_info->precision = mpz_get_si(l);

    /* If the radix is an integral power of 10, add one to the precision. */         
         
    for(k=10; k<=real_info->radix; k*=10)  
      if (k == real_info->radix) real_info->precision++;    
  } 
 
  mpz_clear(l);      
  mpf_clear(q);    
  mpf_clear(z); 
}         
         
         
 
 
int g95_default_integer_kind(void) {         
         
  return g95_integer_kinds[g95_option.i8 ? 1 : 0].kind;  
}          
          


/* g95_check_integer_range()-- Given an integer and a kind, make sure
 * that the integer lies within the range of the kind.  Returns
 * ARITH_OK or ARITH_OVERFLOW. */         
         
static arith g95_check_integer_range(mpz_t c, int k) {         
arith r;
int n;          
          
  n = validate_integer(k);          
  if (n == -1) g95_internal_error("g95_check_integer_range(): Bad kind");

  r = ARITH_OK;   
   
  if (mpz_cmp(c, g95_integer_kinds[n].min_int) < 0 ||       
      mpz_cmp(c, g95_integer_kinds[n].max_int) > 0) r = ARITH_OVERFLOW;   
   
  return r;         
}    
    
    
     
     
arith g95_arith_ne(g95_expr *op_1, g95_expr *op0, g95_expr **rp) {    
g95_expr *res;      
      
  res = g95_constant_result(BT_LOGICAL, g95_default_logical_kind(),      
			       &op_1->where);       
  res->value.logical = (op_1->ts.type == BT_COMPLEX) ?    
    !compare_complex(op_1, op0) : (g95_compare_expr(op_1, op0) != 0);  
  
  *rp = res;         
  return ARITH_OK;     
}   
   
  
  
/* g95_log2log()-- Logical kind conversion. */        
        
g95_expr *g95_log2log(g95_expr *s, int k) {         
g95_expr *res;      
      
  res = g95_constant_result(BT_LOGICAL, k, &s->where);          
  res->value.logical = s->value.logical;          
          
  return res;        
}



         
         
static arith reduce_binary_aa(arith (*eval)(), g95_expr *op_1, g95_expr *op2,   
			      g95_expr **result) {  
g95_constructor *a, *g, *head;          
g95_expr *f; 
arith rv;      
      
  head = g95_copy_constructor(op_1->value.constructor);         
         
  rv = ARITH_OK;   
  g = op2->value.constructor;      
      
  if (g95_check_conformance("Elemental binary operation", op_1, op2)           
      != SUCCESS)      
    rv = ARITH_INCOMMENSURATE;    
  else {  
  
    for(a=head; a; a=a->next, g=g->next) {      
      if (g == NULL) {      
	rv = ARITH_INCOMMENSURATE;     
	break;
      } 
 
      rv = eval(a->expr, g->expr, &f);       
      if (rv != ARITH_OK) break;       
             
      g95_replace_expr(a->expr, f);        
    }    
    
    if (g != NULL) rv = ARITH_INCOMMENSURATE;       
  }       
       
  if (rv != ARITH_OK)    
    g95_free_constructor(head);     
  else {     
    f = g95_get_expr(); 
    f->type = EXPR_ARRAY;
    f->value.constructor = head;          
    f->shape = g95_copy_shape(op_1->shape, op_1->rank);    
    
    f->ts = head->expr->ts;
    f->where = op_1->where;
    f->rank = op_1->rank;         
         
    *result = f;         
  }          
          
  return rv;         
}    
    
    
         
         
/* g95_int2int()-- Convert integers to integers */  
  
g95_expr *g95_int2int(g95_expr *s, int knd) {    
g95_expr *result;          
arith rc;

  result = g95_constant_result(BT_INTEGER, knd, &s->where);        
        
  mpz_set(result->value.integer, s->value.integer);          
          
  if ((rc = g95_check_integer_range(result->value.integer, knd)) 
      != ARITH_OK) {   
    arith_error(rc, &s->ts, &result->ts, &s->where);         
    g95_free_expr(result);        
    return NULL;     
  }  
  
  return result;         
}   
   
   
  
  
/* complex_pow_ui()-- Raise a complex number to positive power */

static void complex_pow_ui(g95_expr *base, int power, g95_expr *rslt) {    
mpf_t temp_r, temp_i, h;   
   
  mpf_set_ui(rslt->value.complex.r, 1);        
  mpf_set_ui(rslt->value.complex.i, 0);     
     
  mpf_init(temp_r); 
  mpf_init(temp_i);         
  mpf_init(h);          
          
  for(; power>0; power--) {        
    mpf_mul(temp_r, base->value.complex.r, rslt->value.complex.r);          
    mpf_mul(h,      base->value.complex.i, rslt->value.complex.i);    
    mpf_sub(temp_r, temp_r, h);   
   
    mpf_mul(temp_i, base->value.complex.r, rslt->value.complex.i);          
    mpf_mul(h,      base->value.complex.i, rslt->value.complex.r);
    mpf_add(temp_i, temp_i, h);        
        
    mpf_set(rslt->value.complex.r, temp_r);    
    mpf_set(rslt->value.complex.i, temp_i);         
  }    
    
  mpf_clear(temp_r);          
  mpf_clear(temp_i);
  mpf_clear(h);      
}         
         
         
     
     
/* g95_real2int()-- Convert default real to default integer */         
         
g95_expr *g95_real2int(g95_expr *source, int k0) {
g95_expr *res;
arith rv;  
  
  res = g95_constant_result(BT_INTEGER, k0, &source->where);         
         
  mpz_set_f(res->value.integer, source->value.real);      
      
  if ((rv = g95_check_integer_range(res->value.integer, k0))         
      != ARITH_OK) {   
    arith_error(rv, &source->ts, &res->ts, &source->where);       
    g95_free_expr(res);       
    return NULL;     
  } 
 
  return res;      
}          
          
          
       
       
/* g95_complex2int()-- Convert complex to integer */      
      
g95_expr *g95_complex2int(g95_expr *s1, int k0) {   
g95_expr *res;    
arith retval;          
          
  res = g95_constant_result(BT_INTEGER, k0, &s1->where);     
     
  mpz_set_f(res->value.integer, s1->value.complex.r);  
  
  if ((retval = g95_check_integer_range(res->value.integer, k0))         
      != ARITH_OK) {  
    arith_error(retval, &s1->ts, &res->ts, &s1->where);       
    g95_free_expr(res); 
    return NULL;  
  } 
 
  return res;          
}        
        
        
     
     
/* g95_check_real_range()-- Given a real and a kind, make sure that
 * the real lies within the range of the kind.  Returns ARITH_OK,
 * ARITH_OVERFLOW or ARITH_UNDERFLOW. */       
       
static arith g95_check_real_range(mpf_t e, int k) {       
arith retval;  
mpf_t n;       
int h;       
       
  mpf_init(n);  
  mpf_abs(n, e); 
 
  h = validate_real(k);         
  if (h == -1) g95_internal_error("g95_check_real_range(): Bad kind");         
         
  retval = ARITH_OK; 
  if (mpf_sgn(n) == 0) goto done;     
     
  if (mpf_cmp(n, g95_real_kinds[h].huge) == 1) {     
    retval = ARITH_OVERFLOW; 
    goto done;
  }     
     
  if (mpf_cmp(n, g95_real_kinds[h].tiny) == -1) retval = ARITH_UNDERFLOW;        
        
done:        
  mpf_clear(n); 
 
  return retval;        
}


  
  
/* reduce_binary0()-- Reduce a binary expression where at least one of
 * the operands involves a zero-length array.  Returns NULL if neither
 * of the operands is a zero-length array. */       
       
static g95_expr *reduce_binary0(g95_expr *op0, g95_expr *op) {

  if (g95_zero_size_array(op0)) {   
    g95_free_expr(op);          
    return op0;    
  }    
    
  if (g95_zero_size_array(op)) {       
    g95_free_expr(op0);
    return op;      
  } 
 
  return NULL;     
}      
      
      
         
         
/* g95_range_check()-- Make sure a constant numeric expression is
 * within the range for it's type and kind.
 * Note that there's also a g95_check_range(), but that one deals
 * with the intrinsic RANGE function. */         
         
arith g95_range_check(g95_expr *f) {     
arith rc;          
          
  switch(f->ts.type) {  
  case BT_INTEGER:       
    rc = g95_check_integer_range(f->value.integer, f->ts.kind);     
    break;  
  
  case BT_REAL:      
    rc = g95_check_real_range(f->value.real, f->ts.kind);   
    break;    
    
  case BT_COMPLEX:     
    rc = g95_check_real_range(f->value.complex.r, f->ts.kind);
    if (rc != ARITH_OK)      
      rc = g95_check_real_range(f->value.complex.i, f->ts.kind);  
  
    break; 
 
  default:     
    g95_internal_error("g95_range_check(): Bad type");       
  }        
        
  return rc;          
}   
   
   
    
    
/* g95_validate_kind()-- Validate a kind given a basic type.  The
 * return value is the same for the child functions, with -1
 * indicating nonexistence of the type */          
          
int g95_validate_kind(bt t, int k0) {
int retval;   
   
  switch(t) {  
  case BT_REAL:     /* Fall through */
  case BT_COMPLEX:    retval = validate_real(k0);      break;         
  case BT_INTEGER:    retval = validate_integer(k0);   break;  
  case BT_LOGICAL:    retval = validate_logical(k0);   break;    
  case BT_CHARACTER:  retval = validate_character(k0); break;        
        
  default:  
    g95_internal_error("g95_validate_kind(): Got bad type");       
  }     
     
  return retval;
}


    
    
/* g95_real2real()-- Convert real to real */    
    
g95_expr *g95_real2real(g95_expr *src, int k) {         
g95_expr *result; 
arith rc;        
        
  result = g95_constant_result(BT_REAL, k, &src->where);      
      
  mpf_set(result->value.real, src->value.real);  
  
  if ((rc = g95_check_real_range(result->value.real, k)) != ARITH_OK) {    
    arith_error(rc, &src->ts, &result->ts, &src->where); 
    g95_free_expr(result);          
    return NULL;    
  } 
 
  return result;     
}     
     
     
     
     
/* g95_complex2complex()-- Convert complex to complex */        
        
g95_expr *g95_complex2complex(g95_expr *s, int k) {
g95_expr *result;        
arith rc;   
   
  result = g95_constant_result(BT_COMPLEX, k, &s->where);      
      
  mpf_set(result->value.complex.r, s->value.complex.r);   
  mpf_set(result->value.complex.i, s->value.complex.i);          
          
  if ((rc = g95_check_real_range(result->value.complex.r, k)) != ARITH_OK ||          
      (rc = g95_check_real_range(result->value.complex.i, k)) != ARITH_OK) {     
    arith_error(rc, &s->ts, &result->ts, &s->where);     
    g95_free_expr(result);          
    return NULL;       
  }         
         
  return result;    
}  
  
  
    
    
static arith reduce_binary_ca(arith (*eval)(), g95_expr *op_1, g95_expr *op_2,       
			      g95_expr **res) {   
g95_constructor *a, *head;         
g95_expr *r;       
arith rc;        
        
  head = g95_copy_constructor(op_2->value.constructor);          
  rc = ARITH_OK; 
 
  for(a=head; a; a=a->next) {    
    rc = eval(op_1, a->expr, &r); 
    if (rc != ARITH_OK) break;  
  
    g95_replace_expr(a->expr, r);          
  }         
         
  if (rc != ARITH_OK)  
    g95_free_constructor(head);         
  else {
    r = g95_get_expr();  
    r->type = EXPR_ARRAY;    
    r->value.constructor = head;
    r->shape = g95_copy_shape(op_2->shape, op_2->rank);   
   
    r->ts = head->expr->ts;        
    r->where = op_2->where;
    r->rank = op_2->rank;    
    
    *res = r;        
  }   
   
  return rc;
}    
    
   
   
static arith reduce_unary(arith (*eval)(), g95_expr *operand, g95_expr **result) {    
g95_constructor *b, *h;
g95_expr *d;     
arith rc;    
    
  if (operand->type == EXPR_CONSTANT) return eval(operand, result);     
     
  rc = ARITH_OK;       
  h = g95_copy_constructor(operand->value.constructor); 
 
  for(b=h; b; b=b->next) { 
    rc = eval(b->expr, &d);       
    if (rc != ARITH_OK) break;   
   
    g95_replace_expr(b->expr, d);    
  }        
        
  if (rc != ARITH_OK)        
    g95_free_constructor(h);        
  else {   
    d = g95_get_expr();       
    d->type = EXPR_ARRAY; 
    d->value.constructor = h; 
    d->shape = g95_copy_shape(operand->shape, operand->rank);        
        
    d->ts = h->expr->ts;
    d->where = operand->where;         
    d->rank = operand->rank;         
         
    *result = d;  
  }      
      
  return rc;    
}    
    
    
    
    
/* g95_arith_concat()-- Concatenate two string constants */    
    
arith g95_arith_concat(g95_expr *op, g95_expr *op2, g95_expr **resultp) {      
g95_expr *res;      
int len;    
    
  res = g95_constant_result(BT_CHARACTER, g95_default_character_kind(),        
			       &op->where);        
        
  len = op->value.character.length + op2->value.character.length;          
          
  res->value.character.string = g95_getmem(len+1);  
  res->value.character.length = len;       
       
  memcpy(res->value.character.string, op->value.character.string,         
	 op->value.character.length);         
         
  memcpy(res->value.character.string + op->value.character.length,        
	 op2->value.character.string, op2->value.character.length);  
  
  res->value.character.string[len] = '\0';     
     
  *resultp = res;    
    
  return ARITH_OK;   
}       
       
       
    
    
/* g95_zero_size_array()-- Return nonzero if the expression is a zero
 * size array. */

int g95_zero_size_array(g95_expr *v) {

  if (v->type != EXPR_ARRAY) return 0; 
 
  return v->value.constructor == NULL;          
}


     
     
/* g95_int2real()-- Convert integers to reals */      
      
g95_expr *g95_int2real(g95_expr *src, int k) {         
g95_expr *result;        
arith retval; 
 
  result = g95_constant_result(BT_REAL, k, &src->where);

  mpf_set_z(result->value.real, src->value.integer);     
     
  if ((retval = g95_check_real_range(result->value.real, k)) != ARITH_OK) {
    arith_error(retval, &src->ts, &result->ts, &src->where); 
    g95_free_expr(result);   
    return NULL;  
  }  
  
  return result;     
}   
   
   
    
    
/* g95_real2complex()-- Convert real to complex. */      
      
g95_expr *g95_real2complex(g95_expr *s1, int knd) {       
g95_expr *result;        
arith rv;     
     
  result = g95_constant_result(BT_COMPLEX, knd, &s1->where);         
         
  mpf_set(result->value.complex.r, s1->value.real);          
  mpf_set_ui(result->value.complex.i, 0);  
  
  if ((rv = g95_check_real_range(result->value.complex.i, knd)) != ARITH_OK) {        
    arith_error(rv, &s1->ts, &result->ts, &s1->where);     
    g95_free_expr(result);  
    return NULL;    
  }   
   
  return result;       
} 
 
 
   
   
arith g95_arith_eq(g95_expr *op, g95_expr *op_2, g95_expr **resultp) {    
g95_expr *r;      
      
  r = g95_constant_result(BT_LOGICAL, g95_default_logical_kind(),  
			       &op->where);       
  r->value.logical = (op->ts.type == BT_COMPLEX) ?        
    compare_complex(op, op_2) : (g95_compare_expr(op, op_2) == 0);  
  
  *resultp = r;       
  return ARITH_OK;        
}        
        
      
      
arith g95_arith_gt(g95_expr *op1, g95_expr *op, g95_expr **resultp) {         
g95_expr *rslt;      
      
  rslt = g95_constant_result(BT_LOGICAL, g95_default_logical_kind(),         
			       &op1->where);
  rslt->value.logical = (g95_compare_expr(op1, op) > 0);        
  *resultp = rslt;    
    
  return ARITH_OK;       
}     
     
 
 
/* g95_complex2real()-- Convert complex to real */   
   
g95_expr *g95_complex2real(g95_expr *s, int k0) {  
g95_expr *rslt;      
arith rc;   
   
  rslt = g95_constant_result(BT_REAL, k0, &s->where);      
      
  mpf_set(rslt->value.real, s->value.complex.r);          
          
  if ((rc = g95_check_real_range(rslt->value.real, k0)) != ARITH_OK) {       
    arith_error(rc, &s->ts, &rslt->ts, &s->where);       
    g95_free_expr(rslt);    
    return NULL;       
  } 
 
  return rslt;        
}    
    
    
    
    
/* Low-level arithmetic functions.  All of these subroutines assume
 * that all operands are of the same type and return an operand of the
 * same type.  The other thing about these subroutines is that they
 * can fail in various ways-- overflow, underflow, division by zero,
 * zero raised to the zero, etc.  */       
       
arith g95_arith_not(g95_expr *op, g95_expr **resultp) {  
g95_expr *result;        
        
  result = g95_constant_result(BT_LOGICAL, op->ts.kind, &op->where);  
  result->value.logical = !op->value.logical;      
  *resultp = result;    
    
  return ARITH_OK;  
}      
      
      
arith g95_arith_and(g95_expr *op, g95_expr *op0, g95_expr **resultp) {      
g95_expr *result;       
       
  result = g95_constant_result(BT_LOGICAL, g95_kind_max(op, op0),  
			       &op->where); 
  result->value.logical = op->value.logical && op0->value.logical; 
  *resultp = result;  
  
  return ARITH_OK;   
}    
    
    
arith g95_arith_or(g95_expr *op, g95_expr *op0, g95_expr **resultp) {       
g95_expr *result;      
      
  result = g95_constant_result(BT_LOGICAL, g95_kind_max(op, op0),      
			       &op->where);   
  result->value.logical = op->value.logical || op0->value.logical;         
  *resultp = result;      
      
  return ARITH_OK;
}          
          
          
arith g95_arith_eqv(g95_expr *op, g95_expr *op0, g95_expr **resultp) { 
g95_expr *result; 
 
  result = g95_constant_result(BT_LOGICAL, g95_kind_max(op, op0),       
			       &op->where);          
  result->value.logical = op->value.logical == op0->value.logical;  
  *resultp = result;      
      
  return ARITH_OK;      
}       
       
       
arith g95_arith_neqv(g95_expr *op, g95_expr *op0, g95_expr **resultp) {     
g95_expr *result;     
     
  result = g95_constant_result(BT_LOGICAL, g95_kind_max(op, op0), 
			       &op->where);      
  result->value.logical = op->value.logical != op0->value.logical;  
  *resultp = result;        
        
  return ARITH_OK;    
} 
 
 
        
        
static arith reduce_binary_ac(arith (*eval)(), g95_expr *op, g95_expr *op2,        
			      g95_expr **rslt) {
g95_constructor *w, *head;     
g95_expr *r;          
arith retval;        
        
  head = g95_copy_constructor(op->value.constructor);    
  retval = ARITH_OK;  
  
  for(w=head; w; w=w->next) {      
    retval = eval(w->expr, op2, &r);         
    if (retval != ARITH_OK) break;

    g95_replace_expr(w->expr, r);    
  } 
 
  if (retval != ARITH_OK)          
    g95_free_constructor(head);    
  else { 
    r = g95_get_expr(); 
    r->type = EXPR_ARRAY;          
    r->value.constructor = head;      
    r->shape = g95_copy_shape(op->shape, op->rank);

    r->ts = head->expr->ts;         
    r->where = op->where;       
    r->rank = op->rank;       
       
    *rslt = r;   
  }      
      
  return retval; 
}   
   
   
  
  
/* g95_convert_complex()-- Convert a pair of real, constant expression
 * nodes to a single complex expression node. */    
    
g95_expr *g95_convert_complex(g95_expr *real, g95_expr *imag, int k0) {          
g95_expr *c;   
   
  c = g95_constant_result(BT_COMPLEX, k0, &real->where);     
  mpf_set(c->value.complex.r, real->value.real);      
  mpf_set(c->value.complex.i, imag->value.real);         
         
  return c;       
}          
          
          
        
        
/* g95_compare_expr()-- Comparison operators.  Assumes that the two
 * expression nodes contain two constants of the same type. */  
  
int g95_compare_expr(g95_expr *op_1, g95_expr *op_2) {      
int r;  
  
  switch(op_1->ts.type) {         
  case BT_INTEGER:     
    r = mpz_cmp(op_1->value.integer, op_2->value.integer);         
    break;

  case BT_REAL:    
    r = mpf_cmp(op_1->value.real, op_2->value.real); 
    break;         
         
  case BT_CHARACTER: 
    r = g95_compare_string(op_1, op_2, NULL);  
    break;    
    
  case BT_LOGICAL:   
    r = (!!op_1->value.logical) - (!!op_2->value.logical);       
       
    if (r < 0) r = -1;      
    if (r > 0) r = 1;
    break;         
         
  default: g95_internal_error("g95_compare_expr(): Bad basic type");
  }    
    
  return r;   
}       
       
       
  
  
/* natural_logarithm()-- Compute a natural logarithm */   
   
void natural_logarithm(mpf_t *argum, mpf_t *rslt) {
mpf_t f, xp, b, log;  
int j, z;

  mpf_init_set(f, *argum);
  mpf_init(b);         
         
  z = 0;   
   
  /* Get the argument into the range 0.5 to 1.5 by successive
   * multiplications or divisions by e. */         
         
  mpf_set_str(b, "0.5", 10);          
  while(mpf_cmp(f, b) < 0) {
    mpf_mul(f, f, e_value);       
    z--;     
  }    
    
  mpf_set_str(b, "1.5", 10);         
  while(mpf_cmp(f, b) > 0) {  
    mpf_div(f, f, e_value); 
    z++;     
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
          
  mpf_sub_ui(f, f, 1);         
  mpf_init_set_ui(log, 0);         
  mpf_init_set_ui(xp, 1);      
      
  for(j=1; j<G95_REAL_BITS; j++) {    
    mpf_mul(xp, xp, f);        
    mpf_div_ui(b, xp, j);          
          
    if (j % 2 == 0)
      mpf_sub(log, log, b);          
    else       
      mpf_add(log, log, b);   
  }    
    
  /* Add in the log (e^p) = p */          
          
  if (z < 0)         
    mpf_sub_ui(log, log, -z);
  else         
    mpf_add_ui(log, log, z);   
   
  mpf_clear(f);     
  mpf_clear(xp);
  mpf_clear(b);          
          
  mpf_set(*rslt, log);      
  mpf_clear(log);     
}  
  
  
    
    
/* g95_int2complex()-- Convert default integer to default complex */        
        
g95_expr *g95_int2complex(g95_expr *source, int knd) { 
g95_expr *rslt;    
arith rc;   
   
  rslt = g95_constant_result(BT_COMPLEX, knd, &source->where); 
 
  mpf_set_z(rslt->value.complex.r, source->value.integer);          
  mpf_set_ui(rslt->value.complex.i, 0);         
         
  if ((rc = g95_check_real_range(rslt->value.complex.i, knd)) != ARITH_OK) {     
    arith_error(rc, &source->ts, &rslt->ts, &source->where);       
    g95_free_expr(rslt);    
    return NULL;        
  }          
          
  return rslt;          
}  
  
  
   
   
/* g95_arith_power()-- Raise a number to an integer power */   
   
arith g95_arith_power(g95_expr *op1, g95_expr *op, g95_expr **resultp) {
int power, apower;
g95_expr *res; 
mpz_t unity_z;     
mpf_t unity_f;   
arith retval;      
      
  retval = ARITH_OK;      
      
  if (g95_extract_int(op, &power) != NULL)         
    g95_internal_error("g95_arith_power(): Bad exponent");     
     
  res = g95_constant_result(op1->ts.type, op1->ts.kind, &op1->where);       
       
  if (power == 0) {     /* Handle something to the zeroth power */      
    switch(op1->ts.type) {      
    case BT_INTEGER:  
      if (mpz_sgn(op1->value.integer) == 0)
	retval = ARITH_0TO0;       
      else          
	mpz_set_ui(res->value.integer, 1);    
	    
      break;         
         
    case BT_REAL:    
      if (mpf_sgn(op1->value.real) == 0) 
	retval = ARITH_0TO0;  
      else       
	mpf_set_ui(res->value.real, 1);         
         
      break;     
     
    case BT_COMPLEX:       
      if (mpf_sgn(op1->value.complex.r) == 0 &&
	  mpf_sgn(op1->value.complex.i) == 0)      
	retval = ARITH_0TO0;          
      else {    
	mpf_set_ui(res->value.complex.r, 1);        
	mpf_set_ui(res->value.complex.r, 0);     
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
      mpz_pow_ui(res->value.integer, op1->value.integer, apower);     
     
      if (power < 0) {     
	mpz_init_set_ui(unity_z, 1);         
	mpz_tdiv_q(res->value.integer, unity_z, res->value.integer);         
	mpz_clear(unity_z);          
      }    
    
      break; 
 
    case BT_REAL:       
      mpf_pow_ui(res->value.real, op1->value.real, apower);          
          
      if (power < 0) {        
	mpf_init_set_ui(unity_f, 1);      
	mpf_div(res->value.real, unity_f, res->value.real);          
	mpf_clear(unity_f);
      }   
   
      break;

    case BT_COMPLEX:          
      complex_pow_ui(op1, apower, res);   
      if (power < 0) complex_reciprocal(res);  
  
      break;

    default:  
      break;      
    }
  }         
         
  if (retval == ARITH_OK) retval = g95_range_check(res);      
      
  if (retval != ARITH_OK)   
    g95_free_expr(res);       
  else          
    *resultp = res;      
      
  return retval;  
}   
   
   
        
        
arith g95_arith_uminus(g95_expr *op1, g95_expr **rp) {  
g95_expr *rslt;        
arith rc;   
   
  rslt = g95_constant_result(op1->ts.type, op1->ts.kind, &op1->where);         
         
  switch(op1->ts.type) {  
  case BT_INTEGER:
    mpz_neg(rslt->value.integer, op1->value.integer);       
    break;   
   
  case BT_REAL:     
    mpf_neg(rslt->value.real, op1->value.real); 
    break;  
  
  case BT_COMPLEX:         
    mpf_neg(rslt->value.complex.r, op1->value.complex.r);     
    mpf_neg(rslt->value.complex.i, op1->value.complex.i);  
    break; 
 
  default:      
    g95_internal_error("g95_arith_uminus(): Bad basic type");         
  }  
  
  rc = g95_range_check(rslt); 
 
  if (rc != ARITH_OK)          
    g95_free_expr(rslt);
  else    
    *rp = rslt;          
          
  return rc;       
}         
         
         
      
      
static arith reduce_binary(arith (*eval)(), g95_expr *op1, g95_expr *op,    
			   g95_expr **rslt) {        
        
  if (op1->type == EXPR_CONSTANT && op->type == EXPR_CONSTANT)      
    return eval(op1, op, rslt);          
          
  if (op1->type == EXPR_CONSTANT && op->type == EXPR_ARRAY)        
    return reduce_binary_ca(eval, op1, op, rslt);

  if (op1->type == EXPR_ARRAY && op->type == EXPR_CONSTANT)      
    return reduce_binary_ac(eval, op1, op, rslt);  
  
  return reduce_binary_aa(eval, op1, op, rslt);          
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
				g95_expr *op, g95_expr *op0) {       
g95_expr temp, *result;     
int unary;  
arith retval;    
    
  g95_clear_ts(&temp.ts);    
    
  switch(oper) {   
  case INTRINSIC_NOT:    /* Logical unary */   
    if (op->ts.type != BT_LOGICAL) goto runtime;       
       
    temp.ts.type = BT_LOGICAL;    
    temp.ts.kind = g95_default_logical_kind(); 
 
    unary = 1;    
    break;  
  
    /* Logical binary operators */  
  case INTRINSIC_OR:     case INTRINSIC_AND:          
  case INTRINSIC_NEQV:   case INTRINSIC_EQV:    
    if (op->ts.type != BT_LOGICAL || op0->ts.type != BT_LOGICAL)    
      goto runtime;  
  
    temp.ts.type = BT_LOGICAL;        
    temp.ts.kind = g95_default_logical_kind();

    unary = 0;     
    break;

  case INTRINSIC_UPLUS:   case INTRINSIC_UMINUS:  /* Numeric unary */         
    if (!g95_numeric_ts(&op->ts)) goto runtime;  
  
    temp.ts = op->ts;   
   
    unary = 1;   
    break;          
          
  case INTRINSIC_GE:  case INTRINSIC_LT:  /* Additional restrictions */
  case INTRINSIC_LE:  case INTRINSIC_GT:  /* for ordering relations */         
    if (op->ts.type == BT_COMPLEX || op0->ts.type == BT_COMPLEX) {         
      temp.ts.type = BT_LOGICAL;        
      temp.ts.kind = g95_default_logical_kind();          
      goto runtime;        
    }  
  
    /* Fall through */

  case INTRINSIC_EQ:      case INTRINSIC_NE:     
    if (op->ts.type == BT_CHARACTER && op0->ts.type == BT_CHARACTER) {
      unary = 0;       
      temp.ts.type = BT_LOGICAL;
      temp.ts.kind = g95_default_logical_kind();       
      break;    
    }   
   
    /* Fall through */         
         
  case INTRINSIC_PLUS:    case INTRINSIC_MINUS:   case INTRINSIC_TIMES:
  case INTRINSIC_DIVIDE:  case INTRINSIC_POWER:   /* Numeric binary */      
    if (!g95_numeric_ts(&op->ts) || !g95_numeric_ts(&op0->ts))          
      goto runtime;      
      
    /* Insert any necessary type conversions to make the operands compatible */        
        
    temp.type = EXPR_OP;        
    g95_clear_ts(&temp.ts);   
    temp.operator = oper;          
          
    temp.op1 = op;  
    temp.op2 = op0;  
  
    g95_type_convert_binary(&temp);    
    
    if (oper == INTRINSIC_EQ || oper == INTRINSIC_NE ||    
	oper == INTRINSIC_GE || oper == INTRINSIC_GT ||  
	oper == INTRINSIC_LE || oper == INTRINSIC_LT) {      
      temp.ts.type = BT_LOGICAL;      
      temp.ts.kind = g95_default_logical_kind();         
    }        
        
    if (oper == INTRINSIC_DIVIDE && op0->type == EXPR_CONSTANT &&         
	((op0->ts.type == BT_INTEGER &&     
	  mpz_cmp_ui(op0->value.integer, 0) == 0) ||       
	 (op0->ts.type == BT_REAL &&        
	  mpf_cmp_ui(op0->value.real, 0) == 0) ||    
	 (op0->ts.type == BT_COMPLEX &&    
	  mpf_cmp_ui(op0->value.complex.r, 0) == 0 &&     
	  mpf_cmp_ui(op0->value.complex.i, 0) == 0)))       
      goto runtime;     
     
    unary = 0;        
    break;       
       
  case INTRINSIC_CONCAT:   /* Character binary */
    if (op->ts.type != BT_CHARACTER || op0->ts.type != BT_CHARACTER) 
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

  if (oper == INTRINSIC_POWER && op0->ts.type != BT_INTEGER)    
    goto runtime;        
        
  if (op->type != EXPR_CONSTANT &&        
      (op->type != EXPR_ARRAY || !g95_is_constant_expr(op) ||  
       !g95_expanded_ac(op)))   
    goto runtime;

  if (op0 != NULL && op0->type != EXPR_CONSTANT &&     
      (op0->type != EXPR_ARRAY || !g95_is_constant_expr(op0) ||
       !g95_expanded_ac(op0)))     
    goto runtime; 
 
  if (unary)   
    retval = reduce_unary(eval, op, &result);      
  else       
    retval = reduce_binary(eval, op, op0, &result);    
    
  if (retval != ARITH_OK) {     /* Something went wrong */ 
    g95_error("%s at %L", g95_arith_error(retval), &op->where); 
    return NULL;          
  }   
   
  g95_free_expr(op); 
  g95_free_expr(op0);     
  return result; 
 
  /* Create a run-time expression */

runtime:
  result = g95_get_expr();         
  result->ts = temp.ts;  
  
  result->type = EXPR_OP;   
  result->operator = oper;   
   
  result->op1 = op;         
  result->op2 = op0;   
   
  result->where = op->where;    
    
  return result;       
}       
       
       
      
      
static g95_expr *eval_intrinsic_f2(g95_intrinsic_op oper,     
				   arith (*eval)(g95_expr *, g95_expr **),        
				   g95_expr *op1, g95_expr *op) { 
g95_expr *r;     
     
  if (op == NULL) {   
    if (g95_zero_size_array(op1)) return eval_type_intrinsic0(oper, op1);        
  } else { 
    r = reduce_binary0(op1, op);
    if (r != NULL) return eval_type_intrinsic0(oper, r);    
  }         
         
  return eval_intrinsic(oper, eval, op1, op); 
}        
        
        
         
         
static g95_expr *eval_intrinsic_f3(g95_intrinsic_op o,      
				   arith (*eval)(g95_expr *, g95_expr *,  
						 g95_expr **),
				   g95_expr *op, g95_expr *op2) {    
g95_expr *result; 
 
  result = reduce_binary0(op, op2);      
  if (result != NULL) return eval_type_intrinsic0(o, result);     
     
  return eval_intrinsic(o, eval, op, op2); 
}         
         
         
    
    
g95_expr *g95_uplus(g95_expr *operand) {  
  return eval_intrinsic_f2(INTRINSIC_UPLUS, g95_arith_uplus, operand, NULL);       
}  
  
    
    
g95_expr *g95_le(g95_expr *op, g95_expr *op_2) {    
  return eval_intrinsic_f3(INTRINSIC_LE, g95_arith_le, op, op_2);   
}      
      
  
  
g95_expr *g95_not(g95_expr *op_1) {   
  return eval_intrinsic_f2(INTRINSIC_NOT, g95_arith_not, op_1, NULL);  
}  
  
          
          
g95_expr *g95_ne(g95_expr *op_1, g95_expr *op0) {
  return eval_intrinsic_f3(INTRINSIC_NE, g95_arith_ne, op_1, op0);      
} 
 
  
  
g95_expr *g95_eq(g95_expr *op0, g95_expr *op2) {     
  return eval_intrinsic_f3(INTRINSIC_EQ, g95_arith_eq, op0, op2);         
}    
    
   
   
g95_expr *g95_uminus(g95_expr *op1) {          
  return eval_intrinsic_f2(INTRINSIC_UMINUS, g95_arith_uminus, op1, NULL);          
}       
       
     
     
g95_expr *g95_multiply(g95_expr *op, g95_expr *op2) {        
  return eval_intrinsic_f3(INTRINSIC_TIMES, g95_arith_times, op, op2);        
}   
   
    
    
g95_expr *g95_divide(g95_expr *op0, g95_expr *op2) {      
  return eval_intrinsic_f3(INTRINSIC_DIVIDE, g95_arith_divide, op0, op2);       
}

 
 
g95_expr *g95_gt(g95_expr *op, g95_expr *op0) {        
  return eval_intrinsic_f3(INTRINSIC_GT, g95_arith_gt, op, op0);      
}

      
      
g95_expr *g95_lt(g95_expr *op, g95_expr *op2) {         
  return eval_intrinsic_f3(INTRINSIC_LT, g95_arith_lt, op, op2);        
}          
          
 
 
g95_expr *g95_concat(g95_expr *op1, g95_expr *op) { 
  return eval_intrinsic_f3(INTRINSIC_CONCAT, g95_arith_concat, op1, op);    
}   
   
      
      
g95_expr *g95_subtract(g95_expr *op, g95_expr *op_2) { 
  return eval_intrinsic_f3(INTRINSIC_MINUS, g95_arith_minus, op, op_2);          
}         
         
         
         
g95_expr *g95_neqv(g95_expr *op1, g95_expr *op) {          
  return eval_intrinsic_f3(INTRINSIC_NEQV, g95_arith_neqv, op1, op);       
}       
      
      
g95_expr *g95_and(g95_expr *op0, g95_expr *op2) {        
  return eval_intrinsic_f3(INTRINSIC_AND, g95_arith_and, op0, op2);         
}        
        
     
     
g95_expr *g95_eqv(g95_expr *op_1, g95_expr *op_2) {     
  return eval_intrinsic_f3(INTRINSIC_EQV, g95_arith_eqv, op_1, op_2);
}  
  
       
       
g95_expr *g95_power(g95_expr *op, g95_expr *op_2) {     
  return eval_intrinsic_f3(INTRINSIC_POWER, g95_arith_power, op, op_2);
}         
         
          
          
g95_expr *g95_ge(g95_expr *op0, g95_expr *op2) {    
  return eval_intrinsic_f3(INTRINSIC_GE, g95_arith_ge, op0, op2);  
}        
        
     
     
g95_expr *g95_or(g95_expr *op_1, g95_expr *op0) {  
  return eval_intrinsic_f3(INTRINSIC_OR, g95_arith_or, op_1, op0);     
}      
      
      
      
g95_expr *g95_add(g95_expr *op1, g95_expr *op0) {         
  return eval_intrinsic_f3(INTRINSIC_PLUS, g95_arith_plus, op1, op0);       
}   
   
