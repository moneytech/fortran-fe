/* Simplify intrinsic functions at compile-time
   Copyright (C) 2000, 2001, 2002 Free Software Foundation, Inc.
   Contributed by Andy Vaught & Katherine Holcomb

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
   
#include "g95.h"
#include "intrinsic.h"
   
#include <string.h>
    
    
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
          
static g95_expr *range_check(g95_expr *rslt, char *name0) {   
   
  if (g95_range_check(rslt) == ARITH_OK)       
    return rslt;

  g95_error("Result of %s overflows its kind at %L", name0, &rslt->where);          
  g95_free_expr(rslt);    
  return &g95_bad_expr;         
} 
 
 
 
 
g95_expr *g95_simplify_tanh(g95_expr *c) {     
g95_expr *rslt;        
mpf_t xp, xq;         
         
  if (c->type != EXPR_CONSTANT) return NULL;  
  
  rslt = g95_constant_result(c->ts.type, c->ts.kind, &c->where);         
         
  mpf_init(xp);       
  mpf_init(xq);   
   
  hypersine(&c->value.real, &xq);   
  hypercos(&c->value.real, &xp);      
      
  mpf_div(rslt->value.real, xq, xp);     
     
  mpf_clear(xp);  
  mpf_clear(xq);         
         
  return range_check(rslt, "TANH");    
}     
     
     
   
   
g95_expr *g95_simplify_ior(g95_expr *n, g95_expr *v) {      
g95_expr *result;        
        
  if (n->type != EXPR_CONSTANT || v->type != EXPR_CONSTANT) return NULL;         
         
  result = g95_constant_result(BT_INTEGER, n->ts.kind, &n->where);   
   
  mpz_ior(result->value.integer, n->value.integer, v->value.integer);          
  return result; 
}      
      
      
  
  
g95_expr *g95_simplify_scan(g95_expr *f, g95_expr *x, g95_expr *b) {    
g95_expr *rslt;        
int back;       
size_t indx, leng, lenc;

  if (f->type != EXPR_CONSTANT || x->type != EXPR_CONSTANT) return NULL;          
          
  if (b != NULL && b->value.logical != 0)   
    back = 1;
  else          
    back = 0;  
  
  rslt = g95_constant_result(BT_INTEGER, g95_default_integer_kind(),  
			       &f->where);         
         
  leng  = f->value.character.length;          
  lenc = x->value.character.length;   
   
  if (leng == 0 || lenc == 0) {     
    indx = 0;          
  } else {          
    indx = strcspn(f->value.character.string, x->value.character.string) + 1;  
    if (indx > leng) indx=0; 
    if (back != 0 && indx != 0) indx = leng - indx + 1;   
  }     
     
  mpz_set_ui(rslt->value.integer, indx);        
  return range_check(rslt, "SCAN");          
} 
 
 
 
 
g95_expr *g95_simplify_trim(g95_expr *m) {    
g95_expr *r;          
int c, n, len, lentrim;  
  
  if (m->type != EXPR_CONSTANT) return NULL;      
      
  len = m->value.character.length;     
     
  r = g95_constant_result(BT_CHARACTER, m->ts.kind, &m->where);         
         
  for (c=0, n=1; n<=len; ++n) {
    if (m->value.character.string[len-n] == ' ')         
      c++;  
    else        
      break; 
  }         
         
  lentrim = len-c; 
 
  r->value.character.length = lentrim;    
  r->value.character.string = g95_getmem(lentrim+1);  
  
  for(n=0; n<lentrim; n++)    
    r->value.character.string[n] = m->value.character.string[n];     
     
  r->value.character.string[lentrim] = '\0';   /* For debugger */

  return r;        
}     
     
     
  
  
g95_expr *g95_simplify_btest(g95_expr *d, g95_expr *bit) {     
int n;  
  
  if (d->type != EXPR_CONSTANT || bit->type != EXPR_CONSTANT) return NULL;      
      
  if (g95_extract_int(bit, &n) != NULL || n < 0)  
    return g95_logical_expr(0, &d->where);   
   
  return g95_logical_expr(mpz_tstbit(d->value.integer, n), &d->where);         
}         
         
         
          
          
g95_expr *g95_simplify_modulo(g95_expr *x, g95_expr *h) {      
g95_expr *r;        
mpf_t quot, iquot, term0;       
       
  if (x->type != EXPR_CONSTANT || h->type != EXPR_CONSTANT) return NULL;          
          
  r = g95_constant_result(x->ts.type, x->ts.kind, &x->where);    
    
  switch (x->ts.type) {    
  case BT_INTEGER:  
    if (mpz_cmp_ui(h->value.integer, 0) == 0) {          
      /* Result is processor-dependent, and this processor doesn't handle it */     
      g95_error("Second argument of MODULO at %L is zero", &x->where);        
      g95_free_expr(r);    
      return &g95_bad_expr;
    }   
    mpz_fdiv_r(r->value.integer, x->value.integer, h->value.integer);          
          
    break;       
       
  case BT_REAL:      
    if (mpf_cmp_ui(h->value.real, 0) == 0) {      
      /* Result is processor-dependent */      
      g95_error("Second argument of MODULO at %L is zero", &h->where);
      g95_free_expr(r);
      return &g95_bad_expr; 
    }      
      
    mpf_init(quot);      
    mpf_init(iquot);       
    mpf_init(term0);        
        
    mpf_div(quot, x->value.real, h->value.real);      
    mpf_floor(iquot, quot);
    mpf_mul(term0, iquot, h->value.real);          
          
    mpf_clear(quot);  
    mpf_clear(iquot);       
    mpf_clear(term0);   
   
    mpf_sub(r->value.real, x->value.real, term0);         
    break;          
          
  default:    
    g95_internal_error("g95_simplify_modulo(): Bad arguments");      
  } 
 
  return range_check(r, "MODULO");       
}      
      
      
          
          
g95_expr *g95_simplify_acos(g95_expr *l) {         
g95_expr *res;         
mpf_t negative, square, term0;   
   
  if (l->type != EXPR_CONSTANT) return NULL;    
    
  if (mpf_cmp_si(l->value.real, 1) > 0 || mpf_cmp_si(l->value.real, -1) < 0) {          
    g95_error("Argument of ACOS at %L must be between -1 and 1", &l->where);      
    return &g95_bad_expr;       
  }      
      
  res = g95_constant_result(l->ts.type, l->ts.kind, &l->where);          
          
  if (mpf_cmp_si(l->value.real, 1) == 0) {        
    mpf_set_ui(res->value.real, 0);      
    return range_check(res, "ACOS");          
  }  
  
  if (mpf_cmp_si(l->value.real, -1) == 0) {  
    mpf_set(res->value.real, pi);
    return range_check(res, "ACOS");    
  } 
 
  mpf_init(negative);        
  mpf_init(square);       
  mpf_init(term0); 
 
  mpf_pow_ui(square, l->value.real, 2);
  mpf_ui_sub(term0, 1, square);         
  mpf_sqrt(term0, term0);
  mpf_div(term0, l->value.real, term0); 
  mpf_neg(term0, term0);      
  arctangent(&term0, &negative);        
  mpf_add(res->value.real, half_pi, negative);  
  
  mpf_clear(negative);    
  mpf_clear(square);
  mpf_clear(term0); 
 
  return range_check(res, "ACOS");        
}          
          
          
         
         
/* get_kind()-- A helper function that gets an optional and possibly
 * missing kind parameter.  Returns the kind, -1 if something went
 * wrong. */        
        
static int get_kind(bt t, g95_expr *v, char *nm, int default_kind) {
int knd;   
   
  if (v == NULL) return default_kind;        
        
  if (v->type != EXPR_CONSTANT) {        
    g95_error("KIND parameter of %s at %L must be an initialization " 
	      "expression", nm, &v->where);   
   
    return -1;
  }    
    
  if (g95_extract_int(v, &knd) != NULL ||     
      g95_validate_kind(t, knd) < 0) {   
   
    g95_error("Invalid KIND parameter of %s at %L", nm, &v->where);    
    return -1;         
  } 
 
  return knd;      
}       
       
       
   
   
g95_expr *g95_simplify_exponent(g95_expr *m) { 
mpf_t i2, absv, ln2, lnx;    
g95_expr *result;         
         
  if (m->type != EXPR_CONSTANT) return NULL;    
    
  result=g95_constant_result(BT_INTEGER, g95_default_integer_kind(),         
			     &m->where);        
        
  if (mpf_cmp(m->value.real, mpf_zero) == 0) {      
    mpz_set_ui(result->value.integer, 0);        
    return result;   
  }   
   
  mpf_init_set_ui(i2, 2);         
  mpf_init(absv);  
  mpf_init(ln2);    
  mpf_init(lnx); 
 
  natural_logarithm(&i2, &ln2);  
  
  mpf_abs(absv, m->value.real);          
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
 
 
       
       
g95_expr *g95_simplify_ichar(g95_expr *k) { 
g95_expr *rslt;   
int ix;        
        
  if (k->type != EXPR_CONSTANT) return NULL;

  if (k->value.character.length != 1) {        
    g95_error("Argument of ICHAR at %L must be of length one", &k->where); 
    return &g95_bad_expr;     
  }          
          
  ix = (int) k->value.character.string[0];

  if (ix < CHAR_MIN || ix > CHAR_MAX) {    
    g95_error("Argument of ICHAR at %L out of range of this processor",     
	      &k->where); 
    return &g95_bad_expr;   
  }   
   
  rslt = g95_int_expr(ix);         
  rslt->where = k->where;       
  return range_check(rslt, "ICHAR");        
}  
  
  
   
   
g95_expr *g95_simplify_sinh(g95_expr *v) {      
g95_expr *result;          
          
  if (v->type != EXPR_CONSTANT) return NULL;      
      
  result = g95_constant_result(v->ts.type, v->ts.kind, &v->where); 
 
  hypersine(&v->value.real, &result->value.real);    
    
  return range_check(result, "SINH");   
}       
       
       
  
  
g95_expr *g95_simplify_dint(g95_expr *d) {    
g95_expr *rtrunc, *result;         
         
  if (d->type != EXPR_CONSTANT) return NULL;          
          
  rtrunc = g95_copy_expr(d);         
         
  mpf_trunc(rtrunc->value.real, d->value.real);

  result = g95_real2real(rtrunc, g95_default_double_kind());         
  g95_free_expr(rtrunc);     
     
  return range_check(result, "DINT");      
      
}


   
   
g95_expr *g95_simplify_iand(g95_expr *f, g95_expr *d) {      
g95_expr *res;       
       
  if (f->type != EXPR_CONSTANT || d->type != EXPR_CONSTANT) return NULL;

  res = g95_constant_result(BT_INTEGER, f->ts.kind, &f->where);         
         
  mpz_and(res->value.integer, f->value.integer, d->value.integer);
  return res;
}       
       
       
      
      
g95_expr *g95_simplify_ceiling(g95_expr *p, g95_expr *f) {     
g95_expr *ceil, *rslt;
int knd;

  knd = get_kind(BT_REAL, f, "CEILING", g95_default_real_kind());       
  if (knd == -1) return &g95_bad_expr;       
       
  if (p->type != EXPR_CONSTANT) return NULL;

  rslt = g95_constant_result(BT_INTEGER, knd, &p->where);          
          
  ceil = g95_copy_expr(p);          
          
  mpf_ceil(ceil->value.real, p->value.real);      
  mpz_set_f(rslt->value.integer, ceil->value.real);          
          
  g95_free_expr(ceil);          
          
  return range_check(rslt, "CEILING"); 
}      
      
      
         
         
g95_expr *g95_simplify_len(g95_expr *n) {        
g95_expr *res;

  if (n->type != EXPR_CONSTANT) return NULL;    
    
  res = g95_constant_result(BT_INTEGER, g95_default_integer_kind(),     
			       &n->where);         
         
  mpz_set_si(res->value.integer, n->value.character.length); 
  return range_check(res, "LEN");       
} 
 
 
          
          
g95_expr *g95_simplify_atan(g95_expr *x) {          
g95_expr *res;  
  
  if (x->type != EXPR_CONSTANT) return NULL;          
          
  res = g95_constant_result(x->ts.type, x->ts.kind, &x->where);          
          
  arctangent(&x->value.real, &res->value.real); 
 
  return range_check(res, "ATAN");

}      
      
      
        
        
g95_expr *g95_simplify_floor(g95_expr *v, g95_expr *s) {
g95_expr *res; 
mpf_t floor;      
int knd;  
  
  knd = get_kind(BT_REAL, s, "FLOOR", g95_default_real_kind()); 
  if (knd == -1) g95_internal_error("g95_simplify_floor(): Bad kind");

  if (v->type != EXPR_CONSTANT) return NULL;      
      
  res = g95_constant_result(BT_INTEGER, knd, &v->where);       
       
  mpf_init(floor);       
  mpf_floor(floor, v->value.real);       
  mpz_set_f(res->value.integer, floor);         
  mpf_clear(floor);     
     
  return range_check(res, "FLOOR");   
}   
   
   
 
 
g95_expr *g95_simplify_repeat(g95_expr *u, g95_expr *n) { 
g95_expr *res;         
int s, r, leng, ncopies, nlen;          
          
  if (u->type != EXPR_CONSTANT || n->type != EXPR_CONSTANT) return NULL;        
        
  if (n !=NULL && (g95_extract_int(n, &ncopies) != NULL || ncopies < 0)) {      
    g95_error("Invalid second argument of REPEAT at %L", &n->where);      
    return &g95_bad_expr;  
  }          
          
  leng    = u->value.character.length;          
  nlen   = ncopies*leng;  
  
  res = g95_constant_result(BT_CHARACTER, u->ts.kind, &u->where);  
  
  if (ncopies == 0) {          
    res->value.character.string=g95_getmem(1);
    res->value.character.length=0;  
    res->value.character.string='\0';       
    return res;  
  }        
        
  res->value.character.length=nlen;      
  res->value.character.string=g95_getmem(nlen+1); 
 
  for(s=0; s<ncopies; s++)    
    for(r=0; r<leng; r++)        
      res->value.character.string[r+s*leng] = u->value.character.string[r];    
    
  res->value.character.string[nlen] = '\0';  /* For debugger */       
  return res;    
}      
      
      
       
       
g95_expr *g95_simplify_cos(g95_expr *e) {
g95_expr *rslt;
mpf_t xp, xq;   
   
  if (e->type != EXPR_CONSTANT) return NULL;   
   
  rslt = g95_constant_result(e->ts.type, e->ts.kind, &e->where);    
    
  switch (e->ts.type) {  
  case BT_REAL: 
    cosine(&e->value.real, &rslt->value.real); 
    break;        
  case BT_COMPLEX:         
    mpf_init(xp);    
    mpf_init(xq);      
      
    cosine(&e->value.complex.r, &xp); 
    hypercos(&e->value.complex.i, &xq);
    mpf_mul(rslt->value.complex.r, xp, xq);  
  
    sine(&e->value.complex.r, &xp);         
    hypersine(&e->value.complex.i, &xq);        
    mpf_mul(xp, xp, xq);   
    mpf_neg(rslt->value.complex.i, xp);  
  
    mpf_clear(xp);
    mpf_clear(xq); 
    break;       
  default:    
    g95_internal_error("in g95_simplify_cos(): Bad type");     
  } 
 
  return range_check(rslt, "COS");         
         
}          
          
          
      
      
g95_expr *g95_simplify_dprod(g95_expr *c, g95_expr *l) {   
g95_expr *mult1, *mult2, *r;   
   
  if (c->type != EXPR_CONSTANT || l->type != EXPR_CONSTANT) return NULL;    
    
  r = g95_constant_result(BT_REAL, g95_default_double_kind(), &c->where);        
        
  mult1 = g95_real2real(c, g95_default_double_kind());       
  mult2 = g95_real2real(l, g95_default_double_kind());          
          
  mpf_mul(r->value.real, mult1->value.real, mult2->value.real); 
 
  g95_free_expr(mult1);          
  g95_free_expr(mult2);

  return range_check(r, "DPROD");   
}     
     
     
        
        
g95_expr *g95_simplify_rrspacing(g95_expr *l) {   
g95_expr *r;      
mpf_t e, absv, ln2, lnx, frac, pow2;     
unsigned long exp2;         
int k, z;   
   
  if (l->type != EXPR_CONSTANT) return NULL; 
 
  k = g95_validate_kind(l->ts.type, l->ts.kind);        
  if (k < 0) g95_internal_error("g95_simplify_rrspacing(): bad kind");        
        
  r=g95_constant_result(BT_REAL, l->ts.kind, &l->where);     
     
  z = g95_real_kinds[k].digits;     
     
  if (mpf_cmp(l->value.real, mpf_zero) == 0) {    
    mpf_ui_div(r->value.real, 1, g95_real_kinds[k].tiny); 
    return r;    
  }          
          
  mpf_init_set_ui(e, 2);      
  mpf_init(ln2);
  mpf_init(absv);       
  mpf_init(lnx);   
  mpf_init(frac);   
  mpf_init(pow2);   
   
  natural_logarithm(&e, &ln2);    
    
  mpf_abs(absv, l->value.real);      
  natural_logarithm(&absv, &lnx);        
        
  mpf_div(lnx, lnx, ln2);    
  mpf_trunc(lnx, lnx);        
  mpf_add_ui(lnx, lnx, 1);   
   
  exp2 = (unsigned long) mpf_get_d(lnx);       
  mpf_pow_ui(pow2, e, exp2);       
  mpf_div(frac, absv, pow2);      
      
  exp2 = (unsigned long) z;        
  mpf_mul_2exp(r->value.real, frac, exp2);   
   
  mpf_clear(e);          
  mpf_clear(ln2);       
  mpf_clear(absv);        
  mpf_clear(lnx);       
  mpf_clear(frac);    
  mpf_clear(pow2);         
         
  return range_check(r, "RRSPACING"); 
}      
      
      
        
        
g95_expr *g95_simplify_llt(g95_expr *s, g95_expr *v) {          
          
  if (s->type != EXPR_CONSTANT || v->type != EXPR_CONSTANT) return NULL;  
  
  return g95_logical_expr(g95_compare_string(s, v, xascii_table) < 0,  
			  &s->where);      
}         
         
         
        
        
g95_expr *g95_simplify_mvbits(g95_expr *m, g95_expr *fp2, g95_expr *l,  
			      g95_expr *to, g95_expr *tp) {         
  return NULL;        
}     
     
     


g95_expr *g95_simplify_idint(g95_expr *d) { 
g95_expr *rtrunc, *result;   
   
  if (d->type != EXPR_CONSTANT) return NULL;       
       
  result = g95_constant_result(BT_INTEGER, g95_default_integer_kind(),     
			       &d->where);

  rtrunc = g95_copy_expr(d);

  mpf_trunc(rtrunc->value.real, d->value.real);      
  mpz_set_f(result->value.integer, rtrunc->value.real); 
 
  g95_free_expr(rtrunc);     
  return range_check(result, "IDINT");      
}          
          
          
      
      
/* twos_complement()-- Given an unsigned multiple precision integer
 * and a bit size, convert it to the equivalent twos complement signed
 * integer. */

static void twos_complement(mpz_t u, int bit_size) {   
int a;   
   
  if (!mpz_tstbit(u, bit_size-1)) return;   
   
  for(a=0; a<bit_size; a++)
    if (mpz_tstbit(u, a)) 
      mpz_clrbit(u, a);
    else   
      mpz_setbit(u, a);  
  
  mpz_add_ui(u, u, 1);    
  mpz_neg(u, u);       
} 
 
 


g95_expr *g95_simplify_maxexponent(g95_expr *r) {  
g95_expr *res;   
int u;    
    
  u = g95_validate_kind(BT_REAL, r->ts.kind);
  if (u < 0) g95_internal_error("g95_simplify_maxexponent(): Bad kind");      
      
  res = g95_int_expr(g95_real_kinds[u].max_exponent); 
  res->where = r->where;   
   
  return res;        
}          
          
          
    
    
g95_expr *g95_simplify_ibset(g95_expr *m, g95_expr *t) {    
g95_expr *r;  
int s, position;       
       
  if (m->type != EXPR_CONSTANT || t->type != EXPR_CONSTANT) return NULL;   
   
  if (g95_extract_int(t, &position) != NULL || position < 0) {
    g95_error("Invalid second argument of IBSET at %L", &t->where);   
    return &g95_bad_expr; 
  }        
        
  s = g95_validate_kind(m->ts.type, m->ts.kind);  
  if (s == -1) g95_internal_error("In g95_simplify_ibset: bad kind"); 
 
  if (position > g95_integer_kinds[s].bit_size) {   
    g95_error("Second argument of IBSET exceeds bit size at %L", &t->where);     
    return &g95_bad_expr;        
  }

  r = g95_copy_expr(m);      
      
  mpz_setbit(r->value.integer, position);    
  return range_check(r, "IBSET");         
}


          
          
g95_expr *g95_simplify_selected_int_kind(g95_expr *y) {      
int i, k, range;        
g95_expr *r;       
       
  if (y->type != EXPR_CONSTANT || g95_extract_int(y, &range) != NULL)   
    return NULL;    
    
  k = INT_MAX;     
     
  for(i=0; g95_integer_kinds[i].kind!=0; i++)    
    if (g95_integer_kinds[i].range >= range && 
	g95_integer_kinds[i].kind < k) k = g95_integer_kinds[i].kind;          
          
  if (k == INT_MAX) k = -1;          
          
  r = g95_int_expr(k);          
  r->where = y->where;  
  
  return r;     
} 
 
 
    
    
g95_expr *g95_simplify_aint(g95_expr *y, g95_expr *g) {
g95_expr *rtrunc, *result;          
int kind;      
      
  kind = get_kind(BT_REAL, g, "AINT", y->ts.kind);     
  if (kind == -1) return &g95_bad_expr;       
       
  if (y->type != EXPR_CONSTANT) return NULL;

  rtrunc = g95_copy_expr(y);    
    
  mpf_trunc(rtrunc->value.real, y->value.real);         
         
  result = g95_real2real(rtrunc, kind);
  g95_free_expr(rtrunc);    
    
  return range_check(result, "AINT");     
}     
     
     
    
    
g95_expr *g95_simplify_lge(g95_expr *c, g95_expr *z) {

  if (c->type != EXPR_CONSTANT || z->type != EXPR_CONSTANT) return NULL;    
    
  return g95_logical_expr(g95_compare_string(c, z, xascii_table) >= 0,  
			  &c->where);  
}        
        
        
    
    
g95_expr *g95_simplify_range(g95_expr *g) {      
g95_expr *result;     
int x; 
long v;      
      
  x = g95_validate_kind(g->ts.type, g->ts.kind);          
  if (x < 0) goto bad_type;        
        
  switch(g->ts.type) {    
  case BT_INTEGER:         
    v = g95_integer_kinds[x].range;  
    break;         
         
  case BT_REAL:      
  case BT_COMPLEX:     
    v = g95_real_kinds[x].range;   
    break; 
 
  bad_type:
  default:  
    g95_internal_error("g95_simplify_range(): Bad kind");         
  } 
 
  result = g95_int_expr(v); 
  result->where = g->where;   
   
  return result;   
}        
        
        
  
  
g95_expr *g95_simplify_achar(g95_expr *e) {         
g95_expr *r;         
int i;        
        
  if (e->type != EXPR_CONSTANT) return NULL;      
      
/* We cannot assume that the native character set is ASCII in this function */      
      
  if (g95_extract_int(e, &i) != NULL  || i < 0 || i > 127) {         
      g95_error("Extended ASCII not implemented: argument of ACHAR at %L "      
		"must be between 0 and 127", &e->where);         
      return &g95_bad_expr;    
  }         
         
  r = g95_constant_result(BT_CHARACTER, g95_default_character_kind(), 
			       &e->where);  
  
  r->value.character.string = g95_getmem(2);   
   
  r->value.character.length = 1;         
  r->value.character.string[0] = ascii_table[i];   
  r->value.character.string[1] = '\0';   /* For debugger */         
  return r;   
}      
      
      
 
 
g95_expr *g95_simplify_conjg(g95_expr *x) {      
g95_expr *res;     
     
  if (x->type != EXPR_CONSTANT) return NULL;         
         
  res = g95_copy_expr(x);         
  mpf_neg(res->value.complex.i, res->value.complex.i);       
       
  return range_check(res, "CONJG");        
}       
       
       
         
         
g95_expr *g95_simplify_tiny(g95_expr *x) {     
g95_expr *res;      
int d;     
     
  d = g95_validate_kind(BT_REAL, x->ts.kind);
  if (d < 0) g95_internal_error("g95_simplify_error(): bad kind");          
          
  res = g95_constant_result(BT_REAL, x->ts.kind, &x->where);  
  mpf_set(res->value.real, g95_real_kinds[d].tiny);      
      
  return res;       
}   
   
   
   
   
g95_expr *g95_simplify_ishft(g95_expr *n, g95_expr *d) {     
int o, j, shift, bit_size;
g95_expr *res;   
mpz_t p;      
      
  if (n->type != EXPR_CONSTANT || d->type != EXPR_CONSTANT) return NULL;         
         
  if (g95_extract_int(d, &shift) != NULL) {      
    g95_error("Invalid second argument of ISHFT at %L", &d->where);      
    return &g95_bad_expr;        
  }  
  
  j = g95_validate_kind(BT_INTEGER, n->ts.kind);  
  if (j == -1) g95_internal_error("In g95_simplify_ishft: bad kind");    
    
  bit_size = g95_integer_kinds[j].bit_size;        
        
  mpz_init_set_ui(p, 0);     
     
  for(o=0; o<bit_size; o++) {   
    if (o-shift < 0 || o-shift >= bit_size) continue;    
    
    if (mpz_tstbit(n->value.integer, o-shift)) mpz_setbit(p, o);      
  }

  twos_complement(p, bit_size);   
   
  res = g95_constant_result(n->ts.type, n->ts.kind, &n->where);
  mpz_set(res->value.integer, p);      
      
  return res;
}     
     
     
    
    
g95_expr *g95_simplify_float(g95_expr *g) {         
g95_expr *rslt;       
       
  if (g->type != EXPR_CONSTANT) return NULL;     
     
  rslt = g95_int2real(g, g95_default_real_kind());   
  return range_check(rslt, "FLOAT");    
}     
     
     


/* simplify_sngl()-- The argument is always a double precision real
 * that is converted to single precision.  TODO: Rounding! */      
      
g95_expr *g95_simplify_sngl(g95_expr *f) {     
g95_expr *res;          
          
  if (f->type != EXPR_CONSTANT) return NULL;        
        
  res=g95_real2real(f, g95_default_real_kind());          
  return range_check(res, "SNGL");      
}         
         
         
       
       
g95_expr *g95_simplify_char(g95_expr *x, g95_expr *o) {   
g95_expr *res;       
int t, k0;

  k0 = get_kind(BT_CHARACTER, o, "CHAR", g95_default_character_kind());     
  if (k0 == -1) return &g95_bad_expr;

  if (x->type != EXPR_CONSTANT) return NULL; 
 
  if (g95_extract_int(x, &t) != NULL || t < 0 || t > 255) {     
    g95_error("Bad character in CHAR function at %L", &x->where);      
    return &g95_bad_expr;          
  }     
     
  res = g95_constant_result(BT_CHARACTER, k0, &x->where);   
   
  res->value.character.length = 1;    
  res->value.character.string = g95_getmem(2);         
         
  res->value.character.string[0] = t;      
  res->value.character.string[1] = '\0';   /* For debugger */     
     
  return res;   
}       
       
       
       
       
g95_expr *g95_simplify_null(g95_expr *mold) {        
g95_expr *res;    
    
  res = g95_get_expr();         
  res->type = EXPR_NULL;      
      
  if (mold == NULL)    
    res->ts.type = BT_UNKNOWN;       
  else {  
    res->ts = mold->ts; 
    res->where = mold->where; 
  }   
   
  return res;     
}    
    
    
   
   
g95_expr *g95_simplify_dnint(g95_expr *b) {   
g95_expr *rtrunc, *result;        
int cmp;        
        
  if (b->type != EXPR_CONSTANT) return NULL;      
      
  result = g95_constant_result(BT_REAL, g95_default_double_kind(), &b->where);    
    
  rtrunc = g95_copy_expr(b);  
  
  cmp = mpf_cmp_ui(b->value.real, 0);   
   
  if (cmp > 0) {     
    mpf_add(rtrunc->value.real, b->value.real, mpf_half);         
    mpf_trunc(result->value.real, rtrunc->value.real);        
  } else if (cmp < 0) {          
    mpf_sub(rtrunc->value.real, b->value.real, mpf_half);         
    mpf_trunc(result->value.real, rtrunc->value.real);  
  } else       
    mpf_set_ui(result->value.real, 0);

  g95_free_expr(rtrunc);   
   
  return range_check(result, "DNINT");          
}          
          
          
          
          
g95_expr *g95_simplify_log10(g95_expr *w) {   
g95_expr *r;   
   
  if (w->type != EXPR_CONSTANT) return NULL;   
   
  if (mpf_cmp(w->value.real, mpf_zero) <= 0) {     
    g95_error("Argument of LOG10 at %L cannot be less than or equal to zero",      
              &w->where);          
    return &g95_bad_expr;         
  }          
          
  r = g95_constant_result(w->ts.type, w->ts.kind, &w->where); 
 
  common_logarithm(&w->value.real, &r->value.real);    
    
  return range_check(r, "LOG10"); 
}          
          
          
 
 
g95_expr *g95_simplify_scale(g95_expr *n, g95_expr *e) {     
int j, neg_flag, power, exp_range;  
mpf_t scale, radix;  
g95_expr *r; 
 
  if (n->type != EXPR_CONSTANT || e->type != EXPR_CONSTANT) return NULL;  
  
  r = g95_constant_result(BT_REAL, n->ts.kind, &n->where);      
      
  if (mpf_sgn(n->value.real) == 0) {  
    mpf_set_ui(r->value.real, 0);   
    return r;  
  } 
 
  j = g95_validate_kind(BT_REAL, n->ts.kind);          
  exp_range = g95_real_kinds[j].max_exponent - g95_real_kinds[j].min_exponent; 
 
  /* This check filters out values of i that would overflow an int */    
    
  if (mpz_cmp_si(e->value.integer, exp_range+2) > 0 ||       
      mpz_cmp_si(e->value.integer, -exp_range-2) < 0) {  
    g95_error("Result of SCALE overflows its kind at %L", &r->where);      
    return &g95_bad_expr;     
  }       
       
  /* Compute scale = radix ** power */          
          
  power = mpz_get_si(e->value.integer);         
         
  if (power >= 0)         
    neg_flag = 0;   
  else {       
    neg_flag = 1;    
    power = -power; 
  }          
          
  mpf_init_set_ui(radix, g95_real_kinds[j].radix);        
  mpf_init(scale);     
  mpf_pow_ui(scale, radix, power);

  if (neg_flag)    
    mpf_div(r->value.real, n->value.real, scale);     
  else       
    mpf_mul(r->value.real, n->value.real, scale);

  mpf_clear(scale); 
  mpf_clear(radix);   
   
  return range_check(r, "SCALE");      
}       
       
       
       
       
g95_expr *g95_simplify_sign(g95_expr *p, g95_expr *f) {   
g95_expr *res;   
   
  if (p->type != EXPR_CONSTANT || f->type != EXPR_CONSTANT) return NULL;    
    
  res = g95_constant_result(p->ts.type, p->ts.kind, &p->where);   
   
  switch(p->ts.type) {    
  case BT_INTEGER:      
    mpz_abs(res->value.integer, p->value.integer);      
    if (mpz_sgn(f->value.integer) < 0) 
      mpz_neg(res->value.integer, res->value.integer);   
   
    break; 
 
  case BT_REAL:  
    /* TODO: Handle -0.0 and +0.0 correctly on machines that support it */        
    mpf_abs(res->value.real, p->value.real); 
    if (mpf_sgn(f->value.integer) < 0)     
      mpf_neg(res->value.real, res->value.real);

    break;        
        
  default:       
    g95_internal_error("Bad type in g95_simplify_sign");          
    g95_free_expr(res);        
    return &g95_bad_expr;   
  }      
      
  return res;          
}        
        
        
 
 
g95_expr *g95_simplify_sqrt(g95_expr *n) {         
g95_expr *result;          
mpf_t ac, ad, j, f, z;       
       
  if (n->type != EXPR_CONSTANT) return NULL;

  result = g95_constant_result(n->ts.type, n->ts.kind, &n->where);      
      
  switch (n->ts.type) {
  case BT_REAL:      
    if (mpf_cmp_si(n->value.real, 0) < 0) return NULL;    
    mpf_sqrt(result->value.real, n->value.real);   
   
    break;       
       
  case BT_COMPLEX:       
    /*Formula taken from Numerical Recipes to avoid over- and underflow*/ 
 
    mpf_init(ac);          
    mpf_init(ad);
    mpf_init(j);       
    mpf_init(f);      
    mpf_init(z);        
        
    if (mpf_cmp_ui(n->value.complex.r, 0) == 0 &&       
	mpf_cmp_ui(n->value.complex.i, 0) == 0) {  
  
      mpf_set_ui(result->value.complex.r, 0);      
      mpf_set_ui(result->value.complex.i, 0);
      break;     
    }    
    
    mpf_abs(ac, n->value.complex.r);     
    mpf_abs(ad, n->value.complex.i);      
      
    if (mpf_cmp(ac, ad) >= 0) { 
      mpf_div(f, n->value.complex.i, n->value.complex.r); 
      mpf_mul(f, f, f);        
      mpf_add_ui(f, f, 1);  
      mpf_sqrt(f, f);   
      mpf_add_ui(f, f, 1);  
      mpf_div_ui(f, f, 2);
      mpf_sqrt(f, f);   
      mpf_sqrt(j, ac);  
      mpf_mul(z, j, f);      
    } else {      
      mpf_div(j, n->value.complex.r, n->value.complex.i);
      mpf_mul(f, j, j);
      mpf_add_ui(f, f, 1);   
      mpf_sqrt(f, f);  
      mpf_abs(j, j);     
      mpf_add(f, f, j);    
      mpf_div_ui(f, f, 2);
      mpf_sqrt(f, f); 
      mpf_sqrt(j, ad);     
      mpf_mul(z, j, f);          
    } 
 
    if (mpf_cmp_ui(z, 0) !=0 && mpf_cmp_ui(n->value.complex.r, 0) >=0) { 
      mpf_mul_ui(f, z, 2);   
      mpf_div(result->value.complex.i, n->value.complex.i, f);      
      mpf_set(result->value.complex.r, z);          
    } else if (mpf_cmp_ui(z, 0) !=0 && mpf_cmp_ui(n->value.complex.r, 0) < 0 &&         
	       mpf_cmp_ui(n->value.complex.i, 0) >= 0) { 
      mpf_mul_ui(f, z, 2);       
      mpf_div(result->value.complex.r, n->value.complex.i, f);   
      mpf_set(result->value.complex.i, z); 
    } else if (mpf_cmp_ui(z, 0) !=0 && mpf_cmp_ui(n->value.complex.r, 0) < 0 &&    
	       mpf_cmp_ui(n->value.complex.i, 0) < 0) {    
      mpf_mul_ui(f, z, 2);    
      mpf_div(result->value.complex.r, ad, f);
      mpf_neg(z, z);    
      mpf_set(result->value.complex.i, z);       
    } else {      
      g95_internal_error("invalid complex argument of SQRT at %L",        
			 &n->where);   
      mpf_clear(j);  mpf_clear(f); mpf_clear(ac);       
      mpf_clear(ad); mpf_clear(z); 
      g95_free_expr(result);       
      return &g95_bad_expr;     
    }        
        
    mpf_clear(j); 
    mpf_clear(f);   
    mpf_clear(ac);       
    mpf_clear(ad);   
    mpf_clear(z);       
       
    break;       
       
  default:    
    g95_internal_error("invalid argument of SQRT at %L", &n->where);     
    g95_free_expr(result);    
    return &g95_bad_expr;   
  }     
     
  return range_check(result, "SQRT");       
}  
  
  
  
  
/* simplify_cmplx()-- Common subroutine for simplifying CMPLX and DCMPLX */         
         
static g95_expr *simplify_cmplx(g95_expr *a, g95_expr *f, int knd,      
				char *nm) {      
g95_expr *rslt;          
          
  rslt = g95_constant_result(BT_COMPLEX, knd, &a->where);         
         
  mpf_set_ui(rslt->value.complex.i, 0);     
     
  switch(a->ts.type) {          
  case BT_INTEGER:
    mpf_set_z(rslt->value.complex.r, a->value.integer);   
    break;         
         
  case BT_REAL: 
    mpf_set(rslt->value.complex.r, a->value.real);     
    break;       
       
  case BT_COMPLEX:       
    mpf_set(rslt->value.complex.r, a->value.complex.r); 
    mpf_set(rslt->value.complex.i, a->value.complex.i);       
    break;   
   
  default:         
    g95_internal_error("g95_simplify_dcmplx(): Bad type (x)");       
  } 
 
  if (f != NULL) {   
    switch(f->ts.type) {
    case BT_INTEGER:     
      mpf_set_z(rslt->value.complex.i, f->value.integer);    
      break;        
        
    case BT_REAL:     
      mpf_set(rslt->value.complex.i, f->value.real);         
      break;          
          
    default: 
      g95_internal_error("g95_simplify_dcmplx(): Bad type (y)");  
    }      
  }      
      
  return range_check(rslt, nm);         
}      
      
      
        
        
g95_expr *g95_simplify_ibits(g95_expr *x, g95_expr *v, g95_expr *f) {
g95_expr *result;  
int p, leng;      
int t, c, bitsize;       
int *bits; 
 
  if (x->type != EXPR_CONSTANT || v->type != EXPR_CONSTANT ||      
      f->type != EXPR_CONSTANT) return NULL;  
  
  if (g95_extract_int(v, &p) != NULL || p < 0) {  
    g95_error("Invalid second argument of IBITS at %L", &v->where);  
    return &g95_bad_expr;    
  }   
   
  if (g95_extract_int(f, &leng) != NULL || leng < 0) {     
    g95_error("Invalid third argument of IBITS at %L", &f->where);      
    return &g95_bad_expr;  
  }          
          
  c = g95_validate_kind(BT_INTEGER, x->ts.kind);          
  if (c == -1) g95_internal_error("In g95_simplify_ibits: bad kind");   
   
  bitsize = g95_integer_kinds[c].bit_size;   
   
  if (p+leng > bitsize) {      
    g95_error("Sum of second and third arguments of IBITS exceeds bit size "         
	      "at %L", &v->where);    
    return &g95_bad_expr;         
  }   
   
  result = g95_constant_result(x->ts.type, x->ts.kind, &x->where);    
    
  bits = g95_getmem(bitsize*sizeof(int)); 
 
  for(t=0; t<bitsize; t++)       
    bits[t] = 0;         
         
  for(t=0; t<leng; t++)          
    bits[t] = mpz_tstbit(x->value.integer, t+p);        
        
  for(t=0; t<bitsize; t++) {
    if (bits[t] == 0) {          
      mpz_clrbit(result->value.integer, t);          
    } else if (bits[t] == 1) {       
      mpz_setbit(result->value.integer, t);         
    } else {
      g95_internal_error("IBITS: Bad bit");  
    }          
  }     
     
  g95_free(bits);     
     
  return range_check(result, "IBITS");
}       
       
       


static g95_expr *simplify_nint(char *name0, g95_expr *d, g95_expr *a) { 
g95_expr *rtrunc, *itrunc, *result;   
int knd, cmp;

  knd = get_kind(BT_INTEGER, a, name0, g95_default_integer_kind());          
  if (knd == -1) return &g95_bad_expr;

  if (d->type != EXPR_CONSTANT) return NULL;      
      
  result = g95_constant_result(BT_INTEGER, knd, &d->where);          
          
  rtrunc = g95_copy_expr(d);     
  itrunc = g95_copy_expr(d); 
 
  cmp = mpf_cmp_ui(d->value.real, 0);  
  
  if (cmp > 0) {
    mpf_add(rtrunc->value.real, d->value.real, mpf_half); 
    mpf_trunc(itrunc->value.real, rtrunc->value.real);    
  } else if (cmp < 0) {   
    mpf_sub(rtrunc->value.real, d->value.real, mpf_half);          
    mpf_trunc(itrunc->value.real, rtrunc->value.real);        
  } else
    mpf_set_ui(itrunc->value.real, 0);     
     
  mpz_set_f(result->value.integer, itrunc->value.real);   
   
  g95_free_expr(itrunc);     
  g95_free_expr(rtrunc);          
          
  return range_check(result, name0);      
}    
    
    
  
  
g95_expr *g95_simplify_bit_size(g95_expr *p) { 
g95_expr *res;         
int s; 
 
  s = g95_validate_kind(p->ts.type, p->ts.kind);        
  if (s < 0) g95_internal_error("In g95_simplify_bit_size(): bad kind");

  res = g95_constant_result(BT_INTEGER, p->ts.kind, &p->where);    
  mpz_set_ui(res->value.integer, g95_integer_kinds[s].bit_size);   
   
  return res;   
}    
    
    
      
      
g95_expr *g95_simplify_adjustl(g95_expr *r) {   
g95_expr *rslt;    
int cnt, x, len;         
char ch;       
       
  if (r->type != EXPR_CONSTANT) return NULL;     
     
  len = r->value.character.length;    
    
  rslt = g95_constant_result(BT_CHARACTER, r->ts.kind, &r->where);  
  
  rslt->value.character.length = len;        
  rslt->value.character.string = g95_getmem(len+1);   
   
  for (cnt=0, x=0; x<len; ++x) {
    ch = r->value.character.string[x];       
    if (ch != ' ') break;       
    ++cnt;   
  }        
        
  for (x=0; x<len-cnt; ++x) {          
    rslt->value.character.string[x] = r->value.character.string[cnt+x];     
  }  
  
  for (x=len-cnt; x<len; ++x) {        
    rslt->value.character.string[x] = ' ';      
  }       
       
  rslt->value.character.string[len] = '\0';   /* For debugger */

  return rslt;         
} 
 
 
      
      
g95_expr *g95_simplify_dble(g95_expr *h) {      
g95_expr *rslt;      
      
  if (h->type != EXPR_CONSTANT) return NULL;  
  
  switch (h->ts.type) {  
  case BT_INTEGER:   
    rslt = g95_int2real(h, g95_default_double_kind()); 
    break;        
        
  case BT_REAL:
    rslt = g95_real2real(h, g95_default_double_kind());   
    break;  
  
  case BT_COMPLEX:  
    rslt = g95_complex2real(h, g95_default_double_kind());         
    break;         
         
  default:     
    g95_internal_error("g95_simplify_dble(): bad type at %L", &h->where);       
  }    
    
  return range_check(rslt, "DBLE");          
}      
      
      
          
          
g95_expr *g95_simplify_real(g95_expr *x, g95_expr *l) {   
g95_expr *result;  
int kind;   
   
  if (x->ts.type == BT_COMPLEX)  
      kind = get_kind(BT_REAL, l, "REAL", x->ts.kind);         
  else   
      kind = get_kind(BT_REAL, l, "REAL", g95_default_real_kind()); 
 
  if (kind == -1) return &g95_bad_expr;   
   
  if (x->type != EXPR_CONSTANT) return NULL;        
        
  switch (x->ts.type) {    
  case BT_INTEGER:       
    result = g95_int2real(x, kind);   
    break;    
    
  case BT_REAL:
    result = g95_real2real(x, kind);    
    break;   
   
  case BT_COMPLEX:      
    result = g95_complex2real(x, kind);      
    break;       
       
  default:     
    g95_internal_error("bad type in REAL");          
    return &g95_bad_expr; 
  }  
  
  return range_check(result, "REAL"); 
} 
 
   
   
g95_expr *g95_simplify_huge(g95_expr *q) {          
g95_expr *rslt;
int x;          
          
  x = g95_validate_kind(q->ts.type, q->ts.kind);     
  if (x == -1) goto bad_type;         
         
  rslt = g95_constant_result(q->ts.type, q->ts.kind, &q->where);     
     
  switch(q->ts.type) {      
  case BT_INTEGER: 
    mpz_set(rslt->value.integer, g95_integer_kinds[x].huge);        
    break;       
       
  case BT_REAL:         
    mpf_set(rslt->value.real, g95_real_kinds[x].huge);     
    break;   
   
  bad_type:     
  default:  
    g95_internal_error("g95_simplify_huge(): Bad type");         
  }          
          
  return rslt;
}        
        
        
    
    
g95_expr *g95_simplify_logical(g95_expr *i, g95_expr *w) {
g95_expr *res;       
int knd;     
     
  knd = get_kind(BT_LOGICAL, w, "LOGICAL", g95_default_logical_kind());
  if (knd < 0) return &g95_bad_expr;          
          
  if (i->type != EXPR_CONSTANT) return NULL;   
   
  res = g95_constant_result(BT_LOGICAL, knd, &i->where);  
  
  res->value.logical = i->value.logical;  
  
  return res;         
}         
         
         
  
  
g95_expr *g95_simplify_log(g95_expr *m) {   
g95_expr *r;     
     
  if (m->type != EXPR_CONSTANT || m->ts.type == BT_COMPLEX) return NULL;

  r = g95_constant_result(m->ts.type, m->ts.kind, &m->where);       
       
  switch(m->ts.type) { 
  case BT_REAL:       
    if (mpf_cmp(m->value.real, mpf_zero) <= 0) {  
      g95_error("Argument of LOG at %L cannot be less than or equal to zero",          
		&m->where);  
      g95_free_expr(r);
      return &g95_bad_expr;   
    }         
         
    natural_logarithm(&m->value.real, &r->value.real);          
    break;   
   
  default:         
    g95_internal_error("g95_simplify_log: bad type"); 
  }

  return range_check(r, "LOG");     
}         
         
         
       
       
g95_expr *g95_simplify_digits(g95_expr *w) {   
int e, digits;        
        
  e = g95_validate_kind(w->ts.type, w->ts.kind);       
  if (e < 0) goto bad;          
          
  switch(w->ts.type) {          
  case BT_INTEGER:   
    digits = g95_integer_kinds[e].digits; 
    break;        
        
  case BT_REAL:         
  case BT_COMPLEX:    
    digits = g95_real_kinds[e].digits;        
    break;        
        
  default:  
  bad:         
    g95_internal_error("g95_simplify_digits(): Bad type");
  }          
          
  return g95_int_expr(digits);         
}  
  
  
     
     
g95_expr *g95_simplify_ishftc(g95_expr *y, g95_expr *h, g95_expr *sz) {          
int z, n, shift, isize, w, bit_size;        
g95_expr *res;         
mpz_t j;         
         
  if (y->type != EXPR_CONSTANT || h->type != EXPR_CONSTANT) return NULL;

  if (g95_extract_int(h, &shift) != NULL) {         
    g95_error("Invalid second argument of ISHFTC at %L", &h->where);  
    return &g95_bad_expr;      
  }

  w = g95_validate_kind(y->ts.type, y->ts.kind);    
  if (w == -1) g95_internal_error("In g95_simplify_ishftc: bad kind");          
          
  bit_size = g95_integer_kinds[w].bit_size;    
    
  if (sz == NULL)  
    isize = g95_integer_kinds[w].bit_size;
  else {         
    if (g95_extract_int(sz, &isize) != NULL || isize <= 0) {       
      g95_error("Invalid third argument of ISHFTC at %L", &sz->where);      
      return &g95_bad_expr;         
    }          
  }  
  
  mpz_init_set_ui(j, 0);    
    
  for(z=0; z<bit_size; z++) {        
    if (z >= isize)          
      n = z;   
    else          
      n = (z - shift) % isize;

    if (mpz_tstbit(y->value.integer, n)) mpz_setbit(j, z); 
  }      
      
  twos_complement(j, bit_size);  
  
  res = g95_constant_result(y->ts.type, y->ts.kind, &y->where); 
  mpz_set(res->value.integer, j);  
  
  return res;         
} 
 
 
  
  
g95_expr *g95_simplify_atan2(g95_expr *s, g95_expr *o) {     
g95_expr *res;          
mpf_t u;      
      
  if (o->type != EXPR_CONSTANT || s->type != EXPR_CONSTANT) return NULL; 
 
  res = g95_constant_result(o->ts.type, o->ts.kind, &o->where);          
          
  mpf_init(u);          
          
  if (mpf_cmp_ui(s->value.real, 0) == 0) {        
    if (mpf_cmp_ui(o->value.real, 0) == 0) {          
      mpf_clear(u);
      g95_error("If first argument of ATAN2 %L is zero, the second argument "         
		"must not be zero", &o->where);
      g95_free_expr(res);     
      return &g95_bad_expr;     
    } 
    else if (mpf_cmp_si(o->value.real, 0) < 0) {     
      mpf_set(res->value.real, pi);          
      mpf_clear(u);        
      return res;     
    }     
    else if (mpf_cmp_si(o->value.real, -1)== 0) { 
      mpf_set_ui(res->value.real, 0);          
      mpf_clear(u);
      return range_check(res, "ATAN2"); 
    }         
  }     
     
  if (mpf_cmp_ui(o->value.real, 0) == 0) { 
    if (mpf_cmp_si(s->value.real, 0) < 0) {         
      mpf_neg(u, half_pi);    
      mpf_set(res->value.real, u);        
      mpf_clear(u);        
      return range_check(res, "ATAN2");    
    }
    else if (mpf_cmp_si(s->value.real, 0) > 0) {   
      mpf_set(res->value.real, half_pi);        
      mpf_clear(u);      
      return range_check(res, "ATAN2"); 
    }       
  } 
 
  mpf_div(u, s->value.real, o->value.real);        
  arctangent(&u, &res->value.real);          
          
  mpf_clear(u);       
       
  return range_check(res, "ATAN2");    
    
}     
     
     
     
     
g95_expr *g95_simplify_ifix(g95_expr *o) {
g95_expr *rtrunc, *rslt;         
         
  if (o->type != EXPR_CONSTANT) return NULL;  
  
  rslt = g95_constant_result(BT_INTEGER, g95_default_integer_kind(),          
			       &o->where);  
  
  rtrunc = g95_copy_expr(o);

  mpf_trunc(rtrunc->value.real, o->value.real);        
  mpz_set_f(rslt->value.integer, rtrunc->value.real);          
          
  g95_free_expr(rtrunc);
  return range_check(rslt, "IFIX");     
}     
     
     
  
  
g95_expr *g95_simplify_aimag(g95_expr *h) { 
g95_expr *res;     
     
  if (h->type != EXPR_CONSTANT) return NULL;         
         
  res = g95_constant_result(BT_REAL, h->ts.kind, &h->where);     
  mpf_set(res->value.real, h->value.complex.i);    
    
  return range_check(res, "AIMAG");
}         
         
         
          
          
/* g95_simplify_reshape()-- This one is a bear, but mainly has to do
 * with shuffling elements. */     
     
g95_expr *g95_simplify_reshape(g95_expr *s1, g95_expr *shape_exp,    
			       g95_expr *pad, g95_expr *order_exp) {       
       
int order[G95_MAX_DIMENSIONS], shape[G95_MAX_DIMENSIONS];        
int i, rnk, npad, d[G95_MAX_DIMENSIONS];    
g95_constructor *h, *end;      
mpz_t ind, size;       
unsigned long w;   
size_t nsource;       
g95_expr *b;       
       
/* Unpack the shape array */        
        
  if (s1->type != EXPR_ARRAY || !g95_is_constant_expr(s1)) return NULL;   
   
  if (shape_exp->type != EXPR_ARRAY || !g95_is_constant_expr(shape_exp))   
    return NULL;  
  
  if (pad != NULL && (pad->type != EXPR_ARRAY || !g95_is_constant_expr(pad))) 
    return NULL;       
       
  if (order_exp != NULL &&         
      (order_exp->type != EXPR_ARRAY || !g95_is_constant_expr(order_exp)))    
    return NULL;      
      
  mpz_init(ind);    
  rnk = 0; 
  h = end = NULL;         
         
  for(;;) {          
    b = g95_get_array_element(shape_exp, rnk);      
    if (b == NULL) break;        
        
    if (g95_extract_int(b, &shape[rnk]) != NULL) {      
      g95_error("Integer too large in shape specification at %L",    
		&b->where);      
      g95_free_expr(b);
      goto bad_reshape;     
    }        
        
    g95_free_expr(b);

    if (rnk >= G95_MAX_DIMENSIONS) { 
      g95_error("Too many dimensions in shape specification for RESHAPE "   
		"at %L", &b->where);       
       
      goto bad_reshape;    
    }     
     
    if (shape[rnk] < 0) { 
      g95_error("Shape specification at %L cannot be negative", &b->where);
      goto bad_reshape;
    }  
  
    rnk++;          
  }  
  
  if (rnk == 0) {    
    g95_error("Shape specification at %L cannot be the null array",
	      &shape_exp->where);         
    goto bad_reshape;          
  } 
 
  /* Now unpack the order array if present */  
  
  if (order_exp == NULL) {          
    for(i=0; i<rnk; i++)          
      order[i] = i;     
     
  } else { 
 
    for(i=0; i<rnk; i++)  
      d[i] = 0;          
          
    for(i=0; i<rnk; i++) {        
      b = g95_get_array_element(order_exp, i);   
      if (b == NULL) {       
	g95_error("ORDER parameter of RESHAPE at %L is not the same size "       
		  "as SHAPE parameter", &b->where);        
	goto bad_reshape;         
      }         
         
      if (g95_extract_int(b, &order[i]) != NULL) {  
	g95_error("Error in ORDER parameter of RESHAPE at %L", &b->where); 
	g95_free_expr(b);
	goto bad_reshape;      
      }   
   
      g95_free_expr(b); 
 
      if (order[i] < 1 || order[i] > rnk) { 
	g95_error("ORDER parameter of RESHAPE at %L is out of range",
		  &b->where);          
	goto bad_reshape;        
      }      
      
      order[i]--;          
          
      if (d[order[i]]) {   
	g95_error("Invalid permutation in ORDER parameter at %L", &b->where);    
	goto bad_reshape;  
      }      
      
      d[order[i]] = 1;        
    }     
  }        
        
  /* Count the elements in the source and padding arrays */  
  
  npad = 0;   
  if (pad != NULL) {
    g95_array_size(pad, &size);          
    npad = mpz_get_ui(size);   
    mpz_clear(size);
  }    
    
  g95_array_size(s1, &size);       
  nsource = mpz_get_ui(size);          
  mpz_clear(size);         
         
  /* If it weren't for that pesky permutation we could just loop
   * through the source and round out any shortage with pad elements.
   * But no, someone just had to have the compiler do something the
   * user should be doing. */

  for(i=0; i<rnk; i++)     
    d[i] = 0;

  for(;;) {     /* Figure out which element to extract */   
    mpz_set_ui(ind, 0);       
       
    for(i=rnk-1; i>=0; i--) {          
      mpz_add_ui(ind, ind, d[order[i]]);          
      if (i != 0) mpz_mul_ui(ind, ind, shape[order[i-1]]);          
    }      
      
    if (mpz_cmp_ui(ind, INT_MAX) > 0) {      
      g95_internal_error("Reshaped array too large at %L", &b->where);          
      goto bad_reshape;        
    }        
        
    w = mpz_get_ui(ind);  
  
    if (w < nsource)    
      b = g95_get_array_element(s1, w);          
    else {          
      w = w - nsource;      
      
      if (npad == 0) { 
	g95_error("PAD parameter required for short SOURCE parameter at %L",     
		  &s1->where);          
	goto bad_reshape;          
      }         
         
      w = w % npad;  
      b = g95_get_array_element(pad, w);        
    }

    if (h == NULL)   
      h = end = g95_get_constructor();      
    else {         
      end->next = g95_get_constructor();  
      end = end->next;         
    }  
  
    if (b == NULL) goto bad_reshape;    
    
    end->where = b->where;  
    end->expr = b; 
 
    /* Calculate the next element */         
         
    i = 0;     
  inc:  
    if (++d[i] < shape[i]) continue;
    d[i++] = 0;        
    if (i < rnk) goto inc;   
   
    break;        
  }       
       
  mpz_clear(ind);         
         
  b = g95_get_expr();         
  b->where = s1->where;     
  b->type = EXPR_ARRAY;
  b->value.constructor = h;      
  b->shape = g95_get_shape(rnk);          
          
  for(i=0; i<rnk; i++)          
    mpz_init_set_ui(b->shape[i], shape[order[i]]);       
       
  b->ts = h->expr->ts; 
  b->rank = rnk;    
    
  return b;     
     
bad_reshape:
  g95_free_constructor(h);        
  mpz_clear(ind);       
  return &g95_bad_expr; 
}         
         
         
       
       
/* invert_table()-- Given a collating table, create the inverse table */      
      
static void invert_table(int *table, int *xtable) {  
int l;      
      
  for(l=0; l<256; l++)       
    xtable[l] = 0;          
          
  for(l=0; l<256; l++)      
    xtable[table[l]] = l;         
}         
         
         
     
     
g95_expr *g95_simplify_cosh(g95_expr *a) {          
g95_expr *result;

  if (a->type != EXPR_CONSTANT) return NULL;         
         
  result = g95_constant_result(a->ts.type, a->ts.kind, &a->where);      
      
  hypercos(&a->value.real, &result->value.real); 
 
  return range_check(result, "COSH");  
}          
          
          
         
         
g95_expr *g95_simplify_adjustr(g95_expr *a) {       
g95_expr *res;        
int cont, o, len;     
char ch; 
 
  if (a->type != EXPR_CONSTANT) return NULL;       
       
  len = a->value.character.length;  
  
  res = g95_constant_result(BT_CHARACTER, a->ts.kind, &a->where);      
      
  res->value.character.length = len;          
  res->value.character.string = g95_getmem(len+1);         
         
  for (cont=0, o=len-1; o>=0; --o) { 
    ch = a->value.character.string[o];     
    if (ch != ' ') break;  
    ++cont;       
  }  
  
  for (o=0; o<cont; ++o) {         
    res->value.character.string[o] = ' ';    
  }   
   
  for (o=cont; o<len; ++o) {        
    res->value.character.string[o] = a->value.character.string[o-cont];
  }       
       
  res->value.character.string[len] = '\0';   /* For debugger */        
        
  return res;          
}


     
     
/* simplify_min_max()-- This function is special since MAX() can take
 * any number of arguments.  The simplified expression is a rewritten
 * version of the argument list containing at most one constant
 * element.  Other constant elements are deleted.  Because the
 * argument list has already been checked, this function always
 * succeeds.  sign is 1 for MAX(), -1 for MIN(). */         
         
static g95_expr *simplify_min_max(g95_expr *e, int sign) {       
g95_actual_arglist *arg, *last, *extremum;     
g95_expr *n;    
    
  last = NULL;     
  extremum = NULL;     
     
  arg = e->value.function.actual;        
        
  for(; arg; last=arg, arg=arg->next) {      
    if (arg->u.expr->type != EXPR_CONSTANT) continue;    
    
    if (extremum == NULL) {        
      extremum = arg; 
      continue;     
    }     
     
    switch(arg->u.expr->ts.type) {      
    case BT_INTEGER:     
      if (mpz_cmp(arg->u.expr->value.integer,          
		  extremum->u.expr->value.integer)*sign > 0)
	mpz_set(extremum->u.expr->value.integer, arg->u.expr->value.integer);        
        
      break; 
 
    case BT_REAL:          
      if (mpf_cmp(arg->u.expr->value.real,  
		  extremum->u.expr->value.real)*sign > 0)   
	mpf_set(extremum->u.expr->value.real, arg->u.expr->value.real);     
     
      break;          
          
    default:     
      g95_internal_error("g95_simplify_max(): Bad type in arglist");  
    }         
         
    /* Delete the extra constant argument */   
   
    if (last == NULL) 
      e->value.function.actual = arg->next;       
    else     
      last->next = arg->next;          
          
    arg->next = NULL;          
    g95_free_actual_arglist(arg); 
    arg = last;          
  }     
     
  /* If there is one value left, replace the function call with the
   * expression */  
  
  if (e->value.function.actual->next != NULL) return NULL;       
       
  n = g95_copy_expr(e->value.function.actual->u.expr);  
  if (g95_convert_type(n, &e->ts, 1) == SUCCESS) return n;    
    
  g95_free_expr(n);   
  return &g95_bad_expr;      
} 
 
 
    
    
g95_expr *g95_simplify_precision(g95_expr *z) {        
g95_expr *res; 
int i;      
      
  i = g95_validate_kind(z->ts.type, z->ts.kind);         
  if (i == -1) g95_internal_error("g95_simplify_precision(): Bad kind");         
         
  res = g95_int_expr(g95_real_kinds[i].precision);       
  res->where = z->where;  
  
  return res;      
}        
        
        
         
         
g95_expr *g95_simplify_nearest(g95_expr *o, g95_expr *g) {       
g95_expr *rslt;  
float rval;        
double val, eps;         
int c, f, t;    
    
/* TODO: This implementation is dopey and probably not quite right,
 * but it's a start.*/   
   
  if (o->type != EXPR_CONSTANT) return NULL;  
  
  t = g95_validate_kind(o->ts.type, o->ts.kind);
  if (t == -1) g95_internal_error("g95_simplify_precision(): Bad kind");          
          
  rslt = g95_constant_result(o->ts.type, o->ts.kind, &o->where);        
        
  val  = mpf_get_d(o->value.real);          
  c    = g95_real_kinds[t].digits;        
        
  eps = 1.;     
  for (f=1;f<c;++f) {       
    eps = eps/2.0;         
  }          
          
  if (mpf_cmp_ui(g->value.real, 0) > 0) { 
    if (t == g95_default_real_kind()) {     
      rval = (float) val;         
      rval = rval + eps;    
      mpf_set_d(rslt->value.real, rval);         
    }  
    else {  
      val = val + eps;      
      mpf_set_d(rslt->value.real, val);          
    }  
  }       
  else if (mpf_cmp_ui(g->value.real, 0) < 0) {        
    if (t == g95_default_real_kind()) {       
      rval = (float) val;         
      rval = rval - eps; 
      mpf_set_d(rslt->value.real, rval);   
    }       
    else {     
      val = val - eps;          
      mpf_set_d(rslt->value.real, val); 
    }  
  }  
  else {
    g95_error("Invalid second argument of NEAREST at %L", &g->where); 
    g95_free(rslt);   
    return &g95_bad_expr;      
  }       
       
  return range_check(rslt, "NEAREST");        
}


          
          
g95_expr *g95_simplify_radix(g95_expr *j) {        
g95_expr *result;     
int c;    
    
  c = g95_validate_kind(j->ts.type, j->ts.kind);       
  if (c < 0) goto bad;        
        
  switch(j->ts.type) {
  case BT_INTEGER:   
    c = g95_integer_kinds[c].radix;    
    break;    
    
  case BT_REAL:   
    c = g95_real_kinds[c].radix;       
    break;         
         
  default: bad:        
    g95_internal_error("g95_simplify_radix(): Bad type");       
  } 
 
  result = g95_int_expr(c);     
  result->where = j->where;      
      
  return result;       
}          
          
          
 
 
g95_expr *g95_simplify_abs(g95_expr *k) {   
g95_expr *rslt;      
mpf_t a, w;   
   
  if (k->type != EXPR_CONSTANT) return NULL;     
     
  switch(k->ts.type) { 
  case BT_INTEGER:  
    rslt = g95_constant_result(BT_INTEGER, k->ts.kind, &k->where); 
 
    mpz_abs(rslt->value.integer, k->value.integer);   
   
    rslt = range_check(rslt, "IABS");
    break;      
      
  case BT_REAL:   
    rslt = g95_constant_result(BT_REAL, k->ts.kind, &k->where);

    mpf_abs(rslt->value.real, k->value.real);   
   
    rslt = range_check(rslt, "ABS");
    break;        
        
  case BT_COMPLEX:     
    rslt = g95_constant_result(BT_REAL, k->ts.kind, &k->where);  
  
    mpf_init(a);     
    mpf_mul(a, k->value.complex.r, k->value.complex.r);   
   
    mpf_init(w);     
    mpf_mul(w, k->value.complex.i, k->value.complex.i);         
         
    mpf_add(a, a, w);         
    mpf_sqrt(rslt->value.real, a);  
  
    mpf_clear(a);       
    mpf_clear(w);  
  
    rslt = range_check(rslt, "CABS");     
    break; 
 
  default:         
    g95_internal_error("g95_simplify_abs(): Bad type");    
  }      
      
  return rslt;
}


      
      
g95_expr *g95_simplify_sin(g95_expr *u) {   
g95_expr *rslt;  
mpf_t xp, xq;        
        
  if (u->type != EXPR_CONSTANT) return NULL;      
      
  rslt = g95_constant_result(u->ts.type, u->ts.kind, &u->where);  
  
  switch (u->ts.type) {
  case BT_REAL: 
    sine(&u->value.real, &rslt->value.real);         
    break;          
          
  case BT_COMPLEX:        
    mpf_init(xp);  
    mpf_init(xq);  
  
    sine(&u->value.complex.r, &xp);     
    hypercos(&u->value.complex.i, &xq);          
    mpf_mul(rslt->value.complex.r, xp, xq);  
  
    cosine(&u->value.complex.r, &xp);     
    hypersine(&u->value.complex.i, &xq); 
    mpf_mul(rslt->value.complex.i, xp, xq);      
      
    mpf_clear(xp);  
    mpf_clear(xq);    
    break;       
       
  default:        
    g95_internal_error("in g95_simplify_sin(): Bad type");     
  }        
        
  return range_check(rslt, "SIN");   
}      
      
      
    
    
g95_expr *g95_simplify_mod(g95_expr *s, g95_expr *b) {        
g95_expr *res;
mpf_t quot, iquot, term0;     
     
  if (s->type != EXPR_CONSTANT || b->type != EXPR_CONSTANT) return NULL;  
  
  res = g95_constant_result(s->ts.type, s->ts.kind, &s->where);       
       
  switch (s->ts.type) {     
  case BT_INTEGER:       
    if (mpz_cmp_ui(b->value.integer, 0) == 0) {     
      /* Result is processor-dependent */      
      g95_error("Second argument MOD at %L is zero", &s->where);     
      g95_free_expr(res);          
      return &g95_bad_expr;   
    }       
    mpz_tdiv_r(res->value.integer, s->value.integer, b->value.integer);    
    break;

  case BT_REAL:   
    if (mpf_cmp_ui(b->value.real, 0) == 0) {          
      /* Result is processor-dependent */        
        
      g95_error("Second argument of MOD at %L is zero", &b->where);        
      g95_free_expr(res);  
      return &g95_bad_expr;
    }   
   
    mpf_init(quot);    
    mpf_init(iquot);
    mpf_init(term0);    
    
    mpf_div(quot, s->value.real, b->value.real);   
    mpf_trunc(iquot, quot);      
    mpf_mul(term0, iquot, b->value.real);     
    mpf_sub(res->value.real, s->value.real, term0);         
         
    mpf_clear(quot);        
    mpf_clear(iquot);    
    mpf_clear(term0);
    break;    
    
  default:       
    g95_internal_error("g95_simplify_mod(): Bad arguments");         
  }      
      
  return range_check(res, "MOD");        
}    
    
    
         
         
g95_expr *g95_simplify_iachar(g95_expr *y) {
g95_expr *r;      
int idx; 
 
  if (y->type != EXPR_CONSTANT) return NULL;    
    
  if (y->value.character.length != 1) {  
    g95_error("Argument of IACHAR at %L must be of length one", &y->where);      
    return &g95_bad_expr;      
  }       
       
  idx = xascii_table[(int) y->value.character.string[0] & 0xFF];          
          
  r = g95_int_expr(idx); 
  r->where = y->where;     
     
  return range_check(r, "IACHAR");   
} 
 
 
  
  
g95_expr *g95_simplify_epsilon(g95_expr *o) {      
g95_expr *r;
int s;          
          
  s = g95_validate_kind(o->ts.type, o->ts.kind);   
  if (s == -1) g95_internal_error("g95_simplify_epsilon(): Bad kind");      
      
  r = g95_constant_result(BT_REAL, o->ts.kind, &o->where);      
      
  mpf_set(r->value.real, g95_real_kinds[s].epsilon);       
       
  return range_check(r, "EPSILON");  
}   
   
   
       
       
g95_expr *g95_simplify_ibclr(g95_expr *c, g95_expr *n) {   
g95_expr *res;
int f, p;   
   
  if (c->type != EXPR_CONSTANT || n->type != EXPR_CONSTANT) return NULL;          
          
  if (g95_extract_int(n, &p) != NULL || p < 0) {         
    g95_error("Invalid second argument of IBCLR at %L", &n->where);       
    return &g95_bad_expr;         
  }

  f = g95_validate_kind(c->ts.type, c->ts.kind);          
  if (f == -1) g95_internal_error("In g95_simplify_ibclr: bad kind"); 
 
  if (p > g95_integer_kinds[f].bit_size) {
    g95_error("Second argument of IBCLR exceeds bit size at %L", &n->where);       
    return &g95_bad_expr;
  }  
  
  res = g95_copy_expr(c);       
  mpz_clrbit(res->value.integer, p);    
    
  return res;       
}      
      
      
        
        
g95_expr *g95_simplify_not(g95_expr *j) {  
g95_expr *result; 
int g;  
  
  if (j->type != EXPR_CONSTANT) return NULL;          
          
  result = g95_constant_result(j->ts.type, j->ts.kind, &j->where);      
      
  mpz_com(result->value.integer, j->value.integer);          
          
  /* Because of how GMP handles numbers, the result must be ANDed with
   * the max_int mask.  For radices <> 2, this will require change */  
  
  g = g95_validate_kind(BT_INTEGER, j->ts.kind);   
  mpz_and(result->value.integer, result->value.integer,        
	  g95_integer_kinds[g].max_int);         
         
  return result;    
}    
    
    
      
      
g95_expr *g95_simplify_lle(g95_expr *n, g95_expr *u) {  
  
  if (n->type != EXPR_CONSTANT || u->type != EXPR_CONSTANT) return NULL; 
 
  return g95_logical_expr(g95_compare_string(n, u, xascii_table) <= 0,
			  &n->where);     
}          
          
          


g95_expr *g95_simplify_lgt(g95_expr *y, g95_expr *e) {         
         
  if (y->type != EXPR_CONSTANT || e->type != EXPR_CONSTANT) return NULL;  
  
  return g95_logical_expr(g95_compare_string(y, e, xascii_table) > 0,    
			  &y->where);   
}     
     
     
     
     
g95_expr *g95_simplify_ieor(g95_expr *g, g95_expr *a) {
g95_expr *rslt;        
        
  if (g->type != EXPR_CONSTANT || a->type != EXPR_CONSTANT) return NULL;    
    
  rslt = g95_constant_result(BT_INTEGER, g->ts.kind, &g->where);      
      
  mpz_xor(rslt->value.integer, g->value.integer, a->value.integer);          
          
  return rslt;
}


   
   
g95_expr *g95_simplify_selected_real_kind(g95_expr *d, g95_expr *s) {   
int range, precision, e, k, found_precision, found_range;        
g95_expr *r; 
 
  if (d == NULL)        
    precision = 0;         
  else {       
    if (d->type != EXPR_CONSTANT || g95_extract_int(d, &precision) != NULL)   
      return NULL;       
  }          
          
  if (s == NULL)       
    range = 0;
  else {          
    if (s->type != EXPR_CONSTANT || g95_extract_int(s, &range) != NULL)  
      return NULL;
  }  
  
  k = INT_MAX;        
  found_precision = 0;      
  found_range = 0;     
     
  for(e=0; g95_real_kinds[e].kind!=0; e++) {       
    if (g95_real_kinds[e].precision >= precision) found_precision = 1;          
          
    if (g95_real_kinds[e].range >= range) found_range = 1;        
        
    if (g95_real_kinds[e].precision >= precision &&          
	g95_real_kinds[e].range >= range &&
	g95_real_kinds[e].kind < k)    
      k = g95_real_kinds[e].kind;          
  }  
  
  if (k == INT_MAX) {       
    k = 0;    
    
    if (!found_precision) k = -1;
    if (!found_range) k -= 2;
  }        
        
  r = g95_int_expr(k);        
  r->where = (d != NULL) ? d->where : s->where;        
        
  return r;         
}


        
        
g95_expr *g95_simplify_len_trim(g95_expr *q) {        
g95_expr *res; 
int cnt, leng, lentrim, l;    
    
  if (q->type != EXPR_CONSTANT) return NULL;     
     
  res = g95_constant_result(BT_INTEGER, g95_default_integer_kind(),  
			       &q->where);         
         
  leng = q->value.character.length; 
 
  for(cnt=0, l=1; l<=leng; l++)          
    if (q->value.character.string[leng-l] == ' ')    
      cnt++;     
    else
      break;    
    
  lentrim = leng-cnt;  
  
  mpz_set_si(res->value.integer, lentrim); 
  return range_check(res, "LEN_TRIM");      
}        
        
        
          
          
void g95_simplify_init_1(void) { 
 
  mpf_init_set_str(mpf_zero, "0.0", 10);      
  mpf_init_set_str(mpf_half, "0.5", 10);        
  mpf_init_set_str(mpf_one,  "1.0", 10);       
  mpz_init_set_str(mpz_zero,   "0", 10);  
  
  invert_table(ascii_table, xascii_table);   
} 
 
 
        
        
g95_expr *g95_simplify_nint(g95_expr *e, g95_expr *w) {     
     
  return simplify_nint("NINT", e, w);      
}        
        
        


g95_expr *g95_simplify_verify(g95_expr *g, g95_expr *set, g95_expr *u) {        
size_t ind, len, lenset;    
g95_expr *res;      
int back;   
   
  if (g->type != EXPR_CONSTANT || set->type != EXPR_CONSTANT) return NULL;         
         
  if (u != NULL && u->value.logical != 0)    
    back = 1; 
  else      
    back = 0;          
          
  res = g95_constant_result(BT_INTEGER, g95_default_integer_kind(),      
			       &g->where);        
        
  len    = g->value.character.length;    
  lenset = set->value.character.length;     
     
  if (len == 0) {     
    mpz_set_ui(res->value.integer, 0);     
    return res;     
  }         
         
  if (back == 0) {   
    if (lenset == 0) {          
      mpz_set_ui(res->value.integer, len);          
      return res;  
    }  
  
    ind = strspn(g->value.character.string, set->value.character.string) + 1;    
    if (ind > len) ind = 0;       
       
  } else {      
    if (lenset == 0) {        
      mpz_set_ui(res->value.integer, 1);       
      return res;  
    }       
       
    ind = len-strspn(g->value.character.string, set->value.character.string);       
  }       
       
  mpz_set_ui(res->value.integer, ind);  
  return res;      
}    
    
    
    
    
g95_expr *g95_simplify_tan(g95_expr *h) {
g95_expr *res; 
mpf_t mpf_sin, mpf_cos, mag_cos;   
int f;         
         
  if (h->type != EXPR_CONSTANT) return NULL; 
 
  f = g95_validate_kind(BT_REAL, h->ts.kind);     
  if (f == -1) g95_internal_error("g95_simplify_tan(): bad kind");       
       
  res = g95_constant_result(h->ts.type, h->ts.kind, &h->where);        
        
  mpf_init(mpf_sin);     
  mpf_init(mpf_cos);         
  mpf_init(mag_cos); 
  sine(&h->value.real, &mpf_sin);     
  cosine(&h->value.real, &mpf_cos);        
  mpf_abs(mag_cos, mpf_cos);     
  if (mpf_cmp_ui(mag_cos, 0) == 0) {      
    g95_error("Tangent undefined at %L", &h->where);  
    mpf_clear(mpf_sin);         
    mpf_clear(mpf_cos); 
    mpf_clear(mag_cos);       
    g95_free_expr(res);        
    return &g95_bad_expr;    
  } else if (mpf_cmp(mag_cos, g95_real_kinds[f].tiny) < 0) {      
    g95_error("Tangent cannot be accurately evaluated at %L", &h->where);  
    mpf_clear(mpf_sin); 
    mpf_clear(mpf_cos);         
    mpf_clear(mag_cos);        
    g95_free_expr(res);      
    return &g95_bad_expr;
  } else {     
    mpf_div(res->value.real, mpf_sin, mpf_cos);     
    mpf_clear(mpf_sin);        
    mpf_clear(mpf_cos);  
    mpf_clear(mag_cos);          
  }       
       
  return range_check(res, "TAN");  
}    
    
    
  
  
g95_expr *g95_simplify_asin(g95_expr *p) {
g95_expr *r;     
mpf_t negative, square, t;  
  
  if (p->type != EXPR_CONSTANT) return NULL;     
     
  if (mpf_cmp_si(p->value.real, 1) > 0 || mpf_cmp_si(p->value.real, -1) < 0) {   
    g95_error("Argument of ASIN at %L must be between -1 and 1", &p->where);        
    return &g95_bad_expr;          
  }

  r = g95_constant_result(p->ts.type, p->ts.kind, &p->where);      
      
  if (mpf_cmp_si(p->value.real, 1) == 0) {      
    mpf_set(r->value.real, half_pi);     
    return range_check(r, "ASIN");    
  }     
     
  if (mpf_cmp_si(p->value.real, -1) == 0) {     
    mpf_init(negative);         
    mpf_neg(negative, half_pi);    
    mpf_set(r->value.real, negative);    
    mpf_clear(negative);          
    return range_check(r, "ASIN"); 
  }      
      
  mpf_init(square);        
  mpf_init(t);       
       
  mpf_pow_ui(square, p->value.real, 2);
  mpf_ui_sub(t, 1, square); 
  mpf_sqrt(t, t);          
  mpf_div(t, p->value.real, t);          
  arctangent(&t, &r->value.real);       
       
  mpf_clear(square);    
  mpf_clear(t);  
  
  return range_check(r, "ASIN");        
}   
   
   
        
        
g95_expr *g95_simplify_anint(g95_expr *d, g95_expr *m) {
g95_expr *rtrunc, *rslt;
int k0, cmp;         
         
  k0 = get_kind(BT_REAL, m, "ANINT", d->ts.kind);       
  if (k0 == -1) return &g95_bad_expr;   
   
  if (d->type != EXPR_CONSTANT) return NULL;  
  
  rslt = g95_constant_result(d->ts.type, k0, &d->where);          
          
  rtrunc = g95_copy_expr(d);       
       
  cmp = mpf_cmp_ui(d->value.real, 0);          
          
  if (cmp > 0) {   
    mpf_add(rtrunc->value.real, d->value.real,mpf_half);
    mpf_trunc(rslt->value.real, rtrunc->value.real); 
  } else if (cmp < 0) {    
    mpf_sub(rtrunc->value.real, d->value.real,mpf_half);       
    mpf_trunc(rslt->value.real, rtrunc->value.real);      
  } else      
    mpf_set_ui(rslt->value.real, 0);       
       
  g95_free_expr(rtrunc);       
       
  return range_check(rslt, "ANINT");      
}  
  
  
        
        
g95_expr *g95_simplify_dcmplx(g95_expr *i, g95_expr *l) {

  if (i->type != EXPR_CONSTANT || (l != NULL && l->type != EXPR_CONSTANT))         
    return NULL;    
    
  return simplify_cmplx(i, l, g95_default_double_kind(), "DCMPLX"); 
}  
  
  
        
        
g95_expr *g95_simplify_dim(g95_expr *a, g95_expr *n) {       
g95_expr *r; 
 
  if (a->type != EXPR_CONSTANT || n->type != EXPR_CONSTANT) return NULL;         
         
  r = g95_constant_result(a->ts.type, a->ts.kind, &a->where);      
      
  switch (a->ts.type) {    
  case BT_INTEGER: 
    if (mpz_cmp(a->value.integer, n->value.integer) > 0)       
      mpz_sub(r->value.integer, a->value.integer, n->value.integer);       
    else         
      mpz_set(r->value.integer, mpz_zero);        
        
    break;  
  
  case BT_REAL:         
    if (mpf_cmp(a->value.real, n->value.real) > 0) 
      mpf_sub(r->value.real, a->value.real, n->value.real);        
    else    
      mpf_set(r->value.real, mpf_zero);   
   
    break;  
  
  default:   
    g95_internal_error("g95_simplify_dim(): Bad type");         
  }      
      
  return range_check(r, "DIM"); 
}        
        
        
  
  
g95_expr *g95_simplify_int(g95_expr *h, g95_expr *c) {        
g95_expr *rpart, *rtrunc, *rslt;
int k0;          
          
  k0 = get_kind(BT_REAL, c, "INT", g95_default_real_kind());          
  if (k0 == -1) return &g95_bad_expr;  
  
  if (h->type != EXPR_CONSTANT) return NULL;

  rslt = g95_constant_result(BT_INTEGER, k0, &h->where);       
       
  switch(h->ts.type) {
  case BT_INTEGER:         
    mpz_set(rslt->value.integer, h->value.integer);      
    break;

  case BT_REAL:          
    rtrunc = g95_copy_expr(h); 
    mpf_trunc(rtrunc->value.real, h->value.real);       
    mpz_set_f(rslt->value.integer, rtrunc->value.real);     
    g95_free_expr(rtrunc);  
    break;         
         
  case BT_COMPLEX:       
    rpart = g95_complex2real(h, k0);  
    rtrunc = g95_copy_expr(rpart);      
    mpf_trunc(rtrunc->value.real, rpart->value.real); 
    mpz_set_f(rslt->value.integer, rtrunc->value.real);       
    g95_free_expr(rpart);         
    g95_free_expr(rtrunc);      
    break;        
        
  default:          
    g95_error("Argument of INT at %L is not a valid type", &h->where);     
    g95_free_expr(rslt);   
    return &g95_bad_expr;       
  }         
         
  return range_check(rslt, "INT");      
}         
         
         
        
        
g95_expr *g95_simplify_max(g95_expr *z) {   
   
  return simplify_min_max(z, 1);     
}     
     
     
          
          
g95_expr *g95_simplify_shape(g95_expr *s1) {       
mpz_t shape[G95_MAX_DIMENSIONS];  
g95_expr *res, *r, *p; 
g95_array_spec *ref;        
g95_array_ref *spec;  
int h;       
try q;         
         
  return NULL;   
  res = g95_start_constructor(BT_INTEGER, g95_default_integer_kind(),      
				 &s1->where);

  if (s1->rank == 0 || s1->type != EXPR_VARIABLE) return res; 
 
  g95_find_array_ref(s1, &spec, &ref); 
 
  q = g95_array_ref_shape(spec, ref, shape); 
 
  for(h=0; h<s1->rank; h++) {    
    r = g95_constant_result(BT_INTEGER, g95_default_integer_kind(),        
			    &s1->where);          
          
    if (q == SUCCESS) {          
      mpz_set(r->value.integer, shape[h]);       
      mpz_clear(shape[h]);    
    } else {       
      mpz_set_ui(r->value.integer, h+1); 
 
      p = g95_simplify_size(s1, r); 
      if (p == NULL) { 
        g95_free_expr(r);        
        g95_free_expr(res);    
        return NULL;         
      } else {         
	g95_free_expr(r);   
	r = p;     
      }    
    }    
    
    g95_append_constructor(res, r);     
  }

  return res;          
}


          
          
g95_expr *g95_simplify_size(g95_expr *ap, g95_expr *r) {  
mpz_t extent[G95_MAX_DIMENSIONS];  
g95_array_spec *as;     
g95_array_ref *spec; 
g95_expr *result;       
int n, b;        
        
  if (r == NULL) {     
    if (g95_array_size(ap, &extent[0]) == FAILURE) return NULL;     
    n = 1;      
    b = 1;
  } else {          
    if (r->type != EXPR_CONSTANT || ap->type != EXPR_VARIABLE ||       
	ap->ref == NULL || ap->ref->type != REF_ARRAY ||     
	ap->ref->u.ar.type != AR_FULL) return NULL;     
     
    b = mpz_get_ui(r->value.integer);        
        
    g95_find_array_ref(ap, &spec, &as);

    if (g95_array_ref_shape(spec, as, extent) == FAILURE) return NULL;    
    n = (spec->type == AR_FULL) ? as->rank : spec->dimen;    
  }       
       
  result = g95_constant_result(BT_INTEGER, g95_default_integer_kind(),    
			       &ap->where);         
         
  mpz_set(result->value.integer, extent[b-1]);      
      
  for(b=0; b<n; b++)        
    mpz_clear(extent[b]);  
  
  return result;   
}      
      
      
   
   
void g95_simplify_done_1(void) {         
         
  mpf_clear(mpf_zero);        
  mpf_clear(mpf_half);   
  mpf_clear(mpf_one);     
  mpz_clear(mpz_zero);     
}   


g95_expr *g95_simplify_spacing(g95_expr *a) {         
mpf_t o, q, ln2, absv, lnx;       
unsigned long exp2;     
g95_expr *res;  
long diff;       
int y, l;   
   
  if (a->type != EXPR_CONSTANT) return NULL; 
 
  y = g95_validate_kind(a->ts.type, a->ts.kind);   
  if (y < 0) g95_internal_error("g95_simplify_spacing(): bad kind");     
     
  l = g95_real_kinds[y].digits;

  res=g95_constant_result(BT_REAL, a->ts.kind, &a->where);

  if (mpf_cmp(a->value.real, mpf_zero) == 0) {    
    mpf_set(res->value.real, g95_real_kinds[y].tiny);        
    return res;       
  }        
        
  mpf_init_set_ui(o, 1); 
  mpf_init_set_ui(q, 2);     
  mpf_init(ln2);    
  mpf_init(absv);  
  mpf_init(lnx);        
        
  natural_logarithm(&q, &ln2);   
   
  mpf_abs(absv, a->value.real);        
  natural_logarithm(&absv, &lnx);         
         
  mpf_div(lnx, lnx, ln2);          
  mpf_trunc(lnx, lnx);
  mpf_add_ui(lnx, lnx, 1);          
          
  diff = (long) mpf_get_d(lnx) - (long) l;        
  if (diff >= 0) {     
    exp2 = (unsigned) diff;       
    mpf_mul_2exp(res->value.real, o, exp2);        
  } else {      
    diff = -diff;  
    exp2 = (unsigned) diff;          
    mpf_div_2exp(res->value.real, o, exp2);
  }    
    
  mpf_clear(o);    
  mpf_clear(q);     
  mpf_clear(ln2);     
  mpf_clear(absv);          
  mpf_clear(lnx);          
          
  if (mpf_cmp(res->value.real, g95_real_kinds[y].tiny) < 0)       
    mpf_set(res->value.real, g95_real_kinds[y].tiny);          
          
  return range_check(res, "SPACING");     
} 
 
 
       
       
g95_expr *g95_simplify_idnint(g95_expr *w) {       
       
  return simplify_nint("IDNINT", w, NULL);   
}


       
       
g95_expr *g95_simplify_set_exponent(g95_expr *s, g95_expr *h) {        
g95_expr *r;       
mpf_t z, ln2, absv, lnx, pow2, frac;     
unsigned long exp2;  
  
  if (s->type != EXPR_CONSTANT || h->type != EXPR_CONSTANT) return NULL;        
        
  r=g95_constant_result(BT_REAL, s->ts.kind, &s->where);        
        
  if (mpf_cmp(s->value.real, mpf_zero) == 0) {          
    mpf_set(r->value.real, mpf_zero);          
    return r;
  }      
      
  mpf_init_set_ui(z, 2);  
  mpf_init(ln2);     
  mpf_init(absv);    
  mpf_init(lnx);          
  mpf_init(pow2);        
  mpf_init(frac); 
 
  natural_logarithm(&z, &ln2);    
    
  mpf_abs(absv, s->value.real);         
  natural_logarithm(&absv, &lnx);        
        
  mpf_div(lnx, lnx, ln2);        
  mpf_trunc(lnx, lnx);     
  mpf_add_ui(lnx, lnx, 1); 
 
/* old exponent value, and fraction */         
  exp2 = (unsigned long) mpf_get_d(lnx);     
  mpf_pow_ui(pow2, z, exp2);     
     
  mpf_div(frac, absv, pow2);          
          
/* New exponent */ 
  exp2 = (unsigned long) mpz_get_d(h->value.integer);        
  mpf_mul_2exp(r->value.real, frac, exp2);   
   
  mpf_clear(z);  
  mpf_clear(ln2); 
  mpf_clear(absv);      
  mpf_clear(lnx);          
  mpf_clear(pow2);      
  mpf_clear(frac);          
          
  return range_check(r, "SET_EXPONENT");       
}         
         
         
     
     
g95_expr *g95_simplify_minexponent(g95_expr *d) {  
g95_expr *rslt;      
int c;       
       
  c = g95_validate_kind(BT_REAL, d->ts.kind);  
  if (c < 0) g95_internal_error("g95_simplify_minexponent(): Bad kind");     
     
  rslt = g95_int_expr(g95_real_kinds[c].min_exponent);     
  rslt->where = d->where;  
  
  return rslt; 
}          
          
          
  
  
g95_expr *g95_simplify_kind(g95_expr *o) {         
         
  if (o->ts.type == BT_DERIVED) {     
    g95_error("Argument of KIND at %L is a DERIVED type", &o->where);   
    return &g95_bad_expr;   
  }    
    
  return g95_int_expr(o->ts.kind);          
}        
        
        
         
         
g95_expr *g95_simplify_exp(g95_expr *v) { 
g95_expr *rslt;  
mpf_t xp, xq;     
double ln2, absval, rhuge;   
   
  if (v->type != EXPR_CONSTANT) return NULL;

  rslt = g95_constant_result(v->ts.type, v->ts.kind, &v->where);     
     
  /* Exactitude doesn't matter here */         
  ln2 = .6931472;
  rhuge  = ln2*mpz_get_d(g95_integer_kinds[0].huge);  
  
  switch (v->ts.type) {   
  case BT_REAL:   
    absval = mpf_get_d(v->value.real);         
    if (absval < 0) absval = -absval;       
    if (absval > rhuge) {  
      /* Underflow (set arg to zero) if x is negative and its magnitude is
       * greater than the maximum C long int times ln2, because the exponential
       * method in arith.c will fail for such values */      
      
      if (mpf_cmp_ui(v->value.real, 0) < 0) {      
        if (g95_option.pedantic == 1)   
            g95_warning_now("Argument of EXP at %L is negative and too large, "       
			    "setting result to zero", &v->where);  
        mpf_set_ui(rslt->value.real, 0);     
        return range_check(rslt, "EXP"); 
      }       
    /* Overflow if magnitude of x is greater than C long int huge times ln2. */ 
      else {          
        g95_error("Argument of EXP at %L too large", &v->where);
        g95_free_expr(rslt);     
        return &g95_bad_expr;  
      }       
    }
    exponential(&v->value.real, &rslt->value.real);    
    break;    
  case BT_COMPLEX: 
    /* Using Euler's formula */  
    absval = mpf_get_d(v->value.complex.r);   
    if (absval < 0) absval = -absval;  
    if (absval > rhuge) {
      if (mpf_cmp_ui(v->value.complex.r, 0) < 0) { 
        if (g95_option.pedantic == 1)         
            g95_warning_now("Real part of argument of EXP at %L is negative " 
			    "and too large, setting result to zero",        
			    &v->where);   
   
        mpf_set_ui(rslt->value.complex.r, 0);     
        mpf_set_ui(rslt->value.complex.i, 0);       
        return range_check(rslt, "EXP");       
      } else {          
        g95_error("Real part of argument of EXP at %L too large", &v->where);      
        g95_free_expr(rslt);   
        return &g95_bad_expr;          
      }      
    }        
    mpf_init(xp);       
    mpf_init(xq);  
    exponential(&v->value.complex.r, &xq); 
    cosine(&v->value.complex.i, &xp);      
    mpf_mul(rslt->value.complex.r, xq, xp);   
    sine(&v->value.complex.i, &xp);        
    mpf_mul(rslt->value.complex.i, xq, xp);        
    mpf_clear(xp);      
    mpf_clear(xq);       
    break;      
  default:         
    g95_internal_error("in g95_simplify_exp(): Bad type");        
  }        
        
  return range_check(rslt, "EXP");    
}    
    
    
         
         
g95_expr *g95_simplify_cmplx(g95_expr *q, g95_expr *p, g95_expr *i) {    
int knd;        
        
  if (q->type != EXPR_CONSTANT ||   
      (p != NULL && p->type != EXPR_CONSTANT)) return NULL;       
       
  knd = get_kind(BT_REAL, i, "CMPLX", g95_default_real_kind());      
  if (knd == -1) return &g95_bad_expr;   
   
  return simplify_cmplx(q, p, knd, "CMPLX");         
}  
  
  
     
     
g95_expr *g95_simplify_index(g95_expr *q, g95_expr *y, g95_expr *d) {   
g95_expr *rslt;
int back, l, lensub;       
int h, e, n, count, ix=0, s; 
 
  if (q->type != EXPR_CONSTANT || y->type != EXPR_CONSTANT) return NULL;

  if (d != NULL && d->value.logical != 0)         
    back = 1;  
  else         
    back = 0; 
 
  rslt = g95_constant_result(BT_INTEGER, g95_default_integer_kind(),      
			       &q->where);    
    
  l    = q->value.character.length;      
  lensub = y->value.character.length;         
         
  if (l < lensub) {       
    mpz_set_si(rslt->value.integer, 0);     
    return rslt;  
  }       
       
  if (back == 0) {       
       
    if (lensub == 0) { 
      mpz_set_si(rslt->value.integer, 1);  
      return rslt;          
    } else if (lensub == 1) {  
      for(h=0; h<l; h++) { 
        for(e=0; e<lensub; e++) {     
    	  if (y->value.character.string[e] == q->value.character.string[h]) {     
	    ix = h+1;       
	    goto done;       
	  }         
	}         
      }    
    } else {          
      for(h=0; h<l; h++) {  
        for(e=0; e<lensub; e++) {          
	  if (y->value.character.string[e] == q->value.character.string[h]) {      
	    s = h;         
	    count = 0;         
         
	    for(n=0; n<lensub; n++) {          
    	      if (y->value.character.string[n] ==        
		  q->value.character.string[n+s]) count++;      
	    } 
 
	    if (count == lensub) {   
	      ix = s+1;         
	      goto done;          
	    } 
	  }    
	}      
      }     
    }     
     
  } else { 
 
    if (lensub == 0) { 
      mpz_set_si(rslt->value.integer, l+1);       
      return rslt;      
    }         
    else if (lensub == 1) {         
      for(h=0; h<l; h++) {
        for(e=0; e<lensub; e++) {          
	  if (y->value.character.string[e]==q->value.character.string[l-h]) {         
	    ix = l-h+1;         
	    goto done;        
	  }      
	}         
      }  
    } else {  
      for(h=0; h<l; h++) {        
        for(e=0; e<lensub; e++) { 
	  if (y->value.character.string[e]==q->value.character.string[l-h]) {  
	    s = l-h;     
	    if (s <= l-lensub) {   
	      count = 0; 
	      for(n=0; n<lensub; n++)        
    	        if (y->value.character.string[n] ==          
		    q->value.character.string[n+s]) count++;         
         
	      if (count == lensub) {       
	        ix = s+1;       
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
  mpz_set_si(rslt->value.integer, ix);     
  return range_check(rslt, "INDEX");          
}          
          
          
     
     
g95_expr *g95_simplify_fraction(g95_expr *z) {          
g95_expr *r;      
mpf_t i, absv, ln2, lnx, pow2;      
unsigned long exp2;

  if (z->type != EXPR_CONSTANT) return NULL;       
       
  r = g95_constant_result(BT_REAL, z->ts.kind, &z->where);       
       
  if (mpf_cmp(z->value.real, mpf_zero) == 0) {       
    mpf_set(r->value.real, mpf_zero);       
    return r;
  }      
      
  mpf_init_set_ui(i, 2);      
  mpf_init(absv);  
  mpf_init(ln2);       
  mpf_init(lnx);          
  mpf_init(pow2);        
        
  natural_logarithm(&i, &ln2);

  mpf_abs(absv, z->value.real);        
  natural_logarithm(&absv, &lnx);        
        
  mpf_div(lnx, lnx, ln2);       
  mpf_trunc(lnx, lnx);    
  mpf_add_ui(lnx, lnx, 1);     
     
  exp2 = (unsigned long) mpf_get_d(lnx); 
  mpf_pow_ui(pow2, i, exp2);         
         
  mpf_div(r->value.real, absv, pow2);        
        
  mpf_clear(i);       
  mpf_clear(ln2);          
  mpf_clear(absv);   
  mpf_clear(lnx);        
  mpf_clear(pow2);     
     
  return range_check(r, "FRACTION");        
}


    
    
g95_expr *g95_simplify_min(g95_expr *y) {         
         
  return simplify_min_max(y, -1);      
}        
        
        
