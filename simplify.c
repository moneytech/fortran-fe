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
      
      
    
    
g95_expr *g95_simplify_shape(g95_expr *s0) {   
mpz_t extent[G95_MAX_DIMENSIONS];
g95_expr *result, *d, *v;
g95_array_spec *ref;      
g95_array_ref *a;         
int x;    
try o;       
       
  return NULL;     
  result = g95_start_constructor(BT_INTEGER, g95_default_integer_kind(),
				 &s0->where);       
       
  if (s0->rank == 0 || s0->type != EXPR_VARIABLE) return result;          
          
  g95_find_array_ref(s0, &a, &ref);        
        
  o = g95_array_ref_shape(a, ref, extent);          
          
  for(x=0; x<s0->rank; x++) {          
    d = g95_constant_result(BT_INTEGER, g95_default_integer_kind(),  
			    &s0->where);      
      
    if (o == SUCCESS) {    
      mpz_set(d->value.integer, extent[x]);   
      mpz_clear(extent[x]);   
    } else {   
      mpz_set_ui(d->value.integer, x+1);        
        
      v = g95_simplify_size(s0, d);  
      if (v == NULL) {  
        g95_free_expr(d);     
        g95_free_expr(result);         
        return NULL;      
      } else {          
	g95_free_expr(d);  
	d = v;       
      }        
    }          
          
    g95_append_constructor(result, d);     
  }       
       
  return result;      
}     
     
     
  
  
g95_expr *g95_simplify_adjustl(g95_expr *b) { 
g95_expr *res;       
int count, q, leng;       
char ch;        
        
  if (b->type != EXPR_CONSTANT) return NULL; 
 
  leng = b->value.character.length;    
    
  res = g95_constant_result(BT_CHARACTER, b->ts.kind, &b->where); 
 
  res->value.character.length = leng;         
  res->value.character.string = g95_getmem(leng+1);      
      
  for (count=0, q=0; q<leng; ++q) {   
    ch = b->value.character.string[q];        
    if (ch != ' ') break;
    ++count;  
  }       
       
  for (q=0; q<leng-count; ++q) {  
    res->value.character.string[q] = b->value.character.string[count+q];        
  }          
          
  for (q=leng-count; q<leng; ++q) {  
    res->value.character.string[q] = ' ';         
  }  
  
  res->value.character.string[leng] = '\0';   /* For debugger */    
    
  return res;   
}  
  
  
 
 
/* get_kind()-- A helper function that gets an optional and possibly
 * missing kind parameter.  Returns the kind, -1 if something went
 * wrong. */  
  
static int get_kind(bt type, g95_expr *d, char *nam, int default_kind) {       
int kind;   
   
  if (d == NULL) return default_kind;         
         
  if (d->type != EXPR_CONSTANT) { 
    g95_error("KIND parameter of %s at %L must be an initialization "          
	      "expression", nam, &d->where);     
     
    return -1;    
  }

  if (g95_extract_int(d, &kind) != NULL ||     
      g95_validate_kind(type, kind) < 0) {        
        
    g95_error("Invalid KIND parameter of %s at %L", nam, &d->where);  
    return -1; 
  }         
         
  return kind;       
}    
    
    
 
 
g95_expr *g95_simplify_bit_size(g95_expr *l) {          
g95_expr *r;     
int d;        
        
  d = g95_validate_kind(l->ts.type, l->ts.kind);
  if (d < 0) g95_internal_error("In g95_simplify_bit_size(): bad kind");         
         
  r = g95_constant_result(BT_INTEGER, l->ts.kind, &l->where);          
  mpz_set_ui(r->value.integer, g95_integer_kinds[d].bit_size);  
  
  return r;     
}         
         
         
   
   
g95_expr *g95_simplify_adjustr(g95_expr *c) {        
g95_expr *result;      
int cnt, m, len;    
char ch;

  if (c->type != EXPR_CONSTANT) return NULL;

  len = c->value.character.length;     
     
  result = g95_constant_result(BT_CHARACTER, c->ts.kind, &c->where);        
        
  result->value.character.length = len;  
  result->value.character.string = g95_getmem(len+1);

  for (cnt=0, m=len-1; m>=0; --m) { 
    ch = c->value.character.string[m]; 
    if (ch != ' ') break;          
    ++cnt;     
  }    
    
  for (m=0; m<cnt; ++m) { 
    result->value.character.string[m] = ' '; 
  } 
 
  for (m=cnt; m<len; ++m) {         
    result->value.character.string[m] = c->value.character.string[m-cnt];  
  }         
         
  result->value.character.string[len] = '\0';   /* For debugger */          
          
  return result;    
}    
    
    
  
  
/* range_check()-- Range checks an expression node.  If all goes well,
 * returns the node, otherwise returns &g95_bad_expr and frees the node. */        
        
static g95_expr *range_check(g95_expr *r, char *n) { 
 
  if (g95_range_check(r) == ARITH_OK)
    return r;   
   
  g95_error("Result of %s overflows its kind at %L", n, &r->where);          
  g95_free_expr(r);      
  return &g95_bad_expr; 
}         
         
         
    
    
g95_expr *g95_simplify_spacing(g95_expr *l) {    
mpf_t z, n, ln2, absv, lnx;
unsigned long exp2; 
g95_expr *rslt;     
long diff;       
int c, a;      
      
  if (l->type != EXPR_CONSTANT) return NULL;       
       
  c = g95_validate_kind(l->ts.type, l->ts.kind);     
  if (c < 0) g95_internal_error("g95_simplify_spacing(): bad kind");    
    
  a = g95_real_kinds[c].digits;

  rslt=g95_constant_result(BT_REAL, l->ts.kind, &l->where);   
   
  if (mpf_cmp(l->value.real, mpf_zero) == 0) {          
    mpf_set(rslt->value.real, g95_real_kinds[c].tiny);     
    return rslt;   
  }         
         
  mpf_init_set_ui(z, 1);    
  mpf_init_set_ui(n, 2);          
  mpf_init(ln2);  
  mpf_init(absv);         
  mpf_init(lnx);        
        
  natural_logarithm(&n, &ln2);          
          
  mpf_abs(absv, l->value.real);  
  natural_logarithm(&absv, &lnx); 
 
  mpf_div(lnx, lnx, ln2);       
  mpf_trunc(lnx, lnx);      
  mpf_add_ui(lnx, lnx, 1);       
       
  diff = (long) mpf_get_d(lnx) - (long) a;      
  if (diff >= 0) { 
    exp2 = (unsigned) diff;       
    mpf_mul_2exp(rslt->value.real, z, exp2);
  } else {      
    diff = -diff;        
    exp2 = (unsigned) diff;         
    mpf_div_2exp(rslt->value.real, z, exp2);     
  }         
         
  mpf_clear(z);       
  mpf_clear(n);
  mpf_clear(ln2);
  mpf_clear(absv);        
  mpf_clear(lnx);       
       
  if (mpf_cmp(rslt->value.real, g95_real_kinds[c].tiny) < 0)   
    mpf_set(rslt->value.real, g95_real_kinds[c].tiny);          
          
  return range_check(rslt, "SPACING");          
}


       
       
g95_expr *g95_simplify_precision(g95_expr *c) {       
g95_expr *res;  
int l;        
        
  l = g95_validate_kind(c->ts.type, c->ts.kind);        
  if (l == -1) g95_internal_error("g95_simplify_precision(): Bad kind");   
   
  res = g95_int_expr(g95_real_kinds[l].precision);   
  res->where = c->where;       
       
  return res;          
}         
         
         
    
    
g95_expr *g95_simplify_dble(g95_expr *t) { 
g95_expr *result; 
 
  if (t->type != EXPR_CONSTANT) return NULL;      
      
  switch (t->ts.type) {         
  case BT_INTEGER:     
    result = g95_int2real(t, g95_default_double_kind()); 
    break;     
     
  case BT_REAL:        
    result = g95_real2real(t, g95_default_double_kind());  
    break;          
          
  case BT_COMPLEX:       
    result = g95_complex2real(t, g95_default_double_kind());
    break;   
   
  default:      
    g95_internal_error("g95_simplify_dble(): bad type at %L", &t->where); 
  }         
         
  return range_check(result, "DBLE");          
}          
          
          
 
 
g95_expr *g95_simplify_lge(g95_expr *m, g95_expr *e) {         
         
  if (m->type != EXPR_CONSTANT || e->type != EXPR_CONSTANT) return NULL;        
        
  return g95_logical_expr(g95_compare_string(m, e, xascii_table) >= 0,        
			  &m->where);   
}


          
          
g95_expr *g95_simplify_ibset(g95_expr *x, g95_expr *g) { 
g95_expr *rslt;   
int p, pos;        
        
  if (x->type != EXPR_CONSTANT || g->type != EXPR_CONSTANT) return NULL; 
 
  if (g95_extract_int(g, &pos) != NULL || pos < 0) {         
    g95_error("Invalid second argument of IBSET at %L", &g->where);          
    return &g95_bad_expr;
  } 
 
  p = g95_validate_kind(x->ts.type, x->ts.kind);          
  if (p == -1) g95_internal_error("In g95_simplify_ibset: bad kind");

  if (pos > g95_integer_kinds[p].bit_size) { 
    g95_error("Second argument of IBSET exceeds bit size at %L", &g->where);         
    return &g95_bad_expr;      
  } 
 
  rslt = g95_copy_expr(x);        
        
  mpz_setbit(rslt->value.integer, pos);        
  return range_check(rslt, "IBSET");     
}         
         
         
   
   
g95_expr *g95_simplify_atan2(g95_expr *q, g95_expr *w) {     
g95_expr *res;    
mpf_t term;          
          
  if (w->type != EXPR_CONSTANT || q->type != EXPR_CONSTANT) return NULL;   
   
  res = g95_constant_result(w->ts.type, w->ts.kind, &w->where);          
          
  mpf_init(term);       
       
  if (mpf_cmp_ui(q->value.real, 0) == 0) {  
    if (mpf_cmp_ui(w->value.real, 0) == 0) {          
      mpf_clear(term); 
      g95_error("If first argument of ATAN2 %L is zero, the second argument "   
		"must not be zero", &w->where);      
      g95_free_expr(res);         
      return &g95_bad_expr;       
    }          
    else if (mpf_cmp_si(w->value.real, 0) < 0) {        
      mpf_set(res->value.real, pi);          
      mpf_clear(term);         
      return res;         
    }   
    else if (mpf_cmp_si(w->value.real, -1)== 0) {  
      mpf_set_ui(res->value.real, 0);    
      mpf_clear(term); 
      return range_check(res, "ATAN2");         
    }         
  }      
      
  if (mpf_cmp_ui(w->value.real, 0) == 0) { 
    if (mpf_cmp_si(q->value.real, 0) < 0) {       
      mpf_neg(term, half_pi);     
      mpf_set(res->value.real, term);        
      mpf_clear(term);       
      return range_check(res, "ATAN2");  
    }      
    else if (mpf_cmp_si(q->value.real, 0) > 0) {     
      mpf_set(res->value.real, half_pi);       
      mpf_clear(term);         
      return range_check(res, "ATAN2");   
    }      
  }    
    
  mpf_div(term, q->value.real, w->value.real);       
  arctangent(&term, &res->value.real);      
      
  mpf_clear(term);       
       
  return range_check(res, "ATAN2");   
   
}  
  
  
      
      
g95_expr *g95_simplify_dim(g95_expr *m, g95_expr *d) { 
g95_expr *rslt;    
    
  if (m->type != EXPR_CONSTANT || d->type != EXPR_CONSTANT) return NULL;       
       
  rslt = g95_constant_result(m->ts.type, m->ts.kind, &m->where);        
        
  switch (m->ts.type) {  
  case BT_INTEGER:          
    if (mpz_cmp(m->value.integer, d->value.integer) > 0)          
      mpz_sub(rslt->value.integer, m->value.integer, d->value.integer); 
    else      
      mpz_set(rslt->value.integer, mpz_zero);      
      
    break;  
  
  case BT_REAL:   
    if (mpf_cmp(m->value.real, d->value.real) > 0)     
      mpf_sub(rslt->value.real, m->value.real, d->value.real);          
    else    
      mpf_set(rslt->value.real, mpf_zero);    
    
    break;      
      
  default:  
    g95_internal_error("g95_simplify_dim(): Bad type");         
  }   
   
  return range_check(rslt, "DIM");        
}


   
   
g95_expr *g95_simplify_exp(g95_expr *s) {      
g95_expr *rslt;    
mpf_t xp, xq;
double ln2, absval, rhuge;    
    
  if (s->type != EXPR_CONSTANT) return NULL;    
    
  rslt = g95_constant_result(s->ts.type, s->ts.kind, &s->where);          
          
  /* Exactitude doesn't matter here */      
  ln2 = .6931472;         
  rhuge  = ln2*mpz_get_d(g95_integer_kinds[0].huge);  
  
  switch (s->ts.type) {     
  case BT_REAL:
    absval = mpf_get_d(s->value.real);     
    if (absval < 0) absval = -absval;  
    if (absval > rhuge) {          
      /* Underflow (set arg to zero) if x is negative and its magnitude is
       * greater than the maximum C long int times ln2, because the exponential
       * method in arith.c will fail for such values */ 
 
      if (mpf_cmp_ui(s->value.real, 0) < 0) {         
        if (g95_option.fmode != 0)      
            g95_warning_now(118, "Argument of EXP at %L is negative and too "    
			    "large, setting result to zero", &s->where);  
        mpf_set_ui(rslt->value.real, 0);    
        return range_check(rslt, "EXP");          
      }  
    /* Overflow if magnitude of x is greater than C long int huge times ln2. */  
      else {     
        g95_error("Argument of EXP at %L too large", &s->where);     
        g95_free_expr(rslt);        
        return &g95_bad_expr; 
      } 
    }          
    exponential(&s->value.real, &rslt->value.real);       
    break;     
  case BT_COMPLEX:   
    /* Using Euler's formula */        
    absval = mpf_get_d(s->value.complex.r);       
    if (absval < 0) absval = -absval;    
    if (absval > rhuge) { 
      if (mpf_cmp_ui(s->value.complex.r, 0) < 0) {   
        if (g95_option.fmode != 0)    
	  g95_warning_now(119, "Real part of argument of EXP at %L is "
			  "negative and too large, setting result to zero",     
			  &s->where); 
 
        mpf_set_ui(rslt->value.complex.r, 0);
        mpf_set_ui(rslt->value.complex.i, 0);  
        return range_check(rslt, "EXP");          
      } else {   
        g95_error("Real part of argument of EXP at %L too large", &s->where);     
        g95_free_expr(rslt);  
        return &g95_bad_expr;       
      }
    }  
    mpf_init(xp);    
    mpf_init(xq);  
    exponential(&s->value.complex.r, &xq);         
    cosine(&s->value.complex.i, &xp);    
    mpf_mul(rslt->value.complex.r, xq, xp);         
    sine(&s->value.complex.i, &xp);     
    mpf_mul(rslt->value.complex.i, xq, xp);         
    mpf_clear(xp);         
    mpf_clear(xq);  
    break; 
  default:      
    g95_internal_error("in g95_simplify_exp(): Bad type");    
  }          
          
  return range_check(rslt, "EXP");       
}      
      
      
      
      
g95_expr *g95_simplify_dnint(g95_expr *s) {         
g95_expr *rtrunc, *result;        
int cmp;     
     
  if (s->type != EXPR_CONSTANT) return NULL;     
     
  result = g95_constant_result(BT_REAL, g95_default_double_kind(), &s->where);       
       
  rtrunc = g95_copy_expr(s);

  cmp = mpf_cmp_ui(s->value.real, 0);   
   
  if (cmp > 0) {     
    mpf_add(rtrunc->value.real, s->value.real, mpf_half);          
    mpf_trunc(result->value.real, rtrunc->value.real);       
  } else if (cmp < 0) {  
    mpf_sub(rtrunc->value.real, s->value.real, mpf_half);   
    mpf_trunc(result->value.real, rtrunc->value.real);          
  } else          
    mpf_set_ui(result->value.real, 0);        
        
  g95_free_expr(rtrunc);

  return range_check(result, "DNINT");     
}         
         
         
          
          
g95_expr *g95_simplify_modulo(g95_expr *e, g95_expr *m) {       
g95_expr *result;        
mpf_t quot, iquot, u;         
         
  if (e->type != EXPR_CONSTANT || m->type != EXPR_CONSTANT) return NULL;   
   
  result = g95_constant_result(e->ts.type, e->ts.kind, &e->where);    
    
  switch (e->ts.type) {
  case BT_INTEGER:      
    if (mpz_cmp_ui(m->value.integer, 0) == 0) {        
      /* Result is processor-dependent, and this processor doesn't handle it */  
      g95_error("Second argument of MODULO at %L is zero", &e->where);    
      g95_free_expr(result);          
      return &g95_bad_expr;   
    }
    mpz_fdiv_r(result->value.integer, e->value.integer, m->value.integer);      
      
    break;     
     
  case BT_REAL:         
    if (mpf_cmp_ui(m->value.real, 0) == 0) {
      /* Result is processor-dependent */          
      g95_error("Second argument of MODULO at %L is zero", &m->where);   
      g95_free_expr(result);         
      return &g95_bad_expr;
    }         
         
    mpf_init(quot); 
    mpf_init(iquot);  
    mpf_init(u);    
    
    mpf_div(quot, e->value.real, m->value.real);         
    mpf_floor(iquot, quot);    
    mpf_mul(u, iquot, m->value.real);  
  
    mpf_clear(quot);       
    mpf_clear(iquot);      
    mpf_clear(u);         
         
    mpf_sub(result->value.real, e->value.real, u);         
    break;     
     
  default:
    g95_internal_error("g95_simplify_modulo(): Bad arguments");  
  }    
    
  return range_check(result, "MODULO"); 
}  
  
  
         
         
g95_expr *g95_simplify_repeat(g95_expr *k, g95_expr *p) {      
g95_expr *res;      
int r, w, leng, ncopies, nlen;

  if (k->type != EXPR_CONSTANT || p->type != EXPR_CONSTANT) return NULL;    
    
  if (p !=NULL && (g95_extract_int(p, &ncopies) != NULL || ncopies < 0)) {    
    g95_error("Invalid second argument of REPEAT at %L", &p->where);        
    return &g95_bad_expr;   
  }    
    
  leng    = k->value.character.length;       
  nlen   = ncopies*leng;        
        
  res = g95_constant_result(BT_CHARACTER, k->ts.kind, &k->where);    
    
  if (ncopies == 0) {        
    res->value.character.string=g95_getmem(1);        
    res->value.character.length=0;        
    res->value.character.string='\0';
    return res;      
  }  
  
  res->value.character.length=nlen;     
  res->value.character.string=g95_getmem(nlen+1);         
         
  for(r=0; r<ncopies; r++)   
    for(w=0; w<leng; w++)  
      res->value.character.string[w+r*leng] = k->value.character.string[w];     
     
  res->value.character.string[nlen] = '\0';  /* For debugger */ 
  return res;       
}          
          
          
         
         
g95_expr *g95_simplify_asin(g95_expr *t) {
g95_expr *r;          
mpf_t negative, square, term;         
         
  if (t->type != EXPR_CONSTANT) return NULL;    
    
  if (mpf_cmp_si(t->value.real, 1) > 0 || mpf_cmp_si(t->value.real, -1) < 0) {       
    g95_error("Argument of ASIN at %L must be between -1 and 1", &t->where);        
    return &g95_bad_expr; 
  }          
          
  r = g95_constant_result(t->ts.type, t->ts.kind, &t->where);     
     
  if (mpf_cmp_si(t->value.real, 1) == 0) {          
    mpf_set(r->value.real, half_pi);         
    return range_check(r, "ASIN");         
  }          
          
  if (mpf_cmp_si(t->value.real, -1) == 0) {    
    mpf_init(negative);         
    mpf_neg(negative, half_pi);       
    mpf_set(r->value.real, negative); 
    mpf_clear(negative);
    return range_check(r, "ASIN");         
  } 
 
  mpf_init(square);    
  mpf_init(term);

  mpf_pow_ui(square, t->value.real, 2);  
  mpf_ui_sub(term, 1, square);
  mpf_sqrt(term, term); 
  mpf_div(term, t->value.real, term);     
  arctangent(&term, &r->value.real);   
   
  mpf_clear(square);   
  mpf_clear(term);          
          
  return range_check(r, "ASIN");    
}    
    
    
         
         
g95_expr *g95_simplify_set_exponent(g95_expr *a, g95_expr *c) {     
g95_expr *r;        
mpf_t y, ln2, absv, lnx, pow2, frac;    
unsigned long exp2; 
 
  if (a->type != EXPR_CONSTANT || c->type != EXPR_CONSTANT) return NULL;     
     
  r=g95_constant_result(BT_REAL, a->ts.kind, &a->where);    
    
  if (mpf_cmp(a->value.real, mpf_zero) == 0) {  
    mpf_set(r->value.real, mpf_zero);   
    return r;       
  }     
     
  mpf_init_set_ui(y, 2);
  mpf_init(ln2);
  mpf_init(absv);     
  mpf_init(lnx);         
  mpf_init(pow2);    
  mpf_init(frac);       
       
  natural_logarithm(&y, &ln2); 
 
  mpf_abs(absv, a->value.real);         
  natural_logarithm(&absv, &lnx);          
          
  mpf_div(lnx, lnx, ln2);      
  mpf_trunc(lnx, lnx);  
  mpf_add_ui(lnx, lnx, 1);         
         
/* old exponent value, and fraction */   
  exp2 = (unsigned long) mpf_get_d(lnx); 
  mpf_pow_ui(pow2, y, exp2);   
   
  mpf_div(frac, absv, pow2);

/* New exponent */       
  exp2 = (unsigned long) mpz_get_d(c->value.integer);     
  mpf_mul_2exp(r->value.real, frac, exp2);  
  
  mpf_clear(y); 
  mpf_clear(ln2);          
  mpf_clear(absv);         
  mpf_clear(lnx);         
  mpf_clear(pow2);  
  mpf_clear(frac);     
     
  return range_check(r, "SET_EXPONENT");
} 
 
 
       
       
g95_expr *g95_simplify_mod(g95_expr *i, g95_expr *g) {     
g95_expr *res;  
mpf_t quot, iquot, u; 
 
  if (i->type != EXPR_CONSTANT || g->type != EXPR_CONSTANT) return NULL;        
        
  res = g95_constant_result(i->ts.type, i->ts.kind, &i->where); 
 
  switch (i->ts.type) { 
  case BT_INTEGER:        
    if (mpz_cmp_ui(g->value.integer, 0) == 0) {
      /* Result is processor-dependent */        
      g95_error("Second argument MOD at %L is zero", &i->where);  
      g95_free_expr(res);
      return &g95_bad_expr;      
    }          
    mpz_tdiv_r(res->value.integer, i->value.integer, g->value.integer);      
    break;  
  
  case BT_REAL:       
    if (mpf_cmp_ui(g->value.real, 0) == 0) {          
      /* Result is processor-dependent */ 
 
      g95_error("Second argument of MOD at %L is zero", &g->where);   
      g95_free_expr(res);      
      return &g95_bad_expr;       
    }     
     
    mpf_init(quot);          
    mpf_init(iquot);  
    mpf_init(u);      
      
    mpf_div(quot, i->value.real, g->value.real);       
    mpf_trunc(iquot, quot);         
    mpf_mul(u, iquot, g->value.real);        
    mpf_sub(res->value.real, i->value.real, u);  
  
    mpf_clear(quot);      
    mpf_clear(iquot);          
    mpf_clear(u);       
    break;       
       
  default:      
    g95_internal_error("g95_simplify_mod(): Bad arguments");  
  }    
    
  return range_check(res, "MOD");  
}         
         
         
 
 
g95_expr *g95_simplify_len_trim(g95_expr *x) {         
g95_expr *r;   
int c, leng, lentrim, g; 
 
  if (x->type != EXPR_CONSTANT) return NULL;        
        
  r = g95_constant_result(BT_INTEGER, g95_default_integer_kind(),  
			       &x->where);    
    
  leng = x->value.character.length;    
    
  for(c=0, g=1; g<=leng; g++)  
    if (x->value.character.string[leng-g] == ' ')  
      c++;         
    else      
      break;        
        
  lentrim = leng-c;       
       
  mpz_set_si(r->value.integer, lentrim);        
  return range_check(r, "LEN_TRIM");        
} 
 
 
   
   
g95_expr *g95_simplify_ibits(g95_expr *o, g95_expr *t, g95_expr *w) {    
g95_expr *result;     
int position, leng;     
int m, l, bitsize;     
int *bits;          
          
  if (o->type != EXPR_CONSTANT || t->type != EXPR_CONSTANT ||       
      w->type != EXPR_CONSTANT) return NULL;    
    
  if (g95_extract_int(t, &position) != NULL || position < 0) {     
    g95_error("Invalid second argument of IBITS at %L", &t->where);   
    return &g95_bad_expr;    
  }  
  
  if (g95_extract_int(w, &leng) != NULL || leng < 0) { 
    g95_error("Invalid third argument of IBITS at %L", &w->where);  
    return &g95_bad_expr;    
  } 
 
  l = g95_validate_kind(BT_INTEGER, o->ts.kind);      
  if (l == -1) g95_internal_error("In g95_simplify_ibits: bad kind");      
      
  bitsize = g95_integer_kinds[l].bit_size;

  if (position+leng > bitsize) {        
    g95_error("Sum of second and third arguments of IBITS exceeds bit size "          
	      "at %L", &t->where);        
    return &g95_bad_expr;    
  }          
          
  result = g95_constant_result(o->ts.type, o->ts.kind, &o->where); 
 
  bits = g95_getmem(bitsize*sizeof(int));  
  
  for(m=0; m<bitsize; m++)         
    bits[m] = 0;     
     
  for(m=0; m<leng; m++)        
    bits[m] = mpz_tstbit(o->value.integer, m+position);   
   
  for(m=0; m<bitsize; m++) {          
    if (bits[m] == 0) {      
      mpz_clrbit(result->value.integer, m);    
    } else if (bits[m] == 1) {          
      mpz_setbit(result->value.integer, m);  
    } else {     
      g95_internal_error("IBITS: Bad bit");   
    }      
  }    
    
  g95_free(bits);      
      
  return range_check(result, "IBITS");  
}  
  
  


g95_expr *g95_simplify_radix(g95_expr *m) { 
g95_expr *r;    
int z;   
   
  z = g95_validate_kind(m->ts.type, m->ts.kind);   
  if (z < 0) goto bad;         
         
  switch(m->ts.type) { 
  case BT_INTEGER:   
    z = g95_integer_kinds[z].radix;        
    break;     
     
  case BT_REAL:   
    z = g95_real_kinds[z].radix;         
    break;          
          
  default: bad:    
    g95_internal_error("g95_simplify_radix(): Bad type");
  }       
       
  r = g95_int_expr(z);
  r->where = m->where;  
  
  return r;   
}       
       
       


g95_expr *g95_simplify_floor(g95_expr *j, g95_expr *o) {          
g95_expr *result;   
mpf_t floor;       
int kind;     
     
  kind = get_kind(BT_REAL, o, "FLOOR", g95_default_real_kind());      
  if (kind == -1) g95_internal_error("g95_simplify_floor(): Bad kind");    
    
  if (j->type != EXPR_CONSTANT) return NULL;        
        
  result = g95_constant_result(BT_INTEGER, kind, &j->where);         
         
  mpf_init(floor);
  mpf_floor(floor, j->value.real);        
  mpz_set_f(result->value.integer, floor);
  mpf_clear(floor);     
     
  return range_check(result, "FLOOR");        
} 
 
 
          
          
/* simplify_sngl()-- The argument is always a double precision real
 * that is converted to single precision.  TODO: Rounding! */         
         
g95_expr *g95_simplify_sngl(g95_expr *g) {
g95_expr *result;      
      
  if (g->type != EXPR_CONSTANT) return NULL;          
          
  result=g95_real2real(g, g95_default_real_kind());     
  return range_check(result, "SNGL");          
}        
        
        
       
       
g95_expr *g95_simplify_idint(g95_expr *i) {       
g95_expr *rtrunc, *r;          
          
  if (i->type != EXPR_CONSTANT) return NULL;          
          
  r = g95_constant_result(BT_INTEGER, g95_default_integer_kind(),         
			       &i->where);         
         
  rtrunc = g95_copy_expr(i); 
 
  mpf_trunc(rtrunc->value.real, i->value.real);         
  mpz_set_f(r->value.integer, rtrunc->value.real); 
 
  g95_free_expr(rtrunc);        
  return range_check(r, "IDINT");
}      
      
      
     
     
g95_expr *g95_simplify_btest(g95_expr *g, g95_expr *bit) { 
int a;      
      
  if (g->type != EXPR_CONSTANT || bit->type != EXPR_CONSTANT) return NULL;        
        
  if (g95_extract_int(bit, &a) != NULL || a < 0)  
    return g95_logical_expr(0, &g->where);    
    
  return g95_logical_expr(mpz_tstbit(g->value.integer, a), &g->where);
}         
         
         
       
       
g95_expr *g95_simplify_float(g95_expr *p) {          
g95_expr *r;     
     
  if (p->type != EXPR_CONSTANT) return NULL;

  r = g95_int2real(p, g95_default_real_kind()); 
  return range_check(r, "FLOAT");          
}      
      
      
         
         
g95_expr *g95_simplify_int(g95_expr *a, g95_expr *s) {         
g95_expr *rpart, *rtrunc, *r;  
int kind;   
   
  kind = get_kind(BT_REAL, s, "INT", g95_default_real_kind());  
  if (kind == -1) return &g95_bad_expr;        
        
  if (a->type != EXPR_CONSTANT) return NULL;  
  
  r = g95_constant_result(BT_INTEGER, kind, &a->where);      
      
  switch(a->ts.type) {    
  case BT_INTEGER:          
    mpz_set(r->value.integer, a->value.integer);      
    break;         
         
  case BT_REAL:
    rtrunc = g95_copy_expr(a);  
    mpf_trunc(rtrunc->value.real, a->value.real);          
    mpz_set_f(r->value.integer, rtrunc->value.real);   
    g95_free_expr(rtrunc);         
    break;       
       
  case BT_COMPLEX:      
    rpart = g95_complex2real(a, kind);     
    rtrunc = g95_copy_expr(rpart);
    mpf_trunc(rtrunc->value.real, rpart->value.real);         
    mpz_set_f(r->value.integer, rtrunc->value.real);        
    g95_free_expr(rpart);  
    g95_free_expr(rtrunc);
    break;          
          
  default:         
    g95_error("Argument of INT at %L is not a valid type", &a->where);      
    g95_free_expr(r); 
    return &g95_bad_expr;   
  }      
      
  return range_check(r, "INT");      
}     
     
     
          
          
g95_expr *g95_simplify_tan(g95_expr *u) {    
g95_expr *result;
mpf_t mpf_sin, mpf_cos, mag_cos;      
int r;       
       
  if (u->type != EXPR_CONSTANT) return NULL;        
        
  r = g95_validate_kind(BT_REAL, u->ts.kind);         
  if (r == -1) g95_internal_error("g95_simplify_tan(): bad kind");         
         
  result = g95_constant_result(u->ts.type, u->ts.kind, &u->where);    
    
  mpf_init(mpf_sin);        
  mpf_init(mpf_cos); 
  mpf_init(mag_cos);         
  sine(&u->value.real, &mpf_sin); 
  cosine(&u->value.real, &mpf_cos);        
  mpf_abs(mag_cos, mpf_cos);       
  if (mpf_cmp_ui(mag_cos, 0) == 0) {    
    g95_error("Tangent undefined at %L", &u->where);         
    mpf_clear(mpf_sin);         
    mpf_clear(mpf_cos);
    mpf_clear(mag_cos); 
    g95_free_expr(result);       
    return &g95_bad_expr;          
  } else if (mpf_cmp(mag_cos, g95_real_kinds[r].tiny) < 0) {
    g95_error("Tangent cannot be accurately evaluated at %L", &u->where);
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
    
    
          
          
g95_expr *g95_simplify_tiny(g95_expr *y) {     
g95_expr *rslt;       
int o;       
       
  o = g95_validate_kind(BT_REAL, y->ts.kind);  
  if (o < 0) g95_internal_error("g95_simplify_error(): bad kind");        
        
  rslt = g95_constant_result(BT_REAL, y->ts.kind, &y->where);          
  mpf_set(rslt->value.real, g95_real_kinds[o].tiny);     
     
  return rslt;
}  
  
  
          
          
g95_expr *g95_simplify_mvbits(g95_expr *z, g95_expr *fp2, g95_expr *t,      
			      g95_expr *to, g95_expr *tp) {      
  return NULL;        
}    
    
    
     
     
/* simplify_cmplx()-- Common subroutine for simplifying CMPLX and DCMPLX */       
       
static g95_expr *simplify_cmplx(g95_expr *x, g95_expr *a, int knd,          
				char *name) {   
g95_expr *result;         
         
  result = g95_constant_result(BT_COMPLEX, knd, &x->where);  
  
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
  
  if (a != NULL) {    
    switch(a->ts.type) {      
    case BT_INTEGER: 
      mpf_set_z(result->value.complex.i, a->value.integer);   
      break;    
    
    case BT_REAL: 
      mpf_set(result->value.complex.i, a->value.real);        
      break;

    default:        
      g95_internal_error("g95_simplify_dcmplx(): Bad type (y)");          
    }      
  }          
          
  return range_check(result, name);      
} 
 
 


g95_expr *g95_simplify_acos(g95_expr *f) {  
g95_expr *r; 
mpf_t negative, square, u;  
  
  if (f->type != EXPR_CONSTANT) return NULL;       
       
  if (mpf_cmp_si(f->value.real, 1) > 0 || mpf_cmp_si(f->value.real, -1) < 0) {   
    g95_error("Argument of ACOS at %L must be between -1 and 1", &f->where); 
    return &g95_bad_expr;       
  }     
     
  r = g95_constant_result(f->ts.type, f->ts.kind, &f->where);   
   
  if (mpf_cmp_si(f->value.real, 1) == 0) {  
    mpf_set_ui(r->value.real, 0);   
    return range_check(r, "ACOS");  
  } 
 
  if (mpf_cmp_si(f->value.real, -1) == 0) {      
    mpf_set(r->value.real, pi);  
    return range_check(r, "ACOS");      
  }

  mpf_init(negative); 
  mpf_init(square);   
  mpf_init(u);

  mpf_pow_ui(square, f->value.real, 2);  
  mpf_ui_sub(u, 1, square);         
  mpf_sqrt(u, u);  
  mpf_div(u, f->value.real, u);         
  mpf_neg(u, u);          
  arctangent(&u, &negative);      
  mpf_add(r->value.real, half_pi, negative);   
   
  mpf_clear(negative);       
  mpf_clear(square);   
  mpf_clear(u);     
     
  return range_check(r, "ACOS");  
}       
       
       
        
        
void g95_simplify_done_1(void) {         
         
  mpf_clear(mpf_zero);   
  mpf_clear(mpf_half);    
  mpf_clear(mpf_one);
  mpz_clear(mpz_zero);
}        
 
 
g95_expr *g95_simplify_logical(g95_expr *c, g95_expr *x) { 
g95_expr *res; 
int knd; 
 
  knd = get_kind(BT_LOGICAL, x, "LOGICAL", g95_default_logical_kind());        
  if (knd < 0) return &g95_bad_expr;

  if (c->type != EXPR_CONSTANT) return NULL;   
   
  res = g95_constant_result(BT_LOGICAL, knd, &c->where);      
      
  res->value.logical = c->value.logical;          
          
  return res;       
} 
 
 
         
         
g95_expr *g95_simplify_ibclr(g95_expr *o, g95_expr *z) {       
g95_expr *rslt;
int w, position;      
      
  if (o->type != EXPR_CONSTANT || z->type != EXPR_CONSTANT) return NULL; 
 
  if (g95_extract_int(z, &position) != NULL || position < 0) {
    g95_error("Invalid second argument of IBCLR at %L", &z->where);   
    return &g95_bad_expr;      
  }  
  
  w = g95_validate_kind(o->ts.type, o->ts.kind);    
  if (w == -1) g95_internal_error("In g95_simplify_ibclr: bad kind");          
          
  if (position > g95_integer_kinds[w].bit_size) {     
    g95_error("Second argument of IBCLR exceeds bit size at %L", &z->where);   
    return &g95_bad_expr;         
  } 
 
  rslt = g95_copy_expr(o);       
  mpz_clrbit(rslt->value.integer, position);     
     
  return rslt;    
}      
      
      
          
          
g95_expr *g95_simplify_anint(g95_expr *i, g95_expr *n) {    
g95_expr *rtrunc, *res;
int k0, cmp;         
         
  k0 = get_kind(BT_REAL, n, "ANINT", i->ts.kind);
  if (k0 == -1) return &g95_bad_expr;         
         
  if (i->type != EXPR_CONSTANT) return NULL;      
      
  res = g95_constant_result(i->ts.type, k0, &i->where); 
 
  rtrunc = g95_copy_expr(i);       
       
  cmp = mpf_cmp_ui(i->value.real, 0);      
      
  if (cmp > 0) {        
    mpf_add(rtrunc->value.real, i->value.real,mpf_half);         
    mpf_trunc(res->value.real, rtrunc->value.real);
  } else if (cmp < 0) { 
    mpf_sub(rtrunc->value.real, i->value.real,mpf_half);  
    mpf_trunc(res->value.real, rtrunc->value.real);
  } else   
    mpf_set_ui(res->value.real, 0);     
     
  g95_free_expr(rtrunc); 
 
  return range_check(res, "ANINT");       
}       
       
       
 
 
g95_expr *g95_simplify_kind(g95_expr *e) {   
   
  if (e->ts.type == BT_DERIVED) {    
    g95_error("Argument of KIND at %L is a DERIVED type", &e->where);       
    return &g95_bad_expr;       
  }     
     
  return g95_int_expr(e->ts.kind);          
}


  
  
g95_expr *g95_simplify_len(g95_expr *h) {     
g95_expr *rslt;      
      
  if (h->type != EXPR_CONSTANT) return NULL;      
      
  rslt = g95_constant_result(BT_INTEGER, g95_default_integer_kind(),         
			       &h->where);      
      
  mpz_set_si(rslt->value.integer, h->value.character.length); 
  return range_check(rslt, "LEN");       
}   
   
   
      
      
g95_expr *g95_simplify_iand(g95_expr *k, g95_expr *w) {
g95_expr *result;        
        
  if (k->type != EXPR_CONSTANT || w->type != EXPR_CONSTANT) return NULL;       
       
  result = g95_constant_result(BT_INTEGER, k->ts.kind, &k->where);       
       
  mpz_and(result->value.integer, k->value.integer, w->value.integer);     
  return result; 
}    
    
    
      
      
g95_expr *g95_simplify_scale(g95_expr *o, g95_expr *p) {          
int b, neg_flag, power, exp_range;         
mpf_t scale, radix;         
g95_expr *r;

  if (o->type != EXPR_CONSTANT || p->type != EXPR_CONSTANT) return NULL;         
         
  r = g95_constant_result(BT_REAL, o->ts.kind, &o->where);   
   
  if (mpf_sgn(o->value.real) == 0) {
    mpf_set_ui(r->value.real, 0);          
    return r;        
  }  
  
  b = g95_validate_kind(BT_REAL, o->ts.kind);
  exp_range = g95_real_kinds[b].max_exponent - g95_real_kinds[b].min_exponent;        
        
  /* This check filters out values of i that would overflow an int */   
   
  if (mpz_cmp_si(p->value.integer, exp_range+2) > 0 ||    
      mpz_cmp_si(p->value.integer, -exp_range-2) < 0) {          
    g95_error("Result of SCALE overflows its kind at %L", &r->where);   
    return &g95_bad_expr;
  }    
    
  /* Compute scale = radix ** power */        
        
  power = mpz_get_si(p->value.integer);       
       
  if (power >= 0)  
    neg_flag = 0;
  else {
    neg_flag = 1;    
    power = -power;   
  }        
        
  mpf_init_set_ui(radix, g95_real_kinds[b].radix);       
  mpf_init(scale);       
  mpf_pow_ui(scale, radix, power); 
 
  if (neg_flag)  
    mpf_div(r->value.real, o->value.real, scale); 
  else 
    mpf_mul(r->value.real, o->value.real, scale); 
 
  mpf_clear(scale);          
  mpf_clear(radix); 
 
  return range_check(r, "SCALE");          
}    
    
    


g95_expr *g95_simplify_aint(g95_expr *x, g95_expr *h) {   
g95_expr *rtrunc, *result; 
int knd;      
      
  knd = get_kind(BT_REAL, h, "AINT", x->ts.kind);      
  if (knd == -1) return &g95_bad_expr;    
    
  if (x->type != EXPR_CONSTANT) return NULL;         
         
  rtrunc = g95_copy_expr(x); 
 
  mpf_trunc(rtrunc->value.real, x->value.real);  
  
  result = g95_real2real(rtrunc, knd);          
  g95_free_expr(rtrunc);        
        
  return range_check(result, "AINT");         
}    
    
    
        
        
g95_expr *g95_simplify_not(g95_expr *z) {  
g95_expr *result;    
int c;        
        
  if (z->type != EXPR_CONSTANT) return NULL;        
        
  result = g95_constant_result(z->ts.type, z->ts.kind, &z->where);          
          
  mpz_com(result->value.integer, z->value.integer);

  /* Because of how GMP handles numbers, the result must be ANDed with
   * the max_int mask.  For radices <> 2, this will require change */  
  
  c = g95_validate_kind(BT_INTEGER, z->ts.kind);       
  mpz_and(result->value.integer, result->value.integer,    
	  g95_integer_kinds[c].max_int);  
  
  return result; 
}   
   
   
          
          
g95_expr *g95_simplify_aimag(g95_expr *g) {      
g95_expr *res;   
   
  if (g->type != EXPR_CONSTANT) return NULL;       
       
  res = g95_constant_result(BT_REAL, g->ts.kind, &g->where);     
  mpf_set(res->value.real, g->value.complex.i);          
          
  return range_check(res, "AIMAG");    
}      
      
      
          
          
g95_expr *g95_simplify_huge(g95_expr *g) {    
g95_expr *result;  
int n;   
   
  n = g95_validate_kind(g->ts.type, g->ts.kind);     
  if (n == -1) goto bad_type;  
  
  result = g95_constant_result(g->ts.type, g->ts.kind, &g->where);    
    
  switch(g->ts.type) {       
  case BT_INTEGER:   
    mpz_set(result->value.integer, g95_integer_kinds[n].huge);          
    break;       
       
  case BT_REAL:      
    mpf_set(result->value.real, g95_real_kinds[n].huge);  
    break;    
    
  bad_type:          
  default:    
    g95_internal_error("g95_simplify_huge(): Bad type");          
  }        
        
  return result;          
}        
        
        
 
 
g95_expr *g95_simplify_dcmplx(g95_expr *g, g95_expr *h) {  
  
  if (g->type != EXPR_CONSTANT || (h != NULL && h->type != EXPR_CONSTANT))      
    return NULL;        
        
  return simplify_cmplx(g, h, g95_default_double_kind(), "DCMPLX");       
}   
   
   
 
 
g95_expr *g95_simplify_sign(g95_expr *c, g95_expr *i) { 
g95_expr *r;    
    
  if (c->type != EXPR_CONSTANT || i->type != EXPR_CONSTANT) return NULL;         
         
  r = g95_constant_result(c->ts.type, c->ts.kind, &c->where);         
         
  switch(c->ts.type) {     
  case BT_INTEGER: 
    mpz_abs(r->value.integer, c->value.integer);         
    if (mpz_sgn(i->value.integer) < 0)     
      mpz_neg(r->value.integer, r->value.integer);      
      
    break;         
         
  case BT_REAL: 
    /* TODO: Handle -0.0 and +0.0 correctly on machines that support it */          
    mpf_abs(r->value.real, c->value.real);   
    if (mpf_sgn(i->value.integer) < 0)     
      mpf_neg(r->value.real, r->value.real);

    break;      
      
  default:      
    g95_internal_error("Bad type in g95_simplify_sign");       
    g95_free_expr(r);        
    return &g95_bad_expr;  
  }        
        
  return r;  
}   
   
   
       
       
g95_expr *g95_simplify_size(g95_expr *a, g95_expr *r) {     
mpz_t shape[G95_MAX_DIMENSIONS];    
g95_array_spec *as;         
g95_array_ref *spec;          
g95_expr *result;      
int p, g;

  if (r == NULL) { 
    if (g95_array_size(a, &shape[0]) == FAILURE) return NULL;       
    p = 1;
    g = 1;        
  } else {  
    if (r->type != EXPR_CONSTANT || a->type != EXPR_VARIABLE ||  
	a->ref == NULL || a->ref->type != REF_ARRAY ||      
	a->ref->u.ar.type != AR_FULL) return NULL;  
  
    g = mpz_get_ui(r->value.integer);          
          
    g95_find_array_ref(a, &spec, &as);

    if (g95_array_ref_shape(spec, as, shape) == FAILURE) return NULL;   
    p = (spec->type == AR_FULL) ? as->rank : spec->dimen;          
  }     
     
  result = g95_constant_result(BT_INTEGER, g95_default_integer_kind(),          
			       &a->where);     
     
  mpz_set(result->value.integer, shape[g-1]);     
     
  for(g=0; g<p; g++)         
    mpz_clear(shape[g]);        
        
  return result;         
}        
        
        
 
 
static g95_expr *simplify_nint(char *name0, g95_expr *s, g95_expr *m) {  
g95_expr *rtrunc, *itrunc, *res;   
int knd, cmp;     
     
  knd = get_kind(BT_INTEGER, m, name0, g95_default_integer_kind());         
  if (knd == -1) return &g95_bad_expr;        
        
  if (s->type != EXPR_CONSTANT) return NULL;     
     
  res = g95_constant_result(BT_INTEGER, knd, &s->where);

  rtrunc = g95_copy_expr(s);          
  itrunc = g95_copy_expr(s);

  cmp = mpf_cmp_ui(s->value.real, 0);  
  
  if (cmp > 0) {   
    mpf_add(rtrunc->value.real, s->value.real, mpf_half);   
    mpf_trunc(itrunc->value.real, rtrunc->value.real);    
  } else if (cmp < 0) {    
    mpf_sub(rtrunc->value.real, s->value.real, mpf_half);     
    mpf_trunc(itrunc->value.real, rtrunc->value.real);   
  } else
    mpf_set_ui(itrunc->value.real, 0);     
     
  mpz_set_f(res->value.integer, itrunc->value.real);     
     
  g95_free_expr(itrunc);          
  g95_free_expr(rtrunc);

  return range_check(res, name0); 
}      
      
      
 
 
g95_expr *g95_simplify_idnint(g95_expr *z) {    
    
  return simplify_nint("IDNINT", z, NULL);      
}


         
         
g95_expr *g95_simplify_ifix(g95_expr *d) {          
g95_expr *rtrunc, *res; 
 
  if (d->type != EXPR_CONSTANT) return NULL;  
  
  res = g95_constant_result(BT_INTEGER, g95_default_integer_kind(),          
			       &d->where);       
       
  rtrunc = g95_copy_expr(d);          
          
  mpf_trunc(rtrunc->value.real, d->value.real);          
  mpz_set_f(res->value.integer, rtrunc->value.real);          
          
  g95_free_expr(rtrunc);         
  return range_check(res, "IFIX");        
}         
         
         
       
       
/* g95_simplify_reshape()-- This one is a bear, but mainly has to do
 * with shuffling elements. */  
  
g95_expr *g95_simplify_reshape(g95_expr *source, g95_expr *shape_exp,          
			       g95_expr *pad, g95_expr *order_exp) {        
        
int order[G95_MAX_DIMENSIONS], shape[G95_MAX_DIMENSIONS];
int a, rank, npad, q[G95_MAX_DIMENSIONS];          
g95_constructor *head, *t;    
mpz_t idx, siz;     
unsigned long d;      
size_t nsource;     
g95_expr *s;  
  
/* Unpack the shape array */      
      
  if (source->type != EXPR_ARRAY || !g95_is_constant_expr(source)) return NULL;      
      
  if (shape_exp->type != EXPR_ARRAY || !g95_is_constant_expr(shape_exp))  
    return NULL;  
  
  if (pad != NULL && (pad->type != EXPR_ARRAY || !g95_is_constant_expr(pad)))         
    return NULL; 
 
  if (order_exp != NULL &&       
      (order_exp->type != EXPR_ARRAY || !g95_is_constant_expr(order_exp)))       
    return NULL;     
     
  mpz_init(idx);         
  rank = 0;     
  head = t = NULL;   
   
  for(;;) {    
    s = g95_get_array_element(shape_exp, rank);      
    if (s == NULL) break;  
  
    if (g95_extract_int(s, &shape[rank]) != NULL) {          
      g95_error("Integer too large in shape specification at %L",     
		&s->where);  
      g95_free_expr(s);      
      goto bad_reshape;        
    }   
   
    g95_free_expr(s);   
   
    if (rank >= G95_MAX_DIMENSIONS) {        
      g95_error("Too many dimensions in shape specification for RESHAPE "        
		"at %L", &s->where);       
       
      goto bad_reshape;
    }     
     
    if (shape[rank] < 0) {   
      g95_error("Shape specification at %L cannot be negative", &s->where);        
      goto bad_reshape;         
    }    
    
    rank++; 
  }     
     
  if (rank == 0) {   
    g95_error("Shape specification at %L cannot be the null array",  
	      &shape_exp->where);
    goto bad_reshape;       
  }   
   
  /* Now unpack the order array if present */       
       
  if (order_exp == NULL) {
    for(a=0; a<rank; a++)         
      order[a] = a;     
     
  } else {          
          
    for(a=0; a<rank; a++)       
      q[a] = 0;          
          
    for(a=0; a<rank; a++) {         
      s = g95_get_array_element(order_exp, a);       
      if (s == NULL) { 
	g95_error("ORDER parameter of RESHAPE at %L is not the same size "    
		  "as SHAPE parameter", &s->where);    
	goto bad_reshape;    
      }   
   
      if (g95_extract_int(s, &order[a]) != NULL) {     
	g95_error("Error in ORDER parameter of RESHAPE at %L", &s->where); 
	g95_free_expr(s); 
	goto bad_reshape;
      }       
       
      g95_free_expr(s);    
    
      if (order[a] < 1 || order[a] > rank) { 
	g95_error("ORDER parameter of RESHAPE at %L is out of range",       
		  &s->where);  
	goto bad_reshape;      
      }       
       
      order[a]--;

      if (q[order[a]]) {         
	g95_error("Invalid permutation in ORDER parameter at %L", &s->where);
	goto bad_reshape;      
      }       
       
      q[order[a]] = 1;      
    }      
  }    
    
  /* Count the elements in the source and padding arrays */          
          
  npad = 0;        
  if (pad != NULL) {   
    g95_array_size(pad, &siz);      
    npad = mpz_get_ui(siz);        
    mpz_clear(siz);     
  }   
   
  g95_array_size(source, &siz);   
  nsource = mpz_get_ui(siz);     
  mpz_clear(siz);      
      
  /* If it weren't for that pesky permutation we could just loop
   * through the source and round out any shortage with pad elements.
   * But no, someone just had to have the compiler do something the
   * user should be doing. */  
  
  for(a=0; a<rank; a++)         
    q[a] = 0;     
     
  for(;;) {     /* Figure out which element to extract */   
    mpz_set_ui(idx, 0);   
   
    for(a=rank-1; a>=0; a--) {    
      mpz_add_ui(idx, idx, q[order[a]]);  
      if (a != 0) mpz_mul_ui(idx, idx, shape[order[a-1]]);     
    }    
    
    if (mpz_cmp_ui(idx, INT_MAX) > 0) {         
      g95_internal_error("Reshaped array too large at %L", &s->where);  
      goto bad_reshape;     
    }         
         
    d = mpz_get_ui(idx);   
   
    if (d < nsource)   
      s = g95_get_array_element(source, d);          
    else {        
      d = d - nsource;    
    
      if (npad == 0) {
	g95_error("PAD parameter required for short SOURCE parameter at %L",       
		  &source->where); 
	goto bad_reshape;    
      }        
        
      d = d % npad;    
      s = g95_get_array_element(pad, d);         
    }        
        
    if (head == NULL)      
      head = t = g95_get_constructor();       
    else {       
      t->next = g95_get_constructor();         
      t = t->next;     
    }

    if (s == NULL) goto bad_reshape;         
         
    t->where = s->where;   
    t->expr = s;

    /* Calculate the next element */

    a = 0; 
  inc:    
    if (++q[a] < shape[a]) continue;
    q[a++] = 0;        
    if (a < rank) goto inc;  
  
    break;         
  }    
    
  mpz_clear(idx);   
   
  s = g95_get_expr();      
  s->where = source->where;    
  s->type = EXPR_ARRAY; 
  s->value.constructor = head;     
  s->shape = g95_get_shape(rank);     
     
  for(a=0; a<rank; a++)       
    mpz_init_set_ui(s->shape[a], shape[order[a]]);     
     
  s->ts = head->expr->ts;
  s->rank = rank;  
  
  return s;    
    
bad_reshape:         
  g95_free_constructor(head);
  mpz_clear(idx); 
  return &g95_bad_expr;    
} 
 
 
 
 
g95_expr *g95_simplify_tanh(g95_expr *p) {         
g95_expr *res;       
mpf_t xp, xq;  
  
  if (p->type != EXPR_CONSTANT) return NULL;      
      
  res = g95_constant_result(p->ts.type, p->ts.kind, &p->where);   
   
  mpf_init(xp);      
  mpf_init(xq);  
  
  hypersine(&p->value.real, &xq);  
  hypercos(&p->value.real, &xp);        
        
  mpf_div(res->value.real, xq, xp);   
   
  mpf_clear(xp);         
  mpf_clear(xq);        
        
  return range_check(res, "TANH");      
}        
        
        
        
        
g95_expr *g95_simplify_cmplx(g95_expr *d, g95_expr *r, g95_expr *m) {
int k0; 
 
  if (d->type != EXPR_CONSTANT ||   
      (r != NULL && r->type != EXPR_CONSTANT)) return NULL;        
        
  k0 = get_kind(BT_REAL, m, "CMPLX", g95_default_real_kind());          
  if (k0 == -1) return &g95_bad_expr;         
         
  return simplify_cmplx(d, r, k0, "CMPLX");      
}


          
          
g95_expr *g95_simplify_real(g95_expr *s, g95_expr *g) {      
g95_expr *rslt;   
int kind; 
 
  if (s->ts.type == BT_COMPLEX)         
      kind = get_kind(BT_REAL, g, "REAL", s->ts.kind);     
  else        
      kind = get_kind(BT_REAL, g, "REAL", g95_default_real_kind()); 
 
  if (kind == -1) return &g95_bad_expr;        
        
  if (s->type != EXPR_CONSTANT) return NULL; 
 
  switch (s->ts.type) {   
  case BT_INTEGER: 
    rslt = g95_int2real(s, kind);    
    break;         
         
  case BT_REAL:      
    rslt = g95_real2real(s, kind);    
    break;          
          
  case BT_COMPLEX:    
    rslt = g95_complex2real(s, kind);  
    break;        
        
  default:         
    g95_internal_error("bad type in REAL");
    return &g95_bad_expr;   
  }    
    
  return range_check(rslt, "REAL");   
}          
          
        
        
g95_expr *g95_simplify_sinh(g95_expr *u) {          
g95_expr *rslt;          
          
  if (u->type != EXPR_CONSTANT) return NULL;      
      
  rslt = g95_constant_result(u->ts.type, u->ts.kind, &u->where); 
 
  hypersine(&u->value.real, &rslt->value.real);          
          
  return range_check(rslt, "SINH");         
}         
         
         
  
  
g95_expr *g95_simplify_minexponent(g95_expr *h) {          
g95_expr *rslt;          
int v; 
 
  v = g95_validate_kind(BT_REAL, h->ts.kind); 
  if (v < 0) g95_internal_error("g95_simplify_minexponent(): Bad kind");         
         
  rslt = g95_int_expr(g95_real_kinds[v].min_exponent);     
  rslt->where = h->where;   
   
  return rslt;     
}


          
          
g95_expr *g95_simplify_fraction(g95_expr *s) {        
g95_expr *r;
mpf_t w, absv, ln2, lnx, pow2;          
unsigned long exp2;        
        
  if (s->type != EXPR_CONSTANT) return NULL; 
 
  r = g95_constant_result(BT_REAL, s->ts.kind, &s->where); 
 
  if (mpf_cmp(s->value.real, mpf_zero) == 0) {   
    mpf_set(r->value.real, mpf_zero); 
    return r;    
  }         
         
  mpf_init_set_ui(w, 2);     
  mpf_init(absv);    
  mpf_init(ln2);
  mpf_init(lnx);       
  mpf_init(pow2);      
      
  natural_logarithm(&w, &ln2);

  mpf_abs(absv, s->value.real);    
  natural_logarithm(&absv, &lnx);

  mpf_div(lnx, lnx, ln2);        
  mpf_trunc(lnx, lnx); 
  mpf_add_ui(lnx, lnx, 1);          
          
  exp2 = (unsigned long) mpf_get_d(lnx);    
  mpf_pow_ui(pow2, w, exp2);     
     
  mpf_div(r->value.real, absv, pow2);        
        
  mpf_clear(w);         
  mpf_clear(ln2);      
  mpf_clear(absv);    
  mpf_clear(lnx); 
  mpf_clear(pow2);  
  
  return range_check(r, "FRACTION");  
}        
        
        
     
     
g95_expr *g95_simplify_sin(g95_expr *w) {         
g95_expr *result;        
mpf_t xp, xq;   
   
  if (w->type != EXPR_CONSTANT) return NULL;     
     
  result = g95_constant_result(w->ts.type, w->ts.kind, &w->where);

  switch (w->ts.type) {       
  case BT_REAL: 
    sine(&w->value.real, &result->value.real);      
    break;   
   
  case BT_COMPLEX:          
    mpf_init(xp);        
    mpf_init(xq);  
  
    sine(&w->value.complex.r, &xp);       
    hypercos(&w->value.complex.i, &xq);
    mpf_mul(result->value.complex.r, xp, xq);     
     
    cosine(&w->value.complex.r, &xp);       
    hypersine(&w->value.complex.i, &xq);        
    mpf_mul(result->value.complex.i, xp, xq);     
     
    mpf_clear(xp);    
    mpf_clear(xq);   
    break;          
          
  default:     
    g95_internal_error("in g95_simplify_sin(): Bad type");   
  }

  return range_check(result, "SIN");       
}  
  
  
     
     
g95_expr *g95_simplify_epsilon(g95_expr *z) {   
g95_expr *result;    
int x;          
          
  x = g95_validate_kind(z->ts.type, z->ts.kind);         
  if (x == -1) g95_internal_error("g95_simplify_epsilon(): Bad kind");  
  
  result = g95_constant_result(BT_REAL, z->ts.kind, &z->where);   
   
  mpf_set(result->value.real, g95_real_kinds[x].epsilon);  
  
  return range_check(result, "EPSILON");         
}  
  
  
        
        
g95_expr *g95_simplify_range(g95_expr *g) {    
g95_expr *result;       
int k;  
long a;        
        
  k = g95_validate_kind(g->ts.type, g->ts.kind);  
  if (k < 0) goto bad_type;      
      
  switch(g->ts.type) {     
  case BT_INTEGER:     
    a = g95_integer_kinds[k].range;          
    break; 
 
  case BT_REAL:         
  case BT_COMPLEX:        
    a = g95_real_kinds[k].range;        
    break;

  bad_type:   
  default:   
    g95_internal_error("g95_simplify_range(): Bad kind");    
  } 
 
  result = g95_int_expr(a);     
  result->where = g->where;     
     
  return result;     
}        
        
        
     
     
g95_expr *g95_simplify_verify(g95_expr *u, g95_expr *set, g95_expr *i) {          
size_t ind, leng, lenset;        
g95_expr *result;         
int back; 
 
  if (u->type != EXPR_CONSTANT || set->type != EXPR_CONSTANT) return NULL;          
          
  if (i != NULL && i->value.logical != 0)         
    back = 1;   
  else     
    back = 0;       
       
  result = g95_constant_result(BT_INTEGER, g95_default_integer_kind(),   
			       &u->where);     
     
  leng    = u->value.character.length;   
  lenset = set->value.character.length;

  if (leng == 0) {     
    mpz_set_ui(result->value.integer, 0);     
    return result;   
  }         
         
  if (back == 0) { 
    if (lenset == 0) {
      mpz_set_ui(result->value.integer, leng);  
      return result;     
    }    
    
    ind = strspn(u->value.character.string, set->value.character.string) + 1;     
    if (ind > leng) ind = 0;     
     
  } else {   
    if (lenset == 0) {        
      mpz_set_ui(result->value.integer, 1);    
      return result;   
    }          
          
    ind = leng-strspn(u->value.character.string, set->value.character.string);   
  }     
     
  mpz_set_ui(result->value.integer, ind);      
  return result;          
}          
          
          
    
    
g95_expr *g95_simplify_scan(g95_expr *a, g95_expr *t, g95_expr *i) {       
g95_expr *rslt;       
int back;     
size_t indx, leng, lenc;     
     
  if (a->type != EXPR_CONSTANT || t->type != EXPR_CONSTANT) return NULL;    
    
  if (i != NULL && i->value.logical != 0)  
    back = 1;
  else     
    back = 0;   
   
  rslt = g95_constant_result(BT_INTEGER, g95_default_integer_kind(),  
			       &a->where);      
      
  leng  = a->value.character.length;
  lenc = t->value.character.length;          
          
  if (leng == 0 || lenc == 0) {          
    indx = 0;         
  } else {         
    indx = strcspn(a->value.character.string, t->value.character.string) + 1;    
    if (indx > leng) indx=0;        
    if (back != 0 && indx != 0) indx = leng - indx + 1;
  }       
       
  mpz_set_ui(rslt->value.integer, indx);    
  return range_check(rslt, "SCAN");  
}   
   
   
          
          
g95_expr *g95_simplify_iachar(g95_expr *z) {        
g95_expr *r;       
int ind;        
        
  if (z->type != EXPR_CONSTANT) return NULL;  
  
  if (z->value.character.length != 1) {   
    g95_error("Argument of IACHAR at %L must be of length one", &z->where);        
    return &g95_bad_expr;         
  }  
  
  ind = xascii_table[(int) z->value.character.string[0] & 0xFF];         
         
  r = g95_int_expr(ind);          
  r->where = z->where;  
  
  return range_check(r, "IACHAR");  
}


      
      
g95_expr *g95_simplify_cosh(g95_expr *l) {
g95_expr *rslt;

  if (l->type != EXPR_CONSTANT) return NULL;   
   
  rslt = g95_constant_result(l->ts.type, l->ts.kind, &l->where); 
 
  hypercos(&l->value.real, &rslt->value.real);      
      
  return range_check(rslt, "COSH");   
}     
     
     
       
       
g95_expr *g95_simplify_atan(g95_expr *e) {    
g95_expr *r;

  if (e->type != EXPR_CONSTANT) return NULL;        
        
  r = g95_constant_result(e->ts.type, e->ts.kind, &e->where);  
  
  arctangent(&e->value.real, &r->value.real);         
         
  return range_check(r, "ATAN");     
     
}         
         
         
         
         
/* twos_complement()-- Given an unsigned multiple precision integer
 * and a bit size, convert it to the equivalent twos complement signed
 * integer. */       
       
static void twos_complement(mpz_t n, int bit_size) {          
int b;     
     
  if (!mpz_tstbit(n, bit_size-1)) return;      
      
  for(b=0; b<bit_size; b++)       
    if (mpz_tstbit(n, b))
      mpz_clrbit(n, b);         
    else       
      mpz_setbit(n, b);     
     
  mpz_add_ui(n, n, 1);      
  mpz_neg(n, n); 
}


   
   
g95_expr *g95_simplify_nearest(g95_expr *d, g95_expr *b) {          
g95_expr *rslt;       
float rval;        
double val, eps;       
int v, f, u;    
    
/* TODO: This implementation is dopey and probably not quite right,
 * but it's a start.*/         
         
  if (d->type != EXPR_CONSTANT) return NULL;   
   
  u = g95_validate_kind(d->ts.type, d->ts.kind);
  if (u == -1) g95_internal_error("g95_simplify_precision(): Bad kind");

  rslt = g95_constant_result(d->ts.type, d->ts.kind, &d->where);   
   
  val  = mpf_get_d(d->value.real);    
  v    = g95_real_kinds[u].digits;      
      
  eps = 1.;         
  for (f=1;f<v;++f) {        
    eps = eps/2.0;    
  }        
        
  if (mpf_cmp_ui(b->value.real, 0) > 0) {        
    if (u == g95_default_real_kind()) {         
      rval = (float) val;   
      rval = rval + eps;        
      mpf_set_d(rslt->value.real, rval);      
    }       
    else { 
      val = val + eps;     
      mpf_set_d(rslt->value.real, val);
    }   
  }
  else if (mpf_cmp_ui(b->value.real, 0) < 0) {      
    if (u == g95_default_real_kind()) {    
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
    g95_error("Invalid second argument of NEAREST at %L", &b->where);  
    g95_free(rslt);    
    return &g95_bad_expr;    
  }    
    
  return range_check(rslt, "NEAREST");
}    
    
    
       
       
g95_expr *g95_simplify_char(g95_expr *e, g95_expr *g) {    
g95_expr *result;   
int m, kind;    
    
  kind = get_kind(BT_CHARACTER, g, "CHAR", g95_default_character_kind());         
  if (kind == -1) return &g95_bad_expr;  
  
  if (e->type != EXPR_CONSTANT) return NULL;   
   
  if (g95_extract_int(e, &m) != NULL || m < 0 || m > 255) {
    g95_error("Bad character in CHAR function at %L", &e->where);   
    return &g95_bad_expr;  
  }     
     
  result = g95_constant_result(BT_CHARACTER, kind, &e->where); 
 
  result->value.character.length = 1;  
  result->value.character.string = g95_getmem(2);     
     
  result->value.character.string[0] = m; 
  result->value.character.string[1] = '\0';   /* For debugger */     
     
  return result;          
}  
  
  
      
      
g95_expr *g95_simplify_nint(g95_expr *j, g95_expr *b) {  
  
  return simplify_nint("NINT", j, b);       
}      
      
      
    
    
g95_expr *g95_simplify_log10(g95_expr *a) {   
g95_expr *rslt;      
      
  if (a->type != EXPR_CONSTANT) return NULL;         
         
  if (mpf_cmp(a->value.real, mpf_zero) <= 0) {    
    g95_error("Argument of LOG10 at %L cannot be less than or equal to zero",   
              &a->where);       
    return &g95_bad_expr;       
  } 
 
  rslt = g95_constant_result(a->ts.type, a->ts.kind, &a->where);        
        
  common_logarithm(&a->value.real, &rslt->value.real);     
     
  return range_check(rslt, "LOG10");  
}      
      
      
     
     
g95_expr *g95_simplify_cos(g95_expr *w) {
g95_expr *res;       
mpf_t xp, xq;        
        
  if (w->type != EXPR_CONSTANT) return NULL;

  res = g95_constant_result(w->ts.type, w->ts.kind, &w->where);  
  
  switch (w->ts.type) {    
  case BT_REAL:     
    cosine(&w->value.real, &res->value.real); 
    break;          
  case BT_COMPLEX:     
    mpf_init(xp);        
    mpf_init(xq); 
 
    cosine(&w->value.complex.r, &xp);      
    hypercos(&w->value.complex.i, &xq);    
    mpf_mul(res->value.complex.r, xp, xq);

    sine(&w->value.complex.r, &xp);     
    hypersine(&w->value.complex.i, &xq);  
    mpf_mul(xp, xp, xq);         
    mpf_neg(res->value.complex.i, xp);        
        
    mpf_clear(xp);        
    mpf_clear(xq);  
    break;  
  default:
    g95_internal_error("in g95_simplify_cos(): Bad type");       
  }     
     
  return range_check(res, "COS");          
          
}


   
   
g95_expr *g95_simplify_null(g95_expr *mold) { 
g95_expr *result;   
   
  result = g95_get_expr();  
  result->type = EXPR_NULL;       
       
  if (mold == NULL)        
    result->ts.type = BT_UNKNOWN;        
  else {         
    result->ts = mold->ts;       
    result->where = mold->where;     
  }        
        
  return result;  
} 
 
 
   
   
g95_expr *g95_simplify_exponent(g95_expr *m) {         
mpf_t b, absv, ln2, lnx;  
g95_expr *result;          
          
  if (m->type != EXPR_CONSTANT) return NULL;   
   
  result=g95_constant_result(BT_INTEGER, g95_default_integer_kind(),  
			     &m->where);         
         
  if (mpf_cmp(m->value.real, mpf_zero) == 0) { 
    mpz_set_ui(result->value.integer, 0);   
    return result;     
  }          
          
  mpf_init_set_ui(b, 2);  
  mpf_init(absv); 
  mpf_init(ln2);
  mpf_init(lnx);       
       
  natural_logarithm(&b, &ln2); 
 
  mpf_abs(absv, m->value.real);     
  natural_logarithm(&absv, &lnx); 
 
  mpf_div(lnx, lnx, ln2);   
  mpf_trunc(lnx, lnx);        
  mpf_add_ui(lnx, lnx, 1);        
  mpz_set_f(result->value.integer, lnx);      
      
  mpf_clear(b);   
  mpf_clear(ln2); 
  mpf_clear(lnx);    
  mpf_clear(absv);    
    
  return range_check(result, "EXPONENT");    
}          
          
          
   
   
g95_expr *g95_simplify_maxexponent(g95_expr *w) {         
g95_expr *res;        
int t;   
   
  t = g95_validate_kind(BT_REAL, w->ts.kind);   
  if (t < 0) g95_internal_error("g95_simplify_maxexponent(): Bad kind");        
        
  res = g95_int_expr(g95_real_kinds[t].max_exponent);          
  res->where = w->where;        
        
  return res;  
}       
       
       
     
     
g95_expr *g95_simplify_lgt(g95_expr *t, g95_expr *s) {          
          
  if (t->type != EXPR_CONSTANT || s->type != EXPR_CONSTANT) return NULL;        
        
  return g95_logical_expr(g95_compare_string(t, s, xascii_table) > 0, 
			  &t->where);    
}   
   
   
    
    
g95_expr *g95_simplify_llt(g95_expr *r, g95_expr *h) {

  if (r->type != EXPR_CONSTANT || h->type != EXPR_CONSTANT) return NULL;          
          
  return g95_logical_expr(g95_compare_string(r, h, xascii_table) < 0,
			  &r->where);    
}         
         
         
    
    
g95_expr *g95_simplify_abs(g95_expr *y) { 
g95_expr *result;      
mpf_t h, t;         
         
  if (y->type != EXPR_CONSTANT) return NULL;

  switch(y->ts.type) {         
  case BT_INTEGER:       
    result = g95_constant_result(BT_INTEGER, y->ts.kind, &y->where);        
        
    mpz_abs(result->value.integer, y->value.integer);       
       
    result = range_check(result, "IABS");  
    break;          
          
  case BT_REAL:    
    result = g95_constant_result(BT_REAL, y->ts.kind, &y->where);      
      
    mpf_abs(result->value.real, y->value.real);          
          
    result = range_check(result, "ABS");       
    break;    
    
  case BT_COMPLEX:        
    result = g95_constant_result(BT_REAL, y->ts.kind, &y->where);          
          
    mpf_init(h);         
    mpf_mul(h, y->value.complex.r, y->value.complex.r);     
     
    mpf_init(t);      
    mpf_mul(t, y->value.complex.i, y->value.complex.i);          
          
    mpf_add(h, h, t);        
    mpf_sqrt(result->value.real, h);  
  
    mpf_clear(h);   
    mpf_clear(t);      
      
    result = range_check(result, "CABS");      
    break;          
          
  default:    
    g95_internal_error("g95_simplify_abs(): Bad type");   
  }          
          
  return result;   
}       
       
       
       
       
g95_expr *g95_simplify_ichar(g95_expr *g) { 
g95_expr *result;    
int ix;     
     
  if (g->type != EXPR_CONSTANT) return NULL;    
    
  if (g->value.character.length != 1) {
    g95_error("Argument of ICHAR at %L must be of length one", &g->where); 
    return &g95_bad_expr;       
  }     
     
  ix = (int) g->value.character.string[0]; 
 
  if (ix < CHAR_MIN || ix > CHAR_MAX) {      
    g95_error("Argument of ICHAR at %L out of range of this processor",          
	      &g->where);        
    return &g95_bad_expr;     
  }    
    
  result = g95_int_expr(ix);  
  result->where = g->where;      
  return range_check(result, "ICHAR");       
}


      
      
g95_expr *g95_simplify_achar(g95_expr *k) {
g95_expr *result; 
int i;        
        
  if (k->type != EXPR_CONSTANT) return NULL;       
       
/* We cannot assume that the native character set is ASCII in this function */     
     
  if (g95_extract_int(k, &i) != NULL  || i < 0 || i > 127) {          
      g95_error("Extended ASCII not implemented: argument of ACHAR at %L "      
		"must be between 0 and 127", &k->where);      
      return &g95_bad_expr;  
  }

  result = g95_constant_result(BT_CHARACTER, g95_default_character_kind(),      
			       &k->where);  
  
  result->value.character.string = g95_getmem(2);

  result->value.character.length = 1;        
  result->value.character.string[0] = ascii_table[i]; 
  result->value.character.string[1] = '\0';   /* For debugger */        
  return result;      
}    
    
    
          
          
g95_expr *g95_simplify_lle(g95_expr *d, g95_expr *l) {         
         
  if (d->type != EXPR_CONSTANT || l->type != EXPR_CONSTANT) return NULL;   
   
  return g95_logical_expr(g95_compare_string(d, l, xascii_table) <= 0,        
			  &d->where);        
}     
     
     
        
        
g95_expr *g95_simplify_dint(g95_expr *c) {    
g95_expr *rtrunc, *r;  
  
  if (c->type != EXPR_CONSTANT) return NULL;    
    
  rtrunc = g95_copy_expr(c); 
 
  mpf_trunc(rtrunc->value.real, c->value.real);      
      
  r = g95_real2real(rtrunc, g95_default_double_kind());         
  g95_free_expr(rtrunc);       
       
  return range_check(r, "DINT");

}  
  
  
         
         
g95_expr *g95_simplify_ior(g95_expr *f, g95_expr *s) {         
g95_expr *result;    
    
  if (f->type != EXPR_CONSTANT || s->type != EXPR_CONSTANT) return NULL;       
       
  result = g95_constant_result(BT_INTEGER, f->ts.kind, &f->where);    
    
  mpz_ior(result->value.integer, f->value.integer, s->value.integer);    
  return result;      
}   
   
   


g95_expr *g95_simplify_ishft(g95_expr *n, g95_expr *j) { 
int y, c, shift, bit_size;         
g95_expr *result;      
mpz_t p;   
   
  if (n->type != EXPR_CONSTANT || j->type != EXPR_CONSTANT) return NULL;       
       
  if (g95_extract_int(j, &shift) != NULL) { 
    g95_error("Invalid second argument of ISHFT at %L", &j->where);     
    return &g95_bad_expr;   
  }         
         
  c = g95_validate_kind(BT_INTEGER, n->ts.kind); 
  if (c == -1) g95_internal_error("In g95_simplify_ishft: bad kind");         
         
  bit_size = g95_integer_kinds[c].bit_size;   
   
  mpz_init_set_ui(p, 0);        
        
  for(y=0; y<bit_size; y++) {          
    if (y-shift < 0 || y-shift >= bit_size) continue;  
  
    if (mpz_tstbit(n->value.integer, y-shift)) mpz_setbit(p, y);       
  }  
  
  twos_complement(p, bit_size);       
       
  result = g95_constant_result(n->ts.type, n->ts.kind, &n->where);         
  mpz_set(result->value.integer, p);  
  
  return result;      
}   
   
   
          
          
g95_expr *g95_simplify_ieor(g95_expr *e, g95_expr *t) {          
g95_expr *rslt;   
   
  if (e->type != EXPR_CONSTANT || t->type != EXPR_CONSTANT) return NULL;     
     
  rslt = g95_constant_result(BT_INTEGER, e->ts.kind, &e->where);         
         
  mpz_xor(rslt->value.integer, e->value.integer, t->value.integer);       
       
  return rslt;         
}    
    
    
    
    
g95_expr *g95_simplify_selected_real_kind(g95_expr *m, g95_expr *n) {      
int range, precision, r, k, found_precision, found_range;          
g95_expr *result;

  if (m == NULL)    
    precision = 0;
  else {
    if (m->type != EXPR_CONSTANT || g95_extract_int(m, &precision) != NULL)          
      return NULL; 
  }       
       
  if (n == NULL)     
    range = 0;   
  else {   
    if (n->type != EXPR_CONSTANT || g95_extract_int(n, &range) != NULL)   
      return NULL;    
  }   
   
  k = INT_MAX;       
  found_precision = 0;     
  found_range = 0;  
  
  for(r=0; g95_real_kinds[r].kind!=0; r++) {   
    if (g95_real_kinds[r].precision >= precision) found_precision = 1;   
   
    if (g95_real_kinds[r].range >= range) found_range = 1;    
    
    if (g95_real_kinds[r].precision >= precision &&          
	g95_real_kinds[r].range >= range &&        
	g95_real_kinds[r].kind < k)   
      k = g95_real_kinds[r].kind;
  }       
       
  if (k == INT_MAX) {        
    k = 0;     
     
    if (!found_precision) k = -1;        
    if (!found_range) k -= 2;      
  }

  result = g95_int_expr(k);        
  result->where = (m != NULL) ? m->where : n->where;  
  
  return result;  
}     
     
     
          
          
g95_expr *g95_simplify_conjg(g95_expr *w) {      
g95_expr *res;    
    
  if (w->type != EXPR_CONSTANT) return NULL; 
 
  res = g95_copy_expr(w);    
  mpf_neg(res->value.complex.i, res->value.complex.i);        
        
  return range_check(res, "CONJG");    
}        
        
        
       
       
/* simplify_min_max()-- This function is special since MAX() can take
 * any number of arguments.  The simplified expression is a rewritten
 * version of the argument list containing at most one constant
 * element.  Other constant elements are deleted.  Because the
 * argument list has already been checked, this function always
 * succeeds.  sign is 1 for MAX(), -1 for MIN(). */        
        
static g95_expr *simplify_min_max(g95_expr *e2, int sign) {        
g95_actual_arglist *arg, *last, *extremum; 
g95_expr *p;      
      
  last = NULL;  
  extremum = NULL;     
     
  arg = e2->value.function.actual;       
       
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
      e2->value.function.actual = arg->next;  
    else
      last->next = arg->next;      
      
    arg->next = NULL;         
    g95_free_actual_arglist(arg);    
    arg = last;          
  }    
    
  /* If there is one value left, replace the function call with the
   * expression */          
          
  if (e2->value.function.actual->next != NULL) return NULL; 
 
  p = g95_copy_expr(e2->value.function.actual->u.expr);
  if (g95_convert_type(p, &e2->ts, 1) == SUCCESS) return p;          
          
  g95_free_expr(p);       
  return &g95_bad_expr;        
}     
     
     
        
        
g95_expr *g95_simplify_rrspacing(g95_expr *l) {     
g95_expr *r; 
mpf_t w, absv, ln2, lnx, frac, pow2;        
unsigned long exp2; 
int o, t;          
          
  if (l->type != EXPR_CONSTANT) return NULL;        
        
  o = g95_validate_kind(l->ts.type, l->ts.kind);         
  if (o < 0) g95_internal_error("g95_simplify_rrspacing(): bad kind");      
      
  r=g95_constant_result(BT_REAL, l->ts.kind, &l->where);

  t = g95_real_kinds[o].digits;     
     
  if (mpf_cmp(l->value.real, mpf_zero) == 0) {    
    mpf_ui_div(r->value.real, 1, g95_real_kinds[o].tiny);
    return r; 
  }        
        
  mpf_init_set_ui(w, 2);          
  mpf_init(ln2);   
  mpf_init(absv); 
  mpf_init(lnx);        
  mpf_init(frac);       
  mpf_init(pow2);    
    
  natural_logarithm(&w, &ln2);       
       
  mpf_abs(absv, l->value.real);       
  natural_logarithm(&absv, &lnx);          
          
  mpf_div(lnx, lnx, ln2);    
  mpf_trunc(lnx, lnx);    
  mpf_add_ui(lnx, lnx, 1);      
      
  exp2 = (unsigned long) mpf_get_d(lnx);       
  mpf_pow_ui(pow2, w, exp2);      
  mpf_div(frac, absv, pow2); 
 
  exp2 = (unsigned long) t;       
  mpf_mul_2exp(r->value.real, frac, exp2);         
         
  mpf_clear(w);       
  mpf_clear(ln2);    
  mpf_clear(absv);       
  mpf_clear(lnx);   
  mpf_clear(frac);       
  mpf_clear(pow2);         
         
  return range_check(r, "RRSPACING"); 
}     
     
     
 
 
g95_expr *g95_simplify_ceiling(g95_expr *g, g95_expr *k) {     
g95_expr *ceil, *result;         
int kind;  
  
  kind = get_kind(BT_REAL, k, "CEILING", g95_default_real_kind());      
  if (kind == -1) return &g95_bad_expr;   
   
  if (g->type != EXPR_CONSTANT) return NULL; 
 
  result = g95_constant_result(BT_INTEGER, kind, &g->where);        
        
  ceil = g95_copy_expr(g);     
     
  mpf_ceil(ceil->value.real, g->value.real);  
  mpz_set_f(result->value.integer, ceil->value.real);        
        
  g95_free_expr(ceil);          
          
  return range_check(result, "CEILING");    
}  
  
  
    
    
g95_expr *g95_simplify_index(g95_expr *h, g95_expr *n, g95_expr *b) {       
g95_expr *result; 
int back, leng, lensub;
int e, p, q, cont, ix=0, st;          
          
  if (h->type != EXPR_CONSTANT || n->type != EXPR_CONSTANT) return NULL;        
        
  if (b != NULL && b->value.logical != 0)       
    back = 1;        
  else       
    back = 0;       
       
  result = g95_constant_result(BT_INTEGER, g95_default_integer_kind(),
			       &h->where);         
         
  leng    = h->value.character.length; 
  lensub = n->value.character.length;       
       
  if (leng < lensub) {
    mpz_set_si(result->value.integer, 0);         
    return result;     
  }  
  
  if (back == 0) {       
       
    if (lensub == 0) {  
      mpz_set_si(result->value.integer, 1);      
      return result;         
    } else if (lensub == 1) {          
      for(e=0; e<leng; e++) { 
        for(p=0; p<lensub; p++) {          
    	  if (n->value.character.string[p] == h->value.character.string[e]) {     
	    ix = e+1;        
	    goto done;        
	  }          
	}    
      }     
    } else {
      for(e=0; e<leng; e++) {          
        for(p=0; p<lensub; p++) {         
	  if (n->value.character.string[p] == h->value.character.string[e]) {
	    st = e;        
	    cont = 0;    
    
	    for(q=0; q<lensub; q++) {   
    	      if (n->value.character.string[q] ==          
		  h->value.character.string[q+st]) cont++;
	    }       
       
	    if (cont == lensub) {        
	      ix = st+1;      
	      goto done;  
	    }      
	  }          
	}
      }      
    }      
      
  } else {    
    
    if (lensub == 0) {
      mpz_set_si(result->value.integer, leng+1);        
      return result;     
    }      
    else if (lensub == 1) {      
      for(e=0; e<leng; e++) {       
        for(p=0; p<lensub; p++) {     
	  if (n->value.character.string[p]==h->value.character.string[leng-e]) {          
	    ix = leng-e+1;    
	    goto done;
	  }       
	}    
      }      
    } else {  
      for(e=0; e<leng; e++) {   
        for(p=0; p<lensub; p++) {       
	  if (n->value.character.string[p]==h->value.character.string[leng-e]) {   
	    st = leng-e;       
	    if (st <= leng-lensub) {       
	      cont = 0;          
	      for(q=0; q<lensub; q++)         
    	        if (n->value.character.string[q] ==   
		    h->value.character.string[q+st]) cont++;

	      if (cont == lensub) {  
	        ix = st+1;   
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
  mpz_set_si(result->value.integer, ix);  
  return range_check(result, "INDEX");
}


     
     
g95_expr *g95_simplify_selected_int_kind(g95_expr *s) {     
int w, k0, range;   
g95_expr *result;          
          
  if (s->type != EXPR_CONSTANT || g95_extract_int(s, &range) != NULL)     
    return NULL;          
          
  k0 = INT_MAX;          
          
  for(w=0; g95_integer_kinds[w].kind!=0; w++)      
    if (g95_integer_kinds[w].range >= range &&      
	g95_integer_kinds[w].kind < k0) k0 = g95_integer_kinds[w].kind;        
        
  if (k0 == INT_MAX) k0 = -1; 
 
  result = g95_int_expr(k0);        
  result->where = s->where;          
          
  return result; 
}  
  
  
          
          
g95_expr *g95_simplify_sqrt(g95_expr *e) {
g95_expr *result;   
mpf_t ac, ad, f, o, u;     
     
  if (e->type != EXPR_CONSTANT) return NULL;      
      
  result = g95_constant_result(e->ts.type, e->ts.kind, &e->where);  
  
  switch (e->ts.type) {       
  case BT_REAL:        
    if (mpf_cmp_si(e->value.real, 0) < 0) return NULL;      
    mpf_sqrt(result->value.real, e->value.real);        
        
    break;  
  
  case BT_COMPLEX:    
    /*Formula taken from Numerical Recipes to avoid over- and underflow*/        
        
    mpf_init(ac);     
    mpf_init(ad);  
    mpf_init(f);    
    mpf_init(o);         
    mpf_init(u);  
  
    if (mpf_cmp_ui(e->value.complex.r, 0) == 0 &&   
	mpf_cmp_ui(e->value.complex.i, 0) == 0) {     
     
      mpf_set_ui(result->value.complex.r, 0);         
      mpf_set_ui(result->value.complex.i, 0);         
      break;   
    }     
     
    mpf_abs(ac, e->value.complex.r);     
    mpf_abs(ad, e->value.complex.i);          
          
    if (mpf_cmp(ac, ad) >= 0) {    
      mpf_div(o, e->value.complex.i, e->value.complex.r);   
      mpf_mul(o, o, o);  
      mpf_add_ui(o, o, 1);  
      mpf_sqrt(o, o);    
      mpf_add_ui(o, o, 1);     
      mpf_div_ui(o, o, 2);          
      mpf_sqrt(o, o);   
      mpf_sqrt(f, ac);
      mpf_mul(u, f, o);      
    } else {       
      mpf_div(f, e->value.complex.r, e->value.complex.i);    
      mpf_mul(o, f, f); 
      mpf_add_ui(o, o, 1);      
      mpf_sqrt(o, o);         
      mpf_abs(f, f);    
      mpf_add(o, o, f);     
      mpf_div_ui(o, o, 2);  
      mpf_sqrt(o, o);     
      mpf_sqrt(f, ad);   
      mpf_mul(u, f, o);    
    } 
 
    if (mpf_cmp_ui(u, 0) !=0 && mpf_cmp_ui(e->value.complex.r, 0) >=0) {       
      mpf_mul_ui(o, u, 2);  
      mpf_div(result->value.complex.i, e->value.complex.i, o);
      mpf_set(result->value.complex.r, u);  
    } else if (mpf_cmp_ui(u, 0) !=0 && mpf_cmp_ui(e->value.complex.r, 0) < 0 &&       
	       mpf_cmp_ui(e->value.complex.i, 0) >= 0) {        
      mpf_mul_ui(o, u, 2); 
      mpf_div(result->value.complex.r, e->value.complex.i, o);  
      mpf_set(result->value.complex.i, u);       
    } else if (mpf_cmp_ui(u, 0) !=0 && mpf_cmp_ui(e->value.complex.r, 0) < 0 && 
	       mpf_cmp_ui(e->value.complex.i, 0) < 0) {         
      mpf_mul_ui(o, u, 2);          
      mpf_div(result->value.complex.r, ad, o);      
      mpf_neg(u, u);   
      mpf_set(result->value.complex.i, u);          
    } else { 
      g95_internal_error("invalid complex argument of SQRT at %L",         
			 &e->where);       
      mpf_clear(f);  mpf_clear(o); mpf_clear(ac);    
      mpf_clear(ad); mpf_clear(u);          
      g95_free_expr(result);      
      return &g95_bad_expr;       
    }

    mpf_clear(f);   
    mpf_clear(o);
    mpf_clear(ac);
    mpf_clear(ad);       
    mpf_clear(u);        
        
    break;      
      
  default:      
    g95_internal_error("invalid argument of SQRT at %L", &e->where);    
    g95_free_expr(result);
    return &g95_bad_expr;  
  }

  return range_check(result, "SQRT");          
}      
      
      
  
  
g95_expr *g95_simplify_ishftc(g95_expr *p, g95_expr *h, g95_expr *sz) {     
int d, v, shift, isize, t, bit_size;
g95_expr *rslt;       
mpz_t w;     
     
  if (p->type != EXPR_CONSTANT || h->type != EXPR_CONSTANT) return NULL;    
    
  if (g95_extract_int(h, &shift) != NULL) {     
    g95_error("Invalid second argument of ISHFTC at %L", &h->where);  
    return &g95_bad_expr;      
  }   
   
  t = g95_validate_kind(p->ts.type, p->ts.kind);      
  if (t == -1) g95_internal_error("In g95_simplify_ishftc: bad kind"); 
 
  bit_size = g95_integer_kinds[t].bit_size;   
   
  if (sz == NULL)         
    isize = g95_integer_kinds[t].bit_size;    
  else {    
    if (g95_extract_int(sz, &isize) != NULL || isize <= 0) {    
      g95_error("Invalid third argument of ISHFTC at %L", &sz->where);   
      return &g95_bad_expr;    
    }       
  }    
    
  mpz_init_set_ui(w, 0);       
       
  for(d=0; d<bit_size; d++) {   
    if (d >= isize)   
      v = d;       
    else         
      v = (d - shift) % isize;     
     
    if (mpz_tstbit(p->value.integer, v)) mpz_setbit(w, d);      
  }     
     
  twos_complement(w, bit_size);    
    
  rslt = g95_constant_result(p->ts.type, p->ts.kind, &p->where); 
  mpz_set(rslt->value.integer, w);          
          
  return rslt;      
}          
          
          
   
   
g95_expr *g95_simplify_digits(g95_expr *m) {        
int l, digits;

  l = g95_validate_kind(m->ts.type, m->ts.kind);          
  if (l < 0) goto bad;        
        
  switch(m->ts.type) {   
  case BT_INTEGER:    
    digits = g95_integer_kinds[l].digits;         
    break;         
         
  case BT_REAL:   
  case BT_COMPLEX:       
    digits = g95_real_kinds[l].digits;     
    break;        
        
  default:
  bad:  
    g95_internal_error("g95_simplify_digits(): Bad type");         
  }   
   
  return g95_int_expr(digits);        
}      
      
      
    
    
g95_expr *g95_simplify_max(g95_expr *g) {

  return simplify_min_max(g, 1);     
}          
          
          
   
   
/* invert_table()-- Given a collating table, create the inverse table */

static void invert_table(int *table, int *xtable) {          
int d;     
     
  for(d=0; d<256; d++)       
    xtable[d] = 0; 
 
  for(d=0; d<256; d++)         
    xtable[table[d]] = d;    
}     
     
     
 
 
g95_expr *g95_simplify_log(g95_expr *i) {   
g95_expr *res;     
     
  if (i->type != EXPR_CONSTANT || i->ts.type == BT_COMPLEX) return NULL;     
     
  res = g95_constant_result(i->ts.type, i->ts.kind, &i->where);

  switch(i->ts.type) {        
  case BT_REAL:       
    if (mpf_cmp(i->value.real, mpf_zero) <= 0) { 
      g95_error("Argument of LOG at %L cannot be less than or equal to zero", 
		&i->where);    
      g95_free_expr(res);
      return &g95_bad_expr;  
    }   
   
    natural_logarithm(&i->value.real, &res->value.real);          
    break;      
      
  default:  
    g95_internal_error("g95_simplify_log: bad type");          
  }          
          
  return range_check(res, "LOG");          
}      
      
      


g95_expr *g95_simplify_min(g95_expr *l) {   
   
  return simplify_min_max(l, -1);       
}         
         
         
 
 
g95_expr *g95_simplify_dprod(g95_expr *v, g95_expr *f) {
g95_expr *mult1, *mult2, *r; 
 
  if (v->type != EXPR_CONSTANT || f->type != EXPR_CONSTANT) return NULL;

  r = g95_constant_result(BT_REAL, g95_default_double_kind(), &v->where);

  mult1 = g95_real2real(v, g95_default_double_kind());
  mult2 = g95_real2real(f, g95_default_double_kind()); 
 
  mpf_mul(r->value.real, mult1->value.real, mult2->value.real);          
          
  g95_free_expr(mult1);          
  g95_free_expr(mult2);     
     
  return range_check(r, "DPROD");     
}     
     
     


g95_expr *g95_simplify_trim(g95_expr *m) {    
g95_expr *r;       
int c, q, leng, lentrim;          
          
  if (m->type != EXPR_CONSTANT) return NULL;    
    
  leng = m->value.character.length;         
         
  r = g95_constant_result(BT_CHARACTER, m->ts.kind, &m->where); 
 
  for (c=0, q=1; q<=leng; ++q) {         
    if (m->value.character.string[leng-q] == ' ') 
      c++;      
    else   
      break;          
  }       
       
  lentrim = leng-c;        
        
  r->value.character.length = lentrim;         
  r->value.character.string = g95_getmem(lentrim+1);     
     
  for(q=0; q<lentrim; q++)          
    r->value.character.string[q] = m->value.character.string[q];    
    
  r->value.character.string[lentrim] = '\0';   /* For debugger */     
     
  return r;          
}   
   
   
 
 
void g95_simplify_init_1(void) {        
        
  mpf_init_set_str(mpf_zero, "0.0", 10);    
  mpf_init_set_str(mpf_half, "0.5", 10);      
  mpf_init_set_str(mpf_one,  "1.0", 10); 
  mpz_init_set_str(mpz_zero,   "0", 10);    
    
  invert_table(ascii_table, xascii_table);       
} 
 
 
