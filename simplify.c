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
         
static g95_expr *range_check(g95_expr *rslt, char *nam) {          
          
  if (g95_range_check(rslt) == ARITH_OK)
    return rslt; 
 
  g95_error("Result of %s overflows its kind at %L", nam, &rslt->where);      
  g95_free_expr(rslt);      
  return &g95_bad_expr;    
}    
    
    
         
         
g95_expr *g95_simplify_len_trim(g95_expr *k) {          
g95_expr *result;  
int cnt, len, lentrim, z;   
   
  if (k->type != EXPR_CONSTANT) return NULL;  
  
  result = g95_constant_result(BT_INTEGER, g95_default_integer_kind(),          
			       &k->where);        
        
  len = k->value.character.length; 
 
  for(cnt=0, z=1; z<=len; z++)     
    if (k->value.character.string[len-z] == ' ')    
      cnt++;    
    else         
      break;  
  
  lentrim = len-cnt;       
       
  mpz_set_si(result->value.integer, lentrim);         
  return range_check(result, "LEN_TRIM");        
}


 
 
/* g95_simplify_reshape()-- This one is a bear, but mainly has to do
 * with shuffling elements. */     
     
g95_expr *g95_simplify_reshape(g95_expr *source, g95_expr *shape_exp,          
			       g95_expr *pad, g95_expr *order_exp) { 
 
int order[G95_MAX_DIMENSIONS], extent[G95_MAX_DIMENSIONS];      
int u, rank, npad, v[G95_MAX_DIMENSIONS];
g95_constructor *head, *tail;         
mpz_t ix, sz;   
unsigned long z;     
size_t nsource; 
g95_expr *g;        
        
/* Unpack the shape array */          
          
  if (source->type != EXPR_ARRAY || !g95_is_constant_expr(source)) return NULL;   
   
  if (shape_exp->type != EXPR_ARRAY || !g95_is_constant_expr(shape_exp))        
    return NULL;      
      
  if (pad != NULL && (pad->type != EXPR_ARRAY || !g95_is_constant_expr(pad)))     
    return NULL;  
  
  if (order_exp != NULL &&   
      (order_exp->type != EXPR_ARRAY || !g95_is_constant_expr(order_exp)))     
    return NULL;      
      
  mpz_init(ix);    
  rank = 0;     
  head = tail = NULL; 
 
  for(;;) {        
    g = g95_get_array_element(shape_exp, rank); 
    if (g == NULL) break; 
 
    if (g95_extract_int(g, &extent[rank]) != NULL) {       
      g95_error("Integer too large in shape specification at %L",       
		&g->where);   
      g95_free_expr(g);
      goto bad_reshape;     
    } 
 
    g95_free_expr(g);         
         
    if (rank >= G95_MAX_DIMENSIONS) {  
      g95_error("Too many dimensions in shape specification for RESHAPE "   
		"at %L", &g->where);      
      
      goto bad_reshape;        
    }     
     
    if (extent[rank] < 0) {     
      g95_error("Shape specification at %L cannot be negative", &g->where);   
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
    for(u=0; u<rank; u++)
      order[u] = u;  
  
  } else {   
   
    for(u=0; u<rank; u++)          
      v[u] = 0;  
  
    for(u=0; u<rank; u++) {   
      g = g95_get_array_element(order_exp, u); 
      if (g == NULL) { 
	g95_error("ORDER parameter of RESHAPE at %L is not the same size "  
		  "as SHAPE parameter", &g->where);        
	goto bad_reshape;      
      }      
      
      if (g95_extract_int(g, &order[u]) != NULL) { 
	g95_error("Error in ORDER parameter of RESHAPE at %L", &g->where); 
	g95_free_expr(g);  
	goto bad_reshape;       
      }

      g95_free_expr(g);     
     
      if (order[u] < 1 || order[u] > rank) {   
	g95_error("ORDER parameter of RESHAPE at %L is out of range",        
		  &g->where);     
	goto bad_reshape;    
      } 
 
      order[u]--;          
          
      if (v[order[u]]) {         
	g95_error("Invalid permutation in ORDER parameter at %L", &g->where);       
	goto bad_reshape;
      }        
        
      v[order[u]] = 1;    
    }      
  }          
          
  /* Count the elements in the source and padding arrays */   
   
  npad = 0;          
  if (pad != NULL) {
    g95_array_size(pad, &sz);
    npad = mpz_get_ui(sz);     
    mpz_clear(sz);        
  }        
        
  g95_array_size(source, &sz); 
  nsource = mpz_get_ui(sz); 
  mpz_clear(sz); 
 
  /* If it weren't for that pesky permutation we could just loop
   * through the source and round out any shortage with pad elements.
   * But no, someone just had to have the compiler do something the
   * user should be doing. */  
  
  for(u=0; u<rank; u++)       
    v[u] = 0;   
   
  for(;;) {     /* Figure out which element to extract */     
    mpz_set_ui(ix, 0);         
         
    for(u=rank-1; u>=0; u--) {   
      mpz_add_ui(ix, ix, v[order[u]]);     
      if (u != 0) mpz_mul_ui(ix, ix, extent[order[u-1]]);  
    } 
 
    if (mpz_cmp_ui(ix, INT_MAX) > 0) {        
      g95_internal_error("Reshaped array too large at %L", &g->where); 
      goto bad_reshape;         
    }        
        
    z = mpz_get_ui(ix);  
  
    if (z < nsource)
      g = g95_get_array_element(source, z);          
    else {      
      z = z - nsource;       
       
      if (npad == 0) {          
	g95_error("PAD parameter required for short SOURCE parameter at %L",         
		  &source->where);
	goto bad_reshape;     
      }

      z = z % npad;     
      g = g95_get_array_element(pad, z);      
    } 
 
    if (head == NULL)  
      head = tail = g95_get_constructor();   
    else {  
      tail->next = g95_get_constructor();   
      tail = tail->next;     
    }    
    
    if (g == NULL) goto bad_reshape;          
          
    tail->where = g->where;  
    tail->expr = g;      
      
    /* Calculate the next element */    
    
    u = 0;  
  inc:     
    if (++v[u] < extent[u]) continue;
    v[u++] = 0;        
    if (u < rank) goto inc; 
 
    break;       
  }       
       
  mpz_clear(ix);     
     
  g = g95_get_expr();          
  g->where = source->where;    
  g->type = EXPR_ARRAY;      
  g->value.constructor = head;      
  g->shape = g95_get_shape(rank);         
         
  for(u=0; u<rank; u++)          
    mpz_init_set_ui(g->shape[u], extent[order[u]]);         
         
  g->ts = head->expr->ts;        
  g->rank = rank;    
    
  return g;  
  
bad_reshape:    
  g95_free_constructor(head);          
  mpz_clear(ix);         
  return &g95_bad_expr;   
}      
      
      
         
         
/* simplify_cmplx()-- Common subroutine for simplifying CMPLX and DCMPLX */    
    
static g95_expr *simplify_cmplx(g95_expr *f, g95_expr *l, int k0,         
				char *nm) {      
g95_expr *rslt;  
  
  rslt = g95_constant_result(BT_COMPLEX, k0, &f->where);   
   
  mpf_set_ui(rslt->value.complex.i, 0);         
         
  switch(f->ts.type) { 
  case BT_INTEGER:    
    mpf_set_z(rslt->value.complex.r, f->value.integer);         
    break;     
     
  case BT_REAL: 
    mpf_set(rslt->value.complex.r, f->value.real);
    break;   
   
  case BT_COMPLEX:          
    mpf_set(rslt->value.complex.r, f->value.complex.r);    
    mpf_set(rslt->value.complex.i, f->value.complex.i);
    break;   
   
  default:    
    g95_internal_error("g95_simplify_dcmplx(): Bad type (x)");     
  }          
          
  if (l != NULL) { 
    switch(l->ts.type) {    
    case BT_INTEGER: 
      mpf_set_z(rslt->value.complex.i, l->value.integer);        
      break; 
 
    case BT_REAL:       
      mpf_set(rslt->value.complex.i, l->value.real);    
      break;       
       
    default:
      g95_internal_error("g95_simplify_dcmplx(): Bad type (y)");         
    }         
  }    
    
  return range_check(rslt, nm);   
}   
   
   
     
     
g95_expr *g95_simplify_achar(g95_expr *v) {   
g95_expr *rslt;  
int ix;        
        
  if (v->type != EXPR_CONSTANT) return NULL;     
     
/* We cannot assume that the native character set is ASCII in this function */ 
 
  if (g95_extract_int(v, &ix) != NULL  || ix < 0 || ix > 127) {      
      g95_error("Extended ASCII not implemented: argument of ACHAR at %L " 
		"must be between 0 and 127", &v->where);        
      return &g95_bad_expr;       
  }      
      
  rslt = g95_constant_result(BT_CHARACTER, g95_default_character_kind(),          
			       &v->where);   
   
  rslt->value.character.string = g95_getmem(2);

  rslt->value.character.length = 1;        
  rslt->value.character.string[0] = ascii_table[ix];        
  rslt->value.character.string[1] = '\0';   /* For debugger */         
  return rslt;     
}   
   
   
      
      
g95_expr *g95_simplify_scale(g95_expr *f, g95_expr *b) {       
int a, neg_flag, power, exp_range;     
mpf_t scale, radix;
g95_expr *res;      
      
  if (f->type != EXPR_CONSTANT || b->type != EXPR_CONSTANT) return NULL;      
      
  res = g95_constant_result(BT_REAL, f->ts.kind, &f->where);        
        
  if (mpf_sgn(f->value.real) == 0) {       
    mpf_set_ui(res->value.real, 0);     
    return res;   
  }         
         
  a = g95_validate_kind(BT_REAL, f->ts.kind); 
  exp_range = g95_real_kinds[a].max_exponent - g95_real_kinds[a].min_exponent;      
      
  /* This check filters out values of i that would overflow an int */       
       
  if (mpz_cmp_si(b->value.integer, exp_range+2) > 0 ||         
      mpz_cmp_si(b->value.integer, -exp_range-2) < 0) {    
    g95_error("Result of SCALE overflows its kind at %L", &res->where);       
    return &g95_bad_expr;
  }   
   
  /* Compute scale = radix ** power */        
        
  power = mpz_get_si(b->value.integer); 
 
  if (power >= 0)          
    neg_flag = 0;      
  else {
    neg_flag = 1;     
    power = -power;    
  }     
     
  mpf_init_set_ui(radix, g95_real_kinds[a].radix);         
  mpf_init(scale);         
  mpf_pow_ui(scale, radix, power);          
          
  if (neg_flag)       
    mpf_div(res->value.real, f->value.real, scale);        
  else    
    mpf_mul(res->value.real, f->value.real, scale);

  mpf_clear(scale); 
  mpf_clear(radix);  
  
  return range_check(res, "SCALE");        
}       
       
       
    
    
g95_expr *g95_simplify_ifix(g95_expr *k) {
g95_expr *rtrunc, *r; 
 
  if (k->type != EXPR_CONSTANT) return NULL; 
 
  r = g95_constant_result(BT_INTEGER, g95_default_integer_kind(),   
			       &k->where);   
   
  rtrunc = g95_copy_expr(k);

  mpf_trunc(rtrunc->value.real, k->value.real);         
  mpz_set_f(r->value.integer, rtrunc->value.real);        
        
  g95_free_expr(rtrunc);     
  return range_check(r, "IFIX");     
}      
      
      
        
        
g95_expr *g95_simplify_maxexponent(g95_expr *b) {
g95_expr *res;     
int i;   
   
  i = g95_validate_kind(BT_REAL, b->ts.kind);          
  if (i < 0) g95_internal_error("g95_simplify_maxexponent(): Bad kind");

  res = g95_int_expr(g95_real_kinds[i].max_exponent);
  res->where = b->where;          
          
  return res;          
}       
       
       
     
     
g95_expr *g95_simplify_adjustr(g95_expr *w) {       
g95_expr *rslt;       
int c, j, len;          
char ch;    
    
  if (w->type != EXPR_CONSTANT) return NULL;          
          
  len = w->value.character.length;          
          
  rslt = g95_constant_result(BT_CHARACTER, w->ts.kind, &w->where);

  rslt->value.character.length = len;
  rslt->value.character.string = g95_getmem(len+1);

  for (c=0, j=len-1; j>=0; --j) {        
    ch = w->value.character.string[j];   
    if (ch != ' ') break;  
    ++c;  
  }

  for (j=0; j<c; ++j) {          
    rslt->value.character.string[j] = ' ';   
  }     
     
  for (j=c; j<len; ++j) {      
    rslt->value.character.string[j] = w->value.character.string[j-c];      
  } 
 
  rslt->value.character.string[len] = '\0';   /* For debugger */

  return rslt;      
} 
 
 
      
      
g95_expr *g95_simplify_iand(g95_expr *a, g95_expr *i) {     
g95_expr *result;         
         
  if (a->type != EXPR_CONSTANT || i->type != EXPR_CONSTANT) return NULL;      
      
  result = g95_constant_result(BT_INTEGER, a->ts.kind, &a->where);  
  
  mpz_and(result->value.integer, a->value.integer, i->value.integer);  
  return result;          
}       
       
       
     
     
g95_expr *g95_simplify_idint(g95_expr *v) { 
g95_expr *rtrunc, *result;          
          
  if (v->type != EXPR_CONSTANT) return NULL;  
  
  result = g95_constant_result(BT_INTEGER, g95_default_integer_kind(), 
			       &v->where);  
  
  rtrunc = g95_copy_expr(v);      
      
  mpf_trunc(rtrunc->value.real, v->value.real);        
  mpz_set_f(result->value.integer, rtrunc->value.real);   
   
  g95_free_expr(rtrunc);
  return range_check(result, "IDINT");          
} 
 
 
          
          
g95_expr *g95_simplify_btest(g95_expr *m, g95_expr *bit) {        
int t;  
  
  if (m->type != EXPR_CONSTANT || bit->type != EXPR_CONSTANT) return NULL;        
        
  if (g95_extract_int(bit, &t) != NULL || t < 0)      
    return g95_logical_expr(0, &m->where);      
      
  return g95_logical_expr(mpz_tstbit(m->value.integer, t), &m->where);
}     
     
     
    
    
g95_expr *g95_simplify_null(g95_expr *mold) {     
g95_expr *rslt;  
  
  rslt = g95_get_expr();  
  rslt->type = EXPR_NULL;     
     
  if (mold == NULL)   
    rslt->ts.type = BT_UNKNOWN;    
  else { 
    rslt->ts = mold->ts; 
    rslt->where = mold->where;       
  }   
   
  return rslt;         
}


    
    
g95_expr *g95_simplify_asin(g95_expr *e) {         
g95_expr *rslt;     
mpf_t negative, square, u;   
   
  if (e->type != EXPR_CONSTANT) return NULL;        
        
  if (mpf_cmp_si(e->value.real, 1) > 0 || mpf_cmp_si(e->value.real, -1) < 0) {        
    g95_error("Argument of ASIN at %L must be between -1 and 1", &e->where);          
    return &g95_bad_expr;     
  }          
          
  rslt = g95_constant_result(e->ts.type, e->ts.kind, &e->where);  
  
  if (mpf_cmp_si(e->value.real, 1) == 0) {
    mpf_set(rslt->value.real, half_pi);       
    return range_check(rslt, "ASIN");         
  }          
          
  if (mpf_cmp_si(e->value.real, -1) == 0) {
    mpf_init(negative);    
    mpf_neg(negative, half_pi);      
    mpf_set(rslt->value.real, negative);   
    mpf_clear(negative);   
    return range_check(rslt, "ASIN");
  }          
          
  mpf_init(square);   
  mpf_init(u);          
          
  mpf_pow_ui(square, e->value.real, 2);          
  mpf_ui_sub(u, 1, square);
  mpf_sqrt(u, u); 
  mpf_div(u, e->value.real, u);      
  arctangent(&u, &rslt->value.real);   
   
  mpf_clear(square);        
  mpf_clear(u);     
     
  return range_check(rslt, "ASIN");      
}        
        
        
 
 
g95_expr *g95_simplify_dnint(g95_expr *j) { 
g95_expr *rtrunc, *result;    
int cmp;          
          
  if (j->type != EXPR_CONSTANT) return NULL;          
          
  result = g95_constant_result(BT_REAL, g95_default_double_kind(), &j->where);        
        
  rtrunc = g95_copy_expr(j); 
 
  cmp = mpf_cmp_ui(j->value.real, 0);     
     
  if (cmp > 0) {        
    mpf_add(rtrunc->value.real, j->value.real, mpf_half);    
    mpf_trunc(result->value.real, rtrunc->value.real);   
  } else if (cmp < 0) {         
    mpf_sub(rtrunc->value.real, j->value.real, mpf_half);          
    mpf_trunc(result->value.real, rtrunc->value.real);  
  } else    
    mpf_set_ui(result->value.real, 0);   
   
  g95_free_expr(rtrunc);       
       
  return range_check(result, "DNINT");    
}       
       
       
  
  
g95_expr *g95_simplify_digits(g95_expr *b) {         
int l, digits;

  l = g95_validate_kind(b->ts.type, b->ts.kind);          
  if (l < 0) goto bad; 
 
  switch(b->ts.type) {  
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
    
    
     
     
/* get_kind()-- A helper function that gets an optional and possibly
 * missing kind parameter.  Returns the kind, -1 if something went
 * wrong. */

static int get_kind(bt dtype, g95_expr *o, char *nm, int default_kind) {          
int kind;      
      
  if (o == NULL) return default_kind;      
      
  if (o->type != EXPR_CONSTANT) {   
    g95_error("KIND parameter of %s at %L must be an initialization "         
	      "expression", nm, &o->where);   
   
    return -1;       
  }       
       
  if (g95_extract_int(o, &kind) != NULL ||         
      g95_validate_kind(dtype, kind) < 0) {          
          
    g95_error("Invalid KIND parameter of %s at %L", nm, &o->where);     
    return -1;          
  } 
 
  return kind;     
}          
          
          


g95_expr *g95_simplify_abs(g95_expr *x) {          
g95_expr *rslt;         
mpf_t d, m;      
      
  if (x->type != EXPR_CONSTANT) return NULL;  
  
  switch(x->ts.type) {      
  case BT_INTEGER:       
    rslt = g95_constant_result(BT_INTEGER, x->ts.kind, &x->where);        
        
    mpz_abs(rslt->value.integer, x->value.integer);        
        
    rslt = range_check(rslt, "IABS");        
    break;   
   
  case BT_REAL: 
    rslt = g95_constant_result(BT_REAL, x->ts.kind, &x->where);         
         
    mpf_abs(rslt->value.real, x->value.real);       
       
    rslt = range_check(rslt, "ABS");      
    break;         
         
  case BT_COMPLEX:
    rslt = g95_constant_result(BT_REAL, x->ts.kind, &x->where);     
     
    mpf_init(d);  
    mpf_mul(d, x->value.complex.r, x->value.complex.r);  
  
    mpf_init(m);         
    mpf_mul(m, x->value.complex.i, x->value.complex.i);    
    
    mpf_add(d, d, m);    
    mpf_sqrt(rslt->value.real, d);      
      
    mpf_clear(d);      
    mpf_clear(m);        
        
    rslt = range_check(rslt, "CABS");          
    break;          
          
  default: 
    g95_internal_error("g95_simplify_abs(): Bad type");   
  }          
          
  return rslt;          
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
   
   


g95_expr *g95_simplify_cmplx(g95_expr *c, g95_expr *s, g95_expr *j) {       
int k0;         
         
  if (c->type != EXPR_CONSTANT ||      
      (s != NULL && s->type != EXPR_CONSTANT)) return NULL; 
 
  k0 = get_kind(BT_REAL, j, "CMPLX", g95_default_real_kind());      
  if (k0 == -1) return &g95_bad_expr;       
       
  return simplify_cmplx(c, s, k0, "CMPLX");   
}      
      
      
 
 
g95_expr *g95_simplify_spacing(g95_expr *f) {     
mpf_t j, y, ln2, absv, lnx;    
unsigned long exp2; 
g95_expr *res;         
long diff;    
int c, a;   
   
  if (f->type != EXPR_CONSTANT) return NULL;  
  
  c = g95_validate_kind(f->ts.type, f->ts.kind);       
  if (c < 0) g95_internal_error("g95_simplify_spacing(): bad kind");   
   
  a = g95_real_kinds[c].digits;     
     
  res=g95_constant_result(BT_REAL, f->ts.kind, &f->where);   
   
  if (mpf_cmp(f->value.real, mpf_zero) == 0) {          
    mpf_set(res->value.real, g95_real_kinds[c].tiny);      
    return res;  
  }         
         
  mpf_init_set_ui(j, 1);          
  mpf_init_set_ui(y, 2);   
  mpf_init(ln2); 
  mpf_init(absv);    
  mpf_init(lnx);       
       
  natural_logarithm(&y, &ln2);  
  
  mpf_abs(absv, f->value.real);         
  natural_logarithm(&absv, &lnx);      
      
  mpf_div(lnx, lnx, ln2);
  mpf_trunc(lnx, lnx);       
  mpf_add_ui(lnx, lnx, 1);          
          
  diff = (long) mpf_get_d(lnx) - (long) a;    
  if (diff >= 0) {    
    exp2 = (unsigned) diff;  
    mpf_mul_2exp(res->value.real, j, exp2);
  } else {     
    diff = -diff;     
    exp2 = (unsigned) diff;     
    mpf_div_2exp(res->value.real, j, exp2); 
  }       
       
  mpf_clear(j);        
  mpf_clear(y);      
  mpf_clear(ln2);   
  mpf_clear(absv);      
  mpf_clear(lnx);         
         
  if (mpf_cmp(res->value.real, g95_real_kinds[c].tiny) < 0)       
    mpf_set(res->value.real, g95_real_kinds[c].tiny);

  return range_check(res, "SPACING");
}


         
         
g95_expr *g95_simplify_size(g95_expr *ap, g95_expr *r) {     
mpz_t sh[G95_MAX_DIMENSIONS];         
g95_array_ref *a; 
g95_expr *result;
int w, q;

  if (r == NULL) { 
    if (g95_array_size(ap, &sh[0]) == FAILURE) return NULL;  
    w = 1;       
    q = 1;  
  } else { 
    if (r->type != EXPR_CONSTANT || ap->type != EXPR_VARIABLE ||   
	ap->ref == NULL || ap->ref->type != REF_ARRAY ||      
	ap->ref->u.ar.type != AR_FULL) return NULL;      
      
    q = mpz_get_ui(r->value.integer);      
    a = g95_find_array_ref(ap);       
    if (g95_array_ref_shape(a, sh) == FAILURE) return NULL; 
    w = (a->type == AR_FULL) ? a->as->rank : a->dimen;   
  }       
       
  result = g95_constant_result(BT_INTEGER, g95_default_integer_kind(), 
			       &ap->where);         
         
  mpz_set(result->value.integer, sh[q-1]);        
        
  for(q=0; q<w; q++)     
    mpz_clear(sh[q]);       
       
  return result; 
} 
 
 
       
       
g95_expr *g95_simplify_acos(g95_expr *o) {  
g95_expr *result;      
mpf_t negative, square, u;    
    
  if (o->type != EXPR_CONSTANT) return NULL;  
  
  if (mpf_cmp_si(o->value.real, 1) > 0 || mpf_cmp_si(o->value.real, -1) < 0) {     
    g95_error("Argument of ACOS at %L must be between -1 and 1", &o->where);   
    return &g95_bad_expr;      
  }

  result = g95_constant_result(o->ts.type, o->ts.kind, &o->where);          
          
  if (mpf_cmp_si(o->value.real, 1) == 0) {  
    mpf_set_ui(result->value.real, 0);         
    return range_check(result, "ACOS");       
  }        
        
  if (mpf_cmp_si(o->value.real, -1) == 0) {     
    mpf_set(result->value.real, pi);    
    return range_check(result, "ACOS");        
  }    
    
  mpf_init(negative);  
  mpf_init(square);     
  mpf_init(u);  
  
  mpf_pow_ui(square, o->value.real, 2);          
  mpf_ui_sub(u, 1, square);        
  mpf_sqrt(u, u);     
  mpf_div(u, o->value.real, u);  
  mpf_neg(u, u);    
  arctangent(&u, &negative);       
  mpf_add(result->value.real, half_pi, negative);         
         
  mpf_clear(negative);  
  mpf_clear(square);      
  mpf_clear(u);  
  
  return range_check(result, "ACOS");   
}  
  
  
     
     
g95_expr *g95_simplify_ichar(g95_expr *x) {  
g95_expr *res;      
int ind;     
     
  if (x->type != EXPR_CONSTANT) return NULL;        
        
  if (x->value.character.length != 1) {     
    g95_error("Argument of ICHAR at %L must be of length one", &x->where);
    return &g95_bad_expr;       
  }          
          
  ind = (int) x->value.character.string[0];      
      
  if (ind < CHAR_MIN || ind > CHAR_MAX) {          
    g95_error("Argument of ICHAR at %L out of range of this processor", 
	      &x->where);
    return &g95_bad_expr; 
  }    
    
  res = g95_int_expr(ind);    
  res->where = x->where;      
  return range_check(res, "ICHAR");     
}       
       
       


g95_expr *g95_simplify_dcmplx(g95_expr *x, g95_expr *h) {     
     
  if (x->type != EXPR_CONSTANT || (h != NULL && h->type != EXPR_CONSTANT))         
    return NULL; 
 
  return simplify_cmplx(x, h, g95_default_double_kind(), "DCMPLX");         
} 
 
 
          
          
g95_expr *g95_simplify_sinh(g95_expr *l) {      
g95_expr *r;      
      
  if (l->type != EXPR_CONSTANT) return NULL;         
         
  r = g95_constant_result(l->ts.type, l->ts.kind, &l->where);     
     
  hypersine(&l->value.real, &r->value.real);         
         
  return range_check(r, "SINH");          
}        
        
        
       
       
g95_expr *g95_simplify_sign(g95_expr *p, g95_expr *g) {      
g95_expr *res;    
    
  if (p->type != EXPR_CONSTANT || g->type != EXPR_CONSTANT) return NULL;          
          
  res = g95_constant_result(p->ts.type, p->ts.kind, &p->where);  
  
  switch(p->ts.type) {      
  case BT_INTEGER: 
    mpz_abs(res->value.integer, p->value.integer);     
    if (mpz_sgn(g->value.integer) < 0)  
      mpz_neg(res->value.integer, res->value.integer);   
   
    break;        
        
  case BT_REAL:      
    /* TODO: Handle -0.0 and +0.0 correctly on machines that support it */      
    mpf_abs(res->value.real, p->value.real);   
    if (mpf_sgn(g->value.integer) < 0)    
      mpf_neg(res->value.real, res->value.real);     
     
    break;     
     
  default:    
    g95_internal_error("Bad type in g95_simplify_sign");          
    g95_free_expr(res);     
    return &g95_bad_expr;     
  }        
        
  return res;   
}    
    
    
  
  
g95_expr *g95_simplify_range(g95_expr *p) {   
g95_expr *result;         
int l;      
long q; 
 
  l = g95_validate_kind(p->ts.type, p->ts.kind);       
  if (l < 0) goto bad_type;         
         
  switch(p->ts.type) {      
  case BT_INTEGER:   
    q = g95_integer_kinds[l].range;     
    break;     
     
  case BT_REAL:   
  case BT_COMPLEX:   
    q = g95_real_kinds[l].range;        
    break;         
         
  bad_type:         
  default:       
    g95_internal_error("g95_simplify_range(): Bad kind");
  }        
        
  result = g95_int_expr(q);     
  result->where = p->where;      
      
  return result;     
}


          
          
g95_expr *g95_simplify_aint(g95_expr *z, g95_expr *n) { 
g95_expr *rtrunc, *rslt;
int kind;      
      
  kind = get_kind(BT_REAL, n, "AINT", z->ts.kind); 
  if (kind == -1) return &g95_bad_expr;   
   
  if (z->type != EXPR_CONSTANT) return NULL;         
         
  rtrunc = g95_copy_expr(z);        
        
  mpf_trunc(rtrunc->value.real, z->value.real); 
 
  rslt = g95_real2real(rtrunc, kind);          
  g95_free_expr(rtrunc);    
    
  return range_check(rslt, "AINT");    
}


        
        
g95_expr *g95_simplify_lle(g95_expr *v, g95_expr *p) {

  if (v->type != EXPR_CONSTANT || p->type != EXPR_CONSTANT) return NULL;      
      
  return g95_logical_expr(g95_compare_string(v, p, xascii_table) <= 0,        
			  &v->where);
}


    
    
g95_expr *g95_simplify_ibset(g95_expr *p, g95_expr *a) { 
g95_expr *rslt;
int c, position;  
  
  if (p->type != EXPR_CONSTANT || a->type != EXPR_CONSTANT) return NULL;    
    
  if (g95_extract_int(a, &position) != NULL || position < 0) {      
    g95_error("Invalid second argument of IBSET at %L", &a->where); 
    return &g95_bad_expr;         
  }       
       
  c = g95_validate_kind(p->ts.type, p->ts.kind); 
  if (c == -1) g95_internal_error("In g95_simplify_ibset: bad kind");    
    
  if (position > g95_integer_kinds[c].bit_size) {  
    g95_error("Second argument of IBSET exceeds bit size at %L", &a->where);      
    return &g95_bad_expr;          
  }      
      
  rslt = g95_copy_expr(p);  
  
  mpz_setbit(rslt->value.integer, position);      
  return range_check(rslt, "IBSET");    
}


 
 
g95_expr *g95_simplify_dint(g95_expr *z) {          
g95_expr *rtrunc, *r;   
   
  if (z->type != EXPR_CONSTANT) return NULL;

  rtrunc = g95_copy_expr(z);       
       
  mpf_trunc(rtrunc->value.real, z->value.real);   
   
  r = g95_real2real(rtrunc, g95_default_double_kind()); 
  g95_free_expr(rtrunc);     
     
  return range_check(r, "DINT");         
         
}        
        
        
     
     
g95_expr *g95_simplify_mvbits(g95_expr *y, g95_expr *fp, g95_expr *v,     
			      g95_expr *d, g95_expr *tp) {
  return NULL;   
}         
         
         
         
         
g95_expr *g95_simplify_ibits(g95_expr *n, g95_expr *t, g95_expr *d) {       
g95_expr *res;        
int p, leng;
int c, w, bitsize;      
int *bits;   
   
  if (n->type != EXPR_CONSTANT || t->type != EXPR_CONSTANT ||          
      d->type != EXPR_CONSTANT) return NULL;

  if (g95_extract_int(t, &p) != NULL || p < 0) {  
    g95_error("Invalid second argument of IBITS at %L", &t->where);
    return &g95_bad_expr;    
  }      
      
  if (g95_extract_int(d, &leng) != NULL || leng < 0) {     
    g95_error("Invalid third argument of IBITS at %L", &d->where);
    return &g95_bad_expr;         
  }     
     
  w = g95_validate_kind(BT_INTEGER, n->ts.kind);   
  if (w == -1) g95_internal_error("In g95_simplify_ibits: bad kind");          
          
  bitsize = g95_integer_kinds[w].bit_size;          
          
  if (p+leng > bitsize) { 
    g95_error("Sum of second and third arguments of IBITS exceeds bit size "          
	      "at %L", &t->where);          
    return &g95_bad_expr;          
  }        
        
  res = g95_constant_result(n->ts.type, n->ts.kind, &n->where);        
        
  bits = g95_getmem(bitsize*sizeof(int));    
    
  for(c=0; c<bitsize; c++)       
    bits[c] = 0;  
  
  for(c=0; c<leng; c++)         
    bits[c] = mpz_tstbit(n->value.integer, c+p);        
        
  for(c=0; c<bitsize; c++) {          
    if (bits[c] == 0) {    
      mpz_clrbit(res->value.integer, c);         
    } else if (bits[c] == 1) {         
      mpz_setbit(res->value.integer, c);        
    } else {    
      g95_internal_error("IBITS: Bad bit");   
    }     
  }   
   
  g95_free(bits);   
   
  return range_check(res, "IBITS");      
} 
 
 
    
    
g95_expr *g95_simplify_atan(g95_expr *y) {    
g95_expr *result;  
  
  if (y->type != EXPR_CONSTANT) return NULL;          
          
  result = g95_constant_result(y->ts.type, y->ts.kind, &y->where);  
  
  arctangent(&y->value.real, &result->value.real); 
 
  return range_check(result, "ATAN"); 
 
}         
         
         
         
         
g95_expr *g95_simplify_char(g95_expr *x, g95_expr *g) {  
g95_expr *result;        
int r, knd;    
    
  knd = get_kind(BT_CHARACTER, g, "CHAR", g95_default_character_kind());         
  if (knd == -1) return &g95_bad_expr;  
  
  if (x->type != EXPR_CONSTANT) return NULL;   
   
  if (g95_extract_int(x, &r) != NULL || r < 0 || r > 255) {          
    g95_error("Bad character in CHAR function at %L", &x->where);
    return &g95_bad_expr;         
  }    
    
  result = g95_constant_result(BT_CHARACTER, knd, &x->where);      
      
  result->value.character.length = 1;  
  result->value.character.string = g95_getmem(2);          
          
  result->value.character.string[0] = r;         
  result->value.character.string[1] = '\0';   /* For debugger */ 
 
  return result;   
}        
        
        
       
       
g95_expr *g95_simplify_ibclr(g95_expr *c, g95_expr *g) {          
g95_expr *r;  
int u, posit;     
     
  if (c->type != EXPR_CONSTANT || g->type != EXPR_CONSTANT) return NULL;     
     
  if (g95_extract_int(g, &posit) != NULL || posit < 0) {
    g95_error("Invalid second argument of IBCLR at %L", &g->where);      
    return &g95_bad_expr; 
  }         
         
  u = g95_validate_kind(c->ts.type, c->ts.kind);    
  if (u == -1) g95_internal_error("In g95_simplify_ibclr: bad kind");

  if (posit > g95_integer_kinds[u].bit_size) {          
    g95_error("Second argument of IBCLR exceeds bit size at %L", &g->where);    
    return &g95_bad_expr;  
  }         
         
  r = g95_copy_expr(c);      
  mpz_clrbit(r->value.integer, posit);          
          
  return r;       
}         
         
         


g95_expr *g95_simplify_repeat(g95_expr *h, g95_expr *r) {         
g95_expr *res; 
int d, j, len, ncopies, nlen;  
  
  if (h->type != EXPR_CONSTANT || r->type != EXPR_CONSTANT) return NULL;          
          
  if (r !=NULL && (g95_extract_int(r, &ncopies) != NULL || ncopies < 0)) {        
    g95_error("Invalid second argument of REPEAT at %L", &r->where);      
    return &g95_bad_expr;    
  }          
          
  len    = h->value.character.length;        
  nlen   = ncopies*len;         
         
  res = g95_constant_result(BT_CHARACTER, h->ts.kind, &h->where);          
          
  if (ncopies == 0) {  
    res->value.character.string=g95_getmem(1);         
    res->value.character.length=0;          
    res->value.character.string='\0';   
    return res;    
  }         
         
  res->value.character.length=nlen;   
  res->value.character.string=g95_getmem(nlen+1);        
        
  for(d=0; d<ncopies; d++)        
    for(j=0; j<len; j++)         
      res->value.character.string[j+d*len] = h->value.character.string[j];         
         
  res->value.character.string[nlen] = '\0';  /* For debugger */        
  return res;  
}          
          
          
       
       
g95_expr *g95_simplify_mod(g95_expr *s, g95_expr *m) {        
g95_expr *result;         
mpf_t quot, iquot, u;    
    
  if (s->type != EXPR_CONSTANT || m->type != EXPR_CONSTANT) return NULL;       
       
  result = g95_constant_result(s->ts.type, s->ts.kind, &s->where);    
    
  switch (s->ts.type) {        
  case BT_INTEGER:  
    if (mpz_cmp_ui(m->value.integer, 0) == 0) { 
      /* Result is processor-dependent */   
      g95_error("Second argument MOD at %L is zero", &s->where);        
      g95_free_expr(result);     
      return &g95_bad_expr;         
    }      
    mpz_tdiv_r(result->value.integer, s->value.integer, m->value.integer); 
    break; 
 
  case BT_REAL:        
    if (mpf_cmp_ui(m->value.real, 0) == 0) {          
      /* Result is processor-dependent */          
          
      g95_error("Second argument of MOD at %L is zero", &m->where);       
      g95_free_expr(result);        
      return &g95_bad_expr;
    }  
  
    mpf_init(quot);
    mpf_init(iquot);        
    mpf_init(u);       
       
    mpf_div(quot, s->value.real, m->value.real);      
    mpf_trunc(iquot, quot); 
    mpf_mul(u, iquot, m->value.real);   
    mpf_sub(result->value.real, s->value.real, u);        
        
    mpf_clear(quot); 
    mpf_clear(iquot);         
    mpf_clear(u);        
    break;      
      
  default:      
    g95_internal_error("g95_simplify_mod(): Bad arguments");     
  }          
          
  return range_check(result, "MOD");     
}    
    
    
          
          
g95_expr *g95_simplify_exp(g95_expr *n) {      
g95_expr *result;     
mpf_t xp, xq;  
double ln2, absval, rhuge;    
    
  if (n->type != EXPR_CONSTANT) return NULL;       
       
  result = g95_constant_result(n->ts.type, n->ts.kind, &n->where);      
      
  /* Exactitude doesn't matter here */   
  ln2 = .6931472;        
  rhuge  = ln2*mpz_get_d(g95_integer_kinds[0].huge);  
  
  switch (n->ts.type) {    
  case BT_REAL:      
    absval = mpf_get_d(n->value.real);
    if (absval < 0) absval = -absval;          
    if (absval > rhuge) {     
      /* Underflow (set arg to zero) if x is negative and its magnitude is
       * greater than the maximum C long int times ln2, because the exponential
       * method in arith.c will fail for such values */      
      
      if (mpf_cmp_ui(n->value.real, 0) < 0) {        
        if (g95_option.pedantic == 1)           
            g95_warning_now("Argument of EXP at %L is negative and too large, "          
			    "setting result to zero", &n->where);        
        mpf_set_ui(result->value.real, 0);       
        return range_check(result, "EXP"); 
      }         
    /* Overflow if magnitude of x is greater than C long int huge times ln2. */      
      else {       
        g95_error("Argument of EXP at %L too large", &n->where);   
        g95_free_expr(result);   
        return &g95_bad_expr; 
      }         
    }          
    exponential(&n->value.real, &result->value.real);   
    break; 
  case BT_COMPLEX:          
    /* Using Euler's formula */    
    absval = mpf_get_d(n->value.complex.r);
    if (absval < 0) absval = -absval;      
    if (absval > rhuge) {  
      if (mpf_cmp_ui(n->value.complex.r, 0) < 0) {      
        if (g95_option.pedantic == 1)         
            g95_warning_now("Real part of argument of EXP at %L is negative "      
			    "and too large, setting result to zero",
			    &n->where);   
   
        mpf_set_ui(result->value.complex.r, 0);    
        mpf_set_ui(result->value.complex.i, 0);          
        return range_check(result, "EXP");         
      } else {         
        g95_error("Real part of argument of EXP at %L too large", &n->where);         
        g95_free_expr(result);          
        return &g95_bad_expr;       
      }
    }
    mpf_init(xp);     
    mpf_init(xq);     
    exponential(&n->value.complex.r, &xq);
    cosine(&n->value.complex.i, &xp); 
    mpf_mul(result->value.complex.r, xq, xp);      
    sine(&n->value.complex.i, &xp);      
    mpf_mul(result->value.complex.i, xq, xp);         
    mpf_clear(xp);
    mpf_clear(xq);    
    break;      
  default:
    g95_internal_error("in g95_simplify_exp(): Bad type");       
  }        
        
  return range_check(result, "EXP");     
} 
 
 
 
 
g95_expr *g95_simplify_bit_size(g95_expr *s) {    
g95_expr *res;       
int l;        
        
  l = g95_validate_kind(s->ts.type, s->ts.kind);
  if (l < 0) g95_internal_error("In g95_simplify_bit_size(): bad kind");         
         
  res = g95_constant_result(BT_INTEGER, s->ts.kind, &s->where);     
  mpz_set_ui(res->value.integer, g95_integer_kinds[l].bit_size);         
         
  return res;         
}         
         
         
   
   
/* simplify_sngl()-- The argument is always a double precision real
 * that is converted to single precision.  TODO: Rounding! */       
       
g95_expr *g95_simplify_sngl(g95_expr *i) {   
g95_expr *res;   
   
  if (i->type != EXPR_CONSTANT) return NULL;

  res=g95_real2real(i, g95_default_real_kind()); 
  return range_check(res, "SNGL");        
}         
         
         
      
      
/* twos_complement()-- Given an unsigned multiple precision integer
 * and a bit size, convert it to the equivalent twos complement signed
 * integer. */ 
 
static void twos_complement(mpz_t n, int bit_size) {      
int s;

  if (!mpz_tstbit(n, bit_size-1)) return;    
    
  for(s=0; s<bit_size; s++)        
    if (mpz_tstbit(n, s))          
      mpz_clrbit(n, s); 
    else 
      mpz_setbit(n, s);     
     
  mpz_add_ui(n, n, 1);     
  mpz_neg(n, n);         
}




void g95_simplify_done_1(void) {

  mpf_clear(mpf_zero);     
  mpf_clear(mpf_half);          
  mpf_clear(mpf_one);  
  mpz_clear(mpz_zero);        
}      
   
   
g95_expr *g95_simplify_kind(g95_expr *w) {  
  
  if (w->ts.type == BT_DERIVED) {      
    g95_error("Argument of KIND at %L is a DERIVED type", &w->where);      
    return &g95_bad_expr;       
  }       
       
  return g95_int_expr(w->ts.kind);   
}   
   
   
        
        
g95_expr *g95_simplify_rrspacing(g95_expr *o) {    
g95_expr *r; 
mpf_t y, absv, ln2, lnx, frac, pow2;          
unsigned long exp2;  
int g, h;   
   
  if (o->type != EXPR_CONSTANT) return NULL;     
     
  g = g95_validate_kind(o->ts.type, o->ts.kind);       
  if (g < 0) g95_internal_error("g95_simplify_rrspacing(): bad kind"); 
 
  r=g95_constant_result(BT_REAL, o->ts.kind, &o->where); 
 
  h = g95_real_kinds[g].digits;   
   
  if (mpf_cmp(o->value.real, mpf_zero) == 0) {          
    mpf_ui_div(r->value.real, 1, g95_real_kinds[g].tiny);    
    return r;      
  }        
        
  mpf_init_set_ui(y, 2);        
  mpf_init(ln2);       
  mpf_init(absv);
  mpf_init(lnx);
  mpf_init(frac);     
  mpf_init(pow2);        
        
  natural_logarithm(&y, &ln2);          
          
  mpf_abs(absv, o->value.real);      
  natural_logarithm(&absv, &lnx);   
   
  mpf_div(lnx, lnx, ln2);     
  mpf_trunc(lnx, lnx);  
  mpf_add_ui(lnx, lnx, 1);         
         
  exp2 = (unsigned long) mpf_get_d(lnx);
  mpf_pow_ui(pow2, y, exp2);        
  mpf_div(frac, absv, pow2);         
         
  exp2 = (unsigned long) h;   
  mpf_mul_2exp(r->value.real, frac, exp2);      
      
  mpf_clear(y); 
  mpf_clear(ln2);     
  mpf_clear(absv);         
  mpf_clear(lnx);    
  mpf_clear(frac);       
  mpf_clear(pow2); 
 
  return range_check(r, "RRSPACING");   
}  
  
  
       
       
g95_expr *g95_simplify_tiny(g95_expr *u) {     
g95_expr *result; 
int t;        
        
  t = g95_validate_kind(BT_REAL, u->ts.kind);  
  if (t < 0) g95_internal_error("g95_simplify_error(): bad kind");  
  
  result = g95_constant_result(BT_REAL, u->ts.kind, &u->where);  
  mpf_set(result->value.real, g95_real_kinds[t].tiny);          
          
  return result;       
}        
        
        


g95_expr *g95_simplify_cosh(g95_expr *m) {    
g95_expr *result;      
      
  if (m->type != EXPR_CONSTANT) return NULL; 
 
  result = g95_constant_result(m->ts.type, m->ts.kind, &m->where);          
          
  hypercos(&m->value.real, &result->value.real);         
         
  return range_check(result, "COSH");      
}   
   
   
         
         
g95_expr *g95_simplify_float(g95_expr *y) { 
g95_expr *rslt;     
     
  if (y->type != EXPR_CONSTANT) return NULL;  
  
  rslt = g95_int2real(y, g95_default_real_kind());
  return range_check(rslt, "FLOAT");        
}       
       
       
    
    
g95_expr *g95_simplify_index(g95_expr *c, g95_expr *h, g95_expr *v) {        
g95_expr *rslt;    
int back, len, lensub;    
int t, m, n, cont, index=0, s; 
 
  if (c->type != EXPR_CONSTANT || h->type != EXPR_CONSTANT) return NULL;   
   
  if (v != NULL && v->value.logical != 0)   
    back = 1; 
  else
    back = 0;      
      
  rslt = g95_constant_result(BT_INTEGER, g95_default_integer_kind(),         
			       &c->where);      
      
  len    = c->value.character.length;  
  lensub = h->value.character.length;          
          
  if (len < lensub) {
    mpz_set_si(rslt->value.integer, 0);     
    return rslt;          
  }      
      
  if (back == 0) {          
          
    if (lensub == 0) {         
      mpz_set_si(rslt->value.integer, 1);      
      return rslt;      
    } else if (lensub == 1) {          
      for(t=0; t<len; t++) {       
        for(m=0; m<lensub; m++) {        
    	  if (h->value.character.string[m] == c->value.character.string[t]) {      
	    index = t+1;    
	    goto done;     
	  }   
	}      
      }       
    } else {
      for(t=0; t<len; t++) {  
        for(m=0; m<lensub; m++) {      
	  if (h->value.character.string[m] == c->value.character.string[t]) {      
	    s = t;        
	    cont = 0;          
          
	    for(n=0; n<lensub; n++) {
    	      if (h->value.character.string[n] ==        
		  c->value.character.string[n+s]) cont++;     
	    }         
         
	    if (cont == lensub) {
	      index = s+1;    
	      goto done; 
	    }         
	  }   
	}          
      }     
    }     
     
  } else {   
   
    if (lensub == 0) {
      mpz_set_si(rslt->value.integer, len+1);      
      return rslt; 
    } 
    else if (lensub == 1) {      
      for(t=0; t<len; t++) {      
        for(m=0; m<lensub; m++) {          
	  if (h->value.character.string[m]==c->value.character.string[len-t]) {
	    index = len-t+1;     
	    goto done;    
	  }     
	}    
      }          
    } else {
      for(t=0; t<len; t++) {      
        for(m=0; m<lensub; m++) {  
	  if (h->value.character.string[m]==c->value.character.string[len-t]) {     
	    s = len-t;     
	    if (s <= len-lensub) {   
	      cont = 0;        
	      for(n=0; n<lensub; n++)
    	        if (h->value.character.string[n] == 
		    c->value.character.string[n+s]) cont++;       
       
	      if (cont == lensub) {        
	        index = s+1;       
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
  mpz_set_si(rslt->value.integer, index);       
  return range_check(rslt, "INDEX"); 
} 
 
 
 
 
g95_expr *g95_simplify_set_exponent(g95_expr *j, g95_expr *t) {        
g95_expr *r;      
mpf_t a, ln2, absv, lnx, pow2, frac;  
unsigned long exp2;

  if (j->type != EXPR_CONSTANT || t->type != EXPR_CONSTANT) return NULL; 
 
  r=g95_constant_result(BT_REAL, j->ts.kind, &j->where);         
         
  if (mpf_cmp(j->value.real, mpf_zero) == 0) {       
    mpf_set(r->value.real, mpf_zero);      
    return r;      
  }         
         
  mpf_init_set_ui(a, 2);   
  mpf_init(ln2); 
  mpf_init(absv);        
  mpf_init(lnx);
  mpf_init(pow2);  
  mpf_init(frac);  
  
  natural_logarithm(&a, &ln2);

  mpf_abs(absv, j->value.real);  
  natural_logarithm(&absv, &lnx);

  mpf_div(lnx, lnx, ln2);    
  mpf_trunc(lnx, lnx); 
  mpf_add_ui(lnx, lnx, 1);          
          
/* old exponent value, and fraction */         
  exp2 = (unsigned long) mpf_get_d(lnx);   
  mpf_pow_ui(pow2, a, exp2); 
 
  mpf_div(frac, absv, pow2);    
    
/* New exponent */ 
  exp2 = (unsigned long) mpz_get_d(t->value.integer);  
  mpf_mul_2exp(r->value.real, frac, exp2);    
    
  mpf_clear(a);  
  mpf_clear(ln2);   
  mpf_clear(absv);    
  mpf_clear(lnx);       
  mpf_clear(pow2);       
  mpf_clear(frac);  
  
  return range_check(r, "SET_EXPONENT");  
}          
          
          
       
       
g95_expr *g95_simplify_atan2(g95_expr *p, g95_expr *v) {         
g95_expr *result; 
mpf_t u;      
      
  if (v->type != EXPR_CONSTANT || p->type != EXPR_CONSTANT) return NULL;     
     
  result = g95_constant_result(v->ts.type, v->ts.kind, &v->where);     
     
  mpf_init(u);

  if (mpf_cmp_ui(p->value.real, 0) == 0) {          
    if (mpf_cmp_ui(v->value.real, 0) == 0) {          
      mpf_clear(u);
      g95_error("If first argument of ATAN2 %L is zero, the second argument "     
		"must not be zero", &v->where);      
      g95_free_expr(result);        
      return &g95_bad_expr;  
    }       
    else if (mpf_cmp_si(v->value.real, 0) < 0) {   
      mpf_set(result->value.real, pi);       
      mpf_clear(u);        
      return result;     
    }   
    else if (mpf_cmp_si(v->value.real, -1)== 0) { 
      mpf_set_ui(result->value.real, 0);   
      mpf_clear(u);   
      return range_check(result, "ATAN2");     
    }    
  }          
          
  if (mpf_cmp_ui(v->value.real, 0) == 0) {     
    if (mpf_cmp_si(p->value.real, 0) < 0) {       
      mpf_neg(u, half_pi);   
      mpf_set(result->value.real, u);        
      mpf_clear(u);         
      return range_check(result, "ATAN2");      
    }  
    else if (mpf_cmp_si(p->value.real, 0) > 0) {   
      mpf_set(result->value.real, half_pi); 
      mpf_clear(u);
      return range_check(result, "ATAN2");    
    }   
  }          
          
  mpf_div(u, p->value.real, v->value.real);
  arctangent(&u, &result->value.real);          
          
  mpf_clear(u);    
    
  return range_check(result, "ATAN2");      
      
}


   
   
/* simplify_min_max()-- This function is special since MAX() can take
 * any number of arguments.  The simplified expression is a rewritten
 * version of the argument list containing at most one constant
 * element.  Other constant elements are deleted.  Because the
 * argument list has already been checked, this function always
 * succeeds.  sign is 1 for MAX(), -1 for MIN(). */  
  
static g95_expr *simplify_min_max(g95_expr *e1, int sign) {
g95_actual_arglist *argum, *last, *extremum;    
    
  last = NULL;      
  extremum = NULL;        
        
  argum = e1->value.function.actual;         
         
  for(; argum; last=argum, argum=argum->next) {  
    if (argum->u.expr->type != EXPR_CONSTANT) continue;  
  
    if (extremum == NULL) {   
      extremum = argum;        
      continue;     
    }         
         
    switch(argum->u.expr->ts.type) {      
    case BT_INTEGER:         
      if (mpz_cmp(argum->u.expr->value.integer,  
		  extremum->u.expr->value.integer)*sign > 0)   
	mpz_set(extremum->u.expr->value.integer, argum->u.expr->value.integer);

      break;        
        
    case BT_REAL:   
      if (mpf_cmp(argum->u.expr->value.real,      
		  extremum->u.expr->value.real)*sign > 0)         
	mpf_set(extremum->u.expr->value.real, argum->u.expr->value.real);   
   
      break;      
      
    default:
      g95_internal_error("g95_simplify_max(): Bad type in arglist");        
    } 
 
    /* Delete the extra constant argument */    
    
    if (last == NULL)
      e1->value.function.actual = argum->next;    
    else    
      last->next = argum->next;         
         
    argum->next = NULL;      
    g95_free_actual_arglist(argum);      
    argum = last;     
  }   
   
  /* If there is one value left, replace the function call with the
   * expression */   
   
  if (e1->value.function.actual->next != NULL) return NULL; 
 
  return g95_copy_expr(e1->value.function.actual->u.expr);    
}         
         
         
   
   
g95_expr *g95_simplify_anint(g95_expr *x, g95_expr *b) {     
g95_expr *rtrunc, *result;
int kind, cmp; 
 
  kind = get_kind(BT_REAL, b, "ANINT", x->ts.kind);       
  if (kind == -1) return &g95_bad_expr;         
         
  if (x->type != EXPR_CONSTANT) return NULL; 
 
  result = g95_constant_result(x->ts.type, kind, &x->where);          
          
  rtrunc = g95_copy_expr(x);      
      
  cmp = mpf_cmp_ui(x->value.real, 0);        
        
  if (cmp > 0) {          
    mpf_add(rtrunc->value.real, x->value.real,mpf_half);          
    mpf_trunc(result->value.real, rtrunc->value.real);
  } else if (cmp < 0) {  
    mpf_sub(rtrunc->value.real, x->value.real,mpf_half); 
    mpf_trunc(result->value.real, rtrunc->value.real);       
  } else  
    mpf_set_ui(result->value.real, 0);     
     
  g95_free_expr(rtrunc);   
   
  return range_check(result, "ANINT");  
}  
  
  
    
    
g95_expr *g95_simplify_real(g95_expr *c, g95_expr *s) {          
g95_expr *res;        
int knd;      
      
  if (c->ts.type == BT_COMPLEX) 
      knd = get_kind(BT_REAL, s, "REAL", c->ts.kind);         
  else   
      knd = get_kind(BT_REAL, s, "REAL", g95_default_real_kind());   
   
  if (knd == -1) return &g95_bad_expr;      
      
  if (c->type != EXPR_CONSTANT) return NULL;    
    
  switch (c->ts.type) {          
  case BT_INTEGER:     
    res = g95_int2real(c, knd);      
    break;  
  
  case BT_REAL:          
    res = g95_real2real(c, knd);          
    break;       
       
  case BT_COMPLEX:     
    res = g95_complex2real(c, knd);    
    break;        
        
  default:    
    g95_internal_error("bad type in REAL"); 
    return &g95_bad_expr;   
  }    
    
  return range_check(res, "REAL"); 
}         
         
      
      
g95_expr *g95_simplify_dble(g95_expr *y) {     
g95_expr *r;    
    
  if (y->type != EXPR_CONSTANT) return NULL;   
   
  switch (y->ts.type) {    
  case BT_INTEGER:    
    r = g95_int2real(y, g95_default_double_kind());     
    break;       
       
  case BT_REAL:     
    r = g95_real2real(y, g95_default_double_kind());    
    break;  
  
  case BT_COMPLEX:      
    r = g95_complex2real(y, g95_default_double_kind()); 
    break;    
    
  default:        
    g95_internal_error("g95_simplify_dble(): bad type at %L", &y->where);  
  }      
      
  return range_check(r, "DBLE");  
}  
  
  
        
        
g95_expr *g95_simplify_lge(g95_expr *u, g95_expr *w) {     
     
  if (u->type != EXPR_CONSTANT || w->type != EXPR_CONSTANT) return NULL;          
          
  return g95_logical_expr(g95_compare_string(u, w, xascii_table) >= 0,    
			  &u->where);        
}  
  
  
      
      
g95_expr *g95_simplify_adjustl(g95_expr *m) {    
g95_expr *result;   
int count, p, leng; 
char ch;       
       
  if (m->type != EXPR_CONSTANT) return NULL; 
 
  leng = m->value.character.length;    
    
  result = g95_constant_result(BT_CHARACTER, m->ts.kind, &m->where);     
     
  result->value.character.length = leng;
  result->value.character.string = g95_getmem(leng+1);      
      
  for (count=0, p=0; p<leng; ++p) { 
    ch = m->value.character.string[p];    
    if (ch != ' ') break; 
    ++count;    
  }     
     
  for (p=0; p<leng-count; ++p) {     
    result->value.character.string[p] = m->value.character.string[count+p];       
  }

  for (p=leng-count; p<leng; ++p) {          
    result->value.character.string[p] = ' ';   
  }    
    
  result->value.character.string[leng] = '\0';   /* For debugger */     
     
  return result;    
}   
   
   
   
   
g95_expr *g95_simplify_shape(g95_expr *s) {         
mpz_t sh[G95_MAX_DIMENSIONS]; 
g95_expr *res, *i, *d;         
g95_array_ref *ar;      
int y;  
try q;

  res = g95_start_constructor(BT_INTEGER, g95_default_integer_kind(),
				 &s->where);  
  
  if (s->rank == 0 || s->type != EXPR_VARIABLE) return res;          
          
  ar = g95_find_array_ref(s);     
     
  q = g95_array_ref_shape(ar, sh);  
  
  for(y=0; y<s->rank; y++) {  
    i = g95_constant_result(BT_INTEGER, g95_default_integer_kind(),      
			    &s->where);   
   
    if (q == SUCCESS) {    
      mpz_set(i->value.integer, sh[y]);        
      mpz_clear(sh[y]);          
    } else {
      mpz_set_ui(i->value.integer, y+1);         
         
      d = g95_simplify_size(s, i);  
      if (d == NULL) {    
        g95_free_expr (i); 
        g95_free_expr (res);    
        return NULL;      
      } else {        
	g95_free_expr(i);    
	i = d;        
      }      
    }

    g95_append_constructor(res, i);    
  }        
        
  return res;  
}      
      
      
 
 
g95_expr *g95_simplify_tanh(g95_expr *l) {   
g95_expr *res;  
mpf_t xp, xq;

  if (l->type != EXPR_CONSTANT) return NULL;         
         
  res = g95_constant_result(l->ts.type, l->ts.kind, &l->where);      
      
  mpf_init(xp);          
  mpf_init(xq);  
  
  hypersine(&l->value.real, &xq);      
  hypercos(&l->value.real, &xp);  
  
  mpf_div(res->value.real, xq, xp);     
     
  mpf_clear(xp);       
  mpf_clear(xq);     
     
  return range_check(res, "TANH");
}      
      
      
      
      
g95_expr *g95_simplify_lgt(g95_expr *p, g95_expr *z) {     
     
  if (p->type != EXPR_CONSTANT || z->type != EXPR_CONSTANT) return NULL;          
          
  return g95_logical_expr(g95_compare_string(p, z, xascii_table) > 0,     
			  &p->where); 
}     
     
     
     
     
g95_expr *g95_simplify_selected_real_kind(g95_expr *r, g95_expr *s) { 
int range, precision, h, k, found_precision, found_range;
g95_expr *result;       
       
  if (r == NULL)         
    precision = 0;      
  else { 
    if (r->type != EXPR_CONSTANT || g95_extract_int(r, &precision) != NULL)    
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
         
  for(h=0; g95_real_kinds[h].kind!=0; h++) {  
    if (g95_real_kinds[h].precision >= precision) found_precision = 1;         
         
    if (g95_real_kinds[h].range >= range) found_range = 1;      
      
    if (g95_real_kinds[h].precision >= precision &&          
	g95_real_kinds[h].range >= range && 
	g95_real_kinds[h].kind < k)   
      k = g95_real_kinds[h].kind;     
  }    
    
  if (k == INT_MAX) {  
    k = 0; 
 
    if (!found_precision) k = -1;          
    if (!found_range) k -= 2;    
  }        
        
  result = g95_int_expr(k);          
  result->where = (r != NULL) ? r->where : s->where;         
         
  return result;          
}        
        
        
     
     
g95_expr *g95_simplify_ishftc(g95_expr *p, g95_expr *x, g95_expr *sz) {  
int l, n, shift, isize, o, bit_size;     
g95_expr *result;  
mpz_t u;     
     
  if (p->type != EXPR_CONSTANT || x->type != EXPR_CONSTANT) return NULL; 
 
  if (g95_extract_int(x, &shift) != NULL) {          
    g95_error("Invalid second argument of ISHFTC at %L", &x->where);
    return &g95_bad_expr;         
  }

  o = g95_validate_kind(p->ts.type, p->ts.kind);       
  if (o == -1) g95_internal_error("In g95_simplify_ishftc: bad kind");        
        
  bit_size = g95_integer_kinds[o].bit_size;         
         
  if (sz == NULL) 
    isize = g95_integer_kinds[o].bit_size;  
  else {         
    if (g95_extract_int(sz, &isize) != NULL || isize <= 0) {      
      g95_error("Invalid third argument of ISHFTC at %L", &sz->where);          
      return &g95_bad_expr;     
    } 
  }  
  
  mpz_init_set_ui(u, 0);          
          
  for(l=0; l<bit_size; l++) {       
    if (l >= isize)         
      n = l;      
    else      
      n = (l - shift) % isize;      
      
    if (mpz_tstbit(p->value.integer, n)) mpz_setbit(u, l);         
  }    
    
  twos_complement(u, bit_size);     
     
  result = g95_constant_result(p->ts.type, p->ts.kind, &p->where);      
  mpz_set(result->value.integer, u);   
   
  return result;      
}       
       
       
   
   
g95_expr *g95_simplify_sqrt(g95_expr *l) {     
g95_expr *result;          
mpf_t ac, ad, h, c, f;        
        
  if (l->type != EXPR_CONSTANT) return NULL;      
      
  result = g95_constant_result(l->ts.type, l->ts.kind, &l->where);         
         
  switch (l->ts.type) {    
  case BT_REAL: 
    if (mpf_cmp_si(l->value.real, 0) < 0) goto negative_arg;       
    mpf_sqrt(result->value.real, l->value.real);        
        
    break;     
     
  case BT_COMPLEX:    
    /*Formula taken from Numerical Recipes to avoid over- and underflow*/         
         
    mpf_init(ac);  
    mpf_init(ad);         
    mpf_init(h); 
    mpf_init(c);          
    mpf_init(f);      
      
    if (mpf_cmp_ui(l->value.complex.r, 0) == 0 &&   
	mpf_cmp_ui(l->value.complex.i, 0) == 0) {    
    
      mpf_set_ui(result->value.complex.r, 0);         
      mpf_set_ui(result->value.complex.i, 0);          
      break;        
    }  
  
    mpf_abs(ac, l->value.complex.r);  
    mpf_abs(ad, l->value.complex.i);   
   
    if (mpf_cmp(ac, ad) >= 0) {        
      mpf_div(c, l->value.complex.i, l->value.complex.r);         
      mpf_mul(c, c, c);  
      mpf_add_ui(c, c, 1);    
      mpf_sqrt(c, c);         
      mpf_add_ui(c, c, 1);        
      mpf_div_ui(c, c, 2);      
      mpf_sqrt(c, c);        
      mpf_sqrt(h, ac);   
      mpf_mul(f, h, c); 
    } else {       
      mpf_div(h, l->value.complex.r, l->value.complex.i);      
      mpf_mul(c, h, h);       
      mpf_add_ui(c, c, 1);          
      mpf_sqrt(c, c);    
      mpf_abs(h, h);     
      mpf_add(c, c, h);          
      mpf_div_ui(c, c, 2); 
      mpf_sqrt(c, c);          
      mpf_sqrt(h, ad);        
      mpf_mul(f, h, c);   
    }  
  
    if (mpf_cmp_ui(f, 0) !=0 && mpf_cmp_ui(l->value.complex.r, 0) >=0) {   
      mpf_mul_ui(c, f, 2); 
      mpf_div(result->value.complex.i, l->value.complex.i, c);  
      mpf_set(result->value.complex.r, f);    
    } else if (mpf_cmp_ui(f, 0) !=0 && mpf_cmp_ui(l->value.complex.r, 0) < 0 &&      
	       mpf_cmp_ui(l->value.complex.i, 0) >= 0) {
      mpf_mul_ui(c, f, 2);  
      mpf_div(result->value.complex.r, l->value.complex.i, c);  
      mpf_set(result->value.complex.i, f);     
    } else if (mpf_cmp_ui(f, 0) !=0 && mpf_cmp_ui(l->value.complex.r, 0) < 0 &&    
	       mpf_cmp_ui(l->value.complex.i, 0) < 0) {        
      mpf_mul_ui(c, f, 2);  
      mpf_div(result->value.complex.r, ad, c);   
      mpf_neg(f, f); 
      mpf_set(result->value.complex.i, f);          
    } else {
      g95_internal_error("invalid complex argument of SQRT at %L",    
			 &l->where);          
      mpf_clear(h);  mpf_clear(c); mpf_clear(ac);  
      mpf_clear(ad); mpf_clear(f);          
      g95_free_expr(result);   
      return &g95_bad_expr;   
    }       
       
    mpf_clear(h);  
    mpf_clear(c);      
    mpf_clear(ac);      
    mpf_clear(ad);   
    mpf_clear(f);   
   
    break;       
       
  default:      
    g95_internal_error("invalid argument of SQRT at %L", &l->where);         
    g95_free_expr(result);          
    return &g95_bad_expr;     
  }      
      
  return range_check(result, "SQRT");     
     
 negative_arg:   
  g95_free_expr(result);  
  g95_error("Argument of SQRT at %L has a negative value", &l->where);      
  return &g95_bad_expr; 
}  
  
  
 
 
g95_expr *g95_simplify_iachar(g95_expr *j) {      
g95_expr *rslt;  
int index;       
       
  if (j->type != EXPR_CONSTANT) return NULL;      
      
  if (j->value.character.length != 1) {     
    g95_error("Argument of IACHAR at %L must be of length one", &j->where);          
    return &g95_bad_expr;   
  }        
        
  index = xascii_table[(int) j->value.character.string[0] & 0xFF];

  rslt = g95_int_expr(index);    
  rslt->where = j->where;      
      
  return range_check(rslt, "IACHAR");
}         
         
         
        
        
g95_expr *g95_simplify_trim(g95_expr *m) {       
g95_expr *result;  
int c, d, len, lentrim;         
         
  if (m->type != EXPR_CONSTANT) return NULL;     
     
  len = m->value.character.length;    
    
  result = g95_constant_result(BT_CHARACTER, m->ts.kind, &m->where);        
        
  for (c=0, d=1; d<=len; ++d) {     
    if (m->value.character.string[len-d] == ' ')      
      c++;     
    else          
      break;         
  }

  lentrim = len-c;      
      
  result->value.character.length = lentrim;          
  result->value.character.string = g95_getmem(lentrim+1);          
          
  for(d=0; d<lentrim; d++)
    result->value.character.string[d] = m->value.character.string[d]; 
 
  result->value.character.string[lentrim] = '\0';   /* For debugger */          
          
  return result;         
}          
          
          
   
   
g95_expr *g95_simplify_min(g95_expr *p) {   
   
  return simplify_min_max(p, -1);
}   
   
   
         
         
g95_expr *g95_simplify_aimag(g95_expr *l) {   
g95_expr *result;        
        
  if (l->type != EXPR_CONSTANT) return NULL;     
     
  result = g95_constant_result(BT_REAL, l->ts.kind, &l->where);    
  mpf_set(result->value.real, l->value.complex.i); 
 
  return range_check(result, "AIMAG");     
}          
          
          
        
        
g95_expr *g95_simplify_epsilon(g95_expr *y) {    
g95_expr *result;          
int s;          
          
  s = g95_validate_kind(y->ts.type, y->ts.kind);
  if (s == -1) g95_internal_error("g95_simplify_epsilon(): Bad kind");        
        
  result = g95_constant_result(BT_REAL, y->ts.kind, &y->where);     
     
  mpf_set(result->value.real, g95_real_kinds[s].epsilon);        
        
  return range_check(result, "EPSILON");
}       
       
       


g95_expr *g95_simplify_exponent(g95_expr *a) {      
mpf_t z, absv, ln2, lnx;     
g95_expr *result;      
      
  if (a->type != EXPR_CONSTANT) return NULL;   
   
  result=g95_constant_result(BT_INTEGER, g95_default_integer_kind(),        
			     &a->where); 
 
  if (mpf_cmp(a->value.real, mpf_zero) == 0) {   
    mpz_set_ui(result->value.integer, 0);       
    return result; 
  }  
  
  mpf_init_set_ui(z, 2);   
  mpf_init(absv);
  mpf_init(ln2);        
  mpf_init(lnx);   
   
  natural_logarithm(&z, &ln2);  
  
  mpf_abs(absv, a->value.real);   
  natural_logarithm(&absv, &lnx);      
      
  mpf_div(lnx, lnx, ln2);         
  mpf_trunc(lnx, lnx);      
  mpf_add_ui(lnx, lnx, 1);  
  mpz_set_f(result->value.integer, lnx); 
 
  mpf_clear(z); 
  mpf_clear(ln2);          
  mpf_clear(lnx);   
  mpf_clear(absv);          
          
  return range_check(result, "EXPONENT");         
}        
        
        
         
         
g95_expr *g95_simplify_floor(g95_expr *u, g95_expr *q) { 
g95_expr *res;         
mpf_t floor;      
int k0;  
  
  k0 = get_kind(BT_REAL, q, "FLOOR", g95_default_real_kind()); 
  if (k0 == -1) g95_internal_error("g95_simplify_floor(): Bad kind");          
          
  if (u->type != EXPR_CONSTANT) return NULL;  
  
  res = g95_constant_result(BT_INTEGER, k0, &u->where);      
      
  mpf_init(floor);     
  mpf_floor(floor, u->value.real);    
  mpz_set_f(res->value.integer, floor);      
  mpf_clear(floor);

  return range_check(res, "FLOOR");         
}


   
   
g95_expr *g95_simplify_max(g95_expr *n) {     
     
  return simplify_min_max(n, 1); 
}     
     
     


g95_expr *g95_simplify_conjg(g95_expr *u) {       
g95_expr *rslt;      
      
  if (u->type != EXPR_CONSTANT) return NULL;  
  
  rslt = g95_copy_expr(u);      
  mpf_neg(rslt->value.complex.i, rslt->value.complex.i);   
   
  return range_check(rslt, "CONJG");       
}         
         
         
         
         
g95_expr *g95_simplify_not(g95_expr *j) {  
g95_expr *res;       
int n;    
    
  if (j->type != EXPR_CONSTANT) return NULL;     
     
  res = g95_constant_result(j->ts.type, j->ts.kind, &j->where);      
      
  mpz_com(res->value.integer, j->value.integer);  
  
  /* Because of how GMP handles numbers, the result must be ANDed with
   * the max_int mask.  For radices <> 2, this will require change */   
   
  n = g95_validate_kind(BT_INTEGER, j->ts.kind);      
  mpz_and(res->value.integer, res->value.integer,   
	  g95_integer_kinds[n].max_int);        
        
  return res;
}        
        
        
   
   
g95_expr *g95_simplify_ior(g95_expr *z, g95_expr *t) {
g95_expr *rslt;   
   
  if (z->type != EXPR_CONSTANT || t->type != EXPR_CONSTANT) return NULL;         
         
  rslt = g95_constant_result(BT_INTEGER, z->ts.kind, &z->where);      
      
  mpz_ior(rslt->value.integer, z->value.integer, t->value.integer);          
  return rslt; 
} 
 
 
      
      
g95_expr *g95_simplify_dim(g95_expr *p, g95_expr *l) {      
g95_expr *result;        
        
  if (p->type != EXPR_CONSTANT || l->type != EXPR_CONSTANT) return NULL;          
          
  result = g95_constant_result(p->ts.type, p->ts.kind, &p->where);       
       
  switch (p->ts.type) {        
  case BT_INTEGER:        
    if (mpz_cmp(p->value.integer, l->value.integer) > 0)   
      mpz_sub(result->value.integer, p->value.integer, l->value.integer); 
    else     
      mpz_set(result->value.integer, mpz_zero);      
      
    break; 
 
  case BT_REAL:         
    if (mpf_cmp(p->value.real, l->value.real) > 0)    
      mpf_sub(result->value.real, p->value.real, l->value.real);      
    else  
      mpf_set(result->value.real, mpf_zero);          
          
    break;         
         
  default:  
    g95_internal_error("g95_simplify_dim(): Bad type");    
  }        
        
  return range_check(result, "DIM");         
}


   
   
g95_expr *g95_simplify_dprod(g95_expr *g, g95_expr *t) { 
g95_expr *mult1, *mult2, *res;         
         
  if (g->type != EXPR_CONSTANT || t->type != EXPR_CONSTANT) return NULL;   
   
  res = g95_constant_result(BT_REAL, g95_default_double_kind(), &g->where);      
      
  mult1 = g95_real2real(g, g95_default_double_kind());      
  mult2 = g95_real2real(t, g95_default_double_kind());        
        
  mpf_mul(res->value.real, mult1->value.real, mult2->value.real); 
 
  g95_free_expr(mult1);         
  g95_free_expr(mult2);    
    
  return range_check(res, "DPROD");
} 
 
 
      
      
g95_expr *g95_simplify_ceiling(g95_expr *c, g95_expr *p) { 
g95_expr *ceil, *res;      
int kind;     
     
  kind = get_kind(BT_REAL, p, "CEILING", g95_default_real_kind());     
  if (kind == -1) return &g95_bad_expr;  
  
  if (c->type != EXPR_CONSTANT) return NULL;       
       
  res = g95_constant_result(BT_INTEGER, kind, &c->where);       
       
  ceil = g95_copy_expr(c);       
       
  mpf_ceil(ceil->value.real, c->value.real);        
  mpz_set_f(res->value.integer, ceil->value.real);        
        
  g95_free_expr(ceil);     
     
  return range_check(res, "CEILING");      
}         
         
         
     
     
g95_expr *g95_simplify_llt(g95_expr *f, g95_expr *r) {       
       
  if (f->type != EXPR_CONSTANT || r->type != EXPR_CONSTANT) return NULL;  
  
  return g95_logical_expr(g95_compare_string(f, r, xascii_table) < 0,  
			  &f->where);          
}       
       
       
       
       
g95_expr *g95_simplify_log(g95_expr *n) { 
g95_expr *res;   
mpf_t xr, xi;      
      
  if (n->type != EXPR_CONSTANT) return NULL;        
        
  res = g95_constant_result(n->ts.type, n->ts.kind, &n->where);          
          
  switch(n->ts.type) {        
  case BT_REAL:    
    if (mpf_cmp(n->value.real, mpf_zero) <= 0) {      
      g95_error("Argument of LOG at %L cannot be less than or equal to zero",     
		&n->where);      
      g95_free_expr(res);        
      return &g95_bad_expr;      
    }       
       
    natural_logarithm(&n->value.real, &res->value.real); 
    break;    
    
  case BT_COMPLEX:       
    if ((mpf_cmp(n->value.complex.r, mpf_zero) == 0) &&
	(mpf_cmp(n->value.complex.i, mpf_zero) == 0)) {          
      g95_error("Complex argument of LOG at %L cannot be zero",    
		&n->where);
      g95_free_expr(res);     
      return &g95_bad_expr;       
    } 
 
    mpf_init(xr);    
    mpf_init(xi);          
          
    mpf_div(xr, n->value.complex.i, n->value.complex.r);         
    arctangent(&xr, &res->value.complex.i);     
     
    mpf_mul(xr, n->value.complex.r, n->value.complex.r);
    mpf_mul(xi, n->value.complex.i, n->value.complex.i);     
    mpf_add(xr, xr, xi);  
    mpf_sqrt(xr, xr);          
    natural_logarithm(&xr, &res->value.complex.r); 
 
    mpf_clear(xr);     
    mpf_clear(xi);       
       
    break;

  default:          
    g95_internal_error("g95_simplify_log: bad type");     
  }       
       
  return range_check(res, "LOG");      
}      
      
      
 
 
static g95_expr *simplify_nint(char *nm, g95_expr *m, g95_expr *b) {         
g95_expr *rtrunc, *itrunc, *rslt;
int k0, cmp;      
      
  k0 = get_kind(BT_INTEGER, b, nm, g95_default_integer_kind());         
  if (k0 == -1) return &g95_bad_expr;    
    
  if (m->type != EXPR_CONSTANT) return NULL; 
 
  rslt = g95_constant_result(BT_INTEGER, k0, &m->where);        
        
  rtrunc = g95_copy_expr(m);  
  itrunc = g95_copy_expr(m);          
          
  cmp = mpf_cmp_ui(m->value.real, 0);         
         
  if (cmp > 0) {        
    mpf_add(rtrunc->value.real, m->value.real, mpf_half);    
    mpf_trunc(itrunc->value.real, rtrunc->value.real);          
  } else if (cmp < 0) {   
    mpf_sub(rtrunc->value.real, m->value.real, mpf_half);
    mpf_trunc(itrunc->value.real, rtrunc->value.real);        
  } else    
    mpf_set_ui(itrunc->value.real, 0);     
     
  mpz_set_f(rslt->value.integer, itrunc->value.real);         
         
  g95_free_expr(itrunc);        
  g95_free_expr(rtrunc);    
    
  return range_check(rslt, nm);         
}          
          
          
         
         
g95_expr *g95_simplify_minexponent(g95_expr *l) {     
g95_expr *res;      
int i;     
     
  i = g95_validate_kind(BT_REAL, l->ts.kind);    
  if (i < 0) g95_internal_error("g95_simplify_minexponent(): Bad kind");        
        
  res = g95_int_expr(g95_real_kinds[i].min_exponent);         
  res->where = l->where;     
     
  return res;  
}        
        
        
          
          
g95_expr *g95_simplify_nint(g95_expr *x, g95_expr *r) {   
   
  return simplify_nint("NINT", x, r);
}      
      
      
   
   
g95_expr *g95_simplify_sin(g95_expr *b) {   
g95_expr *rslt;       
mpf_t xp, xq;     
     
  if (b->type != EXPR_CONSTANT) return NULL;   
   
  rslt = g95_constant_result(b->ts.type, b->ts.kind, &b->where);    
    
  switch (b->ts.type) {        
  case BT_REAL:          
    sine(&b->value.real, &rslt->value.real);          
    break;       
       
  case BT_COMPLEX:  
    mpf_init(xp);         
    mpf_init(xq);  
  
    sine(&b->value.complex.r, &xp);         
    hypercos(&b->value.complex.i, &xq); 
    mpf_mul(rslt->value.complex.r, xp, xq);   
   
    cosine(&b->value.complex.r, &xp);  
    hypersine(&b->value.complex.i, &xq);    
    mpf_mul(rslt->value.complex.i, xp, xq);          
          
    mpf_clear(xp);   
    mpf_clear(xq);   
    break;          
          
  default:       
    g95_internal_error("in g95_simplify_sin(): Bad type");        
  } 
 
  return range_check(rslt, "SIN");   
}  
  
  
 
 
g95_expr *g95_simplify_fraction(g95_expr *m) {      
g95_expr *r;   
mpf_t j, absv, ln2, lnx, pow2;
unsigned long exp2;    
    
  if (m->type != EXPR_CONSTANT) return NULL; 
 
  r = g95_constant_result(BT_REAL, m->ts.kind, &m->where);     
     
  if (mpf_cmp(m->value.real, mpf_zero) == 0) {  
    mpf_set(r->value.real, mpf_zero);      
    return r;  
  }       
       
  mpf_init_set_ui(j, 2);      
  mpf_init(absv);          
  mpf_init(ln2);
  mpf_init(lnx);      
  mpf_init(pow2);    
    
  natural_logarithm(&j, &ln2);         
         
  mpf_abs(absv, m->value.real);   
  natural_logarithm(&absv, &lnx);

  mpf_div(lnx, lnx, ln2);      
  mpf_trunc(lnx, lnx);        
  mpf_add_ui(lnx, lnx, 1);    
    
  exp2 = (unsigned long) mpf_get_d(lnx);       
  mpf_pow_ui(pow2, j, exp2); 
 
  mpf_div(r->value.real, absv, pow2);        
        
  mpf_clear(j);
  mpf_clear(ln2);   
  mpf_clear(absv);        
  mpf_clear(lnx);   
  mpf_clear(pow2);        
        
  return range_check(r, "FRACTION");      
}  
  
  
  
  
g95_expr *g95_simplify_ieor(g95_expr *z, g95_expr *d) {      
g95_expr *rslt; 
 
  if (z->type != EXPR_CONSTANT || d->type != EXPR_CONSTANT) return NULL;         
         
  rslt = g95_constant_result(BT_INTEGER, z->ts.kind, &z->where);        
        
  mpz_xor(rslt->value.integer, z->value.integer, d->value.integer);      
      
  return rslt;  
}       
       
       
   
   
g95_expr *g95_simplify_verify(g95_expr *g, g95_expr *set, g95_expr *q) {  
size_t ix, l, lenset;   
g95_expr *result;         
int back;     
     
  if (g->type != EXPR_CONSTANT || set->type != EXPR_CONSTANT) return NULL;

  if (q != NULL && q->value.logical != 0) 
    back = 1;         
  else   
    back = 0;         
         
  result = g95_constant_result(BT_INTEGER, g95_default_integer_kind(),          
			       &g->where);       
       
  l    = g->value.character.length;          
  lenset = set->value.character.length;      
      
  if (l == 0) {   
    mpz_set_ui(result->value.integer, 0);     
    return result;      
  }   
   
  if (back == 0) {     
    if (lenset == 0) {   
      mpz_set_ui(result->value.integer, l);    
      return result;      
    }         
         
    ix = strspn(g->value.character.string, set->value.character.string) + 1;        
    if (ix > l) ix = 0;

  } else { 
    if (lenset == 0) {     
      mpz_set_ui(result->value.integer, 1);   
      return result;      
    }          
          
    ix = l-strspn(g->value.character.string, set->value.character.string);     
  }     
     
  mpz_set_ui(result->value.integer, ix);          
  return result;
}       
       
       
         
         
g95_expr *g95_simplify_cos(g95_expr *p) {        
g95_expr *rslt; 
mpf_t xp, xq;    
    
  if (p->type != EXPR_CONSTANT) return NULL;  
  
  rslt = g95_constant_result(p->ts.type, p->ts.kind, &p->where);  
  
  switch (p->ts.type) {      
  case BT_REAL:   
    cosine(&p->value.real, &rslt->value.real);     
    break;
  case BT_COMPLEX:          
    mpf_init(xp);         
    mpf_init(xq);      
      
    cosine(&p->value.complex.r, &xp);      
    hypercos(&p->value.complex.i, &xq);       
    mpf_mul(rslt->value.complex.r, xp, xq);       
       
    sine(&p->value.complex.r, &xp);
    hypersine(&p->value.complex.i, &xq);   
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
          
          
  
  
g95_expr *g95_simplify_radix(g95_expr *q) {   
g95_expr *result;       
int u;    
    
  u = g95_validate_kind(q->ts.type, q->ts.kind);         
  if (u < 0) goto bad;  
  
  switch(q->ts.type) {          
  case BT_INTEGER:         
    u = g95_integer_kinds[u].radix;    
    break;

  case BT_REAL:    
    u = g95_real_kinds[u].radix;          
    break; 
 
  default: bad:          
    g95_internal_error("g95_simplify_radix(): Bad type");         
  }          
          
  result = g95_int_expr(u);  
  result->where = q->where;      
      
  return result;   
}  
  
  
 
 
g95_expr *g95_simplify_modulo(g95_expr *f, g95_expr *m) {     
g95_expr *res;
mpf_t quot, iquot, u;     
     
  if (f->type != EXPR_CONSTANT || m->type != EXPR_CONSTANT) return NULL; 
 
  res = g95_constant_result(f->ts.type, f->ts.kind, &f->where);

  switch (f->ts.type) {       
  case BT_INTEGER:    
    if (mpz_cmp_ui(m->value.integer, 0) == 0) {    
      /* Result is processor-dependent, and this processor doesn't handle it */         
      g95_error("Second argument of MODULO at %L is zero", &f->where);          
      g95_free_expr(res);        
      return &g95_bad_expr;   
    }       
    mpz_fdiv_r(res->value.integer, f->value.integer, m->value.integer);          
          
    break;     
     
  case BT_REAL: 
    if (mpf_cmp_ui(m->value.real, 0) == 0) {         
      /* Result is processor-dependent */       
      g95_error("Second argument of MODULO at %L is zero", &m->where);        
      g95_free_expr(res);  
      return &g95_bad_expr;   
    }         
         
    mpf_init(quot);
    mpf_init(iquot);   
    mpf_init(u);  
  
    mpf_div(quot, f->value.real, m->value.real);       
    mpf_floor(iquot, quot);         
    mpf_mul(u, iquot, m->value.real);       
       
    mpf_clear(quot);          
    mpf_clear(iquot);         
    mpf_clear(u);         
         
    mpf_sub(res->value.real, f->value.real, u);  
    break;         
         
  default: 
    g95_internal_error("g95_simplify_modulo(): Bad arguments");   
  } 
 
  return range_check(res, "MODULO");       
}      
      
      
         
         
g95_expr *g95_simplify_log10(g95_expr *i) { 
g95_expr *result;          
          
  if (i->type != EXPR_CONSTANT) return NULL;      
      
  if (mpf_cmp(i->value.real, mpf_zero) <= 0) {    
    g95_error("Argument of LOG10 at %L cannot be less than or equal to zero",  
              &i->where);   
    return &g95_bad_expr;        
  }   
   
  result = g95_constant_result(i->ts.type, i->ts.kind, &i->where);    
    
  common_logarithm(&i->value.real, &result->value.real);

  return range_check(result, "LOG10"); 
}


          
          
g95_expr *g95_simplify_ishft(g95_expr *m, g95_expr *j) {        
int w, z, shift, bit_size;
g95_expr *rslt;   
mpz_t v;  
  
  if (m->type != EXPR_CONSTANT || j->type != EXPR_CONSTANT) return NULL;          
          
  if (g95_extract_int(j, &shift) != NULL) {         
    g95_error("Invalid second argument of ISHFT at %L", &j->where); 
    return &g95_bad_expr;    
  }        
        
  z = g95_validate_kind(BT_INTEGER, m->ts.kind);     
  if (z == -1) g95_internal_error("In g95_simplify_ishft: bad kind");   
   
  bit_size = g95_integer_kinds[z].bit_size;   
   
  mpz_init_set_ui(v, 0);    
    
  for(w=0; w<bit_size; w++) {         
    if (w-shift < 0 || w-shift >= bit_size) continue;     
     
    if (mpz_tstbit(m->value.integer, w-shift)) mpz_setbit(v, w); 
  }      
      
  twos_complement(v, bit_size);         
         
  rslt = g95_constant_result(m->ts.type, m->ts.kind, &m->where);         
  mpz_set(rslt->value.integer, v); 
 
  return rslt;        
}  
  
  
    
    
g95_expr *g95_simplify_nearest(g95_expr *m, g95_expr *r) {    
g95_expr *res;   
float rval;  
double val, eps;    
int q, t, g;      
      
/* TODO: This implementation is dopey and probably not quite right,
 * but it's a start.*/

  if (m->type != EXPR_CONSTANT) return NULL; 
 
  g = g95_validate_kind(m->ts.type, m->ts.kind);
  if (g == -1) g95_internal_error("g95_simplify_precision(): Bad kind"); 
 
  res = g95_constant_result(m->ts.type, m->ts.kind, &m->where);        
        
  val  = mpf_get_d(m->value.real);      
  q    = g95_real_kinds[g].digits;       
       
  eps = 1.;        
  for (t=1;t<q;++t) {        
    eps = eps/2.0;        
  }          
          
  if (mpf_cmp_ui(r->value.real, 0) > 0) {  
    if (g == g95_default_real_kind()) {       
      rval = (float) val;      
      rval = rval + eps;      
      mpf_set_d(res->value.real, rval);        
    }         
    else { 
      val = val + eps;
      mpf_set_d(res->value.real, val);
    }    
  }
  else if (mpf_cmp_ui(r->value.real, 0) < 0) {         
    if (g == g95_default_real_kind()) {
      rval = (float) val;     
      rval = rval - eps;    
      mpf_set_d(res->value.real, rval);    
    }         
    else {      
      val = val - eps; 
      mpf_set_d(res->value.real, val);    
    }   
  }  
  else {  
    g95_error("Invalid second argument of NEAREST at %L", &r->where); 
    g95_free(res); 
    return &g95_bad_expr;       
  }          
          
  return range_check(res, "NEAREST");    
}     
     
     
     
     
/* invert_table()-- Given a collating table, create the inverse table */  
  
static void invert_table(int *table, int *xtable) {   
int h;

  for(h=0; h<256; h++)  
    xtable[h] = 0;  
  
  for(h=0; h<256; h++)      
    xtable[table[h]] = h;    
}    
    
    
   
   
g95_expr *g95_simplify_precision(g95_expr *s) {  
g95_expr *result;
int i; 
 
  i = g95_validate_kind(s->ts.type, s->ts.kind);   
  if (i == -1) g95_internal_error("g95_simplify_precision(): Bad kind");    
    
  result = g95_int_expr(g95_real_kinds[i].precision);          
  result->where = s->where;

  return result;       
}          
          
          
          
          
g95_expr *g95_simplify_huge(g95_expr *c) {         
g95_expr *result; 
int x;          
          
  x = g95_validate_kind(c->ts.type, c->ts.kind);         
  if (x == -1) goto bad_type;    
    
  result = g95_constant_result(c->ts.type, c->ts.kind, &c->where);        
        
  switch(c->ts.type) {  
  case BT_INTEGER:        
    mpz_set(result->value.integer, g95_integer_kinds[x].huge);          
    break;        
        
  case BT_REAL:     
    mpf_set(result->value.real, g95_real_kinds[x].huge);      
    break; 
 
  bad_type:  
  default:      
    g95_internal_error("g95_simplify_huge(): Bad type");       
  }   
   
  return result;         
}    
    
    
   
   
g95_expr *g95_simplify_idnint(g95_expr *l) {     
     
  return simplify_nint("IDNINT", l, NULL); 
}       
       
       
   
   
g95_expr *g95_simplify_logical(g95_expr *q, g95_expr *c) {          
g95_expr *result;   
int knd;     
     
  knd = get_kind(BT_LOGICAL, c, "LOGICAL", g95_default_logical_kind());    
  if (knd < 0) return &g95_bad_expr;     
     
  if (q->type != EXPR_CONSTANT) return NULL; 
 
  result = g95_constant_result(BT_LOGICAL, knd, &q->where);     
     
  result->value.logical = q->value.logical;        
        
  return result; 
}          
          
          
     
     
g95_expr *g95_simplify_scan(g95_expr *q, g95_expr *f, g95_expr *v) {  
g95_expr *result;       
int back;   
size_t indx, leng, lenc;    
    
  if (q->type != EXPR_CONSTANT || f->type != EXPR_CONSTANT) return NULL;

  if (v != NULL && v->value.logical != 0)  
    back = 1;       
  else 
    back = 0;  
  
  result = g95_constant_result(BT_INTEGER, g95_default_integer_kind(),       
			       &q->where);  
  
  leng  = q->value.character.length;       
  lenc = f->value.character.length; 
 
  if (leng == 0 || lenc == 0) {
    indx = 0;   
  } else {        
    indx = strcspn(q->value.character.string, f->value.character.string) + 1;         
    if (indx > leng) indx=0;        
    if (back != 0 && indx != 0) indx = leng - indx + 1;      
  }       
       
  mpz_set_ui(result->value.integer, indx);   
  return range_check(result, "SCAN");   
}       
       
       
  
  
g95_expr *g95_simplify_len(g95_expr *w) {     
g95_expr *result;     
     
  if (w->type != EXPR_CONSTANT) return NULL;         
         
  result = g95_constant_result(BT_INTEGER, g95_default_integer_kind(),        
			       &w->where);         
         
  mpz_set_si(result->value.integer, w->value.character.length);  
  return range_check(result, "LEN");          
}          
          
          
      
      
g95_expr *g95_simplify_selected_int_kind(g95_expr *c) {      
int m, kind, range;      
g95_expr *rslt;  
  
  if (c->type != EXPR_CONSTANT || g95_extract_int(c, &range) != NULL)      
    return NULL;         
         
  kind = INT_MAX;    
    
  for(m=0; g95_integer_kinds[m].kind!=0; m++)  
    if (g95_integer_kinds[m].range >= range &&        
	g95_integer_kinds[m].kind < kind) kind = g95_integer_kinds[m].kind;          
          
  if (kind == INT_MAX) kind = -1;     
     
  rslt = g95_int_expr(kind);    
  rslt->where = c->where;          
          
  return rslt;    
}     
     
     
      
      
g95_expr *g95_simplify_int(g95_expr *o, g95_expr *x) {     
g95_expr *rpart, *rtrunc, *rslt;        
int knd;          
          
  knd = get_kind(BT_REAL, x, "INT", g95_default_real_kind());        
  if (knd == -1) return &g95_bad_expr;

  if (o->type != EXPR_CONSTANT) return NULL;   
   
  rslt = g95_constant_result(BT_INTEGER, knd, &o->where); 
 
  switch(o->ts.type) {         
  case BT_INTEGER:       
    mpz_set(rslt->value.integer, o->value.integer);         
    break;        
        
  case BT_REAL:  
    rtrunc = g95_copy_expr(o);  
    mpf_trunc(rtrunc->value.real, o->value.real);   
    mpz_set_f(rslt->value.integer, rtrunc->value.real);      
    g95_free_expr(rtrunc);   
    break;     
     
  case BT_COMPLEX: 
    rpart = g95_complex2real(o, knd);      
    rtrunc = g95_copy_expr(rpart);
    mpf_trunc(rtrunc->value.real, rpart->value.real);    
    mpz_set_f(rslt->value.integer, rtrunc->value.real);   
    g95_free_expr(rpart);          
    g95_free_expr(rtrunc);    
    break;   
   
  default:  
    g95_error("Argument of INT at %L is not a valid type", &o->where);          
    g95_free_expr(rslt);        
    return &g95_bad_expr;    
  }        
        
  return range_check(rslt, "INT");          
}      
      
      


void g95_simplify_init_1(void) {       
       
  mpf_init_set_str(mpf_zero, "0.0", 10);        
  mpf_init_set_str(mpf_half, "0.5", 10);  
  mpf_init_set_str(mpf_one,  "1.0", 10);    
  mpz_init_set_str(mpz_zero,   "0", 10);      
      
  invert_table(ascii_table, xascii_table);       
}  
  
  
