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
  
  
    
    
/* get_kind()-- A helper function that gets an optional and possibly
 * missing kind parameter.  Returns the kind, -1 if something went
 * wrong. */         
         
static int get_kind(bt type, g95_expr *b, char *name, int default_kind) {         
int kind;        
        
  if (b == NULL) return default_kind;

  if (b->type != EXPR_CONSTANT) {     
    g95_error("KIND parameter of %s at %L must be an initialization "  
	      "expression", name, &b->where);   
   
    return -1; 
  }     
     
  if (g95_extract_int(b, &kind) != NULL ||    
      g95_validate_kind(type, kind) < 0) {         
         
    g95_error("Invalid KIND parameter of %s at %L", name, &b->where);    
    return -1;        
  }          
          
  return kind;  
}    
    
    
 
 
void g95_simplify_done_1(void) {          
          
  mpf_clear(mpf_zero);        
  mpf_clear(mpf_half);   
  mpf_clear(mpf_one);    
  mpz_clear(mpz_zero); 
}    
  
  
g95_expr *g95_simplify_lgt(g95_expr *m, g95_expr *c) {       
       
  if (m->type != EXPR_CONSTANT || c->type != EXPR_CONSTANT) return NULL;         
         
  return g95_logical_expr(g95_compare_string(m, c, xascii_table) > 0,
			  &m->where);     
}          
          
          
          
          
g95_expr *g95_simplify_range(g95_expr *r) {          
g95_expr *result;   
int w;       
long x;   
   
  w = g95_validate_kind(r->ts.type, r->ts.kind);       
  if (w < 0) goto bad_type;         
         
  switch(r->ts.type) {         
  case BT_INTEGER:          
    x = g95_integer_kinds[w].range;    
    break;        
        
  case BT_REAL:      
  case BT_COMPLEX:          
    x = g95_real_kinds[w].range;         
    break;        
        
  bad_type:      
  default:       
    g95_internal_error("g95_simplify_range(): Bad kind"); 
  }    
    
  result = g95_int_expr(x);  
  result->where = r->where;          
          
  return result;       
}         
         
         
      
      
g95_expr *g95_simplify_selected_int_kind(g95_expr *z) {          
int g, kind, range;       
g95_expr *result;    
    
  if (z->type != EXPR_CONSTANT || g95_extract_int(z, &range) != NULL)        
    return NULL;      
      
  kind = INT_MAX;

  for(g=0; g95_integer_kinds[g].kind!=0; g++) 
    if (g95_integer_kinds[g].range >= range &&    
	g95_integer_kinds[g].kind < kind) kind = g95_integer_kinds[g].kind;    
    
  if (kind == INT_MAX) kind = -1;  
  
  result = g95_int_expr(kind);     
  result->where = z->where;    
    
  return result;     
}          
          
          
          
          
g95_expr *g95_simplify_adjustr(g95_expr *h) {       
g95_expr *result;    
int count, f, len; 
char ch;   
   
  if (h->type != EXPR_CONSTANT) return NULL;  
  
  len = h->value.character.length;   
   
  result = g95_constant_result(BT_CHARACTER, h->ts.kind, &h->where);          
          
  result->value.character.length = len;         
  result->value.character.string = g95_getmem(len+1);         
         
  for (count=0, f=len-1; f>=0; --f) {         
    ch = h->value.character.string[f];  
    if (ch != ' ') break; 
    ++count;     
  }  
  
  for (f=0; f<count; ++f) {    
    result->value.character.string[f] = ' ';   
  }     
     
  for (f=count; f<len; ++f) {         
    result->value.character.string[f] = h->value.character.string[f-count];       
  }    
    
  result->value.character.string[len] = '\0';   /* For debugger */   
   
  return result;    
}        
        
        
          
          
/* range_check()-- Range checks an expression node.  If all goes well,
 * returns the node, otherwise returns &g95_bad_expr and frees the node. */ 
 
static g95_expr *range_check(g95_expr *result, char *name) {        
        
  if (g95_range_check(result) == ARITH_OK)        
    return result;      
      
  g95_error("Result of %s overflows its kind at %L", name, &result->where);   
  g95_free_expr(result);        
  return &g95_bad_expr;   
}   
   
   
    
    
g95_expr *g95_simplify_size(g95_expr *array, g95_expr *dim) {      
mpz_t shape[G95_MAX_DIMENSIONS];  
g95_array_ref *ar;
g95_expr *result;          
int r, l;      
      
  if (dim == NULL) {      
    if (g95_array_size(array, &shape[0]) == FAILURE) return NULL;          
    r = 1;    
    l = 1;          
  } else {         
    if (dim->type != EXPR_CONSTANT || array->type != EXPR_VARIABLE ||   
	array->ref == NULL || array->ref->type != REF_ARRAY ||   
	array->ref->u.ar.type != AR_FULL) return NULL;  
  
    l = mpz_get_ui(dim->value.integer);   
    ar = g95_find_array_ref(array);    
    if (g95_array_ref_shape(ar, shape) == FAILURE) return NULL;         
    r = (ar->type == AR_FULL) ? ar->as->rank : ar->dimen;      
  }  
  
  result = g95_constant_result(BT_INTEGER, g95_default_integer_kind(),          
			       &array->where);    
    
  mpz_set(result->value.integer, shape[l-1]);

  for(l=0; l<r; l++) 
    mpz_clear(shape[l]);         
         
  return result;    
}     
     
     
      
      
g95_expr *g95_simplify_kind(g95_expr *b) {

  if (b->ts.type == BT_DERIVED) {         
    g95_error("Argument of KIND at %L is a DERIVED type", &b->where);  
    return &g95_bad_expr;       
  }       
       
  return g95_int_expr(b->ts.kind);    
}    
    
    
 
 
g95_expr *g95_simplify_float(g95_expr *m) {       
g95_expr *result;

  if (m->type != EXPR_CONSTANT) return NULL;

  result = g95_int2real(m, g95_default_real_kind());
  return range_check(result, "FLOAT");       
}       
       
       
    
    
g95_expr *g95_simplify_minexponent(g95_expr *p) {          
g95_expr *result;        
int k;    
    
  k = g95_validate_kind(BT_REAL, p->ts.kind);       
  if (k < 0) g95_internal_error("g95_simplify_minexponent(): Bad kind");      
      
  result = g95_int_expr(g95_real_kinds[k].min_exponent);  
  result->where = p->where;          
          
  return result;
}       
       
       
    
    
g95_expr *g95_simplify_scan(g95_expr *e, g95_expr *h, g95_expr *b) {  
g95_expr *result;      
int back;     
size_t indx, len, lenc;     
     
  if (e->type != EXPR_CONSTANT || h->type != EXPR_CONSTANT) return NULL;         
         
  if (b != NULL && b->value.logical != 0)         
    back = 1;       
  else       
    back = 0;      
      
  result = g95_constant_result(BT_INTEGER, g95_default_integer_kind(),   
			       &e->where);       
       
  len  = e->value.character.length;          
  lenc = h->value.character.length;     
     
  if (len == 0 || lenc == 0) {     
    indx = 0;          
  } else {          
    indx = strcspn(e->value.character.string, h->value.character.string) + 1;         
    if (indx > len) indx=0;       
    if (back != 0 && indx != 0) indx = len - indx + 1;          
  }      
      
  mpz_set_ui(result->value.integer, indx);  
  return range_check(result, "SCAN");
}    
    
    


g95_expr *g95_simplify_mvbits(g95_expr *n, g95_expr *fp, g95_expr *y,       
			      g95_expr *to, g95_expr *tp) { 
  return NULL;         
}        
        
        
     
     
g95_expr *g95_simplify_log10(g95_expr *u) {   
g95_expr *result;      
      
  if (u->type != EXPR_CONSTANT) return NULL;      
      
  if (mpf_cmp(u->value.real, mpf_zero) <= 0) {    
    g95_error("Argument of LOG10 at %L cannot be less than or equal to zero",   
              &u->where);    
    return &g95_bad_expr;         
  }

  result = g95_constant_result(u->ts.type, u->ts.kind, &u->where);         
         
  common_logarithm(&u->value.real, &result->value.real);     
     
  return range_check(result, "LOG10");        
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
 
 
     
     
g95_expr *g95_simplify_len_trim(g95_expr *t) {   
g95_expr *result;      
int count, len, lentrim, s;    
    
  if (t->type != EXPR_CONSTANT) return NULL;    
    
  result = g95_constant_result(BT_INTEGER, g95_default_integer_kind(),      
			       &t->where);         
         
  len = t->value.character.length;          
          
  for(count=0, s=1; s<=len; s++)          
    if (t->value.character.string[len-s] == ' ')     
      count++;      
    else        
      break;       
       
  lentrim = len-count;         
         
  mpz_set_si(result->value.integer, lentrim);        
  return range_check(result, "LEN_TRIM");     
} 
 
 
        
        
g95_expr *g95_simplify_anint(g95_expr *v, g95_expr *z) { 
g95_expr *rtrunc, *result;         
int kind, cmp;      
      
  kind = get_kind(BT_REAL, z, "ANINT", v->ts.kind);     
  if (kind == -1) return &g95_bad_expr;     
     
  if (v->type != EXPR_CONSTANT) return NULL;    
    
  result = g95_constant_result(v->ts.type, kind, &v->where);        
        
  rtrunc = g95_copy_expr(v);      
      
  cmp = mpf_cmp_ui(v->value.real, 0);    
    
  if (cmp > 0) {         
    mpf_add(rtrunc->value.real, v->value.real,mpf_half);
    mpf_trunc(result->value.real, rtrunc->value.real);        
  } else if (cmp < 0) {     
    mpf_sub(rtrunc->value.real, v->value.real,mpf_half);     
    mpf_trunc(result->value.real, rtrunc->value.real);
  } else   
    mpf_set_ui(result->value.real, 0);       
       
  g95_free_expr(rtrunc);     
     
  return range_check(result, "ANINT"); 
}


       
       
g95_expr *g95_simplify_digits(g95_expr *n) { 
int u, digits;    
    
  u = g95_validate_kind(n->ts.type, n->ts.kind);    
  if (u < 0) goto bad;      
      
  switch(n->ts.type) {   
  case BT_INTEGER:  
    digits = g95_integer_kinds[u].digits;
    break;      
      
  case BT_REAL:       
  case BT_COMPLEX:         
    digits = g95_real_kinds[u].digits;         
    break;        
        
  default:       
  bad:       
    g95_internal_error("g95_simplify_digits(): Bad type");      
  }      
      
  return g95_int_expr(digits);       
}      
      
      
  
  
g95_expr *g95_simplify_dint(g95_expr *p) {       
g95_expr *rtrunc, *result;         
         
  if (p->type != EXPR_CONSTANT) return NULL;        
        
  rtrunc = g95_copy_expr(p);         
         
  mpf_trunc(rtrunc->value.real, p->value.real);  
  
  result = g95_real2real(rtrunc, g95_default_double_kind());     
  g95_free_expr(rtrunc);  
  
  return range_check(result, "DINT");         
         
}    
    
    
  
  
g95_expr *g95_simplify_aimag(g95_expr *g) {     
g95_expr *result;          
          
  if (g->type != EXPR_CONSTANT) return NULL;      
      
  result = g95_constant_result(BT_REAL, g->ts.kind, &g->where);      
  mpf_set(result->value.real, g->value.complex.i);      
      
  return range_check(result, "AIMAG");      
}   
   
   
     
     
/* twos_complement()-- Given an unsigned multiple precision integer
 * and a bit size, convert it to the equivalent twos complement signed
 * integer. */          
          
static void twos_complement(mpz_t t, int bit_size) {    
int p;          
          
  if (!mpz_tstbit(t, bit_size-1)) return;        
        
  for(p=0; p<bit_size; p++)  
    if (mpz_tstbit(t, p)) 
      mpz_clrbit(t, p);       
    else        
      mpz_setbit(t, p);          
          
  mpz_add_ui(t, t, 1);      
  mpz_neg(t, t);      
}    
    
    
      
      
g95_expr *g95_simplify_ibclr(g95_expr *m, g95_expr *n) {   
g95_expr *result;          
int p, pos;   
   
  if (m->type != EXPR_CONSTANT || n->type != EXPR_CONSTANT) return NULL;     
     
  if (g95_extract_int(n, &pos) != NULL || pos < 0) {         
    g95_error("Invalid second argument of IBCLR at %L", &n->where);          
    return &g95_bad_expr;       
  }      
      
  p = g95_validate_kind(m->ts.type, m->ts.kind);     
  if (p == -1) g95_internal_error("In g95_simplify_ibclr: bad kind");          
          
  if (pos > g95_integer_kinds[p].bit_size) {      
    g95_error("Second argument of IBCLR exceeds bit size at %L", &n->where);   
    return &g95_bad_expr;   
  }

  result = g95_copy_expr(m);   
  mpz_clrbit(result->value.integer, pos);       
       
  return result;    
}          
          
          
       
       
g95_expr *g95_simplify_adjustl(g95_expr *e) { 
g95_expr *result;    
int count, o, len;    
char ch;          
          
  if (e->type != EXPR_CONSTANT) return NULL;      
      
  len = e->value.character.length;        
        
  result = g95_constant_result(BT_CHARACTER, e->ts.kind, &e->where);   
   
  result->value.character.length = len; 
  result->value.character.string = g95_getmem(len+1);         
         
  for (count=0, o=0; o<len; ++o) { 
    ch = e->value.character.string[o];        
    if (ch != ' ') break;       
    ++count;       
  }        
        
  for (o=0; o<len-count; ++o) {    
    result->value.character.string[o] = e->value.character.string[count+o];
  }   
   
  for (o=len-count; o<len; ++o) {   
    result->value.character.string[o] = ' '; 
  }      
      
  result->value.character.string[len] = '\0';   /* For debugger */     
     
  return result;         
}


      
      
g95_expr *g95_simplify_mod(g95_expr *a, g95_expr *h) {        
g95_expr *result;  
mpf_t quot, iquot, term;  
  
  if (a->type != EXPR_CONSTANT || h->type != EXPR_CONSTANT) return NULL;   
   
  result = g95_constant_result(a->ts.type, a->ts.kind, &a->where);    
    
  switch (a->ts.type) {  
  case BT_INTEGER:          
    if (mpz_cmp_ui(h->value.integer, 0) == 0) {       
      /* Result is processor-dependent */        
      g95_error("Second argument MOD at %L is zero", &a->where);     
      g95_free_expr(result);   
      return &g95_bad_expr; 
    }  
    mpz_tdiv_r(result->value.integer, a->value.integer, h->value.integer); 
    break;          
          
  case BT_REAL:        
    if (mpf_cmp_ui(h->value.real, 0) == 0) {   
      /* Result is processor-dependent */         
         
      g95_error("Second argument of MOD at %L is zero", &h->where);        
      g95_free_expr(result);        
      return &g95_bad_expr; 
    }

    mpf_init(quot);      
    mpf_init(iquot);    
    mpf_init(term);      
      
    mpf_div(quot, a->value.real, h->value.real);         
    mpf_trunc(iquot, quot);        
    mpf_mul(term, iquot, h->value.real);        
    mpf_sub(result->value.real, a->value.real, term);     
     
    mpf_clear(quot);   
    mpf_clear(iquot);          
    mpf_clear(term);          
    break;        
        
  default:       
    g95_internal_error("g95_simplify_mod(): Bad arguments");         
  }        
        
  return range_check(result, "MOD");      
}      
      
      
       
       
g95_expr *g95_simplify_ibset(g95_expr *g, g95_expr *t) {     
g95_expr *result;          
int m, pos;       
       
  if (g->type != EXPR_CONSTANT || t->type != EXPR_CONSTANT) return NULL;      
      
  if (g95_extract_int(t, &pos) != NULL || pos < 0) {       
    g95_error("Invalid second argument of IBSET at %L", &t->where);  
    return &g95_bad_expr;        
  }

  m = g95_validate_kind(g->ts.type, g->ts.kind);  
  if (m == -1) g95_internal_error("In g95_simplify_ibset: bad kind");         
         
  if (pos > g95_integer_kinds[m].bit_size) { 
    g95_error("Second argument of IBSET exceeds bit size at %L", &t->where);         
    return &g95_bad_expr;      
  }   
   
  result = g95_copy_expr(g);         
         
  mpz_setbit(result->value.integer, pos);   
  return range_check(result, "IBSET");      
} 
 
 
    
    
g95_expr *g95_simplify_dprod(g95_expr *o, g95_expr *g) {  
g95_expr *mult1, *mult2, *result;

  if (o->type != EXPR_CONSTANT || g->type != EXPR_CONSTANT) return NULL;

  result = g95_constant_result(BT_REAL, g95_default_double_kind(), &o->where);

  mult1 = g95_real2real(o, g95_default_double_kind());      
  mult2 = g95_real2real(g, g95_default_double_kind());  
  
  mpf_mul(result->value.real, mult1->value.real, mult2->value.real); 
 
  g95_free_expr(mult1);       
  g95_free_expr(mult2);          
          
  return range_check(result, "DPROD");          
}          
          
          
   
   
g95_expr *g95_simplify_sinh(g95_expr *i) { 
g95_expr *result;    
    
  if (i->type != EXPR_CONSTANT) return NULL;       
       
  result = g95_constant_result(i->ts.type, i->ts.kind, &i->where);         
         
  hypersine(&i->value.real, &result->value.real);  
  
  return range_check(result, "SINH");          
}  
  
  
    
    
g95_expr *g95_simplify_bit_size(g95_expr *r) {         
g95_expr *result;          
int u;    
    
  u = g95_validate_kind(r->ts.type, r->ts.kind); 
  if (u < 0) g95_internal_error("In g95_simplify_bit_size(): bad kind");   
   
  result = g95_constant_result(BT_INTEGER, r->ts.kind, &r->where); 
  mpz_set_ui(result->value.integer, g95_integer_kinds[u].bit_size);        
        
  return result;      
}      
      
      
       
       
g95_expr *g95_simplify_btest(g95_expr *f, g95_expr *bit) {          
int b;

  if (f->type != EXPR_CONSTANT || bit->type != EXPR_CONSTANT) return NULL;     
     
  if (g95_extract_int(bit, &b) != NULL || b < 0)        
    return g95_logical_expr(0, &f->where);

  return g95_logical_expr(mpz_tstbit(f->value.integer, b), &f->where);      
}       
       
       
  
  
g95_expr *g95_simplify_cosh(g95_expr *q) {       
g95_expr *result;      
      
  if (q->type != EXPR_CONSTANT) return NULL;  
  
  result = g95_constant_result(q->ts.type, q->ts.kind, &q->where);

  hypercos(&q->value.real, &result->value.real);

  return range_check(result, "COSH");         
}      
      
      
         
         
g95_expr *g95_simplify_maxexponent(g95_expr *m) {          
g95_expr *result;     
int c;  
  
  c = g95_validate_kind(BT_REAL, m->ts.kind);          
  if (c < 0) g95_internal_error("g95_simplify_maxexponent(): Bad kind");         
         
  result = g95_int_expr(g95_real_kinds[c].max_exponent); 
  result->where = m->where;       
       
  return result; 
}      
      
      
 
 
g95_expr *g95_simplify_repeat(g95_expr *u, g95_expr *o) { 
g95_expr *result;         
int a, w, len, ncopies, nlen; 
 
  if (u->type != EXPR_CONSTANT || o->type != EXPR_CONSTANT) return NULL;         
         
  if (o !=NULL && (g95_extract_int(o, &ncopies) != NULL || ncopies < 0)) {     
    g95_error("Invalid second argument of REPEAT at %L", &o->where);       
    return &g95_bad_expr;         
  }      
      
  len    = u->value.character.length;         
  nlen   = ncopies*len; 
 
  result = g95_constant_result(BT_CHARACTER, u->ts.kind, &u->where);      
      
  if (ncopies == 0) {      
    result->value.character.string=g95_getmem(1);   
    result->value.character.length=0;    
    result->value.character.string='\0';     
    return result;        
  }   
   
  result->value.character.length=nlen;        
  result->value.character.string=g95_getmem(nlen+1);      
      
  for(a=0; a<ncopies; a++)          
    for(w=0; w<len; w++)
      result->value.character.string[w+a*len] = u->value.character.string[w];       
       
  result->value.character.string[nlen] = '\0';  /* For debugger */       
  return result;      
}   
   
   
     
     
g95_expr *g95_simplify_atan(g95_expr *g) {    
g95_expr *result;     
     
  if (g->type != EXPR_CONSTANT) return NULL;

  result = g95_constant_result(g->ts.type, g->ts.kind, &g->where);     
     
  arctangent(&g->value.real, &result->value.real);   
   
  return range_check(result, "ATAN");  
  
} 
 
 
         
         
g95_expr *g95_simplify_abs(g95_expr *h) {    
g95_expr *result;        
mpf_t m, k;  
  
  if (h->type != EXPR_CONSTANT) return NULL;          
          
  switch(h->ts.type) {   
  case BT_INTEGER:      
    result = g95_constant_result(BT_INTEGER, h->ts.kind, &h->where);    
    
    mpz_abs(result->value.integer, h->value.integer);    
    
    result = range_check(result, "IABS");        
    break;   
   
  case BT_REAL: 
    result = g95_constant_result(BT_REAL, h->ts.kind, &h->where);       
       
    mpf_abs(result->value.real, h->value.real);      
      
    result = range_check(result, "ABS");          
    break;      
      
  case BT_COMPLEX:    
    result = g95_constant_result(BT_REAL, h->ts.kind, &h->where);       
       
    mpf_init(m);      
    mpf_mul(m, h->value.complex.r, h->value.complex.r);      
      
    mpf_init(k); 
    mpf_mul(k, h->value.complex.i, h->value.complex.i);   
   
    mpf_add(m, m, k);          
    mpf_sqrt(result->value.real, m);    
    
    mpf_clear(m); 
    mpf_clear(k);        
        
    result = range_check(result, "CABS"); 
    break;  
  
  default:
    g95_internal_error("g95_simplify_abs(): Bad type");         
  }       
       
  return result;    
} 
 
 
      
      
g95_expr *g95_simplify_log(g95_expr *x) {        
g95_expr *result;   
mpf_t xr, xi;         
         
  if (x->type != EXPR_CONSTANT) return NULL;    
    
  result = g95_constant_result(x->ts.type, x->ts.kind, &x->where);   
   
  switch(x->ts.type) {   
  case BT_REAL:          
    if (mpf_cmp(x->value.real, mpf_zero) <= 0) {  
      g95_error("Argument of LOG at %L cannot be less than or equal to zero",  
		&x->where); 
      g95_free_expr(result);      
      return &g95_bad_expr;  
    }   
   
    natural_logarithm(&x->value.real, &result->value.real);     
    break;         
         
  case BT_COMPLEX:      
    if ((mpf_cmp(x->value.complex.r, mpf_zero) == 0) &&      
	(mpf_cmp(x->value.complex.i, mpf_zero) == 0)) {   
      g95_error("Complex argument of LOG at %L cannot be zero",       
		&x->where);         
      g95_free_expr(result);         
      return &g95_bad_expr;    
    }       
       
    mpf_init(xr);     
    mpf_init(xi);         
         
    mpf_div(xr, x->value.complex.i, x->value.complex.r);   
    arctangent(&xr, &result->value.complex.i);       
       
    mpf_mul(xr, x->value.complex.r, x->value.complex.r);     
    mpf_mul(xi, x->value.complex.i, x->value.complex.i);   
    mpf_add(xr, xr, xi);        
    mpf_sqrt(xr, xr);   
    natural_logarithm(&xr, &result->value.complex.r);       
       
    mpf_clear(xr);    
    mpf_clear(xi);        
        
    break;     
     
  default:         
    g95_internal_error("g95_simplify_log: bad type"); 
  }         
         
  return range_check(result, "LOG");  
}       
       
       
    
    
g95_expr *g95_simplify_atan2(g95_expr *c, g95_expr *s) {        
g95_expr *result;    
mpf_t term;  
  
  if (s->type != EXPR_CONSTANT || c->type != EXPR_CONSTANT) return NULL;          
          
  result = g95_constant_result(s->ts.type, s->ts.kind, &s->where);     
     
  mpf_init(term);

  if (mpf_cmp_ui(c->value.real, 0) == 0) {
    if (mpf_cmp_ui(s->value.real, 0) == 0) {  
      mpf_clear(term);      
      g95_error("If first argument of ATAN2 %L is zero, the second argument "       
		"must not be zero", &s->where);
      g95_free_expr(result);          
      return &g95_bad_expr;      
    }   
    else if (mpf_cmp_si(s->value.real, 0) < 0) {    
      mpf_set(result->value.real, pi);      
      mpf_clear(term);   
      return result;     
    }  
    else if (mpf_cmp_si(s->value.real, -1)== 0) {        
      mpf_set_ui(result->value.real, 0);
      mpf_clear(term);  
      return range_check(result, "ATAN2");      
    }         
  }     
     
  if (mpf_cmp_ui(s->value.real, 0) == 0) {  
    if (mpf_cmp_si(c->value.real, 0) < 0) {     
      mpf_neg(term, half_pi);   
      mpf_set(result->value.real, term);        
      mpf_clear(term);    
      return range_check(result, "ATAN2");          
    }     
    else if (mpf_cmp_si(c->value.real, 0) > 0) {      
      mpf_set(result->value.real, half_pi);       
      mpf_clear(term);        
      return range_check(result, "ATAN2");         
    }       
  }    
    
  mpf_div(term, c->value.real, s->value.real);    
  arctangent(&term, &result->value.real); 
 
  mpf_clear(term); 
 
  return range_check(result, "ATAN2");     
     
}  
  
  
      
      
g95_expr *g95_simplify_exp(g95_expr *e) {        
g95_expr *result;        
mpf_t xp, xq; 
double ln2, absval, rhuge;         
         
  if (e->type != EXPR_CONSTANT) return NULL; 
 
  result = g95_constant_result(e->ts.type, e->ts.kind, &e->where);

  /* Exactitude doesn't matter here */
  ln2 = .6931472;   
  rhuge  = ln2*mpz_get_d(g95_integer_kinds[0].huge);    
    
  switch (e->ts.type) {     
  case BT_REAL: 
    absval = mpf_get_d(e->value.real);  
    if (absval < 0) absval = -absval;         
    if (absval > rhuge) {         
      /* Underflow (set arg to zero) if x is negative and its magnitude is
       * greater than the maximum C long int times ln2, because the exponential
       * method in arith.c will fail for such values */         
         
      if (mpf_cmp_ui(e->value.real, 0) < 0) { 
        if (g95_option.pedantic == 1)         
            g95_warning_now("Argument of EXP at %L is negative and too large, " 
			    "setting result to zero", &e->where);    
        mpf_set_ui(result->value.real, 0);    
        return range_check(result, "EXP");
      }     
    /* Overflow if magnitude of x is greater than C long int huge times ln2. */  
      else {        
        g95_error("Argument of EXP at %L too large", &e->where); 
        g95_free_expr(result);      
        return &g95_bad_expr;       
      }          
    }     
    exponential(&e->value.real, &result->value.real);    
    break;   
  case BT_COMPLEX:      
    /* Using Euler's formula */   
    absval = mpf_get_d(e->value.complex.r);  
    if (absval < 0) absval = -absval;       
    if (absval > rhuge) {      
      if (mpf_cmp_ui(e->value.complex.r, 0) < 0) {
        if (g95_option.pedantic == 1)      
            g95_warning_now("Real part of argument of EXP at %L is negative "  
			    "and too large, setting result to zero",
			    &e->where);

        mpf_set_ui(result->value.complex.r, 0);    
        mpf_set_ui(result->value.complex.i, 0);          
        return range_check(result, "EXP");     
      } else {
        g95_error("Real part of argument of EXP at %L too large", &e->where);  
        g95_free_expr(result);
        return &g95_bad_expr;
      }
    }     
    mpf_init(xp);     
    mpf_init(xq);       
    exponential(&e->value.complex.r, &xq);    
    cosine(&e->value.complex.i, &xp);     
    mpf_mul(result->value.complex.r, xq, xp);          
    sine(&e->value.complex.i, &xp);  
    mpf_mul(result->value.complex.i, xq, xp);
    mpf_clear(xp);         
    mpf_clear(xq);          
    break;  
  default:  
    g95_internal_error("in g95_simplify_exp(): Bad type");
  }    
    
  return range_check(result, "EXP");      
} 
 
 
      
      
g95_expr *g95_simplify_cos(g95_expr *j) {       
g95_expr *result;        
mpf_t xp, xq;

  if (j->type != EXPR_CONSTANT) return NULL;        
        
  result = g95_constant_result(j->ts.type, j->ts.kind, &j->where);    
    
  switch (j->ts.type) {  
  case BT_REAL: 
    cosine(&j->value.real, &result->value.real);  
    break;     
  case BT_COMPLEX:       
    mpf_init(xp);          
    mpf_init(xq);    
    
    cosine(&j->value.complex.r, &xp);
    hypercos(&j->value.complex.i, &xq);        
    mpf_mul(result->value.complex.r, xp, xq);        
        
    sine(&j->value.complex.r, &xp); 
    hypersine(&j->value.complex.i, &xq);   
    mpf_mul(xp, xp, xq);     
    mpf_neg(result->value.complex.i, xp);   
   
    mpf_clear(xp);        
    mpf_clear(xq); 
    break;
  default:  
    g95_internal_error("in g95_simplify_cos(): Bad type");      
  }   
   
  return range_check(result, "COS");      
      
}  
  
  
       
       
/* simplify_cmplx()-- Common subroutine for simplifying CMPLX and DCMPLX */  
  
static g95_expr *simplify_cmplx(g95_expr *k, g95_expr *t, int kind,     
				char *name) {         
g95_expr *result; 
 
  result = g95_constant_result(BT_COMPLEX, kind, &k->where); 
 
  mpf_set_ui(result->value.complex.i, 0);      
      
  switch(k->ts.type) {  
  case BT_INTEGER:        
    mpf_set_z(result->value.complex.r, k->value.integer);        
    break;      
      
  case BT_REAL:        
    mpf_set(result->value.complex.r, k->value.real);  
    break;     
     
  case BT_COMPLEX:  
    mpf_set(result->value.complex.r, k->value.complex.r);         
    mpf_set(result->value.complex.i, k->value.complex.i);   
    break;      
      
  default:   
    g95_internal_error("g95_simplify_dcmplx(): Bad type (x)");   
  }   
   
  if (t != NULL) {    
    switch(t->ts.type) {      
    case BT_INTEGER:          
      mpf_set_z(result->value.complex.i, t->value.integer);         
      break;         
         
    case BT_REAL:      
      mpf_set(result->value.complex.i, t->value.real);         
      break;  
  
    default:      
      g95_internal_error("g95_simplify_dcmplx(): Bad type (y)");       
    }
  } 
 
  return range_check(result, name);  
}          
          
          
          
          
g95_expr *g95_simplify_acos(g95_expr *o) {     
g95_expr *result;  
mpf_t negative, square, term;

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
  mpf_init(term);  
  
  mpf_pow_ui(square, o->value.real, 2);     
  mpf_ui_sub(term, 1, square);          
  mpf_sqrt(term, term);          
  mpf_div(term, o->value.real, term);         
  mpf_neg(term, term);      
  arctangent(&term, &negative);          
  mpf_add(result->value.real, half_pi, negative);  
  
  mpf_clear(negative);  
  mpf_clear(square);        
  mpf_clear(term);       
       
  return range_check(result, "ACOS");   
}  
  
  
      
      
g95_expr *g95_simplify_selected_real_kind(g95_expr *x, g95_expr *r) {      
int range, precision, i, kind, found_precision, found_range;         
g95_expr *result;      
      
  if (x == NULL)         
    precision = 0;        
  else {   
    if (x->type != EXPR_CONSTANT || g95_extract_int(x, &precision) != NULL) 
      return NULL;     
  }  
  
  if (r == NULL) 
    range = 0;    
  else { 
    if (r->type != EXPR_CONSTANT || g95_extract_int(r, &range) != NULL)        
      return NULL;         
  }     
     
  kind = INT_MAX;
  found_precision = 0;   
  found_range = 0;        
        
  for(i=0; g95_real_kinds[i].kind!=0; i++) {   
    if (g95_real_kinds[i].precision >= precision) found_precision = 1;         
         
    if (g95_real_kinds[i].range >= range) found_range = 1;      
      
    if (g95_real_kinds[i].precision >= precision &&     
	g95_real_kinds[i].range >= range &&     
	g95_real_kinds[i].kind < kind)    
      kind = g95_real_kinds[i].kind;          
  }       
       
  if (kind == INT_MAX) {        
    kind = 0;         
         
    if (!found_precision) kind = -1; 
    if (!found_range) kind -= 2;          
  }

  result = g95_int_expr(kind);      
  result->where = (x != NULL) ? x->where : r->where;     
     
  return result;       
}     
     
     
         
         
g95_expr *g95_simplify_set_exponent(g95_expr *m, g95_expr *d) {          
g95_expr *result;     
mpf_t s, ln2, absv, lnx, pow2, frac;   
unsigned long exp2;      
      
  if (m->type != EXPR_CONSTANT || d->type != EXPR_CONSTANT) return NULL;       
       
  result=g95_constant_result(BT_REAL, m->ts.kind, &m->where);        
        
  if (mpf_cmp(m->value.real, mpf_zero) == 0) {
    mpf_set(result->value.real, mpf_zero);      
    return result;
  }

  mpf_init_set_ui(s, 2);        
  mpf_init(ln2);          
  mpf_init(absv);         
  mpf_init(lnx);        
  mpf_init(pow2);  
  mpf_init(frac);      
      
  natural_logarithm(&s, &ln2);

  mpf_abs(absv, m->value.real);    
  natural_logarithm(&absv, &lnx);    
    
  mpf_div(lnx, lnx, ln2);  
  mpf_trunc(lnx, lnx);       
  mpf_add_ui(lnx, lnx, 1);    
    
/* old exponent value, and fraction */    
  exp2 = (unsigned long) mpf_get_d(lnx);          
  mpf_pow_ui(pow2, s, exp2);    
    
  mpf_div(frac, absv, pow2); 
 
/* New exponent */
  exp2 = (unsigned long) mpz_get_d(d->value.integer);
  mpf_mul_2exp(result->value.real, frac, exp2);

  mpf_clear(s);    
  mpf_clear(ln2);       
  mpf_clear(absv);        
  mpf_clear(lnx);      
  mpf_clear(pow2); 
  mpf_clear(frac);         
         
  return range_check(result, "SET_EXPONENT");          
}    
    
    
  
  
g95_expr *g95_simplify_ceiling(g95_expr *e, g95_expr *s) { 
g95_expr *ceil, *result;       
int kind;

  kind = get_kind(BT_REAL, s, "CEILING", g95_default_real_kind());       
  if (kind == -1) return &g95_bad_expr;    
    
  if (e->type != EXPR_CONSTANT) return NULL;         
         
  result = g95_constant_result(BT_INTEGER, kind, &e->where);          
          
  ceil = g95_copy_expr(e);  
  
  mpf_ceil(ceil->value.real, e->value.real);   
  mpz_set_f(result->value.integer, ceil->value.real);         
         
  g95_free_expr(ceil); 
 
  return range_check(result, "CEILING");         
}          
          
          
     
     
g95_expr *g95_simplify_rrspacing(g95_expr *w) {
g95_expr *result;
mpf_t k, absv, ln2, lnx, frac, pow2;        
unsigned long exp2;    
int t, n;    
    
  if (w->type != EXPR_CONSTANT) return NULL;     
     
  t = g95_validate_kind(w->ts.type, w->ts.kind);    
  if (t < 0) g95_internal_error("g95_simplify_rrspacing(): bad kind");    
    
  result=g95_constant_result(BT_REAL, w->ts.kind, &w->where);    
    
  n = g95_real_kinds[t].digits;       
       
  if (mpf_cmp(w->value.real, mpf_zero) == 0) {       
    mpf_ui_div(result->value.real, 1, g95_real_kinds[t].tiny); 
    return result;         
  }    
    
  mpf_init_set_ui(k, 2);
  mpf_init(ln2);        
  mpf_init(absv);      
  mpf_init(lnx);          
  mpf_init(frac);
  mpf_init(pow2);       
       
  natural_logarithm(&k, &ln2);  
  
  mpf_abs(absv, w->value.real);   
  natural_logarithm(&absv, &lnx); 
 
  mpf_div(lnx, lnx, ln2);         
  mpf_trunc(lnx, lnx);   
  mpf_add_ui(lnx, lnx, 1);

  exp2 = (unsigned long) mpf_get_d(lnx);   
  mpf_pow_ui(pow2, k, exp2); 
  mpf_div(frac, absv, pow2);   
   
  exp2 = (unsigned long) n;        
  mpf_mul_2exp(result->value.real, frac, exp2);      
      
  mpf_clear(k);    
  mpf_clear(ln2);    
  mpf_clear(absv);     
  mpf_clear(lnx);
  mpf_clear(frac);   
  mpf_clear(pow2);  
  
  return range_check(result, "RRSPACING");         
}          
          
          
  
  
g95_expr *g95_simplify_huge(g95_expr *e) {      
g95_expr *result;    
int p;

  p = g95_validate_kind(e->ts.type, e->ts.kind);        
  if (p == -1) goto bad_type;     
     
  result = g95_constant_result(e->ts.type, e->ts.kind, &e->where);         
         
  switch(e->ts.type) {   
  case BT_INTEGER:  
    mpz_set(result->value.integer, g95_integer_kinds[p].huge);        
    break; 
 
  case BT_REAL:      
    mpf_set(result->value.real, g95_real_kinds[p].huge);          
    break;          
          
  bad_type:        
  default:        
    g95_internal_error("g95_simplify_huge(): Bad type");     
  }   
   
  return result;      
}    
    
    
      
      
g95_expr *g95_simplify_char(g95_expr *e, g95_expr *x) {     
g95_expr *result;
int a, kind;    
    
  kind = get_kind(BT_CHARACTER, x, "CHAR", g95_default_character_kind());         
  if (kind == -1) return &g95_bad_expr;   
   
  if (e->type != EXPR_CONSTANT) return NULL;       
       
  if (g95_extract_int(e, &a) != NULL || a < 0 || a > 255) {
    g95_error("Bad character in CHAR function at %L", &e->where);       
    return &g95_bad_expr;  
  }

  result = g95_constant_result(BT_CHARACTER, kind, &e->where); 
 
  result->value.character.length = 1;        
  result->value.character.string = g95_getmem(2);

  result->value.character.string[0] = a;
  result->value.character.string[1] = '\0';   /* For debugger */      
      
  return result;         
}  
  
  
  
  
g95_expr *g95_simplify_spacing(g95_expr *z) {          
mpf_t n, o, ln2, absv, lnx;   
unsigned long exp2;      
g95_expr *result; 
long diff;   
int b, e;          
          
  if (z->type != EXPR_CONSTANT) return NULL;

  b = g95_validate_kind(z->ts.type, z->ts.kind);
  if (b < 0) g95_internal_error("g95_simplify_spacing(): bad kind");

  e = g95_real_kinds[b].digits;

  result=g95_constant_result(BT_REAL, z->ts.kind, &z->where);    
    
  if (mpf_cmp(z->value.real, mpf_zero) == 0) {         
    mpf_set(result->value.real, g95_real_kinds[b].tiny);    
    return result;      
  }   
   
  mpf_init_set_ui(n, 1);          
  mpf_init_set_ui(o, 2);     
  mpf_init(ln2); 
  mpf_init(absv);
  mpf_init(lnx);      
      
  natural_logarithm(&o, &ln2);         
         
  mpf_abs(absv, z->value.real);         
  natural_logarithm(&absv, &lnx);  
  
  mpf_div(lnx, lnx, ln2);        
  mpf_trunc(lnx, lnx); 
  mpf_add_ui(lnx, lnx, 1);  
  
  diff = (long) mpf_get_d(lnx) - (long) e;  
  if (diff >= 0) {   
    exp2 = (unsigned) diff; 
    mpf_mul_2exp(result->value.real, n, exp2);       
  } else {      
    diff = -diff;         
    exp2 = (unsigned) diff;      
    mpf_div_2exp(result->value.real, n, exp2);   
  }       
       
  mpf_clear(n);         
  mpf_clear(o);    
  mpf_clear(ln2);   
  mpf_clear(absv);          
  mpf_clear(lnx);  
  
  if (mpf_cmp(result->value.real, g95_real_kinds[b].tiny) < 0)          
    mpf_set(result->value.real, g95_real_kinds[b].tiny);      
      
  return range_check(result, "SPACING");    
}     
     
     
         
         
/* simplify_min_max()-- This function is special since MAX() can take
 * any number of arguments.  The simplified expression is a rewritten
 * version of the argument list containing at most one constant
 * element.  Other constant elements are deleted.  Because the
 * argument list has already been checked, this function always
 * succeeds.  sign is 1 for MAX(), -1 for MIN(). */  
  
static g95_expr *simplify_min_max(g95_expr *expr, int sign) {
g95_actual_arglist *arg, *last, *extremum;       
       
  last = NULL;          
  extremum = NULL;          
          
  arg = expr->value.function.actual; 
 
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
      expr->value.function.actual = arg->next;   
    else       
      last->next = arg->next; 
 
    arg->next = NULL;  
    g95_free_actual_arglist(arg); 
    arg = last;
  }      
      
  /* If there is one value left, replace the function call with the
   * expression */        
        
  if (expr->value.function.actual->next != NULL) return NULL;  
  
  return g95_copy_expr(expr->value.function.actual->u.expr);          
}        
        
        
          
          
g95_expr *g95_simplify_nearest(g95_expr *r, g95_expr *w) {          
g95_expr *result;        
float rval;    
double val, eps;     
int p, a, q; 
 
/* TODO: This implementation is dopey and probably not quite right,
 * but it's a start.*/          
          
  if (r->type != EXPR_CONSTANT) return NULL; 
 
  q = g95_validate_kind(r->ts.type, r->ts.kind); 
  if (q == -1) g95_internal_error("g95_simplify_precision(): Bad kind");

  result = g95_constant_result(r->ts.type, r->ts.kind, &r->where);     
     
  val  = mpf_get_d(r->value.real); 
  p    = g95_real_kinds[q].digits;       
       
  eps = 1.;         
  for (a=1;a<p;++a) {       
    eps = eps/2.0;      
  }         
         
  if (mpf_cmp_ui(w->value.real, 0) > 0) { 
    if (q == g95_default_real_kind()) {     
      rval = (float) val;
      rval = rval + eps;
      mpf_set_d(result->value.real, rval);     
    }      
    else {  
      val = val + eps;  
      mpf_set_d(result->value.real, val);      
    }      
  }   
  else if (mpf_cmp_ui(w->value.real, 0) < 0) {   
    if (q == g95_default_real_kind()) {  
      rval = (float) val;  
      rval = rval - eps; 
      mpf_set_d(result->value.real, rval);         
    } 
    else {    
      val = val - eps;      
      mpf_set_d(result->value.real, val);       
    }         
  }   
  else {
    g95_error("Invalid second argument of NEAREST at %L", &w->where);
    g95_free(result);         
    return &g95_bad_expr;          
  }          
          
  return range_check(result, "NEAREST");    
}    
    
    
     
     
g95_expr *g95_simplify_exponent(g95_expr *v) {          
mpf_t q, absv, ln2, lnx;  
g95_expr *result;    
    
  if (v->type != EXPR_CONSTANT) return NULL;         
         
  result=g95_constant_result(BT_INTEGER, g95_default_integer_kind(),         
			     &v->where);          
          
  if (mpf_cmp(v->value.real, mpf_zero) == 0) { 
    mpz_set_ui(result->value.integer, 0);  
    return result;
  }      
      
  mpf_init_set_ui(q, 2);      
  mpf_init(absv);  
  mpf_init(ln2);   
  mpf_init(lnx);    
    
  natural_logarithm(&q, &ln2);      
      
  mpf_abs(absv, v->value.real);    
  natural_logarithm(&absv, &lnx);    
    
  mpf_div(lnx, lnx, ln2);      
  mpf_trunc(lnx, lnx);
  mpf_add_ui(lnx, lnx, 1);        
  mpz_set_f(result->value.integer, lnx);

  mpf_clear(q);     
  mpf_clear(ln2);          
  mpf_clear(lnx);  
  mpf_clear(absv);   
   
  return range_check(result, "EXPONENT");    
}         
         
         
     
     
g95_expr *g95_simplify_epsilon(g95_expr *l) {         
g95_expr *result;          
int y;

  y = g95_validate_kind(l->ts.type, l->ts.kind);
  if (y == -1) g95_internal_error("g95_simplify_epsilon(): Bad kind");       
       
  result = g95_constant_result(BT_REAL, l->ts.kind, &l->where);          
          
  mpf_set(result->value.real, g95_real_kinds[y].epsilon);

  return range_check(result, "EPSILON");       
}  
  
  
          
          
g95_expr *g95_simplify_tan(g95_expr *z) { 
g95_expr *result;          
mpf_t mpf_sin, mpf_cos, mag_cos;          
int e;     
     
  if (z->type != EXPR_CONSTANT) return NULL;        
        
  e = g95_validate_kind(BT_REAL, z->ts.kind);   
  if (e == -1) g95_internal_error("g95_simplify_tan(): bad kind");   
   
  result = g95_constant_result(z->ts.type, z->ts.kind, &z->where);      
      
  mpf_init(mpf_sin);      
  mpf_init(mpf_cos);          
  mpf_init(mag_cos);         
  sine(&z->value.real, &mpf_sin);      
  cosine(&z->value.real, &mpf_cos);         
  mpf_abs(mag_cos, mpf_cos);      
  if (mpf_cmp_ui(mag_cos, 0) == 0) {          
    g95_error("Tangent undefined at %L", &z->where); 
    mpf_clear(mpf_sin);      
    mpf_clear(mpf_cos);      
    mpf_clear(mag_cos);       
    g95_free_expr(result); 
    return &g95_bad_expr;      
  } else if (mpf_cmp(mag_cos, g95_real_kinds[e].tiny) < 0) {       
    g95_error("Tangent cannot be accurately evaluated at %L", &z->where);       
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
          
          
   
   
g95_expr *g95_simplify_ishftc(g95_expr *q, g95_expr *l, g95_expr *sz) {
int y, h, shift, isize, k, bit_size;        
g95_expr *result;   
mpz_t r;  
  
  if (q->type != EXPR_CONSTANT || l->type != EXPR_CONSTANT) return NULL;

  if (g95_extract_int(l, &shift) != NULL) { 
    g95_error("Invalid second argument of ISHFTC at %L", &l->where);        
    return &g95_bad_expr;  
  }       
       
  k = g95_validate_kind(q->ts.type, q->ts.kind);          
  if (k == -1) g95_internal_error("In g95_simplify_ishftc: bad kind");         
         
  bit_size = g95_integer_kinds[k].bit_size;         
         
  if (sz == NULL)        
    isize = g95_integer_kinds[k].bit_size; 
  else {    
    if (g95_extract_int(sz, &isize) != NULL || isize <= 0) {        
      g95_error("Invalid third argument of ISHFTC at %L", &sz->where);     
      return &g95_bad_expr;          
    }      
  }

  mpz_init_set_ui(r, 0);          
          
  for(y=0; y<bit_size; y++) {       
    if (y >= isize)   
      h = y;    
    else        
      h = (y - shift) % isize;         
         
    if (mpz_tstbit(q->value.integer, h)) mpz_setbit(r, y);      
  }         
         
  twos_complement(r, bit_size);    
    
  result = g95_constant_result(q->ts.type, q->ts.kind, &q->where);      
  mpz_set(result->value.integer, r); 
 
  return result;       
}         
         
         
  
  
g95_expr *g95_simplify_iachar(g95_expr *w) {   
g95_expr *result;  
int index;  
  
  if (w->type != EXPR_CONSTANT) return NULL;          
          
  if (w->value.character.length != 1) {        
    g95_error("Argument of IACHAR at %L must be of length one", &w->where); 
    return &g95_bad_expr;     
  }      
      
  index = xascii_table[(int) w->value.character.string[0] & 0xFF];  
  
  result = g95_int_expr(index);   
  result->where = w->where;         
         
  return range_check(result, "IACHAR");
}    
    
    
       
       
/* simplify_sngl()-- The argument is always a double precision real
 * that is converted to single precision.  TODO: Rounding! */       
       
g95_expr *g95_simplify_sngl(g95_expr *f) { 
g95_expr *result;  
  
  if (f->type != EXPR_CONSTANT) return NULL;     
     
  result=g95_real2real(f, g95_default_real_kind());     
  return range_check(result, "SNGL");   
}       
       
       
        
        
g95_expr *g95_simplify_lle(g95_expr *o, g95_expr *w) {          
          
  if (o->type != EXPR_CONSTANT || w->type != EXPR_CONSTANT) return NULL;     
     
  return g95_logical_expr(g95_compare_string(o, w, xascii_table) <= 0,          
			  &o->where); 
}


 
 
g95_expr *g95_simplify_ieor(g95_expr *v, g95_expr *t) {   
g95_expr *result;   
   
  if (v->type != EXPR_CONSTANT || t->type != EXPR_CONSTANT) return NULL;   
   
  result = g95_constant_result(BT_INTEGER, v->ts.kind, &v->where);

  mpz_xor(result->value.integer, v->value.integer, t->value.integer);    
    
  return result;         
}          
          
          
         
         
g95_expr *g95_simplify_achar(g95_expr *r) {     
g95_expr *result;          
int index;          
          
  if (r->type != EXPR_CONSTANT) return NULL;     
     
/* We cannot assume that the native character set is ASCII in this function */    
    
  if (g95_extract_int(r, &index) != NULL  || index < 0 || index > 127) {         
      g95_error("Extended ASCII not implemented: argument of ACHAR at %L "      
		"must be between 0 and 127", &r->where);       
      return &g95_bad_expr; 
  }        
        
  result = g95_constant_result(BT_CHARACTER, g95_default_character_kind(),          
			       &r->where);     
     
  result->value.character.string = g95_getmem(2);    
    
  result->value.character.length = 1;  
  result->value.character.string[0] = ascii_table[index];    
  result->value.character.string[1] = '\0';   /* For debugger */    
  return result;          
}   
   
   
          
          
g95_expr *g95_simplify_dim(g95_expr *s, g95_expr *y) {         
g95_expr *result;   
   
  if (s->type != EXPR_CONSTANT || y->type != EXPR_CONSTANT) return NULL;   
   
  result = g95_constant_result(s->ts.type, s->ts.kind, &s->where); 
 
  switch (s->ts.type) {
  case BT_INTEGER:       
    if (mpz_cmp(s->value.integer, y->value.integer) > 0)
      mpz_sub(result->value.integer, s->value.integer, y->value.integer);      
    else
      mpz_set(result->value.integer, mpz_zero);          
          
    break;          
          
  case BT_REAL:         
    if (mpf_cmp(s->value.real, y->value.real) > 0)  
      mpf_sub(result->value.real, s->value.real, y->value.real);     
    else      
      mpf_set(result->value.real, mpf_zero);  
  
    break;        
        
  default:        
    g95_internal_error("g95_simplify_dim(): Bad type");        
  }   
   
  return range_check(result, "DIM");    
}       
       
       
       
       
g95_expr *g95_simplify_max(g95_expr *s) {  
  
  return simplify_min_max(s, 1);     
} 
 
 
      
      
g95_expr *g95_simplify_radix(g95_expr *g) { 
g95_expr *result;         
int b;        
        
  b = g95_validate_kind(g->ts.type, g->ts.kind);          
  if (b < 0) goto bad;  
  
  switch(g->ts.type) {       
  case BT_INTEGER:  
    b = g95_integer_kinds[b].radix;         
    break;          
          
  case BT_REAL:   
    b = g95_real_kinds[b].radix;  
    break;        
        
  default: bad:        
    g95_internal_error("g95_simplify_radix(): Bad type");   
  } 
 
  result = g95_int_expr(b);  
  result->where = g->where;       
       
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
     
     
       
       
g95_expr *g95_simplify_lge(g95_expr *r, g95_expr *v) {          
          
  if (r->type != EXPR_CONSTANT || v->type != EXPR_CONSTANT) return NULL;   
   
  return g95_logical_expr(g95_compare_string(r, v, xascii_table) >= 0,      
			  &r->where);         
}        
        
        
         
         
g95_expr *g95_simplify_shape(g95_expr *source) {     
mpz_t shape[G95_MAX_DIMENSIONS];
g95_expr *result, *v, *f;     
g95_array_ref *ar;   
int k;       
try m;  
  
  result = g95_start_constructor(BT_INTEGER, g95_default_integer_kind(),  
				 &source->where);          
          
  if (source->rank == 0 || source->type != EXPR_VARIABLE) return result;    
    
  ar = g95_find_array_ref(source);      
      
  m = g95_array_ref_shape(ar, shape);      
      
  for(k=0; k<source->rank; k++) { 
    v = g95_constant_result(BT_INTEGER, g95_default_integer_kind(), 
			    &source->where);        
        
    if (m == SUCCESS) {     
      mpz_set(v->value.integer, shape[k]);         
      mpz_clear(shape[k]);          
    } else {      
      mpz_set_ui(v->value.integer, k+1);          
          
      f = g95_simplify_size(source, v);          
      if (f == NULL) {  
        g95_free_expr (v);  
        g95_free_expr (result);        
        return NULL;  
      } else {        
	g95_free_expr(v);   
	v = f;         
      }       
    }      
      
    g95_append_constructor(result, v); 
  }    
    
  return result;    
}     
     
     
  
  
g95_expr *g95_simplify_trim(g95_expr *e) {       
g95_expr *result;    
int count, i, len, lentrim;          
          
  if (e->type != EXPR_CONSTANT) return NULL; 
 
  len = e->value.character.length;     
     
  result = g95_constant_result(BT_CHARACTER, e->ts.kind, &e->where); 
 
  for (count=0, i=1; i<=len; ++i) {    
    if (e->value.character.string[len-i] == ' ')         
      count++;   
    else       
      break;    
  }          
          
  lentrim = len-count;        
        
  result->value.character.length = lentrim;        
  result->value.character.string = g95_getmem(lentrim+1);  
  
  for(i=0; i<lentrim; i++)     
    result->value.character.string[i] = e->value.character.string[i];  
  
  result->value.character.string[lentrim] = '\0';   /* For debugger */  
  
  return result;       
}     
     
     
     
     
g95_expr *g95_simplify_floor(g95_expr *v, g95_expr *d) { 
g95_expr *result;     
mpf_t floor;  
int kind;     
     
  kind = get_kind(BT_REAL, d, "FLOOR", g95_default_real_kind());
  if (kind == -1) g95_internal_error("g95_simplify_floor(): Bad kind");   
   
  if (v->type != EXPR_CONSTANT) return NULL;        
        
  result = g95_constant_result(BT_INTEGER, kind, &v->where);    
    
  mpf_init(floor);   
  mpf_floor(floor, v->value.real);       
  mpz_set_f(result->value.integer, floor); 
  mpf_clear(floor);      
      
  return range_check(result, "FLOOR"); 
}          
          
          
    
    
g95_expr *g95_simplify_sin(g95_expr *d) {    
g95_expr *result;  
mpf_t xp, xq;         
         
  if (d->type != EXPR_CONSTANT) return NULL; 
 
  result = g95_constant_result(d->ts.type, d->ts.kind, &d->where);

  switch (d->ts.type) {     
  case BT_REAL:      
    sine(&d->value.real, &result->value.real);
    break;

  case BT_COMPLEX:      
    mpf_init(xp); 
    mpf_init(xq);        
        
    sine(&d->value.complex.r, &xp);          
    hypercos(&d->value.complex.i, &xq);    
    mpf_mul(result->value.complex.r, xp, xq); 
 
    cosine(&d->value.complex.r, &xp);      
    hypersine(&d->value.complex.i, &xq);     
    mpf_mul(result->value.complex.i, xp, xq);

    mpf_clear(xp);         
    mpf_clear(xq);
    break;

  default: 
    g95_internal_error("in g95_simplify_sin(): Bad type");        
  }    
    
  return range_check(result, "SIN");
}        
        
        


g95_expr *g95_simplify_cmplx(g95_expr *t, g95_expr *m, g95_expr *l) {    
int kind;  
  
  if (t->type != EXPR_CONSTANT ||      
      (m != NULL && m->type != EXPR_CONSTANT)) return NULL;   
   
  kind = get_kind(BT_REAL, l, "CMPLX", g95_default_real_kind());   
  if (kind == -1) return &g95_bad_expr;         
         
  return simplify_cmplx(t, m, kind, "CMPLX");     
}      
      
      


g95_expr *g95_simplify_fraction(g95_expr *w) {       
g95_expr *result;   
mpf_t z, absv, ln2, lnx, pow2;       
unsigned long exp2;     
     
  if (w->type != EXPR_CONSTANT) return NULL;

  result = g95_constant_result(BT_REAL, w->ts.kind, &w->where); 
 
  if (mpf_cmp(w->value.real, mpf_zero) == 0) {    
    mpf_set(result->value.real, mpf_zero);   
    return result;         
  }     
     
  mpf_init_set_ui(z, 2);      
  mpf_init(absv);         
  mpf_init(ln2); 
  mpf_init(lnx);      
  mpf_init(pow2);     
     
  natural_logarithm(&z, &ln2);       
       
  mpf_abs(absv, w->value.real);
  natural_logarithm(&absv, &lnx);          
          
  mpf_div(lnx, lnx, ln2);       
  mpf_trunc(lnx, lnx);    
  mpf_add_ui(lnx, lnx, 1);       
       
  exp2 = (unsigned long) mpf_get_d(lnx);  
  mpf_pow_ui(pow2, z, exp2); 
 
  mpf_div(result->value.real, absv, pow2);

  mpf_clear(z);          
  mpf_clear(ln2);   
  mpf_clear(absv);          
  mpf_clear(lnx);        
  mpf_clear(pow2);

  return range_check(result, "FRACTION");     
}      
      
      
        
        
/* g95_simplify_reshape()-- This one is a bear, but mainly has to do
 * with shuffling elements. */          
          
g95_expr *g95_simplify_reshape(g95_expr *source, g95_expr *shape_exp,        
			       g95_expr *pad, g95_expr *order_exp) {        
        
int order[G95_MAX_DIMENSIONS], shape[G95_MAX_DIMENSIONS];     
int f, rank, npad, u[G95_MAX_DIMENSIONS];     
g95_constructor *head, *tail;      
mpz_t index, size;      
unsigned long h;
size_t nsource;    
g95_expr *v;         
         
/* Unpack the shape array */      
      
  if (source->type != EXPR_ARRAY || !g95_is_constant_expr(source)) return NULL;    
    
  if (shape_exp->type != EXPR_ARRAY || !g95_is_constant_expr(shape_exp))    
    return NULL;    
    
  if (pad != NULL && (pad->type != EXPR_ARRAY || !g95_is_constant_expr(pad)))
    return NULL;  
  
  if (order_exp != NULL &&     
      (order_exp->type != EXPR_ARRAY || !g95_is_constant_expr(order_exp)))  
    return NULL;         
         
  mpz_init(index);          
  rank = 0; 
  head = tail = NULL;

  for(;;) {         
    v = g95_get_array_element(shape_exp, rank); 
    if (v == NULL) break;      
      
    if (g95_extract_int(v, &shape[rank]) != NULL) {
      g95_error("Integer too large in shape specification at %L",          
		&v->where);    
      g95_free_expr(v);        
      goto bad_reshape;          
    }

    g95_free_expr(v);     
     
    if (rank >= G95_MAX_DIMENSIONS) {  
      g95_error("Too many dimensions in shape specification for RESHAPE "         
		"at %L", &v->where);         
         
      goto bad_reshape;  
    } 
 
    if (shape[rank] < 0) {   
      g95_error("Shape specification at %L cannot be negative", &v->where); 
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
    for(f=0; f<rank; f++)      
      order[f] = f;          
          
  } else { 
 
    for(f=0; f<rank; f++)  
      u[f] = 0; 
 
    for(f=0; f<rank; f++) { 
      v = g95_get_array_element(order_exp, f);          
      if (v == NULL) {     
	g95_error("ORDER parameter of RESHAPE at %L is not the same size "        
		  "as SHAPE parameter", &v->where);    
	goto bad_reshape;
      }

      if (g95_extract_int(v, &order[f]) != NULL) { 
	g95_error("Error in ORDER parameter of RESHAPE at %L", &v->where);        
	g95_free_expr(v);  
	goto bad_reshape;   
      }        
        
      g95_free_expr(v);  
  
      if (order[f] < 1 || order[f] > rank) { 
	g95_error("ORDER parameter of RESHAPE at %L is out of range",     
		  &v->where);          
	goto bad_reshape;   
      } 
 
      order[f]--;          
          
      if (u[order[f]]) {    
	g95_error("Invalid permutation in ORDER parameter at %L", &v->where); 
	goto bad_reshape;    
      }       
       
      u[order[f]] = 1;     
    }       
  }      
      
  /* Count the elements in the source and padding arrays */      
      
  npad = 0;     
  if (pad != NULL) {  
    g95_array_size(pad, &size);          
    npad = mpz_get_ui(size);
    mpz_clear(size);          
  }  
  
  g95_array_size(source, &size);     
  nsource = mpz_get_ui(size);        
  mpz_clear(size);      
      
  /* If it weren't for that pesky permutation we could just loop
   * through the source and round out any shortage with pad elements.
   * But no, someone just had to have the compiler do something the
   * user should be doing. */     
     
  for(f=0; f<rank; f++)      
    u[f] = 0;  
  
  for(;;) {     /* Figure out which element to extract */     
    mpz_set_ui(index, 0);       
       
    for(f=rank-1; f>=0; f--) {          
      mpz_add_ui(index, index, u[order[f]]);        
      if (f != 0) mpz_mul_ui(index, index, shape[order[f-1]]);
    }     
     
    if (mpz_cmp_ui(index, INT_MAX) > 0) { 
      g95_internal_error("Reshaped array too large at %L", &v->where);       
      goto bad_reshape;   
    }       
       
    h = mpz_get_ui(index);      
      
    if (h < nsource)        
      v = g95_get_array_element(source, h);      
    else {   
      h = h - nsource;         
         
      if (npad == 0) {   
	g95_error("PAD parameter required for short SOURCE parameter at %L",  
		  &source->where);    
	goto bad_reshape;         
      }     
     
      h = h % npad;      
      v = g95_get_array_element(pad, h);         
    }     
     
    if (head == NULL)       
      head = tail = g95_get_constructor();         
    else {     
      tail->next = g95_get_constructor();          
      tail = tail->next;       
    }  
  
    if (v == NULL) goto bad_reshape;        
        
    tail->where = v->where;          
    tail->expr = v;        
        
    /* Calculate the next element */    
    
    f = 0; 
  inc:  
    if (++u[f] < shape[f]) continue;     
    u[f++] = 0;      
    if (f < rank) goto inc;    
    
    break; 
  }

  mpz_clear(index); 
 
  v = g95_get_expr();        
  v->where = source->where;       
  v->type = EXPR_ARRAY;   
  v->value.constructor = head; 
  v->shape = g95_get_shape(rank);        
        
  for(f=0; f<rank; f++)  
    mpz_init_set_ui(v->shape[f], shape[order[f]]);      
      
  v->ts = head->expr->ts;         
  v->rank = rank; 
 
  return v;     
     
bad_reshape:   
  g95_free_constructor(head);    
  mpz_clear(index);         
  return &g95_bad_expr;          
}  
  
  
 
 
g95_expr *g95_simplify_aint(g95_expr *y, g95_expr *n) {          
g95_expr *rtrunc, *result;          
int kind;   
   
  kind = get_kind(BT_REAL, n, "AINT", y->ts.kind);  
  if (kind == -1) return &g95_bad_expr;  
  
  if (y->type != EXPR_CONSTANT) return NULL;     
     
  rtrunc = g95_copy_expr(y);

  mpf_trunc(rtrunc->value.real, y->value.real);       
       
  result = g95_real2real(rtrunc, kind);       
  g95_free_expr(rtrunc);     
     
  return range_check(result, "AINT");         
} 
 
 
          
          
g95_expr *g95_simplify_ior(g95_expr *c, g95_expr *e) {          
g95_expr *result;    
    
  if (c->type != EXPR_CONSTANT || e->type != EXPR_CONSTANT) return NULL; 
 
  result = g95_constant_result(BT_INTEGER, c->ts.kind, &c->where);

  mpz_ior(result->value.integer, c->value.integer, e->value.integer);   
  return result; 
}


 
 
g95_expr *g95_simplify_conjg(g95_expr *e) {        
g95_expr *result; 
 
  if (e->type != EXPR_CONSTANT) return NULL;     
     
  result = g95_copy_expr(e);        
  mpf_neg(result->value.complex.i, result->value.complex.i);   
   
  return range_check(result, "CONJG");       
} 
 
 
        
        
g95_expr *g95_simplify_len(g95_expr *j) {        
g95_expr *result;   
   
  if (j->type != EXPR_CONSTANT) return NULL;       
       
  result = g95_constant_result(BT_INTEGER, g95_default_integer_kind(),
			       &j->where);   
   
  mpz_set_si(result->value.integer, j->value.character.length);      
  return range_check(result, "LEN");   
}         
         
         


g95_expr *g95_simplify_asin(g95_expr *s) { 
g95_expr *result;      
mpf_t negative, square, term; 
 
  if (s->type != EXPR_CONSTANT) return NULL;       
       
  if (mpf_cmp_si(s->value.real, 1) > 0 || mpf_cmp_si(s->value.real, -1) < 0) { 
    g95_error("Argument of ASIN at %L must be between -1 and 1", &s->where);
    return &g95_bad_expr;       
  }

  result = g95_constant_result(s->ts.type, s->ts.kind, &s->where);  
  
  if (mpf_cmp_si(s->value.real, 1) == 0) {        
    mpf_set(result->value.real, half_pi);          
    return range_check(result, "ASIN");    
  }

  if (mpf_cmp_si(s->value.real, -1) == 0) {         
    mpf_init(negative);
    mpf_neg(negative, half_pi);  
    mpf_set(result->value.real, negative);      
    mpf_clear(negative);         
    return range_check(result, "ASIN");   
  }     
     
  mpf_init(square);
  mpf_init(term);       
       
  mpf_pow_ui(square, s->value.real, 2);  
  mpf_ui_sub(term, 1, square);       
  mpf_sqrt(term, term);  
  mpf_div(term, s->value.real, term);     
  arctangent(&term, &result->value.real);          
          
  mpf_clear(square); 
  mpf_clear(term);   
   
  return range_check(result, "ASIN");    
}         
         
         
  
  
g95_expr *g95_simplify_tiny(g95_expr *j) {     
g95_expr *result;     
int h;    
    
  h = g95_validate_kind(BT_REAL, j->ts.kind);
  if (h < 0) g95_internal_error("g95_simplify_error(): bad kind");   
   
  result = g95_constant_result(BT_REAL, j->ts.kind, &j->where);         
  mpf_set(result->value.real, g95_real_kinds[h].tiny); 
 
  return result;
}        
        
        
 
 
g95_expr *g95_simplify_sign(g95_expr *q, g95_expr *d) {      
g95_expr *result;         
         
  if (q->type != EXPR_CONSTANT || d->type != EXPR_CONSTANT) return NULL; 
 
  result = g95_constant_result(q->ts.type, q->ts.kind, &q->where);         
         
  switch(q->ts.type) { 
  case BT_INTEGER:    
    mpz_abs(result->value.integer, q->value.integer); 
    if (mpz_sgn(d->value.integer) < 0)    
      mpz_neg(result->value.integer, result->value.integer);    
    
    break;    
    
  case BT_REAL:        
    /* TODO: Handle -0.0 and +0.0 correctly on machines that support it */        
    mpf_abs(result->value.real, q->value.real);      
    if (mpf_sgn(d->value.integer) < 0)     
      mpf_neg(result->value.real, result->value.real);    
    
    break;    
    
  default:       
    g95_internal_error("Bad type in g95_simplify_sign");        
    g95_free_expr(result);     
    return &g95_bad_expr;       
  }

  return result;      
}  
  
  


g95_expr *g95_simplify_ishft(g95_expr *e, g95_expr *x) {   
int u, a, shift, bit_size;   
g95_expr *result;
mpz_t d;  
  
  if (e->type != EXPR_CONSTANT || x->type != EXPR_CONSTANT) return NULL;     
     
  if (g95_extract_int(x, &shift) != NULL) {     
    g95_error("Invalid second argument of ISHFT at %L", &x->where);   
    return &g95_bad_expr;   
  }         
         
  a = g95_validate_kind(BT_INTEGER, e->ts.kind);          
  if (a == -1) g95_internal_error("In g95_simplify_ishft: bad kind");          
          
  bit_size = g95_integer_kinds[a].bit_size;   
   
  mpz_init_set_ui(d, 0);     
     
  for(u=0; u<bit_size; u++) {     
    if (u-shift < 0 || u-shift >= bit_size) continue;       
       
    if (mpz_tstbit(e->value.integer, u-shift)) mpz_setbit(d, u);       
  }     
     
  twos_complement(d, bit_size);      
      
  result = g95_constant_result(e->ts.type, e->ts.kind, &e->where);  
  mpz_set(result->value.integer, d); 
 
  return result;     
}      
      
      
          
          
g95_expr *g95_simplify_index(g95_expr *d, g95_expr *w, g95_expr *h) {        
g95_expr *result;        
int back, len, lensub;      
int z, v, m, count, index=0, start;    
    
  if (d->type != EXPR_CONSTANT || w->type != EXPR_CONSTANT) return NULL;      
      
  if (h != NULL && h->value.logical != 0)    
    back = 1;    
  else         
    back = 0;   
   
  result = g95_constant_result(BT_INTEGER, g95_default_integer_kind(),       
			       &d->where);       
       
  len    = d->value.character.length;         
  lensub = w->value.character.length;        
        
  if (len < lensub) {   
    mpz_set_si(result->value.integer, 0);   
    return result;  
  }          
          
  if (back == 0) {          
          
    if (lensub == 0) {       
      mpz_set_si(result->value.integer, 1);        
      return result;   
    } else if (lensub == 1) {       
      for(z=0; z<len; z++) {   
        for(v=0; v<lensub; v++) {        
    	  if (w->value.character.string[v] == d->value.character.string[z]) {     
	    index = z+1;   
	    goto done;          
	  } 
	}        
      }       
    } else {   
      for(z=0; z<len; z++) {       
        for(v=0; v<lensub; v++) {     
	  if (w->value.character.string[v] == d->value.character.string[z]) {     
	    start = z;         
	    count = 0;         
         
	    for(m=0; m<lensub; m++) {    
    	      if (w->value.character.string[m] ==
		  d->value.character.string[m+start]) count++;      
	    }         
         
	    if (count == lensub) { 
	      index = start+1;   
	      goto done;      
	    }   
	  }          
	}
      }          
    }        
        
  } else {         
         
    if (lensub == 0) {          
      mpz_set_si(result->value.integer, len+1);          
      return result;     
    }         
    else if (lensub == 1) {      
      for(z=0; z<len; z++) {   
        for(v=0; v<lensub; v++) {        
	  if (w->value.character.string[v]==d->value.character.string[len-z]) {        
	    index = len-z+1;   
	    goto done; 
	  }       
	}          
      }  
    } else {   
      for(z=0; z<len; z++) {         
        for(v=0; v<lensub; v++) {      
	  if (w->value.character.string[v]==d->value.character.string[len-z]) {     
	    start = len-z;  
	    if (start <= len-lensub) {
	      count = 0;   
	      for(m=0; m<lensub; m++)          
    	        if (w->value.character.string[m] ==       
		    d->value.character.string[m+start]) count++;          
          
	      if (count == lensub) {  
	        index = start+1;          
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
  mpz_set_si(result->value.integer, index);          
  return range_check(result, "INDEX");          
}    
    
    
         
         
/* invert_table()-- Given a collating table, create the inverse table */  
  
static void invert_table(int *table, int *xtable) {   
int x;  
  
  for(x=0; x<256; x++)         
    xtable[x] = 0;  
  
  for(x=0; x<256; x++)      
    xtable[table[x]] = x;
}  
  
  
  
  
g95_expr *g95_simplify_logical(g95_expr *v, g95_expr *j) {    
g95_expr *result;    
int kind;

  kind = get_kind(BT_LOGICAL, j, "LOGICAL", g95_default_logical_kind());
  if (kind < 0) return &g95_bad_expr; 
 
  if (v->type != EXPR_CONSTANT) return NULL; 
 
  result = g95_constant_result(BT_LOGICAL, kind, &v->where);      
      
  result->value.logical = v->value.logical;

  return result;        
}       
       
       
  
  
g95_expr *g95_simplify_iand(g95_expr *j, g95_expr *d) {    
g95_expr *result; 
 
  if (j->type != EXPR_CONSTANT || d->type != EXPR_CONSTANT) return NULL;  
  
  result = g95_constant_result(BT_INTEGER, j->ts.kind, &j->where);       
       
  mpz_and(result->value.integer, j->value.integer, d->value.integer);         
  return result;      
}       
       
       
         
         
g95_expr *g95_simplify_ifix(g95_expr *y) {     
g95_expr *rtrunc, *result;     
     
  if (y->type != EXPR_CONSTANT) return NULL;   
   
  result = g95_constant_result(BT_INTEGER, g95_default_integer_kind(),       
			       &y->where);

  rtrunc = g95_copy_expr(y);  
  
  mpf_trunc(rtrunc->value.real, y->value.real);         
  mpz_set_f(result->value.integer, rtrunc->value.real);          
          
  g95_free_expr(rtrunc); 
  return range_check(result, "IFIX");     
}   
   
   
      
      
g95_expr *g95_simplify_ichar(g95_expr *v) {     
g95_expr *result;    
int index;          
          
  if (v->type != EXPR_CONSTANT) return NULL;         
         
  if (v->value.character.length != 1) {    
    g95_error("Argument of ICHAR at %L must be of length one", &v->where);          
    return &g95_bad_expr;          
  }     
     
  index = (int) v->value.character.string[0];   
   
  if (index < CHAR_MIN || index > CHAR_MAX) {   
    g95_error("Argument of ICHAR at %L out of range of this processor",   
	      &v->where);
    return &g95_bad_expr;
  }  
  
  result = g95_int_expr(index);     
  result->where = v->where;
  return range_check(result, "ICHAR");
}     
     
     
        
        
g95_expr *g95_simplify_min(g95_expr *m) {          
          
  return simplify_min_max(m, -1);
}     
     
     
    
    
void g95_simplify_init_1(void) {          
          
  mpf_init_set_str(mpf_zero, "0.0", 10);      
  mpf_init_set_str(mpf_half, "0.5", 10); 
  mpf_init_set_str(mpf_one,  "1.0", 10);  
  mpz_init_set_str(mpz_zero,   "0", 10);          
          
  invert_table(ascii_table, xascii_table);   
}    
    
    
      
      
g95_expr *g95_simplify_dcmplx(g95_expr *r, g95_expr *m) {       
       
  if (r->type != EXPR_CONSTANT || (m != NULL && m->type != EXPR_CONSTANT))       
    return NULL;        
        
  return simplify_cmplx(r, m, g95_default_double_kind(), "DCMPLX");    
}      
      
      
          
          
g95_expr *g95_simplify_tanh(g95_expr *b) {         
g95_expr *result;   
mpf_t xp, xq;          
          
  if (b->type != EXPR_CONSTANT) return NULL;     
     
  result = g95_constant_result(b->ts.type, b->ts.kind, &b->where);          
          
  mpf_init(xp);          
  mpf_init(xq);

  hypersine(&b->value.real, &xq);          
  hypercos(&b->value.real, &xp);   
   
  mpf_div(result->value.real, xq, xp);       
       
  mpf_clear(xp);         
  mpf_clear(xq);          
          
  return range_check(result, "TANH");       
}       
       
       
       
       
static g95_expr *simplify_nint(char *name, g95_expr *w, g95_expr *j) { 
g95_expr *rtrunc, *itrunc, *result;
int kind, cmp;     
     
  kind = get_kind(BT_INTEGER, j, name, g95_default_integer_kind());      
  if (kind == -1) return &g95_bad_expr;

  if (w->type != EXPR_CONSTANT) return NULL;   
   
  result = g95_constant_result(BT_INTEGER, kind, &w->where);        
        
  rtrunc = g95_copy_expr(w);   
  itrunc = g95_copy_expr(w);        
        
  cmp = mpf_cmp_ui(w->value.real, 0);         
         
  if (cmp > 0) {    
    mpf_add(rtrunc->value.real, w->value.real, mpf_half);   
    mpf_trunc(itrunc->value.real, rtrunc->value.real); 
  } else if (cmp < 0) {         
    mpf_sub(rtrunc->value.real, w->value.real, mpf_half);  
    mpf_trunc(itrunc->value.real, rtrunc->value.real);        
  } else  
    mpf_set_ui(itrunc->value.real, 0);     
     
  mpz_set_f(result->value.integer, itrunc->value.real);

  g95_free_expr(itrunc);        
  g95_free_expr(rtrunc);   
   
  return range_check(result, name);     
}       
       
       
        
        
g95_expr *g95_simplify_scale(g95_expr *r, g95_expr *g) {  
int c, neg_flag, power, exp_range;       
mpf_t scale, radix;
g95_expr *result; 
 
  if (r->type != EXPR_CONSTANT || g->type != EXPR_CONSTANT) return NULL;    
    
  result = g95_constant_result(BT_REAL, r->ts.kind, &r->where);         
         
  if (mpf_sgn(r->value.real) == 0) {  
    mpf_set_ui(result->value.real, 0);        
    return result;    
  }      
      
  c = g95_validate_kind(BT_REAL, r->ts.kind);          
  exp_range = g95_real_kinds[c].max_exponent - g95_real_kinds[c].min_exponent;       
       
  /* This check filters out values of i that would overflow an int */  
  
  if (mpz_cmp_si(g->value.integer, exp_range+2) > 0 ||    
      mpz_cmp_si(g->value.integer, -exp_range-2) < 0) {   
    g95_error("Result of SCALE overflows its kind at %L", &result->where);    
    return &g95_bad_expr;         
  }      
      
  /* Compute scale = radix ** power */ 
 
  power = mpz_get_si(g->value.integer);    
    
  if (power >= 0)      
    neg_flag = 0;        
  else {       
    neg_flag = 1;
    power = -power;         
  }          
          
  mpf_init_set_ui(radix, g95_real_kinds[c].radix);      
  mpf_init(scale);         
  mpf_pow_ui(scale, radix, power);         
         
  if (neg_flag)         
    mpf_div(result->value.real, r->value.real, scale);       
  else         
    mpf_mul(result->value.real, r->value.real, scale);         
         
  mpf_clear(scale);  
  mpf_clear(radix);    
    
  return range_check(result, "SCALE");        
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
      
      
   
   
g95_expr *g95_simplify_modulo(g95_expr *l, g95_expr *u) {          
g95_expr *result;    
mpf_t quot, iquot, term;       
       
  if (l->type != EXPR_CONSTANT || u->type != EXPR_CONSTANT) return NULL;         
         
  result = g95_constant_result(l->ts.type, l->ts.kind, &l->where); 
 
  switch (l->ts.type) {        
  case BT_INTEGER:  
    if (mpz_cmp_ui(u->value.integer, 0) == 0) {       
      /* Result is processor-dependent, and this processor doesn't handle it */   
      g95_error("Second argument of MODULO at %L is zero", &l->where);  
      g95_free_expr(result);  
      return &g95_bad_expr;      
    }     
    mpz_fdiv_r(result->value.integer, l->value.integer, u->value.integer);         
         
    break;      
      
  case BT_REAL:         
    if (mpf_cmp_ui(u->value.real, 0) == 0) {     
      /* Result is processor-dependent */        
      g95_error("Second argument of MODULO at %L is zero", &u->where);
      g95_free_expr(result);         
      return &g95_bad_expr;    
    }        
        
    mpf_init(quot);     
    mpf_init(iquot);     
    mpf_init(term);        
        
    mpf_div(quot, l->value.real, u->value.real);       
    mpf_floor(iquot, quot);     
    mpf_mul(term, iquot, u->value.real);          
          
    mpf_clear(quot);         
    mpf_clear(iquot);         
    mpf_clear(term);   
   
    mpf_sub(result->value.real, l->value.real, term); 
    break;          
          
  default:
    g95_internal_error("g95_simplify_modulo(): Bad arguments");         
  }     
     
  return range_check(result, "MODULO");       
}      
      
      


g95_expr *g95_simplify_verify(g95_expr *s, g95_expr *set, g95_expr *o) {  
size_t index, len, lenset;       
g95_expr *result;      
int back;  
  
  if (s->type != EXPR_CONSTANT || set->type != EXPR_CONSTANT) return NULL;    
    
  if (o != NULL && o->value.logical != 0)       
    back = 1;         
  else          
    back = 0;   
   
  result = g95_constant_result(BT_INTEGER, g95_default_integer_kind(),          
			       &s->where);   
   
  len    = s->value.character.length;  
  lenset = set->value.character.length;    
    
  if (len == 0) {       
    mpz_set_ui(result->value.integer, 0);      
    return result;         
  }    
    
  if (back == 0) {         
    if (lenset == 0) {        
      mpz_set_ui(result->value.integer, len);      
      return result;      
    }          
          
    index = strspn(s->value.character.string, set->value.character.string) + 1;  
    if (index > len) index = 0;         
         
  } else {        
    if (lenset == 0) {          
      mpz_set_ui(result->value.integer, 1); 
      return result;     
    } 
 
    index = len-strspn(s->value.character.string, set->value.character.string);     
  }     
     
  mpz_set_ui(result->value.integer, index);        
  return result;     
}     
     
     
       
       
g95_expr *g95_simplify_real(g95_expr *e, g95_expr *z) {   
g95_expr *result; 
int kind;       
       
  if (e->ts.type == BT_COMPLEX)           
      kind = get_kind(BT_REAL, z, "REAL", e->ts.kind);      
  else
      kind = get_kind(BT_REAL, z, "REAL", g95_default_real_kind());        
        
  if (kind == -1) return &g95_bad_expr;         
         
  if (e->type != EXPR_CONSTANT) return NULL;     
     
  switch (e->ts.type) {     
  case BT_INTEGER:      
    result = g95_int2real(e, kind);       
    break;   
   
  case BT_REAL:     
    result = g95_real2real(e, kind);          
    break;         
         
  case BT_COMPLEX:          
    result = g95_complex2real(e, kind);         
    break;        
        
  default:       
    g95_internal_error("bad type in REAL"); 
    return &g95_bad_expr;          
  }        
        
  return range_check(result, "REAL"); 
}         
         
  
  
g95_expr *g95_simplify_llt(g95_expr *o, g95_expr *p) { 
 
  if (o->type != EXPR_CONSTANT || p->type != EXPR_CONSTANT) return NULL; 
 
  return g95_logical_expr(g95_compare_string(o, p, xascii_table) < 0,   
			  &o->where);    
}        
        
        
          
          
g95_expr *g95_simplify_ibits(g95_expr *b, g95_expr *c, g95_expr *s) {         
g95_expr *result;          
int pos, len;      
int i, k, bitsize;
int *bits;         
         
  if (b->type != EXPR_CONSTANT || c->type != EXPR_CONSTANT ||  
      s->type != EXPR_CONSTANT) return NULL;        
        
  if (g95_extract_int(c, &pos) != NULL || pos < 0) {    
    g95_error("Invalid second argument of IBITS at %L", &c->where);         
    return &g95_bad_expr;         
  }    
    
  if (g95_extract_int(s, &len) != NULL || len < 0) {   
    g95_error("Invalid third argument of IBITS at %L", &s->where);         
    return &g95_bad_expr;   
  }        
        
  k = g95_validate_kind(BT_INTEGER, b->ts.kind);    
  if (k == -1) g95_internal_error("In g95_simplify_ibits: bad kind");         
         
  bitsize = g95_integer_kinds[k].bit_size;  
  
  if (pos+len > bitsize) {   
    g95_error("Sum of second and third arguments of IBITS exceeds bit size "       
	      "at %L", &c->where);          
    return &g95_bad_expr;   
  }          
          
  result = g95_constant_result(b->ts.type, b->ts.kind, &b->where);       
       
  bits = g95_getmem(bitsize*sizeof(int));       
       
  for(i=0; i<bitsize; i++)        
    bits[i] = 0;          
          
  for(i=0; i<len; i++)      
    bits[i] = mpz_tstbit(b->value.integer, i+pos);         
         
  for(i=0; i<bitsize; i++) {     
    if (bits[i] == 0) {     
      mpz_clrbit(result->value.integer, i);     
    } else if (bits[i] == 1) {       
      mpz_setbit(result->value.integer, i);    
    } else {         
      g95_internal_error("IBITS: Bad bit");      
    }      
  }        
        
  g95_free(bits);          
          
  return range_check(result, "IBITS");      
}    
    
    
        
        
g95_expr *g95_simplify_precision(g95_expr *x) {
g95_expr *result;     
int q;

  q = g95_validate_kind(x->ts.type, x->ts.kind);          
  if (q == -1) g95_internal_error("g95_simplify_precision(): Bad kind");      
      
  result = g95_int_expr(g95_real_kinds[q].precision);     
  result->where = x->where;         
         
  return result; 
}    
    
    
 
 
g95_expr *g95_simplify_dble(g95_expr *z) { 
g95_expr *result; 
 
  if (z->type != EXPR_CONSTANT) return NULL;

  switch (z->ts.type) {  
  case BT_INTEGER:    
    result = g95_int2real(z, g95_default_double_kind());   
    break;         
         
  case BT_REAL:        
    result = g95_real2real(z, g95_default_double_kind());       
    break;        
        
  case BT_COMPLEX:    
    result = g95_complex2real(z, g95_default_double_kind());       
    break;          
          
  default: 
    g95_internal_error("g95_simplify_dble(): bad type at %L", &z->where);   
  }         
         
  return range_check(result, "DBLE");       
} 
 
 
          
          
g95_expr *g95_simplify_idnint(g95_expr *o) {        
        
  return simplify_nint("IDNINT", o, NULL);       
}     
     
     
    
    
g95_expr *g95_simplify_sqrt(g95_expr *f) { 
g95_expr *result;          
mpf_t ac, ad, s, q, w;    
    
  if (f->type != EXPR_CONSTANT) return NULL;          
          
  result = g95_constant_result(f->ts.type, f->ts.kind, &f->where); 
 
  switch (f->ts.type) {       
  case BT_REAL:    
    if (mpf_cmp_si(f->value.real, 0) < 0) goto negative_arg;
    mpf_sqrt(result->value.real, f->value.real);    
    
    break;         
         
  case BT_COMPLEX:          
    /*Formula taken from Numerical Recipes to avoid over- and underflow*/       
       
    mpf_init(ac);
    mpf_init(ad);         
    mpf_init(s);
    mpf_init(q);          
    mpf_init(w);       
       
    if (mpf_cmp_ui(f->value.complex.r, 0) == 0 &&   
	mpf_cmp_ui(f->value.complex.i, 0) == 0) {     
     
      mpf_set_ui(result->value.complex.r, 0);      
      mpf_set_ui(result->value.complex.i, 0);    
      break;   
    }   
   
    mpf_abs(ac, f->value.complex.r);        
    mpf_abs(ad, f->value.complex.i);       
       
    if (mpf_cmp(ac, ad) >= 0) {          
      mpf_div(q, f->value.complex.i, f->value.complex.r);        
      mpf_mul(q, q, q);      
      mpf_add_ui(q, q, 1);  
      mpf_sqrt(q, q); 
      mpf_add_ui(q, q, 1);       
      mpf_div_ui(q, q, 2);          
      mpf_sqrt(q, q);          
      mpf_sqrt(s, ac);    
      mpf_mul(w, s, q);   
    } else { 
      mpf_div(s, f->value.complex.r, f->value.complex.i);    
      mpf_mul(q, s, s);    
      mpf_add_ui(q, q, 1);
      mpf_sqrt(q, q);  
      mpf_abs(s, s);          
      mpf_add(q, q, s);
      mpf_div_ui(q, q, 2);      
      mpf_sqrt(q, q);       
      mpf_sqrt(s, ad);    
      mpf_mul(w, s, q);  
    }          
          
    if (mpf_cmp_ui(w, 0) !=0 && mpf_cmp_ui(f->value.complex.r, 0) >=0) { 
      mpf_mul_ui(q, w, 2);
      mpf_div(result->value.complex.i, f->value.complex.i, q);          
      mpf_set(result->value.complex.r, w);        
    } else if (mpf_cmp_ui(w, 0) !=0 && mpf_cmp_ui(f->value.complex.r, 0) < 0 &&       
	       mpf_cmp_ui(f->value.complex.i, 0) >= 0) {    
      mpf_mul_ui(q, w, 2);         
      mpf_div(result->value.complex.r, f->value.complex.i, q);
      mpf_set(result->value.complex.i, w);   
    } else if (mpf_cmp_ui(w, 0) !=0 && mpf_cmp_ui(f->value.complex.r, 0) < 0 && 
	       mpf_cmp_ui(f->value.complex.i, 0) < 0) { 
      mpf_mul_ui(q, w, 2); 
      mpf_div(result->value.complex.r, ad, q); 
      mpf_neg(w, w);         
      mpf_set(result->value.complex.i, w);  
    } else {    
      g95_internal_error("invalid complex argument of SQRT at %L",   
			 &f->where);    
      mpf_clear(s);  mpf_clear(q); mpf_clear(ac);        
      mpf_clear(ad); mpf_clear(w);        
      g95_free_expr(result);       
      return &g95_bad_expr; 
    }     
     
    mpf_clear(s);          
    mpf_clear(q);      
    mpf_clear(ac);        
    mpf_clear(ad); 
    mpf_clear(w);      
      
    break;         
         
  default:         
    g95_internal_error("invalid argument of SQRT at %L", &f->where);      
    g95_free_expr(result);       
    return &g95_bad_expr;       
  }  
  
  return range_check(result, "SQRT");     
     
 negative_arg:       
  g95_free_expr(result);          
  g95_error("Argument of SQRT at %L has a negative value", &f->where);  
  return &g95_bad_expr;    
}   
   
   
   
   
g95_expr *g95_simplify_nint(g95_expr *g, g95_expr *f) {     
     
  return simplify_nint("NINT", g, f);  
}          
          
          


g95_expr *g95_simplify_not(g95_expr *n) {
g95_expr *result;  
int a; 
 
  if (n->type != EXPR_CONSTANT) return NULL;      
      
  result = g95_constant_result(n->ts.type, n->ts.kind, &n->where);     
     
  mpz_com(result->value.integer, n->value.integer);  
  
  /* Because of how GMP handles numbers, the result must be ANDed with
   * the max_int mask.  For radices <> 2, this will require change */    
    
  a = g95_validate_kind(BT_INTEGER, n->ts.kind);
  mpz_and(result->value.integer, result->value.integer,       
	  g95_integer_kinds[a].max_int);    
    
  return result;        
}      
      
      
   
   
g95_expr *g95_simplify_int(g95_expr *w, g95_expr *o) { 
g95_expr *rpart, *rtrunc, *result;      
int kind;   
   
  kind = get_kind(BT_REAL, o, "INT", g95_default_real_kind());        
  if (kind == -1) return &g95_bad_expr;  
  
  if (w->type != EXPR_CONSTANT) return NULL;    
    
  result = g95_constant_result(BT_INTEGER, kind, &w->where);   
   
  switch(w->ts.type) {         
  case BT_INTEGER:         
    mpz_set(result->value.integer, w->value.integer);   
    break;

  case BT_REAL:    
    rtrunc = g95_copy_expr(w);  
    mpf_trunc(rtrunc->value.real, w->value.real);    
    mpz_set_f(result->value.integer, rtrunc->value.real);     
    g95_free_expr(rtrunc);    
    break; 
 
  case BT_COMPLEX:
    rpart = g95_complex2real(w, kind);     
    rtrunc = g95_copy_expr(rpart); 
    mpf_trunc(rtrunc->value.real, rpart->value.real); 
    mpz_set_f(result->value.integer, rtrunc->value.real);  
    g95_free_expr(rpart);  
    g95_free_expr(rtrunc);         
    break;         
         
  default:      
    g95_error("Argument of INT at %L is not a valid type", &w->where);          
    g95_free_expr(result);    
    return &g95_bad_expr;      
  }       
       
  return range_check(result, "INT");
}      
      
      
