/* Intrinsic function resolution
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
        
        
/* iresolve.c-- assign name and types to intrinsic procedures.  For
 * functions, the first argument to a resolution function is an
 * expression pointer to the original function node and the rest are
 * pointers to the arguments of the function call.  For subroutines,
 * a pointer to the code node is passed.
 *
 * The result type and library subroutine name are generally set
 * according to the function arguments. */     
     
#include <string.h>
#include <stdarg.h>
      
#include "g95.h"
#include "intrinsic.h"
  
  
/* String pool subroutines.  This are used to provide static locations
 * for the string constants that represent library function names. */   
   
typedef struct string_node {       
  struct string_node *next;    
  char string[1];
} string_node;    
    
#define HASH_SIZE 13
    
static string_node *string_head[HASH_SIZE];        
        
        
      
      
void g95_resolve_eoshift(g95_expr *d, g95_expr *ap, g95_expr *shift,    
      		                   g95_expr *boundary, g95_expr *r) {        
        
  d->ts = ap->ts;  
  d->rank = ap->rank;        
        
  if (boundary == NULL || boundary->rank == 0)
    d->value.function.name =         
      g95_get_string(PREFIX "eoshift%d_%c%d", (shift->rank == 0) ? 1 : 2,   
		     g95_type_letter(ap->ts.type), ap->ts.kind);    
  else
    d->value.function.name = g95_get_string(PREFIX "eoshift%d",        
					    (shift->rank == 0) ? 3 : 4);    
}     
     
     
       
       
void g95_resolve_char(g95_expr *d, g95_expr *o, g95_expr *k) {         
         
  d->ts.type = BT_CHARACTER; 
  d->ts.kind = (k == NULL) ? g95_default_character_kind()         
    : mpz_get_si(k->value.integer);

  d->value.function.name = g95_get_string(PREFIX "char_%d", d->ts.kind);      
}         
         
         
 
 
void g95_resolve_sin(g95_expr *h, g95_expr *p) {     
     
  h->ts = p->ts;       
  h->value.function.name =  
    g95_get_string(PREFIX "sin_%c%d", g95_type_letter(p->ts.type), p->ts.kind); 
} 
 
 
     
     
/* hash()-- Return a hash code based on the name */ 
 
static int hash(char *name) {         
int m;

  m = 1;        
  while(*name)  
    m = 5311966*m + *name++;   
   
  if (m < 0) m = -m; 
  return m % HASH_SIZE;     
}      
      
      
         
         
void g95_resolve_trim(g95_expr *e, g95_expr *str) { 
 
  e->ts.type = BT_CHARACTER;        
  e->ts.kind = str->ts.kind;   
  e->value.function.name = g95_get_string(PREFIX "trim_%d", str->ts.kind);     
}  
  
  
      
      
void g95_resolve_ibset(g95_expr *b, g95_expr *i, g95_expr *pos) { 
 
  b->ts = i->ts;          
  b->value.function.name = g95_get_string(PREFIX "ibset_%d", i->ts.kind);    
}     
     
     
  
  
void g95_resolve_int(g95_expr *n, g95_expr *m, g95_expr *knd) {     
     
  n->ts.type = BT_INTEGER;         
  n->ts.kind = (knd == NULL) ? g95_default_integer_kind()    
    : mpz_get_si(knd->value.integer); 
 
  n->value.function.name =   
    g95_get_string(PREFIX "int_%d_%c%d", n->ts.kind,
		   g95_type_letter(m->ts.type), m->ts.kind);        
}         
         
         
       
       
void g95_resolve_aimag(g95_expr *z, g95_expr *j) {  
  
  z->ts.type = BT_REAL;      
  z->ts.kind = j->ts.kind;
  z->value.function.name = g95_get_string(PREFIX "aimag_%d", j->ts.kind);     
}


       
       
void g95_resolve_ubound(g95_expr *d, g95_expr *block, g95_expr *r) {    
static char ubound[] = PREFIX "ubound";        
        
  d->ts.type = BT_INTEGER;       
  d->ts.kind = g95_default_integer_kind();       
       
  d->value.function.name = ubound; 
 
  if (r == NULL) d->rank = 1;  
} 
 
 
       
       
void g95_resolve_exp(g95_expr *w, g95_expr *l) {    
    
  w->ts = l->ts;
  w->value.function.name =      
    g95_get_string(PREFIX "exp_%c%d", g95_type_letter(l->ts.type), l->ts.kind);          
}     
     
     
  
  
static void free_strings(void) {     
string_node *p, *m;          
int c;

  for(c=0; c<HASH_SIZE; c++) {         
    for(p=string_head[c]; p; p=m) {      
      m = p->next;     
      g95_free(p); 
    }     
  }          
} 
 
 
       
       
void g95_resolve_cpu_time(g95_code *p) {          
          
  p->sub_name = g95_get_string(PREFIX "cpu_time_%d",        
			       p->ext.actual->u.expr->ts.kind);          
} 
 
 
        
        
/* g95_get_string()-- Given printf-like arguments, return a static
 * address of the resulting string.  If the name is not in the table,
 * it is added. */       
       
char *g95_get_string(char *format, ...) {       
char temp_name[50];     
string_node *m;     
va_list a;   
int s;      
      
  va_start(a, format);         
  vsprintf(temp_name, format, a);  
  va_end(a);    
    
  s = hash(temp_name); 
 
  /* Search */      
      
  for(m=string_head[s]; m; m=m->next)    
    if (strcmp(m->string, temp_name) == 0) return m->string;       
       
  /* Add */     
     
  m = g95_getmem(sizeof(string_node) + strlen(temp_name));

  strcpy(m->string, temp_name);          
          
  m->next = string_head[s];     
  string_head[s] = m;

  return m->string;          
}      
      
      
   
   
void g95_resolve_ichar(g95_expr *i, g95_expr *l) {

  i->ts.type = BT_INTEGER;        
  i->ts.kind = g95_default_integer_kind();        
        
  i->value.function.name = g95_get_string(PREFIX "ichar_%d", l->ts.kind);       
}          
          
          
       
       
void g95_resolve_all(g95_expr *e, g95_expr *maski, g95_expr *dim) {    
    
  e->ts = maski->ts;     
     
  if (dim == NULL || maski->rank == 1)     
    e->value.function.name = g95_get_string(PREFIX "all_%d", maski->ts.kind);
  else { 
    e->rank = maski->rank - 1;    
    e->value.function.name = g95_get_string(PREFIX "alld_%d", maski->ts.kind); 
  }       
}     
     
     
          
          
void g95_resolve_iand(g95_expr *a, g95_expr *s, g95_expr *c) {         
         
  a->ts = s->ts;      
  a->value.function.name = g95_get_string(PREFIX "iand_%d", s->ts.kind);         
}




void g95_resolve_ibits(g95_expr *j, g95_expr *g, g95_expr *pos,
		       g95_expr *len) { 
 
  j->ts = g->ts;
  j->value.function.name = g95_get_string(PREFIX "ibits_%d", g->ts.kind);        
}


         
         
void g95_resolve_ishftc(g95_expr *a, g95_expr *r, g95_expr *shift,     
			g95_expr *siz) {      
int s_kind;

  s_kind = (siz == NULL) ? g95_default_integer_kind() : shift->ts.kind;      
      
  a->ts = r->ts;       
  a->value.function.name =        
    g95_get_string(PREFIX "ishftc_%d_%d_%d", r->ts.kind, shift->ts.kind,      
		   s_kind);   
}




void g95_resolve_exponent(g95_expr *o, g95_expr *x) {

  o->ts.type = BT_INTEGER;
  o->ts.kind = g95_default_integer_kind();      
      
  o->value.function.name = g95_get_string(PREFIX "exponent_%d", x->ts.kind);  
}


          
          
void g95_resolve_reshape(g95_expr *v, g95_expr *s1, g95_expr *shap,         
                         g95_expr *pad, g95_expr *order) { 
mpz_t r;          
          
  v->ts = s1->ts; 
 
  g95_array_size(shap, &r);      
  v->rank = mpz_get_si(r);  
  mpz_clear(r);  
  
  v->value.function.name = g95_get_string(PREFIX "reshape");     
}      
      
      
  
  
void g95_resolve_tanh(g95_expr *j, g95_expr *b) {     
     
  j->ts = b->ts;         
  j->value.function.name =   
    g95_get_string(PREFIX "tanh_%c%d", g95_type_letter(b->ts.type),          
		   b->ts.kind);          
}       
       
       
 
 
void g95_resolve_rrspacing(g95_expr *s, g95_expr *x) {    
    
  s->ts = x->ts;
  s->value.function.name = g95_get_string(PREFIX "rrspacing_%d", x->ts.kind);  
}   
   
   


void g95_resolve_not(g95_expr *w, g95_expr *s) {      
      
  w->ts = s->ts;        
  w->value.function.name = g95_get_string(PREFIX "not_%d", s->ts.kind);    
}          
          
          
       
       
void g95_resolve_shape(g95_expr *y, g95_expr * array) {      
      
  y->ts.type = BT_INTEGER;      
  y->ts.kind = g95_default_integer_kind();         
  y->rank = 1;
  y->value.function.name = PREFIX "shape";         
}


      
      
void g95_resolve_dim(g95_expr *k, g95_expr *e, g95_expr *c) {         
         
  k->ts = e->ts;
  k->value.function.name =  
    g95_get_string(PREFIX "dim_%c%d", g95_type_letter(e->ts.type), e->ts.kind);        
}     
     
     
         
         
void g95_resolve_abs(g95_expr *k, g95_expr *c) {      
      
  k->ts = c->ts;     
  if (k->ts.type == BT_COMPLEX) k->ts.type = BT_REAL;          
          
  k->value.function.name =          
    g95_get_string(PREFIX "abs_%c%d", g95_type_letter(c->ts.type), c->ts.kind);          
}    
    
    
         
         
void g95_resolve_log(g95_expr *l, g95_expr *g) {  
  
  l->ts = g->ts;        
  l->value.function.name =  
    g95_get_string(PREFIX "log_%c%d", g95_type_letter(g->ts.type), g->ts.kind); 
}          
          
          
  
  
void g95_resolve_nint(g95_expr *v, g95_expr *n, g95_expr *kind) {          
          
  v->ts.type = BT_INTEGER;        
  v->ts.kind = (kind == NULL) ? g95_default_integer_kind()       
    : mpz_get_si(kind->value.integer);         
         
  v->value.function.name = g95_get_string(PREFIX "nint_%d", n->ts.kind);   
}    
    
    
    
    
void g95_resolve_unpack(g95_expr *i, g95_expr *vector, g95_expr *m,   
			                              g95_expr *field) { 
 
  i->ts.type = vector->ts.type;   
  i->ts.kind = vector->ts.kind;  
  i->rank = m->rank;      
      
  i->value.function.name =        
    g95_get_string(field->rank != 0 ? PREFIX "unpack" : PREFIX "unpack_s");         
}        
        
        
 
 
void g95_resolve_sqrt(g95_expr *m, g95_expr *x) {  
  
  m->ts = x->ts;         
  m->value.function.name =          
    g95_get_string(PREFIX "sqrt_%c%d", g95_type_letter(x->ts.type),          
		   x->ts.kind);    
}   
   
   
      
      
void g95_resolve_mod(g95_expr *k, g95_expr *d, g95_expr *n) {         
         
  k->ts = d->ts;       
  k->value.function.name =
    g95_get_string(PREFIX "mod_%c%d", g95_type_letter(d->ts.type), d->ts.kind);         
}       
       
       
   
   
void g95_resolve_sign(g95_expr *j, g95_expr *z, g95_expr *v) {        
        
  j->ts = z->ts;      
  j->value.function.name =
    g95_get_string(PREFIX "sign_%c%d", g95_type_letter(z->ts.type),        
		   z->ts.kind);   
}       
       
       
   
   
void g95_iresolve_init_1(void) {        
int p;          
          
  for(p=0; p<HASH_SIZE; p++)
    string_head[p] = NULL;          
}  
  
  
         
         
void g95_resolve_transfer(g95_expr *c, g95_expr *s, g95_expr *mold,  
			  g95_expr *size) {

  c->ts = mold->ts;        
        
  if (size == NULL && mold->rank == 0)      
    c->value.function.name = PREFIX "transfer";     
  else {      
    c->value.function.name = 
      (s->rank == 0) ? PREFIX "transfer1" : PREFIX "transfer2";      
      
    c->rank = 1; 
  }         
}          
          
          
    
    
void g95_resolve_min_max(g95_expr *j, g95_intrinsic_sym *t, g95_expr *p) {

  j->ts = p->ts;    
  j->value.function.name = t->name;       
       
  if (t->ts.type != BT_UNKNOWN &&      
      (t->ts.type != p->ts.type || t->ts.kind != p->ts.kind))        
    j->ts = t->ts;      
} 
 
 
       
       
void g95_resolve_any(g95_expr *v, g95_expr *msk, g95_expr *d) {       
       
  v->ts = msk->ts;          
          
  if (d == NULL || msk->rank == 1) 
    v->value.function.name = g95_get_string(PREFIX "any_%d", msk->ts.kind);    
  else {        
    v->rank = msk->rank - 1;     
    v->value.function.name = g95_get_string(PREFIX "anyd_%d", msk->ts.kind);          
  }  
}         
         
         


void g95_resolve_ieor(g95_expr *f, g95_expr *q, g95_expr *x) {       
       
  f->ts = q->ts; 
  f->value.function.name = g95_get_string(PREFIX "ieor_%d", q->ts.kind);     
}


    
    
void g95_resolve_modulo(g95_expr *z, g95_expr *n, g95_expr *t) {       
       
  z->ts = n->ts;
  z->value.function.name =     
    g95_get_string(PREFIX "modulo_%c%d", g95_type_letter(n->ts.type),     
		   n->ts.kind);     
}     
     
     
          
          
void g95_resolve_cos(g95_expr *s, g95_expr *y) {     
     
  s->ts = y->ts;  
  s->value.function.name =  
    g95_get_string(PREFIX "cos_%c%d", g95_type_letter(y->ts.type), y->ts.kind); 
}          
          
          
     
     
void g95_resolve_real(g95_expr *o, g95_expr *w, g95_expr *k0) {       
       
  o->ts.type = BT_REAL;      
      
  if (k0 != NULL)      
    o->ts.kind = mpz_get_si(k0->value.integer);       
  else     
    o->ts.kind = (w->ts.type == BT_COMPLEX) ?         
      w->ts.kind : g95_default_real_kind();          
          
  o->value.function.name =          
    g95_get_string(PREFIX "real_%d_%c%d", o->ts.kind,      
		   g95_type_letter(w->ts.type), w->ts.kind);
}     
     
     
   
   
void g95_resolve_lbound(g95_expr *e, g95_expr *ap, g95_expr *d) { 
static char lbound[] = PREFIX "lbound";          
          
  e->ts.type = BT_INTEGER;   
  e->ts.kind = g95_default_integer_kind();          
          
  e->value.function.name = lbound; 
 
  if (d == NULL) e->rank = 1;         
} 
 
 


void g95_resolve_count(g95_expr *k, g95_expr *mask, g95_expr *r) {  
  
  k->ts.type = BT_INTEGER;   
  k->ts.kind = g95_default_integer_kind();    
    
  if (r == NULL || mask->rank == 1)         
    k->value.function.name = g95_get_string(PREFIX "count_%d", mask->ts.kind);  
  else {      
    k->rank = mask->rank - 1; 
    k->value.function.name = g95_get_string(PREFIX "countd_%d", mask->ts.kind);  
  }   
}          
          
          
        
        
void g95_resolve_acos(g95_expr *r, g95_expr *s) {   
   
  r->ts = s->ts;  
  r->value.function.name = 
    g95_get_string(PREFIX "acos_%c%d", g95_type_letter(s->ts.type),        
		   s->ts.kind);    
} 
 
 
  
  
void g95_resolve_scan(g95_expr *d, g95_expr *str, g95_expr *set,  
		      g95_expr *back) {  
  
  d->ts.type = BT_INTEGER; 
  d->ts.kind = g95_default_integer_kind();     
  d->value.function.name = g95_get_string(PREFIX "scan_%d", str->ts.kind);    
}




void g95_resolve_atan2(g95_expr *s, g95_expr *o, g95_expr *p) { 
 
  s->ts = o->ts;     
  s->value.function.name =     
    g95_get_string(PREFIX "atan2_%c%d", g95_type_letter(o->ts.type),       
		   o->ts.kind);         
} 
 
 
        
        
void g95_resolve_maxloc(g95_expr *h, g95_expr *ap, g95_expr *dim,   
			g95_expr *mask) {        
char *suffix;  
  
  h->ts.type = BT_INTEGER;    
  h->ts.kind = g95_default_integer_kind();       
       
  if (dim == NULL) {          
    h->rank = 1; 
    suffix = ""; 
  } else {      
    h->rank = ap->rank - 1;     
    suffix = (ap->rank == 1) ? "1" : "d"; 
  }      
      
  h->value.function.name =
    g95_get_string(PREFIX "maxloc%s_%c%d", suffix,          
		   g95_type_letter(ap->ts.type), ap->ts.kind);         
} 
 
 
         
         
void g95_resolve_ior(g95_expr *l, g95_expr *q, g95_expr *x) {     
     
  l->ts = q->ts;     
  l->value.function.name = g95_get_string(PREFIX "ior_%d", q->ts.kind);     
}        
        
        
        
        
void g95_resolve_maxval(g95_expr *t, g95_expr *array, g95_expr *dim,      
			g95_expr *mask) {

  t->ts = array->ts; 
 
  if (dim != NULL && array->rank != 1)       
    t->rank = array->rank - 1;

  t->value.function.name =   
    g95_get_string(PREFIX "%s_%c%d", (dim == NULL) ? "maxval" : "maxvald",       
                   g95_type_letter(array->ts.type), array->ts.kind);      
}        
        
        


void g95_resolve_len_trim(g95_expr *k, g95_expr *str) {          
          
  k->ts.type = BT_INTEGER; 
  k->ts.kind = g95_default_integer_kind();         
  k->value.function.name = g95_get_string(PREFIX "len_trim_%d", 
					  str->ts.kind);   
}         
         
         
     
     
void g95_resolve_spread(g95_expr *e, g95_expr *s0, g95_expr *r, 
			g95_expr *ncopies) {          
          
  e->ts.type = s0->ts.type;          
  e->ts.kind = s0->ts.kind;         
  e->rank = s0->rank + 1;      
  e->value.function.name =    
    g95_get_string(s0->rank > 0 ? PREFIX "spread" : PREFIX "spread_s");      
}      
      
      
       
       
void g95_resolve_repeat(g95_expr *q, g95_expr *s, g95_expr *ncopies) { 
 
  q->ts.type = BT_CHARACTER; 
  q->ts.kind = s->ts.kind; 
  q->value.function.name = g95_get_string(PREFIX "repeat_%d", s->ts.kind);     
} 
 
 
     
     
void g95_resolve_pack(g95_expr *k, g95_expr *a, g95_expr *m, 
		      g95_expr *vector) {     
     
  k->ts = a->ts;       
  k->rank = 1;         
         
  k->value.function.name =    
    g95_get_string(m->rank != 0 ? PREFIX "pack" : PREFIX "pack_s");     
}       
       
       
   
   
void g95_resolve_atan(g95_expr *v, g95_expr *b) {   
   
  v->ts = b->ts;   
  v->value.function.name =    
    g95_get_string(PREFIX "atan_%c%d", g95_type_letter(b->ts.type),         
		   b->ts.kind);        
}     
     
     
      
      
void g95_resolve_dot_product(g95_expr *c, g95_expr *r, g95_expr *l) {
g95_expr temp;     
     
  if (r->ts.type == BT_LOGICAL && l->ts.type == BT_LOGICAL) {
    c->ts.type = BT_LOGICAL;     
    c->ts.kind = g95_default_logical_kind();
  } else {      
    temp.type = EXPR_OP;
    g95_clear_ts(&temp.ts);         
    temp.operator = INTRINSIC_NONE;    
    temp.op1 = r;   
    temp.op2 = l;       
    g95_type_convert_binary(&temp);          
    c->ts = temp.ts;       
  }       
       
  c->value.function.name =  
    g95_get_string(PREFIX "dot_product_%c%d", g95_type_letter(c->ts.type), 
		   c->ts.kind);   
} 
 
 
          
          
void g95_resolve_merge(g95_expr *n, g95_expr *tsource, g95_expr *fsource,  
		       g95_expr *m) {       
       
  n->ts = tsource->ts;  
  n->value.function.name =       
    g95_get_string(PREFIX "merge_%c%d", g95_type_letter(tsource->ts.type),       
		   tsource->ts.kind);     
}     
     
     


void g95_iresolve_done_1(void) {          
          
  free_strings();          
}  
  
          
          
void g95_resolve_dble(g95_expr *g, g95_expr *v) {        
        
  g->ts.type = BT_REAL; 
  g->ts.kind = g95_default_double_kind();       
  g->value.function.name =          
    g95_get_string(PREFIX "dble_%c%d", g95_type_letter(v->ts.type),    
		   v->ts.kind);     
} 
 
 
   
   
void g95_resolve_idnint(g95_expr *y, g95_expr *a) {        
  g95_resolve_nint(y, a, NULL);   
}     
     
     
    
    
void g95_resolve_achar(g95_expr *v, g95_expr *i) {  
  
  v->ts.type = BT_CHARACTER; 
  v->ts.kind = g95_default_character_kind(); 
 
  v->value.function.name = g95_get_string(PREFIX "char_%d", v->ts.kind);         
}      
      
      
      
      
void g95_resolve_cosh(g95_expr *r, g95_expr *c) {

  r->ts = c->ts;       
  r->value.function.name =        
    g95_get_string(PREFIX "cosh_%c%d", g95_type_letter(c->ts.type),          
		   c->ts.kind);       
}        
        
        
        
        
void g95_resolve_set_exponent(g95_expr *n, g95_expr *q, g95_expr *h) {      
      
  n->ts = q->ts;
  n->value.function.name =       
    g95_get_string(PREFIX "set_exponent_%d", q->ts.kind);          
}    
    
    
     
     
void g95_resolve_minval(g95_expr *b, g95_expr *block, g95_expr *d,         
			     g95_expr *mask) {    
    
  b->ts = block->ts;  
  
  if (d != NULL && block->rank != 1)        
    b->rank = block->rank - 1;     
     
  b->value.function.name =       
    g95_get_string(PREFIX "%s_%c%d", (d == NULL) ? "minval" : "minvald",       
                   g95_type_letter(block->ts.type), block->ts.kind); 
} 
 
 
     
     
void g95_resolve_spacing(g95_expr *p, g95_expr *s) {    
    
  p->ts = s->ts;   
  p->value.function.name = g95_get_string(PREFIX "spacing_%d", s->ts.kind);        
}        
        
        
      
      
void g95_resolve_cmplx(g95_expr *k, g95_expr *r, g95_expr *e, g95_expr *k0) {         
         
  k->ts.type = BT_COMPLEX;        
  k->ts.kind = (k0 == NULL) 
    ? g95_default_real_kind()        
    : mpz_get_si(k0->value.integer);      
      
  k->value.function.name = g95_get_string(PREFIX "cmplx");   
}        
        
        
     
     
void g95_resolve_log10(g95_expr *i, g95_expr *z) {       
       
  i->ts = z->ts;         
  i->value.function.name =
    g95_get_string(PREFIX "log10_%c%d", g95_type_letter(z->ts.type),          
		   z->ts.kind);
}    
    
    
          
          
void g95_resolve_fraction(g95_expr *m, g95_expr *l) {    
    
  m->ts = l->ts;    
  m->value.function.name = g95_get_string(PREFIX "fraction_%d", l->ts.kind);   
}   
   
   
      
      
void g95_resolve_cshift(g95_expr *y, g95_expr *block, g95_expr *shift,        
			g95_expr *dim) {

  y->ts = block->ts;  
  y->rank = block->rank;        
        
  y->value.function.name =        
    g95_get_string(PREFIX "cshift%d", (shift->rank == 0) ? 1 : 2);        
}         
         
         


void g95_resolve_verify(g95_expr *n, g95_expr *str, g95_expr *set,         
	                                       	     g95_expr *back) {   
   
  n->ts.type = BT_INTEGER;      
  n->ts.kind = g95_default_integer_kind();         
  n->value.function.name = g95_get_string(PREFIX "verify_%d", str->ts.kind);         
} 
 
 
      
      
void g95_resolve_ibclr(g95_expr *o, g95_expr *c, g95_expr *p) { 
 
  o->ts = c->ts;          
  o->value.function.name = g95_get_string(PREFIX "ibclr_%d", c->ts.kind);     
}   
   
   
       
       
void g95_resolve_tan(g95_expr *j, g95_expr *n) {      
      
  j->ts = n->ts;          
  j->value.function.name = 
    g95_get_string(PREFIX "tan_%c%d", g95_type_letter(n->ts.type), n->ts.kind);       
} 
 
 
      
      
void g95_resolve_product(g95_expr *u, g95_expr *a, g95_expr *dim,  
			 g95_expr *mask) {    
char *suffix;     
     
  u->ts = a->ts;

  if (dim == NULL || a->rank == 1) {     
    u->rank = 0;    
    suffix = "";        
  } else {   
    u->rank = a->rank - 1;      
    suffix = "d";      
  }  
  
  u->value.function.name =    
    g95_get_string(PREFIX "product%s_%c%d", suffix,   
                   g95_type_letter(a->ts.type), a->ts.kind); 
}    
    
    


void g95_resolve_anint(g95_expr *h, g95_expr *b, g95_expr *kind) {     
     
  h->ts.type = b->ts.type;
  h->ts.kind = (kind == NULL) ? b->ts.kind         
    : mpz_get_si(kind->value.integer);  
  
  h->value.function.name =     
    g95_get_string(PREFIX "anint%d_%d", h->ts.kind, b->ts.kind);   
}     
     
     
  
  
void g95_resolve_floor(g95_expr *y, g95_expr *b, g95_expr *k) {        
        
  y->ts.type = BT_INTEGER;        
  y->ts.kind = (k == NULL) ? g95_default_integer_kind()
    : mpz_get_si(k->value.integer);    
    
  y->value.function.name = g95_get_string(PREFIX "floor_%d", b->ts.kind);        
}      
      
      
        
        
void g95_resolve_matmul(g95_expr *k, g95_expr *c, g95_expr *j) {       
g95_expr t0;      
      
  if (c->ts.type == BT_LOGICAL && j->ts.type == BT_LOGICAL) {         
    k->ts.type = BT_LOGICAL;          
    k->ts.kind = g95_default_logical_kind();
  } else {    
    t0.type = EXPR_OP;  
    g95_clear_ts(&t0.ts);      
    t0.operator = INTRINSIC_NONE;     
    t0.op1 = c;    
    t0.op2 = j;   
    g95_type_convert_binary(&t0);      
    k->ts = t0.ts;   
  }      
      
  k->rank = (c->rank == 2 && j->rank == 2) ? 2 : 1; 
 
  k->value.function.name =          
    g95_get_string(PREFIX "matmul%d%d_%c%d", c->rank, j->rank,      
		   g95_type_letter(k->ts.type), k->ts.kind);        
} 
 
 
  
  
void g95_resolve_ishft(g95_expr *n, g95_expr *j, g95_expr *shift) {    
    
  n->ts = j->ts;       
  n->value.function.name =      
    g95_get_string(PREFIX "ishft_%d_%d", j->ts.kind, shift->ts.kind);        
}


       
       
void g95_resolve_aint(g95_expr *x, g95_expr *l, g95_expr *kind) {      
      
  x->ts.type = l->ts.type; 
  x->ts.kind = (kind == NULL) ? l->ts.kind      
    : mpz_get_si(kind->value.integer);    
    
  x->value.function.name =        
    g95_get_string(PREFIX "aint%d_%d", x->ts.kind, l->ts.kind);   
} 
 
 
          
          
void g95_resolve_len(g95_expr *m, g95_expr *s) {   
   
  m->ts.type = BT_INTEGER;          
  m->ts.kind = g95_default_integer_kind(); 
  m->value.function.name = g95_get_string(PREFIX "len_%d", s->ts.kind);          
}         
         
         


void g95_resolve_logical(g95_expr *d, g95_expr *s, g95_expr *kind) {   
   
  d->ts.type = BT_LOGICAL;       
  d->ts.kind = (kind == NULL) ? g95_default_logical_kind()      
    : mpz_get_si(kind->value.integer);   
  d->rank = s->rank; 
 
  d->value.function.name =   
    g95_get_string(PREFIX "logical_%d_%c%d", d->ts.kind,  
		   g95_type_letter(s->ts.type), s->ts.kind);      
}  
  
  
   
   
void g95_resolve_transpose(g95_expr *u, g95_expr *matrix) {  
  
  u->ts = matrix->ts;      
  u->rank = 2;         
         
  u->value.function.name = g95_get_string(PREFIX "transpose");     
}      
      
      
         
         
void g95_resolve_btest(g95_expr *l, g95_expr *m, g95_expr *p) {        
        
  l->ts.type = BT_LOGICAL;     
  l->ts.kind = g95_default_logical_kind();       
       
  l->value.function.name = g95_get_string(PREFIX "btest_%d_%d", m->ts.kind,      
					  p->ts.kind);    
}


      
      
void g95_resolve_random_number(g95_code *w) { 
int k;   
   
  k = w->ext.actual->u.expr->ts.kind;      
  w->sub_name = g95_get_string(PREFIX "random_%d", k);
}        
        
        
          
          
void g95_resolve_ceiling(g95_expr *t, g95_expr *d, g95_expr *k) {

  t->ts.type = BT_INTEGER;      
  t->ts.kind = (k == NULL) ? g95_default_integer_kind()  
    : mpz_get_si(k->value.integer);         
         
  t->value.function.name = g95_get_string(PREFIX "ceiling_%d", d->ts.kind);     
}    
    
    
  
  
void g95_resolve_minloc(g95_expr *n, g95_expr *arr, g95_expr *dim,          
			g95_expr *msk) {     
char *suffix;        
        
  n->ts.type = BT_INTEGER;
  n->ts.kind = g95_default_integer_kind();         
         
  if (dim == NULL) { 
    n->rank = 1;        
    suffix = "";
  } else {  
    n->rank = arr->rank - 1;  
    suffix = (arr->rank == 1) ? "1" : "d";         
  }        
        
  n->value.function.name =      
    g95_get_string(PREFIX "minloc%s_%c%d", suffix,
		   g95_type_letter(arr->ts.type), arr->ts.kind);        
}   
   
   
          
          
void g95_resolve_conjg(g95_expr *h, g95_expr *z) {     
     
  h->ts = z->ts;
  h->value.function.name = g95_get_string(PREFIX "conjg_%d", z->ts.kind);       
}          
          
          
    
    
void g95_resolve_sum(g95_expr *e, g95_expr *arr, g95_expr *dim,  
		     g95_expr *mask) {    
char *suffix;          
          
  e->ts = arr->ts;        
        
  if (dim == NULL || arr->rank == 1) {          
    e->rank = 0;        
    suffix = "";      
  } else {    
    e->rank = arr->rank - 1;   
    suffix = "d";        
  }          
          
  e->value.function.name = 
    g95_get_string(PREFIX "sum%s_%c%d", suffix,       
                   g95_type_letter(arr->ts.type), arr->ts.kind);      
}   
   
   
        
        
void g95_resolve_scale(g95_expr *q, g95_expr *j, g95_expr *v) {       
       
  q->ts = j->ts;      
  q->value.function.name = g95_get_string(PREFIX "scale_%d", j->ts.kind);         
}          
          
          
    
    
void g95_resolve_nearest(g95_expr *k, g95_expr *w, g95_expr *r) {       
       
  k->ts = w->ts;   
  k->rank = w->rank;         
         
  k->value.function.name = g95_get_string(PREFIX "nearest_%d_%d",    
					  w->ts.kind, r->ts.kind);        
}        
        
        


void g95_resolve_sinh(g95_expr *b, g95_expr *i) {          
          
  b->ts = i->ts;
  b->value.function.name =          
    g95_get_string(PREFIX "sinh_%c%d", g95_type_letter(i->ts.type),   
		   i->ts.kind);        
}        
        
        
  
  
void g95_resolve_asin(g95_expr *b, g95_expr *k) {         
         
  b->ts = k->ts;   
  b->value.function.name =  
    g95_get_string(PREFIX "asin_%c%d", g95_type_letter(k->ts.type),  
		   k->ts.kind);          
}        
        
        
