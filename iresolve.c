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
      
      
          
          
void g95_resolve_pack(g95_expr *q, g95_expr *arr, g95_expr *m,         
		      g95_expr *vector) {         
         
  q->ts = arr->ts;          
  q->rank = 1;     
     
  q->value.function.name =    
    g95_get_string(m->rank != 0 ? PREFIX "pack" : PREFIX "pack_s"); 
}      
      
      
      
      
static void free_strings(void) {       
string_node *u, *n; 
int h;       
       
  for(h=0; h<HASH_SIZE; h++) {
    for(u=string_head[h]; u; u=n) { 
      n = u->next;
      g95_free(u);
    } 
  }    
}        
        
        
          
          
void g95_resolve_aint(g95_expr *o, g95_expr *s, g95_expr *knd) {        
        
  o->ts.type = s->ts.type;       
  o->ts.kind = (knd == NULL) ? s->ts.kind    
    : mpz_get_si(knd->value.integer);      
      
  o->value.function.name =     
    g95_get_string(PREFIX "aint_%d_%c%d", o->ts.kind,         
		   g95_type_letter(s->ts.type), s->ts.kind);         
}


        
        
void g95_resolve_floor(g95_expr *q, g95_expr *r, g95_expr *kind) {       
       
  q->ts.type = BT_INTEGER;      
  q->ts.kind = (kind == NULL) ? g95_default_integer_kind()          
    : mpz_get_si(kind->value.integer);  
  
  q->value.function.name = g95_get_string(PREFIX "floor_%d", r->ts.kind);          
}          
          
          
      
      
void g95_resolve_matmul(g95_expr *j, g95_expr *g, g95_expr *y) {    
g95_expr tmp; 
 
  if (g->ts.type == BT_LOGICAL && y->ts.type == BT_LOGICAL) {      
    j->ts.type = BT_LOGICAL;
    j->ts.kind = g95_default_logical_kind();       
  } else {
    tmp.type = EXPR_OP;     
    g95_clear_ts(&tmp.ts);     
    tmp.operator = INTRINSIC_NONE;   
    tmp.op1 = g;      
    tmp.op2 = y;  
    g95_type_convert_binary(&tmp);   
    j->ts = tmp.ts;       
  }        
        
  j->rank = (g->rank == 2 && y->rank == 2) ? 2 : 1;

  j->value.function.name =     
    g95_get_string(PREFIX "matmul%d%d_%c%d", g->rank, y->rank,         
		   g95_type_letter(j->ts.type), j->ts.kind); 
}         
         
         
   
   
void g95_resolve_random_number(g95_code *x) { 
int kind;        
        
  kind = x->ext.actual->u.expr->ts.kind;  
  
  x->sub_name = g95_get_string((x->ext.actual->u.expr->rank == 0) ?       
			       PREFIX "random_%d" : PREFIX "arandom_%d", kind); 
}         
         
         
     
     
void g95_resolve_cshift(g95_expr *y, g95_expr *ap, g95_expr *shift,   
			g95_expr *r) {     
     
  y->ts = ap->ts;
  y->rank = ap->rank;       
       
  y->value.function.name =       
    g95_get_string(PREFIX "cshift%d", (shift->rank == 0) ? 1 : 2);       
}  
  
  
        
        
void g95_resolve_int(g95_expr *j, g95_expr *z, g95_expr *knd) {     
     
  j->ts.type = BT_INTEGER;    
  j->ts.kind = (knd == NULL) ? g95_default_integer_kind()        
    : mpz_get_si(knd->value.integer);         
         
  j->value.function.name =  
    g95_get_string(PREFIX "int_%d_%c%d", j->ts.kind,          
		   g95_type_letter(z->ts.type), z->ts.kind);      
}         
         
         
     
     
void g95_resolve_merge(g95_expr *c, g95_expr *tsource, g95_expr *fsource,
		       g95_expr *msk) {   
   
  c->ts = tsource->ts;       
  c->value.function.name =    
    g95_get_string(PREFIX "merge_%c%d", g95_type_letter(tsource->ts.type),
		   tsource->ts.kind); 
}         
         
         
   
   
void g95_resolve_cpu_time(g95_code *r) {       
       
  r->sub_name = g95_get_string(PREFIX "cpu_time_%d",    
			       r->ext.actual->u.expr->ts.kind);  
}          
          
          
       
       
void g95_resolve_iand(g95_expr *y, g95_expr *l, g95_expr *s) {   
   
  y->ts = l->ts;      
  y->value.function.name = g95_get_string(PREFIX "iand_%d", l->ts.kind);    
}        
        
        
         
         
void g95_resolve_anint(g95_expr *b, g95_expr *g, g95_expr *knd) {     
     
  b->ts.type = g->ts.type;      
  b->ts.kind = (knd == NULL) ? g->ts.kind    
    : mpz_get_si(knd->value.integer);          
          
  b->value.function.name = 
    g95_get_string(PREFIX "anint%d_%d", b->ts.kind, g->ts.kind);       
} 
 
 
          
          
void g95_resolve_count(g95_expr *p, g95_expr *m, g95_expr *dim) {    
    
  p->ts.type = BT_INTEGER;          
  p->ts.kind = g95_default_integer_kind();          
          
  if (dim == NULL || m->rank == 1)   
    p->value.function.name = g95_get_string(PREFIX "count_%d", m->ts.kind);     
  else {      
    p->rank = m->rank - 1;   
    p->value.function.name = g95_get_string(PREFIX "countd_%d", m->ts.kind);    
  }          
}      
      
      
        
        
void g95_resolve_maxloc(g95_expr *u, g95_expr *a, g95_expr *r,          
			g95_expr *mask) {   
char *suffix;

  u->ts.type = BT_INTEGER;          
  u->ts.kind = g95_default_integer_kind();         
         
  if (r == NULL) {     
    u->rank = 1;          
    suffix = "";     
  } else {    
    u->rank = a->rank - 1;        
    suffix = (a->rank == 1) ? "1" : "d";          
  }    
    
  u->value.function.name =         
    g95_get_string(PREFIX "maxloc%s_%c%d", suffix,     
		   g95_type_letter(a->ts.type), a->ts.kind);          
}         
         
         
     
     
void g95_resolve_exp(g95_expr *p, g95_expr *w) {          
          
  p->ts = w->ts;   
  p->value.function.name =  
    g95_get_string(PREFIX "exp_%c%d", g95_type_letter(w->ts.type), w->ts.kind);       
}         
         
         
  
  
void g95_resolve_product(g95_expr *p, g95_expr *ap, g95_expr *d,         
			 g95_expr *msk) {      
char *suffix;       
       
  p->ts = ap->ts;        
        
  if (d == NULL || ap->rank == 1) {    
    p->rank = 0;          
    suffix = "";         
  } else {  
    p->rank = ap->rank - 1;       
    suffix = "d";          
  }  
  
  p->value.function.name =   
    g95_get_string(PREFIX "product%s_%c%d", suffix,     
                   g95_type_letter(ap->ts.type), ap->ts.kind);        
}   
   
   
   
   
void g95_resolve_ceiling(g95_expr *c, g95_expr *v, g95_expr *k0) {         
         
  c->ts.type = BT_INTEGER;
  c->ts.kind = (k0 == NULL) ? g95_default_integer_kind()    
    : mpz_get_si(k0->value.integer);   
   
  c->value.function.name = g95_get_string(PREFIX "ceiling_%d", v->ts.kind);         
}     
     
     
  
  
void g95_resolve_scale(g95_expr *d, g95_expr *s, g95_expr *m) {         
         
  d->ts = s->ts; 
  d->value.function.name = g95_get_string(PREFIX "scale_%d", s->ts.kind);     
}         
         
         
         
         
void g95_resolve_idnint(g95_expr *k, g95_expr *p) {         
  g95_resolve_nint(k, p, NULL);        
}     
     
     
      
      
void g95_resolve_ishft(g95_expr *q, g95_expr *k, g95_expr *shift) {         
         
  q->ts = k->ts;         
  q->value.function.name =         
    g95_get_string(PREFIX "ishft_%d_%d", k->ts.kind, shift->ts.kind); 
}     
     
     
   
   
void g95_resolve_ieor(g95_expr *p, g95_expr *a, g95_expr *e) {      
      
  p->ts = a->ts;  
  p->value.function.name = g95_get_string(PREFIX "ieor_%d", a->ts.kind); 
}     
     
     
  
  
void g95_resolve_cosh(g95_expr *s, g95_expr *k) {     
     
  s->ts = k->ts;         
  s->value.function.name =       
    g95_get_string(PREFIX "cosh_%c%d", g95_type_letter(k->ts.type),        
		   k->ts.kind);     
} 
 
 
         
         
void g95_resolve_ichar(g95_expr *s, g95_expr *e) {        
        
  s->ts.type = BT_INTEGER;     
  s->ts.kind = g95_default_integer_kind();   
   
  s->value.function.name = g95_get_string(PREFIX "ichar_%d", e->ts.kind);     
}


       
       
void g95_resolve_ibits(g95_expr *o, g95_expr *x, g95_expr *p,        
		       g95_expr *leng) {      
      
  o->ts = x->ts;      
  o->value.function.name = g95_get_string(PREFIX "ibits_%d", x->ts.kind);    
}          
          
          
          
          
void g95_resolve_transfer(g95_expr *z, g95_expr *s, g95_expr *mold,      
			  g95_expr *sz) {      
      
  z->ts = mold->ts;      
      
  if (sz == NULL && mold->rank == 0) 
    z->value.function.name = PREFIX "transfer";         
  else {
    z->value.function.name =      
      (s->rank == 0) ? PREFIX "transfer1" : PREFIX "transfer2";    
    
    z->rank = 1;   
  }   
}     
     
     
    
    
void g95_resolve_sqrt(g95_expr *t, g95_expr *e) {          
          
  t->ts = e->ts;
  t->value.function.name =          
    g95_get_string(PREFIX "sqrt_%c%d", g95_type_letter(e->ts.type),       
		   e->ts.kind);       
}     
     
     
   
   
void g95_resolve_ior(g95_expr *u, g95_expr *p, g95_expr *c) {    
    
  u->ts = p->ts;         
  u->value.function.name = g95_get_string(PREFIX "ior_%d", p->ts.kind);  
}         
         
         
    
    
void g95_resolve_len(g95_expr *p, g95_expr *st) {

  p->ts.type = BT_INTEGER;   
  p->ts.kind = g95_default_integer_kind();         
  p->value.function.name = g95_get_string(PREFIX "len_%d", st->ts.kind);       
}      
      
      
       
       
void g95_resolve_fraction(g95_expr *k, g95_expr *x) {      
      
  k->ts = x->ts;
  k->value.function.name = g95_get_string(PREFIX "fraction_%d", x->ts.kind);    
}     
     
     
       
       
void g95_resolve_rrspacing(g95_expr *f, g95_expr *p) {         
         
  f->ts = p->ts;      
  f->value.function.name = g95_get_string(PREFIX "rrspacing_%d", p->ts.kind);  
}     
     
     
  
  
void g95_resolve_sum(g95_expr *i, g95_expr *ap, g95_expr *d,        
		     g95_expr *msk) {  
char *suffix;    
    
  i->ts = ap->ts;  
  
  if (d == NULL || ap->rank == 1) {   
    i->rank = 0;     
    suffix = "";
  } else {    
    i->rank = ap->rank - 1;        
    suffix = "d";
  }  
  
  i->value.function.name =    
    g95_get_string(PREFIX "sum%s_%c%d", suffix,       
                   g95_type_letter(ap->ts.type), ap->ts.kind);        
}        
        
        
       
       
void g95_resolve_logical(g95_expr *g, g95_expr *x, g95_expr *k0) { 
 
  g->ts.type = BT_LOGICAL;   
  g->ts.kind = (k0 == NULL) ? g95_default_logical_kind()  
    : mpz_get_si(k0->value.integer);        
  g->rank = x->rank;      
      
  g->value.function.name =     
    g95_get_string(PREFIX "logical_%d_%c%d", g->ts.kind,        
		   g95_type_letter(x->ts.type), x->ts.kind); 
}        
        
        
      
      
void g95_resolve_abs(g95_expr *e, g95_expr *z) {

  e->ts = z->ts;      
  if (e->ts.type == BT_COMPLEX) e->ts.type = BT_REAL;

  e->value.function.name =      
    g95_get_string(PREFIX "abs_%c%d", g95_type_letter(z->ts.type), z->ts.kind);      
}        
        
        
        
        
void g95_resolve_ubound(g95_expr *c, g95_expr *ap, g95_expr *dim) {     
static char ubound[] = PREFIX "ubound";      
      
  c->ts.type = BT_INTEGER;
  c->ts.kind = g95_default_integer_kind();  
  
  c->value.function.name = ubound;         
         
  if (dim == NULL) c->rank = 1;         
}          
          
          
         
         
void g95_resolve_conjg(g95_expr *k, g95_expr *u) {      
      
  k->ts = u->ts;       
  k->value.function.name = g95_get_string(PREFIX "conjg_%d", u->ts.kind);   
}       
       
       
  
  
void g95_resolve_scan(g95_expr *s, g95_expr *string, g95_expr *set,   
		      g95_expr *back) {      
      
  s->ts.type = BT_INTEGER;    
  s->ts.kind = g95_default_integer_kind();      
  s->value.function.name = g95_get_string(PREFIX "scan_%d", string->ts.kind);         
}




void g95_resolve_achar(g95_expr *g, g95_expr *l) {       
       
  g->ts.type = BT_CHARACTER;         
  g->ts.kind = g95_default_character_kind();       
       
  g->value.function.name = g95_get_string(PREFIX "char_%d", g->ts.kind);       
} 
 
 
       
       
void g95_resolve_tan(g95_expr *u, g95_expr *g) {       
       
  u->ts = g->ts;
  u->value.function.name =   
    g95_get_string(PREFIX "tan_%c%d", g95_type_letter(g->ts.type), g->ts.kind);      
}          
          
          


/* hash()-- Return a hash code based on the name */     
     
static int hash(char *name0) {  
int s;       
       
  s = 1;       
  while(*name0)
    s = 5311966*s + *name0++;         
         
  if (s < 0) s = -s;
  return s % HASH_SIZE;         
}  
  
  
       
       
void g95_resolve_spread(g95_expr *w, g95_expr *s, g95_expr *d,  
			g95_expr *ncopies) {     
     
  w->ts.type = s->ts.type;   
  w->ts.kind = s->ts.kind;          
  w->rank = s->rank + 1;          
  w->value.function.name =  
    g95_get_string(s->rank > 0 ? PREFIX "spread" : PREFIX "spread_s");         
}     
     
     
 
 
void g95_resolve_aimag(g95_expr *g, g95_expr *u) {        
        
  g->ts.type = BT_REAL;      
  g->ts.kind = u->ts.kind;         
  g->value.function.name =  
    g95_get_string(PREFIX "aimag_%c%d", g95_type_letter(u->ts.type),          
		   u->ts.kind);        
}       
       
       
     
     
void g95_resolve_eoshift(g95_expr *l, g95_expr *ap, g95_expr *shift,   
      		                   g95_expr *boundary, g95_expr *d) {         
         
  l->ts = ap->ts;
  l->rank = ap->rank; 
 
  if (boundary == NULL || boundary->rank == 0)          
    l->value.function.name =       
      g95_get_string(PREFIX "eoshift%d_%c%d", (shift->rank == 0) ? 1 : 2,      
		     g95_type_letter(ap->ts.type), ap->ts.kind);
  else      
    l->value.function.name = g95_get_string(PREFIX "eoshift%d",       
					    (shift->rank == 0) ? 3 : 4); 
}       
       
       
      
      
void g95_resolve_nearest(g95_expr *n, g95_expr *m, g95_expr *u) {     
     
  n->ts = m->ts; 
  n->rank = m->rank;   
   
  n->value.function.name = g95_get_string(PREFIX "nearest_%d_%d", 
					  m->ts.kind, u->ts.kind);     
} 
 
 
     
     
void g95_resolve_maxval(g95_expr *x, g95_expr *array, g95_expr *d,   
			g95_expr *msk) { 
 
  x->ts = array->ts;        
        
  if (d != NULL && array->rank != 1)        
    x->rank = array->rank - 1;      
      
  x->value.function.name =   
    g95_get_string(PREFIX "%s_%c%d", (d == NULL) ? "maxval" : "maxvald",  
                   g95_type_letter(array->ts.type), array->ts.kind);      
}         
         
         
         
         
void g95_resolve_exponent(g95_expr *j, g95_expr *w) {          
          
  j->ts.type = BT_INTEGER;          
  j->ts.kind = g95_default_integer_kind();

  j->value.function.name = g95_get_string(PREFIX "exponent_%d", w->ts.kind);    
}  
  
  
         
         
void g95_resolve_lbound(g95_expr *w, g95_expr *ap, g95_expr *r) {  
static char lbound[] = PREFIX "lbound";       
       
  w->ts.type = BT_INTEGER;    
  w->ts.kind = g95_default_integer_kind();     
     
  w->value.function.name = lbound;     
     
  if (r == NULL) w->rank = 1;       
}    
    
    
         
         
void g95_resolve_dim(g95_expr *p, g95_expr *s, g95_expr *o) {        
        
  p->ts = s->ts; 
  p->value.function.name =   
    g95_get_string(PREFIX "dim_%c%d", g95_type_letter(s->ts.type), s->ts.kind);          
}          
          
          
         
         
void g95_resolve_dot_product(g95_expr *r, g95_expr *v, g95_expr *j) {      
g95_expr tmp;          
          
  if (v->ts.type == BT_LOGICAL && j->ts.type == BT_LOGICAL) {   
    r->ts.type = BT_LOGICAL;    
    r->ts.kind = g95_default_logical_kind();         
  } else {
    tmp.type = EXPR_OP;         
    g95_clear_ts(&tmp.ts);     
    tmp.operator = INTRINSIC_NONE;    
    tmp.op1 = v; 
    tmp.op2 = j;   
    g95_type_convert_binary(&tmp);
    r->ts = tmp.ts;       
  }      
      
  r->value.function.name =     
    g95_get_string(PREFIX "dot_product_%c%d", g95_type_letter(r->ts.type),        
		   r->ts.kind);   
}        
        
        
     
     
void g95_resolve_transpose(g95_expr *p, g95_expr *matrix) {   
   
  p->ts = matrix->ts;    
  p->rank = 2;          
          
  p->value.function.name = g95_get_string(PREFIX "transpose");         
}         
         
         
          
          
void g95_resolve_char(g95_expr *k, g95_expr *m, g95_expr *knd) {     
     
  k->ts.type = BT_CHARACTER;     
  k->ts.kind = (knd == NULL) ? g95_default_character_kind()        
    : mpz_get_si(knd->value.integer); 
 
  k->value.function.name = g95_get_string(PREFIX "char_%d", k->ts.kind);     
} 
 
 
        
        
void g95_resolve_ishftc(g95_expr *e, g95_expr *j, g95_expr *shift,  
			g95_expr *siz) {          
int s_kind;

  s_kind = (siz == NULL) ? g95_default_integer_kind() : shift->ts.kind;      
      
  e->ts = j->ts;
  e->value.function.name =         
    g95_get_string(PREFIX "ishftc_%d_%d_%d", j->ts.kind, shift->ts.kind,      
		   s_kind);         
}          
          
          
 
 
void g95_resolve_verify(g95_expr *k, g95_expr *string, g95_expr *set,     
	                                       	     g95_expr *back) {    
    
  k->ts.type = BT_INTEGER;       
  k->ts.kind = g95_default_integer_kind();  
  k->value.function.name = g95_get_string(PREFIX "verify_%d", string->ts.kind);
}          
          
          
        
        
void g95_resolve_btest(g95_expr *b, g95_expr *g, g95_expr *posit) {  
  
  b->ts.type = BT_LOGICAL;      
  b->ts.kind = g95_default_logical_kind();      
      
  b->value.function.name = g95_get_string(PREFIX "btest_%d_%d", g->ts.kind,  
					  posit->ts.kind);    
}      
      
      
 
 
void g95_resolve_all(g95_expr *r, g95_expr *maski, g95_expr *d) {          
          
  r->ts = maski->ts;       
       
  if (d == NULL || maski->rank == 1) 
    r->value.function.name = g95_get_string(PREFIX "all_%d", maski->ts.kind);        
  else {      
    r->rank = maski->rank - 1;          
    r->value.function.name = g95_get_string(PREFIX "alld_%d", maski->ts.kind);   
  } 
}        
        
        
         
         
void g95_resolve_minval(g95_expr *u, g95_expr *a, g95_expr *r,         
			     g95_expr *m) {         
         
  u->ts = a->ts;         
         
  if (r != NULL && a->rank != 1)        
    u->rank = a->rank - 1;          
          
  u->value.function.name =        
    g95_get_string(PREFIX "%s_%c%d", (r == NULL) ? "minval" : "minvald",   
                   g95_type_letter(a->ts.type), a->ts.kind);   
}


         
         
void g95_resolve_log(g95_expr *k, g95_expr *u) {         
         
  k->ts = u->ts;   
  k->value.function.name = 
    g95_get_string(PREFIX "log_%c%d", g95_type_letter(u->ts.type), u->ts.kind);       
}    
    
    


void g95_resolve_modulo(g95_expr *o, g95_expr *v, g95_expr *r) {

  o->ts = v->ts;     
  o->value.function.name =         
    g95_get_string(PREFIX "modulo_%c%d", g95_type_letter(v->ts.type),       
		   v->ts.kind);    
}     
     
     
        
        
void g95_resolve_shape(g95_expr *a, g95_expr * array) {   
   
  a->ts.type = BT_INTEGER; 
  a->ts.kind = g95_default_integer_kind();     
  a->rank = 1;  
  a->value.function.name = PREFIX "shape";    
}   
   
   
       
       
void g95_resolve_log10(g95_expr *z, g95_expr *q) {      
      
  z->ts = q->ts;         
  z->value.function.name =         
    g95_get_string(PREFIX "log10_%c%d", g95_type_letter(q->ts.type),          
		   q->ts.kind);  
}          
          
          
 
 
void g95_iresolve_init_1(void) {  
int w;         
         
  for(w=0; w<HASH_SIZE; w++) 
    string_head[w] = NULL;  
}     
     
     
    
    
void g95_resolve_mod(g95_expr *k, g95_expr *e, g95_expr *h) {          
          
  k->ts = e->ts;        
  k->value.function.name =     
    g95_get_string(PREFIX "mod_%c%d", g95_type_letter(e->ts.type), e->ts.kind);  
}         
         
         
    
    
void g95_resolve_trim(g95_expr *k, g95_expr *str) {  
  
  k->ts.type = BT_CHARACTER;     
  k->ts.kind = str->ts.kind;    
  k->value.function.name = g95_get_string(PREFIX "trim_%d", str->ts.kind);   
}  
  
  
        
        
void g95_resolve_acos(g95_expr *w, g95_expr *o) {      
      
  w->ts = o->ts;
  w->value.function.name = 
    g95_get_string(PREFIX "acos_%c%d", g95_type_letter(o->ts.type),         
		   o->ts.kind);     
}    
    
    
    
    
void g95_resolve_tanh(g95_expr *u, g95_expr *t) {  
  
  u->ts = t->ts;
  u->value.function.name = 
    g95_get_string(PREFIX "tanh_%c%d", g95_type_letter(t->ts.type),  
		   t->ts.kind);        
} 
 
 
          
          
void g95_resolve_asin(g95_expr *i, g95_expr *l) {         
         
  i->ts = l->ts;      
  i->value.function.name =
    g95_get_string(PREFIX "asin_%c%d", g95_type_letter(l->ts.type),   
		   l->ts.kind);        
} 
 
 
        
        
void g95_resolve_atan2(g95_expr *g, g95_expr *w, g95_expr *e) {       
       
  g->ts = w->ts;      
  g->value.function.name =   
    g95_get_string(PREFIX "atan2_%c%d", g95_type_letter(w->ts.type),    
		   w->ts.kind);       
}  
  
  
 
 
void g95_resolve_unpack(g95_expr *j, g95_expr *vector, g95_expr *mask, 
			                              g95_expr *field) {      
      
  j->ts.type = vector->ts.type;
  j->ts.kind = vector->ts.kind;
  j->rank = mask->rank; 
 
  j->value.function.name =         
    g95_get_string(field->rank != 0 ? PREFIX "unpack" : PREFIX "unpack_s");         
}       
       
       
          
          
void g95_resolve_spacing(g95_expr *d, g95_expr *m) {     
     
  d->ts = m->ts;          
  d->value.function.name = g95_get_string(PREFIX "spacing_%d", m->ts.kind);         
}          
          
          
      
      
void g95_resolve_set_exponent(g95_expr *g, g95_expr *p, g95_expr *s) {       
       
  g->ts = p->ts;  
  g->value.function.name =   
    g95_get_string(PREFIX "set_exponent_%d", p->ts.kind);          
}   
   
   


void g95_resolve_dble(g95_expr *l, g95_expr *r) {      
      
  l->ts.type = BT_REAL;      
  l->ts.kind = g95_default_double_kind();        
  l->value.function.name =        
    g95_get_string(PREFIX "dble_%c%d", g95_type_letter(r->ts.type),          
		   r->ts.kind);   
} 
 
 
    
    
void g95_resolve_reshape(g95_expr *t, g95_expr *s0, g95_expr *s,          
                         g95_expr *pad, g95_expr *order) {   
mpz_t dim;         
         
  t->ts = s0->ts;          
          
  g95_array_size(s, &dim);          
  t->rank = mpz_get_si(dim); 
  mpz_clear(dim);  
  
  t->value.function.name = g95_get_string(PREFIX "reshape");     
} 
 
 
      
      
void g95_resolve_any(g95_expr *y, g95_expr *m, g95_expr *dim) {

  y->ts = m->ts;     
     
  if (dim == NULL || m->rank == 1)        
    y->value.function.name = g95_get_string(PREFIX "any_%d", m->ts.kind);         
  else {     
    y->rank = m->rank - 1;       
    y->value.function.name = g95_get_string(PREFIX "anyd_%d", m->ts.kind);       
  }       
}      
      
      
   
   
void g95_resolve_nint(g95_expr *m, g95_expr *g, g95_expr *k0) {   
   
  m->ts.type = BT_INTEGER;        
  m->ts.kind = (k0 == NULL) ? g95_default_integer_kind()     
    : mpz_get_si(k0->value.integer);

  m->value.function.name = g95_get_string(PREFIX "nint_%d", g->ts.kind); 
}          
          
          
   
   
void g95_resolve_repeat(g95_expr *m, g95_expr *string, g95_expr *ncopies) {       
       
  m->ts.type = BT_CHARACTER;
  m->ts.kind = string->ts.kind; 
  m->value.function.name = g95_get_string(PREFIX "repeat_%d", string->ts.kind);    
}         
         
         
       
       
void g95_resolve_sign(g95_expr *m, g95_expr *j, g95_expr *p) {      
      
  m->ts = j->ts;         
  m->value.function.name = g95_get_string(PREFIX "sign_%d", j->ts.kind);     
}


        
        
/* g95_get_string()-- Given printf-like arguments, return a static
 * address of the resulting string.  If the name is not in the table,
 * it is added. */      
      
char *g95_get_string(char *format, ...) {
char temp_name[50];  
string_node *n;          
va_list ap;      
int l;     
     
  va_start(ap, format);       
  vsprintf(temp_name, format, ap);    
  va_end(ap);

  l = hash(temp_name);  
  
  /* Search */   
   
  for(n=string_head[l]; n; n=n->next)        
    if (strcmp(n->string, temp_name) == 0) return n->string;  
  
  /* Add */    
    
  n = g95_getmem(sizeof(string_node) + strlen(temp_name));   
   
  strcpy(n->string, temp_name);       
       
  n->next = string_head[l];
  string_head[l] = n;         
         
  return n->string; 
}      
      
      
       
       
void g95_resolve_len_trim(g95_expr *n, g95_expr *st) {    
    
  n->ts.type = BT_INTEGER;      
  n->ts.kind = g95_default_integer_kind();  
  n->value.function.name = g95_get_string(PREFIX "len_trim_%d",
					  st->ts.kind);         
}


        
        
void g95_resolve_cos(g95_expr *o, g95_expr *e) { 
 
  o->ts = e->ts;   
  o->value.function.name =
    g95_get_string(PREFIX "cos_%c%d", g95_type_letter(e->ts.type), e->ts.kind);    
}    
    
    
 
 
void g95_resolve_atan(g95_expr *e, g95_expr *l) {   
   
  e->ts = l->ts; 
  e->value.function.name =  
    g95_get_string(PREFIX "atan_%c%d", g95_type_letter(l->ts.type),     
		   l->ts.kind);   
} 
 
 
       
       
void g95_resolve_ibclr(g95_expr *d, g95_expr *t, g95_expr *position) {      
      
  d->ts = t->ts;
  d->value.function.name = g95_get_string(PREFIX "ibclr_%d", t->ts.kind);    
}    
    
    


void g95_resolve_sinh(g95_expr *i, g95_expr *z) {          
          
  i->ts = z->ts;
  i->value.function.name =     
    g95_get_string(PREFIX "sinh_%c%d", g95_type_letter(z->ts.type),    
		   z->ts.kind);        
}


       
       
void g95_resolve_real(g95_expr *d, g95_expr *h, g95_expr *kind) {        
        
  d->ts.type = BT_REAL;          
          
  if (kind != NULL)    
    d->ts.kind = mpz_get_si(kind->value.integer);         
  else   
    d->ts.kind = (h->ts.type == BT_COMPLEX) ?          
      h->ts.kind : g95_default_real_kind();  
  
  d->value.function.name =         
    g95_get_string(PREFIX "real_%d_%c%d", d->ts.kind,          
		   g95_type_letter(h->ts.type), h->ts.kind); 
}         
         
         


void g95_resolve_minloc(g95_expr *s, g95_expr *ap, g95_expr *dim,       
			g95_expr *m) { 
char *suffix;  
  
  s->ts.type = BT_INTEGER;
  s->ts.kind = g95_default_integer_kind();  
  
  if (dim == NULL) {         
    s->rank = 1;        
    suffix = "";       
  } else {
    s->rank = ap->rank - 1;       
    suffix = (ap->rank == 1) ? "1" : "d";
  }

  s->value.function.name = 
    g95_get_string(PREFIX "minloc%s_%c%d", suffix,         
		   g95_type_letter(ap->ts.type), ap->ts.kind);       
}


   
   
void g95_resolve_not(g95_expr *r, g95_expr *d) {        
        
  r->ts = d->ts;   
  r->value.function.name = g95_get_string(PREFIX "not_%d", d->ts.kind);  
}  
  
  
   
   
void g95_resolve_cmplx(g95_expr *c, g95_expr *i, g95_expr *n, g95_expr *k0) { 
 
  c->ts.type = BT_COMPLEX;
  c->ts.kind = (k0 == NULL)     
    ? g95_default_real_kind()     
    : mpz_get_si(k0->value.integer); 
 
  c->value.function.name = g95_get_string(PREFIX "cmplx");   
}        
        
        
       
       
void g95_iresolve_done_1(void) {       
       
  free_strings();     
}

     
     
void g95_resolve_sin(g95_expr *a, g95_expr *x) {       
       
  a->ts = x->ts;       
  a->value.function.name =  
    g95_get_string(PREFIX "sin_%c%d", g95_type_letter(x->ts.type), x->ts.kind);      
}       
       
       
    
    
void g95_resolve_ibset(g95_expr *r, g95_expr *n, g95_expr *posit) { 
 
  r->ts = n->ts;    
  r->value.function.name = g95_get_string(PREFIX "ibset_%d", n->ts.kind);          
}


         
         
void g95_resolve_min_max(g95_expr *c, g95_intrinsic_sym *j, g95_expr *y) {      
      
  c->ts = y->ts;    
  c->value.function.name = j->name;       
       
  if (j->ts.type != BT_UNKNOWN &&        
      (j->ts.type != y->ts.type || j->ts.kind != y->ts.kind)) 
    c->ts = j->ts;        
}     
     
     
