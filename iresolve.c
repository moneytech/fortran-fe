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
       
       
          
          
void g95_resolve_transfer(g95_expr *n, g95_expr *source, g95_expr *mold,       
			                                 g95_expr *s) {       
static char transfer0[] = PREFIX "transfer0", transfer1[] = PREFIX "transfer1"; 
 
  n->ts = mold->ts;  
  
  if (s == NULL && mold->rank == 0) {     
    n->rank = 0;  
    n->value.function.name = transfer0;
  } else {          
    n->rank = 1; 
    n->value.function.name = transfer1;    
  }  
}          
          
          
       
       
void g95_resolve_logical(g95_expr *r, g95_expr *u, g95_expr *k) {    
    
  r->ts.type = BT_LOGICAL;         
  r->ts.kind = (k == NULL) ? g95_default_logical_kind() 
    : mpz_get_si(k->value.integer);     
  r->rank = u->rank;         
         
  r->value.function.name =   
    g95_get_string(PREFIX "logical_%d_%c%d", r->ts.kind,         
		   g95_type_letter(u->ts.type), u->ts.kind);        
}         
         
         
  
  
/* hash()-- Return a hash code based on the name */

static int hash(char *nm) {    
int w;       
       
  w = 1;    
  while(*nm)     
    w = 5311966*w + *nm++;        
        
  if (w < 0) w = -w;    
  return w % HASH_SIZE;        
}  
  
  
 
 
void g95_resolve_sin(g95_expr *g, g95_expr *a) {        
        
  g->ts = a->ts;  
  g->value.function.name =         
    g95_get_string(PREFIX "sin_%c%d", g95_type_letter(a->ts.type), a->ts.kind);       
}        
        
        
   
   
void g95_resolve_maxval(g95_expr *l, g95_expr *block, g95_expr *d,
			g95_expr *m) {   
   
  l->ts = block->ts;  
  
  if (d != NULL && block->rank != 1)   
    l->rank = block->rank - 1;      
      
  l->value.function.name = 
    g95_get_string(PREFIX "%s_%c%d", d ? "dmaxval" : "maxval",          
                   g95_type_letter(block->ts.type), block->ts.kind);         
}       
       
       
         
         
void g95_resolve_repeat(g95_expr *z, g95_expr *msg, g95_expr *ncopies) {          
          
  z->ts.type = BT_CHARACTER;   
  z->ts.kind = msg->ts.kind;  
  z->value.function.name = g95_get_string(PREFIX "repeat_%d", msg->ts.kind);         
}      
      
      
       
       
void g95_resolve_int(g95_expr *d, g95_expr *w, g95_expr *k0) {          
          
  d->ts.type = BT_INTEGER;      
  d->ts.kind = (k0 == NULL) ? g95_default_integer_kind()        
    : mpz_get_si(k0->value.integer);    
    
  d->value.function.name =
    g95_get_string(PREFIX "int_%d_%c%d", d->ts.kind,      
		   g95_type_letter(w->ts.type), w->ts.kind);
}


          
          
void g95_resolve_dot_product(g95_expr *y, g95_expr *k, g95_expr *r) {      
g95_expr temp;        
        
  if (k->ts.type == BT_LOGICAL && r->ts.type == BT_LOGICAL) {        
    y->ts.type = BT_LOGICAL;         
    y->ts.kind = g95_default_logical_kind();     
  } else {
    temp.type = EXPR_OP;   
    g95_clear_ts(&temp.ts);    
    temp.operator = INTRINSIC_NONE;     
    temp.op1 = k;  
    temp.op2 = r;         
    g95_type_convert_binary(&temp);
    y->ts = temp.ts;          
  }   
   
  y->value.function.name =    
    g95_get_string(PREFIX "dot_product_%c%d", g95_type_letter(y->ts.type),  
		   y->ts.kind);   
}        
        
        
    
    
void g95_resolve_pack(g95_expr *w, g95_expr *arr, g95_expr *m,        
		      g95_expr *vector) {      
      
  w->ts = arr->ts;       
  w->rank = 1;          
          
  w->value.function.name =
    g95_get_string(PREFIX "pack_%c%d", g95_type_letter(arr->ts.type),         
		   arr->ts.kind);          
} 
 
 
   
   
void g95_resolve_reshape(g95_expr *a, g95_expr *source, g95_expr *extent,    
			 g95_expr *pad, g95_expr *order) {          
static char reshape0[] = PREFIX "reshape";          
mpz_t dim;      
int knd; 
 
  a->ts = source->ts;     
     
  g95_array_size(extent, &dim);        
  a->rank = mpz_get_si(dim);          
  mpz_clear(dim);        
  if (source->ts.type == BT_DERIVED)       
    knd = 0;      
  else
    knd = source->ts.kind;   
   
  switch (knd) {     
  case 4:        
  case 8:     
    /* case 16: */
    a->value.function.name =      
      g95_get_string(PREFIX "reshape_%d", source->ts.kind);         
    break;     
           
  default:  
    a->value.function.name = reshape0;  
    break;  
  }      
      
}    
    
    


void g95_resolve_idnint(g95_expr *p, g95_expr *o) {          
  g95_resolve_nint(p, o, NULL);          
} 
 
 
   
   
void g95_resolve_real(g95_expr *w, g95_expr *h, g95_expr *knd) {   
   
  w->ts.type = BT_REAL;          
          
  if (knd != NULL)        
    w->ts.kind = mpz_get_si(knd->value.integer);  
  else  
    w->ts.kind = (h->ts.type == BT_COMPLEX) ?        
      h->ts.kind : g95_default_real_kind();      
      
  w->value.function.name =       
    g95_get_string(PREFIX "real_%d_%c%d", w->ts.kind,         
		   g95_type_letter(h->ts.type), h->ts.kind);       
}


      
      
void g95_resolve_product(g95_expr *g, g95_expr *array, g95_expr *r,          
			 g95_expr *msk) {         
         
  g->ts = array->ts;    
    
  if (r != NULL && array->rank != 1) g->rank = array->rank - 1;     
     
  g->value.function.name =      
    g95_get_string(PREFIX "%s_%c%d", msk ? "mproduct" : "product",      
                   g95_type_letter(array->ts.type), array->ts.kind);      
}   
   
   
         
         
void g95_resolve_atan2(g95_expr *v, g95_expr *u, g95_expr *l) {      
      
  v->ts = u->ts;    
  v->value.function.name =       
    g95_get_string(PREFIX "atan2_%c%d", g95_type_letter(u->ts.type),     
		   u->ts.kind);  
}  
  
  
      
      
void g95_resolve_atan(g95_expr *w, g95_expr *g) { 
 
  w->ts = g->ts;          
  w->value.function.name =   
    g95_get_string(PREFIX "atan_%c%d", g95_type_letter(g->ts.type),    
		   g->ts.kind);      
}  
  
  
      
      
void g95_resolve_exp(g95_expr *c, g95_expr *a) { 
 
  c->ts = a->ts;        
  c->value.function.name =         
    g95_get_string(PREFIX "exp_%c%d", g95_type_letter(a->ts.type), a->ts.kind);    
}   
   
   
      
      
void g95_resolve_log10(g95_expr *h, g95_expr *q) {   
   
  h->ts = q->ts;        
  h->value.function.name =   
    g95_get_string(PREFIX "log10_%c%d", g95_type_letter(q->ts.type),
		   q->ts.kind);  
}         
         
         
         
         
void g95_resolve_ishftc(g95_expr *x, g95_expr *m, g95_expr *shift,      
			g95_expr *size) {    
int s_kind; 
 
  s_kind = (size == NULL) ? g95_default_integer_kind() : shift->ts.kind;   
   
  x->ts = m->ts;  
  x->value.function.name =         
    g95_get_string(PREFIX "ishftc_%d_%d_%d", m->ts.kind, shift->ts.kind,  
		   s_kind);      
}        
        
        
         
         
void g95_resolve_abs(g95_expr *p, g95_expr *l) {        
        
  p->ts = l->ts;         
  if (p->ts.type == BT_COMPLEX) p->ts.type = BT_REAL;

  p->value.function.name =  
    g95_get_string(PREFIX "abs_%c%d", g95_type_letter(l->ts.type), l->ts.kind);         
}   
   
   
    
    
void g95_resolve_ichar(g95_expr *x, g95_expr *g) {     
     
  x->ts.type = BT_INTEGER;  
  x->ts.kind = g95_default_integer_kind();        
        
  x->value.function.name = g95_get_string(PREFIX "ichar_%d", g->ts.kind);   
}        
        
        
     
     
void g95_resolve_ibits(g95_expr *v, g95_expr *a, g95_expr *posit,          
		       g95_expr *l) { 
 
  v->ts = a->ts;     
  v->value.function.name = g95_get_string(PREFIX "ibits_%d", a->ts.kind);      
} 
 
 
      
      
void g95_resolve_char(g95_expr *x, g95_expr *l, g95_expr *k) {    
    
  x->ts.type = BT_CHARACTER;   
  x->ts.kind = (k == NULL) ? g95_default_character_kind()        
    : mpz_get_si(k->value.integer);     
     
  x->value.function.name = g95_get_string(PREFIX "char_%d", x->ts.kind);      
}  
  
  
      
      
void g95_resolve_spread(g95_expr *g, g95_expr *source, g95_expr *d,    
			g95_expr *ncopies) {  
  
  g->ts.type = source->ts.type;          
  g->ts.kind = source->ts.kind;       
  g->rank = source->rank + 1;    
  g->value.function.name = 
    g95_get_string(PREFIX "spread_%c%d", g95_type_letter(source->ts.type),      
		   source->ts.kind);         
}          
          
          
        
        
void g95_resolve_minloc(g95_expr *p, g95_expr *block, g95_expr *d,        
			g95_expr *msk) {          
char *nam;        
        
  p->ts = block->ts; 
 
  if (d == NULL)
    p->rank = 1;      
  else      
    p->rank = block->rank - 1;        
        
  nam = msk ? "mminloc" : "minloc"; 
  p->value.function.name =      
    g95_get_string(PREFIX "%s%d_%d_%c%d", nam, d != NULL, p->ts.kind,  
                   g95_type_letter(block->ts.type), block->ts.kind); 
}     
     
     


void g95_resolve_aint(g95_expr *e, g95_expr *a, g95_expr *k0) {       
       
  e->ts.type = a->ts.type;    
  e->ts.kind = (k0 == NULL) ? a->ts.kind   
    : mpz_get_si(k0->value.integer);   
   
  e->value.function.name =  
    g95_get_string(PREFIX "aint_%d_%c%d", e->ts.kind,   
		   g95_type_letter(a->ts.type), a->ts.kind);          
}          
          
          
        
        
void g95_resolve_dble(g95_expr *m, g95_expr *c) {

  m->ts.type = BT_REAL; 
  m->ts.kind = g95_default_double_kind();  
  m->value.function.name =          
    g95_get_string(PREFIX "dble_%c%d", g95_type_letter(c->ts.type),         
		   c->ts.kind);  
}        
        
        
          
          
void g95_resolve_unpack(g95_expr *i, g95_expr *vector, g95_expr *msk,     
			                              g95_expr *field) { 
 
  i->ts.type = vector->ts.type;  
  i->ts.kind = vector->ts.kind;          
  i->rank = msk->rank;        
        
  i->value.function.name =          
    g95_get_string(PREFIX "unpack_%c%d", g95_type_letter(vector->ts.type),      
		   vector->ts.kind);       
}  
  
  
        
        
void g95_resolve_eoshift(g95_expr *b, g95_expr *arr, g95_expr *shift,      
      		                   g95_expr *boundary, g95_expr *d) {

  b->ts = arr->ts;       
  b->rank = arr->rank;        
        
  b->value.function.name =   
    g95_get_string(PREFIX "eoshift_%c%d", g95_type_letter(arr->ts.type),     
		   arr->ts.kind);         
}      
      
      
    
    
void g95_resolve_sign(g95_expr *d, g95_expr *p, g95_expr *b) {   
   
  d->ts = p->ts;    
  d->value.function.name = 
    g95_get_string(PREFIX "sign_%c%d", g95_type_letter(p->ts.type),
		   p->ts.kind);
}   
   
   
    
    
void g95_resolve_cpu_time(g95_code *w) {   
   
  w->sub_name = g95_get_string(PREFIX "cpu_time_%d",  
			       w->ext.actual->u.expr->ts.kind);        
}  
  
  
          
          
void g95_resolve_exponent(g95_expr *y, g95_expr *e) {     
     
  y->ts.type = BT_INTEGER; 
  y->ts.kind = g95_default_integer_kind();         
         
  y->value.function.name = g95_get_string(PREFIX "exponent_%d", e->ts.kind); 
}       
       
       
          
          
void g95_resolve_asin(g95_expr *p, g95_expr *y) {  
  
  p->ts = y->ts;        
  p->value.function.name =   
    g95_get_string(PREFIX "asin_%c%d", g95_type_letter(y->ts.type),      
		   y->ts.kind);
}     
     
     


void g95_resolve_ceiling(g95_expr *e, g95_expr *s, g95_expr *k) {     
     
  e->ts.type = BT_INTEGER;        
  e->ts.kind = (k == NULL) ? g95_default_integer_kind()      
    : mpz_get_si(k->value.integer);      
      
  e->value.function.name =
    g95_get_string(PREFIX "ceiling_%d_%c%d", e->ts.kind,
		   g95_type_letter(s->ts.type), s->ts.kind);          
}      
      
      
   
   
void g95_resolve_cmplx(g95_expr *n, g95_expr *z, g95_expr *h, g95_expr *k0) {         
         
  n->ts.type = BT_COMPLEX;      
  n->ts.kind = (k0 == NULL)        
    ? g95_default_real_kind()          
    : mpz_get_si(k0->value.integer);     
     
  n->value.function.name = g95_get_string(PREFIX "cmplx");      
}      
      
      
       
       
void g95_resolve_modulo(g95_expr *g, g95_expr *u, g95_expr *r) {        
        
  g->ts = u->ts;
  g->value.function.name =         
    g95_get_string(PREFIX "modulo_%c%d", g95_type_letter(u->ts.type),     
		   u->ts.kind);          
}    
    
    
     
     
void g95_resolve_tanh(g95_expr *y, g95_expr *d) {      
      
  y->ts = d->ts;       
  y->value.function.name =   
    g95_get_string(PREFIX "tanh_%c%d", g95_type_letter(d->ts.type),          
		   d->ts.kind);          
}       
       
       
      
      
void g95_resolve_dim(g95_expr *f, g95_expr *j, g95_expr *m) {         
         
  f->ts = j->ts;        
  f->value.function.name =         
    g95_get_string(PREFIX "dim_%c%d", g95_type_letter(j->ts.type), j->ts.kind);        
} 
 
 
 
 
void g95_resolve_set_exponent(g95_expr *u, g95_expr *d, g95_expr *y) {   
   
  u->ts = d->ts;         
  u->value.function.name =  
    g95_get_string(PREFIX "set_exponent_%d_%d", d->ts.kind, y->ts.kind);  
}        
        
        
    
    
void g95_resolve_spacing(g95_expr *q, g95_expr *r) {        
        
  q->ts = r->ts;  
  q->value.function.name = g95_get_string(PREFIX "spacing_%d", r->ts.kind);   
}        
        
        
    
    
void g95_resolve_all(g95_expr *u, g95_expr *mask, g95_expr *d) {         
         
  u->ts = mask->ts;

  if (d == NULL || mask->rank == 1)   
    u->value.function.name = g95_get_string(PREFIX "all0_%d", mask->ts.kind);        
  else {         
    u->rank = mask->rank - 1;        
    u->value.function.name = g95_get_string(PREFIX "alln_%d", mask->ts.kind);         
  } 
}         
         
         
         
         
void g95_resolve_tan(g95_expr *n, g95_expr *d) {

  n->ts = d->ts; 
  n->value.function.name = 
    g95_get_string(PREFIX "tan_%c%d", g95_type_letter(d->ts.type), d->ts.kind);  
}         
         
         
     
     
void g95_iresolve_init_1(void) {
int l;          
          
  for(l=0; l<HASH_SIZE; l++)    
    string_head[l] = NULL;        
}          
          
          
         
         
void g95_resolve_log(g95_expr *c, g95_expr *j) {        
        
  c->ts = j->ts; 
  c->value.function.name =          
    g95_get_string(PREFIX "log_%c%d", g95_type_letter(j->ts.type), j->ts.kind);     
}      
      
      
  
  
void g95_resolve_acos(g95_expr *i, g95_expr *s) {   
   
  i->ts = s->ts;        
  i->value.function.name =
    g95_get_string(PREFIX "acos_%c%d", g95_type_letter(s->ts.type),
		   s->ts.kind);     
}  
  
  
     
     
void g95_resolve_scale(g95_expr *i, g95_expr *h, g95_expr *r) {     
     
  i->ts = h->ts;       
  i->value.function.name = g95_get_string(PREFIX "scale_%d_%d", h->ts.kind,      
					  h->ts.kind);        
} 
 
 
   
   
void g95_resolve_conjg(g95_expr *v, g95_expr *p) { 
 
  v->ts = p->ts;
  v->value.function.name = g95_get_string(PREFIX "conjg_%d", p->ts.kind);  
}


          
          
void g95_resolve_not(g95_expr *h, g95_expr *w) {     
     
  h->ts = w->ts;      
  h->value.function.name = g95_get_string(PREFIX "not_%d", w->ts.kind);         
}     
     
     
      
      
void g95_resolve_min_max(g95_expr *q, g95_intrinsic_sym *w, g95_expr *y) {        
        
  q->ts = y->ts;   
  q->value.function.name = w->name;        
        
  if (w->ts.type != BT_UNKNOWN &&        
      (w->ts.type != y->ts.type || w->ts.kind != y->ts.kind))  
    g95_convert_type(q, &w->ts, 2);         
}       
       
       
          
          
void g95_resolve_trim(g95_expr *u, g95_expr *str) {      
      
  u->ts.type = BT_CHARACTER;        
  u->ts.kind = str->ts.kind;       
  u->value.function.name = g95_get_string(PREFIX "trim_%d", str->ts.kind);    
}


        
        
void g95_resolve_btest(g95_expr *a, g95_expr *r, g95_expr *position) {  
  
  a->ts.type = BT_LOGICAL;    
  a->ts.kind = g95_default_logical_kind();       
       
  a->value.function.name = g95_get_string(PREFIX "btest_%d_%d", r->ts.kind, 
					  position->ts.kind);    
}     
     
     
     
     
void g95_resolve_ibset(g95_expr *h, g95_expr *c, g95_expr *position) {          
          
  h->ts = c->ts;  
  h->value.function.name = g95_get_string(PREFIX "ibset_%d", c->ts.kind);       
}


        
        
void g95_resolve_count(g95_expr *k, g95_expr *msk, g95_expr *dim) {          
          
  k->ts.type = BT_INTEGER;     
  k->ts.kind = g95_default_integer_kind();     
     
  if (dim == NULL || msk->rank == 1)   
    k->value.function.name = g95_get_string(PREFIX "count0_%d", msk->ts.kind);         
  else {  
    k->rank = msk->rank - 1;       
    k->value.function.name = g95_get_string(PREFIX "countn_%d", msk->ts.kind); 
  }
}   
   
   
         
         
void g95_resolve_cshift(g95_expr *z, g95_expr *arr, g95_expr *shift,  
			g95_expr *d) {         
static char cshift0[] = PREFIX "cshift0", cshift1[] = PREFIX "cshift1";

  z->ts = arr->ts;     
  z->rank = arr->rank;         
         
  if (d == NULL) 
    z->value.function.name = cshift0;
  else {         
    z->value.function.name = cshift1;   
  }        
}  
  
  
      
      
void g95_resolve_transpose(g95_expr *b, g95_expr *matrix) {    
    
  b->ts = matrix->ts;        
  b->rank = 2;

  b->value.function.name =  
    g95_get_string(PREFIX "transpose_%c%d", g95_type_letter(matrix->ts.type),    
		   matrix->ts.kind);     
}     
     
     
 
 
void g95_resolve_maxloc(g95_expr *g, g95_expr *array, g95_expr *r,
			g95_expr *maski) {  
char *nam;        
        
  g->ts = array->ts;         
         
  if (r == NULL)  
    g->rank = 1;         
  else   
    g->rank = array->rank - 1;    
    
  nam = maski ? "mmaxloc" : "maxloc";         
  g->value.function.name =       
    g95_get_string(PREFIX "%s%d_%d_%c%d", nam, r != NULL, g->ts.kind,          
                   g95_type_letter(array->ts.type), array->ts.kind);  
} 
 
 


void g95_resolve_floor(g95_expr *h, g95_expr *q, g95_expr *kind) {  
  
  h->ts.type = BT_INTEGER;  
  h->ts.kind = (kind == NULL) ? g95_default_integer_kind()   
    : mpz_get_si(kind->value.integer);      
      
  h->value.function.name =        
    g95_get_string(PREFIX "floor%d_%c%d", h->ts.kind,   
		   g95_type_letter(q->ts.type), q->ts.kind);     
}  
  
  
   
   
void g95_resolve_len(g95_expr *y, g95_expr *st) {         
         
  y->ts.type = BT_INTEGER;      
  y->ts.kind = g95_default_integer_kind();     
  y->value.function.name = g95_get_string(PREFIX "len_%d", st->ts.kind);    
}       
       
       
      
      
void g95_resolve_shape(g95_expr *b, g95_expr * ap) {     
     
  b->ts.type = BT_INTEGER;         
  b->ts.kind = g95_default_integer_kind();
  b->rank = 1;
  b->value.function.name = g95_get_string(PREFIX "shape_%d", b->ts.kind);     
}       
       
       
          
          
void g95_resolve_verify(g95_expr *h, g95_expr *str, g95_expr *set,      
	                                       	     g95_expr *back) {    
    
  h->ts.type = BT_INTEGER;        
  h->ts.kind = g95_default_integer_kind();         
  h->value.function.name = g95_get_string(PREFIX "verify_%d", str->ts.kind);     
}        
        
        
          
          
void g95_resolve_merge(g95_expr *a, g95_expr *tsource, g95_expr *fsource,       
		       g95_expr *mask) {     
     
  a->ts = tsource->ts;          
  a->value.function.name =          
    g95_get_string(PREFIX "merge_%c%d", g95_type_letter(tsource->ts.type),
		   tsource->ts.kind);     
}  
  
  
         
         
void g95_resolve_aimag(g95_expr *k, g95_expr *j) {      
      
  k->ts.type = BT_REAL;       
  k->ts.kind = j->ts.kind;     
  k->value.function.name =  
    g95_get_string(PREFIX "aimag_%c%d", g95_type_letter(j->ts.type), 
		   j->ts.kind);    
} 
 
 
 
 
void g95_resolve_random_number(g95_code *b) {  
int kind;

  kind = b->ext.actual->u.expr->ts.kind;       
       
  b->sub_name = g95_get_string((b->ext.actual->u.expr->rank == 0) ?     
			       PREFIX "random_%d" : PREFIX "arandom_%d", kind);       
}


      
      
void g95_resolve_rrspacing(g95_expr *c, g95_expr *o) {        
        
  c->ts = o->ts;          
  c->value.function.name = g95_get_string(PREFIX "rrspacing_%d", o->ts.kind);
}         
         
         
  
  
void g95_resolve_sum(g95_expr *c, g95_expr *block, g95_expr *r,      
		     g95_expr *m) {          
          
  c->ts = block->ts;   
   
  if (r != NULL && block->rank != 1) c->rank = block->rank - 1; 
 
  c->value.function.name = 
    g95_get_string(PREFIX "%s_%c%d", m ? "msum" : "sum",      
                   g95_type_letter(block->ts.type), block->ts.kind);        
}         
         
         
       
       
/* g95_get_string()-- Given printf-like arguments, return a static
 * address of the resulting string.  If the name is not in the table,
 * it is added. */         
         
char *g95_get_string(char *format, ...) {         
char temp_name[50];     
string_node *t;   
va_list actual;       
int u;     
     
  va_start(actual, format);        
  vsprintf(temp_name, format, actual);      
  va_end(actual);    
    
  u = hash(temp_name); 
 
  /* Search */    
    
  for(t=string_head[u]; t; t=t->next)      
    if (strcmp(t->string, temp_name) == 0) return t->string; 
 
  /* Add */  
  
  t = g95_getmem(sizeof(string_node) + strlen(temp_name)); 
 
  strcpy(t->string, temp_name);  
  
  t->next = string_head[u];   
  string_head[u] = t;     
     
  return t->string;          
}


         
         
void g95_resolve_ubound(g95_expr *y, g95_expr *array, g95_expr *d) {     
static char ubound[] = PREFIX "ubound"; 
 
  y->ts.type = BT_INTEGER;  
  y->ts.kind = g95_default_integer_kind();          
          
  y->value.function.name = ubound;  
  
  if (d == NULL) y->rank = 1;      
}


 
 
void g95_resolve_fraction(g95_expr *i, g95_expr *j) {         
         
  i->ts = j->ts; 
  i->value.function.name = g95_get_string(PREFIX "fraction_%d", j->ts.kind);     
}   
   
   
         
         
void g95_resolve_nint(g95_expr *y, g95_expr *z, g95_expr *k0) {     
     
  y->ts.type = BT_INTEGER;      
  y->ts.kind = (k0 == NULL) ? g95_default_integer_kind() 
    : mpz_get_si(k0->value.integer);       
       
  y->value.function.name = 
    g95_get_string(PREFIX "nint_%d_%d", y->ts.kind, z->ts.kind);          
}


   
   
void g95_resolve_ibclr(g95_expr *x, g95_expr *j, g95_expr *p) { 
 
  x->ts = j->ts;       
  x->value.function.name = g95_get_string(PREFIX "ibclr_%d", j->ts.kind);          
}    
    
    
       
       
void g95_resolve_mod(g95_expr *x, g95_expr *d, g95_expr *p) {   
   
  x->ts = d->ts;
  x->value.function.name =        
    g95_get_string(PREFIX "mod_%c%d", g95_type_letter(d->ts.type), d->ts.kind);       
}      
      
      
  
  
void g95_resolve_len_trim(g95_expr *d, g95_expr *st) {         
         
  d->ts.type = BT_INTEGER;          
  d->ts.kind = g95_default_integer_kind();        
  d->value.function.name = g95_get_string(PREFIX "len_trim_%d",       
					  st->ts.kind);
}      
      
      
 
 
void g95_resolve_ishft(g95_expr *d, g95_expr *b, g95_expr *shift) {        
        
  d->ts = b->ts;     
  d->value.function.name =          
    g95_get_string(PREFIX "ishft_%d_%d", b->ts.kind, shift->ts.kind);     
}


        
        
void g95_resolve_scan(g95_expr *f, g95_expr *st, g95_expr *set,   
		      g95_expr *back) {       
       
  f->ts.type = BT_INTEGER;          
  f->ts.kind = g95_default_integer_kind();          
  f->value.function.name = g95_get_string(PREFIX "scan_%d", st->ts.kind);       
}       
       
       
 
 
void g95_resolve_cosh(g95_expr *t, g95_expr *w) {  
  
  t->ts = w->ts; 
  t->value.function.name =        
    g95_get_string(PREFIX "cosh_%c%d", g95_type_letter(w->ts.type),    
		   w->ts.kind);       
}        
        
        
    
    
void g95_resolve_iand(g95_expr *s, g95_expr *a, g95_expr *o) {          
          
  s->ts = a->ts;         
  s->value.function.name = g95_get_string(PREFIX "iand_%d", a->ts.kind);      
}  
  
  
 
 
void g95_resolve_sqrt(g95_expr *m, g95_expr *s) {

  m->ts = s->ts;         
  m->value.function.name =   
    g95_get_string(PREFIX "sqrt_%c%d", g95_type_letter(s->ts.type),    
		   s->ts.kind); 
}




static void free_strings(void) {        
string_node *o, *l;         
int k;        
        
  for(k=0; k<HASH_SIZE; k++) {       
    for(o=string_head[k]; o; o=l) {
      l = o->next; 
      g95_free(o);    
    }      
  }         
}         
         
         
         
         
void g95_resolve_ior(g95_expr *v, g95_expr *d, g95_expr *a) {       
       
  v->ts = d->ts;          
  v->value.function.name = g95_get_string(PREFIX "ior_%d", d->ts.kind);         
}    
    
    
        
        
void g95_resolve_ieor(g95_expr *b, g95_expr *d, g95_expr *t) {         
         
  b->ts = d->ts;       
  b->value.function.name = g95_get_string(PREFIX "ieor_%d", d->ts.kind); 
}  
  
  
     
     
void g95_iresolve_done_1(void) {         
         
  free_strings();          
}   
         
         
void g95_resolve_sinh(g95_expr *i, g95_expr *k) {          
          
  i->ts = k->ts;        
  i->value.function.name =    
    g95_get_string(PREFIX "sinh_%c%d", g95_type_letter(k->ts.type),        
		   k->ts.kind);         
}       
       
       
   
   
void g95_resolve_cos(g95_expr *t, g95_expr *i) {       
       
  t->ts = i->ts;     
  t->value.function.name =    
    g95_get_string(PREFIX "cos_%c%d", g95_type_letter(i->ts.type), i->ts.kind);         
}     
     
     
    
    
void g95_resolve_matmul(g95_expr *m, g95_expr *u, g95_expr *c) { 
g95_expr temp;   
   
  if (u->ts.type == BT_LOGICAL && c->ts.type == BT_LOGICAL) {      
    m->ts.type = BT_LOGICAL;         
    m->ts.kind = g95_default_logical_kind();   
  } else {
    temp.type = EXPR_OP;  
    g95_clear_ts(&temp.ts);         
    temp.operator = INTRINSIC_NONE;          
    temp.op1 = u;
    temp.op2 = c; 
    g95_type_convert_binary(&temp);   
    m->ts = temp.ts;    
  }       
       
  m->rank = (u->rank == 2 && c->rank == 2) ? 2 : 1;      
      
  m->value.function.name = 
    g95_get_string(PREFIX "matmul_%c%d", g95_type_letter(m->ts.type),      
		   m->ts.kind);     
}          
          
          
 
 
void g95_resolve_any(g95_expr *a, g95_expr *mask, g95_expr *d) {       
       
  a->ts = mask->ts;  
  
  if (d == NULL || mask->rank == 1)     
    a->value.function.name = g95_get_string(PREFIX "any0_%d", mask->ts.kind);       
  else {        
    a->rank = mask->rank - 1;       
    a->value.function.name = g95_get_string(PREFIX "anyn_%d", mask->ts.kind); 
  }      
}      
      
      
    
    
void g95_resolve_anint(g95_expr *i, g95_expr *v, g95_expr *knd) {    
    
  i->ts.type = v->ts.type;       
  i->ts.kind = (knd == NULL) ? v->ts.kind       
    : mpz_get_si(knd->value.integer);          
          
  i->value.function.name =  
    g95_get_string(PREFIX "anint_%d_%c%d", i->ts.kind, 
		   g95_type_letter(v->ts.type), v->ts.kind);       
}      
      
      
        
        
void g95_resolve_achar(g95_expr *s, g95_expr *v) {  
  
  s->ts.type = BT_CHARACTER;
  s->ts.kind = g95_default_character_kind();         
         
  s->value.function.name = g95_get_string(PREFIX "char_%d", s->ts.kind);  
}      
      
      


void g95_resolve_minval(g95_expr *g, g95_expr *ap, g95_expr *d,   
			     g95_expr *maski) {     
     
  g->ts = ap->ts;      
      
  if (d != NULL && ap->rank != 1)      
    g->rank = ap->rank - 1;          
          
  g->value.function.name =         
    g95_get_string(PREFIX "%s_%c%d", d ? "dminval" : "minval",        
                   g95_type_letter(ap->ts.type), ap->ts.kind);  
}


          
          
void g95_resolve_lbound(g95_expr *x, g95_expr *block, g95_expr *r) {       
static char lbound[] = PREFIX "lbound";  
  
  x->ts.type = BT_INTEGER;    
  x->ts.kind = g95_default_integer_kind();          
          
  x->value.function.name = lbound;      
      
  if (r == NULL) x->rank = 1;         
}  
  
  
