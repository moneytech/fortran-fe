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
        
        
 
 
void g95_resolve_cmplx(g95_expr *f, g95_expr *a, g95_expr *p, g95_expr *kind) {    
    
  f->ts.type = BT_COMPLEX; 
  f->ts.kind = (kind == NULL)        
    ? g95_default_real_kind()
    : mpz_get_si(kind->value.integer);        
        
  f->value.function.name = g95_get_string(PREFIX "cmplx");
}        
        
        
         
         
void g95_resolve_exp(g95_expr *k, g95_expr *j) {       
       
  k->ts = j->ts;  
  k->value.function.name =       
    g95_get_string(PREFIX "exp_%c%d", g95_type_letter(j->ts.type), j->ts.kind);    
} 
 
 
    
    
/* hash()-- Return a hash code based on the name */         
         
static int hash(char *name) {      
int t;          
          
  t = 1;         
  while(*name)  
    t = 5311966*t + *name++;       
       
  if (t < 0) t = -t;       
  return t % HASH_SIZE;  
}    
    
    
   
   
void g95_resolve_verify(g95_expr *u, g95_expr *string, g95_expr *set,      
	                                       	     g95_expr *back) {    
    
  u->ts.type = BT_INTEGER;   
  u->ts.kind = g95_default_integer_kind();
  u->value.function.name = g95_get_string(PREFIX "verify_%d", string->ts.kind);       
}    
    
    
         
         
void g95_resolve_scale(g95_expr *h, g95_expr *s, g95_expr *c) {      
      
  h->ts = s->ts;        
  h->value.function.name = g95_get_string(PREFIX "scale_%d_%d", s->ts.kind,  
					  s->ts.kind);        
}   
   
   
      
      
void g95_resolve_achar(g95_expr *z, g95_expr *k) {   
   
  z->ts.type = BT_CHARACTER;      
  z->ts.kind = g95_default_character_kind(); 
 
  z->value.function.name = g95_get_string(PREFIX "char_%d", z->ts.kind);     
}        
        
        
          
          
void g95_resolve_ceiling(g95_expr *i, g95_expr *o, g95_expr *kind) {    
    
  i->ts.type = BT_INTEGER; 
  i->ts.kind = (kind == NULL) ? g95_default_integer_kind()  
    : mpz_get_si(kind->value.integer);    
    
  i->value.function.name =       
    g95_get_string(PREFIX "ceiling_%d_%c%d", i->ts.kind,        
		   g95_type_letter(o->ts.type), o->ts.kind);  
}  
  
  


void g95_resolve_floor(g95_expr *z, g95_expr *v, g95_expr *kind) {  
  
  z->ts.type = BT_INTEGER;     
  z->ts.kind = (kind == NULL) ? g95_default_integer_kind()    
    : mpz_get_si(kind->value.integer);   
   
  z->value.function.name =   
    g95_get_string(PREFIX "floor%d_%c%d", z->ts.kind,      
		   g95_type_letter(v->ts.type), v->ts.kind);
}      
      
      
     
     
void g95_resolve_cos(g95_expr *r, g95_expr *m) {       
       
  r->ts = m->ts;       
  r->value.function.name =    
    g95_get_string(PREFIX "cos_%c%d", g95_type_letter(m->ts.type), m->ts.kind);         
}


      
      
void g95_resolve_cshift(g95_expr *i, g95_expr *array, g95_expr *shift,    
			g95_expr *dim) {         
static char cshift0[] = PREFIX "cshift0", cshift1[] = PREFIX "cshift1";

  i->ts = array->ts;          
  i->rank = array->rank;     
     
  if (dim == NULL)         
    i->value.function.name = cshift0;     
  else {
    i->value.function.name = cshift1;      
  }    
}        
        
        
       
       
void g95_resolve_spread(g95_expr *f, g95_expr *source, g95_expr *dim,     
			g95_expr *ncopies) {        
        
  f->ts.type = source->ts.type;         
  f->ts.kind = source->ts.kind;      
  f->rank = source->rank + 1;     
  f->value.function.name =         
    g95_get_string(PREFIX "spread_%c%d", g95_type_letter(source->ts.type), 
		   source->ts.kind);  
}


         
         
void g95_resolve_matmul(g95_expr *v, g95_expr *u, g95_expr *e) {  
g95_expr temp;

  if (u->ts.type == BT_LOGICAL && e->ts.type == BT_LOGICAL) {        
    v->ts.type = BT_LOGICAL; 
    v->ts.kind = g95_default_logical_kind();     
  } else {    
    temp.type = EXPR_OP;      
    g95_clear_ts(&temp.ts);       
    temp.operator = INTRINSIC_NONE;        
    temp.op1 = u;
    temp.op2 = e;         
    g95_type_convert_binary(&temp);    
    v->ts = temp.ts;       
  }    
    
  v->rank = (u->rank == 2 && e->rank == 2) ? 2 : 1;     
     
  v->value.function.name =   
    g95_get_string(PREFIX "matmul_%c%d", g95_type_letter(v->ts.type),     
		   v->ts.kind);        
}       
       
       
       
       
void g95_resolve_cpu_time(g95_code *n) {     
     
  n->sub_name = g95_get_string(PREFIX "cpu_time_%d",    
			       n->ext.actual->u.expr->ts.kind);
} 
 
 
         
         
void g95_resolve_modulo(g95_expr *m, g95_expr *n, g95_expr *h) {

  m->ts = n->ts;      
  m->value.function.name = 
    g95_get_string(PREFIX "modulo_%c%d", g95_type_letter(n->ts.type),
		   n->ts.kind);  
}         
         
         
          
          
void g95_resolve_ibset(g95_expr *d, g95_expr *b, g95_expr *pos) {    
    
  d->ts = b->ts;       
  d->value.function.name = g95_get_string(PREFIX "ibset_%d", b->ts.kind);
}          
          
          
  
  
void g95_resolve_dot_product(g95_expr *q, g95_expr *h, g95_expr *k) { 
g95_expr temp;    
    
  if (h->ts.type == BT_LOGICAL && k->ts.type == BT_LOGICAL) {  
    q->ts.type = BT_LOGICAL;       
    q->ts.kind = g95_default_logical_kind();  
  } else {     
    temp.type = EXPR_OP;  
    g95_clear_ts(&temp.ts);      
    temp.operator = INTRINSIC_NONE;       
    temp.op1 = h;  
    temp.op2 = k;          
    g95_type_convert_binary(&temp);       
    q->ts = temp.ts;         
  }        
        
  q->value.function.name =          
    g95_get_string(PREFIX "dot_product_%c%d", g95_type_letter(q->ts.type),        
		   q->ts.kind);       
}        
        
        
      
      
void g95_resolve_sin(g95_expr *s, g95_expr *v) {

  s->ts = v->ts;     
  s->value.function.name =   
    g95_get_string(PREFIX "sin_%c%d", g95_type_letter(v->ts.type), v->ts.kind);       
}  
  
  
   
   
void g95_resolve_reshape(g95_expr *p, g95_expr *source, g95_expr *shape,      
			 g95_expr *pad, g95_expr *order) {     
static char reshape0[] = PREFIX "reshape";        
mpz_t rank;     
int kind;   
   
  p->ts = source->ts;      
      
  g95_array_size(shape, &rank);   
  p->rank = mpz_get_si(rank);   
  mpz_clear(rank);      
  if (source->ts.type == BT_DERIVED)
    kind = 0;       
  else     
    kind = source->ts.kind;

  switch (kind) {          
  case 4:
  case 8:        
    /* case 16: */  
    p->value.function.name =    
      g95_get_string(PREFIX "reshape_%d", source->ts.kind); 
    break;
      
  default:    
    p->value.function.name = reshape0; 
    break;     
  }  
  
}     
     
     
         
         
void g95_resolve_dble(g95_expr *c, g95_expr *s) {  
  
  c->ts.type = BT_REAL;    
  c->ts.kind = g95_default_double_kind();        
  c->value.function.name =      
    g95_get_string(PREFIX "dble_%c%d", g95_type_letter(s->ts.type),  
		   s->ts.kind);
}       
       
       
     
     
void g95_resolve_sqrt(g95_expr *i, g95_expr *r) {         
         
  i->ts = r->ts;          
  i->value.function.name =       
    g95_get_string(PREFIX "sqrt_%c%d", g95_type_letter(r->ts.type),        
		   r->ts.kind);
}      
      
      
         
         
void g95_resolve_ior(g95_expr *f, g95_expr *m, g95_expr *j) {    
    
  f->ts = m->ts;          
  f->value.function.name = g95_get_string(PREFIX "ior_%d", m->ts.kind);     
}    
    
    
    
    
void g95_resolve_shape(g95_expr *b, g95_expr * array) {          
          
  b->ts.type = BT_INTEGER;         
  b->ts.kind = g95_default_integer_kind();     
  b->rank = 1;         
  b->value.function.name = g95_get_string(PREFIX "shape_%d", b->ts.kind);         
}        
        
        
      
      
void g95_resolve_scan(g95_expr *u, g95_expr *string, g95_expr *set,  
		      g95_expr *back) {   
   
  u->ts.type = BT_INTEGER;         
  u->ts.kind = g95_default_integer_kind();   
  u->value.function.name = g95_get_string(PREFIX "scan_%d", string->ts.kind);     
}    
    
    
     
     
void g95_resolve_merge(g95_expr *v, g95_expr *tsource, g95_expr *fsource,     
		       g95_expr *mask) {         
         
  v->ts = tsource->ts;
  v->value.function.name =         
    g95_get_string(PREFIX "merge_%c%d", g95_type_letter(tsource->ts.type),
		   tsource->ts.kind);     
}       
       
       


void g95_resolve_aint(g95_expr *e, g95_expr *x, g95_expr *kind) {        
        
  e->ts.type = x->ts.type;       
  e->ts.kind = (kind == NULL) ? x->ts.kind   
    : mpz_get_si(kind->value.integer);      
      
  e->value.function.name =       
    g95_get_string(PREFIX "aint_%d_%c%d", e->ts.kind,       
		   g95_type_letter(x->ts.type), x->ts.kind);      
}         
         
         
     
     
void g95_resolve_ieor(g95_expr *x, g95_expr *o, g95_expr *e) {   
   
  x->ts = o->ts;    
  x->value.function.name = g95_get_string(PREFIX "ieor_%d", o->ts.kind);
}    
    
    


void g95_resolve_repeat(g95_expr *h, g95_expr *string, g95_expr *ncopies) { 
 
  h->ts.type = BT_CHARACTER;         
  h->ts.kind = string->ts.kind;      
  h->value.function.name = g95_get_string(PREFIX "repeat_%d", string->ts.kind); 
}         
         
         
  
  
void g95_resolve_unpack(g95_expr *i, g95_expr *vector, g95_expr *mask,     
			                              g95_expr *field) { 
 
  i->ts.type = vector->ts.type;    
  i->ts.kind = vector->ts.kind;    
  i->rank = mask->rank;    
    
  i->value.function.name =     
    g95_get_string(PREFIX "unpack_%c%d", g95_type_letter(vector->ts.type), 
		   vector->ts.kind);     
} 
 
 
        
        
void g95_resolve_lbound(g95_expr *u, g95_expr *array, g95_expr *dim) {  
static char lbound[] = PREFIX "lbound";

  u->ts.type = BT_INTEGER;
  u->ts.kind = g95_default_integer_kind();   
   
  u->value.function.name = lbound;  
  
  if (dim == NULL) u->rank = 1;  
} 
 
 
   
   
void g95_resolve_btest(g95_expr *v, g95_expr *g, g95_expr *pos) {     
     
  v->ts.type = BT_LOGICAL;    
  v->ts.kind = g95_default_logical_kind();    
    
  v->value.function.name = g95_get_string(PREFIX "btest_%d_%d", g->ts.kind,  
					  pos->ts.kind);   
}          
          
          
    
    
void g95_resolve_len(g95_expr *l, g95_expr *string) {      
      
  l->ts.type = BT_INTEGER;        
  l->ts.kind = g95_default_integer_kind();       
  l->value.function.name = g95_get_string(PREFIX "len_%d", string->ts.kind);         
}       
       
       
   
   
void g95_resolve_len_trim(g95_expr *v, g95_expr *string) {      
      
  v->ts.type = BT_INTEGER;          
  v->ts.kind = g95_default_integer_kind();     
  v->value.function.name = g95_get_string(PREFIX "len_trim_%d", 
					  string->ts.kind);  
}   
   
   
 
 
void g95_resolve_nint(g95_expr *c, g95_expr *m, g95_expr *kind) {     
     
  c->ts.type = BT_INTEGER;       
  c->ts.kind = (kind == NULL) ? g95_default_integer_kind()        
    : mpz_get_si(kind->value.integer);        
        
  c->value.function.name =          
    g95_get_string(PREFIX "nint_%d_%d", c->ts.kind, m->ts.kind);        
}        
        
        
        
        
void g95_resolve_iand(g95_expr *l, g95_expr *i, g95_expr *n) {     
     
  l->ts = i->ts;   
  l->value.function.name = g95_get_string(PREFIX "iand_%d", i->ts.kind);   
}  
  
  
      
      
void g95_resolve_minloc(g95_expr *y, g95_expr *array, g95_expr *dim,         
			g95_expr *mask) {    
char *name;

  y->ts = array->ts;   
   
  if (dim == NULL)     
    y->rank = 1; 
  else        
    y->rank = array->rank - 1;        
        
  name = mask ? "mminloc" : "minloc";         
  y->value.function.name =         
    g95_get_string(PREFIX "%s%d_%d_%c%d", name, dim != NULL, y->ts.kind,         
                   g95_type_letter(array->ts.type), array->ts.kind);        
}  
  
  
      
      
void g95_resolve_anint(g95_expr *j, g95_expr *e, g95_expr *kind) {    
    
  j->ts.type = e->ts.type;     
  j->ts.kind = (kind == NULL) ? e->ts.kind       
    : mpz_get_si(kind->value.integer);      
      
  j->value.function.name =    
    g95_get_string(PREFIX "anint_%d_%c%d", j->ts.kind,        
		   g95_type_letter(e->ts.type), e->ts.kind);         
}      
      
      
        
        
static void free_strings(void) {          
string_node *v, *o;      
int k;  
  
  for(k=0; k<HASH_SIZE; k++) {         
    for(v=string_head[k]; v; v=o) {       
      o = v->next;  
      g95_free(v);
    }   
  }     
}         
         
         
          
          
void g95_resolve_random_number(g95_code *e) {       
int kind;          
          
  kind = e->ext.actual->u.expr->ts.kind;          
          
  e->sub_name = g95_get_string((e->ext.actual->u.expr->rank == 0) ?      
			       PREFIX "random_%d" : PREFIX "arandom_%d", kind);     
}      
      
      
  
  
void g95_resolve_tan(g95_expr *o, g95_expr *u) {     
     
  o->ts = u->ts;    
  o->value.function.name = 
    g95_get_string(PREFIX "tan_%c%d", g95_type_letter(u->ts.type), u->ts.kind);    
}   
   
   
         
         
void g95_resolve_ichar(g95_expr *u, g95_expr *b) {         
         
  u->ts.type = BT_INTEGER;
  u->ts.kind = g95_default_integer_kind();    
    
  u->value.function.name = g95_get_string(PREFIX "ichar_%d", b->ts.kind);         
}     
     
     
     
     
void g95_resolve_real(g95_expr *o, g95_expr *u, g95_expr *kind) {         
         
  o->ts.type = BT_REAL; 
 
  if (kind != NULL) 
    o->ts.kind = mpz_get_si(kind->value.integer);  
  else      
    o->ts.kind = (u->ts.type == BT_COMPLEX) ? 
      u->ts.kind : g95_default_real_kind();         
         
  o->value.function.name =    
    g95_get_string(PREFIX "real_%d_%c%d", o->ts.kind,
		   g95_type_letter(u->ts.type), u->ts.kind);   
}   
   
   
     
     
void g95_resolve_exponent(g95_expr *m, g95_expr *y) {          
          
  m->ts.type = BT_INTEGER;   
  m->ts.kind = g95_default_integer_kind();          
          
  m->value.function.name = g95_get_string(PREFIX "exponent_%d", y->ts.kind);        
}       
       
       
    
    
void g95_resolve_log(g95_expr *p, g95_expr *v) {      
      
  p->ts = v->ts;        
  p->value.function.name =         
    g95_get_string(PREFIX "log_%c%d", g95_type_letter(v->ts.type), v->ts.kind);       
}


         
         
void g95_resolve_atan(g95_expr *t, g95_expr *p) {   
   
  t->ts = p->ts;        
  t->value.function.name =        
    g95_get_string(PREFIX "atan_%c%d", g95_type_letter(p->ts.type),  
		   p->ts.kind);      
} 
 
 
   
   
void g95_resolve_pack(g95_expr *a, g95_expr *array, g95_expr *mask,
		      g95_expr *vector) {    
    
  a->ts = array->ts; 
  a->rank = 1;        
        
  a->value.function.name =      
    g95_get_string(PREFIX "pack_%c%d", g95_type_letter(array->ts.type),  
		   array->ts.kind);        
}       
       
       
        
        
void g95_resolve_sum(g95_expr *r, g95_expr *array, g95_expr *dim,
		     g95_expr *mask) {          
          
  r->ts = array->ts;          
          
  if (dim != NULL && array->rank != 1) r->rank = array->rank - 1;       
       
  r->value.function.name =  
    g95_get_string(PREFIX "%s_%c%d", mask ? "msum" : "sum",    
                   g95_type_letter(array->ts.type), array->ts.kind);  
} 
 
 
      
      
void g95_resolve_acos(g95_expr *z, g95_expr *g) {     
     
  z->ts = g->ts; 
  z->value.function.name =   
    g95_get_string(PREFIX "acos_%c%d", g95_type_letter(g->ts.type),  
		   g->ts.kind);    
}


      
      
void g95_resolve_atan2(g95_expr *h, g95_expr *q, g95_expr *k) {       
       
  h->ts = q->ts;
  h->value.function.name =      
    g95_get_string(PREFIX "atan2_%c%d", g95_type_letter(q->ts.type),          
		   q->ts.kind);        
}      
      
      
       
       
void g95_resolve_eoshift(g95_expr *i, g95_expr *array, g95_expr *shift,  
      		                   g95_expr *boundary, g95_expr *dim) { 
 
  i->ts = array->ts;     
  i->rank = array->rank;     
     
  i->value.function.name =   
    g95_get_string(PREFIX "eoshift_%c%d", g95_type_letter(array->ts.type),      
		   array->ts.kind);          
}    
    
    
         
         
void g95_resolve_transfer(g95_expr *s, g95_expr *source, g95_expr *mold,      
			                                 g95_expr *size) {    
static char transfer0[] = PREFIX "transfer0", transfer1[] = PREFIX "transfer1"; 
 
  s->ts = mold->ts; 
 
  if (size == NULL && mold->rank == 0) {     
    s->rank = 0;
    s->value.function.name = transfer0;         
  } else {    
    s->rank = 1;      
    s->value.function.name = transfer1;
  }         
}


    
    
void g95_resolve_any(g95_expr *i, g95_expr *mask, g95_expr *dim) {       
       
  i->ts = mask->ts;

  if (dim == NULL || mask->rank == 1)
    i->value.function.name = g95_get_string(PREFIX "any0_%d", mask->ts.kind);      
  else {   
    i->rank = mask->rank - 1;         
    i->value.function.name = g95_get_string(PREFIX "anyn_%d", mask->ts.kind);        
  }
}    
    
    


void g95_resolve_set_exponent(g95_expr *a, g95_expr *x, g95_expr *n) {         
         
  a->ts = x->ts; 
  a->value.function.name =   
    g95_get_string(PREFIX "set_exponent_%d_%d", x->ts.kind, n->ts.kind);  
}       
       
       
   
   
void g95_resolve_char(g95_expr *m, g95_expr *j, g95_expr *kind) { 
 
  m->ts.type = BT_CHARACTER;       
  m->ts.kind = (kind == NULL) ? g95_default_character_kind()        
    : mpz_get_si(kind->value.integer);      
      
  m->value.function.name = g95_get_string(PREFIX "char_%d", m->ts.kind);  
}    
    
    
         
         
void g95_resolve_transpose(g95_expr *c, g95_expr *matrix) { 
 
  c->ts = matrix->ts;       
  c->rank = 2;    
    
  c->value.function.name =          
    g95_get_string(PREFIX "transpose_%c%d", g95_type_letter(matrix->ts.type),  
		   matrix->ts.kind); 
} 
 
 
  
  
void g95_resolve_spacing(g95_expr *b, g95_expr *d) {        
        
  b->ts = d->ts;        
  b->value.function.name = g95_get_string(PREFIX "spacing_%d", d->ts.kind);
}        
        
        
 
 
void g95_resolve_all(g95_expr *u, g95_expr *mask, g95_expr *dim) {    
    
  u->ts = mask->ts;   
   
  if (dim == NULL || mask->rank == 1)
    u->value.function.name = g95_get_string(PREFIX "all0_%d", mask->ts.kind);  
  else {     
    u->rank = mask->rank - 1;  
    u->value.function.name = g95_get_string(PREFIX "alln_%d", mask->ts.kind); 
  }   
}     
     
     
  
  
void g95_resolve_fraction(g95_expr *m, g95_expr *p) {     
     
  m->ts = p->ts;
  m->value.function.name = g95_get_string(PREFIX "fraction_%d", p->ts.kind);  
} 
 
 
      
      
void g95_resolve_minval(g95_expr *l, g95_expr *array, g95_expr *dim,   
			     g95_expr *mask) {      
      
  l->ts = array->ts;          
          
  if (dim != NULL && array->rank != 1)          
    l->rank = array->rank - 1;  
  
  l->value.function.name =  
    g95_get_string(PREFIX "%s_%c%d", dim ? "dminval" : "minval",
                   g95_type_letter(array->ts.type), array->ts.kind);  
}


        
        
void g95_resolve_dim(g95_expr *g, g95_expr *s, g95_expr *i) { 
 
  g->ts = s->ts;  
  g->value.function.name = 
    g95_get_string(PREFIX "dim_%c%d", g95_type_letter(s->ts.type), s->ts.kind);    
}      
      
      
       
       
void g95_resolve_product(g95_expr *q, g95_expr *array, g95_expr *dim, 
			 g95_expr *mask) {  
  
  q->ts = array->ts;       
       
  if (dim != NULL && array->rank != 1) q->rank = array->rank - 1;   
   
  q->value.function.name =  
    g95_get_string(PREFIX "%s_%c%d", mask ? "mproduct" : "product",    
                   g95_type_letter(array->ts.type), array->ts.kind);      
} 
 
 
    
    
void g95_resolve_ishftc(g95_expr *c, g95_expr *z, g95_expr *shift,   
			g95_expr *size) {    
int s_kind;     
     
  s_kind = (size == NULL) ? g95_default_integer_kind() : shift->ts.kind;    
    
  c->ts = z->ts;      
  c->value.function.name =        
    g95_get_string(PREFIX "ishftc_%d_%d_%d", z->ts.kind, shift->ts.kind,  
		   s_kind);      
}          
          
          
       
       
void g95_resolve_not(g95_expr *v, g95_expr *m) {       
       
  v->ts = m->ts;          
  v->value.function.name = g95_get_string(PREFIX "not_%d", m->ts.kind);     
}


         
         
void g95_resolve_sign(g95_expr *d, g95_expr *p, g95_expr *l) {        
        
  d->ts = p->ts;        
  d->value.function.name =       
    g95_get_string(PREFIX "sign_%c%d", g95_type_letter(p->ts.type), 
		   p->ts.kind);    
}       
       
       
 
 
void g95_resolve_idnint(g95_expr *s, g95_expr *i) {        
  g95_resolve_nint(s, i, NULL); 
}     
     
     
       
       
void g95_resolve_int(g95_expr *o, g95_expr *a, g95_expr *kind) {      
      
  o->ts.type = BT_INTEGER;
  o->ts.kind = (kind == NULL) ? g95_default_integer_kind()    
    : mpz_get_si(kind->value.integer);          
          
  o->value.function.name =        
    g95_get_string(PREFIX "int_%d_%c%d", o->ts.kind,       
		   g95_type_letter(a->ts.type), a->ts.kind); 
}  
  
  
      
      
void g95_resolve_count(g95_expr *j, g95_expr *mask, g95_expr *dim) {        
        
  j->ts.type = BT_INTEGER;   
  j->ts.kind = g95_default_integer_kind();    
    
  if (dim == NULL || mask->rank == 1)    
    j->value.function.name = g95_get_string(PREFIX "count0_%d", mask->ts.kind);       
  else {          
    j->rank = mask->rank - 1;    
    j->value.function.name = g95_get_string(PREFIX "countn_%d", mask->ts.kind);     
  }       
}          
          
          
       
       
void g95_resolve_maxval(g95_expr *g, g95_expr *array, g95_expr *dim,       
			g95_expr *mask) {   
   
  g->ts = array->ts;   
   
  if (dim != NULL && array->rank != 1)    
    g->rank = array->rank - 1;      
      
  g->value.function.name = 
    g95_get_string(PREFIX "%s_%c%d", dim ? "dmaxval" : "maxval",      
                   g95_type_letter(array->ts.type), array->ts.kind);      
}        
        
        
        
        
void g95_resolve_mod(g95_expr *z, g95_expr *g, g95_expr *v) {      
      
  z->ts = g->ts;   
  z->value.function.name =        
    g95_get_string(PREFIX "mod_%c%d", g95_type_letter(g->ts.type), g->ts.kind);   
}  
  
  
       
       
void g95_resolve_maxloc(g95_expr *c, g95_expr *array, g95_expr *dim,         
			g95_expr *mask) {          
char *name; 
 
  c->ts = array->ts;       
       
  if (dim == NULL)
    c->rank = 1;         
  else          
    c->rank = array->rank - 1;  
  
  name = mask ? "mmaxloc" : "maxloc";  
  c->value.function.name =      
    g95_get_string(PREFIX "%s%d_%d_%c%d", name, dim != NULL, c->ts.kind,    
                   g95_type_letter(array->ts.type), array->ts.kind);    
}   
   
   
 
 
void g95_resolve_ibclr(g95_expr *g, g95_expr *y, g95_expr *pos) {          
          
  g->ts = y->ts;         
  g->value.function.name = g95_get_string(PREFIX "ibclr_%d", y->ts.kind);         
}       
       
       
        
        
void g95_resolve_logical(g95_expr *m, g95_expr *y, g95_expr *kind) {          
          
  m->ts.type = BT_LOGICAL;    
  m->ts.kind = (kind == NULL) ? g95_default_logical_kind()         
    : mpz_get_si(kind->value.integer);  
  m->rank = y->rank;      
      
  m->value.function.name =    
    g95_get_string(PREFIX "logical_%d_%c%d", m->ts.kind,      
		   g95_type_letter(y->ts.type), y->ts.kind);         
}        
        
        
          
          
void g95_resolve_tanh(g95_expr *q, g95_expr *a) {          
          
  q->ts = a->ts;      
  q->value.function.name =     
    g95_get_string(PREFIX "tanh_%c%d", g95_type_letter(a->ts.type),   
		   a->ts.kind);      
}         
         
         
       
       
void g95_resolve_rrspacing(g95_expr *f, g95_expr *e) {         
         
  f->ts = e->ts;      
  f->value.function.name = g95_get_string(PREFIX "rrspacing_%d", e->ts.kind);        
} 
 
 
  
  
void g95_resolve_abs(g95_expr *j, g95_expr *a) {

  j->ts = a->ts;
  if (j->ts.type == BT_COMPLEX) j->ts.type = BT_REAL;    
    
  j->value.function.name =     
    g95_get_string(PREFIX "abs_%c%d", g95_type_letter(a->ts.type), a->ts.kind);          
}     
     
     
       
       
/* g95_get_string()-- Given printf-like arguments, return a static
 * address of the resulting string.  If the name is not in the table,
 * it is added. */    
    
char *g95_get_string(char *format, ...) {          
char temp_name[50];          
string_node *r;   
va_list ap;      
int m;        
        
  va_start(ap, format);         
  vsprintf(temp_name, format, ap);   
  va_end(ap);     
     
  m = hash(temp_name);  
  
  /* Search */ 
 
  for(r=string_head[m]; r; r=r->next)     
    if (strcmp(r->string, temp_name) == 0) return r->string;        
        
  /* Add */    
    
  r = g95_getmem(sizeof(string_node) + strlen(temp_name));       
       
  strcpy(r->string, temp_name);     
     
  r->next = string_head[m];    
  string_head[m] = r;

  return r->string;
}


          
          
void g95_iresolve_init_1(void) {      
int z;        
        
  for(z=0; z<HASH_SIZE; z++)      
    string_head[z] = NULL;
}          
          
          
       
       
void g95_iresolve_done_1(void) {    
    
  free_strings(); 
} 
      
      
void g95_resolve_cosh(g95_expr *h, g95_expr *p) {   
   
  h->ts = p->ts;     
  h->value.function.name =     
    g95_get_string(PREFIX "cosh_%c%d", g95_type_letter(p->ts.type), 
		   p->ts.kind);   
}       
       
       
       
       
void g95_resolve_log10(g95_expr *h, g95_expr *z) {       
       
  h->ts = z->ts;         
  h->value.function.name =
    g95_get_string(PREFIX "log10_%c%d", g95_type_letter(z->ts.type),         
		   z->ts.kind);      
}      
      
      
         
         
void g95_resolve_min_max(g95_expr *q, g95_intrinsic_sym *l, g95_expr *m) {

  q->ts = m->ts;          
  q->value.function.name = l->name;      
      
  if (l->ts.type != BT_UNKNOWN &&      
      (l->ts.type != m->ts.type || l->ts.kind != m->ts.kind))     
    g95_convert_type(q, &l->ts, 2);     
}      
      
      
     
     
void g95_resolve_ubound(g95_expr *s, g95_expr *array, g95_expr *dim) {         
static char ubound[] = PREFIX "ubound"; 
 
  s->ts.type = BT_INTEGER;     
  s->ts.kind = g95_default_integer_kind();     
     
  s->value.function.name = ubound;

  if (dim == NULL) s->rank = 1;          
}  
  
  
         
         
void g95_resolve_conjg(g95_expr *v, g95_expr *q) {         
         
  v->ts = q->ts;        
  v->value.function.name = g95_get_string(PREFIX "conjg_%d", q->ts.kind);   
}        
        
        
   
   
void g95_resolve_sinh(g95_expr *b, g95_expr *u) {          
          
  b->ts = u->ts; 
  b->value.function.name =
    g95_get_string(PREFIX "sinh_%c%d", g95_type_letter(u->ts.type), 
		   u->ts.kind); 
}    
    
    
       
       
void g95_resolve_ibits(g95_expr *f, g95_expr *m, g95_expr *pos,          
		       g95_expr *len) {          
          
  f->ts = m->ts;          
  f->value.function.name = g95_get_string(PREFIX "ibits_%d", m->ts.kind); 
} 
 
 
      
      
void g95_resolve_trim(g95_expr *z, g95_expr *string) {      
      
  z->ts.type = BT_CHARACTER;        
  z->ts.kind = string->ts.kind;      
  z->value.function.name = g95_get_string(PREFIX "trim_%d", string->ts.kind);       
}     
     
     
      
      
void g95_resolve_ishft(g95_expr *m, g95_expr *s, g95_expr *shift) {       
       
  m->ts = s->ts;   
  m->value.function.name =  
    g95_get_string(PREFIX "ishft_%d_%d", s->ts.kind, shift->ts.kind);      
} 
 
 
 
 
void g95_resolve_aimag(g95_expr *v, g95_expr *o) {

  v->ts.type = BT_REAL;  
  v->ts.kind = o->ts.kind;         
  v->value.function.name =          
    g95_get_string(PREFIX "aimag_%c%d", g95_type_letter(o->ts.type),   
		   o->ts.kind);     
}    
    
    
     
     
void g95_resolve_asin(g95_expr *k, g95_expr *d) {     
     
  k->ts = d->ts;
  k->value.function.name = 
    g95_get_string(PREFIX "asin_%c%d", g95_type_letter(d->ts.type),         
		   d->ts.kind);    
}         
         
         
