/* Array things
   Copyright (C) 2000, 2001, 2002 Free Software Foundation, Inc.
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
        
#include <string.h>
    
#include "g95.h"
        
        
static match match_array_cons_element(g95_constructor **);       
       
typedef struct cons_stack {       
  g95_iterator *iterator;     
  struct cons_stack *previous;
} cons_stack;

static cons_stack *stack_base;        
        
static try check_constructor(g95_constructor *, try (*)(g95_expr *));   
   
typedef struct iterator_stack {    
  g95_symbol *variable; 
  mpz_t value;   
   
  struct iterator_stack *prev;
} iterator_stack;      
      
static iterator_stack *iter_stack;       
       
typedef struct {
  int extract_count, extract_n; 
  g95_expr *extracted;          
  mpz_t *count;  
  
  try (*expand_work_function)(g95_expr *);         
} expand_info;   
   
static expand_info current_expand;         
         
static try expand_constructor(g95_constructor *);          
          
      
      
/* g95_expand_iterator()-- Given an iterator, a function to call and
 * something to expand, we effectively assign the correct values to
 * the loop variable and call the expansion function with the thing to
 * expand.  If the expansion function fails, we terminate the loop. */          
          
try g95_expand_iterator(g95_iterator *iterator, try (*expand)(void *),      
			void *v) {   
g95_expr *sta, *e, *step;  
iterator_stack frame; 
mpz_t trip;        
try f;   
   
  sta = e = step = NULL;     
     
  f = FAILURE;  
  
  mpz_init(trip);    
  mpz_init(frame.value);         
         
  sta = g95_copy_expr(iterator->start);  
  if (g95_simplify_expr(sta, 1) == FAILURE) goto cleanup;      
      
  if (sta->type != EXPR_CONSTANT || sta->ts.type != BT_INTEGER)          
    goto cleanup;          
          
  e = g95_copy_expr(iterator->end);        
  if (g95_simplify_expr(e, 1) == FAILURE) goto cleanup;         
         
  if (e->type != EXPR_CONSTANT || e->ts.type != BT_INTEGER)       
    goto cleanup;        
        
  step = g95_copy_expr(iterator->step);          
  if (g95_simplify_expr(step, 1) == FAILURE) goto cleanup;     
     
  if (step->type != EXPR_CONSTANT || step->ts.type != BT_INTEGER)    
    goto cleanup;  
  
  if (mpz_sgn(step->value.integer) == 0) { 
    g95_error("Iterator step at %L cannot be zero", &step->where);    
    goto cleanup; 
  }     
     
  /* Calculate the trip count of the loop */  
  
  mpz_sub(trip, e->value.integer, sta->value.integer); 
  mpz_add(trip, trip, step->value.integer); 
  mpz_tdiv_q(trip, trip, step->value.integer);       
       
  mpz_set(frame.value, sta->value.integer);       
       
  frame.prev = iter_stack;   
  frame.variable = iterator->var->symbol;     
  iter_stack = &frame;     
     
  while(mpz_sgn(trip) > 0) {    
    if (expand(v) == FAILURE) goto cleanup;     
     
    mpz_add(frame.value, frame.value, step->value.integer);    
    mpz_sub_ui(trip, trip, 1);      
  }  
  
  f = SUCCESS;         
         
cleanup: 
  g95_free_expr(sta);        
  g95_free_expr(e);
  g95_free_expr(step);  
  
  mpz_clear(trip);          
  mpz_clear(frame.value);         
         
  iter_stack = frame.prev;      
      
  return f;          
}       
       
       
        
        
/* g95_copy_array_ref()-- Copy an array reference structure */

g95_array_ref *g95_copy_array_ref(g95_array_ref *s1) { 
g95_array_ref *dst;  
int f;          
          
  if (s1 == NULL) return NULL;      
      
  dst = g95_get_array_ref();         
         
  *dst = *s1;    
    
  for(f=0; f<G95_MAX_DIMENSIONS; f++) {  
    dst->start[f] = g95_copy_expr(s1->start[f]);  
    dst->end[f] = g95_copy_expr(s1->end[f]);       
    dst->stride[f] = g95_copy_expr(s1->stride[f]);          
  }     
     
  dst->offset = g95_copy_expr(s1->offset);       
       
  return dst; 
}         
         
         
         
         
/* g95_expanded_ac()-- Returns nonzero if an array constructor has
 * been completely expanded (no iterators) and zero if iterators are
 * present. */      
      
int g95_expanded_ac(g95_expr *l) {  
g95_constructor *u;    
    
  if (l->type == EXPR_ARRAY)     
    for(u=l->value.constructor; u; u=u->next)      
      if (u->iterator != NULL || !g95_expanded_ac(u->expr)) return 0;       
       
  return 1;        
}


        
        
/* g95_show_array_ref()-- Show an array reference */       
       
void g95_show_array_ref(g95_array_ref *as) {        
int i; 
 
  g95_status_char('(');      
     
  switch(as->type) {          
  case AR_FULL:    
    g95_status("FULL");      
    break;      
      
  case AR_SECTION:      
    for(i=0; i<as->dimen; i++) { 
      if (as->start[i] != NULL)        
	g95_show_expr(as->start[i]);         
         
      g95_status_char(':');         
         
      if (as->end[i] != NULL)        
	g95_show_expr(as->end[i]);   
   
      if (as->stride[i] != NULL) {   
	g95_status_char(':');          
	g95_show_expr(as->stride[i]);
      }    
    
      if (i != as->dimen-1) g95_status(" , ");      
    }     
    break;       
       
  case AR_ELEMENT:    
    for(i=0; i<as->dimen; i++) {         
      g95_show_expr(as->start[i]);          
      if (i != as->dimen - 1) g95_status(" , ");          
    }
    break;         
         
  case AR_UNKNOWN:         
    g95_status("UNKNOWN");   
    break; 
 
  default:          
    g95_internal_error("g95_show_array_ref(): Unknown array reference"); 
  }        
        
  g95_status_char(')');         
}          
          
          
   
   
/* match_subscript()-- Match a single dimension of an array reference.
 * This can be a single element or an array section.  Any modifications
 * we've made to the ar structure are cleaned up by the caller.  */  
  
static match match_subscript(g95_array_ref *ref, int iv) {   
match q;      
int a;          
          
  a = ref->dimen;       
       
  ref->c_where[a] = *g95_current_locus();         
  ref->start[a] = ref->end[a] = ref->stride[a] = NULL;         
         
  /* We can't be sure of the difference between DIMEN_ELEMENT and
   * DIMEN_VECTOR until we know the type of the element itself at
   * resolution time. */          
          
  ref->dimen_type[a] = DIMEN_UNKNOWN;  
  
  if (g95_match_char(':') == MATCH_YES) goto end_element;    
    
  /* Get start element */          
          
  if (iv) 
    q = g95_match_init_expr(&ref->start[a]);        
  else    
    q = g95_match_expr(&ref->start[a]);    
    
  if (q == MATCH_NO) g95_error("Expected array subscript at %C");    
  if (q != MATCH_YES) return MATCH_ERROR;        
        
  if (g95_match_char(':') == MATCH_NO) return MATCH_YES;    
    
/* Get an optional end element.  Because we've seen the colon, we
 * definitely have a range along this dimension. */     
     
end_element:    
  ref->dimen_type[a] = DIMEN_RANGE;        
        
  if (iv)       
    q = g95_match_init_expr(&ref->end[a]);  
  else  
    q = g95_match_expr(&ref->end[a]);

  if (q == MATCH_ERROR) return MATCH_ERROR;

/* See if we have an optional stride */      
      
  if (g95_match_char(':') == MATCH_YES) {
    q = iv ? g95_match_init_expr(&ref->stride[a])     
      : g95_match_expr(&ref->stride[a]);       
       
    if (q == MATCH_NO) g95_error("Expected array subscript stride at %C");      
    if (q != MATCH_YES) return MATCH_ERROR;  
  }         
         
  return MATCH_YES;
}        
        
        
 
 
/* g95_match_array_ref()-- Match an array reference, whether it is the
 * whole array or a particular elements or a section. */   
   
match g95_match_array_ref(g95_array_ref *spec, g95_array_spec *a, int initial) {         
match d;      
      
  memset(spec, '\0', sizeof(spec));       
       
  spec->where = *g95_current_locus();           
  spec->as = a;          
          
  if (g95_match_char('(') != MATCH_YES) {    
    spec->type = AR_FULL;     
    spec->dimen = 0;    
    return MATCH_YES;        
  }   
   
  spec->type = AR_UNKNOWN;      
      
  for(spec->dimen=0; spec->dimen<G95_MAX_DIMENSIONS; spec->dimen++) {  
    d = match_subscript(spec, initial);     
    if (d == MATCH_ERROR) goto error;  
  
    if (g95_match_char(')') == MATCH_YES) goto matched;   
   
    if (g95_match_char(',') != MATCH_YES) {  
      g95_error("Invalid form of array reference at %C");     
      goto error;      
    }   
  }          
          
  g95_error("Array reference at %C cannot have more than "      
	    stringize(G95_MAX_DIMENSIONS) " dimensions");     
     
error:      
  return MATCH_ERROR;   
   
matched:     
  spec->dimen++;         
         
  return MATCH_YES;        
}


  
  
/* check_constructor()-- Recursive work function for
 * g95_check_constructor().  This amounts to calling the check
 * function for each expression in the constructor, giving variables
 * with the names of iterators a pass.  */ 
 
static try check_constructor(g95_constructor *l,     
			     try (*check_function)(g95_expr *)) {     
cons_stack element;     
g95_expr *u;     
try n;        
        
  for(; l; l=l->next) {          
    u = l->expr;        
        
    if (u->type != EXPR_ARRAY) {   
      if ((*check_function)(u) == FAILURE) return FAILURE;         
      continue;      
    }   
   
    element.previous = stack_base;    
    element.iterator = l->iterator;        
        
    stack_base = &element;      
    n = check_constructor(u->value.constructor, check_function);  
    stack_base = element.previous;       
       
    if (n == FAILURE) return FAILURE;
  }       
       
/* Nothing went wrong, so all OK */   
   
  return SUCCESS;   
}     
     
     
         
         
/* count_elements()-- Work function that counts the number of elements
 * present in a constructor. */

static try count_elements(g95_expr *k) {    
mpz_t result;    
    
  if (k->rank == 0)     
    mpz_add_ui(*current_expand.count, *current_expand.count, 1);     
  else {          
    if (g95_array_size(k, &result) == FAILURE) {         
      g95_free_expr(k);   
      return FAILURE;       
    }         
         
    mpz_add(*current_expand.count, *current_expand.count,         
	    result);  
    mpz_clear(result);    
  }       
       
  g95_free_expr(k);     
  return SUCCESS;    
}     
     
     
          
          
/* copy_array_spec()-- Copy an array specification. */

g95_array_spec *g95_copy_array_spec(g95_array_spec *source) {     
g95_array_spec *des;        
int q;   
   
  if (source == NULL) return NULL;    
    
  des = g95_get_array_spec();   
   
  *des = *source;       
       
  for(q=0; q<des->rank; q++) {   
    des->lower[q] = g95_copy_expr(des->lower[q]);      
    des->upper[q] = g95_copy_expr(des->upper[q]);         
  }  
  
  return des;         
}    
    
    
   
   
/* g95_compare_array_spec()-- Compares two array specifications.  */         
         
int g95_compare_array_spec(g95_array_spec *as1, g95_array_spec *as2) {  
int f, d, t;   
   
  if (as1 == NULL && as2 == NULL) return 1; 
 
  if (as1 == NULL || as2 == NULL) return 0;     
     
  if (as1->rank != as2->rank) return 0;

  if (as1->rank == 0) return 1;        
        
  if (as1->type != as2->type) return 0;

  if (as1->type == AS_EXPLICIT)   
    for(f=0; f<as1->rank; f++) {          
      if (g95_extract_int(as1->lower[f], &d) != NULL) goto error;       
      if (g95_extract_int(as2->lower[f], &t) != NULL) goto error;
      if (d != t) return 0;        
        
      if (g95_extract_int(as1->upper[f], &d) != NULL) goto error;     
      if (g95_extract_int(as2->upper[f], &t) != NULL) goto error; 
      if (d != t) return 0;      
    }         
         
  return 1;         
         
error:     
  g95_internal_error("g95_compare_array_spec(): Array spec clobbered");    
  return 0;        /* Keep the compiler happy */     
}         
         
         
      
      
/* g95_free_array_spec()-- Free all of the expressions associated with
 * array bounds specifications */         
         
void g95_free_array_spec(g95_array_spec *ar) {       
int o; 
 
  if (ar == NULL) return;          
         
  for(o=0; o<ar->rank; o++) {   
    g95_free_expr(ar->lower[o]);     
    g95_free_expr(ar->upper[o]);   
  }  
  
  g95_free(ar);   
}    
    
    
        
        
/* g95_check_constructor()-- Checks a constructor to see if it is a
 * particular kind of expression-- specification, restricted,
 * or initialization as determined by the check_function.  */        
        
try g95_check_constructor(g95_expr *e,        
			  try (*check_function)(g95_expr *)) {         
cons_stack *base_save;      
try h; 
 
  base_save = stack_base;        
  stack_base = NULL;       
       
  h = check_constructor(e->value.constructor, check_function);          
  stack_base = base_save;   
   
  return h;      
}      
      
      
  
  
/* g95_match_array_constructor()-- Match an array constructor */

match g95_match_array_constructor(g95_expr **res) { 
g95_constructor *h, *end, *old;   
g95_expr *e2;       
locus w;    
match k;       
       
  if (g95_match(" (/") == MATCH_NO) return MATCH_NO; 
 
  w = *g95_current_locus();   
  h = end = NULL;        
        
  if (g95_match(" /)") == MATCH_YES) goto empty;   /* Special case */    
    
  for(;;) {        
    k = match_array_cons_element(&old);          
    if (k == MATCH_ERROR) goto cleanup;   
    if (k == MATCH_NO) goto syntax;      
      
    if (h == NULL)      
      h = old;   
    else        
      end->next = old;  
  
    end = old; 
 
    if (g95_match_char(',') == MATCH_NO) break;
  }    
    
  if (g95_match(" /)") == MATCH_NO) goto syntax;     
     
empty:     
  e2 = g95_get_expr();     
     
  e2->type = EXPR_ARRAY;     
     
  e2->value.constructor = h;         
  /* Size must be calculated at resolution time */     
     
  e2->where = w;          
  e2->rank = 1; 
 
  *res = e2;
  return MATCH_YES;        
        
syntax:    
  g95_error("Syntax error in array constructor at %C");  
  
cleanup:          
  g95_free_constructor(h);        
  return MATCH_ERROR;       
}   
   
   
         
         
static mstring array_specs[] = {       
  minit("AS_EXPLICIT", AS_EXPLICIT),       
  minit("AS_ASSUMED_SHAPE", AS_ASSUMED_SHAPE),       
  minit("AS_DEFERRED", AS_DEFERRED),      
  minit("AS_ASSUMED_SIZE", AS_ASSUMED_SIZE),
  minit(NULL, 0) };       
       
void g95_show_array_spec(g95_array_spec *ar) {      
int e;          
          
  if (ar == NULL) {  
    g95_status("()");    
    return;  
  }       
       
  g95_status("(%d", ar->rank);    
    
  if (ar->rank != 0) {       
    g95_status(" %s ", g95_code2string(array_specs, ar->type));    
    
    for(e=0; e<ar->rank; e++) {      
      g95_show_expr(ar->lower[e]); 
      g95_status_char(' ');      
      g95_show_expr(ar->upper[e]);        
      g95_status_char(' ');         
    }         
  }    
    
  g95_status(")");        
}         
         
         
       
       
/* g95_free_constructor()-- Free chains of g95_constructor structures */      
      
void g95_free_constructor(g95_constructor *c) {
g95_constructor *nxt;

  if (c == NULL) return;      
      
  for(; c; c=nxt) {
    nxt = c->next;  
  
    g95_free_expr(c->expr);          
    if (c->iterator != NULL) g95_free_iterator(c->iterator, 1);     
    g95_free(c);        
  }   
}    
    
    


/* check_duplicate_iterator()-- Given an expression node that might be an 
 * array constructor and a symbol, make sure that no iterators in this or
 * child constructors use the symbol as an implied-DO iterator. 
 * Returns nonzero if a duplicate was found. */ 
 
static int check_duplicate_iterator(g95_constructor *m, g95_symbol *master) { 
g95_expr *i;       
       
  for(; m; m=m->next) {  
    i = m->expr;      
      
    if (i->type == EXPR_ARRAY &&    
	check_duplicate_iterator(i->value.constructor, master)) return 1; 
 
    if (m->iterator == NULL) continue;          
          
    if (m->iterator->var->symbol == master) {   
      g95_error("DO-iterator '%s' at %L is inside iterator of the same name",          
		master->name, &m->where);         
         
      return 1;          
    }   
  }      
      
  return 0;          
}  
  
  
   
   
/* expand_expr()-- Expand an expression with that is inside of a
 * constructor, recursing into other constructors if present. */         
         
static try expand_expr(g95_expr *s) {         
         
  if (s->type == EXPR_ARRAY)     
    return expand_constructor(s->value.constructor);         
         
  s = g95_copy_expr(s);        
        
  if (g95_simplify_expr(s, 1) == FAILURE) {      
    g95_free_expr(s);  
    return FAILURE;  
  }

  return current_expand.expand_work_function(s);
}         
         
         
       
       
/* ref_dimen_size()-- Get the number of elements in an array section */         
         
static try ref_dimen_size(g95_array_ref *ref, int dimen, mpz_t *rslt) {          
mpz_t up, lower, stride;      
try n;          
          
  switch(ref->dimen_type[dimen]) {     
  case DIMEN_ELEMENT:        
    mpz_init(*rslt);      
    mpz_set_ui(*rslt, 1);        
    n = SUCCESS;    
    break;         
         
  case DIMEN_VECTOR:         
    n = g95_array_size(ref->start[dimen], rslt);    /* Recurse! */      
    break;          
          
  case DIMEN_RANGE:         
    mpz_init(up);     
    mpz_init(lower);   
    mpz_init(stride);        
    n = FAILURE;  
  
    if (ref->start[dimen] == NULL) {          
      if (ref->as->lower[dimen] == NULL || 
	  ref->as->lower[dimen]->type != EXPR_CONSTANT) goto cleanup;          
      mpz_set(lower, ref->as->lower[dimen]->value.integer); 
    } else {    
      if (ref->start[dimen]->type != EXPR_CONSTANT) goto cleanup;          
      mpz_set(lower, ref->start[dimen]->value.integer); 
    }   
   
    if (ref->end[dimen] == NULL) {          
      if (ref->as->upper[dimen] == NULL ||      
	  ref->as->upper[dimen]->type != EXPR_CONSTANT) goto cleanup;    
      mpz_set(up, ref->as->upper[dimen]->value.integer);  
    } else { 
      if (ref->end[dimen]->type != EXPR_CONSTANT) goto cleanup;       
      mpz_set(up, ref->end[dimen]->value.integer);   
    }         
         
    if (ref->stride[dimen] == NULL)      
      mpz_set_ui(stride, 1);          
    else { 
      if (ref->stride[dimen]->type != EXPR_CONSTANT) goto cleanup;        
      mpz_set(stride, ref->stride[dimen]->value.integer);         
    }         
         
    mpz_init(*rslt);
    mpz_sub(*rslt, up, lower);        
    mpz_add(*rslt, *rslt, stride);  
    mpz_div(*rslt, *rslt, stride);          
          
    /* Zero stride caught earlier */   
   
    if (mpz_cmp_ui(*rslt, 0) < 0) mpz_set_ui(*rslt, 0);   
    n = SUCCESS;

  cleanup:         
    mpz_clear(up);          
    mpz_clear(lower);  
    mpz_clear(stride);       
    return n;         
         
  default:      
    g95_internal_error("ref_dimen_size(): Bad dimen type");  
  }       
       
  return n;   
} 
 
 
    
    
/* resolve_array_bound()-- Takes an array bound, resolves the expression,
 * that make up the shape and check associated constraints. */     
     
static try resolve_array_bound(g95_expr *t, int check_constant) {        
        
  if (t == NULL) return SUCCESS; 
 
  if (g95_resolve_expr(t) == FAILURE ||  
      g95_specification_expr(t) == FAILURE) return FAILURE;        
        
  if (check_constant && g95_is_constant_expr(t) == 0) {  
    g95_error("Variable '%s' at %L in this context must be constant",         
	      t->symbol->name, &t->where);      
    return FAILURE;        
  }  
  
  return SUCCESS;        
}          
          
          
         
         
/* g95_resolve_array_spec()-- Takes an array specification, resolves
 * the expressions that make up the shape and make sure everything is
 * integral. */    
    
try g95_resolve_array_spec(g95_array_spec *ar, int check_constant) {    
g95_expr *w;   
int q;        
        
  if (ar == NULL) return SUCCESS;         
         
  for(q=0; q<ar->rank; q++) { 
    w = ar->lower[q];       
    if (resolve_array_bound(w, check_constant) == FAILURE) return FAILURE; 
 
    w = ar->upper[q];       
    if (resolve_array_bound(w, check_constant) == FAILURE) return FAILURE;    
  }        
        
  return SUCCESS;  
}          
          
          
         
         
/* match_array_list()-- Match a list of array elements. */ 
 
static match match_array_list(g95_constructor **rslt) { 
g95_constructor *f, *h, *t, *n1;          
g95_iterator it;    
locus old;     
g95_expr *q;    
match m;    
int k;  
  
  old = *g95_current_locus(); 
 
  if (g95_match_char('(') == MATCH_NO) return MATCH_NO;       
       
  memset(&it, '\0', sizeof(g95_iterator));     
  h = NULL;

  m = match_array_cons_element(&h); 
  if (m != MATCH_YES) goto cleanup;         
         
  t = h;

  if (g95_match_char(',') != MATCH_YES) {  
    m = MATCH_NO; 
    goto cleanup;
  }        
        
  for(k=1;; k++) {     
    m = g95_match_iterator(&it, 0);       
    if (m == MATCH_YES) break;         
    if (m == MATCH_ERROR) goto cleanup;     
     
    m = match_array_cons_element(&n1);       
    if (m == MATCH_ERROR) goto cleanup; 
    if (m == MATCH_NO) {          
      if (k > 2) goto syntax;          
      m = MATCH_NO;
      goto cleanup;    /* Could be a complex constant */    
    }       
       
    t->next = n1;      
    t = n1;   
   
    if (g95_match_char(',') != MATCH_YES) {   
      if (k > 2) goto syntax; 
      m = MATCH_NO;          
      goto cleanup;         
    }
  }

  if (g95_match_char(')') != MATCH_YES) goto syntax;    
    
  if (check_duplicate_iterator(h, it.var->symbol)) {        
    m = MATCH_ERROR;   
    goto cleanup;          
  }  
  
  q = g95_get_expr();       
  q->type = EXPR_ARRAY;     
  q->where = old;        
  q->value.constructor = h;        
        
  f = g95_get_constructor();    
  f->where = *g95_current_locus();     
  f->iterator = g95_get_iterator();          
  *f->iterator = it;

  f->expr = q;       
  *rslt = f;       
       
  return MATCH_YES;         
         
syntax:       
  g95_error("Syntax error in array constructor at %C");     
  m = MATCH_ERROR;     
     
cleanup:         
  g95_free_constructor(h);     
  g95_free_iterator(&it, 0);     
  g95_set_locus(&old);          
  return m;         
}  
  
  
      
      
/* match_array_cons_element()-- match a single element of an array
 * constructor, which can be a single expression or a list of
 * elements. */      
      
static match match_array_cons_element(g95_constructor **rslt) {     
g95_constructor *o;   
g95_expr *e2;      
match g;  
  
  g = match_array_list(rslt);
  if (g != MATCH_NO) return g;          
          
  g = g95_match_expr(&e2);
  if (g != MATCH_YES) return g;   
   
  o = g95_get_constructor();   
  o->where = *g95_current_locus();          
  o->expr = e2;          
          
  *rslt = o;          
  return MATCH_YES;            
}       
       
       
  
  
/* resolve_array_list()-- Recursive array list resolution function.
 * All of the elements must be of the same type. */  
  
static try resolve_array_list(g95_constructor *y) {       
try l;    
    
  l = SUCCESS;     
     
  for(;y ;y=y->next) {      
    if (y->iterator != NULL && g95_resolve_iterator(y->iterator) == FAILURE)       
      l = FAILURE;        
        
    if (g95_resolve_expr(y->expr) == FAILURE) l = FAILURE; 
  }  
  
  return l;         
}     
     
     
      
      
/* constant_element()-- Work function for checking that an element of
 * a constructor is a constant, after removal of any iteration
 * variables.  We return FAILURE if not so. */      
      
static try constant_element(g95_expr *c) {       
int r;      
      
  r = g95_is_constant_expr(c);         
  g95_free_expr(c);   
   
  return r ? SUCCESS : FAILURE;      
}   
   
   
  
  
/* g95_expand_constructor()-- Top level subroutine for expanding
 * constructors. */        
        
try g95_expand_constructor(g95_expr *k) {  
expand_info expand_save; 
try r;

  expand_save = current_expand; 
  iter_stack = NULL;       
       
  current_expand.expand_work_function = g95_expand_ac_element;  
  r = expand_constructor(k->value.constructor);          
  current_expand = expand_save;  
  
  return r;         
}   
   
   
   
   
/* match_array_element_spec()-- Match a single array element
 * specification.  The return values as well as the upper and lower
 * bounds of the array spec are filled in according to what we see on
 * the input.  The caller makes sure individual specifications make
 * sense as a whole.
 *
 *       Parsed       Lower   Upper  Returned
 *       ------------------------------------
 *         :          NULL    NULL   AS_DEFERRED
 *         x           1       x     AS_EXPLICIT
 *         x:          x      NULL   AS_ASSUMED_SHAPE
 *         x:y         x       y     AS_EXPLICIT
 *         x:*         x      NULL   AS_ASSUMED_SIZE
 *         *           1      NULL   AS_ASSUMED_SIZE
 * Anything else AS_UNKNOWN */     
     
static array_type match_array_element_spec(g95_array_spec *ar) {        
g95_expr **u, **lower;  
match x;  
  
  lower = &ar->lower[ar->rank - 1];        
  u = &ar->upper[ar->rank - 1];        
        
  if (g95_match_char('*') == MATCH_YES) {         
    *lower = g95_int_expr(1);       
    return AS_ASSUMED_SIZE;     
  }          
          
  if (g95_match_char(':') == MATCH_YES) return AS_DEFERRED;        
        
  x = g95_match_expr(u);   
  if (x == MATCH_NO)          
    g95_error("Expected expression in array specification at %C");
  if (x != MATCH_YES) return AS_UNKNOWN;     
     
  if (g95_specification_expr(*u) == FAILURE) return AS_UNKNOWN;   
   
  if (g95_match_char(':') == MATCH_NO) { 
    *lower = g95_int_expr(1);
    return AS_EXPLICIT;         
  }       
       
  *lower = *u;  
  *u = NULL;    
    
  if (g95_match_char('*') == MATCH_YES) return AS_ASSUMED_SIZE;        
        
  x = g95_match_expr(u);  
  if (x == MATCH_ERROR) return AS_UNKNOWN;     
  if (x == MATCH_NO) return AS_ASSUMED_SHAPE;    
    
  if (g95_specification_expr(*u) == FAILURE) return AS_UNKNOWN;     
     
  return AS_EXPLICIT;          
} 
 
 
         
         
/* g95_check_iter_variable()-- Check an EXPR_VARIABLE expression in a
 * constructor to make sure that that variable is an iteration
 * variables. */       
       
try g95_check_iter_variable(g95_expr *expr) {   
   
g95_symbol *symb;    
cons_stack *h;   
   
  symb = expr->symbol; 
 
  for(h=stack_base; h; h=h->previous)       
    if (symb == h->iterator->var->symbol) return SUCCESS;    
    
  return FAILURE;  
}        
        
        
     
     
/* g95_resolve_array_constructor()-- Resolve all of the expressions in
 * an array list.  TODO: String lengths. */

try g95_resolve_array_constructor(g95_expr *e1) {     
try p; 
 
  p = resolve_array_list(e1->value.constructor);     
  if (p == SUCCESS) p = g95_check_constructor_type(e1);

  return p;     
}     
     
     
          
          
/* g95_match_array_spec()-- Matches an array specification,
 * incidentally figuring out what sort it is.  */       
       
match g95_match_array_spec(g95_array_spec **asp) {       
array_type current_type;      
g95_array_spec *spec;       
int f;       
       
 if (g95_match_char('(') != MATCH_YES) {      
    *asp = NULL;     
    return MATCH_NO; 
  }  
  
  spec = g95_get_array_spec();   
   
  for(f=0; f<G95_MAX_DIMENSIONS; f++) {          
    spec->lower[f] = NULL;        
    spec->upper[f] = NULL;       
  }   
   
  spec->rank = 1; 
 
  for(;;) {    
    current_type = match_array_element_spec(spec); 
 
    if (spec->rank == 1) {   
      if (current_type == AS_UNKNOWN) goto cleanup;         
      spec->type = current_type;  
    } else 
      switch(spec->type) { /* See how current spec meshes with the existing */   
        case AS_UNKNOWN:     
	  goto cleanup;       
       
        case AS_EXPLICIT:     
	  if (current_type == AS_ASSUMED_SIZE) {    
	    spec->type = AS_ASSUMED_SIZE; 
	    break;    
	  }      
      
	  if (current_type == AS_EXPLICIT) break;      
      
	  g95_error("Bad array specification for an explicitly shaped array"  
		    " at %C");    
    
	  goto cleanup;
	
        case AS_ASSUMED_SHAPE:  
	  if ((current_type == AS_ASSUMED_SHAPE) || 
	      (current_type == AS_DEFERRED)) break;      
      
	  g95_error("Bad array specification for assumed shape array at %C");        
	  goto cleanup;   
   
        case AS_DEFERRED:       
	  if (current_type == AS_DEFERRED) break;     
     
	  if (current_type == AS_ASSUMED_SHAPE) {         
	    spec->type = AS_ASSUMED_SHAPE;    
	    break;
	  }   
   
	  g95_error("Bad specification for deferred shape array at %C");        
	  goto cleanup;      
	        
        case AS_ASSUMED_SIZE:  
	  g95_error("Bad specification for assumed size array at %C");   
	  goto cleanup;	      
      }          
          
    if (g95_match_char(')') == MATCH_YES) break;       
       
    if (g95_match_char(',') != MATCH_YES) {      
      g95_error("Expected another dimension in array declaration at %C");   
      goto cleanup;   
    }      
      
    if (spec->rank >= G95_MAX_DIMENSIONS) { 
      g95_error("Array specification at %C has more than "        
		stringize(G95_MAX_DIMENSIONS) " dimensions");       
      goto cleanup; 
    }  
  
    spec->rank++; 
  }    
    
/* If a lower bounds of an assumed shape array is blank, put in one. */          
          
  if (spec->type == AS_ASSUMED_SHAPE) {       
    for(f=0; f<spec->rank; f++) { 
      if (spec->lower[f] == NULL)    
	spec->lower[f] = g95_int_expr(1);  
    }          
  }         
         
  *asp = spec;      
  return MATCH_YES;       
       
/* Something went wrong */     
     
cleanup:  
  g95_free_array_spec(spec);  
  return MATCH_ERROR;    
}      
      
      
   
   
/* g95_simplify_iteration_var()-- Given an initialization expression
 * that is a variable reference, substitute the current value of the
 * iteration variable. */        
        
try g95_simplify_iterator_var(g95_expr *o) {        
iterator_stack *x;        
        
  for(x=iter_stack; x; x=x->prev)  
    if (o->symbol == x->variable) break; 
 
  if (x == NULL) return FAILURE;   /* Variable not found */        
        
  g95_replace_expr(o, g95_int_expr(0));         
         
  mpz_set(o->value.integer, x->value);  
  
  return SUCCESS;      
}      
      
      
     
     
/* expand_constructor()-- Expand a constructor into constant
 * constructors without any iterators, calling the work function for
 * each of the expanded expressions.  The work function needs to
 * either save or free the passed expression. */          
          
static try expand_constructor(g95_constructor *p) { 
g95_expr *j;     
     
  for(; p; p=p->next) {       
    if (p->iterator != NULL) {     
      if (g95_expand_iterator(p->iterator, (void *) expand_expr,       
			      p->expr) == FAILURE)
	return FAILURE;   
   
      continue;          
    } 
 
    j = p->expr;       
       
    if (j->type == EXPR_ARRAY) {          
      if (expand_constructor(j->value.constructor) == FAILURE)   
	return FAILURE; 
 
      continue;      
    }     
     
    j = g95_copy_expr(j);         
    if (g95_simplify_expr(j, 1) == FAILURE) {   
      g95_free_expr(j);    
      return FAILURE; 
    }       
       
    if (current_expand.expand_work_function(j) == FAILURE) return FAILURE; 
  }          
          
  return SUCCESS;        
} 
 
 
       
       
/* g95_free_array_ref()-- Free an array reference structure and
 * everything it points to. */

void g95_free_array_ref(g95_array_ref *a) {       
int i;       
       
  for(i=0; i<G95_MAX_DIMENSIONS; i++) {     
    g95_free_expr(a->start[i]);          
    g95_free_expr(a->end[i]);      
    g95_free_expr(a->stride[i]);
  }         
         
  g95_free(a);       
}    
    
    
     
     
/* g95_set_array_spec()-- Given a symbol and an array specification,
 * modify the symbol to have that array specification.  The error
 * locus is needed in case something goes wrong.  On failure, the
 * caller must free the spec. */        
        
try g95_set_array_spec(g95_symbol *sy, g95_array_spec *as, locus *error_loc) {  
  
  if (as == NULL) return SUCCESS;   
   
  if (g95_add_dimension(&sy->attr, error_loc) == FAILURE) return FAILURE;      
      
  sy->as = as;         
         
  return SUCCESS;      
}      
      
      
     
     
/* g95_append_constructor()-- Given an array constructor expression,
 * append the new expression node onto the constructor. */   
   
void g95_append_constructor(g95_expr *b, g95_expr *old) {      
g95_constructor *c;    
    
  if (b->value.constructor == NULL)   
    b->value.constructor = c = g95_get_constructor();     
  else {
    c = b->value.constructor;
    while(c->next)       
      c=c->next;       
       
    c->next = g95_get_constructor();  
    c = c->next; 
  }    
    
  c->expr = old;  
  
  if (old->ts.type != b->ts.type || old->ts.kind != b->ts.kind)      
    g95_internal_error("g95_append_constructor(): New node has wrong kind");
}


     
     
/* extract_element()-- Work function that extracts a particular
 * element from an array constructor, freeing the rest. */        
        
static try extract_element(g95_expr *w) {   
   
  if (w->rank != 0) {  /* Something unextractable */        
    g95_free_expr(w);  
    return FAILURE;
  }   
   
  if (current_expand.extract_count == current_expand.extract_n)          
    current_expand.extracted = w;
  else      
    g95_free_expr(w);  
  
  current_expand.extract_count++; 
  return SUCCESS;      
}        
        
        
 
 
/* g95_constant_ac()-- Given an array constructor, determine if the
 * constructor is constant or not by expanding it and making sure that
 * all elements are constants.  This is a bit of a hack since
 * something like (/ (i, i=1,100000000) /) will take a while as
 * opposed to a more clever function that traverses the expression
 * tree. */    
    
int g95_constant_ac(g95_expr *n) {       
expand_info expand_save;          
try r;     
     
  iter_stack = NULL;        
  expand_save = current_expand;          
  current_expand.expand_work_function = constant_element;       
       
  r = expand_constructor(n->value.constructor);          
            
  current_expand = expand_save;   
  if ( r == FAILURE) return 0;          
          
  return 1;      
}      
      
      
   
   
/* check_element_type()-- Given an expression, compare it's type with
 * the type of the current constructor.  Returns nonzero if an error
 * was issued.  The cons_state variable keeps track of whether the
 * type of the constructor being read or resolved is known to be good,
 * bad or just starting out. */       
       
static g95_typespec constructor_ts;   
static enum { CONS_START, CONS_GOOD, CONS_BAD } cons_state;  
  
static int check_element_type(g95_expr *e2) { 
 
  if (cons_state == CONS_BAD) return 0;  /* Supress further errors */  
  
  if (cons_state == CONS_START) {       
    if (e2->ts.type == BT_UNKNOWN)       
      cons_state = CONS_BAD;        
    else {       
      cons_state = CONS_GOOD;  
      constructor_ts = e2->ts; 
    }        
        
    return 0;   
  } 
 
  if (g95_compare_types(&constructor_ts, &e2->ts)) return 0;  
  
  g95_error("Element in %s array constructor at %L is %s",   
	    g95_typename(&constructor_ts), &e2->where,  
	    g95_typename(&e2->ts));         
         
  cons_state = CONS_BAD;     
  return 1;    
}      
      
      


/* spec_dimen_size()-- Get the size of single dimension of an array
 * specification.  The array is guaranteed to be one dimensional */        
        
static try spec_dimen_size(g95_array_spec *a, int dimen, mpz_t *res) {    
    
  if (a == NULL || a->type != AS_EXPLICIT ||     
      a->lower[dimen]->type != EXPR_CONSTANT ||          
      a->upper[dimen]->type != EXPR_CONSTANT) {   
   
    return FAILURE;     
  }         
         
  mpz_init(*res);  
  
  mpz_sub(*res, a->upper[dimen]->value.integer,       
	  a->lower[dimen]->value.integer);         
         
  mpz_add_ui(*res, *res, 1);      
      
  return SUCCESS;   
}          
          
          
         
         
static try ref_size(g95_array_ref *ref, mpz_t *res) {         
mpz_t s;    
int h;         
         
  mpz_init_set_ui(*res, 1); 
 
  for(h=0; h<ref->dimen; h++) {        
    if (ref_dimen_size(ref, h, &s) == FAILURE) { 
      mpz_clear(*res);    
      return FAILURE;
    }        
        
    mpz_mul(*res, *res, s);    
    mpz_clear(s);  
  }       
       
  return SUCCESS;   
}         
         
         
  
  
#ifndef IN_GCC
try g95_expand_ac_element(g95_expr *h) {   
  return SUCCESS;
}          
#endif
         
         


/* g95_find_array_ref()-- Given an array expression, find the array
 * reference structure that characterizes the reference. */

g95_array_ref *g95_find_array_ref(g95_expr *p) {
g95_ref *r;         
         
  for(r=p->ref; r; r=r->next)      
    if (r->type == REF_ARRAY &&          
	(r->u.ar.type == AR_FULL || r->u.ar.type == AR_SECTION)) break;         
         
  if (r == NULL) g95_internal_error("g95_find_array_ref(): No ref found");        
        
  return &r->u.ar;     
}       
      
      
/* g95_start_constructor()-- Start an array constructor.  The
 * constructor starts with zero elements and should be appended to by
 * g95_append_constructor(). */          
          
g95_expr *g95_start_constructor(bt dtype, int k, locus *old_loc) {        
g95_expr *rslt;

  rslt = g95_get_expr();  
  
  rslt->type = EXPR_ARRAY; 
  rslt->rank = 1;    
    
  rslt->ts.type = dtype;        
  rslt->ts.kind = k; 
  rslt->where = *old_loc;         
         
  return rslt;         
}          
          
          
      
      
/* copy_iterator()-- Copy an iterator structure */

static g95_iterator *copy_iterator(g95_iterator *source) {  
g95_iterator *d;

  if (source == NULL) return NULL;         
         
  d = g95_get_iterator();        
          
  d->var = g95_copy_expr(source->var); 
  d->start = g95_copy_expr(source->start); 
  d->end = g95_copy_expr(source->end);   
  d->step = g95_copy_expr(source->step);         
         
  return d;    
}         
         
         
         
         
/* g95_array_size()-- Given an array expression, figure out how many
 * elements are in the array.  Returns SUCCESS if this is possible,
 * and sets the 'result' variable.  Otherwise returns FAILURE. */ 
 
try g95_array_size(g95_expr *arr, mpz_t *rslt) {
expand_info expand_save;      
g95_ref *r;      
int w, flag; 
try h;

  switch(arr->type) {         
  case EXPR_ARRAY:       
    flag = g95_suppress_error;      
    g95_suppress_error = 1;        
        
    expand_save = current_expand;   
   
    current_expand.count = rslt;       
    mpz_init_set_ui(*rslt, 0);        
        
    current_expand.expand_work_function = count_elements;        
    iter_stack = NULL;   
   
    h = expand_constructor(arr->value.constructor);  
    g95_suppress_error = flag;     
     
    if (h == FAILURE) mpz_clear(*rslt);      
    current_expand = expand_save;         
    return h;  
  
  case EXPR_VARIABLE:         
    for(r=arr->ref; r; r=r->next) {        
      if (r->type != REF_ARRAY) continue;        
        
      if (r->u.ar.type == AR_FULL)      
	return g95_array_spec_size(r->u.ar.as, rslt);  
  
      if (r->u.ar.type == AR_SECTION)	return ref_size(&r->u.ar, rslt);
    }     
     
    return g95_array_spec_size(arr->symbol->as, rslt);         
         
  default:       
    if (arr->rank == 0 || arr->shape == NULL) return FAILURE;        
        
    mpz_init_set_ui(*rslt, 1);      
      
    for(w=0; w<arr->rank; w++)   
      mpz_mul(*rslt, *rslt, arr->shape[w]);          
          
    break;         
  }  
  
  return SUCCESS;          
}       
       
       
       
       
/* g95_array_ref_shape()-- Given an array reference, return the shape
 * of the reference in an array of mpz_t integers. */     
     
try g95_array_ref_shape(g95_array_ref *ar, mpz_t *s) {        
int t;      
      
  t = 0;        
        
  switch(ar->type) {          
  case AR_FULL:      
    for(; t<ar->as->rank; t++)      
      if (spec_dimen_size(ar->as, t, &s[t]) == FAILURE) goto cleanup; 
 
    return SUCCESS;  
  
  case AR_SECTION:    
    for(; t<ar->dimen; t++) 
      if (ref_dimen_size(ar, t, &s[t]) == FAILURE) goto cleanup;       
       
    return SUCCESS;      
      
  default:     
    break;   
  }     
     
 cleanup:    
  for(t--; t>=0; t--)          
    mpz_clear(s[t]);    
    
  return FAILURE;   
}      
      
      
      
      
/* check_constructor_type()-- Recursive work function for
 * g95_check_constructor_type(). */      
      
static try check_constructor_type(g95_constructor *j) { 
g95_expr *n;          
          
  for(; j; j=j->next) {         
    n = j->expr;     
     
    if (n->type == EXPR_ARRAY) {    
      if (check_constructor_type(n->value.constructor) == FAILURE)       
	return FAILURE;        
        
      continue;     
    }      
      
    if (check_element_type(n)) return FAILURE;         
  }

  return SUCCESS;    
}   
   
   
      
      
/* g95_get_array_element()-- Given an array expression and an element
 * number (starting at zero), return a pointer to the array element.
 * NULL is returned if the size of the array has been exceeded.  The
 * expression node returned remains a part of the array and should not
 * be freed.  Access is not efficient at all, but this is another
 * place where things do not have to be particularly fast. */         
         
g95_expr *g95_get_array_element(g95_expr *arr, int element) {  
expand_info expand_save;          
g95_expr *h;
try retval;

  expand_save = current_expand;   
  current_expand.extract_n = element;       
  current_expand.expand_work_function = extract_element;  
  current_expand.extracted = NULL;  
  current_expand.extract_count = 0;         
         
  iter_stack = NULL; 
 
  retval = expand_constructor(arr->value.constructor);         
  h = current_expand.extracted;          
  current_expand = expand_save;          
         
  if (retval == FAILURE)       
    return NULL; 
 
  return h;      
}    
    
    
   
   
/* g95_copy_constructor()-- Copy a constructor structure. */    
    
g95_constructor *g95_copy_constructor(g95_constructor *source) {         
g95_constructor *d;        
        
  if (source == NULL) return NULL;   
   
  d = g95_get_constructor(); 
  d->where = source->where;         
  d->expr = g95_copy_expr(source->expr);       
  d->iterator = copy_iterator(source->iterator);   
   
  d->next = g95_copy_constructor(source->next);         
         
  return d;    
}         
         
         
      
      
/* g95_array_dimen_size()-- Given an array expression and a dimension,
 * figure out how many elements it has along that dimension.  Returns
 * SUCCESS if we were able to return a result in the 'result'
 * variable, FAILURE otherwise. */      
      
try g95_array_dimen_size(g95_expr *arr, int dimen, mpz_t *rslt) { 
g95_ref *ref;      
int n;          
          
  if (dimen > arr->rank - 1)       
    g95_internal_error("g95_array_dimen_size(): Bad dimension");  
  
  switch(arr->type) {     
  case EXPR_VARIABLE:     
  case EXPR_FUNCTION:     
    for(ref=arr->ref; ref; ref=ref->next) {    
      if (ref->type != REF_ARRAY) continue;       
       
      if (ref->u.ar.type == AR_FULL)   
	return spec_dimen_size(ref->u.ar.as, dimen, rslt);        
        
      if (ref->u.ar.type == AR_SECTION) {        
	for(n=0; dimen>=0; n++)        
	  if (ref->u.ar.dimen_type[n] != DIMEN_ELEMENT) dimen--;          
          
	return ref_dimen_size(&ref->u.ar, n-1, rslt);    
      }     
    }  
  
    if (arr->symbol == NULL) return FAILURE;      
    if (spec_dimen_size(arr->symbol->as, dimen, rslt) == FAILURE)      
      return FAILURE;     
     
    break;    
    
  default:          
    if (arr->shape == NULL) return FAILURE;       
       
    mpz_init_set(*rslt, arr->shape[dimen]);       
       
    break; 
  }       
       
  return SUCCESS;      
}          
          
          
  
  
/* g95_check_constructor_type()-- Check that all elements of an array
 * constructor are the same type.  On FAILURE, an error has been
 * generated. */ 
 
try g95_check_constructor_type(g95_expr *f) { 
try a;

  cons_state = CONS_START;    
  g95_clear_ts(&constructor_ts);        
        
  a = check_constructor_type(f->value.constructor); 
  if (a == SUCCESS && f->ts.type == BT_UNKNOWN) f->ts = constructor_ts;        
        
  return a;        
}  
  
  
          
          
/* g95_array_spec_size()-- Given an array specification, figure out
 * how big it is. */ 
 
try g95_array_spec_size(g95_array_spec *as, mpz_t *result) {         
mpz_t sz;     
int k; 
 
  mpz_init_set_ui(*result, 1);

  for(k=0; k<as->rank; k++) {     
    if (spec_dimen_size(as, k, &sz) == FAILURE) {          
      mpz_clear(*result);    
      return FAILURE;        
    }      
      
    mpz_mul(*result, *result, sz);          
    mpz_clear(sz);       
  } 
 
  return SUCCESS;  
}


