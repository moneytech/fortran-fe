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
  g95_constructor *head, *tail;    
    
  try (*expand_work_function)(g95_expr *);          
} expand_info;     
     
static expand_info current_expand;         
         
static g95_typespec constructor_ts;      
static enum { CONS_START, CONS_GOOD, CONS_BAD } cons_state;  
int g95_constructor_string_length;      
      
static try expand_constructor(g95_constructor *); 
 
  
  
/* g95_append_constructor()-- Given an array constructor expression,
 * append the new expression node onto the constructor. */          
          
void g95_append_constructor(g95_expr *start, g95_expr *new) {          
g95_constructor *c;    
    
  if (start->value.constructor == NULL)     
    start->value.constructor = c = g95_get_constructor();         
  else {          
    c = start->value.constructor; 
    while(c->next)    
      c=c->next;          
          
    c->next = g95_get_constructor();          
    c = c->next;    
  }        
        
  c->expr = new;  
  
  if (new->ts.type != start->ts.type || new->ts.kind != start->ts.kind)         
    g95_internal_error("g95_append_constructor(): New node has wrong kind");       
}       
       
       
     
     
/* ref_dimen_size()-- Get the number of elements in an array section */    
    
static try ref_dimen_size(g95_array_ref *ar, g95_array_spec *as, int dimen,         
			  mpz_t *rslt) {     
mpz_t u, low, stride;         
try f;         
         
  switch(ar->dimen_type[dimen]) {        
  case DIMEN_ELEMENT:    
    mpz_init(*rslt);          
    mpz_set_ui(*rslt, 1);          
    f = SUCCESS;       
    break;    
    
  case DIMEN_VECTOR:         
    f = g95_array_size(ar->start[dimen], rslt);    /* Recurse! */      
    break;   
   
  case DIMEN_RANGE:    
    mpz_init(u);          
    mpz_init(low);       
    mpz_init(stride);         
    f = FAILURE;

    if (ar->start[dimen] == NULL) {    
      if (as->lower[dimen] == NULL || 
	  as->lower[dimen]->type != EXPR_CONSTANT) goto cleanup;      
      mpz_set(low, as->lower[dimen]->value.integer);    
    } else {       
      if (ar->start[dimen]->type != EXPR_CONSTANT) goto cleanup;        
      mpz_set(low, ar->start[dimen]->value.integer);   
    }

    if (ar->end[dimen] == NULL) {  
      if (as->upper[dimen] == NULL ||       
	  as->upper[dimen]->type != EXPR_CONSTANT) goto cleanup;          
      mpz_set(u, as->upper[dimen]->value.integer);         
    } else {    
      if (ar->end[dimen]->type != EXPR_CONSTANT) goto cleanup;
      mpz_set(u, ar->end[dimen]->value.integer);    
    }       
       
    if (ar->stride[dimen] == NULL)        
      mpz_set_ui(stride, 1);   
    else {    
      if (ar->stride[dimen]->type != EXPR_CONSTANT) goto cleanup;   
      mpz_set(stride, ar->stride[dimen]->value.integer);
    }  
  
    mpz_init(*rslt);
    mpz_sub(*rslt, u, low);     
    mpz_add(*rslt, *rslt, stride);      
    mpz_div(*rslt, *rslt, stride);   
   
    /* Zero stride caught earlier */

    if (mpz_cmp_ui(*rslt, 0) < 0) mpz_set_ui(*rslt, 0);          
    f = SUCCESS;      
      
  cleanup:     
    mpz_clear(u);      
    mpz_clear(low);         
    mpz_clear(stride);        
    return f;      
      
  default:  
    g95_internal_error("ref_dimen_size(): Bad dimen type");
  }     
     
  return f; 
}    
    
    
    
    
/* spec_dimen_size()-- Get the size of single dimension of an array
 * specification.  The array is guaranteed to be one dimensional */

static try spec_dimen_size(g95_array_spec *as, int dimen, mpz_t *rslt) {

  if (as == NULL || as->type != AS_EXPLICIT ||   
      as->lower[dimen]->type != EXPR_CONSTANT ||       
      as->upper[dimen]->type != EXPR_CONSTANT) {        
        
    return FAILURE;  
  }    
    
  mpz_init(*rslt);       
       
  mpz_sub(*rslt, as->upper[dimen]->value.integer,        
	  as->lower[dimen]->value.integer);          
          
  mpz_add_ui(*rslt, *rslt, 1);

  return SUCCESS; 
}     
     
     
     
     
/* g95_copy_array_ref()-- Copy an array reference structure */    
    
g95_array_ref *g95_copy_array_ref(g95_array_ref *src) {         
g95_array_ref *d;          
int b;        
        
  if (src == NULL) return NULL;       
       
  d = g95_get_array_ref();       
  *d = *src;         
         
  for(b=0; b<G95_MAX_DIMENSIONS; b++) {   
    d->start[b] = g95_copy_expr(src->start[b]);     
    d->end[b] = g95_copy_expr(src->end[b]);    
    d->stride[b] = g95_copy_expr(src->stride[b]);  
  }    
    
  return d;
}      
      
      
  
  
/* g95_expanded_ac()-- Returns nonzero if an array constructor has
 * been completely expanded (no iterators) and zero if iterators are
 * present. */     
     
int g95_expanded_ac(g95_expr *s) {    
g95_constructor *o;          
          
  if (s->type == EXPR_ARRAY)  
    for(o=s->value.constructor; o; o=o->next)  
      if (o->iterator != NULL || !g95_expanded_ac(o->expr)) return 0;          
          
  return 1;
}


    
    
/* g95_compare_array_spec()-- Compares two array specifications.  */   
   
int g95_compare_array_spec(g95_array_spec *as1, g95_array_spec *as2) {  
int t, w, o;   
   
  if (as1 == NULL && as2 == NULL) return 1;  
  
  if (as1 == NULL || as2 == NULL) return 0;         
         
  if (as1->rank != as2->rank) return 0;        
        
  if (as1->rank == 0) return 1;   
   
  if (as1->type != as2->type) return 0;       
       
  if (as1->type == AS_EXPLICIT)
    for(t=0; t<as1->rank; t++) {     
      if (g95_extract_int(as1->lower[t], &w) != NULL) goto error;    
      if (g95_extract_int(as2->lower[t], &o) != NULL) goto error;    
      if (w != o) return 0;        
        
      if (g95_extract_int(as1->upper[t], &w) != NULL) goto error;        
      if (g95_extract_int(as2->upper[t], &o) != NULL) goto error;  
      if (w != o) return 0;          
    }       
       
  return 1;        
        
error:          
  g95_internal_error("g95_compare_array_spec(): Array spec clobbered");      
  return 0;        /* Keep the compiler happy */        
}      
      
      


/* resolve_array_bound()-- Takes an array bound, resolves the expression,
 * that make up the shape and check associated constraints. */    
    
static try resolve_array_bound(g95_expr *y, int check_constant) {         
         
  if (y == NULL) return SUCCESS;     
     
  if (g95_resolve_expr(y) == FAILURE ||         
      g95_specification_expr(y) == FAILURE ||  
      g95_simplify_expr(y, 0) == FAILURE) return FAILURE;  
  
  if (check_constant && g95_is_constant_expr(y) == 0) {          
    g95_error("Bounds of array '%s' at %L must be constant",     
	      y->symbol->name, &y->where); 
    return FAILURE;          
  }       
       
  return SUCCESS;      
}          
          
          


/* g95_match_array_constructor()-- Match an array constructor */          
          
match g95_match_array_constructor(g95_expr **result) {       
g95_constructor *h, *tail, *n1;         
g95_expr *e1;
locus loc;      
match g;    
    
  if (g95_match(" (/") == MATCH_NO &&      
      g95_match_char('[') == MATCH_NO) return MATCH_NO;     
     
  loc = *g95_current_locus();   
  h = tail = NULL;      
      
  if (g95_match(" /)") == MATCH_YES ||         
      g95_match_char(']') == MATCH_YES) goto empty;   /* Special case */       
       
  for(;;) {        
    g = match_array_cons_element(&n1);     
    if (g == MATCH_ERROR) goto cleanup; 
    if (g == MATCH_NO) goto syntax;     
     
    if (h == NULL)        
      h = n1;    
    else   
      tail->next = n1;   
   
    tail = n1;       
       
    if (g95_match_char(',') == MATCH_NO) break;          
  }      
      
  if (g95_match(" /)") == MATCH_NO &&   
      g95_match_char(']') == MATCH_NO) goto syntax;    
    
empty:   
  e1 = g95_get_expr(); 
 
  e1->type = EXPR_ARRAY; 
  e1->value.constructor = h;
  /* Size must be calculated at resolution time */    
    
  e1->where = loc;    
  e1->rank = 1;  
  
  *result = e1;      
  return MATCH_YES;       
       
syntax:   
  g95_error("Syntax error in array constructor at %C");    
    
cleanup:
  g95_free_constructor(h);  
  return MATCH_ERROR;    
}  
  
  
         
         
/* g95_show_array_ref()-- Show an array reference */    
    
void g95_show_array_ref(g95_array_ref *as) { 
int b;      
      
  g95_status_char('(');     
    
  switch(as->type) {
  case AR_FULL:
    g95_status("FULL"); 
    break;    
    
  case AR_SECTION:           
    for(b=0; b<as->dimen; b++) {      
      if (as->start[b] != NULL)          
	g95_show_expr(as->start[b]);  
  
      g95_status_char(':');     
     
      if (as->end[b] != NULL)    
	g95_show_expr(as->end[b]);  
  
      if (as->stride[b] != NULL) {
	g95_status_char(':');        
	g95_show_expr(as->stride[b]);   
      }

      if (b != as->dimen-1) g95_status(" , ");          
    }         
    break;        
        
  case AR_ELEMENT:       
    for(b=0; b<as->dimen; b++) {
      g95_show_expr(as->start[b]);     
      if (b != as->dimen - 1) g95_status(" , ");   
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
        
        
   
   
/* g95_resolve_array_spec()-- Takes an array specification, resolves
 * the expressions that make up the shape and make sure everything is
 * integral. */  
  
try g95_resolve_array_spec(g95_array_spec *ref, int check_constant) {          
g95_expr *e;      
int v;     
     
  if (ref == NULL) return SUCCESS;          
          
  for(v=0; v<ref->rank; v++) {    
    e = ref->lower[v];        
    if (resolve_array_bound(e, check_constant) == FAILURE) return FAILURE;     
     
    e = ref->upper[v];
    if (resolve_array_bound(e, check_constant) == FAILURE) return FAILURE; 
  }

  return SUCCESS;  
}     
     
     
  
  
/* extract_element()-- Work function that extracts a particular
 * element from an array constructor, freeing the rest. */  
  
static try extract_element(g95_expr *o) {     
     
  if (o->rank != 0) {  /* Something unextractable */        
    g95_free_expr(o); 
    return FAILURE;     
  }        
        
  if (current_expand.extract_count == current_expand.extract_n)          
    current_expand.extracted = o;          
  else   
    g95_free_expr(o);   
   
  current_expand.extract_count++;   
  return SUCCESS;          
}       
       
       
 
 
/* g95_free_constructor()-- Free chains of g95_constructor structures */     
     
void g95_free_constructor(g95_constructor *h) {     
g95_constructor *nxt;        
        
  if (h == NULL) return;        
        
  for(; h; h=nxt) {     
    nxt = h->next;          
          
    g95_free_expr(h->expr);   
    if (h->iterator != NULL) g95_free_iterator(h->iterator, 1);      
    g95_free(h);    
  }       
}          
          
          
        
        
static try ref_size(g95_array_ref *ref, g95_array_spec *as, mpz_t *r) {   
mpz_t size; 
int u;

  mpz_init_set_ui(*r, 1);         
         
  for(u=0; u<ref->dimen; u++) {       
    if (ref_dimen_size(ref, as, u, &size) == FAILURE) {  
      mpz_clear(*r); 
      return FAILURE;        
    }        
        
    mpz_mul(*r, *r, size);    
    mpz_clear(size); 
  }        
        
  return SUCCESS;  
}         
         
         
   
   
/* match_subscript()-- Match a single dimension of an array reference.
 * This can be a single element or an array section.  Any modifications
 * we've made to the ar structure are cleaned up by the caller.  */          
          
static match match_subscript(g95_array_ref *ar, int iv) {       
match x;       
int y;

  y = ar->dimen;      
      
  ar->c_where[y] = *g95_current_locus();         
  ar->start[y] = ar->end[y] = ar->stride[y] = NULL;     
     
  /* We can't be sure of the difference between DIMEN_ELEMENT and
   * DIMEN_VECTOR until we know the type of the element itself at
   * resolution time. */    
    
  ar->dimen_type[y] = DIMEN_UNKNOWN;  
  
  if (g95_match_char(':') == MATCH_YES) goto end_element;  
  
  /* Get start element */         
         
  if (iv)          
    x = g95_match_init_expr(&ar->start[y]);          
  else       
    x = g95_match_expr(&ar->start[y]);     
     
  if (x == MATCH_NO) g95_error("Expected array subscript at %C");    
  if (x != MATCH_YES) return MATCH_ERROR; 
 
  if (g95_match_char(':') == MATCH_NO) return MATCH_YES;    
    
/* Get an optional end element.  Because we've seen the colon, we
 * definitely have a range along this dimension. */        
        
end_element:      
  ar->dimen_type[y] = DIMEN_RANGE;   
   
  if (iv)    
    x = g95_match_init_expr(&ar->end[y]);
  else        
    x = g95_match_expr(&ar->end[y]); 
 
  if (x == MATCH_ERROR) return MATCH_ERROR;  
  
/* See if we have an optional stride */         
         
  if (g95_match_char(':') == MATCH_YES) {         
    x = iv ? g95_match_init_expr(&ar->stride[y])      
      : g95_match_expr(&ar->stride[y]);       
       
    if (x == MATCH_NO) g95_error("Expected array subscript stride at %C");    
    if (x != MATCH_YES) return MATCH_ERROR;    
  }         
         
  return MATCH_YES;   
}     
     
     
        
        
/* g95_free_array_spec()-- Free all of the expressions associated with
 * array bounds specifications */

void g95_free_array_spec(g95_array_spec *as) { 
int a;   
   
  if (as == NULL) return;         
        
  for(a=0; a<as->rank; a++) {
    g95_free_expr(as->lower[a]);    
    g95_free_expr(as->upper[a]);         
  }       
       
  g95_free(as);     
}    
    
    
  
  
/* g95_simplify_iteration_var()-- Given an initialization expression
 * that is a variable reference, substitute the current value of the
 * iteration variable. */        
        
try g95_simplify_iterator_var(g95_expr *a) {         
iterator_stack *o;        
        
  for(o=iter_stack; o; o=o->prev) 
    if (a->symbol == o->variable) break;  
  
  if (o == NULL) return FAILURE;   /* Variable not found */       
       
  g95_replace_expr(a, g95_int_expr(0));     
     
  mpz_set(a->value.integer, o->value);   
   
  return SUCCESS;     
}    
    
    
         
         
static mstring array_specs[] = {   
  minit("AS_EXPLICIT", AS_EXPLICIT),       
  minit("AS_ASSUMED_SHAPE", AS_ASSUMED_SHAPE),     
  minit("AS_DEFERRED", AS_DEFERRED),      
  minit("AS_ASSUMED_SIZE", AS_ASSUMED_SIZE),        
  minit(NULL, 0) };         
         
void g95_show_array_spec(g95_array_spec *ref) {      
int g; 
 
  if (ref == NULL) {    
    g95_status("()");        
    return;   
  }    
    
  g95_status("(%d", ref->rank);  
  
  if (ref->rank != 0) {   
    g95_status(" %s ", g95_code2string(array_specs, ref->type));       
       
    for(g=0; g<ref->rank; g++) {       
      g95_show_expr(ref->lower[g]);     
      g95_status_char(' ');        
      g95_show_expr(ref->upper[g]);         
      g95_status_char(' ');    
    }        
  }        
        
  g95_status(")");    
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
   
   


/* g95_set_array_spec()-- Given a symbol and an array specification,
 * modify the symbol to have that array specification.  The error
 * locus is needed in case something goes wrong.  On failure, the
 * caller must free the spec. */        
        
try g95_set_array_spec(g95_symbol *symb, g95_array_spec *a, locus *error_loc) {

  if (a == NULL) return SUCCESS;    
    
  if (g95_add_dimension(&symb->attr, error_loc) == FAILURE) return FAILURE;    
    
  symb->as = a;     
     
  return SUCCESS;       
}  
  
  
   
   
/* g95_check_constructor()-- Checks a constructor to see if it is a
 * particular kind of expression-- specification, restricted,
 * or initialization as determined by the check_function.  */       
       
try g95_check_constructor(g95_expr *e1,    
			  try (*check_function)(g95_expr *)) {    
cons_stack *base_save;         
try w;     
     
  base_save = stack_base;       
  stack_base = NULL;     
     
  w = check_constructor(e1->value.constructor, check_function);
  stack_base = base_save;    
    
  return w;   
}  
  
  
  
  
/* count_elements()-- Work function that counts the number of elements
 * present in a constructor. */    
    
static try count_elements(g95_expr *c) {        
mpz_t res;    
    
  if (c->rank == 0)       
    mpz_add_ui(*current_expand.count, *current_expand.count, 1); 
  else {      
    if (g95_array_size(c, &res) == FAILURE) {          
      g95_free_expr(c);    
      return FAILURE;
    }    
    
    mpz_add(*current_expand.count, *current_expand.count,    
	    res); 
    mpz_clear(res);       
  } 
 
  g95_free_expr(c);   
  return SUCCESS;  
}    
    
    
         
         
/* array_function_size()-- Deduce the size of an array-value intrinsic
 * for a couple common inquiry functions. */          
          
static try array_function_size(g95_expr *x, mpz_t *res) { 
g95_intrinsic_sym *isym;    
    
  isym = x->value.function.isym;    
  if (isym == NULL) return FAILURE;        
        
  /* TODO: follow elemental functions into their arguments */        
        
  switch(isym->generic_id) {
  case G95_ISYM_MINLOC:   
  case G95_ISYM_MAXLOC:         
  case G95_ISYM_SHAPE:   
  case G95_ISYM_LBOUND:
  case G95_ISYM_UBOUND:      
    /* Special cases where the size of the array is equal to the rank
     * of the first argument.  The second argument (DIM) must not be
     * present. */       
       
    if (x->value.function.actual->next != NULL) break;   
   
    mpz_init_set_ui(*res, x->value.function.actual->u.expr->rank);   
    return SUCCESS;         
  }        
        
  return FAILURE;          
}        
        
        
      
      
/* g95_check_iter_variable()-- Check an EXPR_VARIABLE expression in a
 * constructor to make sure that that variable is an iteration
 * variables. */      
      
try g95_check_iter_variable(g95_expr *e1) {         
         
g95_symbol *s;          
cons_stack *y;  
  
  s = e1->symbol;        
        
  for(y=stack_base; y; y=y->previous)        
    if (s == y->iterator->var->symbol) return SUCCESS;          
          
  return FAILURE; 
}          
          
          
  
  
/* element_string_length()-- Return the length of the string
 * expression, or -1 if this is not possible. */       
       
static int element_string_length(g95_expr *u) {      
      
  if (u->ts.type != BT_CHARACTER) return -1;          
          
  switch(u->type) {          
  case EXPR_CONSTANT:     
    return u->value.character.length;     
     
  case EXPR_VARIABLE:   
    if (u->ref != NULL) return -1;         
         
    u = u->symbol->ts.cl->length;          
    if (u == NULL || u->type != EXPR_CONSTANT) return -1;     
     
    return mpz_get_ui(u->value.integer);     
     
  default: 
    break;          
  }   
   
  return -1;       
}      
      
      
     
     
/* g95_start_constructor()-- Start an array constructor.  The
 * constructor starts with zero elements and should be appended to by
 * g95_append_constructor(). */  
  
g95_expr *g95_start_constructor(bt dtype, int k0, locus *loc) {        
g95_expr *result;     
     
  result = g95_get_expr();      
      
  result->type = EXPR_ARRAY;
  result->rank = 1;     
     
  result->ts.type = dtype;
  result->ts.kind = k0;        
  result->where = *loc;       
       
  return result;  
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
g95_expr **u, **low;        
match h; 
 
  low = &ar->lower[ar->rank - 1];       
  u = &ar->upper[ar->rank - 1];

  if (g95_match_char('*') == MATCH_YES) {     
    *low = g95_int_expr(1);     
    return AS_ASSUMED_SIZE;     
  }        
        
  if (g95_match_char(':') == MATCH_YES) return AS_DEFERRED;        
        
  h = g95_match_expr(u);          
  if (h == MATCH_NO)  
    g95_error("Expected expression in array specification at %C");     
  if (h != MATCH_YES) return AS_UNKNOWN;      
      
  if (g95_match_char(':') == MATCH_NO) {   
    *low = g95_int_expr(1);
    return AS_EXPLICIT;     
  }       
       
  *low = *u; 
  *u = NULL;   
   
  if (g95_match_char('*') == MATCH_YES) return AS_ASSUMED_SIZE;  
  
  h = g95_match_expr(u); 
  if (h == MATCH_ERROR) return AS_UNKNOWN;         
  if (h == MATCH_NO) return AS_ASSUMED_SHAPE;     
     
  return AS_EXPLICIT;        
}      
      
      
        
        
/* g95_array_spec_size()-- Given an array specification, figure out
 * how big it is. */     
     
try g95_array_spec_size(g95_array_spec *as, mpz_t *r) {       
mpz_t sz;    
int p;    
    
  mpz_init_set_ui(*r, 1);         
         
  for(p=0; p<as->rank; p++) {  
    if (spec_dimen_size(as, p, &sz) == FAILURE) {    
      mpz_clear(*r);
      return FAILURE;   
    }       
       
    mpz_mul(*r, *r, sz);       
    mpz_clear(sz);      
  }       
       
  return SUCCESS;       
}  
  
  
    
    
/* g95_array_dimen_size()-- Given an array expression and a dimension,
 * figure out how many elements it has along that dimension.  Returns
 * SUCCESS if we were able to return a result in the 'result'
 * variable, FAILURE otherwise. */    
    
try g95_array_dimen_size(g95_expr *array, int dimen, mpz_t *result) {       
g95_array_spec *spec;        
g95_array_ref *ref;         
int r; 
 
  if (dimen > array->rank - 1)     
    g95_internal_error("g95_array_dimen_size(): Bad dimension");    
    
  switch(array->type) {          
  case EXPR_VARIABLE:      
    if (array->symbol == NULL) return FAILURE;   
   
    g95_find_array_ref(array, &ref, &spec);         
         
    switch(ref->type) {   
    case AR_FULL:
      return spec_dimen_size(spec, dimen, result); 
 
    case AR_SECTION: 
      for(r=0; dimen>=0; r++)  
	if (ref->dimen_type[r] != DIMEN_ELEMENT) dimen--; 
 
      return ref_dimen_size(ref, spec, r-1, result);

    default:     
      g95_internal_error("g95_array_dimen_size(): Bad array ref");   
    }     
     
    break;   
   
  default:      
    if (array->shape == NULL) return FAILURE;   
   
    mpz_init_set(*result, array->shape[dimen]);     
    break;       
  }    
    
  return SUCCESS;     
}      
      
      
          
          
/* g95_match_array_spec()-- Matches an array specification,
 * incidentally figuring out what sort it is.  */

match g95_match_array_spec(g95_array_spec **asp) {        
array_type current_type;     
g95_array_spec *ref;       
int v;  
  
 if (g95_match_char('(') != MATCH_YES) { 
    *asp = NULL;         
    return MATCH_NO;
  }  
  
  ref = g95_get_array_spec();   
   
  for(v=0; v<G95_MAX_DIMENSIONS; v++) {      
    ref->lower[v] = NULL;
    ref->upper[v] = NULL;
  }

  ref->rank = 1;      
      
  for(;;) {          
    current_type = match_array_element_spec(ref);   
   
    if (ref->rank == 1) {   
      if (current_type == AS_UNKNOWN) goto cleanup;     
      ref->type = current_type;   
    } else 
      switch(ref->type) { /* See how current spec meshes with the existing */   
        case AS_UNKNOWN:     
	  goto cleanup;

        case AS_EXPLICIT:         
	  if (current_type == AS_ASSUMED_SIZE) {      
	    ref->type = AS_ASSUMED_SIZE;
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
	    ref->type = AS_ASSUMED_SHAPE;  
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
        
    if (ref->rank >= G95_MAX_DIMENSIONS) {      
      g95_error("Array specification at %C has more than "
		stringize(G95_MAX_DIMENSIONS) " dimensions");       
      goto cleanup;   
    } 
 
    ref->rank++; 
  }        
        
/* If a lower bounds of an assumed shape array is blank, put in one. */       
       
  if (ref->type == AS_ASSUMED_SHAPE) {       
    for(v=0; v<ref->rank; v++) {     
      if (ref->lower[v] == NULL)   
	ref->lower[v] = g95_int_expr(1);       
    }
  }      
      
  *asp = ref; 
  return MATCH_YES;        
        
/* Something went wrong */          
          
cleanup:     
  g95_free_array_spec(ref);   
  return MATCH_ERROR;          
}       
       
       
 
 
/* constant_element()-- Work function for checking that an element of
 * a constructor is a constant, after removal of any iteration
 * variables.  We return FAILURE if not so. */      
      
static try constant_element(g95_expr *e) {          
int retcode;        
        
  retcode = g95_is_constant_expr(e);   
  g95_free_expr(e);         
         
  return retcode ? SUCCESS : FAILURE; 
}     
     
     
  
  
/* g95_array_size()-- Given an array expression, figure out how many
 * elements are in the array.  Returns SUCCESS if this is possible,
 * and sets the 'result' variable.  Otherwise returns FAILURE. */          
          
try g95_array_size(g95_expr *ap, mpz_t *r) {        
expand_info expand_save;    
g95_array_spec *ref;       
g95_array_ref *a;
int b, flag;     
try y;         
         
  switch(ap->type) {        
  case EXPR_ARRAY: 
    flag = g95_suppress_error;        
    g95_suppress_error = 1;  
  
    expand_save = current_expand;        
        
    current_expand.count = r;          
    mpz_init_set_ui(*r, 0);       
       
    current_expand.expand_work_function = count_elements;      
    iter_stack = NULL;       
       
    y = expand_constructor(ap->value.constructor);      
    g95_suppress_error = flag;          
          
    if (y == FAILURE) mpz_clear(*r);      
    current_expand = expand_save;         
    return y;       
       
  case EXPR_VARIABLE:        
    g95_find_array_ref(ap, &a, &ref);        
        
    switch(a->type) {     
    case AR_FULL:      
      return g95_array_spec_size(ref, r);  
  
    case AR_SECTION: 
      return ref_size(a, ref, r);      
      
    default:    
      g95_internal_error("g95_array_size(): Bad ref");      
    }   
   
  case EXPR_FUNCTION:  
    if (array_function_size(ap, r) == SUCCESS) break;        
    /* Fall through */         
         
  default:        
    if (ap->rank == 0 || ap->shape == NULL) return FAILURE;        
        
    mpz_init_set_ui(*r, 1);        
        
    for(b=0; b<ap->rank; b++)          
      mpz_mul(*r, *r, ap->shape[b]);         
         
    break;      
  }

  return SUCCESS;          
}  
  
  
        
        
/* g95_free_array_ref()-- Free an array reference structure and
 * everything it points to. */

void g95_free_array_ref(g95_array_ref *ar) {         
int s;          
          
  for(s=0; s<G95_MAX_DIMENSIONS; s++) {        
    g95_free_expr(ar->start[s]);  
    g95_free_expr(ar->end[s]);      
    g95_free_expr(ar->stride[s]);      
  }       
       
  g95_free(ar);      
}     
     
     
   
   
/* check_constructor()-- Recursive work function for
 * g95_check_constructor().  This amounts to calling the check
 * function for each expression in the constructor, giving variables
 * with the names of iterators a pass.  */   
   
static try check_constructor(g95_constructor *j,        
			     try (*check_function)(g95_expr *)) {         
cons_stack element;          
g95_expr *u;    
try n;    
    
  for(; j; j=j->next) {         
    u = j->expr; 
 
    if (u->type != EXPR_ARRAY) {     
      if ((*check_function)(u) == FAILURE) return FAILURE;   
      continue;          
    }        
        
    element.previous = stack_base;     
    element.iterator = j->iterator;   
   
    stack_base = &element;   
    n = check_constructor(u->value.constructor, check_function);    
    stack_base = element.previous;    
    
    if (n == FAILURE) return FAILURE;     
  }   
   
/* Nothing went wrong, so all OK */  
  
  return SUCCESS;       
}       
       
       
  
  
/* copy_array_spec()-- Copy an array specification. */          
          
g95_array_spec *g95_copy_array_spec(g95_array_spec *src) {          
g95_array_spec *d;
int v; 
 
  if (src == NULL) return NULL;    
    
  d = g95_get_array_spec();        
        
  *d = *src;          
          
  for(v=0; v<d->rank; v++) {  
    d->lower[v] = g95_copy_expr(d->lower[v]);  
    d->upper[v] = g95_copy_expr(d->upper[v]);       
  }          
          
  return d;      
}       
       
       
 
 
/* g95_match_array_ref()-- Match an array reference, whether it is the
 * whole array or a particular elements or a section. */        
        
match g95_match_array_ref(g95_array_ref *ref, int i) {       
match n;

  memset(ref, '\0', sizeof(ref));       
       
  if (g95_match_char('(') != MATCH_YES) { 
    ref->type = AR_FULL;  
    ref->dimen = 0;       
    return MATCH_YES;   
  }      
      
  ref->type = AR_UNKNOWN;    
    
  for(ref->dimen=0; ref->dimen<G95_MAX_DIMENSIONS; ref->dimen++) {        
    n = match_subscript(ref, i);        
    if (n == MATCH_ERROR) goto error;        
        
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
  ref->dimen++;         
  return MATCH_YES;  
}          
          
          
   
   
/* expand_ac_element()-- Work function for expanding a constructor */         
         
static try expand_ac_element(g95_expr *d) {      
g95_constructor *p;      
      
  if (d->rank != 0) { 
    g95_free_expr(d);  
    return FAILURE;      
  }       
       
  p = g95_get_constructor();         
         
  if (current_expand.head == NULL)          
    current_expand.head = p;          
  else         
    current_expand.tail->next = p;    
    
  current_expand.tail = p;     
  p->expr = d;    
  return SUCCESS;  
} 
 
 
 
 
/* check_duplicate_iterator()-- Given an expression node that might be an 
 * array constructor and a symbol, make sure that no iterators in this or
 * child constructors use the symbol as an implied-DO iterator. 
 * Returns nonzero if a duplicate was found. */     
     
static int check_duplicate_iterator(g95_constructor *n, g95_symbol *master) {         
g95_expr *r;

  for(; n; n=n->next) {      
    r = n->expr; 
 
    if (r->type == EXPR_ARRAY &&       
	check_duplicate_iterator(r->value.constructor, master)) return 1;  
  
    if (n->iterator == NULL) continue;      
      
    if (n->iterator->var->symbol == master) {
      g95_error("DO-iterator '%s' at %L is inside iterator of the same name",   
		master->name, &n->where);       
       
      return 1;
    }       
  }   
   
  return 0;          
}    
    
    
      
      
/* g95_expand_iterator()-- Given an iterator, a function to call and
 * something to expand, we effectively assign the correct values to
 * the loop variable and call the expansion function with the thing to
 * expand.  If the expansion function fails, we terminate the loop. */    
    
try g95_expand_iterator(g95_iterator *iterator, try (*expand)(void *),  
			void *f) {      
g95_expr *s, *end, *step;   
iterator_stack frame;
mpz_t trip;
try k;       
       
  s = end = step = NULL;         
         
  k = FAILURE; 
 
  mpz_init(trip);   
  mpz_init(frame.value);   
   
  s = g95_copy_expr(iterator->start);        
  if (g95_simplify_expr(s, 1) == FAILURE) goto cleanup;  
  
  if (s->type != EXPR_CONSTANT || s->ts.type != BT_INTEGER)        
    goto cleanup;       
       
  end = g95_copy_expr(iterator->end);   
  if (g95_simplify_expr(end, 1) == FAILURE) goto cleanup;    
    
  if (end->type != EXPR_CONSTANT || end->ts.type != BT_INTEGER)     
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
         
  mpz_sub(trip, end->value.integer, s->value.integer);    
  mpz_add(trip, trip, step->value.integer);    
  mpz_tdiv_q(trip, trip, step->value.integer);          
          
  mpz_set(frame.value, s->value.integer);        
        
  frame.prev = iter_stack;  
  frame.variable = iterator->var->symbol;       
  iter_stack = &frame;         
         
  while(mpz_sgn(trip) > 0) {  
    if (expand(f) == FAILURE) goto cleanup;        
        
    mpz_add(frame.value, frame.value, step->value.integer);  
    mpz_sub_ui(trip, trip, 1);       
  }        
        
  k = SUCCESS;        
        
cleanup:      
  g95_free_expr(s);         
  g95_free_expr(end);    
  g95_free_expr(step);    
    
  mpz_clear(trip);     
  mpz_clear(frame.value);

  iter_stack = frame.prev;   
   
  return k;        
}        
        
        
   
   
/* g95_expand_constructor()-- Top level subroutine for expanding
 * constructors. */    
    
try g95_expand_constructor(g95_expr *y) { 
expand_info expand_save;        
try r;       
       
  expand_save = current_expand; 
  iter_stack = NULL;    
    
  current_expand.expand_work_function = expand_ac_element;          
  current_expand.head = NULL; 
  current_expand.tail = NULL;       
       
  r = expand_constructor(y->value.constructor);    
    
  if (r != SUCCESS)          
    g95_free_constructor(current_expand.head);       
  else {
    g95_free_constructor(y->value.constructor);       
    y->value.constructor = current_expand.head;        
  }   
   
  current_expand = expand_save;     
     
  return r;          
}         
         
         
     
     
/* expand_constructor()-- Expand a constructor into constant
 * constructors without any iterators, calling the work function for
 * each of the expanded expressions.  The work function needs to
 * either save or free the passed expression. */

static try expand_constructor(g95_constructor *y) {      
g95_expr *j;          
          
  for(; y; y=y->next) {   
    if (y->iterator != NULL) {       
      if (g95_expand_iterator(y->iterator, (void *) expand_expr,       
			      y->expr) == FAILURE)        
	return FAILURE;    
    
      continue;  
    }          
          
    j = y->expr;      
      
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
       
       
    
    
/* g95_array_ref_shape()-- Given an array reference, return the shape
 * of the reference in an array of mpz_t integers. */ 
 
try g95_array_ref_shape(g95_array_ref *spec, g95_array_spec *as, mpz_t *shap) {        
int h;   
   
  h = 0;         
         
  switch(spec->type) {    
  case AR_FULL:    
    for(; h<as->rank; h++)    
      if (spec_dimen_size(as, h, &shap[h]) == FAILURE) goto cleanup;     
     
    return SUCCESS;   
   
  case AR_SECTION:         
    for(; h<spec->dimen; h++)         
      if (ref_dimen_size(spec, as, h, &shap[h]) == FAILURE) goto cleanup;    
    
    return SUCCESS;   
   
  default:        
    break;       
  }       
       
cleanup:      
  for(h--; h>=0; h--)       
    mpz_clear(shap[h]);  
  
  return FAILURE;       
}      
      
      
        
        
/* match_array_list()-- Match a list of array elements. */   
   
static match match_array_list(g95_constructor **r) {       
g95_constructor *h, *head, *tail, *old;      
g95_iterator it;   
locus o;          
g95_expr *k; 
match d;     
int s;        
        
  o = *g95_current_locus();

  if (g95_match_char('(') == MATCH_NO) return MATCH_NO;  
  
  memset(&it, '\0', sizeof(g95_iterator));   
  head = NULL;         
         
  d = match_array_cons_element(&head);          
  if (d != MATCH_YES) goto cleanup;         
         
  tail = head;    
    
  if (g95_match_char(',') != MATCH_YES) {        
    d = MATCH_NO;          
    goto cleanup;         
  }

  for(s=1;; s++) {        
    d = g95_match_iterator(&it, 0);        
    if (d == MATCH_YES) break;  
    if (d == MATCH_ERROR) goto cleanup;   
   
    d = match_array_cons_element(&old);     
    if (d == MATCH_ERROR) goto cleanup;          
    if (d == MATCH_NO) {  
      if (s > 2) goto syntax;          
      d = MATCH_NO;    
      goto cleanup;    /* Could be a complex constant */     
    }

    tail->next = old;          
    tail = old;    
    
    if (g95_match_char(',') != MATCH_YES) { 
      if (s > 2) goto syntax;         
      d = MATCH_NO;   
      goto cleanup;
    }  
  }         
         
  if (g95_match_char(')') != MATCH_YES) goto syntax;  
  
  if (check_duplicate_iterator(head, it.var->symbol)) {     
    d = MATCH_ERROR;      
    goto cleanup;  
  }     
     
  k = g95_get_expr();       
  k->type = EXPR_ARRAY;   
  k->rank = 1;
  k->where = o;
  k->value.constructor = head;    
    
  h = g95_get_constructor();       
  h->where = *g95_current_locus();
  h->iterator = g95_get_iterator();   
  *h->iterator = it;         
         
  h->expr = k;        
  *r = h;

  return MATCH_YES;

syntax: 
  g95_error("Syntax error in array constructor at %C");          
  d = MATCH_ERROR;      
      
cleanup:        
  g95_free_constructor(head);
  g95_free_iterator(&it, 0);      
  g95_set_locus(&o);          
  return d;       
}          
          
          


/* match_array_cons_element()-- match a single element of an array
 * constructor, which can be a single expression or a list of
 * elements. */   
   
static match match_array_cons_element(g95_constructor **r) {        
g95_constructor *q;    
g95_expr *expr;      
match k;          
          
  k = match_array_list(r);  
  if (k != MATCH_NO) return k;       
       
  k = g95_match_expr(&expr);   
  if (k != MATCH_YES) return k;  
  
  q = g95_get_constructor();
  q->where = *g95_current_locus();    
  q->expr = expr;        
        
  *r = q;   
  return MATCH_YES;       
}


       
       
/* g95_resolve_array_constructor()-- Resolve all of the expressions in
 * an array list. */         
         
try g95_resolve_array_constructor(g95_expr *e1) {     
g95_constructor *l;          
try e;         
         
  e = SUCCESS;          
          
  for(l=e1->value.constructor; l; l=l->next) {    
    if (l->iterator != NULL && g95_resolve_iterator(l->iterator) == FAILURE)         
      e = FAILURE;   
   
    if (g95_resolve_expr(l->expr) == FAILURE) e = FAILURE;         
  }   
   
  if (e == SUCCESS) e = g95_check_constructor_type(e1);    
  return e;         
}      
      
      
 
 
/* check_element_type()-- Given an expression, compare its type with
 * the type of the current constructor.  Returns nonzero if an error
 * was issued.  The cons_state variable keeps track of whether the
 * type of the constructor being read or resolved is known to be good,
 * bad or just starting out. */  
  
static int check_element_type(g95_expr *e1) {         
int e, retval;

  retval = 0;       
       
  switch(cons_state) {          
  case CONS_BAD: 
    break;        /* Errors are suppressed */     
     
  case CONS_START: 
    if (e1->ts.type == BT_UNKNOWN)      
      cons_state = CONS_BAD;  
    else {          
      cons_state = CONS_GOOD;         
      constructor_ts = e1->ts;        
    }         
         
    g95_constructor_string_length = element_string_length(e1);          
    break;         
         
  case CONS_GOOD:        
    if (!g95_compare_types(&constructor_ts, &e1->ts)) {        
      g95_error("Element in %s array constructor at %L is %s",          
		g95_typename(&constructor_ts), &e1->where,       
		g95_typename(&e1->ts));    
    
      retval = 1;
      break;  
    }

    e = element_string_length(e1);       
       
    if (g95_constructor_string_length == -1)         
      g95_constructor_string_length = e;   
    else if (e != g95_constructor_string_length && e != -1) {
      g95_error("Element in character array constructor at %L has length "     
		"%d instead of %d", &e1->where, e,
		g95_constructor_string_length);   
   
      retval = 1;    
      break;   
    }         
         
    break;         
  }         
         
  if (retval) cons_state = CONS_BAD; 
  return retval;          
}   
   
   


#ifndef IN_GCC
try g95_expand_ac_element(g95_expr *a) {  
  return SUCCESS;       
}    
#endif
         
         
       
       
/* g95_constant_ac()-- Given an array constructor, determine if the
 * constructor is constant or not by expanding it and making sure that
 * all elements are constants.  This is a bit of a hack since
 * something like (/ (i, i=1,100000000) /) will take a while as
 * opposed to a more clever function that traverses the expression
 * tree. */   
   
int g95_constant_ac(g95_expr *k) {  
expand_info expand_save;   
try rc; 
 
  iter_stack = NULL;     
  expand_save = current_expand;     
  current_expand.expand_work_function = constant_element;    
    
  rc = expand_constructor(k->value.constructor);      
        
  current_expand = expand_save;      
  if (rc == FAILURE) return 0;        
        
  return 1;         
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
        
        
       
       
/* g95_copy_constructor()-- Copy a constructor structure. */        
        
g95_constructor *g95_copy_constructor(g95_constructor *s1) {       
g95_constructor *d1;    
    
  if (s1 == NULL) return NULL;       
       
  d1 = g95_get_constructor();         
  d1->where = s1->where;         
  d1->expr = g95_copy_expr(s1->expr);         
  d1->iterator = copy_iterator(s1->iterator);    
    
  d1->next = g95_copy_constructor(s1->next);        
        
  return d1;        
}     
     
     
          
          
/* g95_expand_data_constructor()-- Top level subroutine for expanding
 * constructors in a DATA statement. */ 
 
try g95_expand_data_constructor(g95_expr *d) {       
expand_info expand_save;  
try retval;

  expand_save = current_expand;       
  iter_stack = NULL;     
     
  current_expand.expand_work_function = g95_expand_ac_element;        
  retval = expand_constructor(d->value.constructor);  
  current_expand = expand_save;     
     
  return retval;        
}     
     
     
       
       
/* g95_get_array_element()-- Given an array expression and an element
 * number (starting at zero), return a pointer to the array element.
 * NULL is returned if the size of the array has been exceeded.  The
 * expression node returned remains a part of the array and should not
 * be freed.  Access is not efficient at all, but this is another
 * place where things do not have to be particularly fast. */

g95_expr *g95_get_array_element(g95_expr *block, int elem) {  
expand_info expand_save;    
g95_expr *m;         
try rc;        
        
  expand_save = current_expand;      
  current_expand.extract_n = elem;      
  current_expand.expand_work_function = extract_element;          
  current_expand.extracted = NULL;
  current_expand.extract_count = 0;    
    
  iter_stack = NULL;         
         
  rc = expand_constructor(block->value.constructor);        
  m = current_expand.extracted;    
  current_expand = expand_save;           
          
  if (rc == FAILURE)         
    return NULL;  
  
  return m;     
}       
       
       
       
       
/* check_constructor_type()-- Recursive work function for
 * g95_check_constructor_type(). */         
         
static try check_constructor_type(g95_constructor *u) {          
g95_expr *x;    
    
  for(; u; u=u->next) {          
    x = u->expr;     
     
    if (x->type == EXPR_ARRAY) {       
      if (check_constructor_type(x->value.constructor) == FAILURE) 
	return FAILURE;       
       
      continue;         
    }   
   
    if (check_element_type(x)) return FAILURE; 
  }        
        
  return SUCCESS;  
} 
 
 
      
      
/* g95_check_constructor_type()-- Check that all elements of an array
 * constructor are the same type.  On FAILURE, an error has been
 * generated. */   
   
try g95_check_constructor_type(g95_expr *b) {      
try d; 
 
  cons_state = CONS_START;          
  g95_clear_ts(&constructor_ts);       
  g95_constructor_string_length = -1;      
      
  d = check_constructor_type(b->value.constructor);     
  if (d == SUCCESS && b->ts.type == BT_UNKNOWN) b->ts = constructor_ts;       
       
  return d;    
}        
        
        


/* g95_find_array_ref()-- Given an array expression, find the array
 * reference and/or specification that characterizes the reference. */      
      
void g95_find_array_ref(g95_expr *n, g95_array_ref **ar_p,  
			g95_array_spec **as_p) {         
g95_array_spec *spec;   
g95_ref *re;      
      
  spec = n->symbol->as;

  for(re=n->ref; re; re=re->next)   
    switch(re->type) {     
    case REF_ARRAY:  
      if (re->u.ar.type == AR_FULL || re->u.ar.type == AR_SECTION) goto done;          
          
      spec = NULL;  
      break;          
          
    case REF_COMPONENT: 
      spec = re->u.c.component->as;    
      break;   
   
    case REF_SUBSTRING:
      goto error;           
    }      
      
error:         
  g95_internal_error("g95_find_array_ref(): No ref found");   
   
done:          
          
  if (ar_p != NULL) *ar_p = &re->u.ar;          
  if (as_p != NULL) *as_p = spec;    
}  
