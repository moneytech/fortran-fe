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
      
      
/* This parameter is the size of the largest array constructor that we
 * will expand to an array constructor without iterators.
 * Constructors larger than this will remain in the iterator form. */         
         
#define G95_MAX_AC_EXPAND 100
  
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
  g95_constructor *new_head, *new_tail;  
  int extract_count, extract_n;      
  g95_expr *extracted;          
  mpz_t *count;      
      
  try (*expand_work_function)(g95_expr *);     
} expand_info;      
      
static expand_info current_expand;         
         
static try expand_constructor(g95_constructor *);         
         
   
   
/* g95_copy_array_ref()-- Copy an array reference structure */        
        
g95_array_ref *g95_copy_array_ref(g95_array_ref *src) { 
g95_array_ref *dest;   
int t;  
  
  if (src == NULL) return NULL;         
         
  dest = g95_get_array_ref();   
   
  *dest = *src;   
   
  for(t=0; t<G95_MAX_DIMENSIONS; t++) {          
    dest->start[t] = g95_copy_expr(src->start[t]);        
    dest->end[t] = g95_copy_expr(src->end[t]);
    dest->stride[t] = g95_copy_expr(src->stride[t]);       
  }     
     
  dest->offset = g95_copy_expr(src->offset);

  return dest;  
}        
        
        
 
 
/* g95_show_array_ref()-- Show an array reference */   
   
void g95_show_array_ref(g95_array_ref *ar) {    
int w;  
  
  g95_status_char('(');       
      
  switch(ar->type) {          
  case AR_FULL:        
    g95_status("FULL");   
    break;          
          
  case AR_SECTION:  
    for(w=0; w<ar->dimen; w++) {    
      if (ar->start[w] != NULL)    
	g95_show_expr(ar->start[w]);        
        
      g95_status_char(':');

      if (ar->end[w] != NULL)       
	g95_show_expr(ar->end[w]);      
      
      if (ar->stride[w] != NULL) {   
	g95_status_char(':');        
	g95_show_expr(ar->stride[w]);      
      }        
        
      if (w != ar->dimen-1) g95_status(" , ");          
    }         
    break; 
 
  case AR_ELEMENT:    
    for(w=0; w<ar->dimen; w++) {       
      g95_show_expr(ar->start[w]);       
      if (w != ar->dimen - 1) g95_status(" , ");      
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
 
 
 
 
/* g95_free_array_ref()-- Free an array reference structure and
 * everything it points to. */     
     
void g95_free_array_ref(g95_array_ref *ar) {   
int a;

  for(a=0; a<G95_MAX_DIMENSIONS; a++) {
    g95_free_expr(ar->start[a]);    
    g95_free_expr(ar->end[a]);      
    g95_free_expr(ar->stride[a]);       
  }          
          
  g95_free(ar);     
}    
    
    
         
         
/* match_subscript()-- Match a single dimension of an array reference.
 * This can be a single element or an array section.  Any modifications
 * we've made to the ar structure are cleaned up by the caller.  */ 
 
static match match_subscript(g95_array_ref *ar, int init) {         
match h;         
int v; 
 
  v = ar->dimen; 
 
  ar->c_where[v] = *g95_current_locus();          
  ar->start[v] = ar->end[v] = ar->stride[v] = NULL;         
         
  /* We can't be sure of the difference between DIMEN_ELEMENT and
   * DIMEN_VECTOR until we know the type of the element itself at
   * resolution time. */      
      
  ar->dimen_type[v] = DIMEN_UNKNOWN;         
         
  if (g95_match_char(':') == MATCH_YES) goto end_element; 
 
  /* Get start element */       
       
  if (init)     
    h = g95_match_init_expr(&ar->start[v]);  
  else  
    h = g95_match_expr(&ar->start[v]);       
       
  if (h == MATCH_NO) g95_error("Expected array subscript at %C");     
  if (h != MATCH_YES) return MATCH_ERROR;         
         
  if (g95_match_char(':') == MATCH_NO) return MATCH_YES;

/* Get an optional end element.  Because we've seen the colon, we
 * definitely have a range along this dimension. */

end_element:          
  ar->dimen_type[v] = DIMEN_RANGE;  
  
  if (init)         
    h = g95_match_init_expr(&ar->end[v]);    
  else       
    h = g95_match_expr(&ar->end[v]);         
         
  if (h == MATCH_ERROR) return MATCH_ERROR;          
          
/* See if we have an optional stride */      
      
  if (g95_match_char(':') == MATCH_YES) {  
    h = init ? g95_match_init_expr(&ar->stride[v])       
      : g95_match_expr(&ar->stride[v]);   
   
    if (h == MATCH_NO) g95_error("Expected array subscript stride at %C");      
    if (h != MATCH_YES) return MATCH_ERROR;         
  }        
        
  return MATCH_YES;  
}        
        
        
        
        
static mstring array_specs[] = {
  minit("AS_EXPLICIT", AS_EXPLICIT), 
  minit("AS_ASSUMED_SHAPE", AS_ASSUMED_SHAPE),     
  minit("AS_DEFERRED", AS_DEFERRED),          
  minit("AS_ASSUMED_SIZE", AS_ASSUMED_SIZE),         
  minit(NULL, 0) };    
    
void g95_show_array_spec(g95_array_spec *as) {     
int k;     
     
  if (as == NULL) {          
    g95_status("()");     
    return;         
  }      
      
  g95_status("(%d", as->rank);   
   
  if (as->rank != 0) {         
    g95_status(" %s ", g95_code2string(array_specs, as->type)); 
 
    for(k=0; k<as->rank; k++) {     
      g95_show_expr(as->lower[k]);   
      g95_status_char(' ');  
      g95_show_expr(as->upper[k]);   
      g95_status_char(' ');
    }          
  }    
    
  g95_status(")");          
}  
  
  
         
         
/* g95_append_constructor()-- Given an array constructor expression,
 * append the new expression node onto the constructor. */          
          
void g95_append_constructor(g95_expr *base, g95_expr *new) {          
g95_constructor *q;   
   
  if (base->value.constructor == NULL)    
    base->value.constructor = q = g95_get_constructor();
  else {          
    q = base->value.constructor;          
    while(q->next)        
      q=q->next;          
          
    q->next = g95_get_constructor();         
    q = q->next;      
  }  
  
  q->expr = new;        
        
  if (new->ts.type != base->ts.type || new->ts.kind != base->ts.kind)       
    g95_internal_error("g95_append_constructor(): New node has wrong kind");         
}      
      
      
 
 
/* copy_array_spec()-- Copy an array specification. */ 
 
g95_array_spec *g95_copy_array_spec(g95_array_spec *src) {         
g95_array_spec *dest;   
int a;          
          
  if (src == NULL) return NULL;       
       
  dest = g95_get_array_spec();   
   
  *dest = *src;       
       
  for(a=0; a<dest->rank; a++) { 
    dest->lower[a] = g95_copy_expr(dest->lower[a]);       
    dest->upper[a] = g95_copy_expr(dest->upper[a]);     
  }       
       
  return dest;     
}  
  
  
          
          
/* g95_check_iter_variable()-- Check an EXPR_VARIABLE expression in a
 * constructor to make sure that that variable is an iteration
 * variables. */ 
 
try g95_check_iter_variable(g95_expr *expr) {        
        
g95_symbol *symb;       
cons_stack *t;      
      
  symb = expr->symbol;   
   
  for(t=stack_base; t; t=t->previous)       
    if (symb == t->iterator->var->symbol) return SUCCESS;      
      
  return FAILURE; 
}   
   
   
 
 
/* g95_start_constructor()-- Start an array constructor.  The
 * constructor starts with zero elements and should be appended to by
 * g95_append_constructor(). */         
         
g95_expr *g95_start_constructor(bt type, int kind, locus *where) {          
g95_expr *result;   
   
  result = g95_get_expr();  
  
  result->type = EXPR_ARRAY;         
  result->rank = 1;   
   
  result->ts.type = type;      
  result->ts.kind = kind;         
  result->where = *where;     
     
  return result;   
}    
    
    
     
     
/* constant_element()-- Work function for checking that an element of
 * a constructor is a constant, after removal of any iteration
 * variables.  We return FAILURE if not so. */ 
 
static try constant_element(g95_expr *n) {        
int rv; 
 
  rv = g95_is_constant_expr(n); 
  g95_free_expr(n);         
         
  return rv ? SUCCESS : FAILURE;    
}   
   
   
    
    
/* spec_dimen_size()-- Get the size of single dimension of an array
 * specification.  The array is guaranteed to be one dimensional */   
   
static try spec_dimen_size(g95_array_spec *as, int dimen, mpz_t *result) {         
         
  if (as == NULL || as->type != AS_EXPLICIT ||         
      as->lower[dimen]->type != EXPR_CONSTANT ||         
      as->upper[dimen]->type != EXPR_CONSTANT) {    
    
    return FAILURE;          
  }       
       
  mpz_init(*result);       
       
  mpz_sub(*result, as->upper[dimen]->value.integer,     
	  as->lower[dimen]->value.integer);         
         
  mpz_add_ui(*result, *result, 1);        
        
  return SUCCESS;    
}    
    
    
        
        
/* count_elements()-- Work function that counts the number of elements
 * present in a constructor. */     
     
static try count_elements(g95_expr *u) {       
mpz_t result;    
    
  if (u->rank == 0)        
    mpz_add_ui(*current_expand.count, *current_expand.count, 1);  
  else {    
    if (g95_array_size(u, &result) == FAILURE) {
      g95_free_expr(u);        
      return FAILURE;     
    }          
          
    mpz_add(*current_expand.count, *current_expand.count,     
	    result);     
    mpz_clear(result);         
  }   
   
  g95_free_expr(u);      
  return SUCCESS;          
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
         
static array_type match_array_element_spec(g95_array_spec *as) {    
g95_expr **upper, **lower;          
match u;        
        
  lower = &as->lower[as->rank - 1];  
  upper = &as->upper[as->rank - 1];    
    
  if (g95_match_char('*') == MATCH_YES) {       
    *lower = g95_int_expr(1);       
    return AS_ASSUMED_SIZE;        
  }  
  
  if (g95_match_char(':') == MATCH_YES) return AS_DEFERRED;  
  
  u = g95_match_expr(upper);    
  if (u == MATCH_NO)   
    g95_error("Expected expression in array specification at %C");       
  if (u != MATCH_YES) return AS_UNKNOWN;        
        
  if (g95_match_char(':') == MATCH_NO) {         
    *lower = g95_int_expr(1);         
    return AS_EXPLICIT;  
  }         
         
  *lower = *upper;      
  *upper = NULL;          
          
  if (g95_match_char('*') == MATCH_YES) return AS_ASSUMED_SIZE;          
          
  u = g95_match_expr(upper);       
  if (u == MATCH_ERROR) return AS_UNKNOWN; 
  if (u == MATCH_NO) return AS_ASSUMED_SHAPE;        
        
  return AS_EXPLICIT; 
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
  
  
          
          
/* g95_match_array_constructor()-- Match an array constructor */  
  
match g95_match_array_constructor(g95_expr **result) {        
g95_constructor *head, *tail, *new;          
g95_expr *expr;       
locus where;        
match u;          
          
  if (g95_match(" (/") == MATCH_NO) return MATCH_NO;         
         
  where = *g95_current_locus();        
  head = tail = NULL;

  if (g95_match(" /)") == MATCH_YES) goto empty;   /* Special case */        
        
  for(;;) {        
    u = match_array_cons_element(&new);
    if (u == MATCH_ERROR) goto cleanup;          
    if (u == MATCH_NO) goto syntax; 
 
    if (head == NULL)  
      head = new;   
    else          
      tail->next = new;

    tail = new;       
       
    if (g95_match_char(',') == MATCH_NO) break;  
  }    
    
  if (g95_match(" /)") == MATCH_NO) goto syntax;         
         
empty:         
  expr = g95_get_expr();

  expr->type = EXPR_ARRAY;   
   
  expr->value.constructor = head;     
  /* Size must be calculated at resolution time */      
      
  expr->where = where; 
  expr->rank = 1;    
    
  *result = expr;  
  return MATCH_YES;  
  
syntax:
  g95_error("Syntax error in array constructor at %C");       
       
cleanup:       
  g95_free_constructor(head);   
  return MATCH_ERROR;   
}   
   
   
          
          
/* expand()-- Work function that constructs a new constructor out of
 * the old one, stringing new elements together. */         
         
static try expand(g95_expr *d) {       
       
  if (current_expand.new_head == NULL)        
    current_expand.new_head = current_expand.new_tail = g95_get_constructor();        
  else {    
    current_expand.new_tail->next = g95_get_constructor();        
    current_expand.new_tail = current_expand.new_tail->next;
  }     
     
  current_expand.new_tail->where = d->where;
  current_expand.new_tail->expr = d;  
  
  return SUCCESS;      
}         
         
         
       
       
/* g95_match_array_ref()-- Match an array reference, whether it is the
 * whole array or a particular elements or a section. */        
        
match g95_match_array_ref(g95_array_ref *ar, g95_array_spec *as, int init) {      
match d;          
          
  memset(ar, '\0', sizeof(ar));  
  
  ar->where = *g95_current_locus();   
  ar->as = as;

  if (g95_match_char('(') != MATCH_YES) {          
    ar->type = AR_FULL;        
    ar->dimen = 0;         
    return MATCH_YES; 
  }        
        
  ar->type = AR_UNKNOWN;          
          
  for(ar->dimen=0; ar->dimen<G95_MAX_DIMENSIONS; ar->dimen++) {         
    d = match_subscript(ar, init);     
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
  ar->dimen++;        
        
  return MATCH_YES;         
}    
    
    
        
        
/* g95_check_constructor()-- Checks a constructor to see if it is a
 * particular kind of expression-- specification, restricted,
 * or initialization as determined by the check_function.  */        
        
try g95_check_constructor(g95_expr *expr,          
			  try (*check_function)(g95_expr *)) {       
cons_stack *base_save;      
try q;        
        
  base_save = stack_base;        
  stack_base = NULL;

  q = check_constructor(expr->value.constructor, check_function);    
  stack_base = base_save;

  return q;          
}        
        
        
    
    
/* g95_free_constructor()-- Free chains of g95_constructor structures */    
    
void g95_free_constructor(g95_constructor *h) {   
g95_constructor *next;      
      
  if (h == NULL) return;   
   
  for(; h; h=next) {     
    next = h->next;    
    
    g95_free_expr(h->expr);        
    if (h->iterator != NULL) g95_free_iterator(h->iterator, 1);   
    g95_free(h);        
  } 
}         
         
         
        
        
/* g95_match_array_spec()-- Matches an array specification,
 * incidentally figuring out what sort it is.  */ 
 
match g95_match_array_spec(g95_array_spec **asp) {      
array_type current_type;  
g95_array_spec *as;       
int d;    
    
 if (g95_match_char('(') != MATCH_YES) {       
    *asp = NULL; 
    return MATCH_NO;    
  }      
      
  as = g95_get_array_spec();  
  
  for(d=0; d<G95_MAX_DIMENSIONS; d++) {      
    as->lower[d] = NULL; 
    as->upper[d] = NULL;
  }      
      
  as->rank = 1;

  for(;;) { 
    current_type = match_array_element_spec(as);

    if (as->rank == 1) { 
      if (current_type == AS_UNKNOWN) goto cleanup;        
      as->type = current_type;   
    } else      
      switch(as->type) { /* See how current spec meshes with the existing */       
        case AS_UNKNOWN:         
	  goto cleanup;         
         
        case AS_EXPLICIT:      
	  if (current_type == AS_ASSUMED_SIZE) {          
	    as->type = AS_ASSUMED_SIZE;         
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
	    as->type = AS_ASSUMED_SHAPE;
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
     
    if (as->rank >= G95_MAX_DIMENSIONS) {    
      g95_error("Array specification at %C has more than "         
		stringize(G95_MAX_DIMENSIONS) " dimensions");          
      goto cleanup;      
    }    
    
    as->rank++; 
  }         
         
/* If a lower bounds of an assumed shape array is blank, put in one. */

  if (as->type == AS_ASSUMED_SHAPE) {  
    for(d=0; d<as->rank; d++) {     
      if (as->lower[d] == NULL)  
	as->lower[d] = g95_int_expr(1);  
    }    
  }    
    
  *asp = as;
  return MATCH_YES;          
          
/* Something went wrong */     
     
cleanup:  
  g95_free_array_spec(as);     
  return MATCH_ERROR;         
}


  
  
/* check_duplicate_iterator()-- Given an expression node that might be an 
 * array constructor and a symbol, make sure that no iterators in this or
 * child constructors use the symbol as an implied-DO iterator. 
 * Returns nonzero if a duplicate was found. */         
         
static int check_duplicate_iterator(g95_constructor *k, g95_symbol *master) {         
g95_expr *h;          
          
  for(; k; k=k->next) {  
    h = k->expr;   
   
    if (h->type == EXPR_ARRAY &&
	check_duplicate_iterator(h->value.constructor, master)) return 1;  
  
    if (k->iterator == NULL) continue;    
    
    if (k->iterator->var->symbol == master) {
      g95_error("DO-iterator '%s' at %L is inside iterator of the same name",  
		master->name, &k->where);   
   
      return 1;          
    }     
  }   
   
  return 0;    
}  
  
  


/* match_array_list()-- Match a list of array elements. */  
  
static match match_array_list(g95_constructor **result) {       
g95_constructor *q, *head, *tail, *new;          
g95_iterator iter; 
locus old_loc;   
g95_expr *e;    
match j;        
int n; 
 
  old_loc = *g95_current_locus();       
       
  if (g95_match_char('(') == MATCH_NO) return MATCH_NO; 
 
  memset(&iter, '\0', sizeof(g95_iterator));    
  head = NULL;       
       
  j = match_array_cons_element(&head);   
  if (j != MATCH_YES) goto cleanup;      
      
  tail = head;      
      
  if (g95_match_char(',') != MATCH_YES) {  
    j = MATCH_NO;   
    goto cleanup;  
  } 
 
  for(n=1;; n++) {  
    j = g95_match_iterator(&iter, 0);         
    if (j == MATCH_YES) break;         
    if (j == MATCH_ERROR) goto cleanup;    
    
    j = match_array_cons_element(&new);  
    if (j == MATCH_ERROR) goto cleanup;      
    if (j == MATCH_NO) {       
      if (n > 2) goto syntax;     
      j = MATCH_NO;    
      goto cleanup;    /* Could be a complex constant */ 
    } 
 
    tail->next = new;  
    tail = new;  
  
    if (g95_match_char(',') != MATCH_YES) {    
      if (n > 2) goto syntax;  
      j = MATCH_NO;      
      goto cleanup;         
    }        
  }        
        
  if (g95_match_char(')') != MATCH_YES) goto syntax;  
  
  if (check_duplicate_iterator(head, iter.var->symbol)) {   
    j = MATCH_ERROR;    
    goto cleanup;  
  }

  e = g95_get_expr(); 
  e->type = EXPR_ARRAY;  
  e->where = old_loc;          
  e->value.constructor = head;         
         
  q = g95_get_constructor();     
  q->where = *g95_current_locus();    
  q->iterator = g95_get_iterator();    
  *q->iterator = iter;    
    
  q->expr = e;          
  *result = q;        
        
  return MATCH_YES;          
          
syntax:
  g95_error("Syntax error in array constructor at %C");         
  j = MATCH_ERROR;  
  
cleanup:
  g95_free_constructor(head);      
  g95_free_iterator(&iter, 0);
  g95_set_locus(&old_loc);       
  return j;   
}    
    
    
         
         
/* g95_set_array_spec()-- Given a symbol and an array specification,
 * modify the symbol to have that array specification.  The error
 * locus is needed in case something goes wrong.  On failure, the
 * caller must free the spec. */     
     
try g95_set_array_spec(g95_symbol *symbol, g95_array_spec *as, locus *error_loc) {

  if (as == NULL) return SUCCESS;     
     
  if (g95_add_dimension(&symbol->attr, error_loc) == FAILURE) return FAILURE;     
     
  symbol->as = as;

  return SUCCESS;         
}     
     
     
 
 
/* check_constructor()-- Recursive work function for
 * g95_check_constructor().  This amounts to calling the check
 * function for each expression in the constructor, giving variables
 * with the names of iterators a pass.  */ 
 
static try check_constructor(g95_constructor *i,        
			     try (*check_function)(g95_expr *)) {         
cons_stack element;          
g95_expr *r;     
try t;  
  
  for(; i; i=i->next) {     
    r = i->expr;   
   
    if (r->type != EXPR_ARRAY) {         
      if ((*check_function)(r) == FAILURE) return FAILURE;          
      continue;          
    }     
     
    element.previous = stack_base; 
    element.iterator = i->iterator;         
         
    stack_base = &element;       
    t = check_constructor(r->value.constructor, check_function);     
    stack_base = element.previous;    
    
    if (t == FAILURE) return FAILURE;     
  }     
     
/* Nothing went wrong, so all OK */    
    
  return SUCCESS;      
}        
        
        
  
  
/* g95_find_array_ref()-- Given an array expression, find the array
 * reference structure that characterizes the reference. */       
       
g95_array_ref *g95_find_array_ref(g95_expr *x) {   
g95_ref *r;  
  
  for(r=x->ref; r; r=r->next)  
    if (r->type == REF_ARRAY &&        
	(r->u.ar.type == AR_FULL || r->u.ar.type == AR_SECTION)) break;      
      
  if (r == NULL) g95_internal_error("g95_find_array_ref(): No ref found"); 
 
  return &r->u.ar;   
}  
  


/* g95_expanded_ac()-- Returns nonzero if an array constructor has
 * been completely expanded (no iterators) and zero if iterators are
 * present. */     
     
int g95_expanded_ac(g95_expr *e) {        
g95_constructor *w;    
    
  if (e->type == EXPR_ARRAY)     
    for(w=e->value.constructor; w; w=w->next)   
      if (w->iterator != NULL || !g95_expanded_ac(w->expr)) return 0;     
     
  return 1;    
}       
       
       


/* resolve_array_bound()-- Takes an array bound, resolves the expression,
 * that make up the shape and check associated constraints. */   
   
static try resolve_array_bound(g95_expr *c, int check_constant) {          
          
  if (c == NULL) return SUCCESS;         
         
  if (g95_resolve_expr(c) == FAILURE ||        
      g95_specification_expr(c) == FAILURE) return FAILURE;    
    
  if (check_constant && g95_is_constant_expr(c) == 0) {     
    g95_error("Variable '%s' at %L in this context must be constant",    
	      c->symbol->name, &c->where);    
    return FAILURE;          
  }       
       
  return SUCCESS;     
}       
       
       
      
      
/* check_element_type()-- Given an expression, compare it's type with
 * the type of the current constructor.  Returns nonzero if an error
 * was issued.  The cons_state variable keeps track of whether the
 * type of the constructor being read or resolved is known to be good,
 * bad or just starting out. */

static g95_typespec constructor_ts; 
static enum { CONS_START, CONS_GOOD, CONS_BAD } cons_state;   
   
static int check_element_type(g95_expr *expr) {  
  
  if (cons_state == CONS_BAD) return 0;  /* Supress further errors */    
    
  if (cons_state == CONS_START) {  
    if (expr->ts.type == BT_UNKNOWN)        
      cons_state = CONS_BAD;
    else {     
      cons_state = CONS_GOOD;     
      constructor_ts = expr->ts;         
    }    
    
    return 0;    
  }    
    
  if (g95_compare_types(&constructor_ts, &expr->ts)) return 0;      
      
  g95_error("Element in %s array constructor at %L is %s",        
	    g95_typename(&constructor_ts), &expr->where,    
	    g95_typename(&expr->ts));     
     
  cons_state = CONS_BAD; 
  return 1;   
}     
     
     
     
     
/* expand_expr()-- Expand an expression with that is inside of a
 * constructor, recursing into other constructors if present. */  
  
static try expand_expr(g95_expr *p) {     
     
  if (p->type == EXPR_ARRAY)        
    return expand_constructor(p->value.constructor);          
          
  p = g95_copy_expr(p);    
    
  if (g95_simplify_expr(p, 1) == FAILURE) {      
    g95_free_expr(p);        
    return FAILURE; 
  }          
          
  return current_expand.expand_work_function(p);        
}        
        
        
 
 
/* extract_element()-- Work function that extracts a particular
 * element from an array constructor, freeing the rest. */  
  
static try extract_element(g95_expr *t) {   
   
  if (t->rank != 0) {  /* Something unextractable */ 
    g95_free_expr(t);  
    return FAILURE;       
  }       
       
  if (current_expand.extract_count == current_expand.extract_n)
    current_expand.extracted = t; 
  else      
    g95_free_expr(t);    
    
  current_expand.extract_count++;   
  return SUCCESS;       
}          
          
          
        
        
/* resolve_array_list()-- Recursive array list resolution function.
 * All of the elements must be of the same type. */       
       
static try resolve_array_list(g95_constructor *j) {      
try u;        
        
  u = SUCCESS;      
      
  for(;j ;j=j->next) {  
    if (j->iterator != NULL && g95_resolve_iterator(j->iterator) == FAILURE) 
      u = FAILURE;     
     
    if (g95_resolve_expr(j->expr) == FAILURE) u = FAILURE;     
  }   
   
  return u;          
}     
     
     
     
     
/* g95_resolve_array_spec()-- Takes an array specification, resolves
 * the expressions that make up the shape and make sure everything is
 * integral. */      
      
try g95_resolve_array_spec(g95_array_spec *as, int check_constant) {          
g95_expr *f;
int p;       
       
  if (as == NULL) return SUCCESS;          
          
  for(p=0; p<as->rank; p++) {  
    f = as->lower[p]; 
    if (resolve_array_bound(f, check_constant) == FAILURE) return FAILURE;         
         
    f = as->upper[p];          
    if (resolve_array_bound(f, check_constant) == FAILURE) return FAILURE;     
  }

  return SUCCESS;         
}  
  
  
   
   
/* ref_dimen_size()-- Get the number of elements in an array section */         
         
static try ref_dimen_size(g95_array_ref *ar, int dimen, mpz_t *result) {   
mpz_t upper, lower, stride;    
try z;         
         
  switch(ar->dimen_type[dimen]) {
  case DIMEN_ELEMENT:  
    mpz_init(*result);         
    mpz_set_ui(*result, 1);     
    z = SUCCESS;          
    break;      
      
  case DIMEN_VECTOR:  
    z = g95_array_size(ar->start[dimen], result);    /* Recurse! */      
    break;         
         
  case DIMEN_RANGE:  
    mpz_init(upper);    
    mpz_init(lower);  
    mpz_init(stride);
    z = FAILURE;      
      
    if (ar->start[dimen] == NULL) {     
      if (ar->as->lower[dimen] == NULL ||  
	  ar->as->lower[dimen]->type != EXPR_CONSTANT) goto cleanup;        
      mpz_set(lower, ar->as->lower[dimen]->value.integer);
    } else { 
      if (ar->start[dimen]->type != EXPR_CONSTANT) goto cleanup;        
      mpz_set(lower, ar->start[dimen]->value.integer);      
    }  
  
    if (ar->end[dimen] == NULL) {        
      if (ar->as->upper[dimen] == NULL ||   
	  ar->as->upper[dimen]->type != EXPR_CONSTANT) goto cleanup;    
      mpz_set(upper, ar->as->upper[dimen]->value.integer);    
    } else {   
      if (ar->end[dimen]->type != EXPR_CONSTANT) goto cleanup;          
      mpz_set(upper, ar->end[dimen]->value.integer);          
    } 
 
    if (ar->stride[dimen] == NULL)    
      mpz_set_ui(stride, 1);
    else {     
      if (ar->stride[dimen]->type != EXPR_CONSTANT) goto cleanup;      
      mpz_set(stride, ar->stride[dimen]->value.integer);   
    }         
         
    mpz_init(*result);   
    mpz_sub(*result, upper, lower);          
    mpz_add(*result, *result, stride);     
    mpz_div(*result, *result, stride);     
     
    /* Zero stride caught earlier */         
         
    if (mpz_cmp_ui(*result, 0) < 0) mpz_set_ui(*result, 0);         
    z = SUCCESS;         
         
  cleanup:         
    mpz_clear(upper);       
    mpz_clear(lower);          
    mpz_clear(stride);   
    return z;          
          
  default:         
    g95_internal_error("ref_dimen_size(): Bad dimen type");  
  } 
 
  return z;          
}       
       
       
 
 
/* match_array_cons_element()-- match a single element of an array
 * constructor, which can be a single expression or a list of
 * elements. */

static match match_array_cons_element(g95_constructor **result) {         
g95_constructor *z;
g95_expr *expr;      
match n;          
          
  n = match_array_list(result);
  if (n != MATCH_NO) return n;

  n = g95_match_expr(&expr);       
  if (n != MATCH_YES) return n;       
       
  z = g95_get_constructor(); 
  z->where = *g95_current_locus();  
  z->expr = expr;         
         
  *result = z;          
  return MATCH_YES;      
}    
    
    
   
   
/* g95_array_spec_size()-- Given an array specification, figure out
 * how big it is. */     
     
try g95_array_spec_size(g95_array_spec *as, mpz_t *result) {
mpz_t size;     
int w;    
    
  mpz_init_set_ui(*result, 1);       
       
  for(w=0; w<as->rank; w++) {          
    if (spec_dimen_size(as, w, &size) == FAILURE) {        
      mpz_clear(*result);   
      return FAILURE;     
    }          
          
    mpz_mul(*result, *result, size);      
    mpz_clear(size);      
  }  
  
  return SUCCESS;        
}     
     
     


/* g95_array_dimen_size()-- Given an array expression and a dimension,
 * figure out how many elements it has along that dimension.  Returns
 * SUCCESS if we were able to return a result in the 'result'
 * variable, FAILURE otherwise. */       
       
try g95_array_dimen_size(g95_expr *array, int dimen, mpz_t *result) {          
g95_ref *r;    
int i;  
  
  if (dimen > array->rank - 1)         
    g95_internal_error("g95_array_dimen_size(): Bad dimension");         
         
  switch(array->type) {       
  case EXPR_VARIABLE:          
  case EXPR_FUNCTION:
    for(r=array->ref; r; r=r->next) {  
      if (r->type != REF_ARRAY) continue;    
    
      if (r->u.ar.type == AR_FULL)        
	return spec_dimen_size(r->u.ar.as, dimen, result);     
     
      if (r->u.ar.type == AR_SECTION) {      
	for(i=0; dimen>=0; i++)          
	  if (r->u.ar.dimen_type[i] != DIMEN_ELEMENT) dimen--;        
        
	return ref_dimen_size(&r->u.ar, i-1, result);      
      } 
    }   
   
    if (spec_dimen_size(array->symbol->as, dimen, result) == FAILURE)    
      return FAILURE;        
        
    break;  
  
  default:      
    if (array->shape == NULL) return FAILURE;      
      
    mpz_init_set(*result, array->shape[dimen]);       
       
    break;       
  }      
      
  return SUCCESS;          
}


      
      
/* g95_simplify_iteration_var()-- Given an initialization expression
 * that is a variable reference, substitute the current value of the
 * iteration variable. */

try g95_simplify_iterator_var(g95_expr *y) {   
iterator_stack *l;    
    
  for(l=iter_stack; l; l=l->prev)          
    if (y->symbol == l->variable) break;

  if (l == NULL) return FAILURE;   /* Variable not found */        
        
  g95_replace_expr(y, g95_int_expr(0));

  mpz_set(y->value.integer, l->value);      
      
  return SUCCESS;         
}         
         
         
      
      
/* copy_iterator()-- Copy an iterator structure */   
   
static g95_iterator *copy_iterator(g95_iterator *src) {    
g95_iterator *dest;         
         
  if (src == NULL) return NULL;      
      
  dest = g95_get_iterator();   
     
  dest->var = g95_copy_expr(src->var);        
  dest->start = g95_copy_expr(src->start);  
  dest->end = g95_copy_expr(src->end);        
  dest->step = g95_copy_expr(src->step);       
       
  return dest;      
} 
 
 
    
    
/* g95_array_ref_shape()-- Given an array reference, return the shape
 * of the reference in an array of mpz_t integers. */        
        
try g95_array_ref_shape(g95_array_ref *ar, mpz_t *shape) {       
int p;  
  
  p = 0;

  switch(ar->type) {    
  case AR_FULL:     
    for(; p<ar->as->rank; p++)        
      if (spec_dimen_size(ar->as, p, &shape[p]) == FAILURE) goto cleanup;       
       
    return SUCCESS;        
        
  case AR_SECTION:          
    for(; p<ar->dimen; p++)  
      if (ref_dimen_size(ar, p, &shape[p]) == FAILURE) goto cleanup;         
         
    return SUCCESS;  
  
  default:          
    break;      
  }   
   
 cleanup: 
  for(p--; p>=0; p--)  
    mpz_clear(shape[p]);       
       
  return FAILURE; 
}


 
 
/* g95_constant_ac()-- Given an array constructor, determine if the
 * constructor is constant or not by expanding it and making sure that
 * all elements are constants.  This is a bit of a hack since
 * something like (/ (i, i=1,100000000) /) will take a while as
 * opposed to a more clever function that traverses the expression
 * tree. */      
      
int g95_constant_ac(g95_expr *t) {  
expand_info expand_save;          
try rc;          
          
  iter_stack = NULL;  
  expand_save = current_expand;   
  current_expand.expand_work_function = constant_element;

  rc = expand_constructor(t->value.constructor); 
   
  current_expand = expand_save;    
  if ( rc == FAILURE) return 0;      
      
  return 1;  
}


    
    
/* g95_expand_constructor()-- Top level subroutine for expanding
 * constructors.  We only expand constructor if they are small
 * enough. */  
  
try g95_expand_constructor(g95_expr *a) {       
expand_info expand_save;    
g95_expr *h;     
try rc;

  h = g95_get_array_element(a, G95_MAX_AC_EXPAND);
  if (h != NULL) {    
    g95_free_expr(h);
    return SUCCESS;        
  } 
 
  expand_save = current_expand;   
  current_expand.new_head = current_expand.new_tail = NULL;     
     
  iter_stack = NULL;  
  
  current_expand.expand_work_function = expand;       
       
  if (expand_constructor(a->value.constructor) == FAILURE) {       
    g95_free_constructor(current_expand.new_head);  
    rc = FAILURE;     
    goto done;
  }  
  
  g95_free_constructor(a->value.constructor);   
  a->value.constructor = current_expand.new_head; 
 
  rc = SUCCESS;         
         
done:    
  current_expand = expand_save; 
 
  return rc;   
}    
    
    
          
          
/* g95_copy_constructor()-- Copy a constructor structure. */ 
 
g95_constructor *g95_copy_constructor(g95_constructor *src) {      
g95_constructor *dest; 
 
  if (src == NULL) return NULL;   
   
  dest = g95_get_constructor();
  dest->where = src->where;
  dest->expr = g95_copy_expr(src->expr);      
  dest->iterator = copy_iterator(src->iterator); 
 
  dest->next = g95_copy_constructor(src->next);    
    
  return dest;      
}        
        
        


/* check_constructor_type()-- Recursive work function for
 * g95_check_constructor_type(). */   
   
static try check_constructor_type(g95_constructor *y) {       
g95_expr *h;   
   
  for(; y; y=y->next) {          
    h = y->expr;  
  
    if (h->type == EXPR_ARRAY) {    
      if (check_constructor_type(h->value.constructor) == FAILURE)        
	return FAILURE;    
    
      continue;  
    }      
      
    if (check_element_type(h)) return FAILURE;        
  }    
    
  return SUCCESS;      
}        
        
        
 
 
/* expand_constructor()-- Expand a constructor into constant
 * constructors without any iterators, calling the work function for
 * each of the expanded expressions.  The work function needs to
 * either save or free the passed expression. */     
     
static try expand_constructor(g95_constructor *a) { 
g95_expr *m;       
       
  for(; a; a=a->next) {    
    if (a->iterator != NULL) {   
      if (g95_expand_iterator(a->iterator, (void *) expand_expr,       
			      a->expr) == FAILURE)      
	return FAILURE;

      continue;     
    }     
     
    m = a->expr;      
      
    if (m->type == EXPR_ARRAY) {          
      if (expand_constructor(m->value.constructor) == FAILURE) 
	return FAILURE; 
 
      continue;       
    }        
        
    m = g95_copy_expr(m);         
    if (g95_simplify_expr(m, 1) == FAILURE) {   
      g95_free_expr(m);         
      return FAILURE;   
    }         
         
    if (current_expand.expand_work_function(m) == FAILURE) return FAILURE;      
  }         
         
  return SUCCESS;         
}          
          
          
  
  
/* g95_get_array_element()-- Given an array expression and an element
 * number (starting at zero), return a pointer to the array element.
 * NULL is returned if the size of the array has been exceeded.  The
 * expression node returned remains a part of the array and should not
 * be freed.  Access is not efficient at all, but this is another
 * place where things do not have to be particularly fast. */        
        
g95_expr *g95_get_array_element(g95_expr *array, int element) {   
expand_info expand_save;       
g95_expr *u;  
try rc;       
       
  expand_save = current_expand;     
  current_expand.extract_n = element;     
  current_expand.expand_work_function = extract_element;    
  current_expand.extracted = NULL;  
  current_expand.extract_count = 0;  
  
  iter_stack = NULL;     
     
  rc = expand_constructor(array->value.constructor);          
  u = current_expand.extracted;          
  current_expand = expand_save;      
     
  if (rc == FAILURE)       
    return NULL;

  return u;  
}          
          
          
    
    
/* g95_compare_array_spec()-- Compares two array specifications.  */        
        
int g95_compare_array_spec(g95_array_spec *as1, g95_array_spec *as2) {         
int o, q, a;        
        
  if (as1 == NULL && as2 == NULL) return 1;   
   
  if (as1 == NULL || as2 == NULL) return 0;  
  
  if (as1->rank != as2->rank) return 0;     
     
  if (as1->rank == 0) return 1;      
      
  if (as1->type != as2->type) return 0;     
     
  if (as1->type == AS_EXPLICIT)    
    for(o=0; o<as1->rank; o++) {      
      if (g95_extract_int(as1->lower[o], &q) != NULL) goto error;      
      if (g95_extract_int(as2->lower[o], &a) != NULL) goto error;
      if (q != a) return 0;

      if (g95_extract_int(as1->upper[o], &q) != NULL) goto error;
      if (g95_extract_int(as2->upper[o], &a) != NULL) goto error;         
      if (q != a) return 0;       
    }          
          
  return 1;         
         
error:         
  g95_internal_error("g95_compare_array_spec(): Array spec clobbered");     
  return 0;        /* Keep the compiler happy */   
}      
      
      
 
 
/* g95_resolve_array_constructor()-- Resolve all of the expressions in
 * an array list.  TODO: String lengths. */       
       
try g95_resolve_array_constructor(g95_expr *expr) {   
try f;         
         
  f = resolve_array_list(expr->value.constructor); 
  if (f == SUCCESS) f = g95_check_constructor_type(expr);

  return f;          
}          
          
          
     
     
static try ref_size(g95_array_ref *ar, mpz_t *result) {         
mpz_t size;          
int w;         
         
  mpz_init_set_ui(*result, 1);        
        
  for(w=0; w<ar->dimen; w++) {         
    if (ref_dimen_size(ar, w, &size) == FAILURE) {   
      mpz_clear(*result);  
      return FAILURE;     
    }         
         
    mpz_mul(*result, *result, size);   
    mpz_clear(size);
  }

  return SUCCESS;
}     
     
     
         
         
/* g95_array_size()-- Given an array expression, figure out how many
 * elements are in the array.  Returns SUCCESS if this is possible,
 * and sets the 'result' variable.  Otherwise returns FAILURE. */

try g95_array_size(g95_expr *array, mpz_t *result) {      
expand_info expand_save;      
g95_ref *re;       
int x, flag;  
try o;     
     
  switch(array->type) {
  case EXPR_ARRAY:     
    flag = g95_suppress_error;       
    g95_suppress_error = 1;  
  
    expand_save = current_expand;    
    
    current_expand.count = result;
    mpz_init_set_ui(*result, 0);    
    
    current_expand.expand_work_function = count_elements;       
    iter_stack = NULL;          
          
    o = expand_constructor(array->value.constructor);  
    g95_suppress_error = flag;          
          
    if (o == FAILURE) mpz_clear(*result);       
    current_expand = expand_save;     
    return o;       
       
  case EXPR_VARIABLE:
    for(re=array->ref; re; re=re->next) {       
      if (re->type != REF_ARRAY) continue;    
    
      if (re->u.ar.type == AR_FULL)        
	return g95_array_spec_size(re->u.ar.as, result);      
      
      if (re->u.ar.type == AR_SECTION)	return ref_size(&re->u.ar, result);
    }         
         
    return g95_array_spec_size(array->symbol->as, result);       
       
  default:         
    if (array->rank == 0 || array->shape == NULL) return FAILURE;         
         
    mpz_init_set_ui(*result, 1);         
         
    for(x=0; x<array->rank; x++)      
      mpz_mul(*result, *result, array->shape[x]);          
          
    break;        
  }         
         
  return SUCCESS;          
}   
   
   
    
    
/* g95_check_constructor_type()-- Check that all elements of an array
 * constructor are the same type.  On FAILURE, an error has been
 * generated. */  
  
try g95_check_constructor_type(g95_expr *w) {    
try j;   
   
  cons_state = CONS_START;
  g95_clear_ts(&constructor_ts);    
    
  j = check_constructor_type(w->value.constructor);       
  if (j == SUCCESS && w->ts.type == BT_UNKNOWN) w->ts = constructor_ts;        
        
  return j;  
}


 
 
/* g95_expand_iterator()-- Given an iterator, a function to call and
 * something to expand, we effectively assign the correct values to
 * the loop variable and call the expansion function with the thing to
 * expand.  If the expansion function fails, we terminate the loop. */         
         
try g95_expand_iterator(g95_iterator *iterator, try (*expand)(void *),
			void *y) {         
g95_expr *start, *end, *step;         
iterator_stack frame;  
mpz_t trip;     
try k;         
         
  start = end = step = NULL;      
      
  k = FAILURE;    
    
  mpz_init(trip);   
  mpz_init(frame.value);         
         
  start = g95_copy_expr(iterator->start);          
  if (g95_simplify_expr(start, 1) == FAILURE) goto cleanup;    
    
  if (start->type != EXPR_CONSTANT || start->ts.type != BT_INTEGER) 
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
      
  mpz_sub(trip, end->value.integer, start->value.integer);         
  mpz_add(trip, trip, step->value.integer);     
  mpz_tdiv_q(trip, trip, step->value.integer);        
        
  mpz_set(frame.value, start->value.integer);    
    
  frame.prev = iter_stack;       
  frame.variable = iterator->var->symbol; 
  iter_stack = &frame; 
 
  while(mpz_sgn(trip) > 0) {
    if (expand(y) == FAILURE) goto cleanup;        
        
    mpz_add(frame.value, frame.value, step->value.integer);     
    mpz_sub_ui(trip, trip, 1);          
  }      
      
  k = SUCCESS;   
   
cleanup:       
  g95_free_expr(start);         
  g95_free_expr(end);          
  g95_free_expr(step); 
 
  mpz_clear(trip);       
  mpz_clear(frame.value);        
        
  iter_stack = frame.prev;     
     
  return k;       
}          
          
          
