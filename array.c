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
      
       
       
/* array_function_size()-- Deduce the size of an array-value intrinsic
 * for a couple common inquiry functions. */     
     
static try array_function_size(g95_expr *e, mpz_t *rslt) {      
g95_intrinsic_sym *is;  
  
  is = e->value.function.isym;      
  if (is == NULL) return FAILURE; 
 
  /* TODO: follow elemental functions into their arguments */         
         
  switch(is->generic_id) {   
  case G95_ISYM_MINLOC: 
  case G95_ISYM_MAXLOC:     
  case G95_ISYM_SHAPE: 
  case G95_ISYM_LBOUND:       
  case G95_ISYM_UBOUND:   
    /* Special cases where the size of the array is equal to the rank
     * of the first argument.  The second argument (DIM) must not be
     * present. */ 
 
    if (e->value.function.actual->next != NULL) break;         
         
    mpz_init_set_ui(*rslt, e->value.function.actual->u.expr->rank);  
    return SUCCESS;    
  }          
          
  return FAILURE;     
}  
  
  
     
     
/* match_subscript()-- Match a single dimension of an array reference.
 * This can be a single element or an array section.  Any modifications
 * we've made to the ar structure are cleaned up by the caller.  */   
   
static match match_subscript(g95_array_ref *a, int initial) {         
match d;          
int u;    
    
  u = a->dimen;    
    
  a->c_where[u] = g95_current_locus;       
  a->start[u] = a->end[u] = a->stride[u] = NULL;  
  
  /* We can't be sure of the difference between DIMEN_ELEMENT and
   * DIMEN_VECTOR until we know the type of the element itself at
   * resolution time. */   
   
  a->dimen_type[u] = DIMEN_UNKNOWN;      
      
  if (g95_match_char(':') == MATCH_YES) goto end_element;     
     
  /* Get start element */  
  
  if (initial)         
    d = g95_match_init_expr(&a->start[u]);         
  else    
    d = g95_match_expr(&a->start[u]);      
      
  if (d == MATCH_NO) g95_error("Expected array subscript at %C");    
  if (d != MATCH_YES) return MATCH_ERROR;

  if (g95_match_char(':') == MATCH_NO) return MATCH_YES;         
         
/* Get an optional end element.  Because we've seen the colon, we
 * definitely have a range along this dimension. */    
    
end_element:      
  a->dimen_type[u] = DIMEN_RANGE;   
   
  if (initial)        
    d = g95_match_init_expr(&a->end[u]);    
  else    
    d = g95_match_expr(&a->end[u]);        
        
  if (d == MATCH_ERROR) return MATCH_ERROR;

/* See if we have an optional stride */

  if (g95_match_char(':') == MATCH_YES) {       
    d = initial ? g95_match_init_expr(&a->stride[u])         
      : g95_match_expr(&a->stride[u]);      
      
    if (d == MATCH_NO) g95_error("Expected array subscript stride at %C");   
    if (d != MATCH_YES) return MATCH_ERROR;  
  }       
       
  return MATCH_YES;        
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
   
static array_type match_array_element_spec(g95_array_spec *a) {    
g95_expr **upper, **low;       
match c;          
          
  low = &a->lower[a->rank - 1];          
  upper = &a->upper[a->rank - 1];          
          
  if (g95_match_char('*') == MATCH_YES) {      
    *low = g95_int_expr(1);  
    return AS_ASSUMED_SIZE;
  }      
      
  if (g95_match_char(':') == MATCH_YES) return AS_DEFERRED;    
    
  c = g95_match_expr(upper);   
  if (c == MATCH_NO)    
    g95_error("Expected expression in array specification at %C");          
  if (c != MATCH_YES) return AS_UNKNOWN; 
 
  if (g95_match_char(':') == MATCH_NO) {     
    *low = g95_int_expr(1);          
    return AS_EXPLICIT;       
  }        
        
  *low = *upper;     
  *upper = NULL;   
   
  if (g95_match_char('*') == MATCH_YES) return AS_ASSUMED_SIZE;   
   
  c = g95_match_expr(upper);  
  if (c == MATCH_ERROR) return AS_UNKNOWN;  
  if (c == MATCH_NO) return AS_ASSUMED_SHAPE;

  return AS_EXPLICIT;      
}        
        
        


/* g95_show_array_ref()-- Show an array reference */     
     
void g95_show_array_ref(g95_array_ref *ar) {  
int i;    
    
  g95_status_char('(');    
   
  switch(ar->type) {         
  case AR_FULL:      
    g95_status("FULL");    
    break;       
       
  case AR_SECTION:         
    for(i=0; i<ar->dimen; i++) {  
      if (ar->start[i] != NULL)      
	g95_show_expr(ar->start[i]);        
        
      g95_status_char(':');   
   
      if (ar->end[i] != NULL)
	g95_show_expr(ar->end[i]); 
 
      if (ar->stride[i] != NULL) {         
	g95_status_char(':');          
	g95_show_expr(ar->stride[i]); 
      }       
       
      if (i != ar->dimen-1) g95_status(" , ");      
    }    
    break;        
        
  case AR_ELEMENT:      
    for(i=0; i<ar->dimen; i++) {         
      g95_show_expr(ar->start[i]); 
      if (i != ar->dimen - 1) g95_status(" , ");  
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
  
void g95_free_array_ref(g95_array_ref *as) {   
int e;          
          
  for(e=0; e<G95_MAX_DIMENSIONS; e++) {       
    g95_free_expr(as->start[e]);   
    g95_free_expr(as->end[e]);    
    g95_free_expr(as->stride[e]);         
  } 
 
  g95_free(as);       
}   
   
   
   
   
/* g95_copy_array_ref()-- Copy an array reference structure */        
        
g95_array_ref *g95_copy_array_ref(g95_array_ref *s1) {        
g95_array_ref *d;          
int c;     
     
  if (s1 == NULL) return NULL;     
     
  d = g95_get_array_ref();      
  *d = *s1;      
      
  for(c=0; c<G95_MAX_DIMENSIONS; c++) {       
    d->start[c] = g95_copy_expr(s1->start[c]);     
    d->end[c] = g95_copy_expr(s1->end[c]);  
    d->stride[c] = g95_copy_expr(s1->stride[c]); 
  }

  return d; 
}       
       
       
     
     
/* g95_match_array_constructor()-- Match an array constructor */   
   
match g95_match_array_constructor(g95_expr **r) {        
g95_constructor *h, *end, *n;  
g95_locus where;    
g95_expr *e2;       
match v;          
          
  if (g95_match(" (/") == MATCH_NO &&  
      g95_match_char('[') == MATCH_NO) return MATCH_NO;     
     
  where = g95_current_locus;     
  h = end = NULL;    
    
  if (g95_match(" /)") == MATCH_YES ||        
      g95_match_char(']') == MATCH_YES) goto empty;   /* Special case */

  for(;;) {         
    v = match_array_cons_element(&n);       
    if (v == MATCH_ERROR) goto cleanup;     
    if (v == MATCH_NO) goto syntax;    
    
    if (h == NULL)         
      h = n;        
    else  
      end->next = n; 
 
    end = n;       
       
    if (g95_match_char(',') == MATCH_NO) break;  
  }    
    
  if (g95_match(" /)") == MATCH_NO &&   
      g95_match_char(']') == MATCH_NO) goto syntax;      
      
empty:         
  e2 = g95_get_expr();    
    
  e2->type = EXPR_ARRAY;
  e2->value.constructor = h;         
  /* Size must be calculated at resolution time */          
          
  e2->where = where; 
  e2->rank = 1;  
  
  *r = e2; 
  return MATCH_YES;       
       
syntax:       
  g95_error("Syntax error in array constructor at %C");     
     
cleanup:       
  g95_free_constructor(h);      
  return MATCH_ERROR;  
}     
     
     
          
          
/* g95_simplify_iteration_var()-- Given an initialization expression
 * that is a variable reference, substitute the current value of the
 * iteration variable. */ 
 
try g95_simplify_iterator_var(g95_expr *c) {     
iterator_stack *j;        
        
  for(j=iter_stack; j; j=j->prev)       
    if (c->symbol == j->variable) break;       
       
  if (j == NULL) return FAILURE;   /* Variable not found */    
    
  g95_replace_expr(c, g95_int_expr(0));      
      
  mpz_set(c->value.integer, j->value);  
  
  return SUCCESS;
}   
   
   
        
        
/* g95_expanded_ac()-- Returns nonzero if an array constructor has
 * been completely expanded (no iterators) and zero if iterators are
 * present. */        
        
int g95_expanded_ac(g95_expr *f) {     
g95_constructor *u;         
         
  if (f->type == EXPR_ARRAY)        
    for(u=f->value.constructor; u; u=u->next)        
      if (u->iterator != NULL || !g95_expanded_ac(u->expr)) return 0;       
       
  return 1; 
}       
       
       
        
        
#ifndef IN_GCC
try g95_expand_ac_element(g95_expr *n) {    
  return SUCCESS;      
}     
#endif
        
        
    
    
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
       
       
      
      
/* g95_match_array_ref()-- Match an array reference, whether it is the
 * whole array or a particular elements or a section. */   
   
match g95_match_array_ref(g95_array_ref *spec, int init) {   
match i;    
    
  memset(spec, '\0', sizeof(spec));       
       
  if (g95_match_char('(') != MATCH_YES) {          
    spec->type = AR_FULL;      
    spec->dimen = 0;          
    return MATCH_YES;          
  }      
      
  spec->type = AR_UNKNOWN;  
  
  for(spec->dimen=0; spec->dimen<G95_MAX_DIMENSIONS; spec->dimen++) {   
    i = match_subscript(spec, init); 
    if (i == MATCH_ERROR) goto error;        
        
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
  
  
        
        
/* g95_free_array_spec()-- Free all of the expressions associated with
 * array bounds specifications */   
   
void g95_free_array_spec(g95_array_spec *as) {  
int u;       
       
  if (as == NULL) return; 

  for(u=0; u<as->rank; u++) {  
    g95_free_expr(as->lower[u]);   
    g95_free_expr(as->upper[u]); 
  }       
       
  g95_free(as);       
}        
        
        
          
          
/* copy_iterator()-- Copy an iterator structure */     
     
static g95_iterator *copy_iterator(g95_iterator *source) {        
g95_iterator *d1;   
   
  if (source == NULL) return NULL; 
 
  d1 = g95_get_iterator();     
       
  d1->var = g95_copy_expr(source->var);       
  d1->start = g95_copy_expr(source->start);   
  d1->end = g95_copy_expr(source->end);         
  d1->step = g95_copy_expr(source->step);        
        
  return d1;   
}        
        
        
         
         
/* resolve_array_bound()-- Takes an array bound, resolves the expression,
 * that make up the shape and check associated constraints. */         
         
static try resolve_array_bound(g95_expr *t, int check_constant) {         
         
  if (t == NULL) return SUCCESS;        
        
  if (g95_resolve_expr(t) == FAILURE || 
      g95_specification_expr(t) == FAILURE ||     
      g95_simplify_expr(t, 0) == FAILURE) return FAILURE;

  if (check_constant && g95_is_constant_expr(t) == 0) {  
    g95_error("Bounds of array '%s' at %L must be constant",    
	      t->symbol->name, &t->where);        
    return FAILURE;         
  }      
      
  return SUCCESS;         
}         
         
         
          
          
/* count_elements()-- Work function that counts the number of elements
 * present in a constructor. */        
        
static try count_elements(g95_expr *s) {      
mpz_t res;      
      
  if (s->rank == 0)   
    mpz_add_ui(*current_expand.count, *current_expand.count, 1);          
  else {      
    if (g95_array_size(s, &res) == FAILURE) {   
      g95_free_expr(s);   
      return FAILURE;
    }    
    
    mpz_add(*current_expand.count, *current_expand.count,         
	    res);  
    mpz_clear(res);        
  }

  g95_free_expr(s);      
  return SUCCESS;
}       
       
       
  
  
/* g95_copy_constructor()-- Copy a constructor structure. */ 
 
g95_constructor *g95_copy_constructor(g95_constructor *s1) {      
g95_constructor *dst; 
 
  if (s1 == NULL) return NULL;    
    
  dst = g95_get_constructor();     
  dst->where = s1->where; 
  dst->expr = g95_copy_expr(s1->expr);          
  dst->iterator = copy_iterator(s1->iterator);     
     
  dst->next = g95_copy_constructor(s1->next);    
    
  return dst;  
}      
      
      
       
       
/* g95_start_constructor()-- Start an array constructor.  The
 * constructor starts with zero elements and should be appended to by
 * g95_append_constructor(). */

g95_expr *g95_start_constructor(bt dtype, int k, g95_locus *where) {    
g95_expr *res;    
    
  res = g95_get_expr();  
  
  res->type = EXPR_ARRAY;  
  res->rank = 1;        
        
  res->ts.type = dtype;          
  res->ts.kind = k;          
  res->where = *where;         
         
  return res;         
}      
      
      
          
          
/* g95_set_array_spec()-- Given a symbol and an array specification,
 * modify the symbol to have that array specification.  The error
 * locus is needed in case something goes wrong.  On failure, the
 * caller must free the spec. */  
  
try g95_set_array_spec(g95_symbol *sy, g95_array_spec *ref,         
		       g95_locus *error_loc) {     
     
  if (ref == NULL) return SUCCESS;

  if (g95_add_dimension(&sy->attr, error_loc) == FAILURE) return FAILURE; 
 
  sy->as = ref;      
      
  return SUCCESS;         
}          
          
          
 
 
/* g95_match_array_spec()-- Matches an array specification,
 * incidentally figuring out what sort it is.  */     
     
match g95_match_array_spec(g95_array_spec **asp) {       
array_type current_type;     
g95_array_spec *as;  
int o;         
         
 if (g95_match_char('(') != MATCH_YES) {      
    *asp = NULL;        
    return MATCH_NO; 
  }       
       
  as = g95_get_array_spec();          
          
  for(o=0; o<G95_MAX_DIMENSIONS; o++) {   
    as->lower[o] = NULL;        
    as->upper[o] = NULL;        
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
    for(o=0; o<as->rank; o++) {         
      if (as->lower[o] == NULL)   
	as->lower[o] = g95_int_expr(1);     
    }         
  }

  *asp = as;   
  return MATCH_YES;       
       
/* Something went wrong */

cleanup:         
  g95_free_array_spec(as); 
  return MATCH_ERROR;   
}         
         
         
          
          
/* g95_free_constructor()-- Free chains of g95_constructor structures */      
      
void g95_free_constructor(g95_constructor *p) {         
g95_constructor *next;

  if (p == NULL) return;         
         
  for(; p; p=next) {        
    next = p->next;    
    
    g95_free_expr(p->expr);  
    if (p->iterator != NULL) g95_free_iterator(p->iterator, 1);        
    g95_free(p);      
  }    
} 
 
 
      
      
/* spec_dimen_size()-- Get the size of single dimension of an array
 * specification.  The array is guaranteed to be one dimensional */     
     
static try spec_dimen_size(g95_array_spec *spec, int dimen, mpz_t *res) {     
     
  if (spec == NULL || spec->type != AS_EXPLICIT ||         
      spec->lower[dimen]->type != EXPR_CONSTANT ||     
      spec->upper[dimen]->type != EXPR_CONSTANT) {     
     
    return FAILURE; 
  }  
  
  mpz_init(*res);       
       
  mpz_sub(*res, spec->upper[dimen]->value.integer,      
	  spec->lower[dimen]->value.integer);          
          
  mpz_add_ui(*res, *res, 1);         
         
  return SUCCESS;         
}


        
        
/* g95_compare_array_spec()-- Compares two array specifications.  */      
      
int g95_compare_array_spec(g95_array_spec *as1, g95_array_spec *as2) {
int l, e, a;  
  
  if (as1 == NULL && as2 == NULL) return 1;  
  
  if (as1 == NULL || as2 == NULL) return 0;

  if (as1->rank != as2->rank) return 0;   
   
  if (as1->rank == 0) return 1;         
         
  if (as1->type != as2->type) return 0;    
    
  if (as1->type == AS_EXPLICIT)          
    for(l=0; l<as1->rank; l++) {
      if (g95_extract_int(as1->lower[l], &e) != NULL) goto error;    
      if (g95_extract_int(as2->lower[l], &a) != NULL) goto error;     
      if (e != a) return 0;      
      
      if (g95_extract_int(as1->upper[l], &e) != NULL) goto error;     
      if (g95_extract_int(as2->upper[l], &a) != NULL) goto error;
      if (e != a) return 0;      
    }     
     
  return 1;        
        
error:      
  g95_internal_error("g95_compare_array_spec(): Array spec clobbered");    
  return 0;        /* Keep the compiler happy */     
} 
 
 
     
     
static mstring array_specs[] = {    
  minit("AS_EXPLICIT", AS_EXPLICIT),          
  minit("AS_ASSUMED_SHAPE", AS_ASSUMED_SHAPE),   
  minit("AS_DEFERRED", AS_DEFERRED),  
  minit("AS_ASSUMED_SIZE", AS_ASSUMED_SIZE),    
  minit(NULL, 0) };   
   
void g95_show_array_spec(g95_array_spec *spec) { 
int f;

  if (spec == NULL) {      
    g95_status("()");   
    return;  
  } 
 
  g95_status("(%d", spec->rank);

  if (spec->rank != 0) {   
    g95_status(" %s ", g95_code2string(array_specs, spec->type));        
        
    for(f=0; f<spec->rank; f++) { 
      g95_show_expr(spec->lower[f]);
      g95_status_char(' ');     
      g95_show_expr(spec->upper[f]);          
      g95_status_char(' ');     
    }     
  }       
       
  g95_status(")");
}      
      
      


/* constant_element()-- Work function for checking that an element of
 * a constructor is a constant, after removal of any iteration
 * variables.  We return FAILURE if not so. */         
         
static try constant_element(g95_expr *b) {         
int rc;       
       
  rc = g95_is_constant_expr(b);          
  g95_free_expr(b);       
       
  return rc ? SUCCESS : FAILURE;      
}       
       
       
 
 
/* g95_expand_data_constructor()-- Top level subroutine for expanding
 * constructors in a DATA statement. */    
    
try g95_expand_data_constructor(g95_expr *y) {   
expand_info expand_save;       
try r;    
    
  expand_save = current_expand;  
  iter_stack = NULL;

  current_expand.expand_work_function = g95_expand_ac_element;    
  r = expand_constructor(y->value.constructor);     
  current_expand = expand_save; 
 
  return r;   
} 
 
 
    
    
/* ref_dimen_size()-- Get the number of elements in an array section */    
    
static try ref_dimen_size(g95_array_ref *spec, g95_array_spec *a, int dimen,          
			  mpz_t *res) {    
mpz_t up, low, stride;     
try h;   
   
  switch(spec->dimen_type[dimen]) {    
  case DIMEN_ELEMENT:       
    mpz_init(*res);    
    mpz_set_ui(*res, 1); 
    h = SUCCESS;       
    break;         
         
  case DIMEN_VECTOR:       
    h = g95_array_size(spec->start[dimen], res);    /* Recurse! */          
    break;       
       
  case DIMEN_RANGE:    
    mpz_init(up);          
    mpz_init(low);         
    mpz_init(stride);  
    h = FAILURE;     
     
    if (spec->start[dimen] == NULL) {     
      if (a->lower[dimen] == NULL ||          
	  a->lower[dimen]->type != EXPR_CONSTANT) goto cleanup;        
      mpz_set(low, a->lower[dimen]->value.integer);       
    } else {         
      if (spec->start[dimen]->type != EXPR_CONSTANT) goto cleanup; 
      mpz_set(low, spec->start[dimen]->value.integer);    
    }   
   
    if (spec->end[dimen] == NULL) {
      if (a->upper[dimen] == NULL ||          
	  a->upper[dimen]->type != EXPR_CONSTANT) goto cleanup;       
      mpz_set(up, a->upper[dimen]->value.integer);    
    } else {     
      if (spec->end[dimen]->type != EXPR_CONSTANT) goto cleanup;       
      mpz_set(up, spec->end[dimen]->value.integer);      
    }        
        
    if (spec->stride[dimen] == NULL)      
      mpz_set_ui(stride, 1);         
    else {        
      if (spec->stride[dimen]->type != EXPR_CONSTANT) goto cleanup;        
      mpz_set(stride, spec->stride[dimen]->value.integer);    
    }        
        
    mpz_init(*res);        
    mpz_sub(*res, up, low);       
    mpz_add(*res, *res, stride);         
    mpz_div(*res, *res, stride);   
   
    /* Zero stride caught earlier */         
         
    if (mpz_cmp_ui(*res, 0) < 0) mpz_set_ui(*res, 0);    
    h = SUCCESS;   
   
  cleanup: 
    mpz_clear(up);
    mpz_clear(low);
    mpz_clear(stride);     
    return h;          
          
  default:        
    g95_internal_error("ref_dimen_size(): Bad dimen type"); 
  }       
       
  return h;     
}       
       
       
       
       
/* g95_check_iter_variable()-- Check an EXPR_VARIABLE expression in a
 * constructor to make sure that that variable is an iteration
 * variables. */      
      
try g95_check_iter_variable(g95_expr *e2) {  
  
g95_symbol *symb;        
cons_stack *h;       
       
  symb = e2->symbol;    
    
  for(h=stack_base; h; h=h->previous)    
    if (symb == h->iterator->var->symbol) return SUCCESS; 
 
  return FAILURE;        
}      
      
      


/* g95_resolve_array_spec()-- Takes an array specification, resolves
 * the expressions that make up the shape and make sure everything is
 * integral. */     
     
try g95_resolve_array_spec(g95_array_spec *a, int check_constant) { 
g95_expr *p;       
int x;          
          
  if (a == NULL) return SUCCESS;     
     
  for(x=0; x<a->rank; x++) {       
    p = a->lower[x];     
    if (resolve_array_bound(p, check_constant) == FAILURE) return FAILURE;   
   
    p = a->upper[x];       
    if (resolve_array_bound(p, check_constant) == FAILURE) return FAILURE;      
  }     
     
  return SUCCESS;    
}     
     
     
    
    
/* g95_array_ref_shape()-- Given an array reference, return the shape
 * of the reference in an array of mpz_t integers. */        
        
try g95_array_ref_shape(g95_array_ref *a, g95_array_spec *spec, mpz_t *shape) {         
int n;    
    
  n = 0;          
          
  switch(a->type) {         
  case AR_FULL:   
    for(; n<spec->rank; n++)  
      if (spec_dimen_size(spec, n, &shape[n]) == FAILURE) goto cleanup;       
       
    return SUCCESS;        
        
  case AR_SECTION:       
    for(; n<a->dimen; n++)
      if (ref_dimen_size(a, spec, n, &shape[n]) == FAILURE) goto cleanup;       
       
    return SUCCESS;     
     
  default:          
    break;   
  }

cleanup:    
  for(n--; n>=0; n--)  
    mpz_clear(shape[n]);    
    
  return FAILURE;  
}




/* g95_check_constructor()-- Checks a constructor to see if it is a
 * particular kind of expression-- specification, restricted,
 * or initialization as determined by the check_function.  */       
       
try g95_check_constructor(g95_expr *exp,
			  try (*check_function)(g95_expr *)) {  
cons_stack *base_save;       
try l;          
          
  base_save = stack_base;      
  stack_base = NULL;      
      
  l = check_constructor(exp->value.constructor, check_function);       
  stack_base = base_save;   
   
  return l;      
}         
         
         
 
 
/* check_duplicate_iterator()-- Given an expression node that might be an 
 * array constructor and a symbol, make sure that no iterators in this or
 * child constructors use the symbol as an implied-DO iterator. 
 * Returns nonzero if a duplicate was found. */   
   
static int check_duplicate_iterator(g95_constructor *i, g95_symbol *master) { 
g95_expr *o;

  for(; i; i=i->next) {  
    o = i->expr;     
     
    if (o->type == EXPR_ARRAY &&      
	check_duplicate_iterator(o->value.constructor, master)) return 1; 
 
    if (i->iterator == NULL) continue;          
          
    if (i->iterator->var->symbol == master) {      
      g95_error("DO-iterator '%s' at %L is inside iterator of the same name", 
		master->name, &i->where);          
          
      return 1;      
    }   
  }     
     
  return 0;     
}   
   
   
    
    
static try ref_size(g95_array_ref *ar, g95_array_spec *spec, mpz_t *res) {       
mpz_t s;  
int m;        
        
  mpz_init_set_ui(*res, 1);

  for(m=0; m<ar->dimen; m++) {       
    if (ref_dimen_size(ar, spec, m, &s) == FAILURE) { 
      mpz_clear(*res);   
      return FAILURE;
    }       
       
    mpz_mul(*res, *res, s);    
    mpz_clear(s);    
  }  
  
  return SUCCESS;   
} 
 
 
      
      
/* match_array_list()-- Match a list of array elements. */  
  
static match match_array_list(g95_constructor **res) {    
g95_constructor *z, *head, *end, *old;  
g95_iterator it; 
g95_locus old_loc;     
g95_expr *k;
match i;        
int w; 
 
  old_loc = g95_current_locus; 
 
  if (g95_match_char('(') == MATCH_NO) return MATCH_NO;     
     
  memset(&it, '\0', sizeof(g95_iterator));    
  head = NULL; 
 
  i = match_array_cons_element(&head);
  if (i != MATCH_YES) goto cleanup;     
     
  end = head;

  if (g95_match_char(',') != MATCH_YES) { 
    i = MATCH_NO;  
    goto cleanup;          
  }      
      
  for(w=1;; w++) {          
    i = g95_match_iterator(&it, 0); 
    if (i == MATCH_YES) break;         
    if (i == MATCH_ERROR) goto cleanup;    
    
    i = match_array_cons_element(&old);     
    if (i == MATCH_ERROR) goto cleanup;     
    if (i == MATCH_NO) {     
      if (w > 2) goto syntax;          
      i = MATCH_NO;
      goto cleanup;    /* Could be a complex constant */ 
    }      
      
    end->next = old;   
    end = old;  
  
    if (g95_match_char(',') != MATCH_YES) {         
      if (w > 2) goto syntax;     
      i = MATCH_NO;   
      goto cleanup;    
    }       
  }

  if (g95_match_char(')') != MATCH_YES) goto syntax;

  if (check_duplicate_iterator(head, it.var->symbol)) {
    i = MATCH_ERROR;
    goto cleanup;
  }   
   
  k = g95_get_expr();
  k->type = EXPR_ARRAY; 
  k->rank = 1;    
  k->where = old_loc;      
  k->value.constructor = head;        
        
  z = g95_get_constructor();    
  z->where = g95_current_locus;      
  z->iterator = g95_get_iterator();         
  *z->iterator = it;  
  
  z->expr = k;   
  *res = z;  
  
  return MATCH_YES;

syntax:     
  g95_error("Syntax error in array constructor at %C");  
  i = MATCH_ERROR;         
         
cleanup:    
  g95_free_constructor(head);         
  g95_free_iterator(&it, 0);         
  g95_current_locus = old_loc;   
  return i;  
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
     
     
      
      
/* g95_array_dimen_size()-- Given an array expression and a dimension,
 * figure out how many elements it has along that dimension.  Returns
 * SUCCESS if we were able to return a result in the 'result'
 * variable, FAILURE otherwise. */ 
 
try g95_array_dimen_size(g95_expr *a, int dimen, mpz_t *rslt) {  
g95_array_spec *as;    
g95_array_ref *ar;    
int f;          
          
  if (dimen > a->rank - 1)   
    g95_internal_error("g95_array_dimen_size(): Bad dimension");         
         
  switch(a->type) {          
  case EXPR_VARIABLE:         
    if (a->symbol == NULL) return FAILURE;        
        
    g95_find_array_ref(a, &ar, &as);     
     
    switch(ar->type) {     
    case AR_FULL:       
      return spec_dimen_size(as, dimen, rslt);    
    
    case AR_SECTION:        
      for(f=0; dimen>=0; f++)     
	if (ar->dimen_type[f] != DIMEN_ELEMENT) dimen--;  
  
      return ref_dimen_size(ar, as, f-1, rslt);         
         
    default:        
      g95_internal_error("g95_array_dimen_size(): Bad array ref");
    }          
          
    break;        
        
  default:       
    if (a->shape == NULL) return FAILURE;    
    
    mpz_init_set(*rslt, a->shape[dimen]);  
    break;     
  }          
          
  return SUCCESS; 
}    
    
    
   
   
/* check_element_type()-- Given an expression, compare its type with
 * the type of the current constructor.  Returns nonzero if an error
 * was issued.  The cons_state variable keeps track of whether the
 * type of the constructor being read or resolved is known to be good,
 * bad or just starting out. */

static int check_element_type(g95_expr *expr) {   
int d, r;         
         
  r = 0;  
  
  switch(cons_state) {           
  case CONS_BAD:  
    break;        /* Errors are suppressed */       
       
  case CONS_START:   
    if (expr->ts.type == BT_UNKNOWN)     
      cons_state = CONS_BAD;      
    else {        
      cons_state = CONS_GOOD;   
      constructor_ts = expr->ts;         
    } 
 
    g95_constructor_string_length = element_string_length(expr);   
    break;

  case CONS_GOOD:   
    if (!g95_compare_types(&constructor_ts, &expr->ts)) {    
      g95_error("Element in %s array constructor at %L is %s",      
		g95_typename(&constructor_ts), &expr->where,     
		g95_typename(&expr->ts));     
     
      r = 1;     
      break;      
    }         
         
    d = element_string_length(expr);     
     
    if (g95_constructor_string_length == -1)  
      g95_constructor_string_length = d;          
    else if (d != g95_constructor_string_length && d != -1) {      
      g95_error("Element in character array constructor at %L has length "         
		"%d instead of %d", &expr->where, d,     
		g95_constructor_string_length);  
  
      r = 1;
      break;          
    }        
        
    break;     
  }     
     
  if (r) cons_state = CONS_BAD;
  return r;  
}       
       
       
 
 
/* g95_expand_iterator()-- Given an iterator, a function to call and
 * something to expand, we effectively assign the correct values to
 * the loop variable and call the expansion function with the thing to
 * expand.  If the expansion function fails, we terminate the loop. */     
     
try g95_expand_iterator(g95_iterator *iterator, try (*expand)(void *),      
			void *s) {  
g95_expr *st, *stop, *step;   
iterator_stack frame;        
mpz_t trip;      
try a;       
       
  st = stop = step = NULL;      
      
  a = FAILURE;         
         
  mpz_init(trip);         
  mpz_init(frame.value); 
 
  st = g95_copy_expr(iterator->start);    
  if (g95_simplify_expr(st, 1) == FAILURE) goto cleanup;

  if (st->type != EXPR_CONSTANT || st->ts.type != BT_INTEGER)   
    goto cleanup;  
  
  stop = g95_copy_expr(iterator->end);  
  if (g95_simplify_expr(stop, 1) == FAILURE) goto cleanup;      
      
  if (stop->type != EXPR_CONSTANT || stop->ts.type != BT_INTEGER)   
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
   
  mpz_sub(trip, stop->value.integer, st->value.integer);    
  mpz_add(trip, trip, step->value.integer);        
  mpz_tdiv_q(trip, trip, step->value.integer);         
         
  mpz_set(frame.value, st->value.integer);   
   
  frame.prev = iter_stack;        
  frame.variable = iterator->var->symbol;
  iter_stack = &frame; 
 
  while(mpz_sgn(trip) > 0) {      
    if (expand(s) == FAILURE) goto cleanup;

    mpz_add(frame.value, frame.value, step->value.integer);         
    mpz_sub_ui(trip, trip, 1);     
  }    
    
  a = SUCCESS; 
 
cleanup:    
  g95_free_expr(st);        
  g95_free_expr(stop);      
  g95_free_expr(step);

  mpz_clear(trip);       
  mpz_clear(frame.value);        
        
  iter_stack = frame.prev;        
        
  return a;       
}        
        
        
  
  
/* g95_array_size()-- Given an array expression, figure out how many
 * elements are in the array.  Returns SUCCESS if this is possible,
 * and sets the 'result' variable.  Otherwise returns FAILURE. */ 
 
try g95_array_size(g95_expr *arr, mpz_t *result) {          
expand_info expand_save;     
g95_array_spec *spec;     
g95_array_ref *a;      
int p, flag; 
try n; 
 
  switch(arr->type) {        
  case EXPR_ARRAY:   
    flag = g95_suppress_error;  
    g95_suppress_error = 1;  
  
    expand_save = current_expand;      
      
    current_expand.count = result;          
    mpz_init_set_ui(*result, 0);      
      
    current_expand.expand_work_function = count_elements;        
    iter_stack = NULL;        
        
    n = expand_constructor(arr->value.constructor);          
    g95_suppress_error = flag;        
        
    if (n == FAILURE) mpz_clear(*result);   
    current_expand = expand_save;  
    return n;          
          
  case EXPR_VARIABLE: 
    g95_find_array_ref(arr, &a, &spec);   
   
    switch(a->type) {
    case AR_FULL:      
      return g95_array_spec_size(spec, result);     
     
    case AR_SECTION:       
      return ref_size(a, spec, result);

    default:          
      g95_internal_error("g95_array_size(): Bad ref");
    }          
          
  case EXPR_FUNCTION:       
    if (array_function_size(arr, result) == SUCCESS) break;         
    /* Fall through */      
      
  default:
    if (arr->rank == 0 || arr->shape == NULL) return FAILURE;  
  
    mpz_init_set_ui(*result, 1);      
      
    for(p=0; p<arr->rank; p++)     
      mpz_mul(*result, *result, arr->shape[p]);   
   
    break;       
  }     
     
  return SUCCESS;     
}      
      
      
    
    
/* check_constructor_type()-- Recursive work function for
 * g95_check_constructor_type(). */    
    
static try check_constructor_type(g95_constructor *t) {         
g95_expr *p;

  for(; t; t=t->next) { 
    p = t->expr;     
     
    if (p->type == EXPR_ARRAY) {          
      if (check_constructor_type(p->value.constructor) == FAILURE) 
	return FAILURE;

      continue; 
    }     
     
    if (check_element_type(p)) return FAILURE; 
  }   
   
  return SUCCESS; 
}     
     
     
  
  
/* expand_expr()-- Expand an expression with that is inside of a
 * constructor, recursing into other constructors if present. */    
    
static try expand_expr(g95_expr *e) {  
  
  if (e->type == EXPR_ARRAY)      
    return expand_constructor(e->value.constructor);       
       
  e = g95_copy_expr(e);          
          
  if (g95_simplify_expr(e, 1) == FAILURE) {          
    g95_free_expr(e);
    return FAILURE;          
  }    
    
  return current_expand.expand_work_function(e);     
}     
     
     
     
     
/* expand_ac_element()-- Work function for expanding a constructor */          
          
static try expand_ac_element(g95_expr *i) {      
g95_constructor *f;  
  
  if (i->rank != 0) { 
    g95_free_expr(i);       
    return FAILURE;         
  }     
     
  f = g95_get_constructor();  
  
  if (current_expand.head == NULL)      
    current_expand.head = f;        
  else  
    current_expand.tail->next = f;  
  
  current_expand.tail = f;    
  f->expr = i;   
  return SUCCESS;        
}    
    
    
 
 
/* g95_array_spec_size()-- Given an array specification, figure out
 * how big it is. */        
        
try g95_array_spec_size(g95_array_spec *ref, mpz_t *res) {    
mpz_t size;   
int o;

  mpz_init_set_ui(*res, 1);       
       
  for(o=0; o<ref->rank; o++) { 
    if (spec_dimen_size(ref, o, &size) == FAILURE) {     
      mpz_clear(*res);         
      return FAILURE;        
    } 
 
    mpz_mul(*res, *res, size);        
    mpz_clear(size);  
  }         
         
  return SUCCESS;     
}  
  
  
   
   
/* check_constructor()-- Recursive work function for
 * g95_check_constructor().  This amounts to calling the check
 * function for each expression in the constructor, giving variables
 * with the names of iterators a pass.  */       
       
static try check_constructor(g95_constructor *q,        
			     try (*check_function)(g95_expr *)) {   
cons_stack element;    
g95_expr *g;   
try j;   
   
  for(; q; q=q->next) {
    g = q->expr;

    if (g->type != EXPR_ARRAY) { 
      if ((*check_function)(g) == FAILURE) return FAILURE;          
      continue;    
    }   
   
    element.previous = stack_base;
    element.iterator = q->iterator;    
    
    stack_base = &element;         
    j = check_constructor(g->value.constructor, check_function);
    stack_base = element.previous;    
    
    if (j == FAILURE) return FAILURE;   
  }      
      
/* Nothing went wrong, so all OK */ 
 
  return SUCCESS;  
}          
          
          
      
      
/* g95_resolve_array_constructor()-- Resolve all of the expressions in
 * an array list. */     
     
try g95_resolve_array_constructor(g95_expr *e1) {      
g95_constructor *d;          
try w;      
      
  w = SUCCESS;      
      
  for(d=e1->value.constructor; d; d=d->next) {      
    if (d->iterator != NULL && g95_resolve_iterator(d->iterator) == FAILURE)         
      w = FAILURE;        
        
    if (g95_resolve_expr(d->expr) == FAILURE) w = FAILURE;      
  }         
         
  if (w == SUCCESS) w = g95_check_constructor_type(e1);  
  return w;          
}         
         
         
     
     
/* copy_array_spec()-- Copy an array specification. */  
  
g95_array_spec *g95_copy_array_spec(g95_array_spec *source) {
g95_array_spec *d;
int p;      
      
  if (source == NULL) return NULL;     
     
  d = g95_get_array_spec();    
    
  *d = *source;  
  
  for(p=0; p<d->rank; p++) {  
    d->lower[p] = g95_copy_expr(d->lower[p]);        
    d->upper[p] = g95_copy_expr(d->upper[p]);          
  }     
     
  return d;
}          
          
          
    
    
/* g95_check_constructor_type()-- Check that all elements of an array
 * constructor are the same type.  On FAILURE, an error has been
 * generated. */          
          
try g95_check_constructor_type(g95_expr *i) {      
try l;        
        
  cons_state = CONS_START;      
  g95_clear_ts(&constructor_ts); 
  g95_constructor_string_length = -1;        
        
  l = check_constructor_type(i->value.constructor);    
  if (l == SUCCESS && i->ts.type == BT_UNKNOWN) i->ts = constructor_ts;        
        
  return l;    
}      
      
      
 
 
/* g95_get_array_element()-- Given an array expression and an element
 * number (starting at zero), return a pointer to the array element.
 * NULL is returned if the size of the array has been exceeded.  The
 * expression node returned remains a part of the array and should not
 * be freed.  Access is not efficient at all, but this is another
 * place where things do not have to be particularly fast. */ 
 
g95_expr *g95_get_array_element(g95_expr *array, int elem) { 
expand_info expand_save;
g95_expr *g;     
try retval;    
    
  expand_save = current_expand;     
  current_expand.extract_n = elem;     
  current_expand.expand_work_function = extract_element;    
  current_expand.extracted = NULL; 
  current_expand.extract_count = 0;   
   
  iter_stack = NULL;      
      
  retval = expand_constructor(array->value.constructor);      
  g = current_expand.extracted;
  current_expand = expand_save;   
  
  if (retval == FAILURE)
    return NULL;       
       
  return g; 
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
 
 
       
       
/* match_array_cons_element()-- match a single element of an array
 * constructor, which can be a single expression or a list of
 * elements. */       
       
static match match_array_cons_element(g95_constructor **res) {      
g95_constructor *e;   
g95_expr *expr;       
match w;     
     
  w = match_array_list(res);  
  if (w != MATCH_NO) return w;  
  
  w = g95_match_expr(&expr);
  if (w != MATCH_YES) return w;         
         
  e = g95_get_constructor(); 
  e->where = g95_current_locus;          
  e->expr = expr;          
          
  *res = e;    
  return MATCH_YES;          
}      
      
      
    
    
/* g95_constant_ac()-- Given an array constructor, determine if the
 * constructor is constant or not by expanding it and making sure that
 * all elements are constants.  This is a bit of a hack since
 * something like (/ (i, i=1,100000000) /) will take a while as
 * opposed to a more clever function that traverses the expression
 * tree. */       
       
int g95_constant_ac(g95_expr *t) {       
expand_info expand_save;          
try r;        
        
  iter_stack = NULL;   
  expand_save = current_expand;  
  current_expand.expand_work_function = constant_element;     
     
  r = expand_constructor(t->value.constructor);          
            
  current_expand = expand_save;   
  if (r == FAILURE) return 0;         
         
  return 1;     
}         
         
         
 
 
/* g95_find_array_ref()-- Given an array expression, find the array
 * reference and/or specification that characterizes the reference. */     
     
void g95_find_array_ref(g95_expr *r, g95_array_ref **ar_p,    
			g95_array_spec **as_p) {         
g95_array_spec *spec;         
g95_ref *re;     
     
  spec = r->symbol->as;   
   
  for(re=r->ref; re; re=re->next) 
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
    
    
/* g95_append_constructor()-- Given an array constructor expression,
 * append the new expression node onto the constructor. */          
          
void g95_append_constructor(g95_expr *base, g95_expr *new) {          
g95_constructor *x;         
         
  if (base->value.constructor == NULL)        
    base->value.constructor = x = g95_get_constructor();    
  else { 
    x = base->value.constructor;   
    while(x->next)
      x=x->next;    
    
    x->next = g95_get_constructor();       
    x = x->next;   
  }         
         
  x->expr = new;   
   
  if (new->ts.type != base->ts.type || new->ts.kind != base->ts.kind)  
    g95_internal_error("g95_append_constructor(): New node has wrong kind");        
}       
       
       
 
 
/* expand_constructor()-- Expand a constructor into constant
 * constructors without any iterators, calling the work function for
 * each of the expanded expressions.  The work function needs to
 * either save or free the passed expression. */         
         
static try expand_constructor(g95_constructor *q) {  
g95_expr *d;         
         
  for(; q; q=q->next) {        
    if (q->iterator != NULL) {         
      if (g95_expand_iterator(q->iterator, (void *) expand_expr,      
			      q->expr) == FAILURE)      
	return FAILURE;  
  
      continue;       
    }    
    
    d = q->expr;    
    
    if (d->type == EXPR_ARRAY) {  
      if (expand_constructor(d->value.constructor) == FAILURE)     
	return FAILURE;          
          
      continue;     
    }      
      
    d = g95_copy_expr(d);    
    if (g95_simplify_expr(d, 1) == FAILURE) { 
      g95_free_expr(d); 
      return FAILURE;  
    }      
      
    if (current_expand.expand_work_function(d) == FAILURE) return FAILURE;         
  }      
      
  return SUCCESS;    
}


