      
/* DATA statement translation
   Copyright (C) 2003 Free Software Foundation, Inc.
   Contributed by Andy Vaught.

This file is part of g95.

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
     
#include "trans.h"
#include <string.h>
         
/* Variables pointing to the current data constant and repeat count. */


typedef struct {    
  mpz_t start, end, step, extent, value, trip, count; 
} dimen;  
  
static try expand_data_var(g95_data_variable *);  
  
typedef struct init_tree {  
  BBT_HEADER(init_tree)

  int start, end; 
  tree head, tail;  
} init_tree;         
         
         
typedef struct data_t {        
  init_tree *root;        
        
  tree ac_string_length, array_list;   
  int ac_count, initial_offset, repeat_count, error_flag; 
  g95_data_value *current_value;         
         
  locus current_data;    
  g95_symbol *current_symbol;  
  
  mpz_t current_offset, string_length;      
      
  g95_array_spec *as_save;
  g95_array_ref *ar_save;  
  
  struct data_t *previous;        
} data_t;

static data_t dinfo;      
      
#define get_init_tree() g95_getmem(sizeof(init_tree))
    
    
    
    
/* find_start()-- Recursively find a node with a particular start
 * value.  Returns NULL if no such node exists. */      
      
static init_tree *find_start(init_tree *b, int sta) {          
          
  if (b == NULL) return NULL;         
         
  if (b->start == sta) return b;  
  
  return find_start(b->start < sta ? b->right : b->left, sta);  
}  
  
  
    
    
/* build_structure_init()-- Build a contructor for a heterogeneous
 * init_tree. */       
       
static tree build_structure_init(void) { 
 
  g95_internal_error("build_structure_init(): Not ready");  
}  
  
  
     
     
/* has_current_symbol()-- Checks to see if the data node involves the
 * current symbol.  Returns nonzero if this is so. */         
         
static int has_current_symbol(g95_data_variable *v) {      
      
  if (v == NULL) return 0;       
       
  return (v->expr != NULL && v->expr->symbol == dinfo.current_symbol) ||     
    has_current_symbol(v->list) ||   
    has_current_symbol(v->next);         
}        
        
        
       
       
/* expand_array_ref()-- Accumulate the offset of an array element into
 * the current offset.  Saves array sections to be expanded later.
 * Returns FAILURE if something goes wrong. */          
          
static try expand_array_ref(g95_ref *re, g95_array_spec *as,
			    g95_typespec *typesp) {          
int l, section, element_size;        
mpz_t acc;     
     
  element_size = int_size_in_bytes(g95_typenode_for_spec(typesp));   
   
  if (as == NULL) {   
    g95_error("Array reference follows scalar object at %L", &re->where);         
    return FAILURE; 
  }       
       
  section = (re->u.ar.type == AR_FULL);   
  if (!section)      
    for(l=0; l<as->rank; l++)     
      if (re->u.ar.dimen_type[l] == DIMEN_RANGE) section = 1;       
       
  /* For an element, calculate the offset.  Save a section for later
   * expansion. */          
          
  if (section) {        
    if (dinfo.as_save != NULL) {       
      g95_error("Multiple array references in expression at %L",  
		&re->where);      
      return FAILURE;        
    }        
        
    dinfo.ar_save = &re->u.ar;    
    dinfo.as_save = as;          
  } else {
    l = g95_element_number(&re->u.ar, as);
    if (l < 0) {     
      g95_error("Array reference at %L is outside of array", &re->where);
      return FAILURE;  
    }          
          
    mpz_init_set_ui(acc, element_size);
    mpz_mul_ui(acc, acc, l);       
    mpz_add(dinfo.current_offset, dinfo.current_offset, acc);     
    mpz_clear(acc);        
  } 
 
  return SUCCESS;   
}          
          
          
          
          
/* init_dimen()-- Initialize a dimen structure. */      
      
static void init_dimen(dimen *u) {

  mpz_init(u->start); 
  mpz_init(u->end);         
  mpz_init(u->step);     
  mpz_init(u->extent);   
  mpz_init(u->value);   
  mpz_init(u->trip);  
  mpz_init_set_ui(u->count, 0);     
}         
         
         
       
       
/* free_init_tree()-- Recursively free the entire tree */   
   
static void free_init_tree(init_tree *h) {          
          
  if (h == NULL) return;    
  free_init_tree(h->left);
  free_init_tree(h->right); 
 
  g95_free(h);     
}      
      
      
    
    
/* init_loop()-- Initialize each dimension of an array section.
 * Dimensions that are single elements are replaced with an equivalent
 * single-trip loop. */         
         
static try init_loop(int r, dimen *b) {         
         
  if (dinfo.ar_save->type == AR_FULL) {   
    mpz_set(b->start, dinfo.as_save->lower[r]->value.integer);       
    mpz_set(b->end,   dinfo.as_save->upper[r]->value.integer);      
    mpz_set_ui(b->step, 1);
    return SUCCESS; 
  } 
 
  switch(dinfo.ar_save->dimen_type[r]) {  
  case DIMEN_ELEMENT:   
    if (dinfo.ar_save->start[r]->type != EXPR_CONSTANT) {
      g95_error("Array element in DATA not constant at %L",
		&dinfo.ar_save->start[r]->where);      
      return FAILURE;     
    }       
       
    mpz_set(b->start, dinfo.ar_save->start[r]->value.integer);         
    mpz_set(b->end,   b->start);  
    mpz_set_ui(b->step, 1);        
    break;          
          
  case DIMEN_RANGE:   
    if (dinfo.ar_save->start[r] == NULL)  
      mpz_set(b->start, dinfo.as_save->lower[r]->value.integer);    
    else {       
      if (dinfo.ar_save->start[r]->type != EXPR_CONSTANT) {
	g95_error("Array range start in DATA not constant at %L",       
		  &dinfo.ar_save->start[r]->where);        
	return FAILURE; 
      }        
        
      mpz_set(b->start, dinfo.ar_save->start[r]->value.integer);    
    }         
         
    if (dinfo.ar_save->end[r] == NULL)         
      mpz_set(b->end, dinfo.as_save->upper[r]->value.integer);          
    else {          
      if (dinfo.ar_save->end[r]->type != EXPR_CONSTANT) {     
	g95_error("Array range end in DATA not constant at %L",        
		  &dinfo.ar_save->end[r]->where);         
	return FAILURE;      
      }         
         
      mpz_set(b->end, dinfo.ar_save->end[r]->value.integer);      
    }       
       
    if (dinfo.ar_save->stride[r] == NULL)        
      mpz_set_ui(b->step, 1);       
    else {   
      if (dinfo.ar_save->stride[r]->type != EXPR_CONSTANT) {      
	g95_error("Array range step in DATA not constant at %L",         
		  &dinfo.ar_save->stride[r]->where);       
	return FAILURE;     
      }       
       
      mpz_set(b->step,  dinfo.ar_save->stride[r]->value.integer);        
    }   
   
    break;  
  
  case DIMEN_VECTOR:     
    g95_error("Vector subscripts not allowed in DATA statement at %L", 
	      &dinfo.ar_save->start[r]->where);   
    return FAILURE;       
       
  case DIMEN_UNKNOWN:     
    g95_internal_error("init_dimen(): bad dimension type");     
  }  
  
  return SUCCESS;   
}        
        
        


/* expand_data()-- Translate a single DATA statement.  We traverse the
 * variable and constant lists, matching the two as we go.  The
 * implicit loops within data statements are used as are the repeat
 * counts of the constants. */   
   
static try expand_data(g95_data *e) { 
g95_data_variable *var;   
try c;      
      
  dinfo.repeat_count = 0;        
  dinfo.current_value = e->value;       
  dinfo.current_data = e->where;   
   
  mpz_init(dinfo.current_offset);  
  mpz_init(dinfo.string_length);         
  c = FAILURE;         
         
  for(var=e->var; var; var=var->next) 
    if (expand_data_var(var) == FAILURE) goto done;          
          
  /* We're done.  Make sure that the list of data items is also empty */   
   
  if (dinfo.current_value != NULL) {     
    g95_error("DATA statement at %L has more values that variables",     
	      &dinfo.current_value->expr->where);     
    goto done;        
  } 
 
  c = SUCCESS;   
   
done:  
  mpz_clear(dinfo.current_offset);  
  mpz_clear(dinfo.string_length);          
  return c; 
}          
          
          
   
   
/* push_data()-- Push a data node */          
          
static void push_data(void) {   
data_t *w;

  w = g95_getmem(sizeof(dinfo));         
  *w = dinfo;  
  
  memset(&dinfo, '\0', sizeof(dinfo));   
  dinfo.previous = w;    
}    
    
    
       
       
/* clear_dimen()-- Clear a dimen structure. */        
        
static void clear_dimen(dimen *t) {  
  
  mpz_clear(t->start); 
  mpz_clear(t->end);      
  mpz_clear(t->step);  
  mpz_clear(t->extent); 
  mpz_clear(t->value);     
  mpz_clear(t->trip);
  mpz_clear(t->count);
} 
 
 
         
         
/* pop_data()-- Pop a data node */        
        
static void pop_data(void) {      
data_t *v;          
          
  v = dinfo.previous;     
  dinfo = *v;       
       
  g95_free(v);   
} 
 
 
          
          
/* init_tree_type()-- Return the type of an init_tree, examining the
 * innards of a list node if necessary. */      
      
static tree init_tree_type(tree decl) {    
    
  if (TREE_CODE(decl) == TREE_LIST) decl = TREE_VALUE(decl);      
  return TREE_TYPE(decl);
}         
         
         
 
 
/* same_type0()-- Recursive function for same_type() */      
      
static int same_type0(init_tree *m, tree t) {         
         
  if (m == NULL) return 1;

  return init_tree_type(m->head) == t
    && same_type0(m->left,  t)   
    && same_type0(m->right, t);  
}     
     
     
          
          
/* same_type()-- Checks to see if all elements of the tree are the
 * same type as the root node.  Return nonzero if this is so. */ 
 
static int same_type(void) {   
tree dtype; 
 
  dtype = init_tree_type(dinfo.root->head);        
  return same_type0(dinfo.root, dtype);   
}     
     
     


/* next_value()-- Return the next value in the value list, taking the
 * repeat count into account.  Returns NULL if we are out of data
 * items. */    
    
static tree next_value(g95_expr **last_expr) {        
static tree value;  
g95_se se;       
       
  if (dinfo.current_value == NULL) {     
    g95_error("Not enough values in DATA statement at %L",     
	      &dinfo.current_data);       
    return NULL;        
  } 
 
  *last_expr = dinfo.current_value->expr; 
 
  if (dinfo.repeat_count == 0) {  
    g95_init_se(&se, NULL);   
    g95_conv_constant(&se, dinfo.current_value->expr);       
    value = se.expr;  
  } 
 
  if (++dinfo.repeat_count >= dinfo.current_value->repeat) {
    dinfo.current_value = dinfo.current_value->next;         
    dinfo.repeat_count = 0;   
  }      
      
  return value;     
}  
  
  


/* type_check()-- Given a value and a type, make sure the value can be
 * assigned correctly.  Converts types and in the case of strings,
 * lengths.  Return NULL if something goes wrong. */ 
 
static tree type_check(tree declr, g95_expr *u, g95_typespec *ts) {       
g95_typespec value_ts;        
tree t, length;   
   
  value_ts = u->ts;          
  t = g95_typenode_for_spec(ts);        
        
  switch(ts->type) {  
  case BT_CHARACTER:     
    if (value_ts.type != BT_CHARACTER) {     
      g95_error("Character constant required at %L", &u->where);   
      return NULL_TREE;    
    }  
  
    length = g95_conv_mpz_to_tree(dinfo.string_length, -1); 
    declr = g95_resize_string_constant(declr, length);  
    break; 
 
  case BT_DERIVED:  
    if (u->ts.type != BT_DERIVED || u->ts.derived != value_ts.derived) {        
      g95_error("Structure constructor '%s' required at %L",          
		value_ts.derived->name, &u->where);      
      return NULL_TREE;      
    }   
   
    break;  
  
  case BT_LOGICAL:    
    if (value_ts.type != BT_LOGICAL) {    
      g95_error("Logical constant required at %L", &u->where);     
      return NULL_TREE;      
    }        
        
    declr = fold(convert(t, declr));          
    break;      
      
  case BT_REAL:       
  case BT_INTEGER:    
  case BT_COMPLEX:     
    if (value_ts.type != BT_REAL && value_ts.type != BT_INTEGER &&     
	value_ts.type != BT_COMPLEX) {       
      g95_error("Numeric constant required at %L", &u->where);    
      return NULL_TREE; 
    } 
 
    declr = fold(convert(t, declr));      
    break;       
       
  default:
    g95_internal_error("type_check(): Bad type");    
  } 
 
  return declr;          
}         
         
         
    
    
/* element_number()-- Given a pointer to an mpz_t, figure out which
 * element number this is, zero based. */        
        
static int element_number(int o) {  
int siz;       
       
  siz = int_size_in_bytes(init_tree_type(dinfo.root->head)); 
 
  return o / siz;   
}  
  
  


/* build_array_init0()-- Add the current init_tree to the list of
 * constructors. */      
      
static void build_array_init0(init_tree *i) {          
tree d;      
int v;       
       
  v = element_number(i->start);

  d = i->head; 
  if (TREE_CODE(d) != TREE_LIST) {  /* Single element */
    d = tree_cons(NULL_TREE, d, NULL_TREE);        
    TREE_PURPOSE(d) = build_int_2(v, 0);         
         
    TREE_CHAIN(d) = dinfo.array_list;      
    dinfo.array_list = d;     
     
  } else {   /* List of elements */          
    for(;;) {     
      TREE_PURPOSE(d) = build_int_2(v, 0);     
      if (TREE_CHAIN(d) == NULL) break;         
         
      v++;       
      d = TREE_CHAIN(d);   
    }       
       
    TREE_CHAIN(d) = dinfo.array_list;       
    dinfo.array_list = i->head;    
  }
}        
        
        
       
       
/* expand_string_ref()-- Accumulate offset from a substring reference.
 * Also sets string_length to the correct value */       
       
static try expand_string_ref(g95_ref *re, g95_typespec *typ) {
mpz_t start, fin;   
g95_expr *q;      
try v;          
          
  v = FAILURE;    
  mpz_init(start);     
  mpz_init(fin);        
        
  q = re->u.ss.start;     
  if (q == NULL)       
    mpz_init_set_ui(start, 1);       
  else {       
    if (q->type != EXPR_CONSTANT) {          
      g95_error("Substring start value at %L must be constant", &q->where);         
      goto done;    
    }  
  
    mpz_init_set(start, q->value.integer);      
  }     
     
  q = re->u.ss.end;        
  if (q == NULL) q = typ->cl->length;    
    
  if (q->type != EXPR_CONSTANT) {       
    g95_error("Substring end value at %L must be constant", &q->where);         
    goto done;      
  }          
          
  mpz_add(dinfo.current_offset, dinfo.current_offset, start);  
  mpz_sub_ui(dinfo.current_offset, dinfo.current_offset, 1);   
   
  mpz_sub(dinfo.string_length, fin, start);          
  mpz_add_ui(dinfo.string_length, dinfo.string_length, 1);    
    
done: 
  mpz_clear(start);
  mpz_clear(fin);         
  return v;   
}         
         
         


/* compare_init_tree()-- Compare two init_tree structures.  The
 * structures are assumed not to overlap. */        
        
static int compare_init_tree(void *v, void *k) {    
init_tree *t, *l;

  t = (init_tree *) v;  
  l = (init_tree *) k;

  if (t->start < l->start) return -1;    
  if (t->start > l->start) return 1;   
   
  return 0;         
}  
  
  


/* generate_data()-- Process the DATA statements in a program
 * unit for a particular symbol.  This subroutine can be called
 * multiple times when initializing a COMMON block. */   
   
static void generate_data(g95_symbol *sy, int offset) {     
g95_data *x;   
   
  if (dinfo.error_flag) return;         
         
  dinfo.current_symbol = sy;          
  dinfo.initial_offset = offset;          
          
  for(x=sy->ns->data; x; x=x->next) {    
    if (!has_current_symbol(x->var)) continue;          
    if (expand_data(x) == FAILURE) {        
      dinfo.error_flag = 1;    
      break;
    }    
  }       
}


        
        
/* find_end()-- Recursively find a node with a particular end value.
 * Returns NULL if no such node exists. */   
   
static init_tree *find_end(init_tree *f, int stop) {         
         
  if (f == NULL) return NULL;      
      
  if (f->end == stop) return f;       
       
  return find_end(f->end < stop ? f->right : f->left, stop);   
}  
  
  
     
     
/* char_init()-- Given a range, initialize any holes in that range to
 * blank characters.  Also has the effect of concatenating adjacent
 * strings. */          
          
static void char_init(mpz_t *sta, mpz_t *fin) { 
 
 
}         
         
         
        
        
/* check_overlap()-- Make sure that this init_tree structure does not
 * overlap with any other structure in the tree.  Returns nonzero if
 * an overlap is detected. */  
  
static int check_overlap(init_tree *old, init_tree *current) {     
     
  if (current == NULL) return 0;     
     
  if (old->start == current->start) return 1;     
     
  if (old->start < current->start) {        
    if (old->end > current->start) return 1;         
    current = current->left;    
  } else {          
    if (old->start < current->end) return 1;    
    current = current->right;     
  }        
        
  return check_overlap(old, current);         
}      
      
      


/* expand_list()-- Expand a list of data items for one set of
 * iteration values.  Mutually recursive with expand_data_var(). */         
         
static try expand_list(g95_data_variable *list) {   
   
  for(; list!=NULL; list=list->next)     
    if (expand_data_var(list) == FAILURE) return FAILURE;          
          
  return SUCCESS;         
}  
  
  
   
   
/* join_nodes()-- Given a position in the tree, find the nodes on the
 * left and right and concatenate them into a single constructor list
 * if they are of the same type. */ 
 
static void join_nodes(int w) {   
init_tree *left, *right;

  left  = find_end(dinfo.root, w);        
  right = find_start(dinfo.root, w);       
       
  if (left == NULL || right == NULL ||    
      init_tree_type(left->head) != init_tree_type(right->head)) return;        
        
  /* Join the nodes */   
   
  if (TREE_CODE(left->head) != TREE_LIST) {   
    left->head = g95_chainon_list(NULL_TREE, left->head);
    left->tail = left->head;
  }  
  
  if (TREE_CODE(right->head) != TREE_LIST) {
    right->head = g95_chainon_list(NULL_TREE, right->head);        
    right->tail = right->head; 
  }          
          
  TREE_CHAIN(left->tail) = right->head;      
  left->tail = right->tail;    
    
  left->end = right->end;     
     
  g95_delete_bbt(&dinfo.root, right, compare_init_tree);      
  right->left = right->right = NULL;       
       
  free_init_tree(right);   
} 
 
 


/* add_value()-- Given an initialization value, insert it at the
 * current offset. */ 
 
static try add_value(tree value) {       
int st, e;    
init_tree *f;  
try u;       
       
  f = get_init_tree(); 
  f->head = value;         
         
  f->start = st = mpz_get_ui(dinfo.current_offset);    
  f->end = e = st + int_size_in_bytes(TREE_TYPE(value));        
        
  if (check_overlap(f, dinfo.root)) {   
    g95_error("Memory is initialized more than once (offset %d) at %L", 
	      mpz_get_ui(dinfo.current_offset), &dinfo.current_data);    
        
    free_init_tree(f);         
    u = FAILURE;   
  } else {
    g95_insert_bbt(&dinfo.root, f, compare_init_tree);          
          
    join_nodes(st);       
    join_nodes(e);
    u = SUCCESS;    
  }        
        
  return u;   
}         
         
         
         
         
/* add_element()-- Given a tree node and a starting offset, insert the
 * node into the tree. */   
   
static try add_element(g95_symbol *bottom, g95_typespec *ts) {     
g95_expr *e;    
tree value; 
 
  value = next_value(&e);        
  if (value == NULL_TREE) return FAILURE;          
          
  if (bottom != dinfo.current_symbol) return SUCCESS;    
    
  value = type_check(value, e, ts);   
  if (value == NULL_TREE) return FAILURE;      
      
  return add_value(value);    
}      
      
      
        
        
/* constant_initializer()-- Generate an array constructor that comes
 * from a single constant. */

static tree constant_initializer(variable_info *vin) {         
mpz_t siz;   
g95_se se1;    
int w; 
 
  g95_conv_constant(&se1, vin->value);     
     
  if (STRING_P(se1.expr))     
    se1.expr = g95_resize_string_constant(se1.expr, dinfo.ac_string_length);     
     
  if (g95_array_spec_size(vin->as, &siz) == FAILURE)      
    g95_internal_error("constant_initializer(): Can't get array size"); 
 
  mpz_set_ui(dinfo.current_offset, dinfo.initial_offset);         
  w = int_size_in_bytes(TREE_TYPE(se1.expr)); 
 
  while(mpz_sgn(siz) > 0) {         
    add_value(se1.expr);
    mpz_sub_ui(siz, siz, 1);
    mpz_add_ui(dinfo.current_offset, dinfo.current_offset, w);    
  }     
     
  mpz_clear(siz);      
  return NULL_TREE;       
}        
        
        
       
       
/* g95_generate_data()-- Given a symbol not in a common, generate an
 * initializer from the DATA statements that mention it. */   
   
tree g95_generate_data(g95_symbol *symb) {       
       
  push_data();         
  generate_data(symb, 0);         
         
  return g95_data_initializer(symb->as == NULL);        
}     
     
     
        
        
/* expand_section()-- Expand an array section. */          
          
static try expand_section(g95_symbol *base, g95_typespec *ts) {          
dimen *y, ix[G95_MAX_DIMENSIONS];         
int r, rank, element_size;    
mpz_t offset0;       
try v;           
          
  mpz_init(offset0);      
  rank = dinfo.as_save->rank; 
  v = FAILURE;   
   
  element_size = int_size_in_bytes(g95_typenode_for_spec(ts));       
       
  for(r=0; r<rank; r++)     
    init_dimen(&ix[r]);       
       
  for(r=0; r<rank; r++)     
    if (init_loop(r, &ix[r]) == FAILURE) goto cleanup;          
          
  for(r=0; r<rank; r++) {  
    y = &ix[r];      
    if (init_loop(r, y) == FAILURE) goto cleanup;

    /* Calculate the derived quantities */        
        
    mpz_sub(y->extent,  
	    dinfo.as_save->upper[r]->value.integer,          
	    dinfo.as_save->lower[r]->value.integer);          
    mpz_add_ui(y->extent, y->extent, 1);      
      
    if (mpz_sgn(y->extent) < 0) mpz_set_ui(y->extent, 0);         
         
    mpz_sub(y->trip, y->end, y->start); 
    mpz_add(y->trip, y->trip, y->step);
    mpz_div(y->trip, y->trip, y->step);    
    
    mpz_set(y->value, y->start);     
  }      
      
/* See if there is nothing to do */  
  
  for(r=0; r<rank; r++)       
    if (mpz_sgn(ix[r].trip) <= 0) {         
      v = SUCCESS;         
      goto cleanup;      
    }     
     
/* Now start looping over the whole mess */      
      
  for(;;) {    
    mpz_set_ui(offset0, 0);    /* Calculate the offset of the current entry */

    for(r=rank-1; r>=0; r--) {         
      mpz_mul(offset0, offset0, ix[r].extent);     
     
      mpz_add(offset0, offset0, ix[r].value);
      mpz_sub(offset0, offset0, dinfo.as_save->lower[r]->value.integer);    
    } 
 
    mpz_mul_ui(offset0, offset0, element_size);   
    mpz_add(dinfo.current_offset, dinfo.current_offset, offset0);
    if (add_element(base, ts) == FAILURE) goto cleanup;     
    mpz_sub(dinfo.current_offset, dinfo.current_offset, offset0);    
    
    /* Calculate the next loop index */         
         
    r = 0;     
    for(;;) {      
      mpz_add(ix[r].value, ix[r].value, ix[r].step);          
      mpz_add_ui(ix[r].count, ix[r].count, 1);      
      
      if (mpz_cmp(ix[r].count, ix[r].trip) < 0) break;    
    
      mpz_set_ui(ix[r].count, 0);          
      mpz_set(ix[r].value, ix[r].start); 
 
      if (++r >= rank) {       
	v = SUCCESS; 
	goto cleanup;   
      }  
    }   
  }   
   
cleanup:        
  for(r=0; r<rank; r++)          
    clear_dimen(&ix[r]);   
   
  mpz_clear(offset0); 
 
  return v;          
} 
 
 


/* build_array_init1()-- Recursive function for building the elements
 * of an array constructor.  Traversal is right to left so that the
 * final list is in order of ascending elements. */       
       
static void build_array_init1(init_tree *b) {        
        
  if (b != NULL) {  
    build_array_init1(b->right);   
    build_array_init0(b);      
    build_array_init1(b->left);     
  }         
}


      
      
/* expand_data_var0()-- At this point we have an expression node that is
 * to accept the next data value.  This can be a scalar variable or a
 * an array variable (full or section).  If it is an array variable,
 * we have to loop over the relevant section. */          
          
static try expand_data_var0(g95_expr *b) {    
g95_array_spec *as;  
g95_typespec *typesp;   
int substring;       
g95_ref *reference;     
g95_expr *y;  
try v;          
          
  v = FAILURE;         
  y = g95_copy_expr(b);      
  if (g95_simplify_expr(y, 1) == FAILURE) return FAILURE;     
     
  mpz_set_ui(dinfo.current_offset, dinfo.initial_offset);       
       
  dinfo.as_save = NULL;     
  dinfo.ar_save = NULL;
  dinfo.current_data = b->where; 
 
  as = y->symbol->as;          
  typesp = &y->ts;    
  substring = 0;    
    
  for(reference=y->ref; reference; reference=reference->next) {         
    substring = 0;     
     
    switch(reference->type) {
    case REF_ARRAY:   
      if (expand_array_ref(reference, as, typesp) == FAILURE) goto done;     
      break;      
      
    case REF_COMPONENT:   
      mpz_add_ui(dinfo.current_offset, dinfo.current_offset,          
		 reference->u.c.component->offset);  
      typesp = &reference->u.c.component->ts;   
      break;       
       
    case REF_SUBSTRING:     
      if (expand_string_ref(reference, typesp) == FAILURE) goto done;          
      substring = 1;  
      break;      
    }         
  }    
    
  if (typesp->type == BT_CHARACTER && !substring) {
    b = typesp->cl->length;    
    if (b->type != EXPR_CONSTANT) {      
      g95_error("String length at %L must be constant", &b->where);       
      goto done;     
    }

    mpz_set(dinfo.string_length, b->value.integer);       
  }   
   
  v = (dinfo.as_save == NULL)    
    ? add_element(y->symbol, typesp)  
    : expand_section(y->symbol, typesp);      
      
done:         
  g95_free_expr(y);    
  return v;          
}      
      
      
          
          
/* expand_data_var()-- Expand a single data variable node.  The two
 * cases here are data list with implied DO loops or without the DO
 * loops.  In either case, we pass the expansion onwards. */

static try expand_data_var(g95_data_variable *var) {  
g95_iterator iterator;         
try s;    
    
  if (var->expr != NULL) return expand_data_var0(var->expr);   
   
  /* Translate a list with loops.  First copy the iterator to a
   * local copy, and simplify it with any current values. */         
         
  iterator = var->iter;       
  iterator.start = g95_copy_expr(iterator.start);         
  iterator.end   = g95_copy_expr(iterator.end);     
  iterator.step  = g95_copy_expr(iterator.step);

  s = FAILURE; 
 
  if (g95_simplify_expr(iterator.start, 1) == FAILURE) goto cleanup;     
  if (g95_simplify_expr(iterator.end,   1) == FAILURE) goto cleanup;     
  if (g95_simplify_expr(iterator.step,  1) == FAILURE) goto cleanup;  
  
  s = g95_expand_iterator(&iterator, (void *) expand_list, var->list);     
     
cleanup:
  g95_free_expr(iterator.start);    
  g95_free_expr(iterator.end);        
  g95_free_expr(iterator.step);          
          
  return s;         
}    
    
    


/* build_array_init()-- At this point, all elements in the tree are of
 * the same type and we are building an array constructor. */   
   
static tree build_array_init(void) {        
init_tree *s;    
tree decl;         
         
  dinfo.array_list = NULL; 
  build_array_init1(dinfo.root);     
     
  /* Find the maximum array element */          
          
  s = dinfo.root;       
  while(s->right != NULL) 
    s = s->right;         
         
  decl = build_int_2(element_number(s->end), 0);    
    
  decl = build_index_type(decl);      
  decl = build_array_type(init_tree_type(dinfo.root->head), decl);    
  decl = build(CONSTRUCTOR, decl, NULL_TREE, dinfo.array_list);    
    
  return decl;        
} 
 
 
          
          
/* array_initializer()-- Take a single array initialization
 * and add it to the tree.  Create an array constructor from an
 * initialization expression.  This only handles constant arrays. */          
          
static void array_initializer(variable_info *vin, int off) {        
        
  dinfo.ac_count = 0;         
  dinfo.ac_string_length = (vin->ts.type == BT_CHARACTER)   
    ? vin->ts.cl->backend_decl   
    : NULL_TREE;          
          
  mpz_init(dinfo.current_offset);        
        
  switch(vin->value->type) {  
  case EXPR_CONSTANT:    
    constant_initializer(vin);      
    break;        
        
  case EXPR_ARRAY:    
    g95_expand_data_constructor(vin->value);          
    break; 
 
  default:        
    g95_internal_error("array_initializer(): Bad expression");       
  }         
         
  mpz_clear(dinfo.current_offset); 
}          
          
          
/* scalar_initializer()-- Place the initialization value of a scalar
 * symbol into a common block. */  
  
static void scalar_initializer(g95_symbol *sy, int off) {    
g95_se se1;      
tree length;  
  
  if (sy->value == NULL) return;

  dinfo.current_data = sy->declared_at;         
         
  g95_init_se(&se1, NULL);      
  g95_conv_constant(&se1, sy->value);       
       
  if (STRING_P(se1.expr)) {      
    length = sy->ts.cl->backend_decl;    
    se1.expr = g95_resize_string_constant(se1.expr, length); 
  }  
  
  mpz_init_set_ui(dinfo.current_offset, off);         
  add_value(se1.expr);   
  mpz_clear(dinfo.current_offset);      
}  
  
  
/* g95_init_common_var()-- Set the initial value of a variable within
 * a common block, be it in a DATA statement or with an initial value. */ 
 
void g95_init_common_var(g95_symbol *sy, int off) { 
variable_info vin;

  if (sy->value == NULL)        
    generate_data(sy, off);       
  else {       
    if (sy->as == NULL)          
      scalar_initializer(sy, off);          
    else {   
      g95_symbol_vinfo(sy, &vin);        
      array_initializer(&vin, off);          
    } 
  }     
}   
   
   
          
          
/* build_initializer()-- Given a tree created by calls to
 * g95_generate_data(), build a backend initializer.  For scalar
 * variables, this is just a constant value.  For arrays, we build an
 * array constructor if possible, or failing that a structure and the
 * corresponding constructor.  Returns NULL if something went wrong
 * with the DATA statements that built the tree. */    
    
static tree build_initializer(int scalar) {  
  
  if (dinfo.error_flag) {  
    dinfo.error_flag = 0;    
    return NULL;     
  }       
       
  if (dinfo.root == NULL) return NULL_TREE;     
     
  /* Return a scalar initializer */      
      
  if (scalar) return dinfo.root->head;   
   
  return same_type() ? build_array_init() : build_structure_init();          
}  
  
  
  
  
/* add_array_element()-- Add a new array element to the tree.
 * Elements are always added in sequence. */    
    
static void add_array_element(tree value) {   
   
  mpz_set_ui(dinfo.current_offset,  
	     dinfo.ac_count*int_size_in_bytes(TREE_TYPE(value)));     
  add_value(value);          
  dinfo.ac_count++;        
}     
     
     
          
          
/* g95_start_common()-- Get ready to build a common block. */       
       
void g95_start_common(void) {   
   
  push_data(); 
}


          
          
/* g95_data_initializer()-- Return a constructor for the current
 * object, which can be a wide variety of things. */ 
 
tree g95_data_initializer(int scalar) {       
tree declr;

  declr = build_initializer(scalar);    
    
  free_init_tree(dinfo.root);       
  dinfo.root = NULL;        
  dinfo.error_flag = 0;         
         
  pop_data();
  return declr;   
} 
 
 
  
  
/* g95_conv_array_initializer()-- Main entry point for converting a
 * single array initializer to an initialized block.  This subroutine
 * doesn't have anything to do with DATA statements, but laying out
 * the constructor uses the same machinery. */         
         
tree g95_conv_array_initializer(variable_info *v) {         
         
  push_data();          
  array_initializer(v, 0);      
      
  return g95_data_initializer(0); 
}         
      
      
/* g95_expand_ac_element()-- Convert a constant element of a
 * constructor, adding it to the current list. */          
          
try g95_expand_ac_element(g95_expr *y) {       
g95_se s;    
    
  g95_init_se(&s, NULL);      
  g95_conv_constant(&s, y); 
 
  if (STRING_P(s.expr))         
    s.expr = g95_resize_string_constant(s.expr, dinfo.ac_string_length);  
  
  add_array_element(s.expr);          
          
  g95_free_expr(y);          
  return SUCCESS; 
}  
  
  
