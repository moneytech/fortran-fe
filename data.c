      
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
         
  g95_locus current_data;        
  g95_symbol *current_symbol;    
    
  mpz_t current_offset, string_length;

  g95_array_spec *as_save;         
  g95_array_ref *ar_save;      
      
  struct data_t *previous;   
} data_t;      
      
static data_t dinfo; 
static int last_char_flag, last_right;        
static tree init_type, init_value;         
         
#define get_init_tree() g95_getmem(sizeof(init_tree))
    
    
    
    
/* compare_init_tree()-- Compare two init_tree structures.  The
 * structures are assumed not to overlap. */         
         
static int compare_init_tree(void *i, void *v) {   
init_tree *u, *q;       
       
  u = (init_tree *) i;  
  q = (init_tree *) v;          
          
  if (u->start < q->start) return -1; 
  if (u->start > q->start) return 1;         
         
  return 0;
}       
       
       
          
          
/* type_check()-- Given a value and a type, make sure the value can be
 * assigned correctly.  Converts types and in the case of strings,
 * lengths.  Return NULL if something goes wrong. */   
   
static tree type_check(tree decl, g95_expr *l, g95_typespec *t) {       
g95_typespec value_ts;   
tree type, length;    
    
  value_ts = l->ts;        
  type = g95_typenode_for_spec(t);     
     
  switch(t->type) {          
  case BT_CHARACTER:         
    if (value_ts.type != BT_CHARACTER) { 
      g95_error("Character constant required at %L", &l->where); 
      return NULL_TREE;   
    }

    length = g95_conv_mpz_to_tree(dinfo.string_length, -1);     
    decl = g95_resize_string_constant(decl, length); 
    break;    
    
  case BT_DERIVED:  
    if (l->ts.type != BT_DERIVED || l->ts.derived != value_ts.derived) {
      g95_error("Structure constructor '%s' required at %L", 
		value_ts.derived->name, &l->where);     
      return NULL_TREE;          
    }     
     
    break;    
    
  case BT_LOGICAL:    
    if (value_ts.type != BT_LOGICAL) {  
      g95_error("Logical constant required at %L", &l->where);     
      return NULL_TREE;
    } 
 
    decl = fold(convert(type, decl));     
    break;     
     
  case BT_REAL:          
  case BT_INTEGER:  
  case BT_COMPLEX:          
    if (value_ts.type != BT_REAL && value_ts.type != BT_INTEGER &&       
	value_ts.type != BT_COMPLEX) {          
      g95_error("Numeric constant required at %L", &l->where);         
      return NULL_TREE;    
    }  
  
    decl = fold(convert(type, decl));
    break;      
      
  default: 
    g95_internal_error("type_check(): Bad type");   
  }     
     
  return decl;   
}     
     
     
        
        
/* clear_dimen()-- Clear a dimen structure. */          
          
static void clear_dimen(dimen *w) {     
     
  mpz_clear(w->start);    
  mpz_clear(w->end);       
  mpz_clear(w->step);     
  mpz_clear(w->extent);          
  mpz_clear(w->value);
  mpz_clear(w->trip);     
  mpz_clear(w->count);          
}          
          
          
          
          
/* add_struture_field()-- Given a value, add it to the structure being
 * built. */ 
 
static void add_structure_field(tree value) {  
tree field;    
    
  field = build_decl(FIELD_DECL, NULL_TREE, TREE_TYPE(value));      
  DECL_CONTEXT(field) = init_type;       
  DECL_PACKED(field) = 1;

  TYPE_FIELDS(init_type) = chainon(TYPE_FIELDS(init_type), field);        
        
  init_value = chainon(init_value, tree_cons(field, value, NULL_TREE));        
}  
  
  
   
   
/* check_overlap()-- Make sure that this init_tree structure does not
 * overlap with any other structure in the tree.  Returns nonzero if
 * an overlap is detected. */

static int check_overlap(init_tree *n1, init_tree *current) {   
   
  if (current == NULL) return 0;    
    
  if (n1->start == current->start) return 1;       
       
  if (n1->start < current->start) {       
    if (n1->end > current->start) return 1;    
    current = current->left;     
  } else {         
    if (n1->start < current->end) return 1;       
    current = current->right;   
  }       
       
  return check_overlap(n1, current); 
}        
        
        


/* expand_array_ref()-- Accumulate the offset of an array element into
 * the current offset.  Saves array sections to be expanded later.
 * Returns FAILURE if something goes wrong. */        
        
static try expand_array_ref(g95_ref *r, g95_array_spec *as,     
			    g95_typespec *ts) {       
int n, section, element_size;         
mpz_t acc;   
   
  element_size = int_size_in_bytes(g95_typenode_for_spec(ts));

  if (as == NULL) {       
    g95_error("Array reference follows scalar object at %L", &r->where);          
    return FAILURE;   
  }      
      
  section = (r->u.ar.type == AR_FULL);          
  if (!section)   
    for(n=0; n<as->rank; n++)   
      if (r->u.ar.dimen_type[n] == DIMEN_RANGE) section = 1;        
        
  /* For an element, calculate the offset.  Save a section for later
   * expansion. */          
          
  if (section) {     
    if (dinfo.as_save != NULL) {     
      g95_error("Multiple array references in expression at %L",          
		&r->where);          
      return FAILURE;     
    }         
         
    dinfo.ar_save = &r->u.ar;   
    dinfo.as_save = as;     
  } else {      
    n = g95_element_number(&r->u.ar, as);          
    if (n < 0) {          
      g95_error("Array reference at %L is outside of array", &r->where);       
      return FAILURE; 
    } 
 
    mpz_init_set_ui(acc, element_size);    
    mpz_mul_ui(acc, acc, n);        
    mpz_add(dinfo.current_offset, dinfo.current_offset, acc);          
    mpz_clear(acc); 
  }   
   
  return SUCCESS;         
}  
  
  
   
   
/* init_loop()-- Initialize each dimension of an array section.
 * Dimensions that are single elements are replaced with an equivalent
 * single-trip loop. */          
          
static try init_loop(int b, dimen *f) { 
 
  if (dinfo.ar_save->type == AR_FULL) {          
    mpz_set(f->start, dinfo.as_save->lower[b]->value.integer);    
    mpz_set(f->end,   dinfo.as_save->upper[b]->value.integer);   
    mpz_set_ui(f->step, 1);  
    return SUCCESS;    
  }     
     
  switch(dinfo.ar_save->dimen_type[b]) {    
  case DIMEN_ELEMENT:         
    if (dinfo.ar_save->start[b]->type != EXPR_CONSTANT) {    
      g95_error("Array element in DATA not constant at %L",  
		&dinfo.ar_save->start[b]->where);  
      return FAILURE;
    }         
         
    mpz_set(f->start, dinfo.ar_save->start[b]->value.integer);   
    mpz_set(f->end,   f->start);      
    mpz_set_ui(f->step, 1);     
    break;

  case DIMEN_RANGE:     
    if (dinfo.ar_save->start[b] == NULL)          
      mpz_set(f->start, dinfo.as_save->lower[b]->value.integer);  
    else {   
      if (dinfo.ar_save->start[b]->type != EXPR_CONSTANT) {      
	g95_error("Array range start in DATA not constant at %L",          
		  &dinfo.ar_save->start[b]->where);          
	return FAILURE;  
      }        
        
      mpz_set(f->start, dinfo.ar_save->start[b]->value.integer);      
    }         
         
    if (dinfo.ar_save->end[b] == NULL)       
      mpz_set(f->end, dinfo.as_save->upper[b]->value.integer);    
    else {          
      if (dinfo.ar_save->end[b]->type != EXPR_CONSTANT) {
	g95_error("Array range end in DATA not constant at %L",
		  &dinfo.ar_save->end[b]->where);         
	return FAILURE;   
      }          
          
      mpz_set(f->end, dinfo.ar_save->end[b]->value.integer); 
    }

    if (dinfo.ar_save->stride[b] == NULL)     
      mpz_set_ui(f->step, 1);      
    else {  
      if (dinfo.ar_save->stride[b]->type != EXPR_CONSTANT) {     
	g95_error("Array range step in DATA not constant at %L",     
		  &dinfo.ar_save->stride[b]->where);     
	return FAILURE;          
      }     
     
      mpz_set(f->step,  dinfo.ar_save->stride[b]->value.integer);   
    }   
   
    break;   
   
  case DIMEN_VECTOR:        
    g95_error("Vector subscripts not allowed in DATA statement at %L",      
	      &dinfo.ar_save->start[b]->where);        
    return FAILURE; 
 
  case DIMEN_UNKNOWN:     
    g95_internal_error("init_dimen(): bad dimension type");  
  }       
       
  return SUCCESS;        
}       
       
       
    
    
/* init_tree_type()-- Return the type of an init_tree, examining the
 * innards of a list node if necessary. */   
   
static tree init_tree_type(tree declr) { 
 
  if (TREE_CODE(declr) == TREE_LIST) declr = TREE_VALUE(declr); 
  return TREE_TYPE(declr);  
}         
         
         
       
       
/* pop_data()-- Pop a data node */

static void pop_data(void) {     
data_t *y;     
     
  y = dinfo.previous;
  dinfo = *y;        
        
  g95_free(y);   
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
       
       
     
     
/* push_data()-- Push a data node */      
      
static void push_data(void) {     
data_t *n;      
      
  n = g95_getmem(sizeof(dinfo));          
  *n = dinfo;         
         
  memset(&dinfo, '\0', sizeof(dinfo));  
  dinfo.previous = n; 
}        
        
        
          
          
/* expand_substring_ref()-- Accumulate offset from a substring reference.
 * Also sets string_length to the correct value */  
  
static try expand_substring_ref(g95_ref *re, g95_typespec *typ) {
mpz_t sta, end;       
g95_expr *l; 
try s;          
          
  s = FAILURE;       
  mpz_init(sta);          
  mpz_init(end);         
         
  l = re->u.ss.start;  
  if (l == NULL)          
    mpz_init_set_ui(sta, 1);          
  else {         
    if (l->type != EXPR_CONSTANT) { 
      g95_error("Substring start value at %L must be constant", &l->where);        
      goto done;  
    }      
      
    mpz_init_set(sta, l->value.integer);     
  } 
 
  l = re->u.ss.end;
  if (l == NULL) l = typ->cl->length;

  if (l->type != EXPR_CONSTANT) {    
    g95_error("Substring end value at %L must be constant", &l->where);
    goto done;  
  }        
        
  mpz_init_set(end, l->value.integer);      
      
  mpz_add(dinfo.current_offset, dinfo.current_offset, sta);          
  mpz_sub_ui(dinfo.current_offset, dinfo.current_offset, 1);   
   
  mpz_sub(dinfo.string_length, end, sta);         
  mpz_add_ui(dinfo.string_length, dinfo.string_length, 1);       
       
  if (mpz_cmp_ui(dinfo.string_length, 0) < 0)
    mpz_set_ui(dinfo.string_length, 0);          
          
  s = SUCCESS;       
       
done:        
  mpz_clear(sta); 
  mpz_clear(end);         
  return s;          
}         
         
         


/* has_current_symbol()-- Checks to see if the data node involves the
 * current symbol.  Returns nonzero if this is so. */          
          
static int has_current_symbol(g95_data_variable *var0) {   
   
  if (var0 == NULL) return 0; 
 
  return (var0->expr != NULL && var0->expr->symbol == dinfo.current_symbol) ||         
    has_current_symbol(var0->list) ||         
    has_current_symbol(var0->next);     
}    
    
    
       
       
/* free_init_tree()-- Recursively free the entire tree */      
      
static void free_init_tree(init_tree *z) {  
  
  if (z == NULL) return; 
  free_init_tree(z->left); 
  free_init_tree(z->right);     
     
  g95_free(z);      
}     
     
     
  
  
/* find_end()-- Recursively find a node with a particular end value.
 * Returns NULL if no such node exists. */

static init_tree *find_end(init_tree *p, int end) {     
     
  if (p == NULL) return NULL;         
         
  if (p->end == end) return p;  
  
  return find_end(p->end < end ? p->right : p->left, end);      
}        
        
        
          
          
/* find_start()-- Recursively find a node with a particular start
 * value.  Returns NULL if no such node exists. */   
   
static init_tree *find_start(init_tree *v, int begin) {      
      
  if (v == NULL) return NULL;       
       
  if (v->start == begin) return v;        
        
  return find_start(v->start < begin ? v->right : v->left, begin);        
}      
      
      
      
      
/* add_structure()-- Add the current init_tree to the current
 * structure constructor.  We also consider any holes between this
 * init_tree and the previous offset.  We initialize with a character
 * array of spaces if this or the last node is a character string.
 * Otherwise, we initialize with a character array of zeroes. */ 
 
static void add_structure(init_tree *c) {          
int w, char_flag;         
tree tmp, typ;       
char *i;     
     
  w = c->start - last_right;   
   
  typ = (TREE_CODE(c->head) == TREE_LIST)   
    ? TREE_TYPE(TREE_VALUE(c->head))   
    : TREE_TYPE(c->head);        
        
  if (TREE_CODE(typ) == ARRAY_TYPE) typ = TREE_TYPE(typ);        
        
  char_flag = (typ == g95_character1_type_node);       
       
  if (w > 0) {    /* Build some padding */    
    tmp = build_int_2(w, 0);          
    tmp = build_range_type(g95_default_integer, integer_one_node, tmp);  
    typ = build_array_type(g95_character1_type_node, tmp);   
   
    i = g95_getmem(w);        
    memset(i, (char_flag || last_char_flag) ? ' ' : '\0', w);   
   
    tmp = g95_build_string_const(w, i);   
    add_structure_field(tmp);  
  }    
    
  last_right = c->end;          
  last_char_flag = char_flag;    
    
  if (TREE_CODE(c->head) != TREE_LIST)     
    add_structure_field(c->head);         
  else {      
    w = list_length(c->head);       
    typ = TREE_TYPE(TREE_VALUE(c->head));         
         
    tmp = build_int_2(w, 0);      
    tmp = build_range_type(g95_default_integer, integer_one_node, tmp);      
    typ = build_array_type(typ, tmp);        
        
    tmp = build(CONSTRUCTOR, typ, NULL_TREE, c->head);      
    add_structure_field(tmp);       
  }       
}          
          
          
    
    
/* structure_traverse()-- Traverse the init_tree building a structure
 * constructor. */  
  
static void structure_traverse(init_tree *o) {

  if (o != NULL) { 
    structure_traverse(o->left);     
    add_structure(o);      
    structure_traverse(o->right);  
  } 
}   
   
   
          
          
/* expand_list()-- Expand a list of data items for one set of
 * iteration values.  Mutually recursive with expand_data_var(). */        
        
static try expand_list(g95_data_variable *list) {       
       
  for(; list!=NULL; list=list->next)    
    if (expand_data_var(list) == FAILURE) return FAILURE;  
  
  return SUCCESS; 
}        
        
        


/* g95_start_common()-- Get ready to build a common block. */      
      
void g95_start_common(void) {     
     
  push_data();      
} 
 
 
 
 
/* join_nodes()-- Given a position in the tree, find the nodes on the
 * left and right and concatenate them into a single constructor list
 * if they are of the same type. */          
          
static void join_nodes(int n) {    
init_tree *left, *right; 
 
  left  = find_end(dinfo.root, n);       
  right = find_start(dinfo.root, n);        
        
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
int sta, stop;  
init_tree *h; 
try y;    
    
  h = get_init_tree();        
  h->head = value; 
 
  h->start = sta = mpz_get_ui(dinfo.current_offset);  
  h->end = stop = sta + int_size_in_bytes(TREE_TYPE(value));        
        
  if (check_overlap(h, dinfo.root)) {          
    g95_error("Memory is initialized more than once (offset %d) at %L",  
	      mpz_get_ui(dinfo.current_offset), &dinfo.current_data); 
     
    free_init_tree(h);      
    y = FAILURE;
  } else {       
    g95_insert_bbt(&dinfo.root, h, compare_init_tree);   
   
    join_nodes(sta);      
    join_nodes(stop);         
    y = SUCCESS;         
  }          
          
  return y;      
}          
          
          
    
    
/* init_dimen()-- Initialize a dimen structure. */   
   
static void init_dimen(dimen *d) {    
    
  mpz_init(d->start);     
  mpz_init(d->end);        
  mpz_init(d->step);     
  mpz_init(d->extent);   
  mpz_init(d->value);   
  mpz_init(d->trip);   
  mpz_init_set_ui(d->count, 0);         
}         
         
         
     
     
/* constant_initializer()-- Generate an array constructor that comes
 * from a single constant. */   
   
static tree constant_initializer(variable_info *vin) {         
mpz_t siz;        
g95_se se1;     
int t;   
   
  g95_init_se(&se1, NULL);         
  g95_conv_constant(&se1, vin->value);

  if (STRING_P(se1.expr))        
    se1.expr = g95_resize_string_constant(se1.expr, dinfo.ac_string_length); 
 
  if (g95_array_spec_size(vin->as, &siz) == FAILURE)          
    g95_internal_error("constant_initializer(): Can't get array size");          
          
  mpz_set_ui(dinfo.current_offset, dinfo.initial_offset);          
  t = int_size_in_bytes(TREE_TYPE(se1.expr));      
      
  while(mpz_sgn(siz) > 0) {    
    add_value(se1.expr);       
    mpz_sub_ui(siz, siz, 1);
    mpz_add_ui(dinfo.current_offset, dinfo.current_offset, t);     
  }         
         
  mpz_clear(siz);      
  return NULL_TREE;        
}   
   
   
  
  
/* add_element()-- Given a tree node and a starting offset, insert the
 * node into the tree. */       
       
static try add_element(g95_symbol *bottom, g95_typespec *t) {
g95_expr *c;         
tree value;  
  
  value = next_value(&c);      
  if (value == NULL_TREE) return FAILURE;          
          
  if (bottom != dinfo.current_symbol) return SUCCESS;       
       
  value = type_check(value, c, t);          
  if (value == NULL_TREE) return FAILURE;  
  
  return add_value(value);          
}  
  
  
          
          
/* expand_data()-- Translate a single DATA statement.  We traverse the
 * variable and constant lists, matching the two as we go.  The
 * implicit loops within data statements are used as are the repeat
 * counts of the constants. */

static try expand_data(g95_data *h) {    
g95_data_variable *var;        
try j;

  dinfo.repeat_count = 0;    
  dinfo.current_value = h->value;    
  dinfo.current_data = h->where;       
       
  mpz_init(dinfo.current_offset);   
  mpz_init(dinfo.string_length);      
  j = FAILURE;        
        
  for(var=h->var; var; var=var->next)  
    if (expand_data_var(var) == FAILURE) goto done; 
 
  /* We're done.  Make sure that the list of data items is also empty */         
         
  if (dinfo.current_value != NULL) {  
    g95_error("DATA statement at %L has more values that variables",         
	      &dinfo.current_value->expr->where);          
    goto done;     
  }          
          
  j = SUCCESS;    
    
done:      
  mpz_clear(dinfo.current_offset);      
  mpz_clear(dinfo.string_length);         
  return j;
}     
     
     
    
    
/* expand_section()-- Expand an array section. */ 
 
static try expand_section(g95_symbol *bottom, g95_typespec *typ) { 
dimen *p, index[G95_MAX_DIMENSIONS];  
int u, rnk, element_size;  
mpz_t offset0;
try h;       
      
  mpz_init(offset0);    
  rnk = dinfo.as_save->rank;
  h = FAILURE;       
       
  element_size = int_size_in_bytes(g95_typenode_for_spec(typ)); 
 
  for(u=0; u<rnk; u++)     
    init_dimen(&index[u]);    
    
  for(u=0; u<rnk; u++)      
    if (init_loop(u, &index[u]) == FAILURE) goto cleanup;  
  
  for(u=0; u<rnk; u++) {     
    p = &index[u]; 
    if (init_loop(u, p) == FAILURE) goto cleanup; 
 
    /* Calculate the derived quantities */      
      
    mpz_sub(p->extent,     
	    dinfo.as_save->upper[u]->value.integer,         
	    dinfo.as_save->lower[u]->value.integer);   
    mpz_add_ui(p->extent, p->extent, 1);    
    
    if (mpz_sgn(p->extent) < 0) mpz_set_ui(p->extent, 0);   
   
    mpz_sub(p->trip, p->end, p->start);     
    mpz_add(p->trip, p->trip, p->step);    
    mpz_div(p->trip, p->trip, p->step); 
 
    mpz_set(p->value, p->start);      
  }        
        
/* See if there is nothing to do */  
  
  for(u=0; u<rnk; u++)    
    if (mpz_sgn(index[u].trip) <= 0) {     
      h = SUCCESS;     
      goto cleanup;  
    }    
    
/* Now start looping over the whole mess */     
     
  for(;;) {   
    mpz_set_ui(offset0, 0);    /* Calculate the offset of the current entry */   
   
    for(u=rnk-1; u>=0; u--) {          
      mpz_mul(offset0, offset0, index[u].extent);

      mpz_add(offset0, offset0, index[u].value);  
      mpz_sub(offset0, offset0, dinfo.as_save->lower[u]->value.integer);     
    }       
       
    mpz_mul_ui(offset0, offset0, element_size);       
    mpz_add(dinfo.current_offset, dinfo.current_offset, offset0);        
    if (add_element(bottom, typ) == FAILURE) goto cleanup;     
    mpz_sub(dinfo.current_offset, dinfo.current_offset, offset0);     
     
    /* Calculate the next loop index */   
   
    u = 0;        
    for(;;) {     
      mpz_add(index[u].value, index[u].value, index[u].step);
      mpz_add_ui(index[u].count, index[u].count, 1);          
          
      if (mpz_cmp(index[u].count, index[u].trip) < 0) break; 
 
      mpz_set_ui(index[u].count, 0);      
      mpz_set(index[u].value, index[u].start);          
          
      if (++u >= rnk) {   
	h = SUCCESS;       
	goto cleanup;          
      }  
    }     
  }          
          
cleanup:        
  for(u=0; u<rnk; u++)         
    clear_dimen(&index[u]);       
       
  mpz_clear(offset0);   
   
  return h;      
}        
        
        
        
        
/* generate_data()-- Process the DATA statements in a program
 * unit for a particular symbol.  This subroutine can be called
 * multiple times when initializing a COMMON block. */         
         
static void generate_data(g95_symbol *sy, int off) {
g95_data *l;   
   
  if (dinfo.error_flag) return;      
      
  dinfo.current_symbol = sy;       
  dinfo.initial_offset = off;         
         
  for(l=sy->ns->data; l; l=l->next) {       
    if (!has_current_symbol(l->var)) continue;      
    if (expand_data(l) == FAILURE) {  
      dinfo.error_flag = 1; 
      break;       
    } 
  }
}    
    
    
         
         
/* g95_generate_data()-- Given a symbol not in a common, generate an
 * initializer from the DATA statements that mention it. */  
  
tree g95_generate_data(g95_symbol *symb) { 
 
  push_data();        
  generate_data(symb, 0);

  return g95_data_initializer(symb->as == NULL);         
}          
          
          
        
        
/* add_array_element()-- Add a new array element to the tree.
 * Elements are always added in sequence. */   
   
static void add_array_element(tree value) {     
     
  mpz_set_ui(dinfo.current_offset,   
	     dinfo.ac_count*int_size_in_bytes(TREE_TYPE(value)));       
  add_value(value); 
  dinfo.ac_count++;      
}      
      
      
 
 
/* expand_data_var0()-- At this point we have an expression node that is
 * to accept the next data value.  This can be a scalar variable or a
 * an array variable (full or section).  If it is an array variable,
 * we have to loop over the relevant section. */         
         
static try expand_data_var0(g95_expr *p) {      
g95_array_spec *ar;        
g95_typespec *ts;        
int substring;       
g95_ref *re;      
g95_expr *m;    
try g;       
       
  g = FAILURE;       
  m = g95_copy_expr(p);         
  if (g95_simplify_expr(m, 1) == FAILURE) return FAILURE;      
      
  mpz_set_ui(dinfo.current_offset, dinfo.initial_offset);     
     
  dinfo.as_save = NULL;         
  dinfo.ar_save = NULL;     
  dinfo.current_data = p->where;        
        
  ar = m->symbol->as;       
  ts = &m->ts;         
  substring = 0;         
         
  for(re=m->ref; re; re=re->next) { 
    substring = 0;

    switch(re->type) {          
    case REF_ARRAY:          
      if (expand_array_ref(re, ar, ts) == FAILURE) goto done;         
      break;   
   
    case REF_COMPONENT:      
      mpz_add_ui(dinfo.current_offset, dinfo.current_offset,       
		 re->u.c.component->offset);  
      ts = &re->u.c.component->ts; 
      break;    
    
    case REF_SUBSTRING:  
      if (expand_substring_ref(re, ts) == FAILURE) goto done;         
      substring = 1;  
      break;   
    }       
  }         
         
  if (ts->type == BT_CHARACTER && !substring) {      
    p = ts->cl->length; 
    if (p->type != EXPR_CONSTANT) {
      g95_error("String length at %L must be constant", &p->where);      
      goto done;          
    }          
          
    mpz_set(dinfo.string_length, p->value.integer);        
  }         
         
  g = (dinfo.as_save == NULL)     
    ? add_element(m->symbol, ts) 
    : expand_section(m->symbol, ts);

done:       
  g95_free_expr(m);  
  return g; 
}  
  
  
          
          
/* array_initializer()-- Take a single array initialization
 * and add it to the tree.  Create an array constructor from an
 * initialization expression.  This only handles constant arrays. */ 
 
static void array_initializer(variable_info *info, int off) {     
     
  dinfo.ac_count = 0;        
  dinfo.ac_string_length = (info->ts.type == BT_CHARACTER)      
    ? info->ts.cl->backend_decl        
    : NULL_TREE; 
 
  mpz_init(dinfo.current_offset);

  switch(info->value->type) { 
  case EXPR_CONSTANT:  
  case EXPR_STRUCTURE:
    constant_initializer(info);
    break;         
         
  case EXPR_ARRAY:       
    g95_expand_data_constructor(info->value);     
    break;    
    
  default:    
    g95_internal_error("array_initializer(): Bad expression");          
  }     
     
  mpz_clear(dinfo.current_offset);        
}          
          
          
/* scalar_initializer()-- Place the initialization value of a scalar
 * symbol into a common block. */ 
 
static void scalar_initializer(g95_symbol *sy, int off) {    
g95_se s;     
tree l;         
         
  if (sy->value == NULL) return;  
  
  dinfo.current_data = sy->declared_at;       
       
  g95_init_se(&s, NULL);        
  g95_conv_constant(&s, sy->value);

  if (STRING_P(s.expr)) {    
    l = sy->ts.cl->backend_decl; 
    s.expr = g95_resize_string_constant(s.expr, l);  
  }

  mpz_init_set_ui(dinfo.current_offset, off);     
  add_value(s.expr);  
  mpz_clear(dinfo.current_offset);       
}


/* g95_init_common_var()-- Set the initial value of a variable within
 * a common block, be it in a DATA statement or with an initial value. */ 
 
void g95_init_common_var(g95_symbol *sy, int off) { 
variable_info info;       
       
  if (sy->value == NULL)       
    generate_data(sy, off); 
  else {          
    if (sy->as == NULL)      
      scalar_initializer(sy, off);    
    else {   
      g95_symbol_vinfo(sy, &info);     
      array_initializer(&info, off);     
    }         
  }     
}          
          
          


/* same_type0()-- Recursive function for same_type() */   
   
static int same_type0(init_tree *u, tree typ) { 
 
  if (u == NULL) return 1;       
       
  return init_tree_type(u->head) == typ      
    && same_type0(u->left,  typ)        
    && same_type0(u->right, typ);       
}   
   
   
         
         
/* element_number()-- Given a pointer to an mpz_t, figure out which
 * element number this is, zero based. */        
        
static int element_number(int offs) {   
int size;         
         
  size = int_size_in_bytes(init_tree_type(dinfo.root->head));        
        
  return offs / size;        
}     
     
     
    
    
/* g95_conv_array_initializer()-- Main entry point for converting a
 * single array initializer to an initialized block.  This subroutine
 * doesn't have anything to do with DATA statements, but laying out
 * the constructor uses the same machinery. */

tree g95_conv_array_initializer(variable_info *vin) {  
  
  if (vin->value->type == EXPR_NULL)  
    return g95_build_desc_constructor(vin->as->rank);    
    
  push_data();
  array_initializer(vin, 0);  
  
  return g95_data_initializer(0);     
}   
 
 
/* g95_expand_ac_element()-- Convert a constant element of a
 * constructor, adding it to the current list. */         
         
try g95_expand_ac_element(g95_expr *m) {        
g95_se s;      
      
  g95_init_se(&s, NULL);      
  g95_conv_constant(&s, m);   
   
  if (STRING_P(s.expr))     
    s.expr = g95_resize_string_constant(s.expr, dinfo.ac_string_length);      
      
  add_array_element(s.expr);

  g95_free_expr(m);  
  return SUCCESS;    
}    
    
    
     
     
/* expand_data_var()-- Expand a single data variable node.  The two
 * cases here are data list with implied DO loops or without the DO
 * loops.  In either case, we pass the expansion onwards. */     
     
static try expand_data_var(g95_data_variable *var) {      
g95_iterator iterator;    
try t;  
  
  if (var->expr != NULL) return expand_data_var0(var->expr);          
          
  /* Translate a list with loops.  First copy the iterator to a
   * local copy, and simplify it with any current values. */       
       
  iterator = var->iter;
  iterator.start = g95_copy_expr(iterator.start);
  iterator.end   = g95_copy_expr(iterator.end);          
  iterator.step  = g95_copy_expr(iterator.step);      
      
  t = FAILURE;   
   
  if (g95_simplify_expr(iterator.start, 1) == FAILURE) goto cleanup;        
  if (g95_simplify_expr(iterator.end,   1) == FAILURE) goto cleanup;      
  if (g95_simplify_expr(iterator.step,  1) == FAILURE) goto cleanup;  
  
  t = g95_expand_iterator(&iterator, (void *) expand_list, var->list);         
         
cleanup: 
  g95_free_expr(iterator.start);     
  g95_free_expr(iterator.end);       
  g95_free_expr(iterator.step);    
    
  return t; 
}   
   
   
        
        
/* build_structure_init()-- Build a constructor for a heterogeneous
 * init_tree.  The tree is traversed from low offsets to high offsets. */      
      
static tree build_structure_init(void) {

  last_right = 0;        
  last_char_flag = 0;  
  
  init_type = make_node(RECORD_TYPE);
  init_value = NULL_TREE;   
   
  structure_traverse(dinfo.root);        
        
  g95_finish_type(init_type);   
  return build(CONSTRUCTOR, init_type, NULL_TREE, init_value);   
}


       
       
/* same_type()-- Checks to see if all elements of the tree are the
 * same type as the root node.  Return nonzero if this is so. */

static int same_type(void) {       
tree type;    
    
  type = init_tree_type(dinfo.root->head); 
  return same_type0(dinfo.root, type);   
}        
        
        


/* build_array_init0()-- Add the current init_tree to the list of
 * constructors. */        
        
static void build_array_init0(init_tree *d) {          
tree decl;      
int h;     
     
  h = element_number(d->start);        
        
  decl = d->head;
  if (TREE_CODE(decl) != TREE_LIST) {  /* Single element */
    decl = tree_cons(NULL_TREE, decl, NULL_TREE);    
    TREE_PURPOSE(decl) = build_int_2(h, 0);      
      
    TREE_CHAIN(decl) = dinfo.array_list;       
    dinfo.array_list = decl;  
  
  } else {   /* List of elements */      
    for(;;) {
      TREE_PURPOSE(decl) = build_int_2(h, 0);   
      if (TREE_CHAIN(decl) == NULL) break;       
       
      h++;   
      decl = TREE_CHAIN(decl);         
    }   
   
    TREE_CHAIN(decl) = dinfo.array_list; 
    dinfo.array_list = d->head; 
  }  
}     
     
     
          
          
/* build_array_init1()-- Recursive function for building the elements
 * of an array constructor.  Traversal is right to left so that the
 * final list is in order of ascending elements. */        
        
static void build_array_init1(init_tree *g) {      
      
  if (g != NULL) {  
    build_array_init1(g->right);    
    build_array_init0(g);
    build_array_init1(g->left);     
  }
}     
     
     
        
        
/* build_array_init()-- At this point, all elements in the tree are of
 * the same type and we are building an array constructor. */    
    
static tree build_array_init(void) {          
init_tree *n;   
tree dec;       
       
  dinfo.array_list = NULL;         
  build_array_init1(dinfo.root);  
  
  /* Find the maximum array element */        
        
  n = dinfo.root;  
  while(n->right != NULL)      
    n = n->right;

  dec = build_int_2(element_number(n->end)-1, 0);    
    
  dec = build_index_type(dec);        
  dec = build_array_type(init_tree_type(dinfo.root->head), dec); 
  dec = build(CONSTRUCTOR, dec, NULL_TREE, dinfo.array_list);

  return dec; 
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
 
  /* Return a scalar initializer.  Initialization of substrings can
   * create nontrivial trees. */    
    
  if (scalar)          
    return (dinfo.root->left == NULL && dinfo.root->right == NULL &&         
	    dinfo.root->start == 0)      
      ? dinfo.root->head  
      : build_structure_init();        
        
  return same_type() ? build_array_init() : build_structure_init(); 
}        
        
        
          
          
/* g95_data_initializer()-- Return a constructor for the current
 * object, which can be a wide variety of things. */          
          
tree g95_data_initializer(int scalar) {
tree d;        
        
  d = build_initializer(scalar);

  free_init_tree(dinfo.root);  
  dinfo.root = NULL;  
  dinfo.error_flag = 0;          
          
  pop_data();         
  return d;    
}


