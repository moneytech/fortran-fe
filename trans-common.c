          
/* Common and equivalence block handling
   Copyright (C) 2000-2003 Free Software Foundation, Inc.
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
      
      
/* Transform common blocks.  An integral part of this is processing
 * EQUIVALENCE variables.  Equivalenced variables that are not in a
 * common block end up in a private block of their own.
 *
 * Each common block is a global character array of some length.  The
 * blank common has its own name that is an otherwise inaccessible
 * name.  Variables within the block are represented as an array
 * element within the block.  While it should be possible to declare
 * the areas as a union, it seems much easier to represent things this
 * way, particularly for an initialized common (a block of bytes).
 *
 * So if two variables are equivalenced, they just point to a common
 * area in memory.
 *
 * Mathematically, laying out an equivalence block is equivalent to
 * solving a linear system of equations.  The matrix is a sparse
 * matrix in which each row contains all zero elements except for a +1
 * and a -1, a sort of a generalized Vandermonde matrix.  The matrix
 * is usually block diagonal.  The system can be overdetermined,
 * underdetermined or have a unique solution.  If the system is
 * inconsistent, the program is not standard conforming.  The solution
 * vector is integral, since all of the pivots are +1 or -1.
 *
 * How we lay out an equivalence block is a little less complicated.
 * In an equivalence list with n elements, there are n-1 conditions to
 * be satisfied.  The conditions partition the variables into what we
 * will call segments.  If A and B are equivalenced then A and B are
 * in the same segment.  If B and C are equivalenced as well, then A,
 * B and C are in a segment and so on.  Each segment is a block of
 * memory that has one or more variables equivalenced in some way.  A
 * common block is made up of a series of segments that are joined one
 * after the other.  In the linear system, a segment is a block
 * diagonal.
 *
 * To lay out a segment we first start with some variable and
 * determine its length.  The first variable is assumed to start at
 * offset one and extends to however long it is.  We then traverse the
 * list of equivalences to find an unused condition that involves at
 * least one of the variables currently in the segment.
 *
 * Each equivalence condition amounts to the condition B+b=C+c where B
 * and C are the offsets of the B and C variables, and b and c are
 * constants which are nonzero for array elements, substrings or
 * structure components.  So for
 *
 *   EQUIVALENCE(B(2), C(3))
 * we have
 *   B + 2*size of B's elements = C + 3*size of C's elements.
 *
 * If B and C are known we check to see if the condition already
 * holds.  If B is known we can solve for C.  Since we know the length
 * of C, we can see if the minimum and maximum extents of the segment
 * are affected.  Eventually, we make a full pass through the
 * equivalence list without finding any new conditions and the segment
 * is fully specified.
 *
 * At this point, the segment is added to the current common block.
 * The seed symbol in the common block determines the extent of the
 * segment, the rest of the variables are along for the ride.  The
 * usual case here is that there are no equivalence statements and the
 * common block is series of segments with one variable each, which is
 * a diagonal matrix in the matrix formulation.
 *
 * Once all common blocks have been created, the list of equivalences
 * is examined for still-unused equivalence conditions.  If these
 * exist, a variable from an unused equivalence is placed into a
 * private block in order to start a new segment, which is built as
 * usual.  This process continues until all equivalenced variables
 * have been put into a block.
 *
 * The overall process for creating common blocks is to examine all
 * common blocks within the source file, and create those common
 * blocks that are initialized.  The remaining common blocks are
 * created as uninitialied memory.  When processing program units, we
 * again loop over common blocks in the program unit.  The variables
 * within the common are then declared as offsets within the block.
 */         
         
#include "trans.h"
         
         
typedef struct segment_info {          
  g95_symbol *sym;       
  int offset, length;          
  struct segment_info *next;
  g95_equiv *rule;         
} segment_info;      
      
static segment_info *current_segment, *current_common;   
static int common_length, real_common, seen_init;       
       
static tree blank_common_decl;
static g95_locus blank_common_locus; 
static int blank_common_length, blank_common_seen=0;   
   
   
#define get_segment_info() g95_getmem(sizeof(segment_info))


      
      
/* get_mpz()-- Given an expression node, make sure it is a constant
 * integer and return the mpz_t value. */   
   
static mpz_t *get_mpz(g95_expr *n) {   
   
  if (n->type != EXPR_CONSTANT)        
    g95_internal_error("get_mpz(): Not an integer constant");        
        
  return &n->value.integer;      
} 
 
 
       
       
/* calculate_offset()-- Given a single element of an equivalence list,
 * figure out the offset from the base symbol.  For simple variables
 * or full arrays, this is simply zero.  For an array element we have
 * to calculate the array element number and multiply by the element
 * size.  For a substring we have to calculate the further reference. */     
     
static int calculate_offset(g95_expr *x) {       
int q, element_size, off;  
g95_typespec *element_type;         
g95_array_spec *a;       
g95_ref *ref;       
       
  off = 0;          
  element_type = &x->symbol->ts;    
    
  a = x->symbol->as;    
    
  for(ref=x->ref; ref; ref=ref->next)         
    switch(ref->type) {         
    case REF_ARRAY:    
      switch(ref->u.ar.type) {  
      case AR_FULL: 
	break;        
        
      case AR_ELEMENT:
	q = g95_element_number(&ref->u.ar, a);        
	element_size = int_size_in_bytes(g95_typenode_for_spec(element_type));         
         
	off += q * element_size;       
	break;

      default:     
	g95_internal_error("calculate_offset(): Bad array reference");     
      }     
            
      break;     
     
    case REF_COMPONENT:
      abort();   /* Can't happen */ 
      break; 
 
    case REF_SUBSTRING: 
      if (ref->u.ss.start != NULL)
	off += mpz_get_ui(*get_mpz(ref->u.ss.start)) - 1;

      break;    
    }  
  
  return off; 
}    
    
    
         
         
/* find_segment_info()-- Given a symbol, find it in the current list
 * segment list.  Returns NULL if not found. */  
  
static segment_info *find_segment_info(g95_symbol *symb) {     
segment_info *p;        
        
  for(p=current_segment; p; p=p->next)      
    if (p->sym == symb) return p; 
 
  return NULL;  
}      
      
      
          
          
/* seg_compare()-- Compare the offsets for two segments. */ 
 
static int seg_compare(const void *a, const void *b) {       
segment_info *i, *u;       
       
  i = *((segment_info **) a);    
  u = *((segment_info **) b);          
          
  return i->offset - u->offset; 
}     
     
     
  
  
/* equivalence_conflict()-- Complain about a symbol at two offsets */

static void equivalence_conflict(char *name0, g95_equiv *rule, int h, int t) {  
  
  g95_error("EQUIVALENCE conflict, '%s' at %L has conflicting "     
	    "offsets %d and %d", name0, &rule->expr->where, h, t);  
}


    
    
/* g95_element_number()-- Given an array specification and an array
 * reference, figure out the array element number (zero based).
 * Bounds and elements are guaranteed to be constants. */       
       
int g95_element_number(g95_array_ref *ref, g95_array_spec *a) {   
mpz_t multiplier, offs, ext, k, *index, *lower, *up;
int r, dim;          
          
  dim = a->rank;    
  mpz_init_set_ui(multiplier, 1);          
  mpz_init_set_ui(offs, 0);     
  mpz_init(ext);      
  mpz_init(k);        
        
  for(r=0; r<dim; r++) {      
    if (ref->dimen_type[r] != DIMEN_ELEMENT)  
      g95_internal_error("g95_element_number(): Bad dimension type");   
   
    lower = get_mpz(a->lower[r]);          
    up = get_mpz(a->upper[r]);
    index = get_mpz(ref->start[r]);

    mpz_sub(k, *index, *lower);  
  
    mpz_mul(k, k, multiplier); 
    mpz_add(offs, offs, k);

    mpz_sub(ext, *get_mpz(a->upper[r]), *get_mpz(a->lower[r]));          
    mpz_add_ui(ext, ext, 1);        
        
    if (mpz_sgn(ext) < 0) mpz_set_ui(ext, 0);     
     
    mpz_mul(multiplier, multiplier, ext);          
  }      
      
  r = mpz_get_ui(offs);          
          
  mpz_clear(multiplier);          
  mpz_clear(offs); 
  mpz_clear(ext);  
  mpz_clear(k);     
     
  return r;         
}


       
       
/* calculate_length()-- Given a variable symbol, calculate the total
 * length in bytes of the variable. */     
     
static int calculate_length(g95_symbol *symb) { 
int m, element_size;      
mpz_t elements;         
         
  if (symb->as != NULL && symb->attr.pointer)    
    return int_size_in_bytes(g95_get_array_desc(symb->as->rank));     
     
  if (symb->attr.pointer) return int_size_in_bytes(pchar_type_node);  
  
  element_size = int_size_in_bytes(g95_typenode_for_spec(&symb->ts));     
  if (symb->as == NULL) return element_size;          
          
  /* Calculate the number of elements in the array */   
   
  if (g95_array_spec_size(symb->as, &elements) == FAILURE)    
    g95_internal_error("calculate_length(): Unable to determine array size");

  m = mpz_get_ui(elements);         
  mpz_clear(elements);        
        
  return m*element_size;;   
}       
       
       
   
   
/* check_init()-- If the given symbol is initialized, record the fact. */      
      
static void check_init(g95_symbol *symb) {  
  
  if (symb->value != NULL || symb->attr.data)
    seen_init = 1;       
} 
 
 
       
       
/* common_identifier()-- Given a common name, return an identifier for it. */         
         
static tree common_identifier(char *nam) {        
g95_symbol s;  
  
  if (nam == NULL)    
    return get_identifier(BLANK_COMMON_NAME);          
          
  memset(&s, '\0', sizeof(s));  
  strcpy(s.name, nam);

  s.attr.flavor = FL_BLOCK_DATA;  /* A global name */  
  
  return g95_sym_identifier(&s, NULL);         
}         
         
         
 
 
/* unmark_equivalences()-- Given a namespace, mark all of the
 * equivalences as unused. */

static void unmark_equivalences(g95_namespace *names) { 
g95_equiv *v, *d;          
          
  for(v=names->equiv; v; v=v->next)      
    for(d=v; d; d=d->eq)
      d->used = 0;      
}        
        
        
          
          
/* sort_common()-- Sort the current_common list so that all of the
 * nodes are in order of ascending offsets. */       
       
static void sort_common(void) {      
segment_info *t, **w;         
int h, f;   
   
  if (current_common == NULL) return;   
   
  f = 0;   
  t = current_common;         
  while(t != NULL) {   
    t = t->next; 
    f++;      
  }       
       
  w = g95_getmem(f*sizeof(segment_info *));

  t = current_common;       
  for(h=0; h<f; h++) {  
    w[h] = t;    
    t = t->next;
  }         
         
  qsort(w, f, sizeof(segment_info *), seg_compare);       
       
  current_common = w[0];     
     
  for(h=0; h<f-1; h++)      
    w[h]->next = w[h+1];         
         
  w[f-1]->next = NULL;     
     
  g95_free(w);    
}  
  
  
          
          
/* confirm_condition()-- Given two equivalence structures that are
 * both already in the list, make sure that this new condition is not
 * violated, generating an error if it is. */      
      
static void confirm_condition(segment_info *d, g95_equiv *eq0,        
			      segment_info *w, g95_equiv *eq2) {      
int offset1, offset2;        
        
  offset1 = calculate_offset(eq0->expr);      
  offset2 = calculate_offset(eq2->expr);        
        
  if (d->offset + offset1 != w->offset + offset2)     
    equivalence_conflict(d->sym->name, eq0, d->offset + offset1,
			 w->offset + offset2);
}     
     
     
     
     
/* build_common_vars()-- Given a common block, create the declarations
 * for those variables, which consists of an offset into the common block. */

static void build_common_vars(tree block) { 
tree o, dtype, d, storage;
variable_info info;         
segment_info *w;

  block = build1(ADDR_EXPR, pchar_type_node, block);  
  
  for(w=current_common; w; w=w->next) { 
    o = build_int_2(w->offset, 0);   
   
    g95_symbol_vinfo(w->sym, &info);
    dtype = g95_get_descriptor(&info);      
      
    if (info.as == NULL || info.pointer) {        
      dtype = build_pointer_type(dtype);         
      w->sym->backend_decl = build(PLUS_EXPR, dtype, block, o);     
    } else {      
      g95_get_storage(&info);       /* Create the descriptor */         
      d = build_decl(VAR_DECL, g95_sym_identifier(w->sym, NULL), dtype);      
      info.desc = d;          
          
      if (g95_module_symbol(w->sym)) {        
	TREE_ADDRESSABLE(d) = 1;    
	TREE_STATIC(d) = 1;
      }          
          
      storage = build(PLUS_EXPR, pchar_type_node, block, o);          
      g95_init_descriptor(&info, d, storage); 
 
      w->sym->backend_decl = d;          
      pushdecl(d);    
    }          
  }        
}     
     
     
        
        
/* blank_block()-- Declare a blank character array that will hold a
 * common that is uninitialized at least in the current source file. */         
         
static tree blank_block(tree identifier, int leng) {   
tree dec, tmp1;       
       
  tmp1 = build_int_2(leng, 0);      
  tmp1 = build_range_type(g95_default_integer, integer_one_node, tmp1);    
  tmp1 = build_array_type(g95_character1_type_node, tmp1); 
 
  dec = build_decl(VAR_DECL, identifier, tmp1);

  TREE_PUBLIC(dec) = 1;      
  TREE_STATIC(dec) = 1; 
  DECL_COMMON(dec) = 1;      
      
  pushdecl(dec);  
  rest_of_decl_compilation(dec, NULL, 1, 0);

  return dec;    
}    
    
    
     
     
/* find_equivalence()-- Given a symbol, search through the equivalence
 * lists for an unused condition that involves the symbol.  If a rule
 * is found, we return nonzero, the rule is marked as used and the eq1
 * and eq2 pointers point to the rule. */         
         
static int find_equivalence(g95_symbol *symb, g95_equiv **q, g95_equiv **eq2){      
g95_equiv *u, *z;          
          
  for(u=symb->ns->equiv; u; u=u->next) 
    for(z=u->eq; z; z=z->eq) {         
      if (z->used) continue;       
       
      if (u->expr->symbol == symb || z->expr->symbol == symb) {     
	*q = u;         
	*eq2 = z;     
     
	check_init(u->expr->symbol);   
	check_init(z->expr->symbol);    
	return 1;    
      }    
    } 
 
  return 0;  
}         
         
         
        
        
/* free_current_common()-- Free the list of segments. */ 
 
static void free_current_common(void) {      
segment_info *n;          
          
  while(current_common != NULL) {    
    n = current_common->next;       
    g95_free(current_common);
    current_common = n;   
  }        
}  
  
  
      
      
/* build_uninitialized_common()-- Traverse the global symbol tree
 * looking for uninitialized common blocks.  Create these as character
 * arrays of the correct size. */         
         
static void build_uninitialized_common(g95_gsymbol *t) {  
tree identifier;      
      
  if (t == NULL) return;          
          
  build_uninitialized_common(t->left);         
  build_uninitialized_common(t->right);        
        
  if (t->type == GSYM_COMMON && t->backend_decl == NULL_TREE) {          
    identifier = common_identifier(t->name);    
    t->backend_decl = blank_block(identifier, t->size);          
  }     
}       
       
       
 
 
/* new_condition()-- Add a new segment_info structure to the current
 * eq1 is already in the list at s1, eq2 is not. */     
     
static void new_condition(segment_info *o, g95_equiv *q, g95_equiv *eq0) {    
int offset1, offset2;      
segment_info *r;   
   
  offset1 = calculate_offset(q->expr);     
  offset2 = calculate_offset(eq0->expr);          
          
  r = get_segment_info();   
   
  r->sym = eq0->expr->symbol;       
  r->offset = o->offset + offset1 - offset2;          
  r->length = calculate_length(eq0->expr->symbol);   
  r->rule = q; 
 
  r->next = current_segment;   
  current_segment = r;         
         
  if (real_common && r->offset < 0)       
    g95_error("EQUIVALENCE involving '%s' at %L caused the storage block " 
	      "to be extended before the first variable", o->sym->name,    
	      &q->expr->where);
}      
      
      
     
     
/* add_condition()-- At this point we have a new equivalence condition
 * to process.  If both variables are already present, then we are
 * confirming that the condition holds.  Otherwise we are adding a new
 * variable to the segment list. */    
    
static void add_condition(g95_equiv *q, g95_equiv *eq2) {
segment_info *t, *v;    
    
  eq2->used = 1;       
       
  t = find_segment_info(q->expr->symbol);
  v = find_segment_info(eq2->expr->symbol);       
       
  if (t == NULL && v == NULL) abort();    /* Can't happen */  
  if (t != NULL && v == NULL) new_condition(t, q, eq2);         
  if (t == NULL && v != NULL) new_condition(v, eq2, q);       
  if (t != NULL && v != NULL) confirm_condition(t, q, v, eq2);        
}          
          
          
        
        
/* add_equivalences()-- Function for adding symbols to current
 * segment.  Returns zero if the segment was modified.  Equivalence
 * rules are considered to be between the first expression in the list
 * and each of the other expressions in the list.  Symbols are scanned
 * multiple times because a symbol can be equivalenced more than once. */     
     
static int add_equivalences(void) {    
int segment_modified;
g95_equiv *eq1, *q;          
segment_info *e;   
   
  segment_modified = 0; 
 
  for(e=current_segment; e; e=e->next)          
    if (find_equivalence(e->sym, &eq1, &q)) break;  
  
  if (e != NULL) {       
    add_condition(eq1, q);
    segment_modified = 1;   
  }         
         
  return segment_modified;   
}        
        
        
          
          
/* new_segment()-- Given a seed symbol, create a new segment
 * consisting of that symbol and all of the symbols equivalenced with
 * that symbol.  In a real common, the seed symbols are placed next to
 * one another, causing equivalenced symbols to possible overlap in
 * various ways.  In an equivalence common, the segments do not
 * overlap. */      
      
static void new_segment(g95_symbol *symb) {      
int o, seg_start, seg_end;
segment_info *c;     
     
  check_init(symb);    
   
  for(c=current_common; c; c=c->next) 
    if (c->sym == symb) break;    
    
/* If the current symbol is already in the common, make sure the
 * offset is correct.  Any equivalances to this symbol have already
 * been processed in that case. */      
      
  if (c != NULL) {
    if (c->offset != common_length) 
      equivalence_conflict(c->sym->name, c->rule, c->offset, common_length);      
      
    common_length += c->length;     
  } else {   /* New symbol */         
    current_segment = c = get_segment_info();      
    current_segment->sym = symb;         
    current_segment->offset = common_length;    
    current_segment->length = calculate_length(symb);          
          
    while(add_equivalences());         
         
    if (real_common)  
      common_length += c->length; 
    else {         
      seg_start = current_segment->offset;          
      seg_end   = current_segment->offset + current_segment->length;        
        
      for(c=current_segment->next; c; c=c->next) {        
	if (c->offset < seg_start) seg_start = c->offset; 
 
	o = c->offset + c->length;        
	if (o > seg_end) seg_end = o;
      }      
      
      /* Translate the segment to the right place. */          
          
      o = common_length - seg_start;     
      for(c=current_segment; c; c=c->next)   
	c->offset += o; 
 
      common_length += seg_end - seg_start;    
    }        
        
    /* Append the current segment to the current common */      
      
    c = current_segment; 
    while(c->next != NULL)        
      c = c->next;       
       
    c->next = current_common;
    current_common = current_segment;        
    current_segment = NULL;  
  }    
}      
      
      
 
 
/* traverse_common()-- Traverse a single common block, figuring out
 * where each element is located. */       
       
static void traverse_common(g95_common_head *common) {
g95_symbol *symb; 
segment_info *a;        
int w;         
         
  current_common = NULL;         
  common_length = 0;   
  real_common = 1;    
  seen_init = 0;         
         
  for(symb=common->head; symb; symb=symb->common_next)
    new_segment(symb);   
   
  if (!real_common) {   /* shift things to a zero offset */    
    w = 0;      
    for(a=current_common; a; a=a->next)        
      if (a->offset < w) w = a->offset;  
  
    w = -w;       
    for(a=current_common; a; a=a->next)  
      a->offset += w;  
  }         
         
  /* Figure out the real length of the common block.  It may have been
   * extended by an equivalence. */    
    
  for(a=current_common; a; a=a->next) {  
    w = a->offset + a->length;     
    if (w > common_length) common_length = w;    
  }  
}     
     
     
      
      
/* build_common_decl()-- Declare memory for an initialized common
 * block and create declarations for all of the elements. */       
       
static tree build_common_decl(tree identifier) {      
tree dec, initial_value;   
segment_info *i;
int scalar;

  sort_common();
  g95_start_common();  
  
  for(i=current_common; i; i=i->next)        
    g95_init_common_var(i->sym, i->offset);

  scalar = current_common == NULL ||         
    (current_common->next == NULL &&      
     current_common->sym->as == NULL); 
 
  initial_value = g95_data_initializer(scalar);  
  dec = build_decl(VAR_DECL, identifier, TREE_TYPE(initial_value));    
    
  TREE_PUBLIC(dec) = 1;      
  TREE_STATIC(dec) = 1;  
  DECL_COMMON(dec) = 1; 
 
  DECL_INITIAL(dec) = initial_value;   
   
  pushdecl(dec);
  rest_of_decl_compilation(dec, NULL, 1, 0); 
 
  return dec;  
}     
     
     
      
      
/* trans_common()-- Work function for translating a named common block.  */          
          
static void trans_common(g95_symtree *s) { 
g95_gsymbol *v;          
          
  if (s == NULL) return; 
 
  trans_common(s->left);     
  trans_common(s->right);          
          
  traverse_common(s->n.common); 
 
  v = g95_find_gsymbol(g95_gsym_root, s->name);   
  build_common_vars(v->backend_decl);        
        
  free_current_common();       
}          
          
          
          
          
/* finish_equivalences()-- Create a new block that contains all
 * remaining equivalences.  We just add symbols until no rules are
 * left. */ 
 
static void finish_equivalences(g95_namespace *namesp) {        
tree identifier, d;          
g95_equiv *k, *t;  
  
  current_common = NULL;   
  common_length = 0;      
  real_common = 0;       
  seen_init = 0;         
         
  for(k=namesp->equiv; k; k=k->next)      
    for(t=k->eq; t; t=t->eq) {   
      if (t->used) continue;          
          
      new_segment(k->expr->symbol);          
      break;
    }       
       
  if (current_common != NULL) {         
    identifier = g95_unique_identifier("equiv.common");    
    
    d = (seen_init)      
      ? build_common_decl(identifier)   
      : blank_block(identifier, common_length);     
     
    build_common_vars(d);      
    free_current_common(); 
  }
}         
         
         
  
  
/* init_black_common()-- See about initializing the blank common if it
 * is initialized. */          
          
static void init_blank_common(g95_common_head *common) {  
  
  if (common->head == NULL) return;       
  blank_common_seen = 1;   
   
  traverse_common(common);       
       
  if (blank_common_length < common_length)     
    blank_common_length = common_length;       
       
  if (seen_init) {      
    if (blank_common_decl != NULL_TREE)    
      g95_error("Blank common at %L is initialized in another program unit", 
		&blank_common_locus);       
    else {       
      blank_common_decl = build_common_decl(common_identifier(NULL));  
      blank_common_locus = common->where;        
    }  
  }     
     
  free_current_common();    
}       
       
       


/* g95_trans_common()-- Create common variables within a namespace.
 * Unlike other variables, these have to be created before code,
 * because the backend_decl depends on the rest of the common
 * block. */      
      
void g95_trans_common(g95_namespace *namesp) {        
        
  unmark_equivalences(namesp);         
         
  if (namesp->blank_common.head != NULL) {     
    traverse_common(&namesp->blank_common);        
    build_common_vars(blank_common_decl);          
    free_current_common();
  }          
          
  trans_common(namesp->common_root);    
    
  finish_equivalences(namesp);  
}      
      
      
   
   
/* init_common()-- Figure out how big a particular common is, make
 * sure the size is consistent and initialize a block if necessary. */

static void init_common(g95_symtree *sta) {  
g95_common_head *common;
g95_gsymbol *e;          
char *name0;        
        
  common = sta->n.common;   
  name0 = sta->name;  
  
  traverse_common(sta->n.common);          
  e = g95_get_gsymbol(name0);    
    
  switch(e->type) {          
  case GSYM_UNKNOWN:       
    e->type = GSYM_COMMON;  
    e->size = common_length;       
    e->where = common->where;        
    break;        
        
  case GSYM_COMMON:         
    if (e->size != common_length && strcmp(name0, BLANK_COMMON_NAME) != 0) {         
      g95_warning(121, "COMMON block '%s' is %d bytes at %L and %d bytes "    
		  "at %L", name0, common_length, &common->where, e->size,   
		  &e->where);  
  
      if (common_length > e->size)     
	e->size = common_length;  
    }   
   
    break;          
          
  default:          
    g95_global_used(e, &common->where);     
    e = NULL; 
    goto done;        
  }         
         
  /* If the common is initialized, declare it now */     
     
  if (seen_init) {     
    if (e->backend_decl == NULL_TREE) 
      e->backend_decl = build_common_decl(common_identifier(name0));     
    else 
      g95_error("COMMON block '%s' at %L is initialized in another program "         
		"unit", &common->where, name0);
  }  
  
done:    
  free_current_common();        
}      
      
      
    
    
/* init_common0()-- Traverse all common blocks within a namespace,
 * figuring out how large each block is, and taking care of an
 * initialization if present. */        
        
static void init_common0(g95_symtree *st) {       
       
  if (st == NULL) return;          
          
  init_common0(st->left);     
  init_common0(st->right);     
     
  init_common(st);  
}


  
  
/* init_common1()-- Traverse namespaces. */       
       
static void init_common1(g95_namespace *n) {       
       
  if (n == NULL) return;  
  
  while(n != NULL) {   
    init_blank_common(&n->blank_common);    
    
    unmark_equivalences(n);
    init_common0(n->common_root);          
    init_common1(n->contained);      
      
    n = n->sibling;      
  }       
}      
      
      
       
       
/* g95_init_common()-- Recursively scan all common blocks in all
 * namespaces.  Build up the sizes of common blocks, and
 * initializations. */  
  
void g95_init_common(g95_namespace *ns) {    
tree identifier;

  init_common1(ns);          
  build_uninitialized_common(g95_gsym_root);         
         
  if (blank_common_seen && blank_common_decl == NULL_TREE) {    
    identifier = common_identifier(NULL);        
    blank_common_decl = blank_block(identifier, blank_common_length);    
  }   
}     
