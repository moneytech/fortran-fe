          
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
 * solving a linear system of equations.  The matrix is usually a
 * sparse matrix in which each row contains all zero elements except
 * for a +1 and a -1, a sort of a generalized Vandermonde matrix.  The
 * matrix is usually block diagonal.  The system can be
 * overdetermined, underdetermined or have a unique solution.  If the
 * system is inconsistent, the program is not standard conforming.
 * The solution vector is integral, since all of the pivots are +1 or -1.
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
 * Since we know the minimum extent of the segment, everything in the
 * segment is translated to its position in the common block.  The
 * usual case here is that there are no equivalence statements and the
 * common block is series of segments with one variable each, which is
 * a diagonal matrix in the matrix formulation.
 *
 * Once all common blocks have been created, the list of equivalences
 * is examined for still-unused equivalence conditions.  If these
 * exist, a variable from an unused equivalence is placed into a
 * private block in order to start a new segment, which is built as
 * usual.  This process continues until all equivalenced variables
 * have been put into a block. */          
          
#include "trans.h"
       
       
typedef struct segment_info {     
  g95_symbol *sym;    
  int offset, length; 
  struct segment_info *next;   
} segment_info;       
       
static segment_info *current_segment, *current_common;       
static int current_length;        
        
#define get_segment_info() g95_getmem(sizeof(segment_info))
    
    
   
   
/* create_common()-- Declare memory for the common block and create
 * declarations for all of the elements.  Frees the memory in the
 * current_common list. */          
          
static void create_common(char *name) {   
tree common, tmp, type, array, storage;         
segment_info *h, *next_s; 
variable_info vinfo;        
        
  tmp = build_int_2(current_length, 0);      
  tmp = build_range_type(g95_default_integer, integer_one_node, tmp);  
  type = build_array_type(g95_character1_type_node, tmp);          
          
  common = get_identifier((name == NULL) ? "equiv.common" : name);   
  array = build_decl(VAR_DECL, common, type);       
       
  if (name != NULL) TREE_PUBLIC(array) = 1;         
  TREE_STATIC(array) = 1; 
  DECL_COMMON(array) = 1;       
       
  pushdecl(array);   
  rest_of_decl_compilation(array, NULL, 1, 0);         
         
  /* Declare the variables inside the common */     
     
  for(h=current_common; h; h=next_s) {      
    next_s = h->next;

    tmp = build_int_2(h->offset, 0);    
    storage = build(ARRAY_REF, type, array, tmp);   
   
    if (h->sym->as == NULL) {       
      type = g95_typenode_for_spec(&h->sym->ts);         
      type = build_pointer_type(type);     
     
      h->sym->backend_decl = build1(ADDR_EXPR, type, storage);   
    } else {      
      g95_symbol_vinfo(h->sym, &vinfo);    
      g95_get_storage(&vinfo, NULL_TREE);  
  
      type = g95_get_descriptor(&vinfo);  
      tmp = build_decl(VAR_DECL, g95_sym_identifier(h->sym, NULL), type);   
      pushdecl(tmp);

      g95_init_descriptor(&vinfo, tmp, storage);   
      h->sym->backend_decl = tmp;  
    }  
  
    g95_free(h);    
  }  
}   
   
   


/* find_segment_info()-- Given a symbol, find it in the current list
 * segment list.  Returns NULL if not found. */ 
 
static segment_info *find_segment_info(g95_symbol *symbol) {          
segment_info *n;     
     
  for(n=current_segment; n; n=n->next)
    if (n->sym == symbol) return n;

  return NULL;    
} 
 
 


/* calculate_length()-- Given a variable symbol, calculate the total
 * length in bytes of the variable. */

static int calculate_length(g95_symbol *symbol) {        
int j, element_size;        
mpz_t elements;  
  
  element_size = int_size_in_bytes(g95_typenode_for_spec(&symbol->ts));        
  if (symbol->as == NULL) return element_size;        
        
  /* Calculate the number of elements in the array */  
  
  if (g95_array_spec_size(symbol->as, &elements) == FAILURE)    
    g95_internal_error("calculate_length(): Unable to determine array size");        
        
  j = mpz_get_ui(elements);          
  mpz_clear(elements);

  return j*element_size;;
}     
     
     
       
       
/* get_mpz()-- Given an expression node, make sure it is a constant
 * integer and return the mpz_t value. */     
     
static mpz_t *get_mpz(g95_expr *g) {         
         
  if (g->type != EXPR_CONSTANT)    
    g95_internal_error("get_mpz(): Not an integer constant");          
          
  return &g->value.integer;  
}      
      
      
      
      
/* element_number()-- Given an array specification and an array
 * reference, figure out the array element number (zero based).
 * Bounds and elements are guaranteed to be constants.  If something
 * goes wrong we generate an error and return zero. */ 
 
static int element_number(g95_array_ref *ar) {       
mpz_t multiplier, offset, extent, l;
g95_array_spec *as;         
int b, rank;         
         
  as = ar->as;  
  rank = as->rank;    
  mpz_init_set_ui(multiplier, 1);    
  mpz_init_set_ui(offset, 0);       
  mpz_init(extent);    
  mpz_init(l);     
     
  for(b=0; b<rank; b++) { 
    if (ar->dimen_type[b] != DIMEN_ELEMENT)       
      g95_internal_error("element_number(): Bad dimension type");       
       
    mpz_sub(l, *get_mpz(ar->start[b]), *get_mpz(as->lower[b]));        
        
    mpz_mul(l, l, multiplier);     
    mpz_add(offset, offset, l);       
       
    mpz_sub(extent, *get_mpz(as->upper[b]), *get_mpz(as->lower[b]));
    mpz_add_ui(extent, extent, 1);         
         
    if (mpz_sgn(extent) < 0) mpz_set_ui(extent, 0);    
    
    mpz_mul(multiplier, multiplier, extent);      
  } 
 
  b = mpz_get_ui(offset);         
         
  mpz_clear(multiplier);       
  mpz_clear(offset);      
  mpz_clear(extent);        
  mpz_clear(l);     
     
  return b;   
}    
    
    
      
      
/* calculate_offset()-- Given a single element of an equivalence list,
 * figure out the offset from the base symbol.  For simple variables
 * or full arrays, this is simply zero.  For an array element we have
 * to calculate the array element number and multiply by the element
 * size.  For a substring we have to calculate the further reference. */       
       
static int calculate_offset(g95_expr *s) {          
int a, element_size, offset;    
g95_typespec *element_type; 
g95_ref *reference; 
 
  offset = 0;  
  element_type = &s->symbol->ts; 
 
  for(reference=s->ref; reference; reference=reference->next)        
    switch(reference->type) {  
    case REF_ARRAY:          
      switch(reference->u.ar.type) {       
      case AR_FULL:   
	break;

      case AR_ELEMENT:
	a = element_number(&reference->u.ar); 
	element_size = int_size_in_bytes(g95_typenode_for_spec(element_type));      
      
	offset += a * element_size;     
	break;          
          
      default: 
	g95_internal_error("calculate_offset(): Bad array reference");          
      }   
          
      break;         
         
    case REF_COMPONENT:          
      element_type = &reference->u.c.component->ts;

      /* TODO: Need to finish this */        
      g95_internal_error("calculate_offset(): Can't handle component refs");       
      break;          
          
    case REF_SUBSTRING:  
      if (reference->u.ss.start != NULL)         
	offset += mpz_get_ui(*get_mpz(reference->u.ss.start)) - 1;        
        
      break;     
    } 
 
  return offset;      
}   
   
   
   
   
/* new_condition()-- Add a new segment_info structure to the current
 * eq1 is already in the list at s1, eq2 is not. */   
   
static void new_condition(segment_info *v, g95_equiv *eq1, g95_equiv *eq2) {
int offset1, offset2;      
segment_info *a;      
      
  offset1 = calculate_offset(eq1->expr);
  offset2 = calculate_offset(eq2->expr);

  a = get_segment_info(); 
 
  a->sym = eq2->expr->symbol;   
  a->offset = v->offset + offset1 - offset2; 
  a->length = calculate_length(eq2->expr->symbol);       
       
  a->next = current_segment;       
  current_segment = a;      
}          
          
          
       
       
/* confirm_condition()-- Given two equivalence structures that are
 * both already in the list, make sure that this new condition is not
 * violated, generating an error if it is. */          
          
static void confirm_condition(segment_info *k, g95_equiv *eq1,          
			      segment_info *e, g95_equiv *eq2) {    
int offset1, offset2;

  offset1 = calculate_offset(eq1->expr);         
  offset2 = calculate_offset(eq2->expr);   
   
  if (k->offset + offset1 != e->offset + offset2)          
    g95_error("Inconsistent equivalence rules involving '%s' at %L and "     
	      "'%s' at %L", k->sym->name, &k->sym->declared_at,
	      e->sym->name, &e->sym->declared_at);     
} 
 
 
 
 
/* add_condition()-- At this point we have a new equivalence condition
 * to process.  If both variables are already present, then we are
 * confirming that the condition holds.  Otherwise we are adding a new
 * variable to the segment list. */        
        
static void add_condition(g95_equiv *eq1, g95_equiv *eq2) {   
segment_info *n, *t;

  eq1->expr->symbol->mark = 1;      
  eq2->expr->symbol->mark = 1;   
  
  eq2->used = 1;     
     
  n = find_segment_info(eq1->expr->symbol);         
  t = find_segment_info(eq2->expr->symbol);  
  
  if (n == NULL && t == NULL) abort();    /* Can't happen */
  if (n != NULL && t == NULL) new_condition(n, eq1, eq2);         
  if (n == NULL && t != NULL) new_condition(t, eq2, eq1);          
  if (n != NULL && t != NULL) confirm_condition(n, eq1, t, eq2);          
}




/* find_equivalence()-- Given a symbol, search through the equivalence
 * lists for an unused condition that involves the symbol.  If a rule
 * is found, we return nonzero, the rule is marked as used and the eq1
 * and eq2 pointers point to the rule. */ 
 
static int find_equivalence(g95_symbol *symb, g95_equiv **eq1, g95_equiv **eq2){     
g95_equiv *c, *l;        
        
  for(c=symb->ns->equiv; c; c=c->next)       
    for(l=c->eq; l; l=l->eq) {
      if (l->used) continue;    
    
      if (c->expr->symbol == symb || l->expr->symbol == symb) {    
	*eq1 = c;     
	*eq2 = l;        
	return 1;         
      }   
    }      
      
  return 0;         
}   
   
   
  
  
/* add_equivalences()-- Function for adding symbols to current
 * segment.  Returns zero if the segment was modified.  Equivalence
 * rules are considered to be between the first expression in the list
 * and each of the other expressions in the list.  Symbols are scanned
 * multiple times because a symbol can be equivalenced more than once. */

static int add_equivalences(void) {      
int segment_modified;          
g95_equiv *eq1, *eq2;        
segment_info *f;    
    
  segment_modified = 0;      
      
  for(f=current_segment; f; f=f->next) 
    if (find_equivalence(f->sym, &eq1, &eq2)) break;          
          
  if (f != NULL) {
    add_condition(eq1, eq2);     
    segment_modified = 1;          
  }         
         
  return segment_modified; 
}     
     
     
         
         
/* new_segment()-- Given a seed symbol, create a new segment
 * consisting of that symbol and all of the symbols equivalenced with
 * that symbol. */         
         
static void new_segment(g95_symbol *sym) {  
int seg_start, seg_end, m, end; 
segment_info *v;   
   
  current_segment = get_segment_info();          
  current_segment->sym = sym;        
  current_segment->offset = 0;       
  current_segment->length = calculate_length(sym);        
        
  sym->mark = 1;     
     
  while(add_equivalences()); 
 
  /* Determine the extent of the segment */    
    
  seg_start = 0;  
  seg_end = 0;

  for(v=current_segment; v; v=v->next) {        
    if (v->offset < seg_start) seg_start = v->offset;          
          
    end = v->offset + v->length;      
    if (end > seg_end) seg_end = end;      
  }     
     
  /* Translate the segment so that it starts at the end of the
   * current common. */         
         
  m = current_length - seg_start;    
    
  for(v=current_segment; v; v=v->next)   
    v->offset += m;          
          
  current_length += seg_end - seg_start;       
       
  /* Append the current segment to the current common */         
         
  v = current_segment;         
  while(v->next != NULL)        
    v = v->next;      
      
  v->next = current_common;    
  current_common = current_segment;   
  current_segment = NULL; 
}         
         
         
         
         
/* finish_equivalences()-- Create a new block that contains all
 * remaining equivalences.  We just add symbols until no rules are
 * left. */          
          
static void finish_equivalences(g95_namespace *ns) {        
g95_equiv *z, *y;       
       
  current_common = NULL; 
  current_length = 0;      
      
  for(z=ns->equiv; z; z=z->next)   
    for(y=z->eq; y; y=y->eq) {  
      if (y->used) continue;        
        
      new_segment(z->expr->symbol);        
      break; 
    }  
  
  if (current_common != NULL) create_common(NULL);          
}      
      
      
          
          
/* translate_common()-- Translate a single common block */  
  
static void translate_common(char *name, g95_symbol *var_list) {  
g95_symbol *symb;  
  
  current_common = NULL;
  current_length = 0;  
  
  /* Mark bits indicate which symbols have already been placed in a
   * common area. */    
    
  for(symb=var_list; symb; symb=symb->common_next)
    symb->mark = 0; 
 
  for(;;) {       
    for(symb=var_list; symb; symb=symb->common_next) 
      if (!symb->mark) break;  
  
    if (symb == NULL) break;  /* All symbol have been placed in a common */  
    new_segment(symb);
  }    
    
  create_common(name);        
}          
          
          
         
         
/* named_common()-- Work function for translating a named common block.  */  
  
static void named_common(g95_symbol *s) {         
         
  if (s->attr.common) translate_common(s->name, s->common_head);  
}        
        
        
       
       
/* g95_trans_common()-- Translate the common blocks in a namespace.
 * Unlike other variables, these have to be created before code,
 * because the backend_decl depends on the rest of the common
 * block. */       
       
void g95_trans_common(g95_namespace *ns) {

  if (ns->blank_common != NULL)  
    translate_common(BLANK_COMMON_NAME, ns->blank_common);   
   
  g95_traverse_ns(ns, named_common);        
        
  finish_equivalences(ns);  
}   
