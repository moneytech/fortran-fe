 
/* Transform scalar (and some vector) array expressions

   Copyright (C) 2003 Free Software Foundation, Inc.
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
Boston, MA 02111-1307, USA.

*/


/* An array descriptor looks like:

struct {
  char *offset;
  int rank, element_size;
  void *base;

  struct {
    int mult, lbound, ubound;
  } dimen[G95_MAX_DIMENSIONS];
} g95_array_descriptor;

To make things easier for the back end, the array of structures at the
end is really a one dimensional array of default integers named info[],
which is only as large as it has to be.  The multipliers are at index
3*d, lbounds at 3*d+1 and ubounds at 3*d+2 where d is the (zero based)
dimension we are interested in.  The address of an array element given
by (x_1, x_2, ..., x_n)

is determined by calculating

   address = offset + mult_1*x_1 + mult_2*x_2 + ... + mult_n*x_n.

The offset pointer is first within the descriptor for ease of access.
The base pointer points to a block of memory occupied by the array and
is typically only used if the array is ALLOCATABLE or temporary and
will usually be NULL.  Storage for the array is managed separately
from the descriptor.  The multipliers are calculated by

   mult_1 = element size in bytes
   mult_2 = dim(ubound_1+1, lbound_1) * mult_1
   mult_3 = dim(ubound_2+1, lbound_2) * mult_2
   ...
   mult_n = dim(ubound_{n-1}+1, lbound_{n-1}) * mult_{n-1}

For an n dimensional array, mult_{n+1} is the size of the entire array
in bytes.  This formula correctly calculates the size of an array if
lbound_n>ubound_n for any n, which means a zero-sized array.  mult_1
is usually equal to the element size in bytes, but can be different if
an array section is passed between program units.

The offset pointer is calculated using:

   offset = base - (mult_1*lbound_1 + mult_2*lbound_2 + ... + mult_n*lbound_n)

which is the condition that (lbound_1, lbound_2, ..., lbound_n) points
to the first element of the array.  Note that the base can also be
recovered from the offset through

   base = offset + mult_1*lbound_1 + mult_2*lbound_2 + ... + mult_n*lbound_n.

If the array specification is such that the inner product overflows a
single word, things still work because the calculation of the offset
underflows in the same way: (a+(b-a)) mod N = b mod N.  If a
multiplier overflows, then the array is too big to hold in memory
anyhow.

The rank of the array is stored in the descriptor but is never used by
compiled code, which implicitly knows how many dimensions are in the
array.  This is for use by library and external user subroutines in
other languages.  It also allows us to avoid recompiles of the library
if G95_MAX_DIMENSIONS changes.

Assumed shape arrays are passed to procedures by passing a pointer to
the descriptor.  Passing an array section causes a new descriptor to
be created that accesses the same memory in a different way.  The new
descriptor is passed to the procedure.  Array expressions including
vector subscripts are transformed to full temporary array earlier in
the translation process and are not issues at this level.

Deferred shape arrays use the original descriptor and is therefore the
same shape as the original.  Allocatable arrays are created as a
descriptor that is initialized by the ALLOCATE statement.  Assumed
size arrays create a new descriptor that points into an existing block
of memory, though if the block is not contiguous (through being passed
an array section), the elements of the array must be copied and
repacked into a new block of memory.  A library subroutine takes care
of this mess.

Descriptors live almost exclusively on the stack.  The exception is
arrays that are module variables or are saved.  Common blocks require
storage association and end up being created by subroutines that use
the common block (though they are always explicitly shaped with
constant specifications).

Arrays that live in derived types are treated similarly, mainly
because the derived types can also be in the common blocks and must
storage-associate correctly.  For an allocatable array in a structure
(f2k) it would make sense for the descriptor to live in the structure.
Descriptors for arrays in structures are created as needed. 

The ultimate story of variable reference is as follows:  We
start with the base variable.  If the variable is a derived type and
we are referencing a component, a component reference is built.  If
the entity is then an array, we have to build a descriptor for the
array.  If the array has an array reference associated with it, we use
the descriptor to build a calculation that returns the address of the
element.  And so on through the reference list.

A wicked case in which an intermediate reference is an array section
is handled by recognizing that the ultimate result is an array
section.  The initial descriptor is calculated, then modified by
succeeding references, which can only modify the offset by a constant.
Intermediate references that are vector subscripts are expanded into
temporary arrays at a higher level.

*/  
  
#include "trans.h"
     
     
static tree array_offset[G95_MAX_DIMENSIONS+1];       
static tree array_rank[G95_MAX_DIMENSIONS+1];    
static tree array_esize[G95_MAX_DIMENSIONS+1];         
static tree array_base[G95_MAX_DIMENSIONS+1];   
static tree array_info[G95_MAX_DIMENSIONS+1];        
        
        
/* Type nodes of descriptors of all dimensionalities */       
       
static tree array_type_node[G95_MAX_DIMENSIONS+1];          
static tree array_ptype_node[G95_MAX_DIMENSIONS+1];        
        
        
/* Information on the current array being declared.  This information
 * is available before it can be used, so we store it here
 * temporarily.  */         
         
static struct {       
  tree offset;         
  tree dimensions;          
  tree esize;         
  tree base;   
   
  tree size;    /* Doesn't go in a real descriptor */  
  
  struct {         
    tree mult, lbound, ubound;        
  } info[G95_MAX_DIMENSIONS];   
   
} current_desc;    
    
static tree section_info;    
    
    
     
     
/* conditional_indirect()-- Given a variable which might be a pointer
 * to a descriptor, insert the indirection to return a reference to
 * the descriptor. */

static tree conditional_indirect(tree node) {    
int dim;  
  
  for(dim=1; dim<=G95_MAX_DIMENSIONS; dim++) {    
    if (TREE_TYPE(node) == array_type_node[dim]) return node;

    if (TREE_TYPE(node) == array_ptype_node[dim])          
      return build1(INDIRECT_REF, array_type_node[dim], node);
  } 
 
  g95_internal_error("conditional_indirect(): Not a descriptor");     
}       
       
       
          
          
/* deferred_descriptor()-- Construct a descriptor for a deferred array. */     
     
static void deferred_descriptor(variable_info *vinfo) {      
int w, rank;     
     
  rank = vinfo->as->rank;       
      
  current_desc.offset = null_pointer_node;        
  current_desc.dimensions = build_int_2(rank, 0);   
  current_desc.esize = integer_zero_node;    
     
  for(w=0; w<rank; w++) {     
    current_desc.info[w].mult   = integer_zero_node;    
    current_desc.info[w].lbound = integer_zero_node;          
    current_desc.info[w].ubound = integer_zero_node;  
  }        
}      
      
      
   
   
/* g95_get_array_desc()-- Get an array descriptor type of a particular
 * dimension. */        
        
tree g95_get_array_desc(int dimension) {    
    
  assert(dimension > 0 && dimension <= G95_MAX_DIMENSIONS); 
 
  return array_type_node[dimension]; 
}    
    
    
    
    
/* get_dimension()-- Given a descriptor variable, figure out what
 * dimension it represents.  This forms an index into the component
 * arrays to get the right component of the right type. */        
        
static int get_dimension(tree desc) {      
tree type;     
int dim;     
     
  type = TREE_TYPE(desc);  
  
  for(dim=1; dim<=G95_MAX_DIMENSIONS; dim++)   
    if (array_type_node[dim] == type ||  
	array_ptype_node[dim] == type) return dim;    
    
  g95_internal_error("get_dimension(): Bad array type");       
}          
          
          
 
 
/* get_adesc_esize()-- Given a descriptor node, return a reference to
 * the element size member. */        
        
static tree get_adesc_esize(tree desc) {
int t;  
  
  desc = conditional_indirect(desc);   
  t = get_dimension(desc); 
 
  return build(COMPONENT_REF, TREE_TYPE(array_esize[t]), desc, array_esize[t]);       
}    
    
    
         
         
/* conditional_pointer()-- Given a variable which might be a
 * descriptor, return a pointer to the descriptor. */         
         
static tree conditional_pointer(tree node) {          
int dim;      
      
  for(dim=1; dim<=G95_MAX_DIMENSIONS; dim++) {    
    if (TREE_TYPE(node) == array_ptype_node[dim]) return node; 
 
    if (TREE_TYPE(node) == array_type_node[dim])      
      return build1(ADDR_EXPR, array_ptype_node[dim], node);   
  }          
          
  g95_internal_error("conditional_pointer(): Not a descriptor");    
}        
        
        
 
 
/* g95_adesc_base()-- Given a descriptor node, return a reference to
 * the base member. */      
      
tree g95_adesc_base(tree desc) {    
int e;       
       
  desc = conditional_indirect(desc); 
  e = get_dimension(desc);

  return build(COMPONENT_REF, TREE_TYPE(array_base[e]), desc, array_base[e]);   
}


     
     
/* get_adesc_offset()-- Given a descriptor node, return a reference to
 * the offset member. */

static tree get_adesc_offset(tree desc) {     
int f;       
       
  desc = conditional_indirect(desc);  
  f = get_dimension(desc);   
   
  return build(COMPONENT_REF, TREE_TYPE(array_offset[f]), desc,        
	       array_offset[f]);       
}     
     
     
      
      
/* get_adesc_info()-- Given a descriptor, access a constant element
 * from the info[] array. */

static tree get_adesc_info(tree desc, int element) {         
tree tmp;    
int l;        
        
  desc = conditional_indirect(desc);          
  l = get_dimension(desc);    
    
  tmp = build(COMPONENT_REF, TREE_TYPE(array_info[l]), desc, array_info[l]);  
  return build(ARRAY_REF, g95_default_integer, tmp, build_int_2(element, 0)); 
}         
         
         
         
         
/* g95_set_section_info()-- Generate an instruction that modifies the
 * section_info[] array.  The format of this array is a series of
 * records for each dimension of the original array.  Records start
 * with a word flag that is nonzero for a dimension with an element or
 * zero for a section.  For a dimension with an element, a word
 * follows that gives the element number.  For a dimension with a
 * range, three words follow giving the start, end and stride. */ 
 
void g95_set_section_info(g95_se *se, int w, tree value) {
tree tmp;  
 
  tmp = build(ARRAY_REF, g95_default_integer, section_info, build_int_2(w, 0)); 
  g95_add_modify_expr(&se->pre, tmp, value);        
}    
    
    
          
          
/* g95_get_adesc_lbound()-- Given a descriptor and a dimension, return
 * the tree describing the multiplier. */    
    
tree g95_get_adesc_lbound(tree desc, tree dim) {       
tree tmp, index; 
int v;  
  
  desc = conditional_indirect(desc);        
  v = get_dimension(desc);    
    
  tmp = build(COMPONENT_REF, TREE_TYPE(array_info[v]), desc, array_info[v]);         
         
  index = fold(build(MULT_EXPR, g95_default_integer, dim, build_int_2(3, 0)));   
  index = fold(build(PLUS_EXPR, g95_default_integer, index,          
		     build_int_2(-2, -1)));

  return build(ARRAY_REF, g95_default_integer, tmp, index);    
}   
   
   
 
 
/* init_array_type()-- Initialize array types.  Zero dimensional
 * arrays are not allowed and are not initialized here. */  
  
void g95_init_array_types(void) {      
tree tmp, type;     
char name[20];        
int a;    
    
  for(a=1; a<=G95_MAX_DIMENSIONS; a++) {        
    type = make_node(RECORD_TYPE);   
   
    sprintf(name, "array%d", a);        
    TYPE_NAME(type) = get_identifier(name);          
          
    array_offset[a]   = g95_add_field(type, "offset",  pchar_type_node);        
    array_rank[a]     = g95_add_field(type, "rank",    g95_default_integer);  
    array_esize[a]    = g95_add_field(type, "esize",   g95_default_integer); 
    array_base[a]     = g95_add_field(type, "base",    pvoid_type_node);      
      
    tmp = build_int_2(3*a-1, 0);   
    tmp = build_range_type(g95_default_integer, integer_zero_node, tmp); 
    tmp = build_array_type(g95_default_integer, tmp);          
          
    array_info[a] = g95_add_field(type, "info", tmp);    
    g95_finish_type(type);  
  
    G95_DESCRIPTOR_P(type) = 1;         
    array_type_node[a] = type;  
    array_ptype_node[a] = build_pointer_type(type);         
  } 
 
  /* Build the section_info[] array. */     
     
  tmp = build_int_2(4*G95_MAX_DIMENSIONS, 0);   
  tmp = build_range_type(g95_default_integer, integer_zero_node, tmp);     
  tmp = build_array_type(g95_default_integer, tmp);    
    
  section_info =    
    build_decl(VAR_DECL, get_identifier(PREFIX "section_info"), tmp);    
  DECL_EXTERNAL(section_info) = 1;       
}     
     
     
         
         
/* convert_bounds()-- Given an array spec, convert the expressions
 * forming the bounds into the descriptor.  */    
    
static void convert_bounds(g95_array_spec *as) {      
stmtblock_t post;        
g95_se se;  
int m;          
          
  current_desc.dimensions = build_int_2(as->rank, 0);     
  current_desc.base = null_pointer_node;  
  
  g95_init_block(&post);        
        
  for(m=0; m<as->rank; m++) {    
    if (as->lower[m] != NULL) {      
      g95_init_se(&se, NULL);     
      g95_conv_expr(&se, as->lower[m]);    
      current_desc.info[m].lbound = se.expr;       
       
      g95_add_block_to_block(&g95_context->pre, &se.pre);   
      g95_add_block_to_block(&post, &se.post);
    }    
    
    if (as->upper[m] != NULL) {
      g95_init_se(&se, NULL);    
      g95_conv_expr(&se, as->upper[m]);         
      current_desc.info[m].ubound = se.expr;

      g95_add_block_to_block(&g95_context->pre, &se.pre);      
      g95_add_block_to_block(&post, &se.post);  
    }          
  }        
        
  g95_add_block_to_block(&g95_context->pre, &post);   
}       
       
       
  
  
/* get_adesc_lbound0()-- Given a descriptor and a dimension, return
 * the tree describing the multiplier. */    
    
static tree get_adesc_lbound0(tree desc, int dimension) {        
        
  return get_adesc_info(desc, 3*dimension+1);  
}          
          
     
     
/* get_adesc_ubound0()-- Given a descriptor and a dimension, return
 * the tree describing the multiplier. */

static tree get_adesc_ubound0(tree desc, int dimension) {          
          
  return get_adesc_info(desc, 3*dimension+2);       
}  
  
  
         
         
/* build_desc_constructor()-- Given the junk in the current
 * descriptor, build a constructor for the descriptor. */      
      
static tree build_desc_constructor(int rank) {          
tree node, info_node, tmp;
int a;    
    
  tmp = current_desc.offset;     
  if (tmp == NULL_TREE) tmp = null_pointer_node;    
  node = tree_cons(array_offset[rank], tmp, NULL_TREE);      
      
  tmp = current_desc.dimensions;  
  if (tmp == NULL_TREE) tmp = integer_zero_node;       
  node = tree_cons(array_rank[rank], tmp, node);     
     
  tmp = current_desc.esize;   
  if (tmp == NULL_TREE) tmp = integer_zero_node;    
  node = tree_cons(array_esize[rank], tmp, node); 
 
  tmp =  current_desc.base;      
  if (tmp == NULL_TREE) tmp = null_pointer_node;      
  node = tree_cons(array_base[rank], tmp, node);       
       
  info_node = NULL_TREE;        
        
  for(a=0; a<rank; a++) {     
    tmp = current_desc.info[a].mult;
    if (tmp == NULL_TREE) tmp = integer_zero_node;        
    info_node = tree_cons(build_int_2(3*a, 0), tmp, info_node);   
   
    tmp = current_desc.info[a].lbound;    
    if (tmp == NULL_TREE) tmp = integer_zero_node;   
    info_node = tree_cons(build_int_2(3*a+1, 0), tmp, info_node);

    tmp = current_desc.info[a].ubound;          
    if (tmp == NULL_TREE) tmp = integer_zero_node;  
    info_node = tree_cons(build_int_2(3*a+2, 0), tmp, info_node);   
  }        
        
  tmp = build_int_2(3*rank, 0);         
  tmp = build_range_type(g95_default_integer, integer_zero_node, tmp);        
  tmp = build_array_type(g95_default_integer, tmp);    
    
  info_node = build(CONSTRUCTOR, tmp, NULL_TREE, nreverse(info_node));  
  
  node = tree_cons(array_info[rank], info_node, node);      
      
  node = build(CONSTRUCTOR, array_type_node[rank], NULL_TREE,          
	       nreverse(node));     
     
  /* Zero out the current constructor */       
       
  current_desc.offset = NULL_TREE;         
  current_desc.dimensions = NULL_TREE;
  current_desc.esize = NULL_TREE;        
  current_desc.base = NULL_TREE;          
  current_desc.size = NULL_TREE;      
      
  for(a=0; a<G95_MAX_DIMENSIONS; a++) {          
    current_desc.info[a].mult = NULL_TREE;      
    current_desc.info[a].lbound = NULL_TREE;          
    current_desc.info[a].ubound = NULL_TREE;      
  }        
        
  return node;        
}        
        
        
         
         
static tree get_adesc_rank(tree desc) {  
int v; 
 
  desc = conditional_indirect(desc);       
  v = get_dimension(desc);     
     
  return build(COMPONENT_REF, TREE_TYPE(array_rank[v]), desc, array_rank[v]);     
}




/* get_adesc_multiplier0()-- Given a descriptor and a dimension,
 * return the tree describing the multiplier. */         
         
static tree get_adesc_multiplier0(tree desc, int dimension) {     
     
  return get_adesc_info(desc, 3*dimension);          
}     
     
     
       
       
/* g95_temp_array_descriptor()-- Given a temporary array descriptor
 * created on the fly, initialize its components from the
 * variable_info structure.  The array specification is explicit with
 * constant values.  The array may be zero sized, in which case we
 * don't care about the multipliers, nor the total size, so we avoid
 * the DIM(). */         
         
void g95_temp_array_descriptor(g95_se *se, variable_info *vinfo, tree desc,       
			       tree storage) {          
tree var, tmp, offset, element_size, lbound, ubound, mult, last_mult;          
g95_array_spec *as;  
int j;        
        
  as = vinfo->as;        
  last_mult = NULL_TREE;     
     
  var = get_adesc_rank(desc);     
  g95_add_modify_expr(&se->pre, var, build_int_2(as->rank, 0)); 
 
  var = get_adesc_esize(desc);         
  element_size = size_in_bytes(g95_typenode_for_spec(&vinfo->ts));       
  g95_add_modify_expr(&se->pre, var, element_size);       
       
  offset = build1(ADDR_EXPR, pchar_type_node, storage);       
       
  for(j=0; j<as->rank; j++) {        
    var = get_adesc_lbound0(desc, j);  
    lbound = g95_conv_constant(as->lower[j], NULL_TREE); 
    g95_add_modify_expr(&se->pre, var, lbound);       
       
    var = get_adesc_ubound0(desc, j);
    ubound = g95_conv_constant(as->upper[j], NULL_TREE);          
    g95_add_modify_expr(&se->pre, var, ubound);

    if (j == 0) 
      mult = element_size; 
    else {    
      tmp = fold(build(MINUS_EXPR, g95_default_integer, ubound, lbound));   
      tmp = fold(build(PLUS_EXPR, g95_default_integer, tmp, integer_one_node));  
  
      mult = fold(build(MULT_EXPR, g95_default_integer, tmp, last_mult)); 
    }         
         
    last_mult = mult;       
       
    var = get_adesc_multiplier0(desc, j);          
    g95_add_modify_expr(&se->pre, var, mult);          
          
    tmp = fold(build(MULT_EXPR, g95_default_integer, mult, lbound));
    offset = fold(build(MINUS_EXPR, pchar_type_node, offset, tmp));  
  }     
         
  var = get_adesc_offset(desc);    
  g95_add_modify_expr(&se->pre, var, offset);  
}       
       
       


/* array_desc_argument()-- Convert an actual argument that is a full
 * array or array element.  The value passed to the procedure is a
 * pointer to the first array element for a full array, or the
 * specified array element.  The incoming procedure expects the array
 * data to be contiguous.  If not, the data must be copied into a
 * contiguous block, and unpacked when the procedure exits.  This is
 * all handled by library subroutines.  For character arrays, we pass
 * a pointer to a descriptor of the first element. */  
  
static void array_desc_argument(g95_se *se, g95_actual_arglist *actual) { 
tree tmp, var1, var2, desc;    
g95_ref *reference, *last;         
g95_expr *m; 
int n;     
     
  if (actual->type == FULL_ARRAY) {      
    g95_set_section_info(se, 0, integer_zero_node);          
          
    se->reflevel = 1;
    g95_conv_expr(se, actual->u.expr); 
    reference = NULL; 
    desc = se->expr;

  } else {    
    m = actual->u.expr;   
   
    if (m->ref->next == NULL) {          
      last = m->ref;    
      reference = NULL; 
      m->ref = NULL;          
          
    } else {
      reference = m->ref;   
      while(reference->next->next != NULL)    
	reference = reference->next;          
          
      last = reference->next;    
      reference->next = NULL;       
    }    
    
    g95_conv_expr(se, m);       
    desc = se->expr;   
   
    g95_set_section_info(se, 0, integer_one_node);      
      
    se->reflevel = 0;       
       
    for(n=0; n<last->u.ar.dimen; n++) {    
      g95_conv_expr(se, last->u.ar.start[n]); 
      g95_set_section_info(se, n+1, se->expr); 
    }      
      
  /* Rebuild the original expression */      
      
    if (reference == NULL)   
      m->ref = last;       
    else         
      reference->next = last;          
  }   
   
  var1 = g95_create_var(pchar_type_node, NULL);       
  TREE_ADDRESSABLE(var1) = 1; 
  tmp = build1(ADDR_EXPR, pvoid_type_node, var1);     
     
  tmp = g95_call_library(pvoid_type_node, PREFIX "array_init",   
			 desc, tmp, NULL_TREE);      
      
  var2 = g95_create_var(pvoid_type_node, NULL);        
  g95_add_modify_expr(&se->pre, var2, tmp);     
     
  se->expr = var2;     
     
  tmp = g95_call_library(void_type_node, PREFIX "array_done", var1, NULL_TREE);         
  g95_add_expr_to_block(&se->post, tmp);        
}      
      
      
    
    
/* g95_get_adesc_ubound()-- Given a descriptor and a dimension, return
 * the tree describing the multiplier. */         
         
tree g95_get_adesc_ubound(tree desc, tree dim) {          
tree tmp, index;        
int t;          
          
  desc = conditional_indirect(desc);      
  t = get_dimension(desc);  
  
  tmp = build(COMPONENT_REF, TREE_TYPE(array_info[t]), desc, array_info[t]);   
   
  index = fold(build(MULT_EXPR, g95_default_integer, dim, build_int_2(3, 0)));  
  index = fold(build(PLUS_EXPR, g95_default_integer, index,
		     build_int_2(-1, -1)));        
        
  return build(ARRAY_REF, g95_default_integer, tmp, index);      
}         
         
         
          
          
/* section_ref()-- Convert a section reference.  This amounts to
 * copying the reference information into the new descriptor and
 * calling the section_array() library function to calculate the new
 * descriptor.  The result of this whole thing is the new descriptor.
 * Vector subscripts are handled at a higher level and it is an error
 * to see them here. */       
       
static void section_ref(g95_se *se, g95_array_ref *r) { 
tree tmp, old_desc, new_desc;       
int w, n;     
     
  old_desc = se->expr;       
       
  new_desc = g95_create_var(TREE_TYPE(old_desc), NULL);        
        
  n = 0;         
  for(w=0; w<r->dimen; w++)   
    switch(r->dimen_type[w]) {      
    case DIMEN_ELEMENT:  
      g95_set_section_info(se, n++, integer_one_node); 
 
      g95_conv_expr0(se, r->start[w]);  
      g95_set_section_info(se, n++, se->expr);  
      break;  
  
    case DIMEN_RANGE:      
      g95_set_section_info(se, n++, integer_zero_node);        
        
      if (r->start[w] == NULL) 
	tmp = get_adesc_lbound0(old_desc, w);      
      else {        
	g95_conv_expr0(se, r->start[w]);      
	tmp = se->expr;      
      }    
    
      g95_set_section_info(se, n++, tmp);     
     
      if (r->end[w] == NULL)  
	tmp = get_adesc_ubound0(old_desc, w);      
      else {   
	g95_conv_expr0(se, r->end[w]);        
	tmp = se->expr;        
      }          
          
      g95_set_section_info(se, n++, tmp);

      if (r->stride[w] == NULL)    
	tmp = integer_one_node; 
      else {
	g95_conv_expr0(se, r->stride[w]);
	tmp = se->expr;     
      }       
       
      g95_set_section_info(se, n++, tmp);        
      break;          
          
    default:  
      g95_internal_error("section_ref(): Bad dimension type");        
    }         
         
  se->expr = new_desc; 
 
  /* Now call the subroutine that does the work */        
        
  old_desc = build1(ADDR_EXPR, pvoid_type_node, old_desc);          
  new_desc = build1(ADDR_EXPR, pvoid_type_node, new_desc);          
          
  tmp = g95_call_library(void_type_node, PREFIX "section_array",    
			 old_desc, new_desc, NULL_TREE);

  g95_add_expr_to_block(&se->pre, tmp);        
}          
          
          
          
          
/* explicit_storage()-- Convert an explicit array specification to a
 * constructor that will initialize the descriptor.  With luck, this
 * constructor can be constant.  A tree representing the total size of
 * the array is returned if the expression is constant, NULL_TREE
 * otherwise. */          
          
static tree explicit_storage(g95_array_spec *as, tree element_size) { 
tree tmp, extent;   
int y;    
    
  convert_bounds(as);         
         
  current_desc.esize = save_expr(element_size);      
      
  for(y=0; y<as->rank; y++) {    /* Calculate the multipliers */ 
    if (y == 0) {      
      current_desc.info[y].mult = current_desc.esize;  
      continue;  
    }         
         
    extent = build(PLUS_EXPR, g95_default_integer,      
		   current_desc.info[y-1].ubound, integer_one_node);     
    extent = build(MINUS_EXPR, g95_default_integer, extent,        
		   current_desc.info[y-1].lbound);          
    extent = save_expr(fold(extent));      
      
    /* Make sure the extent is nonnegative */         
         
    tmp = build(GE_EXPR, boolean_type_node, extent, integer_zero_node);        
    extent = fold(build(COND_EXPR, g95_default_integer, tmp, extent, 
			integer_zero_node));        
        
    tmp = build(MULT_EXPR, g95_default_integer, extent,        
		current_desc.info[y-1].mult);         
         
    current_desc.info[y].mult = fold(tmp); 
  }     
     
  /* Build one more 'multiplier', which is the total size of the array */    
    
  y = as->rank - 1; 
 
  extent = build(PLUS_EXPR, g95_default_integer, current_desc.info[y].ubound,        
		 integer_one_node);
  extent = build(MINUS_EXPR, g95_default_integer, extent,
		 current_desc.info[y].lbound);          
  extent = save_expr(fold(extent));          
          
  tmp = build(GE_EXPR, boolean_type_node, extent, integer_zero_node);         
  extent = build(COND_EXPR, g95_default_integer, tmp, extent, 
		 integer_zero_node);          
  extent = fold(extent);

  tmp = fold(build(MULT_EXPR, g95_default_integer, extent,  
		   current_desc.info[y].mult));         
         
  if (TREE_CODE(tmp) == INTEGER_CST) return tmp;   
   
  current_desc.size = tmp; 
  return NULL_TREE;       
}         
         
         
       
       
/* conv_array_initializer()-- Create an array constructor from an
 * initialization expression.  Does not handle nonconstant arrays
 * yet. */      
      
static tree conv_array_initializer(tree type, g95_expr *expr, 
				   tree string_length) {   
g95_constructor *x;    
tree list, tmp, cons;

  list = NULL_TREE;       
       
  for(x=expr->value.constructor; x; x=x->next) {   
    if (x->iterator)      
      g95_internal_error("g95_conv_array_initializer(): "       
			 "Iterators not supported yet");   
   
    assert(x->expr->type == EXPR_CONSTANT);          
          
    tmp = g95_conv_constant(x->expr, string_length);     
    list = tree_cons(NULL_TREE, tmp, list);   
  }     
     
  list = nreverse(list);         
  cons = build(CONSTRUCTOR, type, NULL_TREE, list);       
  TREE_CONSTANT(cons) = 1;    
    
  return cons;          
}      
      
      
    
    
/* g95_get_array_storage()-- Get a declaration for storage associated
 * with an array, or NULL_TREE if the storage is obtained at runtime.
 * As a side effect, the current_desc variable is initialized to a
 * constructor that initializes the descriptor.  This value is grabbed
 * later. */      
      
tree g95_get_array_storage(variable_info *vinfo, tree string_length) {          
tree tmp, etype, storage, element_size;         
         
  if (vinfo->as->type == AS_DEFERRED) return NULL_TREE;       
       
  assert(vinfo->as->type == AS_EXPLICIT); 
  etype = void_type_node;          
          
  if (vinfo->ts.type != BT_CHARACTER) { 
    etype = g95_typenode_for_spec(&vinfo->ts);       
    element_size = size_in_bytes(etype);    
  } else {     
    element_size = string_length;       
       
    if (INTEGER_CST_P(element_size)) {        
      tmp = build_range_type(g95_default_integer, integer_one_node,  
			     element_size);   
      etype = build_array_type(char_type_node, tmp); 
    }          
  }    
    
  tmp = explicit_storage(vinfo->as, element_size);  
  if (tmp == NULL_TREE) return NULL_TREE;       
       
  /* An array can be an array of zero length characters, so head off
   * the nasty 0/0 when calculating the total amount of storage
   * needed. */        
        
  if (TREE_INT_CST_LOW(element_size)  == 0 &&     
      TREE_INT_CST_HIGH(element_size) == 0)
    tmp = integer_zero_node;   
  else        
    tmp = fold(build(TRUNC_DIV_EXPR, g95_default_integer, tmp,        
		     element_size));   
   
  tmp = build_range_type(g95_default_integer, integer_one_node, tmp);      
      
  storage = build_array_type(etype, tmp);      
  return storage;
}        
        
        
      
      
/* array_pointer_argument()-- Convert an actual argument that is an
 * array pointer. */       
       
static void array_pointer_argument(g95_se *se, g95_actual_arglist *actual) {      
symbol_attribute attr; 
 
  attr = g95_variable_attr(actual->u.expr, NULL);        
        
  se->reflevel = (actual->u.expr->ts.type == BT_CHARACTER && attr.pointer)    
    ? 1 : 2;     
     
  g95_conv_expr(se, actual->u.expr);     
}        
        
        
       
       
/* g95_array_argument()-- Convert an actual argument that is an array
 * or array pointer. */    
    
void g95_array_argument(g95_se *se, g95_actual_arglist *arg) {          
          
  if (arg->pointer)         
    array_pointer_argument(se, arg);         
  else {
    if (arg->type != ARRAY_DESC)   
      array_desc_argument(se, arg);         
    else {         
      se->reflevel = 1;  
      g95_conv_expr(se, arg->u.expr);   
    }          
  }         
}   
     
     
/* explicit_descriptor()-- Construct a descriptor for an explicit array. */   
   
static void explicit_descriptor(variable_info *vinfo, tree desc_var,          
				tree storage) {   
tree tmp, node;         
int z, rank;   
   
  rank = get_dimension(desc_var);       
       
  /* Calculate the offset */  
  
  if (storage != NULL_TREE)  
    node = build1(ADDR_EXPR, pchar_type_node, storage);   
  else {  /* Allocate the array on the heap */          
    tmp = g95_adesc_base(desc_var); 
    tmp = g95_call_procedure_alloc(tmp, current_desc.size);   
   
    g95_add_expr_to_block(&g95_context->pre, tmp);     
    node = g95_adesc_base(desc_var);        
  }    
    
  /* Initialize the offset pointer */

  for(z=0; z<rank; z++) {          
    tmp = fold(build(MULT_EXPR, g95_default_integer,   
		     current_desc.info[z].lbound,
		     current_desc.info[z].mult));

    node = fold(build(MINUS_EXPR, g95_default_integer, node, tmp));   
  }         
         
  if (storage != NULL_TREE)   
    current_desc.offset = node;   
  else {     
    current_desc.offset = null_pointer_node;        
        
    tmp = get_adesc_offset(desc_var);  
    g95_add_modify_expr(&g95_context->pre, tmp, node);   
  } 
 
  if (vinfo->value) {   
    DECL_INITIAL(storage) = 
      conv_array_initializer(TREE_TYPE(storage), vinfo->value,       
			     current_desc.esize);        
        
    TREE_STATIC(storage) = 1;          
  }  
}        
        
        
          
          
/* element_ref()-- Convert an array reference to an array element */      
      
static void element_ref(g95_se *se, g95_array_ref *r, g95_typespec *ts) {   
tree type, desc, tmp, pointer;
int f, rank;     
g95_se se0;         
         
  desc = se->expr;         
  pointer = get_adesc_offset(desc);          
          
  rank = r->as->rank;        
        
  for(f=0; f<rank; f++) {      
    g95_init_se(&se0, NULL);       
    g95_conv_expr(&se0, r->start[f]);     
     
    g95_add_block_to_block(&se->pre, &se0.pre);     
    g95_add_block_to_block(&se->post, &se0.post);         
         
    tmp = get_adesc_multiplier0(desc, f);      
    tmp = build(MULT_EXPR, g95_default_integer, se0.expr, tmp);        
    pointer = fold(build(PLUS_EXPR, g95_default_integer, tmp, pointer));    
  }  
  
  /* Now that we have a pointer to the array element, what happens
   * next depends on the type of the element.  If the element is a
   * simple scalar or derived type, then the pointer is left as is.
   * We can't have arrays within arrays, so only the basic type is
   * important. */  
  
  type = g95_typenode_for_spec(ts);  
  
  se->expr = convert(build_pointer_type(type), pointer);     
}         
         
         
      
      
/* g95_fix_dummy_array()-- At this point, sym is a dummy array
 * argument.  The backend_decl is a pointer to the base of an array
 * that has just been passed in.  We have to create a real descriptor
 * for the array, which becomes the new value of the symbol. */         
         
void g95_fix_dummy_array(g95_symbol *s) {     
tree tmp, desc, base, actual_arg;  
char *sub;         
         
  if (s->attr.pointer || s->as->type == AS_DEFERRED) return;          
  /* Everything is OK as is */  
  
  actual_arg = s->backend_decl;      
      
  desc = g95_create_var(array_type_node[s->as->rank], NULL);     
  s->backend_decl = desc;   
   
  /* Convert bounds for the descriptor */  
  
  convert_bounds(s->as);   
   
  /* Extract the array base and determine element size */  
  
  current_desc.esize =   
    (s->ts.type == BT_CHARACTER && s->ts.cl->length == NULL)      
    ? G95_TYPE_STRING_LENGTH(TREE_TYPE(actual_arg))      
    : size_in_bytes(g95_typenode_for_spec(&s->ts));       
       
  base = actual_arg;          
          
  DECL_INITIAL(desc) = build_desc_constructor(s->as->rank);  
  
  tmp = build1(ADDR_EXPR, pvoid_type_node, desc);   
   
  switch(s->as->type) {     
  case AS_ASSUMED_SHAPE: 
    sub = PREFIX "init_assumed_shape";        
    break; 
 
  case AS_EXPLICIT: 
  case AS_ASSUMED_SIZE:    
    sub = PREFIX "init_dummy_descriptor";          
    break;     
     
  default:    
    g95_internal_error("g95_fix_dummy_array(): Bad array spec");     
  }   
   
  tmp = g95_call_library(void_type_node, sub, base, tmp, NULL_TREE);  
  g95_add_expr_to_block(&g95_context->pre, tmp);    
}  
  
  
     
     
/* g95_conv_array_ref()-- Given an array descriptor and an array
 * reference, calculate the array reference.  For full arrays, this is
 * just the descriptor.  For sections, we return a new descriptor that
 * describes the section.  For an element, the element is returned.
 * For character elements, a character descriptor is returned. */  
  
void g95_conv_array_ref(g95_se *se, g95_array_ref *reference, g95_typespec *ts) {          
          
  switch(reference->type) {   
  case AR_FULL:   
    break;    /* Already done */      
      
  case AR_ELEMENT:       
    g95_reflevel(se, 0);      
    element_ref(se, reference, ts);        
    break;        
        
  case AR_SECTION:        
    g95_reflevel(se, 0);       
    section_ref(se, reference);       
    break;

  default:          
    g95_internal_error("g95_conv_array_ref(): Bad ref");        
  }       
}  
  
  
 
 
/* g95_init_array_desc()-- Given a variable that is an array
 * descriptor, initialize it with its initial value.  For array
 * descriptors, this has already been calculated and has been waiting
 * for this moment. */ 
 
void g95_init_array_desc(variable_info *vinfo, tree desc_var, tree storage) {          
          
  switch(vinfo->as->type) {        
  case AS_EXPLICIT:         
    explicit_descriptor(vinfo, desc_var, storage);      
    break;      
      
  case AS_DEFERRED:
    deferred_descriptor(vinfo);        
    break;       
       
  default:    
    abort();     
  }     
     
  DECL_INITIAL(desc_var) = build_desc_constructor(vinfo->as->rank);  
  TREE_READONLY(desc_var) = 1;          
}    
    
    
