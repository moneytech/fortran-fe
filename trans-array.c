 
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
 
 
       
       
/* conditional_pointer()-- Given a variable which might be a
 * descriptor, return a pointer to the descriptor. */     
     
static tree conditional_pointer(tree node) {         
int rank;     
     
  for(rank=1; rank<=G95_MAX_DIMENSIONS; rank++) {       
    if (TREE_TYPE(node) == array_ptype_node[rank]) return node;   
   
    if (TREE_TYPE(node) == array_type_node[rank])      
      return build1(ADDR_EXPR, array_ptype_node[rank], node);  
  }       
       
  g95_internal_error("conditional_pointer(): Not a descriptor");
}         
         
         
    
    
/* init_array_type()-- Initialize array types.  Zero dimensional
 * arrays are not allowed and are not initialized here. */   
   
void g95_init_array_types(void) {          
tree tmp, t; 
char name[20];     
int r;       
       
  for(r=1; r<=G95_MAX_DIMENSIONS; r++) {   
    t = make_node(RECORD_TYPE);  
  
    sprintf(name, "array%d", r);      
    TYPE_NAME(t) = get_identifier(name);          
          
    array_offset[r]   = g95_add_field(t, "offset",  pchar_type_node); 
    array_rank[r]     = g95_add_field(t, "rank",    g95_default_integer);         
    array_esize[r]    = g95_add_field(t, "esize",   g95_default_integer);   
    array_base[r]     = g95_add_field(t, "base",    pvoid_type_node);      
      
    tmp = build_int_2(3*r-1, 0);          
    tmp = build_range_type(g95_default_integer, integer_zero_node, tmp);       
    tmp = build_array_type(g95_default_integer, tmp);    
    
    array_info[r] = g95_add_field(t, "info", tmp);  
    g95_finish_type(t);         
         
    G95_DESCRIPTOR_P(t) = 1;        
    array_type_node[r] = t;       
    array_ptype_node[r] = build_pointer_type(t);       
  }   
   
  /* Build the section_info[] array. */   
   
  tmp = build_int_2(4*G95_MAX_DIMENSIONS, 0);    
  tmp = build_range_type(g95_default_integer, integer_zero_node, tmp);  
  tmp = build_array_type(g95_default_integer, tmp);        
        
  section_info =         
    build_decl(VAR_DECL, get_identifier(PREFIX "section_info"), tmp);          
  DECL_EXTERNAL(section_info) = 1;    
} 
 
 
       
       
/* g95_get_array_desc()-- Get an array descriptor type of a particular
 * dimension. */          
          
tree g95_get_array_desc(int dimension) {         
         
  assert(dimension > 0 && dimension <= G95_MAX_DIMENSIONS);   
   
  return array_type_node[dimension];   
}   
   
   
    
    
/* conditional_indirect()-- Given a variable which might be a pointer
 * to a descriptor, insert the indirection to return a reference to
 * the descriptor. */        
        
static tree conditional_indirect(tree dec) {          
g95_se se;      
int t;      
      
  g95_init_se(&se, NULL);         
         
  se.expr = dec;         
  g95_reflevel(&se, 0);   
   
  for(t=1; t<=G95_MAX_DIMENSIONS; t++) 
    if (TREE_TYPE(se.expr) == array_type_node[t]) return se.expr;      
      
  g95_internal_error("conditional_indirect(): Not a descriptor");   
}         
         
         
 
 
/* get_dimension()-- Given a descriptor variable, figure out what
 * dimension it represents.  This forms an index into the component
 * arrays to get the right component of the right type. */    
    
static int get_dimension(tree d) {          
tree typ;       
int dim;      
      
  typ = TREE_TYPE(d);  
  
  for(dim=1; dim<=G95_MAX_DIMENSIONS; dim++)          
    if (array_type_node[dim] == typ ||       
	array_ptype_node[dim] == typ) return dim;      
      
  g95_internal_error("get_dimension(): Bad array type");     
}         
         
         
         
         
/* g95_get_adesc_ubound()-- Given a descriptor and a dimension, return
 * the tree describing the multiplier. */    
    
tree g95_get_adesc_ubound(tree desc, tree rank) {
tree t, i;       
int o;

  desc = conditional_indirect(desc);         
  o = get_dimension(desc);       
       
  t = build(COMPONENT_REF, TREE_TYPE(array_info[o]), desc, array_info[o]);

  i = fold(build(MULT_EXPR, g95_default_integer, rank, build_int_2(3, 0)));   
  i = fold(build(PLUS_EXPR, g95_default_integer, i,  
		     build_int_2(-1, -1))); 
 
  return build(ARRAY_REF, g95_default_integer, t, i);        
}      
      
      
  
  
/* g95_get_adesc_esize()-- Given a descriptor node, return a reference
 * to the element size member. */  
  
tree g95_get_adesc_esize(tree desc) {
int e;        
        
  desc = conditional_indirect(desc);   
  e = get_dimension(desc);        
        
  return build(COMPONENT_REF, TREE_TYPE(array_esize[e]), desc, array_esize[e]);          
}   
   
   
         
         
static tree get_adesc_rank(tree array) {
int e;       
       
  array = conditional_indirect(array);           
  e = get_dimension(array);         
         
  return build(COMPONENT_REF, TREE_TYPE(array_rank[e]), array, array_rank[e]);     
}     
     
     
        
        
/* make_pointer()-- Given an object, return a pointer to the object if
 * it isn't a pointer already. */          
          
static tree make_pointer(tree object) {
tree type;         
         
  if (!POINTER_TYPE_P(TREE_TYPE(object))) { 
    type = build_pointer_type(TREE_TYPE(object));   
    object = build1(ADDR_EXPR, type, object);          
  }  
  
  return object;    
}    
    
    
   
   
/* get_adesc_offset()-- Given a descriptor node, return a reference to
 * the offset member. */        
        
static tree get_adesc_offset(tree des) {  
int t;

  des = conditional_indirect(des);         
  t = get_dimension(des);    
    
  return build(COMPONENT_REF, TREE_TYPE(array_offset[t]), des,
	       array_offset[t]);       
} 
 
 
    
    
/* g95_adesc_base()-- Given a descriptor node, return a reference to
 * the base member. */    
    
tree g95_adesc_base(tree desc) {         
int v;     
     
  desc = conditional_indirect(desc);          
  v = get_dimension(desc);          
          
  return build(COMPONENT_REF, TREE_TYPE(array_base[v]), desc, array_base[v]);  
}         
         
         
         
         
/* g95_set_section_info()-- Generate an instruction that modifies the
 * section_info[] array.  The format of this array is a series of
 * records for each dimension of the original array.  Records start
 * with a word flag that is nonzero for a dimension with an element or
 * zero for a section.  For a dimension with an element, a word
 * follows that gives the element number.  For a dimension with a
 * range, three words follow giving the start, end and stride. */    
    
void g95_set_section_info(g95_se *se, int i, tree value) {       
tree tmp;     
     
  tmp = build(ARRAY_REF, g95_default_integer, section_info, build_int_2(i, 0));
  g95_add_modify_expr(&se->pre, tmp, value);     
}        
        
        
      
      
/* g95_get_adesc_lbound()-- Given a descriptor and a dimension, return
 * the tree describing the multiplier. */      
      
tree g95_get_adesc_lbound(tree array, tree dim) {
tree t, idx;    
int r;        
        
  array = conditional_indirect(array);    
  r = get_dimension(array); 
 
  t = build(COMPONENT_REF, TREE_TYPE(array_info[r]), array, array_info[r]);  
  
  idx = fold(build(MULT_EXPR, g95_default_integer, dim, build_int_2(3, 0))); 
  idx = fold(build(PLUS_EXPR, g95_default_integer, idx,       
		     build_int_2(-2, -1)));    
    
  return build(ARRAY_REF, g95_default_integer, t, idx);  
}        
        
        
 
 
/* deferred_descriptor()-- Construct a descriptor for a deferred array. */        
        
static void deferred_descriptor(variable_info *vinfo) {    
int r, dim;        
        
  dim = vinfo->as->rank;      
     
  current_desc.offset = null_pointer_node;      
  current_desc.dimensions = build_int_2(dim, 0);    
  current_desc.esize = integer_zero_node;         
  current_desc.base = null_pointer_node;    
    
  for(r=0; r<dim; r++) {         
    current_desc.info[r].mult   = integer_zero_node;      
    current_desc.info[r].lbound = integer_zero_node;     
    current_desc.info[r].ubound = integer_zero_node; 
  }  
} 
 
 


/* get_adesc_info()-- Given a descriptor, access a constant element
 * from the info[] array.  If the descriptor has an initial (constant)
 * value, then return that value. */ 
 
static tree get_adesc_info(tree desc, int e) {      
tree s;      
int w;      
      
  desc = conditional_indirect(desc);  
  s = NULL_TREE;        
        
  if (G95_CONSTANT_DESC(desc) && DECL_INITIAL(desc) != NULL) {         
    s = DECL_INITIAL(desc);  
    s = TREE_OPERAND(s, 1);         
         
    for(w=0; w<4; w++)        
      s = TREE_CHAIN(s);      
      
    s = TREE_VALUE(s);          
    s = TREE_OPERAND(s, 1);          
          
    while(s != NULL_TREE) {         
      if (TREE_INT_CST_LOW(TREE_PURPOSE(s)) == (unsigned) e) {       
	s = TREE_VALUE(s);         
	break;      
      }     
     
      s = TREE_CHAIN(s);         
    }       
  }

  if (s == NULL_TREE) {
    w = get_dimension(desc);  
  
    s = build(COMPONENT_REF, TREE_TYPE(array_info[w]), desc, array_info[w]);
    s = build(ARRAY_REF, g95_default_integer, s, build_int_2(e, 0));         
  } 
 
  return s;
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
         
void g95_temp_array_descriptor(g95_se *s, variable_info *vin, tree array,          
			       tree storage) {
tree v, tmp, off, element_size, lbound, ubound, mult, last_mult;
g95_array_spec *ref;  
g95_se se2;    
int i;      
      
  ref = vin->as;   
  last_mult = NULL_TREE;          
  g95_init_se(&se2, NULL);

  v = get_adesc_rank(array);     
  g95_add_modify_expr(&s->pre, v, build_int_2(ref->rank, 0));    
    
  v = g95_get_adesc_esize(array);   
  element_size = size_in_bytes(g95_typenode_for_spec(&vin->ts));    
  g95_add_modify_expr(&s->pre, v, element_size);  
  
  off = build1(ADDR_EXPR, pchar_type_node, storage); 
 
  for(i=0; i<ref->rank; i++) {
    v = get_adesc_lbound0(array, i);             
          
    g95_conv_constant(&se2, ref->lower[i]);  
    lbound = se2.expr;         
    g95_add_modify_expr(&s->pre, v, lbound);    
    
    v = get_adesc_ubound0(array, i);       
    g95_conv_constant(&se2, ref->upper[i]);    
    ubound = se2.expr;        
    g95_add_modify_expr(&s->pre, v, ubound);        
        
    if (i == 0)
      mult = element_size;  
    else {          
      tmp = fold(build(MINUS_EXPR, g95_default_integer, ubound, lbound));      
      tmp = fold(build(PLUS_EXPR, g95_default_integer, tmp, integer_one_node));          
          
      mult = fold(build(MULT_EXPR, g95_default_integer, tmp, last_mult));   
    }

    last_mult = mult;    
    
    v = get_adesc_multiplier0(array, i);   
    g95_add_modify_expr(&s->pre, v, mult);    
    
    tmp = fold(build(MULT_EXPR, g95_default_integer, mult, lbound));  
    off = fold(build(MINUS_EXPR, pchar_type_node, off, tmp));
  }     
         
  v = get_adesc_offset(array);      
  g95_add_modify_expr(&s->pre, v, off);      
}      
      
      
    
    
/* g95_nullify_array_pointer()-- Given an array pointer, generate code
 * to nullify the pointer. */       
       
void g95_nullify_array_pointer(stmtblock_t *body, tree arr) {          
tree tmp;    
int f, b; 
 
  arr = save_expr(arr);          
          
  tmp = get_adesc_offset(arr);   
  g95_add_modify_expr(body, tmp, null_pointer_node);

  tmp = g95_adesc_base(arr);       
  g95_add_modify_expr(body, tmp, null_pointer_node);      
      
  b = get_dimension(arr);

  for(f=0; f<b; f++) {
    tmp = get_adesc_multiplier0(arr, f);          
    g95_add_modify_expr(body, tmp, integer_zero_node);       
  }       
}


      
      
/* convert_bounds()-- Given an array spec, convert the expressions
 * forming the bounds into the descriptor.  */         
         
static void convert_bounds(g95_array_spec *a, tree array) {         
stmtblock_t post;     
g95_se s;         
tree t; 
int l;          
          
  current_desc.dimensions = build_int_2(a->rank, 0);   
   
  g95_init_block(&post);       
       
  for(l=0; l<a->rank; l++) {          
    if (a->lower[l] == NULL && a->type == AS_ASSUMED_SHAPE)
      current_desc.info[l].lbound = integer_one_node;      
      
    if (a->lower[l] != NULL) {         
      g95_init_se(&s, NULL);        
      g95_conv_expr(&s, a->lower[l]);      
      
      g95_add_block_to_block(&g95_context->pre, &s.pre);      
      g95_add_block_to_block(&post, &s.post);      
      
      if (INTEGER_CST_P(s.expr))        
	current_desc.info[l].lbound = s.expr;        
      else {  
	t = get_adesc_lbound0(array, l); 
	g95_add_modify_expr(&g95_context->pre, t, s.expr);        
      }   
    }       
       
    if (a->upper[l] != NULL) {         
      g95_init_se(&s, NULL);
      g95_conv_expr(&s, a->upper[l]);          
          
      g95_add_block_to_block(&g95_context->pre, &s.pre);
      g95_add_block_to_block(&post, &s.post); 
 
      if (INTEGER_CST_P(s.expr))
	current_desc.info[l].ubound = s.expr; 
      else {    
	t = get_adesc_ubound0(array, l);      
	g95_add_modify_expr(&g95_context->pre, t, s.expr);       
      }    
    }         
  }    
    
  g95_add_block_to_block(&g95_context->pre, &post);         
}         
         
         
        
        
/* g95_build_desc_constructor()-- Given the junk in the current
 * descriptor, build a constructor for the descriptor. */          
          
tree g95_build_desc_constructor(int dim) { 
tree node, info_node, tmp0;   
int u;  
  
  tmp0 = current_desc.offset;    
  if (tmp0 == NULL_TREE) tmp0 = null_pointer_node;      
  node = tree_cons(array_offset[dim], tmp0, NULL_TREE); 
 
  tmp0 = current_desc.dimensions;          
  if (tmp0 == NULL_TREE) tmp0 = integer_zero_node;        
  node = tree_cons(array_rank[dim], tmp0, node);      
      
  tmp0 = current_desc.esize;          
  if (tmp0 == NULL_TREE) tmp0 = integer_zero_node; 
  node = tree_cons(array_esize[dim], tmp0, node);      
      
  tmp0 =  current_desc.base;
  if (tmp0 == NULL_TREE) tmp0 = null_pointer_node;          
  node = tree_cons(array_base[dim], tmp0, node);     
     
  info_node = NULL_TREE;      
      
  for(u=0; u<dim; u++) {     
    tmp0 = current_desc.info[u].mult;    
    if (tmp0 != NULL_TREE)    
      info_node = tree_cons(build_int_2(3*u, 0), tmp0, info_node);    
    
    tmp0 = current_desc.info[u].lbound;   
    if (tmp0 != NULL_TREE)       
      info_node = tree_cons(build_int_2(3*u+1, 0), tmp0, info_node);   
   
    tmp0 = current_desc.info[u].ubound;        
    if (tmp0 != NULL_TREE)  
      info_node = tree_cons(build_int_2(3*u+2, 0), tmp0, info_node);          
  }

  tmp0 = build_int_2(3*dim, 0);     
  tmp0 = build_range_type(g95_default_integer, integer_zero_node, tmp0);         
  tmp0 = build_array_type(g95_default_integer, tmp0);    
    
  info_node = build(CONSTRUCTOR, tmp0, NULL_TREE, nreverse(info_node));      
      
  node = tree_cons(array_info[dim], info_node, node);     
     
  node = build(CONSTRUCTOR, array_type_node[dim], NULL_TREE,   
	       nreverse(node));       
       
  /* Zero out the current constructor */

  current_desc.offset     = NULL_TREE;  
  current_desc.dimensions = NULL_TREE;      
  current_desc.esize      = NULL_TREE;    
  current_desc.base       = NULL_TREE;          
  current_desc.size       = NULL_TREE;     
     
  for(u=0; u<G95_MAX_DIMENSIONS; u++) {      
    current_desc.info[u].mult   = NULL_TREE;    
    current_desc.info[u].lbound = NULL_TREE;    
    current_desc.info[u].ubound = NULL_TREE;      
  }

  return node;        
}       
       
       
  
  
/* g95_fix_dummy_array()-- At this point, sym is a dummy array
 * argument.  The backend_decl is a pointer to the base of an array
 * that has just been passed in.  We have to create a real descriptor
 * for the array, which becomes the new value of the symbol. */       
       
void g95_fix_dummy_array(g95_symbol *symb) {        
tree tmp0, des, b, actual_arg;  
char *sub;         
         
  if (symb->attr.pointer || symb->as->type == AS_DEFERRED) return;       
  /* Everything is OK as is */  
  
  actual_arg = symb->backend_decl;       
       
  des = g95_create_var(array_type_node[symb->as->rank]);          
  symb->backend_decl = des;
  (tree) DECL_LANG_SPECIFIC(des) = actual_arg;    
    
  /* Convert bounds for the descriptor */      
      
  convert_bounds(symb->as, des);       
       
  /* Extract the array base and determine element size */     
     
  current_desc.esize = 
    (symb->ts.type == BT_CHARACTER && symb->ts.cl->length == NULL)         
    ? symb->ts.cl->backend_decl 
    : size_in_bytes(g95_typenode_for_spec(&symb->ts));  
  
  b = actual_arg; 
 
  DECL_INITIAL(des) = g95_build_desc_constructor(symb->as->rank);    
    
  tmp0 = build1(ADDR_EXPR, pvoid_type_node, des); 
 
  switch(symb->as->type) {       
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
        
  tmp0 = g95_call_library(void_type_node, sub, b, tmp0, NULL_TREE);       
  g95_add_expr_to_block(&g95_context->pre, tmp0);  
}        
        
        
          
          
/* array_desc_argument()-- Convert an actual argument that is a full
 * array or array element.  The value passed to the procedure is a
 * pointer to the first array element for a full array, or the
 * specified array element.  The incoming procedure expects the array
 * data to be contiguous.  If not, the data must be copied into a
 * contiguous block, and unpacked when the procedure exits.  This is
 * all handled by library subroutines.  For character arrays, we pass
 * a pointer to a descriptor of the first element. */      
      
static void array_desc_argument(g95_se *se1, g95_actual_arglist *actual) {    
tree tmp0, var1, var2, desc;   
g95_ref *reference, *last;        
g95_expr *c;
int n;  
  
  if (actual->type == FULL_ARRAY) {      
    g95_set_section_info(se1, 0, integer_zero_node);         
         
    se1->reflevel = 1;       
    g95_conv_expr(se1, actual->u.expr);       
    reference = NULL;       
    desc = se1->expr;     
     
  } else {       
    c = actual->u.expr;       
       
    if (c->ref->next == NULL) {      
      last = c->ref;   
      reference = NULL;        
      c->ref = NULL;          
    } else { 
      reference = c->ref;     
      while(reference->next->next != NULL)       
	reference = reference->next;          
          
      last = reference->next; 
      reference->next = NULL;       
    }       
       
    se1->reflevel = 1;          
    g95_conv_expr(se1, c);    
    desc = se1->expr;         
         
    g95_set_section_info(se1, 0, integer_one_node);    
    
    se1->reflevel = 0;      
      
    for(n=0; n<last->u.ar.dimen; n++) {      
      g95_conv_expr(se1, last->u.ar.start[n]);         
      g95_set_section_info(se1, n+1, se1->expr);  
    }      
      
  /* Rebuild the original expression */     
     
    if (reference == NULL)     
      c->ref = last;
    else       
      reference->next = last;   
  }          
          
  var1 = g95_create_var(pchar_type_node);  
  TREE_ADDRESSABLE(var1) = 1;
  tmp0 = build1(ADDR_EXPR, pvoid_type_node, var1);          
          
  tmp0 = g95_call_library(pvoid_type_node, PREFIX "contiguous_array",
			 desc, tmp0, null_pointer_node, NULL_TREE);      
      
  var2 = g95_create_var(pvoid_type_node);
  g95_add_modify_expr(&se1->pre, var2, tmp0);     
     
  se1->expr = var2;        
        
  tmp0 = g95_call_library(void_type_node, PREFIX "contiguous_array_done",          
			 var1, integer_one_node, NULL_TREE);      
  g95_add_expr_to_block(&se1->post, tmp0);     
}


         
         
/* explicit_storage()-- Convert an explicit array specification to a
 * constructor that will initialize the descriptor.  With luck, this
 * constructor will be constant.  A tree representing the total size
 * of the array is returned if the expression is constant, NULL_TREE
 * otherwise. */    
    
static tree explicit_storage(g95_array_spec *ref, tree desc, tree element_size){          
tree t, var0, ext;    
int i;   
   
  convert_bounds(ref, desc);  
  
  current_desc.esize = save_expr(element_size);
  current_desc.base = integer_one_node;  /* Looks allocated */     
     
  for(i=0; i<ref->rank; i++) {    /* Calculate the multipliers */ 
    if (i == 0) {      
      if (INTEGER_CST_P(current_desc.esize))    
	current_desc.info[0].mult = current_desc.esize;        
      else {       
	var0 = get_adesc_multiplier0(desc, 0);          
	g95_add_modify_expr(&g95_context->pre, var0, current_desc.esize); 
      }   
   
      continue;   
    }     
     
    t = current_desc.info[i-1].ubound; 
    if (t == NULL_TREE) t = get_adesc_ubound0(desc, i-1); 
 
    ext = fold(build(PLUS_EXPR, g95_default_integer, t,        
			integer_one_node));       
       
    t = current_desc.info[i-1].lbound;       
    if (t == NULL_TREE) t = get_adesc_lbound0(desc, i-1);      
      
    ext = fold(build(MINUS_EXPR, g95_default_integer, ext, t));   
    ext = save_expr(ext);    
    
    /* Make sure the extent is nonnegative */

    ext = build(MAX_EXPR, g95_default_integer, integer_zero_node, ext);   
    ext = fold(ext);        
        
    t = current_desc.info[i-1].mult;       
    if (t == NULL_TREE) t = get_adesc_multiplier0(desc, i-1); 
 
    t = fold(build(MULT_EXPR, g95_default_integer, ext, t));      
      
    if (INTEGER_CST_P(t))  
      current_desc.info[i].mult = fold(t);
    else {      
      var0 = get_adesc_multiplier0(desc, i);         
      g95_add_modify_expr(&g95_context->pre, var0, t);       
    }          
  }        
        
  /* Build one more 'multiplier', which is the total size of the array */   
   
  i = ref->rank - 1;        
        
  t = current_desc.info[i].ubound;   
  if (t == NULL_TREE) t = get_adesc_ubound0(desc, i);    
    
  ext = fold(build(PLUS_EXPR, g95_default_integer, t, integer_one_node));  
  
  t = current_desc.info[i].lbound;  
  if (t == NULL_TREE) t = get_adesc_lbound0(desc, i);

  ext = fold(build(MINUS_EXPR, g95_default_integer, ext, t));          
  ext = save_expr(ext); 
 
  ext = build(MAX_EXPR, g95_default_integer, integer_zero_node, ext);   
  ext = fold(ext);    
    
  t = current_desc.info[i].mult;       
  if (t == NULL_TREE) t = get_adesc_multiplier0(desc, i);      
      
  t = fold(build(MULT_EXPR, g95_default_integer, ext, t));         
         
  current_desc.size = t;   
  return (TREE_CODE(t) == INTEGER_CST) ? t : NULL_TREE;      
}   
   
   
      
      
/* explicit_descriptor()-- Construct a descriptor for an explicit array. */   
   
static void explicit_descriptor(tree desc_var, tree storage) {   
tree tmp, node, lbound, mult;     
int d, rank;     
     
  rank = get_dimension(desc_var);    
    
  /* Calculate the offset */  
  
  if (storage != NULL_TREE)         
    node = make_pointer(storage);   
  else {  /* Allocate the array on the heap */   
    tmp = g95_adesc_base(desc_var);      
    tmp = g95_call_procedure_alloc(tmp, current_desc.size);         
         
    g95_add_expr_to_block(&g95_context->pre, tmp);         
    node = g95_adesc_base(desc_var);         
  } 
 
  /* Initialize the offset pointer */ 
 
  for(d=0; d<rank; d++) {    
    lbound = current_desc.info[d].lbound; 
    if (lbound == NULL_TREE) lbound = get_adesc_lbound0(desc_var, d);

    mult = current_desc.info[d].mult;  
    if (mult == NULL_TREE) mult = get_adesc_multiplier0(desc_var, d); 
 
    tmp = fold(build(MULT_EXPR, g95_default_integer, lbound, mult));    
    node = fold(build(MINUS_EXPR, g95_default_integer, node, tmp));   
  }       
       
  if (storage != NULL_TREE)       
    current_desc.offset = node;          
  else {
    current_desc.offset = null_pointer_node;   
   
    tmp = get_adesc_offset(desc_var);   
    g95_add_modify_expr(&g95_context->pre, tmp, node); 
  }       
}


       
       
/* section_ref()-- Convert a section reference.  This amounts to
 * copying the reference information into the new descriptor and
 * calling the section_array() library function to calculate the new
 * descriptor.  The result of this whole thing is the new descriptor.
 * Vector subscripts are handled at a higher level and it is an error
 * to see them here. */ 
 
static void section_ref(g95_se *se0, g95_array_ref *reference) {     
tree tmp1, old_desc, new_desc;
int w, h;     
     
  old_desc = se0->expr; 
 
  new_desc = g95_create_var(TREE_TYPE(old_desc));     
     
  h = 0; 
  for(w=0; w<reference->dimen; w++)
    switch(reference->dimen_type[w]) {   
    case DIMEN_ELEMENT:      
      g95_set_section_info(se0, h++, integer_one_node);      
      
      g95_conv_expr0(se0, reference->start[w]); 
      g95_set_section_info(se0, h++, se0->expr);
      break;  
  
    case DIMEN_RANGE:          
      g95_set_section_info(se0, h++, integer_zero_node);  
  
      if (reference->start[w] == NULL)     
	tmp1 = get_adesc_lbound0(old_desc, w);       
      else {   
	g95_conv_expr0(se0, reference->start[w]);        
	tmp1 = se0->expr;
      }         
         
      g95_set_section_info(se0, h++, tmp1);        
        
      if (reference->end[w] == NULL)        
	tmp1 = get_adesc_ubound0(old_desc, w);
      else {    
	g95_conv_expr0(se0, reference->end[w]); 
	tmp1 = se0->expr;         
      }       
       
      g95_set_section_info(se0, h++, tmp1);   
   
      if (reference->stride[w] == NULL)     
	tmp1 = integer_one_node;  
      else { 
	g95_conv_expr0(se0, reference->stride[w]); 
	tmp1 = se0->expr;        
      }   
   
      g95_set_section_info(se0, h++, tmp1);  
      break;          
          
    default:       
      g95_internal_error("section_ref(): Bad dimension type");    
    }    
    
  se0->expr = new_desc;

  /* Now call the subroutine that does the work */

  old_desc = build1(ADDR_EXPR, pvoid_type_node, old_desc);      
  new_desc = build1(ADDR_EXPR, pvoid_type_node, new_desc);      
      
  tmp1 = g95_call_library(void_type_node, PREFIX "section_array",          
			 old_desc, new_desc, NULL_TREE);          
          
  g95_add_expr_to_block(&se0->pre, tmp1);       
}     
     
     
     
     
/* g95_array_argument()-- Convert an actual argument that is an array
 * or array pointer. */  
  
void g95_array_argument(g95_se *s, g95_actual_arglist *a) {       
       
  if (a->pointer) {          
    s->reflevel = 1;  
    g95_conv_expr(s, a->u.expr);       
  } else {
    if (a->type != ARRAY_DESC)        
      array_desc_argument(s, a);  
    else {  
      s->reflevel = 1;  
      g95_conv_expr(s, a->u.expr);  
    }
  } 
}          
       
       
/* g95_init_array_desc()-- Given a variable that is an array
 * descriptor, initialize it with its initial value.  For array
 * descriptors, this has already been calculated and has been waiting
 * for this moment. */        
        
void g95_init_array_desc(variable_info *vin, tree desc_var, tree storage) {    
    
  switch(vin->as->type) {    
  case AS_EXPLICIT: 
    explicit_descriptor(desc_var, storage);     
    G95_CONSTANT_DESC(desc_var) = 1;    
    break;         
         
  case AS_DEFERRED:
    deferred_descriptor(vin);        
    break;

  default:       
    abort();         
  }        
        
  DECL_INITIAL(desc_var) = g95_build_desc_constructor(vin->as->rank);   
  TREE_READONLY(desc_var) = 1;    
}          
          
          
    
    
/* element_ref()-- Convert an array reference to an array element */  
  
static void element_ref(g95_se *se1, g95_array_ref *ar, g95_typespec *t,          
			int as_flag) {    
tree typ, d, tmp1, pointer, g, k;      
int z, rnk;      
g95_se se2; 
 
  d = se1->expr; 
  pointer = get_adesc_offset(d);   
   
  rnk = ar->dimen;          
          
  for(z=0; z<rnk; z++) { 
    g95_init_se(&se2, NULL);          
    g95_conv_expr(&se2, ar->start[z]);      
      
    if (g95_option.bounds_check) {        
      se2.expr = save_expr(se2.expr);          
          
      g = fold(build(LT_EXPR, boolean_type_node, se2.expr,          
		      get_adesc_lbound0(d, z))); 
 
      if (!as_flag) {    
	k = fold(build(GT_EXPR, boolean_type_node, se2.expr,         
			get_adesc_ubound0(d, z)));      
      
	g = fold(build(TRUTH_ORIF_EXPR, boolean_type_node, g, k));  
      }        
        
      k = g95_call_library(void_type_node, PREFIX "array_oob", NULL);
      tmp1 = fold(build_v(COND_EXPR, g, k, empty_stmt_node));   
   
      g95_add_expr_to_block(&se2.pre, tmp1);     
    }     
     
    g95_add_block_to_block(&se1->pre,  &se2.pre);   
    g95_add_block_to_block(&se1->post, &se2.post);        
        
    tmp1 = get_adesc_multiplier0(d, z);    
    
    tmp1 = fold(build(MULT_EXPR, g95_default_integer, se2.expr, tmp1));       
    pointer = fold(build(PLUS_EXPR, g95_default_integer, tmp1, pointer));         
  }     
     
  /* Now that we have a pointer to the array element, what happens
   * next depends on the type of the element.  If the element is a
   * simple scalar or derived type, then the pointer is left as is.
   * We can't have arrays within arrays, so only the basic type is
   * important. */   
   
  typ = g95_typenode_for_spec(t);        
        
  se1->expr = convert(build_pointer_type(typ), pointer); 
}  
  
  
     
     
/* g95_transfer_result()-- Generate a one dimensional array descriptor
 * as the results of a TRANSFER() intrinsic. */        
        
tree g95_transfer_result(stmtblock_t *b, tree esize, tree src, tree s) {    
tree desc, t;          
          
  desc = g95_create_var(g95_get_array_desc(1)); 
 
  t = get_adesc_rank(desc);
  g95_add_modify_expr(b, t, integer_one_node);       
         
  t = g95_get_adesc_esize(desc);
  g95_add_modify_expr(b, t, esize);   
   
  t = get_adesc_multiplier0(desc, 0);
  g95_add_modify_expr(b, t, esize);  
  
  t = get_adesc_lbound0(desc, 0);   
  g95_add_modify_expr(b, t, integer_zero_node);       
       
  t = get_adesc_ubound0(desc, 0);  
  s = fold(build(MINUS_EXPR, g95_default_integer, s, integer_one_node));          
  g95_add_modify_expr(b, t, s);

  t = get_adesc_offset(desc);  
  g95_add_modify_expr(b, t, src);     
     
  return desc;      
}  
  
  
          
          
/* g95_conv_array_ref()-- Given an array descriptor and an array
 * reference, calculate the array reference.  For full arrays, this is
 * just the descriptor.  For sections, we return a new descriptor that
 * describes the section.  For an element, the element is returned.
 * For character elements, a character descriptor is returned. */   
   
void g95_conv_array_ref(g95_se *se0, g95_array_ref *reference, g95_typespec *typesp,          
			int as_flag) {     
     
  if (TREE_CODE(se0->expr) != VAR_DECL &&         
      TREE_CODE(se0->expr) != COMPONENT_REF)      
    se0->expr = save_expr(se0->expr);

  switch(reference->type) { 
  case AR_FULL:    
    break;    /* Already done */

  case AR_ELEMENT:
    g95_reflevel(se0, 0);         
    element_ref(se0, reference, typesp, as_flag);          
    break;          
          
  case AR_SECTION:      
    g95_reflevel(se0, 0);
    section_ref(se0, reference);       
    break;      
      
  default:          
    g95_internal_error("g95_conv_array_ref(): Bad ref");     
  }      
}   
   
   


/* g95_get_array_storage()-- Get a declaration for storage associated
 * with an array, or NULL_TREE if the storage is obtained at runtime.
 * As a side effect, the current_desc variable is initialized to a
 * constructor that initializes the descriptor.  This value is grabbed
 * later. */   
   
tree g95_get_array_storage(variable_info *vin) {         
tree tmp, etype, storage, element_size;        
        
  if (vin->as->type == AS_DEFERRED) return NULL_TREE;         
         
  assert(vin->as->type == AS_EXPLICIT);   
  etype = void_type_node;

  if (vin->ts.type != BT_CHARACTER) {   
    etype = g95_typenode_for_spec(&vin->ts);    
    element_size = size_in_bytes(etype);         
  } else {    
    element_size = vin->ts.cl->backend_decl;  
  
    if (INTEGER_CST_P(element_size)) {         
      tmp = build_range_type(g95_default_integer, integer_one_node,   
			     element_size);  
      etype = build_array_type(char_type_node, tmp); 
    }  
  }         
         
  tmp = explicit_storage(vin->as, vin->desc, element_size);    
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
       
       
