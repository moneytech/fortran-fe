         
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
        
static tree ac_list, ac_string_length; 
static int ac_count;   
   
  
  
/* init_array_type()-- Initialize array types.  Zero dimensional
 * arrays are not allowed and are not initialized here. */    
    
void g95_init_array_types(void) { 
tree tmp1, type;         
char name[20];      
int r;          
          
  for(r=1; r<=G95_MAX_DIMENSIONS; r++) {      
    type = make_node(RECORD_TYPE);    
    
    sprintf(name, "array%d", r);
    TYPE_NAME(type) = get_identifier(name);       
       
    array_offset[r]   = g95_add_field(type, "offset",  pchar_type_node);  
    array_rank[r]     = g95_add_field(type, "rank",    g95_default_integer);        
    array_esize[r]    = g95_add_field(type, "esize",   g95_default_integer); 
    array_base[r]     = g95_add_field(type, "base",    pvoid_type_node);   
   
    tmp1 = build_int_2(3*r-1, 0);      
    tmp1 = build_range_type(g95_default_integer, integer_zero_node, tmp1);          
    tmp1 = build_array_type(g95_default_integer, tmp1);    
    
    array_info[r] = g95_add_field(type, "info", tmp1);   
    g95_finish_type(type); 
 
    G95_DESCRIPTOR_P(type) = 1;
    array_type_node[r] = type;          
    array_ptype_node[r] = build_pointer_type(type);       
  }         
         
  /* Build the section_info[] array. */

  tmp1 = build_int_2(4*G95_MAX_DIMENSIONS, 0);        
  tmp1 = build_range_type(g95_default_integer, integer_zero_node, tmp1);    
  tmp1 = build_array_type(g95_default_integer, tmp1);          
          
  section_info =     
    build_decl(VAR_DECL, get_identifier(PREFIX "section_info"), tmp1);     
  DECL_EXTERNAL(section_info) = 1;       
}      
      
      


/* get_dimension()-- Given a descriptor variable, figure out what
 * dimension it represents.  This forms an index into the component
 * arrays to get the right component of the right type. */

static int get_dimension(tree desc) {        
tree type; 
int rank;         
         
  type = TREE_TYPE(desc);     
     
  for(rank=1; rank<=G95_MAX_DIMENSIONS; rank++)     
    if (array_type_node[rank] == type ||        
	array_ptype_node[rank] == type) return rank; 
 
  g95_internal_error("get_dimension(): Bad array type");    
} 
 
 
  
  
/* conditional_pointer()-- Given a variable which might be a
 * descriptor, return a pointer to the descriptor. */     
     
static tree conditional_pointer(tree node) {
int r;   
   
  for(r=1; r<=G95_MAX_DIMENSIONS; r++) { 
    if (TREE_TYPE(node) == array_ptype_node[r]) return node;        
        
    if (TREE_TYPE(node) == array_type_node[r])  
      return build1(ADDR_EXPR, array_ptype_node[r], node);        
  } 
 
  g95_internal_error("conditional_pointer(): Not a descriptor");      
}      
      
      
         
         
/* deferred_descriptor()-- Construct a descriptor for a deferred array. */        
        
static void deferred_descriptor(variable_info *vinfo) {         
int g, dim;   
   
  dim = vinfo->as->rank;         
        
  current_desc.offset = null_pointer_node;     
  current_desc.dimensions = build_int_2(dim, 0);   
  current_desc.esize = integer_zero_node;    
     
  for(g=0; g<dim; g++) {     
    current_desc.info[g].mult   = integer_zero_node;        
    current_desc.info[g].lbound = integer_zero_node;          
    current_desc.info[g].ubound = integer_zero_node;   
  }     
}


 
 
/* build_desc_constructor()-- Given the junk in the current
 * descriptor, build a constructor for the descriptor. */   
   
static tree build_desc_constructor(int rank) {  
tree node, info_node, tmp0;    
int g;

  tmp0 = current_desc.offset;     
  if (tmp0 == NULL_TREE) tmp0 = null_pointer_node;  
  node = tree_cons(array_offset[rank], tmp0, NULL_TREE);         
         
  tmp0 = current_desc.dimensions;         
  if (tmp0 == NULL_TREE) tmp0 = integer_zero_node; 
  node = tree_cons(array_rank[rank], tmp0, node);       
       
  tmp0 = current_desc.esize;       
  if (tmp0 == NULL_TREE) tmp0 = integer_zero_node;   
  node = tree_cons(array_esize[rank], tmp0, node);          
          
  tmp0 =  current_desc.base;         
  if (tmp0 == NULL_TREE) tmp0 = null_pointer_node;     
  node = tree_cons(array_base[rank], tmp0, node); 
 
  info_node = NULL_TREE; 
 
  for(g=0; g<rank; g++) { 
    tmp0 = current_desc.info[g].mult; 
    if (tmp0 == NULL_TREE) tmp0 = integer_zero_node;          
    info_node = tree_cons(build_int_2(3*g, 0), tmp0, info_node);          
          
    tmp0 = current_desc.info[g].lbound;    
    if (tmp0 == NULL_TREE) tmp0 = integer_zero_node;       
    info_node = tree_cons(build_int_2(3*g+1, 0), tmp0, info_node);       
       
    tmp0 = current_desc.info[g].ubound;      
    if (tmp0 == NULL_TREE) tmp0 = integer_zero_node;     
    info_node = tree_cons(build_int_2(3*g+2, 0), tmp0, info_node);         
  }    
    
  tmp0 = build_int_2(3*rank, 0);    
  tmp0 = build_range_type(g95_default_integer, integer_zero_node, tmp0);      
  tmp0 = build_array_type(g95_default_integer, tmp0);        
        
  info_node = build(CONSTRUCTOR, tmp0, NULL_TREE, nreverse(info_node));    
    
  node = tree_cons(array_info[rank], info_node, node);        
        
  node = build(CONSTRUCTOR, array_type_node[rank], NULL_TREE,    
	       nreverse(node));  
  
  /* Zero out the current constructor */        
        
  current_desc.offset = NULL_TREE;  
  current_desc.dimensions = NULL_TREE;         
  current_desc.esize = NULL_TREE;       
  current_desc.base = NULL_TREE;     
  current_desc.size = NULL_TREE;          
          
  for(g=0; g<G95_MAX_DIMENSIONS; g++) {
    current_desc.info[g].mult = NULL_TREE;       
    current_desc.info[g].lbound = NULL_TREE; 
    current_desc.info[g].ubound = NULL_TREE;     
  }

  return node;   
}      
      
      
        
        
/* conditional_indirect()-- Given a variable which might be a pointer
 * to a descriptor, insert the indirection to return a reference to
 * the descriptor. */      
      
static tree conditional_indirect(tree node) {     
int rank;         
         
  for(rank=1; rank<=G95_MAX_DIMENSIONS; rank++) {    
    if (TREE_TYPE(node) == array_type_node[rank]) return node;    
    
    if (TREE_TYPE(node) == array_ptype_node[rank])        
      return build1(INDIRECT_REF, array_type_node[rank], node);    
  }    
    
  g95_internal_error("conditional_indirect(): Not a descriptor");          
} 
 
 
 
 
/* make_pointer()-- Given an object, return a pointer to the object if
 * it isn't a pointer already. */

static tree make_pointer(tree object) {    
tree dtype;    
    
  if (!POINTER_TYPE_P(TREE_TYPE(object))) {  
    dtype = build_pointer_type(TREE_TYPE(object));      
    object = build1(ADDR_EXPR, dtype, object);    
  }         
         
  return object;  
}          
          
          
   
   
/* get_adesc_esize()-- Given a descriptor node, return a reference to
 * the element size member. */

static tree get_adesc_esize(tree desc) {      
int w;         
         
  desc = conditional_indirect(desc);      
  w = get_dimension(desc);       
       
  return build(COMPONENT_REF, TREE_TYPE(array_esize[w]), desc, array_esize[w]);    
}       
       
       
       
       
/* convert_bounds()-- Given an array spec, convert the expressions
 * forming the bounds into the descriptor.  */  
  
static void convert_bounds(g95_array_spec *as) {     
stmtblock_t post;         
g95_se s;         
int g;         
         
  current_desc.dimensions = build_int_2(as->rank, 0);         
  current_desc.base = null_pointer_node;      
      
  g95_init_block(&post);       
       
  for(g=0; g<as->rank; g++) {      
    if (as->lower[g] != NULL) { 
      g95_init_se(&s, NULL); 
      g95_conv_expr(&s, as->lower[g]);         
      current_desc.info[g].lbound = s.expr;     
     
      g95_add_block_to_block(&g95_context->pre, &s.pre);
      g95_add_block_to_block(&post, &s.post);    
    }     
     
    if (as->upper[g] != NULL) {         
      g95_init_se(&s, NULL);
      g95_conv_expr(&s, as->upper[g]);
      current_desc.info[g].ubound = s.expr;  
  
      g95_add_block_to_block(&g95_context->pre, &s.pre);  
      g95_add_block_to_block(&post, &s.post);         
    }      
  }   
   
  g95_add_block_to_block(&g95_context->pre, &post);       
}         
         
         
     
     
static tree get_adesc_rank(tree array) {    
int a;         
         
  array = conditional_indirect(array);       
  a = get_dimension(array);   
   
  return build(COMPONENT_REF, TREE_TYPE(array_rank[a]), array, array_rank[a]);   
}         
         
         
    
    
/* g95_adesc_base()-- Given a descriptor node, return a reference to
 * the base member. */      
      
tree g95_adesc_base(tree desc) {    
int z;       
       
  desc = conditional_indirect(desc);         
  z = get_dimension(desc);    
    
  return build(COMPONENT_REF, TREE_TYPE(array_base[z]), desc, array_base[z]);         
} 
 
 
 
 
/* g95_set_section_info()-- Generate an instruction that modifies the
 * section_info[] array.  The format of this array is a series of
 * records for each dimension of the original array.  Records start
 * with a word flag that is nonzero for a dimension with an element or
 * zero for a section.  For a dimension with an element, a word
 * follows that gives the element number.  For a dimension with a
 * range, three words follow giving the start, end and stride. */         
         
void g95_set_section_info(g95_se *se, int q, tree value) { 
tree t;  
 
  t = build(ARRAY_REF, g95_default_integer, section_info, build_int_2(q, 0));   
  g95_add_modify_expr(&se->pre, t, value);     
}        
        
        
          
          
/* g95_get_adesc_lbound()-- Given a descriptor and a dimension, return
 * the tree describing the multiplier. */  
  
tree g95_get_adesc_lbound(tree des, tree rank) {         
tree tmp, index;        
int a;

  des = conditional_indirect(des);          
  a = get_dimension(des);

  tmp = build(COMPONENT_REF, TREE_TYPE(array_info[a]), des, array_info[a]);   
   
  index = fold(build(MULT_EXPR, g95_default_integer, rank, build_int_2(3, 0))); 
  index = fold(build(PLUS_EXPR, g95_default_integer, index,       
		     build_int_2(-2, -1)));    
    
  return build(ARRAY_REF, g95_default_integer, tmp, index);         
}  
  
  


/* get_adesc_offset()-- Given a descriptor node, return a reference to
 * the offset member. */

static tree get_adesc_offset(tree desc) {        
int i;       
       
  desc = conditional_indirect(desc);          
  i = get_dimension(desc);  
  
  return build(COMPONENT_REF, TREE_TYPE(array_offset[i]), desc,        
	       array_offset[i]);
}       
       
       
 
 
/* g95_expand_ac_element()-- Convert a constant element of a
 * constructor, adding it to the current list. */          
          
try g95_expand_ac_element(g95_expr *h) {    
g95_se se;         
         
  g95_init_se(&se, NULL);
  g95_conv_constant(&se, h, ac_string_length);          
          
  ac_list = tree_cons(NULL_TREE, se.expr, ac_list);      
  ac_count++;      
      
  g95_free_expr(h);     
  return SUCCESS;   
}         
         
         
     
     
/* explicit_descriptor()-- Construct a descriptor for an explicit array. */    
    
static void explicit_descriptor(tree desc_var, tree storage) {         
tree t, node;      
int q, r;  
  
  r = get_dimension(desc_var);   
   
  /* Calculate the offset */          
          
  if (storage != NULL_TREE)     
    node = make_pointer(storage);   
  else {  /* Allocate the array on the heap */         
    t = g95_adesc_base(desc_var);      
    t = g95_call_procedure_alloc(t, current_desc.size);  
  
    g95_add_expr_to_block(&g95_context->pre, t);    
    node = g95_adesc_base(desc_var);        
  }  
  
  /* Initialize the offset pointer */     
     
  for(q=0; q<r; q++) {   
    t = fold(build(MULT_EXPR, g95_default_integer,    
		     current_desc.info[q].lbound,  
		     current_desc.info[q].mult));       
       
    node = fold(build(MINUS_EXPR, g95_default_integer, node, t));     
  }      
      
  if (storage != NULL_TREE) 
    current_desc.offset = node;     
  else {  
    current_desc.offset = null_pointer_node;  
  
    t = get_adesc_offset(desc_var);
    g95_add_modify_expr(&g95_context->pre, t, node);          
  }
}        
        
        
    
    
/* array_pointer_argument()-- Convert an actual argument that is an
 * array pointer. */      
      
static void array_pointer_argument(g95_se *se1, g95_actual_arglist *act) {   
symbol_attribute a;   
   
  a = g95_variable_attr(act->u.expr, NULL);     
     
  se1->reflevel = (act->u.expr->ts.type == BT_CHARACTER && a.pointer)  
    ? 1 : 2;     
     
  g95_conv_expr(se1, act->u.expr);
}      
      
      
         
         
/* get_adesc_info()-- Given a descriptor, access a constant element
 * from the info[] array. */

static tree get_adesc_info(tree desc, int e) {
tree tmp0;        
int a;          
          
  desc = conditional_indirect(desc);        
  a = get_dimension(desc);          
          
  tmp0 = build(COMPONENT_REF, TREE_TYPE(array_info[a]), desc, array_info[a]);  
  return build(ARRAY_REF, g95_default_integer, tmp0, build_int_2(e, 0));      
}       
       
       
       
       
/* get_adesc_ubound0()-- Given a descriptor and a dimension, return
 * the tree describing the multiplier. */     
     
static tree get_adesc_ubound0(tree des, int dimension) {    
    
  return get_adesc_info(des, 3*dimension+2);
}      
      
      
          
          
/* get_adesc_multiplier0()-- Given a descriptor and a dimension,
 * return the tree describing the multiplier. */   
   
static tree get_adesc_multiplier0(tree array, int dimension) {        
        
  return get_adesc_info(array, 3*dimension);   
}       
       
       
   
   
/* g95_fix_dummy_array()-- At this point, sym is a dummy array
 * argument.  The backend_decl is a pointer to the base of an array
 * that has just been passed in.  We have to create a real descriptor
 * for the array, which becomes the new value of the symbol. */   
   
void g95_fix_dummy_array(g95_symbol *s) {     
tree t, des, base, actual_arg;       
char *sub;   
   
  if (s->attr.pointer || s->as->type == AS_DEFERRED) return;        
  /* Everything is OK as is */      
      
  actual_arg = s->backend_decl; 
 
  des = g95_create_var(array_type_node[s->as->rank], NULL);     
  s->backend_decl = des;         
         
  /* Convert bounds for the descriptor */         
         
  convert_bounds(s->as);        
        
  /* Extract the array base and determine element size */         
         
  current_desc.esize =          
    (s->ts.type == BT_CHARACTER && s->ts.cl->length == NULL)         
    ? s->ts.cl->backend_decl     
    : size_in_bytes(g95_typenode_for_spec(&s->ts));   
   
  base = actual_arg;          
          
  DECL_INITIAL(des) = build_desc_constructor(s->as->rank);     
     
  t = build1(ADDR_EXPR, pvoid_type_node, des);          
          
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
        
  t = g95_call_library(void_type_node, sub, base, t, NULL_TREE);       
  g95_add_expr_to_block(&g95_context->pre, t); 
}      
      
      
 
 
/* constant_initializer()-- Generate an array constructor that comes
 * from a single constant. */        
        
static tree constant_initializer(variable_info *vinfo) {       
mpz_t size;     
g95_se se;     
     
  g95_conv_constant(&se, vinfo->value, ac_string_length);           
          
  if (g95_array_spec_size(vinfo->as, &size) == FAILURE) 
    g95_internal_error("constant_initializer(): Can't get array size");      
      
  while(mpz_sgn(size) > 0) {   
    ac_list = tree_cons(NULL_TREE, se.expr, ac_list);  
    mpz_sub_ui(size, size, 1);   
    ac_count++;      
  } 
 
  mpz_clear(size);        
  return NULL_TREE;   
}  
  
  
 
 
/* element_ref()-- Convert an array reference to an array element */     
     
static void element_ref(g95_se *s, g95_array_ref *re, g95_typespec *typesp) {        
tree dtype, d, t, pointer;  
int m, rank;         
g95_se exp;    
    
  d = s->expr; 
  pointer = get_adesc_offset(d);

  rank = re->as->rank;     
     
  for(m=0; m<rank; m++) {      
    g95_init_se(&exp, NULL); 
    g95_conv_expr(&exp, re->start[m]);  
  
    g95_add_block_to_block(&s->pre, &exp.pre);
    g95_add_block_to_block(&s->post, &exp.post);

    t = get_adesc_multiplier0(d, m);     
    t = build(MULT_EXPR, g95_default_integer, exp.expr, t);    
    pointer = fold(build(PLUS_EXPR, g95_default_integer, t, pointer));      
  }        
        
  /* Now that we have a pointer to the array element, what happens
   * next depends on the type of the element.  If the element is a
   * simple scalar or derived type, then the pointer is left as is.
   * We can't have arrays within arrays, so only the basic type is
   * important. */  
  
  dtype = g95_typenode_for_spec(typesp); 
 
  s->expr = convert(build_pointer_type(dtype), pointer);   
}          
          
          
       
       
/* g95_conv_array_initializer()-- Create an array constructor from an
 * initialization expression.  Does not handle nonconstant arrays yet. */      
      
tree g95_conv_array_initializer(variable_info *vin) {        
tree dec, tmp1, type;

  ac_list = NULL_TREE; 
  ac_string_length = (vin->ts.type == BT_CHARACTER)      
    ? vin->ts.cl->backend_decl          
    : NULL_TREE;          
          
  ac_count = 0;       
       
  switch(vin->value->type) {         
  case EXPR_CONSTANT: 
    constant_initializer(vin);      
    break;        
        
  case EXPR_ARRAY: 
    g95_expand_constructor(vin->value);         
    break;

  default:    
    g95_internal_error("g95_conv_array_initializer(): Bad expression");     
  }          
          
  tmp1 = build_int_2(ac_count, 0);         
  tmp1 = build_range_type(g95_default_integer, integer_one_node, tmp1);    
    
  type = g95_typenode_for_spec(&vin->ts);      
  type = build_array_type(type, tmp1);     
     
  dec = build(CONSTRUCTOR, type, NULL_TREE, nreverse(ac_list));    
  TREE_CONSTANT(dec) = 1;     
     
  return dec;      
}    
    
    
        
        
/* g95_get_adesc_ubound()-- Given a descriptor and a dimension, return
 * the tree describing the multiplier. */ 
 
tree g95_get_adesc_ubound(tree des, tree r) { 
tree t, i;          
int a;       
       
  des = conditional_indirect(des);   
  a = get_dimension(des); 
 
  t = build(COMPONENT_REF, TREE_TYPE(array_info[a]), des, array_info[a]);    
    
  i = fold(build(MULT_EXPR, g95_default_integer, r, build_int_2(3, 0)));      
  i = fold(build(PLUS_EXPR, g95_default_integer, i,          
		     build_int_2(-1, -1)));

  return build(ARRAY_REF, g95_default_integer, t, i);      
}   
   
   
  
  
/* g95_init_array_desc()-- Given a variable that is an array
 * descriptor, initialize it with its initial value.  For array
 * descriptors, this has already been calculated and has been waiting
 * for this moment. */

void g95_init_array_desc(variable_info *info, tree desc_var, tree storage) {   
   
  switch(info->as->type) {  
  case AS_EXPLICIT:
    explicit_descriptor(desc_var, storage);        
    break;

  case AS_DEFERRED: 
    deferred_descriptor(info);  
    break;      
      
  default:     
    abort();   
  }         
         
  DECL_INITIAL(desc_var) = build_desc_constructor(info->as->rank);     
  TREE_READONLY(desc_var) = 1;   
}     
     
     
    
    
/* array_desc_argument()-- Convert an actual argument that is a full
 * array or array element.  The value passed to the procedure is a
 * pointer to the first array element for a full array, or the
 * specified array element.  The incoming procedure expects the array
 * data to be contiguous.  If not, the data must be copied into a
 * contiguous block, and unpacked when the procedure exits.  This is
 * all handled by library subroutines.  For character arrays, we pass
 * a pointer to a descriptor of the first element. */     
     
static void array_desc_argument(g95_se *s, g95_actual_arglist *a) {    
tree tmp, var1, var2, desc;         
g95_ref *re, *last;      
g95_expr *t;   
int l;   
   
  if (a->type == FULL_ARRAY) {          
    g95_set_section_info(s, 0, integer_zero_node);          
          
    s->reflevel = 1; 
    g95_conv_expr(s, a->u.expr);   
    re = NULL;       
    desc = s->expr;     
     
  } else {         
    t = a->u.expr;        
        
    if (t->ref->next == NULL) {     
      last = t->ref;        
      re = NULL;          
      t->ref = NULL;    
    
    } else {     
      re = t->ref;     
      while(re->next->next != NULL)   
	re = re->next; 
 
      last = re->next;         
      re->next = NULL;     
    }

    g95_conv_expr(s, t);          
    desc = s->expr;      
      
    g95_set_section_info(s, 0, integer_one_node);       
       
    s->reflevel = 0;       
       
    for(l=0; l<last->u.ar.dimen; l++) { 
      g95_conv_expr(s, last->u.ar.start[l]);  
      g95_set_section_info(s, l+1, s->expr);         
    }          
          
  /* Rebuild the original expression */         
         
    if (re == NULL) 
      t->ref = last;   
    else
      re->next = last;   
  }  
  
  var1 = g95_create_var(pchar_type_node, NULL);      
  TREE_ADDRESSABLE(var1) = 1;     
  tmp = build1(ADDR_EXPR, pvoid_type_node, var1);      
      
  tmp = g95_call_library(pvoid_type_node, PREFIX "array_init",  
			 desc, tmp, NULL_TREE);      
      
  var2 = g95_create_var(pvoid_type_node, NULL);     
  g95_add_modify_expr(&s->pre, var2, tmp);

  s->expr = var2;    
    
  tmp = g95_call_library(void_type_node, PREFIX "array_done", var1, NULL_TREE);     
  g95_add_expr_to_block(&s->post, tmp);         
}      
      
      
       
       
/* get_adesc_lbound0()-- Given a descriptor and a dimension, return
 * the tree describing the multiplier. */    
    
static tree get_adesc_lbound0(tree desc, int dimension) {       
       
  return get_adesc_info(desc, 3*dimension+1);
}  
  
       
       
/* section_ref()-- Convert a section reference.  This amounts to
 * copying the reference information into the new descriptor and
 * calling the section_array() library function to calculate the new
 * descriptor.  The result of this whole thing is the new descriptor.
 * Vector subscripts are handled at a higher level and it is an error
 * to see them here. */       
       
static void section_ref(g95_se *s, g95_array_ref *ref) {  
tree tmp0, old_desc, new_desc;   
int v, q;         
         
  old_desc = s->expr;     
     
  new_desc = g95_create_var(TREE_TYPE(old_desc), NULL);         
         
  q = 0;     
  for(v=0; v<ref->dimen; v++)        
    switch(ref->dimen_type[v]) {    
    case DIMEN_ELEMENT:         
      g95_set_section_info(s, q++, integer_one_node);  
  
      g95_conv_expr0(s, ref->start[v]);      
      g95_set_section_info(s, q++, s->expr);      
      break;       
       
    case DIMEN_RANGE:          
      g95_set_section_info(s, q++, integer_zero_node);          
          
      if (ref->start[v] == NULL)   
	tmp0 = get_adesc_lbound0(old_desc, v);   
      else {         
	g95_conv_expr0(s, ref->start[v]);    
	tmp0 = s->expr;  
      }    
    
      g95_set_section_info(s, q++, tmp0);    
    
      if (ref->end[v] == NULL)         
	tmp0 = get_adesc_ubound0(old_desc, v);          
      else {       
	g95_conv_expr0(s, ref->end[v]);       
	tmp0 = s->expr;     
      }          
          
      g95_set_section_info(s, q++, tmp0);        
        
      if (ref->stride[v] == NULL)         
	tmp0 = integer_one_node;       
      else {
	g95_conv_expr0(s, ref->stride[v]);      
	tmp0 = s->expr;       
      } 
 
      g95_set_section_info(s, q++, tmp0);          
      break; 
 
    default:    
      g95_internal_error("section_ref(): Bad dimension type");
    }      
      
  s->expr = new_desc;       
       
  /* Now call the subroutine that does the work */   
   
  old_desc = build1(ADDR_EXPR, pvoid_type_node, old_desc);  
  new_desc = build1(ADDR_EXPR, pvoid_type_node, new_desc);    
    
  tmp0 = g95_call_library(void_type_node, PREFIX "section_array",        
			 old_desc, new_desc, NULL_TREE);      
      
  g95_add_expr_to_block(&s->pre, tmp0);         
}      
      
      
          
          
/* explicit_storage()-- Convert an explicit array specification to a
 * constructor that will initialize the descriptor.  With luck, this
 * constructor can be constant.  A tree representing the total size of
 * the array is returned if the expression is constant, NULL_TREE
 * otherwise. */         
         
static tree explicit_storage(g95_array_spec *a, tree element_size) {        
tree tmp, extent;          
int s;        
        
  convert_bounds(a);          
          
  current_desc.esize = save_expr(element_size);

  for(s=0; s<a->rank; s++) {    /* Calculate the multipliers */ 
    if (s == 0) {        
      current_desc.info[s].mult = current_desc.esize; 
      continue;  
    }         
         
    extent = build(PLUS_EXPR, g95_default_integer, 
		   current_desc.info[s-1].ubound, integer_one_node);         
    extent = build(MINUS_EXPR, g95_default_integer, extent,       
		   current_desc.info[s-1].lbound);       
    extent = save_expr(fold(extent));  
  
    /* Make sure the extent is nonnegative */      
      
    tmp = build(GE_EXPR, boolean_type_node, extent, integer_zero_node);     
    extent = fold(build(COND_EXPR, g95_default_integer, tmp, extent,     
			integer_zero_node)); 
 
    tmp = build(MULT_EXPR, g95_default_integer, extent,          
		current_desc.info[s-1].mult); 
 
    current_desc.info[s].mult = fold(tmp);      
  }    
    
  /* Build one more 'multiplier', which is the total size of the array */         
         
  s = a->rank - 1;    
    
  extent = build(PLUS_EXPR, g95_default_integer, current_desc.info[s].ubound,     
		 integer_one_node);   
  extent = build(MINUS_EXPR, g95_default_integer, extent,    
		 current_desc.info[s].lbound);   
  extent = save_expr(fold(extent));    
    
  tmp = build(GE_EXPR, boolean_type_node, extent, integer_zero_node);        
  extent = build(COND_EXPR, g95_default_integer, tmp, extent,        
		 integer_zero_node);  
  extent = fold(extent);     
     
  tmp = fold(build(MULT_EXPR, g95_default_integer, extent, 
		   current_desc.info[s].mult));         
         
  if (TREE_CODE(tmp) == INTEGER_CST) return tmp;        
        
  current_desc.size = tmp;  
  return NULL_TREE;  
}    
    
    
     
     
/* g95_get_array_desc()-- Get an array descriptor type of a particular
 * dimension. */     
     
tree g95_get_array_desc(int dimension) {        
        
  assert(dimension > 0 && dimension <= G95_MAX_DIMENSIONS);    
    
  return array_type_node[dimension];          
}       
       
       
       
       
/* g95_array_argument()-- Convert an actual argument that is an array
 * or array pointer. */     
     
void g95_array_argument(g95_se *se0, g95_actual_arglist *args) { 
 
  if (args->pointer)  
    array_pointer_argument(se0, args);        
  else {          
    if (args->type != ARRAY_DESC)    
      array_desc_argument(se0, args);       
    else {    
      se0->reflevel = 1;    
      g95_conv_expr(se0, args->u.expr); 
    }    
  }      
}      
         
         
/* g95_temp_array_descriptor()-- Given a temporary array descriptor
 * created on the fly, initialize its components from the
 * variable_info structure.  The array specification is explicit with
 * constant values.  The array may be zero sized, in which case we
 * don't care about the multipliers, nor the total size, so we avoid
 * the DIM(). */         
         
void g95_temp_array_descriptor(g95_se *se1, variable_info *info, tree d,   
			       tree storage) {         
tree v, t, offset, element_size, lbound, ubound, mult, last_mult;   
g95_array_spec *a;    
g95_se se2;
int k;       
       
  a = info->as;        
  last_mult = NULL_TREE;    
  g95_init_se(&se2, NULL);  
  
  v = get_adesc_rank(d);       
  g95_add_modify_expr(&se1->pre, v, build_int_2(a->rank, 0));    
    
  v = get_adesc_esize(d);  
  element_size = size_in_bytes(g95_typenode_for_spec(&info->ts)); 
  g95_add_modify_expr(&se1->pre, v, element_size);      
      
  offset = build1(ADDR_EXPR, pchar_type_node, storage); 
 
  for(k=0; k<a->rank; k++) {         
    v = get_adesc_lbound0(d, k);   

    g95_conv_constant(&se2, a->lower[k], NULL_TREE);    
    lbound = se2.expr;    
    g95_add_modify_expr(&se1->pre, v, lbound);

    v = get_adesc_ubound0(d, k);   
    g95_conv_constant(&se2, a->upper[k], NULL_TREE);     
    ubound = se2.expr;        
    g95_add_modify_expr(&se1->pre, v, ubound);     
     
    if (k == 0)       
      mult = element_size;       
    else {       
      t = fold(build(MINUS_EXPR, g95_default_integer, ubound, lbound));
      t = fold(build(PLUS_EXPR, g95_default_integer, t, integer_one_node));   
   
      mult = fold(build(MULT_EXPR, g95_default_integer, t, last_mult));
    } 
 
    last_mult = mult;  
  
    v = get_adesc_multiplier0(d, k);  
    g95_add_modify_expr(&se1->pre, v, mult);     
     
    t = fold(build(MULT_EXPR, g95_default_integer, mult, lbound));         
    offset = fold(build(MINUS_EXPR, pchar_type_node, offset, t));         
  }         
             
  v = get_adesc_offset(d);          
  g95_add_modify_expr(&se1->pre, v, offset);     
} 
 
 
   
   
/* g95_conv_array_ref()-- Given an array descriptor and an array
 * reference, calculate the array reference.  For full arrays, this is
 * just the descriptor.  For sections, we return a new descriptor that
 * describes the section.  For an element, the element is returned.
 * For character elements, a character descriptor is returned. */

void g95_conv_array_ref(g95_se *expr, g95_array_ref *re, g95_typespec *typesp) { 
 
  switch(re->type) {    
  case AR_FULL:  
    break;    /* Already done */          
          
  case AR_ELEMENT:   
    g95_reflevel(expr, 0);   
    element_ref(expr, re, typesp);     
    break;

  case AR_SECTION:        
    g95_reflevel(expr, 0);        
    section_ref(expr, re);
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
tree tmp0, etype, storage, element_size;       
       
  if (vin->as->type == AS_DEFERRED) return NULL_TREE;

  assert(vin->as->type == AS_EXPLICIT);         
  etype = void_type_node;   
   
  if (vin->ts.type != BT_CHARACTER) {
    etype = g95_typenode_for_spec(&vin->ts);      
    element_size = size_in_bytes(etype);
  } else { 
    element_size = vin->ts.cl->backend_decl;   
   
    if (INTEGER_CST_P(element_size)) {          
      tmp0 = build_range_type(g95_default_integer, integer_one_node,  
			     element_size);  
      etype = build_array_type(char_type_node, tmp0);    
    }        
  } 
 
  tmp0 = explicit_storage(vin->as, element_size); 
  if (tmp0 == NULL_TREE) return NULL_TREE;        
        
  /* An array can be an array of zero length characters, so head off
   * the nasty 0/0 when calculating the total amount of storage
   * needed. */     
     
  if (TREE_INT_CST_LOW(element_size)  == 0 &&  
      TREE_INT_CST_HIGH(element_size) == 0)         
    tmp0 = integer_zero_node;         
  else       
    tmp0 = fold(build(TRUNC_DIV_EXPR, g95_default_integer, tmp0,
		     element_size));       
       
  tmp0 = build_range_type(g95_default_integer, integer_one_node, tmp0);         
         
  storage = build_array_type(etype, tmp0);    
  return storage;         
}         
         
         
