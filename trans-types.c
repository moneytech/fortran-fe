/* Backend support for Fortran 95 basic types and derived types.
   Copyright (C) 2002 Free Software Foundation, Inc.
   Contributed by Paul Brook

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
      
/* trans-types.c -- g95 backend types */          
          
#include "trans.h"
      
tree g95_type_nodes[NUM_F95_TYPES];  
  
tree pvoid_type_node;
tree ppvoid_type_node;       
tree pchar_type_node;




/* get_derived_type()-- Build a tree node for a derived type.  */   
   
static tree get_derived_type(g95_symbol *derived) {     
tree dtype, field, field_type, fieldlist, d, tmp0, length;          
variable_info vinfo;   
g95_component *y;
int init_flag;  
g95_se s;        
        
  assert(derived && derived->attr.flavor == FL_DERIVED);    
    
  if (derived->backend_decl) return derived->backend_decl;      
      
  /* Build the type node. */      
      
  dtype = make_node(RECORD_TYPE);
  TYPE_NAME(dtype) = get_identifier(derived->name);         
  TYPE_PACKED(dtype) = g95_option.pack_derived;      
      
  derived->backend_decl = dtype;       
       
  /* See if this structure has any initializers.  If it does, we build
   * a constructor for the whole thing that becomes the initial value
   * if an explicit initialization is not present. */         
         
  init_flag = 0; 
  for(y=derived->components; y; y=y->next)   
    if (y->initializer) {
      init_flag = 1;      
      break;          
    }     
     
  /* Build the type member list. Install the newly created RECORD_TYPE
   * node as DECL_CONTEXT of each FIELD_DECL. */         
         
  fieldlist = NULL_TREE;     
  for(y=derived->components; y; y=y->next) {    
    g95_component_vinfo(y, &vinfo);       
    field_type = g95_get_descriptor(&vinfo);          
          
    if (G95_DESCRIPTOR_P(field_type) && !y->pointer)     
      field_type = g95_get_storage(&vinfo);  
  
    field = build_decl(FIELD_DECL, get_identifier(y->name), field_type); 
 
    DECL_CONTEXT(field) = dtype;          
    DECL_PACKED(field) |= TYPE_PACKED(dtype);      
    DECL_INITIAL(field) = 0; 
 
    DECL_ALIGN(field) = 0; 
    DECL_USER_ALIGN(field) = 0;          
          
    TREE_CHAIN(field) = NULL_TREE;        
        
    fieldlist = chainon(fieldlist, field);          
          
    assert(!y->backend_decl);
    y->backend_decl = field;         
  }   
   
  /* Now we have the final fieldlist.  Record it, then lay out the
   * derived type, including the fields.  */     
     
  TYPE_FIELDS(dtype) = fieldlist;      
  g95_finish_type(dtype);

  if (init_flag) {       
    d = NULL_TREE;       
       
    for(y=derived->components; y; y=y->next) {    
      if (y->initializer == NULL) continue; 
 
      g95_init_se(&s, NULL);        
      if (y->as == NULL) {
	g95_conv_constant(&s, y->initializer);          
	tmp0 = s.expr;       
       
	if (STRING_P(tmp0)) { 
	  length = y->ts.cl->backend_decl;       
	  tmp0 = g95_resize_string_constant(tmp0, length);       
	}
      } else {      
	g95_component_vinfo(y, &vinfo); 
	tmp0 = g95_conv_array_initializer(&vinfo);      
      }      
      
      tmp0 = tree_cons(y->backend_decl, tmp0, NULL_TREE);    
      d = chainon(d, tmp0);       
    }       
       
    (tree) TYPE_LANG_SPECIFIC(dtype) =   
      build(CONSTRUCTOR, dtype, NULL_TREE, d);       
  }   
   
  return dtype;         
}     
     
     
         
         
/* g95_init_types()-- Create the backend type nodes. We map them to
 * their equivalent C type, at least for now.  We also give names to
 * the types here, and we push them in the global binding level
 * context.*/ 
 
void g95_init_types(void) {   
unsigned HOST_WIDE_INT r;        
        
  /* Name the types.  */       
#define PUSH_TYPE(name, node) \
  pushdecl(build_decl(TYPE_DECL, get_identifier(name), node))
    
  g95_int1_type_node = signed_char_type_node;      
  PUSH_TYPE("int1", g95_int1_type_node);         
         
  g95_int2_type_node = short_integer_type_node;      
  PUSH_TYPE("int2", g95_int2_type_node);   
   
  g95_int4_type_node = g95_type_for_size(32, 0 /*unsigned*/);     
  PUSH_TYPE("int4", g95_int4_type_node);   
   
  g95_int8_type_node = g95_type_for_size(64, 0 /*unsigned*/);     
  PUSH_TYPE ("int8", g95_int8_type_node);      
      
#if (G95_USE_TYPES16 && (HOST_BITS_PER_WIDE_INT >= 64))
  g95_int16_type_node = g95_type_for_size (128, 0 /*unsigned*/);          
  PUSH_TYPE ("int16", g95_int16_type_node);          
#endif
      
  g95_real4_type_node = float_type_node;        
  PUSH_TYPE("real4", g95_real4_type_node);         
         
  g95_real8_type_node = double_type_node;    
  PUSH_TYPE("real8", g95_real8_type_node);   
   
#if (G95_USE_TYPES16 && (HOST_BITS_PER_WIDE_INT >= 64))
  /* Hmm, this will not work. Ref. g77 */        
  g95_real16_type_node = long_double_type_node;    
  PUSH_TYPE("real16", g95_real16_type_node);       
#endif
   
  g95_complex4_type_node = complex_float_type_node;     
  PUSH_TYPE("complex4", g95_complex4_type_node);       
       
  g95_complex8_type_node = complex_double_type_node;
  PUSH_TYPE("complex8", g95_complex8_type_node);        
        
#if (G95_USE_TYPES16 && (HOST_BITS_PER_WIDE_INT >= 64))
  /* Hmm, this will not work. Ref. g77 */  
  g95_complex16_type_node = complex_long_double_type_node;      
  PUSH_TYPE ("complex16", g95_complex16_type_node); 
#endif
       
  g95_logical1_type_node = build_qualified_type(g95_int1_type_node, 0);
  PUSH_TYPE("logical1", g95_logical1_type_node);        
        
  g95_logical2_type_node = build_qualified_type(g95_int2_type_node, 0);  
  PUSH_TYPE("logical2", g95_logical2_type_node);       
       
  g95_logical4_type_node = build_qualified_type(g95_int4_type_node, 0);     
  PUSH_TYPE("logical4", g95_logical4_type_node);  
  
  g95_logical8_type_node = build_qualified_type(g95_int8_type_node, 0);   
  PUSH_TYPE("logical8", g95_logical8_type_node);          
          
#if (G95_USE_TYPES16 && (HOST_BITS_PER_WIDE_INT >= 64))
  g95_logical16_type_node =        
    build_qualified_type(g95_int16_integer_type_node, 0);          
  PUSH_TYPE("logical16", g95_logical16_type_node);       
#endif
         
  g95_character1_type_node = build_qualified_type(signed_char_type_node, 0);         
  PUSH_TYPE("char", g95_character1_type_node);

  PUSH_TYPE("byte", unsigned_char_type_node);       
       
  PUSH_TYPE("void", void_type_node);          
          
  /* DBX debugging output gets upset if these aren't set.  */ 
  if (!TYPE_NAME(integer_type_node)) PUSH_TYPE("c_integer", integer_type_node);   
   
  if (!TYPE_NAME(char_type_node)) PUSH_TYPE("c_char", char_type_node);      
#undef PUSH_TYPE
      
  pvoid_type_node = build_pointer_type(void_type_node);   
  ppvoid_type_node = build_pointer_type(pvoid_type_node);     
  pchar_type_node = build_pointer_type(g95_character1_type_node);     
     
  r = TREE_INT_CST_LOW(TYPE_SIZE(g95_default_integer));          
  if (r > sizeof(HOST_WIDE_INT)*8) r = sizeof(HOST_WIDE_INT)*8;      
      
  r += G95_DTYPE_SIZE_SHIFT; 
  size_type_node = g95_default_integer; 
}     
     
     
        
        
/* g95_finish_type()-- Layout and output debug info for a record type.  */ 
void g95_finish_type(tree typ) {  
tree declr;       
       
  declr = build_decl(TYPE_DECL, NULL_TREE, typ); 
  TYPE_STUB_DECL(typ) = declr;   
  layout_type(typ);      
  rest_of_type_compilation(typ, 1); 
  rest_of_decl_compilation(declr, NULL, 1, 0);        
}         
         
         
    
    
/* g95_procedure_type()-- Get the type of a procedure, which
 * includes the types in its argument list. */

tree g95_procedure_type(g95_symbol *symb) {        
tree t, typelist, typelist_tail;      
g95_formal_arglist *s;   
g95_symbol *ap;       
       
  if (symb->backend_decl) return TREE_TYPE(symb->backend_decl);    
    
  typelist = NULL_TREE;     
  typelist_tail = NULL_TREE;          
          
  /* Build the argument types for the function */    
    
  if (symb->attr.function) {    
    if (symb->result->as != NULL && !symb->result->attr.pointer) {        
      t = g95_get_array_desc(symb->result->as->rank);          
      t = build_pointer_type(t);     
      typelist = g95_chainon_list(typelist, t);  
    } else   
      switch(symb->result->ts.type) {     
      case BT_COMPLEX:    
      case BT_DERIVED:   
	t = g95_typenode_for_spec(&symb->result->ts); 
	t = build_pointer_type(t);          
	typelist = g95_chainon_list(typelist, t);  
	break;    
    
      case BT_CHARACTER:   
	t = build_pointer_type(g95_default_integer);          
	typelist = g95_chainon_list(typelist, t);         
	break;       
       
      default:   
	break;    
      }      
  }       
       
  /* User-specified parameters */         
         
  for(s=symb->formal; s; s=s->next) {       
    ap = s->sym;          
    if (ap == NULL) continue;     
     
    t = (ap->attr.flavor == FL_PROCEDURE) ?       
      g95_procedure_type(ap) : g95_dummy_arg_type(ap);

    /* Everything is passed by reference */

    if (G95_DESCRIPTOR_P(t) || TREE_CODE(t) == FUNCTION_TYPE)   
      t = build_pointer_type(t);

    typelist = g95_chainon_list(typelist, t);         
         
    if (ap->ts.type == BT_CHARACTER)
      typelist_tail = g95_chainon_list(typelist_tail, g95_default_integer);  
  }    
    
  if (typelist_tail != NULL_TREE) typelist = chainon(typelist, typelist_tail);

  typelist = g95_chainon_list(typelist, void_type_node);         
  t = g95_result_type(symb);       
       
  return build_function_type(t, typelist);        
}


       
       
/* g95_get_int_type()-- Get a type node for an integer kind */  
  
tree g95_get_int_type(int k) {     
     
  switch(k) {   
  case 1:    return g95_int1_type_node;  
  case 2:    return g95_int2_type_node;        
  case 4:    return g95_int4_type_node;          
  case 8:    return g95_int8_type_node;     
#if (G95_USE_TYPES16 && (HOST_BITS_PER_WIDE_INT >= 64))
  case 16:   return g95_int16_type_node;     
#endif
  default:          
    g95_internal_error("Integer kind=%d not available", k); 
  } 
}


     
     
/* Return a data type that has machine mode MODE.
   If the mode is an integer,
   then UNSIGNEDP selects between signed and unsigned types.  */    
    
tree g95_type_for_mode(enum machine_mode mode, int unsignedp) {         
         
  if (mode == TYPE_MODE (integer_type_node))       
    return unsignedp ? unsigned_type_node : integer_type_node;       
       
  if (mode == TYPE_MODE (signed_char_type_node))         
    return unsignedp ? unsigned_char_type_node : signed_char_type_node;       
       
  if (mode == TYPE_MODE (short_integer_type_node))
    return unsignedp ? short_unsigned_type_node : short_integer_type_node; 
 
  if (mode == TYPE_MODE (long_integer_type_node))      
    return unsignedp ? long_unsigned_type_node : long_integer_type_node;         
         
  if (mode == TYPE_MODE (long_long_integer_type_node))        
    return unsignedp ? long_long_unsigned_type_node     
                     : long_long_integer_type_node;

/*TODO: see above
  if (mode == TYPE_MODE (widest_integer_literal_type_node))
    return unsignedp ? widest_unsigned_literal_type_node
                     : widest_integer_literal_type_node;
*/    
    
  if (mode == QImode)
    return unsignedp ? unsigned_intQI_type_node : intQI_type_node;

  if (mode == HImode) 
    return unsignedp ? unsigned_intHI_type_node : intHI_type_node;   
   
  if (mode == SImode)       
    return unsignedp ? unsigned_intSI_type_node : intSI_type_node;       
       
  if (mode == DImode)       
    return unsignedp ? unsigned_intDI_type_node : intDI_type_node;   
   
#if HOST_BITS_PER_WIDE_INT >= 64
  if (mode == TYPE_MODE(intTI_type_node))   
    return unsignedp ? unsigned_intTI_type_node : intTI_type_node;   
#endif
     
  if (mode == TYPE_MODE(float_type_node))        
    return float_type_node;  
  
  if (mode == TYPE_MODE(double_type_node))        
    return double_type_node;          
          
  if (mode == TYPE_MODE(long_double_type_node))
    return long_double_type_node;      
      
  if (mode == TYPE_MODE(build_pointer_type(char_type_node)))     
    return build_pointer_type (char_type_node);     
     
  if (mode == TYPE_MODE(build_pointer_type(integer_type_node)))      
    return build_pointer_type (integer_type_node);     
     
#ifdef VECTOR_MODE_SUPPORTED_P
  if (VECTOR_MODE_SUPPORTED_P (mode))       
    { 
      switch (mode)          
        {        
        case V16QImode:    
          return unsignedp ? unsigned_V16QI_type_node : V16QI_type_node;        
        case V8HImode:     
          return unsignedp ? unsigned_V8HI_type_node : V8HI_type_node; 
        case V4SImode:
          return unsignedp ? unsigned_V4SI_type_node : V4SI_type_node;          
        case V2DImode:
          return unsignedp ? unsigned_V2DI_type_node : V2DI_type_node;
        case V2SImode:       
          return unsignedp ? unsigned_V2SI_type_node : V2SI_type_node;      
        case V4HImode: 
          return unsignedp ? unsigned_V4HI_type_node : V4HI_type_node; 
        case V8QImode:   
          return unsignedp ? unsigned_V8QI_type_node : V8QI_type_node;
        case V16SFmode:  
          return V16SF_type_node;       
        case V4SFmode:
          return V4SF_type_node;         
        case V2SFmode:      
          return V2SF_type_node;      
        case V2DFmode:    
          return V2DF_type_node;         
        default:         
          break;   
        }         
    }
#endif
       
  return 0;         
}        
        
        
         
         
/* g95_get_descriptor()-- Given a pointer to a variable_info
 * structure, return the type node that corresponds to that symbol.
 * For array variables, a descriptor type is returned. */

tree g95_get_descriptor(variable_info *info) { 
tree typ;          
          
  if (info->as != NULL) {         
    g95_typenode_for_spec(&info->ts); /* init length spec, other types. */      
      
    typ = g95_get_array_desc(info->as->rank);     
  } else {        
    typ = g95_typenode_for_spec(&info->ts);        
    if (info->pointer) typ = build_pointer_type(typ);       
  }     
     
  if (info->dummy) typ = build_pointer_type(typ);        
        
  return typ;
}


 
 
/* g95_dummy_arg_type()-- Return a type node for a dummy argument.
 * These are slightly different than regular variables. */       
       
tree g95_dummy_arg_type(g95_symbol *symb) {      
tree type;     
     
  if (symb->attr.flavor == FL_PROCEDURE) {          
    type = g95_procedure_type(symb);     
    return build_pointer_type(type);        
  }        
        
  type = g95_typenode_for_spec(&symb->ts);       
       
  if (symb->as == NULL) {  /* Scalar dummy argument */          
    if (symb->attr.pointer) type = build_pointer_type(type);
  } else {   /* Array arguments */      
      
    /* Assumed shape arrays and pointer arrays pass a pointer to the
     * descriptor. */ 
 
    if (symb->as->type == AS_ASSUMED_SHAPE ||  
	symb->as->type == AS_DEFERRED)    
      type = g95_get_array_desc(symb->as->rank);          
  }       
       
  return build_pointer_type(type);        
}   
   
   
        
        
/* g95_get_logical_type()-- Get a type node for a logical kind */    
tree g95_get_logical_type(int knd) {   
   
  switch (knd) {  
  case 1:    return g95_logical1_type_node;     
  case 4:    return g95_logical4_type_node;          
#if (G95_USE_TYPES16 && (HOST_BITS_PER_WIDE_INT >= 64))
  case 16:   return g95_logical16_type_node;      
#endif
  default:          
    fatal_error ("Logical kind=%d not available", knd);
  }   
}        
        
        
   
   
/* g95_get_complex_type()-- Get a type node for a complex kind */       
       
tree g95_get_complex_type(int kind) {

  switch(kind) {    
  case 4:   return g95_complex4_type_node;       
  case 8:   return g95_complex8_type_node;         
#if (G95_USE_TYPES16 && (HOST_BITS_PER_WIDE_INT >= 64))
  case 16:  return g95_complex16_type_node;   
#endif
  default:
    g95_internal_error("Complex kind=%d not available", kind);        
  }    
}        
        
        
    
    
/* g95_typenode_for_spec()-- Convert a basic type. */    
    
tree g95_typenode_for_spec(g95_typespec *spec) {       
tree basetype;     
     
  switch(spec->type) {     
  case BT_INTEGER:         
    basetype = g95_get_int_type(spec->kind);   
    break;      
      
  case BT_REAL:
    basetype = g95_get_real_type(spec->kind);        
    break;  
  
  case BT_COMPLEX:         
    basetype = g95_get_complex_type(spec->kind); 
    break;         
         
  case BT_LOGICAL:          
    basetype = g95_get_logical_type(spec->kind);          
    break;       
       
  case BT_CHARACTER:          
    basetype = g95_get_character_type(spec->kind, spec->cl);          
    break;          
          
  case BT_DERIVED:          
    basetype = get_derived_type(spec->derived);    
    break; 
 
  default:       
    g95_internal_error("g95_typenode_for_spec(): Bad typespec"); 
    break;     
  }     
     
  return basetype;          
}        
        
        
     
     
/* g95_result_type()-- Return the return value for the procedure. */   
   
tree g95_result_type(g95_symbol *symb) {       
tree dtype;     
     
  if (!symb->attr.function) return g95_default_integer;    
    
  symb = symb->result;
  if (symb->as) {  /* Arrays and array pointers are the same */     
    dtype = g95_get_array_desc(symb->as->rank);     
    return build_pointer_type(dtype);     
  }      
      
  if (symb->attr.pointer) {    
    dtype = g95_typenode_for_spec(&symb->ts);    
    return build_pointer_type(dtype);      
  }  
  
  switch(symb->ts.type) {       
  case BT_CHARACTER:       
    dtype = pchar_type_node;       
    break;   
   
  case BT_DERIVED:      
  case BT_COMPLEX:    
    dtype = void_type_node;   
    break;       
       
  default:
    dtype = g95_typenode_for_spec(&symb->ts);        
    break;     
  }     
     
  return dtype; 
}          
          
          
         
         
/* g95_symbol_vinfo()-- Given a symbol pointer, initialize a
 * variable_info structure. */        
        
void g95_symbol_vinfo(g95_symbol *symb, variable_info *v) {        
        
  v->ts = symb->ts;          
  v->as = symb->as;    
  v->pointer = symb->attr.pointer;   
  v->dummy = symb->attr.dummy;     
     
  v->static_storage = symb->value != NULL || symb->attr.save ||        
                          symb->attr.data;        
        
  v->value = symb->attr.use_assoc ? NULL : symb->value;
  v->desc = symb->backend_decl;         
}      
      
      
          
          
/* init_varlen_character()-- Given a vinfo node for a character
 * variable, see if the length is a constant or not.  For a constant
 * length, we return a typenode for a block of memory for the string.
 * Otherwise we return a character pointer type and generate code to
 * initialize the pointer to heap memory. */    
    
static void init_varlen_character(variable_info *vin, tree desc_var) {          
tree tmp0, l;   
g95_se se1; 
 
  if (TREE_TYPE(desc_var) != pchar_type_node) return;         
         
  g95_init_se(&se1, NULL);         
  l = g95_conv_char_length(&se1, &vin->ts);         
         
  l = build(MAX_EXPR, g95_default_integer, l, integer_zero_node);    
  l = fold(l);   
   
  tmp0 = build1(ADDR_EXPR, pvoid_type_node, desc_var);

  /* Get some memory from the heap */         
         
  tmp0 = g95_call_procedure_alloc(tmp0, l);  
  
  g95_add_expr_to_block(&g95_context->pre, tmp0);  
  g95_add_block_to_block(&g95_context->pre, &se1.post);       
}          
          
          
       
       
/* g95_component_vinfo()-- Initialize a variable_info structure from a
 * component structure. */

void g95_component_vinfo(g95_component *o, variable_info *vin) {          
          
  vin->ts = o->ts;         
  vin->as = o->as;      
  vin->pointer = o->pointer;        
  vin->dummy = 0;       
  vin->static_storage = 0;      
  vin->value = o->initializer;    
  vin->desc = NULL_TREE;      
}  
  
  
   
   
/* g95_type_for_size()-- Return an integer type with BITS bits of
 * precision, that is unsigned if UNSIGNEDP is nonzero, otherwise
 * signed.  */  
  
tree g95_type_for_size(unsigned bits, int unsignedp) {       
       
  if (bits == TYPE_PRECISION(integer_type_node))   
    return unsignedp ? unsigned_type_node : integer_type_node;          
          
  if (bits == TYPE_PRECISION(signed_char_type_node))          
    return unsignedp ? unsigned_char_type_node : signed_char_type_node;      
      
  if (bits == TYPE_PRECISION(short_integer_type_node)) 
    return unsignedp ? short_unsigned_type_node : short_integer_type_node;     
     
  if (bits == TYPE_PRECISION(long_integer_type_node))   
    return unsignedp ? long_unsigned_type_node : long_integer_type_node;          
          
  if (bits == TYPE_PRECISION(long_long_integer_type_node))         
    return (unsignedp ? long_long_unsigned_type_node        
            : long_long_integer_type_node);     
     
/*TODO: We currently don't initialise this...
  if (bits == TYPE_PRECISION (widest_integer_literal_type_node))
    return (unsignedp ? widest_unsigned_literal_type_node
            : widest_integer_literal_type_node);*/  
  
  if (bits <= TYPE_PRECISION(intQI_type_node))      
    return unsignedp ? unsigned_intQI_type_node : intQI_type_node;          
          
  if (bits <= TYPE_PRECISION(intHI_type_node))      
    return unsignedp ? unsigned_intHI_type_node : intHI_type_node;        
        
  if (bits <= TYPE_PRECISION(intSI_type_node))    
    return unsignedp ? unsigned_intSI_type_node : intSI_type_node;  
  
  if (bits <= TYPE_PRECISION(intDI_type_node))  
    return unsignedp ? unsigned_intDI_type_node : intDI_type_node;          
          
  return 0;    
}        
        
        
    
    
/* g95_get_real_type()-- Get a type node for a real kind */  
  
tree g95_get_real_type(int k) {

  switch (k) {     
  case 4:      return g95_real4_type_node;   
  case 8:      return g95_real8_type_node;      
#if (G95_USE_TYPES16 && (HOST_BITS_PER_WIDE_INT >= 64))
  case 16:     return g95_real16_type_node;         
#endif
  default:        
    g95_internal_error("Real kind=%d not available", k);      
  }   
}      
      
      
         
         
/* g95_init_descriptor()-- Given a descriptor variable and a storage
 * variable, finish initializing the descriptor.  For variables that
 * do not require a descriptor, nothing happens. */ 
 
void g95_init_descriptor(variable_info *v, tree desc_var,        
			 tree storage_var) {  
  
  if (v->as != NULL) {     
    g95_init_array_desc(v, desc_var, storage_var);  
    return;       
  }      
      
  if (v->ts.type == BT_CHARACTER && !v->pointer && !v->dummy) {          
    init_varlen_character(v, desc_var);          
    return;
  }     
     
  /* Type is a scalar that doesn't need a descriptor */       
}        
        
        
          
          
/* g95_get_character_type()-- Get a type node for a character kind.  */     
     
tree g95_get_character_type(int k0, g95_charlen *charlen) {     
tree base, typ, len, bounds;  
g95_se s;

  switch(k0) {      
  case 1:          
    base = g95_character1_type_node;     
    break;

  default:  
    g95_internal_error("Character kind=%d not available", k0); 
  }     
     
  if (charlen->backend_decl != NULL)          
    len = charlen->backend_decl; 
  else { 
    if (charlen->length == NULL)  
      len = NULL_TREE;
    else {    
      g95_init_se(&s, NULL);         
      g95_conv_expr(&s, charlen->length);          
          
      g95_add_block_to_block(&g95_context->pre, &s.pre);      
      g95_add_block_to_block(&g95_context->post, &s.post);          
          
      len = save_expr(fold(build(MAX_EXPR, g95_default_integer,      
				 s.expr, integer_zero_node)));        
    }      
      
    if (charlen->backend_decl == NULL_TREE) charlen->backend_decl = len;        
  }          
          
  bounds = build_range_type(g95_default_integer, integer_one_node, len);       
  typ = build_array_type(base, bounds);

  TYPE_STRING_FLAG(typ) = 1;         
  return typ;        
} 
 
 
      
      
/* g95_unsigned_type()-- Return an unsigned type the same as TYPE in
 * other respects. */          
          
tree g95_unsigned_type(tree t) {       
       
  tree type1 = TYPE_MAIN_VARIANT(t);         
         
  if (type1 == signed_char_type_node || type1 == char_type_node)    
    return unsigned_char_type_node;     
     
  if (type1 == integer_type_node)     
    return unsigned_type_node;         
         
  if (type1 == short_integer_type_node)      
    return short_unsigned_type_node; 
 
  if (type1 == long_integer_type_node)   
    return long_unsigned_type_node;     
     
  if (type1 == long_long_integer_type_node)    
    return long_long_unsigned_type_node;      
      
/*TODO :see others
  if (type1 == widest_integer_literal_type_node)
    return widest_unsigned_literal_type_node;
*/      
#if HOST_BITS_PER_WIDE_INT >= 64
  if (type1 == intTI_type_node)      
    return unsigned_intTI_type_node;         
#endif
 
  if (type1 == intDI_type_node)      
    return unsigned_intDI_type_node;     
     
  if (type1 == intSI_type_node)          
    return unsigned_intSI_type_node;    
    
  if (type1 == intHI_type_node)   
    return unsigned_intHI_type_node;      
      
  if (type1 == intQI_type_node)        
    return unsigned_intQI_type_node;

  return g95_signed_or_unsigned_type(1, t);    
}      
      
      
/* g95_signed_type()-- Return a signed type the same as TYPE in other
 * respects. */        
        
tree g95_signed_type(tree t) {   
tree type1;         
         
  type1 = TYPE_MAIN_VARIANT(t);       
       
  if (type1 == unsigned_char_type_node || type1 == char_type_node)      
    return signed_char_type_node; 
 
  if (type1 == unsigned_type_node)        
    return integer_type_node;    
    
  if (type1 == short_unsigned_type_node)          
    return short_integer_type_node;     
     
  if (type1 == long_unsigned_type_node)   
    return long_integer_type_node;   
   
  if (type1 == long_long_unsigned_type_node)     
    return long_long_integer_type_node;      
      
/*TODO: see others
  if (type1 == widest_unsigned_literal_type_node)
    return widest_integer_literal_type_node;
*/ 
#if HOST_BITS_PER_WIDE_INT >= 64
  if (type1 == unsigned_intTI_type_node)  
    return intTI_type_node;  
#endif
  
  if (type1 == unsigned_intDI_type_node)     
    return intDI_type_node; 
 
  if (type1 == unsigned_intSI_type_node)  
    return intSI_type_node;   
   
  if (type1 == unsigned_intHI_type_node)    
    return intHI_type_node;    
    
  if (type1 == unsigned_intQI_type_node)     
    return intQI_type_node;         
         
  return g95_signed_or_unsigned_type(0, t);
}   
   
   
/* g95_signed_or_unsigned_type()-- Return a type the same as TYPE
 * except unsigned or signed according to UNSIGNEDP. */ 
 
tree g95_signed_or_unsigned_type (int unsignedp, tree t) {       
       
  if (!INTEGRAL_TYPE_P(t) || TREE_UNSIGNED (t) == unsignedp)        
    return t;   
   
  if (TYPE_PRECISION (t) == TYPE_PRECISION (signed_char_type_node))
    return unsignedp ? unsigned_char_type_node : signed_char_type_node;   
   
  if (TYPE_PRECISION (t) == TYPE_PRECISION (integer_type_node))      
    return unsignedp ? unsigned_type_node : integer_type_node;       
       
  if (TYPE_PRECISION (t) == TYPE_PRECISION (short_integer_type_node))      
    return unsignedp ? short_unsigned_type_node : short_integer_type_node;  
  
  if (TYPE_PRECISION (t) == TYPE_PRECISION (long_integer_type_node))  
    return unsignedp ? long_unsigned_type_node : long_integer_type_node;         
         
  if (TYPE_PRECISION (t) == TYPE_PRECISION (long_long_integer_type_node))      
    return (unsignedp ? long_long_unsigned_type_node
            : long_long_integer_type_node);         
         
/*TODO: see others
  if (TYPE_PRECISION (type) == TYPE_PRECISION (widest_integer_literal_type_node))
    return (unsignedp ? widest_unsigned_literal_type_node
            : widest_integer_literal_type_node);
*/   
   
#if HOST_BITS_PER_WIDE_INT >= 64
  if (TYPE_PRECISION (t) == TYPE_PRECISION (intTI_type_node))   
    return unsignedp ? unsigned_intTI_type_node : intTI_type_node;  
#endif
  
  if (TYPE_PRECISION (t) == TYPE_PRECISION (intDI_type_node))   
    return unsignedp ? unsigned_intDI_type_node : intDI_type_node;        
        
  if (TYPE_PRECISION (t) == TYPE_PRECISION (intSI_type_node))    
    return unsignedp ? unsigned_intSI_type_node : intSI_type_node; 
 
  if (TYPE_PRECISION (t) == TYPE_PRECISION (intHI_type_node))        
    return unsignedp ? unsigned_intHI_type_node : intHI_type_node;          
          
  if (TYPE_PRECISION (t) == TYPE_PRECISION (intQI_type_node))      
    return unsignedp ? unsigned_intQI_type_node : intQI_type_node;       
       
  return t;     
}          
        
        
/* g95_get_storage()-- Return a type node suitable for building the
 * data part of a variable or field declaration.  This returns the
 * type for storing the data, not a descriptor.  Returns NULL_TREE if
 * the underlying variable does not require a descriptor. */    
    
tree g95_get_storage(variable_info *vinfo) {   
   
  if (vinfo->as != NULL) return g95_get_array_storage(vinfo);       
       
  return NULL_TREE;       
}        
        
        
