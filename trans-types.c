/* Backend support for Fortran 95 basic types and derived types.
   Copyright (C) 2002 Free Software Foundation, Inc.
   Contributed by Paul Brook

This file is part of GNU G95.

GNU G95 is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU G95 is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU G95; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

/* trans-types.c -- g95 backend types */

#include "config.h"
#include "system.h"
#include "tree.h"
#include <stdio.h>
#include "c-common.h"
#include "ggc.h"
#include "rtl.h"
#include "toplev.h"
#include "function.h"
#include "expr.h"

#include <assert.h>
#define BACKEND_CODE
#include "g95.h"
#include "trans.h"
#include "trans-types.h"
#include "trans-const.h"

static tree g95_get_derived_type (g95_symbol * derived);

tree g95_type_nodes[NUM_F95_TYPES];

int g95_array_index_kind;
tree g95_array_index_type;


/* Create the backend type nodes. We map them to their
   equivalent C type, at least for now.  We also give
   names to the types here, and we push them in the
   global binding level context.*/
void
g95_init_types (void)
{
  /* Name the types.  */
#define PUSH_TYPE(name, node)                   \
  pushdecl (build_decl (TYPE_DECL, get_identifier (name), node))

  g95_int1_type_node = signed_char_type_node;
  PUSH_TYPE ("int1", g95_int1_type_node);
  g95_int2_type_node = short_integer_type_node;
  PUSH_TYPE ("int2", g95_int2_type_node);
  g95_int4_type_node = g95_type_for_size (32, 0 /*unsigned*/);
  PUSH_TYPE ("int4", g95_int4_type_node);
  g95_int8_type_node = g95_type_for_size (64, 0 /*unsigned*/);
  PUSH_TYPE ("int8", g95_int8_type_node);

#if (G95_USE_TYPES16 && (HOST_BITS_PER_WIDE_INT >= 64))
  /* I can't find the standard size for a 128 bit int */
  g95_int16_type_node = g95_type_for_size (128, 0 /*unsigned*/);
  PUSH_TYPE ("int16", g95_int16_type_node);
#endif

  g95_real4_type_node = float_type_node;
  PUSH_TYPE ("real4", g95_real4_type_node);
  g95_real8_type_node = double_type_node;
  PUSH_TYPE ("real8", g95_real8_type_node);
#if (G95_USE_TYPES16 && (HOST_BITS_PER_WIDE_INT >= 64))
  /* Hmm, this will not work. Ref. g77 */
  g95_real16_type_node = long_double_type_node;
  PUSH_TYPE ("real16", g95_real16_type_node);
#endif

  g95_complex4_type_node = complex_float_type_node;
  PUSH_TYPE ("complex4", g95_complex4_type_node);
  g95_complex8_type_node = complex_double_type_node;
  PUSH_TYPE ("complex8", g95_complex8_type_node);
#if (G95_USE_TYPES16 && (HOST_BITS_PER_WIDE_INT >= 64))
  /* Hmm, this will not work. Ref. g77 */
  g95_complex16_type_node = complex_long_double_type_node;
  PUSH_TYPE ("complex16", g95_complex16_type_node);
#endif

  g95_logical1_type_node = build_type_variant (g95_int1_type_node, 0, 0);
  PUSH_TYPE ("logical1", g95_logical1_type_node);
  g95_logical2_type_node = build_type_variant (g95_int2_type_node, 0, 0);
  PUSH_TYPE ("logical2", g95_logical2_type_node);
  g95_logical4_type_node = build_type_variant (g95_int4_type_node, 0, 0);
  PUSH_TYPE ("logical4", g95_logical4_type_node);
  g95_logical8_type_node = build_type_variant (g95_int8_type_node, 0, 0);
  PUSH_TYPE ("logical8", g95_logical8_type_node);
#if (G95_USE_TYPES16 && (HOST_BITS_PER_WIDE_INT >= 64))
  g95_logical16_type_node = build_type_variant (g95_int16_integer_type_node);
  PUSH_TYPE ("logical16", g95_logical16_type_node);
#endif

#undef PUSH_TYPE

  g95_array_index_kind = TYPE_PRECISION (integer_type_node) / 8;
  g95_get_int_type (g95_array_index_kind);
}

/* Get a type node for an integer kind */
tree
g95_get_int_type (int kind)
{
  switch (kind)
    {
    case 1:
      return (g95_int1_type_node);
    case 2:
      return (g95_int2_type_node);
    case 4:
      return (g95_int4_type_node);
    case 8:
      return (g95_int8_type_node);
#if (G95_USE_TYPES16 && (HOST_BITS_PER_WIDE_INT >= 64))
    case 16:
      return (95_int16_type_node);
#endif
    default:
      g95_todo_error ("integer kind=%d not available", kind);
      return error_mark_node;	/* Should never happen... */
    }
}

/* Get a type node for a real kind */
tree
g95_get_real_type (int kind)
{
  switch (kind)
    {
    case 4:
      return (g95_real4_type_node);
    case 8:
      return (g95_real8_type_node);
#if (G95_USE_TYPES16 && (HOST_BITS_PER_WIDE_INT >= 64))
    case 16:
      return (g95_real16_type_node);
#endif
    default:
      g95_todo_error ("real kind=%d not available", kind);
      return error_mark_node;
    }
}

/* Get a type node for a complex kind */
tree
g95_get_complex_type (int kind)
{
  switch (kind)
    {
    case 4:
      return (g95_complex4_type_node);
    case 8:
      return (g95_complex8_type_node);
#if (G95_USE_TYPES16 && (HOST_BITS_PER_WIDE_INT >= 64))
    case 16:
      return (g95_complex16_type_node);
#endif
    default:
      g95_todo_error ("complex kind=%d not available", kind);
      return error_mark_node;
    }
}

/* Get a type node for a logical kind */
tree
g95_get_logical_type (int kind)
{
  switch (kind)
    {
    case 4:
      return (g95_logical4_type_node);
    case 8:
      return (g95_logical8_type_node);
#if (G95_USE_TYPES16 && (HOST_BITS_PER_WIDE_INT >= 64))
    case 16:
      return (g95_logical16_type_node);
#endif
    default:
      g95_todo_error ("logical kind=%d not available", kind);
      return error_mark_node;
    }
}

/* Get a type node for a character kind */
tree
g95_get_character_type (int kind)
{
  switch (kind)
    {
    case 1:
      return (g95_character1_type_node);
    default:
      g95_todo_error ("logical kind=%d not available", kind);
      return error_mark_node;
    }
}

/* Covert a basic type */
tree
g95_typenode_for_spec (g95_typespec * spec)
{
  tree basetype;

  switch (spec->type)
    {
    case BT_INTEGER:
      basetype = g95_get_int_type (spec->kind);
      break;

    case BT_REAL:
      basetype = g95_get_real_type (spec->kind);
      break;

    case BT_COMPLEX:
      basetype = g95_get_complex_type (spec->kind);
      break;

    case BT_LOGICAL:
      basetype = g95_get_logical_type (spec->kind);
      break;

    case BT_CHARACTER:
      basetype = g95_get_character_type (spec->kind);
      break;

    case BT_DERIVED:
      basetype = g95_get_derived_type (spec->derived);
      break;

    default:
      g95_todo_error ("base type %d not implemented", spec->type);
      return error_mark_node;
    }
  return basetype;
}

/* Return the FIELD_DECL for the lbound of the n'th dimension.
   the TREE_CHAIN is the ubound.
   TODO: This should probably be in trans-decl.c.  */
static tree
g95_get_array_bound_decl (tree arraytype, int n)
{
  tree result;

  result = TYPE_FIELDS (arraytype);
  for ( ; n > 0 ; n--)
    {
      result = TREE_CHAIN (TREE_CHAIN (result));
    }
  assert (TREE_TYPE (result) == g95_array_index_type);
  return result;
}

/* Build an array. This function is called from g95_sym_type().
   Actualy returns array descriptor type.

   Format of array descriptors is as follows:

   It may turn out that the lbound is unneccassary.

   struct array_descriptor
   {
     index lbound00;
     index ubound00;
     index lbount01;
     index ubound01;
     ...
     type *data
   }

   Where index is an int.  */

static tree
g95_build_array_type (tree type, g95_array_spec  * as)
{
  tree fat_type, fat_pointer_type;
  tree fieldlist;
  tree arraytype;
  tree decl;
  int n;
  char fieldname[9];
  tree lbound;
  tree ubound;

  /* Build the type node.  */
  fat_type = make_node (RECORD_TYPE);
  TYPE_NAME (fat_type) = get_identifier ("array");
  TYPE_PACKED (fat_type) = 0;

  fat_pointer_type = build_pointer_type (fat_type);

  /* Build an array descriptor.  */
  fieldlist = NULL_TREE;

  /* this will be the type of the array data.  Start with a single element.  */
  arraytype = type;

  for (n = 0 ; n < as->rank; n++)
    {
      /* Add boundary members.  */
      sprintf (fieldname, "lbound%02d", as->rank);
      lbound = build_decl (FIELD_DECL, get_identifier (fieldname), g95_array_index_type);
      DECL_CONTEXT (lbound) = fat_type;
      fieldlist = chainon (fieldlist, lbound);
      fieldname[0] = 'u';
      ubound = build_decl (FIELD_DECL, get_identifier (fieldname), g95_array_index_type);
      DECL_CONTEXT (ubound) = fat_type;
      fieldlist = chainon (fieldlist, ubound);

      /* Create expressions for the bounds of the array.
         Note we re-use ubound and lbound.  */
      switch (as->type)
        {
        case AS_EXPLICIT:
          /* These do not haveto be constant.  */
          assert ((as->lower[n]->expr_type == EXPR_CONSTANT)
              && (as->upper[n]->expr_type == EXPR_CONSTANT));

          lbound = g95_conv_mpz_to_tree (as->lower[n]->value.integer,
                                            g95_array_index_kind);
          ubound = g95_conv_mpz_to_tree (as->upper[n]->value.integer,
                                            g95_array_index_kind);
          break;

        case AS_ASSUMED_SIZE:
          assert (as->rank == 1);

          /* Fall through...  */

        case AS_ASSUMED_SHAPE:
          lbound = g95_conv_mpz_to_tree (as->lower[n]->value.integer,
                                            g95_array_index_kind);

          /* TODO: Descriptor needs correction after psaaing when lbound!=1 */
          ubound = build (COMPONENT_REF, g95_array_index_type,
                  build (PLACEHOLDER_EXPR, fat_type), ubound);
          break;

        case AS_DEFERRED:
          lbound = build (COMPONENT_REF, g95_array_index_type,
                  build (PLACEHOLDER_EXPR, fat_type), lbound);
          ubound = build (COMPONENT_REF, g95_array_index_type,
                  build (PLACEHOLDER_EXPR, fat_type), ubound);
          break;

        default:
          g95_todo_error ("unknown array spec type");
          break;
      }

      /* Build this dimension onto the array.  */
      arraytype = build_array_type (arraytype,
            build_range_type (g95_array_index_type, lbound, ubound));

    }

  /* The pointer to the array data.  */
  decl = build_decl (FIELD_DECL,
                     get_identifier ("data"),
                     build_pointer_type (arraytype));

  DECL_CONTEXT (decl) = fat_type;
  fieldlist = chainon (fieldlist, decl);

  /* Finish off the type.  */
  TYPE_FIELDS (fat_type) = fieldlist;
  layout_decl (fat_type, 0);
  pushdecl (fat_type);

  /* Output the debuggind info for this type.  */
  rest_of_type_compilation (fat_type, 0);

  return fat_type;
}

/* Build an pointer. This function is called from g95_sym_type().  */
static tree
g95_build_pointer_type (tree type)
{
  g95_todo_error ("Pointers not implemented yet...");
}

/* Return the type for a symbol.
   For functions, returns the return type.
   For Subroutines returns void_type_node.
 */
tree
g95_sym_type (g95_symbol * sym)
{
  tree type;

  if (sym->attr.subroutine)
    return void_type_node;

  if (sym->backend_decl)
  {
    if (sym->attr.function)
      return TREE_TYPE (TREE_TYPE (sym->backend_decl));
    else
      return TREE_TYPE (sym->backend_decl);
  }

  type = g95_typenode_for_spec (&sym->ts);

  if (sym->attr.dimension)
      type = g95_build_array_type (type, sym->as);

  if (sym->attr.allocatable || sym->attr.pointer)
    type = g95_build_pointer_type (type);

  /* We currently pass all parameters by reference.
     See f95_get_function_decl.  */
  if (sym->attr.dummy)
    type = build_reference_type (type);

  return (type);
}

/* this is used by g95_get_derived_type, not sure what should go in here */
static void
g95_set_decl_attributes (tree type, symbol_attribute * attr)
{
}

/* Build a tree node for a derived type.  */
static tree
g95_get_derived_type (g95_symbol * derived)
{
  tree typenode, field, field_type, fieldlist;
  g95_component * c;

  if (derived->backend_decl)
    return derived->backend_decl;

  assert (derived && derived->ts.type == BT_DERIVED);

  /* Build the type node.  */
  typenode = make_node (RECORD_TYPE);
  TYPE_NAME (typenode) = get_identifier (derived->name);
  TYPE_PACKED (typenode) = g95_option.pack_derived;
  g95_set_decl_attributes (typenode, &derived->attr);

  /* Build the type member list. Install the newly created RECORD_TYPE
     node as DECL_CONTEXT of each FIELD_DECL.  */
  fieldlist = NULL_TREE;
  for (c = derived->components; c; c = c->next)
    {

      field_type = g95_typenode_for_spec (&c->ts);

      if (c->dimension)
	  field_type = g95_build_array_type (field_type, c->as);

      if (c->pointer)
	field_type = build_pointer_type (field_type);

      field = build_decl (FIELD_DECL,
			  get_identifier (c->name),
			  field_type);

      DECL_CONTEXT (field) = typenode;
      DECL_PACKED (field) |= TYPE_PACKED (typenode);
      DECL_INITIAL (field) = 0;

      DECL_ALIGN (field) = 0;
      DECL_USER_ALIGN (field) = 0;

      TREE_CHAIN (field) = NULL_TREE;

      fieldlist = chainon (fieldlist, field);
    }

  /* Now we have the final fieldlist.  Record it, then lay out the
     derived type, including the fields.  */
  TYPE_FIELDS (typenode) = fieldlist;
  layout_type (typenode);
  pushdecl (typenode);

  /* Finish debugging output for this type.  */
  rest_of_type_compilation (typenode, 0);

  derived->backend_decl = typenode;

  return typenode;
}

/* Routines for getting integer type nodes */


/* Return an integer type with BITS bits of precision,
   that is unsigned if UNSIGNEDP is nonzero, otherwise signed.  */

tree
g95_type_for_size (bits, unsignedp)
     unsigned bits;
     int unsignedp;
{
  if (bits == TYPE_PRECISION (integer_type_node))
    return unsignedp ? unsigned_type_node : integer_type_node;

  if (bits == TYPE_PRECISION (signed_char_type_node))
    return unsignedp ? unsigned_char_type_node : signed_char_type_node;

  if (bits == TYPE_PRECISION (short_integer_type_node))
    return unsignedp ? short_unsigned_type_node : short_integer_type_node;

  if (bits == TYPE_PRECISION (long_integer_type_node))
    return unsignedp ? long_unsigned_type_node : long_integer_type_node;

  if (bits == TYPE_PRECISION (long_long_integer_type_node))
    return (unsignedp ? long_long_unsigned_type_node
	    : long_long_integer_type_node);
/*TODO: We currently don't initialise this...
  if (bits == TYPE_PRECISION (widest_integer_literal_type_node))
    return (unsignedp ? widest_unsigned_literal_type_node
	    : widest_integer_literal_type_node);*/

  if (bits <= TYPE_PRECISION (intQI_type_node))
    return unsignedp ? unsigned_intQI_type_node : intQI_type_node;

  if (bits <= TYPE_PRECISION (intHI_type_node))
    return unsignedp ? unsigned_intHI_type_node : intHI_type_node;

  if (bits <= TYPE_PRECISION (intSI_type_node))
    return unsignedp ? unsigned_intSI_type_node : intSI_type_node;

  if (bits <= TYPE_PRECISION (intDI_type_node))
    return unsignedp ? unsigned_intDI_type_node : intDI_type_node;

  return 0;
}

/* Return a data type that has machine mode MODE.
   If the mode is an integer,
   then UNSIGNEDP selects between signed and unsigned types.  */

tree
g95_type_for_mode (mode, unsignedp)
     enum machine_mode mode;
     int unsignedp;
{
  if (mode == TYPE_MODE (integer_type_node))
    return unsignedp ? unsigned_type_node : integer_type_node;

  if (mode == TYPE_MODE (signed_char_type_node))
    return unsignedp ? unsigned_char_type_node : signed_char_type_node;

  if (mode == TYPE_MODE (short_integer_type_node))
    return unsignedp ? short_unsigned_type_node : short_integer_type_node;

  if (mode == TYPE_MODE (long_integer_type_node))
    return unsignedp ? long_unsigned_type_node : long_integer_type_node;

  if (mode == TYPE_MODE (long_long_integer_type_node))
    return unsignedp ? long_long_unsigned_type_node : long_long_integer_type_node;

/*TODO: We currently don't initialise this.
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
  if (mode == TYPE_MODE (intTI_type_node))
    return unsignedp ? unsigned_intTI_type_node : intTI_type_node;
#endif

  if (mode == TYPE_MODE (float_type_node))
    return float_type_node;

  if (mode == TYPE_MODE (double_type_node))
    return double_type_node;

  if (mode == TYPE_MODE (long_double_type_node))
    return long_double_type_node;

  if (mode == TYPE_MODE (build_pointer_type (char_type_node)))
    return build_pointer_type (char_type_node);

  if (mode == TYPE_MODE (build_pointer_type (integer_type_node)))
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

/* Return an unsigned type the same as TYPE in other respects.  */
tree
g95_unsigned_type (type)
     tree type;
{
  tree type1 = TYPE_MAIN_VARIANT (type);
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
/*TODO:see others
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

  return g95_signed_or_unsigned_type (1, type);
}

/* Return a signed type the same as TYPE in other respects.  */

tree
g95_signed_type (type)
     tree type;
{
  tree type1 = TYPE_MAIN_VARIANT (type);
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

  return g95_signed_or_unsigned_type (0, type);
}

/* Return a type the same as TYPE except unsigned or
   signed according to UNSIGNEDP.  */

tree
g95_signed_or_unsigned_type (unsignedp, type)
     int unsignedp;
     tree type;
{
  if (! INTEGRAL_TYPE_P (type)
      || TREE_UNSIGNED (type) == unsignedp)
    return type;

  if (TYPE_PRECISION (type) == TYPE_PRECISION (signed_char_type_node))
    return unsignedp ? unsigned_char_type_node : signed_char_type_node;
  if (TYPE_PRECISION (type) == TYPE_PRECISION (integer_type_node))
    return unsignedp ? unsigned_type_node : integer_type_node;
  if (TYPE_PRECISION (type) == TYPE_PRECISION (short_integer_type_node))
    return unsignedp ? short_unsigned_type_node : short_integer_type_node;
  if (TYPE_PRECISION (type) == TYPE_PRECISION (long_integer_type_node))
    return unsignedp ? long_unsigned_type_node : long_integer_type_node;
  if (TYPE_PRECISION (type) == TYPE_PRECISION (long_long_integer_type_node))
    return (unsignedp ? long_long_unsigned_type_node
	    : long_long_integer_type_node);
/*TODO: see others
  if (TYPE_PRECISION (type) == TYPE_PRECISION (widest_integer_literal_type_node))
    return (unsignedp ? widest_unsigned_literal_type_node
	    : widest_integer_literal_type_node);
*/
#if HOST_BITS_PER_WIDE_INT >= 64
  if (TYPE_PRECISION (type) == TYPE_PRECISION (intTI_type_node))
    return unsignedp ? unsigned_intTI_type_node : intTI_type_node;
#endif
  if (TYPE_PRECISION (type) == TYPE_PRECISION (intDI_type_node))
    return unsignedp ? unsigned_intDI_type_node : intDI_type_node;
  if (TYPE_PRECISION (type) == TYPE_PRECISION (intSI_type_node))
    return unsignedp ? unsigned_intSI_type_node : intSI_type_node;
  if (TYPE_PRECISION (type) == TYPE_PRECISION (intHI_type_node))
    return unsignedp ? unsigned_intHI_type_node : intHI_type_node;
  if (TYPE_PRECISION (type) == TYPE_PRECISION (intQI_type_node))
    return unsignedp ? unsigned_intQI_type_node : intQI_type_node;

  return type;
}
