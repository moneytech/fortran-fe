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


#if (G95_MAX_DIMENSIONS < 10)
#define G95_RANK_DIGITS 1
#define G95_RANK_PRINTF_FORMAT "%01d"
#elif (G95_MAX_DIMENSIONS < 100)
#define G95_RANK_DIGITS 2
#define G95_RANK_PRINTF_FORMAT "%02d"
#else
#error If you really need >99 dimensions, continue the sequence above...
#endif

static tree g95_get_derived_type (g95_symbol * derived);

tree g95_type_nodes[NUM_F95_TYPES];

int g95_array_index_kind;
tree g95_array_index_type;
tree ppvoid_type_node;
tree pchar_type_node;
static GTY(()) tree g95_desc_dim_type = NULL;

static unsigned HOST_WIDE_INT g95_max_array_element_size;

/* Create the backend type nodes. We map them to their
   equivalent C type, at least for now.  We also give
   names to the types here, and we push them in the
   global binding level context.*/
void
g95_init_types (void)
{
  unsigned HOST_WIDE_INT n;

  /* Name the types.  */
#define PUSH_TYPE(name, node) \
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

  g95_character1_type_node = build_type_variant (signed_char_type_node, 0, 0);
  PUSH_TYPE ("char", g95_character1_type_node);

  PUSH_TYPE ("byte", unsigned_char_type_node);
  PUSH_TYPE ("void", void_type_node);

  /* DBX debugging output gets upset if these aren't set.  */
  if (! TYPE_NAME (integer_type_node))
    PUSH_TYPE ("c_integer", integer_type_node);
  if (! TYPE_NAME (char_type_node))
    PUSH_TYPE ("c_char", char_type_node);
#undef PUSH_TYPE

  ppvoid_type_node = build_pointer_type (build_pointer_type (void_type_node));
  pchar_type_node = build_pointer_type (g95_character1_type_node);

  g95_array_index_kind = TYPE_PRECISION (integer_type_node) / 8;
  g95_array_index_type = g95_get_int_type (g95_array_index_kind);


  n = TREE_INT_CST_LOW (TYPE_SIZE (g95_array_index_type));
  if (n > sizeof(HOST_WIDE_INT) * 8)
    n = sizeof(HOST_WIDE_INT) * 8;
  n += G95_DTYPE_SIZE_SHIFT;
  g95_max_array_element_size = (~(unsigned HOST_WIDE_INT)0) >> n;
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
      fatal_error ("integer kind=%d not available", kind);
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
      fatal_error ("real kind=%d not available", kind);
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
      fatal_error ("complex kind=%d not available", kind);
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
      fatal_error ("logical kind=%d not available", kind);
    }
}

/* Get a type node for a character kind.  */
tree
g95_get_character_type (int kind, g95_charlen * cl)
{
  tree base;
  tree type;
  tree len;
  tree bounds;

  switch (kind)
    {
    case 1:
      base = g95_character1_type_node;
      break;

    default:
      fatal_error ("character kind=%d not available", kind);
    }

  if (cl && cl->length && cl->length->expr_type == EXPR_CONSTANT)
    {
      len = g95_conv_mpz_to_tree (cl->length->value.integer,
                                 cl->length->ts.kind);
    }
  else
    len = NULL_TREE;

  bounds = build_range_type (g95_array_index_type, integer_one_node, len);
  type = build_array_type (base, bounds);
  TYPE_STRING_FLAG (type) = 1;
  if (len != NULL_TREE)
    G95_KNOWN_SIZE_STRING_TYPE (type) = 1;

  return type;
}

/* Covert a basic type.  This will be an array for character types.  */
tree
g95_typenode_for_spec (g95_typespec * spec)
{
  tree basetype;

  switch (spec->type)
    {
    case BT_UNKNOWN:
      abort ();
      break;

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
      basetype = g95_get_character_type (spec->kind, spec->cl);
      break;

    case BT_DERIVED:
      basetype = g95_get_derived_type (spec->derived);
      break;

    default:
      abort ();
      break;
    }
  return basetype;
}

/* Build an INT_CST for constant expressions, otherwise return NULL_TREE.  */
static tree
g95_conv_array_bound (g95_expr * expr)
{
  /* If expr is an integer constant, return that.  */
  if (expr != NULL && expr->expr_type == EXPR_CONSTANT)
    return g95_conv_mpz_to_tree (expr->value.integer, g95_array_index_kind);

  /* Otherwise return NULL.  */
  return NULL_TREE;
}

tree
g95_get_element_type (tree type)
{
  tree element;

  element = TREE_TYPE (TYPE_FIELDS (type));

  assert (TREE_CODE (element) == POINTER_TYPE);
  element = TREE_TYPE (element);

  assert (TREE_CODE (element) == ARRAY_TYPE);
  element = TREE_TYPE (element);

  return element;
}

/* Creates a type with the given size.  Used for holding array data.  */
tree
g95_get_stack_array_type (tree size)
{
  tree type;
  tree bounds;

  bounds = build_range_type (g95_array_index_type, integer_one_node, size);
  type = build_array_type (unsigned_char_type_node, bounds);

  return type;
}

/* Build an array. This function is called from g95_sym_type().
   Actualy returns array descriptor type.

   Format of array descriptors is as follows:

    struct g95_array_descriptor
    {
      array *data
      array *base;
      index dtype;
      struct descriptor_dimension dimension[N_DIM];
    }

    struct descriptor_dimension
    {
      index stride;
      index lbound;
      index ubound;
    }

   Translation code should use g95_conv_descriptor_* rather than accessing
   the descriptor directly.

   This is represented internaly as a RECORD_TYPE. The index nodes are
   g95_array_index_type and the data node is a pointer to the data. See below
   for the handling of character types.

   The dtype member is formatted as follows:
    rank = dtype & G95_DTYPE_RANK_MASK // 3 bits
    type = (dtype & G95_DTYPE_TYPE_MASK) >> G95_DTYPE_TYPE_SHIFT // 3 bits
    size = dtype >> G95_DTYPE_SIZE_SHIFT

   I originaly used nested ARRAY_TYPE nodes to represent arrays, but this
   generated poor code for assumed/deferred size arrays.  These require
   use of PLACEHOLDER_EXPR/WITH_RECORD_EXPR, which isn't part of SIMPLE
   grammar.  Also, There is no way to explicitly set the array stride, so
   all data must be packed(1).  I've tried to mark all the functions which
   would require modification with a GCC ARRAYS comment.

   The data component points to the first element in the array.
   The base component points to the origin (ie. array(0, 0...)).  If the array
   does not contain the origin, base points to where it would be in memory.

   An element is accessed by
   base[index0*stride0 + index1*stride1 + index2*stride2]
   This gives good performance as this computation does not involve the
   bounds of the array.  For packed arrays, this is optimized further by
   substituting the known strides.

   This system has one problem: all array bounds must be withing 2^31 elements
   of the origin (2^63 on 64-bit machines).  For example
   integer, dimension (80000:90000, 80000:90000, 2) :: array
   may not work properly on 32-bit machines because 80000*80000 > 2^31, so
   the calculation for stride02 would overflow.  This may still work, but
   I haven't checked and it relies on the overflow doing the right thing.

   The way to fix this problem is to access alements as follows:
   base[(index0-lbound0)*stride0 + (index1-lobound1)*stride1]
   Obviously this is much slower.  I will make this a compile time option,
   something like -fsmall-array-offsets.  Mixing code compiled with and without
   this switch will work.

   (1) This can be worked around by modifying the upper bound of the previous
   dimension.  This requires extra fields in the descriptor (both real_ubound
   and fake_ubound).  In tree.def there is mention of TYPE_SEP, which
   may allow us to do this, however I can't find mention of this anywhere else.

   Arrays of character strings are stored as an array of string pointers. This
   is slightly inefficient mut makes things much simpler.
   The string data immediately follows this block of pointers.
    CHARACTER(LEN=20), DIMENSION(5)
    struct character_array_data
    {
      char * pstr[5];
      char data[100];
    } data;
    for (i = 0; i < 5; i++)
      data.pstr[i] = data.data[i * 20];
 */

static tree
g95_build_array_type (tree type, g95_array_spec * as)
{
  tree lbound[G95_MAX_DIMENSIONS];
  tree ubound[G95_MAX_DIMENSIONS];
  int n;

  for (n = 0 ; n < as->rank; n++)
    {
      /* Create expressions for the bounds of the array.
         Note we re-use ubound and lbound.  */
      switch (as->type)
        {
        case AS_EXPLICIT:
          lbound[n] = g95_conv_array_bound (as->lower[n]);
          ubound[n] = g95_conv_array_bound (as->upper[n]);
          break;

        case AS_ASSUMED_SIZE:
          assert (as->rank == 1);

          /* Fall through...  */

        case AS_ASSUMED_SHAPE:
          if (as->lower[n] == NULL)
            lbound[n] = integer_one_node;
          else
            lbound[n] = g95_conv_array_bound (as->lower[n]);
          ubound[n] = g95_conv_array_bound (as->upper[n]);
          break;

        case AS_DEFERRED:
          lbound[n] = g95_conv_array_bound (as->lower[n]);
          ubound[n] = g95_conv_array_bound (as->upper[n]);
          break;

        default:
          internal_error ("Bad array spec type (%d)", as->type);
          break;
        }
    }

  return g95_get_array_type_bounds (type, as->rank, lbound, ubound);
}

/* Returns the struct descriptor_dimension type.  */
static tree
g95_get_desc_dim_type (void)
{
  tree type;
  tree decl;
  tree fieldlist;

  if (g95_desc_dim_type)
    return g95_desc_dim_type;

  /* Build the type node.  */
  type = make_node (RECORD_TYPE);

  TYPE_NAME (type) = get_identifier ("descriptor_dimension");
  TYPE_PACKED (type) = 1;

  /* Consists of the stride, lbound and ubound members.  */
  decl = build_decl (FIELD_DECL,
                     get_identifier ("stride"),
                     g95_array_index_type);
  DECL_CONTEXT (decl) = type;
  fieldlist = decl;

  decl = build_decl (FIELD_DECL,
                     get_identifier ("lbound"),
                     g95_array_index_type);
  DECL_CONTEXT (decl) = type;
  fieldlist = chainon (fieldlist, decl);

  decl = build_decl (FIELD_DECL,
                     get_identifier ("ubound"),
                     g95_array_index_type);
  DECL_CONTEXT (decl) = type;
  fieldlist = chainon (fieldlist, decl);

  /* Finish off the type.  */
  TYPE_FIELDS (type) = fieldlist;

  g95_finish_type (type);

  g95_desc_dim_type = type;
  return type;
}

static tree
g95_get_dtype_cst (tree type, int rank)
{
  tree size;
  int n;

  if (G95_DESCRIPTOR_TYPE_P (type))
    return (G95_TYPE_DESCRIPTOR_DTYPE(type));

  /* TODO: Correctly identify LOGICAL types.  */
  switch (TREE_CODE (type))
    {
    case INTEGER_TYPE:
      n = G95_DTYPE_INTEGER;
      break;

    case REAL_TYPE:
      n = G95_DTYPE_REAL;
      break;

    case COMPLEX_TYPE:
      n = G95_DTYPE_COMPLEX;
      break;

    case RECORD_TYPE:
      /* Array descriptors have already been dealt with.  */
      n = G95_DTYPE_DERIVED;
      break;

    case POINTER_TYPE:
      n = G95_DTYPE_CHARACTER;
      break;

    default:
      abort ();
    }

  size = TYPE_SIZE_UNIT (type);
  assert (INTEGER_CST_P (size));
  assert (TREE_INT_CST_HIGH (size) == 0);
  assert (rank < G95_DTYPE_RANK_MASK);
  if (TREE_INT_CST_LOW (size) > g95_max_array_element_size)
    internal_error ("Array elemnt size too big");

  n = rank |
      (n << G95_DTYPE_TYPE_SHIFT) |
      (TREE_INT_CST_LOW (size) << G95_DTYPE_SIZE_SHIFT);

  return build_int_2 (n, 0);
}

/* Build an array (descriptor) type with given bounds.  */
/* TODO: remember and reuse array types.  */
/*GCC ARRAYS*/
tree
g95_get_array_type_bounds (tree etype, int dimen, tree * lbound, tree * ubound)
{
  tree fat_type, fat_pointer_type;
  tree fieldlist;
  tree arraytype;
  tree decl;
  int n;
  char name[8+G95_RANK_DIGITS+G95_MAX_SYMBOL_LEN];
  const char *typename;
  tree lower;
  tree upper;
  tree stride;
  tree tmp;

  /* Build the type node.  */
  fat_type = make_node (RECORD_TYPE);
  G95_DESCRIPTOR_TYPE_P (fat_type) = 1;
  TYPE_LANG_SPECIFIC (fat_type) = (struct lang_type *)
    ggc_alloc_cleared (sizeof (struct lang_type));
  G95_TYPE_DESCRIPTOR_RANK (fat_type) = dimen;
  G95_TYPE_DESCRIPTOR_DTYPE (fat_type) = g95_get_dtype_cst (etype, dimen);

  tmp = TYPE_NAME (etype);
  if (tmp && TREE_CODE (tmp) == TYPE_DECL)
    tmp = DECL_NAME (tmp);
  if (tmp)
    typename = IDENTIFIER_POINTER (tmp);
  else
    typename = "unknown";

  sprintf (name, "array"G95_RANK_PRINTF_FORMAT"_%.*s", dimen,
           G95_MAX_SYMBOL_LEN, typename);
  TYPE_NAME (fat_type) = get_identifier (name);
  TYPE_PACKED (fat_type) = 0;

  fat_pointer_type = build_pointer_type (fat_type);

  /* Build an array descriptor record type.  */
  stride = integer_one_node;

  for (n = 0 ; n < dimen; n++)
    {
      G95_TYPE_DESCRIPTOR_STRIDE (fat_type, n) = stride;
      if (lbound)
        lower = lbound[n];
      else
        lower = NULL_TREE;

      if (lower != NULL_TREE)
        {
          if (INTEGER_CST_P (lower))
            G95_TYPE_DESCRIPTOR_LBOUND (fat_type, n) = lower;
          else
            lower = NULL_TREE;
        }

      upper = ubound[n];
      if (upper != NULL_TREE)
        {
          if (INTEGER_CST_P (upper))
            G95_TYPE_DESCRIPTOR_UBOUND (fat_type, n) = upper;
          else
            upper = NULL_TREE;
        }

      if (upper != NULL_TREE && lower != NULL_TREE
          && stride != NULL_TREE)
        {
          tmp = fold (build (MINUS_EXPR, g95_array_index_type, upper,
                            lower));
          tmp = fold (build (PLUS_EXPR, g95_array_index_type, tmp,
                            integer_one_node));
          stride = fold (build (MULT_EXPR, g95_array_index_type, tmp, stride));
          /* Check the folding worked.  */
          assert (INTEGER_CST_P (stride));
        }
      else
        stride = NULL_TREE;
    }
  G95_TYPE_DESCRIPTOR_SIZE (fat_type) = stride;

   /* We define data as an unknown size array. Much better than doing
      pointer arithmetic.  */
  arraytype = build_array_type (etype, build_range_type (
        g95_array_index_type, integer_zero_node, NULL_TREE));

  /* The pointer to the array data.  */
  decl = build_decl (FIELD_DECL,
                     get_identifier ("data"),
                     build_pointer_type (arraytype));

  DECL_CONTEXT (decl) = fat_type;
  /* Add the data member as the first element of the descriptor.  */
  fieldlist = decl;

  /* Add the base component.  */
  decl = build_decl (FIELD_DECL, get_identifier ("base"),
                      build_pointer_type (arraytype));
  DECL_CONTEXT (decl) = fat_type;
  fieldlist = chainon (fieldlist, decl);

  /* Add the dtype component.  */
  decl = build_decl (FIELD_DECL, get_identifier ("dtype"),
                     g95_array_index_type);
  DECL_CONTEXT (decl) = fat_type;
  fieldlist = chainon (fieldlist, decl);

  /* Build the array type for the stride and bound components.  */
  arraytype = build_array_type (g95_get_desc_dim_type (), build_range_type (
        g95_array_index_type, integer_zero_node, g95_rank_cst[dimen - 1]));

  decl = build_decl (FIELD_DECL, get_identifier ("dim"), arraytype);
  DECL_CONTEXT (decl) = fat_type;
  DECL_INITIAL (decl) = NULL_TREE;
  fieldlist = chainon (fieldlist, decl);

  /* Finish off the type.  */
  TYPE_FIELDS (fat_type) = fieldlist;

  g95_finish_type (fat_type);

  return fat_type;
}

/* Build a pointer type. This function is called from g95_sym_type().  */
static tree
g95_build_pointer_type (g95_symbol * sym, tree type)
{
  /* Array pointer types aren't actualy pointers.  */
  if (sym->attr.dimension)
    return type;
  else
    return build_pointer_type (type);
}

/* Return the type for a symbol.  Special handling is required for character
   types to get the correct level of indirection.
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
    if (sym->attr.function || sym->attr.subroutine)
      return TREE_TYPE (TREE_TYPE (sym->backend_decl));
    else
      return TREE_TYPE (sym->backend_decl);
  }

  /* The frontend doesn't set all the attributes for a function with an
     explicit result value, so we use that instead when present.  */
  if (sym->attr.function && sym->result)
    sym = sym->result;

  type = g95_typenode_for_spec (&sym->ts);

  if (sym->ts.type == BT_CHARACTER)
    {
      if (sym->attr.dimension
          || sym->attr.pointer || sym->attr.allocatable)
        type = build_pointer_type (type);
    }

  if (sym->attr.dimension)
    {
      type = g95_build_array_type (type, sym->as);
    }
  else if (sym->ts.type != BT_CHARACTER)
    {
      if (sym->attr.allocatable || sym->attr.pointer)
        type = g95_build_pointer_type (sym, type);

    }
  else if (! (G95_KNOWN_SIZE_STRING_TYPE (type) || sym->attr.dummy))
    {
      type = build_pointer_type (type);
    }

  /* We currently pass all parameters by reference.
     See f95_get_function_decl.  */
  if (sym->attr.dummy)
    type = build_reference_type (type);

  return (type);
}

/* Layout and output debug info for a record type.  */
void
g95_finish_type (tree type)
{
  tree decl;

  decl = build_decl (TYPE_DECL, NULL_TREE, type);
  TYPE_STUB_DECL (type) = decl;
  layout_type (type);
  rest_of_type_compilation (type, 1);
  rest_of_decl_compilation (decl, NULL, 1, 0);
}

/* This is used by g95_get_derived_type.  Not sure what it was meant to do.  */
static void
g95_set_decl_attributes (tree type ATTRIBUTE_UNUSED, symbol_attribute * attr ATTRIBUTE_UNUSED)
{
}

/* Build a tree node for a derived type.  */
static tree
g95_get_derived_type (g95_symbol * derived)
{
  tree typenode, field, field_type, fieldlist;
  g95_component * c;

  assert (derived && derived->attr.flavor == FL_DERIVED);

  if (derived->backend_decl)
    return derived->backend_decl;

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

      /* This returns an array descriptor type.  Initialisation may be
         required.  */
      if (c->dimension)
	  field_type = g95_build_array_type (field_type, c->as);

      /* Pointers to arrays aren't actualy pointer types.  The descriptors
         are seperate, but the data is common.  */
      else if (c->pointer)
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

  g95_finish_type (typenode);

  derived->backend_decl = typenode;

  return typenode;
}

int
g95_return_by_reference (g95_symbol * sym)
{
  if (sym->attr.subroutine)
    return 0;

  assert (sym->attr.function);

  if (sym->result)
    sym = sym->result;

  if (sym->attr.dimension)
    return 1;

  if (sym->ts.type == BT_CHARACTER)
    g95_todo_error ("Returning character variables");

  /* Possibly return derived types by reference.  */
  return 0;
}

tree
g95_get_function_type (g95_symbol * sym)
{
  tree type;
  tree typelist;
  g95_formal_arglist *f;
  g95_symbol *arg;

  /* make sure this symbol is a function or a subroutine.  */
  assert (sym->attr.function || sym->attr.subroutine);

  if (sym->backend_decl)
    return TREE_TYPE (sym->backend_decl);

  typelist = NULL_TREE;
  /* For functions that return arrays we use an extra parameter for the
     return value.  */
  if (g95_return_by_reference (sym))
    {
      if (sym->result)
        arg = sym->result;
      else
        arg = sym;
      type = build_reference_type (g95_sym_type (arg));
      typelist = chainon (typelist, listify (type));
    }
  /* Build the argument types for the function */
  for (f = sym->formal; f; f = f->next)
    {
      arg = f->sym;
      if (arg)
        {
          if (arg->attr.function || arg->attr.subroutine)
            {
              type = g95_get_function_type (arg);
              type = build_pointer_type (type);
            }
          else
            type = g95_sym_type (arg);

          /* Parameter Passing Convention

             We currently pass all parameters by reference.
             Parameters with INTENT(IN) could be passed by value.
             The problem arises if a function is called via an implicit
             prototype. In this situation the INTENT is not known.
             For this reason all parameters to global functions must be
             passed by reference.  Passing by value would potentialy
             generate bad code.  Worse there would be no way of telling that
             this code wad bad, except that it would give incorrect results.

             Contained procedures could pass by value as these are never
             used without an explicit interface, and connot be passed as
             actual parameters for a dummy procedure.
           */
          if (arg->ts.type == BT_CHARACTER)
            typelist = chainon (typelist, listify (g95_strlen_type_node));
          typelist = chainon (typelist, listify (type));
        }
    }

  typelist = chainon (typelist, listify (void_type_node));

  if (sym->attr.subroutine || g95_return_by_reference (sym))
    type=void_type_node;
  else
    type=g95_sym_type (sym);

  type = build_function_type (type, typelist);

  return type;
}

/* Routines for getting integer type nodes */


/* Return an integer type with BITS bits of precision,
   that is unsigned if UNSIGNEDP is nonzero, otherwise signed.  */

tree
g95_type_for_size (unsigned bits, int unsignedp)
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
g95_type_for_mode (enum machine_mode mode, int unsignedp)
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
g95_unsigned_type (tree type)
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

  return g95_signed_or_unsigned_type (1, type);
}

/* Return a signed type the same as TYPE in other respects.  */

tree
g95_signed_type (tree type)
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
g95_signed_or_unsigned_type (int unsignedp, tree type)
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

#include "gt-f95-trans-types.h"
