#ifndef G95_BACKEND_H
#define G95_BACKEND_H

enum
{
  F95_INT1_TYPE,
  F95_INT2_TYPE,
  F95_INT4_TYPE,
  F95_INT8_TYPE,
  F95_INT16_TYPE,
  F95_REAL4_TYPE,
  F95_REAL8_TYPE,
  F95_REAl16_TYPE,
  F95_COMPLEX4_TYPE,
  F95_COMPLEX8_TYPE,
  F95_COMPLEX16_TYPE,
  F95_LOGICAL1_TYPE,
  F95_LOGICAL2_TYPE,
  F95_LOGICAL4_TYPE,
  F95_LOGICAL8_TYPE,
  F95_LOGICAL16_TYPE,
  F95_CHARACTER1_TYPE,
  NUM_F95_TYPES
};

#define G95_DTYPE_RANK_MASK 0x07
#define G95_DTYPE_TYPE_SHIFT 3
#define G95_DTYPE_TYPE_MASK 0x38
#define G95_DTYPE_SIZE_SHIFT 6

enum
{
  G95_DTYPE_UNKNOWN = 0,
  G95_DTYPE_INTEGER,
  /* TODO: recognize logical types.  */
  G95_DTYPE_LOGICAL,
  G95_DTYPE_REAL,
  G95_DTYPE_COMPLEX,
  G95_DTYPE_DERIVED,
  G95_DTYPE_CHARACTER
};

extern GTY(()) tree g95_type_nodes[NUM_F95_TYPES];

extern int g95_array_index_kind;
extern GTY(()) tree g95_array_index_type;
extern GTY(()) tree ppvoid_type_node;
extern GTY(()) tree pchar_type_node;

#define g95_int1_type_node  g95_type_nodes[F95_INT1_TYPE]
#define g95_int2_type_node  g95_type_nodes[F95_INT2_TYPE]
#define g95_int4_type_node  g95_type_nodes[F95_INT4_TYPE]
#define g95_int8_type_node  g95_type_nodes[F95_INT8_TYPE]
#define g95_int16_type_node g95_type_nodes[F95_INT16_TYPE]

#define g95_real4_type_node  g95_type_nodes[F95_REAL4_TYPE]
#define g95_real8_type_node  g95_type_nodes[F95_REAL8_TYPE]
#define g95_real16_type_node g95_type_nodes[F95_REAL16_TYPE]

#define g95_complex4_type_node  g95_type_nodes[F95_COMPLEX4_TYPE]
#define g95_complex8_type_node  g95_type_nodes[F95_COMPLEX8_TYPE]
#define g95_complex16_type_node g95_type_nodes[F95_COMPLEX16_TYPE]

#define g95_logical1_type_node  g95_type_nodes[F95_LOGICAL1_TYPE]
#define g95_logical2_type_node  g95_type_nodes[F95_LOGICAL2_TYPE]
#define g95_logical4_type_node  g95_type_nodes[F95_LOGICAL4_TYPE]
#define g95_logical8_type_node  g95_type_nodes[F95_LOGICAL8_TYPE]
#define g95_logical16_type_node g95_type_nodes[F95_LOGICAL16_TYPE]

#define g95_character1_type_node g95_type_nodes[F95_CHARACTER1_TYPE]

#define g95_strlen_type_node g95_int4_type_node

/* be-function.c */
void g95_convert_function_code (g95_namespace *);

/* trans-types.c */
void g95_init_types (void);

tree g95_get_int_type (int);
tree g95_get_real_type (int);
tree g95_get_complex_type (int);
tree g95_get_logical_type (int);
tree g95_get_character_type (int, g95_charlen *);

tree g95_sym_type (g95_symbol *);
tree g95_typenode_for_spec (g95_typespec *);

tree g95_type_spec (g95_typespec *);
tree g95_get_function_type (g95_symbol *);

tree g95_type_for_size (unsigned, int);
tree g95_type_for_mode (enum machine_mode, int);
tree g95_unsigned_type (tree);
tree g95_signed_type (tree);
tree g95_signed_or_unsigned_type (int, tree);

tree g95_get_element_type (tree);
tree g95_get_array_type_bounds (tree, int, tree *, tree *);

/* Layout and output debugging info for a type.  */
void g95_finish_type (tree);

/* Some functions have an extra parameter for the return value.  */
int g95_return_by_reference (g95_symbol *);

#endif
