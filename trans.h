/* Header for code translation functions
   Copyright (C) 2000 - 2003 Free Software Foundation, Inc.
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


#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "tree.h"
#include "tree-simple.h"
#include "tree-inline.h"
#include "flags.h"
#include "ggc.h"
#include "toplev.h"
#include "function.h"
#include "intl.h"
#include "real.h"
#include "target.h"
#include "langhooks.h"
#include "langhooks-def.h"
#include "timevar.h"
#include "debug.h"
#include "diagnostic.h"
#include "c-common.h"

#include <assert.h>
#include "g95.h"


/* Structure for holding a block of statements.  It should be treated
 * as an opaque entity and not modified directly.  This allows us to
 * change the underlying representation of statement lists.  */

typedef struct {
  tree head;
  int has_scope:1;
} stmtblock_t;

/* A simplified expresson */

typedef struct g95_se {
  /* Code blocks to be executed before and after using the value.  */
  stmtblock_t pre;
  stmtblock_t post;

  /* The result of the expression */
  tree expr;

  /* The length of a character string value.  */
  tree string_length;
  int reflevel;

  struct g95_se *parent;
} g95_se;


typedef struct g95_trans_context {
  g95_namespace *current_procedure;

  tree saved_current_function, current_function_decl, result_var_decl;
  int frame_size;

  stmtblock_t pre, post;

  struct g95_trans_context *parent;
} g95_trans_context;

extern g95_trans_context *g95_context;


typedef struct {
  g95_typespec ts;
  g95_array_spec *as;
  int pointer, dummy, static_storage;

  g95_expr *value;
  tree desc;
} variable_info;


#define BLANK_COMMON_NAME     PREFIX "blank_common"
#define BLANK_BLOCK_DATA_NAME PREFIX "blank_block_data"


/* trans.c */

tree g95_call_library VPARAMS((tree, char *, ...));

void g95_init_se(g95_se *, g95_se *);
tree g95_create_var(tree);
tree g95_create_var_np(tree);
tree g95_chainon_list(tree, tree);
tree g95_prepend_list(tree, tree);

void g95_conv_expr(g95_se *, g95_expr *);
tree g95_conv_char_length(g95_se *, g95_typespec *);
void g95_conv_expr0(g95_se *, g95_expr *);
void g95_conv_expr_type(g95_se *, g95_expr *, tree);

tree g95_trans_scalar_assign(g95_se *, g95_se *, bt);
tree g95_conv_string_tmp(g95_se *, tree, tree);

void g95_add_expr_to_block(stmtblock_t *, tree);
void g95_add_block_to_block(stmtblock_t *, stmtblock_t *);
tree g95_unique_identifier(char *);
void g95_add_modify_expr(stmtblock_t *, tree, tree);
void g95_init_block(stmtblock_t *);
void g95_start_block(stmtblock_t *);

tree g95_finish_block(stmtblock_t *);

void g95_set_backend_locus(locus *);
void g95_get_backend_locus(locus *);
void g95_save_expr(g95_se *);

tree g95_trans_assignment(g95_expr *, g95_expr *);
void g95_build_io_library_fndecls(void);
tree g95_library_decl VPARAMS((char *name, tree rettype, int nargs, ...));
tree g95_call_procedure_alloc(tree, tree);
void g95_call_temp_alloc(stmtblock_t *, tree, tree);
void g95_call_temp_free(stmtblock_t *, tree);

/* trans-array.c */

void g95_conv_array_ref(g95_se *, g95_array_ref *, g95_typespec *, int);
void g95_fix_dummy_array(g95_symbol *);
tree g95_adesc_base(tree);
void g95_temp_array_descriptor(g95_se *, variable_info *, tree, tree);
void g95_set_section_info(g95_se *, int, tree);

tree g95_get_adesc_lbound(tree, tree);
tree g95_get_adesc_ubound(tree, tree);
void g95_array_argument(g95_se *, g95_actual_arglist *);
void g95_init_array_types(void);
tree g95_get_array_desc(int);
tree g95_get_array_storage(variable_info *);
void g95_init_array_desc(variable_info *, tree, tree);

/* trans-types.c */
tree g95_get_element_type(tree);
tree g95_typenode_for_spec(g95_typespec *);
void g95_finish_type(tree);

tree g95_result_type(g95_symbol *);
void g95_component_vinfo(g95_component *, variable_info *);
void g95_symbol_vinfo(g95_symbol *, variable_info *);
tree g95_get_descriptor(variable_info *);
tree g95_get_storage(variable_info *);
void g95_init_descriptor(variable_info *, tree, tree);

/* trans-const.c  */
tree g95_resize_string_constant(tree, tree);
tree g95_conv_mpf_to_tree(mpf_t, int);
tree g95_build_string_const(int, char *);
tree g95_build_const(tree, tree);
void g95_conv_constant(g95_se *, g95_expr *);

/* trans-decl.c */

tree g95_sym_identifier(g95_symbol *sym, char *);
void g95_build_procedure_decl(g95_symbol *);
void g95_add_decl_to_function(tree, g95_symbol *);
tree g95_initial_value(g95_symbol *);

tree g95_get_label_decl(g95_st_label *);
void g95_get_symbol_decl(g95_symbol *);
tree g95_get_extern_function_decl(g95_symbol *);
tree g95_get_function_decl(g95_symbol *);
tree g95_build_function_call(tree, tree);
tree g95_build_label_decl(tree);
tree g95_get_fake_result_decl(g95_symbol *);
tree g95_get_return_label(void);
void g95_build_builtin_function_decls(void);
void g95_generate_procedure(g95_namespace *);
void g95_generate_procedure_variables(g95_namespace *);

/* trans-expr.c */

tree g95_temp_string(g95_se *, tree);
void g95_reflevel(g95_se *, int);
int g95_stack_variable(tree);

/* trans-intrinsic.c */

void g95_dim(g95_se *, tree, tree);

/* trans-expr.c */

tree g95_trans_pointer_assign(g95_code *);
void g95_conv_intrinsic_function(g95_se *, g95_expr *);
int g95_is_intrinsic_libcall(g95_expr *);
void g95_conv_function_call(g95_se *, g95_symbol *, g95_actual_arglist *);
tree g95_trans_arglist(g95_actual_arglist *, g95_se *);

/* trans-stmt.c */
tree g95_trans_code(g95_code *);

/* trans-io.c */
tree g95_trans_open(g95_code *);
tree g95_trans_close(g95_code *);
tree g95_trans_read(g95_code *);
tree g95_trans_write(g95_code *);
tree g95_trans_iolength(g95_code *);
tree g95_trans_backspace(g95_code *);
tree g95_trans_endfile(g95_code *);
tree g95_trans_inquire(g95_code *);
tree g95_trans_rewind(g95_code *);

tree g95_trans_transfer(g95_code *);
tree g95_trans_dt_end(g95_code *);

/* trans-types.c */

void g95_init_types(void);
tree g95_get_int_type(int);
tree g95_get_real_type(int);
tree g95_get_complex_type(int);
tree g95_get_logical_type(int);
tree g95_get_character_type(int, g95_charlen *);
tree g95_sym_type(g95_symbol *);
tree g95_type_spec(g95_typespec *);
tree g95_procedure_type(g95_symbol *);
tree g95_type_for_size(unsigned, int);
tree g95_type_for_mode(enum machine_mode, int);
tree g95_unsigned_type(tree);
tree g95_signed_type(tree);
tree g95_signed_or_unsigned_type(int, tree);
tree g95_get_array_type_bounds(tree, int, tree *, tree *);
tree g95_dummy_arg_type(g95_symbol *);

/* data.c */

tree g95_generate_data(g95_symbol *);
tree g95_conv_array_initializer(variable_info *);
void g95_start_common(void);
void g95_init_common_var(g95_symbol *, int);
tree g95_data_initializer(int);

/* trans-common.c */

int g95_element_number(g95_array_ref *, g95_array_spec *);
void g95_trans_common(g95_namespace *);



tree g95_conv_mpz_to_tree(mpz_t, int);
void g95_set_error_locus(stmtblock_t *, locus *);
tree g95_add_field(tree stype, char *name, tree type);
tree g95_conv_array_ubound(tree, int);
tree g95_conv_array_lbound(tree, int);
void g95_trans_data(g95_namespace *);

enum {
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


extern tree g95_type_nodes[NUM_F95_TYPES];
extern tree ppvoid_type_node;
extern tree pvoid_type_node;
extern tree pchar_type_node;
extern tree g95_result_var_decl;



#define g95_int1_type_node  g95_type_nodes[F95_INT1_TYPE]
#define g95_int2_type_node  g95_type_nodes[F95_INT2_TYPE]
#define g95_int4_type_node  g95_type_nodes[F95_INT4_TYPE]
#define g95_int8_type_node  g95_type_nodes[F95_INT8_TYPE]
#define g95_int16_type_node g95_type_nodes[F95_INT16_TYPE]

#define g95_default_integer g95_type_nodes[F95_INT4_TYPE]

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


/* be-function.c */
void g95_convert_function_code(g95_namespace *);


/* somewhere! */
tree pushdecl(tree);
void pushlevel(int);
tree poplevel(int, int, int);
void expand_function_body(tree, int);
tree getdecls(void);

/* The remaining space available for stack variables.  */
extern unsigned HOST_WIDE_INT g95_stack_space_left;

/* Runtime library function decls.  */

extern GTY(()) tree library_procedure_alloc;
extern GTY(()) tree library_temp_alloc;
extern GTY(()) tree library_temp_free;

/* External math functions */

extern GTY(()) tree library_integer_4_power;
extern GTY(()) tree library_real_4_power;
extern GTY(()) tree library_real_8_power;
extern GTY(()) tree library_complex_4_power;
extern GTY(()) tree library_complex_8_power;

extern GTY(()) tree library_pow_complex_4;
extern GTY(()) tree library_pow_complex_8;

extern GTY(()) tree gfor_fndecl_math_powf;
extern GTY(()) tree gfor_fndecl_math_pow;
extern GTY(()) tree gfor_fndecl_math_cabsf;
extern GTY(()) tree gfor_fndecl_math_cabs;
extern GTY(()) tree gfor_fndecl_math_ishftc4;
extern GTY(()) tree gfor_fndecl_math_ishftc8;

/* True if node is an integer constant. */
#define INTEGER_CST_P(node) (TREE_CODE(node) == INTEGER_CST)

/* G95-specific declaration information. */

struct lang_type GTY (()) {};
struct lang_decl GTY (()) {};

/* Nonzero if the type is a descriptor (character or array) */

#define G95_DESCRIPTOR_P(node) TREE_LANG_FLAG_0(node)
#define G95_CONSTANT_DESC(node) TREE_LANG_FLAG_1(node)

#define CONSTANT_P(X) \
  (TREE_CODE(X) == INTEGER_CST || \
   TREE_CODE(X) == REAL_CST || \
   TREE_CODE(X) == COMPLEX_CST)

#define STRING_P(x) (TREE_CODE(x) == STRING_CST)


#define g95_todo_error(args...) fatal_error("g95_todo: Not Implemented: " args)
#define build_v(code, args...) build(code, void_type_node, args)
