/* Header for code translation functions
   Copyright (C) 2000, 2001, 2002 Free Software Foundation, Inc.
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

#ifndef G95_TRANS_H
#define G95_TRANS_H

/* Mangled symbols take the form __module__name.  */
#define G95_MAX_MANGLED_SYMBOL_LEN  (G95_MAX_SYMBOL_LEN*2+4)

/* Struct for holding a block of statements.  It should be treated as an
   opaque entity and not modified directly.  This allows us to change the
   underlying representation of statement lists.  */
typedef struct
{
  tree head;
  int has_scope:1;
} stmtblock_t;

/* a simplified expresson */
typedef struct g95_se
{
  /* Code blocks to be executed before and after using the value.  */
  stmtblock_t pre;
  stmtblock_t post;

  /* the result of the expression */
  tree expr;

  /* The length of a character string value.  */
  tree string_length;

  /* If not set 95_conv_variable will return an expression for the array
     descriptor.  Otherwise it will substitute scalarizing variables.  If no
     scalarizing variables have been setup, it will throw an error.  */
  unsigned descriptor_only:1;
  /* When this is set the address or descriptor of a variable is returned.
     Only applies to EXPR_VARIABLE nodes.  */
  unsigned want_pointer:1;

  /* An array function call returning without a temporary.  */
  unsigned direct_byref:1;

  /* Ignore absent optional arguments.  Used for some intrinsics.  */
  unsigned ignore_optional:1;

  /* Scalarization parameters.  */
  struct g95_se *parent;
  struct g95_ss *ss;
  struct g95_loopinfo *loop;
} g95_se;

/* Scalarisation State chain.  Created by walking an expression tree before
   creating the scalarization loops. Then passed as part of a g95_se structure
   to translate the expression inside the loop.  Note that these chains are
   terminated by g95_se_terminator, not NULL.  A NULL pointer in a g95_se
   indicates to g95_conv_* that this is a scalar expression.
   Note that some member arrays correspond to scalarizer rank and others
   are variable rank.  */
typedef struct g95_ss_info
{
  int dimen;
  /* The ref that holds information on this section.  */
  g95_ref *ref;
  /* The descriptor of this array.  */
  tree descriptor;
  /* holds the pointer to the data array.  */
  tree data;
  tree pdata;
  /* To move some of the array index calculation out of the innermost loop.  */
  tree offset;
  tree stride0;
  /* Holds the SS for a subscript.  Indexed by actual dimension.  */
  struct g95_ss *subscript[G95_MAX_DIMENSIONS];
  /* stride and delta are used to access this inside a scalarization loop.
     start is used in the calculation of these.  Indexed by scalarizer
     dimension.  */
  tree start[G95_MAX_DIMENSIONS];
  tree stride[G95_MAX_DIMENSIONS];
  tree delta[G95_MAX_DIMENSIONS];

  /* Translation from scalariser dimensions to actual dimensions.
      actual = dim[scalarizer]  */
  int dim[G95_MAX_DIMENSIONS];
} g95_ss_info;

typedef enum
{
  G95_SS_SCALAR,
  G95_SS_REFERENCE,
  G95_SS_SECTION,
  G95_SS_FUNCTION,
  G95_SS_CONSTRUCTOR,
  G95_SS_VECTOR,
  G95_SS_TEMP,
  G95_SS_INTRINSIC
} g95_ss_type;

/* SS structures can only belong to a single loopinfo.  They must be added
   otherwise they will not get freed.  */
typedef struct g95_ss
{
  g95_ss_type type;
  g95_expr *expr;
  union
  {
    /* If type is G95_SS_SCALAR or G95_SS_REFERENCE.  */
    struct
    {
      tree expr;
      tree string_length;
    } scalar;

    /* G95_SS_TEMP.  */
    struct
    {
      int dimen;
      tree type;
      tree string_length;
    } temp;
    /* All other types.  */
    g95_ss_info info;
  } data;

  /* All the SS in a loop and linked through loop_chain.  The SS for an
     expression are linked by the next pointer.  */
  struct g95_ss *loop_chain;
  struct g95_ss *next;

  unsigned useflags:2;
} g95_ss;
#define g95_get_ss() g95_getmem(sizeof(g95_ss))

/* The contents of this aren't actualy used.  */
extern g95_ss *g95_ss_terminator;

/* Holds information about an expression while it is being scalarized.  */
typedef struct g95_loopinfo
{
  stmtblock_t pre;
  stmtblock_t post;

  int dimen;

  g95_ss *ss;
  g95_ss *temp_ss;

  tree loopvar[G95_MAX_DIMENSIONS];
  tree from[G95_MAX_DIMENSIONS];
  tree to[G95_MAX_DIMENSIONS];
  g95_ss *specloop[G95_MAX_DIMENSIONS];

  /* The code member contains the code for the body of the next outer loop.  */
  stmtblock_t code[G95_MAX_DIMENSIONS];

  /* Order in which the dimensions should be looped, innermost first.  */
  int order[G95_MAX_DIMENSIONS];
  /* The number of dimensions for which a temporary is used.  */
  int temp_dim;
  /* If set we don't need the loop variables.  */
  unsigned array_parameter:1;
} g95_loopinfo;

/* Advance the SS chain to the next term.  */
void g95_advance_se_ss_chain (g95_se *);

/* Call this to initialise a g95_se structure before use
   first parameter is structure to initialise, second is
   parent to get scalarization data from, or NULL.  */
void g95_init_se(g95_se *, g95_se *);

/* Build an expression with void type.  */
void build_v (int code, ...);

/* Create an artificial variable decl and add it to the current scope.  */
tree g95_create_var (tree, const char *);
/* Like above but doesn't add it to the current scope.  */
tree g95_create_var_np (tree, const char *);

/* Store the result of an expression in a temp variable so it can be used
   repeatedly even if the original changes */
void g95_make_safe_expr(g95_se * se);

/* Makes sure se is suitable for passing as a function string parameter.  */
void g95_conv_string_parameter (g95_se *se);

/* Add an item to the end of TREE_LIST.  */
tree g95_chainon_list (tree, tree);

/* When using the g95_conv_* make sure you understand what they do, ie.
   when a POST chain may be created, and what the retured expression may be
   used for.  Note that character strings have special handling.  This
   should not be a problem as most statements/operations only deal with
   numeric/logical types.  */

/* Entry point for expression translation.  */
void g95_conv_expr (g95_se * se, g95_expr * expr);
/* Like g95_conv_expr, but the POST block is guaranteed to be empty for
   numeric expressions.  */
void g95_conv_expr_val (g95_se * se, g95_expr * expr);
/* Like g95_conv_expr_val, but the value is also suitable for use in the lhs of
   an assignment.  */
void g95_conv_expr_lhs (g95_se * se, g95_expr * expr);
/* Converts an expression so that it can be passed be reference.  */
void g95_conv_expr_reference (g95_se * se, g95_expr *);
/* Equivalent to convert(type, g95_conv_expr_val(se, expr)).  */
void g95_conv_expr_type (g95_se * se, g95_expr *, tree);
/* If the value is not constant, Create a temporary and copy the value.  */
tree g95_evaluate_now (tree, stmtblock_t *);

/* Intrinsic function handling.  */
void g95_conv_intrinsic_function (g95_se *, g95_expr *);

/* Does an intrinsic map directly to an external library call.  */
int g95_is_intrinsic_libcall (g95_expr *);

/* Also used to CALL subroutines.  */
void g95_conv_function_call (g95_se *, g95_symbol *, g95_actual_arglist *);
/* g95_trans_* shouldn't call push/poplevel, use g95_push/pop_scope */

/* Generate code for a scalar assignment.  */
tree g95_trans_scalar_assign (g95_se *, g95_se *, bt);

/* Generate code to allocate a string temporary.  */
tree g95_conv_string_tmp (g95_se *, tree, tree);
/* Get the length of a string.  */
tree g95_conv_string_length (tree);
/* Initialize a string length variable.  */
tree g95_conv_init_string_length (g95_symbol *, stmtblock_t *);

/* Add an expression to the end of a block.  */
void g95_add_expr_to_block (stmtblock_t *, tree);
/* Add a block to the end of a block.  */
void g95_add_block_to_block (stmtblock_t *, stmtblock_t *);
/* Add a MODIFY_EXPR to a block.  */
void g95_add_modify_expr (stmtblock_t *, tree, tree);

/* Initialize a statement block.  */
void g95_init_block (stmtblock_t *);
/* Start a new satement block.  Like g95_init_block but also starts a new
   variable scope.  */
void g95_start_block (stmtblock_t *);
/* Finish a statement block.  Also closes the scope if the block was created
   with g95_start_block.  */
tree g95_finish_block (stmtblock_t *);
/* Merge the scope of a block with its parent.  */
void g95_merge_block_scope (stmtblock_t * block);

/* Return the backend label decl.  */
tree g95_get_label_decl(g95_st_label *);

/* Return the decl for an external function.  */
tree g95_get_extern_function_decl (g95_symbol *);

/* Return the decl for a function.  */
tree g95_get_function_decl (g95_symbol *);

/* Build a CALL_EXPR.  */
tree g95_build_function_call (tree, tree);

/* Creates an label.  Decl is artificial if label_id == NULL_TREE.  */
tree g95_build_label_decl(tree);

/* Return the decl used to hold the function return value.
   Do not use if the function has an explicit result variable.  */
tree g95_get_fake_result_decl(g95_symbol *);

/* Get the return label for the current function.  */
tree g95_get_return_label (void);

/* Make prototypes for runtime library functions.  */
void g95_build_builtin_function_decls (void);

/* Return the variable decl for a symbol.  */
tree g95_get_symbol_decl (g95_symbol *);

/* Allocate the lang-spcific part of a decl node.  */
void g95_allocate_lang_decl (tree);

/* Advance along a TREE_CHAIN.  */
tree g95_advance_chain (tree, int);

/* Generate the code for a function.  */
void g95_generate_function_code (g95_namespace *);
/* Output a decl for a module variable.  */
void g95_generate_module_vars (g95_namespace *);

/* Get and set the current location.  */
void g95_set_backend_locus (locus *);
void g95_get_backend_locus (locus *);

/* Handle static constructor functions.  */
extern GTY(()) tree g95_static_ctors;
void g95_generate_constructors (void);

/* Generate a runtime error check.  */
void g95_trans_runtime_check (tree, tree, stmtblock_t *);

/* Generate code for an assigment, includes scalarization.  */
tree g95_trans_assignment (g95_expr *, g95_expr *);

/* Initialize function decls for library functions.  */
void g95_build_intrinsic_lib_fndecls (void);
/* Create function decls for IO library functions.  */
void g95_build_io_library_fndecls (void);
/* Build a function decl for a library function.  */
tree g95_build_library_function_decl VPARAMS((tree name, tree rettype,
                                              int nargs, ...));

/* We have a single io state for a procedure.  */
extern GTY(()) tree g95_current_io_state;

/* somewhere! */
tree pushdecl (tree);
void pushlevel (int);
tree poplevel (int, int, int);
void expand_function_body (tree, int);
tree getdecls(void);

/* Runtime library function decls.  */
extern GTY(()) tree gfor_fndecl_push_context;
extern GTY(()) tree gfor_fndecl_pop_context;
extern GTY(()) tree gfor_fndecl_internal_malloc;
extern GTY(()) tree gfor_fndecl_internal_malloc64;
extern GTY(()) tree gfor_fndecl_internal_free;
extern GTY(()) tree gfor_fndecl_allocate;
extern GTY(()) tree gfor_fndecl_allocate64;
extern GTY(()) tree gfor_fndecl_deallocate;
extern GTY(()) tree gfor_fndecl_stop;
extern GTY(()) tree gfor_fndecl_runtime_error;
extern GTY(()) tree gfor_fndecl_repack[G95_MAX_DIMENSIONS];

/* Math functions.  Many other math functions are handled in
   trans-intrinsic.c.  */
extern GTY(()) tree gfor_fndecl_math_powf;
extern GTY(()) tree gfor_fndecl_math_pow;
extern GTY(()) tree gfor_fndecl_math_cpowf;
extern GTY(()) tree gfor_fndecl_math_cpow;
extern GTY(()) tree gfor_fndecl_math_cabsf;
extern GTY(()) tree gfor_fndecl_math_cabs;
extern GTY(()) tree gfor_fndecl_math_sign4;
extern GTY(()) tree gfor_fndecl_math_sign8;
extern GTY(()) tree gfor_fndecl_math_ishftc4;
extern GTY(()) tree gfor_fndecl_math_ishftc8;

/* String functions.  */
extern GTY(()) tree gfor_fndecl_copy_string;
extern GTY(()) tree gfor_fndecl_compare_string;
extern GTY(()) tree gfor_fndecl_concat_string;
extern GTY(()) tree gfor_fndecl_string_len_trim;
extern GTY(()) tree gfor_fndecl_adjustl;
extern GTY(()) tree gfor_fndecl_adjustr;

/* Other misc. runtime library functions.  */
extern GTY(()) tree gfor_fndecl_size0;
extern GTY(()) tree gfor_fndecl_size1;

/* True if node is an integer constant.  */
#define INTEGER_CST_P(node) (TREE_CODE(node) == INTEGER_CST)

/* G95-specific declaration information.  */

/* Array types only.  */
struct lang_type GTY (())
{
  int rank;
  tree lbound[G95_MAX_DIMENSIONS];
  tree ubound[G95_MAX_DIMENSIONS];
  tree stride[G95_MAX_DIMENSIONS];
  tree size;
  tree offset;
  tree dtype;
  tree dataptr_type;
};

/* String nodes only.  */
struct lang_decl GTY (())
{
  tree stringlength;
  tree saved_descriptor;
};

#define G95_DECL_STRING_LENGTH(node) (DECL_LANG_SPECIFIC(node)->stringlength)
#define G95_DECL_SAVED_DESCRIPTOR(node) \
  (DECL_LANG_SPECIFIC(node)->saved_descriptor)
#define G95_DECL_STRING(node) DECL_LANG_FLAG_0(node)
#define G95_DECL_PACKED_ARRAY(node) DECL_LANG_FLAG_1(node)
#define G95_DECL_PARTIAL_PACKED_ARRAY(node) DECL_LANG_FLAG_2(node)

#define G95_KNOWN_SIZE_STRING_TYPE(node) TYPE_LANG_FLAG_0(node)
/* An array descriptor.  */
#define G95_DESCRIPTOR_TYPE_P(node) TYPE_LANG_FLAG_1(node)
/* An array without a descriptor.  */
#define G95_ARRAY_TYPE_P(node) TYPE_LANG_FLAG_2(node)
/* The G95_TYPE_ARRAY_* members are present in both descriptor and
   descriptorless array types.  */
#define G95_TYPE_ARRAY_LBOUND(node, dim) \
  (TYPE_LANG_SPECIFIC(node)->lbound[dim])
#define G95_TYPE_ARRAY_UBOUND(node, dim) \
  (TYPE_LANG_SPECIFIC(node)->ubound[dim])
#define G95_TYPE_ARRAY_STRIDE(node, dim) \
  (TYPE_LANG_SPECIFIC(node)->stride[dim])
#define G95_TYPE_ARRAY_RANK(node) (TYPE_LANG_SPECIFIC(node)->rank)
#define G95_TYPE_ARRAY_SIZE(node) (TYPE_LANG_SPECIFIC(node)->size)
#define G95_TYPE_ARRAY_OFFSET(node) (TYPE_LANG_SPECIFIC(node)->offset)
#define G95_TYPE_ARRAY_DTYPE(node) (TYPE_LANG_SPECIFIC(node)->dtype)
#define G95_TYPE_ARRAY_DATAPTR_TYPE(node) \
  (TYPE_LANG_SPECIFIC(node)->dataptr_type)

/* I changed this from sorry(...) because it should not return.  */
/* TODO: Remove g95_todo_error before releasing g95.  */
#define g95_todo_error(args...) fatal_error("g95_todo: Not Implemented: " args)

#define build_v(code, args...) build(code, void_type_node, args)

#endif /* G95_TRANS_H */
