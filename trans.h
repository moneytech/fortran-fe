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

/* Currently mangled symbols take the form __module__name.  */
#define G95_MAX_MANGLED_SYMBOL_LEN  (G95_MAX_SYMBOL_LEN*2+4)

/* a simplified expresson */
typedef struct g95_se
{
  /* two chains of *_STMT trees */
  tree pre;
  tree pre_tail;
  tree post;
  tree post_tail;

  /* the result of the expression */
  tree expr;

  /* The length of a character string value.  */
  tree string_length;

  /* If not set 95_conv_variable will return an expression for the array
     descriptor.  Otherwise it will substitute scalarizing variables.  If no
     scalarizing variables have been setup, it will throw an error.
     Setting this also causes the pointer for non array POINTER or ALLOCATABLE
     variables to be returned, rather than the value.  */
  int descriptor_only;
  int want_pointer;

  /* Scalarization parameters.  */
  struct g95_se *parent;
  tree loopvar[G95_MAX_DIMENSIONS];
  struct g95_ss *ss;
} g95_se;

/* Scalarisation State chain.  Created by walking an expression tree before
   creating the scalarization loops. Then passed as part of a g95_se structure
   to translate the expression inside the loop.  Note that these chains are
   terminated by g95_se_terminator, not NULL.  A NULL pointer in a g95_se
   indicates to g95_conv_* that this should be a scalar expression.  */
typedef struct g95_ss_info
{
  g95_ref *ref;
  tree descriptor;
  tree data;
  tree pdata;
  tree start[G95_MAX_DIMENSIONS];
  tree stride[G95_MAX_DIMENSIONS];
  tree delta[G95_MAX_DIMENSIONS];

  /* Translation from scalariser dimensions to actual dimensions.  */
  int dim[G95_MAX_DIMENSIONS];
} g95_ss_info;

/* TODO: Use GCC Garbage Collection for g95_ss.
   Keeping track of them is easy now, but may become less so as they get
   used for other scalarizations (IO, array parameters, FORALL, WHERE).  */
typedef struct g95_ss
{
  int dimen;
  g95_expr *expr;
  union
  {
    /* If dimen == 0.  */
    g95_se se;
    /* If dimen > 0.  */
    g95_ss_info info;
  } data;

  struct g95_ss *next;
} g95_ss;
#define g95_get_ss() g95_getmem(sizeof(g95_ss))

/* The contents of this aren't actualy used.  */
extern g95_ss *g95_ss_terminator;

/* Holds information about an expression while it is being scalarized.  */
typedef struct g95_loopinfo
{
  int dimen;

  g95_ss *ss;
  g95_ss *temp_ss;

  tree loopvar[G95_MAX_DIMENSIONS];
  tree from[G95_MAX_DIMENSIONS];
  tree to[G95_MAX_DIMENSIONS];
  g95_ss *specloop[G95_MAX_DIMENSIONS];

  /* Order in which the dimensions should be looped, innermost first.  */
  int order[G95_MAX_DIMENSIONS];

  tree pre, pre_tail;
  tree post, post_tail;
} g95_loopinfo;

/* See g95_build_array_type
   1 = Type A
   0 = Type B
   This should not be changed during compilation.  */
extern int g95_use_gcc_arrays;

/* Advance the SS chain to the next term.  */
void g95_advance_se_ss_chain (g95_se *);

/* Call this to initialise a g95_se structure before use
   first parameter is structure to initialise, second is
   parent to get scalarization data from, or NULL.  */
void g95_init_se(g95_se *, g95_se *);

/* helpers for adding to stmt chains */
/* Add statements to a g95_se->pre chain.  */
#define g95_add_stmt_to_pre(se, h, t) \
            g95_add_stmt_to_list(&(se)->pre, &(se)->pre_tail, h, t)
/* Add statements to a g95_se->post chain.  */
#define g95_add_stmt_to_post(se, h, t) \
            g95_add_stmt_to_list(&(se)->post, &(se)->post_tail, h, t)

/* Like chainon() but updates both head and tail.
   Used by g95_add_stmt_to_(pre|post).  */
void g95_add_stmt_to_list(tree *, tree *, tree, tree);

/* create a temporary variable in the current scope.  */
tree g95_create_tmp_var(tree);

/* create a temporary variable, but do NOT put it in the current scope.  */
tree g95_create_tmp_alias_var(tree);

/* store the result of an expression on a temp variable so it can be used
 * repeatedly even if the original changes */
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

/* Suitable for array indices and function parameters
   ie. either a constant of a variable.
   Guaranteed to return post=NULL for non-character values.  */
void g95_conv_simple_val(g95_se *, g95_expr *);
/* Like g95_conv_simple_val except the value will be of specified type.  Does
   not work with character types.  */
void g95_conv_simple_val_type(g95_se *, g95_expr *, tree);
/* Suitable for use in if constructs, etc.
   Guaranteed to return post=NULL for non-character values.  */
void g95_conv_simple_cond(g95_se *, g95_expr *);
/* Returns an lvalue, throws error if not possble.
   Guaranteed to return post=NULL for non-character values.  */
void g95_conv_simple_lhs(g95_se *, g95_expr *);

/* Simple expression suitable for RHS of assignment.
   Can return a non-NULL post tree */
void g95_conv_simple_rhs(g95_se *, g95_expr *);

/* Translate a expression to pass by reference.  */
void g95_conv_simple_reference (g95_se *, g95_expr *);

/* Intrinsic function handling.  */
void g95_conv_intrinsic_function (g95_se *, g95_expr *);

/* Also used to CALL subroutines.  */
void g95_conv_function_call (g95_se *, g95_symbol *, g95_actual_arglist *);
/* g95_trans_* shouldn't call push/poplevel, use g95_push/pop_scope */

/* Start a new satement block.  */
void g95_start_stmt (void);
/* Finish a statement block.  The two paramenters are the head and tail of the
   code for the block.  Returns a COMPOUNT_STMT containing the code, decls
   for temporaries and scope neatly wrapped up in a single COMPOUNT_STMT.  */
tree g95_finish_stmt (tree, tree);
/* We've decided we don't need this scope, so merge it with the parent.
   Only variable decls will be merged, you still need to add the code.  */
void g95_merge_stmt (void);
/* Like g95_finish_stmt, but only wraps in COMPOUND_STMT if there are variable
   decls in the scope.  */
void g95_finish_se_stmt (g95_se *);

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
tree g95_get_fake_result_decl(void);

/* Get the return label for the current function.  */
tree g95_get_return_label (void);

/* Make prototypes for runtime library functions.  */
void g95_build_builtin_function_decls (void);

/* Return the variable decl for a symbol.  */
tree g95_get_symbol_decl (g95_symbol *);

/* Advance along a TREE_CHAIN.  */
tree g95_advance_chain (tree, int);

/* Helper routine for constant folding.  */
tree g95_simple_fold(tree, tree *, tree *, tree *);

/* Generate the code for a function.  */
void g95_generate_function_code (g95_namespace *);

/* somewhere! */
tree pushdecl (tree);
void pushlevel (int);
tree poplevel (int, int, int);
void expand_function_body (tree);
tree getdecls(void);

/* External IO function decls.  */
struct g95_io_fndecl_t GTY(())
{
  char *name;
  tree *ptype;
  tree write;
  tree read;
};

typedef struct g95_io_fndecl_t g95_io_fndecl_t;

/* This must be consistent with g95_io_fndecls (see trans-io.c).  */
typedef enum
{
  GFORIO_FNDECL_INT4=0,
  GFORIO_FNDECL_INT8,
  GFORIO_FNDECL_REAL4,
  GFORIO_FNDECL_REAL8,
  GFORIO_FNDECL_COMPLEX4,
  GFORIO_FNDECL_COMPLEX8,
  /* We convert all logical values to kind=4 before passing to IO.  */
  GFORIO_FNDECL_LOGICAL,
  GFORIO_NUM_FNDECLS
} g95_io_fndecl_enum;

extern struct g95_io_fndecl_t g95_io_fndecls[GFORIO_NUM_FNDECLS];

/* Runtime library function decls.  */
extern GTY(()) tree g95_fndecl_push_context;
extern GTY(()) tree g95_fndecl_pop_context;
extern GTY(()) tree g95_fndecl_array_mismatch;
extern GTY(()) tree g95_fndecl_internal_malloc;
extern GTY(()) tree g95_fndecl_internal_malloc64;
extern GTY(()) tree g95_fndecl_internal_free;
extern GTY(()) tree g95_fndecl_allocate;
extern GTY(()) tree g95_fndecl_allocate64;
extern GTY(()) tree g95_fndecl_deallocate;
extern GTY(()) tree g95_fndecl_stop;

/* String functions.  */
extern GTY(()) tree g95_fndecl_copy_string;
extern GTY(()) tree g95_fndecl_compare_string;
extern GTY(()) tree g95_fndecl_concat_string;

/* IO library decls.  */
extern GTY(()) tree g95_fndecl_write_begin;
extern GTY(()) tree g95_fndecl_write_character;

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
};

/* String nodes only.  */
struct lang_decl GTY (())
{
  tree stringlength;
};

#define G95_DECL_STRING_LENGTH(node) (DECL_LANG_SPECIFIC(node)->stringlength)
#define G95_DECL_STRING(node) DECL_LANG_FLAG_0(node)
#define G95_DECL_PACKED_ARRAY(node) DECL_LANG_FLAG_1(node)
#define G95_DECL_PARTIAL_PACKED_ARRAY(node) DECL_LANG_FLAG_2(node)

#define G95_SIZE_STRING_TYPE_P(node) TYPE_LANG_FLAG_0(node)
#define G95_DESCRIPTOR_TYPE_P(node) TYPE_LANG_FLAG_1(node)
#define G95_TYPE_DESCRIPTOR_LBOUND(node, dim) \
  (TYPE_LANG_SPECIFIC(node)->lbound[dim])
#define G95_TYPE_DESCRIPTOR_UBOUND(node, dim) \
  (TYPE_LANG_SPECIFIC(node)->ubound[dim])
#define G95_TYPE_DESCRIPTOR_STRIDE(node, dim) \
  (TYPE_LANG_SPECIFIC(node)->stride[dim])
#define G95_TYPE_DESCRIPTOR_RANK(node) (TYPE_LANG_SPECIFIC(node)->rank)
#define G95_TYPE_DESCRIPTOR_SIZE(node) (TYPE_LANG_SPECIFIC(node)->size)

/* I changed this from sorry(...) because it should not return.  */
/* TODO: Removed g95_todo_error before releasing g95.  */
#define g95_todo_error(args...) fatal_error("g95_todo: Not Implemented: " args)

#endif /* G95_TRANS_H */
