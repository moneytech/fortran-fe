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
typedef struct
{
  /* two chains of *_STMT trees */
  tree pre;
  tree pre_tail;
  tree post;
  tree post_tail;

  /* the result of the expression */
  tree expr;
} g95_se;

/* cll this to initialise a g95_se structure before use
 * first parameter is structure to initialise, second is
 * g95_se to get scalarization data from, or NULL */
void g95_init_se(g95_se *, g95_se *);

/* helpers for adding to stmt chains */
/* Add statements to a g95_se->pre chain.  */
#define g95_add_stmt_to_pre(se, h, t) \
            g95_add_stmt_to_list(&(se)->pre, &(se)->pre_tail, h, t)
/* Add statements to a g95_se->post chain.  */
#define g95_add_stmt_to_post(se, h, t) \
            g95_add_stmt_to_list(&(se)->post, &(se)->post_tail, h, t)

/* like chainon() but updates both head and tail.
Used by g95_add_stmt_to_(pre|post).  */
void g95_add_stmt_to_list(tree *, tree *, tree, tree);

/* create a temporary variable
 * should not be used for array temporaries */
tree g95_create_tmp_var(tree);

/* store the result of an expression on a temp variable so it can be used
 * repeatedly even if the original changes */
void g95_make_tmp_expr(g95_se * se);

/* Only the following 4 functions should be used to translate
 * expressions outside of trans-expr.c */

/* Suitable for array indices and function parameters
 * ie. either a constant of a variable.
 * Guaranteed to return post=NULL*/
void g95_conv_simple_val(g95_se *, g95_expr *);
/* Suitable for use in if constructs, etc
 * Guaranteed to return post=NULL*/
void g95_conv_simple_cond(g95_se *, g95_expr *);
/* Weturns an lvalue, throws error if not possble.
 * Guaranteed to return post=NULL*/
void g95_conv_simple_lhs(g95_se *, g95_expr *);

/* Simple expression suitable for RHS of assignment.
 * Can return a non-NULL post tree */
void g95_conv_simple_rhs(g95_se *, g95_expr *);

/* g95_trans_* shouldn't call push/poplevel, use g95_push/pop_scope */

/* Start a new satement.  */
void g95_start_stmt(void);
/* Finish a statement.  The two paramenters are the head and tail of the
code for the statement.  Returns a COMPOUNT_STMT containing the code, decls
tor temporaries and scope neatly wrapped up in a single COMPOUNT_STMT.  */
tree g95_finish_stmt(tree, tree);

/* tarns-types */
tree g95_sym_type (g95_symbol *);
tree g95_typenode_for_spec (g95_typespec *);

/* Return the backend label decl.  */
tree g95_get_label_decl(g95_st_label *);

/* Return the decl for a function.  */
tree g95_get_function_decl (g95_symbol *);

/* Creates an label.  Decl is artificial if label_id == NULL_TREE.  */
tree g95_build_label_decl(tree);

/* Return the decl used to hold the function return value.
   Do not use if the function has an explicit result variable.  */
tree g95_get_fake_result_decl(void);

/* Get the return label for the current function.  */
tree g95_get_return_label (void);

/* somewhere! */
tree pushdecl (tree);
void pushlevel (int);
tree poplevel (int, int, int);
void expand_function_body (tree);
tree getdecls(void);

#define g95_todo_error sorry

#endif /* G95_TRANS_H */
