/* Header for array handling functions
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

/* Generate code to free an array.  */
tree g95_array_deallocate (tree);

/* Generate code to initialise an allocate an array.  Statements are added to
   se, which should contain an expression for the array descriptor.  */
void g95_array_allocate (g95_se *, g95_ref *, tree);

/* Generate code to initialize an array descriptor.  if lower == NULL, the
   lower bound will be 1.  Type A arrays do not use poffset.  */
tree g95_array_init_size (tree, int, tree *,
    g95_expr **, g95_expr **, tree *, tree *);

/* Generate code for allocation and cleanup of local array variables.  */
tree g95_trans_auto_array_allocation (g95_symbol * sym);
/* Generate entry and exit code for local array variables.  */
tree g95_trans_dummy_array_bias (g95_symbol * sym, tree body);
/* Add initialisation for deferred arrays.  */
tree g95_trans_deferred_array (g95_symbol *, tree);
/* Generate scalarization information for an expression.  */
g95_ss * g95_walk_expr (g95_ss *, g95_expr *);

/* Add the pre and post chains from an SS chain to a loopinfo.  */
void g95_add_ss_stmts (g95_loopinfo *, g95_ss *);
/* Reverse a SS chain.  */
g95_ss * g95_reverse_ss (g95_ss *);

/* Calculates the lower bound and stride of array sections.  */
void g95_conv_ss_startstride (g95_loopinfo *);

void g95_init_loopinfo (g95_loopinfo *);
void g95_copy_loopinfo_to_se (g95_se *, g95_loopinfo *);

/* Generates the actual loops for a scalarized expression.  */
tree g95_trans_scalarizing_loops (g95_loopinfo *, tree);
/* Initialise the scalarization loop parameters.  */
void g95_conv_loopvars (g95_loopinfo *);

/* Translate an array reference.  */
void g95_conv_array_index_ref (g95_se *, tree, tree *, int);

/* Return either an INT_CST or a COMPONENT_REF. */
tree g95_conv_array_stride (tree, int);
tree g95_conv_array_lbound (tree, int);
tree g95_conv_array_ubound (tree, int);

/* The remaining space available for stack variables.  */
extern unsigned HOST_WIDE_INT g95_stack_space_left;
/* Returns true if a variable of specified size should go on the stack.  */
int g95_can_put_var_on_stack (tree);

