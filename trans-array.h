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

/* Generate code to allocate a temporary array.  */
tree g95_trans_allocate_temp_array (g95_loopinfo *, g95_ss_info *, tree, tree);

/* Generate code for allocation of compiler allocated array variables.  */
tree g95_trans_auto_array_allocation (tree, g95_symbol *);
/* Generate entry and exit code for dummy array parameters.  */
tree g95_trans_dummy_array_bias (g95_symbol *, tree, tree);
/* Add initialisation for deferred arrays.  */
tree g95_trans_deferred_array (g95_symbol *, tree);

/* Generate scalarization information for an expression.  */
g95_ss * g95_walk_expr (g95_ss *, g95_expr *);
/* Walk the arguments of an intrinsic function.  */
g95_ss * g95_walk_elemental_function_args (g95_ss *, g95_expr *, g95_ss_type);
/* Walk an intrinsic function.  */
g95_ss * g95_walk_intrinsic_function (g95_ss *, g95_expr *,
                                      g95_intrinsic_sym *);

/* Free the SS assocuated with a loop.  */
void g95_cleanup_loop (g95_loopinfo *);
/* Associate a SS chain with a loop.  */
void g95_add_ss_to_loop (g95_loopinfo *, g95_ss *);
/* Mark a SS chain as used in this loop.  */
void g95_mark_ss_chain_used (g95_ss *, unsigned);
/* Reverse a SS chain.  */
g95_ss * g95_reverse_ss (g95_ss *);

/* Calculates the lower bound and stride of array sections.  */
void g95_conv_ss_startstride (g95_loopinfo *);

void g95_init_loopinfo (g95_loopinfo *);
void g95_copy_loopinfo_to_se (g95_se *, g95_loopinfo *);

/* Marks the start of a scalarized expression, and declares loop variables.  */
void g95_start_scalarized_body (g95_loopinfo *);
/* Generates the actual loops for a scalarized expression.  */
void g95_trans_scalarizing_loops (g95_loopinfo *, tree, tree);
/* Mark the end of the main loop body and the start of the copying loop.  */
void g95_trans_scalarized_loop_boundary (g95_loopinfo *, tree, tree);
/* Initialise the scalarization loop parameters.  */
void g95_conv_loop_setup (g95_loopinfo *);
/* Resolve array assignment dependancies.  */
void g95_conv_resolve_dependencies (g95_loopinfo *, g95_ss *, g95_ss *);

/* Get a single array element.  */
void g95_conv_array_ref (g95_se *, g95_array_ref *);
/* Translate an array reference.  */
//void g95_conv_scalarized_array_ref (g95_se *, g95_array_ref *);
/* Translate a reference to a temporary array.  */
void g95_conv_tmp_array_ref (g95_se * se);
/* Translate a reference to an array temporary.  */
void g95_conv_tmp_ref (g95_se *);

/* Convert an array for passing as an actual function parameter.  */
void g95_conv_array_parameter (g95_se *, g95_expr *, g95_ss *);

/* Return either an INT_CST or a COMPONENT_REF. */
tree g95_conv_array_stride (tree, int);
tree g95_conv_array_lbound (tree, int);
tree g95_conv_array_ubound (tree, int);

/* The remaining space available for stack variables.  */
extern unsigned HOST_WIDE_INT g95_stack_space_left;
/* Returns true if a variable of specified size should go on the stack.  */
int g95_can_put_var_on_stack (tree);

/* Add pre-loop scalarization code for intrinsic functions which require
   special handling.  */
void g95_add_intrinsic_ss_code (g95_loopinfo *, g95_ss *);
