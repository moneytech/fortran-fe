/* G95 Backend interface
   Copyright (C) 2000, 2001, 2002 Free Software Foundation, Inc.
   Contributed by Paul Brook.

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

/* f95-lang.c-- GCC backend interface stuff */

/* declare required prototypes: */

#include "config.h"
#include "system.h"
#include "tree.h"
#include "tree-simple.h"
#include "output.h"
#include <stdio.h>
#include "debug.h"
#include "c-common.h"
#include "rtl.h"
#include "tm_p.h"
#include "ggc.h"
#include "toplev.h"
#include "langhooks.h"
#include "langhooks-def.h"
#include "function.h"
#include "expr.h"
#include "errors.h"
#include "timevar.h"
#include "flags.h"
#include "target.h"
#include "tree-optimize.h"
#include <assert.h>
#define BACKEND_CODE
#include "g95.h"
#include "trans.h"
#include "trans-types.h"
#include "trans-const.h"
#include "g95-support.h"

/* Language-dependent contents of an identifier.  */

/* The limbo_value is used for block level extern declarations, which need
   to be type checked against subsequent extern declarations.  They can't
   be referenced after they fall out of scope, so they can't be global.

   The rid_code field is used for keywords.  It is in all
   lang_identifier nodes, because some keywords are only special in a
   particular context.  */

struct lang_identifier
GTY (())
{
  struct tree_identifier common;
};

/* The resulting tree type.  */

union lang_tree_node
GTY ((desc ("TREE_CODE (&%h.generic) == IDENTIFIER_NODE")))
{
  union tree_node GTY ((tag ("0"),
			desc ("tree_node_structure (&%h)"))) generic;
  struct lang_identifier GTY ((tag ("1"))) identifier;
};

/* Save and restore the variables in this file and elsewhere
   that keep track of the progress of compilation of the current function.
   Used for nested functions.  */

struct language_function
GTY (())
{
  /* struct g95_language_function base; */
  tree named_labels;
  tree shadowed_labels;
  int returns_value;
  int returns_abnormally;
  int warn_about_return_type;
  int extern_inline;
  struct binding_level *binding_level;
};

/* We don't have a lex/yacc lexer/parser, but toplev expects these to
   exist anyway.  */
void yyerror (const char *str);
int yylex (void);

static void g95_init_decl_processing (void);
static void g95_init_builtin_functions (void);

/* Each front end provides its own.  */
static const char *g95_init (const char *);
static void g95_finish (void);
static void g95_print_identifier (FILE *, tree, int);
void do_function_end (void);
int global_bindings_p (void);
void insert_block (tree);
void set_block (tree);
tree g95_truthvalue_conversion (tree);
static void g95_be_parse_file (void *);

#undef LANG_HOOKS_NAME
#undef LANG_HOOKS_INIT
#undef LANG_HOOKS_FINISH
#undef LANG_HOOKS_INIT_OPTIONS
#undef LANG_HOOKS_DECODE_OPTION
#undef LANG_HOOKS_PRINT_IDENTIFIER
#undef LANG_HOOKS_PARSE_FILE
#undef LANG_HOOKS_TRUTHVALUE_CONVERSION
#undef LANG_HOOKS_MARK_ADDRESSABLE
#undef LANG_HOOKS_TYPE_FOR_MODE
#undef LANG_HOOKS_TYPE_FOR_SIZE
#undef LANG_HOOKS_UNSIGNED_TYPE
#undef LANG_HOOKS_SIGNED_TYPE
#undef LANG_HOOKS_SIGNED_OR_UNSIGNED_TYPE

/* Define lang hooks.  */
#define LANG_HOOKS_NAME                 "GNU F95"
#define LANG_HOOKS_INIT                 g95_init
#define LANG_HOOKS_FINISH               g95_finish
#define LANG_HOOKS_INIT_OPTIONS         g95_init_options
#define LANG_HOOKS_DECODE_OPTION        g95_parse_arg
#define LANG_HOOKS_PRINT_IDENTIFIER     g95_print_identifier
#define LANG_HOOKS_PARSE_FILE           g95_be_parse_file
#define LANG_HOOKS_TRUTHVALUE_CONVERSION   g95_truthvalue_conversion
#define LANG_HOOKS_MARK_ADDRESSABLE        g95_mark_addressable
#define LANG_HOOKS_TYPE_FOR_MODE           g95_type_for_mode
#define LANG_HOOKS_TYPE_FOR_SIZE           g95_type_for_size
#define LANG_HOOKS_UNSIGNED_TYPE           g95_unsigned_type
#define LANG_HOOKS_SIGNED_TYPE             g95_signed_type
#define LANG_HOOKS_SIGNED_OR_UNSIGNED_TYPE g95_signed_or_unsigned_type

const struct lang_hooks lang_hooks = LANG_HOOKS_INITIALIZER;

/* A list (chain of TREE_LIST nodes) of all LABEL_DECLs in the function
   that have names.  Here so we can clear out their names' definitions
   at the end of the function.  */

/* Tree code classes.  */

#define DEFTREECODE(SYM, NAME, TYPE, LENGTH) TYPE,

const char tree_code_type[] = {
#include "tree.def"
  'x',
#include "c-common.def"		/* Should disappear once SIMPLE is part of tree.h  */
};
#undef DEFTREECODE

/* Table indexed by tree code giving number of expression
   operands beyond the fixed part of the node structure.
   Not used for types or decls.  */

#define DEFTREECODE(SYM, NAME, TYPE, LENGTH) LENGTH,

const unsigned char tree_code_length[] = {
#include "tree.def"
  0,
#include "c-common.def"
};
#undef DEFTREECODE

/* Names of tree components.
   Used for printing out the tree and error messages.  */
#define DEFTREECODE(SYM, NAME, TYPE, LEN) NAME,

const char *const tree_code_name[] = {
#include "tree.def"
  "@@dummy",
#include "c-common.def"
};
#undef DEFTREECODE

static tree named_labels;

#define NULL_BINDING_LEVEL (struct binding_level *) NULL

/* A chain of binding_level structures awaiting reuse.  */

static GTY(()) struct binding_level *free_binding_level;

/* The elements of `ridpointers' are identifier nodes
   for the reserved type names and storage classes.
   It is indexed by a RID_... value.  */
tree *ridpointers = NULL;

/* language-specific flags.  */

/* static tree shadowed_labels=NULL; */

tree listify (tree chain);

static struct stmt_tree_s g95_stmt_tree;

static GTY (()) tree g95_scope_stmt_stack;

/* Check that a stmt is SIMPLE. Used to be part of tree-simple.c but then
   disappeared.  We generate SIMPLE trees anyway, these are just used to
   double-check.  */
int
is_simple_decl_stmt (tree stmt)
{
  tree decl = DECL_STMT_DECL (stmt);
  tree init = DECL_INITIAL (decl);

  if (!is_simple_val (DECL_SIZE_UNIT (decl)))
    return 0;

  /* Plain decls are simple.  */
  if (init == NULL_TREE || init == error_mark_node)
    return 1;

  /* Don't mess with a compile-time initializer.  */
  if (TREE_STATIC (decl))
    return 1;

  return 0;
}

static int is_simple_stmt (tree t);

static int
is_simple_compstmt (tree t)
{
  if (t == NULL_TREE)
    return 1;

  /* Look for '{'.  */
  if (TREE_CODE (t) != SCOPE_STMT
      || !SCOPE_BEGIN_P (t))
    return 0;

  /* Test all the statements in the body.  */
  for (t = TREE_CHAIN (t);
       t && !(TREE_CODE (t) == SCOPE_STMT && SCOPE_END_P (t));
       t = TREE_CHAIN (t))
    if (!is_simple_stmt (t))
      return 0;

  /* Look for '}'.  */
  if (t
      && (TREE_CODE (t) != SCOPE_STMT
	  || !SCOPE_END_P (t)))
    return 0;

  return 1;
}


static int
is_simple_stmt (tree t)
{
  if (t == NULL_TREE)
    return 1;

  switch (TREE_CODE (t))
    {
    case COMPOUND_STMT:
      return is_simple_compstmt (COMPOUND_BODY (t));

    case SCOPE_STMT:
      return is_simple_compstmt (t);

    case EXPR_STMT:
      return is_simple_expr (EXPR_STMT_EXPR (t));

    case IF_STMT:
      return (is_simple_condexpr (IF_COND (t))
	      && is_simple_stmt (THEN_CLAUSE (t))
	      && is_simple_stmt (ELSE_CLAUSE (t)));

    case WHILE_STMT:
      return (is_simple_condexpr (WHILE_COND (t))
	      && is_simple_stmt (WHILE_BODY (t)));

    case DO_STMT:
      return (is_simple_condexpr (DO_COND (t))
	      && is_simple_stmt (DO_BODY (t)));

    case FOR_STMT:
      {
	int s1, s2, s3, s4;

	if (TREE_CODE (FOR_INIT_STMT (t)) == DECL_STMT)
	  s1 = 0;
	else
	  s1 = is_simple_exprseq (EXPR_STMT_EXPR (FOR_INIT_STMT (t)));

	s2 = is_simple_condexpr (FOR_COND (t));
	s3 = is_simple_exprseq (FOR_EXPR (t));
	s4 = is_simple_stmt (FOR_BODY (t));

	return (s1 && s2 && s3 && s4);
      }

    /* Note that we can assume that we don't need to special case the body
       of the switch() statement.  If we got to this stage, we can assume
       that the switch() is properly formed (i.e., it will be a compound
       statement containing all the case labels).  */
    case SWITCH_STMT:
      return (is_simple_val (SWITCH_COND (t))
	      && is_simple_stmt (SWITCH_BODY (t)));

    case FILE_STMT:
    case LABEL_STMT:
    case GOTO_STMT:
    case ASM_STMT:
    case CASE_LABEL:
    case CONTINUE_STMT:
    case BREAK_STMT:
      return 1;

    case RETURN_STMT:
      {
	tree type = TREE_TYPE (TREE_TYPE (current_function_decl));
	if (TREE_CODE (type) != VOID_TYPE
	    && RETURN_STMT_EXPR (t))
	  return is_simple_rhs (TREE_OPERAND (RETURN_STMT_EXPR (t), 1));
	else
	  return 1;
      }

    case DECL_STMT:
      return is_simple_decl_stmt (t);

    default:
      return 0;
    }
}

void
expand_function_body (tree fndecl, int nested)
{
  timevar_push (TV_EXPAND);

  if (nested)
    {
      push_function_context ();
      current_function_decl = fndecl;
    }

  init_function_start (fndecl, input_filename, 1);

  cfun->x_whole_function_mode_p = 1;

  lineno = 1;
  /* Even though we're inside a function body, we still don't want
     to call expand_expr to calculate the size of a variable-sized
     array.  We haven't necessarily assigned RTL to all variables
     yet, so it's not safe to try to expand expressions involving
     them.  */

  immediate_size_expand = 0;
  cfun->x_dont_save_pending_sizes_p = 1;

  /* The code _should_ already be in SIMPLE form.  If not then something has
     gone wrong.*/
  if (is_simple_stmt (DECL_SAVED_TREE (fndecl)))
    {
      if (flag_tree_ssa)
        optimize_function_tree (fndecl);
      if (! is_simple_stmt (DECL_SAVED_TREE (fndecl)))
        warning ("Function tree not simple after optimization");
    }
  else
    warning ("Internal failure: Function is not SIMPLE. Optimization inhibited.");

  /* create RTL for startup code of function, such as saving registers */
  expand_function_start (fndecl, 0);

  expand_stmt (DECL_SAVED_TREE (fndecl));

  immediate_size_expand = 1;

  expand_function_end (input_filename, lineno, 0);

  if (nested)
    ggc_push_context ();

  rest_of_compilation (fndecl);

  if (DECL_STATIC_CONSTRUCTOR (fndecl))
    {
      if (targetm.have_ctors_dtors)
        {

          (* targetm.asm_out.constructor) (XEXP (DECL_RTL (fndecl), 0),
                                           DEFAULT_INIT_PRIORITY);
        }
      else
        g95_static_ctors = g95_chainon_list (g95_static_ctors, fndecl);
    }
  if (nested)
    ggc_pop_context ();

  if (nested)
    pop_function_context ();

  timevar_pop (TV_EXPAND);
}


/* Prepare expr to be an argument of a TRUTH_NOT_EXPR,
   or validate its data type for an `if' or `while' statement or ?..: exp.

   This preparation consists of taking the ordinary
   representation of an expression expr and producing a valid tree
   boolean expression describing whether expr is nonzero.  We could
   simply always do build_binary_op (NE_EXPR, expr, boolean_false_node, 1),
   but we optimize comparisons, &&, ||, and !.

   The resulting type should always be `boolean_type_node'.  */
/*TODO: fix truthvalue_conversion.  */

tree
g95_truthvalue_conversion (tree expr ATTRIBUTE_UNUSED)
{
  g95_todo_error ("Truthvalue_conversion called");
  /*return (expr); */
}

int
anon_aggr_type_p (tree node ATTRIBUTE_UNUSED)
{
  return 0;
}

int
stmts_are_full_exprs_p (void)
{
  return 0;
}

stmt_tree
current_stmt_tree (void)
{
  return &g95_stmt_tree;
}


tree *
current_scope_stmt_stack (void)
{
  return &g95_scope_stmt_stack;
}

static void
g95_be_parse_file (void *set_yydebug ATTRIBUTE_UNUSED)
{
  g95_parse_file ();
  g95_generate_constructors ();
}

/* Routines Expected by GCC:  */

static void
f95_expand_decl_stmt (tree t)
{
  tree decl;

  decl = DECL_STMT_DECL (t);

  /* Expand nexted functions.  */
  if (TREE_CODE (decl) == FUNCTION_DECL
      && DECL_CONTEXT (decl) == current_function_decl
      && DECL_SAVED_TREE (decl))
    expand_function_body (decl, 1);

}

const char *
g95_init (const char *filename)
{
  if (! filename
      || strcmp (filename, "-") == 0)
    {
      filename = "";
    }

  lang_expand_decl_stmt = f95_expand_decl_stmt;

  g95_option.source = (char *) filename;
  g95_init_1 ();

  g95_init_decl_processing ();

  /* GCC builtins.  */
  g95_init_builtin_functions ();

  /* Runtime/IO library functions.  */
  g95_build_builtin_function_decls ();

  g95_init_constants ();

  if (g95_new_file (g95_option.source, g95_option.form) != SUCCESS)
    return (NULL);

  g95_static_ctors = NULL_TREE;

  return filename;
}

void
g95_finish (void)
{
  g95_done_1 ();
  return;
}

static void
g95_print_identifier (FILE * file ATTRIBUTE_UNUSED,
		      tree t ATTRIBUTE_UNUSED, int i ATTRIBUTE_UNUSED)
{
  return;
}


/* These functions and variables deal with binding contours.  We only
   need these functions for the list of PARM_DECLs, but we leave the
   functions more general; these are a simplified version of the
   functions from GNAT.  */

/* For each binding contour we allocate a binding_level structure which records
   the entities defined or declared in that contour. Contours include:

        the global one
        one for each subprogram definition
        one for each compound statement (declare block)

   Binding contours are used to create GCC tree BLOCK nodes.  */

struct binding_level
GTY (())
{
  /* A chain of ..._DECL nodes for all variables, constants, functions,
     parameters and type declarations.  These ..._DECL nodes are chained
     through the TREE_CHAIN field. Note that these ..._DECL nodes are stored
     in the reverse of the order supplied to be compatible with the
     back-end.  */
  tree names;
  /* For each level (except the global one), a chain of BLOCK nodes for all
     the levels that were entered and exited one level down from this one.  */
  tree blocks;
  /* The back end may need, for its own internal processing, to create a BLOCK
     node. This field is set aside for this purpose. If this field is non-null
     when the level is popped, i.e. when poplevel is invoked, we will use such
     block instead of creating a new one from the 'names' field, that is the
     ..._DECL nodes accumulated so far.  Typically the routine 'pushlevel'
     will be called before setting this field, so that if the front-end had
     inserted ..._DECL nodes in the current block they will not be lost.   */
  tree block_created_by_back_end;
  /* The binding level containing this one (the enclosing binding level). */
  struct binding_level *level_chain;
};

/* The binding level currently in effect.  */
static GTY(()) struct binding_level *current_binding_level = NULL;

/* The outermost binding level. This binding level is created when the
   compiler is started and it will exist through the entire compilation.  */
static GTY(()) struct binding_level *global_binding_level;

/* Binding level structures are initialized by copying this one.  */
static struct binding_level clear_binding_level = { NULL, NULL, NULL, NULL };

/* Return non-zero if we are currently in the global binding level.  */

int
global_bindings_p (void)
{
  return current_binding_level == global_binding_level ? -1 : 0;
}

tree
getdecls (void)
{
  return current_binding_level->names;
}

/* Enter a new binding level. The input parameter is ignored, but has to be
   specified for back-end compatibility.  */

void
pushlevel (int ignore ATTRIBUTE_UNUSED)
{
  struct binding_level *newlevel
    = (struct binding_level *) ggc_alloc (sizeof (struct binding_level));

  *newlevel = clear_binding_level;

  /* Add this level to the front of the chain (stack) of levels that are
     active.  */
  newlevel->level_chain = current_binding_level;
  current_binding_level = newlevel;
}

/* Exit a binding level.
   Pop the level off, and restore the state of the identifier-decl mappings
   that were in effect when this level was entered.

   If KEEP is nonzero, this level had explicit declarations, so
   and create a "block" (a BLOCK node) for the level
   to record its declarations and subblocks for symbol table output.

   If FUNCTIONBODY is nonzero, this level is the body of a function,
   so create a block as if KEEP were set and also clear out all
   label names.

   If REVERSE is nonzero, reverse the order of decls before putting
   them into the BLOCK.  */

tree
poplevel (int keep, int reverse, int functionbody)
{
  /* Points to a BLOCK tree node. This is the BLOCK node construted for the
     binding level that we are about to exit and which is returned by this
     routine.  */
  tree block_node = NULL_TREE;
  tree decl_chain;
  tree subblock_chain = current_binding_level->blocks;
  tree subblock_node;
  tree block_created_by_back_end;

  /* Reverse the list of XXXX_DECL nodes if desired.  Note that the ..._DECL
     nodes chained through the `names' field of current_binding_level are in
     reverse order except for PARM_DECL node, which are explicitely stored in
     the right order.  */
  decl_chain = (reverse) ? nreverse (current_binding_level->names)
    : current_binding_level->names;

  block_created_by_back_end =
    current_binding_level->block_created_by_back_end;
  if (block_created_by_back_end != 0)
    {
      block_node = block_created_by_back_end;

      /* Check if we are about to discard some information that was gathered
         by the front-end. Nameley check if the back-end created a new block
         without calling pushlevel first. To understand why things are lost
         just look at the next case (i.e. no block created by back-end.  */
      if ((keep || functionbody) && (decl_chain || subblock_chain))
	abort ();
    }

  /* If there were any declarations in the current binding level, or if this
     binding level is a function body, or if there are any nested blocks then
     create a BLOCK node to record them for the life of this function.  */
  else if (keep || functionbody)
    block_node = build_block (keep ? decl_chain : 0, 0, subblock_chain, 0, 0);

  /* Record the BLOCK node just built as the subblock its enclosing scope.  */
  for (subblock_node = subblock_chain; subblock_node;
       subblock_node = TREE_CHAIN (subblock_node))
    BLOCK_SUPERCONTEXT (subblock_node) = block_node;

  /* Clear out the meanings of the local variables of this level.  */

  for (subblock_node = decl_chain; subblock_node;
       subblock_node = TREE_CHAIN (subblock_node))
    if (DECL_NAME (subblock_node) != 0)
      /* If the identifier was used or addressed via a local extern decl,
         don't forget that fact.   */
      if (DECL_EXTERNAL (subblock_node))
	{
	  if (TREE_USED (subblock_node))
	    TREE_USED (DECL_NAME (subblock_node)) = 1;
	  if (TREE_ADDRESSABLE (subblock_node))
	    TREE_ADDRESSABLE (DECL_ASSEMBLER_NAME (subblock_node)) = 1;
	}

  /* Pop the current level.  */
  current_binding_level = current_binding_level->level_chain;

  if (functionbody)
    {
      /* This is the top level block of a function. The ..._DECL chain stored
         in BLOCK_VARS are the function's parameters (PARM_DECL nodes). Don't
         leave them in the BLOCK because they are found in the FUNCTION_DECL
         instead.  */
      DECL_INITIAL (current_function_decl) = block_node;
      BLOCK_VARS (block_node) = 0;
    }
  else if (block_node)
    {
      if (block_created_by_back_end == NULL)
	current_binding_level->blocks
	  = chainon (current_binding_level->blocks, block_node);
    }

  /* If we did not make a block for the level just exited, any blocks made for
     inner levels (since they cannot be recorded as subblocks in that level)
     must be carried forward so they will later become subblocks of something
     else.  */
  else if (subblock_chain)
    current_binding_level->blocks
      = chainon (current_binding_level->blocks, subblock_chain);
  if (block_node)
    TREE_USED (block_node) = 1;

  return block_node;
}

/* Insert BLOCK at the end of the list of subblocks of the
   current binding level.  This is used when a BIND_EXPR is expanded,
   to handle the BLOCK node inside the BIND_EXPR.  */

void
insert_block (tree block)
{
  TREE_USED (block) = 1;
  current_binding_level->blocks
    = chainon (current_binding_level->blocks, block);
}

/* Set the BLOCK node for the innermost scope
   (the one we are currently in).  */

void
set_block (tree block)
{
  current_binding_level->block_created_by_back_end = block;
}

/* Records a ..._DECL node DECL as belonging to the current lexical scope.
   Returns the ..._DECL node. */

tree
pushdecl (tree decl)
{
  /* External objects aren't nested, other objects may be.  */
  if ((DECL_EXTERNAL (decl)) || (decl == current_function_decl))
    DECL_CONTEXT (decl) = 0;
  else
    DECL_CONTEXT (decl) = current_function_decl;

  /* Put the declaration on the list.  The list of declarations is in reverse
     order. The list will be reversed later if necessary.  This needs to be
     this way for compatibility with the back-end.  */

  TREE_CHAIN (decl) = current_binding_level->names;
  current_binding_level->names = decl;

  /* For the declartion of a type, set its name if it is not already set. */

  if (TREE_CODE (decl) == TYPE_DECL && TYPE_NAME (TREE_TYPE (decl)) == 0)
    TYPE_NAME (TREE_TYPE (decl)) = DECL_NAME (decl);

  return decl;
}

#ifndef CHAR_TYPE_SIZE
#define CHAR_TYPE_SIZE BITS_PER_UNIT
#endif

#ifndef INT_TYPE_SIZE
#define INT_TYPE_SIZE BITS_PER_WORD
#endif

#undef SIZE_TYPE
#define SIZE_TYPE "long unsigned int"

/* Create tree nodes for the basic scalar types of Fortran 95,
   and some nodes representing standard constants (0, 1, (void *) 0).
   Initialize the global binding level.
   Make definitions for built-in primitive functions.  */
/* G95_TODO */
static void
g95_init_decl_processing (void)
{
  current_function_decl = NULL;
  named_labels = NULL;
  current_binding_level = NULL_BINDING_LEVEL;
  free_binding_level = NULL_BINDING_LEVEL;

  /* Make the binding_level structure for global names. We move all
     variables that are in a COMMON block to this binding level.  */
  pushlevel (0);
  global_binding_level = current_binding_level;

  /* Build common tree nodes. char_type_node is unsigned because we
     only use it for actual characters, not for INTEGER(1). Also, we
     want double_type_node to actually have double precision.   */
  build_common_tree_nodes (0);
  set_sizetype (long_unsigned_type_node);
  build_common_tree_nodes_2 (0);

  /* Set up F95 type nodes.  */
  g95_init_types ();

  g95_init_c_decl_hacks ();
}

/* Mark EXP saying that we need to be able to take the
   address of it; it should not be allocated in a register.
   In Fortran 95 this is only the case for variables with
   the TARGET attribute, but we implement it here for a
   likely future Cray pointer extension.
   Value is 1 if successful.  */
/* G95_TODO */
int
g95_mark_addressable (tree exp)
{
  register tree x = exp;
  while (1)
    switch (TREE_CODE (x))
      {
      case COMPONENT_REF:
	if (DECL_C_BIT_FIELD (TREE_OPERAND (x, 1)))
	  {
	    error ("cannot take address of bitfield `%s'",
		   IDENTIFIER_POINTER (DECL_NAME (TREE_OPERAND (x, 1))));
	    return 0;
	  }

	/* ... fall through ...  */

      case ADDR_EXPR:
      case ARRAY_REF:
      case REALPART_EXPR:
      case IMAGPART_EXPR:
	x = TREE_OPERAND (x, 0);
	break;

      case CONSTRUCTOR:
	TREE_ADDRESSABLE (x) = 1;
	return 1;

      case VAR_DECL:
      case CONST_DECL:
      case PARM_DECL:
      case RESULT_DECL:
	if (DECL_REGISTER (x) && !TREE_ADDRESSABLE (x) && DECL_NONLOCAL (x))
	  {
	    if (TREE_PUBLIC (x))
	      {
		error
		  ("global register variable `%s' used in nested function",
		   IDENTIFIER_POINTER (DECL_NAME (x)));
		return 0;
	      }
	    pedwarn ("register variable `%s' used in nested function",
		     IDENTIFIER_POINTER (DECL_NAME (x)));
	  }
	else if (DECL_REGISTER (x) && !TREE_ADDRESSABLE (x))
	  {
	    if (TREE_PUBLIC (x))
	      {
		error ("address of global register variable `%s' requested",
		       IDENTIFIER_POINTER (DECL_NAME (x)));
		return 0;
	      }

#if 0
	    /* If we are making this addressable due to its having
	       volatile components, give a different error message.  Also
	       handle the case of an unnamed parameter by not trying
	       to give the name.  */

	    else if (C_TYPE_FIELDS_VOLATILE (TREE_TYPE (x)))
	      {
		error ("cannot put object with volatile field into register");
		return 0;
	      }
#endif

	    pedwarn ("address of register variable `%s' requested",
		     IDENTIFIER_POINTER (DECL_NAME (x)));
	  }
	put_var_into_stack (x);

	/* drops in */
      case FUNCTION_DECL:
	TREE_ADDRESSABLE (x) = 1;

      default:
	return 1;
      }
}

/* press the big red button - garbage (ggc) collection is on */

int ggc_p = 1;

/* Builtin function initialisation.  */

/* Return a definition for a builtin function named NAME and whose data type
   is TYPE.  TYPE should be a function type with argument types.
   FUNCTION_CODE tells later passes how to compile calls to this function.
   See tree.h for its possible values.

   If LIBRARY_NAME is nonzero, use that for DECL_ASSEMBLER_NAME,
   the name to be called if we can't opencode the function.  If
   ATTRS is nonzero, use that for the function's attribute list.  */

tree
builtin_function (const char *name,
		  tree type,
		  int function_code,
		  enum built_in_class class,
                  const char *library_name,
                  tree attrs ATTRIBUTE_UNUSED)
{
  tree decl = build_decl (FUNCTION_DECL, get_identifier (name), type);
  DECL_EXTERNAL (decl) = 1;
  TREE_PUBLIC (decl) = 1;
  if (library_name)
    SET_DECL_ASSEMBLER_NAME (decl, get_identifier (library_name));
  make_decl_rtl (decl, NULL);
  pushdecl (decl);
  DECL_BUILT_IN_CLASS (decl) = class;
  DECL_FUNCTION_CODE (decl) = function_code;

#if 0
  if (attrs)
    decl_attributes (&decl, attrs, ATTR_FLAG_BUILT_IN);
  else
    decl_attributes (&decl, NULL_TREE, 0);
#endif
  return decl;
}

#define flag_isoc99 0
enum built_in_attribute
{
#define DEF_ATTR_NULL_TREE(ENUM) ENUM,
#define DEF_ATTR_INT(ENUM, VALUE) ENUM,
#define DEF_ATTR_IDENT(ENUM, STRING) ENUM,
#define DEF_ATTR_TREE_LIST(ENUM, PURPOSE, VALUE, CHAIN) ENUM,
#define DEF_FN_ATTR(NAME, ATTRS, PREDICATE) /* No entry needed in enum.  */
#include "builtin-attrs.def"
#undef DEF_ATTR_NULL_TREE
#undef DEF_ATTR_INT
#undef DEF_ATTR_IDENT
#undef DEF_ATTR_TREE_LIST
#undef DEF_FN_ATTR
  ATTR_LAST
};

/* Initialisation of builtin function nodes.  Copied from c-common.c.  */
static void
g95_init_builtin_functions (void)
{
  enum builtin_type
  {
#define DEF_PRIMITIVE_TYPE(NAME, VALUE) NAME,
#define DEF_FUNCTION_TYPE_0(NAME, RETURN) NAME,
#define DEF_FUNCTION_TYPE_1(NAME, RETURN, ARG1) NAME,
#define DEF_FUNCTION_TYPE_2(NAME, RETURN, ARG1, ARG2) NAME,
#define DEF_FUNCTION_TYPE_3(NAME, RETURN, ARG1, ARG2, ARG3) NAME,
#define DEF_FUNCTION_TYPE_4(NAME, RETURN, ARG1, ARG2, ARG3, ARG4) NAME,
#define DEF_FUNCTION_TYPE_VAR_0(NAME, RETURN) NAME,
#define DEF_FUNCTION_TYPE_VAR_1(NAME, RETURN, ARG1) NAME,
#define DEF_FUNCTION_TYPE_VAR_2(NAME, RETURN, ARG1, ARG2) NAME,
#define DEF_POINTER_TYPE(NAME, TYPE) NAME,
#include "builtin-types.def"
#undef DEF_PRIMITIVE_TYPE
#undef DEF_FUNCTION_TYPE_0
#undef DEF_FUNCTION_TYPE_1
#undef DEF_FUNCTION_TYPE_2
#undef DEF_FUNCTION_TYPE_3
#undef DEF_FUNCTION_TYPE_4
#undef DEF_FUNCTION_TYPE_VAR_0
#undef DEF_FUNCTION_TYPE_VAR_1
#undef DEF_FUNCTION_TYPE_VAR_2
#undef DEF_POINTER_TYPE
    BT_LAST
  };

  typedef enum builtin_type builtin_type;

  tree builtin_types[(int) BT_LAST];
  tree va_list_ref_type_node;
  tree va_list_arg_type_node;

  if (TREE_CODE (va_list_type_node) == ARRAY_TYPE)
    {
      va_list_arg_type_node = va_list_ref_type_node =
	build_pointer_type (TREE_TYPE (va_list_type_node));
    }
  else
    {
      va_list_arg_type_node = va_list_type_node;
      va_list_ref_type_node = build_reference_type (va_list_type_node);
    }

#define DEF_PRIMITIVE_TYPE(ENUM, VALUE) \
  builtin_types[(int) ENUM] = VALUE;
#define DEF_FUNCTION_TYPE_0(ENUM, RETURN)		\
  builtin_types[(int) ENUM]				\
    = build_function_type (builtin_types[(int) RETURN],	\
			   void_list_node);
#define DEF_FUNCTION_TYPE_1(ENUM, RETURN, ARG1)				\
  builtin_types[(int) ENUM]						\
    = build_function_type (builtin_types[(int) RETURN],			\
			   tree_cons (NULL_TREE,			\
				      builtin_types[(int) ARG1],	\
				      void_list_node));
#define DEF_FUNCTION_TYPE_2(ENUM, RETURN, ARG1, ARG2)	\
  builtin_types[(int) ENUM]				\
    = build_function_type 				\
      (builtin_types[(int) RETURN],			\
       tree_cons (NULL_TREE,				\
		  builtin_types[(int) ARG1],		\
		  tree_cons (NULL_TREE,			\
			     builtin_types[(int) ARG2],	\
			     void_list_node)));
#define DEF_FUNCTION_TYPE_3(ENUM, RETURN, ARG1, ARG2, ARG3)		 \
  builtin_types[(int) ENUM]						 \
    = build_function_type						 \
      (builtin_types[(int) RETURN],					 \
       tree_cons (NULL_TREE,						 \
		  builtin_types[(int) ARG1],				 \
		  tree_cons (NULL_TREE,					 \
			     builtin_types[(int) ARG2],			 \
			     tree_cons (NULL_TREE,			 \
					builtin_types[(int) ARG3],	 \
					void_list_node))));
#define DEF_FUNCTION_TYPE_4(ENUM, RETURN, ARG1, ARG2, ARG3, ARG4)	\
  builtin_types[(int) ENUM]						\
    = build_function_type						\
      (builtin_types[(int) RETURN],					\
       tree_cons (NULL_TREE,						\
		  builtin_types[(int) ARG1],				\
		  tree_cons (NULL_TREE,					\
			     builtin_types[(int) ARG2],			\
			     tree_cons 					\
			     (NULL_TREE,				\
			      builtin_types[(int) ARG3],	 	\
			      tree_cons (NULL_TREE,			\
					 builtin_types[(int) ARG4],	\
					 void_list_node)))));
#define DEF_FUNCTION_TYPE_VAR_0(ENUM, RETURN)				\
  builtin_types[(int) ENUM]						\
    = build_function_type (builtin_types[(int) RETURN], NULL_TREE);
#define DEF_FUNCTION_TYPE_VAR_1(ENUM, RETURN, ARG1)			 \
   builtin_types[(int) ENUM]						 \
    = build_function_type (builtin_types[(int) RETURN], 		 \
			   tree_cons (NULL_TREE,			 \
				      builtin_types[(int) ARG1],	 \
				      NULL_TREE));

#define DEF_FUNCTION_TYPE_VAR_2(ENUM, RETURN, ARG1, ARG2)	\
   builtin_types[(int) ENUM]					\
    = build_function_type 					\
      (builtin_types[(int) RETURN],				\
       tree_cons (NULL_TREE,					\
		  builtin_types[(int) ARG1],			\
		  tree_cons (NULL_TREE,				\
			     builtin_types[(int) ARG2],		\
			     NULL_TREE)));
#define DEF_POINTER_TYPE(ENUM, TYPE)			\
  builtin_types[(int) ENUM]				\
    = build_pointer_type (builtin_types[(int) TYPE]);
#include "builtin-types.def"
#undef DEF_PRIMITIVE_TYPE
#undef DEF_FUNCTION_TYPE_1
#undef DEF_FUNCTION_TYPE_2
#undef DEF_FUNCTION_TYPE_3
#undef DEF_FUNCTION_TYPE_4
#undef DEF_FUNCTION_TYPE_VAR_0
#undef DEF_FUNCTION_TYPE_VAR_1
#undef DEF_POINTER_TYPE

#define DEF_BUILTIN(ENUM, NAME, CLASS, TYPE, LIBTYPE,			\
		    BOTH_P, FALLBACK_P, NONANSI_P, ATTRS)		\
  if (NAME)								\
    {									\
      tree decl;							\
									\
      if (strncmp (NAME, "__builtin_", strlen ("__builtin_")) != 0)	\
	abort ();							\
									\
	decl = builtin_function (NAME, builtin_types[TYPE], ENUM,	\
				 CLASS,					\
				 (FALLBACK_P				\
				  ? (NAME + strlen ("__builtin_"))	\
				  : NULL),				\
				 NULL);	                                \
									\
      built_in_decls[(int) ENUM] = decl;				\
    }									
#include "builtins.def"
#undef DEF_BUILTIN

  main_identifier_node = get_identifier ("main");
}

#include "gt-f95-f95-lang.h"
#include "gtype-f95.h"
