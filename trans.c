/* Code translation
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

/* trans.c-- generate GCC trees from g95_code */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "tree.h"
#include "tree-simple.h"
#include <stdio.h>
#include "c-common.h"
#include "ggc.h"
#include "toplev.h"
#include "defaults.h"
#include "real.h"
#include <gmp.h>
#include <assert.h>
#define BACKEND_CODE
#include "g95.h"
#include "trans.h"
#include "trans-stmt.h"
#include "trans-array.h"
#include "trans-types.h"
#include "trans-const.h"

/* Naming convention for backend interface code:
   g95_trans_* translate g95_code into STMT trees.
   g95_conv_* expression conversion
   g95_get_* get a backend tree representation of a decl or type

   Coding conventions for backend interface code:
   GNU Coding Standard + GCC extensions.  */

static g95_file *g95_current_backend_file;

/* Advance along TREE_CHAIN n times.  */
tree
g95_advance_chain (tree t, int n)
{
  for (; n > 0; n--)
    {
      assert (t != NULL_TREE);
      t = TREE_CHAIN (t);
    }
  return t;
}

/* Wrap a node in a list node and add it th the end of a list.  */
tree
g95_chainon_list (tree list, tree add)
{
  tree l;

  l = tree_cons (NULL_TREE, add, NULL_TREE);

  return chainon (list, l);
}

/* Strip off a legitimate source ending from the input string NAME of
   length LEN.  Rather than having to know the names used by all of
   our front ends, we strip off an ending of a period followed by
   up to five characters.  (Java uses ".class".)  */

static inline void
remove_suffix (char *name, int len)
{
  int i;

  for (i = 2;  i < 8 && len > i;  i++)
    {
      if (name[len - i] == '.')
        {
          name[len - i] = '\0';
          break;
        }
    }
}

/* Create a decl for an artificial decl with the given type.  */
tree
g95_create_var_np (tree type, const char * prefix)
{
  static unsigned int id_num = 1;
  char *tmp_name;
  char *preftmp = NULL;
  tree tmp_var;

  if (prefix)
    {
      preftmp = ASTRDUP (prefix);
      remove_suffix (preftmp, strlen (preftmp));
      prefix = preftmp;
    }

  ASM_FORMAT_PRIVATE_NAME (tmp_name, (prefix ? prefix : "T"), id_num++);

  /* Make the type of the variable writable.  */
  type = build_type_variant (type, 0, 0);

  tmp_var = build_decl (VAR_DECL, get_identifier (tmp_name), type);

  /* The variable was declared by the compiler.  */
  DECL_ARTIFICIAL (tmp_var) = 1;

  /* Make the variable writable.  */
  TREE_READONLY (tmp_var) = 0;

  DECL_EXTERNAL (tmp_var) = 0;
  TREE_STATIC (tmp_var) = 0;
  TREE_USED (tmp_var) = 1;

  return tmp_var;
}

/* Like above, but also adds it to the current scope.  */
tree
g95_create_var (tree type, const char * prefix)
{
  tree tmp;

  tmp = g95_create_var_np (type, prefix);

  pushdecl (tmp);

  return tmp;
}

/* If the value is not constant, Create a temporary and copy the value.  */
tree
g95_evaluate_now (tree expr, stmtblock_t * pblock)
{
  tree var;

  if (TREE_CODE_CLASS (TREE_CODE (expr)) == 'c')
    return expr;

  var = g95_create_var (TREE_TYPE (expr), NULL);
  g95_add_modify_expr (pblock, var, expr);

  return var;
}

/* Add a MODIFY_EXPR to a block.  */
void
g95_add_modify_expr (stmtblock_t * pblock, tree lhs, tree rhs)
{
  tree tmp;

  tmp = build (MODIFY_EXPR, void_type_node, lhs, rhs);
  g95_add_expr_to_block (pblock, tmp);
}

/* Create a new scope/binding level and initialize a block.  */
void
g95_start_block (stmtblock_t * block)
{
  pushlevel (0);
  block->head = NULL_TREE;
  block->has_scope = 1;
}

/* Initialize a block without creating a new scope.  */
void
g95_init_block (stmtblock_t * block)
{
  /* This function must not allocate anything that requires freeing as it may
     be discarded without being used.  */
  block->head = NULL_TREE;
  block->has_scope = 0;
}

/* We've decided we don't need this scope, so merge it with the parent.
   Only variable decls will be merged, you still need to add the code.  */
void
g95_merge_block_scope (stmtblock_t * block)
{
  tree decl;
  tree next;

  assert (block->has_scope);
  block->has_scope = 0;

  /* Remember the decls in this scope.  */
  decl = getdecls();
  poplevel (0, 0, 0);

  /* Add them to the parent scope.  */
  while (decl != NULL_TREE)
    {
      next = TREE_CHAIN (decl);
      TREE_CHAIN (decl) = NULL_TREE;

      pushdecl (decl);
      decl = next;
    }
}

/* Finish a scope containing a block of statements.  */
tree
g95_finish_block (stmtblock_t * stmtblock)
{
  tree decl;
  tree expr;
  tree block;

  expr = stmtblock->head;
  stmtblock->head = NULL_TREE;

  if (stmtblock->has_scope)
    {
      decl = getdecls ();

      if (decl)
        {
          block = poplevel (1, 0, 0);
          expr = build_v (BIND_EXPR, decl, expr, block);
        }
      else
        poplevel (0, 0, 0);
    }

  return rationalize_compound_expr (expr);
}

/* Build a CALL_EXPR.  */
tree
g95_build_function_call (tree fndecl, tree arglist)
{
  tree fn;
  tree call;

  fn = build1 (ADDR_EXPR, build_pointer_type (TREE_TYPE (fndecl)), fndecl);
  call = build (CALL_EXPR, TREE_TYPE (TREE_TYPE (fndecl)), fn, arglist);
  TREE_SIDE_EFFECTS (call) = 1;

  return call;
}

/* Generate a runtime error if cond is true.  */
void
g95_trans_runtime_check (tree cond, tree msg, stmtblock_t * pblock)
{
  stmtblock_t block;
  tree body;
  tree tmp;
  tree args;

  cond = fold (cond);

  if (integer_zerop (cond))
    return;

  /* The code to generate the error.  */
  g95_start_block (&block);

  assert (TREE_CODE (msg) == STRING_CST);

  TREE_USED (msg) = 1;

  tmp = build1 (ADDR_EXPR, pchar_type_node, msg);
  args = g95_chainon_list (NULL_TREE, tmp);

  tmp = build1 (ADDR_EXPR, pchar_type_node, g95_strconst_current_filename);
  args = g95_chainon_list (args, tmp);

  tmp = build_int_2 (lineno, 0);
  args = g95_chainon_list (args, tmp);

  tmp = g95_build_function_call (gfor_fndecl_runtime_error, args);
  g95_add_expr_to_block (&block, tmp);

  body = g95_finish_block (&block);

  if (integer_onep (cond))
    {
      g95_add_expr_to_block (pblock, body);
    }
  else
    {
      /* Tell the compiler that this isn't likley.  */
      tmp = g95_chainon_list (NULL_TREE, cond);
      tmp = g95_chainon_list (tmp, integer_zero_node);
      cond = g95_build_function_call (built_in_decls[BUILT_IN_EXPECT], tmp);

      tmp = build_v (COND_EXPR, cond, body, empty_stmt_node);
      g95_add_expr_to_block (pblock, tmp);
    }
}

/* Add a statement to a bock.  */
void
g95_add_expr_to_block (stmtblock_t * block, tree expr)
{
  assert (block);

  if (expr == empty_stmt_node || ! expr)
    return;

  if (block->head)
    block->head = build (COMPOUND_EXPR, void_type_node, block->head, expr);
  else
    block->head = expr;
}

/* Add a block the end of a block.  */
void
g95_add_block_to_block (stmtblock_t * block, stmtblock_t * append)
{
  assert (append);
  assert (! append->has_scope);

  g95_add_expr_to_block (block, append->head);
  append->head = NULL_TREE;
}

/* Get the current locus.  The structure may not be complete, and should only
   be used with g95_set_current_locus.  */
void
g95_get_backend_locus (locus * loc)
{
  loc->line = lineno - 1;
  loc->file = g95_current_backend_file;
}

/* Set the current locus.  */
void
g95_set_backend_locus (locus * loc)
{
  lineno = loc->line + 1;
  g95_current_backend_file = loc->file;
  input_filename = loc->file->filename;
}

/* Translate an executable statement.  */
tree
g95_trans_code (g95_code * code)
{
  stmtblock_t block;
  tree  res;

  if (! code)
    return (NULL_TREE);

  g95_start_block (&block);

  /* Translate statements one by one to SIMPLE trees until we reach
     the end of this g95_code branch.  */
  for ( ; code ; code = code->next)
    {
      g95_set_backend_locus (&code->loc);
      if (code->here != 0)
        {
          res = g95_trans_label_here (code);
          wrap_all_with_wfl (&res, input_filename, lineno);
          g95_add_expr_to_block (&block, res);
        }


      switch (code->op)
        {
        case EXEC_NOP:
          res = NULL_TREE;
          break;

        case EXEC_ASSIGN:
          res = g95_trans_assign (code);
          break;

        case EXEC_POINTER_ASSIGN:
          res = g95_trans_pointer_assign (code);
          break;

        case EXEC_CONTINUE:
          res = NULL_TREE;
          break;

        case EXEC_CYCLE:
          res = g95_trans_cycle (code);
          break;

        case EXEC_EXIT:
          res = g95_trans_exit (code);
          break;

        case EXEC_GOTO:
          res = g95_trans_goto (code);
          break;

        case EXEC_STOP:
          res = g95_trans_stop (code);
          break;

        case EXEC_CALL:
          res = g95_trans_call (code);
          break;

        case EXEC_RETURN:
          res = g95_trans_return (code);
          break;

        case EXEC_IF:
          res = g95_trans_if (code);
          break;

        case EXEC_ARITHMETIC_IF:
          res = g95_trans_arithmetic_if (code);
          break;

        case EXEC_DO:
          res = g95_trans_do (code);
          break;

        case EXEC_DO_WHILE:
          res = g95_trans_do_while (code);
          break;

        case EXEC_SELECT:
          res = g95_trans_select (code);
          break;

        case EXEC_FORALL:
          res = g95_trans_forall (code);
          break;

        case EXEC_WHERE:
          res = g95_trans_where (code);
          break;

        case EXEC_ALLOCATE:
          res = g95_trans_allocate (code);
          break;

        case EXEC_DEALLOCATE:
          res = g95_trans_deallocate (code);
          break;

        case EXEC_OPEN:
          res = g95_trans_open (code);
          break;

        case EXEC_CLOSE:
          res = g95_trans_close (code);
          break;

        case EXEC_READ:
          res = g95_trans_read (code);
          break;

        case EXEC_WRITE:
          res = g95_trans_write (code);
          break;

        case EXEC_IOLENGTH:
          res = g95_trans_iolength (code);
          break;

        case EXEC_BACKSPACE:
          res = g95_trans_backspace (code);
          break;

        case EXEC_ENDFILE:
          res = g95_trans_endfile (code);
          break;

        case EXEC_INQUIRE:
          res = g95_trans_inquire (code);
          break;

        case EXEC_REWIND:
          res = g95_trans_rewind (code);
          break;

	case EXEC_TRANSFER:
	  res = g95_trans_transfer(code);
	  break;

	case EXEC_DT_END:
	  res = g95_trans_dt_end(code);
	  break;

        default:
          internal_error ("g95_trans_code(): Bad statement code");
        }

      if (res && res != empty_stmt_node)
        {
          wrap_all_with_wfl (&res, input_filename, lineno);
          /* Add the new statemment to the block.  */
          g95_add_expr_to_block (&block, res);
        }
    }

  /* Return the finished block.  */
  return g95_finish_block (&block);
}

/* This function is called after a complete program unit has been parsed
   and resolved.  */
void
g95_generate_code (g95_namespace * ns)
{
  g95_symbol *main_program = NULL;
  symbol_attribute attr;

  /* Main program subroutine.  */
  if (! ns->proc_name)
    {
      /* Lots of things get upset if a subroutine doesn't have a symbol, so we
          make one now.  Hopefully we've set all the required fields.  */
      g95_get_symbol ("MAIN__", ns, &main_program);
      g95_clear_attr(&attr);
      attr.flavor = FL_PROCEDURE;
      attr.proc = PROC_UNKNOWN;
      attr.subroutine = 1;
      attr.access = ACCESS_PUBLIC;
      main_program->attr = attr;
      ns->proc_name = main_program;
      g95_commit_symbols ();
    }

  g95_generate_function_code (ns);
}

/* This function is called after a complete module has been parsed
   and resolved.  */
void
g95_generate_module_code (g95_namespace * ns)
{
  g95_namespace *n;

  g95_generate_module_vars (ns);

  for (n = ns->contained ; n ; n = n->sibling)
    g95_generate_function_code (n);
}
