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
#include "tree.h"
#include "tree-simple.h"
#include <stdio.h>
#include "c-common.h"
#include "ggc.h"
#include "rtl.h"
#include "toplev.h"
#include "function.h"
#include "expr.h"
#include "real.h"
#include <gmp.h>
#include <assert.h>
#define BACKEND_CODE
#include "g95.h"
#include "trans.h"
#include "trans-stmt.h"
#include "trans-array.h"

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
  for ( ; n > 0 ; n--)
    {
      assert (t != NULL_TREE);
      t = TREE_CHAIN (t);
    }
  return t;
}

/* Like chainon(list, listify(add)) except it ignores TREE_CHAIN(add).  */
tree
g95_chainon_list (tree list, tree add)
{
  tree l;

  l = tree_cons (NULL_TREE, add, NULL_TREE);

  return chainon (list, l);
}

/* Change the flags for the type of the node T to make it writable.  */
static void
g95_make_type_writable (tree t)
{
  if (t == NULL_TREE)
    abort ();

  if (TYPE_READONLY (TREE_TYPE (t))
      || ((TREE_CODE (TREE_TYPE (t)) == RECORD_TYPE
	   || TREE_CODE (TREE_TYPE (t)) == UNION_TYPE)
	  /* && C_TYPE_FIELDS_READONLY (TREE_TYPE (t))*/))
    {
      /* Make a copy of the type declaration.  */
      TREE_TYPE (t) = build_type_copy (TREE_TYPE (t));
      TYPE_READONLY (TREE_TYPE (t)) = 0;

      /* If the type is a structure that contains a field readonly.  */
      if ((TREE_CODE (TREE_TYPE (t)) == RECORD_TYPE
	   || TREE_CODE (TREE_TYPE (t)) == UNION_TYPE)
	  /*&& C_TYPE_FIELDS_READONLY (TREE_TYPE (t))*/)
	{
	  /*C_TYPE_FIELDS_READONLY (TREE_TYPE (t)) = 0;*/

	  /* Make the fields of the structure writable.  */
	  {
	    tree it;
	    it = TYPE_FIELDS (TREE_TYPE (t));
	    while (it)
	      {
		/* Make the field writable.  */
		TREE_READONLY (it) = 0;
		
		/* Make the type of the field writable.  */
		g95_make_type_writable (it);
		it = TREE_CHAIN (it);
	      }
	  }
	}
    }
}

/* Strip off a legitimate source ending from the input string NAME of
   length LEN.  Rather than having to know the names used by all of
   our front ends, we strip off an ending of a period followed by
   up to five characters.  (Java uses ".class".)  */

static inline void
remove_suffix (name, len)
     char *name;
     int len;
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

/** Create a new temporary variable declaration of type TYPE.  Returns the
    newly created decl and pushes it into the current binding.  */
tree
create_tmp_var (tree type, const char * prefix)
{
  tree tmp_var;

  /* Get a temporary.  */
  tmp_var = create_tmp_alias_var (type, prefix);

  /* Add it to the current scope.  */
  pushdecl (tmp_var);

  return tmp_var;
}

/*  Create a new temporary alias variable declaration of type TYPE.  Returns the
    newly created decl. Does NOT push it into the current binding.  */
tree
create_tmp_alias_var (tree type, const char * prefix)
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

  tmp_var = build_decl (VAR_DECL, get_identifier (tmp_name), type);

  /* The variable was declared by the compiler.  */
  DECL_ARTIFICIAL (tmp_var) = 1;

  /* Make the variable writable.  */
  TREE_READONLY (tmp_var) = 0;

  /* Make the type of the variable writable.  */
  g95_make_type_writable (tmp_var);

  DECL_EXTERNAL (tmp_var) = 0;
  TREE_STATIC (tmp_var) = 0;
  TREE_USED (tmp_var) = 1;


  return tmp_var;
}

/* Create a temporary variable.  Note that it may be used for small array
   temporaries.  Also used for array descriptors.  Be careful when you call
   this as the temporary is added to the current scope.  It is only valid
   inside the g95_start/finish_stmt pair in which it is created.  */
tree
g95_create_tmp_var (tree type)
{
  return create_tmp_var (type, NULL);
}

/* SIMPLE constant folding helper, returns a simple_val.  If tmpvar does
   not point to an existing temporary a new one will be created as neccessary.
   The expression should already be a simple_rhs.  Use with tmpvar=NULL is
   safe.  Care must be taken when reusing temporary variables.  See below.  */
tree
g95_simple_fold(tree expr, tree * phead, tree * ptail, tree * tmpvar)
{
  tree tmp;
  tree var;
  tree stmt;

  assert (is_simple_rhs (expr));

  tmp = fold (expr);
  if (is_simple_val (tmp))
    return tmp;

  if (! is_simple_rhs (expr))
      warning ("Internal inconsistancy: fold broke SIMPLE");

  if (tmpvar != NULL)
    {
      var = *tmpvar;
      assert (var == NULL_TREE || TREE_CODE (var) == VAR_DECL);
    }
  else
    var = NULL_TREE;

  if (var == NULL_TREE)
    {
      var = g95_create_tmp_var (TREE_TYPE (tmp));
      if (tmpvar != NULL)
        *tmpvar = var;
    }

  tmp = build (MODIFY_EXPR, TREE_TYPE (tmp), var, tmp);
  stmt = build_stmt (EXPR_STMT, tmp);
  g95_add_stmt_to_list (phead, ptail, stmt, stmt);

  return var;
}

static int
g95_is_artificial_decl (tree t)
{
  if (TREE_CODE (t) == NON_LVALUE_EXPR)
    t = TREE_OPERAND (t, 0);
  return (DECL_ARTIFICIAL (t));
}

/* Like g95_simple_fold, but will not return annother temporary variable.
   Used when the value of the operands may change before the result is used.

   eg:
    tmp1 = 2;
    tmp2 = fold_tmp (tmp1 * 1); // folds to assign(tmp2, tmp1), not tmp2=>tmp1
    tmp1 = 3;
    tmp2 = tmp2 + tmp1;
   This situation can easily happen inside loops.
   if g95_simple_fold was used, the final value of tmp would be 3, not 5.  */
tree
g95_simple_fold_tmp(tree expr, tree * phead, tree * ptail, tree * tmpvar)
{
  tree tmp;
  tree var;
  tree stmt;

  assert (is_simple_rhs (expr));

  tmp = fold (expr);
  if (is_simple_val (tmp)
      && (is_simple_const (tmp)
          || tmp == *tmpvar
          || ! g95_is_artificial_decl (tmp)))
    return tmp;

  if (! is_simple_rhs (expr))
      warning ("Internal inconsistancy: fold broke SIMPLE");

  if (tmpvar != NULL)
    {
      var = *tmpvar;
      assert (var == NULL_TREE || TREE_CODE (var) == VAR_DECL);
    }
  else
    var = NULL_TREE;

  if (var == NULL_TREE)
    {
      var = g95_create_tmp_var (TREE_TYPE (tmp));
      if (tmpvar != NULL)
        *tmpvar = var;
    }

  tmp = build (MODIFY_EXPR, TREE_TYPE (tmp), var, tmp);
  stmt = build_stmt (EXPR_STMT, tmp);
  g95_add_stmt_to_list (phead, ptail, stmt, stmt);

  return var;
}

/* Create a new scope/binding level.  */
void
g95_start_stmt (void)
{
  pushlevel (0);
}

/* We've decided we don't need this scope, so merge it with the parent.
   Only variable decls will be merged, you still need to add the code.  */
void
g95_merge_stmt (void)
{
  tree decl;
  tree next;

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

/* Like g95_finish_stmt, but only wraps in COMPOUND_STMT if there are variable
   decls in the scope.  Calling with se->post != NULL just doesn't make sense.
   Used to aviod excessive and pointless scopes for expressions.  */
void
g95_finish_se_stmt (g95_se * se)
{
  tree decls;

  /* Check we haven't been called with an incomplete expression.  */
  assert (se->post == NULL_TREE);
  /* Should never happen.  */
  assert (se->pre != NULL_TREE && TREE_CHAIN (se->pre_tail) == NULL_TREE);

  decls = getdecls();
  if (decls != NULL_TREE)
    {
      se->pre = g95_finish_stmt (se->pre, se->pre_tail);
      se->pre_tail = se->pre;
      assert (TREE_CHAIN (se->pre_tail) == NULL_TREE);
    }
  else
    g95_merge_stmt ();
}

/* Finish a scope containing a stmt list, and create and add DECL_STMTs
   for the current binding level.  Wrap the whole thing in a COMPOUND_STMT.  */
tree
g95_finish_stmt (tree body, tree tail)
{
  tree decl;
  tree stmt;
  tree block;
  tree head;
  tree next;

  head = stmt = build_stmt (SCOPE_STMT, NULL_TREE);

  SCOPE_BEGIN_P (stmt) = 1;
  for (decl = getdecls () ; decl ; decl = next)
    {
      next = TREE_CHAIN (decl);
      TREE_CHAIN (decl) = NULL_TREE;
      TREE_CHAIN (stmt) = build_stmt (DECL_STMT, decl);
      stmt = TREE_CHAIN (stmt);
    }

  block = poplevel (1, 0, 0);

  TREE_CHAIN (stmt) = body;

  if (tail == NULL_TREE)
    {
      tail = stmt;
      while (TREE_CHAIN (tail))
        tail = TREE_CHAIN (tail);
    }
  assert (TREE_CHAIN (tail) == NULL_TREE);

  /* Empty scope.  */
  if (TREE_CHAIN (head) == NULL_TREE)
    return (NULL_TREE);

  stmt = build_stmt (SCOPE_STMT, NULL_TREE);
  SCOPE_BEGIN_P (stmt) = 0;

  TREE_CHAIN (tail) = stmt;
  tail = stmt;

  SCOPE_STMT_BLOCK (head) = block;
  SCOPE_STMT_BLOCK (tail) = block;

  return build_stmt (COMPOUND_STMT, head);
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

/* Chain two stmt chains. PHEAD is the head of the current stmt chain
   (or 0 if HEAD is the new phead), HEAD is the chain you want to link
   to it. PTAIL is the new tail of the current stmt chain. It's set to
   TAIL, or to the tail of the HEAD chain if you pass 0 for TAIL.  */
void
g95_add_stmt_to_list (tree * phead, tree * ptail, tree head, tree tail)
{

  /* Make sure we have something to link to.  */
  assert (phead && ptail);

  if (head == NULL_TREE)
  {
    assert (tail == NULL_TREE);
    /* Don't add non-existing code to the tree. */
    return;
  }

  /* Chain the statement to the list or set if empty.  */
  if (*phead == NULL_TREE)
    *phead = head;
  else
    TREE_CHAIN (*ptail) = head;

  /* Find the tail of the passed tree if the caller was lazy.  */
  if (tail == NULL_TREE)
    {
      tail = head;
    }
  /* BUG: this function as sometimes called wit a tail that is not the last
     statement in the chain.  We need to move this loop inside the above if
     statement and incomment the assert below.  */
  while (TREE_CHAIN (tail) != NULL_TREE)
    tail = TREE_CHAIN (tail);

  /* Check this is actualy the tail of the tree.  */
/*  assert (TREE_CHAIN(tail) == NULL_TREE); */

  /* Set the new tail.  */
  *ptail = tail;

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

/* Translate an executable statement.
   Returns NULL_TREE if there is no code to translate.  */
tree
g95_trans_code (g95_code * code)
{
  tree  head;
  tree  tail;
  tree  res;

  if (! code)
    return (NULL_TREE);

  g95_start_stmt ();

  head = NULL_TREE;
  tail = NULL_TREE;

  /* Translate statements one by one to SIMPLE trees until we reach
     the end of this g95_code branch.  */
  for ( ; code ; code = code->next)
    {
      g95_set_backend_locus (&code->loc);
      if (code->here != 0)
        {
          res=g95_trans_label_here (code);
          g95_add_stmt_to_list (&head, &tail, res, NULL_TREE);
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
          if (code->next != NULL)
            warning ("CONTINUE statement is not last statement in block");
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

        default:
          fatal_error ("g95_trans_code(): Bad statement code");
        }

      /* If we've just successfully translated a statement, chain the
         generated stmt chain to the current stmt chain.  */
      if (res)
        {
          if (head)
            TREE_CHAIN (tail) = res;
          else
            head = res;
          tail = res;

          /* Find the tail of the new chain.  */
          while (TREE_CHAIN (tail))
            tail = TREE_CHAIN (tail);
        }
    }

  /* Abort empty chains.  */
  if (! head)
    {
      g95_merge_stmt();
      return NULL_TREE;
    }

  /* We now have translated the full g95_code chain passed to us.  If
     the resulting stmt tree is not a single tree but a chain, put it
     in its own scope.  */
  assert (TREE_CHAIN (tail) == NULL_TREE);
  if (head == tail && TREE_CODE (head) == COMPOUND_STMT)
    g95_merge_stmt ();
  else
    head = g95_finish_stmt (head, tail);

  return head;
}

/* These functions still need a bit of work to support everything,  */

/* Recursively generate prototypes for all functions and contained
   functions in a given namespace.  */
static void
g95_generate_function_protos (g95_namespace * ns)
{
  g95_namespace *n;

  /* Generate prototypes for functions/subroutines declared in an interface.  */
  /* ignore this for now
  for (n = ns->proc_name->formal_ns; n; n = n->sibling) */

  for (n = ns->contained; n; n = n->sibling)
    {

      g95_get_function_decl (n->proc_name);
      /*g95_generate_function_protos (n, func_decl);*/
    }
}

/* This function is called after a complete program unit has been parsed
   and resolved. First we generate prototype declarations for all
   functions, and then we generate code for one function at a time.
*/
void
g95_generate_code (g95_namespace * ns)
{
  tree fndecl;
  g95_namespace *n;
  g95_symbol *main_program = NULL;
  symbol_attribute attr;

  /* Main program subroutine.  */
  if (! ns->proc_name)
    {
      /* Lots of things get upset if a subroutine doesn't have a symbol, so we
          make one now.  Hopefully we've set all the required fields.  */
      g95_get_symbol ("__fortran_main", ns, &main_program);
      attr.flavor = FL_PROCEDURE;
      attr.proc = PROC_UNKNOWN;
      attr.subroutine = 1;
      attr.access = ACCESS_PUBLIC;
      main_program->attr = attr;
      ns->proc_name = main_program;
    }

  fndecl = g95_get_function_decl (ns->proc_name);

  current_function_decl = fndecl;

  g95_generate_function_protos (ns);


  for (n = ns->contained ; n ; n = n->sibling)
    {
      g95_todo_error("contained subroutines");
      g95_generate_function_code (n);
    }

  g95_generate_function_code (ns);

  current_function_decl = NULL;
}

static g95_namespace * module_namespace;

static void
g95_create_module_variable (g95_symbol * sym)
{
  tree decl;

  /* Only output symbols from this module.  */
  if (sym->ns != module_namespace)
    {
      /* I don't think this should ever happen.  */
      internal_error ("module symbol %d in wrong namespace", sym->name);
    }

  /* Only output variables.  */
  if (sym->attr.flavor != FL_VARIABLE)
    return;

  /* Don't generate variables from other modules.  */
  if (sym->attr.use_assoc)
    return;

  if (sym->backend_decl)
    internal_error ("backend decl for module variable %s already exists");

  /* Create the decl.  */
  decl = g95_get_symbol_decl (sym);
  /* Create the variable.  */
  pushdecl (decl);
  rest_of_decl_compilation (decl, NULL, 1, 0);

  /* Also add length of strings.  */
  if (G95_DECL_STRING (decl))
    {
      tree length;

      length = G95_DECL_STRING_LENGTH (decl);
      pushdecl (length);
      rest_of_decl_compilation (length, NULL, 1, 0);
    }
  /* TODO: initialise array variables.  */
}

/* This function is called after a complete module has been parsed
   and resolved.  */
void
g95_generate_module_code (g95_namespace * ns)
{
  g95_namespace *n;

  module_namespace = ns;
  g95_traverse_ns (ns, g95_create_module_variable);

  for (n = ns->contained ; n ; n = n->sibling)
    {
      g95_get_function_decl (n->proc_name);
      g95_generate_function_code (n);
    }
}

