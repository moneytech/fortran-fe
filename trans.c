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

/* Naming convention for backend interface code:
   g95_trans_* translate g95_code into STMT trees.
   g95_conv_* expression conversion
   g95_get_* get a backend tree representation of a decl or type

   Coding conventions for backend interface code:
   GNU Coding Standard + GCC extensions.  */


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

/* Create a temporart variable.  */
/* TODO: Array temporaries. Small array temps. can go on the stack,
   but larger ones should be created elsewhere (malloced, on the
   heap).  */
tree
g95_create_tmp_var (tree type)
{
  static unsigned int tmp_num = 1; /* 2^32 temporaries should be enough.  */
  tree tmp_var;
  char *tmp_name;

  /* We cannot deal with derived types, pointers or arrays yet.  */
  if (TREE_CODE (type) == RECORD_TYPE
      || TREE_CODE (type) == POINTER_TYPE
      || TREE_CODE (type) == ARRAY_TYPE)
      g95_todo_error ("Derived type, pointer and array temporaries");

  /* Build the name of this temporary.  */
  ASM_FORMAT_PRIVATE_NAME (tmp_name, "T", tmp_num++);

  /* If the type is an array, something is wrong.  */
  if (TREE_CODE (type) == ARRAY_TYPE)
    abort ();

  /* Build the temporary variable tree node. */
  tmp_var = build_decl (VAR_DECL, get_identifier (tmp_name), type);

  /* The variable was locally declared by the compiler.  */
  DECL_ARTIFICIAL (tmp_var)=1;
  DECL_EXTERNAL (tmp_var)=0;

  /* The declaration is local to this file and this scope.  */
  TREE_PUBLIC (tmp_var) = 0;
  TREE_STATIC (tmp_var) = 1;

  /* Assuming we're sane, this variable wasn't required by the
     compiler just to fill the scope ;-)  */
  TREE_USED (tmp_var) = 1;

  /* Make the variable writable.  */
  TREE_READONLY (tmp_var) = 0;

  /* Make the type of the variable writable.  */
  g95_make_type_writable (tmp_var);

  /* Put the new variable in the current scope.  */
  pushdecl (tmp_var);

  return tmp_var;
}

/* Create a new scope/binding level.  */
void
g95_push_scope (void)
{
  pushlevel (0);
}

/* Finish a scope containing stmt list, and create and add DECL_STMTs
   for the current binding level. */
tree
g95_pop_scope (tree body, tree tail)
{
  tree  decl;
  tree  stmt;
  tree  block;
  tree  head;

  head = stmt = build_stmt (SCOPE_STMT, NULL_TREE);

  SCOPE_BEGIN_P (stmt) = 1;
  for (decl = getdecls () ; decl ; decl = TREE_CHAIN (decl))
    {
      TREE_CHAIN (stmt) = build_stmt (DECL_STMT, decl);
      stmt = TREE_CHAIN (stmt);
    }

  block = poplevel (1, 0, 0);

  TREE_CHAIN (stmt) = body;

  if (tail == NULL_TREE)
    tail = stmt;
  else
    assert (TREE_CHAIN (tail) == NULL_TREE);
  while (TREE_CHAIN (tail))
    tail = TREE_CHAIN (tail);

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

/* Start a statement.  */
/* These may do more in the future. For now that just wrap g95_*_scope.  */
void
g95_start_stmt ()
{
  g95_push_scope ();
}

/* Finish a statement.  Cleanup all the temporaries and wrap up in a
 COMPOUND_STMT */
tree
g95_finish_stmt (tree head, tree tail)
{
  return g95_pop_scope (head, tail);
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
  head = NULL_TREE;
  tail = NULL_TREE;

  /* Translate statements one by one to SIMPLE trees until we reach
     the end of this g95_code branch.  */
  for ( ; code ; code = code->next)
    {
      if (code->here != 0)
        {
          res=g95_trans_label_here (code);
          g95_add_stmt_to_list (&head, &tail, res, NULLi_TREE);
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

  /* We now have translated the full g95_code chain passed to us.  If
     the resulting stmt tree is not a single tree but a chain, put it
     in its own compound.  */
  if (! head)
    return NULL_TREE;

  return build_stmt (COMPOUND_STMT, head);
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

/* This function is called after a complete program uni has been parsed
   and resolved. First we generate prototype declarations for all
   functions, and then we generate code for one function at a time.
*/
void
g95_generate_code (g95_namespace * ns)
{
  tree fndecl;
  g95_namespace *n;

  if (! ns->proc_name)
    g95_todo_error ("Main Program without symbol");

  fndecl = g95_get_function_decl (ns->proc_name);

  current_function_decl = fndecl;

  g95_generate_function_protos (ns);


  for (n = ns->contained ; n ; n = n->sibling)
    g95_generate_function_code (n);

  g95_generate_function_code (ns);

  current_function_decl = NULL;
}

/* This function is called after a complete module has been parsed
   and resolved.  */
void
g95_generate_module_code (g95_namespace * ns)
{
  g95_todo_error ("modules");
}

