/* Misc tree functions.
   Copyright (C) 2002 Free Software Foundation, Inc.
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

/* f95-tree.c-- Various functions that should be language indepedant, but
   aren't.  */

#include "config.h"
#include "system.h"
#include "tree.h"
#include "tree-simple.h"
#include "tree-inline.h"
#include "c-common.h"
#define BACKEND_CODE
#include "g95.h"
#include "trans.h"

/*  Replaces T; by a COMPOUND_STMT containing {T;}.  */

void
tree_build_scope (t)
     tree *t;
{
  tree comp_stmt, start_scope, end_scope;

  /* If T already has a proper scope, do nothing.  */
  if (*t
      && TREE_CODE (*t) == COMPOUND_STMT
      && COMPOUND_BODY (*t))
    return;

  /* Create a new empty scope.  */
  comp_stmt = make_node (COMPOUND_STMT);

  start_scope = make_node (SCOPE_STMT);
  SCOPE_BEGIN_P (start_scope) = 1;

  end_scope = make_node (SCOPE_STMT);
  SCOPE_BEGIN_P (end_scope) = 0;

  COMPOUND_BODY (comp_stmt) = start_scope;

  if (*t)
    {
      /* If T is not empty, insert it inside the newly created scope.  Note
	 that we can't just join TREE_CHAIN(*T) to the closing scope
	 because even if T wasn't inside a scope, it might be a list of
	 statements.  */
      TREE_CHAIN (start_scope) = *t;
      chainon (*t, end_scope);
    }
  else
    {
      /* T is empty.  Simply join the start/end nodes.  */
      TREE_CHAIN (start_scope) = end_scope;
    }

  /* Set T to the newly constructed scope.  */
  *t = comp_stmt;
}
/*  Copy every statement from the chain CHAIN by calling deep_copy_node().
    Return the new chain.  */

tree
deep_copy_list (chain)
     tree chain;
{
  tree new_chain, res;

  if (chain == NULL_TREE)
    /* Nothing to copy.  */
    return NULL_TREE;

  new_chain = deep_copy_node (chain);
  res = new_chain;

  while (TREE_CHAIN (chain))
    {
      chain = TREE_CHAIN (chain);
      TREE_CHAIN (new_chain) = deep_copy_node (chain);
      new_chain = TREE_CHAIN (new_chain);
    }

  return res;
}


/*  Create a deep copy of NODE.  The only nodes that are not deep copied
    are declarations, constants and types.  */

/*  Create a deep copy of NODE.  The only nodes that are not deep copied
    are declarations, constants and types.  */

tree
deep_copy_node (node)
     tree node;
{
  tree res;

  if (node == NULL_TREE)
    return NULL_TREE;

  switch (TREE_CODE (node))
    {
    case COMPOUND_STMT:
      res = build_stmt (COMPOUND_STMT, deep_copy_list (COMPOUND_BODY (node)));
      break;

    case FOR_STMT:
      res = build_stmt (FOR_STMT,
			deep_copy_node (FOR_INIT_STMT (node)),
			deep_copy_node (FOR_COND (node)),
			deep_copy_node (FOR_EXPR (node)),
			deep_copy_node (FOR_BODY (node)));
      break;

    case WHILE_STMT:
      res = build_stmt (WHILE_STMT,
			deep_copy_node (WHILE_COND (node)),
			deep_copy_node (WHILE_BODY (node)));
      break;

    case DO_STMT:
      res = build_stmt (DO_STMT,
			deep_copy_node (DO_COND (node)),
			deep_copy_node (DO_BODY (node)));
      break;

    case IF_STMT:
      res = build_stmt (IF_STMT,
			deep_copy_node (IF_COND (node)),
			deep_copy_node (THEN_CLAUSE (node)),
			deep_copy_node (ELSE_CLAUSE (node)));
      break;

    case SWITCH_STMT:
      res = build_stmt (SWITCH_STMT,
			deep_copy_node (SWITCH_COND (node)),
			deep_copy_node (SWITCH_BODY (node)));
      break;

    case EXPR_STMT:
      res = build_stmt (EXPR_STMT, deep_copy_node (EXPR_STMT_EXPR (node)));
      break;

    case DECL_STMT:
      res = build_stmt (DECL_STMT, DECL_STMT_DECL (node));
      break;

    case RETURN_STMT:
      res = build_stmt (RETURN_STMT, deep_copy_node (RETURN_STMT_EXPR (node)));
      break;

    case TREE_LIST:
      res = build_tree_list (deep_copy_node (TREE_PURPOSE (node)),
	                     deep_copy_node (TREE_VALUE (node)));
      break;

    case SCOPE_STMT:
      if (SCOPE_BEGIN_P (node))
	{
	  /* ??? The sub-blocks and supercontext for the scope's BLOCK_VARS
		 should be re-computed after copying.  */
	  res = build_stmt (SCOPE_STMT,
			    deep_copy_list (SCOPE_STMT_BLOCK (node)));
	  SCOPE_BEGIN_P (res) = 1;
	}
      else
	{
	  res = build_stmt (SCOPE_STMT, NULL_TREE);
	  SCOPE_BEGIN_P (res) = 0;
	}
      break;

    default:
      walk_tree (&node, copy_tree_r, NULL, NULL);
      res = node;
      break;
    }

  /* Set the line number.  */
  if (statement_code_p (TREE_CODE (node)))
    STMT_LINENO (res) = STMT_LINENO (node);

  return res;
}

/* Change the flags for the type of the node T to make it writable.  */
static void
make_type_writable (tree t)
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
		make_type_writable (it);
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
remove_suffix (char * name, int len)
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
  make_type_writable (tmp_var);

  DECL_EXTERNAL (tmp_var) = 0;
  TREE_STATIC (tmp_var) = 0;
  TREE_USED (tmp_var) = 1;


  return tmp_var;
}

