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
/* TODO: remove this file.  */

#include "config.h"
#include "system.h"
#include "tree.h"
#include "tree-simple.h"
#include "tree-inline.h"
#include "c-common.h"
#include "hard-reg-set.h"
#include "basic-block.h"
#include "tree-flow.h"
#define BACKEND_CODE
#include "g95.h"
#include "trans.h"

/* Mark *EXPR_P as not simplified.  */
void
mark_not_simple (tree *expr_p)
{
  TREE_NOT_GIMPLE (*expr_p) = 1;
}

/* Similar to copy_tree_r() but do not copy SAVE_EXPR nodes.  These nodes
   model computations that should only be done once.  If we were to unshare
   something like SAVE_EXPR(i++), the simplification process would create
   wrong code.

   Additionally, copy any flags that were set in the original tree.  */

static tree
mostly_copy_tree_r (tree *tp, int *walk_subtrees, void *data)
{
  enum tree_code code = TREE_CODE (*tp);
  /* Don't unshare decls, blocks, types and SAVE_EXPR nodes.  */
  if (TREE_CODE_CLASS (code) == 't'
      || TREE_CODE_CLASS (code) == 'd'
      || TREE_CODE_CLASS (code) == 'c'
      || TREE_CODE_CLASS (code) == 'b'
      || code == SAVE_EXPR
      || *tp == empty_stmt_node)
    *walk_subtrees = 0;
  else if (code == STMT_EXPR || code == SCOPE_STMT || code == BIND_EXPR)
    /* Unsharing STMT_EXPRs doesn't make much sense; they tend to be
       complex, so they shouldn't be shared in the first place.  Unsharing
       SCOPE_STMTs breaks because copy_tree_r zeroes out the block.  */
    abort ();
  else
    {
      enum tree_flags flags = tree_flags (*tp);
      copy_tree_r (tp, walk_subtrees, data);
      if (flags)
	set_tree_flag (*tp, flags);
    }

  return NULL_TREE;
}

/* Callback for walk_tree to unshare most of the shared trees rooted at
   *TP.  If *TP has been visited already (i.e., TREE_VISITED (*TP) == 1),
   then *TP is deep copied by calling copy_tree_r.

   This unshares the same trees as copy_tree_r with the exception of
   SAVE_EXPR nodes.  These nodes model computations that should only be
   done once.  If we were to unshare something like SAVE_EXPR(i++), the
   simplification process would create wrong code.  */

static tree
copy_if_shared_r (tree *tp, int *walk_subtrees, void *data ATTRIBUTE_UNUSED)
{
  /* If this node has been visited already, unshare it and don't look
     any deeper.  */
  if (TREE_VISITED (*tp))
    {
      walk_tree (tp, mostly_copy_tree_r, NULL, NULL);
      *walk_subtrees = 0;
    }
  else
    /* Otherwise, mark the tree as visited and keep looking.  */
    TREE_VISITED (*tp) = 1;

  return NULL_TREE;
}

static tree
unmark_visited_r (tree *tp, int *walk_subtrees ATTRIBUTE_UNUSED,
                  void *data ATTRIBUTE_UNUSED)
{
  if (TREE_VISITED (*tp))
    TREE_VISITED (*tp) = 0;
  else
    *walk_subtrees = 0;

  return NULL_TREE;
}


/* Unshare T and all the trees reached from T via TREE_CHAIN.  */

void
unshare_all_trees (t)
     tree t;
{
  walk_tree (&t, copy_if_shared_r, NULL, NULL);
  walk_tree (&t, unmark_visited_r, NULL, NULL);
}

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

