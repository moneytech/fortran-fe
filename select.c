/* SELECT CASE statement
   Copyright (C) 2000, 2001, 2002 Free Software Foundation, Inc.
   Contributed by Andy Vaught and Steven Bosscher

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

/* select.c-- Handle the SELECT CASE statement. */

#include <string.h>

#include "g95.h"

/* Structure which holds information about the AVL tree.
 * Node in this tree are of type "struct g95_case *". */
typedef struct case_tree {
  g95_case root;		/* Tree root node is in root->link[0]. */
  g95_case *default_case;       /* Cannot store in AVL, so keep it here */
  g95_case *unbounded[2];       /* Holds unbounded cases, 0 = low, 1 = high */
}
case_tree;


/* compare_cases() -- helper function for overlap checker.
 * determines interval overlaps for CASEs. Return <0 if op1 < op2,
 * 0 for overlap, >0 for op1 > op2. 
 * Assumes we're not dealing with unbounded or default cases */

static int compare_cases(g95_case *op1, g95_case *op2) {

  if (g95_compare_expr(op1->high, op2->low) < 0) return -1;
  if (g95_compare_expr(op1->low, op2->high) > 0) return 1;
  return 0;
}


/* avl_create() -- Set up a new AVL tree. Free a tree with g95_free() */

static case_tree *avl_create(void) {
case_tree *tree;
  tree = g95_getmem(sizeof(case_tree));
  tree->root.link[0] = tree->root.link[1] = NULL; 
  tree->default_case = NULL;
  tree->unbounded[0] = tree->unbounded[1] = NULL; 
  return tree;
}


/* check_case_overlap() -- Look for and repport overlapping CASEs.
 * Returns FAILURE if overlap is found, and adds the new case and 
 * returns SUCCESS otherwise. CASEs are put into an AVL tree instead 
 * of a linked list to minimize search time at runtime for SELECT CASE
 * blocks where the evaluation expression is of type CHARACTER. Code
 * for such SELECT blocks will be generated from the AVL tree because
 * the GCC backend doesn't support them.
 *
 * AVL insert routine is a modified version of that found in
 * Ben Pfaff's GNU libavl. Uses Knuth's Algorithm 6.2.3A but caches
 * results of comparisons */

static try check_case_overlap(case_tree *tree, g95_case *cp)
{
g95_case *t, *s, *p, *q, *r;
g95_expr *e1, *e2;
int i;

  t = &tree->root;
  s = p = t->link[0];

  /* intercept the default case and unbounded cases */
  if (cp->low == NULL || cp->high == NULL) {

    if (cp->low == NULL && cp->high == NULL) { /* default case */
      tree->default_case = cp;
      return SUCCESS;
    }

    i = (cp->low == NULL) ?  0 : 1;
    s = tree->unbounded[i];
    if (s != NULL) { /* already seen this unbounded case? */
      p = s;
      goto overlap;
    }
    tree->unbounded[i] = cp;

    /* unbounded cases can only overlap with leftmost or rightmost node */
    if (p != NULL) { /* could be an empty tree */
      while(p->link[i] != NULL) 
        p = p->link[i];
      if (i == 0) {
        if (g95_compare_expr(cp->high,p->low) >= 0) goto overlap;
      } else {
        if (g95_compare_expr(cp->low,p->high) <= 0) goto overlap;
      }
    }

    return SUCCESS;
  }

  /* This is were we're going to build the tree */
  if (s == NULL) { /* tree is empty */
    q = t->link[0] = cp;
    q->link[0] = q->link[1] = NULL;
    q->balance = 0;
    return SUCCESS;
  }

  for (;;) { /* search the tree */
    int diff = compare_cases(cp, p);

    if (diff < 0) { /* all values in range for *cp are smaller than  *p->low */
      p->cache = 0;
      q = p->link[0];
      if (q == NULL) {
        p->link[0] = q = cp;
        break;
      }
    } else if (diff > 0) { /* value range for *cp is smaller than for *p */
      p->cache = 1;
      q = p->link[1];
      if (q == NULL) {
        p->link[1] = q = cp;
        break;
      }
    } else goto overlap; /* overlaps with prior CASE */

    if (q->balance != 0) t = p, s = q;
    p = q;
  }
  
  q->link[0] = q->link[1] = NULL;
  q->balance = 0;

  /* Update balance for affected subtree */
  r = p = s->link[(int) s->cache];
  while (p != q) {
    p->balance = p->cache * 2 - 1;
    p = p->link[(int) p->cache];
  }

  /* Check tree balance */
  if (s->cache == 0) { /* node was inserted into left subtree */
    if (s->balance == 0) { /* node balance was neutral */
      s->balance = -1;
      return SUCCESS;
    } else if (s->balance == +1) { /* was right heavy */
      s->balance = 0;
      return SUCCESS;
    }

    /* node was left heavy, so we need rotations */      
    if (r->balance == -1) {
      p = r;
      s->link[0] = r->link[1];
      r->link[1] = s;
      s->balance = r->balance = 0;
    } else {
      p = r->link[1];
      r->link[1] = p->link[0];
      p->link[0] = r;
      s->link[0] = p->link[1];
      p->link[1] = s;
      if (p->balance == -1) 
        s->balance = 1, r->balance = 0;
      else if (p->balance == 0)
        s->balance = r->balance = 0;
      else {
        s->balance = 0;
        r->balance = -1;
      }
      p->balance = 0;
    }
  } 

  else { /* node was inserted into right subtree */
    if (s->balance == 0) {
      s->balance = 1;
      return SUCCESS;
    }
    else if (s->balance == -1) {
      s->balance = 0;
      return SUCCESS;
    }

    if (r->balance == +1) {
      p = r;
      s->link[1] = r->link[0];
      r->link[0] = s;
      s->balance = r->balance = 0;
    } else {
      p = r->link[0];
      r->link[0] = p->link[1];
      p->link[1] = r;
      s->link[1] = p->link[0];
      p->link[0] = s;
      if (p->balance == +1)
        s->balance = -1, r->balance = 0;
      else if (p->balance == 0)
        s->balance = r->balance = 0;
      else {
        s->balance = 0, r->balance = 1;
      }
      p->balance = 0;
    }
  }

  /* reconnect subtree */
  if (t != &tree->root && s == t->link[1])
    t->link[1] = p;
  else
    t->link[0] = p;

  return SUCCESS;

overlap:
  e1 = (cp->low == NULL) ? cp->high : cp->low; /* avoid SIGSEGV */
  e2 = (p->low == NULL) ? p->high : p->low;
  g95_error("CASE value range at %L overlaps with prior CASE statement at %L", 
            &e1->where, &e2->where); 
  return FAILURE;  
}


/* traverse_tree -- visits root, then left, then right (RLN).
 * This is a first step towards generating code for CHARACTER cases. */

static void traverse_tree(g95_case *tree, int depth) {
  if (tree == NULL) return;
#if 0 
  generate_cmps_and_jmps();
#endif
  traverse_tree(tree->link[0], depth + 1);
  traverse_tree(tree->link[1], depth + 1);
}


/* free_case()-- Free a single case structure */

static void free_case(g95_case *p) {

  if (p->low == NULL || p->high == NULL) {
    if (p->low != NULL) g95_free_expr(p->low);
    if (p->high != NULL) g95_free_expr(p->high);
  } else {
    g95_free_expr(p->low);
    if (p->high != p->low) g95_free_expr(p->high);
  }

  g95_free(p);
}


/* g95_free_case_list()-- Free a list of case structures */

void g95_free_case_list(g95_case *p) {
g95_case *q;

  for(;p ;p=q) {
    q = p->next;
    free_case(p);
  }
}


/* g95_match_select()-- Match a SELECT statement */

match g95_match_select(void) {
g95_expr *expr;
match m;

  m = g95_match_label();
  if (m == MATCH_ERROR) return m;

  m = g95_match(" select case ( %e )%t", &expr);
  if (m != MATCH_YES) return m;

  new_st.op = EXEC_SELECT;
  new_st.expr = expr;

  return MATCH_YES;
}


/* match_case_selector()-- Match a single case selector. */

static match match_case_selector(g95_case **cp) {
g95_case *c;
match m;

  c = g95_get_case();

  if (g95_match_char(':') == MATCH_YES) {
    m = g95_match_expr(&c->high);
    if (m == MATCH_NO) goto need_expr;
    if (m == MATCH_ERROR) goto cleanup;

    if (c->high->ts.type == BT_LOGICAL) goto logical_range;
    goto done;
  }

  m = g95_match_expr(&c->low);
  if (m == MATCH_ERROR) goto cleanup;
  if (m == MATCH_NO) goto need_expr;

  if (g95_match_char(':') != MATCH_YES)
    c->high = c->low;      /* Make a range out of a single target */
  else {
    m = g95_match_expr(&c->high);
    if (m == MATCH_ERROR) goto cleanup;
    if (m == MATCH_NO) goto done;   /* It's OK if nothing is there! */

    if (c->high->ts.type == BT_LOGICAL) goto logical_range;
  }

done:
  *cp = c;
  return MATCH_YES;

logical_range:
  g95_error("Logical range in CASE statement at %C not allowed");
  goto cleanup;

need_expr:
  g95_error("Expected expression in CASE at %C");

cleanup:
  free_case(c);
  return MATCH_ERROR;
}


/* match_case_eos()-- Match the end of a case statement */

static match match_case_eos(void) {
char name[G95_MAX_SYMBOL_LEN+1];
match m;

  if (g95_match_eos() == MATCH_YES) return MATCH_YES;

  g95_gobble_whitespace();

  m = g95_match_name(name);
  if (m != MATCH_YES) return m;

  if (strcmp(name, g95_current_block()->name) != 0) {
    g95_error("Expected case name of '%s' at %C", g95_current_block()->name);
    return MATCH_ERROR;
  }

  return g95_match_eos();
}


/* g95_match_case()-- Match a CASE statement */

match g95_match_case(void) {
g95_case *c, *head, *tail;
match m;

  head = tail = NULL;

  if (g95_current_state() != COMP_SELECT) {
    g95_error("Unexpected CASE statement at %C");
    return MATCH_ERROR;
  }

  if (g95_match("% default") == MATCH_YES) {
    m = match_case_eos();
    if (m == MATCH_NO) goto syntax;
    if (m == MATCH_ERROR) goto cleanup;

    new_st.op = EXEC_SELECT;
    new_st.ext.case_list = g95_get_case();
    return MATCH_YES;
  }

  if (g95_match_char('(') != MATCH_YES) goto syntax;

  for(;;) {
    if (match_case_selector(&c) == MATCH_ERROR) goto cleanup;

    if (head == NULL)
      head = c;
    else
      tail->next = c;

    tail = c;

    if (g95_match_char(')') == MATCH_YES) break;
    if (g95_match_char(',') != MATCH_YES) goto syntax;
  }

  m = match_case_eos();
  if (m == MATCH_NO) goto syntax;
  if (m == MATCH_ERROR) goto cleanup;

  new_st.op = EXEC_SELECT;
  new_st.ext.case_list = head;

  return MATCH_YES;

syntax:
  g95_error("Syntax error in CASE-specification at %C");

cleanup:
  g95_free_case_list(head);
  g95_undo_statement();
  return MATCH_ERROR;
}


/* check_case_expr()-- Check to see if an expression is suitable for
 * use in a CASE statement. */

static try check_case_expr(g95_expr *e, bt type) {

  if (e->expr_type != EXPR_CONSTANT) {
    g95_error("Expression in CASE statement at %L must be a constant",
	      &e->where);
    return FAILURE;
  }

  if (e->ts.type != type) {
    g95_error("Expression in CASE statement at %L must be of type %s",
	      &e->where, g95_basic_typename(type));
    return FAILURE;
  }

  if (e->rank != 0) {
    g95_error("Expression in CASE statement at %L must be scalar",
	      &e->where);
    return FAILURE;
  }

  return SUCCESS;
}


/* g95_resolve_select()-- Given a completely parsed select statement, we:
 *
 *   Resolve all expressions and code within the SELECT
 *   Make sure that the selection expression is not of the wrong type
 *   Make sure that all case expressions are of the same type/kind
 *   Make sure that no case ranges overlap
 *
 * We have the additional caveat that a SELECT construct could have
 * been a computed GOTO in the source code. Furtunately we're done
 * here prety quick: all we have to make sure is that the case_expr
 * is a scalar integer expression.
 */

void g95_resolve_select(g95_code *code) {
g95_code *body;
g95_expr *expr;
g95_case *cp;
case_tree *tree;
int kind, overlap;
bt type;
try t;

  if (code->expr == NULL) 
  {
    /* This was actually a computed GOTO statement.  */
    expr = code->expr2;
    if ((expr->ts.type != BT_INTEGER)
        || (expr->rank != 0))
      g95_error("Selection expression in COMPUTED GOTO statement "
                "at %L must be a scalar integer expression",
                &expr->where);
    return; /* Either way, we're done.  */
  }
 
  expr = code->expr;
    
  kind = -1;
  if (expr->ts.type == BT_DERIVED || expr->ts.type == BT_REAL ||
      expr->ts.type == BT_COMPLEX) {
    g95_error("Argument of SELECT statement at %L cannot be %s",
               &expr->where, g95_typename(&expr->ts));
    return; /* Going on here just produce more garbage error messages.  */
  }

  if (expr->rank != 0) {
    g95_error("Argument of SELECT statement at %L must be a scalar "
              "expression", &expr->where);
    return;
  }

  type = expr->ts.type;
  if (type == BT_CHARACTER) kind = expr->ts.kind;

  t = SUCCESS;
  tree = avl_create();
  overlap = 0;
 
  for(body=code->block; body; body=body->block) {
    if (t == FAILURE) continue;

    for(cp=body->ext.case_list; cp; cp=cp->next) {
      if (cp->low != NULL) {
	if (check_case_expr(cp->low, type) == FAILURE) {
	  t = FAILURE;
	  break;
	}

	if (type == BT_CHARACTER && cp->low->ts.kind != kind) {
	  g95_error("Character expression in CASE statement at %L must be "
		    "of kind %d", &cp->low->where, kind);
	  t = FAILURE;
	  break;
	}
      }

      if (cp->high != NULL) {
	if (check_case_expr(cp->high, type) == FAILURE) {
	  t = FAILURE;
	  break;
	}

	if (type == BT_CHARACTER && cp->high->ts.kind != kind) {
	  g95_error("Character expression in CASE statement at %L must be "
		    "of kind %d", &cp->high->where, kind);
	  t = FAILURE;
	  break;
	}
      }

      if (type == BT_LOGICAL && cp->low != NULL && cp->high != NULL &&
	  cp->low != cp->high) {
	g95_error("Logical range in CASE statement at %L is not allowed",
		  &cp->low->where);
	t = FAILURE;
      }      
 
      if (cp->low != NULL && cp->high != NULL && cp->low != cp->high) {
        if (g95_compare_expr(cp->low, cp->high) > 0) {
          g95_warning("Range specification at %L can never be matched;\n\t "
                      "first expression greater than second expression",
                      &cp->high->where);
	  continue; /* just ignore this case, but don't fail */;
        }
      }

      if (check_case_overlap(tree, cp) != SUCCESS) overlap = 1;
    }
  }

  if ((t == FAILURE) || overlap) goto done;

#if 0
  if (type == BT_CHARACTER) {
    setup_labels(); /* put label in front of code block and add a jump to END SELECT at end */
    generate_case_code(code,tree); /* generate compares and jumps */
  }
#endif
done:
  g95_free(tree);
}
