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
#include <stdlib.h>

#include "g95.h"

static int overlap;


/* free_case()-- Free a single case structure. */

static void free_case(g95_case *p) {

  g95_free_expr(p->low);
  if (p->high != p->low) g95_free_expr(p->high);

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
  c->where = *g95_current_locus();

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

    /* Cases that can never be matched are legal to have but mess up
     * code generation, so we discard them here. */

    if (c->low != NULL && c->high != NULL && c->low != c->high &&
	g95_compare_expr(c->low, c->high) > 0) {
      g95_warning("Range specification at %C can never be matched");
      free_case(c);
    } else {
      if (head == NULL)
	head = c;
      else
	tail->next = c;

      tail = c;
    }

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

static try check_case_expr(g95_expr *e, g95_expr *selector) {

  if (e == NULL) return SUCCESS;

  if (e->expr_type != EXPR_CONSTANT) {
    g95_error("Expression in CASE statement at %L must be a constant",
	      &e->where);
    return FAILURE;
  }

  if (e->ts.type != selector->ts.type) {
    g95_error("Expression in CASE statement at %L must be of type %s",
	      &e->where, g95_basic_typename(selector->ts.type));
    return FAILURE;
  }

  if (e->ts.kind != selector->ts.kind) {
    g95_error("Expression in CASE statement at %L must be kind %d",
	      &e->where, selector->ts.kind);
    return FAILURE;
  }

  if (e->rank != 0) {
    g95_error("Expression in CASE statement at %L must be scalar",
	      &e->where);
    return FAILURE;
  }

  return SUCCESS;
}


/* compare_case()-- Compare two case nodes.  This is used to sort the
 * nodes into order and also gives us the perfect opportunity to check
 * for overlap.  The default case is sorted first in the list,
 * followed by a unbounded upper, followed by the full ranges in
 * order, followed by the unbounded lower.  We only report the first
 * overlap. */

static int compare_case(const void *v1, const void *v2) {
const g95_case *c1, *c2;
int p1, p2;
char *msg;

  c1 = *((g95_case **) v1);
  c2 = *((g95_case **) v2);

  /* Check for duplicate defaults */

  if (c1->low == NULL && c1->high == NULL &&
      c2->low == NULL && c2->high == NULL) {

    msg = "Duplicate CASE DEFAULT at %L and %L";
    goto error;
  }

  /* Arrange for the default case to be first of all if present. */

  if (c1->low == NULL && c1->high == NULL) return -1;
  if (c2->low == NULL && c2->high == NULL) return 1;

  /* Detect duplicate X: and :X forms.  These conflict regardless of X. */

  if ((c1->high == NULL && c2->high == NULL) ||
      (c1->low  == NULL && c2->low  == NULL)) {
    msg = "Unbounded CASEs conflict at %L and %L";
    goto error;
  }

  /* Compare X: against :A */

  if (c1->high == NULL && c2->low == NULL) {
    if (g95_compare_expr(c1->low, c2->high) <= 0) goto got_overlap;
    return 1;
  }

  /* Compare :X against B: */

  if (c1->low == NULL && c2->high == NULL) {
    if (g95_compare_expr(c1->high, c2->low) >= 0) goto got_overlap;
    return -1;
  }

  /* Compare :X against A:B */

  if (c1->low == NULL) {  
    if (g95_compare_expr(c1->high, c2->low) >= 0) goto got_overlap;
    return -1;
  }

  /* Compare X: against A:B */

  if (c1->high == NULL) {
    if (g95_compare_expr(c1->low, c2->high) <= 0) goto got_overlap;
    return 1;
  }

  /* Compare X:Y against :A */

  if (c2->low == NULL) {
    if (g95_compare_expr(c1->low, c2->high) <= 0) goto got_overlap;
    return 1;
  }

  /* Compare X:Y against A: */

  if (c2->high == NULL) {
    if (g95_compare_expr(c1->high, c2->low) >= 0) goto got_overlap;
    return -1;
  }

  /* Having dispensed with almost a dozen special cases, we can now
   * deal with the general case of X:Y against A:B */

  p1 = g95_compare_expr(c1->high, c2->low);
  if (p1 < 0) p1 = -1;
  if (p1 > 0) p1 = 1;

  p2 = g95_compare_expr(c1->low, c2->high);
  if (p2 < 0) p2 = -1;
  if (p2 > 0) p2 = 1;

  if (p1 == 0 || p2 == 0 || p1 != p2) goto got_overlap;
  return p1;

got_overlap:
  msg = "CASEs at %L and %L overlap";

error:
  if (!overlap) {
    g95_error(msg, &c1->where, &c2->where);
    overlap = 1;
  }

  /* Because we've generated an error, no code will be generated, and
   * the order of the case array no longer matters.  Return something
   * to keep qsort() happy. */

  return 1;
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
g95_case *p, **array;
g95_code *body;
g95_expr *expr;
int i, n, kind;
try t;

  if (code->expr == NULL) {
    /* This was actually a computed GOTO statement.  */
    expr = code->expr2;

    if (expr->ts.type != BT_INTEGER || expr->rank != 0)
      g95_error("Selection expression in COMPUTED GOTO statement "
                "at %L must be a scalar integer expression",
                &expr->where);
    return;
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

  t = SUCCESS;
  n = 0;

  for(body=code->block; body; body=body->block) {
    for(p=body->ext.case_list; p; p=p->next) {
      p->code = body->next;
      n++;

      if (check_case_expr(p->low, expr) == FAILURE) {
	t = FAILURE;
	break;
      }

      if (check_case_expr(p->high, expr) == FAILURE) {
	t = FAILURE;
	break;
      }

      if (expr->ts.type == BT_LOGICAL &&
	  (p->low != NULL || p->high != NULL) &&
	  (p->low == NULL || p->high == NULL)) {

	g95_error("Logical range in CASE statement at %L is not allowed",
		  &p->low->where);
	t = FAILURE;
      }
    }

    if (t == FAILURE) break;
  }

  if (t == FAILURE || n == 0) return;

  array = g95_getmem(n*sizeof(g95_case *));
  n = 0;

  for(body=code->block; body; body=body->block)
    for(p=body->ext.case_list; p; p=p->next)
      array[n++] = p;

  overlap = 0;

  qsort(array, n, sizeof(g95_case *), compare_case);

  if (overlap) return;

  /* String the case structures together in a doubly linked list */

  array[0]->cprev = NULL;
  array[0]->cnext = (n > 1) ? array[1] : NULL;

  if (n > 1) {
    for(i=1; i<n-1; i++) {
      array[i]->cprev = array[i-1];
      array[i]->cnext = array[i+1];
    }

    array[n-1]->cprev = array[n-2];
    array[n-1]->cnext = NULL;
  }

  g95_free(array);
}
