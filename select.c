/* SELECT CASE statement
   Copyright (C) 2000 Free Software Foundation, Inc.
   Contributed by Andy Vaught

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

#include "g95.h"


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

  if (g95_match(" :") == MATCH_YES) {
    m = g95_match_scalar_expr(&c->high);
    if (m == MATCH_NO) goto need_expr;
    if (m == MATCH_ERROR) goto cleanup;
    goto done;
  }

  m = g95_match_scalar_expr(&c->low);
  if (m == MATCH_ERROR) goto cleanup;
  if (m == MATCH_NO) goto need_expr;

  if (g95_match(" :") != MATCH_YES)
    c->high = c->low;      /* Make a range out of a single target */
  else {
    m = g95_match_scalar_expr(&c->high);
    if (m == MATCH_ERROR) goto cleanup;
    if (m == MATCH_NO) goto done;   /* It's OK if nothing is there! */
  }

done:
  *cp = c;
  return MATCH_YES;

need_expr:
  g95_error("Expected expression in CASE at %C");

cleanup:
  free_case(c);
  g95_free(c);

  return MATCH_ERROR;
}


/* match_case_eos()-- Match the end of a case statement */

static match match_case_eos(void) {
char name[G95_MAX_SYMBOL_LEN+1];
match m;

  if (g95_match_eos() == MATCH_YES) return MATCH_YES;

  if (g95_match_space() != MATCH_YES) return MATCH_NO;

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

  head = NULL;

  if (g95_current_state() != COMP_SELECT) {
    g95_error("Unexpected CASE statement at %C");
    return MATCH_ERROR;
  }

  if (g95_match("% default") == MATCH_YES) {
    m = match_case_eos();
    if (m == MATCH_NO) goto syntax;
    if (m == MATCH_ERROR) goto cleanup;

    new_st.op = EXEC_SELECT;
    new_st.ext = g95_get_case();
    return MATCH_YES;
  }

  if (g95_match(" (") != MATCH_YES) goto syntax;

  for(;;) {
    if (match_case_selector(&c) == MATCH_ERROR) goto cleanup;

    if (head == NULL)
      head = c;
    else
      tail->next = c;

    tail = c;

    if (g95_match(" )") == MATCH_YES) break;
    if (g95_match(" ,") != MATCH_YES) goto syntax;
  }

  m = match_case_eos();
  if (m == MATCH_NO) goto syntax;
  if (m == MATCH_ERROR) goto cleanup;

  new_st.op = EXEC_SELECT;
  new_st.ext = head;

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
	      &e->where, g95_typename(type));
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
 */

void g95_resolve_select(g95_code *code) {
g95_code *body;
g95_expr *expr;
g95_case *cp;
int kind;
bt type;
try t;

  expr = code->expr; 

  if (expr->ts.type == BT_DERIVED || expr->ts.type == BT_REAL ||
      expr->ts.type == BT_COMPLEX)
    g95_error("Argument of SELECT statement at %L cannot be %s",
	      &code->expr->where, g95_typename(expr->ts.type));

  type = expr->ts.type;
  if (type == BT_CHARACTER) kind = expr->ts.kind;

  t = SUCCESS;

  for(body=code; body; body=body->block) {
    g95_resolve_code(body->next);

    if (t == FAILURE) continue;

    for(cp=body->ext; cp; cp=cp->next) {
      if (cp->low != NULL) {
	if (g95_resolve_expr(cp->low) == FAILURE) {
	  t = FAILURE;
	  break;
	}

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
	if (g95_resolve_expr(cp->high) == FAILURE) {
	  t = FAILURE;
	  break;
	}

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
		  cp->low->where);
	t = FAILURE;
      }
    }
  }

  if (t == FAILURE) return;

// check_case_overlap(code);

}
