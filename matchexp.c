/* Expression matcher
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

/* matchexp.c-- Expression parser */


#include <ctype.h>
#include "g95.h"

static char expression_syntax[] = "Syntax error in expression at %C";


/* g95_match_defined_op()-- Match a user-defined operator.  If
 * 'def_flag' is nonzero we assume that we are looking to define a new
 * operator with this name. */

match g95_match_defined_op(g95_symbol **result, int def_flag) {
static char *badops[] = {
 "true", "false", "and", "or", "not", "eqv", "neqv", "eq", "ne",
 "ge", "le", "lt", "gt", NULL };

char name[G95_MAX_SYMBOL_LEN+1];
g95_symbol *sym;
locus old_loc;
match m;
int i;

  old_loc = *g95_current_locus();

  m = g95_match(" . %n .", name);
  if (m != MATCH_YES) return m;

  for(i=0; badops[i]; i++)
    if (strcmp(badops[i], name) == 0) {
      if (def_flag) {
	g95_error("The name '%s' cannot be used as a defined operator at %C",
		  name);
	return MATCH_ERROR;
      } else {
	g95_set_locus(&old_loc);  /* Hope we have a logical constant */
	return MATCH_NO;
      }
    }

  for(i=0; name[i]; i++)
    if (!isalpha(name[i])) {
      g95_error("Bad character '%c' in OPERATOR name at %C", name[i]);
      return MATCH_ERROR;
    }

  sym = g95_get_symbol(name, NULL);

  if (def_flag == 0 && sym->operator != NULL) {
    g95_error("Symbol '%s' at %C is not a defined unary operator", name);
    return MATCH_ERROR;
  }

  *result = sym;
  return MATCH_YES;
}


/* next_operator()-- Checks to see if the given operator is next on
 * the input.  If this is not the case, the parse pointer remains
 * where it was. */

static int next_operator(intrinsic_op t) {
locus old_loc;
int u;

  old_loc = *g95_current_locus();
  if (g95_match_intrinsic_op(&u) == MATCH_YES && t == u) return 1;

  g95_set_locus(&old_loc);
  return 0;
}


/* match_primary()-- Match a primary expression */

static match match_primary(g95_expr **result) {
match m;

  m = g95_match_literal_constant(result, 0);
  if (m != MATCH_NO) return m;

  m = g95_match_array_constructor(result);
  if (m != MATCH_NO) return m;

  m = g95_match_rvalue(result);
  if (m != MATCH_NO) return m;

/* Match an expression in parenthesis */

  if (g95_match(" (") != MATCH_YES) return MATCH_NO;

  m = g95_match_expr(result);
  if (m == MATCH_NO) goto syntax;
  if (m == MATCH_ERROR) return m;

  m = g95_match(" )");
  if (m == MATCH_NO)
    g95_error("Expected a right parenthesis in expression at %C");

  if (m != MATCH_YES) {
    g95_free_expr(*result);
    return MATCH_ERROR;
  }

  return MATCH_YES;

syntax:
  g95_error(expression_syntax);
  return MATCH_ERROR;
}


/* build_node()-- Build an operator expression node. */

static g95_expr *build_node(intrinsic_op operator, locus *where,
			    g95_expr *op1, g95_expr *op2) {
g95_expr *new;

  new = g95_get_expr();
  new->expr_type = EXPR_OP;
  new->rank = 0;
  new->operator = operator;
  new->where = *where;
  
  new->op1 = op1;
  new->op2 = op2;

  return new;
}


/* match_level_1()-- Match a level 1 expression */

static match match_level_1(g95_expr **result) {
g95_symbol *sym;
g95_expr *e;
locus where;
match m;

  where = *g95_current_locus(); 
  sym = NULL; 
  m = g95_match_defined_op(&sym, 0);
  if (m == MATCH_ERROR) return m;

  m = match_primary(&e);
  if (m != MATCH_YES) return m;

  *result = (sym == NULL) ? e : build_node(INTRINSIC_USER, &where, e, NULL);

  return MATCH_YES;
}


static match match_mult_operand(g95_expr **result) {
g95_expr *e, *exp;
locus where;
match m;

  m = match_level_1(&e);
  if (m != MATCH_YES) return m;

  if (!next_operator(INTRINSIC_POWER)) {
    *result = e;
    return MATCH_YES;
  }

  where = *g95_current_locus();

  m = match_mult_operand(&exp);
  if (m == MATCH_NO) g95_error("Expected exponent in expression at %C");
  if (m != MATCH_YES) {
    g95_free_expr(e);
    return MATCH_ERROR;
  }

  *result = build_node(INTRINSIC_POWER, &where, e, exp);
  return MATCH_YES;
}


static match match_add_operand(g95_expr **result) {
locus where, old_loc;
g95_expr *all, *e;
match m;
int i;

  m = match_mult_operand(&all);
  if (m != MATCH_YES) return m;

  for(;;) {    /* Build up a string of products or quotients */
    i = 0;

    old_loc = *g95_current_locus();

    if (next_operator(INTRINSIC_TIMES))
      i = INTRINSIC_TIMES;
    else {
      if (next_operator(INTRINSIC_DIVIDE))
	i = INTRINSIC_DIVIDE;
      else
	break;
    }

    where = *g95_current_locus();

    m = match_mult_operand(&e);
    if (m == MATCH_NO) {
      g95_set_locus(&old_loc);
      break;
    }

    if (m == MATCH_ERROR) {
      g95_free_expr(all);
      return MATCH_ERROR;
    }

    all = build_node(i, &where, all, e);
  }


  *result = all;
  return MATCH_YES;
}


static int match_add_op(void) {

  if (next_operator(INTRINSIC_MINUS)) return -1;
  if (next_operator(INTRINSIC_PLUS)) return 1;
  return 0;
}


/* match_level_2()-- Match a level 2 expression.  */

static match match_level_2(g95_expr **result) {
g95_expr *all, *e;
locus where;
match m;
int i;

  where = *g95_current_locus();
  i = match_add_op();

  m = match_add_operand(&e);
  if (i != 0 && m == MATCH_NO) {
    g95_error(expression_syntax);
    m = MATCH_ERROR;
  }

  if (m != MATCH_YES) return m;

  if (i == 0)
    all = e;
  else
    all = build_node((i == -1) ? INTRINSIC_UMINUS : INTRINSIC_UPLUS,
		     &where, e, NULL);

/* Append add-operands to the sum */

  for(;;) {
    where = *g95_current_locus();
    i = match_add_op();
    if (i == 0) break;

    m = match_add_operand(&e);
    if (m == MATCH_NO) g95_error(expression_syntax);
    if (m != MATCH_YES) {
      g95_free_expr(all);
      return MATCH_ERROR;
    }

    all = build_node((i == -1) ? INTRINSIC_MINUS : INTRINSIC_PLUS,
		     &where, all, e);
  }

  *result = all;
  return MATCH_YES;
}


/* match_level_3()-- Match a level three expression */

static match match_level_3(g95_expr **result) {
g95_expr *all, *e;
locus where;
match m;

  m = match_level_2(&all);
  if (m != MATCH_YES) return m;

  for(;;) {
    if (!next_operator(INTRINSIC_CONCAT)) break;

    where = *g95_current_locus();

    m = match_level_2(&e);
    if (m == MATCH_NO) {
      g95_error(expression_syntax);
      g95_free_expr(all);
    }
    if (m != MATCH_YES) return MATCH_ERROR;

    all = build_node(INTRINSIC_CONCAT, &where, all, e);
  }

  *result = all;
  return MATCH_YES;
}


/* match_level_4()-- Match a level 4 expression */

static match match_level_4(g95_expr **result) {
g95_expr *left, *right;
locus old_loc;
locus where;
match m;
int i;

  m = match_level_3(&left);
  if (m != MATCH_YES) return m;

  old_loc = *g95_current_locus();

  if (g95_match_intrinsic_op(&i) != MATCH_YES) {
    *result = left;
    return MATCH_YES;
  }

  if (i != INTRINSIC_EQ && i != INTRINSIC_NE && i != INTRINSIC_GE &&
      i != INTRINSIC_LE && i != INTRINSIC_LT && i != INTRINSIC_GT) {
    g95_set_locus(&old_loc);
    *result = left;
    return MATCH_YES;
  }

  where = *g95_current_locus();

  m = match_level_3(&right);
  if (m == MATCH_NO) g95_error(expression_syntax);
  if (m != MATCH_YES) {
    g95_free_expr(left);
    return MATCH_ERROR;
  }

  *result = build_node(i, &where, left, right);
  return MATCH_YES;
}


static match match_and_operand(g95_expr **result) {
g95_expr *e;
locus where;
match m;
int i;

  i = next_operator(INTRINSIC_NOT);
  where = *g95_current_locus(); 

  m = match_level_4(&e);
  if (m != MATCH_YES) return m;

  *result = i ? build_node(INTRINSIC_NOT, &where, e, NULL) : e;
  return MATCH_YES;
}


static match match_or_operand(g95_expr **result) {
g95_expr *all, *e;
locus where;
match m;

  m = match_and_operand(&all);
  if (m != MATCH_YES) return m;

  for(;;) {
    if (!next_operator(INTRINSIC_AND)) break;
    where = *g95_current_locus();

    m = match_and_operand(&e);
    if (m == MATCH_NO) g95_error(expression_syntax);
    if (m != MATCH_YES) {
      g95_free_expr(all);
      return MATCH_ERROR;
    }

    all = build_node(INTRINSIC_AND, &where, all, e);
    where = *g95_current_locus();
  }

  *result = all;
  return MATCH_YES;
}


static match match_equiv_operand(g95_expr **result) {
g95_expr *all, *e;
locus where;
match m;

  m = match_or_operand(&all);
  if (m != MATCH_YES) return m;

  for(;;) {
    if (!next_operator(INTRINSIC_OR)) break;
    where = *g95_current_locus();

    m = match_or_operand(&e);
    if (m == MATCH_NO) g95_error(expression_syntax);
    if (m != MATCH_YES) {
      g95_free_expr(all);
      return MATCH_ERROR;
    }

    all = build_node(INTRINSIC_OR, &where, all, e);
    where = *g95_current_locus();
  }

  *result = all;
  return MATCH_YES;
}


/* match_level_5()-- Match a level 5 expression */

static match match_level_5(g95_expr **result) {
g95_expr *all, *e;
locus where;
match m;
int i;

  m = match_equiv_operand(&all);
  if (m != MATCH_YES) return m;

  for(;;) {
    if (next_operator(INTRINSIC_EQV))
      i = INTRINSIC_EQV;
    else {
      if (next_operator(INTRINSIC_NEQV))
	i = INTRINSIC_NEQV;
      else 
	break;
    }

    where = *g95_current_locus();
  
    m = match_equiv_operand(&e);
    if (m == MATCH_NO) g95_error(expression_syntax);
    if (m != MATCH_YES) {
      g95_free_expr(all);
      return MATCH_ERROR;
    }

    all = build_node(i, &where, all, e);
    where = *g95_current_locus();
  }

  *result = all;
  return MATCH_YES;
}


/* g95_match_expr()-- Match an expression.  At this level, we are
 * stringing together level 5 expressions separated by binary operators. */

match g95_match_expr(g95_expr **result) {
g95_expr *all, *e;
g95_symbol *sym;
locus where;
match m;

  m = match_level_5(&all);
  if (m != MATCH_YES) return m;

  for(;;) {
    m = g95_match_defined_op(&sym, 0);
    if (m == MATCH_NO) break;
    if (m == MATCH_ERROR) {
      g95_free_expr(all);
      return MATCH_ERROR;
    }

    where = *g95_current_locus();

    m = match_level_5(&e);
    if (m == MATCH_NO) g95_error(expression_syntax);
    if (m != MATCH_YES) {
      g95_free_expr(all);
      return MATCH_ERROR;
    }

    all = build_node(INTRINSIC_USER, &where, all, e);
    all->symbol = sym;

    where = *g95_current_locus();
  }

  *result = all;
  return MATCH_YES;
}
