/* Expression subroutines
   Copyright (C) 2000, 2001, 2002 Free Software Foundation, Inc.
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

/* expr.c-- Manipulate expression nodes */

#include <stdarg.h>
#include <stdio.h>
#include <string.h>

#include "g95.h"

/* g95_get_expr()-- Get a new expr node */

g95_expr *g95_get_expr(void) {
g95_expr *e;

  e = g95_getmem(sizeof(g95_expr));

  g95_clear_ts(&e->ts);
  e->op1 = NULL;
  e->op2 = NULL;

  return e;
}


/* g95_show_actual_arglist()-- Show an actual argument list */

void g95_show_actual_arglist(g95_actual_arglist *a) {

  g95_status("(");

  for(; a; a=a->next) {
    g95_status_char('(');
    if (a->name[0] != '\0') g95_status("%s = ", a->name);
    if (a->expr != NULL)
      g95_show_expr(a->expr);
    else
      g95_status("(arg not-present)");

    g95_status_char(')');
    if (a->next != NULL) g95_status(" ");
  }

  g95_status(")");
}


/* free_actual_arglist()-- Free an argument list and everything below it. */

void g95_free_actual_arglist(g95_actual_arglist *a1) {
g95_actual_arglist *a2;

  while(a1) {
    a2 = a1->next;
    g95_free_expr(a1->expr);
    g95_free_st_label(a1->label);
    g95_free(a1);
    a1 = a2;
  }
}


/* g95_copy_arglist()-- Copy an arglist structure and all of the arguments. */

g95_actual_arglist *g95_copy_actual_arglist(g95_actual_arglist *p) {
g95_actual_arglist *head, *tail, *new;

  head = tail = NULL;

  for(; p; p=p->next) {
    new = g95_get_actual_arglist();
    *new = *p;

    new->expr = g95_copy_expr(p->expr);
    new->next = NULL;

    if (head == NULL)
      head = new;
    else
      tail->next = new;

    tail = new;
  }

  return head;
}


/* g95_free_ref_list()-- Free a list of reference structures */

void g95_free_ref_list(g95_ref *p) {
g95_ref *q;
int i;

  for(; p; p=q) {
    q = p->next;

    switch(p->type) {
    case REF_ARRAY:
      for(i=0; i<G95_MAX_DIMENSIONS; i++) {
	g95_free_expr(p->u.ar.start[i]);
	g95_free_expr(p->u.ar.end[i]);
	g95_free_expr(p->u.ar.stride[i]);
      }

      break;

    case REF_SUBSTRING:
      g95_free_expr(p->u.ss.start);
      g95_free_expr(p->u.ss.end);
      break;

    case REF_COMPONENT:
      break;
    }

    g95_free(p);
  }
}


/* g95_free_expr0()-- Workhorse function for g95_free_expr() that
 * frees everything beneath an expression node, but not the node
 * itself.  This is useful when we want to simplify a node and replace
 * it with something else or the expression node belongs to another
 * structure.  */

static void g95_free_expr0(g95_expr *e) {

  switch(e->expr_type) {
  case EXPR_CONSTANT:
    switch(e->ts.type) {
    case BT_INTEGER:
      mpz_clear(e->value.integer);
      break;

    case BT_REAL:
      mpf_clear(e->value.real);
      break;

    case BT_CHARACTER:
      g95_free(e->value.character.string); 
      break;

    case BT_COMPLEX:
      mpf_clear(e->value.complex.r);
      mpf_clear(e->value.complex.i);
      break;

    default:
      break;
    }

    break;

  case EXPR_OP:
    if (e->op1 != NULL) g95_free_expr(e->op1);
    if (e->op2 != NULL) g95_free_expr(e->op2);
    break;

  case EXPR_FUNCTION:
    g95_free_actual_arglist(e->value.function.actual);
    break;

  case EXPR_VARIABLE:
    g95_free_ref_list(e->ref);
    break;

  case EXPR_ARRAY:
  case EXPR_STRUCTURE:
    g95_free_constructor(e->value.constructor);
    break;

  case EXPR_SUBSTRING:
    g95_free_ref_list(e->ref);
    g95_free(e->value.character.string);
    break;

  case EXPR_NULL:
    break;

  default:
    g95_internal_error("g95_free_expr0(): Bad expr type");
  }

  memset(e, '\0', sizeof(g95_expr));
}


/* g95_free_expr()-- Free an expression node and everything beneath it. */

void g95_free_expr(g95_expr *e) {

  if (e == NULL) return;

  g95_free_expr0(e);
  g95_free(e);
}


/* g95_replace_expr()-- grafts the *src expression onto the *dest
 * subexpression. */

void g95_replace_expr(g95_expr *dest, g95_expr *src) {

  g95_free_expr0(dest);
  *dest = *src;

  g95_free(src);
}


/* g95_extract_int()-- Tries to extract an integer constant from the
 * passed expression node.  Returns an error message or NULL if the
 * result is set.  It is tempting to generate an error and return
 * SUCCESS or FAILURE, but failure is OK for some callers. */

const char *g95_extract_int(g95_expr *expr, int *result) {

  if (expr->expr_type != EXPR_CONSTANT)
    return "Constant expression required at %C";

  if (expr->ts.type != BT_INTEGER)
    return "Integer expression required at %C";

  if ((mpz_cmp_si(expr->value.integer, INT_MAX) > 0) ||
      (mpz_cmp_si(expr->value.integer, INT_MIN) < 0)) {
    return "Integer value too large in expression at %C";
  }

  *result = (int) mpz_get_si(expr->value.integer);

  return NULL;
}


/* copy_ref()-- Recursively copy a list of reference structures */

static g95_ref *copy_ref(g95_ref *src) {
g95_array_ref *ar;
g95_ref *dest;

  if (src == NULL) return NULL; 

  dest = g95_get_ref();
  dest->type = src->type;

  switch(src->type) {
  case REF_ARRAY:
    ar = g95_copy_array_ref(&src->u.ar);
    dest->u.ar = *ar;
    g95_free(ar);
    break;

  case REF_COMPONENT:
    dest->u.c = src->u.c;
    break;

  case REF_SUBSTRING:
    dest->u.ss.start = g95_copy_expr(src->u.ss.start);
    dest->u.ss.end = g95_copy_expr(src->u.ss.end);
    break;
  }

  dest->next = copy_ref(src->next);

  return dest;
}


/* g95_free_array_shape()-- Free an array shape structure */

void g95_free_array_shape(g95_array_shape *p) {
int i;

  for(i=0; i<p->rank; i++)
    g95_free(p->shape[i]);

  g95_free(p);
}


/* g95_copy_array_shape()-- Copy an array shape structure */

g95_array_shape *g95_copy_array_shape(g95_array_shape *p) {
g95_array_shape *q;
int i;

  q = g95_get_array_shape();

  q->rank = p->rank;

  for(i=0; i<p->rank; i++)
    q->shape[i] = g95_copy_expr(p->shape[i]);

  return q;
}


/* g95_copy_expr()-- Given an expression pointer, return a copy of the
 * expression.  This subroutine is recursive. */

g95_expr *g95_copy_expr(g95_expr *p) {
g95_expr *q;
char *s;

  if (p == NULL) return NULL;

  q = g95_get_expr();
  *q = *p;

  switch(q->expr_type) {
  case EXPR_SUBSTRING:
    s = g95_getmem(p->value.character.length+1);
    q->value.character.string = s;

    memcpy(s, p->value.character.string, p->value.character.length+1);

    q->op1 = g95_copy_expr(p->op1);
    q->op2 = g95_copy_expr(p->op2);
    break;

  case EXPR_CONSTANT:
    switch(q->ts.type) {
    case BT_INTEGER:
      mpz_init_set(q->value.integer, p->value.integer);
      break;

    case BT_REAL:
      mpf_init_set(q->value.real, p->value.real);
      break;

    case BT_COMPLEX:
      mpf_init_set(q->value.complex.r, p->value.complex.r);
      mpf_init_set(q->value.complex.i, p->value.complex.i);
      break;

    case BT_CHARACTER:
      s = g95_getmem(p->value.character.length+1);
      q->value.character.string = s;

      memcpy(s, p->value.character.string, p->value.character.length+1);
      break;

    case BT_LOGICAL:
    case BT_DERIVED:
      break; /* Already done */

    case BT_PROCEDURE:
    case BT_UNKNOWN:
      g95_internal_error("g95_copy_expr(): Bad expr node");
      break;
    }

    break;

  case EXPR_OP:
    switch(q->operator) {
    case INTRINSIC_NOT:
    case INTRINSIC_UPLUS:
    case INTRINSIC_UMINUS:
      q->op1 = g95_copy_expr(p->op1);
      break;

    default:               /* Binary operators */
      q->op1 = g95_copy_expr(p->op1);
      q->op2 = g95_copy_expr(p->op2);
      break;
    }
    break;

  case EXPR_FUNCTION:
    q->value.function.actual =
      g95_copy_actual_arglist(p->value.function.actual);
    break;

  case EXPR_VARIABLE:
    q->ref = copy_ref(p->ref);
    break;

  case EXPR_STRUCTURE:
  case EXPR_ARRAY:
    q->value.constructor = g95_copy_constructor(p->value.constructor);
    break;

  case EXPR_NULL:
    break;
  }

  return q;
}


/* g95_kind_max()-- Return the maximum kind of two expressions.  In
 * general, higher kind numbers mean more precision for numeric types. */

int g95_kind_max(g95_expr *e1, g95_expr *e2) {

  return (e1->ts.kind > e2->ts.kind) ? e1->ts.kind : e2->ts.kind;
}


/* g95_numeric_ts()-- Returns nonzero if the typespec is a numeric
 * type, zero otherwise. */

int g95_numeric_ts(g95_typespec *ts) {

  return (ts->type == BT_COMPLEX || ts->type == BT_REAL ||
	  ts->type == BT_INTEGER);
}


/* g95_int_expr()-- Returns an expression node that is an integer
 * constant. */

g95_expr *g95_int_expr(int i) {
g95_expr *p;

  p = g95_get_expr();

  p->expr_type = EXPR_CONSTANT;
  p->ts.type = BT_INTEGER;
  p->ts.kind = g95_default_integer_kind();

  p->where = *g95_current_locus();
  mpz_init_set_si(p->value.integer, i);

  return p;
}


/* g95_logical_expr()-- Returns an expression node that is a logical
 * constant. */

g95_expr *g95_logical_expr(int i, locus *where) {
g95_expr *p;

  p = g95_get_expr();

  p->expr_type = EXPR_CONSTANT;
  p->ts.type = BT_LOGICAL;
  p->ts.kind = g95_default_logical_kind();

  if (where == NULL) where = g95_current_locus();
  p->where = *where;
  p->value.logical = i;

  return p;
}



/* g95_build_call()-- Build and return a g95_code structure that
 * corresponds to a subroutine call.  The arguments to this function
 * are a set of expression pointers (terminated by NULL) that compose
 * the actual arugment list. */

g95_code *g95_build_call(char *sub_name, ...) {
g95_actual_arglist *tail;
g95_expr *expr;
va_list argp;
g95_code *c; 

  c = g95_get_code();

  c->op = EXEC_CALL;
  c->sub_name = sub_name;

  tail = NULL;

  va_start(argp, sub_name);

  for(;;) {
    expr = va_arg(argp, g95_expr *);
    if (expr == NULL) break;

    if (c->ext.actual == NULL) {
      tail = g95_get_actual_arglist();
      c->ext.actual = tail;
    } else {
      tail->next = g95_get_actual_arglist();
      tail = tail->next;
    }

    tail->expr = expr;
  }

  va_end(argp);

  return c;
}


/* g95_build_funcall()-- Return an expression node with an optional
 * argument list attached.  A variable number of g95_expr pointers are
 * strung together in an argument list with a NULL pointer terminating
 * the list. */

g95_expr *g95_build_funcall(g95_symbol *func, ...) {
g95_actual_arglist *tail;
g95_expr *p, *q;
va_list argp;

  p = g95_get_expr();
  p->expr_type = EXPR_FUNCTION;
  p->symbol = func;
  p->value.function.actual = NULL;

  tail = NULL;

  va_start(argp, func);
  for(;;) {
    q = va_arg(argp, g95_expr *);
    if (q == NULL) break;

    if (tail == NULL) 
      p->value.function.actual = tail = g95_get_actual_arglist();
    else {
      tail->next = g95_get_actual_arglist();
      tail = tail->next;
    }

    tail->expr = q;
  }

  va_end(argp);

  return p;
}



/* g95_type_convert_binary()-- Given an expression node with some sort of
 * numeric binary expression, insert type conversions required to make
 * the operands have the same type.
 *
 * The exception is that the operands of an exponential don't have to
 * have the same type.  If possible, the base is promoted to the type
 * of the exponent.  For example, 1**2.3 becomes 1.0**2.3, but
 * 1.0**2 stays as it is. */

void g95_type_convert_binary(g95_expr *e) {
g95_expr *op1, *op2;

  op1 = e->op1;
  op2 = e->op2;

  if (op1->ts.type == BT_UNKNOWN || op2->ts.type == BT_UNKNOWN) {
    g95_clear_ts(&e->ts);
    return;
  }

/* Kind conversions */

  if (op1->ts.type == op2->ts.type) {

    if (op1->ts.kind == op2->ts.kind) {  /* No type conversions */
      e->ts = op1->ts;
      goto done;
    }

    if (op1->ts.kind > op2->ts.kind)
      g95_convert_type(op2, &op1->ts, 2);
    else
      g95_convert_type(op1, &op2->ts, 2);

    e->ts = op1->ts;
    goto done;
  }

/* Real and integer combined with complex */

  if (op1->ts.type == BT_COMPLEX &&
      (op2->ts.type == BT_REAL || op2->ts.type == BT_INTEGER)) {

    e->ts.type = BT_COMPLEX;
    e->ts.kind = op1->ts.kind;

    if (e->operator == INTRINSIC_POWER) goto done;

    g95_convert_type(e->op2, &e->ts, 2);
    goto done;
  }

  if (op2->ts.type == BT_COMPLEX &&
      (op1->ts.type == BT_REAL || op1->ts.type == BT_INTEGER)) {

    e->ts.type = BT_COMPLEX;
    e->ts.kind = op2->ts.kind;

    g95_convert_type(e->op1, &e->ts, 2);
    goto done;
  }

/* Integer combined with real */

  if (op1->ts.type == BT_REAL && op2->ts.type == BT_INTEGER) {
    e->ts.type = BT_REAL;
    e->ts.kind = op1->ts.kind;

    if (e->operator == INTRINSIC_POWER) goto done;

    g95_convert_type(e->op2, &e->ts, 2);
    goto done;
  }

  if (op1->ts.type == BT_INTEGER && op2->ts.type == BT_REAL) {
    e->ts.type = BT_REAL;
    e->ts.kind = op2->ts.kind;

    g95_convert_type(e->op1, &e->ts, 2);
    goto done;
  }

done:
  return;
}


/* g95_is_constant_expr()-- Function to determine if an expression is
 * constant or not.  This function expects that the expression has
 * already been simplified. */

int g95_is_constant_expr(g95_expr *e) {
g95_constructor *c;
int rv;

  if (e == NULL) return 1; 

  switch(e->expr_type) {
  case EXPR_OP:
  case EXPR_FUNCTION:
  case EXPR_VARIABLE:
    rv = 0; 
    break;

  case EXPR_CONSTANT:
  case EXPR_NULL:
    rv = 1;
    break;

  case EXPR_SUBSTRING:
    rv = g95_is_constant_expr(e->op1) && g95_is_constant_expr(e->op2);
    break;

  case EXPR_STRUCTURE:
    rv = 0;
    for(c=e->value.constructor; c; c=c->next)
      if (!g95_is_constant_expr(c->expr)) break;

    if (c == NULL) rv = 1;
    break;

  case EXPR_ARRAY:
    rv = g95_constant_ac(e);
    break;

  default:
    g95_internal_error("g95_is_constant_expr(): Unknown expression type");
  }

  return rv;
}


/* simplify_intrinsic_op()-- Try to collapse intrinsic expressions */

static try simplify_intrinsic_op(g95_expr *p, int type) {
g95_expr *op1, *op2, *result;

  if (p->operator == INTRINSIC_USER) return SUCCESS;

  op1 = p->op1;
  op2 = p->op2;

  if (g95_simplify_expr(op1, type) == FAILURE) return FAILURE;
  if (g95_simplify_expr(op2, type) == FAILURE) return FAILURE;

  if (!g95_is_constant_expr(op1) ||
      (op2 != NULL && !g95_is_constant_expr(op2)))
    return SUCCESS;

/* Rip p apart */

  p->op1 = NULL;
  p->op2 = NULL;

  switch(p->operator) {
  case INTRINSIC_UPLUS:
    result = g95_uplus(op1);
    break;

  case INTRINSIC_UMINUS:
    result = g95_uminus(op1);
    break;

  case INTRINSIC_PLUS:
    result = g95_add(op1, op2);
    break;

  case INTRINSIC_MINUS:
    result = g95_subtract(op1, op2);
    break;

  case INTRINSIC_TIMES:
    result = g95_multiply(op1, op2);
    break;

  case INTRINSIC_DIVIDE:
    result = g95_divide(op1, op2);
    break;

  case INTRINSIC_POWER:
    result = g95_power(op1, op2);
    break;

  case INTRINSIC_CONCAT:
    result = g95_concat(op1, op2);
    break;

  case INTRINSIC_EQ:      
    result = g95_eq(op1, op2);
    break;

  case INTRINSIC_NE:
    result = g95_ne(op1, op2);
    break;

  case INTRINSIC_GT:
    result = g95_gt(op1, op2);
    break;

  case INTRINSIC_GE:
    result = g95_ge(op1, op2);
    break;

  case INTRINSIC_LT:
    result = g95_lt(op1, op2);
    break;

  case INTRINSIC_LE:
    result = g95_le(op1, op2);
    break;

  case INTRINSIC_NOT:
    result = g95_not(op1);
    break;

  case INTRINSIC_AND:
    result = g95_and(op1, op2);
    break;

  case INTRINSIC_OR:
    result = g95_or(op1, op2);
    break;

  case INTRINSIC_EQV:
    result = g95_eqv(op1, op2);
    break;

  case INTRINSIC_NEQV:
    result = g95_neqv(op1, op2);
    break;

  default: g95_internal_error("simplify_intrinsic_op(): Bad operator"); 
  }

  if (result == NULL) return FAILURE;

  g95_replace_expr(p, result);

  return SUCCESS;
}


/* simplify_constructor()-- Subroutine to simplify constructor
 * expressions.  Mutually recursive with g95_simplify_expr(). */

static try simplify_constructor(g95_constructor *c, int type) {

  for( ;c; c=c->next) {
    if (c->iterator &&
	(g95_simplify_expr(c->iterator->start, type) == FAILURE ||
	 g95_simplify_expr(c->iterator->end, type) == FAILURE ||
	 g95_simplify_expr(c->iterator->step, type) == FAILURE))
      return FAILURE;

    if (c->expr && g95_simplify_expr(c->expr, type) == FAILURE) return FAILURE;
  }

  return SUCCESS;
}


/* g95_simplify_expr()-- Given an expression, simplify it by collapsing
 * constant expressions.  Most simplification takes place when the
 * expression tree is being constructed.  If an intrinsic function is
 * simplified at some point, we get called again to collapse the
 * result against other constants.
 *
 * We work by recursively simplifying expression nodes, simplifying
 * intrinsic functions where possible, which can lead to further
 * constant collapsing.  If an operator has constant operand(s), we
 * rip the expression apart, and rebuild it, hoping that it becomes
 * something simpler.
 *
 * The expression type is defined for:
 *   0   Basic expression parsing
 *   1   Simplifying array constructors-- will substitute iterator values
 */

try g95_simplify_expr(g95_expr *p, int type) {
g95_actual_arglist *ap;

  if (p == NULL) return SUCCESS;

/* Replace a parameter variable with its value */

  switch(p->expr_type) {
  case EXPR_CONSTANT:
  case EXPR_NULL:
    break;

  case EXPR_FUNCTION:
    for(ap=p->value.function.actual; ap; ap=ap->next) 
      if (g95_simplify_expr(ap->expr, type) == FAILURE) return FAILURE;

    if (p->value.function.isym != NULL &&
	g95_intrinsic_func_interface(p, 1) == MATCH_ERROR) return FAILURE;

    break;

  case EXPR_SUBSTRING:
    if (g95_simplify_expr(p->op1, type) == FAILURE ||
	g95_simplify_expr(p->op2, type) == FAILURE) return FAILURE;

/* TODO: evaluate constant substrings */

    break;

  case EXPR_OP:
    if (simplify_intrinsic_op(p, type) == FAILURE) return FAILURE;
    break;

  case EXPR_VARIABLE:
    if (p->symbol->attr.flavor == FL_PARAMETER &&
	p->symbol->value->expr_type != EXPR_ARRAY) {
      g95_replace_expr(p, g95_copy_expr(p->symbol->value));
      break;
    }

    if (type == 1) g95_simplify_iterator_var(p);

    break;

  case EXPR_STRUCTURE:
  case EXPR_ARRAY:
    if (simplify_constructor(p->value.constructor, type) == FAILURE)
      return FAILURE;

    if (type == 1 && p->expr_type == EXPR_ARRAY &&
	g95_expand_constructor(p) == FAILURE)
      return FAILURE;

    break;
  }

  return SUCCESS;
}


/* check_intrinsic_op()-- Check an intrinsic arithmetic operation to
 * see if it is consistent with some type of expression. */

static try check_init_expr(g95_expr *);

static try check_intrinsic_op(g95_expr *e, try (*check_function)(g95_expr *)) {
 
  if ((*check_function)(e->op1) == FAILURE) return FAILURE;

  switch(e->operator) {
  case INTRINSIC_UPLUS:
  case INTRINSIC_UMINUS:
    if (!g95_numeric_ts(&e->op1->ts)) goto not_numeric;
    break;

  case INTRINSIC_EQ:  case INTRINSIC_NE:  case INTRINSIC_GT:
  case INTRINSIC_GE:  case INTRINSIC_LT:  case INTRINSIC_LE:

  case INTRINSIC_PLUS:    case INTRINSIC_MINUS:  case INTRINSIC_TIMES:
  case INTRINSIC_DIVIDE:  case INTRINSIC_POWER:
    if ((*check_function)(e->op2) == FAILURE) return FAILURE;

    if (g95_numeric_ts(&e->op1->ts) == 0 ||
	g95_numeric_ts(&e->op2->ts) == 0) goto not_numeric;

    if (e->operator != INTRINSIC_POWER) break;
    
    if (check_function == check_init_expr &&
	e->op2->ts.type != BT_INTEGER) {
      g95_error("Exponent at %L must be INTEGER for an initialization "
		"expression", &e->op2->where);
      return FAILURE;
    }

    break;

  case INTRINSIC_CONCAT:
    if ((*check_function)(e->op2) == FAILURE) return FAILURE;

    if (e->op1->ts.type != BT_CHARACTER || e->op2->ts.type != BT_CHARACTER) {
      g95_error("Concatenation operator in expression at %L "
		"must have two CHARACTER operands", &e->op1->where);
      return FAILURE;
    }

    if (e->op1->ts.kind != e->op2->ts.kind) {
      g95_error("Concat operator at %L must concatenate strings of the "
		"same kind", &e->where);
      return FAILURE;
    }

    break;

  case INTRINSIC_NOT:
    if (e->op1->ts.type != BT_LOGICAL) {
      g95_error(".NOT. operator in expression at %L must have a LOGICAL "
		"operand", &e->op1->where);
      return FAILURE;
    }

    break;

  case INTRINSIC_AND:    case INTRINSIC_OR:
  case INTRINSIC_EQV:    case INTRINSIC_NEQV:
    if ((*check_function)(e->op2) == FAILURE) return FAILURE;

    if (e->op1->ts.type != BT_LOGICAL || e->op2->ts.type != BT_LOGICAL) {
      g95_error("LOGICAL operands are required in expression at %L",
		&e->where);
      return FAILURE;
    }

    break;

  default:
    g95_error("Only intrinsic operators can be used in expression at %L",
	      &e->where);
    return FAILURE;
  }

  return SUCCESS;

not_numeric:
  g95_error("Numeric operands are required in expression at %L", &e->where);

  return FAILURE;
}



/* check_numeric_inquiry()-- Numeric inquiry functions are
 * specifically allowed to have variable arguments, which is an
 * exception to the normal requirement that an initialization function
 * have initialization arguments.  We head off this problem here.  */

static try check_numeric_inquiry(g95_expr *e) {
char *name;
static const char *inquiry_function[] = {
  "digits", "epsilon", "huge", "kind", "maxexponent", "minexponent",
  "precision", "radix", "range", "tiny", "bit_size", NULL
};

int i;

  if (e->value.function.actual == NULL ||
      e->value.function.actual->next != NULL)  /* Doesn't have one arg */
    return FAILURE;

  if (e->value.function.name != NULL &&
      e->value.function.name[0] != '\0') return FAILURE;

  name = e->symbol->name;

  for(i=0; inquiry_function[i]; i++)
    if (strcmp(inquiry_function[i], name) == 0) break;

  if (inquiry_function[i] == NULL) return FAILURE;

  e = e->value.function.actual->expr;

  if (e == NULL || e->expr_type != EXPR_VARIABLE) return FAILURE;

  return SUCCESS;
}


/* check_init_expr()-- Verify that an expression is an
 * initialization expression.  A side effect is that the expression
 * tree is reduced to a single constant node if all goes well.  This
 * would normally happen when the expression is constructed but
 * function references are assumed to be intrinsics in the context of
 * initialization expressions.  If FAILURE is returned an error
 * message has been generated. */

static try check_init_expr(g95_expr *e) {
g95_actual_arglist *ap;
match m;
try t;

  if (e == NULL) return SUCCESS;

  switch(e->expr_type) {
  case EXPR_OP:
    t = check_intrinsic_op(e, check_init_expr);
    if (t == SUCCESS) t = g95_simplify_expr(e, 0);

    break;

  case EXPR_FUNCTION:
    t = SUCCESS;

    if (check_numeric_inquiry(e) != SUCCESS) {
      t = SUCCESS;
      for(ap=e->value.function.actual; ap; ap=ap->next)
	if (check_init_expr(ap->expr) == FAILURE) {
	  t = FAILURE;
	  break;
	}
    }

    if (t == SUCCESS) {
      m = g95_intrinsic_func_interface(e, 0);

      if (m == MATCH_NO)
	g95_error("Function '%s' in initialization expression at %L "
		  "must be an intrinsic function", e->symbol->name, &e->where);

      if (m != MATCH_YES) t = FAILURE;
    }

    break;

  case EXPR_VARIABLE:
    t = SUCCESS;

    if (g95_check_iter_variable(e) == SUCCESS) break;

    if (e->symbol->attr.flavor == FL_PARAMETER) {
      g95_replace_expr(e, g95_copy_expr(e->symbol->value));
      break;      /* TODO: constant references to subobjects */
    }

    g95_error("Variable '%s' at %L cannot appear in an initialization "
	      "expression", e->symbol->name, &e->where);
    t = FAILURE;
    break;

  case EXPR_CONSTANT:
  case EXPR_NULL:
    t = SUCCESS;
    break;

  case EXPR_SUBSTRING:
    t = check_init_expr(e->op1);
    if (t == FAILURE) break;

    t = check_init_expr(e->op2);
    if (t == SUCCESS) t = g95_simplify_expr(e, 0);

    break;

  case EXPR_STRUCTURE:
    t = g95_check_constructor(e, check_init_expr);
    break;

  case EXPR_ARRAY:
    t = g95_check_constructor(e, check_init_expr);
    if (t == FAILURE) break;

    t = g95_expand_constructor(e);
    if (t == FAILURE) break;

    t = g95_check_constructor_type(e);
    break;
    
  default:
    g95_internal_error("check_init_expr(): Unknown expression type");
  }

  return t;
}


/* g95_match_init_expr()-- Match an initialization expression.  We work
 * by first matching an expression, then reducing it to a constant */

match g95_match_init_expr(g95_expr **result) {
g95_expr *expr;
match m;
try t;

  m = g95_match_expr(&expr);
  if (m != MATCH_YES) return m;

  g95_init_expr = 1;
  t = check_init_expr(expr);
  g95_init_expr = 0;

  if (t == FAILURE) {
    g95_free_expr(expr);
    return MATCH_ERROR;
  }

  if (expr->expr_type == EXPR_ARRAY &&
      (g95_check_constructor_type(expr) == FAILURE ||
       g95_expand_constructor(expr) == FAILURE)) {
    g95_free_expr(expr);
    return MATCH_ERROR;
  }

  if (!g95_is_constant_expr(expr))
    g95_internal_error("Initialization expression didn't reduce %C");

  *result = expr;

  return MATCH_YES;
}



static try check_restricted(g95_expr *);

/* restricted_args()-- Given an actual argument list, test to see that
 * each argument is a restricted expression and optionally if the
 * expression type is integer or character */

static try restricted_args(g95_actual_arglist *a, int check_type) {
bt type;

  for(; a; a=a->next) {
    if (check_restricted(a->expr) == FAILURE) return FAILURE;

    if (!check_type) continue;

    type = a->expr->ts.type;
    if (type != BT_CHARACTER && type != BT_INTEGER) {
      g95_error("Function argument at %L must be of type INTEGER or CHARACTER",
		&a->expr->where);
      return FAILURE;
    }
  }

  return SUCCESS;
}


/************* Restricted/specification expressions *************/


/* external_spec_function()-- Make sure a non-intrinsic function is a
 * specification function. */

static try external_spec_function(g95_expr *e) {
g95_symbol *f;

  f = e->value.function.esym;

  if (f->attr.proc == PROC_ST_FUNCTION) {
    g95_error("Specification function '%s' at %L cannot be a statement "
	      "function", f->name, &e->where);
    return FAILURE;
  }

  if (f->attr.proc == PROC_INTERNAL) {
    g95_error("Specification function '%s' at %L cannot be an internal "
	      "function", f->name, &e->where);
    return FAILURE;
  }

  if (!f->attr.pure) {
    g95_error("Specification function '%s' at %L must be PURE", f->name,
	      &e->where);
    return FAILURE;
  }

  if (f->attr.recursive) {
    g95_error("Specification function '%s' at %L cannot be RECURSIVE",
	      f->name, &e->where);
    return FAILURE;
  }

  return restricted_args(e->value.function.actual, 0);
}


/* restricted_intrinsic()-- Check to see that a function reference to
 * an intrinsic is a restricted expression.  Some functions required
 * by the standard are omitted because references to them have already
 * been simplified.  Strictly speaking, a lot of these checks are
 * redundant with other checks.  If a function is indeed a particular
 * intrinsic, then the type of its argument have already been checked
 * and passed. */

static try restricted_intrinsic(g95_expr *e) {
g95_intrinsic_sym *sym;

static struct { const char *name; int case_number; } *cp, cases[] = {
  { "repeat", 0 },             { "reshape", 0 },  { "selected_int_kind", 0 },
  { "selected_real_kind", 0 }, { "transfer", 0 }, { "trim", 0 },

  { "null", 1 },

  { "lbound", 2 }, { "shape", 2 }, { "size", 2 }, { "ubound", 2 },
  
  /* bit_size() has already been reduced */

  { "len", 0 },

  /* kind() has already been reduced */
  /* Numeric inquiry functions have been reduced */

  { NULL, 0 } };

try t;

  sym = e->value.function.isym;
  if (sym->elemental) return restricted_args(e->value.function.actual, 1);

  for(cp=cases; cp->name; cp++)
    if (strcmp(cp->name, sym->name) == 0) break;

  if (cp->name == NULL) {
    g95_error("Intrinsic function '%s' at %L is not a restricted function",
	      sym->name, &e->where);
    return FAILURE;
  }

  switch(cp->case_number) {
  case 0:
    /* Functions that are restricted if they have character/integer args */

    t = restricted_args(e->value.function.actual, 1);
    break;

  case 1:  /* NULL() */
    t = SUCCESS;
    break;

  case 2:
    /* Functions that could be checking the bounds of an assumed-size array */

    t = SUCCESS;     /* TODO: implement checks from 7.1.6.2 (10) */
    break;

  default:
    g95_internal_error("restricted_intrinsic(): Bad case");
  }

  return t;
}


/* check_restricted()-- Verify that an expression is a restricted
 * expression.  Like its cousin check_init_expr(), an error message is
 * generated if we return FAILURE. */

static try check_restricted(g95_expr *e) {
g95_symbol *sym;
try t;

  if (e == NULL) return SUCCESS;

  switch(e->expr_type) {
  case EXPR_OP:
    t = check_intrinsic_op(e, check_restricted);
    if (t == SUCCESS) t = g95_simplify_expr(e, 0);

    break;

  case EXPR_FUNCTION:
    t = e->value.function.esym ?
      external_spec_function(e) :  restricted_intrinsic(e);

    break;

  case EXPR_VARIABLE:
    sym = e->symbol;
    t = FAILURE;
    
    if (sym->attr.optional) {
      g95_error("Dummy argument '%s' at %L cannot be OPTIONAL",
		sym->name, &e->where);
      break;
    }

    if (sym->attr.intent == INTENT_OUT) {
      g95_error("Dummy argument '%s' at %L cannot be INTENT(OUT)",
		sym->name, &e->where);
      break;
    }

    if (sym->attr.in_common || sym->attr.use_assoc || sym->attr.dummy ||
	sym->ns != g95_current_ns ||
	(sym->ns->proc_name != NULL &&
	 sym->ns->proc_name->attr.flavor == FL_MODULE)) {
      t = SUCCESS;
      break;
    }

    g95_error("Variable '%s' cannot appear in the expression at %L",
	      sym->name, &e->where);

    break;

  case EXPR_NULL:
  case EXPR_CONSTANT:
    t = SUCCESS;
    break;

  case EXPR_SUBSTRING:
    t = g95_specification_expr(e->op1);
    if (t == FAILURE) break;

    t = g95_specification_expr(e->op2);
    if (t == SUCCESS) t = g95_simplify_expr(e, 0);

    break;

  case EXPR_STRUCTURE:
    t = g95_check_constructor(e, check_restricted);
    break;

  case EXPR_ARRAY:
    t = g95_check_constructor(e, check_restricted);
    break;

  default:
    g95_internal_error("check_spec_expr(): Unknown expression type");
  }

  return t;
}


/* g95_check_spec_expr()-- Check to see that an expression is a
 * specification expression.  If we return FAILURE, an error has been
 * generated. */

try g95_specification_expr(g95_expr *e) {

  if (e->ts.type != BT_INTEGER) {
    g95_error("Expression at %L must be of INTEGER type", &e->where);
    return FAILURE;
  }

  if (e->rank != 0) {
    g95_error("Expression at %L must be scalar", &e->where);
    return FAILURE;
  }

  return check_restricted(e);
}


/* show_ref()-- Show a string of g95_ref structures. */

static void show_ref(g95_ref *p) {

  for(; p; p=p->next)
    switch(p->type) {
    case REF_ARRAY:
      g95_show_array_ref(&p->u.ar);
      break;

    case REF_COMPONENT:
      g95_status(" %% %s", p->u.c.component->name);
      break;

    case REF_SUBSTRING:
      g95_status_char('(');
      g95_show_expr(p->u.ss.start);
      g95_status_char(':');
      g95_show_expr(p->u.ss.end);
      g95_status_char(')');
      break;

    default:
      g95_internal_error("show_ref(): Bad component code");
    }
}


/* show_constructor()-- Display a constructor.  Works recursively for
 * array constructors. */

static void show_constructor(g95_constructor *c) {

  for(;c ;c=c->next) {
    if (c->iterator == NULL)
      g95_show_expr(c->expr);
    else {
      g95_status_char('(');
      g95_show_expr(c->expr);

      g95_status_char(' ');
      g95_show_expr(c->iterator->var);
      g95_status_char('=');
      g95_show_expr(c->iterator->start);
      g95_status_char(',');
      g95_show_expr(c->iterator->end);
      g95_status_char(',');
      g95_show_expr(c->iterator->step);

      g95_status_char(')');
    }

    if (c->next != NULL) g95_status(" , ");
  }
}


/* g95_show_expr()-- show an expression */

void g95_show_expr(g95_expr *p) {
char *c;
int i;

  if (p == NULL) {
    g95_status("()");
    return;
  }

/* Show expression */

  switch(p->expr_type) {
  case EXPR_SUBSTRING:
    c = p->value.character.string;

    for(i=0; i<p->value.character.length; i++,c++) {
      if (*c == '\'') g95_status("''");
      else g95_status("%c", *c);
    }

    show_ref(p->ref);
    break;

  case EXPR_STRUCTURE:
    g95_status("%s(", p->symbol->name);
    show_constructor(p->value.constructor);
    g95_status_char(')');
    break;

  case EXPR_ARRAY:
    g95_status("(/ ");
    show_constructor(p->value.constructor);
    g95_status(" /)");

    show_ref(p->ref);
    break;

  case EXPR_NULL:
    g95_status("NULL()");
    break;

  case EXPR_CONSTANT:
    switch(p->ts.type) {
    case BT_INTEGER:
      mpz_out_str(stdout, 10, p->value.integer);

      if (p->ts.kind != g95_default_integer_kind())
	g95_status("_%d", p->ts.kind);
      break;

    case BT_LOGICAL:
      if (p->value.logical) g95_status(".true.");
       else g95_status(".false.");
      break;

    case BT_REAL:
      mpf_out_str(stdout, 10, 0, p->value.real);
      if (p->ts.kind != g95_default_real_kind())
	g95_status("_%d", p->ts.kind);
      break;

    case BT_CHARACTER:
      c = p->value.character.string;

      g95_status_char('\'');

      for(i=0; i<p->value.character.length; i++,c++) {
	if (*c == '\'') g95_status("''");
	else g95_status_char(*c);
      }

      g95_status_char('\'');

      break;

    case BT_COMPLEX:
      g95_status("(complex ");

      mpf_out_str(stdout, 10, 0, p->value.complex.r);
      if (p->ts.kind != g95_default_complex_kind())
	g95_status("_%d", p->ts.kind);

      g95_status(" ");

      mpf_out_str(stdout, 10, 0, p->value.complex.i);
      if (p->ts.kind != g95_default_complex_kind())
	g95_status("_%d", p->ts.kind);

      g95_status(")");
      break;

    default:
      g95_status("???");
      break;
    }

    break;

  case EXPR_VARIABLE:
    g95_status("%s", p->symbol->name);
    show_ref(p->ref);
    break;

  case EXPR_OP:
    g95_status("(");
    switch(p->operator) {
    case INTRINSIC_UPLUS:   g95_status("U+ ");    break;
    case INTRINSIC_UMINUS:  g95_status("U- ");    break;
    case INTRINSIC_PLUS:    g95_status("+ ");     break;
    case INTRINSIC_MINUS:   g95_status("- ");     break;
    case INTRINSIC_TIMES:   g95_status("* ");     break;
    case INTRINSIC_DIVIDE:  g95_status("/ ");     break;
    case INTRINSIC_POWER:   g95_status("** ");    break;
    case INTRINSIC_CONCAT:  g95_status("// ");    break;
    case INTRINSIC_AND:     g95_status("AND ");   break;
    case INTRINSIC_OR:      g95_status("OR ");    break;
    case INTRINSIC_EQV:     g95_status("EQV ");   break;
    case INTRINSIC_NEQV:    g95_status("NEQV ");  break;
    case INTRINSIC_EQ:      g95_status("= ");     break;
    case INTRINSIC_NE:      g95_status("<> ");    break;
    case INTRINSIC_GT:      g95_status("> ");     break;
    case INTRINSIC_GE:      g95_status(">= ");    break;
    case INTRINSIC_LT:      g95_status("< ");     break;
    case INTRINSIC_LE:      g95_status("<= ");    break;
    case INTRINSIC_NOT:     g95_status("NOT ");   break;

    default:
      g95_internal_error("g95_show_expr(): Bad intrinsic in expression!");
    }

    g95_show_expr(p->op1);

    if (p->op2) {
      g95_status(" ");
      g95_show_expr(p->op2);
    }

    g95_status(")");
    break;

  case EXPR_FUNCTION:
    if (p->value.function.name == NULL) {
      g95_status("%s[", p->symbol->name);
      g95_show_actual_arglist(p->value.function.actual);
      g95_status_char(']');
    } else {
      g95_status("%s[[", p->value.function.name);
      g95_show_actual_arglist(p->value.function.actual);
      g95_status_char(']');
      g95_status_char(']');
    }

    break;

  default: g95_internal_error("g95_show_expr(): Don't know how to show expr");
  }
}
