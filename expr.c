/* Expression subroutines
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
    g95_show_expr(a->expr);
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

    for(i=0; i<G95_MAX_DIMENSIONS; i++) {
      g95_free_expr(p->ar.start[i]);
      g95_free_expr(p->ar.end[i]);
      g95_free_expr(p->ar.stride[i]);
    }

    g95_free_expr(p->start);
    g95_free_expr(p->end);

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

  if (e->shape != NULL) g95_free_array_shape(e->shape);

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
  *dest = *src;

  ar = g95_copy_array_ref(&src->ar);
  dest->ar = *ar;
  g95_free(ar);

  dest->start = g95_copy_expr(src->start);
  dest->end = g95_copy_expr(src->end);
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

  if (p->shape != NULL) q->shape = g95_copy_array_shape(p->shape);

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
  p->where = *g95_current_locus();
  p->value.logical = i;

  return p;
}



/* g95_build_call()-- Build and return a g95_code structure that
 * corresponds to a subroutine call.  The arguments to this function
 * are a set of expression pointers (terminated by NULL) that compose
 * the actual arugment list. */

g95_code *g95_build_call(g95_symbol *subr, ...) {
g95_actual_arglist *tail;
g95_expr *expr;
va_list argp;
g95_code *c; 

  c = g95_get_code();

  c->op = EXEC_CALL;
  c->sym = subr;

  tail = NULL;

  va_start(argp, subr);

  for(;;) {
    expr = va_arg(argp, g95_expr *);
    if (expr == NULL) break;

    if (c->ext.arglist == NULL) {
      tail = g95_get_actual_arglist();
      c->ext.arglist = tail;
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


/* is_constant_constructor()-- Recursively checks all elements of a
 * constructor to see if everything is constant. */

static int is_constant_expr(g95_expr *);

static int is_constant_constructor(g95_constructor *c) {

  if (c == NULL) return 1;

  for(; c; c=c->next) {
    if (!is_constant_expr(c->expr)) return 0;

    if (c->iterator != NULL) {
      if (!is_constant_expr(c->iterator->start)) return 0;
      if (!is_constant_expr(c->iterator->end))   return 0;
      if (!is_constant_expr(c->iterator->step))  return 0;
    }
  }

  return 1;
}


/* is_constant_expr()-- Function to determine if an expression is
 * constant or not.  This function expects that the expression has
 * already been simplified.  Mutually recursive with
 * constant_constructor().  */

static int is_constant_expr(g95_expr *e) {
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
    rv = is_constant_expr(e->op1) && is_constant_expr(e->op2);
    break;

  case EXPR_STRUCTURE:
  case EXPR_ARRAY:
    rv = is_constant_constructor(e->value.constructor);
    break;

  default:
    g95_internal_error("is_constant_expr(): Unknown expression type");
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

  if (!is_constant_expr(op1) || (op2 != NULL && !is_constant_expr(op2)))
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

  default: g95_internal_error("g95_simplify_expr(): Impossible operator"); 
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

    if (g95_intrinsic_func_interface(p, 0) == MATCH_ERROR) 
        return FAILURE;
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
    if (p->symbol->attr.flavor == FL_PARAMETER) {
      g95_replace_expr(p, g95_copy_expr(p->symbol->value));
      break;
    }

    if (type == 1) g95_simplify_iterator_var(p);

    break;

  case EXPR_STRUCTURE:
  case EXPR_ARRAY:
    if (simplify_constructor(p->value.constructor, type) == FAILURE)
      return FAILURE;

    if (type == 1 && g95_expand_constructor(p) == FAILURE)
      return FAILURE;

    break;
  }

  return SUCCESS;
}


/****************** Expression name resolution ******************/


/* procedure_kind()-- Figure out if the procedure is specific, generic
 * or unknown. */

typedef enum { PTYPE_GENERIC=1, PTYPE_SPECIFIC, PTYPE_UNKNOWN } proc_type;


proc_type procedure_kind(g95_symbol *sym) {
g95_symbol *s;
char *name;

  name = sym->name;

  /* Locate symbol in the nearest parent scope */

  s = NULL;
  if (sym->ns->parent != NULL) g95_find_symbol(name, sym->ns->parent, 1, &s);

  /* See if a symbol is generic */

  if (sym->attr.generic ||
      (sym->attr.intrinsic && g95_generic_intrinsic(name)))
    return PTYPE_GENERIC;

  g95_find_symbol(name, sym->ns->parent, 1, &s);

  if (s != NULL && (s->attr.generic ||
		    (s->attr.intrinsic && g95_generic_intrinsic(name))))
    return PTYPE_GENERIC;

  /* Not generic, see if it is specific */

  if (sym->attr.interface || sym->attr.proc == PROC_MODULE ||
      sym->attr.proc == PROC_INTERNAL || sym->attr.proc == PROC_ST_FUNCTION ||
      (sym->attr.intrinsic && g95_specific_intrinsic(name)) ||
      sym->attr.external)
    return PTYPE_SPECIFIC;

  /* Check parent scopes */

  if (s != NULL && (s->attr.interface || s->attr.proc == PROC_MODULE ||
		    s->attr.proc == PROC_INTERNAL ||
		    s->attr.proc == PROC_ST_FUNCTION ||
		    (s->attr.intrinsic && g95_specific_intrinsic(name)) ||
		    s->attr.external))
    return PTYPE_SPECIFIC;

  return PTYPE_UNKNOWN;
}


/* resolve_generic()-- Resolve a procedure call known to be generic.
 * Section 14.1.2.4.1. */

static match resolve_generic0(g95_expr *expr, g95_symbol *sym) {
g95_symbol *s;

  if (sym->attr.generic) {
    s = g95_search_interface(sym->generic, expr->value.function.actual);
    if (s != NULL) {
      expr->value.function.name = s->name;
      expr->ts = s->ts;
      return MATCH_YES;
    }

    /* TODO: Need to search for elemental references in generic interface */
  }

  if (sym->attr.intrinsic) return g95_intrinsic_func_interface(expr, 0);

  return MATCH_NO;
}


static try resolve_generic(g95_expr *expr) {
g95_symbol *sym;
match m;

  sym = expr->symbol;

  m = resolve_generic0(expr, sym);
  if (m == MATCH_YES) return SUCCESS;
  if (m == MATCH_ERROR) return FAILURE;

  if (sym->ns->parent != NULL) {
    g95_find_symbol(sym->name, sym->ns->parent, 1, &sym);
    if (sym != NULL) {
      m = resolve_generic0(expr, sym);
      if (m == MATCH_YES) return SUCCESS;
      if (m == MATCH_ERROR) return FAILURE;
    }
  }

  /* Last ditch attempt */

  if (!g95_generic_intrinsic(expr->symbol->name)) {
    g95_error("Generic function '%s' at %L is not a generic intrinsic "
	      "function", expr->symbol->name, &expr->where);
    return FAILURE;
  }

  m = g95_intrinsic_func_interface(expr, 0);
  if (m == MATCH_YES) return SUCCESS;
  if (m == MATCH_ERROR) return FAILURE;

  g95_error("Generic function '%s' at %L is not consistent with a specific "
	    "intrinsic interface", expr->symbol->name, &expr->where);

  return FAILURE;
}


/* resolve_specific()-- Resolve a procedure call known to be specific */

static match resolve_specific0(g95_symbol *sym, g95_expr *expr) {

  if (sym->attr.external || sym->attr.interface) {
    if (sym->attr.dummy) {
      sym->attr.proc = PROC_DUMMY;
      goto found;
    }

    sym->attr.proc = PROC_EXTERNAL;
    goto found;
  }

  if (sym->attr.proc == PROC_MODULE || sym->attr.proc == PROC_ST_FUNCTION ||
      sym->attr.proc == PROC_INTERNAL) goto found;

  return g95_intrinsic_func_interface(expr, 0);

found:
  expr->ts = sym->ts;
  expr->value.function.name = sym->name;
  return MATCH_YES;
}


static try resolve_specific(g95_expr *expr) {
g95_symbol *sym;
match m;

  sym = expr->symbol;

  m = resolve_specific0(sym, expr);
  if (m == MATCH_YES) return SUCCESS;
  if (m == MATCH_ERROR) return FAILURE;

  g95_find_symbol(sym->name, sym->ns->parent, 1, &sym);

  if (sym != NULL) {
    m = resolve_specific0(sym, expr);
    if (m == MATCH_YES) return SUCCESS;
    if (m == MATCH_ERROR) return FAILURE;
  }

  g95_error("Unable to resolve the specific function '%s' at %L",
	    expr->symbol->name, &expr->where);

  return SUCCESS;
}


/* resolve_unknown()-- Resolve a procedure call not known to be
 * generic nor specific */

static try resolve_unknown(g95_expr *expr) {
g95_symbol *sym;
g95_typespec ts;

  sym = expr->symbol; 

  if (sym->attr.dummy) {
    sym->attr.proc = PROC_DUMMY;
    expr->value.function.name = sym->name;
    goto set_type;
  }

  /* See if we have an intrinsic function reference */

  if (g95_intrinsic_name(sym->name, 0) && 
      g95_intrinsic_func_interface(expr, 0) == MATCH_YES)
    return SUCCESS;

  /* The reference is to an external name */

  sym->attr.proc = PROC_EXTERNAL;
  expr->value.function.name = sym->name;

  /* Type of the expression is either the type of the symbol or the
   * default type of the symbol */

 set_type:

  if (sym->ts.type != BT_UNKNOWN)
    expr->ts = sym->ts;
  else {
    ts = sym->ns->default_type[sym->name[0] - 'a'];

    if (ts.type == BT_UNKNOWN)
      g95_error("Function '%s' at %L has no implicit type",
		sym->name, &expr->where);
    else
      expr->ts = ts;
  }

  return SUCCESS;
}


/* resolve_function()-- Resolve a function call, which means resolving
 * the arguments, then figuring out which entity the name refers to. */

static try resolve_function(g95_expr *expr) {
g95_actual_arglist *arg;
try t;

  arg = expr->value.function.actual;
  t = SUCCESS;

  while(arg) {
    if (g95_resolve_expr(arg->expr) != SUCCESS) t = FAILURE;
    arg = arg->next;
  }

  if (t == FAILURE) return FAILURE;

/* See if function is already resolved */
  if (expr->value.function.name != NULL) {
    if (expr->ts.type == BT_UNKNOWN) expr->ts = expr->symbol->ts;
    return SUCCESS;
  }

/* Apply the rules of section 14.1.2 */

  switch(procedure_kind(expr->symbol)) {
  case PTYPE_GENERIC:   t = resolve_generic(expr);   break;
  case PTYPE_SPECIFIC:  t = resolve_specific(expr);  break;
  case PTYPE_UNKNOWN:   t = resolve_unknown(expr);   break;
  default:
    g95_internal_error("resolve_function(): bad function type");
  }

  return t;
}


/* namespace_kind()-- Given a namespace, figure out what kind it is.
 * We return one of the g95_compile_state enums COMP_NONE,
 * COMP_MODULE, COMP_SUBROUTINE or COMP_FUNCTION. */

static g95_compile_state namespace_kind(g95_namespace *ns) {
g95_symbol *sym;

  sym = ns->proc_name;

  if (sym == NULL) return COMP_NONE;

  if (sym->attr.flavor == FL_MODULE) return COMP_MODULE;

  if (sym->attr.subroutine) return COMP_SUBROUTINE;

  if (sym->attr.flavor == FL_VARIABLE ||
      sym->attr.function) return COMP_FUNCTION;

  return COMP_NONE;
}


/* resolve_formal_arglist()-- Resolve types of formal argument lists.
 * These have to be done first so that the formal argument lists of
 * module procedures can be copied to the containing module before the
 * individual procedures are resolved individually.  Since a dummy
 * argument cannot be a non-dummy procedure, the only resort left for
 * untyped names are the IMPLICIT types. */

static try resolve_formal_arglist(g95_formal_arglist *f) {
g95_symbol *sym;
try t;

  t = SUCCESS;

  for(; f; f=f->next) {
    sym = f->sym;
    if (sym->ts.type != BT_UNKNOWN) continue;
    if (g95_set_default_type(sym, 1, sym->ns) != SUCCESS) t = FAILURE;
  }

  return t;
}


/* resolve_entry_arglists()-- Recursive function to seek out ENTRY
 * symbols and resolve their formal argument lists. */

static void resolve_entry_arglists(g95_symbol *sym) {

  if (sym->attr.entry) resolve_formal_arglist(sym->formal);
}


/* resolve_formal_arglists()-- Given a namespace, resolve all formal
 * argument lists within the namespace.  The most obvious is the name
 * of the procedure.  The argument lists of all the ENTRY symbols must
 * also be processed. */

static try resolve_formal_arglists(g95_namespace *ns) {

  if (ns == NULL) return SUCCESS;

  if (ns->proc_name != NULL &&
      resolve_formal_arglist(ns->proc_name->formal) == FAILURE)
    return FAILURE;

  g95_traverse_ns(ns, resolve_entry_arglists);

  /* Recursively resolve child namespaces */

  for(ns=ns->contained; ns; ns=ns->sibling)
    if (resolve_formal_arglists(ns) == FAILURE)
      return FAILURE;

  return SUCCESS;
}


/* g95_resolve_modproc()-- Resolve module procedure types.  Because
 * module procedures can call one another, function types have to be
 * worked out before any of the contained procedures can be resolved.
 * If a function in a module procedure doesn't already have a type,
 * the only way it can get one is through an IMPLICIT. */

void g95_resolve_modproc(g95_namespace *ns) {
g95_symbol *sym_upper, *sym_lower;
g95_namespace *child;

  resolve_formal_arglists(ns);

  if (namespace_kind(ns) != COMP_MODULE) return;

  for(child=ns->contained; child; child=child->sibling) {
    if (namespace_kind(child) != COMP_FUNCTION) continue;

    sym_lower = child->proc_name;

    g95_find_symbol(sym_lower->name, ns, 0, &sym_upper);

    if (sym_upper == NULL)
      g95_internal_error("g95_resolve_modproc(): Module procedure not found");

    if (sym_lower->result != NULL) sym_lower = sym_lower->result;

    if (sym_lower->ts.type == BT_UNKNOWN)
      g95_set_default_type(sym_lower, 1, child);

    sym_upper->ts = sym_lower->ts;
  }
}


/* resolve_ref()-- Resolve subtype references */

static try resolve_ref(g95_expr *expr) {
g95_array_spec *as;
g95_ref *ref;
try t;

  as = expr->symbol->as;
  t = SUCCESS;

  for(ref=expr->ref; ref; ref=ref->next)
    switch(ref->type) {
    case REF_ARRAY:
      if (g95_resolve_array_ref(&ref->ar, as) == FAILURE) t = FAILURE;
      as = NULL;
      break;

    case REF_COMPONENT:
      as = ref->component->as;   /* In case an array ref is next */
      break;

    case REF_SUBSTRING:
      if (g95_resolve_expr(ref->start) == FAILURE) t = FAILURE;

      if (ref->start != NULL && ref->start->ts.type != BT_INTEGER) {
	g95_error("Substring index at %C must be of type INTEGER",
		  &ref->start->where);
	t = FAILURE;
      }

      if (g95_resolve_expr(ref->end) == FAILURE) t = FAILURE;

      if (ref->end != NULL && ref->end->ts.type != BT_INTEGER) {
	g95_error("Substring index at %C must be of type INTEGER",
		  &ref->end->where);
	t = FAILURE;
      }

      break;
    }

  return t;
}


/* resolve_structure_cons()-- Resolve all of the elements of a
 * structure constructor and make sure that the types are correct. */

static try resolve_structure_cons(g95_expr *expr) {
g95_constructor *cons;
g95_component *comp;
try t;

  t = SUCCESS;
  cons = expr->value.constructor;
  comp = expr->symbol->components;

  for(; comp; comp=comp->next, cons=cons->next) {

    if (g95_resolve_expr(cons->expr) == FAILURE) {
      t = FAILURE;
      continue;
    }

/* Make sure we've got the right type */

    if (cons->expr->ts.type != comp->ts.type) {
      g95_error("Expected type %s in structure constructor at %L",
		g95_typename(comp->ts.type), &cons->expr->where);
      t = FAILURE;
      continue;
    }

    if (cons->expr->ts.type == BT_DERIVED) {
      if (cons->expr->ts.derived != comp->ts.derived) {
	g95_error("Expected DERIVED type %s in structure constructor at %L",
		  comp->ts.derived->name, &cons->expr->where);
	t = FAILURE;
      }
    } else {
      if (cons->expr->ts.kind != comp->ts.kind) {
	g95_error("Expected %s kind %d in structure constructor at %L",
		  g95_typename(comp->ts.type), comp->ts.kind,
		  &cons->expr->where);
	t = FAILURE;
      }
    }
  }

  return t;
}


/* g95_resolve_expr()-- Resolve an expression.  That is, make sure
 * that types of operands agree with their operators, intrinsic
 * operators are converted to function calls for overloaded types and
 * unresolved function references are resolved. */

try g95_resolve_expr(g95_expr *e) {
g95_expr *op1, *op2;
const char *msg;
try t;

  if (e == NULL) return SUCCESS;

  t = SUCCESS;
  if (e->ref && resolve_ref(e) == FAILURE) t = FAILURE;

  switch(e->expr_type) {
  case EXPR_OP:
    break;

  case EXPR_FUNCTION:
    return resolve_function(e);

  case EXPR_VARIABLE:
    if (e->ts.type == BT_UNKNOWN) g95_variable_attr(e, &e->ts);

    /* Fall through */

  case EXPR_SUBSTRING:
  case EXPR_CONSTANT:
  case EXPR_NULL:
    return t;

  case EXPR_ARRAY:
    if (g95_resolve_array_constructor(e) == FAILURE) t = FAILURE;
    return t;

  case EXPR_STRUCTURE:
    if (resolve_structure_cons(e) == FAILURE) t = FAILURE;
    return t;

  default:
    g95_internal_error("g95_resolve_expr(): Bad expression type");
  }


/* Resolve all subnodes-- give them types. */

  switch(e->operator) {
  default:
    if (g95_resolve_expr(e->op2) == FAILURE) return FAILURE;

/* Fall through */

  case INTRINSIC_NOT:
  case INTRINSIC_UPLUS:
  case INTRINSIC_UMINUS:
    if (g95_resolve_expr(e->op1) == FAILURE) return FAILURE;
    break;
  }

/* Typecheck the new node. */

  op1 = e->op1;
  op2 = e->op2;

  switch(e->operator) {
  case INTRINSIC_UPLUS:
  case INTRINSIC_UMINUS:
    if ((op1->ts.type == BT_INTEGER) || (op1->ts.type == BT_REAL) ||
	(op1->ts.type == BT_COMPLEX)) {
      e->ts = op1->ts;
      break;
    }

    msg = "Unary numeric operator at %L needs a numeric operand";
    goto bad_op;

  case INTRINSIC_PLUS:
  case INTRINSIC_MINUS:
  case INTRINSIC_TIMES:
  case INTRINSIC_DIVIDE:
  case INTRINSIC_POWER:
    if (g95_numeric_ts(&op1->ts) && g95_numeric_ts(&op2->ts)) {
      g95_type_convert_binary(e);
      break;
    }

    msg = "Binary numeric operator at %L needs numeric operands";
    goto bad_op;

  case INTRINSIC_CONCAT:
    if (op1->ts.type == BT_CHARACTER && op2->ts.type == BT_CHARACTER) {
      e->ts.type = BT_CHARACTER;
      e->ts.kind = op1->ts.kind;
      break;
    }

    msg = "String concatenation operator at %L needs two string operands";
    goto bad_op;

  case INTRINSIC_AND:
  case INTRINSIC_OR:
  case INTRINSIC_EQV:
  case INTRINSIC_NEQV:
    if (op1->ts.type == BT_LOGICAL && op2->ts.type == BT_LOGICAL) {
      e->ts.type = BT_LOGICAL;
      e->ts.kind = g95_kind_max(op1, op2);
      break;
    }

    msg = "Logical operator at %L needs logical operands";
    goto bad_op;
      
  case INTRINSIC_NOT:
    if (op1->ts.type == BT_LOGICAL) {
      e->ts.type = BT_LOGICAL;
      e->ts.kind = op1->ts.kind;
      break;
    }

    msg = ".NOT. operator at %L needs logical operand";
    goto bad_op;

  case INTRINSIC_GT: case INTRINSIC_GE:
  case INTRINSIC_LT: case INTRINSIC_LE:
    if (op1->ts.type == BT_COMPLEX || op2->ts.type == BT_COMPLEX) {
      msg = "COMPLEX quantities cannot be compared at %L";
      goto bad_op;
    }

    /* Fall through */

  case INTRINSIC_EQ: case INTRINSIC_NE:
    if (op1->ts.type == BT_CHARACTER && op2->ts.type == BT_CHARACTER) {
      e->ts.type = BT_LOGICAL;
      e->ts.kind = g95_default_logical_kind();
      break;
    }

    if (g95_numeric_ts(&op1->ts) && g95_numeric_ts(&op2->ts)) {
      g95_type_convert_binary(e);
	
      e->ts.type = BT_LOGICAL;
      e->ts.kind = g95_default_logical_kind();
      break;
    }

    msg = "Comparison operator at %L requires similar operands";
    goto bad_op;

  case INTRINSIC_USER:
    msg = "Can't find compatible interface for user operator at %L";
    goto bad_op;

  default:
    g95_internal_error("g95_resolve_expr(): Bad intrinsic");
  }

/* Deal with arrayness of an operand through an operator */

  switch(e->operator) {
  case INTRINSIC_PLUS:    case INTRINSIC_MINUS:  case INTRINSIC_TIMES:
  case INTRINSIC_DIVIDE:  case INTRINSIC_POWER:  case INTRINSIC_CONCAT:
  case INTRINSIC_AND:     case INTRINSIC_OR:     case INTRINSIC_EQV:
  case INTRINSIC_NEQV:    case INTRINSIC_EQ:     case INTRINSIC_NE:
  case INTRINSIC_GT:      case INTRINSIC_GE:     case INTRINSIC_LT:
  case INTRINSIC_LE:
    if (op1->shape == NULL && op2->shape == NULL) e->shape = NULL;
    if (op1->shape != NULL && op2->shape == NULL)
      e->shape = g95_copy_array_shape(op1->shape);
    if (op1->shape == NULL && op2->shape != NULL)
      e->shape = g95_copy_array_shape(op2->shape);

/* TODO: we should really be comparing array refs where possible */

    if (op1->shape != NULL && op2->shape != NULL) {

      /* g95_check_conformability(op1->shape, op2->shape); */

      e->shape = g95_copy_array_shape(op1->shape);
    }

    break;

  case INTRINSIC_NOT:
  case INTRINSIC_UPLUS:
  case INTRINSIC_UMINUS:
    if (op1->shape != NULL) e->shape = g95_copy_array_shape(op1->shape);
    break;           /* Simply copy arrayness attribute */

  default:
    break;
  }

  if (t == SUCCESS) t = g95_simplify_expr(e, 0);

  return t;

bad_op:
  if (g95_extend_expr(e) == SUCCESS) return t;

  g95_error(msg, &e->where);
  return FAILURE;
}


/* g95_match_expr_type()-- Match an expression and require it to be of
 * a particular type. */

match g95_match_expr_type(bt type, g95_expr **result) {
g95_expr *e;
match m;

  m = g95_match_expr(&e);
  if (m != MATCH_YES) return m;

  if (type == e->ts.type) {
    *result = e;
    return MATCH_YES;
  }

  g95_error("Expression at %C is %s where %s is required",
	    g95_typename(e->ts.type), g95_typename(type));

  g95_free_expr(e);

  return MATCH_ERROR;
}


/* g95_match_scalar_expr()-- Match an expression and require it to be
 * scalar */

match g95_match_scalar_expr(g95_expr **result) {
g95_ref *ref;
g95_expr *e;
match m;

  m = g95_match_expr(&e);
  if (m != MATCH_YES) return m;

  if (e->shape != NULL) {
    for(ref=e->ref; ref->next;)
      ref = ref->next;

    if (ref->type == REF_ARRAY && ref->ar.type != AR_ELEMENT) {
      g95_error("Expression at %C must be scalar valued, not array valued");
      g95_free_expr(e);
      return MATCH_ERROR;
    }
  }

  *result = e;
  return MATCH_YES;
}


/* check_intrinsic_op()-- Check an intrinsic arithmetic operation to
 * see if it is consistent with some type of expression. */

static try check_init_expr(g95_expr *);
static try check_spec_expr(g95_expr *);

static try check_intrinsic_op(g95_expr *e, try (*check_function)(g95_expr *)) {
const char *expr_type = NULL;
 
  if ((*check_function)(e->op1) == FAILURE) return FAILURE;

  if (check_function == check_init_expr) expr_type = "initialization";
  if (check_function == check_spec_expr) expr_type = "specification";

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
      g95_error("Concatenation operator in %s expression at %L "
		"must have two CHARACTER operands", expr_type, &e->op1->where);
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
      g95_error(".NOT. operator in %s expression at %L must have "
		"a LOGICAL operand", expr_type, &e->op1->where);
      return FAILURE;
    }

    break;

  case INTRINSIC_AND:    case INTRINSIC_OR:
  case INTRINSIC_EQV:    case INTRINSIC_NEQV:
    if ((*check_function)(e->op2) == FAILURE) return FAILURE;

    if (e->op1->ts.type != BT_LOGICAL || e->op2->ts.type != BT_LOGICAL) {
      g95_error("LOGICAL operands are required in %s expression at %L",
		expr_type, &e->where);
      return FAILURE;
    }

    break;

  default:
    g95_error("Only intrinsic operators can be used in %s expression at %L",
	      expr_type, &e->where);
    return FAILURE;
  }

  return SUCCESS;

not_numeric:
  g95_error("Numeric operands are required in %s expression at %L",
	    expr_type, &e->where);

  return FAILURE;
}



/* check_numeric_inquiry()-- Numeric inquiry functions are
 * specifically allowed to have variable arguments, which is an
 * exception to the normal requirement that an initialization function
 * have initialization arguments.  We head off this problem here.  */

static try check_numeric_inquiry(g95_expr *e) {
char *name;
static const char *inquiry_function[] = {
  "digits", "epsilon", "huge", "maxexponent", "minexponent",
  "precision", "radix", "range", "tiny", "bit_size", NULL
};

int i;

  if (e->value.function.actual == NULL ||
      e->value.function.actual->next != NULL)  /* Doesn't have one arg */
    return FAILURE;

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
      m = g95_intrinsic_func_interface(e, 1);

      if (m == MATCH_NO)
	g95_error("Function '%s' in initialization expression at %L "
		  "must be an intrinsic function", e->symbol->name, &e->where);

      if (m != MATCH_YES) t = FAILURE;
    }

    break;

  case EXPR_VARIABLE:
    t = SUCCESS;

    if (g95_check_iter_variable(e) == SUCCESS) break;

    /* TODO: constant references to subobjects */

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

  m = g95_match_expr(&expr);
  if (m != MATCH_YES) return m;

  if (check_init_expr(expr) == FAILURE) {
    g95_free_expr(expr);
    return MATCH_ERROR;
  }

  if (expr->expr_type == EXPR_ARRAY &&
      (g95_check_constructor_type(expr) == FAILURE ||
       g95_expand_constructor(expr) == FAILURE)) {
    g95_free_expr(expr);
    return MATCH_ERROR;
  }

  if (!is_constant_expr(expr))
    g95_internal_error("Initialization expression didn't reduce %C");

  *result = expr;

  return MATCH_YES;
}


/* check_spec_expr()-- Verify that an expression is a
 * specification expression.  Like its cousin check_init_expr(),
 * an error message is generated if we return FAILURE. */

static try check_spec_expr(g95_expr *e) {
g95_actual_arglist *ap;
g95_symbol *sym;
match m;
try t;

  if (e == NULL) return SUCCESS;

  switch(e->expr_type) {
  case EXPR_OP:
    t = check_intrinsic_op(e, check_spec_expr);
    if (t == SUCCESS) t = g95_simplify_expr(e, 0);

    break;

  case EXPR_FUNCTION:
    t = SUCCESS;

    for(ap=e->value.function.actual; ap; ap=ap->next)
      if (check_spec_expr(ap->expr) == FAILURE) {
	t = FAILURE;
	break;
      }

    if (t == SUCCESS) {
      m = g95_intrinsic_func_interface(e, 0);

      if (m == MATCH_NO)
	g95_error("Function '%s' in specification expression at %L "
		  "must be an intrinsic function", e->symbol->name, &e->where);

      if (m != MATCH_YES) t = FAILURE;
    }

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

    if (sym->attr.use_assoc || sym->ns != g95_current_ns) {
      t = SUCCESS;
      break;
    }

    g95_error("Variable '%s' cannot appear in the specification expression ",
	      "at %L", sym->name, &e->where);

    break;


  case EXPR_NULL:
    g95_error("NULL at %L cannot appear in a specification expression",
	      &e->where);
    t = FAILURE;
    break;

  case EXPR_CONSTANT:
    t = SUCCESS;
    break;

  case EXPR_SUBSTRING:
    t = check_spec_expr(e->op1);
    if (t == FAILURE) break;

    t = check_spec_expr(e->op2);
    if (t == SUCCESS) t = g95_simplify_expr(e, 0);

    break;

  case EXPR_STRUCTURE:
    t = g95_check_constructor(e, check_spec_expr);
    break;

  case EXPR_ARRAY:
    t = g95_check_constructor(e, check_spec_expr);
    break;

  default:
    g95_internal_error("check_spec_expr(): Unknown expression type");
  }

  return t;
}


/* g95_match_spec_expr()-- Match a specification expression. */

match g95_match_spec_expr(g95_expr **result) {
g95_expr *expr;
match m;

  m = g95_match_expr(&expr); 
  if (m != MATCH_YES) return m;

  if (check_spec_expr(expr) == FAILURE) {
    g95_free_expr(expr);
    return MATCH_ERROR;
  }

  *result = expr;
  return MATCH_YES;
}


/* show_ref()-- Show a string of g95_ref structures. */

static void show_ref(g95_ref *p) {

  for(; p; p=p->next)
    switch(p->type) {
    case REF_ARRAY:
      g95_show_array_ref(&p->ar);
      break;

    case REF_COMPONENT:
      g95_status(" %% %s", p->component->name);
      break;

    case REF_SUBSTRING:
      g95_status_char('(');
      g95_show_expr(p->start);
      g95_status_char(':');
      g95_show_expr(p->end);
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
