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

#include "g95.h"

/* g95_get_expr()-- Get a new expr node */

g95_expr *g95_get_expr(void) {
g95_expr *e;

  e = g95_getmem(sizeof(g95_expr));

  e->ts.type = BT_UNKNOWN;
  e->op1 = NULL;
  e->op2 = NULL;

  return e;
}


/* g95_show_actual_arglist()-- Show an actual argument list */

void g95_show_actual_arglist(g95_actual_arglist *a) {

  g95_status("(");

  for(; a; a=a->next) {
    g95_status_char('(');
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

  head = NULL;

  for(; p; p=p->next) {
    new = g95_get_actual_arglist();
    *new = *p;

    new->expr = g95_copy_expr(p->expr);
    new->next = NULL;

    if (tail == NULL) head = new;
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
      g95_free_expr(p->ar.shape[i].start);
      g95_free_expr(p->ar.shape[i].end);
      g95_free_expr(p->ar.shape[i].stride);
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

  default:
    g95_internal_error("g95_free_expr0(): Bad expr type");
  }
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

char *g95_extract_int(g95_expr *expr, int *result) {

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


/* g95_copy_expr()-- Given an expression pointer, return a copy of the
 * expression.  This subroutine is recursive. */

g95_expr *g95_copy_expr(g95_expr *p) {
g95_expr *q;
char *s;

  if (p == NULL) return NULL;

  q = g95_get_expr();
  *q = *p;

  if (p->ar != NULL) q->ar = g95_copy_array_ref(p->ar);

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

  case EXPR_FUNCTION:  /* Fall through */
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
  p->rank = 0;
  p->ts.type = BT_INTEGER;
  p->ts.kind = g95_default_integer_kind();

  p->where = *g95_current_locus();
  mpz_init_set_si(p->value.integer, i);

  return p;
}


/* g95_constant_expr()-- Convert an integer into a expression node of
 * a given type.  This is used to create small constants like zero,
 * one, etc. */

g95_expr *g95_constant_expr(bt type, int i, locus *where) {
g95_expr *p, *real, *imag;
char *b, buffer[20];
int digit, divisor;

  b = buffer;

  if (i < 0) {
    *b++ = '-';
    i = -i;
  }

  divisor = 1;
  while(i/divisor > 9)
    divisor *= 10;

  while(divisor > 0) {
    digit = i / divisor;
    *b++ = '0' + digit;
    i = i - digit*divisor;
    divisor /= 10;
  }

  *b = '\0';

  switch(type) {
  case BT_INTEGER:
    p = g95_convert_integer(buffer, g95_default_integer_kind(), 10);
    break;

  case BT_REAL:
    p = g95_convert_real(buffer, g95_default_real_kind());
    break;

  case BT_COMPLEX:
    real = g95_convert_real(buffer, g95_default_real_kind());
    imag = g95_convert_real("0", g95_default_real_kind());

    p = g95_convert_complex(real, imag, g95_default_complex_kind());
    g95_free_expr(real);
    g95_free_expr(imag);
    break;

  default:
    g95_internal_error("g95_constant_expr(): Bad type");
  }

  p->where = (where == NULL) ? *g95_current_locus() : *where;
  return p;
}


/* g95_logical_expr()-- Returns an expression node that is a logical
 * constant. */

g95_expr *g95_logical_expr(int i, locus *where) {
g95_expr *p;

  p = g95_get_expr();

  p->expr_type = EXPR_CONSTANT;
  p->rank = 0;
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

    if (c->ext == NULL) {
      tail = g95_get_code();
      c->ext = tail;
    } else {
      tail->next = g95_get_code();
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
  p->ts.type = BT_UNKNOWN;
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



/* type_convert_binary()-- Given an expression node with some sort of
 * numeric binary expression, insert type conversions required to make
 * the operands have the same type.
 *
 * The exception is that the operands of an exponential don't have to
 * have the same type.  If possible, the base is promoted to the type
 * of the exponent.  For example, 1**2.3 becomes 1.0**2.3, but
 * 1.0**2 stays as it is. */

static void type_convert_binary(g95_expr *e) {
g95_expr *op1, *op2;

  op1 = e->op1;
  op2 = e->op2;

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
    e->ts.kind = op1->ts.kind;

    g95_convert_type(e->op1, &e->ts, 2);
    goto done;
  }

done:
  return;
}


/* simplify_expr()-- Given an expression, simplify it by collapsing
 * constant expressions. */

static arith simplify_expr(g95_expr *p, int init_flag) {
g95_expr new_expr, *op1, *op2;
g95_actual_arglist *ap;
int binary_flag;
arith result;

  if (p->rank != 0) return ARITH_OK;

/* Replace a parameter variable with its value */

  if (p->expr_type == EXPR_VARIABLE &&
      p->symbol->attr.flavor == FL_PARAMETER) {
    g95_replace_expr(p, g95_copy_expr(p->symbol->value));
    return ARITH_OK;
  }

/* Simplify a function.  First simplify the arguments */

  if (p->expr_type == EXPR_FUNCTION) {
    int const_flag = 1;

    for(ap=p->value.function.actual; ap; ap=ap->next) {
      if (ap->expr != NULL) {
	simplify_expr(ap->expr, init_flag);
	if (ap->expr->expr_type != EXPR_CONSTANT) const_flag = 0;
      }
    }

    if (const_flag && init_flag) g95_intrinsic_func_interface(p);

    return ARITH_OK;
  }

/* Arithmetic simplifications */

  if (p->expr_type != EXPR_OP) return ARITH_OK;

  switch(p->operator) {
  case INTRINSIC_UPLUS:   case INTRINSIC_UMINUS: case INTRINSIC_NOT:
    binary_flag = 0;
    break;

  case INTRINSIC_PLUS:    case INTRINSIC_MINUS:  case INTRINSIC_TIMES:
  case INTRINSIC_DIVIDE:  case INTRINSIC_POWER:  case INTRINSIC_CONCAT:
  case INTRINSIC_AND:     case INTRINSIC_OR:     case INTRINSIC_EQV:
  case INTRINSIC_NEQV:    case INTRINSIC_EQ:     case INTRINSIC_NE:
  case INTRINSIC_GT:      case INTRINSIC_GE:     case INTRINSIC_LT:
  case INTRINSIC_LE:
    binary_flag = 1;
    break;

  case INTRINSIC_USER:
    binary_flag = (p->op2 != NULL);
    break;

  default:
    g95_internal_error("simplify_expr(): Bad operator");
  }

  result = simplify_expr(op1 = p->op1, init_flag);
  if (result != ARITH_OK) return result;

  if (binary_flag) {
    result = simplify_expr(op2 = p->op2, init_flag);
    if (result != ARITH_OK) return result;

    if (op2->expr_type != EXPR_CONSTANT) return ARITH_OK;
  }

  if (op1->expr_type != EXPR_CONSTANT) return ARITH_OK;

  if (p->operator == INTRINSIC_USER) return ARITH_OK;

  new_expr = *p;
  new_expr.expr_type = EXPR_CONSTANT;
  new_expr.where = op1->where;

  result = ARITH_OK;     /* For operations that always work */

  if (!binary_flag)
    new_expr.ts.kind = op1->ts.kind;
  else {
    new_expr.ts.kind = g95_kind_max(op1, op2);

    if (g95_numeric_ts(&op1->ts) && g95_numeric_ts(&op2->ts))
      type_convert_binary(&new_expr);
  }

  new_expr.ts.type = op1->ts.type;

  switch(p->operator) {
    case INTRINSIC_UPLUS:  /* Unary pluses are gotten rid of */
      new_expr.value = op1->value;
      break;

/* Farm out the more complicated arithmetic operations */

    case INTRINSIC_UMINUS:
      result = g95_arith_uminus(op1, &new_expr);      break;

    case INTRINSIC_PLUS:
      result = g95_arith_plus(op1, op2, &new_expr);   break;

    case INTRINSIC_MINUS:
      result = g95_arith_minus(op1, op2, &new_expr);  break;

    case INTRINSIC_TIMES:
      result = g95_arith_times(op1, op2, &new_expr);  break;

    case INTRINSIC_DIVIDE:
      result = g95_arith_divide(op1, op2, &new_expr); break;

    case INTRINSIC_POWER:
      if (op1->ts.type != BT_INTEGER || op2->ts.type != BT_INTEGER)
	return ARITH_OK;

      result = g95_arith_power(op1, op2, &new_expr);
      break;

    case INTRINSIC_CONCAT:
      result = g95_arith_concat(op1, op2, &new_expr); break;

    case INTRINSIC_EQ:      
      g95_arith_eq(op1, op2, &new_expr);
      new_expr.ts.type = BT_LOGICAL;
      new_expr.ts.kind = g95_default_logical_kind();
      break;

    case INTRINSIC_NE:
      g95_arith_ne(op1, op2, &new_expr);
      new_expr.ts.type = BT_LOGICAL;
      new_expr.ts.kind = g95_default_logical_kind();
      break;

    case INTRINSIC_GT:
      g95_arith_gt(op1, op2, &new_expr);
      new_expr.ts.type = BT_LOGICAL;
      new_expr.ts.kind = g95_default_logical_kind();
      break;

    case INTRINSIC_GE:
      g95_arith_ge(op1, op2, &new_expr);
      new_expr.ts.type = BT_LOGICAL;
      new_expr.ts.kind = g95_default_logical_kind();
      break;

    case INTRINSIC_LT:
      g95_arith_lt(op1, op2, &new_expr);
      new_expr.ts.type = BT_LOGICAL;
      new_expr.ts.kind = g95_default_logical_kind();
      break;

    case INTRINSIC_LE:
      g95_arith_le(op1, op2, &new_expr);
      new_expr.ts.type = BT_LOGICAL;
      new_expr.ts.kind = g95_default_logical_kind();
      break;

/* Logical simplifications are so simple we do them here */

    case INTRINSIC_NOT:
      new_expr.value.logical = !op1->value.logical; break;

    case INTRINSIC_AND:
      new_expr.value.logical = op1->value.logical && op2->value.logical; break;

    case INTRINSIC_OR:
      new_expr.value.logical = op1->value.logical || op2->value.logical; break;

    case INTRINSIC_EQV:
      new_expr.value.logical = op1->value.logical == op2->value.logical; break;

    case INTRINSIC_NEQV:
      new_expr.value.logical = op1->value.logical != op2->value.logical; break;

    default: g95_internal_error("simplify_expr(): Impossible operator"); 
  }

  if (result != ARITH_OK) {
    g95_error("%s at %C", g95_arith_error(result));
    return result;
  }

  *p = new_expr;

  g95_free_expr(op1);
  if (binary_flag) g95_free_expr(op2);

  return result;
}


/****************** Expression name resolution ******************/

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
  if (expr->value.function.name != NULL) return SUCCESS;

/* Now resolve the function itself.  For now, we just see if the function
 * is compatible with an intrinsic. */

  t = g95_intrinsic_func_interface(expr);
  if (t == FAILURE) {
    expr->value.function.name = expr->symbol->name;
    expr->ts = expr->symbol->ts;
  }

  return SUCCESS;  /* Always succeeds for now */
}


/* resolve_ref()-- Resolve subtype references */

static try resolve_ref(g95_expr *expr) {
g95_array_spec *as;
g95_ref *ref;
try t;

  as = &expr->symbol->as;
  t = SUCCESS;

  for(ref=expr->ref; ref; ref=ref->next)
    switch(ref->type) {
    case REF_ARRAY:
      if (g95_resolve_array_ref(&ref->ar, as) == FAILURE) t = FAILURE;
      as = NULL;
      break;

    case REF_COMPONENT:
      as = &ref->component->as;   /* In case an array ref is next */
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

try resolve_structure_cons(g95_expr *expr) {
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
char *msg;
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
      type_convert_binary(e);
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
      type_convert_binary(e);
	
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
    if (op1->ar == NULL && op2->ar == NULL) e->ar = NULL;
    if (op1->ar != NULL && op2->ar == NULL) e->ar = op1->ar;
    if (op1->ar == NULL && op2->ar != NULL) e->ar = op2->ar;

/* TODO: we should really be comparing array refs where possible */
    if (op1->ar != NULL && op2->ar != NULL) e->ar = op1->ar;

    break;

  case INTRINSIC_NOT:
  case INTRINSIC_UPLUS:
  case INTRINSIC_UMINUS:
    e->ar = op1->ar;
    break;           /* Simply copy arrayness attribute */

  default:
    break;
  }

  if (simplify_expr(e, 0) != ARITH_OK) t = FAILURE;

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
g95_expr *e;
match m;

  m = g95_match_expr(&e);
  if (m != MATCH_YES) return m;

  if (e->ar != NULL) {
    g95_error("Expression at %C must be scalar valued, not array valued");
    g95_free_expr(e);
    return MATCH_ERROR;
  }

  *result = e;
  return MATCH_YES;
}


/* is_constant_constructor()-- Recursively checks all elements of a
 * constructor to see if everything is constant. */

static int is_constant_expr(g95_expr *);

static int is_constant_constructor(g95_constructor *c) {

  if (c == NULL) return 1;

  for(; c; c=c->next) {
    if (!is_constant_expr(c->expr)) return 0;

    if (!is_constant_constructor(c->child)) return 0;
    
    if (c->iter != NULL) {
      if (!is_constant_expr(c->iter->start)) return 0;
      if (!is_constant_expr(c->iter->end))   return 0;
      if (!is_constant_expr(c->iter->step))  return 0;
    }
  }

  return 1;
}


/* is_constant_expr()-- Function to determine if an expression is
 * constant or not.  This function expects that the expression has
 * already been simplified.  Mutually recursive with
 * constant_constructor(). */

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
    rv = 1;
    break;

  case EXPR_SUBSTRING:
    rv = is_constant_expr(e->op1) && is_constant_expr(e->op2);
    break;

  case EXPR_STRUCTURE:
  case EXPR_ARRAY:
    rv = is_constant_constructor(e->value.constructor);
    break;
  }

  return rv;
}


/* g95_match_init_expr()-- Match an initialization expression.  We work
 * by first matching an expression, then reducing it to a constant */

match g95_match_init_expr(g95_expr **result) {
g95_expr *expr;
match m;

  m = g95_match_expr(&expr);
  if (m != MATCH_YES) return m;

/* Constant for now */

  if (simplify_expr(expr, 1) != ARITH_OK) return MATCH_ERROR;

  if (!is_constant_expr(expr)) {
    g95_error("Expected an initialization expression at %C");
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
    if (c->child == NULL)
      g95_show_expr(c->expr);
    else {
      g95_status("( ");
      show_constructor(c->child);

      g95_status_char(' ');
      g95_show_expr(c->iter->var);
      g95_status_char('=');
      g95_show_expr(c->iter->start);
      g95_status_char(',');
      g95_show_expr(c->iter->end);
      g95_status_char(',');
      g95_show_expr(c->iter->step);

      g95_status(" )");
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
