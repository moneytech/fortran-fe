/* Expression subroutines
   Copyright (C) 2000, 2001, 2002 Free Software Foundation, Inc.
   Contributed by Andy Vaught

This file is part of G95.

G95 is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

G95 is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with G95; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */ 
 
/* expr.c-- Manipulate expression nodes */        
        
#include <stdarg.h>
#include <stdio.h>
#include <string.h>

#include "g95.h"
      
static try check_init_expr(g95_expr *);          
static try check_restricted(g95_expr *);          
          
          


/* g95_show_actual_arglist()-- Show an actual argument list */

void g95_show_actual_arglist(g95_actual_arglist *v) {    
    
  g95_status("(");        
        
  for(; v; v=v->next) {        
    g95_status_char('(');         
         
    if (v->type == ALT_RETURN)       
      g95_status("*%d", v->u.label->value);          
    else {         
      if (v->pointer) g95_status("P ");         
         
      switch(v->type) {        
      case FULL_ARRAY:     g95_status("F "); break;       
      case ARRAY_ELEMENT:  g95_status("E "); break;       
      case ARRAY_DESC:     g95_status("D "); break;        
      default:                               break;
      }          
          
      if (v->name[0] != '\0') g95_status("%s = ", v->name);

      if (v->u.expr == NULL)        
	g95_status("(arg not-present)");    
      else
	g95_show_expr(v->u.expr);    
    }  
  
    g95_status_char(')');
    if (v->next != NULL) g95_status(" ");         
  }   
   
  g95_status(")");  
}


       
       
/* simplify_ref()-- Simplify an reference structure */    
    
static try simplify_ref(g95_ref *reference, int flag) {  
try n;        
int e;   
   
  n = SUCCESS;    
   
  switch(reference->type) {       
  case REF_ARRAY:     
    for(e=0; e<reference->u.ar.dimen; e++) {      
      if (g95_simplify_expr(reference->u.ar.start[e],  flag) == FAILURE) n = FAILURE;    
      if (g95_simplify_expr(reference->u.ar.end[e],    flag) == FAILURE) n = FAILURE;       
      if (g95_simplify_expr(reference->u.ar.stride[e], flag) == FAILURE) n = FAILURE;
    }    
    
    break;   
   
  case REF_COMPONENT:       
    break;      
      
  case REF_SUBSTRING:   
    if (g95_simplify_expr(reference->u.ss.start, flag) == FAILURE) n = FAILURE;   
    if (g95_simplify_expr(reference->u.ss.end, flag)   == FAILURE) n = FAILURE;    
    break;          
  }     
     
  return n;
}          
          
          
         
         
/* g95_logical_expr()-- Returns an expression node that is a logical
 * constant. */        
        
g95_expr *g95_logical_expr(int s, locus *w) {        
g95_expr *r;        
        
  r = g95_get_expr();       
       
  r->type = EXPR_CONSTANT;  
  r->ts.type = BT_LOGICAL;         
  r->ts.kind = g95_default_logical_kind();         
         
  if (w == NULL) w = g95_current_locus(); 
  r->where = *w; 
  r->value.logical = s;

  return r; 
}    
    
    
     
     
/* free_actual_arglist()-- Free an argument list and everything below it. */         
         
void g95_free_actual_arglist(g95_actual_arglist *f) {      
g95_actual_arglist *c;

  while(f) {
    c = f->next; 
    if (f->type != ALT_RETURN) g95_free_expr(f->u.expr);  
  
    g95_free(f);  
    f = c;       
  }     
}   
   
   
  
  
/* g95_free_expr0()-- Workhorse function for g95_free_expr() that
 * frees everything beneath an expression node, but not the node
 * itself.  This is useful when we want to simplify a node and replace
 * it with something else or the expression node belongs to another
 * structure.  */      
      
static void g95_free_expr0(g95_expr *e) {          
int a;        
        
  switch(e->type) {       
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
   
  case EXPR_NULL: 
  case EXPR_VARIABLE: 
  case EXPR_UNKNOWN:  
    break;  
  
  case EXPR_ARRAY: 
  case EXPR_STRUCTURE:     
    g95_free_constructor(e->value.constructor); 
    break;     
     
  case EXPR_SUBSTRING:   
    g95_free(e->value.character.string); 
    break;    
    
  default:         
    g95_internal_error("g95_free_expr0(): Bad expr type");         
  } 
 
  /* Free a shape array */        
        
  if (e->shape != NULL) {  
    for(a=0; a<e->rank; a++)     
      mpz_clear(e->shape[a]);       
       
    g95_free(e->shape);   
  }  
  
  g95_free_ref_list(e->ref);   
   
  memset(e, '\0', sizeof(g95_expr));   
} 
 
 
        
        
/* restricted_args()-- Given an actual argument list, test to see that
 * each argument is a restricted expression and optionally if the
 * expression type is integer or character */     
     
static try restricted_args(g95_actual_arglist *a, int check_type) {   
bt dtype;

  for(; a; a=a->next) {
    if (check_restricted(a->u.expr) == FAILURE) return FAILURE; 
 
    if (!check_type) continue;     
     
    dtype = a->u.expr->ts.type;        
    if (dtype != BT_CHARACTER && dtype != BT_INTEGER) {      
      g95_error("Function argument at %L must be of type INTEGER or CHARACTER",      
		&a->u.expr->where);        
      return FAILURE;         
    }      
  }     
     
  return SUCCESS; 
}    
    
    
  
  
/* g95_copy_arglist()-- Copy an arglist structure and all of the arguments. */     
     
g95_actual_arglist *g95_copy_actual_arglist(g95_actual_arglist *f) {       
g95_actual_arglist *start, *tail, *old; 
 
  start = tail = NULL;       
       
  for(; f; f=f->next) {         
    old = g95_get_actual_arglist();          
    *old = *f;     
     
    if (f->type != ALT_RETURN) old->u.expr = g95_copy_expr(f->u.expr);     
     
    old->next = NULL;   
   
    if (start == NULL)  
      start = old;      
    else       
      tail->next = old;        
        
    tail = old;          
  }   
   
  return start;  
}      
      
      
        
        
/* g95_check_parameter()-- At this point, we're trying to simplify an
 * expression involving a parameter.  Make sure the parameter has a
 * type, defaulting it if necessary and possibly converting the value
 * to this type. */          
          
try g95_check_parameter(g95_symbol *sym) { 
 
  if (sym->ts.type == BT_UNKNOWN &&   
      g95_set_default_type(sym, 1, sym->ns) == FAILURE) return FAILURE;  
  
  if (g95_compare_types(&sym->ts, &sym->value->ts)) return SUCCESS;         
         
  return g95_convert_type(sym->value, &sym->ts, 1);  
}


       
       
/* numeric_type()-- Returns nonzero if the type is numeric, zero
 * otherwise */      
      
static int numeric_type(bt typ) {  
  
  return typ == BT_COMPLEX || typ == BT_REAL || typ == BT_INTEGER;     
}  
  
  
         
         
/* g95_free_ref_list()-- Free a list of reference structures */      
      
void g95_free_ref_list(g95_ref *h) {     
g95_ref *l;         
int s;

  for(; h; h=l) {       
    l = h->next;          
          
    switch(h->type) {        
    case REF_ARRAY:          
      for(s=0; s<G95_MAX_DIMENSIONS; s++) {        
	g95_free_expr(h->u.ar.start[s]);          
	g95_free_expr(h->u.ar.end[s]); 
	g95_free_expr(h->u.ar.stride[s]);     
      }

      break;      
      
    case REF_SUBSTRING:    
      g95_free_expr(h->u.ss.start);         
      g95_free_expr(h->u.ss.end);       
      break;          
          
    case REF_COMPONENT:    
      break;      
    }        
        
    g95_free(h);       
  }    
}     
     
     
    
    
/* simplify_intrinsic_op()-- Try to collapse intrinsic expressions */      
      
static try simplify_intrinsic_op(g95_expr *a, int dtype) {      
g95_expr *op, *op0, *res;    
    
  if (a->operator == INTRINSIC_USER) return SUCCESS;      
      
  op = a->op1;         
  op0 = a->op2;    
    
  if (g95_simplify_expr(op, dtype) == FAILURE) return FAILURE;          
  if (g95_simplify_expr(op0, dtype) == FAILURE) return FAILURE;     
     
  if (!g95_is_constant_expr(op) ||       
      (op0 != NULL && !g95_is_constant_expr(op0)))         
    return SUCCESS;      
      
/* Rip p apart */ 
 
  a->op1 = NULL;      
  a->op2 = NULL;      
      
  switch(a->operator) { 
  case INTRINSIC_UPLUS:        
    res = g95_uplus(op);   
    break;         
         
  case INTRINSIC_UMINUS:        
    res = g95_uminus(op); 
    break;    
    
  case INTRINSIC_PLUS:      
    res = g95_add(op, op0); 
    break;        
        
  case INTRINSIC_MINUS:      
    res = g95_subtract(op, op0);     
    break;        
        
  case INTRINSIC_TIMES:      
    res = g95_multiply(op, op0);    
    break;    
    
  case INTRINSIC_DIVIDE:    
    if ((op0->ts.type == BT_INTEGER &&     
	 mpz_cmp_ui(op0->value.integer, 0) == 0) ||       
	(op0->ts.type == BT_REAL && 
	 mpf_cmp_ui(op0->value.real, 0) == 0) ||        
	(op0->ts.type == BT_COMPLEX &&
	 mpf_cmp_ui(op0->value.complex.r, 0) == 0 &&   
	 mpf_cmp_ui(op0->value.complex.i, 0) == 0)) {
      a->op1 = op;        
      a->op2 = op0;          
      return SUCCESS;     
    } 
 
    res = g95_divide(op, op0);  
    break;   
   
  case INTRINSIC_POWER:
    res = g95_power(op, op0);
    break;      
      
  case INTRINSIC_CONCAT: 
    res = g95_concat(op, op0);       
    break;         
         
  case INTRINSIC_EQ:              
    res = g95_eq(op, op0);          
    break;     
     
  case INTRINSIC_NE:  
    res = g95_ne(op, op0);        
    break;     
     
  case INTRINSIC_GT:       
    res = g95_gt(op, op0);       
    break;  
  
  case INTRINSIC_GE:    
    res = g95_ge(op, op0);      
    break; 
 
  case INTRINSIC_LT:  
    res = g95_lt(op, op0);     
    break;     
     
  case INTRINSIC_LE:         
    res = g95_le(op, op0);  
    break;    
    
  case INTRINSIC_NOT:         
    res = g95_not(op);      
    break;       
       
  case INTRINSIC_AND:          
    res = g95_and(op, op0);         
    break;  
  
  case INTRINSIC_OR:  
    res = g95_or(op, op0);   
    break;         
         
  case INTRINSIC_EQV: 
    res = g95_eqv(op, op0);       
    break;     
     
  case INTRINSIC_NEQV: 
    res = g95_neqv(op, op0);
    break;        
        
  default: g95_internal_error("simplify_intrinsic_op(): Bad operator");      
  }     
     
  if (res == NULL) {   
    g95_free_expr(op);        
    g95_free_expr(op0); 
    return FAILURE;        
  }  
  
  g95_replace_expr(a, res);     
     
  return SUCCESS;    
}


      
      
/* g95_is_constant_expr()-- Function to determine if an expression is
 * constant or not.  This function expects that the expression has
 * already been simplified. */        
        
int g95_is_constant_expr(g95_expr *o) {          
g95_constructor *v;          
g95_actual_arglist *arg;         
int rv;  
  
  if (o == NULL) return 1;         
        
  switch(o->type) {   
  case EXPR_OP:
    rv = g95_is_constant_expr(o->op1) &&        
      (o->op2 == NULL || g95_is_constant_expr(o->op2));   
   
    break;        
        
  case EXPR_VARIABLE: 
    rv = 0;        
    break;  
  
  case EXPR_FUNCTION:
    rv = 0;   
    /* call to intrinsic with at least one argument */  
    if (o->value.function.isym && o->value.function.actual) {         
      for(arg = o->value.function.actual; arg; arg = arg->next){  
	if (!g95_is_constant_expr(arg->u.expr))    
	  break;   
      }
      if (arg == NULL)    
	rv = 1;        
    }   
    break;         
         
  case EXPR_CONSTANT:         
  case EXPR_NULL:      
    rv = 1;       
    break; 
 
  case EXPR_SUBSTRING:
    rv = g95_is_constant_expr(o->ref->u.ss.start) &&          
         g95_is_constant_expr(o->ref->u.ss.end);       
    break;        
        
  case EXPR_STRUCTURE:   
    rv = 0;      
    for(v=o->value.constructor; v; v=v->next)         
      if (!g95_is_constant_expr(v->expr)) break;         
         
    if (v == NULL) rv = 1;          
    break;       
       
  case EXPR_ARRAY:    
    rv = g95_constant_ac(o);        
    break;        
        
  default:   
    g95_internal_error("g95_is_constant_expr(): Unknown expression type");    
  }  
  
  return rv;   
}   
   
   
  
  
/* check_inquiry()-- Certain inquiry functions are specifically
 * allowed to have variable arguments, which is an exception to the
 * normal requirement that an initialization function have
 * initialization arguments.  We head off this problem here.  */    
    
static try check_inquiry(g95_expr *c) {       
char *name0;         
static char *inquiry_function[] = {          
  "digits", "epsilon", "huge", "kind", "maxexponent", "minexponent",    
  "precision", "radix", "range", "tiny", "bit_size", "size", "shape",         
  "lbound", "ubound", NULL     
};  
  
int v;        
        
  if (c->value.function.actual == NULL || 
      c->value.function.actual->next != NULL)  /* Doesn't have one arg */
    return FAILURE;  
  
  if (c->value.function.name != NULL && 
      c->value.function.name[0] != '\0') return FAILURE;          
          
  name0 = c->symbol->name;  
  
  for(v=0; inquiry_function[v]; v++)
    if (strcmp(inquiry_function[v], name0) == 0) break;

  if (inquiry_function[v] == NULL) return FAILURE;       
       
  c = c->value.function.actual->u.expr;   
   
  if (c == NULL || c->type != EXPR_VARIABLE) return FAILURE;        
        
  /* At this point we have a numeric inquiry function with a variable
   * argument.  The type of the variable might be undefined, but we
   * need it now, because the arguments of these functions are allowed
   * to be undefined. */  
  
  if (c->ts.type == BT_UNKNOWN) { 
    if (c->symbol->ts.type == BT_UNKNOWN && 
	g95_set_default_type(c->symbol, 0, g95_current_ns) == FAILURE)
      return FAILURE;        
        
    c->ts = c->symbol->ts;         
  }       
       
  return SUCCESS; 
}        
        
        
 
 
/* copy_ref()-- Recursively copy a list of reference structures */

static g95_ref *copy_ref(g95_ref *s) {
g95_array_ref *spec;       
g95_ref *des;         
         
  if (s == NULL) return NULL;    
   
  des = g95_get_ref(); 
  des->type = s->type;     
     
  switch(s->type) {  
  case REF_ARRAY:          
    spec = g95_copy_array_ref(&s->u.ar);
    des->u.ar = *spec;  
    g95_free(spec); 
    break;       
       
  case REF_COMPONENT:      
    des->u.c = s->u.c;          
    break;     
     
  case REF_SUBSTRING:      
    des->u.ss = s->u.ss;        
    des->u.ss.start = g95_copy_expr(s->u.ss.start);       
    des->u.ss.end = g95_copy_expr(s->u.ss.end);     
    break; 
  }        
        
  des->next = copy_ref(s->next);    
    
  return des;         
}        
        
        
  
  
/* g95_build_funcall()-- Return an expression node with an optional
 * argument list attached.  A variable number of g95_expr pointers are
 * strung together in an argument list with a NULL pointer terminating
 * the list. */  
  
g95_expr *g95_build_funcall(g95_symbol *func, ...) {  
g95_actual_arglist *end;       
g95_expr *b, *h;   
va_list argum;        
        
  b = g95_get_expr();  
  b->type = EXPR_FUNCTION;  
  b->symbol = func; 
  b->value.function.actual = NULL;         
         
  end = NULL;       
       
  va_start(argum, func);          
  for(;;) {
    h = va_arg(argum, g95_expr *);    
    if (h == NULL) break;          
          
    if (end == NULL)  
      b->value.function.actual = end = g95_get_actual_arglist();    
    else {         
      end->next = g95_get_actual_arglist();          
      end = end->next;  
    }   
   
    end->type = EXPR;          
    end->u.expr = h;   
  }          
          
  va_end(argum);

  return b;          
}


    
    
/* g95_type_convert_binary()-- Given an expression node with some sort of
 * numeric binary expression, insert type conversions required to make
 * the operands have the same type.
 *
 * The exception is that the operands of an exponential don't have to
 * have the same type.  If possible, the base is promoted to the type
 * of the exponent.  For example, 1**2.3 becomes 1.0**2.3, but
 * 1.0**2 stays as it is. */      
      
void g95_type_convert_binary(g95_expr *v) {       
g95_expr *op_1, *op2;    
    
  op_1 = v->op1;
  op2 = v->op2;       
       
  if (op_1->ts.type == BT_UNKNOWN || op2->ts.type == BT_UNKNOWN) { 
    g95_clear_ts(&v->ts); 
    return;   
  }  
  
/* Kind conversions */    
    
  if (op_1->ts.type == op2->ts.type) {   
   
    if (op_1->ts.kind == op2->ts.kind) {  /* No type conversions */   
      v->ts = op_1->ts;   
      goto done;       
    }     
     
    if (op_1->ts.kind > op2->ts.kind)        
      g95_convert_type(op2, &op_1->ts, 2);      
    else      
      g95_convert_type(op_1, &op2->ts, 2);     
     
    v->ts = op_1->ts;       
    goto done;       
  }   
   
/* Real and integer combined with complex */

  if (op_1->ts.type == BT_COMPLEX &&      
      (op2->ts.type == BT_REAL || op2->ts.type == BT_INTEGER)) {         
         
    v->ts.type = BT_COMPLEX;  
    v->ts.kind = op_1->ts.kind;  
  
    if (v->operator == INTRINSIC_POWER && op2->ts.type == BT_INTEGER)       
      goto done;      
      
    g95_convert_type(v->op2, &v->ts, 2);          
    goto done;       
  }          
          
  if (op2->ts.type == BT_COMPLEX &&        
      (op_1->ts.type == BT_REAL || op_1->ts.type == BT_INTEGER)) { 
 
    v->ts.type = BT_COMPLEX;      
    v->ts.kind = op2->ts.kind;      
      
    g95_convert_type(v->op1, &v->ts, 2);
    goto done;         
  }          
          
/* Integer combined with real */   
   
  if (op_1->ts.type == BT_REAL && op2->ts.type == BT_INTEGER) {  
    v->ts.type = BT_REAL;        
    v->ts.kind = op_1->ts.kind;     
     
    if (v->operator == INTRINSIC_POWER) goto done;         
         
    g95_convert_type(v->op2, &v->ts, 2);     
    goto done;      
  }         
         
  if (op_1->ts.type == BT_INTEGER && op2->ts.type == BT_REAL) { 
    v->ts.type = BT_REAL;    
    v->ts.kind = op2->ts.kind;    
    
    g95_convert_type(v->op1, &v->ts, 2); 
    goto done; 
  }     
     
done:  
  return;     
}  
  
  
          
          
/* g95_match_init_expr()-- Match an initialization expression.  We work
 * by first matching an expression, then reducing it to a constant */     
     
match g95_match_init_expr(g95_expr **r) {     
g95_expr *expr;      
match u;         
try d;     
     
  u = g95_match_expr(&expr);   
  if (u != MATCH_YES) return u;      
      
  g95_init_expr = 1;         
  d = g95_resolve_expr(expr); 
  if (d == SUCCESS) d = check_init_expr(expr);  
  g95_init_expr = 0;       
       
  if (d == FAILURE) {  
    g95_free_expr(expr);      
    return MATCH_ERROR;    
  } 
 
  if (!g95_is_constant_expr(expr))      
    g95_internal_error("Initialization expression didn't reduce %C");        
        
  *r = expr;         
  return MATCH_YES;  
}    
    
    
          
          
/* external_spec_function()-- Make sure a non-intrinsic function is a
 * specification function. */     
     
static try external_spec_function(g95_expr *x) {     
g95_symbol *h;         
         
  h = x->symbol;    
    
  if (h->attr.proc == PROC_ST_FUNCTION) {         
    g95_error("Specification function '%s' at %L cannot be a statement "     
	      "function", h->name, &x->where);
    return FAILURE;   
  }

  if (h->attr.proc == PROC_INTERNAL) {      
    g95_error("Specification function '%s' at %L cannot be an internal "     
	      "function", h->name, &x->where);          
    return FAILURE;   
  }      
      
  if (!h->attr.pure) {       
    g95_error("Specification function '%s' at %L must be PURE", h->name,          
	      &x->where);       
    return FAILURE;   
  }          
          
  if (h->attr.recursive) {     
    g95_error("Specification function '%s' at %L cannot be RECURSIVE",          
	      h->name, &x->where);        
    return FAILURE;       
  }          
          
  return restricted_args(x->value.function.actual, 0);         
}       
       
       
          
          
/* g95_check_spec_expr()-- Check to see that an expression is a
 * specification expression.  If we return FAILURE, an error has been
 * generated. */          
          
try g95_specification_expr(g95_expr *w) {

  if (w == NULL) return SUCCESS;         
         
  if (g95_simplify_expr(w, 0) == FAILURE) return FAILURE;   
   
  if (g95_resolve_expr(w) == FAILURE) return FAILURE;   
   
  if (w->ts.type != BT_INTEGER) {        
    g95_error("Expression at %L must be of INTEGER type", &w->where); 
    return FAILURE;         
  }          
          
  if (w->rank != 0) {  
    g95_error("Expression at %L must be scalar", &w->where);        
    return FAILURE;
  }   
   
  return check_restricted(w);       
}  
  
  
          
          
/* g95_copy_shape()-- Copy a shape array. */          
          
mpz_t *g95_copy_shape(mpz_t *extent, int rnk) {        
mpz_t *new_shape; 
int u;       
       
  if (extent == NULL) return NULL;       
       
  new_shape = g95_get_shape(rnk);        
        
  for(u=0; u<rnk; u++)          
    mpz_init_set(new_shape[u], extent[u]);

  return new_shape;    
} 
 
 
 
 
/* restricted_array_inquiry()-- Check an array inquiry function as
 * part of a restricted expression.  The array argument does not have
 * to be a restricted variable, but the array must not be allocatable
 * or a nondummy pointer array. */

static try restricted_array_inquiry(g95_expr *z) {    
g95_expr *ap;   
g95_symbol *s;        
        
  ap = z->value.function.actual->u.expr;    
    
  if (ap->type == EXPR_VARIABLE) {         
    s = ap->symbol;       
       
    if (s->attr.allocatable || (s->attr.pointer && !s->attr.dummy)) {    
      g95_error("Unknown bounds for array '%s' in restricted expression at %C",     
		s->name);      
      return FAILURE;    
    }   
  }          
          
  return restricted_args(z->value.function.actual->next, 0);   
}         
         
         
 
 
/* g95_kind_max()-- Return the maximum kind of two expressions.
 * Higher kind numbers mean more precision for numeric types. */    
    
int g95_kind_max(g95_expr *g, g95_expr *s) {  
  
  return (g->ts.kind > s->ts.kind) ? g->ts.kind : s->ts.kind;        
}   
   
   
       
       
/* simplify_constructor()-- Subroutine to simplify constructor
 * expressions.  Mutually recursive with g95_simplify_expr(). */      
      
static try simplify_constructor(g95_constructor *s, int t) {       
       
  for(;s; s=s->next) {    
    if (s->iterator &&          
	(g95_simplify_expr(s->iterator->start, t) == FAILURE ||  
	 g95_simplify_expr(s->iterator->end,   t) == FAILURE ||
	 g95_simplify_expr(s->iterator->step,  t) == FAILURE))      
      return FAILURE;       
       
    if (s->expr && g95_simplify_expr(s->expr, t) == FAILURE) return FAILURE;      
  }     
     
  return SUCCESS;
} 
 
 
       
       
/* et0()-- Returns the type of an expression with the exception that
 * iterator variables are automatically integers no matter what else
 * they may be declared as. */  
  
static bt et0(g95_expr *r) {    
    
  if (r->type == EXPR_VARIABLE && g95_check_iter_variable(r) == SUCCESS) 
    return BT_INTEGER;   
   
  return r->ts.type;     
}        
        
        
  
  
/* g95_free_expr()-- Free an expression node and everything beneath it. */

void g95_free_expr(g95_expr *m) {       
       
  if (m == NULL) return;        
        
  g95_free_expr0(m);
  g95_free(m);    
}


      
      
/* g95_extract_int()-- Tries to extract an integer constant from the
 * passed expression node.  Returns an error message or NULL if the
 * result is set.  It is tempting to generate an error and return
 * SUCCESS or FAILURE, but failure is OK for some callers. */

char *g95_extract_int(g95_expr *e, int *rslt) {         
         
  if (e->type != EXPR_CONSTANT)
    return "Constant expression required at %C";      
      
  if (e->ts.type != BT_INTEGER)          
    return "Integer expression required at %C";     
     
  if ((mpz_cmp_si(e->value.integer, INT_MAX) > 0) ||         
      (mpz_cmp_si(e->value.integer, INT_MIN) < 0)) {
    return "Integer value too large in expression at %C"; 
  }          
          
  *rslt = (int) mpz_get_si(e->value.integer);  
  
  return NULL;   
}       
       
       
  
  
/* show_ref()-- Show a string of g95_ref structures. */ 
 
static void show_ref(g95_ref *e) {        
        
  for(; e; e=e->next)        
    switch(e->type) { 
    case REF_ARRAY:         
      g95_show_array_ref(&e->u.ar);  
      break;         
         
    case REF_COMPONENT:     
      g95_status(" %% %s", e->u.c.component->name);   
      break; 
 
    case REF_SUBSTRING:         
      g95_status_char('(');     
      g95_show_expr(e->u.ss.start);       
      g95_status_char(':');         
      g95_show_expr(e->u.ss.end);      
      g95_status_char(')');   
      break;      
      
    default:       
      g95_internal_error("show_ref(): Bad component code");    
    }         
}     
     
     
          
          
/* restricted_elemental()-- Check an intrinsic function call to see if
 * it is an elemental function returning character or integer with
 * character and integer arguments.  This function is a catchall for
 * most of the restricted intrinsics. */        
        
static try restricted_elemental(g95_expr *d) {   
g95_intrinsic_sym *symbol;    
    
  symbol = d->value.function.isym; 

  if (!symbol->elemental) {         
    g95_error("Intrinsic function '%s' in specification expression at %C"          
	      "must be ELEMENTAL", symbol->name);        
    return FAILURE;      
  }     
     
  if (symbol->ts.type != BT_INTEGER && symbol->ts.type != BT_CHARACTER) {   
    g95_error("Intrinsic function '%s' in specification expression at %C"   
	      "must return INTEGER or CHARACTER", symbol->name);   
    return FAILURE;         
  }          
          
  return restricted_args(d->value.function.actual->next, 1);        
}         
         
         
  
  
/* check_intrinsic_op()-- Check an intrinsic arithmetic operation to
 * see if it is consistent with some type of expression. */       
       
static try check_intrinsic_op(g95_expr *v, try (*check_function)(g95_expr *)) {     
      
  if ((*check_function)(v->op1) == FAILURE) return FAILURE;   
   
  switch(v->operator) {          
  case INTRINSIC_UPLUS: 
  case INTRINSIC_UMINUS:     
    if (!numeric_type(et0(v->op1))) goto not_numeric;        
    break;     
     
  case INTRINSIC_EQ:  case INTRINSIC_NE:  case INTRINSIC_GT:         
  case INTRINSIC_GE:  case INTRINSIC_LT:  case INTRINSIC_LE: 
 
  case INTRINSIC_PLUS:    case INTRINSIC_MINUS:  case INTRINSIC_TIMES:   
  case INTRINSIC_DIVIDE:  case INTRINSIC_POWER:        
    if ((*check_function)(v->op2) == FAILURE) return FAILURE;  
  
    if (!numeric_type(et0(v->op1)) || 
	!numeric_type(et0(v->op2))) goto not_numeric;      
      
    if (v->operator != INTRINSIC_POWER) break;
    
    if (check_function == check_init_expr && et0(v->op2) != BT_INTEGER) {       
      g95_error("Exponent at %L must be INTEGER for an initialization "        
		"expression", &v->op2->where);   
      return FAILURE;          
    }         
         
    break;       
       
  case INTRINSIC_CONCAT:       
    if ((*check_function)(v->op2) == FAILURE) return FAILURE;    
    
    if (et0(v->op1) != BT_CHARACTER || et0(v->op2) != BT_CHARACTER) {
      g95_error("Concatenation operator in expression at %L "        
		"must have two CHARACTER operands", &v->op1->where);        
      return FAILURE;
    }

    if (v->op1->ts.kind != v->op2->ts.kind) {         
      g95_error("Concat operator at %L must concatenate strings of the "     
		"same kind", &v->where);        
      return FAILURE; 
    } 
 
    break;   
   
  case INTRINSIC_NOT:    
    if (et0(v->op1) != BT_LOGICAL) { 
      g95_error(".NOT. operator in expression at %L must have a LOGICAL "  
		"operand", &v->op1->where);    
      return FAILURE;  
    }        
        
    break;      
      
  case INTRINSIC_AND:    case INTRINSIC_OR:          
  case INTRINSIC_EQV:    case INTRINSIC_NEQV:          
    if ((*check_function)(v->op2) == FAILURE) return FAILURE;         
         
    if (et0(v->op1) != BT_LOGICAL || et0(v->op2) != BT_LOGICAL) {          
      g95_error("LOGICAL operands are required in expression at %L",   
		&v->where);        
      return FAILURE;         
    }       
       
    break;     
     
  default:
    g95_error("Only intrinsic operators can be used in expression at %L",     
	      &v->where);          
    return FAILURE;       
  }      
      
  return SUCCESS;       
       
not_numeric:  
  g95_error("Numeric operands are required in expression at %L", &v->where);

  return FAILURE;         
}         
         
         
 
 
/* restricted_nonelemental()-- Check an intrinsic function that does
 * not have to be elemental, yet requires integer or character
 * arguements. */        
        
static try restricted_nonelemental(g95_expr *n) {      
      
  return restricted_args(n->value.function.actual, 1);      
}   
   
   
    
    
/* g95_int_expr()-- Returns an expression node that is an integer
 * constant. */ 
 
g95_expr *g95_int_expr(int y) {   
g95_expr *a;        
        
  a = g95_get_expr();       
       
  a->type = EXPR_CONSTANT;      
  a->ts.type = BT_INTEGER;        
  a->ts.kind = g95_default_integer_kind();

  a->where = *g95_current_locus();   
  mpz_init_set_si(a->value.integer, y);         
         
  return a;  
}         
         
         
   
   
/* g95_replace_expr()-- grafts the *src expression onto the *dest
 * subexpression. */

void g95_replace_expr(g95_expr *dest, g95_expr *s0) {       
       
  g95_free_expr0(dest);      
  *dest = *s0;         
         
  g95_free(s0);   
}    
    
    


/* g95_numeric_ts()-- Returns nonzero if the typespec is a numeric
 * type, zero otherwise. */   
   
int g95_numeric_ts(g95_typespec *typesp) {     
     
  return numeric_type(typesp->type);  
}        
        
        
 
 
/* show_constructor()-- Display a constructor.  Works recursively for
 * array constructors. */         
         
static void show_constructor(g95_constructor *b) {    
    
  for(;b ;b=b->next) {  
    if (b->iterator == NULL)     
      g95_show_expr(b->expr);   
    else {   
      g95_status_char('(');    
      g95_show_expr(b->expr);    
    
      g95_status_char(' ');      
      g95_show_expr(b->iterator->var);
      g95_status_char('='); 
      g95_show_expr(b->iterator->start);     
      g95_status_char(',');          
      g95_show_expr(b->iterator->end);          
      g95_status_char(','); 
      g95_show_expr(b->iterator->step);          
          
      g95_status_char(')');          
    }       
       
    if (b->next != NULL) g95_status(" , ");         
  }         
}


    
    
/* g95_copy_expr()-- Given an expression pointer, return a copy of the
 * expression.  This subroutine is recursive. */ 
 
g95_expr *g95_copy_expr(g95_expr *w) {  
g95_expr *y;          
int l;    
char *f;        
        
  if (w == NULL) return NULL;        
        
  y = g95_get_expr();
  *y = *w;     
     
  switch(y->type) {        
  case EXPR_SUBSTRING:         
    f = g95_getmem(w->value.character.length+1);         
    y->value.character.string = f;     
     
    memcpy(f, w->value.character.string, w->value.character.length+1);     
    break;       
       
  case EXPR_CONSTANT:       
    switch(y->ts.type) {   
    case BT_INTEGER:     
      mpz_init_set(y->value.integer, w->value.integer);
      break;        
        
    case BT_REAL:     
      mpf_init_set(y->value.real, w->value.real);    
      break;    
    
    case BT_COMPLEX:         
      mpf_init_set(y->value.complex.r, w->value.complex.r);      
      mpf_init_set(y->value.complex.i, w->value.complex.i); 
      break;

    case BT_CHARACTER: 
      l = w->value.character.length;    
    
      f = g95_getmem(l+1);     
      y->value.character.string = f;   
   
      if (l != 0) memcpy(f, w->value.character.string, l+1);          
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
    switch(y->operator) { 
    case INTRINSIC_NOT:       
    case INTRINSIC_UPLUS:          
    case INTRINSIC_UMINUS:      
      y->op1 = g95_copy_expr(w->op1);     
      break;          
          
    default:               /* Binary operators */          
      y->op1 = g95_copy_expr(w->op1);     
      y->op2 = g95_copy_expr(w->op2);      
      break;        
    }      
      
    break;       
       
  case EXPR_FUNCTION:         
    y->value.function.actual =   
      g95_copy_actual_arglist(w->value.function.actual); 
    break;      
      
  case EXPR_STRUCTURE:  
  case EXPR_ARRAY:   
    y->value.constructor = g95_copy_constructor(w->value.constructor);
    break; 
 
  case EXPR_VARIABLE:         
  case EXPR_UNKNOWN:  
  case EXPR_NULL:
    break;  
  }         
         
  y->shape = g95_copy_shape(w->shape, w->rank);     
     
  y->ref = copy_ref(w->ref);     
     
  return y;        
}        
        
        
   
   
/* g95_get_expr()-- Get a new expr node */  
  
g95_expr *g95_get_expr(void) {        
g95_expr *c;          
          
  c = g95_getmem(sizeof(g95_expr));     
     
  g95_clear_ts(&c->ts);         
  c->op1 = NULL;       
  c->op2 = NULL;  
  c->shape = NULL;
  c->ref = NULL; 
  c->symbol = NULL;        
  c->uop = NULL; 
 
  return c;          
}


          
          
/* restricted_null()-- Check the NULL() function as part of a
 * restricted expression.  NULL is always OK. */     
     
static try restricted_null(g95_expr *m) {          
          
  m = NULL; 
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
g95_actual_arglist *actualp;     
match f;    
try n;    
    
  if (e == NULL) return SUCCESS;         
         
  switch(e->type) { 
  case EXPR_OP:        
    n = check_intrinsic_op(e, check_init_expr);      
    if (n == SUCCESS) n = g95_simplify_expr(e, 0);    
    
    break;   
   
  case EXPR_FUNCTION:         
    n = SUCCESS;    
    
    if (check_inquiry(e) != SUCCESS) {          
      n = SUCCESS;      
      for(actualp=e->value.function.actual; actualp; actualp=actualp->next)        
	if (check_init_expr(actualp->u.expr) == FAILURE) { 
	  n = FAILURE;          
	  break;          
	}    
    }

    if (n == SUCCESS) {      
      f = g95_intrinsic_func_interface(e, 0);     
     
      if (f == MATCH_NO)    
	g95_error("Function '%s' in initialization expression at %L "
		  "must be an intrinsic function", e->symbol->name, &e->where);         
         
      if (f != MATCH_YES) n = FAILURE;   
    }          
          
    break;     
     
  case EXPR_VARIABLE:     
    n = SUCCESS;

    if (g95_check_iter_variable(e) == SUCCESS) break;    
    
    if (e->symbol->attr.flavor == FL_PARAMETER) {      
      g95_replace_expr(e, g95_copy_expr(e->symbol->value));        
      break;      /* TODO: constant references to subobjects */     
    }     
     
    g95_error("Variable '%s' at %L cannot appear in an initialization "     
	      "expression", e->symbol->name, &e->where);      
    n = FAILURE;   
    break;         
         
  case EXPR_CONSTANT:       
  case EXPR_NULL:          
    n = SUCCESS;        
    break;       
       
  case EXPR_SUBSTRING: 
    n = check_init_expr(e->ref->u.ss.start);         
    if (n == FAILURE) break; 
 
    n = check_init_expr(e->ref->u.ss.end);  
    if (n == SUCCESS) n = g95_simplify_expr(e, 0); 
 
    break;   
   
  case EXPR_STRUCTURE:    
    n = g95_check_constructor(e, check_init_expr);        
    break;      
      
  case EXPR_ARRAY:
    n = g95_check_constructor(e, check_init_expr);        
    if (n == FAILURE) break;   
   
    n = g95_check_constructor_type(e);
    break;          
              
  default:
    g95_internal_error("check_init_expr(): Unknown expression type");       
  }        
        
  return n;       
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
g95_ref *re;          
          
  if (p == NULL) return SUCCESS;

/* Replace a parameter variable with its value */ 
 
  switch(p->type) {     
  case EXPR_CONSTANT:         
  case EXPR_UNKNOWN:  
  case EXPR_NULL:     
    break;        
        
  case EXPR_FUNCTION:        
    for(ap=p->value.function.actual; ap; ap=ap->next)     
      if (g95_simplify_expr(ap->u.expr, type) == FAILURE) return FAILURE;

    if (p->value.function.isym != NULL &&   
	g95_intrinsic_func_interface(p, 1) == MATCH_ERROR) return FAILURE;   
   
    break;     
     
  case EXPR_OP:        
    if (simplify_intrinsic_op(p, type) == FAILURE) return FAILURE;         
    break;        
        
  case EXPR_VARIABLE:     
    if (p->symbol->attr.flavor == FL_PARAMETER &&  
	g95_check_parameter(p->symbol) == FAILURE) return FAILURE;   
   
    if (p->symbol->attr.flavor == FL_PARAMETER &&   
	p->symbol->as == NULL &&      
	p->symbol->value->type != EXPR_STRUCTURE) {         
      re = p->ref;    
      p->ref = NULL; 
      g95_replace_expr(p, g95_copy_expr(p->symbol->value));      
      p->ref = re;       
      break;   
    } 
 
    if (type == 1) g95_simplify_iterator_var(p);       
       
    /* Fall through */         
         
  case EXPR_SUBSTRING: 
    for(re=p->ref; re; re=re->next)       
      if (simplify_ref(re, type) == FAILURE) return FAILURE;    
    
    break;   
   
  case EXPR_STRUCTURE: 
  case EXPR_ARRAY:  
    if (simplify_constructor(p->value.constructor, type) == FAILURE)     
      return FAILURE;    
    
    break;   
  }        
        
  return SUCCESS;     
}   
   
   
      
      
/* restricted_len()-- Check the LEN() function as part of a restricted
 * expression.  The argument of LEN can be a character variable that
 * isn't a restricted expression, but in which the length is a
 * restricted expression.  If the variable already exists, then the
 * length was already a specification expression. */    
    
static try restricted_len(g95_expr *a) {    
g95_expr *argum;        
        
  argum = a->value.function.actual->u.expr;  
  
  if (argum->type == EXPR_VARIABLE && argum->ts.type == BT_CHARACTER)   
    return SUCCESS;          
          
  return restricted_args(a->value.function.actual, 1);
}    
    
    
    
    
/* g95_char_expr()-- Return an expression node that is a character constant. */      
      
g95_expr *g95_char_expr(int leng, int k, locus *w) {     
g95_charlen *c;     
g95_expr *i;     
     
  c = g95_get_charlen();        
  c->length = g95_int_expr(leng);      
  c->next = g95_current_ns->cl_list;      
  g95_current_ns->cl_list = c; 
 
  i = g95_get_expr();  
  
  i->type = EXPR_CONSTANT;   
  i->ref = NULL;       
  i->ts.type = BT_CHARACTER;      
  i->ts.kind = k;          
  i->ts.cl = c;         
         
  if (w == NULL) w = g95_current_locus();       
  i->where = *w;         
         
  i->value.character.string = g95_getmem(leng+1); 
  i->value.character.length = leng;       
       
  memset(i->value.character.string, ' ', leng);     
  i->value.character.string[leng] = '\0'; 
 
  return i; 
}       
       
       
   
   
/* g95_show_expr()-- show an expression */

void g95_show_expr(g95_expr *f) {       
char *d;   
int t;     
     
  if (f == NULL) {  
    g95_status("()");          
    return;        
  }       
       
/* Show expression */        
        
  switch(f->type) {     
  case EXPR_SUBSTRING:
    d = f->value.character.string;         
         
    for(t=0; t<f->value.character.length; t++,d++) {  
      if (*d == '\'') g95_status("''");       
      else g95_status("%c", *d);      
    }          
          
    g95_status_char(' '); 
    g95_show_expr(f->ref->u.ss.start);    
    g95_status_char(' ');    
    g95_show_expr(f->ref->u.ss.end);    
    break;   
   
  case EXPR_STRUCTURE:
    g95_status("%s(", f->symbol->name);          
    show_constructor(f->value.constructor);      
    g95_status_char(')');        
    break;  
  
  case EXPR_ARRAY:  
    g95_status("(/ ");     
    show_constructor(f->value.constructor);  
    g95_status(" /)");       
       
    show_ref(f->ref); 
    break;

  case EXPR_NULL:        
    g95_status("NULL()"); 
    break;  
  
  case EXPR_CONSTANT:       
    switch(f->ts.type) {   
    case BT_INTEGER:    
      mpz_out_str(stdout, 10, f->value.integer);  
  
      if (f->ts.kind != g95_default_integer_kind())  
	g95_status("_%d", f->ts.kind);
      break;  
  
    case BT_LOGICAL:       
      if (f->value.logical) g95_status(".true.");
       else g95_status(".false.");        
      break;

    case BT_REAL:          
      mpf_out_str(stdout, 10, 0, f->value.real);   
      if (f->ts.kind != g95_default_real_kind())         
	g95_status("_%d", f->ts.kind);        
      break;       
       
    case BT_CHARACTER:         
      d = f->value.character.string;        
        
      g95_status_char('\'');         
         
      for(t=0; t<f->value.character.length; t++,d++) {
	if (*d == '\'') g95_status("''");
	else g95_status_char(*d);     
      }      
      
      g95_status_char('\'');  
  
      break;         
         
    case BT_COMPLEX:  
      g95_status("(complex ");        
        
      mpf_out_str(stdout, 10, 0, f->value.complex.r);  
      if (f->ts.kind != g95_default_complex_kind())   
	g95_status("_%d", f->ts.kind);

      g95_status(" ");    
    
      mpf_out_str(stdout, 10, 0, f->value.complex.i);  
      if (f->ts.kind != g95_default_complex_kind())         
	g95_status("_%d", f->ts.kind);      
      
      g95_status(")");          
      break;   
   
    default:
      g95_status("???");     
      break;      
    }        
        
    break;    
    
  case EXPR_VARIABLE:
    if (f->symbol->ns->proc_name->name != NULL)   
      g95_status("%s:%s", f->symbol->ns->proc_name->name, f->symbol->name);      
    else    
      g95_status(":%s", f->symbol->name);        
        
    show_ref(f->ref); 
    break;   
   
  case EXPR_OP:   
    g95_status("("); 
    switch(f->operator) {
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
  
    g95_show_expr(f->op1);

    if (f->op2) {     
      g95_status(" ");    
      g95_show_expr(f->op2); 
    }        
        
    g95_status(")");     
    break;          
          
  case EXPR_FUNCTION:
    if (f->value.function.isym == NULL) {        
      g95_status("%s:%s[", f->symbol->module, f->symbol->name);       
      g95_show_actual_arglist(f->value.function.actual); 
      g95_status_char(']');       
    } else { 
      g95_status("%s[[", f->value.function.name);  
      g95_show_actual_arglist(f->value.function.actual);   
      g95_status_char(']');     
      g95_status_char(']');
    }   
   
    break;   
   
  case EXPR_UNKNOWN:    
    g95_status("UNK %s", f->symbol->name);     
    break;         
         
  default: g95_internal_error("g95_show_expr(): Don't know how to show expr");  
  }        
}      
      
      
/* restricted_intrinsic()-- Check to see that a function reference to
 * an intrinsic is a restricted expression.  Some functions required
 * by the standard are omitted because references to them have already
 * been simplified.  Strictly speaking, a lot of these checks are
 * redundant with other checks.  If a function is indeed a particular
 * intrinsic, then the type of its argument have already been checked
 * and passed. */       
       
static try restricted_intrinsic(g95_expr *f) {       
       
static struct { char *fname; try (*function)(g95_expr *f); } *cp, cases[] = {       
  { "repeat",              restricted_nonelemental },   
  { "reshape",             restricted_nonelemental },
  { "selected_int_kind",   restricted_nonelemental },         
  { "selected_real_kind",  restricted_nonelemental }, 
  { "transfer",            restricted_nonelemental },   
  { "trim",                restricted_nonelemental }, 
 
  { "min",   restricted_nonelemental },     
  { "max",   restricted_nonelemental },    
    
  { "null",  restricted_null },   
   
  { "shape",  restricted_array_inquiry },    
  { "size",   restricted_array_inquiry },        
  { "lbound", restricted_array_inquiry },      
  { "ubound", restricted_array_inquiry },   
   
  /* bit_size() has already been reduced */     
     
  { "len",    restricted_len },       
       
  /* kind() has already been reduced */  
  /* Numeric inquiry functions have been reduced */ 
 
  { NULL, restricted_elemental } };    
    
char *nm;       
       
  nm = f->value.function.isym->name;        
        
  for(cp=cases; cp->fname; cp++)          
    if (cp->fname == NULL || strcmp(cp->fname, nm) == 0) break;         
         
  return cp->function(f);          
}    
    
    
         
         
/* check_restricted()-- Verify that an expression is a restricted
 * expression.  Like its cousin check_init_expr(), an error message is
 * generated if we return FAILURE. */    
    
static try check_restricted(g95_expr *d) {   
g95_symbol *symb;   
try a;          
          
  if (d == NULL) return SUCCESS;

  a = SUCCESS;          
          
  switch(d->type) {  
  case EXPR_OP:    
    a = check_intrinsic_op(d, check_restricted);       
    if (a == SUCCESS) a = g95_simplify_expr(d, 0);        
        
    break;     
     
  case EXPR_FUNCTION:          
    a = (d->value.function.isym != NULL) ?       
      restricted_intrinsic(d) : external_spec_function(d);        
        
    break;  
  
  case EXPR_VARIABLE:        
    symb = d->symbol;         
    a = FAILURE;   
       
    if (symb->attr.optional) {       
      g95_error("Dummy argument '%s' at %L cannot be OPTIONAL",      
		symb->name, &d->where);      
      break;    
    }    
    
    if (symb->attr.intent == INTENT_OUT) {    
      g95_error("Dummy argument '%s' at %L cannot be INTENT(OUT)",    
		symb->name, &d->where);     
      break;   
    }      
      
    if (symb->attr.in_common || symb->attr.use_assoc || symb->attr.dummy ||     
	symb->ns != g95_current_ns ||          
	(symb->ns->proc_name != NULL &&       
	 symb->ns->proc_name->attr.flavor == FL_MODULE)) {     
      a = SUCCESS; 
      break;         
    }

    if (symb->attr.flavor == FL_PARAMETER) {         
      a = SUCCESS;         
      break;         
    }      
      
    g95_error("Variable '%s' cannot appear in restricted expression at %L", 
	      symb->name, &d->where);    
    break;     
     
  case EXPR_NULL:          
  case EXPR_CONSTANT:          
    a = SUCCESS;    
    break;       
       
  case EXPR_SUBSTRING:     
    a = g95_specification_expr(d->ref->u.ss.start);     
    if (a == FAILURE) break;    
    
    a = g95_specification_expr(d->op2);       
    if (a == SUCCESS) a = g95_simplify_expr(d, 0);  
  
    break;         
         
  case EXPR_STRUCTURE: 
    a = g95_check_constructor(d, check_restricted);   
    break;       
       
  case EXPR_ARRAY:  
    a = g95_check_constructor(d, check_restricted);    
    break; 
 
  default:
    g95_internal_error("check_spec_expr(): Unknown expression type");        
  }   
   
  return a;        
}  
  
  
