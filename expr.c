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
       
void g95_show_actual_arglist(g95_actual_arglist *t) {      
      
  g95_status("(");     
     
  for(; t; t=t->next) {      
    g95_status_char('('); 
 
    if (t->type == ALT_RETURN)    
      g95_status("*%d", t->u.label->value); 
    else { 
      if (t->pointer) g95_status("P ");      
      
      switch(t->type) {         
      case FULL_ARRAY:     g95_status("F "); break;   
      case ARRAY_ELEMENT:  g95_status("E "); break;        
      case ARRAY_DESC:     g95_status("D "); break;        
      default:                               break;     
      }

      if (t->name[0] != '\0') g95_status("%s = ", t->name);   
   
      if (t->u.expr == NULL)   
	g95_status("(arg not-present)");         
      else     
	g95_show_expr(t->u.expr);      
    }     
     
    g95_status_char(')');         
    if (t->next != NULL) g95_status(" ");    
  }  
  
  g95_status(")");        
}        
        
        
 
 
/* show_constructor()-- Display a constructor.  Works recursively for
 * array constructors. */       
       
static void show_constructor(g95_constructor *x) { 
 
  for(;x ;x=x->next) {          
    if (x->iterator == NULL)  
      g95_show_expr(x->expr);  
    else {   
      g95_status_char('(');        
      g95_show_expr(x->expr); 
 
      g95_status_char(' ');          
      g95_show_expr(x->iterator->var);    
      g95_status_char('=');     
      g95_show_expr(x->iterator->start);         
      g95_status_char(',');  
      g95_show_expr(x->iterator->end);         
      g95_status_char(',');  
      g95_show_expr(x->iterator->step);         
         
      g95_status_char(')');   
    }   
   
    if (x->next != NULL) g95_status(" , ");    
  }
}       
       
       
     
     
/* g95_get_expr()-- Get a new expr node */     
     
g95_expr *g95_get_expr(void) {       
g95_expr *z;        
        
  z = g95_getmem(sizeof(g95_expr));        
        
  g95_clear_ts(&z->ts);     
  z->op1 = NULL;        
  z->op2 = NULL;      
  z->shape = NULL;          
  z->ref = NULL;     
  z->symbol = NULL;          
  z->uop = NULL;

  return z;  
}      
      
      
         
         
/* g95_logical_expr()-- Returns an expression node that is a logical
 * constant. */   
   
g95_expr *g95_logical_expr(int x, locus *where) { 
g95_expr *z;        
        
  z = g95_get_expr();     
     
  z->type = EXPR_CONSTANT;      
  z->ts.type = BT_LOGICAL;       
  z->ts.kind = g95_default_logical_kind();  
  
  if (where == NULL) where = g95_current_locus();  
  z->where = *where;   
  z->value.logical = x;      
      
  return z;        
}   
   
   
  
  
/* g95_free_ref_list()-- Free a list of reference structures */    
    
void g95_free_ref_list(g95_ref *e) {         
g95_ref *m;         
int i;      
      
  for(; e; e=m) {    
    m = e->next;     
     
    switch(e->type) {          
    case REF_ARRAY:      
      for(i=0; i<G95_MAX_DIMENSIONS; i++) {          
	g95_free_expr(e->u.ar.start[i]); 
	g95_free_expr(e->u.ar.end[i]);   
	g95_free_expr(e->u.ar.stride[i]);    
      }         
         
      break;

    case REF_SUBSTRING:        
      g95_free_expr(e->u.ss.start);  
      g95_free_expr(e->u.ss.end);        
      break;         
         
    case REF_COMPONENT:  
      break; 
    }        
        
    g95_free(e);  
  } 
} 
 
 
      
      
/* free_actual_arglist()-- Free an argument list and everything below it. */        
        
void g95_free_actual_arglist(g95_actual_arglist *x) {   
g95_actual_arglist *r;      
      
  while(x) {   
    r = x->next;
    if (x->type != ALT_RETURN) g95_free_expr(x->u.expr);  
  
    g95_free(x);          
    x = r;       
  }   
}   
   
   


/* g95_build_funcall()-- Return an expression node with an optional
 * argument list attached.  A variable number of g95_expr pointers are
 * strung together in an argument list with a NULL pointer terminating
 * the list. */

g95_expr *g95_build_funcall(g95_symbol *func, ...) {
g95_actual_arglist *tail;   
g95_expr *n, *b;       
va_list argp;      
      
  n = g95_get_expr();        
  n->type = EXPR_FUNCTION; 
  n->symbol = func; 
  n->value.function.actual = NULL;     
     
  tail = NULL;

  va_start(argp, func);
  for(;;) {    
    b = va_arg(argp, g95_expr *);     
    if (b == NULL) break;      
      
    if (tail == NULL)      
      n->value.function.actual = tail = g95_get_actual_arglist();         
    else { 
      tail->next = g95_get_actual_arglist();       
      tail = tail->next;  
    }      
      
    tail->type = EXPR;      
    tail->u.expr = b;     
  }         
         
  va_end(argp);  
  
  return n; 
}          
          
          
      
      
/* g95_free_expr0()-- Workhorse function for g95_free_expr() that
 * frees everything beneath an expression node, but not the node
 * itself.  This is useful when we want to simplify a node and replace
 * it with something else or the expression node belongs to another
 * structure.  */        
        
static void g95_free_expr0(g95_expr *h) {      
int u; 
 
  switch(h->type) {
  case EXPR_CONSTANT:    
    switch(h->ts.type) {    
    case BT_INTEGER:        
      mpz_clear(h->value.integer);     
      break;    
    
    case BT_REAL:        
      mpf_clear(h->value.real);        
      break;   
   
    case BT_CHARACTER:    
      g95_free(h->value.character.string);         
      break;  
  
    case BT_COMPLEX:   
      mpf_clear(h->value.complex.r); 
      mpf_clear(h->value.complex.i);   
      break;       
       
    default:
      break;  
    } 
 
    break;      
      
  case EXPR_OP: 
    if (h->op1 != NULL) g95_free_expr(h->op1);  
    if (h->op2 != NULL) g95_free_expr(h->op2);   
    break;       
       
  case EXPR_FUNCTION:  
    g95_free_actual_arglist(h->value.function.actual);      
    break;   
   
  case EXPR_VARIABLE:        
    break;          
          
  case EXPR_ARRAY:   
  case EXPR_STRUCTURE:        
    g95_free_constructor(h->value.constructor);       
    break;    
    
  case EXPR_SUBSTRING: 
    g95_free(h->value.character.string);     
    break;       
       
  case EXPR_NULL:   
    break;   
   
  default:     
    g95_internal_error("g95_free_expr0(): Bad expr type");   
  }      
      
  /* Free a shape array */       
       
  if (h->shape != NULL) {        
    for(u=0; u<h->rank; u++)       
      mpz_clear(h->shape[u]);     
     
    g95_free(h->shape);    
  }   
   
  g95_free_ref_list(h->ref);     
     
  memset(h, '\0', sizeof(g95_expr));   
}     
     
     
        
        
/* g95_replace_expr()-- grafts the *src expression onto the *dest
 * subexpression. */          
          
void g95_replace_expr(g95_expr *dest, g95_expr *src) {      
      
  g95_free_expr0(dest);          
  *dest = *src;      
      
  g95_free(src);
}  
  
  
         
         
/* g95_copy_shape()-- Copy a shape array. */         
         
mpz_t *g95_copy_shape(mpz_t *shape, int rank) {  
mpz_t *new_shape;       
int g;     
     
  if (shape == NULL) return NULL;          
          
  new_shape = g95_get_shape(rank);         
         
  for(g=0; g<rank; g++) 
    mpz_init_set(new_shape[g], shape[g]);        
        
  return new_shape;          
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
    dest->u.ss = src->u.ss;     
    dest->u.ss.start = g95_copy_expr(src->u.ss.start);          
    dest->u.ss.end = g95_copy_expr(src->u.ss.end);     
    break;    
  }     
     
  dest->next = copy_ref(src->next); 
 
  return dest;         
}         
         
         
  
  
/* g95_kind_max()-- Return the maximum kind of two expressions.
 * Higher kind numbers mean more precision for numeric types. */  
  
int g95_kind_max(g95_expr *s, g95_expr *r) {

  return (s->ts.kind > r->ts.kind) ? s->ts.kind : r->ts.kind;          
}   
   
   
      
      
/* simplify_ref()-- Simplify an reference structure */     
     
static try simplify_ref(g95_ref *r, int flag) {          
try t;       
int p;         
         
  t = SUCCESS;   
  
  switch(r->type) {      
  case REF_ARRAY:          
    for(p=0; p<r->u.ar.dimen; p++) {         
      if (g95_simplify_expr(r->u.ar.start[p],  flag) == FAILURE) t = FAILURE;  
      if (g95_simplify_expr(r->u.ar.end[p],    flag) == FAILURE) t = FAILURE;      
      if (g95_simplify_expr(r->u.ar.stride[p], flag) == FAILURE) t = FAILURE;        
    }   
   
    break;     
     
  case REF_COMPONENT:      
    break;       
       
  case REF_SUBSTRING:    
    if (g95_simplify_expr(r->u.ss.start, flag) == FAILURE) t = FAILURE; 
    if (g95_simplify_expr(r->u.ss.end, flag)   == FAILURE) t = FAILURE;      
    break;
  }    
    
  return t;         
}     
     
     
        
        
/* g95_is_constant_expr()-- Function to determine if an expression is
 * constant or not.  This function expects that the expression has
 * already been simplified. */         
         
int g95_is_constant_expr(g95_expr *j) {        
g95_constructor *c;          
g95_actual_arglist *arg;         
int rv;    
    
  if (j == NULL) return 1;     
    
  switch(j->type) {      
  case EXPR_OP:     
    rv = g95_is_constant_expr(j->op1) &&   
      (j->op2 == NULL || g95_is_constant_expr(j->op2));       
       
    break;      
      
  case EXPR_VARIABLE:      
    rv = 0;      
    break;         
         
  case EXPR_FUNCTION:     
    rv = 0;        
    /* call to intrinsic with at least one argument */     
    if (j->value.function.isym && j->value.function.actual) {   
      for(arg = j->value.function.actual; arg; arg = arg->next){         
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
    rv = g95_is_constant_expr(j->op1) && g95_is_constant_expr(j->op2);        
    break;          
          
  case EXPR_STRUCTURE:          
    rv = 0;
    for(c=j->value.constructor; c; c=c->next)
      if (!g95_is_constant_expr(c->expr)) break;  
  
    if (c == NULL) rv = 1;        
    break;  
  
  case EXPR_ARRAY:         
    rv = g95_constant_ac(j);         
    break; 
 
  default:          
    g95_internal_error("g95_is_constant_expr(): Unknown expression type");     
  }  
  
  return rv;        
}     
     
     
         
         
/* restricted_args()-- Given an actual argument list, test to see that
 * each argument is a restricted expression and optionally if the
 * expression type is integer or character */         
         
static try restricted_args(g95_actual_arglist *e, int check_type) {    
bt type; 
 
  for(; e; e=e->next) {    
    if (check_restricted(e->u.expr) == FAILURE) return FAILURE;      
      
    if (!check_type) continue;       
       
    type = e->u.expr->ts.type;          
    if (type != BT_CHARACTER && type != BT_INTEGER) {         
      g95_error("Function argument at %L must be of type INTEGER or CHARACTER",
		&e->u.expr->where);      
      return FAILURE; 
    }         
  }  
  
  return SUCCESS;    
}


 
 
/* g95_copy_expr()-- Given an expression pointer, return a copy of the
 * expression.  This subroutine is recursive. */      
      
g95_expr *g95_copy_expr(g95_expr *p) {       
g95_expr *c;        
char *a;  
  
  if (p == NULL) return NULL;       
       
  c = g95_get_expr(); 
  *c = *p; 
 
  switch(c->type) {    
  case EXPR_SUBSTRING:        
    a = g95_getmem(p->value.character.length+1);         
    c->value.character.string = a;       
       
    memcpy(a, p->value.character.string, p->value.character.length+1); 
 
    c->op1 = g95_copy_expr(p->op1);  
    c->op2 = g95_copy_expr(p->op2);    
    break;       
       
  case EXPR_CONSTANT:      
    switch(c->ts.type) { 
    case BT_INTEGER:   
      mpz_init_set(c->value.integer, p->value.integer);   
      break;         
         
    case BT_REAL:    
      mpf_init_set(c->value.real, p->value.real);        
      break;     
     
    case BT_COMPLEX: 
      mpf_init_set(c->value.complex.r, p->value.complex.r);   
      mpf_init_set(c->value.complex.i, p->value.complex.i);       
      break;     
     
    case BT_CHARACTER: 
      a = g95_getmem(p->value.character.length+1); 
      c->value.character.string = a;        
        
      memcpy(a, p->value.character.string, p->value.character.length+1);       
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
    switch(c->operator) {
    case INTRINSIC_NOT:         
    case INTRINSIC_UPLUS:   
    case INTRINSIC_UMINUS: 
      c->op1 = g95_copy_expr(p->op1);         
      break;      
      
    default:               /* Binary operators */         
      c->op1 = g95_copy_expr(p->op1);     
      c->op2 = g95_copy_expr(p->op2);          
      break;
    }       
       
    break;    
    
  case EXPR_FUNCTION:    
    c->value.function.actual =        
      g95_copy_actual_arglist(p->value.function.actual);   
    break;         
         
  case EXPR_STRUCTURE:  
  case EXPR_ARRAY:   
    c->value.constructor = g95_copy_constructor(p->value.constructor); 
    break;      
      
  case EXPR_VARIABLE:      
  case EXPR_NULL:     
    break;  
  }          
          
  c->shape = g95_copy_shape(p->shape, p->rank);     
     
  c->ref = copy_ref(p->ref);         
         
  return c;     
}       
       
       
         
         
/* g95_int_expr()-- Returns an expression node that is an integer
 * constant. */  
  
g95_expr *g95_int_expr(int h) {
g95_expr *w;      
      
  w = g95_get_expr();        
        
  w->type = EXPR_CONSTANT;   
  w->ts.type = BT_INTEGER;     
  w->ts.kind = g95_default_integer_kind();    
    
  w->where = *g95_current_locus();
  mpz_init_set_si(w->value.integer, h);       
       
  return w;     
}    
    
    
   
   
/* external_spec_function()-- Make sure a non-intrinsic function is a
 * specification function. */

static try external_spec_function(g95_expr *t) {   
g95_symbol *c;      
      
  c = t->symbol;        
        
  if (c->attr.proc == PROC_ST_FUNCTION) {          
    g95_error("Specification function '%s' at %L cannot be a statement "       
	      "function", c->name, &t->where);         
    return FAILURE; 
  }

  if (c->attr.proc == PROC_INTERNAL) {      
    g95_error("Specification function '%s' at %L cannot be an internal "
	      "function", c->name, &t->where);   
    return FAILURE;          
  }       
       
  if (!c->attr.pure) {         
    g95_error("Specification function '%s' at %L must be PURE", c->name,      
	      &t->where); 
    return FAILURE;          
  }

  if (c->attr.recursive) {        
    g95_error("Specification function '%s' at %L cannot be RECURSIVE",         
	      c->name, &t->where);     
    return FAILURE;         
  }         
         
  return restricted_args(t->value.function.actual, 0);        
}


   
   
/* simplify_constructor()-- Subroutine to simplify constructor
 * expressions.  Mutually recursive with g95_simplify_expr(). */    
    
static try simplify_constructor(g95_constructor *g, int type) {       
       
  for(;g; g=g->next) {          
    if (g->iterator && 
	(g95_simplify_expr(g->iterator->start, type) == FAILURE ||       
	 g95_simplify_expr(g->iterator->end, type) == FAILURE ||     
	 g95_simplify_expr(g->iterator->step, type) == FAILURE))  
      return FAILURE;     
     
    if (g->expr && g95_simplify_expr(g->expr, type) == FAILURE) return FAILURE;      
  }        
        
  return SUCCESS;  
}     
     
     


/* et0()-- Returns the type of an expression with the exception that
 * iterator variables are automatically integers no matter what else
 * they may be declared as. */       
       
static bt et0(g95_expr *q) {  
  
  if (q->type == EXPR_VARIABLE && g95_check_iter_variable(q) == SUCCESS)  
    return BT_INTEGER;          
          
  return q->ts.type;      
}     
     
     
   
   
/* g95_type_convert_binary()-- Given an expression node with some sort of
 * numeric binary expression, insert type conversions required to make
 * the operands have the same type.
 *
 * The exception is that the operands of an exponential don't have to
 * have the same type.  If possible, the base is promoted to the type
 * of the exponent.  For example, 1**2.3 becomes 1.0**2.3, but
 * 1.0**2 stays as it is. */     
     
void g95_type_convert_binary(g95_expr *m) {       
g95_expr *op1, *op2;     
     
  op1 = m->op1;   
  op2 = m->op2;          
          
  if (op1->ts.type == BT_UNKNOWN || op2->ts.type == BT_UNKNOWN) {      
    g95_clear_ts(&m->ts);  
    return;     
  }        
        
/* Kind conversions */ 
 
  if (op1->ts.type == op2->ts.type) {          
          
    if (op1->ts.kind == op2->ts.kind) {  /* No type conversions */          
      m->ts = op1->ts;          
      goto done; 
    }          
          
    if (op1->ts.kind > op2->ts.kind)         
      g95_convert_type(op2, &op1->ts, 2);
    else       
      g95_convert_type(op1, &op2->ts, 2);      
      
    m->ts = op1->ts;   
    goto done;    
  }   
   
/* Real and integer combined with complex */

  if (op1->ts.type == BT_COMPLEX &&          
      (op2->ts.type == BT_REAL || op2->ts.type == BT_INTEGER)) {  
  
    m->ts.type = BT_COMPLEX;       
    m->ts.kind = op1->ts.kind;    
    
    if (m->operator == INTRINSIC_POWER) goto done;     
     
    g95_convert_type(m->op2, &m->ts, 2);      
    goto done;
  }    
    
  if (op2->ts.type == BT_COMPLEX &&  
      (op1->ts.type == BT_REAL || op1->ts.type == BT_INTEGER)) {       
       
    m->ts.type = BT_COMPLEX;
    m->ts.kind = op2->ts.kind;      
      
    g95_convert_type(m->op1, &m->ts, 2);    
    goto done;    
  } 
 
/* Integer combined with real */       
       
  if (op1->ts.type == BT_REAL && op2->ts.type == BT_INTEGER) {      
    m->ts.type = BT_REAL;         
    m->ts.kind = op1->ts.kind; 
 
    if (m->operator == INTRINSIC_POWER) goto done;        
        
    g95_convert_type(m->op2, &m->ts, 2);    
    goto done;         
  }        
        
  if (op1->ts.type == BT_INTEGER && op2->ts.type == BT_REAL) {     
    m->ts.type = BT_REAL;   
    m->ts.kind = op2->ts.kind;   
   
    g95_convert_type(m->op1, &m->ts, 2);
    goto done;     
  }   
   
done:    
  return; 
}  
  
  
     
     
/* numeric_type()-- Returns nonzero if the type is numeric, zero
 * otherwise */ 
 
static int numeric_type(bt type) { 
 
  return type == BT_COMPLEX || type == BT_REAL || type == BT_INTEGER;          
}        
        
        
  
  
/* g95_numeric_ts()-- Returns nonzero if the typespec is a numeric
 * type, zero otherwise. */  
  
int g95_numeric_ts(g95_typespec *ts) {   
   
  return numeric_type(ts->type);         
}   
   
   
  
  
/* g95_build_call()-- Build and return a g95_code structure that
 * corresponds to a subroutine call.  The arguments to this function
 * are a set of expression pointers (terminated by NULL) that compose
 * the actual arugment list. */         
         
g95_code *g95_build_call(char *sub_name, ...) {     
g95_actual_arglist *tail;       
g95_expr *expr;         
va_list argp;        
g95_code *k;   
  
  k = g95_get_code();         
         
  k->type = EXEC_CALL;         
  k->sub_name = sub_name;        
        
  tail = NULL;

  va_start(argp, sub_name);

  for(;;) {  
    expr = va_arg(argp, g95_expr *);         
    if (expr == NULL) break;          
          
    if (k->ext.actual == NULL) {     
      tail = g95_get_actual_arglist();   
      k->ext.actual = tail;       
    } else { 
      tail->next = g95_get_actual_arglist();     
      tail = tail->next;      
    }       
       
    tail->type = EXPR;     
    tail->u.expr = expr;     
  }  
  
  va_end(argp); 
 
  return k;    
}


       
       
/* g95_extract_int()-- Tries to extract an integer constant from the
 * passed expression node.  Returns an error message or NULL if the
 * result is set.  It is tempting to generate an error and return
 * SUCCESS or FAILURE, but failure is OK for some callers. */ 
 
char *g95_extract_int(g95_expr *expr, int *result) {        
        
  if (expr->type != EXPR_CONSTANT)          
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
        
        
 
 
/* g95_free_expr()-- Free an expression node and everything beneath it. */      
      
void g95_free_expr(g95_expr *g) {   
   
  if (g == NULL) return;          
          
  g95_free_expr0(g);        
  g95_free(g);         
}   
   
   


/* check_inquiry()-- Certain inquiry functions are specifically
 * allowed to have variable arguments, which is an exception to the
 * normal requirement that an initialization function have
 * initialization arguments.  We head off this problem here.  */       
       
static try check_inquiry(g95_expr *c) {     
char *name;      
static char *inquiry_function[] = {      
  "digits", "epsilon", "huge", "kind", "maxexponent", "minexponent",  
  "precision", "radix", "range", "tiny", "bit_size", "size", "shape",        
  "lbound", "ubound", NULL         
};

int p;      
      
  if (c->value.function.actual == NULL ||        
      c->value.function.actual->next != NULL)  /* Doesn't have one arg */  
    return FAILURE;    
    
  if (c->value.function.name != NULL &&        
      c->value.function.name[0] != '\0') return FAILURE;  
  
  name = c->symbol->name;

  for(p=0; inquiry_function[p]; p++)      
    if (strcmp(inquiry_function[p], name) == 0) break;   
   
  if (inquiry_function[p] == NULL) return FAILURE;     
     
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
      
      
        
        
/* check_intrinsic_op()-- Check an intrinsic arithmetic operation to
 * see if it is consistent with some type of expression. */         
         
static try check_intrinsic_op(g95_expr *r, try (*check_function)(g95_expr *)) { 
  
  if ((*check_function)(r->op1) == FAILURE) return FAILURE;   
   
  switch(r->operator) {        
  case INTRINSIC_UPLUS:         
  case INTRINSIC_UMINUS:     
    if (!numeric_type(et0(r->op1))) goto not_numeric;      
    break;      
      
  case INTRINSIC_EQ:  case INTRINSIC_NE:  case INTRINSIC_GT:      
  case INTRINSIC_GE:  case INTRINSIC_LT:  case INTRINSIC_LE:         
         
  case INTRINSIC_PLUS:    case INTRINSIC_MINUS:  case INTRINSIC_TIMES:      
  case INTRINSIC_DIVIDE:  case INTRINSIC_POWER:
    if ((*check_function)(r->op2) == FAILURE) return FAILURE;       
       
    if (!numeric_type(et0(r->op1)) ||      
	!numeric_type(et0(r->op2))) goto not_numeric;  
  
    if (r->operator != INTRINSIC_POWER) break;         
             
    if (check_function == check_init_expr && et0(r->op2) != BT_INTEGER) {     
      g95_error("Exponent at %L must be INTEGER for an initialization "    
		"expression", &r->op2->where);          
      return FAILURE;      
    }  
  
    break;      
      
  case INTRINSIC_CONCAT:  
    if ((*check_function)(r->op2) == FAILURE) return FAILURE;

    if (et0(r->op1) != BT_CHARACTER || et0(r->op2) != BT_CHARACTER) { 
      g95_error("Concatenation operator in expression at %L "       
		"must have two CHARACTER operands", &r->op1->where);     
      return FAILURE;    
    }  
  
    if (r->op1->ts.kind != r->op2->ts.kind) {
      g95_error("Concat operator at %L must concatenate strings of the "      
		"same kind", &r->where);     
      return FAILURE;          
    }       
       
    break;    
    
  case INTRINSIC_NOT:
    if (et0(r->op1) != BT_LOGICAL) {          
      g95_error(".NOT. operator in expression at %L must have a LOGICAL "   
		"operand", &r->op1->where);          
      return FAILURE;    
    }    
    
    break;     
     
  case INTRINSIC_AND:    case INTRINSIC_OR:       
  case INTRINSIC_EQV:    case INTRINSIC_NEQV:      
    if ((*check_function)(r->op2) == FAILURE) return FAILURE;      
      
    if (et0(r->op1) != BT_LOGICAL || et0(r->op2) != BT_LOGICAL) {      
      g95_error("LOGICAL operands are required in expression at %L",  
		&r->where);          
      return FAILURE;   
    }      
      
    break;       
       
  default:     
    g95_error("Only intrinsic operators can be used in expression at %L",       
	      &r->where);    
    return FAILURE;  
  } 
 
  return SUCCESS;      
      
not_numeric: 
  g95_error("Numeric operands are required in expression at %L", &r->where);         
         
  return FAILURE;        
}


      
      
/* restricted_intrinsic()-- Check to see that a function reference to
 * an intrinsic is a restricted expression.  Some functions required
 * by the standard are omitted because references to them have already
 * been simplified.  Strictly speaking, a lot of these checks are
 * redundant with other checks.  If a function is indeed a particular
 * intrinsic, then the type of its argument have already been checked
 * and passed. */  
  
static try restricted_intrinsic(g95_expr *k) {  
g95_intrinsic_sym *symb;        
        
static struct { char *fname; int case_number; } *cp, cases[] = {     
  { "repeat", 0 },             { "reshape", 0 },  { "selected_int_kind", 0 },      
  { "selected_real_kind", 0 }, { "transfer", 0 }, { "trim", 0 },       
       
  { "null", 1 },   
   
  { "lbound", 2 }, { "shape", 2 }, { "size", 2 }, { "ubound", 2 },      
        
  /* bit_size() has already been reduced */

  { "len", 0 },    
    
  /* kind() has already been reduced */         
  /* Numeric inquiry functions have been reduced */      
      
  { NULL, 0 } };        
        
try g;       
       
  symb = k->value.function.isym;       
  if (symb->elemental) return restricted_args(k->value.function.actual, 1); 
 
  for(cp=cases; cp->fname; cp++)          
    if (strcmp(cp->fname, symb->name) == 0) break;  
  
  if (cp->fname == NULL) {         
    g95_error("Intrinsic function '%s' at %L is not a restricted function",  
	      symb->name, &k->where);         
    return FAILURE; 
  }      
      
  switch(cp->case_number) {  
  case 0:   
    /* Functions that are restricted if they have character/integer args */        
        
    g = restricted_args(k->value.function.actual, 1);         
    break;        
        
  case 1:  /* NULL() */         
    g = SUCCESS;  
    break;   
   
  case 2:      
    /* Functions that could be checking the bounds of an assumed-size array */     
     
    g = SUCCESS;     /* TODO: implement checks from 7.1.6.2 (10) */          
    break;        
        
  default: 
    g95_internal_error("restricted_intrinsic(): Bad case");  
  }      
      
  return g;          
}


  
  
/* simplify_intrinsic_op()-- Try to collapse intrinsic expressions */  
  
static try simplify_intrinsic_op(g95_expr *g, int type) { 
g95_expr *op1, *op2, *result;       
       
  if (g->operator == INTRINSIC_USER) return SUCCESS;      
      
  op1 = g->op1;       
  op2 = g->op2; 
 
  if (g95_simplify_expr(op1, type) == FAILURE) return FAILURE;
  if (g95_simplify_expr(op2, type) == FAILURE) return FAILURE;   
   
  if (!g95_is_constant_expr(op1) ||
      (op2 != NULL && !g95_is_constant_expr(op2)))    
    return SUCCESS;  
  
/* Rip p apart */    
    
  g->op1 = NULL;  
  g->op2 = NULL;  
  
  switch(g->operator) {       
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
    if ((op2->ts.type == BT_INTEGER &&          
	 mpz_cmp_ui(op2->value.integer, 0) == 0) || 
	(op2->ts.type == BT_REAL &&  
	 mpf_cmp_ui(op2->value.real, 0) == 0) ||       
	(op2->ts.type == BT_COMPLEX &&      
	 mpf_cmp_ui(op2->value.complex.r, 0) == 0 &&      
	 mpf_cmp_ui(op2->value.complex.i, 0) == 0)) {   
      g->op1 = op1;
      g->op2 = op2;    
      return SUCCESS;       
    }    
    
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
    
  if (result == NULL) {
    g95_free_expr(op1);         
    g95_free_expr(op2); 
    return FAILURE;          
  }        
        
  g95_replace_expr(g, result);          
          
  return SUCCESS;     
}        
        
        
          
          
/* check_restricted()-- Verify that an expression is a restricted
 * expression.  Like its cousin check_init_expr(), an error message is
 * generated if we return FAILURE. */    
    
static try check_restricted(g95_expr *n) {      
g95_symbol *sym;       
try l;  
  
  if (n == NULL) return SUCCESS;    
    
  switch(n->type) {          
  case EXPR_OP:         
    l = check_intrinsic_op(n, check_restricted);
    if (l == SUCCESS) l = g95_simplify_expr(n, 0);       
       
    break;       
       
  case EXPR_FUNCTION:
    l = (n->value.function.isym != NULL) ?          
      restricted_intrinsic(n) : external_spec_function(n);         
         
    break;      
      
  case EXPR_VARIABLE:   
    sym = n->symbol;         
    l = FAILURE;        
            
    if (sym->attr.optional) {
      g95_error("Dummy argument '%s' at %L cannot be OPTIONAL",    
		sym->name, &n->where);        
      break;     
    } 
 
    if (sym->attr.intent == INTENT_OUT) { 
      g95_error("Dummy argument '%s' at %L cannot be INTENT(OUT)",       
		sym->name, &n->where);
      break;   
    }       
       
    if (sym->attr.in_common || sym->attr.use_assoc || sym->attr.dummy ||  
	sym->ns != g95_current_ns ||         
	(sym->ns->proc_name != NULL &&         
	 sym->ns->proc_name->attr.flavor == FL_MODULE)) {    
      l = SUCCESS;     
      break;          
    }       
       
    g95_error("Variable '%s' cannot appear in the expression at %L",  
	      sym->name, &n->where);       
       
    break;

  case EXPR_NULL:    
  case EXPR_CONSTANT: 
    l = SUCCESS;   
    break;          
          
  case EXPR_SUBSTRING:     
    l = g95_specification_expr(n->op1);         
    if (l == FAILURE) break;    
    
    l = g95_specification_expr(n->op2);
    if (l == SUCCESS) l = g95_simplify_expr(n, 0);  
  
    break;    
    
  case EXPR_STRUCTURE:     
    l = g95_check_constructor(n, check_restricted);  
    break;     
     
  case EXPR_ARRAY:      
    l = g95_check_constructor(n, check_restricted);     
    break;

  default:       
    g95_internal_error("check_spec_expr(): Unknown expression type");        
  }      
      
  return l;   
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
   
try g95_simplify_expr(g95_expr *q, int type) {      
g95_actual_arglist *ap;        
g95_ref *r;

  if (q == NULL) return SUCCESS;  
  
/* Replace a parameter variable with its value */        
        
  switch(q->type) { 
  case EXPR_CONSTANT:         
  case EXPR_NULL:    
    break;        
        
  case EXPR_FUNCTION:   
    for(ap=q->value.function.actual; ap; ap=ap->next)          
      if (g95_simplify_expr(ap->u.expr, type) == FAILURE) return FAILURE;        
        
    if (q->value.function.isym != NULL &&  
	g95_intrinsic_func_interface(q, 1) == MATCH_ERROR) return FAILURE;        
        
    break;     
     
  case EXPR_SUBSTRING:          
    if (g95_simplify_expr(q->op1, type) == FAILURE ||      
	g95_simplify_expr(q->op2, type) == FAILURE) return FAILURE;       
       
/* TODO: evaluate constant substrings */     
     
    break;     
     
  case EXPR_OP:      
    if (simplify_intrinsic_op(q, type) == FAILURE) return FAILURE;         
    break;   
   
  case EXPR_VARIABLE:   
    if (q->symbol->attr.flavor == FL_PARAMETER &&         
	q->symbol->value->type != EXPR_ARRAY) {     
      g95_replace_expr(q, g95_copy_expr(q->symbol->value));  
      break;      
    }         
         
    if (type == 1) g95_simplify_iterator_var(q);       
       
    for(r=q->ref; r; r=r->next) 
      if (simplify_ref(r, type) == FAILURE) return FAILURE;       
       
    break;         
         
  case EXPR_STRUCTURE:       
  case EXPR_ARRAY:      
    if (simplify_constructor(q->value.constructor, type) == FAILURE)       
      return FAILURE;    
    
    if (q->type == EXPR_ARRAY && g95_expand_constructor(q) == FAILURE)         
      return FAILURE;    
    
    break;     
  }      
      
  return SUCCESS;         
}       
       
       


/* g95_char_expr()-- Return an expression node that is a character constant. */          
          
g95_expr *g95_char_expr(int length, int kind, locus *where) {       
g95_expr *q;        
        
  q = g95_get_expr();         
         
  q->type = EXPR_CONSTANT;      
  q->ref = NULL; 
  q->ts.type = BT_CHARACTER; 
  q->ts.kind = kind;  
  
  if (where == NULL) where = g95_current_locus();        
  q->where = *where;    
    
  q->value.character.string = g95_getmem(length+1);   
  q->value.character.length = length;

  memset(q->value.character.string, ' ', length);       
  q->value.character.string[length] = '\0';        
        
  return q;      
}         
         
         
 
 
/* g95_check_spec_expr()-- Check to see that an expression is a
 * specification expression.  If we return FAILURE, an error has been
 * generated. */          
          
try g95_specification_expr(g95_expr *y) { 
 
  if (y->ts.type != BT_INTEGER) {  
    g95_error("Expression at %L must be of INTEGER type", &y->where);    
    return FAILURE;        
  }         
         
  if (y->rank != 0) {     
    g95_error("Expression at %L must be scalar", &y->where);
    return FAILURE;  
  }   
   
  return check_restricted(y);     
}         
         
         
      
      
/* g95_match_init_expr()-- Match an initialization expression.  We work
 * by first matching an expression, then reducing it to a constant */      
      
match g95_match_init_expr(g95_expr **result) {    
g95_expr *expr;
match a;          
try e; 
 
  a = g95_match_expr(&expr);        
  if (a != MATCH_YES) return a;    
    
  g95_init_expr = 1;          
  e = g95_resolve_expr(expr);        
  if (e == SUCCESS) e = check_init_expr(expr);        
  g95_init_expr = 0;       
       
  if (e == FAILURE) {
    g95_free_expr(expr);    
    return MATCH_ERROR;          
  }         
         
  if (expr->type == EXPR_ARRAY &&        
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
          
          
        
        
/* show_ref()-- Show a string of g95_ref structures. */    
    
static void show_ref(g95_ref *r) {  
  
  for(; r; r=r->next)    
    switch(r->type) {
    case REF_ARRAY:
      g95_show_array_ref(&r->u.ar);      
      break;        
        
    case REF_COMPONENT:     
      g95_status(" %% %s", r->u.c.component->name); 
      break;    
    
    case REF_SUBSTRING:        
      g95_status_char('(');        
      g95_show_expr(r->u.ss.start);    
      g95_status_char(':');
      g95_show_expr(r->u.ss.end);      
      g95_status_char(')');          
      break;       
       
    default: 
      g95_internal_error("show_ref(): Bad component code");   
    }
}     
     
     
          
          
/* g95_copy_arglist()-- Copy an arglist structure and all of the arguments. */  
  
g95_actual_arglist *g95_copy_actual_arglist(g95_actual_arglist *s) {     
g95_actual_arglist *head, *tail, *new; 
 
  head = tail = NULL;    
    
  for(; s; s=s->next) {      
    new = g95_get_actual_arglist(); 
    *new = *s;      
      
    if (s->type != ALT_RETURN) new->u.expr = g95_copy_expr(s->u.expr);  
  
    new->next = NULL;       
       
    if (head == NULL)          
      head = new;        
    else         
      tail->next = new;       
       
    tail = new; 
  }      
      
  return head;
}      
      
      
         
         
/* check_init_expr()-- Verify that an expression is an
 * initialization expression.  A side effect is that the expression
 * tree is reduced to a single constant node if all goes well.  This
 * would normally happen when the expression is constructed but
 * function references are assumed to be intrinsics in the context of
 * initialization expressions.  If FAILURE is returned an error
 * message has been generated. */  
  
static try check_init_expr(g95_expr *v) {    
g95_actual_arglist *ap;  
match m;  
try k;        
        
  if (v == NULL) return SUCCESS;

  switch(v->type) {   
  case EXPR_OP:        
    k = check_intrinsic_op(v, check_init_expr);   
    if (k == SUCCESS) k = g95_simplify_expr(v, 0);        
        
    break;  
  
  case EXPR_FUNCTION:       
    k = SUCCESS;      
      
    if (check_inquiry(v) != SUCCESS) {  
      k = SUCCESS;   
      for(ap=v->value.function.actual; ap; ap=ap->next)    
	if (check_init_expr(ap->u.expr) == FAILURE) {       
	  k = FAILURE;   
	  break; 
	}          
    }          
          
    if (k == SUCCESS) {   
      m = g95_intrinsic_func_interface(v, 0);

      if (m == MATCH_NO)     
	g95_error("Function '%s' in initialization expression at %L "         
		  "must be an intrinsic function", v->symbol->name, &v->where);       
       
      if (m != MATCH_YES) k = FAILURE;   
    }        
        
    break;     
     
  case EXPR_VARIABLE:       
    k = SUCCESS;     
     
    if (g95_check_iter_variable(v) == SUCCESS) break;        
        
    if (v->symbol->attr.flavor == FL_PARAMETER) {       
      g95_replace_expr(v, g95_copy_expr(v->symbol->value));
      break;      /* TODO: constant references to subobjects */      
    }      
      
    g95_error("Variable '%s' at %L cannot appear in an initialization "        
	      "expression", v->symbol->name, &v->where);
    k = FAILURE;        
    break;   
   
  case EXPR_CONSTANT:   
  case EXPR_NULL:       
    k = SUCCESS;       
    break;      
      
  case EXPR_SUBSTRING:      
    k = check_init_expr(v->op1);          
    if (k == FAILURE) break;

    k = check_init_expr(v->op2);
    if (k == SUCCESS) k = g95_simplify_expr(v, 0);

    break;    
    
  case EXPR_STRUCTURE:     
    k = g95_check_constructor(v, check_init_expr);          
    break;    
    
  case EXPR_ARRAY:          
    k = g95_check_constructor(v, check_init_expr);        
    if (k == FAILURE) break;     
     
    k = g95_expand_constructor(v);         
    if (k == FAILURE) break;      
      
    k = g95_check_constructor_type(v); 
    break;          
              
  default:        
    g95_internal_error("check_init_expr(): Unknown expression type");  
  }       
       
  return k;      
}  
  
  
        
        
/* g95_show_expr()-- show an expression */       
       
void g95_show_expr(g95_expr *v) {    
char *a;   
int i;    
    
  if (v == NULL) {
    g95_status("()");
    return;   
  }

/* Show expression */          
          
  switch(v->type) {        
  case EXPR_SUBSTRING:        
    a = v->value.character.string;    
    
    for(i=0; i<v->value.character.length; i++,a++) {        
      if (*a == '\'') g95_status("''");     
      else g95_status("%c", *a);      
    }         
         
    show_ref(v->ref);        
    break;

  case EXPR_STRUCTURE:          
    g95_status("%s(", v->symbol->name);         
    show_constructor(v->value.constructor);        
    g95_status_char(')');          
    break;

  case EXPR_ARRAY:
    g95_status("(/ ");  
    show_constructor(v->value.constructor);          
    g95_status(" /)");        
        
    show_ref(v->ref);        
    break;          
          
  case EXPR_NULL:   
    g95_status("NULL()");          
    break;      
      
  case EXPR_CONSTANT:        
    switch(v->ts.type) {     
    case BT_INTEGER:   
      mpz_out_str(stdout, 10, v->value.integer);          
          
      if (v->ts.kind != g95_default_integer_kind()) 
	g95_status("_%d", v->ts.kind);       
      break; 
 
    case BT_LOGICAL:  
      if (v->value.logical) g95_status(".true.");    
       else g95_status(".false.");    
      break;      
      
    case BT_REAL:   
      mpf_out_str(stdout, 10, 0, v->value.real);     
      if (v->ts.kind != g95_default_real_kind())
	g95_status("_%d", v->ts.kind);    
      break;   
   
    case BT_CHARACTER:     
      a = v->value.character.string; 
 
      g95_status_char('\'');        
        
      for(i=0; i<v->value.character.length; i++,a++) {  
	if (*a == '\'') g95_status("''");        
	else g95_status_char(*a);     
      }          
          
      g95_status_char('\''); 
 
      break;     
     
    case BT_COMPLEX:     
      g95_status("(complex ");        
        
      mpf_out_str(stdout, 10, 0, v->value.complex.r);      
      if (v->ts.kind != g95_default_complex_kind())      
	g95_status("_%d", v->ts.kind);  
  
      g95_status(" "); 
 
      mpf_out_str(stdout, 10, 0, v->value.complex.i);    
      if (v->ts.kind != g95_default_complex_kind())  
	g95_status("_%d", v->ts.kind);          
          
      g95_status(")");         
      break;  
  
    default:     
      g95_status("???");
      break;  
    }

    break;       
       
  case EXPR_VARIABLE:         
    g95_status("%s:%s", v->symbol->ns->proc_name->name, v->symbol->name);   
    show_ref(v->ref);          
    break;

  case EXPR_OP:      
    g95_status("(");         
    switch(v->operator) {    
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
     
    g95_show_expr(v->op1);        
        
    if (v->op2) {         
      g95_status(" ");          
      g95_show_expr(v->op2); 
    } 
 
    g95_status(")");    
    break;      
      
  case EXPR_FUNCTION:      
    if (v->value.function.isym == NULL) {   
      g95_status("%s:%s[", v->symbol->module, v->symbol->name);      
      g95_show_actual_arglist(v->value.function.actual);   
      g95_status_char(']');         
    } else {       
      g95_status("%s[[", v->value.function.name);      
      g95_show_actual_arglist(v->value.function.actual);         
      g95_status_char(']');
      g95_status_char(']');      
    }         
         
    break;          
          
  default: g95_internal_error("g95_show_expr(): Don't know how to show expr");         
  }        
}      
