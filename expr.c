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
          
          
 
 
/* simplify_intrinsic_op()-- Try to collapse intrinsic expressions */ 
 
static try simplify_intrinsic_op(g95_expr *h, int typ) {      
g95_expr *op, *op2, *result; 
 
  if (h->operator == INTRINSIC_USER) return SUCCESS;          
          
  op = h->op1;         
  op2 = h->op2;     
     
  if (g95_simplify_expr(op, typ) == FAILURE) return FAILURE; 
  if (g95_simplify_expr(op2, typ) == FAILURE) return FAILURE;    
    
  if (!g95_is_constant_expr(op) ||    
      (op2 != NULL && !g95_is_constant_expr(op2)))    
    return SUCCESS;         
         
/* Rip p apart */ 
 
  h->op1 = NULL; 
  h->op2 = NULL;        
        
  switch(h->operator) {     
  case INTRINSIC_UPLUS:  
    result = g95_uplus(op);    
    break;    
    
  case INTRINSIC_UMINUS:    
    result = g95_uminus(op);      
    break;      
      
  case INTRINSIC_PLUS:    
    result = g95_add(op, op2);  
    break;      
      
  case INTRINSIC_MINUS:  
    result = g95_subtract(op, op2);        
    break;   
   
  case INTRINSIC_TIMES:
    result = g95_multiply(op, op2); 
    break;        
        
  case INTRINSIC_DIVIDE:   
    if ((op2->ts.type == BT_INTEGER &&  
	 mpz_cmp_ui(op2->value.integer, 0) == 0) || 
	(op2->ts.type == BT_REAL &&    
	 mpf_cmp_ui(op2->value.real, 0) == 0) ||     
	(op2->ts.type == BT_COMPLEX &&        
	 mpf_cmp_ui(op2->value.complex.r, 0) == 0 &&        
	 mpf_cmp_ui(op2->value.complex.i, 0) == 0)) {         
      h->op1 = op;    
      h->op2 = op2;     
      return SUCCESS;  
    } 
 
    result = g95_divide(op, op2); 
    break;     
     
  case INTRINSIC_POWER: 
    result = g95_power(op, op2);   
    break;         
         
  case INTRINSIC_CONCAT:  
    result = g95_concat(op, op2);      
    break;          
          
  case INTRINSIC_EQ:            
    result = g95_eq(op, op2);       
    break;

  case INTRINSIC_NE:   
    result = g95_ne(op, op2);         
    break;         
         
  case INTRINSIC_GT:         
    result = g95_gt(op, op2);        
    break;

  case INTRINSIC_GE:         
    result = g95_ge(op, op2);
    break;

  case INTRINSIC_LT:       
    result = g95_lt(op, op2);          
    break;      
      
  case INTRINSIC_LE:    
    result = g95_le(op, op2);         
    break;        
        
  case INTRINSIC_NOT:    
    result = g95_not(op);    
    break;  
  
  case INTRINSIC_AND:
    result = g95_and(op, op2);      
    break;      
      
  case INTRINSIC_OR:  
    result = g95_or(op, op2);     
    break; 
 
  case INTRINSIC_EQV: 
    result = g95_eqv(op, op2);       
    break;       
       
  case INTRINSIC_NEQV:
    result = g95_neqv(op, op2);   
    break;          
          
  default: g95_internal_error("simplify_intrinsic_op(): Bad operator");   
  }        
        
  if (result == NULL) {
    g95_free_expr(op);
    g95_free_expr(op2);   
    return FAILURE;     
  }   
   
  g95_replace_expr(h, result);        
        
  return SUCCESS;   
}          
          
          
       
       
/* copy_ref()-- Recursively copy a list of reference structures */ 
 
static g95_ref *copy_ref(g95_ref *s) { 
g95_array_ref *spec;      
g95_ref *d1;       
       
  if (s == NULL) return NULL;       
      
  d1 = g95_get_ref();
  d1->type = s->type;      
      
  switch(s->type) {  
  case REF_ARRAY:
    spec = g95_copy_array_ref(&s->u.ar);   
    d1->u.ar = *spec;      
    g95_free(spec);    
    break;          
          
  case REF_COMPONENT:   
    d1->u.c = s->u.c;
    break;    
    
  case REF_SUBSTRING: 
    d1->u.ss = s->u.ss;   
    d1->u.ss.start = g95_copy_expr(s->u.ss.start);  
    d1->u.ss.end = g95_copy_expr(s->u.ss.end);    
    break;
  }      
      
  d1->next = copy_ref(s->next);         
         
  return d1;     
}        
        
        
   
   
/* restricted_null()-- Check the NULL() function as part of a
 * restricted expression.  NULL is always OK. */        
        
static try restricted_null(g95_expr *a) {  
  
  a = NULL;  
  return SUCCESS;       
} 
 
 
     
     
/* g95_free_ref_list()-- Free a list of reference structures */       
       
void g95_free_ref_list(g95_ref *d) { 
g95_ref *z; 
int h;          
          
  for(; d; d=z) {     
    z = d->next;  
  
    switch(d->type) {          
    case REF_ARRAY:      
      for(h=0; h<G95_MAX_DIMENSIONS; h++) {       
	g95_free_expr(d->u.ar.start[h]);       
	g95_free_expr(d->u.ar.end[h]);
	g95_free_expr(d->u.ar.stride[h]);     
      }      
      
      break;    
    
    case REF_SUBSTRING:       
      g95_free_expr(d->u.ss.start);      
      g95_free_expr(d->u.ss.end);   
      break;          
          
    case REF_COMPONENT:     
      break;          
    }    
    
    g95_free(d);          
  }       
}          
          
          
 
 
/* free_actual_arglist()-- Free an argument list and everything below it. */      
      
void g95_free_actual_arglist(g95_actual_arglist *y) {       
g95_actual_arglist *a2;        
        
  while(y) {     
    a2 = y->next;         
    if (y->type != ALT_RETURN) g95_free_expr(y->u.expr);      
      
    g95_free(y);          
    y = a2; 
  } 
}        
        
        
    
    
/* g95_free_expr0()-- Workhorse function for g95_free_expr() that
 * frees everything beneath an expression node, but not the node
 * itself.  This is useful when we want to simplify a node and replace
 * it with something else or the expression node belongs to another
 * structure.  */          
          
static void g95_free_expr0(g95_expr *c) {   
int t;

  switch(c->type) { 
  case EXPR_CONSTANT:     
    switch(c->ts.type) {   
    case BT_INTEGER:       
      mpz_clear(c->value.integer);        
      break;

    case BT_REAL:        
      mpf_clear(c->value.real);  
      break;   
   
    case BT_CHARACTER:          
      g95_free(c->value.character.string);  
      break;

    case BT_COMPLEX:   
      mpf_clear(c->value.complex.r);     
      mpf_clear(c->value.complex.i);          
      break;    
    
    default:
      break;     
    }        
        
    break;    
    
  case EXPR_OP:        
    if (c->op1 != NULL) g95_free_expr(c->op1);  
    if (c->op2 != NULL) g95_free_expr(c->op2);          
    break;     
     
  case EXPR_FUNCTION:       
    g95_free_actual_arglist(c->value.function.actual);   
    break;         
         
  case EXPR_NULL:     
  case EXPR_VARIABLE:      
  case EXPR_UNKNOWN:      
    break; 
 
  case EXPR_ARRAY:   
  case EXPR_STRUCTURE:          
    g95_free_constructor(c->value.constructor);    
    break;     
     
  case EXPR_SUBSTRING:
    g95_free(c->value.character.string);      
    break;     
     
  default:    
    g95_internal_error("g95_free_expr0(): Bad expr type");    
  }

  /* Free a shape array */    
    
  if (c->shape != NULL) {
    for(t=0; t<c->rank; t++)       
      mpz_clear(c->shape[t]);       
       
    g95_free(c->shape);   
  }         
         
  g95_free_ref_list(c->ref);   
   
  memset(c, '\0', sizeof(g95_expr));  
}


   
   
/* g95_replace_expr()-- Grafts the *src expression onto the *dest
 * subexpression. */ 
 
void g95_replace_expr(g95_expr *des, g95_expr *source) {       
       
  g95_free_expr0(des);      
  *des = *source; 
 
  g95_free(source);     
}


 
 
/* g95_get_variable_expr()-- Given a symbol, create an expression node
 * with that symbol as a variable. */     
     
g95_expr *g95_get_variable_expr(g95_symbol *var0) {  
g95_expr *n;

  n = g95_get_expr();        
  n->type = EXPR_VARIABLE;
  n->symbol = var0;  
  n->ts = var0->ts;

  if (var0->as != NULL) {  
    n->rank = var0->as->rank; 
    n->ref = g95_full_ref();
  }        
        
  return n;
}       
       
       


/* g95_free_expr()-- Free an expression node and everything beneath it. */    
    
void g95_free_expr(g95_expr *x) {

  if (x == NULL) return;

  g95_free_expr0(x);   
  g95_free(x);   
}      
      
      
    
    
/* common_variable()-- Return nonzero if the given symbol is in a
 * common block or is equivalenced to a symbol in a common block. */        
        
static int common_variable(g95_symbol *sy) {
int seen_target, seen_common;      
g95_equiv *p, *u;    
    
  if (sy->attr.in_common)       
    return 1;         
         
  for(p=sy->ns->equiv; p; p=p->next) {         
    seen_common = 0;          
    seen_target = 0;        
        
    for(u=p; u; u=u->eq) {   
      seen_common |= u->expr->symbol->attr.in_common;
      seen_target |= (u->expr->symbol == sy);     
    }     
     
    if (seen_common && seen_target)     
      return 1;          
  }          
          
  return 0;        
}   
   
   
  
  
/* g95_int_expr()-- Returns an expression node that is an integer
 * constant. */      
      
g95_expr *g95_int_expr(int n) {        
g95_expr *l;          
          
  l = g95_get_expr();         
         
  l->type = EXPR_CONSTANT;          
  l->ts.type = BT_INTEGER; 
  l->ts.kind = g95_default_integer_kind();       
       
  l->where = g95_current_locus;      
  mpz_init_set_si(l->value.integer, n);   
   
  return l;     
}   
   
   
      
      
/* g95_logical_expr()-- Returns an expression node that is a logical
 * constant. */ 
 
g95_expr *g95_logical_expr(int v, g95_locus *where) {
g95_expr *d;    
    
  d = g95_get_expr();

  d->type = EXPR_CONSTANT;     
  d->ts.type = BT_LOGICAL;
  d->ts.kind = g95_default_logical_kind();

  if (where == NULL) where = &g95_current_locus;        
  d->where = *where;  
  d->value.logical = v;   
   
  return d;     
} 
 
 
     
     
/* check_inquiry()-- Certain inquiry functions are specifically
 * allowed to have variable arguments, which is an exception to the
 * normal requirement that an initialization function have
 * initialization arguments.  We head off this problem here.  */         
         
static try check_inquiry(g95_expr *n) { 
char *nm;      
static char *inquiry_function[] = {        
  "digits", "epsilon", "huge", "kind", "maxexponent", "minexponent",   
  "precision", "radix", "range", "tiny", "bit_size", "size", "shape",   
  "lbound", "ubound", NULL         
};   
   
int r;    
    
  if (n->value.function.actual == NULL || 
      n->value.function.actual->next != NULL)  /* Doesn't have one arg */       
    return FAILURE;          
          
  if (n->value.function.name != NULL &&       
      n->value.function.name[0] != '\0') return FAILURE;

  nm = n->symbol->name;  
  
  for(r=0; inquiry_function[r]; r++)    
    if (strcmp(inquiry_function[r], nm) == 0) break;          
          
  if (inquiry_function[r] == NULL) return FAILURE; 
 
  n = n->value.function.actual->u.expr;        
        
  if (n == NULL || n->type != EXPR_VARIABLE) return FAILURE; 
 
  /* At this point we have a numeric inquiry function with a variable
   * argument.  The type of the variable might be undefined, but we
   * need it now, because the arguments of these functions are allowed
   * to be undefined. */      
      
  if (n->ts.type == BT_UNKNOWN) {      
    if (n->symbol->ts.type == BT_UNKNOWN &&      
	g95_set_default_type(n->symbol, 0, g95_current_ns) == FAILURE)         
      return FAILURE;   
   
    n->ts = n->symbol->ts;   
  }      
      
  return SUCCESS;     
}  
  
  


/* g95_copy_arglist()-- Copy an arglist structure and all of the arguments. */       
       
g95_actual_arglist *g95_copy_actual_arglist(g95_actual_arglist *f) {      
g95_actual_arglist *head, *end, *n;          
          
  head = end = NULL;          
          
  for(; f; f=f->next) {   
    n = g95_get_actual_arglist();     
    *n = *f;       
       
    if (f->type != ALT_RETURN) n->u.expr = g95_copy_expr(f->u.expr);        
        
    n->next = NULL;      
      
    if (head == NULL)   
      head = n;   
    else         
      end->next = n;      
      
    end = n;     
  }  
  
  return head;
}         
         
         
 
 
/* g95_extract_int()-- Tries to extract an integer constant from the
 * passed expression node.  Returns an error message or NULL if the
 * result is set.  It is tempting to generate an error and return
 * SUCCESS or FAILURE, but failure is OK for some callers. */ 
 
char *g95_extract_int(g95_expr *exp, int *res) {

  if (exp->type != EXPR_CONSTANT)   
    return "Constant expression required at %C";    
    
  if (exp->ts.type != BT_INTEGER)      
    return "Integer expression required at %C";      
      
  if ((mpz_cmp_si(exp->value.integer, INT_MAX) > 0) || 
      (mpz_cmp_si(exp->value.integer, INT_MIN) < 0)) {    
    return "Integer value too large in expression at %C";          
  }    
    
  *res = (int) mpz_get_si(exp->value.integer);          
          
  return NULL;  
}     
     
     
     
     
/* show_constructor()-- Display a constructor.  Works recursively for
 * array constructors. */   
   
static void show_constructor(g95_constructor *k) {  
  
  for(;k ;k=k->next) {        
    if (k->iterator == NULL)        
      g95_show_expr(k->expr);
    else { 
      g95_status_char('(');         
      g95_show_expr(k->expr); 
 
      g95_status_char(' ');   
      g95_show_expr(k->iterator->var);         
      g95_status_char('=');
      g95_show_expr(k->iterator->start);     
      g95_status_char(',');    
      g95_show_expr(k->iterator->end);      
      g95_status_char(',');         
      g95_show_expr(k->iterator->step);        
        
      g95_status_char(')');    
    }        
        
    if (k->next != NULL) g95_status(" , ");        
  }     
}       
       
       
   
   
/* g95_kind_max()-- Return the maximum kind of two expressions.
 * Higher kind numbers mean more precision for numeric types. */     
     
int g95_kind_max(g95_expr *d, g95_expr *v) {    
    
  return (d->ts.kind > v->ts.kind) ? d->ts.kind : v->ts.kind;   
}       
       
       
  
  
/* show_ref()-- Show a string of g95_ref structures. */        
        
static void show_ref(g95_ref *f) {      
      
  for(; f; f=f->next)     
    switch(f->type) {       
    case REF_ARRAY:    
      g95_show_array_ref(&f->u.ar);         
      break;         
         
    case REF_COMPONENT:  
      g95_status(" %% %s", f->u.c.component->name);    
      break;  
  
    case REF_SUBSTRING:       
      g95_status_char('(');      
      g95_show_expr(f->u.ss.start);
      g95_status_char(':');      
      g95_show_expr(f->u.ss.end);         
      g95_status_char(')');      
      break;         
         
    default:          
      g95_internal_error("show_ref(): Bad component code");          
    }     
}       
       
       
        
        
/* numeric_type()-- Returns nonzero if the type is numeric, zero
 * otherwise */ 
 
static int numeric_type(bt t) {      
      
  return t == BT_COMPLEX || t == BT_REAL || t == BT_INTEGER;          
}      
      
      
      
      
/* g95_copy_shape()-- Copy a shape array. */      
      
mpz_t *g95_copy_shape(mpz_t *s, int dim) {   
mpz_t *new_shape;      
int f;          
          
  if (s == NULL) return NULL;  
  
  new_shape = g95_get_shape(dim); 
 
  for(f=0; f<dim; f++)          
    mpz_init_set(new_shape[f], s[f]);         
         
  return new_shape;         
}          
          
          
      
      
/* et0()-- Returns the type of an expression with the exception that
 * iterator variables are automatically integers no matter what else
 * they may be declared as. */     
     
static bt et0(g95_expr *k) {   
   
  if (k->type == EXPR_VARIABLE && g95_check_iter_variable(k) == SUCCESS)
    return BT_INTEGER;          
          
  return k->ts.type;    
}          
          
          
   
   
/* g95_show_expr()-- show an expression */   
   
void g95_show_expr(g95_expr *y) {         
char *d;         
int q; 
 
  if (y == NULL) {          
    g95_status("()");  
    return;    
  }       
       
/* Show expression */       
       
  switch(y->type) {          
  case EXPR_SUBSTRING:        
    d = y->value.character.string;      
      
    for(q=0; q<y->value.character.length; q++,d++) {      
      if (*d == '\'') g95_status("''");      
      else g95_status("%c", *d); 
    }       
       
    g95_status_char(' ');    
    g95_show_expr(y->ref->u.ss.start);    
    g95_status_char(' ');
    g95_show_expr(y->ref->u.ss.end);       
    break;         
         
  case EXPR_STRUCTURE:      
    g95_status("%s(", y->symbol->name); 
    show_constructor(y->value.constructor);          
    g95_status_char(')');   
    break;    
    
  case EXPR_ARRAY:          
    g95_status("(/ ");      
    show_constructor(y->value.constructor);      
    g95_status(" /)");         
         
    show_ref(y->ref);      
    break;   
   
  case EXPR_NULL:         
    g95_status("NULL()");         
    break;         
         
  case EXPR_CONSTANT:
    switch(y->ts.type) {         
    case BT_INTEGER:      
      mpz_out_str(stdout, 10, y->value.integer);          
          
      if (y->ts.kind != g95_default_integer_kind())
	g95_status("_%d", y->ts.kind);        
      break;        
        
    case BT_LOGICAL:        
      if (y->value.logical) g95_status(".true.");         
       else g95_status(".false.");       
      break;     
     
    case BT_REAL:          
      mpf_out_str(stdout, 10, 0, y->value.real);
      if (y->ts.kind != g95_default_real_kind())  
	g95_status("_%d", y->ts.kind);       
      break;

    case BT_CHARACTER:   
      d = y->value.character.string;         
         
      g95_status_char('\'');     
     
      for(q=0; q<y->value.character.length; q++,d++) {  
	if (*d == '\'') g95_status("''");
	else g95_status_char(*d);  
      }

      g95_status_char('\'');   
   
      break;        
        
    case BT_COMPLEX:   
      g95_status("(complex ");        
        
      mpf_out_str(stdout, 10, 0, y->value.complex.r);        
      if (y->ts.kind != g95_default_complex_kind())        
	g95_status("_%d", y->ts.kind); 
 
      g95_status(" ");         
         
      mpf_out_str(stdout, 10, 0, y->value.complex.i);      
      if (y->ts.kind != g95_default_complex_kind())      
	g95_status("_%d", y->ts.kind);         
         
      g95_status(")");
      break; 
 
    default:
      g95_status("???");    
      break;    
    }          
          
    break;          
          
  case EXPR_VARIABLE:   
    if (y->symbol->ns->proc_name->name != NULL)     
      g95_status("%s:%s", y->symbol->ns->proc_name->name, y->symbol->name);     
    else          
      g95_status(":%s", y->symbol->name);       
       
    show_ref(y->ref);   
    break;        
        
  case EXPR_OP:          
    g95_status("(");      
    switch(y->operator) {   
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
      
    g95_show_expr(y->op1);          
          
    if (y->op2) {       
      g95_status(" ");   
      g95_show_expr(y->op2); 
    }          
          
    g95_status(")");    
    break;      
      
  case EXPR_FUNCTION:   
    if (y->value.function.isym == NULL) {        
      g95_status("%s:%s[", y->symbol->module, y->symbol->name);
      g95_show_actual_arglist(y->value.function.actual);         
      g95_status_char(']');    
    } else { 
      g95_status("%s[[", y->value.function.name);
      g95_show_actual_arglist(y->value.function.actual);    
      g95_status_char(']');   
      g95_status_char(']');        
    }          
          
    break;   
   
  case EXPR_UNKNOWN:         
    g95_status("UNK %s", y->symbol->name);    
    break;         
         
  default: g95_internal_error("g95_show_expr(): Don't know how to show expr"); 
  }  
}   
  
  
/* simplify_constructor()-- Subroutine to simplify constructor
 * expressions.  Mutually recursive with g95_simplify_expr(). */  
  
static try simplify_constructor(g95_constructor *s, int typ) {     
     
  for(;s; s=s->next) {    
    if (s->iterator &&     
	(g95_simplify_expr(s->iterator->start, typ) == FAILURE ||      
	 g95_simplify_expr(s->iterator->end,   typ) == FAILURE ||
	 g95_simplify_expr(s->iterator->step,  typ) == FAILURE))
      return FAILURE;    
    
    if (s->expr && g95_simplify_expr(s->expr, typ) == FAILURE) return FAILURE;   
  }    
    
  return SUCCESS;  
}          
          
          
          
          
/* simplify_ref()-- Simplify an reference structure */

static try simplify_ref(g95_ref *re, int flag) {     
try b;    
int c;         
         
  b = SUCCESS;       
      
  switch(re->type) {     
  case REF_ARRAY:          
    for(c=0; c<re->u.ar.dimen; c++) {      
      if (g95_simplify_expr(re->u.ar.start[c],  flag) == FAILURE) b = FAILURE;     
      if (g95_simplify_expr(re->u.ar.end[c],    flag) == FAILURE) b = FAILURE;      
      if (g95_simplify_expr(re->u.ar.stride[c], flag) == FAILURE) b = FAILURE;         
    }     
     
    break;  
  
  case REF_COMPONENT:    
    break;        
        
  case REF_SUBSTRING:  
    if (g95_simplify_expr(re->u.ss.start, flag) == FAILURE) b = FAILURE;      
    if (g95_simplify_expr(re->u.ss.end, flag)   == FAILURE) b = FAILURE;       
    break;    
  }        
        
  return b;         
}       
       
       
  
  
/* check_intrinsic_op()-- Check an intrinsic arithmetic operation to
 * see if it is consistent with some type of expression. */       
       
static try check_intrinsic_op(g95_expr *e, try (*check_function)(g95_expr *)) {   
    
  if ((*check_function)(e->op1) == FAILURE) return FAILURE;     
     
  switch(e->operator) {      
  case INTRINSIC_UPLUS:       
  case INTRINSIC_UMINUS:         
    if (!numeric_type(et0(e->op1))) goto not_numeric;    
    break;         
         
  case INTRINSIC_EQ:  case INTRINSIC_NE:  case INTRINSIC_GT:
  case INTRINSIC_GE:  case INTRINSIC_LT:  case INTRINSIC_LE:        
        
  case INTRINSIC_PLUS:    case INTRINSIC_MINUS:  case INTRINSIC_TIMES:  
  case INTRINSIC_DIVIDE:  case INTRINSIC_POWER:        
    if ((*check_function)(e->op2) == FAILURE) return FAILURE;  
  
    if (!numeric_type(et0(e->op1)) || 
	!numeric_type(et0(e->op2))) goto not_numeric;    
    
    if (e->operator != INTRINSIC_POWER) break;         
             
    if (check_function == check_init_expr && et0(e->op2) != BT_INTEGER) {   
      g95_error("Exponent at %L must be INTEGER for an initialization "   
		"expression", &e->op2->where);         
      return FAILURE;       
    }         
         
    break;   
   
  case INTRINSIC_CONCAT:         
    if ((*check_function)(e->op2) == FAILURE) return FAILURE;

    if (et0(e->op1) != BT_CHARACTER || et0(e->op2) != BT_CHARACTER) {          
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
    if (et0(e->op1) != BT_LOGICAL) {          
      g95_error(".NOT. operator in expression at %L must have a LOGICAL "    
		"operand", &e->op1->where);        
      return FAILURE;   
    }        
        
    break;        
        
  case INTRINSIC_AND:    case INTRINSIC_OR:     
  case INTRINSIC_EQV:    case INTRINSIC_NEQV:         
    if ((*check_function)(e->op2) == FAILURE) return FAILURE;        
        
    if (et0(e->op1) != BT_LOGICAL || et0(e->op2) != BT_LOGICAL) {         
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
       
       
          
          
/* g95_match_init_expr()-- Match an initialization expression.  We work
 * by first matching an expression, then reducing it to a constant */        
        
match g95_match_init_expr(g95_expr **r) {          
g95_expr *e;        
match f;    
try a;        
        
  f = g95_match_expr(&e);        
  if (f != MATCH_YES) return f;         
         
  g95_init_expr = 1;   
  a = g95_resolve_expr(e);  
  if (a == SUCCESS) a = check_init_expr(e);
  g95_init_expr = 0;     
     
  if (a == FAILURE) {          
    g95_free_expr(e);       
    return MATCH_ERROR; 
  }    
    
  if (!g95_is_constant_expr(e))        
    g95_internal_error("Initialization expression didn't reduce %C");          
          
  *r = e;         
  return MATCH_YES;   
}        
        
        
         
         
/* g95_show_actual_arglist()-- Show an actual argument list */      
      
void g95_show_actual_arglist(g95_actual_arglist *z) {      
      
  g95_status("(");        
        
  for(; z; z=z->next) {    
    g95_status_char('(');  
  
    if (z->type == ALT_RETURN) 
      g95_status("*%d", z->u.label->value);          
    else {    
      if (z->pointer) g95_status("P ");     
     
      switch(z->type) { 
      case FULL_ARRAY:     g95_status("F "); break;    
      case ARRAY_ELEMENT:  g95_status("E "); break;     
      case ARRAY_DESC:     g95_status("D "); break;  
      default:                               break;  
      } 
 
      if (z->name[0] != '\0') g95_status("%s = ", z->name);     
     
      if (z->u.expr == NULL)  
	g95_status("(arg not-present)");
      else         
	g95_show_expr(z->u.expr);         
    }    
    
    g95_status_char(')');   
    if (z->next != NULL) g95_status(" ");      
  }       
       
  g95_status(")");    
}       
       
       
         
         
/* g95_copy_expr()-- Given an expression pointer, return a copy of the
 * expression.  This subroutine is recursive. */       
       
g95_expr *g95_copy_expr(g95_expr *b) {     
g95_expr *q;     
int len;
char *h; 
 
  if (b == NULL) return NULL;     
     
  q = g95_get_expr();
  *q = *b;       
       
  switch(q->type) {     
  case EXPR_SUBSTRING: 
    h = g95_getmem(b->value.character.length+1);         
    q->value.character.string = h;  
  
    memcpy(h, b->value.character.string, b->value.character.length+1);          
    break;    
    
  case EXPR_CONSTANT:
    switch(q->ts.type) {      
    case BT_INTEGER:         
      mpz_init_set(q->value.integer, b->value.integer);      
      break;

    case BT_REAL: 
      mpf_init_set(q->value.real, b->value.real);        
      break;      
      
    case BT_COMPLEX:    
      mpf_init_set(q->value.complex.r, b->value.complex.r);      
      mpf_init_set(q->value.complex.i, b->value.complex.i);        
      break;    
    
    case BT_CHARACTER:       
      len = b->value.character.length;          
          
      h = g95_getmem(len+1);  
      q->value.character.string = h;       
       
      if (len != 0) memcpy(h, b->value.character.string, len+1);       
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
      q->op1 = g95_copy_expr(b->op1);         
      break; 
 
    default:               /* Binary operators */       
      q->op1 = g95_copy_expr(b->op1);     
      q->op2 = g95_copy_expr(b->op2);          
      break;        
    }     
     
    break;        
        
  case EXPR_FUNCTION:      
    q->value.function.actual =         
      g95_copy_actual_arglist(b->value.function.actual);   
    break;          
          
  case EXPR_STRUCTURE:     
  case EXPR_ARRAY:        
    q->value.constructor = g95_copy_constructor(b->value.constructor);
    break;   
   
  case EXPR_VARIABLE: 
  case EXPR_UNKNOWN:      
  case EXPR_NULL: 
    break;          
  }   
   
  q->shape = g95_copy_shape(b->shape, b->rank);   
  q->ref = copy_ref(b->ref);    
    
  return q;          
}    
    
    
      
      
/* g95_full_ref()-- Get a full array reference node */  
  
g95_ref *g95_full_ref(void) {
g95_ref *reference;       
       
  reference = g95_get_ref();      
  reference->type = REF_ARRAY;         
  reference->u.ar.type = AR_FULL;         
         
  return reference;         
}


       
       
/* g95_check_parameter()-- At this point, we're trying to simplify an
 * expression involving a parameter.  Make sure the parameter has a
 * type, defaulting it if necessary and possibly converting the value
 * to this type. */

try g95_check_parameter(g95_symbol *sy) {       
       
  if (sy->ts.type == BT_UNKNOWN &&     
      g95_set_default_type(sy, 1, sy->ns) == FAILURE) return FAILURE;          
          
  if (g95_compare_types(&sy->ts, &sy->value->ts)) return SUCCESS;      
      
  return g95_convert_type(sy->value, &sy->ts, 1);   
}   
   
   


/* g95_get_expr()-- Get a new expr node */

g95_expr *g95_get_expr(void) {        
g95_expr *r;      
      
  r = g95_getmem(sizeof(g95_expr));    
    
  g95_clear_ts(&r->ts);          
  r->op1 = NULL; 
  r->op2 = NULL;   
  r->shape = NULL;       
  r->ref = NULL;  
  r->symbol = NULL;     
  r->uop = NULL;          
          
  return r;     
}


      
      
/* g95_copy_formal_arglist()-- Copy a formal argument list. */        
        
g95_formal_arglist *g95_copy_formal_arglist(g95_formal_arglist *o) {     
g95_formal_arglist *start, *tail;       
       
  start = tail = NULL;        
        
  for(; o; o=o->next) {    
    
    if (start == NULL)      
      start = tail = g95_get_formal_arglist();      
    else {   
      tail->next = g95_get_formal_arglist();         
      tail = tail->next;        
    }       
       
    tail->sym = o->sym;   
  }    
    
  return start;    
}     
     
     
         
         
/* g95_numeric_ts()-- Returns nonzero if the typespec is a numeric
 * type, zero otherwise. */       
       
int g95_numeric_ts(g95_typespec *t) {   
   
  return numeric_type(t->type);   
}    
    
    
      
      
/* g95_type_convert_binary()-- Given an expression node with some sort of
 * numeric binary expression, insert type conversions required to make
 * the operands have the same type.
 *
 * The exception is that the operands of an exponential don't have to
 * have the same type.  If possible, the base is promoted to the type
 * of the exponent.  For example, 1**2.3 becomes 1.0**2.3, but
 * 1.0**2 stays as it is. */     
     
void g95_type_convert_binary(g95_expr *k) {        
g95_expr *op0, *op_2;

  op0 = k->op1;  
  op_2 = k->op2;        
        
  if (op0->ts.type == BT_UNKNOWN || op_2->ts.type == BT_UNKNOWN) {    
    g95_clear_ts(&k->ts);         
    return;    
  }          
          
/* Kind conversions */        
        
  if (op0->ts.type == op_2->ts.type) {         
         
    if (op0->ts.kind == op_2->ts.kind) {  /* No type conversions */         
      k->ts = op0->ts;     
      goto done;       
    }         
         
    if (op0->ts.kind > op_2->ts.kind)      
      g95_convert_type(op_2, &op0->ts, 2);          
    else    
      g95_convert_type(op0, &op_2->ts, 2);     
     
    k->ts = op0->ts;         
    goto done;   
  }      
      
/* Real and integer combined with complex */      
      
  if (op0->ts.type == BT_COMPLEX &&  
      (op_2->ts.type == BT_REAL || op_2->ts.type == BT_INTEGER)) {       
       
    k->ts.type = BT_COMPLEX;   
    k->ts.kind = op0->ts.kind;         
         
    if (k->operator == INTRINSIC_POWER && op_2->ts.type == BT_INTEGER)  
      goto done;         
         
    g95_convert_type(k->op2, &k->ts, 2);    
    goto done;      
  } 
 
  if (op_2->ts.type == BT_COMPLEX &&    
      (op0->ts.type == BT_REAL || op0->ts.type == BT_INTEGER)) {  
  
    k->ts.type = BT_COMPLEX;  
    k->ts.kind = op_2->ts.kind;     
     
    g95_convert_type(k->op1, &k->ts, 2);        
    goto done;         
  }         
         
/* Integer combined with real */

  if (op0->ts.type == BT_REAL && op_2->ts.type == BT_INTEGER) {   
    k->ts.type = BT_REAL;   
    k->ts.kind = op0->ts.kind;        
        
    if (k->operator == INTRINSIC_POWER) goto done;         
         
    g95_convert_type(k->op2, &k->ts, 2);  
    goto done;  
  }       
       
  if (op0->ts.type == BT_INTEGER && op_2->ts.type == BT_REAL) {          
    k->ts.type = BT_REAL;         
    k->ts.kind = op_2->ts.kind;          
          
    g95_convert_type(k->op1, &k->ts, 2);
    goto done;       
  } 
 
done: 
  return;         
}    
    
    
     
     
/* g95_is_constant_expr()-- Function to determine if an expression is
 * constant or not.  This function expects that the expression has
 * already been simplified. */    
    
int g95_is_constant_expr(g95_expr *t) {    
g95_constructor *q;     
g95_actual_arglist *ap;  
int retcode;       
       
  if (t == NULL) return 1;       
      
  switch(t->type) {     
  case EXPR_OP:         
    retcode = g95_is_constant_expr(t->op1) &&       
      (t->op2 == NULL || g95_is_constant_expr(t->op2));       
       
    break;     
     
  case EXPR_VARIABLE:  
    retcode = 0;        
    break;   
   
  case EXPR_FUNCTION:        
    retcode = 0; 
    /* call to intrinsic with at least one argument */          
    if (t->value.function.isym && t->value.function.actual) {  
      for(ap = t->value.function.actual; ap; ap = ap->next){       
	if (!g95_is_constant_expr(ap->u.expr))
	  break;  
      }
      if (ap == NULL)          
	retcode = 1;   
    }          
    break;     
     
  case EXPR_CONSTANT:   
  case EXPR_NULL:
    retcode = 1;        
    break;  
  
  case EXPR_SUBSTRING:          
    retcode = g95_is_constant_expr(t->ref->u.ss.start) &&
         g95_is_constant_expr(t->ref->u.ss.end);          
    break;  
  
  case EXPR_STRUCTURE:
    retcode = 0;        
    for(q=t->value.constructor; q; q=q->next)    
      if (!g95_is_constant_expr(q->expr)) break;    
    
    if (q == NULL) retcode = 1;   
    break;       
       
  case EXPR_ARRAY:
    retcode = g95_constant_ac(t);          
    break;        
        
  default:  
    g95_internal_error("g95_is_constant_expr(): Unknown expression type");        
  }  
  
  return retcode;        
}       
       
       
    
    
/* g95_build_funcall()-- Return an expression node with an optional
 * argument list attached.  A variable number of g95_expr pointers are
 * strung together in an argument list with a NULL pointer terminating
 * the list. */         
         
g95_expr *g95_build_funcall(g95_symbol *func, ...) {       
g95_actual_arglist *end;       
g95_expr *g, *v;  
va_list argps;     
     
  g = g95_get_expr();  
  g->type = EXPR_FUNCTION;    
  g->symbol = func;   
  g->value.function.actual = NULL;        
        
  end = NULL;       
       
  va_start(argps, func);  
  for(;;) {      
    v = va_arg(argps, g95_expr *);          
    if (v == NULL) break;         
         
    if (end == NULL)    
      g->value.function.actual = end = g95_get_actual_arglist();    
    else {    
      end->next = g95_get_actual_arglist();  
      end = end->next; 
    }        
        
    end->type = EXPR;   
    end->u.expr = v;  
  }     
     
  va_end(argps);         
         
  return g;  
}  
  
  
    
    
/* check_init_expr()-- Verify that an expression is an
 * initialization expression.  A side effect is that the expression
 * tree is reduced to a single constant node if all goes well.  This
 * would normally happen when the expression is constructed but
 * function references are assumed to be intrinsics in the context of
 * initialization expressions.  If FAILURE is returned an error
 * message has been generated. */          
          
static try check_init_expr(g95_expr *n) {
g95_actual_arglist *actualp;    
match o;
try v;   
   
  if (n == NULL) return SUCCESS;          
          
  switch(n->type) {     
  case EXPR_OP:  
    v = check_intrinsic_op(n, check_init_expr);        
    if (v == SUCCESS) v = g95_simplify_expr(n, 0);       
       
    break;      
      
  case EXPR_FUNCTION:        
    v = SUCCESS;   
   
    if (check_inquiry(n) != SUCCESS) { 
      v = SUCCESS;    
      for(actualp=n->value.function.actual; actualp; actualp=actualp->next)    
	if (check_init_expr(actualp->u.expr) == FAILURE) {  
	  v = FAILURE;       
	  break;     
	}     
    }      
      
    if (v == SUCCESS) {  
      o = g95_intrinsic_func_interface(n, 0);      
      
      if (o == MATCH_NO)     
	g95_error("Function '%s' in initialization expression at %L "        
		  "must be an intrinsic function", n->symbol->name, &n->where);        
        
      if (o != MATCH_YES) v = FAILURE;          
    }   
   
    break;   
   
  case EXPR_VARIABLE:
    v = SUCCESS;   
   
    if (g95_check_iter_variable(n) == SUCCESS) break; 
 
    if (n->symbol->attr.flavor == FL_PARAMETER) {
      g95_replace_expr(n, g95_copy_expr(n->symbol->value));        
      break;      /* TODO: constant references to subobjects */        
    }          
          
    g95_error("Variable '%s' at %L cannot appear in an initialization "        
	      "expression", n->symbol->name, &n->where);       
    v = FAILURE;
    break;         
         
  case EXPR_CONSTANT:         
  case EXPR_NULL:
    v = SUCCESS;
    break;  
  
  case EXPR_SUBSTRING:   
    v = check_init_expr(n->ref->u.ss.start);
    if (v == FAILURE) break;         
         
    v = check_init_expr(n->ref->u.ss.end);       
    if (v == SUCCESS) v = g95_simplify_expr(n, 0);      
      
    break;     
     
  case EXPR_STRUCTURE:   
    v = g95_check_constructor(n, check_init_expr);      
    break;  
  
  case EXPR_ARRAY:   
    v = g95_check_constructor(n, check_init_expr);       
    if (v == FAILURE) break; 
 
    v = g95_check_constructor_type(n);       
    break;      
          
  default:   
    g95_internal_error("check_init_expr(): Unknown expression type");          
  }        
        
  return v; 
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
     
try g95_simplify_expr(g95_expr *n, int dtype) {    
g95_actual_arglist *ap;    
g95_ref *ref;      
      
  if (n == NULL) return SUCCESS;  
  
/* Replace a parameter variable with its value */

  switch(n->type) {       
  case EXPR_CONSTANT:         
  case EXPR_UNKNOWN:          
  case EXPR_NULL:    
    break;

  case EXPR_FUNCTION: 
    for(ap=n->value.function.actual; ap; ap=ap->next) 
      if (g95_simplify_expr(ap->u.expr, dtype) == FAILURE) return FAILURE;

    if (n->value.function.isym != NULL && 
	g95_intrinsic_func_interface(n, 1) == MATCH_ERROR) return FAILURE;     
     
    break;     
     
  case EXPR_OP:         
    if (simplify_intrinsic_op(n, dtype) == FAILURE) return FAILURE;   
    break;    
    
  case EXPR_VARIABLE:
    if (n->symbol->attr.flavor == FL_PARAMETER &&  
	g95_check_parameter(n->symbol) == FAILURE) return FAILURE; 
 
    if (n->symbol->attr.flavor == FL_PARAMETER &&        
	n->symbol->as == NULL &&        
	n->symbol->value->type != EXPR_STRUCTURE) {         
      ref = n->ref;  
      n->ref = NULL;        
      g95_replace_expr(n, g95_copy_expr(n->symbol->value));      
      n->ref = ref;        
      break;    
    }          
          
    if (dtype == 1) g95_simplify_iterator_var(n);          
          
    /* Fall through */        
        
  case EXPR_SUBSTRING:       
    for(ref=n->ref; ref; ref=ref->next)          
      if (simplify_ref(ref, dtype) == FAILURE) return FAILURE;     
     
    break;

  case EXPR_STRUCTURE:         
  case EXPR_ARRAY: 
    if (simplify_constructor(n->value.constructor, dtype) == FAILURE)  
      return FAILURE;    
    
    break;         
  }      
      
  return SUCCESS;      
}        
        
        
        
        
/* g95_get_temporary()-- Creates a temporary symbol which is a
 * variable of a particular type and rank.  If the variable has
 * nonzero rank, it is marked as allocatable with a deferred array
 * specification. */     
     
g95_symbol *g95_get_temporary(g95_typespec *typesp, int rank) {   
char name0[G95_MAX_SYMBOL_LEN+1];
static int serial = 0;         
g95_array_spec *as;       
g95_symtree *s;         
g95_symbol *symbol;         
         
  sprintf(name0, "SC.%d", serial++);  
  symbol = g95_new_symbol(name0, g95_current_ns);     
     
  symbol->ts = *typesp;      
  symbol->attr.flavor = FL_VARIABLE;          
  symbol->attr.used = 1;  
  symbol->attr.artificial = 1;      
  symbol->refs = 1;      
      
  if (rank > 0) { 
    as = symbol->as = g95_get_array_spec();     
    as->rank = rank;     
    as->type = AS_DEFERRED;  
  
    symbol->attr.allocatable = 1;     
  }     
     
  s = g95_new_symtree(&g95_current_ns->sym_root, name0);       
  s->n.sym = symbol;

  return symbol;    
}  
  
  
     
     
/* restricted_args()-- Given an actual argument list, test to see that
 * each argument is a restricted expression and optionally if the
 * expression type is integer or character */      
      
static try restricted_args(g95_actual_arglist *l, int check_type) {    
bt typ; 
 
  for(; l; l=l->next) {      
    if (check_restricted(l->u.expr) == FAILURE) return FAILURE;      
      
    if (!check_type || l->u.expr == NULL) continue;   
   
    typ = l->u.expr->ts.type;     
    if (typ != BT_CHARACTER && typ != BT_INTEGER) {      
      g95_error("Function argument at %L must be of type INTEGER or CHARACTER",     
		&l->u.expr->where);
      return FAILURE;         
    }        
  }   
   
  return SUCCESS;          
}        
        
        
          
          
/* restricted_array_inquiry()-- Check an array inquiry function as
 * part of a restricted expression.  The array argument does not have
 * to be a restricted variable, but in that case, the array must not
 * be allocatable or a dummy pointer array. */         
         
static try restricted_array_inquiry(g95_expr *g) {  
g95_actual_arglist *c;      
g95_expr *arr;       
g95_symbol *sy;          
          
  c = g->value.function.actual;  
  arr = c->u.expr;      
      
  if (arr->type == EXPR_VARIABLE) { 
    sy = arr->symbol;    
    if (!sy->attr.allocatable && (!sy->attr.pointer || sy->attr.dummy))          
      c = c->next;  /* skip array arg */          
  }     
     
  return restricted_args(c, 0);         
}     
     
     
       
       
/* restricted_nonelemental()-- Check an intrinsic function that does
 * not have to be elemental, yet requires integer or character
 * arguements. */         
         
static try restricted_nonelemental(g95_expr *l) {    
    
  return restricted_args(l->value.function.actual, 1);  
}        
        
        
         
         
/* external_spec_function()-- Make sure a non-intrinsic function is a
 * specification function. */  
  
static try external_spec_function(g95_expr *x) {        
g95_symbol *d;         
         
  d = x->symbol;       
       
  if (d->attr.proc == PROC_ST_FUNCTION) { 
    g95_error("Specification function '%s' at %L cannot be a statement "   
	      "function", d->name, &x->where);        
    return FAILURE;  
  }         
         
  if (d->attr.proc == PROC_INTERNAL) {       
    g95_error("Specification function '%s' at %L cannot be an internal "
	      "function", d->name, &x->where);         
    return FAILURE;      
  } 
 
  if (!d->attr.pure) {   
    g95_error("Specification function '%s' at %L must be PURE", d->name,    
	      &x->where);    
    return FAILURE;    
  } 
 
  if (d->attr.recursive) {          
    g95_error("Specification function '%s' at %L cannot be RECURSIVE",   
	      d->name, &x->where);  
    return FAILURE; 
  }       
       
  return restricted_args(x->value.function.actual, 0); 
}  
  
  
         
         
/* g95_char_expr()-- Return an expression node that is a character constant. */      
      
g95_expr *g95_char_expr(int leng, int k, g95_locus *where) {        
g95_charlen *clen;        
g95_expr *r;    
    
  clen = g95_get_charlen();
  clen->length = g95_int_expr(leng);        
  clen->next = g95_current_ns->cl_list;         
  g95_current_ns->cl_list = clen;         
         
  r = g95_get_expr();     
     
  r->type = EXPR_CONSTANT;          
  r->ref = NULL;       
  r->ts.type = BT_CHARACTER;    
  r->ts.kind = k;       
  r->ts.cl = clen;      
      
  if (where == NULL) where = &g95_current_locus; 
  r->where = *where; 
 
  r->value.character.string = g95_getmem(leng+1);   
  r->value.character.length = leng;      
      
  memset(r->value.character.string, ' ', leng);  
  r->value.character.string[leng] = '\0';

  return r;      
}      
      
      
/* g95_null_expr()-- Return an expression that is the NULL() function. */    
    
g95_expr *g95_null_expr(g95_locus *where) {
g95_expr *r;

  r = g95_get_expr();   
  r->type = EXPR_NULL;       
  r->ts.type = BT_UNKNOWN;         
  r->where = (where == NULL) ? g95_current_locus : *where; 
  return r;      
}   
   
   
     
     
/* restricted_len()-- Check the LEN() function as part of a restricted
 * expression.  The argument of LEN can be a character variable that
 * isn't a restricted expression, but in which the length is a
 * restricted expression.  If the variable already exists, then the
 * length was already a specification expression. */     
     
static try restricted_len(g95_expr *x) {
g95_expr *arg; 
 
  arg = x->value.function.actual->u.expr;          
          
  if (arg->type == EXPR_VARIABLE && arg->ts.type == BT_CHARACTER)  
    return SUCCESS;   
   
  return restricted_args(x->value.function.actual, 1); 
}     
     
     


/* g95_check_spec_expr()-- Check to see that an expression is a
 * specification expression.  If we return FAILURE, an error has been
 * generated. */  
  
try g95_specification_expr(g95_expr *l) {      
      
  if (l == NULL) return SUCCESS;

  if (g95_simplify_expr(l, 0) == FAILURE) return FAILURE;       
       
  if (g95_resolve_expr(l) == FAILURE) return FAILURE;  
  
  if (l->ts.type != BT_INTEGER) {    
    g95_error("Expression at %L must be of INTEGER type", &l->where);     
    return FAILURE;     
  }

  if (l->rank != 0) {          
    g95_error("Expression at %L must be scalar", &l->where);     
    return FAILURE;       
  }    
    
  return check_restricted(l);  
}  
  
  
  
  
/* restricted_elemental()-- Check an intrinsic function call to see if
 * it is an elemental function returning character or integer with
 * character and integer arguments.  This function is a catchall for
 * most of the restricted intrinsics. */         
         
static try restricted_elemental(g95_expr *m) {      
g95_intrinsic_sym *symb;  
  
  symb = m->value.function.isym;           
          
  if (!symb->elemental && g95_option.fmode != 0) {          
    g95_error("Intrinsic function '%s' in specification expression at %L "      
	      "must be ELEMENTAL", symb->name, &m->where);         
    return FAILURE;          
  }    
    
  if (m->ts.type != BT_INTEGER && m->ts.type != BT_CHARACTER) {   
    g95_error("Intrinsic function '%s' in specification expression at %C "     
	      "must return INTEGER or CHARACTER", symb->name);     
    return FAILURE;      
  }

  return restricted_args(m->value.function.actual->next, 1);       
}


       
       
/* g95_get_temporary_int()-- Allocate an integer loop variable */         
         
g95_symbol *g95_get_temporary_int(void) {      
g95_typespec typesp;         
         
  typesp.type = BT_INTEGER;      
  typesp.kind = g95_default_integer_kind();     
     
  return g95_get_temporary(&typesp, 0);  
}  
  
  
          
          
/* restricted_intrinsic()-- Check to see that a function reference to
 * an intrinsic is a restricted expression.  Some functions required
 * by the standard are omitted because references to them have already
 * been simplified.  Strictly speaking, a lot of these checks are
 * redundant with other checks.  If a function is indeed a particular
 * intrinsic, then the type of its argument have already been checked
 * and passed. */ 
 
static try restricted_intrinsic(g95_expr *m) {        
        
static struct { char *fname; try (*function)(g95_expr *m); } *cp, cases[] = {       
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
       
char *nam;    
    
  nam = m->value.function.isym->name;         
         
  for(cp=cases; cp->fname; cp++)        
    if (cp->fname == NULL || strcmp(cp->fname, nam) == 0) break;         
         
  return cp->function(m);     
}       
       
       
      
      
/* check_restricted()-- Verify that an expression is a restricted
 * expression.  Like its cousin check_init_expr(), an error message is
 * generated if we return FAILURE. */    
    
static try check_restricted(g95_expr *n) { 
g95_symbol *sym;
try c;     
     
  if (n == NULL) return SUCCESS;  
  
  c = SUCCESS;          
          
  switch(n->type) {  
  case EXPR_OP:    
    c = check_intrinsic_op(n, check_restricted);      
    if (c == SUCCESS) c = g95_simplify_expr(n, 0);     
     
    break;

  case EXPR_FUNCTION: 
    c = (n->value.function.isym != NULL)     
      ? restricted_intrinsic(n)         
      : external_spec_function(n);        
        
    break; 
 
  case EXPR_VARIABLE:    
    sym = n->symbol;        
    c = FAILURE;         
             
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
    
    if (common_variable(sym) || sym->attr.use_assoc || sym->attr.dummy ||          
	sym->ns != g95_current_ns || sym->ns->state == COMP_MODULE) {          
      c = SUCCESS;      
      break;         
    }      
      
    if (sym->attr.flavor == FL_PARAMETER) {    
      c = SUCCESS;   
      break;    
    }

    g95_error("Variable '%s' cannot appear in restricted expression at %L",        
	      sym->name, &n->where);          
    break;        
        
  case EXPR_NULL:        
  case EXPR_CONSTANT:         
    c = SUCCESS;    
    break; 
 
  case EXPR_SUBSTRING:        
    c = g95_specification_expr(n->ref->u.ss.start);       
    if (c == FAILURE) break;     
     
    c = g95_specification_expr(n->op2);       
    if (c == SUCCESS) c = g95_simplify_expr(n, 0);        
        
    break;      
      
  case EXPR_STRUCTURE:   
    c = g95_check_constructor(n, check_restricted);       
    break;     
     
  case EXPR_ARRAY:  
    c = g95_check_constructor(n, check_restricted);       
    break;

  default:     
    g95_internal_error("check_spec_expr(): Unknown expression type");       
  }  
  
  return c;  
}   
   
   
