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


    
    
/* g95_copy_shape()-- Copy a shape array. */         
         
mpz_t *g95_copy_shape(mpz_t *sh, int dim) {     
mpz_t *new_shape; 
int x;  
  
  if (sh == NULL) return NULL;      
      
  new_shape = g95_get_shape(dim);        
        
  for(x=0; x<dim; x++)
    mpz_init_set(new_shape[x], sh[x]);       
       
  return new_shape;          
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
g95_expr *op_1, *op0;         
         
  op_1 = m->op1;          
  op0 = m->op2;       
       
  if (op_1->ts.type == BT_UNKNOWN || op0->ts.type == BT_UNKNOWN) {        
    g95_clear_ts(&m->ts);   
    return;      
  }     
     
/* Kind conversions */        
        
  if (op_1->ts.type == op0->ts.type) {   
   
    if (op_1->ts.kind == op0->ts.kind) {  /* No type conversions */ 
      m->ts = op_1->ts;    
      goto done;       
    }         
         
    if (op_1->ts.kind > op0->ts.kind)      
      g95_convert_type(op0, &op_1->ts, 2);    
    else
      g95_convert_type(op_1, &op0->ts, 2);       
       
    m->ts = op_1->ts; 
    goto done; 
  }    
    
/* Real and integer combined with complex */      
      
  if (op_1->ts.type == BT_COMPLEX &&       
      (op0->ts.type == BT_REAL || op0->ts.type == BT_INTEGER)) {

    m->ts.type = BT_COMPLEX;      
    m->ts.kind = op_1->ts.kind;         
         
    if (m->operator == INTRINSIC_POWER) goto done;      
      
    g95_convert_type(m->op2, &m->ts, 2);        
    goto done;      
  }

  if (op0->ts.type == BT_COMPLEX &&    
      (op_1->ts.type == BT_REAL || op_1->ts.type == BT_INTEGER)) {       
       
    m->ts.type = BT_COMPLEX;  
    m->ts.kind = op0->ts.kind;    
    
    g95_convert_type(m->op1, &m->ts, 2);         
    goto done;   
  }  
  
/* Integer combined with real */       
       
  if (op_1->ts.type == BT_REAL && op0->ts.type == BT_INTEGER) {   
    m->ts.type = BT_REAL;   
    m->ts.kind = op_1->ts.kind;          
          
    if (m->operator == INTRINSIC_POWER) goto done;        
        
    g95_convert_type(m->op2, &m->ts, 2);      
    goto done; 
  }   
   
  if (op_1->ts.type == BT_INTEGER && op0->ts.type == BT_REAL) { 
    m->ts.type = BT_REAL;      
    m->ts.kind = op0->ts.kind;          
          
    g95_convert_type(m->op1, &m->ts, 2); 
    goto done;     
  }       
       
done:      
  return;         
}        
        
        
      
      
/* check_inquiry()-- Certain inquiry functions are specifically
 * allowed to have variable arguments, which is an exception to the
 * normal requirement that an initialization function have
 * initialization arguments.  We head off this problem here.  */ 
 
static try check_inquiry(g95_expr *b) {
char *name0;  
static char *inquiry_function[] = {     
  "digits", "epsilon", "huge", "kind", "maxexponent", "minexponent",     
  "precision", "radix", "range", "tiny", "bit_size", "size", "shape",    
  "lbound", "ubound", NULL        
};       
       
int m;          
          
  if (b->value.function.actual == NULL ||   
      b->value.function.actual->next != NULL)  /* Doesn't have one arg */       
    return FAILURE;         
         
  if (b->value.function.name != NULL &&  
      b->value.function.name[0] != '\0') return FAILURE; 
 
  name0 = b->symbol->name;        
        
  for(m=0; inquiry_function[m]; m++)          
    if (strcmp(inquiry_function[m], name0) == 0) break;

  if (inquiry_function[m] == NULL) return FAILURE; 
 
  b = b->value.function.actual->u.expr;     
     
  if (b == NULL || b->type != EXPR_VARIABLE) return FAILURE;      
      
  /* At this point we have a numeric inquiry function with a variable
   * argument.  The type of the variable might be undefined, but we
   * need it now, because the arguments of these functions are allowed
   * to be undefined. */

  if (b->ts.type == BT_UNKNOWN) {       
    if (b->symbol->ts.type == BT_UNKNOWN &&        
	g95_set_default_type(b->symbol, 0, g95_current_ns) == FAILURE)        
      return FAILURE;          
          
    b->ts = b->symbol->ts;      
  }    
    
  return SUCCESS;  
}          
          
          
  
  
/* restricted_args()-- Given an actual argument list, test to see that
 * each argument is a restricted expression and optionally if the
 * expression type is integer or character */          
          
static try restricted_args(g95_actual_arglist *y, int check_type) {  
bt type;   
   
  for(; y; y=y->next) {         
    if (check_restricted(y->u.expr) == FAILURE) return FAILURE;       
       
    if (!check_type) continue;    
    
    type = y->u.expr->ts.type; 
    if (type != BT_CHARACTER && type != BT_INTEGER) {
      g95_error("Function argument at %L must be of type INTEGER or CHARACTER",   
		&y->u.expr->where);    
      return FAILURE;   
    }  
  }

  return SUCCESS;          
}          
          
          
    
    
/* g95_kind_max()-- Return the maximum kind of two expressions.
 * Higher kind numbers mean more precision for numeric types. */

int g95_kind_max(g95_expr *r, g95_expr *q) {

  return (r->ts.kind > q->ts.kind) ? r->ts.kind : q->ts.kind;        
}       
       
       
  
  
/* g95_extract_int()-- Tries to extract an integer constant from the
 * passed expression node.  Returns an error message or NULL if the
 * result is set.  It is tempting to generate an error and return
 * SUCCESS or FAILURE, but failure is OK for some callers. */         
         
char *g95_extract_int(g95_expr *expr, int *res) {          
          
  if (expr->type != EXPR_CONSTANT)     
    return "Constant expression required at %C";          
          
  if (expr->ts.type != BT_INTEGER) 
    return "Integer expression required at %C";        
        
  if ((mpz_cmp_si(expr->value.integer, INT_MAX) > 0) ||       
      (mpz_cmp_si(expr->value.integer, INT_MIN) < 0)) {  
    return "Integer value too large in expression at %C";
  }          
          
  *res = (int) mpz_get_si(expr->value.integer);  
  
  return NULL;  
}       
       
       


/* free_actual_arglist()-- Free an argument list and everything below it. */        
        
void g95_free_actual_arglist(g95_actual_arglist *n) {  
g95_actual_arglist *a2;      
      
  while(n) {    
    a2 = n->next;     
    if (n->type != ALT_RETURN) g95_free_expr(n->u.expr);      
      
    g95_free(n);          
    n = a2;      
  }       
}   
   
   
       
       
/* g95_build_funcall()-- Return an expression node with an optional
 * argument list attached.  A variable number of g95_expr pointers are
 * strung together in an argument list with a NULL pointer terminating
 * the list. */          
          
g95_expr *g95_build_funcall(g95_symbol *func, ...) {      
g95_actual_arglist *end;      
g95_expr *b, *c;         
va_list argp;          
          
  b = g95_get_expr();  
  b->type = EXPR_FUNCTION;
  b->symbol = func;          
  b->value.function.actual = NULL;        
        
  end = NULL; 
 
  va_start(argp, func); 
  for(;;) {       
    c = va_arg(argp, g95_expr *);        
    if (c == NULL) break;

    if (end == NULL)          
      b->value.function.actual = end = g95_get_actual_arglist();         
    else {      
      end->next = g95_get_actual_arglist();          
      end = end->next;    
    }   
   
    end->type = EXPR;      
    end->u.expr = c;       
  }          
          
  va_end(argp);      
      
  return b;   
}       
       
       
          
          
/* g95_get_expr()-- Get a new expr node */       
       
g95_expr *g95_get_expr(void) {     
g95_expr *e;

  e = g95_getmem(sizeof(g95_expr));    
    
  g95_clear_ts(&e->ts);        
  e->op1 = NULL; 
  e->op2 = NULL;        
  e->shape = NULL;
  e->ref = NULL;   
  e->symbol = NULL;
  e->uop = NULL;          
          
  return e;    
}     
     
     
   
   
/* g95_copy_arglist()-- Copy an arglist structure and all of the arguments. */      
      
g95_actual_arglist *g95_copy_actual_arglist(g95_actual_arglist *c) {
g95_actual_arglist *head, *tail, *new;        
        
  head = tail = NULL;          
          
  for(; c; c=c->next) {       
    new = g95_get_actual_arglist();          
    *new = *c;     
     
    if (c->type != ALT_RETURN) new->u.expr = g95_copy_expr(c->u.expr);        
        
    new->next = NULL;

    if (head == NULL)         
      head = new;        
    else      
      tail->next = new;         
         
    tail = new;   
  }   
   
  return head;       
}   
   
   
  
  
/* show_constructor()-- Display a constructor.  Works recursively for
 * array constructors. */     
     
static void show_constructor(g95_constructor *m) { 
 
  for(;m ;m=m->next) {         
    if (m->iterator == NULL)   
      g95_show_expr(m->expr);          
    else {          
      g95_status_char('(');         
      g95_show_expr(m->expr);    
    
      g95_status_char(' ');      
      g95_show_expr(m->iterator->var);          
      g95_status_char('='); 
      g95_show_expr(m->iterator->start);
      g95_status_char(',');        
      g95_show_expr(m->iterator->end);    
      g95_status_char(',');   
      g95_show_expr(m->iterator->step);        
        
      g95_status_char(')');       
    }

    if (m->next != NULL) g95_status(" , "); 
  }      
}      
      
      
 
 
/* g95_char_expr()-- Return an expression node that is a character constant. */      
      
g95_expr *g95_char_expr(int l, int k, locus *old_loc) {  
g95_expr *y;

  y = g95_get_expr(); 
 
  y->type = EXPR_CONSTANT;   
  y->ref = NULL;
  y->ts.type = BT_CHARACTER;       
  y->ts.kind = k;         
         
  if (old_loc == NULL) old_loc = g95_current_locus();
  y->where = *old_loc;

  y->value.character.string = g95_getmem(l+1);        
  y->value.character.length = l;

  memset(y->value.character.string, ' ', l);   
  y->value.character.string[l] = '\0';      
      
  return y;         
}  
  
  
      
      
/* et0()-- Returns the type of an expression with the exception that
 * iterator variables are automatically integers no matter what else
 * they may be declared as. */          
          
static bt et0(g95_expr *x) {  
  
  if (x->type == EXPR_VARIABLE && g95_check_iter_variable(x) == SUCCESS)          
    return BT_INTEGER;      
      
  return x->ts.type;  
}      
      
      
 
 
/* external_spec_function()-- Make sure a non-intrinsic function is a
 * specification function. */    
    
static try external_spec_function(g95_expr *x) {      
g95_symbol *a;     
     
  a = x->symbol;     
     
  if (a->attr.proc == PROC_ST_FUNCTION) {      
    g95_error("Specification function '%s' at %L cannot be a statement "      
	      "function", a->name, &x->where);       
    return FAILURE;      
  }   
   
  if (a->attr.proc == PROC_INTERNAL) {
    g95_error("Specification function '%s' at %L cannot be an internal "         
	      "function", a->name, &x->where);  
    return FAILURE; 
  }

  if (!a->attr.pure) {          
    g95_error("Specification function '%s' at %L must be PURE", a->name, 
	      &x->where);   
    return FAILURE; 
  }

  if (a->attr.recursive) {          
    g95_error("Specification function '%s' at %L cannot be RECURSIVE",        
	      a->name, &x->where);
    return FAILURE;          
  }  
  
  return restricted_args(x->value.function.actual, 0);     
}       
       
       


/* g95_check_spec_expr()-- Check to see that an expression is a
 * specification expression.  If we return FAILURE, an error has been
 * generated. */

try g95_specification_expr(g95_expr *q) {         
         
  if (g95_simplify_expr(q, 0) == FAILURE) return FAILURE;          
          
  if (g95_resolve_expr(q) == FAILURE) return FAILURE;        
        
  if (q->ts.type != BT_INTEGER) {        
    g95_error("Expression at %L must be of INTEGER type", &q->where); 
    return FAILURE; 
  }       
       
  if (q->rank != 0) {        
    g95_error("Expression at %L must be scalar", &q->where);   
    return FAILURE;
  }  
  
  return check_restricted(q);         
}       
       
       
    
    
/* g95_free_expr0()-- Workhorse function for g95_free_expr() that
 * frees everything beneath an expression node, but not the node
 * itself.  This is useful when we want to simplify a node and replace
 * it with something else or the expression node belongs to another
 * structure.  */    
    
static void g95_free_expr0(g95_expr *u) {      
int b;    
    
  switch(u->type) { 
  case EXPR_CONSTANT:       
    switch(u->ts.type) {          
    case BT_INTEGER:    
      mpz_clear(u->value.integer);
      break;         
         
    case BT_REAL:     
      mpf_clear(u->value.real);        
      break;    
    
    case BT_CHARACTER:     
      g95_free(u->value.character.string); 
      break;   
   
    case BT_COMPLEX:
      mpf_clear(u->value.complex.r);       
      mpf_clear(u->value.complex.i);      
      break; 
 
    default:         
      break;      
    }      
      
    break;      
      
  case EXPR_OP:        
    if (u->op1 != NULL) g95_free_expr(u->op1);
    if (u->op2 != NULL) g95_free_expr(u->op2);         
    break;

  case EXPR_FUNCTION:    
    g95_free_actual_arglist(u->value.function.actual);      
    break;     
     
  case EXPR_VARIABLE:         
    break; 
 
  case EXPR_ARRAY:        
  case EXPR_STRUCTURE:      
    g95_free_constructor(u->value.constructor);    
    break;    
    
  case EXPR_SUBSTRING:  
    g95_free(u->value.character.string);        
    break;      
      
  case EXPR_NULL:       
    break; 
 
  default:  
    g95_internal_error("g95_free_expr0(): Bad expr type");        
  }         
         
  /* Free a shape array */      
      
  if (u->shape != NULL) {     
    for(b=0; b<u->rank; b++)
      mpz_clear(u->shape[b]);    
    
    g95_free(u->shape);          
  }   
   
  g95_free_ref_list(u->ref);   
   
  memset(u, '\0', sizeof(g95_expr));      
}   
   
   
    
    
/* show_ref()-- Show a string of g95_ref structures. */ 
 
static void show_ref(g95_ref *d) { 
 
  for(; d; d=d->next)     
    switch(d->type) {         
    case REF_ARRAY:          
      g95_show_array_ref(&d->u.ar);     
      break; 
 
    case REF_COMPONENT:          
      g95_status(" %% %s", d->u.c.component->name); 
      break;       
       
    case REF_SUBSTRING:     
      g95_status_char('(');    
      g95_show_expr(d->u.ss.start);
      g95_status_char(':');   
      g95_show_expr(d->u.ss.end);         
      g95_status_char(')');          
      break;     
     
    default:         
      g95_internal_error("show_ref(): Bad component code");  
    }     
}         
         
         
      
      
/* g95_replace_expr()-- grafts the *src expression onto the *dest
 * subexpression. */          
          
void g95_replace_expr(g95_expr *d, g95_expr *s) {    
    
  g95_free_expr0(d);      
  *d = *s;   
   
  g95_free(s);          
}     
     
     
    
    
/* g95_free_expr()-- Free an expression node and everything beneath it. */  
  
void g95_free_expr(g95_expr *u) {      
      
  if (u == NULL) return; 
 
  g95_free_expr0(u);       
  g95_free(u);    
}   
   
   
   
   
/* g95_free_ref_list()-- Free a list of reference structures */     
     
void g95_free_ref_list(g95_ref *p) {          
g95_ref *r;   
int t;  
  
  for(; p; p=r) {   
    r = p->next;  
  
    switch(p->type) {     
    case REF_ARRAY:         
      for(t=0; t<G95_MAX_DIMENSIONS; t++) { 
	g95_free_expr(p->u.ar.start[t]);      
	g95_free_expr(p->u.ar.end[t]);         
	g95_free_expr(p->u.ar.stride[t]); 
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
     
     
        
        
/* copy_ref()-- Recursively copy a list of reference structures */  
  
static g95_ref *copy_ref(g95_ref *s0) {          
g95_array_ref *spec;   
g95_ref *dest; 
 
  if (s0 == NULL) return NULL;       
      
  dest = g95_get_ref();      
  dest->type = s0->type; 
 
  switch(s0->type) {    
  case REF_ARRAY: 
    spec = g95_copy_array_ref(&s0->u.ar);      
    dest->u.ar = *spec;    
    g95_free(spec);     
    break;    
    
  case REF_COMPONENT:   
    dest->u.c = s0->u.c;         
    break;          
          
  case REF_SUBSTRING:         
    dest->u.ss = s0->u.ss;
    dest->u.ss.start = g95_copy_expr(s0->u.ss.start);         
    dest->u.ss.end = g95_copy_expr(s0->u.ss.end);  
    break;          
  }        
        
  dest->next = copy_ref(s0->next); 
 
  return dest; 
}    
    
    
  
  
/* g95_show_actual_arglist()-- Show an actual argument list */

void g95_show_actual_arglist(g95_actual_arglist *b) {    
    
  g95_status("(");         
         
  for(; b; b=b->next) {    
    g95_status_char('(');  
  
    if (b->type == ALT_RETURN)         
      g95_status("*%d", b->u.label->value);          
    else {        
      if (b->pointer) g95_status("P ");         
         
      switch(b->type) {         
      case FULL_ARRAY:     g95_status("F "); break;        
      case ARRAY_ELEMENT:  g95_status("E "); break;
      case ARRAY_DESC:     g95_status("D "); break;       
      default:                               break;   
      }

      if (b->name[0] != '\0') g95_status("%s = ", b->name); 
 
      if (b->u.expr == NULL)      
	g95_status("(arg not-present)");        
      else         
	g95_show_expr(b->u.expr);
    }          
          
    g95_status_char(')');     
    if (b->next != NULL) g95_status(" "); 
  }   
   
  g95_status(")");          
}    
    
    
         
         
/* g95_match_init_expr()-- Match an initialization expression.  We work
 * by first matching an expression, then reducing it to a constant */     
     
match g95_match_init_expr(g95_expr **res) {
g95_expr *exp;   
match d;       
try g;    
    
  d = g95_match_expr(&exp);        
  if (d != MATCH_YES) return d;  
  
  g95_init_expr = 1; 
  g = g95_resolve_expr(exp);  
  if (g == SUCCESS) g = check_init_expr(exp);   
  g95_init_expr = 0; 
 
  if (g == FAILURE) {
    g95_free_expr(exp);   
    return MATCH_ERROR;     
  }         
         
  if (!g95_is_constant_expr(exp)) 
    g95_internal_error("Initialization expression didn't reduce %C");       
       
  *res = exp;        
  return MATCH_YES;    
} 
 
 
    
    
/* g95_show_expr()-- show an expression */ 
 
void g95_show_expr(g95_expr *l) {          
char *g; 
int o;       
       
  if (l == NULL) {         
    g95_status("()");       
    return;      
  }         
         
/* Show expression */   
   
  switch(l->type) {
  case EXPR_SUBSTRING:  
    g = l->value.character.string;       
       
    for(o=0; o<l->value.character.length; o++,g++) { 
      if (*g == '\'') g95_status("''");    
      else g95_status("%c", *g);
    } 
 
    show_ref(l->ref);          
    break;     
     
  case EXPR_STRUCTURE:         
    g95_status("%s(", l->symbol->name);        
    show_constructor(l->value.constructor);        
    g95_status_char(')');        
    break;        
        
  case EXPR_ARRAY:  
    g95_status("(/ ");          
    show_constructor(l->value.constructor);   
    g95_status(" /)");

    show_ref(l->ref);         
    break;  
  
  case EXPR_NULL:   
    g95_status("NULL()");   
    break;  
  
  case EXPR_CONSTANT:  
    switch(l->ts.type) {      
    case BT_INTEGER:  
      mpz_out_str(stdout, 10, l->value.integer);

      if (l->ts.kind != g95_default_integer_kind())
	g95_status("_%d", l->ts.kind);        
      break;   
   
    case BT_LOGICAL:         
      if (l->value.logical) g95_status(".true.");          
       else g95_status(".false."); 
      break;

    case BT_REAL: 
      mpf_out_str(stdout, 10, 0, l->value.real);         
      if (l->ts.kind != g95_default_real_kind())     
	g95_status("_%d", l->ts.kind);         
      break; 
 
    case BT_CHARACTER:    
      g = l->value.character.string;      
      
      g95_status_char('\'');          
          
      for(o=0; o<l->value.character.length; o++,g++) {         
	if (*g == '\'') g95_status("''");          
	else g95_status_char(*g); 
      }  
  
      g95_status_char('\'');      
      
      break;    
    
    case BT_COMPLEX:     
      g95_status("(complex ");          
          
      mpf_out_str(stdout, 10, 0, l->value.complex.r);          
      if (l->ts.kind != g95_default_complex_kind())         
	g95_status("_%d", l->ts.kind);         
         
      g95_status(" ");  
  
      mpf_out_str(stdout, 10, 0, l->value.complex.i);       
      if (l->ts.kind != g95_default_complex_kind())      
	g95_status("_%d", l->ts.kind);      
      
      g95_status(")");        
      break;          
          
    default:        
      g95_status("???");       
      break;          
    }  
  
    break;    
    
  case EXPR_VARIABLE: 
    g95_status("%s:%s", l->symbol->ns->proc_name->name, l->symbol->name); 
    show_ref(l->ref);   
    break; 
 
  case EXPR_OP:
    g95_status("(");     
    switch(l->operator) {         
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
         
    g95_show_expr(l->op1);       
       
    if (l->op2) {       
      g95_status(" ");      
      g95_show_expr(l->op2);       
    }     
     
    g95_status(")");   
    break;  
  
  case EXPR_FUNCTION: 
    if (l->value.function.isym == NULL) {       
      g95_status("%s:%s[", l->symbol->module, l->symbol->name);   
      g95_show_actual_arglist(l->value.function.actual);   
      g95_status_char(']');          
    } else {    
      g95_status("%s[[", l->value.function.name);     
      g95_show_actual_arglist(l->value.function.actual);        
      g95_status_char(']'); 
      g95_status_char(']');
    }   
   
    break;        
        
  default: g95_internal_error("g95_show_expr(): Don't know how to show expr");  
  }    
}      
       
       
/* g95_copy_expr()-- Given an expression pointer, return a copy of the
 * expression.  This subroutine is recursive. */  
  
g95_expr *g95_copy_expr(g95_expr *m) {          
g95_expr *v;
int l;          
char *w;   
   
  if (m == NULL) return NULL;     
     
  v = g95_get_expr();         
  *v = *m;       
       
  switch(v->type) {  
  case EXPR_SUBSTRING: 
    w = g95_getmem(m->value.character.length+1);
    v->value.character.string = w; 
 
    memcpy(w, m->value.character.string, m->value.character.length+1);

    v->op1 = g95_copy_expr(m->op1);        
    v->op2 = g95_copy_expr(m->op2);       
    break;         
         
  case EXPR_CONSTANT:      
    switch(v->ts.type) {        
    case BT_INTEGER:    
      mpz_init_set(v->value.integer, m->value.integer);  
      break;       
       
    case BT_REAL: 
      mpf_init_set(v->value.real, m->value.real);        
      break;   
   
    case BT_COMPLEX:       
      mpf_init_set(v->value.complex.r, m->value.complex.r);
      mpf_init_set(v->value.complex.i, m->value.complex.i);      
      break;    
    
    case BT_CHARACTER:       
      l = m->value.character.length;    
    
      w = g95_getmem(l+1);     
      v->value.character.string = w;         
         
      if (l != 0) memcpy(w, m->value.character.string, l+1);  
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
    switch(v->operator) {      
    case INTRINSIC_NOT:       
    case INTRINSIC_UPLUS:        
    case INTRINSIC_UMINUS:   
      v->op1 = g95_copy_expr(m->op1);        
      break;      
      
    default:               /* Binary operators */  
      v->op1 = g95_copy_expr(m->op1);   
      v->op2 = g95_copy_expr(m->op2);  
      break;      
    }          
          
    break;         
         
  case EXPR_FUNCTION: 
    v->value.function.actual =  
      g95_copy_actual_arglist(m->value.function.actual);    
    break;    
    
  case EXPR_STRUCTURE:          
  case EXPR_ARRAY:   
    v->value.constructor = g95_copy_constructor(m->value.constructor);   
    break;         
         
  case EXPR_VARIABLE:
  case EXPR_NULL: 
    break;          
  }        
        
  v->shape = g95_copy_shape(m->shape, m->rank);        
        
  v->ref = copy_ref(m->ref);         
         
  return v;        
}     
     
     
   
   
/* simplify_ref()-- Simplify an reference structure */    
    
static try simplify_ref(g95_ref *re, int flag) {       
try r;    
int a;

  r = SUCCESS;      
     
  switch(re->type) {        
  case REF_ARRAY:      
    for(a=0; a<re->u.ar.dimen; a++) { 
      if (g95_simplify_expr(re->u.ar.start[a],  flag) == FAILURE) r = FAILURE;   
      if (g95_simplify_expr(re->u.ar.end[a],    flag) == FAILURE) r = FAILURE;          
      if (g95_simplify_expr(re->u.ar.stride[a], flag) == FAILURE) r = FAILURE;  
    }          
          
    break; 
 
  case REF_COMPONENT:     
    break;          
          
  case REF_SUBSTRING:     
    if (g95_simplify_expr(re->u.ss.start, flag) == FAILURE) r = FAILURE;        
    if (g95_simplify_expr(re->u.ss.end, flag)   == FAILURE) r = FAILURE;        
    break;
  }

  return r; 
}


     
     
/* g95_logical_expr()-- Returns an expression node that is a logical
 * constant. */ 
 
g95_expr *g95_logical_expr(int e, locus *w) {      
g95_expr *y; 
 
  y = g95_get_expr();        
        
  y->type = EXPR_CONSTANT;          
  y->ts.type = BT_LOGICAL;          
  y->ts.kind = g95_default_logical_kind();         
         
  if (w == NULL) w = g95_current_locus();   
  y->where = *w;         
  y->value.logical = e;       
       
  return y;   
}    
    
    
 
 
/* g95_int_expr()-- Returns an expression node that is an integer
 * constant. */          
          
g95_expr *g95_int_expr(int m) {         
g95_expr *x;    
    
  x = g95_get_expr();    
    
  x->type = EXPR_CONSTANT;  
  x->ts.type = BT_INTEGER;     
  x->ts.kind = g95_default_integer_kind();

  x->where = *g95_current_locus();      
  mpz_init_set_si(x->value.integer, m);        
        
  return x;        
}  
  
  
          
          
/* restricted_elemental()-- Check an intrinsic function call to see if
 * it is an elemental function returning character or integer with
 * character and integer arguments.  This function is a catchall for
 * most of the restricted intrinsics. */        
        
static try restricted_elemental(g95_expr *u) {      
g95_intrinsic_sym *sy;          
          
  sy = u->value.function.isym;       
      
  if (!sy->elemental) {    
    g95_error("Intrinsic function '%s' in specification expression at %C" 
	      "must be ELEMENTAL", sy->name);     
    return FAILURE;       
  }         
         
  if (sy->ts.type != BT_INTEGER && sy->ts.type != BT_CHARACTER) {       
    g95_error("Intrinsic function '%s' in specification expression at %C"     
	      "must return INTEGER or CHARACTER", sy->name);          
    return FAILURE;        
  }      
      
  return restricted_args(u->value.function.actual->next, 1);   
}    
    
    
       
       
/* g95_build_call()-- Build and return a g95_code structure that
 * corresponds to a subroutine call.  The arguments to this function
 * are a set of expression pointers (terminated by NULL) that compose
 * the actual arugment list. */

g95_code *g95_build_call(char *sub_name, ...) {        
g95_actual_arglist *t; 
g95_expr *e1; 
va_list ap;        
g95_code *l;    
   
  l = g95_get_code();         
         
  l->type = EXEC_CALL;       
  l->sub_name = sub_name;  
  
  t = NULL;    
    
  va_start(ap, sub_name);

  for(;;) {
    e1 = va_arg(ap, g95_expr *);  
    if (e1 == NULL) break; 
 
    if (l->ext.actual == NULL) {          
      t = g95_get_actual_arglist();       
      l->ext.actual = t;    
    } else {         
      t->next = g95_get_actual_arglist();  
      t = t->next;       
    }  
  
    t->type = EXPR; 
    t->u.expr = e1;    
  } 
 
  va_end(ap);         
         
  return l;  
}        
        
        
      
      
/* simplify_constructor()-- Subroutine to simplify constructor
 * expressions.  Mutually recursive with g95_simplify_expr(). */         
         
static try simplify_constructor(g95_constructor *a, int dtype) {         
         
  for(;a; a=a->next) {       
    if (a->iterator &&   
	(g95_simplify_expr(a->iterator->start, dtype) == FAILURE ||    
	 g95_simplify_expr(a->iterator->end, dtype) == FAILURE || 
	 g95_simplify_expr(a->iterator->step, dtype) == FAILURE))         
      return FAILURE;     
     
    if (a->expr && g95_simplify_expr(a->expr, dtype) == FAILURE) return FAILURE;      
  }        
        
  return SUCCESS;
}      
      
      
    
    
/* numeric_type()-- Returns nonzero if the type is numeric, zero
 * otherwise */    
    
static int numeric_type(bt type) { 
 
  return type == BT_COMPLEX || type == BT_REAL || type == BT_INTEGER;         
} 
 
 
 
 
/* restricted_array_inquiry()-- Check an array inquiry function as
 * part of a restricted expression.  The array argument does not have
 * to be a restricted variable, but the array must not be allocatable
 * or a nondummy pointer array. */  
  
static try restricted_array_inquiry(g95_expr *g) {    
g95_expr *array; 
g95_symbol *s;          
          
  array = g->value.function.actual->u.expr;       
       
  if (array->type == EXPR_VARIABLE) {         
    s = array->symbol;        
        
    if (s->attr.allocatable || (s->attr.pointer && !s->attr.dummy)) {         
      g95_error("Unknown bounds for array '%s' in restricted expression at %C", 
		s->name); 
      return FAILURE;        
    }          
  }  
  
  return restricted_args(g->value.function.actual->next, 0);          
} 
 
 
   
   
/* restricted_len()-- Check the LEN() function as part of a restricted
 * expression.  The argument of LEN can be a character variable that
 * isn't a restricted expression, but in which the length is a
 * restricted expression.  If the variable already exists, then the
 * length was already a specification expression. */        
        
static try restricted_len(g95_expr *g) {        
g95_expr *arg;       
       
  arg = g->value.function.actual->u.expr;    
    
  if (arg->type == EXPR_VARIABLE && arg->ts.type == BT_CHARACTER)       
    return SUCCESS; 
 
  return restricted_args(g->value.function.actual, 1);       
}    
    
    


/* g95_numeric_ts()-- Returns nonzero if the typespec is a numeric
 * type, zero otherwise. */       
       
int g95_numeric_ts(g95_typespec *ts) {  
  
  return numeric_type(ts->type); 
}         
         
         
         
         
/* g95_is_constant_expr()-- Function to determine if an expression is
 * constant or not.  This function expects that the expression has
 * already been simplified. */          
          
int g95_is_constant_expr(g95_expr *w) {   
g95_constructor *g;        
g95_actual_arglist *argum;    
int rc;

  if (w == NULL) return 1;          
         
  switch(w->type) {   
  case EXPR_OP:        
    rc = g95_is_constant_expr(w->op1) &&  
      (w->op2 == NULL || g95_is_constant_expr(w->op2));      
      
    break;         
         
  case EXPR_VARIABLE:         
    rc = 0;      
    break;   
   
  case EXPR_FUNCTION:
    rc = 0;     
    /* call to intrinsic with at least one argument */       
    if (w->value.function.isym && w->value.function.actual) {     
      for(argum = w->value.function.actual; argum; argum = argum->next){         
	if (!g95_is_constant_expr(argum->u.expr))    
	  break;    
      }     
      if (argum == NULL)      
	rc = 1;       
    }       
    break;     
     
  case EXPR_CONSTANT:         
  case EXPR_NULL:    
    rc = 1;
    break;       
       
  case EXPR_SUBSTRING:       
    rc = g95_is_constant_expr(w->op1) && g95_is_constant_expr(w->op2);     
    break;       
       
  case EXPR_STRUCTURE:          
    rc = 0;        
    for(g=w->value.constructor; g; g=g->next)    
      if (!g95_is_constant_expr(g->expr)) break;        
        
    if (g == NULL) rc = 1;    
    break;     
     
  case EXPR_ARRAY:      
    rc = g95_constant_ac(w);        
    break;

  default:      
    g95_internal_error("g95_is_constant_expr(): Unknown expression type");        
  }         
         
  return rc;     
}


      
      
/* simplify_intrinsic_op()-- Try to collapse intrinsic expressions */          
          
static try simplify_intrinsic_op(g95_expr *s, int t) {      
g95_expr *op_1, *op0, *res;  
  
  if (s->operator == INTRINSIC_USER) return SUCCESS;      
      
  op_1 = s->op1;    
  op0 = s->op2;          
          
  if (g95_simplify_expr(op_1, t) == FAILURE) return FAILURE;       
  if (g95_simplify_expr(op0, t) == FAILURE) return FAILURE;    
    
  if (!g95_is_constant_expr(op_1) ||     
      (op0 != NULL && !g95_is_constant_expr(op0)))      
    return SUCCESS;  
  
/* Rip p apart */    
    
  s->op1 = NULL;         
  s->op2 = NULL;  
  
  switch(s->operator) {       
  case INTRINSIC_UPLUS:  
    res = g95_uplus(op_1);        
    break;       
       
  case INTRINSIC_UMINUS:         
    res = g95_uminus(op_1);   
    break;        
        
  case INTRINSIC_PLUS: 
    res = g95_add(op_1, op0);  
    break;   
   
  case INTRINSIC_MINUS:          
    res = g95_subtract(op_1, op0);     
    break;      
      
  case INTRINSIC_TIMES:       
    res = g95_multiply(op_1, op0);      
    break;        
        
  case INTRINSIC_DIVIDE:     
    if ((op0->ts.type == BT_INTEGER &&
	 mpz_cmp_ui(op0->value.integer, 0) == 0) ||   
	(op0->ts.type == BT_REAL &&  
	 mpf_cmp_ui(op0->value.real, 0) == 0) ||        
	(op0->ts.type == BT_COMPLEX &&         
	 mpf_cmp_ui(op0->value.complex.r, 0) == 0 &&
	 mpf_cmp_ui(op0->value.complex.i, 0) == 0)) {  
      s->op1 = op_1;      
      s->op2 = op0;
      return SUCCESS;
    }    
    
    res = g95_divide(op_1, op0); 
    break;       
       
  case INTRINSIC_POWER:     
    res = g95_power(op_1, op0);         
    break;      
      
  case INTRINSIC_CONCAT:          
    res = g95_concat(op_1, op0);      
    break;        
        
  case INTRINSIC_EQ:              
    res = g95_eq(op_1, op0);
    break; 
 
  case INTRINSIC_NE:    
    res = g95_ne(op_1, op0);          
    break;   
   
  case INTRINSIC_GT:      
    res = g95_gt(op_1, op0);        
    break;

  case INTRINSIC_GE:       
    res = g95_ge(op_1, op0);      
    break;       
       
  case INTRINSIC_LT:   
    res = g95_lt(op_1, op0);    
    break;        
        
  case INTRINSIC_LE:         
    res = g95_le(op_1, op0); 
    break;      
      
  case INTRINSIC_NOT:   
    res = g95_not(op_1);      
    break;     
     
  case INTRINSIC_AND:      
    res = g95_and(op_1, op0);    
    break;

  case INTRINSIC_OR:  
    res = g95_or(op_1, op0);  
    break;        
        
  case INTRINSIC_EQV: 
    res = g95_eqv(op_1, op0);         
    break; 
 
  case INTRINSIC_NEQV:       
    res = g95_neqv(op_1, op0);    
    break;   
   
  default: g95_internal_error("simplify_intrinsic_op(): Bad operator");         
  }  
  
  if (res == NULL) { 
    g95_free_expr(op_1);  
    g95_free_expr(op0);      
    return FAILURE;
  }

  g95_replace_expr(s, res);

  return SUCCESS;         
}


    
    
/* check_intrinsic_op()-- Check an intrinsic arithmetic operation to
 * see if it is consistent with some type of expression. */ 
 
static try check_intrinsic_op(g95_expr *q, try (*check_function)(g95_expr *)) {          
           
  if ((*check_function)(q->op1) == FAILURE) return FAILURE;        
        
  switch(q->operator) {    
  case INTRINSIC_UPLUS:
  case INTRINSIC_UMINUS: 
    if (!numeric_type(et0(q->op1))) goto not_numeric;
    break;       
       
  case INTRINSIC_EQ:  case INTRINSIC_NE:  case INTRINSIC_GT:     
  case INTRINSIC_GE:  case INTRINSIC_LT:  case INTRINSIC_LE:  
  
  case INTRINSIC_PLUS:    case INTRINSIC_MINUS:  case INTRINSIC_TIMES:          
  case INTRINSIC_DIVIDE:  case INTRINSIC_POWER:
    if ((*check_function)(q->op2) == FAILURE) return FAILURE;  
  
    if (!numeric_type(et0(q->op1)) ||         
	!numeric_type(et0(q->op2))) goto not_numeric;     
     
    if (q->operator != INTRINSIC_POWER) break;         
             
    if (check_function == check_init_expr && et0(q->op2) != BT_INTEGER) {   
      g95_error("Exponent at %L must be INTEGER for an initialization "  
		"expression", &q->op2->where);         
      return FAILURE;    
    }    
    
    break;     
     
  case INTRINSIC_CONCAT:     
    if ((*check_function)(q->op2) == FAILURE) return FAILURE;         
         
    if (et0(q->op1) != BT_CHARACTER || et0(q->op2) != BT_CHARACTER) {   
      g95_error("Concatenation operator in expression at %L "     
		"must have two CHARACTER operands", &q->op1->where);     
      return FAILURE;      
    }

    if (q->op1->ts.kind != q->op2->ts.kind) {          
      g95_error("Concat operator at %L must concatenate strings of the "       
		"same kind", &q->where); 
      return FAILURE;
    }

    break;  
  
  case INTRINSIC_NOT:
    if (et0(q->op1) != BT_LOGICAL) {      
      g95_error(".NOT. operator in expression at %L must have a LOGICAL "         
		"operand", &q->op1->where);
      return FAILURE;        
    }     
     
    break;        
        
  case INTRINSIC_AND:    case INTRINSIC_OR:          
  case INTRINSIC_EQV:    case INTRINSIC_NEQV:     
    if ((*check_function)(q->op2) == FAILURE) return FAILURE;    
    
    if (et0(q->op1) != BT_LOGICAL || et0(q->op2) != BT_LOGICAL) {    
      g95_error("LOGICAL operands are required in expression at %L",      
		&q->where);      
      return FAILURE;
    }     
     
    break;  
  
  default:   
    g95_error("Only intrinsic operators can be used in expression at %L",
	      &q->where);          
    return FAILURE;   
  }       
       
  return SUCCESS;        
        
not_numeric:    
  g95_error("Numeric operands are required in expression at %L", &q->where);  
  
  return FAILURE;       
}         
         
         
        
        
/* restricted_null()-- Check the NULL() function as part of a
 * restricted expression.  NULL is always OK. */    
    
static try restricted_null(g95_expr *f) {    
    
  f = NULL;      
  return SUCCESS;  
}         
         
         
     
     
/* restricted_nonelemental()-- Check an intrinsic function that does
 * not have to be elemental, yet requires integer or character
 * arguements. */        
        
static try restricted_nonelemental(g95_expr *o) {      
      
  return restricted_args(o->value.function.actual, 1);      
} 
 
 
 
 
/* check_init_expr()-- Verify that an expression is an
 * initialization expression.  A side effect is that the expression
 * tree is reduced to a single constant node if all goes well.  This
 * would normally happen when the expression is constructed but
 * function references are assumed to be intrinsics in the context of
 * initialization expressions.  If FAILURE is returned an error
 * message has been generated. */     
     
static try check_init_expr(g95_expr *l) {    
g95_actual_arglist *ap;
match g;    
try q;

  if (l == NULL) return SUCCESS;     
     
  switch(l->type) {          
  case EXPR_OP:          
    q = check_intrinsic_op(l, check_init_expr); 
    if (q == SUCCESS) q = g95_simplify_expr(l, 0);    
    
    break;    
    
  case EXPR_FUNCTION: 
    q = SUCCESS;        
        
    if (check_inquiry(l) != SUCCESS) {         
      q = SUCCESS; 
      for(ap=l->value.function.actual; ap; ap=ap->next)      
	if (check_init_expr(ap->u.expr) == FAILURE) {  
	  q = FAILURE;      
	  break;     
	} 
    }         
         
    if (q == SUCCESS) { 
      g = g95_intrinsic_func_interface(l, 0);  
  
      if (g == MATCH_NO)        
	g95_error("Function '%s' in initialization expression at %L "  
		  "must be an intrinsic function", l->symbol->name, &l->where);          
          
      if (g != MATCH_YES) q = FAILURE;  
    }        
        
    break;         
         
  case EXPR_VARIABLE:      
    q = SUCCESS;      
      
    if (g95_check_iter_variable(l) == SUCCESS) break;

    if (l->symbol->attr.flavor == FL_PARAMETER) {    
      g95_replace_expr(l, g95_copy_expr(l->symbol->value));    
      break;      /* TODO: constant references to subobjects */          
    }   
   
    g95_error("Variable '%s' at %L cannot appear in an initialization "        
	      "expression", l->symbol->name, &l->where);    
    q = FAILURE;    
    break;

  case EXPR_CONSTANT:      
  case EXPR_NULL:     
    q = SUCCESS;      
    break;      
      
  case EXPR_SUBSTRING:       
    q = check_init_expr(l->op1);
    if (q == FAILURE) break; 
 
    q = check_init_expr(l->op2);   
    if (q == SUCCESS) q = g95_simplify_expr(l, 0);       
       
    break;  
  
  case EXPR_STRUCTURE:  
    q = g95_check_constructor(l, check_init_expr);     
    break; 
 
  case EXPR_ARRAY:     
    q = g95_check_constructor(l, check_init_expr);        
    if (q == FAILURE) break;         
         
    q = g95_check_constructor_type(l);  
    break;    
        
  default:       
    g95_internal_error("check_init_expr(): Unknown expression type");         
  }        
        
  return q; 
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
       
try g95_simplify_expr(g95_expr *n, int type) {   
g95_actual_arglist *actualp;    
g95_ref *r;       
       
  if (n == NULL) return SUCCESS;      
      
/* Replace a parameter variable with its value */    
    
  switch(n->type) {        
  case EXPR_CONSTANT:
  case EXPR_NULL:          
    break;          
          
  case EXPR_FUNCTION:  
    for(actualp=n->value.function.actual; actualp; actualp=actualp->next) 
      if (g95_simplify_expr(actualp->u.expr, type) == FAILURE) return FAILURE;  
  
    if (n->value.function.isym != NULL &&      
	g95_intrinsic_func_interface(n, 1) == MATCH_ERROR) return FAILURE;   
   
    break;

  case EXPR_SUBSTRING:         
    if (g95_simplify_expr(n->op1, type) == FAILURE ||         
	g95_simplify_expr(n->op2, type) == FAILURE) return FAILURE;       
       
/* TODO: evaluate constant substrings */ 
 
    break;         
         
  case EXPR_OP:
    if (simplify_intrinsic_op(n, type) == FAILURE) return FAILURE;       
    break; 
 
  case EXPR_VARIABLE: 
    if (n->symbol->attr.flavor == FL_PARAMETER &&      
	n->symbol->value->type != EXPR_ARRAY) {      
      g95_replace_expr(n, g95_copy_expr(n->symbol->value));   
      break;        
    }     
     
    if (type == 1) g95_simplify_iterator_var(n);      
      
    for(r=n->ref; r; r=r->next)    
      if (simplify_ref(r, type) == FAILURE) return FAILURE;         
         
    break;     
     
  case EXPR_STRUCTURE:     
  case EXPR_ARRAY: 
    if (simplify_constructor(n->value.constructor, type) == FAILURE)
      return FAILURE;

    break;         
  }         
         
  return SUCCESS;     
}          
          
          
       
       
/* restricted_intrinsic()-- Check to see that a function reference to
 * an intrinsic is a restricted expression.  Some functions required
 * by the standard are omitted because references to them have already
 * been simplified.  Strictly speaking, a lot of these checks are
 * redundant with other checks.  If a function is indeed a particular
 * intrinsic, then the type of its argument have already been checked
 * and passed. */     
     
static try restricted_intrinsic(g95_expr *n) {         
         
static struct { char *fname; try (*function)(g95_expr *n); } *cp, cases[] = {         
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
   
char *name0;    
    
  name0 = n->value.function.isym->name;

  for(cp=cases; cp->fname; cp++)          
    if (cp->fname == NULL || strcmp(cp->fname, name0) == 0) break;     
     
  return cp->function(n);
}     
     
     
       
       
/* check_restricted()-- Verify that an expression is a restricted
 * expression.  Like its cousin check_init_expr(), an error message is
 * generated if we return FAILURE. */  
  
static try check_restricted(g95_expr *m) {         
g95_symbol *symb;
try g;       
       
  if (m == NULL) return SUCCESS;     
     
  g = SUCCESS;  
  
  switch(m->type) {  
  case EXPR_OP:        
    g = check_intrinsic_op(m, check_restricted);       
    if (g == SUCCESS) g = g95_simplify_expr(m, 0);  
  
    break;  
  
  case EXPR_FUNCTION:          
    g = (m->value.function.isym != NULL) ?        
      restricted_intrinsic(m) : external_spec_function(m);

    break;   
   
  case EXPR_VARIABLE:       
    symb = m->symbol;         
    g = FAILURE;    
        
    if (symb->attr.optional) {       
      g95_error("Dummy argument '%s' at %L cannot be OPTIONAL",     
		symb->name, &m->where);   
      break; 
    }

    if (symb->attr.intent == INTENT_OUT) {  
      g95_error("Dummy argument '%s' at %L cannot be INTENT(OUT)",
		symb->name, &m->where);  
      break;         
    }          
          
    if (symb->attr.in_common || symb->attr.use_assoc || symb->attr.dummy ||          
	symb->ns != g95_current_ns ||      
	(symb->ns->proc_name != NULL &&          
	 symb->ns->proc_name->attr.flavor == FL_MODULE)) {       
      g = SUCCESS;        
      break; 
    }     
     
    g95_error("Variable '%s' cannot appear in the expression at %L",     
	      symb->name, &m->where);

    break;        
        
  case EXPR_NULL:  
  case EXPR_CONSTANT:   
    g = SUCCESS;      
    break;     
     
  case EXPR_SUBSTRING:  
    if (m->op1 != NULL) {    
      g = g95_specification_expr(m->op1);    
      if (g == FAILURE) break;  
    }   
   
    if (m->op2 != NULL) {  
      g = g95_specification_expr(m->op2);   
      if (g == SUCCESS) g = g95_simplify_expr(m, 0); 
    }          
          
    break;         
         
  case EXPR_STRUCTURE:     
    g = g95_check_constructor(m, check_restricted);    
    break;          
          
  case EXPR_ARRAY:      
    g = g95_check_constructor(m, check_restricted);     
    break;         
         
  default:        
    g95_internal_error("check_spec_expr(): Unknown expression type");     
  }       
       
  return g;         
}       
       
       
