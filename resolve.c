/* Perform type resolution on the various stuctures.
   Copyright (C) 2001 - 2003 Free Software Foundation, Inc.
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
         
         
#include <string.h>
#include "g95.h"


/* Stack to push the current if we descend into a block during
 * resolution.  See resolve_branch() and resolve_code().  */

typedef struct code_stack { 
  struct g95_code *head, *current;    
  struct code_stack *prev;
} code_stack;         
         
static code_stack *cs_base = NULL;         
         
static int forall_flag;   /* Nonzero if we're inside a FORALL block */         
         
typedef enum { PTYPE_GENERIC=1, PTYPE_SPECIFIC, PTYPE_UNKNOWN } proc_type;
typedef enum { CMP_LT, CMP_EQ, CMP_GT, CMP_UNKNOWN } comparison;         
         
static void move_constructor(g95_constructor *, g95_symbol *, g95_symbol *);         
         
extern int g95_constructor_string_length;     
     
     
   
   
/* resolve_charlen()-- Resolve character lengths */         
         
static try resolve_charlen(g95_namespace *name) {       
g95_charlen *charlen;     
try b;     
     
  b = SUCCESS;     
     
  for(charlen=name->cl_list; charlen; charlen=charlen->next) { 
    if (charlen->length == NULL) continue;

    if (g95_resolve_expr(charlen->length) == FAILURE) {     
      b = FAILURE;    
      continue;
    }       
       
    if (charlen->length->ts.type != BT_INTEGER) {  
      g95_error("Character length specification at %L must be of type INTEGER",        
		&charlen->length->where); 
      b = FAILURE;          
    }
  }         
         
  return b;  
}     
     
     
    
    
/* mark_external()-- It is possible for a symbol to be marked as
 * EXTERNAL or INTRINSIC in a module subprogram, but not be explicitly
 * defined.  This subroutine explicitly marks such procedures and
 * makes sure that they are being used correctly across module
 * procedures. */         
         
static try mark_external(g95_symbol *symbol, int function_flag) {        
        
  if ((function_flag && symbol->attr.subroutine) || 
      (!function_flag && symbol->attr.function)) {
    g95_error("Symbol '%s' at %L is used as both a FUNCTION and a SUBROUTINE",         
	      symbol->name, &symbol->declared_at);     
    return FAILURE;        
  } 
 
  if (symbol->attr.function || symbol->attr.subroutine) return SUCCESS;  
  
  /* Set the procedure type at this point */         
         
  return function_flag ?  
    g95_add_function(&symbol->attr, &symbol->declared_at) :          
    g95_add_subroutine(&symbol->attr, &symbol->declared_at);
} 
 
 
       
       
/* resolve_common_block()-- Resolve the blocks as a whole */    
    
static void resolve_common_block(g95_symtree *st) {   
g95_common_head *c;          
g95_symbol *v;       
int a; 
 
  if (st == NULL) return;          
          
  resolve_common_block(st->left);   
  resolve_common_block(st->right);      
      
  a = 0; 
  c = st->n.common;       
       
  for(v=c->head; v; v=v->common_next)    
    if (v->value != NULL) {  
      a = 1; 
      break; 
    }        
        
  if (g95_option.pedantic && a && !c->saved) {          
    g95_error("Initialized COMMON block '%s' at %L must have the SAVE "         
	      "attribute", st->name, &st->n.common->where);
    return;
  }         
}  
  
  
          
          
/* transform_while()-- Transform a DO WHILE statement into an infinite
 * loop with an appropriate EXIT statement at the front. */         
         
static void transform_while(g95_code *codep) {        
g95_code *n, *x; 
g95_expr *j;    
    
  j = codep->expr;          
  if (j->type == EXPR_CONSTANT && j->value.logical)
    g95_free_expr(j); 
  else {      
    j = g95_get_expr();       
    j->type = EXPR_OP;     
    j->operator = INTRINSIC_NOT;      
      
    j->ts.type = BT_LOGICAL;
    j->ts.kind = g95_default_logical_kind();     
    j->op1 = codep->expr;        
        
    x = g95_get_code();        
    x->type = EXEC_EXIT;  
    x->ext.block = codep;  
  
    n = g95_get_code();    
    n->block = x;    
    n->expr = j;          
    n->type = EXEC_IF;   
   
    n->next = codep->block;       
    codep->block = n;      
  }

  codep->expr = NULL; 
}        
        
        
  
  
/* derived_init()-- Return nonzero if the derived type symbol has a
 * default initialization or contains a subtype that has a default
 * initialization.  */         
         
static int derived_init(g95_symbol *sy) {   
g95_component *w;       
       
  for(w=sy->ts.derived->components; w; w=w->next) {         
    if (w->initializer != NULL) return 1;

    if (w->ts.type != BT_DERIVED || w->pointer) continue;         
         
    if (derived_init(w->ts.derived)) return 1; 
  }   
   
  return 0;
} 
 
 
     
     
/* compare_bound_int()-- Compare an integer expression with an integer. */ 
 
static int compare_bound_int(g95_expr *q, int t) {  
int w;     
     
  if (q == NULL || q->type != EXPR_CONSTANT) return CMP_UNKNOWN;      
       
  if (q->ts.type != BT_INTEGER)     
    g95_internal_error("compare_bound_int(): Bad expression");   
   
  w = mpz_cmp_si(q->value.integer, t); 
 
  if (w < 0) return CMP_LT; 
  if (w > 0) return CMP_GT; 
  return CMP_EQ;         
}     
     
     
       
       
/* resolve_derived()-- Resolve a derived type symbol */    
    
static try resolve_derived(g95_symbol *sym) {          
g95_component *t;   
   
  if (!sym->attr.set) {       
    g95_error("Derived type '%s' at %L never defined",
	      sym->name, &sym->declared_at);      
    return FAILURE;          
  }   
   
  for(t=sym->components; t; t=t->next) {  
    if (t->as == NULL) continue;  
    if (g95_resolve_array_spec(t->as, 1) == FAILURE) return FAILURE;   
  }          
          
  return SUCCESS;         
}          
          
          
          
          
/* insert_full_array_ref()-- Insert a full array reference node in the
 * middle of a reference list just before the 'ref' node. */

static void insert_full_array_ref(g95_expr *g, g95_ref *reference) {         
g95_ref *y, *new;         
         
  new = g95_get_ref();         
         
  new->type = REF_ARRAY;   
  new->u.ar.type = AR_FULL;    
  new->next = reference;

  if (g->ref == reference)       
    g->ref = new;   
  else {       
    y = g->ref;  
    while(y->next != reference)    
      y = y->next;          
          
    y->next = new;        
  }   
}      
      
      
       
       
/* resolve_index()-- Resolve one part of an array index */      
      
static try resolve_index(g95_expr *i, int check_scalar) {   
g95_typespec typ;      
      
  if (i == NULL) return SUCCESS;          
          
  if (g95_resolve_expr(i) == FAILURE) return FAILURE;     
     
  if (i->ts.type != BT_INTEGER) {      
    g95_error("Array index at %L must be of INTEGER type", &i->where);         
    return FAILURE;  
  } 
 
  if (check_scalar && i->rank != 0) {     
    g95_error("Array index at %L must be scalar", &i->where);      
    return FAILURE; 
  }

  if (i->ts.kind != g95_default_integer_kind()) {
    typ.type = BT_INTEGER;   
    typ.kind = g95_default_integer_kind();        
        
    g95_convert_type(i, &typ, 2); 
  }       
       
  return SUCCESS;      
}        
        
        
    
    
/* find_declaration()-- Given a symbol, figure out where it is
 * declared.  Returns the symbol to use, usually the original symbol
 * itself. */        
        
static g95_symbol *find_declaration(g95_symbol *symb) { 
g95_namespace *n;        
g95_symbol *h;   
   
  n = symb->ns->parent;         
         
  for(;;) {     
    if (g95_local_symbol(symb) || n == NULL) break;     
    if (g95_find_symbol(symb->name, n, 1, &h) || h == NULL) break;     
     
    if (g95_local_symbol(h)) {     
      symb->attr.resolved = 1;     
      symb = h;
      break;
    }         
         
    n = n->parent;         
  }         
         
  return symb;        
}       
       
       
   
   
/* expression_shape()-- Given an expression, determine its shape.
 * This is easier than it sounds.  Leaves the shape array NULL if it
 * is not possible to determine the shape. */        
        
static void expression_shape(g95_expr *j) {
mpz_t arr[G95_MAX_DIMENSIONS];      
int y;          
          
  if (j->rank == 0 || j->shape != NULL) return;     
    
  for(y=0; y<j->rank; y++)
    if (g95_array_dimen_size(j, y, &arr[y]) == FAILURE) goto fail;       
       
  j->shape = g95_get_shape(j->rank);        
        
  memcpy(j->shape, &arr, j->rank*sizeof(mpz_t));    
    
  return;       
       
 fail:     
  for(y--; y>=0; y--)       
    mpz_clear(arr[y]);         
}       
       
       
  
  
/* move_variable()-- Given the right hand side of the statement
 * function, replace the old symbol which is a formal argument with a
 * new symbol in the right namespace.  Recursively traverses the
 * expression tree. */     
     
static void move_variable(g95_expr *z, g95_symbol *o, g95_symbol *new) {    
g95_actual_arglist *c;         
g95_ref *r;     
int s;        
        
  if (z == NULL) return;          
          
  switch(z->type) {          
  case EXPR_OP:      
    move_variable(z->op1, o, new);
    move_variable(z->op2, o, new);   
    break;

  case EXPR_FUNCTION:
    if (z->symbol == o) z->symbol = new;

    for(c=z->value.function.actual; c; c=c->next)     
      move_variable(c->u.expr, o, new);

    break;     
     
  case EXPR_CONSTANT:        
  case EXPR_NULL:  
    break;      
      
  case EXPR_UNKNOWN:          
  case EXPR_VARIABLE:      
    if (z->symbol == o) z->symbol = new;        
        
    /* Fall through */   
   
  case EXPR_SUBSTRING: 
    for(r=z->ref; r; r=r->next)    
      switch(r->type) {        
      case REF_ARRAY:     
	for(s=0; s<G95_MAX_DIMENSIONS; s++) {   
	  move_variable(r->u.ar.start[s], o, new);     
	  move_variable(r->u.ar.end[s], o, new);
	  move_variable(r->u.ar.stride[s], o, new);    
	}   
   
	break;         
         
      case REF_SUBSTRING:       
	move_variable(r->u.ss.start, o, new);
	move_variable(r->u.ss.end, o, new);    
	break;        
        
      case REF_COMPONENT:          
	break;     
      }       
       
    break;        
        
  case EXPR_STRUCTURE:  
  case EXPR_ARRAY:   
    move_constructor(z->value.constructor, o, new);         
    break;   
  }        
}         
         
         
        
        
/* resolve_generic_f0()-- Resolve a function call known to be generic.
 * Section 14.1.2.4.1. */ 
 
static match resolve_generic_f0(g95_expr *e2, g95_symbol *sym) {
g95_symbol *q;       
       
  if (sym->attr.generic) {        
    q = g95_search_interface(sym->generic, 0, &e2->value.function.actual);         
    if (q != NULL) {   
      if (q->attr.proc == PROC_UNKNOWN) q->attr.proc = PROC_EXTERNAL;     
     
      e2->value.function.name = q->name;    
    
      if (e2->symbol != q) {       
	e2->symbol->attr.resolved = 1;      
	e2->symbol = q;  
      }      
      
      e2->ts = q->result->ts;  
      if (q->result->as != NULL) e2->rank = q->result->as->rank;         
      return MATCH_YES;     
    }  
  
    /* TODO: Need to search for elemental references in generic interface */        
  }     
     
  if (sym->attr.intrinsic) return g95_intrinsic_func_interface(e2, 0);  
  
  return MATCH_NO; 
}         
         
         
      
      
/* find_component()-- Given a derived type node and a component name,
 * try to locate the component structure.  Returns the NULL pointer if
 * the component is not found or the components are private.  The flag
 * variable is nonzero if the parent variable has been use-associated,
 * which means the check for private components should be bypassed. */  
  
static g95_component *find_component(g95_symbol *sym, g95_ref *reference, int flag) {
g95_component *m; 
char *name; 
 
  name = reference->u.c.name;      
      
  for(m=sym->components; m; m=m->next)      
    if (strcmp(m->name, name) == 0) break;      
      
  if (m == NULL)     
    g95_error("Element '%s' at %L is not a member of the '%s' structure",          
	      name, &reference->where, sym->name);      
  else {   
    if (sym->attr.use_assoc && sym->component_access == ACCESS_PRIVATE &&    
	!flag) {          
      g95_error("Component '%s' at %L is a PRIVATE component of '%s'",     
		name, &reference->where, sym->name);     
      m = NULL;
    }          
  }    
    
  return m;  
}      
      
      


/* move_constructor()-- Mutually recursive with move_variable() to fix
 * symbol references within the right side of a statement function. */  
  
static void move_constructor(g95_constructor *c, g95_symbol *n,         
			     g95_symbol *new) {         
  for(; c; c=c->next) {
    move_variable(c->expr, n, new);  
  
    if (c->iterator == NULL) continue;          
          
    move_variable(c->iterator->start, n, new);    
    move_variable(c->iterator->end,   n, new);
    move_variable(c->iterator->step,  n, new);        
  }         
}        
        
        
         
         
/* g95_impure_variable()-- Determines if a variable is not 'pure', ie
 * not assignable within a pure procedure.  Returns zero if assignment
 * is OK, nonzero if there is a problem. */     
     
int g95_impure_variable(g95_symbol *sym) {          
          
  if (sym->attr.use_assoc || sym->attr.in_common) return 1;         
         
  if (sym->ns != g95_current_ns) return !sym->attr.function;   
   
  /* TODO: Check storage association through EQUIVALENCE statements */         
         
  return 0;       
}    
    
    
     
     
/* resolve_operator()-- Resolve an operator expression node.  This can
 * involve replacing the operation with a user defined function call. */        
        
static try resolve_operator(g95_expr *a) {
g95_expr *op1, *op;  
char msg[200]; 
try c;         
         
/* Resolve all subnodes-- give them types. */    
    
  switch(a->operator) {   
  default: 
    if (g95_resolve_expr(a->op2) == FAILURE) return FAILURE;    
    
/* Fall through */          
          
  case INTRINSIC_NOT: 
  case INTRINSIC_UPLUS: 
  case INTRINSIC_UMINUS:
    if (g95_resolve_expr(a->op1) == FAILURE) return FAILURE;      
    break;     
  }     
     
/* Typecheck the new node. */   
   
  op1 = a->op1;         
  op = a->op2;  
  
  switch(a->operator) {          
  case INTRINSIC_UPLUS:          
  case INTRINSIC_UMINUS:        
    if ((op1->ts.type == BT_INTEGER) || (op1->ts.type == BT_REAL) ||
	(op1->ts.type == BT_COMPLEX)) {      
      a->ts = op1->ts;  
      break;         
    }          
          
    sprintf(msg, "Operand of unary numeric operator '%s' at %%L is %s",    
	    g95_op2string(a->operator), g95_typename(&a->ts));     
    goto bad_op;      
      
  case INTRINSIC_PLUS:   
  case INTRINSIC_MINUS:       
  case INTRINSIC_TIMES:
  case INTRINSIC_DIVIDE: 
  case INTRINSIC_POWER:     
    if (g95_numeric_ts(&op1->ts) && g95_numeric_ts(&op->ts)) {        
      g95_type_convert_binary(a);         
      break;        
    }

    sprintf(msg, "Operands of binary numeric operator '%s' at %%L are %s/%s",    
	    g95_op2string(a->operator), g95_typename(&op1->ts), 
	    g95_typename(&op->ts)); 
    goto bad_op;  
  
  case INTRINSIC_CONCAT:     
    if (op1->ts.type == BT_CHARACTER && op->ts.type == BT_CHARACTER) {      
      a->ts.type = BT_CHARACTER;         
      a->ts.kind = op1->ts.kind;  
      break;          
    }    
    
    sprintf(msg, "Operands of string concatenation operator at %%L are %s/%s",     
	    g95_typename(&op1->ts), g95_typename(&op->ts));        
    goto bad_op;     
     
  case INTRINSIC_AND:
  case INTRINSIC_OR:         
  case INTRINSIC_EQV:
  case INTRINSIC_NEQV:  
    if (op1->ts.type == BT_LOGICAL && op->ts.type == BT_LOGICAL) {
      a->ts.type = BT_LOGICAL;        
      a->ts.kind = g95_kind_max(op1, op);   
      break;    
    }  
  
    sprintf(msg, "Operands of logical operator '%s' at %%L are %s/%s",       
	    g95_op2string(a->operator), g95_typename(&op1->ts),   
	    g95_typename(&op->ts));      
      
    goto bad_op;          
                
  case INTRINSIC_NOT:      
    if (op1->ts.type == BT_LOGICAL) {        
      a->ts.type = BT_LOGICAL;    
      a->ts.kind = op1->ts.kind;          
      break;      
    } 
 
    sprintf(msg, "Operand of .NOT. operator at %%L is %s",         
	    g95_typename(&op1->ts));         
    goto bad_op;     
     
  case INTRINSIC_GT: case INTRINSIC_GE:          
  case INTRINSIC_LT: case INTRINSIC_LE:      
    if (op1->ts.type == BT_COMPLEX || op->ts.type == BT_COMPLEX) {       
      strcpy(msg, "COMPLEX quantities cannot be compared at %L");
      goto bad_op;          
    }        
        
    /* Fall through */

  case INTRINSIC_EQ: case INTRINSIC_NE:  
    if (op1->ts.type == BT_CHARACTER && op->ts.type == BT_CHARACTER) { 
      a->ts.type = BT_LOGICAL;
      a->ts.kind = g95_default_logical_kind();          
      break;     
    }        
        
    if (g95_numeric_ts(&op1->ts) && g95_numeric_ts(&op->ts)) {  
      g95_type_convert_binary(a);
	
      a->ts.type = BT_LOGICAL;    
      a->ts.kind = g95_default_logical_kind();
      break;   
    } 
 
    sprintf(msg, "Operands of comparison operator '%s' at %%L are %s/%s",  
	    g95_op2string(a->operator), g95_typename(&op1->ts),          
	    g95_typename(&op->ts)); 
 
    goto bad_op;         
         
  case INTRINSIC_USER:
    if (op == NULL)         
      sprintf(msg, "Operand of user operator '%s' at %%L is %s",        
	      a->symbol->name, g95_typename(&op1->ts)); 
    else   
      sprintf(msg, "Operands of user operator '%s' at %%L are %s/%s", 
	      a->symbol->name, g95_typename(&op1->ts), g95_typename(&op->ts));      
      
    goto bad_op;     
     
  default:          
    g95_internal_error("resolve_operator(): Bad intrinsic"); 
  }  
  
/* Deal with arrayness of an operand through an operator */      
      
  c = SUCCESS;   
   
  switch(a->operator) {          
  case INTRINSIC_PLUS:    case INTRINSIC_MINUS:  case INTRINSIC_TIMES:      
  case INTRINSIC_DIVIDE:  case INTRINSIC_POWER:  case INTRINSIC_CONCAT:     
  case INTRINSIC_AND:     case INTRINSIC_OR:     case INTRINSIC_EQV:        
  case INTRINSIC_NEQV:    case INTRINSIC_EQ:     case INTRINSIC_NE:         
  case INTRINSIC_GT:      case INTRINSIC_GE:     case INTRINSIC_LT:    
  case INTRINSIC_LE:     
     
    if (op1->rank == 0 && op->rank == 0) a->rank = 0;     
     
    if (op1->rank == 0 && op->rank != 0) {
      a->rank = op->rank;    
    
      if (a->shape == NULL) a->shape = g95_copy_shape(op->shape, op->rank);         
    }       
       
    if (op1->rank != 0 && op->rank == 0) {          
      a->rank = op1->rank;         
         
      if (a->shape == NULL) a->shape = g95_copy_shape(op1->shape, op1->rank);          
    }    
    
    if (op1->rank != 0 && op->rank != 0) {          
      if (op1->rank == op->rank) {    
	a->rank = op1->rank;    
    
	if (a->shape == NULL) a->shape = g95_copy_shape(op1->shape, op1->rank);      
      
      } else {   
	g95_error("Inconsistent ranks for operator at %L and %L",    
		  &op1->where, &op->where);   
	c = FAILURE;    
    
	a->rank = 0;   /* Allow higher level expressions to work */       
      }        
    }       
       
    break;   
   
  case INTRINSIC_NOT:      
  case INTRINSIC_UPLUS:       
  case INTRINSIC_UMINUS:        
    a->rank = op1->rank;        
        
    if (a->shape == NULL) a->shape = g95_copy_shape(op1->shape, op1->rank);

    break;           /* Simply copy arrayness attribute */    
    
  default:  
    break; 
  }

  if (c == SUCCESS) c = g95_simplify_expr(a, 0);        
  return c;     
     
bad_op:    
  if (g95_extend_expr(a) == SUCCESS) return SUCCESS; 
 
  g95_error(msg, &a->where);    
  return FAILURE;        
}     
     
     
       
       
/* compare_bound()-- Compare two integer expressions. */      
      
static comparison compare_bound(g95_expr *t, g95_expr *e) {       
int y;        
        
  if (t == NULL || t->type != EXPR_CONSTANT ||
      e == NULL || e->type != EXPR_CONSTANT) return CMP_UNKNOWN;      
      
  if (t->ts.type != BT_INTEGER || e->ts.type != BT_INTEGER)          
    g95_internal_error("compare_bound(): Bad expression");

  y = mpz_cmp(t->value.integer, e->value.integer);    
    
  if (y < 0) return CMP_LT;  
  if (y > 0) return CMP_GT;     
  return CMP_EQ;  
}          
          
          
     
     
/* move_formal_arglist()-- Given a symbol that is a statement
 * function, effectively move its formal argument list into the
 * contained namespace so that the dummy arguments have a separate
 * existence from symbols of the same name in the parent. */       
       
static void move_formal_arglist(g95_symbol *sym) {  
g95_formal_arglist *g;   
g95_symbol *n, *n1;       
g95_expr *s;       
       
  s = g95_current_ns->code->expr2;    /* RHS of the statement function */         
         
  for(g=sym->formal; g; g=g->next) {
    n = g->sym;     
    n->attr.used = 1;  
  
    g95_get_symbol(n->name, g95_current_ns, &n1);     
    n1->ts = n->ts; 
    n1->declared_at = n->declared_at;   
   
    n1->attr.flavor = FL_VARIABLE;   
    n1->attr.dummy = 1;

    move_variable(s, n, n1);      
    g->sym = n1;       
  }     
}         
         
         
       
       
/* namespace_kind()-- Given a namespace, figure out what kind it is.
 * We return one of the g95_compile_state enums COMP_NONE,
 * COMP_MODULE, COMP_SUBROUTINE or COMP_FUNCTION. */  
  
static g95_compile_state namespace_kind(g95_namespace *n) {     
g95_symbol *s;

  s = n->proc_name;    
    
  if (s == NULL) return COMP_NONE;    
    
  if (s->attr.flavor == FL_MODULE) return COMP_MODULE;          
          
  if (s->attr.subroutine) return COMP_SUBROUTINE;     
     
  if (s->attr.flavor == FL_VARIABLE || 
      s->attr.function) return COMP_FUNCTION;

  return COMP_NONE;     
}       
       
       
          
          
/* g95_elemental()-- Test whether the current procedure is elemental or not */ 
 
int g95_elemental(g95_symbol *sym) {         
symbol_attribute attribute;    
    
  if (sym == NULL) sym = g95_current_ns->proc_name;     
  if (sym == NULL) return 0;        
  attribute = sym->attr;     
     
  return attribute.flavor == FL_PROCEDURE && attribute.elemental;        
}         
         
         
          
          
/* was_declared()-- Returns 0 if a symbol was not declared with a type
 * or attribute declaration statement, nonzero otherwise. */    
    
static int was_declared(g95_symbol *symbol) {      
symbol_attribute k;   
   
  k = symbol->attr;     
     
  if (!k.implicit_type && symbol->ts.type != BT_UNKNOWN) return 1;       
       
  if (k.allocatable || k.dimension || k.external || k.intrinsic ||
      k.optional || k.pointer || k.save || k.target || 
      k.access != ACCESS_UNKNOWN || k.intent != INTENT_UNKNOWN ||        
      k.proc == PROC_MODULE || k.proc == PROC_INTERNAL ||      
      k.proc == PROC_ST_FUNCTION || k.proc == PROC_DUMMY) return 1;          
          
  return 0; 
}        
        
        
  
  
/* resolve_actual_argument()-- Resolve an actual argument. */          
          
static try resolve_actual_argument(g95_expr *k) {       
g95_symbol *sy;    
    
  if (k->ts.type != BT_PROCEDURE) return g95_resolve_expr(k); 
 
  /* See if the expression node should really be a variable reference */ 
 
  sy = find_declaration(k->symbol);       
  k->symbol = sy;    
    
  if (sy->attr.flavor == FL_PROCEDURE || sy->attr.intrinsic ||       
      sy->attr.external) {          
          
    if (sy->attr.proc == PROC_UNKNOWN) {  
      sy->attr.proc = PROC_EXTERNAL;        
      if (sy->result == NULL) sy->result = sy;         
    }     
     
    return SUCCESS;         
  } 
 
  k->type = EXPR_VARIABLE;   
  k->ts = sy->ts;  
  
  if (sy->as != NULL) {  
    k->rank = sy->as->rank;    
    k->ref = g95_get_ref();   
    k->ref->type = REF_ARRAY;      
    k->ref->u.ar.type = AR_FULL;        
  }    
    
  return g95_resolve_expr(k);         
}         
         
         
   
   
/* resolve_st_function()-- Resolve a symbol that is a statement
 * function. */

static void resolve_st_function(g95_symbol *s) {  
  
  if (s->ns == g95_current_ns) return;    
    
  if (s->as != NULL) {   
    g95_error("Statement function '%s' at %L must be scalar",
	      s->name, &s->declared_at);        
    return;          
  }      
      
  move_formal_arglist(s);   
  g95_commit_symbols();  
  
  if (s->ts.type == BT_UNKNOWN) g95_set_default_type(s, 1, s->ns);  
}          
          
          


/* resolve_formal_arglist()-- Resolve types of formal argument lists.
 * These have to be done early so that the formal argument lists of
 * module procedures can be copied to the containing module before the
 * individual procedures are resolved individually.  We also resolve
 * argument lists of procedures in interface blocks because they are
 * self-contained scoping units.
 *
 * Since a dummy argument cannot be a non-dummy procedure, the only
 * resort left for untyped names are the IMPLICIT types. */        
        
static void resolve_formal_arglist(g95_symbol *proc) {    
g95_formal_arglist *t;   
g95_symbol *symb;   
   
  for(t=proc->formal; t; t=t->next) {
    symb = t->sym;  
  
    if (symb == NULL) {  /* Alternate return placeholder */   
      if (g95_elemental(proc))         
	g95_error("Alternate return specifier in elemental subroutine " 
		  "'%s' at %L is not allowed", proc->name, &proc->declared_at);      
      continue;          
    }    
    
    symb->attr.set = 1;
    if (symb->attr.if_source != IFSRC_UNKNOWN) resolve_formal_arglist(symb);

    if (symb->attr.subroutine || symb->attr.external || symb->attr.intrinsic) {       
      if (g95_pure(proc) && !g95_pure(symb)) {          
	g95_error("Dummy procedure '%s' of PURE procedure at %L must also "      
		  "be PURE", symb->name, &symb->declared_at);       
	continue;      
      }    
    
      if (g95_elemental(proc)) {
	g95_error("Dummy procedure at %L not allowed in ELEMENTAL procedure",      
		  &symb->declared_at);      
	continue;
      } 
 
      continue;      
    }  
  
    if (symb->ts.type == BT_UNKNOWN) {    
      if (!symb->attr.function || symb->result == symb)  
	g95_set_default_type(symb, 1, symb->ns); 
      else {    /* Set the type of the RESULT, then copy */        
        
	if (symb->result->ts.type == BT_UNKNOWN)  
	  g95_set_default_type(symb->result, 1, symb->result->ns);         
         
	symb->ts = symb->result->ts;  
	if (symb->as == NULL) symb->as = g95_copy_array_spec(symb->result->as); 
      }      
    }         
         
    g95_resolve_array_spec(symb->as, 0);     
     
    if (proc->attr.proc == PROC_ST_FUNCTION && symb->as != NULL)         
      g95_error("Argument '%s' of statement function at %L must be scalar",      
		symb->name, &symb->declared_at);         
         
    /* If the flavor is unknown at this point, it has to be a variable.
     * A procedure specification would have already set the type */          
          
    if (symb->attr.flavor == FL_UNKNOWN)   
      g95_add_flavor(&symb->attr, FL_VARIABLE, &symb->declared_at);      
      
    if (g95_pure(proc)) {       
      if (proc->attr.function && !symb->attr.pointer &&    
	  symb->attr.intent != INTENT_IN)        
        
	g95_error("Argument '%s' of pure function '%s' at %L must be "      
		  "INTENT(IN)", symb->name, proc->name, &symb->declared_at);          
          
      if (proc->attr.subroutine && !symb->attr.pointer && 
	  symb->attr.intent == INTENT_UNKNOWN)       
      
	g95_error("Argument '%s' of pure subroutine '%s' at %L must have " 
		  "its INTENT specified", symb->name, proc->name,   
		  &symb->declared_at);  
    }        
        
    if (g95_elemental(proc)) {   
      if (symb->as != NULL) {      
	g95_error("Argument '%s' of elemental procedure at %L must be scalar",         
		  symb->name, &symb->declared_at);  
	continue;          
      }       
       
      if (symb->attr.pointer) {         
	g95_error("Argument '%s' of elemental procedure at %L cannot have "        
		  "the POINTER attribute", symb->name, &symb->declared_at);          
	continue;         
      }   
    }          
  }   
}    
    
    
          
          
/* function_type()-- Set the expression type from the function type. */         
         
static try function_type(g95_expr *expr, g95_symbol *symb) {  
g95_typespec *typesp;      
      
  if (symb->result->ts.type != BT_UNKNOWN)         
    expr->ts = symb->result->ts;        
  else { 
    typesp = g95_get_default_type(symb->result, symb->ns);    
    
    if (typesp->type == BT_UNKNOWN) {      
      g95_error("Function '%s' at %L has no implicit type",      
		symb->name, &expr->where); 
      return FAILURE;     
    } else
      expr->ts = *typesp;       
  }          
          
  return SUCCESS;      
} 
 
 
    
    
/* resolve_specific_f0()-- Resolve a function call known to be specific */ 
 
static match resolve_specific_f0(g95_symbol *symb, g95_expr *e2) { 
match u;      
      
  if (symb->attr.external || symb->attr.if_source == IFSRC_IFBODY) {        
    if (symb->attr.dummy) {   
      symb->attr.proc = PROC_DUMMY;          
      goto found;     
    }    
    
    symb->attr.proc = PROC_EXTERNAL;
    goto found;     
  }    
    
  if (symb->attr.proc == PROC_MODULE || symb->attr.proc == PROC_INTERNAL ||        
      symb->attr.proc == PROC_ST_FUNCTION || symb->attr.if_source == IFSRC_DECL)       
    goto found;          
          
  if (symb->attr.intrinsic) { 
    u = g95_intrinsic_func_interface(e2, 1);      
    if (u == MATCH_YES) return MATCH_YES;
    if (u == MATCH_NO)      
      g95_error("Function '%s' at %L is INTRINSIC but is not compatible with "
		"an intrinsic", symb->name, &e2->where);      
            
    return MATCH_ERROR;     
  }    
    
  return MATCH_NO; 
 
found:         
  g95_procedure_use(symb, &e2->value.function.actual, &e2->where);       
       
  if (symb->result == NULL) symb->result = symb;    
    
  if (function_type(e2, symb) == FAILURE) return MATCH_ERROR;   
   
  if (mark_external(symb, 1) == FAILURE) return MATCH_ERROR;    
    
  e2->value.function.name = symb->name; 
  if (e2->symbol != symb) {         
    e2->symbol->attr.resolved = 1;        
    e2->symbol = symb;  
  }        
        
  if (symb->result->as != NULL) e2->rank = symb->result->as->rank;         
         
  return MATCH_YES;       
}


    
    
/* warn_unused_label()-- Warn about unused labels. */          
          
static void warn_unused_label(g95_namespace *names){   
g95_st_label *f;        
        
  f = names->st_labels;        
  if (f == NULL) return;       
       
  while(f->next)        
    f = f->next;     
       
  for(; f; f=f->prev) {       
    if (f->defined == ST_LABEL_UNKNOWN) continue;  
  
    switch(f->referenced){    
    case ST_LABEL_UNKNOWN:
      g95_warning("Label %d at %L defined but not used", f->value, &f->where);    
      break; 
 
    case ST_LABEL_BAD_TARGET:  
      g95_warning("Label %d at %L defined but cannot be used", f->value,      
		  &f->where);        
      break;  
  
    default:
      break;          
    }   
  }          
}  
  
  
   
   
static try resolve_specific_f(g95_expr *expr) {       
g95_symbol *symb; 
match y;        
        
  symb = expr->symbol;  
  
  do {        
    y = resolve_specific_f0(symb, expr); 
    if (y == MATCH_YES) return SUCCESS;   
    if (y == MATCH_ERROR) return FAILURE;     
     
    if (symb->ns->parent == NULL) break;
    g95_find_symbol(symb->name, symb->ns->parent, 1, &symb); 
  } while (symb != NULL);        
        
  g95_error("Unable to resolve the specific function '%s' at %L",          
	    expr->symbol->name, &expr->where);          
          
  return SUCCESS;  
}       
       
       
    
    
/* find_arglists()-- Work function called when searching for symbols
 * that have argument lists associated with them. */     
     
static void find_arglists(g95_symbol *symb) {  
  
  if (symb->attr.proc == PROC_ST_FUNCTION)       
    resolve_st_function(symb);      
  else {          
    if (symb->attr.if_source == IFSRC_UNKNOWN || symb->ns != g95_current_ns) 
      return;
  }        
        
  resolve_formal_arglist(symb);      
}          
          
          
  
  
/* variable_rank()-- Given a variable expression node, compute the
 * rank of the expression by examining the base symbol and any
 * reference structures it may have. */   
   
static void variable_rank(g95_expr *n) {          
g95_array_spec *as;   
int a, t, dim;          
g95_ref *ref;        
        
  dim = 0;        
  as = n->symbol->as; 
 
  for(ref=n->ref; ref; ref=ref->next)      
    switch(ref->type) {   
    case REF_ARRAY:
      switch(ref->u.ar.type) {       
      case AR_FULL:     
	dim = as->rank;     
	goto done;         
         
      case AR_SECTION:   
	goto section;      
      
      default:    
	break;          
      }        
        
      break;       
       
    case REF_COMPONENT:     
      as = ref->u.c.component->as;     
      break;    
    
    case REF_SUBSTRING:    
      break;          
    } 
 
  goto done;  
  
section: 
  for(a=0; a<ref->u.ar.dimen; a++) {      
    t = ref->u.ar.dimen_type[a];    
    
    if (t == DIMEN_RANGE || 
	t == DIMEN_VECTOR) dim++; 
  }        
        
done:         
  n->rank = dim;      
  expression_shape(n);     
}       
       
       
      
      
/* resolve_actual_arglist()-- Resolve an actual argument list.  Most
 * of the time, this is just resolving the expressions in the list.
 * The exception is that we sometimes have to decide whether an 
 * argument refers to a dummy procedure or a simple variable. */  
  
static try resolve_actual_arglist(g95_actual_arglist *ap) {     
     
  for(; ap; ap=ap->next) {          
    if (ap->type == ALT_RETURN || ap->u.expr == NULL) continue;          
    if (resolve_actual_argument(ap->u.expr) == FAILURE) return FAILURE;          
  }      
      
  return SUCCESS;         
}       
       
       
  
  
static void pure_subroutine(g95_code *f, g95_symbol *sy) {      
      
  if (g95_pure(sy)) return;    
    
  if (forall_flag)        
    g95_error("Subroutine call to '%s' in FORALL block at %L is not PURE",
	      f->sym->name, &f->where);      
  else if (g95_pure(NULL))    
    g95_error("Subroutine call to '%s' at %L is not PURE", f->sym->name,         
	      &f->where);          
}   
   
   


/* check_variable_usage()-- Given a symbol that is a variable, issue a
 * warning if the variable is never used, or used without being set */

static void check_variable_usage(g95_symbol *s) {  
  
  if (s->attr.flavor != FL_VARIABLE) return; 
 
  if (!s->attr.used)       
    g95_warning("Variable '%s' at %L is never used", s->name,    
		&s->declared_at);      
  else {   
    if (!s->attr.set)    
      g95_warning("Variable '%s' at %L is used but not set", s->name,         
		  &s->declared_at);      
  }        
}    
    
    
      
      
/* g95_resolve_iterator()-- Resolve the expressions in an iterator
 * structure and require that they all be of integer type. */

try g95_resolve_iterator(g95_iterator *iterator) {  
  
  if (g95_resolve_expr(iterator->var) == FAILURE) return FAILURE;    
    
  if (iterator->var->ts.type != BT_INTEGER || iterator->var->rank != 0) {         
    g95_error("Loop variable at %L must be a scalar INTEGER",
	      &iterator->var->where);          
    return FAILURE;     
  }        
        
  if (g95_pure(NULL) && g95_impure_variable(iterator->var->symbol)) {         
    g95_error("Cannot assign to loop variable in PURE procedure at %L",      
	      &iterator->var->where);       
    return FAILURE;        
  }

  if (g95_resolve_expr(iterator->start) == FAILURE) return FAILURE;  
  
  if (iterator->start->ts.type != BT_INTEGER || iterator->start->rank != 0) {         
    g95_error("Start expression in DO loop at %L must be a scalar INTEGER",    
	      &iterator->start->where);       
    return FAILURE;          
  }

  if (g95_resolve_expr(iterator->end) == FAILURE) return FAILURE;        
        
  if (iterator->end->ts.type != BT_INTEGER || iterator->end->rank != 0) {   
    g95_error("End expression in DO loop at %L must be a scalar INTEGER",    
	      &iterator->end->where);       
    return FAILURE;          
  }        
        
  if (g95_resolve_expr(iterator->step) == FAILURE) return FAILURE;

  if (iterator->step->ts.type != BT_INTEGER || iterator->step->rank != 0) {        
    g95_error("Step expression in DO loop at %L must be a scalar INTEGER",     
	      &iterator->step->where);         
    return FAILURE;     
  }

  if (iterator->step->type == EXPR_CONSTANT &&      
      mpz_cmp_ui(iterator->step->value.integer, 0) == 0) {         
    g95_error("Step expression in DO loop at %L cannot be zero",          
	      &iterator->step->where);         
    return FAILURE;   
  } 
 
  return SUCCESS;          
}      
      
      
          
          
/* resolve_common_var()-- Check the extra restrictions on variables
 * within a common block. */ 
 
static void resolve_common_var(g95_symbol *sym) {     
g95_array_spec *a;       
       
  if (!sym->attr.in_common) return;         
         
  /* Derived type names must have the SEQUENCE attribute */        
        
  if (sym->ts.type == BT_DERIVED) {         
    if (!sym->ts.derived->attr.sequence) { 
      g95_error("Derived type variable '%s' at %L must have the SEQUENCE "     
		"attribute to be in a COMMON", sym->name, &sym->declared_at);         
      return;      
    }      
      
    if (derived_init(sym)) {       
      g95_error("Derived type variable '%s' at %L cannot have a default "     
		"initialization and be in a COMMON", 
		sym->name, &sym->declared_at);      
    }
  }  
  
  a = sym->as;       
  if (a != NULL) {
    if (g95_resolve_array_spec(a, 1) == FAILURE) return;      
      
    if (sym->attr.pointer && a->type == AS_EXPLICIT) {  
      g95_error("POINTER array specfication of '%s' in COMMON at %L cannot "         
		"be explicit", sym->name, &sym->declared_at);        
      return;      
    }    
  }     
}        
        
        
    
    
/* check_dimension()-- Compare a single dimension of an array
 * reference to the array specification. */     
     
static try check_dimension(int p, g95_array_ref *spec, g95_array_spec *ref) {      
      
/* Given start, end and stride values, calculate the minimum and
 * maximum referenced indexes. */       
       
  switch(spec->type) {     
  case AR_FULL: 
    break;         
         
  case AR_ELEMENT:   
    if (compare_bound(spec->start[p], ref->lower[p]) == CMP_LT) goto bound;       
    if (compare_bound(spec->start[p], ref->upper[p]) == CMP_GT) goto bound;     
     
    break;    
    
  case AR_SECTION:     
    if (compare_bound_int(spec->stride[p], 0) == CMP_EQ) {          
      g95_error("Illegal stride of zero at %L", &spec->c_where[p]);        
      return FAILURE;
    }    
    
    /* It is perfectly legal for all section bounds to exceed the
     * range of the array */      
      
    break;   
   
  default:      
    g95_internal_error("check_dimension(): Bad array reference");    
  }         
         
  return SUCCESS;  
  
bound:
  g95_warning("Array reference at %L is out of bounds", &spec->c_where[p]); 
  return SUCCESS;   
}      
      
      
     
     
/* resolve_structure_cons()-- Resolve all of the elements of a
 * structure constructor and make sure that the types are correct. */          
          
static try resolve_structure_cons(g95_expr *e2) { 
g95_constructor *cons;   
g95_component *comp;        
locus w;   
   
  cons = e2->value.constructor;
  comp = e2->symbol->components;      
  w = e2->where;          
          
  for(; comp; comp=comp->next, cons=cons->next) {  
    if (cons == NULL) {       
      g95_error("Not enough values in structure constructor at %L", &w);    
      return FAILURE;         
    }   
   
    w = cons->expr->where; 
 
    if (g95_resolve_expr(cons->expr) == FAILURE) return FAILURE;       
       
    /* If we don't have the right type, try to convert it. */         
         
    if (!g95_compare_types(&cons->expr->ts, &comp->ts) &&         
	g95_convert_type(cons->expr, &comp->ts, 1) == FAILURE)  
      return FAILURE;       
  } 
 
  if (cons != NULL) {
    g95_error("Too many values in structure constructor at %L",         
	      &cons->expr->where);     
    return FAILURE;          
  }         
         
  return SUCCESS;     
} 
 
 
          
          
/* resolve_unknown_f()-- Resolve a procedure call not known to be
 * generic nor specific */          
          
static try resolve_unknown_f(g95_expr *e2) {        
g95_symbol *sy;   
   
  sy = e2->symbol;         
        
  if (sy->attr.dummy) {          
    sy->attr.proc = PROC_DUMMY;  
    e2->value.function.name = sy->name;
    goto set_type;  
  }       
       
  /* See if we have an intrinsic function reference */         
         
  if (g95_intrinsic_name(sy->name, 0)) {      
    if (g95_intrinsic_func_interface(e2, 1) == MATCH_YES) return SUCCESS;  
    return FAILURE;  
  }   
   
  /* The reference is to an external name */   
   
  sy->attr.proc = PROC_EXTERNAL;  
  e2->value.function.name = sy->name;        
        
  if (e2->symbol != sy) {         
    e2->symbol->attr.resolved = 1; 
    e2->symbol = sy;   
  }    
    
  if (sy->result->as != NULL) e2->rank = sy->result->as->rank;        
        
  g95_procedure_use(sy, &e2->value.function.actual, &e2->where);   
   
  /* Type of the expression is either the type of the symbol or the
   * default type of the symbol */      
        
 set_type:        
  return function_type(e2, sy);        
}      
      
      
   
   
/* specific_sym()-- Determine if a symbol is specific or not */

static int specific_sym(g95_symbol *sym) {         
g95_symbol *i;

  if (sym->attr.if_source == IFSRC_IFBODY || sym->attr.proc == PROC_MODULE ||
      sym->attr.if_source == IFSRC_DECL || sym->attr.proc == PROC_INTERNAL ||   
      sym->attr.proc == PROC_ST_FUNCTION || sym->attr.external || 
      (sym->attr.intrinsic && g95_specific_intrinsic(sym->name)))  
    return 1;  
  
  if (was_declared(sym) || sym->ns->parent == NULL) return 0;    
    
  g95_find_symbol(sym->name, sym->ns->parent, 1, &i); 
 
  return (i == NULL) ? 0 : specific_sym(i);
}  
  
  
       
       
static try resolve_generic_f(g95_expr *exp) {  
g95_symbol *symb;         
match u;         
         
  symb = exp->symbol;       
       
  for(;;) {   
    u = resolve_generic_f0(exp, symb); 
    if (u == MATCH_YES) return SUCCESS;     
    if (u == MATCH_ERROR) return FAILURE;       
       
  generic: 
    if (symb->ns->parent == NULL) break;         
    g95_find_symbol(symb->name, symb->ns->parent, 1, &symb);     
     
    if (symb == NULL) break;       
    if (!generic_sym(symb)) goto generic;          
  }       
       
  /* Last ditch attempt */ 
 
  if (!g95_generic_intrinsic(exp->symbol->name)) {  
    g95_error("Generic function '%s' at %L is not an intrinsic function",         
	      exp->symbol->name, &exp->where);        
    return FAILURE;         
  }  
  
  u = g95_intrinsic_func_interface(exp, 0);   
  if (u == MATCH_YES) return SUCCESS;       
  if (u == MATCH_NO) 
    g95_error("Generic function '%s' at %L is not consistent with a specific "          
	      "intrinsic interface", exp->symbol->name, &exp->where);        
        
  return FAILURE;  
}


      
      
/* resolve_substring()-- Resolve a substring reference */    
    
static try resolve_substring(g95_ref *reference) {  
  
  if (reference->u.ss.start != NULL) {      
    if (g95_resolve_expr(reference->u.ss.start) == FAILURE) return FAILURE;          
          
    if (reference->u.ss.start->ts.type != BT_INTEGER) {       
      g95_error("Substring start index at %L must be of type INTEGER",        
		&reference->u.ss.start->where);
      return FAILURE;          
    }         
         
    if (reference->u.ss.start->rank != 0) {      
      g95_error("Substring start index at %L must be scalar",       
		&reference->u.ss.start->where);        
      return FAILURE;        
    }         
         
    if (compare_bound_int(reference->u.ss.start, 1) == CMP_LT) {   
      g95_error("Substring start index at %L is less than one",        
		&reference->u.ss.start->where);         
      return FAILURE; 
    }      
  }   
   
  if (reference->u.ss.end != NULL) {          
    if (g95_resolve_expr(reference->u.ss.end) == FAILURE) return FAILURE;          
          
    if (reference->u.ss.end->ts.type != BT_INTEGER) {  
      g95_error("Substring end index at %L must be of type INTEGER",       
		&reference->u.ss.end->where);       
      return FAILURE;       
    }          
          
    if (reference->u.ss.end->rank != 0) {       
      g95_error("Substring end index at %L must be scalar",         
		&reference->u.ss.end->where);    
      return FAILURE;       
    }        
        
    if (reference->u.ss.length != NULL &&   
	compare_bound(reference->u.ss.end, reference->u.ss.length->length) == CMP_GT) {          
      g95_error("Substring end index at %L is out of bounds",          
		&reference->u.ss.start->where);          
      return FAILURE;      
    }         
  }

  return SUCCESS;       
}      
      
      
    
    
/* compare_spec_to_ref()-- Compare an array reference with an
 * array specification. */      
      
static try compare_spec_to_ref(g95_array_ref *a, g95_array_spec *spec,        
			       locus *loc) {  
int h; 
 
  if (a->type == AR_FULL) return SUCCESS;   
   
  if (spec->rank != a->dimen) {   
    g95_error("Rank mismatch in array reference at %L (%d/%d)",     
	      loc, a->dimen, spec->rank);  
    return FAILURE;       
  }         
         
  for(h=0; h<spec->rank; h++)   
    if (check_dimension(h, a, spec) == FAILURE) return FAILURE; 
 
  return SUCCESS;          
}    
    
    
   
   
static match resolve_generic_s0(g95_code *i, g95_symbol *sym) {
g95_symbol *a;   
   
  if (sym->attr.generic) {        
    a = g95_search_interface(sym->generic, 1, &i->ext.actual);        
    if (a != NULL) {        
      if (a->attr.proc == PROC_UNKNOWN) a->attr.proc = PROC_EXTERNAL;   
   
      i->sym = a;        
      pure_subroutine(i, a);     
      return MATCH_YES;         
    }  
  
    /* TODO: Need to search for elemental references in generic interface */         
  }  
  
  if (sym->attr.intrinsic) return g95_intrinsic_sub_interface(i, 0);        
        
  return MATCH_NO;         
}         
         
         
   
   
/* generic_symbol()-- Determine if a symbol is generic or not */         
         
static int generic_sym(g95_symbol *sy) {    
g95_symbol *u;         
         
  if (sy->attr.generic ||        
      (sy->attr.intrinsic && g95_generic_intrinsic(sy->name))) 
    return 1;         
         
  if (was_declared(sy) || sy->ns->parent == NULL) return 0; 
 
  g95_find_symbol(sy->name, sy->ns->parent, 1, &u);        
        
  return (u == NULL) ? 0 : generic_sym(u);          
}    
    
    


/* resolve_formal_arglists()-- Given a namespace, resolve all formal
 * argument lists within the namespace. */         
         
static void resolve_formal_arglists(g95_namespace *namesp) {  
  
  if (namesp == NULL) return;      
      
  g95_traverse_ns(namesp, find_arglists);      
}  
  
  
 
 
/* resolve_data_variables()-- Resolve the expressions and iterators
 * associated with a data statement.  This is separate from the
 * assignment checking because data lists only should be resolved
 * once. */       
       
static try resolve_data_variables(g95_data_variable *v) {  
  
  for(; v; v=v->next) {          
    if (v->list == NULL) { 
      if (g95_resolve_expr(v->expr) == FAILURE) return FAILURE;          
    } else {     
      if (g95_resolve_iterator(&v->iter) == FAILURE) return FAILURE;     
     
      if (resolve_data_variables(v->list) == FAILURE) return FAILURE;          
    }  
  }          
          
  return SUCCESS;   
}


  
  
/* resolve_deallocate_expr()-- Resolve the argument of a deallocate
 * expression.  The expression must be a pointer or a full array. */   
   
static try resolve_deallocate_expr(g95_expr *m) {      
symbol_attribute atr;  
int allocatable;      
g95_ref *r;     
     
  if (g95_resolve_expr(m) == FAILURE) return FAILURE;   
   
  atr = g95_expr_attr(m);   
  if (atr.pointer) return SUCCESS;      
      
  if (m->type != EXPR_VARIABLE) goto bad;         
         
  allocatable = m->symbol->attr.allocatable;   
  for(r=m->ref; r; r=r->next) 
    switch(r->type) {         
    case REF_ARRAY:        
      if (r->u.ar.type != AR_FULL) allocatable = 0;         
      break;

    case REF_COMPONENT:          
      allocatable = (r->u.c.component->as != NULL &&   
		     r->u.c.component->as->type == AS_DEFERRED); 
      break;      
      
    case REF_SUBSTRING:      
      allocatable = 0;       
      break;        
    }        
        
  if (allocatable == 0) {  
  bad: 
    g95_error("Expression in DEALLOCATE statement at %L must be "  
	      "ALLOCATABLE or a POINTER", &m->where);       
  }      
      
  return SUCCESS;      
}         
         
         
    
    
/* resolve_branch()-- Given a branch to a label and a namespace, see
 * if the branch is conforming.  The code node describes where the
 * branch is located. */  
  
static try resolve_branch(g95_st_label *label, g95_code *codep) {  
g95_code *block, *found;      
code_stack *stack;
g95_st_label *lp;  
  
  if (label == NULL) return SUCCESS;   
  lp = label;  
  
  /* Step one: is this a valid branching target? */    
    
  if (lp->defined == ST_LABEL_UNKNOWN) {      
    g95_error("Label %d referenced at %L is never defined", lp->value,
	      &lp->where);     
    return FAILURE;          
  }        
        
  if (lp->defined != ST_LABEL_TARGET) {  
    g95_error("Statement at %L is not a valid branch target statement " 
	      "for the branch statement at %L", &lp->where, &codep->where);   
    return FAILURE;       
  }       
       
  /* Step two: make sure this branch is not a branch to itself ;-) */ 
 
  if (codep->here == label) {  
    g95_warning("Branch at %L causes an infinite loop", &codep->where);       
    return SUCCESS;       
  }          
          
  /* Step three: Try to find the label in the parse tree. To do this,
   * we traverse the tree block-by-block: first the block that
   * contains this GOTO, then the block that it is nested in, etc.  We
   * can ignore other blocks because branching into another block is
   * not allowed. */       
       
  found = NULL;          
          
  for(stack=cs_base; stack; stack=stack->prev) {     
    for(block=stack->head; block; block=block->next) {
      if (block->here == label) {        
        found = block;        
        break;      
      }      
    }   
   
    if (found) break;        
  }          
          
  if (found == NULL) {    /* still nothing, so illegal.  */        
    g95_error_now("Label at %L is not in the same block as the "          
                  "GOTO statement at %L", &lp->where, &codep->where);        
    return FAILURE;   
  }   
   
  /* Step four: Make sure that the branching target is legal if
   * the statement is an END {SELECT,DO,IF}. */   
   
  if (found->type == EXEC_NOP && found->ext.end_code != ST_ENDDO) {        
    for(stack=cs_base; stack; stack=stack->prev) 
      if (stack->current->next == found) break;         
         
    if (stack == NULL) {   
      g95_error("GOTO at %L cannot jump to END of construct at %L",    
                &found->where, &codep->where);      
      return FAILURE;  
    } 
  }      
      
  return SUCCESS;     
}   
  
  
         
         
/* resolve_specific_s0()-- Resolve a subroutine call known to be specific */ 
 
static match resolve_specific_s0(g95_code *e, g95_symbol *s) {       
match t;         
         
  if (s->attr.external || s->attr.if_source == IFSRC_IFBODY) {         
    if (s->attr.dummy) {         
      s->attr.proc = PROC_DUMMY;       
      goto found;       
    }    
    
    s->attr.proc = PROC_EXTERNAL;         
    goto found;         
  }          
          
  if (s->attr.proc == PROC_MODULE || s->attr.proc == PROC_INTERNAL ||     
      s->attr.if_source == IFSRC_DECL) 
    goto found; 
 
  if (s->attr.intrinsic) {          
    t = g95_intrinsic_sub_interface(e, 1);     
    if (t == MATCH_YES) return MATCH_YES;         
    if (t == MATCH_NO)          
      g95_error("Subroutine '%s' at %L is INTRINSIC but is not compatible "        
		"with an intrinsic", s->name, &e->where);        
        
    return MATCH_ERROR; 
  } 
 
  return MATCH_NO; 
 
found: 
  g95_procedure_use(s, &e->ext.actual, &e->where);      
      
  mark_external(s, 0);     
     
  e->sym = s;          
  pure_subroutine(e, s);      
      
  return MATCH_YES;          
}       
       
       
          
          
/* pure_function()-- Figure out if if a function reference is pure or
 * not.  Also sets the name of the function for a potential error
 * message.  Returns nonzero if the function is PURE, zero if not. */         
         
static int pure_function(g95_expr *o, char **nam) {  
int pure;    
    
  if (o->value.function.isym) {         
    pure = o->value.function.isym->pure || o->value.function.isym->elemental;        
    *nam = o->value.function.isym->name;          
  } else {      
    pure = g95_pure(o->symbol);         
    *nam = o->symbol->name;  
  }   
   
  return pure;  
}    
    
    
          
          
/* resolve_symbol()-- Do anything necessary to resolve a symbol.
 * Right now, we just assume that an otherwise unknown symbol is a
 * variable.  This sort of thing commonly happens for symbols in
 * module. */   
   
static try resolve_symbol(g95_symbol *s) {         
static int formal_ns_flag = 1; /* Zero if we are checking a formal namespace */        
int formal_ns_save, check_constant, mp_flag;        
        
  if (s->attr.resolved) return SUCCESS; 
  s->attr.resolved = 1;   
   
  if (s->attr.flavor == FL_UNKNOWN) {
    if (s->attr.external == 0 && s->attr.intrinsic == 0)         
      s->attr.flavor = FL_VARIABLE;    
    else {        
      s->attr.flavor = FL_PROCEDURE;         
      if (s->attr.dimension) s->attr.function = 1;       
    }          
  }      
      
  if (s->attr.flavor == FL_DERIVED && resolve_derived(s) == FAILURE)      
    return FAILURE;        
        
  /* Symbols that are module procedures with results (functions) have
   * the types and array specification copied for type checking in
   * procedures that call them, as well as for saving to a module
   * file.  These symbols can't stand the scrutiny that their results
   * can. */ 
 
  mp_flag = s->result != NULL && s->result != s;

  /* Assign default type to symbols that need one and don't have one */         
         
  if (s->ts.type == BT_UNKNOWN) {    
    if (s->attr.flavor == FL_VARIABLE || s->attr.flavor == FL_PARAMETER)        
      g95_set_default_type(s, 1, NULL);    
    
    if (s->attr.flavor == FL_PROCEDURE && s->attr.function) {  
      if (!mp_flag)    
	g95_set_default_type(s, 0, NULL);    
      else { 
	if (resolve_symbol(s->result) == FAILURE)  
	  return FAILURE;  /* Result may be in another namespace */    
    
	s->ts = s->result->ts;          
	s->as = g95_copy_array_spec(s->result->as);   
	s->attr.pointer = s->result->attr.pointer;     
	s->attr.dimension = s->result->attr.dimension;          
      } 
    }        
  }

  if (s->as != NULL && (s->as->type == AS_ASSUMED_SIZE ||       
			  s->as->type == AS_ASSUMED_SHAPE ) &&      
      s->attr.dummy == 0) {        
    g95_error("Assumed %s array at %L must be a dummy argument",        
	      s->as->type == AS_ASSUMED_SIZE ? "size" : "shape",   
	      &s->declared_at);     
    return FAILURE;
  }

  /* Mark variables with initialization expressions as having been set */        
        
  if (s->attr.flavor == FL_VARIABLE && s->value != NULL) s->attr.set = 1;        
        
  /* Make sure that character string variables with assumed lengths are
   * dummy arguments or results. */         
         
  if (s->attr.flavor == FL_VARIABLE && s->ts.type == BT_CHARACTER &&        
      s->ts.cl->length == NULL && !s->attr.dummy && !s->attr.result_var){       
    g95_error("Assumed character length variable '%s' at %L must be a "
	      "dummy variable or result", s->name, &s->declared_at);  
    return FAILURE;       
  }        
        
  /* Make sure a parameter that has been implicitly typed still
   * matches the implicit type, since PARAMETER statements can precede
   * IMPLICIT statements. */ 
 
  if (s->attr.flavor == FL_PARAMETER &&       
      g95_check_parameter(s) == FAILURE) return FAILURE;         
         
  /* Make sure the types of derived parameters are consistent.  This
   * type checking is deferred until resolution because the type may
   * refer to a derived type from the host. */         
         
  if (s->attr.flavor == FL_PARAMETER && s->ts.type == BT_DERIVED &&          
      !g95_compare_types(&s->ts, &s->value->ts)) {         
    g95_error("Incompatible derived type in PARAMETER at %L",       
	      &s->value->where);       
    return FAILURE;
  }     
     
  /* Make sure symbols with known intent or optional are really dummy
   * variable.  Because of ENTRY statement, this has to be deferred
   * until resolution time. */       
       
  if ((s->attr.optional || s->attr.intent != INTENT_UNKNOWN) &&       
      s->attr.dummy == 0) {        
        
    g95_error("Symbol at %L is not a DUMMY variable", &s->declared_at);         
    return FAILURE;      
  }          
          
  /* Constraints on deferred shape variables. */      
      
  if (s->attr.flavor == FL_VARIABLE ||       
      (s->attr.flavor == FL_PROCEDURE && s->attr.function)) {    
    if (s->as == NULL || s->as->type != AS_DEFERRED) {  
      if (s->attr.allocatable) {       
	g95_error("Allocatable array at %L must have a deferred shape",         
		  &s->declared_at);   
	return FAILURE;         
      }    
    
      if (s->attr.pointer && s->attr.dimension) {
	g95_error("Pointer to array at %L must have a deferred shape",  
		  &s->declared_at);       
	return FAILURE;   
      }          
          
    } else { 
      if (!mp_flag && !s->attr.allocatable && !s->attr.pointer &&         
	  !s->attr.dummy) {   
	g95_error("Array at %L cannot have a deferred shape",       
		  &s->declared_at);     
	return FAILURE;         
      } 
    } 
  }       
       
  /* Make sure that an intrinsic exists */          
          
  if (s->attr.intrinsic && !g95_intrinsic_name(s->name, 0)         
      && !g95_intrinsic_name(s->name, 1)) {     
    g95_error("Intrinsic at %L does not exist", &s->declared_at);    
    return FAILURE;      
  }
      
  /* Resolve array specification.  If the array is a non-dummy
   * parameter that lives in static memory, then the specification
   * must be constant. */  
  
  check_constant = !s->attr.pointer &&    
    (s->attr.in_common || s->attr.save || s->attr.data ||          
     s->value != NULL);     
     
  g95_resolve_array_spec(s->as, check_constant);     
     
  /* Resolve formal namespaces. */         
         
  if (formal_ns_flag && s != NULL && s->formal_ns != NULL) {        
    formal_ns_save = formal_ns_flag;    
    formal_ns_flag = 0;
    if (g95_resolve(s->formal_ns) == FAILURE) return FAILURE;    
    formal_ns_flag = formal_ns_save;        
  }        
        
  return SUCCESS;   
}    
    
    
     
     
/* resolve_forall_iterators()-- Resolve a list of FORALL iterators */        
        
static try resolve_forall_iterators(g95_forall_iterator *iterator) {     
     
  for(; iterator; iterator=iterator->next) {  
    if (g95_resolve_expr(iterator->var) == FAILURE) return FAILURE; 
 
    if (iterator->var->ts.type != BT_INTEGER) {      
      g95_error("FORALL Iteration variable at %L must be INTEGER",
		&iterator->var->where);       
      return FAILURE;         
    }     
     
    if (g95_resolve_expr(iterator->start) == FAILURE) return FAILURE;  
  
    if (iterator->start->ts.type != BT_INTEGER) { 
      g95_error("FORALL start expression at %L must be INTEGER", 
		&iterator->start->where);        
      return FAILURE;  
    }     
     
    if (g95_resolve_expr(iterator->end) == FAILURE) return FAILURE;       
       
    if (iterator->end->ts.type != BT_INTEGER) {         
      g95_error("FORALL end expression at %L must be INTEGER",    
		&iterator->end->where);          
      return FAILURE;       
    }      
      
    if (g95_resolve_expr(iterator->stride) == FAILURE) return FAILURE;   
   
    if (iterator->stride->ts.type != BT_INTEGER) {    
      g95_error("FORALL Stride expression at %L must be INTEGER",   
		&iterator->stride->where);
      return FAILURE;         
    }        
  }   
   
  return SUCCESS;     
}      
      
      
        
        
/* resolve_unknown_expr()-- Unknown expressions are single symbol
 * actual arguments.  Figure out what the symbol is, then what the
 * expression type from that. */

static try resolve_unknown_expr(g95_expr *u) {          
g95_symbol *s;          
          
  s = find_declaration(u->symbol);   
   
  if (resolve_symbol(s) == FAILURE) return FAILURE;         
  u->symbol = s;      
  u->type = EXPR_VARIABLE;

  switch(s->attr.flavor) {
  case FL_VARIABLE: 
    u->ts = s->ts;  
    break; 
 
  case FL_PROCEDURE:         
    u->ts.type = BT_PROCEDURE;
    break;       
       
  default:
    g95_internal_error("resolve_unknown_expr(): bad flavor");   
  }

  return SUCCESS;   
}     
     
     
          
          
/* convert_substring()-- Convert an (incorrect) array reference into a
 * substring reference and resolve the node. */      
      
static try convert_substring(g95_ref *re, g95_charlen *clen) {   
g95_expr *st, *stop;     
     
  st = re->u.ar.start[0];         
  stop   = re->u.ar.end[0];   
   
  if (re->u.ar.dimen > 1 || re->u.ar.dimen_type[0] != DIMEN_RANGE ||         
      re->u.ar.stride[0] != NULL) {         
    g95_error("Syntax error in substring reference at %L",   
	      &re->where);     
    return FAILURE; 
  }      
      
  re->type = REF_SUBSTRING;      
  re->u.ss.start = st;       
  re->u.ss.end = stop;       
  re->u.ss.length = clen;        
        
  return resolve_substring(re);       
}


      
      
/* g95_pure()-- Test whether a symbol is pure or not.  For a NULL
 * pointer, checks the symbol of the current procedure. */     
     
int g95_pure(g95_symbol *s) {          
symbol_attribute a;        
        
  if (s == NULL) s = g95_current_ns->proc_name;     
  if (s == NULL) return 0;       
       
  a = s->attr;        
        
  return a.flavor == FL_PROCEDURE && (a.pure || a.elemental);         
}   
   
   
     
     
static try resolve_generic_s(g95_code *g) { 
g95_symbol *s;          
match a;    
    
  s = g->sym; 
 
  a = resolve_generic_s0(g, s);         
  if (a == MATCH_YES) return SUCCESS;
  if (a == MATCH_ERROR) return FAILURE;      
      
  if (s->ns->parent != NULL) {  
    g95_find_symbol(s->name, s->ns->parent, 1, &s);  
    if (s != NULL) {   
      a = resolve_generic_s0(g, s); 
      if (a == MATCH_YES) return SUCCESS;
      if (a == MATCH_ERROR) return FAILURE;         
    }       
  } 
 
  /* Last ditch attempt */          
          
  if (!g95_generic_intrinsic(s->name)) {
    g95_error("Generic subroutine '%s' at %L is not an intrinsic subroutine",         
	      s->name, &g->where);        
    return FAILURE;         
  }

  a = g95_intrinsic_sub_interface(g, 0); 
  if (a == MATCH_YES) return SUCCESS;        
  if (a == MATCH_NO)   
    g95_error("Generic subroutine '%s' at %L is not consistent with an "      
	      "intrinsic subroutine interface", s->name, &g->where);     
     
  return FAILURE;    
}       
       
       
        
        
/* resolve_data()-- Resolve a single DATA statement. */

static try resolve_data(g95_data *q) {   
g95_data_value *val;        
        
  if (resolve_data_variables(q->var) == FAILURE) return FAILURE;

  for(val=q->value; val; val=val->next)      
    if (g95_resolve_expr(val->expr) == FAILURE) return FAILURE;     
     
  return SUCCESS;
}       
       
       
   
   
static try resolve_specific_s(g95_code *t) {        
g95_symbol *symbol;         
match v;     
     
  symbol = t->sym;      
      
  v = resolve_specific_s0(t, symbol);  
  if (v == MATCH_YES) return SUCCESS;      
  if (v == MATCH_ERROR) return FAILURE;     
     
  g95_find_symbol(symbol->name, symbol->ns->parent, 1, &symbol);   
   
  if (symbol != NULL) {      
    v = resolve_specific_s0(t, symbol);          
    if (v == MATCH_YES) return SUCCESS;          
    if (v == MATCH_ERROR) return FAILURE;   
  }    
    
  g95_error("Unable to resolve the specific subroutine '%s' at %L",  
	    symbol->name, &t->where);      
      
  return FAILURE;     
} 
 
 
    
    
/* procedure_kind()-- Figure out if the procedure is specific, generic
 * or unknown. */       
       
static proc_type procedure_kind(g95_symbol *symbol) { 
 
  if (generic_sym(symbol))  return PTYPE_GENERIC;        
  if (specific_sym(symbol)) return PTYPE_SPECIFIC;          
  return PTYPE_UNKNOWN;       
}


          
          
/* resolve_function()-- Resolve a function call, which means resolving
 * the arguments, then figuring out which entity the name refers to.
 * TODO: Check procedure arguments so that an INTENT(IN) isn't passed
 * to INTENT(OUT) or INTENT(INOUT).  */ 
 
static try resolve_function(g95_expr *e1) {    
g95_actual_arglist *a;   
char *name0;       
try s;         
         
  if (resolve_actual_arglist(e1->value.function.actual) == FAILURE)        
    return FAILURE;      
      
/* See if function is already resolved */

  if (e1->value.function.isym != NULL) {  
    if (e1->ts.type == BT_UNKNOWN) 
      g95_internal_error("resolve_function(): untyped intrinsic");          
          
    s = SUCCESS;       
  } else {     /* Apply the rules of section 14.1.2.4 */          
          
    switch(procedure_kind(e1->symbol)) {          
    case PTYPE_GENERIC:      
      s = resolve_generic_f(e1); 
      break;          
          
    case PTYPE_SPECIFIC:     
      s = resolve_specific_f(e1);          
      break;    
    
    case PTYPE_UNKNOWN:         
      s = resolve_unknown_f(e1);  
      break;  
  
    default: 
      g95_internal_error("resolve_function(): bad function type");    
    }   
  } 
 
  /* If the expression is still a function (it might have simplified),
   * then we check to see if we are calling an elemental function */

  if (e1->type != EXPR_FUNCTION) return s;         
         
  if (e1->value.function.actual != NULL && 
      ((e1->symbol != NULL && e1->symbol->attr.elemental) ||  
       (e1->value.function.isym != NULL &&        
	e1->value.function.isym->elemental))) { 
 
    /* The rank of an elemental is the rank of its array argument(s) */         
         
    for(a=e1->value.function.actual; a; a=a->next) {        
      if (a->type != ALT_RETURN && a->u.expr != NULL &&    
	  a->u.expr->rank > 0) {
	e1->rank = a->u.expr->rank;         
	break; 
      }          
    }         
  }          
          
  if (!pure_function(e1, &name0)) {         
    if (forall_flag) {     
      g95_error("Function reference to '%s' at %L is inside a FORALL block",       
		name0, &e1->where);      
      s = FAILURE;   
    } else if (g95_pure(NULL)) {         
      g95_error("Function reference to '%s' at %L is to a non-PURE "   
		"procedure within a PURE procedure", name0, &e1->where);
      s = FAILURE; 
    }          
  }   
   
  return s;       
}      
      
      


/* resolve_unknown_s()-- Resolve a subroutine call not known to be
 * generic nor specific */   
   
static try resolve_unknown_s(g95_code *c) { 
g95_symbol *sym;     
     
  sym = c->sym;   
   
  if (sym->attr.dummy) {          
    sym->attr.proc = PROC_DUMMY;   
    c->sym = sym;       
    return SUCCESS;          
  }        
        
  /* See if we have an intrinsic function reference */

  if (g95_intrinsic_name(sym->name, 1)) {
    if (g95_intrinsic_sub_interface(c, 1) == MATCH_YES) return SUCCESS;          
    return FAILURE;         
  }    
    
  /* The reference is to an external name */    
    
  if (sym->attr.proc == PROC_UNKNOWN) sym->attr.proc = PROC_EXTERNAL;         
         
  g95_procedure_use(sym, &c->ext.actual, &c->where);      
  c->sym = sym;         
         
  pure_subroutine(c, sym);        
        
  return SUCCESS;  
}       
       
       
 
 
/* resolve_symbols()-- Recursively resolve all symbol in a namespace */        
        
static try resolve_symbols(g95_symtree *st1) {       
       
  if (st1 == NULL) return SUCCESS;        
        
  if (resolve_symbols(st1->left) == FAILURE ||     
      resolve_symbols(st1->right) == FAILURE) return FAILURE;        
        
  return resolve_symbol(st1->n.sym);      
}        
        
        
 
 
/* in_common_block()-- Return nonzero if a symbol is within a
 * particular common block.  The 'common' variable here is the value
 * of the pointer to the head of the common list, not the symbol of
 * the common variable.  This allows this subroutine to be used with
 * the blank common. */  
  
static int in_common_block(g95_symbol *common, g95_symbol *s) {      
      
  for(; common!=NULL; common=common->common_next)
    if (common == s) return 1;

  return 0;   
}         
         
         
   
   
/* resolve_array_ref()-- Resolve an array reference */ 
 
static try resolve_array_ref(g95_array_ref *ar, g95_array_spec *spec,          
			     locus *pos) {    
int y, check_scalar;

  for(y=0; y<ar->dimen; y++) {      
    check_scalar = ar->dimen_type[y] == DIMEN_RANGE;   
   
    if (resolve_index(ar->start[y],  check_scalar) == FAILURE) return FAILURE;     
    if (resolve_index(ar->end[y],    check_scalar) == FAILURE) return FAILURE;         
    if (resolve_index(ar->stride[y], check_scalar) == FAILURE) return FAILURE;     
     
    if (ar->dimen_type[y] == DIMEN_UNKNOWN)      
      switch(ar->start[y]->rank) {     
      case 0:    
	ar->dimen_type[y] = DIMEN_ELEMENT; 
	break;     
     
      case 1:      
	ar->dimen_type[y] = DIMEN_VECTOR;        
	break;

      default:        
	g95_error("Array index at %L is an array of rank %d", &ar->c_where[y],     
		  ar->start[y]->rank);  
	return FAILURE;      
      }
  }       
       
  /* If the reference type is unknown, figure out what kind it is */      
      
  if (ar->type == AR_UNKNOWN) {         
    ar->type = AR_ELEMENT;     
    for(y=0; y<ar->dimen; y++) 
      if (ar->dimen_type[y] == DIMEN_RANGE ||          
	  ar->dimen_type[y] == DIMEN_VECTOR) {   
	ar->type = AR_SECTION;         
	break;          
      } 
  } 
 
  if (compare_spec_to_ref(ar, spec, pos) == FAILURE) return FAILURE; 
 
  return SUCCESS;          
}


         
         
/* resolve_allocate_expr()-- Resolve the expression in an ALLOCATE
 * statement, doing the additional checks to see whether the
 * expression is OK or not.  The expression must have a trailing array
 * reference that gives the size of the array. */        
        
static try resolve_allocate_expr(g95_expr *d) {   
int y, pointer, allocatable, dimension;  
symbol_attribute atr;
g95_ref *ref, *ref2;   
g95_array_ref *a;         
         
  if (g95_resolve_expr(d) == FAILURE) return FAILURE;    
    
  /* Make sure the expression is allocatable or a pointer.  If it is
   * pointer, the next-to-last reference must be a pointer. */

  ref2 = NULL;       
       
  if (d->type != EXPR_VARIABLE) {         
    allocatable = 0;         
         
    atr = g95_expr_attr(d);    
    pointer = atr.pointer;      
    dimension = atr.dimension;

  } else {       
    allocatable = d->symbol->attr.allocatable; 
    pointer = d->symbol->attr.pointer;         
    dimension = d->symbol->attr.dimension;        
        
    for(ref=d->ref; ref; ref2=ref, ref=ref->next)     
      switch(ref->type) {    
      case REF_ARRAY:       
	if (ref->next != NULL) pointer = 0; 
       	break;        
        
      case REF_COMPONENT:
	allocatable = (ref->u.c.component->as != NULL &&  
		       ref->u.c.component->as->type == AS_DEFERRED);  
  
	pointer = ref->u.c.component->pointer;     
	dimension = ref->u.c.component->dimension;    
	break;         
         
      case REF_SUBSTRING:         
	allocatable = 0;
	pointer = 0;
	break;        
      }          
  }         
         
  if (allocatable == 0 && pointer == 0) {        
    g95_error("Expression in ALLOCATE statement at %L must be "        
	      "ALLOCATABLE or a POINTER", &d->where);    
    return FAILURE;
  }          
          
  if (pointer && dimension == 0) return SUCCESS;         
         
  /* Make sure the next-to-last reference node is an array specification. */       
       
  if (ref2 == NULL || ref2->type != REF_ARRAY || ref2->u.ar.type == AR_FULL) {          
    g95_error("Array specification required in ALLOCATE statement "   
	      "at %L", &d->where); 
    return FAILURE;          
  }

  if (ref2->u.ar.type == AR_ELEMENT) return SUCCESS;        
        
  /* Make sure that the array section reference makes sense in the
   * context of an ALLOCATE specification. */         
         
  a = &ref2->u.ar;   
   
  for(y=0; y<a->dimen; y++)      
    switch(a->dimen_type[y]) {        
    case DIMEN_ELEMENT:   
      break;      
      
    case DIMEN_RANGE:    
      if (a->start[y] != NULL && a->end[y] != NULL &&        
	  a->stride[y] == NULL) break;          
          
      /* Fall Through */   
   
    case DIMEN_UNKNOWN:         
    case DIMEN_VECTOR:         
      g95_error("Bad array specification in ALLOCATE statement at %L",   
		&d->where);
      return FAILURE;     
    }    
    
  return SUCCESS;     
}     
     
     
         
         
/* resolve_constant()-- Constants with reference structures can be
 * created by parameter substitution.  The only legal cases here are a
 * string constant followed by a substring reference or a parameter
 * array. */       
       
static try resolve_constant(g95_expr *a) { 
g95_ref *reference;          
          
  reference = a->ref;      
  if (reference == NULL) return SUCCESS;     
     
  if (a->ts.type != BT_CHARACTER ||       
      reference->type != REF_ARRAY || reference->next != NULL) {      
    g95_error("Syntax error in variable reference at %L", &reference->where);        
    return FAILURE;    
  }          
          
  if (reference->next != NULL) {     
    g95_error("Syntax error in substring reference at %L", &reference->where);  
    return FAILURE;         
  } 
 
  /* Convert the substring of a constant to what it should really be */   
   
  if (convert_substring(reference, NULL) == FAILURE) return FAILURE;  
  
  a->type = EXPR_SUBSTRING;   
   
  return SUCCESS;     
}   
   
   
     
     
/* resolve_call()-- Resolve a subroutine call.  Although it was
 * tempting to use the same code for functions, subroutines and
 * functions are stored differently and this makes things awkward. */       
       
static try resolve_call(g95_code *y) {         
try g;         
         
  if (resolve_actual_arglist(y->ext.actual) == FAILURE) return FAILURE; 
 
  switch(procedure_kind(y->sym)) {  
  case PTYPE_GENERIC:      
    g = resolve_generic_s(y);  
    break;     
     
  case PTYPE_SPECIFIC: 
    g = resolve_specific_s(y);          
    break;  
  
  case PTYPE_UNKNOWN: 
    g = resolve_unknown_s(y);   
    break;

  default:       
    g95_internal_error("resolve_call(): bad function type");     
  }        
        
  return g;
}       
       
       
         
         
/* resolve_ref()-- Resolve part references. */  
  
static try resolve_ref(g95_expr *e2) {    
g95_array_spec *a, *result_as;    
g95_component *r;       
g95_typespec typ;     
g95_ref *ref;    
    
  typ = e2->symbol->ts;       
  a = e2->symbol->as;  
  result_as = NULL;   
   
  for(ref=e2->ref; ref; ref=ref->next)      
    switch(ref->type) {   
    case REF_ARRAY:        
      if (a == NULL) {       
	if (typ.type == BT_CHARACTER) {        
	  if (convert_substring(ref, typ.cl) == FAILURE) return FAILURE;       
	  break;        
	}   
   
	g95_error("Unexpected array reference at %L", &ref->where); 
	return FAILURE;        
      }

      if (resolve_array_ref(&ref->u.ar, a, &ref->where) == FAILURE)     
	return FAILURE;      
      
      if (ref->u.ar.type == AR_FULL || ref->u.ar.type == AR_SECTION) {      
	if (result_as != NULL) goto multiple_sections;   
	result_as = a; 
      }          
          
      a = NULL; 
      break; 
 
    case REF_COMPONENT:         
      if (a != NULL) {  
	insert_full_array_ref(e2, ref);      
	if (result_as != NULL) goto multiple_sections;    
	a = NULL;       
      }      
      
      if (typ.type != BT_DERIVED) {
	g95_error("Unexpected component reference at %L", &ref->where);   
	return FAILURE;
      }  
  
      r = find_component(typ.derived, ref, e2->symbol->attr.use_assoc);      
      if (r == NULL) return FAILURE;      
      
      if (r->pointer && result_as != NULL) {
 	g95_error("Component to the right of a part reference with nonzero "
		  "rank must not have the POINTER attribute at %L",
		  &ref->where);
	return FAILURE;  
      }          
          
      ref->u.c.component = r;  
      ref->u.c.sym = typ.derived;      
      
      typ = r->ts;   
      a = r->as;         
      break;         
         
    case REF_SUBSTRING:         
      if (typ.type != BT_CHARACTER) {      
	g95_error("Substring reference at %L must follow a CHARACTER variable",       
		  &ref->where);      
	return FAILURE;        
      }   
   
      if (a != NULL) {  
	g95_error("Substring reference at %L must follow a scalar variable",     
		  &ref->where);        
	return FAILURE;        
      }      
      
      resolve_substring(ref);          
          
      typ.type = BT_UNKNOWN;          
      a = NULL;   
      break;      
    }    
    
  if (a != NULL) {  /* Insert a trailing AR_FULL */        
    ref = g95_extend_ref(e2, REF_ARRAY); 
    ref->u.ar.type = AR_FULL;     
     
    if (result_as != NULL) goto multiple_sections;        
    result_as = a;
  }        
        
  e2->ts = typ;   
  e2->rank = (result_as == NULL) ? 0 : result_as->rank;        
        
  return SUCCESS;      
      
multiple_sections:         
  g95_error("Only one part references with nonzero rank can "        
	    "be specified at %L", &e2->where);     
  return FAILURE;
}       
       
       


/* resolve_contained_functions()-- Resolve contained function types.
 * Because contained functions can call one another, they have to be
 * worked out before any of the contained procedures can be resolved.
 * If a function doesn't already have a type, the only way it can get
 * one is through an IMPLICIT type or a RESULT variable. */      
      
static void resolve_contained_functions(g95_namespace *n) {          
g95_symbol *sym_upper, *sym_lower, *r;
g95_namespace *child;          
int d;        
        
  resolve_formal_arglists(n);        
        
  for(child=n->contained; child; child=child->sibling) { 
    sym_lower = child->proc_name;
    if (sym_lower == NULL) continue;  
  
    g95_find_symbol(sym_lower->name, n, 0, &sym_upper);     
     
    if (sym_upper == NULL)   
      g95_internal_error("resolve_modproc(): Module procedure not found");    
    
    d = namespace_kind(child);    
    if (d == COMP_SUBROUTINE) sym_upper->ts.type = BT_PROCEDURE;      
    if (d != COMP_FUNCTION) continue;          
          
    if (sym_lower->result != NULL) sym_lower = sym_lower->result;       
       
    if (sym_lower->ts.type == BT_UNKNOWN) {        
      if (sym_lower->result == NULL)  
	g95_set_default_type(sym_lower, 1, child);          
      else {  
	r = sym_lower->result;          
          
	if (r->ts.type == BT_UNKNOWN)
	  g95_set_default_type(r, 1, NULL);         
         
	sym_lower->ts = r->ts;       
      }   
    }     
     
    if (sym_upper != sym_lower) {
      sym_upper->ts = sym_lower->ts;          
      sym_upper->as = g95_copy_array_spec(sym_lower->as);

      sym_upper->attr.pointer = sym_lower->attr.pointer;   
      sym_upper->attr.dimension = sym_lower->attr.dimension;
    }         
  }   
}      
      
      
  
  
/* find_common_block0()-- Recursive work function for find_common_block(). */   
   
static g95_symtree *find_common_block0(g95_symtree *st, g95_symbol *symb) {    
g95_symtree *r;        
        
  if (st == NULL) return NULL;

  r = find_common_block0(st->left, symb);         
  if (r != NULL) return r;  
  
  r = find_common_block0(st->right, symb);      
  if (r != NULL) return r;    
    
  return in_common_block(st->n.common->head, symb) ? st : NULL;      
}       
       
       
         
         
/* resolve_variable()-- Resolve a variable. */

static try resolve_variable(g95_expr *x) {         
g95_symbol *sym;       
       
  sym = find_declaration(x->symbol);   
   
  if (resolve_symbol(sym) == FAILURE) return FAILURE;   
  x->symbol = sym;     
     
  sym->attr.used = 1;     
  if (resolve_ref(x) == FAILURE) return FAILURE;       
       
  if (x->symbol->attr.flavor == FL_PROCEDURE && !x->symbol->attr.function) {    
    x->ts.type = BT_PROCEDURE; 
    return SUCCESS;   
  }          
          
  if (x->symbol->ts.type != BT_UNKNOWN)          
    g95_variable_attr(x, &x->ts);          
  else {    /* Must be a simple variable reference */   
    if (g95_set_default_type(x->symbol, 1, NULL) == FAILURE) return FAILURE;  
    x->ts = x->symbol->ts;   
  }        
        
  return SUCCESS;          
}    
    
    
          
          
/* g95_resolve_expr()-- Resolve an expression.  Make sure that types
 * of operands agree with their operators, intrinsic operators are
 * converted to function calls for overloaded types and unresolved
 * function references are resolved. */      
      
try g95_resolve_expr(g95_expr *a) {     
try i;          
          
  if (a == NULL) return SUCCESS; 
 
  if (g95_simplify_expr(a, 0) == FAILURE) return FAILURE;  
  
  switch(a->type) { 
  case EXPR_OP:   
    i = resolve_operator(a);    
    break;     
     
  case EXPR_FUNCTION:    
    i = resolve_function(a);     
    break;    
    
  case EXPR_VARIABLE:  
    i = resolve_variable(a);         
    if (i == SUCCESS) variable_rank(a);    
    break; 
 
  case EXPR_SUBSTRING:  
    i = resolve_substring(a->ref);       
    break;          
          
  case EXPR_CONSTANT:    
    i = resolve_constant(a); 
    break;          
          
  case EXPR_NULL:          
    i = SUCCESS;       
    break; 
 
  case EXPR_ARRAY: 
    i = FAILURE;   
    if (g95_resolve_array_constructor(a) == FAILURE) break;  
  
    i = SUCCESS;   
    /* expression_rank(e);*/   
    break;       
       
  case EXPR_STRUCTURE: 
    i = resolve_structure_cons(a);        
    break;        
        
  case EXPR_UNKNOWN:      
    i = resolve_unknown_expr(a);
    break;     
     
  default:  
    g95_internal_error("g95_resolve_expr(): Bad expression type");         
  }       
       
  return i;     
}       
       
       
         
         
/* find_common_block()-- Given a symbol that is in a common block,
 * figure out which block it is in.  Returns NULL for the blank common
 * block. */   
   
static g95_symtree *find_common_block(g95_symbol *symbol) {      
g95_symtree *common;   
   
  if (in_common_block(symbol->ns->blank_common.head, symbol)) return NULL;         
         
  common = find_common_block0(symbol->ns->common_root, symbol); 
 
  if (common == NULL) 
    g95_internal_error("Symbol '%s' at %L not in any common block",     
		       symbol->name, &symbol->declared_at); 
 
  return common;   
}        
        
        
         
         
/* resolve_initial_values()-- Resolve initial values and make sure
 * they are compatible with the variable */

static try resolve_initial_values(g95_symtree *st1) {        
g95_symbol *symbol;      
      
  if (st1 == NULL) return SUCCESS;        
        
  if (resolve_initial_values(st1->left) == FAILURE || 
      resolve_initial_values(st1->right) == FAILURE) return FAILURE;       
       
  symbol = st1->n.sym;          
          
  if (symbol->value == NULL) return SUCCESS;    
    
  if (g95_resolve_expr(symbol->value) == FAILURE) return FAILURE;     
     
  if (resolve_symbol(symbol) == FAILURE) return FAILURE;         
         
  return g95_check_assign_symbol(symbol, symbol->value);        
}   
   
   
  
  
/* derived_pointer()-- Given a pointer to a symbol that is a derived
 * type, see if any components have the POINTER attribute.  The search
 * is recursive if necessary.  Returns zero if no pointer components
 * are found, nonzero otherwise. */   
   
static int derived_pointer(g95_symbol *sy) {   
g95_component *c;        
        
  for(c=sy->components; c; c=c->next) {       
    if (c->pointer) return 1;          
          
    if (c->ts.type == BT_DERIVED && derived_pointer(c->ts.derived)) return 1;       
  } 
 
  return 0;        
}        
        
        
         
         
/* resolve_equivalenced_var()-- Resolve equivalenced variables and
 * check the additional restrictions that they must satisfy. */          
          
static try resolve_equivalenced_var(g95_expr *d) {
g95_symbol *s;   
g95_ref *r;   
   
  if (g95_resolve_expr(d) == FAILURE) return FAILURE; 
 
  s = d->symbol;       
       
  if (s->attr.dummy) {        
    g95_error("Dummy variable '%s' at %L cannot be EQUIVALENCEd",        
	      s->name, &d->where);   
    return FAILURE;        
  }        
        
  if (s->attr.allocatable) {          
    g95_error("Allocatable array '%s' at %L cannot be EQUIVALENCEd",      
	      s->name, &d->where);  
    return FAILURE;         
  }          
          
  if (s->attr.pointer) {        
    g95_error("Pointer variable '%s' at %L cannot be EQUIVALENCEd",      
	      s->name, &d->where);       
    return FAILURE;         
  }        
        
  if (s->ts.type == BT_DERIVED) {         
         
    if (!s->ts.derived->attr.sequence) {          
      g95_error("Derived variable '%s' in EQUIVALENCE at %L must be a "       
		"SEQUENCE type", s->name, &d->where);         
      return FAILURE; 
    }

    if (derived_pointer(s->ts.derived)) {         
      g95_error("Derived variable '%s' in EQUIVALENCE at %L contains a "       
		"pointer component", s->name, &d->where);   
      return FAILURE;       
    }   
  }         
         
  if (s->attr.function) {   
    g95_error("Function '%s' at %L cannot be EQUIVALENCEd", s->name, 
	      &d->where);      
    return FAILURE;        
  }  
  
  if (s->attr.result_var) {   
    g95_error("RESULT variable '%s' at %L cannot be EQUIVALENCEd", s->name,    
	      &d->where);    
    return FAILURE;          
  }  
  
  if (s->attr.entry) {  
    g95_error("ENTRY name '%s' at %L cannot be EQUIVALENCEd", s->name,        
	      &d->where);   
    return FAILURE;       
  }

  /* Parameters are already taken care of */     
     
  if (s->attr.target) {        
    g95_error("TARGET variable '%s' at %L cannot be EQUIVALENCEd", s->name,     
	      &d->where);        
    return FAILURE;         
  }      
      
  if (s->attr.use_assoc) {         
    g95_error("USE associated variable '%s' at %L cannot be EQUIVALENCEd",    
	      s->name, &d->where);  
    return FAILURE;      
  }      
      
  for(r=d->ref; r; r=r->next)  
    if (r->type == REF_COMPONENT) {  
      g95_error("Structure component of '%s' at %L cannot be EQUIVALENCEd",     
		s->name, &d->where);       
      return FAILURE;          
    }         
         
  return SUCCESS;      
}     
     
     
        
        
/* resolve_equivalence()-- Validate a single equivalance by making
 * sure that if one of a set of equivalent variables is in a common
 * block, the rest are not in another block. */

static try resolve_equivalence(g95_equiv *r) {       
g95_symtree *q, *c;         
g95_symbol *j, *g;     
int seen_common;   
   
  seen_common = 0;        
  q = NULL;          
  j = NULL;     
     
  for(; r; r=r->eq) {        
    if (resolve_equivalenced_var(r->expr) == FAILURE) return FAILURE;      
      
    g = r->expr->symbol;    
    if (!g->attr.in_common) continue;

    c = find_common_block(g);  
  
    if (!seen_common) {   
      q = c;       
      j = g;  
      seen_common = 1;        
    } else {
      if (q != c) {       
	g95_error("Symbols '%s' at %L and '%s' are equivalenced but in "
		  "different common blocks", j->name, &j->declared_at,   
		  g->name);
	return FAILURE; 
      }      
    }   
  }      
      
  return SUCCESS;     
}  
  
  
     
     
/* resolve_code()-- Given a block of code, recursively resolve
 * everything pointed to by this code block */    
    
static void resolve_code(g95_code *c, g95_namespace *names) {      
int forall_save=0;   
code_stack frame;
g95_alloc *g;      
g95_code *m;        
        
  if (c == NULL) return;  
  
  frame.prev = cs_base;         
  frame.head = c;     
  cs_base = &frame;   
   
  for(; c; c=c->next) {         
    frame.current = c;         
        
    if (c->type == EXEC_FORALL) {          
      forall_save = forall_flag;      
      forall_flag = 1;       
    }     
     
    if (c->type == EXEC_FORALL) forall_flag = forall_save;   
   
    if (g95_resolve_expr(c->expr) == FAILURE ||    
	g95_resolve_expr(c->expr2) == FAILURE) continue;       
       
    switch(c->type) {     
    case EXEC_NOP:       case EXEC_CYCLE:   
    case EXEC_STOP:      case EXEC_EXIT:      case EXEC_CONTINUE:       
    case EXEC_DT_END:    case EXEC_TRANSFER:
      break;        
        
    case EXEC_PAUSE: 
      if (c->expr != NULL &&          
	  c->expr->ts.type != BT_INTEGER &&    
	  c->expr->ts.type != BT_CHARACTER)         
	g95_error("PAUSE code at %L must be INTEGER or CHARACTER",      
		  &c->expr->where);        
        
      break; 
 
    case EXEC_GOTO:          
      resolve_branch(c->label, c); 
      break;       
       
    case EXEC_RETURN: 
      if (c->expr != NULL && c->expr->ts.type != BT_INTEGER)   
	g95_error("Alternate RETURN statement at %L requires an INTEGER "     
		  "return specifier", &c->expr->where); 
 
      break;   
   
    case EXEC_ASSIGN:      
      if (g95_extend_assign(c, names) == SUCCESS) goto call;

      c->expr->symbol->attr.set = 1;       
       
      if (g95_pure(NULL)) {          
	if (g95_impure_variable(c->expr->symbol)) {    
	  g95_error("Cannot assign to variable '%s' in PURE procedure at %L",       
		    c->expr->symbol->name, &c->expr->where);         
	  break;       
	}        
        
	if (c->expr2->ts.type == BT_DERIVED &&        
	    derived_pointer(c->expr2->ts.derived)) {     
	  g95_error("Right side of assignment at %L is a derived type "      
		    "containing a POINTER in a PURE procedure",     
		    &c->expr2->where);      
	  break;      
	}     
      }    
    
      g95_check_assign(c->expr, c->expr2, 1);     
      break;    
    
    case EXEC_POINTER_ASSIGN:         
      c->expr->symbol->attr.set = 1;    
      g95_check_pointer_assign(c->expr, c->expr2);     
      break;     
     
    case EXEC_ARITHMETIC_IF: 
      if (c->expr->ts.type != BT_INTEGER &&          
	  c->expr->ts.type != BT_REAL) {       
	g95_error("Arithmetic IF statement at %L requires a numeric "
		  "expression", &c->expr->where);      
	break;      
      }          
          
      if (resolve_branch(c->label, c) == FAILURE ||        
	  resolve_branch(c->label2, c) == FAILURE ||       
	  resolve_branch(c->label3, c) == FAILURE) break;

      break;     
     
    case EXEC_IF:  
      if (c->expr != NULL &&         
	  (c->expr->ts.type != BT_LOGICAL || c->expr->rank != 0)) {    
	g95_error("IF clause at %L requires a scalar LOGICAL expression",     
		  &c->expr->where);     
	break;         
      }      
      
      resolve_code(c->block, names);       
      resolve_code(c->ext.block, names);      
      break;     
     
    case EXEC_CALL: 
    call:         
      resolve_call(c);
      break;      
      
    case EXEC_SELECT:      /* Select is complicated */  
      g95_resolve_select(c);          
          
      /* Fall through */

    case EXEC_WHERE:   
      for(m=c->block; m; m=m->block)        
	resolve_code(m->next, names);   
   
      break;  
  
    case EXEC_DO:   
      if (c->ext.iterator != NULL)  
	g95_resolve_iterator(c->ext.iterator);         
         
      resolve_code(c->block, names);
      break; 
 
    case EXEC_DO_WHILE:    
      resolve_code(c->block, names);  
  
      if (c->expr == NULL) break;       
       
      if (c->expr->rank != 0 || c->expr->ts.type != BT_LOGICAL)  
	g95_error("Exit condition of DO WHILE loop at %L must be "       
		  "a scalar LOGICAL expression", 
		  &c->expr->where);       
       
      transform_while(c);          
      break;   
   
    case EXEC_ALLOCATE:       
      if (c->expr != NULL) {     
	c->expr->symbol->attr.set = 1;    
    
	if (c->expr->ts.type != BT_INTEGER) {        
	  g95_error("STAT tag in ALLOCATE statement at %L must be "        
		    "of type INTEGER", &c->expr->where);     
	  break;         
	} 
      }      
      
      for(g=c->ext.alloc_list; g; g=g->next)      
        if (resolve_allocate_expr(g->expr) == FAILURE) break;          
          
      break;

    case EXEC_DEALLOCATE:      
      if (c->expr != NULL) {   
	c->expr->symbol->attr.set = 1;        
        
	if (c->expr->ts.type != BT_INTEGER) {        
	  g95_error("STAT tag in DEALLOCATE statement at %L must be of type "
		    "INTEGER", &c->expr->where);          
	  break;   
	}        
      }   
   
      for(g=c->ext.alloc_list; g; g=g->next)      
	if (resolve_deallocate_expr(g->expr) == FAILURE) break; 
 
      break;      
      
    case EXEC_OPEN:         
      if (g95_resolve_open(c->ext.open) == SUCCESS)          
	resolve_branch(c->ext.open->err, c);        
        
      break;    
    
    case EXEC_CLOSE:      
      if (g95_resolve_close(c->ext.close) == SUCCESS)    
	resolve_branch(c->ext.close->err, c);

      break;         
         
    case EXEC_BACKSPACE:     
    case EXEC_ENDFILE:         
    case EXEC_REWIND:          
      if (g95_resolve_filepos(c->ext.filepos) == SUCCESS)   
	  resolve_branch(c->ext.filepos->err, c); 
 
      break;   
   
    case EXEC_INQUIRE:     
      if (g95_resolve_inquire(c->ext.inquire) == SUCCESS) 
	resolve_branch(c->ext.inquire->err, c);    
    
      break;  
  
    case EXEC_IOLENGTH:     
      if (c->expr->ts.type != BT_INTEGER ||   
	  c->expr->ts.kind != g95_default_integer_kind())        
	g95_error("IOLENGTH variable in INQUIRE statement at %L must be " 
		  "default integer", &c->expr->where);     
     
      break;       
       
    case EXEC_READ:       
    case EXEC_WRITE:          
      if (g95_resolve_dt(c->ext.dt) == FAILURE || 
	  resolve_branch(c->ext.dt->err, c) == FAILURE ||         
	  resolve_branch(c->ext.dt->end, c) == FAILURE ||   
	  resolve_branch(c->ext.dt->eor, c) == FAILURE) 
	break;         
         
      break;     
     
    case EXEC_FORALL:        
      if (resolve_forall_iterators(c->ext.forall_iterator) == FAILURE)      
	break;  
  
      if (c->expr != NULL && c->expr->ts.type != BT_LOGICAL)        
	g95_error("FORALL mask clause at %L requires a LOGICAL expression",    
		  &c->expr->where);      
      
      resolve_code(c->block, names);     
      break; 
 
    default:       
      g95_internal_error("resolve_code(): Bad statement code");          
    }      
  }      
      
  cs_base = frame.prev;    
    
  return;  
}         
         
         
     
     
/* g95_resolve()-- This function is called after a complete program
 * unit has been compiled.  Its purpose is to examine all of the
 * expressions associated with a program unit, assign types to all
 * intermediate expressions, make sure that all assignments are to
 * compatible types and figure out which names refer to which
 * functions or subroutines. */          
          
try g95_resolve(g95_namespace *namesp) {        
g95_namespace *old_ns, *v;      
g95_symbol *s;      
g95_equiv *eq2;   
g95_data *m;     
try r;         
         
  old_ns = g95_current_ns;         
  g95_current_ns = namesp; 
 
  r = FAILURE;     
  resolve_contained_functions(namesp);     
     
  for(v=namesp->contained; v; v=v->sibling) {      
    if (g95_pure(namesp->proc_name) && !g95_pure(v->proc_name))       
      g95_error("Contained procedure '%s' at %L of a PURE procedure must "    
		"also be PURE", v->proc_name->name, 
		&v->proc_name->declared_at);     
     
    if (g95_resolve(v) == FAILURE) goto done;  
  } 
 
  forall_flag = 0;         
  g95_check_interfaces(namesp);       
       
  if (resolve_charlen(namesp) == FAILURE) goto done;  
  
  if (resolve_initial_values(namesp->sym_root) == FAILURE) goto done;      
      
  if (namesp->save_all) g95_save_all(namesp);

  for(m=namesp->data; m; m=m->next) 
    if (resolve_data(m) == FAILURE) goto done;    
    
  resolve_common_block(namesp->common_root);      
  g95_traverse_ns(namesp, resolve_common_var);     
     
  for(eq2=namesp->equiv; eq2; eq2=eq2->next)          
    if (resolve_equivalence(eq2) == FAILURE) goto done;       
       
  cs_base = NULL;       
  resolve_code(namesp->code, namesp);  
  
  if (resolve_symbols(namesp->sym_root) == FAILURE) goto done;      
      
  if (g95_option.unused_label) warn_unused_label(namesp);    
    
  if (g95_option.pedantic) g95_traverse_ns(namesp, check_variable_usage);       
       
  s = namesp->proc_name;  
  if (namespace_kind(namesp) == COMP_FUNCTION &&     
      s->result->ts.type == BT_UNKNOWN && !s->result->attr.untyped) {       
    g95_error("Function '%s' at %L has no IMPLICIT type",        
	      s->name, &s->declared_at);  
    goto done;      
  }       
       
  r = SUCCESS;  
  
done: 
  g95_current_ns = old_ns;      
  return r;       
}   
