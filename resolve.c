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
 
 
         
         
/* resolve_operator()-- Resolve an operator expression node.  This can
 * involve replacing the operation with a user defined function call. */      
      
static try resolve_operator(g95_expr *g) {
g95_expr *op_1, *op;          
char msg[200];   
try m;         
         
/* Resolve all subnodes-- give them types. */   
   
  switch(g->operator) {  
  default: 
    if (g95_resolve_expr(g->op2) == FAILURE) return FAILURE;         
         
/* Fall through */

  case INTRINSIC_NOT:        
  case INTRINSIC_UPLUS:         
  case INTRINSIC_UMINUS:     
    if (g95_resolve_expr(g->op1) == FAILURE) return FAILURE;         
    break;    
  }       
       
/* Typecheck the new node. */   
   
  op_1 = g->op1; 
  op = g->op2; 
 
  switch(g->operator) {     
  case INTRINSIC_UPLUS:       
  case INTRINSIC_UMINUS:          
    if ((op_1->ts.type == BT_INTEGER) || (op_1->ts.type == BT_REAL) ||         
	(op_1->ts.type == BT_COMPLEX)) {          
      g->ts = op_1->ts;      
      break;  
    }          
          
    sprintf(msg, "Operand of unary numeric operator '%s' at %%L is %s",
	    g95_op2string(g->operator), g95_typename(&g->ts)); 
    goto bad_op;     
     
  case INTRINSIC_PLUS:      
  case INTRINSIC_MINUS: 
  case INTRINSIC_TIMES:    
  case INTRINSIC_DIVIDE:     
  case INTRINSIC_POWER:  
    if (g95_numeric_ts(&op_1->ts) && g95_numeric_ts(&op->ts)) {      
      g95_type_convert_binary(g);
      break;  
    }          
          
    sprintf(msg, "Operands of binary numeric operator '%s' at %%L are %s/%s",         
	    g95_op2string(g->operator), g95_typename(&op_1->ts),         
	    g95_typename(&op->ts));      
    goto bad_op;   
   
  case INTRINSIC_CONCAT:          
    if (op_1->ts.type == BT_CHARACTER && op->ts.type == BT_CHARACTER) {
      g->ts.type = BT_CHARACTER;   
      g->ts.kind = op_1->ts.kind;       
      break;          
    }      
      
    sprintf(msg, "Operands of string concatenation operator at %%L are %s/%s",
	    g95_typename(&op_1->ts), g95_typename(&op->ts));    
    goto bad_op;   
   
  case INTRINSIC_AND:       
  case INTRINSIC_OR:         
  case INTRINSIC_EQV:   
  case INTRINSIC_NEQV:  
    if (op_1->ts.type == BT_LOGICAL && op->ts.type == BT_LOGICAL) {        
      g->ts.type = BT_LOGICAL;  
      g->ts.kind = g95_kind_max(op_1, op);         
      break; 
    }

    sprintf(msg, "Operands of logical operator '%s' at %%L are %s/%s",          
	    g95_op2string(g->operator), g95_typename(&op_1->ts),       
	    g95_typename(&op->ts));          
          
    goto bad_op;          
                
  case INTRINSIC_NOT:          
    if (op_1->ts.type == BT_LOGICAL) {   
      g->ts.type = BT_LOGICAL;
      g->ts.kind = op_1->ts.kind;     
      break;        
    }    
    
    sprintf(msg, "Operand of .NOT. operator at %%L is %s",   
	    g95_typename(&op_1->ts));      
    goto bad_op;      
      
  case INTRINSIC_GT: case INTRINSIC_GE:  
  case INTRINSIC_LT: case INTRINSIC_LE:      
    if (op_1->ts.type == BT_COMPLEX || op->ts.type == BT_COMPLEX) {    
      strcpy(msg, "COMPLEX quantities cannot be compared at %L");  
      goto bad_op;        
    }         
         
    /* Fall through */         
         
  case INTRINSIC_EQ: case INTRINSIC_NE:         
    if (op_1->ts.type == BT_CHARACTER && op->ts.type == BT_CHARACTER) {          
      g->ts.type = BT_LOGICAL; 
      g->ts.kind = g95_default_logical_kind();          
      break;          
    }     
     
    if (g95_numeric_ts(&op_1->ts) && g95_numeric_ts(&op->ts)) {          
      g95_type_convert_binary(g);        
	        
      g->ts.type = BT_LOGICAL;  
      g->ts.kind = g95_default_logical_kind();        
      break;     
    }      
      
    sprintf(msg, "Operands of comparison operator '%s' at %%L are %s/%s", 
	    g95_op2string(g->operator), g95_typename(&op_1->ts), 
	    g95_typename(&op->ts));        
        
    goto bad_op;        
        
  case INTRINSIC_USER:         
    if (op == NULL)        
      sprintf(msg, "Operand of user operator '%s' at %%L is %s",   
	      g->symbol->name, g95_typename(&op_1->ts));
    else 
      sprintf(msg, "Operands of user operator '%s' at %%L are %s/%s",    
	      g->symbol->name, g95_typename(&op_1->ts), g95_typename(&op->ts));  
  
    goto bad_op;          
          
  default:
    g95_internal_error("resolve_operator(): Bad intrinsic");        
  }         
         
/* Deal with arrayness of an operand through an operator */ 
 
  m = SUCCESS;         
         
  switch(g->operator) {
  case INTRINSIC_PLUS:    case INTRINSIC_MINUS:  case INTRINSIC_TIMES:
  case INTRINSIC_DIVIDE:  case INTRINSIC_POWER:  case INTRINSIC_CONCAT:       
  case INTRINSIC_AND:     case INTRINSIC_OR:     case INTRINSIC_EQV: 
  case INTRINSIC_NEQV:    case INTRINSIC_EQ:     case INTRINSIC_NE:       
  case INTRINSIC_GT:      case INTRINSIC_GE:     case INTRINSIC_LT:  
  case INTRINSIC_LE:     
     
    if (op_1->rank == 0 && op->rank == 0) g->rank = 0;    
    
    if (op_1->rank == 0 && op->rank != 0) {  
      g->rank = op->rank;     
     
      if (g->shape == NULL) g->shape = g95_copy_shape(op->shape, op->rank);          
    }     
     
    if (op_1->rank != 0 && op->rank == 0) {     
      g->rank = op_1->rank;       
       
      if (g->shape == NULL) g->shape = g95_copy_shape(op_1->shape, op_1->rank);        
    } 
 
    if (op_1->rank != 0 && op->rank != 0) {  
      if (op_1->rank == op->rank) {     
	g->rank = op_1->rank;          
          
	if (g->shape == NULL) g->shape = g95_copy_shape(op_1->shape, op_1->rank);    
    
      } else {     
	g95_error("Inconsistent ranks for operator at %L and %L", 
		  &op_1->where, &op->where); 
	m = FAILURE;  
  
	g->rank = 0;   /* Allow higher level expressions to work */  
      } 
    }

    break;     
     
  case INTRINSIC_NOT: 
  case INTRINSIC_UPLUS: 
  case INTRINSIC_UMINUS:
    g->rank = op_1->rank;  
  
    if (g->shape == NULL) g->shape = g95_copy_shape(op_1->shape, op_1->rank);         
         
    break;           /* Simply copy arrayness attribute */         
         
  default:  
    break;        
  }        
        
  if (m == SUCCESS) m = g95_simplify_expr(g, 0);     
  return m;         
         
bad_op:   
  if (g95_extend_expr(g) == SUCCESS) return SUCCESS;  
  
  g95_error(msg, &g->where);         
  return FAILURE;    
}      
      
      
    
    
/* resolve_deallocate_expr()-- Resolve the argument of a deallocate
 * expression.  The expression must be a pointer or a full array. */         
         
static try resolve_deallocate_expr(g95_expr *n) {       
symbol_attribute atr;  
int allocatable;         
g95_ref *re;      
      
  if (g95_resolve_expr(n) == FAILURE) return FAILURE;      
      
  atr = g95_expr_attr(n);         
  if (atr.pointer) return SUCCESS;     
     
  if (n->type != EXPR_VARIABLE) goto bad;          
          
  allocatable = n->symbol->attr.allocatable;    
  for(re=n->ref; re; re=re->next) 
    switch(re->type) {          
    case REF_ARRAY:  
      if (re->u.ar.type != AR_FULL) allocatable = 0;        
      break;

    case REF_COMPONENT:  
      allocatable = (re->u.c.component->as != NULL &&          
		     re->u.c.component->as->type == AS_DEFERRED);        
      break; 
 
    case REF_SUBSTRING:    
      allocatable = 0;    
      break; 
    }        
        
  if (allocatable == 0) {         
  bad:        
    g95_error("Expression in DEALLOCATE statement at %L must be "        
	      "ALLOCATABLE or a POINTER", &n->where);      
  }  
  
  return SUCCESS; 
} 
 
 
          
          
static void pure_subroutine(g95_code *t, g95_symbol *s) {   
   
  if (g95_pure(s)) return;        
        
  if (forall_flag)       
    g95_error("Subroutine call to '%s' in FORALL block at %L is not PURE",
	      t->sym->name, &t->loc);     
  else if (g95_pure(NULL))   
    g95_error("Subroutine call to '%s' at %L is not PURE", t->sym->name,       
	      &t->loc);  
}          
          
          
          
          
/* namespace_kind()-- Given a namespace, figure out what kind it is.
 * We return one of the g95_compile_state enums COMP_NONE,
 * COMP_MODULE, COMP_SUBROUTINE or COMP_FUNCTION. */     
     
static g95_compile_state namespace_kind(g95_namespace *names) {       
g95_symbol *s; 
 
  s = names->proc_name;    
    
  if (s == NULL) return COMP_NONE;       
       
  if (s->attr.flavor == FL_MODULE) return COMP_MODULE;

  if (s->attr.subroutine) return COMP_SUBROUTINE;          
          
  if (s->attr.flavor == FL_VARIABLE ||  
      s->attr.function) return COMP_FUNCTION;  
  
  return COMP_NONE;     
}      
      
      
         
         
/* move_variable()-- Given the right hand side of the statement
 * function, replace the old symbol which is a formal argument with a
 * new symbol in the right namespace.  Recursively traverses the
 * expression tree. */    
    
static void move_variable(g95_expr *r, g95_symbol *old, g95_symbol *n1) {        
g95_actual_arglist *g;  
  
  if (r == NULL) return;        
        
  switch(r->type) {        
  case EXPR_OP:  
  case EXPR_SUBSTRING:
    move_variable(r->op1, old, n1);       
    move_variable(r->op2, old, n1); 
    break;          
          
  case EXPR_FUNCTION:         
    if (r->symbol == old) r->symbol = n1;    
    
    for(g=r->value.function.actual; g; g=g->next)       
      move_variable(g->u.expr, old, n1); 
 
    break;     
     
  case EXPR_CONSTANT:  
  case EXPR_NULL:     
    break;

  case EXPR_VARIABLE:      
    if (r->symbol == old) r->symbol = n1;     
    break;      
      
  case EXPR_STRUCTURE:      
  case EXPR_ARRAY:     
    move_constructor(r->value.constructor, old, n1);         
    break;     
  } 
}  
  
  
        
        
/* derived_init()-- Return nonzero if the derived type symbol has a
 * default initialization or contains a subtype that has a default
 * initialization.  */          
          
static int derived_init(g95_symbol *s) {        
g95_component *z;         
         
  for(z=s->ts.derived->components; z; z=z->next) {       
    if (z->initializer != NULL) return 1;        
        
    if (z->ts.type != BT_DERIVED || z->pointer) continue; 
 
    if (derived_init(z->ts.derived)) return 1; 
  }        
        
  return 0; 
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
g95_formal_arglist *h;    
g95_symbol *s;      
      
  for(h=proc->formal; h; h=h->next) {
    s = h->sym;       
       
    if (s == NULL) {  /* Alternate return placeholder */         
      if (g95_elemental(proc))        
	g95_error("Alternate return specifier in elemental subroutine "  
		  "'%s' at %L is not allowed", proc->name, &proc->declared_at);        
      continue;      
    }   
   
    s->attr.set = 1;         
    if (s->attr.if_source != IFSRC_UNKNOWN) resolve_formal_arglist(s);  
  
    if (s->attr.subroutine || s->attr.external || s->attr.intrinsic) {     
      if (g95_pure(proc) && !g95_pure(s)) {   
	g95_error("Dummy procedure '%s' of PURE procedure at %L must also "      
		  "be PURE", s->name, &s->declared_at);       
	continue;       
      }    
    
      if (g95_elemental(proc)) {    
	g95_error("Dummy procedure at %L not allowed in ELEMENTAL procedure",  
		  &s->declared_at);
	continue;          
      }          
          
      continue; 
    }

    if (s->ts.type == BT_UNKNOWN) {      
      if (!s->attr.function || s->result == s)         
	g95_set_default_type(s, 1, s->ns);
      else {    /* Set the type of the RESULT, then copy */

	if (s->result->ts.type == BT_UNKNOWN)  
	  g95_set_default_type(s->result, 1, s->result->ns);   
   
	s->ts = s->result->ts;    
	if (s->as == NULL) s->as = g95_copy_array_spec(s->result->as);  
      }   
    }  
  
    g95_resolve_array_spec(s->as, 0);

    if (proc->attr.proc == PROC_ST_FUNCTION && s->as != NULL) 
      g95_error("Argument '%s' of statement function at %L must be scalar",        
		s->name, &s->declared_at); 
 
    /* If the flavor is unknown at this point, it has to be a variable.
     * A procedure specification would have already set the type */

    if (s->attr.flavor == FL_UNKNOWN)        
      g95_add_flavor(&s->attr, FL_VARIABLE, &s->declared_at);    
    
    if (g95_pure(proc)) {         
      if (proc->attr.function && !s->attr.pointer &&         
	  s->attr.intent != INTENT_IN)        
        
	g95_error("Argument '%s' of pure function '%s' at %L must be "         
		  "INTENT(IN)", s->name, proc->name, &s->declared_at);       
       
      if (proc->attr.subroutine && !s->attr.pointer &&
	  s->attr.intent == INTENT_UNKNOWN)        
       
	g95_error("Argument '%s' of pure subroutine '%s' at %L must have " 
		  "its INTENT specified", s->name, proc->name,  
		  &s->declared_at);      
    }  
  
    if (g95_elemental(proc)) {       
      if (s->as != NULL) {  
	g95_error("Argument '%s' of elemental procedure at %L must be scalar",      
		  s->name, &s->declared_at);       
	continue;         
      }    
    
      if (s->attr.pointer) {         
	g95_error("Argument '%s' of elemental procedure at %L cannot have "          
		  "the POINTER attribute", s->name, &s->declared_at);      
	continue;         
      }  
    }    
  }         
}         
         
         
     
     
/* resolve_allocate_expr()-- Resolve the expression in an ALLOCATE
 * statement, doing the additional checks to see whether the
 * expression is OK or not.  The expression must have a trailing array
 * reference that gives the size of the array. */      
      
static try resolve_allocate_expr(g95_expr *d) {      
int p, pointer, allocatable, dimension;     
symbol_attribute atr;  
g95_ref *reference, *ref2;     
g95_array_ref *spec;  
  
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
         
    for(reference=d->ref; reference; ref2=reference, reference=reference->next)   
      switch(reference->type) {  
      case REF_ARRAY:  
	if (reference->next != NULL) pointer = 0; 
       	break;       
       
      case REF_COMPONENT:
	allocatable = (reference->u.c.component->as != NULL &&
		       reference->u.c.component->as->type == AS_DEFERRED);   
   
	pointer = reference->u.c.component->pointer;      
	dimension = reference->u.c.component->dimension;   
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
    
  spec = &ref2->u.ar;       
       
  for(p=0; p<spec->dimen; p++)          
    switch(spec->dimen_type[p]) {        
    case DIMEN_ELEMENT:        
      break;        
        
    case DIMEN_RANGE:         
      if (spec->start[p] != NULL && spec->end[p] != NULL && 
	  spec->stride[p] == NULL) break;   
   
      /* Fall Through */   
   
    case DIMEN_UNKNOWN:
    case DIMEN_VECTOR:  
      g95_error("Bad array specification in ALLOCATE statement at %L",          
		&d->where);      
      return FAILURE;    
    }      
      
  return SUCCESS;      
}    
    
    
  
  
/* resolve_forall_iterators()-- Resolve a list of FORALL iterators */      
      
static void resolve_forall_iterators(g95_forall_iterator *i) {   
   
  while(i) {
    if (g95_resolve_expr(i->var) == SUCCESS &&
	i->var->ts.type != BT_INTEGER)         
      g95_error("FORALL Iteration variable at %L must be INTEGER",      
		&i->var->where);      
      
    if (g95_resolve_expr(i->start) == SUCCESS &&      
	i->start->ts.type != BT_INTEGER)      
      g95_error("FORALL start expression at %L must be INTEGER",          
		&i->start->where);       
       
    if (g95_resolve_expr(i->end) == SUCCESS &&          
	i->end->ts.type != BT_INTEGER) 
      g95_error("FORALL end expression at %L must be INTEGER",   
		&i->end->where); 
 
    if (g95_resolve_expr(i->stride) == SUCCESS &&   
	i->stride->ts.type != BT_INTEGER)
      g95_error("FORALL Stride expression at %L must be INTEGER",       
		&i->stride->where);        
        
    i = i->next;       
  }      
}      
      
      
 
 
/* move_constructor()-- Mutually recursive with move_variable() to fix
 * symbol references within the right side of a statement function. */      
      
static void move_constructor(g95_constructor *z, g95_symbol *o,
			     g95_symbol *n) { 
  for(; z; z=z->next) {          
    move_variable(z->expr, o, n); 
 
    if (z->iterator == NULL) continue;          
          
    move_variable(z->iterator->start, o, n);         
    move_variable(z->iterator->end,   o, n);
    move_variable(z->iterator->step,  o, n);   
  } 
}       
       
       
     
     
/* derived_pointer()-- Given a pointer to a symbol that is a derived
 * type, see if any components have the POINTER attribute.  The search
 * is recursive if necessary.  Returns zero if no pointer components
 * are found, nonzero otherwise. */       
       
static int derived_pointer(g95_symbol *sym) {      
g95_component *n; 
 
  for(n=sym->components; n; n=n->next) {      
    if (n->pointer) return 1; 
 
    if (n->ts.type == BT_DERIVED && derived_pointer(n->ts.derived)) return 1;      
  }          
          
  return 0;  
}        
        
        
   
   
/* g95_elemental()-- Test whether the current procedure is elemental or not */    
    
int g95_elemental(g95_symbol *sy) {     
symbol_attribute a;     
     
  if (sy == NULL) sy = g95_current_ns->proc_name;    
  if (sy == NULL) return 0;         
  a = sy->attr;    
    
  return a.flavor == FL_PROCEDURE && a.elemental;        
}        
        
        
       
       
/* resolve_common_var()-- Check the extra restrictions on variables
 * within a common block. */      
      
static void resolve_common_var(g95_symbol *symbol) {         
g95_array_spec *as;
int k;    
    
  if (!symbol->attr.in_common) return;  
  
  /* Derived type names must have the SEQUENCE attribute */  
  
  if (symbol->ts.type == BT_DERIVED) {       
    if (!symbol->ts.derived->attr.sequence) { 
      g95_error("Derived type variable '%s' at %L must have the SEQUENCE "         
		"attribute to be in a COMMON", symbol->name, &symbol->declared_at); 
      return;      
    }     
     
    if (derived_init(symbol)) {   
      g95_error("Derived type variable '%s' at %L cannot have a default "          
		"initialization and be in a COMMON",
		symbol->name, &symbol->declared_at);
    }   
  }       
       
  as = symbol->as;     
  if (as != NULL) {    
    if (as->type != AS_EXPLICIT) {    
      g95_error("Array specification for symbol '%s' in COMMON at %L"    
		"must be explicit", symbol->name, &symbol->declared_at); 
      return;
    }   
   
    for(k=0; k<as->rank; k++) {     
      if (as->lower[k]->type != EXPR_CONSTANT ||    
	  as->upper[k]->type != EXPR_CONSTANT) { 
	g95_error("Dimension %d of array '%s' in COMMON at %L is not constant",     
		  k+1, symbol->name, &symbol->declared_at);       
	return;   
      }       
    }     
     
    if (symbol->attr.pointer) {    
      g95_error("Array '%s' in COMMON at %L cannot be a POINTER array",       
		symbol->name, &symbol->declared_at);         
      return;   
    }          
  }      
}    
    
    
 
 
/* resolve_structure_cons()-- Resolve all of the elements of a
 * structure constructor and make sure that the types are correct. */         
         
static try resolve_structure_cons(g95_expr *e) {
g95_constructor *cons;     
g95_component *comp; 
try p;

  p = SUCCESS;         
  cons = e->value.constructor;   
  comp = e->symbol->components;    
    
  for(; comp; comp=comp->next, cons=cons->next) { 
    if (g95_resolve_expr(cons->expr) == FAILURE) {     
      p = FAILURE;          
      continue;        
    }  
  
    /* If we don't have the right type, try to convert it. */         
         
    if (!g95_compare_types(&cons->expr->ts, &comp->ts) &&         
	g95_convert_type(cons->expr, &comp->ts, 1) == FAILURE)      
      p = FAILURE;       
  }     
     
  return p;       
}       
       
       
        
        
/* resolve_index()-- Resolve one part of an array index */    
    
static try resolve_index(g95_expr *ind, int check_scalar) {     
g95_typespec t; 
 
  if (ind == NULL) return SUCCESS;        
        
  if (g95_resolve_expr(ind) == FAILURE) return FAILURE;        
        
  if (ind->ts.type != BT_INTEGER) {     
    g95_error("Array index at %L must be of INTEGER type", &ind->where);    
    return FAILURE;
  }

  if (check_scalar && ind->rank != 0) {      
    g95_error("Array index at %L must be scalar", &ind->where);        
    return FAILURE;
  }  
  
  if (ind->ts.kind != g95_default_integer_kind()) {         
    t.type = BT_INTEGER;     
    t.kind = g95_default_integer_kind(); 
 
    g95_convert_type(ind, &t, 2);    
  }       
       
  return SUCCESS;  
} 
 
 
       
       
static match resolve_generic_s0(g95_code *z, g95_symbol *sy) {       
g95_symbol *x;     
     
  if (sy->attr.generic) {
    x = g95_search_interface(sy->generic, 1, &z->ext.actual);        
    if (x != NULL) {          
      if (x->attr.proc == PROC_UNKNOWN) x->attr.proc = PROC_EXTERNAL;  
  
      z->sym = x;
      pure_subroutine(z, x);   
      return MATCH_YES; 
    }          
          
    /* TODO: Need to search for elemental references in generic interface */       
  }         
         
  if (sy->attr.intrinsic) return g95_intrinsic_sub_interface(z, 0);      
      
  return MATCH_NO;    
} 
 
 


/* transform_while()-- Transform a DO WHILE statement into an infinite
 * loop with an appropriate EXIT statement at the front. */ 
 
static void transform_while(g95_code *codep) {       
g95_code *u, *b;  
g95_expr *d;        
        
  d = codep->expr;
  if (d->type == EXPR_CONSTANT && d->value.logical)       
    g95_free_expr(d);
  else {        
    d = g95_get_expr();         
    d->type = EXPR_OP;    
    d->operator = INTRINSIC_NOT;      
      
    d->ts.type = BT_LOGICAL;         
    d->ts.kind = g95_default_logical_kind();          
    d->op1 = codep->expr;    
    
    b = g95_get_code();         
    b->type = EXEC_EXIT; 
    b->ext.block = codep;       
       
    u = g95_get_code();       
    u->block = b;   
    u->expr = d;      
    u->type = EXEC_IF; 
 
    u->next = codep->block;     
    codep->block = u;   
  }   
   
  codep->expr = NULL;
}  
  
  
  
  
/* move_formal_arglist()-- Given a symbol that is a statement
 * function, effectively move its formal argument list into the
 * contained namespace so that the dummy arguments have a separate
 * existence from symbols of the same name in the parent. */         
         
static void move_formal_arglist(g95_symbol *symbol) {  
g95_formal_arglist *j;   
g95_symbol *old, *n1;     
g95_expr *x;    
    
  x = g95_current_ns->code->expr2;    /* RHS of the statement function */        
        
  for(j=symbol->formal; j; j=j->next) {     
    old = j->sym;   
    old->attr.used = 1;          
          
    g95_get_symbol(old->name, g95_current_ns, &n1);      
    n1->ts = old->ts;       
    n1->declared_at = old->declared_at;          
          
    n1->attr.flavor = FL_VARIABLE;       
    n1->attr.dummy = 1;

    move_variable(x, old, n1);        
    j->sym = n1;         
  }    
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
         
         
  
  
/* warn_unused_label()-- Warn about unused labels. */

static void warn_unused_label(g95_namespace *names){   
g95_st_label *i;          
          
  i = names->st_labels;        
  if (i == NULL) return;          
          
  while(i->next)     
    i = i->next;    
      
  for(; i; i=i->prev) {  
    if (i->defined == ST_LABEL_UNKNOWN) continue; 
 
    switch(i->referenced){      
    case ST_LABEL_UNKNOWN:          
      g95_warning("Label %d at %L defined but not used", i->value, &i->where);          
      break;

    case ST_LABEL_BAD_TARGET:   
      g95_warning("Label %d at %L defined but cannot be used", i->value,
		  &i->where);
      break;      
      
    default:      
      break; 
    }        
  }    
}        
        
        
     
     
/* compare_bound()-- Compare two integer expressions. */       
       
static comparison compare_bound(g95_expr *e, g95_expr *r) {  
int h;        
        
  if (e == NULL || e->type != EXPR_CONSTANT ||   
      r == NULL || r->type != EXPR_CONSTANT) return CMP_UNKNOWN;          
          
  if (e->ts.type != BT_INTEGER || r->ts.type != BT_INTEGER)    
    g95_internal_error("compare_bound(): Bad expression");          
          
  h = mpz_cmp(e->value.integer, r->value.integer);   
   
  if (h < 0) return CMP_LT;
  if (h > 0) return CMP_GT;      
  return CMP_EQ;    
}       
       
       
   
   
/* function_type()-- Set the expression type from the function type. */    
    
static try function_type(g95_expr *expr, g95_symbol *symb) {       
g95_typespec *typ;   
   
  if (symb->result->ts.type != BT_UNKNOWN)     
    expr->ts = symb->result->ts;       
  else {         
    typ = g95_get_default_type(symb->result, symb->ns);

    if (typ->type == BT_UNKNOWN) { 
      g95_error("Function '%s' at %L has no implicit type",     
		symb->name, &expr->where);         
      return FAILURE;        
    } else    
      expr->ts = *typ;      
  }      
      
  return SUCCESS;         
}          
          
          
 
 
/* was_declared()-- Returns 0 if a symbol was not declared with a type
 * or attribute declaration statement, nonzero otherwise. */        
        
static int was_declared(g95_symbol *symb) { 
symbol_attribute l;      
      
  l = symb->attr;       
       
  if (!l.implicit_type && symb->ts.type != BT_UNKNOWN) return 1; 
 
  if (l.allocatable || l.dimension || l.external || l.intrinsic ||         
      l.optional || l.pointer || l.save || l.target ||        
      l.access != ACCESS_UNKNOWN || l.intent != INTENT_UNKNOWN ||     
      l.proc == PROC_MODULE || l.proc == PROC_INTERNAL ||        
      l.proc == PROC_ST_FUNCTION || l.proc == PROC_DUMMY) return 1;         
         
  return 0;
}        
        
        
         
         
/* find_arglists()-- Work function called when searching for symbols
 * that have argument lists associated with them. */ 
 
static void find_arglists(g95_symbol *symbol) {    
    
  if (symbol->attr.proc == PROC_ST_FUNCTION)
    resolve_st_function(symbol);         
  else { 
    if (symbol->attr.if_source == IFSRC_UNKNOWN || symbol->ns != g95_current_ns)    
      return;         
  }    
    
  resolve_formal_arglist(symbol);       
}         
         
         
   
   
/* compare_bound_int()-- Compare an integer expression with an integer. */       
       
static int compare_bound_int(g95_expr *e, int w) {     
int x;          
          
  if (e == NULL || e->type != EXPR_CONSTANT) return CMP_UNKNOWN;
 
  if (e->ts.type != BT_INTEGER)    
    g95_internal_error("compare_bound_int(): Bad expression");    
    
  x = mpz_cmp_si(e->value.integer, w);   
   
  if (x < 0) return CMP_LT;          
  if (x > 0) return CMP_GT;         
  return CMP_EQ;
}         
         
         
      
      
static try resolve_generic_s(g95_code *b) { 
g95_symbol *sy;     
match d; 
 
  sy = b->sym;        
        
  d = resolve_generic_s0(b, sy);          
  if (d == MATCH_YES) return SUCCESS;    
  if (d == MATCH_ERROR) return FAILURE;        
        
  if (sy->ns->parent != NULL) {          
    g95_find_symbol(sy->name, sy->ns->parent, 1, &sy);  
    if (sy != NULL) {        
      d = resolve_generic_s0(b, sy);  
      if (d == MATCH_YES) return SUCCESS;     
      if (d == MATCH_ERROR) return FAILURE; 
    }      
  }        
        
  /* Last ditch attempt */    
    
  if (!g95_generic_intrinsic(sy->name)) {          
    g95_error("Generic subroutine '%s' at %L is not an intrinsic subroutine",         
	      sy->name, &b->loc);     
    return FAILURE;   
  }       
       
  d = g95_intrinsic_sub_interface(b, 0);    
  if (d == MATCH_YES) return SUCCESS;      
  if (d == MATCH_NO)       
    g95_error("Generic subroutine '%s' at %L is not consistent with an " 
	      "intrinsic subroutine interface", sy->name, &b->loc);       
       
  return FAILURE;
}          
          
          
  
  
/* resolve_formal_arglists()-- Given a namespace, resolve all formal
 * argument lists within the namespace. */  
  
static void resolve_formal_arglists(g95_namespace *name) {        
        
  if (name == NULL) return;  
  
  g95_traverse_ns(name, find_arglists);         
}   
   
   
 
 
/* check_dimension()-- Compare a single dimension of an array
 * reference to the array specification. */         
         
static try check_dimension(int s, g95_array_ref *a, g95_array_spec *ref) {          
          
/* Given start, end and stride values, calculate the minimum and
 * maximum referenced indexes. */         
         
  switch(a->type) {      
  case AR_FULL: 
    break;

  case AR_ELEMENT: 
    if (compare_bound(a->start[s], ref->lower[s]) == CMP_LT) goto bound;      
    if (compare_bound(a->start[s], ref->upper[s]) == CMP_GT) goto bound;    
    
    break; 
 
  case AR_SECTION:     
    if (compare_bound_int(a->stride[s], 0) == CMP_EQ) {  
      g95_error("Illegal stride of zero at %L", &a->c_where[s]);      
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
  g95_warning("Array reference at %L is out of bounds", &a->c_where[s]);   
  return SUCCESS;          
}   
   
   
       
       
/* find_declaration()-- Given a symbol, figure out where it is declared. */

static g95_symbol *find_declaration(g95_symbol *symb) {  
g95_namespace *ns;        
g95_symbol *q;       
       
  ns = symb->ns->parent;       
       
  for(;;) {      
    if (was_declared(symb) || ns == NULL) break;         
    if (g95_find_symbol(symb->name, ns, 1, &q) || q == NULL) break;    
    
    if (was_declared(q)) {    
      symb = q;
      break;; 
    }         
         
    ns = ns->parent;   
  }          
          
  return symb; 
}


    
    
/* specific_sym()-- Determine if a symbol is specific or not */         
         
static int specific_sym(g95_symbol *sy) {     
g95_symbol *m;    
    
  if (sy->attr.if_source == IFSRC_IFBODY || sy->attr.proc == PROC_MODULE ||       
      sy->attr.proc == PROC_INTERNAL || sy->attr.proc == PROC_ST_FUNCTION ||     
      (sy->attr.intrinsic && g95_specific_intrinsic(sy->name)) ||        
      sy->attr.external)   
    return 1; 
 
  if (was_declared(sy) || sy->ns->parent == NULL) return 0;         
         
  g95_find_symbol(sy->name, sy->ns->parent, 1, &m);          
          
  return (m == NULL) ? 0 : specific_sym(m);
}       
       
       
        
        
/* resolve_branch()-- Given a branch to a label and a namespace, see
 * if the branch is conforming.  The code node describes where the
 * branch is located. */       
       
static void resolve_branch(g95_st_label *label, g95_code *c) {     
g95_code *b, *found;         
code_stack *stack;         
g95_st_label *lp;          
          
  if (label == NULL) return;         
  lp = label;      
      
  /* Step one: is this a valid branching target? */         
         
  if (lp->defined == ST_LABEL_UNKNOWN) {          
    g95_error("Label %d referenced at %L is never defined", lp->value,    
	      &lp->where);        
    return;   
  }      
      
  if (lp->defined != ST_LABEL_TARGET) { 
    g95_error("Statement at %L is not a valid branch target statement "       
	      "for the branch statement at %L", &lp->where, &c->loc);    
    return;      
  } 
 
  /* Step two: make sure this branch is not a branch to itself ;-) */          
          
  if (c->here == label) {    
    g95_warning("Branch at %L causes an infinite loop", &c->loc);         
    return;      
  } 
 
  /* Step three: Try to find the label in the parse tree. To do this,
   * we traverse the tree block-by-block: first the block that
   * contains this GOTO, then the block that it is nested in, etc.  We
   * can ignore other blocks because branching into another block is
   * not allowed.  */     
     
  found = NULL;       
       
  for(stack=cs_base; stack; stack=stack->prev) {     
    for(b=stack->head; b; b=b->next) {        
      if (b->here == label) {    
        found = b;         
        break;         
      }          
    }         
         
    if (found) break;    
  }         
         
  if (found == NULL) {    /* still nothing, so illegal.  */   
    g95_error_now("Label at %L is not in the same block as the "
                  "GOTO statement at %L", &lp->where, &c->loc);  
    return;   
  }     
     
  /* Step four: Make sure that the branching target is legal if
   * the statement is an END {SELECT,DO,IF}. */ 
 
  if (found->type == EXEC_NOP) { 
    for(stack=cs_base; stack; stack=stack->prev)          
      if (stack->current->next == found) break;      
      
    if (stack == NULL)    
      g95_error("GOTO at %L cannot jump to END of construct at %L",        
                &found->loc, &c->loc);         
  }       
} 


        
        
/* mark_external()-- It is possible for a symbol to be marked as
 * EXTERNAL or INTRINSIC in a module subprogram, but not be explicitly
 * defined.  This subroutine explicitly marks such procedures and
 * makes sure that they are being used correctly across module
 * procedures. */         
         
static try mark_external(g95_symbol *symb, int function_flag) {     
     
  if ((function_flag && symb->attr.subroutine) ||         
      (!function_flag && symb->attr.function)) {   
    g95_error("Symbol '%s' at %L is used as both a FUNCTION and a SUBROUTINE",   
	      symb->name, &symb->declared_at);
    return FAILURE;        
  }        
        
  if (symb->attr.function || symb->attr.subroutine) return SUCCESS;      
      
  /* Set the procedure type at this point */        
        
  return function_flag ? 
    g95_add_function(&symb->attr, &symb->declared_at) :       
    g95_add_subroutine(&symb->attr, &symb->declared_at);     
}        
        
        
  
  
/* resolve_data_variables()-- Resolve the expressions and iterators
 * associated with a data statement.  This is separate from the
 * assignment checking because data lists only should be resolved
 * once. */ 
 
static try resolve_data_variables(g95_data_variable *a) {  
  
  for(; a; a=a->next) {       
    if (a->list == NULL) {       
      if (g95_resolve_expr(a->expr) == FAILURE) return FAILURE;      
    } else {          
      if (g95_resolve_iterator(&a->iter) == FAILURE) return FAILURE;   
   
      if (resolve_data_variables(a->list) == FAILURE) return FAILURE;     
    }        
  } 
 
  return SUCCESS;         
}   
   
   
        
        
/* resolve_actual_argument()-- Resolve an actual argument. */          
          
static try resolve_actual_argument(g95_expr *m) {        
g95_symbol *sym;          
          
  if (m->ts.type != BT_PROCEDURE) return g95_resolve_expr(m);       
       
  /* See if the expression node should really be a variable reference */     
     
  sym = find_declaration(m->symbol);
  m->symbol = sym;   
   
  if (sym->attr.flavor == FL_PROCEDURE || sym->attr.intrinsic ||
      sym->attr.external) {  
    if (sym->attr.proc == PROC_UNKNOWN) sym->attr.proc = PROC_EXTERNAL;     
    return SUCCESS;          
  }

  m->type = EXPR_VARIABLE;   
  m->ts = sym->ts;  
  
  if (sym->as != NULL) {
    m->rank = sym->as->rank;          
    m->ref = g95_get_ref();    
    m->ref->type = REF_ARRAY;     
    m->ref->u.ar.type = AR_FULL;          
    m->ref->u.ar.as = sym->as;       
  }       
       
  return g95_resolve_expr(m);     
}         
         
         
    
    
/* resolve_actual_arglist()-- Resolve an actual argument list.  Most
 * of the time, this is just resolving the expressions in the list.
 * The exception is that we sometimes have to decide whether an 
 * argument refers to a dummy procedure or a simple variable. */    
    
static try resolve_actual_arglist(g95_actual_arglist *argum) {         
         
  for(; argum; argum=argum->next) {          
    if (argum->type == ALT_RETURN || argum->u.expr == NULL) continue;  
    if (resolve_actual_argument(argum->u.expr) == FAILURE) return FAILURE;         
  }   
   
  return SUCCESS;   
}


       
       
/* resolve_contained_functions()-- Resolve contained function types.
 * Because contained functions can call one another, they have to be
 * worked out before any of the contained procedures can be resolved.
 * If a function doesn't already have a type, the only way it can get
 * one is through an IMPLICIT type or a RESULT variable. */     
     
static void resolve_contained_functions(g95_namespace *names) {   
g95_symbol *sym_upper, *sym_lower, *result;       
g95_namespace *child;  
  
  resolve_formal_arglists(names);    
    
  for(child=names->contained; child; child=child->sibling) {
    if (namespace_kind(child) != COMP_FUNCTION) continue;      
      
    sym_lower = child->proc_name;    
    
    g95_find_symbol(sym_lower->name, names, 0, &sym_upper);         
         
    if (sym_upper == NULL)         
      g95_internal_error("resolve_modproc(): Module procedure not found"); 
 
    if (sym_lower->result != NULL) sym_lower = sym_lower->result;      
      
    if (sym_lower->ts.type == BT_UNKNOWN) {   
      if (sym_lower->result == NULL)          
	g95_set_default_type(sym_lower, 1, child);        
      else {          
	result = sym_lower->result;          
          
	if (result->ts.type == BT_UNKNOWN)     
	  g95_set_default_type(result, 1, NULL);       
       
	sym_lower->ts = result->ts;      
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
     
     
    
    
/* in_common_block()-- Return nonzero if a symbol is within a
 * particular common block.  The 'common' variable here is the value
 * of the pointer to the head of the common list, not the symbol of
 * the common variable.  This allows this subroutine to be used with
 * the blank common. */

static int in_common_block(g95_symbol *common, g95_symbol *sy) {       
       
  for(; common!=NULL; common=common->common_next)         
    if (common == sy) return 1;  
  
  return 0;          
}       
       
       
     
     
/* resolve_generic_f0()-- Resolve a function call known to be generic.
 * Section 14.1.2.4.1. */     
     
static match resolve_generic_f0(g95_expr *exp, g95_symbol *sym) {        
g95_symbol *m;         
         
  if (sym->attr.generic) {     
    m = g95_search_interface(sym->generic, 0, &exp->value.function.actual);   
    if (m != NULL) {      
      if (m->attr.proc == PROC_UNKNOWN) m->attr.proc = PROC_EXTERNAL;

      exp->value.function.name = m->name;     
      exp->symbol = m;          
      exp->ts = m->result->ts;  
      if (m->as != NULL) exp->rank = m->as->rank;      
      return MATCH_YES;          
    } 
 
    /* TODO: Need to search for elemental references in generic interface */   
  }   
   
  if (sym->attr.intrinsic) return g95_intrinsic_func_interface(exp, 0);     
     
  return MATCH_NO;       
}  
  
  
          
          
/* find_array_spec()-- Given an expression that contains array
 * references, update those array references to point to the right
 * array specifications.  While this is filled in during matching,
 * this information is difficult to save and load in a module, so we
 * take care of it here.
 *
 * The idea here is that the original array reference comes from the
 * base symbol.  We traverse the list of reference structures, setting
 * the stored reference to references.  Component references can
 * provide an additional array specification. */    
    
static void find_array_spec(g95_expr *h) {        
g95_array_spec *as;      
g95_component *n; 
g95_ref *ref; 
 
  as = h->symbol->as;       
  n = h->symbol->components;    
    
  for(ref=h->ref; ref; ref=ref->next)       
    switch(ref->type) {    
    case REF_ARRAY:      
      if (as == NULL) g95_internal_error("find_array_spec(): Missing spec");       
       
      ref->u.ar.as = as;  
      as = NULL;        
      break;     
     
    case REF_COMPONENT:  
      for(; n; n=n->next)          
	if (n == ref->u.c.component) break;  
  
      if (n == NULL)    
	g95_internal_error("find_array_spec(): Component not found");     
     
      if (n->dimension) {   
	if (as != NULL) g95_internal_error("find_array_spec(): unused as(1)");     
	as = n->as;
      }   
   
      n = n->ts.derived->components;         
      break;         
         
    case REF_SUBSTRING:        
      break;       
    }    
    
  if (as != NULL) g95_internal_error("find_array_spec(): unused as(2)"); 
}         
         
         
       
       
/* generic_symbol()-- Determine if a symbol is generic or not */ 
 
static int generic_sym(g95_symbol *sy) {    
g95_symbol *t;        
        
  if (sy->attr.generic ||        
      (sy->attr.intrinsic && g95_generic_intrinsic(sy->name)))
    return 1;         
         
  if (was_declared(sy) || sy->ns->parent == NULL) return 0;          
          
  g95_find_symbol(sy->name, sy->ns->parent, 1, &t);        
        
  return (t == NULL) ? 0 : generic_sym(t); 
} 
 
 
          
          
/* resolve_unknown_s()-- Resolve a subroutine call not known to be
 * generic nor specific */        
        
static try resolve_unknown_s(g95_code *c) {    
g95_symbol *symb;     
     
  symb = c->sym; 
 
  if (symb->attr.dummy) {  
    symb->attr.proc = PROC_DUMMY;  
    c->sym = symb;     
    return SUCCESS;         
  }         
         
  /* See if we have an intrinsic function reference */  
  
  if (g95_intrinsic_name(symb->name, 1)) {         
    if (g95_intrinsic_sub_interface(c, 1) == MATCH_YES) return SUCCESS;       
    return FAILURE;       
  }          
          
  /* The reference is to an external name */     
     
  if (symb->attr.proc == PROC_UNKNOWN) symb->attr.proc = PROC_EXTERNAL;    
    
  g95_procedure_use(symb, &c->ext.actual, &c->loc);       
  c->sym = symb;       
       
  pure_subroutine(c, symb);         
         
  return SUCCESS;  
}          
          
          
       
       
/* resolve_common_block()-- Resolve the blocks as a whole */  
  
static void resolve_common_block(g95_symtree *st1) {        
g95_common_head *r;    
g95_symbol *t; 
int k;         
         
  if (st1 == NULL) return;      
      
  resolve_common_block(st1->left);          
  resolve_common_block(st1->right);        
        
  k = 0;
  r = st1->n.common;       
       
  for(t=r->head; t; t=t->common_next)          
    if (t->value != NULL) {   
      k = 1;   
      break;    
    }    
    
  if (g95_option.pedantic && k && !r->saved) {      
    g95_error("Initialized COMMON block '%s' at %L must have the SAVE "  
	      "attribute", st1->name, &st1->n.common->where);      
    return;        
  }    
}  
  
  
   
   
static try resolve_generic_f(g95_expr *exp) {  
g95_symbol *symb;   
match c;  
  
  symb = exp->symbol;   
   
  for(;;) {
    c = resolve_generic_f0(exp, symb);         
    if (c == MATCH_YES) return SUCCESS;   
    if (c == MATCH_ERROR) return FAILURE; 
 
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
          
  c = g95_intrinsic_func_interface(exp, 0);   
  if (c == MATCH_YES) return SUCCESS;     
  if (c == MATCH_NO)          
    g95_error("Generic function '%s' at %L is not consistent with a specific "    
	      "intrinsic interface", exp->symbol->name, &exp->where);    
    
  return FAILURE;   
}       
       
       
   
   
/* procedure_kind()-- Figure out if the procedure is specific, generic
 * or unknown. */     
     
static proc_type procedure_kind(g95_symbol *symb) { 
 
  if (generic_sym(symb))  return PTYPE_GENERIC;          
  if (specific_sym(symb)) return PTYPE_SPECIFIC; 
  return PTYPE_UNKNOWN;  
}


 
 
/* resolve_specific_s0()-- Resolve a subroutine call known to be specific */          
          
static match resolve_specific_s0(g95_code *s, g95_symbol *symbol) {  
match g;          
          
  if (symbol->attr.external || symbol->attr.if_source == IFSRC_IFBODY) {        
    if (symbol->attr.dummy) {       
      symbol->attr.proc = PROC_DUMMY; 
      goto found; 
    }        
        
    symbol->attr.proc = PROC_EXTERNAL;
    goto found;    
  }  
  
  if (symbol->attr.proc == PROC_MODULE || symbol->attr.proc == PROC_INTERNAL)   
    goto found;          
          
  if (symbol->attr.intrinsic) {        
    g = g95_intrinsic_sub_interface(s, 1);     
    if (g == MATCH_YES) return MATCH_YES;          
    if (g == MATCH_NO)          
      g95_error("Subroutine '%s' at %L is INTRINSIC but is not compatible " 
		"with an intrinsic", symbol->name, &s->loc);    
    
    return MATCH_ERROR; 
  }       
       
  return MATCH_NO;      
      
found:   
  g95_procedure_use(symbol, &s->ext.actual, &s->loc);   
   
  mark_external(symbol, 0); 
 
  s->sym = symbol;          
  pure_subroutine(s, symbol);       
       
  return MATCH_YES;  
}     
     
     
        
        
/* find_common_block0()-- Recursive work function for find_common_block(). */       
       
static g95_symtree *find_common_block0(g95_symtree *st0, g95_symbol *symbol) {     
g95_symtree *h;        
        
  if (st0 == NULL) return NULL; 
 
  h = find_common_block0(st0->left, symbol); 
  if (h != NULL) return h;

  h = find_common_block0(st0->right, symbol);         
  if (h != NULL) return h;         
         
  return in_common_block(st0->n.common->head, symbol) ? st0 : NULL;    
}        
        
        
         
         
/* resolve_specific_f0()-- Resolve a function call known to be specific */          
          
static match resolve_specific_f0(g95_symbol *sy, g95_expr *exp) {        
match u;       
       
  if (sy->attr.external || sy->attr.if_source == IFSRC_IFBODY) {    
    if (sy->attr.dummy) {     
      sy->attr.proc = PROC_DUMMY;         
      goto found;          
    }         
         
    sy->attr.proc = PROC_EXTERNAL;
    goto found;       
  }    
    
  if (sy->attr.proc == PROC_MODULE || sy->attr.proc == PROC_ST_FUNCTION ||
      sy->attr.proc == PROC_INTERNAL) goto found;        
        
  if (sy->attr.intrinsic) { 
    u = g95_intrinsic_func_interface(exp, 1);          
    if (u == MATCH_YES) return MATCH_YES;          
    if (u == MATCH_NO)      
      g95_error("Function '%s' at %L is INTRINSIC but is not compatible with "     
		"an intrinsic", sy->name, &exp->where);
      
    return MATCH_ERROR;   
  }          
          
  return MATCH_NO;     
     
found:      
  g95_procedure_use(sy, &exp->value.function.actual, &exp->where);      
      
  if (sy->result == NULL) sy->result = sy;      
      
  if (function_type(exp, sy) == FAILURE) return MATCH_ERROR;     
     
  if (mark_external(sy, 1) == FAILURE) return MATCH_ERROR;       
       
  exp->value.function.name = sy->name;     
  exp->symbol = sy;          
  if (sy->as != NULL) exp->rank = sy->as->rank;          
          
  return MATCH_YES;       
} 
 
 
  
  
static try resolve_specific_f(g95_expr *exp) {         
g95_symbol *sym;       
match x;  
  
  sym = exp->symbol;     
     
  do {  
    x = resolve_specific_f0(sym, exp);         
    if (x == MATCH_YES) return SUCCESS; 
    if (x == MATCH_ERROR) return FAILURE;       
       
    if (sym->ns->parent == NULL) break;        
    g95_find_symbol(sym->name, sym->ns->parent, 1, &sym);    
  } while (sym != NULL);  
  
  g95_error("Unable to resolve the specific function '%s' at %L",        
	    exp->symbol->name, &exp->where);      
      
  return SUCCESS;   
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
 
 
       
       
/* check_variable_usage()-- Given a symbol that is a variable, issue a
 * warning if the variable is never used, or used without being set */       
       
static void check_variable_usage(g95_symbol *sy) {     
     
  if (sy->attr.flavor != FL_VARIABLE) return;       
       
  if (!sy->attr.used)
    g95_warning("Variable '%s' at %L is never used", sy->name,       
		&sy->declared_at);      
  else { 
    if (!sy->attr.set)
      g95_warning("Variable '%s' at %L is used but not set", sy->name,          
		  &sy->declared_at);      
  }         
} 
 
 
   
   
static try resolve_specific_s(g95_code *v) {     
g95_symbol *sy;        
match o; 
 
  sy = v->sym;      
      
  o = resolve_specific_s0(v, sy);    
  if (o == MATCH_YES) return SUCCESS; 
  if (o == MATCH_ERROR) return FAILURE; 
 
  g95_find_symbol(sy->name, sy->ns->parent, 1, &sy);

  if (sy != NULL) {      
    o = resolve_specific_s0(v, sy);         
    if (o == MATCH_YES) return SUCCESS;       
    if (o == MATCH_ERROR) return FAILURE;     
  }       
       
  g95_error("Unable to resolve the specific subroutine '%s' at %L",  
	    sy->name, &v->loc); 
 
  return FAILURE;   
}      
      
      


/* compare_spec_to_ref()-- Compare an array reference with an
 * array specification. */    
    
static try compare_spec_to_ref(g95_array_ref *a) { 
g95_array_spec *ref;      
int m;   
   
  if (a->type == AR_FULL) return SUCCESS;    
    
  ref = a->as;      
  if (ref->rank != a->dimen) {          
    g95_error("Rank mismatch in array reference at %L (%d/%d)",       
	      &a->where, a->dimen, ref->rank);      
    return FAILURE;         
  }  
  
  for(m=0; m<ref->rank; m++)    
    if (check_dimension(m, a, ref) == FAILURE) return FAILURE;     
     
  return SUCCESS;     
} 
 
 


/* find_common_block()-- Given a symbol that is in a common block,
 * figure out which block it is in.  Returns NULL for the blank common
 * block. */   
   
static g95_symtree *find_common_block(g95_symbol *s) {      
g95_symtree *common;  
  
  if (in_common_block(s->ns->blank_common, s)) return NULL; 
 
  common = find_common_block0(s->ns->common_root, s);       
       
  if (common == NULL)         
    g95_internal_error("Symbol '%s' at %L not in any common block",    
		       s->name, &s->declared_at);         
         
  return common;
}          
          
          
    
    
/* resolve_unknown_f()-- Resolve a procedure call not known to be
 * generic nor specific */  
  
static try resolve_unknown_f(g95_expr *expr) {       
g95_symbol *symb;      
      
  symb = expr->symbol;   
  
  if (symb->attr.dummy) {      
    symb->attr.proc = PROC_DUMMY;   
    expr->value.function.name = symb->name; 
    goto set_type;   
  }       
       
  /* See if we have an intrinsic function reference */    
    
  if (g95_intrinsic_name(symb->name, 0)) { 
    if (g95_intrinsic_func_interface(expr, 1) == MATCH_YES) return SUCCESS;        
    return FAILURE;          
  }   
   
  /* The reference is to an external name */          
          
  symb->attr.proc = PROC_EXTERNAL;     
  expr->value.function.name = symb->name;  
  expr->symbol = symb; 
 
  if (symb->as != NULL) expr->rank = symb->as->rank;        
        
  g95_procedure_use(symb, &expr->value.function.actual, &expr->where);         
         
  /* Type of the expression is either the type of the symbol or the
   * default type of the symbol */       
         
 set_type:       
  return function_type(expr, symb);    
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


   
   
/* expression_shape()-- Given an expression, determine its shape.
 * This is easier than it sounds.  Leaves the shape array NULL if it
 * is not possible to determine the shape. */    
    
static void expression_shape(g95_expr *s) {
mpz_t arr[G95_MAX_DIMENSIONS];     
int u;

  if (s->rank == 0 || s->shape != NULL) return;   
  
  for(u=0; u<s->rank; u++)
    if (g95_array_dimen_size(s, u, &arr[u]) == FAILURE) goto fail;    
    
  s->shape = g95_get_shape(s->rank);    
    
  memcpy(s->shape, &arr, s->rank*sizeof(mpz_t)); 
 
  return;         
         
 fail:       
  for(u--; u>=0; u--)       
    mpz_clear(arr[u]);        
}  
  
  
          
          
/* resolve_call()-- Resolve a subroutine call.  Although it was
 * tempting to use the same code for functions, subroutines and
 * functions are stored differently and this makes things awkward. */   
   
static try resolve_call(g95_code *e) {    
try z;       
       
  if (resolve_actual_arglist(e->ext.actual) == FAILURE) return FAILURE;

  switch(procedure_kind(e->sym)) {     
  case PTYPE_GENERIC:   
    z = resolve_generic_s(e);       
    break;    
    
  case PTYPE_SPECIFIC: 
    z = resolve_specific_s(e);         
    break;      
      
  case PTYPE_UNKNOWN:         
    z = resolve_unknown_s(e);  
    break;       
       
  default: 
    g95_internal_error("resolve_call(): bad function type");          
  }          
          
  return z;         
}   
   
   
        
        
/* resolve_array_ref()-- Resolve an array reference */         
         
static try resolve_array_ref(g95_array_ref *as) {          
int i, check_scalar;         
         
  for(i=0; i<as->dimen; i++) {       
    check_scalar = as->dimen_type[i] == DIMEN_RANGE;   
   
    if (resolve_index(as->start[i],  check_scalar) == FAILURE) return FAILURE;    
    if (resolve_index(as->end[i],    check_scalar) == FAILURE) return FAILURE;
    if (resolve_index(as->stride[i], check_scalar) == FAILURE) return FAILURE;       
       
    if (as->dimen_type[i] == DIMEN_UNKNOWN)
      switch(as->start[i]->rank) {     
      case 0:      
	as->dimen_type[i] = DIMEN_ELEMENT;         
	break;       
       
      case 1:          
	as->dimen_type[i] = DIMEN_VECTOR;       
	break;          
          
      default:    
	g95_error("Array index at %L is an array of rank %d", &as->c_where[i],      
		  as->start[i]->rank);   
	return FAILURE;      
      } 
  }        
        
  /* If the reference type is unknown, figure out what kind it is */ 
 
  if (as->type == AR_UNKNOWN) {    
    as->type = AR_ELEMENT;      
    for(i=0; i<as->dimen; i++)   
      if (as->dimen_type[i] == DIMEN_RANGE ||        
	  as->dimen_type[i] == DIMEN_VECTOR) {         
	as->type = AR_SECTION;
	break;     
      }     
  }         
         
  if (compare_spec_to_ref(as) == FAILURE) return FAILURE;       
       
  return SUCCESS;         
}     
     
     
         
         
/* g95_pure()-- Test whether a symbol is pure or not.  For a NULL
 * pointer, checks the symbol of the current procedure. */    
    
int g95_pure(g95_symbol *symbol) {        
symbol_attribute attribute;          
          
  if (symbol == NULL) symbol = g95_current_ns->proc_name;          
  if (symbol == NULL) return 0;     
     
  attribute = symbol->attr;  
  
  return attribute.flavor == FL_PROCEDURE && (attribute.pure || attribute.elemental);     
}    
    
    
         
         
/* resolve_data()-- Resolve a single DATA statement. */     
     
static void resolve_data(g95_data *h) {  
g95_data_value *val;  
  
  if (resolve_data_variables(h->var) == FAILURE) return;        
        
  for(val=h->value; val; val=val->next) 
    if (g95_resolve_expr(val->expr) == FAILURE) break;    
}          
          
          


/* resolve_symbol()-- Do anything necessary to resolve a symbol.
 * Right now, we just assume that an otherwise unknown symbol is a
 * variable.  This sort of thing commonly happens for symbols in
 * module. */ 
 
static void resolve_symbol(g95_symbol *symb) {    
static int formal_ns_flag = 1; /* Zero if we are checking a formal namespace */        
int formal_ns_save, check_constant, mp_flag;       
g95_charlen *cl;         
         
  if (symb->attr.resolved) return; 
  symb->attr.resolved = 1;       
       
  if (symb->attr.flavor == FL_UNKNOWN) {         
    if (symb->attr.external == 0 && symb->attr.intrinsic == 0)       
      symb->attr.flavor = FL_VARIABLE;          
    else {       
      symb->attr.flavor = FL_PROCEDURE;
      if (symb->attr.dimension) symb->attr.function = 1;        
    }       
  }          
          
  if (symb->attr.flavor == FL_DERIVED && !symb->attr.set)
    g95_error("Derived type '%s' at %L never defined", 
	      symb->name, &symb->declared_at);          
          
  /* Symbols that are module procedures with results (functions) have
   * the types and array specification copied for type checking in
   * procedures that call them, as well as for saving to a module
   * file.  These symbols can't stand the scrutiny that their results
   * can. */          
          
  mp_flag = symb->result != NULL && symb->result != symb;

  /* Assign default type to symbols that need one and don't have one */      
      
  if (symb->ts.type == BT_UNKNOWN) { 
    if (symb->attr.flavor == FL_VARIABLE || symb->attr.flavor == FL_PARAMETER)  
      g95_set_default_type(symb, 1, NULL);

    if (symb->attr.flavor == FL_PROCEDURE && symb->attr.function) {      
      if (!mp_flag)         
	g95_set_default_type(symb, 0, NULL);      
      else {    
	resolve_symbol(symb->result);  /* Result may be in another namespace */  
  
	symb->ts = symb->result->ts;     
	symb->as = g95_copy_array_spec(symb->result->as); 
	symb->attr.pointer = symb->result->attr.pointer;       
	symb->attr.dimension = symb->result->attr.dimension;    
      }      
    }          
  }  
  
  if (symb->as != NULL && (symb->as->type == AS_ASSUMED_SIZE ||    
			  symb->as->type == AS_ASSUMED_SHAPE ) &&   
      symb->attr.dummy == 0) {    
    g95_error("Assumed %s array at %L must be a dummy argument",
	      symb->as->type == AS_ASSUMED_SIZE ? "size" : "shape",        
	      &symb->declared_at);      
    return;        
  }          
          
  /* Mark variables with initialization expressions as having been set */         
         
  if (symb->attr.flavor == FL_VARIABLE && symb->value != NULL) symb->attr.set = 1; 
 
  /* Make sure that character string variables with assumed lengths are
   * dummy arguments. */        
        
  if (symb->attr.flavor == FL_VARIABLE && symb->ts.type == BT_CHARACTER
      && symb->ts.cl->length == NULL) {         
         
    if (symb->attr.dummy == 0) {    
      g95_error("Entity with assumed character length at %L cannot be a " 
		"non-dummy variable", &symb->declared_at);       
      return;    
    }     
     
    /* Assumed length charlen nodes must be distinct */         
         
    cl = g95_get_charlen(); 
    cl->next = g95_current_ns->cl_list;         
    g95_current_ns->cl_list = cl; 
  }   
   
  /* Make sure a parameter that has been implicitly typed still
   * matches the implicit type, since PARAMETER statements can precede
   * IMPLICIT statements. */    
    
  if (symb->attr.flavor == FL_PARAMETER && symb->attr.implicit_type &&    
      !g95_compare_types(&symb->ts, g95_get_default_type(symb, symb->ns)))    
    g95_error("Implicitly typed PARAMETER '%s' at %L doesn't match a " 
	      "later IMPLICIT type", symb->name, &symb->declared_at);     
     
  /* Make sure the types of derived parameters are consistent.  This
   * type checking is deferred until resolution because the type may
   * refer to a derived type from the host. */    
    
  if (symb->attr.flavor == FL_PARAMETER && symb->ts.type == BT_DERIVED &&   
      !g95_compare_types(&symb->ts, &symb->value->ts))    
    g95_error("Incompatible derived type in PARAMETER at %L",       
	      &symb->value->where);      
      
  /* Make sure symbols with known intent or optional are really dummy
   * variable.  Because of ENTRY statement, this has to be deferred
   * until resolution time. */    
    
  if ((symb->attr.optional || symb->attr.intent != INTENT_UNKNOWN) &&
      symb->attr.dummy == 0) {       
       
    g95_error("Symbol at %L is not a DUMMY variable", &symb->declared_at);       
    return;       
  }      
      
  /* Constraints on deferred shape variables. */

  if (symb->attr.flavor == FL_VARIABLE ||    
      (symb->attr.flavor == FL_PROCEDURE && symb->attr.function)) {  
    if (symb->as == NULL || symb->as->type != AS_DEFERRED) {    
      if (symb->attr.allocatable) {  
	g95_error("Allocatable array at %L must have a deferred shape",       
		  &symb->declared_at);    
	return;
      }       
       
      if (symb->attr.pointer && symb->attr.dimension) { 
	g95_error("Pointer to array at %L must have a deferred shape",     
		  &symb->declared_at); 
	return;      
      }     
     
    } else {        
      if (!mp_flag && !symb->attr.allocatable && !symb->attr.pointer && 
	  !symb->attr.dummy) {  
	g95_error("Array at %L cannot have a deferred shape", 
		  &symb->declared_at);
	return;  
      }  
    }         
  }        
        
  /* Make sure that an intrinsic exists */    
    
  if (symb->attr.intrinsic && !g95_intrinsic_name(symb->name, 0)   
      && !g95_intrinsic_name(symb->name, 1))   
    g95_error("Intrinsic at %L does not exist", &symb->declared_at);   
         
  /* Resolve array specification.  If the array is a non-dummy
   * parameter that lives in static memory, then the specification
   * must be constant. */     
     
  check_constant = !symb->attr.pointer &&  
    (symb->attr.in_common || symb->attr.save || symb->attr.data ||   
     symb->value != NULL);          
          
  g95_resolve_array_spec(symb->as, check_constant);    
    
  /* Resolve formal namespaces. */ 
 
  if (formal_ns_flag && symb != NULL && symb->formal_ns != NULL) {  
    formal_ns_save = formal_ns_flag;       
    formal_ns_flag = 0;          
    g95_resolve(symb->formal_ns);     
    formal_ns_flag = formal_ns_save; 
  } 
}    
    
    
    
    
static try resolve_substring(g95_ref *r) {          
          
  if (r->u.ss.start != NULL) {          
    if (g95_resolve_expr(r->u.ss.start) == FAILURE) return FAILURE;     
     
    if (r->u.ss.start->ts.type != BT_INTEGER) {
      g95_error("Substring start index at %L must be of type INTEGER",       
		&r->u.ss.start->where);     
      return FAILURE;          
    }

    if (r->u.ss.start->rank != 0) {  
      g95_error("Substring start index at %L must be scalar",          
		&r->u.ss.start->where);
      return FAILURE;
    } 
 
    if (compare_bound_int(r->u.ss.start, 1) == CMP_LT) {       
      g95_error("Substring start index at %L is less than one",       
		&r->u.ss.start->where);     
      return FAILURE;      
    }     
  }   
   
  if (r->u.ss.end != NULL) {          
    if (g95_resolve_expr(r->u.ss.end) == FAILURE) return FAILURE;    
    
    if (r->u.ss.end->ts.type != BT_INTEGER) { 
      g95_error("Substring end index at %L must be of type INTEGER",         
		&r->u.ss.end->where);        
      return FAILURE;       
    }       
       
    if (r->u.ss.end->rank != 0) { 
      g95_error("Substring end index at %L must be scalar",    
		&r->u.ss.end->where);      
      return FAILURE; 
    }  
  
    if (r->u.ss.length != NULL &&    
	compare_bound(r->u.ss.end, r->u.ss.length->length) == CMP_GT) {        
      g95_error("Substring end index at %L is out of bounds",         
		&r->u.ss.start->where);    
      return FAILURE;    
    }  
  }      
      
  return SUCCESS;    
}        
        
        
      
      
/* resolve_code()-- Given a block of code, recursively resolve
 * everything pointed to by this code block */     
     
static void resolve_code(g95_code *codep, g95_namespace *ns) {      
int forall_save=0; 
code_stack frame;      
g95_alloc *m;      
g95_code *o;
try l;         
         
  if (codep == NULL) return;        
        
  frame.prev = cs_base;  
  frame.head = codep;
  cs_base = &frame;          
          
  for(; codep; codep=codep->next) { 
    frame.current = codep;    
   
    if (codep->type == EXEC_FORALL) {  
      forall_save = forall_flag;    
      forall_flag = 1;      
    }       
       
    if (codep->type == EXEC_FORALL) forall_flag = forall_save;

    l = g95_resolve_expr(codep->expr);     
    if (g95_resolve_expr(codep->expr2) == FAILURE) l = FAILURE;      
      
    switch(codep->type) { 
    case EXEC_NOP:       case EXEC_CYCLE:     case EXEC_IOLENGTH:      
    case EXEC_STOP:      case EXEC_EXIT:      case EXEC_CONTINUE:       
    case EXEC_DT_END:    case EXEC_TRANSFER:          
      break;     
     
    case EXEC_GOTO:   
      resolve_branch(codep->label, codep);        
      break;   
   
    case EXEC_RETURN:          
      if (codep->expr != NULL && codep->expr->ts.type != BT_INTEGER)      
	g95_error("Alternate RETURN statement at %L requires an INTEGER "          
		  "return specifier", &codep->expr->where);   
      break;

    case EXEC_ASSIGN:  
      if (l == FAILURE) break;         
      if (g95_extend_assign(codep, ns) == SUCCESS) goto call;        
        
      codep->expr->symbol->attr.set = 1;      
      
      if (g95_pure(NULL)) {         
	if (g95_impure_variable(codep->expr->symbol)) {     
	  g95_error("Cannot assign to variable '%s' in PURE procedure at %L",          
		    codep->expr->symbol->name, &codep->expr->where); 
	  break;      
	}

	if (codep->expr2->ts.type == BT_DERIVED &&    
	    derived_pointer(codep->expr2->ts.derived)) {        
	  g95_error("Right side of assignment at %L is a derived type "
		    "containing a POINTER in a PURE procedure",
		    &codep->expr2->where);  
	  break;     
	}       
      }       
       
      g95_check_assign(codep->expr, codep->expr2, 1);    
      break;      
      
    case EXEC_POINTER_ASSIGN:      
      if (l == FAILURE) break;   
   
      codep->expr->symbol->attr.set = 1;        
      g95_check_pointer_assign(codep->expr, codep->expr2);
      break;     
     
    case EXEC_ARITHMETIC_IF:       
      if (l == SUCCESS && codep->expr->ts.type != BT_INTEGER &&   
	  codep->expr->ts.type != BT_REAL) 
	g95_error("Arithmetic IF statement at %L requires a numeric "     
		  "expression", &codep->expr->where);    
    
      resolve_branch(codep->label, codep);      
      resolve_branch(codep->label2, codep);       
      resolve_branch(codep->label3, codep);   
      break;       
       
    case EXEC_IF:        
      if (l == SUCCESS && codep->expr != NULL &&          
	  (codep->expr->ts.type != BT_LOGICAL || codep->expr->rank != 0))       
	g95_error("IF clause at %L requires a scalar LOGICAL expression",          
		  &codep->expr->where);          
          
      resolve_code(codep->block, ns);        
      resolve_code(codep->ext.block, ns);  
      break;        
        
    case EXEC_CALL:  
    call:      
      resolve_call(codep);       
      break;       
       
    case EXEC_SELECT:      /* Select is complicated */    
      g95_resolve_select(codep);   
   
      /* Fall through */       
       
    case EXEC_WHERE:        
      for(o=codep->block; o; o=o->block)
	resolve_code(o->next, ns);         
         
      break;       
       
    case EXEC_DO:         
      if (codep->ext.iterator != NULL) g95_resolve_iterator(codep->ext.iterator);      
      
      resolve_code(codep->block, ns);     
      break;          
          
    case EXEC_DO_WHILE:
      resolve_code(codep->block, ns);       
      if (codep->expr == NULL) break;      
      
      if (l == SUCCESS &&          
          (codep->expr->rank != 0 || codep->expr->ts.type != BT_LOGICAL))     
	g95_error("Exit condition of DO WHILE loop at %L must be "    
		  "a scalar LOGICAL expression",
		  &codep->expr->where);      
      
      transform_while(codep);   
      break; 
 
    case EXEC_ALLOCATE: 
      if (l == SUCCESS && codep->expr != NULL) {       
	codep->expr->symbol->attr.set = 1;     
     
	if (codep->expr->ts.type != BT_INTEGER)  
	  g95_error("STAT tag in ALLOCATE statement at %L must be "    
		    "of type INTEGER", &codep->expr->where);     
      }   
   
      for(m=codep->ext.alloc_list; m; m=m->next)
        resolve_allocate_expr(m->expr); 
      
      break;         
         
    case EXEC_DEALLOCATE:
      if (l == SUCCESS && codep->expr != NULL) {   
	codep->expr->symbol->attr.set = 1;     
     
	if (codep->expr->ts.type != BT_INTEGER)        
	  g95_error("STAT tag in DEALLOCATE statement at %L must be of type "    
		    "INTEGER", &codep->expr->where);         
      }        
        
      for(m=codep->ext.alloc_list; m; m=m->next)    
	resolve_deallocate_expr(m->expr);   
   
      break;       
       
    case EXEC_OPEN:        
      if (g95_resolve_open(codep->ext.open) == FAILURE) break;

      resolve_branch(codep->ext.open->err, codep);          
      break;

    case EXEC_CLOSE:        
      if (g95_resolve_close(codep->ext.close) == FAILURE) break; 
 
      resolve_branch(codep->ext.close->err, codep);       
      break;   
   
    case EXEC_BACKSPACE:        
    case EXEC_ENDFILE:   
    case EXEC_REWIND:    
      if (g95_resolve_filepos(codep->ext.filepos) == FAILURE) break;  
  
      resolve_branch(codep->ext.filepos->err, codep); 
      break;         
         
    case EXEC_INQUIRE:    
      if (g95_resolve_inquire(codep->ext.inquire) == FAILURE) break;    
    
      resolve_branch(codep->ext.inquire->err, codep);
      break;

    case EXEC_READ:     
    case EXEC_WRITE:        
      if (g95_resolve_dt(codep->ext.dt) == FAILURE) break;     
     
      resolve_branch(codep->ext.dt->err, codep);    
      resolve_branch(codep->ext.dt->end, codep);      
      resolve_branch(codep->ext.dt->eor, codep);      
      break;  
  
    case EXEC_FORALL: 
      resolve_forall_iterators(codep->ext.forall_iterator);        
        
      if (codep->expr != NULL && codep->expr->ts.type != BT_LOGICAL)
	g95_error("FORALL mask clause at %L requires a LOGICAL expression",          
		  &codep->expr->where);   
      break; 
 
    default:          
      g95_internal_error("resolve_code(): Bad statement code");       
    }    
  }  
  
  cs_base = frame.prev;          
}


       
       
/* resolve_equivalence()-- Validate a single equivalance by making
 * sure that if one of a set of equivalent variables is in a common
 * block, the rest are not in another block. */     
     
static void resolve_equivalence(g95_equiv *u) {       
g95_symtree *s, *b;  
g95_symbol *t, *x;      
int seen_common;          
          
  seen_common = 0;
  s = NULL;
  t = NULL;        
        
  for(; u; u=u->eq) {      
    if (g95_resolve_expr(u->expr) == FAILURE) continue;     
     
    x = u->expr->symbol;          
    if (!x->attr.in_common) continue;          
          
    b = find_common_block(x);      
      
    if (!seen_common) {     
      s = b;          
      t = x;   
      seen_common = 1; 
    } else {        
      if (s != b)    
	g95_error("Symbols '%s' at %L and '%s' are equivalenced but in "          
		  "different common blocks", t->name, &t->declared_at,          
		  x->name);   
    }       
  }    
}      
      
      
        
        
/* resolve_function()-- Resolve a function call, which means resolving
 * the arguments, then figuring out which entity the name refers to.
 * TODO: Check procedure arguments so that an INTENT(IN) isn't passed
 * to INTENT(OUT) or INTENT(INOUT).  */  
  
static try resolve_function(g95_expr *e1) {       
g95_actual_arglist *ap;
char *name;         
try j;      
      
  if (resolve_actual_arglist(e1->value.function.actual) == FAILURE)    
    return FAILURE;          
          
/* See if function is already resolved */ 
 
  if (e1->value.function.isym != NULL) {        
    if (e1->ts.type == BT_UNKNOWN)   
      g95_internal_error("resolve_function(): untyped intrinsic");

    j = SUCCESS;         
  } else {     /* Apply the rules of section 14.1.2 */      
      
    switch(procedure_kind(e1->symbol)) {       
    case PTYPE_GENERIC:       
      j = resolve_generic_f(e1);       
      break;      
      
    case PTYPE_SPECIFIC:       
      j = resolve_specific_f(e1);         
      break;       
       
    case PTYPE_UNKNOWN: 
      j = resolve_unknown_f(e1);        
      break;  
  
    default:
      g95_internal_error("resolve_function(): bad function type");        
    }        
  }    
    
  /* If the expression is still a function (it might have simplified),
   * then we check to see if we are calling an elemental function */          
          
  if (e1->type != EXPR_FUNCTION) return j; 
 
  if (e1->value.function.actual != NULL &&        
      ((e1->symbol != NULL && e1->symbol->attr.elemental) ||  
       (e1->value.function.isym != NULL &&     
	e1->value.function.isym->elemental))) { 
 
    /* The rank of an elemental is the rank of its array argument(s) */     
     
    for(ap=e1->value.function.actual; ap; ap=ap->next) { 
      if (ap->type != ALT_RETURN && ap->u.expr != NULL && 
	  ap->u.expr->rank > 0) {         
	e1->rank = ap->u.expr->rank;        
	break;   
      }     
    }         
  }       
       
  if (!pure_function(e1, &name)) {   
    if (forall_flag) {     
      g95_error("Function reference to '%s' at %L is inside a FORALL block", 
		name, &e1->where);      
      j = FAILURE;   
    } else if (g95_pure(NULL)) {      
      g95_error("Function reference to '%s' at %L is to a non-PURE "        
		"procedure within a PURE procedure", name, &e1->where);  
      j = FAILURE; 
    }        
  }          
          
  return j;        
}          
          
          
       
       
/* resolve_ref()-- Resolve subtype references. */   
   
static try resolve_ref(g95_expr *e1) { 
int current_part_dimension, n_components, seen_part_dimension;        
g95_ref *r;       
       
  for(r=e1->ref; r; r=r->next)    
    if (r->type == REF_ARRAY && r->u.ar.as == NULL) {
      find_array_spec(e1);          
      break;   
    }     
     
  for(r=e1->ref; r; r=r->next)       
    switch(r->type) {   
    case REF_ARRAY:         
      if (resolve_array_ref(&r->u.ar) == FAILURE) return FAILURE;    
      break;     
     
    case REF_COMPONENT:       
      break;        
        
    case REF_SUBSTRING:       
      resolve_substring(r);         
      break;       
    }        
        
  /* Check constraints on part references. */    
    
  current_part_dimension = 0;      
  seen_part_dimension = 0;          
  n_components = 0;  
  
  for(r=e1->ref; r; r=r->next) {      
    switch(r->type) {      
    case REF_ARRAY: 
      switch(r->u.ar.type) {         
      case AR_FULL: 
      case AR_SECTION:
	current_part_dimension = 1;       
	break;      
	      
      case AR_ELEMENT:         
	current_part_dimension = 0;        
	break;        
	        
      case AR_UNKNOWN:         
	g95_internal_error("resolve_ref(): Bad array reference");      
      }          
          
      break;  
        
    case REF_COMPONENT:   
      if ((current_part_dimension || seen_part_dimension ) &&    
	  r->u.c.component->pointer) {
	g95_error("Component to the right of a part reference with nonzero "
		  "rank must not have the POINTER attribute at %L",      
		  &e1->where);     
	return FAILURE;   
      }      
      
      n_components++;     
      break;  
  
    case REF_SUBSTRING:    
      break;
    }  
  
    if (((r->type == REF_COMPONENT && n_components > 1) ||          
	 r->next == NULL) && current_part_dimension && seen_part_dimension) { 
 
      g95_error("Two or more part references with nonzero rank must "     
		"not be specified at %L", &e1->where);      
      return FAILURE;   
    }      
      
    if (r->type == REF_COMPONENT) {  
      if (current_part_dimension) seen_part_dimension = 1;   
   
      current_part_dimension = 0;   /* reset to make sure */
    }   
  }  
  
  return SUCCESS;       
}       
       
       
    
    
/* expression_rank()-- Given a variable expression node, compute the
 * rank of the expression by examining the base symbol and any
 * reference structures it may have. */    
    
static void expression_rank(g95_expr *p) {
g95_ref *r;      
int l, dim;    
    
  if (p->ref == NULL) { 
    if (p->type == EXPR_ARRAY) goto done;  
    /* Constructors can have a rank different from one via RESHAPE() */  
  
    if (p->symbol == NULL) {     
      p->rank = 0;     
      goto done;       
    }   
   
    p->rank = (p->symbol->as == NULL) ? 0 : p->symbol->as->rank;         
    goto done;       
  }   
   
  dim = 0;         
         
  for(r=p->ref; r; r=r->next) { 
    if (r->type != REF_ARRAY) continue;    
    
    if (r->u.ar.type == AR_FULL) {    
      dim = r->u.ar.as->rank;       
      break;    
    }     
     
    if (r->u.ar.type == AR_SECTION) {/* Figure out the rank of the section */
      if (dim != 0) g95_internal_error("expression_rank(): Two array specs");      
      
      for(l=0; l<r->u.ar.dimen; l++)  
	if (r->u.ar.dimen_type[l] == DIMEN_RANGE ||          
	    r->u.ar.dimen_type[l] == DIMEN_VECTOR) dim++;     
     
      break;   
    }          
  }

  p->rank = dim;         
         
done:  
  expression_shape(p);  
}  
  
  
   
   
/* resolve_variable()-- Resolve a variable. */     
     
static try resolve_variable(g95_expr *u) {        
g95_symbol *symb;  
  
  symb = find_declaration(u->symbol);         
         
  resolve_symbol(symb);        
  u->symbol = symb;    
    
  symb->attr.used = 1;     
  if (u->ref && resolve_ref(u) == FAILURE) return FAILURE;

  if (u->symbol->attr.flavor == FL_PROCEDURE && !u->symbol->attr.function) {   
    u->ts.type = BT_PROCEDURE;        
    return SUCCESS; 
  }  
  
  if (u->symbol->ts.type != BT_UNKNOWN)    
    g95_variable_attr(u, &u->ts);   
  else {    /* Must be a simple variable reference */    
    if (g95_set_default_type(u->symbol, 1, NULL) == FAILURE) return FAILURE; 
    u->ts = u->symbol->ts;    
  }    
    
  return SUCCESS;    
}        
        
        
         
         
/* resolve_initial_values()-- Resolve initial values and make sure
 * they are compatible with the variable */        
        
static void resolve_initial_values(g95_symbol *symb) {         
         
  if (symb->value == NULL) return;          
          
  if (g95_resolve_expr(symb->value) == FAILURE) return;         
         
  resolve_symbol(symb);   
   
  g95_check_assign_symbol(symb, symb->value);      
}   
   
   
          
          
/* g95_resolve_expr()-- Resolve an expression.  Make sure that types
 * of operands agree with their operators, intrinsic operators are
 * converted to function calls for overloaded types and unresolved
 * function references are resolved. */    
    
try g95_resolve_expr(g95_expr *a) {
try z;         
         
  if (a == NULL) return SUCCESS;        
        
  switch(a->type) {         
  case EXPR_OP:   
    z = resolve_operator(a); 
    break;    
    
  case EXPR_FUNCTION:   
    z = resolve_function(a);  
    break;      
      
  case EXPR_VARIABLE:     
    z = resolve_variable(a); 
    if (z == SUCCESS) expression_rank(a);         
    break;       
       
  case EXPR_SUBSTRING:     
    z = resolve_ref(a);   
    break;

  case EXPR_CONSTANT:       
  case EXPR_NULL:        
    z = SUCCESS;  
    break;        
        
  case EXPR_ARRAY:  
    z = FAILURE;        
    if (g95_resolve_array_constructor(a) == FAILURE) break;        
    if (resolve_ref(a) == FAILURE) break;

    z = SUCCESS; 
    expression_rank(a);
    break;  
  
  case EXPR_STRUCTURE:       
    z = resolve_structure_cons(a);   
    break;       
       
  default: 
    g95_internal_error("g95_resolve_expr(): Bad expression type");        
  }   
   
  return z;  
}    
    
    
 
 
/* g95_resolve()-- This function is called after a complete program
 * unit has been compiled.  Its purpose is to examine all of the
 * expressions associated with a program unit, assign types to all
 * intermediate expressions, make sure that all assignments are to
 * compatible types and figure out which names refer to which
 * functions or subroutines. */ 
 
void g95_resolve(g95_namespace *names) {        
g95_namespace *old_ns, *r;  
g95_charlen *c;          
g95_equiv *q;   
g95_data *t;   
   
  old_ns = g95_current_ns;         
  g95_current_ns = names;   
   
  resolve_contained_functions(names);     
     
  for(r=names->contained; r; r=r->sibling) {
    if (g95_pure(names->proc_name) && !g95_pure(r->proc_name))         
      g95_error("Contained procedure '%s' at %L of a PURE procedure must "          
		"also be PURE", r->proc_name->name,     
		&r->proc_name->declared_at);      
      
    g95_resolve(r);       
  }          
          
  forall_flag = 0;         
  g95_check_interfaces(names);

  for(c=names->cl_list; c; c=c->next) {
    if (c->length == NULL || g95_resolve_expr(c->length) == FAILURE)  
      continue;    
    
    if (c->length->ts.type != BT_INTEGER)        
      g95_error("Character length specification at %L must be of type INTEGER",     
		&c->length->where); 
  }        
        
  g95_traverse_ns(names, resolve_initial_values);     
     
  if (names->save_all) g95_save_all(names);        
        
  for(t=names->data; t; t=t->next)          
    resolve_data(t);       
       
  g95_generate_data(names);      
      
  resolve_common_block(names->common_root);     
  g95_traverse_ns(names, resolve_common_var); 
 
  for(q=names->equiv; q; q=q->next)      
    resolve_equivalence(q);          
          
  cs_base = NULL;       
  resolve_code(names->code, names);         
         
  g95_traverse_ns(names, resolve_symbol);   
   
  if (g95_option.unused_label) warn_unused_label(names);        
        
  if (g95_option.pedantic) g95_traverse_ns(names, check_variable_usage);   
   
  g95_current_ns = old_ns;    
}  
