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
static g95_symbol *entries;     
     
     
        
        
/* namespace_kind()-- Given a namespace, figure out what kind it is.
 * We return one of the g95_compile_state enums COMP_NONE,
 * COMP_MODULE, COMP_SUBROUTINE or COMP_FUNCTION. */

static g95_compile_state namespace_kind(g95_namespace *namesp) {
g95_symbol *symb; 
 
  symb = namesp->proc_name;    
    
  if (symb == NULL) return COMP_NONE;

  if (symb->attr.flavor == FL_MODULE) return COMP_MODULE;          
          
  if (symb->attr.subroutine) return COMP_SUBROUTINE;      
      
  if (symb->attr.flavor == FL_VARIABLE ||       
      symb->attr.function) return COMP_FUNCTION;    
    
  return COMP_NONE;          
}         
         
         
      
      
/* move_expr()-- Given the right hand side of the statement
 * function, replace the old symbol which is a formal argument with a
 * new symbol in the right namespace.  Recursively traverses the
 * expression tree. */   
   
static void move_expr(g95_expr *y, g95_symbol *old, g95_symbol *n1) {          
g95_actual_arglist *x;   
g95_ref *reference;   
int c;          
          
  if (y == NULL) return;          
          
  switch(y->type) {         
  case EXPR_OP:          
    move_expr(y->op1, old, n1);          
    move_expr(y->op2, old, n1);  
    break;  
  
  case EXPR_FUNCTION: 
    if (y->symbol == old) y->symbol = n1;  
  
    for(x=y->value.function.actual; x; x=x->next)      
      move_expr(x->u.expr, old, n1);    
    
    break;   
   
  case EXPR_CONSTANT:        
  case EXPR_NULL:      
    break;         
         
  case EXPR_UNKNOWN:  
  case EXPR_VARIABLE:     
    if (y->symbol == old) y->symbol = n1;          
          
    /* Fall through */          
          
  case EXPR_SUBSTRING:       
    for(reference=y->ref; reference; reference=reference->next)  
      switch(reference->type) {         
      case REF_ARRAY:        
	for(c=0; c<G95_MAX_DIMENSIONS; c++) {    
	  move_expr(reference->u.ar.start[c], old, n1);    
	  move_expr(reference->u.ar.end[c], old, n1); 
	  move_expr(reference->u.ar.stride[c], old, n1);         
	}      
      
	break;         
         
      case REF_SUBSTRING:  
	move_expr(reference->u.ss.start, old, n1); 
	move_expr(reference->u.ss.end, old, n1);    
	break;   
   
      case REF_COMPONENT:         
	break;    
      }     
     
    break;          
          
  case EXPR_STRUCTURE:  
  case EXPR_ARRAY:          
    move_constructor(y->value.constructor, old, n1);         
    break;    
  }          
}   
   
   
 
 
/* find_declaration()-- Given a symbol, figure out where it is
 * declared.  Returns the symbol to use, usually the original symbol
 * itself. */        
        
static g95_symbol *find_declaration(g95_symbol *sym) {     
g95_namespace *n;      
g95_symbol *d;          
          
  n = sym->ns->parent;  
  
  for(;;) {     
    if (g95_local_symbol(sym) || n == NULL) break;       
    if (g95_find_symbol(sym->name, n, 1, &d) || d == NULL) break;

    if (g95_local_symbol(d)) {   
      sym->attr.resolved = 1;          
      sym = d;      
      break;        
    }         
         
    n = n->parent;          
  }       
       
  return sym;  
}     
     
     
         
         
/* derived_init()-- Return nonzero if the derived type symbol has a
 * default initialization or contains a subtype that has a default
 * initialization.  */         
         
static int derived_init(g95_symbol *sy) {    
g95_component *m; 
 
  for(m=sy->ts.derived->components; m; m=m->next) { 
    if (m->initializer != NULL) return 1;        
        
    if (m->ts.type != BT_DERIVED || m->pointer) continue;   
   
    if (derived_init(m->ts.derived)) return 1; 
  }      
      
  return 0;       
} 
 
 
    
    
/* move_constructor()-- Mutually recursive with move_expr() to fix
 * symbol references within the right side of a statement function. */ 
 
static void move_constructor(g95_constructor *p, g95_symbol *old,  
			     g95_symbol *n1) {        
  for(; p; p=p->next) {  
    move_expr(p->expr, old, n1);         
         
    if (p->iterator == NULL) continue;         
         
    move_expr(p->iterator->start, old, n1);         
    move_expr(p->iterator->end,   old, n1);      
    move_expr(p->iterator->step,  old, n1);   
  }          
}       
       
       
      
      
/* move_formal_arglist()-- Given a symbol that is a statement
 * function, effectively move its formal argument list into the
 * contained namespace so that the dummy arguments have a separate
 * existence from symbols of the same name in the parent. */         
         
static void move_formal_arglist(g95_symbol *sym) {
g95_formal_arglist *h; 
g95_symbol *n, *new;          
g95_expr *r;  
  
  r = g95_current_ns->code->expr2;    /* RHS of the statement function */         
         
  for(h=sym->formal; h; h=h->next) {         
    n = h->sym;   
    n->attr.used = 1;         
         
    g95_get_symbol(n->name, g95_current_ns, &new);        
    new->ts = n->ts;
    new->declared_at = n->declared_at; 
 
    new->attr.flavor = FL_VARIABLE;       
    new->attr.dummy = 1;        
        
    move_expr(r, n, new); 
    h->sym = new;      
  }
}      
      
      
 
 
/* resolve_index()-- Resolve one part of an array index */        
        
static try resolve_index(g95_expr *idx, int check_scalar) { 
g95_typespec typ;   
   
  if (idx == NULL) return SUCCESS;         
         
  if (g95_resolve_expr(idx) == FAILURE) return FAILURE;  
  
  if (idx->ts.type != BT_INTEGER) {      
    g95_error("Array index at %L must be of INTEGER type", &idx->where); 
    return FAILURE;          
  }

  if (check_scalar && idx->rank != 0) {  
    g95_error("Array index at %L must be scalar", &idx->where);     
    return FAILURE;    
  }  
  
  if (idx->ts.kind != g95_default_integer_kind()) {         
    typ.type = BT_INTEGER;    
    typ.kind = g95_default_integer_kind();   
   
    g95_convert_type(idx, &typ, 2); 
  }        
        
  return SUCCESS; 
}          
          
          
      
      
/* resolve_st_function()-- Resolve a symbol that is a statement
 * function. */     
     
static void resolve_st_function(g95_symbol *symb) {      
      
  if (symb->ns == g95_current_ns) return; 
 
  if (symb->as != NULL) {      
    g95_error("Statement function '%s' at %L must be scalar",          
	      symb->name, &symb->declared_at);         
    return;        
  }      
      
  move_formal_arglist(symb);       
  g95_commit_symbols();      
      
  if (symb->ts.type == BT_UNKNOWN) g95_set_default_type(symb, 1, symb->ns);   
}


    
    
/* was_declared()-- Returns 0 if a symbol was not declared with a type
 * or attribute declaration statement, nonzero otherwise. */        
        
static int was_declared(g95_symbol *symbol) {     
symbol_attribute r;      
      
  r = symbol->attr;   
   
  if (!r.implicit_type && symbol->ts.type != BT_UNKNOWN) return 1; 
 
  if (r.allocatable || r.dimension || r.external || r.intrinsic ||       
      r.optional || r.pointer || r.save || r.target ||
      r.access != ACCESS_UNKNOWN || r.intent != INTENT_UNKNOWN ||       
      r.proc == PROC_MODULE || r.proc == PROC_INTERNAL ||      
      r.proc == PROC_ST_FUNCTION || r.proc == PROC_DUMMY) return 1;     
     
  return 0;          
} 
 
 


/* compare_bound_int()-- Compare an integer expression with an integer. */        
        
static int compare_bound_int(g95_expr *w, int n) {         
int k;        
        
  if (w == NULL || w->type != EXPR_CONSTANT) return CMP_UNKNOWN;      
       
  if (w->ts.type != BT_INTEGER)   
    g95_internal_error("compare_bound_int(): Bad expression");   
   
  k = mpz_cmp_si(w->value.integer, n);      
      
  if (k < 0) return CMP_LT;         
  if (k > 0) return CMP_GT;
  return CMP_EQ;
}     
     
     
    
    
/* resolve_deallocate_expr()-- Resolve the argument of a deallocate
 * expression.  The expression must be a pointer or a full array. */        
        
static try resolve_deallocate_expr(g95_expr *m) {
symbol_attribute attribute;          
int allocatable;    
g95_ref *ref; 
 
  if (g95_resolve_expr(m) == FAILURE) return FAILURE;

  attribute = g95_expr_attr(m);       
  if (attribute.pointer) return SUCCESS;

  if (m->type != EXPR_VARIABLE) goto bad;    
    
  allocatable = m->symbol->attr.allocatable; 
  for(ref=m->ref; ref; ref=ref->next)    
    switch(ref->type) {         
    case REF_ARRAY:    
      if (ref->u.ar.type != AR_FULL) allocatable = 0;  
      break;   
   
    case REF_COMPONENT:     
      allocatable = (ref->u.c.component->as != NULL && 
		     ref->u.c.component->as->type == AS_DEFERRED); 
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
g95_formal_arglist *g;   
g95_symbol *symbol;          
          
  for(g=proc->formal; g; g=g->next) {     
    symbol = g->sym;      
      
    if (symbol == NULL) {  /* Alternate return placeholder */
      if (g95_elemental(proc))  
	g95_error("Alternate return specifier in elemental subroutine "          
		  "'%s' at %L is not allowed", proc->name, &proc->declared_at);         
      continue;          
    }   
   
    if (symbol->as != NULL && symbol->as->type == AS_DEFERRED && !symbol->attr.pointer)         
      symbol->as->type = AS_ASSUMED_SHAPE;     
     
    symbol->attr.set = 1; 
    if (symbol->attr.if_source != IFSRC_UNKNOWN) resolve_formal_arglist(symbol);     
     
    if (symbol->attr.subroutine || symbol->attr.external || symbol->attr.intrinsic) {          
      if (g95_pure(proc) && !g95_pure(symbol)) {      
	g95_error("Dummy procedure '%s' of PURE procedure at %L must also "    
		  "be PURE", symbol->name, &symbol->declared_at);     
	continue;         
      }      
      
      if (g95_elemental(proc)) {   
	g95_error("Dummy procedure at %L not allowed in ELEMENTAL procedure",  
		  &symbol->declared_at);  
	continue;    
      }       
       
      continue;        
    }        
        
    if (symbol->ts.type == BT_UNKNOWN) {       
      if (!symbol->attr.function || symbol->result == symbol)      
	g95_set_default_type(symbol, 1, symbol->ns);         
      else {    /* Set the type of the RESULT, then copy */       
       
	if (symbol->result->ts.type == BT_UNKNOWN)         
	  g95_set_default_type(symbol->result, 1, symbol->result->ns);   
   
	symbol->ts = symbol->result->ts;      
	if (symbol->as == NULL) symbol->as = g95_copy_array_spec(symbol->result->as);     
      }
    }      
      
    g95_resolve_array_spec(symbol->as, 0);  
  
    if (proc->attr.proc == PROC_ST_FUNCTION && symbol->as != NULL)         
      g95_error("Argument '%s' of statement function at %L must be scalar",   
		symbol->name, &symbol->declared_at); 
 
    /* If the flavor is unknown at this point, it has to be a variable.
     * A procedure specification would have already set the type */

    if (symbol->attr.flavor == FL_UNKNOWN)
      g95_add_flavor(&symbol->attr, FL_VARIABLE, &symbol->declared_at);

    if (g95_pure(proc)) {     
      if (proc->attr.function && !symbol->attr.pointer &&    
	  symbol->attr.intent != INTENT_IN)   
   
	g95_error("Argument '%s' of pure function '%s' at %L must be "  
		  "INTENT(IN)", symbol->name, proc->name, &symbol->declared_at);       
       
      if (proc->attr.subroutine && !symbol->attr.pointer &&  
	  symbol->attr.intent == INTENT_UNKNOWN) 

	g95_error("Argument '%s' of pure subroutine '%s' at %L must have "   
		  "its INTENT specified", symbol->name, proc->name,    
		  &symbol->declared_at);     
    }  
  
    if (g95_elemental(proc)) {   
      if (symbol->as != NULL) {      
	g95_error("Argument '%s' of elemental procedure at %L must be scalar",          
		  symbol->name, &symbol->declared_at);        
	continue;
      }       
       
      if (symbol->attr.pointer) { 
	g95_error("Argument '%s' of elemental procedure at %L cannot have "
		  "the POINTER attribute", symbol->name, &symbol->declared_at);  
	continue;          
      }   
    }         
  }   
}         
         
         
       
       
/* g95_pure()-- Test whether a symbol is pure or not.  For a NULL
 * pointer, checks the symbol of the current procedure. */

int g95_pure(g95_symbol *symb) {          
symbol_attribute a;        
        
  if (symb == NULL) symb = g95_current_ns->proc_name; 
  if (symb == NULL) return 0;         
         
  a = symb->attr;         
         
  return a.flavor == FL_PROCEDURE && (a.pure || a.elemental);
}        
        
        


/* resolve_generic_f0()-- Resolve a function call known to be generic.
 * Section 14.1.2.4.1. */     
     
static match resolve_generic_f0(g95_expr *expr, g95_symbol *sy) {          
g95_symbol *u;        
        
  if (sy->attr.generic) {  
    u = g95_search_interface(sy->generic, 0, &expr->value.function.actual);          
    if (u != NULL) {   
      if (u->attr.proc == PROC_UNKNOWN) u->attr.proc = PROC_EXTERNAL;     
     
      expr->value.function.name = u->name;   
   
      if (expr->symbol != u) {     
	expr->symbol->attr.resolved = 1;        
	expr->symbol = u;      
      }

      expr->ts = u->result->ts;    
      if (u->result->as != NULL) expr->rank = u->result->as->rank;         
      return MATCH_YES;
    }      
      
    /* TODO: Need to search for elemental references in generic interface */       
  }  
  
  if (sy->attr.intrinsic) return g95_intrinsic_func_interface(expr, 0); 
 
  return MATCH_NO;        
}     
     
     
     
     
/* resolve_actual_argument()-- Resolve an actual argument. */       
       
static try resolve_actual_argument(g95_expr *l) {       
g95_symbol *sym;   
   
  if (l->ts.type != BT_PROCEDURE) return g95_resolve_expr(l);

  /* See if the expression node should really be a variable reference */  
  
  sym = find_declaration(l->symbol);  
  l->symbol = sym;    
    
  if (sym->attr.flavor == FL_PROCEDURE || sym->attr.intrinsic ||     
      sym->attr.external) { 
 
    if (sym->attr.proc == PROC_UNKNOWN) {    
      sym->attr.proc = PROC_EXTERNAL;         
      if (sym->result == NULL) sym->result = sym;   
    }     
     
    return SUCCESS;  
  }          
          
  l->type = EXPR_VARIABLE;       
  l->ts = sym->ts;

  if (sym->as != NULL) {          
    l->rank = sym->as->rank;      
    l->ref = g95_get_ref();
    l->ref->type = REF_ARRAY;          
    l->ref->u.ar.type = AR_FULL;          
  }   
   
  return g95_resolve_expr(l);     
}    
    
    
 
 
/* resolve_structure_cons()-- Resolve all of the elements of a
 * structure constructor and make sure that the types are correct. */  
  
static try resolve_structure_cons(g95_expr *e2) {     
g95_constructor *cons;          
g95_component *comp;  
g95_locus where;  
  
  cons = e2->value.constructor;  
  comp = e2->symbol->components;  
  where = e2->where;     
     
  for(; comp; comp=comp->next, cons=cons->next) {  
    if (cons == NULL) { 
      g95_error("Not enough values in structure constructor at %L", &where);      
      return FAILURE;    
    }  
  
    where = cons->expr->where;    
    
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
       
       
   
   
/* is_default_kind()-- Return nonzero if the kind of a type is the
 * default kind.  Derived types return zero. */        
        
static int is_default_kind(g95_typespec *ts) {       
       
  switch(ts->type) {       
  case BT_INTEGER:     
    if (ts->kind == g95_default_integer_kind()) return 1;
    break;          
          
  case BT_LOGICAL:     
    if (ts->kind == g95_default_logical_kind()) return 1;
    break;     
     
  case BT_REAL:       
  case BT_COMPLEX:     
    if (ts->kind == g95_default_real_kind() ||       
	ts->kind == g95_default_double_kind()) return 1;      
    break;          
          
  case BT_CHARACTER:   
    if (ts->kind == g95_default_character_kind()) return 1;       
    break;     
     
  default:   
    break;    
  }       
       
  return 0;      
}     
     
     
          
          
/* resolve_operator()-- Resolve an operator expression node.  This can
 * involve replacing the operation with a user defined function call. */    
    
static try resolve_operator(g95_expr *f) {      
g95_expr *op1, *op0;       
char msg[200];     
try s;        
        
/* Resolve all subnodes-- give them types. */         
         
  switch(f->operator) {       
  default:
    if (g95_resolve_expr(f->op2) == FAILURE) return FAILURE;       
       
/* Fall through */      
      
  case INTRINSIC_NOT:        
  case INTRINSIC_UPLUS:  
  case INTRINSIC_UMINUS:          
    if (g95_resolve_expr(f->op1) == FAILURE) return FAILURE;      
    break;
  }    
    
/* Typecheck the new node. */  
  
  op1 = f->op1;         
  op0 = f->op2;          
          
  switch(f->operator) {       
  case INTRINSIC_UPLUS:  
  case INTRINSIC_UMINUS:      
    if ((op1->ts.type == BT_INTEGER) || (op1->ts.type == BT_REAL) ||
	(op1->ts.type == BT_COMPLEX)) {      
      f->ts = op1->ts;   
      break;    
    }

    sprintf(msg, "Operand of unary numeric operator '%s' at %%L is %s",        
	    g95_op2string(f->operator), g95_typename(&f->ts));  
    goto bad_op;          
          
  case INTRINSIC_PLUS:        
  case INTRINSIC_MINUS:  
  case INTRINSIC_TIMES:  
  case INTRINSIC_DIVIDE:    
  case INTRINSIC_POWER:   
    if (g95_numeric_ts(&op1->ts) && g95_numeric_ts(&op0->ts)) {   
      g95_type_convert_binary(f);       
      break;     
    }

    sprintf(msg, "Operands of binary numeric operator '%s' at %%L are %s/%s",
	    g95_op2string(f->operator), g95_typename(&op1->ts),      
	    g95_typename(&op0->ts)); 
    goto bad_op;     
     
  case INTRINSIC_CONCAT:      
    if (op1->ts.type == BT_CHARACTER && op0->ts.type == BT_CHARACTER) {  
      f->ts.type = BT_CHARACTER;
      f->ts.kind = op1->ts.kind;   
      break;       
    }          
          
    sprintf(msg, "Operands of string concatenation operator at %%L are %s/%s",        
	    g95_typename(&op1->ts), g95_typename(&op0->ts));        
    goto bad_op;      
      
  case INTRINSIC_AND:     
  case INTRINSIC_OR:    
  case INTRINSIC_EQV:   
  case INTRINSIC_NEQV:    
    if (op1->ts.type == BT_LOGICAL && op0->ts.type == BT_LOGICAL) {
      f->ts.type = BT_LOGICAL;
      f->ts.kind = g95_kind_max(op1, op0);         
      break;         
    }        
        
    sprintf(msg, "Operands of logical operator '%s' at %%L are %s/%s",    
	    g95_op2string(f->operator), g95_typename(&op1->ts),  
	    g95_typename(&op0->ts));         
         
    goto bad_op;     
           
  case INTRINSIC_NOT:  
    if (op1->ts.type == BT_LOGICAL) {        
      f->ts.type = BT_LOGICAL;          
      f->ts.kind = op1->ts.kind;      
      break;          
    }      
      
    sprintf(msg, "Operand of .NOT. operator at %%L is %s",  
	    g95_typename(&op1->ts));       
    goto bad_op;    
    
  case INTRINSIC_GT: case INTRINSIC_GE:        
  case INTRINSIC_LT: case INTRINSIC_LE:  
    if (op1->ts.type == BT_COMPLEX || op0->ts.type == BT_COMPLEX) {         
      strcpy(msg, "COMPLEX quantities cannot be compared at %L");  
      goto bad_op;        
    }       
       
    /* Fall through */

  case INTRINSIC_EQ: case INTRINSIC_NE:          
    if (op1->ts.type == BT_CHARACTER && op0->ts.type == BT_CHARACTER) {          
      f->ts.type = BT_LOGICAL;         
      f->ts.kind = g95_default_logical_kind();  
      break;          
    }   
   
    if (g95_numeric_ts(&op1->ts) && g95_numeric_ts(&op0->ts)) {    
      g95_type_convert_binary(f);        
	        
      f->ts.type = BT_LOGICAL;  
      f->ts.kind = g95_default_logical_kind();  
      break;        
    }      
      
    sprintf(msg, "Operands of comparison operator '%s' at %%L are %s/%s",   
	    g95_op2string(f->operator), g95_typename(&op1->ts),        
	    g95_typename(&op0->ts));    
    
    goto bad_op;        
        
  case INTRINSIC_USER:         
    if (op0 == NULL)    
      sprintf(msg, "Operand of user operator '%s' at %%L is %s",   
	      f->symbol->name, g95_typename(&op1->ts));   
    else         
      sprintf(msg, "Operands of user operator '%s' at %%L are %s/%s",  
	      f->symbol->name, g95_typename(&op1->ts), g95_typename(&op0->ts)); 
 
    goto bad_op;     
     
  default:         
    g95_internal_error("resolve_operator(): Bad intrinsic");
  } 
 
/* Deal with arrayness of an operand through an operator */

  s = SUCCESS;      
      
  switch(f->operator) {
  case INTRINSIC_PLUS:    case INTRINSIC_MINUS:  case INTRINSIC_TIMES:  
  case INTRINSIC_DIVIDE:  case INTRINSIC_POWER:  case INTRINSIC_CONCAT: 
  case INTRINSIC_AND:     case INTRINSIC_OR:     case INTRINSIC_EQV:   
  case INTRINSIC_NEQV:    case INTRINSIC_EQ:     case INTRINSIC_NE:    
  case INTRINSIC_GT:      case INTRINSIC_GE:     case INTRINSIC_LT:        
  case INTRINSIC_LE:   
   
    if (op1->rank == 0 && op0->rank == 0) f->rank = 0;     
     
    if (op1->rank == 0 && op0->rank != 0) {         
      f->rank = op0->rank;        
        
      if (f->shape == NULL) f->shape = g95_copy_shape(op0->shape, op0->rank); 
    }   
   
    if (op1->rank != 0 && op0->rank == 0) {        
      f->rank = op1->rank;        
        
      if (f->shape == NULL) f->shape = g95_copy_shape(op1->shape, op1->rank);   
    }     
     
    if (op1->rank != 0 && op0->rank != 0) {       
      if (op1->rank == op0->rank) {      
	f->rank = op1->rank;        
        
	if (f->shape == NULL) f->shape = g95_copy_shape(op1->shape, op1->rank);     
     
      } else {          
	g95_error("Inconsistent ranks for operator at %L and %L",      
		  &op1->where, &op0->where);   
	s = FAILURE; 
 
	f->rank = 0;   /* Allow higher level expressions to work */
      }   
    }      
      
    break;          
          
  case INTRINSIC_NOT:         
  case INTRINSIC_UPLUS:   
  case INTRINSIC_UMINUS:  
    f->rank = op1->rank; 
 
    if (f->shape == NULL) f->shape = g95_copy_shape(op1->shape, op1->rank);    
    
    break;           /* Simply copy arrayness attribute */    
    
  default:       
    break;  
  }         
         
  if (s == SUCCESS) s = g95_simplify_expr(f, 0); 
  return s;    
    
bad_op:         
  if (g95_extend_expr(f) == SUCCESS) return SUCCESS;    
    
  g95_error(msg, &f->where);   
  return FAILURE;         
}


 
 
/* g95_find_entries()-- Recursive function for finding and linking
 * entry symbols within a namespace. */  
  
g95_symbol *g95_find_entries(g95_symtree *s, int flag) {          
g95_symbol *symb;     
     
  if (flag) entries = NULL; 
 
  if (s == NULL) return NULL; 
 
  symb = s->n.sym;          
          
  if (symb->attr.entry) {         
    symb->tlink = entries;    
    entries = symb; 
  }      
      
  g95_find_entries(s->left, 0);         
  g95_find_entries(s->right, 0);        
        
  return entries;   
}     
     
     
     
     
/* specific_sym()-- Determine if a symbol is specific or not */ 
 
static int specific_sym(g95_symbol *sy) {   
g95_symbol *g;       
       
  if (sy->attr.if_source == IFSRC_IFBODY || sy->attr.proc == PROC_MODULE ||     
      sy->attr.if_source == IFSRC_DECL || sy->attr.proc == PROC_INTERNAL ||     
      sy->attr.proc == PROC_ST_FUNCTION || sy->attr.external || 
      (sy->attr.intrinsic && g95_specific_intrinsic(sy->name)))     
    return 1;    
    
  if (was_declared(sy) || sy->ns->parent == NULL) return 0;       
       
  g95_find_symbol(sy->name, sy->ns->parent, 1, &g);          
          
  return (g == NULL) ? 0 : specific_sym(g);        
}         
         
         
       
       
/* augment_arglist()-- We search through an existing argument list for
 * the smybol and add it if it is not there. */          
          
static void augment_arglist(g95_formal_arglist **fp2, g95_symbol *sy) {         
g95_formal_arglist *s;          
          
  while(*fp2 != NULL) {      
    if ((*fp2)->sym == sy) return;     
     
    fp2 = &((*fp2)->next);         
  }  
  
  s = g95_get_formal_arglist(); 
  *fp2 = s;        
        
  s->sym = sy;          
} 
 
 
        
        
/* find_arglists()-- Work function called when searching for symbols
 * that have argument lists associated with them. */       
       
static void find_arglists(g95_symbol *s) {     
     
  if (s->attr.proc == PROC_ST_FUNCTION)       
    resolve_st_function(s);   
  else {         
    if (s->attr.if_source == IFSRC_UNKNOWN || s->ns != g95_current_ns)     
      return;          
  }  
  
  resolve_formal_arglist(s);    
}     
     
     
        
        
/* g95_elemental()-- Test whether the current procedure is elemental or not */   
   
int g95_elemental(g95_symbol *s) {
symbol_attribute a;    
    
  if (s == NULL) s = g95_current_ns->proc_name;         
  if (s == NULL) return 0;
  a = s->attr;       
       
  return a.flavor == FL_PROCEDURE && a.elemental;         
}     
     
     
      
      
static try resolve_generic_f(g95_expr *exp) {     
g95_symbol *symb; 
match l;       
       
  symb = exp->symbol;    
    
  for(;;) {       
    l = resolve_generic_f0(exp, symb);      
    if (l == MATCH_YES) return SUCCESS;          
    if (l == MATCH_ERROR) return FAILURE;        
        
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
  
  l = g95_intrinsic_func_interface(exp, 0);
  if (l == MATCH_YES) return SUCCESS;      
  if (l == MATCH_NO)    
    g95_error("Generic function '%s' at %L is not consistent with a specific "       
	      "intrinsic interface", exp->symbol->name, &exp->where); 
 
  return FAILURE;    
}     
     
     
     
     
/* numeric_sequence_type()-- Return nonzero if the type is a numeric
 * sequence derived type. */         
         
static int numeric_sequence_type(g95_typespec *typesp) {         
g95_component *g;         
g95_symbol *sy;       
       
  if (typesp->type != BT_DERIVED) return 0;

  sy = typesp->derived;  
  if (!sy->attr.sequence) return 0;   
   
  for(g=sy->components; g; g=g->next) {    
    if (g->pointer) return 0;    
    
    typesp = &g->ts;      
    if ((typesp->type != BT_INTEGER && typesp->type != BT_REAL &&       
	 typesp->type != BT_COMPLEX && typesp->type != BT_LOGICAL) ||   
	!is_default_kind(typesp))         
      return 0;  
  }   
   
  return 1;     
}     
     
     
 
 
/* resolve_actual_arglist()-- Resolve an actual argument list.  Most
 * of the time, this is just resolving the expressions in the list.
 * The exception is that we sometimes have to decide whether an 
 * argument refers to a dummy procedure or a simple variable. */     
     
static try resolve_actual_arglist(g95_actual_arglist *args) {          
          
  for(; args; args=args->next) { 
    if (args->type == ALT_RETURN || args->u.expr == NULL) continue;      
    if (resolve_actual_argument(args->u.expr) == FAILURE) return FAILURE;    
  }     
     
  return SUCCESS;  
}          
          
          
          
          
/* generic_symbol()-- Determine if a symbol is generic or not */     
     
static int generic_sym(g95_symbol *symbol) {         
g95_symbol *g;    
    
  if (symbol->attr.generic ||    
      (symbol->attr.intrinsic && g95_generic_intrinsic(symbol->name)))         
    return 1;    
    
  if (was_declared(symbol) || symbol->ns->parent == NULL) return 0;      
      
  g95_find_symbol(symbol->name, symbol->ns->parent, 1, &g);        
        
  return (g == NULL) ? 0 : generic_sym(g);   
}    
    
    
      
      
/* resolve_charlen()-- Resolve character lengths */

static try resolve_charlen(g95_namespace *ns) {   
g95_charlen *clen;         
try l;        
        
  l = SUCCESS;          
          
  for(clen=ns->cl_list; clen; clen=clen->next) {   
    if (clen->length == NULL) continue;          
          
    if (g95_resolve_expr(clen->length) == FAILURE) {     
      l = FAILURE;  
      continue;   
    }        
        
    if (clen->length->ts.type != BT_INTEGER) {     
      g95_error("Character length specification at %L must be of type INTEGER", 
		&clen->length->where);         
      l = FAILURE;
    }    
  }  
  
  return l;        
}        
        
        


/* function_type()-- Set the expression type from the function type. */       
       
static try function_type(g95_expr *e2, g95_symbol *symb) {          
g95_typespec *ts;

  if (symb->result->ts.type != BT_UNKNOWN)    
    e2->ts = symb->result->ts;   
  else {        
    ts = g95_get_default_type(symb->result, symb->ns);       
       
    if (ts->type == BT_UNKNOWN) {    
      g95_error("Function '%s' at %L has no implicit type",         
		symb->name, &e2->where);
      return FAILURE;
    } else 
      e2->ts = *ts;      
  }      
      
  return SUCCESS;          
}         
         
         
          
          
/* resolve_data_variables()-- Resolve the expressions and iterators
 * associated with a data statement.  This is separate from the
 * assignment checking because data lists only should be resolved
 * once. */          
          
static try resolve_data_variables(g95_data_variable *x) {          
          
  for(; x; x=x->next) {      
    if (x->list == NULL) {
      if (g95_resolve_expr(x->expr) == FAILURE) return FAILURE;   
    } else { 
      if (g95_resolve_iterator(&x->iter) == FAILURE) return FAILURE;    
    
      if (resolve_data_variables(x->list) == FAILURE) return FAILURE;    
    }    
  }       
       
  return SUCCESS;         
}     
     
     
          
          
/* character_sequence_type()-- Return nonzero if the type is a numeric
 * sequence derived type. */      
      
static int character_sequence_type(g95_typespec *ts) {        
g95_component *p;   
g95_symbol *sym;          
          
  if (ts->type != BT_DERIVED) return 0;       
       
  sym = ts->derived;       
  if (!sym->attr.sequence) return 0;         
         
  for(p=sym->components; p; p=p->next)          
    if (p->pointer || p->ts.type != BT_CHARACTER || !is_default_kind(&p->ts))         
      return 0; 
 
  return 1;    
}     
     
     
  
  
/* resolve_formal_arglists()-- Given a namespace, resolve all formal
 * argument lists within the namespace. */  
  
static void resolve_formal_arglists(g95_namespace *ns) {       
       
  if (ns == NULL) return;          
          
  g95_traverse_ns(ns, find_arglists);  
}          
          
          
 
 
/* find_component()-- Given a derived type node and a component name,
 * try to locate the component structure.  Returns the NULL pointer if
 * the component is not found or the components are private.  The flag
 * variable is nonzero if the parent variable has been use-associated,
 * which means the check for private components should be bypassed. */     
     
static g95_component *find_component(g95_symbol *sym, g95_ref *ref, int flag) {        
g95_component *j;        
char *name0;    
    
  name0 = ref->u.c.name; 
 
  for(j=sym->components; j; j=j->next)   
    if (strcmp(j->name, name0) == 0) break;         
         
  if (j == NULL)       
    g95_error("Element '%s' at %L is not a member of the '%s' structure",        
	      name0, &ref->where, sym->name);          
  else {       
    if (sym->attr.use_assoc && sym->component_access == ACCESS_PRIVATE &&      
	!flag) {  
      g95_error("Component '%s' at %L is a PRIVATE component of '%s'",     
		name0, &ref->where, sym->name); 
      j = NULL;         
    }    
  }      
      
  return j;        
}    
    
    
   
   
/* resolve_derived()-- Resolve a derived type symbol */    
    
static try resolve_derived(g95_symbol *sy) {    
g95_component *u;    
    
  if (!sy->attr.set) {  
    g95_error("Derived type '%s' at %L never defined",        
	      sy->name, &sy->declared_at);      
    return FAILURE;      
  }    
    
  for(u=sy->components; u; u=u->next) {         
    if (u->as == NULL) continue;     
    if (g95_resolve_array_spec(u->as, 1) == FAILURE) return FAILURE;
  }         
         
  return SUCCESS;        
}          
          
          
      
      
/* derived_pointer()-- Given a pointer to a symbol that is a derived
 * type, see if any components have the POINTER attribute.  The search
 * is recursive if necessary.  Returns zero if no pointer components
 * are found, nonzero otherwise. */        
        
static int derived_pointer(g95_symbol *symb) {      
g95_component *s;    
    
  for(s=symb->components; s; s=s->next) {   
    if (s->pointer) return 1;   
   
    if (s->ts.type == BT_DERIVED && derived_pointer(s->ts.derived)) return 1;       
  }       
       
  return 0;
}        
        
        
      
      
/* pure_function()-- Figure out if if a function reference is pure or
 * not.  Also sets the name of the function for a potential error
 * message.  Returns nonzero if the function is PURE, zero if not. */      
      
static int pure_function(g95_expr *n, char **name0) {          
int pure;         
         
  if (n->value.function.isym) {
    pure = n->value.function.isym->pure || n->value.function.isym->elemental;     
    *name0 = n->value.function.isym->name;          
  } else {         
    pure = g95_pure(n->symbol);        
    *name0 = n->symbol->name;         
  }       
       
  return pure;
}          
          
          
 
 
/* resolve_branch()-- Given a branch to a label and a namespace, see
 * if the branch is conforming.  The code node describes where the
 * branch is located. */    
    
static try resolve_branch(g95_st_label *l, g95_code *c) { 
g95_code *body, *found; 
code_stack *stack;   
g95_st_label *lp;    
    
  if (l == NULL) return SUCCESS; 
  lp = l;        
        
  /* Step one: is this a valid branching target? */     
     
  if (lp->defined == ST_LABEL_UNKNOWN) {    
    g95_error("Label %d referenced at %L is never defined", lp->value,  
	      &lp->where);         
    lp->defined = ST_LABEL_BAD_TARGET2;  
    return FAILURE;        
  } 
 
  if (lp->defined == ST_LABEL_BAD_TARGET2)       
    return FAILURE;      
      
  if (lp->defined != ST_LABEL_TARGET) {        
    g95_error("Statement at %L is not a valid branch target statement "        
	      "for the branch statement at %L", &lp->where, &c->where);        
    return FAILURE;   
  }        
        
  /* Step two: make sure this branch is not a branch to itself ;-) */        
        
  if (c->here == l) { 
    g95_warning(109, "Branch at %L causes an infinite loop", &c->where);         
    return SUCCESS;  
  } 
 
  /* Step three: Try to find the label in the parse tree. To do this,
   * we traverse the tree block-by-block: first the block that
   * contains this GOTO, then the block that it is nested in, etc.  We
   * can ignore other blocks because branching into another block is
   * not allowed. */   
   
  found = NULL;   
   
  for(stack=cs_base; stack; stack=stack->prev) {      
    for(body=stack->head; body; body=body->next) {     
      if (body->here == l) {         
        found = body; 
        break;      
      }         
    }    
    
    if (found) break;         
  }   
   
  if (found == NULL) {    /* still nothing, so illegal.  */   
    g95_error_now("Label at %L is not in the same block as the "      
                  "GOTO statement at %L", &lp->where, &c->where);     
    return FAILURE;     
  }     
     
  /* Step four: Make sure that the branching target is legal if
   * the statement is an END {SELECT,DO,IF}. */

  if (found->type == EXEC_NOP && found->ext.end_code != ST_ENDDO) {   
    for(stack=cs_base; stack; stack=stack->prev)         
      if (stack->current->next == found) break;  
  
    if (stack == NULL) {   
      g95_error("GOTO at %L cannot jump to END of construct at %L",    
                &found->where, &c->where);      
      return FAILURE;        
    }         
  }        
        
  return SUCCESS;  
}           
          
          
 
 
/* procedure_kind()-- Figure out if the procedure is specific, generic
 * or unknown. */ 
 
static proc_type procedure_kind(g95_symbol *symbol) {   
   
  if (generic_sym(symbol))  return PTYPE_GENERIC;   
  if (specific_sym(symbol)) return PTYPE_SPECIFIC;      
  return PTYPE_UNKNOWN;        
}      
      
      
 
 
/* mark_external()-- It is possible for a symbol to be marked as
 * EXTERNAL or INTRINSIC in a module subprogram, but not be explicitly
 * defined.  This subroutine explicitly marks such procedures and
 * makes sure that they are being used correctly across module
 * procedures. */ 
 
static try mark_external(g95_symbol *sy, int function_flag) {     
     
  if ((function_flag && sy->attr.subroutine) ||   
      (!function_flag && sy->attr.function)) {    
    g95_error("Symbol '%s' at %L is used as both a FUNCTION and a SUBROUTINE",       
	      sy->name, &sy->declared_at);        
    return FAILURE; 
  }    
    
  if (sy->attr.function || sy->attr.subroutine) return SUCCESS;         
         
  /* Set the procedure type at this point */ 
 
  return function_flag ?    
    g95_add_function(&sy->attr, &sy->declared_at) :        
    g95_add_subroutine(&sy->attr, &sy->declared_at);    
}          
          
          
          
          
/* resolve_unknown_f()-- Resolve a procedure call not known to be
 * generic nor specific */          
          
static try resolve_unknown_f(g95_expr *e) {        
g95_symbol *sym;  
  
  sym = e->symbol;      
     
  if (sym->attr.dummy) {     
    sym->attr.proc = PROC_DUMMY;     
    e->value.function.name = sym->name;  
    goto set_type;    
  }  
  
  /* See if we have an intrinsic function reference */

  if (g95_intrinsic_name(sym->name, 0)) {      
    if (g95_intrinsic_func_interface(e, 1) == MATCH_YES) return SUCCESS; 
    return FAILURE;     
  }     
     
  /* The reference is to an external name */         
         
  sym->attr.proc = PROC_EXTERNAL; 
  e->value.function.name = sym->name;    
    
  if (e->symbol != sym) {         
    e->symbol->attr.resolved = 1;
    e->symbol = sym;
  } 
 
  if (sym->result->as != NULL) e->rank = sym->result->as->rank;          
          
 set_type:         
  g95_procedure_use(sym, &e->value.function.actual, &e->where);    
    
  /* Type of the expression is either the type of the symbol or the
   * default type of the symbol */        
          
  return function_type(e, sym);          
}  
  
  
      
      
static void pure_subroutine(g95_code *x, g95_symbol *sym) { 
 
  if (g95_pure(sym)) return;  
  
  if (forall_flag)        
    g95_error("Subroutine call to '%s' in FORALL block at %L is not PURE",          
	      x->sym->name, &x->where);
  else if (g95_pure(NULL))          
    g95_error("Subroutine call to '%s' at %L is not PURE", x->sym->name,     
	      &x->where);   
}    
    
    
      
      
/* g95_impure_variable()-- Determines if a variable is not 'pure', ie
 * not assignable within a pure procedure.  Returns zero if assignment
 * is OK, nonzero if there is a problem. */         
         
int g95_impure_variable(g95_symbol *symb) {        
        
  if (symb->attr.use_assoc || symb->attr.in_common) return 1;      
      
  if (symb->ns != g95_current_ns) return !symb->attr.function;   
   
  /* TODO: Check storage association through EQUIVALENCE statements */   
   
  return 0;        
}   
   
   
        
        
/* resolve_specific_f0()-- Resolve a function call known to be specific */ 
 
static match resolve_specific_f0(g95_symbol *sym, g95_expr *e2) {    
match m; 
 
  if (sym->attr.external || sym->attr.if_source == IFSRC_IFBODY) {   
    if (sym->attr.dummy) {        
      sym->attr.proc = PROC_DUMMY;      
      goto found;      
    }

    sym->attr.proc = PROC_EXTERNAL;       
    goto found;         
  }          
          
  if (sym->attr.proc == PROC_MODULE || sym->attr.proc == PROC_INTERNAL ||         
      sym->attr.proc == PROC_ST_FUNCTION || sym->attr.if_source == IFSRC_DECL)       
    goto found;     
     
  if (sym->attr.intrinsic) {
    m = g95_intrinsic_func_interface(e2, 1);       
    if (m == MATCH_YES) return MATCH_YES;         
    if (m == MATCH_NO) 
      g95_error("Function '%s' at %L is INTRINSIC but is not compatible with "  
		"an intrinsic", sym->name, &e2->where);         
               
    return MATCH_ERROR;  
  }     
     
  return MATCH_NO;      
      
found:         
  g95_procedure_use(sym, &e2->value.function.actual, &e2->where);  
  
  if (sym->result == NULL) sym->result = sym;      
      
  if (function_type(e2, sym) == FAILURE) return MATCH_ERROR;    
    
  if (mark_external(sym, 1) == FAILURE) return MATCH_ERROR;       
       
  e2->value.function.name = sym->name;         
  if (e2->symbol != sym) {  
    e2->symbol->attr.resolved = 1;        
    e2->symbol = sym;        
  } 
 
  if (sym->result->as != NULL) e2->rank = sym->result->as->rank;     
     
  return MATCH_YES;      
}   
   
   
       
       
/* resolve_symbol()-- Do anything necessary to resolve a symbol.
 * Right now, we just assume that an otherwise unknown symbol is a
 * variable.  This sort of thing commonly happens for symbols in
 * module. */    
    
static try resolve_symbol(g95_symbol *symbol) {      
static int formal_ns_flag = 1; /* Zero if we are checking a formal namespace */          
int formal_ns_save, check_constant, mp_flag;

  if (symbol->attr.resolved) return SUCCESS;      
  symbol->attr.resolved = 1;        
        
  if (symbol->attr.flavor == FL_UNKNOWN) {      
    if (symbol->attr.external == 0 && symbol->attr.intrinsic == 0)         
      symbol->attr.flavor = FL_VARIABLE;    
    else {        
      symbol->attr.flavor = FL_PROCEDURE; 
      if (symbol->attr.dimension) symbol->attr.function = 1;  
    }      
  } 
 
  if (symbol->attr.flavor == FL_DERIVED && resolve_derived(symbol) == FAILURE)   
    return FAILURE;

  /* Symbols that are module procedures with results (functions) have
   * the types and array specification copied for type checking in
   * procedures that call them, as well as for saving to a module
   * file.  These symbols can't stand the scrutiny that their results
   * can. */         
         
  mp_flag = symbol->result != NULL && symbol->result != symbol;      
      
  /* Assign default type to symbols that need one and don't have one */   
   
  if (symbol->ts.type == BT_UNKNOWN) {   
    if (symbol->attr.flavor == FL_VARIABLE || symbol->attr.flavor == FL_PARAMETER)      
      g95_set_default_type(symbol, 1, NULL);       
       
    if (symbol->attr.flavor == FL_PROCEDURE && symbol->attr.function) { 
      if (!mp_flag)
	g95_set_default_type(symbol, 0, NULL);
      else {    
	if (resolve_symbol(symbol->result) == FAILURE)    
	  return FAILURE;  /* Result may be in another namespace */ 
 
	symbol->ts = symbol->result->ts;       
	symbol->as = g95_copy_array_spec(symbol->result->as);  
	symbol->attr.pointer = symbol->result->attr.pointer;
	symbol->attr.dimension = symbol->result->attr.dimension;        
      }    
    }         
  }    
    
  if (symbol->as != NULL && (symbol->as->type == AS_ASSUMED_SIZE ||           
			  symbol->as->type == AS_ASSUMED_SHAPE ) &&
      symbol->attr.dummy == 0) {         
    g95_error("Assumed %s array at %L must be a dummy argument",      
	      symbol->as->type == AS_ASSUMED_SIZE ? "size" : "shape",
	      &symbol->declared_at);          
    return FAILURE;         
  }         
         
  /* Mark variables with initialization expressions as having been set */         
         
  if (symbol->attr.flavor == FL_VARIABLE && symbol->value != NULL) symbol->attr.set = 1;      
      
  /* Make sure that character string variables with assumed lengths are
   * dummy arguments or results. */   
   
  if (symbol->attr.flavor == FL_VARIABLE && symbol->ts.type == BT_CHARACTER &&   
      symbol->ts.cl->length == NULL && !symbol->attr.dummy && !symbol->attr.result_var){        
    g95_error("Assumed character length variable '%s' at %L must be a "       
	      "dummy variable or result", symbol->name, &symbol->declared_at);   
    return FAILURE;
  } 
 
  /* Make sure a parameter that has been implicitly typed still
   * matches the implicit type, since PARAMETER statements can precede
   * IMPLICIT statements. */         
         
  if (symbol->attr.flavor == FL_PARAMETER &&  
      g95_check_parameter(symbol) == FAILURE) return FAILURE;   
   
  /* Make sure the types of derived parameters are consistent.  This
   * type checking is deferred until resolution because the type may
   * refer to a derived type from the host. */    
    
  if (symbol->attr.flavor == FL_PARAMETER && symbol->ts.type == BT_DERIVED &&    
      !g95_compare_types(&symbol->ts, &symbol->value->ts)) {      
    g95_error("Incompatible derived type in PARAMETER at %L",      
	      &symbol->value->where);   
    return FAILURE; 
  }   
   
  /* Make sure symbols with known intent or optional are really dummy
   * variable.  Because of ENTRY statement, this has to be deferred
   * until resolution time. */  
  
  if ((symbol->attr.optional || symbol->attr.intent != INTENT_UNKNOWN) &&
      symbol->attr.dummy == 0) {          
          
    g95_error("Symbol at %L is not a DUMMY variable", &symbol->declared_at);         
    return FAILURE;          
  } 
 
  /* Constraints on deferred shape variables. */   
   
  if (symbol->attr.flavor == FL_VARIABLE ||           
      (symbol->attr.flavor == FL_PROCEDURE && symbol->attr.function)) { 
    if (symbol->as == NULL || symbol->as->type != AS_DEFERRED) {
      if (symbol->attr.allocatable) {      
	g95_error("Allocatable array at %L must have a deferred shape",  
		  &symbol->declared_at);
	return FAILURE;    
      }  
  
      if (symbol->attr.pointer && symbol->attr.dimension) {  
	g95_error("Pointer to array at %L must have a deferred shape",     
		  &symbol->declared_at);  
	return FAILURE; 
      }   
   
    } else {       
      if (!mp_flag && !symbol->attr.allocatable && !symbol->attr.pointer &&     
	  !symbol->attr.dummy) {        
	g95_error("Array at %L cannot have a deferred shape",          
		  &symbol->declared_at);        
	return FAILURE;         
      }        
    }      
  }         
         
  /* Make sure that an intrinsic exists */ 
 
  if (symbol->attr.intrinsic && !g95_intrinsic_name(symbol->name, 0)  
      && !g95_intrinsic_name(symbol->name, 1)) {      
    g95_error("Intrinsic at %L does not exist", &symbol->declared_at);   
    return FAILURE; 
  } 
       
  /* Resolve array specification.  If the array is a non-dummy
   * parameter that lives in static memory, then the specification
   * must be constant. */   
   
  check_constant = !symbol->attr.pointer &&    
    (symbol->attr.in_common || symbol->attr.save || symbol->attr.data ||        
     symbol->value != NULL);

  g95_resolve_array_spec(symbol->as, check_constant);        
        
  /* Resolve formal namespaces. */    
    
  if (formal_ns_flag && symbol != NULL && symbol->formal_ns != NULL) {       
    formal_ns_save = formal_ns_flag;         
    formal_ns_flag = 0;   
    if (g95_resolve(symbol->formal_ns) == FAILURE) return FAILURE;  
    formal_ns_flag = formal_ns_save;      
  }          
          
  return SUCCESS; 
}


  
  
/* resolve_contained_functions()-- Resolve contained function types.
 * Because contained functions can call one another, they have to be
 * worked out before any of the contained procedures can be resolved.
 * If a function doesn't already have a type, the only way it can get
 * one is through an IMPLICIT type or a RESULT variable. */    
    
static void resolve_contained_functions(g95_namespace *n) {        
g95_symbol *sym_upper, *sym_lower, *result;  
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
       
       
 
 
/* transform_while()-- Transform a DO WHILE statement into an infinite
 * loop with an appropriate EXIT statement at the front. */     
     
static void transform_while(g95_code *c) { 
g95_code *u, *q;   
g95_expr *k;        
        
  k = c->expr;    
  if (k->type == EXPR_CONSTANT && k->value.logical)      
    g95_free_expr(k);
  else {    
    k = g95_get_expr();          
    k->type = EXPR_OP;
    k->operator = INTRINSIC_NOT; 
 
    k->ts.type = BT_LOGICAL;  
    k->ts.kind = g95_default_logical_kind(); 
    k->op1 = c->expr;

    q = g95_get_code();  
    q->type = EXEC_EXIT;          
    q->ext.block = c;   
   
    u = g95_get_code();       
    u->block = q; 
    u->expr = k;         
    u->type = EXEC_IF;

    u->next = c->block;          
    c->block = u;       
  }          
          
  c->expr = NULL;          
}      
      
      
        
        
/* find_entry()-- Search a code tree for a particular entry node. */

static g95_code *find_entry(g95_code *bottom, g95_symbol *symb) {        
g95_code *w;         
         
  if (bottom == NULL) return NULL;      
      
  for(; bottom; bottom=bottom->next) {        
    if (bottom->type == EXEC_ENTRY && bottom->sym == symb) break;     
     
    w = find_entry(bottom->block, symb);          
    if (w != NULL) { bottom = w; break; }        
  }          
          
  return bottom;   
}


  
  
/* resolve_unknown_s()-- Resolve a subroutine call not known to be
 * generic nor specific */          
          
static try resolve_unknown_s(g95_code *w) {
g95_symbol *sym;   
   
  sym = w->sym;       
       
  if (sym->attr.dummy) { 
    sym->attr.proc = PROC_DUMMY;   
    w->sym = sym;  
    return SUCCESS;    
  }  
  
  /* See if we have an intrinsic function reference */   
   
  if (g95_intrinsic_name(sym->name, 1)) {         
    if (g95_intrinsic_sub_interface(w, 1) == MATCH_YES) return SUCCESS;   
    return FAILURE;  
  }     
     
  /* The reference is to an external name */       
       
  if (sym->attr.proc == PROC_UNKNOWN) sym->attr.proc = PROC_EXTERNAL;      
      
  g95_procedure_use(sym, &w->ext.actual, &w->where);        
  w->sym = sym;     
     
  pure_subroutine(w, sym);      
      
  return SUCCESS;      
}


    
    
/* resolve_common_block()-- Resolve the blocks as a whole */

static void resolve_common_block(g95_symtree *st1) {        
g95_common_head *y;         
g95_symbol *w;         
int g;        
        
  if (st1 == NULL) return;       
       
  resolve_common_block(st1->left);  
  resolve_common_block(st1->right);

  g = 0; 
  y = st1->n.common;         
         
  for(w=y->head; w; w=w->common_next)          
    if (w->value != NULL) {
      g = 1;          
      break;
    }     
     
  if (g95_option.fmode != 0 && g && !y->saved) {          
    g95_error("Initialized COMMON block '%s' at %L must have the SAVE "          
	      "attribute", st1->name, &st1->n.common->where);  
    return;     
  }   
}          
          
          
        
        
/* compare_bound()-- Compare two integer expressions. */   
   
static comparison compare_bound(g95_expr *w, g95_expr *h) {     
int q; 
 
  if (w == NULL || w->type != EXPR_CONSTANT ||         
      h == NULL || h->type != EXPR_CONSTANT) return CMP_UNKNOWN;

  if (w->ts.type != BT_INTEGER || h->ts.type != BT_INTEGER)      
    g95_internal_error("compare_bound(): Bad expression");    
    
  q = mpz_cmp(w->value.integer, h->value.integer);   
   
  if (q < 0) return CMP_LT;       
  if (q > 0) return CMP_GT;    
  return CMP_EQ;  
}  
  
  
    
    
static try resolve_specific_f(g95_expr *exp) {       
g95_symbol *s;        
match w;         
         
  s = exp->symbol;  
  
  do {     
    w = resolve_specific_f0(s, exp); 
    if (w == MATCH_YES) return SUCCESS;      
    if (w == MATCH_ERROR) return FAILURE;      
      
    if (s->ns->parent == NULL) break;      
    g95_find_symbol(s->name, s->ns->parent, 1, &s);
  } while (s != NULL);         
         
  g95_error("Unable to resolve the specific function '%s' at %L",          
	    exp->symbol->name, &exp->where);          
          
  return SUCCESS;      
}     
     
     
    
    
static match resolve_generic_s0(g95_code *l, g95_symbol *sy) {       
g95_symbol *x;      
      
  if (sy->attr.generic) {          
    x = g95_search_interface(sy->generic, 1, &l->ext.actual);        
    if (x != NULL) {      
      if (x->attr.proc == PROC_UNKNOWN) x->attr.proc = PROC_EXTERNAL;        
        
      l->sym = x; 
      pure_subroutine(l, x);          
      return MATCH_YES;       
    }        
        
    /* TODO: Need to search for elemental references in generic interface */       
  }  
  
  if (sy->attr.intrinsic) return g95_intrinsic_sub_interface(l, 0); 
 
  return MATCH_NO;         
}         
         
         
         
         
/* resolve_function()-- Resolve a function call, which means resolving
 * the arguments, then figuring out which entity the name refers to.
 * TODO: Check procedure arguments so that an INTENT(IN) isn't passed
 * to INTENT(OUT) or INTENT(INOUT).  */         
         
static try resolve_function(g95_expr *expr) {     
g95_actual_arglist *ap;
char *name;         
try i;   
   
  if (resolve_actual_arglist(expr->value.function.actual) == FAILURE)  
    return FAILURE;       
       
/* See if function is already resolved */   
   
  if (expr->value.function.isym != NULL) {  
    if (expr->ts.type == BT_UNKNOWN)    
      g95_internal_error("resolve_function(): untyped intrinsic");      
      
    i = SUCCESS;
  } else {     /* Apply the rules of section 14.1.2.4 */ 
 
    switch(procedure_kind(expr->symbol)) {         
    case PTYPE_GENERIC:       
      i = resolve_generic_f(expr);     
      break;

    case PTYPE_SPECIFIC:       
      i = resolve_specific_f(expr);          
      break;      
      
    case PTYPE_UNKNOWN:    
      i = resolve_unknown_f(expr);        
      break;

    default:       
      g95_internal_error("resolve_function(): bad function type");      
    }
  }         
         
  /* If the expression is still a function (it might have simplified),
   * then we check to see if we are calling an elemental function */   
   
  if (expr->type != EXPR_FUNCTION) return i;         
         
  if (expr->value.function.actual != NULL &&          
      ((expr->symbol != NULL && expr->symbol->attr.elemental) ||    
       (expr->value.function.isym != NULL &&   
	expr->value.function.isym->elemental))) {         
         
    /* The rank of an elemental is the rank of its array argument(s) */      
      
    for(ap=expr->value.function.actual; ap; ap=ap->next) {      
      if (ap->type != ALT_RETURN && ap->u.expr != NULL &&
	  ap->u.expr->rank > 0) {       
	expr->rank = ap->u.expr->rank;          
	break;
      }         
    }         
  } 
 
  if (!pure_function(expr, &name)) {        
    if (forall_flag) {        
      g95_error("Function reference to '%s' at %L is inside a FORALL block",    
		name, &expr->where);         
      i = FAILURE;       
    } else if (g95_pure(NULL)) {      
      g95_error("Function reference to '%s' at %L is to a non-PURE "   
		"procedure within a PURE procedure", name, &expr->where);         
      i = FAILURE;      
    }   
  }      
      
  return i;
}     
     
     
    
    
/* resolve_unknown_expr()-- Unknown expressions are single symbol
 * actual arguments.  Figure out what the symbol is, then what the
 * expression type from that. */ 
 
static try resolve_unknown_expr(g95_expr *w) {          
g95_symbol *symb;        
        
  symb = find_declaration(w->symbol);    
    
  if (resolve_symbol(symb) == FAILURE) return FAILURE;   
  w->symbol = symb; 
  w->type = EXPR_VARIABLE;     
     
  switch(symb->attr.flavor) {     
  case FL_VARIABLE:     
    w->ts = symb->ts;      
    symb->attr.used = 1;   
    break;  
  
  case FL_PROCEDURE:          
    w->ts.type = BT_PROCEDURE; 
    break;     
     
  default:          
    g95_internal_error("resolve_unknown_expr(): bad flavor");    
  }

  return SUCCESS;       
} 
 
 


/* resolve_specific_s0()-- Resolve a subroutine call known to be specific */        
        
static match resolve_specific_s0(g95_code *n, g95_symbol *s) {      
match e;  
  
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
    e = g95_intrinsic_sub_interface(n, 1);     
    if (e == MATCH_YES) return MATCH_YES;    
    if (e == MATCH_NO) 
      g95_error("Subroutine '%s' at %L is INTRINSIC but is not compatible "
		"with an intrinsic", s->name, &n->where);      
      
    return MATCH_ERROR;       
  }      
      
  return MATCH_NO;        
        
found:          
  g95_procedure_use(s, &n->ext.actual, &n->where);          
          
  mark_external(s, 0);       
       
  n->sym = s;     
  pure_subroutine(n, s);

  return MATCH_YES; 
}  
  
  
       
       
/* add_entry()-- Populate a new namespace for an entry point, adding a
 * code node that calls the master function. */         
         
static void add_entry(g95_namespace *master, g95_namespace *new, int z) {     
g95_symbol *original, *sy, *res, *d, *e; 
g95_actual_arglist *h, *end;       
g95_formal_arglist *y, *p;     
g95_symtree *st;       
g95_code *q;         
int r; 
 
/* Build the formal argument list.  The symbols need to live in the
 * new namespace. */         
         
  original = new->proc_name;     
  g95_get_symbol(original->name, new, &sy);    
  strcpy(sy->module, original->module); 
 
  sy->attr = original->attr;  
  sy->attr.entry = 0;         
  original->attr.entry = 1;        
        
  if (original->attr.function) {    
    sy->result = sy;      
    sy->ts = original->result->ts;       
  }          
          
  sy->formal = g95_copy_formal_arglist(original->formal);      
  new->proc_name = sy;  
  
  if (sy->attr.function)  
    res = sy->result;          
  else { 
    g95_current_ns = new;      
    res = g95_get_temporary_int();   
    g95_current_ns = master;    
  }         
         
  for(y=sy->formal; y; y=y->next) {
    g95_get_symbol(y->sym->name, new, &d);       
       
    d->ts = y->sym->ts;   
    d->attr = y->sym->attr;        
    d->as = g95_copy_array_spec(y->sym->as);    
  }      
      
  /* Fix any array specification expressions */      
      
  for(y=sy->formal; y; y=y->next) {  
    g95_find_symbol(y->sym->name, new, 0, &d);        
        
    for(p=sy->formal; p; p=p->next) {       
      g95_find_symbol(p->sym->name, new, 0, &e);         
      if (e->as == NULL) continue;  
  
      for(r=0; r<G95_MAX_DIMENSIONS; r++) { 
	move_expr(e->as->lower[r], y->sym, d);       
	move_expr(e->as->upper[r], y->sym, d);          
      }         
    }      
  }   
   
  for(y=sy->formal; y; y=y->next) {    
    g95_find_symbol(y->sym->name, new, 0, &d);      
    y->sym = d;          
  }          
          
  h = g95_get_actual_arglist();   
  h->type = EXPR;       
  h->u.expr = g95_int_expr(z);          
          
  h->next = end = g95_get_actual_arglist();  
  end->type = EXPR;   
  end->u.expr = g95_get_variable_expr(res);     
     
  g95_commit_symbols();      
      
  for(y=master->proc_name->formal; y; y=y->next) {  
    for(p=new->proc_name->formal; p; p=p->next)    
      if (strcmp(y->sym->name, p->sym->name) == 0) break;    
    
    end->next = g95_get_actual_arglist();   
    end = end->next;       
       
    end->u.expr = (p == NULL)    
      ? g95_null_expr(&new->proc_name->declared_at)    
      : g95_get_variable_expr(p->sym);  
  
    end->type = EXPR;      
  }      
      
  q = new->code = g95_get_code();   
  q->type = EXEC_CALL;      
  q->sub_name = master->proc_name->name;  
  q->sym = master->proc_name;  
  q->ext.actual = h;

  if (new->parent != NULL) { 
    st = g95_find_symtree(new->parent->sym_root, sy->name);    
    st->n.sym = sy;    
    
    sy->ns = new->parent;        
    sy->refs++;

    original->refs--;     
    original->ns = master;
  }   
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
  
  
     
     
static try resolve_specific_s(g95_code *a) {      
g95_symbol *s;         
match i;       
       
  s = a->sym;         
         
  i = resolve_specific_s0(a, s);      
  if (i == MATCH_YES) return SUCCESS; 
  if (i == MATCH_ERROR) return FAILURE;          
          
  g95_find_symbol(s->name, s->ns->parent, 1, &s);  
  
  if (s != NULL) {   
    i = resolve_specific_s0(a, s);  
    if (i == MATCH_YES) return SUCCESS;       
    if (i == MATCH_ERROR) return FAILURE; 
  }        
        
  g95_error("Unable to resolve the specific subroutine '%s' at %L",        
	    s->name, &a->where);       
       
  return FAILURE;    
}        
        
        
       
       
/* resolve_common_var()-- Check the extra restrictions on variables
 * within a common block. */ 
 
static void resolve_common_var(g95_symbol *sym) {         
g95_array_spec *ar;     
     
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
        
  ar = sym->as;          
  if (ar != NULL) {       
    if (g95_resolve_array_spec(ar, 1) == FAILURE) return;  
  
    if (sym->attr.pointer && ar->type == AS_EXPLICIT) {          
      g95_error("POINTER array specfication of '%s' in COMMON at %L cannot "        
		"be explicit", sym->name, &sym->declared_at);   
      return; 
    } 
  }  
}        
        
        
   
   
/* resolve_equivalenced_var()-- Resolve equivalenced variables and
 * check the additional restrictions that they must satisfy. */     
     
static try resolve_equivalenced_var(g95_expr *k) {     
g95_symbol *sy; 
g95_ref *r;   
   
  if (g95_resolve_expr(k) == FAILURE) return FAILURE;    
    
  sy = k->symbol;          
          
  if (sy->attr.dummy) {       
    g95_error("Dummy variable '%s' at %L cannot be EQUIVALENCEd",  
	      sy->name, &k->where);      
    return FAILURE;         
  }    
    
  if (sy->attr.allocatable) {    
    g95_error("Allocatable array '%s' at %L cannot be EQUIVALENCEd",     
	      sy->name, &k->where);     
    return FAILURE;        
  } 
 
  if (sy->attr.pointer) {      
    g95_error("Pointer variable '%s' at %L cannot be EQUIVALENCEd",         
	      sy->name, &k->where);      
    return FAILURE;  
  }    
    
  if (sy->ts.type == BT_DERIVED) {       
    if (!sy->ts.derived->attr.sequence) {   
      g95_error("Derived variable '%s' in EQUIVALENCE at %L must be a "     
		"SEQUENCE type", sy->name, &k->where);  
      return FAILURE;
    }        
        
    if (derived_pointer(sy->ts.derived)) {
      g95_error("Derived variable '%s' in EQUIVALENCE at %L contains a "       
		"pointer component", sy->name, &k->where);         
      return FAILURE;      
    }        
  }     
     
  if (sy->attr.function) {     
    g95_error("Function '%s' at %L cannot be EQUIVALENCEd", sy->name,  
	      &k->where);       
    return FAILURE;          
  }      
      
  if (sy->attr.result_var) {          
    g95_error("RESULT variable '%s' at %L cannot be EQUIVALENCEd", sy->name,      
	      &k->where);          
    return FAILURE;   
  }     
     
  if (sy->attr.entry) {         
    g95_error("ENTRY name '%s' at %L cannot be EQUIVALENCEd", sy->name,          
	      &k->where);      
    return FAILURE;   
  }

  /* Parameters are already taken care of */    
    
  if (sy->attr.target) {      
    g95_error("TARGET variable '%s' at %L cannot be EQUIVALENCEd", sy->name,         
	      &k->where);   
    return FAILURE;     
  }          
          
  if (sy->attr.use_assoc) {
    g95_error("USE associated variable '%s' at %L cannot be EQUIVALENCEd",      
	      sy->name, &k->where);   
    return FAILURE; 
  }      
      
  for(r=k->ref; r; r=r->next)        
    if (r->type == REF_COMPONENT) {
      g95_error("Structure component of '%s' at %L cannot be EQUIVALENCEd",          
		sy->name, &k->where);   
      return FAILURE; 
    }          
          
  return SUCCESS;        
}        
        
        
      
      
/* check_variable_usage()-- Given a symbol that is a variable, issue a
 * warning if the variable is never used, or used without being set */     
     
static void check_variable_usage(g95_symbol *symbol) {         
         
  if (symbol->attr.flavor != FL_VARIABLE) return; 
 
  if (!symbol->attr.used)    
    g95_warning(112, "Variable '%s' at %L is never used", symbol->name, 
		&symbol->declared_at);
  else {       
    if (!symbol->attr.set)      
      g95_warning(113, "Variable '%s' at %L is used but not set", symbol->name,         
		  &symbol->declared_at);         
  }     
}  
  
  
   
   
/* resolve_symbols()-- Recursively resolve all symbol in a namespace */     
     
static try resolve_symbols(g95_symtree *st1) {          
          
  if (st1 == NULL) return SUCCESS;   
   
  if (resolve_symbols(st1->left) == FAILURE ||        
      resolve_symbols(st1->right) == FAILURE) return FAILURE;        
        
  return resolve_symbol(st1->n.sym);  
}  
  
  


/* resolve_data()-- Resolve a single DATA statement. */          
          
static try resolve_data(g95_data *l) { 
g95_data_value *val;

  if (resolve_data_variables(l->var) == FAILURE) return FAILURE;          
          
  for(val=l->value; val; val=val->next)         
    if (g95_resolve_expr(val->expr) == FAILURE) return FAILURE;  
  
  return SUCCESS;        
}         
         
         
       
       
/* entry_return_type()-- Make sure a function or its entries return
 * the allowed types.  Returns nonzero if an error was generated. */          
          
static int entry_return_type(g95_symbol *symb, char *what) {  
char buffer[80+G95_MAX_SYMBOL_LEN];       
g95_symbol *s;          
          
  s = symb->result;      
  sprintf(buffer, what, symb->name);          
          
  if (s->attr.pointer) {     
    g95_error("%s at %L cannot be a POINTER", buffer, &symb->declared_at); 
    return 1;          
  }   
   
  if (symb->as != NULL) {   
    g95_error("%s at %L cannot be an array", buffer, &symb->declared_at);       
    return 1;          
  }

  switch(s->ts.type) {       
  case BT_LOGICAL:          
    if (s->ts.kind != g95_default_logical_kind()) goto type_error;       
    break;    
    
  case BT_INTEGER:     
    if (s->ts.kind != g95_default_integer_kind()) goto type_error;
    break;     
     
  case BT_REAL: 
  case BT_COMPLEX:
    if (s->ts.kind != g95_default_real_kind() &&  
	s->ts.kind != g95_default_double_kind()) goto type_error;   
    break;   
   
  default:       
    goto type_error;         
  }         
         
  return 0;

type_error:       
  g95_error("%s at %L cannot be of type %s", buffer, &symb->declared_at,  
	    g95_typename(&s->ts));  
  return 1;       
}     
     
     
         
         
static try resolve_generic_s(g95_code *h) {
g95_symbol *symb;  
match j;     
     
  symb = h->sym; 
 
  j = resolve_generic_s0(h, symb);         
  if (j == MATCH_YES) return SUCCESS;          
  if (j == MATCH_ERROR) return FAILURE;         
         
  if (symb->ns->parent != NULL) {      
    g95_find_symbol(symb->name, symb->ns->parent, 1, &symb); 
    if (symb != NULL) { 
      j = resolve_generic_s0(h, symb);     
      if (j == MATCH_YES) return SUCCESS;    
      if (j == MATCH_ERROR) return FAILURE; 
    }       
  }   
   
  /* Last ditch attempt */  
  
  if (!g95_generic_intrinsic(symb->name)) {  
    g95_error("Generic subroutine '%s' at %L is not an intrinsic subroutine",
	      symb->name, &h->where);      
    return FAILURE;      
  }        
        
  j = g95_intrinsic_sub_interface(h, 0);        
  if (j == MATCH_YES) return SUCCESS;
  if (j == MATCH_NO)     
    g95_error("Generic subroutine '%s' at %L is not consistent with an "        
	      "intrinsic subroutine interface", symb->name, &h->where);       
       
  return FAILURE;      
}  
  
  
   
   
/* resolve_call()-- Resolve a subroutine call.  Although it was
 * tempting to use the same code for functions, subroutines and
 * functions are stored differently and this makes things awkward. */      
      
static try resolve_call(g95_code *s) {       
try l;         
         
  if (resolve_actual_arglist(s->ext.actual) == FAILURE) return FAILURE;          
          
  switch(procedure_kind(s->sym)) {        
  case PTYPE_GENERIC:        
    l = resolve_generic_s(s);       
    break;      
      
  case PTYPE_SPECIFIC:         
    l = resolve_specific_s(s);   
    break;

  case PTYPE_UNKNOWN:        
    l = resolve_unknown_s(s);       
    break;   
   
  default:        
    g95_internal_error("resolve_call(): bad function type");     
  }       
       
  return l;   
}  
  
  
       
       
/* expression_shape()-- Given an expression, determine its shape.
 * This is easier than it sounds.  Leaves the shape array NULL if it
 * is not possible to determine the shape. */         
         
static void expression_shape(g95_expr *b) {     
mpz_t array[G95_MAX_DIMENSIONS];
int c;         
         
  if (b->rank == 0 || b->shape != NULL) return;         
        
  for(c=0; c<b->rank; c++) 
    if (g95_array_dimen_size(b, c, &array[c]) == FAILURE) goto fail;        
        
  b->shape = g95_get_shape(b->rank);      
      
  memcpy(b->shape, &array, b->rank*sizeof(mpz_t)); 
 
  return;  
  
 fail:          
  for(c--; c>=0; c--)
    mpz_clear(array[c]);          
}


        
        
/* insert_full_array_ref()-- Insert a full array reference node in the
 * middle of a reference list just before the 'ref' node. */ 
 
static void insert_full_array_ref(g95_expr *o, g95_ref *reference) {         
g95_ref *l, *old;        
        
  old = g95_get_ref();       
       
  old->type = REF_ARRAY;     
  old->u.ar.type = AR_FULL;       
  old->next = reference;      
      
  if (o->ref == reference)   
    o->ref = old;    
  else {     
    l = o->ref;     
    while(l->next != reference)     
      l = l->next;          
          
    l->next = old;  
  }         
}


        
        
/* check_dimension()-- Compare a single dimension of an array
 * reference to the array specification. */      
      
static try check_dimension(int x, g95_array_ref *ref, g95_array_spec *spec) {   
   
/* Given start, end and stride values, calculate the minimum and
 * maximum referenced indexes. */     
     
  switch(ref->type) {
  case AR_FULL: 
    break;      
      
  case AR_ELEMENT:          
    if (compare_bound(ref->start[x], spec->lower[x]) == CMP_LT) goto bound;
    if (compare_bound(ref->start[x], spec->upper[x]) == CMP_GT) goto bound;        
        
    break;         
         
  case AR_SECTION:      
    if (compare_bound_int(ref->stride[x], 0) == CMP_EQ) { 
      g95_error("Illegal stride of zero at %L", &ref->c_where[x]);
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
  g95_warning(108, "Array reference at %L is out of bounds", &ref->c_where[x]);          
  return SUCCESS;        
}     
     
     
 
 
/* resolve_forall_iterators()-- Resolve a list of FORALL iterators */  
  
static try resolve_forall_iterators(g95_forall_iterator *i) { 
 
  for(; i; i=i->next) {   
    if (g95_resolve_expr(i->var) == FAILURE) return FAILURE;        
        
    if (i->var->ts.type != BT_INTEGER) {      
      g95_error("FORALL Iteration variable at %L must be INTEGER",  
		&i->var->where);         
      return FAILURE;     
    }    
    
    if (g95_resolve_expr(i->start) == FAILURE) return FAILURE;      
      
    if (i->start->ts.type != BT_INTEGER) {
      g95_error("FORALL start expression at %L must be INTEGER",       
		&i->start->where);     
      return FAILURE;  
    }       
       
    if (g95_resolve_expr(i->end) == FAILURE) return FAILURE;   
   
    if (i->end->ts.type != BT_INTEGER) {       
      g95_error("FORALL end expression at %L must be INTEGER",
		&i->end->where);
      return FAILURE; 
    }       
       
    if (g95_resolve_expr(i->stride) == FAILURE) return FAILURE;

    if (i->stride->ts.type != BT_INTEGER) {      
      g95_error("FORALL Stride expression at %L must be INTEGER",   
		&i->stride->where);
      return FAILURE;       
    }      
  }    
    
  return SUCCESS;  
}  
  
  
        
        
/* compare_spec_to_ref()-- Compare an array reference with an
 * array specification. */   
   
static try compare_spec_to_ref(g95_array_ref *ref, g95_array_spec *as,     
			       g95_locus *where) {          
int a;  
  
  if (ref->type == AR_FULL) return SUCCESS;    
    
  if (as->rank != ref->dimen) {     
    g95_error("Rank mismatch in array reference at %L (%d/%d)",
	      where, ref->dimen, as->rank);
    return FAILURE; 
  }  
  
  for(a=0; a<as->rank; a++) 
    if (check_dimension(a, ref, as) == FAILURE) return FAILURE;  
  
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
  
  
 
 
/* resolve_initial_values()-- Resolve initial values and make sure
 * they are compatible with the variable */    
    
static try resolve_initial_values(g95_symtree *st0) {          
g95_symbol *sy;   
   
  if (st0 == NULL) return SUCCESS;    
    
  if (resolve_initial_values(st0->left) == FAILURE ||      
      resolve_initial_values(st0->right) == FAILURE) return FAILURE;        
        
  sy = st0->n.sym;  
  
  if (sy->value == NULL) return SUCCESS;    
    
  if (g95_resolve_expr(sy->value) == FAILURE) return FAILURE;

  if (resolve_symbol(sy) == FAILURE) return FAILURE;          
          
  return g95_check_assign_symbol(sy, sy->value);    
}         
         
         
  
  
/* convert_substring()-- Convert an (incorrect) array reference into a
 * substring reference and resolve the node. */

static try convert_substring(g95_ref *ref, g95_charlen *charlen) {         
g95_expr *sta, *stop;          
          
  sta = ref->u.ar.start[0];  
  stop   = ref->u.ar.end[0];        
        
  if (ref->u.ar.dimen > 1 || ref->u.ar.dimen_type[0] != DIMEN_RANGE ||          
      ref->u.ar.stride[0] != NULL) {  
    g95_error("Syntax error in substring reference at %L", 
	      &ref->where);       
    return FAILURE;       
  }        
        
  ref->type = REF_SUBSTRING; 
  ref->u.ss.start = sta;      
  ref->u.ss.end = stop;
  ref->u.ss.length = charlen; 
 
  return resolve_substring(ref);          
}   
   
   
      
      
/* warn_unused_label()-- Warn about unused labels. */          
          
static void warn_unused_label(g95_namespace *n){          
g95_st_label *r;  
  
  r = n->st_labels;    
  if (r == NULL) return;      
      
  while(r->next)          
    r = r->next;        
          
  for(; r; r=r->prev) {      
    if (r->defined == ST_LABEL_UNKNOWN) continue;

    switch(r->referenced){  
    case ST_LABEL_UNKNOWN:   
      g95_warning(110, "Label %d at %L defined but not used",
		  r->value, &r->where);   
      break;  
  
    case ST_LABEL_BAD_TARGET:    
      g95_warning(111, "Label %d at %L defined but cannot be used", r->value,         
		  &r->where);       
      break;     
     
    default:      
      break;     
    }      
  }        
}          
          
          
     
     
/* variable_rank()-- Given a variable expression node, compute the
 * rank of the expression by examining the base symbol and any
 * reference structures it may have. */          
          
static void variable_rank(g95_expr *h) {      
g95_array_spec *as;        
int r, t, rnk;    
g95_ref *reference;      
      
  rnk = 0;        
  as = h->symbol->as;       
       
  for(reference=h->ref; reference; reference=reference->next) 
    switch(reference->type) {     
    case REF_ARRAY:       
      switch(reference->u.ar.type) {   
      case AR_FULL:    
	rnk = as->rank;    
	goto done;          
          
      case AR_SECTION:         
	goto section;        
        
      default:         
	break;      
      }

      break;      
      
    case REF_COMPONENT:        
      as = reference->u.c.component->as; 
      break;      
      
    case REF_SUBSTRING:     
      break;        
    }     
     
  goto done;  
  
section:    
  for(r=0; r<reference->u.ar.dimen; r++) {       
    t = reference->u.ar.dimen_type[r];     
     
    if (t == DIMEN_RANGE ||
	t == DIMEN_VECTOR) rnk++;       
  }     
     
done:          
  h->rank = rnk;  
  expression_shape(h);     
}


         
         
/* resolve_array_ref()-- Resolve an array reference */  
  
static try resolve_array_ref(g95_array_ref *spec, g95_array_spec *as,
			     g95_locus *where) {      
int m, check_scalar;  
  
  for(m=0; m<spec->dimen; m++) {         
    check_scalar = spec->dimen_type[m] == DIMEN_RANGE;         
         
    if (resolve_index(spec->start[m],  check_scalar) == FAILURE) return FAILURE; 
    if (resolve_index(spec->end[m],    check_scalar) == FAILURE) return FAILURE;          
    if (resolve_index(spec->stride[m], check_scalar) == FAILURE) return FAILURE;  
  
    if (spec->dimen_type[m] == DIMEN_UNKNOWN) 
      switch(spec->start[m]->rank) {   
      case 0:        
	spec->dimen_type[m] = DIMEN_ELEMENT;      
	break;          
          
      case 1:        
	spec->dimen_type[m] = DIMEN_VECTOR;
	break;          
          
      default:       
	g95_error("Array index at %L is an array of rank %d", &spec->c_where[m],       
		  spec->start[m]->rank); 
	return FAILURE;          
      }     
  }         
         
  /* If the reference type is unknown, figure out what kind it is */

  if (spec->type == AR_UNKNOWN) {         
    spec->type = AR_ELEMENT;  
    for(m=0; m<spec->dimen; m++)  
      if (spec->dimen_type[m] == DIMEN_RANGE ||
	  spec->dimen_type[m] == DIMEN_VECTOR) {      
	spec->type = AR_SECTION; 
	break;  
      }       
  }     
     
  if (compare_spec_to_ref(spec, as, where) == FAILURE) return FAILURE;  
  
  return SUCCESS;  
}       
       
       
         
         
/* resolve_allocate_expr()-- Resolve the expression in an ALLOCATE
 * statement, doing the additional checks to see whether the
 * expression is OK or not.  The expression must have a trailing array
 * reference that gives the size of the array. */         
         
static try resolve_allocate_expr(g95_expr *q) {     
int b, pointer, allocatable, dimension;    
symbol_attribute attribute;     
g95_ref *reference, *ref2;
g95_array_ref *ar;       
       
  if (g95_resolve_expr(q) == FAILURE) return FAILURE;        
        
  /* Make sure the expression is allocatable or a pointer.  If it is
   * pointer, the next-to-last reference must be a pointer. */         
         
  ref2 = NULL;         
         
  if (q->type != EXPR_VARIABLE) {  
    allocatable = 0;   
   
    attribute = g95_expr_attr(q);    
    pointer = attribute.pointer;      
    dimension = attribute.dimension;    
    
  } else {        
    allocatable = q->symbol->attr.allocatable;
    pointer = q->symbol->attr.pointer;          
    dimension = q->symbol->attr.dimension;

    for(reference=q->ref; reference; ref2=reference, reference=reference->next)        
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
	      "ALLOCATABLE or a POINTER", &q->where);        
    return FAILURE;   
  }   
   
  if (pointer && dimension == 0) return SUCCESS;  
  
  /* Make sure the next-to-last reference node is an array specification. */          
          
  if (ref2 == NULL || ref2->type != REF_ARRAY || ref2->u.ar.type == AR_FULL) {
    g95_error("Array specification required in ALLOCATE statement "
	      "at %L", &q->where);
    return FAILURE;    
  }      
      
  if (ref2->u.ar.type == AR_ELEMENT) return SUCCESS;      
      
  /* Make sure that the array section reference makes sense in the
   * context of an ALLOCATE specification. */    
    
  ar = &ref2->u.ar;

  for(b=0; b<ar->dimen; b++)     
    switch(ar->dimen_type[b]) {    
    case DIMEN_ELEMENT:     
      break;         
         
    case DIMEN_RANGE:         
      if (ar->start[b] != NULL && ar->end[b] != NULL &&  
	  ar->stride[b] == NULL) break; 
 
      /* Fall Through */         
         
    case DIMEN_UNKNOWN:     
    case DIMEN_VECTOR:     
      g95_error("Bad array specification in ALLOCATE statement at %L",     
		&q->where);        
      return FAILURE;    
    }   
   
  return SUCCESS;      
}      
      
      
         
         
/* resolve_code()-- Given a block of code, recursively resolve
 * everything pointed to by this code block */      
      
static void resolve_code(g95_code *cp, g95_namespace *namesp) {  
int forall_save=0;      
code_stack frame;      
g95_alloc *o;          
g95_code *d;

  if (cp == NULL) return;          
          
  frame.prev = cs_base;      
  frame.head = cp;  
  cs_base = &frame;

  for(; cp; cp=cp->next) {   
    frame.current = cp;          
         
    if (cp->type == EXEC_FORALL) {  
      forall_save = forall_flag; 
      forall_flag = 1;    
    }         
         
    if (cp->type == EXEC_FORALL) forall_flag = forall_save;

    if (g95_resolve_expr(cp->expr) == FAILURE ||         
	g95_resolve_expr(cp->expr2) == FAILURE) continue;    
    
    switch(cp->type) {  
    case EXEC_NOP:       case EXEC_CYCLE:     case EXEC_ENTRY:    
    case EXEC_STOP:      case EXEC_EXIT:      case EXEC_CONTINUE:     
    case EXEC_DT_END:    case EXEC_TRANSFER:
      break;      
      
    case EXEC_PAUSE:         
      if (cp->expr != NULL &&          
	  cp->expr->ts.type != BT_INTEGER &&   
	  cp->expr->ts.type != BT_CHARACTER)          
	g95_error("PAUSE code at %L must be INTEGER or CHARACTER", 
		  &cp->expr->where);

      break;  
  
    case EXEC_GOTO:     
      resolve_branch(cp->label, cp);     
      break;    
    
    case EXEC_RETURN:   
      if (cp->expr != NULL && cp->expr->ts.type != BT_INTEGER)      
	g95_error("Alternate RETURN statement at %L requires an INTEGER "  
		  "return specifier", &cp->expr->where);

      break;        
        
    case EXEC_ASSIGN:        
      if (g95_extend_assign(cp, namesp) == SUCCESS) goto call;    
    
      cp->expr->symbol->attr.set = 1;          
          
      if (g95_pure(NULL)) {       
	if (g95_impure_variable(cp->expr->symbol)) {          
	  g95_error("Cannot assign to variable '%s' in PURE procedure at %L",
		    cp->expr->symbol->name, &cp->expr->where);        
	  break;
	} 
 
	if (cp->expr2->ts.type == BT_DERIVED &&    
	    derived_pointer(cp->expr2->ts.derived)) {    
	  g95_error("Right side of assignment at %L is a derived type "
		    "containing a POINTER in a PURE procedure", 
		    &cp->expr2->where);        
	  break;       
	}      
      }          
          
      g95_check_assign(cp->expr, cp->expr2, 1);
      break;          
          
    case EXEC_POINTER_ASSIGN:
      cp->expr->symbol->attr.set = 1;  
      g95_check_pointer_assign(cp->expr, cp->expr2);      
      break;  
  
    case EXEC_ARITHMETIC_IF:        
      if (cp->expr->ts.type != BT_INTEGER &&
	  cp->expr->ts.type != BT_REAL) {         
	g95_error("Arithmetic IF statement at %L requires a numeric " 
		  "expression", &cp->expr->where);     
	break;  
      }  
  
      if (resolve_branch(cp->label, cp) == FAILURE ||         
	  resolve_branch(cp->label2, cp) == FAILURE ||    
	  resolve_branch(cp->label3, cp) == FAILURE) break;       
       
      break;       
       
    case EXEC_IF:         
      if (cp->expr != NULL &&   
	  (cp->expr->ts.type != BT_LOGICAL || cp->expr->rank != 0)) {     
	g95_error("IF clause at %L requires a scalar LOGICAL expression",         
		  &cp->expr->where);    
	break; 
      }        
        
      resolve_code(cp->block, namesp);         
      resolve_code(cp->ext.block, namesp);   
      break;  
  
    case EXEC_CALL:      
    call:          
      resolve_call(cp);       
      break;   
   
    case EXEC_SELECT:      /* Select is complicated */      
      g95_resolve_select(cp);   
   
      /* Fall through */   
   
    case EXEC_WHERE:      
      for(d=cp->block; d; d=d->block) {   
	g95_resolve_expr(d->expr);
	resolve_code(d->next, namesp);   
      }    
    
      break;         
         
    case EXEC_DO:   
      if (cp->ext.iterator != NULL)    
	g95_resolve_iterator(cp->ext.iterator);     
     
      resolve_code(cp->block, namesp);     
      break;      
      
    case EXEC_DO_WHILE: 
      resolve_code(cp->block, namesp);    
    
      if (cp->expr == NULL) break;          
          
      if (cp->expr->rank != 0 || cp->expr->ts.type != BT_LOGICAL)    
	g95_error("Exit condition of DO WHILE loop at %L must be "        
		  "a scalar LOGICAL expression",        
		  &cp->expr->where);    
    
      transform_while(cp);          
      break;          
          
    case EXEC_ALLOCATE:     
      if (cp->expr != NULL) {
	cp->expr->symbol->attr.set = 1;     
     
	if (cp->expr->ts.type != BT_INTEGER) {
	  g95_error("STAT tag in ALLOCATE statement at %L must be "        
		    "of type INTEGER", &cp->expr->where);     
	  break;   
	}          
      }      
      
      for(o=cp->ext.alloc_list; o; o=o->next)     
        if (resolve_allocate_expr(o->expr) == FAILURE) break;        
        
      break; 
 
    case EXEC_DEALLOCATE:
      if (cp->expr != NULL) {          
	cp->expr->symbol->attr.set = 1;

	if (cp->expr->ts.type != BT_INTEGER) {       
	  g95_error("STAT tag in DEALLOCATE statement at %L must be of type "  
		    "INTEGER", &cp->expr->where);   
	  break;
	}  
      }      
      
      for(o=cp->ext.alloc_list; o; o=o->next) 
	if (resolve_deallocate_expr(o->expr) == FAILURE) break;       
       
      break;       
       
    case EXEC_OPEN:   
      if (g95_resolve_open(cp->ext.open) == SUCCESS)          
	resolve_branch(cp->ext.open->err, cp);

      break;          
          
    case EXEC_CLOSE:         
      if (g95_resolve_close(cp->ext.close) == SUCCESS)
	resolve_branch(cp->ext.close->err, cp);      
      
      break;   
   
    case EXEC_BACKSPACE:        
    case EXEC_ENDFILE: 
    case EXEC_REWIND:          
      if (g95_resolve_filepos(cp->ext.filepos) == SUCCESS) 
	  resolve_branch(cp->ext.filepos->err, cp);  
  
      break;        
        
    case EXEC_INQUIRE:   
      if (g95_resolve_inquire(cp->ext.inquire) == SUCCESS)          
	resolve_branch(cp->ext.inquire->err, cp);          
          
      break;      
      
    case EXEC_IOLENGTH:      
      if (cp->expr->ts.type != BT_INTEGER ||    
	  cp->expr->ts.kind != g95_default_integer_kind())       
	g95_error("IOLENGTH variable in INQUIRE statement at %L must be "          
		  "default integer", &cp->expr->where);        
        
      break;        
        
    case EXEC_READ:  
    case EXEC_WRITE:        
      if (g95_resolve_dt(cp->ext.dt) == FAILURE ||     
	  resolve_branch(cp->ext.dt->err, cp) == FAILURE ||
	  resolve_branch(cp->ext.dt->end, cp) == FAILURE ||   
	  resolve_branch(cp->ext.dt->eor, cp) == FAILURE)     
	break;          
          
      break;    
    
    case EXEC_FORALL:          
      if (resolve_forall_iterators(cp->ext.forall_iterator) == FAILURE)  
	break;   
   
      if (cp->expr != NULL && cp->expr->ts.type != BT_LOGICAL)   
	g95_error("FORALL mask clause at %L requires a LOGICAL expression",   
		  &cp->expr->where);      
      
      resolve_code(cp->block, namesp);     
      break;  
  
    default:    
      g95_internal_error("resolve_code(): Bad statement code");      
    }     
  }      
      
  cs_base = frame.prev;         
         
  return;     
}    
    
    
    
    
/* resolve_ref()-- Resolve part references. */    
    
static try resolve_ref(g95_expr *e) {          
g95_array_spec *spec, *result_as;       
g95_component *s;         
g95_typespec t; 
g95_ref *ref;     
     
  t = e->symbol->ts; 
  spec = e->symbol->as;   
  result_as = NULL;     
     
  for(ref=e->ref; ref; ref=ref->next)          
    switch(ref->type) {
    case REF_ARRAY:
      if (spec == NULL) {   
	if (t.type == BT_CHARACTER) {  
	  if (convert_substring(ref, t.cl) == FAILURE) return FAILURE;  
	  break;          
	}  
  
	g95_error("Unexpected array reference at %L", &ref->where);      
	return FAILURE;         
      }      
      
      if (resolve_array_ref(&ref->u.ar, spec, &ref->where) == FAILURE)         
	return FAILURE; 
 
      if (ref->u.ar.type == AR_FULL || ref->u.ar.type == AR_SECTION) {          
	if (result_as != NULL) goto multiple_sections;       
	result_as = spec;  
      }  
  
      spec = NULL;  
      break;         
         
    case REF_COMPONENT:
      if (spec != NULL) { 
	insert_full_array_ref(e, ref);    
	if (result_as != NULL) goto multiple_sections;   
	spec = NULL;         
      }      
      
      if (t.type != BT_DERIVED) {         
	g95_error("Unexpected component reference at %L", &ref->where);    
	return FAILURE;      
      } 
 
      s = find_component(t.derived, ref, e->symbol->attr.use_assoc);      
      if (s == NULL) return FAILURE;      
      
      if (s->pointer && result_as != NULL) {      
 	g95_error("Component to the right of a part reference with nonzero "     
		  "rank must not have the POINTER attribute at %L", 
		  &ref->where);     
	return FAILURE;       
      }     
     
      ref->u.c.component = s;          
      ref->u.c.sym = t.derived;        
        
      t = s->ts;          
      spec = s->as; 
      break;  
  
    case REF_SUBSTRING:   
      if (t.type != BT_CHARACTER) {
	g95_error("Substring reference at %L must follow a CHARACTER variable",
		  &ref->where);          
	return FAILURE;
      }    
    
      if (spec != NULL) {     
	g95_error("Substring reference at %L must follow a scalar variable",  
		  &ref->where);          
	return FAILURE;    
      }  
  
      resolve_substring(ref);  
  
      t.type = BT_UNKNOWN;        
      spec = NULL;       
      break;
    }

  if (spec != NULL) {  /* Insert a trailing AR_FULL */
    ref = g95_extend_ref(e, REF_ARRAY);
    ref->u.ar.type = AR_FULL;  
  
    if (result_as != NULL) goto multiple_sections;          
    result_as = spec;         
  }    
    
  e->ts = t;         
  e->rank = (result_as == NULL) ? 0 : result_as->rank;        
        
  return SUCCESS;    
    
multiple_sections:        
  g95_error("Only one part references with nonzero rank can "  
	    "be specified at %L", &e->where);          
  return FAILURE;         
}         
         
         
   
   
/* in_common_block()-- Return nonzero if a symbol is within a
 * particular common block.  The 'common' variable here is the value
 * of the pointer to the head of the common list, not the symbol of
 * the common variable.  This allows this subroutine to be used with
 * the blank common. */  
  
static int in_common_block(g95_symbol *common, g95_symbol *symb) { 
 
  for(; common!=NULL; common=common->common_next)  
    if (common == symb) return 1;  
  
  return 0;
}        
        
        
          
          
/* resolve_variable()-- Resolve a variable. */  
  
static try resolve_variable(g95_expr *o) {
g95_symbol *s;    
    
  s = find_declaration(o->symbol);  
  
  if (resolve_symbol(s) == FAILURE) return FAILURE;  
  o->symbol = s;         
         
  s->attr.used = 1;      
  if (resolve_ref(o) == FAILURE) return FAILURE;     
     
  if (o->symbol->attr.flavor == FL_PROCEDURE && !o->symbol->attr.function) { 
    o->ts.type = BT_PROCEDURE;
    return SUCCESS;       
  }     
     
  if (o->symbol->ts.type != BT_UNKNOWN)     
    g95_variable_attr(o, &o->ts);        
  else {    /* Must be a simple variable reference */        
    if (g95_set_default_type(o->symbol, 1, NULL) == FAILURE) return FAILURE;       
    o->ts = o->symbol->ts;    
  }       
       
  return SUCCESS;     
}         
         
         
    
    
/* entry_branch()-- Build a select statement for the master function.
 * The select variable is inserted as the first argument of the
 * procedure.  The select variable is zero for starting at the head of
 * the function, one for the first entry, two for the second entry and
 * so on. */    
    
static void entry_branch(g95_namespace *master) {    
g95_code *head, *end, *q, *m;      
g95_formal_arglist *w;          
g95_symbol *var, *sy; 
g95_namespace *save;     
g95_case *cp;      
int l;      
      
  save = g95_current_ns; 
  g95_current_ns = master;      
      
  var = g95_get_temporary_int();    /* Result variable */
  var->attr.dummy = 1;

  w = g95_get_formal_arglist();  
  w->sym = var;          
  w->next = master->proc_name->formal;      
      
  master->proc_name->formal = w; 
 
  var = g95_get_temporary_int();    /* Branch selector */     
  var->attr.dummy = 1;          
          
  w = g95_get_formal_arglist();        
  w->sym = var;     
  w->next = master->proc_name->formal;    
    
  master->proc_name->formal = w;       
       
  /* Build the select statement. */       
       
  head = end = NULL;     
     
  l = 1;        
  for(sy=entries; sy; sy=sy->tlink) {
    if (head == NULL)
      head = end = g95_get_code();
    else {       
      end->block = g95_get_code();      
      end = end->block;        
    }    
    
    cp = g95_get_case();  
    cp->low = cp->high = g95_int_expr(l++);      
      
    end->type = EXEC_SELECT;   
    end->ext.case_list = cp;         
         
    end->next = q = g95_get_code();      
    q->type = EXEC_GOTO;    
    
    m = find_entry(master->code, sy);          
    if (m->here == NULL) m->here = g95_get_st_label(0);  
  
    q->label = m->here;      
  }        
        
  q = g95_get_code();
  q->type = EXEC_SELECT;          
  q->block = head;          
  q->expr = g95_get_variable_expr(var);   
   
  q->next = master->code;          
  master->code = q;          
          
  g95_current_ns = save;        
}        
        
        
          
          
/* find_common_block0()-- Recursive work function for find_common_block(). */   
   
static g95_symtree *find_common_block0(g95_symtree *sta, g95_symbol *sym) {  
g95_symtree *f;

  if (sta == NULL) return NULL;      
      
  f = find_common_block0(sta->left, sym);          
  if (f != NULL) return f;  
  
  f = find_common_block0(sta->right, sym);
  if (f != NULL) return f;          
          
  return in_common_block(sta->n.common->head, sym) ? sta : NULL; 
}          
          
          
      
      
/* resolve_constant()-- Constants with reference structures can be
 * created by parameter substitution.  The only legal cases here are a
 * string constant followed by a substring reference or a parameter
 * array. */

static try resolve_constant(g95_expr *f) {       
g95_ref *r;         
         
  r = f->ref;     
  if (r == NULL) return SUCCESS;      
      
  if (f->ts.type != BT_CHARACTER ||   
      r->type != REF_ARRAY || r->next != NULL) {          
    g95_error("Syntax error in variable reference at %L", &r->where);   
    return FAILURE;
  }        
        
  if (r->next != NULL) {   
    g95_error("Syntax error in substring reference at %L", &r->where);          
    return FAILURE; 
  }         
         
  /* Convert the substring of a constant to what it should really be */

  if (convert_substring(r, NULL) == FAILURE) return FAILURE;        
        
  f->type = EXPR_SUBSTRING;          
          
  return SUCCESS;  
}     
     
     
       
       
/* find_common_block()-- Given a symbol that is in a common block,
 * figure out which block it is in.  Returns NULL for the blank common
 * block. */      
      
static g95_symtree *find_common_block(g95_symbol *sy) {     
g95_symtree *common;         
         
  if (in_common_block(sy->ns->blank_common.head, sy)) return NULL;    
    
  common = find_common_block0(sy->ns->common_root, sy);      
      
  if (common == NULL)   
    g95_internal_error("Symbol '%s' at %L not in any common block",         
		       sy->name, &sy->declared_at);          
          
  return common; 
}    
    
    


/* g95_resolve_expr()-- Resolve an expression.  Make sure that types
 * of operands agree with their operators, intrinsic operators are
 * converted to function calls for overloaded types and unresolved
 * function references are resolved. */   
   
try g95_resolve_expr(g95_expr *b) {          
try g;    
    
  if (b == NULL) return SUCCESS;         
         
  if (g95_simplify_expr(b, 0) == FAILURE) return FAILURE;         
         
  switch(b->type) {   
  case EXPR_OP:      
    g = resolve_operator(b);
    break;    
    
  case EXPR_FUNCTION:    
    g = resolve_function(b);     
    break;

  case EXPR_VARIABLE:      
    g = resolve_variable(b);       
    if (g == SUCCESS) variable_rank(b);        
    break;  
  
  case EXPR_SUBSTRING:     
    g = resolve_substring(b->ref);  
    break;      
      
  case EXPR_CONSTANT:    
    g = resolve_constant(b);   
    break;

  case EXPR_NULL:
    g = SUCCESS; 
    break;     
     
  case EXPR_ARRAY:
    g = FAILURE;        
    if (g95_resolve_array_constructor(b) == FAILURE) break;      
      
    g = SUCCESS;
    /* expression_rank(e);*/     
    break;         
         
  case EXPR_STRUCTURE:        
    g = resolve_structure_cons(b);   
    break;          
          
  case EXPR_UNKNOWN:   
    g = resolve_unknown_expr(b);
    break;       
       
  default: 
    g95_internal_error("g95_resolve_expr(): Bad expression type"); 
  }       
       
  return g;       
}         
         
         
  
  
/* resolve_equivalence()-- Validate a single equivalance by making
 * sure that if one of a set of equivalent variables is in a common
 * block, the rest are not in another block. */     
     
static try resolve_equivalence(g95_equiv *j) { 
g95_symtree *c, *i;     
g95_symbol *k, *z;       
int seen_common;    
g95_equiv *y;    
    
  seen_common = 0; 
  c = NULL;     
  k = NULL;      
      
  for(y=j; y; y=y->eq) {     
    if (resolve_equivalenced_var(y->expr) == FAILURE) return FAILURE;         
         
    z = y->expr->symbol;       
    if (!z->attr.in_common) continue;  
  
    i = find_common_block(z);    
    
    if (!seen_common) { 
      c = i;  
      k = z;          
      seen_common = 1;        
    } else {   
      if (c != i) {  
	g95_error("Symbols '%s' at %L and '%s' are equivalenced but in "      
		  "different common blocks", k->name, &k->declared_at,
		  z->name);      
	return FAILURE;       
      }   
    }       
  }  
  
  /* Check type restrictions */       
       
  if (numeric_sequence_type(&j->expr->ts)) {  
    for(y=j->eq; y; y=y->eq)   
      if (!numeric_sequence_type(&y->expr->ts) && g95_option.fmode != 0) {          
	g95_warning(122, "EQUIVALENCE variable '%s' at %L must be a numeric " 
		    "sequence type", y->expr->symbol->name, &y->expr->where);     
	break;    
      }  
  } else if (character_sequence_type(&j->expr->ts)) {    
    for(y=j->eq; y; y=y->eq)     
      if (!character_sequence_type(&y->expr->ts) && g95_option.fmode != 0) {
	g95_warning(123, "EQUIVALENCE variable '%s' at %L must be a character "      
		    "sequence type", y->expr->symbol->name, &y->expr->where); 
	break;       
      }   
  } else if (is_default_kind(&j->expr->ts)) {          
    for(y=j->eq; y; y=y->eq)          
      if (!is_default_kind(&y->expr->ts) && g95_option.fmode != 0) {      
	g95_warning(124, "EQUIVALENCE variable '%s' at %L must be of default "   
		    "kind", y->expr->symbol->name, &y->expr->where);   
	break;          
      }     
  } else {       
    for(y=j->eq; y; y=y->eq)    
      if (!g95_compare_types(&j->expr->ts, &y->expr->ts) &&  
	  g95_option.fmode != 0) {
	g95_warning(125, "EQUIVALENCE variable '%s' at %L must be %s",   
		    y->expr->symbol->name, &y->expr->where,      
		    g95_typename(&y->expr->ts)); 
	break;   
      }         
  }   
   
  return SUCCESS; 
}      
      
      
        
        
/* resolve_entry()-- Resolve ENTRY points */        
        
static try resolve_entry(g95_namespace *namesp) {    
g95_symbol *s;          
          
  g95_find_entries(namesp->sym_root, 1);         
         
  if (entries == NULL || !namesp->proc_name->attr.function) return SUCCESS;         
         
  if (entry_return_type(namesp->proc_name, "Function '%s' containing an ENTRY"))   
    return FAILURE;

  for(s=entries; s; s=s->tlink)          
    if (entry_return_type(s, "ENTRY '%s'")) return FAILURE;

  return SUCCESS;  
}          
          
          
   
   
/* g95_resolve()-- This function is called after a complete program
 * unit has been compiled.  Its purpose is to examine all of the
 * expressions associated with a program unit, assign types to all
 * intermediate expressions, make sure that all assignments are to
 * compatible types and figure out which names refer to which
 * functions or subroutines. */ 
 
try g95_resolve(g95_namespace *namesp) {
g95_namespace *old_ns, *z;
g95_symbol *s;      
g95_equiv *eq0;         
g95_data *j;
try o;        
        
  old_ns = g95_current_ns;       
  g95_current_ns = namesp;   
   
  resolve_contained_functions(namesp);          
          
  o = FAILURE;       
       
  for(z=namesp->contained; z; z=z->sibling) {      
    if (g95_pure(namesp->proc_name) && !g95_pure(z->proc_name))         
      g95_error("Contained procedure '%s' at %L of a PURE procedure must "       
		"also be PURE", z->proc_name->name,        
		&z->proc_name->declared_at);          
          
    if (g95_resolve(z) == FAILURE) goto done;   
  }   
   
  forall_flag = 0;        
  g95_check_interfaces(namesp);        
        
  if (resolve_charlen(namesp) == FAILURE) goto done;       
       
  if (resolve_initial_values(namesp->sym_root) == FAILURE) goto done;    
    
  if (namesp->save_all) g95_save_all(namesp);      
      
  for(j=namesp->data; j; j=j->next)          
    if (resolve_data(j) == FAILURE) goto done;       
       
  resolve_common_block(namesp->common_root);   
  g95_traverse_ns(namesp, resolve_common_var);         
         
  for(eq0=namesp->equiv; eq0; eq0=eq0->next)  
    if (resolve_equivalence(eq0) == FAILURE) goto done;

  cs_base = NULL;  
  resolve_code(namesp->code, namesp);       
       
  if (resolve_symbols(namesp->sym_root) == FAILURE) goto done;     
     
  if (g95_option.unused_label) warn_unused_label(namesp);

  if (g95_option.pedantic != 0) g95_traverse_ns(namesp, check_variable_usage);     
     
  s = namesp->proc_name;         
  if (namespace_kind(namesp) == COMP_FUNCTION &&         
      s->result->ts.type == BT_UNKNOWN && !s->result->attr.untyped) {      
    g95_error("Function '%s' at %L has no IMPLICIT type",   
	      s->name, &s->declared_at);          
    goto done;
  } 
 
  if (resolve_entry(namesp) == FAILURE) goto done;     
     
  o = SUCCESS;        
        
done: 
  g95_current_ns = old_ns;       
  return o;
}
    
    
/* process_entry()-- Process the ENTRY statements within a namespace */    
    
static void process_entry(g95_namespace *namesp) {          
g95_namespace *master, *old, tmp;       
char nam[G95_MAX_SYMBOL_LEN+1];
g95_formal_arglist *m, *u;          
g95_symbol *sy, *a;         
static int serial=0;  
int v;     
     
  g95_find_entries(namesp->sym_root, 1);         
  if (entries == NULL) return;          
          
  master = g95_get_namespace(namesp->parent, 0);          
          
  /* There are two reasons for switching the contents of the master
   * and ns nodes-- the symbols that live in the master namespace must
   * point to the master node, and we also want the master node to be
   * compiled first. */        
        
  tmp = *master; 
  *master = *namesp;        
  *namesp = tmp; 
 
  old = master;   
  master = namesp;    
  namesp = old;  
  
  master->sibling = namesp;      
      
  master->sym_root = namesp->sym_root;       
  namesp->sym_root = NULL;  
  
  master->code = namesp->code;      
  namesp->code = NULL;  
  
  m = g95_copy_formal_arglist(namesp->proc_name->formal);       
       
  for(sy=entries; sy!=NULL; sy=sy->tlink)         
    for(u=sy->formal; u; u=u->next)        
      augment_arglist(&m, u->sym);

  /* Create the master function. */

  a = namesp->proc_name;    
    
  sprintf(nam, "__g95_master_%d", serial++);  
  g95_get_symbol(nam, master, &sy);     
  sy->formal = m;       
  master->proc_name = sy;          
  master->state = COMP_SUBROUTINE;  
  
  sy->attr = a->attr;        
  sy->attr.function = 0;        
  sy->attr.subroutine = 1;      
  sy->attr.artificial = 1;          
          
  g95_commit_symbols(); 
 
  add_entry(master, namesp, 0);      
      
  /* Add public procedures for the entry points. */ 
 
  v = 1;         
  for(sy=entries; sy; sy=sy->tlink, v++) {        
    old = g95_get_namespace(namesp->parent, 0);
    old->proc_name = sy;         
    old->state = sy->attr.function ? COMP_FUNCTION : COMP_SUBROUTINE;       
       
    add_entry(master, old, v);      
      
    old->sibling = master->sibling;        
    master->sibling = old;
  } 
 
  entry_branch(master);        
}  
  
  
         
         
/* g95_process_entry()-- Resolve entry statements in multiple or single
 * namespaces. */

void g95_process_entry(g95_namespace *namesp) { 
 
  if (namesp->proc_name == NULL) return;  
  
  if (namesp->state != COMP_MODULE)       
    process_entry(namesp);  
  else  
    for(namesp=namesp->contained; namesp; namesp=namesp->sibling)         
      if (namesp->proc_name != NULL && !namesp->proc_name->attr.entry)        
	process_entry(namesp);     
}     
     
     
