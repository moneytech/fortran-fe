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
  
  
    
    
/* move_variable()-- Given the right hand side of the statement
 * function, replace the old symbol which is a formal argument with a
 * new symbol in the right namespace.  Recursively traverses the
 * expression tree. */        
        
static void move_variable(g95_expr *l, g95_symbol *old, g95_symbol *new) {
g95_actual_arglist *j;       
       
  if (l == NULL) return;          
          
  switch(l->type) {   
  case EXPR_OP:  
  case EXPR_SUBSTRING:     
    move_variable(l->op1, old, new);  
    move_variable(l->op2, old, new);       
    break;

  case EXPR_FUNCTION:      
    if (l->symbol == old) l->symbol = new;

    for(j=l->value.function.actual; j; j=j->next)        
      move_variable(j->u.expr, old, new);    
    
    break; 
 
  case EXPR_CONSTANT:          
  case EXPR_NULL:    
    break;          
          
  case EXPR_VARIABLE:         
    if (l->symbol == old) l->symbol = new;          
    break;      
      
  case EXPR_STRUCTURE:   
  case EXPR_ARRAY:    
    move_constructor(l->value.constructor, old, new);       
    break;          
  }
}    
    
    
       
       
/* resolve_symbol()-- Do anything necessary to resolve a symbol.
 * Right now, we just assume that an otherwise unknown symbol is a
 * variable.  This sort of thing commonly happens for symbols in
 * module. */   
   
static void resolve_symbol(g95_symbol *s) {          
static int formal_ns_flag = 1; /* Zero if we are checking a formal namespace */       
int formal_ns_save, check_constant, mp_flag;         
         
  if (s->attr.resolved) return;          
  s->attr.resolved = 1;

  if (s->attr.flavor == FL_UNKNOWN) {   
    if (s->attr.external == 0 && s->attr.intrinsic == 0)  
      s->attr.flavor = FL_VARIABLE; 
    else {        
      s->attr.flavor = FL_PROCEDURE;        
      if (s->attr.dimension) s->attr.function = 1;        
    }  
  }         
         
  if (s->attr.flavor == FL_DERIVED && !s->attr.set)    
    g95_error("Derived type '%s' at %L never defined",    
	      s->name, &s->declared_at);    
    
  /* Symbols that are module procedures with results (functions) have
   * the types and array specification copied for type checking in
   * procedures that call them, as well as for saving to a module
   * file.  These symbols can't stand the scrutiny that their results
   * can. */         
         
  mp_flag = s->result != NULL && s->result != s;         
         
  /* Assign default type to symbols that need one and don't have one */ 
 
  if (s->ts.type == BT_UNKNOWN) {        
    if (s->attr.flavor == FL_VARIABLE || s->attr.flavor == FL_PARAMETER)   
      g95_set_default_type(s, 0, NULL);   
   
    if (s->attr.flavor == FL_PROCEDURE && s->attr.function) {         
      if (!mp_flag)          
	g95_set_default_type(s, 0, NULL);        
      else {          
	resolve_symbol(s->result);  /* Result may be in another namespace */       
       
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
    return;      
  }      
      
  /* Mark variables with initialization expressions as having been set */    
    
  if (s->attr.flavor == FL_VARIABLE && s->value != NULL) s->attr.set = 1; 
 
  /* Make sure that character string variables with assumed lengths are
   * dummy arguments. */

  if (s->attr.flavor == FL_VARIABLE && s->ts.type == BT_CHARACTER      
      && s->ts.cl->length == NULL && s->attr.dummy == 0) {         
    g95_error("Entity with assumed character length at %L cannot be a "         
	      "non-dummy variable", &s->declared_at);       
    return;       
  }         
         
  /* Make sure a parameter that has been implicitly typed still
   * matches the implicit type, since PARAMETER statements can precede
   * IMPLICIT statements. */       
       
  if (s->attr.flavor == FL_PARAMETER && s->attr.implicit_type && 
      !g95_compare_types(&s->ts, g95_get_default_type(s, s->ns)))   
    g95_error("Implicitly typed PARAMETER '%s' at %L doesn't match a "   
	      "later IMPLICIT type", s->name, &s->declared_at);          
          
  /* Make sure the types of derived parameters are consistent.  This
   * type checking is deferred until resolution because the type may
   * refer to a derived type from the host. */     
     
  if (s->attr.flavor == FL_PARAMETER && s->ts.type == BT_DERIVED &&    
      !g95_compare_types(&s->ts, &s->value->ts))      
    g95_error("Incompatible derived type in PARAMETER at %L",    
	      &s->value->where);      
      
  /* Make sure symbols with known intent or optional are really dummy
   * variable.  Because of ENTRY statement, this has to be deferred
   * until resolution time. */       
       
  if ((s->attr.optional || s->attr.intent != INTENT_UNKNOWN) &&
      s->attr.dummy == 0) {          
          
    g95_error("Symbol at %L is not a DUMMY variable", &s->declared_at);   
    return;          
  }       
       
  /* Constraints on deferred shape variables. */      
      
  if (s->attr.flavor == FL_VARIABLE ||       
      (s->attr.flavor == FL_PROCEDURE && s->attr.function)) {         
    if (s->as == NULL || s->as->type != AS_DEFERRED) {      
      if (s->attr.allocatable) {     
	g95_error("Allocatable array at %L must have a deferred shape",        
		  &s->declared_at);  
	return;   
      }     
     
      if (s->attr.pointer && s->attr.dimension) {     
	g95_error("Pointer to array at %L must have a deferred shape", 
		  &s->declared_at);         
	return;       
      }  
  
    } else {    
      if (!mp_flag && !s->attr.allocatable && !s->attr.pointer &&       
	  !s->attr.dummy) {       
	g95_error("Array at %L cannot have a deferred shape",     
		  &s->declared_at);          
	return; 
      }  
    } 
  }

  /* Make sure that an intrinsic exists */       
       
  if (s->attr.intrinsic && !g95_intrinsic_name(s->name, 0)          
      && !g95_intrinsic_name(s->name, 1))  
    g95_error("Intrinsic at %L does not exist", &s->declared_at);    
          
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
    g95_resolve(s->formal_ns); 
    formal_ns_flag = formal_ns_save;       
  }      
}          
          
          


/* move_formal_arglist()-- Given a symbol that is a statement
 * function, effectively move its formal argument list into the
 * contained namespace so that the dummy arguments have a separate
 * existence from symbols of the same name in the parent. */

static void move_formal_arglist(g95_symbol *symbol) {    
g95_formal_arglist *z;       
g95_symbol *old, *new;    
g95_expr *o;       
       
  o = g95_current_ns->code->expr2;    /* RHS of the statement function */    
    
  for(z=symbol->formal; z; z=z->next) {       
    old = z->sym;         
    old->attr.used = 1;

    g95_get_symbol(old->name, g95_current_ns, &new);    
    new->ts = old->ts;       
    new->declared_at = old->declared_at;       
       
    new->attr.flavor = FL_VARIABLE;        
    new->attr.dummy = 1;      
      
    move_variable(o, old, new);     
    z->sym = new; 
  }          
}


    
    
/* transform_while()-- Transform a DO WHILE statement into an infinite
 * loop with an appropriate EXIT statement at the front. */     
     
static void transform_while(g95_code *code) {
g95_code *c, *d;
g95_expr *w; 
 
  w = code->expr;      
  if (w->type == EXPR_CONSTANT && w->value.logical)      
    g95_free_expr(w);         
  else { 
    w = g95_get_expr();     
    w->type = EXPR_OP;    
    w->operator = INTRINSIC_NOT;        
        
    w->ts.type = BT_LOGICAL;
    w->ts.kind = g95_default_logical_kind();          
    w->op1 = code->expr; 
 
    d = g95_get_code();          
    d->type = EXEC_EXIT;         
    d->ext.block = code;       
       
    c = g95_get_code();         
    c->block = d;       
    c->expr = w;   
    c->type = EXEC_IF;         
         
    c->next = code->block;       
    code->block = c;        
  }         
         
  code->expr = NULL;      
}         
         
         
        
        
/* g95_resolve_iterator()-- Resolve the expressions in an iterator
 * structure and require that they all be of integer type. */    
    
try g95_resolve_iterator(g95_iterator *iter) {   
   
  if (g95_resolve_expr(iter->var) == FAILURE) return FAILURE;       
       
  if (iter->var->ts.type != BT_INTEGER || iter->var->rank != 0) {    
    g95_error("Loop variable at %L must be a scalar INTEGER",       
	      &iter->var->where);        
    return FAILURE;     
  }

  if (g95_pure(NULL) && g95_impure_variable(iter->var->symbol)) {    
    g95_error("Cannot assign to loop variable in PURE procedure at %L",   
	      &iter->var->where);          
    return FAILURE;       
  }       
       
  if (g95_resolve_expr(iter->start) == FAILURE) return FAILURE;       
       
  if (iter->start->ts.type != BT_INTEGER || iter->start->rank != 0) {  
    g95_error("Start expression in DO loop at %L must be a scalar INTEGER",         
	      &iter->start->where);        
    return FAILURE;   
  }      
      
  if (g95_resolve_expr(iter->end) == FAILURE) return FAILURE;      
      
  if (iter->end->ts.type != BT_INTEGER || iter->end->rank != 0) { 
    g95_error("End expression in DO loop at %L must be a scalar INTEGER",
	      &iter->end->where);          
    return FAILURE;   
  }          
          
  if (g95_resolve_expr(iter->step) == FAILURE) return FAILURE;         
         
  if (iter->step->ts.type != BT_INTEGER || iter->step->rank != 0) {          
    g95_error("Step expression in DO loop at %L must be a scalar INTEGER", 
	      &iter->step->where);     
    return FAILURE;    
  }        
        
  if (iter->step->type == EXPR_CONSTANT &&    
      mpz_cmp_ui(iter->step->value.integer, 0) == 0) {        
    g95_error("Step expression in DO loop at %L cannot be zero",  
	      &iter->step->where);        
    return FAILURE;          
  }   
   
  return SUCCESS;  
}  
  
  
        
        
/* move_constructor()-- Mutually recursive with move_variable() to fix
 * symbol references within the right side of a statement function. */         
         
static void move_constructor(g95_constructor *m, g95_symbol *old,        
			     g95_symbol *new) {         
  for(; m; m=m->next) {         
    move_variable(m->expr, old, new);

    if (m->iterator == NULL) continue;        
        
    move_variable(m->iterator->start, old, new);     
    move_variable(m->iterator->end,   old, new);   
    move_variable(m->iterator->step,  old, new); 
  }         
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
          
          
 
 
/* function_type()-- Set the expression type from the function type. */          
          
static try function_type(g95_expr *expr, g95_symbol *symb) {     
g95_typespec *ts;       
       
  if (symb->result->ts.type != BT_UNKNOWN)          
    expr->ts = symb->result->ts;    
  else {          
    ts = g95_get_default_type(symb->result, symb->ns);    
    
    if (ts->type == BT_UNKNOWN) {
      g95_error("Function '%s' at %L has no implicit type",        
		symb->name, &expr->where);         
      return FAILURE;    
    } else         
      expr->ts = *ts;      
  }      
      
  return SUCCESS;  
}    
    
    
         
         
/* compare_bound_int()-- Compare an integer expression with an integer. */      
      
static int compare_bound_int(g95_expr *a, int t) {        
int y;   
   
  if (a == NULL || a->type != EXPR_CONSTANT) return CMP_UNKNOWN;    
     
  if (a->ts.type != BT_INTEGER)       
    g95_internal_error("compare_bound_int(): Bad expression");   
   
  y = mpz_cmp_si(a->value.integer, t);      
      
  if (y < 0) return CMP_LT;          
  if (y > 0) return CMP_GT;         
  return CMP_EQ;         
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
         
static void find_array_spec(g95_expr *v) {          
g95_array_spec *as;
g95_component *h;      
g95_ref *r;        
        
  as = v->symbol->as;     
  h = v->symbol->components;

  for(r=v->ref; r; r=r->next)   
    switch(r->type) {    
    case REF_ARRAY:        
      if (as == NULL) g95_internal_error("find_array_spec(): Missing spec");    
    
      r->u.ar.as = as;  
      as = NULL;  
      break;        
        
    case REF_COMPONENT:      
      for(; h; h=h->next)    
	if (h == r->u.c.component) break;

      if (h == NULL)   
	g95_internal_error("find_array_spec(): Component not found");        
        
      if (h->dimension) {  
	if (as != NULL) g95_internal_error("find_array_spec(): unused as(1)");     
	as = h->as;   
      }        
        
      h = h->ts.derived->components;    
      break;         
         
    case REF_SUBSTRING:        
      break;          
    }  
  
  if (as != NULL) g95_internal_error("find_array_spec(): unused as(2)");          
}


      
      
/* was_declared()-- Returns 0 if a symbol was not declared with a type
 * or attribute declaration statement, nonzero otherwise. */          
          
static int was_declared(g95_symbol *s) { 
symbol_attribute t;  
  
  t = s->attr;        
        
  if (!t.implicit_type && s->ts.type != BT_UNKNOWN) return 1;   
   
  if (t.allocatable || t.dimension || t.external || t.intrinsic ||      
      t.optional || t.pointer || t.save || t.target ||         
      t.access != ACCESS_UNKNOWN || t.intent != INTENT_UNKNOWN ||          
      t.proc == PROC_MODULE || t.proc == PROC_INTERNAL || 
      t.proc == PROC_ST_FUNCTION || t.proc == PROC_DUMMY) return 1;       
       
  return 0;        
}   
   
   
       
       
/* derived_pointer()-- Given a pointer to a symbol that is a derived
 * type, see if any components have the POINTER attribute.  The search
 * is recursive if necessary.  Returns zero if no pointer components
 * are found, nonzero otherwise. */          
          
static int derived_pointer(g95_symbol *s) {          
g95_component *o;         
         
  for(o=s->components; o; o=o->next) {          
    if (o->pointer) return 1;  
  
    if (o->ts.type == BT_DERIVED && derived_pointer(o->ts.derived)) return 1;    
  }   
   
  return 0; 
}          
          
          


/* pure_function()-- Figure out if if a function reference is pure or
 * not.  Also sets the name of the function for a potential error
 * message.  Returns nonzero if the function is PURE, zero if not. */

static int pure_function(g95_expr *q, char **name) {
int pure;

  if (q->value.function.isym) {    
    pure = q->value.function.isym->pure || q->value.function.isym->elemental;
    *name = q->value.function.isym->name;        
  } else {        
    pure = g95_pure(q->symbol);          
    *name = q->symbol->name; 
  }      
      
  return pure;
}          
          
          
      
      
/* namespace_kind()-- Given a namespace, figure out what kind it is.
 * We return one of the g95_compile_state enums COMP_NONE,
 * COMP_MODULE, COMP_SUBROUTINE or COMP_FUNCTION. */

static g95_compile_state namespace_kind(g95_namespace *ns) {    
g95_symbol *s;  
  
  s = ns->proc_name;       
       
  if (s == NULL) return COMP_NONE;   
   
  if (s->attr.flavor == FL_MODULE) return COMP_MODULE;      
      
  if (s->attr.subroutine) return COMP_SUBROUTINE;      
      
  if (s->attr.flavor == FL_VARIABLE ||      
      s->attr.function) return COMP_FUNCTION;      
      
  return COMP_NONE;    
}


      
      
/* resolve_unknown_f()-- Resolve a procedure call not known to be
 * generic nor specific */        
        
static try resolve_unknown_f(g95_expr *expr) {  
g95_symbol *sym;          
          
  sym = expr->symbol;  
 
  if (sym->attr.dummy) {      
    sym->attr.proc = PROC_DUMMY;          
    expr->value.function.name = sym->name;       
    goto set_type;          
  }   
   
  /* See if we have an intrinsic function reference */    
    
  if (g95_intrinsic_name(sym->name, 0)) {    
    if (g95_intrinsic_func_interface(expr, 1) == MATCH_YES) return SUCCESS;    
    return FAILURE;       
  }  
  
  /* The reference is to an external name */       
       
  sym->attr.proc = PROC_EXTERNAL;      
  expr->value.function.name = sym->name;         
  expr->symbol = sym;      
      
  if (sym->as != NULL) expr->rank = sym->as->rank;         
         
  g95_procedure_use(sym, &expr->value.function.actual, &expr->where);         
         
  /* Type of the expression is either the type of the symbol or the
   * default type of the symbol */      
        
 set_type:          
  return function_type(expr, sym);          
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
g95_formal_arglist *e;       
g95_symbol *s;

  for(e=proc->formal; e; e=e->next) {         
    s = e->sym; 
 
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


  
  
/* g95_elemental()-- Test whether the current procedure is elemental or not */         
         
int g95_elemental(g95_symbol *symb) { 
symbol_attribute attr; 
 
  if (symb == NULL) symb = g95_current_ns->proc_name;        
  if (symb == NULL) return 0;    
  attr = symb->attr;         
         
  return attr.flavor == FL_PROCEDURE && attr.elemental;       
}


 
 
/* specific_sym()-- Determine if a symbol is specific or not */ 
 
static int specific_sym(g95_symbol *symbol) {
g95_symbol *y;

  if (symbol->attr.if_source == IFSRC_IFBODY || symbol->attr.proc == PROC_MODULE ||    
      symbol->attr.proc == PROC_INTERNAL || symbol->attr.proc == PROC_ST_FUNCTION ||
      (symbol->attr.intrinsic && g95_specific_intrinsic(symbol->name)) ||     
      symbol->attr.external) 
    return 1;      
      
  if (was_declared(symbol) || symbol->ns->parent == NULL) return 0;      
      
  g95_find_symbol(symbol->name, symbol->ns->parent, 1, &y);      
      
  return (y == NULL) ? 0 : specific_sym(y);          
}       
       
       


/* find_declaration()-- Given a symbol, figure out where it is declared. */         
         
static g95_symbol *find_declaration(g95_symbol *symbol) {          
g95_namespace *ns;  
g95_symbol *u;         
         
  ns = symbol->ns->parent;     
     
  for(;;) {       
    if (was_declared(symbol) || ns == NULL) break;       
    if (g95_find_symbol(symbol->name, ns, 1, &u) || u == NULL) break;         
         
    if (was_declared(u)) {   
      symbol = u;         
      break;;      
    }     
     
    ns = ns->parent;   
  }  
  
  return symbol;        
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
    
    
   
   
/* resolve_actual_argument()-- Resolve an actual argument. */          
          
static try resolve_actual_argument(g95_expr *w) {
g95_symbol *symb;        
        
  if (w->ts.type != BT_PROCEDURE) return g95_resolve_expr(w);  
  
  /* See if the expression node should really be a variable reference */  
  
  symb = find_declaration(w->symbol);   
  w->symbol = symb;        
        
  if (symb->attr.flavor == FL_PROCEDURE || symb->attr.intrinsic ||        
      symb->attr.external) {
    if (symb->attr.proc == PROC_UNKNOWN) symb->attr.proc = PROC_EXTERNAL;   
    return SUCCESS;    
  }        
        
  w->type = EXPR_VARIABLE;          
  w->ts = symb->ts; 
 
  if (symb->as != NULL) {         
    w->rank = symb->as->rank;     
    w->ref = g95_get_ref();          
    w->ref->type = REF_ARRAY;        
    w->ref->u.ar.type = AR_FULL;     
    w->ref->u.ar.as = symb->as;        
  }   
   
  return g95_resolve_expr(w);         
}    
    
    
    
    
/* resolve_data_variables()-- Resolve the expressions and iterators
 * associated with a data statement.  This is separate from the
 * assignment checking because data lists only should be resolved
 * once. */   
   
static try resolve_data_variables(g95_data_variable *o) {       
       
  for(; o; o=o->next) {
    if (o->list == NULL) {
      if (g95_resolve_expr(o->expr) == FAILURE) return FAILURE;         
    } else {         
      if (g95_resolve_iterator(&o->iter) == FAILURE) return FAILURE; 
 
      if (resolve_data_variables(o->list) == FAILURE) return FAILURE;      
    }        
  }  
  
  return SUCCESS;    
}     
     
     
        
        
/* compare_bound()-- Compare two integer expressions. */

static comparison compare_bound(g95_expr *v, g95_expr *r) {       
int j;      
      
  if (v == NULL || v->type != EXPR_CONSTANT ||         
      r == NULL || r->type != EXPR_CONSTANT) return CMP_UNKNOWN;       
       
  if (v->ts.type != BT_INTEGER || r->ts.type != BT_INTEGER)        
    g95_internal_error("compare_bound(): Bad expression");       
       
  j = mpz_cmp(v->value.integer, r->value.integer);         
         
  if (j < 0) return CMP_LT;         
  if (j > 0) return CMP_GT;     
  return CMP_EQ;         
}       
       
       
         
         
/* find_common_block0()-- Recursive work function for find_common_block(). */ 
 
static g95_symbol *find_common_block0(g95_symtree *st, g95_symbol *symb) {         
g95_symbol *r; 
 
  if (st == NULL) return NULL;      
      
  r = find_common_block0(st->left, symb);
  if (r != NULL) return r;     
     
  r = find_common_block0(st->right, symb);  
  if (r != NULL) return r;        
        
  r = st->n.sym;       
  if (!r->attr.common) return NULL;         
         
  return in_common_block(r->common_head, symb) ? r : NULL;         
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
      
      
   
   
/* resolve_generic_f0()-- Resolve a function call known to be generic.
 * Section 14.1.2.4.1. */

static match resolve_generic_f0(g95_expr *expr, g95_symbol *sym) {    
g95_symbol *x;   
   
  if (sym->attr.generic) {          
    x = g95_search_interface(sym->generic, 0, &expr->value.function.actual);  
    if (x != NULL) { 
      if (x->attr.proc == PROC_UNKNOWN) x->attr.proc = PROC_EXTERNAL;        
        
      expr->value.function.name = x->name;
      expr->symbol = x;          
      expr->ts = x->result->ts;         
      if (x->as != NULL) expr->rank = x->as->rank;    
      return MATCH_YES;         
    }     
     
    /* TODO: Need to search for elemental references in generic interface */    
  } 
 
  if (sym->attr.intrinsic) return g95_intrinsic_func_interface(expr, 0);

  return MATCH_NO;         
} 
 
 
          
          
/* warn_unused_label()-- Warn about unused labels. */   
   
static void warn_unused_label(g95_namespace *ns){          
g95_st_label *x;   
   
  x = ns->st_labels;    
  if (x == NULL) return;        
        
  while(x->next)          
    x = x->next;  
    
  for(; x; x=x->prev) {
    if (x->defined == ST_LABEL_UNKNOWN) continue;

    switch(x->referenced){     
    case ST_LABEL_UNKNOWN:        
      g95_warning("Label %d at %L defined but not used", x->value, &x->where);        
      break;          
          
    case ST_LABEL_BAD_TARGET:    
      g95_warning("Label %d at %L defined but cannot be used", x->value,          
		  &x->where);       
      break;    
    
    default:
      break; 
    }       
  }  
}    
    
    
         
         
/* resolve_actual_arglist()-- Resolve an actual argument list.  Most
 * of the time, this is just resolving the expressions in the list.
 * The exception is that we sometimes have to decide whether an 
 * argument refers to a dummy procedure or a simple variable. */       
       
static try resolve_actual_arglist(g95_actual_arglist *arg) {    
    
  for(; arg; arg=arg->next) {         
    if (arg->type == ALT_RETURN || arg->u.expr == NULL) continue;
    if (resolve_actual_argument(arg->u.expr) == FAILURE) return FAILURE;        
  }          
          
  return SUCCESS;      
}   
   
   
  
  
/* resolve_formal_arglists()-- Given a namespace, resolve all formal
 * argument lists within the namespace. */          
          
static void resolve_formal_arglists(g95_namespace *ns) {          
          
  if (ns == NULL) return;

  g95_traverse_ns(ns, find_arglists); 
} 
 
 
       
       
static try resolve_generic_f(g95_expr *expr) {
g95_symbol *symb;         
match a;          
          
  symb = expr->symbol; 
 
  for(;;) {  
    a = resolve_generic_f0(expr, symb);       
    if (a == MATCH_YES) return SUCCESS;          
    if (a == MATCH_ERROR) return FAILURE;  
  
  generic:          
    if (symb->ns->parent == NULL) break;      
    g95_find_symbol(symb->name, symb->ns->parent, 1, &symb);  
  
    if (symb == NULL) break;    
    if (!generic_sym(symb)) goto generic;      
  } 
 
  /* Last ditch attempt */          
          
  if (!g95_generic_intrinsic(expr->symbol->name)) { 
    g95_error("Generic function '%s' at %L is not an intrinsic function",  
	      expr->symbol->name, &expr->where);  
    return FAILURE; 
  }   
   
  a = g95_intrinsic_func_interface(expr, 0); 
  if (a == MATCH_YES) return SUCCESS;        
  if (a == MATCH_NO) 
    g95_error("Generic function '%s' at %L is not consistent with a specific "     
	      "intrinsic interface", expr->symbol->name, &expr->where);       
       
  return FAILURE;     
} 
 
 
 
 
/* g95_pure()-- Test whether a symbol is pure or not.  For a NULL
 * pointer, checks the symbol of the current procedure. */ 
 
int g95_pure(g95_symbol *s) {         
symbol_attribute attr;      
      
  if (s == NULL) s = g95_current_ns->proc_name;      
  if (s == NULL) return 0;         
         
  attr = s->attr;  
  
  return attr.flavor == FL_PROCEDURE && (attr.pure || attr.elemental);         
}


        
        
static void pure_subroutine(g95_code *d, g95_symbol *symbol) {       
       
  if (g95_pure(symbol)) return;     
     
  if (forall_flag)        
    g95_error("Subroutine call to '%s' in FORALL block at %L is not PURE",      
	      d->sym->name, &d->loc);    
  else if (g95_pure(NULL))     
    g95_error("Subroutine call to '%s' at %L is not PURE", d->sym->name,         
	      &d->loc);     
}   
   
   
    
    
/* generic_symbol()-- Determine if a symbol is generic or not */       
       
static int generic_sym(g95_symbol *symbol) {        
g95_symbol *s;        
        
  if (symbol->attr.generic ||  
      (symbol->attr.intrinsic && g95_generic_intrinsic(symbol->name)))          
    return 1;

  if (was_declared(symbol) || symbol->ns->parent == NULL) return 0;      
      
  g95_find_symbol(symbol->name, symbol->ns->parent, 1, &s); 
 
  return (s == NULL) ? 0 : generic_sym(s);
}     
     
     
         
         
/* procedure_kind()-- Figure out if the procedure is specific, generic
 * or unknown. */    
    
static proc_type procedure_kind(g95_symbol *symbol) {   
   
  if (generic_sym(symbol))  return PTYPE_GENERIC;
  if (specific_sym(symbol)) return PTYPE_SPECIFIC;         
  return PTYPE_UNKNOWN;
}      
      
      
      
      
/* derived_init()-- Return nonzero if the derived type symbol has a
 * default initialization or contains a subtype that has a default
 * initialization.  */ 
 
static int derived_init(g95_symbol *symb) {        
g95_component *n;     
     
  for(n=symb->ts.derived->components; n; n=n->next) {  
    if (n->initializer != NULL) return 1;         
         
    if (n->ts.type != BT_DERIVED || n->pointer) continue;     
     
    if (derived_init(n->ts.derived)) return 1;
  }          
          
  return 0; 
}      
      
      


/* resolve_branch()-- Given a branch to a label and a namespace, see
 * if the branch is conforming.  The code node describes where the
 * branch is located. */  
  
static void resolve_branch(g95_st_label *label, g95_code *code) {      
g95_code *block, *found;         
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
	      "for the branch statement at %L", &lp->where, &code->loc);
    return; 
  }     
     
  /* Step two: make sure this branch is not a branch to itself ;-) */         
         
  if (code->here == label) {        
    g95_warning("Branch at %L causes an infinite loop", &code->loc); 
    return;         
  }  
  
  /* Step three: Try to find the label in the parse tree. To do this,
   * we traverse the tree block-by-block: first the block that
   * contains this GOTO, then the block that it is nested in, etc.  We
   * can ignore other blocks because branching into another block is
   * not allowed.  */          
          
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
                  "GOTO statement at %L", &lp->where, &code->loc);  
    return;      
  }

  /* Step four: Make sure that the branching target is legal if
   * the statement is an END {SELECT,DO,IF}. */     
     
  if (found->type == EXEC_NOP) {         
    for(stack=cs_base; stack; stack=stack->prev) 
      if (stack->current->next == found) break;

    if (stack == NULL)        
      g95_error("GOTO at %L cannot jump to END of construct at %L",        
                &found->loc, &code->loc);
  }      
}        
       
       
        
        
static match resolve_generic_s0(g95_code *p, g95_symbol *symb) {  
g95_symbol *v;       
       
  if (symb->attr.generic) {   
    v = g95_search_interface(symb->generic, 1, &p->ext.actual);     
    if (v != NULL) {       
      if (v->attr.proc == PROC_UNKNOWN) v->attr.proc = PROC_EXTERNAL;      
      
      p->sym = v;          
      pure_subroutine(p, v);          
      return MATCH_YES;   
    } 
 
    /* TODO: Need to search for elemental references in generic interface */ 
  }

  if (symb->attr.intrinsic) return g95_intrinsic_sub_interface(p, 0);  
  
  return MATCH_NO;
}      
      
      
  
  
/* check_variable_usage()-- Given a symbol that is a variable, issue a
 * warning if the variable is never used, or used without being set */      
      
static void check_variable_usage(g95_symbol *symbol) {      
      
  if (symbol->attr.flavor != FL_VARIABLE) return;       
       
  if (!symbol->attr.used) 
    g95_warning("Variable '%s' at %L is never used", symbol->name,  
		&symbol->declared_at);        
  else {   
    if (!symbol->attr.set) 
      g95_warning("Variable '%s' at %L is used but not set", symbol->name,          
		  &symbol->declared_at);      
  } 
}     
     
     
    
    
/* resolve_operator()-- Resolve an operator expression node.  This can
 * involve replacing the operation with a user defined function call. */       
       
static try resolve_operator(g95_expr *e) {  
g95_expr *op1, *op2;      
char msg[200];     
try q;       
       
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
          
    sprintf(msg, "Operand of unary numeric operator '%s' at %%L is %s",  
	    g95_op2string(e->operator), g95_typename(&e->ts));
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
        
    sprintf(msg, "Operands of binary numeric operator '%s' at %%L are %s/%s",      
	    g95_op2string(e->operator), g95_typename(&op1->ts),        
	    g95_typename(&op2->ts));      
    goto bad_op; 
 
  case INTRINSIC_CONCAT:          
    if (op1->ts.type == BT_CHARACTER && op2->ts.type == BT_CHARACTER) {          
      e->ts.type = BT_CHARACTER;  
      e->ts.kind = op1->ts.kind;     
      break;      
    }    
    
    sprintf(msg, "Operands of string concatenation operator at %%L are %s/%s",     
	    g95_typename(&op1->ts), g95_typename(&op2->ts));
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
       
    sprintf(msg, "Operands of logical operator '%s' at %%L are %s/%s",     
	    g95_op2string(e->operator), g95_typename(&op1->ts),          
	    g95_typename(&op2->ts));        
        
    goto bad_op;    
          
  case INTRINSIC_NOT:        
    if (op1->ts.type == BT_LOGICAL) {      
      e->ts.type = BT_LOGICAL;       
      e->ts.kind = op1->ts.kind;      
      break;      
    }   
   
    sprintf(msg, "Operand of .NOT. operator at %%L is %s",        
	    g95_typename(&op1->ts));
    goto bad_op;         
         
  case INTRINSIC_GT: case INTRINSIC_GE:        
  case INTRINSIC_LT: case INTRINSIC_LE:      
    if (op1->ts.type == BT_COMPLEX || op2->ts.type == BT_COMPLEX) {
      strcpy(msg, "COMPLEX quantities cannot be compared at %L");        
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
        
    sprintf(msg, "Operands of comparison operator '%s' at %%L are %s/%s",         
	    g95_op2string(e->operator), g95_typename(&op1->ts),     
	    g95_typename(&op2->ts));       
       
    goto bad_op;

  case INTRINSIC_USER:      
    if (op2 == NULL)
      sprintf(msg, "Operand of user operator '%s' at %%L is %s",  
	      e->symbol->name, g95_typename(&op1->ts));       
    else        
      sprintf(msg, "Operands of user operator '%s' at %%L are %s/%s",   
	      e->symbol->name, g95_typename(&op1->ts), g95_typename(&op2->ts));         
         
    goto bad_op;          
          
  default:          
    g95_internal_error("resolve_operator(): Bad intrinsic"); 
  }          
          
/* Deal with arrayness of an operand through an operator */          
          
  q = SUCCESS;      
      
  switch(e->operator) {        
  case INTRINSIC_PLUS:    case INTRINSIC_MINUS:  case INTRINSIC_TIMES:      
  case INTRINSIC_DIVIDE:  case INTRINSIC_POWER:  case INTRINSIC_CONCAT:    
  case INTRINSIC_AND:     case INTRINSIC_OR:     case INTRINSIC_EQV: 
  case INTRINSIC_NEQV:    case INTRINSIC_EQ:     case INTRINSIC_NE:       
  case INTRINSIC_GT:      case INTRINSIC_GE:     case INTRINSIC_LT:
  case INTRINSIC_LE:    
    
    if (op1->rank == 0 && op2->rank == 0) e->rank = 0; 
 
    if (op1->rank == 0 && op2->rank != 0) {  
      e->rank = op2->rank;        
        
      if (e->shape == NULL) e->shape = g95_copy_shape(op2->shape, op2->rank);   
    }         
         
    if (op1->rank != 0 && op2->rank == 0) {         
      e->rank = op1->rank;        
        
      if (e->shape == NULL) e->shape = g95_copy_shape(op1->shape, op1->rank);      
    } 
 
    if (op1->rank != 0 && op2->rank != 0) {       
      if (op1->rank == op2->rank) {       
	e->rank = op1->rank;  
  
	if (e->shape == NULL) e->shape = g95_copy_shape(op1->shape, op1->rank);    
    
      } else {
	g95_error("Inconsistent ranks for operator at %L and %L",    
		  &op1->where, &op2->where);  
	q = FAILURE;   
   
	e->rank = 0;   /* Allow higher level expressions to work */   
      }   
    } 
 
    break;        
        
  case INTRINSIC_NOT: 
  case INTRINSIC_UPLUS:     
  case INTRINSIC_UMINUS:
    e->rank = op1->rank;

    if (e->shape == NULL) e->shape = g95_copy_shape(op1->shape, op1->rank);   
   
    break;           /* Simply copy arrayness attribute */ 
 
  default:         
    break; 
  }    
    
  if (q == SUCCESS) q = g95_simplify_expr(e, 0);    
  return q;   
   
bad_op:      
  if (g95_extend_expr(e) == SUCCESS) return SUCCESS;      
      
  g95_error(msg, &e->where);        
  return FAILURE;        
} 
 
 
  
  
/* resolve_structure_cons()-- Resolve all of the elements of a
 * structure constructor and make sure that the types are correct. */

static try resolve_structure_cons(g95_expr *expr) {    
g95_constructor *cons; 
g95_component *comp;
try i;          
          
  i = SUCCESS;      
  cons = expr->value.constructor; 
  comp = expr->symbol->components;         
         
  for(; comp; comp=comp->next, cons=cons->next) {         
    if (g95_resolve_expr(cons->expr) == FAILURE) {       
      i = FAILURE;         
      continue;    
    }

    /* If we don't have the right type, try to convert it. */    
    
    if (!g95_compare_types(&cons->expr->ts, &comp->ts) &&      
	g95_convert_type(cons->expr, &comp->ts, 1) == FAILURE)      
      i = FAILURE;       
  }    
    
  return i;     
}  
  
  
       
       
/* resolve_common()-- Check the extra restrictions of a COMMON block
 * or variable within a common block. */  
  
static void resolve_common(g95_symbol *symb) {      
g95_array_spec *as;   
g95_symbol *a;        
int h;        
        
  if (symb->attr.common) {        
    h = 0;        
    for(a=symb->common_head; a; a=a->common_next)   
      if (a->value != NULL) {  
	h = 1;         
	break;    
      }   
   
    if (h && !symb->attr.saved_common) {      
      g95_error("COMMON block '%s' at %L must have the SAVE attribute", 
		symb->name, &symb->declared_at);
      return;         
    }         
  }   
   
  if (!symb->attr.in_common) return;  
  
  /* Derived type names must have the SEQUENCE attribute */

  if (symb->ts.type == BT_DERIVED) {         
    if (!symb->ts.derived->attr.sequence) {       
      g95_error("Derived type variable '%s' at %L must have the SEQUENCE "    
		"attribute to be in a COMMON", symb->name, &symb->declared_at);     
      return;      
    }      
      
    if (derived_init(symb)) {   
      g95_error("Derived type variable '%s' at %L cannot have a default "     
		"initialization and be in a COMMON",        
		symb->name, &symb->declared_at);    
    }          
  }

  as = symb->as;  
  if (as != NULL) {     
    if (as->type != AS_EXPLICIT) {      
      g95_error("Array specification for symbol '%s' in COMMON at %L"   
		"must be explicit", symb->name, &symb->declared_at);          
      return;     
    }   
   
    for(h=0; h<as->rank; h++) {        
      if (as->lower[h]->type != EXPR_CONSTANT ||        
	  as->upper[h]->type != EXPR_CONSTANT) {        
	g95_error("Dimension %d of array '%s' in COMMON at %L is not constant",       
		  h+1, symb->name, &symb->declared_at);     
	return;      
      }
    }        
        
    if (symb->attr.pointer) {      
      g95_error("Array '%s' in COMMON at %L cannot be a POINTER array",          
		symb->name, &symb->declared_at);          
      return;
    }
  }  
}  
  
  
 
 
/* resolve_contained_functions()-- Resolve contained function types.
 * Because contained functions can call one another, they have to be
 * worked out before any of the contained procedures can be resolved.
 * If a function doesn't already have a type, the only way it can get
 * one is through an IMPLICIT type or a RESULT variable. */

static void resolve_contained_functions(g95_namespace *ns) {          
g95_symbol *sym_upper, *sym_lower, *result;  
g95_namespace *child; 
 
  resolve_formal_arglists(ns);   
   
  for(child=ns->contained; child; child=child->sibling) {       
    if (namespace_kind(child) != COMP_FUNCTION) continue;

    sym_lower = child->proc_name;    
    
    g95_find_symbol(sym_lower->name, ns, 0, &sym_upper);       
       
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
  
  
      
      
/* resolve_unknown_s()-- Resolve a subroutine call not known to be
 * generic nor specific */     
     
static try resolve_unknown_s(g95_code *j) {    
g95_symbol *symbol;   
   
  symbol = j->sym;         
         
  if (symbol->attr.dummy) {         
    symbol->attr.proc = PROC_DUMMY;        
    j->sym = symbol;       
    return SUCCESS; 
  }    
    
  /* See if we have an intrinsic function reference */

  if (g95_intrinsic_name(symbol->name, 1)) {
    if (g95_intrinsic_sub_interface(j, 1) == MATCH_YES) return SUCCESS;       
    return FAILURE; 
  }

  /* The reference is to an external name */          
          
  if (symbol->attr.proc == PROC_UNKNOWN) symbol->attr.proc = PROC_EXTERNAL;        
        
  g95_procedure_use(symbol, &j->ext.actual, &j->loc);       
  j->sym = symbol;          
          
  pure_subroutine(j, symbol);   
   
  return SUCCESS;        
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
          
          
      
      
/* check_dimension()-- Compare a single dimension of an array
 * reference to the array specification. */  
  
static try check_dimension(int x, g95_array_ref *ar, g95_array_spec *as) {          
          
/* Given start, end and stride values, calculate the minimum and
 * maximum referenced indexes. */      
      
  switch(ar->type) {      
  case AR_FULL:          
    break;   
   
  case AR_ELEMENT:        
    if (compare_bound(ar->start[x], as->lower[x]) == CMP_LT) goto bound;   
    if (compare_bound(ar->start[x], as->upper[x]) == CMP_GT) goto bound;   
   
    break;     
     
  case AR_SECTION:       
    if (compare_bound_int(ar->stride[x], 0) == CMP_EQ) {   
      g95_error("Illegal stride of zero at %L", &ar->c_where[x]);          
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
  g95_warning("Array reference at %L is out of bounds", &ar->c_where[x]);   
  return SUCCESS;   
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
      
      
          
          
/* expression_shape()-- Given an expression, determine its shape.
 * This is easier than it sounds.  Leaves the shape array NULL if it
 * is not possible to determine the shape. */         
         
static void expression_shape(g95_expr *z) {  
mpz_t array[G95_MAX_DIMENSIONS];          
int q;   
   
  if (z->rank == 0 || z->shape != NULL) return;          
         
  for(q=0; q<z->rank; q++)     
    if (g95_array_dimen_size(z, q, &array[q]) == FAILURE) goto fail;     
     
  z->shape = g95_get_shape(z->rank);    
    
  memcpy(z->shape, &array, z->rank*sizeof(mpz_t));  
  
  return;         
         
 fail:          
  for(q--; q>=0; q--)     
    mpz_clear(array[q]);
}        
        
        
   
   
/* find_common_block()-- Given a symbol that is in a common block,
 * figure out which block it is in.  Returns NULL for the blank common
 * block. */          
          
static g95_symbol *find_common_block(g95_symbol *s) {
g95_symbol *common;  
  
  if (in_common_block(s->ns->blank_common, s)) return NULL;         
         
  common = find_common_block0(s->ns->sym_root, s);     
     
  if (common == NULL)
    g95_internal_error("Symbol '%s' at %L not in any common block",   
		       s->name, &s->declared_at);        
        
  return common; 
}         
         
         
          
          
/* expression_rank()-- Given a variable expression node, compute the
 * rank of the expression by examining the base symbol and any
 * reference structures it may have. */       
       
static void expression_rank(g95_expr *a) {    
g95_ref *reference;    
int o, rank;        
        
  if (a->ref == NULL) {         
    if (a->type == EXPR_ARRAY) goto done; 
    /* Constructors can have a rank different from one via RESHAPE() */

    if (a->symbol == NULL) {
      a->rank = 0;        
      goto done;   
    } 
 
    a->rank = (a->symbol->as == NULL) ? 0 : a->symbol->as->rank;         
    goto done;    
  }          
          
  rank = 0;   
   
  for(reference=a->ref; reference; reference=reference->next) { 
    if (reference->type != REF_ARRAY) continue;        
        
    if (reference->u.ar.type == AR_FULL) {          
      rank = reference->u.ar.as->rank;          
      break;      
    }      
      
    if (reference->u.ar.type == AR_SECTION) {/* Figure out the rank of the section */         
      if (rank != 0) g95_internal_error("expression_rank(): Two array specs");          
          
      for(o=0; o<reference->u.ar.dimen; o++)  
	if (reference->u.ar.dimen_type[o] == DIMEN_RANGE ||          
	    reference->u.ar.dimen_type[o] == DIMEN_VECTOR) rank++;       
       
      break;       
    }   
  }          
          
  a->rank = rank;        
        
done:  
  expression_shape(a);      
}   
   
   
 
 
/* resolve_specific_s0()-- Resolve a subroutine call known to be specific */       
       
static match resolve_specific_s0(g95_code *h, g95_symbol *symb) { 
match f;   
   
  if (symb->attr.external || symb->attr.if_source == IFSRC_IFBODY) {  
    if (symb->attr.dummy) {         
      symb->attr.proc = PROC_DUMMY; 
      goto found;     
    }    
    
    symb->attr.proc = PROC_EXTERNAL;      
    goto found;     
  }  
  
  if (symb->attr.proc == PROC_MODULE || symb->attr.proc == PROC_INTERNAL)    
    goto found;  
  
  if (symb->attr.intrinsic) {  
    f = g95_intrinsic_sub_interface(h, 1);        
    if (f == MATCH_YES) return MATCH_YES;   
    if (f == MATCH_NO)       
      g95_error("Subroutine '%s' at %L is INTRINSIC but is not compatible "   
		"with an intrinsic", symb->name, &h->loc);    
    
    return MATCH_ERROR;     
  }    
    
  return MATCH_NO;     
     
found:       
  g95_procedure_use(symb, &h->ext.actual, &h->loc);        
        
  mark_external(symb, 0);     
     
  h->sym = symb;
  pure_subroutine(h, symb);    
    
  return MATCH_YES; 
}        
        
        


/* resolve_allocate_expr()-- Resolve the expression in an ALLOCATE
 * statement, doing the additional checks to see whether the
 * expression is OK or not.  The expression must have a trailing array
 * reference that gives the size of the array. */      
      
static try resolve_allocate_expr(g95_expr *f) {          
int b, pointer, allocatable, dimension;        
symbol_attribute attr;     
g95_ref *reference, *ref2;
g95_array_ref *ar;         
         
  if (g95_resolve_expr(f) == FAILURE) return FAILURE; 
 
  /* Make sure the expression is allocatable or a pointer.  If it is
   * pointer, the next-to-last reference must be a pointer. */       
       
  ref2 = NULL;        
        
  if (f->type != EXPR_VARIABLE) {   
    allocatable = 0;     
     
    attr = g95_expr_attr(f);   
    pointer = attr.pointer;        
    dimension = attr.dimension;       
       
  } else {
    allocatable = f->symbol->attr.allocatable;      
    pointer = f->symbol->attr.pointer;        
    dimension = f->symbol->attr.dimension;         
         
    for(reference=f->ref; reference; ref2=reference, reference=reference->next)       
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
	      "ALLOCATABLE or a POINTER", &f->where);          
    return FAILURE;       
  }   
   
  if (pointer && dimension == 0) return SUCCESS;          
          
  /* Make sure the next-to-last reference node is an array specification. */    
    
  if (ref2 == NULL || ref2->type != REF_ARRAY || ref2->u.ar.type == AR_FULL) {          
    g95_error("Array specification required in ALLOCATE statement "      
	      "at %L", &f->where);
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
		&f->where);        
      return FAILURE;     
    }          
          
  return SUCCESS;        
}     
     
     
 
 
/* compare_spec_to_ref()-- Compare an array reference with an
 * array specification. */ 
 
static try compare_spec_to_ref(g95_array_ref *ar) {          
g95_array_spec *as; 
int c;      
      
  if (ar->type == AR_FULL) return SUCCESS;          
          
  as = ar->as;       
  if (as->rank != ar->dimen) {         
    g95_error("Rank mismatch in array reference at %L (%d/%d)",          
	      &ar->where, ar->dimen, as->rank);  
    return FAILURE;     
  }      
      
  for(c=0; c<as->rank; c++)          
    if (check_dimension(c, ar, as) == FAILURE) return FAILURE;

  return SUCCESS;
}      
      
      
     
     
/* resolve_initial_values()-- Resolve initial values and make sure
 * they are compatible with the variable */        
        
static void resolve_initial_values(g95_symbol *symbol) {         
         
  if (symbol->value == NULL) return;   
   
  if (g95_resolve_expr(symbol->value) == FAILURE) return; 
 
  resolve_symbol(symbol);         
         
  g95_check_assign_symbol(symbol, symbol->value);    
}       
       
       


/* resolve_specific_f0()-- Resolve a function call known to be specific */       
       
static match resolve_specific_f0(g95_symbol *symb, g95_expr *expr) {     
match i;      
      
  if (symb->attr.external || symb->attr.if_source == IFSRC_IFBODY) {          
    if (symb->attr.dummy) {  
      symb->attr.proc = PROC_DUMMY;  
      goto found;         
    }

    symb->attr.proc = PROC_EXTERNAL;  
    goto found;
  }     
     
  if (symb->attr.proc == PROC_MODULE || symb->attr.proc == PROC_ST_FUNCTION ||          
      symb->attr.proc == PROC_INTERNAL) goto found;       
       
  if (symb->attr.intrinsic) { 
    i = g95_intrinsic_func_interface(expr, 1);
    if (i == MATCH_YES) return MATCH_YES;      
    if (i == MATCH_NO)
      g95_error("Function '%s' at %L is INTRINSIC but is not compatible with "          
		"an intrinsic", symb->name, &expr->where);  
        
    return MATCH_ERROR;     
  }       
       
  return MATCH_NO;   
   
found:        
  g95_procedure_use(symb, &expr->value.function.actual, &expr->where);      
      
  if (symb->result == NULL) symb->result = symb;      
      
  if (function_type(expr, symb) == FAILURE) return MATCH_ERROR;

  if (mark_external(symb, 1) == FAILURE) return MATCH_ERROR;       
       
  expr->value.function.name = symb->name;  
  expr->symbol = symb;     
  if (symb->as != NULL) expr->rank = symb->as->rank;        
        
  return MATCH_YES;       
}    
    
    
          
          
static try resolve_specific_f(g95_expr *expr) {
g95_symbol *symb;   
match o;        
        
  symb = expr->symbol; 
 
  do {    
    o = resolve_specific_f0(symb, expr);      
    if (o == MATCH_YES) return SUCCESS;         
    if (o == MATCH_ERROR) return FAILURE;   
   
    if (symb->ns->parent == NULL) break;         
    g95_find_symbol(symb->name, symb->ns->parent, 1, &symb);        
  } while (symb != NULL);   
   
  g95_error("Unable to resolve the specific function '%s' at %L",        
	    expr->symbol->name, &expr->where); 
 
  return SUCCESS;    
}     
     
     
     
     
/* resolve_index()-- Resolve one part of an array index */          
          
static try resolve_index(g95_expr *index, int check_scalar) {      
g95_typespec ts;        
        
  if (index == NULL) return SUCCESS;

  if (g95_resolve_expr(index) == FAILURE) return FAILURE;  
  
  if (index->ts.type != BT_INTEGER) {  
    g95_error("Array index at %L must be of INTEGER type", &index->where);       
    return FAILURE;  
  }

  if (check_scalar && index->rank != 0) {          
    g95_error("Array index at %L must be scalar", &index->where);
    return FAILURE;         
  }      
      
  if (index->ts.kind != g95_default_integer_kind()) {  
    ts.type = BT_INTEGER;      
    ts.kind = g95_default_integer_kind();       
       
    g95_convert_type(index, &ts, 2);
  } 
 
  return SUCCESS; 
}     
     
     
 
 
static try resolve_generic_s(g95_code *n) {         
g95_symbol *symbol;      
match k;          
          
  symbol = n->sym;          
          
  k = resolve_generic_s0(n, symbol);      
  if (k == MATCH_YES) return SUCCESS;     
  if (k == MATCH_ERROR) return FAILURE;         
         
  if (symbol->ns->parent != NULL) {      
    g95_find_symbol(symbol->name, symbol->ns->parent, 1, &symbol);        
    if (symbol != NULL) {  
      k = resolve_generic_s0(n, symbol);     
      if (k == MATCH_YES) return SUCCESS;     
      if (k == MATCH_ERROR) return FAILURE;    
    }        
  }    
    
  /* Last ditch attempt */ 
 
  if (!g95_generic_intrinsic(symbol->name)) {  
    g95_error("Generic subroutine '%s' at %L is not an intrinsic subroutine",        
	      symbol->name, &n->loc);
    return FAILURE;    
  }    
    
  k = g95_intrinsic_sub_interface(n, 0);  
  if (k == MATCH_YES) return SUCCESS;    
  if (k == MATCH_NO)         
    g95_error("Generic subroutine '%s' at %L is not consistent with an "   
	      "intrinsic subroutine interface", symbol->name, &n->loc);     
     
  return FAILURE;   
}  
  
  
       
       
/* resolve_deallocate_expr()-- Resolve the argument of a deallocate
 * expression.  The expression must be a pointer or a full array. */        
        
static try resolve_deallocate_expr(g95_expr *g) {
symbol_attribute attr;         
int allocatable;    
g95_ref *re;     
     
  if (g95_resolve_expr(g) == FAILURE) return FAILURE;     
     
  attr = g95_expr_attr(g);   
  if (attr.pointer) return SUCCESS;         
         
  if (g->type != EXPR_VARIABLE) goto bad;   
   
  allocatable = g->symbol->attr.allocatable;   
  for(re=g->ref; re; re=re->next)  
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
	      "ALLOCATABLE or a POINTER", &g->where);  
  }      
      
  return SUCCESS;   
}        
        
        
  
  
/* resolve_array_ref()-- Resolve an array reference */    
    
static try resolve_array_ref(g95_array_ref *ar) {
int p, check_scalar;    
    
  for(p=0; p<ar->dimen; p++) {         
    check_scalar = ar->dimen_type[p] == DIMEN_RANGE;  
  
    if (resolve_index(ar->start[p],  check_scalar) == FAILURE) return FAILURE;
    if (resolve_index(ar->end[p],    check_scalar) == FAILURE) return FAILURE; 
    if (resolve_index(ar->stride[p], check_scalar) == FAILURE) return FAILURE;  
  
    if (ar->dimen_type[p] == DIMEN_UNKNOWN)
      switch(ar->start[p]->rank) {       
      case 0:      
	ar->dimen_type[p] = DIMEN_ELEMENT;
	break;          
          
      case 1:      
	ar->dimen_type[p] = DIMEN_VECTOR;
	break;     
     
      default:          
	g95_error("Array index at %L is an array of rank %d", &ar->c_where[p],   
		  ar->start[p]->rank);         
	return FAILURE;       
      }       
  }   
   
  /* If the reference type is unknown, figure out what kind it is */         
         
  if (ar->type == AR_UNKNOWN) { 
    ar->type = AR_ELEMENT;          
    for(p=0; p<ar->dimen; p++)  
      if (ar->dimen_type[p] == DIMEN_RANGE ||          
	  ar->dimen_type[p] == DIMEN_VECTOR) {   
	ar->type = AR_SECTION; 
	break;       
      }  
  }

  if (compare_spec_to_ref(ar) == FAILURE) return FAILURE;         
         
  return SUCCESS;   
}    
    
    
    
    
/* resolve_function()-- Resolve a function call, which means resolving
 * the arguments, then figuring out which entity the name refers to.
 * TODO: Check procedure arguments so that an INTENT(IN) isn't passed
 * to INTENT(OUT) or INTENT(INOUT).  */          
          
static try resolve_function(g95_expr *expr) {
g95_actual_arglist *arg; 
char *name;    
try s;      
      
  if (resolve_actual_arglist(expr->value.function.actual) == FAILURE) 
    return FAILURE;       
       
/* See if function is already resolved */    
    
  if (expr->value.function.isym != NULL) {         
    if (expr->ts.type == BT_UNKNOWN)       
      g95_internal_error("resolve_function(): untyped intrinsic");     
     
    s = SUCCESS;  
  } else {     /* Apply the rules of section 14.1.2 */  
  
    switch(procedure_kind(expr->symbol)) {     
    case PTYPE_GENERIC:     
      s = resolve_generic_f(expr);    
      break;         
         
    case PTYPE_SPECIFIC:     
      s = resolve_specific_f(expr);          
      break;

    case PTYPE_UNKNOWN:       
      s = resolve_unknown_f(expr);   
      break;    
    
    default:    
      g95_internal_error("resolve_function(): bad function type");        
    }     
  } 
 
  /* If the expression is still a function (it might have simplified),
   * then we check to see if we are calling an elemental function */   
   
  if (expr->type != EXPR_FUNCTION) return s;

  if (expr->value.function.actual != NULL &&         
      ((expr->symbol != NULL && expr->symbol->attr.elemental) ||     
       (expr->value.function.isym != NULL &&        
	expr->value.function.isym->elemental))) {

    /* The rank of an elemental is the rank of its array argument(s) */       
       
    for(arg=expr->value.function.actual; arg; arg=arg->next) {       
      if (arg->type != ALT_RETURN && arg->u.expr != NULL &&         
	  arg->u.expr->rank > 0) { 
	expr->rank = arg->u.expr->rank;         
	break;      
      }
    }  
  }        
        
  if (!pure_function(expr, &name)) {      
    if (forall_flag) {
      g95_error("Function reference to '%s' at %L is inside a FORALL block",      
		name, &expr->where);     
      s = FAILURE;  
    } else if (g95_pure(NULL)) {   
      g95_error("Function reference to '%s' at %L is to a non-PURE "  
		"procedure within a PURE procedure", name, &expr->where);     
      s = FAILURE;
    }     
  }       
       
  return s;        
} 
 
 
        
        
static try resolve_specific_s(g95_code *k) {  
g95_symbol *symb;      
match r;  
  
  symb = k->sym;   
   
  r = resolve_specific_s0(k, symb);   
  if (r == MATCH_YES) return SUCCESS;        
  if (r == MATCH_ERROR) return FAILURE; 
 
  g95_find_symbol(symb->name, symb->ns->parent, 1, &symb);    
    
  if (symb != NULL) {       
    r = resolve_specific_s0(k, symb);          
    if (r == MATCH_YES) return SUCCESS;       
    if (r == MATCH_ERROR) return FAILURE;         
  }    
    
  g95_error("Unable to resolve the specific subroutine '%s' at %L",   
	    symb->name, &k->loc);

  return FAILURE;  
}  
  
  
        
        
/* resolve_ref()-- Resolve subtype references. */ 
 
static try resolve_ref(g95_expr *expr) {         
int current_part_dimension, n_components, seen_part_dimension;    
g95_ref *re;        
        
  for(re=expr->ref; re; re=re->next)      
    if (re->type == REF_ARRAY && re->u.ar.as == NULL) {         
      find_array_spec(expr);         
      break;  
    }  
  
  for(re=expr->ref; re; re=re->next)
    switch(re->type) {   
    case REF_ARRAY:   
      if (resolve_array_ref(&re->u.ar) == FAILURE) return FAILURE;          
      break;       
       
    case REF_COMPONENT:    
      break;          
          
    case REF_SUBSTRING:         
      resolve_substring(re);
      break;       
    }      
      
  /* Check constraints on part references. */      
      
  current_part_dimension = 0;
  seen_part_dimension = 0;         
  n_components = 0;   
   
  for(re=expr->ref; re; re=re->next) {    
    switch(re->type) { 
    case REF_ARRAY:        
      switch(re->u.ar.type) {  
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
	  re->u.c.component->pointer) {      
	g95_error("Component to the right of a part reference with nonzero "
		  "rank must not have the POINTER attribute at %L",         
		  &expr->where);
	return FAILURE;
      }   
   
      n_components++;         
      break;       
       
    case REF_SUBSTRING:     
      break;       
    }  
  
    if (((re->type == REF_COMPONENT && n_components > 1) ||       
	 re->next == NULL) && current_part_dimension && seen_part_dimension) {      
      
      g95_error("Two or more part references with nonzero rank must "    
		"not be specified at %L", &expr->where);  
      return FAILURE;     
    }      
      
    if (re->type == REF_COMPONENT) {     
      if (current_part_dimension) seen_part_dimension = 1;     
     
      current_part_dimension = 0;   /* reset to make sure */    
    }          
  }         
         
  return SUCCESS;  
}       
       
       


/* resolve_forall_iterators()-- Resolve a list of FORALL iterators */       
       
static void resolve_forall_iterators(g95_forall_iterator *iter) {    
    
  while(iter) {      
    if (g95_resolve_expr(iter->var) == SUCCESS &&  
	iter->var->ts.type != BT_INTEGER)       
      g95_error("FORALL Iteration variable at %L must be INTEGER", 
		&iter->var->where);      
      
    if (g95_resolve_expr(iter->start) == SUCCESS &&          
	iter->start->ts.type != BT_INTEGER)          
      g95_error("FORALL start expression at %L must be INTEGER",    
		&iter->start->where);       
       
    if (g95_resolve_expr(iter->end) == SUCCESS &&        
	iter->end->ts.type != BT_INTEGER)         
      g95_error("FORALL end expression at %L must be INTEGER",          
		&iter->end->where);

    if (g95_resolve_expr(iter->stride) == SUCCESS &&    
	iter->stride->ts.type != BT_INTEGER)    
      g95_error("FORALL Stride expression at %L must be INTEGER",          
		&iter->stride->where);       
       
    iter = iter->next;      
  }    
}


  
  
/* resolve_data()-- Resolve a single DATA statement. */         
         
static void resolve_data(g95_data *z) {       
g95_data_value *val;        
        
  if (resolve_data_variables(z->var) == FAILURE) return; 
 
  for(val=z->value; val; val=val->next)
    if (g95_resolve_expr(val->expr) == FAILURE) break;         
}          
          
          
 
 
/* resolve_variable()-- Resolve a variable. */         
         
static try resolve_variable(g95_expr *c) {  
g95_symbol *symb;    
    
  symb = find_declaration(c->symbol);  
  
  resolve_symbol(symb);
  c->symbol = symb;   
   
  symb->attr.used = 1;  
  if (c->ref && resolve_ref(c) == FAILURE) return FAILURE;        
        
  if (c->symbol->attr.flavor == FL_PROCEDURE && !c->symbol->attr.function) {   
    c->ts.type = BT_PROCEDURE;   
    return SUCCESS;      
  }         
         
  if (c->symbol->ts.type != BT_UNKNOWN)      
    g95_variable_attr(c, &c->ts); 
  else {    /* Must be a simple variable reference */  
    if (g95_set_default_type(c->symbol, 1, NULL) == FAILURE) return FAILURE;      
    c->ts = c->symbol->ts;          
  }       
       
  return SUCCESS;      
}       
       
       
     
     
/* resolve_call()-- Resolve a subroutine call.  Although it was
 * tempting to use the same code for functions, subroutines and
 * functions are stored differently and this makes things awkward. */  
  
static try resolve_call(g95_code *z) {   
try d;    
    
  if (resolve_actual_arglist(z->ext.actual) == FAILURE) return FAILURE;       
       
  switch(procedure_kind(z->sym)) {   
  case PTYPE_GENERIC:      
    d = resolve_generic_s(z);     
    break;     
     
  case PTYPE_SPECIFIC:       
    d = resolve_specific_s(z);     
    break;          
          
  case PTYPE_UNKNOWN: 
    d = resolve_unknown_s(z);    
    break;  
  
  default:    
    g95_internal_error("resolve_call(): bad function type");          
  }

  return d;  
}      
      
      
   
   
/* resolve_code()-- Given a block of code, recursively resolve
 * everything pointed to by this code block */          
          
static void resolve_code(g95_code *code, g95_namespace *ns) {         
int forall_save=0; 
code_stack frame;     
g95_alloc *a;        
g95_code *u;   
try t;   
   
  if (code == NULL) return;  
  
  frame.prev = cs_base;   
  frame.head = code;     
  cs_base = &frame;         
         
  for(; code; code=code->next) {       
    frame.current = code;       
      
    if (code->type == EXEC_FORALL) {          
      forall_save = forall_flag;       
      forall_flag = 1;   
    }         
         
    if (code->type == EXEC_FORALL) forall_flag = forall_save;    
    
    t = g95_resolve_expr(code->expr);      
    if (g95_resolve_expr(code->expr2) == FAILURE) t = FAILURE;     
     
    switch(code->type) {       
    case EXEC_NOP:       case EXEC_CYCLE:     case EXEC_IOLENGTH:
    case EXEC_STOP:      case EXEC_EXIT:      case EXEC_CONTINUE:    
    case EXEC_DT_END:    case EXEC_TRANSFER:  
      break; 
 
    case EXEC_GOTO:   
      resolve_branch(code->label, code);          
      break;  
  
    case EXEC_RETURN:         
      if (code->expr != NULL && code->expr->ts.type != BT_INTEGER) 
	g95_error("Alternate RETURN statement at %L requires an INTEGER " 
		  "return specifier", &code->expr->where);    
      break;        
        
    case EXEC_ASSIGN:          
      if (t == FAILURE) break;     
      if (g95_extend_assign(code, ns) == SUCCESS) goto call;         
         
      code->expr->symbol->attr.set = 1;          
          
      if (g95_pure(NULL)) {       
	if (g95_impure_variable(code->expr->symbol)) {       
	  g95_error("Cannot assign to variable '%s' in PURE procedure at %L",    
		    code->expr->symbol->name, &code->expr->where);   
	  break;    
	}        
        
	if (code->expr2->ts.type == BT_DERIVED &&   
	    derived_pointer(code->expr2->ts.derived)) {   
	  g95_error("Right side of assignment at %L is a derived type " 
		    "containing a POINTER in a PURE procedure",         
		    &code->expr2->where);         
	  break;          
	}        
      }        
        
      g95_check_assign(code->expr, code->expr2, 1);   
      break;

    case EXEC_POINTER_ASSIGN:     
      if (t == FAILURE) break;       
       
      code->expr->symbol->attr.set = 1;      
      g95_check_pointer_assign(code->expr, code->expr2);
      break; 
 
    case EXEC_ARITHMETIC_IF:  
      if (t == SUCCESS && code->expr->ts.type != BT_INTEGER && 
	  code->expr->ts.type != BT_REAL)          
	g95_error("Arithmetic IF statement at %L requires a numeric "    
		  "expression", &code->expr->where);     
     
      resolve_branch(code->label, code);
      resolve_branch(code->label2, code);          
      resolve_branch(code->label3, code);    
      break;     
     
    case EXEC_IF:
      if (t == SUCCESS && code->expr != NULL &&       
	  (code->expr->ts.type != BT_LOGICAL || code->expr->rank != 0))  
	g95_error("IF clause at %L requires a scalar LOGICAL expression", 
		  &code->expr->where);        
        
      resolve_code(code->block, ns);        
      resolve_code(code->ext.block, ns);     
      break;

    case EXEC_CALL:      
    call:   
      resolve_call(code);      
      break;       
       
    case EXEC_SELECT:      /* Select is complicated */          
      g95_resolve_select(code); 
 
      /* Fall through */         
         
    case EXEC_WHERE:       
      for(u=code->block; u; u=u->block)         
	resolve_code(u->next, ns);      
      
      break;          
          
    case EXEC_DO:       
      if (code->ext.iterator != NULL) g95_resolve_iterator(code->ext.iterator);    
    
      resolve_code(code->block, ns);   
      break;    
    
    case EXEC_DO_WHILE:      
      resolve_code(code->block, ns);    
      if (code->expr == NULL) break;   
   
      if (t == SUCCESS &&  
          (code->expr->rank != 0 || code->expr->ts.type != BT_LOGICAL))          
	g95_error("Exit condition of DO WHILE loop at %L must be "     
		  "a scalar LOGICAL expression",      
		  &code->expr->where);     
     
      transform_while(code);   
      break; 
 
    case EXEC_ALLOCATE:       
      if (t == SUCCESS && code->expr != NULL) {
	code->expr->symbol->attr.set = 1;          
          
	if (code->expr->ts.type != BT_INTEGER) 
	  g95_error("STAT tag in ALLOCATE statement at %L must be "     
		    "of type INTEGER", &code->expr->where);          
      }       
       
      for(a=code->ext.alloc_list; a; a=a->next)      
        resolve_allocate_expr(a->expr);   
        
      break;   
   
    case EXEC_DEALLOCATE:     
      if (t == SUCCESS && code->expr != NULL) {  
	code->expr->symbol->attr.set = 1;    
    
	if (code->expr->ts.type != BT_INTEGER)   
	  g95_error("STAT tag in DEALLOCATE statement at %L must be of type "   
		    "INTEGER", &code->expr->where);        
      }         
         
      for(a=code->ext.alloc_list; a; a=a->next)    
	resolve_deallocate_expr(a->expr);        
        
      break;      
      
    case EXEC_OPEN:        
      if (g95_resolve_open(code->ext.open) == FAILURE) break;

      resolve_branch(code->ext.open->err, code);    
      break;        
        
    case EXEC_CLOSE:      
      if (g95_resolve_close(code->ext.close) == FAILURE) break;      
      
      resolve_branch(code->ext.close->err, code);  
      break;     
     
    case EXEC_BACKSPACE:    
    case EXEC_ENDFILE:       
    case EXEC_REWIND:      
      if (g95_resolve_filepos(code->ext.filepos) == FAILURE) break;       
       
      resolve_branch(code->ext.filepos->err, code);      
      break;         
         
    case EXEC_INQUIRE:       
      if (g95_resolve_inquire(code->ext.inquire) == FAILURE) break;

      resolve_branch(code->ext.inquire->err, code);    
      break; 
 
    case EXEC_READ:         
    case EXEC_WRITE:  
      if (g95_resolve_dt(code->ext.dt) == FAILURE) break;         
         
      resolve_branch(code->ext.dt->err, code);      
      resolve_branch(code->ext.dt->end, code);
      resolve_branch(code->ext.dt->eor, code);          
      break;       
       
    case EXEC_FORALL:     
      resolve_forall_iterators(code->ext.forall_iterator);     
     
      if (code->expr != NULL && code->expr->ts.type != BT_LOGICAL)       
	g95_error("FORALL mask clause at %L requires a LOGICAL expression",       
		  &code->expr->where);   
      break;

    default:              
      g95_internal_error("resolve_code(): Bad statement code");          
    }  
  }          
          
  cs_base = frame.prev; 
}   
   
   
        
        
/* g95_resolve_expr()-- Resolve an expression.  Make sure that types
 * of operands agree with their operators, intrinsic operators are
 * converted to function calls for overloaded types and unresolved
 * function references are resolved. */        
        
try g95_resolve_expr(g95_expr *h) { 
try q;          
          
  if (h == NULL) return SUCCESS;

  switch(h->type) {     
  case EXPR_OP:      
    q = resolve_operator(h);
    break;    
    
  case EXPR_FUNCTION:     
    q = resolve_function(h); 
    break;

  case EXPR_VARIABLE:  
    q = resolve_variable(h);
    if (q == SUCCESS) expression_rank(h);          
    break;          
          
  case EXPR_SUBSTRING:      
    q = resolve_ref(h);    
    break;        
        
  case EXPR_CONSTANT:          
  case EXPR_NULL:  
    q = SUCCESS;   
    break;         
         
  case EXPR_ARRAY: 
    q = FAILURE;   
    if (resolve_ref(h) == FAILURE) break;         
         
    expression_rank(h);   
   
    q = g95_resolve_array_constructor(h);
    if (q == SUCCESS) q = g95_expand_constructor(h);         
         
    break;         
         
  case EXPR_STRUCTURE:          
    q = resolve_structure_cons(h); 
    break;    
    
  default:    
    g95_internal_error("g95_resolve_expr(): Bad expression type");   
  }          
          
  return q;          
}        
        
        
     
     
/* resolve_equivalence()-- Validate a single equivalance by making
 * sure that if one of a set of equivalent variables is in a common
 * block, the rest are not in another block. */      
      
static void resolve_equivalence(g95_equiv *a) {   
g95_symbol *t, *j, *d, *s2;      
int seen_common;    
    
  seen_common = 0;     
  t = d = NULL;    
    
  for(; a; a=a->eq) {
    if (g95_resolve_expr(a->expr) == FAILURE) continue;        
        
    s2 = a->expr->symbol;     
    if (!s2->attr.in_common) continue; 
 
    j = find_common_block(s2);    
    
    if (!seen_common) {      
      t = j;   
      d = s2;       
      seen_common = 1;     
    } else {          
      if (t != j)   
	g95_error("Symbols '%s' at %L and '%s' are equivalenced but in " 
		  "different common blocks", d->name, &d->declared_at,  
		  s2->name);     
    }     
  }        
}          
          
          
    
    
/* g95_resolve()-- This function is called after a complete program
 * unit has been compiled.  Its purpose is to examine all of the
 * expressions associated with a program unit, assign types to all
 * intermediate expressions, make sure that all assignments are to
 * compatible types and figure out which names refer to which
 * functions or subroutines. */     
     
void g95_resolve(g95_namespace *ns) { 
g95_namespace *old_ns, *f;        
g95_charlen *cl;
g95_equiv *eq;       
g95_data *a;

  old_ns = g95_current_ns; 
  g95_current_ns = ns;  
  
  resolve_contained_functions(ns);          
          
  for(f=ns->contained; f; f=f->sibling) {        
    if (g95_pure(ns->proc_name) && !g95_pure(f->proc_name))        
      g95_error("Contained procedure '%s' at %L of a PURE procedure must "       
		"also be PURE", f->proc_name->name,      
		&f->proc_name->declared_at);         
         
    g95_resolve(f);        
  }         
         
  forall_flag = 0; 
  g95_check_interfaces(ns);    
    
  for(cl=ns->cl_list; cl; cl=cl->next) {          
    if (cl->length == NULL || g95_resolve_expr(cl->length) == FAILURE)
      continue;         
         
    if (cl->length->ts.type != BT_INTEGER)     
      g95_error("Character length specification at %L must be of type INTEGER",          
		&cl->length->where);        
  }   
   
  g95_traverse_ns(ns, resolve_initial_values); 
 
  if (ns->save_all) g95_save_all(ns);    
    
  for(a=ns->data; a; a=a->next)      
    resolve_data(a);          
          
  g95_generate_data(ns); 
 
  g95_traverse_ns(ns, resolve_common);          
          
  for(eq=ns->equiv; eq; eq=eq->next)     
    resolve_equivalence(eq);

  cs_base = NULL;     
  resolve_code(ns->code, ns);         
         
  g95_traverse_ns(ns, resolve_symbol);     
     
  if (g95_option.unused_label) warn_unused_label(ns);         
         
  if (g95_option.pedantic) g95_traverse_ns(ns, check_variable_usage);        
        
  g95_current_ns = old_ns;          
}          
