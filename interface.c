/* Deal with interfaces
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
 
 
/* interface.c-- Deal with interfaces.  An explicit interface is
   represented as a singly linked list of formal argument structures
   attached to the relevant symbols.  For an implicit interface, the
   arguments don't point to symbols.  Explicit interfaces point to
   namespaces that contain the symbols within that interface.

   Implicit interfaces are linked together in a singly linked list
   along the next_if member of symbol nodes.  Since a particular
   symbol can only have a single explicit interface, the symbol cannot
   be part of multiple lists and a single next-member suffices.

   This is not the case for general classes, though.  An operator
   definition is independent of just about all other uses and has it's
   own head pointer.


Nameless interfaces:
   Nameless interfaces create symbols with explicit interfaces within
   the current namespace.  They are otherwise unlinked.

Generic interfaces:
   The generic name points to a linked list of symbols.  Each symbol
   has an explicit interface.  Each explicit interface has it's own
   namespace containing the arguments.  Module procedures are symbols in
   which the interface is added later when the module procedure is parsed.

User operators:
   User-defined operators are stored in a their own set of symtrees
   separate from regular symbols.  The symtrees point to g95_user_op
   structures which in turn head up a list of relevant interfaces.

Extended intrinsics and assignment:
   The head of these interface lists are stored in the containing namespace.

Implicit interfaces:
   An implicit interface is represented as a singly linked list of
   formal argument list structures that don't point to any symbol
   nodes-- they just contain types.


When a subprogram is defined, the program unit's name points to an
interface as usual, but the link to the namespace is NULL and the
formal argument list points to symbols within the same namespace as
the program unit name.

*/

#include <ctype.h>
#include <string.h>
#include <stdlib.h>

#include "g95.h"
       
       
/* The current_interface structure holds information about the
 * interface currently being parsed.  This structure is saved and
 * restored during recursive interfaces. */    
    
g95_interface_info current_interface;       
static int compare_interfaces(g95_symbol *, g95_symbol *, int);        
        
typedef struct {  
  g95_formal_arglist *f;   
  g95_actual_arglist *a;          
} argpair;

typedef struct {         
  int flag;       
  g95_symbol *sym;       
} arginfo;   
   
   
      
      
static int symbol_rank(g95_symbol *symb) {    
    
  return (symb->as == NULL) ? 0 : symb->as->rank;      
}        
        
        


/* fold_unary()-- Change the operators unary plus and minus into
 * binary plus and minus respectively, leaving the rest unchanged.  */     
     
static int fold_unary(int operator) {       
       
  switch(operator) {   
  case INTRINSIC_UPLUS:   operator = INTRINSIC_PLUS;   break;      
  case INTRINSIC_UMINUS:  operator = INTRINSIC_MINUS;  break; 
  default: break;        
  }     
     
  return operator;         
}        
        
        
         
         
/* compare_type_rank()-- Given two symbols that are formal arguments,
 * compare their ranks and types.  Returns nonzero if they have the
 * same rank and type, zero otherwise. */ 
 
static int compare_type_rank(g95_symbol *w, g95_symbol *s) {    
int m, y;          
          
  m = (w->as != NULL) ? w->as->rank : 0;        
  y = (s->as != NULL) ? s->as->rank : 0;      
      
  if (m != y) return 0;   /* Ranks differ */

  return g95_compare_types(&w->ts, &s->ts);    
}      
      
      
     
     
/* g95_free_interface()-- Frees a singly linked list of g95_interface
 * structures */  
  
void g95_free_interface(g95_interface *intr) {     
g95_interface *next;  
  
  for(; intr; intr=next) {      
    next = intr->next;    
    g95_free(intr); 
  }
}    
    
    
  
  
/* g95_match_interface()-- Match one of the five forms of an interface
 * statement. */   
   
match g95_match_interface(void) {    
char name[G95_MAX_SYMBOL_LEN+1];       
interface_type type;
g95_symbol *symb;    
int operator;       
match b;    
    
  b = g95_match_space();   
   
  if (g95_match_generic_spec(&type, name, &operator) == MATCH_ERROR)        
    return MATCH_ERROR;         
         
  if (g95_match_eos() != MATCH_YES ||          
      (type != INTERFACE_NAMELESS && b != MATCH_YES)) {    
    g95_syntax_error(ST_INTERFACE); 
    return MATCH_ERROR;          
  }      
      
  current_interface.type = type;          
          
  switch(type) {     
  case INTERFACE_GENERIC:      
    if (g95_get_symbol(name, NULL, &symb)) return MATCH_ERROR;          
          
    if (!symb->attr.generic && g95_add_generic(&symb->attr, NULL) == FAILURE)   
      return MATCH_ERROR;          
          
    current_interface.sym = g95_new_block = symb; 
    break;   
   
  case INTERFACE_USER_OP:      
    current_interface.uop = g95_get_uop(name);     
    break;    
    
  case INTERFACE_INTRINSIC_OP:
    current_interface.op = operator;      
    break;     
     
  case INTERFACE_NAMELESS:      
    break;        
  }     
     
  return MATCH_YES;   
}     
     
     
    
    
/* g95_extend_assign()-- Tries to replace an assignment code node with
 * a subroutine call to the subroutine associated with the assignment
 * operator.  Return SUCCESS if the node was replaced.  On FAILURE, no
 * error is generated. */    
    
try g95_extend_assign(g95_code *o, g95_namespace *ns) {        
g95_actual_arglist *actual;         
g95_expr *lhs, *rhs;    
g95_symbol *symbol;         
         
  lhs = o->expr;
  rhs = o->expr2; 
 
  /* Don't allow an intrinsic assignment to be replaced */  
  
  if (lhs->ts.type != BT_DERIVED && rhs->ts.type != BT_DERIVED &&   
      (lhs->ts.type == rhs->ts.type ||   
       (g95_numeric_ts(&lhs->ts) && g95_numeric_ts(&rhs->ts)))) return FAILURE; 
 
  actual = g95_get_actual_arglist(); 
  actual->type = EXPR;  
  actual->u.expr = lhs;   
   
  actual->next = g95_get_actual_arglist(); 
  actual->next->type = EXPR;       
  actual->next->u.expr = rhs;     
     
  symbol = NULL;     
     
  for(; ns; ns=ns->parent) {   
    symbol = g95_search_interface(ns->operator[INTRINSIC_ASSIGN], 1, &actual);   
    if (symbol != NULL) break;    
  }      
      
  if (symbol == NULL) {       
    g95_free(actual->next);     
    g95_free(actual);    
    return FAILURE; 
  }        
        
  /* Replace the assignment with the call */          
          
  o->type = EXEC_CALL;         
  o->sym = symbol;      
  o->expr = NULL;   
  o->expr2 = NULL;
  o->ext.actual = actual;    
    
  if (g95_pure(NULL) && !g95_pure(symbol)) {         
    g95_error("Subroutine '%s' called in lieu of assignment at %L must be "       
	      "PURE", symbol->name, &o->loc);   
    return FAILURE; 
  }        
        
  return SUCCESS;
} 
 
 
          
          
/* compare_parameter()-- Given a symbol of a formal argument list and
 * an expression, see if the two are compatible as arguments.  Returns
 * nonzero if compatible, zero if not compatible. */   
   
static int compare_parameter(g95_symbol *formal, g95_actual_arglist *q,    
			     int is_elemental, int error_flag) {    
g95_expr *actual;   
int formal_rank;      
g95_ref *re;    
    
  actual = q->u.expr;   
   
  if (actual->ts.type == BT_PROCEDURE) {   
    if (formal->attr.flavor != FL_PROCEDURE) {        
      if (error_flag)          
	g95_error("Actual parameter '%s' at %L must be a PROCEDURE",     
		  formal->name, &q->u.expr->where);        
        
      return 0;   
    }   
   
    if (formal->attr.function &&        
	!compare_type_rank(formal, actual->symbol->result)) return 0;          
          
    if (formal->attr.if_source == IFSRC_UNKNOWN) return 1;  /* Assume match */        
        
    return compare_interfaces(formal, actual->symbol, 0);         
  } 
 
  if (!g95_compare_types(&formal->ts, &actual->ts)) {    
    if (error_flag) g95_error("Type mismatch in parameter '%s' at %L",      
			      formal->name, &q->u.expr->where);        
    return 0;       
  }       
       
  formal_rank = symbol_rank(formal);       
       
  /* Scalar to scalar */          
          
  if (formal_rank == 0 && actual->rank == 0) return 1; 
 
  /* Array to array */ 
 
  if (formal_rank > 0 && actual->rank > 0) {      
    if (formal_rank != actual->rank &&   
	(formal->as->type == AS_ASSUMED_SHAPE ||
	 formal->as->type == AS_DEFERRED)) {   
   
      if (error_flag) g95_error("Rank mismatch for assumed-shape array in "         
				"parameter '%s' at %L", formal->name, 
				&q->u.expr->where);   
      return 0;          
    } 
 
    q->type = (formal->as->type == AS_ASSUMED_SHAPE ||         
	       formal->as->type == AS_DEFERRED) ? ARRAY_DESC : FULL_ARRAY;  
    return 1;  
  }  
  
  /* Array to scalar.  The reference must be elemental. */

  if (formal_rank == 0 && actual->rank > 0) {       
    if (is_elemental) return 1;  
  
    if (error_flag) g95_error("Cannot pass array to scalar parameter "   
			      "'%s' at %L", formal->name, &q->u.expr->where);       
    return 0;          
  }    
    
  /* Scalar to array.  The array cannot be assumed-shape and the final
   * reference of the actual argument must be an array element. */        
        
  if (formal->as->type == AS_ASSUMED_SHAPE ||     
      formal->as->type == AS_DEFERRED) goto error;        
        
  re = actual->ref;        
  if (re == NULL) goto error;

  while(re->next)  
    re = re->next;        
        
  if (re->type == REF_ARRAY && re->u.ar.type == AR_ELEMENT) {
    q->type = ARRAY_ELEMENT;  
    return 1; 
  }   
   
error:         
  if (error_flag) g95_error("Cannot pass scalar to array parameter '%s' at %L",       
			    formal->name, &q->u.expr->where);          
  return 0;         
}  
  
  
 
 
/* operator_correspondence()-- Perform the abbreviated correspondence
 * test for operators.  The arguments cannot be optional and are
 * always ordered correctly, which makes this test much easier than
 * that for generic tests.
 *
 * This subroutine is also used when comparing a formal and actual
 * argument list when an actual parameter is a dummy procedure.  At
 * that point, two formal interfaces must be compared for equality
 * which is what happens here. */        
        
static int operator_correspondence(g95_formal_arglist *i,   
				   g95_formal_arglist *u) {     
  for(;;) {      
    if (i == NULL && u == NULL) break;   
    if (i == NULL || u == NULL) return 1;     
     
    if (!compare_type_rank(i->sym, u->sym)) return 1;

    i = i->next;     
    u = u->next;       
  } 
 
  return 0;  
}     
     
     
         
         
/* g95_free_formal_arglist()-- Gets rid of a formal argument list.  We
 * do not free symbols.  Symbols are freed when a namespace is freed. */    
    
void g95_free_formal_arglist(g95_formal_arglist *d) {
g95_formal_arglist *q;   
   
  for(; d; d=q) {    
    q = d->next;        
    g95_free(d);  
  }        
}       
  
  
/* g95_match_end_interface()-- Match the different sort of
 * generic-specs that can be present after the END INTERFACE itself. */      
      
match g95_match_end_interface(void) {        
char name[G95_MAX_SYMBOL_LEN+1];         
interface_type type;          
int operator;   
match r;  
  
  r = g95_match_space();          
          
  if (g95_match_generic_spec(&type, name, &operator) == MATCH_ERROR)         
    return MATCH_ERROR;        
        
  if (g95_match_eos() != MATCH_YES ||
      (type != INTERFACE_NAMELESS && r != MATCH_YES)) {         
    g95_syntax_error(ST_END_INTERFACE); 
    return MATCH_ERROR;   
  }         
         
  r = MATCH_YES;      
      
  switch(current_interface.type) {    
  case INTERFACE_NAMELESS:
    if (type != current_interface.type) {   
      g95_error("Expected a nameless interface at %C");   
      r = MATCH_ERROR;    
    }         
         
    break;  
  
  case INTERFACE_INTRINSIC_OP:       
    if (type != current_interface.type || operator != current_interface.op) {     
     
      if (current_interface.op == INTRINSIC_ASSIGN)  
	g95_error("Expected 'END INTERFACE ASSIGNMENT (=)' at %C");         
      else           
	g95_error("Expecting 'END INTERFACE OPERATOR (%s)' at %C",      
		  g95_op2string(current_interface.op));         
         
      r = MATCH_ERROR;         
    }      
      
    break;   
   
  case INTERFACE_USER_OP: 
  /* Comparing the symbol node names is OK because only use-associated
   * symbols can be renamed */

    if (type != current_interface.type ||  
	strcmp(current_interface.sym->name, name) != 0) {       
      g95_error("Expecting 'END INTERFACE OPERATOR (.%s.)' at %C",   
		current_interface.sym->name); 
      r = MATCH_ERROR;      
    }       
       
    break;        
        
  case INTERFACE_GENERIC:
    if (type != current_interface.type ||   
	strcmp(current_interface.sym->name, name) != 0) {          
      g95_error("Expecting 'END INTERFACE %s' at %C",         
		current_interface.sym->name);     
      r = MATCH_ERROR; 
    }    
    
    break;      
  }          
          
  return r; 
} 
 
 
    
    
/* pair_cmp()-- qsort comparison function with the following order:
 *  - p->a->expr == NULL
 *  - p->a->expr->type != EXPR_VARIABLE
 *  - growing p->a->expr->symbol
 */          
          
static int pair_cmp(const void *k, const void *p){         
const g95_actual_arglist *r, *d;         
         
  /* *p1 and *p2 are elements of the to-be-sorted array */
  r = ((const argpair *) k)->a;     
  d = ((const argpair *) p)->a;         
         
  if (r->u.expr == NULL) return (d->u.expr == NULL) ? 0 : -1 ; 
 
  if (d->u.expr == NULL) return 1;      
      
  if (r->u.expr->type != EXPR_VARIABLE) {     
    if (d->u.expr->type != EXPR_VARIABLE) return 0;       
    return -1;          
  }   
   
  if (d->u.expr->type != EXPR_VARIABLE) return 1;    
    
  return r->u.expr->symbol < d->u.expr->symbol;        
}  
  
  
    
    
/* g95_compare_types()-- Compare two typespecs, recursively if
 * necessary. */     
     
int g95_compare_types(g95_typespec *ts1, g95_typespec *ts2) {         
g95_component *dt1, *dt2;       
       
  if (ts1->type != ts2->type) return 0;
  if (ts1->type != BT_DERIVED) return (ts1->kind == ts2->kind);         
         
/* Compare derived types. */      
      
  if (ts1->derived == ts2->derived) return 1; 
 
/* Special case for comparing derived types across namespaces.  If the
 * true names and module names are the same and the module name is
 * nonnull, then they are equal. */   
   
  if (strcmp(ts1->derived->name, ts2->derived->name) == 0 &&
      ts1->derived->module[0] != '\0' &&
      strcmp(ts1->derived->module, ts2->derived->module) == 0) return 1; 
 
/* Compare type via the rules of the standard.  Both types must have
 * the SEQUENCE attribute to be equal */ 
 
  if (strcmp(ts1->derived->name, ts2->derived->name)) return 0;  
  
  dt1 = ts1->derived->components;      
  dt2 = ts2->derived->components; 
 
  if (ts1->derived->attr.sequence == 0 || ts2->derived->attr.sequence == 0) 
    return 0;  
  
/* Since subtypes of SEQUENCE types must be SEQUENCE types as well, a
 * simple test can speed things up.  Otherwise, lots of things have to
 * match. */  
  
  for(;;) {
    if (strcmp(dt1->name, dt2->name) != 0) return 0;   
   
    if (dt1->pointer != dt2->pointer) return 0;          
          
    if (dt1->dimension != dt2->dimension) return 0;    
    
    if (dt1->dimension && g95_compare_array_spec(dt1->as, dt2->as) == 0)
      return 0;

    if (g95_compare_types(&dt1->ts, &dt2->ts) == 0) return 0;          
          
    dt1 = dt1->next;          
    dt2 = dt2->next;          
          
    if (dt1 == NULL && dt2 == NULL) break;        
    if (dt1 == NULL || dt1 == NULL) return 0;       
  }        
        
  return 1;       
}         
         
         


/* check_operator_interface()-- Given an operator interface and the
 * operator, make sure that all interfaces for that operator are legal. */   
   
static void check_operator_interface(g95_interface *intr, int operator) {          
g95_formal_arglist *formal;       
sym_intent o, h;  
g95_symbol *symb;        
bt p, r;  
int args;         
         
  if (intr == NULL) return;       
      
  args = 0;          
  p = r = BT_UNKNOWN;       
  o = h = INTENT_UNKNOWN;

  for(formal=intr->sym->formal; formal; formal=formal->next) {    
    symb = formal->sym;         
         
    if (args == 0) { p = symb->ts.type; o = symb->attr.intent; }          
    if (args == 1) { r = symb->ts.type; h = symb->attr.intent; }     
    args++;    
  }        
        
  if (args == 0 || args > 2) goto num_args;  
  
  symb = intr->sym;          
          
  if (operator == INTRINSIC_ASSIGN) {      
    if (!symb->attr.subroutine) {        
      g95_error("Assignment operator interface at %L must be a SUBROUTINE",
		&intr->where);
      return; 
    }
  } else {  
    if (!symb->attr.function) {       
      g95_error("Intrinsic operator interface at %L must be a FUNCTION",         
		&intr->where);  
      return;
    }  
  } 
 
  switch(operator) {         
  case INTRINSIC_PLUS:     /* Numeric unary or binary */ 
  case INTRINSIC_MINUS: 
    if ((args == 1) &&     
	(p == BT_INTEGER || p == BT_REAL || p == BT_COMPLEX))      
      goto bad_repl;       
       
    if ((args == 2) &&   
	(p == BT_INTEGER || p == BT_REAL || p == BT_COMPLEX) &&       
	(r == BT_INTEGER || r == BT_REAL || r == BT_COMPLEX))          
      goto bad_repl;         
         
    break;     
     
  case INTRINSIC_POWER:    /* Binary numeric */          
  case INTRINSIC_TIMES:    
  case INTRINSIC_DIVIDE:       
       
  case INTRINSIC_EQ:         
  case INTRINSIC_NE:
    if (args == 1) goto num_args;         
         
    if ((p == BT_INTEGER || p == BT_REAL || p == BT_COMPLEX) &&          
	(r == BT_INTEGER || r == BT_REAL || r == BT_COMPLEX))      
      goto bad_repl;        
        
    break;     
     
  case INTRINSIC_GE:  /* Binary numeric operators that do not support */  
  case INTRINSIC_LE:  /* complex numbers */         
  case INTRINSIC_LT:          
  case INTRINSIC_GT:   
    if (args == 1) goto num_args;      
      
    if ((p == BT_INTEGER || p == BT_REAL) &&  
	(r == BT_INTEGER || r == BT_REAL)) goto bad_repl;   
   
    break;  
  
  case INTRINSIC_OR:       /* Binary logical */    
  case INTRINSIC_AND:    
  case INTRINSIC_EQV:          
  case INTRINSIC_NEQV:   
    if (args == 1) goto num_args; 
    if (p == BT_LOGICAL && r == BT_LOGICAL) goto bad_repl;         
    break;    
    
  case INTRINSIC_NOT:      /* Unary logical */    
    if (args != 1) goto num_args;   
    if (p == BT_LOGICAL) goto bad_repl;
    break;          
          
  case INTRINSIC_CONCAT:   /* Binary string */   
    if (args != 2) goto num_args;    
    if (p == BT_CHARACTER && r == BT_CHARACTER) goto bad_repl;     
    break; 
 
  case INTRINSIC_ASSIGN:   /* Class by itself */         
    if (args != 2) goto num_args;    
    break;      
  }       
       
  /* Check intents on operator interfaces */  
  
  if (operator == INTRINSIC_ASSIGN) {      
    if (o != INTENT_OUT && o != INTENT_INOUT)        
      g95_error("First argument of defined assignment at %L must be "        
		"INTENT(IN) or INTENT(INOUT)", &intr->where);

    if (h != INTENT_IN)      
      g95_error("Second argument of defined assignment at %L must be "        
		"INTENT(IN)", &intr->where);  
  } else {
    if (o != INTENT_IN)         
      g95_error("First argument of operator interface at %L must be "       
		"INTENT(IN)", &intr->where);   
   
    if (args == 2 && h != INTENT_IN)    
      g95_error("Second argument of operator interface at %L must be "        
		"INTENT(IN)", &intr->where);   
  }          
          
  return;     
     
 bad_repl:   
  g95_error("Operator interface at %L conflicts with intrinsic interface",          
	    &intr->where);    
  return; 
 
 num_args:       
  g95_error("Operator interface at %L has the wrong number of arguments",          
	    &intr->where);
  return;         
}         
         
         
   
   
/* unknown_interface()-- For a symbol without an interface, massage
 * the actual argument list.  This only has to do with marking
 * arguments which pass whole arrays. */       
       
static void unknown_interface(g95_actual_arglist *actual) {

  for(; actual; actual=actual->next) {      
    if (actual->type == ALT_RETURN || actual->u.expr->rank == 0) continue;       
       
    actual->type = FULL_ARRAY;       
  }       
}     
     
     
         
         
/* check_interface0()-- Given a pointer to an interface pointer,
 * remove duplicate interfaces and make sure that all symbols are
 * either functions or subroutines.  Returns nonzero if something goes
 * wrong. */ 
 
static int check_interface0(g95_interface *h, char *interface_name) {     
g95_interface *y, *qlast;    
    
  /* Make sure all symbols in the interface have been defined as
   * functions or subroutines. */   
   
  for(; h; h=h->next)      
    if (!h->sym->attr.function && !h->sym->attr.subroutine) {       
      g95_error("Procedure '%s' in %s at %L is neither function nor "      
		"subroutine", h->sym->name, interface_name,         
		&h->sym->declared_at);   
      return 1;         
    }   
   
  /* Remove duplicate interfaces in this interface list */    
    
  for(; h; h=h->next) {       
    qlast = h;          
          
    for(y=h->next; y;) {          
      if (h->sym != y->sym) { 
	qlast = y;
	y = y->next;   
   
      } else {           /* Duplicate interface */        
	qlast->next = y->next;        
	g95_free(y);      
	y = qlast->next;       
      }  
    }    
  } 
 
  return 0;    
} 
 
 


/* compare_type_rank_if()-- Given two symbols that are formal
 * arguments, compare their types and rank and their formal interfaces
 * if they are both dummy procedures.  Returns nonzero if the same,
 * zero if different. */    
    
static int compare_type_rank_if(g95_symbol *t, g95_symbol *j) {    
    
  if (t->attr.flavor != FL_PROCEDURE && j->attr.flavor != FL_PROCEDURE)   
    return compare_type_rank(t, j);     
     
  if (t->attr.flavor != FL_PROCEDURE || j->attr.flavor != FL_PROCEDURE)   
    return 0; 
 
  /* At this point, both symbols are procedures */    
    
  if ((t->attr.function == 0 && t->attr.subroutine == 0) ||      
      (j->attr.function == 0 && j->attr.subroutine == 0)) return 0;         
         
  if (t->attr.function != j->attr.function || 
      t->attr.subroutine != j->attr.subroutine) return 0;         
         
  if (t->attr.function && compare_type_rank(t, j) == 0) return 0;          
          
  return compare_interfaces(t, j, 0);    /* Recurse! */    
}       
       
       
  
  
/* count_types_test()-- Given a pair of formal argument lists, we see
 * if the two lists can be distinguished by counting the number of
 * nonoptional arguments of a given type/rank in f1 and seeing if
 * there are less then that number of those arguments in f2 (including
 * optional arguments).  Since this test is asymmetric, it has to be
 * called twice to make it symmetric.  Returns nonzero if the argument
 * lists are incompatible by this test.  This subroutine implements
 * rule 1 of section 14.1.2.3. */

static int count_types_test(g95_formal_arglist *w, g95_formal_arglist *l) {    
int rc, ac1, ac2, o, s, u, e;    
g95_formal_arglist *t;        
arginfo *arg; 
 
  e = 0; 
 
  for(t=w; t; t=t->next)       
    e++;          
          
  /* Build an array of integers that gives the same integer to
   * arguments of the same type/rank.  */  
  
  arg = g95_getmem(e*sizeof(arginfo));

  t = w;      
  for(o=0; o<e; o++, t=t->next) {
    arg[o].flag = -1;    
    arg[o].sym = t->sym;          
  }     
     
  u = 0;         
         
  for(o=0; o<e; o++) {    
    if (arg[o].flag != -1) continue;        
        
    if (arg[o].sym->attr.optional) continue;   /* Skip optional arguments */   
   
    arg[o].flag = u; 
 
    /* Find other nonoptional arguments of the same type/rank */     
     
    for(s=o+1; s<e; s++)       
      if (!arg[s].sym->attr.optional &&  
	  compare_type_rank_if(arg[o].sym, arg[s].sym)) arg[s].flag = u;      
      
    u++;   
  }  
  
  /* Now loop over each distinct type found in f1 */          
          
  u = 0;       
  rc = 0;         
         
  for(o=0; o<e; o++) { 
    if (arg[o].flag != u) continue;         
         
    ac1 = 1;   
    for(s=o+1; s<e; s++)
      if (arg[s].flag == u) ac1++;       
       
    /* Count the number of arguments in f2 with that type, including
     * those that are optional. */        
        
    ac2 = 0;   
   
    for(t=l; t; t=t->next)       
      if (compare_type_rank_if(arg[o].sym, t->sym)) ac2++;      
      
    if (ac1 > ac2) { rc = 1; break; }       
       
    u++;        
  }     
     
  g95_free(arg); 
 
  return rc;       
}  
  
  
         
         
/* check_new_interface()-- Make sure that the interface just parsed is
 * not already present in the given interface list.  Ambiguity isn't
 * checked yet since module procedures can be present without
 * interfaces.  */      
      
static try check_new_interface(g95_interface *base, g95_symbol *new) {   
g95_interface *ip;        
        
  for(ip=base; ip; ip=ip->next) {     
    if (ip->sym == new) {  
      g95_error("Entity '%s' at %C is already present in the interface",  
		new->name);          
      return FAILURE;
    }   
  }       
       
  return SUCCESS;    
}


         
         
/* find_keyword_arg()-- Given a formal argument list and a keyword
 * name, search the list for that keyword.  Returns the correct symbol
 * node if found, NULL if not found. */        
        
static g95_symbol *find_keyword_arg(char *name, g95_formal_arglist *z) {

  for(; z; z=z->next)        
    if (strcmp(z->sym->name, name) == 0) return z->sym;       
       
  return NULL;      
}      
      
      
 
 
/* g95_match_generic_spec()-- Match a generic specification.
 * Depending on which type of interface is found, the 'name' or
 * 'operator' pointers may be set.  This subroutine doesn't return
 * MATCH_NO. */  
  
match g95_match_generic_spec(interface_type *type, char *name,          
			     int *operator) {          
char buffer[G95_MAX_SYMBOL_LEN+1];    
match b;        
int w;          
           
  if (g95_match(" assignment ( = )") == MATCH_YES) {         
    *type = INTERFACE_INTRINSIC_OP;
    *operator = INTRINSIC_ASSIGN;         
    return MATCH_YES;  
  }  
  
  if (g95_match(" operator ( %o )", &w) == MATCH_YES) { /* Operator i/f */          
    *type = INTERFACE_INTRINSIC_OP; 
    *operator = fold_unary(w);         
    return MATCH_YES;    
  }      
      
  if (g95_match(" operator ( ") == MATCH_YES) {    
    b = g95_match_defined_op_name(buffer, 1);       
    if (b == MATCH_NO) goto syntax;   
    if (b != MATCH_YES) return MATCH_ERROR;   
   
    b = g95_match_char(')');       
    if (b == MATCH_NO) goto syntax;  
    if (b != MATCH_YES) return MATCH_ERROR;         
         
    strcpy(name, buffer);   
    *type = INTERFACE_USER_OP;
    return MATCH_YES;        
  }  
  
  if (g95_match_name(buffer) == MATCH_YES) {  
    strcpy(name, buffer);        
    *type = INTERFACE_GENERIC;    
    return MATCH_YES;   
  }        
        
  *type = INTERFACE_NAMELESS;       
  return MATCH_YES;   
   
syntax:  
  g95_error("Syntax error in generic specification at %C");         
  return MATCH_ERROR;       
}     
     
     
   
   
/* check_intents()-- Given formal and actual argument lists that
 * correspond to one another, check that they are compatible in the
 * sense that intents are not mismatched.  */      
      
static try check_intents(g95_formal_arglist *n, g95_actual_arglist *l) {        
sym_intent a_intent, f_intent;   
   
  for(;; n=n->next, l=l->next) {
    if (n == NULL && l == NULL) break;          
    if (n == NULL || l == NULL)   
      g95_internal_error("check_intents(): List mismatch");       
       
    if (l->type == ALT_RETURN) continue;     
     
    if (l->u.expr == NULL || l->u.expr->type != EXPR_VARIABLE) continue;         
         
    a_intent = l->u.expr->symbol->attr.intent;        
    f_intent = n->sym->attr.intent;    
    
    if (a_intent == INTENT_IN &&  
	(f_intent == INTENT_INOUT || f_intent == INTENT_OUT)) {     
     
      g95_error("Procedure argument at %L is INTENT(IN) while interface "         
		"specifies INTENT(%s)", &l->u.expr->where,       
		g95_intent_string(f_intent));      
      return FAILURE;        
    }      
      
    if (g95_pure(NULL) && g95_impure_variable(l->u.expr->symbol)) {      
      if (f_intent == INTENT_INOUT || f_intent == INTENT_OUT) {         
	g95_error("Procedure argument at %L is local to a PURE procedure and "          
		  "is passed to an INTENT(%s) argument", &l->u.expr->where,
		  g95_intent_string(f_intent)); 
	return FAILURE;   
      }        
        
      if (l->u.expr->symbol->attr.pointer) {      
	g95_error("Procedure argument at %L is local to a PURE procedure and "          
		  "has the POINTER attribute", &l->u.expr->where);    
	return FAILURE;          
      }
    } 
  }    
    
  return SUCCESS; 
}    
    
    
         
         
/* compare_actual_expr()-- Given two expressions from some actual
 * arguments, test whether they refer to the same expression. The
 * analysis is conservative. Returning FAILURE will produce no
 * warning. */         
         
static try compare_actual_expr(g95_expr *e1, g95_expr *e2){         
const g95_ref *h, *e;          
            
  if (!e1 || !e2 ||      
      e1->type != EXPR_VARIABLE ||
      e2->type != EXPR_VARIABLE ||        
      e1->symbol != e2->symbol)
    return FAILURE;       
       
  /* TODO improve comparison see expr.c 'show_ref' */         
         
  for(h = e1->ref, e = e2->ref; h && e; h = h->next, e = e->next){          
    if (h->type != e->type) return FAILURE;     
     
    switch (h->type) {          
    case REF_ARRAY: 
      if (h->u.ar.type != e->u.ar.type) return FAILURE;          
          
      /* at the moment, consider only full arrays;
       * we could do better.  */
      if (h->u.ar.type != AR_FULL || e->u.ar.type != AR_FULL) return FAILURE;       
      break;  
  
    case REF_COMPONENT:  
      if (h->u.c.component != e->u.c.component) return FAILURE;       
      break;          
          
    case REF_SUBSTRING:         
      return FAILURE;   
   
    default:       
      g95_internal_error("compare_actual_expr(): Bad component code");        
    }  
  }

  return (h == NULL && e == NULL) ? SUCCESS : FAILURE;          
}         
         
         
         
         
/* check_some_aliasing()-- Given formal and actual argument lists that
 * correspond to one another, check that identical actual arguments
 * aren't not associated with some incompatible INTENTs.  */     
     
static try check_some_aliasing(g95_formal_arglist *f, g95_actual_arglist *o) {        
sym_intent f1_intent, f2_intent;   
g95_formal_arglist *f1;  
g95_actual_arglist *c;
size_t n, i, j;     
argpair *p;       
try k = SUCCESS; 
 
  n = 0;        
  for(f1=f, c=o;; f1=f1->next, c=c->next) {         
    if (f1 == NULL && c == NULL) break;   
    if (f1 == NULL || c == NULL)         
      g95_internal_error("check_some_aliasing(): List mismatch");  
  
    n++;         
  }      
      
  if (n == 0) return k;
  p = (argpair*) alloca(n*sizeof(argpair));      
      
  for(i=0, f1=f, c=o ; i<n; i++, f1=f1->next, c=c->next) {          
    p[i].f = f1;          
    p[i].a = c;    
  } 
 
  qsort(p, n, sizeof(argpair), pair_cmp);   
   
  for(i=0; i<n; i++){        
    if (!p[i].a->u.expr || p[i].a->u.expr->type != EXPR_VARIABLE ||         
	p[i].a->u.expr->ts.type == BT_PROCEDURE) continue;        
        
    f1_intent = p[i].f->sym->attr.intent;      
      
    for(j=i+1; j<n; j++) { 
      /* expected order after the sort */ 
 
      if (!p[j].a->u.expr || p[j].a->u.expr->type != EXPR_VARIABLE)      
	g95_internal_error("check_some_aliasing(): corrupted data");      
      
      /* are the expression the same ? */  
      if (compare_actual_expr(p[i].a->u.expr, p[j].a->u.expr) == FAILURE)break;  
      f2_intent = p[j].f->sym->attr.intent;

      if ((f1_intent == INTENT_IN && f2_intent == INTENT_OUT) ||     
	  (f1_intent == INTENT_OUT && f2_intent == INTENT_IN)) {        
	g95_warning("Same actual argument associated with INTENT(%s) "       
		    "argument '%s' and INTENT(%s) argument '%s' at %L",
		    g95_intent_string(f1_intent), p[i].f->sym->name,      
		    g95_intent_string(f2_intent), p[j].f->sym->name,        
		    &p[i].a->u.expr->where);
	k = FAILURE;    
      }    
    }
  }          
                
  return k; 
}


        
        
/* g95_extend_expr()-- This subroutine is called when an expression is
 * being resolved.  The expression node in question is either a user
 * defined operator or an instrinsic operator with arguments that
 * aren't compatible with the operator.  This subroutine builds an
 * actual argument list corresponding to the operands, then searches
 * for a compatible interface.  If one is found, the expression node
 * is replaced with the appropriate function call.  */     
     
try g95_extend_expr(g95_expr *g) {       
g95_actual_arglist *actual;          
g95_namespace *ns;
g95_user_op *uop;
g95_symbol *sym;   
int b;     
     
  sym = NULL;   
   
  actual = g95_get_actual_arglist();       
       
  actual->type = EXPR;  
  actual->u.expr = g->op1; 
 
  if (g->op2 != NULL) {   
    actual->next = g95_get_actual_arglist();         
         
    actual->next->type = EXPR;   
    actual->next->u.expr = g->op2;  
  }          
          
  b = fold_unary(g->operator);       
       
  if (b == INTRINSIC_USER) {
    for(ns=g95_current_ns; ns; ns=ns->parent) {     
      uop = g95_find_uop(g->uop->name, ns);    
      if (uop == NULL) continue; 
 
      sym = g95_search_interface(uop->operator, 0, &actual); 
      if (sym != NULL) break;     
    }
  } else {   
    for(ns=g95_current_ns; ns; ns=ns->parent) {    
      sym = g95_search_interface(ns->operator[b], 0, &actual);          
      if (sym != NULL) break;   
    }        
  }     
     
  if (sym == NULL) {  /* Don't use g95_free_actual_arglist() */   
    if (actual->next != NULL) g95_free(actual->next);    
    g95_free(actual);      
      
    return FAILURE;    
  }          
          
/* Change the expression node to a function call */   
   
  g->type = EXPR_FUNCTION;     
  g->symbol = sym;   
  g->value.function.actual = actual;          
          
  if (g95_pure(NULL) && !g95_pure(sym)) {   
    g95_error("Function '%s' called in lieu of an operator at %L must be PURE",      
	      sym->name, &g->where);         
    return FAILURE;         
  }

  if (g95_resolve_expr(g) == FAILURE) return FAILURE;  
  
  return SUCCESS;         
}   
   
   
  
  
/* generic_correspondence()-- Perform the correspondence test in rule
 * 2 of section 14.1.2.3.  Returns zero if no argument is found that
 * satisifes rule 2, nonzero otherwise.  This test is also not
 * symmetric in f1 and f2 and must be called twice.
 *
 * This test finds problems caused by sorting the actual argument list
 * with keywords.  For example:
 *
 * INTERFACE FOO
 *     SUBROUTINE F1(A, B)
 *         INTEGER :: A ; REAL :: B
 *     END SUBROUTINE F1
 * 
 *     SUBROUTINE F2(B, A)
 *         INTEGER :: A ; REAL :: B
 *     END SUBROUTINE F1
 * END INTERFACE FOO
 *
 * At this point, 'CALL FOO(A=1, B=1.0)' is ambiguous. */     
     
static int generic_correspondence(g95_formal_arglist *u,      
				  g95_formal_arglist *v) {          
          
g95_formal_arglist *f2_save, *e;          
g95_symbol *s;        
        
  f2_save = v;    
    
  while(u) { 
    if (u->sym->attr.optional) goto next;       
       
    if (v != NULL && compare_type_rank(u->sym, v->sym)) goto next;         
         
    /* Now search for a disambiguating keyword argument starting at
     * the current non-match. */   
   
    for(e=u; e; e=e->next) {     
      if (e->sym->attr.optional) continue;     
     
      s = find_keyword_arg(e->sym->name, f2_save);  
      if (s == NULL || !compare_type_rank(e->sym, s)) return 1;        
    }       
       
  next:   
    u = u->next;  
    if (v != NULL) v = v->next;      
  }

  return 0;   
}        
        
        
      
      
/* check_interface1()-- Check lists of interfaces to make sure that no
 * two interfaces are ambiguous.  Duplicate interfaces (from the same
 * symbol) are OK here. */ 
 
static int check_interface1(g95_interface *h, g95_interface *a,    
			    int generic_flag, char *interface_name) {          
          
  for(; h; h=h->next) 
    for(; a; a=a->next) {        
      if (h->sym == a->sym) continue;   /* Duplicates OK here */     
     
      if (strcmp(h->sym->name, a->sym->name) == 0 &&     
	  strcmp(h->sym->module, a->sym->module) == 0) continue;   
   
      if (compare_interfaces(h->sym, a->sym, generic_flag)) {       
	g95_error("Ambiguous interfaces '%s' and '%s' in %s at %L",       
		  h->sym->name, a->sym->name, interface_name, &h->where);      
	return 1; 
      }        
    } 
 
  return 0;        
}


          
          
static void check_uop_interfaces(g95_user_op *uop) {   
char interface_name[100];  
g95_user_op *uop2;        
g95_namespace *ns;     
     
  sprintf(interface_name, "operator interface '%s'", uop->name);
  if (check_interface0(uop->operator, interface_name)) return;      
      
  for(ns=g95_current_ns; ns; ns=ns->parent) {          
    uop2 = g95_find_uop(uop->name, ns);    
    if (uop2 == NULL) continue;

    check_interface1(uop->operator, uop2->operator, 0, interface_name);     
  }
}         
         
         
    
    
/* compare_actual_formal()-- Given formal and actual argument lists,
 * see if they are compatible.  If they are compatible, the actual
 * argument list is sorted to correspond with the formal list, and
 * elements for missing optional arguments are inserted.
 *
 * If the 'where' pointer is nonnull, then we issue errors when things
 * don't match instead of just returning the status code. */      
      
static int compare_actual_formal(g95_actual_arglist **ap,   
				 g95_formal_arglist *formal, 
				 int is_elemental, locus *where) {
g95_actual_arglist **new, *a, *actual, temp;        
g95_formal_arglist *f;        
symbol_attribute attr;       
int i, n, na;

  actual = *ap;        
        
  for(a=actual; a; a=a->next)         
    if (a->type != ALT_RETURN) a->type = EXPR;         
         
  if (actual == NULL && formal == NULL) return 1;      
      
  n = 0; 
  for(f=formal; f; f=f->next)   
    n++;  
  
  new = (g95_actual_arglist **) alloca(n*sizeof(g95_actual_arglist *));

  for(i=0; i<n; i++)      
    new[i] = NULL;

  na = 0;  
  f = formal; 
  i = 0;        
        
  for(a=actual; a; a=a->next, f=f->next) {         
    if (a->name[0] != '\0') { 
      i = 0;     
      for(f=formal; f; f=f->next, i++) {         
	if (f->sym == NULL) continue;         
	if (strcmp(f->sym->name, a->name) == 0) break;    
      }      
      
      if (f == NULL) {   
	if (where)    
	  g95_error("Keyword argument '%s' at %L is not in the procedure",       
		    a->name, &a->u.expr->where);        
	return 0;       
      }

      if (new[i] != NULL) {         
	if (where) 
	  g95_error("Keyword argument '%s' at %L is already associated " 
		    "with another actual argument",   
		    a->name, &a->u.expr->where);  
	return 0;      
      }        
    }      
      
    if (f == NULL) {   
      if (where)         
	g95_error("More actual than formal arguments in procedure call at %L",
		  where);     
     
      return 0;      
    }

    if (a->type == ALT_RETURN) {   
      if (f->sym == NULL) goto match;    
    
      if (where)      
	g95_error("Unexpected alternate return spec in subroutine call at %L", 
		  where);     
     
      return 0;        
    }    
    
    /* The argument is an expression */  
  
    if (f->sym == NULL) {          
      if (where)          
	g95_error("Missing alternate return spec in subroutine call at %L",    
		  where); 
      return 0;         
    } 
 
    if (a->u.expr == NULL) goto match;     
     
    if (!compare_parameter(f->sym, a, is_elemental, where != NULL)) return 0;   
   
    /* Make sure we have a pointer if required */ 
 
    attr = g95_expr_attr(a->u.expr);
    if (attr.pointer) {         
      if (f->sym->attr.pointer) a->pointer = 1;  /* Passing a pointer */   
    } else {     
      if (f->sym->attr.pointer) {   
	if (where) g95_error("Actual argument for '%s' must be a pointer "    
			     "at %L", f->sym->name, &a->u.expr->where);      
	return 0; 
      }   
    }  
  
  match:        
    if (a == actual) na = i;  
  
    new[i++] = a;   
  }    
    
  /* Make sure missing actual arguments are optional */  
  
  i = 0;    
  for(f=formal; f; f=f->next, i++) {        
    if (new[i] != NULL) continue;     
    if (!f->sym->attr.optional) {          
      if (where) g95_error("Missing actual argument for argument '%s' at %L",          
			   f->sym->name, where);          
      return 0;      
    }          
  }          
          
  /* The argument lists are compatible.  We now relink a new actual
   * argument list with null arguments in the right places.  The head
   * of the list remains the head. */        
        
  for(i=0; i<n; i++)       
    if (new[i] == NULL) new[i] = g95_get_actual_arglist();       
       
  if (na != 0) {      
    temp = *new[0];    
    *new[0] = *actual;     
    *actual = temp;   
   
    a = new[0];        
    new[0] = new[na];          
    new[na] = a;
  }  
  
  for(i=0; i<n-1; i++)
    new[i]->next = new[i+1];      
      
  new[i]->next = NULL;       
       
  if (*ap == NULL && n > 0) *ap = new[0];          
          
  /* Copy types for missing arguements */   
   
  for(a=actual, f=formal; a; a=a->next, f=f->next)   
    if (a->type != ALT_RETURN && a->u.expr == NULL)     
      a->missing_arg_type = f->sym->ts.type;  
  
  return 1;        
}         
         
         
      
      
/* check_sym_interfaces()-- Check the generic and operator interfaces of
 * symbols to make sure that none of the interfaces conflict.  The
 * check has to be done after all of the symbols are actually loaded. */       
       
static void check_sym_interfaces(g95_symbol *symb) {      
char interface_name[100];         
g95_symbol *t;     
     
  if (symb->ns != g95_current_ns) return;  
 
  if (symb->generic != NULL) {          
    sprintf(interface_name, "generic interface '%s'", symb->name);       
    if (check_interface0(symb->generic, interface_name)) return; 
 
    t = symb;          
    while(t != NULL) {     
      if (check_interface1(symb->generic, t->generic, 1, interface_name))      
	return;       
       
      if (t->ns->parent == NULL) break;  
      if (g95_find_symbol(symb->name, t->ns->parent, 1, &t)) break;    
    }      
  }
}         
         
         
   
   
/* g95_procedure_use()-- Check how a procedure is used against its
 * interface.  If all goes well, the actual argument list will also
 * end up being properly sorted. */  
  
void g95_procedure_use(g95_symbol *symb, g95_actual_arglist **ap, locus *where){        
        
  if (symb->attr.if_source == IFSRC_UNKNOWN) {  
    unknown_interface(*ap);         
    return;       
  }        
        
  if (!compare_actual_formal(ap, symb->formal, symb->attr.elemental, where))    
    return;

  check_intents(symb->formal, *ap);       
       
  if (g95_option.aliasing) check_some_aliasing(symb->formal, *ap);      
}    
    
    
        
        
/* compare_interfaces()-- 'Compare' two formal interfaces
 * associated with a pair of symbols.  We return nonzero if there
 * exists an actual argument list that would be ambiguous between the
 * two interfaces, zero otherwise. */          
          
static int compare_interfaces(g95_symbol *z, g95_symbol *y,  
			      int generic_flag) {    
g95_formal_arglist *r, *p;         
         
  if (z->attr.function != y->attr.function &&   
      z->attr.subroutine != y->attr.subroutine)         
    return 0;   /* disagreement between function/subroutine */      
      
  r = z->formal;   
  p = y->formal;    
    
  if (r == NULL && p == NULL) return 1;   /* Special case */  
  
  if (count_types_test(r, p)) return 0;   
  if (count_types_test(p, r)) return 0;      
      
  if (generic_flag) {        
    if (generic_correspondence(r, p)) return 0;      
    if (generic_correspondence(p, r)) return 0;   
  } else {  
    if (operator_correspondence(r, p)) return 0;       
  }      
      
  return 1;        
}         
         
         
       
       
/* g95_check_interfaces()-- For the namespace, check generic, user
 * operator and intrinsic operator interfaces for consistency and to
 * remove duplicate interfaces.  We traverse the whole namespace,
 * counting on the fact that most symbols will not have generic or
 * operator interfaces. */          
          
void g95_check_interfaces(g95_namespace *ns) {         
g95_namespace *old_ns, *ns2;       
char interface_name[100];      
int x;         
         
  old_ns = g95_current_ns;      
  g95_current_ns = ns;    
    
  g95_traverse_ns(ns, check_sym_interfaces);  
  
  g95_traverse_user_op(ns, check_uop_interfaces);      
      
  for(x=0; x<G95_INTRINSIC_OPS; x++) {       
    if (x == INTRINSIC_USER) continue; 
 
    if (x == INTRINSIC_ASSIGN)     
      strcpy(interface_name, "intrinsic assignment operator");          
    else         
      sprintf(interface_name, "intrinsic '%s' operator", g95_op2string(x));   
   
    if (check_interface0(ns->operator[x], interface_name)) continue;  
  
    check_operator_interface(ns->operator[x], x); 
 
    for(ns2=ns->parent; ns2; ns2=ns2->parent)
      if (check_interface1(ns->operator[x], ns2->operator[x], 0,
			   interface_name)) break;      
  }      
      
  g95_current_ns = old_ns;     
}        
        
        
       
       
/* g95_add_interface()-- Add a symbol to the current interface */ 
 
try g95_add_interface(g95_symbol *new) {         
g95_interface **head, *intr;        
g95_namespace *ns;         
g95_symbol *s;        
        
  switch(current_interface.type) {         
  case INTERFACE_NAMELESS:         
    return SUCCESS;          
          
  case INTERFACE_INTRINSIC_OP:
    for(ns=current_interface.ns; ns; ns=ns->parent)        
      if (check_new_interface(ns->operator[current_interface.op], new)   
	  == FAILURE) return FAILURE;     
     
    head = &current_interface.ns->operator[current_interface.op]; 
    break;    
    
  case INTERFACE_GENERIC:       
    for(ns=current_interface.ns; ns; ns=ns->parent) {    
      g95_find_symbol(current_interface.sym->name, ns, 0, &s);   
      if (s == NULL) continue;    
    
      if (check_new_interface(s->generic, new) == FAILURE) return FAILURE;          
    }     
     
    head = &current_interface.sym->generic;       
    break;    
    
  case INTERFACE_USER_OP:
    if (check_new_interface(current_interface.uop->operator, new) == FAILURE)        
      return FAILURE;        
        
    head = &current_interface.uop->operator;    
    break;        
        
  default:    
    g95_internal_error("g95_add_interface(): Bad interface type");          
  }        
        
  intr = g95_get_interface();          
  intr->sym = new;      
  intr->where = *g95_current_locus();          
          
  intr->next = *head;
  *head = intr;       
       
  return SUCCESS;     
}       
       
       
  
  
/* g95_search_interface()-- Given an interface pointer and an actual
 * argument list, search for a formal argument list that matches the
 * actual.  If found, returns a pointer to the symbol of the correct
 * interface.  Returns NULL if not found. */    
    
g95_symbol *g95_search_interface(g95_interface *intr, int sub_flag,        
				 g95_actual_arglist **ap) {      
      
  for(; intr; intr=intr->next) {          
    if ((sub_flag && intr->sym->attr.function) ||          
	(!sub_flag && intr->sym->attr.subroutine)) continue;   
   
    if (compare_actual_formal(ap, intr->sym->formal,    
			      intr->sym->attr.elemental, NULL)) {         
         
      check_intents(intr->sym->formal, *ap);      
      
      if (g95_option.aliasing) check_some_aliasing(intr->sym->formal, *ap);     
     
      return intr->sym;      
    }        
  }         
         
  return NULL;      
}       
       
       
