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
 
 
   
   
/* compare_type_rank()-- Given two symbols that are formal arguments,
 * compare their ranks and types.  Returns nonzero if they have the
 * same rank and type, zero otherwise. */     
     
static int compare_type_rank(g95_symbol *r, g95_symbol *o) {   
int c, f;         
         
  c = (r->as != NULL) ? r->as->rank : 0;     
  f = (o->as != NULL) ? o->as->rank : 0;       
       
  if (c != f) return 0;   /* Ranks differ */       
       
  return g95_compare_types(&r->ts, &o->ts);          
}         
         
         
   
   
/* check_intents()-- Given formal and actual argument lists that
 * correspond to one another, check that they are compatible in the
 * sense that intents are not mismatched.  */          
          
static try check_intents(g95_formal_arglist *c, g95_actual_arglist *m) {       
sym_intent a_intent, f_intent;       
       
  for(;; c=c->next, m=m->next) {     
    if (c == NULL && m == NULL) break;      
    if (c == NULL || m == NULL)       
      g95_internal_error("check_intents(): List mismatch");         
         
    if (m->type == ALT_RETURN) continue;       
       
    if (m->u.expr == NULL || m->u.expr->type != EXPR_VARIABLE) continue;      
      
    a_intent = m->u.expr->symbol->attr.intent;        
    f_intent = c->sym->attr.intent;   
   
    if (a_intent == INTENT_IN &&      
	(f_intent == INTENT_INOUT || f_intent == INTENT_OUT)) {      
      
      g95_error("Procedure argument at %L is INTENT(IN) while interface "      
		"specifies INTENT(%s)", &m->u.expr->where,     
		g95_intent_string(f_intent));       
      return FAILURE;     
    }

    if (g95_pure(NULL) && g95_impure_variable(m->u.expr->symbol)) {        
      if (f_intent == INTENT_INOUT || f_intent == INTENT_OUT) {  
	g95_error("Procedure argument at %L is local to a PURE procedure and "   
		  "is passed to an INTENT(%s) argument", &m->u.expr->where,    
		  g95_intent_string(f_intent));          
	return FAILURE;    
      }     
     
      if (m->u.expr->symbol->attr.pointer) {   
	g95_error("Procedure argument at %L is local to a PURE procedure and " 
		  "has the POINTER attribute", &m->u.expr->where);         
	return FAILURE;        
      }          
    }       
  }

  return SUCCESS;       
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
      
      


/* g95_free_interface()-- Frees a singly linked list of g95_interface
 * structures */    
    
void g95_free_interface(g95_interface *inter) {    
g95_interface *n;         
         
  for(; inter; inter=n) {
    n = inter->next;          
    g95_free(inter);        
  }        
}  
  
  
     
     
/* check_operator_interface()-- Given an operator interface and the
 * operator, make sure that all interfaces for that operator are legal. */        
        
static void check_operator_interface(g95_interface *intr, int op1) {        
g95_formal_arglist *formal;    
sym_intent i1, e;      
g95_symbol *symbol;      
bt x, m;
int arg;      
      
  if (intr == NULL) return;          
         
  arg = 0;          
  x = m = BT_UNKNOWN;        
  i1 = e = INTENT_UNKNOWN;       
       
  for(formal=intr->sym->formal; formal; formal=formal->next) {       
    symbol = formal->sym;        
        
    if (arg == 0) { x = symbol->ts.type; i1 = symbol->attr.intent; }
    if (arg == 1) { m = symbol->ts.type; e = symbol->attr.intent; }        
    arg++;      
  }    
    
  if (arg == 0 || arg > 2) goto num_args;       
       
  symbol = intr->sym;   
   
  if (op1 == INTRINSIC_ASSIGN) {          
    if (!symbol->attr.subroutine) {       
      g95_error("Assignment operator interface at %L must be a SUBROUTINE",        
		&intr->where);     
      return;
    }       
  } else { 
    if (!symbol->attr.function) {
      g95_error("Intrinsic operator interface at %L must be a FUNCTION", 
		&intr->where);          
      return;    
    }      
  }     
     
  switch(op1) {
  case INTRINSIC_PLUS:     /* Numeric unary or binary */        
  case INTRINSIC_MINUS:      
    if ((arg == 1) &&      
	(x == BT_INTEGER || x == BT_REAL || x == BT_COMPLEX))       
      goto bad_repl;         
         
    if ((arg == 2) &&          
	(x == BT_INTEGER || x == BT_REAL || x == BT_COMPLEX) &&    
	(m == BT_INTEGER || m == BT_REAL || m == BT_COMPLEX))    
      goto bad_repl; 
 
    break;

  case INTRINSIC_POWER:    /* Binary numeric */    
  case INTRINSIC_TIMES:        
  case INTRINSIC_DIVIDE:        
        
  case INTRINSIC_EQ:         
  case INTRINSIC_NE:         
    if (arg == 1) goto num_args;

    if ((x == BT_INTEGER || x == BT_REAL || x == BT_COMPLEX) &&     
	(m == BT_INTEGER || m == BT_REAL || m == BT_COMPLEX))     
      goto bad_repl;         
         
    break;

  case INTRINSIC_GE:  /* Binary numeric operators that do not support */  
  case INTRINSIC_LE:  /* complex numbers */        
  case INTRINSIC_LT:         
  case INTRINSIC_GT:          
    if (arg == 1) goto num_args;         
         
    if ((x == BT_INTEGER || x == BT_REAL) &&        
	(m == BT_INTEGER || m == BT_REAL)) goto bad_repl;

    break;    
    
  case INTRINSIC_OR:       /* Binary logical */  
  case INTRINSIC_AND:    
  case INTRINSIC_EQV:        
  case INTRINSIC_NEQV:   
    if (arg == 1) goto num_args;       
    if (x == BT_LOGICAL && m == BT_LOGICAL) goto bad_repl;  
    break;     
     
  case INTRINSIC_NOT:      /* Unary logical */  
    if (arg != 1) goto num_args;  
    if (x == BT_LOGICAL) goto bad_repl;     
    break;    
    
  case INTRINSIC_CONCAT:   /* Binary string */       
    if (arg != 2) goto num_args;         
    if (x == BT_CHARACTER && m == BT_CHARACTER) goto bad_repl;     
    break;    
    
  case INTRINSIC_ASSIGN:   /* Class by itself */        
    if (arg != 2) goto num_args;         
    break;   
  }

  /* Check intents on operator interfaces */        
        
  if (op1 == INTRINSIC_ASSIGN) {      
    if (i1 != INTENT_OUT && i1 != INTENT_INOUT)  
      g95_error("First argument of defined assignment at %L must be "          
		"INTENT(IN) or INTENT(INOUT)", &intr->where);   
   
    if (e != INTENT_IN)         
      g95_error("Second argument of defined assignment at %L must be "
		"INTENT(IN)", &intr->where);      
  } else {     
    if (i1 != INTENT_IN)    
      g95_error("First argument of operator interface at %L must be "          
		"INTENT(IN)", &intr->where);          
          
    if (arg == 2 && e != INTENT_IN)       
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
    
    
       
       
/* fold_unary()-- Change the operators unary plus and minus into
 * binary plus and minus respectively, leaving the rest unchanged.  */  
  
static int fold_unary(int o) {        
        
  switch(o) {     
  case INTRINSIC_UPLUS:   o = INTRINSIC_PLUS;   break; 
  case INTRINSIC_UMINUS:  o = INTRINSIC_MINUS;  break;         
  default: break;         
  }         
         
  return o;       
}        
        
        
    
    
/* g95_match_interface()-- Match one of the five forms of an interface
 * statement. */

match g95_match_interface(void) {    
char n[G95_MAX_SYMBOL_LEN+1];  
interface_type type;  
g95_symbol *sy; 
int op1;
match t;

  t = g95_match_space();  
  
  if (g95_match_generic_spec(&type, n, &op1) == MATCH_ERROR)      
    return MATCH_ERROR;

  if (g95_match_eos() != MATCH_YES ||       
      (type != INTERFACE_NAMELESS && t != MATCH_YES)) { 
    g95_syntax_error(ST_INTERFACE);          
    return MATCH_ERROR;     
  }        
        
  current_interface.type = type; 
 
  switch(type) {   
  case INTERFACE_GENERIC:        
    if (g95_get_symbol(n, NULL, &sy)) return MATCH_ERROR;       
       
    if (!sy->attr.generic && g95_add_generic(&sy->attr, NULL) == FAILURE)        
      return MATCH_ERROR;   
   
    current_interface.sym = g95_new_block = sy;     
    break;

  case INTERFACE_USER_OP: 
    current_interface.uop = g95_get_uop(n);         
    break;  
  
  case INTERFACE_INTRINSIC_OP:         
    current_interface.op = op1; 
    break;   
   
  case INTERFACE_NAMELESS:  
    break;        
  }   
   
  return MATCH_YES;     
}       
       
       
        
        
/* pair_cmp()-- qsort comparison function with the following order:
 *  - p->a->expr == NULL
 *  - p->a->expr->type != EXPR_VARIABLE
 *  - growing p->a->expr->symbol
 */      
      
static int pair_cmp(const void *r, const void *q){  
const g95_actual_arglist *g, *p;  
  
  /* *p1 and *p2 are elements of the to-be-sorted array */     
  g = ((const argpair *) r)->a;         
  p = ((const argpair *) q)->a;          
          
  if (g->u.expr == NULL) return (p->u.expr == NULL) ? 0 : -1 ;          
          
  if (p->u.expr == NULL) return 1;     
     
  if (g->u.expr->type != EXPR_VARIABLE) {        
    if (p->u.expr->type != EXPR_VARIABLE) return 0;      
    return -1;          
  }

  if (p->u.expr->type != EXPR_VARIABLE) return 1;       
       
  return g->u.expr->symbol < p->u.expr->symbol;
}          
          
          
   
   
/* compare_type_rank_if()-- Given two symbols that are formal
 * arguments, compare their types and rank and their formal interfaces
 * if they are both dummy procedures.  Returns nonzero if the same,
 * zero if different. */ 
 
static int compare_type_rank_if(g95_symbol *p, g95_symbol *l) { 
 
  if (p->attr.flavor != FL_PROCEDURE && l->attr.flavor != FL_PROCEDURE)         
    return compare_type_rank(p, l);     
     
  if (p->attr.flavor != FL_PROCEDURE || l->attr.flavor != FL_PROCEDURE)  
    return 0;        
        
  /* At this point, both symbols are procedures */     
     
  if ((p->attr.function == 0 && p->attr.subroutine == 0) ||
      (l->attr.function == 0 && l->attr.subroutine == 0)) return 0;  
  
  if (p->attr.function != l->attr.function ||         
      p->attr.subroutine != l->attr.subroutine) return 0;       
       
  if (p->attr.function && compare_type_rank(p, l) == 0) return 0;

  return compare_interfaces(p, l, 0);    /* Recurse! */       
} 
 
 
          
          
/* g95_match_generic_spec()-- Match a generic specification.
 * Depending on which type of interface is found, the 'name' or
 * 'operator' pointers may be set.  This subroutine doesn't return
 * MATCH_NO. */      
      
match g95_match_generic_spec(interface_type *typ, char *nm,       
			     int *operator) { 
char buffer[G95_MAX_SYMBOL_LEN+1];    
match x;    
int w;
 
  if (g95_match(" assignment ( = )") == MATCH_YES) {
    *typ = INTERFACE_INTRINSIC_OP;        
    *operator = INTRINSIC_ASSIGN; 
    return MATCH_YES;     
  }        
        
  if (g95_match(" operator ( %o )", &w) == MATCH_YES) { /* Operator i/f */       
    *typ = INTERFACE_INTRINSIC_OP;  
    *operator = fold_unary(w);      
    return MATCH_YES;      
  }      
      
  if (g95_match(" operator ( ") == MATCH_YES) {       
    x = g95_match_defined_op_name(buffer, 1);
    if (x == MATCH_NO) goto syntax;       
    if (x != MATCH_YES) return MATCH_ERROR;         
         
    x = g95_match_char(')');
    if (x == MATCH_NO) goto syntax;    
    if (x != MATCH_YES) return MATCH_ERROR;      
      
    strcpy(nm, buffer);     
    *typ = INTERFACE_USER_OP;         
    return MATCH_YES;      
  }

  if (g95_match_name(buffer) == MATCH_YES) {
    strcpy(nm, buffer);       
    *typ = INTERFACE_GENERIC;     
    return MATCH_YES; 
  }

  *typ = INTERFACE_NAMELESS;    
  return MATCH_YES;     
     
syntax:          
  g95_error("Syntax error in generic specification at %C");         
  return MATCH_ERROR;      
}


      
      
/* compare_actual_expr()-- Given two expressions from some actual
 * arguments, test whether they refer to the same expression. The
 * analysis is conservative. Returning FAILURE will produce no
 * warning. */

static try compare_actual_expr(g95_expr *a, g95_expr *s){       
const g95_ref *o, *y;          
            
  if (!a || !s ||     
      a->type != EXPR_VARIABLE ||  
      s->type != EXPR_VARIABLE ||          
      a->symbol != s->symbol)        
    return FAILURE;  
  
  /* TODO improve comparison see expr.c 'show_ref' */   
   
  for(o = a->ref, y = s->ref; o && y; o = o->next, y = y->next){         
    if (o->type != y->type) return FAILURE;         
         
    switch (o->type) {         
    case REF_ARRAY:        
      if (o->u.ar.type != y->u.ar.type) return FAILURE;      
      
      /* at the moment, consider only full arrays;
       * we could do better.  */ 
      if (o->u.ar.type != AR_FULL || y->u.ar.type != AR_FULL) return FAILURE;     
      break;       
       
    case REF_COMPONENT: 
      if (o->u.c.component != y->u.c.component) return FAILURE;    
      break;          
          
    case REF_SUBSTRING:         
      return FAILURE;   
   
    default:   
      g95_internal_error("compare_actual_expr(): Bad component code"); 
    }         
  }       
       
  return (o == NULL && y == NULL) ? SUCCESS : FAILURE;       
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
         
static int operator_correspondence(g95_formal_arglist *u, 
				   g95_formal_arglist *l) {     
  for(;;) {    
    if (u == NULL && l == NULL) break;          
    if (u == NULL || l == NULL) return 1;          
          
    if (!compare_type_rank(u->sym, l->sym)) return 1;   
   
    u = u->next;        
    l = l->next; 
  }

  return 0;         
}          
          
          
  
  
/* check_interface1()-- Check lists of interfaces to make sure that no
 * two interfaces are ambiguous.  Duplicate interfaces (from the same
 * symbol) are OK here. */    
    
static int check_interface1(g95_interface *f, g95_interface *k, 
			    int generic_flag, char *interface_name) {      
      
  for(; f; f=f->next)        
    for(; k; k=k->next) {         
      if (f->sym == k->sym) continue;   /* Duplicates OK here */          
          
      if (strcmp(f->sym->name, k->sym->name) == 0 &&      
	  strcmp(f->sym->module, k->sym->module) == 0) continue;   
   
      if (compare_interfaces(f->sym, k->sym, generic_flag)) {
	g95_error("Ambiguous interfaces '%s' and '%s' in %s at %L",          
		  f->sym->name, k->sym->name, interface_name, &f->where);      
	return 1;          
      }         
    }      
      
  return 0;        
}         
         
         
          
          
/* check_some_aliasing()-- Given formal and actual argument lists that
 * correspond to one another, check that identical actual arguments
 * aren't not associated with some incompatible INTENTs.  */ 
 
static try check_some_aliasing(g95_formal_arglist *m, g95_actual_arglist *d) {          
sym_intent f1_intent, f2_intent;
g95_formal_arglist *b;   
g95_actual_arglist *o;    
size_t n, x, v;          
argpair *e;      
try h = SUCCESS;          
          
  n = 0;         
  for(b=m, o=d;; b=b->next, o=o->next) {
    if (b == NULL && o == NULL) break;     
    if (b == NULL || o == NULL)    
      g95_internal_error("check_some_aliasing(): List mismatch");     
     
    n++;         
  }        
        
  if (n == 0) return h;
  e = (argpair*) alloca(n*sizeof(argpair));        
        
  for(x=0, b=m, o=d ; x<n; x++, b=b->next, o=o->next) {       
    e[x].f = b;
    e[x].a = o;    
  }       
       
  qsort(e, n, sizeof(argpair), pair_cmp); 
 
  for(x=0; x<n; x++){          
    if (!e[x].a->u.expr || e[x].a->u.expr->type != EXPR_VARIABLE ||     
	e[x].a->u.expr->ts.type == BT_PROCEDURE) continue; 
 
    f1_intent = e[x].f->sym->attr.intent; 
 
    for(v=x+1; v<n; v++) {    
      /* expected order after the sort */     
     
      if (!e[v].a->u.expr || e[v].a->u.expr->type != EXPR_VARIABLE)
	g95_internal_error("check_some_aliasing(): corrupted data");     
     
      /* are the expression the same ? */ 
      if (compare_actual_expr(e[x].a->u.expr, e[v].a->u.expr) == FAILURE)break;         
      f2_intent = e[v].f->sym->attr.intent;  
  
      if ((f1_intent == INTENT_IN && f2_intent == INTENT_OUT) ||     
	  (f1_intent == INTENT_OUT && f2_intent == INTENT_IN)) {
	g95_warning("Same actual argument associated with INTENT(%s) "     
		    "argument '%s' and INTENT(%s) argument '%s' at %L",   
		    g95_intent_string(f1_intent), e[x].f->sym->name,
		    g95_intent_string(f2_intent), e[v].f->sym->name,       
		    &e[x].a->u.expr->where);  
	h = FAILURE;
      }
    }    
  }  
        
  return h;   
} 
 
 
     
     
/* check_interface0()-- Given a pointer to an interface pointer,
 * remove duplicate interfaces and make sure that all symbols are
 * either functions or subroutines.  Returns nonzero if something goes
 * wrong. */  
  
static int check_interface0(g95_interface *l, char *interface_name) {   
g95_interface *n, *qlast;        
        
  /* Make sure all symbols in the interface have been defined as
   * functions or subroutines. */        
        
  for(; l; l=l->next)         
    if (!l->sym->attr.function && !l->sym->attr.subroutine) {    
      g95_error("Procedure '%s' in %s at %L is neither function nor "   
		"subroutine", l->sym->name, interface_name,  
		&l->sym->declared_at);        
      return 1;    
    }   
   
  /* Remove duplicate interfaces in this interface list */          
          
  for(; l; l=l->next) {    
    qlast = l; 
 
    for(n=l->next; n;) {         
      if (l->sym != n->sym) {      
	qlast = n;     
	n = n->next;       
       
      } else {           /* Duplicate interface */       
	qlast->next = n->next;       
	g95_free(n);         
	n = qlast->next;    
      }         
    }   
  }    
    
  return 0;       
}         
         
         
  
  
/* count_types_test()-- Given a pair of formal argument lists, we see
 * if the two lists can be distinguished by counting the number of
 * nonoptional arguments of a given type/rank in f1 and seeing if
 * there are less then that number of those arguments in f2 (including
 * optional arguments).  Since this test is asymmetric, it has to be
 * called twice to make it symmetric.  Returns nonzero if the argument
 * lists are incompatible by this test.  This subroutine implements
 * rule 1 of section 14.1.2.3. */      
      
static int count_types_test(g95_formal_arglist *z, g95_formal_arglist *r) {  
int rc, ac1, ac2, a, h, x, w;
g95_formal_arglist *p;  
arginfo *args;         
         
  w = 0;   
   
  for(p=z; p; p=p->next)       
    w++;         
         
  /* Build an array of integers that gives the same integer to
   * arguments of the same type/rank.  */        
        
  args = g95_getmem(w*sizeof(arginfo));

  p = z;       
  for(a=0; a<w; a++, p=p->next) {      
    args[a].flag = -1;       
    args[a].sym = p->sym;          
  }   
   
  x = 0;     
     
  for(a=0; a<w; a++) { 
    if (args[a].flag != -1) continue;          
          
    if (args[a].sym->attr.optional) continue;   /* Skip optional arguments */ 
 
    args[a].flag = x;       
       
    /* Find other nonoptional arguments of the same type/rank */         
         
    for(h=a+1; h<w; h++) 
      if (!args[h].sym->attr.optional &&    
	  compare_type_rank_if(args[a].sym, args[h].sym)) args[h].flag = x;   
   
    x++;          
  }     
     
  /* Now loop over each distinct type found in f1 */       
       
  x = 0;    
  rc = 0; 
 
  for(a=0; a<w; a++) {       
    if (args[a].flag != x) continue;     
     
    ac1 = 1; 
    for(h=a+1; h<w; h++)         
      if (args[h].flag == x) ac1++;  
  
    /* Count the number of arguments in f2 with that type, including
     * those that are optional. */     
     
    ac2 = 0; 
 
    for(p=r; p; p=p->next)   
      if (compare_type_rank_if(args[a].sym, p->sym)) ac2++;       
       
    if (ac1 > ac2) { rc = 1; break; }

    x++;          
  }  
  
  g95_free(args);       
       
  return rc;    
}  
  
  


/* g95_match_end_interface()-- Match the different sort of
 * generic-specs that can be present after the END INTERFACE itself. */  
  
match g95_match_end_interface(void) {      
char name[G95_MAX_SYMBOL_LEN+1];   
interface_type dtype;        
int o;        
match l;      
      
  l = g95_match_space();     
     
  if (g95_match_generic_spec(&dtype, name, &o) == MATCH_ERROR)         
    return MATCH_ERROR;  
  
  if (g95_match_eos() != MATCH_YES || 
      (dtype != INTERFACE_NAMELESS && l != MATCH_YES)) {    
    g95_syntax_error(ST_END_INTERFACE);  
    return MATCH_ERROR;         
  }   
   
  l = MATCH_YES;  
  
  switch(current_interface.type) {    
  case INTERFACE_NAMELESS:
    if (dtype != current_interface.type) { 
      g95_error("Expected a nameless interface at %C");  
      l = MATCH_ERROR;      
    }     
     
    break;      
      
  case INTERFACE_INTRINSIC_OP:          
    if (dtype != current_interface.type || o != current_interface.op) {        
        
      if (current_interface.op == INTRINSIC_ASSIGN)       
	g95_error("Expected 'END INTERFACE ASSIGNMENT (=)' at %C");   
      else          
	g95_error("Expecting 'END INTERFACE OPERATOR (%s)' at %C",   
		  g95_op2string(current_interface.op));   
   
      l = MATCH_ERROR;          
    }

    break;

  case INTERFACE_USER_OP:      
  /* Comparing the symbol node names is OK because only use-associated
   * symbols can be renamed */          
          
    if (dtype != current_interface.type ||       
	strcmp(current_interface.sym->name, name) != 0) {  
      g95_error("Expecting 'END INTERFACE OPERATOR (.%s.)' at %C",      
		current_interface.sym->name);  
      l = MATCH_ERROR;  
    }

    break;       
       
  case INTERFACE_GENERIC:         
    if (dtype != current_interface.type || 
	strcmp(current_interface.sym->name, name) != 0) {        
      g95_error("Expecting 'END INTERFACE %s' at %C", 
		current_interface.sym->name);        
      l = MATCH_ERROR;     
    }         
         
    break;      
  }    
    
  return l;         
} 
 
 
    
    
/* find_keyword_arg()-- Given a formal argument list and a keyword
 * name, search the list for that keyword.  Returns the correct symbol
 * node if found, NULL if not found. */     
     
static g95_symbol *find_keyword_arg(char *name0, g95_formal_arglist *f) {       
       
  for(; f; f=f->next)
    if (strcmp(f->sym->name, name0) == 0) return f->sym;  
  
  return NULL;
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
         
static int generic_correspondence(g95_formal_arglist *i,   
				  g95_formal_arglist *t) {       
       
g95_formal_arglist *f2_save, *k;         
g95_symbol *symbol;     
     
  f2_save = t;  
  
  while(i) { 
    if (i->sym->attr.optional) goto next;     
     
    if (t != NULL && compare_type_rank(i->sym, t->sym)) goto next;   
   
    /* Now search for a disambiguating keyword argument starting at
     * the current non-match. */

    for(k=i; k; k=k->next) {       
      if (k->sym->attr.optional) continue; 
 
      symbol = find_keyword_arg(k->sym->name, f2_save);    
      if (symbol == NULL || !compare_type_rank(k->sym, symbol)) return 1;
    }     
     
  next:      
    i = i->next; 
    if (t != NULL) t = t->next;     
  }      
      
  return 0;      
}          
          
          
        
        
/* check_new_interface()-- Make sure that the interface just parsed is
 * not already present in the given interface list.  Ambiguity isn't
 * checked yet since module procedures can be present without
 * interfaces.  */   
   
static try check_new_interface(g95_interface *b, g95_symbol *n1) { 
g95_interface *ifp;         
         
  for(ifp=b; ifp; ifp=ifp->next) {     
    if (ifp->sym == n1) {         
      g95_error("Entity '%s' at %C is already present in the interface",      
		n1->name);      
      return FAILURE;
    }  
  }  
  
  return SUCCESS;          
}        
        
        
    
    
/* unknown_interface()-- For a symbol without an interface, massage
 * the actual argument list.  This only has to do with marking
 * arguments which pass whole arrays. */         
         
static void unknown_interface(g95_actual_arglist *f) {         
         
  for(; f; f=f->next) {       
    if (f->type == ALT_RETURN || f->u.expr->rank == 0) continue;          
          
    f->type = FULL_ARRAY; 
  }         
}          
          
          


static void check_uop_interfaces(g95_user_op *u) { 
char interface_name[100];      
g95_user_op *uop2;        
g95_namespace *ns;      
      
  sprintf(interface_name, "operator interface '%s'", u->name);        
  if (check_interface0(u->operator, interface_name)) return;       
       
  for(ns=g95_current_ns; ns; ns=ns->parent) { 
    uop2 = g95_find_uop(u->name, ns);          
    if (uop2 == NULL) continue;          
          
    check_interface1(u->operator, uop2->operator, 0, interface_name);     
  }      
}    
    
    


/* check_sym_interfaces()-- Check the generic and operator interfaces of
 * symbols to make sure that none of the interfaces conflict.  The
 * check has to be done after all of the symbols are actually loaded. */       
       
static void check_sym_interfaces(g95_symbol *s) {     
char interface_name[100];          
g95_symbol *v;      
      
  if (s->ns != g95_current_ns) return;   
  
  if (s->generic != NULL) { 
    sprintf(interface_name, "generic interface '%s'", s->name);      
    if (check_interface0(s->generic, interface_name)) return;    
    
    v = s;  
    while(v != NULL) {    
      if (check_interface1(s->generic, v->generic, 1, interface_name))
	return;   
   
      if (v->ns->parent == NULL) break;      
      if (g95_find_symbol(s->name, v->ns->parent, 1, &v)) break;       
    }          
  }  
}         
         
         
    
    
static int symbol_rank(g95_symbol *sy) {    
    
  return (sy->as == NULL) ? 0 : sy->as->rank;     
}       
       
       
     
     
/* compare_interfaces()-- 'Compare' two formal interfaces
 * associated with a pair of symbols.  We return nonzero if there
 * exists an actual argument list that would be ambiguous between the
 * two interfaces, zero otherwise. */       
       
static int compare_interfaces(g95_symbol *g, g95_symbol *w,
			      int generic_flag) {    
g95_formal_arglist *e, *q;     
     
  if (g->attr.function != w->attr.function &&         
      g->attr.subroutine != w->attr.subroutine)     
    return 0;   /* disagreement between function/subroutine */   
   
  e = g->formal;   
  q = w->formal;     
     
  if (e == NULL && q == NULL) return 1;   /* Special case */      
      
  if (count_types_test(e, q)) return 0;  
  if (count_types_test(q, e)) return 0;     
     
  if (generic_flag) {         
    if (generic_correspondence(e, q)) return 0; 
    if (generic_correspondence(q, e)) return 0;     
  } else {  
    if (operator_correspondence(e, q)) return 0;      
  }         
         
  return 1;
}   
   
   


/* compare_parameter()-- Given a symbol of a formal argument list and
 * an expression, see if the two are compatible as arguments.  Returns
 * nonzero if compatible, zero if not compatible. */       
       
static int compare_parameter(g95_symbol *formal, g95_actual_arglist *p,
			     int is_elemental, int error_flag) {  
g95_expr *act;        
int formal_rank; 
g95_ref *ref;         
         
  act = p->u.expr;    
    
  if (act->ts.type == BT_PROCEDURE) {      
    if (formal->attr.flavor != FL_PROCEDURE) {          
      if (error_flag)
	g95_error("Actual parameter '%s' at %L must be a PROCEDURE",    
		  formal->name, &p->u.expr->where);        
        
      return 0;     
    }    
    
    if (formal->attr.function &&         
	!compare_type_rank(formal, act->symbol->result)) return 0;  
  
    if (formal->attr.if_source == IFSRC_UNKNOWN) return 1;  /* Assume match */

    return compare_interfaces(formal, act->symbol, 0);       
  }    
    
  if (!g95_compare_types(&formal->ts, &act->ts)) {
    if (error_flag) g95_error("Type mismatch in parameter '%s' at %L",  
			      formal->name, &p->u.expr->where);     
    return 0;
  }

  formal_rank = symbol_rank(formal);      
      
  /* Scalar to scalar */      
      
  if (formal_rank == 0 && act->rank == 0) return 1;         
         
  /* Array to array */      
      
  if (formal_rank > 0 && act->rank > 0) { 
    if (formal_rank != act->rank &&        
	(formal->as->type == AS_ASSUMED_SHAPE ||    
	 formal->as->type == AS_DEFERRED)) {      
      
      if (error_flag) g95_error("Rank mismatch for assumed-shape array in "        
				"parameter '%s' at %L", formal->name,  
				&p->u.expr->where);   
      return 0;         
    }  
  
    p->type = (formal->as->type == AS_ASSUMED_SHAPE ||       
	       formal->as->type == AS_DEFERRED) ? ARRAY_DESC : FULL_ARRAY;       
    return 1;       
  }         
         
  /* Array to scalar.  The reference must be elemental. */       
       
  if (formal_rank == 0 && act->rank > 0) { 
    if (is_elemental) return 1;  
  
    if (error_flag) g95_error("Cannot pass array to scalar parameter "          
			      "'%s' at %L", formal->name, &p->u.expr->where);         
    return 0;         
  }   
   
  /* Scalar to array.  The array cannot be assumed-shape and the final
   * reference of the actual argument must be an array element. */ 
 
  if (formal->as->type == AS_ASSUMED_SHAPE ||
      formal->as->type == AS_DEFERRED) goto error;

  ref = act->ref;
  if (ref == NULL) goto error;     
     
  while(ref->next)     
    ref = ref->next; 
 
  if (ref->type == REF_ARRAY && ref->u.ar.type == AR_ELEMENT) {       
    p->type = ARRAY_ELEMENT;    
    return 1;  
  }     
     
error:       
  if (error_flag) g95_error("Cannot pass scalar to array parameter '%s' at %L",      
			    formal->name, &p->u.expr->where);        
  return 0;        
} 
 
 
          
          
/* g95_check_interfaces()-- For the namespace, check generic, user
 * operator and intrinsic operator interfaces for consistency and to
 * remove duplicate interfaces.  We traverse the whole namespace,
 * counting on the fact that most symbols will not have generic or
 * operator interfaces. */      
      
void g95_check_interfaces(g95_namespace *ns) {  
g95_namespace *old_ns, *ns2;       
char interface_name[100];          
int e;   
   
  old_ns = g95_current_ns;   
  g95_current_ns = ns;  
  
  g95_traverse_ns(ns, check_sym_interfaces);     
     
  g95_traverse_user_op(ns, check_uop_interfaces);   
   
  for(e=0; e<G95_INTRINSIC_OPS; e++) {     
    if (e == INTRINSIC_USER) continue;          
          
    if (e == INTRINSIC_ASSIGN)     
      strcpy(interface_name, "intrinsic assignment operator");    
    else    
      sprintf(interface_name, "intrinsic '%s' operator", g95_op2string(e));

    if (check_interface0(ns->operator[e], interface_name)) continue; 
 
    check_operator_interface(ns->operator[e], e);       
       
    for(ns2=ns->parent; ns2; ns2=ns2->parent)        
      if (check_interface1(ns->operator[e], ns2->operator[e], 0,  
			   interface_name)) break;          
  } 
 
  g95_current_ns = old_ns;  
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
				 int is_elemental, locus *loc) {       
g95_actual_arglist **old, *s, *real, t0;     
g95_formal_arglist *h;  
symbol_attribute attribute;        
int c, t, na;      
      
  real = *ap;      
      
  for(s=real; s; s=s->next)     
    if (s->type != ALT_RETURN) s->type = EXPR;       
       
  if (real == NULL && formal == NULL) return 1;

  t = 0;      
  for(h=formal; h; h=h->next)    
    t++;          
          
  old = (g95_actual_arglist **) alloca(t*sizeof(g95_actual_arglist *));    
    
  for(c=0; c<t; c++)       
    old[c] = NULL;  
  
  na = 0;        
  h = formal;  
  c = 0;        
        
  for(s=real; s; s=s->next, h=h->next) {          
    if (s->name[0] != '\0') {      
      c = 0;         
      for(h=formal; h; h=h->next, c++) {   
	if (h->sym == NULL) continue;
	if (strcmp(h->sym->name, s->name) == 0) break;      
      } 
 
      if (h == NULL) {        
	if (loc)    
	  g95_error("Keyword argument '%s' at %L is not in the procedure",         
		    s->name, &s->u.expr->where);    
	return 0;        
      }          
          
      if (old[c] != NULL) {        
	if (loc)    
	  g95_error("Keyword argument '%s' at %L is already associated "   
		    "with another actual argument",
		    s->name, &s->u.expr->where);        
	return 0;         
      } 
    }       
       
    if (h == NULL) {  
      if (loc)  
	g95_error("More actual than formal arguments in procedure call at %L",   
		  loc);         
         
      return 0;      
    }

    if (s->type == ALT_RETURN) {
      if (h->sym == NULL) goto match;      
      
      if (loc)
	g95_error("Unexpected alternate return spec in subroutine call at %L",       
		  loc);   
   
      return 0;
    }     
     
    /* The argument is an expression */         
         
    if (h->sym == NULL) {    
      if (loc)         
	g95_error("Missing alternate return spec in subroutine call at %L",
		  loc);        
      return 0;       
    }   
   
    if (s->u.expr == NULL) goto match;          
          
    if (!compare_parameter(h->sym, s, is_elemental, loc != NULL)) return 0;        
        
    /* Make sure we have a pointer if required */      
      
    attribute = g95_expr_attr(s->u.expr);
    if (attribute.pointer) {
      if (h->sym->attr.pointer) s->pointer = 1;  /* Passing a pointer */         
    } else {       
      if (h->sym->attr.pointer) {     
	if (loc) g95_error("Actual argument for '%s' must be a pointer "  
			     "at %L", h->sym->name, &s->u.expr->where);  
	return 0;    
      }         
    }

  match:    
    if (s == real) na = c;     
     
    old[c++] = s;    
  }

  /* Make sure missing actual arguments are optional */   
   
  c = 0;  
  for(h=formal; h; h=h->next, c++) {     
    if (old[c] != NULL) continue;         
    if (!h->sym->attr.optional) {       
      if (loc) g95_error("Missing actual argument for argument '%s' at %L",          
			   h->sym->name, loc); 
      return 0;   
    }        
  }     
     
  /* The argument lists are compatible.  We now relink a new actual
   * argument list with null arguments in the right places.  The head
   * of the list remains the head. */     
     
  for(c=0; c<t; c++)      
    if (old[c] == NULL) old[c] = g95_get_actual_arglist();         
         
  if (na != 0) {   
    t0 = *old[0];
    *old[0] = *real;    
    *real = t0;      
      
    s = old[0];         
    old[0] = old[na];  
    old[na] = s;      
  } 
 
  for(c=0; c<t-1; c++)  
    old[c]->next = old[c+1];  
  
  old[c]->next = NULL;

  if (*ap == NULL && t > 0) *ap = old[0];         
         
  /* Copy types for missing arguements */          
          
  for(s=real, h=formal; s; s=s->next, h=h->next)       
    if (s->type != ALT_RETURN && s->u.expr == NULL)   
      s->missing_arg_type = h->sym->ts.type; 
 
  return 1;   
}


        
        
/* g95_search_interface()-- Given an interface pointer and an actual
 * argument list, search for a formal argument list that matches the
 * actual.  If found, returns a pointer to the symbol of the correct
 * interface.  Returns NULL if not found. */       
       
g95_symbol *g95_search_interface(g95_interface *i, int sub_flag,
				 g95_actual_arglist **a) {

  for(; i; i=i->next) {     
    if ((sub_flag && i->sym->attr.function) ||   
	(!sub_flag && i->sym->attr.subroutine)) continue;        
        
    if (compare_actual_formal(a, i->sym->formal,         
			      i->sym->attr.elemental, NULL)) {     
     
      check_intents(i->sym->formal, *a);

      if (g95_option.aliasing) check_some_aliasing(i->sym->formal, *a);      
      
      return i->sym;     
    }     
  }

  return NULL; 
}  
  
  
  
  
/* g95_extend_assign()-- Tries to replace an assignment code node with
 * a subroutine call to the subroutine associated with the assignment
 * operator.  Return SUCCESS if the node was replaced.  On FAILURE, no
 * error is generated. */   
   
try g95_extend_assign(g95_code *i, g95_namespace *n) {
g95_actual_arglist *real;  
g95_expr *l, *right;        
g95_symbol *s;       
       
  l = i->expr;         
  right = i->expr2;   
   
  /* Don't allow an intrinsic assignment to be replaced */        
        
  if (l->ts.type != BT_DERIVED && right->ts.type != BT_DERIVED &&    
      (l->ts.type == right->ts.type ||      
       (g95_numeric_ts(&l->ts) && g95_numeric_ts(&right->ts)))) return FAILURE;

  real = g95_get_actual_arglist();
  real->type = EXPR;      
  real->u.expr = l;

  real->next = g95_get_actual_arglist();  
  real->next->type = EXPR;         
  real->next->u.expr = right;    
    
  s = NULL;  
  
  for(; n; n=n->parent) {       
    s = g95_search_interface(n->operator[INTRINSIC_ASSIGN], 1, &real);
    if (s != NULL) break;   
  }

  if (s == NULL) {       
    g95_free(real->next);          
    g95_free(real);  
    return FAILURE;          
  }    
    
  /* Replace the assignment with the call */         
         
  i->type = EXEC_CALL;      
  i->sym = s;       
  i->expr = NULL;      
  i->expr2 = NULL;         
  i->ext.actual = real;

  if (g95_pure(NULL) && !g95_pure(s)) {
    g95_error("Subroutine '%s' called in lieu of assignment at %L must be "        
	      "PURE", s->name, &i->loc);        
    return FAILURE;   
  }        
        
  return SUCCESS; 
} 
 
 
       
       
/* g95_add_interface()-- Add a symbol to the current interface */       
       
try g95_add_interface(g95_symbol *old) {      
g95_interface **head, *inter;  
g95_namespace *names;  
g95_symbol *symb;   
   
  switch(current_interface.type) {          
  case INTERFACE_NAMELESS:  
    return SUCCESS;      
      
  case INTERFACE_INTRINSIC_OP:        
    for(names=current_interface.ns; names; names=names->parent)   
      if (check_new_interface(names->operator[current_interface.op], old)     
	  == FAILURE) return FAILURE;   
   
    head = &current_interface.ns->operator[current_interface.op]; 
    break; 
 
  case INTERFACE_GENERIC:        
    for(names=current_interface.ns; names; names=names->parent) {        
      g95_find_symbol(current_interface.sym->name, names, 0, &symb); 
      if (symb == NULL) continue;     
     
      if (check_new_interface(symb->generic, old) == FAILURE) return FAILURE;   
    } 
 
    head = &current_interface.sym->generic;
    break;

  case INTERFACE_USER_OP:       
    if (check_new_interface(current_interface.uop->operator, old) == FAILURE) 
      return FAILURE;  
  
    head = &current_interface.uop->operator;      
    break;        
        
  default:
    g95_internal_error("g95_add_interface(): Bad interface type");   
  }         
         
  inter = g95_get_interface(); 
  inter->sym = old;        
  inter->where = *g95_current_locus();     
     
  inter->next = *head;       
  *head = inter;        
        
  return SUCCESS;          
}          
          
          
      
      
/* g95_extend_expr()-- This subroutine is called when an expression is
 * being resolved.  The expression node in question is either a user
 * defined operator or an instrinsic operator with arguments that
 * aren't compatible with the operator.  This subroutine builds an
 * actual argument list corresponding to the operands, then searches
 * for a compatible interface.  If one is found, the expression node
 * is replaced with the appropriate function call.  */       
       
try g95_extend_expr(g95_expr *o) {    
g95_actual_arglist *a;          
g95_namespace *ns;    
g95_user_op *op;     
g95_symbol *sym;       
int g;  
  
  sym = NULL;    
    
  a = g95_get_actual_arglist();       
       
  a->type = EXPR;      
  a->u.expr = o->op1;         
         
  if (o->op2 != NULL) {    
    a->next = g95_get_actual_arglist();

    a->next->type = EXPR;      
    a->next->u.expr = o->op2;  
  }       
       
  g = fold_unary(o->operator);  
  
  if (g == INTRINSIC_USER) {      
    for(ns=g95_current_ns; ns; ns=ns->parent) {  
      op = g95_find_uop(o->uop->name, ns);     
      if (op == NULL) continue;      
      
      sym = g95_search_interface(op->operator, 0, &a);          
      if (sym != NULL) break; 
    }         
  } else {         
    for(ns=g95_current_ns; ns; ns=ns->parent) {    
      sym = g95_search_interface(ns->operator[g], 0, &a);       
      if (sym != NULL) break;       
    } 
  }  
  
  if (sym == NULL) {  /* Don't use g95_free_actual_arglist() */     
    if (a->next != NULL) g95_free(a->next);          
    g95_free(a);          
          
    return FAILURE;         
  }          
          
/* Change the expression node to a function call */

  o->type = EXPR_FUNCTION;
  o->symbol = sym;
  o->value.function.actual = a;      
      
  if (g95_pure(NULL) && !g95_pure(sym)) {        
    g95_error("Function '%s' called in lieu of an operator at %L must be PURE",          
	      sym->name, &o->where);
    return FAILURE;       
  }    
    
  if (g95_resolve_expr(o) == FAILURE) return FAILURE;   
   
  return SUCCESS;      
}    
    
    
          
          
/* g95_free_formal_arglist()-- Gets rid of a formal argument list.  We
 * do not free symbols.  Symbols are freed when a namespace is freed. */     
     
void g95_free_formal_arglist(g95_formal_arglist *n) {      
g95_formal_arglist *f;

  for(; n; n=f) {          
    f = n->next;     
    g95_free(n);       
  }   
}     
        
        
/* g95_procedure_use()-- Check how a procedure is used against its
 * interface.  If all goes well, the actual argument list will also
 * end up being properly sorted. */     
     
void g95_procedure_use(g95_symbol *symbol, g95_actual_arglist **actual, locus *old_loc){ 
 
  if (symbol->attr.if_source == IFSRC_UNKNOWN) {
    unknown_interface(*actual); 
    return;         
  }       
       
  if (!compare_actual_formal(actual, symbol->formal, symbol->attr.elemental, old_loc))   
    return;       
       
  check_intents(symbol->formal, *actual);      
      
  if (g95_option.aliasing) check_some_aliasing(symbol->formal, *actual);          
}          
          
          
