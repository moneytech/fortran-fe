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
        
        
         
         
/* check_interface0()-- Given a pointer to an interface pointer,
 * remove duplicate interfaces and make sure that all symbols are
 * either functions or subroutines.  Returns nonzero if something goes
 * wrong. */   
   
static int check_interface0(g95_interface *f, char *interface_name) {   
g95_interface *t, *qlast;  
  
  /* Make sure all symbols in the interface have been defined as
   * functions or subroutines. */        
        
  for(; f; f=f->next) 
    if (!f->sym->attr.function && !f->sym->attr.subroutine) {         
      g95_error("Procedure '%s' in %s at %L is neither function nor "      
		"subroutine", f->sym->name, interface_name,        
		&f->sym->declared_at);   
      return 1;     
    }       
       
  /* Remove duplicate interfaces in this interface list */ 
 
  for(; f; f=f->next) {  
    qlast = f;      
      
    for(t=f->next; t;) { 
      if (f->sym != t->sym) {   
	qlast = t;          
	t = t->next;      
      
      } else {           /* Duplicate interface */     
	qlast->next = t->next;  
	g95_free(t);        
	t = qlast->next;          
      }         
    }      
  }    
    
  return 0;   
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
   
   
       
       
static int symbol_rank(g95_symbol *sym) {    
    
  return (sym->as == NULL) ? 0 : sym->as->rank; 
}    
    
    


/* g95_match_end_interface()-- Match the different sort of
 * generic-specs that can be present after the END INTERFACE itself. */         
         
match g95_match_end_interface(void) {         
char n[G95_MAX_SYMBOL_LEN+1];  
interface_type typ;       
int oper;          
match d;   
   
  d = g95_match_space();          
          
  if (g95_match_generic_spec(&typ, n, &oper) == MATCH_ERROR)       
    return MATCH_ERROR;  
  
  if (g95_match_eos() != MATCH_YES ||     
      (typ != INTERFACE_NAMELESS && d != MATCH_YES)) {      
    g95_syntax_error(ST_END_INTERFACE);       
    return MATCH_ERROR;       
  }         
         
  d = MATCH_YES;

  switch(current_interface.type) { 
  case INTERFACE_NAMELESS: 
    if (typ != current_interface.type) {     
      g95_error("Expected a nameless interface at %C"); 
      d = MATCH_ERROR;         
    }    
    
    break;

  case INTERFACE_INTRINSIC_OP:   
    if (typ != current_interface.type || oper != current_interface.op) {        
        
      if (current_interface.op == INTRINSIC_ASSIGN)   
	g95_error("Expected 'END INTERFACE ASSIGNMENT (=)' at %C");     
      else       
	g95_error("Expecting 'END INTERFACE OPERATOR (%s)' at %C",  
		  g95_op2string(current_interface.op));  
  
      d = MATCH_ERROR;       
    }       
       
    break;     
     
  case INTERFACE_USER_OP:      
  /* Comparing the symbol node names is OK because only use-associated
   * symbols can be renamed */    
    
    if (typ != current_interface.type ||  
	strcmp(current_interface.sym->name, n) != 0) {        
      g95_error("Expecting 'END INTERFACE OPERATOR (.%s.)' at %C",  
		current_interface.sym->name);  
      d = MATCH_ERROR;          
    }          
          
    break;         
         
  case INTERFACE_GENERIC:      
    if (typ != current_interface.type ||          
	strcmp(current_interface.sym->name, n) != 0) {
      g95_error("Expecting 'END INTERFACE %s' at %C",     
		current_interface.sym->name); 
      d = MATCH_ERROR;   
    }    
    
    break;        
  }      
      
  return d;         
}  
  
  
         
         
/* check_interface1()-- Check lists of interfaces to make sure that no
 * two interfaces are ambiguous.  Duplicate interfaces (from the same
 * symbol) are OK here. */       
       
static int check_interface1(g95_interface *l, g95_interface *j,        
			    int generic_flag, char *interface_name) {        
        
  for(; l; l=l->next)  
    for(; j; j=j->next) {    
      if (l->sym == j->sym) continue;   /* Duplicates OK here */         
         
      if (strcmp(l->sym->name, j->sym->name) == 0 &&  
	  strcmp(l->sym->module, j->sym->module) == 0) continue;  
  
      if (compare_interfaces(l->sym, j->sym, generic_flag)) {  
	g95_error("Ambiguous interfaces '%s' and '%s' in %s at %L",        
		  l->sym->name, j->sym->name, interface_name, &l->where);   
	return 1;      
      } 
    }      
      
  return 0;        
}      
      
      
         
         
/* check_new_interface()-- Make sure that the interface just parsed is
 * not already present in the given interface list.  Ambiguity isn't
 * checked yet since module procedures can be present without
 * interfaces.  */         
         
static try check_new_interface(g95_interface *bottom, g95_symbol *n1) {          
g95_interface *ifp;

  for(ifp=bottom; ifp; ifp=ifp->next) {        
    if (ifp->sym == n1) {  
      g95_error("Entity '%s' at %C is already present in the interface",        
		n1->name);   
      return FAILURE;      
    }        
  }

  return SUCCESS;         
}       
       
       
 
 
/* find_keyword_arg()-- Given a formal argument list and a keyword
 * name, search the list for that keyword.  Returns the correct symbol
 * node if found, NULL if not found. */

static g95_symbol *find_keyword_arg(char *nam, g95_formal_arglist *y) {

  for(; y; y=y->next)   
    if (strcmp(y->sym->name, nam) == 0) return y->sym;     
     
  return NULL;    
}        
        
        
          
          
/* check_operator_interface()-- Given an operator interface and the
 * operator, make sure that all interfaces for that operator are legal. */         
         
static void check_operator_interface(g95_interface *intf, int operator) {         
g95_formal_arglist *frm;   
sym_intent b, w;       
g95_symbol *sym;       
bt s, a;  
int arg;  
  
  if (intf == NULL) return;    
   
  arg = 0;      
  s = a = BT_UNKNOWN;         
  b = w = INTENT_UNKNOWN;

  for(frm=intf->sym->formal; frm; frm=frm->next) {     
    sym = frm->sym;   
   
    if (arg == 0) { s = sym->ts.type; b = sym->attr.intent; }        
    if (arg == 1) { a = sym->ts.type; w = sym->attr.intent; }          
    arg++;       
  }     
     
  if (arg == 0 || arg > 2) goto num_args;      
      
  sym = intf->sym;          
          
  if (operator == INTRINSIC_ASSIGN) {     
    if (!sym->attr.subroutine) {       
      g95_error("Assignment operator interface at %L must be a SUBROUTINE",    
		&intf->where);     
      return;      
    }        
  } else {      
    if (!sym->attr.function) {      
      g95_error("Intrinsic operator interface at %L must be a FUNCTION", 
		&intf->where);  
      return;        
    }       
  }         
         
  switch(operator) {     
  case INTRINSIC_PLUS:     /* Numeric unary or binary */      
  case INTRINSIC_MINUS:   
    if ((arg == 1) &&        
	(s == BT_INTEGER || s == BT_REAL || s == BT_COMPLEX))   
      goto bad_repl;    
    
    if ((arg == 2) &&
	(s == BT_INTEGER || s == BT_REAL || s == BT_COMPLEX) &&   
	(a == BT_INTEGER || a == BT_REAL || a == BT_COMPLEX))  
      goto bad_repl;      
      
    break;   
   
  case INTRINSIC_POWER:    /* Binary numeric */        
  case INTRINSIC_TIMES:        
  case INTRINSIC_DIVIDE:          
          
  case INTRINSIC_EQ:
  case INTRINSIC_NE:  
    if (arg == 1) goto num_args; 
 
    if ((s == BT_INTEGER || s == BT_REAL || s == BT_COMPLEX) &&     
	(a == BT_INTEGER || a == BT_REAL || a == BT_COMPLEX))     
      goto bad_repl;          
          
    break;      
      
  case INTRINSIC_GE:  /* Binary numeric operators that do not support */    
  case INTRINSIC_LE:  /* complex numbers */      
  case INTRINSIC_LT:       
  case INTRINSIC_GT:    
    if (arg == 1) goto num_args; 
 
    if ((s == BT_INTEGER || s == BT_REAL) &&        
	(a == BT_INTEGER || a == BT_REAL)) goto bad_repl;     
     
    break; 
 
  case INTRINSIC_OR:       /* Binary logical */          
  case INTRINSIC_AND:   
  case INTRINSIC_EQV: 
  case INTRINSIC_NEQV:
    if (arg == 1) goto num_args;     
    if (s == BT_LOGICAL && a == BT_LOGICAL) goto bad_repl;    
    break; 
 
  case INTRINSIC_NOT:      /* Unary logical */    
    if (arg != 1) goto num_args;    
    if (s == BT_LOGICAL) goto bad_repl;       
    break;     
     
  case INTRINSIC_CONCAT:   /* Binary string */   
    if (arg != 2) goto num_args;    
    if (s == BT_CHARACTER && a == BT_CHARACTER) goto bad_repl;         
    break;     
     
  case INTRINSIC_ASSIGN:   /* Class by itself */     
    if (arg != 2) goto num_args;    
    break;     
  }         
         
  /* Check intents on operator interfaces */       
       
  if (operator == INTRINSIC_ASSIGN) {  
    if (b != INTENT_OUT && b != INTENT_INOUT)
      g95_error("First argument of defined assignment at %L must be "          
		"INTENT(IN) or INTENT(INOUT)", &intf->where);     
     
    if (w != INTENT_IN) 
      g95_error("Second argument of defined assignment at %L must be "    
		"INTENT(IN)", &intf->where);      
  } else {      
    if (b != INTENT_IN)        
      g95_error("First argument of operator interface at %L must be "  
		"INTENT(IN)", &intf->where);

    if (arg == 2 && w != INTENT_IN)          
      g95_error("Second argument of operator interface at %L must be "          
		"INTENT(IN)", &intf->where);       
  }   
   
  return;    
    
 bad_repl:      
  g95_error("Operator interface at %L conflicts with intrinsic interface", 
	    &intf->where);       
  return;       
       
 num_args:       
  g95_error("Operator interface at %L has the wrong number of arguments",        
	    &intf->where);       
  return;
}    
    
    
      
      
static void check_uop_interfaces(g95_user_op *uop) { 
char interface_name[100];      
g95_user_op *uop2; 
g95_namespace *n;         
         
  sprintf(interface_name, "operator interface '%s'", uop->name);       
  if (check_interface0(uop->operator, interface_name)) return;  
  
  for(n=g95_current_ns; n; n=n->parent) { 
    uop2 = g95_find_uop(uop->name, n);          
    if (uop2 == NULL) continue;   
   
    check_interface1(uop->operator, uop2->operator, 0, interface_name);
  }       
}      
      
      
 
 
/* pair_cmp()-- qsort comparison function with the following order:
 *  - p->a->expr == NULL
 *  - p->a->expr->type != EXPR_VARIABLE
 *  - growing p->a->expr->symbol
 */     
     
static int pair_cmp(const void *b, const void *q){          
const g95_actual_arglist *i, *k;         
         
  /* *p1 and *p2 are elements of the to-be-sorted array */  
  i = ((const argpair *) b)->a;    
  k = ((const argpair *) q)->a;     
     
  if (i->u.expr == NULL) return (k->u.expr == NULL) ? 0 : -1 ;   
   
  if (k->u.expr == NULL) return 1;  
  
  if (i->u.expr->type != EXPR_VARIABLE) {    
    if (k->u.expr->type != EXPR_VARIABLE) return 0;    
    return -1;         
  } 
 
  if (k->u.expr->type != EXPR_VARIABLE) return 1;        
        
  return i->u.expr->symbol < k->u.expr->symbol;         
}       
       
       


/* g95_free_interface()-- Frees a singly linked list of g95_interface
 * structures */     
     
void g95_free_interface(g95_interface *inter) {  
g95_interface *nxt;      
      
  for(; inter; inter=nxt) {         
    nxt = inter->next;  
    g95_free(inter);  
  }     
}         
         
         
       
       
/* compare_type_rank()-- Given two symbols that are formal arguments,
 * compare their ranks and types.  Returns nonzero if they have the
 * same rank and type, zero otherwise. */          
          
static int compare_type_rank(g95_symbol *p, g95_symbol *u) {
int w, a;    
    
  w = (p->as != NULL) ? p->as->rank : 0;
  a = (u->as != NULL) ? u->as->rank : 0;  
  
  if (w != a) return 0;   /* Ranks differ */   
   
  return g95_compare_types(&p->ts, &u->ts);   
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
         
static int operator_correspondence(g95_formal_arglist *c,    
				   g95_formal_arglist *q) {         
  for(;;) {      
    if (c == NULL && q == NULL) break;   
    if (c == NULL || q == NULL) return 1; 
 
    if (!compare_type_rank(c->sym, q->sym)) return 1;   
   
    c = c->next;   
    q = q->next;         
  }    
    
  return 0;      
}       
       
       
     
     
/* g95_free_formal_arglist()-- Gets rid of a formal argument list.  We
 * do not free symbols.  Symbols are freed when a namespace is freed. */          
          
void g95_free_formal_arglist(g95_formal_arglist *t) {      
g95_formal_arglist *y;

  for(; t; t=y) {
    y = t->next;  
    g95_free(t);          
  }
}     
         
         
/* g95_match_generic_spec()-- Match a generic specification.
 * Depending on which type of interface is found, the 'name' or
 * 'operator' pointers may be set.  This subroutine doesn't return
 * MATCH_NO. */ 
 
match g95_match_generic_spec(interface_type *t, char *name0,    
			     int *op1) {         
char b[G95_MAX_SYMBOL_LEN+1];         
match a;       
int j;       
        
  if (g95_match(" assignment ( = )") == MATCH_YES) {  
    *t = INTERFACE_INTRINSIC_OP;        
    *op1 = INTRINSIC_ASSIGN;          
    return MATCH_YES;        
  }     
     
  if (g95_match(" operator ( %o )", &j) == MATCH_YES) { /* Operator i/f */ 
    *t = INTERFACE_INTRINSIC_OP;        
    *op1 = fold_unary(j);      
    return MATCH_YES;          
  }      
      
  if (g95_match(" operator ( ") == MATCH_YES) {          
    a = g95_match_defined_op_name(b, 1);        
    if (a == MATCH_NO) goto syntax;       
    if (a != MATCH_YES) return MATCH_ERROR;  
  
    a = g95_match_char(')');        
    if (a == MATCH_NO) goto syntax;    
    if (a != MATCH_YES) return MATCH_ERROR;   
   
    strcpy(name0, b);   
    *t = INTERFACE_USER_OP;      
    return MATCH_YES;     
  }      
      
  if (g95_match_name(b) == MATCH_YES) {     
    strcpy(name0, b);         
    *t = INTERFACE_GENERIC;          
    return MATCH_YES;        
  }       
       
  *t = INTERFACE_NAMELESS;   
  return MATCH_YES;         
         
syntax:         
  g95_error("Syntax error in generic specification at %C");      
  return MATCH_ERROR;          
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
     
static int generic_correspondence(g95_formal_arglist *a,     
				  g95_formal_arglist *e) {     
     
g95_formal_arglist *f2_save, *x;   
g95_symbol *sym;

  f2_save = e;          
          
  while(a) {      
    if (a->sym->attr.optional) goto next;        
        
    if (e != NULL && compare_type_rank(a->sym, e->sym)) goto next;       
       
    /* Now search for a disambiguating keyword argument starting at
     * the current non-match. */          
          
    for(x=a; x; x=x->next) {        
      if (x->sym->attr.optional) continue;       
       
      sym = find_keyword_arg(x->sym->name, f2_save);        
      if (sym == NULL || !compare_type_rank(x->sym, sym)) return 1;        
    }        
        
  next:         
    a = a->next;        
    if (e != NULL) e = e->next;    
  } 
 
  return 0;  
}   
   
   
          
          
/* g95_extend_assign()-- Tries to replace an assignment code node with
 * a subroutine call to the subroutine associated with the assignment
 * operator.  Return SUCCESS if the node was replaced.  On FAILURE, no
 * error is generated. */      
      
try g95_extend_assign(g95_code *z, g95_namespace *namesp) {       
g95_actual_arglist *real;      
g95_expr *left, *rhs;    
g95_symbol *symbol;     
     
  left = z->expr;    
  rhs = z->expr2;   
   
  /* Don't allow an intrinsic assignment to be replaced */       
       
  if (left->ts.type != BT_DERIVED && rhs->ts.type != BT_DERIVED &&         
      (left->ts.type == rhs->ts.type ||   
       (g95_numeric_ts(&left->ts) && g95_numeric_ts(&rhs->ts)))) return FAILURE;       
       
  real = g95_get_actual_arglist(); 
  real->type = EXPR;  
  real->u.expr = left; 
 
  real->next = g95_get_actual_arglist();      
  real->next->type = EXPR; 
  real->next->u.expr = rhs;          
          
  symbol = NULL;      
      
  for(; namesp; namesp=namesp->parent) {        
    symbol = g95_search_interface(namesp->operator[INTRINSIC_ASSIGN], 1, &real);
    if (symbol != NULL) break;  
  }    
    
  if (symbol == NULL) {       
    g95_free(real->next);  
    g95_free(real);       
    return FAILURE;
  }   
   
  /* Replace the assignment with the call */       
       
  z->type = EXEC_CALL;
  z->sym = symbol;     
  z->expr = NULL;
  z->expr2 = NULL; 
  z->ext.actual = real;          
          
  if (g95_pure(NULL) && !g95_pure(symbol)) {          
    g95_error("Subroutine '%s' called in lieu of assignment at %L must be " 
	      "PURE", symbol->name, &z->where); 
    return FAILURE;        
  } 
 
  return SUCCESS;   
}       
       
       
 
 
/* compare_parameter()-- Given a symbol of a formal argument list and
 * an expression, see if the two are compatible as arguments.  Returns
 * nonzero if compatible, zero if not compatible. */   
   
static int compare_parameter(g95_symbol *f, g95_actual_arglist *k,     
			     int is_elemental, int error_flag) {      
g95_expr *actual;         
int formal_rank;
g95_ref *reference;          
          
  actual = k->u.expr;      
      
  if (actual->ts.type == BT_PROCEDURE) {
    if (f->attr.flavor != FL_PROCEDURE) {   
      if (error_flag)   
	g95_error("Actual parameter '%s' at %L must be a PROCEDURE",  
		  f->name, &k->u.expr->where); 
 
      return 0;        
    }         
         
    if (f->attr.function &&      
	!compare_type_rank(f, actual->symbol->result)) return 0;      
      
    if (f->attr.if_source == IFSRC_UNKNOWN) return 1;  /* Assume match */   
   
    return compare_interfaces(f, actual->symbol, 0);  
  }     
     
  if (actual->type != EXPR_NULL &&       
      !g95_compare_types(&f->ts, &actual->ts)) {
    if (error_flag) g95_error("Type mismatch in parameter '%s' at %L",    
			      f->name, &k->u.expr->where);       
    return 0; 
  }    
    
  formal_rank = symbol_rank(f);      
      
  /* Scalar to scalar */        
        
  if (formal_rank == 0 && actual->rank == 0) return 1;     
     
  /* Array to array */  
  
  if (formal_rank > 0 && actual->rank > 0) {      
    if (formal_rank != actual->rank &&        
	(f->as->type == AS_ASSUMED_SHAPE ||   
	 f->as->type == AS_DEFERRED)) {  
  
      if (error_flag) g95_error("Rank mismatch for assumed-shape array in "          
				"parameter '%s' at %L", f->name,     
				&k->u.expr->where);   
      return 0;
    }       
       
    k->type = (f->as->type == AS_ASSUMED_SHAPE ||   
	       f->as->type == AS_DEFERRED) ? ARRAY_DESC : FULL_ARRAY;        
    return 1;  
  }      
      
  /* Array to scalar.  The reference must be elemental. */        
        
  if (formal_rank == 0 && actual->rank > 0) {         
    if (is_elemental) return 1;  
  
    if (error_flag) g95_error("Cannot pass array to scalar parameter "
			      "'%s' at %L", f->name, &k->u.expr->where);
    return 0; 
  }

  /* Scalar to array.  The array cannot be assumed-shape and the final
   * reference of the actual argument must be an array element. */ 
 
  if (f->as->type == AS_ASSUMED_SHAPE ||     
      f->as->type == AS_DEFERRED) goto error;        
        
  reference = actual->ref;
  if (reference == NULL) goto error;     
     
  while(reference->next)       
    reference = reference->next;     
     
  if (reference->type == REF_ARRAY && reference->u.ar.type == AR_ELEMENT) { 
    k->type = ARRAY_ELEMENT;
    return 1;      
  }   
   
error:      
  if (error_flag) g95_error("Cannot pass scalar to array parameter '%s' at %L",        
			    f->name, &k->u.expr->where);         
  return 0;          
}      
      
      
     
     
/* g95_match_interface()-- Match one of the five forms of an interface
 * statement. */         
         
match g95_match_interface(void) {          
char nam[G95_MAX_SYMBOL_LEN+1];        
interface_type type; 
g95_symbol *sy;  
int o;       
match i;      
      
  i = g95_match_space();  
  
  if (g95_match_generic_spec(&type, nam, &o) == MATCH_ERROR)         
    return MATCH_ERROR;      
      
  if (g95_match_eos() != MATCH_YES ||           
      (type != INTERFACE_NAMELESS && i != MATCH_YES)) {   
    g95_syntax_error(ST_INTERFACE);    
    return MATCH_ERROR;   
  } 
 
  current_interface.type = type;          
          
  switch(type) {   
  case INTERFACE_GENERIC:          
    if (g95_get_symbol(nam, NULL, &sy)) return MATCH_ERROR;     
     
    if (!sy->attr.generic && g95_add_generic(&sy->attr, NULL) == FAILURE)     
      return MATCH_ERROR;       
       
    current_interface.sym = g95_new_block = sy;    
    break;         
         
  case INTERFACE_USER_OP:   
    current_interface.uop = g95_get_uop(nam);  
    break;    
    
  case INTERFACE_INTRINSIC_OP:      
    current_interface.op = o;       
    break;         
         
  case INTERFACE_NAMELESS:   
    break;     
  } 
 
  return MATCH_YES; 
}          
          
          
        
        
/* compare_type_rank_if()-- Given two symbols that are formal
 * arguments, compare their types and rank and their formal interfaces
 * if they are both dummy procedures.  Returns nonzero if the same,
 * zero if different. */    
    
static int compare_type_rank_if(g95_symbol *q, g95_symbol *m) {         
         
  if (q->attr.flavor != FL_PROCEDURE && m->attr.flavor != FL_PROCEDURE)      
    return compare_type_rank(q, m);          
          
  if (q->attr.flavor != FL_PROCEDURE || m->attr.flavor != FL_PROCEDURE)   
    return 0;   
   
  /* At this point, both symbols are procedures */   
   
  if ((q->attr.function == 0 && q->attr.subroutine == 0) ||   
      (m->attr.function == 0 && m->attr.subroutine == 0)) return 0;          
          
  if (q->attr.function != m->attr.function || 
      q->attr.subroutine != m->attr.subroutine) return 0;       
       
  if (q->attr.function && compare_type_rank(q, m) == 0) return 0; 
 
  return compare_interfaces(q, m, 0);    /* Recurse! */        
}  
  
  
   
   
/* compare_actual_formal()-- Given formal and actual argument lists,
 * see if they are compatible.  If they are compatible, the actual
 * argument list is sorted to correspond with the formal list, and
 * elements for missing optional arguments are inserted.
 *
 * If the 'where' pointer is nonnull, then we issue errors when things
 * don't match instead of just returning the status code. */

static int compare_actual_formal(g95_actual_arglist **ap,      
				 g95_formal_arglist *frm,   
				 int is_elemental, g95_locus *where) {      
g95_actual_arglist **new, *z, *act, t;      
g95_formal_arglist *m; 
symbol_attribute attribute;    
int i, y, na;   
   
  act = *ap;

  for(z=act; z; z=z->next)      
    if (z->type != ALT_RETURN) z->type = EXPR;         
         
  if (act == NULL && frm == NULL) return 1;       
       
  y = 0; 
  for(m=frm; m; m=m->next) 
    y++;        
        
  new = (g95_actual_arglist **) alloca(y*sizeof(g95_actual_arglist *));    
    
  for(i=0; i<y; i++)         
    new[i] = NULL;     
     
  na = 0;       
  m = frm;          
  i = 0;     
     
  for(z=act; z; z=z->next, m=m->next) {          
    if (z->name[0] != '\0') {       
      i = 0;     
      for(m=frm; m; m=m->next, i++) {        
	if (m->sym == NULL) continue;         
	if (strcmp(m->sym->name, z->name) == 0) break;        
      }         
         
      if (m == NULL) {   
	if (where)     
	  g95_error("Keyword argument '%s' at %L is not in the procedure",   
		    z->name, &z->u.expr->where);    
	return 0;  
      }   
   
      if (new[i] != NULL) {     
	if (where)   
	  g95_error("Keyword argument '%s' at %L is already associated "        
		    "with another actual argument",   
		    z->name, &z->u.expr->where);     
	return 0;      
      }     
    }  
  
    if (m == NULL) {          
      if (where)
	g95_error("More actual than formal arguments in procedure call at %L",
		  where);      
      
      return 0;        
    }    
    
    if (z->type == ALT_RETURN) { 
      if (m->sym == NULL) goto match;        
        
      if (where)
	g95_error("Unexpected alternate return spec in subroutine call at %L",     
		  where);     
     
      return 0;    
    }       
       
    /* The argument is an expression */

    if (m->sym == NULL) {    
      if (where)       
	g95_error("Missing alternate return spec in subroutine call at %L",          
		  where);    
      return 0;     
    }   
   
    if (z->u.expr == NULL) goto match;    
    
    if (!compare_parameter(m->sym, z, is_elemental, where != NULL)) return 0;          
          
    /* Make sure we have a pointer if required */         
         
    attribute = g95_expr_attr(z->u.expr);      
    if (attribute.pointer || z->u.expr->type == EXPR_NULL) {    
      if (m->sym->attr.pointer) z->pointer = 1;  /* Passing a pointer */      
    } else {      
      if (m->sym->attr.pointer) {  
	if (where) g95_error("Actual argument for '%s' must be a pointer "    
			     "at %L", m->sym->name, &z->u.expr->where);      
	return 0;    
      }        
    }  
  
  match: 
    if (z == act) na = i;       
       
    new[i++] = z;   
  }  
  
  /* Make sure missing actual arguments are optional */       
       
  i = 0; 
  for(m=frm; m; m=m->next, i++) {      
    if (new[i] != NULL) continue; 
    if (!m->sym->attr.optional) {        
      if (where) g95_error("Missing actual argument for argument '%s' at %L",         
			   m->sym->name, where);      
      return 0;         
    }
  }       
       
  /* The argument lists are compatible.  We now relink a new actual
   * argument list with null arguments in the right places.  The head
   * of the list remains the head. */  
  
  for(i=0; i<y; i++)        
    if (new[i] == NULL) new[i] = g95_get_actual_arglist();          
          
  if (na != 0) {  
    t = *new[0];
    *new[0] = *act;
    *act = t;     
     
    z = new[0];     
    new[0] = new[na];          
    new[na] = z;        
  }        
        
  for(i=0; i<y-1; i++)       
    new[i]->next = new[i+1];         
         
  new[i]->next = NULL;    
    
  if (*ap == NULL && y > 0) *ap = new[0];         
         
  /* Copy types for missing arguements */

  for(z=act, m=frm; z; z=z->next, m=m->next)          
    if (z->type != ALT_RETURN && z->u.expr == NULL)          
      z->missing_arg_type = m->sym->ts.type;     
     
  return 1;  
}      
      
      
       
       
/* compare_actual_expr()-- Given two expressions from some actual
 * arguments, test whether they refer to the same expression. The
 * analysis is conservative. Returning FAILURE will produce no
 * warning. */          
          
static try compare_actual_expr(g95_expr *z, g95_expr *q){   
const g95_ref *l, *w;  
    
  if (!z || !q || 
      z->type != EXPR_VARIABLE ||        
      q->type != EXPR_VARIABLE ||      
      z->symbol != q->symbol) 
    return FAILURE;

  /* TODO improve comparison see expr.c 'show_ref' */  
  
  for(l = z->ref, w = q->ref; l && w; l = l->next, w = w->next){      
    if (l->type != w->type) return FAILURE; 
 
    switch (l->type) { 
    case REF_ARRAY:   
      if (l->u.ar.type != w->u.ar.type) return FAILURE;  
  
      /* at the moment, consider only full arrays;
       * we could do better.  */      
      if (l->u.ar.type != AR_FULL || w->u.ar.type != AR_FULL) return FAILURE;   
      break; 
 
    case REF_COMPONENT:        
      if (l->u.c.component != w->u.c.component) return FAILURE;         
      break;     
     
    case REF_SUBSTRING:    
      return FAILURE;  
  
    default:    
      g95_internal_error("compare_actual_expr(): Bad component code");   
    }      
  }          
          
  return (l == NULL && w == NULL) ? SUCCESS : FAILURE;          
}         
         
         
    
    
/* count_types_test()-- Given a pair of formal argument lists, we see
 * if the two lists can be distinguished by counting the number of
 * nonoptional arguments of a given type/rank in f1 and seeing if
 * there are less then that number of those arguments in f2 (including
 * optional arguments).  Since this test is asymmetric, it has to be
 * called twice to make it symmetric.  Returns nonzero if the argument
 * lists are incompatible by this test.  This subroutine implements
 * rule 1 of section 14.1.2.3. */       
       
static int count_types_test(g95_formal_arglist *m, g95_formal_arglist *v) {    
int retval, ac1, ac2, u, e, q, r;         
g95_formal_arglist *d;      
arginfo *argum;          
          
  r = 0;

  for(d=m; d; d=d->next)         
    r++; 
 
  /* Build an array of integers that gives the same integer to
   * arguments of the same type/rank.  */  
  
  argum = g95_getmem(r*sizeof(arginfo));        
        
  d = m; 
  for(u=0; u<r; u++, d=d->next) {    
    argum[u].flag = -1;         
    argum[u].sym = d->sym;         
  }       
       
  q = 0;          
          
  for(u=0; u<r; u++) {      
    if (argum[u].flag != -1) continue;  
  
    if (argum[u].sym->attr.optional) continue;   /* Skip optional arguments */       
       
    argum[u].flag = q;     
     
    /* Find other nonoptional arguments of the same type/rank */     
     
    for(e=u+1; e<r; e++)         
      if (!argum[e].sym->attr.optional &&   
	  compare_type_rank_if(argum[u].sym, argum[e].sym)) argum[e].flag = q;          
          
    q++;       
  }

  /* Now loop over each distinct type found in f1 */

  q = 0;      
  retval = 0;        
        
  for(u=0; u<r; u++) {        
    if (argum[u].flag != q) continue;  
  
    ac1 = 1;    
    for(e=u+1; e<r; e++)
      if (argum[e].flag == q) ac1++;          
          
    /* Count the number of arguments in f2 with that type, including
     * those that are optional. */ 
 
    ac2 = 0;       
       
    for(d=v; d; d=d->next)       
      if (compare_type_rank_if(argum[u].sym, d->sym)) ac2++;  
  
    if (ac1 > ac2) { retval = 1; break; }    
    
    q++;          
  } 
 
  g95_free(argum);   
   
  return retval;     
}  
  
  
       
       
/* check_some_aliasing()-- Given formal and actual argument lists that
 * correspond to one another, check that identical actual arguments
 * aren't not associated with some incompatible INTENTs.  */     
     
static try check_some_aliasing(g95_formal_arglist *w, g95_actual_arglist *v) { 
sym_intent f1_intent, f2_intent;        
g95_formal_arglist *d;      
g95_actual_arglist *r;      
size_t s, h, g;     
argpair *b;     
try o = SUCCESS;     
     
  s = 0;         
  for(d=w, r=v;; d=d->next, r=r->next) {          
    if (d == NULL && r == NULL) break;         
    if (d == NULL || r == NULL) 
      g95_internal_error("check_some_aliasing(): List mismatch");        
        
    s++;      
  }

  if (s == 0) return o;   
  b = (argpair*) alloca(s*sizeof(argpair));  
  
  for(h=0, d=w, r=v ; h<s; h++, d=d->next, r=r->next) {        
    b[h].f = d;        
    b[h].a = r;  
  }     
     
  qsort(b, s, sizeof(argpair), pair_cmp); 
 
  for(h=0; h<s; h++){          
    if (!b[h].a->u.expr || b[h].a->u.expr->type != EXPR_VARIABLE ||           
	b[h].a->u.expr->ts.type == BT_PROCEDURE) continue;       
       
    f1_intent = b[h].f->sym->attr.intent;

    for(g=h+1; g<s; g++) {           
      /* expected order after the sort */          
          
      if (!b[g].a->u.expr || b[g].a->u.expr->type != EXPR_VARIABLE)          
	g95_internal_error("check_some_aliasing(): corrupted data"); 
 
      /* are the expression the same ? */      
      if (compare_actual_expr(b[h].a->u.expr, b[g].a->u.expr) == FAILURE)break;         
      f2_intent = b[g].f->sym->attr.intent;    
    
      if ((f1_intent == INTENT_IN && f2_intent == INTENT_OUT) ||        
	  (f1_intent == INTENT_OUT && f2_intent == INTENT_IN)) {    
	g95_warning(101, "Same actual argument associated with INTENT(%s) "          
		    "argument '%s' and INTENT(%s) argument '%s' at %L",    
		    g95_intent_string(f1_intent), b[h].f->sym->name,    
		    g95_intent_string(f2_intent), b[g].f->sym->name,      
		    &b[h].a->u.expr->where);    
	o = FAILURE;     
      }   
    }  
  }     
           
  return o;        
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
       
       
 
 
/* g95_check_interfaces()-- For the namespace, check generic, user
 * operator and intrinsic operator interfaces for consistency and to
 * remove duplicate interfaces.  We traverse the whole namespace,
 * counting on the fact that most symbols will not have generic or
 * operator interfaces. */ 
 
void g95_check_interfaces(g95_namespace *ns) { 
g95_namespace *old_ns, *ns2; 
char interface_name[100];     
int i;    
    
  old_ns = g95_current_ns;       
  g95_current_ns = ns;   
   
  g95_traverse_ns(ns, check_sym_interfaces);        
        
  g95_traverse_user_op(ns, check_uop_interfaces); 
 
  for(i=0; i<G95_INTRINSIC_OPS; i++) {          
    if (i == INTRINSIC_USER) continue;        
        
    if (i == INTRINSIC_ASSIGN)      
      strcpy(interface_name, "intrinsic assignment operator");
    else          
      sprintf(interface_name, "intrinsic '%s' operator", g95_op2string(i));        
        
    if (check_interface0(ns->operator[i], interface_name)) continue;     
     
    check_operator_interface(ns->operator[i], i);       
       
    for(ns2=ns->parent; ns2; ns2=ns2->parent)    
      if (check_interface1(ns->operator[i], ns2->operator[i], 0,       
			   interface_name)) break;    
  } 
 
  g95_current_ns = old_ns;    
}        
        
        
  
  
/* g95_add_interface()-- Add a symbol to the current interface */        
        
try g95_add_interface(g95_symbol *n) { 
g95_interface **h, *i; 
g95_namespace *ns;        
g95_symbol *s;         
         
  switch(current_interface.type) {   
  case INTERFACE_NAMELESS:          
    return SUCCESS;        
        
  case INTERFACE_INTRINSIC_OP: 
    for(ns=current_interface.ns; ns; ns=ns->parent)    
      if (check_new_interface(ns->operator[current_interface.op], n)     
	  == FAILURE) return FAILURE;        
        
    h = &current_interface.ns->operator[current_interface.op]; 
    break;  
  
  case INTERFACE_GENERIC:      
    for(ns=current_interface.ns; ns; ns=ns->parent) {        
      g95_find_symbol(current_interface.sym->name, ns, 0, &s);  
      if (s == NULL) continue;       
       
      if (check_new_interface(s->generic, n) == FAILURE) return FAILURE;     
    }

    h = &current_interface.sym->generic;       
    break;

  case INTERFACE_USER_OP:         
    if (check_new_interface(current_interface.uop->operator, n) == FAILURE) 
      return FAILURE;          
          
    h = &current_interface.uop->operator;
    break;      
      
  default:     
    g95_internal_error("g95_add_interface(): Bad interface type"); 
  }      
      
  i = g95_get_interface(); 
  i->sym = n;       
  i->where = g95_current_locus;          
          
  i->next = *h;     
  *h = i; 
 
  return SUCCESS;
}   
   
   
       
       
/* unknown_interface()-- For a symbol without an interface, massage
 * the actual argument list.  This only has to do with marking
 * arguments which pass whole arrays. */  
  
static void unknown_interface(g95_actual_arglist *real) {        
        
  for(; real; real=real->next) {       
    if (real->type == ALT_RETURN || real->u.expr->rank == 0) continue; 
 
    real->type = FULL_ARRAY;       
  }        
}


       
       
/* compare_interfaces()-- 'Compare' two formal interfaces
 * associated with a pair of symbols.  We return nonzero if there
 * exists an actual argument list that would be ambiguous between the
 * two interfaces, zero otherwise. */      
      
static int compare_interfaces(g95_symbol *e, g95_symbol *n,      
			      int generic_flag) {        
g95_formal_arglist *a, *o;     
     
  if (e->attr.function != n->attr.function &&         
      e->attr.subroutine != n->attr.subroutine)
    return 0;   /* disagreement between function/subroutine */    
    
  a = e->formal;          
  o = n->formal;       
       
  if (a == NULL && o == NULL) return 1;   /* Special case */   
   
  if (count_types_test(a, o)) return 0;    
  if (count_types_test(o, a)) return 0;          
          
  if (generic_flag) {        
    if (generic_correspondence(a, o)) return 0;         
    if (generic_correspondence(o, a)) return 0;          
  } else { 
    if (operator_correspondence(a, o)) return 0;  
  }          
          
  return 1;   
}     
     
     
     
     
/* check_intents()-- Given formal and actual argument lists that
 * correspond to one another, check that they are compatible in the
 * sense that intents are not mismatched.  */   
   
static try check_intents(g95_formal_arglist *o, g95_actual_arglist *y) { 
sym_intent a_intent, f_intent;      
      
  for(;; o=o->next, y=y->next) {        
    if (o == NULL && y == NULL) break;       
    if (o == NULL || y == NULL) 
      g95_internal_error("check_intents(): List mismatch");          
          
    if (y->type == ALT_RETURN) continue;        
        
    if (y->u.expr == NULL || y->u.expr->type != EXPR_VARIABLE) continue;     
     
    a_intent = y->u.expr->symbol->attr.intent;          
    f_intent = o->sym->attr.intent;   
   
    if (a_intent == INTENT_IN &&     
	(f_intent == INTENT_INOUT || f_intent == INTENT_OUT)) {  
  
      g95_error("Procedure argument at %L is INTENT(IN) while interface "
		"specifies INTENT(%s)", &y->u.expr->where,        
		g95_intent_string(f_intent));        
      return FAILURE; 
    }     
     
    if (g95_pure(NULL) && g95_impure_variable(y->u.expr->symbol)) { 
      if (f_intent == INTENT_INOUT || f_intent == INTENT_OUT) {  
	g95_error("Procedure argument at %L is local to a PURE procedure and " 
		  "is passed to an INTENT(%s) argument", &y->u.expr->where,        
		  g95_intent_string(f_intent));      
	return FAILURE;
      }     
     
      if (y->u.expr->symbol->attr.pointer) { 
	g95_error("Procedure argument at %L is local to a PURE procedure and "          
		  "has the POINTER attribute", &y->u.expr->where);    
	return FAILURE;   
      }      
    }         
  }  
  
  return SUCCESS;    
}          
          
          
          
          
/* g95_search_interface()-- Given an interface pointer and an actual
 * argument list, search for a formal argument list that matches the
 * actual.  If found, returns a pointer to the symbol of the correct
 * interface.  Returns NULL if not found. */

g95_symbol *g95_search_interface(g95_interface *intr, int sub_flag,
				 g95_actual_arglist **a) {    
    
  for(; intr; intr=intr->next) {       
    if ((sub_flag && intr->sym->attr.function) ||     
	(!sub_flag && intr->sym->attr.subroutine)) continue;

    if (compare_actual_formal(a, intr->sym->formal,         
			      intr->sym->attr.elemental, NULL)) {

      check_intents(intr->sym->formal, *a);   
   
      if (g95_option.aliasing) check_some_aliasing(intr->sym->formal, *a);   
   
      return intr->sym;          
    }         
  }

  return NULL;       
}     
     
     
          
          
/* g95_procedure_use()-- Check how a procedure is used against its
 * interface.  If all goes well, the actual argument list will also
 * end up being properly sorted. */         
         
void g95_procedure_use(g95_symbol *symb, g95_actual_arglist **ap,    
		       g95_locus *where) {       
       
  if (symb->attr.if_source == IFSRC_UNKNOWN) {
    unknown_interface(*ap);      
    return;          
  }  
  
  if (!compare_actual_formal(ap, symb->formal, symb->attr.elemental, where))         
    return;       
       
  check_intents(symb->formal, *ap);        
        
  if (g95_option.aliasing) check_some_aliasing(symb->formal, *ap);   
}        
        
        
       
       
/* g95_extend_expr()-- This subroutine is called when an expression is
 * being resolved.  The expression node in question is either a user
 * defined operator or an instrinsic operator with arguments that
 * aren't compatible with the operator.  This subroutine builds an
 * actual argument list corresponding to the operands, then searches
 * for a compatible interface.  If one is found, the expression node
 * is replaced with the appropriate function call.  */     
     
try g95_extend_expr(g95_expr *l) {   
g95_actual_arglist *a;  
g95_namespace *namesp;      
g95_user_op *op;         
g95_symbol *symb;  
int b;        
        
  symb = NULL;

  a = g95_get_actual_arglist();     
     
  a->type = EXPR;
  a->u.expr = l->op1;  
  
  if (l->op2 != NULL) {     
    a->next = g95_get_actual_arglist();  
  
    a->next->type = EXPR;        
    a->next->u.expr = l->op2; 
  }     
     
  b = fold_unary(l->operator);         
         
  if (b == INTRINSIC_USER) {   
    for(namesp=g95_current_ns; namesp; namesp=namesp->parent) {         
      op = g95_find_uop(l->uop->name, namesp);    
      if (op == NULL) continue;         
         
      symb = g95_search_interface(op->operator, 0, &a);        
      if (symb != NULL) break;          
    }    
  } else {
    for(namesp=g95_current_ns; namesp; namesp=namesp->parent) {          
      symb = g95_search_interface(namesp->operator[b], 0, &a); 
      if (symb != NULL) break;         
    }          
  }         
         
  if (symb == NULL) {  /* Don't use g95_free_actual_arglist() */      
    if (a->next != NULL) g95_free(a->next);  
    g95_free(a);        
        
    return FAILURE;        
  }     
     
/* Change the expression node to a function call */ 
 
  l->type = EXPR_FUNCTION;
  l->symbol = symb;  
  l->value.function.actual = a;         
         
  if (g95_pure(NULL) && !g95_pure(symb)) {   
    g95_error("Function '%s' called in lieu of an operator at %L must be PURE",    
	      symb->name, &l->where);      
    return FAILURE;   
  }     
     
  if (g95_resolve_expr(l) == FAILURE) return FAILURE;        
        
  return SUCCESS;         
}         
         
         
