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
  
  
     
     
/* pair_cmp()-- qsort comparison function with the following order:
 *  - p->a->expr == NULL
 *  - p->a->expr->type != EXPR_VARIABLE
 *  - growing p->a->expr->symbol
 */    
    
static int pair_cmp(const void *q, const void *p2){ 
const g95_actual_arglist *y, *d;        
        
  /* *p1 and *p2 are elements of the to-be-sorted array */         
  y = ((const argpair *) q)->a;     
  d = ((const argpair *) p2)->a;          
          
  if (y->u.expr == NULL) return (d->u.expr == NULL) ? 0 : -1 ;   
   
  if (d->u.expr == NULL) return 1;   
   
  if (y->u.expr->type != EXPR_VARIABLE) {         
    if (d->u.expr->type != EXPR_VARIABLE) return 0;    
    return -1; 
  }        
        
  if (d->u.expr->type != EXPR_VARIABLE) return 1;       
       
  return y->u.expr->symbol < d->u.expr->symbol;          
}     
     
     


/* check_operator_interface()-- Given an operator interface and the
 * operator, make sure that all interfaces for that operator are legal. */     
     
static void check_operator_interface(g95_interface *inter, int op1) {   
g95_formal_arglist *form;      
sym_intent i, d;   
g95_symbol *sym;
bt r, o;       
int argu;       
       
  if (inter == NULL) return;     
    
  argu = 0;          
  r = o = BT_UNKNOWN;    
  i = d = INTENT_UNKNOWN;          
          
  for(form=inter->sym->formal; form; form=form->next) {  
    sym = form->sym;      
      
    if (argu == 0) { r = sym->ts.type; i = sym->attr.intent; }      
    if (argu == 1) { o = sym->ts.type; d = sym->attr.intent; }      
    argu++;      
  }   
   
  if (argu == 0 || argu > 2) goto num_args; 
 
  sym = inter->sym;      
      
  if (op1 == INTRINSIC_ASSIGN) {   
    if (!sym->attr.subroutine) {  
      g95_error("Assignment operator interface at %L must be a SUBROUTINE",
		&inter->where);      
      return;  
    }          
  } else {          
    if (!sym->attr.function) {          
      g95_error("Intrinsic operator interface at %L must be a FUNCTION",         
		&inter->where);    
      return;  
    }         
  }        
        
  switch(op1) {    
  case INTRINSIC_PLUS:     /* Numeric unary or binary */     
  case INTRINSIC_MINUS:       
    if ((argu == 1) &&   
	(r == BT_INTEGER || r == BT_REAL || r == BT_COMPLEX)) 
      goto bad_repl;    
    
    if ((argu == 2) &&    
	(r == BT_INTEGER || r == BT_REAL || r == BT_COMPLEX) &&  
	(o == BT_INTEGER || o == BT_REAL || o == BT_COMPLEX))   
      goto bad_repl;      
      
    break;     
     
  case INTRINSIC_POWER:    /* Binary numeric */  
  case INTRINSIC_TIMES:   
  case INTRINSIC_DIVIDE:          
          
  case INTRINSIC_EQ:
  case INTRINSIC_NE:    
    if (argu == 1) goto num_args;

    if ((r == BT_INTEGER || r == BT_REAL || r == BT_COMPLEX) &&        
	(o == BT_INTEGER || o == BT_REAL || o == BT_COMPLEX))      
      goto bad_repl;  
  
    break;       
       
  case INTRINSIC_GE:  /* Binary numeric operators that do not support */      
  case INTRINSIC_LE:  /* complex numbers */    
  case INTRINSIC_LT:     
  case INTRINSIC_GT:       
    if (argu == 1) goto num_args;    
    
    if ((r == BT_INTEGER || r == BT_REAL) &&
	(o == BT_INTEGER || o == BT_REAL)) goto bad_repl;     
     
    break;   
   
  case INTRINSIC_OR:       /* Binary logical */        
  case INTRINSIC_AND:     
  case INTRINSIC_EQV:   
  case INTRINSIC_NEQV: 
    if (argu == 1) goto num_args;   
    if (r == BT_LOGICAL && o == BT_LOGICAL) goto bad_repl;      
    break;          
          
  case INTRINSIC_NOT:      /* Unary logical */  
    if (argu != 1) goto num_args;     
    if (r == BT_LOGICAL) goto bad_repl;         
    break;          
          
  case INTRINSIC_CONCAT:   /* Binary string */  
    if (argu != 2) goto num_args;        
    if (r == BT_CHARACTER && o == BT_CHARACTER) goto bad_repl; 
    break;         
         
  case INTRINSIC_ASSIGN:   /* Class by itself */   
    if (argu != 2) goto num_args;    
    break;   
  }      
      
  /* Check intents on operator interfaces */      
      
  if (op1 == INTRINSIC_ASSIGN) {         
    if (i != INTENT_OUT && i != INTENT_INOUT)          
      g95_error("First argument of defined assignment at %L must be "   
		"INTENT(IN) or INTENT(INOUT)", &inter->where);     
     
    if (d != INTENT_IN)  
      g95_error("Second argument of defined assignment at %L must be "    
		"INTENT(IN)", &inter->where);       
  } else {  
    if (i != INTENT_IN)      
      g95_error("First argument of operator interface at %L must be "          
		"INTENT(IN)", &inter->where);     
     
    if (argu == 2 && d != INTENT_IN)  
      g95_error("Second argument of operator interface at %L must be "      
		"INTENT(IN)", &inter->where);   
  }        
        
  return; 
 
 bad_repl:        
  g95_error("Operator interface at %L conflicts with intrinsic interface",        
	    &inter->where);       
  return;         
         
 num_args:        
  g95_error("Operator interface at %L has the wrong number of arguments",
	    &inter->where);  
  return;         
}      
      
      
        
        
/* check_intents()-- Given formal and actual argument lists that
 * correspond to one another, check that they are compatible in the
 * sense that intents are not mismatched.  */        
        
static try check_intents(g95_formal_arglist *h, g95_actual_arglist *m) { 
sym_intent a_intent, f_intent;         
         
  for(;; h=h->next, m=m->next) {      
    if (h == NULL && m == NULL) break;     
    if (h == NULL || m == NULL) 
      g95_internal_error("check_intents(): List mismatch");   
   
    if (m->type == ALT_RETURN) continue;         
         
    if (m->u.expr == NULL || m->u.expr->type != EXPR_VARIABLE) continue;   
   
    a_intent = m->u.expr->symbol->attr.intent;   
    f_intent = h->sym->attr.intent;    
    
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
        
        


/* g95_match_interface()-- Match one of the five forms of an interface
 * statement. */       
       
match g95_match_interface(void) {    
char nam[G95_MAX_SYMBOL_LEN+1];    
interface_type type;   
g95_symbol *sym; 
int o;     
match z;      
      
  z = g95_match_space(); 
 
  if (g95_match_generic_spec(&type, nam, &o) == MATCH_ERROR)   
    return MATCH_ERROR;      
      
  if (g95_match_eos() != MATCH_YES || 
      (type != INTERFACE_NAMELESS && z != MATCH_YES)) {  
    g95_syntax_error(ST_INTERFACE);    
    return MATCH_ERROR;
  }   
   
  current_interface.type = type;     
     
  switch(type) {   
  case INTERFACE_GENERIC:          
    if (g95_get_symbol(nam, NULL, &sym)) return MATCH_ERROR;       
       
    if (!sym->attr.generic && g95_add_generic(&sym->attr, NULL) == FAILURE) 
      return MATCH_ERROR;      
      
    current_interface.sym = g95_new_block = sym;
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
 
 
   
   
/* compare_type_rank()-- Given two symbols that are formal arguments,
 * compare their ranks and types.  Returns nonzero if they have the
 * same rank and type, zero otherwise. */      
      
static int compare_type_rank(g95_symbol *k, g95_symbol *c) {    
int y, t;     
     
  y = (k->as != NULL) ? k->as->rank : 0;   
  t = (c->as != NULL) ? c->as->rank : 0;     
     
  if (y != t) return 0;   /* Ranks differ */         
         
  return g95_compare_types(&k->ts, &c->ts);    
}  
  
  
    
    
/* check_interface1()-- Check lists of interfaces to make sure that no
 * two interfaces are ambiguous.  Duplicate interfaces (from the same
 * symbol) are OK here. */ 
 
static int check_interface1(g95_interface *n, g95_interface *x,       
			    int generic_flag, char *interface_name) {

  for(; n; n=n->next)   
    for(; x; x=x->next) {      
      if (n->sym == x->sym) continue;   /* Duplicates OK here */       
       
      if (strcmp(n->sym->name, x->sym->name) == 0 &&        
	  strcmp(n->sym->module, x->sym->module) == 0) continue;

      if (compare_interfaces(n->sym, x->sym, generic_flag)) {          
	g95_error("Ambiguous interfaces '%s' and '%s' in %s at %L",       
		  n->sym->name, x->sym->name, interface_name, &n->where);  
	return 1;      
      }      
    }       
       
  return 0;         
}       
       
       


static int symbol_rank(g95_symbol *s) {

  return (s->as == NULL) ? 0 : s->as->rank;     
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
         
static int operator_correspondence(g95_formal_arglist *w,
				   g95_formal_arglist *g) {        
  for(;;) {      
    if (w == NULL && g == NULL) break; 
    if (w == NULL || g == NULL) return 1;  
  
    if (!compare_type_rank(w->sym, g->sym)) return 1;  
  
    w = w->next;      
    g = g->next;
  } 
 
  return 0;     
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
    
    
     
     
/* compare_parameter()-- Given a symbol of a formal argument list and
 * an expression, see if the two are compatible as arguments.  Returns
 * nonzero if compatible, zero if not compatible. */         
         
static int compare_parameter(g95_symbol *formal, g95_actual_arglist *c,        
			     int is_elemental, int error_flag) {    
g95_expr *real;
int formal_rank;        
g95_ref *re;   
   
  real = c->u.expr;

  if (real->ts.type == BT_PROCEDURE) {        
    if (formal->attr.flavor != FL_PROCEDURE) {      
      if (error_flag)      
	g95_error("Actual parameter '%s' at %L must be a PROCEDURE",     
		  formal->name, &c->u.expr->where); 
 
      return 0;   
    }         
         
    if (formal->attr.function &&     
	!compare_type_rank(formal, real->symbol->result)) return 0;         
         
    if (formal->attr.if_source == IFSRC_UNKNOWN) return 1;  /* Assume match */          
          
    return compare_interfaces(formal, real->symbol, 0);          
  }    
    
  if (real->type != EXPR_NULL &&        
      !g95_compare_types(&formal->ts, &real->ts)) {         
    if (error_flag) g95_error("Type mismatch in parameter '%s' at %L",
			      formal->name, &c->u.expr->where);       
    return 0;     
  }     
     
  formal_rank = symbol_rank(formal);   
   
  /* Scalar to scalar */  
  
  if (formal_rank == 0 && real->rank == 0) return 1;  
  
  /* Array to array */     
     
  if (formal_rank > 0 && real->rank > 0) {     
    if (formal_rank != real->rank &&    
	(formal->as->type == AS_ASSUMED_SHAPE || 
	 formal->as->type == AS_DEFERRED)) { 
 
      if (error_flag) g95_error("Rank mismatch for assumed-shape array in "      
				"parameter '%s' at %L", formal->name,
				&c->u.expr->where);        
      return 0;       
    }   
   
    c->type = (formal->as->type == AS_ASSUMED_SHAPE ||    
	       formal->as->type == AS_DEFERRED) ? ARRAY_DESC : FULL_ARRAY;       
    return 1;
  }         
         
  /* Array to scalar.  The reference must be elemental. */          
          
  if (formal_rank == 0 && real->rank > 0) {
    if (is_elemental) return 1;   
   
    if (error_flag) g95_error("Cannot pass array to scalar parameter "
			      "'%s' at %L", formal->name, &c->u.expr->where);    
    return 0;   
  }   
   
  /* Scalar to array.  The array cannot be assumed-shape and the final
   * reference of the actual argument must be an array element. */     
     
  if (formal->as->type == AS_ASSUMED_SHAPE ||    
      formal->as->type == AS_DEFERRED) goto error;   
   
  re = real->ref; 
  if (re == NULL) goto error;        
        
  while(re->next)       
    re = re->next;         
         
  if (re->type == REF_ARRAY && re->u.ar.type == AR_ELEMENT) {     
    c->type = ARRAY_ELEMENT;    
    return 1;  
  }          
          
error:
  if (error_flag) g95_error("Cannot pass scalar to array parameter '%s' at %L",    
			    formal->name, &c->u.expr->where);
  return 0;    
}     
     
     
      
      
/* compare_actual_expr()-- Given two expressions from some actual
 * arguments, test whether they refer to the same expression. The
 * analysis is conservative. Returning FAILURE will produce no
 * warning. */  
  
static try compare_actual_expr(g95_expr *m, g95_expr *s){        
const g95_ref *o, *j;    
      
  if (!m || !s ||
      m->type != EXPR_VARIABLE || 
      s->type != EXPR_VARIABLE ||
      m->symbol != s->symbol)
    return FAILURE;   
   
  /* TODO improve comparison see expr.c 'show_ref' */      
      
  for(o = m->ref, j = s->ref; o && j; o = o->next, j = j->next){   
    if (o->type != j->type) return FAILURE;

    switch (o->type) {         
    case REF_ARRAY:  
      if (o->u.ar.type != j->u.ar.type) return FAILURE;          
          
      /* at the moment, consider only full arrays;
       * we could do better.  */   
      if (o->u.ar.type != AR_FULL || j->u.ar.type != AR_FULL) return FAILURE;          
      break;          
          
    case REF_COMPONENT:      
      if (o->u.c.component != j->u.c.component) return FAILURE;        
      break;      
      
    case REF_SUBSTRING:        
      return FAILURE;     
     
    default: 
      g95_internal_error("compare_actual_expr(): Bad component code");        
    }          
  }         
         
  return (o == NULL && j == NULL) ? SUCCESS : FAILURE;      
}       
       
       
  
  
/* g95_free_formal_arglist()-- Gets rid of a formal argument list.  We
 * do not free symbols.  Symbols are freed when a namespace is freed. */          
          
void g95_free_formal_arglist(g95_formal_arglist *z) {    
g95_formal_arglist *s;

  for(; z; z=s) {         
    s = z->next;          
    g95_free(z); 
  }        
}       
  
  
/* compare_type_rank_if()-- Given two symbols that are formal
 * arguments, compare their types and rank and their formal interfaces
 * if they are both dummy procedures.  Returns nonzero if the same,
 * zero if different. */  
  
static int compare_type_rank_if(g95_symbol *b, g95_symbol *n) {       
       
  if (b->attr.flavor != FL_PROCEDURE && n->attr.flavor != FL_PROCEDURE)          
    return compare_type_rank(b, n);          
          
  if (b->attr.flavor != FL_PROCEDURE || n->attr.flavor != FL_PROCEDURE)       
    return 0;       
       
  /* At this point, both symbols are procedures */     
     
  if ((b->attr.function == 0 && b->attr.subroutine == 0) ||          
      (n->attr.function == 0 && n->attr.subroutine == 0)) return 0;       
       
  if (b->attr.function != n->attr.function ||
      b->attr.subroutine != n->attr.subroutine) return 0;     
     
  if (b->attr.function && compare_type_rank(b, n) == 0) return 0;      
      
  return compare_interfaces(b, n, 0);    /* Recurse! */     
}        
        
        
         
         
/* check_some_aliasing()-- Given formal and actual argument lists that
 * correspond to one another, check that identical actual arguments
 * aren't not associated with some incompatible INTENTs.  */      
      
static try check_some_aliasing(g95_formal_arglist *y, g95_actual_arglist *r) {      
sym_intent f1_intent, f2_intent;    
g95_formal_arglist *d;        
g95_actual_arglist *b;    
size_t o, q, m;         
argpair *c;   
try w = SUCCESS;     
     
  o = 0;          
  for(d=y, b=r;; d=d->next, b=b->next) {         
    if (d == NULL && b == NULL) break;         
    if (d == NULL || b == NULL)  
      g95_internal_error("check_some_aliasing(): List mismatch");       
       
    o++;      
  }   
   
  if (o == 0) return w;  
  c = (argpair*) alloca(o*sizeof(argpair));         
         
  for(q=0, d=y, b=r ; q<o; q++, d=d->next, b=b->next) {         
    c[q].f = d;        
    c[q].a = b;     
  }

  qsort(c, o, sizeof(argpair), pair_cmp);         
         
  for(q=0; q<o; q++){ 
    if (!c[q].a->u.expr || c[q].a->u.expr->type != EXPR_VARIABLE ||      
	c[q].a->u.expr->ts.type == BT_PROCEDURE) continue;   
   
    f1_intent = c[q].f->sym->attr.intent;        
        
    for(m=q+1; m<o; m++) {     
      /* expected order after the sort */        
        
      if (!c[m].a->u.expr || c[m].a->u.expr->type != EXPR_VARIABLE)          
	g95_internal_error("check_some_aliasing(): corrupted data");   
   
      /* are the expression the same ? */    
      if (compare_actual_expr(c[q].a->u.expr, c[m].a->u.expr) == FAILURE)break;          
      f2_intent = c[m].f->sym->attr.intent;        
        
      if ((f1_intent == INTENT_IN && f2_intent == INTENT_OUT) ||    
	  (f1_intent == INTENT_OUT && f2_intent == INTENT_IN)) {    
	g95_warning("Same actual argument associated with INTENT(%s) "   
		    "argument '%s' and INTENT(%s) argument '%s' at %L",        
		    g95_intent_string(f1_intent), c[q].f->sym->name,      
		    g95_intent_string(f2_intent), c[m].f->sym->name,     
		    &c[q].a->u.expr->where);         
	w = FAILURE;
      }          
    }        
  }         
               
  return w; 
}      
      
      
  
  
/* find_keyword_arg()-- Given a formal argument list and a keyword
 * name, search the list for that keyword.  Returns the correct symbol
 * node if found, NULL if not found. */   
   
static g95_symbol *find_keyword_arg(char *name0, g95_formal_arglist *u) {        
        
  for(; u; u=u->next) 
    if (strcmp(u->sym->name, name0) == 0) return u->sym;   
   
  return NULL;    
} 
 
 
   
   
/* g95_free_interface()-- Frees a singly linked list of g95_interface
 * structures */         
         
void g95_free_interface(g95_interface *intr) {        
g95_interface *next1;      
      
  for(; intr; intr=next1) {  
    next1 = intr->next;        
    g95_free(intr);   
  }       
}  
  
  
        
        
/* g95_match_generic_spec()-- Match a generic specification.
 * Depending on which type of interface is found, the 'name' or
 * 'operator' pointers may be set.  This subroutine doesn't return
 * MATCH_NO. */       
       
match g95_match_generic_spec(interface_type *type, char *nam,        
			     int *oper) {    
char b[G95_MAX_SYMBOL_LEN+1];   
match m; 
int o;      
       
  if (g95_match(" assignment ( = )") == MATCH_YES) {      
    *type = INTERFACE_INTRINSIC_OP;          
    *oper = INTRINSIC_ASSIGN;
    return MATCH_YES;       
  }         
         
  if (g95_match(" operator ( %o )", &o) == MATCH_YES) { /* Operator i/f */   
    *type = INTERFACE_INTRINSIC_OP; 
    *oper = fold_unary(o);       
    return MATCH_YES;       
  }          
          
  if (g95_match(" operator ( ") == MATCH_YES) {    
    m = g95_match_defined_op_name(b, 1);   
    if (m == MATCH_NO) goto syntax;       
    if (m != MATCH_YES) return MATCH_ERROR;  
  
    m = g95_match_char(')'); 
    if (m == MATCH_NO) goto syntax;   
    if (m != MATCH_YES) return MATCH_ERROR;    
    
    strcpy(nam, b);          
    *type = INTERFACE_USER_OP;          
    return MATCH_YES;          
  }      
      
  if (g95_match_name(b) == MATCH_YES) {   
    strcpy(nam, b);   
    *type = INTERFACE_GENERIC;         
    return MATCH_YES;   
  }  
  
  *type = INTERFACE_NAMELESS;          
  return MATCH_YES;         
         
syntax:     
  g95_error("Syntax error in generic specification at %C");     
  return MATCH_ERROR; 
}          
          
          
 
 
/* unknown_interface()-- For a symbol without an interface, massage
 * the actual argument list.  This only has to do with marking
 * arguments which pass whole arrays. */   
   
static void unknown_interface(g95_actual_arglist *a) {        
        
  for(; a; a=a->next) {     
    if (a->type == ALT_RETURN || a->u.expr->rank == 0) continue;        
        
    a->type = FULL_ARRAY;      
  }        
}      
      
      


/* check_interface0()-- Given a pointer to an interface pointer,
 * remove duplicate interfaces and make sure that all symbols are
 * either functions or subroutines.  Returns nonzero if something goes
 * wrong. */   
   
static int check_interface0(g95_interface *d, char *interface_name) {          
g95_interface *w, *qlast;   
   
  /* Make sure all symbols in the interface have been defined as
   * functions or subroutines. */      
      
  for(; d; d=d->next)    
    if (!d->sym->attr.function && !d->sym->attr.subroutine) {        
      g95_error("Procedure '%s' in %s at %L is neither function nor "    
		"subroutine", d->sym->name, interface_name, 
		&d->sym->declared_at);
      return 1;   
    }        
        
  /* Remove duplicate interfaces in this interface list */       
       
  for(; d; d=d->next) { 
    qlast = d;

    for(w=d->next; w;) {        
      if (d->sym != w->sym) {   
	qlast = w;  
	w = w->next;     
     
      } else {           /* Duplicate interface */     
	qlast->next = w->next;        
	g95_free(w);     
	w = qlast->next;
      }       
    }   
  } 
 
  return 0;   
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
          
          
    
    
static void check_uop_interfaces(g95_user_op *op) {
char interface_name[100];       
g95_user_op *uop2;      
g95_namespace *names;     
     
  sprintf(interface_name, "operator interface '%s'", op->name);        
  if (check_interface0(op->operator, interface_name)) return;          
          
  for(names=g95_current_ns; names; names=names->parent) {
    uop2 = g95_find_uop(op->name, names);        
    if (uop2 == NULL) continue;      
      
    check_interface1(op->operator, uop2->operator, 0, interface_name);     
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
				 g95_formal_arglist *form,      
				 int is_elemental, locus *where) {
g95_actual_arglist **old, *v, *act, tmp;      
g95_formal_arglist *z;         
symbol_attribute attr;
int k, l, na;  
  
  act = *ap;         
         
  for(v=act; v; v=v->next)    
    if (v->type != ALT_RETURN) v->type = EXPR;

  if (act == NULL && form == NULL) return 1;       
       
  l = 0;       
  for(z=form; z; z=z->next)   
    l++;

  old = (g95_actual_arglist **) alloca(l*sizeof(g95_actual_arglist *));       
       
  for(k=0; k<l; k++)  
    old[k] = NULL;          
          
  na = 0;         
  z = form;        
  k = 0;     
     
  for(v=act; v; v=v->next, z=z->next) {
    if (v->name[0] != '\0') {         
      k = 0;         
      for(z=form; z; z=z->next, k++) {      
	if (z->sym == NULL) continue;
	if (strcmp(z->sym->name, v->name) == 0) break;  
      }         
         
      if (z == NULL) {   
	if (where)      
	  g95_error("Keyword argument '%s' at %L is not in the procedure",    
		    v->name, &v->u.expr->where);        
	return 0;       
      }   
   
      if (old[k] != NULL) {
	if (where)        
	  g95_error("Keyword argument '%s' at %L is already associated "       
		    "with another actual argument",   
		    v->name, &v->u.expr->where);    
	return 0;        
      }
    }         
         
    if (z == NULL) {      
      if (where)   
	g95_error("More actual than formal arguments in procedure call at %L",      
		  where);        
        
      return 0;  
    }          
          
    if (v->type == ALT_RETURN) {          
      if (z->sym == NULL) goto match;

      if (where) 
	g95_error("Unexpected alternate return spec in subroutine call at %L",      
		  where);     
     
      return 0; 
    }        
        
    /* The argument is an expression */  
  
    if (z->sym == NULL) {      
      if (where)  
	g95_error("Missing alternate return spec in subroutine call at %L", 
		  where);          
      return 0;      
    }      
      
    if (v->u.expr == NULL) goto match;     
     
    if (!compare_parameter(z->sym, v, is_elemental, where != NULL)) return 0;  
  
    /* Make sure we have a pointer if required */ 
 
    attr = g95_expr_attr(v->u.expr); 
    if (attr.pointer || v->u.expr->type == EXPR_NULL) {  
      if (z->sym->attr.pointer) v->pointer = 1;  /* Passing a pointer */    
    } else {
      if (z->sym->attr.pointer) {  
	if (where) g95_error("Actual argument for '%s' must be a pointer " 
			     "at %L", z->sym->name, &v->u.expr->where);       
	return 0;   
      } 
    }  
  
  match:         
    if (v == act) na = k;  
  
    old[k++] = v;          
  }    
    
  /* Make sure missing actual arguments are optional */        
        
  k = 0;       
  for(z=form; z; z=z->next, k++) {  
    if (old[k] != NULL) continue;       
    if (!z->sym->attr.optional) {         
      if (where) g95_error("Missing actual argument for argument '%s' at %L",   
			   z->sym->name, where);  
      return 0;         
    }          
  }    
    
  /* The argument lists are compatible.  We now relink a new actual
   * argument list with null arguments in the right places.  The head
   * of the list remains the head. */

  for(k=0; k<l; k++)    
    if (old[k] == NULL) old[k] = g95_get_actual_arglist();        
        
  if (na != 0) {         
    tmp = *old[0];        
    *old[0] = *act;
    *act = tmp;         
         
    v = old[0];         
    old[0] = old[na];         
    old[na] = v;  
  }        
        
  for(k=0; k<l-1; k++)
    old[k]->next = old[k+1];         
         
  old[k]->next = NULL;       
       
  if (*ap == NULL && l > 0) *ap = old[0];    
    
  /* Copy types for missing arguements */ 
 
  for(v=act, z=form; v; v=v->next, z=z->next)      
    if (v->type != ALT_RETURN && v->u.expr == NULL) 
      v->missing_arg_type = z->sym->ts.type; 
 
  return 1;          
} 
 
 
     
     
/* g95_extend_assign()-- Tries to replace an assignment code node with
 * a subroutine call to the subroutine associated with the assignment
 * operator.  Return SUCCESS if the node was replaced.  On FAILURE, no
 * error is generated. */         
         
try g95_extend_assign(g95_code *l, g95_namespace *namesp) {     
g95_actual_arglist *act;
g95_expr *left, *right;     
g95_symbol *sy;  
  
  left = l->expr;  
  right = l->expr2;     
     
  /* Don't allow an intrinsic assignment to be replaced */      
      
  if (left->ts.type != BT_DERIVED && right->ts.type != BT_DERIVED && 
      (left->ts.type == right->ts.type ||        
       (g95_numeric_ts(&left->ts) && g95_numeric_ts(&right->ts)))) return FAILURE;      
      
  act = g95_get_actual_arglist();  
  act->type = EXPR;     
  act->u.expr = left;     
     
  act->next = g95_get_actual_arglist();
  act->next->type = EXPR;       
  act->next->u.expr = right;     
     
  sy = NULL;         
         
  for(; namesp; namesp=namesp->parent) { 
    sy = g95_search_interface(namesp->operator[INTRINSIC_ASSIGN], 1, &act);          
    if (sy != NULL) break;         
  }  
  
  if (sy == NULL) {        
    g95_free(act->next);     
    g95_free(act);      
    return FAILURE;  
  }      
      
  /* Replace the assignment with the call */

  l->type = EXEC_CALL;      
  l->sym = sy;    
  l->expr = NULL;       
  l->expr2 = NULL;
  l->ext.actual = act;     
     
  if (g95_pure(NULL) && !g95_pure(sy)) { 
    g95_error("Subroutine '%s' called in lieu of assignment at %L must be " 
	      "PURE", sy->name, &l->where);   
    return FAILURE;          
  }       
       
  return SUCCESS;        
}       
       
       
  
  
/* count_types_test()-- Given a pair of formal argument lists, we see
 * if the two lists can be distinguished by counting the number of
 * nonoptional arguments of a given type/rank in f1 and seeing if
 * there are less then that number of those arguments in f2 (including
 * optional arguments).  Since this test is asymmetric, it has to be
 * called twice to make it symmetric.  Returns nonzero if the argument
 * lists are incompatible by this test.  This subroutine implements
 * rule 1 of section 14.1.2.3. */     
     
static int count_types_test(g95_formal_arglist *g, g95_formal_arglist *h) {      
int rv, ac1, ac2, a, u, c, r;     
g95_formal_arglist *o;         
arginfo *args; 
 
  r = 0;      
      
  for(o=g; o; o=o->next)  
    r++;

  /* Build an array of integers that gives the same integer to
   * arguments of the same type/rank.  */      
      
  args = g95_getmem(r*sizeof(arginfo));  
  
  o = g;  
  for(a=0; a<r; a++, o=o->next) {     
    args[a].flag = -1;    
    args[a].sym = o->sym;     
  }  
  
  c = 0; 
 
  for(a=0; a<r; a++) {    
    if (args[a].flag != -1) continue; 
 
    if (args[a].sym->attr.optional) continue;   /* Skip optional arguments */    
    
    args[a].flag = c;  
  
    /* Find other nonoptional arguments of the same type/rank */          
          
    for(u=a+1; u<r; u++)        
      if (!args[u].sym->attr.optional &&      
	  compare_type_rank_if(args[a].sym, args[u].sym)) args[u].flag = c;   
   
    c++;
  }       
       
  /* Now loop over each distinct type found in f1 */ 
 
  c = 0;      
  rv = 0;          
          
  for(a=0; a<r; a++) {       
    if (args[a].flag != c) continue;     
     
    ac1 = 1;          
    for(u=a+1; u<r; u++)    
      if (args[u].flag == c) ac1++;          
          
    /* Count the number of arguments in f2 with that type, including
     * those that are optional. */          
          
    ac2 = 0;      
      
    for(o=h; o; o=o->next)        
      if (compare_type_rank_if(args[a].sym, o->sym)) ac2++;          
          
    if (ac1 > ac2) { rv = 1; break; }      
      
    c++;         
  }  
  
  g95_free(args);   
   
  return rv;   
}      
      
      
          
          
/* g95_procedure_use()-- Check how a procedure is used against its
 * interface.  If all goes well, the actual argument list will also
 * end up being properly sorted. */ 
 
void g95_procedure_use(g95_symbol *symb, g95_actual_arglist **a, locus *w){        
        
  if (symb->attr.if_source == IFSRC_UNKNOWN) {     
    unknown_interface(*a);       
    return;       
  }          
          
  if (!compare_actual_formal(a, symb->formal, symb->attr.elemental, w))       
    return;         
         
  check_intents(symb->formal, *a);    
    
  if (g95_option.aliasing) check_some_aliasing(symb->formal, *a);        
}      
      
      
  
  
/* g95_extend_expr()-- This subroutine is called when an expression is
 * being resolved.  The expression node in question is either a user
 * defined operator or an instrinsic operator with arguments that
 * aren't compatible with the operator.  This subroutine builds an
 * actual argument list corresponding to the operands, then searches
 * for a compatible interface.  If one is found, the expression node
 * is replaced with the appropriate function call.  */      
      
try g95_extend_expr(g95_expr *q) {
g95_actual_arglist *real;   
g95_namespace *namesp;  
g95_user_op *uop;
g95_symbol *sym;      
int m;    
    
  sym = NULL;      
      
  real = g95_get_actual_arglist();

  real->type = EXPR; 
  real->u.expr = q->op1;  
  
  if (q->op2 != NULL) {    
    real->next = g95_get_actual_arglist();  
  
    real->next->type = EXPR;   
    real->next->u.expr = q->op2;      
  } 
 
  m = fold_unary(q->operator);  
  
  if (m == INTRINSIC_USER) {       
    for(namesp=g95_current_ns; namesp; namesp=namesp->parent) {        
      uop = g95_find_uop(q->uop->name, namesp);    
      if (uop == NULL) continue;

      sym = g95_search_interface(uop->operator, 0, &real);         
      if (sym != NULL) break;    
    }  
  } else { 
    for(namesp=g95_current_ns; namesp; namesp=namesp->parent) {      
      sym = g95_search_interface(namesp->operator[m], 0, &real);          
      if (sym != NULL) break;     
    }     
  }    
    
  if (sym == NULL) {  /* Don't use g95_free_actual_arglist() */      
    if (real->next != NULL) g95_free(real->next);  
    g95_free(real);       
       
    return FAILURE;       
  }     
     
/* Change the expression node to a function call */  
  
  q->type = EXPR_FUNCTION;        
  q->symbol = sym;        
  q->value.function.actual = real;

  if (g95_pure(NULL) && !g95_pure(sym)) {         
    g95_error("Function '%s' called in lieu of an operator at %L must be PURE",  
	      sym->name, &q->where);        
    return FAILURE;         
  } 
 
  if (g95_resolve_expr(q) == FAILURE) return FAILURE;  
  
  return SUCCESS; 
}   
   
   
    
    
/* check_sym_interfaces()-- Check the generic and operator interfaces of
 * symbols to make sure that none of the interfaces conflict.  The
 * check has to be done after all of the symbols are actually loaded. */         
         
static void check_sym_interfaces(g95_symbol *symb) {        
char interface_name[100];  
g95_symbol *b;         
         
  if (symb->ns != g95_current_ns) return;          
         
  if (symb->generic != NULL) {      
    sprintf(interface_name, "generic interface '%s'", symb->name);        
    if (check_interface0(symb->generic, interface_name)) return;  
  
    b = symb; 
    while(b != NULL) {      
      if (check_interface1(symb->generic, b->generic, 1, interface_name))      
	return;          
          
      if (b->ns->parent == NULL) break;       
      if (g95_find_symbol(symb->name, b->ns->parent, 1, &b)) break;  
    }          
  }         
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
 
static int generic_correspondence(g95_formal_arglist *p,   
				  g95_formal_arglist *n) {   
   
g95_formal_arglist *f2_save, *z;   
g95_symbol *symb;         
         
  f2_save = n; 
 
  while(p) {          
    if (p->sym->attr.optional) goto nxt;       
       
    if (n != NULL && compare_type_rank(p->sym, n->sym)) goto nxt;      
      
    /* Now search for a disambiguating keyword argument starting at
     * the current non-match. */ 
 
    for(z=p; z; z=z->next) { 
      if (z->sym->attr.optional) continue;         
         
      symb = find_keyword_arg(z->sym->name, f2_save);  
      if (symb == NULL || !compare_type_rank(z->sym, symb)) return 1;      
    }         
         
  nxt:    
    p = p->next;        
    if (n != NULL) n = n->next;         
  }    
    
  return 0;    
}        
        
        
     
     
/* check_new_interface()-- Make sure that the interface just parsed is
 * not already present in the given interface list.  Ambiguity isn't
 * checked yet since module procedures can be present without
 * interfaces.  */

static try check_new_interface(g95_interface *b, g95_symbol *old) {          
g95_interface *ip;          
          
  for(ip=b; ip; ip=ip->next) {        
    if (ip->sym == old) {         
      g95_error("Entity '%s' at %C is already present in the interface",
		old->name);      
      return FAILURE;  
    }      
  }   
   
  return SUCCESS;         
}          
          
          
    
    
/* g95_match_end_interface()-- Match the different sort of
 * generic-specs that can be present after the END INTERFACE itself. */         
         
match g95_match_end_interface(void) {    
char name0[G95_MAX_SYMBOL_LEN+1];     
interface_type t;    
int op1;  
match a;          
          
  a = g95_match_space();

  if (g95_match_generic_spec(&t, name0, &op1) == MATCH_ERROR) 
    return MATCH_ERROR;   
   
  if (g95_match_eos() != MATCH_YES ||
      (t != INTERFACE_NAMELESS && a != MATCH_YES)) {          
    g95_syntax_error(ST_END_INTERFACE);  
    return MATCH_ERROR; 
  }          
          
  a = MATCH_YES;     
     
  switch(current_interface.type) { 
  case INTERFACE_NAMELESS:    
    if (t != current_interface.type) {   
      g95_error("Expected a nameless interface at %C");      
      a = MATCH_ERROR;
    }         
         
    break;   
   
  case INTERFACE_INTRINSIC_OP:     
    if (t != current_interface.type || op1 != current_interface.op) {     
     
      if (current_interface.op == INTRINSIC_ASSIGN)   
	g95_error("Expected 'END INTERFACE ASSIGNMENT (=)' at %C");  
      else   
	g95_error("Expecting 'END INTERFACE OPERATOR (%s)' at %C",          
		  g95_op2string(current_interface.op));  
  
      a = MATCH_ERROR;        
    }       
       
    break;       
       
  case INTERFACE_USER_OP:         
  /* Comparing the symbol node names is OK because only use-associated
   * symbols can be renamed */      
      
    if (t != current_interface.type ||      
	strcmp(current_interface.sym->name, name0) != 0) {        
      g95_error("Expecting 'END INTERFACE OPERATOR (.%s.)' at %C",   
		current_interface.sym->name);          
      a = MATCH_ERROR;         
    }    
    
    break;

  case INTERFACE_GENERIC:         
    if (t != current_interface.type ||   
	strcmp(current_interface.sym->name, name0) != 0) {      
      g95_error("Expecting 'END INTERFACE %s' at %C",   
		current_interface.sym->name);  
      a = MATCH_ERROR;       
    }     
     
    break;       
  }       
       
  return a;   
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
 
 
   
   
/* g95_check_interfaces()-- For the namespace, check generic, user
 * operator and intrinsic operator interfaces for consistency and to
 * remove duplicate interfaces.  We traverse the whole namespace,
 * counting on the fact that most symbols will not have generic or
 * operator interfaces. */ 
 
void g95_check_interfaces(g95_namespace *name) {      
g95_namespace *old_ns, *ns2;  
char interface_name[100];     
int s;      
      
  old_ns = g95_current_ns; 
  g95_current_ns = name;

  g95_traverse_ns(name, check_sym_interfaces);          
          
  g95_traverse_user_op(name, check_uop_interfaces);        
        
  for(s=0; s<G95_INTRINSIC_OPS; s++) {          
    if (s == INTRINSIC_USER) continue;   
   
    if (s == INTRINSIC_ASSIGN)   
      strcpy(interface_name, "intrinsic assignment operator");   
    else      
      sprintf(interface_name, "intrinsic '%s' operator", g95_op2string(s));          
          
    if (check_interface0(name->operator[s], interface_name)) continue;

    check_operator_interface(name->operator[s], s);

    for(ns2=name->parent; ns2; ns2=ns2->parent)          
      if (check_interface1(name->operator[s], ns2->operator[s], 0, 
			   interface_name)) break;         
  }   
   
  g95_current_ns = old_ns;     
}       
       
       
       
       
/* compare_interfaces()-- 'Compare' two formal interfaces
 * associated with a pair of symbols.  We return nonzero if there
 * exists an actual argument list that would be ambiguous between the
 * two interfaces, zero otherwise. */ 
 
static int compare_interfaces(g95_symbol *r, g95_symbol *a,        
			      int generic_flag) {
g95_formal_arglist *e, *j;         
         
  if (r->attr.function != a->attr.function &&     
      r->attr.subroutine != a->attr.subroutine)        
    return 0;   /* disagreement between function/subroutine */ 
 
  e = r->formal;         
  j = a->formal;      
      
  if (e == NULL && j == NULL) return 1;   /* Special case */ 
 
  if (count_types_test(e, j)) return 0;    
  if (count_types_test(j, e)) return 0;    
    
  if (generic_flag) {  
    if (generic_correspondence(e, j)) return 0;      
    if (generic_correspondence(j, e)) return 0; 
  } else {
    if (operator_correspondence(e, j)) return 0;   
  }   
   
  return 1;     
}   
   
   
   
   
/* g95_add_interface()-- Add a symbol to the current interface */          
          
try g95_add_interface(g95_symbol *new) {
g95_interface **head, *inter;  
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
         
  inter = g95_get_interface();    
  inter->sym = new;   
  inter->where = *g95_current_locus();    
    
  inter->next = *head;   
  *head = inter;   
   
  return SUCCESS; 
}        
        
        
