/* Deal with interfaces
   Copyright (C) 2000 Free Software Foundation, Inc.
   Contributed by Andy Vaught

This file is part of GNU G95.

GNU G95 is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU G95 is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU G95; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */


/* interface.c-- Deal with interfaces.  Interfaces amount to a list of
 * formal arguments much like C prototypes and are sufficiently
 * complicated that pictures are worthwhile.  An interface by itself
 * looks like:

       Interface  -->  Namespace
          |               |
          v               v
        Arg 1     -->   Symbol
          |               |
          v               v
        Arg 2     -->   Symbol

A namespace is required because we have to keep track of the names of
the actual arguments.  The namespace also contains any symbols that
are derived type names.  Interfaces are pointed to mostly by symbols,
but intrisic operators associated with namespaces can also point to
other interfaces.

Nameless interfaces:               Generic interfaces/defined operator

                                   Symbol 1 is the generic or operator 
                                   name, 2 and 3 are specific interfaces.

   Namespace                        Namespace
       |                                |
       v                                v   (via generic member)
    Symbol  <-->  Interface          Symbol1 ----------+
       |                                |              |
    Symbol  <-->  Interface             v              v 
                                     Symbol2  <-->  Interface
                                        |              |
                                        v              v
                                     Symbol3  <-->  Interface

Intrinsic operator interface

   Namespace ----------+
      |                |
      v                v
    Symbol   <-->  Interface
      |                |
      v                v
    Symbol   <-->  Interface


When a subprogram is defined, the program unit's name points to an
interface as usual, but the link to the name space is NULL and the
formal argument list points to symbols within the same namespace as
the program unit name.

*/

#include <ctype.h>
#include "g95.h"


/* The current_interface structure holds information about the
 * interface currently being parsed.  This structure is saved and
 * restored during recursive interfaces. */

g95_interface_info current_interface;


/* g95_free_interface()-- Free an interface and everything "below" it. */

void g95_free_interface(g95_interface *p) {

  if (p == NULL) return; 

  g95_free_formal_arglist(p->formal);
  g95_free_namespace(p->ns);
  g95_free(p);
}


/* g95_add_interface()-- Add an interface structure to a symbol */

void g95_add_interface(g95_symbol *sym, g95_formal_arglist *formal) {
g95_interface *interface;

  interface = sym->interface = g95_getmem(sizeof(g95_interface));

  interface->defined_at = *g95_current_locus();
  interface->formal = formal;
  interface->sym = sym;
  interface->type = INTERFACE_NAMELESS;   /* Fixed later if wrong */
}


/* g95_show_formal_arglist()-- Show a formal argument list */

void g95_show_formal_arglist(g95_formal_arglist *formal) {

  g95_status("(");

  for(; formal; formal=formal->next)
    g95_show_symbol(formal->sym);

  g95_status(")");
}



/* g95_show_interface()-- Dump an interface */

void g95_show_interface(g95_interface *ip) {
g95_formal_arglist *formal;

  g95_status("(interface");

  if (ip != NULL) {
    g95_status(" ");
    switch(ip->type) {
    case INTERFACE_NAMELESS:      g95_status("NAMELESS ");   break;
    case INTERFACE_GENERIC:       g95_status("GENERIC ");    break;
    case INTERFACE_INTRINSIC_OP:  g95_status("INTRINSIC ");  break;
    case INTERFACE_USER_OP:       g95_status("USER ");       break;
    }	    

    g95_show_namespace(ip->ns);
    g95_status("(");

    for(formal=ip->formal; formal; formal=formal->next)
      g95_status((formal==ip->formal) ? "%s" : " %s", formal->sym->name);

    g95_status(")");
  }

  g95_status(")");
}


/* g95_match_generic_spec()-- Match a generic specification.
 * Depending on which type of interface is found, the 'name' or
 * 'operator' pointers may be set.  This subroutine doesn't return
 * MATCH_NO. */

match g95_match_generic_spec(interface_type *type, char *name,
			     int *operator) {
char buffer[G95_MAX_SYMBOL_LEN+1];
match m;
int i;
 
  if (g95_match(" assignment ( = )") == MATCH_YES) {
    *type = INTERFACE_INTRINSIC_OP;
    *operator = INTRINSIC_ASSIGN;
    return MATCH_YES;
  }

  if (g95_match(" operator ( %o )", &i) == MATCH_YES) { /* Operator i/f */
    *type = INTERFACE_INTRINSIC_OP;
    *operator = i;
    return MATCH_YES;
  }

  if (g95_match(" operator ( ") == MATCH_YES) {
    m = g95_match_defined_op_name(buffer, 1);
    if (m == MATCH_NO) goto syntax;
    if (m != MATCH_YES) return MATCH_ERROR;

    m = g95_match(" )");
    if (m == MATCH_NO) goto syntax;
    if (m != MATCH_YES) return MATCH_ERROR;

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
  g95_error("Syntax error in OPERATOR specification at %C");
  return MATCH_ERROR;
}


/* g95_match_interface()-- Match one of the five forms of an interface
 * statement.  */

match g95_match_interface(void) {
char name[G95_MAX_SYMBOL_LEN+1];
interface_type type;
g95_symbol *sym;
int operator;

  if (g95_match_generic_spec(&type, name, &operator) == MATCH_ERROR)
    return MATCH_ERROR;

  if (g95_match_eos() != MATCH_YES) {
    g95_syntax_error(ST_INTERFACE);
    return MATCH_ERROR;
  }

  current_interface.type = type;

  switch(type) {
  case INTERFACE_GENERIC:
    if (g95_get_symbol(name, NULL, 0, &sym)) return MATCH_ERROR;

    if (g95_add_flavor(&sym->attr, FL_GENERIC, NULL) == FAILURE)
      return MATCH_ERROR;

    current_interface.generic = g95_new_block = sym;
    break;

  case INTERFACE_USER_OP:
    if (g95_get_symbol(name, NULL, 0, &sym)) return MATCH_ERROR;

    current_interface.generic = sym;
    break;

  case INTERFACE_INTRINSIC_OP:
    current_interface.op = operator;
    break;

  case INTERFACE_NAMELESS:
    break;
  }

  return MATCH_YES;
}


/* g95_match_end_interface()-- Match the different sort of
 * generic-specs that can be present after the END INTERFACE itself. */

match g95_match_end_interface(void) {
char name[G95_MAX_SYMBOL_LEN+1];
interface_type type;
int operator;
match m;

  if (g95_match_generic_spec(&type, name, &operator) == MATCH_ERROR)
    return MATCH_ERROR;

  if (g95_match_eos() != MATCH_YES) {
    g95_syntax_error(ST_END_INTERFACE);
    return MATCH_ERROR;
  }

  m = MATCH_YES;

  switch(current_interface.type) {
  case INTERFACE_NAMELESS:
    if (type != current_interface.type) {
      g95_error("Expected a nameless interface at %C");
      m = MATCH_ERROR;
    }

    break;

  case INTERFACE_INTRINSIC_OP:
    if (type != current_interface.type || operator != current_interface.op) {

      if (current_interface.op == INTRINSIC_ASSIGN)
	g95_error("Expected 'END INTERFACE ASSIGNMENT (=)' at %C");
      else 
	g95_error("Expecting 'END INTERFACE OPERATOR (%s)' at %C",
		  g95_op2string(current_interface.op));

      m = MATCH_ERROR;
    }

    break;

  case INTERFACE_USER_OP:
  /* Comparing the symbol node names is OK because only use-associated
   * symbols can be renamed */

    if (type != current_interface.type || 
	strcmp(current_interface.generic->name, name) != 0) {
      g95_error("Expecting 'END INTERFACE OPERATOR (.%s.)' at %C",
		current_interface.generic->name);
      m = MATCH_ERROR;
    }

    break;

  case INTERFACE_GENERIC:
    if (type != current_interface.type ||
	strcmp(current_interface.generic->name, name) != 0) {
      g95_error("Expecting 'END INTERFACE %s' at %C",
		current_interface.generic->name);
      m = MATCH_ERROR;
    }

    break;
  }

  return m;
}



/* g95_compare_types()-- Compare two typespecs, recursively if
 * necessary. */

int g95_compare_types(g95_typespec *ts1, g95_typespec *ts2) {
g95_component *dt1, *dt2;

  if (ts1->type != ts2->type) return 0;
  if (ts1->type != BT_DERIVED) return (ts1->kind == ts2->kind);

/* Compare derived types.  Both types must have the SEQUENCE attribute
 * to be equal */

  if (ts1->derived->attr.sequence == 0 || ts2->derived->attr.sequence == 0)
    return 0;

  dt1 = ts1->derived->components;
  dt2 = ts2->derived->components;

/* Since subtypes of SEQUENCE types must be SEQUENCE types as well, a
 * simple test can speed things up.  Otherwise, lots of things have to
 * match. */

  if (dt1 == dt2) return 1;

  for(;;) {
    if (strcmp(dt1->name, dt2->name) != 0) return 0;

    if (g95_compare_attr(&dt1->attr, &dt2->attr) == 0) return 0;

    if (g95_compare_array_spec(dt1->as, dt2->as) == 0) return 0;
      
    if (g95_compare_types(&dt1->ts, &dt2->ts) == 0) return 0;

    dt1 = dt1->next;
    dt2 = dt2->next;

    if (dt1 == NULL && dt2 == NULL) break;
    if (dt1 == NULL || dt1 == NULL) return 0;
  }

  return 1;
}


/* g95_compare_formal_arglist()-- Compare two formal argument lists
 * for equality. */

int g95_compare_formal_arglist(g95_formal_arglist *a1,
			       g95_formal_arglist *a2) {
g95_symbol *s1, *s2;
 
  for(;;) { 
    s1 = a1->sym;
    s2 = a2->sym;

    if (g95_compare_types(&s1->ts, &s2->ts) == 0) return 0;

    if (g95_compare_array_spec(s1->as, s2->as) == 0) return 0;

    a1 = a1->next;
    a2 = a2->next;

    if (a1 == NULL && a2 == NULL) break;
    if (a1 == NULL || a2 == NULL) return 0;
  }

  return 1;
}


/* g95_check_interface()-- Make sure that the interface just parsed
 * makes sense.  Depending on the type of interface this can mean
 * several things.  No checking is required for a nameless interface.
 * For a generic interface, the interface must be unique within the
 * block.  For an intrinsic operator interface, the interface must not
 * conflict with the intrinsic operator itself and must have the
 * correct number of arguments. */

try g95_check_interface(g95_interface *base, g95_interface *ip) {
g95_formal_arglist *p, *arg1, *arg2;
bt t1, t2;
int args;
try t;

  for(; base; base=base->next)
    if (g95_compare_formal_arglist(ip->formal, base->formal) == MATCH_YES) {
      g95_error("Interface ending at %C is the same as the interface at %L",
		&base->defined_at);

      return FAILURE;
    }

  args = 0;
  t1 = BT_UNKNOWN;
  t2 = BT_UNKNOWN;

  for(p=ip->formal; p; p=p->next) {
    if (args == 0) { arg1 = p;  t1 = p->sym->ts.type; }
    if (args == 1) { arg2 = p;  t2 = p->sym->ts.type; }

    args++;
  }

/* All kinds of checking needs to be done on intrinsic operator interfaces */

  t = SUCCESS;

  switch(current_interface.type) {
  case INTERFACE_NAMELESS:
  case INTERFACE_GENERIC:
  case INTERFACE_USER_OP:
    break;

  case INTERFACE_INTRINSIC_OP:
    if (current_interface.op == INTRINSIC_ASSIGN) {
      if (!ip->sym->attr.subroutine) {
	g95_error("Assignment operator interface at %C must be a SUBROUTINE");
	t = FAILURE;
	break;
      }
    } else {
      if (!ip->sym->attr.function) {
	g95_error("Intrinsic operator interface at %C must be a FUNCTION");
	t = FAILURE;
	break;
      }
    }

    if (args == 0 || args > 2) goto num_args;

    switch(current_interface.op) {
     
    case INTRINSIC_PLUS:     /* Numeric unary or binary */
    case INTRINSIC_MINUS: 
      if ((args == 1) &&
	  (t1 == BT_INTEGER || t1 == BT_REAL || t1 == BT_COMPLEX))
	goto bad_repl;

      if ((args == 2) &&
	  (t1 == BT_INTEGER || t1 == BT_REAL || t1 == BT_COMPLEX) &&
	  (t2 == BT_INTEGER || t2 == BT_REAL || t2 == BT_COMPLEX))
	goto bad_repl;

      break;

    case INTRINSIC_POWER:    /* Binary numeric */
    case INTRINSIC_TIMES:
    case INTRINSIC_DIVIDE:    

    case INTRINSIC_EQ:
    case INTRINSIC_NE:
      if (args == 1) goto num_args;

      if ((t1 == BT_INTEGER || t1 == BT_REAL || t1 == BT_COMPLEX) &&
	  (t2 == BT_INTEGER || t2 == BT_REAL || t2 == BT_COMPLEX))
	goto bad_repl;

      break;

    case INTRINSIC_GE:  /* Binary numeric operators that do not support */
    case INTRINSIC_LE:  /* complex numbers */
    case INTRINSIC_LT:
    case INTRINSIC_GT:
      if (args == 1) goto num_args;

      if ((t1 == BT_INTEGER || t1 == BT_REAL) &&
	  (t2 == BT_INTEGER || t2 == BT_REAL)) goto bad_repl;

      break;

    case INTRINSIC_OR:       /* Binary logical */
    case INTRINSIC_AND:
    case INTRINSIC_EQV:
    case INTRINSIC_NEQV:
      if (args == 1) goto num_args;
      if (t1 == BT_LOGICAL && t2 == BT_LOGICAL) goto bad_repl;
      break;

    case INTRINSIC_NOT:      /* Unary logical */
      if (args != 1) goto num_args;
      if (t1 == BT_LOGICAL) goto bad_repl;
      break;

    case INTRINSIC_CONCAT:   /* Binary string */
      if (args != 2) goto num_args;
      if (t1 == BT_CHARACTER) goto bad_repl;
      break;

    case INTRINSIC_ASSIGN:   /* Class by itself */
      if (args != 2) goto num_args;
      break;
    }

    break;

  bad_repl:
    g95_error("Operator interface at %C conflicts with intrinsic interface");
    t = FAILURE;
    break;

  num_args:
    g95_error("Operator interface at %C has the wrong number of arguments");
    t = FAILURE;
    break;

  }

  return t;
}


/* g95_compare_formal_actual()-- Given formal and actual argument
 * lists, see if they are compatible.  */

int g95_compare_actual_formal(g95_actual_arglist *actual,
			      g95_formal_arglist *formal) {
g95_formal_arglist *f;

  for(; actual; actual=actual->next) {
    if (actual->name[0] == '\0') { 
      if (g95_compare_types(&formal->sym->ts, &actual->expr->ts)==0) return 0;
      formal = formal->next;

    } else {  /* Search for an optional arg */

      for(f=formal; f; f=f->next)
	if (strcmp(actual->name, f->sym->name) == 0) break;

      if (f == NULL) return 0;

      if (g95_compare_types(&f->sym->ts, &actual->expr->ts) == 0) return 0;
    }
  }

  return 1;
}




/* search_interface()-- Given an interface pointer and an actual
 * argument list, search for a formal argument list that matches the
 * actual.  If found, returns a pointer to the symbol of the correct
 * interface.  Returns NULL if not found. */

g95_symbol *search_interface(g95_interface *ip, g95_actual_arglist *actual) {

  for(; ip; ip=ip->next)
    if (g95_compare_actual_formal(actual, ip->formal)) return ip->sym;

  return NULL;
}


/* g95_extend_expr()-- This subroutine is called when an expression is
 * being built.  The expression node in question is either a user
 * defined operator or an instrinsic operator with arguments that
 * aren't compatible with the operator.  This subroutine builds an
 * actual argument list corresponding to the operands, then searches
 * for a compatible interface.  If one is found, the expression node
 * is replaced with the appropriate function call.  */

try g95_extend_expr(g95_expr *e) {
g95_actual_arglist *actual;
g95_interface *ip;
g95_namespace *ns;
g95_symbol *sym;
int i;

  actual = g95_get_actual_arglist();
  actual->expr = e->op1;

  if (e->op2 != NULL) {
    actual->next = g95_get_actual_arglist();
    actual->next->expr = e->op2;
  }

  i = e->operator;

  switch(i) {
  case INTRINSIC_UPLUS:    i = INTRINSIC_PLUS;  break;
  case INTRINSIC_UMINUS:   i = INTRINSIC_MINUS;  break;

  case INTRINSIC_PLUS:    case INTRINSIC_MINUS:   case INTRINSIC_TIMES: 
  case INTRINSIC_DIVIDE:  case INTRINSIC_POWER:   case INTRINSIC_CONCAT:
  case INTRINSIC_AND:     case INTRINSIC_OR:      case INTRINSIC_EQV:   
  case INTRINSIC_NEQV:    case INTRINSIC_EQ:      case INTRINSIC_NE:    
  case INTRINSIC_GT:      case INTRINSIC_GE:      case INTRINSIC_LT:    
  case INTRINSIC_LE:      case INTRINSIC_NOT:
    break;
    
  case INTRINSIC_USER:
    i = -1;
    ip = e->symbol->interface;
    break;

  default:
    g95_internal_error("g95_extend_expr(): Bad operator");
  }
    
  if (i == -1) sym = search_interface(ip, actual);
  else {
    for(ns=g95_current_ns; ns; ns=ns->parent) {
      ip = ns->operator[i];
      sym = search_interface(ip, actual);
      if (sym != NULL) break;
    }
  }
    
  if (sym == NULL) {
    g95_free(actual->next);  /* Don't use g95_free_actual_arglist() */
    g95_free(actual);

    return FAILURE;
  }

/* Change the expression node to a function call */

  e->expr_type = EXPR_FUNCTION;
  e->symbol = sym;
  e->ts = sym->ts;
  e->value.function.actual = actual;

  return SUCCESS;
}
