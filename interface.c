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
   If a symbol is a user-defined operator, the operator member heads up
   the list of relevant interfaces.

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

#include "g95.h"


/* The current_interface structure holds information about the
 * interface currently being parsed.  This structure is saved and
 * restored during recursive interfaces. */

g95_interface_info current_interface;


/* g95_free_interface()-- Frees a singly linked list of g95_interface
 * structures */

void g95_free_interface(g95_interface *intr) {
g95_interface *next;

  for(; intr; intr=next) {
    next = intr->next;
    g95_free(intr);
  }
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
 
  if (g95_match("% assignment ( = )") == MATCH_YES) {
    *type = INTERFACE_INTRINSIC_OP;
    *operator = INTRINSIC_ASSIGN;
    return MATCH_YES;
  }

  if (g95_match("% operator ( %o )", &i) == MATCH_YES) { /* Operator i/f */
    *type = INTERFACE_INTRINSIC_OP;
    *operator = i;
    return MATCH_YES;
  }

  if (g95_match("% operator ( ") == MATCH_YES) {
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
  g95_error("Syntax error in generic specification at %C");
  return MATCH_ERROR;
}


/* g95_match_interface()-- Match one of the five forms of an interface
 * statement. */

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

    if (sym->attr.flavor != FL_GENERIC &&
	g95_add_flavor(&sym->attr, FL_GENERIC, NULL) == FAILURE)
      return MATCH_ERROR;

    current_interface.sym = g95_new_block = sym;
    break;

  case INTERFACE_USER_OP:
    if (g95_get_symbol(name, NULL, 0, &sym)) return MATCH_ERROR;

    current_interface.sym = sym;
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
	strcmp(current_interface.sym->name, name) != 0) {
      g95_error("Expecting 'END INTERFACE OPERATOR (.%s.)' at %C",
		current_interface.sym->name);
      m = MATCH_ERROR;
    }

    break;

  case INTERFACE_GENERIC:
    if (type != current_interface.type ||
	strcmp(current_interface.sym->name, name) != 0) {
      g95_error("Expecting 'END INTERFACE %s' at %C",
		current_interface.sym->name);
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
 * correct number of arguments.  The 'base' pointer points to the
 * first symbol node in the list of operators (which might be NULL) */

try g95_check_interface(g95_interface *base, g95_symbol *new) {
g95_formal_arglist *p, *arg1, *arg2;
g95_interface *ip;
bt t1, t2;
int args;

  for(ip=base; ip; ip=ip->next) {
    if (ip->sym == new) {
      g95_error("Entity '%s' at %C is already present in the interface",
		new->name);
      return FAILURE;
    }

    if (new->formal == NULL) continue;

    if (g95_compare_formal_arglist(new->formal, ip->sym->formal)==MATCH_YES) {
      g95_error("Interface ending at %C is the same as the interface at %L",
		&ip->where);

      return FAILURE;
    }
  }

  args = 0;
  t1 = BT_UNKNOWN;
  t2 = BT_UNKNOWN;

  for(p=new->formal; p; p=p->next) {
    if (args == 0) { arg1 = p;  t1 = p->sym->ts.type; }
    if (args == 1) { arg2 = p;  t2 = p->sym->ts.type; }

    args++;
  }

/* All kinds of checking needs to be done on intrinsic operator interfaces */

  if (current_interface.type == INTERFACE_INTRINSIC_OP) {

    if (current_interface.op == INTRINSIC_ASSIGN) {
      if (!new->attr.subroutine) {
	g95_error("Assignment operator interface at %C must be a SUBROUTINE");
	return FAILURE;
      }
    } else {
      if (!new->attr.function) {
	g95_error("Intrinsic operator interface at %C must be a FUNCTION");
	return FAILURE;
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

    return SUCCESS;

  bad_repl:
    g95_error("Operator interface at %C conflicts with intrinsic interface");
    return FAILURE;

  num_args:
    g95_error("Operator interface at %C has the wrong number of arguments");
    return FAILURE;
  }

  return SUCCESS;
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

static g95_symbol *search_interface(g95_interface *intr,
				    g95_actual_arglist *actual) {

  for(; intr; intr=intr->next)
    if (g95_compare_actual_formal(actual, intr->sym->formal)) return intr->sym;

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
g95_symbol *ip, *sym;
g95_namespace *ns;
int i;

  actual = g95_get_actual_arglist();
  actual->expr = e->op1;

  if (e->op2 != NULL) {
    actual->next = g95_get_actual_arglist();
    actual->next->expr = e->op2;
  }

  i = e->operator;

  switch(i) {
  case INTRINSIC_UPLUS:   i = INTRINSIC_PLUS;     break;
  case INTRINSIC_UMINUS:  i = INTRINSIC_MINUS;    break;

  case INTRINSIC_PLUS:    case INTRINSIC_MINUS:   case INTRINSIC_TIMES: 
  case INTRINSIC_DIVIDE:  case INTRINSIC_POWER:   case INTRINSIC_CONCAT:
  case INTRINSIC_AND:     case INTRINSIC_OR:      case INTRINSIC_EQV:   
  case INTRINSIC_NEQV:    case INTRINSIC_EQ:      case INTRINSIC_NE:    
  case INTRINSIC_GT:      case INTRINSIC_GE:      case INTRINSIC_LT:    
  case INTRINSIC_LE:      case INTRINSIC_NOT:
    break;
    
  case INTRINSIC_USER:
    i = -1;
    ip = e->symbol;
    break;

  default:
    g95_internal_error("g95_extend_expr(): Bad operator");
  }
    
  if (i == -1)
    sym = search_interface(ip->operator, actual);
  else {
    for(ns=g95_current_ns; ns; ns=ns->parent) {
      sym = search_interface(ns->operator[i], actual);
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


/* g95_add_interface()-- Add a symbol to the current interface */

void g95_add_interface(g95_symbol *new) {
g95_interface **head, *intr;

  switch(current_interface.type) {
  case INTERFACE_NAMELESS:
    return;

  case INTERFACE_INTRINSIC_OP:
    head = &current_interface.ns->operator[current_interface.op];
    break;

  case INTERFACE_GENERIC:
    head = &current_interface.sym->generic;
    break;

  case INTERFACE_USER_OP:
    head = &current_interface.sym->operator;
    break;

  default:
    g95_internal_error("g95_add_interface(): Bad interface type");
  }

  if (g95_check_interface(*head, new) == SUCCESS) {
    intr = g95_get_interface();
    intr->sym = new;

    intr->next = *head;
    *head = intr;
  }
}



/* find_modproc()-- Work function for g95_parent_procedure(). */

static g95_symbol *module_procedure;
static try modproc_return;

static void find_modproc(g95_symbol *generic_sym) {
g95_interface *ip;

  if (generic_sym->attr.flavor != FL_GENERIC) return;

  for(ip=generic_sym->generic; ip; ip=ip->next)
    if (ip->sym == module_procedure) break;

  if (ip == NULL) return;

  if (module_procedure->attr.function && generic_sym->attr.function == 0 &&
      g95_add_function(&generic_sym->attr, NULL) == FAILURE)
    modproc_return = FAILURE;

  if (module_procedure->attr.subroutine && generic_sym->attr.subroutine == 0 &&
      g95_add_subroutine(&generic_sym->attr, NULL) == FAILURE)
    modproc_return = FAILURE;
}


/* g95_parent_procedure()-- Given a symbol that is the name of a
 * subroutine or a function contained in a module, make the necessary
 * changes in the module namespace.  If the symbol is a module
 * procedure, we can decide if the generic name and other specific
 * functions are subroutines or functions. */

try g95_parent_procedure(g95_symbol *sym, int sub_flag) {
g95_symbol *m;

  /* Non-internal Function or subroutine statements */

  if (g95_state_stack->state == COMP_CONTAINS &&
      g95_state_stack->previous->state == COMP_MODULE) goto ok;

  /* Entry statements */

  if ((g95_state_stack->state == COMP_SUBROUTINE ||
       g95_state_stack->state == COMP_FUNCTION) &&
      g95_state_stack->previous != NULL &&
      g95_state_stack->previous->state == COMP_CONTAINS &&
      g95_state_stack->previous->previous->state == COMP_MODULE) goto ok;

  return SUCCESS;

 ok:
  if (g95_get_symbol(sym->name, g95_current_ns->parent, 0, &m)) return FAILURE;

  if (m->attr.flavor != FL_MODULE_PROC &&
      g95_add_flavor(&m->attr, FL_MODULE_PROC, NULL) == FAILURE)
    return FAILURE;

  if (sub_flag) {
    if (g95_add_subroutine(&m->attr, NULL) == FAILURE) return FAILURE;
  } else {
    if (g95_add_function(&m->attr, NULL) == FAILURE) return FAILURE;
  }

/* At this point, we've made the module procedure a FUNCTION or a
 * SUBROUTINE.  We now need to find the generic procedure(s) that have
 * this particular procedure and update the function/subroutine bits */

  module_procedure = m;
  modproc_return = SUCCESS;

  g95_traverse_ns(g95_current_ns->parent, &find_modproc);

  return modproc_return;
}

