/* Deal with interfaces
   Copyright (C) 2000, 2001, 2002 Free Software Foundation, Inc.
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


/* g95_free_interface()-- Frees a singly linked list of g95_interface
 * structures */

void g95_free_interface(g95_interface *intr) {
g95_interface *next;

  for(; intr; intr=next) {
    next = intr->next;
    g95_free(intr);
  }
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
    *operator = fold_unary(i);
    return MATCH_YES;
  }

  if (g95_match(" operator ( ") == MATCH_YES) {
    m = g95_match_defined_op_name(buffer, 1);
    if (m == MATCH_NO) goto syntax;
    if (m != MATCH_YES) return MATCH_ERROR;

    m = g95_match_char(')');
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
match m;

  m = g95_match_space();

  if (g95_match_generic_spec(&type, name, &operator) == MATCH_ERROR)
    return MATCH_ERROR;

  if (g95_match_eos() != MATCH_YES || 
      (type != INTERFACE_NAMELESS && m != MATCH_YES)) {
    g95_syntax_error(ST_INTERFACE);
    return MATCH_ERROR;
  }

  current_interface.type = type;

  switch(type) {
  case INTERFACE_GENERIC:
    if (g95_get_symbol(name, NULL, &sym)) return MATCH_ERROR;

    if (!sym->attr.generic && g95_add_generic(&sym->attr, NULL) == FAILURE)
      return MATCH_ERROR;

    current_interface.sym = g95_new_block = sym;
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


/* g95_match_end_interface()-- Match the different sort of
 * generic-specs that can be present after the END INTERFACE itself. */

match g95_match_end_interface(void) {
char name[G95_MAX_SYMBOL_LEN+1];
interface_type type;
int operator;
match m;

  m = g95_match_space();

  if (g95_match_generic_spec(&type, name, &operator) == MATCH_ERROR)
    return MATCH_ERROR;

  if (g95_match_eos() != MATCH_YES ||
      (type != INTERFACE_NAMELESS && m != MATCH_YES)) {
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


/* compare_type_rank()-- Given two symbols that are formal arguments,
 * compare their ranks and types.  Returns nonzero if they have the
 * same rank and type, zero otherwise. */

static int compare_type_rank(g95_symbol *s1, g95_symbol *s2) {
int r1, r2;

  r1 = (s1->as != NULL) ? s1->as->rank : 0;
  r2 = (s2->as != NULL) ? s2->as->rank : 0;

  if (r1 != r2) return 0;   /* Ranks differ */

  return g95_compare_types(&s1->ts, &s2->ts);
}


static int compare_interfaces(g95_symbol *, g95_symbol *, int);

/* compare_type_rank_if()-- Given two symbols that are formal
 * arguments, compare their types and rank and their formal interfaces
 * if they are both dummy procedures.  Returns nonzero if the same,
 * zero if different. */

static int compare_type_rank_if(g95_symbol *s1, g95_symbol *s2) {

  if (s1->attr.flavor != FL_PROCEDURE && s2->attr.flavor != FL_PROCEDURE)
    return compare_type_rank(s1, s2);

  if (s1->attr.flavor != FL_PROCEDURE || s2->attr.flavor != FL_PROCEDURE)
    return 0;

  /* At this point, both symbols are procedures */

  if ((s1->attr.function == 0 && s1->attr.subroutine == 0) ||
      (s2->attr.function == 0 && s2->attr.subroutine == 0)) return 0;

  if (s1->attr.function != s2->attr.function ||
      s1->attr.subroutine != s2->attr.subroutine) return 0;

  if (s1->attr.function && compare_type_rank(s1, s2) == 0) return 0;

  return compare_interfaces(s1, s2, 1);    /* Recurse! */
}


/* find_keyword_arg()-- Given a formal argument list and a keyword
 * name, search the list for that keyword.  Returns the correct symbol
 * node if found, NULL if not found. */

static g95_symbol *find_keyword_arg(char *name, g95_formal_arglist *f) {

  for(; f; f=f->next)
    if (strcmp(f->sym->name, name) == 0) return f->sym;

  return NULL;
}


/******** Interface checking subroutines **********/


/* check_operator_interface()-- Given an operator interface and the
 * operator, make sure that all interfaces for that operator are legal. */

static void check_operator_interface(g95_interface *intr, int operator) {
g95_formal_arglist *formal;
sym_intent i1, i2;
g95_symbol *sym;
bt t1, t2;
int args;

  if (intr == NULL) return; 

  args = 0;
  t1 = t2 = BT_UNKNOWN;
  i1 = i2 = INTENT_UNKNOWN;

  for(formal=intr->sym->formal; formal; formal=formal->next) {
    sym = formal->sym;

    if (args == 0) { t1 = sym->ts.type; i1 = sym->attr.intent; }
    if (args == 1) { t2 = sym->ts.type; i2 = sym->attr.intent; }
    args++;
  }

  if (args == 0 || args > 2) goto num_args;

  sym = intr->sym;

  if (operator == INTRINSIC_ASSIGN) {
    if (!sym->attr.subroutine) {
      g95_error("Assignment operator interface at %L must be a SUBROUTINE",
		&intr->where);
      return;
    }
  } else {
    if (!sym->attr.function) {
      g95_error("Intrinsic operator interface at %L must be a FUNCTION",
		&intr->where);
      return;
    }
  }

  switch(operator) {
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
    if (t1 == BT_CHARACTER && t2 == BT_CHARACTER) goto bad_repl;
    break;

  case INTRINSIC_ASSIGN:   /* Class by itself */
    if (args != 2) goto num_args;
    break;
  }

  /* Check intents on operator interfaces */

  if (operator == INTRINSIC_ASSIGN) {
    if (i1 != INTENT_OUT && i1 != INTENT_INOUT)
      g95_error("First argument of defined assignment at %L must be "
		"INTENT(IN) or INTENT(INOUT)", &intr->where);

    if (i2 != INTENT_IN)
      g95_error("Second argument of defined assignment at %L must be "
		"INTENT(IN)", &intr->where);
  } else {
    if (i1 != INTENT_IN)
      g95_error("First argument of operator interface at %L must be "
		"INTENT(IN)", &intr->where);

    if (args == 2 && i2 != INTENT_IN)
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


/* count_types_test()-- Given a pair of formal argument lists, we see
 * if the two lists can be distinguished by counting the number of
 * nonoptional arguments of a given type/rank in f1 and seeing if
 * there are less then that number of those arguments in f2 (including
 * optional arguments).  Since this test is asymmetric, it has to be
 * called twice to make it symmetric.  Returns nonzero if the argument
 * lists are incompatible by this test.  This subroutine implements
 * rule 1 of section 14.1.2.3. */

static int count_types_test(g95_formal_arglist *f1, g95_formal_arglist *f2) {
int rc, ac1, ac2, i, j, k, n1;
g95_formal_arglist *f;

typedef struct {
  int flag;
  g95_symbol *sym;
} arginfo;

arginfo *arg;

  n1 = 0;

  for(f=f1; f; f=f->next)
    n1++;

  /* Build an array of integers that gives the same integer to
   * arguments of the same type/rank.  */

  arg = g95_getmem(n1*sizeof(arginfo));

  f = f1;
  for(i=0; i<n1; i++, f=f->next) {
    arg[i].flag = -1;
    arg[i].sym = f->sym;
  }

  k = 0;

  for(i=0; i<n1; i++) {
    if (arg[i].flag != -1) continue;

    if (arg[i].sym->attr.optional) continue;   /* Skip optional arguments */

    arg[i].flag = k;

    /* Find other nonoptional arguments of the same type/rank */

    for(j=i+1; j<n1; j++)
      if (!arg[j].sym->attr.optional &&
	  compare_type_rank_if(arg[i].sym, arg[j].sym)) arg[j].flag = k;

    k++;
  }

  /* Now loop over each distinct type found in f1 */

  k = 0;
  rc = 0;

  for(i=0; i<n1; i++) {
    if (arg[i].flag != k) continue;

    ac1 = 1;
    for(j=i+1; j<n1; j++)
      if (arg[j].flag == k) ac1++;

    /* Count the number of arguments in f2 with that type, including
     * those that are optional. */

    ac2 = 0;

    for(f=f2; f; f=f->next)
      if (compare_type_rank_if(arg[i].sym, f->sym)) ac2++;

    if (ac1 > ac2) { rc = 1; break; }

    k++;
  }

  g95_free(arg);

  return rc;
}


/* operator_correspondence()-- Perform the abbreviated correspondence
 * test for operators.  The arguments cannot be optional and are
 * always ordered correctly, which makes this test much easier than
 * that for generic tests. */

static int operator_correspondence(g95_formal_arglist *f1,
				   g95_formal_arglist *f2) {
  for(;;) {
    if (f1 == NULL && f2 == NULL) break;
    if (f1 == NULL || f2 == NULL) return 1;

    if (!compare_type_rank(f1->sym, f2->sym)) return 1;

    f1 = f1->next;
    f2 = f2->next;
  }

  return 0;
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

static int generic_correspondence(g95_formal_arglist *f1,
				  g95_formal_arglist *f2) {

g95_formal_arglist *f2_save, *g;
g95_symbol *sym;

  f2_save = f2;

  while(f1) {
    if (f1->sym->attr.optional) goto next;

    if (f2 != NULL && compare_type_rank(f1->sym, f2->sym)) goto next;

    /* Now search for a disambiguating keyword argument starting at
     * the current non-match. */

    for(g=f1; g; g=g->next) {
      if (g->sym->attr.optional) continue;

      sym = find_keyword_arg(g->sym->name, f2_save);
      if (sym == NULL || !compare_type_rank(g->sym, sym)) return 1;
    }

  next:
    f1 = f1->next;
    if (f2 != NULL) f2 = f2->next;
  }

  return 0;
}


/* compare_interfaces()-- 'Compare' two formal interfaces
 * associated with a pair of symbols.  We return nonzero if there
 * exists an actual argument list that would be ambiguous between the
 * two interfaces, zero otherwise. */

static int compare_interfaces(g95_symbol *s1, g95_symbol *s2,
			      int generic_flag) {
g95_formal_arglist *f1, *f2;

  if (s1->attr.function != s2->attr.function &&
      s1->attr.subroutine != s2->attr.subroutine)
    return 0;   /* disagreement between function/subroutine */

  f1 = s1->formal;
  f2 = s2->formal;

  if (f1 == NULL && f2 == NULL) return 1;   /* Special case */

  if (count_types_test(f1, f2)) return 0;
  if (count_types_test(f2, f1)) return 0;

  if (generic_flag) {
    if (generic_correspondence(f1, f2)) return 0;
    if (generic_correspondence(f2, f1)) return 0;
  } else {
    if (operator_correspondence(f1, f2)) return 0;
  }

  return 1;
}


/* check_interface0()-- Given a pointer to an interface pointer,
 * remove duplicate interfaces and make sure that all symbols are
 * either functions or subroutines.  Returns nonzero if something goes
 * wrong. */

static int check_interface0(g95_interface *p, char *interface_name) {
g95_interface *q, *qlast;

  /* Make sure all symbols in the interface have been defined as
   * functions or subroutines. */

  for(; p; p=p->next)
    if (!p->sym->attr.function && !p->sym->attr.subroutine) {
      g95_error("Procedure '%s' in %s at %L is neither function nor "
		"subroutine", p->sym->name, interface_name,
		&p->sym->declared_at);
      return 1;
    }

  /* Remove duplicate interfaces in this interface list */

  for(; p; p=p->next) {
    qlast = p;

    for(q=p->next; q;) {
      if (p->sym != q->sym) {
	qlast = q;
	q = q->next;

      } else {           /* Duplicate interface */
	qlast->next = q->next;
	g95_free(q);
	q = qlast->next;
      }
    }
  }

  return 0;
}


/* check_interface1()-- Check lists of interfaces to make sure that no
 * two interfaces are ambiguous.  Duplicate interfaces (from the same
 * symbol) are OK here. */

static int check_interface1(g95_interface *p, g95_interface *q,
			    int generic_flag, char *interface_name) {

  for(; p; p=p->next)
    for(; q; q=q->next) {
      if (p->sym == q->sym) continue;   /* Duplicates OK here */

      if (strcmp(p->sym->name, q->sym->name) == 0 &&
	  strcmp(p->sym->module, q->sym->module) == 0) continue;

      if (compare_interfaces(p->sym, q->sym, generic_flag)) {
	g95_error("Ambiguous interfaces '%s' and '%s' in %s at %L",
		  p->sym->name, q->sym->name, interface_name, &p->where);
	return 1;
      }
    }

  return 0;
}


/* check_sym_interfaces()-- Check the generic and operator interfaces of
 * symbols to make sure that none of the interfaces conflict.  The
 * check has to be done after all of the symbols are actually loaded. */

static void check_sym_interfaces(g95_symbol *sym) {
char interface_name[100];
g95_symbol *s2;

  if (sym->ns != g95_current_ns) return; 

  if (sym->generic != NULL) {
    sprintf(interface_name, "generic interface '%s'", sym->name);
    if (check_interface0(sym->generic, interface_name)) return;

    s2 = sym;
    while(s2 != NULL) {
      if (check_interface1(sym->generic, s2->generic, 1, interface_name))
	return;

      if (s2->ns->parent == NULL) break;
      if (g95_find_symbol(sym->name, s2->ns->parent, 1, &s2)) break;
    }
  }
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


static int symbol_rank(g95_symbol *sym) {

  return (sym->as == NULL) ? 0 : sym->as->rank;
}


/* compare_pointer()-- Given a symbol of a formal argument list and an
 * expression, if the formal argument is a pointer, see if the actual
 * argument is a pointer. Returns nonzero if compatible, zero if not
 * compatible. */

static int compare_pointer(g95_symbol *formal, g95_expr *actual) {
symbol_attribute attr;

  if (formal->attr.pointer) {
    attr = g95_expr_attr(actual);
    if (!attr.pointer) return 0;
  }

  return 1;
}

/* compare_parameter()-- Given a symbol of a formal argument list and
 * an expression, see if the two are compatible as arguments.  Returns
 * nonzero if compatible, zero if not compatible. */

static int compare_parameter(g95_symbol *formal, g95_expr *actual,
			     int ranks_must_agree, int is_elemental) {
g95_ref *ref;

  if (actual->ts.type == BT_PROCEDURE) {
    if (formal->attr.flavor != FL_PROCEDURE) return 0;

    if (formal->attr.function &&
	!compare_type_rank(formal, actual->symbol)) return 0;

    if (formal->attr.if_source == IFSRC_UNKNOWN) return 1;  /* Assume match */

    return compare_interfaces(formal, actual->symbol, 1);
  }

  if (!g95_compare_types(&formal->ts, &actual->ts)) return 0;

  if (symbol_rank(formal) == actual->rank) return 1;

  /* At this point the ranks didn't agree. */

  if (ranks_must_agree || formal->attr.pointer) return 0;

  if (actual->rank != 0) return is_elemental || formal->attr.dimension;

  /* At this point, we are considering a scalar passed to an array.
   * This is legal if the scalar is an array element of the right sort. */

  if (formal->as->type == AS_ASSUMED_SHAPE) return 0;

  for(ref=actual->ref; ref; ref=ref->next)
    if (ref->type == REF_SUBSTRING) return 0;

  for(ref=actual->ref; ref; ref=ref->next)
    if (ref->type == REF_ARRAY && ref->u.ar.type == AR_ELEMENT) break;

  if (ref == NULL) return 0;   /* Not an array element */

  return 1;
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
				 int ranks_must_agree, int is_elemental,
				 locus *where) {
g95_actual_arglist **new, *a, *actual, temp;
g95_formal_arglist *f;
int i, n, na;

  actual = *ap;

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
		    a->name, &a->expr->where);
	return 0;
      }

      if (new[i] != NULL) {
	if (where)
	  g95_error("Keyword argument '%s' at %L is already associated "
		    "with another actual argument", a->name, &a->expr->where);
	return 0;
      }
    }

    if (f == NULL) {
      if (where)
	g95_error("More actual than formal arguments in procedure call at %L",
		  where);

      return 0;
    }

    if (f->sym == NULL && a->expr == NULL) goto match;

    if (f->sym == NULL) {
      if (where)
	g95_error("Missing alternate return spec in subroutine call at %L",
		  where);
      return 0;
    }

    if (a->expr == NULL) {
      if (where)
	g95_error("Unexpected alternate return spec in subroutine call at %L",
		  where);
      return 0;
    }

    if (!compare_parameter(f->sym, a->expr, ranks_must_agree, is_elemental)) {
      if (where) g95_error("Type/rank mismatch in argument '%s' at %L",
			   f->sym->name, &a->expr->where);
      return 0;
    }

    if (compare_pointer(f->sym, a->expr) == 0) {
      if (where) g95_error("Actual argument for '%s' must be a pointer at %L",
			   f->sym->name, &a->expr->where);
      return 0;
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

  return 1;
}

typedef struct {
  g95_formal_arglist *f;
  g95_actual_arglist *a;
} argpair;

/* pair_cmp()-- qsort comparison function with the following order:
 *  - p->a->expr == NULL
 *  - p->a->expr->expr_type != EXPR_VARIABLE
 *  - growing p->a->expr->symbol
 */
static int pair_cmp(const void *p1, const void *p2){
const g95_actual_arglist *a1, *a2;

  /* *p1 and *p2 are elements of the to-be-sorted array */
  a1 = ((const argpair *)p1)->a;
  a2 = ((const argpair *)p2)->a;
  if (!a1->expr) {
    if (!a2->expr) return 0;
    return -1;
  }
  if (!a2->expr)
    return 1;
  if (a1->expr->expr_type != EXPR_VARIABLE) {
    if (a2->expr->expr_type != EXPR_VARIABLE)
      return 0;
    return -1;
  }
  if (a2->expr->expr_type != EXPR_VARIABLE)
    return 1;
  return a1->expr->symbol < a2->expr->symbol;
}

/* compare_actual_expr()-- Given two expressions from some actual arguments,
 * test whether they refer to the same expression. The analysis is conserva-
 * tive. Returning FAILURE will produce no warning. */

static try compare_actual_expr(g95_expr *e1, g95_expr *e2){
const g95_ref *r1, *r2;
  
  if (!e1 || !e2 ||
      e1->expr_type != EXPR_VARIABLE ||
      e2->expr_type != EXPR_VARIABLE ||
      e1->symbol != e2->symbol)
    return FAILURE;

  /* TODO improve comparison see expr.c 'show_ref' */

  for(r1 = e1->ref, r2 = e2->ref; r1 && r2; r1 = r1->next, r2 = r2->next){
    if (r1->type != r2->type)
      return FAILURE;
    switch (r1->type) {
    case REF_ARRAY:
      if (r1->u.ar.type != r2->u.ar.type)
	return FAILURE;
      /* at the moment, consider only full arrays;
       * we could do better.  */
      if (r1->u.ar.type != AR_FULL ||
	  r2->u.ar.type != AR_FULL )
	return FAILURE;
      break;
    case REF_COMPONENT:
      if (r1->u.c.component != r2->u.c.component)
	return FAILURE;
      break;
    case REF_SUBSTRING:
      return FAILURE;
      break;
    default:
      g95_internal_error("compare_actual_expr(): Bad component code");
    }
  }
  if (!r1 && !r2)
    return SUCCESS;
  return FAILURE;
}

/* check_some_aliasing()-- Given formal and actual argument lists that
 * correspond to one another, check that identical actual arguments
 * aren't not associated with some incompatible INTENTs.  */

static try check_some_aliasing(g95_formal_arglist *f, g95_actual_arglist *a) {
sym_intent f1_intent, f2_intent;
g95_formal_arglist *f1;
g95_actual_arglist *a1;
size_t n, i, j;
argpair *p;
try t = SUCCESS;

  n = 0; 
  for(f1=f, a1=a;; f1=f1->next, a1=a1->next) {
    if (f1 == NULL && a1 == NULL) break;
    if (f1 == NULL || a1 == NULL)
      g95_internal_error("check_some_aliasing(): List mismatch");
    n++;
  }
  if (n == 0)
    return t;
  p = (argpair*) alloca(n*sizeof(argpair));

  for(i=0, f1=f, a1=a ; i<n; i++, f1=f1->next, a1=a1->next){
    p[i].f = f1;
    p[i].a = a1;
  }

  qsort(p, n, sizeof(argpair), pair_cmp);

  for(i=0; i<n; i++){
    if (!p[i].a->expr || 
	p[i].a->expr->expr_type != EXPR_VARIABLE || 
	p[i].a->expr->ts.type == BT_PROCEDURE)
      continue;
    f1_intent = p[i].f->sym->attr.intent;
    for(j=i+1; j<n; j++){ 
      /* expected order after the sort */
      if (!p[j].a->expr || p[j].a->expr->expr_type != EXPR_VARIABLE)
	g95_internal_error("check_some_aliasing(): corrupted data");
      /* are the expression the same ? */
      if (compare_actual_expr(p[i].a->expr, p[j].a->expr) == FAILURE)
	break;
      f2_intent = p[j].f->sym->attr.intent;
      if ((f1_intent == INTENT_IN && f2_intent == INTENT_OUT) || 
	  (f1_intent == INTENT_OUT && f2_intent == INTENT_IN)) {
	g95_warning("Same actual argument associated with INTENT(%s) "
		    "argument '%s' and INTENT(%s) argument '%s' at %L",
		    g95_intent_string(f1_intent), p[i].f->sym->name,
		    g95_intent_string(f2_intent), p[j].f->sym->name,
		    &p[i].a->expr->where);
	t = FAILURE;
      }
    }
  }
      
  return t;
}

/* check_intents()-- Given formal and actual argument lists that
 * correspond to one another, check that they are compatible in the
 * sense that intents are not mismatched.  */

static try check_intents(g95_formal_arglist *f, g95_actual_arglist *a) {
sym_intent a_intent, f_intent;

  for(;; f=f->next, a=a->next) {
    if (f == NULL && a == NULL) break;
    if (f == NULL || a == NULL)
      g95_internal_error("check_intents(): List mismatch");

    if (a->expr == NULL || a->expr->expr_type != EXPR_VARIABLE) continue;

    a_intent = a->expr->symbol->attr.intent;
    f_intent = f->sym->attr.intent;

    if (a_intent == INTENT_IN &&
	(f_intent == INTENT_INOUT || f_intent == INTENT_OUT)) {

      g95_error("Procedure argument at %L is INTENT(IN) while interface "
		"specifies INTENT(%s)", &a->expr->where,
		g95_intent_string(f_intent));
      return FAILURE;
    }

    if (g95_pure(NULL) && g95_impure_variable(a->expr->symbol)) {
      if (f_intent == INTENT_INOUT || f_intent == INTENT_OUT) {
	g95_error("Procedure argument at %L is local to a PURE procedure and "
		  "is passed to an INTENT(%s) argument", &a->expr->where,
		  g95_intent_string(f_intent));
	return FAILURE;
      }

      if (a->expr->symbol->attr.pointer) {
	g95_error("Procedure argument at %L is local to a PURE procedure and "
		  "has the POINTER attribute", &a->expr->where);
	return FAILURE;
      }
    }
  }

  return SUCCESS;
}


/* g95_procedure_use()-- Check how a procedure is used against its
 * interface.  If all goes well, the actual argument list will also
 * end up being properly sorted. */

void g95_procedure_use(g95_symbol *sym, g95_actual_arglist **ap, locus *where){

  if (sym->attr.if_source == IFSRC_UNKNOWN ||
      !compare_actual_formal(ap, sym->formal, 0, sym->attr.elemental, where))
    return;

  check_intents(sym->formal, *ap);
  if (g95_option.aliasing)
    check_some_aliasing(sym->formal, *ap);
}


/* g95_search_interface()-- Given an interface pointer and an actual
 * argument list, search for a formal argument list that matches the
 * actual.  If found, returns a pointer to the symbol of the correct
 * interface.  Returns NULL if not found. */

g95_symbol *g95_search_interface(g95_interface *intr, int sub_flag,
				 g95_actual_arglist **ap) {
int r;

  for(; intr; intr=intr->next) {
    if (sub_flag && intr->sym->attr.function) continue;
    if (!sub_flag && intr->sym->attr.subroutine) continue;

    r = !intr->sym->attr.elemental;

    if (compare_actual_formal(ap, intr->sym->formal, r, !r, NULL)) {
      check_intents(intr->sym->formal, *ap);
      if (g95_option.aliasing)
	check_some_aliasing(intr->sym->formal, *ap);
      return intr->sym;
    }
  }

  return NULL;
}


/* g95_extend_expr()-- This subroutine is called when an expression is
 * being resolved.  The expression node in question is either a user
 * defined operator or an instrinsic operator with arguments that
 * aren't compatible with the operator.  This subroutine builds an
 * actual argument list corresponding to the operands, then searches
 * for a compatible interface.  If one is found, the expression node
 * is replaced with the appropriate function call.  */

try g95_extend_expr(g95_expr *e) {
g95_actual_arglist *actual;
g95_symbol *sym;
g95_namespace *ns;
g95_user_op *uop;
int i;

  sym = NULL;

  actual = g95_get_actual_arglist();
  actual->expr = e->op1;

  if (e->op2 != NULL) {
    actual->next = g95_get_actual_arglist();
    actual->next->expr = e->op2;
  }

  i = fold_unary(e->operator);

  if (i == INTRINSIC_USER) {
    for(ns=g95_current_ns; ns; ns=ns->parent) {
      uop = g95_find_uop(e->uop->name, ns);
      if (uop == NULL) continue;

      sym = g95_search_interface(uop->operator, 0, &actual);
      if (sym != NULL) break;
    }
  } else {
    for(ns=g95_current_ns; ns; ns=ns->parent) {
      sym = g95_search_interface(ns->operator[i], 0, &actual);
      if (sym != NULL) break;
    }
  }

  if (sym == NULL) {  /* Don't use g95_free_actual_arglist() */
    if (actual->next != NULL) g95_free(actual->next);
    g95_free(actual);

    return FAILURE;
  }

/* Change the expression node to a function call */

  e->expr_type = EXPR_FUNCTION;
  e->symbol = sym;
  e->value.function.actual = actual;

  if (g95_pure(NULL) && !g95_pure(sym)) {
    g95_error("Function '%s' called in lieu of an operator at %L must be PURE",
	      sym->name, &e->where);
    return FAILURE;
  }

  if (g95_resolve_expr(e) == FAILURE) return FAILURE;

  return SUCCESS;
}


/* g95_extend_assign()-- Tries to replace an assignment code node with
 * a subroutine call to the subroutine associated with the assignment
 * operator.  Return SUCCESS if the node was replaced.  On FAILURE, no
 * error is generated. */

try g95_extend_assign(g95_code *c, g95_namespace *ns) {
g95_actual_arglist *actual;
g95_expr *lhs, *rhs;
g95_symbol *sym;

  lhs = c->expr;
  rhs = c->expr2;

  /* Don't allow an intrinsic assignment to be replaced */

  if (lhs->ts.type != BT_DERIVED && rhs->ts.type != BT_DERIVED && 
      (lhs->ts.type == rhs->ts.type ||
       (g95_numeric_ts(&lhs->ts) && g95_numeric_ts(&rhs->ts)))) return FAILURE;

  actual = g95_get_actual_arglist();
  actual->expr = lhs;

  actual->next = g95_get_actual_arglist();
  actual->next->expr = rhs;

  sym = NULL;

  for(; ns; ns=ns->parent) {
    sym = g95_search_interface(ns->operator[INTRINSIC_ASSIGN], 1, &actual);
    if (sym != NULL) break;
  }

  if (sym == NULL) {
    g95_free(actual->next);
    g95_free(actual);
    return FAILURE;
  }

  /* Replace the assignment with the call */

  c->op = EXEC_CALL;
  c->sym = sym;
  c->expr = NULL;
  c->expr2 = NULL;
  c->ext.actual = actual;

  if (g95_pure(NULL) && !g95_pure(sym)) {
    g95_error("Subroutine '%s' called in lieu of assignment at %L must be "
	      "PURE", sym->name, &c->loc);
    return FAILURE;
  }

  return SUCCESS;
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


/* g95_add_interface()-- Add a symbol to the current interface */

try g95_add_interface(g95_symbol *new) {
g95_interface **head, *intr;
g95_namespace *ns;
g95_symbol *sym;

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
      g95_find_symbol(current_interface.sym->name, ns, 0, &sym);
      if (sym == NULL) continue;

      if (check_new_interface(sym->generic, new) == FAILURE) return FAILURE;
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


/* g95_free_formal_arglist()-- Gets rid of a formal argument list.  We
 * do not free symbols.  Symbols are freed when a namespace is freed. */

void g95_free_formal_arglist(g95_formal_arglist *p) {
g95_formal_arglist *q;

  for(; p; p=q) {
    q = p->next;
    g95_free(p);
  }
}

