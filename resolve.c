/* Perform type resolution on the various stuctures.
   Copyright (C) 2001 Free Software Foundation, Inc.
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


#include <string.h>
#include "g95.h"

static g95_block_stack *block_stack;


/* namespace_kind()-- Given a namespace, figure out what kind it is.
 * We return one of the g95_compile_state enums COMP_NONE,
 * COMP_MODULE, COMP_SUBROUTINE or COMP_FUNCTION. */

static g95_compile_state namespace_kind(g95_namespace *ns) {
g95_symbol *sym;

  sym = ns->proc_name;

  if (sym == NULL) return COMP_NONE;

  if (sym->attr.flavor == FL_MODULE) return COMP_MODULE;

  if (sym->attr.subroutine) return COMP_SUBROUTINE;

  if (sym->attr.flavor == FL_VARIABLE ||
      sym->attr.function) return COMP_FUNCTION;

  return COMP_NONE;
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

static void resolve_formal_arglist(g95_formal_arglist *f) {
g95_symbol *sym;

  for(; f; f=f->next) {
    sym = f->sym;

    if (sym == NULL) continue;  /* Alternate return placeholder */

    if (sym->formal) resolve_formal_arglist(sym->formal);

    if (sym->attr.subroutine || sym->attr.external || sym->attr.intrinsic)
      continue;

    if (sym->ts.type == BT_UNKNOWN) {
      if (!sym->attr.function || sym->result == sym)
	g95_set_default_type(sym, 1, sym->ns);
      else {    /* Set the type of the RESULT, then copy */

	if (sym->result->ts.type == BT_UNKNOWN)
	  g95_set_default_type(sym->result, 1, sym->result->ns);

	sym->ts = sym->result->ts;
	if (sym->as == NULL) sym->as = g95_copy_array_spec(sym->result->as);
      }
    }

    g95_resolve_array_spec(sym->as);

    /* If the flavor is unknown at this point, it has to be a variable.
     * A procedure specification would have already set the type */

    if (sym->attr.flavor == FL_UNKNOWN)
      g95_add_flavor(&sym->attr, FL_VARIABLE, &sym->declared_at);

    if (sym->formal != NULL) resolve_formal_arglist(sym->formal);
  }
}


/* find_arglists()-- Work function called when searching for symbols
 * that have argument lists associated with them. */

static void find_arglists(g95_symbol *sym) {

  if (sym->formal == NULL) return;
  resolve_formal_arglist(sym->formal);
}


/* resolve_formal_arglists()-- Given a namespace, resolve all formal
 * argument lists within the namespace. */

static void resolve_formal_arglists(g95_namespace *ns) {

  if (ns == NULL) return;

  g95_traverse_ns(ns, find_arglists);
}


/* resolve_contained_functions()-- Resolve contained function types.
 * Because contained functions can call one another, they have to be
 * worked out before any of the contained procedures can be resolved.
 * If a function doesn't already have a type, the only way it can get
 * one is through an IMPLICIT type or a RESULT variable. */

void resolve_contained_functions(g95_namespace *ns) {
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

    sym_upper->ts = sym_lower->ts;
    sym_upper->as = g95_copy_array_spec(sym_lower->as);
  }
}


/* resolve_ref()-- Resolve subtype references */

static try resolve_ref(g95_expr *expr) {
g95_array_spec *as;
g95_ref *ref;
try t;

  as = (expr->symbol) ? expr->symbol->as : NULL; /* NULL for substrings */
  t = SUCCESS;

  for(ref=expr->ref; ref; ref=ref->next)
    switch(ref->type) {
    case REF_ARRAY:
      if (g95_resolve_array_ref(&ref->ar, as) == FAILURE) t = FAILURE;
      as = NULL;
      break;

    case REF_COMPONENT:
      as = ref->component->as;   /* In case an array ref is next */
      break;

    case REF_SUBSTRING:
      if (g95_resolve_expr(ref->start) == FAILURE) t = FAILURE;

      if (ref->start != NULL && ref->start->ts.type != BT_INTEGER) {
	g95_error("Substring index at %C must be of type INTEGER",
		  &ref->start->where);
	t = FAILURE;
      }

      if (g95_resolve_expr(ref->end) == FAILURE) t = FAILURE;

      if (ref->end != NULL && ref->end->ts.type != BT_INTEGER) {
	g95_error("Substring index at %C must be of type INTEGER",
		  &ref->end->where);
	t = FAILURE;
      }

      break;
    }

  return t;
}


/* resolve_structure_cons()-- Resolve all of the elements of a
 * structure constructor and make sure that the types are correct. */

static try resolve_structure_cons(g95_expr *expr) {
g95_constructor *cons;
g95_component *comp;
try t;

  t = SUCCESS;
  cons = expr->value.constructor;
  comp = expr->symbol->components;

  for(; comp; comp=comp->next, cons=cons->next) {
    if (g95_resolve_expr(cons->expr) == FAILURE) {
      t = FAILURE;
      continue;
    }

    /* If we don't have the right type, try to convert it. */

    if (!g95_compare_types(&cons->expr->ts, &comp->ts) &&
	g95_convert_type(cons->expr, &comp->ts, 1) == FAILURE)
      t = FAILURE;
  }

  return t;
}



/****************** Expression name resolution ******************/


/* procedure_kind()-- Figure out if the procedure is specific, generic
 * or unknown. */

typedef enum { PTYPE_GENERIC=1, PTYPE_SPECIFIC, PTYPE_UNKNOWN } proc_type;


proc_type procedure_kind(g95_symbol *sym) {
g95_symbol *s;
char *name;

  name = sym->name;

  /* Locate symbol in the nearest parent scope */

  s = NULL;
  if (sym->ns->parent != NULL) g95_find_symbol(name, sym->ns->parent, 1, &s);

  /* See if a symbol is generic */

  if (sym->attr.generic ||
      (sym->attr.intrinsic && g95_generic_intrinsic(name)))
    return PTYPE_GENERIC;

  g95_find_symbol(name, sym->ns->parent, 1, &s);

  if (s != NULL && (s->attr.generic ||
		    (s->attr.intrinsic && g95_generic_intrinsic(name))))
    return PTYPE_GENERIC;

  /* Not generic, see if it is specific */

  if (sym->attr.interface || sym->attr.proc == PROC_MODULE ||
      sym->attr.proc == PROC_INTERNAL || sym->attr.proc == PROC_ST_FUNCTION ||
      (sym->attr.intrinsic && g95_specific_intrinsic(name)) ||
      sym->attr.external)
    return PTYPE_SPECIFIC;

  /* Check parent scopes */

  if (s != NULL && (s->attr.interface || s->attr.proc == PROC_MODULE ||
		    s->attr.proc == PROC_INTERNAL ||
		    s->attr.proc == PROC_ST_FUNCTION ||
		    (s->attr.intrinsic && g95_specific_intrinsic(name)) ||
		    s->attr.external))
    return PTYPE_SPECIFIC;

  return PTYPE_UNKNOWN;
}


/* resolve_generic()-- Resolve a procedure call known to be generic.
 * Section 14.1.2.4.1. */

static match resolve_generic0(g95_expr *expr, g95_symbol *sym) {
g95_symbol *s;

  if (sym->attr.generic) {
    s = g95_search_interface(sym->generic, 0, expr->value.function.actual);
    if (s != NULL) {
      expr->value.function.name = s->name;
      expr->ts = s->ts;
      if (sym->as != NULL) expr->rank = sym->as->rank;
      return MATCH_YES;
    }

    /* TODO: Need to search for elemental references in generic interface */
  }

  if (sym->attr.intrinsic) return g95_intrinsic_func_interface(expr, 0);

  return MATCH_NO;
}


static try resolve_generic(g95_expr *expr) {
g95_symbol *sym;
match m;

  sym = expr->symbol;

  m = resolve_generic0(expr, sym);
  if (m == MATCH_YES) return SUCCESS;
  if (m == MATCH_ERROR) return FAILURE;

  if (sym->ns->parent != NULL) {
    g95_find_symbol(sym->name, sym->ns->parent, 1, &sym);
    if (sym != NULL) {
      m = resolve_generic0(expr, sym);
      if (m == MATCH_YES) return SUCCESS;
      if (m == MATCH_ERROR) return FAILURE;
    }
  }

  /* Last ditch attempt */

  if (!g95_generic_intrinsic(expr->symbol->name)) {
    g95_error("Generic function '%s' at %L is not a generic intrinsic "
	      "function", expr->symbol->name, &expr->where);
    return FAILURE;
  }

  m = g95_intrinsic_func_interface(expr, 0);
  if (m == MATCH_YES) return SUCCESS;
  if (m == MATCH_NO)
    g95_error("Generic function '%s' at %L is not consistent with a specific "
	      "intrinsic interface", expr->symbol->name, &expr->where);

  return FAILURE;
}


/* resolve_specific()-- Resolve a procedure call known to be specific */

static match resolve_specific0(g95_symbol *sym, g95_expr *expr) {
match m;

  if (sym->attr.external || sym->attr.interface) {
    if (sym->attr.dummy) {
      sym->attr.proc = PROC_DUMMY;
      goto found;
    }

    sym->attr.proc = PROC_EXTERNAL;
    goto found;
  }

  if (sym->attr.proc == PROC_MODULE || sym->attr.proc == PROC_ST_FUNCTION ||
      sym->attr.proc == PROC_INTERNAL) goto found;

  if (sym->attr.intrinsic) {
    m = g95_intrinsic_func_interface(expr, 1);
    if (m == MATCH_YES) return MATCH_YES;
    if (m == MATCH_NO)
      g95_error("Symbol '%s' at %L is INTRINSIC but is not compatible with "
		"an intrinsic", sym->name, &expr->where);
      
    return MATCH_ERROR;
  }

  return MATCH_NO;

found:
  expr->ts = sym->ts;
  expr->value.function.name = sym->name;
  if (sym->as != NULL) expr->rank = sym->as->rank;

  return MATCH_YES;
}


static try resolve_specific(g95_expr *expr) {
g95_symbol *sym;
match m;

  sym = expr->symbol;

  m = resolve_specific0(sym, expr);
  if (m == MATCH_YES) return SUCCESS;
  if (m == MATCH_ERROR) return FAILURE;

  g95_find_symbol(sym->name, sym->ns->parent, 1, &sym);

  if (sym != NULL) {
    m = resolve_specific0(sym, expr);
    if (m == MATCH_YES) return SUCCESS;
    if (m == MATCH_ERROR) return FAILURE;
  }

  g95_error("Unable to resolve the specific function '%s' at %L",
	    expr->symbol->name, &expr->where);

  return SUCCESS;
}


/* resolve_unknown()-- Resolve a procedure call not known to be
 * generic nor specific */

static try resolve_unknown(g95_expr *expr) {
g95_symbol *sym;
g95_typespec ts;

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
  if (sym->as != NULL) expr->rank = sym->as->rank;

  /* Type of the expression is either the type of the symbol or the
   * default type of the symbol */

 set_type:

  if (sym->ts.type != BT_UNKNOWN)
    expr->ts = sym->ts;
  else {
    ts = sym->ns->default_type[sym->name[0] - 'a'];

    if (ts.type == BT_UNKNOWN) {
      g95_error("Function '%s' at %L has no implicit type",
		sym->name, &expr->where);
      return FAILURE;
    } else
      expr->ts = ts;
  }

  return SUCCESS;
}


/* was_declared()-- Returns 0 if a symbol was not declared with a type
 * or attriute declaration statement, nonzero otherwise. */

static int was_declared(g95_symbol *sym) {
symbol_attribute a;

  a = sym->attr;

  if (!a.implicit_type && sym->ts.type != BT_UNKNOWN) return 1;

  if (a.allocatable || a.dimension || a.external || a.intrinsic ||
      a.optional || a.pointer || a.save || a.target ||
      a.access != ACCESS_UNKNOWN || a.intent != INTENT_UNKNOWN) return 1;

  return 0;
}


/* resolve_actual_arglist()-- Resolve and actual argument list.  Most
 * of the time, this is just resolving the expressions in the list.
 * The exception is that we sometimes have to decide whether arguments
 * that look like procedure arguments are really simple variable
 * references. */

static try resolve_actual_arglist(g95_actual_arglist *arg) {
g95_symbol *parent_sym, *sym;
g95_expr *e;

  for(; arg; arg=arg->next) {

    e = arg->expr;
    if (e == NULL) continue;

    if (e->ts.type != BT_PROCEDURE) {
      if (g95_resolve_expr(e) != SUCCESS) return FAILURE;
      continue;
    }

    /* See if the expression node should really be a variable reference */

    sym = e->symbol;

    if (sym->attr.flavor == FL_PROCEDURE || sym->attr.intrinsic ||
	sym->attr.external) {

      /* If the symbol is the function that names the current (or
       * parent) scope, then we really have a variable reference */

      if (sym->attr.function && sym->result == sym &&
	  (sym->ns->proc_name == sym ||
	   (sym->ns->parent != NULL && sym->ns->parent->proc_name == sym)))
	goto got_variable;

      continue;
    } 

    /* See if the name is a module procedure in a parent unit */

    if (was_declared(sym) || sym->ns->parent == NULL) goto got_variable;

    if (g95_find_symbol(sym->name, sym->ns->parent, 1, &parent_sym)) {
      g95_error("Symbol '%s' at %L is ambiguous", sym->name, &e->where);
      return FAILURE;
    }

    if (parent_sym == NULL) goto got_variable;

    sym = parent_sym;
    e->symbol = sym;    /* Point to the right thing */

    if (sym->attr.flavor == FL_PROCEDURE || sym->attr.intrinsic ||
	sym->attr.external) {
      continue;
    }

  got_variable:
    e->expr_type = EXPR_VARIABLE;
    e->ts = sym->ts;
    if (sym->as != NULL) e->rank = sym->as->rank;
  }

  return SUCCESS;
}


/* resolve_function()-- Resolve a function call, which means resolving
 * the arguments, then figuring out which entity the name refers to.
 * TODO: Check procedure arguments so that an INTENT(IN) isn't passed
 * to INTENT(OUT) or INTENT(INOUT).  */

static try resolve_function(g95_expr *expr) {
try t;

  if (resolve_actual_arglist(expr->value.function.actual) == FAILURE)
    return FAILURE;

/* See if function is already resolved */
  if (expr->value.function.name != NULL) {
    if (expr->ts.type == BT_UNKNOWN) expr->ts = expr->symbol->ts;
    return SUCCESS;
  }

/* Apply the rules of section 14.1.2 */

  switch(procedure_kind(expr->symbol)) {
  case PTYPE_GENERIC:   t = resolve_generic(expr);   break;
  case PTYPE_SPECIFIC:  t = resolve_specific(expr);  break;
  case PTYPE_UNKNOWN:   t = resolve_unknown(expr);   break;
  default:
    g95_internal_error("resolve_function(): bad function type");
  }

  return t;
}


/* resolve_operator()-- Resolve an operator expression node.  This can
 * involve replacing the operation with a user defined function call. */

static try resolve_operator(g95_expr *e) {
g95_expr *op1, *op2;
char msg[200];
try t;

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
	    g95_typename(&op1->ts));

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
    g95_internal_error("g95_resolve_expr(): Bad intrinsic");
  }

/* Deal with arrayness of an operand through an operator */

  t = SUCCESS;

  switch(e->operator) {
  case INTRINSIC_PLUS:    case INTRINSIC_MINUS:  case INTRINSIC_TIMES:
  case INTRINSIC_DIVIDE:  case INTRINSIC_POWER:  case INTRINSIC_CONCAT:
  case INTRINSIC_AND:     case INTRINSIC_OR:     case INTRINSIC_EQV:
  case INTRINSIC_NEQV:    case INTRINSIC_EQ:     case INTRINSIC_NE:
  case INTRINSIC_GT:      case INTRINSIC_GE:     case INTRINSIC_LT:
  case INTRINSIC_LE:

    if (op1->rank == 0 && op2->rank == 0) e->rank = 0;
    if (op1->rank == 0 && op2->rank != 0) e->rank = op2->rank;
    if (op1->rank != 0 && op2->rank == 0) e->rank = op1->rank;
    if (op1->rank != 0 && op2->rank != 0) {
      if (op1->rank == op2->rank)
	e->rank = op1->rank;
      else {
	g95_error("Inconsistent ranks for operator at %L and %L",
		  &op1->where, &op2->where);
	t = FAILURE;

	e->rank = 0;   /* Allow higher level expressions to work */
      }
    }

    break;

  case INTRINSIC_NOT:
  case INTRINSIC_UPLUS:
  case INTRINSIC_UMINUS:
    e->rank = op1->rank;
    break;           /* Simply copy arrayness attribute */

  default:
    break;
  }

  if (t == SUCCESS) t = g95_simplify_expr(e, 0);
  return t;

bad_op:
  if (g95_extend_expr(e) == SUCCESS) return SUCCESS;

  g95_error(msg, &e->where);
  return FAILURE;
}


/* g95_resolve_expr()-- Resolve an expression.  That is, make sure
 * that types of operands agree with their operators, intrinsic
 * operators are converted to function calls for overloaded types and
 * unresolved function references are resolved. */

try g95_resolve_expr(g95_expr *e) {
try t;

  if (e == NULL) return SUCCESS;

  switch(e->expr_type) {
  case EXPR_OP:
    t = resolve_operator(e);
    break;

  case EXPR_FUNCTION:
    t = resolve_function(e);
    break;

  case EXPR_VARIABLE:
    t = FAILURE;
    if (e->ref && resolve_ref(e) == FAILURE) break;

    if (e->symbol->attr.flavor == FL_PROCEDURE && !e->symbol->attr.function) {
      e->ts.type = BT_PROCEDURE;
      t = SUCCESS;
      break;
    }

    if (e->symbol->ts.type != BT_UNKNOWN)
      g95_variable_attr(e, &e->ts);
    else {     /* Must be a simple variable reference */
      if (g95_set_default_type(e->symbol, 1, NULL) == FAILURE) break;
      e->ts = e->symbol->ts;
    }

    t = SUCCESS;
    break;

  case EXPR_SUBSTRING:
    t = resolve_ref(e);
    break;

  case EXPR_CONSTANT:
  case EXPR_NULL:
    t = SUCCESS;
    break;

  case EXPR_ARRAY:
    t = g95_resolve_array_constructor(e);
    break;

  case EXPR_STRUCTURE:
    t = resolve_structure_cons(e);
    break;

  default:
    g95_internal_error("g95_resolve_expr(): Bad expression type");
  }

  return t;
}


/* resolve_call()-- Resolve a subroutine call.  More here later. */

static try resolve_call(g95_symbol *sym, g95_actual_arglist *arg) {

  return resolve_actual_arglist(arg);
}


/* g95_resolve_iterator()-- Resolve the expressions in an iterator
 * structure and require that they all be of integer type. */

try g95_resolve_iterator(g95_iterator *iter) {
try t;

  t = SUCCESS; 

  if (g95_resolve_expr(iter->var) == SUCCESS &&
      iter->var->ts.type != BT_INTEGER) {
    g95_error("Loop variable at %L must be INTEGER in Fortran 95",
	      &iter->var->where);
    t = FAILURE;
  }

  if (g95_resolve_expr(iter->start) == SUCCESS &&
      iter->start->ts.type != BT_INTEGER) {
    g95_error("Start expression in DO loop at %L must be INTEGER",
	      &iter->start->where);
    t = FAILURE;
  }

  if (g95_resolve_expr(iter->end) == SUCCESS &&
      iter->end->ts.type != BT_INTEGER) {
    g95_error("End expression in DO loop at %L must be INTEGER",
	      &iter->end->where);
    t = FAILURE;
  }

  if (g95_resolve_expr(iter->step) == SUCCESS &&
      iter->step->ts.type != BT_INTEGER) {
    g95_error("Step expression in DO loop at %L must be INTEGER",
	      &iter->step->where);
    t = FAILURE;
  }

  return t;
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


/* g95_resolve_code()-- Given a block of code, recursively resolve
 * everything pointed to by this code block */

void g95_resolve_code(g95_code *code, g95_namespace *ns) {
g95_block_stack *s;
g95_st_label *lp;
g95_alloc *a;
try t;

  for(; code; code=code->next) {

    for(s=block_stack; s; s=s->prev)
      if (s->block_no == code->block_no) break;

    if (s == NULL) {
      s = g95_get_block_stack();
      s->block_no = code->block_no;
      s->prev = block_stack;
      block_stack = s;
    } else {
      while (s != block_stack) {
	g95_block_stack *t = block_stack;
	block_stack = block_stack->prev;
	g95_free(t);
      }
    }

    if (code->op != EXEC_SELECT && code->block != NULL)
      g95_resolve_code(code->block,ns);

    t = g95_resolve_expr(code->expr);
    if (g95_resolve_expr(code->expr2) == FAILURE) t = FAILURE;

    switch(code->op) {
    case EXEC_NOP:  case EXEC_CYCLE:  case EXEC_IOLENGTH:
    case EXEC_STOP: case EXEC_NULLIFY: case EXEC_EXIT:
      break;

    case EXEC_GOTO:
      for(lp=ns->st_labels; lp; lp=lp->next)
        if (lp->label == code->label) break; /* always in list */

      for(s = block_stack; s; s = s->prev)
        if (s->block_no == lp->block_no) break;

      if (lp->defined == ST_LABEL_TARGET && s == NULL) {
        g95_error("Label at %L is not in the same block as the "
                  "GOTO statement at %L", &lp->where, &code->loc);
      } else if (lp->defined == ST_LABEL_BAD_TARGET) {
        g95_error("Statement at %L is not a valid branch target statement "
                  "for the GOTO statement at %L", &lp->where, &code->loc);
      } /* else: deal with undefined or FORMAT labels later */

      break;

    case EXEC_RETURN:
      if (code->expr != NULL && code->expr->ts.type != BT_INTEGER)
	g95_error("Alternate RETURN statement at %L requires an INTEGER "
		  "return specifier", &code->expr->where);
      break;

    case EXEC_ASSIGN:
      if (t == FAILURE) break;

      if (g95_extend_assign(code, ns) == SUCCESS) break;

      g95_check_assign(code->expr, code->expr2);
      break;

    case EXEC_POINTER_ASSIGN:
      if (t == FAILURE) break;

      g95_check_pointer_assign(code->expr, code->expr2);
      break;

    case EXEC_ARITHMETIC_IF:
      if (t == SUCCESS && code->expr->ts.type != BT_INTEGER &&
	  code->expr->ts.type != BT_REAL)
	g95_error("Arithmetic IF statement at %L requires a numeric "
		  "expression", &code->expr->where);
      break;

    case EXEC_IF:
      if (t == SUCCESS && code->expr != NULL &&
	  code->expr->ts.type != BT_LOGICAL)
	g95_error("IF/ELSE IF clause at %L requires a LOGICAL expression",
		  &code->expr->where);
      break;

    case EXEC_WHERE:
      if (t == SUCCESS && code->expr != NULL &&
	  (code->expr->ts.type != BT_LOGICAL || code->expr->rank == 0))
	g95_error("WHERE/ELSEWHERE clause at %L requires a LOGICAL array",
		  &code->expr->where);
      break;

    case EXEC_CALL:
      resolve_call(code->sym, code->ext.arglist);
      break;

    case EXEC_SELECT:
      g95_resolve_select(code, ns);      /* Select is complicated */
      break;

    case EXEC_DO:
      if (code->ext.iterator != NULL) g95_resolve_iterator(code->ext.iterator);
      break;

    case EXEC_DO_WHILE:
      if (t == SUCCESS && code->expr != NULL &&
	  code->expr->ts.type != BT_LOGICAL)
	g95_error("Argument of DO WHILE loop at %L must be of type LOGICAL",
		  &code->expr->where);
      break;

    case EXEC_ALLOCATE:
      if (t == SUCCESS && code->expr != NULL &&
	  code->expr->ts.type != BT_INTEGER)
	g95_error("STAT tag in ALLOCATE statement at %L must be "
		  "of type INTEGER", &code->expr->where);

      for(a=code->ext.alloc_list; a; a=a->next)
	g95_resolve_expr(a->expr);
      break;

    case EXEC_DEALLOCATE:
      if (t == SUCCESS && code->expr != NULL &&
	  code->expr->ts.type != BT_INTEGER)
	g95_error("STAT tag in DEALLOCATE statement at %L must be of type "
		  "INTEGER", &code->expr->where);

      for(a=code->ext.alloc_list; a; a=a->next)
	g95_resolve_expr(a->expr);
      break;

    case EXEC_OPEN:
      g95_resolve_open(code->ext.open);
      break;

    case EXEC_CLOSE:
      g95_resolve_close(code->ext.close);
      break;

    case EXEC_BACKSPACE:
    case EXEC_ENDFILE:
    case EXEC_REWIND:
      g95_resolve_filepos(code->ext.filepos);
      break;

    case EXEC_INQUIRE:
      g95_resolve_inquire(code->ext.inquire);
      break;

    case EXEC_READ:
    case EXEC_WRITE:
      g95_resolve_dt(code->ext.dt);
      break;

    case EXEC_FORALL:
      resolve_forall_iterators(code->ext.forall_iterator);

      if (code->expr != NULL && code->expr->ts.type != BT_LOGICAL)
	g95_error("FORALL mask clause at %L requires a LOGICAL expression",
		  &code->expr->where);
      break;

    default:    
      g95_internal_error("g95_resolve_code(): Bad statement code");
    }
  }
}


/* resolve_symbol()-- Do anything necessary to resolve a symbol.
 * Right now, we just assume that an otherwise unknown symbol is a
 * variable.  This sort of thing commonly happens for symbols in module. */

static void resolve_symbol(g95_symbol *sym) {

  if (sym->attr.flavor == FL_UNKNOWN) {
    if (sym->attr.external == 0 && sym->attr.intrinsic == 0)
      sym->attr.flavor = FL_VARIABLE;
    else {
      sym->attr.flavor = FL_PROCEDURE;
      if (sym->attr.dimension) sym->attr.function = 1;
    }
  }

  /* Assign default type to symbols that need one and don't have one */

  if (sym->ts.type == BT_UNKNOWN) {
    if (sym->attr.flavor == FL_VARIABLE || sym->attr.flavor == FL_PARAMETER)
      g95_set_default_type(sym, 0, NULL);

    if (sym->attr.flavor == FL_PROCEDURE && sym->attr.function) {
      if (sym->result == sym || sym->result == NULL)
	g95_set_default_type(sym, 0, NULL);
      else {
	resolve_symbol(sym->result);  /* Result may be in another namespace */

	sym->ts = sym->result->ts;
	sym->as = g95_copy_array_spec(sym->result->as);
      }
    }
  }

  if (sym->as != NULL && sym->as->type == AS_ASSUMED_SIZE &&
      sym->attr.dummy == 0) {
    g95_error("Assumed size array at %L must be a dummy argument",
	      &sym->declared_at);
    return;
  }

  /* Resolve initial values and make sure they are compatible with the
   * variable */

  if (sym->value != NULL) {
    if (g95_resolve_expr(sym->value) == FAILURE ||
	g95_check_assign_symbol(sym, sym->value) == FAILURE) return;
  }

  /* Make sure the types of derived parameters are consistent.  This
   * type checking is deferred until resolution because the type may
   * refer to a derived type from the host. */

  if (sym->attr.flavor == FL_PARAMETER && sym->ts.type == BT_DERIVED &&
      !g95_compare_types(&sym->ts, &sym->value->ts))
    g95_error("Incompatible derived type in PARAMETER at %L",
	      &sym->value->where);

  /* Make sure symbols with known intent or optional are really dummy
   * variable.  Because of ENTRY statement, this has to be deferred
   * until resolution time. */

  if ((sym->attr.optional || sym->attr.intent != INTENT_UNKNOWN) &&
      sym->attr.dummy == 0) {

    g95_error("Symbol at %L is not a DUMMY variable", &sym->declared_at);
    return;
  }

  g95_resolve_array_spec(sym->as);
}


/* g95_resolve()-- This function is called after a complete program
 * unit has been compiled.  Its purpose is to examine all of the
 * expressions associated with a program unit, assign types to all
 * intermediate expressions, make sure that all assignments are to
 * compatible types and figure out which names refer to which
 * functions or subroutines. */

void g95_resolve(g95_namespace *ns) {
g95_namespace *old_ns, *n;
g95_charlen *cl;

  old_ns = g95_current_ns;
  g95_current_ns = ns;

  g95_check_interfaces(ns);

  resolve_contained_functions(ns);

  g95_traverse_ns(ns, resolve_symbol);

  for(n=ns->contained; n; n=n->sibling)
    g95_resolve(n);

  if (ns->save_all) g95_save_all(ns);

  for(cl=ns->cl_list; cl; cl=cl->next) {
    if (cl->length == NULL || g95_resolve_expr(cl->length) == FAILURE)
      continue;

    if (cl->length->ts.type != BT_INTEGER)
      g95_error("Character length specification at %L must be of type INTEGER",
		&cl->length->where);
  }

  g95_resolve_code(ns->code, ns);

  g95_check_st_labels(ns);

  g95_current_ns = old_ns;
}
