/* Perform type resolution on the various stuctures.
   Copyright (C) 2001, 2002 Free Software Foundation, Inc.
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


/* Stack to push the current if we descend into a block during
 * resolution.  See resolve_branch() and resolve_code().  */

typedef struct code_stack {
  struct g95_code *head, *current;
  struct code_stack *prev;
} code_stack;

static code_stack *cs_base = NULL;


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

static void resolve_formal_arglist(g95_symbol *proc) {
g95_formal_arglist *f;
g95_symbol *sym;

  for(f=proc->formal; f; f=f->next) {
    sym = f->sym;

    if (sym == NULL) {  /* Alternate return placeholder */
      if (g95_elemental(proc))
	g95_error("Alternate return specifier in elemental subroutine "
		  "'%s' at %L is not allowed", proc->name, &proc->declared_at);
      continue;
    }

    if (sym->attr.if_source != IFSRC_UNKNOWN) resolve_formal_arglist(sym);

    if (sym->attr.subroutine || sym->attr.external || sym->attr.intrinsic) {
      if (g95_pure(proc) && !g95_pure(sym)) {
	g95_error("Dummy procedure '%s' of PURE procedure at %L must also "
		  "be PURE", sym->name, &sym->declared_at);
	continue;
      }

      if (g95_elemental(proc)) {
	g95_error("Dummy procedure at %L not allowed in ELEMENTAL procedure",
		  &sym->declared_at);
	continue;
      }

      continue;
    }

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

    if (g95_pure(proc)) {
      if (proc->attr.function && !sym->attr.pointer &&
	  sym->attr.intent != INTENT_IN)

	g95_error("Argument '%s' of pure function '%s' at %L must be "
		  "INTENT(IN)", sym->name, proc->name, &sym->declared_at);

      if (proc->attr.subroutine && !sym->attr.pointer &&
	  sym->attr.intent == INTENT_UNKNOWN) 

	g95_error("Argument '%s' of pure subroutine '%s' at %L must have "
		  "its INTENT specified", sym->name, proc->name,
		  &sym->declared_at);
    }


    if (g95_elemental(proc)) {
      if (sym->as != NULL) {
	g95_error("Argument '%s' of elemental procedure at %L must be scalar",
		  sym->name, &sym->declared_at);
	continue;
      }

      if (sym->attr.pointer) {
	g95_error("Argument '%s' of elemental procedure at %L cannot have "
		  "the POINTER attribute", sym->name, &sym->declared_at);
	continue;
      }
    }
  }
}


/* find_arglists()-- Work function called when searching for symbols
 * that have argument lists associated with them. */

static void find_arglists(g95_symbol *sym) {

  if (sym->attr.if_source == IFSRC_UNKNOWN || sym->ns != g95_current_ns)
    return;

  resolve_formal_arglist(sym);
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

    sym_upper->ts = sym_lower->ts;
    sym_upper->as = g95_copy_array_spec(sym_lower->as);
  }
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

static proc_type procedure_kind(g95_symbol *sym) {
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

  if (sym->attr.if_source == IFSRC_IFBODY || sym->attr.proc == PROC_MODULE ||
      sym->attr.proc == PROC_INTERNAL || sym->attr.proc == PROC_ST_FUNCTION ||
      (sym->attr.intrinsic && g95_specific_intrinsic(name)) ||
      sym->attr.external)
    return PTYPE_SPECIFIC;

  /* Check parent scopes */

  if (s != NULL && (s->attr.if_source == IFSRC_IFBODY ||
		    s->attr.proc == PROC_MODULE ||
		    s->attr.proc == PROC_INTERNAL ||
		    s->attr.proc == PROC_ST_FUNCTION ||
		    (s->attr.intrinsic && g95_specific_intrinsic(name)) ||
		    s->attr.external))
    return PTYPE_SPECIFIC;

  return PTYPE_UNKNOWN;
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
    if (sym->as != NULL) {
      e->rank = sym->as->rank;
      e->ref = g95_get_ref();
      e->ref->type = REF_ARRAY;
      e->ref->u.ar.type = AR_FULL;
      e->ref->u.ar.as = sym->as;
    }
  }

  return SUCCESS;
}


/************* Function resolution *************/

/* resolve_generic_f0()-- Resolve a function call known to be generic.
 * Section 14.1.2.4.1. */

static match resolve_generic_f0(g95_expr *expr, g95_symbol *sym) {
g95_symbol *s;

  if (sym->attr.generic) {
    s = g95_search_interface(sym->generic, 0, &expr->value.function.actual);
    if (s != NULL) {
      expr->value.function.name = s->name;
      expr->value.function.esym = s;
      expr->ts = s->ts;
      if (s->as != NULL) expr->rank = s->as->rank;
      return MATCH_YES;
    }

    /* TODO: Need to search for elemental references in generic interface */
  }

  if (sym->attr.intrinsic) return g95_intrinsic_func_interface(expr, 0);

  return MATCH_NO;
}


static try resolve_generic_f(g95_expr *expr) {
g95_symbol *sym;
match m;

  sym = expr->symbol;

  m = resolve_generic_f0(expr, sym);
  if (m == MATCH_YES) return SUCCESS;
  if (m == MATCH_ERROR) return FAILURE;

  if (sym->ns->parent != NULL) {
    g95_find_symbol(sym->name, sym->ns->parent, 1, &sym);
    if (sym != NULL) {
      m = resolve_generic_f0(expr, sym);
      if (m == MATCH_YES) return SUCCESS;
      if (m == MATCH_ERROR) return FAILURE;
    }
  }

  /* Last ditch attempt */

  if (!g95_generic_intrinsic(expr->symbol->name)) {
    g95_error("Generic function '%s' at %L is not an intrinsic function",
	      expr->symbol->name, &expr->where);
    return FAILURE;
  }

  m = g95_intrinsic_func_interface(expr, 0);
  if (m == MATCH_YES) return SUCCESS;
  if (m == MATCH_NO)
    g95_error("Generic function '%s' at %L is not consistent with a specific "
	      "intrinsic interface", expr->symbol->name, &expr->where);

  return FAILURE;
}


/* resolve_specific_f0()-- Resolve a function call known to be specific */

static match resolve_specific_f0(g95_symbol *sym, g95_expr *expr) {
match m;

  if (sym->attr.external || sym->attr.if_source == IFSRC_IFBODY) {
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
  g95_procedure_use(sym, &expr->value.function.actual, &expr->where);

  expr->ts = sym->ts;
  expr->value.function.name = sym->name;
  expr->value.function.esym = sym;
  if (sym->as != NULL) expr->rank = sym->as->rank;

  return MATCH_YES;
}


static try resolve_specific_f(g95_expr *expr) {
g95_symbol *sym;
match m;

  sym = expr->symbol;

  m = resolve_specific_f0(sym, expr);
  if (m == MATCH_YES) return SUCCESS;
  if (m == MATCH_ERROR) return FAILURE;

  g95_find_symbol(sym->name, sym->ns->parent, 1, &sym);

  if (sym != NULL) {
    m = resolve_specific_f0(sym, expr);
    if (m == MATCH_YES) return SUCCESS;
    if (m == MATCH_ERROR) return FAILURE;
  }

  g95_error("Unable to resolve the specific function '%s' at %L",
	    expr->symbol->name, &expr->where);

  return SUCCESS;
}


/* resolve_unknown_f()-- Resolve a procedure call not known to be
 * generic nor specific */

static try resolve_unknown_f(g95_expr *expr) {
g95_symbol *sym;
g95_typespec *ts;

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

  g95_procedure_use(sym, &expr->value.function.actual, &expr->where);

  /* Type of the expression is either the type of the symbol or the
   * default type of the symbol */

 set_type:
  if (sym->ts.type != BT_UNKNOWN)
    expr->ts = sym->ts;
  else {
    ts = g95_get_default_type(sym, sym->ns);

    if (ts->type == BT_UNKNOWN) {
      g95_error("Function '%s' at %L has no implicit type",
		sym->name, &expr->where);
      return FAILURE;
    } else
      expr->ts = *ts;
  }

  return SUCCESS;
}


/* resolve_function()-- Resolve a function call, which means resolving
 * the arguments, then figuring out which entity the name refers to.
 * TODO: Check procedure arguments so that an INTENT(IN) isn't passed
 * to INTENT(OUT) or INTENT(INOUT).  */

static try resolve_function(g95_expr *expr) {
g95_actual_arglist *arg;
char *name;
int flag;
try t;

  if (resolve_actual_arglist(expr->value.function.actual) == FAILURE)
    return FAILURE;

/* See if function is already resolved */

  if (expr->value.function.name != NULL) {
    if (expr->ts.type == BT_UNKNOWN) expr->ts = expr->symbol->ts;
    t = SUCCESS;
  } else {     /* Apply the rules of section 14.1.2 */

    switch(procedure_kind(expr->symbol)) {
    case PTYPE_GENERIC:
      t = resolve_generic_f(expr);
      break;

    case PTYPE_SPECIFIC:
      t = resolve_specific_f(expr);
      break;

    case PTYPE_UNKNOWN:
      t = resolve_unknown_f(expr);
      break;

    default:
      g95_internal_error("resolve_function(): bad function type");
    }
  }

  /* If the expression is still a function (it might have simplified),
   * then we check to see if we are calling an elemental function */

  if (expr->expr_type == EXPR_FUNCTION) {
    if (expr->value.function.actual != NULL &&
	((expr->value.function.esym != NULL &&
	  expr->value.function.esym->attr.elemental) ||
	 (expr->value.function.isym != NULL &&
	  expr->value.function.isym->elemental))) {

      /* The rank of an elemental is the rank of its array argument(s) */

      for(arg=expr->value.function.actual; arg; arg=arg->next) {
	if (arg->expr != NULL && arg->expr->rank > 0) {
	  expr->rank = arg->expr->rank;
	  break;
	}
      }
    }

    if (g95_pure(NULL)) {
      if (expr->value.function.esym) {
	flag = g95_pure(expr->value.function.esym);
	name = expr->value.function.esym->name;
      } else if (expr->value.function.isym) {
	flag = expr->value.function.isym->pure;
	name = expr->value.function.isym->name;
      } else {
        /* function declared implicitly, hence not pure */
        flag = 0;
        name = expr->value.function.name;
      }

      if (!flag) {
	g95_error("Function reference to '%s' at %L is to a non-PURE "
		  "procedure within a PURE procedure", name, &expr->where);
	t = FAILURE;
      }
    }
  }

  return t;
}


/************* Subroutine resolution *************/

static void pure_subroutine(g95_code *c, g95_symbol *sym) {

  if (!g95_pure(NULL) || g95_pure(sym)) return;

  g95_error("Subroutine call to '%s' at %L is not PURE", c->sub_name, &c->loc);
}


static match resolve_generic_s0(g95_code *c, g95_symbol *sym) {
g95_symbol *s;

  if (sym->attr.generic) {
    s = g95_search_interface(sym->generic, 1, &c->ext.actual);
    if (s != NULL) {
      c->sub_name = s->name;
      pure_subroutine(c, s);
      return MATCH_YES;
    }

    /* TODO: Need to search for elemental references in generic interface */
  }

  if (sym->attr.intrinsic)
    return g95_intrinsic_sub_interface(c) == SUCCESS ? MATCH_YES : MATCH_ERROR;

  return MATCH_NO;
}


static try resolve_generic_s(g95_code *c) {
g95_symbol *sym;
match m;

  sym = c->sym;

  m = resolve_generic_s0(c, sym);
  if (m == MATCH_YES) return SUCCESS;
  if (m == MATCH_ERROR) return FAILURE;

  if (sym->ns->parent != NULL) {
    g95_find_symbol(sym->name, sym->ns->parent, 1, &sym);
    if (sym != NULL) {
      m = resolve_generic_s0(c, sym);
      if (m == MATCH_YES) return SUCCESS;
      if (m == MATCH_ERROR) return FAILURE;
    }
  }

  /* Last ditch attempt */

  if (!g95_generic_intrinsic(sym->name)) {
    g95_error("Generic subroutine '%s' at %L is not an intrinsic subroutine",
	      sym->name, &c->loc);
    return FAILURE;
  }

  return g95_intrinsic_sub_interface(c) == SUCCESS ? MATCH_YES : MATCH_ERROR;
}


/* resolve_specific_s0()-- Resolve a subroutine call known to be specific */

static match resolve_specific_s0(g95_code *c, g95_symbol *sym) {

  if (sym->attr.external || sym->attr.if_source == IFSRC_IFBODY) {
    if (sym->attr.dummy) {
      sym->attr.proc = PROC_DUMMY;
      goto found;
    }

    sym->attr.proc = PROC_EXTERNAL;
    goto found;
  }

  if (sym->attr.proc == PROC_MODULE || sym->attr.proc == PROC_INTERNAL)
    goto found;

  if (sym->attr.intrinsic) {
    if (g95_intrinsic_sub_interface(c) == SUCCESS) return MATCH_YES;

    g95_error("Symbol '%s' at %L is INTRINSIC but is not compatible with "
	      "an intrinsic", sym->name, &c->loc);

    return MATCH_ERROR;
  }

  return MATCH_NO;

found:
  g95_procedure_use(sym, &c->ext.actual, &c->loc);

  c->sub_name = sym->name;
  pure_subroutine(c, sym);

  return MATCH_YES;
}


static try resolve_specific_s(g95_code *c) {
g95_symbol *sym;
match m;

  sym = c->sym;

  m = resolve_specific_s0(c, sym);
  if (m == MATCH_YES) return SUCCESS;
  if (m == MATCH_ERROR) return FAILURE;

  g95_find_symbol(sym->name, sym->ns->parent, 1, &sym);

  if (sym != NULL) {
    m = resolve_specific_s0(c, sym);
    if (m == MATCH_YES) return SUCCESS;
    if (m == MATCH_ERROR) return FAILURE;
  }

  g95_error("Unable to resolve the specific subroutine '%s' at %L",
	    sym->name, &c->loc);

  return SUCCESS;
}


/* resolve_unknown_s()-- Resolve a subroutine call not known to be
 * generic nor specific */

static try resolve_unknown_s(g95_code *c) {
g95_symbol *sym;

  sym = c->sym;

  if (sym->attr.dummy) {
    sym->attr.proc = PROC_DUMMY;
    c->sub_name = sym->name;
    return SUCCESS;
  }

  /* See if we have an intrinsic function reference */

  if (g95_intrinsic_name(sym->name, 1)) return g95_intrinsic_sub_interface(c);

  /* The reference is to an external name */

  g95_procedure_use(sym, &c->ext.actual, &c->loc);

  c->sub_name = sym->name;

  pure_subroutine(c, sym);

  return SUCCESS;
}


/* resolve_call()-- Resolve a subroutine call.  Although it was
 * tempting to use the same code for functions, subroutines and
 * functions are stored differently and this makes things awkward. */

static try resolve_call(g95_code *c) {
try t;

  if (resolve_actual_arglist(c->ext.actual) == FAILURE) return FAILURE;

  if (c->sub_name != NULL) return SUCCESS;

  switch(procedure_kind(c->sym)) {
  case PTYPE_GENERIC:
    t = resolve_generic_s(c);
    break;

  case PTYPE_SPECIFIC:
    t = resolve_specific_s(c);
    break;

  case PTYPE_UNKNOWN:
    t = resolve_unknown_s(c);
    break;

  default:
    g95_internal_error("resolve_subroutine(): bad function type");
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


/************** Array resolution subroutines **************/


typedef enum { CMP_LT, CMP_EQ, CMP_GT, CMP_UNKNOWN } comparison;

/* compare_bound()-- Compare two integer expressions to see if a<b.
 * Returns nonzero if this is so, zero if not so.  If either a or b is
 * indeterminate at compile time, zero is returned. */

static comparison compare_bound(g95_expr *a, g95_expr *b) {
int i;

  if (a == NULL || a->expr_type != EXPR_CONSTANT ||
      b == NULL || b->expr_type != EXPR_CONSTANT) return CMP_UNKNOWN;

  i = mpz_cmp(a->value.integer, b->value.integer);

  if (i < 0) return CMP_LT;
  if (i > 0) return CMP_GT;
  return CMP_EQ;
}


/* compare_bound_int()-- Compare an integer expression with an integer. */

static int compare_bound_int(g95_expr *a, int b) {
int i;

  if (a == NULL || a->expr_type != EXPR_CONSTANT) return CMP_UNKNOWN;

  i = mpz_cmp_si(a->value.integer, b);

  if (i < 0) return CMP_LT;
  if (i > 0) return CMP_GT;
  return CMP_EQ;
}


/* check_dimension()-- Compare a single dimension of an array
 * reference to the array specification. */

static try check_dimension(int i, g95_array_ref *ar, g95_array_spec *as) {

/* Given start, end and stride values, calculate the minimum and
 * maximum referenced indexes. */

  switch(ar->type) {
  case AR_FULL:
    break;

  case AR_ELEMENT:
    if (compare_bound(ar->start[i], as->lower[i]) == CMP_LT) goto bound;
    if (compare_bound(ar->start[i], as->upper[i]) == CMP_GT) goto bound;

    break;

  case AR_SECTION:
    if (compare_bound_int(ar->stride[i], 0) == CMP_EQ) {
      g95_error("Illegal stride of zero at %L", &ar->c_where[i]);
      return FAILURE;
    }

    /* TODO: More complicated range check in which the sign of the
     * stride figures */

    break;

  default:
    g95_internal_error("check_dimension(): Bad array reference");
  }

  return SUCCESS;

bound:
  g95_warning("Array reference at %L is out of bounds", &ar->c_where[i]);
  return SUCCESS;
}


/* compare_spec_to_ref()-- Compare an array reference with an
 * array specification. */

static try compare_spec_to_ref(g95_array_ref *ar) {
g95_array_spec *as;
int i;

  if (ar->type == AR_FULL) return SUCCESS;

  as = ar->as; 
  if (as->rank != ar->dimen) {
    g95_error("Rank mismatch in array reference at %L (%d/%d)",
	      &ar->where, ar->dimen, as->rank);
    return FAILURE;
  }

  for(i=0; i<as->rank; i++)
    if (check_dimension(i, ar, as) == FAILURE) return FAILURE;

  return SUCCESS;
}


/* resolve_index()-- Resolve one part of an array index */

static try resolve_index(g95_expr *index) {
g95_typespec ts;

  if (index == NULL) return SUCCESS;

  if (g95_resolve_expr(index) == FAILURE) return FAILURE;

  if (!g95_numeric_ts(&index->ts)) {
    g95_error("Array index at %L must be of numeric type", &index->where);
    return FAILURE;
  }

  if (index->ts.type != BT_INTEGER ||
      index->ts.kind != g95_default_integer_kind()) {
    ts.type = BT_INTEGER;
    ts.kind = g95_default_integer_kind();

    g95_convert_type(index, &ts, 2);
  }

  return SUCCESS;
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

static void find_array_spec(g95_expr *e) {
g95_array_spec *as;
g95_component *c;
g95_ref *ref;

  as = e->symbol->as; 
  c = e->symbol->components;

  for(ref=e->ref; ref; ref=ref->next)
    switch(ref->type) {
    case REF_ARRAY:
      if (as == NULL) g95_internal_error("find_array_spec(): Missing spec");

      ref->u.ar.as = as;
      as = NULL;
      break;

    case REF_COMPONENT:
      for(; c; c=c->next)
	if (c == ref->u.c.component) break;

      if (c == NULL)
	g95_internal_error("find_array_spec(): Component not found");

      if (c->dimension) {
	if (as != NULL) g95_internal_error("find_array_spec(): unused as(1)");
	as = c->as;
      }

      c = c->ts.derived->components;
      break;

    case REF_SUBSTRING:
      break;
    }

  if (as != NULL) g95_internal_error("find_array_spec(): unused as(2)");
}


/* resolve_array_ref()-- Resolve an array reference */

static try resolve_array_ref(g95_array_ref *ar) {
int i;

  for(i=0; i<ar->dimen; i++) {
    if (resolve_index(ar->start[i]) == FAILURE) return FAILURE;
    if (resolve_index(ar->end[i]) == FAILURE) return FAILURE;
    if (resolve_index(ar->stride[i]) == FAILURE) return FAILURE;

    if (ar->dimen_type[i] == DIMEN_UNKNOWN)
      switch(ar->start[i]->rank) {
      case 0:
	ar->dimen_type[i] = DIMEN_ELEMENT;
	break;

      case 1:
	ar->dimen_type[i] = DIMEN_VECTOR;
	break;

      default:
	g95_error("Array index at %L is an array of rank %d", &ar->c_where[i],
		  ar->start[i]->rank);
	return FAILURE;
      }
  }

  /* If the reference type is unknown, figure out what kind it is */

  if (ar->type == AR_UNKNOWN) {
    ar->type = AR_ELEMENT;
    for(i=0; i<ar->dimen; i++)
      if (ar->dimen_type[i] == DIMEN_RANGE ||
	  ar->dimen_type[i] == DIMEN_VECTOR) {
	ar->type = AR_SECTION;
	break;
      }
  }

  if (compare_spec_to_ref(ar) == FAILURE) return FAILURE;

  return SUCCESS;
}


static try resolve_substring(g95_ref *ref) {

  if (ref->u.ss.start != NULL) {
    if (g95_resolve_expr(ref->u.ss.start) == FAILURE) return FAILURE;

    if (ref->u.ss.start->ts.type != BT_INTEGER) {
      g95_error("Substring start index at %L must be of type INTEGER",
		&ref->u.ss.start->where);
      return FAILURE;
    }

    if (ref->u.ss.start->rank != 0) {
      g95_error("Substring start index at %L must be scalar",
		&ref->u.ss.start->where);
      return FAILURE;
    }

    if (compare_bound_int(ref->u.ss.start, 1) == CMP_LT) {
      g95_error("Substring start index at %L is less than one",
		&ref->u.ss.start->where);
      return FAILURE;
    }
  }

  if (ref->u.ss.end != NULL) {
    if (g95_resolve_expr(ref->u.ss.end) == FAILURE) return FAILURE;

    if (ref->u.ss.end->ts.type != BT_INTEGER) {
      g95_error("Substring end index at %C must be of type INTEGER",
		&ref->u.ss.end->where);
      return FAILURE;
    }

    if (ref->u.ss.end->rank != 0) {
      g95_error("Substring end index at %L must be scalar",
		&ref->u.ss.end->where);
      return FAILURE;
    }

    if (ref->u.ss.length != NULL &&
	compare_bound(ref->u.ss.end, ref->u.ss.length->length) == CMP_GT) {
      g95_error("Substring end index at %L is out of bounds",
		&ref->u.ss.start->where);
      return FAILURE;
    }
  }

  return SUCCESS;
}


/* resolve_ref()-- Resolve subtype references */

static try resolve_ref(g95_expr *expr) {
g95_ref *ref;

  for(ref=expr->ref; ref; ref=ref->next) {
    if (ref->type == REF_ARRAY && ref->u.ar.as == NULL) {
      find_array_spec(expr);
      break;
    }
  }

  for(ref=expr->ref; ref; ref=ref->next)
    switch(ref->type) {
    case REF_ARRAY:
      if (resolve_array_ref(&ref->u.ar) == FAILURE) return FAILURE;
      break;

    case REF_COMPONENT:
      break;

    case REF_SUBSTRING:
      resolve_substring(ref);
      break;
    }

  return SUCCESS;
}


/* expression_rank()-- Given a variable expression node, compute the
 * rank of the expression by examining the base symbol and any
 * reference structures it may have. */

static void expression_rank(g95_expr *e) {
g95_ref *ref;
int i, rank;

  if (e->ref == NULL) {
    if (e->expr_type == EXPR_ARRAY) return;
    /* Constructors can have a rank different from one via RESHAPE() */

    if (e->symbol == NULL) {
      e->rank = 0; 
      return;
    }

    e->rank = (e->symbol->as == NULL) ? 0 : e->symbol->as->rank;
    return;
  }

  rank = 0;

  for(ref=e->ref; ref; ref=ref->next) {
    if (ref->type != REF_ARRAY) continue;

    if (ref->u.ar.type == AR_FULL) {
      rank = ref->u.ar.as->rank;
      break;
    }

    if (ref->u.ar.type == AR_SECTION) {/* Figure out the rank of the section */
      if (rank != 0) g95_internal_error("expression_rank(): Two array specs");

      for(i=0; i<ref->u.ar.dimen; i++)
	if (ref->u.ar.dimen_type[i] == DIMEN_RANGE ||
	    ref->u.ar.dimen_type[i] == DIMEN_VECTOR) rank++;

      break;
    }
  }

  e->rank = rank;
}


/* resolve_variable()-- Resolve a variable expression. */

static try resolve_variable(g95_expr *e) {

  if (e->ref && resolve_ref(e) == FAILURE) return FAILURE;

  if (e->symbol->attr.flavor == FL_PROCEDURE && !e->symbol->attr.function) {
    e->ts.type = BT_PROCEDURE;
    return SUCCESS;
  }

  if (e->symbol->ts.type != BT_UNKNOWN)
    g95_variable_attr(e, &e->ts);
  else {        /* Must be a simple variable reference */
    if (g95_set_default_type(e->symbol, 1, NULL) == FAILURE) return FAILURE;
    e->ts = e->symbol->ts;
  }

  expression_rank(e);
  return SUCCESS;
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
    t = resolve_variable(e);
    break;

  case EXPR_SUBSTRING:
    t = resolve_ref(e);
    break;

  case EXPR_CONSTANT:
  case EXPR_NULL:
    t = SUCCESS;
    break;

  case EXPR_ARRAY:
    t = FAILURE;
    if (resolve_ref(e) == FAILURE) break;

    expression_rank(e);

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

  if (iter->step->expr_type == EXPR_CONSTANT &&
      mpz_cmp_ui(iter->step->value.integer, 0) == 0) {
    g95_error("Step expression in DO loop at %L cannot be zero",
	      &iter->step->where);
    return FAILURE;
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


/* derived_pointer()-- Given a pointer to a symbol that is a derived
 * type, see if any components have the POINTER attribute.  The search
 * is recursive if necessary.  Returns zero if no pointer components
 * are found, nonzero otherwise. */

static int derived_pointer(g95_symbol *sym) {
g95_component *c;

  for(c=sym->components; c; c=c->next) {
    if (c->pointer) return 1;

    if (c->ts.type == BT_DERIVED && derived_pointer(c->ts.derived)) return 1;
  }

  return 0;
}


/* resolve_branch()-- Given a branch to a label and a namespace, if
 * the branch is conforming.  The code node described where the branch
 * is located. */

static void resolve_branch(int label, g95_code *code, g95_namespace *ns) {
g95_code *block, *found;
code_stack *stack;
g95_st_label *lp;

  if (label == 0) return;

  /* Step one: find the label in this namespace */

  for(lp=ns->st_labels; lp; lp=lp->next)
    if (lp->label == label) break; /* always in list */

  /* Step two: is this a valid branching target? */

  if (lp->defined == ST_LABEL_UNKNOWN) {
    g95_error("Label %d referenced at %L is never defined", lp->label,
	      &lp->where);
    return;
  }

  if (lp->defined != ST_LABEL_TARGET) {
    g95_error("Statement at %L is not a valid branch target statement "
	      "for the branch statement at %L", &lp->where, &code->loc);
    return;
  }

  /* Step three: make sure this branch is not a branch to itself ;-) */

  if (code->here == label) {
    g95_warning("Branch at %L causes an infinite loop", &code->loc);
    return;
  }

  /* Step four: Try to find the label in the parse tree. To do this,
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

  /* Step five: Make sure that the branching target is legal if
   * the statement is an END {SELECT,DO,IF}. */

  if (found->op == EXEC_NOP) {
    for(stack=cs_base; stack; stack=stack->prev)
      if (stack->current->next == found) break;

    if (stack == NULL)
      g95_error("GOTO at %L cannot jump to END of construct at %L",
                &found->loc, &code->loc);
  }
} 


/* resolve_blocks()-- Resolve lists of blocks found in IF, SELECT CASE,
 * WHERE, FORALL and DO code nodes. */

static void resolve_code(g95_code *, g95_namespace *);

static void resolve_blocks(g95_code *b, g95_namespace *ns) {
try t;

  for(; b; b=b->block) {
    t = g95_resolve_expr(b->expr);
    if (g95_resolve_expr(b->expr2) == FAILURE) t = FAILURE;

    switch(b->op) {
    case EXEC_IF:
      if (t == SUCCESS && b->expr != NULL &&
	  b->expr->ts.type != BT_LOGICAL)
	g95_error("ELSE IF clause at %L requires a LOGICAL expression",
		  &b->expr->where);
      break;

    case EXEC_WHERE:
      if (t == SUCCESS && b->expr != NULL &&
	  (b->expr->ts.type != BT_LOGICAL || b->expr->rank == 0))
	g95_error("WHERE/ELSEWHERE clause at %L requires a LOGICAL array",
		  &b->expr->where);
      break;

    case EXEC_SELECT:  case EXEC_FORALL:  case EXEC_DO:  case EXEC_DO_WHILE:
      break;

    default:
      g95_internal_error("resolve_block(): Bad block type");
    }

    resolve_code(b->next, ns);
  }
}


/* resolve_code()-- Given a block of code, recursively resolve
 * everything pointed to by this code block */

void resolve_code(g95_code *code, g95_namespace *ns) {
code_stack frame;
g95_alloc *a;
try t;

  frame.prev = cs_base;
  frame.head = code;
  cs_base = &frame;

  for(; code; code=code->next) {
    frame.current = code; 

    if (code->op != EXEC_WHERE || code->expr != NULL)
      resolve_blocks(code->block, ns);

    t = g95_resolve_expr(code->expr);
    if (g95_resolve_expr(code->expr2) == FAILURE) t = FAILURE;

    switch(code->op) {
    case EXEC_NOP:       case EXEC_CYCLE:     case EXEC_IOLENGTH:
    case EXEC_STOP:      case EXEC_EXIT:      case EXEC_CONTINUE:
    case EXEC_WHERE:
      break;

    case EXEC_GOTO:
      resolve_branch(code->label, code, ns);
      break;

    case EXEC_RETURN:
      if (code->expr != NULL && code->expr->ts.type != BT_INTEGER)
	g95_error("Alternate RETURN statement at %L requires an INTEGER "
		  "return specifier", &code->expr->where);
      break;

    case EXEC_ASSIGN:
      if (t == FAILURE) break;

      if (g95_extend_assign(code, ns) == SUCCESS) break;

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

      g95_check_pointer_assign(code->expr, code->expr2);
      break;

    case EXEC_ARITHMETIC_IF:
      if (t == SUCCESS && code->expr->ts.type != BT_INTEGER &&
	  code->expr->ts.type != BT_REAL)
	g95_error("Arithmetic IF statement at %L requires a numeric "
		  "expression", &code->expr->where);

      resolve_branch(code->label, code, ns);
      resolve_branch(code->label2, code, ns);
      resolve_branch(code->label3, code, ns);
      break;

    case EXEC_IF:
      if (t == SUCCESS && code->expr != NULL &&
	  code->expr->ts.type != BT_LOGICAL)
	g95_error("IF clause at %L requires a LOGICAL expression",
		  &code->expr->where);
      break;

    case EXEC_CALL:
      resolve_call(code);
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
      if (g95_resolve_open(code->ext.open) == FAILURE) break;

      resolve_branch(code->ext.open->err, code, ns);
      break;

    case EXEC_CLOSE:
      if (g95_resolve_close(code->ext.close) == FAILURE) break;

      resolve_branch(code->ext.close->err, code, ns);
      break;

    case EXEC_BACKSPACE:
    case EXEC_ENDFILE:
    case EXEC_REWIND:
      if (g95_resolve_filepos(code->ext.filepos) == FAILURE) break;

      resolve_branch(code->ext.filepos->err, code, ns);
      break;

    case EXEC_INQUIRE:
      if (g95_resolve_inquire(code->ext.inquire) == FAILURE) break;

      resolve_branch(code->ext.inquire->err, code, ns);
      break;

    case EXEC_READ:
    case EXEC_WRITE:
      if (g95_resolve_dt(code->ext.dt) == FAILURE) break;

      resolve_branch(code->ext.dt->err, code, ns);
      resolve_branch(code->ext.dt->end, code, ns);
      resolve_branch(code->ext.dt->eor, code, ns);
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


/* resolve_values()-- Resolve initial values and make sure they are
 * compatible with the variable */

static void resolve_values(g95_symbol *sym) {

  if (sym->value == NULL) return;

  if (g95_resolve_expr(sym->value) == FAILURE) return;

  g95_check_assign_symbol(sym, sym->value);
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



/************* Resolve DATA statements *************/

static struct {
  g95_data_value *vnode;
  int left;
} values;


/* next_data_value()-- Advance the values structure to point to the
 * next value in the data list */

static try next_data_value(void) {

  while(values.left == 0) {
    if (values.vnode->next == NULL) return FAILURE;

    values.vnode = values.vnode->next;
    values.left = values.vnode->repeat;
  }

  values.left--;
  return SUCCESS;
}


static try check_data_variable(g95_data_variable *var, locus *where) {
g95_expr *e;
mpz_t size;
try t;

  if (g95_resolve_expr(var->expr) == FAILURE) return FAILURE;

  e = var->expr;

  if (e->expr_type != EXPR_VARIABLE)
    g95_internal_error("check_data_variable(): Bad expression");

  if (e->rank == 0)
    mpz_init_set_ui(size, 1);
  else {
    if (g95_array_size(e, &size) == FAILURE) {
      g95_error("Nonconstant array section at %L in DATA statement",
		&e->where);
      return FAILURE;
    }
  }

  t = SUCCESS;

  while(mpz_cmp_ui(size, 0) > 0) {
    if (next_data_value() == FAILURE) {
      g95_error("DATA statement at %L has more variables than values", where);
      t = FAILURE;
      break;
    }

    t = g95_check_assign(var->expr, values.vnode->expr, 0);
    if (t == FAILURE) break;
      
    mpz_sub_ui(size, size, 1);
  }

  mpz_clear(size);

  return t;
}


static try traverse_data_var(g95_data_variable *, locus *);

/* traverse_data_list()-- Iterate over a list of elements in a DATA
 * statement */

static try traverse_data_list(g95_data_variable *var, locus *where) {
mpz_t trip;

  mpz_init_set(trip, var->iter.end->value.integer);
  mpz_sub(trip, trip, var->iter.start->value.integer);
  mpz_add(trip, trip, var->iter.step->value.integer);

  mpz_div(trip, trip, var->iter.step->value.integer);

  while(mpz_cmp_ui(trip, 0) > 0) {
    if (traverse_data_var(var->list, where) == FAILURE) return FAILURE;

    mpz_sub_ui(trip, trip, 1);
  }

  mpz_clear(trip);

  return SUCCESS;
}


/* traverse_data_var()-- Type resolve variables in the variable list
 * of a DATA statement. */

static try traverse_data_var(g95_data_variable *var, locus *where) {
try t;

  for(; var; var=var->next) {
    if (var->expr == NULL)
      t = traverse_data_list(var, where);
    else
      t = check_data_variable(var, where);

    if (t == FAILURE) return FAILURE;
  }

  return SUCCESS;
}


/* resolve_data_variables()-- Resolve the expressions and iterators
 * associated with a data statement.  This is separate from the
 * assignment checking because data lists only should be resolved
 * once. */

static try resolve_data_variables(g95_data_variable *d) {

  for(; d; d=d->next) {
    if (d->list == NULL) {
      if (g95_resolve_expr(d->expr) == FAILURE) return FAILURE;
    } else {
      if (g95_resolve_iterator(&d->iter) == FAILURE) return FAILURE;

      if (d->iter.start->expr_type != EXPR_CONSTANT ||
	  d->iter.end->expr_type != EXPR_CONSTANT ||
	  d->iter.step->expr_type != EXPR_CONSTANT)
	g95_internal_error("resolve_data_variables(): Bad iterator");
	
      if (resolve_data_variables(d->list) == FAILURE) return FAILURE;
    }
  }

  return SUCCESS; 
}

/* resolve_data()-- Resolve a single DATA statement.  We implement
 * this by storing a pointer to the value list into static variables,
 * and then recursively traversing the variables list, expanding
 * iterators and such.  */

static void resolve_data(g95_data *d) {

  if (resolve_data_variables(d->var) == FAILURE) return;

  values.vnode = d->value;
  values.left = (d->value == NULL) ? 0 : d->value->repeat;

  if (traverse_data_var(d->var, &d->where) == FAILURE) return;

  /* At this point, we better not have any values left */

  if (next_data_value() == SUCCESS)
    g95_error("DATA statement at %L has more values than variables",
	      &d->where);
}


/* g95_impure_variable()-- Determines if a variable is not 'pure', ie
 * not assignable within a pure procedure.  Returns zero if assignment
 * is OK, nonzero if there is a problem. */

int g95_impure_variable(g95_symbol *sym) {

  if (sym->attr.use_assoc || sym->attr.in_common) return 1;

  if (sym->ns != g95_current_ns) return !sym->attr.function;

  /* TODO: Check storage association through EQUIVALENCE statements */

  return 0;
}


/* g95_pure()-- Test whether a symbol is pure or not.  For a NULL
 * pointer, checks the symbol of the current procedure. */

int g95_pure(g95_symbol *sym) {
symbol_attribute attr;

  if (sym == NULL) sym = g95_current_ns->proc_name;
  if (sym == NULL) return 0;

  attr = sym->attr;

  return attr.flavor == FL_PROCEDURE && (attr.pure || attr.elemental);
}


/* g95_elemental()-- Test whether the current procedure is elemental or not */

int g95_elemental(g95_symbol *sym) {
symbol_attribute attr;

  if (sym == NULL) sym = g95_current_ns->proc_name;
  if (sym == NULL) return 0;
  attr = sym->attr;

  return attr.flavor == FL_PROCEDURE && attr.elemental;
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
g95_data *d;

  old_ns = g95_current_ns;
  g95_current_ns = ns;

  resolve_contained_functions(ns);

  g95_traverse_ns(ns, resolve_symbol);

  for(n=ns->contained; n; n=n->sibling) {
    if (g95_pure(ns->proc_name) && !g95_pure(n->proc_name))
      g95_error("Contained procedure '%s' at %L of a PURE procedure must "
		"also be PURE", n->proc_name->name,
		&n->proc_name->declared_at);

    g95_resolve(n);
  }

  g95_check_interfaces(ns);

  for(cl=ns->cl_list; cl; cl=cl->next) {
    if (cl->length == NULL || g95_resolve_expr(cl->length) == FAILURE)
      continue;

    if (cl->length->ts.type != BT_INTEGER)
      g95_error("Character length specification at %L must be of type INTEGER",
		&cl->length->where);
  }

  g95_traverse_ns(ns, resolve_values);

  if (ns->save_all) g95_save_all(ns);

  for(d=ns->data; d; d=d->next)
    resolve_data(d);

  cs_base = NULL;
  resolve_code(ns->code, ns);

  g95_current_ns = old_ns;
}
