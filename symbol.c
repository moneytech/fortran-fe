/* Symbol handling
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

/* symbol.c-- Maintains binary trees of symbols */

#include <string.h>
#include <stdio.h>
#include <ctype.h>
#include <stdlib.h>

#include "g95.h"

g95_namespace *g95_current_ns;

static g95_symbol *changed_syms = NULL;
static g95_symtree *changed_st = NULL;


/*********** IMPLICIT NONE and IMPLICIT statement handlers ***********/

/* The following static variables hold the default types set by
 * IMPLICIT statements.  We have to store kind information because of
 * IMPLICIT DOUBLE PRECISION statements.  IMPLICIT NONE stores a
 * BT_UNKNOWN into all elements.  The arrays of flags indicate whether
 * a particular element has been explicitly set or not.  */

static g95_typespec new_ts[G95_LETTERS];
static int new_flag[G95_LETTERS];


/* g95_match_implicit_none()-- Match an IMPLICIT NONE statement. */

match g95_match_implicit_none(void) {

  return (g95_match_eos() == MATCH_YES) ? MATCH_YES : MATCH_NO;
}


/* g95_set_implicit_none()-- Handle a correctly parsed IMPLICIT NONE */

void g95_set_implicit_none(void) {
int i;

  for(i='a'; i<='z'; i++) {
    g95_clear_ts(&g95_current_ns->default_type[i - 'a']);
    g95_current_ns->set_flag[i - 'a'] = 1;
  }
}


/* match_implicit_range()-- Match the letter range(s) of an IMPLICIT
 * statement.  Sets flags in new_flag[] and copies the typespec to
 * new_ts[]. */

static match match_implicit_range(g95_typespec *ts) {
int c, i, c1, c2, inner;
locus cur_loc;

  cur_loc = *g95_current_locus(); 

  g95_gobble_whitespace();
  c = g95_next_char();
  if (c != '(') {
    g95_error("Missing character range in IMPLICIT at %C");
    goto bad;
  }

  inner = 1;
  while(inner) {
    g95_gobble_whitespace();
    c1 = g95_next_char();
    if (!isalpha(c1)) goto bad;

    g95_gobble_whitespace();
    c = g95_next_char();

    switch(c) {
    case ')':
      inner = 0;   /* Fall through */

    case ',':
      c2 = c1;
      break;

    case '-':
      g95_gobble_whitespace();
      c2 = g95_next_char();
      if (!isalpha(c2)) goto bad;

      g95_gobble_whitespace();
      c = g95_next_char();

      if ((c != ',') && (c != ')')) goto bad;
      if (c == ')') inner = 0;

      break;

    default: goto bad;
    }

    if (c1 > c2) {
      g95_error("Letters must be in alphabetic order in IMPLICIT statement "
		"at %C");
      goto bad;
    }

    c1 -= 'a'; 
    c2 -= 'a';

    for(i=c1; i<=c2; i++) {
      if (new_flag[i]) {
	g95_error("Letter '%c' already set in IMPLICIT statement at %C",
		  i+'A');
	goto bad;
      }

      new_ts[i] = *ts;
      new_flag[i] = 1;
    }
  }

  return MATCH_YES;

 bad:
  g95_syntax_error(ST_IMPLICIT);

  g95_set_locus(&cur_loc);
  return MATCH_ERROR;
}


/* g95_match_implicit()-- Match an IMPLICIT statement, storing the
 * types for g95_set_implicit() if the statement is accepted by the
 * parser.  There is a strange looking, but legal syntactic
 * construction possible.  It looks like
 *                  IMPLICIT INTEGER (a-b) (c-d)
 *
 * This is legal if "a-b" is a constant expression that happens to
 * equal one of the legal kinds for integers.  The real problem
 * happens with an implicit specification that looks like
 *                  IMPLICIT INTEGER (a-b)
 *
 * In this case, a typespec matcher that is "greedy" (as most of the
 * matchers are) gobbles the character range as a kindspec, leaving
 * nothing left.  We therefore have to go a bit more slowly in the
 * matching process by inhibiting the kindspec checking during
 * typespec matching and checking for a kind later. */

match g95_match_implicit(void) {
g95_typespec ts;
locus cur_loc;
int c, i;
match m;

  for(i=0; i<G95_LETTERS; i++) {
    g95_clear_ts(&new_ts[i]);
    if (new_flag[i]) new_flag[i] = 0;
  }

  if (g95_match_eos() == MATCH_YES) {
    g95_error("Empty IMPLICIT statement at %C");
    return MATCH_ERROR;
  }

  do {
    m = g95_match_type_spec(&ts, 0, 0); /* A basic type is mandatory here */
    if (m == MATCH_ERROR) goto error;
    if (m == MATCH_NO) goto syntax;

    cur_loc = *g95_current_locus();
    m = match_implicit_range(&ts);

    if (m == MATCH_YES) {   /* Looks like we have the <TYPE> (<RANGE>) */
      g95_gobble_whitespace();
      c = g95_next_char();
      if ((c == '\n') || (c == ',')) continue;

      g95_set_locus(&cur_loc);
    }

    /* Last chance-- check <TYPE> (<KIND>) (<RANGE>) */

    m = g95_match_kind_spec(&ts);
    if (m == MATCH_ERROR) goto error;
    if (m == MATCH_NO) {
      m = g95_match_old_kind_spec(&ts);
      if (m == MATCH_ERROR) goto error;
      if (m == MATCH_NO) goto syntax;
    }

    m = match_implicit_range(&ts);
    if (m == MATCH_ERROR) goto error;
    if (m == MATCH_NO) goto syntax;
    
    g95_gobble_whitespace();
    c = g95_next_char();
    if ((c != '\n') && (c != ',')) goto syntax;

  } while(c == ',');

/* An implicit statement has been fully matched at this point.  Now
 * check to see if merging the new implicit types back into the
 * existing types will work. */

  for(i=0; i<G95_LETTERS; i++)
    if (new_flag[i]) {
      if (g95_current_ns->set_flag[i]) {
	g95_error("Letter %c already has an IMPLICIT type at %C", i+'A');
	goto error;
      }
    }

  return MATCH_YES;

syntax:
  g95_syntax_error(ST_IMPLICIT);

error:
  return MATCH_ERROR;
}


/* g95_set_implicit()-- Sets the implicit types parsed by the previous
 * g95_match_implicit(). */

void g95_set_implicit(void) {
int i;

  for(i=0; i<G95_LETTERS; i++)
    if (new_flag[i]) {
      g95_current_ns->default_type[i] = new_ts[i];
      g95_current_ns->set_flag[i] = 1;
    }
}


/* g95_get_default_type()-- Given a symbol, return a pointer to the
 * typespec for it's default type */

g95_typespec *g95_get_default_type(g95_symbol *sym, g95_namespace *ns) {
char letter;

  letter = sym->name[0];
  if (letter < 'a' || letter > 'z')
    g95_internal_error("g95_get_default_type(): Bad symbol");

  if (ns == NULL) ns = g95_current_ns;

  return &ns->default_type[letter - 'a'];
}


/* g95_set_default_type()-- Given a pointer to a symbol, set its type
 * according to the first letter of its name.  Fails if the letter in
 * question has no default type. */

try g95_set_default_type(g95_symbol *sym, int error_flag, g95_namespace *ns) {
g95_typespec *ts;

  if (sym->ts.type != BT_UNKNOWN)
    g95_internal_error("g95_set_default_type(): symbol already has a type");

  ts = g95_get_default_type(sym, ns);

  if (ts->type == BT_UNKNOWN) {
    if (error_flag)
      g95_error("Symbol '%s' at %L has no IMPLICIT type", sym->name,
		&sym->declared_at);

    return FAILURE;
  }

  sym->ts = *ts;
  sym->attr.implicit_type = 1;

  return SUCCESS;
}


/* g95_check_assign()-- Given an assignable expression and an
 * arbitrary expression, make sure that the assignment can take
 * place. */

try g95_check_assign(g95_expr *lvalue, g95_expr *rvalue, int conform) {
mpz_t lsize, rsize;
int lflag, rflag;
g95_symbol *sym;
try t;

  sym = lvalue->symbol;

  if (sym->attr.intent == INTENT_IN) {
    g95_error("Can't assign to INTENT(IN) variable '%s' at %L",
	      sym->name, &lvalue->where);
    return FAILURE;
  }

  if (rvalue->rank != 0 && lvalue->rank != rvalue->rank) {
    g95_error("Incompatible ranks in assignment at %L", &lvalue->where);
    return FAILURE;
  }

  if (lvalue->ts.type == BT_UNKNOWN) {
    g95_error("Variable type is UNKNOWN in assignment at %L", &lvalue->where);
    return FAILURE;
  }

  /* Check size of array assignments */

  if (lvalue->rank != 0 && rvalue->rank != 0) {
    t = SUCCESS;

    lflag = g95_array_size(lvalue, &lsize) == SUCCESS;
    rflag = g95_array_size(rvalue, &rsize) == SUCCESS;

    if (lflag && rflag && mpz_cmp(lsize, rsize) < 0) {

      g95_error("Array assignment at %L has more values on the right "
		"than left", &rvalue->where);

      t = FAILURE;
    }

    if (lflag) mpz_clear(lsize);
    if (rflag) mpz_clear(rsize);

    if (t == FAILURE) return FAILURE;
  }

  if (g95_compare_types(&lvalue->ts, &rvalue->ts)) return SUCCESS;

  if (!conform) {
    if (g95_numeric_ts(&lvalue->ts) && g95_numeric_ts(&rvalue->ts))
      return SUCCESS;

    g95_error("Incompatible types in assignment at %L, %s to %s",
	      &rvalue->where, g95_typename(&rvalue->ts),
	      g95_typename(&lvalue->ts));

    return FAILURE;
  }

  return g95_convert_type(rvalue, &lvalue->ts, 1);
}


/* g95_check_pointer_assign()-- Check that a pointer assignment is OK.
 * We first check lvalue, and we only check rvalue if it's not an
 * assignment to NULL() or a NULLIFY statement. */

try g95_check_pointer_assign(g95_expr *lvalue, g95_expr *rvalue) {
symbol_attribute attr;
int is_pure;

  if (lvalue->symbol->ts.type == BT_UNKNOWN) {
    g95_error("Pointer assignment target is not a POINTER at %L",
	      &lvalue->where);
    return FAILURE;
  }

  attr = g95_variable_attr(lvalue, NULL);
  if (!attr.pointer) {
    g95_error("Pointer assignment to non-POINTER at %L",
	      &lvalue->where);
    return FAILURE;
  }

  is_pure = g95_pure(NULL);

  if (is_pure && g95_impure_variable(lvalue->symbol)) {
    g95_error("Bad pointer object in PURE procedure at %L", &lvalue->where);
    return FAILURE;
  }

  /* If rvalue is a NULL() or NULLIFY, we're done. Otherwise the type,
   * kind, etc for lvalue and rvalue must match, and rvalue must be a 
   * pure variable if we're in a pure function. */

  if (rvalue->expr_type != EXPR_NULL) {

    if (lvalue->ts.type != rvalue->ts.type) {
      g95_error("Different types in pointer assignment at %L",
                &lvalue->where);
      return FAILURE;
    }

    if (lvalue->ts.type == BT_DERIVED
	&& lvalue->ts.derived != rvalue->ts.derived) {
      g95_error("Incompatible derived types in pointer assignment at %L",
                &lvalue->where);
      return FAILURE;
    }

    if (lvalue->ts.kind != rvalue->ts.kind) {
      g95_error("Different kind type parameters in pointer assignment at %L",
                &lvalue->where);
      return FAILURE;
    }

    attr = g95_expr_attr(rvalue);
    if (!attr.target && !attr.pointer) {
      g95_error("Pointer assignment target is neither TARGET nor POINTER at "
		"%L", &rvalue->where);
      return FAILURE;
    }

    if (is_pure && g95_impure_variable(rvalue->symbol)) {
      g95_error("Bad target in pointer assignment in PURE procedure at %L",
		&rvalue->where);
    }
  }

  return SUCCESS;
}


/* g95_check_assign_symbol()-- Relative of g95_check_assign() except
 * that the lvalue is a single symbol. */

try g95_check_assign_symbol(g95_symbol *sym, g95_expr *rvalue) {
g95_expr lvalue;

  memset(&lvalue, '\0', sizeof(g95_expr));

  lvalue.expr_type = EXPR_VARIABLE;
  lvalue.ts = sym->ts;
  if (sym->as) lvalue.rank = sym->as->rank;
  lvalue.symbol = sym;
  lvalue.where = sym->declared_at;

  return g95_check_assign(&lvalue, rvalue, 1);
}

/******************** Symbol attribute stuff *********************/

/* Get rid of this copy, use version in modules.c only */

static mstring flavors[] = {
  minit("UNKNOWN-FL",  FL_UNKNOWN),      minit("PROGRAM",     FL_PROGRAM),
  minit("BLOCK-DATA",  FL_BLOCK_DATA),   minit("MODULE",      FL_MODULE),
  minit("VARIABLE",    FL_VARIABLE),     minit("PARAMETER",   FL_PARAMETER),
  minit("LABEL",       FL_LABEL),        minit("PROCEDURE",   FL_PROCEDURE),
  minit("DERIVED",     FL_DERIVED),      minit("NAMELIST",    FL_NAMELIST),
  minit(NULL, -1)
},

procedures[] = {
  minit("UNKNOWN-PROC",   PROC_UNKNOWN),     minit("MODULE-PROC", PROC_MODULE),
  minit("INTERNAL-PROC",  PROC_INTERNAL),    minit("DUMMY-PROC",  PROC_DUMMY),
  minit("INTRINSIC-PROC", PROC_INTRINSIC),
  minit("EXTERNAL-PROC",  PROC_EXTERNAL),
  minit("STATEMENT-PROC", PROC_ST_FUNCTION), minit(NULL, -1)
},

accessibility[] = {
  minit("UNKNOWN-ACCESS", ACCESS_UNKNOWN),   minit("PUBLIC", ACCESS_PUBLIC),
  minit("PRIVATE", ACCESS_PRIVATE),          minit(NULL, -1)
};



/* g95_show_attr()-- Show symbol attributes.  The flavor and intent
 * are followed by whatever single bit attributes are present */

void g95_show_attr(symbol_attribute *attr) {

  g95_status("(%s %s %s %s", g95_code2string(flavors, attr->flavor),
	     g95_intent_string(attr->intent),
	     g95_code2string(accessibility, attr->access),
	     g95_code2string(procedures, attr->proc));

  if (attr->allocatable)    g95_status(" ALLOCATABLE");
  if (attr->dimension)      g95_status(" DIMENSION");
  if (attr->external)       g95_status(" EXTERNAL");
  if (attr->intrinsic)      g95_status(" INTRINSIC");
  if (attr->optional)       g95_status(" OPTIONAL");
  if (attr->pointer)        g95_status(" POINTER");
  if (attr->save)           g95_status(" SAVE");
  if (attr->target)         g95_status(" TARGET");
  if (attr->dummy)          g95_status(" DUMMY");
  if (attr->common)         g95_status(" COMMON");
  if (attr->result)         g95_status(" RESULT");
  if (attr->entry)          g95_status(" ENTRY");

  if (attr->data)           g95_status(" DATA");
  if (attr->use_assoc)      g95_status(" USE-ASSOC");
  if (attr->in_namelist)    g95_status(" IN-NAMELIST");
  if (attr->in_common)      g95_status(" IN-COMMON");
  if (attr->saved_common)   g95_status(" SAVED-COMMON");

  if (attr->function)       g95_status(" FUNCTION");
  if (attr->subroutine)     g95_status(" SUBROUTINE");
  if (attr->implicit_type)  g95_status(" IMPLICIT-TYPE");

  if (attr->sequence)       g95_status(" SEQUENCE");
  if (attr->elemental)      g95_status(" ELEMENTAL");
  if (attr->pure)           g95_status(" PURE");
  if (attr->recursive)      g95_status(" RECURSIVE");

  g95_status(")");
}


/* check_conflict()-- This is a generic conflict-checker.  We do this
 * to avoid having a single conflict in two places. */

#define conf(a, b) if (attr->a && attr->b) { a1 = a; a2 = b; goto conflict; }
#define conf2(a) if (attr->a) { a2 = a; goto conflict; }

static try check_conflict(symbol_attribute *attr, locus *loc) {
char *a1, *a2;

static char *dummy = "DUMMY", *save = "SAVE", *pointer = "POINTER",
  *target = "TARGET", *external = "EXTERNAL", *intent = "INTENT",
  *intrinsic = "INTRINSIC", *allocatable = "ALLOCATABLE",
  *elemental = "ELEMENTAL", *private = "PRIVATE", *recursive = "RECURSIVE",
  *in_common = "COMMON", *result = "RESULT", *in_namelist = "NAMELIST",
  *public = "PUBLIC", *optional = "OPTIONAL", *entry = "ENTRY",
  *function = "FUNCTION", *subroutine = "SUBROUTINE";

  if (loc == NULL) loc = g95_current_locus();

  if (attr->pointer && attr->intent != INTENT_UNKNOWN) {
    a1 = pointer; a2 = intent; goto conflict; }

/* Check for attributes not allowed in a BLOCK DATA */

  if (g95_current_state() == COMP_BLOCK_DATA) {
    a1 = NULL;

    if (attr->allocatable) a1 = allocatable;
    if (attr->external) a1 = external;
    if (attr->optional) a1 = optional;
    if (attr->access == ACCESS_PRIVATE) a1 = private;
    if (attr->access == ACCESS_PUBLIC) a1 = public;
    if (attr->intent != INTENT_UNKNOWN) a1 = intent;

    if (a1 != NULL) {
      g95_error("%s attribute not allowed in BLOCK DATA program unit at %L",
		a1, loc);
      return FAILURE;
    }
  }

  conf(dummy, save);
  conf(pointer, target);
  conf(pointer, external);
  conf(pointer, intrinsic);
  conf(target, external);
  conf(target, intrinsic);

  conf(external, intrinsic);
  conf(allocatable, pointer);
  conf(elemental, recursive);

  conf(in_common, dummy);
  conf(in_common, allocatable);
  conf(in_common, result);
  conf(dummy, result);

  conf(in_namelist, pointer);
  conf(in_namelist, allocatable);

  conf(entry, result);

  conf(function, subroutine);

  a1 = g95_code2string(flavors, attr->flavor);

  if (attr->in_namelist && attr->flavor != FL_VARIABLE &&
      attr->flavor != FL_UNKNOWN) {

    a2 = in_namelist;
    goto conflict;
  }

  switch(attr->flavor) {
  case FL_PROGRAM: case FL_BLOCK_DATA: case FL_MODULE: case FL_LABEL:
    conf2(dummy);         conf2(save);        conf2(pointer);
    conf2(target);        conf2(external);    conf2(intrinsic);
    conf2(allocatable);   conf2(result);      conf2(in_namelist);
    conf2(optional);      conf2(function);    conf2(subroutine);
    break;

  case FL_VARIABLE:
  case FL_NAMELIST:
    break;

  case FL_PROCEDURE:
    switch(attr->proc) {
    case PROC_ST_FUNCTION:
      conf2(in_common);
      break;

    case PROC_MODULE:
      conf2(dummy);
      break;

    case PROC_DUMMY:
      conf2(result);
      conf2(in_common);
      conf2(save);
      break;

    default:
      break;
    }

    break;

  case FL_DERIVED:
    conf2(dummy);        conf2(save);        conf2(pointer);
    conf2(target);       conf2(external);    conf2(intrinsic);
    conf2(allocatable);  conf2(optional);    conf2(entry);
    conf2(function);     conf2(subroutine);
      
    if (attr->intent != INTENT_UNKNOWN) { a2 = intent; goto conflict; }
    break;

  case FL_PARAMETER:
    conf2(external);      conf2(intrinsic);    conf2(optional);
    conf2(allocatable);   conf2(function);     conf2(subroutine);
    conf2(entry);         conf2(pointer);      conf2(target);
    conf2(dummy);         conf2(in_common);
    break;

  default:
    break;
  }

  return SUCCESS;

conflict:
  g95_error("%s attribute conflicts with %s attribute at %L", a1, a2, loc);
  return FAILURE;
}

#undef conf
#undef conf2


/* check_used()-- Common subroutine called by attribute
 * changing subroutine in order to prevent them from changing a symbol
 * that has been use-associated.  Returns zero if it is OK to change
 * the symbol, nonzero if not.  */

static int check_used(symbol_attribute *attr, locus *loc) {

  if (attr->use_assoc == 0) return 0;

  if (loc == NULL) loc = g95_current_locus();

  g95_error("Cannot change attributes of USE-associated symbol at %L", loc);

  return 1;
}


try g95_add_allocatable(symbol_attribute *attr, locus *loc) {

  if (check_used(attr, loc)) return FAILURE;

  attr->allocatable = 1;
  return check_conflict(attr, loc);
}

try g95_add_dimension(symbol_attribute *attr, locus *loc) {

  if (check_used(attr, loc)) return FAILURE;

  attr->dimension = 1;
  return check_conflict(attr, loc);
}

try g95_add_external(symbol_attribute *attr, locus *loc) {

  if (check_used(attr, loc)) return FAILURE;

  attr->external = 1;

  return check_conflict(attr, loc);
}

try g95_add_intrinsic(symbol_attribute *attr, locus *loc) {

  if (check_used(attr, loc)) return FAILURE;

  attr->intrinsic = 1;

  return check_conflict(attr, loc);
}

try g95_add_optional(symbol_attribute *attr, locus *loc) {

  if (check_used(attr, loc)) return FAILURE;

  attr->optional = 1;
  return check_conflict(attr, loc);
}

try g95_add_pointer(symbol_attribute *attr, locus *loc) {

  if (check_used(attr, loc)) return FAILURE;

  attr->pointer = 1;
  return check_conflict(attr, loc);
}

try g95_add_result(symbol_attribute *attr, locus *loc) {

  if (check_used(attr, loc)) return FAILURE;

  attr->result = 1;
  return check_conflict(attr, loc);
}

try g95_add_save(symbol_attribute *attr, locus *loc) {

  if (check_used(attr, loc)) return FAILURE;

  if (g95_pure(NULL)) {
    g95_error("Symbol at %L cannot be SAVEd in a PURE procedure", loc);
    return FAILURE;
  }

  attr->save = 1;
  return check_conflict(attr, loc);
}

try g95_add_saved_common(symbol_attribute *attr, locus *loc) {

  if (check_used(attr, loc)) return FAILURE;

  attr->saved_common = 1;
  return check_conflict(attr, loc);
}

try g95_add_target(symbol_attribute *attr, locus *loc) {

  if (check_used(attr, loc)) return FAILURE;

  attr->target = 1;
  return check_conflict(attr, loc);
}

try g95_add_dummy(symbol_attribute *attr, locus *loc) {

  if (check_used(attr, loc)) return FAILURE;

  attr->dummy = 1;
  return check_conflict(attr, loc);
}

try g95_add_data(symbol_attribute *attr, locus *loc) {

  if (check_used(attr, loc)) return FAILURE;

  attr->data = 1;
  return check_conflict(attr, loc);
}

try g95_add_common(symbol_attribute *attr, locus *loc) {

  if (check_used(attr, loc)) return FAILURE;

  attr->common = 1;
  return check_conflict(attr, loc);
}

try g95_add_in_common(symbol_attribute *attr, locus *loc) {

  if (check_used(attr, loc)) return FAILURE;

  attr->in_common = 1;
  if (check_conflict(attr, loc) == FAILURE) return FAILURE;

  if (attr->flavor == FL_VARIABLE) return SUCCESS;

  return g95_add_flavor(attr, FL_VARIABLE, loc);
}

try g95_add_in_namelist(symbol_attribute *attr, locus *loc) {

  attr->in_namelist = 1;
  return check_conflict(attr, loc);
}

try g95_add_sequence(symbol_attribute *attr, locus *loc) {

  if (check_used(attr, loc)) return FAILURE;

  attr->sequence = 1;
  return check_conflict(attr, loc);
}

try g95_add_elemental(symbol_attribute *attr, locus *loc) {

  if (check_used(attr, loc)) return FAILURE;

  attr->elemental = 1;
  return check_conflict(attr, loc);
}

try g95_add_pure(symbol_attribute *attr, locus *loc) {

  if (check_used(attr, loc)) return FAILURE;

  attr->pure = 1;
  return check_conflict(attr, loc);
}

try g95_add_recursive(symbol_attribute *attr, locus *loc) {

  if (check_used(attr, loc)) return FAILURE;

  attr->recursive = 1;
  return check_conflict(attr, loc);
}

try g95_add_entry(symbol_attribute *attr, locus *loc) {

  if (check_used(attr, loc)) return FAILURE;

  attr->entry = 1;
  return check_conflict(attr, loc);
}

try g95_add_function(symbol_attribute *attr, locus *loc) {

  if (attr->flavor != FL_PROCEDURE &&
      g95_add_flavor(attr, FL_PROCEDURE, loc) == FAILURE) return FAILURE;

  attr->function = 1;
  return check_conflict(attr, loc);
}

try g95_add_subroutine(symbol_attribute *attr, locus *loc) {

  if (attr->flavor != FL_PROCEDURE &&
      g95_add_flavor(attr, FL_PROCEDURE, loc) == FAILURE) return FAILURE;

  attr->subroutine = 1;
  return check_conflict(attr, loc);
}

try g95_add_generic(symbol_attribute *attr, locus *loc) {

  if (attr->flavor != FL_PROCEDURE &&
      g95_add_flavor(attr, FL_PROCEDURE, loc) == FAILURE) return FAILURE;

  attr->generic = 1;
  return check_conflict(attr, loc);
}


/* g95_add_flavor()-- Flavors are special because some flavors are not
 * what fortran considers attributes and can be reaffirmed multiple
 * times. */

try g95_add_flavor(symbol_attribute *attr, sym_flavor f, locus *loc) {

  if ((f == FL_PROGRAM || f == FL_BLOCK_DATA || f == FL_MODULE ||
       f == FL_PARAMETER || f == FL_LABEL || f == FL_DERIVED ||
       f == FL_NAMELIST) && check_used(attr, loc)) return FAILURE;

  if (attr->flavor == f && f == FL_VARIABLE) return SUCCESS;

  if (attr->flavor != FL_UNKNOWN) {
    if (loc == NULL) loc = g95_current_locus();

    g95_error("%s attribute conflicts with %s attribute at %L",
	      g95_code2string(flavors, attr->flavor),
	      g95_code2string(flavors, f), loc);

    return FAILURE;
  }

  attr->flavor = f;

  return check_conflict(attr, loc);
}


try g95_add_procedure(symbol_attribute *attr, procedure_type t, locus *loc) {

  if (check_used(attr, loc)) return FAILURE;

  if (attr->flavor != FL_PROCEDURE &&
      g95_add_flavor(attr, FL_PROCEDURE, loc) == FAILURE) return FAILURE;

  if (loc == NULL) loc = g95_current_locus();

  if (attr->proc != PROC_UNKNOWN) {
    g95_error("%s procedure at %L is already %s %s procedure",
	      g95_code2string(procedures, t), loc,
	      g95_article(g95_code2string(procedures, attr->proc)),
	      g95_code2string(procedures, attr->proc));

    return FAILURE;
  }

  attr->proc = t;

/* Statement functions are always functions */

  if (t == PROC_ST_FUNCTION && !attr->function &&
      g95_add_function(attr, loc) == FAILURE) return FAILURE;

  return check_conflict(attr, loc);
}


try g95_add_intent(symbol_attribute *attr, sym_intent intent, locus *loc) {

  if (check_used(attr, loc)) return FAILURE;

  if (attr->intent == INTENT_UNKNOWN) {
    attr->intent = intent;
    return check_conflict(attr, loc);
  }

  if (loc == NULL) loc = g95_current_locus();

  g95_error("INTENT (%s) conflicts with INTENT(%s) at %L",
	    g95_intent_string(attr->intent),
	    g95_intent_string(intent), loc);

  return FAILURE;
}

/* No checks for use-association in public and private statements */

try g95_add_access(symbol_attribute *attr, g95_access access, locus *loc) {

  if (attr->access == ACCESS_UNKNOWN) {
    attr->access = access;
    return check_conflict(attr, loc);
  }

  if (loc == NULL) loc = g95_current_locus();
  g95_error("ACCESS specification at %L was already specified", loc);

  return FAILURE;
}


try g95_add_explicit_interface(g95_symbol *sym, ifsrc source,
			       g95_formal_arglist *formal, locus *loc) {

  if (check_used(&sym->attr, loc)) return FAILURE;

  if (loc == NULL) loc = g95_current_locus();

  if (sym->attr.if_source != IFSRC_UNKNOWN &&
      sym->attr.if_source != IFSRC_DECL) {
    g95_error("Symbol '%s' at %L already has an explicit interface",
	      sym->name, loc);
    return FAILURE;
  }

  sym->formal = formal;
  sym->attr.if_source = source;

  return SUCCESS;
}


/* g95_compare_attr()-- Compares two attributes */

int g95_compare_attr(symbol_attribute *a1, symbol_attribute *a2) {

  return a1->allocatable == a2->allocatable &&
    a1->dimension == a2->dimension && a1->external == a2->external &&
    a1->intrinsic == a2->intrinsic && a1->optional == a2->optional &&
    a1->pointer == a2->pointer     && a1->save == a2->save &&
    a1->target == a2->target       && a1->dummy == a2->dummy &&
    a1->common == a2->common       && a1->result == a2->result &&
    a1->entry == a2->entry         && a1->data == a2->data &&
    a1->use_assoc == a2->use_assoc && a1->in_namelist == a2->in_namelist &&
    a1->in_common == a2->in_common && a1->saved_common == a2->saved_common && 
    a1->function == a2->function   && a1->subroutine == a2->subroutine &&
    a1->sequence == a2->sequence   && a1->elemental == a2->elemental &&
    a1->pure == a2->pure           && a1->recursive == a2->recursive &&
    a1->access == a2->access       && a1->intent == a2->intent &&
    a1->flavor == a2->flavor       && a1->proc == a2->proc &&
    a1->generic == a2->generic;
}


/* g95_clear_attr()-- Clears all attributes */

void g95_clear_attr(symbol_attribute *attr) {

  attr->allocatable = 0;
  attr->dimension = 0;
  attr->external = 0;
  attr->intrinsic = 0;
  attr->optional = 0;
  attr->pointer = 0;
  attr->save = 0;
  attr->target = 0;
  attr->dummy = 0;
  attr->common = 0;
  attr->result = 0;
  attr->entry = 0;
  attr->data = 0;
  attr->use_assoc = 0;
  attr->in_namelist = 0;
  
  attr->in_common = 0;
  attr->saved_common = 0;
  attr->function = 0;
  attr->subroutine = 0;
  attr->generic = 0;
  attr->implicit_type = 0;
  attr->sequence = 0;
  attr->elemental = 0;
  attr->pure = 0;
  attr->recursive = 0;

  attr->access = ACCESS_UNKNOWN;
  attr->intent = INTENT_UNKNOWN;
  attr->flavor = FL_UNKNOWN;
  attr->proc = PROC_UNKNOWN;
  attr->if_source = IFSRC_UNKNOWN;
}


/* g95_missing_attr()-- Check for missing attributes in the new
 * symbol.  Currently does nothing, but it's not clear that it is
 * unnecessary yet.  AEV 7/4/01 */

try g95_missing_attr(symbol_attribute *attr, locus *loc) {

  return SUCCESS;
}


/* g95_copy_attr()-- copy one attribute over another, bit by bit.
 * Some attributes have a lot of side-effects but cannot be present
 * given where we are called from, so we ignore some bits */

try g95_copy_attr(symbol_attribute *dest, symbol_attribute *src, locus *loc) {

  if (src->allocatable && g95_add_allocatable(dest, loc) == FAILURE) goto fail;
  if (src->dimension && g95_add_dimension(dest, loc) == FAILURE) goto fail;
  if (src->optional && g95_add_optional(dest, loc) == FAILURE) goto fail;
  if (src->pointer && g95_add_pointer(dest, loc) == FAILURE) goto fail;
  if (src->save && g95_add_save(dest, loc) == FAILURE) goto fail;
  if (src->target && g95_add_target(dest, loc) == FAILURE) goto fail;
  if (src->dummy && g95_add_dummy(dest, loc) == FAILURE) goto fail;
  if (src->common && g95_add_common(dest, loc) == FAILURE) goto fail;
  if (src->result && g95_add_result(dest, loc) == FAILURE) goto fail;
  if (src->entry) dest->entry = 1;

  if (src->data && g95_add_data(dest, loc) == FAILURE) goto fail;
  if (src->in_namelist && g95_add_in_namelist(dest, loc) == FAILURE) goto fail;
  if (src->in_common && g95_add_in_common(dest, loc) == FAILURE) goto fail;
  if (src->saved_common && g95_add_saved_common(dest, loc)==FAILURE) goto fail;

  if (src->generic && g95_add_generic(dest, loc) == FAILURE) goto fail;
  if (src->function && g95_add_function(dest, loc) == FAILURE) goto fail;
  if (src->subroutine && g95_add_subroutine(dest, loc) == FAILURE) goto fail;

  if (src->sequence && g95_add_sequence(dest, loc) == FAILURE) goto fail;
  if (src->elemental && g95_add_elemental(dest, loc) == FAILURE) goto fail;
  if (src->pure && g95_add_pure(dest, loc) == FAILURE) goto fail;
  if (src->recursive && g95_add_recursive(dest, loc) == FAILURE) goto fail;

  if (src->flavor != FL_UNKNOWN &&
      g95_add_flavor(dest, src->flavor, loc) == FAILURE) goto fail;

  if (src->intent != INTENT_UNKNOWN &&
      g95_add_intent(dest, src->intent, loc) == FAILURE) goto fail;

  if (src->access != ACCESS_UNKNOWN &&
      g95_add_access(dest, src->access, loc) == FAILURE) goto fail;

  if (g95_missing_attr(dest, loc) == FAILURE) goto fail;

/* The subroutines that set these bits also cause flavors to be set,
 * and that has already happened in the original, so don't let to
 * happen again. */

  if (src->external) dest->external = 1;
  if (src->intrinsic) dest->intrinsic = 1;

  return SUCCESS;

fail:
  return FAILURE;
}


/************** Component name management ************/

/* Component names of a derived type form their own little namespaces
 * that are separate from all other spaces.  The space is composed of
 * a singly linked list of g95_component structures whose head is
 * located in the parent symbol. */


/* g95_add_component()-- Add a component name to a symbol.  The call
 * fails if the name is already present.  On success, the component
 * pointer is modified to point to the additional component structure. */

try g95_add_component(g95_symbol *sym, char *name,
		      g95_component **component) {
g95_component *p, *tail;

  tail = NULL;

  for(p=sym->components; p; p=p->next) {
    if (strcmp(p->name, name) == 0) {
      g95_error("Component '%s' at %C already declared at %L",
		name, &p->loc);
      return FAILURE;
    }

    tail = p;
  }

/* Allocate new component */

  p = g95_get_component();

  if (tail == NULL) sym->components = p;
  else tail->next = p;

  strcpy(p->name, name);
  p->loc = *g95_current_locus();

  *component = p;
  return SUCCESS;
}


/* switch_type()-- Recursive function to switch derived types of all
 * symbol in a namespace. */

static void switch_types(g95_symtree *st, g95_symbol *from, g95_symbol *to) {
g95_symbol *sym;

  if (st == NULL) return;

  sym = st->n.sym;
  if (sym->ts.type == BT_DERIVED && sym->ts.derived == from)
    sym->ts.derived = to;

  switch_types(st->left, from, to);
  switch_types(st->right, from, to);
}


/* g95_use_derived()-- This subroutine is called when a derived type
 * is used in order to make the final determination about which
 * version to use.  The standard requires that a type be defined
 * before it is 'used', but such types can appear in IMPLICIT
 * statements before the actual definition.  'Using' in this context
 * means declaring a variable to be that type or using the type
 * constructor.
 *
 * If a type is used and the components haven't been defined, then we
 * have to have a derived type in a parent unit.  We find the node in
 * the other namespace and point the symtree node in this namespace to
 * that node.  Further reference to this name point to the correct
 * node.  If we can't find the node in a parent namespace, then have
 * an error.
 *
 * This subroutine takes a pointer to a symbol node and returns a
 * pointer to the translated node or NULL for an error.  Usually there
 * is no translation and we return the node we were passed.  */

g95_symbol *g95_use_derived(g95_symbol *sym) {
g95_symbol *s, *p;
g95_typespec *t;
g95_symtree *st;
int i;

  if (sym->components != NULL) return sym;   /* Already defined */

  if (sym->ns->parent == NULL) goto bad;

  if (g95_find_symbol(sym->name, sym->ns->parent, 1, &s)) {
    g95_error("Symbol '%s' at %C is ambiguous", sym->name);
    return NULL;
  }

  if (s == NULL || s->attr.flavor != FL_DERIVED) goto bad;

  /* Get rid of symbol sym, translating all references to s */

  for(i=0; i<G95_LETTERS; i++) {
    t = &sym->ns->default_type[i];
    if (t->derived == sym) t->derived = s;
  }

  st = g95_find_symtree(sym->ns->sym_root, sym->name);
  st->n.sym = s;

  s->refs++;

  /* Unlink from list of modified symbols */

  if (changed_syms == sym)
    changed_syms = sym->tlink;
  else
    for(p=changed_syms; p; p=p->tlink)
      if (p->tlink == sym) {
	p->tlink = sym->tlink;
	break;
      }

  switch_types(sym->ns->sym_root, sym, s);

  /* TODO: Also have to replace sym -> s in other lists like
   * namelists, common lists and interface lists.  */

  g95_free_symbol(sym);

  return s;

 bad:
  g95_error("Derived type '%s' at %C is being used before it is defined",
	    sym->name);
  return NULL;
}


/* g95_find_component()-- Given a derived type node and a component
 * name, try to locate the component structure.  Returns the NULL
 * pointer if the component is not found or the components are
 * private. */
 
g95_component *g95_find_component(g95_symbol *sym, char *name) {
g95_component *p;

  if (name == NULL) return NULL;

  sym = g95_use_derived(sym);

  if (sym == NULL) return NULL;

  for(p=sym->components; p; p=p->next)
    if (strcmp(p->name, name) == 0) break;

  if (p == NULL)
    g95_error("'%s' at %C is not a member of the '%s' structure",
	      name, sym->name);
  else {
    if (sym->attr.use_assoc && sym->component_access == ACCESS_PRIVATE) {
      g95_error("Component '%s' at %C is a PRIVATE component of '%s'",
		name, sym->name);
      p = NULL;
    }
  }

  return p;
}


/* free_components()-- Given a symbol, free all of the component
 * structures and everything they point to. */

static void free_components(g95_component *p) {
g95_component *q;

  for(; p; p=q) {
    q = p->next;

    g95_free_array_spec(p->as);
    g95_free_expr(p->initializer);

    g95_free(p);
  }
}


void g95_show_components(g95_symbol *sym) {
g95_component *c;

  for(c=sym->components; c; c=c->next) {
    g95_status("(%s ", c->name);
    g95_show_typespec(&c->ts);
    if (c->pointer) g95_status(" POINTER");
    if (c->dimension) g95_status(" DIMENSION");
    g95_status_char(' ');
    g95_show_array_spec(c->as);
    g95_status(")");
    if (c->next != NULL) g95_status_char(' ');
  }
}


/* g95_set_component_attr()-- Set component attributes from a standard
 * symbol attribute structure. */

void g95_set_component_attr(g95_component *c, symbol_attribute *attr) {

  c->dimension = attr->dimension;
  c->pointer = attr->pointer;
}


/* g95_get_componentr_attr()-- Get a standard symbol attribute
   structure given the component structure. */

void g95_get_component_attr(symbol_attribute *attr, g95_component *c) {

  g95_clear_attr(attr);
  attr->dimension = c->dimension;
  attr->pointer = c->pointer;
}


/******************** Statement label management ********************/

/* Free a single g95_st_label structure, making sure the list is not
   messed up.  This function is called only when some parse error
   occurs.  */
void g95_free_st_label(g95_st_label *l) {

  if (l == NULL) return;

  if (l->prev)
    (l->prev->next = l->next);
 
  if (l->next)
    (l->next->prev = l->prev);

  if (l->format != NULL) g95_free(l->format);
  g95_free(l);
}

/* Free a whole list of g95_st_label structures.  */
static void free_st_labels(g95_st_label *l1) {
g95_st_label *l2;

  for(; l1; l1=l2) {
    l2 = l1->next;
    if (l1->format != NULL) g95_free(l1->format);
    g95_free(l1);
  }
}


/* g95_get_st_label()-- Given a label number, search for and return a
 * pointer to the label structure, creating it if it does not exist. */

g95_st_label *g95_get_st_label(int labelno) {
g95_st_label *lp;

/* First see if the label is already in this namespace.  */
  for(lp=g95_current_ns->st_labels; lp; lp=lp->next)
    if (lp->value == labelno) break;
  if (lp != NULL) return lp;
  
  lp = g95_getmem(sizeof(g95_st_label));

  lp->value = labelno;
  lp->defined = ST_LABEL_UNKNOWN;
  lp->referenced = ST_LABEL_UNKNOWN;

  lp->prev = NULL;
  lp->next = g95_current_ns->st_labels;
  if (g95_current_ns->st_labels)
    g95_current_ns->st_labels->prev = lp;
  g95_current_ns->st_labels = lp;

  return lp;
}


/* g95_new_internal_label() -- create a branch label for g95 internal use */

g95_st_label *g95_new_internal_label(void) {
static int next_label = 100000; /* only initialized at startup! */

  return g95_get_st_label (next_label++);
}

/* TODO : redo comment */
/* g95_define_st_label()-- Called when a statement with a statement
 * label is about to be accepted. We add the label to the list of the
 * current namespace, making sure it hasn't been defined previously
 * and referenced correctly. */

void g95_define_st_label(g95_st_label *lp, g95_sl_type type,
                         locus *label_locus) {
int labelno;

  labelno = lp->value;

  if (lp->defined != ST_LABEL_UNKNOWN)
    g95_error("Duplicate statement label %d at %L and %C", labelno, &lp->where);
  else {
    lp->where = *label_locus;

    switch(type) {
    case ST_LABEL_FORMAT:
      if (lp->referenced == ST_LABEL_TARGET) 
        g95_error("Label %d at %C already referenced as branch target", labelno);
      else
        lp->defined = ST_LABEL_FORMAT;

      break;

    case ST_LABEL_TARGET:
      if (lp->referenced == ST_LABEL_FORMAT)
        g95_error("Label %d at %C already referenced as a format label", labelno);
      else
        lp->defined = ST_LABEL_TARGET;

      break;

    default:
      lp->defined = ST_LABEL_BAD_TARGET;
      lp->referenced = ST_LABEL_BAD_TARGET;
    }
  }
}


/* g95_reference_st_label()-- Reference a label.  Given a label
 * and its type, see if that reference is consistent with what is
 * known about that label, updating the unknown state.  Returns
 * FAILURE if something goes wrong. */

try g95_reference_st_label(g95_st_label * lp, g95_sl_type type) {
g95_sl_type label_type;
int labelno;
try rc;

  if (lp == NULL)
    return SUCCESS;

  labelno = lp->value;

  if (lp->defined != ST_LABEL_UNKNOWN)
    label_type = lp->defined;
  else {
    label_type = lp->referenced;
    lp->where = *g95_current_locus();
  }

  if (label_type == ST_LABEL_FORMAT && type == ST_LABEL_TARGET) {
    g95_error("Label %d at %C previously used as a FORMAT label", labelno);
    rc = FAILURE;
    goto done;
  }

  if ((label_type == ST_LABEL_TARGET || label_type == ST_LABEL_BAD_TARGET)
      && type == ST_LABEL_FORMAT) {
    g95_error("Label %d at %C previously used as branch target", labelno);
    rc = FAILURE;
    goto done;
  }

  lp->referenced = type;
  rc = SUCCESS;

done:
  return rc;
}


/************** Symbol table management subroutines ****************/

/* Basic details: Fortran 95 requires a potentially unlimited number
 * of distinct namespaces when compiling a program unit.  This case
 * occurs during a compilation of internal subprograms because all of
 * the internal subprograms must be read before we can start
 * generating code for the host.
 * 
 * Given the tricky nature of the fortran grammar, we must be able to
 * undo changes made to a symbol table if the current interpretation
 * of a statement is found to be incorrect.  Whenever a symbol is
 * looked up, we make a copy of it and link to it.  All of these
 * symbols are kept in a singly linked list so that we can commit or
 * undo the changes at a later time. 
 *
 * A symtree may point to a symbol node outside of it's namespace.  In
 * this case, that symbol has been used as a host associated variable
 * at some previous time.  */

/* g95_get_namespace()-- Allocate a new namespace structure.  */

g95_namespace *g95_get_namespace(void) {
g95_namespace *ns;
g95_typespec *ts;
int i;
 
  ns = g95_getmem(sizeof(g95_namespace));
  ns->sym_root = NULL;
  ns->uop_root = NULL;
  ns->default_access = ACCESS_UNKNOWN;

  for(i=0; i<G95_INTRINSIC_OPS; i++)
    ns->operator_access[i] = ACCESS_UNKNOWN;

/* Initialize default types */

  for(i='a'; i<='z'; i++) {
    ns->set_flag[i - 'a'] = 0;
    ts = &ns->default_type[i - 'a'];

    if (ns->parent != NULL) {    /* Copy parent settings */
      *ts = ns->parent->default_type[i - 'a'];
      continue;
    }

    if (g95_option.implicit_none != 0) {
      g95_clear_ts(ts);
      continue;
    }

    if ('i' <= i && i <= 'n') {
      ts->type = BT_INTEGER;
      ts->kind = g95_default_integer_kind();
    } else {
      ts->type = BT_REAL;
      ts->kind = g95_default_real_kind();
    }
  }

  return ns;
}


/* g95_compare_symtree()-- Comparison function for symtree nodes. */

int g95_compare_symtree(g95_symtree *st1, g95_symtree *st2) {

  return strcmp(st1->name, st2->name);
}


/* g95_new_symtree()-- Allocate a new symtree node and associate it
 * with the new symbol. */

g95_symtree *g95_new_symtree(g95_symtree **root, char *name) {
g95_symtree *st;

  st = g95_getmem(sizeof(g95_symtree));
  strcpy(st->name, name);

  g95_insert_bbt(root, st, g95_compare_symtree);
  return st;
}


/* delete_symtree()-- delete a symbol from the tree.  Does not free the
 * symbol itself! */

static void delete_symtree(g95_symtree **root, char *name) {
g95_symtree st, *st0;

  st0 = g95_find_symtree(*root, name); 

  strcpy(st.name, name);
  g95_delete_bbt(root, &st, g95_compare_symtree);

  g95_free(st0);
}


/* g95_find_symtree()-- Given a root symtree node and a name, try to
 * find the symbol within the namespace.  Returns NULL if the symbol
 * is not found. */

g95_symtree *g95_find_symtree(g95_symtree *st, char *name) {
int c;

  while(st != NULL) {
    c = strcmp(name, st->name);
    if (c == 0) return st;

    st = (c < 0) ? st->left : st->right;
  }

  return NULL;
}


/* g95_get_uop()-- Given a name find a user operator node, creating it
 * if it doesn't exist.  These are much simpler than symbols because
 * they can't be ambiguous with one another */

g95_user_op *g95_get_uop(char *name) {
g95_user_op *uop;
g95_symtree *st;

  st = g95_find_symtree(g95_current_ns->uop_root, name);
  if (st != NULL) return st->n.uop;

  st = g95_new_symtree(&g95_current_ns->uop_root, name);

  uop = st->n.uop = g95_getmem(sizeof(g95_user_op));
  strcpy(uop->name, name);
  uop->access = ACCESS_UNKNOWN;
  uop->ns = g95_current_ns;

  return uop;
}


/* g95_find_uop()-- Given a name find the user operator node.  Returns
 * NULL if it does not exist. */

g95_user_op *g95_find_uop(char *name, g95_namespace *ns) {
g95_symtree *st;

  if (ns == NULL) ns = g95_current_ns;

  st = g95_find_symtree(ns->uop_root, name);
  return (st == NULL) ? NULL : st->n.uop;
}


/* g95_free_symbol()-- Remove a g95_symbol structure and everything it
 * points to. */

void g95_free_symbol(g95_symbol *sym) {

  if (sym == NULL) return;

  g95_free_array_spec(sym->as);

  free_components(sym->components);

  g95_free_expr(sym->value);

  g95_free_namelist(sym->namelist);

  g95_free_namespace(sym->formal_ns);

  g95_free_interface(sym->generic);

  g95_free_formal_arglist(sym->formal);

  g95_free(sym);
}


/* g95_new_symbol()-- Allocate and initialize a new symbol node */

g95_symbol *g95_new_symbol(char *name, g95_namespace *ns) {
g95_symbol *p;

  p = g95_getmem(sizeof(g95_symbol));

  g95_clear_ts(&p->ts);
  g95_clear_attr(&p->attr);
  p->ns = ns;

  p->declared_at = *g95_current_locus();

  if (strlen(name) > G95_MAX_SYMBOL_LEN)
    g95_internal_error("new_symbol(): Symbol name too long");

  strcpy(p->name, name);
  return p;
}


/* g95_find_symbol()-- search for a symbol starting in the current
 * namespace, resorting to any parent namespaces if requested by a
 * nonzero parent_flag.  Returns nonzero if the symbol is
 * ambiguous. */

int g95_find_symbol(char *name, g95_namespace *ns, int parent_flag,
		    g95_symbol **result) {
g95_symtree *st;

  if (ns == NULL) ns = g95_current_ns;

  do {
    st = g95_find_symtree(ns->sym_root, name);
    if (st != NULL) {
      if (st->ambiguous) return 1;

      *result = st->n.sym;
      return 0;
    }

    if (!parent_flag) break;

    ns = ns->parent;
  } while (ns != NULL);

  *result = NULL;
  return 0;
}


/* save_symbol_data()-- Save symbol with the information necessary to
 * back it out. */

static void save_symbol_data(g95_symbol *sym) {

  if (sym->new || sym->old_symbol != NULL) return;

  sym->old_symbol = g95_getmem(sizeof(g95_symbol));
  *(sym->old_symbol) = *sym;

  sym->tlink = changed_syms;
  changed_syms = sym;
}


/* g95_get_symbol()-- Given a name, find a symbol, or create it if it does
 * not exist yet in the current namespace.
 * If the symbol is found we make sure that it's OK.
 *
 * The integer return code indicates
 *  0   All OK
 *  1   The symbol name was ambiguous
 *  2   The name meant to be established was already host associated.
 *
 * So if nonzero, then an error was issued.  */

int g95_get_symbol(char *name, g95_namespace *ns, g95_symbol **result) {
g95_symtree *st;
g95_symbol *p;

  /* This doesn't usually happen during resolution.  */
  if (ns == NULL) ns = g95_current_ns;

  /* Try to find the symbol in ns.  */
  st = g95_find_symtree(ns->sym_root, name);

  if (st == NULL) {     /* If not there, create a new symbol */
    p = g95_new_symbol(name, ns); 

    p->old_symbol = NULL;   /* Add to the list of tentative symbols. */
    p->tlink = changed_syms;
    p->mark = 1;
    p->new = 1;
    changed_syms = p;

    st = g95_new_symtree(&ns->sym_root, name);
    st->n.sym = p;
    p->refs++;

  } else {    /* Make sure the existing symbol is OK */

    if (st->ambiguous) {
      if (st->n.sym->module[0])
	g95_error("Name '%s' at %C is an ambiguous reference to '%s' "
		  "from module '%s'", name, st->n.sym->name,
		  st->n.sym->module);
      else
	g95_error("Name '%s' at %C is an ambiguous reference to '%s' "
		  "from current program unit", name, st->n.sym->name);

      return 1;
    }

    p = st->n.sym;

    if (p->ns != ns && (!p->attr.function || ns->proc_name != p)) {
      /* Symbol is from another namespace */
      g95_error("Symbol '%s' at %C has already been host associated", name);
      return 2;
    }

    p->mark = 1;

    save_symbol_data(p);      /* Copy in case this symbol is changed */
  }

  *result = p;
  return 0;
}


/* g95_get_ha_symbol()-- Subroutine that searches for a symbol,
 * creating it if it doesn't exist, but tries to host-associate the
 * symbol if possible. */

int g95_get_ha_symbol(char *name, g95_symbol **result) {
g95_symbol *sym;
int i;

  i = g95_find_symbol(name, g95_current_ns, 0, &sym);
  if (sym != NULL) {
    save_symbol_data(sym);

    *result = sym;
    return i;
  }

  if (g95_current_ns->parent != NULL) {
    i = g95_find_symbol(name, g95_current_ns->parent, 1, &sym);
    if (i) return i;

    if (sym != NULL) {
      *result = sym;
      return 0;
    }
  }

  return g95_get_symbol(name, g95_current_ns, result);
}


/* g95_undo_symbols()-- Undoes all the changes made to symbols in the
 * current statement.  This subroutine is made simpler due to the fact
 * that attributes are never removed once added. */

void g95_undo_symbols(void) {
g95_symbol *p, *q, *old;
g95_symtree *v, *w;

/* if (changed_syms != NULL) g95_status("Undoing symbols\n"); */

  for(p=changed_syms; p; p=q) {
    q = p->tlink;
    /* g95_status("Undoing %s\n", p->name); */

    if (p->new) {  /* Symbol was new */
      delete_symtree(&p->ns->sym_root, p->name);

      p->refs--;
      if (p->refs < 0) g95_internal_error("g95_undo_symbols(): Negative refs");
      if (p->refs == 0) g95_free_symbol(p);
      continue;
    }

/* Restore previous state of symbol.  Just copy simple stuff */

    p->mark = 0;
    old = p->old_symbol;

    p->ts.type = old->ts.type;
    p->ts.kind = old->ts.kind;

    p->attr = old->attr;

    if (p->value != old->value) {
      g95_free_expr(old->value);
      p->value = NULL;
    }

    if (p->as != old->as) {
      if (p->as) g95_free_array_spec(p->as);
      p->as = old->as;
    }

    p->generic = old->generic;
    p->component_access = old->component_access;

    if (p->namelist != NULL && old->namelist == NULL) {
      g95_free_namelist(p->namelist);
      p->namelist = NULL;
    } else {

      if (p->namelist_tail != old->namelist_tail) {
	g95_free_namelist(old->namelist_tail);
	old->namelist_tail->next = NULL;
      }
    }

    p->namelist_tail = old->namelist_tail;

    if (p->formal != old->formal) {
      g95_free_formal_arglist(p->formal);
      p->formal = old->formal;
    }

    g95_free(p->old_symbol);
    p->old_symbol = NULL;
    p->tlink = NULL;
  }

  changed_syms = NULL;

  /* Unlink host associated symtrees */

  for(v=changed_st; v; v=w) {
    w = v->link;

    g95_delete_bbt(&g95_current_ns->sym_root, v, g95_compare_symtree);
    g95_free(v);
  }

  changed_st = NULL;
}


/* g95_commit_symbols()-- Makes the changes made in the current
 * statement permanent-- gets rid of undo information. */

void g95_commit_symbols(void) {
g95_symbol *p, *q;
g95_symtree *v, *w;

#if 0
  if (changed_syms != NULL) g95_status("Committing symbols\n");
#endif

  for(p=changed_syms; p; p=q) {
    q = p->tlink;
    p->tlink = NULL;
    p->mark = 0;
    p->new = 0;

    if (p->old_symbol != NULL) {
      g95_free(p->old_symbol);
      p->old_symbol = NULL;
    }
  }

  changed_syms = NULL;

  for(v=changed_st; v; v=w) {
    w = v->link;
    v->link = NULL;
  }

  changed_st = NULL;
}


/* free_uop_tree()-- Recursive function that deletes an entire
 * red-black tree and all the user operator nodes that it contains. */

static void free_uop_tree(g95_symtree *rb) {

  if (rb == NULL) return;

  free_uop_tree(rb->left);
  free_uop_tree(rb->right);

  g95_free_interface(rb->n.uop->operator);

  g95_free(rb->n.uop);
  g95_free(rb);
}


/* free_sym_tree()-- Recursive function that deletes an entire
 * red-black tree and all the symbols that it contains. */

static void free_sym_tree(g95_symtree *rb) {
g95_namespace *ns;
g95_symbol *sym;

  if (rb == NULL) return;

  free_sym_tree(rb->left);
  free_sym_tree(rb->right);

  sym = rb->n.sym;

  if (sym->formal_ns != NULL) {
    ns = sym->formal_ns;
    sym->formal_ns = NULL;
    g95_free_namespace(ns);
  }

  sym->refs--;
  if (sym->refs < 0) g95_internal_error("free_sym_tree(): Negative refs");
  if (sym->refs == 0) g95_free_symbol(sym);

  g95_free(rb);
}


/* g95_free_namespace()-- Free a namespace structure and everything
 * below it.  Interface lists associated with intrinsic operators are
 * not freed.  These are taken care of when a specific name is freed. */

void g95_free_namespace(g95_namespace *ns) {
g95_namespace *next_ns;
g95_charlen *cl, *cl2;
int i;

  if (ns == NULL) return; 

  g95_free_statements(ns->code);

  free_sym_tree(ns->sym_root);
  free_uop_tree(ns->uop_root);

  for(cl=ns->cl_list; cl; cl=cl2) {
    cl2 = cl->next;
    g95_free_expr(cl->length);
    g95_free(cl);
  }

  free_st_labels(ns->st_labels);

  g95_free_equiv(ns->equiv);

  for(i=0; i<G95_INTRINSIC_OPS; i++)
    g95_free_interface(ns->operator[i]);

  g95_free_data(ns->data);
  next_ns = ns->contained;
  g95_free(ns);

  /* Recursively free any contained namespaces */

  while(next_ns != NULL) {
    ns = next_ns;
    next_ns = next_ns->sibling;

    g95_free_namespace(ns);
  }
}


void g95_symbol_init_2(void) {

  g95_current_ns = g95_get_namespace();
}


void g95_symbol_done_2(void) {

  g95_free_namespace(g95_current_ns);
}


static int show_level=0;

static void show_indent(void) {
int i;

  g95_status_char('\n');
  for(i=0; i<2*show_level; i++)
    g95_status_char(' ');
}


/* g95_show_symbol()-- Show a symbol.  If a symbol is an ENTRY,
 * SUBROUTINE or FUNCTION, we show the interface.  Information needed
 * to reconstruct the list of specific interfaces associated with a
 * generic symbol is done within that symbol. */

void g95_show_symbol(g95_symbol *sym) {
g95_formal_arglist *formal;
g95_interface *intr;
g95_symbol *s;

  if (sym == NULL) return;

  show_indent();

  g95_status("symbol %s ", sym->name);
  g95_show_typespec(&sym->ts);
  g95_show_attr(&sym->attr);

  if (sym->value) {
    show_indent();
    g95_status("value: ");
    g95_show_expr(sym->value);
  }

  if (sym->as) {
    show_indent();
    g95_status("Array spec:");
    g95_show_array_spec(sym->as);
  }

  if (sym->generic) {
    show_indent();
    g95_status("Generic interfaces:");
    for(intr=sym->generic; intr; intr=intr->next)
      g95_status(" %s", intr->sym->name);
  }

  if (sym->common_head) {
    show_indent();
    g95_status("Common members:");
    for(s=sym->common_head; s; s=s->common_next)
      g95_status(" %s", s->name);
  }

  if (sym->result) {
    show_indent();
    g95_status("result: %s", sym->result->name);
  }

  if (sym->components) {
    show_indent();
    g95_status("components: ");
    g95_show_components(sym);
  }

  if (sym->formal) {
    show_indent();
    g95_status("Formal arglist:");

    for(formal=sym->formal; formal; formal=formal->next)
      g95_status(" %s", formal->sym->name);
  }

  if (sym->formal_ns) {
    show_indent();
    g95_status("Formal namespace");
    g95_show_namespace(sym->formal_ns);
  }

  g95_status_char('\n');
}


static void show_uop(g95_user_op *uop) {
g95_interface *intr;

  show_indent();
  g95_status("%s:", uop->name);
  
  for(intr=uop->operator; intr; intr=intr->next)
    g95_status(" %s", intr->sym->name);
}


/* show_symtree()-- Worker function to display the symbol tree */

static void show_symtree(g95_symtree *st) {

  show_indent();
  g95_status("symtree: %s  Ambig %d", st->name, st->ambiguous);

  if (st->n.sym->ns != g95_current_ns)
    g95_status(" from namespace %s", st->n.sym->ns->proc_name->name);
  else
    g95_show_symbol(st->n.sym);
}


/* clear_sym_mark()-- Clear mark bits from symbol nodes associated
 * with a symtree node */

static void clear_sym_mark(g95_symtree *st) {

  st->n.sym->mark = 0;
}


/* traverse_symtree()-- Recursively traverse the symtree nodes. */

static void traverse_symtree(g95_symtree *st, void (*func)(g95_symtree *)) {

  if (st != NULL) {
    (*func)(st);

    traverse_symtree(st->left, func);
    traverse_symtree(st->right, func);
  }
}


void g95_traverse_symtree(g95_namespace *ns, void (*func)(g95_symtree *)) {

  traverse_symtree(ns->sym_root, func);
}


/* traverse_ns()-- Recursive namespace traversal function. */

static void traverse_ns(g95_symtree *st, void (*func)(g95_symbol *)) {

  if (st == NULL) return;

  if (st->n.sym->mark == 0) (*func)(st->n.sym);
  st->n.sym->mark = 1;

  traverse_ns(st->left, func);
  traverse_ns(st->right, func);
}


/* g95_traverse_ns()-- Call a given function for all symbols in the
 * namespace.  We take care that each g95_symbol node is called
 * exactly once. */

void g95_traverse_ns(g95_namespace *ns, void (*func)(g95_symbol *)) {

  g95_traverse_symtree(ns, clear_sym_mark);

  traverse_ns(ns->sym_root, func);
}


/* traverse_uop()-- Function for traversing the user operator symtree */

static void traverse_uop(g95_symtree *st, void (*func)(g95_user_op *)) {

  if (st == NULL) return;

  (*func)(st->n.uop);

  traverse_uop(st->left, func);
  traverse_uop(st->right, func);
}


/* g95_traverse_user_op()-- Traverse the tree of user operator nodes.  */

void g95_traverse_user_op(g95_namespace *ns, void (*func)(g95_user_op *)) {

  traverse_uop(ns->uop_root, func);
}


/* save_symbol()-- Given a symbol, mark it as SAVEd if it is allowed */

static void save_symbol(g95_symbol *sym) {

  if (sym->attr.use_assoc) return;

  if (sym->attr.common) {
    g95_add_saved_common(&sym->attr, &sym->declared_at);
    return;
  }

  if (sym->attr.in_common || sym->attr.dummy ||
      sym->attr.flavor != FL_VARIABLE) return;

  g95_add_save(&sym->attr, &sym->declared_at);
}


/* g95_save_all()-- Mark those symbols which can be SAVEd as such. */

void g95_save_all(g95_namespace *ns) {

  g95_traverse_ns(ns, save_symbol);
}


/* g95_show_namespace()-- Show a namespace */

void g95_show_namespace(g95_namespace *ns) {
g95_interface *intr;
g95_namespace *save;
int i;

  save = g95_current_ns; 
  show_level++; 

  show_indent();
  g95_status("Namespace:");

  if (ns != NULL) {
    for(i=0; i<G95_LETTERS; i++) {
      g95_status(" %c: ", i+'A');
      g95_show_typespec(&ns->default_type[i]);
    }

    if (ns->proc_name != NULL) {
      show_indent();
      g95_status("procedure name = %s", ns->proc_name->name);
    }

    g95_traverse_symtree(ns, clear_sym_mark);

    g95_current_ns = ns;
    g95_traverse_symtree(ns, show_symtree);

    for(i=0; i<G95_INTRINSIC_OPS; i++) {    /* User operator interfaces */
      intr = ns->operator[i];
      if (intr == NULL) continue;

      show_indent();
      g95_status("Operator interfaces for %s:", g95_op2string(i));

      for(; intr; intr=intr->next)
	g95_status(" %s", intr->sym->name);
    }

    if (ns->uop_root != NULL) {
      show_indent();
      g95_status("User operators:\n");
      g95_traverse_user_op(ns, show_uop);
    }
  }

  g95_status_char('\n');
  g95_status_char('\n');

#ifdef G95_DEBUG
  g95_show_code(0, ns->code);
#endif

  for(ns=ns->contained; ns; ns=ns->sibling) {
    g95_status("CONTAINS\n");
    g95_show_namespace(ns);
  }

  show_level--;
  g95_status_char('\n');
  g95_current_ns = save;
}


/* Scaffolding */

#ifdef G95_DEBUG

/* g95_symbol_state()-- Makes sure that no changes to symbols are pending */

void g95_symbol_state(void) {

  if (changed_syms != NULL)
    g95_internal_error("Symbol changes still pending");
}

#endif

