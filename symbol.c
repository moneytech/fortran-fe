/* Symbol handling
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

/* symbol.c-- Maintains binary trees of symbols */

#include <string.h>
#include <stdio.h>
#include <ctype.h>
#include <stdlib.h>

#include "g95.h"

g95_namespace *g95_current_ns;


/*********** IMPLICIT NONE and IMPLICIT statement handlers ***********/

/* The following static variables hold the default types set by
 * IMPLICIT statements.  We have to store kind information because of
 * IMPLICIT DOUBLE PRECISION statements.  IMPLICIT NONE stores a
 * BT_UNKNOWN into all elements.  The arrays of flags indicate whether
 * a particular element has been explicitly set or not.  */


/* g95_match_implicit_none()-- Match an IMPLICIT NONE statement. */

match g95_match_implicit_none(void) {

  return (g95_match_eos() == MATCH_YES) ? MATCH_YES : MATCH_NO;
}


/* g95_set_implicit_none()-- Handle a correctly parsed IMPLICIT NONE */

void g95_set_implicit_none(void) {
int i;

  for(i='a'; i<='z'; i++) {
    g95_current_ns->default_type[i - 'a'].type = BT_UNKNOWN;
    g95_current_ns->set_flag[i - 'a'] = 1;
  }
}


static g95_typespec new_ts[G95_LETTERS];
static int new_flag[G95_LETTERS];


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
    new_ts[i].type = BT_UNKNOWN;
    if (new_flag[i]) new_flag[i] = 0;
  }

  if (g95_match_eos() == MATCH_YES) {
    g95_error("Empty IMPLICIT statement at %C");
    return MATCH_ERROR;
  }

  do {
    m = g95_match_type_spec(&ts, 0, 0, 0); /* A basic type is mandatory here */
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


/* g95_set_default_type()-- Given a pointer to a symbol, set its type
 * according to the first letter of its name.  Fails if the letter in
 * question has no default type. */

try g95_set_default_type(g95_symbol *sym) {
int i;

  i = sym->name[0] - 'a';

  if (g95_current_ns->default_type[i].type == BT_UNKNOWN) {
    g95_error("Symbol '%s' at %C has no IMPLICIT type", sym->name);
    return FAILURE;
  }

  if (sym->ts.type != BT_UNKNOWN)
    g95_internal_error("g95_set_default_type(): symbol already has a type");

  sym->ts = g95_current_ns->default_type[i];

  return SUCCESS;
}


/* g95_check_assign()-- Given an assignable expression and an
 * arbitrary expression, make sure that the assignment can take place.
 *
 * If the root symbol associated with the variable has no type, then
 * we try to give it the default type.   */

try g95_check_assign(g95_expr *lvalue, g95_expr *rvalue) {
g95_symbol *sym;

  sym = lvalue->symbol; 

  if (sym->ts.type == BT_UNKNOWN) {
    if (g95_set_default_type(sym) == FAILURE) return FAILURE;
    lvalue->ts = sym->ts;
  }

  if (lvalue->ts.type == BT_DERIVED && rvalue->ts.type == BT_DERIVED &&
      lvalue->ts.derived != rvalue->ts.derived) {
    g95_error("Incompatible derived types in assignment at %C");
    return FAILURE;
  }

  if (lvalue->ts.type == rvalue->ts.type &&
      lvalue->ts.kind == rvalue->ts.kind) return SUCCESS;

  return g95_convert_type(rvalue, &lvalue->ts, 1);
}


/* g95_check_pointer_assign()-- Check that a pointer assignment is OK */

try g95_check_pointer_assign(g95_expr *lvalue, g95_expr *rvalue) {
int failure;  

  failure = 0;

  if (lvalue->symbol->ts.type == BT_UNKNOWN) {
    g95_error("Pointer assignment target is not a POINTER at %L",
	      &lvalue->where);
    return FAILURE; /* Further checks are impossible and worthless 
		       in this case */
  }

  if (lvalue->ts.type == BT_DERIVED && rvalue->ts.type == BT_DERIVED &&
      lvalue->ts.derived != rvalue->ts.derived) {
    g95_error("Incompatible derived types in pointer assignment at %L",
	      &lvalue->where);
    failure = 1 ;
  }

  if (lvalue->ts.type != rvalue->ts.type) {
    g95_error("Different types in pointer assignment at %L",
	      &lvalue->where);
    failure = 1; 
  }

  if (lvalue->ts.kind != rvalue->ts.kind) {
    g95_error("Different kind type parameters in pointer assignment at %L",
	      &lvalue->where);
    failure = 1; 
  }

  if (!lvalue->symbol->attr.pointer) {
    g95_error("Pointer assignment to non-POINTER at %L",
	      &lvalue->where);
    failure = 1; 
  }

  if (!lvalue->symbol->attr.target && !lvalue->symbol->attr.pointer) {
    g95_error("Pointer assignment target is neither TARGET nor POINTER at %L",
	      &rvalue->where);
    failure = 1;
  }

/* TODO: further checks required */
  g95_warning("Checks for pointer assignment are incomplete.");

  if (failure) return FAILURE; 

  return SUCCESS;
}


/* g95_check_assign_symbol()-- Relative of g95_check_assign() except
 * that the lvalue is a single symbol. */

try g95_check_assign_symbol(g95_symbol *sym, g95_expr *rvalue) {
g95_expr lvalue;

  memset(&lvalue, '\0', sizeof(g95_expr));

  lvalue.expr_type = EXPR_VARIABLE;
  lvalue.ts = sym->ts;
  lvalue.symbol = sym;

  return g95_check_assign(&lvalue, rvalue);
}

/******************** Symbol attribute stuff *********************/

static mstring flavors[] = {
  minit("UNKNOWN",             FL_UNKNOWN),
  minit("PROGRAM",             FL_PROGRAM), 
  minit("SUBROUTINE",          FL_SUBROUTINE),
  minit("FUNCTION",            FL_FUNCTION),
  minit("MODULE",              FL_MODULE),
  minit("BLOCK-DATA",          FL_BLOCK_DATA),
  minit("VARIABLE",            FL_VARIABLE),
  minit("PARAMETER",           FL_PARAMETER),
  minit("DERIVED",             FL_DERIVED),
  minit("LABEL",               FL_LABEL),
  minit("STATEMENT-FUNCTION",  FL_ST_FUNCTION),
  minit("MODPROC",             FL_MODPROC),
  minit("NAMELIST-GROUP",      FL_NAMELIST),
  minit("OPERATOR",            FL_OPERATOR),
  minit(NULL, -1) },

intents[] = {
  minit("UNKNOWN", INTENT_UNKNOWN),
  minit("IN", INTENT_IN),
  minit("OUT", INTENT_OUT),
  minit("INOUT", INTENT_INOUT),
  minit(NULL, -1)
#if 0
},

scopes[] = {
  minit("UNKNOWN",    SCOPE_UNKNOWN),
  minit("EXTERNAL",   SCOPE_EXTERNAL),
  minit("INTERNAL",   SCOPE_INTERNAL),
  minit("INTRINSIC",  SCOPE_INTRINSIC),
  minit(NULL, -1)
#endif
};


/* g95_show_attr()-- Show symbol attributes.  The flavor and intent
 * are followed by whatever single bit attributes are present */

void g95_show_attr(symbol_attribute *attr) {

  g95_status("(%s %s", g95_code2string(flavors, attr->flavor),
	     g95_code2string(intents, attr->intent));

  if (attr->allocatable)  g95_status(" ALLOCATABLE");
  if (attr->dimension)    g95_status(" DIMENSION");
  if (attr->external)     g95_status(" EXTERNAL");
  if (attr->intrinsic)    g95_status(" INTRINSIC");
  if (attr->optional)     g95_status(" OPTIONAL");
  if (attr->pointer)      g95_status(" POINTER");
  if (attr->private)      g95_status(" PRIVATE");
  if (attr->public)       g95_status(" PUBLIC");
  if (attr->result)       g95_status(" RESULT");
  if (attr->save)         g95_status(" SAVE");
  if (attr->target)       g95_status(" TARGET");
  if (attr->entry)        g95_status(" ENTRY");

  if (attr->dummy)        g95_status(" DUMMY");
  if (attr->data)         g95_status(" DATA");
  if (attr->common)       g95_status(" COMMON");
  if (attr->in_common)    g95_status(" IN-COMMON");
  if (attr->in_namelist)  g95_status(" IN-NAMELIST");
  if (attr->generic)      g95_status(" GENERIC");

  if (attr->sequence)     g95_status(" SEQUENCE");
  if (attr->elemental)    g95_status(" ELEMENTAL");
  if (attr->pure)         g95_status(" PURE");
  if (attr->recursive)    g95_status(" RECURSIVE");

  g95_status(")");
}


/* check_conflict()-- This is a generic conflict-checker.  We do this
 * to avoid having a single conflict in two places. */

#define conf(a, b) if (attr->a && attr->b) { a1 = a; a2 = b; goto conflict; }

static try check_conflict(symbol_attribute *attr, locus *loc) {
char *a1, *a2;

static char *dummy = "DUMMY", *parameter = "PARAMETER", *save = "SAVE",
  *pointer = "POINTER", *target = "TARGET", *external = "EXTERNAL",
  *intent = "INTENT", *intrinsic = "INTRINSIC", *allocatable = "ALLOCATABLE",
  *elemental = "ELEMENTAL", *private = "PRIVATE", *sequence = "SEQUENCE",
  *recursive = "RECURSIVE", *in_common = "COMMON", *result = "RESULT",
  *in_namelist = "NAMELIST", *public = "PUBLIC", *optional = "OPTIONAL",
  *entry = "ENTRY";

  if (loc == NULL) loc = g95_current_locus();

  if (attr->pointer && attr->intent != INTENT_UNKNOWN) {
    a1 = pointer; a2 = intent; goto conflict; }

/* Check for attributes not allowed in a BLOCK DATA */

  if (g95_current_state() == COMP_BLOCK_DATA) {
    a1 = NULL;

    if (attr->allocatable) a1 = allocatable;
    if (attr->external) a1 = external;
    if (attr->optional) a1 = optional;
    if (attr->private) a1 = private;
    if (attr->public) a1 = public;
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

  conf(in_namelist, pointer);
  conf(in_namelist, allocatable);

  conf(entry, result);

  a1 = g95_code2string(flavors, attr->flavor);

  if (attr->in_namelist && attr->flavor != FL_VARIABLE &&
      attr->flavor != FL_UNKNOWN) {

    a2 = in_namelist;
    goto conflict;
  }

  switch(attr->flavor) {
  case FL_FUNCTION:
    if (attr->result)    { a2 = result; goto conflict; }
    if (attr->in_common) { a2 = in_common; goto conflict; }
    break;

  case FL_ST_FUNCTION:
    if (attr->in_common) { a2 = in_common; goto conflict; }
    break;

  case FL_DERIVED:
    if (attr->private)  { a2 = private;  goto conflict; }
    if (attr->sequence) { a2 = sequence; goto conflict; }
    break;

  case FL_PARAMETER:
    if (attr->target)    { a2 = parameter; goto conflict; }
    if (attr->dummy)     { a2 = dummy;     goto conflict; }
    if (attr->in_common) { a2 = in_common; goto conflict; }
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


try g95_add_allocatable(symbol_attribute *attr, locus *loc) {

  attr->allocatable = 1;
  return check_conflict(attr, loc);
}

try g95_add_dimension(symbol_attribute *attr, locus *loc) {

  attr->dimension = 1;
  return check_conflict(attr, loc);
}

try g95_add_external(symbol_attribute *attr, locus *loc) {

  attr->external = 1;
  return check_conflict(attr, loc);
}

try g95_add_intrinsic(symbol_attribute *attr, locus *loc) {

  attr->intrinsic = 1;
  return check_conflict(attr, loc);
}

try g95_add_optional(symbol_attribute *attr, locus *loc) {

  attr->optional = 1;
  return check_conflict(attr, loc);
}

try g95_add_pointer(symbol_attribute *attr, locus *loc) {

  attr->pointer = 1;
  return check_conflict(attr, loc);
}

try g95_add_private(symbol_attribute *attr, locus *loc) {

  attr->private = 1;
  return check_conflict(attr, loc);
}

try g95_add_public(symbol_attribute *attr, locus *loc) {

  attr->public = 1;
  return check_conflict(attr, loc);
}

try g95_add_result(symbol_attribute *attr, locus *loc) {

  attr->result = 1;
  return check_conflict(attr, loc);
}

try g95_add_save(symbol_attribute *attr, locus *loc) {

  attr->save = 1;
  return check_conflict(attr, loc);
}

try g95_add_saved_common(symbol_attribute *attr, locus *loc) {

  attr->saved_common = 1;
  return check_conflict(attr, loc);
}

try g95_add_target(symbol_attribute *attr, locus *loc) {

  attr->target = 1;
  return check_conflict(attr, loc);
}

try g95_add_dummy(symbol_attribute *attr, locus *loc) {

  attr->dummy = 1;
  return check_conflict(attr, loc);
}

try g95_add_data(symbol_attribute *attr, locus *loc) {

  attr->data = 1;
  return check_conflict(attr, loc);
}

try g95_add_generic(symbol_attribute *attr, locus *loc) {

  attr->generic = 1;
  return check_conflict(attr, loc);
}

try g95_add_common(symbol_attribute *attr, locus *loc) {

  attr->common = 1;
  return check_conflict(attr, loc);
}

try g95_add_in_common(symbol_attribute *attr, locus *loc) {

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

  attr->sequence = 1;
  return check_conflict(attr, loc);
}

try g95_add_elemental(symbol_attribute *attr, locus *loc) {

  attr->elemental = 1;
  return check_conflict(attr, loc);
}

try g95_add_pure(symbol_attribute *attr, locus *loc) {

  attr->pure = 1;
  return check_conflict(attr, loc);
}

try g95_add_recursive(symbol_attribute *attr, locus *loc) {

  attr->recursive = 1;
  return check_conflict(attr, loc);
}


try g95_add_flavor(symbol_attribute *attr, sym_flavor f, locus *loc) {

  if (attr->flavor == FL_UNKNOWN) {
    attr->flavor = f;
    return SUCCESS;
  }

  if (loc == NULL) loc = g95_current_locus();

  g95_error("%s attribute conflicts with %s attribute at %L",
	    g95_code2string(flavors, attr->flavor),
	    g95_code2string(flavors, f), loc);

  return FAILURE;
}


try g95_add_entry(symbol_attribute *attr, sym_flavor f, locus *loc) {

  if (attr->flavor == FL_UNKNOWN) {
    attr->flavor = f;
    attr->entry = 1;
    return SUCCESS;
  }

  if (loc == NULL) loc = g95_current_locus();

  g95_error("ENTRY attribute conflicts with %s attribute at %L",
	    g95_code2string(flavors, attr->flavor), loc);

  return FAILURE;
}


try g95_add_intent(symbol_attribute *attr, sym_intent intent, locus *loc) {

  if (attr->intent == INTENT_UNKNOWN) {
    attr->intent = intent;
    return SUCCESS;
  }

  if (loc == NULL) loc = g95_current_locus();

  g95_error("INTENT (%s) conflicts with INTENT(%s) at %L",
	    g95_code2string(intents, attr->intent),
	    g95_code2string(intents, intent), loc);

  return FAILURE;
}


/* g95_compare_attr()-- Compares two attributes */

int g95_compare_attr(symbol_attribute *a1, symbol_attribute *a2) {

  return a1->allocatable == a2->allocatable &&
    a1->dimension == a2->dimension && a1->external == a2->external &&
    a1->intrinsic == a2->intrinsic && a1->optional == a2->optional &&
    a1->pointer == a2->pointer     && a1->private == a2->private &&
    a1->public == a2->public       && a1->save == a2->save &&
    a1->target == a2->target       && a1->dummy == a2->dummy &&
    a1->data == a2->data           && a1->generic == a2->generic &&
    a1->common == a2->common       && a1->in_common == a2->in_common &&
    a1->sequence == a2->sequence   && a1->elemental == a2->elemental &&
    a1->pure == a2->pure           && a1->recursive == a2->recursive &&
    a1->intent == a2->intent       && a1->flavor == a2->flavor &&
    a1->result == a2->result       && a1->in_namelist == a2->in_namelist &&
    a1->saved_common == a2->saved_common && a1->entry == a2->entry;
}


/* g95_clear_attr()-- Clears all attributes */

void g95_clear_attr(symbol_attribute *attr) {

  attr->allocatable = 0;
  attr->dimension = 0;
  attr->external = 0;
  attr->intrinsic = 0;
  attr->optional = 0;
  attr->pointer = 0;
  attr->private = 0;
  attr->public = 0;
  attr->result = 0;
  attr->save = 0;
  attr->saved_common = 0;
  attr->target = 0;
  attr->entry = 0;
  
  attr->dummy = 0;
  attr->data = 0;
  attr->generic = 0;
  attr->common = 0;
  attr->in_common = 0;
  attr->in_namelist = 0;
  
  attr->sequence = 0;
  attr->elemental = 0;
  attr->pure = 0;
  attr->recursive = 0;

  attr->flavor = FL_UNKNOWN;
  attr->intent = INTENT_UNKNOWN;
}


/* g95_missing_attr()-- Check for missing attributes in the new symbol */

try g95_missing_attr(symbol_attribute *attr, locus *loc) {

  if ((attr->optional || attr->intent != INTENT_UNKNOWN) &&
      attr->dummy == 0) {

    if (loc == NULL) loc = g95_current_locus();

    g95_error("Symbol at %L is not a DUMMY variable", loc);
    return FAILURE;
  }

  return SUCCESS;
}


/* g95_copy_attr()-- copy one attribute over another, bit by bit. */

try g95_copy_attr(symbol_attribute *dest, symbol_attribute *src, locus *loc) {

  if (src->allocatable && g95_add_allocatable(dest, loc) == FAILURE) goto fail;
  if (src->dimension && g95_add_dimension(dest, loc) == FAILURE) goto fail;
  if (src->external && g95_add_external(dest, loc) == FAILURE) goto fail;
  if (src->intrinsic && g95_add_intrinsic(dest, loc) == FAILURE) goto fail;
  if (src->optional && g95_add_optional(dest, loc) == FAILURE) goto fail;
  if (src->pointer && g95_add_pointer(dest, loc) == FAILURE) goto fail;
  if (src->private && g95_add_private(dest, loc) == FAILURE) goto fail;
  if (src->public && g95_add_public(dest, loc) == FAILURE) goto fail;
  if (src->result && g95_add_result(dest, loc) == FAILURE) goto fail;
  if (src->save && g95_add_save(dest, loc) == FAILURE) goto fail;
  if (src->saved_common && g95_add_saved_common(dest, loc)==FAILURE) goto fail;
  if (src->target && g95_add_target(dest, loc) == FAILURE) goto fail;
  if (src->entry) dest->entry = 1;

  if (src->dummy && g95_add_dummy(dest, loc) == FAILURE) goto fail;
  if (src->data && g95_add_data(dest, loc) == FAILURE) goto fail;
  if (src->generic && g95_add_generic(dest, loc) == FAILURE) goto fail;
  if (src->common && g95_add_common(dest, loc) == FAILURE) goto fail;
  if (src->in_common && g95_add_in_common(dest, loc) == FAILURE) goto fail;
  if (src->in_namelist && g95_add_in_namelist(dest, loc) == FAILURE) goto fail;

  if (src->sequence && g95_add_sequence(dest, loc) == FAILURE) goto fail;
  if (src->elemental && g95_add_elemental(dest, loc) == FAILURE) goto fail;
  if (src->pure && g95_add_pure(dest, loc) == FAILURE) goto fail;
  if (src->recursive && g95_add_recursive(dest, loc) == FAILURE) goto fail;

  if (src->flavor != FL_UNKNOWN &&
      g95_add_flavor(dest, src->flavor, loc) == FAILURE) goto fail;

  if (src->intent != INTENT_UNKNOWN &&
      g95_add_intent(dest, src->intent, loc) == FAILURE) goto fail;

  if (g95_missing_attr(dest, loc) == FAILURE) goto fail;

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

try g95_add_component(g95_symbol *sym, char *name, g95_component **component) {
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

  p = g95_getmem(sizeof(g95_component));

  if (tail == NULL) sym->components = p;
  else tail->next = p;

  strcpy(p->name, name);
  p->loc = *g95_current_locus();

  *component = p;
  return SUCCESS;
}


/* g95_find_component()-- Given a derived type node and a component
 * name, try to locate the component structure.  Returns the NULL
 * pointer if the component is not found. */

g95_component *g95_find_component(g95_symbol *sym, char *name) {
g95_component *p;

  for(p=sym->components; p; p=p->next)
    if (strcmp(p->name, name) == 0) break;

  return p;
}


/* free_components()-- Given a symbol, free all of the component
 * structures and everything they point to. */

static void free_components(g95_component *p) {
g95_component *q;

  for(; p; p=q) {
    q = p->next;

    g95_free_array_spec(&p->as);
    g95_free_expr(p->initializer);

    g95_free(p);
  }
}


void g95_show_components(g95_symbol *sym) {
g95_component *c;

  g95_status("(");
  for(c=sym->components; c; c=c->next) {
    g95_status("(%s", c->name);
    g95_show_typespec(&c->ts);
    g95_show_array_spec(&c->as);
    g95_show_attr(&c->attr);
    g95_status(")");
  }

  g95_status(")");
}


/******************** Statement label management ********************/

static void free_st_labels(g95_st_label *l1) {
g95_st_label *l2;

  for(; l1; l1=l2) {
    l2 = l1->next;
    if (l1->format != NULL) g95_free(l1->format);
    g95_free(l1);
  }
}


/* g95_check_st_labels()-- Check statement labels within a namespace
 * to make sure that referenced labels are defined and warn if defined
 * labels are never referenced. */

void g95_check_st_labels(g95_namespace *ns) {
g95_st_label *p;

  for(p=ns->st_labels; p; p=p->next) {
    if (p->defined == ST_LABEL_UNKNOWN)
      g95_error("Label %d referenced at %L is never defined", p->label,
		&p->where);

    if (p->referenced == ST_LABEL_UNKNOWN)
      g95_warning("Label %d defined at %L is never referenced", p->label,
		  &p->where);
  }
}


/* get_st_label()-- Given a label number, search for and return a
 * pointer to the label structure, creating it if it does not exist. */

static g95_st_label *get_st_label(int label) {
g95_st_label *lp;

  for(lp=g95_current_ns->st_labels; lp; lp=lp->next)
    if (lp->label == label) return lp;

  lp = g95_get_st_label();

  lp->label = label;
  lp->defined = ST_LABEL_UNKNOWN;
  lp->referenced = ST_LABEL_UNKNOWN;

  lp->next = g95_current_ns->st_labels;
  g95_current_ns->st_labels = lp;

  return lp;
}


/* g95_define_st_label()-- Called when a statement with a statement
 * label is about to be accepted.  The format_flag indicates whether
 * the statement in question is a format statement or not.  We add the
 * label to the list of the current namespace, making sure it hasn't
 * been defined previously and referenced correctly. */

void g95_define_st_label(int label, locus *label_locus, int format_flag) {
g95_st_label *lp;

  lp = get_st_label(label);

  if (lp->defined != ST_LABEL_UNKNOWN) {
    g95_error("Statement label %d at %C has already been defined at %L",
	      label, &lp->where);
    return;
  }

  lp->where = *label_locus;

  if (format_flag) {
    if (lp->referenced == ST_LABEL_TARGET) 
      g95_error("Label %d at %C already referenced as branch target", label);
    else
      lp->defined = ST_LABEL_FORMAT;

  } else {
    if (lp->referenced == ST_LABEL_FORMAT)
      g95_error("Label %d at %C already referenced as a format label", label);
    else
      lp->defined = ST_LABEL_TARGET;
  }
}


/* g95_reference_st_label()-- Reference a label.  Given a label number
 * and its type, see if that reference is consistent with what is
 * known about that label, updating the unknown state.  Returns
 * FAILURE if something goes wrong. */

try g95_reference_st_label(int label, g95_sl_type type) {
g95_sl_type label_type;
g95_st_label *lp;
try rc;

  if (label == 0) return SUCCESS;

  lp = get_st_label(label);

  if (lp->defined != ST_LABEL_UNKNOWN)
    label_type = lp->defined;
  else {
    label_type = lp->referenced;
    lp->where = *g95_current_locus();
  }

  if (label_type == ST_LABEL_FORMAT && type == ST_LABEL_TARGET) {
    g95_error("Label %d at %C previously used as a FORMAT label", label);
    rc = FAILURE;
    goto done;
  }

  if (label_type == ST_LABEL_TARGET && type == ST_LABEL_FORMAT) {
    g95_error("Label %d at %C previously used as branch target", label);
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
 * Each namespace consists of symbol nodes linked together by red-black
 * trees adapted from an implementation by Thomas Niemann.  The
 * original is available on his algorithm collection at
 * http://members.xoom.com/thomasn/s_man.htm and was not subject to
 * copyright restrictions.  Thanks Thomas!
 *
 * Given the tricky nature of the fortran grammar, we must be able to
 * undo changes made to a symbol table if the current interpretation
 * of a statement is found to be incorrect.  Whenever a symbol is
 * looked up, we make a copy of it and link to it.  All of these
 * symbols are kept in a singly linked list so that we can commit or
 * undo the changes at a later time.  */

static g95_symbol *changed = NULL;

#define NIL &sentinel           /* all leafs are sentinels */
static g95_redblack sentinel = { NIL, NIL, NULL, BLACK, NULL};

#define CompLT(a,b) (strcmp(a,b) < 0)
#define CompEQ(a,b) (strcmp(a,b) == 0)


/* g95_get_namespace()-- Allocate a new namespace structure.  */

g95_namespace *g95_get_namespace(void) {
g95_namespace *ns;
int i;
 
  ns = g95_getmem(sizeof(g95_namespace));
  ns->root = NIL;
  ns->default_access = ACCESS_PUBLIC;

/* Initialize default types */

  for(i='a'; i<='z'; i++) {
    ns->set_flag[i - 'a'] = 0;

    if (ns->parent != NULL)     /* Copy previous settings */
      ns->default_type[i - 'a'] = ns->parent->default_type[i - 'a'];
    else {
      if ((i >= 'i') && (i <= 'n')) {
	ns->default_type[i - 'a'].type = BT_INTEGER;
	ns->default_type[i - 'a'].kind = g95_default_integer_kind();
      } else {
	ns->default_type[i - 'a'].type = BT_REAL;
	ns->default_type[i - 'a'].kind = g95_default_real_kind();
      }
    }
  }

  return ns;
}


/* rotateLeft()-- rotate node x to left */

static void rotateLeft(g95_namespace *ns, g95_redblack *x) {
g95_redblack *y = x->right;
  
  x->right = y->left;    /* establish x->right link */
  if (y->left != NIL) y->left->parent = x;

  /* establish y->parent link */
  if (y != NIL) y->parent = x->parent;
  if (x->parent) {
    if (x == x->parent->left)
      x->parent->left = y;
    else
      x->parent->right = y;
  } else {
    ns->root = y;
  }

  /* link x and y */
  y->left = x;
  if (x != NIL) x->parent = y;
}


/* rotateRight()-- rotate node x to right */

static void rotateRight(g95_namespace *ns, g95_redblack *x) {
g95_redblack *y = x->left;

  /* establish x->left link */
  x->left = y->right;
  if (y->right != NIL) y->right->parent = x;

  /* establish y->parent link */
  if (y != NIL) y->parent = x->parent;
  if (x->parent) {
    if (x == x->parent->right)
      x->parent->right = y;
    else
      x->parent->left = y;
  } else {
    ns->root = y;
  }

  /* link x and y */
  y->right = x;
  if (x != NIL) x->parent = y;
}


/* insertFixup()-- maintain Red-Black tree balance after inserting node x */

static void insertFixup(g95_namespace *ns, g95_redblack *x) {

  /* check Red-Black properties */

  while (x != ns->root && x->parent->color == RED) {  /* we have a violation */
    if (x->parent == x->parent->parent->left) {
      g95_redblack *y = x->parent->parent->right;
      if (y->color == RED) {

	/* uncle is RED */
	x->parent->color = BLACK;
	y->color = BLACK;
	x->parent->parent->color = RED;
	x = x->parent->parent;
      } else {

	/* uncle is BLACK */
	if (x == x->parent->right) {
	  /* make x a left child */
	  x = x->parent;
	  rotateLeft(ns, x);
	}

	/* recolor and rotate */
	x->parent->color = BLACK;
	x->parent->parent->color = RED;
	rotateRight(ns, x->parent->parent);
      }
    } else {

      /* mirror image of above code */
      g95_redblack *y = x->parent->parent->left;
      if (y->color == RED) {

	/* uncle is RED */
	x->parent->color = BLACK;
	y->color = BLACK;
	x->parent->parent->color = RED;
	x = x->parent->parent;
      } else {

	/* uncle is BLACK */
	if (x == x->parent->left) {
	  x = x->parent;
	  rotateRight(ns, x);
	}
	x->parent->color = BLACK;
	x->parent->parent->color = RED;
	rotateLeft(ns, x->parent->parent);
      }
    }
  }
  ns->root->color = BLACK;
}


/* insert_node()-- Allocate a new red/black node and associate it with
 * the new symbol. */

static void insert_node(g95_namespace *ns, g95_symbol *sym) {
g95_redblack *current, *parent, *x;

  current = ns->root;     /* find future parent */
  parent = NULL;
  while (current != NIL) {
    if (CompEQ(sym->name, current->sym->name))
      g95_internal_error("insert_node(): Node already in tree!");

    parent = current;
    current = CompLT(sym->name, current->sym->name) ?
      current->left : current->right;
  }

  /* setup new node */

  x = g95_getmem(sizeof(g95_redblack));

  x->parent = parent;
  x->left = NIL;
  x->right = NIL;
  x->color = RED;
  x->sym = sym;

    /* insert node in tree */
  if (parent) {
    if (CompLT(sym->name, parent->sym->name))
      parent->left = x;
    else
      parent->right = x;
  } else {
    ns->root = x;
  }

  insertFixup(ns, x);
}


/* deleteFixup()-- maintain Red-Black tree balance after deleting node x */

static void deleteFixup(g95_namespace *ns, g95_redblack *x) {

  while (x != ns->root && x->color == BLACK) {
    if (x == x->parent->left) {
      g95_redblack *w = x->parent->right;

      if (w->color == RED) {
	w->color = BLACK;
	x->parent->color = RED;
	rotateLeft(ns, x->parent);
	w = x->parent->right;
      }
      if (w->left->color == BLACK && w->right->color == BLACK) {
	w->color = RED;
	x = x->parent;
      } else {
	if (w->right->color == BLACK) {
	  w->left->color = BLACK;
	  w->color = RED;
	  rotateRight(ns, w);
	  w = x->parent->right;
	}
	w->color = x->parent->color;
	x->parent->color = BLACK;
	w->right->color = BLACK;
	rotateLeft(ns, x->parent);
	x = ns->root;
      }
    } else {
      g95_redblack *w = x->parent->left;
      if (w->color == RED) {
	w->color = BLACK;
	x->parent->color = RED;
	rotateRight(ns, x->parent);
	w = x->parent->left;
      }
      if (w->right->color == BLACK && w->left->color == BLACK) {
	w->color = RED;
	x = x->parent;
      } else {
	if (w->left->color == BLACK) {
	  w->right->color = BLACK;
	  w->color = RED;
	  rotateLeft(ns, w);
	  w = x->parent->left;
	}
	w->color = x->parent->color;
	x->parent->color = BLACK;
	w->left->color = BLACK;
	rotateRight(ns, x->parent);
	x = ns->root;
      }
    }
  }

  x->color = BLACK;
}


/* delete_node()-- delete a symbol from the tree.  Does not free the
 * symbol itself! */

void delete_node(g95_namespace *ns, g95_symbol *sym) {
g95_redblack *x, *y, *z;

  /* find node in tree */
  z = ns->root;
  while(z != NIL) {
    if (CompEQ(sym->name, z->sym->name)) break;

    z = CompLT(sym->name, z->sym->name) ? z->left : z->right;
  }

  if (z == NIL) g95_internal_error("delete_node(): node not found!");

  if (z->left == NIL || z->right == NIL) { /* y has a NIL node as a child */
    y = z;
  } else {  /* find tree successor with a NIL node as a child */
    y = z->right;
    while (y->left != NIL) y = y->left;
  }

  /* x is y's only child */

  if (y->left != NIL)
    x = y->left;
  else
    x = y->right;

  /* remove y from the parent chain */
  x->parent = y->parent;
  if (y->parent)
    if (y == y->parent->left)
      y->parent->left = x;
    else
      y->parent->right = x;
  else
    ns->root = x;

  if (y != z) z->sym = y->sym;

  if (y->color == BLACK) deleteFixup(ns, x);

  g95_free (y);
}


/* find_node()-- Given a namespace and a name, try to find the symbol
 * within the namespace.  Returns NULL if the symbol is not found. */

static g95_symbol *find_node(g95_namespace *ns, char *name) {
g95_redblack *current = ns->root;

  while(current != NIL) {
    if (CompEQ(name, current->sym->name)) return current->sym;

    current = CompLT (name, current->sym->name) ?
      current->left : current->right;
  }

  return NULL;
}


/* g95_free_symbol()-- Remove a g95_symbol structure and everything it
 * points to. */

void g95_free_symbol(g95_symbol *sym) {

  if (sym->attr.dimension) g95_free_array_spec(&sym->as);

  free_components(sym->components);

  g95_free_interface(sym->interface);
  g95_free_expr(sym->value);

  g95_free_namelist(sym->namelist);

  g95_free(sym);
}


/* new_symbol()-- Allocate and initialize a new symbol node */

static g95_symbol *new_symbol(char *name, g95_namespace *ns) {
g95_symbol *p;

  p = g95_getmem(sizeof(g95_symbol));

  p->ts.type = BT_UNKNOWN;
  g95_clear_attr(&p->attr);
  p->ns = ns;

  p->declared_at = *g95_current_locus();

  if (strlen(name) > G95_MAX_SYMBOL_LEN)
    g95_internal_error("new_symbol(): Symbol name too long");
  strcpy(p->name, name);

/* Add to the tentative list of tentative symbols. */

  p->old_symbol = NULL;
  p->tlink = changed;
  p->mark = 1;
  changed = p;

  return p;
}


/* g95_find_symbol()-- search for a symbol, returning NULL if we can't
 * find it */

g95_symbol *g95_find_symbol(char *name, g95_namespace *ns) {
g95_symbol *p;

  if (ns == NULL) ns = g95_current_ns;

  do {
    p = find_node(ns, name);
    if (p != NULL) break;
    ns = ns->parent;
  } while (ns != NULL);

  return p;
}


/* g95_get_symbol()-- Given a name, search the current namespace on up
 * for the symbol.  If we don't find it anywhere, create the symbol in
 * the current space. */

g95_symbol *g95_get_symbol(char *name, g95_namespace *ns) {
g95_symbol *p;

  if (ns == NULL) ns = g95_current_ns;

  p = g95_find_symbol(name, ns);

  if (p != NULL && p->mark == 0) {
    p->mark = 1;

    if (p->old_symbol == NULL) { /* Copy in case this symbol is changed */
      p->old_symbol = g95_getmem(sizeof(g95_symbol));
      *(p->old_symbol) = *p;

      p->tlink = changed;
      changed = p;
    }
  }

  if (p == NULL) {
    p = new_symbol(name, ns);
    insert_node(ns, p);
  }

  return p;
}


/* delete_symbol()-- Delete a symbol */

static void delete_symbol(g95_symbol *sym) {

  delete_node(sym->ns, sym);
  g95_free_symbol(sym);
}


/* g95_undo_symbols()-- Undoes all the changes made to symbols in the
 * current statement.  This subroutine is made simpler due to the fact
 * that attributes are never removed once added. */

void g95_undo_symbols(void) {
g95_symbol *p, *q, *old;

/* if (changed != NULL) g95_status("Undoing symbols\n"); */

  for(p=changed; p; p=q) {
    q = p->tlink;
    /*    g95_status("Undoing %s\n", p->name); */

    if (p->old_symbol == NULL) {  /* Symbol was new */
      delete_symbol(p);
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

    if (p->as.rank != old->as.rank) {
      g95_free_array_spec(&p->as);
      p->as = old->as;
    }

    if (p->interface != old->interface) {
      g95_free_interface(p->interface);
      p->interface = old->interface;
    }

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

    g95_free(p->old_symbol);
    p->old_symbol = NULL;
    p->tlink = NULL;
  }

  changed = NULL;
}


/* g95_commit_symbols()-- Makes the changes made in the current
 * statement permanent-- gets rid of undo information. */

void g95_commit_symbols(void) {
g95_symbol *p, *q;

//  if (changed != NULL) g95_status("Committing symbols\n");

  for(p=changed; p; p=q) {
    q = p->tlink;
    p->tlink = NULL;
    p->mark = 0;

    if (p->old_symbol != NULL) {
      g95_free(p->old_symbol);
      p->old_symbol = NULL;
    }
  }

  changed = NULL;
}


/* free_rb_tree()-- Recursive function that deletes an entire
 * red-black tree and all the symbols that it contains. */

static void free_rb_tree(g95_redblack *rb) {

  if (rb == NIL) return;

  free_rb_tree(rb->left);
  free_rb_tree(rb->right);

  g95_free_symbol(rb->sym);

  g95_free(rb);
}


/* g95_free_namespace()-- Free a namespace structure and everything
 * below it.  Interface lists associated with intrinsic operators are
 * not freed.  These are take care of when a specific name is freed. */

void g95_free_namespace(g95_namespace *ns) {
g95_charlen *cl, *cl2;

  if (ns == NULL) return; 

  g95_free_statements(ns->code);

  free_rb_tree(ns->root);

  for(cl=ns->cl_list; cl; cl=cl2) {
    cl2 = cl->next;
    g95_free_expr(cl->length);
  }

  free_st_labels(ns->st_labels);

  g95_free_equiv(ns->equiv);

  g95_free(ns);
}


/* free_all_symbols()-- Free all of the namespaces, starting with the
 * current namespace, its siblings and parents.  Relies on the fact
 * sibling can only occur on the bottom level. */

static void free_all_symbols(void) {
g95_namespace *ns;

  for(;;) {
    ns = g95_current_ns->sibling;
    if (ns == NULL) break;

    g95_free_namespace(g95_current_ns);
    g95_current_ns = ns;
  }

  do {
    ns = g95_current_ns->parent;
    g95_free_namespace(g95_current_ns);
    g95_current_ns = ns;

  } while(g95_current_ns != NULL);
}


void g95_symbol_init_2(void) {

  g95_current_ns = g95_get_namespace();
}


void g95_symbol_done_2(void) {

  free_all_symbols();
}


/* g95_show_symbol()-- Show a symbol.  If a symbol is an ENTRY,
 * SUBROUTINE or FUNCTION, we show the interface.  Information needed
 * to reconstruct the list of specific interfaces associated with a
 * generic symbol is done within that symbol. */

void g95_show_symbol(g95_symbol *sym) {
g95_symbol *s;

  if (sym == NULL) {
    g95_status("()");
    return;
  }

  g95_status("(symbol %s ", sym->name);

  g95_show_typespec(&sym->ts);

  g95_show_attr(&sym->attr);

  if ((sym->attr.flavor == FL_SUBROUTINE ||
       sym->attr.flavor == FL_FUNCTION ||
       sym->attr.entry) && sym->interface != NULL)
    g95_show_formal_arglist(sym->interface->formal);
  else g95_status("()");

  g95_show_expr(sym->value);

  g95_show_array_spec(&sym->as);

  g95_status_char('(');
  for(s=sym->common_head; s; s=s->common_next) {
    g95_status("%s", s->name);

    if (s->common_next != NULL)
      g95_status_char(' ');
  }
  g95_status_char(')');

  if (sym->result == NULL) g95_status(" () ");
   else g95_status(" %s ", sym->result->name);

  g95_show_components(sym);

  g95_status(")\n");
}


/* traverse_ns()-- Recursive namespace traversal function. */

static void traverse_ns(g95_redblack *rb, void (*func)(g95_symbol *)) {

  if (rb != NIL) {
    (*func)(rb->sym);

    traverse_ns(rb->left, func);
    traverse_ns(rb->right, func);
  }
}

/* g95_traverse_ns()-- Call a given function for all symbols in the
 * namespace */

void g95_traverse_ns(g95_namespace *ns, void (*func)(g95_symbol *)) {

  traverse_ns(ns->root, func);
}


/* save_symbol()-- Given a symbol, mark it as SAVEd if it is allowed */

static void save_symbol(g95_symbol *sym) {

  if (sym->attr.common &&
      g95_add_saved_common(&sym->attr, NULL) == FAILURE) goto fatal;

  if (sym->attr.in_common || sym->attr.dummy ||
      sym->attr.flavor != FL_VARIABLE) return;

  if (g95_add_save(&sym->attr, NULL) == SUCCESS) return;

fatal:
  g95_internal_error("save_symbol(): Tried to save the unsaveable!");
}


/* g95_save_all()-- Mark those symbols which can be SAVEd as such. */

void g95_save_all(g95_namespace *ns) {

  g95_traverse_ns(ns, save_symbol);
}


/* set_sym_defaults()-- Sets the default type for a particular symbol
 * if it doesn't have one already.  Meant to be called from
 * g95_set_sym_defaults() via traverse_ns(). */

static void set_sym_defaults(g95_symbol *sym) {
sym_flavor flavor;

  if (sym->ts.type != BT_UNKNOWN) return;

  flavor = sym->attr.flavor;

  if (flavor != FL_FUNCTION && flavor != FL_VARIABLE &&
      flavor != FL_PARAMETER && flavor != FL_ST_FUNCTION) return;

  g95_set_default_type(sym);
}


/* g95_set_sym_defaults()-- Set all symbols that don't have a type to
 * their name-dependent default type. */

void g95_set_sym_defaults(g95_namespace *ns) {

  g95_traverse_ns(ns, set_sym_defaults);
}


/* show_generics()-- If a symbol is a generic interface symbol, run
 * through the list of specific symbols */

static void show_generics(g95_symbol *sym) {
g95_interface *ip;

  if (sym->attr.generic) {
    g95_status("(%s", sym->name);

    for(ip=sym->generic; ip; ip=ip->next)
      g95_status(" %s", ip->sym->name);

    g95_status(")");
  }
}


/* g95_show_namespace()-- Show a namespace */

void g95_show_namespace(g95_namespace *ns) {
g95_interface *ip;
int i;

  g95_status("(namespace (");

  if (ns != NULL) {
    for(i=0; i<G95_LETTERS; i++)
      g95_show_typespec(&ns->default_type[i]);

    g95_status(")\n");

    g95_traverse_ns(ns, g95_show_symbol);

    g95_status(" (");
    g95_traverse_ns(ns, show_generics);   /* Generic interfaces */

    for(i=0; i<G95_INTRINSIC_OPS; i++) {    /* User operator interfaces */
      ip = ns->operator[i];
      if (ip == NULL) continue;

      g95_status("(%s", g95_op2string(i));

      for(; ip; ip=ip->next)
	g95_status(" %s", ip->sym->name);

      g95_status(")");
    }
  }

  g95_status("))\n");
  g95_show_code(0, ns->code);

  for(ns=ns->contained; ns; ns=ns->sibling) {
    g95_status("CONTAINS\n");
    g95_show_namespace(ns);
  }
}


/* Scaffolding */

#ifdef G95_DEBUG

/* g95_symbol_state()-- Makes sure that no changes to symbols are pending */

void g95_symbol_state(void) {

  if (changed != NULL)
    g95_internal_error("Symbol changes still pending");
}

#endif
