/* Declaration statement matcher
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


/* decl.c -- Declaration statement matcher.  */

#include "g95.h"


/* This flag is set if a an old-style length selector is matched
 * during an type-declaration statement. */

static int old_char_selector;

/* When variables aquire types and attributes from a declaration
 * statement, they get them from the following static variables.  The
 * first part of a declaration sets these variables and the second
 * part copies these into symbol structures. */

static g95_typespec current_ts;

static symbol_attribute current_attr;
static g95_array_spec *current_as;
static int colon_seen;

/* g95_new_block points to the symbol of a newly matched block. */

g95_symbol *g95_new_block;


/* match_intent_spec()-- Match an intent specification.  Since this
 * can only happen after an INTENT word, a legal intent-spec must
 * follow. */

static sym_intent match_intent_spec(void) {

  if (g95_match(" ( in out )") == MATCH_YES) return INTENT_INOUT;
  if (g95_match(" ( in )") == MATCH_YES)     return INTENT_IN;
  if (g95_match(" ( out )") == MATCH_YES)    return INTENT_OUT;

  g95_error("Bad INTENT specification at %C");
  return INTENT_UNKNOWN;
}


/* char_len_param_value()-- Matches a character length specification,
 * which is either a specification expression or a '*'. */

static match char_len_param_value(g95_expr **exp) {

  if (g95_match(" *") == MATCH_YES) {
    exp = NULL;
    return MATCH_YES;
  }

  return g95_match(" %e", exp);
}


/* match_char_length()-- A character length is a '*' followed by a
 * literal integer or a char_len_param_value in parenthesis. */

static match match_char_length(g95_expr **exp) {
int length;
match m;

  m = g95_match(" *");
  if (m != MATCH_YES) return m;

  m = g95_match_small_literal_int(&length);
  if (m == MATCH_ERROR) return m;

  if (m == MATCH_YES) {
    *exp = g95_int_expr(length);
    return m;
  }

  if (g95_match(" (") == MATCH_NO) goto syntax;

  m = char_len_param_value(exp);
  if (m == MATCH_ERROR) return m;
  if (m == MATCH_NO) goto syntax;

  if (g95_match(" )") == MATCH_NO) {
    g95_free_expr(*exp);
    *exp = NULL;
    goto syntax;
  }

  return MATCH_YES;

syntax:
  g95_error("Syntax error in character length specification at %C");
  return MATCH_ERROR;
}


/* find_special()-- Special subroutine for finding a symbol.  If we're
 * compiling a function or subroutine and the parent compilation unit
 * is an interface, then check to see if the name we've been given is
 * the name of the interface (located in another namespace).  If so,
 * return that symbol.  If not, use g95_get_symbol(). */

static int find_special(char *name, g95_symbol **result) {
g95_state_data *s;

  if (g95_current_state() != COMP_SUBROUTINE &&
      g95_current_state() != COMP_FUNCTION) goto normal;

  s = g95_state_stack->previous;
  if (s == NULL) goto normal;

  if (s->state != COMP_INTERFACE) goto normal;
  if (s->sym == NULL) goto normal;   /* Nameless interface */

  if (strcmp(name, s->sym->name ) == 0) {
    *result = s->sym;
    return 0;
  }

normal:
  return g95_get_symbol(name, NULL, 0, result);
}


/* get_proc_name()-- Special subroutine for getting a symbol node
 * associated with a procedure name, used in SUBROUTINE and FUNCTION
 * statements.  Normally, we just g95_get_symbol() without searching
 * any parent units.  If we are compiling an interface, we
 * search/create in the parent unit and create a link from the current
 * namespace.  Returns value from g95_get_symbol() */

static int get_proc_name(char *name, g95_symbol **result) {

  if (g95_current_state() == COMP_INTERFACE)
    return g95_get_symbol(name, g95_current_ns->parent, 0, result);

  return g95_get_symbol(name, NULL, 0, result);
}


/* build_sym()-- Function called by variable_decl() that adds a name
 * to the symbol table. */

static try build_sym(char *name, g95_charlen *cl, g95_expr **initp,
		     g95_array_spec **as, locus *var_locus) {
symbol_attribute attr;
g95_symbol *sym;
g95_expr *init;

  init = *initp;
  if (find_special(name, &sym)) return FAILURE;

/* Start updating the symbol table.  Add basic type attribute if present */

  if (current_ts.type != BT_UNKNOWN) {
    if (sym->ts.type != BT_UNKNOWN) {
      g95_error("Symbol at %L already has basic type of %s", var_locus,
		g95_typename(sym->ts.type));
      return FAILURE;
    }

    sym->ts = current_ts;
  }

  if (sym->ts.type == BT_CHARACTER) sym->ts.cl = cl;

/* Add dimension attribute if present. */

  if (g95_set_array_spec(sym, *as, var_locus) == FAILURE) return FAILURE;
  *as = NULL;

/* Add attribute to symbol.  The copy is so that we can reset the
 * dimension attribute. */

  attr = current_attr;
  attr.dimension = 0;

  if (g95_copy_attr(&sym->attr, &attr, var_locus) == FAILURE) return FAILURE;

/* Add initializer, required for PARAMETERs. */

  if (init == NULL) {
    if (sym->attr.flavor == FL_PARAMETER) {
      g95_error("PARAMETER at %L is missing an initializer", var_locus);
      return FAILURE;
    }
  } else {
    if (g95_check_assign_symbol(sym, init) == FAILURE) return FAILURE;

    sym->value = init;
    *initp = NULL;
  }

  return SUCCESS;
}


/* build_struct()-- Function called by variable_decl() that adds a
 * name to a structure being built. */

static try build_struct(char *name, g95_charlen *cl, g95_expr **init,
			g95_array_spec **as) {
g95_component *c;

  if ((current_ts.type == BT_DERIVED) &&
      (current_ts.derived == g95_current_block()) &&
      (current_attr.pointer == 0)) {
    g95_error("Component at %C must have the POINTER attribute");
    return FAILURE;
  }

  if (g95_current_block()->attr.pointer && (*as)->rank != 0) {
    if ((*as)->type != AS_DEFERRED && (*as)->type != AS_EXPLICIT) {
      g95_error("Array component of structure at %C must have explicit "
		"or deferred shape");
      return FAILURE;
    }
  }

  if (g95_add_component(g95_current_block(), name, &c) == FAILURE)
    return FAILURE;

  c->ts = current_ts;
  c->ts.cl = cl;
  g95_set_component_attr(c, &current_attr);

  c->initializer = *init;
  *init = NULL;

  c->as = *as;
  *as = NULL;

  return SUCCESS;
}


/* variable_decl()-- Match a variable name with an optional
 * initializer.  When this subroutine is called, a variable is
 * expected to be parsed next.  Depending on what is happening at the
 * moment, updates either the symbol table or the current
 * interface. */

static match variable_decl(void) {
char name[G95_MAX_SYMBOL_LEN+1];
g95_expr *initializer, *char_len;
g95_array_spec *as;
g95_charlen *cl;
locus var_locus;
match m;
try t;

  initializer = NULL;
  as = NULL;

  m = g95_match_name(name);
  if (m != MATCH_YES) goto cleanup;

  var_locus = *g95_current_locus();

  m = g95_match_array_spec(&as);
  if (m == MATCH_ERROR) goto cleanup;
  if (m == MATCH_NO) as = g95_copy_array_spec(current_as);

  char_len = NULL;
  cl = NULL;

  if (current_ts.type == BT_CHARACTER) {
    switch(match_char_length(&char_len)) {
    case MATCH_YES:
      cl = g95_get_charlen();
      cl->next = g95_current_ns->cl_list;
      g95_current_ns->cl_list = cl;

      cl->length = char_len;
      break;

    case MATCH_NO:
      cl = current_ts.cl;
      break;

    case MATCH_ERROR:
      goto cleanup;
    }
  }

/* The double colon must be present in order to have initializers.
 * Otherwise the statement is ambiguous with an assignment statement. */

  if (colon_seen) {
    if (g95_match(" =>") == MATCH_YES)
      g95_internal_error("variable_decl(): Not ready for pointer "
			 "initializers yet");

    if (g95_match(" =") == MATCH_YES) {
      m = g95_match_init_expr(&initializer);
      if (m == MATCH_NO)
	g95_error("Expected an initialization expression at %C");

      if (m != MATCH_YES) goto cleanup;
    }
  }

/* In functions that have a RESULT variable defined, the function name
 * always refers to function calls.  Therefore, the name is not
 * allowed to appear in specification statements. */

  if (g95_current_state() == COMP_FUNCTION && g95_current_block() != NULL &&
      g95_current_block()->result != NULL &&
      strcmp(g95_current_block()->name, name) == 0) {
    g95_error("Function name '%s' not allowed at %C", name);
    goto cleanup;
  }

  if (g95_current_state() == COMP_DERIVED)
    t = build_struct(name, cl, &initializer, &as);
  else
    t = build_sym(name, cl, &initializer, &as, &var_locus);

  m = (t == SUCCESS) ? MATCH_YES : MATCH_ERROR;

/* Free stuff up and return */

cleanup:
  g95_free_expr(initializer);
  g95_free_array_spec(as);

  return m;
}


/* g95_match_old_kind_spec()-- Match an extended-f77 kind specification */

match g95_match_old_kind_spec(g95_typespec *ts) {
match m;
  
  if (g95_match(" *") != MATCH_YES) return MATCH_NO;

  m = g95_match_small_literal_int(&ts->kind);
  if (m != MATCH_YES) return MATCH_ERROR;

/* Massage the kind numbers for complex types */

  if (ts->type == BT_COMPLEX && ts->kind == 8) ts->kind = 4;
  if (ts->type == BT_COMPLEX && ts->kind == 16) ts->kind = 8;

  if (g95_validate_kind(ts->type, ts->kind) == -1) {
    g95_error("Old-style kind %d not supported for type %s at %C", ts->kind,
	      g95_typename(ts->type));

    return MATCH_ERROR;
  }

  return MATCH_YES;
}


/* g95_match_kind_spec()-- Match a kind specification.  Since kinds
 * are generally optional, we usually return MATCH_NO if something
 * goes wrong.  If a "kind=" string is found, then we know we have an
 * error. */

match g95_match_kind_spec(g95_typespec *ts) {
locus cur_loc;
g95_expr *e;
match m, n;
char *msg;

  m = MATCH_NO;
  e = NULL;

  cur_loc = *g95_current_locus();

  if (g95_match(" (") == MATCH_NO) return MATCH_NO;

/* Also gobbles optional text */
  if (g95_match(" kind = ") == MATCH_YES) m = MATCH_ERROR;

  n = g95_match_init_expr(&e);
  if (n == MATCH_NO) g95_error("Expected initialization expression at %C");
  if (n != MATCH_YES) return MATCH_ERROR;

  if (e->as != NULL) {
    g95_error("Expected scalar initialization expression at %C");
    m = MATCH_ERROR;
    goto no_match;
  }

  msg = g95_extract_int(e, &ts->kind);
  if (msg != NULL) {
    g95_error(msg);
    m = MATCH_ERROR;
    goto no_match;
  }

  g95_free_expr(e);
  e = NULL;

  if (g95_validate_kind(ts->type, ts->kind) == -1) {
    g95_error("Kind %d not supported for type %s at %C", ts->kind,
	      g95_typename(ts->type));

    m = MATCH_ERROR;
    goto no_match;
  }

  if (g95_match(" )") != MATCH_YES) {
    g95_error("Missing right paren at %C");
    goto no_match;
  }

  return MATCH_YES;

no_match:
  g95_free_expr(e);
  g95_set_locus(&cur_loc);
  return m;
}


/* match_char_spec()-- Match the various kind/length specifications in
 * a CHARACTER declaration.  We don't return MATCH_NO. */

static match match_char_spec(g95_typespec *ts, int type_decl) {
int i, kind, seen_length;
g95_charlen *cl;
g95_expr *len;
match m;

  kind = g95_default_character_kind(); 
  len = NULL;
  seen_length = 0;

/* Try the old-style specification first */

  old_char_selector = 0;

  m = match_char_length(&len);
  if (m != MATCH_NO) {
    if (m == MATCH_YES) old_char_selector = 1;
    seen_length = 1;
    goto done;
  }

  m = g95_match(" (");
  if (m != MATCH_YES) {
    m = MATCH_YES;  /* character without length is a single char */
    goto done;
  }

/* Try the weird case:  ( KIND = <int> [ , LEN = <len-param> ] )   */

  if (g95_match(" kind =") == MATCH_YES) {
    m = g95_match_small_int(&kind);
    if (m == MATCH_ERROR) goto done;
    if (m == MATCH_NO) goto syntax;

    if (g95_match(" , len =") == MATCH_NO) goto rparen;

    m = char_len_param_value(&len);
    if (m == MATCH_NO) goto syntax;
    if (m == MATCH_ERROR) goto done;
    seen_length = 1;

    goto rparen;
  }

/* Try to match ( LEN = <len-param> ) or ( LEN = <len-param>, KIND = <int> ) */

  if (g95_match(" len =") == MATCH_YES) {
    m = char_len_param_value(&len);
    if (m == MATCH_NO) goto syntax;
    if (m == MATCH_ERROR) goto done;
    seen_length = 1;

    if (g95_match(" )") == MATCH_YES) goto done;

    if (g95_match(" , kind =") != MATCH_YES) goto syntax;

    g95_match_small_int(&kind);

    if (g95_validate_kind(BT_CHARACTER, kind) == -1) {
      g95_error("Kind %d is not a CHARACTER kind at %C", kind);
      return MATCH_YES;
    }

    goto rparen;
  }

/* Try to match   ( <len-param> ) or ( <len-param> , [ KIND = ] <int> ) */

  m = char_len_param_value(&len);
  if (m == MATCH_NO) goto syntax;
  if (m == MATCH_ERROR) goto done;
  seen_length = 1;

  m = g95_match(" )");
  if (m == MATCH_YES) goto done;

  if (g95_match(" ,") != MATCH_YES) goto syntax;

  g95_match(" kind =");   /* Gobble optional text */

  m = g95_match_small_int(&kind);
  if (m == MATCH_ERROR) goto done;
  if (m == MATCH_NO) goto syntax;

/* require a right-paren at this point */

rparen:
  m = g95_match(" )");
  if (m == MATCH_YES) goto done;

syntax:
  g95_error("Syntax error in CHARACTER declaration at %C");
  m = MATCH_ERROR;

done:
  if (m == MATCH_YES && g95_validate_kind(BT_CHARACTER, kind) == -1) {
    g95_error("Kind %d is not a CHARACTER kind at %C", kind);
    m = MATCH_ERROR;
  }

  if (m != MATCH_YES) {
    g95_free_expr(len);
    len = NULL;
    return m;
  }

/* Do some final massaging of the length values */
 
  cl = g95_get_charlen();
  cl->next = g95_current_ns->cl_list;
  g95_current_ns->cl_list = cl;

  if (seen_length == 0)
    cl->length = g95_int_expr(1);
  else {
    if (len == NULL || g95_extract_int(len, &i) != NULL || i >= 0)
      cl->length = len;
    else {
      g95_free_expr(len);
      cl->length = g95_int_expr(0);
    }
  }

  ts->cl = cl;
  ts->kind = kind;

  return MATCH_YES;
}


/* g95_match_type_spec()-- Matches a type specification.  If
 * successful, sets the ts structure to the matched specification.
 * This is necessary for FUNCTION and IMPLICIT statements.
 *
 * If kind_flag is nonzero, then we check for the optional kind
 * specification.  Not doing so is needed for matching an IMPLICIT
 * statement correctly. */

match g95_match_type_spec(g95_typespec *ts, int kind_flag, int data_decl) {
char name[G95_MAX_SYMBOL_LEN+1];
g95_symbol *sym;
match m;

  g95_clear_ts(ts);

  if (g95_match(" integer") == MATCH_YES) {
    ts->type = BT_INTEGER;
    ts->kind = g95_default_integer_kind();
    goto get_kind;
  }

  if (g95_match(" character") == MATCH_YES) {
    ts->type = BT_CHARACTER;
    return match_char_spec(ts, data_decl);
  }

  if (g95_match(" real") == MATCH_YES) {
    ts->type = BT_REAL;
    ts->kind = g95_default_real_kind();
    goto get_kind;
  }

  if (g95_match(" double precision") == MATCH_YES) {
    ts->type = BT_REAL;
    ts->kind = g95_default_double_kind();
    return MATCH_YES;
  }

  if (g95_match(" complex") == MATCH_YES) {
    ts->type = BT_COMPLEX;
    ts->kind = g95_default_complex_kind();
    goto get_kind;
  }

  if (g95_match(" double complex") == MATCH_YES) {
    ts->type = BT_COMPLEX;
    ts->kind = g95_default_double_kind();
    return MATCH_YES;
  }

  if (g95_match(" logical") == MATCH_YES) {
    ts->type = BT_LOGICAL;
    ts->kind = g95_default_logical_kind();
    goto get_kind;
  }

  m = g95_match(" type ( %n )", name);
  if (m != MATCH_YES) return m;

  if (data_decl) {  /* Require type to exist now */
    if (g95_find_symbol(name, NULL, 1, &sym)) return MATCH_ERROR;

    if (sym == NULL) {
      g95_error("Derived type '%s' at %C has not been defined", name);
      return MATCH_ERROR;
    }

    if (sym->attr.flavor != FL_DERIVED) {
      g95_error("Name '%s' at %C is not a derived type", name);
      return MATCH_ERROR;
    }

  } else {     /* Allow the type to be defined later. */

    if (g95_get_symbol(name, NULL, 0, &sym)) return MATCH_ERROR;

    if (sym->attr.flavor != FL_DERIVED &&
	g95_add_flavor(&sym->attr, FL_DERIVED, NULL) == FAILURE)
      return MATCH_ERROR;
  }

  ts->type = BT_DERIVED;
  ts->kind = 0;
  ts->derived = sym;

  return MATCH_YES;

/* For all types except double, derived and character, look for an
 * optional kind specifier.  MATCH_NO is actually OK at this point. */

get_kind:
  if (kind_flag == 0) return MATCH_YES;

  m = g95_match_kind_spec(ts);
  if (m == MATCH_NO && ts->type != BT_CHARACTER)
    m = g95_match_old_kind_spec(ts);

  if (m == MATCH_NO) m = MATCH_YES;  /* No kind specifier found */

  return m;
}


/* match_attr_spec()-- Matches an attribute specification including
 * array specs.  If successful, leaves the variables current_attr and
 * current_as holding the specification.  Also sets the colon_seen
 * variable for later use by matchers associated with initializations.
 *
 * This subroutine is a little tricky in the sense that we don't know
 * if we really have an attr-spec until we hit the double colon.
 * Until that time, we can only return MATCH_NO.  This forces us to
 * check for duplicate specification at this level.  */

static match match_attr_spec(void) {

/* Modifiers that can exist in a type statement */

typedef enum { DECL_ALLOCATABLE=0, DECL_DIMENSION, DECL_EXTERNAL,
   DECL_IN, DECL_OUT, DECL_INOUT, DECL_INTRINSIC, DECL_OPTIONAL,
   DECL_PARAMETER, DECL_POINTER, DECL_PRIVATE, DECL_PUBLIC, DECL_SAVE,
   DECL_TARGET, DECL_COLON, DECL_NONE
} decl_types;

#define NUM_DECL (DECL_TARGET+1)  /* DECL_TARGET is the last attribute */

static mstring decls[] = {
   minit(", allocatable",        DECL_ALLOCATABLE),
   minit(", dimension",          DECL_DIMENSION),
   minit(", external",           DECL_EXTERNAL),
   minit(", intent ( in )",      DECL_IN),
   minit(", intent ( out )",     DECL_OUT),
   minit(", intent ( in out )",  DECL_INOUT),
   minit(", intrinsic",          DECL_INTRINSIC),
   minit(", optional",           DECL_OPTIONAL),
   minit(", parameter",          DECL_PARAMETER),
   minit(", pointer",            DECL_POINTER),
   minit(", private",            DECL_PRIVATE),
   minit(", public",             DECL_PUBLIC),
   minit(", save",               DECL_SAVE),
   minit(", target",             DECL_TARGET),
   minit("::",                   DECL_COLON),
   minit(NULL, DECL_NONE)
};

locus start, seen_at[NUM_DECL];
int i, seen[NUM_DECL];
decl_types d;
char *attr;
match m;
try t;

  g95_clear_attr(&current_attr);
  start = *g95_current_locus();

  current_as = NULL;

/* See if we get all of the keywords up to the final double colon */

  for(i=0; i<NUM_DECL; i++)
    seen[i] = 0;

  for(;;) {
    d = g95_match_strings(decls);
    if (d == DECL_NONE || d == DECL_COLON) break;

    seen[d]++;
    seen_at[d] = *g95_current_locus();

    if (d == DECL_DIMENSION &&
	g95_match_array_spec(&current_as) != MATCH_YES) {
      m = MATCH_NO;  /* If we had an error here, it'll be back */
      goto cleanup;
    }
  }

/* No double colon, so assume that we've been looking at something
 * else the whole time */

  if (d == DECL_NONE) {
    m = MATCH_NO;
    goto cleanup;
  }

/* Since we've seen a double colon, we have to be looking at an
 * attr-spec.  This means that we can now issue errors */

  for(d=0; d<NUM_DECL; d++)
    if (seen[d] > 1) {
      switch(d) {
      case DECL_ALLOCATABLE:  attr = "ALLOCATABLE";      break;
      case DECL_DIMENSION:    attr = "DIMENSION";        break;
      case DECL_EXTERNAL:     attr = "EXTERNAL";         break;
      case DECL_IN:           attr = "INTENT (IN)";      break;
      case DECL_OUT:          attr = "INTENT (OUT)";     break;
      case DECL_INOUT:        attr = "INTENT (IN OUT)";  break;
      case DECL_INTRINSIC:    attr = "INTRINSIC";        break;
      case DECL_OPTIONAL:     attr = "OPTIONAL";         break;
      case DECL_PARAMETER:    attr = "PARAMETER";        break;
      case DECL_POINTER:      attr = "POINTER";          break;
      case DECL_PRIVATE:      attr = "PRIVATE";          break;
      case DECL_PUBLIC:       attr = "PUBLIC";           break;
      case DECL_SAVE:         attr = "SAVE";             break;
      case DECL_TARGET:       attr = "TARGET";           break;
      default:
	attr = NULL;  /* This shouldn't happen */
      }

      g95_error("Duplicate %s attribute at %L", attr, &seen_at[d]);
      m = MATCH_ERROR;
      goto cleanup;
    }

/* Now that we've dealt with duplicate attributes, add the attributes to the 
 * current attribute. */

  for(d=0; d<NUM_DECL; d++) {
    if (seen[d] == 0) continue;

    if (g95_current_state() == COMP_DERIVED &&
	d != DECL_DIMENSION && d != DECL_POINTER &&
	d != DECL_COLON && d != DECL_NONE) {

      g95_error("Attribute at %L is not allowed in a TYPE definition",
		&seen_at[d]);
      m = MATCH_ERROR;
      goto cleanup;
    }

    switch(d) {
    case DECL_ALLOCATABLE:
      t = g95_add_allocatable(&current_attr, &seen_at[d]);
      break;

    case DECL_DIMENSION:
      t = g95_add_dimension(&current_attr, &seen_at[d]);
      break;

    case DECL_EXTERNAL:
      t = g95_add_external(&current_attr, &seen_at[d]);
      break;

    case DECL_IN:
      t = g95_add_intent(&current_attr, INTENT_IN, &seen_at[d]);
      break;

    case DECL_OUT:
      t = g95_add_intent(&current_attr, INTENT_OUT, &seen_at[d]);
      break;

    case DECL_INOUT:
      t = g95_add_intent(&current_attr, INTENT_INOUT, &seen_at[d]);
      break;

    case DECL_INTRINSIC:
      t = g95_add_intrinsic(&current_attr, &seen_at[d]);
      break;

    case DECL_OPTIONAL:
      t = g95_add_optional(&current_attr, &seen_at[d]);
      break;

    case DECL_PARAMETER:
      t = g95_add_flavor(&current_attr, FL_PARAMETER, &seen_at[d]);
      break;

    case DECL_POINTER:
      t = g95_add_pointer(&current_attr, &seen_at[d]);
      break;

    case DECL_PRIVATE:
      t = g95_add_access(&current_attr, ACCESS_PRIVATE, &seen_at[d]);
      break;

    case DECL_PUBLIC:
      t = g95_add_access(&current_attr, ACCESS_PUBLIC, &seen_at[d]);
      break;

    case DECL_SAVE:
      t = g95_add_save(&current_attr, &seen_at[d]);
      break;

    case DECL_TARGET:
      t = g95_add_target(&current_attr, &seen_at[d]);
      break;

    default:
      g95_internal_error("match_attr_spec(): Bad attribute");
    }

    if (t == FAILURE) {
      m = MATCH_ERROR;
      goto cleanup;
    }
  }

  colon_seen = 1;
  return MATCH_YES;

cleanup:
  g95_set_locus(&start);
  g95_free_array_spec(current_as);
  current_as = NULL;
  return m;
}


/* g95_match_data_decl()-- Match a data declaration statement */

match g95_match_data_decl(void) {
match m;

  m = g95_match_type_spec(&current_ts, 1, 1);
  if (m != MATCH_YES) return m;

  m = match_attr_spec();
  if (m == MATCH_ERROR) {
    m = MATCH_NO;
    goto cleanup;
  }

/* Explanation is required here.  If we have an old-style character
 * declaration, and no new-style attribute specifications, then there
 * a comma is optional between the type specification and the variable
 * list. */

  if (m == MATCH_NO && current_ts.type == BT_CHARACTER && old_char_selector)
    g95_match(" ,");

/* Give the types/attributes to symbols that follow */

  for(;;) {
    m = variable_decl();
    if (m == MATCH_ERROR) goto cleanup;
    if (m == MATCH_NO) break;

    if (g95_match_eos() == MATCH_YES) goto cleanup;
    if (g95_match(" ,") != MATCH_YES) break;
  }
  
  g95_error("Syntax error in data declaration at %C");
  m = MATCH_ERROR;

cleanup:
  g95_free_array_spec(current_as);
  current_as = NULL;
  return m;
}


/* match_prefix()-- Match a prefix associated with a function or
 * subroutine declaration.  If the typespec pointer is nonnull, then a
 * typespec can be matched.  Note that if nothing matches, MATCH_YES
 * is returned (the null string was matched).  This condition can be
 * detected by comparing *attr with zero.  */

static match match_prefix(symbol_attribute *attr, g95_typespec *ts) {
int seen_type;

  g95_clear_attr(attr); 
  seen_type = 0;

loop:
  if (!seen_type && ts != NULL &&
      g95_match_type_spec(ts, 1, 0) == MATCH_YES &&
      g95_match_space() == MATCH_YES) {

    seen_type = 1;
    goto loop;
  }

  if (g95_match("elemental% ") == MATCH_YES ) {
    if (g95_add_elemental(attr, NULL) == FAILURE)
      return MATCH_ERROR;

    goto loop;
  }

  if (g95_match("pure% ") == MATCH_YES) {
    if (g95_add_pure(attr, NULL) == FAILURE)
      return MATCH_ERROR;

    goto loop;
  }

  if (g95_match("recursive% ") == MATCH_YES) {
    if (g95_add_recursive(attr, NULL) == FAILURE)
      return MATCH_ERROR;

    goto loop;
  }

/* At this point, the next item is not a prefix */

  return MATCH_YES;
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


/* g95_match_formal_arglist()-- Match a formal argument list. */

match g95_match_formal_arglist(g95_symbol *progname, int st_flag) {
g95_formal_arglist *head, *tail, *p;
char name[G95_MAX_SYMBOL_LEN+1];
g95_symbol *sym;
match m;

  head = tail = NULL;

  if (g95_match(" (") != MATCH_YES) {
    if (!st_flag) goto ok;
    return MATCH_NO;
  }

  if (g95_match(" )") == MATCH_YES) goto ok;

  for(;;) {
    if (g95_match(" *") == MATCH_YES)
      sym = NULL;
    else {
      m = g95_match_name(name);
      if (m != MATCH_YES) goto cleanup;

      if (g95_get_symbol(name, NULL, 0, &sym)) goto cleanup;
    }

    p = g95_getmem(sizeof(g95_formal_arglist));

    if (head == NULL)
      head = tail = p;
    else {
      tail->next = p;
      tail = p;
    }

    tail->sym = sym;

/* Duplicate symbols in an argument list are detected when we add the
 * DUMMY flavor.  We don't add the VARIABLE flavor because the name
 * could be a dummy procedure.  We don't apply these attributes to
 * formal arguments of statement functions. */

    if (sym != NULL && !st_flag &&
	(g95_add_dummy(&sym->attr, NULL) == FAILURE ||
	 g95_missing_attr(&sym->attr, NULL) == FAILURE)) {
      m = MATCH_ERROR;
      goto cleanup;
    }

/* The name of a program unit can be in a different namespace, so
 * check for it explicitly.  After the statement is accepted, the name
 * is checked for especially in g95_get_symbol(). */

    if (g95_new_block != NULL && sym != NULL &&
	strcmp(sym->name, g95_new_block->name) == 0) {
      g95_error("Name '%s' at %C is the name of the procedure", sym->name);
      m = MATCH_ERROR;
      goto cleanup;
    }

    if (g95_match(" )") == MATCH_YES) goto ok;

    m = g95_match(" ,");
    if (m != MATCH_YES) {      
      g95_error("Unexpected junk in formal argument list at %C");
      goto cleanup;
    }
  }

ok:
  if (progname->formal != NULL) {
    g95_error("Symbol '%s' at %C already has an explicit interface",
	      progname->name);
    m = MATCH_ERROR;
    goto cleanup;
  }

  progname->formal = head;
  return MATCH_YES;
  
cleanup:
  g95_free_formal_arglist(head);
  return m;
}


/* match_result()-- Match a RESULT specification following a function
 * declaration or ENTRY statement.  Also matches the end-of-statement. */

static match match_result(g95_symbol *function, g95_symbol **result) {
char name[G95_MAX_SYMBOL_LEN+1];
g95_symbol *r;
match m;

  if (g95_match(" result (") != MATCH_YES) return MATCH_NO;

  m = g95_match_name(name);
  if (m != MATCH_YES) return m;

  if (g95_match(" )%t") != MATCH_YES) {
    g95_error("Unexpected junk following function argument list at %C");
    return MATCH_ERROR;
  }

  if (strcmp(function->name, name) == 0) {
    g95_error("RESULT variable at %C must be different than function name");
    return MATCH_ERROR;
  }

  if (g95_get_symbol(name, NULL, 0, &r)) return MATCH_ERROR;

  if (g95_add_flavor(&r->attr, FL_VARIABLE, NULL) == FAILURE ||
      g95_add_result(&r->attr, NULL) == FAILURE) return MATCH_ERROR;

  *result = r;

  return MATCH_YES;
}


/* g95_match_function_decl()-- Match a function declaration */

match g95_match_function_decl(void) {
char name[G95_MAX_SYMBOL_LEN+1];
g95_formal_arglist *arglist;
g95_symbol *sym, *result;
locus old_loc;
match m;

  g95_clear_attr(&current_attr);

  g95_clear_ts(&current_ts);

  arglist = NULL;
  old_loc = *g95_current_locus();

  m = match_prefix(&current_attr, &current_ts);
  if (m != MATCH_YES) {
    g95_set_locus(&old_loc);
    return m;
  }

  if (g95_match("function% %n", name) != MATCH_YES) {
    g95_set_locus(&old_loc);
    return MATCH_NO;
  }

  if (get_proc_name(name, &sym)) return MATCH_ERROR;
  g95_new_block = sym;

  m = g95_match_formal_arglist(sym, 0);
  if (m == MATCH_NO)
    g95_error("Expected formal argument list in function definition at %C");
  if (m != MATCH_YES) goto cleanup;

  result = NULL;

  if (g95_match_eos() != MATCH_YES) { /* See if a result variable is present */
    m = match_result(sym, &result);
    if (m == MATCH_NO)
      g95_error("Unexpected junk after function declaration at %C");

    if (m != MATCH_YES) {
      m = MATCH_ERROR;
      goto cleanup;
    }
  }

/* Make changes to the symbol */

  m = MATCH_ERROR;

  if (current_attr.recursive && result == NULL) {
    g95_error("RECURSIVE function at %C requires a RESULT specification");
    goto cleanup;
  }

  if (g95_current_state() == COMP_INTERFACE || result != NULL) {
    if (g95_add_function(&sym->attr, NULL) == FAILURE) goto cleanup;
  } else {
    if (g95_add_flavor(&sym->attr, FL_VARIABLE, NULL) == FAILURE) goto cleanup;
  }

  if (g95_missing_attr(&sym->attr, NULL) == FAILURE ||
      g95_copy_attr(&sym->attr, &current_attr, NULL) == FAILURE) goto cleanup;

  sym->result = result;

  if (result == NULL)
    sym->ts = current_ts;
  else
    result->ts = current_ts;

  if (g95_parent_procedure(sym, 0) == FAILURE) goto cleanup;

  return MATCH_YES;

cleanup:
  g95_reject_statement();
  g95_set_locus(&old_loc);
  return m;
}


/* g95_match_entry()-- Match an ENTRY statement */

match g95_match_entry(void) {
g95_symbol *function, *result, *entry;
g95_compile_state state;
match m;

  m = g95_match_symbol(&entry);
  if (m != MATCH_YES) return m;

  m = g95_match_formal_arglist(entry, 0);
  if (m != MATCH_YES) return MATCH_ERROR;

  g95_enclosing_unit(&state);
  switch(state) {
  case COMP_SUBROUTINE:
    if (g95_current_state() != COMP_SUBROUTINE) goto exec_construct;

    if (g95_add_entry(&entry->attr, NULL) == FAILURE ||
	g95_add_subroutine(&entry->attr, NULL) == FAILURE ||
	g95_parent_procedure(entry, 1) == FAILURE)
      return MATCH_ERROR;

    break;

  case COMP_FUNCTION:
    if (g95_current_state() != COMP_FUNCTION) goto exec_construct;
    function = g95_state_stack->sym;

    result = NULL;

    if (g95_match_eos() == MATCH_YES) {
      if (g95_add_entry(&entry->attr, NULL) == FAILURE ||
	  g95_add_function(&entry->attr, NULL) == FAILURE)
	return MATCH_ERROR;

    } else {
      m = match_result(function, &result);
      if (m == MATCH_NO) g95_syntax_error(ST_ENTRY);
      if (m != MATCH_YES) return MATCH_ERROR;

      if (g95_add_result(&result->attr, NULL) == FAILURE ||
	  g95_add_entry(&entry->attr, NULL) == FAILURE ||
	  g95_add_function(&entry->attr, NULL) == FAILURE)
	return MATCH_ERROR;
    }

    if (function->attr.recursive && result == NULL) {
      g95_error("RESULT attribute required in ENTRY statement at %C");
      return MATCH_ERROR;
    }

    if (g95_parent_procedure(entry, 0) == FAILURE) return MATCH_ERROR;

    break;

  default:
    goto exec_construct;
  }

  if (g95_match_eos() != MATCH_YES) {
    g95_syntax_error(ST_ENTRY);
    return MATCH_ERROR;
  }

  return MATCH_YES;


exec_construct:
  g95_error("ENTRY statement at %C cannot appear within %s",
	    g95_state_name(g95_current_state()));

  return MATCH_ERROR;
}


/* g95_match_subroutine()-- Match a subroutine statement, including
 * optional prefixes. */

match g95_match_subroutine(void) {
char name[G95_MAX_SYMBOL_LEN+1];
symbol_attribute attr;
g95_symbol *sym;
match m;

  g95_clear_attr(&attr);

  m = match_prefix(&attr, NULL);
  if (m != MATCH_YES) return m;

  m = g95_match("subroutine% %n", name);
  if (m != MATCH_YES) return m;

  if (get_proc_name(name, &sym)) return MATCH_ERROR;
  g95_new_block = sym;

  if (g95_add_subroutine(&sym->attr, NULL) == FAILURE) return MATCH_ERROR;

  if (g95_match_formal_arglist(sym, 0) != MATCH_YES) return MATCH_ERROR;

  if (g95_match_eos() != MATCH_YES) {
    g95_syntax_error(ST_SUBROUTINE);
    return MATCH_ERROR;
  }

  if (g95_parent_procedure(sym, 1) == FAILURE) return MATCH_ERROR;

  return MATCH_YES;
}


/* g95_match_end()-- Match any of the various end-block statements.
 * Returns the type of END to the caller.  The END INTERFACE, END IF,
 * END DO and END SELECT statements cannot be replaced by a single END
 * statement. */

match g95_match_end(g95_statement *st) {
char *target, name[G95_MAX_SYMBOL_LEN+1];
g95_compile_state state;
char *block_name;
locus old_loc;
match m;

  old_loc = *g95_current_locus();
  if (g95_match("end") != MATCH_YES) return MATCH_NO;

  state = g95_current_state();
  block_name = g95_current_block() == NULL ? NULL : g95_current_block()->name;

  if (state == COMP_CONTAINS) {
    state = g95_state_stack->previous->state;
    block_name = g95_state_stack->previous->sym == NULL ? NULL
      : g95_state_stack->previous->sym->name;
  }

  switch(state) {
  case COMP_NONE:
  case COMP_PROGRAM:
    *st = ST_END_PROGRAM;
    target = " program";
    break;

  case COMP_SUBROUTINE:
    *st = ST_END_SUBROUTINE;
    target = " subroutine";
    break;

  case COMP_FUNCTION:
    *st = ST_END_FUNCTION;
    target = " function";
    break;

  case COMP_BLOCK_DATA:
    *st = ST_END_BLOCK_DATA;
    target = " block data";
    break;

  case COMP_MODULE:
    *st = ST_END_MODULE;
    target = " module";
    break;

  case COMP_INTERFACE:
    *st = ST_END_INTERFACE;
    target = " interface";
    break;

  case COMP_DERIVED:
    *st = ST_END_TYPE;
    target = " type";
    break;

  case COMP_IF:
    *st = ST_ENDIF;
    target = " if";
    break;

  case COMP_DO:
    *st = ST_ENDDO;
    target = " do";
    break;

  case COMP_SELECT:
    *st = ST_END_SELECT;
    target = " select";
    break;

  case COMP_FORALL:
    *st = ST_END_FORALL;
    target = " forall";
    break;

  case COMP_WHERE:
    *st = ST_END_WHERE;
    target = " where";
    break;

  default:
    g95_error("Unexpected END statement at %C");
    goto cleanup;
  }

  if (g95_match_eos() == MATCH_YES) {

    if (*st == ST_ENDIF || *st == ST_ENDDO || *st == ST_END_SELECT ||
	*st == ST_END_INTERFACE || *st == ST_END_FORALL ||
	*st == ST_END_WHERE) {

      g95_error("%s statement expected at %C", g95_ascii_statement(*st));
      goto cleanup;
    }

    return MATCH_YES;
  }

/* Verify that we've got the sort of end-block that we're expecting */

  if (g95_match(target) != MATCH_YES) {
    g95_error("Expecting %s statement at %C", g95_ascii_statement(*st));
    goto cleanup;
  }

/* If we're at the end, make sure a block name wasn't required */

  if (g95_match_eos() == MATCH_YES) {

    if (*st != ST_ENDDO && *st != ST_ENDIF && *st != ST_END_SELECT)
      return MATCH_YES;

    if (g95_current_block() == NULL) return MATCH_YES;

    g95_error("Expected block name of '%s' in %s statement at %C",
	      block_name, g95_ascii_statement(*st));

    return MATCH_ERROR;
  }

/* END INTERFACE has a special handler for its several possible endings */

  if (*st == ST_END_INTERFACE) return g95_match_end_interface();

/* We haven't hit the end of statement, so what is left must be an end-name */

  m = g95_match_space();
  if (m == MATCH_YES) m = g95_match_name(name);

  if (m == MATCH_NO) g95_error("Expected terminating name at %C");
  if (m != MATCH_YES) goto cleanup;

  if (block_name == NULL) goto syntax;

  if (strcmp(name, block_name) != 0) {
    g95_error("Expected label '%s' for %s statement at %C", block_name,
	      g95_ascii_statement(*st));
    goto cleanup;
  }

  if (g95_match_eos() == MATCH_YES) return MATCH_YES;

syntax:
  g95_syntax_error(*st);

cleanup:
  g95_set_locus(&old_loc);
  return MATCH_ERROR;
}



/***************** Attribute declaration statements ****************/

/* attr_decl1()-- Function that sets the attribute of a single variable */

static match attr_decl1(void) {
char name[G95_MAX_SYMBOL_LEN+1];
g95_array_spec *as;
g95_symbol *sym;
locus var_locus;
match m;

  as = NULL;

  m = g95_match_name(name);
  if (m != MATCH_YES) goto cleanup;

  if (find_special(name, &sym)) return MATCH_ERROR;

  var_locus = *g95_current_locus();

/* Deal with possible array specification for certain attributes */

  if (current_attr.dimension || current_attr.allocatable ||
      current_attr.pointer   || current_attr.target) {
    m = g95_match_array_spec(&as);
    if (m == MATCH_ERROR) goto cleanup;

    if (current_attr.dimension && m == MATCH_NO) {
	g95_error("Missing array specification at %L in DIMENSION statement",
		  &var_locus);
	m = MATCH_ERROR;
	goto cleanup;
    }

    if ((current_attr.allocatable || current_attr.pointer) &&
	  (m == MATCH_YES) && (as->type != AS_DEFERRED)) {
      g95_error("Array specification must be deferred at %L",
		&var_locus);
      m = MATCH_ERROR;
      goto cleanup;
    }
  }

/* Update symbol table.  DIMENSION attribute is set in g95_set_array_spec(). */

  if (current_attr.dimension == 0 &&
      g95_copy_attr(&sym->attr, &current_attr, NULL) == FAILURE) {
    m = MATCH_ERROR;
    goto cleanup;
  }

  if (g95_set_array_spec(sym, as, &var_locus) == FAILURE) {
    m = MATCH_ERROR;
    goto cleanup;
  }

  return MATCH_YES;

cleanup:
  g95_free_array_spec(as);
  return m;
}


/* attr_decl()-- Generic attribute declaration subroutine.  Used for
 * attributes that just have a list of names. */

static match attr_decl(void) {
match m;

  g95_match(" ::");   /* Gobble the optional double colon */  

  for(;;) {
    m = attr_decl1();
    if (m != MATCH_YES) break;

    if (g95_match_eos() == MATCH_YES) {
      m = MATCH_YES;
      break;
    }

    if (g95_match(" ,") != MATCH_YES) {
      g95_error("Unexpected character in variable list at %C");
      m = MATCH_ERROR;
      break;
    }
  }

  return m;
}


match g95_match_external(void) {

  g95_clear_attr(&current_attr);
  g95_add_external(&current_attr, NULL);

  return attr_decl();
}



match g95_match_intent(void) {
sym_intent intent;
match m;

  intent = match_intent_spec();
  if (intent == INTENT_UNKNOWN) return MATCH_ERROR;

  g95_clear_attr(&current_attr);
  g95_add_intent(&current_attr, intent, NULL);   /* Can't fail */
  m = MATCH_YES;

  return attr_decl();
}


match g95_match_intrinsic(void) {

  g95_clear_attr(&current_attr);
  g95_add_intrinsic(&current_attr, NULL);

  return attr_decl();
}


match g95_match_optional(void) {

  g95_clear_attr(&current_attr);
  g95_add_optional(&current_attr, NULL);

  return attr_decl();
}


match g95_match_pointer(void) {

  g95_clear_attr(&current_attr);
  g95_add_pointer(&current_attr, NULL);

  return attr_decl();
}


match g95_match_allocatable(void) {

  g95_clear_attr(&current_attr);
  g95_add_allocatable(&current_attr, NULL);

  return attr_decl();
}


match g95_match_dimension(void) {

  g95_clear_attr(&current_attr);
  g95_add_dimension(&current_attr, NULL);

  return attr_decl();
}


match g95_match_target(void) {

  g95_clear_attr(&current_attr);
  g95_add_target(&current_attr, NULL);

  return attr_decl();
}


/* access_attr_decl()-- match the list of entities being specified in
 * a PUBLIC or PRIVATE statement. */

static match access_attr_decl(g95_statement st) {
char name[G95_MAX_SYMBOL_LEN+1];
interface_type type;
g95_symbol *sym;
int operator;
match m;

  g95_match(" ::");

  for(;;) {
    m = g95_match_generic_spec(&type, name, &operator);
    if (m == MATCH_NO) goto syntax;
    if (m == MATCH_ERROR) return MATCH_ERROR;

    switch(type) {
    case INTERFACE_NAMELESS:
      goto syntax;

    case INTERFACE_GENERIC:
      if (g95_get_symbol(name, NULL, 0, &sym)) goto done;

      if (g95_add_access(&sym->attr,
			 (st == ST_PUBLIC) ? ACCESS_PUBLIC : ACCESS_PRIVATE,
			 NULL) == FAILURE) return MATCH_ERROR;

      break;

    case INTERFACE_INTRINSIC_OP:
      if (g95_current_ns->operator_access[operator] == ACCESS_UNKNOWN) {
	g95_current_ns->operator_access[operator] =
	  (st == ST_PUBLIC) ? ACCESS_PUBLIC : ACCESS_PRIVATE;
      } else {
	g95_error("Access specification of the %s operator at %C has "
		  "already been specified", g95_op2string(operator));
	goto done;
      }

      break;

    case INTERFACE_USER_OP:
      if (g95_get_symbol(name, NULL, 0, &sym)) goto done;

      if (sym->operator_access == ACCESS_UNKNOWN) {
	sym->operator_access =
	  (st == ST_PUBLIC) ? ACCESS_PUBLIC : ACCESS_PRIVATE;
      } else {
	g95_error("Access specification of the .%s. operator at %C has "
		  "already been specified", sym->name);
	goto done;
      }

      break;
    }

    if (g95_match(" ,") == MATCH_NO) break;
  }

  if (g95_match_eos() != MATCH_YES) goto syntax;
  return MATCH_YES;

syntax:
  g95_syntax_error(st);

done:
  return MATCH_ERROR;
}


/* g95_match_private()-- The PRIVATE statement is a bit weird in that
 * it can be a attribute declaration, but also works as a standlone
 * statement inside of a type declaration or a module. */

match g95_match_private(g95_statement *st) {

  if (g95_match("private") != MATCH_YES) return MATCH_NO;

  if (g95_current_state() == COMP_DERIVED) {
    if (g95_match_eos() == MATCH_YES) {
      *st = ST_PRIVATE;
      return MATCH_YES;
    }

    g95_syntax_error(ST_PRIVATE);
    return MATCH_ERROR;
  }

  if (g95_match_eos() == MATCH_YES) {
    *st = ST_PRIVATE;
    return MATCH_YES;
  }

  *st = ST_ATTR_DECL;
  return access_attr_decl(ST_PRIVATE);
}


match g95_match_public(g95_statement *st) {

  if (g95_match("public") != MATCH_YES) return MATCH_NO;

  if (g95_match_eos() == MATCH_YES) {
    *st = ST_PUBLIC;
    return MATCH_YES;
  }

  *st = ST_ATTR_DECL;
  return access_attr_decl(ST_PUBLIC);
}


/* do_parm()-- Workhorse for g95_match_parameter */

static match do_parm(void) {
g95_symbol *sym;
locus var_locus;
g95_expr *init;
match m;

  m = g95_match_symbol(&sym);
  if (m == MATCH_NO)
    g95_error("Expected variable name at %C in PARAMETER statement");

  if (m != MATCH_YES) return m;

  var_locus = *g95_current_locus();

  if (g95_match(" =") == MATCH_NO) {
    g95_error("Expected = sign in PARAMETER statement at %C");
    return MATCH_ERROR;
  }

  m = g95_match_init_expr(&init);
  if (m == MATCH_NO)
    g95_error("Expected expression at %C in PARAMETER statement");
  if (m != MATCH_YES) return m;

  if (g95_check_assign_symbol(sym, init) == FAILURE ||
      g95_add_flavor(&sym->attr, FL_PARAMETER, NULL) == FAILURE) {
    m = MATCH_ERROR;
    goto cleanup;
  }

  sym->value = init;
  return MATCH_YES;

cleanup:
  g95_free_expr(init);
  return m;
}

/* g95_match_parameter()-- Match a parameter statement, with the weird
 * syntax that these have */

match g95_match_parameter(void) {
match m;

  if (g95_match(" (") == MATCH_NO) return MATCH_NO;

  for(;;) {
    m = do_parm();
    if (m != MATCH_YES) break;

    if (g95_match(" )%t") == MATCH_YES) break;

    if (g95_match(" ,") != MATCH_YES) {
      g95_error("Unexpected characters in PARAMETER statement at %C");
      m = MATCH_ERROR;
      break;
    }
  }

  return m;
}


/* g95_match_save()-- Save statements have a special syntax */

match g95_match_save(void) {
g95_symbol *sym;
match m;

  if (g95_match_eos() == MATCH_YES) {
    if (g95_current_ns->seen_save) {
      g95_error("Blanket SAVE statement at %C follows previous "
		"SAVE statement");

      return MATCH_ERROR;
    }

    g95_current_ns->save_all = g95_current_ns->seen_save = 1;
    return MATCH_YES;
  }

  if (g95_current_ns->save_all) {
    g95_error("SAVE statement at %C follows blanket SAVE statement");
    return MATCH_ERROR;
  }

  for(;;) { 
    m = g95_match(" %s", &sym);
    switch(m) {
    case MATCH_YES:
      if (g95_add_save(&sym->attr, NULL) == FAILURE) return MATCH_ERROR;
      goto next_item;

    case MATCH_NO:
      break;

    case MATCH_ERROR:
      return MATCH_ERROR;
    }

    m = g95_match(" / %s /", &sym);
    if (m == MATCH_ERROR) return MATCH_ERROR;
    if (m == MATCH_NO) goto syntax;

    if (g95_add_saved_common(&sym->attr, NULL) == FAILURE) return MATCH_ERROR;
    g95_current_ns->seen_save = 1;

  next_item:
    if (g95_match_eos() == MATCH_YES) break;
    if (g95_match(" ,") != MATCH_YES) goto syntax;
  }

  return MATCH_YES;

syntax:
  g95_error("Syntax error in SAVE statement at %C");
  return MATCH_ERROR;
}


/* g95_match_modproc()-- Match a module procedure statement.  Note
 * that we have to modify symbols in the parent's namespace because
 * the current one was there to receive symbols that are in a
 * interface's formal argument list. */

match g95_match_modproc(void) {
char name[G95_MAX_SYMBOL_LEN+1];
g95_symbol *sym;
match m;

  if (g95_state_stack->state != COMP_INTERFACE ||
      g95_state_stack->previous == NULL ||
      g95_state_stack->previous->state != COMP_MODULE ||
      current_interface.type == INTERFACE_NAMELESS) {
    g95_error("MODULE PROCEDURE at %C must be in a generic module interface");
    return MATCH_ERROR;
  }

  for(;;) {
    m = g95_match_name(name);
    if (m == MATCH_NO) goto syntax;
    if (m != MATCH_YES) return MATCH_ERROR;

    if (g95_get_symbol(name, g95_current_ns->parent, 0, &sym))
      return MATCH_ERROR;

    if (sym->attr.flavor != FL_MODULE_PROC &&
	g95_add_flavor(&sym->attr, FL_MODULE_PROC, NULL) == FAILURE)
      return MATCH_ERROR;

    g95_add_interface(sym);

    if (g95_match_eos() == MATCH_YES) break;
    if (g95_match(" ,") != MATCH_YES) goto syntax;
  }

  return MATCH_YES;

syntax:
  g95_syntax_error(ST_MODULE_PROC);
  return MATCH_ERROR;
}


/* g95_match_derived_decl()-- Match the beginning of a derived type
 * declaration.  If a type name was the result of a function, then it is
 * possible to have a symbol already to be known as a derived type yet
 * have no components. */

match g95_match_derived_decl(void) {
char name[G95_MAX_SYMBOL_LEN+1];
symbol_attribute attr;
g95_symbol *sym;
match m;

  if (g95_current_state() == COMP_DERIVED) return MATCH_NO;

  g95_clear_attr(&attr);

loop:
  if (g95_match(" , private") == MATCH_YES) {
    if (g95_find_state(COMP_MODULE) == FAILURE) {
      g95_error("Derived type at %C can only be PRIVATE within a MODULE");
      return MATCH_ERROR;
    }

    if (g95_add_access(&attr, ACCESS_PRIVATE, NULL) == FAILURE)
      return MATCH_ERROR;
    goto loop;
  }

  if (g95_match(" , public") == MATCH_YES) {
    if (g95_find_state(COMP_MODULE) == FAILURE) {
      g95_error("Derived type at %C can only be PUBLIC within a MODULE");
      return MATCH_ERROR;
    }

    if (g95_add_access(&attr, ACCESS_PUBLIC, NULL) == FAILURE)
      return MATCH_ERROR;
    goto loop;
  }

  if (g95_match(" ::") != MATCH_YES && attr.access != ACCESS_UNKNOWN) {
    g95_error("Expected :: in TYPE definition at %C");
    return MATCH_ERROR;
  }

  m = g95_match(" %n%t", &name);
  if (m != MATCH_YES) return m;

/* Make sure the name isn't the name of an intrinsic type.  The
 * 'double precision' type doesn't get past the name matcher */

  if (strcmp(name, "integer") == 0   || strcmp(name, "real") == 0 ||
      strcmp(name, "character") == 0 || strcmp(name, "logical") == 0 ||
      strcmp(name, "complex") == 0) {
    g95_error("Type name '%s' at %C cannot be the same as an intrinsic type",
	      name);
    return MATCH_ERROR;
  }

  if (g95_get_symbol(name, NULL, 1, &sym)) return MATCH_ERROR;

/* The symbol may already have the derived attribute without the
 * components.  The only way this can happen is during a function
 * definition or INTRINSIC statement.  The first part of the AND
 * clause is true if a the symbol is not the return value of a
 * function. */

  if (sym->attr.flavor != FL_DERIVED &&
      g95_add_flavor(&sym->attr, FL_DERIVED, NULL) == FAILURE)
    return MATCH_ERROR;

  if (sym->components != NULL) {
    g95_error("Derived type definition of '%s' at %C has already been defined",
	      sym->name);
    return MATCH_ERROR;
  }

  if (attr.access != ACCESS_UNKNOWN &&
      g95_add_access(&sym->attr, attr.access, NULL) == FAILURE)
    return MATCH_ERROR;

  g95_new_block = sym;

  return MATCH_YES;
}

