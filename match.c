/* Matching subroutines
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

/* match.c-- matchers in all sizes, shapes and colors. */

#include <stdarg.h>
#include <ctype.h>
#include <string.h>

#include "g95.h"


/******************** Generic matching subroutines ************************/

/* g95_match_space()-- In free form, match at least one space.  Always
 * matches in fixed form. */

match g95_match_space(void) {
locus old_loc;

  if (g95_current_file->form == FORM_FIXED) return MATCH_YES;

  old_loc = *g95_current_locus();

  if (!g95_is_whitespace(g95_next_char())) {
    g95_set_locus(&old_loc);
    return MATCH_NO;
  }

  g95_gobble_whitespace();

  return MATCH_YES;
}


/* g95_match_eos()-- Match an end of statement.  End of statement is
 * optional whitespace, followed by a ';' or '\n' or comment '!'.  If
 * a semicolon is found, we continue to eat whitespace and semicolons. */

match g95_match_eos(void) {
locus old_loc;
int flag, c;

  flag = 0;

  for(;;) { 
    old_loc = *g95_current_locus();
    g95_gobble_whitespace();
   
    c = g95_next_char();
    switch(c) {
    case '!':
      do {
	c = g95_next_char();
      } while(c != '\n');

      /* Fall through */

    case '\n':
      return MATCH_YES;

    case ';':
      flag = 1;
      continue;
    }

    break;
  }

  g95_set_locus(&old_loc);
  return (flag) ? MATCH_YES : MATCH_NO;
}


/* g95_match_small_literal_int()-- Match a literal integer on the
 * input, setting the value on MATCH_YES.  Literal ints occur in
 * kind-parameters as well as old-style character length
 * specifications. */

match g95_match_small_literal_int(int *value) {
locus old_loc;
char c;
int i;

  old_loc = *g95_current_locus();

  g95_gobble_whitespace();
  c = g95_next_char();

  if (!isdigit(c)) {
    g95_set_locus(&old_loc);
    return MATCH_NO;
  }

  i = c - '0';

  for(;;) {
    old_loc = *g95_current_locus();
    c = g95_next_char();

    if (!isdigit(c)) break;

    i = 10*i + c - '0';

    if (i > 99999999) {
      g95_error("Integer too large at %C");
      return MATCH_ERROR;
    }
  }

  g95_set_locus(&old_loc);

  *value = i;
  return MATCH_YES;
}


/* g95_match_small_int()-- Match a small, constant integer expression,
 * like in a kind statement.  On MATCH_YES, 'value' is set. */

match g95_match_small_int(int *value) {
g95_expr *expr;
const char *p;
match m;
int i;

  m = g95_match_expr(&expr);
  if (m != MATCH_YES) return m;

  p = g95_extract_int(expr, &i);
  g95_free_expr(expr);

  if (p != NULL) {
    g95_error(p);
    m = MATCH_ERROR;
  }

  *value = i;
  return m;
}


/* g95_match_st_label()-- Matches a statement label.  Uses
 * g95_match_small_literal_int() to do most of the work. */

match g95_match_st_label(int *label) {
locus old_loc;
match m;
int i;

  old_loc = *g95_current_locus();

  m = g95_match_small_literal_int(&i);
  if (m != MATCH_YES) return m;

  if (i != 0 && i <= 99999) {
    *label = i;
    return MATCH_YES;
  }

  g95_error("Statement label at %C is out of range");

  g95_set_locus(&old_loc);

  return MATCH_ERROR;
}


/* g95_match_label()-- Match and validate a label associated with a
 * named IF, DO or SELECT statement.  If the symbol does not have the
 * label attribute, we add it.  We also make sure the symbol does not
 * refer to another (active) block.  A matched label is pointed to by
 * g95_new_block. */

match g95_match_label(void) {
char name[G95_MAX_SYMBOL_LEN+1];
g95_state_data *p;
match m;

  g95_new_block = NULL;

  m = g95_match(" %n :", name);
  if (m != MATCH_YES) return m;

  if (g95_findget_symbol(name, NULL, 0, &g95_new_block)) return MATCH_ERROR;

  if (g95_new_block->attr.flavor != FL_LABEL &&
      g95_add_flavor(&g95_new_block->attr, FL_LABEL, NULL) == FAILURE)
    return MATCH_ERROR;

  for(p=g95_state_stack; p; p=p->previous)
    if (p->sym == g95_new_block) {
      g95_error("Label %s at %C already in use by a parent block",
		g95_new_block->name);
      return MATCH_ERROR;
    }

  return MATCH_YES;
}


/* g95_match_strings()-- Try and match the input against an array of
 * possibilities.  If one potential matching string is a substring of
 * another, the longest match takes precedence.  Spaces in the target
 * strings are optional spaces that do not necessarily have to be
 * found in the input stream.  In fixed mode, spaces never appear.  If
 * whitespace is matched, it matches unlimited whitespace in the
 * input.  For this reason, the 'mp' member of the mstring structure
 * is used to track the progress of each potential match.
 *
 * If there is no match we return the tag associated with the
 * terminating NULL mstring structure and leave the locus pointer
 * where it started.  If there is a match we return the tag member of
 * the matched mstring and leave the locus pointer after the matched
 * character.
 *
 * A '%' character is a mandatory space.
 */

int g95_match_strings(mstring *a) {
mstring *p, *best_match;
int no_match, c, possibles;
locus match_loc;

  possibles = 0;

  for(p=a; p->string != NULL; p++) {
    p->mp = p->string;
    possibles++;
  }

  no_match = p->tag;

  best_match = NULL;
  match_loc = *g95_current_locus();

  g95_gobble_whitespace();

  while(possibles > 0) {
    c = g95_next_char();

/* Apply the next character to the current possibilities */

    for(p=a; p->string!=NULL; p++) {
      if (p->mp == NULL) continue;

      if (*p->mp == ' ') {    /* Space matches 1+ whitespace(s) */

	if ((g95_current_file->form == FORM_FREE) && 
	      g95_is_whitespace(c)) continue;

	p->mp++;
      }

      if (*p->mp != c) {      /* Match failed */
	p->mp = NULL;
	possibles--;
	continue;
      }

      p->mp++;
      if (*p->mp == '\0') {   /* Found a match */
	match_loc = *g95_current_locus();
	best_match = p;
	possibles--;
	p->mp = NULL;
      }
    }
  }

  g95_set_locus(&match_loc);

  return (best_match == NULL) ? no_match : best_match->tag;
}


/* g95_match_name()-- See if the current input looks like a name of
 * some sort.  Modifies the passed buffer which must be
 * G95_MAX_SYMBOL_LEN+1 bytes long. */

match g95_match_name(char *buffer) {
locus old_loc;
int i, c;

  old_loc = *g95_current_locus();
  g95_gobble_whitespace();

  c = g95_next_char();
  if (!isalpha(c)) {
    g95_set_locus(&old_loc);
    return MATCH_NO;
  }

  i = 0;

  do {
    buffer[i++] = c;

    if (i > G95_MAX_SYMBOL_LEN) {
      g95_error("Name at %C is too long");
      return MATCH_ERROR;
    }

    old_loc = *g95_current_locus();
    c = g95_next_char();
  } while(isalnum(c) || c == '_' || (g95_option.dollar && c == '$'));

  buffer[i] = '\0';
  g95_set_locus(&old_loc);

  return MATCH_YES;
}


/* g95_match_symbol()-- Match a symbol on the input.  Modifies the
 * pointer to the symbol pointer if successful. */

match g95_match_symbol(g95_symbol **matched_symbol) {
char buffer[G95_MAX_SYMBOL_LEN+1];
match m;

  m = g95_match_name(buffer);
  if (m == MATCH_YES && g95_get_symbol(buffer, NULL, 1, matched_symbol))
    m = MATCH_ERROR;

  return m;
}


/* g95_match_intrinsic_op()-- Match an intrinsic operator.  Returns an
 * INTRINSIC enum. */

match g95_match_intrinsic_op(g95_intrinsic_op *result) {
static mstring operators_in[] = {
  minit("+", INTRINSIC_PLUS),      minit("-", INTRINSIC_MINUS),
  minit("**", INTRINSIC_POWER),    minit("//", INTRINSIC_CONCAT),
  minit("*", INTRINSIC_TIMES),     minit("/", INTRINSIC_DIVIDE),
  minit(".and.", INTRINSIC_AND),   minit(".or.", INTRINSIC_OR),
  minit(".eqv.", INTRINSIC_EQV),   minit(".neqv.", INTRINSIC_NEQV),
  minit(".eq.", INTRINSIC_EQ),     minit("==", INTRINSIC_EQ),
  minit(".ne.", INTRINSIC_NE),     minit("/=", INTRINSIC_NE),
  minit(".ge.", INTRINSIC_GE),     minit(">=", INTRINSIC_GE),
  minit(".le.", INTRINSIC_LE),     minit("<=", INTRINSIC_LE),
  minit(".lt.", INTRINSIC_LT),     minit("<", INTRINSIC_LT),
  minit(".gt.", INTRINSIC_GT),     minit(">", INTRINSIC_GT),
  minit(".not.", INTRINSIC_NOT),   minit(NULL, INTRINSIC_NONE) };

g95_intrinsic_op op;

  op = g95_match_strings(operators_in);

  if (op == INTRINSIC_NONE) return MATCH_NO;

  *result = op;
  return MATCH_YES;
}



const char *g95_op2string(int i) {
static mstring operators_out[] = {
  minit("+", INTRINSIC_PLUS),      minit("-", INTRINSIC_MINUS),
  minit("+", INTRINSIC_UPLUS),     minit("-", INTRINSIC_UMINUS),
  minit("**", INTRINSIC_POWER),    minit("//", INTRINSIC_CONCAT),
  minit("*", INTRINSIC_TIMES),     minit("/", INTRINSIC_DIVIDE),
  minit(".and.", INTRINSIC_AND),   minit(".or.", INTRINSIC_OR),
  minit(".eqv.", INTRINSIC_EQV),   minit(".neqv.", INTRINSIC_NEQV),
  minit(".eq.", INTRINSIC_EQ),     minit("==", INTRINSIC_EQ),
  minit(".ne.", INTRINSIC_NE),     minit("/=", INTRINSIC_NE),
  minit(".ge.", INTRINSIC_GE),     minit(">=", INTRINSIC_GE),
  minit(".le.", INTRINSIC_LE),     minit("<=", INTRINSIC_LE),
  minit(".lt.", INTRINSIC_LT),     minit("<", INTRINSIC_LT),
  minit(".gt.", INTRINSIC_GT),     minit(">", INTRINSIC_GT),
  minit(".not.", INTRINSIC_NOT),   minit(NULL, INTRINSIC_NONE) };

  if (i == INTRINSIC_ASSIGN) return "=";
  return g95_code2string(operators_out, i);
}


/* g95_match_iterator()-- Match a loop control phrase:
 *
 *     <LVALUE> = <EXPR>, <EXPR> [, <EXPR> ]
 *
 * If the final integer expression is not present, a constant unity
 * expression is returned.  We don't return MATCH_ERROR until after
 * the equals sign is seen. */

match g95_match_iterator(g95_iterator *iter) {
g95_expr *var, *e1, *e2, *e3;
match m;

  m = g95_match(" %e =", &var);
  if (m != MATCH_YES) return MATCH_NO;

  e1 = e2 = e3 = NULL;

  if (var->expr_type != EXPR_VARIABLE) {
    g95_error("Loop variable expected for DO loop at %C");
    goto cleanup;
  }

  if (var->ref != NULL) {
    g95_error("Loop variable at %C must be a scalar variable");
    goto cleanup;
  }

  if (var->symbol->attr.pointer) {
    g95_error("Loop variable at %C cannot have the POINTER attribute");
    goto cleanup;
  }

  m = g95_match_expr(&e1);
  if (m == MATCH_NO) goto syntax;
  if (m == MATCH_ERROR) goto cleanup;

  if (g95_match(" ,") != MATCH_YES) goto syntax;

  m = g95_match_expr(&e2);
  if (m == MATCH_NO) goto syntax;
  if (m == MATCH_ERROR) goto cleanup;

  if (g95_match(" ,") != MATCH_YES) {
    e3 = g95_int_expr(1);
    goto done;
  }

  m = g95_match(" %e", &e3);
  if (m == MATCH_ERROR) goto cleanup;
  if (m == MATCH_NO) {
    g95_error("Expected a step value in iterator at %C");
    goto cleanup;
  }

done:
  iter->var = var;
  iter->start = e1;
  iter->end = e2;
  iter->step = e3;
  return MATCH_YES;

syntax:
  g95_error("Syntax error in iterator at %C");
   
cleanup:
  g95_free_expr(e1);
  g95_free_expr(e2);
  g95_free_expr(e3);

  return MATCH_ERROR;
}


/* g95_match()-- General purpose matching subroutine.  The target
 * string is a scanf-like format string in which spaces correspond to
 * arbitrary whitespace (including no whitespace), characters
 * correspond to themselves.  The %-codes are:
 *
 * %%  Literal percent sign
 * %e  Expression, pointer to a pointer is set
 * %E  Like %e, but allows array values expressions
 * %s  Symbol, pointer to the symbol is set
 * %n  Name, character buffer is set to name
 * %t  Matches end of statement.
 * %o  Matches an intrinsic operator, returned as an INTRINSIC enum.
 * %l  Matches a statement label number
 * %v  Matches a variable expression (an lvalue)
 * %   Matches a required space (in free form) and optional spaces.
 */

match g95_match(const char *target, ...) {
int matches, *ip;
const char *p;
locus old_loc;
va_list argp;
char c, *np;
match m, n;
void **vp;

  old_loc = *g95_current_locus();
  va_start(argp, target);
  m = MATCH_NO;
  matches = 0;
  p = target;

loop:
  c = *p++;
  switch(c) {
  case ' ':   g95_gobble_whitespace(); goto loop;
  case '\0':  m = MATCH_YES; break;

  case '%':
    c = *p++;
    switch(c) {
    case 'e':
      vp = va_arg(argp, void **);
      n = g95_match_scalar_expr((g95_expr **) vp);
      if (n != MATCH_YES) { m = n; goto not_yes; }

      matches++;
      goto loop;

    case 'v':
      vp = va_arg(argp, void **);
      n = g95_match_variable((g95_expr **) vp, 0);
      if (n != MATCH_YES) { m = n; goto not_yes; }

      matches++;
      goto loop;

    case 'E':
      vp = va_arg(argp, void **);
      n = g95_match_expr((g95_expr **) vp);
      if (n != MATCH_YES) { m = n; goto not_yes; }

      matches++;
      goto loop;

    case 's':
      vp = va_arg(argp, void **);
      n = g95_match_symbol((g95_symbol **) vp);
      if (n != MATCH_YES) { m = n; goto not_yes; }

      matches++;
      goto loop;

    case 'n':
      np = va_arg(argp, char *);
      n = g95_match_name(np);
      if (n != MATCH_YES) { m = n; goto not_yes; }

      matches++;
      goto loop;

    case 'l':
      ip = va_arg(argp, int *);
      n = g95_match_st_label(ip);
      if (n != MATCH_YES) { m = n; goto not_yes; }

      matches++;
      goto loop;

    case 'o':
      ip = va_arg(argp, int *);
      n = g95_match_intrinsic_op((g95_intrinsic_op *) ip);
      if (n != MATCH_YES) { m = n; goto not_yes; }

      matches++;
      goto loop;

    case 't':
      if (g95_match_eos() != MATCH_YES) { m = MATCH_NO; goto not_yes; }
      goto loop;

    case ' ':
      if (g95_match_space() == MATCH_YES) goto loop;
      m = MATCH_NO;
      goto not_yes;

    case '%': break;  /* Fall through to character matcher */

    default:
      g95_internal_error("g95_match(): Bad match code %c", c);
    }

  default:
    if (c == g95_next_char()) goto loop;
    break;
  }

not_yes:
  va_end(argp);

  if (m != MATCH_YES) {   /* Clean up after a failed match */
    g95_set_locus(&old_loc);
    va_start(argp, target);

    p = target;
    for(; matches>0; matches--) {
      while(*p++ != '%');

      switch(*p++) {
      case '%': matches++; break;   /* Skip */

      case 'I': case 'L': case 'C':
	if (*p++ == 'e') goto undo_expr;
	break;

      case 'o': case 'l':	/* Matches that don't have to be undone */
      case 'n': case 's':
	vp = va_arg(argp, void **);
	break;

      case 'e': case 'E': case 'v':
      undo_expr:
	vp = va_arg(argp, void **);
	g95_free_expr(*vp);
	*vp = NULL;
	break;
      }
    }  

    va_end(argp);
  }

  return m;
}


/*********************** Statement level matching **********************/


/* g95_match_program()-- Matches the start of a program unit, which is
 * the program keyword followed by an optional symbol. */

match g95_match_program(void) {
g95_symbol *sym;
match m;

  m = g95_match_eos();
  if (m == MATCH_YES) return m;

  m = g95_match("% %s%t", &sym);

  if (m == MATCH_NO) {
    g95_error("Invalid form of PROGRAM statement at %C");
    m = MATCH_ERROR;
  }

  if (m == MATCH_ERROR) return m;

  if (g95_add_flavor(&sym->attr, FL_PROGRAM, NULL) == FAILURE)
    return MATCH_ERROR;

  g95_new_block = sym;

  return MATCH_YES;
}


/* g95_match_assignment(void)-- Match a simple assignment statement */

match g95_match_assignment(void) {
g95_expr *lvalue, *rvalue;
locus old_loc;
match m;

  old_loc = *g95_current_locus(); 

  lvalue = rvalue = NULL;
  m = g95_match(" %v =", &lvalue);
  if (m != MATCH_YES) goto cleanup;

  m = g95_match(" %E%t", &rvalue);
  if (m != MATCH_YES) goto cleanup;

  new_st.op = EXEC_ASSIGN;
  new_st.expr = lvalue;
  new_st.expr2 = rvalue;

  return MATCH_YES;

cleanup:
  g95_set_locus(&old_loc);
  g95_free_expr(lvalue);
  g95_free_expr(rvalue);
  return m;
}


/* g95_match_pointer_assignment()-- Match a pointer assignment statement */

match g95_match_pointer_assignment(void) {
g95_expr *lvalue, *rvalue;
locus old_loc;
match m;

  old_loc = *g95_current_locus();

  lvalue = rvalue = NULL;

  m = g95_match(" %v =>", &lvalue);
  if (m != MATCH_YES) { m = MATCH_NO; goto cleanup; }

  m = g95_match(" %E%t", &rvalue);
  if (m != MATCH_YES) goto cleanup;

  new_st.op = EXEC_POINTER_ASSIGN;
  new_st.expr = lvalue;
  new_st.expr2 = rvalue;

  return MATCH_YES;

cleanup:
  g95_set_locus(&old_loc);
  g95_free_expr(lvalue);
  g95_free_expr(rvalue);
  return m;
}


/* g95_match_if()-- The IF statement is a bit of a pain.  First of
 * all, there are three forms of it, the simple IF, the IF that starts
 * a block and the arithmetic IF.
 *
 * There is a problem with the simple IF and that is the fact that we
 * only have a single level of undo information on symbols.  What this
 * means is for a simple IF, we must re-match the whole IF statement
 * multiple times in order to guarantee that the symbol table ends up
 * in the proper state. */

match g95_match_if(g95_statement *if_type) {
g95_expr *expr;
int l1, l2, l3;
locus old_loc;
g95_code *p;
match m, n;

  n = g95_match_label();
  if (n == MATCH_ERROR) return n;

  old_loc = *g95_current_locus();

  m = g95_match(" if ( %e", &expr);
  if (m != MATCH_YES) return m;

  if (g95_match(" )") != MATCH_YES) {
    g95_error("Syntax error in IF-expression at %C");
    g95_free_expr(expr);
    return MATCH_ERROR;
  }

  m = g95_match(" %l , %l , %l%t", &l1, &l2, &l3);

  if (m == MATCH_YES) {
    if (n == MATCH_YES) {
      g95_error("Block label not appropriate for arithmetic IF statement "
		"at %C");

      g95_free_expr(expr);
      return MATCH_ERROR;
    }

    if (g95_reference_st_label(l1, ST_LABEL_TARGET) == FAILURE ||
	g95_reference_st_label(l2, ST_LABEL_TARGET) == FAILURE ||
	g95_reference_st_label(l3, ST_LABEL_TARGET) == FAILURE) {

      g95_free_expr(expr);
      return MATCH_ERROR;
    }

    new_st.op = EXEC_ARITHMETIC_IF;
    new_st.expr = expr;
    new_st.label = l1;
    new_st.label2 = l2;
    new_st.label3 = l3;

    *if_type = ST_ARITHMETIC_IF;
    return MATCH_YES;
  }

  if (g95_match(" then %t") == MATCH_YES) {
    new_st.op = EXEC_IF;
    new_st.expr = expr;

    *if_type = ST_IF_BLOCK;
    return MATCH_YES;
  }

  if (n == MATCH_YES) {
    g95_error("Block label is not appropriate IF statement at %C");

    g95_free_expr(expr);
    return MATCH_ERROR;
  }

/* At this point the only thing left is a simple IF statement.  At
 * this point, n has to be MATCH_NO, so we don't have to worry about
 * re-matching a block label.  From what we've got so far, try
 * matching an assignment. */

  *if_type = ST_SIMPLE_IF;

  m = g95_match_assignment();
  if (m == MATCH_YES) goto got_match;

  g95_free_expr(expr);
  g95_undo_symbols();
  g95_set_locus(&old_loc);

  g95_match(" if ( %e ) ", &expr);  /* Guaranteed to match */

  m = g95_match_pointer_assignment();
  if (m == MATCH_YES) goto got_match;

  g95_free_expr(expr);
  g95_undo_symbols();
  g95_set_locus(&old_loc);

  g95_match(" if ( %e ) ", &expr);  /* Guaranteed to match */

/* Look at the next keyword to see which matcher to call.  Matching
 * the keyword doesn't affect the symbol table, so we don't have to
 * restore between tries. */

#define match(string, subr, statement) \
  if (g95_match(string) == MATCH_YES) { m = subr(); goto got_match; }

  g95_clear_error();

  match("allocate",   g95_match_allocate,    ST_ALLOCATE)
  match("backspace",  g95_match_backspace,   ST_BACKSPACE)
  match("call",       g95_match_call,        ST_CALL)
  match("close",      g95_match_close,       ST_CLOSE)
  match("continue",   g95_match_continue,    ST_CONTINUE)
  match("cycle",      g95_match_cycle,       ST_CYCLE)
  match("deallocate", g95_match_deallocate,  ST_DEALLOCATE)
  match("end file",   g95_match_endfile,     ST_END_FILE)
  match("exit",       g95_match_exit,        ST_EXIT)
  match("assign",     g95_match_assign,      ST_NONE)
  match("go to",      g95_match_goto,        ST_GOTO)
  match("inquire",    g95_match_inquire,     ST_INQUIRE)
  match("nullify",    g95_match_nullify,     ST_NULLIFY)
  match("open",       g95_match_open,        ST_OPEN)
  match("pause",      g95_match_pause,       ST_NONE)
  match("print",      g95_match_print,       ST_WRITE)
  match("read",       g95_match_read,        ST_READ)
  match("return",     g95_match_return,      ST_RETURN)
  match("rewind",     g95_match_rewind,      ST_REWIND)  
  match("stop",       g95_match_stop,        ST_STOP)
  match("write",      g95_match_write,       ST_WRITE)

/* All else has failed, so give up.  See if any of the matchers has
 * stored an error message of some sort. */

  if (g95_error_check() == 0)
    g95_error("Unclassifiable statement in IF-clause at %C");

  return MATCH_ERROR;

got_match:
  if (m == MATCH_NO) g95_error("Syntax error in IF-clause at %C");
  if (m != MATCH_YES) return MATCH_ERROR;

/* At this point, we've matched the single IF and the action clause is
 * in new_st.  Rearrange things so that the IF statement appears in new_st */

  p = g95_get_code();
  p->next = g95_get_code();
  *p->next = new_st;

  p->expr = expr;
  p->op = EXEC_IF;

  g95_clear_new_st();

  new_st.op = EXEC_IF;
  new_st.block = p;

  return MATCH_YES;
}

#undef match


/* g95_match_else()-- Match an ELSE statement */

match g95_match_else(void) {
char name[G95_MAX_SYMBOL_LEN+1];
 
  if (g95_match_eos() == MATCH_YES) return MATCH_YES;

  if (g95_match_name(name) != MATCH_YES || g95_current_block() == NULL ||
      g95_match_eos() != MATCH_YES) {
    g95_error("Unexpected junk after ELSE statement at %C");
    return MATCH_ERROR;
  }

  if (strcmp(name, g95_current_block()->name) != 0) {
    g95_error("Label '%s' at %C doesn't match IF label '%s'",
	      name, g95_current_block()->name);
    return MATCH_ERROR;
  }

  return MATCH_YES;
}


/* g95_match_elseif()-- Match an ELSE IF statement */

match g95_match_elseif(void) {
char name[G95_MAX_SYMBOL_LEN+1];
g95_expr *expr;
match m;

  m = g95_match(" ( %e ) then", &expr);
  if (m != MATCH_YES) return m;

  if (g95_match_eos() == MATCH_YES) goto done;

  if (g95_match_name(name) != MATCH_YES || g95_current_block() == NULL ||
      g95_match_eos() != MATCH_YES) {
    g95_error("Unexpected junk after ELSE IF statement at %C");
    goto cleanup;
  }

  if (strcmp(name, g95_current_block()->name) != 0) {
    g95_error("Label '%s' at %C doesn't match IF label '%s'",
	      name, g95_current_block()->name);
    goto cleanup;
  }

done:
  new_st.op = EXEC_IF;
  new_st.expr = expr;
  return MATCH_YES;

cleanup:
  g95_free_expr(expr);
  return MATCH_ERROR;
}


/* g95_free_iterator()-- Free a g95_iterator structure */

void g95_free_iterator(g95_iterator *iter, int flag) {

  if (iter == NULL) return;

  g95_free_expr(iter->var);
  g95_free_expr(iter->start);
  g95_free_expr(iter->end);
  g95_free_expr(iter->step);

  if (flag) g95_free(iter);
}


/* g95_match_do()-- Match a DO statement */

match g95_match_do(void) {
g95_iterator iter, *ip;
locus old_loc;
int label;
match m;

  old_loc = *g95_current_locus();
  label = 0;

  iter.var = iter.start = iter.end = iter.step = NULL;

  m = g95_match_label();
  if (m == MATCH_ERROR) return m;

  if (g95_match(" do") != MATCH_YES) return MATCH_NO;

/* Match an infinite DO, make it like a DO WHILE(.TRUE.) */

  if (g95_match_eos() == MATCH_YES) {
    iter.end = g95_logical_expr(1, NULL);
    new_st.op = EXEC_DO_WHILE;
    goto done;
  }

  if (g95_match("% ") != MATCH_YES) return MATCH_NO;

/* See if we have a DO WHILE */

  if (g95_match(" while ( %e )%t", &iter.end) == MATCH_YES) {
    new_st.op = EXEC_DO_WHILE;
    goto done;
  }

/* The abortive DO WHILE may have done something to the symbol table,
 * so we start over: */

  g95_undo_symbols();
  g95_set_locus(&old_loc);

  g95_match_label();    /* This won't error */
  g95_match(" do ");     /* This will work */

  m = g95_match_st_label(&label);
  if (m == MATCH_ERROR) goto cleanup;

  g95_match(" ,");      /* Optional comma */

  m = g95_match_iterator(&iter);
  if (m == MATCH_NO) return MATCH_NO;
  if (m == MATCH_ERROR) goto cleanup;

  if (g95_match_eos() != MATCH_YES) {
    g95_syntax_error(ST_DO);
    goto cleanup;
  }

  new_st.op = EXEC_DO;

done:
  new_st.label = label;
  g95_reference_st_label(label, ST_LABEL_TARGET);

  if (new_st.op == EXEC_DO_WHILE)
    new_st.expr = iter.end;
  else {
    new_st.ext.iterator = ip = g95_get_iterator();
    *ip = iter;
  }

  return MATCH_YES;

cleanup:
  g95_free_iterator(&iter, 0);

  return MATCH_ERROR;
}


/* match_exit_cycle()-- Match an EXIT or CYCLE statement */

static match match_exit_cycle(g95_statement st, g95_exec_op op) {
g95_state_data *p;
g95_symbol *sym;
match m;

  if (g95_match_eos() == MATCH_YES)
    sym = NULL;
  else {
    m = g95_match("% %s%t", &sym);
    if (m == MATCH_ERROR) return MATCH_ERROR;
    if (m == MATCH_NO) {
      g95_syntax_error(st);
      return MATCH_ERROR;
    }

    if (sym->attr.flavor != FL_LABEL) {
      g95_error("Name '%s' in %s statement at %C is not a loop name",
		sym->name, g95_ascii_statement(st));
      return MATCH_ERROR;
    }
  }

/* Find the loop mentioned specified by the label (or lack of a label) */

  for(p=g95_state_stack; p; p=p->previous)
    if (p->state == COMP_DO && (sym == NULL || sym == p->sym)) break;

  if (p == NULL) {
    if (sym == NULL)
      g95_error("%s statement at %C is not within a loop",
		g95_ascii_statement(st));
    else
      g95_error("%s statement at %C is not within loop '%s'",
		g95_ascii_statement(st), sym->name);

    return MATCH_ERROR;
  }

  new_st.op = op;
  new_st.sym = sym;

  return MATCH_YES;
}


/* g95_match_exit()-- Match the EXIT statement */

match g95_match_exit(void) {

  return match_exit_cycle(ST_EXIT,  EXEC_EXIT);
}


/* g95_match_cycle()-- Match the CYCLE statement */

match g95_match_cycle(void) {

  return match_exit_cycle(ST_CYCLE, EXEC_CYCLE);
}


/* g95_match_pause()-- Match the (deprecated) PAUSE statement */

match g95_match_pause(void) {
g95_expr *expr;

  if (g95_match_eos() == MATCH_YES) goto got_match;

  if (g95_match(" %e%t", &expr) == MATCH_YES) {
    g95_free_expr(expr);
    goto got_match;
  }

  return MATCH_NO;

got_match:
  g95_error("The PAUSE statement at %C is not allowed in Fortran 95");
  return MATCH_ERROR;
}


/* g95_match_stop()-- Match the STOP statement */

match g95_match_stop(void) {
const char *error_msg;
g95_expr *e;
int label;
match m;

  if (g95_match_eos() == MATCH_YES) {
    new_st.op = EXEC_STOP;
    new_st.label = -1;
    return MATCH_YES;
  }

  m = g95_match(" %e%t", &e);
  if (m != MATCH_YES) return m;

  if (e->expr_type != EXPR_CONSTANT) goto syntax;

  if (e->ts.type == BT_CHARACTER) {
    new_st.op = EXEC_STOP;
    new_st.expr = e;
    return MATCH_YES;
  }

  if (e->ts.type != BT_INTEGER) goto syntax;

  error_msg = g95_extract_int(e, &label);
  if (error_msg != NULL) {
    g95_error(error_msg);
    goto cleanup;
  }

  if (label < 0 || label > 99999) {
    g95_error("STOP label out of range at %C");
    goto cleanup;
  }

  new_st.op = EXEC_STOP;
  new_st.label = label;

  g95_free_expr(e);
  return MATCH_YES;

syntax:
  g95_syntax_error(ST_STOP);

cleanup:
  g95_free_expr(e);
  return MATCH_ERROR;
}    


/* g95_match_continue()-- match a CONTINUE statement */

match g95_match_continue(void) {

  if (g95_match_eos() != MATCH_YES) {
    g95_syntax_error(ST_CONTINUE);
    return MATCH_ERROR;
  }

  new_st.op = EXEC_NOP;
  return MATCH_YES;
}


/* g95_match_assign()-- Match the (deprecated) ASSIGN statement. */

match g95_match_assign(void) {
g95_expr *expr;
int label;

  if (g95_match(" %l to %v%t", &label, &expr) == MATCH_YES) {
    g95_free_expr(expr);
    g95_error("The ASSIGN statement at %C is not allowed in Fortran 95");
    return MATCH_ERROR;
  }

  return MATCH_NO;
}


/* g95_match_goto()-- Match the GO TO statement.  As a computed GOTO
 * statement is matched, it is transformed into an equivalent SELECT
 * block.  No tree is necessary, and the resulting jumps-to-jumps are
 * specifically optimized away by the back end. */

match g95_match_goto(void) {
g95_code *head, *tail;
g95_expr *expr;
g95_case *cp;
int i, label;
match m;

  if (g95_match(" %l%t", &label) == MATCH_YES) {
    if (g95_reference_st_label(label, ST_LABEL_TARGET) == FAILURE)
      return MATCH_ERROR;

    new_st.op = EXEC_GOTO;
    new_st.label = label;
    return MATCH_YES;
  }

/* The assigned GO TO statement is not allowed in Fortran 95 */

  if (g95_match(" %v", &expr) == MATCH_YES) {
    g95_free_expr(expr);
    g95_error("The assigned GO TO statement at %C is not allowed in "
	      "Fortran 95");
    return MATCH_ERROR;
  }

/* Last chance is a computed GO TO statement */

  if (g95_match(" (") != MATCH_YES) {
    g95_syntax_error(ST_GOTO);
    return MATCH_ERROR;
  }

  head = tail = NULL;
  i = 1;

  do {
    m = g95_match(" %l", &label);
    if (m != MATCH_YES) goto syntax;

    if (g95_reference_st_label(label, ST_LABEL_TARGET) == FAILURE)
      goto cleanup;

    if (head == NULL)
      head = tail = g95_get_code();
    else {
      tail->block = g95_get_code();
      tail = tail->block;
    }
    tail->label = label;
    tail->op = EXEC_SELECT;

    cp = g95_get_case();
    cp->low = cp->high = g95_int_expr(i++);
    tail->ext.case_list = cp;

    tail->next = g95_get_code();
    tail->next->op = EXEC_GOTO;
    tail->next->label = label;
  } while(g95_match(" ,") == MATCH_YES);

  if (g95_match(" )") != MATCH_YES) goto syntax;

  if (head == NULL) {
    g95_error("Statement label list in GOTO at %C cannot be empty");
    goto syntax;
  }

/* Get the rest of the statement */

  g95_match(" ,");

  if (g95_match(" %e%t", &expr) != MATCH_YES) goto syntax;

/* At this point, a computed GOTO has been fully matched and an
 * equivalent SELECT statement constructed. */

  new_st.op = EXEC_SELECT;
  new_st.expr = expr;
  new_st.block = head;
  return MATCH_YES;

syntax:
  g95_syntax_error(ST_GOTO);
cleanup:
  g95_free_statements(head);
  return MATCH_ERROR;
}


/* g95_free_alloc_list()-- Frees a list of g95_alloc structures */

void g95_free_alloc_list(g95_alloc *p) {
g95_alloc *q;

  for(; p; p=q) {
    q = p->next;
    g95_free_expr(p->expr);
    g95_free(p);
  }
}


/* g95_match_allocate()-- Match an ALLOCATE statement */

match g95_match_allocate(void) {
g95_alloc *head, *tail;
symbol_attribute attr;
g95_expr *stat;
match m;

  head = tail = NULL;
  stat = NULL;

  if (g95_match(" (") != MATCH_YES) goto syntax;

  for(;;) {
    if (head == NULL) 
      head = tail = g95_get_alloc();
    else {
      tail->next = g95_get_alloc();
      tail = tail->next;
    }

    m = g95_match(" %v", &tail->expr);
    if (m == MATCH_NO) goto syntax;
    if (m == MATCH_ERROR) goto cleanup;

    attr = g95_variable_attr(tail->expr, NULL);

    if (attr.allocatable == 0 && attr.pointer == 0) {
      g95_error("Expression in ALLOCATE statement at %C must be ALLOCATABLE "
		"or a POINTER");
      goto cleanup;
    }

    if (attr.dimension && tail->expr->rank == 0) {
      g95_error("Array specification required in ALLOCATE statement at %C");
      goto cleanup;
    }

    if (m == MATCH_ERROR) goto cleanup;
    if (m == MATCH_NO) goto syntax;

    if (g95_match(" ,") != MATCH_YES) break;

    m = g95_match(" stat = %v", &stat);
    if (m == MATCH_ERROR) goto cleanup;
    if (m == MATCH_YES) break;
  }

  if (g95_match(" )%t") != MATCH_YES) goto syntax;

  new_st.op = EXEC_ALLOCATE;
  new_st.expr = stat;
  new_st.ext.alloc_list = head;

  return MATCH_YES;

syntax:
  g95_syntax_error(ST_ALLOCATE);

cleanup:
  g95_free_expr(stat);
  g95_free_alloc_list(head);
  return MATCH_ERROR;
}


/* g95_match_nullify()-- Match a NULLIFY statement */

match g95_match_nullify(void) {
g95_alloc *head, *tail;
symbol_attribute attr;
match m;

  head = tail = NULL;
  if (g95_match(" (") != MATCH_YES) goto syntax;

  for(;;) {
    if (head == NULL) 
      head = tail = g95_get_alloc();
    else {
      tail->next = g95_get_alloc();
      tail = tail->next;
    }

    m = g95_match(" %v", &tail->expr);
    if (m == MATCH_ERROR) goto cleanup;
    if (m == MATCH_NO) goto syntax;

    attr = g95_variable_attr(tail->expr, NULL);
    if (attr.pointer == 0) {
      g95_error("Variable in NULLIFY statement at %C must be a POINTER");
      goto cleanup;
    }

    if (g95_match(" )") == MATCH_YES) break;
    if (g95_match(" ,") != MATCH_YES) goto syntax;
  }

  new_st.op = EXEC_NULLIFY;
  new_st.ext.alloc_list = head;

  return MATCH_YES;

syntax:
  g95_syntax_error(ST_NULLIFY);

cleanup:
  g95_free_alloc_list(head);
  return MATCH_ERROR;
}


/* g95_match_deallocate()-- Match a DEALLOCATE statement */

match g95_match_deallocate(void) {
g95_alloc *head, *tail;
symbol_attribute attr;
g95_expr *stat;
match m;

  head = tail = NULL;
  stat = NULL;

  if (g95_match(" (") != MATCH_YES) goto syntax;

  for(;;) {
    if (head == NULL)
      head = tail = g95_get_alloc();
    else {
      tail->next = g95_get_alloc();
      tail = tail->next;
    }

    m = g95_match(" %v", &tail->expr);
    if (m == MATCH_ERROR) goto cleanup;
    if (m == MATCH_NO) goto syntax;

    attr = g95_variable_attr(tail->expr, NULL);
    if (attr.pointer == 0 && attr.allocatable == 0) {
      g95_error("Expression in DEALLOCATE statement at %C must be "
		"ALLOCATABLE or a POINTER");
      goto cleanup;
    }

    if (g95_match(" ,") != MATCH_YES) break;

    m = g95_match(" stat = %v", &stat);
    if (m == MATCH_ERROR) goto cleanup;
    if (m == MATCH_YES) break;
  }

  if (g95_match(" )%t") != MATCH_YES) goto syntax;

  new_st.op = EXEC_DEALLOCATE;
  new_st.expr = stat;
  new_st.ext.alloc_list = head;

  return MATCH_YES;

syntax:
  g95_syntax_error(ST_DEALLOCATE);

cleanup:
  g95_free_expr(stat);
  g95_free_alloc_list(head);
  return MATCH_ERROR;
}


/* g95_match_return()-- Match a RETURN statement */

match g95_match_return(void) {
g95_expr *e;
match m;

  e = NULL;
  if (g95_match_eos() == MATCH_YES) goto done;

  if (g95_find_state(COMP_SUBROUTINE) == FAILURE) {
    g95_error("Alternate RETURN statement at %C is only allowed within "
	      "a SUBROUTINE");
    goto cleanup;
  }

  m = g95_match("% %e%t", &e);
  if (m == MATCH_YES) goto done;
  if (m == MATCH_ERROR) goto cleanup;

  g95_syntax_error(ST_RETURN);

cleanup:
  g95_free_expr(e);
  return MATCH_ERROR;

done:
  new_st.op = EXEC_RETURN;
  new_st.expr = e;

  return MATCH_YES;
}


/* g95_match_call()-- Match a CALL statement.  The tricky part here
 * are possible alternate return specifiers.  We handle these by
 * having all "subroutines" actually return an integer via a register
 * that gives the return number.  If the call specifies alternate
 * returns, we generate code for a SELECT statement whose case clauses
 * contain GOTOs to the various labels. */

match g95_match_call(void) {
char name[G95_MAX_SYMBOL_LEN+1];
g95_actual_arglist *a, *arglist;
g95_case *new_case;
g95_symbol *sym;
g95_code *c;
match m;
int i;

  arglist = NULL;

  m = g95_match("% %n", name);
  if (m == MATCH_NO) goto syntax;
  if (m != MATCH_YES) return m;

  if (g95_findget_symbol(name, NULL, 0, &sym)) return MATCH_ERROR;

  if (!sym->attr.generic &&
      !sym->attr.subroutine &&
      g95_add_subroutine(&sym->attr, NULL) == FAILURE)
    return MATCH_ERROR;

  if (g95_match_eos() != MATCH_YES) {
    m = g95_match_actual_arglist(1, &arglist);
    if (m == MATCH_NO) goto syntax;
    if (m == MATCH_ERROR) goto cleanup;

    if (g95_match_eos() != MATCH_YES) goto syntax;
  }

/* If any alternate return labels were found, construct a SELECT
 * statement that will jump to the right place */

  i = 0;
  for(a=arglist; a; a=a->next)
    if (a->expr == NULL) i = 1;

  if (i) {
    new_st.next = c = g95_get_code();
    c->op = EXEC_SELECT;
    c->expr = g95_int_expr(0);  /* For now */

    i = 0;
    for(a=arglist; a; a=a->next) {
      if (a->expr != NULL) continue;
      i++;

      c->block = g95_get_code();
      c = c->block;
      c->op = EXEC_SELECT;

      new_case = g95_get_case();
      new_case->high = new_case->low = g95_int_expr(i);
      c->ext.case_list = new_case;

      c->next = g95_get_code();
      c->next->op = EXEC_GOTO;
      c->next->label = a->label;

      g95_reference_st_label(a->label, ST_LABEL_TARGET);
    }
  }

  new_st.op = EXEC_CALL;
  new_st.sym = sym;
  new_st.ext.arglist = arglist;

  return MATCH_YES;

syntax:
  g95_syntax_error(ST_CALL);

cleanup:
  g95_free_actual_arglist(arglist);
  return MATCH_ERROR;
}


/* g95_match_common()-- Match a COMMON statement */

match g95_match_common(void) {
g95_symbol *sym, *common_name, **head, *tail;
g95_array_spec *as;
match m, m2;

  if (g95_match_eos() == MATCH_YES) goto syntax;

  if (g95_match(" / %s /", &common_name) != MATCH_YES) {
    common_name = NULL;
    g95_match(" / /");
  }

  as = NULL;

  for(;;) {
    if (common_name == NULL)
      head = &g95_current_ns->blank_common;
    else {
      head = &common_name->common_head;
      
      if (g95_add_common(&common_name->attr, NULL) == FAILURE)
	goto cleanup;
    }

    if (*head == NULL) tail = NULL;
    else {
      tail = *head;
      while(tail->common_next)
	tail = tail->common_next;
    }

/* Grab the list of symbols */

    for(;;) {
      m = g95_match(" %s", &sym);
      if (m == MATCH_ERROR) goto cleanup;
      if (m == MATCH_NO) goto syntax;

      if (sym->attr.in_common) {
	g95_error("Symbol '%s' at %C is already in a COMMON block", sym->name);
	goto cleanup;
      }

      if (g95_add_in_common(&sym->attr, NULL) == FAILURE) goto cleanup;

      if (tail != NULL)
	tail->common_next = sym;
      else
	*head = sym;

      tail = sym;

/* Deal with an optional array specification after the symbol name */

      m = g95_match_array_spec(&as);
      if (m == MATCH_ERROR) goto cleanup;

      if (m == MATCH_YES) {
	if (as->type != AS_EXPLICIT) {
	  g95_error("Array specification for symbol '%s' in COMMON at %C "
		    "must be explicit", sym->name);
	  goto cleanup;
	}

	if (g95_add_dimension(&sym->attr, NULL) == FAILURE) goto cleanup;

	if (sym->attr.pointer) {
	  g95_error("Symbol '%s' in COMMON at %C cannot be a POINTER array",
		    sym->name);
	  goto cleanup;
	}

	sym->as = as;
      }

      if (g95_match_eos() == MATCH_YES) goto done;

      m = g95_match(" ,");

      if (g95_match(" /") == MATCH_YES) {
	m2 = g95_match(" %s /", &common_name);
	if (m2 == MATCH_YES) break;
	if (m2 == MATCH_NO) goto syntax;
	goto cleanup;
      }

      if (m != MATCH_YES) goto syntax;
    }
  }

done:
  return MATCH_YES;

syntax:
  g95_syntax_error(ST_COMMON);

cleanup:
  g95_free_array_spec(as);

  return MATCH_ERROR;
}


/* g95_match_block_data()-- Match a BLOCK DATA program unit */

match g95_match_block_data(void) {
char name[G95_MAX_SYMBOL_LEN+1];
g95_symbol *sym;
match m;

  if (g95_match_eos() == MATCH_YES) {
    g95_new_block = NULL;
    return MATCH_YES;
  }   

  m = g95_match(" %n%t", &name);
  if (m != MATCH_YES) return MATCH_ERROR;

  if (g95_get_symbol(name, NULL, 0, &sym)) return MATCH_ERROR;

  if (g95_add_flavor(&sym->attr, FL_BLOCK_DATA, NULL) == FAILURE)
    return MATCH_ERROR;

  g95_new_block = sym;

  return MATCH_YES;
}


/* g95_free_namelist()-- Free a namelist structure */

void g95_free_namelist(g95_namelist *name) {
g95_namelist *n;

  for(;name; name=n) {
    n = name->next;
    g95_free(name);
  }
}


/* g95_match_namelist()-- Match a NAMELIST statement */

match g95_match_namelist(void) {
g95_symbol *group_name, *sym;
g95_namelist *nl;
match m, m2;

  m = g95_match(" / %s /", &group_name);
  if (m == MATCH_NO) goto syntax;
  if (m == MATCH_ERROR) goto error;

  for(;;) {
    if (group_name->attr.flavor != FL_NAMELIST &&
	g95_add_flavor(&group_name->attr, FL_NAMELIST, NULL) == FAILURE)
      return MATCH_ERROR;

    for(;;) {
      m = g95_match(" %s", &sym);
      if (m == MATCH_NO) goto syntax;
      if (m == MATCH_ERROR) goto error;

      if (sym->attr.in_namelist == 0 &&
	  g95_add_in_namelist(&sym->attr, NULL) == FAILURE) goto error;

/* TODO: worry about PRIVATE members of a PUBLIC namelist group */

      nl = g95_get_namelist();
      nl->sym = sym;

      if (group_name->namelist == NULL) 
	group_name->namelist = group_name->namelist_tail = nl;
      else {
	group_name->namelist_tail->next = nl;
	group_name->namelist_tail = nl;
      }

      if (g95_match_eos() == MATCH_YES) goto done;

      m = g95_match(" ,");

      if (g95_match(" /") == MATCH_YES) {
	m2 = g95_match(" %s /", &group_name);
	if (m2 == MATCH_YES) break;
	if (m2 == MATCH_ERROR) goto error;
	goto syntax;
      }

      if (m != MATCH_YES) goto syntax;
    }
  }

done:
  return MATCH_YES;

syntax:
  g95_syntax_error(ST_NAMELIST);

error:
  return MATCH_ERROR;
}


/* g95_match_module()-- Match a MODULE statement */

match g95_match_module(void) {
match m;

  m = g95_match(" %s%t", &g95_new_block);
  if (m != MATCH_YES) return m;

  if (g95_add_flavor(&g95_new_block->attr, FL_MODULE, NULL) == FAILURE)
    return MATCH_ERROR;

  return MATCH_YES;
}


/* g95_free_equiv()-- Free equivalence sets and lists.  Recursively is
 * the easiest way to do this. */

void g95_free_equiv(g95_equiv *eq) {

  if (eq == NULL) return;

  g95_free_equiv(eq->eq);
  g95_free_equiv(eq->next);

  g95_free_expr(eq->expr);
  g95_free(eq);
}


/* g95_match_equivalence()-- Match an EQUIVALENCE statement */

match g95_match_equivalence(void) {
g95_equiv *eq, *set, *tail;
g95_ref *ref;
match m;

  tail = NULL;

  for(;;) {
    eq = g95_get_equiv();
    if (tail == NULL) tail = eq;

    eq->next = g95_current_ns->equiv;
    g95_current_ns->equiv = eq;

    if (g95_match(" (") != MATCH_YES) goto syntax;

    set = eq;

    for(;;) {
      m = g95_match_variable(&set->expr, 1);
      if (m == MATCH_ERROR) goto cleanup;
      if (m == MATCH_NO) goto syntax;

      for(ref=set->expr->ref; ref; ref=ref->next)
	if (ref->type == REF_ARRAY && ref->ar.type == AR_SECTION) {
	  g95_error("Array reference in EQUIVALENCE at %C cannot be an "
		    "array section");
	  goto cleanup;
	}

      if (g95_match(" )") == MATCH_YES) break;
      if (g95_match(" ,") != MATCH_YES) goto syntax;

      set->eq = g95_get_equiv();
      set = set->eq;
    }

    if (g95_match_eos() == MATCH_YES) break;
    if (g95_match(" ,") != MATCH_YES) goto syntax;
  }

  return MATCH_YES;

syntax:
  g95_syntax_error(ST_EQUIVALENCE);

cleanup:
  eq = tail->next;
  tail->next = NULL;

  g95_free_equiv(g95_current_ns->equiv);
  g95_current_ns->equiv = eq;

  return MATCH_ERROR;
}


/* g95_match_st_function()-- Match a statement function declaration.
 * It is so easy to match non-statement function statements with a
 * MATCH_ERROR as opposed to MATCH_NO that we suppress error message
 * in most cases. */

match g95_match_st_function(void) {
g95_error_buf old_error;
g95_symbol *sym;
g95_expr *expr;
match m;

  m = g95_match(" %s", &sym);
  if (m != MATCH_YES) return m;

  g95_push_error(&old_error);

  if (g95_add_procedure(&sym->attr, PROC_ST_FUNCTION, NULL) == FAILURE)
    goto undo_error;

  if (g95_match_formal_arglist(sym, 1) != MATCH_YES) goto undo_error;

  m = g95_match(" = %E%t", &expr);
  if (m == MATCH_NO) goto undo_error;
  if (m == MATCH_ERROR) return m;

  sym->value = expr;

  return MATCH_YES;

undo_error:
  g95_pop_error(&old_error);
  return MATCH_NO;
}


/********************* DATA statement subroutines *********************/


/* free_variable()-- Free a g95_data_variable structure and everything
 * beneath it */

static void free_variable(g95_data_variable *p) {
g95_data_variable *q;

  for(; p; p=q) {
    q = p->next;
    g95_free_expr(p->expr);
    g95_free_iterator(&p->iter, 0);
    free_variable(p->list);
    
    g95_free(p);
  }
}


/* free_value()-- Free a g95_data_value structure and everything
 * beneath it */

static void free_value(g95_data_value *p) {
g95_data_value *q;

  for(; p; p=q) {
    q = p->next;
    g95_free_expr(p->expr);
    g95_free(p);
  }
}


/* g95_free_data()-- Free a list of g95_data structures */

void g95_free_data(g95_data *p) {
g95_data *q;

  for(; p; p=q) {
    q = p->next;

    free_variable(p->var);
    free_value(p->value);

    g95_free(p);
  }
}


static match var_element(g95_data_variable *);

/* var_list()-- Match a list of variables terminated by an iterator
 * and a right paren. */

static match var_list(g95_data_variable *parent) {
g95_data_variable *tail, var;
match m;

  m = var_element(&var);
  if (m == MATCH_ERROR) return MATCH_ERROR;
  if (m == MATCH_NO) goto syntax;

  tail = g95_get_data_variable();
  *tail = var;

  parent->list = tail;

  for(;;) {
    if (g95_match(" ,") != MATCH_YES) goto syntax;

    m = g95_match_iterator(&parent->iter);
    if (m == MATCH_YES) break;
    if (m == MATCH_ERROR) return MATCH_ERROR;

    m = var_element(&var);
    if (m == MATCH_ERROR) return MATCH_ERROR;
    if (m == MATCH_NO) goto syntax;

    tail->next = g95_get_data_variable();
    tail = tail->next;

    *tail = var;
  }

  if (g95_match(" )") != MATCH_YES) goto syntax;
  return MATCH_YES;

syntax:
  g95_syntax_error(ST_DATA);
  return MATCH_ERROR;
}


/* var_element()-- Match a single element in a data variable list,
 * which can be a variable-iterator list. */

static match var_element(g95_data_variable *new) {

  memset(new, '\0', sizeof(g95_data_variable));

  if (g95_match(" (") == MATCH_YES) return var_list(new);

  return g95_match(" %v", &new->expr);
}


/* top_var_list()-- Match the top-level list of data variables */

static match top_var_list(g95_data *d) {
g95_data_variable var, *tail, *new;
match m;

  tail = NULL;

  for(;;) {
    m = var_element(&var);
    if (m == MATCH_NO) goto syntax;
    if (m == MATCH_ERROR) return MATCH_ERROR;

    new = g95_get_data_variable();
    *new = var;

    if (tail == NULL)
      d->var = new;
    else
      tail->next = new;

    tail = new;

    if (g95_match(" /") == MATCH_YES) break;
    if (g95_match(" ,") != MATCH_YES) goto syntax;
  }

  return MATCH_YES;

syntax:
  g95_syntax_error(ST_DATA);
  return MATCH_ERROR;
}


static match match_data_constant(g95_expr **result) {
g95_symbol *sym;
g95_expr *expr;
match m;

  m = g95_match_literal_constant(&expr, 1);
  if (m == MATCH_YES) {
    *result = expr;
    return MATCH_YES;
  }

  if (m == MATCH_ERROR) return MATCH_ERROR;

  m = g95_match_null(result);
  if (m != MATCH_NO) return m;

  m = g95_match(" %s", &sym);
  if (m != MATCH_YES) return m;

  if (sym->attr.flavor != FL_PARAMETER) {
    g95_error("Symbol '%s' must be a PARAMETER in DATA statement at %C",
	      sym->name);
    return MATCH_ERROR;
  }

  *result = g95_copy_expr(sym->value);
  return MATCH_YES;
}


/* top_val_list()-- Match a list of values in a DATA statement.  The
 * leading '/' has already been seen at this point. */

static match top_val_list(g95_data *data) {
g95_data_value *new, *tail;
const char *msg;
g95_expr *expr;
match m;

  tail = NULL;

  for(;;) {
    m = match_data_constant(&expr);
    if (m == MATCH_NO) goto syntax;
    if (m == MATCH_ERROR) return MATCH_ERROR;

    new = g95_get_data_value();

    if (tail == NULL)
      data->value = new;
    else
      tail->next = new;

    tail = new;

    if (expr->ts.type != BT_INTEGER || g95_match(" *") != MATCH_YES) {
      tail->expr = expr;
      tail->repeat = 1;
    } else {
      msg = g95_extract_int(expr, &tail->repeat);
      if (msg != NULL) {
	g95_free_expr(expr);
	g95_error(msg);
	return MATCH_ERROR;
      }

      m = match_data_constant(&tail->expr);
      if (m == MATCH_NO) goto syntax;
      if (m == MATCH_ERROR) return MATCH_ERROR;
    }

    if (g95_option.verbose) {
      g95_status("DATA element:  %d * ", tail->repeat);
      g95_show_expr(tail->expr);
      g95_status("\n");
    }

    if (g95_match(" /") == MATCH_YES) break;
    if (g95_match(" ,") == MATCH_NO) goto syntax;
  }

  return MATCH_YES;

syntax:
  g95_syntax_error(ST_DATA);
  return MATCH_ERROR;
}


/* g95_match_data()-- Match a DATA statement */

match g95_match_data(void) {
g95_data *new;
match m;
  
  for(;;) {
    new = g95_get_data();

    m = top_var_list(new);
    if (m != MATCH_YES) goto cleanup;

    m = top_val_list(new);
    if (m != MATCH_YES) goto cleanup;

    new->next = g95_current_ns->data;
    g95_current_ns->data = new;

    if (g95_match_eos() == MATCH_YES) break;
    g95_match(" ,");
  }

  return MATCH_YES;

cleanup:
  g95_free_data(new);
  return MATCH_ERROR;
}


/* g95_match_where()-- Match a WHERE statement */

match g95_match_where(g95_statement *st) {
g95_expr *expr;
match m0, m;
g95_code *c;

  m0 = g95_match_label();
  if (m0 == MATCH_ERROR) return m0;

  m = g95_match(" where ( %e )", &expr);
  if (m != MATCH_YES) return m;

  if (g95_match_eos() == MATCH_YES) {
    *st = ST_WHERE_BLOCK;

    new_st.op = EXEC_WHERE;
    new_st.expr = expr;
    return MATCH_YES;
  }

  m = g95_match_assignment();
  if (m == MATCH_NO) g95_syntax_error(ST_WHERE);

  if (m != MATCH_YES) {
    g95_free_expr(expr);
    return MATCH_ERROR;
  }

/* We've got a simple WHERE statement */

  *st = ST_WHERE;
  c = g95_get_code();
  *c = new_st;

  g95_clear_new_st();

  new_st.op = EXEC_WHERE;
  new_st.expr = expr;
  new_st.block = c;

  return MATCH_YES;
}


/* g95_match_elsewhere()-- Match an ELSEWHERE statement.  We leave
 * behind a WHERE node in new_st if successful. */

match g95_match_elsewhere(void) {
char name[G95_MAX_SYMBOL_LEN+1];
g95_expr *expr;
match m;

  if (g95_current_state() != COMP_WHERE) {
    g95_error("ELSEWHERE statement at %C not enclosed in WHERE block");
    return MATCH_ERROR;
  }

  expr = NULL;

  if (g95_match(" (") == MATCH_YES) {
    m = g95_match_expr(&expr);
    if (m == MATCH_NO) goto syntax;
    if (m == MATCH_ERROR) return MATCH_ERROR;

    if (g95_match(" )") != MATCH_YES) goto syntax;
  }

  if (g95_match_eos() != MATCH_YES) { /* Better be a name at this point */
    m = g95_match_name(name);
    if (m == MATCH_NO) goto syntax;
    if (m == MATCH_ERROR) goto cleanup;

    if (g95_match_eos() != MATCH_YES) goto syntax;

    if (strcmp(name, g95_current_block()->name) != 0) {
      g95_error("Label '%s' at %C doesn't match WHERE label '%s'",
		name, g95_current_block()->name);
      goto cleanup;
    }
  }

  new_st.op = EXEC_WHERE;
  new_st.expr = expr;
  return MATCH_YES;

syntax:
  g95_syntax_error(ST_ELSEWHERE);

cleanup:
  g95_free_expr(expr);
  return MATCH_ERROR;
}


/******************** FORALL subroutines ********************/

/* g95_free_forall_iterator()-- Free a list of FORALL iterators */

void g95_free_forall_iterator(g95_forall_iterator *iter) {
g95_forall_iterator *next;

  while(iter) {
    next = iter->next;

    g95_free_expr(iter->var);
    g95_free_expr(iter->start);
    g95_free_expr(iter->end);
    g95_free_expr(iter->stride);

    g95_free(iter);
    iter = next;
  }
}


/* match_forall_iterator()-- Match an iterator as part of a FORALL
 * statement.  The format is:
 *     <var> = <start>:<end>[:<stride>][, <scalar mask>]  */

static match match_forall_iterator(g95_forall_iterator **result) {
g95_forall_iterator *iter;
locus where;
match m;

  where = *g95_current_locus(); 
  iter = g95_getmem(sizeof(g95_forall_iterator));

  m = g95_match_variable(&iter->var, 0);
  if (m != MATCH_YES) goto cleanup;

  if (g95_match(" =") != MATCH_YES) {
    m = MATCH_NO;
    goto cleanup;
  }

  m = g95_match_expr(&iter->start);
  if (m == MATCH_NO) goto syntax;
  if (m == MATCH_ERROR) goto cleanup;

  if (g95_match(" :") != MATCH_YES) goto syntax;

  m = g95_match_expr(&iter->end);
  if (m == MATCH_NO) goto syntax;
  if (m == MATCH_ERROR) goto cleanup;

  if (g95_match(" :") == MATCH_NO)
    iter->stride = g95_int_expr(1);
  else {
    m = g95_match_expr(&iter->stride);
    if (m == MATCH_NO) goto syntax;
    if (m == MATCH_ERROR) goto cleanup;
  }

  *result = iter;
  return MATCH_YES;

syntax:
  g95_error("Syntax error in FORALL iterator at %C");
  m = MATCH_ERROR;

cleanup:
  g95_set_locus(&where);
  g95_free_forall_iterator(iter);
  return m;
}


/* g95_match_forall()-- Match a FORALL statement */

match g95_match_forall(g95_statement *st) {
g95_forall_iterator *head, *tail, *new;
g95_expr *mask;
g95_code *c;
match m0, m;

  head = tail = NULL; 
  mask = NULL;
  c = NULL;

  m0 = g95_match_label(); 
  if (m0 == MATCH_ERROR) return MATCH_ERROR;

  m = g95_match(" forall (");
  if (m != MATCH_YES) return m;

  m = match_forall_iterator(&new);
  if (m == MATCH_ERROR) goto cleanup;
  if (m == MATCH_NO) goto syntax;

  head = tail = new;

  for(;;) {
    if (g95_match(" ,") != MATCH_YES) break;

    m = match_forall_iterator(&new);
    if (m == MATCH_ERROR) goto cleanup;
    if (m == MATCH_YES) {
      tail->next = new;
      tail = new;
      continue;
    }

/* Have to have a mask expression */

    m = g95_match_expr(&mask);
    if (m == MATCH_NO) goto syntax;
    if (m == MATCH_ERROR) goto cleanup;

    break;
  }

  if (g95_match(" )") == MATCH_NO) goto syntax;

  if (g95_match_eos() == MATCH_YES) {
    *st = ST_FORALL_BLOCK;

    new_st.op = EXEC_FORALL;
    new_st.expr = mask;
    new_st.ext.forall_iterator = head;

    return MATCH_YES;
  }

  m = g95_match_assignment();
  if (m == MATCH_ERROR) goto cleanup;
  if (m == MATCH_NO) {
    m = g95_match_pointer_assignment();
    if (m == MATCH_ERROR) goto cleanup;
    if (m == MATCH_NO) goto syntax;
  }

  c = g95_get_code();
  *c = new_st;

  if (g95_match_eos() != MATCH_YES) goto syntax;

  g95_clear_new_st();
  new_st.op = EXEC_FORALL;
  new_st.expr = mask;
  new_st.ext.forall_iterator = head;
  new_st.block = g95_get_code();

  new_st.block->op = EXEC_FORALL;
  new_st.block->next = c;

  *st = ST_FORALL;
  return MATCH_YES;

syntax:
  g95_syntax_error(ST_FORALL);

cleanup:
  g95_free_forall_iterator(head);
  g95_free_expr(mask);
  g95_free_statements(c);
  return MATCH_NO;
}

