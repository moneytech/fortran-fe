/* FORMAT statement
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

/* format.c-- match the FORMAT statement and check format string */

#include <ctype.h>
#include <string.h>
#include "g95.h"

/* format tokens returned by format_lex() */

typedef enum {
  FMT_UNKNOWN=1, FMT_NEGINT, FMT_ZERO, FMT_POSINT, FMT_PERIOD, FMT_COMMA,
  FMT_COLON, FMT_SLASH, FMT_DOLLAR, FMT_POS, FMT_LPAREN, FMT_RPAREN, FMT_X,
  FMT_SIGN, FMT_BLANK, FMT_CHAR, FMT_P, FMT_IBOZ, FMT_F, FMT_E, FMT_EXT,
  FMT_G, FMT_L, FMT_A, FMT_D, FMT_H, FMT_END
} format_token;


/* Local variables for checking format strings.  The saved_token is
 * used to back up by a single format token during the parsing process. */

static char last_char, *format_string;
static int format_length, use_last_char;

static format_token saved_token;

static enum { MODE_STRING, MODE_FORMAT, MODE_COPY } mode;
static locus last_locus;


/* next_char()-- Return the next character in the format string */

static char next_char(int in_string) {
char c;

  if (use_last_char) {
    use_last_char = 0;
    return last_char;
  }

  last_locus = *g95_current_locus();

  format_length++;

  if (mode == MODE_STRING)
    c = *format_string++;
  else {
    c = g95_next_char_literal(in_string);
    if (c == '\n') c = '\0';

    if (mode == MODE_COPY) *format_string++ = c;
  }


  c = toupper(c);
  last_char = c;
  return c;
}


/* unget_char()-- Back up one character position.  Only works once. */

static void unget_char(void) {

  use_last_char = 1;
}


/* format_lex()-- Simple lexical analyzer for getting the next token
 * in a FORMAT statement. */

static format_token format_lex(void) {
format_token token;
char c, delim;
int zflag;

  if (saved_token != 0) {
    token = saved_token;
    saved_token = 0;
    return token;
  }

  do {
    c = next_char(0);
  } while(g95_is_whitespace(c));

  switch(c) {
  case '-':
    c = next_char(0);
    if (!isdigit(c)) {
      token = FMT_UNKNOWN;
      break;
    }

    do {
      c = next_char(0);
    } while(isdigit(c));

    unget_char();
    token = FMT_NEGINT;
    break;

  case '0': case '1': case '2': case '3': case '4':
  case '5': case '6': case '7': case '8': case '9':
    zflag = 1;

    do {
      if (c != '0') zflag = 0;
      c = next_char(0);
    } while(isdigit(c));

    unget_char();
    token = zflag ? FMT_ZERO : FMT_POSINT;
    break;

  case '.':
    token = FMT_PERIOD;
    break;

  case ',':
    token = FMT_COMMA;
    break;

  case ':':
    token = FMT_COLON;
    break;

  case '/':
    token = FMT_SLASH;
    break;

  case '$':
    token = FMT_DOLLAR;
    break;

  case 'T':
    c = next_char(0);
    if (c != 'L' && c != 'R') unget_char();

    token = FMT_POS;
    break;

  case '(':
    token = FMT_LPAREN;
    break;

  case ')':
    token = FMT_RPAREN;
    break;

  case 'X':
    token = FMT_X;
    break;

  case 'S':
    c = next_char(0);
    if (c != 'P' && c != 'S') unget_char();

    token = FMT_SIGN;
    break;

  case 'B':
    c = next_char(0);
    if (c == 'N' || c == 'Z')
      token = FMT_BLANK;
    else {
      unget_char();
      token = FMT_IBOZ;
    }

    break;

  case '\'': case '"':
    delim = c;      

    for(;;) {
      c = next_char(1);
      if (c == '\0') {
	token = FMT_END;
	break;
      }

      if (c == delim) {
	c = next_char(1);

	if (c == '\0') {
	  token = FMT_END;
	  break;
	}

	if (c != delim) {
	  unget_char();
	  token = FMT_CHAR;
	  break;
	}
      }
    }
    break;

  case 'P':
    token = FMT_P;
    break;

  case 'I': case 'O': case 'Z':
    token = FMT_IBOZ;
    break;

  case 'F':
    token = FMT_F;
    break;

  case 'E':
    c = next_char(0);
    if (c == 'N' || c == 'S') 
      token = FMT_EXT;
    else {
      token = FMT_E;
      unget_char();
    }

    break;

  case 'G':
    token = FMT_G;
    break;

  case 'H':
    token = FMT_H;
    break;

  case 'L':
    token = FMT_L;
    break;

  case 'A':
    token = FMT_A;
    break;

  case 'D':
    token = FMT_D;
    break;

  case '\0':
    token = FMT_END;
    break;

  default:
    token = FMT_UNKNOWN;
    break;
  }

  return token;
}


/* check_format()-- Check a format statement.  The format string,
 * either from a FORMAT statement or a constant in an I/O statement
 * has already been parsed by itself, and we are checking it for validity.
 *
 * The dual origin means that the warning message is a little less than
 * great. */


static try check_format(void) {
const char *error,
     *posint_required = "Positive width required",
     *period_required = "Period required",
     *nonneg_required = "Nonnegative width required",
     *unexpected_element = "Unexpected element",
     *unexpected_end = "Unexpected end of format string";

format_token t, u;
int level;
try rv;

  use_last_char = 0; 
  saved_token = 0;
  level = 0;
  rv = SUCCESS;

  t = format_lex();
  if (t != FMT_LPAREN) {
    error = "Missing leading left parenthesis";
    goto syntax;
  }

  t = format_lex();
  if (t == FMT_RPAREN) goto finished;   /* Empty format is legal */
  saved_token = t;

/* In this state, the next thing has to be a format item */

format_item:
  t = format_lex();
  switch(t) {
  case FMT_POSINT:
    t = format_lex();
    if (t == FMT_LPAREN) {
      level++;
      goto format_item;
    }

    if (t == FMT_SLASH) goto optional_comma;

    goto data_desc;

  case FMT_LPAREN:
    level++;
    goto format_item;

  case FMT_NEGINT:
  case FMT_ZERO:   /* Nonpositive can only be a prelude to a P format */
    t = format_lex();
    if (t != FMT_P) {
      error = "Expected P edit descriptor";
      goto syntax;
    }

    goto data_desc;

  case FMT_P:       /* P and X require a prior number */
    error = "P descriptor requires leading scale factor";
    goto syntax;

  case FMT_X:
    error = "X descriptor requires leading space count";
    goto syntax;

  case FMT_SIGN:
  case FMT_BLANK:
  case FMT_CHAR:
    goto between_desc;

  case FMT_COLON:
  case FMT_SLASH:
    goto optional_comma;

  case FMT_DOLLAR:
    t = format_lex();
    if (t != FMT_RPAREN || level > 0) {
      error = "$ must the last specifier";
      goto syntax;
    }

    if (t == FMT_RPAREN) level--;
    goto finished;

  case FMT_POS:  case FMT_IBOZ:  case FMT_F:  case FMT_E:  case FMT_EXT:
  case FMT_G:    case FMT_L:     case FMT_A:  case FMT_D:
    goto data_desc;

  case FMT_H:
    error = "The H format specifier is not allowed in Fortran 95";
    goto syntax;

  case FMT_END:
    error = unexpected_end;
    goto syntax;

  default:
    error = unexpected_element;
    goto syntax;
  }

/* In this state, t must currently be a data descriptor.  Deal with
 * things that can/must follow the descriptor */

data_desc:
  switch(t) {
  case FMT_SIGN:
  case FMT_BLANK:
  case FMT_X:
    break;

  case FMT_P:
    goto optional_comma;

  case FMT_POS:
  case FMT_L:
    t = format_lex();
    if (t == FMT_POSINT) break;

    error = posint_required;
    goto syntax;

  case FMT_A:
    t = format_lex();
    if (t != FMT_POSINT) saved_token = t;
    break;

  case FMT_D:  case FMT_E:
  case FMT_G:  case FMT_EXT:
    u = format_lex();
    if (u != FMT_POSINT) {
      error = posint_required;
      goto syntax;
    }

    u = format_lex();
    if (u != FMT_PERIOD) {
      error = period_required;
      goto syntax;
    }

    u = format_lex();
    if (u != FMT_ZERO && u != FMT_POSINT) {
      error = nonneg_required;
      goto syntax;
    }

    if (t == FMT_D) break;

/* Look for optional exponent */

    u = format_lex();
    if (u != FMT_E) {
      saved_token = u;
    } else {
      u = format_lex();
      if (u != FMT_POSINT) {
	error = "Positive exponent width required";
	goto syntax;
      }
    }

    break;

  case FMT_F:
    t = format_lex();
    if (t != FMT_POSINT && t != FMT_ZERO) {
      error = nonneg_required;
      goto syntax;
    }

    t = format_lex();
    if (t != FMT_PERIOD) {
      error = period_required;
      goto syntax;
    }

    t = format_lex();
    if (t != FMT_ZERO && t != FMT_POSINT) {
      error = nonneg_required;
      goto syntax;
    }

    break;

  case FMT_H:
    error = "The H format specifier is not allowed in Fortran 95";
    goto syntax;

  case FMT_IBOZ:
    t = format_lex();
    if (t != FMT_ZERO && t != FMT_POSINT) {
      error = nonneg_required;
      goto syntax;
    }

    t = format_lex();
    if (t != FMT_PERIOD) {
      saved_token = t;
    } else {
      t = format_lex();
      if (t != FMT_ZERO && t != FMT_POSINT) {
	error = nonneg_required;
	goto syntax;
      }
    }

    break;

  default:
    error = unexpected_element;
    goto syntax;
  }

/* Between a descriptor and what comes next */

between_desc:
  t = format_lex();
  switch(t) {

  case FMT_COMMA:
    goto format_item;

  case FMT_RPAREN:
    level--;
    if (level < 0) goto finished;
    goto between_desc;

  case FMT_COLON:
  case FMT_SLASH:
    goto optional_comma;

  case FMT_END:
    error = unexpected_end;
    goto syntax;

  default:
    error = "Missing comma";
    goto syntax;
  }

/* Optional comma is a weird between state where we've just finished
 * reading a colon, slash or P descriptor. */

optional_comma:
  t = format_lex();
  switch(t) {
  case FMT_COMMA:
    break;

  case FMT_RPAREN:
    level--;
    if (level < 0) goto finished;
    goto between_desc;

  default:     /* Assume that we have another format item */
    saved_token = t;
    break;
  }

  goto format_item;

/* Something went wrong.  If the format we're checking is a string,
 * generate a warning, since the program is correct.  If the format is
 * in a FORMAT statement, this messes up parsing, which is an error. */

syntax:
  if (mode != MODE_STRING)
    g95_error("%s in format string at %C", error);
  else {
    g95_warning("%s in format string at %C", error);

    /* More elaborate measures are needed to show where a problem is
     * within a format string that has been calculated. */
  }

  rv = FAILURE;

finished:
  return rv;
}


/* g95_check_format_string()-- Given an expression node that is a
 * constant string, see if it looks like a format string */

void g95_check_format_string(g95_expr *e) {

  mode = MODE_STRING;
  format_string = e->value.character.string;
  check_format();
}


/* g95_match_format()-- Match a FORMAT statement.  This amounts to
 * actually parsing the format descriptors in order to correctly
 * locate the end of the format string.  */

match g95_match_format(void) {
char *p;
locus start;

  if (g95_statement_label == 0) {
    g95_error("FORMAT statement at %C does not have a statement label");
    return MATCH_ERROR;
  }

  g95_gobble_whitespace();

  mode = MODE_FORMAT;
  format_length = 0;

  start = *g95_current_locus();

  if (check_format() == FAILURE) return MATCH_ERROR;

  if (g95_match_eos() != MATCH_YES) {
    g95_syntax_error(ST_FORMAT);
    return MATCH_ERROR;
  }

  g95_set_locus(&start);      /* Back to the beginning */
  p = format_string = g95_getmem(format_length+1);

  mode = MODE_COPY;
  check_format();       /* Guaranteed to succeed */

  g95_match_eos();      /* Guaranteed to succeed */

/* More here later */

  new_st.op = EXEC_NOP;
  return MATCH_YES;
}

