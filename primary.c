/* Primary expression subroutines
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

/* primary.c-- Match primary expressions */

#include <ctype.h>
#include <string.h>
#include <stdlib.h>
#include "g95.h"

/* match_kind_param()-- Matches a kind-parameter expression, which is
 * either a named symbolic constant or a nonnegative integer constant.
 * If successful, sets the kind value to the correct integer.  */

static match match_kind_param(int *kind) {
char name[G95_MAX_SYMBOL_LEN+1];
g95_symbol *sym;
const char *p;
match m;

  m = g95_match_small_literal_int(kind);
  if (m != MATCH_NO) return m;

  m = g95_match_name(name);
  if (m != MATCH_YES) return m;

  if (g95_find_symbol(name, NULL, 1, &sym)) return MATCH_ERROR;

  if (sym == NULL) return MATCH_NO;

  if (sym->attr.flavor != FL_PARAMETER) return MATCH_NO;

  p = g95_extract_int(sym->value, kind);
  if (p != NULL) return MATCH_NO;

  if (*kind < 0) return MATCH_NO;

  return MATCH_YES;
}


/* get_kind()-- Gets a trailing kind-specification for non-character
 * variables.  Normally returns the integer kind value or
 *   -1   if an error was generated
 *   -2   if no kind was found
 */

static int get_kind(void) {
int kind;
match m;

  if (g95_match_char('_') != MATCH_YES) return -2;

  m = match_kind_param(&kind);
  if (m == MATCH_NO) g95_error("Missing kind-parameter at %C");

  return (m == MATCH_YES) ? kind : -1;
}


/* check_digit()-- Given a character and a radix, see if the character
 * is a valid digit in that radix. */

static int check_digit(int c, int radix) {
int r;

  switch(radix) {
  case 2:
    r = ('0' <= c && c <= '1');
    break;

  case 8:
    r = ('0' <= c && c <= '7');
    break;

  case 10:
    r = ('0' <= c && c <= '9');
    break;

  case 16:
    r = ('0' <= c && c <= '9') || ('a' <= c && c <= 'f');
    break;

  default:
    g95_internal_error("check_digit(): bad radix");
  }

  return r;
}


/* match_digits()-- Match the digit string part of an integer.
 * If the buffer is NULL, we just count characters for the second
 * pass.  Returns the number of characters matched, -1 for no match. */

static int match_digits(int signflag, int radix, char *buffer) {
locus old_loc;
int length, c;

  length = 0;
  c = g95_next_char();

  if (signflag && (c == '+' || c == '-')) {
    if (buffer != NULL) *buffer++ = c;
    c = g95_next_char();
    length++;
  }

  if (!check_digit(c, radix)) return -1;

  length++;
  if (buffer != NULL) *buffer++ = c;

  for(;;) {
    old_loc = *g95_current_locus();
    c = g95_next_char();

    if (!check_digit(c, radix)) break;

    if (buffer != NULL) *buffer++ = c;
    length++;
  }

  g95_set_locus(&old_loc);

  return length;
}


/* match_integer_constant()-- Match an integer (digit string and
 * optional kind). */

static match match_integer_constant(g95_expr **result, int signflag) {
int length, kind;
locus old_loc;
char *buffer;
g95_expr *e;

  old_loc = *g95_current_locus();
  g95_gobble_whitespace();

  length = match_digits(signflag, 10, NULL);
  g95_set_locus(&old_loc);
  if (length == -1) return MATCH_NO;

  buffer = alloca(length+1);
  memset(buffer, '\0', length+1);

  g95_gobble_whitespace();

  match_digits(signflag, 10, buffer);

  kind = get_kind();
  if (kind == -2) kind = g95_default_integer_kind();
  if (kind == -1) return MATCH_ERROR;

  if (g95_validate_kind(BT_INTEGER, kind) == -1) {
    g95_error("Integer kind %d at %C not available", kind);
    return MATCH_ERROR;
  }

  e = g95_convert_integer(buffer, kind, 10);
  e->where = *g95_current_locus();

  if (g95_check_integer_range(e->value.integer, kind) != ARITH_OK) {
    g95_error("Integer too big for its kind at %C");

    g95_free_expr(e);
    return MATCH_ERROR;
  }

  *result = e;
  return MATCH_YES;
}


/* match_boz_constant()-- Match a binary, octal or hexadecimal
 * constant that can be found in a DATA statement */

static match match_boz_constant(g95_expr **result) {
int radix, delim, length;
const char *rname;
locus old_loc;
char *buffer;
g95_expr *e;
mpz_t mask;

  old_loc = *g95_current_locus();
  g95_gobble_whitespace();

  switch (g95_next_char()) {
  case 'b':  radix = 2;  rname = "binary";       break;
  case 'o':  radix = 8;  rname = "octal";        break;
  case 'z':  radix = 16; rname = "hexadecimal";  break;
  default: goto backup;
  }

  /* no whitespace allowed here */

  delim = g95_next_char();
  if (delim != '\'' && delim != '\"') goto backup;

  old_loc = *g95_current_locus();
  
  length = match_digits(0, radix, NULL);
  if (length == -1) {
    g95_error("Empty set of digits in %s constants at %C", rname);
    return MATCH_ERROR;
  }

  if (g95_next_char() != delim) {
    g95_error("Illegal character in %s constant at %C.", rname);
    return MATCH_ERROR;
  }

  g95_set_locus(&old_loc);

  buffer = alloca(length+1);
  memset(buffer, '\0', length+1);

  match_digits(0, radix, buffer);
  g95_next_char();

  e = g95_convert_integer(buffer, g95_default_integer_kind(), radix);
  e->where = *g95_current_locus();

/* If we're doing twos complement, allow constants with the high bit
 * set to be faithfully reproduced. */

  if (g95_integer_kinds[0].radix == 2 && 
      mpz_cmp(e->value.integer, g95_integer_kinds[0].huge) > 0) {

    mpz_init(mask);
    mpz_ui_pow_ui(mask, 2, g95_integer_kinds[0].digits);

    if (mpz_cmp(e->value.integer, mask) != 0) {
      mpz_com(e->value.integer, e->value.integer);
      mpz_add_ui(e->value.integer, e->value.integer, 1);

      mpz_ui_pow_ui(mask, 2, g95_integer_kinds[0].digits);
      mpz_sub_ui(mask, mask, 1);

      mpz_and(e->value.integer, e->value.integer, mask);
      mpz_neg(e->value.integer, e->value.integer);
    }

    mpz_clear(mask);
  }

  if (g95_check_integer_range(e->value.integer, g95_default_integer_kind())
      != ARITH_OK) {
    g95_error("Integer too big for default integer kind at %C");

    g95_free_expr(e);
    return MATCH_ERROR;
  }

  *result = e;
  return MATCH_YES;

backup:
  g95_set_locus(&old_loc);
  return MATCH_NO;
}


/* match_real_constant()-- Match a real constant of some sort. */

static match match_real_constant(g95_expr **result, int signflag) {
int kind, c, count, seen_dp, seen_digits, exp_char;
locus old_loc, temp_loc;
char *p, *buffer;
g95_expr *e;

  old_loc = *g95_current_locus();
  g95_gobble_whitespace();

  e = NULL;
  buffer = NULL;

  count = 0;
  seen_dp = 0;
  seen_digits = 0;
  exp_char = ' ';

  c = g95_next_char();
  if (signflag && (c == '+' || c == '-')) {
    c = g95_next_char();
    count++;
  }

/* Scan significand */

  for(;; c=g95_next_char(), count++) {
    if (c == '.') {
      if (seen_dp) goto done;

      /* Check to see if "." goes with a following operator like ".eq." */

      temp_loc = *g95_current_locus();
      c = g95_next_char();

      if (c == 'e' || c == 'd' || c == 'q') {
	c = g95_next_char();
	if (c == '.') goto done;   /* Operator named .e. or .d. */
      }

      if (isalpha(c)) goto done;   /* Distinguish 1.e9 from 1.eq.2 */

      g95_set_locus(&temp_loc);
      seen_dp = 1;
      continue;
    }

    if (isdigit(c)) {
      seen_digits = 1;
      continue;
    }

    break;
  }

  if (!seen_digits || (c != 'e' && c != 'd' && c != 'q')) goto done;
  exp_char = c;

/* scan exponent */

  c = g95_next_char();
  count++;

  if (c == '+' || c == '-') {  /* optional sign */
    c = g95_next_char();
    count++;
  }

  if (!isdigit(c)) {
    if (!seen_digits) {
      g95_set_locus(&old_loc);
      return MATCH_NO;   /* ".e" can be something else */
    }

    g95_error("Missing exponent in real number at %C");
    return MATCH_ERROR;
  }

  while(isdigit(c)) {
    c = g95_next_char();
    count++;
  }

/* See what we've got */

done:
  if (!seen_digits || (!seen_dp && exp_char == ' ')) {
    g95_set_locus(&old_loc);
    return MATCH_NO;
  }

/* Convert the number */

  g95_set_locus(&old_loc);
  g95_gobble_whitespace();

  buffer = alloca(count+1);
  memset(buffer, '\0', count+1);

  p = buffer;
  while(count>0) {
    *p = g95_next_char();
    if (*p == 'd' || *p == 'q') *p = 'e';   /* Hack for mpf_init_set_str() */
    p++;
    count--;
  }

  kind = get_kind();
  if (kind == -1) goto cleanup;

  switch(exp_char) {
  case 'd':
    if (kind != -2) {
      g95_error("Real number at %C has a 'd' exponent and an explicit kind");
      goto cleanup;
    }
    kind = g95_default_double_kind();
    break;

  case 'q':
    if (kind != -2) {
      g95_error("Real number at %C has a 'q' exponent and an explicit kind");
      goto cleanup;
    }
    kind = g95_option.q_kind;
    break;

  default:
    if (kind == -2)  kind = g95_default_real_kind(); 

    if (g95_validate_kind(BT_REAL, kind) == -1) {
      g95_error("Invalid real kind %d at %C", kind);
      goto cleanup;
    }
  }

  e = g95_convert_real(buffer, kind);
  e->where = *g95_current_locus();

  switch(g95_check_real_range(e->value.real, kind)) {
    case ARITH_OK: break;
    case ARITH_OVERFLOW:
      g95_error("Real constant overflows its kind at %C");
      goto cleanup;

    case ARITH_UNDERFLOW:
      g95_error("Real constant underflows its kind at %C");
      goto cleanup;

    default:
      g95_internal_error("g95_check_real_range() returned bad value");
  }

  *result = e;
  return MATCH_YES;

cleanup:
  g95_free_expr(e);
  return MATCH_ERROR;
}


/* g95_next_string_char()-- Reads the next character of a string constant,
 * taking care to return doubled delimiters on the input as a single
 * instance of the delimiter.  Special return values are:
 *   -1   End of the string, as determined by the delimiter
 *   -2   Unterminated string detected
 *
 * Backslash codes are also expanded at this time. */

int g95_next_string_char(char delimiter) {
locus old_locus;
int c;

  c = g95_next_char_literal(1);

  if (c == '\n') return -2;

  if (c == '\\') {
    old_locus = *g95_current_locus();

    switch(g95_next_char_literal(1)) {
    case 'a':  c = '\a'; break;
    case 'b':  c = '\b'; break;
    case 't':  c = '\t'; break;
    case 'f':  c = '\f'; break;
    case 'n':  c = '\n'; break;
    case 'r':  c = '\r'; break;
    case 'v':  c = '\v'; break;
    case '\\': c = '\\'; break;

    default:     /* Unknown backslash codes are simply not expanded */
      g95_set_locus(&old_locus);
      break;
    }
  }

  if (c != delimiter) return c;

  old_locus = *g95_current_locus();
  c = g95_next_char_literal(1);

  if (c == delimiter) return c;
  g95_set_locus(&old_locus);

  return -1;
}


/* match_charkind_name()-- Special case of g95_match_name() that
 * matches a parameter kind name before a string constant.  This takes
 * case of the weird but legal case of: weird case of:
 *             kind_____'string'
 *
 * where kind____ is a parameter. g95_match_name() will happily slurp
 * up all the underscores, which leads to problems.  If we return
 * MATCH_YES, the parse pointer points to the final underscore, which
 * is not part of the name.  We never return MATCH_ERROR-- errors in
 * the name will be detected later. */

static match match_charkind_name(char *name) {
locus old_loc;
char c, peek;
int len;

  g95_gobble_whitespace(); 
  c = g95_next_char();
  if (!isalpha(c)) return MATCH_NO;

  *name++ = c;
  len = 1;

  for(;;) {
    old_loc = *g95_current_locus();
    c = g95_next_char();

    if (c == '_') {
      peek = g95_peek_char();
      
      if (peek == '\'' || peek == '\"') {
	g95_set_locus(&old_loc);
	*name = '\0';
	return MATCH_YES;
      }
    }

    if (!isalnum(c) && c != '_' && g95_option.dollar && c != '$') break;

    *name++ = c;
    if (++len > G95_MAX_SYMBOL_LEN) break;
  }

  return MATCH_NO;
}


/* match_string_constant()-- See if the current input matches a
 * character constant.  Lots of contortions have to be done to match
 * the kind parameter which comes before the actual string.  The main
 * consideration is that we don't want to error out too quickly.  For
 * example, we don't actually do any validation of the kinds until we
 * have actually seen a legal delimiter.  Using match_kind_param()
 * generates errors too quickly. */

static match match_string_constant(g95_expr **result) {
char *p, name[G95_MAX_SYMBOL_LEN+1];
int i, c, kind, length, delimiter;
locus old_locus, start_locus;
g95_symbol *sym;
const char *q;
g95_expr *e;
match m;

  old_locus = *g95_current_locus();

  g95_gobble_whitespace();

  start_locus = *g95_current_locus();

  c = g95_next_char();
  if (c == '\'' || c == '"') {
    kind = g95_default_character_kind();
    goto got_delim;
  }

  if (isdigit(c)) {
    kind = 0;

    while(isdigit(c)) {
      kind = kind*10 + c - '0';
      if (kind > 9999999) goto no_match;
      c = g95_next_char();
    }

  } else {
    g95_set_locus(&old_locus);

    m = match_charkind_name(name);
    if (m != MATCH_YES) goto no_match;

    if (g95_find_symbol(name, NULL, 1, &sym) || sym == NULL ||
	sym->attr.flavor != FL_PARAMETER) goto no_match;

    kind = -1;
    c = g95_next_char();
  }

  if (c == ' ') { 
    g95_gobble_whitespace();
    c = g95_next_char();
  }

  if (c != '_') goto no_match;

  g95_gobble_whitespace();
  start_locus = *g95_current_locus();

  c = g95_next_char();
  if (c != '\'' && c != '"') goto no_match;

  if (kind == -1) {
    q = g95_extract_int(sym->value, &kind);
    if (q != NULL) {
      g95_error(q);
      return MATCH_ERROR;
    }
  }

  if (g95_validate_kind(BT_CHARACTER, kind) == -1) {
    g95_error("Invalid kind %d for CHARACTER constant at %C", kind);
    return MATCH_ERROR;
  }

/* Scan the string into a block of memory by first figuring out how
 * long it is, allocating the structure, then re-reading it.  This
 * isn't particularly efficient, but string constants aren't that
 * common in most code.  Someday I'll read more on obstacks. */

got_delim:
  delimiter = c;
  length = 0;

  for(;;) {
    c = g95_next_string_char(delimiter);
    if (c == -1) break;
    if (c == -2) {
      g95_set_locus(&start_locus);
      g95_error("Unterminated character constant beginning at %C");
      return MATCH_ERROR;
    }

    length++;
  }

  e = g95_get_expr();

  e->expr_type = EXPR_CONSTANT;
  e->ref = NULL;
  e->ts.type = BT_CHARACTER;
  e->ts.kind = kind;
  e->where = start_locus;
   
  e->value.character.string = p = g95_getmem(length+1);
  e->value.character.length = length;

  g95_set_locus(&start_locus);
  g95_next_char();              /* Skip delimiter */

  for(i=0; i<length; i++)
    *p++ = g95_next_string_char(delimiter);

  *p = '\0';     /* C-style string is for development/debug purposes */

  if (g95_next_string_char(delimiter) != -1)
    g95_internal_error("match_string_constant(): Delimiter not found");

  if (g95_match_substring(&e->ref, 0) != MATCH_NO)
    e->expr_type = EXPR_SUBSTRING;

  *result = e;

  return MATCH_YES;

no_match:
  g95_set_locus(&old_locus);
  return MATCH_NO;
}


/* match_logical_constant().  Match a .true. or .false. */ 

static match match_logical_constant(g95_expr **result) {
static mstring logical_ops[] = {
  minit(".false.", 0), 
  minit(".true.", 1),
  minit(NULL, -1) };

g95_expr *e;
int i, kind;

  i = g95_match_strings(logical_ops);
  if (i == -1) return MATCH_NO;

  kind = get_kind();
  if (kind == -1) return MATCH_ERROR;
  if (kind == -2) kind = g95_default_logical_kind();

  if (g95_validate_kind(BT_LOGICAL, kind) == -1)
    g95_error("Bad kind for logical constant at %C");

  e = g95_get_expr();

  e->expr_type = EXPR_CONSTANT;
  e->value.logical = i;
  e->ts.type = BT_LOGICAL;
  e->ts.kind = kind;
  e->where = *g95_current_locus();

  *result = e;
  return MATCH_YES;
}


/* match_sym_complex_part()-- Match a real or imaginary part of a complex
 * constant that is a symbolic constant. */

static match match_sym_complex_part(g95_expr **result) {
char name[G95_MAX_SYMBOL_LEN+1];
g95_symbol *sym;
g95_expr *e;
match m;

  m = g95_match_name(name);
  if (m != MATCH_YES) return m;

  if (g95_find_symbol(name, NULL, 1, &sym) || sym == NULL) return MATCH_NO;

  if (sym->attr.flavor != FL_PARAMETER) {
    g95_error("Expected PARAMETER symbol in complex constant at %C");
    return MATCH_ERROR;
  }

  if (!g95_numeric_ts(&sym->value->ts)) {
    g95_error("Numeric PARAMETER required in complex constant at %C");
    return MATCH_ERROR;
  }

  if (sym->value->rank != 0) {
    g95_error("Scalar PARAMETER required in complex constant at %C");
    return MATCH_ERROR;
  }

  switch(sym->value->ts.type) {
  case BT_REAL:
    e = g95_copy_expr(sym->value);
    break;

  case BT_COMPLEX:
    e = g95_complex2real(sym->value, sym->value->ts.kind);
    if (e == NULL) goto error;
    break;

  case BT_INTEGER:
    e = g95_int2real(sym->value, g95_default_real_kind());
    if (e == NULL) goto error;
    break;

  default:
    g95_internal_error("g95_match_sym_complex_part(): Bad type");
  }

  *result = e;     /* e is a scalar, real, constant expression */
  return MATCH_YES;

error:
  g95_error("Error converting PARAMETER constant in complex constant at %C");
  return MATCH_ERROR;
}


/* match_const_complex_part()-- Match the real and imaginary parts of
 * a complex number.  This subroutine is essentially
 * match_real_constant() modified in a couple of ways: A sign is
 * always allowed and numbers that would look like an integer to
 * match_real_constant() are automatically created as floating point
 * numbers.  The messiness involved with making sure a decimal point
 * belongs to the number and not a trailing operator is not necessary
 * here either (Hooray!). */

static match match_const_complex_part(g95_expr **result) {
int kind, seen_digits, seen_dp, count;
char *p, c, exp_char, *buffer;
locus old_loc;

  old_loc = *g95_current_locus(); 
  g95_gobble_whitespace();

  seen_dp = 0;
  seen_digits = 0;
  count = 0;
  exp_char = ' ';

  c = g95_next_char();
  if (c == '-' || c == '+') {
    c = g95_next_char();
    count++;
  }

  for(;; c=g95_next_char(), count++) {
    if (c == '.') {
      if (seen_dp) goto no_match;
      seen_dp = 1;
      continue;
    }

    if (isdigit(c)) {
      seen_digits = 1;
      continue;
    }

    break;
  }

  if (!seen_digits || (c != 'd' && c != 'e')) goto done;
  exp_char = c;

/* scan exponent */

  c = g95_next_char();
  count++;

  if (c == '+' || c == '-') {  /* optional sign */
    c = g95_next_char();
    count++;
  }

  if (!isdigit(c)) {
    g95_error("Missing exponent in real number at %C");
    return MATCH_ERROR;
  }

  while(isdigit(c)) {
    c = g95_next_char();
    count++;
  }

done:
  if (!seen_digits) goto no_match;

/* Convert the number */

  g95_set_locus(&old_loc);
  g95_gobble_whitespace();

  buffer = alloca(count+1);
  memset(buffer, '\0', count+1);

  p = buffer;
  while(count>0) {
    c = g95_next_char();
    if (c == 'd') c = 'e';   /* Hack for mpf_init_set_str() */
    *p++ = c;
    count--;
  }

  *p = '\0';

  kind = get_kind();
  if (kind == -1) return MATCH_ERROR;

/* If the number looked like an integer, forget about a kind we may
 * have seen, otherwise validate the kind against real kinds. */

  if (seen_dp == 0 && exp_char == ' ') {
    if (kind == -2) kind = g95_default_integer_kind();

  } else {
    if (exp_char == 'd') {
      if (kind != -2) {
	g95_error("Real number at %C has a 'd' exponent and an explicit kind");
	return MATCH_ERROR;
      }
      kind = g95_default_double_kind();
      
    } else {
      if (kind == -2) kind = g95_default_real_kind();
    }

    if (g95_validate_kind(BT_REAL, kind) == -1) {
      g95_error("Invalid real kind %d at %C", kind);
      return MATCH_ERROR;
    }
  }

  *result = g95_convert_real(buffer, kind);
  return MATCH_YES;

no_match:
  g95_set_locus(&old_loc);
  return MATCH_NO;
}


/* match_complex_part()-- Match a real or imaginary part of a complex number */

static match match_complex_part(g95_expr **result) {
match m;

  m = match_sym_complex_part(result);
  if (m != MATCH_NO) return m;

  return match_const_complex_part(result);
}


/* match_complex_constant()-- Try to match a complex constant */

static match match_complex_constant(g95_expr **result) {
g95_expr *e, *real, *imag;
g95_error_buf old_error;
g95_typespec target;
locus old_loc;
int kind;
match m;

  old_loc = *g95_current_locus();
  real = imag = e = NULL;

  m = g95_match_char('(');
  if (m != MATCH_YES) return m;

  g95_push_error(&old_error);

  m = match_complex_part(&real);
  if (m == MATCH_NO) goto cleanup;

  if (g95_match_char(',') == MATCH_NO) {
    g95_pop_error(&old_error);
    m = MATCH_NO;
    goto cleanup;
  }

/* If m is error, then something was wrong with the real part and we
 * definitely have a complex constant because we've seen the ',' */
  
  if (m == MATCH_ERROR) goto cleanup;
  g95_pop_error(&old_error);

  m = match_complex_part(&imag);
  if (m == MATCH_NO) goto syntax;
  if (m == MATCH_ERROR) goto cleanup;

  m = g95_match_char(')');
  if (m == MATCH_NO) goto syntax;

  if (m == MATCH_ERROR) goto cleanup;

/* Decide on the kind of this complex number */

  kind = g95_kind_max(real, imag);
  target.type = BT_REAL;
  target.kind = kind;

  if (kind != real->ts.kind) g95_convert_type(real, &target, 2);
  if (kind != imag->ts.kind) g95_convert_type(imag, &target, 2);

  e = g95_convert_complex(real, imag, kind);
  e->where = *g95_current_locus();

  g95_free_expr(real);
  g95_free_expr(imag);

  *result = e;
  return MATCH_YES;

syntax:
  g95_error("Syntax error in COMPLEX constant at %C");
  m = MATCH_ERROR;

cleanup:
  g95_free_expr(e);
  g95_free_expr(real);
  g95_free_expr(imag);
  g95_set_locus(&old_loc);

  return m;
}


/* g95_match_literal_constant()-- Match constants in any of several
 * forms.  Returns nonzero for a match, zero for no match. */

match g95_match_literal_constant(g95_expr **result, int signflag) {
match m;

  m = match_complex_constant(result);
  if (m != MATCH_NO) return m;

  m = match_string_constant(result);
  if (m != MATCH_NO) return m;

  m = match_boz_constant(result);
  if (m != MATCH_NO) return m;

  m = match_real_constant(result, signflag);
  if (m != MATCH_NO) return m;

  m = match_integer_constant(result, signflag);
  if (m != MATCH_NO) return m;

  m = match_logical_constant(result);
  if (m != MATCH_NO) return m;

  return MATCH_NO;
}


/* match_actual_arg()-- match a single actual argument value.  An
 * actual argument is usually an expression, but can also be a
 * procedure name.  If the argument is a single name, it is not always
 * possible to tell whether the name is a dummy procedure or not.  We
 * treat these cases by creating an argument that looks like a dummy
 * procedure and fixing things later during resolution. */

static match match_actual_arg(g95_expr **result) {
char name[G95_MAX_SYMBOL_LEN+1];
g95_symbol *sym;
locus where, w;
g95_expr *e;
int c;

  where = *g95_current_locus();

  switch(g95_match_name(name)) {
  case MATCH_ERROR:
    return MATCH_ERROR;

  case MATCH_NO:
    break;

  case MATCH_YES:
    w = *g95_current_locus();
    g95_gobble_whitespace();
    c = g95_next_char();
    g95_set_locus(&w);

    if (c != ',' && c != ')') break;

    if (g95_find_symbol(name, NULL, 1, &sym)) break;
    /* Handle error elsewhere */

    /* Eliminate a couple of common cases where we know we don't have
     * a function argument. */

    if (sym == NULL)
      g95_get_symbol(name, NULL, 0, &sym);
    else {
      if (sym->attr.flavor != FL_PROCEDURE && sym->attr.flavor != FL_UNKNOWN)
	break;

      /* If the symbol is a function with itself as the result and is
       * being defined, then we have a variable */

      if (sym->result == sym &&
	  (g95_current_ns->proc_name == sym ||
	   (g95_current_ns->parent != NULL &&
	    g95_current_ns->parent->proc_name == sym))) break;
    }

    e = g95_get_expr();              /* Leave it unknown for now */
    e->symbol = sym;
    e->expr_type = EXPR_VARIABLE;
    e->ts.type = BT_PROCEDURE;
    e->where = where;

    *result = e;
    return MATCH_YES;
  }

  g95_set_locus(&where);
  return g95_match_expr(result);
}


/* match_keyword_arg()-- Match a keyword argument. */

match match_keyword_arg(g95_actual_arglist *actual, g95_actual_arglist *base) {
char name[G95_MAX_SYMBOL_LEN+1];
g95_actual_arglist *a;
locus name_locus;
match m;

  name_locus = *g95_current_locus();
  m = g95_match_name(name);

  if (m != MATCH_YES) goto cleanup;
  if (g95_match_char('=') != MATCH_YES) goto cleanup;

  m = match_actual_arg(&actual->expr);
  if (m != MATCH_YES) goto cleanup;

  /* Make sure this name has not appeared yet */

  if (name[0] != '\0') {
    for(a=base; a; a=a->next)
      if (strcmp(a->name, name) == 0) {
	g95_error("Keyword '%s' at %C has already appeared in the current "
		  "argument list", name);
	return MATCH_ERROR;;
      }
  }

  strcpy(actual->name, name);
  return MATCH_YES;

cleanup:
  g95_set_locus(&name_locus);
  return MATCH_NO;
}


/* g95_match_actual_arglist()-- Matches an actual argument list of a
 * function or subroutine, from the opening parenthesis to the closing
 * parenthesis.  The argument list is assumed to allow keyword
 * arguments because we don't know if the symbol associated with the
 * procedure has an implicit interface or not.  We make sure keywords
 * are unique. */

match g95_match_actual_arglist(int sub_flag, g95_actual_arglist **argp) {
g95_actual_arglist *head, *tail;
int label, seen_keyword;
locus old_loc;
match m;

  *argp = tail = NULL;
  old_loc = *g95_current_locus();

  seen_keyword = 0;

  if (g95_match_char('(') == MATCH_NO)
    return (sub_flag) ? MATCH_YES : MATCH_NO;

  if (g95_match_char(')') == MATCH_YES) return MATCH_YES;
  head = NULL;

  for(;;) {
    if (head == NULL)
      head = tail = g95_get_actual_arglist();
    else {
      tail->next = g95_get_actual_arglist();
      tail = tail->next;
    }

    if (sub_flag && g95_match_char('*') == MATCH_YES) {
      m = g95_match_st_label(&label);
      if (m == MATCH_NO) g95_error("Expected alternate return label at %C");
      if (m != MATCH_YES) goto cleanup;

      tail->label = label;
      goto next;
    }

    /* After the first keyword argument is seen, the following
     * arguments must also have keywords. */

    if (seen_keyword) {
      m = match_keyword_arg(tail, head);

      if (m == MATCH_ERROR) goto cleanup;
      if (m == MATCH_NO) {
	g95_error("Missing keyword name in actual argument list at %C");
	goto cleanup;
      }

    } else {   /* See if we have the first keyword argument */

      m = match_keyword_arg(tail, head);
      if (m == MATCH_YES) seen_keyword = 1;
      if (m == MATCH_ERROR) goto cleanup;

      if (m == MATCH_NO) {  /* Try for a non-keyword argument */
	m = match_actual_arg(&tail->expr);
	if (m == MATCH_ERROR) goto cleanup;
	if (m == MATCH_NO) goto syntax;
      }
    }

  next:
    if (g95_match_char(')') == MATCH_YES) break;
    if (g95_match_char(',') != MATCH_YES) goto syntax;
  }

  *argp = head;
  return MATCH_YES;

syntax:
  g95_error("Syntax error in argument list at %C");

cleanup:
  g95_free_actual_arglist(head);
  g95_set_locus(&old_loc);

  return MATCH_ERROR;
}


/* extend_ref()-- Used by match_varspec() to extend the reference list
 * by one element. */

static g95_ref *extend_ref(g95_expr *primary, g95_ref *tail) {

  if (primary->ref == NULL)
    primary->ref = tail = g95_get_ref();
  else {
    tail->next = g95_get_ref();
    tail = tail->next;
  }

  return tail;
}


/* g95_match_substring()-- Match a substring reference */

match g95_match_substring(g95_ref **result, int init) {
g95_expr *start, *end;
locus old_loc;
g95_ref *ref;
match m;

  start = NULL;
  end = NULL;

  old_loc = *g95_current_locus();

  m = g95_match_char('(');
  if (m != MATCH_YES) return MATCH_NO;

  if (g95_match_char(':') != MATCH_YES) {
    if (init)
      m = g95_match_init_expr(&start);
    else
      m = g95_match_expr(&start);

    if (m != MATCH_YES) {
      m = MATCH_NO;
      goto cleanup;
    }

    m = g95_match_char(':');
    if (m != MATCH_YES) goto cleanup;
  }

  if (g95_match_char(')') != MATCH_YES) {
    if (init)
      m = g95_match_init_expr(&end);
    else
      m = g95_match_expr(&end);

    if (m == MATCH_NO) goto syntax;
    if (m == MATCH_ERROR) goto cleanup;

    m = g95_match_char(')');
    if (m == MATCH_NO) goto syntax;
  }

/* Optimize away the (:) reference */

  if (start == NULL && end == NULL)
    ref = NULL;
  else {
    ref = g95_get_ref();

    ref->type = REF_SUBSTRING;
    ref->start = start;
    ref->end = end;
  }

  *result = ref;
  return MATCH_YES;

syntax:
  g95_error("Syntax error in SUBSTRING specification at %C");
  m = MATCH_ERROR;

cleanup:
  g95_free_expr(start);
  g95_free_expr(end);

  g95_set_locus(&old_loc);
  return m;
}


/* match_varspec()-- Match any additional specifications associated
 * with the current variable like member references or substrings. */

static match match_varspec(g95_expr *primary, int equiv_flag) {
char name[G95_MAX_SYMBOL_LEN+1];
g95_ref *substring, *tail;
g95_component *component;
g95_symbol *sym;
match m;

  tail = NULL;

  if (primary->symbol->attr.dimension ||
      (equiv_flag && g95_peek_char() == '(')) {

    tail = extend_ref(primary, tail);
    tail->type = REF_ARRAY;

    m = g95_match_array_ref(&tail->ar, primary->symbol->as, equiv_flag);
    if (m != MATCH_YES) return m;
  }

  sym = primary->symbol;
  primary->ts = sym->ts;

  if (sym->ts.type != BT_DERIVED ||
      g95_match_char('%') != MATCH_YES) goto check_substring;

  sym = sym->ts.derived;

  for(;;) {
    m = g95_match_name(name);
    if (m == MATCH_NO) g95_error("Expected structure component name at %C");
    if (m != MATCH_YES) return MATCH_ERROR;

    component = g95_find_component(sym, name);
    if (component == NULL) return MATCH_ERROR;

    tail = extend_ref(primary, tail);
    tail->type = REF_COMPONENT;

    tail->component = component;
    tail->sym = sym;

    primary->ts = component->ts;

    if (component->as != NULL) {
      tail = extend_ref(primary, tail);
      tail->type = REF_ARRAY;

      m = g95_match_array_ref(&tail->ar, component->as, equiv_flag);
      if (m != MATCH_YES) return m;
    }

    if (component->ts.type != BT_DERIVED || g95_match_char('%') != MATCH_YES)
      break;

    sym = component->ts.derived;
  }

check_substring:
  if (primary->ts.type == BT_CHARACTER) {
    switch(g95_match_substring(&substring, equiv_flag)) {
    case MATCH_YES:
      if (tail == NULL) 
	primary->ref = substring;
      else
	tail->next = substring;

      break;

    case MATCH_NO:
      break;

    case MATCH_ERROR:
      return MATCH_ERROR;
    }
  }

  return MATCH_YES;
}


/* g95_variable_attr()-- Given an expression that is a variable,
 * figure out what the ultimate variable's type and attribute is,
 * traversing the reference structures if necessary.
 *
 * This subroutine is trickier than it looks.  We start at the base
 * symbol and store the attribute.  Component references load a
 * completely new attribute.
 *
 * If the new attribute is an array, then the next element is scanned.
 * If the next element is an array reference, then the array bit is
 * cleared.  If the next element is *not* an array reference, then the
 * overall expression is an array expression.
 *
 * We can have at most one full array reference. */

symbol_attribute g95_variable_attr(g95_expr *expr, g95_typespec *ts) {
symbol_attribute attr;
int add_array;
g95_ref *ref;

  add_array = 0;

  if (expr->expr_type != EXPR_VARIABLE)
    g95_internal_error("g95_variable_attr(): Expression isn't a variable");

  ref = expr->ref;
  attr = expr->symbol->attr;
  if (ts != NULL && expr->ts.type == BT_UNKNOWN) *ts = expr->symbol->ts;

new_attr:
  if (attr.dimension && ref != NULL) {
    if (ref->type != REF_ARRAY)
      add_array = 1;
    else {
      attr.dimension = 0;
      ref = ref->next;
    }
  }

  for(; ref; ref=ref->next)
    switch(ref->type) {
    case REF_ARRAY:
      attr.dimension = 0;
      break;

    case REF_COMPONENT:
      g95_get_component_attr(&attr, ref->component);
      if (ts != NULL) *ts = ref->component->ts;
      ref = ref->next;

      goto new_attr;

    case REF_SUBSTRING:
      break;
    }

  if (add_array) attr.dimension = 1;

  return attr;
}



/* match_structure_constructor()-- Match a structure constructor.  The
 * initial symbol has already been seen. */

static match match_structure_constructor(g95_symbol *sym, g95_expr **result) {
g95_constructor *head, *tail;
g95_component *comp;
g95_expr *e;
locus where;
match m;

  head = tail = NULL;

  if (g95_match_char('(') != MATCH_YES) goto syntax;

  where = *g95_current_locus();

  g95_find_component(sym, NULL);

  for(comp=sym->components; comp; comp=comp->next) {
    if (head == NULL)
      tail = head = g95_get_constructor();
    else {
      tail->next = g95_get_constructor();
      tail = tail->next;
    }
	
    m = g95_match_expr(&tail->expr);
    if (m == MATCH_NO) goto syntax;
    if (m == MATCH_ERROR) goto cleanup;

    if (g95_match_char(',') == MATCH_YES) {
      if (comp->next == NULL) {
	g95_error("Too many components in structure constructor at %C");
	goto cleanup;
      }

      continue;
    }

    break;
  }

  if (g95_match_char(')') != MATCH_YES) goto syntax;

  if (comp->next != NULL) {
    g95_error("Too few components in structure constructor at %C");
    goto cleanup;
  }

  e = g95_get_expr();

  e->expr_type = EXPR_STRUCTURE;

  e->ts.type = BT_DERIVED;
  e->ts.derived = sym;
  e->where = where;

  e->value.constructor = head;

  *result = e;
  return MATCH_YES;  

syntax:
  g95_error("Syntax error in structure constructor at %C");

cleanup:
  g95_free_constructor(head);
  return MATCH_ERROR;
}


/* g95_match_rvalue()-- Matches a variable name followed by anything
 * that might follow it-- array reference, argument list of a
 * function, etc. */

match g95_match_rvalue(g95_expr **result) {
g95_actual_arglist *actual_arglist;
char name[G95_MAX_SYMBOL_LEN+1];
g95_state_data *st;
g95_symbol *sym;
locus where;
g95_expr *e;
match m;

  m = g95_match_name(name); 
  if (m != MATCH_YES) return m;

  if (g95_get_symbol(name, NULL, 1, &sym)) return MATCH_ERROR;

  e = NULL;
  where = *g95_current_locus();

  if (sym->attr.function && sym->result == sym &&
      (g95_current_ns->proc_name == sym ||
       (g95_current_ns->parent != NULL &&
	g95_current_ns->parent->proc_name == sym))) goto variable;

  if (sym->attr.function || sym->attr.external || sym->attr.intrinsic)
    goto function0;

  if (sym->attr.generic) goto generic_function;

  switch(sym->attr.flavor) {
  case FL_VARIABLE:
  variable:
    e = g95_get_expr();

    e->expr_type = EXPR_VARIABLE;
    e->symbol = sym;
    m = match_varspec(e, 0);
    break;

  case FL_PARAMETER:
    e = g95_copy_expr(sym->value);
    e->symbol = sym;
    if (sym->as != NULL) e->rank = sym->as->rank;
    m = match_varspec(e, 0);
    break;

  case FL_DERIVED:
    sym = g95_use_derived(sym);
    if (sym == NULL)
      m = MATCH_ERROR;
    else {
      m = match_structure_constructor(sym, &e);
      if (m == MATCH_YES) e->symbol = sym;
    }

    break;

/* If we're here, then the name is known to be the name of a
 * procedure, yet it is not sure to be the name of a function. */

  case FL_PROCEDURE:
    if (sym->attr.subroutine) {
      g95_error("Unexpected use of subroutine name '%s' at %C", sym->name);
      m = MATCH_ERROR;
      break;
    }

/* At this point, the name has to be a non-statement function.  If the
 * name is the same as the current function being compiled, then we
 * have a variable reference (to the function result) if the name is
 * non-recursive. */

    st = g95_enclosing_unit(NULL);

    if (st != NULL && st->state == COMP_FUNCTION && st->sym == sym &&
	!sym->attr.recursive) {
      e = g95_get_expr();
      e->symbol = sym;
      e->expr_type = EXPR_VARIABLE;
      
      m = match_varspec(e, 0);
      break;
    }

    /* Match a function reference */

  function0:
    m = g95_match_actual_arglist(0, &actual_arglist);
    if (m == MATCH_NO) {
      if (sym->attr.proc == PROC_ST_FUNCTION)
	g95_error("Statement function '%s' requires argument list at %C",
		  sym->name);
      else
	g95_error("Function '%s' requires an argument list at %C",
		  sym->name);

      m = MATCH_ERROR;
      break;
    }

    if (m != MATCH_YES) {
      m = MATCH_ERROR;
      break;
    }

    g95_get_symbol(name, NULL, 0, &sym);   /* Can't fail */

    e = g95_get_expr();
    e->symbol = sym;
    e->expr_type = EXPR_FUNCTION;
    e->value.function.actual = actual_arglist;
    e->where = *g95_current_locus();

    if (sym->as != NULL) e->rank = sym->as->rank;

    if (!sym->attr.function && g95_add_function(&sym->attr, NULL) == FAILURE) {
      m = MATCH_ERROR;
      break;
    }

    m = MATCH_YES;
    break;

  case FL_UNKNOWN:

/* If the symbol has a dimension attribute, the expression is a variable */

    if (sym->attr.dimension) {
      if (g95_add_flavor(&sym->attr, FL_VARIABLE, NULL) == FAILURE) {
	m = MATCH_ERROR;
	break;
      }

      e = g95_get_expr();
      e->symbol = sym;
      e->expr_type = EXPR_VARIABLE;
      m = match_varspec(e, 0);
      break;
    }

/* Name is not an array, so we peek to see if a '(' implies a function
 * call or a substring reference.  Otherwise the variable is just a
 * scalar. */

    g95_gobble_whitespace();
    if (g95_peek_char() != '(') {   /* Assume a scalar variable */
      e = g95_get_expr();
      e->symbol = sym;
      e->expr_type = EXPR_VARIABLE;

      if (g95_add_flavor(&sym->attr, FL_VARIABLE, NULL) == FAILURE) {
	m = MATCH_ERROR;
	break;
      }

      if (sym->ts.type == BT_UNKNOWN &&
	  g95_set_default_type(sym, 1, NULL) == FAILURE) {
	m = MATCH_ERROR;
	break;
      }

      e->ts = sym->ts;
      m = match_varspec(e, 0);
      break;
    }

/* See if this could possibly be a substring reference of a name that
 * we're not sure is a variable yet. */

    e = g95_get_expr();
    e->symbol = sym;

    if ((sym->ts.type == BT_UNKNOWN || sym->ts.type == BT_CHARACTER) &&
	g95_match_substring(&e->ref, 0) == MATCH_YES) {

      e->expr_type = EXPR_VARIABLE;

      if (sym->attr.flavor != FL_VARIABLE &&
	  g95_add_flavor(&sym->attr, FL_VARIABLE, NULL) == FAILURE) {
	m = MATCH_ERROR;
	break;
      }

      if (sym->ts.type == BT_UNKNOWN &&
	  g95_set_default_type(sym, 1, NULL) == FAILURE) {
	m = MATCH_ERROR;
	break;
      }

      e->ts = sym->ts;
      m = MATCH_YES;
      break;
    }

/* Give up, assume we have a function */

    g95_get_symbol(name, NULL, 0, &sym);   /* Can't fail */
    e->expr_type = EXPR_FUNCTION;

    if (!sym->attr.function && g95_add_function(&sym->attr, NULL) == FAILURE) {
      m = MATCH_ERROR;
      break;
    }

    m = g95_match_actual_arglist(0, &e->value.function.actual);
    if (m == MATCH_NO)
      g95_error("Missing argument list in function '%s' at %C", sym->name);

    if (m != MATCH_YES) {
      m = MATCH_ERROR;
      break;
    }

    /* If our new function returns a character, array or structure
     * type, it might have subsequent references. */

    m = match_varspec(e, 0);
    if (m == MATCH_NO) m = MATCH_YES;

    break;

  generic_function:
    g95_get_symbol(name, NULL, 0, &sym);   /* Can't fail */

    e = g95_get_expr();
    e->symbol = sym;
    e->expr_type = EXPR_FUNCTION;

    m = g95_match_actual_arglist(0, &e->value.function.actual);
    break;

  default:
    g95_error("Symbol at %C is not appropriate for an expression");
    return MATCH_ERROR;
  }

  if (m == MATCH_YES) {
    e->where = where;
    *result = e;
  } else
    g95_free_expr(e);

  return m;
}


/* g95_match_variable()-- Match a variable, ie something that can be
 * assigned to.  This starts as a symbol, can be a structure component
 * or an array reference.  It can be a function if the function
 * doesn't have a separate RESULT variable.  If the symbol has not
 * been previously seen, we assume it is a variable. */

match g95_match_variable(g95_expr **result, int equiv_flag) {
g95_symbol *sym;
g95_expr *expr;
locus where;
match m;

  m = g95_match_symbol(&sym);
  if (m != MATCH_YES) return m;
  where = *g95_current_locus();

  switch(sym->attr.flavor) {
  case FL_VARIABLE:
    break;

  case FL_UNKNOWN:
    if (g95_add_flavor(&sym->attr, FL_VARIABLE, NULL) == FAILURE)
      return MATCH_ERROR;

    break;

  case FL_PROCEDURE:  /* Check for a nonrecursive function result */
    if (sym->attr.function && sym->result == sym) {

      /* If a function results is a derived type, then the derived
       * type may still have to be resolved. */

      if (sym->ts.type == BT_DERIVED &&
	  g95_use_derived(sym->ts.derived) == NULL) return MATCH_ERROR;

      break;
    }

    /* Fall through to error */

  default:
    g95_error("Expected VARIABLE at %C");
    return MATCH_ERROR;
  }

  expr = g95_get_expr();

  expr->expr_type = EXPR_VARIABLE;
  expr->symbol = sym;
  expr->ts = sym->ts;
  expr->where = where;

/* Now see if we have to do more */

  m = match_varspec(expr, equiv_flag);
  if (m != MATCH_YES) {
    g95_free_expr(expr);
    return m;
  }

  *result = expr;
  return MATCH_YES;
}
