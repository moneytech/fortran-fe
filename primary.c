/* Primary expression subroutines
   Copyright (C) 2000, 2001, 2002 Free Software Foundation, Inc.
   Contributed by Andy Vaught

This file is part of G95.

G95 is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

G95 is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with G95; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */     
     
/* primary.c-- Match primary expressions */        
        
#include <ctype.h>
#include <string.h>
#include <stdlib.h>
#include "g95.h"
    
typedef enum {          
  RVAL_VARIABLE, RVAL_FUNCTION, RVAL_SUBROUTINE, 
  RVAL_DERIVED, RVAL_UNKNOWN, RVAL_ERROR        
} rvalue_type;       
       
       
       
       
/* match_substring()-- Match a substring reference associated with a
 * constant string. */      
      
static match match_substring(g95_charlen *clen, g95_ref **r) {         
g95_expr *st, *e;  
locus oldl;         
g95_ref *ref;     
match v;     
     
  st = NULL;  
  e = NULL;        
        
  oldl = *g95_current_locus();         
         
  v = g95_match_char('(');    
  if (v != MATCH_YES) return MATCH_NO;         
         
  if (g95_match_char(':') != MATCH_YES) {      
    v = g95_match_expr(&st);         
    if (v != MATCH_YES) {          
      v = MATCH_NO; 
      goto cleanup;       
    }      
      
    v = g95_match_char(':');   
    if (v != MATCH_YES) goto cleanup;        
  }          
          
  if (g95_match_char(')') != MATCH_YES) {
    v = g95_match_expr(&e);        
        
    if (v == MATCH_NO) goto syntax;  
    if (v == MATCH_ERROR) goto cleanup; 
 
    v = g95_match_char(')');   
    if (v == MATCH_NO) goto syntax;      
  }         
         
/* Optimize away the (:) reference */  
  
  if (st == NULL && e == NULL)          
    ref = NULL; 
  else {       
    ref = g95_get_ref();

    ref->type = REF_SUBSTRING;    
    ref->u.ss.start = st;          
    ref->u.ss.end = e;          
    ref->u.ss.length = clen;  
  }    
    
  *r = ref;       
  return MATCH_YES;    
    
syntax:       
  g95_error("Syntax error in SUBSTRING specification at %C");
  v = MATCH_ERROR;  
  
cleanup:         
  g95_free_expr(st);  
  g95_free_expr(e);

  g95_set_locus(&oldl);          
  return v;  
}          
          
          
  
  
/* match_kind_param()-- Matches a kind-parameter expression, which is
 * either a named symbolic constant or a nonnegative integer constant.
 * If successful, sets the kind value to the correct integer.  */      
      
static match match_kind_param(int *k) {     
char nm[G95_MAX_SYMBOL_LEN+1];   
g95_symbol *symb;  
char *b;
match o;         
         
  o = g95_match_small_literal_int(k);          
  if (o != MATCH_NO) return o;      
      
  o = g95_match_name(nm);  
  if (o != MATCH_YES) return o;          
          
  if (g95_find_symbol(nm, NULL, 1, &symb)) return MATCH_ERROR; 
 
  if (symb == NULL) return MATCH_NO;         
         
  if (symb->attr.flavor != FL_PARAMETER) return MATCH_NO;

  b = g95_extract_int(symb->value, k);       
  if (b != NULL) return MATCH_NO;     
     
  if (*k < 0) return MATCH_NO;

  return MATCH_YES;  
}     
     
     
         
         
/* check_digit()-- Given a character and a radix, see if the character
 * is a valid digit in that radix. */   
   
static int check_digit(int m, int radix) {       
int s;

  switch(radix) {        
  case 2:
    s = ('0' <= m && m <= '1');     
    break; 
 
  case 8:         
    s = ('0' <= m && m <= '7');      
    break;  
  
  case 10:
    s = ('0' <= m && m <= '9');         
    break;  
  
  case 16:        
    s = ('0' <= m && m <= '9') || ('a' <= m && m <= 'f');      
    break;      
      
  default:   
    g95_internal_error("check_digit(): bad radix");       
  }     
     
  return s;    
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
int f;

  f = g95_next_char_literal(1);  
  
  if (f == '\n') return -2; 
 
  if (f == '\\') {   
    old_locus = *g95_current_locus();        
        
    switch(g95_next_char_literal(1)) {         
    case 'a':  f = '\a'; break;         
    case 'b':  f = '\b'; break;     
    case 't':  f = '\t'; break;   
    case 'f':  f = '\f'; break;      
    case 'n':  f = '\n'; break;   
    case 'r':  f = '\r'; break;      
    case 'v':  f = '\v'; break;       
    case '\\': f = '\\'; break;     
     
    default:     /* Unknown backslash codes are simply not expanded */    
      g95_set_locus(&old_locus);       
      break;      
    }       
  }        
        
  if (f != delimiter) return f; 
 
  old_locus = *g95_current_locus(); 
  f = g95_next_char_literal(1);    
    
  if (f == delimiter) return f;     
  g95_set_locus(&old_locus);  
  
  return -1;         
}      
      
      
          
          
/* match_sym_complex_part()-- Match a real or imaginary part of a complex
 * constant that is a symbolic constant. */     
     
static match match_sym_complex_part(g95_expr **result) {        
char nam[G95_MAX_SYMBOL_LEN+1];       
g95_symbol *symb;        
g95_expr *p;         
match w;          
          
  w = g95_match_name(nam); 
  if (w != MATCH_YES) return w;    
    
  if (g95_find_symbol(nam, NULL, 1, &symb) || symb == NULL) return MATCH_NO;     
     
  if (symb->attr.flavor != FL_PARAMETER) {   
    g95_error("Expected PARAMETER symbol in complex constant at %C");       
    return MATCH_ERROR;         
  }        
        
  if (!g95_numeric_ts(&symb->value->ts)) {    
    g95_error("Numeric PARAMETER required in complex constant at %C");    
    return MATCH_ERROR;         
  }      
      
  if (symb->value->rank != 0) {     
    g95_error("Scalar PARAMETER required in complex constant at %C");          
    return MATCH_ERROR;        
  }    
    
  switch(symb->value->ts.type) {      
  case BT_REAL:  
    p = g95_copy_expr(symb->value);    
    break;

  case BT_COMPLEX:         
    p = g95_complex2real(symb->value, symb->value->ts.kind);          
    if (p == NULL) goto error;          
    break; 
 
  case BT_INTEGER:    
    p = g95_int2real(symb->value, g95_default_real_kind());       
    if (p == NULL) goto error;    
    break;  
  
  default:      
    g95_internal_error("g95_match_sym_complex_part(): Bad type");     
  }    
    
  *result = p;     /* e is a scalar, real, constant expression */       
  return MATCH_YES;

error:     
  g95_error("Error converting PARAMETER constant in complex constant at %C");   
  return MATCH_ERROR; 
}     
     
     
 
 
/* get_kind()-- Gets a trailing kind-specification for non-character
 * variables.  Normally returns the integer kind value or
 *   -1   if an error was generated
 *   -2   if no kind was found
 */       
       
static int get_kind(void) {          
int kind;       
match j;          
          
  if (g95_match_char('_') != MATCH_YES) return -2;   
   
  j = match_kind_param(&kind);     
  if (j == MATCH_NO) g95_error("Missing kind-parameter at %C");  
  
  return (j == MATCH_YES) ? kind : -1;      
}          
          
          
          
          
/* match_digits()-- Match the digit string part of an integer.
 * If the buffer is NULL, we just count characters for the second
 * pass.  Returns the number of characters matched, -1 for no match. */   
   
static int match_digits(int signflag, int radix, char *b) {         
locus old;        
int l, u;      
      
  l = 0;         
  u = g95_next_char();          
          
  if (signflag && (u == '+' || u == '-')) {       
    if (b != NULL) *b++ = u;        
    u = g95_next_char();          
    l++;          
  }      
      
  if (!check_digit(u, radix)) return -1;

  l++;   
  if (b != NULL) *b++ = u;   
   
  for(;;) {  
    old = *g95_current_locus();    
    u = g95_next_char();         
         
    if (!check_digit(u, radix)) break;         
         
    if (b != NULL) *b++ = u;      
    l++; 
  }      
      
  g95_set_locus(&old);   
   
  return l;          
}    
    
    
        
        
/* match_real_constant()-- Match a real constant of some sort. */

static match match_real_constant(g95_expr **res, int signflag) { 
int k0, o, count, seen_dp, seen_digits, exp_char; 
locus oldl, temp_loc;          
char *b, *buffer; 
g95_expr *k;        
        
  oldl = *g95_current_locus();  
  g95_gobble_whitespace();          
          
  k = NULL;          
  buffer = NULL;  
  
  count = 0;  
  seen_dp = 0;      
  seen_digits = 0;    
  exp_char = ' ';  
  
  o = g95_next_char();          
  if (signflag && (o == '+' || o == '-')) {        
    o = g95_next_char();        
    count++;        
  }

/* Scan significand */   
   
  for(;; o=g95_next_char(), count++) {    
    if (o == '.') {    
      if (seen_dp) goto done;        
        
      /* Check to see if "." goes with a following operator like ".eq." */  
  
      temp_loc = *g95_current_locus();          
      o = g95_next_char(); 
 
      if (o == 'e' || o == 'd' || o == 'q') {
	o = g95_next_char();          
	if (o == '.') goto done;   /* Operator named .e. or .d. */          
      }     
     
      if (isalpha(o)) goto done;   /* Distinguish 1.e9 from 1.eq.2 */  
  
      g95_set_locus(&temp_loc);          
      seen_dp = 1;       
      continue;         
    }  
  
    if (isdigit(o)) {      
      seen_digits = 1;     
      continue;          
    }  
  
    break;        
  }  
  
  if (!seen_digits || (o != 'e' && o != 'd' && o != 'q')) goto done;          
  exp_char = o;          
          
/* scan exponent */  
  
  o = g95_next_char();      
  count++;

  if (o == '+' || o == '-') {  /* optional sign */      
    o = g95_next_char();
    count++;    
  }     
     
  if (!isdigit(o)) {    
    if (!seen_digits) {          
      g95_set_locus(&oldl);      
      return MATCH_NO;   /* ".e" can be something else */          
    }      
      
    g95_error("Missing exponent in real number at %C");         
    return MATCH_ERROR;
  }          
          
  while(isdigit(o)) {         
    o = g95_next_char();      
    count++;   
  }          
          
/* See what we've got */ 
 
done:  
  if (!seen_digits || (!seen_dp && exp_char == ' ')) {       
    g95_set_locus(&oldl);          
    return MATCH_NO; 
  }        
        
/* Convert the number */

  g95_set_locus(&oldl);          
  g95_gobble_whitespace();          
          
  buffer = alloca(count+1);        
  memset(buffer, '\0', count+1);    
    
  b = buffer;  
  while(count>0) {  
    *b = g95_next_char();     
    if (*b == 'd' || *b == 'q') *b = 'e';   /* Hack for mpf_init_set_str() */        
    b++;  
    count--;        
  }

  k0 = get_kind();
  if (k0 == -1) goto cleanup;       
       
  switch(exp_char) {     
  case 'd':   
    if (k0 != -2) {
      g95_error("Real number at %C has a 'd' exponent and an explicit kind");       
      goto cleanup;     
    }     
    k0 = g95_default_double_kind();    
    break;          
          
  case 'q': 
    if (k0 != -2) {        
      g95_error("Real number at %C has a 'q' exponent and an explicit kind");        
      goto cleanup;       
    }    
    k0 = g95_option.q_kind;    
    break;     
     
  default:  
    if (k0 == -2)  k0 = g95_default_real_kind();   
  
    if (g95_validate_kind(BT_REAL, k0) == -1) { 
      g95_error("Invalid real kind %d at %C", k0);  
      goto cleanup;       
    }  
  }        
        
  k = g95_convert_real(buffer, k0, g95_current_locus());          
          
  switch(g95_range_check(k)) {   
    case ARITH_OK: break;  
    case ARITH_OVERFLOW:   
      g95_error("Real constant overflows its kind at %C");    
      goto cleanup; 
 
    case ARITH_UNDERFLOW:          
      g95_error("Real constant underflows its kind at %C");  
      goto cleanup;    
    
    default:        
      g95_internal_error("g95_range_check() returned bad value");     
  }

  *res = k;       
  return MATCH_YES;   
   
cleanup:  
  g95_free_expr(k); 
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
          
static match match_const_complex_part(g95_expr **rslt) { 
int k, seen_digits, seen_dp, cnt;   
char *y, l, exp_char, *buf;         
locus old_loc;        
        
  old_loc = *g95_current_locus();           
  g95_gobble_whitespace();  
  
  seen_dp = 0;       
  seen_digits = 0;         
  cnt = 0;
  exp_char = ' ';          
          
  l = g95_next_char();   
  if (l == '-' || l == '+') {          
    l = g95_next_char();   
    cnt++;     
  }      
      
  for(;; l=g95_next_char(), cnt++) {        
    if (l == '.') {          
      if (seen_dp) goto no_match;          
      seen_dp = 1;   
      continue;  
    }       
       
    if (isdigit(l)) {       
      seen_digits = 1;     
      continue;    
    }       
       
    break;     
  }         
         
  if (!seen_digits || (l != 'd' && l != 'e')) goto done;
  exp_char = l;       
       
/* scan exponent */   
   
  l = g95_next_char();          
  cnt++;

  if (l == '+' || l == '-') {  /* optional sign */    
    l = g95_next_char(); 
    cnt++;         
  }   
   
  if (!isdigit(l)) {   
    g95_error("Missing exponent in real number at %C");          
    return MATCH_ERROR;          
  }       
       
  while(isdigit(l)) { 
    l = g95_next_char();
    cnt++;        
  }

done:   
  if (!seen_digits) goto no_match;        
        
/* Convert the number */   
   
  g95_set_locus(&old_loc);   
  g95_gobble_whitespace();     
     
  buf = alloca(cnt+1);    
  memset(buf, '\0', cnt+1);

  y = buf;     
  while(cnt>0) {       
    l = g95_next_char();     
    if (l == 'd') l = 'e';   /* Hack for mpf_init_set_str() */    
    *y++ = l;
    cnt--;       
  }          
          
  *y = '\0';          
          
  k = get_kind();   
  if (k == -1) return MATCH_ERROR;   
   
/* If the number looked like an integer, forget about a kind we may
 * have seen, otherwise validate the kind against real kinds. */     
     
  if (seen_dp == 0 && exp_char == ' ') {         
    if (k == -2) k = g95_default_integer_kind();         
         
  } else {       
    if (exp_char == 'd') {
      if (k != -2) {   
	g95_error("Real number at %C has a 'd' exponent and an explicit kind");    
	return MATCH_ERROR;      
      }          
      k = g95_default_double_kind();    
          
    } else { 
      if (k == -2) k = g95_default_real_kind();      
    } 
 
    if (g95_validate_kind(BT_REAL, k) == -1) {          
      g95_error("Invalid real kind %d at %C", k);  
      return MATCH_ERROR;       
    }       
  }    
    
  *rslt = g95_convert_real(buf, k, g95_current_locus());         
  return MATCH_YES;        
        
no_match:          
  g95_set_locus(&old_loc);        
  return MATCH_NO;       
}


      
      
/* ha_symbol()-- Given a name, look for it in parent scopes, stopping
 * when required.  Creates a new symbol in the current namespace if
 * nothing is found. */      
      
static g95_symbol *ha_symbol(char *nam) {       
g95_state_data *comp;  
g95_namespace *names;    
g95_symbol *sym;        
        
  names = g95_current_ns;  
  comp = g95_state_stack;   
   
  for(;;) {     
    if (g95_find_symbol(nam, names, 0, &sym)) {      
      g95_error("Symbol '%s' at %C is ambiguous", nam);      
      return NULL;   
    }         
         
    if (sym != NULL && g95_local_symbol(sym)) break;      
      
    names = names->parent;         
    if (names == NULL) goto unknown;

  loop:         
    comp = comp->previous;     
    switch(comp->state) {          
    case COMP_INTERFACE:        
      goto unknown;  
  
    case COMP_NONE:        case COMP_PROGRAM:    case COMP_MODULE:   
    case COMP_SUBROUTINE:  case COMP_FUNCTION:   case COMP_BLOCK_DATA:
      break;      
      
    case COMP_DERIVED:  case COMP_IF:     case COMP_SELECT:    case COMP_DO:       
    case COMP_FORALL:   case COMP_WHERE:  case COMP_CONTAINS:    
      goto loop;        
    }        
  }    
    
  return sym;          
          
unknown:    
  g95_get_symbol(nam, NULL, &sym);  
  return sym;         
}     
     
     
    
    
/* match_charkind_name()-- Special case of g95_match_name() that
 * matches a parameter kind name before a string constant.  This takes
 * case of the weird but legal case of:
 *             kind_____'string'
 *
 * where kind____ is a parameter. g95_match_name() will happily slurp
 * up all the underscores, which leads to problems.  If we return
 * MATCH_YES, the parse pointer points to the final underscore, which
 * is not part of the name.  We never return MATCH_ERROR-- errors in
 * the name will be detected later. */     
     
static match match_charkind_name(char *nm) {  
locus o;
char n, peek; 
int l;  
  
  g95_gobble_whitespace();          
  n = g95_next_char();     
  if (!isalpha(n)) return MATCH_NO;  
  
  *nm++ = n;      
  l = 1;       
       
  for(;;) {       
    o = *g95_current_locus();  
    n = g95_next_char();  
  
    if (n == '_') {       
      peek = g95_peek_char();          
                
      if (peek == '\'' || peek == '\"') {      
	g95_set_locus(&o);        
	*nm = '\0';      
	return MATCH_YES;   
      } 
    }          
          
    if (!isalnum(n) && n != '_' && g95_option.dollar && n != '$') break;    
    
    *nm++ = n;         
    if (++l > G95_MAX_SYMBOL_LEN) break;         
  }      
      
  return MATCH_NO;  
}       
       
       
        
        
/* match_logical_constant().  Match a .true. or .false. */           
          
static match match_logical_constant(g95_expr **res) {
static mstring logical_ops[] = {   
  minit(".false.", 0),       
  minit(".true.", 1),          
  minit(NULL, -1) };      
      
g95_expr *k;    
int z, knd;          
          
  z = g95_match_strings(logical_ops);   
  if (z == -1) return MATCH_NO;        
        
  knd = get_kind();  
  if (knd == -1) return MATCH_ERROR;         
  if (knd == -2) knd = g95_default_logical_kind();        
        
  if (g95_validate_kind(BT_LOGICAL, knd) == -1)  
    g95_error("Bad kind for logical constant at %C");          
          
  k = g95_get_expr();        
        
  k->type = EXPR_CONSTANT;        
  k->value.logical = z;
  k->ts.type = BT_LOGICAL;    
  k->ts.kind = knd; 
  k->where = *g95_current_locus(); 
 
  *res = k;      
  return MATCH_YES;        
} 
 
 
 
 
/* match_boz_constant()-- Match a binary, octal or hexadecimal
 * constant that can be found in a DATA statement */ 
 
static match match_boz_constant(g95_expr **r) {       
int radix, del, length, k0;   
locus old_loc;
char *b;         
g95_expr *e;    
char *rname;   
   
  old_loc = *g95_current_locus();   
  g95_gobble_whitespace();  
  
  switch (g95_next_char()) {       
  case 'b':  radix = 2;  rname = "binary";       break;          
  case 'o':  radix = 8;  rname = "octal";        break;        
  case 'z':  radix = 16; rname = "hexadecimal";  break;   
  default: goto backup; 
  }     
     
  /* no whitespace allowed here */         
         
  del = g95_next_char();   
  if (del != '\'' && del != '\"') goto backup;     
     
  old_loc = *g95_current_locus();   
     
  length = match_digits(0, radix, NULL);   
  if (length == -1) {  
    g95_error("Empty set of digits in %s constants at %C", rname);    
    return MATCH_ERROR;         
  }         
         
  if (g95_next_char() != del) {          
    g95_error("Illegal character in %s constant at %C.", rname);         
    return MATCH_ERROR;        
  }  
  
  g95_set_locus(&old_loc);       
       
  b = alloca(length+1);     
  memset(b, '\0', length+1);  
  
  match_digits(0, radix, b); 
  g95_next_char();  /* Gobble trailing ' */          
          
  k0 = get_kind();          
  if (k0 == -1) return MATCH_ERROR;       
  if (k0 == -2) k0 = g95_default_integer_kind();       
       
  e = g95_convert_integer(b, k0, radix, g95_current_locus()); 
 
  if (g95_range_check(e) != ARITH_OK) {       
    g95_error("Integer too big for its kind at %C");

    g95_free_expr(e); 
    return MATCH_ERROR;  
  }       
       
  *r = e;
  return MATCH_YES; 
 
backup:     
  g95_set_locus(&old_loc);
  return MATCH_NO;        
}  
  
  
  
  
/* match_hollerith_constant()-- Read a Hollerith constant.  The length
 * and the H have already been matched. */          
          
static void match_hollerith_constant(g95_expr *t) {  
int leng;          
char n, *q;          
          
  q = t->value.character.string;    
  leng = t->value.character.length;        
        
  while(leng > 0) {    
    n = g95_next_char_literal(1);          
          
    if (n == '\n') {    
      memset(q, ' ', leng);       
      break;      
    }       
       
    *q++ = n;          
    leng--;   
  }         
}     
     
     
 
 
/* match_complex_part()-- Match a real or imaginary part of a complex number */     
     
static match match_complex_part(g95_expr **result) { 
match i;  
  
  i = match_sym_complex_part(result);
  if (i != MATCH_NO) return i;        
        
  return match_const_complex_part(result);       
}  
  
  
       
       
/* match_complex_constant()-- Try to match a complex constant */ 
 
static match match_complex_constant(g95_expr **r) {  
g95_expr *o, *real, *imag;       
g95_error_buf old_error;       
g95_typespec target;    
locus oldl;          
int k; 
match s;      
      
  oldl = *g95_current_locus();  
  real = imag = o = NULL;        
        
  s = g95_match_char('(');  
  if (s != MATCH_YES) return s;   
   
  g95_push_error(&old_error);     
     
  s = match_complex_part(&real);        
  if (s == MATCH_NO) goto cleanup;   
   
  if (g95_match_char(',') == MATCH_NO) {   
    g95_pop_error(&old_error);       
    s = MATCH_NO;         
    goto cleanup;  
  }       
       
/* If m is error, then something was wrong with the real part and we
 * assume we have a complex constant because we've seen the ','.  An
 * ambiguous case here is the start of an iterator list of some sort.
 * These sort of lists are matched prior to coming here. */        
          
  if (s == MATCH_ERROR) goto cleanup;          
  g95_pop_error(&old_error);   
   
  s = match_complex_part(&imag); 
  if (s == MATCH_NO) goto syntax;          
  if (s == MATCH_ERROR) goto cleanup;          
          
  s = g95_match_char(')');      
  if (s == MATCH_NO) goto syntax; 
 
  if (s == MATCH_ERROR) goto cleanup;    
    
/* Decide on the kind of this complex number */        
        
  k = g95_kind_max(real, imag);          
  target.type = BT_REAL;   
  target.kind = k;  
  
  if (k != real->ts.kind) g95_convert_type(real, &target, 2);
  if (k != imag->ts.kind) g95_convert_type(imag, &target, 2);          
          
  o = g95_convert_complex(real, imag, k); 
  o->where = *g95_current_locus();       
       
  g95_free_expr(real); 
  g95_free_expr(imag);  
  
  *r = o;          
  return MATCH_YES;          
          
syntax: 
  g95_error("Syntax error in COMPLEX constant at %C");     
  s = MATCH_ERROR;        
        
cleanup:     
  g95_free_expr(o);       
  g95_free_expr(real);       
  g95_free_expr(imag);         
  g95_set_locus(&oldl);  
  
  return s; 
}  
  
  
 
 
/* match_substring_rest()-- Match the trailing part of a substring.  A
 * colon is guaranteed to be next.  */

static match match_substring_rest(g95_symbol *sy, locus *loc,        
				  g95_expr *start, g95_expr **result) {    
g95_expr *x, *fin; 
g95_ref *reference;   
match w;          
          
  g95_next_char();       
  fin = NULL;         
         
  g95_gobble_whitespace();       
  if (g95_peek_char() != ')') {          
    w = g95_match_expr(&fin);
    if (w == MATCH_ERROR) goto cleanup; 
    if (w == MATCH_NO) goto syntax;    
  }        
        
  if (g95_match_char(')') != MATCH_YES) goto syntax;    
    
  x = g95_get_expr();          
  x->type = EXPR_VARIABLE;          
  x->symbol = sy;         
  x->where = *loc;

  x->ref = reference = g95_get_ref();          
          
  reference->type = REF_SUBSTRING; 
  reference->u.ss.start = start;     
  reference->u.ss.end = fin;       
  reference->where = *loc;   
   
  *result = x; 
  return MATCH_YES;       
       
syntax:          
   g95_error("Syntax error in substring reference at %C");         
         
cleanup: 
  if (start != NULL) g95_free_expr(start);      
  if (fin   != NULL) g95_free_expr(fin); 
  return MATCH_ERROR;    
}  
  
  


/* match_integer_constant()-- Match an integer (digit string and
 * optional kind). */   
   
static match match_integer_constant(g95_expr **result, int signflag) { 
int len, k0;          
locus old_loc; 
char *buf;     
g95_expr *r;         
         
  old_loc = *g95_current_locus();       
  g95_gobble_whitespace();    
    
  len = match_digits(signflag, 10, NULL);     
  g95_set_locus(&old_loc);    
  if (len == -1) return MATCH_NO; 
 
  buf = alloca(len+1);  
  memset(buf, '\0', len+1);       
       
  g95_gobble_whitespace();         
         
  match_digits(signflag, 10, buf);     
     
  k0 = get_kind();          
  if (k0 == -2) k0 = g95_default_integer_kind();     
  if (k0 == -1) return MATCH_ERROR; 
 
  if (g95_validate_kind(BT_INTEGER, k0) == -1) { 
    g95_error("Integer kind %d at %C not available", k0);         
    return MATCH_ERROR;
  }

  r = g95_convert_integer(buf, k0, 10, g95_current_locus());  
  
  if (g95_range_check(r) != ARITH_OK) {   
    g95_error("Integer too big for its kind at %C");        
        
    g95_free_expr(r);  
    return MATCH_ERROR;    
  }        
        
  *result = r;    
  return MATCH_YES;  
} 
 
 
    
    
/* compiling()-- Return nonzero if we are currently compiling the
 * given symbol.  This is used for determining if a function name is
 * its own result variable. */   
   
static int compiling(g95_symbol *symb) {          
g95_namespace *n;          
g95_symbol *h;

  for(n=g95_current_ns; n; n=n->parent)         
    if (!symb->attr.entry) {     
      if (n->proc_name == symb) return 1;      
    } else {        
      h = NULL;       
      g95_find_symbol(symb->name, n, 0, &h);      
      
      if (h == symb) return 1;     
    } 
 
  return 0;          
}        
        
        
 
 
/* match_string_constant()-- See if the current input matches a
 * character constant.  Lots of contortions have to be done to match
 * the kind parameter which comes before the actual string.  The main
 * consideration is that we don't want to error out too quickly.  For
 * example, we don't actually do any validation of the kinds until we
 * have actually seen a legal delimiter.  Using match_kind_param()
 * generates errors too quickly. */     
     
static match match_string_constant(g95_expr **rslt) {  
char *h, n[G95_MAX_SYMBOL_LEN+1];          
int l, j, kind, length, delimiter;          
locus old_locus, start_locus;      
g95_symbol *symb;    
g95_expr *a;  
char *r;  
match k;          
          
  old_locus = *g95_current_locus();      
      
  g95_gobble_whitespace();      
      
  start_locus = *g95_current_locus();       
       
  j = g95_next_char();       
  if (j == '\'' || j == '"') {          
    kind = g95_default_character_kind();        
    goto got_delim;        
  } 
 
  if (isdigit(j)) {      
    kind = 0;         
         
    while(isdigit(j)) {     
      kind = kind*10 + j - '0';  
      if (kind > 9999999) goto no_match;       
      j = g95_next_char();
    }  
  
  } else {      
    g95_set_locus(&old_locus);      
      
    k = match_charkind_name(n);   
    if (k != MATCH_YES) goto no_match;     
     
    if (g95_find_symbol(n, NULL, 1, &symb) || symb == NULL ||       
	symb->attr.flavor != FL_PARAMETER) goto no_match;       
       
    kind = -1;   
    j = g95_next_char();
  }    
    
  if (j == ' ') {    
    g95_gobble_whitespace();      
    j = g95_next_char();      
  }    
    
  if (j == 'h') {  
    *rslt = g95_char_expr(kind, g95_default_character_kind(), &start_locus);     
    match_hollerith_constant(*rslt);
    return MATCH_YES;          
  }

  if (j != '_') goto no_match;      
      
  g95_gobble_whitespace();
  start_locus = *g95_current_locus();   
   
  j = g95_next_char();   
  if (j != '\'' && j != '"') goto no_match;      
      
  if (kind == -1) { 
    r = g95_extract_int(symb->value, &kind);     
    if (r != NULL) {  
      g95_error(r);
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
  delimiter = j;        
  length = 0;  
  
  for(;;) {   
    j = g95_next_string_char(delimiter);
    if (j == -1) break;     
    if (j == -2) {          
      g95_set_locus(&start_locus);  
      g95_error("Unterminated character constant beginning at %C");    
      return MATCH_ERROR;
    }         
         
    length++;         
  }  
  
  a = g95_char_expr(length, kind, &start_locus); 
  h = a->value.character.string;

  g95_set_locus(&start_locus); 
  g95_next_char();              /* Skip delimiter */   
   
  for(l=0; l<length; l++)    
    *h++ = g95_next_string_char(delimiter);       
       
  *h = '\0';     /* C-style string is for development/debug purposes */          
          
  if (g95_next_string_char(delimiter) != -1)    
    g95_internal_error("match_string_constant(): Delimiter not found");     
     
  if (match_substring(NULL, &a->ref) != MATCH_NO) a->type = EXPR_SUBSTRING;   
   
  *rslt = a;          
  return MATCH_YES;      
      
no_match:   
  g95_set_locus(&old_locus);        
  return MATCH_NO;      
}          
          
          
      
      
/* g95_match_literal_constant()-- Match constants in any of several
 * forms.  Returns nonzero for a match, zero for no match. */

match g95_match_literal_constant(g95_expr **res, int signflag) {   
match r;

  r = match_complex_constant(res);          
  if (r != MATCH_NO) return r;     
     
  r = match_string_constant(res);         
  if (r != MATCH_NO) return r;   
   
  r = match_boz_constant(res);         
  if (r != MATCH_NO) return r;        
        
  r = match_real_constant(res, signflag);        
  if (r != MATCH_NO) return r;  
  
  r = match_integer_constant(res, signflag);        
  if (r != MATCH_NO) return r;          
          
  r = match_logical_constant(res);     
  if (r != MATCH_NO) return r;  
  
  return MATCH_NO;          
}  
  
  
         
         
/* match_function_call()-- Match a function reference */      
      
static match match_function_call(g95_symbol *sy, g95_expr **r) {   
g95_actual_arglist *a;
locus old_loc;        
g95_expr *w;    
match x;          
          
  if (g95_add_function(&sy->attr, NULL) == FAILURE) return MATCH_ERROR; 
 
  old_loc = *g95_current_locus(); 
  x = g95_match_actual_arglist(0, &a);    
    
  switch(x) {        
  case MATCH_YES:    
    w = g95_get_expr();
    w->type = EXPR_FUNCTION;          
    w->symbol = sy;       
    w->where = old_loc;     
     
    w->value.function.actual = a;        
        
    if (sy->result == NULL) sy->result = sy;    
    
    *r = w;          
    break;  
  
  case MATCH_NO:      
    g95_error("Missing actual argument list in function call to '%s' at %C",  
	      sy->name);     
    x = MATCH_ERROR;     
    break; 
 
  case MATCH_ERROR:   
    break;      
  }

  return x;     
}          
          
          
         
         
/* get_rvalue_type()-- Given the name of an rvalue, figure out what
 * kind it is, peeking into host scopes if necessary. */ 
 
static rvalue_type get_rvalue_type(char *name, g95_symbol **r) {
g95_symbol *symb;   
rvalue_type rv; 
 
  *r = symb = ha_symbol(name);      
  if (symb == NULL) return RVAL_ERROR;     
     
  g95_save_symbol_data(symb);      
      
  switch(symb->attr.flavor) {     
  case FL_PROCEDURE:          
    if (symb->attr.subroutine) {       
      rv = RVAL_SUBROUTINE;         
      break;  
    }      
      
    if (symb->attr.proc == PROC_ST_FUNCTION) {         
      rv = RVAL_FUNCTION;      
      break;         
    }   
   
    rv = (symb->attr.function && symb->result == symb && compiling(symb))
      ? RVAL_VARIABLE         
      : RVAL_FUNCTION;          
          
    break; 
 
    /* Fall through */         
         
  default:       
    g95_error("Symbol '%s' at %C is not appropriate for a primary expression",        
	      name);         
    rv = RVAL_ERROR;    
    break;     
     
  case FL_VARIABLE:     
  case FL_PARAMETER:     
    rv = RVAL_VARIABLE;     
    break;          
          
  case FL_UNKNOWN:    
    if (symb->attr.generic || symb->attr.external || symb->attr.intrinsic) {    
      rv = RVAL_FUNCTION;          
      break;    
    }   
   
    if (symb->as != NULL) { 
      rv = RVAL_VARIABLE;         
      break;   
    }     
     
    rv = RVAL_UNKNOWN;          
    break;         
         
  case FL_DERIVED:
    rv = RVAL_DERIVED;
    break;      
  }  
  
  return rv;          
}        
        
        
          
          
/* g95_local_symbol()-- Return nonzero if this symbol is a local
 * symbol in it's namespace and prevents another symbol of the same
 * name from being host associated */ 
 
int g95_local_symbol(g95_symbol *s) {   
symbol_attribute z;  
  
  z = s->attr;     
    
  if ((s->ts.type != BT_UNKNOWN && !z.implicit_type) ||    
      z.proc == PROC_ST_FUNCTION || z.pointer || z.save || z.target ||      
      z.flavor == FL_PARAMETER || s->as != NULL || z.allocatable ||       
      z.in_common || z.data || z.dummy || z.result_var || z.intrinsic ||
      s->namelist != NULL || z.generic || z.flavor == FL_LABEL ||        
      z.equivalenced) return 1; 
 
  if (z.flavor == FL_PROCEDURE && z.if_source == IFSRC_DECL) return 1;

  if (z.proc == PROC_MODULE || z.flavor == FL_DERIVED) return 1;         
         
  return 0;     
} 
 
 
        
        
/* g95_variable_attr()-- Given an expression that is a variable,
 * figure out what the ultimate variable's type and attribute is,
 * traversing the reference structures if necessary.
 *
 * This subroutine is trickier than it looks.  We start at the base
 * symbol and store the attribute.  Component references load a
 * completely new attribute.
 *
 * A couple of rules come into play.  Subobjects of targets are always
 * targets themselves.  If we see a component that goes through a
 * pointer, then the expression must also be a target, since the
 * pointer is associated with something (if it isn't core will soon be
 * dumped).  If we see a full part or section of an array, the
 * expression is also an array.  This means that 'target' might be set
 * when it is not really.
 *
 * We can have at most one full array reference. */          
          
symbol_attribute g95_variable_attr(g95_expr *e, g95_typespec *t) {    
int dimension, pointer, target;   
symbol_attribute attribute;      
g95_ref *ref;          
          
  if (e->type != EXPR_VARIABLE)          
    g95_internal_error("g95_variable_attr(): Expression isn't a variable");          
          
  ref = e->ref;     
  attribute = e->symbol->attr; 
 
  dimension = attribute.dimension;          
  pointer = attribute.pointer;         
         
  target = attribute.target;      
  if (pointer) target = 1;         
         
  if (t != NULL && e->ts.type == BT_UNKNOWN) *t = e->symbol->ts; 
 
  for(; ref; ref=ref->next)         
    switch(ref->type) {          
    case REF_ARRAY:     
      switch(ref->u.ar.type) {    
      case AR_FULL: 
	dimension = 1;
	break;        
        
      case AR_SECTION:
	pointer = 0;
	dimension = 1;      
	break;        
        
      case AR_ELEMENT:     
	pointer = 0;        
	break;     
     
      case AR_UNKNOWN:   
	g95_internal_error("g95_variable_attr(): Bad array reference");        
      }         
         
      break;     
     
    case REF_COMPONENT: 
      g95_get_component_attr(&attribute, ref->u.c.component);     
      if (t != NULL) *t = ref->u.c.component->ts;          
          
      pointer = ref->u.c.component->pointer;        
      if (pointer) target = 1;          
          
      break;     
     
    case REF_SUBSTRING:         
      pointer = 0;       
      break;       
    }

  attribute.dimension = dimension;  
  attribute.pointer = pointer;      
  attribute.target = target;      
      
  return attribute;          
}        
        
        
 
 
/* single_name_arg()-- Given an actual argument that is a single name,
 * figure out what to do with it.  The name can potentially be a dummy
 * procedure that we only find out about later. */   
   
static match single_name_arg(char *name0, g95_expr **r) {         
g95_symbol *s;  
g95_typespec typ;        
int expr_type;     
g95_expr *j;    
    
  g95_clear_ts(&typ);   
  expr_type = 0;      
      
  switch(get_rvalue_type(name0, &s)) {          
  case RVAL_VARIABLE: 
    expr_type = EXPR_VARIABLE;    
    if (s->ts.type != BT_UNKNOWN) typ = s->ts;     
    break;         
         
  case RVAL_FUNCTION:    
  case RVAL_SUBROUTINE:         
    expr_type = EXPR_VARIABLE;       
    typ.type = BT_PROCEDURE;          
    break;        
        
  case RVAL_DERIVED:      
    g95_error("Actual argument '%s' at %C is a derived type name", name0);     
    /* Fall through */         
         
  case RVAL_ERROR:      
    return MATCH_ERROR;    
    
  case RVAL_UNKNOWN:     
    expr_type = EXPR_UNKNOWN;      
    break;     
  }       
       
  j = g95_get_expr();    
  j->symbol = s;  
  j->type = expr_type;  
  j->ts = typ;      
  j->where = *g95_current_locus();

  *r = j;         
  return MATCH_YES;         
}     
     
     
         
         
/* match_actual_arg()-- match a single actual argument.  An expression
 * consisting of a single name is a very special case handled elsewhere */ 
 
static match match_actual_arg(g95_expr **r) {         
char nm[G95_MAX_SYMBOL_LEN+1];    
locus old_loc, f;       
int x;

  old_loc = *g95_current_locus();  
  
  switch(g95_match_name(nm)) {  
  case MATCH_ERROR:         
    return MATCH_ERROR;       
       
  case MATCH_NO:       
    break;       
       
  case MATCH_YES:         
    f = *g95_current_locus();
    g95_gobble_whitespace();          
    x = g95_next_char();  
    g95_set_locus(&f);        
        
    if (x != ',' && x != ')') break;     
     
    return single_name_arg(nm, r); 
  } 
 
  g95_set_locus(&old_loc);    
  return g95_match_expr(r);   
}    
    
    
    
    
/* g95_extend_ref()-- Used by match_varspec() to extend the reference
 * list by one element. */   
   
g95_ref *g95_extend_ref(g95_expr *primary, int type) {    
g95_ref *t;          
          
  if (primary->ref == NULL)        
    primary->ref = t = g95_get_ref();  
  else {
    t = primary->ref;         
    while(t->next != NULL)    
      t = t->next;  
  
    t->next = g95_get_ref();    
    t = t->next;         
  }

  t->type = type; 
  t->where = *g95_current_locus();   
   
  return t; 
}


   
   
/* g95_expr_attr()-- Return the attribute from a general expression */

symbol_attribute g95_expr_attr(g95_expr *c) {   
symbol_attribute atr;        
        
  switch(c->type) {      
  case EXPR_VARIABLE:         
    atr = g95_variable_attr(c, NULL);
    break;  
  
  case EXPR_FUNCTION:
    g95_clear_attr(&atr);        
    atr = c->symbol->result->attr;       
       
    /* NULL() returns pointers.  May have to take care of this here */

    break;         
         
  default:       
    g95_clear_attr(&atr);  
    break;          
  }  
  
  return atr;     
}         
         
         
 
 
/* match_variable_part()-- Match the parts of a variable that come
 * after the initial name. */   
   
static match match_variable_part(g95_symbol *s, g95_expr **rslt,    
				 int array_ok) { 
g95_ref *re;    
g95_expr *g;
match f;         
         
  g = g95_get_expr();
  g->type = EXPR_VARIABLE;      
  g->symbol = s;   
  g->where = *g95_current_locus();     
     
  array_ok |= (s->as != NULL || s->ts.type == BT_CHARACTER);          
          
loop:    
  g95_gobble_whitespace();      
  switch(g95_peek_char()) {      
  case '(':          
    if (!array_ok) {       
      g95_free_expr(g);   
      g95_error("Unexpected array reference at %C");         
      return MATCH_ERROR;        
    }          
          
    array_ok = 1;    /* for substring refs */         
         
    re = g95_extend_ref(g, REF_ARRAY);   
   
    f = g95_match_array_ref(&re->u.ar, 0);     
    if (f == MATCH_ERROR) goto cleanup;    
    if (f == MATCH_YES) goto loop;  
  
    g95_internal_error("match_variable_part(): Bad array match");         
         
  case '%':    
    array_ok = 1;         
    g95_next_char();         
         
    re = g95_extend_ref(g, REF_COMPONENT);         
         
    f = g95_match_name(re->u.c.name);
    if (f == MATCH_ERROR) goto cleanup;      
    if (f == MATCH_YES) goto loop;   
   
    g95_error("Missing component name at %C");         
    goto cleanup;       
       
  default:         
    break;     
  }   
   
  switch(s->attr.flavor) { 
  case FL_VARIABLE:      
  case FL_PARAMETER: 
    break;     
     
  case FL_PROCEDURE:    
    if (s->attr.function && s->result == s) break;   
   
    /* Fall through to error */        
        
  default:      
    if (g95_add_flavor(&s->attr, FL_VARIABLE, NULL) == FAILURE)       
      goto cleanup;    
  }     
     
  *rslt = g;        
  return MATCH_YES;      
      
cleanup:          
  g95_free_expr(g);
  return MATCH_ERROR;  
}          
          
          
         
         
/* match_keyword_arg()-- Match a keyword argument. */      
      
static match match_keyword_arg(g95_actual_arglist *real,       
			       g95_actual_arglist *base) { 
char name0[G95_MAX_SYMBOL_LEN+1]; 
g95_actual_arglist *z;          
locus name_locus;          
match i;  
  
  name_locus = *g95_current_locus();
  i = g95_match_name(name0); 
 
  if (i != MATCH_YES) goto cleanup;        
  if (g95_match_char('=') != MATCH_YES) {        
    i = MATCH_NO;      
    goto cleanup;          
  }     
     
  i = match_actual_arg(&real->u.expr);  
  if (i != MATCH_YES) goto cleanup;
  real->type = EXPR;      
      
  /* Make sure this name has not appeared yet */     
     
  if (name0[0] != '\0') {         
    for(z=base; z; z=z->next)         
      if (strcmp(z->name, name0) == 0) {          
	g95_error("Keyword '%s' at %C has already appeared in the current "      
		  "argument list", name0);     
	return MATCH_ERROR;;          
      }     
  } 
 
  strcpy(real->name, name0);       
  return MATCH_YES; 
 
cleanup:    
  g95_set_locus(&name_locus);
  return i;  
}




/* g95_match_actual_arglist()-- Matches an actual argument list of a
 * function or subroutine, from the opening parenthesis to the closing
 * parenthesis.  The argument list is assumed to allow keyword
 * arguments because we don't know if the symbol associated with the
 * procedure has an implicit interface or not.  We make sure keywords
 * are unique. */        
        
match g95_match_actual_arglist(int sub_flag, g95_actual_arglist **argum) {        
g95_actual_arglist *head, *tail;    
int seen_keyword;
g95_st_label *label;  
locus old_loc;          
match j;          
          
  *argum = tail = NULL;      
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
      j = g95_match_st_label(&label, 0);        
      if (j == MATCH_NO) g95_error("Expected alternate return label at %C");    
      if (j != MATCH_YES) goto cleanup;

      tail->u.label = label;       
      tail->type = ALT_RETURN;         
      goto n;   
    }    
    
    /* After the first keyword argument is seen, the following
     * arguments must also have keywords. */ 
 
    if (seen_keyword) { 
      j = match_keyword_arg(tail, head);         
         
      if (j == MATCH_ERROR) goto cleanup;         
      if (j == MATCH_NO) {  
	g95_error("Missing keyword name in actual argument list at %C");          
	goto cleanup;  
      }    
    
    } else {   /* See if we have the first keyword argument */        
        
      j = match_keyword_arg(tail, head);   
      if (j == MATCH_YES) seen_keyword = 1;        
      if (j == MATCH_ERROR) goto cleanup;         
         
      if (j == MATCH_NO) {  /* Try for a non-keyword argument */     
	j = match_actual_arg(&tail->u.expr);      
	tail->type = EXPR;        
        
	if (j == MATCH_ERROR) goto cleanup;   
	if (j == MATCH_NO) goto syntax;     
      }    
    }  
  
  n:    
    if (g95_match_char(')') == MATCH_YES) break;  
    if (g95_match_char(',') != MATCH_YES) goto syntax;          
  }         
         
  *argum = head;       
  return MATCH_YES;

syntax:
  g95_error("Syntax error in argument list at %C");        
        
cleanup:   
  g95_free_actual_arglist(head);         
  g95_set_locus(&old_loc);     
     
  return MATCH_ERROR;      
}  
  
  
        
        
/* match_aa_rest()-- Match an actual argument list of a function where
 * the first argument has already been read. */     
     
static match match_aa_rest(g95_expr *first, g95_actual_arglist **argum) {          
g95_actual_arglist *start, *t;        
int seen_keyword;    
locus where;          
match f;      
int a;     
     
  where = *g95_current_locus();  
  
  seen_keyword = 0;       
       
  start = t = g95_get_actual_arglist();    
  start->type = EXPR;         
  start->u.expr = first;       
       
  for(;;) {     
    g95_gobble_whitespace();     
    a = g95_next_char(); 
    if (a == ')') break;      
    if (a != ',') goto syntax;          
          
    t->next = g95_get_actual_arglist(); 
    t = t->next;         
         
    /* After the first keyword argument is seen, the following
     * arguments must also have keywords. */       
       
    if (seen_keyword) {      
      f = match_keyword_arg(t, start); 
 
      if (f == MATCH_ERROR) goto cleanup;      
      if (f == MATCH_NO) {       
	g95_error("Missing keyword name in actual argument list at %C");
	goto cleanup; 
      }      
      
    } else {   /* See if we have the first keyword argument */         
         
      f = match_keyword_arg(t, start);    
      if (f == MATCH_YES) seen_keyword = 1;     
      if (f == MATCH_ERROR) goto cleanup;          
          
      if (f == MATCH_NO) {  /* Try for a non-keyword argument */      
	f = match_actual_arg(&t->u.expr);     
	t->type = EXPR;

	if (f == MATCH_ERROR) goto cleanup;
	if (f == MATCH_NO) goto syntax;
      }        
    }    
  }     
     
  *argum = start; 
  return MATCH_YES;          
          
syntax: 
  g95_error("Syntax error in argument list at %C");    
    
cleanup:    
  g95_free_actual_arglist(start);      
  g95_set_locus(&where);  
  
  return MATCH_ERROR;
}     
     
     
   
   
/* match_function_rest()-- Match the rest of a function call where the
 * first argument has already been matched. */ 
 
static match match_function_rest(g95_symbol *s, locus *where,  
				 g95_expr *first, g95_expr **r) {
g95_actual_arglist *a;      
g95_expr *d;         
match q;    
    
  if (g95_add_function(&s->attr, where) == FAILURE) {
    g95_free_expr(first);   
    return MATCH_ERROR;         
  }       
       
  if (s->result == NULL) s->result = s;          
          
  q = match_aa_rest(first, &a);          
  if (q == MATCH_ERROR) return q;   
  if (q == MATCH_NO) abort();     
     
  d = g95_get_expr();
  d->type = EXPR_FUNCTION;      
  d->where = *where;      
  d->symbol = s;          
          
  d->value.function.actual = a;         
         
  *r = d;    
  return MATCH_YES;        
}       
       
       


/* match_ss_fc()-- Match a substring reference or function call,
 * figuring out what we have in the process.  The colon is mandatory
 * in the substring and cannot appear in the function argument list.
 * A keyword argument also indicates a function call.  A left paren is
 * next on the input. */      
      
static match match_ss_fc(g95_symbol *sy, g95_expr **r) { 
char name[G95_MAX_SYMBOL_LEN+1];  
g95_expr *first; 
locus loc;       
match f;     
int l; 
 
  loc = *g95_current_locus();     
  g95_match_char('('); 
 
  g95_gobble_whitespace();      
  l = g95_peek_char() == ')' || 
    (g95_match_name(name) == MATCH_YES && g95_match_char('=') == MATCH_YES);        
        
  g95_set_locus(&loc);      
  if (l) return match_function_call(sy, r);  
  
  g95_match_char('(');
  g95_gobble_whitespace();        
        
  if (g95_peek_char() == ':') 
    return match_substring_rest(sy, &loc, NULL, r);         
         
  f = match_actual_arg(&first);     
  switch(f) {        
  case MATCH_YES:       
    break;         
         
  case MATCH_NO:        
    g95_error("Syntax error at %C");   
    f = MATCH_ERROR;      
      
    /* Fall through */        
  case MATCH_ERROR:        
    return f;    
  }          
          
  g95_gobble_whitespace();         
  f = (g95_peek_char() == ':')      
    ? match_substring_rest(sy, &loc, first, r)   
    : match_function_rest(sy, &loc, first, r);     
     
  return f;     
}         
         
         
        
        
/* g95_match_variable()-- Match a variable, ie something that can be
 * assigned to.  This starts as a symbol, can be a structure
 * component, array reference or substring.  If the symbol has not
 * been previously seen, we assume it is a variable. */

match g95_match_variable(g95_expr **r, int decl_flag) {  
char nam[G95_MAX_SYMBOL_LEN+1];          
g95_symbol *symbol;    
match g; 
 
  g = g95_match_name(nam);  
  if (g != MATCH_YES) return g;         
         
  switch(get_rvalue_type(nam, &symbol)) {          
  case RVAL_VARIABLE:     
  case RVAL_UNKNOWN:        
    g = match_variable_part(symbol, r, decl_flag);      
    break;   
   
  default:       
    g95_error("Expected '%s' at %C to be a VARIABLE", nam);        
    g = MATCH_ERROR;        
  }     
     
  return g;       
}          
    
    
/* match_structure_constructor()-- Match a structure constructor.  The
 * initial symbol has already been seen. */    
    
static match match_structure_constructor(g95_symbol *sy, g95_expr **result) {  
g95_constructor *start, *tail;   
g95_expr *h; 
locus where;  
match w;  
int k;         
         
  start = tail = NULL;    
    
  if (g95_match_char('(') != MATCH_YES) goto syntax;       
  where = *g95_current_locus();   
   
  for(;;) {     
    if (start == NULL)   
      tail = start = g95_get_constructor();       
    else {       
      tail->next = g95_get_constructor();     
      tail = tail->next;   
    }
	
    w = g95_match_expr(&tail->expr);     
    if (w == MATCH_NO) goto syntax;          
    if (w == MATCH_ERROR) goto cleanup;          
          
    g95_gobble_whitespace();      
    k = g95_next_char();      
    if (k == ')') break; 
    if (k != ',') goto syntax;
  }    
    
  h = g95_get_expr();       
       
  h->type = EXPR_STRUCTURE;     
  h->ts.type = BT_DERIVED;      
  h->ts.derived = sy;
  h->symbol = sy;  
  
  h->where = where;         
         
  h->value.constructor = start;   
   
  *result = h; 
  return MATCH_YES;  

syntax: 
  g95_error("Syntax error in structure constructor at %C");      
      
cleanup:      
  g95_free_constructor(start);        
  return MATCH_ERROR;    
}


      
      
/* g95_match_rvalue()-- Matches a variable name followed by anything
 * that might follow it-- array reference, argument list of a
 * function, etc. */       
       
match g95_match_rvalue(g95_expr **res) {       
char nam[G95_MAX_SYMBOL_LEN+1];          
g95_symbol *symb;     
locus pos;  
match u;        
       
  u = g95_match_name(nam);       
  if (u != MATCH_YES) return u;  
  
  pos = *g95_current_locus();        
        
  switch(get_rvalue_type(nam, &symb)) {     
  case RVAL_VARIABLE:
    u = match_variable_part(symb, res, 0);  
    break;   
   
  case RVAL_FUNCTION:  
    u = match_function_call(symb, res);          
    break; 
 
  case RVAL_DERIVED: 
    u = match_structure_constructor(symb, res);      
    break;

  case RVAL_UNKNOWN:        
    g95_gobble_whitespace(); 
    if (g95_peek_char() != '(')      
      u = match_variable_part(symb, res, 0);        
    else      
      u = match_ss_fc(symb, res);  
  
    break;      
      
  case RVAL_SUBROUTINE:       
    g95_error("Expected '%s' at %C to be a VARIABLE", nam);       
    /* Fall through */      
      
  case RVAL_ERROR:      
    u = MATCH_ERROR;        
    break;     
  }       
       
  return u;   
} 
 
 
