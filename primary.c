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
        
        
  
  
/* check_digit()-- Given a character and a radix, see if the character
 * is a valid digit in that radix. */        
        
static int check_digit(int i, int radix) {
int w;    
    
  switch(radix) {      
  case 2:  
    w = ('0' <= i && i <= '1');        
    break;        
        
  case 8:       
    w = ('0' <= i && i <= '7');    
    break;     
     
  case 10:    
    w = ('0' <= i && i <= '9');     
    break;      
      
  case 16:    
    w = ('0' <= i && i <= '9') || ('a' <= i && i <= 'f');  
    break;

  default:  
    g95_internal_error("check_digit(): bad radix");    
  }     
     
  return w; 
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
g95_locus oldl;      
char z, peek;        
int len;  
  
  g95_gobble_whitespace();   
  z = g95_next_char();  
  if (!isalpha(z)) return MATCH_NO;  
  
  *nm++ = z;   
  len = 1;          
          
  for(;;) {        
    oldl = g95_current_locus;   
    z = g95_next_char();         
         
    if (z == '_') {        
      peek = g95_peek_char();    
          
      if (peek == '\'' || peek == '\"') {
	g95_current_locus = oldl;
	*nm = '\0';       
	return MATCH_YES; 
      }     
    }    
    
    if (!isalnum(z) && z != '_' && g95_option.dollar && z != '$') break;          
          
    *nm++ = z;       
    if (++len > G95_MAX_SYMBOL_LEN) break; 
  }          
          
  return MATCH_NO;    
} 
 
 


/* g95_next_string_char()-- Reads the next character of a string constant,
 * taking care to return doubled delimiters on the input as a single
 * instance of the delimiter.  Special return values are:
 *   -1   End of the string, as determined by the delimiter
 *   -2   Unterminated string detected
 *
 * Backslash codes are also expanded at this time. */         
         
int g95_next_string_char(char delimiter) {       
g95_locus old_locus;
int t;        
        
  t = g95_next_char_literal(1);

  if (t == '\n') return -2;    
    
  if (t == '\\') { 
    old_locus = g95_current_locus; 
 
    switch(g95_next_char_literal(1)) {
    case 'a':  t = '\a'; break;        
    case 'b':  t = '\b'; break; 
    case 't':  t = '\t'; break;         
    case 'f':  t = '\f'; break;     
    case 'n':  t = '\n'; break;       
    case 'r':  t = '\r'; break;         
    case 'v':  t = '\v'; break;        
    case '\\': t = '\\'; break;          
          
    default:     /* Unknown backslash codes are simply not expanded */        
      g95_current_locus = old_locus; 
      break;          
    } 
  }

  if (t != delimiter) return t;      
      
  old_locus = g95_current_locus;      
  t = g95_next_char_literal(1);       
       
  if (t == delimiter) return t;     
  g95_current_locus = old_locus;          
          
  return -1; 
}  
  
  
 
 
/* match_kind_param()-- Matches a kind-parameter expression, which is
 * either a named symbolic constant or a nonnegative integer constant.
 * If successful, sets the kind value to the correct integer.  */     
     
static match match_kind_param(int *k) {    
char nm[G95_MAX_SYMBOL_LEN+1]; 
g95_symbol *s;  
char *u; 
match c;    
    
  c = g95_match_small_literal_int(k);          
  if (c != MATCH_NO) return c;          
          
  c = g95_match_name(nm);          
  if (c != MATCH_YES) return c; 
 
  if (g95_find_symbol(nm, NULL, 1, &s)) return MATCH_ERROR;     
     
  if (s == NULL) return MATCH_NO;   
   
  if (s->attr.flavor != FL_PARAMETER) return MATCH_NO;          
          
  u = g95_extract_int(s->value, k);       
  if (u != NULL) return MATCH_NO;       
       
  if (*k < 0) return MATCH_NO;       
       
  return MATCH_YES;
}     
     
     
 
 
/* match_hollerith_constant()-- Read a Hollerith constant.  The length
 * and the H have already been matched. */          
          
static void match_hollerith_constant(g95_expr *q) {          
int len;   
char u, *a;       
       
  a = q->value.character.string; 
  len = q->value.character.length;         
         
  while(len > 0) {   
    u = g95_next_char_literal(1); 
 
    if (u == '\n') {         
      memset(a, ' ', len);       
      break;         
    }        
        
    *a++ = u;       
    len--;       
  }  
}         
         
         


/* g95_extend_ref()-- Used by match_varspec() to extend the reference
 * list by one element. */    
    
g95_ref *g95_extend_ref(g95_expr *primary, int type) {          
g95_ref *tail;   
   
  if (primary->ref == NULL)          
    primary->ref = tail = g95_get_ref();
  else {   
    tail = primary->ref;        
    while(tail->next != NULL)     
      tail = tail->next;         
         
    tail->next = g95_get_ref();        
    tail = tail->next;      
  }       
       
  tail->type = type;     
  tail->where = g95_current_locus;

  return tail;     
}        
        
        
    
    
/* match_substring_rest()-- Match the trailing part of a substring.  A
 * colon is guaranteed to be next.  */       
       
static match match_substring_rest(g95_symbol *sym, g95_locus *where, 
				  g95_expr *s, g95_expr **r) { 
g95_expr *k, *stop; 
g95_ref *ref;      
match v;   
   
  g95_next_char(); 
  stop = NULL;    
    
  g95_gobble_whitespace();       
  if (g95_peek_char() != ')') { 
    v = g95_match_expr(&stop);         
    if (v == MATCH_ERROR) goto cleanup; 
    if (v == MATCH_NO) goto syntax;      
  } 
 
  if (g95_match_char(')') != MATCH_YES) goto syntax;

  k = g95_get_expr();         
  k->type = EXPR_VARIABLE;          
  k->symbol = sym;   
  k->where = *where;          
          
  k->ref = ref = g95_get_ref();  
  
  ref->type = REF_SUBSTRING;       
  ref->u.ss.start = s;     
  ref->u.ss.end = stop;
  ref->where = *where;          
          
  *r = k;  
  return MATCH_YES;  
  
syntax:      
   g95_error("Syntax error in substring reference at %C");          
          
cleanup:       
  if (s != NULL) g95_free_expr(s);
  if (stop   != NULL) g95_free_expr(stop);         
  return MATCH_ERROR;  
}   
   
   


/* compiling()-- Return nonzero if we are currently compiling the
 * given symbol.  This is used for determining if a function name is
 * its own result variable. */   
   
static int compiling(g95_symbol *symbol) {       
g95_namespace *ns;   
g95_symbol *f;   
   
  for(ns=g95_current_ns; ns; ns=ns->parent)  
    if (!symbol->attr.entry) {          
      if (ns->proc_name == symbol) return 1;   
    } else {  
      f = NULL; 
      g95_find_symbol(symbol->name, ns, 0, &f);

      if (f == symbol) return 1;      
    }          
          
  return 0;       
}        
        
        
 
 
/* match_digits()-- Match the digit string part of an integer.
 * If the buffer is NULL, we just count characters for the second
 * pass.  Returns the number of characters matched, -1 for no match. */  
  
static int match_digits(int signflag, int radix, char *buffer) {        
g95_locus oldl;     
int length, s;         
         
  length = 0;        
  s = g95_next_char();         
         
  if (signflag && (s == '+' || s == '-')) {       
    if (buffer != NULL) *buffer++ = s;    
    g95_gobble_whitespace(); 
    s = g95_next_char();
    length++;       
  }      
      
  if (!check_digit(s, radix)) return -1;       
       
  length++;      
  if (buffer != NULL) *buffer++ = s;       
       
  for(;;) {         
    oldl = g95_current_locus;     
    s = g95_next_char();    
    
    if (!check_digit(s, radix)) break;     
     
    if (buffer != NULL) *buffer++ = s;  
    length++;       
  } 
 
  g95_current_locus = oldl;        
        
  return length;     
}       
       
       
       
       
/* ha_symbol()-- Given a name, look for it in parent scopes, stopping
 * when required.  Creates a new symbol in the current namespace if
 * nothing is found. */      
      
static g95_symbol *ha_symbol(char *nm) {
g95_state_data *comp;
g95_namespace *ns;       
g95_symbol *s;

  ns = g95_current_ns;        
  comp = g95_state_stack;        
        
  for(;;) {
    if (g95_find_symbol(nm, ns, 0, &s)) {   
      g95_error("Symbol '%s' at %C is ambiguous", nm);    
      return NULL;        
    } 
 
    if (s != NULL && g95_local_symbol(s)) break; 
 
    ns = ns->parent; 
    if (ns == NULL) goto unknown; 
 
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
 
  return s;    
    
unknown:  
  g95_get_symbol(nm, NULL, &s);
  return s;         
}       
       
       
        
        
/* g95_match_structure_constructor()-- Match a structure constructor.
 * The initial symbol has already been seen. */

match g95_match_structure_constructor(g95_symbol *sy, g95_expr **r) {
g95_constructor *h, *tail;
g95_locus where;     
g95_expr *p;       
match g;       
int n;       
       
  h = tail = NULL;     
     
  if (g95_match_char('(') != MATCH_YES) goto syntax;      
  where = g95_current_locus;   
   
  for(;;) {          
    if (h == NULL)      
      tail = h = g95_get_constructor();
    else {   
      tail->next = g95_get_constructor();        
      tail = tail->next;     
    }          
	          
    g = g95_match_expr(&tail->expr);   
    if (g == MATCH_NO) goto syntax;    
    if (g == MATCH_ERROR) goto cleanup;  
  
    g95_gobble_whitespace();         
    n = g95_next_char();         
    if (n == ')') break;    
    if (n != ',') goto syntax;     
  }        
        
  p = g95_get_expr();          
          
  p->type = EXPR_STRUCTURE;         
  p->ts.type = BT_DERIVED;       
  p->ts.derived = sy;    
  p->symbol = sy; 
 
  p->where = where;   
   
  p->value.constructor = h;         
         
  *r = p;      
  return MATCH_YES;     
   
syntax:       
  g95_error("Syntax error in structure constructor at %C");  
  
cleanup:   
  g95_free_constructor(h);        
  return MATCH_ERROR;         
}


      
      
/* match_substring()-- Match a substring reference associated with a
 * constant string. */         
         
static match match_substring(g95_charlen *charlen, g95_ref **r) {    
g95_expr *s, *e;    
g95_locus old;  
g95_ref *ref;      
match c;       
       
  s = NULL;
  e = NULL;      
      
  old = g95_current_locus;          
          
  c = g95_match_char('('); 
  if (c != MATCH_YES) return MATCH_NO;      
      
  if (g95_match_char(':') != MATCH_YES) {        
    c = g95_match_expr(&s);   
    if (c != MATCH_YES) {         
      c = MATCH_NO;         
      goto cleanup;
    }        
        
    c = g95_match_char(':');        
    if (c != MATCH_YES) goto cleanup;         
  }          
          
  if (g95_match_char(')') != MATCH_YES) {  
    c = g95_match_expr(&e);         
         
    if (c == MATCH_NO) goto syntax;         
    if (c == MATCH_ERROR) goto cleanup;       
       
    c = g95_match_char(')');   
    if (c == MATCH_NO) goto syntax; 
  }          
          
/* Optimize away the (:) reference */    
    
  if (s == NULL && e == NULL)         
    ref = NULL;         
  else {
    ref = g95_get_ref();       
       
    ref->type = REF_SUBSTRING;   
    ref->u.ss.start = s;        
    ref->u.ss.end = e; 
    ref->u.ss.length = charlen;         
  }       
       
  *r = ref;  
  return MATCH_YES;         
         
syntax:    
  g95_error("Syntax error in SUBSTRING specification at %C");   
  c = MATCH_ERROR; 
 
cleanup:      
  g95_free_expr(s);
  g95_free_expr(e);  
  
  g95_current_locus = old;         
  return c;    
} 
 
 
       
       
/* get_kind()-- Gets a trailing kind-specification for non-character
 * variables.  Normally returns the integer kind value or
 *   -1   if an error was generated
 *   -2   if no kind was found
 */   
   
static int get_kind(void) {      
int knd;   
match g;

  if (g95_match_char('_') != MATCH_YES) return -2;   
   
  g = match_kind_param(&knd);     
  if (g == MATCH_NO) g95_error("Missing kind-parameter at %C");         
         
  return (g == MATCH_YES) ? knd : -1;          
} 
 
 
 
 
/* match_sym_complex_part()-- Match a real or imaginary part of a complex
 * constant that is a symbolic constant. */       
       
static match match_sym_complex_part(g95_expr **result) {          
char name0[G95_MAX_SYMBOL_LEN+1];       
g95_symbol *s;   
g95_expr *u;     
match n;    
    
  n = g95_match_name(name0);     
  if (n != MATCH_YES) return n;     
     
  if (g95_find_symbol(name0, NULL, 1, &s) || s == NULL) return MATCH_NO;   
   
  if (s->attr.flavor != FL_PARAMETER) {     
    g95_error("Expected PARAMETER symbol in complex constant at %C");          
    return MATCH_ERROR;     
  } 
 
  if (!g95_numeric_ts(&s->value->ts)) {         
    g95_error("Numeric PARAMETER required in complex constant at %C");      
    return MATCH_ERROR;
  }          
          
  if (s->value->rank != 0) {      
    g95_error("Scalar PARAMETER required in complex constant at %C");         
    return MATCH_ERROR;    
  }

  switch(s->value->ts.type) {     
  case BT_REAL:       
    u = g95_copy_expr(s->value);        
    break;         
         
  case BT_COMPLEX:   
    u = g95_complex2real(s->value, s->value->ts.kind);          
    if (u == NULL) goto error;          
    break;

  case BT_INTEGER:          
    u = g95_int2real(s->value, g95_default_real_kind());          
    if (u == NULL) goto error;          
    break;      
      
  default:  
    g95_internal_error("g95_match_sym_complex_part(): Bad type");
  }         
         
  *result = u;     /* e is a scalar, real, constant expression */       
  return MATCH_YES;          
          
error:     
  g95_error("Error converting PARAMETER constant in complex constant at %C");   
  return MATCH_ERROR;       
}  
  
  
          
          
/* match_string_constant()-- See if the current input matches a
 * character constant.  Lots of contortions have to be done to match
 * the kind parameter which comes before the actual string.  The main
 * consideration is that we don't want to error out too quickly.  For
 * example, we don't actually do any validation of the kinds until we
 * have actually seen a legal delimiter.  Using match_kind_param()
 * generates errors too quickly. */ 
 
static match match_string_constant(g95_expr **res) {   
char *a, n[G95_MAX_SYMBOL_LEN+1]; 
int o, v, k0, len, delimiter;
g95_locus old_locus, start_locus;  
g95_symbol *sy;         
g95_expr *b;      
char *y; 
match k;      
      
  old_locus = g95_current_locus;      
      
  g95_gobble_whitespace();  
  
  start_locus = g95_current_locus;     
     
  v = g95_next_char(); 
  if (v == '\'' || v == '"') {       
    k0 = g95_default_character_kind();
    goto got_delim;
  } 
 
  if (isdigit(v)) {    
    k0 = 0;         
         
    while(isdigit(v)) {       
      k0 = k0*10 + v - '0';  
      if (k0 > 9999999) goto no_match;
      v = g95_next_char();          
    }

  } else {          
    g95_current_locus = old_locus;      
      
    k = match_charkind_name(n); 
    if (k != MATCH_YES) goto no_match;

    if (g95_find_symbol(n, NULL, 1, &sy) || sy == NULL ||          
	sy->attr.flavor != FL_PARAMETER) goto no_match;   
   
    k0 = -1;   
    v = g95_next_char();          
  }      
      
  if (v == ' ') {         
    g95_gobble_whitespace();       
    v = g95_next_char();         
  } 
 
  if (v == 'h') {  
    *res = g95_char_expr(k0, g95_default_character_kind(), &start_locus);        
    match_hollerith_constant(*res);          
    return MATCH_YES;
  }    
    
  if (v != '_') goto no_match;      
      
  g95_gobble_whitespace();
  start_locus = g95_current_locus;          
          
  v = g95_next_char();          
  if (v != '\'' && v != '"') goto no_match;        
        
  if (k0 == -1) {       
    y = g95_extract_int(sy->value, &k0);     
    if (y != NULL) {   
      g95_error(y);    
      return MATCH_ERROR;       
    }
  } 
 
  if (g95_validate_kind(BT_CHARACTER, k0) == -1) {    
    g95_error("Invalid kind %d for CHARACTER constant at %C", k0);    
    return MATCH_ERROR;       
  }       
       
/* Scan the string into a block of memory by first figuring out how
 * long it is, allocating the structure, then re-reading it.  This
 * isn't particularly efficient, but string constants aren't that
 * common in most code.  Someday I'll read more on obstacks. */    
    
got_delim:   
  delimiter = v;         
  len = 0;       
       
  for(;;) {         
    v = g95_next_string_char(delimiter);  
    if (v == -1) break; 
    if (v == -2) {        
      g95_current_locus = start_locus;
      g95_error("Unterminated character constant beginning at %C");          
      return MATCH_ERROR;      
    }      
      
    len++;       
  }      
      
  b = g95_char_expr(len, k0, &start_locus);         
  a = b->value.character.string;        
        
  g95_current_locus = start_locus;          
  g95_next_char();              /* Skip delimiter */          
          
  for(o=0; o<len; o++) 
    *a++ = g95_next_string_char(delimiter);   
   
  *a = '\0';     /* C-style string is for development/debug purposes */

  if (g95_next_string_char(delimiter) != -1)     
    g95_internal_error("match_string_constant(): Delimiter not found");

  if (match_substring(NULL, &b->ref) != MATCH_NO) b->type = EXPR_SUBSTRING;

  *res = b;
  return MATCH_YES;

no_match:  
  g95_current_locus = old_locus;         
  return MATCH_NO;       
}         
         
         
          
          
/* match_integer_constant()-- Match an integer (digit string and
 * optional kind). */    
    
static match match_integer_constant(g95_expr **result, int signflag) {       
g95_locus where;   
int l, knd;
char *buf;          
g95_expr *q;          
          
  where = g95_current_locus;       
  g95_gobble_whitespace();          
          
  l = match_digits(signflag, 10, NULL);         
  g95_current_locus = where;        
  if (l == -1) return MATCH_NO;     
     
  buf = alloca(l+1);     
  memset(buf, '\0', l+1);  
  
  g95_gobble_whitespace();       
       
  match_digits(signflag, 10, buf);       
       
  knd = get_kind();        
  if (knd == -2) knd = g95_default_integer_kind();   
  if (knd == -1) return MATCH_ERROR;       
       
  if (g95_validate_kind(BT_INTEGER, knd) == -1) {         
    g95_error("Integer kind %d at %C not available", knd);  
    return MATCH_ERROR;  
  }    
    
  q = g95_convert_integer(buf, knd, 10, &g95_current_locus);  
  
  if (g95_range_check(q) != ARITH_OK) {
    g95_error("Integer too big for its kind at %C"); 
 
    g95_free_expr(q);          
    return MATCH_ERROR;   
  }     
     
  *result = q;    
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
 * A couple of rules come into play.  Subobjects of targets are always
 * targets themselves.  If we see a component that goes through a
 * pointer, then the expression must also be a target, since the
 * pointer is associated with something (if it isn't core will soon be
 * dumped).  If we see a full part or section of an array, the
 * expression is also an array.  This means that 'target' might be set
 * when it is not really.
 *
 * We can have at most one full array reference. */  
  
symbol_attribute g95_variable_attr(g95_expr *e2, g95_typespec *typesp) {    
int dimension, pointer, target;
symbol_attribute attribute;       
g95_ref *reference;  
  
  if (e2->type != EXPR_VARIABLE)       
    g95_internal_error("g95_variable_attr(): Expression isn't a variable");

  reference = e2->ref;      
  attribute = e2->symbol->attr;      
      
  dimension = attribute.dimension; 
  pointer = attribute.pointer;   
   
  target = attribute.target;     
  if (pointer) target = 1;   
   
  if (typesp != NULL && e2->ts.type == BT_UNKNOWN) *typesp = e2->symbol->ts;          
          
  for(; reference; reference=reference->next)     
    switch(reference->type) {         
    case REF_ARRAY:   
      switch(reference->u.ar.type) {      
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
      g95_get_component_attr(&attribute, reference->u.c.component);       
      if (typesp != NULL) *typesp = reference->u.c.component->ts;     
     
      pointer = reference->u.c.component->pointer;       
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
       
       
    
    
/* match_const_complex_part()-- Match the real and imaginary parts of
 * a complex number.  This subroutine is essentially
 * match_real_constant() modified in a couple of ways: A sign is
 * always allowed and numbers that would look like an integer to
 * match_real_constant() are automatically created as floating point
 * numbers.  The messiness involved with making sure a decimal point
 * belongs to the number and not a trailing operator is not necessary
 * here either (Hooray!). */

static match match_const_complex_part(g95_expr **result) {        
int knd, seen_digits, seen_dp, cnt; 
char *p, f, exp_char, *b;         
g95_locus old;  
  
  old = g95_current_locus; 
  g95_gobble_whitespace();    
    
  seen_dp = 0;  
  seen_digits = 0;
  cnt = 0;         
  exp_char = ' ';         
         
  f = g95_next_char();    
  if (f == '-' || f == '+') {
    f = g95_next_char();    
    cnt++;  
  }     
     
  for(;; f=g95_next_char(), cnt++) { 
    if (f == '.') {    
      if (seen_dp) goto no_match;      
      seen_dp = 1;     
      continue;     
    }        
        
    if (isdigit(f)) {          
      seen_digits = 1;         
      continue;         
    }      
      
    break;      
  }   
   
  if (!seen_digits || (f != 'd' && f != 'e')) goto done;       
  exp_char = f;   
   
/* scan exponent */ 
 
  f = g95_next_char(); 
  cnt++;    
    
  if (f == '+' || f == '-') {  /* optional sign */         
    f = g95_next_char();
    cnt++;    
  }        
        
  if (!isdigit(f)) {         
    g95_error("Missing exponent in real number at %C");  
    return MATCH_ERROR;      
  }          
          
  while(isdigit(f)) {        
    f = g95_next_char();
    cnt++;      
  }

done: 
  if (!seen_digits) goto no_match;          
          
/* Convert the number */     
     
  g95_current_locus = old;       
  g95_gobble_whitespace();         
         
  b = alloca(cnt+1);          
  memset(b, '\0', cnt+1);         
         
  p = b;          
  while(cnt>0) { 
    f = g95_next_char();    
    if (f == 'd') f = 'e';   /* Hack for mpf_init_set_str() */     
    *p++ = f;
    cnt--;       
  }         
         
  *p = '\0'; 
 
  knd = get_kind();        
  if (knd == -1) return MATCH_ERROR;

/* If the number looked like an integer, forget about a kind we may
 * have seen, otherwise validate the kind against real kinds. */  
  
  if (seen_dp == 0 && exp_char == ' ') {   
    if (knd == -2) knd = g95_default_integer_kind();   
   
  } else {        
    if (exp_char == 'd') {  
      if (knd != -2) {
	g95_error("Real number at %C has a 'd' exponent and an explicit kind");
	return MATCH_ERROR;
      }      
      knd = g95_default_double_kind();        
              
    } else {      
      if (knd == -2) knd = g95_default_real_kind();       
    }   
   
    if (g95_validate_kind(BT_REAL, knd) == -1) {         
      g95_error("Invalid real kind %d at %C", knd); 
      return MATCH_ERROR;    
    } 
  }      
      
  *result = g95_convert_real(b, knd, &g95_current_locus);   
  return MATCH_YES;       
       
no_match:          
  g95_current_locus = old;          
  return MATCH_NO;       
}


        
        
/* match_complex_part()-- Match a real or imaginary part of a complex number */ 
 
static match match_complex_part(g95_expr **result) {    
match w;          
          
  w = match_sym_complex_part(result);     
  if (w != MATCH_NO) return w;  
  
  return match_const_complex_part(result);        
}   
   
   
          
          
/* match_real_constant()-- Match a real constant of some sort. */     
     
static match match_real_constant(g95_expr **rslt, int signflag) { 
int kind, y, cnt, seen_dp, seen_digits, exp_char;        
g95_locus old, temp_loc;          
char *w, *buf;   
g95_expr *s;        
        
  old = g95_current_locus;       
  g95_gobble_whitespace();   
   
  s = NULL;          
  buf = NULL;   
   
  cnt = 0;         
  seen_dp = 0;
  seen_digits = 0;        
  exp_char = ' '; 
 
  y = g95_next_char(); 
  if (signflag && (y == '+' || y == '-')) {
    y = g95_next_char();         
    cnt++;     
  }         
         
/* Scan significand */        
        
  for(;; y=g95_next_char(), cnt++) {
    if (y == '.') {      
      if (seen_dp) goto done;   
   
      /* Check to see if "." goes with a following operator like ".eq." */    
    
      temp_loc = g95_current_locus;         
      y = g95_next_char();

      if (y == 'e' || y == 'd' || y == 'q') {   
	y = g95_next_char();    
	if (y == '.') goto done;   /* Operator named .e. or .d. */ 
      }          
          
      if (isalpha(y)) goto done;   /* Distinguish 1.e9 from 1.eq.2 */       
       
      g95_current_locus = temp_loc;     
      seen_dp = 1;        
      continue;  
    }  
  
    if (isdigit(y)) {    
      seen_digits = 1;         
      continue;       
    }

    break; 
  }        
        
  if (!seen_digits || (y != 'e' && y != 'd' && y != 'q')) goto done;
  exp_char = y;     
     
/* scan exponent */        
        
  y = g95_next_char(); 
  cnt++;   
   
  if (y == '+' || y == '-') {  /* optional sign */ 
    y = g95_next_char();    
    cnt++;        
  }          
          
  if (!isdigit(y)) {     
    if (!seen_digits) {          
      g95_current_locus = old;     
      return MATCH_NO;   /* ".e" can be something else */   
    }       
       
    g95_error("Missing exponent in real number at %C");  
    return MATCH_ERROR;       
  }        
        
  while(isdigit(y)) {      
    y = g95_next_char();         
    cnt++;         
  }       
       
/* See what we've got */       
       
done:        
  if (!seen_digits || (!seen_dp && exp_char == ' ')) {    
    g95_current_locus = old;          
    return MATCH_NO; 
  }     
     
/* Convert the number */    
    
  g95_current_locus = old;      
  g95_gobble_whitespace();     
     
  buf = alloca(cnt+1);         
  memset(buf, '\0', cnt+1);   
   
  w = buf; 
  while(cnt>0) {  
    *w = g95_next_char();     
    if (*w == 'd' || *w == 'q') *w = 'e';   /* Hack for mpf_init_set_str() */      
    w++;          
    cnt--;  
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
   
  s = g95_convert_real(buf, kind, &g95_current_locus);        
        
  switch(g95_range_check(s)) {          
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
     
  *rslt = s;       
  return MATCH_YES;   
   
cleanup:        
  g95_free_expr(s);
  return MATCH_ERROR; 
}  
  
  
      
      
/* g95_expr_attr()-- Return the attribute from a general expression */     
     
symbol_attribute g95_expr_attr(g95_expr *d) {  
symbol_attribute atr;

  switch(d->type) {         
  case EXPR_VARIABLE:  
    atr = g95_variable_attr(d, NULL);      
    break;   
   
  case EXPR_FUNCTION:          
    g95_clear_attr(&atr);          
    atr = d->symbol->result->attr;     
     
    /* NULL() returns pointers.  May have to take care of this here */         
         
    break;  
  
  default:         
    g95_clear_attr(&atr);
    break;    
  }          
          
  return atr;   
}    
    
    
     
     
/* g95_local_symbol()-- Return nonzero if this symbol is a local
 * symbol in it's namespace and prevents another symbol of the same
 * name from being host associated */      
      
int g95_local_symbol(g95_symbol *sym) {       
symbol_attribute e;   
   
  e = sym->attr;      
     
  if ((sym->ts.type != BT_UNKNOWN && !e.implicit_type) ||        
      e.proc == PROC_ST_FUNCTION || e.pointer || e.save || e.target ||         
      e.flavor == FL_PARAMETER || sym->as != NULL || e.allocatable ||         
      e.in_common || e.data || e.dummy || e.result_var || e.intrinsic ||        
      sym->namelist != NULL || e.generic || e.flavor == FL_LABEL || 
      e.equivalenced) return 1;        
        
  if (e.flavor == FL_PROCEDURE && e.if_source == IFSRC_DECL) return 1;         
         
  if (e.proc == PROC_MODULE || e.flavor == FL_DERIVED) return 1;      
      
  if (sym->ns->interface) return 1;    
    
  return 0;        
}         
         
         
      
      
/* match_function_call()-- Match a function reference */      
      
static match match_function_call(g95_symbol *sym, g95_expr **result) { 
g95_actual_arglist *real;   
g95_locus where;       
g95_expr *k;  
match n;     
     
  if (g95_add_function(&sym->attr, NULL) == FAILURE) return MATCH_ERROR; 
 
  where = g95_current_locus;     
  n = g95_match_actual_arglist(0, &real);      
      
  switch(n) {      
  case MATCH_YES:      
    k = g95_get_expr();       
    k->type = EXPR_FUNCTION;     
    k->symbol = sym;         
    k->where = where;          
          
    k->value.function.actual = real; 
 
    if (sym->result == NULL) sym->result = sym;          
          
    *result = k;       
    break;  
  
  case MATCH_NO:        
    g95_error("Missing actual argument list in function call to '%s' at %C",       
	      sym->name);         
    n = MATCH_ERROR;        
    break;         
         
  case MATCH_ERROR:      
    break;       
  }         
         
  return n;         
}        
        
        


/* match_variable_part()-- Match the parts of a variable that come
 * after the initial name. */      
      
static match match_variable_part(g95_symbol *sym, g95_expr **res,       
				 int array_ok) { 
g95_ref *re;          
g95_expr *b;    
match w;     
     
  b = g95_get_expr();   
  b->type = EXPR_VARIABLE;  
  b->symbol = sym;     
  b->where = g95_current_locus;     
     
  array_ok |= (sym->as != NULL || sym->ts.type == BT_CHARACTER);  
  
loop: 
  g95_gobble_whitespace();         
  switch(g95_peek_char()) {    
  case '(':
    if (!array_ok) {    
      g95_free_expr(b);     
      g95_error("Unexpected array reference at %C");   
      return MATCH_ERROR;     
    }      
      
    array_ok = 1;    /* for substring refs */   
   
    re = g95_extend_ref(b, REF_ARRAY); 
 
    w = g95_match_array_ref(&re->u.ar, 0);         
    if (w == MATCH_ERROR) goto cleanup; 
    if (w == MATCH_YES) goto loop;         
         
    g95_internal_error("match_variable_part(): Bad array match");          
          
  case '%':    
    array_ok = 1;     
    g95_next_char();          
          
    re = g95_extend_ref(b, REF_COMPONENT);

    w = g95_match_name(re->u.c.name);        
    if (w == MATCH_ERROR) goto cleanup;        
    if (w == MATCH_YES) goto loop;        
        
    g95_error("Missing component name at %C");        
    goto cleanup;  
  
  default:    
    break;          
  }         
         
  switch(sym->attr.flavor) {      
  case FL_VARIABLE:   
  case FL_PARAMETER:
    break;  
  
  case FL_PROCEDURE:  
    if (sym->attr.function && sym->result == sym) break; 
 
    /* Fall through to error */   
   
  default:  
    if (g95_add_flavor(&sym->attr, FL_VARIABLE, NULL) == FAILURE)   
      goto cleanup;         
  }        
        
  *res = b;          
  return MATCH_YES; 
 
cleanup:         
  g95_free_expr(b);         
  return MATCH_ERROR;   
}          
          
          
     
     
/* match_complex_constant()-- Try to match a complex constant */     
     
static match match_complex_constant(g95_expr **rslt) {     
g95_expr *p, *real, *imag;
g95_error_buf old_error;  
g95_typespec target;   
g95_locus oldl;     
int k0;       
match b;        
        
  oldl = g95_current_locus;       
  real = imag = p = NULL;         
         
  b = g95_match_char('('); 
  if (b != MATCH_YES) return b;    
    
  g95_push_error(&old_error);     
     
  b = match_complex_part(&real);         
  if (b == MATCH_NO) goto cleanup; 
 
  if (g95_match_char(',') == MATCH_NO) {
    g95_pop_error(&old_error);
    b = MATCH_NO;          
    goto cleanup;         
  }      
      
/* If m is error, then something was wrong with the real part and we
 * assume we have a complex constant because we've seen the ','.  An
 * ambiguous case here is the start of an iterator list of some sort.
 * These sort of lists are matched prior to coming here. */   
     
  if (b == MATCH_ERROR) goto cleanup;     
  g95_pop_error(&old_error);         
         
  b = match_complex_part(&imag);
  if (b == MATCH_NO) goto syntax;      
  if (b == MATCH_ERROR) goto cleanup;  
  
  b = g95_match_char(')');        
  if (b == MATCH_NO) goto syntax;  
  
  if (b == MATCH_ERROR) goto cleanup;      
      
/* Decide on the kind of this complex number */         
         
  k0 = g95_kind_max(real, imag);
  target.type = BT_REAL;      
  target.kind = k0;

  if (k0 != real->ts.kind) g95_convert_type(real, &target, 2);      
  if (k0 != imag->ts.kind) g95_convert_type(imag, &target, 2);

  p = g95_convert_complex(real, imag, k0);  
  p->where = g95_current_locus;          
          
  g95_free_expr(real);   
  g95_free_expr(imag);

  *rslt = p; 
  return MATCH_YES;     
     
syntax:      
  g95_error("Syntax error in COMPLEX constant at %C");        
  b = MATCH_ERROR;        
        
cleanup:      
  g95_free_expr(p);   
  g95_free_expr(real);       
  g95_free_expr(imag);    
  g95_current_locus = oldl;         
         
  return b;
}        
        
        
   
   
/* match_logical_constant().  Match a .true. or .false. */    
   
static match match_logical_constant(g95_expr **rslt) {        
static mstring logical_ops[] = {       
  minit(".false.", 0), 
  minit(".true.", 1),         
  minit(NULL, -1) };        
        
g95_expr *v;       
int i, k;         
         
  i = g95_match_strings(logical_ops);       
  if (i == -1) return MATCH_NO; 
 
  k = get_kind();    
  if (k == -1) return MATCH_ERROR;  
  if (k == -2) k = g95_default_logical_kind();         
         
  if (g95_validate_kind(BT_LOGICAL, k) == -1)       
    g95_error("Bad kind for logical constant at %C"); 
 
  v = g95_get_expr();         
         
  v->type = EXPR_CONSTANT;  
  v->value.logical = i;         
  v->ts.type = BT_LOGICAL;         
  v->ts.kind = k;        
  v->where = g95_current_locus;        
        
  *rslt = v; 
  return MATCH_YES;     
} 
 
 
 
 
/* match_boz_constant()-- Match a binary, octal or hexadecimal
 * constant that can be found in a DATA statement */     
     
static match match_boz_constant(g95_expr **result) {       
int radix, del, l, k;      
g95_locus o;     
char *buffer;   
g95_expr *i;      
char *rname;     
     
  o = g95_current_locus;      
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
      
  o = g95_current_locus;
  
  l = match_digits(0, radix, NULL);        
  if (l == -1) {        
    g95_error("Empty set of digits in %s constants at %C", rname); 
    return MATCH_ERROR;         
  }         
         
  if (g95_next_char() != del) {       
    g95_error("Illegal character in %s constant at %C.", rname);     
    return MATCH_ERROR; 
  }          
          
  g95_current_locus = o;   
   
  buffer = alloca(l+1);      
  memset(buffer, '\0', l+1); 
 
  match_digits(0, radix, buffer);       
  g95_next_char();  /* Gobble trailing ' */   
   
  k = get_kind();          
  if (k == -1) return MATCH_ERROR;    
  if (k == -2) k = g95_default_integer_kind(); 
 
  i = g95_convert_integer(buffer, k, radix, &g95_current_locus); 
 
  if (g95_range_check(i) != ARITH_OK) {         
    g95_error("Integer too big for its kind at %C");        
        
    g95_free_expr(i);          
    return MATCH_ERROR;
  }

  *result = i;         
  return MATCH_YES;  
  
backup:       
  g95_current_locus = o;          
  return MATCH_NO;    
}          
          
          
          
          
/* get_rvalue_type()-- Given the name of an rvalue, figure out what
 * kind it is, peeking into host scopes if necessary. */      
      
static rvalue_type get_rvalue_type(char *nm, g95_symbol **i) { 
g95_symbol *symbol;        
rvalue_type r;  
  
  *i = symbol = ha_symbol(nm);  
  if (symbol == NULL) return RVAL_ERROR;    
    
  g95_save_symbol_data(symbol);

  switch(symbol->attr.flavor) {          
  case FL_PROCEDURE: 
    if (symbol->attr.subroutine) {
      r = RVAL_SUBROUTINE;      
      break;         
    }   
   
    if (symbol->attr.proc == PROC_ST_FUNCTION) {          
      r = RVAL_FUNCTION;   
      break;    
    }     
     
    r = (symbol->attr.function && symbol->result == symbol && compiling(symbol))     
      ? RVAL_VARIABLE 
      : RVAL_FUNCTION;   
   
    break;  
  
    /* Fall through */    
    
  default:         
    g95_error("Symbol '%s' at %C is not appropriate for a primary expression", 
	      nm);  
    r = RVAL_ERROR;   
    break;     
     
  case FL_VARIABLE:          
  case FL_PARAMETER:    
    r = RVAL_VARIABLE; 
    break;      
      
  case FL_UNKNOWN:
    if (symbol->attr.generic || symbol->attr.external || symbol->attr.intrinsic) {          
      r = RVAL_FUNCTION;          
      break;   
    } 
 
    if (symbol->as != NULL) {        
      r = RVAL_VARIABLE;      
      break;     
    }          
          
    r = RVAL_UNKNOWN;
    break;         
         
  case FL_DERIVED:         
    r = RVAL_DERIVED;     
    break;         
  }         
         
  return r;         
}          
          
          
       
       
/* single_name_arg()-- Given an actual argument that is a single name,
 * figure out what to do with it.  The name can potentially be a dummy
 * procedure that we only find out about later. */  
  
static match single_name_arg(char *name0, g95_expr **result) {    
g95_symbol *s;         
g95_typespec typesp;     
int expr_type;  
g95_expr *k;  
  
  g95_clear_ts(&typesp);     
  expr_type = 0;   
   
  switch(get_rvalue_type(name0, &s)) {    
  case RVAL_VARIABLE:   
    expr_type = EXPR_VARIABLE;    
    if (s->ts.type != BT_UNKNOWN) typesp = s->ts;  
    break; 
 
  case RVAL_FUNCTION:  
  case RVAL_SUBROUTINE:     
    expr_type = EXPR_VARIABLE;     
    typesp.type = BT_PROCEDURE;       
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
       
  k = g95_get_expr();          
  k->symbol = s;
  k->type = expr_type;  
  k->ts = typesp;     
  k->where = g95_current_locus;    
    
  *result = k;
  return MATCH_YES;     
}      
      
      
 
 
/* g95_match_literal_constant()-- Match constants in any of several
 * forms.  Returns nonzero for a match, zero for no match. */

match g95_match_literal_constant(g95_expr **rslt, int signflag) {    
match u; 
 
  u = match_complex_constant(rslt);          
  if (u != MATCH_NO) return u;    
    
  u = match_string_constant(rslt);        
  if (u != MATCH_NO) return u; 
 
  u = match_boz_constant(rslt);
  if (u != MATCH_NO) return u;      
      
  u = match_real_constant(rslt, signflag);      
  if (u != MATCH_NO) return u;          
          
  u = match_integer_constant(rslt, signflag);      
  if (u != MATCH_NO) return u;          
          
  u = match_logical_constant(rslt); 
  if (u != MATCH_NO) return u;          
          
  return MATCH_NO;         
}  
  
  
        
        
/* match_actual_arg()-- match a single actual argument.  An expression
 * consisting of a single name is a very special case handled elsewhere */          
          
static match match_actual_arg(g95_expr **rslt) {   
char nm[G95_MAX_SYMBOL_LEN+1];         
g95_locus where, e;          
int l;

  where = g95_current_locus;        
        
  switch(g95_match_name(nm)) {  
  case MATCH_ERROR:        
    return MATCH_ERROR;          
          
  case MATCH_NO:    
    break;  
  
  case MATCH_YES:          
    e = g95_current_locus;      
    g95_gobble_whitespace();    
    l = g95_next_char();      
    g95_current_locus = e; 
 
    if (l != ',' && l != ')') break;   
   
    return single_name_arg(nm, rslt); 
  }

  g95_current_locus = where;         
  return g95_match_expr(rslt);        
}  
  
  
        
        
/* match_keyword_arg()-- Match a keyword argument. */         
         
static match match_keyword_arg(g95_actual_arglist *real,          
			       g95_actual_arglist *base) {         
char n[G95_MAX_SYMBOL_LEN+1];          
g95_actual_arglist *q;    
g95_locus name_locus;
match x;

  name_locus = g95_current_locus;       
  x = g95_match_name(n);       
       
  if (x != MATCH_YES) goto cleanup;     
  if (g95_match_char('=') != MATCH_YES) {      
    x = MATCH_NO; 
    goto cleanup;        
  }     
     
  x = match_actual_arg(&real->u.expr);       
  if (x != MATCH_YES) goto cleanup;  
  real->type = EXPR;

  /* Make sure this name has not appeared yet */        
        
  if (n[0] != '\0') {        
    for(q=base; q; q=q->next)     
      if (strcmp(q->name, n) == 0) {         
	g95_error("Keyword '%s' at %C has already appeared in the current "          
		  "argument list", n);      
	return MATCH_ERROR;;        
      }  
  }       
       
  strcpy(real->name, n); 
  return MATCH_YES;       
       
cleanup:        
  g95_current_locus = name_locus;    
  return x;
}    
    
    
    
    
/* match_aa_rest()-- Match an actual argument list of a function where
 * the first argument has already been read. */          
          
static match match_aa_rest(g95_expr *first, g95_actual_arglist **argps) {         
g95_actual_arglist *start, *end;      
g95_locus oldl;    
int seen_keyword;        
match z;
int l; 
 
  oldl = g95_current_locus;

  seen_keyword = 0;

  start = end = g95_get_actual_arglist();        
  start->type = EXPR;       
  start->u.expr = first; 
 
  for(;;) {       
    g95_gobble_whitespace();     
    l = g95_next_char();         
    if (l == ')') break; 
    if (l != ',') goto syntax;        
        
    end->next = g95_get_actual_arglist();        
    end = end->next;        
        
    /* After the first keyword argument is seen, the following
     * arguments must also have keywords. */       
       
    if (seen_keyword) {        
      z = match_keyword_arg(end, start);        
        
      if (z == MATCH_ERROR) goto cleanup;  
      if (z == MATCH_NO) {    
	g95_error("Missing keyword name in actual argument list at %C");
	goto cleanup;   
      }

    } else {   /* See if we have the first keyword argument */        
        
      z = match_keyword_arg(end, start);
      if (z == MATCH_YES) seen_keyword = 1;        
      if (z == MATCH_ERROR) goto cleanup;     
     
      if (z == MATCH_NO) {  /* Try for a non-keyword argument */   
	z = match_actual_arg(&end->u.expr);
	end->type = EXPR; 
 
	if (z == MATCH_ERROR) goto cleanup;    
	if (z == MATCH_NO) goto syntax;       
      } 
    }    
  }        
        
  *argps = start; 
  return MATCH_YES;     
     
syntax:         
  g95_error("Syntax error in argument list at %C");          
          
cleanup:         
  g95_free_actual_arglist(start);         
  g95_current_locus = oldl;        
        
  return MATCH_ERROR;     
}     
     
     
       
       
/* g95_match_actual_arglist()-- Matches an actual argument list of a
 * function or subroutine, from the opening parenthesis to the closing
 * parenthesis.  The argument list is assumed to allow keyword
 * arguments because we don't know if the symbol associated with the
 * procedure has an implicit interface or not.  We make sure keywords
 * are unique. */       
       
match g95_match_actual_arglist(int sub_flag, g95_actual_arglist **argum) {    
g95_actual_arglist *h, *t;     
g95_st_label *lab;  
g95_locus where;   
int seen_keyword; 
match x;        
        
  *argum = t = NULL;     
  where = g95_current_locus;     
     
  seen_keyword = 0;   
   
  if (g95_match_char('(') == MATCH_NO)       
    return (sub_flag) ? MATCH_YES : MATCH_NO;         
         
  if (g95_match_char(')') == MATCH_YES) return MATCH_YES;    
  h = NULL;         
         
  for(;;) {        
    if (h == NULL)      
      h = t = g95_get_actual_arglist();   
    else {  
      t->next = g95_get_actual_arglist();     
      t = t->next;     
    }        
        
    if (sub_flag && g95_match_char('*') == MATCH_YES) {      
      x = g95_match_st_label(&lab, 0);
      if (x == MATCH_NO) g95_error("Expected alternate return label at %C");    
      if (x != MATCH_YES) goto cleanup;      
      
      t->u.label = lab;   
      t->type = ALT_RETURN;         
      goto next;     
    }        
        
    /* After the first keyword argument is seen, the following
     * arguments must also have keywords. */       
       
    if (seen_keyword) {      
      x = match_keyword_arg(t, h);          
          
      if (x == MATCH_ERROR) goto cleanup;    
      if (x == MATCH_NO) {   
	g95_error("Missing keyword name in actual argument list at %C");   
	goto cleanup;         
      } 
 
    } else {   /* See if we have the first keyword argument */       
       
      x = match_keyword_arg(t, h);  
      if (x == MATCH_YES) seen_keyword = 1;  
      if (x == MATCH_ERROR) goto cleanup;      
      
      if (x == MATCH_NO) {  /* Try for a non-keyword argument */
	x = match_actual_arg(&t->u.expr);    
	t->type = EXPR;     
     
	if (x == MATCH_ERROR) goto cleanup;   
	if (x == MATCH_NO) goto syntax;    
      }   
    }      
      
  next:   
    if (g95_match_char(')') == MATCH_YES) break;  
    if (g95_match_char(',') != MATCH_YES) goto syntax;      
  }     
     
  *argum = h;   
  return MATCH_YES;       
       
syntax:       
  g95_error("Syntax error in argument list at %C");    
    
cleanup:     
  g95_free_actual_arglist(h);   
  g95_current_locus = where;      
      
  return MATCH_ERROR;     
}     
     
     
  
  
/* g95_match_variable()-- Match a variable, ie something that can be
 * assigned to.  This starts as a symbol, can be a structure
 * component, array reference or substring.  If the symbol has not
 * been previously seen, we assume it is a variable. */    
    
match g95_match_variable(g95_expr **result, int decl_flag) {   
char name0[G95_MAX_SYMBOL_LEN+1];     
g95_symbol *s;   
match r;  
  
  r = g95_match_name(name0);    
  if (r != MATCH_YES) return r;        
        
  switch(get_rvalue_type(name0, &s)) {   
  case RVAL_VARIABLE:        
  case RVAL_UNKNOWN: 
    r = match_variable_part(s, result, decl_flag);      
    break;        
        
  default:       
    g95_error("Expected '%s' at %C to be a VARIABLE", name0);         
    r = MATCH_ERROR;    
  }     
     
  return r;  
}      
  
  
/* match_function_rest()-- Match the rest of a function call where the
 * first argument has already been matched. */

static match match_function_rest(g95_symbol *sym, g95_locus *where,
				 g95_expr *first, g95_expr **r) {        
g95_actual_arglist *real;
g95_expr *b;     
match s;    
    
  if (g95_add_function(&sym->attr, where) == FAILURE) {       
    g95_free_expr(first);        
    return MATCH_ERROR; 
  }      
      
  if (sym->result == NULL) sym->result = sym;      
      
  s = match_aa_rest(first, &real);         
  if (s == MATCH_ERROR) return s; 
  if (s == MATCH_NO) abort();       
       
  b = g95_get_expr();
  b->type = EXPR_FUNCTION;        
  b->where = *where;     
  b->symbol = sym;    
    
  b->value.function.actual = real;        
        
  *r = b;    
  return MATCH_YES;        
}  
  
  
          
          
/* match_ss_fc()-- Match a substring reference or function call,
 * figuring out what we have in the process.  The colon is mandatory
 * in the substring and cannot appear in the function argument list.
 * A keyword argument also indicates a function call.  A left paren is
 * next on the input. */      
      
static match match_ss_fc(g95_symbol *s, g95_expr **result) {         
char name0[G95_MAX_SYMBOL_LEN+1];
g95_expr *first;     
g95_locus where;        
match w;      
int i;       
       
  where = g95_current_locus;         
  g95_match_char('(');         
         
  g95_gobble_whitespace();      
  i = g95_peek_char() == ')' ||     
    (g95_match_name(name0) == MATCH_YES && g95_match_char('=') == MATCH_YES);         
         
  g95_current_locus = where; 
  if (i) return match_function_call(s, result);       
       
  g95_match_char('(');          
  g95_gobble_whitespace();     
     
  if (g95_peek_char() == ':')         
    return match_substring_rest(s, &where, NULL, result);       
       
  w = match_actual_arg(&first); 
  switch(w) {        
  case MATCH_YES:          
    break; 
 
  case MATCH_NO:  
    g95_error("Syntax error at %C");      
    w = MATCH_ERROR;        
        
    /* Fall through */     
  case MATCH_ERROR:        
    return w; 
  }     
     
  g95_gobble_whitespace();     
  w = (g95_peek_char() == ':')          
    ? match_substring_rest(s, &where, first, result)     
    : match_function_rest(s, &where, first, result);    
    
  return w;    
}       
       
       
 
 
/* g95_match_rvalue()-- Matches a variable name followed by anything
 * that might follow it-- array reference, argument list of a
 * function, etc. */          
          
match g95_match_rvalue(g95_expr **rslt) {          
char name[G95_MAX_SYMBOL_LEN+1];  
g95_symbol *symbol;       
g95_locus where;     
match r;      
     
  r = g95_match_name(name);          
  if (r != MATCH_YES) return r;        
        
  where = g95_current_locus;

  switch(get_rvalue_type(name, &symbol)) { 
  case RVAL_VARIABLE:       
    r = match_variable_part(symbol, rslt, 0);        
    break;   
   
  case RVAL_FUNCTION:     
    r = match_function_call(symbol, rslt);
    break; 
 
  case RVAL_DERIVED:  
    r = g95_match_structure_constructor(symbol, rslt);   
    break;        
        
  case RVAL_UNKNOWN:    
    g95_gobble_whitespace();  
    if (g95_peek_char() != '(')   
      r = match_variable_part(symbol, rslt, 0);       
    else
      r = match_ss_fc(symbol, rslt);  
  
    break;        
        
  case RVAL_SUBROUTINE:   
    g95_error("Expected '%s' at %C to be a VARIABLE", name);   
    /* Fall through */     
     
  case RVAL_ERROR:          
    r = MATCH_ERROR;        
    break;      
  }          
          
  return r;
}      
      
      
