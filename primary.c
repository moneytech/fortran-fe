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
      
      
   
   
/* check_digit()-- Given a character and a radix, see if the character
 * is a valid digit in that radix. */        
        
static int check_digit(int m, int radix) {  
int n;    
    
  switch(radix) {  
  case 2:       
    n = ('0' <= m && m <= '1'); 
    break;         
         
  case 8:          
    n = ('0' <= m && m <= '7');
    break;      
      
  case 10:  
    n = ('0' <= m && m <= '9');
    break;

  case 16:  
    n = ('0' <= m && m <= '9') || ('a' <= m && m <= 'f');        
    break;  
  
  default:
    g95_internal_error("check_digit(): bad radix");       
  }

  return n;          
} 
 
 
 
 
/* match_sym_complex_part()-- Match a real or imaginary part of a complex
 * constant that is a symbolic constant. */         
         
static match match_sym_complex_part(g95_expr **result) {     
char name[G95_MAX_SYMBOL_LEN+1];        
g95_symbol *s;   
g95_expr *d;     
match q;     
     
  q = g95_match_name(name);     
  if (q != MATCH_YES) return q;        
        
  if (g95_find_symbol(name, NULL, 1, &s) || s == NULL) return MATCH_NO;          
          
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
    d = g95_copy_expr(s->value);   
    break;       
       
  case BT_COMPLEX:
    d = g95_complex2real(s->value, s->value->ts.kind); 
    if (d == NULL) goto error;       
    break;          
          
  case BT_INTEGER:      
    d = g95_int2real(s->value, g95_default_real_kind());   
    if (d == NULL) goto error;          
    break;        
        
  default:     
    g95_internal_error("g95_match_sym_complex_part(): Bad type");     
  }       
       
  *result = d;     /* e is a scalar, real, constant expression */     
  return MATCH_YES;     
     
error:  
  g95_error("Error converting PARAMETER constant in complex constant at %C");    
  return MATCH_ERROR;     
}         
         
         
      
      
/* match_digits()-- Match the digit string part of an integer.
 * If the buffer is NULL, we just count characters for the second
 * pass.  Returns the number of characters matched, -1 for no match. */          
          
static int match_digits(int signflag, int radix, char *buffer) { 
locus old_loc;   
int length, f; 
 
  length = 0;         
  f = g95_next_char();         
         
  if (signflag && (f == '+' || f == '-')) {       
    if (buffer != NULL) *buffer++ = f;  
    f = g95_next_char();  
    length++;     
  }     
     
  if (!check_digit(f, radix)) return -1;          
          
  length++;  
  if (buffer != NULL) *buffer++ = f;      
      
  for(;;) {          
    old_loc = *g95_current_locus();   
    f = g95_next_char();    
    
    if (!check_digit(f, radix)) break;         
         
    if (buffer != NULL) *buffer++ = f;  
    length++;    
  }       
       
  g95_set_locus(&old_loc);          
          
  return length;        
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
   
   
     
     
/* match_structure_constructor()-- Match a structure constructor.  The
 * initial symbol has already been seen. */

static match match_structure_constructor(g95_symbol *s, g95_expr **result) {    
g95_constructor *head, *tail;     
g95_component *comp;    
g95_expr *q;   
locus where;      
match m;    
    
  head = tail = NULL;       
       
  if (g95_match_char('(') != MATCH_YES) goto syntax;      
      
  where = *g95_current_locus();

  g95_find_component(s, NULL);

  for(comp=s->components; comp; comp=comp->next) {          
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
      
  q = g95_get_expr();    
    
  q->type = EXPR_STRUCTURE;

  q->ts.type = BT_DERIVED;    
  q->ts.derived = s;        
  q->where = where;         
         
  q->value.constructor = head;       
       
  *result = q; 
  return MATCH_YES;    
  
syntax: 
  g95_error("Syntax error in structure constructor at %C");      
      
cleanup:    
  g95_free_constructor(head);   
  return MATCH_ERROR;  
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
       
static match match_charkind_name(char *name) {   
locus old_loc;      
char u, peek; 
int len;       
       
  g95_gobble_whitespace();     
  u = g95_next_char();   
  if (!isalpha(u)) return MATCH_NO;       
       
  *name++ = u;
  len = 1;          
          
  for(;;) { 
    old_loc = *g95_current_locus();
    u = g95_next_char(); 
 
    if (u == '_') {     
      peek = g95_peek_char();   
         
      if (peek == '\'' || peek == '\"') {       
	g95_set_locus(&old_loc);        
	*name = '\0';   
	return MATCH_YES;      
      }      
    }   
   
    if (!isalnum(u) && u != '_' && g95_option.dollar && u != '$') break;  
  
    *name++ = u;     
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
 
 
    
    
/* match_boz_constant()-- Match a binary, octal or hexadecimal
 * constant that can be found in a DATA statement */     
     
static match match_boz_constant(g95_expr **result) {        
int radix, delim, length;       
locus old_loc;
char *buffer;        
g95_expr *q;      
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
    
  q = g95_convert_integer(buffer, g95_default_integer_kind(), radix, 
			  g95_current_locus());       
       
  if (g95_range_check(q) != ARITH_OK) {         
    g95_error("Integer too big for default integer kind at %C");     
     
    g95_free_expr(q); 
    return MATCH_ERROR;      
  }

  *result = q;   
  return MATCH_YES;         
         
backup: 
  g95_set_locus(&old_loc);    
  return MATCH_NO;
}  
  
  
      
      
/* match_kind_param()-- Matches a kind-parameter expression, which is
 * either a named symbolic constant or a nonnegative integer constant.
 * If successful, sets the kind value to the correct integer.  */          
          
static match match_kind_param(int *kind) {   
char name[G95_MAX_SYMBOL_LEN+1];         
g95_symbol *symb;     
char *n; 
match s;

  s = g95_match_small_literal_int(kind);     
  if (s != MATCH_NO) return s;     
     
  s = g95_match_name(name);  
  if (s != MATCH_YES) return s;  
  
  if (g95_find_symbol(name, NULL, 1, &symb)) return MATCH_ERROR; 
 
  if (symb == NULL) return MATCH_NO;         
         
  if (symb->attr.flavor != FL_PARAMETER) return MATCH_NO;  
  
  n = g95_extract_int(symb->value, kind);       
  if (n != NULL) return MATCH_NO;         
         
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
match g;     
     
  if (g95_match_char('_') != MATCH_YES) return -2;          
          
  g = match_kind_param(&kind);       
  if (g == MATCH_NO) g95_error("Missing kind-parameter at %C");  
  
  return (g == MATCH_YES) ? kind : -1;       
}     
     
     
      
      
/* match_substring()-- Match a substring reference */      
      
static match match_substring(g95_charlen *cl, int init, g95_ref **result) {  
g95_expr *start, *end;       
locus old_loc;
g95_ref *reference;          
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
    reference = NULL; 
  else {   
    reference = g95_get_ref(); 
 
    reference->type = REF_SUBSTRING;
    reference->u.ss.start = start;        
    reference->u.ss.end = end;  
    reference->u.ss.length = cl;  
  }       
       
  *result = reference;  
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


          
          
/* match_logical_constant().  Match a .true. or .false. */       
      
static match match_logical_constant(g95_expr **result) { 
static mstring logical_ops[] = {        
  minit(".false.", 0),       
  minit(".true.", 1),    
  minit(NULL, -1) };    
    
g95_expr *w;  
int v, kind;       
       
  v = g95_match_strings(logical_ops);  
  if (v == -1) return MATCH_NO;          
          
  kind = get_kind();       
  if (kind == -1) return MATCH_ERROR;      
  if (kind == -2) kind = g95_default_logical_kind();    
    
  if (g95_validate_kind(BT_LOGICAL, kind) == -1)       
    g95_error("Bad kind for logical constant at %C"); 
 
  w = g95_get_expr();  
  
  w->type = EXPR_CONSTANT;  
  w->value.logical = v;    
  w->ts.type = BT_LOGICAL;      
  w->ts.kind = kind;         
  w->where = *g95_current_locus();      
      
  *result = w;   
  return MATCH_YES;      
}     
     
     
  
  
/* match_varspec()-- Match any additional specifications associated
 * with the current variable like member references or substrings. */

static match match_varspec(g95_expr *primary, int equiv_flag) { 
char name[G95_MAX_SYMBOL_LEN+1];        
g95_ref *substring, *tail;          
g95_component *component;         
g95_symbol *symb;  
match k;    
    
  tail = NULL;    
    
  if (primary->symbol->attr.dimension ||       
      (equiv_flag && g95_peek_char() == '(')) {  
  
    tail = extend_ref(primary, tail);    
    tail->type = REF_ARRAY;    
    
    k = g95_match_array_ref(&tail->u.ar, primary->symbol->as, equiv_flag);     
    if (k != MATCH_YES) return k;   
  }  
  
  symb = primary->symbol;          
  primary->ts = symb->ts;       
       
  if (symb->ts.type != BT_DERIVED ||      
      g95_match_char('%') != MATCH_YES) goto check_substring;     
     
  symb = symb->ts.derived;   
   
  for(;;) {          
    k = g95_match_name(name);  
    if (k == MATCH_NO) g95_error("Expected structure component name at %C");     
    if (k != MATCH_YES) return MATCH_ERROR;         
         
    component = g95_find_component(symb, name);        
    if (component == NULL) return MATCH_ERROR;        
        
    tail = extend_ref(primary, tail);      
    tail->type = REF_COMPONENT;    
    
    tail->u.c.component = component;  
    tail->u.c.sym = symb;

    primary->ts = component->ts;          
          
    if (component->as != NULL) {        
      tail = extend_ref(primary, tail); 
      tail->type = REF_ARRAY;        
        
      k = g95_match_array_ref(&tail->u.ar, component->as, equiv_flag);        
      if (k != MATCH_YES) return k;     
    }        
        
    if (component->ts.type != BT_DERIVED || g95_match_char('%') != MATCH_YES)   
      break; 
 
    symb = component->ts.derived;     
  }    
    
check_substring:       
  if (primary->ts.type == BT_CHARACTER) {      
    switch(match_substring(primary->ts.cl, equiv_flag, &substring)) {         
    case MATCH_YES:   
      if (tail == NULL)        
	primary->ref = substring;     
      else     
	tail->next = substring;         
         
      if (primary->type == EXPR_CONSTANT)          
	primary->type = EXPR_SUBSTRING;    
    
      break;        
        
    case MATCH_NO:          
      break;          
          
    case MATCH_ERROR:          
      return MATCH_ERROR;  
    }       
  }    
    
  return MATCH_YES;       
}  
  
  
         
         
/* g95_match_variable()-- Match a variable, ie something that can be
 * assigned to.  This starts as a symbol, can be a structure
 * component, array reference or substring.  It can be a function if
 * the function doesn't have a separate RESULT variable.  If the
 * symbol has not been previously seen, we assume it is a variable. */         
         
match g95_match_variable(g95_expr **result, int equiv_flag) {         
g95_symbol *symb;        
g95_expr *expr;     
locus where;      
match p;          
          
  p = g95_match_symbol(&symb, 1);     
  if (p != MATCH_YES) return p;        
  where = *g95_current_locus();

  switch(symb->attr.flavor) { 
  case FL_VARIABLE:         
    break; 
 
  case FL_UNKNOWN:
    if (g95_add_flavor(&symb->attr, FL_VARIABLE, NULL) == FAILURE)          
      return MATCH_ERROR;    
    
    /* Special case for derived type variables that get their types
     * via an IMPLICIT statement.  This can't wait for the resolution
     * phase. */

    if (g95_peek_char() == '%' &&          
	g95_get_default_type(symb, symb->ns)->type == BT_DERIVED)      
      g95_set_default_type(symb, 0, symb->ns);  
  
    break;       
       
  case FL_PROCEDURE:  /* Check for a nonrecursive function result */       
    if (symb->attr.function && (symb->result == symb || symb->attr.entry)) {     
     
      /* If a function result is a derived type, then the derived
       * type may still have to be resolved. */

      if (symb->ts.type == BT_DERIVED &&       
	  g95_use_derived(symb->ts.derived) == NULL) return MATCH_ERROR;     
     
      break;   
    }         
         
    /* Fall through to error */ 
 
  default:          
    g95_error("Expected VARIABLE at %C");      
    return MATCH_ERROR;       
  } 
 
  expr = g95_get_expr();          
          
  expr->type = EXPR_VARIABLE;         
  expr->symbol = symb;     
  expr->ts = symb->ts;      
  expr->where = where;    
    
/* Now see if we have to do more */          
          
  p = match_varspec(expr, equiv_flag);       
  if (p != MATCH_YES) {  
    g95_free_expr(expr);  
    return p;       
  }     
     
  *result = expr; 
  return MATCH_YES;      
}     
     
  
  
/* g95_match_rvalue()-- Matches a variable name followed by anything
 * that might follow it-- array reference, argument list of a
 * function, etc. */ 
 
match g95_match_rvalue(g95_expr **result) {         
g95_actual_arglist *actual_arglist;       
char name[G95_MAX_SYMBOL_LEN+1];         
g95_state_data *st;        
g95_symbol *symb;      
locus where;          
g95_expr *n;   
match d;         
int a;       
       
  d = g95_match_name(name);        
  if (d != MATCH_YES) return d;     
     
  if (g95_find_state(COMP_INTERFACE) == SUCCESS)    
    a = g95_get_symbol(name, NULL, &symb);      
  else    
    a = g95_get_ha_symbol(name, &symb);   
   
  if (a) return MATCH_ERROR;

  n = NULL;
  where = *g95_current_locus();        
        
  if (symb->attr.function && symb->result == symb &&     
      (g95_current_ns->proc_name == symb ||   
       (g95_current_ns->parent != NULL &&    
	g95_current_ns->parent->proc_name == symb))) goto variable;         
         
  if (symb->attr.function || symb->attr.external || symb->attr.intrinsic) 
    goto function0; 
 
  if (symb->attr.generic) goto generic_function;    
    
  switch(symb->attr.flavor) { 
  case FL_VARIABLE:         
  variable:      
    if (symb->ts.type == BT_UNKNOWN && g95_peek_char() == '%' &&          
	g95_get_default_type(symb, symb->ns)->type == BT_DERIVED)       
      g95_set_default_type(symb, 0, symb->ns);      
      
    n = g95_get_expr();        
        
    n->type = EXPR_VARIABLE;     
    n->symbol = symb;  
  
    d = match_varspec(n, 0); 
    break;      
      
  case FL_PARAMETER:     
    if (symb->value->type != EXPR_ARRAY)   
      n = g95_copy_expr(symb->value);  
    else {    
      n = g95_get_expr();         
      n->type = EXPR_VARIABLE;         
    } 
 
    n->symbol = symb;     
    d = match_varspec(n, 0);  
    break;      
      
  case FL_DERIVED:     
    symb = g95_use_derived(symb);        
    if (symb == NULL)       
      d = MATCH_ERROR;
    else {          
      d = match_structure_constructor(symb, &n);
      if (d == MATCH_YES) n->symbol = symb; 
    }          
          
    break;       
       
/* If we're here, then the name is known to be the name of a
 * procedure, yet it is not sure to be the name of a function. */      
      
  case FL_PROCEDURE:  
    if (symb->attr.subroutine) {  
      g95_error("Unexpected use of subroutine name '%s' at %C", symb->name);
      d = MATCH_ERROR;       
      break;          
    }        
        
/* At this point, the name has to be a non-statement function.  If the
 * name is the same as the current function being compiled, then we
 * have a variable reference (to the function result) if the name is
 * non-recursive. */      
      
    st = g95_enclosing_unit(NULL); 
 
    if (st != NULL && st->state == COMP_FUNCTION && st->sym == symb &&  
	!symb->attr.recursive) {   
      n = g95_get_expr(); 
      n->symbol = symb;      
      n->type = EXPR_VARIABLE;   
         
      d = match_varspec(n, 0);       
      break;      
    }     
     
    /* Match a function reference */ 
 
  function0:
    d = g95_match_actual_arglist(0, &actual_arglist);      
    if (d == MATCH_NO) {    
      if (symb->attr.proc == PROC_ST_FUNCTION)       
	g95_error("Statement function '%s' requires argument list at %C",        
		  symb->name);     
      else       
	g95_error("Function '%s' requires an argument list at %C",  
		  symb->name);    
    
      d = MATCH_ERROR;      
      break;    
    }    
    
    if (d != MATCH_YES) {          
      d = MATCH_ERROR;
      break;      
    }        
        
    g95_get_symbol(name, NULL, &symb);

    n = g95_get_expr();    
    n->symbol = symb;   
    n->type = EXPR_FUNCTION;
    n->value.function.actual = actual_arglist;
    n->where = *g95_current_locus();      
      
    if (symb->as != NULL) n->rank = symb->as->rank;       
       
    if (!symb->attr.function && g95_add_function(&symb->attr, NULL) == FAILURE) {  
      d = MATCH_ERROR;        
      break;         
    }    
    
    d = MATCH_YES;        
    break;        
        
  case FL_UNKNOWN:   
   
    /* Special case for derived type variables that get their types
     * via an IMPLICIT statement.  This can't wait for the resolution
     * phase. */      
      
    if (g95_peek_char() == '%' &&    
	g95_get_default_type(symb, symb->ns)->type == BT_DERIVED)   
      g95_set_default_type(symb, 0, symb->ns);  
  
/* If the symbol has a dimension attribute, the expression is a variable */ 
 
    if (symb->attr.dimension) {    
      if (g95_add_flavor(&symb->attr, FL_VARIABLE, NULL) == FAILURE) { 
	d = MATCH_ERROR;      
	break;     
      }

      n = g95_get_expr();   
      n->symbol = symb; 
      n->type = EXPR_VARIABLE;   
      d = match_varspec(n, 0);      
      break;         
    }          
          
/* Name is not an array, so we peek to see if a '(' implies a function
 * call or a substring reference.  Otherwise the variable is just a
 * scalar. */  
  
    g95_gobble_whitespace();       
    if (g95_peek_char() != '(') {   /* Assume a scalar variable */   
      n = g95_get_expr();       
      n->symbol = symb;      
      n->type = EXPR_VARIABLE;          
          
      if (g95_add_flavor(&symb->attr, FL_VARIABLE, NULL) == FAILURE) {    
	d = MATCH_ERROR;    
	break;   
      }

      n->ts = symb->ts;     
      d = match_varspec(n, 0);     
      break;      
    }  
  
/* See if this could possibly be a substring reference of a name that
 * we're not sure is a variable yet. */          
          
    n = g95_get_expr(); 
    n->symbol = symb;     
     
    if ((symb->ts.type == BT_UNKNOWN || symb->ts.type == BT_CHARACTER) &&
	match_substring(symb->ts.cl, 0, &n->ref) == MATCH_YES) {  
  
      n->type = EXPR_VARIABLE;      
      
      if (symb->attr.flavor != FL_VARIABLE &&         
	  g95_add_flavor(&symb->attr, FL_VARIABLE, NULL) == FAILURE) {      
	d = MATCH_ERROR;         
	break; 
      }       
       
      if (symb->ts.type == BT_UNKNOWN &&        
	  g95_set_default_type(symb, 1, NULL) == FAILURE) {     
	d = MATCH_ERROR;   
	break;     
      }      
      
      n->ts = symb->ts;        
      d = MATCH_YES;     
      break; 
    } 
 
/* Give up, assume we have a function */ 
 
    g95_get_symbol(name, NULL, &symb);   /* Can't fail */          
    n->type = EXPR_FUNCTION;        
        
    if (!symb->attr.function && g95_add_function(&symb->attr, NULL) == FAILURE) {   
      d = MATCH_ERROR;  
      break;     
    }      
      
    symb->result = symb;         
         
    d = g95_match_actual_arglist(0, &n->value.function.actual);         
    if (d == MATCH_NO)    
      g95_error("Missing argument list in function '%s' at %C", symb->name);

    if (d != MATCH_YES) {   
      d = MATCH_ERROR;  
      break;  
    } 
 
    /* If our new function returns a character, array or structure
     * type, it might have subsequent references. */

    d = match_varspec(n, 0);  
    if (d == MATCH_NO) d = MATCH_YES;      
      
    break;

  generic_function: 
    g95_get_symbol(name, NULL, &symb);   /* Can't fail */     
     
    n = g95_get_expr(); 
    n->symbol = symb;     
    n->type = EXPR_FUNCTION;   
   
    d = g95_match_actual_arglist(0, &n->value.function.actual);          
    break;       
       
  default:      
    g95_error("Symbol at %C is not appropriate for an expression");         
    return MATCH_ERROR;      
  }          
          
  if (d == MATCH_YES) {
    n->where = where;
    *result = n;      
  } else       
    g95_free_expr(n);         
         
  return d;       
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
       
symbol_attribute g95_variable_attr(g95_expr *expr, g95_typespec *ts) {       
int dimension, pointer, target;         
symbol_attribute attr;          
g95_ref *reference;        
        
  if (expr->type != EXPR_VARIABLE)         
    g95_internal_error("g95_variable_attr(): Expression isn't a variable");         
         
  reference = expr->ref;    
  attr = expr->symbol->attr;

  dimension = attr.dimension;  
  pointer = attr.pointer;  
  
  target = attr.target;     
  if (pointer) target = 1;         
         
  if (ts != NULL && expr->ts.type == BT_UNKNOWN) *ts = expr->symbol->ts;         
         
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
      g95_get_component_attr(&attr, reference->u.c.component);         
      if (ts != NULL) *ts = reference->u.c.component->ts;

      pointer = reference->u.c.component->pointer;   
      if (pointer) target = 1;  
  
      break; 
 
    case REF_SUBSTRING:      
      pointer = 0;
      break;    
    }

  attr.dimension = dimension; 
  attr.pointer = pointer;     
  attr.target = target;

  return attr;       
}        
        
        
 
 
/* g95_expr_attr()-- Return the attribute from a general expression */       
       
symbol_attribute g95_expr_attr(g95_expr *k) {  
symbol_attribute attr;        
        
  switch(k->type) {      
  case EXPR_VARIABLE:     
    attr = g95_variable_attr(k, NULL);          
    break;   
   
  case EXPR_FUNCTION:  
    g95_clear_attr(&attr);          
    attr = k->symbol->result->attr;    
    
    /* NULL() returns pointers.  May have to take care of this here */         
         
    break;   
   
  default:          
    g95_clear_attr(&attr);         
    break;
  }          
          
  return attr;    
}  
  
  
  
  
/* match_string_constant()-- See if the current input matches a
 * character constant.  Lots of contortions have to be done to match
 * the kind parameter which comes before the actual string.  The main
 * consideration is that we don't want to error out too quickly.  For
 * example, we don't actually do any validation of the kinds until we
 * have actually seen a legal delimiter.  Using match_kind_param()
 * generates errors too quickly. */         
         
static match match_string_constant(g95_expr **result) {         
char *n, name[G95_MAX_SYMBOL_LEN+1]; 
int i, c, kind, length, delimiter;
locus old_locus, start_locus;       
g95_symbol *symb;  
g95_expr *v;  
char *t;        
match s;  
  
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
    
    s = match_charkind_name(name);   
    if (s != MATCH_YES) goto no_match;     
     
    if (g95_find_symbol(name, NULL, 1, &symb) || symb == NULL ||   
	symb->attr.flavor != FL_PARAMETER) goto no_match;   
   
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
    t = g95_extract_int(symb->value, &kind);       
    if (t != NULL) {
      g95_error(t);       
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
        
  v = g95_char_expr(length, kind, &start_locus);       
  n = v->value.character.string;      
      
  g95_set_locus(&start_locus); 
  g95_next_char();              /* Skip delimiter */       
       
  for(i=0; i<length; i++)  
    *n++ = g95_next_string_char(delimiter);       
       
  *n = '\0';     /* C-style string is for development/debug purposes */         
         
  if (g95_next_string_char(delimiter) != -1) 
    g95_internal_error("match_string_constant(): Delimiter not found");  
  
  if (match_substring(NULL, 0, &v->ref) != MATCH_NO) v->type = EXPR_SUBSTRING;          
          
  *result = v;         
         
  return MATCH_YES;  
  
no_match:       
  g95_set_locus(&old_locus);     
  return MATCH_NO; 
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
char *e, w, exp_char, *buffer;     
locus old_loc;          
          
  old_loc = *g95_current_locus();   
  g95_gobble_whitespace();     
     
  seen_dp = 0;   
  seen_digits = 0;      
  count = 0;  
  exp_char = ' '; 
 
  w = g95_next_char();         
  if (w == '-' || w == '+') {      
    w = g95_next_char();        
    count++;          
  }       
       
  for(;; w=g95_next_char(), count++) {         
    if (w == '.') {          
      if (seen_dp) goto no_match;  
      seen_dp = 1;     
      continue;
    }    
    
    if (isdigit(w)) {      
      seen_digits = 1;          
      continue; 
    }    
    
    break;  
  }         
         
  if (!seen_digits || (w != 'd' && w != 'e')) goto done; 
  exp_char = w;   
   
/* scan exponent */     
     
  w = g95_next_char();    
  count++; 
 
  if (w == '+' || w == '-') {  /* optional sign */   
    w = g95_next_char();  
    count++;         
  }     
     
  if (!isdigit(w)) {
    g95_error("Missing exponent in real number at %C");         
    return MATCH_ERROR;   
  } 
 
  while(isdigit(w)) {   
    w = g95_next_char();        
    count++;       
  }          
          
done: 
  if (!seen_digits) goto no_match;       
       
/* Convert the number */    
    
  g95_set_locus(&old_loc);  
  g95_gobble_whitespace();     
     
  buffer = alloca(count+1);
  memset(buffer, '\0', count+1);       
       
  e = buffer;  
  while(count>0) {     
    w = g95_next_char();    
    if (w == 'd') w = 'e';   /* Hack for mpf_init_set_str() */         
    *e++ = w;    
    count--;         
  }     
     
  *e = '\0';   
   
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
     
  *result = g95_convert_real(buffer, kind, g95_current_locus());      
  return MATCH_YES;

no_match:       
  g95_set_locus(&old_loc);        
  return MATCH_NO;         
}       
       
       
  
  
/* match_real_constant()-- Match a real constant of some sort. */          
          
static match match_real_constant(g95_expr **result, int signflag) {  
int kind, y, count, seen_dp, seen_digits, exp_char;    
locus old_loc, temp_loc;
char *j, *buffer;
g95_expr *z;       
       
  old_loc = *g95_current_locus();    
  g95_gobble_whitespace();          
          
  z = NULL;       
  buffer = NULL;   
   
  count = 0;      
  seen_dp = 0; 
  seen_digits = 0;         
  exp_char = ' '; 
 
  y = g95_next_char();     
  if (signflag && (y == '+' || y == '-')) {    
    y = g95_next_char();       
    count++;  
  }   
   
/* Scan significand */       
       
  for(;; y=g95_next_char(), count++) {      
    if (y == '.') {         
      if (seen_dp) goto done;       
       
      /* Check to see if "." goes with a following operator like ".eq." */    
    
      temp_loc = *g95_current_locus();
      y = g95_next_char();   
   
      if (y == 'e' || y == 'd' || y == 'q') {      
	y = g95_next_char();  
	if (y == '.') goto done;   /* Operator named .e. or .d. */     
      }  
  
      if (isalpha(y)) goto done;   /* Distinguish 1.e9 from 1.eq.2 */   
   
      g95_set_locus(&temp_loc);         
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
  count++;      
      
  if (y == '+' || y == '-') {  /* optional sign */ 
    y = g95_next_char();       
    count++;        
  }        
        
  if (!isdigit(y)) {  
    if (!seen_digits) {    
      g95_set_locus(&old_loc);        
      return MATCH_NO;   /* ".e" can be something else */       
    }          
          
    g95_error("Missing exponent in real number at %C");  
    return MATCH_ERROR;        
  } 
 
  while(isdigit(y)) {      
    y = g95_next_char();   
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
  
  j = buffer;      
  while(count>0) {         
    *j = g95_next_char();     
    if (*j == 'd' || *j == 'q') *j = 'e';   /* Hack for mpf_init_set_str() */        
    j++;   
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
      
  z = g95_convert_real(buffer, kind, g95_current_locus());      
      
  switch(g95_range_check(z)) {       
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
        
  *result = z;         
  return MATCH_YES;       
       
cleanup:
  g95_free_expr(z);      
  return MATCH_ERROR;     
}      
      
      
      
      
/* match_integer_constant()-- Match an integer (digit string and
 * optional kind). */     
     
static match match_integer_constant(g95_expr **result, int signflag) {        
int length, kind;    
locus old_loc;  
char *buffer;  
g95_expr *j;  
  
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
     
  j = g95_convert_integer(buffer, kind, 10, g95_current_locus());    
    
  if (g95_range_check(j) != ARITH_OK) {      
    g95_error("Integer too big for its kind at %C");        
        
    g95_free_expr(j);    
    return MATCH_ERROR;      
  } 
 
  *result = j;        
  return MATCH_YES;   
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
g95_expr *n; 
int s;    
    
  where = *g95_current_locus();    
    
  switch(g95_match_name(name)) {         
  case MATCH_ERROR:   
    return MATCH_ERROR; 
 
  case MATCH_NO:         
    break; 
 
  case MATCH_YES:      
    w = *g95_current_locus();    
    g95_gobble_whitespace();    
    s = g95_next_char();          
    g95_set_locus(&w);        
        
    if (s != ',' && s != ')') break;      
      
    if (g95_find_symbol(name, NULL, 1, &sym)) break;        
    /* Handle error elsewhere */ 
 
    /* Eliminate a couple of common cases where we know we don't have
     * a function argument. */          
          
    if (sym == NULL)  
      g95_get_symbol(name, NULL, &sym);     
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
     
    n = g95_get_expr();              /* Leave it unknown for now */
    n->symbol = sym;   
    n->type = EXPR_VARIABLE;     
    n->ts.type = BT_PROCEDURE;    
    n->where = where;  
  
    *result = n;    
    return MATCH_YES;      
  }

  g95_set_locus(&where); 
  return g95_match_expr(result);  
}      
      
      


/* match_keyword_arg()-- Match a keyword argument. */     
     
static match match_keyword_arg(g95_actual_arglist *actual,   
			       g95_actual_arglist *base) {          
char name[G95_MAX_SYMBOL_LEN+1];  
g95_actual_arglist *q;
locus name_locus; 
match b;         
         
  name_locus = *g95_current_locus();
  b = g95_match_name(name); 
 
  if (b != MATCH_YES) goto cleanup;
  if (g95_match_char('=') != MATCH_YES) {  
    b = MATCH_NO;
    goto cleanup;        
  }         
         
  b = match_actual_arg(&actual->u.expr);      
  if (b != MATCH_YES) goto cleanup;    
  actual->type = EXPR;          
          
  /* Make sure this name has not appeared yet */    
    
  if (name[0] != '\0') {        
    for(q=base; q; q=q->next)  
      if (strcmp(q->name, name) == 0) {         
	g95_error("Keyword '%s' at %C has already appeared in the current "    
		  "argument list", name);     
	return MATCH_ERROR;;
      }   
  }    
    
  strcpy(actual->name, name);        
  return MATCH_YES;  
  
cleanup:       
  g95_set_locus(&name_locus);          
  return b;       
}  
  
  
 
 
/* match_complex_part()-- Match a real or imaginary part of a complex number */        
        
static match match_complex_part(g95_expr **result) {
match g;       
       
  g = match_sym_complex_part(result);  
  if (g != MATCH_NO) return g; 
 
  return match_const_complex_part(result);   
}   
   
   
   
   
/* g95_match_actual_arglist()-- Matches an actual argument list of a
 * function or subroutine, from the opening parenthesis to the closing
 * parenthesis.  The argument list is assumed to allow keyword
 * arguments because we don't know if the symbol associated with the
 * procedure has an implicit interface or not.  We make sure keywords
 * are unique. */     
     
match g95_match_actual_arglist(int sub_flag, g95_actual_arglist **argp) {
g95_actual_arglist *head, *tail;    
int seen_keyword;    
g95_st_label *label;
locus old_loc;
match l; 
 
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
      l = g95_match_st_label(&label, 0);       
      if (l == MATCH_NO) g95_error("Expected alternate return label at %C"); 
      if (l != MATCH_YES) goto cleanup;    
    
      tail->u.label = label;
      tail->type = ALT_RETURN;    
      goto next;      
    } 
 
    /* After the first keyword argument is seen, the following
     * arguments must also have keywords. */   
   
    if (seen_keyword) {        
      l = match_keyword_arg(tail, head);

      if (l == MATCH_ERROR) goto cleanup;       
      if (l == MATCH_NO) {  
	g95_error("Missing keyword name in actual argument list at %C");
	goto cleanup;     
      }   
   
    } else {   /* See if we have the first keyword argument */    
    
      l = match_keyword_arg(tail, head);     
      if (l == MATCH_YES) seen_keyword = 1;    
      if (l == MATCH_ERROR) goto cleanup;     
     
      if (l == MATCH_NO) {  /* Try for a non-keyword argument */         
	l = match_actual_arg(&tail->u.expr); 
	tail->type = EXPR;   
   
	if (l == MATCH_ERROR) goto cleanup;          
	if (l == MATCH_NO) goto syntax;         
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
  
  
 
 
/* match_complex_constant()-- Try to match a complex constant */    
    
static match match_complex_constant(g95_expr **result) {          
g95_expr *c, *real, *imag;         
g95_error_buf old_error;      
g95_typespec target;   
locus old_loc;   
int kind;   
match s;   
   
  old_loc = *g95_current_locus();      
  real = imag = c = NULL;          
          
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
         
  kind = g95_kind_max(real, imag);     
  target.type = BT_REAL;         
  target.kind = kind;    
    
  if (kind != real->ts.kind) g95_convert_type(real, &target, 2);        
  if (kind != imag->ts.kind) g95_convert_type(imag, &target, 2);     
     
  c = g95_convert_complex(real, imag, kind);          
  c->where = *g95_current_locus(); 
 
  g95_free_expr(real); 
  g95_free_expr(imag);   
   
  *result = c;        
  return MATCH_YES;  
  
syntax:   
  g95_error("Syntax error in COMPLEX constant at %C");    
  s = MATCH_ERROR;         
         
cleanup: 
  g95_free_expr(c);
  g95_free_expr(real);        
  g95_free_expr(imag);   
  g95_set_locus(&old_loc);    
    
  return s;       
}  
  
  
       
       
/* g95_match_literal_constant()-- Match constants in any of several
 * forms.  Returns nonzero for a match, zero for no match. */  
  
match g95_match_literal_constant(g95_expr **result, int signflag) { 
match t;      
      
  t = match_complex_constant(result); 
  if (t != MATCH_NO) return t;         
         
  t = match_string_constant(result); 
  if (t != MATCH_NO) return t;   
   
  t = match_boz_constant(result);        
  if (t != MATCH_NO) return t;       
       
  t = match_real_constant(result, signflag);       
  if (t != MATCH_NO) return t; 
 
  t = match_integer_constant(result, signflag);        
  if (t != MATCH_NO) return t;      
      
  t = match_logical_constant(result);       
  if (t != MATCH_NO) return t; 
 
  return MATCH_NO;     
} 
 
 
