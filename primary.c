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
    
static int check_digit(int y, int radix) {      
int k;   
   
  switch(radix) {
  case 2:  
    k = ('0' <= y && y <= '1');         
    break;    
    
  case 8:     
    k = ('0' <= y && y <= '7');   
    break;       
       
  case 10: 
    k = ('0' <= y && y <= '9');          
    break; 
 
  case 16:
    k = ('0' <= y && y <= '9') || ('a' <= y && y <= 'f');      
    break;   
   
  default:
    g95_internal_error("check_digit(): bad radix");
  }    
    
  return k;         
}  
  
  
      
      
/* match_kind_param()-- Matches a kind-parameter expression, which is
 * either a named symbolic constant or a nonnegative integer constant.
 * If successful, sets the kind value to the correct integer.  */        
        
static match match_kind_param(int *k0) {
char nam[G95_MAX_SYMBOL_LEN+1];      
g95_symbol *symbol;         
char *p; 
match d;  
  
  d = g95_match_small_literal_int(k0);         
  if (d != MATCH_NO) return d;     
     
  d = g95_match_name(nam);     
  if (d != MATCH_YES) return d;          
          
  if (g95_find_symbol(nam, NULL, 1, &symbol)) return MATCH_ERROR;       
       
  if (symbol == NULL) return MATCH_NO;         
         
  if (symbol->attr.flavor != FL_PARAMETER) return MATCH_NO;    
    
  p = g95_extract_int(symbol->value, k0);  
  if (p != NULL) return MATCH_NO;         
         
  if (*k0 < 0) return MATCH_NO;    
    
  return MATCH_YES;       
}       
       
       
    
    
/* match_substring()-- Match a substring reference */   
   
static match match_substring(g95_charlen *charlen, int i, g95_ref **rslt) {
g95_expr *sta, *end;      
locus o;          
g95_ref *reference;     
match q;      
      
  sta = NULL;         
  end = NULL;   
   
  o = *g95_current_locus();       
       
  q = g95_match_char('(');      
  if (q != MATCH_YES) return MATCH_NO;        
        
  if (g95_match_char(':') != MATCH_YES) {   
    if (i)        
      q = g95_match_init_expr(&sta);        
    else       
      q = g95_match_expr(&sta);     
     
    if (q != MATCH_YES) {   
      q = MATCH_NO;         
      goto cleanup;   
    }    
    
    q = g95_match_char(':');      
    if (q != MATCH_YES) goto cleanup;      
  }        
        
  if (g95_match_char(')') != MATCH_YES) {       
    if (i) 
      q = g95_match_init_expr(&end);        
    else
      q = g95_match_expr(&end);    
    
    if (q == MATCH_NO) goto syntax; 
    if (q == MATCH_ERROR) goto cleanup;       
       
    q = g95_match_char(')');  
    if (q == MATCH_NO) goto syntax;        
  }          
          
/* Optimize away the (:) reference */          
          
  if (sta == NULL && end == NULL) 
    reference = NULL;         
  else {          
    reference = g95_get_ref();         
         
    reference->type = REF_SUBSTRING;         
    reference->u.ss.start = sta;        
    reference->u.ss.end = end;         
    reference->u.ss.length = charlen;       
  } 
 
  *rslt = reference;     
  return MATCH_YES;          
          
syntax:    
  g95_error("Syntax error in SUBSTRING specification at %C");     
  q = MATCH_ERROR;     
     
cleanup:        
  g95_free_expr(sta);    
  g95_free_expr(end);  
  
  g95_set_locus(&o);    
  return q;
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
    
symbol_attribute g95_variable_attr(g95_expr *exp, g95_typespec *t) {         
int dimension, pointer, target;          
symbol_attribute attr;         
g95_ref *ref;  
  
  if (exp->type != EXPR_VARIABLE)       
    g95_internal_error("g95_variable_attr(): Expression isn't a variable");         
         
  ref = exp->ref; 
  attr = exp->symbol->attr;      
      
  dimension = attr.dimension;  
  pointer = attr.pointer;   
   
  target = attr.target;         
  if (pointer) target = 1;      
      
  if (t != NULL && exp->ts.type == BT_UNKNOWN) *t = exp->symbol->ts;

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
      g95_get_component_attr(&attr, ref->u.c.component);        
      if (t != NULL) *t = ref->u.c.component->ts;   
   
      pointer = ref->u.c.component->pointer;    
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
   
   
          
          
/* get_kind()-- Gets a trailing kind-specification for non-character
 * variables.  Normally returns the integer kind value or
 *   -1   if an error was generated
 *   -2   if no kind was found
 */         
         
static int get_kind(void) {    
int knd;          
match h;       
       
  if (g95_match_char('_') != MATCH_YES) return -2;      
      
  h = match_kind_param(&knd);    
  if (h == MATCH_NO) g95_error("Missing kind-parameter at %C");         
         
  return (h == MATCH_YES) ? knd : -1;       
}      
      
      
      
      
/* match_actual_arg()-- match a single actual argument value.  An
 * actual argument is usually an expression, but can also be a
 * procedure name.  If the argument is a single name, it is not always
 * possible to tell whether the name is a dummy procedure or not.  We
 * treat these cases by creating an argument that looks like a dummy
 * procedure and fixing things later during resolution. */     
     
static match match_actual_arg(g95_expr **rslt) {       
char name[G95_MAX_SYMBOL_LEN+1];      
g95_symbol *symb;   
locus loc, t;      
g95_expr *e;
int j;   
   
  loc = *g95_current_locus(); 
 
  switch(g95_match_name(name)) {          
  case MATCH_ERROR:          
    return MATCH_ERROR; 
 
  case MATCH_NO: 
    break;  
  
  case MATCH_YES:          
    t = *g95_current_locus();       
    g95_gobble_whitespace();     
    j = g95_next_char();       
    g95_set_locus(&t);   
   
    if (j != ',' && j != ')') break;  
  
    if (g95_find_symbol(name, NULL, 1, &symb)) break;        
    /* Handle error elsewhere */         
         
    /* Eliminate a couple of common cases where we know we don't have
     * a function argument. */     
     
    if (symb == NULL)         
      g95_get_symbol(name, NULL, &symb); 
    else {          
      if (symb->attr.flavor != FL_PROCEDURE && symb->attr.flavor != FL_UNKNOWN)  
	break;      
      
      /* If the symbol is a function with itself as the result and is
       * being defined, then we have a variable */

      if (symb->result == symb &&          
	  (g95_current_ns->proc_name == symb ||  
	   (g95_current_ns->parent != NULL &&      
	    g95_current_ns->parent->proc_name == symb))) break;        
    }

    e = g95_get_expr();              /* Leave it unknown for now */      
    e->symbol = symb;          
    e->type = EXPR_VARIABLE;   
    e->ts.type = BT_PROCEDURE;    
    e->where = loc;      
      
    *rslt = e;  
    return MATCH_YES;  
  }    
    
  g95_set_locus(&loc);     
  return g95_match_expr(rslt);        
}          
          
          
   
   
/* match_digits()-- Match the digit string part of an integer.
 * If the buffer is NULL, we just count characters for the second
 * pass.  Returns the number of characters matched, -1 for no match. */         
         
static int match_digits(int signflag, int radix, char *buffer) {       
locus oldl;     
int len, j;    
    
  len = 0;       
  j = g95_next_char();  
  
  if (signflag && (j == '+' || j == '-')) {    
    if (buffer != NULL) *buffer++ = j;
    j = g95_next_char();     
    len++;   
  }

  if (!check_digit(j, radix)) return -1;    
    
  len++;      
  if (buffer != NULL) *buffer++ = j;  
  
  for(;;) {     
    oldl = *g95_current_locus();     
    j = g95_next_char();   
   
    if (!check_digit(j, radix)) break;  
  
    if (buffer != NULL) *buffer++ = j;      
    len++;     
  }      
      
  g95_set_locus(&oldl);         
         
  return len;         
}        
        
        
       
       
/* match_keyword_arg()-- Match a keyword argument. */          
          
static match match_keyword_arg(g95_actual_arglist *f,          
			       g95_actual_arglist *bottom) {    
char nam[G95_MAX_SYMBOL_LEN+1];   
g95_actual_arglist *b;        
locus name_locus;      
match d;      
      
  name_locus = *g95_current_locus();         
  d = g95_match_name(nam);  
  
  if (d != MATCH_YES) goto cleanup;       
  if (g95_match_char('=') != MATCH_YES) {     
    d = MATCH_NO;
    goto cleanup;      
  }         
         
  d = match_actual_arg(&f->u.expr);          
  if (d != MATCH_YES) goto cleanup;         
  f->type = EXPR;          
          
  /* Make sure this name has not appeared yet */  
  
  if (nam[0] != '\0') {         
    for(b=bottom; b; b=b->next)
      if (strcmp(b->name, nam) == 0) {     
	g95_error("Keyword '%s' at %C has already appeared in the current "  
		  "argument list", nam);     
	return MATCH_ERROR;;    
      }      
  }    
    
  strcpy(f->name, nam);
  return MATCH_YES;      
      
cleanup:    
  g95_set_locus(&name_locus);        
  return d;       
}         
         
         
      
      
/* match_real_constant()-- Match a real constant of some sort. */       
       
static match match_real_constant(g95_expr **rslt, int signflag) {      
int k, u, cnt, seen_dp, seen_digits, exp_char;   
locus o, temp_loc;          
char *d, *b;  
g95_expr *r; 
 
  o = *g95_current_locus();
  g95_gobble_whitespace();  
  
  r = NULL;    
  b = NULL;        
        
  cnt = 0;    
  seen_dp = 0;    
  seen_digits = 0; 
  exp_char = ' ';  
  
  u = g95_next_char();       
  if (signflag && (u == '+' || u == '-')) { 
    u = g95_next_char();     
    cnt++;     
  }    
    
/* Scan significand */         
         
  for(;; u=g95_next_char(), cnt++) {          
    if (u == '.') {       
      if (seen_dp) goto done;    
    
      /* Check to see if "." goes with a following operator like ".eq." */    
    
      temp_loc = *g95_current_locus();      
      u = g95_next_char();          
          
      if (u == 'e' || u == 'd' || u == 'q') {         
	u = g95_next_char();          
	if (u == '.') goto done;   /* Operator named .e. or .d. */  
      }         
         
      if (isalpha(u)) goto done;   /* Distinguish 1.e9 from 1.eq.2 */      
      
      g95_set_locus(&temp_loc);         
      seen_dp = 1;         
      continue;
    }          
          
    if (isdigit(u)) {      
      seen_digits = 1;          
      continue;  
    }   
   
    break;      
  }    
    
  if (!seen_digits || (u != 'e' && u != 'd' && u != 'q')) goto done;     
  exp_char = u;         
         
/* scan exponent */     
     
  u = g95_next_char();   
  cnt++;       
       
  if (u == '+' || u == '-') {  /* optional sign */  
    u = g95_next_char();      
    cnt++;         
  }     
     
  if (!isdigit(u)) {    
    if (!seen_digits) {
      g95_set_locus(&o);       
      return MATCH_NO;   /* ".e" can be something else */  
    }

    g95_error("Missing exponent in real number at %C");         
    return MATCH_ERROR;    
  }       
       
  while(isdigit(u)) {  
    u = g95_next_char();    
    cnt++;   
  }      
      
/* See what we've got */       
       
done:         
  if (!seen_digits || (!seen_dp && exp_char == ' ')) {         
    g95_set_locus(&o);    
    return MATCH_NO;      
  }   
   
/* Convert the number */      
      
  g95_set_locus(&o);      
  g95_gobble_whitespace();

  b = alloca(cnt+1);  
  memset(b, '\0', cnt+1);

  d = b;         
  while(cnt>0) {          
    *d = g95_next_char();       
    if (*d == 'd' || *d == 'q') *d = 'e';   /* Hack for mpf_init_set_str() */    
    d++;        
    cnt--;          
  }         
         
  k = get_kind();   
  if (k == -1) goto cleanup;     
     
  switch(exp_char) {      
  case 'd':          
    if (k != -2) {        
      g95_error("Real number at %C has a 'd' exponent and an explicit kind");    
      goto cleanup;       
    }    
    k = g95_default_double_kind();      
    break;      
      
  case 'q':    
    if (k != -2) {          
      g95_error("Real number at %C has a 'q' exponent and an explicit kind"); 
      goto cleanup;         
    }        
    k = g95_option.q_kind;  
    break;     
     
  default:  
    if (k == -2)  k = g95_default_real_kind();     
    
    if (g95_validate_kind(BT_REAL, k) == -1) { 
      g95_error("Invalid real kind %d at %C", k);     
      goto cleanup;        
    }    
  }   
   
  r = g95_convert_real(b, k, g95_current_locus());       
       
  switch(g95_range_check(r)) {   
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
      
  *rslt = r;          
  return MATCH_YES;

cleanup:   
  g95_free_expr(r);  
  return MATCH_ERROR;     
}  
  
  
    
    
/* match_structure_constructor()-- Match a structure constructor.  The
 * initial symbol has already been seen. */     
     
static match match_structure_constructor(g95_symbol *symbol, g95_expr **r) {   
g95_constructor *h, *end; 
g95_component *comp;    
g95_expr *u;    
locus pos;      
match t;       
       
  h = end = NULL;   
   
  if (g95_match_char('(') != MATCH_YES) goto syntax;    
    
  pos = *g95_current_locus();     
     
  g95_find_component(symbol, NULL);          
          
  for(comp=symbol->components; comp; comp=comp->next) {       
    if (h == NULL) 
      end = h = g95_get_constructor();    
    else {         
      end->next = g95_get_constructor();      
      end = end->next;     
    }     
	     
    t = g95_match_expr(&end->expr);          
    if (t == MATCH_NO) goto syntax;         
    if (t == MATCH_ERROR) goto cleanup; 
 
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
    
  u = g95_get_expr();        
        
  u->type = EXPR_STRUCTURE; 
 
  u->ts.type = BT_DERIVED; 
  u->ts.derived = symbol;   
  u->where = pos;   
   
  u->value.constructor = h;      
      
  *r = u;       
  return MATCH_YES;    
  
syntax:      
  g95_error("Syntax error in structure constructor at %C");   
   
cleanup:
  g95_free_constructor(h); 
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
     
static match match_charkind_name(char *n) {  
locus where;  
char t, peek;   
int leng;       
       
  g95_gobble_whitespace();     
  t = g95_next_char(); 
  if (!isalpha(t)) return MATCH_NO;        
        
  *n++ = t;      
  leng = 1;      
      
  for(;;) { 
    where = *g95_current_locus();        
    t = g95_next_char();

    if (t == '_') {     
      peek = g95_peek_char(); 
       
      if (peek == '\'' || peek == '\"') {    
	g95_set_locus(&where);
	*n = '\0';
	return MATCH_YES;      
      }        
    }       
       
    if (!isalnum(t) && t != '_' && g95_option.dollar && t != '$') break;      
      
    *n++ = t;          
    if (++leng > G95_MAX_SYMBOL_LEN) break;   
  }          
          
  return MATCH_NO;        
}     
     
     
         
         
/* match_boz_constant()-- Match a binary, octal or hexadecimal
 * constant that can be found in a DATA statement */  
  
static match match_boz_constant(g95_expr **rslt) {    
int radix, c, length;        
locus where;         
char *b;        
g95_expr *o;  
char *rname;   
   
  where = *g95_current_locus();  
  g95_gobble_whitespace();   
   
  switch (g95_next_char()) {         
  case 'b':  radix = 2;  rname = "binary";       break;          
  case 'o':  radix = 8;  rname = "octal";        break; 
  case 'z':  radix = 16; rname = "hexadecimal";  break;         
  default: goto backup;        
  }        
        
  /* no whitespace allowed here */

  c = g95_next_char();  
  if (c != '\'' && c != '\"') goto backup;        
        
  where = *g95_current_locus(); 
   
  length = match_digits(0, radix, NULL);       
  if (length == -1) {        
    g95_error("Empty set of digits in %s constants at %C", rname);      
    return MATCH_ERROR;          
  }      
      
  if (g95_next_char() != c) {         
    g95_error("Illegal character in %s constant at %C.", rname);    
    return MATCH_ERROR;   
  }         
         
  g95_set_locus(&where); 
 
  b = alloca(length+1);         
  memset(b, '\0', length+1);

  match_digits(0, radix, b);       
  g95_next_char();     
     
  o = g95_convert_integer(b, g95_default_integer_kind(), radix,      
			  g95_current_locus());   
   
  if (g95_range_check(o) != ARITH_OK) {  
    g95_error("Integer too big for default integer kind at %C");       
       
    g95_free_expr(o);         
    return MATCH_ERROR;       
  }         
         
  *rslt = o;     
  return MATCH_YES;         
         
backup:      
  g95_set_locus(&where);
  return MATCH_NO;   
}       
       
       
        
        
/* extend_ref()-- Used by match_varspec() to extend the reference list
 * by one element. */     
     
static g95_ref *extend_ref(g95_expr *primary, g95_ref *t) {        
        
  if (primary->ref == NULL)        
    primary->ref = t = g95_get_ref();    
  else { 
    t->next = g95_get_ref();    
    t = t->next;  
  }        
        
  return t;      
}       
       
       
  
  
/* match_logical_constant().  Match a .true. or .false. */       
      
static match match_logical_constant(g95_expr **result) {   
static mstring logical_ops[] = {          
  minit(".false.", 0),       
  minit(".true.", 1),   
  minit(NULL, -1) };    
    
g95_expr *u;        
int f, k;    
    
  f = g95_match_strings(logical_ops);          
  if (f == -1) return MATCH_NO;         
         
  k = get_kind();   
  if (k == -1) return MATCH_ERROR;    
  if (k == -2) k = g95_default_logical_kind();   
   
  if (g95_validate_kind(BT_LOGICAL, k) == -1)         
    g95_error("Bad kind for logical constant at %C");        
        
  u = g95_get_expr();     
     
  u->type = EXPR_CONSTANT;         
  u->value.logical = f;  
  u->ts.type = BT_LOGICAL;
  u->ts.kind = k;    
  u->where = *g95_current_locus();   
   
  *result = u;  
  return MATCH_YES;   
}     
     
     
   
   
/* match_const_complex_part()-- Match the real and imaginary parts of
 * a complex number.  This subroutine is essentially
 * match_real_constant() modified in a couple of ways: A sign is
 * always allowed and numbers that would look like an integer to
 * match_real_constant() are automatically created as floating point
 * numbers.  The messiness involved with making sure a decimal point
 * belongs to the number and not a trailing operator is not necessary
 * here either (Hooray!). */ 
 
static match match_const_complex_part(g95_expr **r) {  
int k, seen_digits, seen_dp, cont;  
char *u, s, exp_char, *buf;      
locus old;     
     
  old = *g95_current_locus();      
  g95_gobble_whitespace();

  seen_dp = 0; 
  seen_digits = 0; 
  cont = 0;        
  exp_char = ' ';

  s = g95_next_char();        
  if (s == '-' || s == '+') {         
    s = g95_next_char();    
    cont++;
  }  
  
  for(;; s=g95_next_char(), cont++) {      
    if (s == '.') {    
      if (seen_dp) goto no_match;          
      seen_dp = 1;      
      continue;  
    }        
        
    if (isdigit(s)) {       
      seen_digits = 1;         
      continue;  
    }        
        
    break; 
  }    
    
  if (!seen_digits || (s != 'd' && s != 'e')) goto done;         
  exp_char = s;      
      
/* scan exponent */ 
 
  s = g95_next_char();    
  cont++;          
          
  if (s == '+' || s == '-') {  /* optional sign */    
    s = g95_next_char();        
    cont++;
  }        
        
  if (!isdigit(s)) {     
    g95_error("Missing exponent in real number at %C");      
    return MATCH_ERROR;
  }        
        
  while(isdigit(s)) {      
    s = g95_next_char();   
    cont++;       
  }   
   
done:        
  if (!seen_digits) goto no_match; 
 
/* Convert the number */ 
 
  g95_set_locus(&old);    
  g95_gobble_whitespace();     
     
  buf = alloca(cont+1);
  memset(buf, '\0', cont+1);        
        
  u = buf;
  while(cont>0) {      
    s = g95_next_char(); 
    if (s == 'd') s = 'e';   /* Hack for mpf_init_set_str() */   
    *u++ = s;  
    cont--;    
  }          
          
  *u = '\0'; 
 
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
      
  *r = g95_convert_real(buf, k, g95_current_locus());         
  return MATCH_YES;   
   
no_match:          
  g95_set_locus(&old);
  return MATCH_NO;
}     
     
     
 
 
/* match_varspec()-- Match any additional specifications associated
 * with the current variable like member references or substrings. */   
   
static match match_varspec(g95_expr *primary, int equiv_flag) {  
char nm[G95_MAX_SYMBOL_LEN+1]; 
g95_ref *substring, *end;  
g95_component *component;       
g95_symbol *sym;  
match z;      
      
  end = NULL;          
          
  if (primary->symbol->attr.dimension ||    
      (equiv_flag && g95_peek_char() == '(')) {  
  
    end = extend_ref(primary, end);         
    end->type = REF_ARRAY;     
     
    z = g95_match_array_ref(&end->u.ar, primary->symbol->as, equiv_flag);  
    if (z != MATCH_YES) return z;      
  }      
      
  sym = primary->symbol;       
  primary->ts = sym->ts;      
      
  if (sym->ts.type != BT_DERIVED ||   
      g95_match_char('%') != MATCH_YES) goto check_substring;   
   
  sym = sym->ts.derived;     
     
  for(;;) {          
    z = g95_match_name(nm);   
    if (z == MATCH_NO) g95_error("Expected structure component name at %C");       
    if (z != MATCH_YES) return MATCH_ERROR;        
        
    component = g95_find_component(sym, nm);   
    if (component == NULL) return MATCH_ERROR;

    end = extend_ref(primary, end);       
    end->type = REF_COMPONENT;

    end->u.c.component = component; 
    end->u.c.sym = sym; 
 
    primary->ts = component->ts;    
    
    if (component->as != NULL) {          
      end = extend_ref(primary, end);       
      end->type = REF_ARRAY;         
         
      z = g95_match_array_ref(&end->u.ar, component->as, equiv_flag);          
      if (z != MATCH_YES) return z;    
    }         
         
    if (component->ts.type != BT_DERIVED || g95_match_char('%') != MATCH_YES)
      break;  
  
    sym = component->ts.derived;
  }          
          
check_substring:    
  if (primary->ts.type == BT_CHARACTER) {      
    switch(match_substring(primary->ts.cl, equiv_flag, &substring)) {
    case MATCH_YES: 
      if (end == NULL)     
	primary->ref = substring; 
      else     
	end->next = substring;     
     
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
 
 
    
    
/* g95_match_rvalue()-- Matches a variable name followed by anything
 * that might follow it-- array reference, argument list of a
 * function, etc. */       
       
match g95_match_rvalue(g95_expr **rslt) {
g95_actual_arglist *actual_arglist; 
char nm[G95_MAX_SYMBOL_LEN+1];     
g95_state_data *st1;         
g95_symbol *sy;     
locus loc;
g95_expr *h;       
match b;
int u; 
 
  b = g95_match_name(nm);      
  if (b != MATCH_YES) return b;   
   
  if (g95_find_state(COMP_INTERFACE) == SUCCESS)     
    u = g95_get_symbol(nm, NULL, &sy);     
  else    
    u = g95_get_ha_symbol(nm, &sy);     
     
  if (u) return MATCH_ERROR;  
  
  h = NULL;  
  loc = *g95_current_locus();

  if (sy->attr.function && sy->result == sy &&    
      (g95_current_ns->proc_name == sy ||          
       (g95_current_ns->parent != NULL &&
	g95_current_ns->parent->proc_name == sy))) goto variable;     
     
  if (sy->attr.function || sy->attr.external || sy->attr.intrinsic)   
    goto function0; 
 
  if (sy->attr.generic) goto generic_function;      
      
  switch(sy->attr.flavor) {  
  case FL_VARIABLE: 
  variable:         
    if (sy->ts.type == BT_UNKNOWN && g95_peek_char() == '%' &&       
	g95_get_default_type(sy, sy->ns)->type == BT_DERIVED)         
      g95_set_default_type(sy, 0, sy->ns);  
  
    h = g95_get_expr();          
          
    h->type = EXPR_VARIABLE;  
    h->symbol = sy;

    b = match_varspec(h, 0);       
    break; 
 
  case FL_PARAMETER:     
    if (sy->value->type != EXPR_ARRAY)       
      h = g95_copy_expr(sy->value); 
    else {   
      h = g95_get_expr();          
      h->type = EXPR_VARIABLE;     
    }        
        
    h->symbol = sy;       
    b = match_varspec(h, 0);      
    break;         
         
  case FL_DERIVED:     
    sy = g95_use_derived(sy);        
    if (sy == NULL)       
      b = MATCH_ERROR; 
    else {
      b = match_structure_constructor(sy, &h);    
      if (b == MATCH_YES) h->symbol = sy;        
    }

    break;   
   
/* If we're here, then the name is known to be the name of a
 * procedure, yet it is not sure to be the name of a function. */        
        
  case FL_PROCEDURE:          
    if (sy->attr.subroutine) {        
      g95_error("Unexpected use of subroutine name '%s' at %C", sy->name);     
      b = MATCH_ERROR;        
      break;        
    }

/* At this point, the name has to be a non-statement function.  If the
 * name is the same as the current function being compiled, then we
 * have a variable reference (to the function result) if the name is
 * non-recursive. */   
   
    st1 = g95_enclosing_unit(NULL);     
     
    if (st1 != NULL && st1->state == COMP_FUNCTION && st1->sym == sy &&     
	!sy->attr.recursive) {         
      h = g95_get_expr();  
      h->symbol = sy;      
      h->type = EXPR_VARIABLE;     
           
      b = match_varspec(h, 0);      
      break;   
    }          
          
    /* Match a function reference */         
         
  function0:        
    b = g95_match_actual_arglist(0, &actual_arglist);     
    if (b == MATCH_NO) {          
      if (sy->attr.proc == PROC_ST_FUNCTION)     
	g95_error("Statement function '%s' requires argument list at %C",   
		  sy->name);   
      else      
	g95_error("Function '%s' requires an argument list at %C", 
		  sy->name);

      b = MATCH_ERROR;        
      break;         
    }      
      
    if (b != MATCH_YES) {  
      b = MATCH_ERROR;    
      break;          
    }  
  
    g95_get_symbol(nm, NULL, &sy);   
   
    h = g95_get_expr();    
    h->symbol = sy;          
    h->type = EXPR_FUNCTION;   
    h->value.function.actual = actual_arglist;  
    h->where = *g95_current_locus();       
       
    if (sy->as != NULL) h->rank = sy->as->rank;         
         
    if (!sy->attr.function && g95_add_function(&sy->attr, NULL) == FAILURE) {      
      b = MATCH_ERROR;  
      break;        
    }      
      
    b = MATCH_YES;   
    break;        
        
  case FL_UNKNOWN: 
 
    /* Special case for derived type variables that get their types
     * via an IMPLICIT statement.  This can't wait for the resolution
     * phase. */  
  
    if (g95_peek_char() == '%' && 
	g95_get_default_type(sy, sy->ns)->type == BT_DERIVED)        
      g95_set_default_type(sy, 0, sy->ns);        
        
/* If the symbol has a dimension attribute, the expression is a variable */     
     
    if (sy->attr.dimension) { 
      if (g95_add_flavor(&sy->attr, FL_VARIABLE, NULL) == FAILURE) {
	b = MATCH_ERROR;     
	break;          
      }     
     
      h = g95_get_expr();      
      h->symbol = sy;
      h->type = EXPR_VARIABLE;      
      b = match_varspec(h, 0);       
      break; 
    }        
        
/* Name is not an array, so we peek to see if a '(' implies a function
 * call or a substring reference.  Otherwise the variable is just a
 * scalar. */      
      
    g95_gobble_whitespace();        
    if (g95_peek_char() != '(') {   /* Assume a scalar variable */      
      h = g95_get_expr();         
      h->symbol = sy;       
      h->type = EXPR_VARIABLE;       
       
      if (g95_add_flavor(&sy->attr, FL_VARIABLE, NULL) == FAILURE) { 
	b = MATCH_ERROR;    
	break;  
      }

      h->ts = sy->ts;       
      b = match_varspec(h, 0);
      break;   
    }

/* See if this could possibly be a substring reference of a name that
 * we're not sure is a variable yet. */       
       
    h = g95_get_expr();          
    h->symbol = sy;

    if ((sy->ts.type == BT_UNKNOWN || sy->ts.type == BT_CHARACTER) &&    
	match_substring(sy->ts.cl, 0, &h->ref) == MATCH_YES) {      
      
      h->type = EXPR_VARIABLE;       
       
      if (sy->attr.flavor != FL_VARIABLE &&
	  g95_add_flavor(&sy->attr, FL_VARIABLE, NULL) == FAILURE) {   
	b = MATCH_ERROR;      
	break;      
      }

      if (sy->ts.type == BT_UNKNOWN &&
	  g95_set_default_type(sy, 1, NULL) == FAILURE) {   
	b = MATCH_ERROR;          
	break;
      }  
  
      h->ts = sy->ts;  
      b = MATCH_YES;         
      break;     
    }         
         
/* Give up, assume we have a function */       
       
    g95_get_symbol(nm, NULL, &sy);   /* Can't fail */      
    h->type = EXPR_FUNCTION;  
  
    if (!sy->attr.function && g95_add_function(&sy->attr, NULL) == FAILURE) { 
      b = MATCH_ERROR;        
      break;
    }

    sy->result = sy;      
      
    b = g95_match_actual_arglist(0, &h->value.function.actual);    
    if (b == MATCH_NO)     
      g95_error("Missing argument list in function '%s' at %C", sy->name);    
    
    if (b != MATCH_YES) {          
      b = MATCH_ERROR;      
      break;        
    }    
    
    /* If our new function returns a character, array or structure
     * type, it might have subsequent references. */    
    
    b = match_varspec(h, 0);  
    if (b == MATCH_NO) b = MATCH_YES;    
    
    break; 
 
  generic_function:        
    g95_get_symbol(nm, NULL, &sy);   /* Can't fail */   
   
    h = g95_get_expr();      
    h->symbol = sy;         
    h->type = EXPR_FUNCTION;

    b = g95_match_actual_arglist(0, &h->value.function.actual);        
    break;        
        
  default:     
    g95_error("Symbol at %C is not appropriate for an expression");   
    return MATCH_ERROR;         
  }         
         
  if (b == MATCH_YES) {       
    h->where = loc;          
    *rslt = h; 
  } else       
    g95_free_expr(h);    
    
  return b;     
}     
     
     
  
  
/* match_string_constant()-- See if the current input matches a
 * character constant.  Lots of contortions have to be done to match
 * the kind parameter which comes before the actual string.  The main
 * consideration is that we don't want to error out too quickly.  For
 * example, we don't actually do any validation of the kinds until we
 * have actually seen a legal delimiter.  Using match_kind_param()
 * generates errors too quickly. */          
          
static match match_string_constant(g95_expr **res) {      
char *f, nam[G95_MAX_SYMBOL_LEN+1];  
int r, v, k0, len, delimiter;     
locus old_locus, start_locus;     
g95_symbol *symb;          
g95_expr *z;        
char *x;        
match h;          
          
  old_locus = *g95_current_locus();   
   
  g95_gobble_whitespace();      
      
  start_locus = *g95_current_locus();   
   
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
    g95_set_locus(&old_locus);        
        
    h = match_charkind_name(nam);     
    if (h != MATCH_YES) goto no_match; 
 
    if (g95_find_symbol(nam, NULL, 1, &symb) || symb == NULL ||     
	symb->attr.flavor != FL_PARAMETER) goto no_match;    
    
    k0 = -1;         
    v = g95_next_char();  
  }      
      
  if (v == ' ') {       
    g95_gobble_whitespace();      
    v = g95_next_char(); 
  }         
         
  if (v != '_') goto no_match;    
    
  g95_gobble_whitespace();        
  start_locus = *g95_current_locus();     
     
  v = g95_next_char();   
  if (v != '\'' && v != '"') goto no_match;    
    
  if (k0 == -1) { 
    x = g95_extract_int(symb->value, &k0);  
    if (x != NULL) {      
      g95_error(x);        
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
      g95_set_locus(&start_locus);     
      g95_error("Unterminated character constant beginning at %C");   
      return MATCH_ERROR;    
    } 
 
    len++;   
  }          
          
  z = g95_char_expr(len, k0, &start_locus);      
  f = z->value.character.string;       
       
  g95_set_locus(&start_locus);   
  g95_next_char();              /* Skip delimiter */   
   
  for(r=0; r<len; r++)          
    *f++ = g95_next_string_char(delimiter);      
      
  *f = '\0';     /* C-style string is for development/debug purposes */     
     
  if (g95_next_string_char(delimiter) != -1)          
    g95_internal_error("match_string_constant(): Delimiter not found");      
      
  if (match_substring(NULL, 0, &z->ref) != MATCH_NO) z->type = EXPR_SUBSTRING;    
    
  *res = z;  
  
  return MATCH_YES;       
       
no_match:   
  g95_set_locus(&old_locus);       
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
int r;      
      
  r = g95_next_char_literal(1);    
    
  if (r == '\n') return -2;          
          
  if (r == '\\') {
    old_locus = *g95_current_locus();          
          
    switch(g95_next_char_literal(1)) {
    case 'a':  r = '\a'; break;        
    case 'b':  r = '\b'; break;    
    case 't':  r = '\t'; break;         
    case 'f':  r = '\f'; break;    
    case 'n':  r = '\n'; break;        
    case 'r':  r = '\r'; break;      
    case 'v':  r = '\v'; break;     
    case '\\': r = '\\'; break;       
       
    default:     /* Unknown backslash codes are simply not expanded */         
      g95_set_locus(&old_locus);       
      break;   
    }
  }       
       
  if (r != delimiter) return r;          
          
  old_locus = *g95_current_locus(); 
  r = g95_next_char_literal(1);      
      
  if (r == delimiter) return r;
  g95_set_locus(&old_locus);

  return -1;    
}    
    
    
  
  
/* match_integer_constant()-- Match an integer (digit string and
 * optional kind). */

static match match_integer_constant(g95_expr **res, int signflag) {   
int leng, k0;         
locus old_loc;      
char *buf;      
g95_expr *f;     
     
  old_loc = *g95_current_locus();     
  g95_gobble_whitespace(); 
 
  leng = match_digits(signflag, 10, NULL);       
  g95_set_locus(&old_loc);          
  if (leng == -1) return MATCH_NO;        
        
  buf = alloca(leng+1);          
  memset(buf, '\0', leng+1);      
      
  g95_gobble_whitespace();   
   
  match_digits(signflag, 10, buf);

  k0 = get_kind();    
  if (k0 == -2) k0 = g95_default_integer_kind();     
  if (k0 == -1) return MATCH_ERROR;    
    
  if (g95_validate_kind(BT_INTEGER, k0) == -1) {     
    g95_error("Integer kind %d at %C not available", k0);      
    return MATCH_ERROR;        
  } 
 
  f = g95_convert_integer(buf, k0, 10, g95_current_locus());

  if (g95_range_check(f) != ARITH_OK) { 
    g95_error("Integer too big for its kind at %C");        
        
    g95_free_expr(f);  
    return MATCH_ERROR;   
  }

  *res = f;      
  return MATCH_YES;          
}       
       
       
      
      
/* g95_match_actual_arglist()-- Matches an actual argument list of a
 * function or subroutine, from the opening parenthesis to the closing
 * parenthesis.  The argument list is assumed to allow keyword
 * arguments because we don't know if the symbol associated with the
 * procedure has an implicit interface or not.  We make sure keywords
 * are unique. */  
  
match g95_match_actual_arglist(int sub_flag, g95_actual_arglist **argps) {      
g95_actual_arglist *start, *end;     
int seen_keyword;         
g95_st_label *lab;         
locus old_loc;    
match w;

  *argps = end = NULL;  
  old_loc = *g95_current_locus();     
     
  seen_keyword = 0;   
   
  if (g95_match_char('(') == MATCH_NO)   
    return (sub_flag) ? MATCH_YES : MATCH_NO;    
    
  if (g95_match_char(')') == MATCH_YES) return MATCH_YES;         
  start = NULL;      
      
  for(;;) {  
    if (start == NULL)        
      start = end = g95_get_actual_arglist();       
    else {      
      end->next = g95_get_actual_arglist();      
      end = end->next;          
    }      
      
    if (sub_flag && g95_match_char('*') == MATCH_YES) {    
      w = g95_match_st_label(&lab, 0);          
      if (w == MATCH_NO) g95_error("Expected alternate return label at %C"); 
      if (w != MATCH_YES) goto cleanup;  
  
      end->u.label = lab;
      end->type = ALT_RETURN;        
      goto next1; 
    }         
         
    /* After the first keyword argument is seen, the following
     * arguments must also have keywords. */ 
 
    if (seen_keyword) {    
      w = match_keyword_arg(end, start);    
    
      if (w == MATCH_ERROR) goto cleanup;          
      if (w == MATCH_NO) {      
	g95_error("Missing keyword name in actual argument list at %C");         
	goto cleanup;     
      }          
          
    } else {   /* See if we have the first keyword argument */

      w = match_keyword_arg(end, start);   
      if (w == MATCH_YES) seen_keyword = 1;
      if (w == MATCH_ERROR) goto cleanup;        
        
      if (w == MATCH_NO) {  /* Try for a non-keyword argument */          
	w = match_actual_arg(&end->u.expr); 
	end->type = EXPR;  
  
	if (w == MATCH_ERROR) goto cleanup;  
	if (w == MATCH_NO) goto syntax;      
      }       
    }         
         
  next1:   
    if (g95_match_char(')') == MATCH_YES) break;   
    if (g95_match_char(',') != MATCH_YES) goto syntax;   
  }    
    
  *argps = start;       
  return MATCH_YES;      
      
syntax:     
  g95_error("Syntax error in argument list at %C");

cleanup:     
  g95_free_actual_arglist(start);    
  g95_set_locus(&old_loc);          
          
  return MATCH_ERROR;        
}      
      
      


/* g95_match_variable()-- Match a variable, ie something that can be
 * assigned to.  This starts as a symbol, can be a structure
 * component, array reference or substring.  It can be a function if
 * the function doesn't have a separate RESULT variable.  If the
 * symbol has not been previously seen, we assume it is a variable. */         
         
match g95_match_variable(g95_expr **result, int equiv_flag) {    
g95_symbol *sy;         
g95_expr *exp;    
locus w;         
match e;          
          
  e = g95_match_symbol(&sy, 1);      
  if (e != MATCH_YES) return e;    
  w = *g95_current_locus();        
        
  switch(sy->attr.flavor) {         
  case FL_VARIABLE:    
    break;         
         
  case FL_UNKNOWN:   
    if (g95_add_flavor(&sy->attr, FL_VARIABLE, NULL) == FAILURE)
      return MATCH_ERROR;  
  
    /* Special case for derived type variables that get their types
     * via an IMPLICIT statement.  This can't wait for the resolution
     * phase. */  
  
    if (g95_peek_char() == '%' &&       
	g95_get_default_type(sy, sy->ns)->type == BT_DERIVED)   
      g95_set_default_type(sy, 0, sy->ns);      
      
    break;         
         
  case FL_PROCEDURE:  /* Check for a nonrecursive function result */     
    if (sy->attr.function && (sy->result == sy || sy->attr.entry)) {

      /* If a function result is a derived type, then the derived
       * type may still have to be resolved. */     
     
      if (sy->ts.type == BT_DERIVED &&         
	  g95_use_derived(sy->ts.derived) == NULL) return MATCH_ERROR; 
 
      break;      
    }        
        
    /* Fall through to error */ 
 
  default:  
    g95_error("Expected VARIABLE at %C");  
    return MATCH_ERROR;    
  }       
       
  exp = g95_get_expr();   
   
  exp->type = EXPR_VARIABLE;
  exp->symbol = sy;        
  exp->ts = sy->ts;          
  exp->where = w;    
    
/* Now see if we have to do more */       
       
  e = match_varspec(exp, equiv_flag);        
  if (e != MATCH_YES) {   
    g95_free_expr(exp);   
    return e;        
  }     
     
  *result = exp;   
  return MATCH_YES;    
}    
    
       
       
/* match_sym_complex_part()-- Match a real or imaginary part of a complex
 * constant that is a symbolic constant. */     
     
static match match_sym_complex_part(g95_expr **res) {        
char name[G95_MAX_SYMBOL_LEN+1]; 
g95_symbol *s;          
g95_expr *n; 
match y;  
  
  y = g95_match_name(name); 
  if (y != MATCH_YES) return y;        
        
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
    n = g95_copy_expr(s->value); 
    break;          
          
  case BT_COMPLEX:     
    n = g95_complex2real(s->value, s->value->ts.kind);      
    if (n == NULL) goto error;     
    break; 
 
  case BT_INTEGER:  
    n = g95_int2real(s->value, g95_default_real_kind());  
    if (n == NULL) goto error;
    break;

  default:       
    g95_internal_error("g95_match_sym_complex_part(): Bad type");       
  }       
       
  *res = n;     /* e is a scalar, real, constant expression */  
  return MATCH_YES;  
  
error:          
  g95_error("Error converting PARAMETER constant in complex constant at %C");      
  return MATCH_ERROR;     
}      
      
      
        
        
/* g95_expr_attr()-- Return the attribute from a general expression */         
         
symbol_attribute g95_expr_attr(g95_expr *o) {
symbol_attribute atr; 
 
  switch(o->type) {       
  case EXPR_VARIABLE:       
    atr = g95_variable_attr(o, NULL);  
    break;         
         
  case EXPR_FUNCTION:  
    g95_clear_attr(&atr);         
    atr = o->symbol->result->attr;       
       
    /* NULL() returns pointers.  May have to take care of this here */         
         
    break;   
   
  default:
    g95_clear_attr(&atr);    
    break;     
  }     
     
  return atr;      
}          
          
          
       
       
/* match_complex_part()-- Match a real or imaginary part of a complex number */   
   
static match match_complex_part(g95_expr **r) {       
match i; 
 
  i = match_sym_complex_part(r);    
  if (i != MATCH_NO) return i;         
         
  return match_const_complex_part(r);          
}  
  
  
 
 
/* match_complex_constant()-- Try to match a complex constant */    
    
static match match_complex_constant(g95_expr **r) {          
g95_expr *t, *real, *imag;        
g95_error_buf old_error;
g95_typespec target;     
locus old_loc;      
int knd;          
match s;     
     
  old_loc = *g95_current_locus();   
  real = imag = t = NULL;    
    
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
   
  knd = g95_kind_max(real, imag);    
  target.type = BT_REAL;  
  target.kind = knd;

  if (knd != real->ts.kind) g95_convert_type(real, &target, 2);         
  if (knd != imag->ts.kind) g95_convert_type(imag, &target, 2);    
    
  t = g95_convert_complex(real, imag, knd);     
  t->where = *g95_current_locus();

  g95_free_expr(real);         
  g95_free_expr(imag);   
   
  *r = t;       
  return MATCH_YES;  
  
syntax:     
  g95_error("Syntax error in COMPLEX constant at %C");         
  s = MATCH_ERROR;        
        
cleanup:
  g95_free_expr(t);          
  g95_free_expr(real);     
  g95_free_expr(imag);  
  g95_set_locus(&old_loc); 
 
  return s; 
}         
         
         
   
   
/* g95_match_literal_constant()-- Match constants in any of several
 * forms.  Returns nonzero for a match, zero for no match. */        
        
match g95_match_literal_constant(g95_expr **rslt, int signflag) {
match l;

  l = match_complex_constant(rslt); 
  if (l != MATCH_NO) return l;     
     
  l = match_string_constant(rslt); 
  if (l != MATCH_NO) return l;        
        
  l = match_boz_constant(rslt);     
  if (l != MATCH_NO) return l;  
  
  l = match_real_constant(rslt, signflag);
  if (l != MATCH_NO) return l;   
   
  l = match_integer_constant(rslt, signflag);         
  if (l != MATCH_NO) return l;    
    
  l = match_logical_constant(rslt);       
  if (l != MATCH_NO) return l;      
      
  return MATCH_NO; 
} 
 
 
