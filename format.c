/* FORMAT statement
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
  
/* format.c-- match the FORMAT statement and check format string */      
      
#include <ctype.h>
#include <string.h>
#include "g95.h"
    
/* format tokens returned by format_lex() */     
     
typedef enum {        
  FMT_NONE, FMT_UNKNOWN, FMT_SIGNED_INT, FMT_ZERO, FMT_POSINT, FMT_PERIOD,   
  FMT_COMMA, FMT_COLON, FMT_SLASH, FMT_DOLLAR, FMT_POS, FMT_LPAREN,          
  FMT_RPAREN, FMT_X, FMT_SIGN, FMT_BLANK, FMT_CHAR, FMT_P, FMT_IBOZ, FMT_F,    
  FMT_E, FMT_EXT, FMT_G, FMT_L, FMT_A, FMT_D, FMT_H, FMT_END          
} format_token;      
      
      
/* Local variables for checking format strings.  The saved_token is
 * used to back up by a single format token during the parsing process. */      
      
static char last_char, *format_string;     
static int format_length, use_last_char, repeat;   
   
static format_token saved_token;         
         
static enum { MODE_STRING, MODE_FORMAT, MODE_COPY } mode;         
         
         
     
     
/* next_char()-- Return the next character in the format string */       
       
static char next_char(int in_string) {          
char k;         
         
  if (use_last_char) {         
    use_last_char = 0;       
    return last_char;     
  }      
      
  format_length++; 
 
  if (mode == MODE_STRING)
    k = *format_string++;       
  else {       
    k = g95_next_char_literal(in_string);       
    if (k == '\n') k = '\0'; 
 
    if (mode == MODE_COPY) *format_string++ = k;  
  }         
         
  k = toupper(k);
  last_char = k;  
  return k;          
}        
        
        


/* unget_char()-- Back up one character position.  Only works once. */   
   
static void unget_char(void) {   
   
  use_last_char = 1;      
}




/* format_lex()-- Simple lexical analyzer for getting the next token
 * in a FORMAT statement. */    
    
static format_token format_lex(void) { 
format_token token;         
char x, delim;   
   
  if (saved_token != FMT_NONE) {          
    token = saved_token;        
    saved_token = FMT_NONE;         
    return token;         
  }       
       
  do {    
    x = next_char(0);
  } while(g95_is_whitespace(x));    
    
  switch(x) {    
  case '-':      
  case '+':     
    x = next_char(0);          
    if (!isdigit(x)) {   
      token = FMT_UNKNOWN;         
      break;
    }         
         
    do {       
      x = next_char(0);       
    } while(isdigit(x));     
     
    unget_char();    
    token = FMT_SIGNED_INT;   
    break;      
      
  case '0': case '1': case '2': case '3': case '4':         
  case '5': case '6': case '7': case '8': case '9':     
    repeat = x - '0';  
  
    for(;;) {  
      x = next_char(0);        
      if (!isdigit(x)) break;         
         
      repeat = 10*repeat + x - '0';  
    }          
          
    unget_char();     
    token = (repeat == 0) ? FMT_ZERO : FMT_POSINT;     
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
    x = next_char(0);    
    if (x != 'L' && x != 'R') unget_char();  
  
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
    x = next_char(0);     
    if (x != 'P' && x != 'S') unget_char();    
    
    token = FMT_SIGN;       
    break;        
        
  case 'B':   
    x = next_char(0);
    if (x == 'N' || x == 'Z')         
      token = FMT_BLANK;          
    else {   
      unget_char();
      token = FMT_IBOZ;      
    }     
     
    break;        
        
  case '\'': case '"':      
    delim = x;           
     
    for(;;) {  
      x = next_char(1);    
      if (x == '\0') {    
	token = FMT_END;   
	break;
      }     
     
      if (x == delim) {     
	x = next_char(1);      
      
	if (x == '\0') {          
	  token = FMT_END; 
	  break;  
	} 
 
	if (x != delim) {       
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
    x = next_char(0);         
    if (x == 'N' || x == 'S')       
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
char *error, 
     *posint_required = "Positive width required", 
     *period_required = "Period required",   
     *nonneg_required = "Nonnegative width required",  
     *unexpected_element = "Unexpected element",     
     *unexpected_end = "Unexpected end of format string";    
    
format_token c, l; 
int m;      
try rv;   
   
  use_last_char = 0;    
  saved_token = FMT_NONE;      
  m = 0; 
  rv = SUCCESS;      
      
  c = format_lex();    
  if (c != FMT_LPAREN) {        
    error = "Missing leading left parenthesis";    
    goto syntax;      
  }       
       
  c = format_lex(); 
  if (c == FMT_RPAREN) goto finished;   /* Empty format is legal */          
  saved_token = c;    
    
/* In this state, the next thing has to be a format item */         
         
format_item:      
  c = format_lex();    
  switch(c) {   
  case FMT_POSINT:
    c = format_lex();     
    if (c == FMT_LPAREN) {    
      m++;      
      goto format_item;    
    }          
          
    if (c == FMT_SLASH) goto optional_comma;         
         
    goto data_desc;        
        
  case FMT_ZERO:     
    c = format_lex();     
    if (c != FMT_P) {   
      error = "Zero repeat count not allowed";        
      goto syntax;    
    }          
          
    goto p_descriptor;        
        
  case FMT_LPAREN:  
    m++;  
    goto format_item;       
       
  case FMT_SIGNED_INT:  /* Signed integer can only precede a P format */          
    c = format_lex();    
    if (c != FMT_P) {          
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
    c = format_lex();         
    if (c != FMT_RPAREN || m > 0) {     
      error = "$ must the last specifier";   
      goto syntax;     
    }   
   
    goto finished;      
      
  case FMT_POS:  case FMT_IBOZ:  case FMT_F:  case FMT_E:  case FMT_EXT:        
  case FMT_G:    case FMT_L:     case FMT_A:  case FMT_D:
    goto data_desc;        
        
  case FMT_H:      
    repeat = 1;        
    goto handle_hollerith;      
      
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
  switch(c) {  
  case FMT_SIGN: 
  case FMT_BLANK:       
  case FMT_X: 
    break;   
   
  case FMT_P: 
  p_descriptor:      
    if (g95_option.pedantic) {          
      c = format_lex();     
      if (c == FMT_POSINT) {
	error = "Repeat count cannot follow P descriptor";        
	goto syntax;   
      }  
  
      saved_token = c;      
    }          
          
    goto optional_comma;    
    
  case FMT_POS:    
  case FMT_L:         
    c = format_lex();         
    if (c == FMT_POSINT) break; 
 
    error = posint_required;     
    goto syntax;  
  
  case FMT_A:   
    c = format_lex();        
    if (c != FMT_POSINT) saved_token = c;       
    break;   
   
  case FMT_D:  case FMT_E:        
  case FMT_G:  case FMT_EXT:      
    l = format_lex();    
    if (l != FMT_POSINT) {          
      error = posint_required;     
      goto syntax;        
    }      
      
    l = format_lex();     
    if (l != FMT_PERIOD) {        
      error = period_required; 
      goto syntax;    
    } 
 
    l = format_lex();   
    if (l != FMT_ZERO && l != FMT_POSINT) { 
      error = nonneg_required; 
      goto syntax;        
    }    
    
    if (c == FMT_D) break;     
     
/* Look for optional exponent */   
   
    l = format_lex();          
    if (l != FMT_E) {      
      saved_token = l;        
    } else {          
      l = format_lex();
      if (l != FMT_POSINT) {  
	error = "Positive exponent width required";  
	goto syntax;  
      }         
    }       
       
    break;     
     
  case FMT_F:        
    c = format_lex();      
    if (c != FMT_ZERO && c != FMT_POSINT) {   
      error = nonneg_required;    
      goto syntax;        
    }       
       
    c = format_lex(); 
    if (c != FMT_PERIOD) {        
      error = period_required;     
      goto syntax;     
    }          
          
    c = format_lex();
    if (c != FMT_ZERO && c != FMT_POSINT) {  
      error = nonneg_required;          
      goto syntax;  
    }         
         
    break;    
    
  case FMT_H:  
  handle_hollerith: 
    if (g95_option.pedantic) {
      error = "The H format specifier is not allowed in Fortran 95"; 
      goto syntax; 
    }

    while(repeat>0) {       
      if (next_char(0) == '\0') {   
	error = unexpected_end;          
	goto syntax;   
      }          
          
      repeat--;        
    }  
  
    break;          
          
  case FMT_IBOZ: 
    c = format_lex();        
    if (c != FMT_ZERO && c != FMT_POSINT) {
      error = nonneg_required; 
      goto syntax; 
    }     
     
    c = format_lex();   
    if (c != FMT_PERIOD) {
      saved_token = c;  
    } else {      
      c = format_lex();    
      if (c != FMT_ZERO && c != FMT_POSINT) {         
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
  c = format_lex(); 
  switch(c) {      
      
  case FMT_COMMA:  
    goto format_item;        
        
  case FMT_RPAREN:   
    m--; 
    if (m < 0) goto finished;
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
  c = format_lex(); 
  switch(c) {    
  case FMT_COMMA:  
    break;        
        
  case FMT_RPAREN:  
    m--;       
    if (m < 0) goto finished;   
    goto between_desc;         
         
  default:     /* Assume that we have another format item */          
    saved_token = c;
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
   
void g95_check_format_string(g95_expr *p) {          
          
  mode = MODE_STRING;     
  format_string = p->value.character.string;        
  check_format();
}    
    
    
          
          
/* g95_match_format()-- Match a FORMAT statement.  This amounts to
 * actually parsing the format descriptors in order to correctly
 * locate the end of the format string.  */        
        
match g95_match_format(void) {         
locus start; 
g95_expr *e;

  if (g95_statement_label == NULL) {     
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
          
  /* The label doesn't get created until after the statement is done
   * being matched, so we have to leave the string for later. */     
     
  g95_set_locus(&start);      /* Back to the beginning */    
    
  e = g95_get_expr();          
  e->type = EXPR_CONSTANT;        
  e->ts.type = BT_CHARACTER;        
  e->ts.kind = g95_default_character_kind();  
  
  e->where = start;       
  e->value.character.string = format_string = g95_getmem(format_length+1);        
  e->value.character.length = format_length;        
        
  g95_statement_label->format = e;        
        
  mode = MODE_COPY;    
  check_format();       /* Guaranteed to succeed */   
   
  g95_match_eos();      /* Guaranteed to succeed */         
  new_st.type = EXEC_NOP;    
    
  return MATCH_YES;   
}          
