/* Matching subroutines
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
          
/* match.c-- matchers in all sizes, shapes and colors. */      
      
#include <stdarg.h>
#include <ctype.h>
#include <string.h>
  
#include "g95.h"
     
static match var_element(g95_data_variable *);


/******************** Generic matching subroutines ************************/   
   
   
    
    
/* g95_match_char()-- Tries to match the next non-whitespace character
 * on the input.  This subroutine does not return MATCH_ERROR.  */        
        
match g95_match_char(char q) {         
locus loc;   
   
  loc = *g95_current_locus();
  g95_gobble_whitespace();  
  
  if (g95_next_char() == q) return MATCH_YES;          
          
  g95_set_locus(&loc);          
  return MATCH_NO;      
}         
         
         


/* g95_match_st_function()-- Match a statement function declaration.
 * It is so easy to match non-statement function statements with a
 * MATCH_ERROR as opposed to MATCH_NO that we suppress error message
 * in most cases. */   
   
match g95_match_st_function(void) {         
g95_error_buf old_error;   
g95_expr *u, *e1;         
g95_namespace *names;       
g95_symbol *sy;         
g95_symtree *sta;      
g95_code *o;        
match f; 
 
  f = g95_match_symbol(&sy, 0);   
  if (f != MATCH_YES) return f;   
   
  g95_push_error(&old_error);   
   
  if (g95_add_procedure(&sy->attr, PROC_ST_FUNCTION, NULL) == FAILURE)    
    goto undo_error;

  if (g95_match_formal_arglist(sy, 1, 0) != MATCH_YES) goto undo_error;          
          
  f = g95_match(" = %e%t", &e1);     
  if (f == MATCH_NO) goto undo_error;         
  if (f == MATCH_ERROR) return f;    
    
  /* At this point, we have a statement function.  These are
   * implemented by turning them into the equivalent contained
   * procedure.  The dummy arguments are copied into the contained
   * space during resolution. */   
   
  sy->result = sy;   
   
  u = g95_get_expr();
  u->type = EXPR_VARIABLE;    
  u->symbol = sy;
  u->where = *g95_current_locus();         
         
  o = g95_get_code();          
  o->type = EXEC_ASSIGN;  
  
  o->expr = u;        
  o->expr2 = e1;     
     
  names = g95_get_namespace(g95_current_ns); 
  names->proc_name = sy;       
  names->code = o;     
  names->parent = g95_current_ns;    
    
  names->sibling = g95_current_ns->contained;    
  g95_current_ns->contained = names;      
      
  sta = g95_new_symtree(&names->sym_root, sy->name);        
  sta->n.sym = sy;

  sy->refs++;

  return MATCH_YES; 
 
undo_error:     
  g95_pop_error(&old_error);   
  return MATCH_NO;     
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
g95_st_label *g, *x, *l3; 
match mm, y, k; 
g95_expr *exp;     
locus o;          
g95_code *f;          
          
  k = g95_match_label();    
  if (k == MATCH_ERROR) return k;

  o = *g95_current_locus();  
  
  y = g95_match(" if ( %e", &exp);  
  if (y != MATCH_YES) return y;        
        
  if (g95_match_char(')') != MATCH_YES) {        
    g95_error("Syntax error in IF-expression at %C");         
    g95_free_expr(exp);      
    return MATCH_ERROR;
  }     
     
  y = g95_match(" %l , %l , %l%t", &g, &x, &l3); 
 
  if (y == MATCH_YES) { 
    if (k == MATCH_YES) {     
      g95_error("Block label not appropriate for arithmetic IF statement "    
		"at %C");   
   
      g95_free_expr(exp);      
      return MATCH_ERROR;  
    }        
        
    if (g95_reference_st_label(g, ST_LABEL_TARGET) == FAILURE ||
	g95_reference_st_label(x, ST_LABEL_TARGET) == FAILURE ||  
	g95_reference_st_label(l3, ST_LABEL_TARGET) == FAILURE) {          
          
      g95_free_expr(exp);       
      return MATCH_ERROR;          
    }         
         
    new_st.type = EXEC_ARITHMETIC_IF;   
    new_st.expr = exp;     
    new_st.label = g;       
    new_st.label2 = x;          
    new_st.label3 = l3;     
     
    *if_type = ST_ARITHMETIC_IF;  
    return MATCH_YES;      
  }         
         
  if (g95_match(" then %t") == MATCH_YES) {
    new_st.type = EXEC_IF;      
    new_st.expr = exp; 
 
    *if_type = ST_IF_BLOCK;   
    return MATCH_YES;     
  }          
          
  if (k == MATCH_YES) {  
    g95_error("Block label is not appropriate IF statement at %C");       
       
    g95_free_expr(exp);
    return MATCH_ERROR;         
  }         
         
/* At this point the only thing left is a simple IF statement.  At
 * this point, n has to be MATCH_NO, so we don't have to worry about
 * re-matching a block label.  From what we've got so far, try
 * matching an assignment. */  
  
  *if_type = ST_SIMPLE_IF;   
   
  mm = g95_match_assignment();  
  if (mm == MATCH_YES) goto got_match;       
       
  g95_free_expr(exp);   
  g95_undo_symbols();     
  g95_set_locus(&o);        
        
  g95_match(" if ( %e ) ", &exp);  /* Guaranteed to match */      
      
  mm = g95_match_pointer_assignment();          
  if (mm == MATCH_YES) goto got_match;          
          
  g95_free_expr(exp);        
  g95_undo_symbols();      
  g95_set_locus(&o);        
        
  g95_match(" if ( %e ) ", &exp);  /* Guaranteed to match */          
          
/* Look at the next keyword to see which matcher to call.  Matching
 * the keyword doesn't affect the symbol table, so we don't have to
 * restore between tries. */     
     
#define match(string, subr, statement) \
  if (g95_match(string) == MATCH_YES) { mm = subr(); goto got_match; }
       
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

  g95_free_expr(exp);    
  return MATCH_ERROR;        
        
got_match:
  if (mm == MATCH_NO) g95_error("Syntax error in IF-clause at %C");    
  if (mm != MATCH_YES) {    
    g95_free_expr(exp);        
    return MATCH_ERROR;
  }  
  
/* At this point, we've matched the single IF and the action clause is
 * in new_st.  Rearrange things so that the IF statement appears in new_st */    
    
  f = g95_get_code();       
  *f = new_st;
  f->loc = *g95_current_locus();

  g95_clear_new_st();     
     
  new_st.type = EXEC_IF;  
  new_st.block = f;  
  new_st.expr = exp;        
        
  return MATCH_YES;  
}      
      
#undef match
      
      
 
 
/* g95_free_iterator()-- Free a g95_iterator structure */  
  
void g95_free_iterator(g95_iterator *it, int flag) {        
        
  if (it == NULL) return;        
        
  g95_free_expr(it->var);     
  g95_free_expr(it->start);      
  g95_free_expr(it->end);  
  g95_free_expr(it->step);

  if (flag) g95_free(it);       
}      
      
      
      
      
/* g95_match_st_label()-- Matches a statement label.  Uses
 * g95_match_small_literal_int() to do most of the work. */    
    
match g95_match_st_label(g95_st_label **labl, int allow_zero) { 
int label_value;
locus old;          
match q;      
      
  old = *g95_current_locus();        
        
  q = g95_match_small_literal_int(&label_value);        
  if (q != MATCH_YES) return q;          
          
  if ((label_value == 0 && allow_zero) || label_value <= 99999) {       
    *labl = g95_get_st_label(label_value);   
    return MATCH_YES;     
  }     
     
  g95_error("Statement label at %C is out of range");       
  g95_set_locus(&old);   
  return MATCH_ERROR;         
}          
          
          
      
      
/* g95_match_elseif()-- Match an ELSE IF statement */     
     
match g95_match_elseif(void) {  
char name[G95_MAX_SYMBOL_LEN+1]; 
g95_expr *e2;          
match g;    
    
  g = g95_match(" ( %e ) then", &e2); 
  if (g != MATCH_YES) return g;          
          
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
  new_st.type = EXEC_IF;   
  new_st.expr = e2;    
  return MATCH_YES;          
          
cleanup:    
  g95_free_expr(e2);      
  return MATCH_ERROR;     
}         
         
         
        
        
/* g95_match_space()-- In free form, match at least one space.  Always
 * matches in fixed form. */  
  
match g95_match_space(void) {        
locus oldl;
int z;   
   
  if (g95_current_file->form == FORM_FIXED) return MATCH_YES;          
          
  oldl = *g95_current_locus();         
         
  z = g95_next_char();          
  if (!g95_is_whitespace(z)) {         
    g95_set_locus(&oldl);    
    return MATCH_NO;    
  }       
       
  g95_gobble_whitespace(); 
 
  return MATCH_YES;        
}


        
        
static match match_data_constant(g95_expr **res) {  
char n[G95_MAX_SYMBOL_LEN+1];          
g95_symbol *symbol;          
g95_expr *e2; 
match s;

  s = g95_match_literal_constant(&e2, 1);
  if (s == MATCH_YES) {        
    *res = e2;      
    return MATCH_YES;
  } 
 
  if (s == MATCH_ERROR) return MATCH_ERROR;          
          
  s = g95_match_null(res);       
  if (s != MATCH_NO) return s;

  s = g95_match_name(n);     
  if (s != MATCH_YES) return s;        
        
  if (g95_find_symbol(n, NULL, 1, &symbol)) return MATCH_ERROR;

  if (symbol == NULL || symbol->attr.flavor != FL_PARAMETER) {          
    g95_error("Symbol '%s' must be a PARAMETER in DATA statement at %C",       
	      n);       
    return MATCH_ERROR;      
  }

  *res = g95_copy_expr(symbol->value);          
  return MATCH_YES;          
}     
     
     
  
  
/* g95_match_return()-- Match a RETURN statement */

match g95_match_return(void) { 
g95_expr *e; 
match c; 
 
  e = NULL;      
  if (g95_match_eos() == MATCH_YES) goto done;      
      
  if (g95_find_state(COMP_SUBROUTINE) == FAILURE) {        
    g95_error("Alternate RETURN statement at %C is only allowed within "          
	      "a SUBROUTINE");   
    goto cleanup;
  }          
          
  c = g95_match("% %e%t", &e);        
  if (c == MATCH_YES) goto done;     
  if (c == MATCH_ERROR) goto cleanup;          
          
  g95_syntax_error(ST_RETURN); 
 
cleanup:         
  g95_free_expr(e);   
  return MATCH_ERROR; 
 
done:         
  new_st.type = EXEC_RETURN;     
  new_st.expr = e;      
      
  return MATCH_YES;          
}   
   
   
  
  
/* g95_match_iterator()-- Match a loop control phrase:
 *
 *     <LVALUE> = <EXPR>, <EXPR> [, <EXPR> ]
 *
 * If the final integer expression is not present, a constant unity
 * expression is returned.  We don't return MATCH_ERROR until after
 * the equals sign is seen. */     
     
match g95_match_iterator(g95_iterator *it, int init_flag) {      
char name0[G95_MAX_SYMBOL_LEN+1];
g95_expr *v, *n, *k, *e3;       
locus start;        
match l; 
 
  /* Match the start of an iterator without affecting the symbol table */          
          
  start = *g95_current_locus();           
  l = g95_match(" %n =", name0);   
  g95_set_locus(&start);      
      
  if (l != MATCH_YES) return MATCH_NO;

  l = g95_match_variable(&v, 0);
  if (l != MATCH_YES) return MATCH_NO;          
          
  g95_match_char('=');  
  
  n = k = e3 = NULL;  
  
  if (v->ref != NULL) {
    g95_error("Loop variable at %C cannot be a sub-component");        
    goto cleanup;  
  }

  if (v->symbol->attr.intent == INTENT_IN) {
    g95_error("Loop variable '%s' at %C cannot be INTENT(IN)",
	      v->symbol->name);
    goto cleanup;
  }      
      
  if (v->symbol->attr.pointer) { 
    g95_error("Loop variable at %C cannot have the POINTER attribute");          
    goto cleanup;     
  }       
       
  l = init_flag ? g95_match_init_expr(&n) : g95_match_expr(&n);          
  if (l == MATCH_NO) goto syntax;  
  if (l == MATCH_ERROR) goto cleanup;        
        
  if (g95_match_char(',') != MATCH_YES) goto syntax;       
       
  l = init_flag ? g95_match_init_expr(&k) : g95_match_expr(&k);       
  if (l == MATCH_NO) goto syntax;
  if (l == MATCH_ERROR) goto cleanup;         
         
  if (g95_match_char(',') != MATCH_YES) {     
    e3 = g95_int_expr(1);    
    goto done;    
  }          
          
  l = init_flag ? g95_match_init_expr(&e3) : g95_match_expr(&e3); 
  if (l == MATCH_ERROR) goto cleanup;    
  if (l == MATCH_NO) {        
    g95_error("Expected a step value in iterator at %C");     
    goto cleanup;     
  } 
 
done:          
  it->var = v;    
  it->start = n;  
  it->end = k;    
  it->step = e3;       
  return MATCH_YES;          
          
syntax:   
  g95_error("Syntax error in iterator at %C");          
             
cleanup:    
  g95_free_expr(n);    
  g95_free_expr(k);        
  g95_free_expr(e3);   
   
  return MATCH_ERROR;    
}         
         
         
       
       
/* g95_match_deallocate()-- Match a DEALLOCATE statement */    
    
match g95_match_deallocate(void) {     
g95_alloc *start, *tail;
g95_expr *stat;  
match c;          
          
  start = tail = NULL;     
  stat = NULL;    
    
  if (g95_match_char('(') != MATCH_YES) goto syntax; 
 
  for(;;) {     
    if (start == NULL)          
      start = tail = g95_get_alloc(); 
    else {   
      tail->next = g95_get_alloc();     
      tail = tail->next; 
    }       
       
    c = g95_match_variable(&tail->expr, 0);     
    if (c == MATCH_ERROR) goto cleanup;        
    if (c == MATCH_NO) goto syntax;    
    
    if (g95_pure(NULL) && g95_impure_variable(tail->expr->symbol)) {       
      g95_error("Illegal deallocate-expression in DEALLOCATE at %C for a PURE "      
		"procedure");        
      goto cleanup;
    }        
        
    if (g95_match_char(',') != MATCH_YES) break;  
  
    c = g95_match(" stat = %v", &stat);        
    if (c == MATCH_ERROR) goto cleanup;      
    if (c == MATCH_YES) break;     
  }         
         
  if (stat != NULL && stat->symbol->attr.intent == INTENT_IN) {          
    g95_error("STAT variable '%s' of DEALLOCATE statement at %C cannot be "   
	      "INTENT(IN)", stat->symbol->name); 
    goto cleanup;          
          
    if (g95_pure(NULL) && g95_impure_variable(stat->symbol)) {       
      g95_error("Illegal STAT variable in DEALLOCATE statement at %C for "      
		"a PURE procedure"); 
      goto cleanup;         
    }        
        
    g95_check_do_variable(stat->symbol);   
  }          
          
  if (g95_match(" )%t") != MATCH_YES) goto syntax;

  new_st.type = EXEC_DEALLOCATE;          
  new_st.expr = stat;          
  new_st.ext.alloc_list = start;    
    
  return MATCH_YES;   
   
syntax:      
  g95_syntax_error(ST_DEALLOCATE);         
         
cleanup:          
  g95_free_expr(stat);   
  g95_free_alloc_list(start);    
  return MATCH_ERROR;        
}         
         
         
   
   
/* g95_match_stop()-- Match the STOP statement.  We can't match a
 * label here because labels can't be zero and a stop code can. */

match g95_match_stop(void) {   
int stop_code;    
g95_expr *a;         
match c;  
  
  stop_code = -1;   /* blank stop */    
  a = NULL;

  if (g95_match_eos() != MATCH_YES) {        
    c = g95_match_small_literal_int(&stop_code);     
    if (c == MATCH_ERROR) goto cleanup;         
         
    if (c == MATCH_YES && stop_code > 99999) {     
      g95_error("STOP code out of range at %C");
      goto cleanup;      
    }      
      
    if (c == MATCH_NO) {  /* Try a character constant */         
      c = g95_match_expr(&a);   
      if (c == MATCH_ERROR) goto cleanup;     
      if (c == MATCH_NO) goto syntax;    
    
      if (a->ts.type != BT_CHARACTER || a->type != EXPR_CONSTANT)          
	goto syntax;   
    }  
  
    if (g95_match_eos() != MATCH_YES) goto syntax; 
  }   
   
  if (g95_pure(NULL)) {    
    g95_error("STOP statement not allowed in PURE procedure at %C");         
    goto cleanup;      
  }       
       
  new_st.type = EXEC_STOP;   
  new_st.expr = a;       
  new_st.ext.stop_code = stop_code;        
        
  return MATCH_YES;          
          
syntax:      
  g95_syntax_error(ST_STOP);       
       
cleanup:      
  g95_free_expr(a);     
  return MATCH_ERROR;          
}


         
         
/* g95_match_small_int()-- Match a small, constant integer expression,
 * like in a kind statement.  On MATCH_YES, 'value' is set. */ 
 
match g95_match_small_int(int *value) { 
g95_expr *e1;     
char *a;     
match y;
int t;     
     
  y = g95_match_expr(&e1);        
  if (y != MATCH_YES) return y;         
         
  a = g95_extract_int(e1, &t);
  g95_free_expr(e1);       
       
  if (a != NULL) {   
    g95_error(a);          
    y = MATCH_ERROR;   
  }          
          
  *value = t;       
  return y;         
}   
   
   
 
 
/* g95_match_eos()-- Match an end of statement.  End of statement is
 * optional whitespace, followed by a ';' or '\n' or comment '!'.  If
 * a semicolon is found, we continue to eat whitespace and semicolons. */      
      
match g95_match_eos(void) {    
locus where;     
int flag, z;

  flag = 0;     
     
  for(;;) {    
    where = *g95_current_locus();
    g95_gobble_whitespace();      
         
    z = g95_next_char();       
    switch(z) {  
    case '!':    
      do {     
	z = g95_next_char();  
      } while(z != '\n');    
    
      /* Fall through */   
    case '\n':     
      return MATCH_YES;    
    
    case ';':         
      flag = 1;    
      continue;      
    }          
          
    break;   
  }       
       
  g95_set_locus(&where);         
  return (flag) ? MATCH_YES : MATCH_NO;
}        
        
        


/* g95_match_continue()-- match a CONTINUE statement */          
          
match g95_match_continue(void) {          
          
  if (g95_match_eos() != MATCH_YES) { 
    g95_syntax_error(ST_CONTINUE);    
    return MATCH_ERROR;      
  }

  new_st.type = EXEC_CONTINUE;          
  return MATCH_YES;      
}      
      
      
   
   
/* match_common_name()-- Match a common block name.  Returns a null
 * string for the blank common. */       
       
static match match_common_name(char *n) {       
match s;        
        
  if (g95_match_char('/') == MATCH_NO) { 
    n[0] = '\0';        
    return MATCH_YES;      
  } 
 
  if (g95_match_char('/') == MATCH_YES) {     
    n[0] = '\0';        
    return MATCH_YES;         
  } 
 
  s = g95_match_name(n);        
        
  if (s == MATCH_ERROR) return MATCH_ERROR;          
  if (s == MATCH_YES && g95_match_char('/') == MATCH_YES) return MATCH_YES;

  g95_error("Syntax error in common block name at %C");        
  return MATCH_ERROR;
}      
      
      
  
  
/* g95_free_namelist()-- Free a namelist structure */   
   
void g95_free_namelist(g95_namelist *nam) {    
g95_namelist *x;   
   
  for(;nam; nam=x) {     
    x = nam->next;    
    g95_free(nam);    
  }  
}


         
         
/* g95_match_intrinsic_op()-- Match an intrinsic operator.  Returns an
 * INTRINSIC enum. */     
     
match g95_match_intrinsic_op(g95_intrinsic_op *r) {
g95_intrinsic_op op1;  
static mstring operators_in[] = {
  minit("+",     INTRINSIC_PLUS),   minit("-",      INTRINSIC_MINUS),       
  minit("**",    INTRINSIC_POWER),  minit("//",     INTRINSIC_CONCAT),  
  minit("*",     INTRINSIC_TIMES),  minit("/",      INTRINSIC_DIVIDE),      
  minit(".and.", INTRINSIC_AND),    minit(".or.",   INTRINSIC_OR),  
  minit(".eqv.", INTRINSIC_EQV),    minit(".neqv.", INTRINSIC_NEQV),          
  minit(".eq.",  INTRINSIC_EQ),     minit("==",     INTRINSIC_EQ),          
  minit(".ne.",  INTRINSIC_NE),     minit("/=",     INTRINSIC_NE),  
  minit(".ge.",  INTRINSIC_GE),     minit(">=",     INTRINSIC_GE),    
  minit(".le.",  INTRINSIC_LE),     minit("<=",     INTRINSIC_LE),          
  minit(".lt.",  INTRINSIC_LT),     minit("<",      INTRINSIC_LT),    
  minit(".gt.",  INTRINSIC_GT),     minit(">",      INTRINSIC_GT), 
  minit(".not.", INTRINSIC_NOT),    minit(NULL,     INTRINSIC_NONE) };      
      
  op1 = g95_match_strings(operators_in); 
  if (op1 == INTRINSIC_NONE) return MATCH_NO; 
 
  *r = op1;    
  return MATCH_YES;         
}     
     
     
char *g95_op2string(int o) {    
static mstring operators_out[] = {        
  minit("+",     INTRINSIC_PLUS),   minit("-",      INTRINSIC_MINUS),  
  minit("+",     INTRINSIC_UPLUS),  minit("-",      INTRINSIC_UMINUS), 
  minit("**",    INTRINSIC_POWER),  minit("//",     INTRINSIC_CONCAT),      
  minit("*",     INTRINSIC_TIMES),  minit("/",      INTRINSIC_DIVIDE),  
  minit(".and.", INTRINSIC_AND),    minit(".or.",   INTRINSIC_OR),       
  minit(".eqv.", INTRINSIC_EQV),    minit(".neqv.", INTRINSIC_NEQV),
  minit(".eq.",  INTRINSIC_EQ),     minit("==",     INTRINSIC_EQ),         
  minit(".ne.",  INTRINSIC_NE),     minit("/=",     INTRINSIC_NE), 
  minit(".ge.",  INTRINSIC_GE),     minit(">=",     INTRINSIC_GE),   
  minit(".le.",  INTRINSIC_LE),     minit("<=",     INTRINSIC_LE),         
  minit(".lt.",  INTRINSIC_LT),     minit("<",      INTRINSIC_LT),      
  minit(".gt.",  INTRINSIC_GT),     minit(">",      INTRINSIC_GT),
  minit(".not.", INTRINSIC_NOT),    minit(NULL,     INTRINSIC_NONE) };        
        
  if (o == INTRINSIC_ASSIGN) return "=";     
  return g95_code2string(operators_out, o);    
}   
   
   


/* g95_get_common()-- Given a name, return a pointer to the common
 * head structure, creating it if it does not exist. */        
        
g95_common_head *g95_get_common(char *name0) { 
g95_symtree *st1;      
      
  st1 = g95_find_symtree(g95_current_ns->common_root, name0);    
  if (st1 == NULL) st1 = g95_new_symtree(&g95_current_ns->common_root, name0);  
  
  if (st1->n.common == NULL) { 
    st1->n.common = g95_get_common_head();      
    st1->n.common->where = *g95_current_locus();       
  }        
        
  return st1->n.common; 
}        
        
        
    
    
/* g95_match_nullify()-- Match a NULLIFY statement. A NULLIFY
 * statement is transformed into a set of pointer assignments to
 * intrinsic NULL(). */         
         
match g95_match_nullify(void) {
g95_code *tail;         
g95_expr *i, *j;     
match w;  
  
  tail = NULL;

  if (g95_match_char('(') != MATCH_YES) goto syntax;          
          
  for(;;) {  
    w = g95_match_variable(&j, 0);     
    if (w == MATCH_ERROR) goto cleanup;      
    if (w == MATCH_NO) goto syntax;  
  
    if (g95_pure(NULL) && g95_impure_variable(j->symbol)) { 
      g95_error("Illegal variable in NULLIFY at %C for a PURE procedure");     
      goto cleanup;        
    }  
  
    /* build ' => NULL() ' */ 
    i = g95_get_expr();        
    i->where = *g95_current_locus();    
    i->type = EXPR_NULL;  
    i->ts.type = BT_UNKNOWN;    
    
    /* Chain to list */       
    if (tail == NULL)  
      tail = &new_st;    
    else {      
      tail->next = g95_get_code();
      tail = tail->next;    
    }         
         
    tail->type = EXEC_POINTER_ASSIGN; 
    tail->expr = j;     
    tail->expr2 = i;    
    
    if (g95_match_char(')') == MATCH_YES) break;         
    if (g95_match_char(',') != MATCH_YES) goto syntax;      
  }       
       
  return MATCH_YES;          
          
syntax:          
  g95_syntax_error(ST_NULLIFY);        
        
cleanup:         
  g95_free_statements(tail);      
  return MATCH_ERROR;
}   
   
   
  
  
/* g95_match_program()-- Matches the start of a program unit, which is
 * the program keyword followed by an optional symbol. */

match g95_match_program(void) {      
g95_symbol *s;         
match v;         
         
  v = g95_match_eos();
  if (v == MATCH_YES) return v;      
      
  v = g95_match("% %s%t", &s);   
   
  if (v == MATCH_NO) {      
    g95_error("Invalid form of PROGRAM statement at %C");  
    v = MATCH_ERROR; 
  }

  if (v == MATCH_ERROR) return v;        
        
  if (g95_add_flavor(&s->attr, FL_PROGRAM, NULL) == FAILURE)  
    return MATCH_ERROR; 
 
  g95_new_block = s;        
        
  return MATCH_YES;         
}     
     
     
          
          
/* g95_match_allocate()-- Match an ALLOCATE statement */          
          
match g95_match_allocate(void) {          
g95_alloc *h, *end; 
g95_expr *stat;  
match z;          
          
  h = end = NULL;       
  stat = NULL;

  if (g95_match_char('(') != MATCH_YES) goto syntax;      
      
  for(;;) {     
    if (h == NULL)     
      h = end = g95_get_alloc();    
    else {         
      end->next = g95_get_alloc();         
      end = end->next;       
    }          
          
    z = g95_match_variable(&end->expr, 0);   
    if (z == MATCH_NO) goto syntax; 
    if (z == MATCH_ERROR) goto cleanup;   
   
    if (g95_pure(NULL) && g95_impure_variable(end->expr->symbol)) {      
      g95_error("Bad allocate-object in ALLOCATE statement at %C for a "  
		"PURE procedure");   
      goto cleanup;    
    }

    if (g95_match_char(',') != MATCH_YES) break;

    z = g95_match(" stat = %v", &stat);
    if (z == MATCH_ERROR) goto cleanup;   
    if (z == MATCH_YES) break; 
  }

  if (stat != NULL) {         
    if (stat->symbol->attr.intent == INTENT_IN) {      
      g95_error("STAT variable '%s' of ALLOCATE statement at %C cannot be "
		"INTENT(IN)", stat->symbol->name);
      goto cleanup;      
    }         
         
    if (g95_pure(NULL) && g95_impure_variable(stat->symbol)) {  
      g95_error("Illegal STAT variable in ALLOCATE statement at %C for a PURE "  
		"procedure");         
      goto cleanup;    
    }

    g95_check_do_variable(stat->symbol);          
  }

  if (g95_match(" )%t") != MATCH_YES) goto syntax;        
        
  new_st.type = EXEC_ALLOCATE;         
  new_st.expr = stat;     
  new_st.ext.alloc_list = h;        
        
  return MATCH_YES;

syntax:  
  g95_syntax_error(ST_ALLOCATE);        
        
cleanup:        
  g95_free_expr(stat);    
  g95_free_alloc_list(h);
  return MATCH_ERROR;     
} 
 
 
          
          
/* g95_match_where()-- Match a WHERE statement */         
         
match g95_match_where(g95_statement *s) { 
g95_expr *e1;         
match f, n;         
g95_code *z;    
    
  f = g95_match_label();       
  if (f == MATCH_ERROR) return f; 
 
  n = g95_match(" where ( %e )", &e1);       
  if (n != MATCH_YES) return n;         
         
  if (g95_match_eos() == MATCH_YES) {
    *s = ST_WHERE_BLOCK;

    new_st.type = EXEC_WHERE;      
    new_st.expr = e1;      
    return MATCH_YES;          
  }     
     
  n = g95_match_assignment();         
  if (n == MATCH_NO) g95_syntax_error(ST_WHERE);

  if (n != MATCH_YES) {        
    g95_free_expr(e1);         
    return MATCH_ERROR;     
  }         
         
/* We've got a simple WHERE statement */       
       
  *s = ST_WHERE; 
  z = g95_get_code();

  z->type = EXEC_WHERE;         
  z->expr = e1;      
  z->next = g95_get_code();   
   
  *z->next = new_st;        
  g95_clear_new_st();        
        
  new_st.type = EXEC_WHERE;          
  new_st.block = z;

  return MATCH_YES;    
}    
    
    
     
     
/* g95_match_else()-- Match an ELSE statement */         
         
match g95_match_else(void) {    
char n[G95_MAX_SYMBOL_LEN+1];  
   
  if (g95_match_eos() == MATCH_YES) return MATCH_YES;  
  
  if (g95_match_name(n) != MATCH_YES || g95_current_block() == NULL ||         
      g95_match_eos() != MATCH_YES) { 
    g95_error("Unexpected junk after ELSE statement at %C");       
    return MATCH_ERROR; 
  }    
    
  if (strcmp(n, g95_current_block()->name) != 0) {       
    g95_error("Label '%s' at %C doesn't match IF label '%s'",  
	      n, g95_current_block()->name); 
    return MATCH_ERROR;   
  } 
 
  return MATCH_YES;   
}


        
        
/* g95_match_elsewhere()-- Match an ELSEWHERE statement.  We leave
 * behind a WHERE node in new_st if successful. */   
   
match g95_match_elsewhere(void) {         
char n[G95_MAX_SYMBOL_LEN+1];     
g95_expr *e1;     
match b;      
      
  if (g95_current_state() != COMP_WHERE) {  
    g95_error("ELSEWHERE statement at %C not enclosed in WHERE block");         
    return MATCH_ERROR;      
  }         
         
  e1 = NULL;    
    
  if (g95_match_char('(') == MATCH_YES) {          
    b = g95_match_expr(&e1);      
    if (b == MATCH_NO) goto syntax;     
    if (b == MATCH_ERROR) return MATCH_ERROR;      
      
    if (g95_match_char(')') != MATCH_YES) goto syntax;      
  }       
       
  if (g95_match_eos() != MATCH_YES) { /* Better be a name at this point */        
    b = g95_match_name(n);          
    if (b == MATCH_NO) goto syntax;    
    if (b == MATCH_ERROR) goto cleanup;     
     
    if (g95_match_eos() != MATCH_YES) goto syntax;  
  
    if (strcmp(n, g95_current_block()->name) != 0) {
      g95_error("Label '%s' at %C doesn't match WHERE label '%s'",         
		n, g95_current_block()->name);  
      goto cleanup;  
    }     
  }   
   
  new_st.type = EXEC_WHERE;   
  new_st.expr = e1; 
  return MATCH_YES;       
       
syntax:         
  g95_syntax_error(ST_ELSEWHERE);

cleanup:     
  g95_free_expr(e1);    
  return MATCH_ERROR;        
}      
      
      
          
          
/* g95_match_label()-- Match and validate a label associated with a
 * named IF, DO or SELECT statement.  If the symbol does not have the
 * label attribute, we add it.  We also make sure the symbol does not
 * refer to another (active) block.  A matched label is pointed to by
 * g95_new_block. */ 
 
match g95_match_label(void) {    
char n[G95_MAX_SYMBOL_LEN+1];         
g95_state_data *u;
match e;      
      
  g95_new_block = NULL;

  e = g95_match(" %n :", n);  
  if (e != MATCH_YES) return e;       
       
  if (g95_get_symbol(n, NULL, &g95_new_block)) {
    g95_error("Label name '%s' at %C is ambiguous", n);    
    return MATCH_ERROR;     
  }          
          
  if (g95_new_block->attr.flavor != FL_LABEL &&      
      g95_add_flavor(&g95_new_block->attr, FL_LABEL, NULL) == FAILURE)       
    return MATCH_ERROR;    
    
  for(u=g95_state_stack; u; u=u->previous)    
    if (u->sym == g95_new_block) {  
      g95_error("Label %s at %C already in use by a parent block", 
		g95_new_block->name);         
      return MATCH_ERROR;   
    }          
          
  return MATCH_YES;     
}  
  
  
 
 
/* g95_match_module()-- Match a MODULE statement */      
      
match g95_match_module(void) {     
match j;       
       
  j = g95_match(" %s%t", &g95_new_block);        
  if (j != MATCH_YES) return j;          
          
  if (g95_add_flavor(&g95_new_block->attr, FL_MODULE, NULL) == FAILURE)    
    return MATCH_ERROR; 
 
  return MATCH_YES;  
}


 
 
/* g95_free_alloc_list()-- Frees a list of g95_alloc structures */          
          
void g95_free_alloc_list(g95_alloc *j) {         
g95_alloc *n;      
      
  for(; j; j=n) {       
    n = j->next;          
    g95_free_expr(j->expr);     
    g95_free(j);  
  } 
}       
       
       
  
  
/* match_exit_cycle()-- Match an EXIT or CYCLE statement */     
     
static match match_exit_cycle(g95_statement st1, g95_exec_op operand) {       
g95_state_data *r; 
g95_symbol *sy;        
match w;         
         
  if (g95_match_eos() == MATCH_YES)      
    sy = NULL;
  else {      
    w = g95_match("% %s%t", &sy);         
    if (w == MATCH_ERROR) return MATCH_ERROR;     
    if (w == MATCH_NO) {          
      g95_syntax_error(st1);   
      return MATCH_ERROR;        
    }

    if (sy->attr.flavor != FL_LABEL) { 
      g95_error("Name '%s' in %s statement at %C is not a loop name",          
		sy->name, g95_ascii_statement(st1));          
      return MATCH_ERROR;     
    }        
  }    
    
/* Find the loop mentioned specified by the label (or lack of a label) */         
         
  for(r=g95_state_stack; r; r=r->previous)       
    if (r->state == COMP_DO && (sy == NULL || sy == r->sym)) break;         
         
  if (r == NULL) { 
    if (sy == NULL)  
      g95_error("%s statement at %C is not within a loop",
		g95_ascii_statement(st1));     
    else 
      g95_error("%s statement at %C is not within loop '%s'",        
		g95_ascii_statement(st1), sy->name);      
      
    return MATCH_ERROR;     
  }       
       
  /* Save the first statement in the loop - needed by the backend */          
          
  new_st.ext.block = r->top;       
  new_st.type = operand;          
  new_st.sym = sy;

  return MATCH_YES;         
}         
         
         
      
      
/* g95_match_equivalence()-- Match an EQUIVALENCE statement */ 
 
match g95_match_equivalence(void) {          
g95_equiv *q, *set, *end;         
g95_ref *re;   
match g;         
         
  end = NULL;       
       
  for(;;) {    
    q = g95_get_equiv();    
    if (end == NULL) end = q;          
          
    q->next = g95_current_ns->equiv;         
    g95_current_ns->equiv = q;       
       
    if (g95_match_char('(') != MATCH_YES) goto syntax;

    set = q;         
         
    for(;;) {      
      g = g95_match_variable(&set->expr, 1);  
      if (g == MATCH_ERROR) goto cleanup;         
      if (g == MATCH_NO) goto syntax;

      for(re=set->expr->ref; re; re=re->next)        
	if (re->type == REF_ARRAY && re->u.ar.type == AR_SECTION) {         
	  g95_error("Array reference in EQUIVALENCE at %C cannot be an "    
		    "array section"); 
	  goto cleanup; 
	}    
    
      if (g95_match_char(')') == MATCH_YES) break; 
      if (g95_match_char(',') != MATCH_YES) goto syntax;          
          
      set->eq = g95_get_equiv();     
      set = set->eq;    
    }    
    
    if (g95_match_eos() == MATCH_YES) break;     
    if (g95_match_char(',') != MATCH_YES) goto syntax;     
  } 
 
  return MATCH_YES;

syntax:  
  g95_syntax_error(ST_EQUIVALENCE);      
      
cleanup:   
  q = end->next;       
  end->next = NULL;       
       
  g95_free_equiv(g95_current_ns->equiv);    
  g95_current_ns->equiv = q;

  return MATCH_ERROR;   
}


      
      
/* g95_match_exit()-- Match the EXIT statement */     
     
match g95_match_exit(void) {  
  
  return match_exit_cycle(ST_EXIT,  EXEC_EXIT);     
}       
       
       
          
          
/* g95_match_pointer_assignment()-- Match a pointer assignment statement */   
   
match g95_match_pointer_assignment(void) {       
g95_expr *lvalue, *rvalue;    
locus old_loc;  
match n;       
       
  old_loc = *g95_current_locus();     
     
  lvalue = rvalue = NULL;          
          
  n = g95_match(" %v =>", &lvalue);   
  if (n != MATCH_YES) { n = MATCH_NO; goto cleanup; } 
 
  n = g95_match(" %e%t", &rvalue);    
  if (n != MATCH_YES) goto cleanup;    
    
  new_st.type = EXEC_POINTER_ASSIGN;        
  new_st.expr = lvalue; 
  new_st.expr2 = rvalue;      
      
  return MATCH_YES;   
   
cleanup:  
  g95_set_locus(&old_loc);  
  g95_free_expr(lvalue);          
  g95_free_expr(rvalue);       
  return n; 
} 
 
 
     
     
/* g95_match_call()-- Match a CALL statement.  The tricky part here
 * are possible alternate return specifiers.  We handle these by
 * having all "subroutines" actually return an integer via a register
 * that gives the return number.  If the call specifies alternate
 * returns, we generate code for a SELECT statement whose case clauses
 * contain GOTOs to the various labels. */        
        
match g95_match_call(void) {
char nm[G95_MAX_SYMBOL_LEN+1];        
g95_actual_arglist *o, *args;    
g95_case *new_case; 
g95_symbol *s;      
g95_code *v;  
match n;    
int t;  
  
  args = NULL;          
          
  n = g95_match("% %n", nm);        
  if (n == MATCH_NO) goto syntax;        
  if (n != MATCH_YES) return n; 
 
  if (g95_get_ha_symbol(nm, &s)) return MATCH_ERROR;        
        
  if (!s->attr.generic && !s->attr.subroutine && 
      g95_add_subroutine(&s->attr, NULL) == FAILURE)
    return MATCH_ERROR;  
  
  if (g95_match_eos() != MATCH_YES) {       
    n = g95_match_actual_arglist(1, &args);     
    if (n == MATCH_NO) goto syntax;   
    if (n == MATCH_ERROR) goto cleanup;       
       
    if (g95_match_eos() != MATCH_YES) goto syntax;        
  }  
  
/* If any alternate return labels were found, construct a SELECT
 * statement that will jump to the right place */         
         
  if (g95_has_alt_return(args)) {   
    new_st.next = v = g95_get_code();         
    v->type = EXEC_SELECT;       
    v->expr = g95_int_expr(0);  /* For now */

    t = 0;    
    for(o=args; o; o=o->next) {         
      if (o->type != ALT_RETURN) continue;   
   
      if (g95_reference_st_label(o->u.label, ST_LABEL_TARGET) == FAILURE)          
        continue;          
          
      t++;   
   
      v->block = g95_get_code();          
      v = v->block;     
      v->type = EXEC_SELECT;  
  
      new_case = g95_get_case();
      new_case->high = new_case->low = g95_int_expr(t);       
      v->ext.case_list = new_case;  
  
      v->next = g95_get_code();       
      v->next->type = EXEC_GOTO;    
      v->next->label = o->u.label;     
    }   
  }         
         
  new_st.type = EXEC_CALL;    
  new_st.sym = s;        
  new_st.ext.actual = args;        
  return MATCH_YES;

syntax:
  g95_syntax_error(ST_CALL);      
      
cleanup:       
  g95_free_actual_arglist(args);         
  return MATCH_ERROR;  
} 
 
 
    
    
/* g95_match_goto()-- Match the GO TO statement.  As a computed GOTO
 * statement is matched, it is transformed into an equivalent SELECT
 * block.  No tree is necessary, and the resulting jumps-to-jumps are
 * specifically optimized away by the back end. */   
   
match g95_match_goto(void) {         
g95_code *start, *tail;      
g95_expr *e2;  
g95_case *cp;     
g95_st_label *l;      
int p;
match f;         
         
  if (g95_match(" %l%t", &l) == MATCH_YES) {      
    if (g95_reference_st_label(l, ST_LABEL_TARGET) == FAILURE)     
      return MATCH_ERROR;

    new_st.type = EXEC_GOTO;    
    new_st.label = l;      
    return MATCH_YES;          
  }  
  
/* The assigned GO TO statement is not allowed in Fortran 95 */   
   
  if (g95_match_variable(&e2, 0) == MATCH_YES) {       
    g95_free_expr(e2); 
    g95_error("The assigned GO TO statement at %C is not allowed in "  
	      "Fortran 95");  
    return MATCH_ERROR;         
  }  
  
/* Last chance is a computed GO TO statement */        
        
  if (g95_match_char('(') != MATCH_YES) {     
    g95_syntax_error(ST_GOTO);        
    return MATCH_ERROR;
  }          
          
  start = tail = NULL;    
  p = 1;          
          
  do {      
    f = g95_match_st_label(&l, 0);      
    if (f != MATCH_YES) goto syntax; 
 
    if (g95_reference_st_label(l, ST_LABEL_TARGET) == FAILURE)  
      goto cleanup; 
 
    if (start == NULL) 
      start = tail = g95_get_code();     
    else {     
      tail->block = g95_get_code();       
      tail = tail->block;   
    }        
        
    cp = g95_get_case();      
    cp->low = cp->high = g95_int_expr(p++);

    tail->type = EXEC_SELECT;  
    tail->ext.case_list = cp;         
         
    tail->next = g95_get_code();     
    tail->next->type = EXEC_GOTO; 
    tail->next->label = l;       
  } while(g95_match_char(',') == MATCH_YES);   
   
  if (g95_match_char(')') != MATCH_YES) goto syntax;     
     
  if (start == NULL) {     
    g95_error("Statement label list in GOTO at %C cannot be empty");       
    goto syntax;  
  }    
    
/* Get the rest of the statement */      
      
  g95_match_char(',');          
          
  if (g95_match(" %e%t", &e2) != MATCH_YES) goto syntax;     
     
/* At this point, a computed GOTO has been fully matched and an
 * equivalent SELECT statement constructed. */   
   
  new_st.type = EXEC_SELECT;         
  new_st.expr = NULL;          
  /* For a "real" SELECT, the expression is in expr. We put it in expr2. */      
  new_st.expr2 = e2;  
  new_st.block = start;          
  return MATCH_YES;     
     
syntax:  
  g95_syntax_error(ST_GOTO);        
cleanup:         
  g95_free_statements(start);       
  return MATCH_ERROR;          
}       
       
       
       
       
/* g95_match_assignment(void)-- Match a simple assignment statement */        
        
match g95_match_assignment(void) {    
g95_expr *lvalue, *rvalue;        
locus where;        
match p;    
    
  where = *g95_current_locus();  
 
  lvalue = rvalue = NULL;        
  p = g95_match(" %v =", &lvalue);         
  if (p != MATCH_YES) goto cleanup;      
      
  p = g95_match(" %e%t", &rvalue);   
  if (p != MATCH_YES) goto cleanup;     
     
  new_st.type = EXEC_ASSIGN;    
  new_st.expr = lvalue;     
  new_st.expr2 = rvalue;       
       
  g95_check_do_variable(lvalue->symbol);     
     
  return MATCH_YES;

cleanup:
  g95_set_locus(&where);
  g95_free_expr(lvalue);         
  g95_free_expr(rvalue);       
  return p;   
} 
 
 
        
        
/* g95_match_name()-- See if the current input looks like a name of
 * some sort.  Modifies the passed buffer which must be
 * G95_MAX_SYMBOL_LEN+1 bytes long. */      
      
match g95_match_name(char *buf) {   
locus oldl;       
int q, z;        
        
  oldl = *g95_current_locus();         
  g95_gobble_whitespace(); 
 
  z = g95_next_char();   
  if (!isalpha(z)) {        
    g95_set_locus(&oldl);      
    return MATCH_NO;        
  }          
          
  q = 0;         
         
  do {
    buf[q++] = z;

    if (q > G95_MAX_SYMBOL_LEN) {      
      g95_error("Name at %C is too long");          
      return MATCH_ERROR;       
    }      
      
    oldl = *g95_current_locus();
    z = g95_next_char();     
  } while(isalnum(z) || z == '_' || (g95_option.dollar && z == '$'));

  buf[q] = '\0';     
  g95_set_locus(&oldl);  
  
  return MATCH_YES;  
}    
    
    
        
        
/* g95_match_cycle()-- Match the CYCLE statement */    
    
match g95_match_cycle(void) {         
         
  return match_exit_cycle(ST_CYCLE, EXEC_CYCLE);   
}        
        
        
  
  
/* g95_match_block_data()-- Match a BLOCK DATA program unit */       
       
match g95_match_block_data(void) {          
char n[G95_MAX_SYMBOL_LEN+1];  
g95_symbol *symb;  
match y;     
     
  if (g95_match_eos() == MATCH_YES) {          
    g95_new_block = NULL;   
    return MATCH_YES;          
  }      
   
  y = g95_match(" %n%t", n);   
  if (y != MATCH_YES) return MATCH_ERROR;       
       
  if (g95_get_symbol(n, NULL, &symb)) return MATCH_ERROR;   
   
  if (g95_add_flavor(&symb->attr, FL_BLOCK_DATA, NULL) == FAILURE) 
    return MATCH_ERROR;   
   
  g95_new_block = symb;        
        
  return MATCH_YES;    
}


        
        
/* g95_match_symbol()-- Match a symbol on the input.  Modifies the
 * pointer to the symbol pointer if successful. */      
      
match g95_match_symbol(g95_symbol **matched_symbol, int host_assoc) {          
char buf[G95_MAX_SYMBOL_LEN+1]; 
match v;   
   
  v = g95_match_name(buf);       
  if (v != MATCH_YES) return v;          
          
  if (host_assoc)         
    return (g95_get_ha_symbol(buf, matched_symbol))          
      ? MATCH_ERROR : MATCH_YES;       
       
  if (g95_get_symbol(buf, NULL, matched_symbol)) return MATCH_ERROR;  
  
  return MATCH_YES;     
}   
   
   
          
          
/* g95_match_namelist()-- Match a NAMELIST statement */  
  
match g95_match_namelist(void) {          
g95_symbol *group_name, *symb;
g95_namelist *nl; 
match p, g;

  p = g95_match(" / %s /", &group_name);
  if (p == MATCH_NO) goto syntax;   
  if (p == MATCH_ERROR) goto error;    
    
  for(;;) { 
    if (group_name->ts.type != BT_UNKNOWN) {       
      g95_error("Namelist group name '%s' at %C already has a basic type "
		"of %s", group_name->name, g95_typename(&group_name->ts));
      return MATCH_ERROR;      
    }       
       
    if (group_name->attr.flavor != FL_NAMELIST &&     
	g95_add_flavor(&group_name->attr, FL_NAMELIST, NULL) == FAILURE)          
      return MATCH_ERROR;       
       
    for(;;) {
      p = g95_match_symbol(&symb, 1);      
      if (p == MATCH_NO) goto syntax;         
      if (p == MATCH_ERROR) goto error;   
   
      if (symb->attr.in_namelist == 0 &&       
	  g95_add_in_namelist(&symb->attr, NULL) == FAILURE) goto error;  
  
/* TODO: worry about PRIVATE members of a PUBLIC namelist group */      
      
      nl = g95_get_namelist();     
      nl->sym = symb;      
      
      if (group_name->namelist == NULL)  
	group_name->namelist = group_name->namelist_tail = nl;
      else {        
	group_name->namelist_tail->next = nl;     
	group_name->namelist_tail = nl;  
      }  
  
      if (g95_match_eos() == MATCH_YES) goto done;      
      
      p = g95_match_char(',');    
    
      if (g95_match_char('/') == MATCH_YES) {       
	g = g95_match(" %s /", &group_name);     
	if (g == MATCH_YES) break;        
	if (g == MATCH_ERROR) goto error;        
	goto syntax;          
      }  
  
      if (p != MATCH_YES) goto syntax;   
    }      
  }        
        
done:     
  return MATCH_YES;         
         
syntax:     
  g95_syntax_error(ST_NAMELIST);          
          
error:     
  return MATCH_ERROR;      
}         
         
         
 
 
/* g95_match()-- General purpose matching subroutine.  The target
 * string is a scanf-like format string in which spaces correspond to
 * arbitrary whitespace (including no whitespace), characters
 * correspond to themselves.  The %-codes are:
 *
 * %%  Literal percent sign
 * %e  Expression, pointer to a pointer is set
 * %s  Symbol, pointer to the symbol is set
 * %n  Name, character buffer is set to name
 * %t  Matches end of statement.
 * %o  Matches an intrinsic operator, returned as an INTRINSIC enum.
 * %l  Matches a statement label
 * %v  Matches a variable expression (an lvalue)
 * %   Matches a required space (in free form) and optional spaces.
 */         
         
match g95_match(char *target, ...) {    
g95_st_label **lab;    
int matches, *ifp;       
locus old_loc;      
va_list argps;      
char i, *np;     
match g, u;       
void **vp;    
char *j;       
       
  old_loc = *g95_current_locus();   
  va_start(argps, target);       
  g = MATCH_NO; 
  matches = 0;
  j = target;          
          
loop:          
  i = *j++;    
  switch(i) {   
  case ' ':   g95_gobble_whitespace(); goto loop;
  case '\0':  g = MATCH_YES; break;  
  
  case '%':     
    i = *j++;
    switch(i) {         
    case 'e':         
      vp = va_arg(argps, void **);   
      u = g95_match_expr((g95_expr **) vp);          
      if (u != MATCH_YES) { g = u; goto not_yes; }    
    
      matches++;          
      goto loop;    
    
    case 'v':         
      vp = va_arg(argps, void **);      
      u = g95_match_variable((g95_expr **) vp, 0);     
      if (u != MATCH_YES) { g = u; goto not_yes; }        
        
      matches++;   
      goto loop;        
        
    case 's':   
      vp = va_arg(argps, void **);  
      u = g95_match_symbol((g95_symbol **) vp, 0);     
      if (u != MATCH_YES) { g = u; goto not_yes; }          
          
      matches++;
      goto loop;          
          
    case 'n': 
      np = va_arg(argps, char *);       
      u = g95_match_name(np);      
      if (u != MATCH_YES) { g = u; goto not_yes; }   
   
      matches++;      
      goto loop;       
       
    case 'l':   
      lab = va_arg(argps, g95_st_label **); 
      u = g95_match_st_label(lab, 0);       
      if (u != MATCH_YES) { g = u; goto not_yes; }          
          
      matches++;    
      goto loop;

    case 'o':     
      ifp = va_arg(argps, int *);    
      u = g95_match_intrinsic_op((g95_intrinsic_op *) ifp);    
      if (u != MATCH_YES) { g = u; goto not_yes; }       
       
      matches++;
      goto loop;   
   
    case 't':
      if (g95_match_eos() != MATCH_YES) { g = MATCH_NO; goto not_yes; }   
      goto loop;       
       
    case ' ': 
      if (g95_match_space() == MATCH_YES) goto loop;          
      g = MATCH_NO;   
      goto not_yes;    
    
    case '%': break;  /* Fall through to character matcher */      
      
    default:  
      g95_internal_error("g95_match(): Bad match code %c", i);          
    }    
    
  default:      
    if (i == g95_next_char()) goto loop;    
    break;         
  } 
 
not_yes:        
  va_end(argps);    
    
  if (g != MATCH_YES) {   /* Clean up after a failed match */   
    g95_set_locus(&old_loc);     
    va_start(argps, target);      
      
    j = target;       
    for(; matches>0; matches--) {          
      while(*j++ != '%');       
       
      switch(*j++) {        
      case '%': matches++; break;   /* Skip */     
     
      case 'I': case 'L': case 'C': 
	if (*j++ == 'e') goto undo_expr; 
	break;     
     
      case 'o': case 'l':	/* Matches that don't have to be undone */       
      case 'n': case 's':   
	vp = va_arg(argps, void **);     
	break;      
      
      case 'e': case 'E': case 'v': 
      undo_expr:     
	vp = va_arg(argps, void **);          
	g95_free_expr(*vp);          
	*vp = NULL;          
	break;         
      } 
    }  

    va_end(argps);        
  } 
 
  return g;       
}         
         
         
        
        
/* g95_match_do()-- Match a DO statement */     
     
match g95_match_do(void) {  
g95_expr *while_condition;       
g95_iterator iter, *interp;
g95_st_label *l;     
locus o;    
match d;    
    
  o = *g95_current_locus();         
         
  l = NULL;
  iter.var = iter.start = iter.end = iter.step = NULL;   
  while_condition = NULL;      
      
  d = g95_match_label();   
  if (d == MATCH_ERROR) return d;       
       
  if (g95_match(" do") != MATCH_YES) return MATCH_NO;        
        
/* Match an infinite DO, make it like a DO WHILE(.TRUE.) */       
       
  if (g95_match_eos() == MATCH_YES) {        
    new_st.type = EXEC_DO_WHILE;     
    goto done;  
  }     
     
  d = g95_match_st_label(&l, 0);  
  if (d == MATCH_ERROR) goto cleanup;       
       
  g95_match_char(',');   
   
  if (g95_match("% ") != MATCH_YES) return MATCH_NO;          
          
/* See if we have a DO WHILE */ 
 
  if (g95_match(" while ( %e )%t", &while_condition) == MATCH_YES) {      
    new_st.type = EXEC_DO_WHILE;         
    goto done;
  }        
        
/* The abortive DO WHILE may have done something to the symbol table,
 * so we start over: */     
     
  g95_undo_symbols();         
  g95_set_locus(&o);  
  
  g95_match_label();    /* This won't error */         
  g95_match(" do ");    /* This will work */          
          
  g95_match_st_label(&l, 0);  /* Can't error out */        
  g95_match_char(',');            /* Optional comma */

  d = g95_match_iterator(&iter, 0);      
  if (d == MATCH_NO) return MATCH_NO;  
  if (d == MATCH_ERROR) goto cleanup;    
    
  g95_check_do_variable(iter.var->symbol);        
        
  if (g95_match_eos() != MATCH_YES) {  
    g95_syntax_error(ST_DO);
    goto cleanup;          
  }          
          
  new_st.type = EXEC_DO;          
          
done:         
  if (l != NULL &&        
      g95_reference_st_label(l, ST_LABEL_TARGET) == FAILURE)       
    goto cleanup;

  new_st.label = l;     
     
  if (new_st.type == EXEC_DO_WHILE)
    new_st.expr = while_condition;          
  else {         
    new_st.ext.iterator = interp = g95_get_iterator();        
    *interp = iter; 
  }       
       
  return MATCH_YES;

cleanup:    
  g95_free_iterator(&iter, 0);   
  return MATCH_ERROR;        
}      
      
      
   
   
/* g95_match_pause()-- Match the (deprecated) PAUSE statement */         
         
match g95_match_pause(void) {         
g95_expr *e2;          
          
  if (g95_match_eos() == MATCH_YES) goto got_match;        
        
  if (g95_match(" %e%t", &e2) == MATCH_YES) {   
    g95_free_expr(e2);       
    goto got_match; 
  } 
 
  return MATCH_NO;    
    
got_match:      
  g95_error("The PAUSE statement at %C is not allowed in Fortran 95"); 
  return MATCH_ERROR; 
}    
    
    
    
    
/* g95_match_assign()-- Match the (deprecated) ASSIGN statement. */   
   
match g95_match_assign(void) {      
g95_expr *e1;   
int lab;        
        
  if (g95_match(" %l to %v%t", &lab, &e1) == MATCH_YES) {        
    g95_free_expr(e1);         
    g95_error("The ASSIGN statement at %C is not allowed in Fortran 95");   
    return MATCH_ERROR;
  } 
 
  return MATCH_NO;  
}        
        
        
    
    
/* free_value()-- Free a g95_data_value structure and everything
 * beneath it */

static void free_value(g95_data_value *y) {       
g95_data_value *l;       
       
  for(; y; y=l) {    
    l = y->next;
    g95_free_expr(y->expr);    
    g95_free(y); 
  }         
}  
  
  
    
    
/* free_variable()-- Free a g95_data_variable structure and everything
 * beneath it */         
         
static void free_variable(g95_data_variable *c) {        
g95_data_variable *s;          
          
  for(; c; c=s) {
    s = c->next;   
    g95_free_expr(c->expr);  
    g95_free_iterator(&c->iter, 0);          
    free_variable(c->list);   
       
    g95_free(c);   
  }  
}     
     
     
      
      
/* top_val_list()-- Match a list of values in a DATA statement.  The
 * leading '/' has already been seen at this point. */  
  
static match top_val_list(g95_data *data) {    
g95_data_value *n, *end;         
g95_expr *expr;  
char *message;  
match a;

  end = NULL;         
         
  for(;;) {     
    a = match_data_constant(&expr);       
    if (a == MATCH_NO) goto syntax;   
    if (a == MATCH_ERROR) return MATCH_ERROR;   
   
    n = g95_get_data_value();      
      
    if (end == NULL)      
      data->value = n;     
    else
      end->next = n; 
 
    end = n; 
 
    if (expr->ts.type != BT_INTEGER || g95_match_char('*') != MATCH_YES) {      
      end->expr = expr;        
      end->repeat = 1;      
    } else {       
      message = g95_extract_int(expr, &end->repeat);     
      g95_free_expr(expr);         
      if (message != NULL) {    
	g95_error(message);       
	return MATCH_ERROR;       
      }          
          
      a = match_data_constant(&end->expr);         
      if (a == MATCH_NO) goto syntax;  
      if (a == MATCH_ERROR) return MATCH_ERROR;   
    }    
    
    if (g95_option.verbose) {     
      g95_status("DATA element:  %d * ", end->repeat);       
      g95_show_expr(end->expr);     
      g95_status("\n"); 
    }         
         
    if (g95_match_char('/') == MATCH_YES) break;    
    if (g95_match_char(',') == MATCH_NO) goto syntax;      
  }         
         
  return MATCH_YES;       
       
syntax:        
  g95_syntax_error(ST_DATA);       
  return MATCH_ERROR;
}


      
      
/* g95_free_equiv()-- Free equivalence sets and lists.  Recursively is
 * the easiest way to do this. */     
     
void g95_free_equiv(g95_equiv *eq2) {       
       
  if (eq2 == NULL) return; 
 
  g95_free_equiv(eq2->eq); 
  g95_free_equiv(eq2->next);          
          
  g95_free_expr(eq2->expr);       
  g95_free(eq2);      
}        
        
        
         
         
/* g95_free_data()-- Free a list of g95_data structures */         
         
void g95_free_data(g95_data *f) {          
g95_data *k; 
 
  for(; f; f=k) {  
    k = f->next;      
      
    free_variable(f->var);        
    free_value(f->value);      
      
    g95_free(f);     
  } 
}


         
         
/* g95_match_small_literal_int()-- Match a literal integer on the
 * input, setting the value on MATCH_YES.  Literal integers occur in
 * kind-parameters as well as old-style character length
 * specifications. */       
       
match g95_match_small_literal_int(int *value) {  
locus old;       
char s;
int z;        
        
  old = *g95_current_locus();

  g95_gobble_whitespace();          
  s = g95_next_char();     
     
  if (!isdigit(s)) {
    g95_set_locus(&old);    
    return MATCH_NO;          
  }     
     
  z = s - '0';      
      
  for(;;) {
    old = *g95_current_locus();        
    s = g95_next_char();      
      
    if (!isdigit(s)) break;        
        
    z = 10*z + s - '0'; 
 
    if (z > 99999999) { 
      g95_error("Integer too large at %C");      
      return MATCH_ERROR;        
    }         
  }

  g95_set_locus(&old); 
 
  *value = z;          
  return MATCH_YES;      
}      
      
      
        
        
/* match_forall_iterator()-- Match an iterator as part of a FORALL
 * statement.  The format is:
 *     <var> = <start>:<end>[:<stride>][, <scalar mask>]  */          
          
static match match_forall_iterator(g95_forall_iterator **rslt) {  
g95_forall_iterator *iter;  
locus old_loc;   
match v;

  old_loc = *g95_current_locus();     
  iter = g95_getmem(sizeof(g95_forall_iterator));

  v = g95_match_variable(&iter->var, 0);   
  if (v != MATCH_YES) goto cleanup;     
     
  if (g95_match_char('=') != MATCH_YES) {  
    v = MATCH_NO;       
    goto cleanup;       
  }  
  
  v = g95_match_expr(&iter->start);     
  if (v == MATCH_NO) goto syntax;        
  if (v == MATCH_ERROR) goto cleanup;     
     
  if (g95_match_char(':') != MATCH_YES) goto syntax;         
         
  v = g95_match_expr(&iter->end);     
  if (v == MATCH_NO) goto syntax;
  if (v == MATCH_ERROR) goto cleanup;      
      
  if (g95_match_char(':') == MATCH_NO)
    iter->stride = g95_int_expr(1);    
  else {   
    v = g95_match_expr(&iter->stride);  
    if (v == MATCH_NO) goto syntax;     
    if (v == MATCH_ERROR) goto cleanup;
  }      
      
  *rslt = iter; 
  return MATCH_YES;

syntax:   
  g95_error("Syntax error in FORALL iterator at %C");          
  v = MATCH_ERROR; 
 
cleanup:    
  g95_set_locus(&old_loc);      
  g95_free_forall_iterator(iter);     
  return v; 
}      
      
      
       
       
/* var_list()-- Match a list of variables terminated by an iterator
 * and a right paren. */  
  
static match var_list(g95_data_variable *parent) {         
g95_data_variable *t, var;          
match b;    
    
  b = var_element(&var);      
  if (b == MATCH_ERROR) return MATCH_ERROR;          
  if (b == MATCH_NO) goto syntax;       
       
  t = g95_get_data_variable(); 
  *t = var;  
  
  parent->list = t;        
        
  for(;;) {  
    if (g95_match_char(',') != MATCH_YES) goto syntax;  
  
    b = g95_match_iterator(&parent->iter, 0);
    if (b == MATCH_YES) break;        
    if (b == MATCH_ERROR) return MATCH_ERROR;          
          
    b = var_element(&var);     
    if (b == MATCH_ERROR) return MATCH_ERROR; 
    if (b == MATCH_NO) goto syntax;  
  
    t->next = g95_get_data_variable(); 
    t = t->next;

    *t = var;         
  }       
       
  if (g95_match_char(')') != MATCH_YES) goto syntax;
  return MATCH_YES;         
         
syntax:      
  g95_syntax_error(ST_DATA);   
  return MATCH_ERROR;         
} 
 
 
          
          
/* var_element()-- Match a single element in a data variable list,
 * which can be a variable-iterator list. */  
  
static match var_element(g95_data_variable *old) {       
match x;         
         
  memset(old, '\0', sizeof(g95_data_variable));     
     
  if (g95_match_char('(') == MATCH_YES) return var_list(old);    
    
  x = g95_match_variable(&old->expr, 0);  
  if (x != MATCH_YES) return x;      
      
  if (old->expr->symbol->value != NULL) {      
    g95_error("Variable '%s' at %C already has an initialization",       
	      old->expr->symbol->name);          
    return MATCH_ERROR;
  }   
   
  old->expr->symbol->attr.data = 1;      
  return MATCH_YES;    
}       
       
       
     
     
/* g95_match_common()-- Match a COMMON statement */     
     
match g95_match_common(void) {   
g95_symbol *symb, **head, *end, *old_blank_common;        
char nm[G95_MAX_SYMBOL_LEN+1];      
g95_common_head *z;
g95_array_spec *a;        
match x;

  old_blank_common = g95_current_ns->blank_common;    
  if (old_blank_common != NULL) {   
    while(old_blank_common->common_next != NULL)
      old_blank_common = old_blank_common->common_next;   
  }         
         
  a = NULL; 
 
  if (g95_match_eos() == MATCH_YES) goto syntax; 
 
  for(;;) {    
    x = match_common_name(nm);   
    if (x == MATCH_ERROR) goto cleanup;        
        
    if (nm[0] == '\0')        
      head = &g95_current_ns->blank_common; 
    else {        
      z = g95_get_common(nm);  
      head = &z->head;      
      
      if (z->use_assoc) { 
	g95_error("COMMON block '%s' at %C has already been USE-associated");       
	goto cleanup;   
      }   
    }      
      
    if (*head == NULL)  
      end = NULL;     
    else {     
      end = *head;      
      while(end->common_next)       
	end = end->common_next;   
    }      
      
/* Grab the list of symbols for this common. */

    for(;;) {       
      x = g95_match_symbol(&symb, 0);         
      if (x == MATCH_ERROR) goto cleanup;     
      if (x == MATCH_NO) goto syntax;        
        
      if (symb->attr.in_common) {  
	g95_error("Symbol '%s' at %C is already in a COMMON block", symb->name);  
	goto cleanup; 
      }         
         
      if (g95_add_in_common(&symb->attr, NULL) == FAILURE) goto cleanup;

      if (end != NULL)       
	end->common_next = symb;  
      else       
	*head = symb;          
          
      end = symb;    
    
/* Deal with an optional array specification after the symbol name */     
     
      x = g95_match_array_spec(&a);          
      if (x == MATCH_ERROR) goto cleanup;     
      if (x == MATCH_YES) {      
	if (g95_add_dimension(&symb->attr, NULL) == FAILURE) goto cleanup;     
     
	symb->as = a;   
	a = NULL;    
      }    
    
      if (g95_match_eos() == MATCH_YES) goto done;       
      if (g95_peek_char() == '/') break;       
      if (g95_match_char(',') != MATCH_YES) goto syntax;      
      if (g95_peek_char() == '/') break;
    }         
  } 
 
done:  
  return MATCH_YES;         
         
syntax:   
  g95_syntax_error(ST_COMMON);

cleanup:     
  if (old_blank_common != NULL)
    old_blank_common->common_next = NULL;       
  else          
    g95_current_ns->blank_common = NULL;   
   
  g95_free_array_spec(a);   
  return MATCH_ERROR;     
}          
          
          
      
      
/* g95_free_forall_iterator()-- Free a list of FORALL iterators */ 
 
void g95_free_forall_iterator(g95_forall_iterator *it) {
g95_forall_iterator *next;      
      
  while(it) {        
    next = it->next;         
         
    g95_free_expr(it->var);  
    g95_free_expr(it->start);    
    g95_free_expr(it->end);        
    g95_free_expr(it->stride);    
    
    g95_free(it);   
    it = next;         
  }
}         
         
         
         
         
/* g95_match_forall()-- Match a FORALL statement */   
   
match g95_match_forall(g95_statement *st) {       
g95_forall_iterator *h, *end, *n;   
g95_expr *mask;   
g95_code *v;         
match o, j;        
        
  h = end = NULL;     
  mask = NULL;          
  v = NULL;       
       
  o = g95_match_label();        
  if (o == MATCH_ERROR) return MATCH_ERROR;     
     
  j = g95_match(" forall (");       
  if (j != MATCH_YES) return j;

  j = match_forall_iterator(&n);         
  if (j == MATCH_ERROR) goto cleanup;   
  if (j == MATCH_NO) goto syntax;  
  
  h = end = n; 
 
  for(;;) {   
    if (g95_match_char(',') != MATCH_YES) break; 
 
    j = match_forall_iterator(&n);     
    if (j == MATCH_ERROR) goto cleanup;   
    if (j == MATCH_YES) {     
      end->next = n;  
      end = n;         
      continue;          
    }

/* Have to have a mask expression */    
    
    j = g95_match_expr(&mask);   
    if (j == MATCH_NO) goto syntax;  
    if (j == MATCH_ERROR) goto cleanup;     
     
    break;
  }        
        
  if (g95_match_char(')') == MATCH_NO) goto syntax;         
         
  if (g95_match_eos() == MATCH_YES) {  
    *st = ST_FORALL_BLOCK;   
   
    new_st.type = EXEC_FORALL;        
    new_st.expr = mask; 
    new_st.ext.forall_iterator = h;  
  
    return MATCH_YES; 
  }        
        
  j = g95_match_assignment();      
  if (j == MATCH_ERROR) goto cleanup;      
  if (j == MATCH_NO) {    
    j = g95_match_pointer_assignment();  
    if (j == MATCH_ERROR) goto cleanup; 
    if (j == MATCH_NO) goto syntax;     
  }          
          
  v = g95_get_code();   
  *v = new_st;    
    
  if (g95_match_eos() != MATCH_YES) goto syntax;     
     
  g95_clear_new_st();   
  new_st.type = EXEC_FORALL;          
  new_st.expr = mask; 
  new_st.ext.forall_iterator = h;
  new_st.block = g95_get_code();       
       
  new_st.block->type = EXEC_FORALL;
  new_st.block->next = v;  
  
  *st = ST_FORALL;    
  return MATCH_YES;  
  
syntax:         
  g95_syntax_error(ST_FORALL);       
       
cleanup:   
  g95_free_forall_iterator(h);          
  g95_free_expr(mask);          
  g95_free_statements(v); 
  return MATCH_NO;         
}       
  
  
/* top_var_list()-- Match the top-level list of data variables */

static match top_var_list(g95_data *r) {      
g95_data_variable var, *end, *n1;         
match i;   
   
  end = NULL; 
 
  for(;;) {   
    i = var_element(&var);  
    if (i == MATCH_NO) goto syntax;        
    if (i == MATCH_ERROR) return MATCH_ERROR;

    n1 = g95_get_data_variable();     
    *n1 = var;  
  
    if (end == NULL)   
      r->var = n1;    
    else         
      end->next = n1;       
       
    end = n1;

    if (g95_match_char('/') == MATCH_YES) break;   
    if (g95_match_char(',') != MATCH_YES) goto syntax;        
  }        
        
  return MATCH_YES;   
   
syntax:          
  g95_syntax_error(ST_DATA);       
  return MATCH_ERROR;      
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
        
int g95_match_strings(mstring *l) {        
int no_match, v, possibles;
mstring *y, *best_match;       
locus match_loc;  
  
  possibles = 0;      
      
  for(y=l; y->string != NULL; y++) {   
    y->mp = y->string; 
    possibles++;     
  }   
   
  no_match = y->tag;    
    
  best_match = NULL;       
  match_loc = *g95_current_locus();      
      
  g95_gobble_whitespace();    
    
  while(possibles > 0) {          
    v = g95_next_char();       
       
/* Apply the next character to the current possibilities */     
     
    for(y=l; y->string!=NULL; y++) {      
      if (y->mp == NULL) continue;

      if (*y->mp == ' ') {    /* Space matches 1+ whitespace(s) */          
          
	if ((g95_current_file->form == FORM_FREE) &&         
	      g95_is_whitespace(v)) continue;         
         
	y->mp++;          
      }   
   
      if (*y->mp != v) {      /* Match failed */        
	y->mp = NULL;
	possibles--;    
	continue;  
      }         
         
      y->mp++;
      if (*y->mp == '\0') {   /* Found a match */       
	match_loc = *g95_current_locus();          
	best_match = y;
	possibles--;       
	y->mp = NULL;       
      }     
    }        
  }  
  
  g95_set_locus(&match_loc);         
         
  return (best_match == NULL) ? no_match : best_match->tag; 
}        
        
        
 
 
/* g95_match_data()-- Match a DATA statement */  
  
match g95_match_data(void) {     
g95_data *new;          
match q;          
            
  for(;;) { 
    new = g95_get_data(); 
    new->where = *g95_current_locus();         
         
    q = top_var_list(new);     
    if (q != MATCH_YES) goto cleanup;    
    
    q = top_val_list(new);     
    if (q != MATCH_YES) goto cleanup;      
      
    new->next = g95_current_ns->data;  
    g95_current_ns->data = new;         
         
    if (g95_match_eos() == MATCH_YES) break;

    g95_match_char(',');  /* Optional comma */         
  }    
    
  if (g95_pure(NULL)) {
    g95_error("DATA statement at %C is not allowed in a PURE procedure"); 
    return MATCH_ERROR;   
  } 
 
  return MATCH_YES;     
     
cleanup: 
  g95_free_data(new);  
  return MATCH_ERROR;          
}         
         
         
