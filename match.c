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
     
  
  
/* g95_match_deallocate()-- Match a DEALLOCATE statement */        
        
match g95_match_deallocate(void) {         
g95_alloc *start, *tail;       
g95_expr *stat;          
match n;        
        
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
    
    n = g95_match_variable(&tail->expr, 0);   
    if (n == MATCH_ERROR) goto cleanup;      
    if (n == MATCH_NO) goto syntax; 
 
    if (g95_pure(NULL) && g95_impure_variable(tail->expr->symbol)) { 
      g95_error("Illegal deallocate-expression in DEALLOCATE at %C for a PURE "          
		"procedure");       
      goto cleanup; 
    }       
       
    if (g95_match_char(',') != MATCH_YES) break;    
    
    n = g95_match(" stat = %v", &stat);  
    if (n == MATCH_ERROR) goto cleanup;
    if (n == MATCH_YES) break;          
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
   
   
    
    
/* free_variable()-- Free a g95_data_variable structure and everything
 * beneath it */       
       
static void free_variable(g95_data_variable *e) {        
g95_data_variable *f;        
        
  for(; e; e=f) {   
    f = e->next;          
    g95_free_expr(e->expr);     
    g95_free_iterator(&e->iter, 0);  
    free_variable(e->list);   
       
    g95_free(e);  
  }          
}        
        
        
      
      
/* g95_match_assignment(void)-- Match a simple assignment statement */         
         
match g95_match_assignment(void) {   
g95_expr *lvalue, *rvalue;     
g95_locus old_loc; 
match d;     
     
  old_loc = g95_current_locus;          
         
  lvalue = rvalue = NULL;       
  d = g95_match(" %v =", &lvalue);          
  if (d != MATCH_YES) goto cleanup;        
        
  if (lvalue->symbol->attr.flavor == FL_PARAMETER) {         
    g95_error("Cannot assign to a PARAMETER variable at %C");     
    d = MATCH_ERROR; 
    goto cleanup;    
  } 
 
  d = g95_match(" %e%t", &rvalue);       
  if (d != MATCH_YES) goto cleanup;   
   
  new_st.type = EXEC_ASSIGN;       
  new_st.expr = lvalue;       
  new_st.expr2 = rvalue;       
       
  g95_check_do_variable(lvalue->symbol);

  return MATCH_YES;   
   
cleanup:        
  g95_current_locus = old_loc;        
  g95_free_expr(lvalue);    
  g95_free_expr(rvalue);         
  return d;         
} 
 
 
    
    
/* g95_match_intrinsic_op()-- Match an intrinsic operator.  Returns an
 * INTRINSIC enum. */         
         
match g95_match_intrinsic_op(g95_intrinsic_op *res) {    
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
      
  *res = op1;    
  return MATCH_YES; 
}       
       
       
char *g95_op2string(int d) {          
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
    
  if (d == INTRINSIC_ASSIGN) return "=";    
  return g95_code2string(operators_out, d);  
}       
       
       
       
       
/* g95_match_iterator()-- Match a loop control phrase:
 *
 *     <LVALUE> = <EXPR>, <EXPR> [, <EXPR> ]
 *
 * If the final integer expression is not present, a constant unity
 * expression is returned.  We don't return MATCH_ERROR until after
 * the equals sign is seen. */   
   
match g95_match_iterator(g95_iterator *iterator, int init_flag) {          
char name0[G95_MAX_SYMBOL_LEN+1];     
g95_expr *var, *d, *f, *e3;
g95_locus start;      
match r;     
     
  /* Match the start of an iterator without affecting the symbol table */ 
 
  start = g95_current_locus;     
  r = g95_match(" %n =", name0);      
  g95_current_locus = start;      
      
  if (r != MATCH_YES) return MATCH_NO;

  r = g95_match_variable(&var, 0);  
  if (r != MATCH_YES) return MATCH_NO;    
    
  g95_match_char('='); 
 
  d = f = e3 = NULL;      
      
  if (var->ref != NULL) {    
    g95_error("Loop variable at %C cannot be a sub-component");   
    goto cleanup;         
  }    
    
  if (var->symbol->attr.intent == INTENT_IN) {     
    g95_error("Loop variable '%s' at %C cannot be INTENT(IN)",      
	      var->symbol->name);        
    goto cleanup;   
  } 
 
  r = init_flag ? g95_match_init_expr(&d) : g95_match_expr(&d);   
  if (r == MATCH_NO) goto syntax;
  if (r == MATCH_ERROR) goto cleanup;  
  
  if (g95_match_char(',') != MATCH_YES) goto syntax;      
      
  r = init_flag ? g95_match_init_expr(&f) : g95_match_expr(&f);  
  if (r == MATCH_NO) goto syntax;      
  if (r == MATCH_ERROR) goto cleanup;         
         
  if (g95_match_char(',') != MATCH_YES) {        
    e3 = g95_int_expr(1);    
    goto done;   
  }  
  
  r = init_flag ? g95_match_init_expr(&e3) : g95_match_expr(&e3);     
  if (r == MATCH_ERROR) goto cleanup; 
  if (r == MATCH_NO) {     
    g95_error("Expected a step value in iterator at %C");         
    goto cleanup;       
  }          
          
done:         
  iterator->var = var;      
  iterator->start = d;  
  iterator->end = f;      
  iterator->step = e3;          
  return MATCH_YES;     
     
syntax:      
  g95_error("Syntax error in iterator at %C"); 
    
cleanup:     
  g95_free_expr(d);        
  g95_free_expr(f);  
  g95_free_expr(e3);

  return MATCH_ERROR;
}


       
       
/* g95_match_st_label()-- Matches a statement label.  Uses
 * g95_match_small_literal_int() to do most of the work. */       
       
match g95_match_st_label(g95_st_label **l, int allow_zero) {
g95_locus o;
int label_value;        
match p;   
   
  o = g95_current_locus;          
          
  p = g95_match_small_literal_int(&label_value);       
  if (p != MATCH_YES) return p;    
    
  if ((label_value == 0 && allow_zero) || label_value <= 99999) {  
    *l = g95_get_st_label(label_value);    
    return MATCH_YES;     
  } 
 
  g95_error("Statement label at %C is out of range");
  g95_current_locus = o;
  return MATCH_ERROR;         
} 
 
 
    
    
/* g95_match_small_int()-- Match a small, constant integer expression,
 * like in a kind statement.  On MATCH_YES, 'value' is set. */  
  
match g95_match_small_int(int *value) {  
g95_expr *expr;      
char *x;
match y;  
int a;     
     
  y = g95_match_expr(&expr);    
  if (y != MATCH_YES) return y;

  x = g95_extract_int(expr, &a);          
  g95_free_expr(expr);  
  
  if (x != NULL) {   
    g95_error(x);   
    y = MATCH_ERROR; 
  }   
   
  *value = a;          
  return y;    
}     
     
     
      
      
/* g95_match_stop()-- Match the STOP statement.  We can't match a
 * label here because labels can't be zero and a stop code can. */

match g95_match_stop(void) { 
int stop_code;      
g95_expr *n;      
match c; 
 
  stop_code = -1;   /* blank stop */   
  n = NULL;          
          
  if (g95_match_eos() != MATCH_YES) {     
    c = g95_match_small_literal_int(&stop_code);    
    if (c == MATCH_ERROR) goto cleanup;   
   
    if (c == MATCH_YES && stop_code > 99999) {     
      g95_error("STOP code out of range at %C");      
      goto cleanup; 
    }   
   
    if (c == MATCH_NO) {  /* Try a character constant */          
      c = g95_match_expr(&n);  
      if (c == MATCH_ERROR) goto cleanup;      
      if (c == MATCH_NO) goto syntax;   
   
      if (n->ts.type != BT_CHARACTER || n->type != EXPR_CONSTANT)    
	goto syntax;
    }       
       
    if (g95_match_eos() != MATCH_YES) goto syntax;
  }    
    
  if (g95_pure(NULL)) {     
    g95_error("STOP statement not allowed in PURE procedure at %C");       
    goto cleanup;  
  }  
  
  new_st.type = EXEC_STOP;
  new_st.expr = n;     
  new_st.ext.stop_code = stop_code;   
   
  return MATCH_YES;     
     
syntax:          
  g95_syntax_error(ST_STOP);          
          
cleanup:       
  g95_free_expr(n);   
  return MATCH_ERROR;   
}        
        
        
 
 
/* g95_match_small_literal_int()-- Match a literal integer on the
 * input, setting the value on MATCH_YES.  Literal integers occur in
 * kind-parameters as well as old-style character length
 * specifications. */       
       
match g95_match_small_literal_int(int *value) {        
g95_locus o;       
char g;          
int x;  
  
  o = g95_current_locus; 
 
  g95_gobble_whitespace();       
  g = g95_next_char();    
    
  if (!isdigit(g)) { 
    g95_current_locus = o;  
    return MATCH_NO;          
  }   
   
  x = g - '0';     
     
  for(;;) {     
    o = g95_current_locus;  
    g = g95_next_char();      
      
    if (!isdigit(g)) break;          
          
    x = 10*x + g - '0'; 
 
    if (x > 99999999) {    
      g95_error("Integer too large at %C");         
      return MATCH_ERROR;    
    }
  }  
  
  g95_current_locus = o;     
     
  *value = x;        
  return MATCH_YES;          
}


    
    
/* g95_match_program()-- Matches the start of a program unit, which is
 * the program keyword followed by an optional symbol. */     
     
match g95_match_program(void) {    
g95_symbol *sym;   
match p;         
         
  p = g95_match_eos();       
  if (p == MATCH_YES) return p;

  p = g95_match("% %s%t", &sym);  
  
  if (p == MATCH_NO) { 
    g95_error("Invalid form of PROGRAM statement at %C");     
    p = MATCH_ERROR;
  }

  if (p == MATCH_ERROR) return p;          
          
  if (g95_add_flavor(&sym->attr, FL_PROGRAM, NULL) == FAILURE) 
    return MATCH_ERROR;   
   
  g95_new_block = sym;     
     
  return MATCH_YES;         
}   
   
   
    
    
/* g95_free_iterator()-- Free a g95_iterator structure */       
       
void g95_free_iterator(g95_iterator *i, int flag) {        
        
  if (i == NULL) return;          
          
  g95_free_expr(i->var);          
  g95_free_expr(i->start);         
  g95_free_expr(i->end);   
  g95_free_expr(i->step);  
  
  if (flag) g95_free(i);  
}    
    
    
  
  
/* g95_match_char()-- Tries to match the next non-whitespace character
 * on the input.  This subroutine does not return MATCH_ERROR.  */        
        
match g95_match_char(char r) {       
g95_locus where;        
        
  where = g95_current_locus; 
  g95_gobble_whitespace(); 
 
  if (g95_next_char() == r) return MATCH_YES;  
  
  g95_current_locus = where;  
  return MATCH_NO;          
}          
          
          
  
  
/* g95_match_symbol()-- Match a symbol on the input.  Modifies the
 * pointer to the symbol pointer if successful. */     
     
match g95_match_symbol(g95_symbol **matched_symbol, int host_assoc) {
char buf[G95_MAX_SYMBOL_LEN+1];         
match s;

  s = g95_match_name(buf);
  if (s != MATCH_YES) return s;  
  
  if (host_assoc)        
    return (g95_get_ha_symbol(buf, matched_symbol))      
      ? MATCH_ERROR : MATCH_YES;     
     
  if (g95_get_symbol(buf, NULL, matched_symbol)) return MATCH_ERROR;    
    
  return MATCH_YES;
}         
         
         
     
     
/* g95_free_alloc_list()-- Frees a list of g95_alloc structures */        
        
void g95_free_alloc_list(g95_alloc *i) {   
g95_alloc *v;   
   
  for(; i; i=v) {     
    v = i->next;     
    g95_free_expr(i->expr);     
    g95_free(i);    
  }          
}    
    
    
   
   
/* g95_match_block_data()-- Match a BLOCK DATA program unit */   
   
match g95_match_block_data(void) { 
char nm[G95_MAX_SYMBOL_LEN+1];
g95_symbol *sym;         
match g;     
     
  if (g95_match_eos() == MATCH_YES) { 
    g95_new_block = NULL;  
    return MATCH_YES;    
  }       
    
  g = g95_match(" %n%t", nm);     
  if (g != MATCH_YES) return MATCH_ERROR;       
       
  if (g95_get_symbol(nm, NULL, &sym)) return MATCH_ERROR;   
   
  if (g95_add_flavor(&sym->attr, FL_BLOCK_DATA, NULL) == FAILURE)  
    return MATCH_ERROR; 
 
  g95_new_block = sym; 
 
  return MATCH_YES;     
}         
         
         
        
        
/* match_arithmetic_if()-- Match an arithmetic IF statement that
 * happens to follow a simple IF statement. */ 
 
static match match_arithmetic_if(void) {     
g95_st_label *x, *b, *l3;      
g95_expr *y;        
match r;        
        
  r = g95_match(" ( %e ) %l , %l , %l %t", &y, &x, &b, &l3);      
  if (r != MATCH_YES) return r;    
    
  if (g95_reference_st_label(x, ST_LABEL_TARGET) == FAILURE ||        
      g95_reference_st_label(b, ST_LABEL_TARGET) == FAILURE ||     
      g95_reference_st_label(l3, ST_LABEL_TARGET) == FAILURE) {        
    g95_free_expr(y);          
    return MATCH_ERROR;         
  }

  new_st.type = EXEC_ARITHMETIC_IF;  
  new_st.expr = y; 
  new_st.label  = x;         
  new_st.label2 = b;
  new_st.label3 = l3;     
     
  return MATCH_YES;    
}   
   
   
/*@match_simple_where()*/   
   
/* match_simple_where()-- Match the rest of a simple WHERE statement
 * that follows an IF statement. */        
        
static match match_simple_where(void) {  
g95_expr *exp;      
g95_code *f;    
match r;    
    
  r = g95_match("( %e )", &exp);    
  if (r != MATCH_YES) return r;       
       
  r = g95_match_assignment();         
  if (r == MATCH_NO) goto syntax;        
  if (r == MATCH_ERROR) goto cleanup; 
 
  if (g95_match_eos() != MATCH_YES) goto syntax;

  f = g95_get_code();     
     
  f->type = EXEC_WHERE;    
  f->expr = exp;  
  f->next = g95_get_code();     
     
  *f->next = new_st;        
  g95_clear_new_st();      
      
  new_st.type = EXEC_WHERE;     
  new_st.block = f;   
   
  return MATCH_YES;      
      
syntax:          
  g95_syntax_error(ST_WHERE);          
          
cleanup:        
  g95_free_expr(exp);   
  return MATCH_ERROR;  
}       
       
       
    
    
/* g95_match_goto()-- Match the GO TO statement.  As a computed GOTO
 * statement is matched, it is transformed into an equivalent SELECT
 * block.  No tree is necessary, and the resulting jumps-to-jumps are
 * specifically optimized away by the back end. */        
        
match g95_match_goto(void) {         
g95_code *head, *end;   
g95_st_label *lab;  
g95_expr *e2;       
g95_case *cp;   
match l;       
int e;     
     
  if (g95_match(" %l%t", &lab) == MATCH_YES) {       
    if (g95_reference_st_label(lab, ST_LABEL_TARGET) == FAILURE)   
      return MATCH_ERROR;     
     
    new_st.type = EXEC_GOTO;          
    new_st.label = lab;       
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
    
  head = end = NULL;         
  e = 1;      
      
  do {         
    l = g95_match_st_label(&lab, 0);   
    if (l != MATCH_YES) goto syntax;          
          
    if (g95_reference_st_label(lab, ST_LABEL_TARGET) == FAILURE)          
      goto cleanup;         
         
    if (head == NULL)      
      head = end = g95_get_code();          
    else {       
      end->block = g95_get_code();         
      end = end->block;   
    }          
          
    cp = g95_get_case();      
    cp->low = cp->high = g95_int_expr(e++);

    end->type = EXEC_SELECT;     
    end->ext.case_list = cp;        
        
    end->next = g95_get_code();       
    end->next->type = EXEC_GOTO;
    end->next->label = lab;    
  } while(g95_match_char(',') == MATCH_YES);     
     
  if (g95_match_char(')') != MATCH_YES) goto syntax;  
  
  if (head == NULL) {
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
  new_st.block = head;  
  return MATCH_YES; 
 
syntax:          
  g95_syntax_error(ST_GOTO);
cleanup:       
  g95_free_statements(head);       
  return MATCH_ERROR;
}        
        
        
        
        
/* g95_match_nullify()-- Match a NULLIFY statement. A NULLIFY
 * statement is transformed into a set of pointer assignments to
 * intrinsic NULL(). */

match g95_match_nullify(void) {        
g95_code *end;    
g95_expr *t, *l;         
match i;   
   
  end = NULL;          
          
  if (g95_match_char('(') != MATCH_YES) goto syntax;        
        
  for(;;) {
    i = g95_match_variable(&l, 0);        
    if (i == MATCH_ERROR) goto cleanup;      
    if (i == MATCH_NO) goto syntax;     
     
    if (g95_pure(NULL) && g95_impure_variable(l->symbol)) {         
      g95_error("Illegal variable in NULLIFY at %C for a PURE procedure");        
      goto cleanup;   
    }        
        
    /* build ' => NULL() ' */      
    t = g95_null_expr(NULL);  
  
    /* Chain to list */       
    if (end == NULL)     
      end = &new_st;   
    else {
      end->next = g95_get_code();  
      end = end->next;        
    }       
       
    end->type = EXEC_POINTER_ASSIGN;         
    end->expr = l;    
    end->expr2 = t;       
       
    if (g95_match_char(')') == MATCH_YES) break;    
    if (g95_match_char(',') != MATCH_YES) goto syntax;        
  }        
        
  return MATCH_YES;       
       
syntax:  
  g95_syntax_error(ST_NULLIFY);       
       
cleanup:      
  g95_free_statements(end);        
  return MATCH_ERROR;      
}     
     
     
         
         
/* g95_match_pointer_assignment()-- Match a pointer assignment statement */       
       
match g95_match_pointer_assignment(void) {   
g95_expr *lvalue, *rvalue;
g95_locus oldl;
match u;       
       
  oldl = g95_current_locus;

  lvalue = rvalue = NULL;    
    
  u = g95_match(" %v =>", &lvalue);      
  if (u != MATCH_YES) { u = MATCH_NO; goto cleanup; }     
     
  if (lvalue->symbol->attr.flavor == FL_PARAMETER) {         
    g95_error("Cannot assign to a PARAMETER variable at %C");     
    u = MATCH_ERROR;  
    goto cleanup;     
  }          
          
  u = g95_match(" %e%t", &rvalue);     
  if (u != MATCH_YES) goto cleanup;    
    
  new_st.type = EXEC_POINTER_ASSIGN; 
  new_st.expr = lvalue;
  new_st.expr2 = rvalue;   
   
  return MATCH_YES;        
        
cleanup:       
  g95_current_locus = oldl;     
  g95_free_expr(lvalue);      
  g95_free_expr(rvalue);          
  return u;      
}


 
 
/* g95_match_name()-- See if the current input looks like a name of
 * some sort.  Modifies the passed buffer which must be
 * G95_MAX_SYMBOL_LEN+1 bytes long. */ 
 
match g95_match_name(char *buf) {          
g95_locus oldl;   
int p, n;       
       
  oldl = g95_current_locus;          
  g95_gobble_whitespace();     
     
  n = g95_next_char(); 
  if (!isalpha(n)) {    
    g95_current_locus = oldl;       
    return MATCH_NO;   
  } 
 
  p = 0; 
 
  do {  
    buf[p++] = n;   
   
    if (p > G95_MAX_SYMBOL_LEN) {        
      g95_error("Name at %C is too long");       
      return MATCH_ERROR;       
    }        
        
    oldl = g95_current_locus;
    n = g95_next_char();  
  } while(isalnum(n) || n == '_' || (g95_option.dollar && n == '$'));       
       
  buf[p] = '\0';         
  g95_current_locus = oldl;        
        
  return MATCH_YES;   
}   
   
   
  
  
/* var_list()-- Match a list of variables terminated by an iterator
 * and a right paren. */          
          
static match var_list(g95_data_variable *parent) {   
g95_data_variable *tail, var0;  
match r;  
  
  r = var_element(&var0);        
  if (r == MATCH_ERROR) return MATCH_ERROR;
  if (r == MATCH_NO) goto syntax;  
  
  tail = g95_get_data_variable();
  *tail = var0;   
   
  parent->list = tail;          
          
  for(;;) {         
    if (g95_match_char(',') != MATCH_YES) goto syntax;

    r = g95_match_iterator(&parent->iter, 0);     
    if (r == MATCH_YES) break;       
    if (r == MATCH_ERROR) return MATCH_ERROR;   
   
    r = var_element(&var0);          
    if (r == MATCH_ERROR) return MATCH_ERROR;    
    if (r == MATCH_NO) goto syntax;    
    
    tail->next = g95_get_data_variable();  
    tail = tail->next;     
     
    *tail = var0;    
  }        
        
  if (g95_match_char(')') != MATCH_YES) goto syntax;       
  return MATCH_YES;    
    
syntax:       
  g95_syntax_error(ST_DATA);         
  return MATCH_ERROR;     
}      
      
      
       
       
static match match_data_constant(g95_expr **res) {     
char name0[G95_MAX_SYMBOL_LEN+1];    
g95_symbol *symb;          
g95_expr *e;    
match c;   
   
  c = g95_match_literal_constant(&e, 1);         
  if (c == MATCH_YES) {
    *res = e;      
    return MATCH_YES;  
  }          
          
  if (c == MATCH_ERROR) return MATCH_ERROR;      
      
  c = g95_match_null(res);        
  if (c != MATCH_NO) return c;

  c = g95_match_name(name0);        
  if (c != MATCH_YES) return c;          
          
  if (g95_find_symbol(name0, NULL, 1, &symb)) return MATCH_ERROR;  
  
  if (symb->attr.flavor == FL_DERIVED)      
    return g95_match_structure_constructor(symb, res); 
 
  if (symb == NULL || symb->attr.flavor != FL_PARAMETER) {         
    g95_error("Symbol '%s' must be a PARAMETER in DATA statement at %C", 
	      name0);     
    return MATCH_ERROR;          
  }   
   
  *res = g95_copy_expr(symb->value);        
  return MATCH_YES;  
}        
        
        


/* g95_match_namelist()-- Match a NAMELIST statement */    
    
match g95_match_namelist(void) {    
g95_symbol *group_name, *s;     
g95_namelist *nl;
match b, i;      
      
  b = g95_match(" / %s /", &group_name);         
  if (b == MATCH_NO) goto syntax;     
  if (b == MATCH_ERROR) goto error;        
        
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
      b = g95_match_symbol(&s, 1);     
      if (b == MATCH_NO) goto syntax;   
      if (b == MATCH_ERROR) goto error;

      if (s->attr.in_namelist == 0 &&  
	  g95_add_in_namelist(&s->attr, NULL) == FAILURE) goto error;        
        
/* TODO: worry about PRIVATE members of a PUBLIC namelist group */    
    
      nl = g95_get_namelist();         
      nl->sym = s;   
   
      if (group_name->namelist == NULL)   
	group_name->namelist = group_name->namelist_tail = nl;     
      else {     
	group_name->namelist_tail->next = nl;   
	group_name->namelist_tail = nl;          
      }      
      
      if (g95_match_eos() == MATCH_YES) goto done;   
   
      b = g95_match_char(',');

      if (g95_match_char('/') == MATCH_YES) {    
	i = g95_match(" %s /", &group_name);   
	if (i == MATCH_YES) break;    
	if (i == MATCH_ERROR) goto error;    
	goto syntax;       
      }          
          
      if (b != MATCH_YES) goto syntax;  
    } 
  }       
       
done:    
  return MATCH_YES; 
 
syntax:        
  g95_syntax_error(ST_NAMELIST);  
  
error:     
  return MATCH_ERROR;    
}   
   
   
       
       
/* g95_match_else()-- Match an ELSE statement */       
       
match g95_match_else(void) {        
char nam[G95_MAX_SYMBOL_LEN+1];          
           
  if (g95_match_eos() == MATCH_YES) return MATCH_YES;    
    
  if (g95_match_name(nam) != MATCH_YES || g95_current_block() == NULL ||
      g95_match_eos() != MATCH_YES) {      
    g95_error("Unexpected junk after ELSE statement at %C");   
    return MATCH_ERROR;    
  }         
         
  if (strcmp(nam, g95_current_block()->name) != 0) {     
    g95_error("Label '%s' at %C doesn't match IF label '%s'",          
	      nam, g95_current_block()->name); 
    return MATCH_ERROR;      
  }     
     
  return MATCH_YES;   
}         
         
         
        
        
/* top_var_list()-- Match the top-level list of data variables */      
      
static match top_var_list(g95_data *g) {  
g95_data_variable variable, *tail, *n;      
match b;      
      
  tail = NULL;     
     
  for(;;) {         
    b = var_element(&variable);     
    if (b == MATCH_NO) goto syntax;
    if (b == MATCH_ERROR) return MATCH_ERROR;

    n = g95_get_data_variable();     
    *n = variable;    
    
    if (tail == NULL)   
      g->var = n;          
    else   
      tail->next = n;     
     
    tail = n;          
          
    if (g95_match_char('/') == MATCH_YES) break;         
    if (g95_match_char(',') != MATCH_YES) goto syntax;       
  }     
     
  return MATCH_YES;

syntax:    
  g95_syntax_error(ST_DATA);  
  return MATCH_ERROR;         
}


          
          
/* var_element()-- Match a single element in a data variable list,
 * which can be a variable-iterator list. */ 
 
static match var_element(g95_data_variable *n1) {      
match d;

  memset(n1, '\0', sizeof(g95_data_variable));      
      
  if (g95_match_char('(') == MATCH_YES) return var_list(n1);          
          
  d = g95_match_variable(&n1->expr, 1);    
  if (d != MATCH_YES) return d; 
 
  if (n1->expr->symbol->value != NULL) {        
    g95_error("Variable '%s' at %C already has an initialization",       
	      n1->expr->symbol->name);          
    return MATCH_ERROR;   
  }         
         
  n1->expr->symbol->attr.data = 1;    
  return MATCH_YES;   
}  
  
  
       
       
/* match_forall_iterator()-- Match an iterator as part of a FORALL
 * statement.  The format is:
 *     <var> = <start>:<end>[:<stride>][, <scalar mask>]  */   
   
static match match_forall_iterator(g95_forall_iterator **res) {       
g95_forall_iterator *iter;      
g95_locus where;      
match h;      
      
  where = g95_current_locus;           
  iter = g95_getmem(sizeof(g95_forall_iterator));   
   
  h = g95_match_variable(&iter->var, 0);          
  if (h != MATCH_YES) {
    h = MATCH_NO;
    goto cleanup;  
  }      
      
  if (g95_match_char('=') != MATCH_YES) {          
    h = MATCH_NO; 
    goto cleanup;       
  }   
   
  h = g95_match_expr(&iter->start);       
  if (h == MATCH_NO) goto syntax;     
  if (h == MATCH_ERROR) goto cleanup;

  if (g95_match_char(':') != MATCH_YES) goto syntax;

  h = g95_match_expr(&iter->end);       
  if (h == MATCH_NO) goto syntax;       
  if (h == MATCH_ERROR) goto cleanup;  
  
  if (g95_match_char(':') == MATCH_NO)          
    iter->stride = g95_int_expr(1);          
  else {  
    h = g95_match_expr(&iter->stride);   
    if (h == MATCH_NO) goto syntax;   
    if (h == MATCH_ERROR) goto cleanup;       
  }     
     
  *res = iter;
  return MATCH_YES;         
         
syntax:       
  g95_error("Syntax error in FORALL iterator at %C");       
  h = MATCH_ERROR;         
         
cleanup:       
  g95_current_locus = where;        
  g95_free_forall_iterator(iter);       
  return h;     
}


       
       
/* g95_match_eos()-- Match an end of statement.  End of statement is
 * optional whitespace, followed by a ';' or '\n' or comment '!'.  If
 * a semicolon is found, we continue to eat whitespace and semicolons. */

match g95_match_eos(void) {   
g95_locus o;        
int flag, j;       
       
  flag = 0;        
        
  for(;;) { 
    o = g95_current_locus;         
    g95_gobble_whitespace();          
             
    j = g95_next_char();         
    switch(j) {  
    case '!': 
      do {     
	j = g95_next_char();          
      } while(j != '\n');         
         
      /* Fall through */  
    case '\n':      
      return MATCH_YES; 
 
    case ';':      
      flag = 1;     
      continue;        
    } 
 
    break;        
  }         
         
  g95_current_locus = o;     
  return (flag) ? MATCH_YES : MATCH_NO;   
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
g95_st_label **labl;      
g95_locus old;    
int matches, *ifp;       
va_list argp;         
char v, *np;    
match m, s;   
void **vp;          
char *d; 
 
  old = g95_current_locus;         
  va_start(argp, target);
  m = MATCH_NO;        
  matches = 0;          
  d = target;    
    
loop:     
  v = *d++;
  switch(v) {       
  case ' ':   g95_gobble_whitespace(); goto loop;       
  case '\0':  m = MATCH_YES; break;

  case '%':       
    v = *d++;    
    switch(v) {  
    case 'e':    
      vp = va_arg(argp, void **);         
      s = g95_match_expr((g95_expr **) vp); 
      if (s != MATCH_YES) { m = s; goto not_yes; }

      matches++;      
      goto loop;          
          
    case 'v':       
      vp = va_arg(argp, void **);     
      s = g95_match_variable((g95_expr **) vp, 0);   
      if (s != MATCH_YES) { m = s; goto not_yes; }     
     
      matches++;    
      goto loop;          
          
    case 's':
      vp = va_arg(argp, void **);      
      s = g95_match_symbol((g95_symbol **) vp, 0);   
      if (s != MATCH_YES) { m = s; goto not_yes; }

      matches++;
      goto loop;   
   
    case 'n':      
      np = va_arg(argp, char *);          
      s = g95_match_name(np);    
      if (s != MATCH_YES) { m = s; goto not_yes; }         
         
      matches++;   
      goto loop; 
 
    case 'l':      
      labl = va_arg(argp, g95_st_label **);         
      s = g95_match_st_label(labl, 0);
      if (s != MATCH_YES) { m = s; goto not_yes; }        
        
      matches++;     
      goto loop;   
   
    case 'o':   
      ifp = va_arg(argp, int *);        
      s = g95_match_intrinsic_op((g95_intrinsic_op *) ifp);
      if (s != MATCH_YES) { m = s; goto not_yes; }   
   
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
      g95_internal_error("g95_match(): Bad match code %c", v);        
    }    
    
  default:          
    if (v == g95_next_char()) goto loop; 
    break;        
  }         
         
not_yes:     
  va_end(argp); 
 
  if (m != MATCH_YES) {   /* Clean up after a failed match */          
    g95_current_locus = old;        
    va_start(argp, target);     
     
    d = target;    
    for(; matches>0; matches--) {    
      while(*d++ != '%');          
          
      switch(*d++) {       
      case '%': matches++; break;   /* Skip */         
         
      case 'I': case 'L': case 'C':   
	if (*d++ == 'e') goto undo_expr;         
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
      
      
         
         
/* g95_match_return()-- Match a RETURN statement */        
        
match g95_match_return(void) {
g95_expr *y; 
match u;

  y = NULL;   
  if (g95_match_eos() == MATCH_YES) goto done;   
   
  if (g95_find_state(COMP_SUBROUTINE) == FAILURE) {         
    g95_error("Alternate RETURN statement at %C is only allowed within "        
	      "a SUBROUTINE");          
    goto cleanup;     
  } 
 
  u = g95_match("% %e%t", &y); 
  if (u == MATCH_YES) goto done;
  if (u == MATCH_ERROR) goto cleanup;         
         
  g95_syntax_error(ST_RETURN);  
  
cleanup:    
  g95_free_expr(y);       
  return MATCH_ERROR;   
   
done:  
  new_st.type = EXEC_RETURN;        
  new_st.expr = y;    
    
  return MATCH_YES;     
}  
  
  
    
    
/* top_val_list()-- Match a list of values in a DATA statement.  The
 * leading '/' has already been seen at this point. */   
   
static match top_val_list(g95_data *data) {     
g95_data_value *old, *t;       
g95_expr *e2;
char *msg;     
match d;         
         
  t = NULL;       
       
  for(;;) {         
    d = match_data_constant(&e2);         
    if (d == MATCH_NO) goto syntax;        
    if (d == MATCH_ERROR) return MATCH_ERROR;  
  
    old = g95_get_data_value();         
         
    if (t == NULL)
      data->value = old;  
    else          
      t->next = old;

    t = old;   
   
    if (e2->ts.type != BT_INTEGER || g95_match_char('*') != MATCH_YES) {   
      t->expr = e2;      
      t->repeat = 1;   
    } else {     
      msg = g95_extract_int(e2, &t->repeat);        
      g95_free_expr(e2);  
      if (msg != NULL) {     
	g95_error(msg);       
	return MATCH_ERROR;
      }    
    
      d = match_data_constant(&t->expr); 
      if (d == MATCH_NO) goto syntax;       
      if (d == MATCH_ERROR) return MATCH_ERROR;      
    }        
        
    if (g95_option.verbose) {   
      g95_status("DATA element:  %d * ", t->repeat);      
      g95_show_expr(t->expr);    
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
      
      
     
     
/* g95_match_do()-- Match a DO statement */        
        
match g95_match_do(void) {  
g95_expr *while_condition;         
g95_iterator iter, *i;    
g95_st_label *label;    
g95_locus o;  
match z;      
      
  o = g95_current_locus;  
  
  label = NULL;   
  iter.var = iter.start = iter.end = iter.step = NULL;        
  while_condition = NULL;       
       
  z = g95_match_label();
  if (z == MATCH_ERROR) return z;     
     
  if (g95_match(" do") != MATCH_YES) return MATCH_NO;      
      
/* Match an infinite DO, make it like a DO WHILE(.TRUE.) */          
          
  if (g95_match_eos() == MATCH_YES) {      
    new_st.type = EXEC_DO_WHILE;       
    goto done;        
  }         
         
  z = g95_match_st_label(&label, 0); 
  if (z == MATCH_ERROR) goto cleanup;         
         
  if (g95_match_eos() == MATCH_YES) {         
    new_st.type = EXEC_DO_WHILE;         
    goto done;      
  }    
    
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
  g95_current_locus = o;         
         
  g95_match_label();    /* This won't error */          
  g95_match(" do ");    /* This will work */    
    
  g95_match_st_label(&label, 0);  /* Can't error out */   
  g95_match_char(',');            /* Optional comma */      
      
  z = g95_match_iterator(&iter, 0);     
  if (z == MATCH_NO) return MATCH_NO;
  if (z == MATCH_ERROR) goto cleanup;

  g95_check_do_variable(iter.var->symbol); 
 
  if (g95_match_eos() != MATCH_YES) {       
    g95_syntax_error(ST_DO);         
    goto cleanup;        
  }

  new_st.type = EXEC_DO;         
         
done:         
  if (label != NULL &&      
      g95_reference_st_label(label, ST_LABEL_TARGET) == FAILURE)   
    goto cleanup;        
        
  new_st.label = label;     
     
  if (new_st.type == EXEC_DO_WHILE)         
    new_st.expr = while_condition;      
  else {  
    new_st.ext.iterator = i = g95_get_iterator(); 
    *i = iter;        
  }  
  
  return MATCH_YES;    
    
cleanup:
  g95_free_iterator(&iter, 0);     
  return MATCH_ERROR;   
} 
 
 
    
    
/* g95_free_forall_iterator()-- Free a list of FORALL iterators */          
          
void g95_free_forall_iterator(g95_forall_iterator *iter) {        
g95_forall_iterator *n;       
       
  while(iter) { 
    n = iter->next;    
    
    g95_free_expr(iter->var);        
    g95_free_expr(iter->start);          
    g95_free_expr(iter->end);          
    g95_free_expr(iter->stride);      
      
    g95_free(iter);  
    iter = n;
  }    
}    
    
    


/* g95_match_module()-- Match a MODULE statement */ 
 
match g95_match_module(void) {
match t;      
      
  t = g95_match(" %s%t", &g95_new_block); 
  if (t != MATCH_YES) return t; 
 
  if (g95_add_flavor(&g95_new_block->attr, FL_MODULE, NULL) == FAILURE)  
    return MATCH_ERROR;         
         
  return MATCH_YES;         
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
 
 
 
 
/* g95_match_label()-- Match and validate a label associated with a
 * named IF, DO or SELECT statement.  If the symbol does not have the
 * label attribute, we add it.  We also make sure the symbol does not
 * refer to another (active) block.  A matched label is pointed to by
 * g95_new_block. */      
      
match g95_match_label(void) {    
char nam[G95_MAX_SYMBOL_LEN+1];        
g95_state_data *f;
match d;          
          
  g95_new_block = NULL; 
 
  d = g95_match(" %n :", nam);       
  if (d != MATCH_YES) return d;

  if (g95_get_symbol(nam, NULL, &g95_new_block)) {   
    g95_error("Label name '%s' at %C is ambiguous", nam);         
    return MATCH_ERROR;       
  }       
       
  if (g95_new_block->attr.flavor != FL_LABEL &&     
      g95_add_flavor(&g95_new_block->attr, FL_LABEL, NULL) == FAILURE)  
    return MATCH_ERROR;   
   
  for(f=g95_state_stack; f; f=f->previous)  
    if (f->sym == g95_new_block) {   
      g95_error("Label %s at %C already in use by a parent block",     
		g95_new_block->name);   
      return MATCH_ERROR;   
    }          
          
  return MATCH_YES; 
}          
          
          
  
  
/* g95_match_pause()-- Match the (deprecated) PAUSE statement */  
  
match g95_match_pause(void) {     
g95_expr *expr; 
match k;     
     
  expr = NULL;          
  if (g95_match_eos() == MATCH_YES) goto got_match;      
      
  k = g95_match(" %e%t", &expr); 
  if (k == MATCH_YES) goto got_match;         
  if (k == MATCH_NO) g95_syntax_error(ST_PAUSE);     
  return MATCH_ERROR;    
    
got_match:       
  new_st.type = EXEC_PAUSE;         
  new_st.expr = expr;     
  return MATCH_YES;         
}      
      
      
 
 
/* g95_get_common()-- Given a name, return a pointer to the common
 * head structure, creating it if it does not exist. */       
       
g95_common_head *g95_get_common(char *nm) {      
g95_symtree *st0;   
   
  st0 = g95_find_symtree(g95_current_ns->common_root, nm);      
  if (st0 == NULL) st0 = g95_new_symtree(&g95_current_ns->common_root, nm);     
     
  if (st0->n.common == NULL) {   
    st0->n.common = g95_get_common_head();       
    st0->n.common->where = g95_current_locus;          
  }         
         
  return st0->n.common;   
}       
       
       
     
     
/* match_common_name()-- Match a common block name.  Returns a null
 * string for the blank common. */ 
 
static match match_common_name(char *name0) {
match g; 
 
  if (g95_match_char('/') == MATCH_NO) {    
    name0[0] = '\0';   
    return MATCH_YES;   
  }

  if (g95_match_char('/') == MATCH_YES) {     
    name0[0] = '\0';          
    return MATCH_YES;      
  }    
    
  g = g95_match_name(name0);   
   
  if (g == MATCH_ERROR) return MATCH_ERROR;       
  if (g == MATCH_YES && g95_match_char('/') == MATCH_YES) return MATCH_YES; 
 
  g95_error("Syntax error in common block name at %C");          
  return MATCH_ERROR;
}       
       
       
   
   
/* g95_match_assign()-- Match the (deprecated) ASSIGN statement. */   
   
match g95_match_assign(void) {
g95_expr *e;
int l;        
        
  if (g95_match(" %l to %v%t", &l, &e) == MATCH_YES) {          
    g95_free_expr(e);         
    g95_error("The ASSIGN statement at %C is not allowed in Fortran 95");       
    return MATCH_ERROR;        
  }         
         
  return MATCH_NO;  
}      
      
      
  
  
/* g95_match_equivalence()-- Match an EQUIVALENCE statement */          
          
match g95_match_equivalence(void) {
g95_equiv *eq1, *set, *t;
g95_ref *reference;
g95_expr *q;      
match m;      
      
  t = NULL; 
 
  for(;;) {  
    eq1 = g95_get_equiv();      
    if (t == NULL) t = eq1;        
        
    eq1->next = g95_current_ns->equiv;         
    g95_current_ns->equiv = eq1;       
       
    if (g95_match_char('(') != MATCH_YES) goto syntax;     
     
    set = eq1;   
   
    for(;;) {       
      m = g95_match_variable(&q, 1);   
      if (m == MATCH_ERROR) goto cleanup;     
      if (m == MATCH_NO) goto syntax;          
          
      set->expr = q;        
      q->symbol->attr.equivalenced = 1;       
       
      for(reference=q->ref; reference; reference=reference->next) 
	if (reference->type == REF_ARRAY && reference->u.ar.type == AR_SECTION) {
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
  eq1 = t->next;     
  t->next = NULL;      
      
  g95_free_equiv(g95_current_ns->equiv);  
  g95_current_ns->equiv = eq1;  
  
  return MATCH_ERROR;   
}        
        
        


/* g95_match_data()-- Match a DATA statement */    
    
match g95_match_data(void) {         
g95_data *old;      
match v; 
   
  for(;;) {     
    old = g95_get_data(); 
    old->where = g95_current_locus;     
     
    v = top_var_list(old);
    if (v != MATCH_YES) goto cleanup;      
      
    v = top_val_list(old);
    if (v != MATCH_YES) goto cleanup; 
 
    old->next = g95_current_ns->data;   
    g95_current_ns->data = old;       
       
    if (g95_match_eos() == MATCH_YES) break; 
 
    g95_match_char(',');  /* Optional comma */          
  }    
    
  if (g95_pure(NULL)) {    
    g95_error("DATA statement at %C is not allowed in a PURE procedure");   
    return MATCH_ERROR;         
  }         
         
  return MATCH_YES;

cleanup:
  g95_free_data(old); 
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
         
int g95_match_strings(mstring *z) {
int no_match, d, possibles;          
mstring *h, *best_match;      
g95_locus match_loc;  
  
  possibles = 0;   
   
  for(h=z; h->string != NULL; h++) {      
    h->mp = h->string;   
    possibles++;        
  }

  no_match = h->tag;

  best_match = NULL;
  match_loc = g95_current_locus;  
  
  g95_gobble_whitespace();

  while(possibles > 0) {          
    d = g95_next_char();

/* Apply the next character to the current possibilities */

    for(h=z; h->string!=NULL; h++) {       
      if (h->mp == NULL) continue;          
          
      if (*h->mp == ' ') {    /* Space matches 1+ whitespace(s) */ 
	if ((g95_current_form == FORM_FREE) && g95_is_whitespace(d))         
	  continue;     
     
	h->mp++; 
      }       
       
      if (*h->mp != d) {      /* Match failed */ 
	h->mp = NULL;         
	possibles--;        
	continue; 
      }

      h->mp++; 
      if (*h->mp == '\0') {   /* Found a match */     
	match_loc = g95_current_locus;      
	best_match = h;    
	possibles--;      
	h->mp = NULL;      
      }  
    }          
  }     
     
  g95_current_locus = match_loc;   
   
  return (best_match == NULL) ? no_match : best_match->tag;
}  
  
  
       
       
/* g95_match_call()-- Match a CALL statement.  The tricky part here
 * are possible alternate return specifiers.  We handle these by
 * having all "subroutines" actually return an integer via a register
 * that gives the return number.  If the call specifies alternate
 * returns, we generate code for a SELECT statement whose case clauses
 * contain GOTOs to the various labels. */      
      
match g95_match_call(void) {         
char nm[G95_MAX_SYMBOL_LEN+1];      
g95_actual_arglist *p, *arglist;         
g95_symbol *symbol;      
match m;    
int k;  
  
  arglist = NULL;   
   
  m = g95_match("% %n", nm);
  if (m == MATCH_NO) goto syntax;         
  if (m != MATCH_YES) return m;   
   
  if (g95_get_ha_symbol(nm, &symbol)) return MATCH_ERROR;     
     
  if (!symbol->attr.generic && !symbol->attr.subroutine &&         
      g95_add_subroutine(&symbol->attr, NULL) == FAILURE)        
    return MATCH_ERROR;

  if (g95_match_eos() != MATCH_YES) {         
    m = g95_match_actual_arglist(1, &arglist);        
    if (m == MATCH_NO) goto syntax;    
    if (m == MATCH_ERROR) goto cleanup;      
      
    if (g95_match_eos() != MATCH_YES) goto syntax;        
  }      
      
  k = 0;         
  for(p=arglist; p; p=p->next)
    if (p->type == ALT_RETURN &&          
	g95_reference_st_label(p->u.label, ST_LABEL_TARGET) == FAILURE)         
      k = 1;       
           
  if (k) goto cleanup;        
        
  new_st.type = EXEC_CALL;      
  new_st.sym = symbol;        
  new_st.ext.actual = arglist;      
  return MATCH_YES;          
          
syntax:
  g95_syntax_error(ST_CALL);  
  
cleanup:
  g95_free_actual_arglist(arglist);          
  return MATCH_ERROR;        
}        
        
        
 
 
/* free_value()-- Free a g95_data_value structure and everything
 * beneath it */      
      
static void free_value(g95_data_value *o) {      
g95_data_value *e;         
         
  for(; o; o=e) {      
    e = o->next;          
    g95_free_expr(o->expr);         
    g95_free(o);      
  }   
}          
          
          
  
  
/* g95_match_space()-- In free form, match at least one space.  Always
 * matches in fixed form. */     
     
match g95_match_space(void) {
g95_locus old_loc;   
int c;   
   
  if (g95_current_form == FORM_FIXED) return MATCH_YES;    
    
  old_loc = g95_current_locus;   
   
  c = g95_next_char();         
  if (!g95_is_whitespace(c)) {         
    g95_current_locus = old_loc;          
    return MATCH_NO; 
  }  
  
  g95_gobble_whitespace();

  return MATCH_YES;      
}   
   
   
    
    
/* g95_free_namelist()-- Free a namelist structure */      
      
void g95_free_namelist(g95_namelist *name) {
g95_namelist *r;

  for(;name; name=r) {    
    r = name->next;   
    g95_free(name);       
  }        
}        
        
        
 
 
/* match_forall_header()-- Match the header of a FORALL statement. */        
        
static match match_forall_header(g95_forall_iterator **header,         
				 g95_expr **mask) {  
g95_forall_iterator *h, *end, *new;     
match t; 
 
  g95_gobble_whitespace();       
       
  h = end = NULL;    
  *mask = NULL;        
        
  if (g95_match_char('(') != MATCH_YES)     
    return MATCH_NO;    
    
  t = match_forall_iterator(&new);  
  if (t == MATCH_ERROR) goto cleanup; 
  if (t == MATCH_NO) goto syntax;

  h = end = new;     
     
  for(;;) {       
    if (g95_match_char(',') != MATCH_YES) break;     
     
    t = match_forall_iterator(&new);          
    if (t == MATCH_ERROR) goto cleanup;  
    if (t == MATCH_YES) {         
      end->next = new;  
      end = new;   
      continue;          
    }     
     
/* Have to have a mask expression */   
   
    t = g95_match_expr(mask);        
    if (t == MATCH_NO) goto syntax;      
    if (t == MATCH_ERROR) goto cleanup;   
   
    break;  
  } 
 
  if (g95_match_char(')') == MATCH_NO) goto syntax;   
   
  *header = h;      
  return MATCH_YES;         
         
syntax:          
  g95_syntax_error(ST_FORALL);

cleanup:        
  g95_free_expr(*mask);      
  g95_free_forall_iterator(h);      
      
  return MATCH_ERROR;    
}          
          
          
        
        
/* match_simple_forall()-- Match the rest of a simple FORALL statement
 * that follows an IF statement. */      
      
static match match_simple_forall(void) {        
g95_forall_iterator *head;
g95_expr *mask;  
g95_code *p; 
match z;         
         
  mask = NULL;  
  head = NULL;      
  p = NULL;    
    
  z = match_forall_header(&head, &mask);     
     
  if (z == MATCH_NO) goto syntax;    
  if (z != MATCH_YES) goto cleanup;  
  
  z = g95_match_assignment();          
          
  if (z == MATCH_ERROR) goto cleanup;         
  if (z == MATCH_NO) {    
    z = g95_match_pointer_assignment();
    if (z == MATCH_ERROR) goto cleanup;        
    if (z == MATCH_NO) goto syntax;    
  }   
   
  p = g95_get_code();     
  *p = new_st;       
  p->where = g95_current_locus;    
    
  if (g95_match_eos() != MATCH_YES) goto syntax;          
          
  g95_clear_new_st(); 
  new_st.type = EXEC_FORALL;   
  new_st.expr = mask;    
  new_st.ext.forall_iterator = head;    
  new_st.block = g95_get_code();

  new_st.block->type = EXEC_FORALL;        
  new_st.block->next = p;       
       
  return MATCH_YES;   
     
syntax:         
  g95_syntax_error(ST_FORALL);

cleanup:
  g95_free_forall_iterator(head);       
  g95_free_expr(mask);   
  g95_free_statements(p);          
          
  return MATCH_ERROR;  
}


    
    
/* g95_match_forall()-- Match a FORALL statement */      
      
match g95_match_forall(g95_statement *st) {       
g95_forall_iterator *head; 
g95_expr *msk;     
g95_code *u;      
match e, a;          
          
  head = NULL;          
  msk = NULL;         
  u = NULL;          
          
  e = g95_match_label();         
  if (e == MATCH_ERROR) return MATCH_ERROR;       
       
  a = g95_match(" forall");
  if (a != MATCH_YES) return a;    
    
  a = match_forall_header(&head, &msk);          
  if (a == MATCH_ERROR) goto cleanup;       
  if (a == MATCH_NO) goto syntax;      
      
  if (g95_match_eos() == MATCH_YES) {  
    *st = ST_FORALL_BLOCK;      
      
    new_st.type = EXEC_FORALL;     
    new_st.expr = msk; 
    new_st.ext.forall_iterator = head;        
        
    return MATCH_YES;         
  } 
 
  a = g95_match_assignment(); 
  if (a == MATCH_ERROR) goto cleanup;
  if (a == MATCH_NO) {      
    a = g95_match_pointer_assignment();      
    if (a == MATCH_ERROR) goto cleanup;
    if (a == MATCH_NO) goto syntax;   
  }    
    
  u = g95_get_code();          
  *u = new_st;     
  u->where = g95_current_locus;     
     
  if (g95_match_eos() != MATCH_YES) goto syntax;    
    
  g95_clear_new_st();
  new_st.type = EXEC_FORALL;    
  new_st.expr = msk;      
  new_st.ext.forall_iterator = head;      
  new_st.block = g95_get_code();    
    
  new_st.block->type = EXEC_FORALL;        
  new_st.block->next = u;   
   
  *st = ST_FORALL;        
  return MATCH_YES;      
      
syntax:     
  g95_syntax_error(ST_FORALL); 
 
cleanup:    
  g95_free_forall_iterator(head);     
  g95_free_expr(msk);       
  g95_free_statements(u);

  return MATCH_NO;    
}         
         
         
    
    
/* stf_pre_match()-- See if the thing on the input sort of looks like a
 * statement function.  This is:  WORD ( [ WORD [, WORD ] ... ] ) =
 * The idea is to eliminate things that can't possibly be statement
 * functions without messing with the symbol table.  We never return
 * error. */

static match stf_pre_match(void) { 
char name[G95_MAX_SYMBOL_LEN+1];   
g95_locus where;
match k;       
char d;

  where = g95_current_locus;    
    
  k = MATCH_NO;         
  if (g95_match_name(name) != MATCH_YES) goto done;          
  if (g95_match_char('(') != MATCH_YES) goto done;        
        
  g95_gobble_whitespace();   
  if (g95_peek_char() == ')')  
    g95_next_char();    
  else {    
    for(;;) {
      if (g95_match_name(name) != MATCH_YES) goto done;      
      
      g95_gobble_whitespace();     
      d = g95_next_char(); 

      if (d == ')') break;     
      if (d != ',') goto done;  
    }    
  }     
     
  k = g95_match_char('='); 
 
done: 
  g95_current_locus = where;  
  return k;        
}       
       
       
         
         
/* g95_match_elseif()-- Match an ELSE IF statement */        
        
match g95_match_elseif(void) {         
char nm[G95_MAX_SYMBOL_LEN+1];
g95_expr *e1;       
match n;          
          
  n = g95_match(" ( %e ) then", &e1);         
  if (n != MATCH_YES) return n;         
         
  if (g95_match_eos() == MATCH_YES) goto done;

  if (g95_match_name(nm) != MATCH_YES || g95_current_block() == NULL ||     
      g95_match_eos() != MATCH_YES) {         
    g95_error("Unexpected junk after ELSE IF statement at %C");         
    goto cleanup;        
  }          
          
  if (strcmp(nm, g95_current_block()->name) != 0) {          
    g95_error("Label '%s' at %C doesn't match IF label '%s'",      
	      nm, g95_current_block()->name);     
    goto cleanup;
  }          
          
done:        
  new_st.type = EXEC_IF;   
  new_st.expr = e1;   
  return MATCH_YES;

cleanup:  
  g95_free_expr(e1);  
  return MATCH_ERROR;  
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
g95_st_label *v, *s, *l3;      
g95_locus old_loc;   
match mm, b, h;          
g95_expr *e2; 
g95_code *j;    
    
  h = g95_match_label();      
  if (h == MATCH_ERROR) return h;      
      
  old_loc = g95_current_locus;

  b = g95_match(" if ( %e", &e2); 
  if (b != MATCH_YES) return b;      
      
  if (g95_match_char(')') != MATCH_YES) {         
    g95_error("Syntax error in IF-expression at %C");     
    g95_free_expr(e2); 
    return MATCH_ERROR;          
  }    
    
  b = g95_match(" %l , %l , %l%t", &v, &s, &l3);       
       
  if (b == MATCH_YES) {      
    if (h == MATCH_YES) { 
      g95_error("Block label not appropriate for arithmetic IF statement "       
		"at %C");         
         
      g95_free_expr(e2);          
      return MATCH_ERROR;     
    }        
        
    if (g95_reference_st_label(v, ST_LABEL_TARGET) == FAILURE ||    
	g95_reference_st_label(s, ST_LABEL_TARGET) == FAILURE ||       
	g95_reference_st_label(l3, ST_LABEL_TARGET) == FAILURE) {        
      g95_free_expr(e2);    
      return MATCH_ERROR;          
    }  
  
    new_st.type = EXEC_ARITHMETIC_IF;  
    new_st.expr = e2;        
    new_st.label  = v;       
    new_st.label2 = s;    
    new_st.label3 = l3;         
         
    *if_type = ST_ARITHMETIC_IF; 
    return MATCH_YES;        
  }          
          
  if (g95_match(" then %t") == MATCH_YES) {         
    new_st.type = EXEC_IF;      
    new_st.expr = e2;   
   
    *if_type = ST_IF_BLOCK; 
    return MATCH_YES;          
  }          
          
  if (h == MATCH_YES) {
    g95_error("Block label is not appropriate for the IF statement at %C");          
          
    g95_free_expr(e2);       
    return MATCH_ERROR;       
  }    
    
/* At this point the only thing left is a simple IF statement.  At
 * this point, n has to be MATCH_NO, so we don't have to worry about
 * re-matching a block label.  From what we've got so far, try
 * matching an assignment. */         
         
  *if_type = ST_SIMPLE_IF;

  mm = g95_match_assignment();         
  if (mm == MATCH_YES) goto got_match;  
  
  g95_free_expr(e2);  
  g95_undo_symbols();
  g95_current_locus = old_loc;        
        
  g95_match(" if ( %e ) ", &e2);  /* Guaranteed to match */         
         
  mm = g95_match_pointer_assignment();      
  if (mm == MATCH_YES) goto got_match;          
          
  g95_free_expr(e2);   
  g95_undo_symbols();          
  g95_current_locus = old_loc;

  g95_match(" if ( %e ) ", &e2);  /* Guaranteed to match */         
         
/* Look at the next word to see which matcher to call.  Matching the
 * keyword doesn't affect the symbol table, so we don't have to
 * restore between tries. */

#define match(string, subr, statement) \
  if (g95_match(string) == MATCH_YES) { mm = subr(); goto got_match; }
     
  g95_clear_error();   
   
  match("allocate",   g95_match_allocate,    ST_ALLOCATE)          
  match("assign",     g95_match_assign,      ST_NONE)   
  match("backspace",  g95_match_backspace,   ST_BACKSPACE)          
  match("call",       g95_match_call,        ST_CALL)   
  match("close",      g95_match_close,       ST_CLOSE)   
  match("continue",   g95_match_continue,    ST_CONTINUE)          
  match("cycle",      g95_match_cycle,       ST_CYCLE) 
  match("deallocate", g95_match_deallocate,  ST_DEALLOCATE)      
  match("end file",   g95_match_endfile,     ST_END_FILE)    
  match("exit",       g95_match_exit,        ST_EXIT)         
  match("forall",     match_simple_forall,   ST_FORALL)          
  match("go to",      g95_match_goto,        ST_GOTO)   
  match("if",         match_arithmetic_if,   ST_ARITHMETIC_IF)        
  match("inquire",    g95_match_inquire,     ST_INQUIRE)         
  match("nullify",    g95_match_nullify,     ST_NULLIFY) 
  match("open",       g95_match_open,        ST_OPEN)     
  match("pause",      g95_match_pause,       ST_NONE)   
  match("print",      g95_match_print,       ST_WRITE)    
  match("read",       g95_match_read,        ST_READ)        
  match("return",     g95_match_return,      ST_RETURN)
  match("rewind",     g95_match_rewind,      ST_REWIND)           
  match("stop",       g95_match_stop,        ST_STOP) 
  match("where",      match_simple_where,    ST_WHERE)
  match("write",      g95_match_write,       ST_WRITE)      
      
/* All else has failed, so give up.  See if any of the matchers has
 * stored an error message of some sort. */

  if (g95_error_check() == 0)
    g95_error("Unclassifiable statement following IF-clause at %C"); 
 
  g95_free_expr(e2);         
  return MATCH_ERROR;   
   
got_match:         
  if (mm == MATCH_NO) g95_error("Syntax error in IF-clause at %C"); 
  if (mm != MATCH_YES) {   
    g95_free_expr(e2);          
    return MATCH_ERROR;      
  }        
        
/* At this point, we've matched the single IF and the action clause is
 * in new_st.  Rearrange things so that the IF statement appears in new_st */      
      
  j = g95_get_code();          
  *j = new_st;      
  j->where = g95_current_locus;      
      
  g95_clear_new_st();

  new_st.type = EXEC_IF;          
  new_st.block = j;         
  new_st.expr = e2;   
   
  return MATCH_YES;       
}

#undef match
     
     
     
     
/* match_exit_cycle()-- Match an EXIT or CYCLE statement */       
       
static match match_exit_cycle(g95_statement st0, g95_exec_op operand) {       
g95_state_data *t;        
g95_symbol *symb;      
match m; 
 
  if (g95_match_eos() == MATCH_YES)
    symb = NULL;     
  else {   
    m = g95_match("% %s%t", &symb);         
    if (m == MATCH_ERROR) return MATCH_ERROR;        
    if (m == MATCH_NO) {       
      g95_syntax_error(st0);      
      return MATCH_ERROR;     
    }        
        
    if (symb->attr.flavor != FL_LABEL) {   
      g95_error("Name '%s' in %s statement at %C is not a loop name",          
		symb->name, g95_ascii_statement(st0));  
      return MATCH_ERROR;  
    }       
  }      
      
/* Find the loop mentioned specified by the label (or lack of a label) */         
         
  for(t=g95_state_stack; t; t=t->previous)      
    if (t->state == COMP_DO && (symb == NULL || symb == t->sym)) break;   
   
  if (t == NULL) {     
    if (symb == NULL)         
      g95_error("%s statement at %C is not within a loop",  
		g95_ascii_statement(st0));      
    else         
      g95_error("%s statement at %C is not within loop '%s'",         
		g95_ascii_statement(st0), symb->name);       
       
    return MATCH_ERROR;    
  }          
          
  /* Save the first statement in the loop - needed by the backend */          
          
  new_st.ext.block = t->top;
  new_st.type = operand;  
  new_st.sym = symb; 
 
  return MATCH_YES;        
}   
   
   
          
          
/* g95_match_st_function()-- Match a statement function declaration.
 * It is so easy to match non-statement function statements with a
 * MATCH_ERROR as opposed to MATCH_NO that we suppress error message
 * in most cases. */     
     
match g95_match_st_function(void) {    
g95_error_buf old_error;          
g95_expr *t, *exp;       
g95_namespace *namesp;   
g95_symbol *sy;         
g95_symtree *st0;
g95_code *h;  
match w;          
          
  if (stf_pre_match() == MATCH_NO) return MATCH_NO;    
    
  w = g95_match_symbol(&sy, 0);       
  if (w != MATCH_YES) return w;     
     
  g95_push_error(&old_error);  
  
  if (g95_add_procedure(&sy->attr, PROC_ST_FUNCTION, NULL) == FAILURE)          
    goto undo_error;         
         
  if (g95_match_formal_arglist(sy, 1, 0) != MATCH_YES) goto undo_error;  
  
  w = g95_match(" = %e%t", &exp); 
  if (w == MATCH_NO) goto undo_error;   
  if (w == MATCH_ERROR) return w;     
     
  /* At this point, we have a statement function.  These are
   * implemented by turning them into the equivalent contained
   * procedure.  The dummy arguments are copied into the contained
   * space during resolution. */     
     
  sy->result = sy;

  t = g95_get_expr();     
  t->type = EXPR_VARIABLE;         
  t->symbol = sy; 
  t->where = g95_current_locus;     
     
  h = g95_get_code();       
  h->type = EXEC_ASSIGN;      
      
  h->expr = t;   
  h->expr2 = exp;   
   
  namesp = g95_get_namespace(g95_current_ns, 1);  
  namesp->proc_name = sy;  
  namesp->code = h;    
  namesp->parent = g95_current_ns;          
          
  namesp->sibling = g95_current_ns->contained;   
  g95_current_ns->contained = namesp;    
    
  st0 = g95_new_symtree(&namesp->sym_root, sy->name);        
  st0->n.sym = sy;       
       
  sy->refs++; 
 
  return MATCH_YES;         
         
undo_error:          
  g95_pop_error(&old_error);       
  return MATCH_NO;   
}         
         
         
   
   
/* g95_match_cycle()-- Match the CYCLE statement */

match g95_match_cycle(void) {         
         
  return match_exit_cycle(ST_CYCLE, EXEC_CYCLE);         
}        
        
        
    
    
/* g95_match_elsewhere()-- Match an ELSEWHERE statement.  We leave
 * behind a WHERE node in new_st if successful. */ 
 
match g95_match_elsewhere(void) { 
char n[G95_MAX_SYMBOL_LEN+1];    
g95_expr *e2; 
match y;     
     
  if (g95_current_state() != COMP_WHERE) {          
    g95_error("ELSEWHERE statement at %C not enclosed in WHERE block");    
    return MATCH_ERROR;      
  }         
         
  e2 = NULL;         
         
  if (g95_match_char('(') == MATCH_YES) {         
    y = g95_match_expr(&e2);  
    if (y == MATCH_NO) goto syntax; 
    if (y == MATCH_ERROR) return MATCH_ERROR;          
          
    if (g95_match_char(')') != MATCH_YES) goto syntax;        
  }  
  
  if (g95_match_eos() != MATCH_YES) { /* Better be a name at this point */        
    y = g95_match_name(n);
    if (y == MATCH_NO) goto syntax;       
    if (y == MATCH_ERROR) goto cleanup; 
 
    if (g95_match_eos() != MATCH_YES) goto syntax;      
      
    if (strcmp(n, g95_current_block()->name) != 0) {         
      g95_error("Label '%s' at %C doesn't match WHERE label '%s'",         
		n, g95_current_block()->name);   
      goto cleanup;     
    }       
  }    
    
  new_st.type = EXEC_WHERE;     
  new_st.expr = e2;    
  return MATCH_YES;   
   
syntax:    
  g95_syntax_error(ST_ELSEWHERE);         
         
cleanup: 
  g95_free_expr(e2); 
  return MATCH_ERROR;   
}         
         
 
 
/* g95_match_exit()-- Match the EXIT statement */          
          
match g95_match_exit(void) {         
         
  return match_exit_cycle(ST_EXIT,  EXEC_EXIT);  
}    
    
    
    
    
/* g95_match_common()-- Match a COMMON statement */       
       
match g95_match_common(void) {          
g95_symbol *symbol, **start, *tail, *old_blank_common;  
char nm[G95_MAX_SYMBOL_LEN+1]; 
g95_common_head *v;  
g95_array_spec *ar; 
match t;         
         
  old_blank_common = g95_current_ns->blank_common.head;      
  if (old_blank_common != NULL) {    
    while(old_blank_common->common_next != NULL) 
      old_blank_common = old_blank_common->common_next;     
  }   
   
  ar = NULL;      
      
  if (g95_match_eos() == MATCH_YES) goto done;         
         
  for(;;) {      
    t = match_common_name(nm);    
    if (t == MATCH_ERROR) goto cleanup; 
 
    if (nm[0] == '\0') {
      v = &g95_current_ns->blank_common;       
      if (v->head == NULL) v->where = g95_current_locus;      
      start = &v->head;     
    } else { 
      v = g95_get_common(nm);  
      start = &v->head;  
  
      if (v->use_assoc) {       
	g95_error("COMMON block '%s' at %C has already been USE-associated");          
	goto cleanup;         
      }      
    } 
 
    if (*start == NULL)        
      tail = NULL;    
    else {     
      tail = *start;    
      while(tail->common_next)
	tail = tail->common_next;      
    }

/* Grab the list of symbols for this common. */    
    if (g95_match_eos() == MATCH_YES) goto done;     
     
    for(;;) {  
      t = g95_match_symbol(&symbol, 0);        
      if (t == MATCH_ERROR) goto cleanup;      
      if (t == MATCH_NO) goto syntax; 
 
      if (symbol->attr.in_common) {
	g95_error("Symbol '%s' at %C is already in a COMMON block", symbol->name);         
	goto cleanup;     
      }   
   
      if (g95_add_in_common(&symbol->attr, NULL) == FAILURE) goto cleanup;

      if (tail != NULL)         
	tail->common_next = symbol;   
      else
	*start = symbol;        
        
      tail = symbol;     
     
/* Deal with an optional array specification after the symbol name */       
       
      t = g95_match_array_spec(&ar);         
      if (t == MATCH_ERROR) goto cleanup;        
      if (t == MATCH_YES) {
	if (g95_add_dimension(&symbol->attr, NULL) == FAILURE) goto cleanup;     
     
	symbol->as = ar;  
	ar = NULL;          
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
    g95_current_ns->blank_common.head = NULL;          
          
  g95_free_array_spec(ar);       
  return MATCH_ERROR;        
}          
          
          
        
        
/* g95_match_allocate()-- Match an ALLOCATE statement */          
          
match g95_match_allocate(void) {        
g95_alloc *start, *end;     
g95_expr *stat;     
match p;      
      
  start = end = NULL; 
  stat = NULL;

  if (g95_match_char('(') != MATCH_YES) goto syntax;        
        
  for(;;) {      
    if (start == NULL)      
      start = end = g95_get_alloc();         
    else {    
      end->next = g95_get_alloc();     
      end = end->next;        
    }      
      
    p = g95_match_variable(&end->expr, 0);  
    if (p == MATCH_NO) goto syntax;     
    if (p == MATCH_ERROR) goto cleanup;     
     
    if (g95_pure(NULL) && g95_impure_variable(end->expr->symbol)) {
      g95_error("Bad allocate-object in ALLOCATE statement at %C for a " 
		"PURE procedure");       
      goto cleanup;    
    }    
    
    if (g95_match_char(',') != MATCH_YES) break;         
         
    p = g95_match(" stat = %v", &stat);    
    if (p == MATCH_ERROR) goto cleanup; 
    if (p == MATCH_YES) break;   
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
  new_st.ext.alloc_list = start;  
  
  return MATCH_YES;          
          
syntax:        
  g95_syntax_error(ST_ALLOCATE);        
        
cleanup:       
  g95_free_expr(stat);      
  g95_free_alloc_list(start);         
  return MATCH_ERROR; 
}   
   
   
          
          
/* g95_free_equiv()-- Free equivalence sets and lists.  Recursively is
 * the easiest way to do this. */          
          
void g95_free_equiv(g95_equiv *eq0) {   
   
  if (eq0 == NULL) return;    
    
  g95_free_equiv(eq0->eq);     
  g95_free_equiv(eq0->next);

  g95_free_expr(eq0->expr);
  g95_free(eq0);        
}      
      
      
    
    
/* g95_free_data()-- Free a list of g95_data structures */         
         
void g95_free_data(g95_data *j) {    
g95_data *q;

  for(; j; j=q) {        
    q = j->next;

    free_variable(j->var);        
    free_value(j->value);        
        
    g95_free(j);
  }       
}




/* g95_match_where()-- Match a WHERE statement */     
     
match g95_match_where(g95_statement *st) {     
g95_expr *exp;
match k, r;   
g95_code *f;   
   
  k = g95_match_label();
  if (k == MATCH_ERROR) return k;         
         
  r = g95_match(" where ( %e )", &exp);   
  if (r != MATCH_YES) return r;    
    
  if (g95_match_eos() == MATCH_YES) {         
    *st = ST_WHERE_BLOCK;     
     
    new_st.type = EXEC_WHERE;     
    new_st.expr = exp;
    return MATCH_YES;    
  }

  r = g95_match_assignment();       
  if (r == MATCH_NO) g95_syntax_error(ST_WHERE); 
 
  if (r != MATCH_YES) {        
    g95_free_expr(exp);       
    return MATCH_ERROR;  
  }       
       
/* We've got a simple WHERE statement */          
          
  *st = ST_WHERE;        
  f = g95_get_code();       
       
  f->type = EXEC_WHERE;       
  f->expr = exp;      
  f->next = g95_get_code();    
    
  *f->next = new_st;       
  g95_clear_new_st();

  new_st.type = EXEC_WHERE;
  new_st.block = f;

  return MATCH_YES;         
}          
          
          
