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
        
   
   
/* match_exit_cycle()-- Match an EXIT or CYCLE statement */

static match match_exit_cycle(g95_statement st1, g95_exec_op operand) {       
g95_state_data *o;   
g95_symbol *symbol;       
match v;          
          
  if (g95_match_eos() == MATCH_YES)
    symbol = NULL;        
  else {       
    v = g95_match("% %s%t", &symbol); 
    if (v == MATCH_ERROR) return MATCH_ERROR;    
    if (v == MATCH_NO) {          
      g95_syntax_error(st1);       
      return MATCH_ERROR;
    }   
   
    if (symbol->attr.flavor != FL_LABEL) {    
      g95_error("Name '%s' in %s statement at %C is not a loop name",    
		symbol->name, g95_ascii_statement(st1));        
      return MATCH_ERROR;    
    }          
  }  
  
/* Find the loop mentioned specified by the label (or lack of a label) */ 
 
  for(o=g95_state_stack; o; o=o->previous)         
    if (o->state == COMP_DO && (symbol == NULL || symbol == o->sym)) break;

  if (o == NULL) {       
    if (symbol == NULL)       
      g95_error("%s statement at %C is not within a loop",        
		g95_ascii_statement(st1));     
    else      
      g95_error("%s statement at %C is not within loop '%s'",         
		g95_ascii_statement(st1), symbol->name);        
        
    return MATCH_ERROR;        
  }  
  
  /* Save the first statement in the loop - needed by the backend */    
    
  new_st.ext.block = o->top;   
  new_st.type = operand; 
  new_st.sym = symbol;        
        
  return MATCH_YES;       
}    
    
    
         
         
/* g95_match_symbol()-- Match a symbol on the input.  Modifies the
 * pointer to the symbol pointer if successful. */  
  
match g95_match_symbol(g95_symbol **matched_symbol, int host_assoc) {   
char buffer[G95_MAX_SYMBOL_LEN+1];    
match v;        
        
  v = g95_match_name(buffer);  
  if (v != MATCH_YES) return v;    
    
  if (host_assoc)        
    return (g95_get_ha_symbol(buffer, matched_symbol)) 
      ? MATCH_ERROR : MATCH_YES;  
  
  if (g95_get_symbol(buffer, NULL, matched_symbol)) return MATCH_ERROR;      
      
  return MATCH_YES;         
}          
          
          
  
  
/* g95_match_small_int()-- Match a small, constant integer expression,
 * like in a kind statement.  On MATCH_YES, 'value' is set. */ 
 
match g95_match_small_int(int *value) {  
g95_expr *e;         
char *p;      
match u;  
int k;         
         
  u = g95_match_expr(&e);       
  if (u != MATCH_YES) return u;

  p = g95_extract_int(e, &k);       
  g95_free_expr(e);         
         
  if (p != NULL) {          
    g95_error(p);   
    u = MATCH_ERROR;        
  }         
         
  *value = k;     
  return u;         
}          
          
          
    
    
/* g95_match_elsewhere()-- Match an ELSEWHERE statement.  We leave
 * behind a WHERE node in new_st if successful. */         
         
match g95_match_elsewhere(void) {     
char n[G95_MAX_SYMBOL_LEN+1];    
g95_expr *expr;      
match l;    
    
  if (g95_current_state() != COMP_WHERE) {       
    g95_error("ELSEWHERE statement at %C not enclosed in WHERE block");     
    return MATCH_ERROR;   
  }  
  
  expr = NULL; 
 
  if (g95_match_char('(') == MATCH_YES) {
    l = g95_match_expr(&expr);          
    if (l == MATCH_NO) goto syntax;         
    if (l == MATCH_ERROR) return MATCH_ERROR;     
     
    if (g95_match_char(')') != MATCH_YES) goto syntax;   
  }      
      
  if (g95_match_eos() != MATCH_YES) { /* Better be a name at this point */   
    l = g95_match_name(n);          
    if (l == MATCH_NO) goto syntax;         
    if (l == MATCH_ERROR) goto cleanup;  
  
    if (g95_match_eos() != MATCH_YES) goto syntax;     
     
    if (strcmp(n, g95_current_block()->name) != 0) {   
      g95_error("Label '%s' at %C doesn't match WHERE label '%s'",          
		n, g95_current_block()->name);       
      goto cleanup;     
    }    
  }         
         
  new_st.type = EXEC_WHERE;      
  new_st.expr = expr;        
  return MATCH_YES;  
  
syntax:  
  g95_syntax_error(ST_ELSEWHERE); 
 
cleanup:       
  g95_free_expr(expr);   
  return MATCH_ERROR;          
}   
   
   
       
       
/* g95_match_block_data()-- Match a BLOCK DATA program unit */        
        
match g95_match_block_data(void) {         
char nm[G95_MAX_SYMBOL_LEN+1]; 
g95_symbol *s;  
match x;        
        
  if (g95_match_eos() == MATCH_YES) {     
    g95_new_block = NULL;
    return MATCH_YES;    
  }       
    
  x = g95_match(" %n%t", nm);  
  if (x != MATCH_YES) return MATCH_ERROR; 
 
  if (g95_get_symbol(nm, NULL, &s)) return MATCH_ERROR;      
      
  if (g95_add_flavor(&s->attr, FL_BLOCK_DATA, NULL) == FAILURE)       
    return MATCH_ERROR;          
          
  g95_new_block = s;      
      
  return MATCH_YES;        
}  
  
  
    
    
static match match_data_constant(g95_expr **r) {     
char nm[G95_MAX_SYMBOL_LEN+1];     
g95_symbol *symb; 
g95_expr *e2;    
match p;         
         
  p = g95_match_literal_constant(&e2, 1); 
  if (p == MATCH_YES) {      
    *r = e2;          
    return MATCH_YES;         
  }         
         
  if (p == MATCH_ERROR) return MATCH_ERROR;  
  
  p = g95_match_null(r);     
  if (p != MATCH_NO) return p;     
     
  p = g95_match_name(nm);          
  if (p != MATCH_YES) return p;    
    
  if (g95_find_symbol(nm, NULL, 1, &symb)) return MATCH_ERROR;          
          
  if (symb == NULL || symb->attr.flavor != FL_PARAMETER) {    
    g95_error("Symbol '%s' must be a PARAMETER in DATA statement at %C",
	      nm);       
    return MATCH_ERROR;       
  }    
    
  *r = g95_copy_expr(symb->value);          
  return MATCH_YES;        
}      
      
      
  
  
/* g95_match_st_label()-- Matches a statement label.  Uses
 * g95_match_small_literal_int() to do most of the work. */   
   
match g95_match_st_label(g95_st_label **label, int allow_zero) {   
int label_value;     
locus old;  
match k;

  old = *g95_current_locus();      
      
  k = g95_match_small_literal_int(&label_value);       
  if (k != MATCH_YES) return k;   
   
  if ((label_value == 0 && allow_zero) || label_value <= 99999) { 
    *label = g95_get_st_label(label_value);   
    return MATCH_YES; 
  }    
    
  g95_error("Statement label at %C is out of range");
  g95_set_locus(&old);  
  return MATCH_ERROR;      
}   
   
   
    
    
/* g95_match_equivalence()-- Match an EQUIVALENCE statement */          
          
match g95_match_equivalence(void) {       
g95_equiv *eq, *set, *t;       
g95_ref *r;     
g95_expr *f;
match p;   
   
  t = NULL;      
      
  for(;;) {        
    eq = g95_get_equiv();        
    if (t == NULL) t = eq;     
     
    eq->next = g95_current_ns->equiv;       
    g95_current_ns->equiv = eq;    
    
    if (g95_match_char('(') != MATCH_YES) goto syntax;         
         
    set = eq;

    for(;;) {         
      p = g95_match_variable(&f, 1);  
      if (p == MATCH_ERROR) goto cleanup;  
      if (p == MATCH_NO) goto syntax;   
   
      set->expr = f;
      f->symbol->attr.equivalenced = 1;    
    
      for(r=f->ref; r; r=r->next)         
	if (r->type == REF_ARRAY && r->u.ar.type == AR_SECTION) {    
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
  eq = t->next;
  t->next = NULL;     
     
  g95_free_equiv(g95_current_ns->equiv);          
  g95_current_ns->equiv = eq;

  return MATCH_ERROR;         
}     
     
     
         
         
/* g95_match_deallocate()-- Match a DEALLOCATE statement */ 
 
match g95_match_deallocate(void) {      
g95_alloc *h, *tail;   
g95_expr *stat;         
match i;          
          
  h = tail = NULL;        
  stat = NULL;          
          
  if (g95_match_char('(') != MATCH_YES) goto syntax;      
      
  for(;;) {          
    if (h == NULL)        
      h = tail = g95_get_alloc();         
    else {      
      tail->next = g95_get_alloc();     
      tail = tail->next; 
    }        
        
    i = g95_match_variable(&tail->expr, 0);       
    if (i == MATCH_ERROR) goto cleanup;        
    if (i == MATCH_NO) goto syntax;          
          
    if (g95_pure(NULL) && g95_impure_variable(tail->expr->symbol)) {          
      g95_error("Illegal deallocate-expression in DEALLOCATE at %C for a PURE "        
		"procedure"); 
      goto cleanup;   
    }   
   
    if (g95_match_char(',') != MATCH_YES) break;   
   
    i = g95_match(" stat = %v", &stat);      
    if (i == MATCH_ERROR) goto cleanup;          
    if (i == MATCH_YES) break;   
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
  new_st.ext.alloc_list = h; 
 
  return MATCH_YES;   
   
syntax:        
  g95_syntax_error(ST_DEALLOCATE);         
         
cleanup:
  g95_free_expr(stat);       
  g95_free_alloc_list(h); 
  return MATCH_ERROR;   
}   
   
   
      
      
/* g95_match_iterator()-- Match a loop control phrase:
 *
 *     <LVALUE> = <EXPR>, <EXPR> [, <EXPR> ]
 *
 * If the final integer expression is not present, a constant unity
 * expression is returned.  We don't return MATCH_ERROR until after
 * the equals sign is seen. */  
  
match g95_match_iterator(g95_iterator *i, int init_flag) {     
char n[G95_MAX_SYMBOL_LEN+1];          
g95_expr *var0, *a, *l, *e3;       
locus st;   
match t;     
     
  /* Match the start of an iterator without affecting the symbol table */          
          
  st = *g95_current_locus();             
  t = g95_match(" %n =", n);    
  g95_set_locus(&st);          
          
  if (t != MATCH_YES) return MATCH_NO; 
 
  t = g95_match_variable(&var0, 0);        
  if (t != MATCH_YES) return MATCH_NO;          
          
  g95_match_char('='); 
 
  a = l = e3 = NULL;   
   
  if (var0->ref != NULL) {  
    g95_error("Loop variable at %C cannot be a sub-component");   
    goto cleanup;         
  }         
         
  if (var0->symbol->attr.intent == INTENT_IN) {         
    g95_error("Loop variable '%s' at %C cannot be INTENT(IN)",         
	      var0->symbol->name);       
    goto cleanup;         
  }      
      
  t = init_flag ? g95_match_init_expr(&a) : g95_match_expr(&a);  
  if (t == MATCH_NO) goto syntax;     
  if (t == MATCH_ERROR) goto cleanup;        
        
  if (g95_match_char(',') != MATCH_YES) goto syntax;      
      
  t = init_flag ? g95_match_init_expr(&l) : g95_match_expr(&l);   
  if (t == MATCH_NO) goto syntax;   
  if (t == MATCH_ERROR) goto cleanup; 
 
  if (g95_match_char(',') != MATCH_YES) {  
    e3 = g95_int_expr(1);      
    goto done;    
  }   
   
  t = init_flag ? g95_match_init_expr(&e3) : g95_match_expr(&e3);         
  if (t == MATCH_ERROR) goto cleanup;      
  if (t == MATCH_NO) {  
    g95_error("Expected a step value in iterator at %C");         
    goto cleanup;      
  }       
       
done:          
  i->var = var0; 
  i->start = a;      
  i->end = l;    
  i->step = e3;          
  return MATCH_YES;        
        
syntax:
  g95_error("Syntax error in iterator at %C");   
      
cleanup: 
  g95_free_expr(a);  
  g95_free_expr(l);     
  g95_free_expr(e3);       
       
  return MATCH_ERROR;     
}     
     
     
          
          
/* g95_match_stop()-- Match the STOP statement.  We can't match a
 * label here because labels can't be zero and a stop code can. */    
    
match g95_match_stop(void) { 
int stop_code;         
g95_expr *k;    
match p;     
     
  stop_code = -1;   /* blank stop */     
  k = NULL;        
        
  if (g95_match_eos() != MATCH_YES) {     
    p = g95_match_small_literal_int(&stop_code);          
    if (p == MATCH_ERROR) goto cleanup;      
      
    if (p == MATCH_YES && stop_code > 99999) {        
      g95_error("STOP code out of range at %C");
      goto cleanup;       
    }         
         
    if (p == MATCH_NO) {  /* Try a character constant */          
      p = g95_match_expr(&k);
      if (p == MATCH_ERROR) goto cleanup;     
      if (p == MATCH_NO) goto syntax;   
   
      if (k->ts.type != BT_CHARACTER || k->type != EXPR_CONSTANT)  
	goto syntax;         
    }  
  
    if (g95_match_eos() != MATCH_YES) goto syntax;  
  }          
          
  if (g95_pure(NULL)) {    
    g95_error("STOP statement not allowed in PURE procedure at %C");         
    goto cleanup;  
  }     
     
  new_st.type = EXEC_STOP; 
  new_st.expr = k;     
  new_st.ext.stop_code = stop_code; 
 
  return MATCH_YES;  
  
syntax:      
  g95_syntax_error(ST_STOP);  
  
cleanup:       
  g95_free_expr(k);        
  return MATCH_ERROR;        
}        
        
        
       
       
/* free_variable()-- Free a g95_data_variable structure and everything
 * beneath it */          
          
static void free_variable(g95_data_variable *x) {         
g95_data_variable *d;       
       
  for(; x; x=d) {  
    d = x->next; 
    g95_free_expr(x->expr);  
    g95_free_iterator(&x->iter, 0);  
    free_variable(x->list);          
              
    g95_free(x);     
  }          
} 
 
 
       
       
/* g95_match_nullify()-- Match a NULLIFY statement. A NULLIFY
 * statement is transformed into a set of pointer assignments to
 * intrinsic NULL(). */          
          
match g95_match_nullify(void) { 
g95_code *tail;     
g95_expr *w, *x;        
match i; 
 
  tail = NULL;      
      
  if (g95_match_char('(') != MATCH_YES) goto syntax;        
        
  for(;;) {  
    i = g95_match_variable(&x, 0);     
    if (i == MATCH_ERROR) goto cleanup;      
    if (i == MATCH_NO) goto syntax;     
     
    if (g95_pure(NULL) && g95_impure_variable(x->symbol)) {        
      g95_error("Illegal variable in NULLIFY at %C for a PURE procedure");  
      goto cleanup;   
    }         
         
    /* build ' => NULL() ' */
    w = g95_get_expr();          
    w->where = *g95_current_locus();  
    w->type = EXPR_NULL;   
    w->ts.type = BT_UNKNOWN;

    /* Chain to list */      
    if (tail == NULL)  
      tail = &new_st;    
    else {    
      tail->next = g95_get_code();
      tail = tail->next;
    } 
 
    tail->type = EXEC_POINTER_ASSIGN;   
    tail->expr = x; 
    tail->expr2 = w;   
   
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
  
  
          
          
/* g95_match_namelist()-- Match a NAMELIST statement */   
   
match g95_match_namelist(void) { 
g95_symbol *group_name, *s;
g95_namelist *nl;
match c, w;   
   
  c = g95_match(" / %s /", &group_name);  
  if (c == MATCH_NO) goto syntax;         
  if (c == MATCH_ERROR) goto error;     
     
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
      c = g95_match_symbol(&s, 1); 
      if (c == MATCH_NO) goto syntax;      
      if (c == MATCH_ERROR) goto error;       
       
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
      
      c = g95_match_char(',');         
         
      if (g95_match_char('/') == MATCH_YES) {     
	w = g95_match(" %s /", &group_name);      
	if (w == MATCH_YES) break;    
	if (w == MATCH_ERROR) goto error;        
	goto syntax;      
      }

      if (c != MATCH_YES) goto syntax;        
    }
  } 
 
done:         
  return MATCH_YES;   
   
syntax:      
  g95_syntax_error(ST_NAMELIST);      
      
error:         
  return MATCH_ERROR;        
}        
        
        
         
         
/* g95_match_return()-- Match a RETURN statement */       
       
match g95_match_return(void) {    
g95_expr *q;         
match v;       
       
  q = NULL;    
  if (g95_match_eos() == MATCH_YES) goto done;        
        
  if (g95_find_state(COMP_SUBROUTINE) == FAILURE) {          
    g95_error("Alternate RETURN statement at %C is only allowed within "    
	      "a SUBROUTINE");      
    goto cleanup;          
  }    
    
  v = g95_match("% %e%t", &q);
  if (v == MATCH_YES) goto done;          
  if (v == MATCH_ERROR) goto cleanup;         
         
  g95_syntax_error(ST_RETURN);       
       
cleanup:   
  g95_free_expr(q);     
  return MATCH_ERROR;      
      
done:      
  new_st.type = EXEC_RETURN;     
  new_st.expr = q;      
      
  return MATCH_YES;       
}   
   
   
  
  
/* g95_match_call()-- Match a CALL statement.  The tricky part here
 * are possible alternate return specifiers.  We handle these by
 * having all "subroutines" actually return an integer via a register
 * that gives the return number.  If the call specifies alternate
 * returns, we generate code for a SELECT statement whose case clauses
 * contain GOTOs to the various labels. */     
     
match g95_match_call(void) {         
char n[G95_MAX_SYMBOL_LEN+1]; 
g95_actual_arglist *g, *arglist;      
g95_symbol *sy;      
match q;        
int w;          
          
  arglist = NULL;         
         
  q = g95_match("% %n", n);       
  if (q == MATCH_NO) goto syntax;      
  if (q != MATCH_YES) return q;   
   
  if (g95_get_ha_symbol(n, &sy)) return MATCH_ERROR;

  if (!sy->attr.generic && !sy->attr.subroutine &&        
      g95_add_subroutine(&sy->attr, NULL) == FAILURE)         
    return MATCH_ERROR;    
    
  if (g95_match_eos() != MATCH_YES) {      
    q = g95_match_actual_arglist(1, &arglist);    
    if (q == MATCH_NO) goto syntax;   
    if (q == MATCH_ERROR) goto cleanup;  
  
    if (g95_match_eos() != MATCH_YES) goto syntax;    
  }     
     
  w = 0;         
  for(g=arglist; g; g=g->next)    
    if (g->type == ALT_RETURN && 
	g95_reference_st_label(g->u.label, ST_LABEL_TARGET) == FAILURE)      
      w = 1;    
        
  if (w) goto cleanup;   
   
  new_st.type = EXEC_CALL;
  new_st.sym = sy;          
  new_st.ext.actual = arglist;   
  return MATCH_YES; 
 
syntax:   
  g95_syntax_error(ST_CALL);

cleanup:      
  g95_free_actual_arglist(arglist);          
  return MATCH_ERROR;    
} 
 
 
      
      
/* g95_match_label()-- Match and validate a label associated with a
 * named IF, DO or SELECT statement.  If the symbol does not have the
 * label attribute, we add it.  We also make sure the symbol does not
 * refer to another (active) block.  A matched label is pointed to by
 * g95_new_block. */        
        
match g95_match_label(void) {       
char name0[G95_MAX_SYMBOL_LEN+1];      
g95_state_data *q;          
match g;        
        
  g95_new_block = NULL;          
          
  g = g95_match(" %n :", name0);    
  if (g != MATCH_YES) return g;         
         
  if (g95_get_symbol(name0, NULL, &g95_new_block)) { 
    g95_error("Label name '%s' at %C is ambiguous", name0);    
    return MATCH_ERROR;         
  }  
  
  if (g95_new_block->attr.flavor != FL_LABEL &&          
      g95_add_flavor(&g95_new_block->attr, FL_LABEL, NULL) == FAILURE)      
    return MATCH_ERROR; 
 
  for(q=g95_state_stack; q; q=q->previous) 
    if (q->sym == g95_new_block) {       
      g95_error("Label %s at %C already in use by a parent block",    
		g95_new_block->name);          
      return MATCH_ERROR;  
    }

  return MATCH_YES;
}        
        
        
 
 
/* g95_match_allocate()-- Match an ALLOCATE statement */         
         
match g95_match_allocate(void) {  
g95_alloc *head, *tail;  
g95_expr *stat;         
match u;         
         
  head = tail = NULL;
  stat = NULL; 
 
  if (g95_match_char('(') != MATCH_YES) goto syntax;         
         
  for(;;) { 
    if (head == NULL)         
      head = tail = g95_get_alloc();      
    else { 
      tail->next = g95_get_alloc();    
      tail = tail->next;          
    }    
    
    u = g95_match_variable(&tail->expr, 0);          
    if (u == MATCH_NO) goto syntax;  
    if (u == MATCH_ERROR) goto cleanup;   
   
    if (g95_pure(NULL) && g95_impure_variable(tail->expr->symbol)) {       
      g95_error("Bad allocate-object in ALLOCATE statement at %C for a " 
		"PURE procedure");       
      goto cleanup;      
    }   
   
    if (g95_match_char(',') != MATCH_YES) break;     
     
    u = g95_match(" stat = %v", &stat);  
    if (u == MATCH_ERROR) goto cleanup;       
    if (u == MATCH_YES) break; 
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
  new_st.ext.alloc_list = head;       
       
  return MATCH_YES;       
       
syntax:      
  g95_syntax_error(ST_ALLOCATE);       
       
cleanup:          
  g95_free_expr(stat);        
  g95_free_alloc_list(head);    
  return MATCH_ERROR;
}  
  
  
      
      
/* g95_match_else()-- Match an ELSE statement */    
    
match g95_match_else(void) {         
char nm[G95_MAX_SYMBOL_LEN+1];          
           
  if (g95_match_eos() == MATCH_YES) return MATCH_YES;     
     
  if (g95_match_name(nm) != MATCH_YES || g95_current_block() == NULL ||
      g95_match_eos() != MATCH_YES) {  
    g95_error("Unexpected junk after ELSE statement at %C");      
    return MATCH_ERROR;         
  }          
          
  if (strcmp(nm, g95_current_block()->name) != 0) {          
    g95_error("Label '%s' at %C doesn't match IF label '%s'",    
	      nm, g95_current_block()->name);   
    return MATCH_ERROR;
  }

  return MATCH_YES;       
}         
         
         
  
  
/* g95_free_namelist()-- Free a namelist structure */         
         
void g95_free_namelist(g95_namelist *name0) {         
g95_namelist *g;      
      
  for(;name0; name0=g) {     
    g = name0->next;        
    g95_free(name0);
  }        
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
  
  


/* g95_match_continue()-- match a CONTINUE statement */     
     
match g95_match_continue(void) {      
      
  if (g95_match_eos() != MATCH_YES) {     
    g95_syntax_error(ST_CONTINUE);   
    return MATCH_ERROR; 
  }     
     
  new_st.type = EXEC_CONTINUE;       
  return MATCH_YES;    
}   
   
   
   
   
/* g95_match_name()-- See if the current input looks like a name of
 * some sort.  Modifies the passed buffer which must be
 * G95_MAX_SYMBOL_LEN+1 bytes long. */ 
 
match g95_match_name(char *buffer) {        
locus where;     
int a, d;     
     
  where = *g95_current_locus();   
  g95_gobble_whitespace();     
     
  d = g95_next_char();   
  if (!isalpha(d)) {  
    g95_set_locus(&where);     
    return MATCH_NO;          
  } 
 
  a = 0;

  do {        
    buffer[a++] = d;  
  
    if (a > G95_MAX_SYMBOL_LEN) {     
      g95_error("Name at %C is too long");        
      return MATCH_ERROR; 
    }      
      
    where = *g95_current_locus();  
    d = g95_next_char();     
  } while(isalnum(d) || d == '_' || (g95_option.dollar && d == '$'));     
     
  buffer[a] = '\0';        
  g95_set_locus(&where);       
       
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
        
int g95_match_strings(mstring *n) {         
int no_match, l, possibles;          
mstring *t, *best_match;         
locus match_loc;  
  
  possibles = 0;      
      
  for(t=n; t->string != NULL; t++) {   
    t->mp = t->string;      
    possibles++;        
  } 
 
  no_match = t->tag;          
          
  best_match = NULL;        
  match_loc = *g95_current_locus();        
        
  g95_gobble_whitespace();    
    
  while(possibles > 0) {       
    l = g95_next_char();         
         
/* Apply the next character to the current possibilities */     
     
    for(t=n; t->string!=NULL; t++) {    
      if (t->mp == NULL) continue;          
          
      if (*t->mp == ' ') {    /* Space matches 1+ whitespace(s) */       
       
	if ((g95_current_file->form == FORM_FREE) &&      
	      g95_is_whitespace(l)) continue;   
   
	t->mp++;          
      }         
         
      if (*t->mp != l) {      /* Match failed */      
	t->mp = NULL; 
	possibles--;
	continue;       
      }          
          
      t->mp++;      
      if (*t->mp == '\0') {   /* Found a match */  
	match_loc = *g95_current_locus();
	best_match = t;       
	possibles--;   
	t->mp = NULL;   
      }         
    } 
  }   
   
  g95_set_locus(&match_loc);    
    
  return (best_match == NULL) ? no_match : best_match->tag; 
}       
       
       


/* stf_pre_match()-- See if the thing on the input sort of looks like a
 * statement function.  This is:  WORD ( [ WORD [, WORD ] ... ] ) =
 * The idea is to eliminate things that can't possibly be statement
 * functions without messing with the symbol table.  We never return
 * error. */         
         
static match stf_pre_match(void) {    
char name0[G95_MAX_SYMBOL_LEN+1];
locus pos;     
match b;   
char o;        
        
  pos = *g95_current_locus();   
   
  b = MATCH_NO;  
  if (g95_match_name(name0) != MATCH_YES) goto done;          
  if (g95_match_char('(') != MATCH_YES) goto done;  
  
  g95_gobble_whitespace();     
  if (g95_peek_char() == ')')
    g95_next_char();      
  else {          
    for(;;) {
      if (g95_match_name(name0) != MATCH_YES) goto done;   
   
      g95_gobble_whitespace();          
      o = g95_next_char();           
          
      if (o == ')') break;  
      if (o != ',') goto done;  
    }         
  }  
  
  b = g95_match_char('=');       
       
done:  
  g95_set_locus(&pos);  
  return b;       
}       
       
       
      
      
/* g95_match_module()-- Match a MODULE statement */   
   
match g95_match_module(void) {    
match h;  
  
  h = g95_match(" %s%t", &g95_new_block);        
  if (h != MATCH_YES) return h;   
   
  if (g95_add_flavor(&g95_new_block->attr, FL_MODULE, NULL) == FAILURE)
    return MATCH_ERROR;    
    
  return MATCH_YES;     
}       
       
       
    
    
/* var_list()-- Match a list of variables terminated by an iterator
 * and a right paren. */       
       
static match var_list(g95_data_variable *parent) {
g95_data_variable *end, variable;    
match m;

  m = var_element(&variable);        
  if (m == MATCH_ERROR) return MATCH_ERROR;    
  if (m == MATCH_NO) goto syntax;  
  
  end = g95_get_data_variable();     
  *end = variable;

  parent->list = end;          
          
  for(;;) {          
    if (g95_match_char(',') != MATCH_YES) goto syntax;   
   
    m = g95_match_iterator(&parent->iter, 0);     
    if (m == MATCH_YES) break; 
    if (m == MATCH_ERROR) return MATCH_ERROR;

    m = var_element(&variable);       
    if (m == MATCH_ERROR) return MATCH_ERROR;     
    if (m == MATCH_NO) goto syntax;         
         
    end->next = g95_get_data_variable();   
    end = end->next;       
       
    *end = variable;  
  }  
  
  if (g95_match_char(')') != MATCH_YES) goto syntax;
  return MATCH_YES;  
  
syntax:
  g95_syntax_error(ST_DATA); 
  return MATCH_ERROR;      
}        
        
        


/* g95_match_small_literal_int()-- Match a literal integer on the
 * input, setting the value on MATCH_YES.  Literal integers occur in
 * kind-parameters as well as old-style character length
 * specifications. */      
      
match g95_match_small_literal_int(int *value) {         
locus o;       
char l;   
int u;        
        
  o = *g95_current_locus();

  g95_gobble_whitespace();         
  l = g95_next_char();         
         
  if (!isdigit(l)) {   
    g95_set_locus(&o);
    return MATCH_NO;       
  }    
    
  u = l - '0'; 
 
  for(;;) {         
    o = *g95_current_locus();         
    l = g95_next_char();

    if (!isdigit(l)) break;  
  
    u = 10*u + l - '0';    
    
    if (u > 99999999) {     
      g95_error("Integer too large at %C");    
      return MATCH_ERROR;     
    }  
  }      
      
  g95_set_locus(&o);          
          
  *value = u;  
  return MATCH_YES; 
}


 
 
/* g95_match_assignment(void)-- Match a simple assignment statement */    
    
match g95_match_assignment(void) {    
g95_expr *lvalue, *rvalue; 
locus oldl;       
match c;        
        
  oldl = *g95_current_locus();         
        
  lvalue = rvalue = NULL;  
  c = g95_match(" %v =", &lvalue);       
  if (c != MATCH_YES) goto cleanup;  
  
  if (lvalue->symbol->attr.flavor == FL_PARAMETER) {         
    g95_error("Cannot assign to a PARAMETER variable at %C");
    c = MATCH_ERROR;        
    goto cleanup;  
  }       
       
  c = g95_match(" %e%t", &rvalue);  
  if (c != MATCH_YES) goto cleanup;

  new_st.type = EXEC_ASSIGN;          
  new_st.expr = lvalue; 
  new_st.expr2 = rvalue;       
       
  g95_check_do_variable(lvalue->symbol);  
  
  return MATCH_YES;     
     
cleanup:  
  g95_set_locus(&oldl); 
  g95_free_expr(lvalue);          
  g95_free_expr(rvalue);         
  return c;    
}        
        
        
       
       
/* free_value()-- Free a g95_data_value structure and everything
 * beneath it */    
    
static void free_value(g95_data_value *z) {    
g95_data_value *w;       
       
  for(; z; z=w) { 
    w = z->next;        
    g95_free_expr(z->expr);       
    g95_free(z);   
  }          
}          
          
          


/* g95_match_pause()-- Match the (deprecated) PAUSE statement */     
     
match g95_match_pause(void) {   
g95_expr *e2;
match k;         
         
  e2 = NULL;   
  if (g95_match_eos() == MATCH_YES) goto got_match;       
       
  k = g95_match(" %e%t", &e2);     
  if (k == MATCH_YES) goto got_match;         
  if (k == MATCH_NO) g95_syntax_error(ST_PAUSE);      
  return MATCH_ERROR;   
   
got_match:      
  new_st.type = EXEC_PAUSE;   
  new_st.expr = e2;  
  return MATCH_YES;  
}  
  
  
      
      
/* g95_free_forall_iterator()-- Free a list of FORALL iterators */       
       
void g95_free_forall_iterator(g95_forall_iterator *iterator) {  
g95_forall_iterator *next;   
   
  while(iterator) {        
    next = iterator->next;        
        
    g95_free_expr(iterator->var); 
    g95_free_expr(iterator->start);       
    g95_free_expr(iterator->end);       
    g95_free_expr(iterator->stride);    
    
    g95_free(iterator);   
    iterator = next;        
  }    
}


        
        
/* g95_match_where()-- Match a WHERE statement */         
         
match g95_match_where(g95_statement *s) {      
g95_expr *e2;    
match f, p;    
g95_code *a;  
  
  f = g95_match_label();
  if (f == MATCH_ERROR) return f;     
     
  p = g95_match(" where ( %e )", &e2); 
  if (p != MATCH_YES) return p;      
      
  if (g95_match_eos() == MATCH_YES) {          
    *s = ST_WHERE_BLOCK;       
       
    new_st.type = EXEC_WHERE;          
    new_st.expr = e2;      
    return MATCH_YES;    
  }  
  
  p = g95_match_assignment();
  if (p == MATCH_NO) g95_syntax_error(ST_WHERE);      
      
  if (p != MATCH_YES) {     
    g95_free_expr(e2);     
    return MATCH_ERROR;       
  } 
 
/* We've got a simple WHERE statement */         
         
  *s = ST_WHERE;        
  a = g95_get_code();      
      
  a->type = EXEC_WHERE;   
  a->expr = e2; 
  a->next = g95_get_code(); 
 
  *a->next = new_st;
  g95_clear_new_st();     
     
  new_st.type = EXEC_WHERE;
  new_st.block = a;

  return MATCH_YES;    
}     
     
     
     
     
/* g95_free_data()-- Free a list of g95_data structures */      
      
void g95_free_data(g95_data *i) { 
g95_data *b;    
    
  for(; i; i=b) {          
    b = i->next;         
         
    free_variable(i->var);       
    free_value(i->value);

    g95_free(i);      
  }      
}        
        
        
      
      
/* match_arithmetic_if()-- Match an arithmetic IF statement that
 * happens to follow a simple IF statement. */   
   
static match match_arithmetic_if(void) {         
g95_st_label *i, *g, *l3;      
g95_expr *v;  
match s;     
     
  s = g95_match(" ( %e ) %l , %l , %l %t", &v, &i, &g, &l3);   
  if (s != MATCH_YES) return s;       
       
  if (g95_reference_st_label(i, ST_LABEL_TARGET) == FAILURE ||   
      g95_reference_st_label(g, ST_LABEL_TARGET) == FAILURE ||
      g95_reference_st_label(l3, ST_LABEL_TARGET) == FAILURE) {      
    g95_free_expr(v);          
    return MATCH_ERROR;          
  }       
       
  new_st.type = EXEC_ARITHMETIC_IF;  
  new_st.expr = v;
  new_st.label  = i;
  new_st.label2 = g;   
  new_st.label3 = l3;   
   
  return MATCH_YES;   
}       
       
       
 
 
/* g95_match_pointer_assignment()-- Match a pointer assignment statement */      
      
match g95_match_pointer_assignment(void) {   
g95_expr *lvalue, *rvalue;     
locus old_loc;         
match p;    
    
  old_loc = *g95_current_locus();        
        
  lvalue = rvalue = NULL;

  p = g95_match(" %v =>", &lvalue);      
  if (p != MATCH_YES) { p = MATCH_NO; goto cleanup; }     
     
  if (lvalue->symbol->attr.flavor == FL_PARAMETER) {         
    g95_error("Cannot assign to a PARAMETER variable at %C");          
    p = MATCH_ERROR; 
    goto cleanup;          
  }     
     
  p = g95_match(" %e%t", &rvalue);          
  if (p != MATCH_YES) goto cleanup;       
       
  new_st.type = EXEC_POINTER_ASSIGN; 
  new_st.expr = lvalue; 
  new_st.expr2 = rvalue;  
  
  return MATCH_YES;       
       
cleanup:     
  g95_set_locus(&old_loc);  
  g95_free_expr(lvalue);        
  g95_free_expr(rvalue);        
  return p;   
}  
  
  
      
      
/* g95_get_common()-- Given a name, return a pointer to the common
 * head structure, creating it if it does not exist. */    
    
g95_common_head *g95_get_common(char *name0) {
g95_symtree *s;          
          
  s = g95_find_symtree(g95_current_ns->common_root, name0);         
  if (s == NULL) s = g95_new_symtree(&g95_current_ns->common_root, name0);          
          
  if (s->n.common == NULL) { 
    s->n.common = g95_get_common_head();          
    s->n.common->where = *g95_current_locus();       
  } 
 
  return s->n.common;  
}     
     
     
 
 
/* g95_match_intrinsic_op()-- Match an intrinsic operator.  Returns an
 * INTRINSIC enum. */    
    
match g95_match_intrinsic_op(g95_intrinsic_op *r) {     
g95_intrinsic_op operand;          
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
   
  operand = g95_match_strings(operators_in);     
  if (operand == INTRINSIC_NONE) return MATCH_NO; 
 
  *r = operand;        
  return MATCH_YES;    
}       
       
       
char *g95_op2string(int z) {     
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
 
  if (z == INTRINSIC_ASSIGN) return "=";          
  return g95_code2string(operators_out, z);  
}    
    
    
  
  
/* g95_match_eos()-- Match an end of statement.  End of statement is
 * optional whitespace, followed by a ';' or '\n' or comment '!'.  If
 * a semicolon is found, we continue to eat whitespace and semicolons. */     
     
match g95_match_eos(void) {    
locus old_loc;   
int flag, u;        
        
  flag = 0;  
  
  for(;;) {     
    old_loc = *g95_current_locus(); 
    g95_gobble_whitespace();
   
    u = g95_next_char();       
    switch(u) {    
    case '!': 
      do {   
	u = g95_next_char();      
      } while(u != '\n');  
  
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
        
        
    
    
/* g95_match_do()-- Match a DO statement */      
      
match g95_match_do(void) {   
g95_expr *while_condition;    
g95_iterator iter, *interp;        
g95_st_label *labl;          
locus o; 
match l;        
        
  o = *g95_current_locus();

  labl = NULL; 
  iter.var = iter.start = iter.end = iter.step = NULL;        
  while_condition = NULL;      
      
  l = g95_match_label();       
  if (l == MATCH_ERROR) return l;       
       
  if (g95_match(" do") != MATCH_YES) return MATCH_NO;  
  
/* Match an infinite DO, make it like a DO WHILE(.TRUE.) */     
     
  if (g95_match_eos() == MATCH_YES) {  
    new_st.type = EXEC_DO_WHILE;          
    goto done;          
  }         
         
  l = g95_match_st_label(&labl, 0);      
  if (l == MATCH_ERROR) goto cleanup;    
    
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
   
  g95_match_st_label(&labl, 0);  /* Can't error out */       
  g95_match_char(',');            /* Optional comma */ 
 
  l = g95_match_iterator(&iter, 0);        
  if (l == MATCH_NO) return MATCH_NO;        
  if (l == MATCH_ERROR) goto cleanup;       
       
  g95_check_do_variable(iter.var->symbol);    
    
  if (g95_match_eos() != MATCH_YES) {          
    g95_syntax_error(ST_DO); 
    goto cleanup;   
  }      
      
  new_st.type = EXEC_DO;    
    
done:        
  if (labl != NULL &&   
      g95_reference_st_label(labl, ST_LABEL_TARGET) == FAILURE)   
    goto cleanup; 
 
  new_st.label = labl;     
     
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
        
        
    
    
/* top_val_list()-- Match a list of values in a DATA statement.  The
 * leading '/' has already been seen at this point. */  
  
static match top_val_list(g95_data *data) {        
g95_data_value *n1, *tail;    
g95_expr *expr;  
char *message; 
match s;

  tail = NULL;       
       
  for(;;) {    
    s = match_data_constant(&expr);
    if (s == MATCH_NO) goto syntax;      
    if (s == MATCH_ERROR) return MATCH_ERROR;

    n1 = g95_get_data_value();  
  
    if (tail == NULL)  
      data->value = n1;          
    else  
      tail->next = n1;        
        
    tail = n1; 
 
    if (expr->ts.type != BT_INTEGER || g95_match_char('*') != MATCH_YES) {          
      tail->expr = expr;        
      tail->repeat = 1;        
    } else {   
      message = g95_extract_int(expr, &tail->repeat);         
      g95_free_expr(expr);  
      if (message != NULL) {     
	g95_error(message);       
	return MATCH_ERROR;
      }   
   
      s = match_data_constant(&tail->expr);        
      if (s == MATCH_NO) goto syntax;      
      if (s == MATCH_ERROR) return MATCH_ERROR;
    } 
 
    if (g95_option.verbose) {  
      g95_status("DATA element:  %d * ", tail->repeat);        
      g95_show_expr(tail->expr);     
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




/* var_element()-- Match a single element in a data variable list,
 * which can be a variable-iterator list. */   
   
static match var_element(g95_data_variable *old) {       
match r;     
     
  memset(old, '\0', sizeof(g95_data_variable));

  if (g95_match_char('(') == MATCH_YES) return var_list(old);   
   
  r = g95_match_variable(&old->expr, 1);
  if (r != MATCH_YES) return r;     
     
  if (old->expr->symbol->value != NULL) {       
    g95_error("Variable '%s' at %C already has an initialization",  
	      old->expr->symbol->name);         
    return MATCH_ERROR;  
  }   
   
  old->expr->symbol->attr.data = 1;    
  return MATCH_YES;          
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
int matches, *ip;          
locus oldl;    
va_list argps;       
char z, *np;    
match o, d;        
void **vp; 
char *h;     
     
  oldl = *g95_current_locus();  
  va_start(argps, target);      
  o = MATCH_NO;    
  matches = 0;         
  h = target;      
      
loop:
  z = *h++;        
  switch(z) {  
  case ' ':   g95_gobble_whitespace(); goto loop;    
  case '\0':  o = MATCH_YES; break;      
      
  case '%': 
    z = *h++;     
    switch(z) {   
    case 'e':          
      vp = va_arg(argps, void **);       
      d = g95_match_expr((g95_expr **) vp);          
      if (d != MATCH_YES) { o = d; goto not_yes; }    
    
      matches++;   
      goto loop;   
   
    case 'v':          
      vp = va_arg(argps, void **);
      d = g95_match_variable((g95_expr **) vp, 0);       
      if (d != MATCH_YES) { o = d; goto not_yes; }         
         
      matches++;
      goto loop;     
     
    case 's':   
      vp = va_arg(argps, void **); 
      d = g95_match_symbol((g95_symbol **) vp, 0);  
      if (d != MATCH_YES) { o = d; goto not_yes; }       
       
      matches++;         
      goto loop;      
      
    case 'n':      
      np = va_arg(argps, char *);
      d = g95_match_name(np);          
      if (d != MATCH_YES) { o = d; goto not_yes; }   
   
      matches++; 
      goto loop;    
    
    case 'l':  
      lab = va_arg(argps, g95_st_label **); 
      d = g95_match_st_label(lab, 0);   
      if (d != MATCH_YES) { o = d; goto not_yes; }      
      
      matches++;  
      goto loop;       
       
    case 'o': 
      ip = va_arg(argps, int *);     
      d = g95_match_intrinsic_op((g95_intrinsic_op *) ip);   
      if (d != MATCH_YES) { o = d; goto not_yes; }   
   
      matches++;         
      goto loop;   
   
    case 't':       
      if (g95_match_eos() != MATCH_YES) { o = MATCH_NO; goto not_yes; }  
      goto loop;          
          
    case ' ':        
      if (g95_match_space() == MATCH_YES) goto loop; 
      o = MATCH_NO;       
      goto not_yes;      
      
    case '%': break;  /* Fall through to character matcher */ 
 
    default:      
      g95_internal_error("g95_match(): Bad match code %c", z);      
    }          
          
  default:       
    if (z == g95_next_char()) goto loop;      
    break;
  }       
       
not_yes: 
  va_end(argps);      
      
  if (o != MATCH_YES) {   /* Clean up after a failed match */    
    g95_set_locus(&oldl);        
    va_start(argps, target);        
        
    h = target;      
    for(; matches>0; matches--) {          
      while(*h++ != '%');        
        
      switch(*h++) {    
      case '%': matches++; break;   /* Skip */  
  
      case 'I': case 'L': case 'C':    
	if (*h++ == 'e') goto undo_expr;        
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
         
  return o;       
}  
  
  


/* top_var_list()-- Match the top-level list of data variables */         
         
static match top_var_list(g95_data *k) {       
g95_data_variable variable, *end, *n;        
match h;        
        
  end = NULL;

  for(;;) {   
    h = var_element(&variable);        
    if (h == MATCH_NO) goto syntax;   
    if (h == MATCH_ERROR) return MATCH_ERROR;   
   
    n = g95_get_data_variable(); 
    *n = variable; 
 
    if (end == NULL)   
      k->var = n;          
    else     
      end->next = n;     
     
    end = n;  
  
    if (g95_match_char('/') == MATCH_YES) break;      
    if (g95_match_char(',') != MATCH_YES) goto syntax;     
  }     
     
  return MATCH_YES;   
   
syntax:     
  g95_syntax_error(ST_DATA);        
  return MATCH_ERROR;         
}  
  
  
     
     
/* g95_match_st_function()-- Match a statement function declaration.
 * It is so easy to match non-statement function statements with a
 * MATCH_ERROR as opposed to MATCH_NO that we suppress error message
 * in most cases. */ 
 
match g95_match_st_function(void) {      
g95_error_buf old_error;
g95_expr *p, *exp;    
g95_namespace *ns;    
g95_symbol *s;         
g95_symtree *st;         
g95_code *d;     
match v;     
     
  if (stf_pre_match() == MATCH_NO) return MATCH_NO;       
       
  v = g95_match_symbol(&s, 0);     
  if (v != MATCH_YES) return v;

  g95_push_error(&old_error);    
    
  if (g95_add_procedure(&s->attr, PROC_ST_FUNCTION, NULL) == FAILURE)    
    goto undo_error; 
 
  if (g95_match_formal_arglist(s, 1, 0) != MATCH_YES) goto undo_error;  
  
  v = g95_match(" = %e%t", &exp);        
  if (v == MATCH_NO) goto undo_error;          
  if (v == MATCH_ERROR) return v; 
 
  /* At this point, we have a statement function.  These are
   * implemented by turning them into the equivalent contained
   * procedure.  The dummy arguments are copied into the contained
   * space during resolution. */     
     
  s->result = s;

  p = g95_get_expr();
  p->type = EXPR_VARIABLE;          
  p->symbol = s;          
  p->where = *g95_current_locus();        
        
  d = g95_get_code();    
  d->type = EXEC_ASSIGN;          
          
  d->expr = p;      
  d->expr2 = exp;      
      
  ns = g95_get_namespace(g95_current_ns);       
  ns->proc_name = s;   
  ns->code = d;
  ns->parent = g95_current_ns;     
     
  ns->sibling = g95_current_ns->contained;
  g95_current_ns->contained = ns;        
        
  st = g95_new_symtree(&ns->sym_root, s->name);       
  st->n.sym = s;       
       
  s->refs++;    
    
  return MATCH_YES;  
  
undo_error:    
  g95_pop_error(&old_error); 
  return MATCH_NO;  
}         
         
         


/* g95_match_elseif()-- Match an ELSE IF statement */   
   
match g95_match_elseif(void) {  
char nm[G95_MAX_SYMBOL_LEN+1];  
g95_expr *e1;         
match u;       
       
  u = g95_match(" ( %e ) then", &e1);      
  if (u != MATCH_YES) return u;  
  
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
g95_st_label *d, *q, *l3;        
match mm, a, c;  
g95_expr *exp; 
locus old;          
g95_code *g;    
    
  c = g95_match_label();    
  if (c == MATCH_ERROR) return c; 
 
  old = *g95_current_locus();       
       
  a = g95_match(" if ( %e", &exp);     
  if (a != MATCH_YES) return a;          
          
  if (g95_match_char(')') != MATCH_YES) {
    g95_error("Syntax error in IF-expression at %C");      
    g95_free_expr(exp);  
    return MATCH_ERROR;    
  } 
 
  a = g95_match(" %l , %l , %l%t", &d, &q, &l3);   
   
  if (a == MATCH_YES) {     
    if (c == MATCH_YES) {  
      g95_error("Block label not appropriate for arithmetic IF statement "    
		"at %C");      
      
      g95_free_expr(exp);     
      return MATCH_ERROR;         
    }        
        
    if (g95_reference_st_label(d, ST_LABEL_TARGET) == FAILURE ||     
	g95_reference_st_label(q, ST_LABEL_TARGET) == FAILURE ||        
	g95_reference_st_label(l3, ST_LABEL_TARGET) == FAILURE) {   
      g95_free_expr(exp);      
      return MATCH_ERROR;    
    }          
          
    new_st.type = EXEC_ARITHMETIC_IF;       
    new_st.expr = exp;         
    new_st.label  = d;     
    new_st.label2 = q;      
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

  if (c == MATCH_YES) {  
    g95_error("Block label is not appropriate for the IF statement at %C");

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
  g95_set_locus(&old);   
   
  g95_match(" if ( %e ) ", &exp);  /* Guaranteed to match */ 
 
  mm = g95_match_pointer_assignment();  
  if (mm == MATCH_YES) goto got_match;       
       
  g95_free_expr(exp);    
  g95_undo_symbols();     
  g95_set_locus(&old);    
    
  g95_match(" if ( %e ) ", &exp);  /* Guaranteed to match */   
   
/* Look at the next word to see which matcher to call.  Matching the
 * keyword doesn't affect the symbol table, so we don't have to
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
        
  g = g95_get_code();      
  *g = new_st; 
  g->where = *g95_current_locus();   
   
  g95_clear_new_st();    
    
  new_st.type = EXEC_IF;         
  new_st.block = g;       
  new_st.expr = exp;      
      
  return MATCH_YES;          
}         
         
#undef match
    
    
  
  
/* g95_match_exit()-- Match the EXIT statement */  
  
match g95_match_exit(void) {    
    
  return match_exit_cycle(ST_EXIT,  EXEC_EXIT);
}   
   
   
  
  
/* g95_match_goto()-- Match the GO TO statement.  As a computed GOTO
 * statement is matched, it is transformed into an equivalent SELECT
 * block.  No tree is necessary, and the resulting jumps-to-jumps are
 * specifically optimized away by the back end. */  
  
match g95_match_goto(void) {      
g95_code *start, *end;
g95_expr *expr;         
g95_case *cp;     
g95_st_label *lab;
int x;        
match c;    
    
  if (g95_match(" %l%t", &lab) == MATCH_YES) {
    if (g95_reference_st_label(lab, ST_LABEL_TARGET) == FAILURE)  
      return MATCH_ERROR;       
       
    new_st.type = EXEC_GOTO;        
    new_st.label = lab;        
    return MATCH_YES;   
  }          
          
/* The assigned GO TO statement is not allowed in Fortran 95 */      
      
  if (g95_match_variable(&expr, 0) == MATCH_YES) {      
    g95_free_expr(expr);    
    g95_error("The assigned GO TO statement at %C is not allowed in "
	      "Fortran 95");
    return MATCH_ERROR;         
  } 
 
/* Last chance is a computed GO TO statement */

  if (g95_match_char('(') != MATCH_YES) {          
    g95_syntax_error(ST_GOTO);    
    return MATCH_ERROR;
  }   
   
  start = end = NULL;       
  x = 1;      
      
  do { 
    c = g95_match_st_label(&lab, 0);
    if (c != MATCH_YES) goto syntax;

    if (g95_reference_st_label(lab, ST_LABEL_TARGET) == FAILURE)
      goto cleanup;      
      
    if (start == NULL)       
      start = end = g95_get_code();   
    else {    
      end->block = g95_get_code();   
      end = end->block;    
    }         
         
    cp = g95_get_case();   
    cp->low = cp->high = g95_int_expr(x++);      
      
    end->type = EXEC_SELECT;        
    end->ext.case_list = cp;

    end->next = g95_get_code();     
    end->next->type = EXEC_GOTO;   
    end->next->label = lab;          
  } while(g95_match_char(',') == MATCH_YES);    
    
  if (g95_match_char(')') != MATCH_YES) goto syntax;         
         
  if (start == NULL) {      
    g95_error("Statement label list in GOTO at %C cannot be empty");        
    goto syntax;
  }  
  
/* Get the rest of the statement */     
     
  g95_match_char(',');      
      
  if (g95_match(" %e%t", &expr) != MATCH_YES) goto syntax;         
         
/* At this point, a computed GOTO has been fully matched and an
 * equivalent SELECT statement constructed. */    
    
  new_st.type = EXEC_SELECT;    
  new_st.expr = NULL;     
  /* For a "real" SELECT, the expression is in expr. We put it in expr2. */ 
  new_st.expr2 = expr;    
  new_st.block = start;         
  return MATCH_YES;        
        
syntax:
  g95_syntax_error(ST_GOTO);        
cleanup:   
  g95_free_statements(start);        
  return MATCH_ERROR; 
}   
   
   
  
  
/* g95_free_alloc_list()-- Frees a list of g95_alloc structures */     
     
void g95_free_alloc_list(g95_alloc *y) {         
g95_alloc *e;         
         
  for(; y; y=e) {
    e = y->next;          
    g95_free_expr(y->expr);          
    g95_free(y);     
  }          
}  
  
  
  
  
/* g95_match_data()-- Match a DATA statement */          
          
match g95_match_data(void) { 
g95_data *n;        
match y;       
         
  for(;;) {         
    n = g95_get_data();         
    n->where = *g95_current_locus();  
  
    y = top_var_list(n);    
    if (y != MATCH_YES) goto cleanup;   
   
    y = top_val_list(n);        
    if (y != MATCH_YES) goto cleanup;    
    
    n->next = g95_current_ns->data;     
    g95_current_ns->data = n;       
       
    if (g95_match_eos() == MATCH_YES) break;  
  
    g95_match_char(',');  /* Optional comma */       
  }          
          
  if (g95_pure(NULL)) {  
    g95_error("DATA statement at %C is not allowed in a PURE procedure");     
    return MATCH_ERROR;   
  }     
     
  return MATCH_YES; 
 
cleanup:          
  g95_free_data(n);    
  return MATCH_ERROR;    
}         
         
         
        
        
/* g95_match_char()-- Tries to match the next non-whitespace character
 * on the input.  This subroutine does not return MATCH_ERROR.  */        
        
match g95_match_char(char r) { 
locus old_loc;    
    
  old_loc = *g95_current_locus();    
  g95_gobble_whitespace();          
          
  if (g95_next_char() == r) return MATCH_YES;        
        
  g95_set_locus(&old_loc);       
  return MATCH_NO;        
}   
   
   
  
  
/* match_common_name()-- Match a common block name.  Returns a null
 * string for the blank common. */        
        
static match match_common_name(char *nm) { 
match h;         
         
  if (g95_match_char('/') == MATCH_NO) {
    nm[0] = '\0';  
    return MATCH_YES;          
  }        
        
  if (g95_match_char('/') == MATCH_YES) {        
    nm[0] = '\0';         
    return MATCH_YES;         
  }         
         
  h = g95_match_name(nm); 
 
  if (h == MATCH_ERROR) return MATCH_ERROR;          
  if (h == MATCH_YES && g95_match_char('/') == MATCH_YES) return MATCH_YES;   
   
  g95_error("Syntax error in common block name at %C");        
  return MATCH_ERROR;
}   
   
   
      
      
/* g95_match_common()-- Match a COMMON statement */       
       
match g95_match_common(void) {       
g95_symbol *symb, **head, *t, *old_blank_common;        
char nm[G95_MAX_SYMBOL_LEN+1];   
g95_common_head *e;   
g95_array_spec *as;         
match u;

  old_blank_common = g95_current_ns->blank_common.head;      
  if (old_blank_common != NULL) {          
    while(old_blank_common->common_next != NULL)         
      old_blank_common = old_blank_common->common_next;         
  }

  as = NULL;      
      
  if (g95_match_eos() == MATCH_YES) goto syntax;   
   
  for(;;) { 
    u = match_common_name(nm);    
    if (u == MATCH_ERROR) goto cleanup;          
          
    if (nm[0] == '\0') {   
      e = &g95_current_ns->blank_common;   
      if (e->head == NULL) e->where = *g95_current_locus();      
      head = &e->head;    
    } else {
      e = g95_get_common(nm);    
      head = &e->head;    
    
      if (e->use_assoc) {
	g95_error("COMMON block '%s' at %C has already been USE-associated");  
	goto cleanup;        
      }     
    }        
        
    if (*head == NULL)  
      t = NULL;  
    else {    
      t = *head;        
      while(t->common_next)     
	t = t->common_next;        
    }    
    
/* Grab the list of symbols for this common. */  
  
    for(;;) {      
      u = g95_match_symbol(&symb, 0);         
      if (u == MATCH_ERROR) goto cleanup;  
      if (u == MATCH_NO) goto syntax;

      if (symb->attr.in_common) {          
	g95_error("Symbol '%s' at %C is already in a COMMON block", symb->name);   
	goto cleanup;   
      } 
 
      if (g95_add_in_common(&symb->attr, NULL) == FAILURE) goto cleanup;  
  
      if (t != NULL)          
	t->common_next = symb;        
      else
	*head = symb;      
      
      t = symb;          
          
/* Deal with an optional array specification after the symbol name */   
   
      u = g95_match_array_spec(&as); 
      if (u == MATCH_ERROR) goto cleanup;  
      if (u == MATCH_YES) { 
	if (g95_add_dimension(&symb->attr, NULL) == FAILURE) goto cleanup; 
 
	symb->as = as;        
	as = NULL;     
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
   
  g95_free_array_spec(as);          
  return MATCH_ERROR;     
}       
       
       
    
    
/* g95_match_space()-- In free form, match at least one space.  Always
 * matches in fixed form. */      
      
match g95_match_space(void) {     
locus old;  
int p; 
 
  if (g95_current_file->form == FORM_FIXED) return MATCH_YES;          
          
  old = *g95_current_locus();          
          
  p = g95_next_char();       
  if (!g95_is_whitespace(p)) {
    g95_set_locus(&old);
    return MATCH_NO;      
  }     
     
  g95_gobble_whitespace();         
         
  return MATCH_YES;        
}         
         
         
       
       
/* g95_match_program()-- Matches the start of a program unit, which is
 * the program keyword followed by an optional symbol. */   
   
match g95_match_program(void) {  
g95_symbol *sym;     
match v;         
         
  v = g95_match_eos();   
  if (v == MATCH_YES) return v;       
       
  v = g95_match("% %s%t", &sym); 
 
  if (v == MATCH_NO) {
    g95_error("Invalid form of PROGRAM statement at %C");  
    v = MATCH_ERROR;      
  }    
    
  if (v == MATCH_ERROR) return v;  
  
  if (g95_add_flavor(&sym->attr, FL_PROGRAM, NULL) == FAILURE)         
    return MATCH_ERROR;  
  
  g95_new_block = sym;      
      
  return MATCH_YES;        
}       
       
       
          
          
/* g95_match_cycle()-- Match the CYCLE statement */       
       
match g95_match_cycle(void) {     
     
  return match_exit_cycle(ST_CYCLE, EXEC_CYCLE);
}         
         
         
         
         
/* match_forall_iterator()-- Match an iterator as part of a FORALL
 * statement.  The format is:
 *     <var> = <start>:<end>[:<stride>][, <scalar mask>]  */

static match match_forall_iterator(g95_forall_iterator **rslt) {          
g95_forall_iterator *iter;         
locus where;
match d;          
          
  where = *g95_current_locus();          
  iter = g95_getmem(sizeof(g95_forall_iterator));         
         
  d = g95_match_variable(&iter->var, 0);  
  if (d != MATCH_YES) {          
    d = MATCH_NO;    
    goto cleanup;    
  } 
 
  if (g95_match_char('=') != MATCH_YES) { 
    d = MATCH_NO;    
    goto cleanup;   
  }      
      
  d = g95_match_expr(&iter->start);          
  if (d == MATCH_NO) goto syntax;       
  if (d == MATCH_ERROR) goto cleanup;   
   
  if (g95_match_char(':') != MATCH_YES) goto syntax;      
      
  d = g95_match_expr(&iter->end);         
  if (d == MATCH_NO) goto syntax;    
  if (d == MATCH_ERROR) goto cleanup;

  if (g95_match_char(':') == MATCH_NO)          
    iter->stride = g95_int_expr(1); 
  else {      
    d = g95_match_expr(&iter->stride);         
    if (d == MATCH_NO) goto syntax;  
    if (d == MATCH_ERROR) goto cleanup;          
  }          
          
  *rslt = iter;
  return MATCH_YES;

syntax:         
  g95_error("Syntax error in FORALL iterator at %C");   
  d = MATCH_ERROR;  
  
cleanup: 
  g95_set_locus(&where);
  g95_free_forall_iterator(iter); 
  return d;     
}  
  
  
 
 
/* g95_match_forall()-- Match a FORALL statement */        
        
match g95_match_forall(g95_statement *sta) {    
g95_forall_iterator *start, *tail, *n;      
g95_expr *msk;        
g95_code *p;
match x, q;       
       
  start = tail = NULL; 
  msk = NULL;  
  p = NULL;       
       
  x = g95_match_label();       
  if (x == MATCH_ERROR) return MATCH_ERROR;         
         
  q = g95_match(" forall ("); 
  if (q != MATCH_YES) return q;      
      
  q = match_forall_iterator(&n);   
  if (q == MATCH_ERROR) goto cleanup;         
  if (q == MATCH_NO) goto syntax;         
         
  start = tail = n; 
 
  for(;;) {      
    if (g95_match_char(',') != MATCH_YES) break;      
      
    q = match_forall_iterator(&n); 
    if (q == MATCH_ERROR) goto cleanup;       
    if (q == MATCH_YES) {
      tail->next = n;       
      tail = n;         
      continue;
    }          
          
/* Have to have a mask expression */        
        
    q = g95_match_expr(&msk);   
    if (q == MATCH_NO) goto syntax;  
    if (q == MATCH_ERROR) goto cleanup;          
          
    break;
  }         
         
  if (g95_match_char(')') == MATCH_NO) goto syntax;          
          
  if (g95_match_eos() == MATCH_YES) {      
    *sta = ST_FORALL_BLOCK;  
  
    new_st.type = EXEC_FORALL;
    new_st.expr = msk;    
    new_st.ext.forall_iterator = start;        
        
    return MATCH_YES;          
  }  
  
  q = g95_match_assignment();     
  if (q == MATCH_ERROR) goto cleanup;  
  if (q == MATCH_NO) {  
    q = g95_match_pointer_assignment();     
    if (q == MATCH_ERROR) goto cleanup;
    if (q == MATCH_NO) goto syntax;      
  }          
          
  p = g95_get_code();   
  *p = new_st;          
  p->where = *g95_current_locus();         
         
  if (g95_match_eos() != MATCH_YES) goto syntax;    
    
  g95_clear_new_st();       
  new_st.type = EXEC_FORALL;       
  new_st.expr = msk;          
  new_st.ext.forall_iterator = start;     
  new_st.block = g95_get_code();  
  
  new_st.block->type = EXEC_FORALL;       
  new_st.block->next = p;

  *sta = ST_FORALL;  
  return MATCH_YES;         
         
syntax:    
  g95_syntax_error(ST_FORALL);        
        
cleanup:          
  g95_free_forall_iterator(start);        
  g95_free_expr(msk);         
  g95_free_statements(p);
  return MATCH_NO;      
}     
     
     
/* g95_match_assign()-- Match the (deprecated) ASSIGN statement. */       
       
match g95_match_assign(void) {   
g95_expr *expr;
int l;  
  
  if (g95_match(" %l to %v%t", &l, &expr) == MATCH_YES) {         
    g95_free_expr(expr);     
    g95_error("The ASSIGN statement at %C is not allowed in Fortran 95"); 
    return MATCH_ERROR;    
  }       
       
  return MATCH_NO;     
}    
    
    
         
         
/* g95_free_equiv()-- Free equivalence sets and lists.  Recursively is
 * the easiest way to do this. */ 
 
void g95_free_equiv(g95_equiv *q) {      
      
  if (q == NULL) return;       
       
  g95_free_equiv(q->eq);    
  g95_free_equiv(q->next);     
     
  g95_free_expr(q->expr);   
  g95_free(q);     
}       
       
       
