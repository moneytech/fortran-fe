/* SELECT CASE statement
   Copyright (C) 2000, 2001, 2002 Free Software Foundation, Inc.
   Contributed by Andy Vaught and Steven Bosscher

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

/* select.c-- Handle the SELECT CASE statement. */       
       
#include <string.h>
#include <stdlib.h>
   
#include "g95.h"
 
static int overlap;       
       
       
         
         
/* match_case_eos()-- Match the end of a case statement */     
     
static match match_case_eos(void) {  
char name[G95_MAX_SYMBOL_LEN+1];       
match d;     
     
  if (g95_match_eos() == MATCH_YES) return MATCH_YES;         
         
  g95_gobble_whitespace();       
       
  d = g95_match_name(name);          
  if (d != MATCH_YES) return d;  
  
  if (strcmp(name, g95_current_block()->name) != 0) {        
    g95_error("Expected case name of '%s' at %C", g95_current_block()->name);    
    return MATCH_ERROR;        
  }      
      
  return g95_match_eos();       
} 
 
 
  
  
/* free_case()-- Free a single case structure. */  
  
static void free_case(g95_case *h) {   
   
  g95_free_expr(h->low);        
  if (h->high != h->low) g95_free_expr(h->high);       
       
  g95_free(h);       
}          
          
          
   
   
/* check_case_expr()-- Check to see if an expression is suitable for
 * use in a CASE statement. */     
     
static try check_case_expr(g95_expr *i, g95_expr *selector) {          
          
  if (i == NULL) return SUCCESS;     
     
  if (i->type != EXPR_CONSTANT) {         
    g95_error("Expression in CASE statement at %L must be a constant", 
	      &i->where);        
    return FAILURE;       
  }          
          
  if (i->ts.type != selector->ts.type) {
    g95_error("Expression in CASE statement at %L must be of type %s", 
	      &i->where, g95_basic_typename(selector->ts.type));     
    return FAILURE;   
  }       
       
  if (i->ts.kind != selector->ts.kind) {   
    g95_error("Expression in CASE statement at %L must be kind %d",   
	      &i->where, selector->ts.kind);  
    return FAILURE;   
  } 
 
  if (i->rank != 0) {        
    g95_error("Expression in CASE statement at %L must be scalar",       
	      &i->where);   
    return FAILURE;      
  }     
     
  return SUCCESS; 
}     
     
     
     
     
/* g95_free_case_list()-- Free a list of case structures */ 
 
void g95_free_case_list(g95_case *z) {
g95_case *o;      
      
  for(;z ;z=o) {     
    o = z->next;       
    free_case(z);     
  }    
}         
         
         
     
     
/* g95_match_select()-- Match a SELECT statement */    
    
match g95_match_select(void) {       
g95_expr *expr;         
match h;    
    
  h = g95_match_label();        
  if (h == MATCH_ERROR) return h;          
          
  h = g95_match(" select case ( %e )%t", &expr);   
  if (h != MATCH_YES) return h;         
         
  new_st.type = EXEC_SELECT;  
  new_st.expr = expr;         
         
  return MATCH_YES; 
}   
   
   
     
     
/* compare_case()-- Compare two case nodes.  This is used to sort the
 * nodes into order and also gives us the perfect opportunity to check
 * for overlap.  The default case is sorted first in the list,
 * followed by a unbounded upper, followed by the full ranges in
 * order, followed by the unbounded lower.  We only report the first
 * overlap. */ 
 
static int compare_case(const void *v1, const void *h) {   
const g95_case *f, *e;       
int z, p2;       
char *msg; 
 
  f = *((g95_case **) v1);      
  e = *((g95_case **) h);         
         
  /* Check for duplicate defaults */          
          
  if (f->low == NULL && f->high == NULL &&       
      e->low == NULL && e->high == NULL) { 
 
    msg = "Duplicate CASE DEFAULT at %L and %L";         
    goto error;          
  }     
     
  /* Arrange for the default case to be first of all if present. */   
   
  if (f->low == NULL && f->high == NULL) return -1;    
  if (e->low == NULL && e->high == NULL) return 1;          
          
  /* Detect duplicate X: and :X forms.  These conflict regardless of X. */      
      
  if ((f->high == NULL && e->high == NULL) ||        
      (f->low  == NULL && e->low  == NULL)) {          
    msg = "Unbounded CASEs conflict at %L and %L";          
    goto error;        
  }      
      
  /* Compare X: against :A */  
  
  if (f->high == NULL && e->low == NULL) {    
    if (g95_compare_expr(f->low, e->high) <= 0) goto got_overlap;
    return 1; 
  }     
     
  /* Compare :X against B: */        
        
  if (f->low == NULL && e->high == NULL) {
    if (g95_compare_expr(f->high, e->low) >= 0) goto got_overlap;     
    return -1;         
  }         
         
  /* Compare :X against A:B */      
      
  if (f->low == NULL) {       
    if (g95_compare_expr(f->high, e->low) >= 0) goto got_overlap;        
    return -1;   
  }   
   
  /* Compare X: against A:B */    
    
  if (f->high == NULL) {      
    if (g95_compare_expr(f->low, e->high) <= 0) goto got_overlap;
    return 1;         
  }      
      
  /* Compare X:Y against :A */ 
 
  if (e->low == NULL) { 
    if (g95_compare_expr(f->low, e->high) <= 0) goto got_overlap;    
    return 1;      
  }  
  
  /* Compare X:Y against A: */       
       
  if (e->high == NULL) {   
    if (g95_compare_expr(f->high, e->low) >= 0) goto got_overlap;         
    return -1;
  } 
 
  /* Having dispensed with almost a dozen special cases, we can now
   * deal with the general case of X:Y against A:B */        
        
  z = g95_compare_expr(f->high, e->low);    
  if (z < 0) z = -1;        
  if (z > 0) z = 1;     
     
  p2 = g95_compare_expr(f->low, e->high);    
  if (p2 < 0) p2 = -1;
  if (p2 > 0) p2 = 1;   
   
  if (z == 0 || p2 == 0 || z != p2) goto got_overlap;    
  return z;          
          
got_overlap:      
  msg = "CASEs at %L and %L overlap";      
      
error:     
  if (!overlap) {   
    g95_error(msg, &f->where, &e->where);     
    overlap = 1;    
  }    
    
  /* Because we've generated an error, no code will be generated, and
   * the order of the case array no longer matters.  Return something
   * to keep qsort() happy. */       
       
  return 1;         
}     
     
     
       
       
/* match_case_selector()-- Match a single case selector. */   
   
static match match_case_selector(g95_case **cp) {  
g95_case *n;  
match u;     
     
  n = g95_get_case();    
  n->where = *g95_current_locus(); 
 
  if (g95_match_char(':') == MATCH_YES) {      
    u = g95_match_expr(&n->high);  
    if (u == MATCH_NO) goto need_expr;
    if (u == MATCH_ERROR) goto cleanup;        
        
    if (n->high->ts.type == BT_LOGICAL) goto logical_range; 
    goto done;          
  } 
 
  u = g95_match_expr(&n->low);      
  if (u == MATCH_ERROR) goto cleanup;  
  if (u == MATCH_NO) goto need_expr;   
   
  if (g95_match_char(':') != MATCH_YES)          
    n->high = n->low;      /* Make a range out of a single target */ 
  else {         
    u = g95_match_expr(&n->high);
    if (u == MATCH_ERROR) goto cleanup;  
    if (u == MATCH_NO) goto done;   /* It's OK if nothing is there! */

    if (n->high->ts.type == BT_LOGICAL) goto logical_range;       
  }     
     
done:     
  *cp = n; 
  return MATCH_YES;  
  
logical_range:
  g95_error("Logical range in CASE statement at %C not allowed");        
  goto cleanup;  
  
need_expr:          
  g95_error("Expected expression in CASE at %C");         
         
cleanup:      
  free_case(n);       
  return MATCH_ERROR; 
}     
     
     
    
    
/* g95_match_case()-- Match a CASE statement */   
   
match g95_match_case(void) {          
g95_case *u, *head, *tail;  
match q;     
     
  head = tail = NULL;   
   
  if (g95_current_state() != COMP_SELECT) {       
    g95_error("Unexpected CASE statement at %C");    
    return MATCH_ERROR;     
  }         
         
  if (g95_match("% default") == MATCH_YES) {       
    q = match_case_eos();  
    if (q == MATCH_NO) goto syntax;
    if (q == MATCH_ERROR) goto cleanup; 
 
    new_st.type = EXEC_SELECT;         
    new_st.ext.case_list = g95_get_case();      
    return MATCH_YES;     
  }          
          
  if (g95_match_char('(') != MATCH_YES) goto syntax; 
 
  for(;;) {       
    if (match_case_selector(&u) == MATCH_ERROR) goto cleanup;     
     
    /* Cases that can never be matched are legal to have but mess up
     * code generation, so we discard them here. */         
         
    if (u->low != NULL && u->high != NULL && u->low != u->high &&      
	g95_compare_expr(u->low, u->high) > 0) {         
      g95_warning("Range specification at %C can never be matched");    
      free_case(u);         
    } else {     
      if (head == NULL)       
	head = u;        
      else     
	tail->next = u; 
 
      tail = u;   
    }  
  
    if (g95_match_char(')') == MATCH_YES) break;   
    if (g95_match_char(',') != MATCH_YES) goto syntax;       
  }     
     
  q = match_case_eos();     
  if (q == MATCH_NO) goto syntax;     
  if (q == MATCH_ERROR) goto cleanup;

  new_st.type = EXEC_SELECT;         
  new_st.ext.case_list = head;         
         
  return MATCH_YES;

syntax: 
  g95_error("Syntax error in CASE-specification at %C");   
   
cleanup:   
  g95_free_case_list(head);     
  g95_undo_statement();         
  return MATCH_ERROR;    
}  
  
  
  
  
/* g95_resolve_select()-- Given a completely parsed select statement, we:
 *
 *   Resolve all expressions and code within the SELECT
 *   Make sure that the selection expression is not of the wrong type
 *   Make sure that all case expressions are of the same type/kind
 *   Make sure that no case ranges overlap
 *
 * We have the additional caveat that a SELECT construct could have
 * been a computed GOTO in the source code. Furtunately we're done
 * here prety quick: all we have to make sure is that the case_expr
 * is a scalar integer expression.
 */       
       
void g95_resolve_select(g95_code *code) {  
g95_case *h, **array;
g95_code *body;         
g95_expr *expr;         
int c, f;          
try x;    
    
  if (code->expr == NULL) {          
    /* This was originally a computed GOTO statement.  */  
    expr = code->expr2;       
       
    if (expr->ts.type != BT_INTEGER || expr->rank != 0)
      g95_error("Selection expression in COMPUTED GOTO statement "   
                "at %L must be a scalar integer expression",    
                &expr->where);          
    return; 
  }       
       
  expr = code->expr;         
             
  if (expr->ts.type == BT_DERIVED || expr->ts.type == BT_REAL ||       
      expr->ts.type == BT_COMPLEX) {   
    g95_error("Argument of SELECT statement at %L cannot be %s",        
               &expr->where, g95_typename(&expr->ts));     
    return; /* Going on here just produce more garbage error messages.  */   
  }       
       
  if (expr->rank != 0) {        
    g95_error("Argument of SELECT statement at %L must be a scalar "       
              "expression", &expr->where);   
    return;      
  }    
    
  x = SUCCESS;        
  f = 0;   
   
  for(body=code->block; body; body=body->block) {        
    for(h=body->ext.case_list; h; h=h->next) {          
      h->code = body->next;        
      f++;      
      
      if (check_case_expr(h->low, expr) == FAILURE) {
	x = FAILURE;  
	break;
      }     
     
      if (check_case_expr(h->high, expr) == FAILURE) {          
	x = FAILURE;  
	break;         
      }

      if (expr->ts.type == BT_LOGICAL &&          
	  (h->low != NULL || h->high != NULL) &&   
	  (h->low == NULL || h->high == NULL)) {          
          
	g95_error("Logical range in CASE statement at %L is not allowed",
		  &h->low->where); 
	x = FAILURE;
      }       
    }       
       
    if (x == FAILURE) break;  
  }

  if (x == FAILURE || f == 0) return;

  array = g95_getmem(f*sizeof(g95_case *));        
  f = 0;         
         
  for(body=code->block; body; body=body->block)      
    for(h=body->ext.case_list; h; h=h->next)       
      array[f++] = h;    
    
  overlap = 0;   
   
  qsort(array, f, sizeof(g95_case *), compare_case);   
   
  if (overlap) return;  
  
  /* String the case structures together in a doubly linked list */         
         
  array[0]->cprev = NULL;
  array[0]->cnext = (f > 1) ? array[1] : NULL;

  if (f > 1) {    
    for(c=1; c<f-1; c++) {       
      array[c]->cprev = array[c-1];  
      array[c]->cnext = array[c+1];        
    }       
       
    array[f-1]->cprev = array[f-2]; 
    array[f-1]->cnext = NULL;      
  } 
 
  g95_free(array);    
}        
