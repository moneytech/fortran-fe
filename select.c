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
  
  


/* g95_match_select()-- Match a SELECT statement */

match g95_match_select(void) {   
g95_expr *e1;
match f; 
 
  f = g95_match_label();         
  if (f == MATCH_ERROR) return f;          
          
  f = g95_match(" select case ( %e )%t", &e1);         
  if (f != MATCH_YES) return f;          
          
  new_st.type = EXEC_SELECT;
  new_st.expr = e1;       
       
  return MATCH_YES;       
}      
      
      
          
          
/* compare_case()-- Compare two case nodes.  This is used to sort the
 * nodes into order and also gives us the perfect opportunity to check
 * for overlap.  The default case is sorted first in the list,
 * followed by a unbounded upper, followed by the full ranges in
 * order, followed by the unbounded lower.  We only report the first
 * overlap. */     
     
static int compare_case(const void *w, const void *y) {       
const g95_case *q, *u;    
int o, f;
char *m;

  q = *((g95_case **) w);
  u = *((g95_case **) y);       
       
  /* Check for duplicate defaults */        
        
  if (q->low == NULL && q->high == NULL &&   
      u->low == NULL && u->high == NULL) {          
          
    m = "Duplicate CASE DEFAULT at %L and %L";     
    goto error; 
  }  
  
  /* Arrange for the default case to be first of all if present. */          
          
  if (q->low == NULL && q->high == NULL) return -1;        
  if (u->low == NULL && u->high == NULL) return 1; 
 
  /* Detect duplicate X: and :X forms.  These conflict regardless of X. */       
       
  if ((q->high == NULL && u->high == NULL) ||       
      (q->low  == NULL && u->low  == NULL)) { 
    m = "Unbounded CASEs conflict at %L and %L";      
    goto error;      
  }

  /* Compare X: against :A */        
        
  if (q->high == NULL && u->low == NULL) { 
    if (g95_compare_expr(q->low, u->high) <= 0) goto got_overlap; 
    return 1;   
  }        
        
  /* Compare :X against B: */          
          
  if (q->low == NULL && u->high == NULL) {       
    if (g95_compare_expr(q->high, u->low) >= 0) goto got_overlap;   
    return -1;     
  }          
          
  /* Compare :X against A:B */          
          
  if (q->low == NULL) {        
    if (g95_compare_expr(q->high, u->low) >= 0) goto got_overlap;     
    return -1;      
  }   
   
  /* Compare X: against A:B */        
        
  if (q->high == NULL) {  
    if (g95_compare_expr(q->low, u->high) <= 0) goto got_overlap;       
    return 1;         
  }   
   
  /* Compare X:Y against :A */    
    
  if (u->low == NULL) {          
    if (g95_compare_expr(q->low, u->high) <= 0) goto got_overlap; 
    return 1;   
  } 
 
  /* Compare X:Y against A: */       
       
  if (u->high == NULL) {          
    if (g95_compare_expr(q->high, u->low) >= 0) goto got_overlap;    
    return -1;         
  }    
    
  /* Having dispensed with almost a dozen special cases, we can now
   * deal with the general case of X:Y against A:B */      
      
  o = g95_compare_expr(q->high, u->low);
  if (o < 0) o = -1;
  if (o > 0) o = 1; 
 
  f = g95_compare_expr(q->low, u->high);  
  if (f < 0) f = -1;         
  if (f > 0) f = 1;     
     
  if (o == 0 || f == 0 || o != f) goto got_overlap;  
  return o; 
 
got_overlap:      
  m = "CASEs at %L and %L overlap";        
        
error:        
  if (!overlap) {        
    g95_error(m, &q->where, &u->where);         
    overlap = 1;        
  }   
   
  /* Because we've generated an error, no code will be generated, and
   * the order of the case array no longer matters.  Return something
   * to keep qsort() happy. */        
        
  return 1;   
}          
          
          
 
 
/* free_case()-- Free a single case structure. */    
    
static void free_case(g95_case *f) {  
  
  g95_free_expr(f->low);          
  if (f->high != f->low) g95_free_expr(f->high);       
       
  g95_free(f);          
}       
       
       
         
         
/* check_case_expr()-- Check to see if an expression is suitable for
 * use in a CASE statement. */   
   
static try check_case_expr(g95_expr *g, g95_expr *selector) { 
 
  if (g == NULL) return SUCCESS;  
  
  if (g->type != EXPR_CONSTANT) {
    g95_error("Expression in CASE statement at %L must be a constant",          
	      &g->where);         
    return FAILURE;     
  }  
  
  if (g->ts.type != selector->ts.type) {   
    g95_error("Expression in CASE statement at %L must be of type %s",     
	      &g->where, g95_basic_typename(selector->ts.type)); 
    return FAILURE;
  }

  if (g->ts.kind != selector->ts.kind) {          
    g95_error("Expression in CASE statement at %L must be kind %d",     
	      &g->where, selector->ts.kind);       
    return FAILURE;        
  }   
   
  if (g->rank != 0) {         
    g95_error("Expression in CASE statement at %L must be scalar",          
	      &g->where);      
    return FAILURE;   
  }      
      
  return SUCCESS;    
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
         
try g95_resolve_select(g95_code *code) { 
g95_case *d, **a;    
g95_code *body;
g95_expr *e2;     
int f, b;       
       
  if (code->expr == NULL) {    
    /* This was originally a computed GOTO statement.  */  
    e2 = code->expr2;          
          
    if (e2->ts.type != BT_INTEGER || e2->rank != 0) {  
      g95_error("Selection expression in COMPUTED GOTO statement "        
                "at %L must be a scalar integer expression",      
                &e2->where);        
      return FAILURE;          
    }    
    
    return SUCCESS;         
  }  
  
  e2 = code->expr;          
              
  if (e2->ts.type == BT_DERIVED || e2->ts.type == BT_REAL ||    
      e2->ts.type == BT_COMPLEX) {       
    g95_error("Argument of SELECT statement at %L cannot be %s",   
               &e2->where, g95_typename(&e2->ts)); 
    return FAILURE; 
  }  
  
  if (e2->rank != 0) { 
    g95_error("Argument of SELECT statement at %L must be a scalar "  
              "expression", &e2->where);        
    return FAILURE;   
  }    
    
  b = 0;   
   
  for(body=code->block; body; body=body->block) {    
    for(d=body->ext.case_list; d; d=d->next) {          
      d->code = body->next;         
      b++;         
         
      if (check_case_expr(d->low, e2) == FAILURE) return FAILURE;      
      
      if (check_case_expr(d->high, e2) == FAILURE) return FAILURE;          
          
      if (e2->ts.type == BT_LOGICAL &&      
	  (d->low != NULL || d->high != NULL) && 
	  (d->low == NULL || d->high == NULL)) {      
      
	g95_error("Logical range in CASE statement at %L is not allowed",          
		  &d->low->where);      
	return FAILURE;        
      }   
    }
  }     
     
  if (b == 0) return SUCCESS; 
 
  a = g95_getmem(b*sizeof(g95_case *));   
  b = 0;        
        
  for(body=code->block; body; body=body->block)        
    for(d=body->ext.case_list; d; d=d->next)      
      a[b++] = d;  
  
  overlap = 0; 
 
  qsort(a, b, sizeof(g95_case *), compare_case);      
      
  if (overlap) return FAILURE;  
  
  /* String the case structures together in a doubly linked list */   
   
  a[0]->cprev = NULL;    
  a[0]->cnext = (b > 1) ? a[1] : NULL;      
      
  if (b > 1) {      
    for(f=1; f<b-1; f++) { 
      a[f]->cprev = a[f-1];     
      a[f]->cnext = a[f+1];          
    }     
     
    a[b-1]->cprev = a[b-2];       
    a[b-1]->cnext = NULL;   
  }  
  
  g95_free(a);      
  return SUCCESS;      
}  
       
       
/* match_case_selector()-- Match a single case selector. */          
          
static match match_case_selector(g95_case **cp) {       
g95_case *n; 
match u;   
   
  n = g95_get_case();    
  n->where = g95_current_locus;    
    
  if (g95_match_char(':') == MATCH_YES) {          
    u = g95_match_init_expr(&n->high);  
    if (u == MATCH_NO) goto need_expr;         
    if (u == MATCH_ERROR) goto cleanup;   
   
    if (n->high->ts.type == BT_LOGICAL) goto logical_range;          
    goto done;    
  } 
 
  u = g95_match_init_expr(&n->low);  
  if (u == MATCH_ERROR) goto cleanup;      
  if (u == MATCH_NO) goto need_expr; 
 
  if (g95_match_char(':') != MATCH_YES)    
    n->high = n->low;      /* Make a range out of a single target */ 
  else {
    u = g95_match_init_expr(&n->high); 
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
     
     
        
        
/* match_case_eos()-- Match the end of a case statement */       
       
static match match_case_eos(void) {        
char nam[G95_MAX_SYMBOL_LEN+1];     
match t;    
    
  if (g95_match_eos() == MATCH_YES) return MATCH_YES;       
       
  g95_gobble_whitespace();      
      
  t = g95_match_name(nam); 
  if (t != MATCH_YES) return t;     
     
  if (strcmp(nam, g95_current_block()->name) != 0) {
    g95_error("Expected case name of '%s' at %C", g95_current_block()->name);        
    return MATCH_ERROR;    
  }  
  
  return g95_match_eos();   
}          
          
          
        
        
/* g95_free_case_list()-- Free a list of case structures */       
       
void g95_free_case_list(g95_case *p) {
g95_case *d;          
          
  for(;p ;p=d) {       
    d = p->next;         
    free_case(p); 
  }          
}    
    
    
   
   
/* g95_match_case()-- Match a CASE statement */       
       
match g95_match_case(void) {         
g95_case *h, *start, *end;        
match u;          
          
  start = end = NULL;          
          
  if (g95_current_state() != COMP_SELECT) {
    g95_error("Unexpected CASE statement at %C");          
    return MATCH_ERROR;       
  }     
     
  if (g95_match("% default") == MATCH_YES) {
    u = match_case_eos();         
    if (u == MATCH_NO) goto syntax;       
    if (u == MATCH_ERROR) goto cleanup;   
   
    new_st.type = EXEC_SELECT;     
    new_st.ext.case_list = g95_get_case();   
    return MATCH_YES;        
  }      
      
  if (g95_match_char('(') != MATCH_YES) goto syntax;   
   
  for(;;) {        
    if (match_case_selector(&h) == MATCH_ERROR) goto cleanup; 
 
    /* Cases that can never be matched are legal to have but mess up
     * code generation, so we discard them here. */        
        
    if (h->low != NULL && h->high != NULL && h->low != h->high &&        
	g95_compare_expr(h->low, h->high) > 0) { 
      g95_warning(117, "Range specification at %C can never be matched");      
      free_case(h);       
    } else {  
      if (start == NULL)  
	start = h;     
      else       
	end->next = h;       
       
      end = h;      
    }         
         
    if (g95_match_char(')') == MATCH_YES) break;     
    if (g95_match_char(',') != MATCH_YES) goto syntax;      
  }          
          
  u = match_case_eos();    
  if (u == MATCH_NO) goto syntax;
  if (u == MATCH_ERROR) goto cleanup;     
     
  new_st.type = EXEC_SELECT;       
  new_st.ext.case_list = start; 
 
  return MATCH_YES;    
    
syntax:     
  g95_error("Syntax error in CASE-specification at %C");    
    
cleanup:       
  g95_free_case_list(start);          
  g95_undo_statement();          
  return MATCH_ERROR;
}      
      
      
