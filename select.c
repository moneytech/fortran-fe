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
  
  
    
    
/* compare_case()-- Compare two case nodes.  This is used to sort the
 * nodes into order and also gives us the perfect opportunity to check
 * for overlap.  The default case is sorted first in the list,
 * followed by a unbounded upper, followed by the full ranges in
 * order, followed by the unbounded lower.  We only report the first
 * overlap. */         
         
static int compare_case(const void *b, const void *z) {       
const g95_case *f, *c;          
int q, x;  
char *msg;       
       
  f = *((g95_case **) b); 
  c = *((g95_case **) z); 
 
  /* Check for duplicate defaults */     
     
  if (f->low == NULL && f->high == NULL &&       
      c->low == NULL && c->high == NULL) {

    msg = "Duplicate CASE DEFAULT at %L and %L";
    goto error;        
  }    
    
  /* Arrange for the default case to be first of all if present. */  
  
  if (f->low == NULL && f->high == NULL) return -1;
  if (c->low == NULL && c->high == NULL) return 1;    
    
  /* Detect duplicate X: and :X forms.  These conflict regardless of X. */ 
 
  if ((f->high == NULL && c->high == NULL) ||        
      (f->low  == NULL && c->low  == NULL)) {          
    msg = "Unbounded CASEs conflict at %L and %L";      
    goto error;         
  }        
        
  /* Compare X: against :A */   
   
  if (f->high == NULL && c->low == NULL) {
    if (g95_compare_expr(f->low, c->high) <= 0) goto got_overlap;       
    return 1;  
  }

  /* Compare :X against B: */      
      
  if (f->low == NULL && c->high == NULL) {   
    if (g95_compare_expr(f->high, c->low) >= 0) goto got_overlap; 
    return -1;         
  }          
          
  /* Compare :X against A:B */ 
 
  if (f->low == NULL) {        
    if (g95_compare_expr(f->high, c->low) >= 0) goto got_overlap;      
    return -1;    
  }  
  
  /* Compare X: against A:B */ 
 
  if (f->high == NULL) {    
    if (g95_compare_expr(f->low, c->high) <= 0) goto got_overlap;        
    return 1;   
  }   
   
  /* Compare X:Y against :A */     
     
  if (c->low == NULL) {
    if (g95_compare_expr(f->low, c->high) <= 0) goto got_overlap;       
    return 1;       
  }        
        
  /* Compare X:Y against A: */        
        
  if (c->high == NULL) {     
    if (g95_compare_expr(f->high, c->low) >= 0) goto got_overlap; 
    return -1;        
  }    
    
  /* Having dispensed with almost a dozen special cases, we can now
   * deal with the general case of X:Y against A:B */     
     
  q = g95_compare_expr(f->high, c->low);     
  if (q < 0) q = -1;   
  if (q > 0) q = 1;

  x = g95_compare_expr(f->low, c->high);       
  if (x < 0) x = -1;   
  if (x > 0) x = 1;  
  
  if (q == 0 || x == 0 || q != x) goto got_overlap; 
  return q;     
     
got_overlap:      
  msg = "CASEs at %L and %L overlap";       
       
error:     
  if (!overlap) {         
    g95_error(msg, &f->where, &c->where);     
    overlap = 1;
  }   
   
  /* Because we've generated an error, no code will be generated, and
   * the order of the case array no longer matters.  Return something
   * to keep qsort() happy. */ 
 
  return 1;   
}   
   
   
    
    
/* g95_match_select()-- Match a SELECT statement */    
    
match g95_match_select(void) {        
g95_expr *e2;         
match w;        
        
  w = g95_match_label();   
  if (w == MATCH_ERROR) return w; 
 
  w = g95_match(" select case ( %e )%t", &e2);     
  if (w != MATCH_YES) return w;       
       
  new_st.type = EXEC_SELECT;         
  new_st.expr = e2;         
         
  return MATCH_YES;          
}     
     
     


/* free_case()-- Free a single case structure. */

static void free_case(g95_case *b) {     
     
  g95_free_expr(b->low);  
  if (b->high != b->low) g95_free_expr(b->high);          
          
  g95_free(b);          
}  
  
  
        
        
/* match_case_eos()-- Match the end of a case statement */       
       
static match match_case_eos(void) {  
char n[G95_MAX_SYMBOL_LEN+1];      
match s;     
     
  if (g95_match_eos() == MATCH_YES) return MATCH_YES;  
  
  g95_gobble_whitespace();  
  
  s = g95_match_name(n);          
  if (s != MATCH_YES) return s;         
         
  if (strcmp(n, g95_current_block()->name) != 0) {  
    g95_error("Expected case name of '%s' at %C", g95_current_block()->name);    
    return MATCH_ERROR;
  }   
   
  return g95_match_eos();    
}  
  
  
        
        
/* check_case_expr()-- Check to see if an expression is suitable for
 * use in a CASE statement. */     
     
static try check_case_expr(g95_expr *m, g95_expr *selector) {       
       
  if (m == NULL) return SUCCESS;        
        
  if (m->type != EXPR_CONSTANT) {  
    g95_error("Expression in CASE statement at %L must be a constant",   
	      &m->where);    
    return FAILURE;        
  }  
  
  if (m->ts.type != selector->ts.type) {   
    g95_error("Expression in CASE statement at %L must be of type %s",    
	      &m->where, g95_basic_typename(selector->ts.type));          
    return FAILURE;          
  }        
        
  if (m->ts.kind != selector->ts.kind) {        
    g95_error("Expression in CASE statement at %L must be kind %d",     
	      &m->where, selector->ts.kind);      
    return FAILURE;      
  }     
     
  if (m->rank != 0) {       
    g95_error("Expression in CASE statement at %L must be scalar",    
	      &m->where);         
    return FAILURE;         
  } 
 
  return SUCCESS;       
}   
   
   
        
        
/* match_case_selector()-- Match a single case selector. */  
  
static match match_case_selector(g95_case **cp) { 
g95_case *n;      
match d;

  n = g95_get_case();   
  n->where = *g95_current_locus();        
        
  if (g95_match_char(':') == MATCH_YES) {         
    d = g95_match_init_expr(&n->high);
    if (d == MATCH_NO) goto need_expr;   
    if (d == MATCH_ERROR) goto cleanup;          
          
    if (n->high->ts.type == BT_LOGICAL) goto logical_range;       
    goto done;     
  }    
    
  d = g95_match_init_expr(&n->low);          
  if (d == MATCH_ERROR) goto cleanup;  
  if (d == MATCH_NO) goto need_expr;  
  
  if (g95_match_char(':') != MATCH_YES)     
    n->high = n->low;      /* Make a range out of a single target */        
  else {     
    d = g95_match_init_expr(&n->high);          
    if (d == MATCH_ERROR) goto cleanup;
    if (d == MATCH_NO) goto done;   /* It's OK if nothing is there! */    
    
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
g95_case *a, *h, *tail; 
match z;   
   
  h = tail = NULL;    
    
  if (g95_current_state() != COMP_SELECT) {          
    g95_error("Unexpected CASE statement at %C");     
    return MATCH_ERROR;    
  }    
    
  if (g95_match("% default") == MATCH_YES) {
    z = match_case_eos();    
    if (z == MATCH_NO) goto syntax;          
    if (z == MATCH_ERROR) goto cleanup;       
       
    new_st.type = EXEC_SELECT;        
    new_st.ext.case_list = g95_get_case();     
    return MATCH_YES;  
  }      
      
  if (g95_match_char('(') != MATCH_YES) goto syntax; 
 
  for(;;) {   
    if (match_case_selector(&a) == MATCH_ERROR) goto cleanup;       
       
    /* Cases that can never be matched are legal to have but mess up
     * code generation, so we discard them here. */  
  
    if (a->low != NULL && a->high != NULL && a->low != a->high &&          
	g95_compare_expr(a->low, a->high) > 0) {       
      g95_warning("Range specification at %C can never be matched");
      free_case(a);    
    } else { 
      if (h == NULL)      
	h = a;        
      else     
	tail->next = a;       
       
      tail = a;      
    }         
         
    if (g95_match_char(')') == MATCH_YES) break; 
    if (g95_match_char(',') != MATCH_YES) goto syntax;         
  } 
 
  z = match_case_eos();
  if (z == MATCH_NO) goto syntax;
  if (z == MATCH_ERROR) goto cleanup;       
       
  new_st.type = EXEC_SELECT;         
  new_st.ext.case_list = h;     
     
  return MATCH_YES;          
          
syntax: 
  g95_error("Syntax error in CASE-specification at %C");      
      
cleanup:        
  g95_free_case_list(h);          
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
        
try g95_resolve_select(g95_code *cp) {    
g95_case *k, **ap;         
g95_code *list;       
g95_expr *exp; 
int y, a;  
  
  if (cp->expr == NULL) {       
    /* This was originally a computed GOTO statement.  */      
    exp = cp->expr2;     
     
    if (exp->ts.type != BT_INTEGER || exp->rank != 0) {
      g95_error("Selection expression in COMPUTED GOTO statement "
                "at %L must be a scalar integer expression",         
                &exp->where);      
      return FAILURE;          
    }        
        
    return SUCCESS;    
  }        
        
  exp = cp->expr;    
        
  if (exp->ts.type == BT_DERIVED || exp->ts.type == BT_REAL ||  
      exp->ts.type == BT_COMPLEX) {          
    g95_error("Argument of SELECT statement at %L cannot be %s", 
               &exp->where, g95_typename(&exp->ts));    
    return FAILURE;         
  } 
 
  if (exp->rank != 0) {  
    g95_error("Argument of SELECT statement at %L must be a scalar "   
              "expression", &exp->where);
    return FAILURE;   
  }      
      
  a = 0;         
         
  for(list=cp->block; list; list=list->block) {
    for(k=list->ext.case_list; k; k=k->next) {
      k->code = list->next;      
      a++;   
   
      if (check_case_expr(k->low, exp) == FAILURE) return FAILURE;          
          
      if (check_case_expr(k->high, exp) == FAILURE) return FAILURE;      
      
      if (exp->ts.type == BT_LOGICAL &&
	  (k->low != NULL || k->high != NULL) &&         
	  (k->low == NULL || k->high == NULL)) {         
         
	g95_error("Logical range in CASE statement at %L is not allowed",     
		  &k->low->where);          
	return FAILURE;       
      }  
    }   
  }          
          
  if (a == 0) return SUCCESS;   
   
  ap = g95_getmem(a*sizeof(g95_case *));  
  a = 0;       
       
  for(list=cp->block; list; list=list->block)      
    for(k=list->ext.case_list; k; k=k->next)          
      ap[a++] = k;  
  
  overlap = 0;

  qsort(ap, a, sizeof(g95_case *), compare_case);          
          
  if (overlap) return FAILURE;     
     
  /* String the case structures together in a doubly linked list */      
      
  ap[0]->cprev = NULL;   
  ap[0]->cnext = (a > 1) ? ap[1] : NULL;    
    
  if (a > 1) {     
    for(y=1; y<a-1; y++) {   
      ap[y]->cprev = ap[y-1];     
      ap[y]->cnext = ap[y+1];
    }          
          
    ap[a-1]->cprev = ap[a-2];  
    ap[a-1]->cnext = NULL;         
  }    
    
  g95_free(ap);          
  return SUCCESS; 
}  
         
         
/* g95_free_case_list()-- Free a list of case structures */

void g95_free_case_list(g95_case *u) {          
g95_case *y;          
          
  for(;u ;u=y) { 
    y = u->next;
    free_case(u);  
  }
}     
     
     
