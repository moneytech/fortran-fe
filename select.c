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
g95_expr *exp;   
match z;     
     
  z = g95_match_label();    
  if (z == MATCH_ERROR) return z;   
   
  z = g95_match(" select case ( %e )%t", &exp);     
  if (z != MATCH_YES) return z;     
     
  new_st.type = EXEC_SELECT;    
  new_st.expr = exp;        
        
  return MATCH_YES;      
}


  
  
/* compare_case()-- Compare two case nodes.  This is used to sort the
 * nodes into order and also gives us the perfect opportunity to check
 * for overlap.  The default case is sorted first in the list,
 * followed by a unbounded upper, followed by the full ranges in
 * order, followed by the unbounded lower.  We only report the first
 * overlap. */          
          
static int compare_case(const void *h, const void *q) {          
const g95_case *u, *i;     
int x, v; 
char *m;       
       
  u = *((g95_case **) h);          
  i = *((g95_case **) q);      
      
  /* Check for duplicate defaults */  
  
  if (u->low == NULL && u->high == NULL && 
      i->low == NULL && i->high == NULL) {   
   
    m = "Duplicate CASE DEFAULT at %L and %L"; 
    goto error;  
  }   
   
  /* Arrange for the default case to be first of all if present. */     
     
  if (u->low == NULL && u->high == NULL) return -1;      
  if (i->low == NULL && i->high == NULL) return 1;  
  
  /* Detect duplicate X: and :X forms.  These conflict regardless of X. */   
   
  if ((u->high == NULL && i->high == NULL) ||
      (u->low  == NULL && i->low  == NULL)) {  
    m = "Unbounded CASEs conflict at %L and %L";  
    goto error;       
  }     
     
  /* Compare X: against :A */     
     
  if (u->high == NULL && i->low == NULL) {       
    if (g95_compare_expr(u->low, i->high) <= 0) goto got_overlap; 
    return 1;     
  }    
    
  /* Compare :X against B: */     
     
  if (u->low == NULL && i->high == NULL) {          
    if (g95_compare_expr(u->high, i->low) >= 0) goto got_overlap;     
    return -1;        
  }        
        
  /* Compare :X against A:B */

  if (u->low == NULL) {       
    if (g95_compare_expr(u->high, i->low) >= 0) goto got_overlap;     
    return -1;       
  }     
     
  /* Compare X: against A:B */

  if (u->high == NULL) {          
    if (g95_compare_expr(u->low, i->high) <= 0) goto got_overlap;    
    return 1;          
  } 
 
  /* Compare X:Y against :A */ 
 
  if (i->low == NULL) {
    if (g95_compare_expr(u->low, i->high) <= 0) goto got_overlap;  
    return 1;        
  }    
    
  /* Compare X:Y against A: */    
    
  if (i->high == NULL) {      
    if (g95_compare_expr(u->high, i->low) >= 0) goto got_overlap;        
    return -1;
  }

  /* Having dispensed with almost a dozen special cases, we can now
   * deal with the general case of X:Y against A:B */        
        
  x = g95_compare_expr(u->high, i->low);          
  if (x < 0) x = -1;     
  if (x > 0) x = 1;   
   
  v = g95_compare_expr(u->low, i->high);     
  if (v < 0) v = -1;
  if (v > 0) v = 1;      
      
  if (x == 0 || v == 0 || x != v) goto got_overlap;      
  return x;   
   
got_overlap:     
  m = "CASEs at %L and %L overlap";        
        
error:        
  if (!overlap) {   
    g95_error(m, &u->where, &i->where);         
    overlap = 1;       
  } 
 
  /* Because we've generated an error, no code will be generated, and
   * the order of the case array no longer matters.  Return something
   * to keep qsort() happy. */   
   
  return 1; 
}  
  
  
 
 
/* free_case()-- Free a single case structure. */ 
 
static void free_case(g95_case *d) {     
     
  g95_free_expr(d->low);
  if (d->high != d->low) g95_free_expr(d->high);

  g95_free(d);       
}       
       
       
 
 
/* match_case_selector()-- Match a single case selector. */    
    
static match match_case_selector(g95_case **cp) {         
g95_case *k; 
match e;

  k = g95_get_case();         
  k->where = *g95_current_locus(); 
 
  if (g95_match_char(':') == MATCH_YES) {  
    e = g95_match_expr(&k->high);       
    if (e == MATCH_NO) goto need_expr;    
    if (e == MATCH_ERROR) goto cleanup;

    if (k->high->ts.type == BT_LOGICAL) goto logical_range;          
    goto done;        
  }          
          
  e = g95_match_expr(&k->low);          
  if (e == MATCH_ERROR) goto cleanup;    
  if (e == MATCH_NO) goto need_expr;        
        
  if (g95_match_char(':') != MATCH_YES)        
    k->high = k->low;      /* Make a range out of a single target */       
  else {  
    e = g95_match_expr(&k->high);        
    if (e == MATCH_ERROR) goto cleanup;
    if (e == MATCH_NO) goto done;   /* It's OK if nothing is there! */  
  
    if (k->high->ts.type == BT_LOGICAL) goto logical_range;       
  } 
 
done:         
  *cp = k;     
  return MATCH_YES;  
  
logical_range:          
  g95_error("Logical range in CASE statement at %C not allowed");         
  goto cleanup;      
      
need_expr:       
  g95_error("Expected expression in CASE at %C");     
     
cleanup: 
  free_case(k);       
  return MATCH_ERROR;       
}


          
          
/* match_case_eos()-- Match the end of a case statement */    
    
static match match_case_eos(void) {         
char nam[G95_MAX_SYMBOL_LEN+1];          
match m;      
      
  if (g95_match_eos() == MATCH_YES) return MATCH_YES;   
   
  g95_gobble_whitespace();          
          
  m = g95_match_name(nam);    
  if (m != MATCH_YES) return m;     
     
  if (strcmp(nam, g95_current_block()->name) != 0) { 
    g95_error("Expected case name of '%s' at %C", g95_current_block()->name);      
    return MATCH_ERROR;  
  }        
        
  return g95_match_eos();
} 
 
 
      
      
/* check_case_expr()-- Check to see if an expression is suitable for
 * use in a CASE statement. */  
  
static try check_case_expr(g95_expr *a, g95_expr *selector) {          
          
  if (a == NULL) return SUCCESS;     
     
  if (a->type != EXPR_CONSTANT) {      
    g95_error("Expression in CASE statement at %L must be a constant",       
	      &a->where); 
    return FAILURE;     
  }       
       
  if (a->ts.type != selector->ts.type) {    
    g95_error("Expression in CASE statement at %L must be of type %s",
	      &a->where, g95_basic_typename(selector->ts.type));       
    return FAILURE;        
  }      
      
  if (a->ts.kind != selector->ts.kind) {      
    g95_error("Expression in CASE statement at %L must be kind %d",         
	      &a->where, selector->ts.kind);  
    return FAILURE;        
  }      
      
  if (a->rank != 0) {         
    g95_error("Expression in CASE statement at %L must be scalar",  
	      &a->where);   
    return FAILURE; 
  } 
 
  return SUCCESS;        
}      
      
      
      
      
/* g95_free_case_list()-- Free a list of case structures */         
         
void g95_free_case_list(g95_case *e) {       
g95_case *a;

  for(;e ;e=a) {          
    a = e->next;        
    free_case(e);         
  }    
}         
         
         
   
   
/* g95_match_case()-- Match a CASE statement */       
       
match g95_match_case(void) {         
g95_case *w, *head, *tail;
match k;    
    
  head = tail = NULL;       
       
  if (g95_current_state() != COMP_SELECT) {         
    g95_error("Unexpected CASE statement at %C");          
    return MATCH_ERROR;        
  }       
       
  if (g95_match("% default") == MATCH_YES) {    
    k = match_case_eos();
    if (k == MATCH_NO) goto syntax;       
    if (k == MATCH_ERROR) goto cleanup;         
         
    new_st.type = EXEC_SELECT;         
    new_st.ext.case_list = g95_get_case();     
    return MATCH_YES; 
  }        
        
  if (g95_match_char('(') != MATCH_YES) goto syntax;         
         
  for(;;) {      
    if (match_case_selector(&w) == MATCH_ERROR) goto cleanup;        
        
    /* Cases that can never be matched are legal to have but mess up
     * code generation, so we discard them here. */    
    
    if (w->low != NULL && w->high != NULL && w->low != w->high &&      
	g95_compare_expr(w->low, w->high) > 0) {      
      g95_warning("Range specification at %C can never be matched");
      free_case(w);   
    } else {        
      if (head == NULL)   
	head = w;      
      else         
	tail->next = w;       
       
      tail = w;  
    }       
       
    if (g95_match_char(')') == MATCH_YES) break;  
    if (g95_match_char(',') != MATCH_YES) goto syntax;  
  }  
  
  k = match_case_eos();     
  if (k == MATCH_NO) goto syntax;     
  if (k == MATCH_ERROR) goto cleanup; 
 
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
g95_case *c, **ap;     
g95_code *body;       
g95_expr *e2;  
int g, e;    
try a;       
       
  if (code->expr == NULL) {    
    /* This was originally a computed GOTO statement.  */      
    e2 = code->expr2;      
      
    if (e2->ts.type != BT_INTEGER || e2->rank != 0)  
      g95_error("Selection expression in COMPUTED GOTO statement "     
                "at %L must be a scalar integer expression",     
                &e2->where);       
    return;         
  }      
      
  e2 = code->expr;         
             
  if (e2->ts.type == BT_DERIVED || e2->ts.type == BT_REAL ||      
      e2->ts.type == BT_COMPLEX) {
    g95_error("Argument of SELECT statement at %L cannot be %s",   
               &e2->where, g95_typename(&e2->ts));    
    return; /* Going on here just produce more garbage error messages.  */ 
  }          
          
  if (e2->rank != 0) {          
    g95_error("Argument of SELECT statement at %L must be a scalar "       
              "expression", &e2->where);
    return;   
  }

  a = SUCCESS;
  e = 0;    
    
  for(body=code->block; body; body=body->block) {     
    for(c=body->ext.case_list; c; c=c->next) {  
      c->code = body->next;          
      e++;    
    
      if (check_case_expr(c->low, e2) == FAILURE) {   
	a = FAILURE;
	break;     
      }    
    
      if (check_case_expr(c->high, e2) == FAILURE) {        
	a = FAILURE;   
	break;  
      }        
        
      if (e2->ts.type == BT_LOGICAL &&
	  (c->low != NULL || c->high != NULL) &&      
	  (c->low == NULL || c->high == NULL)) {      
      
	g95_error("Logical range in CASE statement at %L is not allowed",   
		  &c->low->where);        
	a = FAILURE;      
      }   
    }         
         
    if (a == FAILURE) break;        
  }        
        
  if (a == FAILURE || e == 0) return;      
      
  ap = g95_getmem(e*sizeof(g95_case *));      
  e = 0;        
        
  for(body=code->block; body; body=body->block)   
    for(c=body->ext.case_list; c; c=c->next) 
      ap[e++] = c; 
 
  overlap = 0;

  qsort(ap, e, sizeof(g95_case *), compare_case);          
          
  if (overlap) return;      
      
  /* String the case structures together in a doubly linked list */          
          
  ap[0]->cprev = NULL;          
  ap[0]->cnext = (e > 1) ? ap[1] : NULL; 
 
  if (e > 1) {          
    for(g=1; g<e-1; g++) {          
      ap[g]->cprev = ap[g-1];         
      ap[g]->cnext = ap[g+1];       
    }      
      
    ap[e-1]->cprev = ap[e-2];      
    ap[e-1]->cnext = NULL;      
  }         
         
  g95_free(ap);
}      
