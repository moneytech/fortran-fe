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
      
      
   
   
/* g95_match_name()-- See if the current input looks like a name of
 * some sort.  Modifies the passed buffer which must be
 * G95_MAX_SYMBOL_LEN+1 bytes long. */       
       
match g95_match_name(char *buffer) {    
locus old_loc;      
int a, z;     
     
  old_loc = *g95_current_locus();    
  g95_gobble_whitespace();          
          
  z = g95_next_char();  
  if (!isalpha(z)) {      
    g95_set_locus(&old_loc);        
    return MATCH_NO; 
  }

  a = 0;  
  
  do {
    buffer[a++] = z;     
     
    if (a > G95_MAX_SYMBOL_LEN) {   
      g95_error("Name at %C is too long");    
      return MATCH_ERROR;     
    }         
         
    old_loc = *g95_current_locus();     
    z = g95_next_char();  
  } while(isalnum(z) || z == '_' || (g95_option.dollar && z == '$')); 
 
  buffer[a] = '\0';         
  g95_set_locus(&old_loc);       
       
  return MATCH_YES;    
}


    
    
/* g95_match_iterator()-- Match a loop control phrase:
 *
 *     <LVALUE> = <EXPR>, <EXPR> [, <EXPR> ]
 *
 * If the final integer expression is not present, a constant unity
 * expression is returned.  We don't return MATCH_ERROR until after
 * the equals sign is seen. */   
   
match g95_match_iterator(g95_iterator *iter, int init_flag) {     
char name[G95_MAX_SYMBOL_LEN+1];         
g95_expr *var, *j, *y, *e3;         
locus start;        
match z;          
          
  /* Match the start of an iterator without affecting the symbol table */ 
 
  start = *g95_current_locus();        
  z = g95_match(" %n =", name); 
  g95_set_locus(&start);     
     
  if (z != MATCH_YES) return MATCH_NO;    
    
  z = g95_match_variable(&var, 0);    
  if (z != MATCH_YES) return MATCH_NO;  
  
  g95_match_char('=');         
         
  j = y = e3 = NULL;

  if (var->ref != NULL) {        
    g95_error("Loop variable at %C cannot be a sub-component");       
    goto cleanup;  
  }         
         
  if (var->symbol->attr.intent == INTENT_IN) {    
    g95_error("Loop variable '%s' at %C cannot be INTENT(IN)", 
	      var->symbol->name);         
    goto cleanup;    
  }

  if (var->symbol->attr.pointer) {          
    g95_error("Loop variable at %C cannot have the POINTER attribute");      
    goto cleanup;   
  }         
         
  z = init_flag ? g95_match_init_expr(&j) : g95_match_expr(&j);    
  if (z == MATCH_NO) goto syntax;  
  if (z == MATCH_ERROR) goto cleanup;

  if (g95_match_char(',') != MATCH_YES) goto syntax;       
       
  z = init_flag ? g95_match_init_expr(&y) : g95_match_expr(&y);         
  if (z == MATCH_NO) goto syntax;   
  if (z == MATCH_ERROR) goto cleanup;    
    
  if (g95_match_char(',') != MATCH_YES) {  
    e3 = g95_int_expr(1);      
    goto done; 
  }      
      
  z = init_flag ? g95_match_init_expr(&e3) : g95_match_expr(&e3); 
  if (z == MATCH_ERROR) goto cleanup;          
  if (z == MATCH_NO) {      
    g95_error("Expected a step value in iterator at %C");   
    goto cleanup;     
  }

done: 
  iter->var = var; 
  iter->start = j;
  iter->end = y;        
  iter->step = e3;       
  return MATCH_YES; 
 
syntax:        
  g95_error("Syntax error in iterator at %C");     
        
cleanup:         
  g95_free_expr(j);
  g95_free_expr(y);     
  g95_free_expr(e3);    
    
  return MATCH_ERROR;          
}       
       
       
 
 
/* g95_match_st_label()-- Matches a statement label.  Uses
 * g95_match_small_literal_int() to do most of the work. */     
     
match g95_match_st_label(g95_st_label **label, int allow_zero) {     
int label_value;       
locus old_loc;      
match r; 
 
  old_loc = *g95_current_locus();       
       
  r = g95_match_small_literal_int(&label_value); 
  if (r != MATCH_YES) return r;     
     
  if ((label_value == 0 && allow_zero) || label_value <= 99999) {
    *label = g95_get_st_label(label_value);    
    return MATCH_YES;          
  }     
     
  g95_error("Statement label at %C is out of range");   
  g95_set_locus(&old_loc);       
  return MATCH_ERROR; 
}  
  
  
     
     
/* var_list()-- Match a list of variables terminated by an iterator
 * and a right paren. */    
    
static match var_list(g95_data_variable *parent) {
g95_data_variable *tail, var; 
match t;   
   
  t = var_element(&var);  
  if (t == MATCH_ERROR) return MATCH_ERROR;         
  if (t == MATCH_NO) goto syntax;  
  
  tail = g95_get_data_variable();   
  *tail = var; 
 
  parent->list = tail;       
       
  for(;;) { 
    if (g95_match_char(',') != MATCH_YES) goto syntax; 
 
    t = g95_match_iterator(&parent->iter, 0);  
    if (t == MATCH_YES) break;         
    if (t == MATCH_ERROR) return MATCH_ERROR;

    t = var_element(&var);        
    if (t == MATCH_ERROR) return MATCH_ERROR;  
    if (t == MATCH_NO) goto syntax;      
      
    tail->next = g95_get_data_variable();    
    tail = tail->next;

    *tail = var;     
  }      
      
  if (g95_match_char(')') != MATCH_YES) goto syntax;         
  return MATCH_YES;       
       
syntax:
  g95_syntax_error(ST_DATA);        
  return MATCH_ERROR;   
}         
         
         
 
 
/* g95_match_namelist()-- Match a NAMELIST statement */     
     
match g95_match_namelist(void) { 
g95_symbol *group_name, *s;   
g95_namelist *nl;      
match u, n;       
       
  u = g95_match(" / %s /", &group_name);          
  if (u == MATCH_NO) goto syntax; 
  if (u == MATCH_ERROR) goto error;     
     
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
      u = g95_match_symbol(&s, 1);      
      if (u == MATCH_NO) goto syntax;        
      if (u == MATCH_ERROR) goto error;

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
     
      u = g95_match_char(',');  
  
      if (g95_match_char('/') == MATCH_YES) {          
	n = g95_match(" %s /", &group_name);          
	if (n == MATCH_YES) break;        
	if (n == MATCH_ERROR) goto error;      
	goto syntax;      
      }   
   
      if (u != MATCH_YES) goto syntax;          
    }  
  }    
    
done:          
  return MATCH_YES; 
 
syntax:      
  g95_syntax_error(ST_NAMELIST);  
  
error: 
  return MATCH_ERROR;      
}    
    
    
        
        
/* var_element()-- Match a single element in a data variable list,
 * which can be a variable-iterator list. */    
    
static match var_element(g95_data_variable *new) {   
match y;      
      
  memset(new, '\0', sizeof(g95_data_variable));

  if (g95_match_char('(') == MATCH_YES) return var_list(new);        
        
  y = g95_match_variable(&new->expr, 0);      
  if (y != MATCH_YES) return y;         
         
  if (new->expr->symbol->value != NULL) {  
    g95_error("Variable '%s' at %C already has an initialization",  
	      new->expr->symbol->name);          
    return MATCH_ERROR;    
  }   
   
  new->expr->symbol->attr.data = 1;     
  return MATCH_YES; 
}         
         
         
   
   
/* g95_match_where()-- Match a WHERE statement */ 
 
match g95_match_where(g95_statement *st) {    
g95_expr *expr;   
match m0, i;
g95_code *z;       
       
  m0 = g95_match_label();   
  if (m0 == MATCH_ERROR) return m0;   
   
  i = g95_match(" where ( %e )", &expr);    
  if (i != MATCH_YES) return i;          
          
  if (g95_match_eos() == MATCH_YES) {    
    *st = ST_WHERE_BLOCK;         
         
    new_st.type = EXEC_WHERE;        
    new_st.expr = expr;     
    return MATCH_YES;        
  }     
     
  i = g95_match_assignment();   
  if (i == MATCH_NO) g95_syntax_error(ST_WHERE);  
  
  if (i != MATCH_YES) {    
    g95_free_expr(expr);  
    return MATCH_ERROR;  
  }

/* We've got a simple WHERE statement */

  *st = ST_WHERE;     
  z = g95_get_code();     
     
  z->type = EXEC_WHERE;  
  z->expr = expr;     
  z->next = g95_get_code();      
      
  *z->next = new_st;      
  g95_clear_new_st();      
      
  new_st.type = EXEC_WHERE;     
  new_st.block = z;   
   
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
 
int g95_match_strings(mstring *a) {         
int no_match, q, possibles;  
mstring *p, *best_match;    
locus match_loc;  
  
  possibles = 0;          
          
  for(p=a; p->string != NULL; p++) {          
    p->mp = p->string; 
    possibles++;      
  }        
        
  no_match = p->tag;          
          
  best_match = NULL;      
  match_loc = *g95_current_locus();     
     
  g95_gobble_whitespace();    
    
  while(possibles > 0) {       
    q = g95_next_char(); 
 
/* Apply the next character to the current possibilities */  
  
    for(p=a; p->string!=NULL; p++) {          
      if (p->mp == NULL) continue;         
         
      if (*p->mp == ' ') {    /* Space matches 1+ whitespace(s) */       
       
	if ((g95_current_file->form == FORM_FREE) &&      
	      g95_is_whitespace(q)) continue;  
  
	p->mp++; 
      }    
    
      if (*p->mp != q) {      /* Match failed */    
	p->mp = NULL;  
	possibles--;  
	continue;        
      }

      p->mp++;
      if (*p->mp == '\0') {   /* Found a match */      
	match_loc = *g95_current_locus();         
	best_match = p;   
	possibles--;          
	p->mp = NULL;          
      }   
    }   
  }

  g95_set_locus(&match_loc);  
  
  return (best_match == NULL) ? no_match : best_match->tag;
} 
 
 
          
          
/* g95_match_small_literal_int()-- Match a literal integer on the
 * input, setting the value on MATCH_YES.  Literal integers occur in
 * kind-parameters as well as old-style character length
 * specifications. */    
    
match g95_match_small_literal_int(int *value) {   
locus old_loc;        
char q; 
int o;          
          
  old_loc = *g95_current_locus();      
      
  g95_gobble_whitespace();     
  q = g95_next_char();   
   
  if (!isdigit(q)) {      
    g95_set_locus(&old_loc);      
    return MATCH_NO;
  }        
        
  o = q - '0'; 
 
  for(;;) {   
    old_loc = *g95_current_locus();   
    q = g95_next_char();      
      
    if (!isdigit(q)) break;    
    
    o = 10*o + q - '0';    
    
    if (o > 99999999) {   
      g95_error("Integer too large at %C");    
      return MATCH_ERROR;   
    }        
  }       
       
  g95_set_locus(&old_loc);    
    
  *value = o;     
  return MATCH_YES;         
}          
          
          
         
         
/* g95_match_call()-- Match a CALL statement.  The tricky part here
 * are possible alternate return specifiers.  We handle these by
 * having all "subroutines" actually return an integer via a register
 * that gives the return number.  If the call specifies alternate
 * returns, we generate code for a SELECT statement whose case clauses
 * contain GOTOs to the various labels. */      
      
match g95_match_call(void) {  
char name[G95_MAX_SYMBOL_LEN+1];     
g95_actual_arglist *v, *arglist;
g95_case *new_case;      
g95_symbol *symb;        
g95_code *h;     
match m;     
int s;       
       
  arglist = NULL;   
   
  m = g95_match("% %n", name);    
  if (m == MATCH_NO) goto syntax;         
  if (m != MATCH_YES) return m;  
  
  if (g95_get_ha_symbol(name, &symb)) return MATCH_ERROR;       
       
  if (!symb->attr.generic && !symb->attr.subroutine &&  
      g95_add_subroutine(&symb->attr, NULL) == FAILURE)
    return MATCH_ERROR;      
      
  if (g95_match_eos() != MATCH_YES) {        
    m = g95_match_actual_arglist(1, &arglist);        
    if (m == MATCH_NO) goto syntax; 
    if (m == MATCH_ERROR) goto cleanup;      
      
    if (g95_match_eos() != MATCH_YES) goto syntax;  
  }     
     
/* If any alternate return labels were found, construct a SELECT
 * statement that will jump to the right place */      
      
  if (g95_has_alt_return(arglist)) {     
    new_st.next = h = g95_get_code();      
    h->type = EXEC_SELECT;  
    h->expr = g95_int_expr(0);  /* For now */        
        
    s = 0; 
    for(v=arglist; v; v=v->next) {
      if (v->type != ALT_RETURN) continue;  
  
      if (g95_reference_st_label(v->u.label, ST_LABEL_TARGET) == FAILURE)         
        continue;     
     
      s++;       
       
      h->block = g95_get_code();          
      h = h->block;      
      h->type = EXEC_SELECT;

      new_case = g95_get_case();         
      new_case->high = new_case->low = g95_int_expr(s);       
      h->ext.case_list = new_case;        
        
      h->next = g95_get_code();    
      h->next->type = EXEC_GOTO; 
      h->next->label = v->u.label;          
    }
  }     
     
  new_st.type = EXEC_CALL; 
  new_st.sym = symb;        
  new_st.ext.actual = arglist;        
  return MATCH_YES;   
   
syntax:    
  g95_syntax_error(ST_CALL);          
          
cleanup:       
  g95_free_actual_arglist(arglist);      
  return MATCH_ERROR;         
}   
   
   
    
    
/* g95_match_block_data()-- Match a BLOCK DATA program unit */  
  
match g95_match_block_data(void) {        
char name[G95_MAX_SYMBOL_LEN+1]; 
g95_symbol *s;    
match h;       
       
  if (g95_match_eos() == MATCH_YES) {          
    g95_new_block = NULL;
    return MATCH_YES;  
  }           
        
  h = g95_match(" %n%t", name); 
  if (h != MATCH_YES) return MATCH_ERROR;   
   
  if (g95_get_symbol(name, NULL, &s)) return MATCH_ERROR;  
  
  if (g95_add_flavor(&s->attr, FL_BLOCK_DATA, NULL) == FAILURE)      
    return MATCH_ERROR;

  g95_new_block = s;     
     
  return MATCH_YES; 
}     
     
     
          
          
/* free_value()-- Free a g95_data_value structure and everything
 * beneath it */       
       
static void free_value(g95_data_value *u) {
g95_data_value *r;

  for(; u; u=r) {   
    r = u->next;          
    g95_free_expr(u->expr);     
    g95_free(u);      
  } 
}   
   
   
     
     
/* g95_match_char()-- Tries to match the next non-whitespace character
 * on the input.  This subroutine does not return MATCH_ERROR.  */          
          
match g95_match_char(char m) {          
locus where;    
    
  where = *g95_current_locus();  
  g95_gobble_whitespace();    
    
  if (g95_next_char() == m) return MATCH_YES;  
  
  g95_set_locus(&where);
  return MATCH_NO;     
}   
   
   
   
   
/* match_common_name()-- Match a common block name.  */      
      
static match match_common_name(g95_symbol **symb) {        
match q;         
         
  if (g95_match_char('/') == MATCH_NO) return MATCH_NO;

  if (g95_match_char('/') == MATCH_YES) {         
    *symb = NULL;  
    return MATCH_YES;  
  }  
  
  q = g95_match_symbol(symb, 0);      
      
  if (q == MATCH_ERROR) return MATCH_ERROR;     
  if (q == MATCH_YES && g95_match_char('/') == MATCH_YES) return MATCH_YES;       
       
  g95_error("Syntax error in common block name at %C");      
  return MATCH_ERROR;    
}   
   
   
         
         
/* g95_match_label()-- Match and validate a label associated with a
 * named IF, DO or SELECT statement.  If the symbol does not have the
 * label attribute, we add it.  We also make sure the symbol does not
 * refer to another (active) block.  A matched label is pointed to by
 * g95_new_block. */   
   
match g95_match_label(void) { 
char name[G95_MAX_SYMBOL_LEN+1];
g95_state_data *p;          
match y;  
  
  g95_new_block = NULL;         
         
  y = g95_match(" %n :", name);      
  if (y != MATCH_YES) return y;       
       
  if (g95_get_symbol(name, NULL, &g95_new_block)) { 
    g95_error("Label name '%s' at %C is ambiguous", name);          
    return MATCH_ERROR;         
  }

  if (g95_new_block->attr.flavor != FL_LABEL &&   
      g95_add_flavor(&g95_new_block->attr, FL_LABEL, NULL) == FAILURE)         
    return MATCH_ERROR;  
  
  for(p=g95_state_stack; p; p=p->previous)       
    if (p->sym == g95_new_block) {   
      g95_error("Label %s at %C already in use by a parent block",  
		g95_new_block->name);   
      return MATCH_ERROR;   
    }     
     
  return MATCH_YES;     
}     
     
     
 
 
/* g95_match_assign()-- Match the (deprecated) ASSIGN statement. */   
   
match g95_match_assign(void) {          
g95_expr *expr;   
int label;     
     
  if (g95_match(" %l to %v%t", &label, &expr) == MATCH_YES) {        
    g95_free_expr(expr);        
    g95_error("The ASSIGN statement at %C is not allowed in Fortran 95");  
    return MATCH_ERROR;   
  }          
          
  return MATCH_NO;  
}  
  
  
     
     
/* top_var_list()-- Match the top-level list of data variables */          
          
static match top_var_list(g95_data *y) {
g95_data_variable var, *tail, *new;     
match n;

  tail = NULL;      
      
  for(;;) {         
    n = var_element(&var);  
    if (n == MATCH_NO) goto syntax;     
    if (n == MATCH_ERROR) return MATCH_ERROR; 
 
    new = g95_get_data_variable();         
    *new = var;       
       
    if (tail == NULL)         
      y->var = new;    
    else        
      tail->next = new;         
         
    tail = new;

    if (g95_match_char('/') == MATCH_YES) break;     
    if (g95_match_char(',') != MATCH_YES) goto syntax;   
  }     
     
  return MATCH_YES;        
        
syntax:          
  g95_syntax_error(ST_DATA);  
  return MATCH_ERROR;    
}          
          
          


/* g95_free_alloc_list()-- Frees a list of g95_alloc structures */          
          
void g95_free_alloc_list(g95_alloc *g) {
g95_alloc *m;  
  
  for(; g; g=m) {  
    m = g->next;     
    g95_free_expr(g->expr);      
    g95_free(g);      
  }   
}     
     
     
    
    
/* g95_match_common()-- Match a COMMON statement */

match g95_match_common(void) {
g95_symbol *s, *common_name, **head, *tail, *old_blank_common;  
g95_array_spec *as;    
match j;       
       
  old_blank_common = g95_current_ns->blank_common;
  if (old_blank_common) {  
    while(old_blank_common->common_next)         
      old_blank_common = old_blank_common->common_next;
  }  
  
  common_name = NULL;         
  as = NULL;  
  
  if (g95_match_eos() == MATCH_YES) goto syntax;         
         
  for(;;) {        
    j = match_common_name(&common_name);    
    if (j == MATCH_ERROR) goto cleanup;        
        
    if (common_name == NULL)      
      head = &g95_current_ns->blank_common;
    else {         
      head = &common_name->common_head;         
               
      if (!common_name->attr.common &&        
	  g95_add_common(&common_name->attr, NULL) == FAILURE)
	goto cleanup;  
    }          
          
    if (*head == NULL)  
      tail = NULL;   
    else {    
      tail = *head;
      while(tail->common_next)     
	tail = tail->common_next;       
    }        
        
/* Grab the list of symbols */        
        
    for(;;) {      
      j = g95_match_symbol(&s, 0);   
      if (j == MATCH_ERROR) goto cleanup;
      if (j == MATCH_NO) goto syntax;       
       
      if (s->attr.in_common) {          
	g95_error("Symbol '%s' at %C is already in a COMMON block", s->name); 
	goto cleanup;     
      }        
        
      if (g95_add_in_common(&s->attr, NULL) == FAILURE) goto cleanup;  
  
      if (tail != NULL)        
	tail->common_next = s;  
      else        
	*head = s;  
  
      tail = s;     
     
/* Deal with an optional array specification after the symbol name */          
          
      j = g95_match_array_spec(&as);      
      if (j == MATCH_ERROR) goto cleanup; 
      if (j == MATCH_YES) {       
	if (g95_add_dimension(&s->attr, NULL) == FAILURE) goto cleanup;       
       
	s->as = as;  
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
  if (old_blank_common)       
    old_blank_common->common_next = NULL;       
  else 
    g95_current_ns->blank_common = NULL;    
  g95_free_array_spec(as);    
  return MATCH_ERROR;         
}        
        
        
 
 
/* g95_free_equiv()-- Free equivalence sets and lists.  Recursively is
 * the easiest way to do this. */         
         
void g95_free_equiv(g95_equiv *eq) {          
          
  if (eq == NULL) return;         
         
  g95_free_equiv(eq->eq);         
  g95_free_equiv(eq->next);

  g95_free_expr(eq->expr);
  g95_free(eq);     
}          
          
          
    
    
/* g95_match_do()-- Match a DO statement */ 
 
match g95_match_do(void) {          
g95_expr *while_condition;     
g95_iterator iter, *ip;
g95_st_label *label;
locus old_loc;   
match t; 
 
  old_loc = *g95_current_locus();   
   
  label = NULL;      
  iter.var = iter.start = iter.end = iter.step = NULL;        
  while_condition = NULL;

  t = g95_match_label();  
  if (t == MATCH_ERROR) return t; 
 
  if (g95_match(" do") != MATCH_YES) return MATCH_NO;         
         
/* Match an infinite DO, make it like a DO WHILE(.TRUE.) */  
  
  if (g95_match_eos() == MATCH_YES) {    
    new_st.type = EXEC_DO_WHILE;     
    goto done;        
  }   
   
  t = g95_match_st_label(&label, 0);          
  if (t == MATCH_ERROR) goto cleanup; 
 
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
  g95_set_locus(&old_loc); 
 
  g95_match_label();    /* This won't error */    
  g95_match(" do ");    /* This will work */          
          
  g95_match_st_label(&label, 0);  /* Can't error out */ 
  g95_match_char(',');            /* Optional comma */          
          
  t = g95_match_iterator(&iter, 0);       
  if (t == MATCH_NO) return MATCH_NO;     
  if (t == MATCH_ERROR) goto cleanup;   
   
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
    new_st.ext.iterator = ip = g95_get_iterator();  
    *ip = iter;  
  }          
          
  return MATCH_YES;   
   
cleanup: 
  g95_free_iterator(&iter, 0);       
  return MATCH_ERROR;     
}   
   
   
       
       
/* g95_match_pointer_assignment()-- Match a pointer assignment statement */   
   
match g95_match_pointer_assignment(void) {        
g95_expr *lvalue, *rvalue;    
locus old_loc;     
match b;

  old_loc = *g95_current_locus(); 
 
  lvalue = rvalue = NULL;    
    
  b = g95_match(" %v =>", &lvalue);  
  if (b != MATCH_YES) { b = MATCH_NO; goto cleanup; }

  b = g95_match(" %e%t", &rvalue);       
  if (b != MATCH_YES) goto cleanup;     
     
  new_st.type = EXEC_POINTER_ASSIGN; 
  new_st.expr = lvalue;      
  new_st.expr2 = rvalue;          
          
  return MATCH_YES; 
 
cleanup: 
  g95_set_locus(&old_loc);        
  g95_free_expr(lvalue);     
  g95_free_expr(rvalue);
  return b;     
}   
   
   
     
     
/* g95_free_namelist()-- Free a namelist structure */       
       
void g95_free_namelist(g95_namelist *name) {      
g95_namelist *l;    
    
  for(;name; name=l) {
    l = name->next;        
    g95_free(name);     
  }     
}  
  
  


/* g95_match_symbol()-- Match a symbol on the input.  Modifies the
 * pointer to the symbol pointer if successful. */ 
 
match g95_match_symbol(g95_symbol **matched_symbol, int host_assoc) {          
char buffer[G95_MAX_SYMBOL_LEN+1];     
match c;    
    
  c = g95_match_name(buffer);   
  if (c != MATCH_YES) return c;

  if (host_assoc)          
    return (g95_get_ha_symbol(buffer, matched_symbol))          
      ? MATCH_ERROR : MATCH_YES;     
     
  if (g95_get_symbol(buffer, NULL, matched_symbol)) return MATCH_ERROR;        
        
  return MATCH_YES;  
}          
          
          
       
       
/* g95_match_intrinsic_op()-- Match an intrinsic operator.  Returns an
 * INTRINSIC enum. */         
         
match g95_match_intrinsic_op(g95_intrinsic_op *result) {  
g95_intrinsic_op op;      
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
       
  op = g95_match_strings(operators_in);         
  if (op == INTRINSIC_NONE) return MATCH_NO;  
  
  *result = op;         
  return MATCH_YES;
}       
       
       
char *g95_op2string(int c) {    
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
     
  if (c == INTRINSIC_ASSIGN) return "=";
  return g95_code2string(operators_out, c);          
}   
   
   
   
   
/* g95_match_deallocate()-- Match a DEALLOCATE statement */      
      
match g95_match_deallocate(void) {    
g95_alloc *head, *tail;          
g95_expr *stat; 
match d;       
       
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
 
    d = g95_match_variable(&tail->expr, 0);      
    if (d == MATCH_ERROR) goto cleanup;   
    if (d == MATCH_NO) goto syntax;        
        
    if (g95_pure(NULL) && g95_impure_variable(tail->expr->symbol)) { 
      g95_error("Illegal deallocate-expression in DEALLOCATE at %C for a PURE "       
		"procedure");        
      goto cleanup;         
    }      
      
    if (g95_match_char(',') != MATCH_YES) break;         
         
    d = g95_match(" stat = %v", &stat);      
    if (d == MATCH_ERROR) goto cleanup;  
    if (d == MATCH_YES) break;  
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
  new_st.ext.alloc_list = head;       
       
  return MATCH_YES;  
  
syntax:        
  g95_syntax_error(ST_DEALLOCATE);          
          
cleanup:       
  g95_free_expr(stat);         
  g95_free_alloc_list(head); 
  return MATCH_ERROR;       
}         
         
         
     
     
/* g95_match_allocate()-- Match an ALLOCATE statement */   
   
match g95_match_allocate(void) { 
g95_alloc *head, *tail;       
g95_expr *stat;      
match e;      
      
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

    e = g95_match_variable(&tail->expr, 0);       
    if (e == MATCH_NO) goto syntax;   
    if (e == MATCH_ERROR) goto cleanup;     
     
    if (g95_pure(NULL) && g95_impure_variable(tail->expr->symbol)) {
      g95_error("Bad allocate-object in ALLOCATE statement at %C for a "       
		"PURE procedure");
      goto cleanup;    
    }         
         
    if (g95_match_char(',') != MATCH_YES) break;       
       
    e = g95_match(" stat = %v", &stat); 
    if (e == MATCH_ERROR) goto cleanup;     
    if (e == MATCH_YES) break; 
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
       
       
          
          
/* match_forall_iterator()-- Match an iterator as part of a FORALL
 * statement.  The format is:
 *     <var> = <start>:<end>[:<stride>][, <scalar mask>]  */      
      
static match match_forall_iterator(g95_forall_iterator **result) {  
g95_forall_iterator *iter;
locus where;        
match e;  
  
  where = *g95_current_locus();     
  iter = g95_getmem(sizeof(g95_forall_iterator));          
          
  e = g95_match_variable(&iter->var, 0);          
  if (e != MATCH_YES) goto cleanup;    
    
  if (g95_match_char('=') != MATCH_YES) {   
    e = MATCH_NO;       
    goto cleanup;         
  }  
  
  e = g95_match_expr(&iter->start);          
  if (e == MATCH_NO) goto syntax;       
  if (e == MATCH_ERROR) goto cleanup;     
     
  if (g95_match_char(':') != MATCH_YES) goto syntax;         
         
  e = g95_match_expr(&iter->end);         
  if (e == MATCH_NO) goto syntax;      
  if (e == MATCH_ERROR) goto cleanup;          
          
  if (g95_match_char(':') == MATCH_NO)     
    iter->stride = g95_int_expr(1);          
  else {  
    e = g95_match_expr(&iter->stride);
    if (e == MATCH_NO) goto syntax;      
    if (e == MATCH_ERROR) goto cleanup;   
  }        
        
  *result = iter;     
  return MATCH_YES;  
  
syntax: 
  g95_error("Syntax error in FORALL iterator at %C");
  e = MATCH_ERROR; 
 
cleanup:     
  g95_set_locus(&where);     
  g95_free_forall_iterator(iter);
  return e;   
}        
        
        
         
         
/* g95_match_else()-- Match an ELSE statement */         
         
match g95_match_else(void) {       
char name[G95_MAX_SYMBOL_LEN+1];          
           
  if (g95_match_eos() == MATCH_YES) return MATCH_YES;    
    
  if (g95_match_name(name) != MATCH_YES || g95_current_block() == NULL ||      
      g95_match_eos() != MATCH_YES) { 
    g95_error("Unexpected junk after ELSE statement at %C");     
    return MATCH_ERROR;          
  }  
  
  if (strcmp(name, g95_current_block()->name) != 0) {       
    g95_error("Label '%s' at %C doesn't match IF label '%s'",  
	      name, g95_current_block()->name);         
    return MATCH_ERROR;    
  }      
      
  return MATCH_YES;  
}     
     
     
      
      
/* g95_match_assignment(void)-- Match a simple assignment statement */

match g95_match_assignment(void) {          
g95_expr *lvalue, *rvalue;
locus old_loc;       
match x;         
         
  old_loc = *g95_current_locus();           
          
  lvalue = rvalue = NULL;          
  x = g95_match(" %v =", &lvalue);  
  if (x != MATCH_YES) goto cleanup;     
     
  x = g95_match(" %e%t", &rvalue);
  if (x != MATCH_YES) goto cleanup;  
  
  new_st.type = EXEC_ASSIGN;       
  new_st.expr = lvalue;  
  new_st.expr2 = rvalue;     
     
  g95_check_do_variable(lvalue->symbol);     
     
  return MATCH_YES;        
        
cleanup:     
  g95_set_locus(&old_loc);     
  g95_free_expr(lvalue);   
  g95_free_expr(rvalue); 
  return x;  
}          
          
          
       
       
/* free_variable()-- Free a g95_data_variable structure and everything
 * beneath it */       
       
static void free_variable(g95_data_variable *i) {   
g95_data_variable *v;     
     
  for(; i; i=v) {   
    v = i->next;   
    g95_free_expr(i->expr);   
    g95_free_iterator(&i->iter, 0);
    free_variable(i->list);      
          
    g95_free(i);        
  } 
}        
        
        
 
 
/* g95_match_eos()-- Match an end of statement.  End of statement is
 * optional whitespace, followed by a ';' or '\n' or comment '!'.  If
 * a semicolon is found, we continue to eat whitespace and semicolons. */   
   
match g95_match_eos(void) {        
locus old_loc;  
int flag, q;       
       
  flag = 0;  
  
  for(;;) {        
    old_loc = *g95_current_locus();
    g95_gobble_whitespace();
   
    q = g95_next_char();    
    switch(q) {        
    case '!':       
      do {         
	q = g95_next_char();      
      } while(q != '\n');   
   
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
g95_expr *expr;       
g95_st_label *u, *l2, *l3; 
locus old_loc;     
g95_code *b;          
match mm, k, c;     
     
  c = g95_match_label();
  if (c == MATCH_ERROR) return c;      
      
  old_loc = *g95_current_locus();        
        
  k = g95_match(" if ( %e", &expr);      
  if (k != MATCH_YES) return k;        
        
  if (g95_match_char(')') != MATCH_YES) {    
    g95_error("Syntax error in IF-expression at %C");        
    g95_free_expr(expr);       
    return MATCH_ERROR;
  }      
      
  k = g95_match(" %l , %l , %l%t", &u, &l2, &l3);          
          
  if (k == MATCH_YES) {    
    if (c == MATCH_YES) {         
      g95_error("Block label not appropriate for arithmetic IF statement "       
		"at %C"); 
 
      g95_free_expr(expr);         
      return MATCH_ERROR;      
    }  
  
    if (g95_reference_st_label(u, ST_LABEL_TARGET) == FAILURE ||        
	g95_reference_st_label(l2, ST_LABEL_TARGET) == FAILURE ||
	g95_reference_st_label(l3, ST_LABEL_TARGET) == FAILURE) {          
          
      g95_free_expr(expr);        
      return MATCH_ERROR;    
    }         
         
    new_st.type = EXEC_ARITHMETIC_IF;   
    new_st.expr = expr;         
    new_st.label = u;        
    new_st.label2 = l2;         
    new_st.label3 = l3;  
  
    *if_type = ST_ARITHMETIC_IF;  
    return MATCH_YES;       
  }

  if (g95_match(" then %t") == MATCH_YES) {       
    new_st.type = EXEC_IF;         
    new_st.expr = expr;      
      
    *if_type = ST_IF_BLOCK;    
    return MATCH_YES;    
  }

  if (c == MATCH_YES) {   
    g95_error("Block label is not appropriate IF statement at %C"); 
 
    g95_free_expr(expr);       
    return MATCH_ERROR;        
  }   
   
/* At this point the only thing left is a simple IF statement.  At
 * this point, n has to be MATCH_NO, so we don't have to worry about
 * re-matching a block label.  From what we've got so far, try
 * matching an assignment. */    
    
  *if_type = ST_SIMPLE_IF;

  k = g95_match_assignment();      
  if (k == MATCH_YES) goto got_match;   
   
  g95_free_expr(expr);         
  g95_undo_symbols();   
  g95_set_locus(&old_loc);     
     
  g95_match(" if ( %e ) ", &expr);  /* Guaranteed to match */       
       
  k = g95_match_pointer_assignment();     
  if (k == MATCH_YES) goto got_match; 
 
  g95_free_expr(expr);   
  g95_undo_symbols();    
  g95_set_locus(&old_loc);    
    
  g95_match(" if ( %e ) ", &expr);  /* Guaranteed to match */         
         
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
        
  g95_free_expr(expr);  
  return MATCH_ERROR;        
        
got_match:     
  if (mm == MATCH_NO) g95_error("Syntax error in IF-clause at %C");      
  if (mm != MATCH_YES) {     
    g95_free_expr(expr);   
    return MATCH_ERROR;        
  }

/* At this point, we've matched the single IF and the action clause is
 * in new_st.  Rearrange things so that the IF statement appears in new_st */      
      
  b = g95_get_code();      
  *b = new_st;         
  b->loc = *g95_current_locus();      
      
  g95_clear_new_st(); 
 
  new_st.type = EXEC_IF;          
  new_st.block = b;       
  new_st.expr = expr; 
 
  return MATCH_YES;          
}  
  
#undef match
         
         
     
     
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
    
    
         
         
/* g95_match_return()-- Match a RETURN statement */      
      
match g95_match_return(void) {      
g95_expr *q;         
match k;   
   
  q = NULL;         
  if (g95_match_eos() == MATCH_YES) goto done;        
        
  if (g95_find_state(COMP_SUBROUTINE) == FAILURE) {          
    g95_error("Alternate RETURN statement at %C is only allowed within "        
	      "a SUBROUTINE"); 
    goto cleanup;         
  }     
     
  k = g95_match("% %e%t", &q);      
  if (k == MATCH_YES) goto done;        
  if (k == MATCH_ERROR) goto cleanup;    
    
  g95_syntax_error(ST_RETURN);       
       
cleanup:          
  g95_free_expr(q);  
  return MATCH_ERROR;  
  
done:
  new_st.type = EXEC_RETURN;       
  new_st.expr = q;    
    
  return MATCH_YES;     
}    
    
    
    
    
/* g95_match_space()-- In free form, match at least one space.  Always
 * matches in fixed form. */       
       
match g95_match_space(void) {
locus old_loc;    
int t;        
        
  if (g95_current_file->form == FORM_FIXED) return MATCH_YES;         
         
  old_loc = *g95_current_locus();          
          
  t = g95_next_char();       
  if (!g95_is_whitespace(t)) {      
    g95_set_locus(&old_loc);      
    return MATCH_NO;  
  } 
 
  g95_gobble_whitespace();       
       
  return MATCH_YES;
}        
        
        


/* g95_match_stop()-- Match the STOP statement.  We can't match a
 * label here because labels can't be zero and a stop code can. */         
         
match g95_match_stop(void) {   
int stop_code;
g95_expr *c;         
match n;     
     
  stop_code = -1;   /* blank stop */          
  c = NULL;    
    
  if (g95_match_eos() != MATCH_YES) {        
    n = g95_match_small_literal_int(&stop_code); 
    if (n == MATCH_ERROR) goto cleanup;     
     
    if (n == MATCH_YES && stop_code > 99999) {          
      g95_error("STOP code out of range at %C"); 
      goto cleanup;        
    }       
       
    if (n == MATCH_NO) {  /* Try a character constant */     
      n = g95_match_expr(&c);        
      if (n == MATCH_ERROR) goto cleanup; 
      if (n == MATCH_NO) goto syntax;  
  
      if (c->ts.type != BT_CHARACTER || c->type != EXPR_CONSTANT)        
	goto syntax;     
    }  
  
    if (g95_match_eos() != MATCH_YES) goto syntax;    
  }          
          
  if (g95_pure(NULL)) {      
    g95_error("STOP statement not allowed in PURE procedure at %C");          
    goto cleanup;   
  }     
     
  new_st.type = EXEC_STOP;        
  new_st.expr = c;     
  new_st.ext.stop_code = stop_code; 
 
  return MATCH_YES;        
        
syntax:          
  g95_syntax_error(ST_STOP);         
         
cleanup:  
  g95_free_expr(c);  
  return MATCH_ERROR;  
}          
          
          
          
          
/* g95_match_elsewhere()-- Match an ELSEWHERE statement.  We leave
 * behind a WHERE node in new_st if successful. */    
    
match g95_match_elsewhere(void) {   
char name[G95_MAX_SYMBOL_LEN+1];     
g95_expr *expr; 
match v;     
     
  if (g95_current_state() != COMP_WHERE) {         
    g95_error("ELSEWHERE statement at %C not enclosed in WHERE block");        
    return MATCH_ERROR;         
  }     
     
  expr = NULL;       
       
  if (g95_match_char('(') == MATCH_YES) {         
    v = g95_match_expr(&expr);          
    if (v == MATCH_NO) goto syntax;      
    if (v == MATCH_ERROR) return MATCH_ERROR;

    if (g95_match_char(')') != MATCH_YES) goto syntax;      
  }   
   
  if (g95_match_eos() != MATCH_YES) { /* Better be a name at this point */    
    v = g95_match_name(name);      
    if (v == MATCH_NO) goto syntax; 
    if (v == MATCH_ERROR) goto cleanup;      
      
    if (g95_match_eos() != MATCH_YES) goto syntax;      
      
    if (strcmp(name, g95_current_block()->name) != 0) {     
      g95_error("Label '%s' at %C doesn't match WHERE label '%s'",        
		name, g95_current_block()->name);   
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
        
        
        
        
/* g95_free_forall_iterator()-- Free a list of FORALL iterators */       
       
void g95_free_forall_iterator(g95_forall_iterator *iter) {       
g95_forall_iterator *next;     
     
  while(iter) { 
    next = iter->next;      
      
    g95_free_expr(iter->var);          
    g95_free_expr(iter->start);
    g95_free_expr(iter->end);  
    g95_free_expr(iter->stride);        
        
    g95_free(iter);         
    iter = next;  
  }      
}  
  
  
       
       
/* g95_match_nullify()-- Match a NULLIFY statement. A NULLIFY
 * statement is transformed into a set of pointer assignments to
 * intrinsic NULL(). */  
  
match g95_match_nullify(void) {          
g95_code *tail; 
g95_expr *u, *q;      
match n; 
 
  tail = NULL; 
 
  if (g95_match_char('(') != MATCH_YES) goto syntax;

  for(;;) {  
    n = g95_match_variable(&q, 0);      
    if (n == MATCH_ERROR) goto cleanup;
    if (n == MATCH_NO) goto syntax;     
     
    if (g95_pure(NULL) && g95_impure_variable(q->symbol)) {    
      g95_error("Illegal variable in NULLIFY at %C for a PURE procedure");  
      goto cleanup; 
    }         
         
    /* build ' => NULL() ' */       
    u = g95_get_expr();        
    u->where = *g95_current_locus();  
    u->type = EXPR_NULL;    
    u->ts.type = BT_UNKNOWN;      
      
    /* Chain to list */    
    if (tail == NULL)       
      tail = &new_st;      
    else {      
      tail->next = g95_get_code();       
      tail = tail->next;        
    }         
         
    tail->type = EXEC_POINTER_ASSIGN;      
    tail->expr = q;     
    tail->expr2 = u; 
 
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
match p;          
          
  p = g95_match_eos();      
  if (p == MATCH_YES) return p;    
    
  p = g95_match("% %s%t", &s);   
   
  if (p == MATCH_NO) {        
    g95_error("Invalid form of PROGRAM statement at %C");
    p = MATCH_ERROR;     
  }  
  
  if (p == MATCH_ERROR) return p;     
     
  if (g95_add_flavor(&s->attr, FL_PROGRAM, NULL) == FAILURE)        
    return MATCH_ERROR;  
  
  g95_new_block = s;          
          
  return MATCH_YES;       
}     
     
     
   
   
/* g95_match_forall()-- Match a FORALL statement */  
  
match g95_match_forall(g95_statement *st) {       
g95_forall_iterator *head, *tail, *new;  
g95_expr *mask;     
g95_code *r;       
match m0, n;

  head = tail = NULL; 
  mask = NULL;        
  r = NULL;  
  
  m0 = g95_match_label();  
  if (m0 == MATCH_ERROR) return MATCH_ERROR;    
    
  n = g95_match(" forall (");    
  if (n != MATCH_YES) return n;  
  
  n = match_forall_iterator(&new);   
  if (n == MATCH_ERROR) goto cleanup;  
  if (n == MATCH_NO) goto syntax;          
          
  head = tail = new;          
          
  for(;;) {      
    if (g95_match_char(',') != MATCH_YES) break;      
      
    n = match_forall_iterator(&new);  
    if (n == MATCH_ERROR) goto cleanup;          
    if (n == MATCH_YES) {     
      tail->next = new;        
      tail = new;    
      continue;
    }  
  
/* Have to have a mask expression */         
         
    n = g95_match_expr(&mask);      
    if (n == MATCH_NO) goto syntax; 
    if (n == MATCH_ERROR) goto cleanup;    
    
    break;       
  }          
          
  if (g95_match_char(')') == MATCH_NO) goto syntax;         
         
  if (g95_match_eos() == MATCH_YES) {
    *st = ST_FORALL_BLOCK;      
      
    new_st.type = EXEC_FORALL;        
    new_st.expr = mask;         
    new_st.ext.forall_iterator = head;  
  
    return MATCH_YES;         
  } 
 
  n = g95_match_assignment();        
  if (n == MATCH_ERROR) goto cleanup;  
  if (n == MATCH_NO) {         
    n = g95_match_pointer_assignment();      
    if (n == MATCH_ERROR) goto cleanup;
    if (n == MATCH_NO) goto syntax;  
  } 
 
  r = g95_get_code();  
  *r = new_st;     
     
  if (g95_match_eos() != MATCH_YES) goto syntax;       
       
  g95_clear_new_st();        
  new_st.type = EXEC_FORALL;
  new_st.expr = mask;      
  new_st.ext.forall_iterator = head;  
  new_st.block = g95_get_code();        
        
  new_st.block->type = EXEC_FORALL;      
  new_st.block->next = r;        
        
  *st = ST_FORALL; 
  return MATCH_YES;     
     
syntax:       
  g95_syntax_error(ST_FORALL); 
 
cleanup:          
  g95_free_forall_iterator(head);   
  g95_free_expr(mask);          
  g95_free_statements(r);      
  return MATCH_NO;       
}    
      
      
/* g95_match_goto()-- Match the GO TO statement.  As a computed GOTO
 * statement is matched, it is transformed into an equivalent SELECT
 * block.  No tree is necessary, and the resulting jumps-to-jumps are
 * specifically optimized away by the back end. */      
      
match g95_match_goto(void) {      
g95_code *head, *tail;     
g95_expr *expr;       
g95_case *cp; 
g95_st_label *label;  
int z; 
match m;    
    
  if (g95_match(" %l%t", &label) == MATCH_YES) { 
    if (g95_reference_st_label(label, ST_LABEL_TARGET) == FAILURE)         
      return MATCH_ERROR;   
   
    new_st.type = EXEC_GOTO;
    new_st.label = label;       
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
 
  head = tail = NULL;        
  z = 1;          
          
  do {         
    m = g95_match_st_label(&label, 0);  
    if (m != MATCH_YES) goto syntax; 
 
    if (g95_reference_st_label(label, ST_LABEL_TARGET) == FAILURE)   
      goto cleanup;       
       
    if (head == NULL)     
      head = tail = g95_get_code();        
    else {    
      tail->block = g95_get_code();      
      tail = tail->block;    
    }          
          
    cp = g95_get_case();      
    cp->low = cp->high = g95_int_expr(z++);

    tail->type = EXEC_SELECT;   
    tail->ext.case_list = cp;   
   
    tail->next = g95_get_code();   
    tail->next->type = EXEC_GOTO;      
    tail->next->label = label;   
  } while(g95_match_char(',') == MATCH_YES);  
  
  if (g95_match_char(')') != MATCH_YES) goto syntax;        
        
  if (head == NULL) {        
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
  new_st.block = head;   
  return MATCH_YES; 
 
syntax: 
  g95_syntax_error(ST_GOTO);       
cleanup:     
  g95_free_statements(head);
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
g95_st_label **label;   
int matches, *ip;        
locus old_loc;       
va_list argp;    
char w, *np;          
match q, n;          
void **vp;
char *r;

  old_loc = *g95_current_locus();
  va_start(argp, target);      
  q = MATCH_NO;     
  matches = 0;         
  r = target;       
       
loop:         
  w = *r++;       
  switch(w) {      
  case ' ':   g95_gobble_whitespace(); goto loop;         
  case '\0':  q = MATCH_YES; break;      
      
  case '%':          
    w = *r++;  
    switch(w) {  
    case 'e':   
      vp = va_arg(argp, void **);      
      n = g95_match_expr((g95_expr **) vp);
      if (n != MATCH_YES) { q = n; goto not_yes; }        
        
      matches++;   
      goto loop;  
  
    case 'v':         
      vp = va_arg(argp, void **);     
      n = g95_match_variable((g95_expr **) vp, 0);       
      if (n != MATCH_YES) { q = n; goto not_yes; }        
        
      matches++;       
      goto loop;   
   
    case 's':  
      vp = va_arg(argp, void **);  
      n = g95_match_symbol((g95_symbol **) vp, 0);       
      if (n != MATCH_YES) { q = n; goto not_yes; }    
    
      matches++;     
      goto loop; 
 
    case 'n':
      np = va_arg(argp, char *);          
      n = g95_match_name(np);      
      if (n != MATCH_YES) { q = n; goto not_yes; }   
   
      matches++;     
      goto loop;       
       
    case 'l':   
      label = va_arg(argp, g95_st_label **);        
      n = g95_match_st_label(label, 0);      
      if (n != MATCH_YES) { q = n; goto not_yes; }  
  
      matches++; 
      goto loop;   
   
    case 'o':  
      ip = va_arg(argp, int *);      
      n = g95_match_intrinsic_op((g95_intrinsic_op *) ip); 
      if (n != MATCH_YES) { q = n; goto not_yes; } 
 
      matches++;    
      goto loop;          
          
    case 't':     
      if (g95_match_eos() != MATCH_YES) { q = MATCH_NO; goto not_yes; }   
      goto loop;          
          
    case ' ':  
      if (g95_match_space() == MATCH_YES) goto loop;  
      q = MATCH_NO;    
      goto not_yes;     
     
    case '%': break;  /* Fall through to character matcher */    
    
    default:    
      g95_internal_error("g95_match(): Bad match code %c", w);     
    }   
   
  default:    
    if (w == g95_next_char()) goto loop;         
    break;    
  }       
       
not_yes:          
  va_end(argp);   
   
  if (q != MATCH_YES) {   /* Clean up after a failed match */        
    g95_set_locus(&old_loc);         
    va_start(argp, target);          
          
    r = target;
    for(; matches>0; matches--) {  
      while(*r++ != '%');    
    
      switch(*r++) {    
      case '%': matches++; break;   /* Skip */        
        
      case 'I': case 'L': case 'C':        
	if (*r++ == 'e') goto undo_expr;          
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
      
  return q;       
}       
       
       
       
       
/* g95_match_elseif()-- Match an ELSE IF statement */        
        
match g95_match_elseif(void) {   
char name[G95_MAX_SYMBOL_LEN+1];          
g95_expr *expr;
match i;

  i = g95_match(" ( %e ) then", &expr);     
  if (i != MATCH_YES) return i;          
          
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
  new_st.expr = expr;      
  return MATCH_YES;    
    
cleanup:         
  g95_free_expr(expr);         
  return MATCH_ERROR;     
}


   
   
/* match_exit_cycle()-- Match an EXIT or CYCLE statement */      
      
static match match_exit_cycle(g95_statement st, g95_exec_op op) {   
g95_state_data *s;      
g95_symbol *symb;  
match b;       
       
  if (g95_match_eos() == MATCH_YES)       
    symb = NULL;        
  else {   
    b = g95_match("% %s%t", &symb);     
    if (b == MATCH_ERROR) return MATCH_ERROR;      
    if (b == MATCH_NO) {          
      g95_syntax_error(st);         
      return MATCH_ERROR;      
    }   
   
    if (symb->attr.flavor != FL_LABEL) {  
      g95_error("Name '%s' in %s statement at %C is not a loop name",
		symb->name, g95_ascii_statement(st));   
      return MATCH_ERROR;      
    }        
  }       
       
/* Find the loop mentioned specified by the label (or lack of a label) */   
   
  for(s=g95_state_stack; s; s=s->previous)        
    if (s->state == COMP_DO && (symb == NULL || symb == s->sym)) break;  
  
  if (s == NULL) {      
    if (symb == NULL)     
      g95_error("%s statement at %C is not within a loop",      
		g95_ascii_statement(st));  
    else          
      g95_error("%s statement at %C is not within loop '%s'",
		g95_ascii_statement(st), symb->name);      
      
    return MATCH_ERROR;     
  }

  /* Save the first statement in the loop - needed by the backend */        
        
  new_st.ext.block = s->top;       
  new_st.type = op; 
  new_st.sym = symb;    
    
  return MATCH_YES;      
}         
         
         
    
    
/* g95_free_data()-- Free a list of g95_data structures */        
        
void g95_free_data(g95_data *b) {     
g95_data *j;         
         
  for(; b; b=j) {  
    j = b->next; 
 
    free_variable(b->var);    
    free_value(b->value);       
       
    g95_free(b); 
  }    
}


        
        
/* g95_match_small_int()-- Match a small, constant integer expression,
 * like in a kind statement.  On MATCH_YES, 'value' is set. */ 
 
match g95_match_small_int(int *value) {  
g95_expr *expr;      
char *o;    
match r; 
int j;        
        
  r = g95_match_expr(&expr);          
  if (r != MATCH_YES) return r;        
        
  o = g95_extract_int(expr, &j);    
  g95_free_expr(expr);     
     
  if (o != NULL) { 
    g95_error(o);         
    r = MATCH_ERROR;         
  }          
          
  *value = j; 
  return r; 
}         
         
         
          
          
static match match_data_constant(g95_expr **result) {       
char name[G95_MAX_SYMBOL_LEN+1]; 
g95_symbol *symbol;
g95_expr *expr;         
match p;        
        
  p = g95_match_literal_constant(&expr, 1);  
  if (p == MATCH_YES) {      
    *result = expr;        
    return MATCH_YES;     
  }        
        
  if (p == MATCH_ERROR) return MATCH_ERROR;     
     
  p = g95_match_null(result); 
  if (p != MATCH_NO) return p; 
 
  p = g95_match_name(name);   
  if (p != MATCH_YES) return p;          
          
  if (g95_find_symbol(name, NULL, 1, &symbol)) return MATCH_ERROR;       
       
  if (symbol == NULL || symbol->attr.flavor != FL_PARAMETER) {    
    g95_error("Symbol '%s' must be a PARAMETER in DATA statement at %C",         
	      name);        
    return MATCH_ERROR;         
  }

  *result = g95_copy_expr(symbol->value);          
  return MATCH_YES;       
}      
      
      
  
  
/* g95_match_st_function()-- Match a statement function declaration.
 * It is so easy to match non-statement function statements with a
 * MATCH_ERROR as opposed to MATCH_NO that we suppress error message
 * in most cases. */     
     
match g95_match_st_function(void) {      
g95_error_buf old_error;
g95_expr *e, *expr;   
g95_namespace *ns;
g95_symbol *s;          
g95_symtree *st;   
g95_code *v;    
match m;      
      
  m = g95_match_symbol(&s, 0);     
  if (m != MATCH_YES) return m;

  g95_push_error(&old_error);     
     
  if (g95_add_procedure(&s->attr, PROC_ST_FUNCTION, NULL) == FAILURE)          
    goto undo_error;          
          
  if (g95_match_formal_arglist(s, 1, 0) != MATCH_YES) goto undo_error;         
         
  m = g95_match(" = %e%t", &expr); 
  if (m == MATCH_NO) goto undo_error;    
  if (m == MATCH_ERROR) return m;  
  
  /* At this point, we have a statement function.  These are
   * implemented by turning them into the equivalent contained
   * procedure.  The dummy arguments are copied into the contained
   * space during resolution. */       
       
  s->result = s;       
       
  e = g95_get_expr();
  e->type = EXPR_VARIABLE;
  e->symbol = s; 
  e->where = *g95_current_locus();

  v = g95_get_code(); 
  v->type = EXEC_ASSIGN;   
   
  v->expr = e;
  v->expr2 = expr;    
    
  ns = g95_get_namespace(g95_current_ns);         
  ns->proc_name = s; 
  ns->code = v;       
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


        
        
/* g95_match_module()-- Match a MODULE statement */      
      
match g95_match_module(void) {    
match l;     
     
  l = g95_match(" %s%t", &g95_new_block);         
  if (l != MATCH_YES) return l;        
        
  if (g95_add_flavor(&g95_new_block->attr, FL_MODULE, NULL) == FAILURE)    
    return MATCH_ERROR;   
   
  return MATCH_YES;    
}          
          
          


/* g95_match_pause()-- Match the (deprecated) PAUSE statement */      
      
match g95_match_pause(void) {      
g95_expr *expr; 
 
  if (g95_match_eos() == MATCH_YES) goto got_match;  
  
  if (g95_match(" %e%t", &expr) == MATCH_YES) {      
    g95_free_expr(expr); 
    goto got_match;          
  }   
   
  return MATCH_NO;    
    
got_match: 
  g95_error("The PAUSE statement at %C is not allowed in Fortran 95");     
  return MATCH_ERROR;     
}    
    
    
          
          
/* top_val_list()-- Match a list of values in a DATA statement.  The
 * leading '/' has already been seen at this point. */         
         
static match top_val_list(g95_data *data) {   
g95_data_value *new, *tail; 
g95_expr *expr;    
char *msg;        
match d;        
        
  tail = NULL; 
 
  for(;;) {       
    d = match_data_constant(&expr);   
    if (d == MATCH_NO) goto syntax;          
    if (d == MATCH_ERROR) return MATCH_ERROR;   
   
    new = g95_get_data_value();          
          
    if (tail == NULL)     
      data->value = new;        
    else      
      tail->next = new;     
     
    tail = new;      
      
    if (expr->ts.type != BT_INTEGER || g95_match_char('*') != MATCH_YES) {   
      tail->expr = expr;       
      tail->repeat = 1;      
    } else {
      msg = g95_extract_int(expr, &tail->repeat);         
      g95_free_expr(expr);        
      if (msg != NULL) {      
	g95_error(msg);      
	return MATCH_ERROR;       
      }   
   
      d = match_data_constant(&tail->expr);          
      if (d == MATCH_NO) goto syntax; 
      if (d == MATCH_ERROR) return MATCH_ERROR; 
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
       
       
       
       
/* g95_match_exit()-- Match the EXIT statement */     
     
match g95_match_exit(void) {

  return match_exit_cycle(ST_EXIT,  EXEC_EXIT);     
}         
         
         
        
        
/* g95_match_data()-- Match a DATA statement */ 
 
match g95_match_data(void) { 
g95_data *new;        
match f;    
      
  for(;;) {        
    new = g95_get_data();          
    new->where = *g95_current_locus();          
          
    f = top_var_list(new);    
    if (f != MATCH_YES) goto cleanup;

    f = top_val_list(new); 
    if (f != MATCH_YES) goto cleanup;         
         
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
     
     


/* g95_match_equivalence()-- Match an EQUIVALENCE statement */      
      
match g95_match_equivalence(void) {  
g95_equiv *eq, *set, *tail; 
g95_ref *r;      
match q;  
  
  tail = NULL;        
        
  for(;;) {    
    eq = g95_get_equiv();    
    if (tail == NULL) tail = eq;   
   
    eq->next = g95_current_ns->equiv;  
    g95_current_ns->equiv = eq;       
       
    if (g95_match_char('(') != MATCH_YES) goto syntax;         
         
    set = eq;          
          
    for(;;) {      
      q = g95_match_variable(&set->expr, 1);        
      if (q == MATCH_ERROR) goto cleanup;
      if (q == MATCH_NO) goto syntax;    
    
      for(r=set->expr->ref; r; r=r->next)      
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
  eq = tail->next;        
  tail->next = NULL;       
       
  g95_free_equiv(g95_current_ns->equiv);  
  g95_current_ns->equiv = eq;    
    
  return MATCH_ERROR;    
}


       
       
/* g95_match_cycle()-- Match the CYCLE statement */         
         
match g95_match_cycle(void) {         
         
  return match_exit_cycle(ST_CYCLE, EXEC_CYCLE);     
}  
  
  
