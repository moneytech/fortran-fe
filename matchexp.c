/* Expression matcher
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
          
/* matchexp.c-- Expression parser */   
   
   
#include <string.h>
#include <ctype.h>
#include "g95.h"
      
static char expression_syntax[] = "Syntax error in expression at %C";


   
   
/* g95_match_defined_op_name()-- Match a user-defined operator name.
 * This is a normal name with a few restrictions.  The error_flag
 * controls whether an error is raised if 'true' or 'false' are used
 * or not. */  
  
match g95_match_defined_op_name(char *result, int error_flag) {  
static char *badops[] = { 
 "and", "or", "not", "eqv", "neqv", "eq", "ne", "ge", "le", "lt", "gt", NULL };          
          
char name[G95_MAX_SYMBOL_LEN+1];   
locus old_loc;        
match h;         
int g;

  old_loc = *g95_current_locus();         
         
  h = g95_match(" . %n .", name);     
  if (h != MATCH_YES) return h;   
   
/* .true. and .false. have interpretations as constants.  Trying to
 * use these as operators will fail at a later time */        
        
  if (strcmp(name, "true") == 0 || strcmp(name, "false") == 0) {    
    if (error_flag) goto error;          
    g95_set_locus(&old_loc);  
    return MATCH_NO;       
  }     
     
  for(g=0; badops[g]; g++)
    if (strcmp(badops[g], name) == 0) goto error;   
   
  for(g=0; name[g]; g++)          
    if (!isalpha(name[g])) {        
      g95_error("Bad character '%c' in OPERATOR name at %C", name[g]);
      return MATCH_ERROR;  
    }

  strcpy(result, name);      
  return MATCH_YES;          
          
error:      
  g95_error("The name '%s' cannot be used as a defined operator at %C",      
	    name);       
       
  g95_set_locus(&old_loc);          
  return MATCH_ERROR;  
}


  
  
/* match_primary()-- Match a primary expression */          
          
static match match_primary(g95_expr **result) {
match m;       
       
  m = g95_match_literal_constant(result, 0);  
  if (m != MATCH_NO) return m;         
         
  m = g95_match_array_constructor(result);      
  if (m != MATCH_NO) return m;  
  
  m = g95_match_rvalue(result);  
  if (m != MATCH_NO) return m;

/* Match an expression in parenthesis */      
      
  if (g95_match_char('(') != MATCH_YES) return MATCH_NO;          
          
  m = g95_match_expr(result);       
  if (m == MATCH_NO) goto syntax;      
  if (m == MATCH_ERROR) return m;

  m = g95_match_char(')');       
  if (m == MATCH_NO)     
    g95_error("Expected a right parenthesis in expression at %C"); 
 
  if (m != MATCH_YES) {     
    g95_free_expr(*result);        
    return MATCH_ERROR;       
  }      
      
  return MATCH_YES;

syntax:         
  g95_error(expression_syntax);   
  return MATCH_ERROR;    
}  
  
  
  
  
/* next_operator()-- Checks to see if the given operator is next on
 * the input.  If this is not the case, the parse pointer remains
 * where it was. */

static int next_operator(g95_intrinsic_op q) {    
g95_intrinsic_op m;        
locus old_loc;    
    
  old_loc = *g95_current_locus();         
  if (g95_match_intrinsic_op(&m) == MATCH_YES && q == m) return 1;          
          
  g95_set_locus(&old_loc);          
  return 0;          
}  
  
  
    
    
/* match_defined_operator()-- Match a user defined operator.  The
 * symbol found must be an operator already. */        
        
static match match_defined_operator(g95_user_op **result) {   
char name[G95_MAX_SYMBOL_LEN+1];    
match e; 
 
  e = g95_match_defined_op_name(name, 0);
  if (e != MATCH_YES) return e;       
       
  *result = g95_get_uop(name);    
  return MATCH_YES;    
}          
          
          
          
          
/* build_node()-- Build an operator expression node. */

static g95_expr *build_node(g95_intrinsic_op operator, locus *where,          
			    g95_expr *op1, g95_expr *op2) {      
g95_expr *new;       
       
  new = g95_get_expr();      
  new->type = EXPR_OP; 
  new->operator = operator;          
  new->where = *where;       
         
  new->op1 = op1;          
  new->op2 = op2;

  return new;
}         
         
         
     
     
/* match_level_1()-- Match a level 1 expression */     
     
static match match_level_1(g95_expr **result) {   
g95_user_op *uop;        
g95_expr *q, *w;  
locus where;      
match m; 
 
  where = *g95_current_locus();         
  uop = NULL;           
  m = match_defined_operator(&uop);
  if (m == MATCH_ERROR) return m;       
       
  m = match_primary(&q);
  if (m != MATCH_YES) return m;         
         
  if (uop == NULL)       
    *result = q;
  else {          
    w = build_node(INTRINSIC_USER, &where, q, NULL);     
    w->uop = uop;     
    *result = w;   
  }        
        
  return MATCH_YES;   
}      
      
      
         
         
static match match_mult_operand(g95_expr **result) {
g95_expr *p, *exp, *z;
locus where;       
match v;    
    
  v = match_level_1(&p);   
  if (v != MATCH_YES) return v;       
       
  if (!next_operator(INTRINSIC_POWER)) {    
    *result = p;          
    return MATCH_YES;  
  }

  where = *g95_current_locus();        
        
  v = match_mult_operand(&exp);
  if (v == MATCH_NO) g95_error("Expected exponent in expression at %C"); 
  if (v != MATCH_YES) {     
    g95_free_expr(p);    
    return MATCH_ERROR;          
  } 
 
  z = g95_power(p, exp);     
  if (z == NULL) { 
    g95_free_expr(p);
    g95_free_expr(exp);      
    return MATCH_ERROR;     
  }          
          
  z->where = where;    
  *result = z;  
  
  return MATCH_YES;      
}          
          
          
       
       
static match match_add_operand(g95_expr **result) {      
g95_expr *all, *q, *total;      
locus where, old_loc;
match u;   
int x;  
  
  u = match_mult_operand(&all);        
  if (u != MATCH_YES) return u;     
     
  for(;;) {    /* Build up a string of products or quotients */
    x = 0;

    old_loc = *g95_current_locus();     
     
    if (next_operator(INTRINSIC_TIMES)) 
      x = INTRINSIC_TIMES;          
    else {  
      if (next_operator(INTRINSIC_DIVIDE))        
	x = INTRINSIC_DIVIDE;         
      else    
	break;    
    }         
         
    where = *g95_current_locus();

    u = match_mult_operand(&q);       
    if (u == MATCH_NO) {     
      g95_set_locus(&old_loc);      
      break;      
    }    
    
    if (u == MATCH_ERROR) {        
      g95_free_expr(all);   
      return MATCH_ERROR;   
    }   
   
    if (x == INTRINSIC_TIMES)      
      total = g95_multiply(all, q);     
    else         
      total = g95_divide(all, q);     
     
    if (total == NULL) {          
      g95_free_expr(all);      
      g95_free_expr(q);
      return MATCH_ERROR; 
    }          
          
    all = total;     
    all->where = where;       
  }     
     
  *result = all;  
  return MATCH_YES;   
}       
       
       
      
      
static int match_add_op(void) {        
        
  if (next_operator(INTRINSIC_MINUS)) return -1;      
  if (next_operator(INTRINSIC_PLUS)) return 1;
  return 0;          
}   
   
   
   
   
/* match_level_2()-- Match a level 2 expression.  */   
   
static match match_level_2(g95_expr **result) { 
g95_expr *all, *u, *total;         
locus where;      
match h;    
int i;     
     
  where = *g95_current_locus();   
  i = match_add_op();        
        
  h = match_add_operand(&u);        
  if (i != 0 && h == MATCH_NO) {   
    g95_error(expression_syntax);       
    h = MATCH_ERROR;
  }          
          
  if (h != MATCH_YES) return h;          
          
  if (i == 0)      
    all = u;          
  else {      
    if (i == -1)          
      all = g95_uminus(u);      
    else     
      all = g95_uplus(u);        
        
    if (all == NULL) {      
      g95_free_expr(u);
      return MATCH_ERROR;         
    }          
  }

  all->where = where;          
          
/* Append add-operands to the sum */       
       
  for(;;) { 
    where = *g95_current_locus();       
    i = match_add_op();        
    if (i == 0) break;   
   
    h = match_add_operand(&u);      
    if (h == MATCH_NO) g95_error(expression_syntax);       
    if (h != MATCH_YES) {       
      g95_free_expr(all);        
      return MATCH_ERROR;     
    }  
  
    if (i == -1) 
      total = g95_subtract(all, u); 
    else        
      total = g95_add(all, u);  
  
    if (total == NULL) {  
      g95_free_expr(all);         
      g95_free_expr(u); 
      return MATCH_ERROR;     
    }  
  
    all = total;
    all->where = where;       
  }     
     
  *result = all;
  return MATCH_YES;      
} 
 
 
       
       
/* match_level_3()-- Match a level three expression */    
    
static match match_level_3(g95_expr **result) {    
g95_expr *all, *r, *total;      
locus where;       
match m;          
          
  m = match_level_2(&all);         
  if (m != MATCH_YES) return m;       
       
  for(;;) {
    if (!next_operator(INTRINSIC_CONCAT)) break; 
 
    where = *g95_current_locus();     
     
    m = match_level_2(&r);
    if (m == MATCH_NO) {       
      g95_error(expression_syntax);     
      g95_free_expr(all);     
    }      
    if (m != MATCH_YES) return MATCH_ERROR;         
         
    total = g95_concat(all, r);  
    if (total == NULL) {  
      g95_free_expr(all);       
      g95_free_expr(r);     
      return MATCH_ERROR; 
    }  
  
    all = total;      
    all->where = where;
  }      
      
  *result = all;   
  return MATCH_YES;      
}          
          
          
     
     
/* match_level_4()-- Match a level 4 expression */  
  
static match match_level_4(g95_expr **result) {     
g95_expr *left, *right, *a;        
g95_intrinsic_op i;        
locus old_loc;  
locus where;
match o;        
        
  o = match_level_3(&left);          
  if (o != MATCH_YES) return o;      
      
  old_loc = *g95_current_locus();   
   
  if (g95_match_intrinsic_op(&i) != MATCH_YES) {         
    *result = left;    
    return MATCH_YES;   
  }  
  
  if (i != INTRINSIC_EQ && i != INTRINSIC_NE && i != INTRINSIC_GE &&       
      i != INTRINSIC_LE && i != INTRINSIC_LT && i != INTRINSIC_GT) {        
    g95_set_locus(&old_loc);   
    *result = left;      
    return MATCH_YES;          
  }       
       
  where = *g95_current_locus();         
         
  o = match_level_3(&right);    
  if (o == MATCH_NO) g95_error(expression_syntax);       
  if (o != MATCH_YES) {    
    g95_free_expr(left);         
    return MATCH_ERROR;     
  }       
       
  switch(i) {        
  case INTRINSIC_EQ:          
    a = g95_eq(left, right);
    break;     
     
  case INTRINSIC_NE:     
    a = g95_ne(left, right); 
    break;  
  
  case INTRINSIC_LT:  
    a = g95_lt(left, right);          
    break;     
     
  case INTRINSIC_LE:    
    a = g95_le(left, right);         
    break;      
      
  case INTRINSIC_GT:
    a = g95_gt(left, right); 
    break;   
   
  case INTRINSIC_GE:  
    a = g95_ge(left, right);   
    break;    
    
  default:      
    g95_internal_error("match_level_4(): Bad operator");
  }  
  
  if (a == NULL) {          
    g95_free_expr(left);   
    g95_free_expr(right);         
    return MATCH_ERROR;
  }          
          
  a->where = where; 
  *result = a;         
         
  return MATCH_YES;   
}




static match match_and_operand(g95_expr **result) {         
g95_expr *k, *v;        
locus where; 
match m; 
int a;  
  
  a = next_operator(INTRINSIC_NOT);  
  where = *g95_current_locus();      
     
  m = match_level_4(&k);    
  if (m != MATCH_YES) return m;       
       
  v = k;    
  if (a) {      
    v = g95_not(k);   
    if (v == NULL) {          
      g95_free_expr(k);  
      return MATCH_ERROR;        
    }
  }      
      
  v->where = where;       
  *result = v; 
 
  return MATCH_YES;     
}        
        
        
         
         
static match match_or_operand(g95_expr **result) {     
g95_expr *all, *g, *total;  
locus where;      
match w;      
      
  w = match_and_operand(&all);       
  if (w != MATCH_YES) return w;         
         
  for(;;) {
    if (!next_operator(INTRINSIC_AND)) break; 
    where = *g95_current_locus();  
  
    w = match_and_operand(&g);   
    if (w == MATCH_NO) g95_error(expression_syntax);  
    if (w != MATCH_YES) {      
      g95_free_expr(all);    
      return MATCH_ERROR;    
    }       
       
    total = g95_and(all, g);
    if (total == NULL) {  
      g95_free_expr(all);  
      g95_free_expr(g);
      return MATCH_ERROR;   
    }          
          
    all = total; 
    all->where = where;         
  }      
      
  *result = all;      
  return MATCH_YES;     
}     
     
     
     
     
static match match_equiv_operand(g95_expr **result) {    
g95_expr *all, *l, *total; 
locus where;        
match j;

  j = match_or_operand(&all);         
  if (j != MATCH_YES) return j;

  for(;;) {          
    if (!next_operator(INTRINSIC_OR)) break;       
    where = *g95_current_locus();      
      
    j = match_or_operand(&l); 
    if (j == MATCH_NO) g95_error(expression_syntax);  
    if (j != MATCH_YES) {       
      g95_free_expr(all);         
      return MATCH_ERROR;  
    }          
          
    total = g95_or(all, l);          
    if (total == NULL) {      
      g95_free_expr(all);    
      g95_free_expr(l);     
      return MATCH_ERROR; 
    }         
         
    all = total;         
    all->where = where;          
  }          
          
  *result = all;   
  return MATCH_YES;          
}        
        
        
     
     
/* match_level_5()-- Match a level 5 expression */          
          
static match match_level_5(g95_expr **result) {   
g95_expr *all, *c, *total;     
locus where;  
match a;
int r;       
       
  a = match_equiv_operand(&all);         
  if (a != MATCH_YES) return a;        
        
  for(;;) {          
    if (next_operator(INTRINSIC_EQV))          
      r = INTRINSIC_EQV;      
    else {       
      if (next_operator(INTRINSIC_NEQV))   
	r = INTRINSIC_NEQV;  
      else       
	break;       
    }     
     
    where = *g95_current_locus();         
           
    a = match_equiv_operand(&c);          
    if (a == MATCH_NO) g95_error(expression_syntax);          
    if (a != MATCH_YES) {  
      g95_free_expr(all);          
      return MATCH_ERROR; 
    } 
 
    if (r == INTRINSIC_EQV)  
      total = g95_eqv(all, c);   
    else         
      total = g95_neqv(all, c);        
        
    if (total == NULL) {        
      g95_free_expr(all);      
      g95_free_expr(c); 
      return MATCH_ERROR;        
    } 
 
    all = total;
    all->where = where;         
  }

  *result = all;          
  return MATCH_YES;     
}          
          
          


/* g95_match_expr()-- Match an expression.  At this level, we are
 * stringing together level 5 expressions separated by binary operators. */      
      
match g95_match_expr(g95_expr **result) {       
g95_expr *all, *o;          
g95_user_op *uop;   
locus where;          
match h;

  h = match_level_5(&all);     
  if (h != MATCH_YES) return h;          
          
  for(;;) {    
    h = match_defined_operator(&uop);  
    if (h == MATCH_NO) break;         
    if (h == MATCH_ERROR) {
      g95_free_expr(all);   
      return MATCH_ERROR;      
    }         
         
    where = *g95_current_locus();          
          
    h = match_level_5(&o); 
    if (h == MATCH_NO) g95_error(expression_syntax);        
    if (h != MATCH_YES) {        
      g95_free_expr(all);         
      return MATCH_ERROR;
    } 
 
    all = build_node(INTRINSIC_USER, &where, all, o); 
    all->uop = uop;         
  } 
 
  *result = all;       
  return MATCH_YES;       
}      
