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
       
       
       
       
/* next_operator()-- Checks to see if the given operator is next on
 * the input.  If this is not the case, the parse pointer remains
 * where it was. */      
      
static int next_operator(g95_intrinsic_op o) {       
g95_intrinsic_op g;      
locus where;     
     
  where = *g95_current_locus();  
  if (g95_match_intrinsic_op(&g) == MATCH_YES && o == g) return 1;       
       
  g95_set_locus(&where);       
  return 0;      
}       
       
       


/* g95_match_defined_op_name()-- Match a user-defined operator name.
 * This is a normal name with a few restrictions.  The error_flag
 * controls whether an error is raised if 'true' or 'false' are used
 * or not. */         
         
match g95_match_defined_op_name(char *rslt, int error_flag) {        
static char *badops[] = {          
 "and", "or", "not", "eqv", "neqv", "eq", "ne", "ge", "le", "lt", "gt", NULL };      
      
char nam[G95_MAX_SYMBOL_LEN+1]; 
locus old;
match f;   
int y;    
    
  old = *g95_current_locus();  
  
  f = g95_match(" . %n .", nam);    
  if (f != MATCH_YES) return f;         
         
/* .true. and .false. have interpretations as constants.  Trying to
 * use these as operators will fail at a later time */         
         
  if (strcmp(nam, "true") == 0 || strcmp(nam, "false") == 0) {      
    if (error_flag) goto error; 
    g95_set_locus(&old);        
    return MATCH_NO;    
  }        
        
  for(y=0; badops[y]; y++)        
    if (strcmp(badops[y], nam) == 0) goto error;     
     
  for(y=0; nam[y]; y++)       
    if (!isalpha(nam[y])) {      
      g95_error("Bad character '%c' in OPERATOR name at %C", nam[y]);
      return MATCH_ERROR;    
    }

  strcpy(rslt, nam);          
  return MATCH_YES;    
    
error:    
  g95_error("The name '%s' cannot be used as a defined operator at %C",
	    nam);      
      
  g95_set_locus(&old);    
  return MATCH_ERROR;        
}       
       
       
    
    
/* match_primary()-- Match a primary expression */    
    
static match match_primary(g95_expr **rslt) {   
match y;  
  
  y = g95_match_literal_constant(rslt, 0);      
  if (y != MATCH_NO) return y;    
    
  y = g95_match_array_constructor(rslt);
  if (y != MATCH_NO) return y;     
     
  y = g95_match_rvalue(rslt);
  if (y != MATCH_NO) return y;          
          
/* Match an expression in parenthesis */

  if (g95_match_char('(') != MATCH_YES) return MATCH_NO;  
  
  y = g95_match_expr(rslt);   
  if (y == MATCH_NO) goto syntax;     
  if (y == MATCH_ERROR) return y;    
    
  y = g95_match_char(')');      
  if (y == MATCH_NO)    
    g95_error("Expected a right parenthesis in expression at %C");       
       
  if (y != MATCH_YES) {     
    g95_free_expr(*rslt);       
    return MATCH_ERROR;        
  }    
    
  return MATCH_YES;         
         
syntax:        
  g95_error(expression_syntax);       
  return MATCH_ERROR;     
}          
          
          
   
   
/* match_defined_operator()-- Match a user defined operator.  The
 * symbol found must be an operator already. */  
  
static match match_defined_operator(g95_user_op **r) {       
char n[G95_MAX_SYMBOL_LEN+1];          
match p;  
  
  p = g95_match_defined_op_name(n, 0);    
  if (p != MATCH_YES) return p;      
      
  *r = g95_get_uop(n);         
  return MATCH_YES;         
}          
          
          
          
          
/* build_node()-- Build an operator expression node. */ 
 
static g95_expr *build_node(g95_intrinsic_op operator, locus *loc,        
			    g95_expr *op, g95_expr *op0) {      
g95_expr *old;    
    
  old = g95_get_expr();
  old->type = EXPR_OP; 
  old->operator = operator;          
  old->where = *loc;    
      
  old->op1 = op;        
  old->op2 = op0;    
    
  return old;         
}     
     
     
        
        
/* match_level_1()-- Match a level 1 expression */   
   
static match match_level_1(g95_expr **r) {
g95_user_op *uop;  
g95_expr *u, *k;
locus w;    
match j; 
 
  w = *g95_current_locus();  
  uop = NULL;   
  j = match_defined_operator(&uop);     
  if (j == MATCH_ERROR) return j;     
     
  j = match_primary(&u);      
  if (j != MATCH_YES) return j;         
         
  if (uop == NULL)       
    *r = u;   
  else {       
    k = build_node(INTRINSIC_USER, &w, u, NULL);    
    k->uop = uop;    
    *r = k;
  } 
 
  return MATCH_YES;         
}  
  
  
     
     
static match match_mult_operand(g95_expr **rslt) {   
g95_expr *t, *exp, *k;
locus w; 
match p; 
 
  p = match_level_1(&t);   
  if (p != MATCH_YES) return p;      
      
  if (!next_operator(INTRINSIC_POWER)) {    
    *rslt = t;      
    return MATCH_YES;    
  }       
       
  w = *g95_current_locus();      
      
  p = match_mult_operand(&exp);        
  if (p == MATCH_NO) g95_error("Expected exponent in expression at %C");   
  if (p != MATCH_YES) {    
    g95_free_expr(t);          
    return MATCH_ERROR;         
  }    
    
  k = g95_power(t, exp);       
  if (k == NULL) { 
    g95_free_expr(t);   
    g95_free_expr(exp);        
    return MATCH_ERROR;       
  }  
  
  k->where = w;       
  *rslt = k;    
    
  return MATCH_YES;
} 
 
 
        
        
static match match_add_operand(g95_expr **res) {   
g95_expr *all, *v, *total; 
locus pos, old;   
match d;    
int l;   
   
  d = match_mult_operand(&all);       
  if (d != MATCH_YES) return d;       
       
  for(;;) {    /* Build up a string of products or quotients */ 
    l = 0;          
          
    old = *g95_current_locus(); 
 
    if (next_operator(INTRINSIC_TIMES)) 
      l = INTRINSIC_TIMES;
    else {  
      if (next_operator(INTRINSIC_DIVIDE))  
	l = INTRINSIC_DIVIDE;          
      else         
	break;      
    }        
        
    pos = *g95_current_locus();          
          
    d = match_mult_operand(&v);    
    if (d == MATCH_NO) {  
      g95_set_locus(&old);    
      break;    
    } 
 
    if (d == MATCH_ERROR) {          
      g95_free_expr(all);          
      return MATCH_ERROR;
    }  
  
    if (l == INTRINSIC_TIMES)   
      total = g95_multiply(all, v); 
    else         
      total = g95_divide(all, v);          
          
    if (total == NULL) {       
      g95_free_expr(all);    
      g95_free_expr(v);   
      return MATCH_ERROR;    
    }          
          
    all = total;         
    all->where = pos;    
  } 
 
  *res = all;    
  return MATCH_YES;   
}   
   
   
  
  
static int match_add_op(void) {

  if (next_operator(INTRINSIC_MINUS)) return -1;       
  if (next_operator(INTRINSIC_PLUS)) return 1;         
  return 0;          
}     
     
     
     
     
/* match_level_2()-- Match a level 2 expression.  */  
  
static match match_level_2(g95_expr **res) {    
g95_expr *all, *c, *total;          
locus old_loc;
match a;          
int g;          
          
  old_loc = *g95_current_locus();          
  g = match_add_op();   
   
  a = match_add_operand(&c);
  if (g != 0 && a == MATCH_NO) {     
    g95_error(expression_syntax); 
    a = MATCH_ERROR;          
  }  
  
  if (a != MATCH_YES) return a;         
         
  if (g == 0)    
    all = c;       
  else {  
    if (g == -1) 
      all = g95_uminus(c);     
    else      
      all = g95_uplus(c);          
          
    if (all == NULL) {
      g95_free_expr(c);
      return MATCH_ERROR;        
    } 
  }     
     
  all->where = old_loc;          
          
/* Append add-operands to the sum */ 
 
  for(;;) {     
    old_loc = *g95_current_locus(); 
    g = match_add_op();       
    if (g == 0) break;   
   
    a = match_add_operand(&c);          
    if (a == MATCH_NO) g95_error(expression_syntax);
    if (a != MATCH_YES) {    
      g95_free_expr(all);       
      return MATCH_ERROR;   
    }  
  
    if (g == -1)      
      total = g95_subtract(all, c);       
    else       
      total = g95_add(all, c);

    if (total == NULL) {         
      g95_free_expr(all);       
      g95_free_expr(c);        
      return MATCH_ERROR;        
    }

    all = total;      
    all->where = old_loc;  
  }    
    
  *res = all;    
  return MATCH_YES;        
}  
  
  
     
     
/* match_level_3()-- Match a level three expression */    
    
static match match_level_3(g95_expr **rslt) {          
g95_expr *all, *w, *total;
locus old_loc;
match v;       
       
  v = match_level_2(&all);          
  if (v != MATCH_YES) return v;    
    
  for(;;) {
    if (!next_operator(INTRINSIC_CONCAT)) break;   
   
    old_loc = *g95_current_locus();   
   
    v = match_level_2(&w);  
    if (v == MATCH_NO) {   
      g95_error(expression_syntax);  
      g95_free_expr(all); 
    }      
    if (v != MATCH_YES) return MATCH_ERROR; 
 
    total = g95_concat(all, w);         
    if (total == NULL) {
      g95_free_expr(all);    
      g95_free_expr(w);  
      return MATCH_ERROR;      
    }     
     
    all = total;        
    all->where = old_loc;        
  }          
          
  *rslt = all;    
  return MATCH_YES;         
}


     
     
/* match_level_4()-- Match a level 4 expression */ 
 
static match match_level_4(g95_expr **result) {     
g95_expr *left, *right, *u;       
g95_intrinsic_op b;
locus old;
locus loc;
match w;        
        
  w = match_level_3(&left);          
  if (w != MATCH_YES) return w;   
   
  old = *g95_current_locus();  
  
  if (g95_match_intrinsic_op(&b) != MATCH_YES) {     
    *result = left;  
    return MATCH_YES;     
  } 
 
  if (b != INTRINSIC_EQ && b != INTRINSIC_NE && b != INTRINSIC_GE &&       
      b != INTRINSIC_LE && b != INTRINSIC_LT && b != INTRINSIC_GT) {    
    g95_set_locus(&old);  
    *result = left;    
    return MATCH_YES;     
  }       
       
  loc = *g95_current_locus();

  w = match_level_3(&right);    
  if (w == MATCH_NO) g95_error(expression_syntax);        
  if (w != MATCH_YES) {       
    g95_free_expr(left);     
    return MATCH_ERROR;
  }  
  
  switch(b) {   
  case INTRINSIC_EQ:   
    u = g95_eq(left, right);        
    break;          
          
  case INTRINSIC_NE:    
    u = g95_ne(left, right);    
    break; 
 
  case INTRINSIC_LT:          
    u = g95_lt(left, right);          
    break;      
      
  case INTRINSIC_LE:         
    u = g95_le(left, right);    
    break;   
   
  case INTRINSIC_GT:   
    u = g95_gt(left, right);          
    break;  
  
  case INTRINSIC_GE:         
    u = g95_ge(left, right);   
    break;   
   
  default:          
    g95_internal_error("match_level_4(): Bad operator");       
  }         
         
  if (u == NULL) {          
    g95_free_expr(left); 
    g95_free_expr(right);  
    return MATCH_ERROR;    
  }      
      
  u->where = loc;         
  *result = u;

  return MATCH_YES;       
}    
    
    


static match match_and_operand(g95_expr **res) {        
g95_expr *v, *l;    
locus old_loc;         
match q;         
int w;      
      
  w = next_operator(INTRINSIC_NOT);          
  old_loc = *g95_current_locus();          
         
  q = match_level_4(&v);          
  if (q != MATCH_YES) return q; 
 
  l = v;       
  if (w) {    
    l = g95_not(v);
    if (l == NULL) {         
      g95_free_expr(v);     
      return MATCH_ERROR;      
    }        
  }     
     
  l->where = old_loc;          
  *res = l;      
      
  return MATCH_YES;      
} 
 
 


static match match_or_operand(g95_expr **rslt) {         
g95_expr *all, *z, *total;          
locus pos;  
match k;        
        
  k = match_and_operand(&all);    
  if (k != MATCH_YES) return k;     
     
  for(;;) {  
    if (!next_operator(INTRINSIC_AND)) break;      
    pos = *g95_current_locus();       
       
    k = match_and_operand(&z);  
    if (k == MATCH_NO) g95_error(expression_syntax);      
    if (k != MATCH_YES) { 
      g95_free_expr(all);          
      return MATCH_ERROR;          
    }  
  
    total = g95_and(all, z);          
    if (total == NULL) {   
      g95_free_expr(all);       
      g95_free_expr(z); 
      return MATCH_ERROR;      
    }      
      
    all = total;  
    all->where = pos;        
  } 
 
  *rslt = all;     
  return MATCH_YES;      
}    
    
    
         
         
static match match_equiv_operand(g95_expr **result) {  
g95_expr *all, *c, *total;      
locus pos;         
match x;   
   
  x = match_or_operand(&all);    
  if (x != MATCH_YES) return x;   
   
  for(;;) {   
    if (!next_operator(INTRINSIC_OR)) break;     
    pos = *g95_current_locus();

    x = match_or_operand(&c);
    if (x == MATCH_NO) g95_error(expression_syntax); 
    if (x != MATCH_YES) {     
      g95_free_expr(all);  
      return MATCH_ERROR;
    }    
    
    total = g95_or(all, c);     
    if (total == NULL) {     
      g95_free_expr(all); 
      g95_free_expr(c);  
      return MATCH_ERROR; 
    }       
       
    all = total;         
    all->where = pos;      
  }   
   
  *result = all;
  return MATCH_YES;    
}   
   
   
    
    
/* match_level_5()-- Match a level 5 expression */     
     
static match match_level_5(g95_expr **r) { 
g95_expr *all, *d, *total;
locus where;  
match b;
int p;    
    
  b = match_equiv_operand(&all);   
  if (b != MATCH_YES) return b;   
   
  for(;;) {          
    if (next_operator(INTRINSIC_EQV))       
      p = INTRINSIC_EQV;    
    else {         
      if (next_operator(INTRINSIC_NEQV))         
	p = INTRINSIC_NEQV;   
      else    
	break;   
    }      
      
    where = *g95_current_locus();  
    
    b = match_equiv_operand(&d);
    if (b == MATCH_NO) g95_error(expression_syntax);     
    if (b != MATCH_YES) {   
      g95_free_expr(all); 
      return MATCH_ERROR;     
    }     
     
    if (p == INTRINSIC_EQV)       
      total = g95_eqv(all, d); 
    else        
      total = g95_neqv(all, d);       
       
    if (total == NULL) {        
      g95_free_expr(all);         
      g95_free_expr(d);   
      return MATCH_ERROR;          
    } 
 
    all = total;       
    all->where = where;  
  }          
          
  *r = all;        
  return MATCH_YES;        
}


       
       
/* g95_match_expr()-- Match an expression.  At this level, we are
 * stringing together level 5 expressions separated by binary operators. */    
    
match g95_match_expr(g95_expr **res) {        
g95_expr *all, *y;         
g95_user_op *u;
locus where;
match j;

  j = match_level_5(&all);     
  if (j != MATCH_YES) return j;        
        
  for(;;) {    
    j = match_defined_operator(&u);      
    if (j == MATCH_NO) break;   
    if (j == MATCH_ERROR) {
      g95_free_expr(all);   
      return MATCH_ERROR;         
    }         
         
    where = *g95_current_locus();      
      
    j = match_level_5(&y);         
    if (j == MATCH_NO) g95_error(expression_syntax); 
    if (j != MATCH_YES) {
      g95_free_expr(all);    
      return MATCH_ERROR;
    }          
          
    all = build_node(INTRINSIC_USER, &where, all, y);  
    all->uop = u; 
  }

  *res = all;         
  return MATCH_YES;   
}  
