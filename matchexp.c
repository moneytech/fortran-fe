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

static int next_operator(g95_intrinsic_op m) {  
g95_intrinsic_op d;         
locus old_loc;      
      
  old_loc = *g95_current_locus();  
  if (g95_match_intrinsic_op(&d) == MATCH_YES && m == d) return 1; 
 
  g95_set_locus(&old_loc);   
  return 0; 
}  
  
  


/* match_primary()-- Match a primary expression */   
   
static match match_primary(g95_expr **res) {
match y; 
 
  y = g95_match_literal_constant(res, 0);     
  if (y != MATCH_NO) return y;     
     
  y = g95_match_array_constructor(res);
  if (y != MATCH_NO) return y;    
    
  y = g95_match_rvalue(res);        
  if (y != MATCH_NO) return y;       
       
/* Match an expression in parenthesis */     
     
  if (g95_match_char('(') != MATCH_YES) return MATCH_NO;    
    
  y = g95_match_expr(res);
  if (y == MATCH_NO) goto syntax;          
  if (y == MATCH_ERROR) return y;  
  
  y = g95_match_char(')');     
  if (y == MATCH_NO)         
    g95_error("Expected a right parenthesis in expression at %C");          
          
  if (y != MATCH_YES) {  
    g95_free_expr(*res);
    return MATCH_ERROR;  
  } 
 
  return MATCH_YES; 
 
syntax:      
  g95_error(expression_syntax); 
  return MATCH_ERROR;     
}          
          
          


/* build_node()-- Build an operator expression node. */     
     
static g95_expr *build_node(g95_intrinsic_op o, locus *where,
			    g95_expr *op0, g95_expr *op) {     
g95_expr *new;      
      
  new = g95_get_expr();        
  new->type = EXPR_OP;       
  new->operator = o;      
  new->where = *where;    
      
  new->op1 = op0;  
  new->op2 = op;          
          
  return new;
}  
  
  
          
          
/* g95_match_defined_op_name()-- Match a user-defined operator name.
 * This is a normal name with a few restrictions.  The error_flag
 * controls whether an error is raised if 'true' or 'false' are used
 * or not. */ 
 
match g95_match_defined_op_name(char *rslt, int error_flag) { 
static char *badops[] = {       
 "and", "or", "not", "eqv", "neqv", "eq", "ne", "ge", "le", "lt", "gt", NULL };          
          
char nm[G95_MAX_SYMBOL_LEN+1]; 
locus old;
match s;
int y;     
     
  old = *g95_current_locus();   
   
  s = g95_match(" . %n .", nm);       
  if (s != MATCH_YES) return s;      
      
/* .true. and .false. have interpretations as constants.  Trying to
 * use these as operators will fail at a later time */     
     
  if (strcmp(nm, "true") == 0 || strcmp(nm, "false") == 0) {    
    if (error_flag) goto error;   
    g95_set_locus(&old);  
    return MATCH_NO;   
  }

  for(y=0; badops[y]; y++)      
    if (strcmp(badops[y], nm) == 0) goto error;   
   
  for(y=0; nm[y]; y++)          
    if (!isalpha(nm[y])) {      
      g95_error("Bad character '%c' in OPERATOR name at %C", nm[y]);         
      return MATCH_ERROR; 
    }

  strcpy(rslt, nm);
  return MATCH_YES;   
   
error:         
  g95_error("The name '%s' cannot be used as a defined operator at %C",    
	    nm);      
      
  g95_set_locus(&old);       
  return MATCH_ERROR;          
}        
        
        
     
     
static int match_add_op(void) {   
   
  if (next_operator(INTRINSIC_MINUS)) return -1;         
  if (next_operator(INTRINSIC_PLUS)) return 1;  
  return 0;    
}      
      
      
        
        
/* match_defined_operator()-- Match a user defined operator.  The
 * symbol found must be an operator already. */         
         
static match match_defined_operator(g95_user_op **res) {
char n[G95_MAX_SYMBOL_LEN+1];
match c; 
 
  c = g95_match_defined_op_name(n, 0);     
  if (c != MATCH_YES) return c;          
          
  *res = g95_get_uop(n);          
  return MATCH_YES;    
}     
     
     


/* match_level_1()-- Match a level 1 expression */        
        
static match match_level_1(g95_expr **res) { 
g95_user_op *operator;     
g95_expr *i, *z;     
locus w;  
match l;         
         
  w = *g95_current_locus();           
  operator = NULL;          
  l = match_defined_operator(&operator);         
  if (l == MATCH_ERROR) return l;   
   
  l = match_primary(&i);   
  if (l != MATCH_YES) return l;        
        
  if (operator == NULL)      
    *res = i;   
  else {      
    z = build_node(INTRINSIC_USER, &w, i, NULL);        
    z->uop = operator;         
    *res = z;        
  }         
         
  return MATCH_YES;        
}


     
     
static match match_mult_operand(g95_expr **res) {  
g95_expr *t, *exp, *l;        
locus where; 
match s;         
         
  s = match_level_1(&t);      
  if (s != MATCH_YES) return s;     
     
  if (!next_operator(INTRINSIC_POWER)) { 
    *res = t;         
    return MATCH_YES;    
  }         
         
  where = *g95_current_locus();     
     
  s = match_mult_operand(&exp);  
  if (s == MATCH_NO) g95_error("Expected exponent in expression at %C");          
  if (s != MATCH_YES) {         
    g95_free_expr(t);   
    return MATCH_ERROR;        
  }          
          
  l = g95_power(t, exp);          
  if (l == NULL) {      
    g95_free_expr(t);       
    g95_free_expr(exp);         
    return MATCH_ERROR;        
  }  
  
  l->where = where;
  *res = l;        
        
  return MATCH_YES;
}         
         
         
   
   
static match match_add_operand(g95_expr **rslt) {  
g95_expr *all, *b, *total; 
locus pos, old_loc;   
match r;       
int n;        
        
  r = match_mult_operand(&all);        
  if (r != MATCH_YES) return r;       
       
  for(;;) {    /* Build up a string of products or quotients */ 
    n = 0;          
          
    old_loc = *g95_current_locus();   
   
    if (next_operator(INTRINSIC_TIMES))          
      n = INTRINSIC_TIMES;      
    else {   
      if (next_operator(INTRINSIC_DIVIDE))      
	n = INTRINSIC_DIVIDE;      
      else    
	break;
    }  
  
    pos = *g95_current_locus();    
    
    r = match_mult_operand(&b);     
    if (r == MATCH_NO) {    
      g95_set_locus(&old_loc);     
      break;   
    }  
  
    if (r == MATCH_ERROR) {          
      g95_free_expr(all);       
      return MATCH_ERROR;        
    }     
     
    if (n == INTRINSIC_TIMES)         
      total = g95_multiply(all, b);     
    else
      total = g95_divide(all, b);    
    
    if (total == NULL) { 
      g95_free_expr(all);      
      g95_free_expr(b);  
      return MATCH_ERROR;     
    }       
       
    all = total;
    all->where = pos;  
  }    
    
  *rslt = all;      
  return MATCH_YES;       
}       
       
       


/* match_level_2()-- Match a level 2 expression.  */          
          
static match match_level_2(g95_expr **rslt) {    
g95_expr *all, *n, *total;     
locus old_loc;       
match j;         
int c;          
          
  old_loc = *g95_current_locus();  
  c = match_add_op();

  j = match_add_operand(&n);   
  if (c != 0 && j == MATCH_NO) {   
    g95_error(expression_syntax);         
    j = MATCH_ERROR;      
  }        
        
  if (j != MATCH_YES) return j; 
 
  if (c == 0)      
    all = n;       
  else { 
    if (c == -1)     
      all = g95_uminus(n);      
    else       
      all = g95_uplus(n);     
     
    if (all == NULL) {      
      g95_free_expr(n);   
      return MATCH_ERROR;     
    }    
  }    
    
  all->where = old_loc;     
     
/* Append add-operands to the sum */    
    
  for(;;) {    
    old_loc = *g95_current_locus();         
    c = match_add_op();   
    if (c == 0) break;     
     
    j = match_add_operand(&n);      
    if (j == MATCH_NO) g95_error(expression_syntax);       
    if (j != MATCH_YES) {  
      g95_free_expr(all);       
      return MATCH_ERROR; 
    }

    if (c == -1)    
      total = g95_subtract(all, n);          
    else     
      total = g95_add(all, n);        
        
    if (total == NULL) {
      g95_free_expr(all);  
      g95_free_expr(n);    
      return MATCH_ERROR;  
    } 
 
    all = total; 
    all->where = old_loc;
  }     
     
  *rslt = all;      
  return MATCH_YES;   
}  
  
  
     
     
/* match_level_3()-- Match a level three expression */       
       
static match match_level_3(g95_expr **result) {
g95_expr *all, *j, *total;  
locus pos;     
match d;

  d = match_level_2(&all);         
  if (d != MATCH_YES) return d;          
          
  for(;;) {          
    if (!next_operator(INTRINSIC_CONCAT)) break;          
          
    pos = *g95_current_locus();        
        
    d = match_level_2(&j);     
    if (d == MATCH_NO) {        
      g95_error(expression_syntax); 
      g95_free_expr(all);        
    }         
    if (d != MATCH_YES) return MATCH_ERROR; 
 
    total = g95_concat(all, j);  
    if (total == NULL) {        
      g95_free_expr(all);         
      g95_free_expr(j); 
      return MATCH_ERROR;      
    }     
     
    all = total;          
    all->where = pos;      
  }

  *result = all;          
  return MATCH_YES; 
}         
         
         
 
 
/* match_level_4()-- Match a level 4 expression */  
  
static match match_level_4(g95_expr **result) {     
g95_expr *left, *right, *y;       
g95_intrinsic_op w;       
locus o;        
locus where;
match h;

  h = match_level_3(&left); 
  if (h != MATCH_YES) return h;   
   
  o = *g95_current_locus();          
          
  if (g95_match_intrinsic_op(&w) != MATCH_YES) {    
    *result = left;         
    return MATCH_YES;          
  } 
 
  if (w != INTRINSIC_EQ && w != INTRINSIC_NE && w != INTRINSIC_GE && 
      w != INTRINSIC_LE && w != INTRINSIC_LT && w != INTRINSIC_GT) {      
    g95_set_locus(&o);         
    *result = left;       
    return MATCH_YES;         
  }        
        
  where = *g95_current_locus();        
        
  h = match_level_3(&right);    
  if (h == MATCH_NO) g95_error(expression_syntax);      
  if (h != MATCH_YES) {    
    g95_free_expr(left);         
    return MATCH_ERROR;       
  }    
    
  switch(w) {      
  case INTRINSIC_EQ:       
    y = g95_eq(left, right);       
    break;        
        
  case INTRINSIC_NE:      
    y = g95_ne(left, right);      
    break;         
         
  case INTRINSIC_LT:      
    y = g95_lt(left, right);          
    break;    
    
  case INTRINSIC_LE:       
    y = g95_le(left, right);       
    break; 
 
  case INTRINSIC_GT:
    y = g95_gt(left, right);        
    break;    
    
  case INTRINSIC_GE:        
    y = g95_ge(left, right);         
    break;       
       
  default:  
    g95_internal_error("match_level_4(): Bad operator");       
  }  
  
  if (y == NULL) {    
    g95_free_expr(left); 
    g95_free_expr(right);     
    return MATCH_ERROR;
  }      
      
  y->where = where;    
  *result = y;          
          
  return MATCH_YES;         
}    
    
    
 
 
static match match_and_operand(g95_expr **rslt) { 
g95_expr *p, *u;         
locus where;        
match s;          
int w;        
        
  w = next_operator(INTRINSIC_NOT);   
  where = *g95_current_locus();      
     
  s = match_level_4(&p);          
  if (s != MATCH_YES) return s;          
          
  u = p;          
  if (w) { 
    u = g95_not(p);  
    if (u == NULL) {          
      g95_free_expr(p);          
      return MATCH_ERROR;        
    }   
  }      
      
  u->where = where;        
  *rslt = u;       
       
  return MATCH_YES;        
}          
          
          
        
        
static match match_or_operand(g95_expr **res) {     
g95_expr *all, *x, *total;  
locus loc;          
match k;        
        
  k = match_and_operand(&all);          
  if (k != MATCH_YES) return k;        
        
  for(;;) {          
    if (!next_operator(INTRINSIC_AND)) break;  
    loc = *g95_current_locus();  
  
    k = match_and_operand(&x);       
    if (k == MATCH_NO) g95_error(expression_syntax);        
    if (k != MATCH_YES) {         
      g95_free_expr(all);      
      return MATCH_ERROR;    
    }  
  
    total = g95_and(all, x);   
    if (total == NULL) {  
      g95_free_expr(all);    
      g95_free_expr(x);   
      return MATCH_ERROR;    
    } 
 
    all = total;     
    all->where = loc; 
  }      
      
  *res = all;    
  return MATCH_YES;
}   
   
   
      
      
static match match_equiv_operand(g95_expr **result) { 
g95_expr *all, *u, *total;    
locus old_loc;    
match r;          
          
  r = match_or_operand(&all);  
  if (r != MATCH_YES) return r;  
  
  for(;;) {      
    if (!next_operator(INTRINSIC_OR)) break;          
    old_loc = *g95_current_locus();         
         
    r = match_or_operand(&u);         
    if (r == MATCH_NO) g95_error(expression_syntax);        
    if (r != MATCH_YES) {
      g95_free_expr(all);  
      return MATCH_ERROR; 
    }   
   
    total = g95_or(all, u);          
    if (total == NULL) {  
      g95_free_expr(all);          
      g95_free_expr(u);   
      return MATCH_ERROR;     
    }          
          
    all = total;   
    all->where = old_loc;    
  }        
        
  *result = all;  
  return MATCH_YES;     
}          
          
          


/* match_level_5()-- Match a level 5 expression */    
    
static match match_level_5(g95_expr **r) {     
g95_expr *all, *j, *total;        
locus pos;       
match h;
int t;    
    
  h = match_equiv_operand(&all);  
  if (h != MATCH_YES) return h;    
    
  for(;;) {    
    if (next_operator(INTRINSIC_EQV)) 
      t = INTRINSIC_EQV; 
    else {       
      if (next_operator(INTRINSIC_NEQV)) 
	t = INTRINSIC_NEQV;
      else      
	break;          
    }

    pos = *g95_current_locus();      
        
    h = match_equiv_operand(&j);    
    if (h == MATCH_NO) g95_error(expression_syntax);     
    if (h != MATCH_YES) {        
      g95_free_expr(all);    
      return MATCH_ERROR;        
    }      
      
    if (t == INTRINSIC_EQV)  
      total = g95_eqv(all, j);  
    else   
      total = g95_neqv(all, j);   
   
    if (total == NULL) {    
      g95_free_expr(all);       
      g95_free_expr(j);          
      return MATCH_ERROR;        
    }  
  
    all = total;     
    all->where = pos;          
  } 
 
  *r = all;          
  return MATCH_YES;         
}      
      
      
          
          
/* g95_match_expr()-- Match an expression.  At this level, we are
 * stringing together level 5 expressions separated by binary operators. */     
     
match g95_match_expr(g95_expr **rslt) {  
g95_expr *all, *q;          
g95_user_op *operator;  
locus old_loc;  
match p; 
 
  p = match_level_5(&all);       
  if (p != MATCH_YES) return p; 
 
  for(;;) {
    p = match_defined_operator(&operator);     
    if (p == MATCH_NO) break;   
    if (p == MATCH_ERROR) {
      g95_free_expr(all);    
      return MATCH_ERROR;     
    }         
         
    old_loc = *g95_current_locus();     
     
    p = match_level_5(&q);      
    if (p == MATCH_NO) g95_error(expression_syntax);         
    if (p != MATCH_YES) {  
      g95_free_expr(all);   
      return MATCH_ERROR;       
    }         
         
    all = build_node(INTRINSIC_USER, &old_loc, all, q);     
    all->uop = operator;      
  }     
     
  *rslt = all;        
  return MATCH_YES;       
}         
