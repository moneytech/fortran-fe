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
          
          
      
      
/* match_primary()-- Match a primary expression */ 
 
static match match_primary(g95_expr **result) {         
match y;   
   
  y = g95_match_literal_constant(result, 0);  
  if (y != MATCH_NO) return y; 
 
  y = g95_match_array_constructor(result);       
  if (y != MATCH_NO) return y;  
  
  y = g95_match_rvalue(result);    
  if (y != MATCH_NO) return y;      
      
/* Match an expression in parenthesis */        
        
  if (g95_match_char('(') != MATCH_YES) return MATCH_NO;          
          
  y = g95_match_expr(result);    
  if (y == MATCH_NO) goto syntax;   
  if (y == MATCH_ERROR) return y;          
          
  y = g95_match_char(')');      
  if (y == MATCH_NO)        
    g95_error("Expected a right parenthesis in expression at %C");       
       
  if (y != MATCH_YES) {         
    g95_free_expr(*result);  
    return MATCH_ERROR;   
  }

  return MATCH_YES;        
        
syntax:     
  g95_error(expression_syntax);   
  return MATCH_ERROR;  
}         
         
         
     
     
/* g95_match_defined_op_name()-- Match a user-defined operator name.
 * This is a normal name with a few restrictions.  The error_flag
 * controls whether an error is raised if 'true' or 'false' are used
 * or not. */        
        
match g95_match_defined_op_name(char *result, int error_flag) {    
static char *badops[] = { 
 "and", "or", "not", "eqv", "neqv", "eq", "ne", "ge", "le", "lt", "gt", NULL };    
    
char nm[G95_MAX_SYMBOL_LEN+1]; 
g95_locus oldl;      
match s;      
int y;  
  
  oldl = g95_current_locus;  
  
  s = g95_match(" . %n .", nm);        
  if (s != MATCH_YES) return s;    
    
/* .true. and .false. have interpretations as constants.  Trying to
 * use these as operators will fail at a later time */    
    
  if (strcmp(nm, "true") == 0 || strcmp(nm, "false") == 0) {          
    if (error_flag) goto error;        
    g95_current_locus = oldl;  
    return MATCH_NO;       
  }          
          
  for(y=0; badops[y]; y++)     
    if (strcmp(badops[y], nm) == 0) goto error;         
         
  for(y=0; nm[y]; y++)      
    if (!isalpha(nm[y])) {          
      g95_error("Bad character '%c' in OPERATOR name at %C", nm[y]);  
      return MATCH_ERROR;  
    }     
     
  strcpy(result, nm);    
  return MATCH_YES;      
      
error:      
  g95_error("The name '%s' cannot be used as a defined operator at %C",   
	    nm);          
          
  g95_current_locus = oldl;        
  return MATCH_ERROR;
}         
         
         
      
      
/* next_operator()-- Checks to see if the given operator is next on
 * the input.  If this is not the case, the parse pointer remains
 * where it was. */     
     
static int next_operator(g95_intrinsic_op v) {         
g95_intrinsic_op n;        
g95_locus old_loc;        
        
  old_loc = g95_current_locus;    
  if (g95_match_intrinsic_op(&n) == MATCH_YES && v == n) return 1;      
      
  g95_current_locus = old_loc;        
  return 0;         
}      
      
      
  
  
static int match_add_op(void) {  
  
  if (next_operator(INTRINSIC_MINUS)) return -1; 
  if (next_operator(INTRINSIC_PLUS)) return 1;     
  return 0;        
}          
          
          


/* build_node()-- Build an operator expression node. */         
         
static g95_expr *build_node(g95_intrinsic_op oper, g95_locus *where,      
			    g95_expr *op, g95_expr *op2) { 
g95_expr *n1; 
 
  n1 = g95_get_expr();          
  n1->type = EXPR_OP;
  n1->operator = oper;   
  n1->where = *where;         
           
  n1->op1 = op;          
  n1->op2 = op2;          
          
  return n1;  
}        
        
        
     
     
/* match_defined_operator()-- Match a user defined operator.  The
 * symbol found must be an operator already. */ 
 
static match match_defined_operator(g95_user_op **rslt) {          
char name[G95_MAX_SYMBOL_LEN+1];      
match m;     
     
  m = g95_match_defined_op_name(name, 0);      
  if (m != MATCH_YES) return m;       
       
  *rslt = g95_get_uop(name);    
  return MATCH_YES;       
}


 
 
/* match_level_1()-- Match a level 1 expression */

static match match_level_1(g95_expr **result) {        
g95_user_op *op;    
g95_expr *z, *o;         
g95_locus where; 
match x;   
   
  where = g95_current_locus;        
  op = NULL;     
  x = match_defined_operator(&op);        
  if (x == MATCH_ERROR) return x;   
   
  x = match_primary(&z);         
  if (x != MATCH_YES) return x;         
         
  if (op == NULL)     
    *result = z;       
  else {     
    o = build_node(INTRINSIC_USER, &where, z, NULL);
    o->uop = op;  
    *result = o;       
  }

  return MATCH_YES;    
}


        
        
static match match_mult_operand(g95_expr **res) {          
g95_expr *o, *exp, *u;          
g95_locus where;
match p;    
    
  p = match_level_1(&o); 
  if (p != MATCH_YES) return p;

  if (!next_operator(INTRINSIC_POWER)) {          
    *res = o;      
    return MATCH_YES;        
  } 
 
  where = g95_current_locus;       
       
  p = match_mult_operand(&exp);
  if (p == MATCH_NO) g95_error("Expected exponent in expression at %C");      
  if (p != MATCH_YES) {      
    g95_free_expr(o);    
    return MATCH_ERROR;          
  }          
          
  u = g95_power(o, exp);       
  if (u == NULL) {       
    g95_free_expr(o);  
    g95_free_expr(exp);       
    return MATCH_ERROR;  
  }   
   
  u->where = where;        
  *res = u;  
  
  return MATCH_YES;     
}       
       
       
 
 
static match match_add_operand(g95_expr **res) {  
g95_expr *all, *f, *total;    
g95_locus where, oldl;         
match u;         
int g;       
       
  u = match_mult_operand(&all);     
  if (u != MATCH_YES) return u;  
  
  for(;;) {    /* Build up a string of products or quotients */
    g = 0;         
         
    oldl = g95_current_locus;          
          
    if (next_operator(INTRINSIC_TIMES))   
      g = INTRINSIC_TIMES;        
    else { 
      if (next_operator(INTRINSIC_DIVIDE)) 
	g = INTRINSIC_DIVIDE;        
      else         
	break;         
    }   
   
    where = g95_current_locus;  
  
    u = match_mult_operand(&f);     
    if (u == MATCH_NO) {      
      g95_current_locus = oldl;
      break;    
    }          
          
    if (u == MATCH_ERROR) {   
      g95_free_expr(all);       
      return MATCH_ERROR;       
    }          
          
    if (g == INTRINSIC_TIMES)       
      total = g95_multiply(all, f);         
    else   
      total = g95_divide(all, f);         
         
    if (total == NULL) {    
      g95_free_expr(all);
      g95_free_expr(f);      
      return MATCH_ERROR;         
    } 
 
    all = total;
    all->where = where;  
  }    
    
  *res = all;      
  return MATCH_YES;       
}   
   
   
         
         
/* match_level_2()-- Match a level 2 expression.  */ 
 
static match match_level_2(g95_expr **rslt) { 
g95_expr *all, *b, *total;
g95_locus where; 
match v;       
int j;    
    
  where = g95_current_locus;   
  j = match_add_op();    
    
  v = match_add_operand(&b);    
  if (j != 0 && v == MATCH_NO) {       
    g95_error(expression_syntax);       
    v = MATCH_ERROR;          
  }      
      
  if (v != MATCH_YES) return v;    
    
  if (j == 0)        
    all = b; 
  else {     
    if (j == -1)   
      all = g95_uminus(b);    
    else   
      all = g95_uplus(b);     
     
    if (all == NULL) { 
      g95_free_expr(b);          
      return MATCH_ERROR;        
    }     
  }

  all->where = where;     
     
/* Append add-operands to the sum */

  for(;;) { 
    where = g95_current_locus;          
    j = match_add_op();        
    if (j == 0) break; 
 
    v = match_add_operand(&b);        
    if (v == MATCH_NO) g95_error(expression_syntax);       
    if (v != MATCH_YES) {         
      g95_free_expr(all);   
      return MATCH_ERROR;  
    }          
          
    if (j == -1)        
      total = g95_subtract(all, b);     
    else        
      total = g95_add(all, b);   
   
    if (total == NULL) {         
      g95_free_expr(all);     
      g95_free_expr(b);          
      return MATCH_ERROR;     
    }         
         
    all = total;   
    all->where = where;          
  }      
      
  *rslt = all;  
  return MATCH_YES;       
} 
 
 
     
     
/* match_level_3()-- Match a level three expression */

static match match_level_3(g95_expr **result) {      
g95_expr *all, *p, *total;          
g95_locus where;  
match y; 
 
  y = match_level_2(&all);         
  if (y != MATCH_YES) return y;        
        
  for(;;) {         
    if (!next_operator(INTRINSIC_CONCAT)) break;    
    
    where = g95_current_locus;      
      
    y = match_level_2(&p);          
    if (y == MATCH_NO) {   
      g95_error(expression_syntax);  
      g95_free_expr(all);         
    } 
    if (y != MATCH_YES) return MATCH_ERROR;  
  
    total = g95_concat(all, p);
    if (total == NULL) {    
      g95_free_expr(all);   
      g95_free_expr(p);  
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
g95_expr *left, *right, *t;          
g95_locus old, where;     
g95_intrinsic_op l;  
match n;

  n = match_level_3(&left);  
  if (n != MATCH_YES) return n;    
    
  old = g95_current_locus;      
      
  if (g95_match_intrinsic_op(&l) != MATCH_YES) {
    *result = left; 
    return MATCH_YES;    
  }     
     
  if (l != INTRINSIC_EQ && l != INTRINSIC_NE && l != INTRINSIC_GE && 
      l != INTRINSIC_LE && l != INTRINSIC_LT && l != INTRINSIC_GT) {         
    g95_current_locus = old;      
    *result = left;
    return MATCH_YES;       
  }     
     
  where = g95_current_locus;         
         
  n = match_level_3(&right); 
  if (n == MATCH_NO) g95_error(expression_syntax); 
  if (n != MATCH_YES) {   
    g95_free_expr(left);          
    return MATCH_ERROR;     
  }     
     
  switch(l) {        
  case INTRINSIC_EQ:       
    t = g95_eq(left, right);     
    break;       
       
  case INTRINSIC_NE:         
    t = g95_ne(left, right);         
    break;      
      
  case INTRINSIC_LT:   
    t = g95_lt(left, right);     
    break;        
        
  case INTRINSIC_LE:        
    t = g95_le(left, right);
    break;         
         
  case INTRINSIC_GT:        
    t = g95_gt(left, right);         
    break;  
  
  case INTRINSIC_GE:   
    t = g95_ge(left, right);      
    break;       
       
  default:  
    g95_internal_error("match_level_4(): Bad operator");       
  }   
   
  if (t == NULL) {
    g95_free_expr(left);     
    g95_free_expr(right);          
    return MATCH_ERROR;         
  } 
 
  t->where = where;    
  *result = t;  
  
  return MATCH_YES;   
}        
        
        
     
     
static match match_and_operand(g95_expr **result) {          
g95_expr *u, *l;         
g95_locus where;    
match o;   
int d;          
          
  d = next_operator(INTRINSIC_NOT);      
  where = g95_current_locus;           
          
  o = match_level_4(&u);   
  if (o != MATCH_YES) return o;      
      
  l = u;         
  if (d) {  
    l = g95_not(u);   
    if (l == NULL) {  
      g95_free_expr(u);        
      return MATCH_ERROR;       
    }        
  }      
      
  l->where = where;       
  *result = l;

  return MATCH_YES;    
}     
     
     
       
       
static match match_or_operand(g95_expr **res) {       
g95_expr *all, *e, *total; 
g95_locus where;  
match k;   
   
  k = match_and_operand(&all);          
  if (k != MATCH_YES) return k;     
     
  for(;;) {   
    if (!next_operator(INTRINSIC_AND)) break;  
    where = g95_current_locus;      
      
    k = match_and_operand(&e);        
    if (k == MATCH_NO) g95_error(expression_syntax);  
    if (k != MATCH_YES) {        
      g95_free_expr(all);       
      return MATCH_ERROR;
    }    
    
    total = g95_and(all, e);  
    if (total == NULL) {       
      g95_free_expr(all);     
      g95_free_expr(e);        
      return MATCH_ERROR;       
    }  
  
    all = total;
    all->where = where;  
  }     
     
  *res = all; 
  return MATCH_YES;    
} 
 
 


static match match_equiv_operand(g95_expr **res) {     
g95_expr *all, *q, *total;        
g95_locus where; 
match l;  
  
  l = match_or_operand(&all);
  if (l != MATCH_YES) return l;      
      
  for(;;) {          
    if (!next_operator(INTRINSIC_OR)) break;        
    where = g95_current_locus;          
          
    l = match_or_operand(&q);     
    if (l == MATCH_NO) g95_error(expression_syntax); 
    if (l != MATCH_YES) {   
      g95_free_expr(all);      
      return MATCH_ERROR;          
    }         
         
    total = g95_or(all, q);   
    if (total == NULL) {         
      g95_free_expr(all);  
      g95_free_expr(q);     
      return MATCH_ERROR; 
    }      
      
    all = total;      
    all->where = where;         
  }      
      
  *res = all;       
  return MATCH_YES;          
} 
 
 
       
       
/* match_level_5()-- Match a level 5 expression */

static match match_level_5(g95_expr **r) {   
g95_expr *all, *n, *total;        
g95_locus where;    
match k;      
int p;       
       
  k = match_equiv_operand(&all);          
  if (k != MATCH_YES) return k;         
         
  for(;;) {        
    if (next_operator(INTRINSIC_EQV)) 
      p = INTRINSIC_EQV;      
    else {    
      if (next_operator(INTRINSIC_NEQV))    
	p = INTRINSIC_NEQV;     
      else       
	break;       
    }      
      
    where = g95_current_locus;          
            
    k = match_equiv_operand(&n); 
    if (k == MATCH_NO) g95_error(expression_syntax);   
    if (k != MATCH_YES) { 
      g95_free_expr(all);        
      return MATCH_ERROR;
    }    
    
    if (p == INTRINSIC_EQV)    
      total = g95_eqv(all, n); 
    else
      total = g95_neqv(all, n);      
      
    if (total == NULL) {    
      g95_free_expr(all);     
      g95_free_expr(n);
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
       
match g95_match_expr(g95_expr **rslt) {   
g95_expr *all, *f;          
g95_user_op *op;       
g95_locus where;        
match s;      
      
  s = match_level_5(&all);      
  if (s != MATCH_YES) return s;

  for(;;) {  
    s = match_defined_operator(&op);      
    if (s == MATCH_NO) break; 
    if (s == MATCH_ERROR) {   
      g95_free_expr(all);      
      return MATCH_ERROR;          
    }    
    
    where = g95_current_locus;  
  
    s = match_level_5(&f);      
    if (s == MATCH_NO) g95_error(expression_syntax);     
    if (s != MATCH_YES) {    
      g95_free_expr(all);     
      return MATCH_ERROR;   
    }   
   
    all = build_node(INTRINSIC_USER, &where, all, f);      
    all->uop = op;       
  }          
          
  *rslt = all;  
  return MATCH_YES;       
}      
