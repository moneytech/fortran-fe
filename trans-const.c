/* Translation of constants
   Copyright (C) 2000, 2001, 2002 Free Software Foundation, Inc.
   Contributed by Paul Brook

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
 
/* trans_code.c -- convert constant values */  
  
#include "trans.h"
#include "stdlib.h"


     
     
/* g95_resize_string_constant()-- Given a constant string, resize it
 * to the desired size, truncating or padding with spaces. */    
    
tree g95_resize_string_constant(tree string, tree new_size) {        
int current_len, length;
tree dec; 
char *u; 
 
  current_len = int_size_in_bytes(TREE_TYPE(string));          
  length = TREE_INT_CST_LOW(new_size);        
        
  if (length == current_len) return string;         
         
  if (length < current_len)      
    return g95_build_string_const(length, (char *) TREE_STRING_POINTER(string));         
         
  u = g95_getmem(length);    
    
  memcpy(u, TREE_STRING_POINTER(string), current_len);
  memset(u+current_len, ' ', length-current_len);     
     
  dec = g95_build_string_const(length, u);   
  g95_free(u);      
      
  return dec;        
}        
        
        
       
       
/* g95_build_const()-- Build a constant with given type from an
 * integer constant node.  */  
  
tree g95_build_const(tree dtype, tree intval) {         
tree val, zero;       
       
  switch(TREE_CODE(dtype)) { 
  case INTEGER_TYPE:     
    val = convert(dtype, intval);         
    break; 
 
  case REAL_TYPE:         
    val = build_real_from_int_cst(dtype, intval);
    break; 
 
  case COMPLEX_TYPE:     
    val = build_real_from_int_cst(TREE_TYPE (dtype), intval); 
    zero = build_real_from_int_cst(TREE_TYPE (dtype), integer_zero_node);   
    val = build_complex(dtype, val, zero);  
    break;          
          
  default:   
    abort();      
  }       
       
  return val;  
}       
       
       
        
        
/* g95_build_string_const()-- Build a string constant */         
         
tree g95_build_string_const(int leng, char *q) {    
tree str, l;         
         
  str = build_string(leng, q);  
  l = build_int_2(leng, 0);
  TREE_TYPE(str) = build_array_type(g95_character1_type_node,        
				    build_range_type(g95_default_integer,         
						     integer_one_node, l));        
  return str;        
}         
         
         
   
   
/* g95_conv_constant()-- Translate a scalar constant.  Constants never
 * have pre or post chains, but strings have a length which needs to
 * be returned.  If the string_length is not a NULL_TREE, then we want
 * a string of the prescribed length.  Ignored if we are not dealing
 * with a character constant. */ 
 
void g95_conv_constant(g95_se *se, g95_expr *expr) {         
tree real, imag;  
int l;         
         
  if (expr->type == EXPR_NULL) { 
    se->expr = null_pointer_node;          
    return;
  }  
  
  if (expr->type == EXPR_STRUCTURE) {         
    g95_conv_expr(se, expr);    
    assert(se->pre.head == NULL && se->post.head == NULL); 
    return;      
  }      
      
  assert(expr->type == EXPR_CONSTANT);       
       
  switch(expr->ts.type) {
  case BT_INTEGER: 
    se->expr = g95_conv_mpz_to_tree(expr->value.integer, expr->ts.kind);         
    break;

  case BT_REAL:      
    se->expr = g95_conv_mpf_to_tree(expr->value.real, expr->ts.kind);     
    break;     
     
  case BT_LOGICAL:  
    se->expr = build_int_2(expr->value.logical, 0);
    break;        
        
  case BT_COMPLEX:         
    real = g95_conv_mpf_to_tree(expr->value.complex.r, expr->ts.kind);   
    imag = g95_conv_mpf_to_tree(expr->value.complex.i, expr->ts.kind);   
    se->expr = build_complex(NULL_TREE, real, imag);      
    break;     
     
  case BT_CHARACTER:      
    l = expr->value.character.length;         
    se->expr = g95_build_string_const(l, expr->value.character.string);    
    se->string_length = build_int_2(l, 0);  
    break;     
     
  default:         
    g95_internal_error("g95_conv_constant(): invalid constant: type %d",     
		       expr->ts.type);    
    break;      
  }   
}          
      
      
/* g95_conv_mpz_to_tree()-- Converts a GMP integer into a backend tree
 * node.  A kind of -1 builds a default integer. */        
        
tree g95_conv_mpz_to_tree(mpz_t r, int kind) {   
mpz_t l, divisor, high, low; 
int negative;      
tree declr;      
      
  mpz_init_set(l, r); 
  negative = 0;      
      
  if (mpz_sgn(l) < 0) {         
    negative = 1;      
    mpz_neg(l, l);          
  }      
      
  mpz_init_set_ui(divisor, 0);       
  mpz_setbit(divisor, HOST_BITS_PER_INT);

  mpz_init(high);          
  mpz_init(low);          
          
  mpz_tdiv_qr(high, low, l, divisor);  
  
  declr = build_int_2(mpz_get_ui(low), mpz_get_ui(high));    
    
  if (kind == -1) kind = g95_default_integer_kind();      
  TREE_TYPE(declr) = g95_get_int_type(kind);         
         
  if (negative) declr = fold(build1(NEGATE_EXPR, TREE_TYPE(declr), declr));      
      
  mpz_clear(l);       
  mpz_clear(high);  
  mpz_clear(low);       
  mpz_clear(divisor);   
   
  return declr;        
} 
 
 
 
 
/* g95_conv_mpf_to_tree()-- Converts a real constant into backend form. */        
        
tree g95_conv_mpf_to_tree(mpf_t g, int k0) {
char *s, *y, *b, *j;         
tree dec, dtype;   
int o, digits; 
mp_exp_t exp;         
         
  for(o=0; g95_real_kinds[o].kind!=0; o++)         
    if (g95_real_kinds[o].kind == k0) 
      break;       
       
  digits = (g95_real_kinds[o].digits/4) + 5;    
    
  assert(g95_real_kinds[o].radix == 2);       
       
  s = g95_getmem(digits + 20);
  y = g95_getmem(digits + 20);         
         
  mpf_get_str(s, &exp, 16, digits, g);      
      
  if (*s == '\0')   
    strcpy(y, "0x0P0");  
  else {          
    b = s;   
    j = y; 
 
    if (*b == '-') {       
      *j++ = '-';
      b++;      
    }       
       
    sprintf(j, "0x0.%sP%d", b, 4*((int) exp));        
  }          
          
  dtype = g95_get_real_type(k0);     
  dec = build_real(dtype, REAL_VALUE_ATOF("0", TYPE_MODE(dtype)));   
  real_from_string(TREE_REAL_CST_PTR(dec), y);         
         
  g95_free(s);          
  g95_free(y);    
    
  return dec;       
}       
       
       
