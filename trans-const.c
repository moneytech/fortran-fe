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
      
      
          
          
/* g95_build_const()-- Build a constant with given type from an
 * integer constant node.  */ 
 
tree g95_build_const(tree type, tree intval) {          
tree val, zero;  
  
  switch(TREE_CODE(type)) {         
  case INTEGER_TYPE: 
    val = convert(type, intval);       
    break;  
  
  case REAL_TYPE:      
    val = build_real_from_int_cst(type, intval);     
    break;   
   
  case COMPLEX_TYPE: 
    val = build_real_from_int_cst(TREE_TYPE (type), intval);
    zero = build_real_from_int_cst(TREE_TYPE (type), integer_zero_node);      
    val = build_complex(type, val, zero);          
    break;       
       
  default: 
    abort();         
  }  
  
  return val;      
}    
    
    
   
   
/* g95_conv_constant()-- Translate a scalar constant.  Constants never
 * have pre or post chains, but strings have a length which needs to
 * be returned.  If the string_length is not a NULL_TREE, then we want
 * a string of the prescribed length.  Ignored if we are not dealing
 * with a character constant. */  
  
void g95_conv_constant(g95_se *s, g95_expr *expr) {     
tree real, imag;          
int leng;      
      
  if (expr->type == EXPR_NULL) {  
    s->expr = null_pointer_node;
    return;        
  }  
  
  if (expr->type == EXPR_STRUCTURE) {   
    g95_conv_expr(s, expr);      
    assert(s->pre.head == NULL && s->post.head == NULL);   
    return;          
  }        
        
  assert(expr->type == EXPR_CONSTANT);

  switch(expr->ts.type) {    
  case BT_INTEGER:       
    s->expr = g95_conv_mpz_to_tree(expr->value.integer, expr->ts.kind);         
    break;    
    
  case BT_REAL:     
    s->expr = g95_conv_mpf_to_tree(expr->value.real, expr->ts.kind);     
    break;

  case BT_LOGICAL:          
    s->expr = build_int_2(expr->value.logical, 0);      
    break;      
      
  case BT_COMPLEX:          
    real = g95_conv_mpf_to_tree(expr->value.complex.r, expr->ts.kind);
    imag = g95_conv_mpf_to_tree(expr->value.complex.i, expr->ts.kind); 
    s->expr = build_complex(NULL_TREE, real, imag); 
    break;   
   
  case BT_CHARACTER:       
    leng = expr->value.character.length;       
    s->expr = g95_build_string_const(leng, expr->value.character.string); 
    s->string_length = build_int_2(leng, 0); 
    break;         
         
  default: 
    g95_internal_error("g95_conv_constant(): invalid constant: type %d",         
		       expr->ts.type);         
    break;   
  }         
}    
      
      
/* g95_build_string_const()-- Build a string constant */         
         
tree g95_build_string_const(int leng, char *e) {    
tree str, l;         
         
  str = build_string(leng, e);  
  l = build_int_2(leng, 0);     
  TREE_TYPE(str) = build_array_type(g95_character1_type_node,    
				    build_range_type(g95_default_integer,  
						     integer_one_node, l));  
  return str;          
}


    
    
/* g95_resize_string_constant()-- Given a constant string, resize it
 * to the desired size, truncating or padding with spaces. */ 
 
tree g95_resize_string_constant(tree str, tree new_size) {   
int current_len, length;    
tree decl;     
char *s;  
  
  current_len = int_size_in_bytes(TREE_TYPE(str));   
  length = TREE_INT_CST_LOW(new_size);      
      
  if (length == current_len) return str;  
  
  if (length < current_len)       
    return g95_build_string_const(length, (char *) TREE_STRING_POINTER(str));          
          
  s = g95_getmem(length);       
       
  memcpy(s, TREE_STRING_POINTER(str), current_len);         
  memset(s+current_len, ' ', length-current_len);       
       
  decl = g95_build_string_const(length, s);    
  g95_free(s);   
   
  return decl;          
}    
    
    
        
        
/* g95_conv_mpz_to_tree()-- Converts a GMP integer into a backend tree
 * node.  A kind of -1 builds a default integer. */         
         
tree g95_conv_mpz_to_tree(mpz_t c, int knd) { 
mpz_t b, divisor, high, low;    
int negative;         
tree decl;    
    
  mpz_init_set(b, c); 
  negative = 0;  
  
  if (mpz_sgn(b) < 0) {    
    negative = 1; 
    mpz_neg(b, b);       
  } 
 
  mpz_init_set_ui(divisor, 0);  
  mpz_setbit(divisor, HOST_BITS_PER_INT);  
  
  mpz_init(high);    
  mpz_init(low);   
   
  mpz_tdiv_qr(high, low, b, divisor);        
        
  decl = build_int_2(mpz_get_ui(low), mpz_get_ui(high));       
       
  if (knd == -1) knd = g95_default_integer_kind();         
  TREE_TYPE(decl) = g95_get_int_type(knd);  
  
  if (negative) decl = fold(build1(NEGATE_EXPR, TREE_TYPE(decl), decl));          
          
  mpz_clear(b);         
  mpz_clear(high);      
  mpz_clear(low);     
  mpz_clear(divisor); 
 
  return decl; 
}     
     
     


/* g95_conv_mpf_to_tree()-- Converts a real constant into backend form. */         
         
tree g95_conv_mpf_to_tree(mpf_t j, int kind) {   
char *g, *r, *b, *c;         
tree dec, type;     
int d, digits;     
mp_exp_t exp;          
          
  for(d=0; g95_real_kinds[d].kind!=0; d++)          
    if (g95_real_kinds[d].kind == kind)  
      break;

  digits = (g95_real_kinds[d].digits/4) + 1;       
       
  assert(g95_real_kinds[d].radix == 2); 
 
  g = g95_getmem(digits + 20);          
  r = g95_getmem(digits + 20);

  mpf_get_str(g, &exp, 16, digits, j);          
          
  if (*g == '\0')     
    strcpy(r, "0x0P0");  
  else {      
    b = g;       
    c = r;      
      
    if (*b == '-') {         
      *c++ = '-';  
      b++;    
    }       
       
    sprintf(c, "0x0.%sP%d", b, 4*((int) exp));    
  }          
          
  type = g95_get_real_type(kind);   
  dec = build_real(type, REAL_VALUE_ATOF("0", TYPE_MODE(type)));  
  real_from_string(TREE_REAL_CST_PTR(dec), r);         
         
  g95_free(g);  
  g95_free(r);  
  
  return dec;      
}    
    
    
