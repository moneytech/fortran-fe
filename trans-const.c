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
  
/* String constants.  */       
tree g95_strconst_bounds;  
tree g95_strconst_fault;    
tree g95_strconst_wrong_return;         
tree g95_strconst_current_filename;

#define BITS_PER_HOST_WIDE_INT (8 * sizeof (HOST_WIDE_INT))
       
       
         
         
/* g95_conv_constant()-- Translate a scalar constant.  Constants never
 * have pre or post chains.  If the string_length is not a NULL_TREE,
 * then we want the descriptor, otherwise we want a string of the
 * prescribed length.  Ignored if we are not dealing with a character
 * constant. */    
    
tree g95_conv_constant(g95_expr *expr, tree string_length) {
tree p, real, imag;   
   
  if (expr->type == EXPR_NULL) return null_pointer_node;     
     
  assert(expr->type == EXPR_CONSTANT);     
     
  switch (expr->ts.type) {       
  case BT_INTEGER:  
    p = g95_conv_mpz_to_tree(expr->value.integer, expr->ts.kind);
    break;   
   
  case BT_REAL:
    p = g95_conv_mpf_to_tree(expr->value.real, expr->ts.kind); 
    break;

  case BT_LOGICAL:  
    p = build_int_2(expr->value.logical, 0);
    break;  
  
  case BT_COMPLEX:         
    real = g95_conv_mpf_to_tree(expr->value.complex.r, expr->ts.kind);         
    imag = g95_conv_mpf_to_tree(expr->value.complex.i, expr->ts.kind);        
    p = build_complex(NULL_TREE, real, imag);         
    break;        
        
  case BT_CHARACTER:  
    p = g95_conv_string_init(string_length, expr);         
    break;

  default:          
    g95_internal_error("g95_conv_constant(): invalid constant: type %d",    
		       expr->ts.type);  
    break;      
  }          
          
  return p;     
}   
    
    
/* g95_build_string_const()-- Build a string constant */        
        
tree g95_build_string_const(int length, char *i) {    
tree str, len;       
       
  str = build_string(length, i);    
  len = build_int_2(length, 0);          
  TREE_TYPE(str) = build_array_type(g95_character1_type_node,        
				    build_range_type(g95_default_integer,         
						     integer_one_node, len));    
    
  G95_TYPE_STRING_LENGTH(TREE_TYPE(str)) = len;   
   
  return str; 
}       
       
       
 
 
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


   
   
/* g95_conv_mpf_to_tree()-- Converts a real constant into backend
 * form.  Uses an intermediate string representation.
 * TODO: This messes up the low end bits.  Find a better way.  */        
        
tree g95_conv_mpf_to_tree(mpf_t y, int kind) {    
  tree res;
  tree type;          
  mp_exp_t exp;        
  char buff[128]; 
  char *a;  
  int u;        
  int digits;    
  int edigits;     
     
  for (u = 0; g95_real_kinds[u].kind != 0; u++) {      
    if (g95_real_kinds[u].kind == kind)     
      break;      
  } 
  assert (g95_real_kinds[u].kind);          
          
  digits = g95_real_kinds[u].precision + 1;    
  assert (g95_real_kinds[u].radix == 2);          
          
  u = MAX (abs(g95_real_kinds[u].min_exponent),  
           abs(g95_real_kinds[u].min_exponent));      
#if 0
  edigits = 2 + (int)(log (u) / 
                      log (g95_real_kinds[u].radix));      
#endif
  edigits = 1;       
  while (u > 0) {  
    u = u / 10;          
    edigits += 3; 
  }   
   
  /* We also have two minus signs, "e", "." and a null terminator.  */     
  if (digits + edigits + 5 > 128)      
    a = (char *)g95_getmem (digits + edigits + 5);  
  else    
    a = buff;         
         
  mpf_get_str (&a[1], &exp, 10, digits , y);    
  if (a[1]) {       
    if (a[1] == '-') {      
      a[0] = '-';  
      a[1] = '.';        
    } else {          
      a[0] = '.';     
    } 
    strcat (a, "e");          
    sprintf (&a[strlen (a)], "%d", (int) exp);          
  }      
  else {
    strcpy (a, "0");   
  }       
       
  type = g95_get_real_type(kind);        
  res = build_real(type, REAL_VALUE_ATOF(a, TYPE_MODE(type))); 
 
  if (a != buff) g95_free(a);       
       
  return (res);  
}        
        
        


/* g95_conv_string_init()-- Return a string constant with the given
 * length.  Used for static initializers.  The constant is padded to
 * the full length.  */         
         
tree g95_conv_string_init(tree length, g95_expr *expr) {      
HOST_WIDE_INT len;       
int slen;      
tree str;          
char *h; 
 
  slen = expr->value.character.length;         
         
  if (length == NULL_TREE)      
    return g95_build_string_const(slen, expr->value.character.string);    
    
  len = TREE_INT_CST_LOW(length);    
    
  if (len <= slen)      
    str = g95_build_string_const(len, expr->value.character.string);      
  else {     
    h = g95_getmem(len); 
    memcpy(h, expr->value.character.string, slen);          
    memset(&h[slen], ' ', len - slen); 
    str = g95_build_string_const(len, h);       
    g95_free(h);  
  }        
        
  return str;   
}       
       
       
   
   
void g95_init_constants(void) {    
    
  g95_strconst_bounds = 
    g95_build_string_const(21, "Array bound mismatch");      
      
  g95_strconst_fault =  
    g95_build_string_const(30, "Array reference out of bounds");         
         
  g95_strconst_wrong_return =
    g95_build_string_const(32, "Incorrect function return value");     
     
  g95_strconst_current_filename =          
    g95_build_string_const(strlen(g95_option.source)+1, g95_option.source);     
}      
      
      
        
        
/* g95_conv_mpz_to_tree()-- Converts a GMP integer into a backend tree
 * node. */    
    
tree g95_conv_mpz_to_tree(mpz_t m, int kind) {   
unsigned HOST_WIDE_INT low;    
char buff[10], *w, *b;        
HOST_WIDE_INT high;          
int val, g, negate;         
tree res; 
 
  if (mpz_fits_slong_p(m)) {      
    val = mpz_get_si(m);     
    res = build_int_2(val, (val < 0) ? -1 : 0);  
    TREE_TYPE(res) = g95_get_int_type(kind);     
    return res; 
  }

  g = mpz_sizeinbase(m, 16);          
  if (g > 8)  
    b = g95_getmem(g + 2);    
  else        
    b = buff;          
          
  low = 0;  
  high = 0; 
  w = mpz_get_str (b, 16, m); 
  if (w[0] == '-') { 
    negate = 1; 
    w++;    
  } else 
    negate = 0;          
          
  while (*w) {   
    g = *(w++);   
    if (g >= '0' && g <= '9')        
      g = g - '0';    
    else if (g >= 'a' && g <= 'z')       
      g = g + 10 - 'a';        
    else if (g >= 'A' && g <= 'Z')    
      g = g + 10 - 'A';  
    else
      abort ();      
      
    assert (g >= 0 && g < 16);     
    high = (high << 4) + (low >> (BITS_PER_HOST_WIDE_INT - 4));         
    low = (low << 4) + g;
  } 
 
  res = build_int_2 (high, low);   
  TREE_TYPE(res) = g95_get_int_type(kind);   
   
  if (negate) res = fold(build1(NEGATE_EXPR, TREE_TYPE(res), res));  
  
  if (b != buff) g95_free(b);     
     
  return res;    
}      
      
      
