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
   
   
         
         
/* g95_conv_mpz_to_tree()-- Converts a GMP integer into a backend tree
 * node. */

tree g95_conv_mpz_to_tree(mpz_t h, int k) {  
unsigned HOST_WIDE_INT low;   
char buff[10], *a, *u;  
HOST_WIDE_INT high;
int val, g, negate;
tree res;  
  
  if (mpz_fits_slong_p(h)) {    
    val = mpz_get_si(h);      
    res = build_int_2(val, (val < 0) ? -1 : 0); 
    TREE_TYPE(res) = g95_get_int_type(k);         
    return res;   
  }   
   
  g = mpz_sizeinbase(h, 16);  
  if (g > 8) 
    u = g95_getmem(g + 2);        
  else    
    u = buff;       
       
  low = 0;          
  high = 0;          
  a = mpz_get_str (u, 16, h);      
  if (a[0] == '-') {    
    negate = 1;     
    a++;    
  } else   
    negate = 0;

  while (*a) {      
    g = *(a++);    
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
        
  res = build_int_2(high, low); 
  TREE_TYPE(res) = g95_get_int_type(k);    
    
  if (negate) res = fold(build1(NEGATE_EXPR, TREE_TYPE(res), res));      
      
  if (u != buff) g95_free(u);          
          
  return res;      
}       
       
       
   
   
/* g95_build_string_const()-- Build a string constant */      
      
tree g95_build_string_const(int leng, char *d) {        
tree str, len;

  str = build_string(leng, d);     
  len = build_int_2(leng, 0);    
  TREE_TYPE(str) = build_array_type(g95_character1_type_node,
				    build_range_type(g95_default_integer,         
						     integer_one_node, len));          
  return str;        
}       
       
       
          
          
/* conv_string()-- Return a string constant with the given length.
 * If the length is NULL_TREE, then the length of the string is the
 * natural width of the string.  Otherwise the string is truncated or
 * blank padded. */   
   
static void conv_string(g95_se *se0, tree length, g95_expr *e1) {    
HOST_WIDE_INT leng;   
int slen; 
tree str;       
char *o;          
          
  slen = e1->value.character.length;    
    
  if (length == NULL_TREE) {       
    se0->expr = g95_build_string_const(slen, e1->value.character.string); 
    se0->string_length = build_int_2(slen, 0);          
    return;          
  }

  leng = TREE_INT_CST_LOW(length);        
        
  if (leng <= slen) 
    str = g95_build_string_const(leng, e1->value.character.string);       
  else {      
    o = g95_getmem(leng);         
    memcpy(o, e1->value.character.string, slen);     
    memset(&o[slen], ' ', leng - slen);
    str = g95_build_string_const(leng, o);       
    g95_free(o);          
  }        
        
  se0->expr = str;   
  se0->string_length = length; 
} 
 
 
       
       
/* g95_build_const()-- Build a constant with given type from an
 * integer constant node.  */       
       
tree g95_build_const(tree typ, tree intval) {    
tree val, zero;  
  
  switch(TREE_CODE(typ)) {     
  case INTEGER_TYPE:    
    val = convert(typ, intval); 
    break;     
     
  case REAL_TYPE:        
    val = build_real_from_int_cst(typ, intval);      
    break;       
       
  case COMPLEX_TYPE:       
    val = build_real_from_int_cst(TREE_TYPE (typ), intval);
    zero = build_real_from_int_cst(TREE_TYPE (typ), integer_zero_node); 
    val = build_complex(typ, val, zero);        
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
   
void g95_conv_constant(g95_se *se, g95_expr *e, tree string_length) {
tree real, imag;        
        
  if (e->type == EXPR_NULL) {          
    se->expr = null_pointer_node;
    return;         
  }       
       
  assert(e->type == EXPR_CONSTANT);      
      
  switch (e->ts.type) {       
  case BT_INTEGER:         
    se->expr = g95_conv_mpz_to_tree(e->value.integer, e->ts.kind); 
    break;       
       
  case BT_REAL:       
    se->expr = g95_conv_mpf_to_tree(e->value.real, e->ts.kind);       
    break;          
          
  case BT_LOGICAL:    
    se->expr = build_int_2(e->value.logical, 0);      
    break;

  case BT_COMPLEX:        
    real = g95_conv_mpf_to_tree(e->value.complex.r, e->ts.kind);   
    imag = g95_conv_mpf_to_tree(e->value.complex.i, e->ts.kind); 
    se->expr = build_complex(NULL_TREE, real, imag);       
    break;     
     
  case BT_CHARACTER:      
    conv_string(se, string_length, e);          
    break;      
      
  default:         
    g95_internal_error("g95_conv_constant(): invalid constant: type %d",         
		       e->ts.type);  
    break;    
  }
}         
         
         
void g95_init_constants(void) {         
         
} 
 
 
         
         
/* g95_conv_mpf_to_tree()-- Converts a real constant into backend
 * form.  Uses an intermediate string representation.
 * TODO: This messes up the low end bits.  Find a better way.  */

tree g95_conv_mpf_to_tree(mpf_t o, int knd) {  
  tree res;
  tree dtype;     
  mp_exp_t exp;         
  char buff[128];   
  char *c;   
  int k;  
  int digits;        
  int edigits;   
   
  for (k = 0; g95_real_kinds[k].kind != 0; k++) {     
    if (g95_real_kinds[k].kind == knd)          
      break;    
  }   
  assert (g95_real_kinds[k].kind);       
       
  digits = g95_real_kinds[k].precision + 1;      
  assert (g95_real_kinds[k].radix == 2);

  k = MAX (abs(g95_real_kinds[k].min_exponent),    
           abs(g95_real_kinds[k].min_exponent));
#if 0
  edigits = 2 + (int)(log (k) /       
                      log (g95_real_kinds[k].radix)); 
#endif
  edigits = 1;   
  while (k > 0) { 
    k = k / 10;     
    edigits += 3;      
  }        
        
  /* We also have two minus signs, "e", "." and a null terminator.  */
  if (digits + edigits + 5 > 128)   
    c = (char *)g95_getmem (digits + edigits + 5); 
  else          
    c = buff;       
       
  mpf_get_str (&c[1], &exp, 10, digits , o);   
  if (c[1]) {   
    if (c[1] == '-') {          
      c[0] = '-'; 
      c[1] = '.';    
    } else {    
      c[0] = '.';   
    }   
    strcat (c, "e");   
    sprintf (&c[strlen (c)], "%d", (int) exp);        
  } 
  else { 
    strcpy (c, "0");   
  }         
         
  dtype = g95_get_real_type(knd);        
  res = build_real(dtype, REAL_VALUE_ATOF(c, TYPE_MODE(dtype)));         
         
  if (c != buff) g95_free(c);          
          
  return (res);   
}         
         
         
