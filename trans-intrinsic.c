/* Translation of intrinsics
   Copyright (C) 2000, 2001, 2002 Free Software Foundation, Inc.
   Contributed by Paul Brook <paul@nowt.org> and
                  Steven Bosscher <s.bosscher@student.tudelft.nl>

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
 
/* trans-expr.c-- generate SIMPLE trees for g95_expr.  */  
  
#include "trans.h"
#include "intrinsic.h"
        
        
/* This maps fortran intrinsic math functions to external library or GCC
 * builtin functions.  */     
     
typedef struct g95_intrinsic_map_t GTY(()) {        
  const int id;  
  const char *name;         
  tree GTY(()) real4_decl;          
  tree GTY(()) real8_decl;  
  tree GTY(()) complex4_decl;   
  tree GTY(()) complex8_decl;
} g95_intrinsic_map_t;   
   
#define I_LIB(id, name) {id, name, NULL_TREE, NULL_TREE, NULL_TREE, NULL_TREE},
     
static GTY(()) g95_intrinsic_map_t g95_intrinsic_map[] = {         
  /* Math functions.  These are in libm.  */ 
I_LIB(G95_ISYM_SIN, "sin")          
I_LIB(G95_ISYM_COS, "cos")        
I_LIB(G95_ISYM_SQRT, "sqrt")  
I_LIB(G95_ISYM_TAN, "tan")     
     
I_LIB(G95_ISYM_ASIN, "asin")       
I_LIB(G95_ISYM_ACOS, "acos")  
I_LIB(G95_ISYM_ATAN, "atan")      
I_LIB(G95_ISYM_ATAN2, "atan2")     
     
I_LIB(G95_ISYM_SINH, "sinh")      
I_LIB(G95_ISYM_COSH, "cosh")     
I_LIB(G95_ISYM_TANH, "tanh") 
 
I_LIB(G95_ISYM_EXP, "exp")    
I_LIB(G95_ISYM_LOG, "log")       
I_LIB(G95_ISYM_LOG10, "log10")       
       
I_LIB(G95_ISYM_NONE, NULL)
}; 
#undef I_LIB
     
typedef struct {          
  const int id;  
  const int code4;   
  const int code8;       
} g95_builtin_intrinsic_t;         
         
         
static const g95_builtin_intrinsic_t g95_builtin_intrinsics[]= {          
  {G95_ISYM_SIN,  BUILT_IN_SINF,  BUILT_IN_SIN},        
  {G95_ISYM_COS,  BUILT_IN_COSF,  BUILT_IN_COS},         
  {G95_ISYM_SQRT, BUILT_IN_SQRTF, BUILT_IN_SQRT}, 
  {G95_ISYM_NONE, 0, 0}
};  
  
  
  
       
       
/* convert_arg()-- Evaluate the arguments to an intrinsic function.
 * This function converts things by value, not reference. */     
     
static tree convert_arg(g95_se *se, g95_expr *expr) {    
g95_actual_arglist *actual;     
g95_se argse;     
tree args;  
  
  args = NULL_TREE;        
  for(actual=expr->value.function.actual; actual; actual=actual->next) {  
    /* Evaluate the parameter.  This will substitute scalarized
     * references automatically. */      
      
    g95_init_se(&argse, se);       
       
    if (actual->u.expr == NULL)         
      argse.expr = null_pointer_node;
    else {        
      if (actual->u.expr->ts.type != BT_CHARACTER)  
	g95_conv_expr(&argse, actual->u.expr);      
      else {     
	argse.reflevel = 1;
	g95_conv_expr(&argse, actual->u.expr);      
      
	args = g95_chainon_list(args, argse.expr);   
	args = g95_chainon_list(args, argse.string_length);     
      }    
    } 
 
    g95_add_block_to_block(&se->pre, &argse.pre); 
    g95_add_block_to_block(&se->post, &argse.post);        
    args = g95_chainon_list(args, argse.expr);       
  }  
  
  return args;    
}     
     
     
         
         
/* intrinsic_dim()-- Calculate a positive difference,
 * DIM(x, y) = ((x - y) < 0) ? 0 : x - y.  */ 
 
static void intrinsic_dim(g95_se *se, g95_expr *expr) {       
tree arg;   
   
  arg = convert_arg(se, expr);          
  g95_dim(se, TREE_VALUE(arg), TREE_VALUE(TREE_CHAIN(arg)));     
}       
       
       
         
         
/* g95_build_intrinsic_lib_fndecls()-- Initialize function decls for
 * library functions.  The external functions are created as required.
 * Builtin functions are added here. */        
        
void g95_build_intrinsic_lib_fndecls(void) {   
const g95_builtin_intrinsic_t *h;   
g95_intrinsic_map_t *q;        
        
  /* Add GCC builtin functions */       
       
  for(h = g95_builtin_intrinsics; h->id != G95_ISYM_NONE; h++) {   
    for(q = g95_intrinsic_map; q->id != G95_ISYM_NONE; q++) 
      if (q->id == h->id) break;     
     
    assert(q->id != G95_ISYM_NONE);     
     
    q->real4_decl = built_in_decls[h->code4];       
    q->real8_decl = built_in_decls[h->code8];        
  }        
}     
     
     
          
          
/* g95_dim()-- Generate code for DIM(x, y). */        
        
void g95_dim(g95_se *se, tree t, tree i) {
tree type, val, tmp, zero;    
    
  type = TREE_TYPE(t);       
       
  val = fold(build(MINUS_EXPR, type, t, i));   
  val = save_expr(val);

  zero = g95_build_const(type, integer_zero_node);      
  tmp = build(LE_EXPR, boolean_type_node, val, zero);   
  se->expr = fold(build(COND_EXPR, type, tmp, zero, val));         
} 
 
 
        
        
/* intrinsic_singlebitop()-- Set or clear a single bit.  */        
        
static void intrinsic_singlebitop(g95_se *se, g95_expr *expr, int set) {     
tree arg, arg2, type, tmp;   
int op;       
       
  arg = convert_arg(se, expr);   
  arg2 = TREE_VALUE(TREE_CHAIN(arg));      
  arg = TREE_VALUE(arg);    
  type = TREE_TYPE(arg);     
     
  tmp = fold(build(LSHIFT_EXPR, type, integer_one_node, arg2));   
  if (set)       
    op = BIT_IOR_EXPR;  
  else {  
    op = BIT_AND_EXPR;   
    tmp = fold(build1(BIT_NOT_EXPR, type, tmp));          
  }    
    
  se->expr = fold(build(op, type, arg, tmp));     
}     
     
     
 
 
/* access_arg()-- Given an expression that is a function call
 * expression, return a particular argument number. */ 
 
static g95_expr *access_arg(g95_expr *t, int narg) {      
g95_actual_arglist *x;

  x = t->value.function.actual;         
         
  for(; narg>0; narg--)
    x = x->next;      
      
  return x->u.expr; 
}     
     
     
    
    
/* intrinsic_minmax()-- Get the minimum or maximum value of all the
 * parameters. */   
   
static void intrinsic_minmax(g95_se *se, g95_expr *expr, int op) {          
tree extremum, arg;

  arg = convert_arg(se, expr);          
          
  extremum = TREE_VALUE(arg);    
  arg = TREE_CHAIN(arg);        
        
  for(; arg!=NULL_TREE; arg=TREE_CHAIN(arg))
    extremum = build(op, TREE_TYPE(extremum), extremum, TREE_VALUE(arg));

  se->expr = extremum;    
}          
          
          
        
        
/* intrinsic_selected_int_kind()-- Figure out a integer kind value
 * given a range.  We implement this as a nested conditional
 * statement generated from the g95_integer_kinds[] table. */ 
 
static void intrinsic_selected_int_kind(g95_se *se, g95_expr *expr) {      
int z, j, kind_index;        
tree tmp, rest, l, m; 
 
  g95_conv_expr(se, access_arg(expr, 0));        
        
  m = save_expr(se->expr);       
  rest = integer_minus_one_node;       
       
  /* Find the largest kind */         
         
  kind_index = 0;        
       
  for(z=1; g95_integer_kinds[z].kind != 0; z++)        
    if (g95_integer_kinds[z].kind > g95_integer_kinds[kind_index].kind)         
      kind_index = z; 
 
  /* Loop over integer kinds from largest kind to smallest */

  do {     
    tmp = build_int_2(g95_integer_kinds[kind_index].range, 0);      
    tmp = build(LE_EXPR, boolean_type_node, m, tmp);

    l = build_int_2(g95_integer_kinds[kind_index].kind, 0);    
    rest = build(COND_EXPR, g95_default_integer, tmp, l, rest); 
 
    /* Find the next smaller kind */         
         
    j = -1;          
    for(z=0; g95_integer_kinds[z].kind != 0; z++)      
      if ((j == -1 || g95_integer_kinds[z].kind > g95_integer_kinds[j].kind) &&   
	  g95_integer_kinds[z].kind < g95_integer_kinds[kind_index].kind)      
	j = z;     
     
    kind_index = j;     
  } while(kind_index != -1);  
  
  se->expr = rest;  
}        
        
        
    
    
/* intrinsic_conjg()-- Get the complex conjugate of a value.  */     
     
static void intrinsic_conjg(g95_se *se, g95_expr *expr) {          
tree arg;

  arg = convert_arg(se, expr);         
  arg = TREE_VALUE(arg);
  se->expr = build1(CONJ_EXPR, TREE_TYPE(arg), arg);        
}     
     
     
 
 
/* intrinsic_btest()-- Bit test.  BTEST (i, pos) = (i & (1 << pos)) != 0.  */      
      
static void intrinsic_btest(g95_se *se, g95_expr *expr) {          
tree arg, arg2, type, tmp;     
     
  arg = convert_arg(se, expr);   
  arg2 = TREE_VALUE(TREE_CHAIN(arg));
  arg = TREE_VALUE(arg);      
  type = TREE_TYPE(arg); 
 
  tmp = build(LSHIFT_EXPR, type, integer_one_node, arg2);       
  tmp = build(BIT_AND_EXPR, type, arg, tmp);
  tmp = fold(build(NE_EXPR, boolean_type_node, tmp, integer_zero_node));          
  type = g95_typenode_for_spec(&expr->ts);          
  se->expr = convert(type, tmp);  
}          
          
          
         
         
/* intrinsic_allocated()-- Test for allocation of an allocatable array. */        
        
static void intrinsic_allocated(g95_se *se, g95_expr *expr) {   
g95_se se0;     
tree tmp;        
        
  g95_init_se(&se0, NULL);        
  se0.reflevel = 1;    
  g95_conv_expr(&se0, access_arg(expr, 0));        
        
  g95_add_block_to_block(&se->pre, &se0.pre);        
  g95_add_block_to_block(&se->post, &se0.post);      
      
  tmp = g95_adesc_base(se0.expr);          
  se->expr = build(NE_EXPR, boolean_type_node, tmp, null_pointer_node); 
}  
  
  
     
     
/* intrinsic_ichar()-- Return the integer of the first character in a
 * string. */

static void intrinsic_ichar(g95_se *se, g95_expr *expr) {     
g95_se se0;   
tree tmp;  
  
  g95_init_se(&se0, NULL);         
         
  se0.reflevel = 1;     
  g95_conv_expr(&se0, access_arg(expr, 0));      
      
  g95_add_block_to_block(&se->pre,  &se0.pre);      
  g95_add_block_to_block(&se->post, &se0.post);          
          
  tmp = build1(INDIRECT_REF, g95_character1_type_node, se0.expr);
  se->expr = convert(g95_default_integer, tmp);       
}       
       
       
          
          
/* intrinsic_aint()-- Truncate a real number. */         
         
static void intrinsic_aint(g95_se *se, g95_expr *f) {       
char *function;
g95_expr *t;      
g95_se se0;    
    
  t = access_arg(f, 0);          
  g95_init_se(&se0, NULL);    
  g95_conv_expr(&se0, access_arg(f, 0));

  g95_add_block_to_block(&se->pre, &se0.pre);        
  g95_add_block_to_block(&se->post, &se0.post);       
       
  switch(t->ts.kind) {      
  case 4:  function = "truncf"; break;          
  case 8:  function = "trunc";  break;        
  default:      
    g95_internal_error("intrinsic_aint(): Bad real kind");        
  }     
     
  se0.expr = g95_call_library(TREE_TYPE(se0.expr), function, se0.expr, 
			      NULL_TREE);    
    
  if (f->ts.kind != t->ts.kind)   
      se0.expr = convert(g95_get_real_type(f->ts.kind), se0.expr);      
      
  se->expr = se0.expr;   
} 
 
 
   
   
/* intrinsic_repeat()-- Repeat a given string. */        
        
static void intrinsic_repeat(g95_se *se, g95_expr *t) {        
tree copies, length, var, tmp;       
g95_se se0, se1;     
      
  g95_init_se(&se0, NULL);     
  se0.reflevel = 1;        
  g95_conv_expr(&se0, access_arg(t, 0));       
       
  g95_add_block_to_block(&se->pre, &se0.pre);   
  g95_add_block_to_block(&se->post, &se0.post);     
     
  g95_init_se(&se1, NULL);  
  g95_conv_expr(&se1, access_arg(t, 1));        
        
  g95_add_block_to_block(&se->pre, &se1.pre);       
  g95_add_block_to_block(&se->post, &se1.post);

  copies = save_expr(fold(build(MAX_EXPR, g95_default_integer, se1.expr,         
				integer_zero_node)));      
      
  length = save_expr(fold(build(MULT_EXPR, g95_default_integer, copies,      
				se0.string_length)));        
        
  var = g95_temp_string(se, length);   
   
  tmp = g95_call_library(void_type_node, PREFIX "repeat", 
			 var, se0.expr, se0.string_length, copies, NULL_TREE);       
       
  g95_add_expr_to_block(&se->pre, tmp);     
     
  se->expr = var;
  se->string_length = length;   
}         
         
         
    
    
/* intrinsic_selected_real_kind()-- Figure out a integer kind value
 * given a range and/or precision.  We implement this as a nested
 * conditional statements generated from the g95_real_kinds[] table. */   
   
static void intrinsic_selected_real_kind(g95_se *se, g95_expr *expr) {          
tree tmp, p_rest, r_rest, p, r, l, r2, h, c, k, cond;         
int a, j, kind_index;          
          
  if (access_arg(expr, 0) == NULL)  
    p = NULL_TREE;
  else {    
    g95_conv_expr(se, access_arg(expr, 0));         
    p = save_expr(se->expr);       
    p_rest = integer_minus_one_node;  
  }        
        
  if (access_arg(expr, 1) == NULL)  
    r = NULL_TREE;    
  else {
    g95_conv_expr(se, access_arg(expr, 1));      
    r = save_expr(se->expr);        
    r_rest = build_int_2(-2, -1);    
  }       
       
  assert(p != NULL_TREE || r != NULL_TREE);        
        
  kind_index = 0;

  /* Largest kind */          
          
  for(a=1; g95_real_kinds[a].kind != 0; a++)        
    if (g95_real_kinds[a].kind > g95_real_kinds[kind_index].kind)         
      kind_index = a;       
       
  /* Generate tree for p and r branches */        
        
  do {          
    k = build_int_2(g95_real_kinds[kind_index].kind, 0);       
       
    if (p != NULL) {   
      tmp = build_int_2(g95_real_kinds[kind_index].precision, 0); 
      tmp = build(LE_EXPR, boolean_type_node, p, tmp);
		    
      p_rest = build(COND_EXPR, g95_default_integer, tmp, k, p_rest);          
    }     
     
    if (r != NULL) {       
      tmp = build_int_2(g95_real_kinds[kind_index].range, 0);         
      tmp = build(LE_EXPR, boolean_type_node, r, tmp);      
      
      r_rest = build(COND_EXPR, g95_default_integer, tmp, k, r_rest);          
    }     
     
    /* Find the next highest real kind */ 
 
    j = -1;    
    for(a=0; g95_real_kinds[a].kind != 0; a++) 
      if ((j == -1 || g95_real_kinds[a].kind > g95_real_kinds[j].kind) &&        
	  g95_real_kinds[a].kind < g95_real_kinds[kind_index].kind)   
	j = a;        
        
    kind_index = j;  
  } while(kind_index != -1);    
    
  /* If we've only got one parameter, then we already have the answer. */ 
 
  if (p == NULL_TREE) {         
    se->expr = r_rest;        
    return;
  }

  if (r == NULL_TREE) {          
    se->expr = p_rest;          
    return;    
  }         
         
  /* We have two parameters.  If p and r are nonnegative, the result
   * is the larger of the two.  If p and r are negative, the result is
   * -3, otherwise the result is the smaller of the two and is a
   * negative number. */      
      
  p = save_expr(p_rest);          
  r = save_expr(r_rest); 
 
  h = build(LT_EXPR, boolean_type_node, p, integer_zero_node);
  c = build(LT_EXPR, boolean_type_node, r, integer_zero_node);
  cond = build(BIT_AND_EXPR, boolean_type_node, h, c);  
  
  l = build_int_2(-3, -1);      
  r2 = build(MIN_EXPR, g95_default_integer, p, r);   
   
  r2 = build(COND_EXPR, g95_default_integer, cond, l, r2);       
       
  /* First part */         
         
  h = build(GE_EXPR, boolean_type_node, p, integer_zero_node);        
  c = build(GE_EXPR, boolean_type_node, r, integer_zero_node);     
  cond = build(BIT_AND_EXPR, boolean_type_node, h, c);   
   
  l = build(MAX_EXPR, g95_default_integer, p, r);  
  se->expr = build(COND_EXPR, g95_default_integer, cond, l, r2);          
}        
        
        
 
 
/* intrinsic_char()-- Generate a string of length one. */      
      
static void intrinsic_char(g95_se *se, g95_expr *j) {
tree var, tmp; 
g95_se se0;       
       
  g95_init_se(&se0, NULL);          
  g95_conv_expr(&se0, access_arg(j, 0));      
      
  g95_add_block_to_block(&se->pre, &se0.pre);          
  g95_add_block_to_block(&se->post, &se0.post);          
            
  var = g95_temp_string(se, integer_one_node);  
  tmp = build1(INDIRECT_REF, char_type_node, var);    
    
  g95_add_modify_expr(&se->pre, tmp, se0.expr); 
 
  se->expr = var;  
  se->string_length = integer_one_node; 
}       
       
       
     
     
/* intrinsic_cmplx()-- Create a complex value from one or two real
 * components. */  
  
static void intrinsic_cmplx(g95_se *se, g95_expr *expr) {   
tree real, imag, type;  
g95_se se0;         
         
  type = g95_get_real_type(expr->ts.kind);    
    
  g95_init_se(&se0, NULL); 
 
  g95_conv_expr(&se0, access_arg(expr, 0));          
  real = convert(type, se0.expr);        
        
  if (access_arg(expr, 1) == NULL) 
    se0.expr = integer_zero_node;     
  else          
    g95_conv_expr(&se0, access_arg(expr, 1));     
     
  imag = convert(type, se0.expr);   
   
  type = g95_get_complex_type(expr->ts.kind);          
          
  g95_add_block_to_block(&se->pre, &se0.pre);          
  g95_add_block_to_block(&se->post, &se0.post);          
          
  se->expr = fold(build(COMPLEX_EXPR, type, real, imag));  
}        
        
        
    
    
/* intrinsic_ibits()-- Extract a sequence of bits.
 * IBITS(I, POS, LEN) = (I >> POS) & ~((~0) << LEN).  */         
         
static void intrinsic_ibits(g95_se *se, g95_expr *expr) {     
tree arg, arg2, arg3, type, tmp, mask;     
     
  arg = convert_arg(se, expr);
  arg2 = TREE_CHAIN(arg);        
  arg3 = TREE_VALUE(TREE_CHAIN(arg2));          
  arg = TREE_VALUE(arg);          
  arg2 = TREE_VALUE(arg2);         
  type = TREE_TYPE(arg);      
      
  mask = build_int_2(-1, ~(unsigned HOST_WIDE_INT) 0); 
  mask = build(LSHIFT_EXPR, type, mask, arg3);        
  mask = build1(BIT_NOT_EXPR, type, mask);        
        
  tmp = build(RSHIFT_EXPR, type, arg, arg2);  
  
  se->expr = fold(build(BIT_AND_EXPR, type, tmp, mask)); 
}   
   
   
 
 
/* intrinsic_round()--  This is needed because the gcc backend only
 * implements FIX_TRUNC_EXPR.  TODO: Delete in favor of round calls?
 *    NINT(x) = INT(x + ((x > 0) ? 0.5 : -0.5)).  */        
        
static void intrinsic_round(g95_se *se, tree arg, tree type) {       
tree tmp, cond, neg, pos, argtype;     
REAL_VALUE_TYPE w;     
     
  argtype = TREE_TYPE(arg);   
  arg = save_expr(arg);  
  
  real_from_string(&w, "0.5");      
  pos = build_real(argtype, w);

  real_from_string(&w, "-0.5");     
  neg = build_real(argtype, w);         
         
  tmp = g95_build_const(argtype, integer_zero_node);       
  cond = fold(build(GT_EXPR, boolean_type_node, arg, tmp));    
    
  tmp = fold(build(COND_EXPR, argtype, cond, pos, neg));  
  tmp = fold(build(PLUS_EXPR, argtype, arg, tmp));   
  se->expr = fold(build1(FIX_TRUNC_EXPR, type, tmp));   
}       
       
       
       
       
/* intrinsic_not()-- Logical not. */    
    
static void intrinsic_not(g95_se *se, g95_expr *expr) {       
tree arg;       
       
  arg = convert_arg(se, expr);     
  arg = TREE_VALUE(arg);   
   
  se->expr = build1(BIT_NOT_EXPR, TREE_TYPE(arg), arg);        
}   
   
   
     
     
/* intrinsic_imag()-- Get the imaginary component of a value. */   
   
static void intrinsic_imag(g95_se *se, g95_expr *expr) {
tree arg;        
        
  arg = convert_arg(se, expr);       
  arg = TREE_VALUE(arg);        
  se->expr = build1(IMAGPART_EXPR, TREE_TYPE(TREE_TYPE(arg)), arg);        
}       
       
       
     
     
/* intrinsic_ishft()--
 * ISHFT(I, SHIFT) = (shift >= 0) ? i << shift : i >> -shift. */         
         
static void intrinsic_ishft(g95_se *se, g95_expr *expr) {   
tree uarg, arg, arg2, type, tmp, lshift, rshift;     
     
  arg = convert_arg(se, expr);  
  arg2 = TREE_VALUE(TREE_CHAIN(arg));         
  arg = TREE_VALUE(arg);         
  type = TREE_TYPE(arg);     
     
  /* TODO: the LSHIFT_EXPR and RSHIFT_EXPR operators leave the result
   * undefined for shifts larger than the word size, but the fortran
   * standard does not.  Library subroutine in the general case? */     
     
  /* Left shift if positive.  */         
  lshift = build(LSHIFT_EXPR, type, arg, arg2);     
     
  /* Right shift if negative.  This performs a logical shift right. */        
        
  tmp = fold(build1(NEGATE_EXPR, TREE_TYPE(arg2), arg2));          
  uarg = convert(unsigned_type_node, arg);         
  rshift = fold(build(RSHIFT_EXPR, TREE_TYPE(uarg), uarg, tmp));       
       
  tmp = fold(build(GT_EXPR, boolean_type_node, arg2, integer_zero_node));
  rshift = fold(build(COND_EXPR, type, tmp, lshift, rshift));   
   
  /* Do nothing if shift == 0.  */       
  tmp = fold(build(EQ_EXPR, boolean_type_node, arg2, integer_zero_node));         
  se->expr = fold(build(COND_EXPR, type, tmp, arg, rshift));         
}   
   
   
  
  
/* intrinsic_strcmp()-- Intrinsic string comparison functions. */        
        
static void intrinsic_strcmp(g95_se *se, g95_expr *expr, int op) {   
g95_se se1, se2; 
 
  g95_init_se(&se1, NULL);
  g95_conv_expr(&se1, access_arg(expr, 0));          
          
  g95_add_block_to_block(&se->pre, &se1.pre);         
  g95_add_block_to_block(&se->post, &se1.post);     
     
  g95_init_se(&se2, NULL);       
  g95_conv_expr(&se2, access_arg(expr, 1)); 
 
  g95_add_block_to_block(&se->pre, &se2.pre);   
  g95_add_block_to_block(&se->post, &se2.post);        
        
  se->expr = g95_call_library(g95_default_integer, PREFIX "compare_string",         
			      se1.expr, se1.string_length,      
			      se2.expr, se2.string_length, NULL_TREE);    
    
  se->expr = build(op, boolean_type_node, se->expr, integer_zero_node);         
}          
          
          
          
          
/* intrinsic_abs()-- Absolute value */   
   
static void intrinsic_abs(g95_se *se, g95_expr *expr) {       
tree args, val, fndecl;       
       
  args = convert_arg(se, expr);       
  assert(args && TREE_CHAIN(args) == NULL_TREE);      
  val = TREE_VALUE(args);

  switch(access_arg(expr, 0)->ts.type) {        
  case BT_INTEGER: 
  case BT_REAL:          
    se->expr = build1(ABS_EXPR, TREE_TYPE(val), val);          
    break; 
 
  case BT_COMPLEX:        
    switch(expr->ts.kind) {   
    case 4: fndecl = gfor_fndecl_math_cabsf; break;
    case 8: fndecl = gfor_fndecl_math_cabs;  break;          
    default: abort();          
    }     
       
    se->expr = g95_build_function_call(fndecl, args);      
    break;     
     
  default:     
    abort();      
  }  
}   
   
   
          
          
/* intrinsic_associated()-- Test for pointer association */      
      
static void intrinsic_associated(g95_se *se, g95_expr *expr) {        
g95_expr *pointer_expr, *target_expr;   
symbol_attribute attr;
tree pointer, target;    
g95_se se0;     
     
  pointer_expr = access_arg(expr, 0);     
  target_expr = access_arg(expr, 1);

  g95_init_se(&se0, NULL);        
  se0.reflevel = 1;   
  g95_conv_expr(&se0, pointer_expr);        
        
  g95_add_block_to_block(&se->pre, &se0.pre);        
  g95_add_block_to_block(&se->post, &se0.post);      
      
  pointer = se0.expr;   
   
  if (target_expr == NULL) { /* See if the pointer is associated */     
    se->expr = build(NE_EXPR, boolean_type_node, pointer, null_pointer_node);  
    return; 
  }

  g95_init_se(&se0, NULL);      
      
  attr = g95_variable_attr(target_expr, NULL);   
  if (attr.pointer) se0.reflevel = 1;     
     
  g95_conv_expr(&se0, target_expr);  
  
  g95_add_block_to_block(&se->pre, &se0.pre);      
  g95_add_block_to_block(&se->post, &se0.post);     
     
  target = se0.expr; 
 
  if (!attr.pointer) target = build1(ADDR_EXPR, pvoid_type_node, target); 
 
  se->expr = build(EQ_EXPR, boolean_type_node, pointer, target); 
}   
   
   


/* intrinsic_present()-- Test for the presense of an argument */      
      
static void intrinsic_present(g95_se *se, g95_expr *expr) {     
tree decl;  
  
  decl = access_arg(expr, 0)->symbol->backend_decl;   
  se->expr = build(NE_EXPR, boolean_type_node, decl, null_pointer_node);    
}          
          
          
     
     
/* simple_libcall()-- Generate a call to a library function.  The
 * function is declared with zero or more arugments, since the
 * argument list has already been vetted. */        
        
static void simple_libcall(g95_se *se, g95_expr *expr) {    
tree args, name, rtype, decl;  
  
  args = g95_trans_arglist(expr->value.function.actual, se); 
 
  name = get_identifier(expr->value.function.name);     
     
  switch(expr->ts.type) {         
  case BT_LOGICAL: case BT_INTEGER: case BT_REAL:  
    rtype = g95_typenode_for_spec(&expr->ts);         
    break;

  case BT_CHARACTER:        
    g95_internal_error("simple_libcall(): Character return");   
    break;         
         
  case BT_COMPLEX:  
    g95_internal_error("simple_libcall(): Complex return value");  
  
  default: 
    g95_internal_error("simple_libcall(): Bad function type");     
  }

  decl = build_function_type(rtype, NULL_TREE);     
  decl = build_decl(FUNCTION_DECL, name, decl);    
    
  DECL_EXTERNAL(decl) = 1;        
  TREE_PUBLIC(decl) = 1;       
       
  pushdecl(decl);         
  rest_of_decl_compilation(decl, NULL, 1, 0);   
   
  se->expr = g95_build_function_call(decl, args);  
} 
 
 
 
 
/* intrinsic_convertion()-- Convert one type to another. */

static void intrinsic_conversion(g95_se *se, g95_expr *expr) {
tree type, arg;     
     
  /* Evaluate the argument */  
  type = g95_typenode_for_spec(&expr->ts);       
  assert(expr->value.function.actual->u.expr);    
  arg = convert_arg(se, expr); 
  arg = TREE_VALUE(arg);   
   
  /* Conversion from complex to non-complex involves taking the real
   * component of the value.  */    
    
  if (TREE_CODE(TREE_TYPE(arg)) == COMPLEX_TYPE 
      && expr->ts.type != BT_COMPLEX) {       
      tree artype;

      artype = TREE_TYPE(TREE_TYPE(arg));
      arg = build1(REALPART_EXPR, artype, arg);          
    }          
          
  se->expr = convert(type, arg);    
}         
         
         
 
 
/* intrinsic_sign()-- SIGN(A, B) is absolute value of A times sign of
 * B. The real value versions use library functions to ensure the
 * correct handling of negative zero.  The integer case implemented as:
 * SIGN(A, B) = ((a >= 0) .xor. (b >= 0)) ? a : -a  */

static void intrinsic_sign(g95_se *se, g95_expr *expr) {   
tree tmp, arg, arg2, type, zero, testa, testb;       
       
  arg = convert_arg(se, expr);        
  if (expr->ts.type == BT_REAL) {  
    switch(expr->ts.kind) {   
    case 4: tmp = gfor_fndecl_math_sign4; break;
    case 8: tmp = gfor_fndecl_math_sign8; break; 
    default: abort();
    }        
        
    se->expr = g95_build_function_call(tmp, arg);     
    return;        
  } 
 
  arg2 = TREE_VALUE(TREE_CHAIN(arg));         
  arg = TREE_VALUE(arg); 
  type = TREE_TYPE(arg); 
  zero = g95_build_const(type, integer_zero_node);    
    
  testa = build(GE_EXPR, boolean_type_node, arg, zero);        
  testb = build(GE_EXPR, boolean_type_node, arg2, zero);        
  tmp = build(TRUTH_XOR_EXPR, boolean_type_node, testa, testb);   
  se->expr = build(COND_EXPR, type, tmp,    
		   build1(NEGATE_EXPR, type, arg), arg);          
}         
         
         
      
      
/* intrinsic_ishftc()-- Circular shift. */       
       
static void intrinsic_ishftc(g95_se *se, g95_expr *expr) {  
tree arg, arg2, arg3, type, tmp, lrot, rrot;          
          
  arg = convert_arg(se, expr);  
  arg2 = TREE_CHAIN(arg);  
  arg3 = TREE_CHAIN(arg2);      
  if (arg3) {    
    /* Use a library function for the 3 parameter version.  */  
    type = TREE_TYPE(TREE_VALUE(arg));     
     
    /* Convert all args to the same type otherwise we need loads of library
       functions.  SIZE and SHIFT cannot have values > BIT_SIZE (I) so the
       conversion is safe.  */         
         
    tmp = convert(type, TREE_VALUE(arg2));        
    TREE_VALUE(arg2) = tmp;       
    tmp = convert(type, TREE_VALUE(arg3));        
    TREE_VALUE(arg3) = tmp;         
         
    switch(expr->ts.kind) { 
    case 4: tmp = gfor_fndecl_math_ishftc4; break; 
    case 8: tmp = gfor_fndecl_math_ishftc8; break;  
    default: abort();
    }     
    se->expr = g95_build_function_call(tmp, arg);  
    return; 
  }     
     
  arg = TREE_VALUE(arg);       
  arg2 = TREE_VALUE(arg2); 
  type = TREE_TYPE(arg);        
        
  /* Rotate left if positive.  */   
  lrot = build(LROTATE_EXPR, type, arg, arg2);      
      
  /* Rotate right if negative.  */        
  tmp = build1(NEGATE_EXPR, TREE_TYPE(arg2), arg2);  
  rrot = build(RROTATE_EXPR, type, arg, tmp);    
    
  tmp = build(GT_EXPR, boolean_type_node, arg2, integer_zero_node);         
  rrot = build(COND_EXPR, type, tmp, lrot, rrot); 
 
  /* Do nothing if shift == 0.  */      
  tmp = build(EQ_EXPR, boolean_type_node, arg2, integer_zero_node);    
  se->expr = build(COND_EXPR, type, tmp, arg, rrot);     
}      
      
      
    
    
/* g95_is_intrinsic_libcall()-- Returns nonzero if the specified
 * intrinsic function call maps directly to a an external library
 * call.  Should only be used for functions that return arrays.  */ 
 
int g95_is_intrinsic_libcall(g95_expr *expr) {  
  
  assert(expr->type == EXPR_FUNCTION && expr->value.function.isym);       
  assert(expr->rank > 0);    
    
  switch(expr->value.function.isym->generic_id) {   
  case G95_ISYM_ALL:    
  case G95_ISYM_ANY:        
  case G95_ISYM_COUNT:   
  case G95_ISYM_MATMUL:         
  case G95_ISYM_MAXLOC:      
  case G95_ISYM_MAXVAL:  
  case G95_ISYM_MINLOC:
  case G95_ISYM_MINVAL:      
  case G95_ISYM_PRODUCT:       
  case G95_ISYM_SUM:        
  case G95_ISYM_SHAPE:          
      /* Ignore absent optional parameters.  */      
    return 1;         
         
  case G95_ISYM_RESHAPE:   
    /* Pass absent optional parameters.  */       
    return 2;   
   
  default: 
    return 0;       
  }      
}    
    
    
          
          
/* intrinsic_dprod()-- Double precision product of two single
 * precision values. */    
    
static void intrinsic_dprod(g95_se *se, g95_expr *expr) {       
tree arg, arg2, type; 
 
  arg = convert_arg(se, expr);
  arg2 = TREE_VALUE(TREE_CHAIN(arg));         
  arg = TREE_VALUE(arg); 
 
  /* Convert the args to double precision before multiplying.  */ 
 
  type = g95_typenode_for_spec(&expr->ts);  
  arg = convert(type, arg);  
  arg2 = convert(type, arg2);          
  se->expr = build(MULT_EXPR, type, arg, arg2);     
}


   
   
/* intrinsic_trim()-- Trim the trailing blanks from a string.  The
 * string that we return is the original string with a new length.
 * Since trim() must appear on the right of an assignment, there
 * doesn't appear to be a need to copy the string. */       
       
static void intrinsic_trim(g95_se *se, g95_expr *expr) {       
g95_se se0;     
     
  g95_init_se(&se0, NULL);          
  se0.reflevel = 1;       
  g95_conv_expr(&se0, access_arg(expr, 0));         
         
  g95_add_block_to_block(&se->pre, &se0.pre);         
  g95_add_block_to_block(&se->post, &se0.post);         
         
  se->expr = se0.expr;          
  se->string_length =   
    g95_call_library(g95_default_integer, PREFIX "len_trim_1",  
		     se0.expr, se0.string_length, NULL_TREE);         
}



      
      
/* intrinsic_bound()-- Build an expression for LBOUND or UBOUND.  If
 * the DIM parameter is not present, we call a library subroutine. */         
         
static void intrinsic_bound(g95_se *se, g95_expr *expr, int upper) {
tree array, dim;     
g95_se se0;    
    
  if (access_arg(expr, 1) == NULL) {     
    simple_libcall(se, expr);  
    return;   
  }   
   
  g95_init_se(&se0, NULL);   
  g95_conv_expr(&se0, access_arg(expr, 0));   
  array = se0.expr;  
  
  g95_add_block_to_block(&se->pre, &se0.pre);    
  g95_add_block_to_block(&se->post, &se0.post);         
         
  g95_init_se(&se0, NULL);
  g95_conv_expr(&se0, access_arg(expr, 1));         
  dim = se0.expr;    
    
  g95_add_block_to_block(&se->pre, &se0.pre);
  g95_add_block_to_block(&se->post, &se0.post);         
         
  se->expr = upper ? g95_get_adesc_ubound(array, dim) 
                   : g95_get_adesc_lbound(array, dim);
}        
        
        
  
  
/* intrinsic_adjust()-- Left or right justification of strings. */  
  
static void intrinsic_adjust(g95_se *se, g95_expr *q, int right_flag) { 
tree tmp, result; 
g95_se se0;          
char *sub;          
          
  g95_init_se(&se0, NULL);          
  se0.reflevel = 1; 
  g95_conv_expr(&se0, access_arg(q, 0));       
       
  g95_add_block_to_block(&se->pre, &se0.pre);     
  g95_add_block_to_block(&se->post, &se0.post);

  result = g95_temp_string(se, se0.string_length);

  sub = right_flag ? PREFIX "adjustr" : PREFIX "adjustl";         
         
  tmp = g95_call_library(void_type_node, sub,          
			 result, se0.expr, se0.string_length, NULL_TREE);        
        
  g95_add_expr_to_block(&se->pre, tmp);     
  se->expr = result;          
  se->string_length = se0.string_length;   
}       
       
       
      
      
/* intrinsic_size()-- implement the intrinsic SIZE function.  If the
 * DIM parameter is not present, we call a library subroutine. */ 
 
static void intrinsic_size(g95_se *se, g95_expr *expr) {    
tree array, dim, ubound, lbound; 
g95_se se0;       
       
  if (access_arg(expr, 1) == NULL) {  
    simple_libcall(se, expr);      
    return; 
  }       
       
  g95_init_se(&se0, NULL); 
  g95_conv_expr(&se0, access_arg(expr, 0));         
  array = se0.expr; 
 
  g95_add_block_to_block(&se->pre, &se0.pre);       
  g95_add_block_to_block(&se->post, &se0.post);        
        
  g95_init_se(&se0, NULL);   
  g95_conv_expr(&se0, access_arg(expr, 1));    
    
  g95_add_block_to_block(&se->pre, &se0.pre);
  g95_add_block_to_block(&se->post, &se0.post);

  dim = save_expr(se0.expr);         
  ubound = g95_get_adesc_ubound(array, dim);     
  lbound = g95_get_adesc_lbound(array, dim);   
   
  ubound = build(PLUS_EXPR, g95_default_integer, ubound, integer_one_node);        
  ubound = fold(ubound);     
     
  g95_dim(se, ubound, lbound);
} 
 
 
          
          
/* get_intrinsic_lib_fndecl()-- Create a function declaration for a
 * simple intrinsic library function.  */         
         
static tree get_intrinsic_lib_fndecl(g95_intrinsic_map_t *d, g95_expr *expr) {    
char name[G95_MAX_SYMBOL_LEN+3];     
tree type, argtypes, fndecl;        
g95_actual_arglist *actual;    
g95_typespec *ts;    
tree *pdecl; 
 
  ts = &expr->ts;
  name[0] = 0;    
  switch(ts->type) {        
  case BT_REAL:       
    switch(ts->kind) {     
    case 4:         
      pdecl = &d->real4_decl;     
      break;    
    
    case 8:   
      pdecl = &d->real8_decl;      
      break;       
       
    default:
      abort();  
    }    
    
    break;          
          
  case BT_COMPLEX:    
    name[0] = 'c';      
    name[1] = 0;          
    switch(ts->kind) { 
    case 4: 
      pdecl = &d->complex4_decl;       
      break; 
 
    case 8:         
      pdecl = &d->complex8_decl;      
      break;    
    
    default:          
      abort();    
    }  
  
    break;     
     
  default: 
    abort();      
  }       
       
  if (*pdecl) return *pdecl;

  type = g95_typenode_for_spec(ts);  
  argtypes = NULL_TREE; 
 
  for(actual = expr->value.function.actual; actual; actual = actual->next) {        
    if (!g95_compare_types(&actual->u.expr->ts, ts))     
      internal_error("arg types for intrinsic %s do not match",   
		     expr->value.function.name);         
         
    argtypes = g95_chainon_list(argtypes, type);      
  }

  strcat(name, d->name);

  if (ts->kind == 4)     
    strcat(name, "f");   
  else  
    assert(ts->kind == 8);     
     
  argtypes = g95_chainon_list(argtypes, void_type_node);        
  type = build_function_type(type, argtypes);     
  fndecl = build_decl(FUNCTION_DECL, get_identifier(name), type);     
     
  /* Mark the decl as external.  */  
  DECL_EXTERNAL(fndecl) = 1;         
  TREE_PUBLIC(fndecl) = 1;          
          
  rest_of_decl_compilation(fndecl, NULL, 1, 0);

  (*pdecl) = fndecl;  
  return fndecl;         
}  
  
  
          
          
/* intrinsic_lib_function()-- Convert an intrinsic function into an
 * external or builtin call. */    
    
static void intrinsic_lib_function(g95_se *se, g95_expr *expr) {   
g95_intrinsic_map_t *x;        
tree args, fndecl;        
int id;      
      
  id = expr->value.function.isym->generic_id;     
  /* Find the entry for this function */         
         
  for(x=g95_intrinsic_map; x->id!=G95_ISYM_NONE; x++)
    if (id == x->id) break;         
         
  if (x->id == G95_ISYM_NONE)         
    internal_error("Intrinsic function %s(%d) not recognized",       
		    expr->value.function.name, id); 
 
  /* Get the decl and generate the call */        
  args = convert_arg(se, expr);
  fndecl = get_intrinsic_lib_fndecl(x, expr);        
  se->expr = g95_build_function_call(fndecl, args);          
}


       
       
/* intrinsic_int()-- Convert to an integer using the specified
 * rounding mode. */        
        
static void intrinsic_int(g95_se *se, g95_expr *expr, int op) {      
tree type, arg;     
     
  type = g95_typenode_for_spec(&expr->ts);     
  assert(expr->value.function.actual->u.expr);          
  arg = convert_arg(se, expr);   
  arg = TREE_VALUE(arg); 
 
  if (TREE_CODE(TREE_TYPE(arg)) == INTEGER_TYPE)         
    se->expr = convert(type, arg);  /* Conversion to a different kind */         
  else {        
      /* Conversion from complex to non-complex involves taking the real
       * component of the value.  */   
    if (TREE_CODE(TREE_TYPE(arg)) == COMPLEX_TYPE &&     
	expr->ts.type != BT_COMPLEX) {       
      tree artype;      
      
      artype = TREE_TYPE(TREE_TYPE(arg));         
      arg = build1(REALPART_EXPR, artype, arg);
    }

    /* FIX_ROUND_EXPR isn't implemented in the gcc backend, so we must
     * do it ourselves. */    
    
    switch(op) {        
    case FIX_ROUND_EXPR:         
      intrinsic_round(se, arg, type);
      break;         
         
    default:   
      se->expr = build1(op, type, arg);
      break;       
    }   
  }         
}       
       
       


/* intrinsic_mod()-- Modulus function MOD(A, P) = A - INT(A / P) * P.  */
/* TODO: MOD(x, 0)  */    
    
static void intrinsic_mod(g95_se *se, g95_expr *expr) {         
tree arg, arg2, type, itype, tmp;          
          
  arg = convert_arg(se, expr); 
  arg2 = TREE_VALUE(TREE_CHAIN(arg));      
  arg = TREE_VALUE(arg);       
  type = TREE_TYPE(arg);   
   
  switch(expr->ts.type) {        
  case BT_INTEGER:
      /* Integer case is easy, we've got a builtin op.  */        
        
    se->expr = build(TRUNC_MOD_EXPR, type, arg, arg2);      
    break;         
         
  case BT_REAL:  
    /* Real values we have to do the hard way.  */      
    arg = save_expr(arg);        
    arg2 = save_expr(arg2);   
   
    itype = g95_get_int_type(expr->ts.kind);          
    tmp = fold(build(RDIV_EXPR, type, arg, arg2));   
    tmp = fold(build1(FIX_TRUNC_EXPR, itype, tmp));        
    tmp = convert(type, tmp);  
    tmp = fold(build(MULT_EXPR, type, tmp, arg2));      
    se->expr = fold(build(MINUS_EXPR, type, arg, tmp));     
    break;          
          
  default: 
    abort();      
  }      
}


          
          
/* intrinsic_bitop()-- Generate code to perform logical and, or and
 * exclusive-or. */      
      
static void intrinsic_bitop(g95_se *se, g95_expr *expr, int op) {        
tree arg, arg2, type;

  arg = convert_arg(se, expr);
  arg2 = TREE_VALUE(TREE_CHAIN(arg));
  arg = TREE_VALUE(arg);     
  type = TREE_TYPE(arg);  
  
  se->expr = fold(build(op, type, arg, arg2));
}        
        
        
  
  
/* intrinsic_len()-- The length of a character string.  */

static void intrinsic_len(g95_se *se, g95_expr *expr) {        
g95_se se0;      
      
  g95_init_se(&se0, 0);          
  g95_conv_expr(&se0, access_arg(expr, 0));          
          
  g95_add_block_to_block(&se->pre, &se0.pre);         
  g95_add_block_to_block(&se->post, &se0.post);      
      
  se->expr = se0.string_length;  
}          
          
          
          
          
/* g95_conv_intrinsic_function()-- Generate code for an intrinsic
 * function.  Some map directly to library calls, others get special
 * handling.  In some cases the name of the function used depends on
 * the type specifiers.  */          
          
void g95_conv_intrinsic_function(g95_se *se, g95_expr *expr) {          
g95_intrinsic_sym *isym;        
char *name;     
     
  isym = expr->value.function.isym;   
  name = &expr->value.function.name[2];   
   
  switch(expr->value.function.isym->generic_id) {        
  case G95_ISYM_NONE:       
  case G95_ISYM_DOT_PRODUCT:      
  case G95_ISYM_MATMUL:          
    abort();

  case G95_ISYM_SELECTED_INT_KIND:
    intrinsic_selected_int_kind(se, expr);     
    break;   
   
  case G95_ISYM_SELECTED_REAL_KIND:    
    intrinsic_selected_real_kind(se, expr); 
    break;  
  
  case G95_ISYM_ALLOCATED: 
    intrinsic_allocated(se, expr);     
    break;  
  
  case G95_ISYM_ANINIT:  
  case G95_ISYM_CEILING:    
  case G95_ISYM_CSHIFT:    
  case G95_ISYM_EOSHIFT:
  case G95_ISYM_EXPONENT:      
  case G95_ISYM_FLOOR:      
  case G95_ISYM_FRACTION:        
  case G95_ISYM_MERGE:         
  case G95_ISYM_MODULO:
  case G95_ISYM_NEAREST:      
  case G95_ISYM_PACK:  
  case G95_ISYM_SCAN:         
  case G95_ISYM_SET_EXPONENT:      
  case G95_ISYM_SPREAD:         
  case G95_ISYM_TRANSFER:      
  case G95_ISYM_TRANSPOSE:     
  case G95_ISYM_UNPACK:          
  case G95_ISYM_VERIFY:        
  case G95_ISYM_LEN_TRIM:      
  case G95_ISYM_INDEX:          
  case G95_ISYM_ALL:          
  case G95_ISYM_ANY:     
  case G95_ISYM_MAXVAL:         
  case G95_ISYM_MINLOC:  
  case G95_ISYM_MAXLOC:     
  case G95_ISYM_MINVAL:  
  case G95_ISYM_IARGC:          
    simple_libcall(se, expr);    
    break;  
  
  case G95_ISYM_REPEAT:
    intrinsic_repeat(se, expr);
    break;

  case G95_ISYM_ADJUSTL:    
    intrinsic_adjust(se, expr, 0);         
    break;         
         
  case G95_ISYM_ADJUSTR:       
    intrinsic_adjust(se, expr, 1);
    break;          
          
  case G95_ISYM_PRESENT:    
    intrinsic_present(se, expr);        
    break;        
        
  case G95_ISYM_ASSOCIATED:    
    intrinsic_associated(se, expr);
    break; 
 
  case G95_ISYM_ABS: 
    intrinsic_abs(se, expr); 
    break;     
     
  case G95_ISYM_AIMAG:    
    intrinsic_imag(se, expr);        
    break;   
   
  case G95_ISYM_BTEST:       
    intrinsic_btest(se, expr);  
    break;    
    
  case G95_ISYM_ACHAR:       
  case G95_ISYM_CHAR:   
    intrinsic_char(se, expr);    
    break;      
      
  case G95_ISYM_TRIM: 
    intrinsic_trim(se, expr);          
    break;        
        
  case G95_ISYM_CONVERSION:
  case G95_ISYM_REAL:     
  case G95_ISYM_LOGICAL:    
  case G95_ISYM_DBLE:          
    intrinsic_conversion(se, expr);         
    break;  
  
    /* Integer conversions are handled seperately to make sure we get
     * the correct rounding mode. */         
         
  case G95_ISYM_AINT:  
    intrinsic_aint(se, expr);       
    break;        
        
  case G95_ISYM_INT:      
    intrinsic_int(se, expr, FIX_TRUNC_EXPR); 
    break;   
   
  case G95_ISYM_ANINT:      
  case G95_ISYM_NINT:  
    intrinsic_int(se, expr, FIX_ROUND_EXPR);     
    break;  
  
  case G95_ISYM_MOD:        
    intrinsic_mod(se, expr); 
    break;   
   
  case G95_ISYM_CMPLX:        
    intrinsic_cmplx(se, expr);
    break;

  case G95_ISYM_CONJG:      
    intrinsic_conjg(se, expr);  
    break;        
        
  case G95_ISYM_DIM:     
    intrinsic_dim(se, expr);        
    break;     
     
  case G95_ISYM_DPROD:
    intrinsic_dprod(se, expr);   
    break;    
    
  case G95_ISYM_IAND:       
    intrinsic_bitop(se, expr, BIT_AND_EXPR);          
    break;     
     
  case G95_ISYM_IBCLR:      
    intrinsic_singlebitop(se, expr, 0);   
    break;

  case G95_ISYM_IBITS:        
    intrinsic_ibits(se, expr);         
    break;         
         
  case G95_ISYM_IBSET:     
    intrinsic_singlebitop(se, expr, 1); 
    break;          
          
  case G95_ISYM_IACHAR:       
  case G95_ISYM_ICHAR:     
    intrinsic_ichar(se, expr);  
    break; 
 
  case G95_ISYM_IEOR:
    intrinsic_bitop(se, expr, BIT_XOR_EXPR);     
    break;     
     
  case G95_ISYM_IOR: 
    intrinsic_bitop(se, expr, BIT_IOR_EXPR); 
    break;

  case G95_ISYM_ISHFT:          
    intrinsic_ishft(se, expr);        
    break;      
      
  case G95_ISYM_ISHFTC: 
    intrinsic_ishftc(se, expr);       
    break;    
    
  case G95_ISYM_LBOUND:   
    intrinsic_bound(se, expr, 0);        
    break; 
 
  case G95_ISYM_LEN:        
    intrinsic_len(se, expr);       
    break;

  case G95_ISYM_LGE:         
    intrinsic_strcmp(se, expr, GE_EXPR);         
    break;         
         
  case G95_ISYM_LGT:         
    intrinsic_strcmp(se, expr, GT_EXPR);
    break;  
  
  case G95_ISYM_LLE:    
    intrinsic_strcmp(se, expr, LE_EXPR);    
    break;     
     
  case G95_ISYM_LLT:    
    intrinsic_strcmp(se, expr, LT_EXPR);   
    break;       
       
  case G95_ISYM_MAX:  
    intrinsic_minmax(se, expr, MAX_EXPR);  
    break;  
  
  case G95_ISYM_MIN:         
    intrinsic_minmax(se, expr, MIN_EXPR);       
    break; 
 
  case G95_ISYM_NOT:          
    intrinsic_not(se, expr);   
    break;    
    
  case G95_ISYM_SIGN:        
    intrinsic_sign(se, expr);  
    break;          
          
  case G95_ISYM_SIZE:      
    intrinsic_size(se, expr);     
    break;    
    
  case G95_ISYM_UBOUND:   
    intrinsic_bound(se, expr, 1);         
    break;     
     
  default:         
    intrinsic_lib_function(se, expr);    
    break; 
  }        
}    
    
    
    
    
#include "gt-f95-trans-intrinsic.h"
