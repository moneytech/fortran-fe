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
      
      
      
   
   
/* simple_libcall()-- Generate a call to a library function.  The
 * function is declared with zero or more arugments, since the
 * argument list has already been vetted. */  
  
static void simple_libcall(g95_se *se1, g95_expr *e1) {   
tree arg, name, rtype, dec;     
     
  arg = g95_trans_arglist(e1->value.function.actual, se1);        
        
  name = get_identifier(e1->value.function.name);         
         
  switch(e1->ts.type) {       
  case BT_LOGICAL: case BT_INTEGER: case BT_REAL:   
    rtype = g95_typenode_for_spec(&e1->ts);     
    break; 
 
  case BT_CHARACTER:      
    g95_internal_error("simple_libcall(): Character return"); 
    break;      
      
  case BT_COMPLEX:          
    g95_internal_error("simple_libcall(): Complex return value");   
   
  default:   
    g95_internal_error("simple_libcall(): Bad function type");       
  }

  dec = build_function_type(rtype, NULL_TREE);     
  dec = build_decl(FUNCTION_DECL, name, dec);         
         
  DECL_EXTERNAL(dec) = 1;     
  TREE_PUBLIC(dec) = 1;   
   
  pushdecl(dec);  
  rest_of_decl_compilation(dec, NULL, 1, 0);

  se1->expr = g95_build_function_call(dec, arg); 
}    
    
    
      
      
/* access_arg()-- Given an expression that is a function call
 * expression, return a particular argument number. */    
    
static g95_expr *access_arg(g95_expr *q, int narg) {    
g95_actual_arglist *j;      
      
  j = q->value.function.actual;

  for(; narg>0; narg--)        
    j = j->next;         
         
  return j->u.expr;
}




/* intrinsic_selected_real_kind()-- Figure out a integer kind value
 * given a range and/or precision.  We implement this as a nested
 * conditional statements generated from the g95_real_kinds[] table. */

static void intrinsic_selected_real_kind(g95_se *s, g95_expr *e2) {      
tree tmp0, p_rest, r_rest, p, g, e, u, c, d, z, cond;         
int m, v, kind_index;   
   
  if (access_arg(e2, 0) == NULL)     
    p = NULL_TREE;       
  else {    
    g95_conv_expr(s, access_arg(e2, 0));
    p = save_expr(s->expr);     
    p_rest = integer_minus_one_node;       
  }          
          
  if (access_arg(e2, 1) == NULL)     
    g = NULL_TREE;     
  else {         
    g95_conv_expr(s, access_arg(e2, 1));  
    g = save_expr(s->expr);  
    r_rest = build_int_2(-2, -1);         
  }  
  
  assert(p != NULL_TREE || g != NULL_TREE);   
   
  kind_index = 0;     
     
  /* Largest kind */        
        
  for(m=1; g95_real_kinds[m].kind != 0; m++)  
    if (g95_real_kinds[m].kind > g95_real_kinds[kind_index].kind)      
      kind_index = m;        
        
  /* Generate tree for p and r branches */        
        
  do {          
    z = build_int_2(g95_real_kinds[kind_index].kind, 0);  
  
    if (p != NULL) {         
      tmp0 = build_int_2(g95_real_kinds[kind_index].precision, 0); 
      tmp0 = build(LE_EXPR, boolean_type_node, p, tmp0);      
		          
      p_rest = build(COND_EXPR, g95_default_integer, tmp0, z, p_rest);         
    }

    if (g != NULL) {        
      tmp0 = build_int_2(g95_real_kinds[kind_index].range, 0);     
      tmp0 = build(LE_EXPR, boolean_type_node, g, tmp0);

      r_rest = build(COND_EXPR, g95_default_integer, tmp0, z, r_rest);  
    }       
       
    /* Find the next highest real kind */          
          
    v = -1;        
    for(m=0; g95_real_kinds[m].kind != 0; m++)    
      if ((v == -1 || g95_real_kinds[m].kind > g95_real_kinds[v].kind) &&      
	  g95_real_kinds[m].kind < g95_real_kinds[kind_index].kind)       
	v = m;  
  
    kind_index = v;  
  } while(kind_index != -1);  
  
  /* If we've only got one parameter, then we already have the answer. */

  if (p == NULL_TREE) {         
    s->expr = r_rest;      
    return; 
  }      
      
  if (g == NULL_TREE) {        
    s->expr = p_rest;          
    return;  
  }          
          
  /* We have two parameters.  If p and r are nonnegative, the result
   * is the larger of the two.  If p and r are negative, the result is
   * -3, otherwise the result is the smaller of the two and is a
   * negative number. */     
     
  p = save_expr(p_rest);  
  g = save_expr(r_rest);

  c = build(LT_EXPR, boolean_type_node, p, integer_zero_node); 
  d = build(LT_EXPR, boolean_type_node, g, integer_zero_node); 
  cond = build(BIT_AND_EXPR, boolean_type_node, c, d);          
          
  e = build_int_2(-3, -1);       
  u = build(MIN_EXPR, g95_default_integer, p, g);   
   
  u = build(COND_EXPR, g95_default_integer, cond, e, u);    
    
  /* First part */

  c = build(GE_EXPR, boolean_type_node, p, integer_zero_node);     
  d = build(GE_EXPR, boolean_type_node, g, integer_zero_node);      
  cond = build(BIT_AND_EXPR, boolean_type_node, c, d);    
    
  e = build(MAX_EXPR, g95_default_integer, p, g);          
  s->expr = build(COND_EXPR, g95_default_integer, cond, e, u);   
} 
 
 
      
      
/* intrinsic_char()-- Generate a string of length one. */          
          
static void intrinsic_char(g95_se *se1, g95_expr *g) {   
tree var, tmp;    
g95_se se0;      
      
  g95_init_se(&se0, NULL);      
  g95_conv_expr(&se0, access_arg(g, 0));     
     
  g95_add_block_to_block(&se1->pre, &se0.pre);    
  g95_add_block_to_block(&se1->post, &se0.post);        
          
  var = g95_temp_string(se1, integer_one_node);         
  tmp = build1(INDIRECT_REF, char_type_node, var);       
       
  g95_add_modify_expr(&se1->pre, tmp, se0.expr);   
   
  se1->expr = var;     
  se1->string_length = integer_one_node;       
}         
         
         
          
          
/* get_intrinsic_lib_fndecl()-- Create a function declaration for a
 * simple intrinsic library function.  */      
      
static tree get_intrinsic_lib_fndecl(g95_intrinsic_map_t *b, g95_expr *e1) { 
char n[G95_MAX_SYMBOL_LEN+3];        
tree dtype, argtypes, f;         
g95_actual_arglist *real;  
g95_typespec *typesp;   
tree *pdecl;     
     
  typesp = &e1->ts;     
  n[0] = 0;  
  switch(typesp->type) {       
  case BT_REAL:  
    switch(typesp->kind) {
    case 4: 
      pdecl = &b->real4_decl;         
      break;    
    
    case 8:         
      pdecl = &b->real8_decl;     
      break;   
   
    default:          
      abort();   
    }         
         
    break;  
  
  case BT_COMPLEX:    
    n[0] = 'c';       
    n[1] = 0; 
    switch(typesp->kind) {          
    case 4:         
      pdecl = &b->complex4_decl; 
      break;        
        
    case 8:
      pdecl = &b->complex8_decl;    
      break;    
    
    default:         
      abort();    
    } 
 
    break;   
   
  default:  
    abort();       
  }      
      
  if (*pdecl) return *pdecl;   
   
  dtype = g95_typenode_for_spec(typesp);     
  argtypes = NULL_TREE;    
    
  for(real = e1->value.function.actual; real; real = real->next) {    
    if (!g95_compare_types(&real->u.expr->ts, typesp))        
      internal_error("arg types for intrinsic %s do not match", 
		     e1->value.function.name);        
        
    argtypes = g95_chainon_list(argtypes, dtype);     
  }

  strcat(n, b->name);       
       
  if (typesp->kind == 4)   
    strcat(n, "f");        
  else  
    assert(typesp->kind == 8);         
         
  argtypes = g95_chainon_list(argtypes, void_type_node);   
  dtype = build_function_type(dtype, argtypes);
  f = build_decl(FUNCTION_DECL, get_identifier(n), dtype);      
      
  /* Mark the decl as external.  */   
  DECL_EXTERNAL(f) = 1;  
  TREE_PUBLIC(f) = 1;    
    
  rest_of_decl_compilation(f, NULL, 1, 0); 
 
  (*pdecl) = f;
  return f;     
}         
         
         
 
 
/* intrinsic_repeat()-- Repeat a given string. */   
   
static void intrinsic_repeat(g95_se *se, g95_expr *f) {   
tree copies, length, variable, tmp;  
g95_se s, se1;         
          
  g95_init_se(&s, NULL);         
  s.reflevel = 1;    
  g95_conv_expr(&s, access_arg(f, 0));  
  
  g95_add_block_to_block(&se->pre, &s.pre);    
  g95_add_block_to_block(&se->post, &s.post);  
  
  g95_init_se(&se1, NULL);    
  g95_conv_expr(&se1, access_arg(f, 1));   
   
  g95_add_block_to_block(&se->pre, &se1.pre);  
  g95_add_block_to_block(&se->post, &se1.post);  
  
  copies = save_expr(fold(build(MAX_EXPR, g95_default_integer, se1.expr,     
				integer_zero_node)));          
          
  length = save_expr(fold(build(MULT_EXPR, g95_default_integer, copies,   
				s.string_length)));      
      
  variable = g95_temp_string(se, length);    
    
  tmp = g95_call_library(void_type_node, PREFIX "repeat",
			 variable, s.expr, s.string_length, copies, NULL_TREE);     
     
  g95_add_expr_to_block(&se->pre, tmp);         
         
  se->expr = variable;      
  se->string_length = length;      
}   
   
   
  
  
/* g95_build_intrinsic_lib_fndecls()-- Initialize function decls for
 * library functions.  The external functions are created as required.
 * Builtin functions are added here. */       
       
void g95_build_intrinsic_lib_fndecls(void) {     
const g95_builtin_intrinsic_t *f; 
g95_intrinsic_map_t *h;    
    
  /* Add GCC builtin functions */          
          
  for(f = g95_builtin_intrinsics; f->id != G95_ISYM_NONE; f++) {    
    for(h = g95_intrinsic_map; h->id != G95_ISYM_NONE; h++)      
      if (h->id == f->id) break;     
     
    assert(h->id != G95_ISYM_NONE);   
   
    h->real4_decl = built_in_decls[f->code4];   
    h->real8_decl = built_in_decls[f->code8];         
  }       
}  
  
  


/* intrinsic_len()-- The length of a character string.  */   
   
static void intrinsic_len(g95_se *se1, g95_expr *e2) {        
g95_se se2;  
  
  g95_init_se(&se2, 0);      
  g95_conv_expr(&se2, access_arg(e2, 0));       
       
  g95_add_block_to_block(&se1->pre, &se2.pre);   
  g95_add_block_to_block(&se1->post, &se2.post);        
        
  se1->expr = se2.string_length;        
}         
         
         
          
          
/* intrinsic_associated()-- Test for pointer association */        
        
static void intrinsic_associated(g95_se *s, g95_expr *exp) {   
g95_expr *pointer_expr, *target_expr;       
symbol_attribute attr;         
tree pointer, target; 
g95_se se1;    
    
  pointer_expr = access_arg(exp, 0);         
  target_expr = access_arg(exp, 1);    
    
  g95_init_se(&se1, NULL);          
  se1.reflevel = 1;         
  g95_conv_expr(&se1, pointer_expr);        
        
  g95_add_block_to_block(&s->pre, &se1.pre);          
  g95_add_block_to_block(&s->post, &se1.post);       
       
  pointer = se1.expr;

  if (target_expr == NULL) { /* See if the pointer is associated */          
    s->expr = build(NE_EXPR, boolean_type_node, pointer, null_pointer_node);      
    return;  
  }    
    
  g95_init_se(&se1, NULL);  
  
  attr = g95_variable_attr(target_expr, NULL);    
  if (attr.pointer) se1.reflevel = 1;  
  
  g95_conv_expr(&se1, target_expr);        
        
  g95_add_block_to_block(&s->pre, &se1.pre);    
  g95_add_block_to_block(&s->post, &se1.post);  
  
  target = se1.expr; 
 
  if (!attr.pointer) target = build1(ADDR_EXPR, pvoid_type_node, target);

  s->expr = build(EQ_EXPR, boolean_type_node, pointer, target);
}  
  
  
         
         
/* intrinsic_strcmp()-- Intrinsic string comparison functions. */ 
 
static void intrinsic_strcmp(g95_se *se, g95_expr *e2, int operand) {  
g95_se se1, se2;         
         
  g95_init_se(&se1, NULL);      
  g95_conv_expr(&se1, access_arg(e2, 0));  
  
  g95_add_block_to_block(&se->pre, &se1.pre);         
  g95_add_block_to_block(&se->post, &se1.post); 
 
  g95_init_se(&se2, NULL);          
  g95_conv_expr(&se2, access_arg(e2, 1));        
        
  g95_add_block_to_block(&se->pre, &se2.pre);       
  g95_add_block_to_block(&se->post, &se2.post);    
    
  se->expr = g95_call_library(g95_default_integer, PREFIX "compare_string",    
			      se1.expr, se1.string_length,        
			      se2.expr, se2.string_length, NULL_TREE); 
 
  se->expr = build(operand, boolean_type_node, se->expr, integer_zero_node);         
} 
 
 


/* intrinsic_round()--  This is needed because the gcc backend only
 * implements FIX_TRUNC_EXPR.  TODO: Delete in favor of round calls?
 *    NINT(x) = INT(x + ((x > 0) ? 0.5 : -0.5)).  */ 
 
static void intrinsic_round(g95_se *se, tree ap, tree dtype) {         
tree tmp1, cond, neg, p, argtype;         
REAL_VALUE_TYPE j; 
 
  argtype = TREE_TYPE(ap); 
  ap = save_expr(ap);        
        
  real_from_string(&j, "0.5");       
  p = build_real(argtype, j); 
 
  real_from_string(&j, "-0.5");    
  neg = build_real(argtype, j);

  tmp1 = g95_build_const(argtype, integer_zero_node);        
  cond = fold(build(GT_EXPR, boolean_type_node, ap, tmp1));        
        
  tmp1 = fold(build(COND_EXPR, argtype, cond, p, neg));          
  tmp1 = fold(build(PLUS_EXPR, argtype, ap, tmp1));     
  se->expr = fold(build1(FIX_TRUNC_EXPR, dtype, tmp1));          
}




/* intrinsic_cmplx()-- Create a complex value from one or two real
 * components. */          
          
static void intrinsic_cmplx(g95_se *s, g95_expr *e) {          
tree real, imag, dtype;  
g95_se se0; 
 
  dtype = g95_get_real_type(e->ts.kind);          
          
  g95_init_se(&se0, NULL);         
         
  g95_conv_expr(&se0, access_arg(e, 0));    
  real = convert(dtype, se0.expr);          
          
  if (access_arg(e, 1) == NULL)        
    se0.expr = integer_zero_node;    
  else  
    g95_conv_expr(&se0, access_arg(e, 1));  
  
  imag = convert(dtype, se0.expr);   
   
  dtype = g95_get_complex_type(e->ts.kind); 
 
  g95_add_block_to_block(&s->pre, &se0.pre);    
  g95_add_block_to_block(&s->post, &se0.post);       
       
  s->expr = fold(build(COMPLEX_EXPR, dtype, real, imag));
}     
     
     
       
       
/* intrinsic_ishft()-- Care must be taken if the number of shifts is
 * greater than the word size.  The SHIFT_EXPRs are undefined for more
 * shifts than bits but the fortran standard is not. */ 
 
static void intrinsic_ishft(g95_se *se1, g95_expr *e1) { 
tree e, shifts, typ, t, lshift, rshift, rslt, word_size;       
g95_se se2;  
int u;        
        
  g95_init_se(&se2, NULL);         
  g95_conv_expr(&se2, access_arg(e1, 0));      
  typ = TREE_TYPE(se2.expr);  
  
  g95_save_expr(&se2);  
  e = se2.expr;         
         
  g95_conv_expr(&se2, access_arg(e1, 1));     
  shifts = save_expr(se2.expr);       
       
  g95_add_block_to_block(&se1->pre, &se2.pre); 
  g95_add_block_to_block(&se1->post, &se2.post);    
    
  /* Right shifts if nonpositive */        
  t = fold(build1(NEGATE_EXPR, g95_default_integer, shifts));  
  rshift = fold(build(RSHIFT_EXPR, unsigned_type_node, e, t)); 
 
  /* Left shifts if positive */      
  lshift = build(LSHIFT_EXPR, unsigned_type_node, e, shifts);     
     
  t = fold(build(GT_EXPR, boolean_type_node, shifts, integer_zero_node));        
  rslt = fold(build(COND_EXPR, typ, t, lshift, rshift));        
        
  /* Too many shifts mean a zero result */   
   
  u = g95_validate_kind(BT_INTEGER, access_arg(e1, 0)->ts.kind);       
  u = g95_integer_kinds[u].bit_size;      
  word_size = build_int_2(u, 0);     
     
  t = fold(build1(ABS_EXPR, typ, e));  
  t = fold(build(GE_EXPR, boolean_type_node, shifts, word_size));   
   
  se1->expr = fold(build(COND_EXPR, typ, t, integer_zero_node, rslt));        
}     
     
     
        
        
/* intrinsic_selected_int_kind()-- Figure out a integer kind value
 * given a range.  We implement this as a nested conditional
 * statement generated from the g95_integer_kinds[] table. */          
          
static void intrinsic_selected_int_kind(g95_se *se0, g95_expr *expr) {       
int m, a, kind_index; 
tree tmp, rest, g, r;    
    
  g95_conv_expr(se0, access_arg(expr, 0));        
        
  r = save_expr(se0->expr);
  rest = integer_minus_one_node;     
     
  /* Find the largest kind */      
      
  kind_index = 0;          
         
  for(m=1; g95_integer_kinds[m].kind != 0; m++)      
    if (g95_integer_kinds[m].kind > g95_integer_kinds[kind_index].kind)         
      kind_index = m;  
  
  /* Loop over integer kinds from largest kind to smallest */ 
 
  do { 
    tmp = build_int_2(g95_integer_kinds[kind_index].range, 0);   
    tmp = build(LE_EXPR, boolean_type_node, r, tmp);    
    
    g = build_int_2(g95_integer_kinds[kind_index].kind, 0);       
    rest = build(COND_EXPR, g95_default_integer, tmp, g, rest);        
        
    /* Find the next smaller kind */   
   
    a = -1;         
    for(m=0; g95_integer_kinds[m].kind != 0; m++) 
      if ((a == -1 || g95_integer_kinds[m].kind > g95_integer_kinds[a].kind) &&      
	  g95_integer_kinds[m].kind < g95_integer_kinds[kind_index].kind)    
	a = m;

    kind_index = a;   
  } while(kind_index != -1);       
       
  se0->expr = rest;        
}    
    
    
      
      
/* convert_arg()-- Evaluate the arguments to an intrinsic function.
 * This function converts things by value, not reference. */     
     
static tree convert_arg(g95_se *se0, g95_expr *expr) {       
g95_actual_arglist *a;     
g95_se argse;         
tree arg;   
   
  arg = NULL_TREE;  
  for(a=expr->value.function.actual; a; a=a->next) {          
    g95_init_se(&argse, se0);    
    
    if (a->u.expr == NULL)        
      argse.expr = null_pointer_node;       
    else {        
      if (a->u.expr->ts.type != BT_CHARACTER)          
	g95_conv_expr(&argse, a->u.expr);     
      else {    
	argse.reflevel = 1;      
	g95_conv_expr(&argse, a->u.expr);    
    
	arg = g95_chainon_list(arg, argse.expr);          
	arg = g95_chainon_list(arg, argse.string_length);   
      }       
    } 
 
    g95_add_block_to_block(&se0->pre, &argse.pre); 
    g95_add_block_to_block(&se0->post, &argse.post);
    arg = g95_chainon_list(arg, argse.expr);          
  }      
      
  return arg;        
}      
      
      
      
      
/* intrinsic_dprod()-- Double precision product of two single
 * precision values. */

static void intrinsic_dprod(g95_se *se, g95_expr *e1) {          
tree args, arg2, t;       
       
  args = convert_arg(se, e1);
  arg2 = TREE_VALUE(TREE_CHAIN(args));  
  args = TREE_VALUE(args);   
   
  /* Convert the args to double precision before multiplying.  */     
     
  t = g95_typenode_for_spec(&e1->ts);      
  args = convert(t, args);         
  arg2 = convert(t, arg2);
  se->expr = build(MULT_EXPR, t, args, arg2); 
}       
       
       


/* intrinsic_btest()-- Bit test.  BTEST (i, pos) = (i & (1 << pos)) != 0.  */  
  
static void intrinsic_btest(g95_se *se0, g95_expr *e) {   
tree argum, arg2, dtype, tmp;   
   
  argum = convert_arg(se0, e);        
  arg2 = TREE_VALUE(TREE_CHAIN(argum));         
  argum = TREE_VALUE(argum);   
  dtype = TREE_TYPE(argum);   
   
  tmp = build(LSHIFT_EXPR, dtype, integer_one_node, arg2);       
  tmp = build(BIT_AND_EXPR, dtype, argum, tmp);  
  tmp = fold(build(NE_EXPR, boolean_type_node, tmp, integer_zero_node)); 
  dtype = g95_typenode_for_spec(&e->ts);      
  se0->expr = convert(dtype, tmp);   
}          
          
          
          
          
/* intrinsic_sign()-- SIGN(A, B) is absolute value of A times sign of
 * B. The real value versions use library functions to ensure the
 * correct handling of negative zero.  The integer case implemented as:
 * SIGN(A, B) = ((a >= 0) .xor. (b >= 0)) ? a : -a  */ 
 
static void intrinsic_sign(g95_se *s, g95_expr *e1) {     
tree tmp1, a, arg2, dtype, zero, testa, testb;   
   
  a = convert_arg(s, e1); 
  if (e1->ts.type == BT_REAL) {         
    switch(e1->ts.kind) {      
    case 4: tmp1 = gfor_fndecl_math_sign4; break; 
    case 8: tmp1 = gfor_fndecl_math_sign8; break;          
    default: abort();         
    }      
      
    s->expr = g95_build_function_call(tmp1, a);    
    return;      
  }        
        
  arg2 = TREE_VALUE(TREE_CHAIN(a));         
  a = TREE_VALUE(a);    
  dtype = TREE_TYPE(a);    
  zero = g95_build_const(dtype, integer_zero_node);    
    
  testa = build(GE_EXPR, boolean_type_node, a, zero);
  testb = build(GE_EXPR, boolean_type_node, arg2, zero);      
  tmp1 = build(TRUTH_XOR_EXPR, boolean_type_node, testa, testb);    
  s->expr = build(COND_EXPR, dtype, tmp1, 
		   build1(NEGATE_EXPR, dtype, a), a);      
}       
       
       
    
    
/* intrinsic_bound()-- Build an expression for LBOUND or UBOUND.  If
 * the DIM parameter is not present, we call a library subroutine. */    
    
static void intrinsic_bound(g95_se *se, g95_expr *e, int upper) {   
tree arr, dim;  
g95_se se2;   
   
  if (access_arg(e, 1) == NULL) {          
    simple_libcall(se, e);         
    return;        
  }      
      
  g95_init_se(&se2, NULL);  
  g95_conv_expr(&se2, access_arg(e, 0));       
  arr = se2.expr;

  g95_add_block_to_block(&se->pre, &se2.pre);  
  g95_add_block_to_block(&se->post, &se2.post);       
       
  g95_init_se(&se2, NULL);  
  g95_conv_expr(&se2, access_arg(e, 1));      
  dim = se2.expr;       
       
  g95_add_block_to_block(&se->pre, &se2.pre);    
  g95_add_block_to_block(&se->post, &se2.post); 
 
  se->expr = upper ? g95_get_adesc_ubound(arr, dim)     
                   : g95_get_adesc_lbound(arr, dim);      
}     
     
     
  
  
/* intrinsic_imag()-- Get the imaginary component of a value. */      
      
static void intrinsic_imag(g95_se *s, g95_expr *e) {   
tree ap;

  ap = convert_arg(s, e); 
  ap = TREE_VALUE(ap);     
  s->expr = build1(IMAGPART_EXPR, TREE_TYPE(TREE_TYPE(ap)), ap);       
}       
       
       
      
      
/* intrinsic_singlebitop()-- Set or clear a single bit.  */ 
 
static void intrinsic_singlebitop(g95_se *se, g95_expr *e2, int set) {         
tree ap, arg2, dtype, tmp1;   
int o;

  ap = convert_arg(se, e2);      
  arg2 = TREE_VALUE(TREE_CHAIN(ap));    
  ap = TREE_VALUE(ap);      
  dtype = TREE_TYPE(ap);         
         
  tmp1 = fold(build(LSHIFT_EXPR, dtype, integer_one_node, arg2));        
  if (set)      
    o = BIT_IOR_EXPR;          
  else {        
    o = BIT_AND_EXPR;         
    tmp1 = fold(build1(BIT_NOT_EXPR, dtype, tmp1));       
  }

  se->expr = fold(build(o, dtype, ap, tmp1));       
} 
 
 
    
    
/* intrinsic_adjust()-- Left or right justification of strings. */        
        
static void intrinsic_adjust(g95_se *se1, g95_expr *n, int right_flag) {   
tree tmp1, res;         
g95_se se0;          
char *sub;        
        
  g95_init_se(&se0, NULL);
  se0.reflevel = 1;
  g95_conv_expr(&se0, access_arg(n, 0));    
    
  g95_add_block_to_block(&se1->pre, &se0.pre);         
  g95_add_block_to_block(&se1->post, &se0.post); 
 
  res = g95_temp_string(se1, se0.string_length);     
     
  sub = right_flag ? PREFIX "adjustr" : PREFIX "adjustl"; 
 
  tmp1 = g95_call_library(void_type_node, sub,  
			 res, se0.expr, se0.string_length, NULL_TREE);   
   
  g95_add_expr_to_block(&se1->pre, tmp1);      
  se1->expr = res;      
  se1->string_length = se0.string_length;    
}         
         
         
 
 
/* intrinsic_minmax()-- Get the minimum or maximum value of all the
 * parameters. */         
         
static void intrinsic_minmax(g95_se *se, g95_expr *e1, int op) {      
tree extremum, ap;      
      
  ap = convert_arg(se, e1); 
 
  extremum = TREE_VALUE(ap);    
  ap = TREE_CHAIN(ap);    
    
  for(; ap!=NULL_TREE; ap=TREE_CHAIN(ap))          
    extremum = build(op, TREE_TYPE(extremum), extremum, TREE_VALUE(ap));        
        
  se->expr = extremum;        
}


      
      
/* intrinsic_abs()-- Absolute value */ 
 
static void intrinsic_abs(g95_se *s, g95_expr *e2) {  
tree a, val, fd;         
         
  a = convert_arg(s, e2);          
  assert(a && TREE_CHAIN(a) == NULL_TREE); 
  val = TREE_VALUE(a);          
          
  switch(access_arg(e2, 0)->ts.type) {     
  case BT_INTEGER:  
  case BT_REAL: 
    s->expr = build1(ABS_EXPR, TREE_TYPE(val), val);   
    break;          
          
  case BT_COMPLEX:          
    switch(e2->ts.kind) {      
    case 4: fd = gfor_fndecl_math_cabsf; break;     
    case 8: fd = gfor_fndecl_math_cabs;  break;  
    default: abort();  
    }          
            
    s->expr = g95_build_function_call(fd, a);   
    break;

  default:         
    abort();          
  } 
}     
     
     
       
       
/* intrinsic_aint()-- Truncate a real number. */        
        
static void intrinsic_aint(g95_se *se, g95_expr *m) {   
char *function;         
g95_expr *y;    
g95_se exp;   
   
  y = access_arg(m, 0);         
  g95_init_se(&exp, NULL);        
  g95_conv_expr(&exp, access_arg(m, 0));     
     
  g95_add_block_to_block(&se->pre, &exp.pre);  
  g95_add_block_to_block(&se->post, &exp.post); 
 
  switch(y->ts.kind) {      
  case 4:  function = "truncf"; break;          
  case 8:  function = "trunc";  break;     
  default:    
    g95_internal_error("intrinsic_aint(): Bad real kind"); 
  }      
      
  exp.expr = g95_call_library(TREE_TYPE(exp.expr), function, exp.expr, 
			      NULL_TREE);        
        
  if (m->ts.kind != y->ts.kind)      
      exp.expr = convert(g95_get_real_type(m->ts.kind), exp.expr);    
    
  se->expr = exp.expr;      
}          
          
          
         
         
/* intrinsic_mod()-- Modulus function MOD(A, P) = A - INT(A / P) * P.  */     
     
static void intrinsic_mod(g95_se *se0, g95_expr *expr) {          
tree ap, arg2, type, itype, tmp1;    
    
  ap = convert_arg(se0, expr); 
  arg2 = TREE_VALUE(TREE_CHAIN(ap));
  ap = TREE_VALUE(ap);         
  type = TREE_TYPE(ap);

  switch(expr->ts.type) {   
  case BT_INTEGER:          
    se0->expr = build(TRUNC_MOD_EXPR, type, ap, arg2);       
    break; 
 
  case BT_REAL:    
    /* Real values we have to do the hard way.  */         
    ap = save_expr(ap);     
    arg2 = save_expr(arg2);          
          
    itype = g95_get_int_type(expr->ts.kind);    
    tmp1 = fold(build(RDIV_EXPR, type, ap, arg2));          
    tmp1 = fold(build1(FIX_TRUNC_EXPR, itype, tmp1));         
    tmp1 = convert(type, tmp1);          
    tmp1 = fold(build(MULT_EXPR, type, tmp1, arg2));    
    se0->expr = fold(build(MINUS_EXPR, type, ap, tmp1));  
    break;      
      
  default:         
    abort();   
  } 
}  
  
  
     
     
/* intrinsic_int()-- Convert to an integer using the specified
 * rounding mode. */       
       
static void intrinsic_int(g95_se *se1, g95_expr *e, int o) {     
tree t, ap;

  t = g95_typenode_for_spec(&e->ts);         
  assert(e->value.function.actual->u.expr); 
  ap = convert_arg(se1, e);        
  ap = TREE_VALUE(ap);          
          
  if (TREE_CODE(TREE_TYPE(ap)) == INTEGER_TYPE)      
    se1->expr = convert(t, ap);  /* Conversion to a different kind */      
  else {     
      /* Conversion from complex to non-complex involves taking the real
       * component of the value.  */        
    if (TREE_CODE(TREE_TYPE(ap)) == COMPLEX_TYPE &&      
	e->ts.type != BT_COMPLEX) {         
      tree artype;          
          
      artype = TREE_TYPE(TREE_TYPE(ap)); 
      ap = build1(REALPART_EXPR, artype, ap);        
    }          
          
    /* FIX_ROUND_EXPR isn't implemented in the gcc backend, so we must
     * do it ourselves. */  
  
    switch(o) {          
    case FIX_ROUND_EXPR:      
      intrinsic_round(se1, ap, t); 
      break;   
   
    default:     
      se1->expr = build1(o, t, ap);          
      break;       
    }
  }   
}      
      
      
 
 
/* g95_is_intrinsic_libcall()-- Returns nonzero if the specified
 * intrinsic function call maps directly to a an external library
 * call.  Should only be used for functions that return arrays.  */

int g95_is_intrinsic_libcall(g95_expr *e1) {   
   
  assert(e1->type == EXPR_FUNCTION && e1->value.function.isym);
  assert(e1->rank > 0);      
      
  switch(e1->value.function.isym->generic_id) {          
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
      
      
          
          
/* intrinsic_size()-- implement the intrinsic SIZE function.  If the
 * DIM parameter is not present, we call a library subroutine. */         
         
static void intrinsic_size(g95_se *se1, g95_expr *e1) {       
tree block, r, ubound, lbound;  
g95_se s;         
         
  if (access_arg(e1, 1) == NULL) {     
    simple_libcall(se1, e1); 
    return;        
  }         
         
  g95_init_se(&s, NULL);   
  g95_conv_expr(&s, access_arg(e1, 0));  
  block = s.expr;   
   
  g95_add_block_to_block(&se1->pre, &s.pre);     
  g95_add_block_to_block(&se1->post, &s.post);  
  
  g95_init_se(&s, NULL); 
  g95_conv_expr(&s, access_arg(e1, 1));  
  
  g95_add_block_to_block(&se1->pre, &s.pre);      
  g95_add_block_to_block(&se1->post, &s.post);        
        
  r = save_expr(s.expr);    
  ubound = g95_get_adesc_ubound(block, r);     
  lbound = g95_get_adesc_lbound(block, r);      
      
  ubound = build(PLUS_EXPR, g95_default_integer, ubound, integer_one_node);          
  ubound = fold(ubound);          
          
  g95_dim(se1, ubound, lbound); 
}          
          
          
 
 
/* intrinsic_trim()-- Trim the trailing blanks from a string.  The
 * string that we return is the original string with a new length.
 * Since trim() must appear on the right of an assignment, there
 * doesn't appear to be a need to copy the string. */  
  
static void intrinsic_trim(g95_se *s, g95_expr *e) {         
g95_se se2;     
     
  g95_init_se(&se2, NULL); 
  se2.reflevel = 1;         
  g95_conv_expr(&se2, access_arg(e, 0));     
     
  g95_add_block_to_block(&s->pre, &se2.pre);
  g95_add_block_to_block(&s->post, &se2.post);   
   
  s->expr = se2.expr;   
  s->string_length =          
    g95_call_library(g95_default_integer, PREFIX "len_trim_1",    
		     se2.expr, se2.string_length, NULL_TREE);        
}         
         
         
         
     
     
/* intrinsic_lib_function()-- Convert an intrinsic function into an
 * external or builtin call. */  
  
static void intrinsic_lib_function(g95_se *se0, g95_expr *exp) {         
g95_intrinsic_map_t *a;      
tree argu, fndecl;         
int id;         
         
  id = exp->value.function.isym->generic_id;     
  /* Find the entry for this function */    
    
  for(a=g95_intrinsic_map; a->id!=G95_ISYM_NONE; a++)      
    if (id == a->id) break;        
        
  if (a->id == G95_ISYM_NONE)
    internal_error("Intrinsic function %s(%d) not recognized",         
		    exp->value.function.name, id);         
         
  /* Get the decl and generate the call */     
  argu = convert_arg(se0, exp); 
  fndecl = get_intrinsic_lib_fndecl(a, exp); 
  se0->expr = g95_build_function_call(fndecl, argu);       
}      
      
      
       
       
/* intrinsic_present()-- Test for the presense of an argument */   
   
static void intrinsic_present(g95_se *se, g95_expr *e2) {         
tree declr;         
         
  declr = access_arg(e2, 0)->symbol->backend_decl;          
  se->expr = build(NE_EXPR, boolean_type_node, declr, null_pointer_node);     
}


  
  
/* intrinsic_ibits()-- Extract a sequence of bits.
 * IBITS(I, POS, LEN) = (I >> POS) & ~((~0) << LEN).  */        
        
static void intrinsic_ibits(g95_se *s, g95_expr *exp) {         
tree arg, arg2, arg3, t, tmp0, mask;    
    
  arg = convert_arg(s, exp);  
  arg2 = TREE_CHAIN(arg);        
  arg3 = TREE_VALUE(TREE_CHAIN(arg2));        
  arg = TREE_VALUE(arg);          
  arg2 = TREE_VALUE(arg2);       
  t = TREE_TYPE(arg);      
      
  mask = build_int_2(-1, ~(unsigned HOST_WIDE_INT) 0); 
  mask = build(LSHIFT_EXPR, t, mask, arg3);    
  mask = build1(BIT_NOT_EXPR, t, mask);

  tmp0 = build(RSHIFT_EXPR, t, arg, arg2);          
          
  s->expr = fold(build(BIT_AND_EXPR, t, tmp0, mask));       
} 
 
 
     
     
/* intrinsic_allocated()-- Test for allocation of an allocatable array. */

static void intrinsic_allocated(g95_se *se, g95_expr *exp) { 
g95_se se0; 
tree t;       
       
  g95_init_se(&se0, NULL);       
  se0.reflevel = 1;       
  g95_conv_expr(&se0, access_arg(exp, 0));       
       
  g95_add_block_to_block(&se->pre, &se0.pre); 
  g95_add_block_to_block(&se->post, &se0.post);      
      
  t = g95_adesc_base(se0.expr);     
  se->expr = build(NE_EXPR, boolean_type_node, t, null_pointer_node);      
}     
     
     
        
        
/* intrinsic_convertion()-- Convert one type to another. */     
     
static void intrinsic_conversion(g95_se *se1, g95_expr *expr) {
tree typ, argum;    
    
  /* Evaluate the argument */   
  typ = g95_typenode_for_spec(&expr->ts);        
  assert(expr->value.function.actual->u.expr);     
  argum = convert_arg(se1, expr);
  argum = TREE_VALUE(argum);   
   
  /* Conversion from complex to non-complex involves taking the real
   * component of the value.  */        
        
  if (TREE_CODE(TREE_TYPE(argum)) == COMPLEX_TYPE   
      && expr->ts.type != BT_COMPLEX) {
      tree artype;         
         
      artype = TREE_TYPE(TREE_TYPE(argum));        
      argum = build1(REALPART_EXPR, artype, argum);        
    }      
      
  se1->expr = convert(typ, argum);    
}


  
  
/* intrinsic_ishftc()-- Circular shift. */       
       
static void intrinsic_ishftc(g95_se *se0, g95_expr *e) {     
tree arg, arg2, arg3, dtype, tmp0, lrot, rrot;      
      
  arg = convert_arg(se0, e); 
  arg2 = TREE_CHAIN(arg);   
  arg3 = TREE_CHAIN(arg2);        
  if (arg3) {      
    /* Use a library function for the 3 parameter version.  */    
    dtype = TREE_TYPE(TREE_VALUE(arg));    
    
    /* Convert all args to the same type otherwise we need loads of library
       functions.  SIZE and SHIFT cannot have values > BIT_SIZE (I) so the
       conversion is safe.  */         
         
    tmp0 = convert(dtype, TREE_VALUE(arg2));       
    TREE_VALUE(arg2) = tmp0;  
    tmp0 = convert(dtype, TREE_VALUE(arg3));       
    TREE_VALUE(arg3) = tmp0;  
  
    switch(e->ts.kind) { 
    case 4: tmp0 = gfor_fndecl_math_ishftc4; break;          
    case 8: tmp0 = gfor_fndecl_math_ishftc8; break;        
    default: abort();
    }  
    se0->expr = g95_build_function_call(tmp0, arg);    
    return;      
  } 
 
  arg = TREE_VALUE(arg);        
  arg2 = TREE_VALUE(arg2);    
  dtype = TREE_TYPE(arg);       
       
  /* Rotate left if positive.  */      
  lrot = build(LROTATE_EXPR, dtype, arg, arg2);         
         
  /* Rotate right if negative.  */  
  tmp0 = build1(NEGATE_EXPR, TREE_TYPE(arg2), arg2);        
  rrot = build(RROTATE_EXPR, dtype, arg, tmp0);      
      
  tmp0 = build(GT_EXPR, boolean_type_node, arg2, integer_zero_node);     
  rrot = build(COND_EXPR, dtype, tmp0, lrot, rrot);         
         
  /* Do nothing if shift == 0.  */       
  tmp0 = build(EQ_EXPR, boolean_type_node, arg2, integer_zero_node); 
  se0->expr = build(COND_EXPR, dtype, tmp0, arg, rrot);    
}   
   
   
  
  
/* g95_dim()-- Generate code for DIM(x, y). */       
       
void g95_dim(g95_se *s, tree w, tree e) {        
tree typ, val, tmp0, zero;         
         
  typ = TREE_TYPE(w);  
  
  val = fold(build(MINUS_EXPR, typ, w, e));    
  val = save_expr(val);  
  
  zero = g95_build_const(typ, integer_zero_node);        
  tmp0 = build(LE_EXPR, boolean_type_node, val, zero); 
  s->expr = fold(build(COND_EXPR, typ, tmp0, zero, val));      
}          
          
          
 
 
/* intrinsic_bitop()-- Generate code to perform logical and, or and
 * exclusive-or. */       
       
static void intrinsic_bitop(g95_se *s, g95_expr *e, int op) {     
tree argum, arg2, typ;    
    
  argum = convert_arg(s, e); 
  arg2 = TREE_VALUE(TREE_CHAIN(argum));     
  argum = TREE_VALUE(argum);          
  typ = TREE_TYPE(argum);   
   
  s->expr = fold(build(op, typ, argum, arg2));  
} 
 
 
  
  
/* intrinsic_not()-- Logical not. */      
      
static void intrinsic_not(g95_se *se1, g95_expr *e2) {          
tree ap;     
     
  ap = convert_arg(se1, e2);          
  ap = TREE_VALUE(ap); 
 
  se1->expr = build1(BIT_NOT_EXPR, TREE_TYPE(ap), ap);         
}         
         
         
       
       
/* intrinsic_ichar()-- Return the integer of the first character in a
 * string. */    
    
static void intrinsic_ichar(g95_se *se, g95_expr *e2) {      
g95_se s;     
tree tmp;          
          
  g95_init_se(&s, NULL);      
      
  s.reflevel = 1;     
  g95_conv_expr(&s, access_arg(e2, 0));    
    
  g95_add_block_to_block(&se->pre,  &s.pre);       
  g95_add_block_to_block(&se->post, &s.post);    
    
  tmp = build1(INDIRECT_REF, g95_character1_type_node, s.expr);  
  se->expr = convert(g95_default_integer, tmp);         
}




/* intrinsic_conjg()-- Get the complex conjugate of a value.  */         
         
static void intrinsic_conjg(g95_se *se, g95_expr *e1) {
tree arg; 
 
  arg = convert_arg(se, e1);    
  arg = TREE_VALUE(arg);   
  se->expr = build1(CONJ_EXPR, TREE_TYPE(arg), arg);         
}


  
  
/* intrinsic_dim()-- Calculate a positive difference,
 * DIM(x, y) = ((x - y) < 0) ? 0 : x - y.  */ 
 
static void intrinsic_dim(g95_se *s, g95_expr *e1) {          
tree arg;      
      
  arg = convert_arg(s, e1);    
  g95_dim(s, TREE_VALUE(arg), TREE_VALUE(TREE_CHAIN(arg)));  
}  
  
  
      
      
/* g95_conv_intrinsic_function()-- Generate code for an intrinsic
 * function.  Some map directly to library calls, others get special
 * handling.  In some cases the name of the function used depends on
 * the type specifiers.  */       
       
void g95_conv_intrinsic_function(g95_se *se0, g95_expr *e2) {          
g95_intrinsic_sym *is;      
      
  is = e2->value.function.isym;   
   
  switch(e2->value.function.isym->generic_id) {          
  case G95_ISYM_NONE:     
  case G95_ISYM_DOT_PRODUCT:  
  case G95_ISYM_MATMUL:     
    abort(); 
 
  case G95_ISYM_SELECTED_INT_KIND:       
    intrinsic_selected_int_kind(se0, e2);    
    break;          
          
  case G95_ISYM_SELECTED_REAL_KIND:         
    intrinsic_selected_real_kind(se0, e2);  
    break;

  case G95_ISYM_ALLOCATED:     
    intrinsic_allocated(se0, e2);          
    break;      
      
  case G95_ISYM_ANINIT:     
  case G95_ISYM_CEILING:  
  case G95_ISYM_CSHIFT:   
  case G95_ISYM_EOSHIFT:        
  case G95_ISYM_EXPONENT:   
  case G95_ISYM_FLOOR:        
  case G95_ISYM_FRACTION:    
  case G95_ISYM_MERGE: 
  case G95_ISYM_NEAREST:     
  case G95_ISYM_PACK:   
  case G95_ISYM_SCAN:        
  case G95_ISYM_SET_EXPONENT:  
  case G95_ISYM_MODULO:   
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
    simple_libcall(se0, e2);          
    break;    
    
  case G95_ISYM_REPEAT:
    intrinsic_repeat(se0, e2);         
    break; 
 
  case G95_ISYM_ADJUSTL: 
    intrinsic_adjust(se0, e2, 0);     
    break;   
   
  case G95_ISYM_ADJUSTR:        
    intrinsic_adjust(se0, e2, 1);          
    break;        
        
  case G95_ISYM_PRESENT:        
    intrinsic_present(se0, e2);    
    break;  
  
  case G95_ISYM_ASSOCIATED:    
    intrinsic_associated(se0, e2);        
    break;        
        
  case G95_ISYM_ABS:     
    intrinsic_abs(se0, e2);        
    break;   
   
  case G95_ISYM_AIMAG:       
    intrinsic_imag(se0, e2);   
    break;      
      
  case G95_ISYM_BTEST:   
    intrinsic_btest(se0, e2);        
    break;      
      
  case G95_ISYM_ACHAR: 
  case G95_ISYM_CHAR:  
    intrinsic_char(se0, e2);   
    break;     
     
  case G95_ISYM_TRIM:        
    intrinsic_trim(se0, e2);  
    break;         
         
  case G95_ISYM_CONVERSION:   
  case G95_ISYM_REAL:      
  case G95_ISYM_LOGICAL: 
  case G95_ISYM_DBLE:
    intrinsic_conversion(se0, e2);      
    break;       
       
    /* Integer conversions are handled seperately to make sure we get
     * the correct rounding mode. */     
     
  case G95_ISYM_AINT:         
    intrinsic_aint(se0, e2); 
    break;         
         
  case G95_ISYM_INT:        
    intrinsic_int(se0, e2, FIX_TRUNC_EXPR);    
    break;          
          
  case G95_ISYM_ANINT:       
  case G95_ISYM_NINT:
    intrinsic_int(se0, e2, FIX_ROUND_EXPR);        
    break;  
  
  case G95_ISYM_MOD:      
    intrinsic_mod(se0, e2);    
    break; 
 
  case G95_ISYM_CMPLX:   
    intrinsic_cmplx(se0, e2);         
    break;        
        
  case G95_ISYM_CONJG:     
    intrinsic_conjg(se0, e2);      
    break;          
          
  case G95_ISYM_DIM: 
    intrinsic_dim(se0, e2);       
    break;          
          
  case G95_ISYM_DPROD:         
    intrinsic_dprod(se0, e2);
    break;

  case G95_ISYM_IAND:    
    intrinsic_bitop(se0, e2, BIT_AND_EXPR);        
    break;      
      
  case G95_ISYM_IBCLR:          
    intrinsic_singlebitop(se0, e2, 0);      
    break;         
         
  case G95_ISYM_IBITS:     
    intrinsic_ibits(se0, e2);
    break;

  case G95_ISYM_IBSET:   
    intrinsic_singlebitop(se0, e2, 1);   
    break;        
        
  case G95_ISYM_IACHAR: 
  case G95_ISYM_ICHAR:
    intrinsic_ichar(se0, e2);       
    break;   
   
  case G95_ISYM_IEOR: 
    intrinsic_bitop(se0, e2, BIT_XOR_EXPR);     
    break;        
        
  case G95_ISYM_IOR: 
    intrinsic_bitop(se0, e2, BIT_IOR_EXPR);       
    break;          
          
  case G95_ISYM_ISHFT:    
    intrinsic_ishft(se0, e2);     
    break;  
  
  case G95_ISYM_ISHFTC:  
    intrinsic_ishftc(se0, e2);     
    break;      
      
  case G95_ISYM_LBOUND: 
    intrinsic_bound(se0, e2, 0);    
    break;       
       
  case G95_ISYM_LEN:         
    intrinsic_len(se0, e2);     
    break;         
         
  case G95_ISYM_LGE:        
    intrinsic_strcmp(se0, e2, GE_EXPR);        
    break;

  case G95_ISYM_LGT:       
    intrinsic_strcmp(se0, e2, GT_EXPR);   
    break;

  case G95_ISYM_LLE:      
    intrinsic_strcmp(se0, e2, LE_EXPR);          
    break;    
    
  case G95_ISYM_LLT:        
    intrinsic_strcmp(se0, e2, LT_EXPR);         
    break;   
   
  case G95_ISYM_MAX:      
    intrinsic_minmax(se0, e2, MAX_EXPR);
    break; 
 
  case G95_ISYM_MIN:          
    intrinsic_minmax(se0, e2, MIN_EXPR);  
    break;     
     
  case G95_ISYM_NOT:   
    intrinsic_not(se0, e2);       
    break; 
 
  case G95_ISYM_SIGN:   
    intrinsic_sign(se0, e2);          
    break;   
   
  case G95_ISYM_SIZE:     
    intrinsic_size(se0, e2);   
    break;       
       
  case G95_ISYM_UBOUND:          
    intrinsic_bound(se0, e2, 1);    
    break;   
   
  default:  
    intrinsic_lib_function(se0, e2);   
    break;      
  }   
}          
          
          
   
   
#include "gt-f95-trans-intrinsic.h"
