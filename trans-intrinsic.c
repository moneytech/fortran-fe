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
  
#include "trans.h"
#include "intrinsic.h"
     
     
  
  
/* convert_args()-- Evaluate the arguments to an intrinsic function.
 * This function converts things by value, not reference. */   
   
static tree convert_args VPARAMS((g95_se *se, g95_expr *e, ...)) {      
g95_actual_arglist *actual;         
g95_se argse, *dest;      
tree args; 
int flag;     
     
  VA_OPEN(actualp, e);          
  VA_FIXEDARG(actualp, g95_se *, se);    
  VA_FIXEDARG(actualp, g95_expr *, e);          
          
  args = NULL_TREE;        
  dest = NULL; 
  flag = 1;         
         
  for(actual=e->value.function.actual; actual; actual=actual->next) {         
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
 
    g95_add_block_to_block(&se->pre,  &argse.pre); 
    g95_add_block_to_block(&se->post, &argse.post);      
      
    if (flag) {       
      dest = va_arg(actualp, g95_se *); 
      if (dest == NULL) flag = 0;         
    }  
  
    if (dest != NULL) {  
      g95_init_se(dest, NULL);

      dest->expr = (argse.expr == null_pointer_node) ? NULL : argse.expr; 
      dest->string_length = argse.string_length;
    }          
          
    args = g95_chainon_list(args, argse.expr);         
  }    
    
  VA_CLOSE(actualp);      
  return args;          
}      
      
      
          
          
/* intrinsic_strcmp()-- Intrinsic string comparison functions. */      
      
static void intrinsic_strcmp(g95_se *se0, g95_expr *expr, int operand) {          
g95_se r, q;     
     
  convert_args(se0, expr, &r, &q);        
       
  se0->expr = g95_call_library(g95_default_integer, PREFIX "compare_string",      
			      r.expr, r.string_length, q.expr, q.string_length,  
			      NULL_TREE);          
          
  se0->expr = build(operand, boolean_type_node, se0->expr, integer_zero_node);         
}     
     
     


/* g95_dim()-- Generate code for DIM(x, y). */         
         
void g95_dim(g95_se *s, tree w, tree u) {          
tree t, val, tmp1, zero; 
 
  t = TREE_TYPE(w);      
      
  val = fold(build(MINUS_EXPR, t, w, u));      
  val = save_expr(val);          
          
  zero = g95_build_const(t, integer_zero_node);     
  tmp1 = build(LE_EXPR, boolean_type_node, val, zero);
  s->expr = fold(build(COND_EXPR, t, tmp1, zero, val));      
}         
         
         
        
        
/* intrinsic_convertion()-- Convert one type to another. */   
   
static void intrinsic_conversion(g95_se *se1, g95_expr *e1) {         
g95_se arg;     
tree t;     
     
  /* Evaluate the argument */
  t = g95_typenode_for_spec(&e1->ts);        
  assert(e1->value.function.actual->u.expr);  
  
  convert_args(se1, e1, &arg, NULL);         
         
  /* Conversion from complex to non-complex involves taking the real
   * component of the value.  */  
  
  if (TREE_CODE(TREE_TYPE(arg.expr)) == COMPLEX_TYPE
      && e1->ts.type != BT_COMPLEX) {      
      tree artype;        
        
      artype = TREE_TYPE(TREE_TYPE(arg.expr));       
      arg.expr = build1(REALPART_EXPR, artype, arg.expr);    
    } 
 
  se1->expr = convert(t, arg.expr);     
}


  
  
/* intrinsic_selected_real_kind()-- Figure out a integer kind value
 * given a range and/or precision.  We implement this as a nested
 * conditional statements generated from the g95_real_kinds[] table. */       
       
static void intrinsic_selected_real_kind(g95_se *se, g95_expr *exp) {         
tree tmp, p_rest, r_rest, a, z, m, g, u, cond, h, s;    
int o, x, kind_index;         
g95_se e, v;   
   
  convert_args(se, exp, &e, &v);         
  h = e.expr;   
  s = v.expr;

  if (h != NULL_TREE) {      
    h = save_expr(h);       
    p_rest = integer_minus_one_node;
  }

  if (s != NULL_TREE) {        
    s = save_expr(s);       
    r_rest = build_int_2(-2, -1);         
  }  
  
  assert(h != NULL_TREE || s != NULL_TREE); 
  kind_index = 0;         
         
  /* Largest kind */    
    
  for(o=1; g95_real_kinds[o].kind != 0; o++)         
    if (g95_real_kinds[o].kind > g95_real_kinds[kind_index].kind)        
      kind_index = o;  
  
  /* Generate tree for p and r branches */   
   
  do {   
    u = build_int_2(g95_real_kinds[kind_index].kind, 0);   
   
    if (h != NULL) {     
      tmp = build_int_2(g95_real_kinds[kind_index].precision, 0);        
      tmp = build(LE_EXPR, boolean_type_node, h, tmp);         
		             
      p_rest = build(COND_EXPR, g95_default_integer, tmp, u, p_rest);   
    }  
  
    if (s != NULL) { 
      tmp = build_int_2(g95_real_kinds[kind_index].range, 0);       
      tmp = build(LE_EXPR, boolean_type_node, s, tmp);      
      
      r_rest = build(COND_EXPR, g95_default_integer, tmp, u, r_rest);
    } 
 
    /* Find the next highest real kind */ 
 
    x = -1;       
    for(o=0; g95_real_kinds[o].kind != 0; o++)    
      if ((x == -1 || g95_real_kinds[o].kind > g95_real_kinds[x].kind) &&
	  g95_real_kinds[o].kind < g95_real_kinds[kind_index].kind)      
	x = o;     
     
    kind_index = x; 
  } while(kind_index != -1);        
        
  /* If we've only got one parameter, then we already have the answer. */          
          
  if (h == NULL_TREE) {        
    se->expr = r_rest;        
    return;          
  }          
          
  if (s == NULL_TREE) {    
    se->expr = p_rest;  
    return;     
  }     
     
  /* We have two parameters.  If p and r are nonnegative, the result
   * is the larger of the two.  If p and r are negative, the result is
   * -3, otherwise the result is the smaller of the two and is a
   * negative number. */

  h = save_expr(p_rest);      
  s = save_expr(r_rest);       
       
  m = build(LT_EXPR, boolean_type_node, h, integer_zero_node);   
  g = build(LT_EXPR, boolean_type_node, s, integer_zero_node);
  cond = build(BIT_AND_EXPR, boolean_type_node, m, g);     
     
  a = build_int_2(-3, -1); 
  z = build(MIN_EXPR, g95_default_integer, h, s);          
          
  z = build(COND_EXPR, g95_default_integer, cond, a, z);  
  
  /* First part */          
          
  m = build(GE_EXPR, boolean_type_node, h, integer_zero_node);        
  g = build(GE_EXPR, boolean_type_node, s, integer_zero_node);       
  cond = build(BIT_AND_EXPR, boolean_type_node, m, g);     
     
  a = build(MAX_EXPR, g95_default_integer, h, s);     
  se->expr = build(COND_EXPR, g95_default_integer, cond, a, z); 
}      
      
      
        
        
/* intrinsic_aimag()-- Get the imaginary component of a value. */  
  
static void intrinsic_aimag(g95_se *se1, g95_expr *expr) {  
tree typ; 
g95_se w;         
         
  convert_args(se1, expr, &w); 
  typ = g95_get_real_type(expr->ts.kind);   
  se1->expr = build1(IMAGPART_EXPR, typ, w.expr);         
}       
       
       
         
         
/* intrinsic_bitop()-- Generate code to perform logical and, or and
 * exclusive-or. */       
       
static void intrinsic_bitop(g95_se *se, g95_expr *expr, int op) {   
g95_se c, a;         
         
  convert_args(se, expr, &c, &a);
  se->expr = fold(build(op, TREE_TYPE(c.expr), c.expr, a.expr));     
}  
  
  
     
     
/* simple_libcall()-- Generate a call to a library function.  The
 * function is declared with zero or more arguments, since the
 * argument list has already been vetted. */     
     
static void simple_libcall(g95_se *se1, g95_expr *e) {   
tree argu, n, typ, var, tmp0, rtype, decl;         
         
  argu = g95_trans_arglist(e->value.function.actual, se1);
  n = get_identifier(e->value.function.name);         
  var = NULL_TREE; 
 
  if (e->rank > 0)          
    rtype = build_pointer_type(g95_get_array_desc(e->rank));  
  else   
    switch(e->ts.type) {       
    case BT_LOGICAL: case BT_INTEGER: case BT_REAL:      
      rtype = g95_typenode_for_spec(&e->ts);       
      break;          
          
    case BT_CHARACTER:      
      g95_internal_error("simple_libcall(): Character return");          
      break;      
      
    case BT_COMPLEX:      
      typ = g95_get_complex_type(e->ts.kind); 
      var = g95_create_var(typ);
      TREE_ADDRESSABLE(var) = 1;    
    
      tmp0 = build1(ADDR_EXPR, build_pointer_type(typ), var);      
      tmp0 = tree_cons(NULL_TREE, tmp0, NULL_TREE);  
      argu = chainon(tmp0, argu);      
      
      rtype = void_type_node;       
      break;    
    
    default:  
      g95_internal_error("simple_libcall(): Bad function type");         
    }    
    
  decl = build_function_type(rtype, NULL_TREE);          
  decl = build_decl(FUNCTION_DECL, n, decl);      
      
  DECL_EXTERNAL(decl) = 1;    
  TREE_PUBLIC(decl) = 1;    
    
  pushdecl(decl);        
  rest_of_decl_compilation(decl, NULL, 1, 0);  
  
  tmp0 = g95_build_function_call(decl, argu);          
          
  if (var == NULL_TREE)    
    se1->expr = tmp0;       
  else {       
    g95_add_expr_to_block(&se1->pre, tmp0);    
    se1->expr = var;   
  }  
}       
       
       
    
    
/* intrinsic_cmov()-- Intrinsic conditional move subroutine generated
 * by WHERE expansion.  If the first argument is true, the second
 * argument is assigned the third argument. */       
       
static tree intrinsic_cmov(g95_code *codep) {         
g95_expr *left, *right;
g95_actual_arglist *z;  
stmtblock_t blk;        
g95_se s;        
tree tmp;        
        
  g95_init_se(&s, NULL);          
          
  z = codep->ext.actual;         
         
  g95_init_se(&s, NULL);        
  g95_conv_expr(&s, z->u.expr);          
          
  z = z->next;     
  left = z->u.expr;   
   
  z = z->next;     
  right = z->u.expr;     
     
  tmp = fold(build(NE_EXPR, boolean_type_node, s.expr, integer_zero_node)); 
  tmp = fold(build_v(COND_EXPR, tmp, g95_trans_assignment(left, right),  
		     empty_stmt_node));    
    
  g95_init_block(&blk);          
  g95_add_block_to_block(&blk, &s.pre);         
  g95_add_expr_to_block(&blk, tmp);     
  g95_add_block_to_block(&blk, &s.post);    
    
  return g95_finish_block(&blk);      
} 
 
 
   
   
/* intrinsic_adjust()-- Left or right justification of strings. */         
         
static void intrinsic_adjust(g95_se *se1, g95_expr *l, int right_flag) {        
tree tmp, r;         
g95_se s;          
char *name; 
 
  convert_args(se1, l, &s);

  r = g95_temp_string(se1, s.string_length);     
     
  name = right_flag ? PREFIX "adjustr" : PREFIX "adjustl";   
  tmp = g95_call_library(void_type_node, name,       
			 r, s.expr, s.string_length, NULL_TREE);       
       
  g95_add_expr_to_block(&se1->pre, tmp);      
      
  se1->expr = r;  
  se1->string_length = s.string_length;   
}  
  
  
  
  
/* intrinsic_ichar()-- Return the integer of the first character in a
 * string. */        
        
static void intrinsic_ichar(g95_se *se0, g95_expr *e) {      
tree tmp0; 
g95_se v;        
        
  convert_args(se0, e, &v, NULL);   
   
  tmp0 = build1(INDIRECT_REF, g95_character1_type_node, v.expr);    
  se0->expr = convert(g95_default_integer, tmp0);          
}        
        
        
   
   
/* access_arg()-- Given an expression that is a function call
 * expression, return a particular argument number (zero based). */     
     
static g95_expr *access_arg(g95_expr *t, int narg) {          
g95_actual_arglist *m;       
       
  m = t->value.function.actual;          
          
  for(; narg>0; narg--)         
    m = m->next;         
         
  return m->u.expr;         
}          
          
          
       
       
/* intrinsic_associated()-- Test for pointer association */    
    
static void intrinsic_associated(g95_se *se1, g95_expr *expr) {    
g95_expr *pointer_expr, *target_expr;
symbol_attribute atr;          
tree pointer, target;        
g95_se se2;       
       
  pointer_expr = access_arg(expr, 0);    
  target_expr = access_arg(expr, 1);      
      
  g95_init_se(&se2, NULL); 
 
  se2.reflevel = (pointer_expr->rank == 0);        
  g95_conv_expr(&se2, pointer_expr);          
          
  if (pointer_expr->rank > 0)      
    se2.expr = g95_adesc_base(se2.expr);   
   
  g95_add_block_to_block(&se1->pre, &se2.pre);       
  g95_add_block_to_block(&se1->post, &se2.post);    
    
  pointer = se2.expr;      
      
  if (target_expr == NULL) { /* See if the pointer is associated */ 
    se1->expr = build(NE_EXPR, boolean_type_node, pointer, null_pointer_node);     
    return;    
  }  
  
  /* See if the pointer is associated with a particular target */          
          
  g95_init_se(&se2, NULL);    
    
  atr = g95_variable_attr(target_expr, NULL);     
     
  se2.reflevel = (target_expr->rank > 0) ? 0 : 1;          
  g95_conv_expr(&se2, target_expr);     
     
  g95_add_block_to_block(&se1->pre, &se2.pre);        
  g95_add_block_to_block(&se1->post, &se2.post);

  target = se2.expr;      
  if (target_expr->rank > 0)          
    target = g95_adesc_base(target);      
      
  se1->expr = build(EQ_EXPR, boolean_type_node, pointer, target);          
} 
 
 
          
          
/* intrinsic_abs()-- Absolute value */     
     
static void intrinsic_abs(g95_se *se1, g95_expr *e) {         
tree args, f;
g95_se g;       
       
  args = convert_args(se1, e, &g, NULL);       
       
  switch(access_arg(e, 0)->ts.type) {       
  case BT_INTEGER:        
  case BT_REAL:         
    se1->expr = build1(ABS_EXPR, TREE_TYPE(g.expr), g.expr);    
    break;     
     
  case BT_COMPLEX:  
    switch(e->ts.kind) {  
    case 4: f = gfor_fndecl_math_cabsf; break; 
    case 8: f = gfor_fndecl_math_cabs;  break;       
    default: abort();
    }        
          
    se1->expr = g95_build_function_call(f, args);       
    break;         
         
  default:        
    abort();  
  }        
}  
  
  
   
   
/* intrinsic_allocated()-- Test for allocation of an allocatable array. */        
        
static void intrinsic_allocated(g95_se *se1, g95_expr *e2) {
g95_se exp;    
tree tmp1;        
        
  g95_init_se(&exp, NULL);     
  exp.reflevel = 1;     
  g95_conv_expr(&exp, access_arg(e2, 0));       
       
  g95_add_block_to_block(&se1->pre,  &exp.pre);    
  g95_add_block_to_block(&se1->post, &exp.post);

  tmp1 = g95_adesc_base(exp.expr);      
  se1->expr = build(NE_EXPR, boolean_type_node, tmp1, null_pointer_node);         
}       
       
       
    
    
/* intrinsic_nint()-- Nearest integer.  Real argument, integer result */       
       
static void intrinsic_nint(g95_se *se0, g95_expr *e) { 
tree t;

  simple_libcall(se0, e);       
       
  if (e->ts.kind != g95_default_integer_kind()) {        
    t = g95_get_int_type(e->ts.kind); 
    se0->expr = convert(t, se0->expr); 
  }     
}


        
        
/* intrinsic_merge()-- The MERGE() intrinsic is just like the ternary
 * conditional operator in C. */      
      
static void intrinsic_merge(g95_se *s, g95_expr *e) {   
g95_se c, h, msk;        
tree tmp0; 
 
  convert_args(s, e, &c, &h, &msk);       
       
  tmp0 = build(NE_EXPR, boolean_type_node, msk.expr, integer_zero_node); 
  tmp0 = build(COND_EXPR, TREE_TYPE(c.expr), tmp0, c.expr, h.expr);        
        
  s->expr = fold(tmp0);       
  s->string_length = c.string_length;          
}     
     
     
         
         
/* intrinsic_char()-- Generate a string of length one. */     
     
static void intrinsic_char(g95_se *se, g95_expr *c) {         
tree var0, t;       
g95_se w;

  convert_args(se, c, &w, NULL);     
     
  var0 = g95_temp_string(se, integer_one_node);        
  t = build1(INDIRECT_REF, char_type_node, var0);  
  g95_add_modify_expr(&se->pre, t, w.expr);       
       
  se->expr = var0;         
  se->string_length = integer_one_node;        
} 
 
 
    
    
/* intrinsic_selected_int_kind()-- Figure out a integer kind value
 * given a range.  We implement this as a nested conditional
 * statement generated from the g95_integer_kinds[] table. */ 
 
static void intrinsic_selected_int_kind(g95_se *se0, g95_expr *e2) {    
int c, v, kind_index; 
tree tmp, rest, f;        
g95_se m;         
         
  convert_args(se0, e2, &m);       
       
  m.expr = save_expr(m.expr);
  rest = integer_minus_one_node;     
     
  /* Find the largest kind */  
  
  kind_index = 0;           
          
  for(c=1; g95_integer_kinds[c].kind != 0; c++)    
    if (g95_integer_kinds[c].kind > g95_integer_kinds[kind_index].kind)        
      kind_index = c; 
 
  /* Loop over integer kinds from largest kind to smallest */  
  
  do {  
    tmp = build_int_2(g95_integer_kinds[kind_index].range, 0);        
    tmp = build(LE_EXPR, boolean_type_node, m.expr, tmp);

    f = build_int_2(g95_integer_kinds[kind_index].kind, 0); 
    rest = build(COND_EXPR, g95_default_integer, tmp, f, rest);   
   
    /* Find the next smaller kind */  
  
    v = -1;    
    for(c=0; g95_integer_kinds[c].kind != 0; c++) 
      if ((v == -1 || g95_integer_kinds[c].kind > g95_integer_kinds[v].kind) &&      
	  g95_integer_kinds[c].kind < g95_integer_kinds[kind_index].kind)      
	v = c;   
   
    kind_index = v;
  } while(kind_index != -1);

  se0->expr = rest;  
}  
  
  
  
  
/* intrinsic_conjg()-- Get the complex conjugate. */      
      
static void intrinsic_conjg(g95_se *se, g95_expr *e1) {
g95_se y;      
      
  convert_args(se, e1, &y);      
  se->expr = build1(CONJ_EXPR, TREE_TYPE(y.expr), y.expr);          
}         
         
         
 
 
/* intrinsic_minmax()-- Get the minimum or maximum value of all the
 * parameters. */ 
 
static void intrinsic_minmax(g95_se *se1, g95_expr *e2, int op) {     
tree extremum, args;    
g95_typespec typesp;      
      
  args = convert_args(se1, e2, NULL);        
        
  extremum = TREE_VALUE(args);         
  args = TREE_CHAIN(args);          
          
  for(; args!=NULL_TREE; args=TREE_CHAIN(args))     
    extremum = build(op, TREE_TYPE(extremum), extremum, TREE_VALUE(args)); 
 
  se1->expr = extremum;     
     
  typesp = access_arg(e2, 0)->ts;  
  if (e2->ts.type != typesp.type || e2->ts.kind != typesp.kind) 
    se1->expr = convert(g95_typenode_for_spec(&e2->ts), se1->expr);        
}  
  
  
          
          
/* intrinsic_ibits()-- Extract a sequence of bits.
 * IBITS(I, POS, LEN) = (I >> POS) & ~((~0) << LEN).  */     
     
static void intrinsic_ibits(g95_se *se, g95_expr *e1) {  
tree dtype, tmp, msk;   
g95_se v, p, leng;       
       
  convert_args(se, e1, &v, &p, &leng);          
  dtype = TREE_TYPE(v.expr);         
         
  msk = build_int_2(-1, ~(unsigned HOST_WIDE_INT) 0);        
  msk = build(LSHIFT_EXPR, dtype, msk, leng.expr);    
  msk = build1(BIT_NOT_EXPR, dtype, msk);    
    
  tmp = build(RSHIFT_EXPR, dtype, v.expr, p.expr);          
  se->expr = fold(build(BIT_AND_EXPR, dtype, tmp, msk));       
}




/* intrinsic_dprod()-- Double precision product of two single
 * precision values. */      
      
static void intrinsic_dprod(g95_se *se0, g95_expr *expr) {  
g95_se g, m;         
tree type;    
    
  convert_args(se0, expr, &g, &m);  
  
  type = g95_typenode_for_spec(&expr->ts);  
  g.expr = convert(type, g.expr);          
  m.expr = convert(type, m.expr);

  se0->expr = build(MULT_EXPR, type, g.expr, m.expr);  
}       
       
       
  
  
/* intrinsic_tranfer()-- Implement the TRANSFER function.  If the
 * result not scalar, then we have to call a library function. */  
  
static void intrinsic_transfer(g95_se *se0, g95_expr *expr) {         
tree y, src, tmp, var, pvar;
g95_se source, mold, siz;

  convert_args(se0, expr, &source, &mold, &siz, NULL);   
   
  source.expr = build1(ADDR_EXPR, build_pointer_type(TREE_TYPE(source.expr)),       
		       source.expr);

  if (access_arg(expr, 1)->rank == 0 &&    
      access_arg(expr, 2) == NULL) { /* Scalar result */          
          
    se0->expr = source.expr;         
    TREE_TYPE(se0->expr) = build_pointer_type(TREE_TYPE(mold.expr));    
  } else { /* Rank 1 array result */ 
 
    /* If the source is an array, we need a pointer to a contiguous
     * block of data. */

    if (access_arg(expr, 0)->rank == 0)   
      src = source.expr;         
    else {   
      var = g95_create_var(pchar_type_node);
      TREE_ADDRESSABLE(var) = 1;     
     
      pvar = build1(ADDR_EXPR, pvoid_type_node, var);    
      g95_set_section_info(se0, 0, integer_zero_node);          
          
      src = g95_call_library(pvoid_type_node, PREFIX "contiguous_array", 
			     source.expr, pvar, null_pointer_node,        
			     NULL_TREE);    
    
      tmp = g95_call_library(void_type_node, PREFIX "contiguous_array_done",     
			     var, integer_zero_node, NULL_TREE);         
         
      g95_add_expr_to_block(&se0->post, tmp);     
    }         
         
  /* Figure out how big to make the array */   
   
    if (access_arg(expr, 2) != NULL) 
      y = siz.expr;        
    else {          
      if (access_arg(expr, 0)->rank == 0)      
	y = size_in_bytes(TREE_TYPE(source.expr));   
      else {         
	tmp = g95_call_library(g95_default_integer, PREFIX "size",       
			       source.expr, NULL_TREE);

	y = g95_get_adesc_esize(source.expr);         
	y = build(MULT_EXPR, g95_default_integer, y, tmp);   
      }      
      
      /* Convert size in bytes to size in elements */         
         
      tmp = size_in_bytes(g95_typenode_for_spec(&access_arg(expr, 1)->ts));          
      y = fold(build(CEIL_DIV_EXPR, g95_default_integer, y, tmp));      
    }        
        
    /* Initialize the descriptor */   
   
    tmp = g95_get_adesc_esize(mold.expr);    
    se0->expr = g95_transfer_result(&se0->pre, tmp, src, y);
  }    
    
  se0->string_length = mold.string_length;   
}         
         
         
     
     
/* intrinsic_int()-- Convert to an integer using the specified
 * rounding mode. */ 
 
static void intrinsic_int(g95_se *se1, g95_expr *e2) {      
tree type, tmp;      
g95_se f;        
        
  type = g95_typenode_for_spec(&e2->ts);   
  convert_args(se1, e2, &f, NULL);      
      
  switch(access_arg(e2, 0)->ts.type) {     
  case BT_INTEGER:          
    se1->expr = convert(type, f.expr);   /* Convert to a different kind */  
    break;      
      
  case BT_COMPLEX:        
    tmp = g95_get_real_type(access_arg(e2, 0)->ts.kind);       
    f.expr = build1(REALPART_EXPR, tmp, f.expr); 
 
    /* Fall through */  
  
  case BT_REAL:         
    se1->expr = build1(FIX_TRUNC_EXPR, type, f.expr);     
    break;

  default:          
    abort();    
  }  
}   
   
   
       
       
/* intrinsic_repeat()-- Repeat a given string. */

static void intrinsic_repeat(g95_se *se0, g95_expr *x) {
g95_se str, copies;         
tree length, var0, tmp; 
 
  convert_args(se0, x, &str, &copies);         
         
  copies.expr = save_expr(fold(build(MAX_EXPR, g95_default_integer,        
				     copies.expr, integer_zero_node)));        
        
  length = save_expr(fold(build(MULT_EXPR, g95_default_integer, copies.expr,       
				str.string_length)));          
          
  var0 = g95_temp_string(se0, length);          
  tmp = g95_call_library(void_type_node, PREFIX "repeat",        
			 var0, str.expr, str.string_length, copies.expr,     
			 NULL);         
         
  g95_add_expr_to_block(&se0->pre, tmp);     
     
  se0->expr = var0;          
  se0->string_length = length;      
}          
          
          
    
    
/* intrinsic_ishftc()-- Circular shift. */  
  
static void intrinsic_ishftc(g95_se *s, g95_expr *expr) {    
tree typ, l, tmp, lrot, rrot, k, bitsize, f;        
g95_se q, shift, siz;   
int y;       
       
  convert_args(s, expr, &q, &shift, &siz);  
  typ = g95_unsigned_type(TREE_TYPE(q.expr));   
   
  tmp = g95_create_var(typ);
  g95_add_modify_expr(&s->pre, tmp, q.expr);       
  q.expr = tmp;         
         
  if (siz.expr == NULL_TREE) { /* Special case */   
    lrot = build(LROTATE_EXPR, typ, q.expr, shift.expr);       
       
    tmp = build1(NEGATE_EXPR, TREE_TYPE(shift.expr), shift.expr);        
    rrot = build(RROTATE_EXPR, typ, q.expr, tmp);          
          
    tmp = build(GT_EXPR, boolean_type_node, shift.expr, integer_zero_node);  
    l = build(COND_EXPR, typ, tmp, lrot, rrot);    
    
    /* Do nothing if shift == 0.  */     
    tmp = build(EQ_EXPR, boolean_type_node, shift.expr, integer_zero_node); 
    s->expr = build(COND_EXPR, typ, tmp, q.expr, l);       
       
  } else {       
       
    /* General case.  Let M = (~0) >> (bitsize(I) - size)
     * If S>0:
     *   result = (I & ~M) | ((I << S) & M) | (((I & M) >> (size-S)) & M)
     * If S<0:
     *   result = (I & ~M) | (((I & M) >> -S) & M) | ((I << (size+S)) & M)
     * If S==0:
     *   result = I
     */

    shift.expr = save_expr(shift.expr);     
     
    tmp = g95_create_var(g95_default_integer);       
    g95_add_modify_expr(&s->pre, tmp, siz.expr);  
    siz.expr = tmp;    
    
    /* Calculate bitsize and the mask */     
     
    y = g95_validate_kind(BT_INTEGER, access_arg(expr, 0)->ts.kind);    
    bitsize = build_int_2(g95_integer_kinds[y].bit_size, 0);        
        
    tmp = build(MINUS_EXPR, g95_default_integer, bitsize, siz.expr);     
    l = build1(BIT_NOT_EXPR, typ, integer_zero_node);         
    l = build(RSHIFT_EXPR, typ, l, tmp);    
    
    k = g95_create_var(typ);     
    g95_add_modify_expr(&s->pre, k, l);          
          
    /* Left shift */ 
 
    tmp = build(BIT_AND_EXPR, typ, q.expr, k); 
    l = build(MINUS_EXPR, g95_default_integer, siz.expr, shift.expr);       
    tmp = build(RSHIFT_EXPR, typ, tmp, l);     
    tmp = build(BIT_AND_EXPR, typ, k, tmp);      
      
    l = build(LSHIFT_EXPR, typ, q.expr, shift.expr);
    l = build(BIT_AND_EXPR, typ, l, k);     
    lrot = build(BIT_IOR_EXPR, typ, tmp, l);      
      
    /* Right shift */     
     
    tmp = build(PLUS_EXPR, g95_default_integer, siz.expr, shift.expr);       
    tmp = build(LSHIFT_EXPR, typ, q.expr, tmp); 
    tmp = build(BIT_AND_EXPR, typ, k, tmp);   
   
    f = build(BIT_AND_EXPR, typ, q.expr, k);          
    l = build1(NEGATE_EXPR, g95_default_integer, shift.expr);     
    l = build(RSHIFT_EXPR, typ, f, l);       
    l = build(BIT_AND_EXPR, typ, k, l);   
   
    rrot = build(BIT_IOR_EXPR, typ, tmp, l);         
         
    tmp = build(GT_EXPR, boolean_type_node, shift.expr, integer_zero_node); 
    tmp = build(COND_EXPR, typ, tmp, lrot, rrot);  
  
    /* Final term */

    l = build1(BIT_NOT_EXPR, typ, k);     
    l = build(BIT_AND_EXPR, typ, l, q.expr);  
    l = build(BIT_IOR_EXPR, typ, l, tmp);  
  
    /* Zero shift condition */  
  
    tmp = build(EQ_EXPR, boolean_type_node, shift.expr, integer_zero_node);     
    s->expr = build(COND_EXPR, typ, tmp, q.expr, l); 
  } 
}       
       
       
     
     
/* intrinsic_size()-- implement the intrinsic SIZE function.  If the
 * DIM parameter is not present, we call a library subroutine. */  
  
static void intrinsic_size(g95_se *se, g95_expr *exp) {         
tree ubound, lbound;     
g95_se block, r;    
    
  if (access_arg(exp, 1) == NULL)  
    simple_libcall(se, exp);   
  else {    
    convert_args(se, exp, &block, &r);   
    r.expr = save_expr(r.expr); 
 
    ubound = g95_get_adesc_ubound(block.expr, r.expr);       
    lbound = g95_get_adesc_lbound(block.expr, r.expr);       
       
    ubound = build(PLUS_EXPR, g95_default_integer, ubound, integer_one_node);   
    ubound = fold(ubound);  
  
    g95_dim(se, ubound, lbound);        
  }    
}  
  
  
   
   
/* g95_conv_intrinsic_subroutine()-- Convert an intrinsic subroutine.
 * If we return NULL_TREE, the intrinsic is just a library call. */          
          
tree g95_conv_intrinsic_subroutine(g95_code *code) {         
tree decl; 
 
  switch(code->isym->generic_id) {   
  case G95_ISYM_CMOV:        
    decl = intrinsic_cmov(code);          
    break;

  default:
    decl = NULL_TREE;  
    break;         
  }

  return decl;        
}    
      
      
/* intrinsic_present()-- Test for the presense of an argument */         
         
static void intrinsic_present(g95_se *se, g95_expr *e1) {     
tree declr;  
  
  declr = access_arg(e1, 0)->symbol->backend_decl;      
      
  if (TREE_CODE(declr) != PARM_DECL)    
    declr = (tree) DECL_LANG_SPECIFIC(declr);     
     
  se->expr = build(NE_EXPR, boolean_type_node, declr, null_pointer_node);  
}      
      
      
   
   
/* intrinsic_trim()-- Trim the trailing blanks from a string.  The
 * string that we return is the original string with a new length.
 * Since trim() must appear on the right of an assignment, there
 * doesn't appear to be a need to copy the string. */    
    
static void intrinsic_trim(g95_se *s, g95_expr *e1) {       
g95_se st; 
 
  convert_args(s, e1, &st);        
        
  s->expr = st.expr;       
  s->string_length =   
    g95_call_library(g95_default_integer, PREFIX "len_trim_1",       
		     st.expr, st.string_length, NULL_TREE); 
}  
  
  
         
         
/* intrinsic_cmplx()-- Create a complex value from one or two real
 * components. */   
   
static void intrinsic_cmplx(g95_se *se, g95_expr *expr) {        
g95_se w, y;        
tree type;    
    
  convert_args(se, expr, &w, &y, NULL);

  if (access_arg(expr, 0)->ts.type == BT_COMPLEX) {  
    type = g95_get_complex_type(expr->ts.kind);   
    se->expr = convert(type, w.expr);  
  } else {   
    type = g95_get_real_type(expr->ts.kind);
    w.expr = convert(type, w.expr);

    if (y.expr == NULL_TREE) y.expr = integer_zero_node;
    y.expr = convert(type, y.expr);     
     
    type = g95_get_complex_type(expr->ts.kind);  
    se->expr = fold(build(COMPLEX_EXPR, type, w.expr, y.expr));  
  }       
}        
        
        
 
 
/* intrinsic_ishft()-- Care must be taken if the number of shifts is
 * greater than the word size.  The SHIFT_EXPRs are undefined for more
 * shifts than bits but the fortran standard is not. */   
   
static void intrinsic_ishft(g95_se *s, g95_expr *expr) {  
tree f, shifts, typ, tmp1, lshift, rshift, res, word_size;        
g95_se exp;       
int r;          
          
  g95_init_se(&exp, NULL);         
  g95_conv_expr(&exp, access_arg(expr, 0));  
  typ = TREE_TYPE(exp.expr);        
        
  g95_save_expr(&exp);    
  f = exp.expr;       
       
  g95_conv_expr(&exp, access_arg(expr, 1));  
  shifts = save_expr(exp.expr); 
 
  g95_add_block_to_block(&s->pre,  &exp.pre);          
  g95_add_block_to_block(&s->post, &exp.post);          
          
  /* Right shifts if nonpositive */       
  tmp1 = fold(build1(NEGATE_EXPR, g95_default_integer, shifts));    
  rshift = fold(build(RSHIFT_EXPR, unsigned_type_node, f, tmp1));      
      
  /* Left shifts if positive */  
  lshift = build(LSHIFT_EXPR, unsigned_type_node, f, shifts); 
 
  tmp1 = fold(build(GT_EXPR, boolean_type_node, shifts, integer_zero_node));       
  res = fold(build(COND_EXPR, typ, tmp1, lshift, rshift)); 
 
  /* Too many shifts mean a zero result */  
  
  r = g95_validate_kind(BT_INTEGER, access_arg(expr, 0)->ts.kind);
  r = g95_integer_kinds[r].bit_size;     
  word_size = build_int_2(r, 0);          
          
  tmp1 = fold(build1(ABS_EXPR, typ, f));       
  tmp1 = fold(build(GE_EXPR, boolean_type_node, shifts, word_size));

  s->expr = fold(build(COND_EXPR, typ, tmp1, integer_zero_node, res));  
}     
     
     
        
        
/* intrinsic_len()-- The length of a character string.  */         
         
static void intrinsic_len(g95_se *se1, g95_expr *e) {
g95_se string;      
      
  convert_args(se1, e, &string, &se1->expr); 
  se1->expr = string.string_length;     
}     
     
     
         
         
/* intrinsic_sign()-- SIGN(A, B) is absolute value of A times sign of
 * B. The real value versions use library functions to ensure the
 * correct handling of negative zero.  The integer case is implemented as:
 * SIGN(A, B) = ((a >= 0) .xor. (b >= 0)) ? a : -a  */          
          
static void intrinsic_sign(g95_se *s, g95_expr *exp) {       
tree arg, tmp1, type, testa, testb;    
g95_se j, m;      
      
  if (exp->ts.type == BT_REAL) {      
    simple_libcall(s, exp);        
    return;    
  }         
         
  arg = convert_args(s, exp, &j, &m);   
  type = TREE_TYPE(j.expr);

  testa = build(GE_EXPR, boolean_type_node, j.expr, integer_zero_node);         
  testb = build(GE_EXPR, boolean_type_node, m.expr, integer_zero_node);      
      
  tmp1 = build(TRUTH_XOR_EXPR, boolean_type_node, testa, testb);   
  s->expr = build(COND_EXPR, type,     
		   tmp1, build1(NEGATE_EXPR, type, j.expr), j.expr);    
}   
   
   
    
    
/* intrinsic_dim()-- Calculate a positive difference,
 * DIM(x, y) = ((x - y) < 0) ? 0 : x - y.  */ 
 
static void intrinsic_dim(g95_se *se0, g95_expr *e1) {        
g95_se v, c;          
          
  convert_args(se0, e1, &v, &c);     
  g95_dim(se0, v.expr, c.expr);        
}     
     
     
 
 
/* intrinsic_singlebitop()-- Set or clear a single bit.  */

static void intrinsic_singlebitop(g95_se *se1, g95_expr *expr, int set) {       
tree dtype, msk; 
g95_se e, p;      
int operand;    
    
  convert_args(se1, expr, &e, &p);
  dtype = TREE_TYPE(e.expr);        
        
  msk = fold(build(LSHIFT_EXPR, dtype, integer_one_node, p.expr));

  if (set)         
    operand = BIT_IOR_EXPR;    
  else {     
    operand = BIT_AND_EXPR;          
    msk = fold(build1(BIT_NOT_EXPR, dtype, msk));  
  }     
     
  se1->expr = fold(build(operand, dtype, e.expr, msk));        
}        
        
        
      
      
/* intrinsic_modulus()-- Works for both mod() and modulo() */        
        
static void intrinsic_modulus(g95_se *s, g95_expr *e) {        
g95_se f, q;  
  
  switch(e->ts.type) {       
  case BT_INTEGER:          
    convert_args(s, e, &f, &q); 
    s->expr = build(TRUNC_MOD_EXPR, TREE_TYPE(f.expr), f.expr, q.expr);       
    break;   
   
  case BT_REAL:     
    simple_libcall(s, e);
    break;         
         
  default:
    abort();    
  }  
}      
      
      
       
       
/* intrinsic_spread()-- Implement SPREAD().  For the version for a
 * scalar source variable, we have to add the length of the source to
 * the argument list. */          
          
static void intrinsic_spread(g95_se *se, g95_expr *e1) {       
tree arg, size, decl, name0, t;       
       
  if (access_arg(e1, 0)->rank != 0) 
    simple_libcall(se, e1);      
  else {   
    arg = g95_trans_arglist(e1->value.function.actual, se);      
      
    size = size_in_bytes(g95_typenode_for_spec(&access_arg(e1, 0)->ts));       
    g95_chainon_list(arg, size);        
        
    name0 = get_identifier(e1->value.function.name); 
 
    t = build_pointer_type(g95_get_array_desc(e1->rank));
    decl = build_function_type(t, NULL);          
    decl = build_decl(FUNCTION_DECL, name0, decl);

    DECL_EXTERNAL(decl) = 1;     
    TREE_PUBLIC(decl) = 1;    
    
    pushdecl(decl);        
    rest_of_decl_compilation(decl, NULL, 1, 0);         
         
    se->expr = g95_build_function_call(decl, arg);     
  }       
}        
        
        


/* intrinsic_not()-- Logical not. */   
   
static void intrinsic_not(g95_se *s, g95_expr *e1) { 
g95_se x;  
  
  convert_args(s, e1, &x);      
  s->expr = build1(BIT_NOT_EXPR, TREE_TYPE(x.expr), x.expr); 
}        
        
        
       
       
/* intrinsic_ceiling_floor()-- Handle the CEILING() and FLOOR()
 * intrinsics */    
    
static void intrinsic_ceiling_floor(g95_se *se1, g95_expr *e2) {    
tree type;          
          
  simple_libcall(se1, e2);   
   
  if (e2->ts.kind != g95_default_integer_kind()) {        
    type = g95_get_int_type(e2->ts.kind);         
    se1->expr = convert(type, se1->expr); 
  }  
}          
          
          


/* intrinsic_bound()-- Build an expression for LBOUND or UBOUND.  If
 * the DIM parameter is not present, we call a library subroutine. */

static void intrinsic_bound(g95_se *se1, g95_expr *e2, int up) {      
g95_se array, rank;     
     
  if (access_arg(e2, 1) == NULL)      
    simple_libcall(se1, e2); 
  else {          
    convert_args(se1, e2, &array, &rank);    
    
    se1->expr = up 
      ? g95_get_adesc_ubound(array.expr, rank.expr)       
      : g95_get_adesc_lbound(array.expr, rank.expr);     
  }      
}       
       
       


/* intrinsic_btest()-- Bit test.  BTEST (i, pos) = (i & (1 << pos)) != 0.  */ 
 
static void intrinsic_btest(g95_se *se0, g95_expr *expr) {
tree tmp0, dtype;          
g95_se a, p;        
        
  convert_args(se0, expr, &a, &p);
  dtype = TREE_TYPE(a.expr);         
         
  tmp0 = build(MINUS_EXPR, dtype, p.expr, integer_one_node);      
  tmp0 = build(LSHIFT_EXPR, dtype, integer_one_node, tmp0);         
  tmp0 = build(BIT_AND_EXPR, dtype, a.expr, tmp0);         
  se0->expr = fold(build(NE_EXPR, boolean_type_node, tmp0, integer_zero_node));     
}   
   
   
        
        
/* g95_conv_intrinsic_function()-- Generate code for an intrinsic
 * function.  Some map directly to library calls, others get special
 * handling.  In some cases the name of the function used depends on
 * the type specifiers.  */

void g95_conv_intrinsic_function(g95_se *se, g95_expr *e) { 
g95_intrinsic_sym *isym;        
        
  isym = e->value.function.isym;   
   
  switch(e->value.function.isym->generic_id) {    
  case G95_ISYM_SELECTED_INT_KIND:  
    intrinsic_selected_int_kind(se, e); 
    break;          
          
  case G95_ISYM_SELECTED_REAL_KIND:     
    intrinsic_selected_real_kind(se, e);          
    break;       
       
  case G95_ISYM_ALLOCATED:
    intrinsic_allocated(se, e);          
    break; 
 
  case G95_ISYM_REPEAT:    
    intrinsic_repeat(se, e);   
    break;       
       
  case G95_ISYM_ADJUSTL:     
    intrinsic_adjust(se, e, 0);
    break;     
     
  case G95_ISYM_ADJUSTR:        
    intrinsic_adjust(se, e, 1);  
    break;       
       
  case G95_ISYM_PRESENT: 
    intrinsic_present(se, e);
    break;  
  
  case G95_ISYM_ASSOCIATED:     
    intrinsic_associated(se, e);         
    break;         
         
  case G95_ISYM_ABS:         
    intrinsic_abs(se, e);          
    break; 
 
  case G95_ISYM_AIMAG:       
    intrinsic_aimag(se, e);
    break;

  case G95_ISYM_BTEST:
    intrinsic_btest(se, e);      
    break;   
   
  case G95_ISYM_ACHAR:         
  case G95_ISYM_CHAR:      
    intrinsic_char(se, e);         
    break;     
     
  case G95_ISYM_CEILING:   
  case G95_ISYM_FLOOR:   
    intrinsic_ceiling_floor(se, e);      
    break;       
       
  case G95_ISYM_TRIM:    
    intrinsic_trim(se, e);  
    break;         
         
  case G95_ISYM_CONVERSION:  
  case G95_ISYM_REAL:  
  case G95_ISYM_LOGICAL:         
  case G95_ISYM_DBLE:    
    intrinsic_conversion(se, e);       
    break;   
   
    /* Integer conversions are handled seperately to make sure we get
     * the correct rounding mode. */        
        
  case G95_ISYM_INT:  
    intrinsic_int(se, e);         
    break;       
       
  case G95_ISYM_NINT:  
    intrinsic_nint(se, e); 
    break; 
 
  case G95_ISYM_MOD: 
  case G95_ISYM_MODULO:     
    intrinsic_modulus(se, e);         
    break;

  case G95_ISYM_CMPLX:          
    intrinsic_cmplx(se, e);     
    break;     
     
  case G95_ISYM_CONJG: 
    intrinsic_conjg(se, e);        
    break;      
      
  case G95_ISYM_DIM:   
    intrinsic_dim(se, e);    
    break; 
 
  case G95_ISYM_DPROD:       
    intrinsic_dprod(se, e);     
    break;   
   
  case G95_ISYM_IAND:
    intrinsic_bitop(se, e, BIT_AND_EXPR);  
    break;    
    
  case G95_ISYM_IBCLR:      
    intrinsic_singlebitop(se, e, 0);       
    break;          
          
  case G95_ISYM_IBITS:          
    intrinsic_ibits(se, e);      
    break;      
      
  case G95_ISYM_IBSET:   
    intrinsic_singlebitop(se, e, 1);
    break;

  case G95_ISYM_IACHAR:        
  case G95_ISYM_ICHAR:  
    intrinsic_ichar(se, e);   
    break;

  case G95_ISYM_IEOR:      
    intrinsic_bitop(se, e, BIT_XOR_EXPR); 
    break;       
       
  case G95_ISYM_IOR:         
    intrinsic_bitop(se, e, BIT_IOR_EXPR);   
    break;       
       
  case G95_ISYM_ISHFT:       
    intrinsic_ishft(se, e);    
    break;

  case G95_ISYM_ISHFTC:         
    intrinsic_ishftc(se, e);       
    break;       
       
  case G95_ISYM_LBOUND:        
    intrinsic_bound(se, e, 0);    
    break; 
 
  case G95_ISYM_LEN:    
    intrinsic_len(se, e);         
    break;

  case G95_ISYM_LGE:         
    intrinsic_strcmp(se, e, GE_EXPR);        
    break;          
          
  case G95_ISYM_LGT:     
    intrinsic_strcmp(se, e, GT_EXPR);     
    break;     
     
  case G95_ISYM_LLE:        
    intrinsic_strcmp(se, e, LE_EXPR);
    break;     
     
  case G95_ISYM_LLT: 
    intrinsic_strcmp(se, e, LT_EXPR);      
    break;    
    
  case G95_ISYM_MAX:     
    intrinsic_minmax(se, e, MAX_EXPR);       
    break;    
    
  case G95_ISYM_MERGE:  
    intrinsic_merge(se, e);
    break;    
    
  case G95_ISYM_MIN:  
    intrinsic_minmax(se, e, MIN_EXPR);       
    break;   
   
  case G95_ISYM_NOT:       
    intrinsic_not(se, e);   
    break;

  case G95_ISYM_SIGN:   
    intrinsic_sign(se, e);      
    break;  
  
  case G95_ISYM_SIZE:     
    intrinsic_size(se, e);     
    break;   
   
  case G95_ISYM_TRANSFER:          
    intrinsic_transfer(se, e);       
    break;          
          
  case G95_ISYM_UBOUND:        
    intrinsic_bound(se, e, 1);       
    break;

  case G95_ISYM_SPREAD:      
    intrinsic_spread(se, e);      
    break;         
         
  default:     
    simple_libcall(se, e);    
    break;         
  }        
}         
         
         
