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
          
static tree convert_args VPARAMS((g95_se *se0, g95_expr *exp, ...)) {       
g95_actual_arglist *a;   
g95_se argse, *dst;        
tree argu;
int flag;         
         
  VA_OPEN(actualp, exp);        
  VA_FIXEDARG(actualp, g95_se *, se0);  
  VA_FIXEDARG(actualp, g95_expr *, exp);         
         
  argu = NULL_TREE;        
  dst = NULL;         
  flag = 1;          
          
  for(a=exp->value.function.actual; a; a=a->next) {  
    g95_init_se(&argse, se0);       
       
    if (a->u.expr == NULL)
      argse.expr = null_pointer_node;          
    else {      
      if (a->u.expr->ts.type != BT_CHARACTER)       
	g95_conv_expr(&argse, a->u.expr);   
      else {  
	argse.reflevel = 1;        
	g95_conv_expr(&argse, a->u.expr);    
    
	argu = g95_chainon_list(argu, argse.expr);   
	argu = g95_chainon_list(argu, argse.string_length);  
      }          
    }      
      
    g95_add_block_to_block(&se0->pre,  &argse.pre);         
    g95_add_block_to_block(&se0->post, &argse.post); 
 
    if (flag) {      
      dst = va_arg(actualp, g95_se *);         
      if (dst == NULL) flag = 0;
    }      
      
    if (dst != NULL) {       
      g95_init_se(dst, NULL);         
         
      dst->expr = (argse.expr == null_pointer_node) ? NULL : argse.expr;    
      dst->string_length = argse.string_length;    
    } 
 
    argu = g95_chainon_list(argu, argse.expr);          
  }        
        
  VA_CLOSE(actualp);         
  return argu;         
}  
  
  
       
       
/* intrinsic_bitop()-- Generate code to perform logical and, or and
 * exclusive-or. */

static void intrinsic_bitop(g95_se *se1, g95_expr *e, int o) {      
g95_se p, s;  
  
  convert_args(se1, e, &p, &s);         
  se1->expr = fold(build(o, TREE_TYPE(p.expr), p.expr, s.expr));        
}     
     
     
     
     
/* intrinsic_not()-- Logical not. */  
  
static void intrinsic_not(g95_se *se1, g95_expr *e1) {         
g95_se v;      
      
  convert_args(se1, e1, &v);   
  se1->expr = build1(BIT_NOT_EXPR, TREE_TYPE(v.expr), v.expr);         
}       
       
       
  
  
/* intrinsic_char()-- Generate a string of length one. */    
    
static void intrinsic_char(g95_se *s, g95_expr *l) {   
tree var0, t;      
g95_se o;      
      
  convert_args(s, l, &o, NULL);      
      
  var0 = g95_temp_string(s, integer_one_node);  
  t = build1(INDIRECT_REF, char_type_node, var0);    
  g95_add_modify_expr(&s->pre, t, o.expr);         
         
  s->expr = var0;          
  s->string_length = integer_one_node;     
}         
         
         
     
     
/* access_arg()-- Given an expression that is a function call
 * expression, return a particular argument number (zero based). */  
  
static g95_expr *access_arg(g95_expr *t, int narg) { 
g95_actual_arglist *y; 
 
  y = t->value.function.actual;      
      
  for(; narg>0; narg--)       
    y = y->next;        
        
  return y->u.expr;
}  
  
  
         
         
/* intrinsic_dprod()-- Double precision product of two single
 * precision values. */     
     
static void intrinsic_dprod(g95_se *se1, g95_expr *e) { 
g95_se l, a;         
tree dtype;     
     
  convert_args(se1, e, &l, &a);   
   
  dtype = g95_typenode_for_spec(&e->ts);
  l.expr = convert(dtype, l.expr);         
  a.expr = convert(dtype, a.expr);      
      
  se1->expr = build(MULT_EXPR, dtype, l.expr, a.expr); 
}          
          
          


/* intrinsic_cmplx()-- Create a complex value from one or two real
 * components. */    
    
static void intrinsic_cmplx(g95_se *se0, g95_expr *e1) {  
g95_se k, o;    
tree dtype;      
      
  convert_args(se0, e1, &k, &o, NULL);   
   
  if (access_arg(e1, 0)->ts.type == BT_COMPLEX) {    
    dtype = g95_get_complex_type(e1->ts.kind);      
    se0->expr = convert(dtype, k.expr);   
  } else {          
    dtype = g95_get_real_type(e1->ts.kind);  
    k.expr = convert(dtype, k.expr);

    if (o.expr == NULL_TREE) o.expr = integer_zero_node; 
    o.expr = convert(dtype, o.expr);       
       
    dtype = g95_get_complex_type(e1->ts.kind);       
    se0->expr = fold(build(COMPLEX_EXPR, dtype, k.expr, o.expr)); 
  }      
}         
         
         
          
          
/* intrinsic_selected_int_kind()-- Figure out a integer kind value
 * given a range.  We implement this as a nested conditional
 * statement generated from the g95_integer_kinds[] table. */    
    
static void intrinsic_selected_int_kind(g95_se *se1, g95_expr *exp) {      
int a, c, kind_index;     
tree t, rest, k;   
g95_se y;     
     
  convert_args(se1, exp, &y);     
     
  y.expr = save_expr(y.expr);          
  rest = integer_minus_one_node;      
      
  /* Find the largest kind */        
        
  kind_index = 0;          
         
  for(a=1; g95_integer_kinds[a].kind != 0; a++)
    if (g95_integer_kinds[a].kind > g95_integer_kinds[kind_index].kind)        
      kind_index = a;    
    
  /* Loop over integer kinds from largest kind to smallest */   
   
  do {  
    t = build_int_2(g95_integer_kinds[kind_index].range, 0);
    t = build(LE_EXPR, boolean_type_node, y.expr, t);

    k = build_int_2(g95_integer_kinds[kind_index].kind, 0);      
    rest = build(COND_EXPR, g95_default_integer, t, k, rest);        
        
    /* Find the next smaller kind */  
  
    c = -1;  
    for(a=0; g95_integer_kinds[a].kind != 0; a++)      
      if ((c == -1 || g95_integer_kinds[a].kind > g95_integer_kinds[c].kind) &&  
	  g95_integer_kinds[a].kind < g95_integer_kinds[kind_index].kind)     
	c = a; 
 
    kind_index = c;  
  } while(kind_index != -1);    
    
  se1->expr = rest;
} 
 
 
  
  
/* intrinsic_minmax()-- Get the minimum or maximum value of all the
 * parameters. */        
        
static void intrinsic_minmax(g95_se *se, g95_expr *expr, int op1) {      
tree extremum, argum;    
g95_typespec typesp; 
 
  argum = convert_args(se, expr, NULL);        
        
  extremum = TREE_VALUE(argum);    
  argum = TREE_CHAIN(argum);  
  
  for(; argum!=NULL_TREE; argum=TREE_CHAIN(argum))       
    extremum = build(op1, TREE_TYPE(extremum), extremum, TREE_VALUE(argum));         
         
  se->expr = extremum;  
  
  typesp = access_arg(expr, 0)->ts;       
  if (expr->ts.type != typesp.type || expr->ts.kind != typesp.kind)
    se->expr = convert(g95_typenode_for_spec(&expr->ts), se->expr);        
} 
 
 


/* intrinsic_ishftc()-- Circular shift. */  
  
static void intrinsic_ishftc(g95_se *se, g95_expr *exp) {   
tree type, t, lrot, rrot; 
g95_se b, shift, size;          
          
  convert_args(se, exp, &b, &shift, &size);       
       
  if (size.expr != NULL_TREE) abort();       
       
  /* Need g95_save_expr(i) here */     
     
  type = TREE_TYPE(b.expr);
  lrot = build(LROTATE_EXPR, type, b.expr, shift.expr);          
          
  t = build1(NEGATE_EXPR, TREE_TYPE(shift.expr), shift.expr);  
  rrot = build(RROTATE_EXPR, type, b.expr, t);  
  
  t = build(GT_EXPR, boolean_type_node, shift.expr, integer_zero_node);      
  rrot = build(COND_EXPR, type, t, lrot, rrot);        
        
  /* Do nothing if shift == 0.  */   
  t = build(EQ_EXPR, boolean_type_node, shift.expr, integer_zero_node);       
  se->expr = build(COND_EXPR, type, t, lrot, rrot);        
}     
     
     
       
       
/* intrinsic_conjg()-- Get the complex conjugate. */ 
 
static void intrinsic_conjg(g95_se *se0, g95_expr *e1) {        
g95_se t;   
   
  convert_args(se0, e1, &t);       
  se0->expr = build1(CONJ_EXPR, TREE_TYPE(t.expr), t.expr);        
}   
   
   
          
          
/* intrinsic_int()-- Convert to an integer using the specified
 * rounding mode. */   
   
static void intrinsic_int(g95_se *se1, g95_expr *e2) {    
tree typ, tmp;      
g95_se a;     
     
  typ = g95_typenode_for_spec(&e2->ts);   
  convert_args(se1, e2, &a, NULL);  
  
  switch(access_arg(e2, 0)->ts.type) {       
  case BT_INTEGER:  
    se1->expr = convert(typ, a.expr);   /* Convert to a different kind */  
    break;     
     
  case BT_COMPLEX:
    tmp = g95_get_real_type(access_arg(e2, 0)->ts.kind);
    a.expr = build1(REALPART_EXPR, tmp, a.expr);

    /* Fall through */

  case BT_REAL:    
    se1->expr = build1(FIX_TRUNC_EXPR, typ, a.expr);          
    break;       
       
  default:    
    abort();          
  }      
}          
          
          
  
  
/* intrinsic_merge()-- The MERGE() intrinsic is just like the ternary
 * conditional operator in C. */        
        
static void intrinsic_merge(g95_se *s, g95_expr *e1) {         
g95_se n, d, msk;    
tree tmp0;        
        
  convert_args(s, e1, &n, &d, &msk); 
 
  tmp0 = build(NE_EXPR, boolean_type_node, msk.expr, integer_zero_node);  
  tmp0 = build(COND_EXPR, TREE_TYPE(n.expr), tmp0, n.expr, d.expr);  
  
  s->expr = fold(tmp0);      
  s->string_length = n.string_length;        
}         
         
         
  
  
/* intrinsic_repeat()-- Repeat a given string. */          
          
static void intrinsic_repeat(g95_se *se1, g95_expr *k) {       
g95_se str, copies;         
tree leng, variable, tmp;

  convert_args(se1, k, &str, &copies);   
   
  copies.expr = save_expr(fold(build(MAX_EXPR, g95_default_integer,          
				     copies.expr, integer_zero_node)));        
        
  leng = save_expr(fold(build(MULT_EXPR, g95_default_integer, copies.expr,      
				str.string_length))); 
 
  variable = g95_temp_string(se1, leng);      
  tmp = g95_call_library(void_type_node, PREFIX "repeat",         
			 variable, str.expr, str.string_length, copies.expr,
			 NULL); 
 
  g95_add_expr_to_block(&se1->pre, tmp);     
     
  se1->expr = variable;  
  se1->string_length = leng;    
}    
    
    
          
          
/* intrinsic_aint()-- Truncate a real number. */ 
 
static void intrinsic_aint(g95_se *s, g95_expr *p) {     
char *function;      
tree tmp0; 
g95_se q;    
    
  convert_args(s, p, &q, NULL);  
  
  switch(access_arg(p, 0)->ts.kind) {        
  case 4:  function = "truncf"; break;   
  case 8:  function = "trunc";  break;    
  default:     
    g95_internal_error("intrinsic_aint(): Bad real kind");   
  }

  tmp0 = g95_call_library(TREE_TYPE(q.expr), function, q.expr, NULL_TREE);      
      
  if (p->ts.kind != access_arg(p, 0)->ts.kind)  
    tmp0 = convert(g95_get_real_type(p->ts.kind), tmp0);          
          
  s->expr = tmp0;
} 
 
 


/* intrinsic_convertion()-- Convert one type to another. */       
       
static void intrinsic_conversion(g95_se *se1, g95_expr *e) {    
g95_se ap;         
tree t;

  /* Evaluate the argument */      
  t = g95_typenode_for_spec(&e->ts);     
  assert(e->value.function.actual->u.expr);

  convert_args(se1, e, &ap, NULL);         
         
  /* Conversion from complex to non-complex involves taking the real
   * component of the value.  */  
  
  if (TREE_CODE(TREE_TYPE(ap.expr)) == COMPLEX_TYPE 
      && e->ts.type != BT_COMPLEX) { 
      tree artype;        
        
      artype = TREE_TYPE(TREE_TYPE(ap.expr));    
      ap.expr = build1(REALPART_EXPR, artype, ap.expr);  
    }       
       
  se1->expr = convert(t, ap.expr);    
}


    
    
/* intrinsic_aimag()-- Get the imaginary component of a value. */  
  
static void intrinsic_aimag(g95_se *se, g95_expr *expr) {      
tree type;          
g95_se f;   
   
  convert_args(se, expr, &f);     
  type = g95_get_real_type(expr->ts.kind);          
  se->expr = build1(IMAGPART_EXPR, type, f.expr);         
}          
          
          


/* intrinsic_len()-- The length of a character string.  */     
     
static void intrinsic_len(g95_se *se, g95_expr *expr) {
g95_se s;          
          
  convert_args(se, expr, &s, &se->expr);   
  se->expr = s.string_length; 
}      
      
      
       
       
/* simple_libcall()-- Generate a call to a library function.  The
 * function is declared with zero or more arguments, since the
 * argument list has already been vetted. */   
   
static void simple_libcall(g95_se *se, g95_expr *exp) {     
tree argu, name0, typ, var, tmp, rtype, decl;    
    
  argu = g95_trans_arglist(exp->value.function.actual, se);     
  name0 = get_identifier(exp->value.function.name);
  var = NULL_TREE;      
      
  if (exp->rank > 0)
    rtype = build_pointer_type(g95_get_array_desc(exp->rank));  
  else     
    switch(exp->ts.type) {       
    case BT_LOGICAL: case BT_INTEGER: case BT_REAL:        
      rtype = g95_typenode_for_spec(&exp->ts);         
      break;  
  
    case BT_CHARACTER:      
      g95_internal_error("simple_libcall(): Character return");
      break;       
       
    case BT_COMPLEX:  
      typ = g95_get_complex_type(exp->ts.kind);    
      var = g95_create_var(typ);        
      TREE_ADDRESSABLE(var) = 1;

      tmp = build1(ADDR_EXPR, build_pointer_type(typ), var);          
      tmp = tree_cons(NULL_TREE, tmp, NULL_TREE);        
      argu = chainon(tmp, argu);  
  
      rtype = void_type_node;       
      break;   
   
    default:         
      g95_internal_error("simple_libcall(): Bad function type");    
    }       
       
  decl = build_function_type(rtype, NULL_TREE);
  decl = build_decl(FUNCTION_DECL, name0, decl);          
          
  DECL_EXTERNAL(decl) = 1;        
  TREE_PUBLIC(decl) = 1;       
       
  pushdecl(decl);   
  rest_of_decl_compilation(decl, NULL, 1, 0);     
     
  tmp = g95_build_function_call(decl, argu); 
 
  if (var == NULL_TREE)         
    se->expr = tmp;         
  else {     
    g95_add_expr_to_block(&se->pre, tmp); 
    se->expr = var;
  }   
}        
        
        
 
 
/* intrinsic_ichar()-- Return the integer of the first character in a
 * string. */     
     
static void intrinsic_ichar(g95_se *se0, g95_expr *e1) {  
tree tmp;     
g95_se b;       
       
  convert_args(se0, e1, &b, NULL);    
    
  tmp = build1(INDIRECT_REF, g95_character1_type_node, b.expr);          
  se0->expr = convert(g95_default_integer, tmp);  
}    
    
    
    
    
/* intrinsic_sign()-- SIGN(A, B) is absolute value of A times sign of
 * B. The real value versions use library functions to ensure the
 * correct handling of negative zero.  The integer case is implemented as:
 * SIGN(A, B) = ((a >= 0) .xor. (b >= 0)) ? a : -a  */         
         
static void intrinsic_sign(g95_se *se, g95_expr *e2) {          
tree argum, tmp1, typ, testa, testb;         
g95_se l, v;      
      
  if (e2->ts.type == BT_REAL) {        
    simple_libcall(se, e2);         
    return;  
  }       
       
  argum = convert_args(se, e2, &l, &v);      
  typ = TREE_TYPE(l.expr);        
        
  testa = build(GE_EXPR, boolean_type_node, l.expr, integer_zero_node);         
  testb = build(GE_EXPR, boolean_type_node, v.expr, integer_zero_node); 
 
  tmp1 = build(TRUTH_XOR_EXPR, boolean_type_node, testa, testb);          
  se->expr = build(COND_EXPR, typ,  
		   tmp1, build1(NEGATE_EXPR, typ, l.expr), l.expr);    
}         
         
         
     
     
/* intrinsic_dim()-- Calculate a positive difference,
 * DIM(x, y) = ((x - y) < 0) ? 0 : x - y.  */  
  
static void intrinsic_dim(g95_se *se0, g95_expr *expr) {       
g95_se h, r;       
       
  convert_args(se0, expr, &h, &r);    
  g95_dim(se0, h.expr, r.expr); 
}      
      
      
     
     
/* intrinsic_size()-- implement the intrinsic SIZE function.  If the
 * DIM parameter is not present, we call a library subroutine. */   
   
static void intrinsic_size(g95_se *s, g95_expr *e) { 
tree ubound, lbound;   
g95_se ap, dim;       
       
  if (access_arg(e, 1) == NULL)       
    simple_libcall(s, e);         
  else { 
    convert_args(s, e, &ap, &dim);  
    dim.expr = save_expr(dim.expr);       
       
    ubound = g95_get_adesc_ubound(ap.expr, dim.expr);    
    lbound = g95_get_adesc_lbound(ap.expr, dim.expr);  
  
    ubound = build(PLUS_EXPR, g95_default_integer, ubound, integer_one_node);
    ubound = fold(ubound);  
  
    g95_dim(s, ubound, lbound);    
  }      
}          
          
          
       
       
/* intrinsic_allocated()-- Test for allocation of an allocatable array. */      
      
static void intrinsic_allocated(g95_se *se1, g95_expr *e) {       
g95_se se2;   
tree tmp0;      
      
  g95_init_se(&se2, NULL);        
  se2.reflevel = 1;          
  g95_conv_expr(&se2, access_arg(e, 0));       
       
  g95_add_block_to_block(&se1->pre,  &se2.pre);     
  g95_add_block_to_block(&se1->post, &se2.post);         
         
  tmp0 = g95_adesc_base(se2.expr);
  se1->expr = build(NE_EXPR, boolean_type_node, tmp0, null_pointer_node);          
}    
    
    
      
      
/* intrinsic_singlebitop()-- Set or clear a single bit.  */     
     
static void intrinsic_singlebitop(g95_se *s, g95_expr *e2, int set) {       
tree t, maski;  
g95_se v, posit;          
int op1;         
         
  convert_args(s, e2, &v, &posit);
  t = TREE_TYPE(v.expr);      
      
  maski = fold(build(LSHIFT_EXPR, t, integer_one_node, posit.expr)); 
 
  if (set)         
    op1 = BIT_IOR_EXPR;  
  else {        
    op1 = BIT_AND_EXPR;  
    maski = fold(build1(BIT_NOT_EXPR, t, maski));     
  }   
   
  s->expr = fold(build(op1, t, v.expr, maski));      
}        
        
        
     
     
/* intrinsic_present()-- Test for the presense of an argument */

static void intrinsic_present(g95_se *se1, g95_expr *e) {  
tree decl;     
     
  decl = access_arg(e, 0)->symbol->backend_decl;       
  if (TREE_CODE(decl) != PARM_DECL) decl = (tree) DECL_LANG_SPECIFIC(decl);        
        
  se1->expr = build(NE_EXPR, boolean_type_node, decl, null_pointer_node);      
}       
       
       


/* intrinsic_ceiling_floor()-- Handle the CEILING() and FLOOR()
 * intrinsics */          
          
static void intrinsic_ceiling_floor(g95_se *se, g95_expr *e1) {      
tree t;

  simple_libcall(se, e1);          
          
  if (e1->ts.kind != g95_default_integer_kind()) {       
    t = g95_get_int_type(e1->ts.kind);          
    se->expr = convert(t, se->expr);        
  }     
}  
  
  


/* intrinsic_nint()-- Nearest integer.  Real argument, integer result */          
          
static void intrinsic_nint(g95_se *se0, g95_expr *e1) {
tree t;    
    
  simple_libcall(se0, e1);         
         
  if (e1->ts.kind != g95_default_integer_kind()) {      
    t = g95_get_int_type(e1->ts.kind);         
    se0->expr = convert(t, se0->expr);          
  }   
}      
      
      
   
   
/* intrinsic_selected_real_kind()-- Figure out a integer kind value
 * given a range and/or precision.  We implement this as a nested
 * conditional statements generated from the g95_real_kinds[] table. */         
         
static void intrinsic_selected_real_kind(g95_se *s, g95_expr *e) {    
tree tmp, p_rest, r_rest, x, f, u, z, q, cond, w, g;        
int h, y, kind_index;
g95_se a, t;   
   
  convert_args(s, e, &a, &t);       
  w = a.expr;         
  g = t.expr;        
        
  if (w != NULL_TREE) {        
    w = save_expr(w);      
    p_rest = integer_minus_one_node;      
  }   
   
  if (g != NULL_TREE) { 
    g = save_expr(g);         
    r_rest = build_int_2(-2, -1);       
  } 
 
  assert(w != NULL_TREE || g != NULL_TREE);      
  kind_index = 0;    
    
  /* Largest kind */ 
 
  for(h=1; g95_real_kinds[h].kind != 0; h++)
    if (g95_real_kinds[h].kind > g95_real_kinds[kind_index].kind)      
      kind_index = h;    
    
  /* Generate tree for p and r branches */  
  
  do {
    q = build_int_2(g95_real_kinds[kind_index].kind, 0);    
    
    if (w != NULL) {      
      tmp = build_int_2(g95_real_kinds[kind_index].precision, 0);
      tmp = build(LE_EXPR, boolean_type_node, w, tmp);     
		         
      p_rest = build(COND_EXPR, g95_default_integer, tmp, q, p_rest);        
    }   
   
    if (g != NULL) {         
      tmp = build_int_2(g95_real_kinds[kind_index].range, 0);
      tmp = build(LE_EXPR, boolean_type_node, g, tmp);     
     
      r_rest = build(COND_EXPR, g95_default_integer, tmp, q, r_rest);
    }     
     
    /* Find the next highest real kind */ 
 
    y = -1;      
    for(h=0; g95_real_kinds[h].kind != 0; h++)          
      if ((y == -1 || g95_real_kinds[h].kind > g95_real_kinds[y].kind) &&
	  g95_real_kinds[h].kind < g95_real_kinds[kind_index].kind) 
	y = h;  
  
    kind_index = y;    
  } while(kind_index != -1);          
          
  /* If we've only got one parameter, then we already have the answer. */     
     
  if (w == NULL_TREE) {         
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
       
  w = save_expr(p_rest);         
  g = save_expr(r_rest);          
          
  u = build(LT_EXPR, boolean_type_node, w, integer_zero_node);      
  z = build(LT_EXPR, boolean_type_node, g, integer_zero_node);        
  cond = build(BIT_AND_EXPR, boolean_type_node, u, z);        
        
  x = build_int_2(-3, -1);        
  f = build(MIN_EXPR, g95_default_integer, w, g);          
          
  f = build(COND_EXPR, g95_default_integer, cond, x, f); 
 
  /* First part */

  u = build(GE_EXPR, boolean_type_node, w, integer_zero_node);    
  z = build(GE_EXPR, boolean_type_node, g, integer_zero_node);        
  cond = build(BIT_AND_EXPR, boolean_type_node, u, z);        
        
  x = build(MAX_EXPR, g95_default_integer, w, g);   
  s->expr = build(COND_EXPR, g95_default_integer, cond, x, f);        
}   
   
   
   
   
/* intrinsic_ibits()-- Extract a sequence of bits.
 * IBITS(I, POS, LEN) = (I >> POS) & ~((~0) << LEN).  */   
   
static void intrinsic_ibits(g95_se *se1, g95_expr *exp) {     
tree dtype, tmp0, mask;          
g95_se l, posit, length;   
   
  convert_args(se1, exp, &l, &posit, &length);
  dtype = TREE_TYPE(l.expr);    
    
  mask = build_int_2(-1, ~(unsigned HOST_WIDE_INT) 0);      
  mask = build(LSHIFT_EXPR, dtype, mask, length.expr);          
  mask = build1(BIT_NOT_EXPR, dtype, mask);          
          
  tmp0 = build(RSHIFT_EXPR, dtype, l.expr, posit.expr);         
  se1->expr = fold(build(BIT_AND_EXPR, dtype, tmp0, mask));     
}   
   
   
    
    
/* g95_dim()-- Generate code for DIM(x, y). */      
      
void g95_dim(g95_se *se1, tree v, tree a) {    
tree t, val, tmp0, zero; 
 
  t = TREE_TYPE(v);       
       
  val = fold(build(MINUS_EXPR, t, v, a));         
  val = save_expr(val);   
   
  zero = g95_build_const(t, integer_zero_node);  
  tmp0 = build(LE_EXPR, boolean_type_node, val, zero);         
  se1->expr = fold(build(COND_EXPR, t, tmp0, zero, val)); 
}


  
  
/* intrinsic_tranfer()-- Implement the TRANSFER function.  If the
 * result not scalar, then we have to call a library function. */    
    
static void intrinsic_transfer(g95_se *se1, g95_expr *e1) {      
g95_actual_arglist *x;  
g95_expr *o;  
g95_se exp;   
tree t;          
int h;        
        
  if (e1->rank > 0) {
    /* If the source is scalar, we have to pass the size of the object */         
         
    o = access_arg(e1, 0);      
    if (o->rank == 0) {  
      h = int_size_in_bytes(g95_typenode_for_spec(&o->ts));

      x = g95_get_actual_arglist();        
      x->u.expr = g95_int_expr(h);        
        
      e1->value.function.actual->next->next = x;       
    }          
          
    simple_libcall(se1, e1);     
     
  } else {       
    g95_init_se(&exp, NULL);     
    exp.reflevel = 1;     
     
    g95_conv_expr(&exp, access_arg(e1, 0)); 
 
    g95_add_block_to_block(&se1->pre,  &exp.pre);  
    g95_add_block_to_block(&se1->post, &exp.post);         
    se1->expr = exp.expr;  
  
    o = access_arg(e1, 1);       
    if (o->ts.type == BT_CHARACTER)       
      se1->string_length = o->ts.cl->backend_decl;         
         
    t = g95_typenode_for_spec(&o->ts);     
    t = build_pointer_type(t); 
    TREE_TYPE(se1->expr) = t;     
  }         
}   
   
   
 
 
/* intrinsic_strcmp()-- Intrinsic string comparison functions. */      
      
static void intrinsic_strcmp(g95_se *se1, g95_expr *e2, int op) {       
g95_se w, g;     
     
  convert_args(se1, e2, &w, &g);    
   
  se1->expr = g95_call_library(g95_default_integer, PREFIX "compare_string",         
			      w.expr, w.string_length, g.expr, g.string_length, 
			      NULL_TREE); 
 
  se1->expr = build(op, boolean_type_node, se1->expr, integer_zero_node);  
}


 
 
/* intrinsic_adjust()-- Left or right justification of strings. */     
     
static void intrinsic_adjust(g95_se *se1, g95_expr *f, int right_flag) {
tree t, res;
g95_se str;          
char *name;       
       
  convert_args(se1, f, &str);        
        
  res = g95_temp_string(se1, str.string_length); 
 
  name = right_flag ? PREFIX "adjustr" : PREFIX "adjustl";  
  t = g95_call_library(void_type_node, name,         
			 res, str.expr, str.string_length, NULL_TREE);

  g95_add_expr_to_block(&se1->pre, t);         
         
  se1->expr = res;  
  se1->string_length = str.string_length;         
}       
       
       
     
     
/* intrinsic_abs()-- Absolute value */

static void intrinsic_abs(g95_se *se, g95_expr *e2) {          
tree arg, fndecl;   
g95_se s;         
         
  arg = convert_args(se, e2, &s, NULL);         
         
  switch(access_arg(e2, 0)->ts.type) {    
  case BT_INTEGER:
  case BT_REAL:          
    se->expr = build1(ABS_EXPR, TREE_TYPE(s.expr), s.expr);    
    break;      
      
  case BT_COMPLEX:  
    switch(e2->ts.kind) {        
    case 4: fndecl = gfor_fndecl_math_cabsf; break;          
    case 8: fndecl = gfor_fndecl_math_cabs;  break;      
    default: abort();          
    }         
           
    se->expr = g95_build_function_call(fndecl, arg);          
    break;          
          
  default: 
    abort();        
  }       
}    
    
    
    
    
/* intrinsic_ishft()-- Care must be taken if the number of shifts is
 * greater than the word size.  The SHIFT_EXPRs are undefined for more
 * shifts than bits but the fortran standard is not. */          
          
static void intrinsic_ishft(g95_se *s, g95_expr *e2) { 
tree e, shifts, t, tmp0, lshift, rshift, rslt, word_size;     
g95_se se0;
int d;     
     
  g95_init_se(&se0, NULL);     
  g95_conv_expr(&se0, access_arg(e2, 0));       
  t = TREE_TYPE(se0.expr);      
      
  g95_save_expr(&se0);       
  e = se0.expr;          
          
  g95_conv_expr(&se0, access_arg(e2, 1));
  shifts = save_expr(se0.expr);          
          
  g95_add_block_to_block(&s->pre,  &se0.pre);   
  g95_add_block_to_block(&s->post, &se0.post);

  /* Right shifts if nonpositive */
  tmp0 = fold(build1(NEGATE_EXPR, g95_default_integer, shifts));        
  rshift = fold(build(RSHIFT_EXPR, unsigned_type_node, e, tmp0));     
     
  /* Left shifts if positive */        
  lshift = build(LSHIFT_EXPR, unsigned_type_node, e, shifts);      
      
  tmp0 = fold(build(GT_EXPR, boolean_type_node, shifts, integer_zero_node));  
  rslt = fold(build(COND_EXPR, t, tmp0, lshift, rshift));    
    
  /* Too many shifts mean a zero result */         
         
  d = g95_validate_kind(BT_INTEGER, access_arg(e2, 0)->ts.kind); 
  d = g95_integer_kinds[d].bit_size;         
  word_size = build_int_2(d, 0);         
         
  tmp0 = fold(build1(ABS_EXPR, t, e));      
  tmp0 = fold(build(GE_EXPR, boolean_type_node, shifts, word_size));          
          
  s->expr = fold(build(COND_EXPR, t, tmp0, integer_zero_node, rslt));   
}    
    
    
       
       
/* intrinsic_modulus()-- Works for both mod() and modulo() */        
        
static void intrinsic_modulus(g95_se *se1, g95_expr *e1) {  
g95_se e, s;     
     
  switch(e1->ts.type) {       
  case BT_INTEGER:   
    convert_args(se1, e1, &e, &s);
    se1->expr = build(TRUNC_MOD_EXPR, TREE_TYPE(e.expr), e.expr, s.expr);          
    break;

  case BT_REAL:  
    simple_libcall(se1, e1);         
    break;       
       
  default:       
    abort();   
  }    
}


     
     
/* intrinsic_spread()-- Implement SPREAD().  For the version for a
 * scalar source variable, we have to add the length of the source to
 * the argument list. */    
    
static void intrinsic_spread(g95_se *se, g95_expr *e2) { 
tree a, siz, d, n;          
          
  if (access_arg(e2, 0)->rank != 0)      
    simple_libcall(se, e2);      
  else {
    a = g95_trans_arglist(e2->value.function.actual, se);

    siz = size_in_bytes(g95_typenode_for_spec(&access_arg(e2, 0)->ts)); 
    g95_chainon_list(a, siz);        
        
    n = get_identifier(e2->value.function.name);         
         
    d = build_function_type(pvoid_type_node, NULL);   
    d = build_decl(FUNCTION_DECL, n, d);       
       
    DECL_EXTERNAL(d) = 1;         
    TREE_PUBLIC(d) = 1;       
       
    pushdecl(d);          
    rest_of_decl_compilation(d, NULL, 1, 0);  
  
    se->expr = g95_build_function_call(d, a);          
  } 
}


 
 
/* intrinsic_bound()-- Build an expression for LBOUND or UBOUND.  If
 * the DIM parameter is not present, we call a library subroutine. */          
          
static void intrinsic_bound(g95_se *se1, g95_expr *e2, int up) { 
g95_se block, d;   
   
  if (access_arg(e2, 1) == NULL) 
    simple_libcall(se1, e2);  
  else {     
    convert_args(se1, e2, &block, &d);        
        
    se1->expr = up      
      ? g95_get_adesc_ubound(block.expr, d.expr)          
      : g95_get_adesc_lbound(block.expr, d.expr);    
  }    
}       
       
       
    
    
/* intrinsic_trim()-- Trim the trailing blanks from a string.  The
 * string that we return is the original string with a new length.
 * Since trim() must appear on the right of an assignment, there
 * doesn't appear to be a need to copy the string. */          
          
static void intrinsic_trim(g95_se *se0, g95_expr *e2) {    
g95_se s;         
         
  convert_args(se0, e2, &s);        
        
  se0->expr = s.expr; 
  se0->string_length =   
    g95_call_library(g95_default_integer, PREFIX "len_trim_1",
		     s.expr, s.string_length, NULL_TREE);       
}        
        
        
     
     
/* intrinsic_btest()-- Bit test.  BTEST (i, pos) = (i & (1 << pos)) != 0.  */     
     
static void intrinsic_btest(g95_se *se1, g95_expr *exp) {         
tree tmp1, typ;        
g95_se m, p;         
         
  convert_args(se1, exp, &m, &p);
  typ = TREE_TYPE(m.expr);     
     
  tmp1 = build(MINUS_EXPR, typ, p.expr, integer_one_node);   
  tmp1 = build(LSHIFT_EXPR, typ, integer_one_node, tmp1);   
  tmp1 = build(BIT_AND_EXPR, typ, m.expr, tmp1);    
  se1->expr = fold(build(NE_EXPR, boolean_type_node, tmp1, integer_zero_node));         
}   
   
   
       
       
/* intrinsic_associated()-- Test for pointer association */     
     
static void intrinsic_associated(g95_se *se1, g95_expr *e) {
g95_expr *pointer_expr, *target_expr;    
symbol_attribute attribute;     
tree pointer, target;  
g95_se se2;  
  
  pointer_expr = access_arg(e, 0);         
  target_expr = access_arg(e, 1);

  g95_init_se(&se2, NULL);           
  se2.reflevel = 1;  
  g95_conv_expr(&se2, pointer_expr);     
     
  g95_add_block_to_block(&se1->pre, &se2.pre);
  g95_add_block_to_block(&se1->post, &se2.post);         
         
  pointer = se2.expr;       
       
  if (target_expr == NULL) { /* See if the pointer is associated */          
    se1->expr = build(NE_EXPR, boolean_type_node, pointer, null_pointer_node);
    return;          
  }    
    
  g95_init_se(&se2, NULL);        
        
  attribute = g95_variable_attr(target_expr, NULL);         
  if (attribute.pointer) se2.reflevel = 1; 
 
  g95_conv_expr(&se2, target_expr);          
          
  g95_add_block_to_block(&se1->pre, &se2.pre);       
  g95_add_block_to_block(&se1->post, &se2.post); 
 
  target = se2.expr;    
    
  if (!attribute.pointer) target = build1(ADDR_EXPR, pvoid_type_node, target); 
 
  se1->expr = build(EQ_EXPR, boolean_type_node, pointer, target);
}      
      
      
          
          
/* g95_conv_intrinsic_function()-- Generate code for an intrinsic
 * function.  Some map directly to library calls, others get special
 * handling.  In some cases the name of the function used depends on
 * the type specifiers.  */

void g95_conv_intrinsic_function(g95_se *se, g95_expr *e1) {   
g95_intrinsic_sym *isym; 
 
  isym = e1->value.function.isym;         
         
  switch(e1->value.function.isym->generic_id) {
  case G95_ISYM_SELECTED_INT_KIND: 
    intrinsic_selected_int_kind(se, e1);  
    break;     
     
  case G95_ISYM_SELECTED_REAL_KIND:
    intrinsic_selected_real_kind(se, e1);         
    break;          
          
  case G95_ISYM_ALLOCATED:    
    intrinsic_allocated(se, e1);     
    break; 
 
  case G95_ISYM_REPEAT:  
    intrinsic_repeat(se, e1);    
    break;      
      
  case G95_ISYM_ADJUSTL:      
    intrinsic_adjust(se, e1, 0);          
    break;         
         
  case G95_ISYM_ADJUSTR:         
    intrinsic_adjust(se, e1, 1);  
    break;          
          
  case G95_ISYM_PRESENT:         
    intrinsic_present(se, e1);     
    break;    
    
  case G95_ISYM_ASSOCIATED:    
    intrinsic_associated(se, e1);        
    break;       
       
  case G95_ISYM_ABS:
    intrinsic_abs(se, e1);          
    break;   
   
  case G95_ISYM_AIMAG:
    intrinsic_aimag(se, e1); 
    break;

  case G95_ISYM_BTEST:     
    intrinsic_btest(se, e1);
    break;      
      
  case G95_ISYM_ACHAR:       
  case G95_ISYM_CHAR:          
    intrinsic_char(se, e1);   
    break;  
  
  case G95_ISYM_CEILING:   
  case G95_ISYM_FLOOR:         
    intrinsic_ceiling_floor(se, e1);
    break;    
    
  case G95_ISYM_TRIM:    
    intrinsic_trim(se, e1);       
    break;      
      
  case G95_ISYM_CONVERSION:     
  case G95_ISYM_REAL:   
  case G95_ISYM_LOGICAL:      
  case G95_ISYM_DBLE:        
    intrinsic_conversion(se, e1);   
    break;  
  
    /* Integer conversions are handled seperately to make sure we get
     * the correct rounding mode. */         
         
  case G95_ISYM_AINT:   
    intrinsic_aint(se, e1);   
    break;     
     
  case G95_ISYM_INT:          
    intrinsic_int(se, e1);      
    break;          
          
  case G95_ISYM_NINT:        
    intrinsic_nint(se, e1);        
    break;        
        
  case G95_ISYM_MOD:        
  case G95_ISYM_MODULO:       
    intrinsic_modulus(se, e1);          
    break;        
        
  case G95_ISYM_CMPLX:    
    intrinsic_cmplx(se, e1);  
    break;         
         
  case G95_ISYM_CONJG:     
    intrinsic_conjg(se, e1);        
    break;

  case G95_ISYM_DIM: 
    intrinsic_dim(se, e1);       
    break;        
        
  case G95_ISYM_DPROD:       
    intrinsic_dprod(se, e1);
    break;      
      
  case G95_ISYM_IAND:        
    intrinsic_bitop(se, e1, BIT_AND_EXPR);   
    break;  
  
  case G95_ISYM_IBCLR:        
    intrinsic_singlebitop(se, e1, 0);       
    break;         
         
  case G95_ISYM_IBITS:     
    intrinsic_ibits(se, e1);       
    break;  
  
  case G95_ISYM_IBSET:    
    intrinsic_singlebitop(se, e1, 1);    
    break;   
   
  case G95_ISYM_IACHAR:      
  case G95_ISYM_ICHAR:   
    intrinsic_ichar(se, e1);   
    break;  
  
  case G95_ISYM_IEOR:      
    intrinsic_bitop(se, e1, BIT_XOR_EXPR);        
    break;          
          
  case G95_ISYM_IOR: 
    intrinsic_bitop(se, e1, BIT_IOR_EXPR);    
    break;

  case G95_ISYM_ISHFT:
    intrinsic_ishft(se, e1); 
    break;     
     
  case G95_ISYM_ISHFTC:         
    intrinsic_ishftc(se, e1);       
    break; 
 
  case G95_ISYM_LBOUND: 
    intrinsic_bound(se, e1, 0);          
    break; 
 
  case G95_ISYM_LEN:  
    intrinsic_len(se, e1); 
    break;          
          
  case G95_ISYM_LGE:          
    intrinsic_strcmp(se, e1, GE_EXPR);          
    break;   
   
  case G95_ISYM_LGT: 
    intrinsic_strcmp(se, e1, GT_EXPR);          
    break; 
 
  case G95_ISYM_LLE:        
    intrinsic_strcmp(se, e1, LE_EXPR);      
    break;         
         
  case G95_ISYM_LLT:         
    intrinsic_strcmp(se, e1, LT_EXPR);        
    break;   
   
  case G95_ISYM_MAX:   
    intrinsic_minmax(se, e1, MAX_EXPR);
    break;    
    
  case G95_ISYM_MERGE:     
    intrinsic_merge(se, e1);      
    break; 
 
  case G95_ISYM_MIN:   
    intrinsic_minmax(se, e1, MIN_EXPR);        
    break;       
       
  case G95_ISYM_NOT:          
    intrinsic_not(se, e1);  
    break;         
         
  case G95_ISYM_SIGN:   
    intrinsic_sign(se, e1);     
    break; 
 
  case G95_ISYM_SIZE:     
    intrinsic_size(se, e1);        
    break;         
         
  case G95_ISYM_TRANSFER:         
    intrinsic_transfer(se, e1);          
    break;          
          
  case G95_ISYM_UBOUND:
    intrinsic_bound(se, e1, 1);          
    break;      
      
  case G95_ISYM_SPREAD: 
    intrinsic_spread(se, e1);     
    break;      
      
  default:  
    simple_libcall(se, e1);  
    break;     
  }        
}     
