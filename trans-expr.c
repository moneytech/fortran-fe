/* Expression translation
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
         
/* trans-expr.c-- generate SIMPLE trees for expressions.  */    
    
#include "trans.h"
 
/* Tree for computing the addition chains used for the computation of
 * integer powers.  See the discussion in Knuth's "The Art of Computer
 * Programming", Vol 2, section 4.6.3.  This table has been typed in
 * from figure 14.  If we start at node i, this contains the index of
 * the previous node and so on to one.  So for example if we are
 * interested in the addition chain for 10, the chain (in reverse
 * order) is 10, 5, 3, 2, 1. 
 *
 * The addition chain is a series of numbers that satisfies
 *  a_i = a_j + a_k, where i > j >= k.
 *
 * We start by computing x and x^2.  Following the addition chain
 * leads us to the desired power by a series of multiplications.  This
 * table is special in that it gives the minimal number of
 * multiplications. */    
    
static int power_tree[] = {  0,      
  /*  1-10  */   0,  1,  2,  2,  3,  3,  5,  4,  8,  5,
  /* 11-20  */  10,  6,  9,  7, 12,  8,  9, 16, 14, 10,         
  /* 21-30  */  11, 11, 20, 12, 24, 13, 15, 14, 28, 15,       
  /* 31-40  */  21, 16, 32, 17, 26, 18, 36, 19, 27, 20,
  /* 41-50  */  40, 21, 34, 22, 30, 23, 46, 24, 33, 25,
  /* 51-60  */  48, 26, 51, 27, 54, 28, 56, 29, 56, 30,       
  /* 61-70  */  52, 31, 60, 32, 64, 33, 66, 34, 68, 35,          
  /* 71-80  */  70, 36, 72, 37, 60, 38, 43, 39, 78, 40,        
  /* 81-90  */  65, 41, 80, 42, 80, 43, 86, 44, 88, 45,     
  /* 91-100 */  90, 46, 92, 47, 85, 48, 96, 49, 96, 50, 0 };       
       
#define MAX_CHAIN_LEN 50
     
        
        
/* component_array()-- Create a descriptor for the array.  TODO: if
 * the next reference is for an array element, then we can merge those
 * two steps and avoid initializing a descriptor that is only used
 * once.  Otherwise we are probably passing the array section as an
 * actual parameter and need the descriptor. */ 
 
static void component_array(g95_se *se, g95_component *s, tree storage) { 
variable_info vinfo;
tree desc;     
     
  g95_component_vinfo(s, &vinfo);         
  desc = g95_create_var(g95_get_descriptor(&vinfo), NULL);  
  g95_temp_array_descriptor(se, &vinfo, desc, storage);   
   
  se->expr = desc; 
}     
     
     
   
   
/* g95_trans_assignment()-- Translate an assignment statement */        
        
tree g95_trans_assignment(g95_expr *expr1, g95_expr *expr2) {          
stmtblock_t block;     
g95_se lse, rse;      
tree tmp; 
 
  if (expr1->rank > 0 || expr2->rank > 0)        
    g95_internal_error("Unexpected array assignment statement at %L",      
		       &expr1->where);    
    
  g95_start_block(&block);     
     
  g95_init_se(&lse, NULL);    
  g95_init_se(&rse, NULL);       
       
  if (expr1->ts.type == BT_CHARACTER) {   
    lse.reflevel = 1;         
    rse.reflevel = 1;       
  }     
     
  g95_conv_expr(&lse, expr1); 
  g95_conv_expr(&rse, expr2);   
   
  g95_add_block_to_block(&block, &lse.pre);          
  g95_add_block_to_block(&block, &rse.pre);    
    
  if (expr1->ts.type != BT_CHARACTER) 
    g95_add_modify_expr(&block, lse.expr, rse.expr);     
  else {   
    tmp = g95_call_library(void_type_node, PREFIX "copy_string",      
			   lse.expr, lse.string_length,      
			   rse.expr, rse.string_length, NULL_TREE);    
    
    g95_add_expr_to_block(&block, tmp);    
  }         
         
  g95_add_block_to_block(&block, &lse.post);      
  g95_add_block_to_block(&block, &rse.post);      
      
  return g95_finish_block(&block);     
}       


/* reciprocal()-- Given a tree, return the reciprocal. */     
     
static tree reciprocal(tree t) {       
tree tmp;      
      
  if (TREE_TYPE(t) == TREE_TYPE(integer_one_node))     
    tmp = build(TRUNC_DIV_EXPR, TREE_TYPE(t), integer_one_node, t);       
  else {        
    tmp = g95_build_const(TREE_TYPE(t), integer_one_node);       
    tmp = build(RDIV_EXPR, TREE_TYPE(t), tmp, t);
  }     
     
  return tmp;       
}   
   
   
   


/* g95_conv_char_length()-- Convert a character length parameter.
 * This is special in that negative lengths must be clamped to
 * zero.  The length must not be assumed. */ 
 
tree g95_conv_char_length(g95_se *se, g95_typespec *ts) { 
tree length, tmp;     
g95_se se0;        
        
  g95_init_se(&se0, NULL);     
     
  g95_conv_expr(&se0, ts->cl->length);      
  g95_add_block_to_block(&se->pre, &se0.pre); 
  g95_add_block_to_block(&se->post, &se0.post);  
  
  length = save_expr(se0.expr);         
         
  tmp = fold(build(LT_EXPR, boolean_type_node, length, integer_zero_node)); 
  length = fold(build(COND_EXPR, g95_default_integer, tmp, integer_zero_node,  
		      length)); 
 
  return length; 
}     
     
     
          
          
/* g95_conv_expr0()-- Convert an expression without having to worry
 * about the parent se. */

void g95_conv_expr0(g95_se *se, g95_expr *expr) {  
g95_se se0;    
    
  g95_init_se(&se0, NULL); 
  g95_conv_expr(&se0, expr);

  se->expr = se0.expr;      
      
  g95_add_block_to_block(&se->pre, &se0.pre);        
  g95_add_block_to_block(&se->post, &se0.post);   
}   
   
   
          
          
/* conv_concat_op()-- Handle a string concatenation operation.  A
 * temporary is allocated to hold the result */      
      
static void conv_concat_op(g95_se *se, g95_expr *expr) {          
g95_se left, right;     
tree var, tmp;      
      
  g95_init_se(&left, se);         
  left.reflevel = 1;        
  g95_conv_expr(&left, expr->op1); 
 
  g95_add_block_to_block(&se->pre, &left.pre);      
  g95_add_block_to_block(&se->post, &left.post);       
       
  g95_init_se(&right, se); 
  right.reflevel = 1;  
  g95_conv_expr(&right, expr->op2);

  g95_add_block_to_block(&se->pre, &right.pre);
  g95_add_block_to_block(&se->post, &right.post);         
         
  se->string_length = fold(build(PLUS_EXPR, g95_default_integer,   
				 left.string_length, right.string_length));

  se->string_length = save_expr(se->string_length);   
   
  var = g95_temp_string(se, se->string_length);          
          
  tmp = g95_call_library(void_type_node, PREFIX "concat_string", var,
			 left.expr,  left.string_length, 
			 right.expr, right.string_length, NULL_TREE);      
      
  g95_add_expr_to_block(&se->pre, tmp);

  se->expr = var;      
}        
        
        


/* g95_init_se()-- Initialize a simple expression holder.

   Care must be taken when multiple se are created with the same parent.
   The child se must be kept in sync.  The easiest way is to delay creation
   of a child se until after after the previous se has been translated.  */     
     
void g95_init_se(g95_se *se, g95_se *parent) {  
  
  memset(se, 0, sizeof(g95_se));       
  g95_init_block(&se->pre);         
  g95_init_block(&se->post);  
  
  se->parent = parent;   
}


   
   
/* g95_trans_arglist()-- Translate an actual argument list into tree
 * form.  Alternate return elements are ignored. */     
     
tree g95_trans_arglist(g95_actual_arglist *arg, g95_se *se) {         
tree arglist, arglist_tail;          
g95_se parmse;        
        
  arglist = NULL_TREE;       
  arglist_tail = NULL_TREE;         
         
  for(; arg!=NULL; arg=arg->next) {      
    g95_init_se(&parmse, NULL);     
     
    switch(arg->type) {          
    case ALT_RETURN: 
      continue;          
          
    case EXPR:
      if (arg->u.expr == NULL)      
	parmse.expr = null_pointer_node;       
      else {        
	parmse.reflevel = arg->pointer ? 2 : 1;        
	g95_conv_expr(&parmse, arg->u.expr);          
      }
      break;   
   
    case ARRAY_DESC: 
    case ARRAY_ELEMENT:    
    case FULL_ARRAY:       
      g95_array_argument(&parmse, arg);  
      break;        
    }  
  
    g95_add_block_to_block(&se->pre,  &parmse.pre);     
    g95_add_block_to_block(&se->post, &parmse.post);  
  
    arglist = g95_chainon_list(arglist, parmse.expr);    
    
    if (arg->u.expr == NULL) {       
      if (arg->missing_arg_type == BT_CHARACTER)        
	arglist_tail = g95_chainon_list(arglist_tail, integer_zero_node);       
    } else {   
      if (arg->u.expr->ts.type == BT_CHARACTER)      
	arglist_tail = g95_chainon_list(arglist_tail, parmse.string_length);
    }  
  }      
      
  if (arglist_tail != NULL_TREE) arglist = chainon(arglist, arglist_tail);

  return arglist;   
}   
   
   
          
          
/* g95_reflevel()-- Given something that is an object, a pointer to an
 * object or a pointer to a pointer to an object and a reference
 * level, generate code to bring the object to the desired level.
 * Level zero is a reference to some object, one is a pointer to an
 * object and two is a pointer to a pointer to the object. */          
          
void g95_reflevel(g95_se *se, int level) {        
tree tmp, type, new_type, object;     
     
  object = se->expr;        
  type = TREE_TYPE(object); 
 
  if (!POINTER_TYPE_P(type)) {  /* Object is not a pointer */ 
    switch(level) {    
    case 0:     
      break;          
          
    case 1:     
      if (CONSTANT_P(object) || TREE_CODE(object) == CALL_EXPR ||      
	  TREE_CODE(object) == NOP_EXPR) {
	tmp = g95_create_var(type, NULL);    
	g95_add_modify_expr(&se->pre, tmp, object);        
	object = tmp;          
      }     
     
      se->expr = build1(ADDR_EXPR, build_pointer_type(type), object);    
      break;     
     
    case 2: 
      /* Can't think of a case where this is needed */      
      g95_internal_error("g95_reflevel(): Bad case");  
    }  
  
    return;     
  }         
         
  new_type = TREE_TYPE(type);     
  if (!POINTER_TYPE_P(new_type)) {  /* Pointer to an object */        
    switch(level) {     
    case 0:    
      se->expr = build1(INDIRECT_REF, new_type, object);          
      break;       
       
    case 1:         
      break;  
  
    case 2: 
      se->expr = build1(ADDR_EXPR, build_pointer_type(type), object);       
      break;
    }  
  
    return;  
  }  
  
  new_type = TREE_TYPE(new_type);      
  if (!POINTER_TYPE_P(new_type)) {  /* Pointer to a pointer to an object */      
    switch(level) {       
    case 0:       
      new_type = build_pointer_type(new_type);   
      object = build1(INDIRECT_REF, new_type, object);       
       
      new_type = build_pointer_type(new_type);        
      se->expr = build1(INDIRECT_REF, new_type, object);          
      break;      
      
    case 1:        
      se->expr = build1(INDIRECT_REF, build_pointer_type(new_type), object);       
      break;         
         
    case 2:   
      break;     
    }       
       
    return;        
  }         
         
  g95_internal_error("g95_reflevel(): Bad type");
}          
          
          


/* conv_substring()-- Converts a string into a substring.  The
 * standard specifies that the start and end indexes be within the
 * range of the string.  This implementation doesn't check for
 * violations of this condition, allowing access to memory outside of
 * the string.  To fix this, start and end would need to be clamped to
 * the legal ranges. */        
        
static void conv_substring(g95_se *se, g95_ref *reference) {
tree start_minus_one, type; 
g95_se start, end, length;         
         
  g95_reflevel(se, 1);      
  g95_init_se(&start, se);         
         
  if (reference->u.ss.start == NULL)          
    start.expr = integer_one_node;
  else {        
    g95_conv_expr_type(&start, reference->u.ss.start, g95_default_integer);       
       
    g95_add_block_to_block(&se->pre, &start.pre);         
    g95_add_block_to_block(&se->post, &start.post);          
  }     
     
  start_minus_one = save_expr(fold(build(MINUS_EXPR, g95_default_integer,     
					 start.expr, integer_one_node)));   
   
  /* Deal with the end specification */         
         
  g95_init_se(&end, se); 
  type = TREE_TYPE(se->expr);

  if (reference->u.ss.end == NULL)    
    end.expr = se->string_length;     
  else {        
    g95_conv_expr_type(&end, reference->u.ss.end, g95_default_integer);   
   
    g95_add_block_to_block(&se->pre, &end.pre);     
    g95_add_block_to_block(&se->post, &end.post);
  }         
         
  /* string length = DIM(end, start-1).  We choose this form because
   * 'start' is likely to be one, in which case the subtraction
   * collapses to DIM(end, 0). */         
         
  g95_init_se(&length, se);   
  g95_dim(&length, end.expr, start_minus_one);          
          
  g95_add_block_to_block(&se->pre,  &length.pre);      
  g95_add_block_to_block(&se->post, &length.post);       
       
  se->string_length = length.expr;    
  se->expr = fold(build(PLUS_EXPR, type, se->expr, start_minus_one));    
}         
         
         
 
 
/* conv_component_ref()-- Convert a derived type component reference.  */       
       
static void conv_component_ref(g95_se *se, g95_ref *r) {  
tree tmp, decl, field;      
g95_component *b;        
        
  g95_reflevel(se, 0);  
  b = r->u.c.component; 
  assert(b->backend_decl); 
 
  field = b->backend_decl;         
  assert(TREE_CODE(field) == FIELD_DECL);          
  decl = se->expr;

  tmp = build(COMPONENT_REF, TREE_TYPE(field), decl, field);     
     
  if (b->pointer) {
    //se->expr = build1(INDIRECT_REF, TREE_TYPE(TREE_TYPE(tmp)), tmp);    
    se->expr = tmp;  
    return; 
  }        
        
  if (b->dimension) {
    component_array(se, b, tmp);  
    return;        
  }         
         
  if (b->ts.type == BT_CHARACTER)    
    se->string_length = G95_TYPE_STRING_LENGTH(TREE_TYPE(b->backend_decl)); 
 
  se->expr = tmp; 
}       
       
       
          
          
static void chain_multiply(g95_se *se, tree base, int power) {
int q, b, k, chain_length, negative, addition_chain[MAX_CHAIN_LEN];        
tree tmp, intermediate[MAX_CHAIN_LEN];         
         
  if (power > 0)         
    negative = 0;  
  else {        
    negative = 1;          
    power = -power;     
  }  
  
  chain_length = 0;
  for(q=power; q!=0; q=power_tree[q])      
    chain_length++;    
    
  if (chain_length > MAX_CHAIN_LEN)    
    g95_internal_error("chain_multiply(): Table too small");     
     
  b = 0;       
  for(q=power; q!=0; q=power_tree[q], b++)         
    addition_chain[chain_length-b-1] = q;       
       
  /* Now we have the chain.  Compute x and x^2 and store them to
   * temporaries. */    
    
  intermediate[0] = save_expr(base);    
  intermediate[1] = build(MULT_EXPR, TREE_TYPE(base),         
			  intermediate[0], intermediate[0]); 
 
  tmp = NULL_TREE;         
         
  /* Now traverse the chain.  If we're not at the end of the chain,
   * store the result to a temporary so that it can be re-used without
   * being recomputed.  The last element is the result of the whole
   * expression. */   
   
  for(q=2; q<chain_length; q++) {  
    for(b=0; b<q; b++)          
      for(k=0; k<q; k++)      
	if (addition_chain[b] + addition_chain[k] == addition_chain[q])       
	  goto found;    
    
    g95_internal_error("chain_multiply(): Corrupt addition chain for n=%d",    
		       power);        
  found:      
    tmp = build(MULT_EXPR, TREE_TYPE(base), intermediate[b], intermediate[k]);  
  
    if (q != chain_length-1) intermediate[q] = save_expr(tmp);      
  } 
 
  if (negative) tmp = reciprocal(tmp);     
  se->expr = tmp;    
}        
        
        
         
         
/* conv_unary_op()-- Convert unary operators .not. and minus */       
       
static void conv_unary_op(enum tree_code code, g95_se *se, g95_expr *expr) {         
g95_se operand;   
   
  g95_init_se(&operand, se);          
  g95_conv_expr(&operand, expr->op1);   
   
  g95_add_block_to_block(&se->pre, &operand.pre);  
  g95_add_block_to_block(&se->post, &operand.post);   
   
 /* TRUTH_NOT_EXPR is not a unary operator in GCC.  We must convert it
  * to a comparison with zero (e.g. EQ_EXPR (op1, 0)).  All other
  * unary operators have an equivalent SIMPLE unary operator  */         
         
  if (code == TRUTH_NOT_EXPR)         
    se->expr = build(EQ_EXPR, TREE_TYPE(operand.expr), operand.expr,    
		     integer_zero_node); 
  else
    se->expr = build1(code, TREE_TYPE(operand.expr), operand.expr); 
}


       
       
/* g95_stack_variable()-- Returns nonzero if a variable should
 * go on the stack. */          
          
int g95_stack_variable(tree size) {     
unsigned HOST_WIDE_INT low;     
     
  if (! INTEGER_CST_P (size))         
    return 0;        
        
  if (g95_option.max_stack_var_size < 0)  
    return 1;          
          
  if (TREE_INT_CST_HIGH (size) != 0)    
    return 0;       
       
  low = TREE_INT_CST_LOW (size);    
  if (low > (unsigned HOST_WIDE_INT) g95_option.max_stack_var_size)        
    return 0;   
   
/* TODO: Set a per-function stack size limit.  */          
#if 0
  /* We should be a bit more clever with array temps.  */         
  if (g95_option.max_function_vars_size >= 0)        
    {        
      if (low > g95_stack_space_left)          
        return 0;       
       
      g95_stack_space_left -= low;    
    }      
#endif
    
  return 1;   
} 
 
 
    
    
/* call_temp_free()-- Generate a call to the temp_free() library
 * function. */   
   
static void call_temp_free(stmtblock_t *block, tree var) { 
tree tmp, args;     
     
  var = build1(ADDR_EXPR, pvoid_type_node, var);    
  args = g95_chainon_list(NULL_TREE, var);  
  tmp = g95_build_function_call(library_temp_free, args);         
         
  g95_add_expr_to_block(block, tmp);       
       
  g95_add_modify_expr(block, var, null_pointer_node);        
}    
    
    
          
          
/* conv_variable()-- Return the contents of a variable. Also handles
 * reference/pointer variables (all Fortran pointer refrences are
 * implicit).  When processing a variable reference the rule is to do
 * as little as possible.  If an array reference is computed, a
 * pointer to the reference is computed. */      
      
static void conv_variable(g95_se *se, g95_expr *expr) {         
g95_typespec ts;       
g95_symbol *sym;          
g95_ref *r; 
 
  sym = expr->symbol;         
  se->expr = sym->backend_decl; 
 
  /* Procedure actual arguments. */ 
 
  if (expr->ts.type == BT_PROCEDURE && se->expr != current_function_decl) {  
    if (!expr->symbol->attr.dummy) {      
      assert(TREE_CODE(se->expr) == FUNCTION_DECL && DECL_EXTERNAL(se->expr));  
      se->expr = build1(ADDR_EXPR, build_pointer_type(TREE_TYPE(se->expr)),
			se->expr);         
    }        
        
    return;         
  }   
   
  if (sym->ts.type == BT_CHARACTER)          
    se->string_length = G95_TYPE_STRING_LENGTH(TREE_TYPE(sym->backend_decl));        
        
  r = expr->ref;    
  ts = sym->ts; 
 
  for(r=expr->ref; r; r=r->next)    
    switch(r->type) {          
    case REF_ARRAY:  
      g95_conv_array_ref(se, &r->u.ar, &ts);    
      break;   
   
    case REF_COMPONENT:  
      conv_component_ref(se, r);  
      ts = r->u.c.component->ts;          
      break;          
          
    case REF_SUBSTRING:    
      conv_substring(se, r);      
      break;      
      
    default:       
      abort();    
      break;         
    }        
}        
        
        
          
          
/* conv_function_expr()-- Translate a function call. */ 
 
static void conv_function_expr(g95_se *se, g95_expr *expr) {          
          
  if (expr->value.function.isym)    
    g95_conv_intrinsic_function(se, expr);       
  else  
    g95_conv_function_call(se, expr->symbol, expr->value.function.actual);       
}       
       
       
  
  
/* conv_constant_substring()-- This subroutine converts a substring of
 * a constant string. */          
          
static void conv_constant_substring(g95_se *se, g95_expr *expr) {     
     
  se->expr = g95_build_string_const(expr->value.character.length, 
				    expr->value.character.string); 
 
  conv_substring(se, expr->ref);      
}       
       
       
          
          
/* integer_exponent()-- Handle the case of constant integer exponent.
 * Returns nonzero if the exponent was such that we want to call the
 * library function, zero if we've taken care of everything. */  
  
static int integer_exponent(g95_se *se, tree base, g95_expr *exponent) {      
int v, *ip, power;   
   
  power = mpz_get_si(exponent->value.integer); 
 
  /* Special cases of the special case */          
          
  switch(power) {      
  case -2:        
    base = save_expr(base);   
    se->expr = reciprocal(build(MULT_EXPR, TREE_TYPE(base), base, base));       
    return 0;   
   
  case -1: 
    se->expr = reciprocal(base);
    return 0;     
     
  case 0:          
    return 1;      /* Want the check for 0**0 */          
          
  case 1:  
    se->expr = base;    
    return 0;       
       
  case 2:     
    base = save_expr(base);  
    se->expr = build(MULT_EXPR, TREE_TYPE(base), base, base);    
    return 0;
  } 
 
  /* See if we have the information on how to do this with the minimum
   * multiplications. */     
     
  ip = power_tree + 2;
  for(v=2; v<abs(power); v++, ip++)  
    if (*ip == 0) return 1;   /* Hit the end of the table */  
  
  chain_multiply(se, base, power);         
  return 0;  
}   
   
   
         
         
/* conv_power_op()-- Handle the exponentiation operator. */       
       
static void conv_power_op(g95_se *se, g95_expr *expr) {      
tree fndecl, tmp, type;         
int use_library, kind;     
g95_se base, exp;  
  
  g95_init_se(&base, se);     
  g95_conv_expr(&base, expr->op1);

  g95_add_block_to_block(&se->pre, &base.pre);       
  g95_add_block_to_block(&se->post, &base.post);        
        
  use_library = 1;    
    
  if (expr->op2->ts.type == BT_INTEGER &&  
      expr->op2->type == EXPR_CONSTANT)         
    use_library = integer_exponent(se, base.expr, expr->op2);    
    
  if (use_library) {     
    g95_init_se(&exp, se);   
    g95_conv_expr(&exp, expr->op2);          
    g95_add_block_to_block(&se->pre, &exp.pre);  
    g95_add_block_to_block(&se->post, &exp.post);  
  
    type = TREE_TYPE(base.expr);      
      
    kind = expr->op1->ts.kind;    
    switch(expr->op2->ts.type) {          
    case BT_INTEGER:         
         
      switch(expr->op1->ts.type) {   
      case BT_INTEGER:        
	fndecl = library_integer_4_power;     
	break;          
          
      case BT_REAL:        
	switch(expr->op1->ts.kind) {      
	case 4: fndecl = library_real_4_power; break;  
	case 8: fndecl = library_real_8_power; break;      
	default:
	  g95_internal_error("Unknown real kind for ** base");    
	}          
	            
	break;   
   
      case BT_COMPLEX:     
	g95_todo_error("Complex powers");    
    
      default:   
	g95_internal_error("conv_power_op(): Bad base type"); 
      }        
        
      break;      
      
    case BT_REAL:
      switch (kind) {  
      case 4: fndecl = gfor_fndecl_math_powf; break;    
      case 8: fndecl = gfor_fndecl_math_pow;  break;        
      default: abort();       
      }
      break;    
    
    case BT_COMPLEX:     
      switch (kind) {  
      case 4: fndecl = gfor_fndecl_math_cpowf; break;       
      case 8: fndecl = gfor_fndecl_math_cpow;  break;    
      default: abort();
      }       
      break; 
 
    default:        
      abort();  
      break;       
    }  
  
    tmp = g95_chainon_list(NULL_TREE, base.expr); 
    tmp = g95_chainon_list(tmp, exp.expr);         
    se->expr = g95_build_function_call(fndecl, tmp);  
  }          
}  
  
  
       
       
/* call_temp_alloc()-- Calls the temp_alloc() library function to get
 * some memory from the heap.  Returns the tree representing the
 * function call. */          
          
static void call_temp_alloc(stmtblock_t *block, tree var, tree length) {          
tree tmp, args;          
          
  args = g95_chainon_list(NULL_TREE, length);    
  tmp = g95_build_function_call(library_temp_alloc, args);        
        
  g95_add_modify_expr(block, var, tmp);        
}          
          
          
   
   
/* g95_conv_function_call()-- Generate code for a function call.  For
 * functions that return simple types, this is just generating the
 * call and adding it to the current expression.  For more complicated
 * types we create a temporary return value and pass a pointer to it
 * in the argument list.  The function call is then added to the
 * pre-chain and the result is the temporary. */ 
 
void g95_conv_function_call(g95_se *se, g95_symbol *symbol,     
			    g95_actual_arglist *arg) {          
tree arglist, tmp, var, length, return_var, pre_args, type, result_type;      
      
  arglist = g95_trans_arglist(arg, se); 
 
  return_var = NULL_TREE;   
  pre_args   = NULL_TREE;          
  length     = NULL_TREE;
  var        = NULL_TREE;        
        
  result_type = g95_result_type(symbol);  
  
  if (symbol->result->as != NULL)       
    g95_internal_error("Array return");         
  else      
    switch(symbol->ts.type) {   
    case BT_CHARACTER:        
      tmp = TREE_TYPE(TREE_TYPE(symbol->result->backend_decl)); 
      length = G95_TYPE_STRING_LENGTH(tmp);  
      return_var = g95_temp_string(se, length);      
      
      pre_args = g95_chainon_list(pre_args, return_var);      
      pre_args = g95_chainon_list(pre_args, length);     
      break;  
  
    case BT_DERIVED:      
    case BT_COMPLEX:   
      type = g95_typenode_for_spec(&symbol->result->ts);    
      return_var = g95_create_var(type, NULL);  
      TREE_ADDRESSABLE(return_var) = 1;      
      
      type = build_pointer_type(type);          
      tmp = build1(ADDR_EXPR, type, return_var); 
 
      pre_args = g95_chainon_list(pre_args, tmp);       
      break;       
       
    default:       
      break; 
    }       
       
  if (pre_args != NULL_TREE) arglist = chainon(pre_args, arglist);    
    
  if (symbol->attr.dummy) 
    tmp = symbol->backend_decl;      /* Function pointer */ 
  else {       
    type = build_pointer_type(TREE_TYPE(symbol->backend_decl));         
    tmp = build1(ADDR_EXPR, type, symbol->backend_decl);      
  }      
      
  tmp = build(CALL_EXPR, result_type, tmp, arglist);       
  TREE_SIDE_EFFECTS(tmp) = 1;      
      
  if (result_type != void_type_node)  
    se->expr = tmp;   
  else {      
    g95_add_expr_to_block(&se->pre, tmp);  
    se->expr = return_var;     
     
    if (length != NULL_TREE) se->string_length = length;     
  }      
} 
 
 
          
          
tree g95_trans_pointer_assign(g95_code *code) {     
stmtblock_t block;     
g95_se left, right;     
     
  g95_start_block(&block);          
          
  g95_init_se(&left, NULL);    
  left.reflevel = 1; 
  g95_conv_expr(&left, code->expr);  
  
  g95_init_se(&right, NULL);     
  right.reflevel = 1;          
  g95_conv_expr(&right, code->expr2);       
       
  g95_add_block_to_block(&block, &left.pre);       
  g95_add_block_to_block(&block, &right.pre);   
   
  g95_add_modify_expr(&block, left.expr, right.expr);        
        
  g95_add_block_to_block(&block, &left.post);
  g95_add_block_to_block(&block, &right.post);          
          
  return g95_finish_block(&block);        
} 
 
 
       
       
/* conv_expr_op()-- Translate an intrinsic operator expression.  Most
 * binary operators are handled by this function, others are passed
 * on.  Recursion is used in either case.  We are guaranteed that
 * typespecs are the same for the operands of binary operators except
 * exponentiation.  Character strings get special handling.  */        
        
static void conv_expr_op(g95_se *se, g95_expr *expr) {   
int logical, checkstring;    
enum tree_code code;     
g95_se left, right;    
tree type, tmp;          
          
  checkstring = 0;         
  logical = 0;   
   
  switch(expr->operator) {    
  case INTRINSIC_UPLUS:        
    g95_conv_expr(se, expr->op1);   
    return;       
       
  case INTRINSIC_UMINUS:
    conv_unary_op(NEGATE_EXPR, se, expr);
    return;       
       
  case INTRINSIC_NOT:  
    conv_unary_op(TRUTH_NOT_EXPR, se, expr);       
    return;     
     
  case INTRINSIC_PLUS:       
    code = PLUS_EXPR;       
    break;        
        
  case INTRINSIC_MINUS:    
    code = MINUS_EXPR;    
    break;     
     
  case INTRINSIC_TIMES:   
    code = MULT_EXPR;  
    break;    
    
  case INTRINSIC_DIVIDE: 
    /* If the expression is real or complex, use an RDIV_EXPR. If
     * integer, we must round towards zero, so we use a TRUNC_DIV_EXPR. */        
        
    code = (expr->ts.type == BT_INTEGER) ? TRUNC_DIV_EXPR : RDIV_EXPR;         
    break;

  case INTRINSIC_POWER:        
    conv_power_op(se, expr);
    return;   
   
  case INTRINSIC_CONCAT:   
    conv_concat_op(se, expr);  
    return;        
        
  case INTRINSIC_AND:    
    code = BIT_AND_EXPR;        
    logical = 1;   
    break;         
         
  case INTRINSIC_OR:          
    code = BIT_IOR_EXPR;          
    logical = 1;
    break;

    /* EQV and NEQV only work on logicals, but since we represent them
     * as integers, we can use EQ_EXPR and NE_EXPR for them.  */          
          
  case INTRINSIC_EQ:        
  case INTRINSIC_EQV:    
    code = EQ_EXPR;   
    checkstring = 1;  
    logical = 1;
    break;   
   
  case INTRINSIC_NE:         
  case INTRINSIC_NEQV:          
    code = NE_EXPR;         
    checkstring = 1;          
    logical = 1;  
    break;      
      
  case INTRINSIC_GT:       
    code = GT_EXPR;    
    checkstring = 1;        
    logical = 1;  
    break;    
    
  case INTRINSIC_GE:   
    code = GE_EXPR;     
    checkstring = 1;          
    logical = 1;  
    break;       
       
  case INTRINSIC_LT:
    code = LT_EXPR;      
    checkstring = 1;       
    logical = 1; 
    break;          
          
  case INTRINSIC_LE:   
    code = LE_EXPR;     
    checkstring = 1;
    logical = 1;  
    break;        
        
  case INTRINSIC_USER:  
  case INTRINSIC_ASSIGN:         
    g95_internal_error("conv_expr_op(): User operator still exists");    
    return; 
 
  default:        
    g95_internal_error("conv_expr_op(): Unknown intrinsic op");  
    return;      
  }    
    
  /* The only exception to this is **, which is handled seperately anyway.  */    
    
  assert(expr->op1->ts.type == expr->op2->ts.type);    
    
  if (checkstring && expr->op1->ts.type != BT_CHARACTER) checkstring = 0;          
          
  g95_init_se(&left, se);        
  g95_init_se(&right, se);   
   
  if (checkstring) {     
    left.reflevel = 1;          
    right.reflevel = 1;         
  } 
 
  g95_conv_expr(&left, expr->op1);   
  g95_add_block_to_block(&se->pre, &left.pre);        
        
  g95_conv_expr(&right, expr->op2);     
  g95_add_block_to_block(&se->pre, &right.pre);      
      
  /* For string comparisons we generate a library call, and compare
   * the return value with zero. */     
     
  if (checkstring) { 
    tmp = g95_call_library(g95_default_integer, PREFIX "compare_string",      
			   left.expr,  left.string_length,  
			   right.expr, right.string_length, NULL_TREE);          
          
    left.expr = tmp;
    right.expr = integer_zero_node;     
  }      
      
  type = g95_typenode_for_spec(&expr->ts);    
    
  if (logical) {  /* The result of logical ops is always boolean_type_node.  */
    tmp = build(code, type, left.expr, right.expr);
    se->expr = convert(type, tmp);       
  } else          
    se->expr = build(code, type, left.expr, right.expr);  
  
  /* Add the post blocks.  */    
  g95_add_block_to_block(&se->post, &right.post);         
  g95_add_block_to_block(&se->post, &left.post);  
} 
 
 
     
     
/* g95_temp_string()-- Generate code to allocate a string temporary.
 * Returns a pointer to the storage area or a character pointer
 * variable. */     
     
tree g95_temp_string(g95_se *se, tree length) {          
tree var, tmp;

 if (g95_stack_variable(length)) {       
    tmp = build_range_type(g95_default_integer, integer_one_node, length);    
    tmp = build_array_type(g95_character1_type_node, tmp);    
    
    var = g95_create_var(tmp, NULL);     
    TREE_ADDRESSABLE(var) = 1;      
      
    var = build1(ADDR_EXPR, pchar_type_node, var);    
    
  } else {          
    var = g95_create_var(pchar_type_node, "tempstr");  
  
    call_temp_alloc(&se->pre, var, length);          
    call_temp_free(&se->post, var);         
  }     
     
  return var;      
}     
     
     
 
 
/* g95_conv_expr()-- Translate an expression. */   
   
void g95_conv_expr(g95_se *se, g95_expr *expr) {      
      
  switch(expr->type) {       
  case EXPR_OP:   
    conv_expr_op(se, expr); 
    break;      
      
  case EXPR_FUNCTION:       
    conv_function_expr(se, expr);          
    break;  
  
  case EXPR_CONSTANT:          
    se->expr = g95_conv_constant(expr, NULL_TREE);
    if (expr->ts.type == BT_CHARACTER)        
      se->string_length = build_int_2(expr->value.character.length, 0);   
   
    break;         
         
  case EXPR_VARIABLE:          
    conv_variable(se, expr); 
    break;      
      
  case EXPR_NULL:  
    se->expr = null_pointer_node;   
    break;     
     
  case EXPR_SUBSTRING:          
    conv_constant_substring(se, expr);      
    break;  
  
  case EXPR_STRUCTURE:        
    g95_todo_error("EXPR_STRUCTURE");     
    break;     
     
  case EXPR_ARRAY: 
    g95_todo_error("Array constructors");    
    break; 
 
  default:
    abort();        
    break;     
  }   
   
  g95_reflevel(se, se->reflevel);     
}          
          
          
  
  
/* g95_conv_expr_type()-- Convert an expression to a particular
 * destination type. */ 
 
void g95_conv_expr_type(g95_se *se, g95_expr *expr, tree type) {    
    
  g95_conv_expr(se, expr);        
  se->expr = convert(type, se->expr);         
}        
        
        
