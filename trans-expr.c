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
          
          
static void ac_assign(stmtblock_t *, g95_constructor *, int);         
         
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
          
   
   
/* g95_init_se()-- Initialize a simple expression holder.

   Care must be taken when multiple se are created with the same parent.
   The child se must be kept in sync.  The easiest way is to delay creation
   of a child se until after after the previous se has been translated.  */    
    
void g95_init_se(g95_se *s, g95_se *parent) {     
     
  memset(s, 0, sizeof(g95_se));        
  g95_init_block(&s->pre);     
  g95_init_block(&s->post);    
    
  s->parent = parent;       
}         
         
         


/* g95_reflevel()-- Given something that is an object, a pointer to an
 * object or a pointer to a pointer to an object and a reference
 * level, generate code to bring the object to the desired level.
 * Level zero is a reference to some object, one is a pointer to an
 * object and two is a pointer to a pointer to the object. */ 
 
void g95_reflevel(g95_se *se1, int lvl) {  
tree tmp0, typ, new_type, object;          
          
  object = se1->expr;        
  typ = TREE_TYPE(object);       
       
  if (!POINTER_TYPE_P(typ)) {  /* Object is not a pointer */          
    switch(lvl) {         
    case 0:          
      break;  
  
    case 1:   
      if (CONSTANT_P(object) || TREE_CODE(object) == CALL_EXPR ||     
	  TREE_CODE(object) == NOP_EXPR ||    
	  IS_EXPR_CODE_CLASS(TREE_CODE_CLASS(TREE_CODE(object)))) {

	tmp0 = g95_create_var(typ);      
	g95_add_modify_expr(&se1->pre, tmp0, object);          
	object = tmp0;      
      }        
        
      se1->expr = build1(ADDR_EXPR, build_pointer_type(typ), object);
      break;

    case 2:          
      /* Can't think of a case where this is needed */      
      g95_internal_error("g95_reflevel(): Bad case");       
    }      
      
    return;        
  }  
  
  new_type = TREE_TYPE(typ);
  if (!POINTER_TYPE_P(new_type)) {  /* Pointer to an object */
    switch(lvl) {       
    case 0:  
      se1->expr = build1(INDIRECT_REF, new_type, object);         
      break;     
     
    case 1:      
      break;

    case 2:    
      se1->expr = build1(ADDR_EXPR, build_pointer_type(typ), object);   
      break;       
    } 
 
    return;     
  }         
         
  new_type = TREE_TYPE(new_type);         
  if (!POINTER_TYPE_P(new_type)) {  /* Pointer to a pointer to an object */         
    switch(lvl) {    
    case 0:   
      typ = build_pointer_type(new_type);          
      object = build1(INDIRECT_REF, typ, object);          
          
      se1->expr = build1(INDIRECT_REF, new_type, object);     
      break;       
       
    case 1: 
      se1->expr = build1(INDIRECT_REF, build_pointer_type(new_type), object);   
      break;

    case 2:         
      break;     
    }

    return;       
  }

  g95_internal_error("g95_reflevel(): Bad type");          
}      
      
      
  
  
/* g95_conv_char_length()-- Convert a character length parameter.
 * This is special in that negative lengths must be clamped to
 * zero.  The length must not be assumed. */          
          
tree g95_conv_char_length(g95_se *se, g95_typespec *typesp) { 
tree leng, tmp1;         
g95_se exp;    
    
  g95_init_se(&exp, NULL);          
          
  g95_conv_expr(&exp, typesp->cl->length);    
  g95_add_block_to_block(&se->pre, &exp.pre);          
  g95_add_block_to_block(&se->post, &exp.post);       
       
  leng = save_expr(exp.expr);         
         
  tmp1 = fold(build(LT_EXPR, boolean_type_node, leng, integer_zero_node));  
  leng = fold(build(COND_EXPR, g95_default_integer, tmp1, integer_zero_node,  
		      leng));     
     
  return leng;
}      
      
      
      
          
          
/* conv_substring()-- Converts a string into a substring.  The
 * standard specifies that the start and end indexes be within the
 * range of the string.  This implementation doesn't check for
 * violations of this condition, allowing access to memory outside of
 * the string.  To fix this, start and end would need to be clamped to
 * the legal ranges. */

static void conv_substring(g95_se *se1, g95_ref *re) {   
tree start_minus_one, t;       
g95_se st, fin, length;          
          
  g95_reflevel(se1, 1);
  g95_init_se(&st, se1);     
     
  if (re->u.ss.start == NULL)   
    st.expr = integer_one_node;
  else {       
    g95_conv_expr_type(&st, re->u.ss.start, g95_default_integer);

    g95_add_block_to_block(&se1->pre, &st.pre);   
    g95_add_block_to_block(&se1->post, &st.post); 
  }

  start_minus_one = save_expr(fold(build(MINUS_EXPR, g95_default_integer,    
					 st.expr, integer_one_node)));

  /* Deal with the end specification */        
        
  g95_init_se(&fin, se1);          
  t = TREE_TYPE(se1->expr);          
          
  if (re->u.ss.end == NULL)
    fin.expr = se1->string_length;       
  else {  
    g95_conv_expr_type(&fin, re->u.ss.end, g95_default_integer);          
          
    g95_add_block_to_block(&se1->pre, &fin.pre);        
    g95_add_block_to_block(&se1->post, &fin.post);        
  }          
          
  /* string length = DIM(end, start-1).  We choose this form because
   * 'start' is likely to be one, in which case the subtraction
   * collapses to DIM(end, 0). */ 
 
  g95_init_se(&length, se1);   
  g95_dim(&length, fin.expr, start_minus_one);          
          
  g95_add_block_to_block(&se1->pre,  &length.pre);       
  g95_add_block_to_block(&se1->post, &length.post);    
    
  se1->string_length = length.expr;      
  se1->expr = fold(build(PLUS_EXPR, t, se1->expr, start_minus_one));         
}     
     
     
    
    
/* g95_conv_expr_type()-- Convert an expression to a particular
 * destination type. */ 
 
void g95_conv_expr_type(g95_se *se0, g95_expr *exp, tree dtype) {         
         
  g95_conv_expr(se0, exp);          
  se0->expr = convert(dtype, se0->expr);         
}          
          
          
         
         
/* trans_constructor_assign()-- Translate the assignment of an array
 * constructor to an array. */          
          
static tree trans_constructor_assign(g95_expr *expr1, g95_expr *expr2) {        
stmtblock_t block;       
tree size, tmp0;          
g95_se s;     
     
  g95_init_block(&block);       
  g95_init_se(&s, NULL);    
  s.reflevel = 1;

  g95_conv_expr(&s, expr1);  
  
  g95_add_block_to_block(&block, &s.pre);    
    
  size = size_in_bytes(g95_typenode_for_spec(&expr1->ts));          
  tmp0 = expr1->symbol->attr.artificial ? integer_one_node : integer_zero_node;       
       
  tmp0 = g95_call_library(void_type_node, PREFIX "start_ac_assign", s.expr,      
			 tmp0, size, NULL);      
      
  g95_add_expr_to_block(&block, tmp0);      
  g95_add_block_to_block(&block, &s.post);     
     
/* Traverse the constructor, calculating each element.  Elements here
 * can be general expressions. */   
   
  ac_assign(&block, expr2->value.constructor, expr1->symbol->attr.artificial);     
     
  return g95_finish_block(&block);
} 
 
 
 
 
/* g95_conv_expr0()-- Convert an expression without having to worry
 * about the parent se. */         
         
void g95_conv_expr0(g95_se *se1, g95_expr *exp) {      
g95_se s; 
 
  g95_init_se(&s, NULL); 
  g95_conv_expr(&s, exp);      
      
  se1->expr = s.expr;  
  
  g95_add_block_to_block(&se1->pre, &s.pre);   
  g95_add_block_to_block(&se1->post, &s.post);    
}          
          
          
       
       
/* g95_trans_assignment()-- Translate an assignment statement */      
      
tree g95_trans_assignment(g95_expr *expr1, g95_expr *expr2) {          
stmtblock_t list;     
g95_se lse, rse;   
tree tmp0;      
      
  if (expr1->rank > 0 || expr2->rank > 0) {        
    switch(expr2->type) {          
    case EXPR_ARRAY:  
      return trans_constructor_assign(expr1, expr2);          
          
    case EXPR_FUNCTION:
      break;    
    
    default:       
      g95_internal_error("Unexpected array assignment statement at %L", 
			 &expr1->where);      
    }       
  } 
 
  g95_init_block(&list);       
       
  g95_init_se(&lse, NULL);     
  g95_init_se(&rse, NULL);          
          
  if (expr1->ts.type == BT_CHARACTER || expr1->rank > 0) {         
    lse.reflevel = 1; 
    rse.reflevel = 1;         
  }       
       
  g95_conv_expr(&lse, expr1);   
  g95_conv_expr(&rse, expr2);  
  
  g95_add_block_to_block(&list, &lse.pre);      
  g95_add_block_to_block(&list, &rse.pre);         
         
  if (expr1->ts.type != BT_CHARACTER || expr1->rank > 0)      
    g95_add_modify_expr(&list, lse.expr, rse.expr);       
  else {        
    tmp0 = g95_call_library(void_type_node, PREFIX "copy_string",       
			   lse.expr, lse.string_length,       
			   rse.expr, rse.string_length, NULL_TREE);      
      
    g95_add_expr_to_block(&list, tmp0);          
  }   
   
  g95_add_block_to_block(&list, &lse.post);   
  g95_add_block_to_block(&list, &rse.post);    
    
  return g95_finish_block(&list);    
}         
 
 
/* conv_constant_substring()-- This subroutine converts a substring of
 * a constant string. */ 
 
static void conv_constant_substring(g95_se *s, g95_expr *exp) { 
int l; 
 
  l = exp->value.character.length;    
  s->expr = g95_build_string_const(l, exp->value.character.string);      
  s->string_length = build_int_2(l, 0);          
          
  conv_substring(s, exp->ref);         
}       
       
       
    
    
/* component_array()-- Create a descriptor for the array.  TODO: if
 * the next reference is for an array element, then we can merge those
 * two steps and avoid initializing a descriptor that is only used
 * once.  Otherwise we are probably passing the array section as an
 * actual parameter and need the descriptor. */          
          
static void component_array(g95_se *se0, g95_component *v, tree storage) {       
variable_info vin;
tree desc;

  g95_component_vinfo(v, &vin);  
  desc = g95_create_var(g95_get_descriptor(&vin));   
  g95_temp_array_descriptor(se0, &vin, desc, storage);

  se0->expr = desc;          
}      
      
      
   
   
/* g95_trans_arglist()-- Translate an actual argument list into tree
 * form.  Alternate return elements are ignored. */      
      
tree g95_trans_arglist(g95_actual_arglist *args, g95_se *se) {          
tree alist, arglist_tail;        
g95_se parmse;          
          
  alist = NULL_TREE;  
  arglist_tail = NULL_TREE;

  for(; args!=NULL; args=args->next) {          
    g95_init_se(&parmse, NULL);          
          
    switch(args->type) {  
    case ALT_RETURN:      
      continue;      
      
    case EXPR:    
      if (args->u.expr == NULL)          
	parmse.expr = null_pointer_node;  
      else {          
	parmse.reflevel = args->pointer ? 2 : 1;    
	g95_conv_expr(&parmse, args->u.expr);    
      }    
      break;  
  
    case ARRAY_DESC:      
    case ARRAY_ELEMENT:
    case FULL_ARRAY:  
      g95_array_argument(&parmse, args);        
      break;         
    }

    g95_add_block_to_block(&se->pre,  &parmse.pre);
    g95_add_block_to_block(&se->post, &parmse.post);   
   
    alist = g95_chainon_list(alist, parmse.expr);         
         
    if (args->u.expr == NULL) {    
      if (args->missing_arg_type == BT_CHARACTER)          
	arglist_tail = g95_chainon_list(arglist_tail, integer_zero_node);     
    } else {    
      if (args->u.expr->ts.type == BT_CHARACTER)  
	arglist_tail = g95_chainon_list(arglist_tail, parmse.string_length);          
    }   
  }    
    
  if (arglist_tail != NULL_TREE) alist = chainon(alist, arglist_tail);      
      
  return alist;     
}    
    
    
        
        
/* g95_stack_variable()-- Returns nonzero if a variable of the given
 * type should go on the stack.  This is used mainly for arrays.  When
 * the current frame size is exceeded, we start spilling things to the
 * heap. */          
          
int g95_stack_variable(tree typ) {  
int s;          
          
  s = int_size_in_bytes(typ);         
  if (s == -1) return 0;

  if (g95_context->frame_size + s > g95_option.max_frame_size) return 0;  
  
  g95_context->frame_size += s;    
    
  return 1;       
}   
   
   
        
        
/* conv_component_ref()-- Convert a derived type component reference.  */       
       
static void conv_component_ref(g95_se *s, g95_ref *re) {  
tree tmp, dec, field;         
g95_component *q;          
          
  g95_reflevel(s, 0); 
  q = re->u.c.component;       
  assert(q->backend_decl);  
  
  field = q->backend_decl;        
  assert(TREE_CODE(field) == FIELD_DECL);     
  dec = s->expr;      
      
  tmp = build(COMPONENT_REF, TREE_TYPE(field), dec, field);        
        
  if (q->pointer) { 
    //s->expr = build1(INDIRECT_REF, TREE_TYPE(TREE_TYPE(tmp)), tmp);    
    s->expr = tmp;       
    return;         
  }        
        
  if (q->dimension) {    
    component_array(s, q, tmp);  
    return;      
  }    
    
  if (q->ts.type == BT_CHARACTER) s->string_length = q->ts.cl->backend_decl;       
       
  s->expr = tmp;        
}          
          
          
    
    
/* conv_unary_op()-- Convert unary operators .not. and minus */  
  
static void conv_unary_op(enum tree_code c, g95_se *se, g95_expr *e1) {     
g95_se operand;

  g95_init_se(&operand, se);  
  g95_conv_expr(&operand, e1->op1);      
      
  g95_add_block_to_block(&se->pre, &operand.pre); 
  g95_add_block_to_block(&se->post, &operand.post);

 /* TRUTH_NOT_EXPR is not a unary operator in GCC.  We must convert it
  * to a comparison with zero (e.g. EQ_EXPR (op1, 0)).  All other
  * unary operators have an equivalent SIMPLE unary operator  */    
    
  if (c == TRUTH_NOT_EXPR)       
    se->expr = build(EQ_EXPR, TREE_TYPE(operand.expr), operand.expr,          
		     integer_zero_node);    
  else  
    se->expr = build1(c, TREE_TYPE(operand.expr), operand.expr);          
}      
      
      
 
 
/* conv_function_expr()-- Translate a function call. */      
      
static void conv_function_expr(g95_se *s, g95_expr *exp) {

  if (exp->value.function.isym)
    g95_conv_intrinsic_function(s, exp);      
  else    
    g95_conv_function_call(s, exp->symbol, exp->value.function.actual);       
}    
    
    
/* constant_structure()-- Create a constant derived type constuctor. */          
          
static void conv_structure(g95_se *s, g95_expr *q) {       
g95_constructor *cons;     
g95_component *comp;         
variable_info info;   
g95_expr *k;        
g95_se se2;    
tree declr;   
   
  declr = NULL_TREE;         
         
  cons = q->value.constructor;  
  comp = q->symbol->components;        
        
  while(cons != NULL) {    
    g95_init_se(&se2, NULL);      
      
    k = cons->expr; 
 
    if (comp->dimension) {  
      g95_component_vinfo(comp, &info);     
      info.value = k;      
      
      se2.expr = g95_conv_array_initializer(&info);     
    } else {     
      if (comp->pointer) se2.reflevel = 1;  
  
      g95_conv_expr(&se2, k);      
      
      if (k->ts.type == BT_CHARACTER && k->rank == 0)   
	se2.expr = g95_resize_string_constant(se2.expr,   
					      comp->ts.cl->backend_decl);         
    } 
 
    g95_add_block_to_block(&s->pre,  &se2.pre);       
    g95_add_block_to_block(&s->post, &se2.post);          
          
    declr = tree_cons(comp->backend_decl, se2.expr, declr);        
        
    cons = cons->next;          
    comp = comp->next;    
  }     
     
  s->expr = build(CONSTRUCTOR, q->symbol->backend_decl, NULL_TREE,       
		   nreverse(declr));  
} 
 
 
   
   
/* conv_variable()-- Return the contents of a variable. Also handles
 * reference/pointer variables (all Fortran pointer references are
 * implicit).  When processing a variable reference the rule is to do
 * as little as possible.  If an array reference is computed, a
 * pointer to the reference is computed. */

static void conv_variable(g95_se *se0, g95_expr *exp) {         
g95_typespec ts;
g95_symbol *sy;       
g95_ref *r;    
int as_flag;       
       
  sy = exp->symbol;  
  if (sy->backend_decl == NULL_TREE) g95_get_symbol_decl(sy);        
  se0->expr = sy->backend_decl; 
 
  /* Procedure actual arguments. */  
  
  if (exp->ts.type == BT_PROCEDURE && se0->expr != current_function_decl) {
    if (!exp->symbol->attr.dummy) {  
      assert(TREE_CODE(se0->expr) == FUNCTION_DECL);    
      se0->expr = build1(ADDR_EXPR, build_pointer_type(TREE_TYPE(se0->expr)),         
			se0->expr);      
    }      
      
    return;         
  }       
       
  if (sy->ts.type == BT_CHARACTER)        
    se0->string_length = sy->ts.cl->backend_decl;   
   
  r = exp->ref;       
  ts = sy->ts;       
       
  as_flag = (sy->as == NULL) ? 0 : sy->as->type == AS_ASSUMED_SIZE;       
       
  for(r=exp->ref; r; r=r->next) {        
    switch(r->type) {     
    case REF_ARRAY:   
      g95_conv_array_ref(se0, &r->u.ar, &ts, as_flag);    
      break;   
   
    case REF_COMPONENT:  
      conv_component_ref(se0, r);    
      ts = r->u.c.component->ts;    
    
      if (ts.cl != NULL) se0->string_length = ts.cl->backend_decl;         
      break;     
     
    case REF_SUBSTRING:       
      conv_substring(se0, r);     
      break; 
 
    default:   
      abort();          
      break;         
    }     
     
    as_flag = 0;
  }
}


    
    
/* reciprocal()-- Given a tree, return the reciprocal. */         
         
static tree reciprocal(tree h) {          
tree tmp;  
  
  if (TREE_TYPE(h) == TREE_TYPE(integer_one_node))
    tmp = build(TRUNC_DIV_EXPR, TREE_TYPE(h), integer_one_node, h);      
  else {         
    tmp = g95_build_const(TREE_TYPE(h), integer_one_node);  
    tmp = build(RDIV_EXPR, TREE_TYPE(h), tmp, h);  
  }  
  
  return tmp; 
} 
 
 


static void chain_multiply(g95_se *se, tree start, int power) {
int y, b, n, chain_length, negative, addition_chain[MAX_CHAIN_LEN];     
tree tmp0, intermediate[MAX_CHAIN_LEN];          
          
  if (power > 0)     
    negative = 0;   
  else {         
    negative = 1;       
    power = -power;   
  }        
        
  chain_length = 0;    
  for(y=power; y!=0; y=power_tree[y])    
    chain_length++;        
        
  if (chain_length > MAX_CHAIN_LEN)        
    g95_internal_error("chain_multiply(): Table too small");       
       
  b = 0;       
  for(y=power; y!=0; y=power_tree[y], b++)       
    addition_chain[chain_length-b-1] = y; 
 
  /* Now we have the chain.  Compute x and x^2 and store them to
   * temporaries. */      
      
  intermediate[0] = save_expr(start); 
  intermediate[1] = build(MULT_EXPR, TREE_TYPE(start),      
			  intermediate[0], intermediate[0]);       
       
  tmp0 = NULL_TREE;

  /* Now traverse the chain.  If we're not at the end of the chain,
   * store the result to a temporary so that it can be re-used without
   * being recomputed.  The last element is the result of the whole
   * expression. */   
   
  for(y=2; y<chain_length; y++) {    
    for(b=0; b<y; b++)    
      for(n=0; n<y; n++) 
	if (addition_chain[b] + addition_chain[n] == addition_chain[y])       
	  goto found;      
      
    g95_internal_error("chain_multiply(): Corrupt addition chain for n=%d",      
		       power);     
  found:        
    tmp0 = build(MULT_EXPR, TREE_TYPE(start), intermediate[b], intermediate[n]);       
       
    if (y != chain_length-1) intermediate[y] = save_expr(tmp0);        
  }     
     
  if (negative) tmp0 = reciprocal(tmp0);         
  se->expr = tmp0;        
}        
        
        


/* integer_exponent()-- Handle the case of constant integer exponent.
 * Returns nonzero if the exponent was such that we want to call the
 * library function, zero if we've taken care of everything. */

static int integer_exponent(g95_se *se1, tree start, g95_expr *exponent) {        
int s, *ifp, power; 
 
  power = mpz_get_si(exponent->value.integer);         
         
  /* Special cases of the special case */

  switch(power) {         
  case -2:     
    start = save_expr(start);      
    se1->expr = reciprocal(build(MULT_EXPR, TREE_TYPE(start), start, start));    
    return 0;  
  
  case -1:      
    se1->expr = reciprocal(start); 
    return 0;        
        
  case 0:  
    return 1;      /* Want the check for 0**0 */     
     
  case 1:    
    se1->expr = start;      
    return 0;  
  
  case 2:        
    start = save_expr(start); 
    se1->expr = build(MULT_EXPR, TREE_TYPE(start), start, start); 
    return 0;       
  } 
 
  /* See if we have the information on how to do this with the minimum
   * multiplications. */     
     
  ifp = power_tree + 2;         
  for(s=2; s<abs(power); s++, ifp++)          
    if (*ifp == 0) return 1;   /* Hit the end of the table */        
        
  chain_multiply(se1, start, power);         
  return 0; 
}  
  
  
      
      
/* g95_conv_function_call()-- Generate code for a function call.  For
 * functions that return simple types, this is just generating the
 * call and adding it to the current expression.  For more complicated
 * types we create a temporary return value and pass a pointer to it
 * in the argument list.  The function call is then added to the
 * pre-chain and the result is the temporary. */       
       
void g95_conv_function_call(g95_se *se, g95_symbol *sy,  
			    g95_actual_arglist *arg) {        
tree a, tmp1, var, l, pre_args, t, result_type;  
g95_symbol *res;       
int char_flag;      
g95_expr *leng;      
g95_se s;   
   
  a = g95_trans_arglist(arg, se);     
     
  char_flag  = 0;        
  pre_args   = NULL_TREE;      
  l     = NULL_TREE;   
  var        = NULL_TREE;     
     
  res = sy->result;         
  result_type = g95_result_type(sy);         
         
  if (!res->attr.pointer && res->as == NULL)    
    switch(res->ts.type) {         
    case BT_CHARACTER:     
      char_flag = 1;         
      l = g95_create_var(g95_default_integer);         
      TREE_ADDRESSABLE(l) = 1;       
       
      /* If the local declaration is something more complicated than a
       * constant, then we have a problem because the length can
       * involve dummy variables of the function being called.  The
       * length needs to be computed in the caller for assumed-length
       * functions.  For fortran 77, these all have to be constant, so
       * this code is mainly for legacy applications. */          
          
      leng = res->ts.cl->length;     
      if (leng->type != EXPR_CONSTANT)  
	tmp1 = integer_zero_node; 
      else {  
	g95_conv_constant(&s, leng);      
	tmp1 = s.expr;          
      }      
      
      g95_add_modify_expr(&se->pre, l, tmp1);         
         
      t = build_pointer_type(g95_default_integer);          
      tmp1 = build1(ADDR_EXPR, t, l);

      pre_args = g95_chainon_list(pre_args, tmp1);
      var = g95_create_var(pchar_type_node);     
     
      g95_call_temp_free(&se->post, var);         
         
      se->string_length = l;      
      break; 
 
    case BT_DERIVED:    
    case BT_COMPLEX:
      t = g95_typenode_for_spec(&res->ts); 
      var = g95_create_var(t);       
      TREE_ADDRESSABLE(var) = 1;         
         
      t = build_pointer_type(t);  
      tmp1 = build1(ADDR_EXPR, t, var);        
        
      pre_args = g95_chainon_list(pre_args, tmp1);      
      break;   
   
    default:          
      break; 
    } 
 
  /* Character returns for pointer valued or array valued functions
   * cannot be assumed length, so the length is calculated locally. */        
        
  if (res->ts.type == BT_CHARACTER &&
      (res->attr.pointer || res->as != NULL))         
    se->string_length = res->ts.cl->backend_decl;        
        
  if (pre_args != NULL_TREE) a = chainon(pre_args, a);         
         
  if (sy->attr.dummy)  
    tmp1 = sy->backend_decl;      /* Function pointer */       
  else {     
    t = build_pointer_type(TREE_TYPE(sy->backend_decl));         
    tmp1 = build1(ADDR_EXPR, t, sy->backend_decl);  
  }         
         
  tmp1 = build(CALL_EXPR, result_type, tmp1, a);    
  TREE_SIDE_EFFECTS(tmp1) = 1;   
   
  if (char_flag) { 
    g95_add_modify_expr(&se->pre, var, tmp1);        
    se->expr = var;
  } else if (var == NULL)
    se->expr = tmp1;     
  else {    
    g95_add_expr_to_block(&se->pre, tmp1);      
    se->expr = var;   
  }  
}    
    
    
 
 
/* conv_concat_op()-- Handle a string concatenation operation.  A
 * temporary is allocated to hold the result */      
      
static void conv_concat_op(g95_se *se1, g95_expr *expr) { 
g95_se left, right;     
tree var0, tmp1;    
    
  g95_init_se(&left, se1);          
  left.reflevel = 1;    
  g95_conv_expr(&left, expr->op1);      
      
  g95_add_block_to_block(&se1->pre, &left.pre);  
  g95_add_block_to_block(&se1->post, &left.post);        
        
  g95_init_se(&right, se1); 
  right.reflevel = 1;         
  g95_conv_expr(&right, expr->op2);      
      
  g95_add_block_to_block(&se1->pre, &right.pre); 
  g95_add_block_to_block(&se1->post, &right.post);      
      
  se1->string_length = fold(build(PLUS_EXPR, g95_default_integer, 
				 left.string_length, right.string_length));  
  
  se1->string_length = save_expr(se1->string_length);          
          
  var0 = g95_temp_string(se1, se1->string_length);        
        
  tmp1 = g95_call_library(void_type_node, PREFIX "concat_string", var0,         
			 left.expr,  left.string_length,    
			 right.expr, right.string_length, NULL_TREE);  
  
  g95_add_expr_to_block(&se1->pre, tmp1);

  se1->expr = var0;         
}  
  
  
 
 
/* ac_assign_expr()-- Assign a scalar expression within an array
 * constructor to an array */       
       
static void ac_assign_expr(stmtblock_t *body, g95_expr *t, int dynamic) {  
tree tmp, l;
g95_se s;         
         
  if (t->type == EXPR_ARRAY) {  
    ac_assign(body, t->value.constructor, dynamic);  
    return;     
  }   
   
  g95_init_se(&s, NULL);     
  s.reflevel = 1;       
  g95_conv_expr(&s, t);     
     
  g95_add_block_to_block(body, &s.pre);

  l = (s.string_length == NULL) 
    ? integer_minus_one_node         
    : s.string_length;        
        
  tmp = g95_call_library(void_type_node,
			 dynamic ? PREFIX "ac_assign2" : PREFIX "ac_assign1", 
			 s.expr, l, NULL);  
  g95_add_expr_to_block(body, tmp);          
          
  g95_add_block_to_block(body, &s.post);        
}          
          
          
 
 
/* g95_temp_string()-- Generate code to allocate a string temporary.
 * Returns a pointer to the storage area or a character pointer
 * variable. */  
  
tree g95_temp_string(g95_se *expr, tree len) {   
tree v, dtype;

  dtype = build_range_type(g95_default_integer, integer_one_node, len);      
  dtype = build_array_type(g95_character1_type_node, dtype);       
       
  if (g95_stack_variable(dtype)) { 
    v = g95_create_var(dtype);    
    TREE_ADDRESSABLE(v) = 1;   
   
    v = build1(ADDR_EXPR, pchar_type_node, v);      
  } else {   
    v = g95_create_var(pchar_type_node);

    g95_call_temp_alloc(&expr->pre, v, len);      
    g95_call_temp_free(&expr->post, v);        
  }          
          
  return v;      
}          
          
          


/* conv_power_op()-- Handle the exponentiation operator. */ 
 
static void conv_power_op(g95_se *se1, g95_expr *e) {       
tree decl, tmp, dtype, v, arglist;
int use_library, kind;    
g95_se start, exp;  
  
  g95_init_se(&start, se1);        
  g95_conv_expr(&start, e->op1);  
  
  g95_add_block_to_block(&se1->pre, &start.pre);         
  g95_add_block_to_block(&se1->post, &start.post);     
     
  use_library = 1;     
     
  if (e->op2->ts.type == BT_INTEGER &&       
      e->op2->type == EXPR_CONSTANT)
    use_library = integer_exponent(se1, start.expr, e->op2);    
    
  if (use_library) {    
    g95_init_se(&exp, se1);       
       
    if (e->op2->ts.type == BT_COMPLEX) exp.reflevel = 1;  
    g95_conv_expr(&exp, e->op2);

    g95_add_block_to_block(&se1->pre, &exp.pre);        
    g95_add_block_to_block(&se1->post, &exp.post);  
  
    dtype = TREE_TYPE(start.expr); 
 
    kind = e->op1->ts.kind;        
    switch(e->op2->ts.type) {    
    case BT_INTEGER:     
      switch(e->op1->ts.type) {      
      case BT_INTEGER:          
	decl = library_integer_4_power;   
	break;       
       
      case BT_REAL:         
	switch(e->op1->ts.kind) { 
	case 4: decl = library_real_4_power; break;          
	case 8: decl = library_real_8_power; break;      
	default: 
	  g95_internal_error("Unknown real kind for ** base");        
	} 
	   
	break;      
      
      case BT_COMPLEX: 
	switch(e->op1->ts.kind) {       
	case 4: decl = library_complex_4_power; break;    
	case 8: decl = library_complex_8_power; break;       
	default:    
	  g95_internal_error("Unknown complex kind for ** base");        
	}       
       
	goto libcall;     
     
      default:          
	g95_internal_error("conv_power_op(): Bad base type"); 
      }       
       
      break;

    case BT_REAL:     
      switch(kind) {       
      case 4: decl = gfor_fndecl_math_powf; break;         
      case 8: decl = gfor_fndecl_math_pow;  break;          
      default: abort();
      }
      break;

    case BT_COMPLEX:          
      switch(kind) {
      case 4: decl = library_pow_complex_4; break;  
      case 8: decl = library_pow_complex_8; break;          
      default: abort();
      }          
          
    libcall:       
      dtype = g95_typenode_for_spec(&e->op1->ts);         
      v = g95_create_var(dtype);
      se1->expr = v;     
   
      dtype = build_pointer_type(dtype);     
      tmp = build1(ADDR_EXPR, dtype, v); 
      arglist = g95_chainon_list(NULL_TREE, tmp);    
    
      tmp = build1(ADDR_EXPR, dtype, start.expr);         
      arglist = g95_chainon_list(arglist, tmp);        
        
      arglist = g95_chainon_list(arglist, exp.expr);         
      tmp = g95_build_function_call(decl, arglist); 
 
      g95_add_expr_to_block(&se1->pre, tmp);  
      return;      
      
    default:    
      abort();      
      break;    
    }     
     
    tmp = g95_chainon_list(NULL_TREE, start.expr);    
    tmp = g95_chainon_list(tmp, exp.expr);        
    se1->expr = g95_build_function_call(decl, tmp);    
  }  
}         
         
         
 
 
tree g95_trans_pointer_assign(g95_code *codep) {        
g95_se left, right;          
stmtblock_t b;         
int lvl;          
          
  g95_init_block(&b);       
       
  lvl = (codep->expr->rank == 0);       
       
  g95_init_se(&left, NULL);         
  left.reflevel = lvl; 
  g95_conv_expr(&left, codep->expr);       
       
  if (codep->expr->rank > 0 && codep->expr2->type == EXPR_NULL) {       
    /* Nullify an array pointer (descriptor) */         
         
    g95_add_block_to_block(&b, &left.pre);          
    g95_nullify_array_pointer(&b, left.expr);  
    g95_add_block_to_block(&b, &left.post);  
  
  } else {        
    g95_init_se(&right, NULL);       
    right.reflevel = lvl;         
    g95_conv_expr(&right, codep->expr2);          
          
    g95_add_block_to_block(&b, &left.pre);  
    g95_add_block_to_block(&b, &right.pre);          
          
    g95_add_modify_expr(&b, left.expr, right.expr);     
     
    g95_add_block_to_block(&b, &left.post);
    g95_add_block_to_block(&b, &right.post);          
  }      
      
  return g95_finish_block(&b);   
} 
 
 
         
         
/* ac_assign_iterator()-- Given a constructor node that represents an
 * iterator, translate the iterator into the correct loop.  While
 * similar, these are not quite like regular do-loops because there
 * cannot be an CYCLE or EXIT statements within them. */       
       
static void ac_assign_iterator(stmtblock_t *list, g95_constructor *h,   
			       int dynamic) { 
tree tmp0, cond, cnt, var, l, s, fin, step, save; 
stmtblock_t b;     
g95_symbol *symb; 
g95_se se0;     
     
  g95_init_se(&se0, NULL); 
 
  g95_conv_expr(&se0, h->iterator->start);     
  s = save_expr(se0.expr);

  g95_conv_expr(&se0, h->iterator->end);      
  fin = save_expr(se0.expr);   
   
  g95_conv_expr(&se0, h->iterator->step);   
  step = save_expr(se0.expr);          
          
  var = g95_create_var(g95_default_integer);    
  symb = h->iterator->var->symbol;     
     
  save = symb->backend_decl;          
  symb->backend_decl = var;    
    
  g95_add_block_to_block(list, &se0.pre);   
  g95_add_block_to_block(list, &se0.post);       
       
  /* Calculate the trip count */

  tmp0 = fold(build(MINUS_EXPR, g95_default_integer, step, s));     
  tmp0 = fold(build(PLUS_EXPR, g95_default_integer, fin, tmp0));      
  tmp0 = fold(build(TRUNC_DIV_EXPR, g95_default_integer, tmp0, step)); 
 
  cnt = g95_create_var(g95_default_integer);    
  g95_add_modify_expr(list, cnt, tmp0);    
  g95_add_modify_expr(list, var, s);

  g95_init_block(&b);      
      
  l = g95_build_label_decl(NULL_TREE);

  cond = build(LE_EXPR, boolean_type_node, cnt, integer_zero_node);  
  tmp0 = build_v(GOTO_EXPR, l);     
  tmp0 = build(COND_EXPR, boolean_type_node, cond, tmp0, empty_stmt_node);        
  g95_add_expr_to_block(&b, tmp0);       
       
  ac_assign_expr(&b, h->expr, dynamic);         
         
  tmp0 = build(PLUS_EXPR, g95_default_integer, var, step);   
  g95_add_modify_expr(&b, var, tmp0);     
     
  tmp0 = build(MINUS_EXPR, g95_default_integer, cnt, integer_one_node);          
  g95_add_modify_expr(&b, cnt, tmp0);     
     
  tmp0 = g95_finish_block(&b);        
  tmp0 = build_v(LOOP_EXPR, tmp0);         
  g95_add_expr_to_block(list, tmp0);          
          
  tmp0 = build_v(LABEL_EXPR, l);    
  g95_add_expr_to_block(list, tmp0);  
  
  symb->backend_decl = save;        
}    
    
    
  
  
/* conv_expr_op()-- Translate an intrinsic operator expression.  Most
 * binary operators are handled by this function, others are passed
 * on.  Recursion is used in either case.  We are guaranteed that
 * typespecs are the same for the operands of binary operators except
 * exponentiation.  Character strings get special handling.  */         
         
static void conv_expr_op(g95_se *se0, g95_expr *expr) {     
int logical, checkstring; 
enum tree_code code; 
g95_se left, right;        
tree type, tmp;          
          
  checkstring = 0;         
  logical = 0;          
          
  switch(expr->operator) {      
  case INTRINSIC_UPLUS:   
    g95_conv_expr(se0, expr->op1);          
    return;      
      
  case INTRINSIC_UMINUS:    
    conv_unary_op(NEGATE_EXPR, se0, expr);     
    return;      
      
  case INTRINSIC_NOT:          
    conv_unary_op(TRUTH_NOT_EXPR, se0, expr); 
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
    conv_power_op(se0, expr);    
    return;        
        
  case INTRINSIC_CONCAT: 
    conv_concat_op(se0, expr);        
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
   
  g95_init_se(&left,  se0);    
  g95_init_se(&right, se0); 
 
  if (checkstring) {   
    left.reflevel  = 1;         
    right.reflevel = 1;       
  }       
       
  g95_conv_expr(&left, expr->op1);
  g95_add_block_to_block(&se0->pre, &left.pre);         
         
  g95_conv_expr(&right, expr->op2); 
  g95_add_block_to_block(&se0->pre, &right.pre);     
     
  /* For string comparisons we generate a library call, and compare
   * the return value with zero. */   
   
  if (checkstring) {       
    tmp = g95_call_library(g95_default_integer, PREFIX "compare_string",      
			   left.expr,  left.string_length,     
			   right.expr, right.string_length, NULL_TREE);     
     
    left.expr = tmp;   
    right.expr = integer_zero_node;   
  }        
        
  type = logical ? boolean_type_node : g95_typenode_for_spec(&expr->ts);        
        
  se0->expr = build(code, type, left.expr, right.expr);        
  se0->expr = fold(se0->expr);

  /* Add the post blocks.  */   
  g95_add_block_to_block(&se0->post, &right.post);     
  g95_add_block_to_block(&se0->post, &left.post);  
}   
   
   
  
  
/* ac_assign()-- Traverse a constructor, calculating elements and
 * assigning them to the current array.  Iterators are translated into
 * loops. */         
         
static void ac_assign(stmtblock_t *list, g95_constructor *n, int dynamic) {          
          
  while(n!=NULL) {        
    if (n->iterator == NULL)   
      ac_assign_expr(list, n->expr, dynamic);          
    else       
      ac_assign_iterator(list, n, dynamic);    
    
    n = n->next;     
  }
}     
     
     
        
        
/* g95_conv_expr()-- Translate an expression. */        
        
void g95_conv_expr(g95_se *s, g95_expr *e2) {      
      
  switch(e2->type) {  
  case EXPR_OP:
    conv_expr_op(s, e2);   
    break;

  case EXPR_FUNCTION:         
    conv_function_expr(s, e2);       
    break;      
      
  case EXPR_CONSTANT:          
    g95_conv_constant(s, e2); 
    break;   
   
  case EXPR_VARIABLE:          
    conv_variable(s, e2);         
    break;         
         
  case EXPR_NULL:    
    s->expr = null_pointer_node;
    break;  
  
  case EXPR_SUBSTRING:     
    conv_constant_substring(s, e2); 
    break;   
   
  case EXPR_STRUCTURE:     
    conv_structure(s, e2);      
    break;         
         
  case EXPR_ARRAY:       
    g95_internal_error("g95_conv_expr(): Array constructors");         
    break;

  default:   
    abort();    
    break;    
  }        
        
  g95_reflevel(s, s->reflevel);    
}


