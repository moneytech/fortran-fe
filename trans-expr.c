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
     


/* g95_conv_function_call()-- Generate code for a function call.  For
 * functions that return simple types, this is just generating the
 * call and adding it to the current expression.  For more complicated
 * types we create a temporary return value and pass a pointer to it
 * in the argument list.  The function call is then added to the
 * pre-chain and the result is the temporary. */  
  
void g95_conv_function_call(g95_se *s, g95_symbol *symbol,         
			    g95_actual_arglist *ap) {  
tree alist, tmp0, var, length, pre_args, t, result_type;         
int char_flag;      
g95_expr *leng;        
g95_se exp;       
       
  alist = g95_trans_arglist(ap, s);         
         
  char_flag  = 0; 
  pre_args   = NULL_TREE;  
  length     = NULL_TREE;        
  var        = NULL_TREE;   
   
  result_type = g95_result_type(symbol);          
          
  if (!symbol->result->attr.pointer && symbol->result->as == NULL)
    switch(symbol->ts.type) {
    case BT_CHARACTER:   
      char_flag = 1;      
      length = g95_create_var(g95_default_integer);
      TREE_ADDRESSABLE(length) = 1;       
       
      /* If the local declaration is something more complicated than a
       * constant, then we have a problem because the length can
       * involve dummy variables of the function being called.  The
       * length needs to be computed in the caller for assumed-length
       * functions.  For fortran 77, these all have to be constant, so
       * this code is mainly for legacy applications. */    
    
      leng = symbol->result->ts.cl->length;        
      if (leng->type != EXPR_CONSTANT)
	tmp0 = integer_zero_node;   
      else { 
	g95_conv_constant(&exp, leng); 
	tmp0 = exp.expr; 
      }     
     
      g95_add_modify_expr(&s->pre, length, tmp0);          
          
      t = build_pointer_type(g95_default_integer);        
      tmp0 = build1(ADDR_EXPR, t, length);     
     
      pre_args = g95_chainon_list(pre_args, tmp0); 
      var = g95_create_var(pchar_type_node);

      g95_call_temp_free(&s->post, var);  
  
      s->string_length = length;
      break; 
 
    case BT_DERIVED:        
    case BT_COMPLEX:  
      t = g95_typenode_for_spec(&symbol->result->ts); 
      var = g95_create_var(t);      
      TREE_ADDRESSABLE(var) = 1;

      t = build_pointer_type(t); 
      tmp0 = build1(ADDR_EXPR, t, var);     
     
      pre_args = g95_chainon_list(pre_args, tmp0);        
      break;    
    
    default:        
      break;      
    }        
        
  if (pre_args != NULL_TREE) alist = chainon(pre_args, alist);       
       
  if (symbol->attr.dummy) 
    tmp0 = symbol->backend_decl;      /* Function pointer */        
  else {          
    t = build_pointer_type(TREE_TYPE(symbol->backend_decl));    
    tmp0 = build1(ADDR_EXPR, t, symbol->backend_decl);   
  }         
         
  tmp0 = build(CALL_EXPR, result_type, tmp0, alist);   
  TREE_SIDE_EFFECTS(tmp0) = 1;  
  
  if (char_flag) {        
    g95_add_modify_expr(&s->pre, var, tmp0);   
    s->expr = var;
  } else if (var == NULL)          
    s->expr = tmp0;   
  else {      
    g95_add_expr_to_block(&s->pre, tmp0);      
    s->expr = var;  
  }       
} 
 
 
      
      
/* ac_assign_expr()-- Assign a scalar expression within an array
 * constructor to an array */    
    
static void ac_assign_expr(stmtblock_t *body, g95_expr *g, int dynamic) {
tree tmp, l;          
g95_se se0;         
         
  if (g->type == EXPR_ARRAY) {  
    ac_assign(body, g->value.constructor, dynamic);          
    return; 
  }          
          
  g95_init_se(&se0, NULL);        
  se0.reflevel = 1;  
  g95_conv_expr(&se0, g);         
         
  g95_add_block_to_block(body, &se0.pre);   
   
  l = (se0.string_length == NULL) 
    ? integer_minus_one_node    
    : se0.string_length;        
        
  tmp = g95_call_library(void_type_node,   
			 dynamic ? PREFIX "ac_assign2" : PREFIX "ac_assign1", 
			 se0.expr, l, NULL);     
  g95_add_expr_to_block(body, tmp);        
        
  g95_add_block_to_block(body, &se0.post);     
}


       
       
/* conv_concat_op()-- Handle a string concatenation operation.  A
 * temporary is allocated to hold the result */         
         
static void conv_concat_op(g95_se *se, g95_expr *e2) {     
g95_se left, right;        
tree var0, tmp;    
    
  g95_init_se(&left, se);     
  left.reflevel = 1;    
  g95_conv_expr(&left, e2->op1);   
   
  g95_add_block_to_block(&se->pre, &left.pre);        
  g95_add_block_to_block(&se->post, &left.post);     
     
  g95_init_se(&right, se);         
  right.reflevel = 1;       
  g95_conv_expr(&right, e2->op2);      
      
  g95_add_block_to_block(&se->pre, &right.pre);          
  g95_add_block_to_block(&se->post, &right.post);      
      
  se->string_length = fold(build(PLUS_EXPR, g95_default_integer,      
				 left.string_length, right.string_length));  
  
  se->string_length = save_expr(se->string_length);          
          
  var0 = g95_temp_string(se, se->string_length);          
          
  tmp = g95_call_library(void_type_node, PREFIX "concat_string", var0,       
			 left.expr,  left.string_length,        
			 right.expr, right.string_length, NULL_TREE);         
         
  g95_add_expr_to_block(&se->pre, tmp); 
 
  se->expr = var0;      
}    
    
    
       
       
/* trans_constructor_assign()-- Translate the assignment of an array
 * constructor to an array. */     
     
static tree trans_constructor_assign(g95_expr *expr1, g95_expr *expr2) {         
stmtblock_t block;  
tree s, tmp0;     
g95_se se1;         
         
  g95_init_block(&block);
  g95_init_se(&se1, NULL);      
  se1.reflevel = 1;  
  
  g95_conv_expr(&se1, expr1);  
  
  g95_add_block_to_block(&block, &se1.pre);          
          
  s = size_in_bytes(g95_typenode_for_spec(&expr1->ts));      
  tmp0 = expr1->symbol->attr.artificial ? integer_one_node : integer_zero_node;      
      
  tmp0 = g95_call_library(void_type_node, PREFIX "start_ac_assign", se1.expr,    
			 tmp0, s, NULL);        
        
  g95_add_expr_to_block(&block, tmp0);        
  g95_add_block_to_block(&block, &se1.post);     
     
/* Traverse the constructor, calculating each element.  Elements here
 * can be general expressions. */         
         
  ac_assign(&block, expr2->value.constructor, expr1->symbol->attr.artificial);          
          
  return g95_finish_block(&block);
}          
          
          
   
   
/* g95_init_se()-- Initialize a simple expression holder.

   Care must be taken when multiple se are created with the same parent.
   The child se must be kept in sync.  The easiest way is to delay creation
   of a child se until after after the previous se has been translated.  */ 
 
void g95_init_se(g95_se *expr, g95_se *parent) {        
        
  memset(expr, 0, sizeof(g95_se)); 
  g95_init_block(&expr->pre);        
  g95_init_block(&expr->post);     
     
  expr->parent = parent;     
}     
     
     


/* conv_function_expr()-- Translate a function call. */       
       
static void conv_function_expr(g95_se *se, g95_expr *exp) {    
    
  if (exp->value.function.isym) 
    g95_conv_intrinsic_function(se, exp);    
  else 
    g95_conv_function_call(se, exp->symbol, exp->value.function.actual); 
}


/* constant_structure()-- Create a constant derived type constuctor. */        
        
static void conv_structure(g95_se *se, g95_expr *w) {        
g95_constructor *cons;         
g95_component *comp;          
variable_info v;        
g95_expr *l;      
g95_se se2;        
tree d;

  d = NULL_TREE;    
    
  cons = w->value.constructor;     
  comp = w->symbol->components;        
        
  while(cons != NULL) {   
    g95_init_se(&se2, NULL);  
  
    l = cons->expr;      
      
    if (comp->dimension) { 
      g95_component_vinfo(comp, &v);          
      v.value = l;          
          
      se2.expr = g95_conv_array_initializer(&v);     
    } else {          
      if (comp->pointer) se2.reflevel = 1;     
     
      g95_conv_expr(&se2, l);         
         
      if (l->ts.type == BT_CHARACTER && l->rank == 0)    
	se2.expr = g95_resize_string_constant(se2.expr,   
					      comp->ts.cl->backend_decl);    
    }      
      
    g95_add_block_to_block(&se->pre,  &se2.pre);  
    g95_add_block_to_block(&se->post, &se2.post); 
 
    d = tree_cons(comp->backend_decl, se2.expr, d);     
     
    cons = cons->next; 
    comp = comp->next;
  }          
          
  se->expr = build(CONSTRUCTOR, w->symbol->backend_decl, NULL_TREE,     
		   nreverse(d));         
}    
    
    


/* g95_trans_arglist()-- Translate an actual argument list into tree
 * form.  Alternate return elements are ignored. */     
     
tree g95_trans_arglist(g95_actual_arglist *args, g95_se *s) {       
tree a, arglist_tail;         
g95_se parmse;   
   
  a = NULL_TREE; 
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
  
    g95_add_block_to_block(&s->pre,  &parmse.pre);          
    g95_add_block_to_block(&s->post, &parmse.post);          
          
    a = g95_chainon_list(a, parmse.expr);        
        
    if (args->u.expr == NULL) {        
      if (args->missing_arg_type == BT_CHARACTER)       
	arglist_tail = g95_chainon_list(arglist_tail, integer_zero_node);        
    } else {   
      if (args->u.expr->ts.type == BT_CHARACTER)      
	arglist_tail = g95_chainon_list(arglist_tail, parmse.string_length);         
    } 
  }        
        
  if (arglist_tail != NULL_TREE) a = chainon(a, arglist_tail);     
     
  return a;
}       
       
       
  
  
/* component_array()-- Create a descriptor for the array.  TODO: if
 * the next reference is for an array element, then we can merge those
 * two steps and avoid initializing a descriptor that is only used
 * once.  Otherwise we are probably passing the array section as an
 * actual parameter and need the descriptor. */     
     
static void component_array(g95_se *s, g95_component *w, tree storage) {         
variable_info vinfo;      
tree desc;          
          
  g95_component_vinfo(w, &vinfo);   
  desc = g95_create_var(g95_get_descriptor(&vinfo)); 
  g95_temp_array_descriptor(s, &vinfo, desc, storage);       
       
  s->expr = desc;   
}       
       
       
    
    
/* ac_assign_iterator()-- Given a constructor node that represents an
 * iterator, translate the iterator into the correct loop.  While
 * similar, these are not quite like regular do-loops because there
 * cannot be an CYCLE or EXIT statements within them. */        
        
static void ac_assign_iterator(stmtblock_t *block, g95_constructor *h,      
			       int dynamic) {          
tree t, cond, count, variable, label, start, e, step;    
stmtblock_t b;  
g95_se s; 
 
  g95_init_se(&s, NULL);       
       
  g95_conv_expr(&s, h->iterator->start);  
  start = save_expr(s.expr);  
  
  g95_conv_expr(&s, h->iterator->end);   
  e = save_expr(s.expr);

  g95_conv_expr(&s, h->iterator->step);         
  step = save_expr(s.expr);       
       
  g95_conv_expr(&s, h->iterator->var);          
  variable = s.expr;

  g95_add_block_to_block(block, &s.pre);  
  g95_add_block_to_block(block, &s.post);       
       
  /* Calculate the trip count */          
          
  t = fold(build(MINUS_EXPR, g95_default_integer, step, start));          
  t = fold(build(PLUS_EXPR, g95_default_integer, e, t));        
  t = fold(build(TRUNC_DIV_EXPR, g95_default_integer, t, step)); 
 
  count = g95_create_var(g95_default_integer);   
  g95_add_modify_expr(block, count, t);        
  g95_add_modify_expr(block, variable, start);         
         
  g95_init_block(&b);    
    
  label = g95_build_label_decl(NULL_TREE); 
 
  cond = build(LE_EXPR, boolean_type_node, count, integer_zero_node); 
  t = build_v(GOTO_EXPR, label);      
  t = build(COND_EXPR, boolean_type_node, cond, t, empty_stmt_node);
  g95_add_expr_to_block(&b, t);          
          
  ac_assign_expr(&b, h->expr, dynamic);     
     
  t = build(PLUS_EXPR, g95_default_integer, variable, step);
  g95_add_modify_expr(&b, variable, t);    
    
  t = build(MINUS_EXPR, g95_default_integer, count, integer_one_node);
  g95_add_modify_expr(&b, count, t);       
       
  t = g95_finish_block(&b);          
  t = build_v(LOOP_EXPR, t);   
  g95_add_expr_to_block(block, t);

  t = build_v(LABEL_EXPR, label);  
  g95_add_expr_to_block(block, t);          
}          
          
          
     
     
/* g95_reflevel()-- Given something that is an object, a pointer to an
 * object or a pointer to a pointer to an object and a reference
 * level, generate code to bring the object to the desired level.
 * Level zero is a reference to some object, one is a pointer to an
 * object and two is a pointer to a pointer to the object. */    
    
void g95_reflevel(g95_se *se0, int l) {         
tree tmp1, dtype, new_type, object;   
   
  object = se0->expr;
  dtype = TREE_TYPE(object);

  if (!POINTER_TYPE_P(dtype)) {  /* Object is not a pointer */  
    switch(l) {      
    case 0:     
      break;  
  
    case 1:     
      if (CONSTANT_P(object) || TREE_CODE(object) == CALL_EXPR ||        
	  TREE_CODE(object) == NOP_EXPR ||   
	  IS_EXPR_CODE_CLASS(TREE_CODE_CLASS(TREE_CODE(object)))) {   
   
	tmp1 = g95_create_var(dtype);
	g95_add_modify_expr(&se0->pre, tmp1, object);          
	object = tmp1;        
      }     
     
      se0->expr = build1(ADDR_EXPR, build_pointer_type(dtype), object);
      break;          
          
    case 2:   
      /* Can't think of a case where this is needed */        
      g95_internal_error("g95_reflevel(): Bad case"); 
    }    
    
    return;  
  }       
       
  new_type = TREE_TYPE(dtype); 
  if (!POINTER_TYPE_P(new_type)) {  /* Pointer to an object */  
    switch(l) {    
    case 0:          
      se0->expr = build1(INDIRECT_REF, new_type, object);      
      break;

    case 1:         
      break;   
   
    case 2:    
      se0->expr = build1(ADDR_EXPR, build_pointer_type(dtype), object);      
      break;
    } 
 
    return;         
  }     
     
  new_type = TREE_TYPE(new_type);        
  if (!POINTER_TYPE_P(new_type)) {  /* Pointer to a pointer to an object */       
    switch(l) {     
    case 0:     
      dtype = build_pointer_type(new_type);    
      object = build1(INDIRECT_REF, dtype, object);   
   
      se0->expr = build1(INDIRECT_REF, new_type, object);  
      break;   
   
    case 1:
      se0->expr = build1(INDIRECT_REF, build_pointer_type(new_type), object);     
      break; 
 
    case 2:     
      break; 
    }  
  
    return;        
  }   
   
  g95_internal_error("g95_reflevel(): Bad type");   
}       
       
       
          
          
/* ac_assign()-- Traverse a constructor, calculating elements and
 * assigning them to the current array.  Iterators are translated into
 * loops. */          
          
static void ac_assign(stmtblock_t *body, g95_constructor *m, int dynamic) {       
       
  while(m!=NULL) {          
    if (m->iterator == NULL)         
      ac_assign_expr(body, m->expr, dynamic);        
    else  
      ac_assign_iterator(body, m, dynamic); 
 
    m = m->next;     
  }  
} 
 
 


/* conv_component_ref()-- Convert a derived type component reference.  */      
      
static void conv_component_ref(g95_se *se1, g95_ref *ref) {
tree tmp0, declr, field;   
g95_component *t;          
          
  g95_reflevel(se1, 0);        
  t = ref->u.c.component;  
  assert(t->backend_decl);      
      
  field = t->backend_decl;         
  assert(TREE_CODE(field) == FIELD_DECL);   
  declr = se1->expr; 
 
  tmp0 = build(COMPONENT_REF, TREE_TYPE(field), declr, field); 
 
  if (t->pointer) {       
    //se1->expr = build1(INDIRECT_REF, TREE_TYPE(TREE_TYPE(tmp0)), tmp0);        
    se1->expr = tmp0;      
    return;     
  }   
   
  if (t->dimension) {
    component_array(se1, t, tmp0);      
    return;        
  }     
     
  if (t->ts.type == BT_CHARACTER) se1->string_length = t->ts.cl->backend_decl;     
     
  se1->expr = tmp0;          
} 
 
 
        
        
/* conv_substring()-- Converts a string into a substring.  The
 * standard specifies that the start and end indexes be within the
 * range of the string.  This implementation doesn't check for
 * violations of this condition, allowing access to memory outside of
 * the string.  To fix this, start and end would need to be clamped to
 * the legal ranges. */    
    
static void conv_substring(g95_se *se0, g95_ref *reference) {       
tree start_minus_one, typ;   
g95_se start, end, len;        
        
  g95_reflevel(se0, 1);   
  g95_init_se(&start, se0);

  if (reference->u.ss.start == NULL)       
    start.expr = integer_one_node; 
  else {          
    g95_conv_expr_type(&start, reference->u.ss.start, g95_default_integer);

    g95_add_block_to_block(&se0->pre, &start.pre);          
    g95_add_block_to_block(&se0->post, &start.post);      
  }

  start_minus_one = save_expr(fold(build(MINUS_EXPR, g95_default_integer,      
					 start.expr, integer_one_node)));     
     
  /* Deal with the end specification */ 
 
  g95_init_se(&end, se0);    
  typ = TREE_TYPE(se0->expr);       
       
  if (reference->u.ss.end == NULL)      
    end.expr = se0->string_length;        
  else {
    g95_conv_expr_type(&end, reference->u.ss.end, g95_default_integer);      
      
    g95_add_block_to_block(&se0->pre, &end.pre);    
    g95_add_block_to_block(&se0->post, &end.post);
  }      
      
  /* string length = DIM(end, start-1).  We choose this form because
   * 'start' is likely to be one, in which case the subtraction
   * collapses to DIM(end, 0). */   
   
  g95_init_se(&len, se0); 
  g95_dim(&len, end.expr, start_minus_one);         
         
  g95_add_block_to_block(&se0->pre,  &len.pre);        
  g95_add_block_to_block(&se0->post, &len.post);        
        
  se0->string_length = len.expr; 
  se0->expr = fold(build(PLUS_EXPR, typ, se0->expr, start_minus_one));    
} 
 
 
         
         
/* conv_variable()-- Return the contents of a variable. Also handles
 * reference/pointer variables (all Fortran pointer references are
 * implicit).  When processing a variable reference the rule is to do
 * as little as possible.  If an array reference is computed, a
 * pointer to the reference is computed. */      
      
static void conv_variable(g95_se *se1, g95_expr *expr) {    
g95_typespec t;
g95_symbol *s;        
g95_ref *ref;     
int as_flag;          
          
  s = expr->symbol; 
  if (s->backend_decl == NULL_TREE) g95_get_symbol_decl(s);
  se1->expr = s->backend_decl;        
        
  /* Procedure actual arguments. */     
     
  if (expr->ts.type == BT_PROCEDURE && se1->expr != current_function_decl) {          
    if (!expr->symbol->attr.dummy) {  
      assert(TREE_CODE(se1->expr) == FUNCTION_DECL); 
      se1->expr = build1(ADDR_EXPR, build_pointer_type(TREE_TYPE(se1->expr)),      
			se1->expr);      
    }   
   
    return;     
  }       
       
  if (s->ts.type == BT_CHARACTER)
    se1->string_length = s->ts.cl->backend_decl;   
   
  ref = expr->ref;  
  t = s->ts;   
   
  as_flag = (s->as == NULL) ? 0 : s->as->type == AS_ASSUMED_SIZE;          
          
  for(ref=expr->ref; ref; ref=ref->next) {        
    switch(ref->type) {        
    case REF_ARRAY:       
      g95_conv_array_ref(se1, &ref->u.ar, &t, as_flag);   
      break;     
     
    case REF_COMPONENT: 
      conv_component_ref(se1, ref);   
      t = ref->u.c.component->ts;        
        
      if (t.cl != NULL) se1->string_length = t.cl->backend_decl;  
      break;    
    
    case REF_SUBSTRING:   
      conv_substring(se1, ref); 
      break;     
     
    default:     
      abort();    
      break;
    }         
         
    as_flag = 0;     
  }
}    
    
    
     
     
/* g95_conv_expr_type()-- Convert an expression to a particular
 * destination type. */   
   
void g95_conv_expr_type(g95_se *se1, g95_expr *e, tree type) {   
   
  g95_conv_expr(se1, e);     
  se1->expr = convert(type, se1->expr);      
}       
       
       
        
        
/* reciprocal()-- Given a tree, return the reciprocal. */         
         
static tree reciprocal(tree b) {
tree tmp0;         
         
  if (TREE_TYPE(b) == TREE_TYPE(integer_one_node))        
    tmp0 = build(TRUNC_DIV_EXPR, TREE_TYPE(b), integer_one_node, b);     
  else {      
    tmp0 = g95_build_const(TREE_TYPE(b), integer_one_node);   
    tmp0 = build(RDIV_EXPR, TREE_TYPE(b), tmp0, b);         
  }   
   
  return tmp0;          
}        
        
        
   
   
static void chain_multiply(g95_se *se, tree bottom, int power) {     
int b, r, z, chain_length, negative, addition_chain[MAX_CHAIN_LEN];         
tree tmp1, intermediate[MAX_CHAIN_LEN];     
     
  if (power > 0)          
    negative = 0;      
  else {
    negative = 1;   
    power = -power;          
  } 
 
  chain_length = 0;    
  for(b=power; b!=0; b=power_tree[b])        
    chain_length++;

  if (chain_length > MAX_CHAIN_LEN) 
    g95_internal_error("chain_multiply(): Table too small");       
       
  r = 0;   
  for(b=power; b!=0; b=power_tree[b], r++)          
    addition_chain[chain_length-r-1] = b;   
   
  /* Now we have the chain.  Compute x and x^2 and store them to
   * temporaries. */

  intermediate[0] = save_expr(bottom);         
  intermediate[1] = build(MULT_EXPR, TREE_TYPE(bottom),  
			  intermediate[0], intermediate[0]);        
        
  tmp1 = NULL_TREE;    
    
  /* Now traverse the chain.  If we're not at the end of the chain,
   * store the result to a temporary so that it can be re-used without
   * being recomputed.  The last element is the result of the whole
   * expression. */          
          
  for(b=2; b<chain_length; b++) {   
    for(r=0; r<b; r++)     
      for(z=0; z<b; z++)  
	if (addition_chain[r] + addition_chain[z] == addition_chain[b])       
	  goto found;

    g95_internal_error("chain_multiply(): Corrupt addition chain for n=%d",
		       power);        
  found: 
    tmp1 = build(MULT_EXPR, TREE_TYPE(bottom), intermediate[r], intermediate[z]);    
    
    if (b != chain_length-1) intermediate[b] = save_expr(tmp1);
  }         
         
  if (negative) tmp1 = reciprocal(tmp1);    
  se->expr = tmp1;      
}     
     
     
        
        
/* g95_conv_char_length()-- Convert a character length parameter.
 * This is special in that negative lengths must be clamped to
 * zero.  The length must not be assumed. */          
          
tree g95_conv_char_length(g95_se *s, g95_typespec *typ) {      
tree l, tmp1;      
g95_se se0;        
        
  g95_init_se(&se0, NULL);  
  
  g95_conv_expr(&se0, typ->cl->length);      
  g95_add_block_to_block(&s->pre, &se0.pre);          
  g95_add_block_to_block(&s->post, &se0.post);        
        
  l = save_expr(se0.expr);          
          
  tmp1 = fold(build(LT_EXPR, boolean_type_node, l, integer_zero_node));
  l = fold(build(COND_EXPR, g95_default_integer, tmp1, integer_zero_node,        
		      l));

  return l;     
}        
        
        
        
       
       
/* integer_exponent()-- Handle the case of constant integer exponent.
 * Returns nonzero if the exponent was such that we want to call the
 * library function, zero if we've taken care of everything. */ 
 
static int integer_exponent(g95_se *s, tree start, g95_expr *exponent) {          
int z, *interp, power;    
    
  power = mpz_get_si(exponent->value.integer);          
          
  /* Special cases of the special case */

  switch(power) { 
  case -2:       
    start = save_expr(start);      
    s->expr = reciprocal(build(MULT_EXPR, TREE_TYPE(start), start, start)); 
    return 0;

  case -1:   
    s->expr = reciprocal(start);        
    return 0;  
  
  case 0:          
    return 1;      /* Want the check for 0**0 */        
        
  case 1: 
    s->expr = start;          
    return 0;   
   
  case 2:      
    start = save_expr(start);       
    s->expr = build(MULT_EXPR, TREE_TYPE(start), start, start); 
    return 0;       
  }   
   
  /* See if we have the information on how to do this with the minimum
   * multiplications. */         
         
  interp = power_tree + 2;   
  for(z=2; z<abs(power); z++, interp++)       
    if (*interp == 0) return 1;   /* Hit the end of the table */     
     
  chain_multiply(s, start, power);      
  return 0;     
}          
          
          
         
         
/* g95_trans_assignment()-- Translate an assignment statement */       
       
tree g95_trans_assignment(g95_expr *expr1, g95_expr *expr2) {       
stmtblock_t blk;         
g95_se lse, rse;  
tree t;       
       
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
         
  g95_init_block(&blk);     
     
  g95_init_se(&lse, NULL);       
  g95_init_se(&rse, NULL);    
    
  if (expr1->ts.type == BT_CHARACTER || expr1->rank > 0) {  
    lse.reflevel = 1;       
    rse.reflevel = 1;     
  }        
        
  g95_conv_expr(&lse, expr1);          
  g95_conv_expr(&rse, expr2);      
      
  g95_add_block_to_block(&blk, &lse.pre);     
  g95_add_block_to_block(&blk, &rse.pre);      
      
  if (expr1->ts.type != BT_CHARACTER || expr1->rank > 0)     
    g95_add_modify_expr(&blk, lse.expr, rse.expr);       
  else { 
    t = g95_call_library(void_type_node, PREFIX "copy_string",    
			   lse.expr, lse.string_length, 
			   rse.expr, rse.string_length, NULL_TREE);

    g95_add_expr_to_block(&blk, t);
  }        
        
  g95_add_block_to_block(&blk, &lse.post);          
  g95_add_block_to_block(&blk, &rse.post);      
      
  return g95_finish_block(&blk);  
}  
       
       
/* g95_temp_string()-- Generate code to allocate a string temporary.
 * Returns a pointer to the storage area or a character pointer
 * variable. */

tree g95_temp_string(g95_se *expr, tree length) {    
tree var0, type;        
        
  type = build_range_type(g95_default_integer, integer_one_node, length);     
  type = build_array_type(g95_character1_type_node, type); 
 
  if (g95_stack_variable(type)) {   
    var0 = g95_create_var(type);  
    TREE_ADDRESSABLE(var0) = 1;    
    
    var0 = build1(ADDR_EXPR, pchar_type_node, var0);       
  } else {    
    var0 = g95_create_var(pchar_type_node);         
         
    g95_call_temp_alloc(&expr->pre, var0, length);
    g95_call_temp_free(&expr->post, var0);       
  }     
     
  return var0;  
}         
         
         
     
     
/* conv_constant_substring()-- This subroutine converts a substring of
 * a constant string. */   
   
static void conv_constant_substring(g95_se *se, g95_expr *e) {
int leng;

  leng = e->value.character.length;         
  se->expr = g95_build_string_const(leng, e->value.character.string);
  se->string_length = build_int_2(leng, 0);          
          
  conv_substring(se, e->ref);  
}         
         
         
         
         
/* conv_power_op()-- Handle the exponentiation operator. */  
  
static void conv_power_op(g95_se *se0, g95_expr *expr) {  
tree fndecl, t, typ, var, arg; 
int use_library, knd;      
g95_se b, exp;          
          
  g95_init_se(&b, se0);        
  g95_conv_expr(&b, expr->op1); 
 
  g95_add_block_to_block(&se0->pre, &b.pre);     
  g95_add_block_to_block(&se0->post, &b.post);       
       
  use_library = 1;         
         
  if (expr->op2->ts.type == BT_INTEGER &&         
      expr->op2->type == EXPR_CONSTANT)
    use_library = integer_exponent(se0, b.expr, expr->op2);     
     
  if (use_library) {
    g95_init_se(&exp, se0);      
      
    if (expr->op2->ts.type == BT_COMPLEX) exp.reflevel = 1;         
    g95_conv_expr(&exp, expr->op2);   
   
    g95_add_block_to_block(&se0->pre, &exp.pre);  
    g95_add_block_to_block(&se0->post, &exp.post);          
          
    typ = TREE_TYPE(b.expr);     
     
    knd = expr->op1->ts.kind;        
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
	switch(expr->op1->ts.kind) {     
	case 4: fndecl = library_complex_4_power; break;       
	case 8: fndecl = library_complex_8_power; break;        
	default:      
	  g95_internal_error("Unknown complex kind for ** base");         
	}          
          
	goto libcall;    
    
      default:        
	g95_internal_error("conv_power_op(): Bad base type");     
      }          
          
      break;  
  
    case BT_REAL:         
      switch(knd) {     
      case 4: fndecl = gfor_fndecl_math_powf; break;      
      case 8: fndecl = gfor_fndecl_math_pow;  break;
      default: abort();          
      }       
      break; 
 
    case BT_COMPLEX:         
      switch(knd) {   
      case 4: fndecl = library_pow_complex_4; break;          
      case 8: fndecl = library_pow_complex_8; break;       
      default: abort();        
      }   
   
    libcall:       
      typ = g95_typenode_for_spec(&expr->op1->ts);       
      var = g95_create_var(typ);     
      se0->expr = var;          
        
      typ = build_pointer_type(typ);    
      t = build1(ADDR_EXPR, typ, var);
      arg = g95_chainon_list(NULL_TREE, t);

      t = build1(ADDR_EXPR, typ, b.expr);     
      arg = g95_chainon_list(arg, t);      
      
      arg = g95_chainon_list(arg, exp.expr);      
      t = g95_build_function_call(fndecl, arg);  
  
      g95_add_expr_to_block(&se0->pre, t);      
      return;         
         
    default:
      abort();   
      break;        
    }      
      
    t = g95_chainon_list(NULL_TREE, b.expr);         
    t = g95_chainon_list(t, exp.expr);         
    se0->expr = g95_build_function_call(fndecl, t);   
  }       
}      
      
      
      
      
/* g95_stack_variable()-- Returns nonzero if a variable of the given
 * type should go on the stack.  This is used mainly for arrays.  When
 * the current frame size is exceeded, we start spilling things to the
 * heap. */ 
 
int g95_stack_variable(tree typ) {        
int q;         
         
  q = int_size_in_bytes(typ);  
  if (q == -1) return 0;     
     
  if (g95_context->frame_size + q > g95_option.max_frame_size) return 0;  
  
  g95_context->frame_size += q;      
      
  return 1;      
}       
       
       
      
      
/* conv_unary_op()-- Convert unary operators .not. and minus */        
        
static void conv_unary_op(enum tree_code code, g95_se *se, g95_expr *e2) {        
g95_se operand;     
     
  g95_init_se(&operand, se);     
  g95_conv_expr(&operand, e2->op1);     
     
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


        
        
/* g95_conv_expr0()-- Convert an expression without having to worry
 * about the parent se. */  
  
void g95_conv_expr0(g95_se *se1, g95_expr *expr) {        
g95_se se0;    
    
  g95_init_se(&se0, NULL);      
  g95_conv_expr(&se0, expr);       
       
  se1->expr = se0.expr;  
  
  g95_add_block_to_block(&se1->pre, &se0.pre);  
  g95_add_block_to_block(&se1->post, &se0.post);         
}        
        
        
    
    
tree g95_trans_pointer_assign(g95_code *cp) {      
stmtblock_t b;         
g95_se left, right;         
         
  g95_init_block(&b);

  g95_init_se(&left, NULL);     
  left.reflevel = 1;
  g95_conv_expr(&left, cp->expr);         
         
  g95_init_se(&right, NULL);  
  right.reflevel = 1;     
  g95_conv_expr(&right, cp->expr2);       
       
  g95_add_block_to_block(&b, &left.pre);      
  g95_add_block_to_block(&b, &right.pre);      
      
  g95_add_modify_expr(&b, left.expr, right.expr);          
          
  g95_add_block_to_block(&b, &left.post);  
  g95_add_block_to_block(&b, &right.post);      
      
  return g95_finish_block(&b);  
}  
  
  


/* conv_expr_op()-- Translate an intrinsic operator expression.  Most
 * binary operators are handled by this function, others are passed
 * on.  Recursion is used in either case.  We are guaranteed that
 * typespecs are the same for the operands of binary operators except
 * exponentiation.  Character strings get special handling.  */ 
 
static void conv_expr_op(g95_se *s, g95_expr *e2) {    
int logical, checkstring;         
enum tree_code codep;  
g95_se left, right;        
tree typ, t;

  checkstring = 0;  
  logical = 0;    
    
  switch(e2->operator) {  
  case INTRINSIC_UPLUS:        
    g95_conv_expr(s, e2->op1);         
    return;   
   
  case INTRINSIC_UMINUS:  
    conv_unary_op(NEGATE_EXPR, s, e2);
    return;    
    
  case INTRINSIC_NOT: 
    conv_unary_op(TRUTH_NOT_EXPR, s, e2);         
    return;      
      
  case INTRINSIC_PLUS:    
    codep = PLUS_EXPR;    
    break;    
    
  case INTRINSIC_MINUS:     
    codep = MINUS_EXPR;     
    break;       
       
  case INTRINSIC_TIMES:        
    codep = MULT_EXPR;    
    break;         
         
  case INTRINSIC_DIVIDE:        
    /* If the expression is real or complex, use an RDIV_EXPR. If
     * integer, we must round towards zero, so we use a TRUNC_DIV_EXPR. */   
   
    codep = (e2->ts.type == BT_INTEGER) ? TRUNC_DIV_EXPR : RDIV_EXPR;      
    break;    
    
  case INTRINSIC_POWER:     
    conv_power_op(s, e2); 
    return;    
    
  case INTRINSIC_CONCAT:    
    conv_concat_op(s, e2);    
    return;        
        
  case INTRINSIC_AND:          
    codep = BIT_AND_EXPR;
    logical = 1;         
    break;  
  
  case INTRINSIC_OR:          
    codep = BIT_IOR_EXPR;      
    logical = 1;      
    break;  
  
    /* EQV and NEQV only work on logicals, but since we represent them
     * as integers, we can use EQ_EXPR and NE_EXPR for them.  */  
  
  case INTRINSIC_EQ:
  case INTRINSIC_EQV:        
    codep = EQ_EXPR; 
    checkstring = 1;   
    logical = 1;          
    break;    
    
  case INTRINSIC_NE:         
  case INTRINSIC_NEQV:  
    codep = NE_EXPR;  
    checkstring = 1;   
    logical = 1;   
    break;         
         
  case INTRINSIC_GT:       
    codep = GT_EXPR;     
    checkstring = 1;     
    logical = 1;     
    break;

  case INTRINSIC_GE:   
    codep = GE_EXPR;         
    checkstring = 1;         
    logical = 1;         
    break;   
   
  case INTRINSIC_LT:        
    codep = LT_EXPR;          
    checkstring = 1;         
    logical = 1;      
    break;       
       
  case INTRINSIC_LE:      
    codep = LE_EXPR;  
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
   
  assert(e2->op1->ts.type == e2->op2->ts.type);  
  
  if (checkstring && e2->op1->ts.type != BT_CHARACTER) checkstring = 0;   
   
  g95_init_se(&left,  s);   
  g95_init_se(&right, s);   
   
  if (checkstring) {          
    left.reflevel  = 1;          
    right.reflevel = 1;    
  }   
   
  g95_conv_expr(&left, e2->op1);      
  g95_add_block_to_block(&s->pre, &left.pre);     
     
  g95_conv_expr(&right, e2->op2);       
  g95_add_block_to_block(&s->pre, &right.pre);

  /* For string comparisons we generate a library call, and compare
   * the return value with zero. */         
         
  if (checkstring) {  
    t = g95_call_library(g95_default_integer, PREFIX "compare_string",  
			   left.expr,  left.string_length,       
			   right.expr, right.string_length, NULL_TREE);          
          
    left.expr = t;   
    right.expr = integer_zero_node;   
  }          
          
  typ = logical ? boolean_type_node : g95_typenode_for_spec(&e2->ts);          
          
  s->expr = build(codep, typ, left.expr, right.expr);
  s->expr = fold(s->expr);        
        
  /* Add the post blocks.  */  
  g95_add_block_to_block(&s->post, &right.post);     
  g95_add_block_to_block(&s->post, &left.post);  
}       
       
       
        
        
/* g95_conv_expr()-- Translate an expression. */       
       
void g95_conv_expr(g95_se *se0, g95_expr *e2) { 
 
  switch(e2->type) {     
  case EXPR_OP:     
    conv_expr_op(se0, e2);        
    break; 
 
  case EXPR_FUNCTION:       
    conv_function_expr(se0, e2);       
    break;

  case EXPR_CONSTANT:       
    g95_conv_constant(se0, e2);        
    break;

  case EXPR_VARIABLE:
    conv_variable(se0, e2);  
    break;        
        
  case EXPR_NULL:      
    se0->expr = null_pointer_node;        
    break;

  case EXPR_SUBSTRING:       
    conv_constant_substring(se0, e2);   
    break;        
        
  case EXPR_STRUCTURE:         
    conv_structure(se0, e2);       
    break; 
 
  case EXPR_ARRAY:        
    g95_internal_error("g95_conv_expr(): Array constructors"); 
    break;        
        
  default:    
    abort(); 
    break;   
  }  
  
  g95_reflevel(se0, se0->reflevel);     
}       
       
       
