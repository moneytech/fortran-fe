/* Statement translation
   Copyright (C) 2000, 2001, 2002 Free Software Foundation, Inc.
   Contributed by Paul Brook <paul@nowt.org>
   and Steven Bosscher <s.bosscher@student.tudelft.nl>

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
          
/* trans-stmt.c-- generate GCC trees from g95_code */

#include "trans.h"
   
   


/* alt_return_jump()-- Generate a switch to jump to the right
 * alternate return point. */     
     
static tree alt_return_jump(tree variable, g95_actual_arglist *actual) {       
stmtblock_t block;
tree tmp0;     
int c;        
        
  g95_init_block(&block);
  c = 1;          
          
  for(; actual!=NULL; actual=actual->next) {   
    if (actual->type != ALT_RETURN) continue;    
    
    tmp0 = build_int_2(c++, 0);  
    tmp0 = build_v(CASE_LABEL_EXPR, tmp0, tmp0); 
    g95_add_expr_to_block(&block, tmp0); 
 
    tmp0 = g95_get_label_decl(actual->u.label);    
    tmp0 = build_v(GOTO_EXPR, tmp0);       
    g95_add_expr_to_block(&block, tmp0);        
  } 
 
  tmp0 = g95_finish_block(&block);    
  return build_v(SWITCH_EXPR, variable, tmp0, NULL_TREE); 
}  
  
    
    
/* trans_if()-- Translate an IF statement.  */         
         
static tree trans_if(g95_code *c) {          
tree then_stmt, else_stmt, predicate, tmp0;
stmtblock_t body;          
g95_se se;  
  
  g95_init_se(&se, NULL);    
  g95_init_block(&body);

  g95_conv_expr(&se, c->expr);     
     
  g95_add_block_to_block(&body, &se.pre);

  then_stmt = g95_trans_code(c->block);          
  if (then_stmt == NULL_TREE) then_stmt = empty_stmt_node;          
          
  else_stmt = g95_trans_code(c->ext.block);        
  if (else_stmt == NULL_TREE) else_stmt = empty_stmt_node;

  /* If there is a post chain, we have to stuff the result into a
   * temporary variable, clean up, then take the branch based on the
   * variable. */     
     
  if (se.post.head == NULL_TREE) {    
    predicate = se.expr;       
  } else {    
    predicate = g95_create_var(boolean_type_node);   
    g95_add_modify_expr(&body, predicate, se.expr);          
          
    g95_add_block_to_block(&body, &se.post); 
  }        
        
  tmp0 = build_v(COND_EXPR, predicate, then_stmt, else_stmt);   
  g95_add_expr_to_block(&body, tmp0);         
         
  return g95_finish_block(&body); 
}          
          
          
 
 
/* trans_arithmetic_if()-- Translate an arithmetic IF statement.  */   
   
static tree trans_arithmetic_if(g95_code *codep) {    
tree tmp, branch1, branch2, zero;          
g95_se se1; 
 
  g95_init_se(&se1, NULL);          
  g95_init_block(&se1.pre);          
          
  g95_conv_expr(&se1, codep->expr); 
 
  zero = g95_build_const(TREE_TYPE(se1.expr), integer_zero_node);          
          
  branch1 = build_v(GOTO_EXPR, g95_get_label_decl(codep->label));       
  branch2 = build_v(GOTO_EXPR, g95_get_label_decl(codep->label2));    
    
  tmp = build(LT_EXPR, boolean_type_node, se1.expr, zero);   
  branch1 = build_v(COND_EXPR, tmp, branch1, branch2);       
  branch2 = build_v(GOTO_EXPR, g95_get_label_decl(codep->label3)); 
  tmp = build(LE_EXPR, boolean_type_node, se1.expr, zero);        
  branch1 = build_v(COND_EXPR, tmp, branch1, branch2);         
         
  g95_add_expr_to_block(&se1.pre, branch1);         
         
  return g95_finish_block(&se1.pre);      
}


       
       
/* trans_do()-- Translate a DO statement.  Currently calculates
 * the loop count before entering the loop, but it may be possible to
 * optimize if step is a constant. The main advantage is that the loop
 * test is a single SIMPLE node.

   We translate a do loop from:

   DO dovar = from, to, step
      body
   END DO

   to:

   pre_dovar;
   pre_from;
   pre_to;
   pre_step;
   temp1=to_expr-from_expr;
   step_temp=step_expr;
   range_temp=step_tmp/range_temp;
   for ( ; range_temp > 0 ; range_temp = range_temp - 1)
     {
       body;
cycle_label:
       dovar_temp = dovar
       dovar=dovar_temp + step_temp;
     }
exit_label:

   Some optimization is done for empty do loops. We can't just let
   dovar=to because it's possible for from+range*loopcount!=to.  Anyone
   who writes empty DO deserves sub-optimal (but correct) code anyway.

   TODO: Large loop counts
   Does not work loop counts which do not fit into a signed integer kind,
   ie. Does not work for loop counts > 2^31 for integer(kind=4) variables
   We must support the full range.

   TODO: add save_expr() and insert post code.  */

static tree trans_do(g95_code *code) {        
tree dovar, f, t, step, cnt, dtype, cond, cycle_label, exit_label, tmp;         
stmtblock_t block, b;          
g95_se s; 
 
  g95_init_block(&block);    
    
  /* Create SIMPLE versions of all expressions.  */     
  g95_init_se(&s, NULL);      
  g95_conv_expr(&s, code->ext.iterator->var);
  g95_add_block_to_block(&block, &s.pre);  
  dovar = s.expr;       
  dtype = TREE_TYPE(dovar); 
 
  g95_init_se(&s, NULL);   
  g95_conv_expr_type(&s, code->ext.iterator->start, dtype);          
  g95_add_block_to_block(&block, &s.pre);   
  f = s.expr;

  g95_init_se(&s, NULL);  
  g95_conv_expr_type(&s, code->ext.iterator->end, dtype);
  g95_add_block_to_block(&block, &s.pre);   
  t = s.expr;         
         
  g95_init_se(&s, NULL);
  g95_conv_expr_type(&s, code->ext.iterator->step, dtype);

  g95_add_block_to_block(&block, &s.pre);       
  step = s.expr;         
         
  /* Initialize the trip count.  This code is executed before we enter
   * the loop body.  We generate: count = (to + step - from) / step.  */  
  
  tmp = fold(build(MINUS_EXPR, dtype, step, f));   
  tmp = fold(build(PLUS_EXPR, dtype, t, tmp));       
  tmp = fold(build(TRUNC_DIV_EXPR, dtype, tmp, step));

  cnt = g95_create_var(dtype);      
  g95_add_modify_expr(&block, cnt, tmp); 
 
  /* Initialize the DO variable: dovar = from.  */       
  g95_add_modify_expr(&block, dovar, f);

  /* Loop body */    
  g95_init_block(&b);       
       
  /* Cycle and exit statements are implemented with gotos.  Put these
   * labels where they can be found later. We put the labels in a
   * TREE_LIST node (because TREE_CHAIN is already used). cycle_label
   * goes in TREE_PURPOSE (backend_decl), exit label in TREE_VALUE
   * (backend_decl).  */  
  
  cycle_label = g95_build_label_decl(NULL_TREE);         
  exit_label  = g95_build_label_decl(NULL_TREE);    
  code->backend_decl = tree_cons(cycle_label, exit_label, NULL);     
     
  /* Start with the loop condition.  Loop until trip count <= 0.  */          
  cond = build(LE_EXPR, boolean_type_node, cnt, integer_zero_node);       
  tmp = build_v(GOTO_EXPR, exit_label);       
  TREE_USED(exit_label) = 1;   
  tmp = build_v(COND_EXPR, cond, tmp, empty_stmt_node);
  g95_add_expr_to_block(&b, tmp);        
        
        
  /* Main loop body. */    
  tmp = g95_trans_code(code->block);   
  g95_add_expr_to_block(&b, tmp);   
   
  /* Label for cycle statements (if needed). */    
  if (TREE_USED(cycle_label)) {      
    tmp = build_v(LABEL_EXPR, cycle_label);  
    g95_add_expr_to_block(&b, tmp);   
  }          
          
  /* Increment the loop variable. */   
  tmp = build(PLUS_EXPR, dtype, dovar, step);
  g95_add_modify_expr(&b, dovar, tmp);          
          
  /* Decrement the trip count. */  
  tmp = build(MINUS_EXPR, dtype, cnt, integer_one_node);         
  g95_add_modify_expr(&b, cnt, tmp);

  /* End of loop body. */    
  tmp = g95_finish_block(&b); 
 
  /* The for loop itself. */        
  tmp = build_v(LOOP_EXPR, tmp);   
  g95_add_expr_to_block(&block, tmp);

  /* Add the exit label */          
  tmp = build_v(LABEL_EXPR, exit_label);  
  g95_add_expr_to_block(&block, tmp);       
       
  return g95_finish_block(&block);         
}         
         
         
     
     
/* trans_return()-- Translate a RETURN statement */

static tree trans_return(g95_code *c) {      
stmtblock_t blk;     
g95_se se0;   
tree tmp;          
          
  g95_init_block(&blk);    
    
  if (c->expr != NULL) {     
    g95_init_se(&se0, NULL);         
    g95_conv_expr(&se0, c->expr);

    g95_add_block_to_block(&blk, &se0.pre);   
    g95_add_modify_expr(&blk, g95_result_var_decl, se0.expr);          
    g95_add_block_to_block(&blk, &se0.post);       
  }  
  
  tmp = build_v(GOTO_EXPR, g95_get_return_label());          
  g95_add_expr_to_block(&blk, tmp);    
    
  return g95_finish_block(&blk);          
}  
  
  
    
    
/* trans_exit()-- EXIT a DO loop.  Similar to CYCLE, but now the
 * label is in TREE_VALUE(backend_decl) of the g95_code node at the
 * head of the loop. */     
     
static tree trans_exit(g95_code *c) {         
tree exit_label; 
 
  exit_label = TREE_VALUE(c->ext.block->backend_decl);
  TREE_USED(exit_label) = 1; 
  return build_v(GOTO_EXPR, exit_label);       
}      
      
      
         
         
/* trans_pause()-- Translate a PAUSE statement */  
  
static tree trans_pause(g95_code *c) {          
stmtblock_t block;
g95_se s;       
tree tmp;       
bt type;        
        
  g95_init_block(&block);      
      
  if (c->expr == NULL) {     
    tmp = g95_call_library(void_type_node, PREFIX "pause", NULL_TREE); 
    g95_add_expr_to_block(&block, tmp);   
  } else {
    type = c->expr->ts.type;   
    g95_init_se(&s, NULL);

    if (type == BT_CHARACTER) s.reflevel = 1;   
    g95_conv_expr(&s, c->expr);       
    g95_add_block_to_block(&block, &s.pre);

    if (type == BT_INTEGER)  
      tmp = g95_call_library(void_type_node, PREFIX "pause_integer", s.expr, 
			     NULL);     
    else        
      tmp = g95_call_library(void_type_node, PREFIX "pause_string", s.expr,     
			     s.string_length, NULL);     
     
    g95_add_expr_to_block(&block, tmp);     
    g95_add_block_to_block(&block, &s.post); 
  } 
 
  return g95_finish_block(&block);    
}          
          
          
    
    
/* character_select()-- Implement a character SELECT statement.  We
 * generate an array of structures describing the cases in order and
 * call a library subroutine that locates the right case.  The library
 * subroutine returns a pointer to jump to or NULL if no branches are
 * to be taken. */       
       
static tree character_select(g95_code *codep, g95_expr *selector) {  
tree init, node, end_label, l, t, *labels, default_jump; 
stmtblock_t blk, list;         
g95_case *cp, *q;
g95_se se1, s;       
g95_code *r;        
int o, n;          
          
static tree select_struct, ss_string1, ss_string1_len,       
            ss_string2, ss_string2_len, ss_target;  
  
  if (select_struct == NULL) {       
    select_struct = make_node(RECORD_TYPE);          
          
    TYPE_NAME(select_struct) = g95_unique_identifier("jump_struct");   
     
    ss_string1_len = g95_add_field(select_struct, "string1_len",         
				   g95_default_integer);      
    ss_string1     = g95_add_field(select_struct, "string1", pchar_type_node);

    ss_string2_len = g95_add_field(select_struct, "string2_len",      
				   g95_default_integer);
    ss_string2     = g95_add_field(select_struct, "string2", pchar_type_node);

    ss_target      = g95_add_field(select_struct, "target", pvoid_type_node);  
  
    g95_finish_type(select_struct);     
  }   
   
  cp = codep->block->ext.case_list;        
  while(cp->cprev != NULL)      
    cp = cp->cprev;     
     
  n = 0;        
  for(q=cp; q; q=q->cnext)    
    q->n = n++;       
       
  labels = NULL;  
  if (n != 0) labels = g95_getmem(n*sizeof(tree));  
  
  for(o=0; o<n; o++) 
    labels[o] = g95_build_label_decl(NULL_TREE);          
          
  end_label = g95_build_label_decl(NULL_TREE); 
 
/* Generate the body */         
         
  g95_init_block(&blk);       
  g95_init_block(&list);    
  g95_init_se(&s, NULL);     
     
  for(r=codep->block; r; r=r->block) {    
    for(q=r->ext.case_list; q; q=q->next) {         
      t = build_v(LABEL_EXPR, labels[q->n]);        
      g95_add_expr_to_block(&list, t);          
    }         
         
    t = g95_trans_code(r->next);          
    g95_add_expr_to_block(&list, t);     
     
    t = build_v(GOTO_EXPR, end_label);         
    g95_add_expr_to_block(&list, t);       
  }        
        
/* Generate the structure describing the branches */    
    
  init = NULL_TREE;        
        
  o = 0;      
  for(q=cp; q; q=q->cnext, o++) {          
    node = NULL_TREE;    
    
    if (q->low == NULL) {     
      node = tree_cons(ss_string1_len, integer_zero_node, node); 
      node = tree_cons(ss_string1, null_pointer_node, node);        
    } else {     
      g95_conv_expr(&s, q->low);  
      t = build1(ADDR_EXPR, pchar_type_node, s.expr);    
    
      node = tree_cons(ss_string1_len, s.string_length, node);    
      node = tree_cons(ss_string1, t, node);     
    }       
       
    if (q->high == NULL) {     
      node = tree_cons(ss_string2_len, integer_zero_node, node);     
      node = tree_cons(ss_string2, null_pointer_node, node);    
    } else {         
      g95_conv_expr(&s, q->high);      
      t = build1(ADDR_EXPR, pchar_type_node, s.expr);   
   
      node = tree_cons(ss_string2_len, s.string_length, node);    
      node = tree_cons(ss_string2, t, node);    
    }      
      
    t = build1(ADDR_EXPR, pvoid_type_node, labels[o]);          
    node = tree_cons(ss_target, t, node);     
     
    t = build(CONSTRUCTOR, select_struct, NULL_TREE, nreverse(node));         
    init = tree_cons(NULL_TREE, t, init);  
  }   
   
  t = build_array_type(select_struct, build_index_type(build_int_2(n-1, 0)));          
          
  init = build(CONSTRUCTOR, t, NULL_TREE, nreverse(init));          
  TREE_CONSTANT(init) = 1;      
  TREE_STATIC(init) = 1;

  /* Build an argument list for the library call */    
    
  init = build1(ADDR_EXPR, pvoid_type_node, init);
  l = build_int_2(n, 0);         
  default_jump = build1(ADDR_EXPR, pvoid_type_node, end_label);       
       
  g95_init_se(&se1, NULL);  
  se1.reflevel = 1;   
  g95_conv_expr(&se1, selector);          
          
  g95_add_block_to_block(&blk, &se1.pre);

  t = g95_call_library(pvoid_type_node, PREFIX "select_string",       
			 init, l, default_jump,
			 se1.expr, se1.string_length, NULL_TREE);  
  
  t = build1(GOTO_EXPR, void_type_node, t);   
  g95_add_expr_to_block(&blk, t);   
   
  t = g95_finish_block(&list); 
  g95_add_expr_to_block(&blk, t);       
       
  t = build_v(LABEL_EXPR, end_label);      
  g95_add_expr_to_block(&blk, t);

  if (n != 0) g95_free(labels);  
  
  return g95_finish_block(&blk);        
}    
    
    
        
        
/* allocate_scalar()-- Allocate a scalar object. */        
        
static void allocate_scalar(g95_expr *exp, g95_se *s, tree stat_var) {     
tree length, tmp1; 
g95_se se0;   
   
  g95_init_se(&se0, NULL);  
  se0.reflevel = 2;       
       
  g95_conv_expr(&se0, exp);          
  g95_add_block_to_block(&s->pre, &se0.pre);         
  g95_add_block_to_block(&s->post, &se0.post);      
      
  if (exp->ts.type == BT_CHARACTER) {
    se0.reflevel = 0;       
    length = g95_conv_char_length(&se0, &exp->ts);          
          
    tmp1 = g95_call_library(void_type_node, PREFIX "allocate_string",      
			   se0.expr, length, stat_var, NULL_TREE); 
  } else { 
    length = size_in_bytes(g95_typenode_for_spec(&exp->ts));         
    tmp1 = g95_call_library(void_type_node, PREFIX "allocate_scalar",   
			   se0.expr, length, stat_var, NULL_TREE);          
  }          
          
  g95_add_expr_to_block(&s->pre, tmp1);          
}       
       
       
     
     
/* trans_cycle()-- CYCLE a DO loop. The label decl has already
 * been created by g95_trans_do(), it's in TREE_PURPOSE(backend_decl)
 * of the g95_code node at the head of the loop. We must mark the
 * label as used. */   
   
static tree trans_cycle(g95_code *c) {       
tree cycle_label;   
   
  cycle_label = TREE_PURPOSE(c->ext.block->backend_decl);       
  TREE_USED(cycle_label) = 1;       
  return build_v(GOTO_EXPR, cycle_label);          
}       
       
       
        
        
/* deallocate_char_pointer_array()-- Deallocate a character pointer array. */

static tree deallocate_char_pointer_array(g95_se *se1, g95_expr *exp) {
g95_se se0;  
  
  g95_init_se(&se0, NULL);  
  
  se0.reflevel = 2;          
  g95_conv_expr(&se0, exp);    
    
  g95_add_block_to_block(&se1->pre, &se0.pre);  
  g95_add_block_to_block(&se1->post, &se0.post);        
        
  return se0.expr;    
}     
     
     
        
        
/* trans_do_while()-- Translate a DO WHILE statement.  By the time we
 * get here, this statement is always an infinite loop with an exit
 * statement up front. */ 
 
static tree trans_do_while(g95_code *code) {
tree tmp, cycle_label, exit_label;         
stmtblock_t body;     
     
  g95_init_block(&body);    
    
  cycle_label = g95_build_label_decl(NULL_TREE);        
  exit_label  = g95_build_label_decl(NULL_TREE);

  code->backend_decl = tree_cons(cycle_label, exit_label, NULL);          
          
  tmp = g95_trans_code(code->block);
  g95_add_expr_to_block(&body, tmp); 
 
  if (TREE_USED(cycle_label)) {          
    tmp = build_v(LABEL_EXPR, cycle_label);   
    g95_add_expr_to_block(&body, tmp);  
  } 
 
  tmp = g95_finish_block(&body);         
  g95_init_block(&body);       
       
  tmp = build_v(LOOP_EXPR, tmp);      
  g95_add_expr_to_block(&body, tmp);         
         
  tmp = build_v(LABEL_EXPR, exit_label);         
  g95_add_expr_to_block(&body, tmp); 
 
  return g95_finish_block(&body);      
}        
        
        
       
       
/* trans_call()-- Translate a CALL statement */

static tree trans_call(g95_code *c) {      
tree tmp, variable, arg, nm, decl;        
g95_se se;

  g95_init_se(&se, NULL);   
  g95_init_block(&se.pre);      
      
  /* If the sub_name member is nonnull, it is the thing to call.
   * Otherwise call the symbol. */      
      
  if (c->sub_name == NULL) {   
    decl = c->sym->backend_decl;      
      
    if (POINTER_TYPE_P(TREE_TYPE(decl)))         
      decl = build1(INDIRECT_REF, TREE_TYPE(TREE_TYPE(decl)), decl);   
   
  } else { 
    nm = get_identifier(c->sub_name);        
    decl = build_function_type(void_type_node, NULL_TREE); 
    decl = build_decl(FUNCTION_DECL, nm, decl);      
      
    DECL_EXTERNAL(decl) = 1;  
    TREE_PUBLIC(decl) = 1;         
         
    pushdecl(decl);
    rest_of_decl_compilation(decl, NULL, 1, 0);      
  }

  arg = g95_trans_arglist(c->ext.actual, &se);  
  tmp = g95_build_function_call(decl, arg);       
       
  if (!g95_has_alt_return(c->ext.actual)) {   
    g95_add_expr_to_block(&se.pre, tmp);        
    g95_add_block_to_block(&se.pre, &se.post); 
  } else {         
    variable = g95_create_var(g95_default_integer);     
    g95_add_modify_expr(&se.pre, variable, tmp);   
   
    g95_add_block_to_block(&se.pre, &se.post);          
          
    tmp = alt_return_jump(variable, c->ext.actual);   
    g95_add_expr_to_block(&se.pre, tmp);        
  }  
  
  return g95_finish_block(&se.pre);      
}         
         
         
        
        
/* init_section_info()-- Initialize the section_info[] array with
 * information about the array being allocated.  The first integer is
 * the rank, the second is the element size.  This is followed by a
 * pair of integers for each dimension that give the upper and lower
 * bound. */  
  
static void init_section_info(g95_se *s, g95_array_ref *spec, g95_expr *e2) {   
g95_typespec *ts;    
g95_se se0;    
tree tmp1;
int i, y;         
         
  y = 0;      
  ts = &e2->ts;    
  g95_set_section_info(s, y++, build_int_2(spec->dimen, 0));  
  
  tmp1 = (ts->type != BT_CHARACTER) 
    ? size_in_bytes(g95_typenode_for_spec(ts))    
    : ts->cl->backend_decl;  
  
  g95_set_section_info(s, y++, tmp1);    
    
  for(i=0; i<spec->dimen; i++) {       
    switch(spec->dimen_type[i]) {          
    case DIMEN_ELEMENT:          
      g95_set_section_info(s, y++, integer_one_node);         
         
      g95_init_se(&se0, NULL);       
      g95_conv_expr(&se0, spec->start[i]);       
       
      g95_add_block_to_block(&s->pre, &se0.pre);     
      g95_set_section_info(s, y++, se0.expr);      
      g95_add_block_to_block(&s->post, &se0.post); 
 
      break;     
     
    case DIMEN_RANGE:    
      g95_init_se(&se0, NULL);
      g95_conv_expr(&se0, spec->start[i]);    
    
      g95_add_block_to_block(&s->pre, &se0.pre);       
      g95_set_section_info(s, y++, se0.expr);
      g95_add_block_to_block(&s->post, &se0.post);       
       
      g95_init_se(&se0, NULL); 
      g95_conv_expr(&se0, spec->end[i]);         
         
      g95_add_block_to_block(&s->pre, &se0.pre);   
      g95_set_section_info(s, y++, se0.expr);       
      g95_add_block_to_block(&s->post, &se0.post);        
        
      break;

    default: 
      g95_internal_error("init_section_info(): Bad array ref");     
    }  
  }     
}       
       
       
      
      
/* trans_goto()-- Translate a GOTO statement */     
     
static tree trans_goto(g95_code *codep) {          
          
  return build_v(GOTO_EXPR, g95_get_label_decl(codep->label));    
}  
  
  
       
       
/* integer_select()-- Build an integer selection statement */   
   
static tree integer_select(g95_code *codep, g95_expr *selector) {    
tree end_label, ulow_label, uhigh_label, tmp0, low, high, goto_expr;         
g95_case *cp, *unbounded_low, *unbounded_high;     
stmtblock_t block, b;
g95_code *u;    
g95_se s;         
int knd;        
        
  g95_init_block(&block);      
      
  g95_init_se(&s, NULL); 
  g95_conv_expr(&s, selector);    
    
  knd = selector->ts.kind;        
        
  end_label = g95_build_label_decl(NULL_TREE);

  g95_init_block(&b);

  /* Look for the unbounded low and unbounded high cases. */   
   
  unbounded_low = NULL;     
  unbounded_high = NULL;         
         
  ulow_label = NULL_TREE;      
  uhigh_label = NULL_TREE;        
        
  cp = codep->block->ext.case_list;  
  
  while(cp->cnext != NULL)        
    cp = cp->cnext;  
  
  if (cp->high == NULL) {        
    unbounded_low = cp;          
    ulow_label = g95_build_label_decl(NULL_TREE); 
  }    
    
  cp = codep->block->ext.case_list;       
       
  while(cp->cprev != NULL)        
    cp = cp->cprev;       
       
  /* The unbounded high is the first or second element */        
        
  if (cp->low == NULL && cp->high != NULL)   
    unbounded_high = cp;   
  else {          
    cp = cp->cnext;         
    if (cp != NULL && cp->low == NULL && cp->high != NULL) unbounded_high = cp;
  }       
       
  if (unbounded_high != NULL) uhigh_label = g95_build_label_decl(NULL_TREE);          
          
  if (unbounded_high != NULL || unbounded_low != NULL)        
    s.expr = save_expr(s.expr);   
   
  g95_add_block_to_block(&block, &s.pre);    
    
/* Build branch statements to the unbounded cases */  
  
  if (unbounded_high != NULL) {
    high = g95_conv_mpz_to_tree(unbounded_high->high->value.integer, knd); 
    tmp0 = build(LE_EXPR, boolean_type_node, s.expr, high);     
     
    goto_expr = build_v(GOTO_EXPR, uhigh_label);         
         
    tmp0 = build_v(COND_EXPR, tmp0, goto_expr, empty_stmt_node);     
    g95_add_expr_to_block(&block, tmp0);         
  }          
          
  if (unbounded_low != NULL) {  
    low = g95_conv_mpz_to_tree(unbounded_low->low->value.integer, knd);
    tmp0 = build(GE_EXPR, boolean_type_node, s.expr, low);      
      
    goto_expr = build_v(GOTO_EXPR, ulow_label);    
    
    tmp0 = build_v(COND_EXPR, tmp0, goto_expr, empty_stmt_node);    
    g95_add_expr_to_block(&block, tmp0);   
  }         
         
  /* Build the body */ 
 
  for(u=codep->block; u; u=u->block) {   
    for(cp=u->ext.case_list; cp; cp=cp->next) { 
      if (cp == unbounded_low) {         
	tmp0 = build_v(LABEL_EXPR, ulow_label);        
	g95_add_expr_to_block(&b, tmp0);        
	continue;   
      }      
      
      if (cp == unbounded_high) {
	tmp0 = build_v(LABEL_EXPR, uhigh_label);  
	g95_add_expr_to_block(&b, tmp0);    
	continue; 
      }

      if (cp->low == NULL && cp->high == NULL)  /* Case DEFAULT.  */    
	low = high = NULL_TREE;      
      else {          
	low = g95_conv_mpz_to_tree(cp->low->value.integer, knd); 
 
	if (cp->low == cp->high)
	  high = NULL_TREE;         
	else     
	  high = g95_conv_mpz_to_tree(cp->high->value.integer, knd);          
      } 
 
      /* Add this case label.  */         
         
      tmp0 = build_v(CASE_LABEL_EXPR, low, high);       
      g95_add_expr_to_block(&b, tmp0); 
    }   
   
    /* Add the statements for this case.  */         
         
    tmp0 = g95_trans_code(u->next);     
    g95_add_expr_to_block(&b, tmp0);          
          
    /* Break to the end of the loop. */       
       
    tmp0 = build_v(GOTO_EXPR, end_label);     
    g95_add_expr_to_block(&b, tmp0);          
  }       
       
  tmp0 = g95_finish_block(&b);  
  tmp0 = build_v(SWITCH_EXPR, s.expr, tmp0, NULL_TREE);   
  g95_add_expr_to_block(&block, tmp0);          
          
  tmp0 = build_v(LABEL_EXPR, end_label);      
  g95_add_expr_to_block(&block, tmp0);   
   
  return g95_finish_block(&block);  
}          
          
          
  
  
/* allocate_pointer_array()-- Generate code to allocate a pointer
 * array. */   
   
static void allocate_pointer_array(g95_expr *e2, g95_se *se0, 
				   g95_array_ref *as, tree stat_var) {   
tree tmp0;          
          
  init_section_info(se0, as, e2);  
  
  se0->reflevel = 2;         
  g95_conv_expr(se0, e2);       
       
  tmp0 = g95_call_library(pvoid_type_node, PREFIX "allocate_pointer_array",  
			 se0->expr, stat_var, NULL_TREE);      
      
  g95_add_expr_to_block(&se0->pre, tmp0);     
}       
       
       
       
       
/* trans_stop()-- Translate a STOP statement */      
      
static tree trans_stop(g95_code *c) {   
g95_se se1;      
tree tmp;    
    
  g95_init_se(&se1, NULL); 
  g95_init_block(&se1.pre);  
  
  if (c->expr == NULL) {     
    tmp = build_int_2(c->ext.stop_code, 0);
    tmp = g95_call_library(void_type_node, PREFIX "stop_numeric",      
			   tmp, NULL_TREE); 
  } else {        
    se1.reflevel = 1;
    g95_conv_expr(&se1, c->expr);  
  
    tmp = g95_call_library(void_type_node, PREFIX "stop_string",     
			   se1.expr, se1.string_length, NULL_TREE);       
  }        
        
  g95_set_error_locus(&se1.pre, &c->where);      
  g95_add_expr_to_block(&se1.pre, tmp);  
  
  return g95_finish_block(&se1.pre);          
}       
       
       
     
     
/* logical_select()-- Build a logical selection statement. */         
         
static tree logical_select(g95_code *code, g95_expr *selector) {       
g95_case *m, *true_case, *false_case, *default_case;  
stmtblock_t list, b;     
tree tmp0, end_label;   
g95_se s;    
    
  true_case = false_case = default_case = NULL;       
       
  m = code->block->ext.case_list;  
  
  while(m->cprev != NULL)    
    m = m->cprev;         
         
  /* Extract out the cases */     
     
  for(; m; m=m->cnext) {        
    if (m->low == NULL && m->high == NULL) {         
      default_case = m;         
      continue; 
    }         
         
    if (m->low->value.logical)     
      true_case = m;      
    else     
      false_case = m;     
  } 
 
  g95_init_block(&list);   
  g95_init_block(&b);          
          
  g95_init_se(&s, NULL);      
  g95_conv_expr(&s, selector);   
   
  end_label = g95_build_label_decl(NULL_TREE);   
   
  if (false_case != NULL) {       
    tmp0 = build_v(CASE_LABEL_EXPR, integer_zero_node, NULL_TREE); 
    g95_add_expr_to_block(&b, tmp0);          
          
    tmp0 = g95_trans_code(false_case->code);
    g95_add_expr_to_block(&b, tmp0);      
      
    tmp0 = build_v(GOTO_EXPR, end_label);          
    g95_add_expr_to_block(&b, tmp0);        
  }         
         
  if (true_case != NULL) {          
    tmp0 = build_v(CASE_LABEL_EXPR, integer_one_node, NULL_TREE);          
    g95_add_expr_to_block(&b, tmp0);     
     
    tmp0 = g95_trans_code(true_case->code);     
    g95_add_expr_to_block(&b, tmp0);    
    
    tmp0 = build_v(GOTO_EXPR, end_label);       
    g95_add_expr_to_block(&b, tmp0);        
  }         
         
  if (default_case != NULL) {     
    tmp0 = build_v(CASE_LABEL_EXPR, NULL_TREE, NULL_TREE);    
    g95_add_expr_to_block(&b, tmp0);      
      
    tmp0 = g95_trans_code(default_case->code);    
    g95_add_expr_to_block(&b, tmp0);  
  
    tmp0 = build_v(GOTO_EXPR, end_label);       
    g95_add_expr_to_block(&b, tmp0);     
  }      
      
  tmp0 = g95_finish_block(&b);        
  tmp0 = build_v(SWITCH_EXPR, convert(g95_default_integer, s.expr),      
		tmp0, NULL_TREE);    
    
  g95_add_expr_to_block(&list, tmp0);        
        
  tmp0 = build_v(LABEL_EXPR, end_label);         
  g95_add_expr_to_block(&list, tmp0);     
     
  return g95_finish_block(&list);    
}   
   
   
      
      
/* static_alloc_symbol()-- Determine if an allocatable array is in
 * static memory or not. */    
    
static int static_alloc_symbol(g95_symbol *symb) {    
    
  return symb->attr.save || symb->attr.use_assoc ||    
    g95_module_symbol(symb) || symb->attr.artificial;        
}      
      
      
   
   
/* trans_select()-- Translate a SELECT block */ 
 
static tree trans_select(g95_code *cp) {        
g95_expr *e2;       
tree tmp; 
 
  /* Normal select statements put the condition in expr, computed GOTO
   * statements put it in expr2. */        
        
  e2 = (cp->expr == NULL) ? cp->expr2 : cp->expr;     
     
  switch(e2->ts.type) {      
  case BT_INTEGER:    tmp = integer_select(cp, e2);    break;
  case BT_LOGICAL:    tmp = logical_select(cp, e2);    break;     
  case BT_CHARACTER:  tmp = character_select(cp, e2);  break;   
  default:          
    g95_internal_error("g95_trans_select(): Bad type");    
    tmp = NULL_TREE;       
  }        
        
  return tmp;      
}   
   
   
  
  
/* allocate_allocatable_array()-- Generate code to allocate a pointer
 * array.  The section_info[] array is initialized in the same manner
 * as for allocate_pointer_array().  The calling sequence is different
 * as we pass the address of the descriptor. */  
  
static void allocate_allocatable_array(g95_expr *e2, g95_se *se0,   
				       g95_array_ref *ar, tree stat_var) {     
tree t;  
  
  init_section_info(se0, ar, e2); 
 
  se0->reflevel = 1; 
  g95_conv_expr(se0, e2);      
      
  t = build_int_2(static_alloc_symbol(e2->symbol), 0);   
  t = g95_call_library(void_type_node, PREFIX "allocate_allocatable_array",         
			 se0->expr, t, stat_var, NULL_TREE);   
   
  g95_add_expr_to_block(&se0->pre, t);     
}       
       
       
 
 
/* trans_allocate()-- Translate an ALLOCATE statement.  If the
 * statement has a STAT variable, we zero it, then generate statements
 * that initialize the right descriptors followed by allocate_array()
 * calls for arrays or just simple allocate_scalar() calls.  The
 * library calls update STAT if something goes wrong, or does nothing
 * if the STAT variable is already nonzero.  If something goes wrong
 * and there is no STAT variable, the program is terminated. */         
         
static tree trans_allocate(g95_code *c) {        
tree tmp0, stat_var;         
stmtblock_t blk;    
g95_ref *z, *r;         
g95_alloc *alloc;
g95_expr *e1;       
g95_se s;         
         
  g95_init_se(&s, NULL);    
  g95_set_error_locus(&s.pre, &c->ext.alloc_list->expr->where);

  if (c->expr == NULL)       
    stat_var = null_pointer_node;     
  else {        
    s.reflevel = 1;   
    g95_conv_expr(&s, c->expr);  
    stat_var = save_expr(s.expr);     
     
    /* Init the status to success */  
  
    tmp0 = build1(INDIRECT_REF, g95_default_integer, stat_var);    
    g95_add_modify_expr(&s.pre, tmp0, integer_zero_node);
  }   
   
  /* Allocate expressions are messed up in the sense that the
   * specification is stored as an array reference.  This tail
   * reference is temporarily pruned from the reference list when
   * actually generating code so that we get the pointer to the array
   * or the allocatable array itself. */          
          
  for(alloc=c->ext.alloc_list; alloc; alloc=alloc->next) {
    e1 = alloc->expr;        
        
    if (e1->ref == NULL) {      
      allocate_scalar(e1, &s, stat_var);         
      continue;     
    }  
  
    z = NULL;     
     
    for(r=e1->ref; r; r=r->next) { 
      if (r->type == REF_ARRAY && r->next == NULL) break;
      z = r;   
    }   
   
    if (r == NULL) {  /* Scalar pointer */      
      allocate_scalar(e1, &s, stat_var);        
      continue;    
    } 
 
    if (e1->ref == r) {          
      e1->ref = NULL;

      if (e1->symbol->attr.allocatable)         
	allocate_allocatable_array(e1, &s, &r->u.ar, stat_var);   
      else    
	allocate_pointer_array(e1, &s, &r->u.ar, stat_var); 
 
      e1->ref = r;     
    } else {     
      z->next = NULL;        
      allocate_pointer_array(e1, &s, &r->u.ar, stat_var);          
      z->next = r;       
    }  
  }      
      
  g95_init_block(&blk);  
  
  g95_add_block_to_block(&blk, &s.pre);     
  g95_add_block_to_block(&blk, &s.post);  
  
  return g95_finish_block(&blk);      
}       
       
       
     
     
/* deallocate_noncp_array()-- Deallocate a entity that is not a
 * character pointer array. */         
         
static tree deallocate_noncp_array(g95_se *s, g95_expr *expr) { 
g95_se se2;        
        
  g95_init_se(&se2, NULL);
  se2.reflevel = 2;         
  g95_conv_expr(&se2, expr); 
 
  g95_add_block_to_block(&s->pre, &se2.pre);     
  g95_add_block_to_block(&s->post, &se2.post);  
  
  return se2.expr;     
}   
   
   
       
       
/* deallocate_allocatable()-- Deallocate an allocatable array */   
   
static void deallocate_allocatable(g95_se *se, g95_expr *e, tree stat_var) {  
g95_se s;  
tree tmp0;     
     
  g95_init_se(&s, NULL);         
  s.reflevel = 1;          
  g95_conv_expr(&s, e);         
         
  g95_add_block_to_block(&se->pre, &s.pre);          
  g95_add_block_to_block(&se->post, &s.post);   
   
  tmp0 = static_alloc_symbol(e->symbol)          
    ? integer_one_node        
    : integer_zero_node;  
  
  tmp0 = g95_call_library(void_type_node, PREFIX "deallocate_allocatable", 
			 s.expr, tmp0, stat_var, NULL_TREE);      
      
  g95_add_expr_to_block(&se->pre, tmp0);
}          
          
          
    
    
static void deallocate_pointer(int cp_array, g95_se *se1, g95_expr *expr,  
			       tree stat_var) {  
tree t, pointer;        
        
  pointer = cp_array
    ? deallocate_char_pointer_array(se1, expr)
    : deallocate_noncp_array(se1, expr);       
       
  t = g95_call_library(void_type_node, PREFIX "deallocate_pointer",        
			 pointer, stat_var, NULL_TREE);

  g95_add_expr_to_block(&se1->pre, t); 
}        
        
        
    
    
/* trans_deallocate()-- Translate a DEALLOCATE statement */          
          
static tree trans_deallocate(g95_code *codep) {          
symbol_attribute attr;          
tree t, stat_var;         
stmtblock_t list;          
g95_alloc *alloc;   
g95_typespec ts;
g95_expr *e2;        
int cp_flag;
g95_se s;     
     
  g95_init_se(&s, NULL);       
  g95_set_error_locus(&s.pre, &codep->ext.alloc_list->expr->where);  
  
  if (codep->expr == NULL)     
    stat_var = null_pointer_node;        
  else {   
    s.reflevel = 1;   
    g95_conv_expr(&s, codep->expr);     
    stat_var = save_expr(s.expr);    
    
    t = build1(INDIRECT_REF, g95_default_integer, stat_var);      
    g95_add_modify_expr(&s.pre, t, integer_zero_node);
  } 
 
  for(alloc=codep->ext.alloc_list; alloc; alloc=alloc->next) {        
    e2 = alloc->expr; 
    attr = g95_variable_attr(e2, &ts);   
   
    if (e2->symbol->attr.allocatable && e2->ref->type == REF_ARRAY &&  
	e2->ref->next == NULL)          
      deallocate_allocatable(&s, e2, stat_var); 
    else {         
      cp_flag = (e2->ts.type == BT_CHARACTER) && attr.pointer;   
      deallocate_pointer(cp_flag, &s, e2, stat_var);      
    }   
  }        
        
  g95_init_block(&list);    
    
  g95_add_block_to_block(&list, &s.pre);      
  g95_add_block_to_block(&list, &s.post);       
       
  return g95_finish_block(&list);     
} 
 
 
          
          
/* g95_trans_code()-- Translate a list of executable statements. */  
  
tree g95_trans_code(g95_code *c) {   
stmtblock_t list; 
tree d;   
   
  if (c == NULL) return NULL_TREE;    
    
  g95_init_block(&list);     
     
  for(; c; c=c->next) {    
    g95_set_backend_locus(&c->where);      
    if (c->here != 0) {   
      d = build_v(LABEL_EXPR, g95_get_label_decl(c->here));        
      wrap_all_with_wfl(&d, input_filename, lineno);      
      g95_add_expr_to_block(&list, d);      
    }          
          
    switch (c->type) {   
    case EXEC_NOP:         
      d = NULL_TREE;   
      break;      
      
    case EXEC_ASSIGN:       
      d = g95_trans_assignment(c->expr, c->expr2);
      break;        
        
    case EXEC_POINTER_ASSIGN:          
      d = g95_trans_pointer_assign(c);        
      break;

    case EXEC_CONTINUE: 
      d = NULL_TREE;
      break;    
    
    case EXEC_CYCLE:     
      d = trans_cycle(c);        
      break; 
 
    case EXEC_EXIT:       
      d = trans_exit(c);       
      break; 
 
    case EXEC_GOTO:       
      d = trans_goto(c);       
      break;     
     
    case EXEC_STOP: 
      d = trans_stop(c);     
      break;        
        
    case EXEC_PAUSE:          
      d = trans_pause(c);   
      break;     
     
    case EXEC_CALL:         
      d = trans_call(c);    
      break;  
  
    case EXEC_RETURN:      
      d = trans_return(c);    
      break;        
        
    case EXEC_IF:         
      d = trans_if(c);          
      break;     
     
    case EXEC_ARITHMETIC_IF:      
      d = trans_arithmetic_if(c);      
      break;      
      
    case EXEC_DO: 
      d = trans_do(c); 
      break;

    case EXEC_DO_WHILE:       
      d = trans_do_while(c); 
      break;      
      
    case EXEC_SELECT:       
      d = trans_select(c);
      break;      
      
    case EXEC_FORALL:   
      g95_internal_error("g95_trans_code(): Unexpected FORALL");      
      break;

    case EXEC_WHERE:  
      g95_internal_error("g95_trans_code(): Unexpected WHERE");      
      break;   
   
    case EXEC_ALLOCATE:    
      d = trans_allocate(c);         
      break;          
          
    case EXEC_DEALLOCATE:       
      d = trans_deallocate(c);          
      break;      
      
    case EXEC_OPEN: 
      d = g95_trans_open(c); 
      break;         
         
    case EXEC_CLOSE:         
      d = g95_trans_close(c);  
      break;    
    
    case EXEC_READ:      
      d = g95_trans_read(c);      
      break;   
   
    case EXEC_WRITE:       
      d = g95_trans_write(c);     
      break;       
       
    case EXEC_IOLENGTH:
      d = g95_trans_iolength(c);     
      break;       
       
    case EXEC_BACKSPACE:     
      d = g95_trans_backspace(c);  
      break;     
     
    case EXEC_ENDFILE: 
      d = g95_trans_endfile(c);  
      break;      
      
    case EXEC_INQUIRE:          
      d = g95_trans_inquire(c);        
      break; 
 
    case EXEC_REWIND:     
      d = g95_trans_rewind(c);          
      break;    
    
    case EXEC_TRANSFER: 
      d = g95_trans_transfer(c);   
      break; 
 
    case EXEC_DT_END:
      d = g95_trans_dt_end(c);          
      break;       
       
    default: 
      g95_internal_error("g95_trans_code(): Bad statement code");       
    }    
    
    if (d != NULL && d != empty_stmt_node) {          
      wrap_all_with_wfl(&d, input_filename, lineno);          
      /* Add the new statemment to the block.  */     
      g95_add_expr_to_block(&list, d);
    }    
  }     
     
  /* Return the finished block. */     
  return g95_finish_block(&list);
}
