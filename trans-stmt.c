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
   
   
      
      
/* character_select()-- Implement a character SELECT statement.  We
 * generate an array of structures describing the cases in order and
 * call a library subroutine that locates the right case.  The library
 * subroutine returns a pointer to jump to or NULL if no branches are
 * to be taken. */         
         
static tree character_select(g95_code *codep, g95_expr *selector) {         
tree init, node, end_label, l, tmp0, *labels, default_jump;        
stmtblock_t blk, list;          
g95_case *cp, *k;   
g95_se se1, se0;         
g95_code *o;      
int b, m; 
 
static tree select_struct, ss_string1, ss_string1_len,    
            ss_string2, ss_string2_len, ss_target;       
       
  if (select_struct == NULL) {   
    select_struct = make_node(RECORD_TYPE);     
     
    TYPE_NAME(select_struct) = get_identifier("_jump_struct");       
         
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
         
  m = 0;        
  for(k=cp; k; k=k->cnext)      
    k->n = m++;        
        
  if (m != 0) labels = g95_getmem(m*sizeof(tree));   
   
  for(b=0; b<m; b++)       
    labels[b] = g95_build_label_decl(NULL_TREE);   
   
  end_label = g95_build_label_decl(NULL_TREE);    
    
/* Generate the body */  
  
  g95_start_block(&blk); 
  g95_init_block(&list);       
  g95_init_se(&se0, NULL);

  for(o=codep->block; o; o=o->block) {
    for(k=o->ext.case_list; k; k=k->next) {   
      tmp0 = build_v(LABEL_EXPR, labels[k->n]);
      g95_add_expr_to_block(&list, tmp0);      
    }         
         
    tmp0 = g95_trans_code(o->next);         
    g95_add_expr_to_block(&list, tmp0);    
    
    tmp0 = build_v(GOTO_EXPR, end_label); 
    g95_add_expr_to_block(&list, tmp0);          
  }     
     
/* Generate the structure describing the branches */   
   
  init = NULL_TREE;   
   
  b = 0;      
  for(k=cp; k; k=k->cnext, b++) {
    node = NULL_TREE;          
          
    if (k->low == NULL) {   
      node = tree_cons(ss_string1_len, integer_zero_node, node);         
      node = tree_cons(ss_string1, null_pointer_node, node);   
    } else {  
      g95_conv_expr(&se0, k->low);         
      tmp0 = build1(ADDR_EXPR, pchar_type_node, se0.expr);     
     
      node = tree_cons(ss_string1_len, se0.string_length, node);       
      node = tree_cons(ss_string1, tmp0, node);   
    }     
     
    if (k->high == NULL) {          
      node = tree_cons(ss_string2_len, integer_zero_node, node);  
      node = tree_cons(ss_string2, null_pointer_node, node);  
    } else {      
      g95_conv_expr(&se0, k->high);   
      tmp0 = build1(ADDR_EXPR, pchar_type_node, se0.expr);

      node = tree_cons(ss_string2_len, se0.string_length, node);
      node = tree_cons(ss_string2, tmp0, node);    
    }        
        
    tmp0 = build1(ADDR_EXPR, pvoid_type_node, labels[b]);      
    node = tree_cons(ss_target, tmp0, node);  
  
    tmp0 = build(CONSTRUCTOR, select_struct, NULL_TREE, nreverse(node));        
    init = tree_cons(NULL_TREE, tmp0, init);    
  }     
     
  tmp0 = build_array_type(select_struct, build_index_type(build_int_2(m-1, 0)));

  init = build(CONSTRUCTOR, tmp0, NULL_TREE, nreverse(init));          
  TREE_CONSTANT(init) = 1;         
  TREE_STATIC(init) = 1;      
      
  /* Build an argument list for the library call */ 
 
  init = build1(ADDR_EXPR, pvoid_type_node, init);       
  l = build_int_2(m, 0); 
  default_jump = build1(ADDR_EXPR, pvoid_type_node, end_label); 
 
  g95_init_se(&se1, NULL);      
  se1.reflevel = 1;      
  g95_conv_expr(&se1, selector);          
          
  g95_add_block_to_block(&blk, &se1.pre);        
        
  tmp0 = g95_call_library(pvoid_type_node, PREFIX "select_string",   
			 init, l, default_jump,         
			 se1.expr, se1.string_length, NULL_TREE);        
        
  tmp0 = build1(GOTO_EXPR, void_type_node, tmp0);  
  g95_add_expr_to_block(&blk, tmp0);         
         
  tmp0 = g95_finish_block(&list);       
  g95_add_expr_to_block(&blk, tmp0);      
      
  tmp0 = build_v(LABEL_EXPR, end_label);     
  g95_add_expr_to_block(&blk, tmp0);   
   
  if (m != 0) g95_free(labels);  
  
  return g95_finish_block(&blk);          
}  
  
  
      
      
/* trans_where()-- Translate a WHERE statement. */        
        
static tree trans_where(g95_code * codep ATTRIBUTE_UNUSED) {     
  g95_todo_error("Statement not implemented: WHERE");       
}        
        
        
  
  
static tree g95_trans_forall_loop(tree *var0, tree *begin, tree *e, 
				  tree *step, int nvar, tree body) {        
  int t; 
  tree tmp;        
  tree cond;   
  stmtblock_t list;      
  tree exit_label;       
       
  for (t = 0; t < nvar; t++)       
    {          
      exit_label = g95_build_label_decl (NULL_TREE);    
      TREE_USED (exit_label) = 1;    
    
      /* The body of the loop.  */      
      g95_init_block (&list);       
       
      /* The exit condition.  */   
      cond = build (GT_EXPR, boolean_type_node, var0[t], e[t]);         
      tmp = build_v (GOTO_EXPR, exit_label);     
      tmp = build_v (COND_EXPR, cond, tmp, empty_stmt_node);        
      g95_add_expr_to_block (&list, tmp);

      /* The main loop body.  */      
      g95_add_expr_to_block (&list, body);      
      
      /* Increment the loop variable.  */   
      tmp = build (PLUS_EXPR, TREE_TYPE (var0[t]), var0[t], step[t]);        
      g95_add_modify_expr (&list, var0[t], tmp);  
  
      body = g95_finish_block (&list);    
    
      /* Loop var initialization.  */
      g95_init_block (&list);   
      g95_add_modify_expr (&list, var0[t], begin[t]);

      /* The loop expression.  */     
      tmp = build_v (LOOP_EXPR, body);      
      g95_add_expr_to_block (&list, tmp);   
   
      /* The exit label.  */  
      tmp = build_v (LABEL_EXPR, exit_label); 
      g95_add_expr_to_block (&list, tmp);    
    
      body = g95_finish_block (&list);          
    } 
  return body;         
}         
         
         
       
       
/* trans_call()-- Translate a CALL statement */  
  
static tree trans_call(g95_code *code) {     
tree tmp0, a, n, declr;      
g95_se se1;      
      
  g95_init_se(&se1, NULL);         
  g95_start_block(&se1.pre);   
   
  if (g95_has_alt_return(code->ext.actual))
    g95_internal_error("g95_trans_call(): Can't handle alternate returns yet");

  /* If the sub_name member is nonnull, it is the thing to call.
   * Otherwise call the symbol. */      
      
  if (code->sub_name == NULL) {          
    declr = code->sym->backend_decl; 
 
    if (POINTER_TYPE_P(TREE_TYPE(declr)))          
      declr = build1(INDIRECT_REF, TREE_TYPE(TREE_TYPE(declr)), declr);     
     
  } else {        
    n = get_identifier(code->sub_name);     
    declr = build_function_type(void_type_node, NULL_TREE);   
    declr = build_decl(FUNCTION_DECL, n, declr);        
        
    DECL_EXTERNAL(declr) = 1;
    TREE_PUBLIC(declr) = 1;          
          
    pushdecl(declr);    
    rest_of_decl_compilation(declr, NULL, 1, 0);    
  }   
   
  a = g95_trans_arglist(code->ext.actual, &se1); 
 
  tmp0 = g95_build_function_call(declr, a); 
  TREE_SIDE_EFFECTS(tmp0) = 1;    
    
  g95_add_expr_to_block(&se1.pre, tmp0);         
  g95_add_block_to_block(&se1.pre, &se1.post);    
    
  return g95_finish_block(&se1.pre);   
}          
          
          
    
    
/* trans_goto()-- Translate a GOTO statement */       
       
static tree trans_goto(g95_code *codep) {          
          
  return build_v(GOTO_EXPR, g95_get_label_decl(codep->label));         
}    
    
    


/* allocate_scalar()-- Allocate a scalar object. */    
    
static void allocate_scalar(g95_expr *e2, g95_se *s, tree stat_var) { 
tree len, tmp0;         
g95_se se2;       
       
  g95_init_se(&se2, NULL);         
  se2.reflevel = 2;          
          
  g95_conv_expr(&se2, e2);  
  g95_add_block_to_block(&s->pre, &se2.pre);          
  g95_add_block_to_block(&s->post, &se2.post);      
      
  if (e2->ts.type == BT_CHARACTER) {    
    se2.reflevel = 0;       
    len = g95_conv_char_length(&se2, &e2->ts); 
 
    tmp0 = g95_call_library(void_type_node, PREFIX "allocate_string",  
			   se2.expr, len, stat_var, NULL_TREE);          
  } else {    
    len = size_in_bytes(g95_typenode_for_spec(&e2->ts));  
    tmp0 = g95_call_library(void_type_node, PREFIX "allocate_scalar",     
			   se2.expr, len, stat_var, NULL_TREE);      
  }   
   
  g95_add_expr_to_block(&s->pre, tmp0); 
}         
         
         
 
 
/* trans_do_while()-- Translate a DO WHILE statement.  By the time we
 * get here, this statement is always an infinite loop with an exit
 * statement up front. */          
          
static tree trans_do_while(g95_code *c) {  
tree t, cycle_label, exit_label;        
stmtblock_t block;       
       
  g95_start_block(&block);  
  
  cycle_label = g95_build_label_decl(NULL_TREE);     
  exit_label  = g95_build_label_decl(NULL_TREE);        
        
  c->backend_decl = tree_cons(cycle_label, exit_label, NULL);       
       
  t = g95_trans_code(c->block); 
  g95_add_expr_to_block(&block, t);        
        
  if (TREE_USED(cycle_label)) {    
    t = build_v(LABEL_EXPR, cycle_label);        
    g95_add_expr_to_block(&block, t);     
  } 
 
  t = g95_finish_block(&block);     
  g95_init_block(&block);       
       
  t = build_v(LOOP_EXPR, t);       
  g95_add_expr_to_block(&block, t);

  t = build_v(LABEL_EXPR, exit_label);   
  g95_add_expr_to_block(&block, t);     
     
  return g95_finish_block(&block);          
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
   We must support the full range.  */  
  
static tree trans_do(g95_code *c) {    
tree dovar, from, t, step, cnt, type, cond, cycle_label, exit_label, tmp0;         
stmtblock_t block, list;       
g95_se se;     
     
  g95_start_block(&block);    
    
  /* Create SIMPLE versions of all expressions.  */      
  g95_init_se(&se, NULL);       
  g95_conv_expr(&se, c->ext.iterator->var);
  g95_add_block_to_block(&block, &se.pre);        
  dovar = se.expr;      
  type = TREE_TYPE(dovar);          
          
  g95_init_se(&se, NULL);  
  g95_conv_expr_type(&se, c->ext.iterator->start, type);          
  g95_add_block_to_block(&block, &se.pre);         
  from = se.expr;         
         
  g95_init_se(&se, NULL);      
  g95_conv_expr_type(&se, c->ext.iterator->end, type);      
  g95_add_block_to_block(&block, &se.pre);         
  t = se.expr;          
          
  g95_init_se(&se, NULL);        
  g95_conv_expr_type(&se, c->ext.iterator->step, type);       
       
  g95_add_block_to_block(&block, &se.pre);    
  step = se.expr;  
  
  /* Initialize the trip count.  This code is executed before we enter
   * the loop body.  We generate: count = (to + step - from) / step.  */          
          
  tmp0 = fold(build(MINUS_EXPR, type, step, from));     
  tmp0 = fold(build(PLUS_EXPR, type, t, tmp0)); 
  tmp0 = fold(build(TRUNC_DIV_EXPR, type, tmp0, step));         
         
  cnt = g95_create_var(type, "count");
  g95_add_modify_expr(&block, cnt, tmp0);      
      
  /* Initialize the DO variable: dovar = from.  */ 
  g95_add_modify_expr(&block, dovar, from);     
     
  /* Loop body */        
  g95_start_block(&list);  
  
  /* Cycle and exit statements are implemented with gotos.  Put these
   * labels where they can be found later. We put the labels in a
   * TREE_LIST node (because TREE_CHAIN is already used). cycle_label
   * goes in TREE_PURPOSE (backend_decl), exit label in TREE_VALUE
   * (backend_decl).  */     
     
  cycle_label = g95_build_label_decl(NULL_TREE);  
  exit_label  = g95_build_label_decl(NULL_TREE);          
  c->backend_decl = tree_cons(cycle_label, exit_label, NULL);      
      
  /* Start with the loop condition.  Loop until trip count <= 0.  */     
  cond = build(LE_EXPR, boolean_type_node, cnt, integer_zero_node);    
  tmp0 = build_v(GOTO_EXPR, exit_label);      
  TREE_USED(exit_label) = 1;         
  tmp0 = build_v(COND_EXPR, cond, tmp0, empty_stmt_node);      
  g95_add_expr_to_block(&list, tmp0);       
       
       
  /* Main loop body. */    
  tmp0 = g95_trans_code(c->block);    
  g95_add_expr_to_block(&list, tmp0);

  /* Label for cycle statements (if needed). */     
  if (TREE_USED(cycle_label)) {     
    tmp0 = build_v(LABEL_EXPR, cycle_label);      
    g95_add_expr_to_block(&list, tmp0);         
  }

  /* Increment the loop variable. */         
  tmp0 = build(PLUS_EXPR, type, dovar, step);       
  g95_add_modify_expr(&list, dovar, tmp0);         
         
  /* Decrement the trip count. */
  tmp0 = build(MINUS_EXPR, type, cnt, integer_one_node); 
  g95_add_modify_expr(&list, cnt, tmp0);  
  
  /* End of loop body. */        
  tmp0 = g95_finish_block(&list);  
  
  /* The for loop itself. */       
  tmp0 = build_v(LOOP_EXPR, tmp0);     
  g95_add_expr_to_block(&block, tmp0);       
       
  /* Add the exit label */         
  tmp0 = build_v(LABEL_EXPR, exit_label);   
  g95_add_expr_to_block(&block, tmp0);     
     
  return g95_finish_block(&block);     
}   
   
   
     
     
/* g95_do_allocate()-- Allocate data for holding a temporary array.
 * Returns either a local temporary array or a pointer variable.  */        
        
static tree g95_do_allocate(tree bytesize, tree siz, tree *pdata, 
			    stmtblock_t *pblock) { 
tree tmpvar, pointer, typ, tmp0;    
    
  if (INTEGER_CST_P(siz))
    tmp0 = fold(build(MINUS_EXPR, g95_default_integer, siz,        
		     integer_one_node));      
  else          
    tmp0 = NULL_TREE;       
       
  typ = build_range_type(g95_default_integer, integer_zero_node, tmp0);
  typ = build_array_type(boolean_type_node, typ);        
        
  if (g95_stack_variable(bytesize)) {       
    assert(INTEGER_CST_P(siz)); 
    tmpvar = g95_create_var(typ, "mask");          
    pointer = NULL_TREE;
  } else {          
    tmpvar = g95_create_var(build_pointer_type(typ), "mask");      
    tmp0 = g95_call_procedure_alloc(tmpvar, bytesize);       
       
    g95_add_expr_to_block(pblock, tmp0);  
  } 
 
  *pdata = pointer;     
  return tmpvar;         
}          
          
          
 
 
/* trans_stop()-- Translate a STOP statement */        
        
static tree trans_stop(g95_code *cp) {       
g95_se se0;
tree tmp1;      
      
  g95_init_se(&se0, NULL);
  g95_start_block(&se0.pre);

  if (cp->expr == NULL) { 
    tmp1 = build_int_2(cp->ext.stop_code, 0);          
    tmp1 = g95_call_library(void_type_node, PREFIX "stop_numeric",      
			   tmp1, NULL_TREE);  
  } else { 
    se0.reflevel = 1;      
    g95_conv_expr(&se0, cp->expr);          
          
    tmp1 = g95_call_library(void_type_node, PREFIX "stop_string",     
			   se0.expr, se0.string_length, NULL_TREE);   
  }     
     
  g95_set_error_locus(&se0.pre, &cp->loc);   
  g95_add_expr_to_block(&se0.pre, tmp1);        
        
  return g95_finish_block(&se0.pre);
}   
   
   
  
  
/* deallocate_noncp_array()-- Deallocate a entity that is not a
 * character pointer array. */  
  
static tree deallocate_noncp_array(g95_se *se, g95_expr *e1) {
g95_se exp;    
    
  g95_init_se(&exp, NULL);   
  exp.reflevel = 2;     
  g95_conv_expr(&exp, e1);     
     
  g95_add_block_to_block(&se->pre, &exp.pre);      
  g95_add_block_to_block(&se->post, &exp.post);       
       
  return exp.expr; 
}         
         
         
     
     
/* trans_if()-- Translate an IF statement.  */          
          
static tree trans_if(g95_code *code) {   
tree then_stmt, else_stmt, predicate, tmp0;         
stmtblock_t body;      
g95_se s;     
     
  g95_init_se(&s, NULL);         
  g95_start_block(&body);   
   
  g95_conv_expr(&s, code->expr);         
         
  g95_add_block_to_block(&body, &s.pre);  
  
  then_stmt = g95_trans_code(code->block);       
  if (then_stmt == NULL_TREE) then_stmt = empty_stmt_node;    
    
  else_stmt = g95_trans_code(code->ext.block);       
  if (else_stmt == NULL_TREE) else_stmt = empty_stmt_node;          
          
  /* If there is a post chain, we have to stuff the result into a
   * temporary variable, clean up, then take the branch based on the
   * variable. */       
       
  if (s.post.head == NULL_TREE) {         
    predicate = s.expr;   
  } else {      
    predicate = g95_create_var(boolean_type_node, NULL);          
    g95_add_modify_expr(&body, predicate, s.expr);     
     
    g95_add_block_to_block(&body, &s.post);    
  }        
        
  tmp0 = build_v(COND_EXPR, predicate, then_stmt, else_stmt);         
  g95_add_expr_to_block(&body, tmp0);    
    
  return g95_finish_block(&body);      
}     
     
     
         
         
/* logical_select()-- Build a logical selection statement. */

static tree logical_select(g95_code *cp, g95_expr *selector) {       
g95_case *t, *true_case, *false_case, *default_case;    
stmtblock_t blk, b; 
tree tmp, end_label;          
g95_se s;       
       
  true_case = false_case = default_case = NULL;          
          
  t = cp->block->ext.case_list;       
       
  while(t->cprev != NULL)    
    t = t->cprev;    
    
  /* Extract out the cases */    
    
  for(; t; t=t->cnext) {        
    if (t->low == NULL && t->high == NULL) { 
      default_case = t;     
      continue;      
    }

    if (t->low->value.logical)       
      true_case = t;
    else 
      false_case = t;          
  }   
   
  g95_start_block(&blk);      
  g95_init_block(&b);  
  
  g95_init_se(&s, NULL);       
  g95_conv_expr(&s, selector);  
  
  end_label = g95_build_label_decl(NULL_TREE);        
        
  if (false_case != NULL) {
    tmp = build_v(CASE_LABEL_EXPR, integer_zero_node, NULL_TREE);  
    g95_add_expr_to_block(&b, tmp);    
    
    tmp = g95_trans_code(false_case->code);       
    g95_add_expr_to_block(&b, tmp);         
         
    tmp = build_v(GOTO_EXPR, end_label);  
    g95_add_expr_to_block(&b, tmp); 
  }     
     
  if (true_case != NULL) {        
    tmp = build_v(CASE_LABEL_EXPR, integer_one_node, NULL_TREE);          
    g95_add_expr_to_block(&b, tmp);         
         
    tmp = g95_trans_code(true_case->code);         
    g95_add_expr_to_block(&b, tmp);       
       
    tmp = build_v(GOTO_EXPR, end_label);
    g95_add_expr_to_block(&b, tmp);  
  }      
      
  if (default_case != NULL) {        
    tmp = build_v(CASE_LABEL_EXPR, NULL_TREE, NULL_TREE);     
    g95_add_expr_to_block(&b, tmp);         
         
    tmp = g95_trans_code(default_case->code);         
    g95_add_expr_to_block(&b, tmp);

    tmp = build_v(GOTO_EXPR, end_label);
    g95_add_expr_to_block(&b, tmp);         
  }     
     
  tmp = g95_finish_block(&b);    
  tmp = build_v(SWITCH_EXPR, convert(g95_default_integer, s.expr),  
		tmp, NULL_TREE);      
      
  g95_add_expr_to_block(&blk, tmp);      
      
  tmp = build_v(LABEL_EXPR, end_label); 
  g95_add_expr_to_block(&blk, tmp);  
  
  return g95_finish_block(&blk);          
}          
          
          
       
       
/* trans_arithmetic_if()-- Translate an arithmetic IF statement.  */     
     
static tree trans_arithmetic_if(g95_code *c) {
tree t, branch1, branch2, zero;    
g95_se se;      
      
  g95_init_se(&se, NULL);   
  g95_start_block(&se.pre);      
      
  g95_conv_expr(&se, c->expr);       
       
  zero = g95_build_const(TREE_TYPE(se.expr), integer_zero_node);    
    
  branch1 = build_v(GOTO_EXPR, g95_get_label_decl(c->label));         
  branch2 = build_v(GOTO_EXPR, g95_get_label_decl(c->label2));         
         
  t = build(LT_EXPR, boolean_type_node, se.expr, zero);          
  branch1 = build_v(COND_EXPR, t, branch1, branch2);          
  branch2 = build_v(GOTO_EXPR, g95_get_label_decl (c->label3));          
  t = build(LE_EXPR, boolean_type_node, se.expr, zero);   
  branch1 = build_v(COND_EXPR, t, branch1, branch2);       
       
  g95_add_expr_to_block(&se.pre, branch1); 
 
  return g95_finish_block(&se.pre);          
}


       
       
/* init_section_info()-- Initialize the section_info[] array with
 * information about the array being allocated.  The first integer is
 * the rank, the second is the element size.  This is followed by a
 * pair of integers for each dimension that give the upper and lower
 * bound. */        
        
static void init_section_info(g95_se *se1, g95_array_ref *ar, g95_expr *e1,      
			      tree assumed_length) {  
g95_typespec *typesp;      
g95_se se0;       
tree t;
int q, r;   
   
  r = 0;        
  typesp = &e1->ts;   
  g95_set_section_info(se1, r++, build_int_2(ar->dimen, 0)); 
 
  if (typesp->type != BT_CHARACTER)       
    t = size_in_bytes(g95_typenode_for_spec(typesp));      
  else         
    t = (typesp->cl->length != NULL)   
      ? typesp->cl->backend_decl       
      : assumed_length;    
    
  g95_set_section_info(se1, r++, t);     
     
  for(q=0; q<ar->dimen; q++) { 
    switch(ar->dimen_type[q]) { 
    case DIMEN_ELEMENT:     
      g95_set_section_info(se1, r++, integer_one_node);   
   
      g95_init_se(&se0, NULL);  
      g95_conv_expr(&se0, ar->start[q]);    
    
      g95_add_block_to_block(&se1->pre, &se0.pre);    
      g95_set_section_info(se1, r++, se0.expr);       
      g95_add_block_to_block(&se1->post, &se0.post);       
       
      break;         
         
    case DIMEN_RANGE:         
      g95_init_se(&se0, NULL);
      g95_conv_expr(&se0, ar->start[q]);         
         
      g95_add_block_to_block(&se1->pre, &se0.pre);      
      g95_set_section_info(se1, r++, se0.expr);          
      g95_add_block_to_block(&se1->post, &se0.post);

      g95_init_se(&se0, NULL);         
      g95_conv_expr(&se0, ar->end[q]);

      g95_add_block_to_block(&se1->pre, &se0.pre);         
      g95_set_section_info(se1, r++, se0.expr);   
      g95_add_block_to_block(&se1->post, &se0.post);    
    
      break;      
      
    default:
      g95_internal_error("init_section_info(): Bad array ref");          
    }
  }         
}        
        
        
    
    
/* static_alloc_symbol()-- Determine if an allocatable array is in
 * static memory or not. */       
       
static int static_alloc_symbol(g95_symbol *sy) {

  return sy->attr.save || sy->attr.use_assoc || g95_module_symbol(sy);   
}       
       
       
          
          
/* deallocate_char_pointer_array()-- Deallocate a character pointer array. */   
   
static tree deallocate_char_pointer_array(g95_se *s, g95_expr *e1) {   
g95_se se0;       
       
  g95_init_se(&se0, NULL);

  se0.reflevel = 2;     
  g95_conv_expr(&se0, e1);          
          
  g95_add_block_to_block(&s->pre, &se0.pre);        
  g95_add_block_to_block(&s->post, &se0.post); 
 
  return se0.expr;         
}        
        
        
         
         
static void deallocate_pointer(int cp_array, g95_se *se1, g95_expr *e1,
			       tree stat_var) {        
tree tmp1, pointer;      
      
  pointer = cp_array      
    ? deallocate_char_pointer_array(se1, e1)   
    : deallocate_noncp_array(se1, e1);  
  
  tmp1 = g95_call_library(void_type_node, PREFIX "deallocate_pointer",  
			 pointer, stat_var, NULL_TREE);     
     
  g95_add_expr_to_block(&se1->pre, tmp1);
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
  
  
      
      
/* integer_select()-- Build an integer selection statement */    
    
static tree integer_select(g95_code *code, g95_expr *selector) {         
tree end_label, ulow_label, uhigh_label, tmp0, low, high, goto_expr;   
g95_case *cp, *unbounded_low, *unbounded_high;       
stmtblock_t list, b;      
g95_code *s;     
g95_se se1;       
int kind;         
         
  g95_start_block(&list); 
 
  g95_init_se(&se1, NULL);
  g95_conv_expr(&se1, selector);     
     
  kind = selector->ts.kind;    
    
  end_label = g95_build_label_decl(NULL_TREE); 
 
  g95_init_block(&b);

  /* Look for the unbounded low and unbounded high cases. */         
         
  unbounded_low = NULL;   
  unbounded_high = NULL;       
       
  ulow_label = NULL_TREE;        
  uhigh_label = NULL_TREE;

  cp = code->block->ext.case_list; 
 
  while(cp->cnext != NULL)         
    cp = cp->cnext;    
    
  if (cp->high == NULL) {    
    unbounded_low = cp;        
    ulow_label = g95_build_label_decl(NULL_TREE);      
  }      
      
  cp = code->block->ext.case_list; 
 
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
    se1.expr = save_expr(se1.expr);        
        
  g95_add_block_to_block(&list, &se1.pre);    
    
/* Build branch statements to the unbounded cases */        
        
  if (unbounded_high != NULL) {       
    high = g95_conv_mpz_to_tree(unbounded_high->high->value.integer, kind);         
    tmp0 = build(LE_EXPR, boolean_type_node, se1.expr, high);   
   
    goto_expr = build_v(GOTO_EXPR, uhigh_label);   
   
    tmp0 = build_v(COND_EXPR, tmp0, goto_expr, empty_stmt_node);     
    g95_add_expr_to_block(&list, tmp0);    
  }     
     
  if (unbounded_low != NULL) {     
    low = g95_conv_mpz_to_tree(unbounded_low->low->value.integer, kind);         
    tmp0 = build(GE_EXPR, boolean_type_node, se1.expr, low);  
  
    goto_expr = build_v(GOTO_EXPR, ulow_label);  
  
    tmp0 = build_v(COND_EXPR, tmp0, goto_expr, empty_stmt_node); 
    g95_add_expr_to_block(&list, tmp0);    
  }       
       
  /* Build the body */    
    
  for(s=code->block; s; s=s->block) {    
    for(cp=s->ext.case_list; cp; cp=cp->next) {   
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
	low = g95_conv_mpz_to_tree(cp->low->value.integer, kind);     
     
	if (cp->low == cp->high)   
	  high = NULL_TREE;        
	else    
	  high = g95_conv_mpz_to_tree(cp->high->value.integer, kind);        
      }   
   
      /* Add this case label.  */         
         
      tmp0 = build_v(CASE_LABEL_EXPR, low, high);        
      g95_add_expr_to_block(&b, tmp0);        
    }       
       
    /* Add the statements for this case.  */

    tmp0 = g95_trans_code(s->next);       
    g95_add_expr_to_block(&b, tmp0);   
   
    /* Break to the end of the loop. */      
      
    tmp0 = build_v(GOTO_EXPR, end_label);       
    g95_add_expr_to_block(&b, tmp0);     
  }          
          
  tmp0 = g95_finish_block(&b);         
  tmp0 = build_v(SWITCH_EXPR, se1.expr, tmp0, NULL_TREE);       
  g95_add_expr_to_block(&list, tmp0);         
         
  tmp0 = build_v(LABEL_EXPR, end_label);  
  g95_add_expr_to_block(&list, tmp0);         
         
  return g95_finish_block(&list);
}  
  
  
       
       
/* trans_forall()-- FORALL and WHERE statements are really nasty,
 * especially when you nest them.  All the rhs of a forall assignment
 * must be evaluated before the actual assignments are
 * performed. Presumably this also applies to all the assignments in
 * an inner where statement.  */ 
 
/* It is possible to want more than G95_MAX_DIMENSIONS vars, but unlikley.  */       
       
#define MAX_FORALL_VARS G95_MAX_DIMENSIONS
   
/* Generate code for a FORALL statement.  Any temporaries are
 * allocated as a linear array, relying on the fact that we process in
 * the same order in all loops. */  
  
static tree trans_forall(g95_code * code) {     
  stmtblock_t b;       
  stmtblock_t list;         
  tree var[MAX_FORALL_VARS];     
  tree s[MAX_FORALL_VARS];       
  tree fin[MAX_FORALL_VARS];
  tree step[MAX_FORALL_VARS];   
  tree tmp1;       
  tree assign;        
  tree sz;    
  tree bytesize;     
  tree tmpvar;  
  tree sizevar;          
  tree lenvar;    
  tree maskindex;
  tree maski;   
  tree pmask;  
  int i;    
  int nvar;       
  int need_temp;   
  g95_forall_iterator *fa;         
  g95_se se0;          
  g95_code *e;    
    
  g95_start_block (&b);         
         
  i = 0;        
  for (fa = code->ext.forall_iterator; fa; fa = fa->next) {  
    if (i == MAX_FORALL_VARS)   
      fatal_error("too many variables in FORALL statement");  
  
    /* TODO: don't use actual variables in forall.  */      
    g95_init_se(&se0, NULL);         
    g95_conv_expr(&se0, fa->var);          
    assert(is_simple_id(se0.expr));          
          
    /* se.pre should be empty anyway.  */       
    g95_add_block_to_block(&b, &se0.pre);
    var[i] = se0.expr;   
   
    g95_init_se(&se0, NULL);      
    g95_conv_expr(&se0, fa->start);      
    g95_add_block_to_block(&b, &se0.pre);     
    s[i] = se0.expr;    
    
    g95_init_se(&se0, NULL);       
    g95_conv_expr(&se0, fa->end); 
    g95_add_block_to_block(&b, &se0.pre);     
    fin[i] = se0.expr; 
 
    g95_init_se(&se0, NULL);        
    g95_conv_expr(&se0, fa->stride);        
    g95_add_block_to_block(&b, &se0.pre);   
    step[i] = se0.expr; 
 
    i++;        
  }         
  nvar = i;     
       
  assert (nvar <= G95_MAX_DIMENSIONS);        
        
  tmpvar = NULL_TREE;      
  lenvar = NULL_TREE;    
  sz = integer_one_node;  
  sizevar = NULL_TREE; 
  for (i = 0; i < nvar; i++)   
    {
      if (lenvar && TREE_TYPE (lenvar) != TREE_TYPE (s[i]))        
        lenvar = NULL_TREE;         
      /* size = (end + step - start) / step.  */      
      tmp1 = build (MINUS_EXPR, TREE_TYPE (s[i]), step[i], s[i]);    
      tmp1 = build (PLUS_EXPR, TREE_TYPE (fin[i]), fin[i], tmp1);       
       
      tmp1 = build (FLOOR_DIV_EXPR, TREE_TYPE (tmp1), tmp1, step[i]);       
      tmp1 = convert (g95_default_integer, tmp1);        
        
      sz = fold (build (MULT_EXPR, g95_default_integer, sz, tmp1));          
    }

  /* Copy the mask into a temporary variable if required.  */   
  /* For now we assume a mask temporary is needed. */       
  if (code->expr)     
    {  
      bytesize = fold (build (MULT_EXPR, g95_default_integer, sz,   
                              TYPE_SIZE_UNIT (boolean_type_node)));  
  
      maski = g95_do_allocate (bytesize, sz, &pmask, &b); 
 
      maskindex = g95_create_var_np (g95_default_integer, "mi");        
      g95_add_modify_expr (&b, maskindex, integer_zero_node);    
    
      /* Start of mask assignment loop body.  */         
      g95_start_block (&list);      
      
      /* Evaluate the mask expression.  */         
      g95_init_se(&se0, NULL); 
      g95_conv_expr(&se0, code->expr);   
      g95_add_block_to_block (&list, &se0.pre);          
          
      /* Store the mask.  */   
      se0.expr = convert (boolean_type_node, se0.expr);

      if (pmask) 
        tmp1 = build1 (INDIRECT_REF, TREE_TYPE (TREE_TYPE (maski)), maski);         
      else   
        tmp1 = maski;   
      tmp1 = build (ARRAY_REF, boolean_type_node, tmp1, maskindex);      
      g95_add_modify_expr (&list, tmp1, se0.expr);   
   
      /* Advance to the next mask element.  */       
      tmp1 = build (PLUS_EXPR, g95_default_integer, maskindex,
                   integer_one_node); 
      g95_add_modify_expr (&list, maskindex, tmp1);  
  
      /* Generate the loops.  */        
      tmp1 = g95_finish_block (&list);          
      tmp1 = g95_trans_forall_loop (var, s, fin, step, nvar, tmp1);        
      g95_add_expr_to_block (&b, tmp1);    
    }       
  else         
   {    
     /* No mask was specified.  */     
     maskindex = NULL_TREE;    
     maski = pmask = NULL_TREE;          
   }          
          
  e = code->block->next;          
          
  /* TODO: loop merging in FORALL statements.  */     
  while (e)         
    {     
      switch (e->type)  
        {  
        case EXEC_ASSIGN:          
          //need_temp = g95_check_dependency (e->expr, e->expr2, varexpr, nvar);       
          if (need_temp)        
            g95_todo_error ("Forall with temporary");  
  
          if (maski)        
            g95_add_modify_expr (&b, maskindex, integer_zero_node); 
 
          g95_start_block (&list); 
 
          assign = g95_trans_assignment(e->expr, e->expr2);    
    
          if (maski) 
            {        
              /* If a mask was specified make the assignment contitional.  */          
              if (pmask)        
                tmp1 = build1 (INDIRECT_REF, TREE_TYPE (TREE_TYPE (maski)), maski);
              else  
                tmp1 = maski;        
              tmp1 = build (ARRAY_REF, boolean_type_node, tmp1, maskindex);     
     
              tmp1 = build_v (COND_EXPR, tmp1, assign, empty_stmt_node);       
              g95_add_expr_to_block (&list, tmp1);  
  
              /* Advance to the next element.  */        
              tmp1 = build (PLUS_EXPR, g95_default_integer, maskindex,    
                           integer_one_node);        
              g95_add_modify_expr (&list, maskindex, tmp1);    
            }  
          else       
            g95_add_expr_to_block (&list, assign); 
 
          /* Finish the loop.  */       
          tmp1 = g95_finish_block (&list); 
          tmp1 = g95_trans_forall_loop (var, s, fin, step, nvar, tmp1);        
          g95_add_expr_to_block (&b, tmp1);   
          break;          
          
        case EXEC_WHERE:   
          g95_todo_error ("WHERE inside FORALL");       
          break;          
          
        case EXEC_POINTER_ASSIGN:        
          g95_todo_error ("Pointer assignment inside FORALL"); 
          break;       
       
        case EXEC_FORALL:  
          g95_todo_error ("Nested FORALL");      
          break;          
          
        default:
          abort ();    
          break;       
        } 
 
      e = e->next;        
    }  
  
  if (pmask) {        
    /* Free the temporary for the mask.  */
    tmp1 = g95_chainon_list (NULL_TREE, pmask);    
    tmp1 = g95_build_function_call(library_temp_free, tmp1);          
    g95_add_expr_to_block(&b, tmp1);       
  }       
       
  if (maskindex) pushdecl(maskindex);         
         
  return g95_finish_block(&b); 
}          
          
          
         
         
/* allocate_nonchar_pointer_array()-- Allocate an array of a
 * noncharacter type */ 
 
static tree allocate_nonchar_pointer_array(g95_expr *expr, g95_se *s,   
					   g95_array_ref *ref, tree stat_var) {     
tree args;    
    
  init_section_info(s, ref, expr, NULL);     
     
  s->reflevel = 2;       
  g95_conv_expr(s, expr);          
          
  args = g95_chainon_list(NULL_TREE, s->expr);     
  args = g95_chainon_list(args, stat_var);      
  return args;   
}     
     
     
         
         
/* allocate_char_pointer_array()-- Allocate a pointer character array.
 * Instead of just a pointer to the array descriptor, we have a
 * character descriptor that points to the descriptor. */         
         
static tree allocate_char_pointer_array(g95_expr *expr, g95_se *se,        
					g95_array_ref *ar, tree stat_var) {    
tree tmp, argu; 
 
  se->reflevel = 2;  
  g95_conv_expr(se, expr);   
   
  tmp = expr->ts.cl->backend_decl;       
  init_section_info(se, ar, expr, tmp);        
        
  argu = g95_chainon_list(NULL_TREE, se->expr);      
  argu = g95_chainon_list(argu, stat_var);   
   
  return argu;     
}     
     
     
         
         
/* trans_return()-- Translate a RETURN statement */   
   
static tree trans_return(g95_code *code ATTRIBUTE_UNUSED) {        
        
  return build_v(GOTO_EXPR, g95_get_return_label());       
}    
    
    
         
         
/* allocate_pointer_array()-- Generate code to allocate a pointer
 * array. */          
          
static void allocate_pointer_array(bt type, g95_expr *e1, g95_se *s,
				   g95_array_ref *spec, tree stat_var) {    
tree tmp, args; 
 
  if (type == BT_CHARACTER)    
    args = allocate_char_pointer_array(e1, s, spec, stat_var);      
  else       
    args = allocate_nonchar_pointer_array(e1, s, spec, stat_var);  
  
  tmp = g95_build_function_call(library_allocate_pointer_array, args);          
  g95_add_expr_to_block(&s->pre, tmp);   
} 
 
 
        
        
/* deallocate_allocatable()-- Deallocate an allocatable array */          
          
static void deallocate_allocatable(g95_se *se, g95_expr *e, tree stat_var) {          
g95_se se2;
tree tmp1;        
        
  g95_init_se(&se2, NULL);
  se2.reflevel = 1; 
  g95_conv_expr(&se2, e);    
    
  g95_add_block_to_block(&se->pre, &se2.pre);
  g95_add_block_to_block(&se->post, &se2.post);    
    
  tmp1 = build_int_2(static_alloc_symbol(e->symbol), 0);  
  tmp1 = g95_call_library(void_type_node, PREFIX "deallocate_allocatable",   
			 se2.expr, tmp1, stat_var, NULL_TREE); 
 
  g95_add_expr_to_block(&se->pre, tmp1);      
}         
         
         
  
  
/* trans_cycle()-- CYCLE a DO loop. The label decl has already
 * been created by g95_trans_do(), it's in TREE_PURPOSE(backend_decl)
 * of the g95_code node at the head of the loop. We must mark the
 * label as used. */    
    
static tree trans_cycle(g95_code *code) {       
tree cycle_label;     
     
  cycle_label = TREE_PURPOSE(code->ext.block->backend_decl);         
  TREE_USED(cycle_label) = 1;      
  return build_v(GOTO_EXPR, cycle_label);   
} 
 
 
  
  
/* allocate_allocatable_array()-- Generate code to allocate a pointer
 * array.  The section_info[] array is initialized in the same manner
 * as for allocate_pointer_array().  The calling sequence is different
 * as we pass the address of the descriptor. */        
        
static void allocate_allocatable_array(g95_expr *e, g95_se *se1,          
				       g95_array_ref *ar, tree stat_var) {  
tree tmp;          
          
  init_section_info(se1, ar, e, NULL);        
        
  se1->reflevel = 1;  
  g95_conv_expr(se1, e);       
       
  tmp = build_int_2(static_alloc_symbol(e->symbol), 0);    
  tmp = g95_call_library(void_type_node, PREFIX "allocate_allocatable_array",      
			 se1->expr, tmp, stat_var, NULL_TREE);     
     
  g95_add_expr_to_block(&se1->pre, tmp);      
}        
        
        
        
        
/* trans_select()-- Translate a SELECT block */      
      
static tree trans_select(g95_code *code) {         
g95_expr *e;          
tree tmp0;       
       
  /* Normal select statements put the condition in expr, computed GOTO
   * statements put it in expr2. */      
      
  e = (code->expr == NULL) ? code->expr2 : code->expr;          
          
  switch(e->ts.type) {       
  case BT_INTEGER:    tmp0 = integer_select(code, e);    break;    
  case BT_LOGICAL:    tmp0 = logical_select(code, e);    break;          
  case BT_CHARACTER:  tmp0 = character_select(code, e);  break;  
  default:         
    g95_internal_error("g95_trans_select(): Bad type");    
    tmp0 = NULL_TREE;       
  }    
    
  return tmp0;       
}    
    
    
    
 
 
/* trans_deallocate()-- Translate a DEALLOCATE statement */        
        
static tree trans_deallocate(g95_code *code) {     
symbol_attribute attribute;    
tree t, stat_var;       
stmtblock_t b;   
g95_alloc *alloc;        
g95_typespec typ;         
g95_expr *exp; 
int cp_flag;       
g95_se se0;        
        
  g95_init_se(&se0, NULL);    
  g95_set_error_locus(&se0.pre, &code->ext.alloc_list->expr->where);     
     
  if (code->expr == NULL)       
    stat_var = null_pointer_node; 
  else {      
    se0.reflevel = 1;          
    g95_conv_expr(&se0, code->expr);     
    stat_var = save_expr(se0.expr); 
 
    t = build1(INDIRECT_REF, g95_default_integer, stat_var); 
    g95_add_modify_expr(&se0.pre, t, integer_zero_node);     
  }    
    
  for(alloc=code->ext.alloc_list; alloc; alloc=alloc->next) {         
    exp = alloc->expr;         
    attribute = g95_variable_attr(exp, &typ);          
          
    if (exp->symbol->attr.allocatable && exp->ref->type == REF_ARRAY &&         
	exp->ref->next == NULL)          
      deallocate_allocatable(&se0, exp, stat_var);  
    else {       
      cp_flag = (exp->ts.type == BT_CHARACTER) && attribute.pointer;     
      deallocate_pointer(cp_flag, &se0, exp, stat_var);    
    }   
  }

  g95_init_block(&b);      
      
  g95_add_block_to_block(&b, &se0.pre);  
  g95_add_block_to_block(&b, &se0.post);       
       
  return g95_finish_block(&b);     
}       
       
       
    
    
/* trans_allocate()-- Translate an ALLOCATE statement.  If the
 * statement has a STAT variable, we zero it, then generate statements
 * that initialize the right descriptors followed by allocate_array()
 * calls for arrays or just simple allocate_scalar() calls.  The
 * library calls update STAT if something goes wrong, or does nothing
 * if the STAT variable is already nonzero.  If something goes wrong
 * and there is no STAT variable, the program is terminated. */        
        
static tree trans_allocate(g95_code *cp) {  
tree tmp0, stat_var;
stmtblock_t list;          
g95_ref *j, *reference;     
g95_alloc *alloc;     
g95_expr *e1;     
g95_se s;          
          
  g95_init_se(&s, NULL);
  g95_set_error_locus(&s.pre, &cp->ext.alloc_list->expr->where);          
          
  if (cp->expr == NULL)        
    stat_var = null_pointer_node;
  else {      
    s.reflevel = 1;         
    g95_conv_expr(&s, cp->expr); 
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
     
  for(alloc=cp->ext.alloc_list; alloc; alloc=alloc->next) {         
    e1 = alloc->expr;     
     
    if (e1->ref == NULL) { 
      allocate_scalar(e1, &s, stat_var);        
      continue;    
    }         
         
    j = NULL;        
        
    for(reference=e1->ref; reference; reference=reference->next) { 
      if (reference->type == REF_ARRAY && reference->next == NULL) break;          
      j = reference;          
    }     
     
    if (reference == NULL) {  /* Scalar pointer */         
      allocate_scalar(e1, &s, stat_var);    
      continue;        
    }   
   
    if (e1->ref == reference) {  
      e1->ref = NULL;    
    
      if (e1->symbol->attr.allocatable)   
	allocate_allocatable_array(e1, &s, &reference->u.ar, stat_var); 
      else          
	allocate_pointer_array(e1->ts.type, e1, &s, &reference->u.ar, stat_var);     
     
      e1->ref = reference;        
    } else {       
      j->next = NULL;   
      allocate_pointer_array(e1->ts.type, e1, &s, &reference->u.ar, stat_var);   
      j->next = reference;         
    } 
  }     
     
  g95_init_block(&list);   
   
  g95_add_block_to_block(&list, &s.pre);         
  g95_add_block_to_block(&list, &s.post);     
     
  return g95_finish_block(&list);     
} 
 
 
 
 
/* g95_trans_code()-- Translate a list of executable statements. */       
       
tree g95_trans_code(g95_code *code) {
stmtblock_t body;      
tree res;

  if (code == NULL) return NULL_TREE;     
     
  g95_start_block(&body);   
   
  for(; code; code=code->next) {     
    g95_set_backend_locus(&code->loc);         
    if (code->here != 0) {     
      res = build_v(LABEL_EXPR, g95_get_label_decl(code->here));    
      wrap_all_with_wfl(&res, input_filename, lineno);       
      g95_add_expr_to_block(&body, res);   
    }     
     
    switch (code->type) { 
    case EXEC_NOP:         
      res = NULL_TREE;          
      break;

    case EXEC_ASSIGN:
      res = g95_trans_assignment(code->expr, code->expr2);         
      break;        
        
    case EXEC_POINTER_ASSIGN:      
      res = g95_trans_pointer_assign(code);   
      break; 
 
    case EXEC_CONTINUE:         
      res = NULL_TREE;  
      break;  
  
    case EXEC_CYCLE:        
      res = trans_cycle(code);  
      break;         
         
    case EXEC_EXIT:    
      res = trans_exit(code);   
      break;          
          
    case EXEC_GOTO:        
      res = trans_goto(code);
      break;      
      
    case EXEC_STOP:   
      res = trans_stop(code);   
      break;

    case EXEC_CALL:     
      res = trans_call(code);      
      break; 
 
    case EXEC_RETURN:      
      res = trans_return(code);         
      break;  
  
    case EXEC_IF:  
      res = trans_if(code);    
      break;      
      
    case EXEC_ARITHMETIC_IF:          
      res = trans_arithmetic_if(code);    
      break;      
      
    case EXEC_DO:     
      res = trans_do(code);    
      break;         
         
    case EXEC_DO_WHILE:   
      res = trans_do_while(code);       
      break;   
   
    case EXEC_SELECT:  
      res = trans_select(code);   
      break;     
     
    case EXEC_FORALL:          
      res = trans_forall(code);
      break;   
   
    case EXEC_WHERE:     
      res = trans_where(code);         
      break; 
 
    case EXEC_ALLOCATE:  
      res = trans_allocate(code); 
      break;          
          
    case EXEC_DEALLOCATE:    
      res = trans_deallocate(code);     
      break;     
     
    case EXEC_OPEN:  
      res = g95_trans_open(code);      
      break;        
        
    case EXEC_CLOSE:     
      res = g95_trans_close(code);     
      break;        
        
    case EXEC_READ: 
      res = g95_trans_read(code);        
      break;   
   
    case EXEC_WRITE:      
      res = g95_trans_write(code); 
      break;     
     
    case EXEC_IOLENGTH: 
      res = g95_trans_iolength(code); 
      break;         
         
    case EXEC_BACKSPACE:          
      res = g95_trans_backspace(code);          
      break;         
         
    case EXEC_ENDFILE:   
      res = g95_trans_endfile(code); 
      break;  
  
    case EXEC_INQUIRE:      
      res = g95_trans_inquire(code);
      break; 
 
    case EXEC_REWIND:
      res = g95_trans_rewind(code);      
      break; 
 
    case EXEC_TRANSFER:  
      res = g95_trans_transfer(code);      
      break;     
     
    case EXEC_DT_END:
      res = g95_trans_dt_end(code);    
      break;

    default:  
      g95_internal_error("g95_trans_code(): Bad statement code");     
    }  
  
    if (res != NULL && res != empty_stmt_node) {  
      wrap_all_with_wfl(&res, input_filename, lineno);       
      /* Add the new statemment to the block.  */   
      g95_add_expr_to_block (&body, res);          
    }   
  }     
     
  /* Return the finished block. */
  return g95_finish_block(&body);          
} 
