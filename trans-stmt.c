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
   
static tree trans_do(g95_code *code) { 
tree dovar, from, to, step, count, type, cond, cycle_label, exit_label, tmp;    
stmtblock_t block, body;
g95_se se;

  g95_start_block(&block);    
    
  /* Create SIMPLE versions of all expressions.  */  
  g95_init_se(&se, NULL);
  g95_conv_expr(&se, code->ext.iterator->var);          
  g95_add_block_to_block(&block, &se.pre);  
  dovar = se.expr;       
  type = TREE_TYPE(dovar);     
     
  g95_init_se(&se, NULL);   
  g95_conv_expr_type(&se, code->ext.iterator->start, type);     
  g95_add_block_to_block(&block, &se.pre);          
  from = se.expr;    
    
  g95_init_se(&se, NULL);   
  g95_conv_expr_type(&se, code->ext.iterator->end, type);          
  g95_add_block_to_block(&block, &se.pre);
  to = se.expr;     
     
  g95_init_se(&se, NULL);    
  g95_conv_expr_type(&se, code->ext.iterator->step, type);       
       
  g95_add_block_to_block(&block, &se.pre);      
  step = se.expr;        
        
  /* Initialize the trip count.  This code is executed before we enter
   * the loop body.  We generate: count = (to + step - from) / step.  */      
      
  tmp = fold(build(MINUS_EXPR, type, step, from));    
  tmp = fold(build(PLUS_EXPR, type, to, tmp)); 
  tmp = fold(build(TRUNC_DIV_EXPR, type, tmp, step));       
       
  count = g95_create_var(type, "count");     
  g95_add_modify_expr(&block, count, tmp);  
  
  /* Initialize the DO variable: dovar = from.  */   
  g95_add_modify_expr(&block, dovar, from);      
      
  /* Loop body */       
  g95_start_block(&body);        
        
  /* Cycle and exit statements are implemented with gotos.  Put these
   * labels where they can be found later. We put the labels in a
   * TREE_LIST node (because TREE_CHAIN is already used). cycle_label
   * goes in TREE_PURPOSE (backend_decl), exit label in TREE_VALUE
   * (backend_decl).  */ 
 
  cycle_label = g95_build_label_decl(NULL_TREE);          
  exit_label  = g95_build_label_decl(NULL_TREE);
  code->backend_decl = tree_cons(cycle_label, exit_label, NULL);          
          
  /* Start with the loop condition.  Loop until trip count <= 0.  */  
  cond = build(LE_EXPR, boolean_type_node, count, integer_zero_node);       
  tmp = build_v(GOTO_EXPR, exit_label);   
  TREE_USED(exit_label) = 1;
  tmp = build_v(COND_EXPR, cond, tmp, empty_stmt_node);    
  g95_add_expr_to_block(&body, tmp);   
   
   
  /* Main loop body. */          
  tmp = g95_trans_code(code->block); 
  g95_add_expr_to_block(&body, tmp);          
          
  /* Label for cycle statements (if needed). */     
  if (TREE_USED(cycle_label)) {    
    tmp = build_v(LABEL_EXPR, cycle_label);  
    g95_add_expr_to_block(&body, tmp);     
  }         
         
  /* Increment the loop variable. */        
  tmp = build(PLUS_EXPR, type, dovar, step);     
  g95_add_modify_expr(&body, dovar, tmp);         
         
  /* Decrement the trip count. */    
  tmp = build(MINUS_EXPR, type, count, integer_one_node);    
  g95_add_modify_expr(&body, count, tmp);         
         
  /* End of loop body. */     
  tmp = g95_finish_block(&body);       
       
  /* The for loop itself. */  
  tmp = build_v(LOOP_EXPR, tmp);
  g95_add_expr_to_block(&block, tmp);          
          
  /* Add the exit label */       
  tmp = build_v(LABEL_EXPR, exit_label);  
  g95_add_expr_to_block(&block, tmp);    
    
  return g95_finish_block(&block);   
}         
         
         
 
 
/* trans_call()-- Translate a CALL statement */         
         
static tree trans_call(g95_code *code) {         
tree tmp, args, name, decl; 
g95_se se;

  g95_init_se(&se, NULL);   
  g95_start_block(&se.pre);       
       
  if (g95_has_alt_return(code->ext.actual))   
    g95_internal_error("g95_trans_call(): Can't handle alternate returns yet");         
         
  /* If the sub_name member is nonnull, it is the thing to call.
   * Otherwise call the symbol. */   
   
  if (code->sub_name == NULL) {         
    decl = code->sym->backend_decl;

    if (POINTER_TYPE_P(TREE_TYPE(decl)))      
      decl = build1(INDIRECT_REF, TREE_TYPE(TREE_TYPE(decl)), decl);          
          
  } else {
    name = get_identifier(code->sub_name);      
    decl = build_function_type(void_type_node, NULL_TREE);
    decl = build_decl(FUNCTION_DECL, name, decl);    
    
    DECL_EXTERNAL(decl) = 1; 
    TREE_PUBLIC(decl) = 1; 
 
    pushdecl(decl); 
    rest_of_decl_compilation(decl, NULL, 1, 0);          
  }  
  
  args = g95_trans_arglist(code->ext.actual, &se);        
        
  tmp = g95_build_function_call(decl, args);         
  TREE_SIDE_EFFECTS(tmp) = 1;    
    
  g95_add_expr_to_block(&se.pre, tmp);          
  g95_add_block_to_block(&se.pre, &se.post); 
 
  return g95_finish_block(&se.pre); 
}  
  
  
          
          
/* trans_arithmetic_if()-- Translate an arithmetic IF statement.  */     
     
static tree trans_arithmetic_if(g95_code *code) {          
tree tmp, branch1, branch2, zero;
g95_se se;         
         
  g95_init_se(&se, NULL);         
  g95_start_block(&se.pre);       
       
  g95_conv_expr(&se, code->expr);       
       
  zero = g95_build_const(TREE_TYPE(se.expr), integer_zero_node);

  branch1 = build_v(GOTO_EXPR, g95_get_label_decl(code->label));      
  branch2 = build_v(GOTO_EXPR, g95_get_label_decl(code->label2));          
          
  tmp = build(LT_EXPR, boolean_type_node, se.expr, zero); 
  branch1 = build_v(COND_EXPR, tmp, branch1, branch2);
  branch2 = build_v(GOTO_EXPR, g95_get_label_decl (code->label3));          
  tmp = build(LE_EXPR, boolean_type_node, se.expr, zero);          
  branch1 = build_v(COND_EXPR, tmp, branch1, branch2);

  g95_add_expr_to_block(&se.pre, branch1);    
    
  return g95_finish_block(&se.pre);     
}          
          
          
         
         
/* trans_exit()-- EXIT a DO loop.  Similar to CYCLE, but now the
 * label is in TREE_VALUE(backend_decl) of the g95_code node at the
 * head of the loop. */         
         
static tree trans_exit(g95_code *code) {         
tree exit_label;

  exit_label = TREE_VALUE(code->ext.block->backend_decl);        
  TREE_USED(exit_label) = 1;
  return build_v(GOTO_EXPR, exit_label);  
}    
    
    
  
  
/* g95_do_allocate()-- Allocate data for holding a temporary array.
 * Returns either a local temporary array or a pointer variable.  */  
  
static tree g95_do_allocate(tree bytesize, tree size, tree *pdata,         
			    stmtblock_t *pblock) {   
tree tmpvar, pointer, type, tmp;      
      
  if (INTEGER_CST_P(size))      
    tmp = fold(build(MINUS_EXPR, g95_default_integer, size,     
		     integer_one_node));   
  else
    tmp = NULL_TREE;       
       
  type = build_range_type(g95_default_integer, integer_zero_node, tmp);    
  type = build_array_type(boolean_type_node, type);   
   
  if (g95_stack_variable(bytesize)) {     
    assert(INTEGER_CST_P(size));       
    tmpvar = g95_create_var(type, "mask");     
    pointer = NULL_TREE;        
  } else {  
    tmpvar = g95_create_var(build_pointer_type(type), "mask"); 
    tmp = g95_call_procedure_alloc(tmpvar, bytesize);    
    
    g95_add_expr_to_block(pblock, tmp);    
  }    
    
  *pdata = pointer;     
  return tmpvar;     
}    
    
    
      
      
/* trans_goto()-- Translate a GOTO statement */       
       
static tree trans_goto(g95_code *code) {   
   
  return build_v(GOTO_EXPR, g95_get_label_decl(code->label));
}          
          
          
     
     
/* trans_do_while()-- Translate a DO WHILE statement.  By the time we
 * get here, this statement is always an infinite loop with an exit
 * statement up front. */         
         
static tree trans_do_while(g95_code *code) {          
tree tmp, cycle_label, exit_label;          
stmtblock_t block;  
  
  g95_start_block(&block);   
   
  cycle_label = g95_build_label_decl(NULL_TREE);     
  exit_label  = g95_build_label_decl(NULL_TREE);        
        
  code->backend_decl = tree_cons(cycle_label, exit_label, NULL);        
        
  tmp = g95_trans_code(code->block);         
  g95_add_expr_to_block(&block, tmp);      
      
  if (TREE_USED(cycle_label)) { 
    tmp = build_v(LABEL_EXPR, cycle_label);     
    g95_add_expr_to_block(&block, tmp);   
  }       
       
  tmp = g95_finish_block(&block);
  g95_init_block(&block);       
       
  tmp = build_v(LOOP_EXPR, tmp);     
  g95_add_expr_to_block(&block, tmp);   
   
  tmp = build_v(LABEL_EXPR, exit_label);  
  g95_add_expr_to_block(&block, tmp);  
  
  return g95_finish_block(&block);  
}  
  
  
     
     
/* integer_select()-- Build an integer selection statement */      
      
static tree integer_select(g95_code *code, g95_expr *selector) {   
tree end_label, ulow_label, uhigh_label, tmp, low, high, goto_expr;    
g95_case *cp, *unbounded_low, *unbounded_high; 
stmtblock_t block, body;  
g95_code *n;       
g95_se se;
int kind;     
     
  g95_start_block(&block);      
      
  g95_init_se(&se, NULL);      
  g95_conv_expr(&se, selector);       
       
  kind = selector->ts.kind;    
    
  end_label = g95_build_label_decl(NULL_TREE);        
        
  g95_init_block(&body);    
    
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
    se.expr = save_expr(se.expr);     
     
  g95_add_block_to_block(&block, &se.pre);   
   
/* Build branch statements to the unbounded cases */         
         
  if (unbounded_high != NULL) {   
    high = g95_conv_mpz_to_tree(unbounded_high->high->value.integer, kind);  
    tmp = build(LE_EXPR, boolean_type_node, se.expr, high);          
          
    goto_expr = build_v(GOTO_EXPR, uhigh_label);         
         
    tmp = build_v(COND_EXPR, tmp, goto_expr, empty_stmt_node);        
    g95_add_expr_to_block(&block, tmp);       
  }    
    
  if (unbounded_low != NULL) {  
    low = g95_conv_mpz_to_tree(unbounded_low->low->value.integer, kind);        
    tmp = build(GE_EXPR, boolean_type_node, se.expr, low);        
        
    goto_expr = build_v(GOTO_EXPR, ulow_label);

    tmp = build_v(COND_EXPR, tmp, goto_expr, empty_stmt_node);         
    g95_add_expr_to_block(&block, tmp);       
  }       
       
  /* Build the body */     
     
  for(n=code->block; n; n=n->block) {    
    for(cp=n->ext.case_list; cp; cp=cp->next) {  
      if (cp == unbounded_low) {
	tmp = build_v(LABEL_EXPR, ulow_label);        
	g95_add_expr_to_block(&body, tmp);    
	continue;  
      } 
 
      if (cp == unbounded_high) {    
	tmp = build_v(LABEL_EXPR, uhigh_label);    
	g95_add_expr_to_block(&body, tmp); 
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
        
      tmp = build_v(CASE_LABEL_EXPR, low, high);         
      g95_add_expr_to_block(&body, tmp);         
    }         
         
    /* Add the statements for this case.  */          
          
    tmp = g95_trans_code(n->next);        
    g95_add_expr_to_block(&body, tmp);    
    
    /* Break to the end of the loop. */         
         
    tmp = build_v(GOTO_EXPR, end_label);       
    g95_add_expr_to_block(&body, tmp);      
  }          
          
  tmp = g95_finish_block(&body);          
  tmp = build_v(SWITCH_EXPR, se.expr, tmp, NULL_TREE);        
  g95_add_expr_to_block(&block, tmp);         
         
  tmp = build_v(LABEL_EXPR, end_label);       
  g95_add_expr_to_block(&block, tmp);          
          
  return g95_finish_block(&block);        
}     
     
     
      
      
/* allocate_scalar()-- Allocate a scalar object. */         
         
static void allocate_scalar(g95_expr *expr, g95_se *se, tree stat_var) {     
tree length, tmp;          
g95_se se0;   
   
  g95_init_se(&se0, NULL);       
  se0.reflevel = 2;   
   
  g95_conv_expr(&se0, expr); 
  g95_add_block_to_block(&se->pre, &se0.pre);         
  g95_add_block_to_block(&se->post, &se0.post);          
          
  if (expr->ts.type == BT_CHARACTER) {         
    se0.reflevel = 0;   
    length = g95_conv_char_length(&se0, &expr->ts);          
          
    tmp = g95_call_library(void_type_node, PREFIX "allocate_string",       
			   se0.expr, length, stat_var, NULL_TREE);         
  } else {        
    length = size_in_bytes(g95_typenode_for_spec(&expr->ts)); 
    tmp = g95_call_library(void_type_node, PREFIX "allocate_scalar",       
			   se0.expr, length, stat_var, NULL_TREE);   
  }          
          
  g95_add_expr_to_block(&se->pre, tmp);       
}       
       
       
       
       
/* trans_where()-- Translate a WHERE statement. */    
    
static tree trans_where(g95_code * code ATTRIBUTE_UNUSED) {      
  g95_todo_error("Statement not implemented: WHERE");          
}  
  
  
       
       
/* trans_return()-- Translate a RETURN statement */       
       
static tree trans_return(g95_code *code ATTRIBUTE_UNUSED) {          
          
  return build_v(GOTO_EXPR, g95_get_return_label()); 
} 
 
 
        
        
static tree g95_trans_forall_loop(tree *var, tree *start, tree *end,         
				  tree *step, int nvar, tree body) {     
  int o;
  tree tmp;       
  tree cond;    
  stmtblock_t block;       
  tree exit_label;      
      
  for (o = 0; o < nvar; o++)          
    {  
      exit_label = g95_build_label_decl (NULL_TREE);    
      TREE_USED (exit_label) = 1;    
    
      /* The body of the loop.  */          
      g95_init_block (&block);     
     
      /* The exit condition.  */    
      cond = build (GT_EXPR, boolean_type_node, var[o], end[o]);
      tmp = build_v (GOTO_EXPR, exit_label);        
      tmp = build_v (COND_EXPR, cond, tmp, empty_stmt_node);         
      g95_add_expr_to_block (&block, tmp);        
        
      /* The main loop body.  */ 
      g95_add_expr_to_block (&block, body);      
      
      /* Increment the loop variable.  */ 
      tmp = build (PLUS_EXPR, TREE_TYPE (var[o]), var[o], step[o]);   
      g95_add_modify_expr (&block, var[o], tmp);         
         
      body = g95_finish_block (&block);         
         
      /* Loop var initialization.  */       
      g95_init_block (&block); 
      g95_add_modify_expr (&block, var[o], start[o]);          
          
      /* The loop expression.  */
      tmp = build_v (LOOP_EXPR, body);  
      g95_add_expr_to_block (&block, tmp);          
          
      /* The exit label.  */   
      tmp = build_v (LABEL_EXPR, exit_label);   
      g95_add_expr_to_block (&block, tmp);   
   
      body = g95_finish_block (&block);      
    } 
  return body;      
}          
          
          


/* logical_select()-- Build a logical selection statement. */

static tree logical_select(g95_code *code, g95_expr *selector) {        
g95_case *y, *true_case, *false_case, *default_case;       
stmtblock_t block, body;          
tree tmp, end_label;  
g95_se se;        
        
  true_case = false_case = default_case = NULL;   
   
  y = code->block->ext.case_list;    
    
  while(y->cprev != NULL)      
    y = y->cprev;       
       
  /* Extract out the cases */   
   
  for(; y; y=y->cnext) {     
    if (y->low == NULL && y->high == NULL) {    
      default_case = y; 
      continue;     
    }          
          
    if (y->low->value.logical)         
      true_case = y;
    else       
      false_case = y;         
  }       
       
  g95_start_block(&block);    
  g95_init_block(&body); 
 
  g95_init_se(&se, NULL);         
  g95_conv_expr(&se, selector);

  end_label = g95_build_label_decl(NULL_TREE);       
       
  if (false_case != NULL) {
    tmp = build_v(CASE_LABEL_EXPR, integer_zero_node, NULL_TREE);   
    g95_add_expr_to_block(&body, tmp);       
       
    tmp = g95_trans_code(false_case->code);       
    g95_add_expr_to_block(&body, tmp);      
      
    tmp = build_v(GOTO_EXPR, end_label);         
    g95_add_expr_to_block(&body, tmp); 
  }    
    
  if (true_case != NULL) {   
    tmp = build_v(CASE_LABEL_EXPR, integer_one_node, NULL_TREE);     
    g95_add_expr_to_block(&body, tmp);

    tmp = g95_trans_code(true_case->code);        
    g95_add_expr_to_block(&body, tmp);  
  
    tmp = build_v(GOTO_EXPR, end_label);   
    g95_add_expr_to_block(&body, tmp); 
  }

  if (default_case != NULL) {   
    tmp = build_v(CASE_LABEL_EXPR, NULL_TREE, NULL_TREE);   
    g95_add_expr_to_block(&body, tmp);      
      
    tmp = g95_trans_code(default_case->code);        
    g95_add_expr_to_block(&body, tmp);  
  
    tmp = build_v(GOTO_EXPR, end_label);     
    g95_add_expr_to_block(&body, tmp);
  }      
      
  tmp = g95_finish_block(&body);        
  tmp = build_v(SWITCH_EXPR, convert(g95_default_integer, se.expr),          
		tmp, NULL_TREE); 
 
  g95_add_expr_to_block(&block, tmp);

  tmp = build_v(LABEL_EXPR, end_label);     
  g95_add_expr_to_block(&block, tmp);  
  
  return g95_finish_block(&block);  
}        
        
        
       
       
/* character_select()-- Implement a character SELECT statement.  We
 * generate an array of structures describing the cases in order and
 * call a library subroutine that locates the right case.  The library
 * subroutine returns a pointer to jump to or NULL if no branches are
 * to be taken. */ 
 
static tree character_select(g95_code *code, g95_expr *selector) {          
tree init, node, end_label, len, tmp, *labels, default_jump;  
stmtblock_t block, body;  
g95_case *cp, *q;          
g95_code *c; 
g95_se se;
int l, v;        
        
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
     
  cp = code->block->ext.case_list;    
  while(cp->cprev != NULL)  
    cp = cp->cprev;          
          
  v = 0; 
  for(q=cp; q; q=q->cnext) 
    q->n = v++;

  if (v != 0) labels = g95_getmem(v*sizeof(tree));          
          
  for(l=0; l<v; l++)         
    labels[l] = g95_build_label_decl(NULL_TREE);    
    
  end_label = g95_build_label_decl(NULL_TREE);     
     
/* Generate the body */     
     
  g95_start_block(&block);   
  g95_init_block(&body);      
      
  for(c=code->block; c; c=c->block) {          
    for(q=c->ext.case_list; q; q=q->next) {
      tmp = build_v(LABEL_EXPR, labels[q->n]);    
      g95_add_expr_to_block(&body, tmp);         
    }       
       
    tmp = g95_trans_code(c->next);        
    g95_add_expr_to_block(&body, tmp);   
   
    tmp = build_v(GOTO_EXPR, end_label);          
    g95_add_expr_to_block(&body, tmp);
  }    
    
/* Generate the structure describing the branches */       
       
  init = NULL_TREE;        
        
  l = 0;   
  for(q=cp; q; q=q->cnext, l++) {       
    node = NULL_TREE; 
 
    if (q->low == NULL) {          
      node = tree_cons(ss_string1_len, integer_zero_node, node);        
      node = tree_cons(ss_string1,     null_pointer_node, node); 
    } else {   
      len = build_int_2(q->low->value.character.length, 0);         
         
      tmp = g95_conv_string_init(len, q->low);        
      tmp = build1(ADDR_EXPR, pchar_type_node, tmp);   
   
      node = tree_cons(ss_string1_len, len, node);      
      node = tree_cons(ss_string1,     tmp, node);         
    }      
      
    if (q->high == NULL) {       
      node = tree_cons(ss_string2_len, integer_zero_node, node);    
      node = tree_cons(ss_string2,     null_pointer_node, node);   
    } else {      
      len = build_int_2(q->high->value.character.length, 0);       
       
      tmp = g95_conv_string_init(len, q->high);  
      tmp = build1(ADDR_EXPR, pchar_type_node, tmp);   
   
      node = tree_cons(ss_string2_len, len, node);
      node = tree_cons(ss_string2,     tmp, node);       
    }

    tmp = build1(ADDR_EXPR, pvoid_type_node, labels[l]);  
    node = tree_cons(ss_target, tmp, node);      
      
    tmp = build(CONSTRUCTOR, select_struct, NULL_TREE, nreverse(node));        
    init = tree_cons(NULL_TREE, tmp, init);         
  }  
  
  tmp = build_array_type(select_struct, build_index_type(build_int_2(v-1, 0))); 
 
  init = build(CONSTRUCTOR, tmp, NULL_TREE, nreverse(init));       
  TREE_CONSTANT(init) = 1;         
  TREE_STATIC(init) = 1;       
       
  /* Build an argument list for the library call */  
  
  init = build1(ADDR_EXPR, pvoid_type_node, init);   
  len = build_int_2(v, 0);          
  default_jump = build1(ADDR_EXPR, pvoid_type_node, end_label);        
        
  g95_init_se(&se, NULL); 
  se.reflevel = 1;          
  g95_conv_expr(&se, selector);       
       
  g95_add_block_to_block(&block, &se.pre);     
     
  tmp = g95_call_library(pvoid_type_node, PREFIX "select_string",
			 init, len, default_jump,      
			 se.expr, se.string_length, NULL_TREE);

  tmp = build1(GOTO_EXPR, void_type_node, tmp); 
  g95_add_expr_to_block(&block, tmp);     
     
  tmp = g95_finish_block(&body);     
  g95_add_expr_to_block(&block, tmp);      
      
  tmp = build_v(LABEL_EXPR, end_label);
  g95_add_expr_to_block(&block, tmp);

  if (v != 0) g95_free(labels);        
        
  return g95_finish_block(&block);
}          
          
          
    
    
/* static_alloc_symbol()-- Determine if an allocatable array is in
 * static memory or not. */          
          
static int static_alloc_symbol(g95_symbol *symb) {     
     
  return symb->attr.save || symb->attr.use_assoc || g95_module_symbol(symb);       
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
          
          
    
    
/* deallocate_char_pointer_array()-- Deallocate a character pointer array. */ 
 
static tree deallocate_char_pointer_array(g95_se *se, g95_expr *expr) {  
g95_se se0;   
   
  g95_init_se(&se0, NULL);       
       
  se0.reflevel = 2;     
  g95_conv_expr(&se0, expr);  
  
  g95_add_block_to_block(&se->pre, &se0.pre);     
  g95_add_block_to_block(&se->post, &se0.post);  
  
  return se0.expr;      
}   
   
   
     
     
/* deallocate_noncp_array()-- Deallocate a entity that is not a
 * character pointer array. */  
  
static tree deallocate_noncp_array(g95_se *se, g95_expr *expr) {    
g95_se se0;        
        
  g95_init_se(&se0, NULL);          
  se0.reflevel = 2;         
  g95_conv_expr(&se0, expr);         
         
  g95_add_block_to_block(&se->pre, &se0.pre);      
  g95_add_block_to_block(&se->post, &se0.post);         
         
  return se0.expr;        
}        
        
        
  
  
static void deallocate_pointer(int cp_array, g95_se *se, g95_expr *expr,      
			       tree stat_var) { 
tree tmp, pointer;          
          
  pointer = cp_array      
    ? deallocate_char_pointer_array(se, expr)     
    : deallocate_noncp_array(se, expr);        
        
  tmp = g95_call_library(void_type_node, PREFIX "deallocate_pointer",       
			 pointer, stat_var, NULL_TREE); 
 
  g95_add_expr_to_block(&se->pre, tmp);     
}    
    
    
       
       
/* init_section_info()-- Initialize the section_info[] array with
 * information about the array being allocated.  The first integer is
 * the rank, the second is the element size.  This is followed by a
 * pair of integers for each dimension that give the upper and lower
 * bound. */

static void init_section_info(g95_se *se, g95_array_ref *ar, g95_expr *expr, 
			      tree assumed_length) {          
g95_typespec *ts; 
g95_se se0;  
tree tmp;         
int a, t;     
     
  t = 0;      
  ts = &expr->ts;    
  g95_set_section_info(se, t++, build_int_2(ar->dimen, 0));   
   
  if (ts->type != BT_CHARACTER)       
    tmp = size_in_bytes(g95_typenode_for_spec(ts));          
  else  
    tmp = (ts->cl->length != NULL)    
      ? g95_conv_constant(ts->cl->length, NULL_TREE)          
      : assumed_length;      
      
  g95_set_section_info(se, t++, tmp);    
    
  for(a=0; a<ar->dimen; a++) {    
    switch(ar->dimen_type[a]) { 
    case DIMEN_ELEMENT:         
      g95_set_section_info(se, t++, integer_one_node);  
  
      g95_init_se(&se0, NULL);       
      g95_conv_expr(&se0, ar->start[a]);     
     
      g95_add_block_to_block(&se->pre, &se0.pre);        
      g95_set_section_info(se, t++, se0.expr);
      g95_add_block_to_block(&se->post, &se0.post);        
        
      break;        
        
    case DIMEN_RANGE:   
      g95_init_se(&se0, NULL);    
      g95_conv_expr(&se0, ar->start[a]);         
         
      g95_add_block_to_block(&se->pre, &se0.pre);        
      g95_set_section_info(se, t++, se0.expr);        
      g95_add_block_to_block(&se->post, &se0.post);         
         
      g95_init_se(&se0, NULL);       
      g95_conv_expr(&se0, ar->end[a]);          
          
      g95_add_block_to_block(&se->pre, &se0.pre);     
      g95_set_section_info(se, t++, se0.expr);
      g95_add_block_to_block(&se->post, &se0.post);

      break;         
         
    default: 
      g95_internal_error("init_section_info(): Bad array ref");        
    }     
  } 
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
  stmtblock_t block;          
  stmtblock_t body;       
  tree var[MAX_FORALL_VARS];     
  tree start[MAX_FORALL_VARS]; 
  tree end[MAX_FORALL_VARS];        
  tree step[MAX_FORALL_VARS];        
  tree tmp; 
  tree assign;   
  tree size;   
  tree bytesize;         
  tree tmpvar;        
  tree sizevar;       
  tree lenvar;
  tree maskindex;       
  tree mask;          
  tree pmask;   
  int a;        
  int nvar;      
  int need_temp; 
  g95_forall_iterator *fa; 
  g95_se se;          
  g95_code *f;     
     
  g95_start_block (&block);      
      
  a = 0;       
  for (fa = code->ext.forall_iterator; fa; fa = fa->next) {   
    if (a == MAX_FORALL_VARS)       
      fatal_error("too many variables in FORALL statement");

    /* TODO: don't use actual variables in forall.  */        
    g95_init_se(&se, NULL);         
    g95_conv_expr(&se, fa->var);       
    assert(is_simple_id(se.expr));  
  
    /* se.pre should be empty anyway.  */    
    g95_add_block_to_block(&block, &se.pre);   
    var[a] = se.expr;       
       
    g95_init_se(&se, NULL);        
    g95_conv_expr(&se, fa->start);       
    g95_add_block_to_block(&block, &se.pre);       
    start[a] = se.expr;       
       
    g95_init_se(&se, NULL);         
    g95_conv_expr(&se, fa->end);         
    g95_add_block_to_block(&block, &se.pre);         
    end[a] = se.expr;  
  
    g95_init_se(&se, NULL); 
    g95_conv_expr(&se, fa->stride);      
    g95_add_block_to_block(&block, &se.pre);      
    step[a] = se.expr;

    a++;  
  }     
  nvar = a;   
     
  assert (nvar <= G95_MAX_DIMENSIONS);   
   
  tmpvar = NULL_TREE;  
  lenvar = NULL_TREE;  
  size = integer_one_node; 
  sizevar = NULL_TREE;         
  for (a = 0; a < nvar; a++) 
    {    
      if (lenvar && TREE_TYPE (lenvar) != TREE_TYPE (start[a]))   
        lenvar = NULL_TREE;   
      /* size = (end + step - start) / step.  */          
      tmp = build (MINUS_EXPR, TREE_TYPE (start[a]), step[a], start[a]);    
      tmp = build (PLUS_EXPR, TREE_TYPE (end[a]), end[a], tmp);   
   
      tmp = build (FLOOR_DIV_EXPR, TREE_TYPE (tmp), tmp, step[a]);      
      tmp = convert (g95_default_integer, tmp);         
         
      size = fold (build (MULT_EXPR, g95_default_integer, size, tmp));    
    }        
        
  /* Copy the mask into a temporary variable if required.  */      
  /* For now we assume a mask temporary is needed. */    
  if (code->expr)    
    {  
      bytesize = fold (build (MULT_EXPR, g95_default_integer, size,   
                              TYPE_SIZE_UNIT (boolean_type_node)));

      mask = g95_do_allocate (bytesize, size, &pmask, &block);

      maskindex = g95_create_var_np (g95_default_integer, "mi");          
      g95_add_modify_expr (&block, maskindex, integer_zero_node);         
         
      /* Start of mask assignment loop body.  */   
      g95_start_block (&body);      
      
      /* Evaluate the mask expression.  */          
      g95_init_se(&se, NULL);  
      g95_conv_expr(&se, code->expr);        
      g95_add_block_to_block (&body, &se.pre);    
    
      /* Store the mask.  */         
      se.expr = convert (boolean_type_node, se.expr);   
   
      if (pmask)       
        tmp = build1 (INDIRECT_REF, TREE_TYPE (TREE_TYPE (mask)), mask);          
      else  
        tmp = mask; 
      tmp = build (ARRAY_REF, boolean_type_node, tmp, maskindex);  
      g95_add_modify_expr (&body, tmp, se.expr);       
       
      /* Advance to the next mask element.  */     
      tmp = build (PLUS_EXPR, g95_default_integer, maskindex,        
                   integer_one_node);      
      g95_add_modify_expr (&body, maskindex, tmp);

      /* Generate the loops.  */      
      tmp = g95_finish_block (&body);   
      tmp = g95_trans_forall_loop (var, start, end, step, nvar, tmp);         
      g95_add_expr_to_block (&block, tmp);       
    }    
  else  
   { 
     /* No mask was specified.  */         
     maskindex = NULL_TREE;  
     mask = pmask = NULL_TREE;          
   }       
       
  f = code->block->next;  
  
  /* TODO: loop merging in FORALL statements.  */
  while (f)    
    {         
      switch (f->type)     
        {  
        case EXEC_ASSIGN:      
          //need_temp = g95_check_dependency (f->expr, f->expr2, varexpr, nvar);      
          if (need_temp)
            g95_todo_error ("Forall with temporary");

          if (mask)      
            g95_add_modify_expr (&block, maskindex, integer_zero_node);       
       
          g95_start_block (&body);      
      
          assign = g95_trans_assignment(f->expr, f->expr2);      
      
          if (mask)
            {    
              /* If a mask was specified make the assignment contitional.  */     
              if (pmask)
                tmp = build1 (INDIRECT_REF, TREE_TYPE (TREE_TYPE (mask)), mask);     
              else    
                tmp = mask;       
              tmp = build (ARRAY_REF, boolean_type_node, tmp, maskindex);     
     
              tmp = build_v (COND_EXPR, tmp, assign, empty_stmt_node);   
              g95_add_expr_to_block (&body, tmp);

              /* Advance to the next element.  */
              tmp = build (PLUS_EXPR, g95_default_integer, maskindex,    
                           integer_one_node); 
              g95_add_modify_expr (&body, maskindex, tmp); 
            } 
          else    
            g95_add_expr_to_block (&body, assign);        
        
          /* Finish the loop.  */       
          tmp = g95_finish_block (&body);          
          tmp = g95_trans_forall_loop (var, start, end, step, nvar, tmp);       
          g95_add_expr_to_block (&block, tmp);          
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
 
      f = f->next;          
    }        
        
  if (pmask) {     
    /* Free the temporary for the mask.  */      
    tmp = g95_chainon_list (NULL_TREE, pmask);       
    tmp = g95_build_function_call(library_temp_free, tmp);      
    g95_add_expr_to_block(&block, tmp);         
  }        
        
  if (maskindex) pushdecl(maskindex); 
 
  return g95_finish_block(&block);     
}     
     
     
    
    
/* trans_stop()-- Translate a STOP statement */       
       
static tree trans_stop(g95_code *code) {  
g95_se se;     
tree tmp;         
         
  g95_init_se(&se, NULL);          
  g95_start_block(&se.pre);          
          
  if (code->expr == NULL) {     
    tmp = build_int_2(code->ext.stop_code, 0);     
    tmp = g95_call_library(void_type_node, PREFIX "stop_numeric",       
			   tmp, NULL_TREE);     
  } else {        
    se.reflevel = 1;     
    g95_conv_expr(&se, code->expr);     
     
    tmp = g95_call_library(void_type_node, PREFIX "stop_string",        
			   se.expr, se.string_length, NULL_TREE);         
  }

  g95_set_error_locus(&se.pre, &code->loc);       
  g95_add_expr_to_block(&se.pre, tmp);     
     
  return g95_finish_block(&se.pre);    
}     
     
     
   
   
/* deallocate_allocatable()-- Deallocate an allocatable array */         
         
static void deallocate_allocatable(g95_se *se, g95_expr *expr, tree stat_var) {        
g95_se se0;        
tree tmp;          
          
  g95_init_se(&se0, NULL);    
  se0.reflevel = 1;       
  g95_conv_expr(&se0, expr);          
          
  g95_add_block_to_block(&se->pre, &se0.pre);       
  g95_add_block_to_block(&se->post, &se0.post); 
 
  tmp = build_int_2(static_alloc_symbol(expr->symbol), 0);     
  tmp = g95_call_library(void_type_node, PREFIX "deallocate_allocatable",
			 se0.expr, tmp, stat_var, NULL_TREE);      
      
  g95_add_expr_to_block(&se->pre, tmp);          
}       
       
       
 
 
/* allocate_char_pointer_array()-- Allocate a pointer character array.
 * Instead of just a pointer to the array descriptor, we have a
 * character descriptor that points to the descriptor. */

static tree allocate_char_pointer_array(g95_expr *expr, g95_se *se,    
					g95_array_ref *ar, tree stat_var) {       
tree tmp, args;        
        
  se->reflevel = 2;         
  g95_conv_expr(se, expr);    
    
  tmp = G95_TYPE_STRING_LENGTH(TREE_TYPE(se->expr));  
  init_section_info(se, ar, expr, tmp);   
   
  args = g95_chainon_list(NULL_TREE, se->expr);      
  args = g95_chainon_list(args, stat_var);       
       
  return args;        
}    
    
    
      
      
/* trans_select()-- Translate a SELECT block */          
          
static tree trans_select(g95_code *code) {   
g95_expr *expr;       
tree tmp;  
  
  /* Normal select statements put the condition in expr, computed GOTO
   * statements put it in expr2. */       
       
  expr = (code->expr == NULL) ? code->expr2 : code->expr;

  switch(expr->ts.type) {
  case BT_INTEGER:    tmp = integer_select(code, expr);    break;          
  case BT_LOGICAL:    tmp = logical_select(code, expr);    break;  
  case BT_CHARACTER:  tmp = character_select(code, expr);  break;       
  default:   
    g95_internal_error("g95_trans_select(): Bad type");
    tmp = NULL_TREE;   
  }       
       
  return tmp;       
}



  
  
/* trans_if()-- Translate an IF statement.  */          
          
static tree trans_if(g95_code *code) {          
tree then_stmt, else_stmt, predicate, tmp;       
stmtblock_t block;          
g95_se se;         
         
  g95_init_se(&se, NULL); 
  g95_start_block(&block);       
       
  g95_conv_expr(&se, code->expr);    
    
  g95_add_block_to_block(&block, &se.pre);     
     
  then_stmt = g95_trans_code(code->block);    
  if (then_stmt == NULL_TREE) then_stmt = empty_stmt_node;      
      
  else_stmt = g95_trans_code(code->ext.block);         
  if (else_stmt == NULL_TREE) else_stmt = empty_stmt_node;       
       
  /* If there is a post chain, we have to stuff the result into a
   * temporary variable, clean up, then take the branch based on the
   * variable. */     
     
  if (se.post.head == NULL_TREE) {         
    predicate = se.expr;     
  } else {
    predicate = g95_create_var(boolean_type_node, NULL);       
    g95_add_modify_expr(&block, predicate, se.expr);     
     
    g95_add_block_to_block(&block, &se.post);          
  }       
       
  tmp = build_v(COND_EXPR, predicate, then_stmt, else_stmt);    
  g95_add_expr_to_block(&block, tmp);         
         
  return g95_finish_block(&block);         
}


 
 
/* allocate_allocatable_array()-- Generate code to allocate a pointer
 * array.  The section_info[] array is initialized in the same manner
 * as for allocate_pointer_array().  The calling sequence is different
 * as we pass the address of the descriptor. */   
   
static void allocate_allocatable_array(g95_expr *expr, g95_se *se,   
				       g95_array_ref *ar, tree stat_var) {       
tree tmp;         
         
  init_section_info(se, ar, expr, NULL);

  se->reflevel = 1;  
  g95_conv_expr(se, expr);

  tmp = build_int_2(static_alloc_symbol(expr->symbol), 0);         
  tmp = g95_call_library(void_type_node, PREFIX "allocate_allocatable_array",          
			 se->expr, tmp, stat_var, NULL_TREE);   
   
  g95_add_expr_to_block(&se->pre, tmp);    
}       
       
       
         
         
/* allocate_nonchar_pointer_array()-- Allocate an array of a
 * noncharacter type */    
    
static tree allocate_nonchar_pointer_array(g95_expr *expr, g95_se *se,   
					   g95_array_ref *ar, tree stat_var) {       
tree args;        
        
  init_section_info(se, ar, expr, NULL);       
       
  se->reflevel = 2;        
  g95_conv_expr(se, expr);        
        
  args = g95_chainon_list(NULL_TREE, se->expr);    
  args = g95_chainon_list(args, stat_var);         
  return args;
}         
         
         
   
   
/* allocate_pointer_array()-- Generate code to allocate a pointer
 * array. */     
     
static void allocate_pointer_array(bt type, g95_expr *expr, g95_se *se,         
				   g95_array_ref *ar, tree stat_var) {       
tree tmp, args;     
     
  if (type == BT_CHARACTER)  
    args = allocate_char_pointer_array(expr, se, ar, stat_var);        
  else        
    args = allocate_nonchar_pointer_array(expr, se, ar, stat_var);          
          
  tmp = g95_build_function_call(library_allocate_pointer_array, args);      
  g95_add_expr_to_block(&se->pre, tmp);
}          
          
          
        
        
/* trans_deallocate()-- Translate a DEALLOCATE statement */         
         
static tree trans_deallocate(g95_code *code) {  
symbol_attribute attr;
tree tmp, stat_var;       
stmtblock_t block;         
g95_alloc *alloc;
g95_typespec ts;
g95_expr *expr;        
int cp_flag;
g95_se se;      
      
  g95_init_se(&se, NULL);      
  g95_set_error_locus(&se.pre, &code->ext.alloc_list->expr->where);         
         
  if (code->expr == NULL)        
    stat_var = null_pointer_node;       
  else {  
    se.reflevel = 1;       
    g95_conv_expr(&se, code->expr);          
    stat_var = save_expr(se.expr);          
          
    tmp = build1(INDIRECT_REF, g95_default_integer, stat_var);    
    g95_add_modify_expr(&se.pre, tmp, integer_zero_node);        
  }   
   
  for(alloc=code->ext.alloc_list; alloc; alloc=alloc->next) {
    expr = alloc->expr;    
    attr = g95_variable_attr(expr, &ts); 
 
    if (expr->symbol->attr.allocatable && expr->ref->type == REF_ARRAY &&  
	expr->ref->next == NULL)       
      deallocate_allocatable(&se, expr, stat_var); 
    else {     
      cp_flag = (expr->ts.type == BT_CHARACTER) && attr.pointer;         
      deallocate_pointer(cp_flag, &se, expr, stat_var);
    } 
  }     
     
  g95_init_block(&block);       
       
  g95_add_block_to_block(&block, &se.pre); 
  g95_add_block_to_block(&block, &se.post);    
    
  return g95_finish_block(&block);       
} 
 
 
    
    
/* trans_allocate()-- Translate an ALLOCATE statement.  If the
 * statement has a STAT variable, we zero it, then generate statements
 * that initialize the right descriptors followed by allocate_array()
 * calls for arrays or just simple allocate_scalar() calls.  The
 * library calls update STAT if something goes wrong, or do nothing if
 * the STAT variable is already nonzero or terminate the program
 * if something goes wrong and there is no STAT variable. */  
  
static tree trans_allocate(g95_code *code) {          
tree tmp, stat_var;         
stmtblock_t block;     
g95_ref *k, *r;     
g95_alloc *alloc;   
g95_expr *expr;  
g95_se se;      
      
  g95_init_se(&se, NULL);  
  g95_set_error_locus(&se.pre, &code->ext.alloc_list->expr->where);         
         
  if (code->expr == NULL)          
    stat_var = null_pointer_node;
  else {  
    se.reflevel = 1;         
    g95_conv_expr(&se, code->expr); 
    stat_var = save_expr(se.expr);          
          
    /* Init the status to success */        
        
    tmp = build1(INDIRECT_REF, g95_default_integer, stat_var);         
    g95_add_modify_expr(&se.pre, tmp, integer_zero_node);        
  }       
       
  /* Allocate expressions are messed up in the sense that the
   * specification is stored as an array reference.  This tail
   * reference is temporarily pruned from the reference list when
   * actually generating code so that we get the pointer to the array
   * or the allocatable array itself. */  
  
  for(alloc=code->ext.alloc_list; alloc; alloc=alloc->next) {       
    expr = alloc->expr; 
 
    if (expr->ref == NULL) {  
      allocate_scalar(expr, &se, stat_var); 
      continue;   
    }         
         
    k = NULL; 
 
    for(r=expr->ref; r; r=r->next) {       
      if (r->type == REF_ARRAY && r->next == NULL) break;    
      k = r; 
    }     
     
    if (r == NULL) {  /* Scalar pointer */          
      allocate_scalar(expr, &se, stat_var);          
      continue;        
    }

    if (expr->ref == r) {    
      expr->ref = NULL;     
     
      if (expr->symbol->attr.allocatable) 
	allocate_allocatable_array(expr, &se, &r->u.ar, stat_var);
      else  
	allocate_pointer_array(expr->ts.type, expr, &se, &r->u.ar, stat_var); 
 
      expr->ref = r;        
    } else {   
      k->next = NULL;       
      allocate_pointer_array(expr->ts.type, expr, &se, &r->u.ar, stat_var);         
      k->next = r;     
    }    
  }    
    
  g95_init_block(&block);     
     
  g95_add_block_to_block(&block, &se.pre);         
  g95_add_block_to_block(&block, &se.post);    
    
  return g95_finish_block(&block);  
}   
   
   


/* g95_trans_code()-- Translate a list of executable statements. */          
          
tree g95_trans_code(g95_code *code) {  
stmtblock_t block;   
tree res;   
   
  if (code == NULL) return NULL_TREE;       
       
  g95_start_block(&block);          
          
  for(; code; code=code->next) {
    g95_set_backend_locus(&code->loc);       
    if (code->here != 0) {
      res = build_v(LABEL_EXPR, g95_get_label_decl(code->here));  
      wrap_all_with_wfl(&res, input_filename, lineno);       
      g95_add_expr_to_block(&block, res);     
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
      g95_add_expr_to_block (&block, res);         
    }   
  }          
          
  /* Return the finished block. */        
  return g95_finish_block(&block);         
}  
