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
 
 
       
       
/* logical_select()-- Build a logical selection statement. */       
       
static tree logical_select(g95_code *codep, g95_expr *selector) {  
g95_case *v, *true_case, *false_case, *default_case;         
stmtblock_t block, list;          
tree t, end_label;   
g95_se se;        
        
  true_case = false_case = default_case = NULL;       
       
  v = codep->block->ext.case_list;    
    
  while(v->cprev != NULL)        
    v = v->cprev;

  /* Extract out the cases */         
         
  for(; v; v=v->cnext) {        
    if (v->low == NULL && v->high == NULL) {          
      default_case = v;
      continue;        
    }    
    
    if (v->low->value.logical)
      true_case = v;     
    else     
      false_case = v;         
  }      
      
  g95_init_block(&block); 
  g95_init_block(&list);         
         
  g95_init_se(&se, NULL);
  g95_conv_expr(&se, selector);          
          
  end_label = g95_build_label_decl(NULL_TREE);        
        
  if (false_case != NULL) {          
    t = build_v(CASE_LABEL_EXPR, integer_zero_node, NULL_TREE);    
    g95_add_expr_to_block(&list, t);        
        
    t = g95_trans_code(false_case->code);       
    g95_add_expr_to_block(&list, t);

    t = build_v(GOTO_EXPR, end_label);         
    g95_add_expr_to_block(&list, t);
  }    
    
  if (true_case != NULL) {  
    t = build_v(CASE_LABEL_EXPR, integer_one_node, NULL_TREE);         
    g95_add_expr_to_block(&list, t);     
     
    t = g95_trans_code(true_case->code);   
    g95_add_expr_to_block(&list, t);          
          
    t = build_v(GOTO_EXPR, end_label); 
    g95_add_expr_to_block(&list, t);          
  }       
       
  if (default_case != NULL) {     
    t = build_v(CASE_LABEL_EXPR, NULL_TREE, NULL_TREE);        
    g95_add_expr_to_block(&list, t); 
 
    t = g95_trans_code(default_case->code);       
    g95_add_expr_to_block(&list, t);     
     
    t = build_v(GOTO_EXPR, end_label);   
    g95_add_expr_to_block(&list, t);     
  } 
 
  t = g95_finish_block(&list);   
  t = build_v(SWITCH_EXPR, convert(g95_default_integer, se.expr),  
		t, NULL_TREE);          
          
  g95_add_expr_to_block(&block, t);    
    
  t = build_v(LABEL_EXPR, end_label); 
  g95_add_expr_to_block(&block, t); 
 
  return g95_finish_block(&block);     
}     
     
     
        
        
/* allocate_scalar()-- Allocate a scalar object. */        
        
static void allocate_scalar(g95_expr *e, g95_se *se, tree stat_var) {      
tree l, tmp0;       
g95_se se1;      
      
  g95_init_se(&se1, NULL);          
  se1.reflevel = 2;     
     
  g95_conv_expr(&se1, e);
  g95_add_block_to_block(&se->pre, &se1.pre);     
  g95_add_block_to_block(&se->post, &se1.post);   
   
  if (e->ts.type == BT_CHARACTER) {       
    se1.reflevel = 0;
    l = g95_conv_char_length(&se1, &e->ts);

    tmp0 = g95_call_library(void_type_node, PREFIX "allocate_string",    
			   se1.expr, l, stat_var, NULL_TREE);     
  } else {      
    l = size_in_bytes(g95_typenode_for_spec(&e->ts));   
    tmp0 = g95_call_library(void_type_node, PREFIX "allocate_scalar",       
			   se1.expr, l, stat_var, NULL_TREE);      
  }  
  
  g95_add_expr_to_block(&se->pre, tmp0);
}       
       
       
         
         
/* init_section_info()-- Initialize the section_info[] array with
 * information about the array being allocated.  The first integer is
 * the rank, the second is the element size.  This is followed by a
 * pair of integers for each dimension that give the upper and lower
 * bound. */   
   
static void init_section_info(g95_se *s, g95_array_ref *as, g95_expr *e1) {  
g95_typespec *typesp; 
g95_se exp;          
tree tmp1; 
int v, f;    
    
  f = 0;   
  typesp = &e1->ts;  
  g95_set_section_info(s, f++, build_int_2(as->dimen, 0));          
          
  tmp1 = (typesp->type != BT_CHARACTER)         
    ? size_in_bytes(g95_typenode_for_spec(typesp))      
    : typesp->cl->backend_decl;  
  
  g95_set_section_info(s, f++, tmp1);      
      
  for(v=0; v<as->dimen; v++) {         
    switch(as->dimen_type[v]) {       
    case DIMEN_ELEMENT:      
      g95_set_section_info(s, f++, integer_one_node);

      g95_init_se(&exp, NULL);     
      g95_conv_expr(&exp, as->start[v]);        
        
      g95_add_block_to_block(&s->pre, &exp.pre);   
      g95_set_section_info(s, f++, exp.expr); 
      g95_add_block_to_block(&s->post, &exp.post);      
      
      break;        
        
    case DIMEN_RANGE:      
      g95_init_se(&exp, NULL);
      g95_conv_expr(&exp, as->start[v]);          
          
      g95_add_block_to_block(&s->pre, &exp.pre); 
      g95_set_section_info(s, f++, exp.expr);  
      g95_add_block_to_block(&s->post, &exp.post);     
     
      g95_init_se(&exp, NULL);       
      g95_conv_expr(&exp, as->end[v]);     
     
      g95_add_block_to_block(&s->pre, &exp.pre);      
      g95_set_section_info(s, f++, exp.expr);          
      g95_add_block_to_block(&s->post, &exp.post); 
 
      break;        
        
    default:  
      g95_internal_error("init_section_info(): Bad array ref");   
    }    
  }      
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
  
static tree trans_do(g95_code *codep) {    
tree dovar, frm, d, step, c, typ, cond, cycle_label, exit_label, t;       
stmtblock_t blk, body;  
g95_se se1;          
          
  g95_init_block(&blk);      
      
  /* Create SIMPLE versions of all expressions.  */     
  g95_init_se(&se1, NULL);
  g95_conv_expr(&se1, codep->ext.iterator->var);       
  g95_add_block_to_block(&blk, &se1.pre);        
  dovar = se1.expr;          
  typ = TREE_TYPE(dovar); 
 
  g95_init_se(&se1, NULL);          
  g95_conv_expr_type(&se1, codep->ext.iterator->start, typ);  
  g95_add_block_to_block(&blk, &se1.pre);   
  frm = se1.expr;         
         
  g95_init_se(&se1, NULL);        
  g95_conv_expr_type(&se1, codep->ext.iterator->end, typ);    
  g95_add_block_to_block(&blk, &se1.pre);     
  d = se1.expr;         
         
  g95_init_se(&se1, NULL);         
  g95_conv_expr_type(&se1, codep->ext.iterator->step, typ);  
  
  g95_add_block_to_block(&blk, &se1.pre);    
  step = se1.expr;

  /* Initialize the trip count.  This code is executed before we enter
   * the loop body.  We generate: count = (to + step - from) / step.  */ 
 
  t = fold(build(MINUS_EXPR, typ, step, frm));        
  t = fold(build(PLUS_EXPR, typ, d, t));        
  t = fold(build(TRUNC_DIV_EXPR, typ, t, step));      
      
  c = g95_create_var(typ);
  g95_add_modify_expr(&blk, c, t);       
       
  /* Initialize the DO variable: dovar = from.  */      
  g95_add_modify_expr(&blk, dovar, frm);   
   
  /* Loop body */      
  g95_init_block(&body);

  /* Cycle and exit statements are implemented with gotos.  Put these
   * labels where they can be found later. We put the labels in a
   * TREE_LIST node (because TREE_CHAIN is already used). cycle_label
   * goes in TREE_PURPOSE (backend_decl), exit label in TREE_VALUE
   * (backend_decl).  */  
  
  cycle_label = g95_build_label_decl(NULL_TREE);  
  exit_label  = g95_build_label_decl(NULL_TREE);      
  codep->backend_decl = tree_cons(cycle_label, exit_label, NULL);         
         
  /* Start with the loop condition.  Loop until trip count <= 0.  */        
  cond = build(LE_EXPR, boolean_type_node, c, integer_zero_node);     
  t = build_v(GOTO_EXPR, exit_label);        
  TREE_USED(exit_label) = 1;          
  t = build_v(COND_EXPR, cond, t, empty_stmt_node);        
  g95_add_expr_to_block(&body, t);      
      
      
  /* Main loop body. */       
  t = g95_trans_code(codep->block); 
  g95_add_expr_to_block(&body, t);      
      
  /* Label for cycle statements (if needed). */         
  if (TREE_USED(cycle_label)) {        
    t = build_v(LABEL_EXPR, cycle_label);      
    g95_add_expr_to_block(&body, t);     
  }     
     
  /* Increment the loop variable. */       
  t = build(PLUS_EXPR, typ, dovar, step);   
  g95_add_modify_expr(&body, dovar, t); 
 
  /* Decrement the trip count. */
  t = build(MINUS_EXPR, typ, c, integer_one_node);   
  g95_add_modify_expr(&body, c, t);  
  
  /* End of loop body. */   
  t = g95_finish_block(&body);         
         
  /* The for loop itself. */    
  t = build_v(LOOP_EXPR, t); 
  g95_add_expr_to_block(&blk, t);         
         
  /* Add the exit label */  
  t = build_v(LABEL_EXPR, exit_label);   
  g95_add_expr_to_block(&blk, t);        
        
  return g95_finish_block(&blk);   
}    
    
    
     
     
/* trans_pause()-- Translate a PAUSE statement */  
  
static tree trans_pause(g95_code *cp) {   
stmtblock_t block;      
g95_se se0;       
tree t; 
bt type;      
      
  g95_init_block(&block);          
          
  if (cp->expr == NULL) {          
    t = g95_call_library(void_type_node, PREFIX "pause", NULL_TREE);          
    g95_add_expr_to_block(&block, t);     
  } else {          
    type = cp->expr->ts.type;
    g95_init_se(&se0, NULL);      
      
    if (type == BT_CHARACTER) se0.reflevel = 1;
    g95_conv_expr(&se0, cp->expr);          
    g95_add_block_to_block(&block, &se0.pre);         
         
    if (type == BT_INTEGER)  
      t = g95_call_library(void_type_node, PREFIX "pause_integer", se0.expr,         
			     NULL);   
    else     
      t = g95_call_library(void_type_node, PREFIX "pause_string", se0.expr,         
			     se0.string_length, NULL);       
       
    g95_add_expr_to_block(&block, t);    
    g95_add_block_to_block(&block, &se0.post);        
  }      
      
  return g95_finish_block(&block);
}        
        
        
     
     
/* trans_stop()-- Translate a STOP statement */ 
 
static tree trans_stop(g95_code *c) { 
g95_se s;  
tree tmp1;  
  
  g95_init_se(&s, NULL);
  g95_init_block(&s.pre);

  if (c->expr == NULL) {   
    tmp1 = build_int_2(c->ext.stop_code, 0);          
    tmp1 = g95_call_library(void_type_node, PREFIX "stop_numeric",       
			   tmp1, NULL_TREE); 
  } else {     
    s.reflevel = 1;     
    g95_conv_expr(&s, c->expr);

    tmp1 = g95_call_library(void_type_node, PREFIX "stop_string",          
			   s.expr, s.string_length, NULL_TREE);       
  }   
   
  g95_set_error_locus(&s.pre, &c->where);    
  g95_add_expr_to_block(&s.pre, tmp1);       
       
  return g95_finish_block(&s.pre);        
}      
      
      
       
       
/* character_select()-- Implement a character SELECT statement.  We
 * generate an array of structures describing the cases in order and
 * call a library subroutine that locates the right case.  The library
 * subroutine returns a pointer to jump to or NULL if no branches are
 * to be taken. */  
  
static tree character_select(g95_code *codep, g95_expr *selector) {   
tree iv, node, end_label, l, tmp, *labels, default_jump;         
stmtblock_t b, list;         
g95_case *cp, *s;         
g95_se se1, se2;      
g95_code *o;        
int h, r; 
 
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

  r = 0;     
  for(s=cp; s; s=s->cnext)      
    s->n = r++;       
       
  labels = NULL;  
  if (r != 0) labels = g95_getmem(r*sizeof(tree)); 
 
  for(h=0; h<r; h++) 
    labels[h] = g95_build_label_decl(NULL_TREE);     
     
  end_label = g95_build_label_decl(NULL_TREE);       
       
/* Generate the body */   
   
  g95_init_block(&b);          
  g95_init_block(&list); 
  g95_init_se(&se2, NULL);  
  
  for(o=codep->block; o; o=o->block) {      
    for(s=o->ext.case_list; s; s=s->next) {       
      tmp = build_v(LABEL_EXPR, labels[s->n]);        
      g95_add_expr_to_block(&list, tmp);        
    }    
    
    tmp = g95_trans_code(o->next);         
    g95_add_expr_to_block(&list, tmp);      
      
    tmp = build_v(GOTO_EXPR, end_label); 
    g95_add_expr_to_block(&list, tmp);        
  } 
 
/* Generate the structure describing the branches */

  iv = NULL_TREE;   
   
  h = 0;        
  for(s=cp; s; s=s->cnext, h++) {   
    node = NULL_TREE;  
  
    if (s->low == NULL) {       
      node = tree_cons(ss_string1_len, integer_zero_node, node);       
      node = tree_cons(ss_string1, null_pointer_node, node);        
    } else {     
      g95_conv_expr(&se2, s->low);          
      tmp = build1(ADDR_EXPR, pchar_type_node, se2.expr);      
      
      node = tree_cons(ss_string1_len, se2.string_length, node);
      node = tree_cons(ss_string1, tmp, node);   
    }   
   
    if (s->high == NULL) { 
      node = tree_cons(ss_string2_len, integer_zero_node, node);      
      node = tree_cons(ss_string2, null_pointer_node, node);   
    } else {       
      g95_conv_expr(&se2, s->high);        
      tmp = build1(ADDR_EXPR, pchar_type_node, se2.expr);     
     
      node = tree_cons(ss_string2_len, se2.string_length, node);      
      node = tree_cons(ss_string2, tmp, node);       
    }       
       
    tmp = build1(ADDR_EXPR, pvoid_type_node, labels[h]);          
    node = tree_cons(ss_target, tmp, node);   
   
    tmp = build(CONSTRUCTOR, select_struct, NULL_TREE, nreverse(node));       
    iv = tree_cons(NULL_TREE, tmp, iv);
  }        
        
  tmp = build_array_type(select_struct, build_index_type(build_int_2(r-1, 0)));    
    
  iv = build(CONSTRUCTOR, tmp, NULL_TREE, nreverse(iv));     
  TREE_CONSTANT(iv) = 1;         
  TREE_STATIC(iv) = 1;  
  
  /* Build an argument list for the library call */   
   
  iv = build1(ADDR_EXPR, pvoid_type_node, iv);        
  l = build_int_2(r, 0);    
  default_jump = build1(ADDR_EXPR, pvoid_type_node, end_label);       
       
  g95_init_se(&se1, NULL);  
  se1.reflevel = 1; 
  g95_conv_expr(&se1, selector);     
     
  g95_add_block_to_block(&b, &se1.pre);          
          
  tmp = g95_call_library(pvoid_type_node, PREFIX "select_string",
			 iv, l, default_jump,    
			 se1.expr, se1.string_length, NULL_TREE);      
      
  tmp = build1(GOTO_EXPR, void_type_node, tmp);
  g95_add_expr_to_block(&b, tmp);   
   
  tmp = g95_finish_block(&list);   
  g95_add_expr_to_block(&b, tmp);    
    
  tmp = build_v(LABEL_EXPR, end_label);        
  g95_add_expr_to_block(&b, tmp);   
   
  if (r != 0) g95_free(labels);          
          
  return g95_finish_block(&b); 
}          
          
          
      
      
/* alt_return_jump()-- Generate a switch to jump to the right
 * alternate return point. */     
     
static tree alt_return_jump(tree var0, g95_actual_arglist *f) {   
stmtblock_t block;     
tree tmp0;       
int l;

  g95_init_block(&block); 
  l = 1;         
         
  for(; f!=NULL; f=f->next) {   
    if (f->type != ALT_RETURN) continue;       
       
    tmp0 = build_int_2(l++, 0);      
    tmp0 = build_v(CASE_LABEL_EXPR, tmp0, tmp0); 
    g95_add_expr_to_block(&block, tmp0);          
          
    tmp0 = g95_get_label_decl(f->u.label);
    tmp0 = build_v(GOTO_EXPR, tmp0);   
    g95_add_expr_to_block(&block, tmp0);       
  }    
    
  tmp0 = g95_finish_block(&block);
  return build_v(SWITCH_EXPR, var0, tmp0, NULL_TREE);     
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
     
     
      
      
static void deallocate_pointer(g95_se *s, g95_expr *exp, tree stat_var) {        
g95_se se2;
tree t;      
      
  g95_init_se(&se2, NULL);        
  se2.reflevel = 2;          
  g95_conv_expr(&se2, exp);   
   
  g95_add_block_to_block(&s->pre, &se2.pre);      
  g95_add_block_to_block(&s->post, &se2.post);   
   
  t = g95_call_library(void_type_node, PREFIX "deallocate_pointer",      
			 se2.expr, stat_var, NULL_TREE);          
          
  g95_add_expr_to_block(&s->pre, t);      
}       
       
       
         
         
/* trans_call()-- Translate a CALL statement */  
  
static tree trans_call(g95_code *code) {   
tree tmp1, variable, args, nam, dec;          
g95_se expr;          
          
  if (code->isym == NULL) {    
    dec = code->sym->backend_decl;   
    if (POINTER_TYPE_P(TREE_TYPE(dec)))     
      dec = build1(INDIRECT_REF, TREE_TYPE(TREE_TYPE(dec)), dec);          
          
  } else {
    dec = g95_conv_intrinsic_subroutine(code);
    if (dec != NULL_TREE) return dec;      
      
    /* Call sub_name */  
  
    nam = get_identifier(code->sub_name); 
    dec = build_function_type(void_type_node, NULL_TREE);    
    dec = build_decl(FUNCTION_DECL, nam, dec);          
          
    DECL_EXTERNAL(dec) = 1;    
    TREE_PUBLIC(dec) = 1;        
        
    pushdecl(dec);          
    rest_of_decl_compilation(dec, NULL, 1, 0);         
  }      
      
  g95_init_se(&expr, NULL);
  g95_init_block(&expr.pre);    
    
  args = g95_trans_arglist(code->ext.actual, &expr);
  tmp1  = g95_build_function_call(dec, args);    
    
  if (!g95_has_alt_return(code->ext.actual)) {     
    g95_add_expr_to_block(&expr.pre, tmp1);          
    g95_add_block_to_block(&expr.pre, &expr.post); 
  } else { 
    variable = g95_create_var(g95_default_integer);    
    g95_add_modify_expr(&expr.pre, variable, tmp1);     
     
    g95_add_block_to_block(&expr.pre, &expr.post);        
        
    tmp1 = alt_return_jump(variable, code->ext.actual);    
    g95_add_expr_to_block(&expr.pre, tmp1); 
  }       
       
  return g95_finish_block(&expr.pre);    
}     
     
     
          
          
/* trans_cycle()-- CYCLE a DO loop. The label decl has already
 * been created by g95_trans_do(), it's in TREE_PURPOSE(backend_decl)
 * of the g95_code node at the head of the loop. We must mark the
 * label as used. */       
       
static tree trans_cycle(g95_code *codep) {    
tree cycle_label;       
       
  cycle_label = TREE_PURPOSE(codep->ext.block->backend_decl);
  TREE_USED(cycle_label) = 1;         
  return build_v(GOTO_EXPR, cycle_label);      
}


      
      
/* trans_return()-- Translate a RETURN statement */ 
 
static tree trans_return(g95_code *cp) {
stmtblock_t blk;
g95_se se0;  
tree tmp1;   
   
  g95_init_block(&blk);

  if (cp->expr != NULL) {        
    g95_init_se(&se0, NULL);         
    g95_conv_expr(&se0, cp->expr); 
 
    g95_add_block_to_block(&blk, &se0.pre);
    g95_add_modify_expr(&blk, g95_result_var_decl, se0.expr);      
    g95_add_block_to_block(&blk, &se0.post);          
  }

  tmp1 = build_v(GOTO_EXPR, g95_get_return_label());   
  g95_add_expr_to_block(&blk, tmp1);    
    
  return g95_finish_block(&blk);         
}        
        
        
  
  
/* trans_goto()-- Translate a GOTO statement */      
      
static tree trans_goto(g95_code *code) {  
  
  return build_v(GOTO_EXPR, g95_get_label_decl(code->label));      
}     
     
     
       
       
/* procedure_mem()-- Return an integer node indicating which memory
 * block we are dealing with-- procedure or user memory. */          
          
static tree procedure_mem(g95_expr *d) {         
symbol_attribute a;   
g95_symbol *sy;

  sy = d->symbol;         
  a = g95_variable_attr(d, NULL);     
     
  if (a.pointer || sy->attr.save || sy->attr.use_assoc ||          
      g95_module_symbol(sy) || sy->attr.artificial)          
    return integer_one_node;       
       
  return integer_zero_node;          
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
    
    
         
         
/* allocate_array()-- Generate code to allocate an array. */   
   
static void allocate_array(g95_expr *exp, g95_se *se1, g95_array_ref *ar, 
			   tree stat_var) {  
tree t, pointer;     
symbol_attribute attribute;        
        
  init_section_info(se1, ar, exp);     
     
  se1->reflevel = 1;
  g95_conv_expr(se1, exp);   
   
  attribute = g95_variable_attr(exp, NULL);     
  pointer = attribute.pointer ? integer_one_node : integer_zero_node;        
        
  t = procedure_mem(exp);  
  t = g95_call_library(void_type_node, PREFIX "allocate_array",        
			 se1->expr, pointer, t, stat_var, NULL_TREE);     
     
  g95_add_expr_to_block(&se1->pre, t);  
}


    
    
/* integer_select()-- Build an integer selection statement */    
    
static tree integer_select(g95_code *codep, g95_expr *selector) {     
tree end_label, ulow_label, uhigh_label, tmp, low, high, goto_expr;  
g95_case *cp, *unbounded_low, *unbounded_high;          
stmtblock_t b, body;    
g95_code *z;        
g95_se s;     
int knd;       
       
  g95_init_block(&b);          
          
  g95_init_se(&s, NULL);      
  g95_conv_expr(&s, selector);         
         
  knd = selector->ts.kind;         
         
  end_label = g95_build_label_decl(NULL_TREE);         
         
  g95_init_block(&body);          
          
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

  g95_add_block_to_block(&b, &s.pre);     
     
/* Build branch statements to the unbounded cases */

  if (unbounded_high != NULL) {  
    high = g95_conv_mpz_to_tree(unbounded_high->high->value.integer, knd);    
    tmp = build(LE_EXPR, boolean_type_node, s.expr, high);    
    
    goto_expr = build_v(GOTO_EXPR, uhigh_label);     
     
    tmp = build_v(COND_EXPR, tmp, goto_expr, empty_stmt_node);      
    g95_add_expr_to_block(&b, tmp);  
  }   
   
  if (unbounded_low != NULL) { 
    low = g95_conv_mpz_to_tree(unbounded_low->low->value.integer, knd); 
    tmp = build(GE_EXPR, boolean_type_node, s.expr, low);     
     
    goto_expr = build_v(GOTO_EXPR, ulow_label);  
  
    tmp = build_v(COND_EXPR, tmp, goto_expr, empty_stmt_node);         
    g95_add_expr_to_block(&b, tmp);         
  }   
   
  /* Build the body */          
          
  for(z=codep->block; z; z=z->block) {      
    for(cp=z->ext.case_list; cp; cp=cp->next) {         
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
	low = g95_conv_mpz_to_tree(cp->low->value.integer, knd);  
  
	if (cp->low == cp->high)        
	  high = NULL_TREE;
	else  
	  high = g95_conv_mpz_to_tree(cp->high->value.integer, knd);      
      }     
     
      /* Add this case label.  */  
  
      tmp = build_v(CASE_LABEL_EXPR, low, high);        
      g95_add_expr_to_block(&body, tmp);       
    }    
    
    /* Add the statements for this case.  */         
         
    tmp = g95_trans_code(z->next);     
    g95_add_expr_to_block(&body, tmp); 
 
    /* Break to the end of the loop. */     
     
    tmp = build_v(GOTO_EXPR, end_label);        
    g95_add_expr_to_block(&body, tmp); 
  }         
         
  tmp = g95_finish_block(&body); 
  tmp = build_v(SWITCH_EXPR, s.expr, tmp, NULL_TREE);         
  g95_add_expr_to_block(&b, tmp);         
         
  tmp = build_v(LABEL_EXPR, end_label);        
  g95_add_expr_to_block(&b, tmp);      
      
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
tree tmp, stat_var; 
stmtblock_t block;          
g95_ref *x, *r;   
g95_alloc *alloc;        
g95_expr *exp;         
g95_se se1;          
          
  g95_init_se(&se1, NULL);    
  g95_set_error_locus(&se1.pre, &cp->ext.alloc_list->expr->where);         
         
  if (cp->expr == NULL)    
    stat_var = null_pointer_node;        
  else {       
    se1.reflevel = 1;        
    g95_conv_expr(&se1, cp->expr);          
    stat_var = save_expr(se1.expr);   
   
    /* Init the status to success */      
      
    tmp = build1(INDIRECT_REF, g95_default_integer, stat_var);   
    g95_add_modify_expr(&se1.pre, tmp, integer_zero_node);         
  }     
     
  /* Allocate expressions are messed up in the sense that the
   * specification is stored as an array reference.  This tail
   * reference is temporarily pruned from the reference list when
   * actually generating code so that we get the pointer to the array
   * or the allocatable array itself. */

  for(alloc=cp->ext.alloc_list; alloc; alloc=alloc->next) {  
    exp = alloc->expr;     
     
    if (exp->ref == NULL) {
      allocate_scalar(exp, &se1, stat_var);          
      continue;
    }          
          
    x = NULL;

    for(r=exp->ref; r; r=r->next) {        
      if (r->type == REF_ARRAY && r->next == NULL) break;        
      x = r;
    }     
     
    if (r == NULL) {  /* Scalar pointer */       
      allocate_scalar(exp, &se1, stat_var); 
      continue;      
    }         
         
    if (exp->ref == r) {      
      exp->ref = NULL; 
 
      allocate_array(exp, &se1, &r->u.ar, stat_var);     
      exp->ref = r;          
    } else {       
      x->next = NULL;         
         
      allocate_array(exp, &se1, &r->u.ar, stat_var);       
      x->next = r;   
    } 
  }

  g95_init_block(&block);         
         
  g95_add_block_to_block(&block, &se1.pre);         
  g95_add_block_to_block(&block, &se1.post);     
     
  return g95_finish_block(&block);          
}    
    
    
     
     
/* deallocate_array()-- Deallocate an allocatable array or an
 * array pointer. */      
      
static void deallocate_array(g95_se *s, g95_expr *exp, tree stat_var) {         
g95_se se2; 
tree t;      
      
  g95_init_se(&se2, NULL);         
  se2.reflevel = 1;
  g95_conv_expr(&se2, exp);

  g95_add_block_to_block(&s->pre, &se2.pre); 
  g95_add_block_to_block(&s->post, &se2.post);      
      
  t = procedure_mem(exp);       
  t = g95_call_library(void_type_node, PREFIX "deallocate_array",
			 se2.expr, t, stat_var, NULL_TREE);     
     
  g95_add_expr_to_block(&s->pre, t);   
} 
 
 
         
         
/* trans_if()-- Translate an IF statement.  */         
         
static tree trans_if(g95_code *codep) {       
tree then_stmt, else_stmt, predicate, tmp1;       
stmtblock_t block;     
g95_se se;    
    
  g95_init_se(&se, NULL);      
  g95_init_block(&block);     
     
  g95_conv_expr(&se, codep->expr); 
 
  g95_add_block_to_block(&block, &se.pre);  
  
  then_stmt = g95_trans_code(codep->block);          
  if (then_stmt == NULL_TREE) then_stmt = empty_stmt_node;      
      
  else_stmt = g95_trans_code(codep->ext.block);          
  if (else_stmt == NULL_TREE) else_stmt = empty_stmt_node;         
         
  /* If there is a post chain, we have to stuff the result into a
   * temporary variable, clean up, then take the branch based on the
   * variable. */ 
 
  if (se.post.head == NULL_TREE) {         
    predicate = se.expr;
  } else {        
    predicate = g95_create_var(boolean_type_node); 
    g95_add_modify_expr(&block, predicate, se.expr);       
       
    g95_add_block_to_block(&block, &se.post);     
  }     
     
  tmp1 = build_v(COND_EXPR, predicate, then_stmt, else_stmt);        
  g95_add_expr_to_block(&block, tmp1);      
      
  return g95_finish_block(&block);    
}      
      
      
    
    
/* trans_select()-- Translate a SELECT block */      
      
static tree trans_select(g95_code *c) {      
g95_expr *e1;  
tree tmp1; 
 
  /* Normal select statements put the condition in expr, computed GOTO
   * statements put it in expr2. */       
       
  e1 = (c->expr == NULL) ? c->expr2 : c->expr;     
     
  switch(e1->ts.type) {        
  case BT_INTEGER:    tmp1 = integer_select(c, e1);    break;         
  case BT_LOGICAL:    tmp1 = logical_select(c, e1);    break;      
  case BT_CHARACTER:  tmp1 = character_select(c, e1);  break;       
  default:    
    g95_internal_error("g95_trans_select(): Bad type"); 
    tmp1 = NULL_TREE;        
  }        
        
  return tmp1;     
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
   
   


/* trans_deallocate()-- Translate a DEALLOCATE statement */          
          
static tree trans_deallocate(g95_code *code) {          
symbol_attribute atr;      
tree tmp, stat_var;
stmtblock_t block;         
g95_alloc *alloc;          
g95_typespec t;       
g95_expr *exp;        
g95_se s;        
        
  g95_init_se(&s, NULL);        
  g95_set_error_locus(&s.pre, &code->ext.alloc_list->expr->where);         
         
  if (code->expr == NULL)        
    stat_var = null_pointer_node;     
  else {     
    s.reflevel = 1;   
    g95_conv_expr(&s, code->expr);  
    stat_var = save_expr(s.expr);     
     
    tmp = build1(INDIRECT_REF, g95_default_integer, stat_var);      
    g95_add_modify_expr(&s.pre, tmp, integer_zero_node);   
  }

  for(alloc=code->ext.alloc_list; alloc; alloc=alloc->next) {          
    exp = alloc->expr;   
    atr = g95_variable_attr(exp, &t);          
          
    if (exp->rank > 0)       
      deallocate_array(&s, exp, stat_var);        
    else {       
      deallocate_pointer(&s, exp, stat_var);    
    }
  }       
       
  g95_init_block(&block);        
        
  g95_add_block_to_block(&block, &s.pre);
  g95_add_block_to_block(&block, &s.post);     
     
  return g95_finish_block(&block);         
}     
     
     
      
      
/* g95_trans_code()-- Translate a list of executable statements. */          
          
tree g95_trans_code(g95_code *codep) {  
stmtblock_t block;      
tree decl;  
  
  if (codep == NULL) return NULL_TREE;        
        
  g95_init_block(&block);          
          
  for(; codep; codep=codep->next) {    
    g95_set_backend_locus(&codep->where); 
    if (codep->here != 0) {       
      decl = build_v(LABEL_EXPR, g95_get_label_decl(codep->here));          
      wrap_all_with_wfl(&decl, input_filename, lineno);      
      g95_add_expr_to_block(&block, decl);         
    }          
          
    switch (codep->type) {   
    case EXEC_NOP:        
      decl = NULL_TREE;  
      break;       
       
    case EXEC_ASSIGN:
      decl = g95_trans_assignment(codep->expr, codep->expr2);       
      break; 
 
    case EXEC_POINTER_ASSIGN:   
      decl = g95_trans_pointer_assign(codep);     
      break;    
    
    case EXEC_CONTINUE:        
    case EXEC_ENTRY:      
      decl = NULL_TREE;    
      break; 
 
    case EXEC_CYCLE:          
      decl = trans_cycle(codep);         
      break;     
     
    case EXEC_EXIT:     
      decl = trans_exit(codep); 
      break;      
      
    case EXEC_GOTO:
      decl = trans_goto(codep);      
      break;    
    
    case EXEC_STOP: 
      decl = trans_stop(codep);      
      break;        
        
    case EXEC_PAUSE:    
      decl = trans_pause(codep);         
      break;

    case EXEC_CALL:         
      decl = trans_call(codep);
      break;   
   
    case EXEC_RETURN:       
      decl = trans_return(codep);     
      break;    
    
    case EXEC_IF:        
      decl = trans_if(codep);   
      break;    
    
    case EXEC_ARITHMETIC_IF:    
      decl = trans_arithmetic_if(codep); 
      break;        
        
    case EXEC_DO:     
      decl = trans_do(codep);  
      break;      
      
    case EXEC_DO_WHILE:     
      decl = trans_do_while(codep);
      break;

    case EXEC_SELECT:
      decl = trans_select(codep);   
      break;      
      
    case EXEC_FORALL:          
      g95_internal_error("g95_trans_code(): Unexpected FORALL");     
      break;   
   
    case EXEC_WHERE:  
      g95_internal_error("g95_trans_code(): Unexpected WHERE");         
      break;

    case EXEC_ALLOCATE:      
      decl = trans_allocate(codep);        
      break;     
     
    case EXEC_DEALLOCATE:  
      decl = trans_deallocate(codep);       
      break;      
      
    case EXEC_OPEN:    
      decl = g95_trans_open(codep);        
      break;  
  
    case EXEC_CLOSE:         
      decl = g95_trans_close(codep);         
      break; 
 
    case EXEC_READ: 
      decl = g95_trans_read(codep);    
      break;          
          
    case EXEC_WRITE:      
      decl = g95_trans_write(codep);       
      break;        
        
    case EXEC_IOLENGTH:  
      decl = g95_trans_iolength(codep);        
      break; 
 
    case EXEC_BACKSPACE:  
      decl = g95_trans_backspace(codep);          
      break; 
 
    case EXEC_ENDFILE:    
      decl = g95_trans_endfile(codep);        
      break;     
     
    case EXEC_INQUIRE:       
      decl = g95_trans_inquire(codep);       
      break;        
        
    case EXEC_REWIND:          
      decl = g95_trans_rewind(codep);      
      break;        
        
    case EXEC_TRANSFER:        
      decl = g95_trans_transfer(codep);       
      break;         
         
    case EXEC_DT_END:
      decl = g95_trans_dt_end(codep);   
      break;       
       
    default:  
      g95_internal_error("g95_trans_code(): Bad statement code"); 
    }       
       
    if (decl != NULL && decl != empty_stmt_node) {      
      wrap_all_with_wfl(&decl, input_filename, lineno); 
      /* Add the new statemment to the block.  */  
      g95_add_expr_to_block(&block, decl);      
    }    
  } 
 
  /* Return the finished block. */          
  return g95_finish_block(&block);
}
