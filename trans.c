/* Code translation
   Copyright (C) 2000, 2001, 2002 Free Software Foundation, Inc.
   Contributed by Paul Brook

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
         
/* trans.c-- generate GCC trees from g95_code */

#include "trans.h"
        
        
/* Naming convention for backend interface code:
 * g95_trans_* translate g95_code into STMT trees.
 * g95_conv_* expression conversion
 * g95_get_* get a backend tree representation of a decl or type
 */         
         
         
  
  
/* g95_call_library()-- Generate a library call to a subroutine.  This
 * subroutine has a variable number of arguments, as many as the
 * library call and is terminated by a NULL_TREE element. */         
         
tree g95_call_library VPARAMS((tree rtype, char *n, ...)) { 
tree arg, arglist, declr;   
   
  VA_OPEN(actualp, n);         
  VA_FIXEDARG(actualp, stmtblock_t, blk);    
  VA_FIXEDARG(actualp, tree, rtype);      
  VA_FIXEDARG(actualp, char *, n);        
        
  arglist = NULL_TREE;

  for(;;) {
    arg = va_arg(actualp, tree); 
    if (arg == NULL_TREE) break;

    arglist = g95_chainon_list(arglist, arg);   
  } 
 
  VA_CLOSE(actualp);  
  
  declr = build_function_type(rtype, NULL_TREE);   
  declr = build_decl(FUNCTION_DECL, get_identifier(n), declr);        
        
  DECL_EXTERNAL(declr) = 1;      
  TREE_PUBLIC(declr) = 1;          
          
  pushdecl(declr);          
  rest_of_decl_compilation(declr, NULL, 1, 0);          
          
  return g95_build_function_call(declr, arglist); 
}   
   
   
  
  
/* g95_start_block()-- Create a new scope/binding level and initialize
 * a block. */    
    
void g95_start_block(stmtblock_t *block) {   
   
  pushlevel(0);          
          
  block->head = NULL_TREE;   
  block->has_scope = 1;       
}      
      
      
   
   
/* generate_code()-- Generate a program unit containing executable
 * statements. */

static void generate_code(g95_namespace *names) {  
  
  g95_build_procedure_decl(names->proc_name);  
  g95_generate_procedure(names);  
}         
         
         
     
     
/* g95_add_block_to_block()-- Add a block the end of a block.  */     
     
void g95_add_block_to_block(stmtblock_t *list, stmtblock_t *append) {         
         
  assert(append);    
  assert(!append->has_scope);     
     
  g95_add_expr_to_block(list, append->head);         
  append->head = NULL_TREE;        
}      
      
      
          
          
/* g95_init_block()-- Initialize a block without creating a new scope.
 * This function must not allocate anything that requires freeing as
 * it may be discarded without being used.  */      
      
void g95_init_block(stmtblock_t *block) {      
      
  block->head = NULL_TREE;   
  block->has_scope = 0;      
}    
    
    
     
     
/* generate_module()-- This function is called after a module
 * has been parsed and resolved. */       
       
static void generate_module(g95_namespace *ns) {  
g95_namespace *child;      
      
  for(child=ns->contained; child; child=child->sibling) { 
    if (child->parent != ns) continue; /* Skip namespaces from used modules */       
    g95_build_procedure_decl(child->proc_name); 
  }          
          
  g95_trans_common(ns);     
  g95_generate_procedure_variables(ns);     
     
  for(child=ns->contained; child; child=child->sibling) {
    if (child->parent != ns) continue;      
    g95_generate_procedure(child);     
  } 
}    
    
    
     
     
/* g95_prepend_list()-- Add something to the start of a list */     
     
tree g95_prepend_list(tree list, tree add) {        
tree v;          
          
  v = tree_cons(NULL_TREE, add, NULL_TREE);       
  TREE_CHAIN(v) = list;         
  return v;    
}        
        
        
   
   
/* g95_call_temp_alloc()-- Calls the temp_alloc() library function to
 * get some memory from the heap. */      
      
void g95_call_temp_alloc(stmtblock_t *b, tree variable, tree l) {     
tree tmp0, a;

  a = g95_chainon_list(NULL_TREE, l); 
  tmp0 = g95_build_function_call(library_temp_alloc, a);    
    
  g95_add_modify_expr(b, variable, tmp0);       
}         
         
         
    
    
/* g95_finish_block()-- Finish a scope containing a block of statements.  */       
       
tree g95_finish_block(stmtblock_t *stmtblock) {     
tree d, e1, block;        
        
  e1 = stmtblock->head;       
  stmtblock->head = NULL_TREE;          
          
  if (stmtblock->has_scope) {     
    d = getdecls();  
  
    if (d == NULL_TREE)      
      poplevel(0, 0, 0);        
    else {     
      block = poplevel(1, 0, 0);          
      e1 = build_v(BIND_EXPR, d, e1, block);        
    } 
  } 
 
  return rationalize_compound_expr(e1);         
}        
        
        
       
       
/* g95_unique_identifier()-- Get a unique identifier within a single
 * object file. */   
   
tree g95_unique_identifier(char *bottom) {       
static int serial;  
char nm[100];    
    
  sprintf(nm, "%s.%d", bottom, serial++);         
  return get_identifier(nm);      
}


          
          
/* g95_add_expr_to_block()-- Add a statement to a bock.  */          
          
void g95_add_expr_to_block(stmtblock_t *list, tree exp) {    
    
  assert(list);         
         
  if (exp == empty_stmt_node || exp == NULL) return;          
          
  if (list->head)        
    list->head = build(COMPOUND_EXPR, void_type_node, list->head, exp);    
  else          
    list->head = exp;   
}


   
   
/* g95_create_var()-- Like above, but also adds it to the current scope.  */     
     
tree g95_create_var(tree typ) {         
tree tmp;    
    
  tmp = g95_create_var_np(typ);   
  pushdecl(tmp);

  return tmp;         
}       
       
       
         
         
/* g95_save_expr()-- Build a temporary to save an expression for later
 * use.  This subroutine is used when gcc's save_expr() won't suffice.
 * For example, if the saved value is used on opposite sides of a
 * conditional. */          
          
void g95_save_expr(g95_se *se0) {    
tree variable;    
    
  if (!CONSTANT_P(se0->expr) && TREE_CODE(se0->expr) != VAR_DECL) {    
    variable = g95_create_var(TREE_TYPE(se0->expr));    
    g95_add_modify_expr(&se0->pre, variable, se0->expr); 
    se0->expr = variable;       
  }         
}   
   
   


/* g95_build_function_call()-- Build a CALL_EXPR.  */

tree g95_build_function_call(tree d, tree arg) {          
tree tmp0, call;       
       
  tmp0 = build1(ADDR_EXPR, build_pointer_type(TREE_TYPE(d)), d);    
  call = build(CALL_EXPR, TREE_TYPE(TREE_TYPE(d)), tmp0, arg); 
  TREE_SIDE_EFFECTS(call) = 1;         
         
  return call;         
}        
        
        
    
    
/* g95_set_backed_locus()-- Set the current locus. */

void g95_set_backend_locus(g95_locus *loc) {         
         
  lineno = loc->lb->linenum;  
  input_filename = loc->lb->file->filename;      
}        
        
        
 
 
/* g95_call_temp_free()-- Generate a call to the temp_free() library
 * function. */     
     
void g95_call_temp_free(stmtblock_t *b, tree variable) {        
tree tmp1, argu;   
   
  variable = build1(ADDR_EXPR, pvoid_type_node, variable);        
  argu = g95_chainon_list(NULL_TREE, variable);      
  tmp1 = g95_build_function_call(library_temp_free, argu);       
       
  g95_add_expr_to_block(b, tmp1);   
   
  g95_add_modify_expr(b, variable, null_pointer_node); 
} 
 
 
      
      
/* g95_create_var_np()-- Create a decl for an artificial decl with the
 * given type. */   
   
tree g95_create_var_np(tree dtype) {   
tree v;      
      
  dtype = build_type_variant(dtype, 0, 0);        
  v = build_decl(VAR_DECL, g95_unique_identifier("T"), dtype);    
    
  /* The variable was declared by the compiler.  */ 
  DECL_ARTIFICIAL(v) = 1;        
        
  /* Make the variable writable.  */
  TREE_READONLY(v) = 0;        
        
  DECL_EXTERNAL(v) = 0;   
  TREE_STATIC(v) = 0;    
  TREE_USED(v) = 1;
  TREE_ADDRESSABLE(v) = 1;        
        
  return v;
}  
  
  
        
        
/* g95_add_mofify_expr()-- Add a MODIFY_EXPR to a block.  */ 
 
void g95_add_modify_expr(stmtblock_t *pblock, tree left_side, tree rhs) {      
tree t;        
        
  t = build(MODIFY_EXPR, TREE_TYPE(left_side), left_side, rhs);     
  g95_add_expr_to_block(pblock, t);      
}  
  
  
    
    
/* g95_chainon_list()-- Wrap a node in a list node and add it to the
 * end of a list.  */   
   
tree g95_chainon_list(tree list, tree add) { 
tree g;     
     
  g = tree_cons(NULL_TREE, add, NULL_TREE);     
  return chainon(list, g);        
}       
       
       
          
          
/* generate_block_data()-- Generate the initialized common blocks in a
 * BLOCK DATA program unit.  We create a public variable with the name
 * of the block data to ensure that the final program can only contain
 * one block data of that name (or the blank block data). */    
    
static void generate_block_data(g95_namespace *n) {          
char *nam;
tree declr;     
     
  g95_trans_common(n);          
          
  nam = (n->proc_name == NULL) ? BLANK_BLOCK_DATA_NAME : n->proc_name->name;   
   
  declr = build_decl(VAR_DECL, get_identifier(nam), g95_default_integer);         
  TREE_PUBLIC(declr) = 1;
  TREE_STATIC(declr) = 1;       
       
  pushdecl(declr);      
  rest_of_decl_compilation(declr, NULL, 1, 0); 
}         
         
         
          
          
/* g95_generate_code()-- Generate code for the parsed and resolved
 * namespaces */        
        
void g95_generate_code(g95_namespace *n) {       
g95_symbol *symbol;     
     
  g95_init_common(n);   
   
  for(; n; n=n->sibling) { 
    g95_current_ns = n;      
    symbol = n->proc_name;     
     
    switch(n->state) {        
    case COMP_MODULE:        
      generate_module(n);
      break; 
 
    case COMP_PROGRAM:
    case COMP_SUBROUTINE:  
    case COMP_FUNCTION:      
      generate_code(n);      
      break;

    case COMP_BLOCK_DATA:      
      generate_block_data(n);          
      break;         
         
    default:          
      g95_internal_error("g95_generate_code(): Bad program unit");   
    }          
  }  
}        
 
 
/* g95_call_procedure_alloc()-- Call the procedure_alloc subroutine */          
          
tree g95_call_procedure_alloc(tree v, tree sz) {  
tree a, t;          
          
  t = build1(ADDR_EXPR, ppvoid_type_node, v);

  a = g95_chainon_list(NULL_TREE, t); 
  a = g95_chainon_list(a, sz);     
  return g95_build_function_call(library_procedure_alloc, a);         
}     
     
     
