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
          
static g95_file *g95_current_backend_file;        
        
        
        
        
/* g95_get_backend_locus()-- Get the current locus.  The structure may
 * not be complete, and should only be used with
 * g95_set_current_locus.  */       
       
void g95_get_backend_locus(locus *loc) {   
   
  loc->line = lineno - 1;     
  loc->lp = NULL;       
  loc->file = g95_current_backend_file;     
}          
          
          
          
          
/* g95_unique_identifier()-- Get a unique identifier within a single
 * object file. */

tree g95_unique_identifier(char *base) {       
static int serial;         
char nm[100];   
   
  sprintf(nm, "%s.%d", base, serial++);          
  return get_identifier(nm);         
}   
   
   
         
         
/* g95_build_function_call()-- Build a CALL_EXPR.  */      
      
tree g95_build_function_call(tree d, tree arglist) {       
tree tmp, call;       
       
  tmp = build1(ADDR_EXPR, build_pointer_type(TREE_TYPE(d)), d); 
  call = build(CALL_EXPR, TREE_TYPE(TREE_TYPE(d)), tmp, arglist);        
  TREE_SIDE_EFFECTS(call) = 1;  
  
  return call;  
}      
      
      
  
  
/* g95_add_expr_to_block()-- Add a statement to a bock.  */     
     
void g95_add_expr_to_block(stmtblock_t *block, tree exp) {          
          
  assert(block);

  if (exp == empty_stmt_node || exp == NULL) return;     
     
  if (block->head)    
    block->head = build(COMPOUND_EXPR, void_type_node, block->head, exp);       
  else       
    block->head = exp;
}      
      
      
     
     
/* g95_start_block()-- Create a new scope/binding level and initialize
 * a block. */     
     
void g95_start_block(stmtblock_t *list) { 
 
  pushlevel(0);    
    
  list->head = NULL_TREE;  
  list->has_scope = 1;    
}   
   
   
   
   
/* generate_block_data()-- Generate the initialized common blocks in a
 * BLOCK DATA program unit.  We create a public variable with the name
 * of the block data to ensure that the final program can only contain
 * one block data of that name (or the blank block data). */   
   
void g95_generate_block_data(g95_namespace *ns) {       
char *n;         
tree declr; 
 
  g95_trans_common(ns); 
 
  n = (ns->proc_name == NULL) ? BLANK_BLOCK_DATA_NAME : ns->proc_name->name;  
  
  declr = build_decl(VAR_DECL, get_identifier(n), g95_default_integer);
  TREE_PUBLIC(declr) = 1;       
  TREE_STATIC(declr) = 1;   
   
  pushdecl(declr);        
  rest_of_decl_compilation(declr, NULL, 1, 0);   
}     
      
      
/* g95_call_procedure_alloc()-- Call the procedure_alloc subroutine */          
          
tree g95_call_procedure_alloc(tree var, tree sz) {    
tree argu, tmp0; 
 
  tmp0 = build1(ADDR_EXPR, ppvoid_type_node, var);      
      
  argu = g95_chainon_list(NULL_TREE, tmp0);        
  argu = g95_chainon_list(argu, sz);    
  return g95_build_function_call(library_procedure_alloc, argu);          
}        
        
        
          
          
/* g95_create_var_np()-- Create a decl for an artificial decl with the
 * given type. */      
      
tree g95_create_var_np(tree dtype) {       
tree variable;   
   
  dtype = build_type_variant(dtype, 0, 0);     
  variable = build_decl(VAR_DECL, g95_unique_identifier("T"), dtype);       
       
  /* The variable was declared by the compiler.  */      
  DECL_ARTIFICIAL(variable) = 1;      
      
  /* Make the variable writable.  */        
  TREE_READONLY(variable) = 0;          
          
  DECL_EXTERNAL(variable) = 0;
  TREE_STATIC(variable) = 0; 
  TREE_USED(variable) = 1;    
  TREE_ADDRESSABLE(variable) = 1;      
      
  return variable;     
}       
       
       
 
 
/* g95_save_expr()-- Build a temporary to save an expression for later
 * use.  This subroutine is used when gcc's save_expr() won't suffice.
 * For example, if the saved value is used on opposite sides of a
 * conditional. */         
         
void g95_save_expr(g95_se *s) {         
tree var0;       
       
  if (!CONSTANT_P(s->expr) && TREE_CODE(s->expr) != VAR_DECL) {     
    var0 = g95_create_var(TREE_TYPE(s->expr)); 
    g95_add_modify_expr(&s->pre, var0, s->expr);         
    s->expr = var0;      
  }     
}     
     
     
       
       
/* g95_generate_module()-- This function is called after a module
 * has been parsed and resolved. */      
      
void g95_generate_module(g95_namespace *name) {       
g95_namespace *child;   
   
  for(child=name->contained; child; child=child->sibling) {         
    if (child->parent != name) continue; /* Skip namespaces from used modules */        
    g95_build_procedure_decl(child->proc_name);
  } 
 
  g95_trans_common(name);       
  g95_generate_procedure_variables(name); 
 
  for(child=name->contained; child; child=child->sibling) {    
    if (child->parent != name) continue;     
    g95_generate_procedure(child);     
  }   
}      
      
      
 
 
/* g95_chainon_list()-- Wrap a node in a list node and add it to the
 * end of a list.  */          
          
tree g95_chainon_list(tree list, tree add) {
tree i;        
        
  i = tree_cons(NULL_TREE, add, NULL_TREE); 
  return chainon(list, i);    
}   
   
   
         
         
/* g95_add_mofify_expr()-- Add a MODIFY_EXPR to a block.  */        
        
void g95_add_modify_expr(stmtblock_t *pblock, tree lhs, tree right) {         
tree tmp;

  tmp = build(MODIFY_EXPR, TREE_TYPE(lhs), lhs, right);        
  g95_add_expr_to_block(pblock, tmp);          
}       
       
       


/* g95_set_backed_locus()-- Set the current locus. */ 
 
void g95_set_backend_locus(locus *loc) {     
     
  lineno = loc->line;     
  if (loc->lp != NULL) lineno += loc->lp->start_line;        
        
  g95_current_backend_file = loc->file; 
  input_filename = loc->file->filename;        
}  
  
  
 
 
/* g95_init_block()-- Initialize a block without creating a new scope.
 * This function must not allocate anything that requires freeing as
 * it may be discarded without being used.  */          
          
void g95_init_block(stmtblock_t *blk) {  
  
  blk->head = NULL_TREE;       
  blk->has_scope = 0;      
}         
         
         
    
    
/* g95_create_var()-- Like above, but also adds it to the current scope.  */       
       
tree g95_create_var(tree t) {       
tree tmp1;      
      
  tmp1 = g95_create_var_np(t);      
  pushdecl(tmp1);     
     
  return tmp1;       
}      
      
      
  
  
/* g95_call_temp_alloc()-- Calls the temp_alloc() library function to
 * get some memory from the heap. */   
   
void g95_call_temp_alloc(stmtblock_t *b, tree v, tree leng) {       
tree t, a;          
          
  a = g95_chainon_list(NULL_TREE, leng);          
  t = g95_build_function_call(library_temp_alloc, a);        
        
  g95_add_modify_expr(b, v, t);    
}    
    
    
  
  
/* g95_add_block_to_block()-- Add a block the end of a block.  */

void g95_add_block_to_block(stmtblock_t *b, stmtblock_t *append) {      
      
  assert(append);
  assert(!append->has_scope);       
       
  g95_add_expr_to_block (b, append->head);       
  append->head = NULL_TREE;  
}     
     
     
     
     
/* g95_call_temp_free()-- Generate a call to the temp_free() library
 * function. */  
  
void g95_call_temp_free(stmtblock_t *list, tree variable) {        
tree tmp, argu;         
         
  variable = build1(ADDR_EXPR, pvoid_type_node, variable);         
  argu = g95_chainon_list(NULL_TREE, variable);
  tmp = g95_build_function_call(library_temp_free, argu);        
        
  g95_add_expr_to_block(list, tmp);          
          
  g95_add_modify_expr(list, variable, null_pointer_node);         
}   
   
   
  
  
/* g95_call_library()-- Generate a library call to a subroutine.  This
 * subroutine has a variable number of arguments, as many as the
 * library call and is terminated by a NULL_TREE element. */        
        
tree g95_call_library VPARAMS((tree rtype, char *name0, ...)) {        
tree argum, alist, declr;

  VA_OPEN(actualp, name0);          
  VA_FIXEDARG(actualp, stmtblock_t, list);        
  VA_FIXEDARG(actualp, tree, rtype);        
  VA_FIXEDARG(actualp, char *, name0); 
 
  alist = NULL_TREE;  
  
  for(;;) {        
    argum = va_arg(actualp, tree);  
    if (argum == NULL_TREE) break;   
   
    alist = g95_chainon_list(alist, argum);     
  }          
          
  VA_CLOSE(actualp);    
    
  declr = build_function_type(rtype, NULL_TREE);         
  declr = build_decl(FUNCTION_DECL, get_identifier(name0), declr);      
      
  DECL_EXTERNAL(declr) = 1;         
  TREE_PUBLIC(declr) = 1;

  pushdecl(declr);
  rest_of_decl_compilation(declr, NULL, 1, 0);       
       
  return g95_build_function_call(declr, alist);    
}  
  
  
 
 
/* g95_prepend_list()-- Add something to the start of a list */   
   
tree g95_prepend_list(tree list, tree add) {     
tree q;

  q = tree_cons(NULL_TREE, add, NULL_TREE);          
  TREE_CHAIN(q) = list;          
  return q;        
}      
      
      
     
     
/* g95_finish_block()-- Finish a scope containing a block of statements.  */          
          
tree g95_finish_block(stmtblock_t *stmtblock) {   
tree declr, e2, list;       
       
  e2 = stmtblock->head;          
  stmtblock->head = NULL_TREE;     
     
  if (stmtblock->has_scope) {      
    declr = getdecls();         
         
    if (declr == NULL_TREE)  
      poplevel(0, 0, 0);          
    else {   
      list = poplevel(1, 0, 0);        
      e2 = build_v(BIND_EXPR, declr, e2, list);     
    }
  }          
          
  return rationalize_compound_expr(e2);     
}   
   
   
       
       
/* g95_generate_code()-- This function is called after a complete
 * program unit has been parsed and resolved. */          
          
void g95_generate_code(g95_namespace *names) {      
g95_symbol *main_program = NULL;         
symbol_attribute attr;         
         
  /* Main program subroutine.  */       
  if (!names->proc_name) {
      /* Lots of things get upset if a subroutine doesn't have a symbol, so we
       * make one now.  Hopefully we've set all the required fields.  */

    g95_get_symbol("MAIN_", names, &main_program);          
    g95_clear_attr(&attr);          
    attr.flavor = FL_PROCEDURE; 
    attr.proc = PROC_UNKNOWN;    
    attr.subroutine = 1;         
    attr.access = ACCESS_PUBLIC;       
    main_program->attr = attr;     
    names->proc_name = main_program;   
    g95_commit_symbols();    
  }        
        
  g95_build_procedure_decl(names->proc_name);
  g95_generate_procedure(names);          
}


