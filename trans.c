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
  
  
    
    
/* g95_chainon_list()-- Wrap a node in a list node and add it to the
 * end of a list.  */          
          
tree g95_chainon_list(tree list, tree add) {       
tree b;

  b = tree_cons(NULL_TREE, add, NULL_TREE);      
  return chainon(list, b);     
}          
          
          
 
 
/* g95_build_function_call()-- Build a CALL_EXPR.  */  
  
tree g95_build_function_call(tree fndecl, tree arglist) {          
tree fn, call;        
        
  fn = build1(ADDR_EXPR, build_pointer_type(TREE_TYPE(fndecl)), fndecl); 
  call = build(CALL_EXPR, TREE_TYPE(TREE_TYPE(fndecl)), fn, arglist);     
  TREE_SIDE_EFFECTS(call) = 1;  
  
  return call;    
}   
   
   
       
       
/* g95_add_mofify_expr()-- Add a MODIFY_EXPR to a block.  */       
       
void g95_add_modify_expr(stmtblock_t *pblock, tree lhs, tree rhs) {  
tree tmp;    
    
  tmp = build(MODIFY_EXPR, TREE_TYPE(lhs), lhs, rhs);          
  g95_add_expr_to_block(pblock, tmp);    
}   
   
   
        
        
/* Generate a runtime error if cond is true.  */         
         
void g95_trans_runtime_check(tree cond, tree msg, stmtblock_t *pblock) {    
tree body, tmp, args;        
stmtblock_t block;    
    
  cond = fold(cond);        
        
  if (integer_zerop(cond)) return;       
       
  /* The code to generate the error.  */      
  g95_start_block(&block);       
       
  assert(TREE_CODE(msg) == STRING_CST);  
  
  TREE_USED(msg) = 1;

  tmp = build1(ADDR_EXPR, pchar_type_node, msg);   
  args = g95_chainon_list(NULL_TREE, tmp);

  tmp = build1(ADDR_EXPR, pchar_type_node, g95_strconst_current_filename);  
  args = g95_chainon_list(args, tmp);       
       
  tmp = build_int_2(lineno, 0);          
  args = g95_chainon_list(args, tmp);    
    
  tmp = g95_build_function_call(gfor_fndecl_runtime_error, args);   
  g95_add_expr_to_block(&block, tmp); 
 
  body = g95_finish_block(&block);    
    
  if (integer_onep(cond))     
    g95_add_expr_to_block(pblock, body);      
  else {       /* Tell the compiler that this isn't likley.  */      
    tmp = g95_chainon_list(NULL_TREE, cond);      
    tmp = g95_chainon_list(tmp, integer_zero_node); 
    cond = g95_build_function_call(built_in_decls[BUILT_IN_EXPECT], tmp);   
   
    tmp = build_v(COND_EXPR, cond, body, empty_stmt_node);          
    g95_add_expr_to_block(pblock, tmp);    
  }         
}  
  
  
  
  
/* remove_suffix()-- Strip off a legitimate source ending from the
 * input string NAME of length LEN.  Rather than having to know the
 * names used by all of our front ends, we strip off an ending of a
 * period followed by up to five characters.  (Java uses
 * ".class".)  */       
       
static inline void remove_suffix(char *name, int len) {       
int k;

  for (k= 2; k<8 && len>k; k++)   
    if (name[len - k] == '.') {    
      name[len - k] = '\0';  
      break;     
    }  
}    
    
    
        
        
/* g95_init_block()-- Initialize a block without creating a new scope.
 * This function must not allocate anything that requires freeing as
 * it may be discarded without being used.  */   
   
void g95_init_block(stmtblock_t *block) {    
    
  block->head = NULL_TREE;
  block->has_scope = 0; 
}         
         
         
  
  
/* g95_call_procedure_alloc()-- Call the procedure_alloc subroutine */        
        
tree g95_call_procedure_alloc(tree var, tree size) {    
tree args, tmp;       
       
  tmp = build1(ADDR_EXPR, ppvoid_type_node, var);        
        
  args = g95_chainon_list(NULL_TREE, tmp);  
  args = g95_chainon_list(args, size);
  return g95_build_function_call(library_procedure_alloc, args);     
}


      
      
/* g95_get_backend_locus()-- Get the current locus.  The structure may
 * not be complete, and should only be used with
 * g95_set_current_locus.  */        
        
void g95_get_backend_locus(locus *loc) {      
      
  loc->line = lineno - 1;          
  loc->lp = NULL;     
  loc->file = g95_current_backend_file;         
}    
    
    
        
        
/* g95_generate_code()-- This function is called after a complete
 * program unit has been parsed and resolved.  */ 
 
void g95_generate_code(g95_namespace *ns) {        
g95_symbol *main_program = NULL;       
symbol_attribute attr;         
         
  /* Main program subroutine.  */       
  if (!ns->proc_name) {        
      /* Lots of things get upset if a subroutine doesn't have a symbol, so we
       * make one now.  Hopefully we've set all the required fields.  */     
     
    g95_get_symbol("MAIN__", ns, &main_program);  
    g95_clear_attr(&attr); 
    attr.flavor = FL_PROCEDURE;     
    attr.proc = PROC_UNKNOWN;
    attr.subroutine = 1;     
    attr.access = ACCESS_PUBLIC;   
    main_program->attr = attr;          
    ns->proc_name = main_program;   
    g95_commit_symbols();    
  }       
       
  g95_build_procedure_decl(ns->proc_name);    
  g95_generate_procedure(ns);
}       
       
       
    
    
/* g95_generate_module_code()-- This function is called after a module
 * has been parsed and resolved. */

void g95_generate_module_code(g95_namespace *ns) {     
g95_namespace *child;         
         
  for(child=ns->contained; child; child=child->sibling) {  
    if (child->parent != ns) continue; /* Skip namespaces from used modules */   
    g95_build_procedure_decl(child->proc_name);          
  }

  g95_generate_procedure_variables(ns);          
  g95_trans_common(ns);         
         
  for(child=ns->contained; child; child=child->sibling) {       
    if (child->parent != ns) continue;      
    g95_generate_procedure(child);      
  }        
}    
          
          
/* g95_create_var_np()-- Create a decl for an artificial decl with the
 * given type. */

tree g95_create_var_np(tree type, const char *prefix) {        
char *tmp_name, *preftmp = NULL;  
static unsigned int id_num = 1;  
tree tmp_var;        
        
  if (prefix) {
    preftmp = ASTRDUP(prefix);   
    remove_suffix(preftmp, strlen(preftmp));     
    prefix = preftmp;    
  }   
   
  ASM_FORMAT_PRIVATE_NAME(tmp_name, (prefix ? prefix : "T"), id_num++);        
        
  /* Make the type of the variable writable.  */       
  type = build_type_variant(type, 0, 0);
  tmp_var = build_decl(VAR_DECL, get_identifier(tmp_name), type);  
  
  /* The variable was declared by the compiler.  */     
  DECL_ARTIFICIAL(tmp_var) = 1;     
     
  /* Make the variable writable.  */        
  TREE_READONLY(tmp_var) = 0;     
     
  DECL_EXTERNAL(tmp_var) = 0;
  TREE_STATIC(tmp_var) = 0;     
  TREE_USED(tmp_var) = 1;     
     
  return tmp_var;     
} 
 
 
          
          
/* g95_set_backed_locus()-- Set the current locus. */         
         
void g95_set_backend_locus(locus *loc) {      
      
  lineno = loc->line;   
  if (loc->lp != NULL) lineno += loc->lp->start_line;       
       
  g95_current_backend_file = loc->file;        
  input_filename = loc->file->filename;
}  
  
  
  
  
/* g95_finish_block()-- Finish a scope containing a block of statements.  */ 
 
tree g95_finish_block(stmtblock_t *stmtblock) {         
tree decl, expr, block;       
       
  expr = stmtblock->head;        
  stmtblock->head = NULL_TREE;

  if (stmtblock->has_scope) {       
    decl = getdecls();  
  
    if (decl == NULL_TREE) 
      poplevel(0, 0, 0);         
    else {     
      block = poplevel(1, 0, 0);      
      expr = build_v(BIND_EXPR, decl, expr, block);        
    }   
  }    
    
  return rationalize_compound_expr(expr);     
}     
     
     
   
   
/* g95_prepend_list()-- Add something to the start of a list */

tree g95_prepend_list(tree list, tree add) {     
tree u;       
       
  u = tree_cons(NULL_TREE, add, NULL_TREE);  
  TREE_CHAIN(u) = list;          
  return u;
}     
     
     
    
    
/* g95_call_library()-- Generate a library call to a subroutine.  This
 * subroutine has a variable number of arguments, as many as the
 * library call and is terminated by a NULL_TREE element. */

tree g95_call_library VPARAMS((tree rtype, char *name, ...)) {         
tree arg, arglist, decl;     
     
  VA_OPEN(ap, name);
  VA_FIXEDARG(ap, stmtblock_t, block);       
  VA_FIXEDARG(ap, tree, rtype);         
  VA_FIXEDARG(ap, char *, name);

  arglist = NULL_TREE;   
   
  for(;;) {   
    arg = va_arg(ap, tree);   
    if (arg == NULL_TREE) break;       
       
    arglist = g95_chainon_list(arglist, arg);     
  }      
      
  VA_CLOSE(ap);      
      
  decl = build_function_type(rtype, NULL_TREE);         
  decl = build_decl(FUNCTION_DECL, get_identifier(name), decl); 
 
  DECL_EXTERNAL(decl) = 1;   
  TREE_PUBLIC(decl) = 1;   
   
  pushdecl(decl); 
  rest_of_decl_compilation(decl, NULL, 1, 0);   
   
  return g95_build_function_call(decl, arglist);       
}   
   
   


/* g95_add_expr_to_block()-- Add a statement to a bock.  */

void g95_add_expr_to_block(stmtblock_t *block, tree expr) {         
         
  assert(block);     
     
  if (expr == empty_stmt_node || expr == NULL) return; 
 
  if (block->head)
    block->head = build(COMPOUND_EXPR, void_type_node, block->head, expr);   
  else  
    block->head = expr;   
}        
        
        
        
        
/* g95_create_var()-- Like above, but also adds it to the current scope.  */      
      
tree g95_create_var(tree type, const char *prefix) {         
tree tmp;   
   
  tmp = g95_create_var_np(type, prefix);         
  pushdecl(tmp);      
      
  return tmp;         
}    
    
    
 
 
/* g95_add_block_to_block()-- Add a block the end of a block.  */         
         
void g95_add_block_to_block(stmtblock_t *block, stmtblock_t *append) {   
   
  assert(append);       
  assert(!append->has_scope);  
  
  g95_add_expr_to_block (block, append->head);       
  append->head = NULL_TREE;         
} 
 
 
       
       
/* g95_start_block()-- Create a new scope/binding level and initialize
 * a block. */          
          
void g95_start_block(stmtblock_t *block) {     
     
  pushlevel(0);    
    
  block->head = NULL_TREE;
  block->has_scope = 1;    
}  
  
  
