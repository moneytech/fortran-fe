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
   
   
        
        
/* remove_suffix()-- Strip off a legitimate source ending from the
 * input string NAME of length LEN.  Rather than having to know the
 * names used by all of our front ends, we strip off an ending of a
 * period followed by up to five characters.  (Java uses
 * ".class".)  */       
       
static inline void remove_suffix(char *n, int length) {   
int i;          
          
  for (i= 2; i<8 && length>i; i++)    
    if (n[length - i] == '.') {        
      n[length - i] = '\0';         
      break;         
    }         
}    
    
    
      
      
/* g95_build_function_call()-- Build a CALL_EXPR.  */   
   
tree g95_build_function_call(tree decl, tree arglist) {        
tree fn, call;    
    
  fn = build1(ADDR_EXPR, build_pointer_type(TREE_TYPE(decl)), decl);          
  call = build(CALL_EXPR, TREE_TYPE(TREE_TYPE(decl)), fn, arglist);   
  TREE_SIDE_EFFECTS(call) = 1;  
  
  return call;     
}      
      
      
    
    
/* g95_create_var_np()-- Create a decl for an artificial decl with the
 * given type. */     
     
tree g95_create_var_np(tree dtype, const char *prefix) {          
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
  dtype = build_type_variant(dtype, 0, 0);     
  tmp_var = build_decl(VAR_DECL, get_identifier(tmp_name), dtype);    
    
  /* The variable was declared by the compiler.  */
  DECL_ARTIFICIAL(tmp_var) = 1;       
       
  /* Make the variable writable.  */
  TREE_READONLY(tmp_var) = 0;  
  
  DECL_EXTERNAL(tmp_var) = 0;        
  TREE_STATIC(tmp_var) = 0; 
  TREE_USED(tmp_var) = 1;     
     
  return tmp_var;   
} 
 
 
       
       
/* g95_create_var()-- Like above, but also adds it to the current scope.  */      
      
tree g95_create_var(tree typ, const char *prefix) { 
tree tmp1;   
   
  tmp1 = g95_create_var_np(typ, prefix);       
  pushdecl(tmp1);   
   
  return tmp1;        
}    
    
    
         
         
/* g95_add_expr_to_block()-- Add a statement to a bock.  */         
         
void g95_add_expr_to_block(stmtblock_t *blk, tree e) {

  assert(blk);        
        
  if (e == empty_stmt_node || e == NULL) return; 
 
  if (blk->head)  
    blk->head = build(COMPOUND_EXPR, void_type_node, blk->head, e);     
  else          
    blk->head = e;   
}   
   
   
  
  
/* g95_get_backend_locus()-- Get the current locus.  The structure may
 * not be complete, and should only be used with
 * g95_set_current_locus.  */       
       
void g95_get_backend_locus(locus *loc) {  
  
  loc->line = lineno - 1;        
  loc->lp = NULL;  
  loc->file = g95_current_backend_file;          
}        
        
        
      
      
/* g95_init_block()-- Initialize a block without creating a new scope.
 * This function must not allocate anything that requires freeing as
 * it may be discarded without being used.  */      
      
void g95_init_block(stmtblock_t *b) {  
  
  b->head = NULL_TREE; 
  b->has_scope = 0;  
}     
     
     
       
       
/* g95_call_library()-- Generate a library call to a subroutine.  This
 * subroutine has a variable number of arguments, as many as the
 * library call and is terminated by a NULL_TREE element. */         
         
tree g95_call_library VPARAMS((tree rtype, char *n, ...)) { 
tree a, arglist, d;         
         
  VA_OPEN(ap, n);
  VA_FIXEDARG(ap, stmtblock_t, blk);      
  VA_FIXEDARG(ap, tree, rtype);
  VA_FIXEDARG(ap, char *, n); 
 
  arglist = NULL_TREE; 
 
  for(;;) {      
    a = va_arg(ap, tree); 
    if (a == NULL_TREE) break;     
     
    arglist = g95_chainon_list(arglist, a);          
  }   
   
  VA_CLOSE(ap);         
         
  d = build_function_type(rtype, NULL_TREE);        
  d = build_decl(FUNCTION_DECL, get_identifier(n), d);   
   
  DECL_EXTERNAL(d) = 1;      
  TREE_PUBLIC(d) = 1;      
      
  pushdecl(d);   
  rest_of_decl_compilation(d, NULL, 1, 0);

  return g95_build_function_call(d, arglist);    
}      
      
      
         
         
/* g95_add_mofify_expr()-- Add a MODIFY_EXPR to a block.  */         
         
void g95_add_modify_expr(stmtblock_t *pblock, tree left_side, tree right) {      
tree tmp0;   
   
  tmp0 = build(MODIFY_EXPR, TREE_TYPE(left_side), left_side, right);      
  g95_add_expr_to_block(pblock, tmp0);         
}


    
    
/* generate_block_data()-- Generate the initialized common blocks in a
 * BLOCK DATA program unit.  We create a public variable with the name
 * of the block data to ensure that the final program can only contain
 * one block data of that name (or the blank block data). */         
         
void g95_generate_block_data(g95_namespace *n) {         
char *name0; 
tree decl;          
          
  g95_trans_common(n);          
          
  name0 = (n->proc_name == NULL) ? BLANK_BLOCK_DATA_NAME : n->proc_name->name;       
       
  decl = build_decl(VAR_DECL, get_identifier(name0), g95_default_integer);       
  TREE_PUBLIC(decl) = 1;   
  TREE_STATIC(decl) = 1;   
   
  pushdecl(decl);        
  rest_of_decl_compilation(decl, NULL, 1, 0);         
}   
    
    
/* g95_prepend_list()-- Add something to the start of a list */        
        
tree g95_prepend_list(tree list, tree add) {
tree i;       
       
  i = tree_cons(NULL_TREE, add, NULL_TREE);
  TREE_CHAIN(i) = list;   
  return i;         
}      
      
      
  
  
/* g95_add_block_to_block()-- Add a block the end of a block.  */

void g95_add_block_to_block(stmtblock_t *b, stmtblock_t *append) {       
       
  assert(append);       
  assert(!append->has_scope);   
   
  g95_add_expr_to_block (b, append->head); 
  append->head = NULL_TREE;  
}          
          
          
          
          
/* g95_call_procedure_alloc()-- Call the procedure_alloc subroutine */  
  
tree g95_call_procedure_alloc(tree var0, tree sz) {        
tree argu, t;          
          
  t = build1(ADDR_EXPR, ppvoid_type_node, var0);

  argu = g95_chainon_list(NULL_TREE, t);    
  argu = g95_chainon_list(argu, sz);
  return g95_build_function_call(library_procedure_alloc, argu);
}        
        
        
      
      
/* g95_start_block()-- Create a new scope/binding level and initialize
 * a block. */   
   
void g95_start_block(stmtblock_t *blk) {         
         
  pushlevel(0);  
  
  blk->head = NULL_TREE; 
  blk->has_scope = 1;  
}          
          
          
          
          
/* g95_generate_code()-- This function is called after a complete
 * program unit has been parsed and resolved. */         
         
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
     
     
 
 
/* g95_set_backed_locus()-- Set the current locus. */   
   
void g95_set_backend_locus(locus *loc) {     
     
  lineno = loc->line;       
  if (loc->lp != NULL) lineno += loc->lp->start_line; 
 
  g95_current_backend_file = loc->file;          
  input_filename = loc->file->filename;       
} 
 
 
         
         
/* Generate a runtime error if cond is true.  */ 
 
void g95_trans_runtime_check(tree cond, tree message, stmtblock_t *pblock) {     
tree list, t, argu;    
stmtblock_t b;      
      
  cond = fold(cond);   
   
  if (integer_zerop(cond)) return; 
 
  /* The code to generate the error.  */         
  g95_start_block(&b);

  assert(TREE_CODE(message) == STRING_CST);          
          
  TREE_USED(message) = 1;        
        
  t = build1(ADDR_EXPR, pchar_type_node, message);    
  argu = g95_chainon_list(NULL_TREE, t);          
          
  t = build1(ADDR_EXPR, pchar_type_node, g95_strconst_current_filename);          
  argu = g95_chainon_list(argu, t); 
 
  t = build_int_2(lineno, 0);     
  argu = g95_chainon_list(argu, t);          
          
  t = g95_build_function_call(gfor_fndecl_runtime_error, argu);    
  g95_add_expr_to_block(&b, t);   
   
  list = g95_finish_block(&b);    
    
  if (integer_onep(cond))
    g95_add_expr_to_block(pblock, list); 
  else {       /* Tell the compiler that this isn't likley.  */ 
    t = g95_chainon_list(NULL_TREE, cond);
    t = g95_chainon_list(t, integer_zero_node);          
    cond = g95_build_function_call(built_in_decls[BUILT_IN_EXPECT], t);         
         
    t = build_v(COND_EXPR, cond, list, empty_stmt_node);    
    g95_add_expr_to_block(pblock, t);         
  }    
}


         
         
/* g95_save_expr()-- Build a temporary to save an expression for later
 * use.  This subroutine is used when gcc's save_expr() won't suffice.
 * For example, if the saved value is used on opposite sides of a
 * conditional. */       
       
void g95_save_expr(g95_se *se) {
tree var;      
      
  if (!CONSTANT_P(se->expr) && TREE_CODE(se->expr) != VAR_DECL) {          
    var = g95_create_var(TREE_TYPE(se->expr), NULL);      
    g95_add_modify_expr(&se->pre, var, se->expr);
    se->expr = var;   
  } 
}         
         
         
       
       
/* g95_finish_block()-- Finish a scope containing a block of statements.  */

tree g95_finish_block(stmtblock_t *stmtblock) {      
tree dec, e, b;     
     
  e = stmtblock->head;        
  stmtblock->head = NULL_TREE;       
       
  if (stmtblock->has_scope) {  
    dec = getdecls();      
      
    if (dec == NULL_TREE)          
      poplevel(0, 0, 0);          
    else {      
      b = poplevel(1, 0, 0);   
      e = build_v(BIND_EXPR, dec, e, b);    
    }
  }  
  
  return rationalize_compound_expr(e);
}  
  
  


/* g95_chainon_list()-- Wrap a node in a list node and add it to the
 * end of a list.  */    
    
tree g95_chainon_list(tree list, tree add) {        
tree i;        
        
  i = tree_cons(NULL_TREE, add, NULL_TREE);      
  return chainon(list, i);
}   
   
   
 
 
/* g95_generate_module_code()-- This function is called after a module
 * has been parsed and resolved. */  
  
void g95_generate_module_code(g95_namespace *names) {    
g95_namespace *child; 
 
  for(child=names->contained; child; child=child->sibling) {       
    if (child->parent != names) continue; /* Skip namespaces from used modules */    
    g95_build_procedure_decl(child->proc_name);       
  }      
      
  g95_trans_common(names);     
  g95_generate_procedure_variables(names);          
          
  for(child=names->contained; child; child=child->sibling) { 
    if (child->parent != names) continue;       
    g95_generate_procedure(child);    
  }        
}     
     
     
