/* Backend function setup
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
    
/* trans-decl.c -- Handling of backend function and variable decls, etc */ 
 
#include "trans.h"
       
       
#define MAX_LABEL_VALUE 99999
  
/* Holds the result of the current function */  
  
static GTY(()) tree current_function_return_label; 
GTY(()) tree g95_result_var_decl;       
       
/* Function declarations for builtin library functions.  */   
   
tree library_temp_alloc;
tree library_temp_free; 
tree library_procedure_alloc;  
  
tree library_select_string; 
 
/* Math functions.  Many other math functions are handled in
 * trans-intrinsic.c.  */ 
 
tree library_integer_4_power;         
tree library_real_4_power;       
tree library_real_8_power;   
   
tree library_complex_4_power;         
tree library_complex_8_power;        
        
tree library_pow_complex_4;    
tree library_pow_complex_8;     
     
tree gfor_fndecl_math_powf;      
tree gfor_fndecl_math_pow;         
         
tree gfor_fndecl_math_cabsf;          
tree gfor_fndecl_math_cabs;      
tree gfor_fndecl_math_ishftc4;
tree gfor_fndecl_math_ishftc8;  
  
g95_trans_context *g95_context = NULL;     
static int symbol_count; 
 
/* None of the specific intrinsics which can be passed as actual arguments
 * for dummy procedures has more then two parameters.  */          
          
#define G95_MAX_SPECIFIC_ARGS 2
    
static void create_procedure_variable(g95_symbol *);    
static void traverse_spec_expr(g95_expr *);


    
/* Generate entry and exit code, and add it to the function body. */  
  
static tree generate_entry_exit(tree fnbody) {  
stmtblock_t block;    
tree tmp1;         
         
  g95_init_block(&block);     
     
  tmp1 = g95_call_library(void_type_node, PREFIX "push_context", NULL_TREE);  
  g95_add_expr_to_block(&block, tmp1);      
      
  g95_add_expr_to_block(&block, fnbody);     
  tmp1 = g95_call_library(void_type_node, PREFIX "pop_context", NULL_TREE);    
  g95_add_expr_to_block(&block, tmp1);

  return g95_finish_block(&block); 
}


 
 
/* g95_sym_identifier()-- Convert a name of a g95_symbol to an
 * identifier name. */       
       
tree g95_sym_identifier(g95_symbol *symbol, char *suffix) {    
char mangled_name[3*G95_MAX_SYMBOL_LEN+4];         
         
  if (symbol->ns == NULL || !g95_module_symbol(symbol) ||  
      strcmp(symbol->module, "(global)") == 0)   
    sprintf(mangled_name, "%s_", symbol->name);     
  else   
    sprintf(mangled_name, "%s.%s_", symbol->module, symbol->name);       
       
  if (suffix != NULL) {         
    strcat(mangled_name, ".");      
    strcat(mangled_name, suffix);    
  }     
     
  return get_identifier(mangled_name); 
}     
     
     
         
         
/* g95_library_decl()-- Builds a function decl.  The remaining
 * parameters are the types of the function arguments.  Negative nargs
 * indicates a varargs function.  */     
     
tree g95_library_decl VPARAMS((char *nam, tree rettype, int nargs, ...)) {         
tree a, argtype, fntype, decl;        
int j;        
        
  /* Library functions must be declared with global scope.  */         
  assert(current_function_decl == NULL_TREE);   
   
  VA_OPEN(s, nargs);        
  VA_FIXEDARG(s, tree, get_identifier(nam));         
  VA_FIXEDARG(s, tree, retval);          
  VA_FIXEDARG(s, int, nargs);   
   
  /* Create a list of the argument types */   
  for(a=NULL_TREE, j=abs(nargs); j>0; j--) {    
    argtype = va_arg(s, tree);  
    a = g95_chainon_list(a, argtype); 
  }  
  
  /* Terminate the list */ 
 
  if (nargs >= 0) a = g95_chainon_list(a, void_type_node);   
   
  /* Build the function type and decl */         
         
  fntype = build_function_type(rettype, a); 
  decl = build_decl(FUNCTION_DECL, get_identifier(nam), fntype);      
      
  /* Mark this decl as external */  
  
  DECL_EXTERNAL(decl) = 1;          
  TREE_PUBLIC(decl) = 1;      
  VA_CLOSE(s); 
 
  pushdecl(decl);       
       
  rest_of_decl_compilation(decl, NULL, 1, 0); 
  return decl;     
}          
          
          
          
          
/* g95_build_label_decl()-- Build a backend label declaration.  Set
 * TREE_USED for named lables.  For artificial labels it's up to the
 * caller to mark the label as used.  */       
       
tree g95_build_label_decl(tree label_id) {     
char *nam;     
tree decl;      
      
  if (label_id != NULL_TREE)         
    nam = NULL;        
  else     /* Build an internal label name. */     
    label_id = g95_unique_identifier("label"); 
 
  /* Build the LABEL_DECL node. Labels have no type. */    
  decl = build_decl(LABEL_DECL, label_id, void_type_node);     
     
  DECL_CONTEXT(decl) = current_function_decl; 
  DECL_MODE(decl) = VOIDmode;

  DECL_ARTIFICIAL(decl) = 1;        
  TREE_USED(decl) = 1;    
      
  /* We always define the label as used, even if the original source
   * file never references the label.  We don't want all kinds of
   * spurious warnings for old-style Fortran code with too many
   * labels. */    
    
  return decl;      
}     
     
     
    
    
/* g95_get_label_decl()-- Return the backend label declaration for a
 * given label structure, or create it if it doesn't exist yet.  */  
  
tree g95_get_label_decl(g95_st_label *lp) {       
char name[20];    
tree d;      
      
  if (lp->backend_decl) return lp->backend_decl;  
  
  sprintf(name, "__label_%d", lp->value);         
  d = g95_build_label_decl(get_identifier(name));        
        
  if (lp->value <= MAX_LABEL_VALUE) {          
    DECL_SOURCE_LINE(d) = lp->where.lb->linenum;  
    DECL_SOURCE_FILE(d) = lp->where.lb->file->filename;  
  } else       
    DECL_ARTIFICIAL(d) = 1; 
 
  lp->backend_decl = d;       
  return d;          
}    
    
    


/* specification_variable()-- Given a variable that is part of a
 * specification expression, create it if it needs to be created. */          
          
static void specification_variable(g95_expr *y) {   
g95_array_ref *ar;   
g95_symbol *sy;  
g95_ref *reference;          
int b;          
          
  sy = y->symbol;     
  if (sy->ns != g95_current_ns) return;     
     
  if (sy->backend_decl == NULL || (sy->attr.dummy && !sy->mark)) {    
    symbol_count--;    
    if (symbol_count == 0)          
      g95_internal_error("traverse_spec_expr(): Circular specification");    
    
    create_procedure_variable(sy);
    symbol_count++;       
  }  
  
  /* Traverse variable references */       
       
  for(reference=y->ref; reference; reference=reference->next)        
    switch(reference->type) {          
    case REF_COMPONENT:
      break;        
        
    case REF_SUBSTRING: 
      traverse_spec_expr(reference->u.ss.start);   
      traverse_spec_expr(reference->u.ss.end);         
      break;         
         
    case REF_ARRAY:  
      ar = &reference->u.ar;     
     
      for(b=0; b<ar->dimen; b++) {          
	traverse_spec_expr(ar->start[b]);      
	traverse_spec_expr(ar->end[b]);   
	traverse_spec_expr(ar->stride[b]);          
      }	       
    }       
}


   
   
/* get_parm_decl()-- Get a declaration associated with a dummy variable */ 
 
static void get_parm_decl(g95_symbol *sy) {   
tree parm, type;      
      
  type = g95_dummy_arg_type(sy);      
      
  parm = build_decl(PARM_DECL, g95_sym_identifier(sy, NULL), type);          
          
  DECL_ARG_TYPE(parm) = type;        
  DECL_ARG_TYPE_AS_WRITTEN(parm) = type;     
  TREE_READONLY(parm) = 1;    
    
  sy->backend_decl = parm;  
}      
      
      
   
   
/* g95_initial_value()-- Return the initial value for a variable. */         
         
tree g95_initial_value(g95_symbol *sym) {        
variable_info info;   
g95_se se0;
tree leng;   
   
  if (sym->attr.use_assoc) return NULL_TREE; 
 
  if (sym->attr.data) return g95_generate_data(sym);        
        
  if (sym->value == NULL)      
    return (sym->ts.type == BT_DERIVED)   
      ? (tree) TYPE_LANG_SPECIFIC(TREE_TYPE(sym->backend_decl))  
      : NULL_TREE; 
 
  g95_init_se(&se0, NULL);  
  
  if (sym->as == NULL) {          
    g95_conv_constant(&se0, sym->value);

    if (STRING_P(se0.expr)) {          
      leng = sym->ts.cl->backend_decl;         
      se0.expr = g95_resize_string_constant(se0.expr, leng);         
    }        
  } else {  
    g95_symbol_vinfo(sym, &info);         
    se0.expr = g95_conv_array_initializer(&info);       
  } 
 
  return se0.expr;   
}         
         
         
         
         
/* g95_finish_decl()-- Finish processing of a declaration and install
 * its initial value. */        
        
static void g95_finish_decl(tree dec, tree init) {   
   
  if (TREE_CODE(dec) == PARM_DECL)   
    assert(init == NULL_TREE);          
          
  /* Remember that PARM_DECL doesn't have a DECL_INITIAL field per se
     -- it overlaps DECL_ARG_TYPE.  */       
       
  else if (init == NULL_TREE)    
    assert(DECL_INITIAL (dec) == NULL_TREE);        
  else    
    assert(DECL_INITIAL (dec) == error_mark_node);

  if (init != NULL_TREE) {   
    if (TREE_CODE(dec) != TYPE_DECL)    
      DECL_INITIAL(dec) = init;    
    else {  
      /* typedef foo = bar; store the type of bar as the type of foo.  */    
      TREE_TYPE(dec) = TREE_TYPE(init);
      DECL_INITIAL(dec) = init = 0;        
    }   
  }      
      
  if (TREE_CODE(dec) == VAR_DECL) {
    if (DECL_SIZE (dec) == NULL_TREE     
	&& TYPE_SIZE(TREE_TYPE(dec)) != NULL_TREE)    
      layout_decl(dec, 0);        
        
    if (DECL_SIZE(dec) == NULL_TREE && (TREE_STATIC(dec) ? 
          /* A static variable with an incomplete type is an error if it is
             initialized. Also if it is not file scope. Otherwise, let it
             through, but if it is not `extern' then it may cause an error
             message later.  */          
					 (DECL_INITIAL (dec) != 0         
					  || DECL_CONTEXT (dec) != 0) :
          /* An automatic variable with an incomplete type is an error.  */      
					 !DECL_EXTERNAL (dec))) {  
      g95_fatal_error("storage size not known");     
    }     
     
    if ((DECL_EXTERNAL(dec) || TREE_STATIC(dec))         
	&& (DECL_SIZE(dec) != 0)      
	&& (TREE_CODE(DECL_SIZE(dec)) != INTEGER_CST)) {          
      g95_fatal_error("storage size not constant");       
    }        
  }   
}          
          
          


/* finish_var_decl()-- Apply symbol attributes to a variable, and add
 * it to the function scope.  */    
    
static void finish_var_decl(tree declr, g95_symbol *s) {     
     
  /* TREE_ADDRESSABLE means the address of this variable is needed for
   * a TARGET variable.  We also need to set this if the variable is
   * passed by reference in a CALL statement. */     
     
  if (s->attr.target) TREE_ADDRESSABLE(declr) = 1; 
 
  /* If it wasn't used we wouldn't be getting it.  */    
  TREE_USED(declr) = 1;    
    
  /* If a variable is USE associated, it's always external.  */   
   
  if (s->attr.use_assoc) {      
    DECL_EXTERNAL(declr) = 1;          
    TREE_PUBLIC(declr) = 1;
  } else if (g95_module_symbol(s)) {    
    
    /* This is the declaration of a module variable.  */
    TREE_PUBLIC(declr) = 1;        
    TREE_STATIC(declr) = 1;        
  } 
 
  if ((s->attr.save || s->attr.data || s->value)      
      && !s->attr.use_assoc) TREE_STATIC(declr) = 1;     
     
  DECL_CONTEXT(declr) = s->ns->backend_decl;   
}         
         
         
      
      
/* pointer_array_return()-- Generate code for a pointer array return.
 * Up until now, the result variable has been an array descriptor, but
 * we now have to return a pointer to the descriptor.  The descriptor
 * is on the stack, so we copy the variable to a static temporary and
 * return a pointer to the temporary. */         
         
static tree pointer_array_return(stmtblock_t *block) {  
tree type, var0;          
          
  var0 = g95_create_var(TREE_TYPE(g95_result_var_decl));
  TREE_STATIC(var0) = 1; 
  TREE_ADDRESSABLE(var0) = 1;  
  
  g95_add_modify_expr(block, var0, g95_result_var_decl);        
  type = build_pointer_type(TREE_TYPE(var0));    
  return build1(ADDR_EXPR, type, var0);  
} 
 
 
         
         
/* get_variable_decl()-- Create the declaration for a variable */  
  
static void get_variable_decl(g95_symbol *sy) {     
tree declr, tmp1, initial;
variable_info info;         
         
  /* Catch function declarations.  Only used for actual parameters.  */        
        
  if (sy->attr.flavor == FL_PROCEDURE) {   
    /* If a procedure is not known to be a function or subroutine,
     * assume that it is a subroutine for the purposes of getting a
     * declaration. */       
       
    if (!sy->attr.function && !sy->attr.subroutine)     
      sy->attr.subroutine = 1; 
 
    sy->backend_decl = g95_get_extern_function_decl(sy);    
    return;   
  }  
  
  g95_symbol_vinfo(sy, &info);       
  tmp1 = g95_get_descriptor(&info);      
      
  declr = build_decl(VAR_DECL, g95_sym_identifier(sy, NULL), tmp1);          
          
  pushdecl(declr);        
  finish_var_decl(declr, sy); 
 
  sy->backend_decl = declr;     
     
  if (!G95_DESCRIPTOR_P(tmp1)) 
    DECL_INITIAL(sy->backend_decl) = g95_initial_value(sy);     
  else if (!sy->attr.use_assoc) {        
    info.desc = declr;       
    tmp1 = g95_get_storage(&info);         
    if (tmp1 != NULL) {     
      initial = g95_initial_value(sy);

      if (g95_context != NULL && initial == NULL && !sy->attr.save &&  
	  !g95_stack_variable(tmp1)) /* Var on heap */     
	tmp1 = NULL_TREE;     
      else { /* Variable not on heap */         
	tmp1 = build_decl(VAR_DECL, g95_sym_identifier(sy, "data"), tmp1); 
 
	DECL_INITIAL(tmp1) = initial;    
	pushdecl(tmp1);       
	finish_var_decl(tmp1, sy);        
      }   
    }   
   
    g95_init_descriptor(&info, declr, tmp1);        
  }       
}     
     
     
         
         
/* generate_entries()-- Generate entry variables within the master
 * function.  The result variable is the second argument to the
 * current function. */         
         
static void generate_entries(g95_namespace *ns) {   
g95_symbol *s;   
tree d, dtype; 
 
  s = g95_find_entries(ns->sym_root, 1);    
  if (s == NULL) return;      
      
  d = DECL_ARGUMENTS(ns->proc_name->backend_decl);      
  d = TREE_CHAIN(d);        
        
  for(; s; s=s->tlink) {   
    if (s->attr.subroutine) 
      s->backend_decl = g95_get_extern_function_decl(s);       
    else {   
      if (s->result != s)
	s->backend_decl = g95_get_extern_function_decl(s);   
   
      dtype = g95_typenode_for_spec(&s->result->ts);        
      dtype = build_pointer_type(dtype);  
  
      s->result->backend_decl = convert(dtype, d);        
    }      
  }    
}




/* find_dependent_var()-- Given a symbol that we are about to create,
 * traverse any array specification or character length, and find
 * other variables that need to be created before this one.  Mutually
 * recursive with create_procedure_variable().  If the stack depth
 * exceeds the number of symbols in the namespace, then we have found
 * variables that depend on one other, which shouldn't happen.  The
 * symbol is guaranteed to be a variable.  This recursive solution
 * avoids a nasty topological sort. */  
  
static void find_dependent_vars(g95_symbol *symbol) {   
g95_array_spec *as;         
int h;     
     
  if (symbol->ts.type == BT_CHARACTER && symbol->ts.cl != NULL &&          
      symbol->ts.cl->length != NULL)         
    traverse_spec_expr(symbol->ts.cl->length);     
     
  as = symbol->as;     
  if (as == NULL) return;

  for(h=0; h<as->rank; h++) {         
    traverse_spec_expr(as->lower[h]);    
    traverse_spec_expr(as->upper[h]);          
  }       
}  
  
  


/* create_procedure_variable()-- Create a variable */

static void create_procedure_variable(g95_symbol *sy) {          
          
  if (sy->ns != g95_current_ns) return;  
  
  if (sy->attr.dummy && !sy->mark) {          
    find_dependent_vars(sy);         
    if (!sy->attr.function && sy->attr.dimension) {     
      g95_fix_dummy_array(sy); 
      sy->mark = 1;
    }         
         
    return;         
  }  
  
  if (sy->backend_decl != NULL || sy->attr.entry) return;      
      
  if (sy->attr.flavor == FL_PROCEDURE &&          
      sy->attr.proc != PROC_INTRINSIC &&          
      sy->attr.proc != PROC_UNKNOWN) {
    g95_get_extern_function_decl(sy);   
    return;        
  }

  if (sy->attr.in_common) return;       
       
  if (sy->attr.flavor != FL_VARIABLE &&
      sy->attr.flavor != FL_PARAMETER)   
    return;     
     
  if (sy->attr.flavor == FL_PARAMETER && sy->as == NULL &&       
      sy->ts.type != BT_DERIVED)         
    return;       
       
  if (!sy->attr.used && !sy->attr.set &&   
      sy->ns->proc_name->attr.flavor != FL_MODULE)   
    return;

  /* At this point, start thinking about creating the variable */    
    
  find_dependent_vars(sy);   
   
  g95_get_symbol_decl(sy);         
}          
          
          
     
     
/* create_procvar()-- Recursively traverse a namespace, creating
 * variables as we go. */     
     
static void create_procvar(g95_symtree *sta) {       
       
  if (sta == NULL) return;     
     
  create_procvar(sta->left); 
  create_procvar(sta->right);     
     
  create_procedure_variable(sta->n.sym); 
}          
          
          


/* allocate_array_return()-- Generate code to initialize an array
 * return. */        
        
static void allocate_array_return(g95_symbol *result) { 
g95_array_spec *spec;   
stmtblock_t block;   
int rank, i, j;  
g95_se se0;        
tree tmp1;      
      
  g95_init_se(&se0, NULL);       
      
  j = 0;
  spec = result->as;       
  rank = spec->rank;          
          
  tmp1 = build_int_2(rank, 0);      
  g95_set_section_info(&se0, j++, tmp1);         
         
  tmp1 = size_in_bytes(g95_typenode_for_spec(&result->ts));   
  g95_set_section_info(&se0, j++, tmp1); 
 
  for(i=0; i<rank; i++) {     
    g95_conv_expr(&se0, spec->lower[i]);         
    g95_set_section_info(&se0, j++, se0.expr);

    g95_conv_expr(&se0, spec->upper[i]);     
    g95_set_section_info(&se0, j++, se0.expr);         
  }       
       
  g95_init_block(&block);       
  g95_add_block_to_block(&block, &se0.pre);          
          
  tmp1 = g95_call_library(pvoid_type_node, PREFIX "array_from_section",   
			 NULL_TREE); 
 
  g95_add_modify_expr(&block, g95_result_var_decl, tmp1);   
  g95_add_block_to_block(&block, &se0.post);       
       
  g95_add_block_to_block(&g95_context->pre, &block); 
}  
  
  


/* g95_build_procedure_decl()-- Create a declaration for a procedure.
 * For external functions (in the C sense) use g95_get_extern_function_decl. */         
         
void g95_build_procedure_decl(g95_symbol *sym) {      
tree proc_decl, t, result_decl, typelist, arglist, arglist_tail,      
     parm, result_pointer, result_length, tmp1;         
symbol_attribute atr;  
g95_formal_arglist *p;        
g95_symbol *frm;       
       
  if (sym->backend_decl) return;        
  assert(!sym->attr.external);    
    
  atr = sym->attr;   
   
  t = g95_procedure_type(sym); 
  proc_decl = build_decl(FUNCTION_DECL, g95_sym_identifier(sym, NULL), t);    
    
  /* Figure out the return type of the procedure, and build a
   * RESULT_DECL for it.  If this is subroutine with alternate
   * returns, build a RESULT_DECL for it. */       
       
  result_decl = build_decl(RESULT_DECL, NULL_TREE, g95_result_type(sym));         
  DECL_CONTEXT(result_decl) = proc_decl;   
  DECL_RESULT(proc_decl) = result_decl;     
     
  /* Don't call layout_decl for a RESULT_DECL.
  layout_decl(result_decl, 0); */         
         
  /* If the return type is a pointer, avoid alias issues by setting
   * DECL_IS_MALLOC to nonzero. This means that the function should
   * be treated as if it were a malloc, meaning it returns a pointer
   * that is not an alias. */  
  
  if (POINTER_TYPE_P(t)) DECL_IS_MALLOC(proc_decl) = 1;     
     
  /* Set up all attributes for the function. */  
  
  DECL_EXTERNAL(proc_decl) = 0;  
  DECL_CONTEXT(proc_decl) = sym->ns->backend_decl;        
        
  /* This specifies if a function is globally addressable, ie. it is
   * the opposite of declaring static in C.  */    
    
  if (current_function_decl == NULL_TREE &&     
      !sym->attr.artificial) TREE_PUBLIC(proc_decl) = 1;  
  
  /* TREE_STATIC means the function body is defined here.  */    
  TREE_STATIC(proc_decl) = 1;  
  
  /* Set attributes for PURE functions. A call to PURE function in the
   * Fortran 95 sense is both pure and without side effects in the C
   * sense */       
       
  if (atr.pure || atr.elemental) {  
    DECL_IS_PURE(proc_decl) = 1;    
    TREE_SIDE_EFFECTS(proc_decl) = 0;     
  } 
 
  /* Layout the function declaration and put it in the binding level
   * of the current function.  */          
          
  pushdecl(proc_decl);  
  
  /* Build formal argument list. Make sure that their TREE_CONTEXT
   * is the new FUNCTION_DECL node. */   
   
  current_function_decl = proc_decl;          
  arglist = NULL_TREE;          
  arglist_tail = NULL_TREE;

  typelist = TYPE_ARG_TYPES(TREE_TYPE(proc_decl));    
    
  /* Declare prepended arguments used for returning non-simple values */ 
 
  result_pointer = NULL_TREE;     
  result_length  = NULL_TREE;

  if (sym->attr.function && !sym->result->attr.pointer &&  
      sym->result->as == NULL) {

    switch(sym->result->ts.type) {       
    case BT_CHARACTER:       
      if (sym->result->ts.type == BT_CHARACTER) {       
	tmp1 = g95_sym_identifier(sym, "result_len");        
        
	t = build_pointer_type(g95_default_integer);    
	result_length = build_decl(PARM_DECL, tmp1, t);  
  
	DECL_CONTEXT(result_length) = proc_decl;      
	DECL_ARG_TYPE(result_length) = t;      
      
	g95_finish_decl(result_length, NULL_TREE);     
     
	arglist = chainon(arglist, result_length);  
	typelist = TREE_CHAIN(typelist);      
      }     
     
      break;      
      
    case BT_COMPLEX: 
    case BT_DERIVED:         
      t = TREE_VALUE(typelist);         
         
      tmp1 = g95_sym_identifier(sym, "result"); 
      result_pointer = build_decl(PARM_DECL, tmp1, t);        
        
      DECL_CONTEXT(result_pointer) = proc_decl;       
      DECL_ARG_TYPE(result_pointer) = t;    
      TREE_READONLY(result_pointer) = 1;    
    
      g95_finish_decl(result_pointer, NULL_TREE);      
      
      arglist = chainon(arglist, result_pointer);     
      typelist = TREE_CHAIN(typelist);          
          
    default:     
      break;      
    }     
  }      
      
  for(p=sym->formal; p; p=p->next) { 
    frm = p->sym;          
    if (frm == NULL) continue;   /* ignore alt return placeholders. */
    frm->mark = 0;          
          
    if (frm->backend_decl == NULL) g95_get_symbol_decl(frm);        
    parm = frm->backend_decl;    
    DECL_CONTEXT(parm) = proc_decl; 
 
    g95_finish_decl(parm, NULL_TREE);         
         
    arglist = chainon(arglist, parm); 
    typelist = TREE_CHAIN(typelist);         
         
    if (frm->ts.type == BT_CHARACTER) {      
      parm = build_decl(PARM_DECL, g95_sym_identifier(frm, "len"), 
			g95_default_integer);          
          
      DECL_CONTEXT(parm) = proc_decl;         
      DECL_ARG_TYPE(parm) = g95_default_integer;   
      DECL_ARG_TYPE_AS_WRITTEN(parm) = g95_default_integer;     
      TREE_READONLY(parm) = 1;     
      g95_finish_decl(parm, NULL_TREE);       
       
      arglist_tail = chainon(arglist_tail, parm);          
          
      /* Give an assumed length character its length, otherwise ignore. */   
   
      if (frm->ts.cl->length == NULL)  
	frm->ts.cl->backend_decl = parm;
    } 
  }          
          
  arglist = chainon(arglist, arglist_tail);     
  DECL_ARGUMENTS(proc_decl) = arglist;     
     
  /* Restore the old context.  */          
  current_function_decl = DECL_CONTEXT(proc_decl);       
       
  sym->backend_decl = proc_decl;          
}    
    
    
          
          
/* g95_build_builtin_decls()-- Make prototypes for runtime
 * library functions.  */      
      
void g95_build_builtin_decls(void) { 
 
  g95_init_array_types();

  library_procedure_alloc =  
    g95_library_decl(PREFIX "procedure_alloc", void_type_node, 2, 
		     ppvoid_type_node, g95_default_integer);    
    
  library_temp_alloc =
    g95_library_decl(PREFIX "temp_alloc", pvoid_type_node, 1,   
		     g95_default_integer);       
       
  library_temp_free =
    g95_library_decl(PREFIX "temp_free", void_type_node, 1,     
		     ppvoid_type_node);

  /* Exponentiation functions */    
    
  library_integer_4_power =     
    g95_library_decl(PREFIX "integer_4_power", g95_default_integer, 2,      
		     g95_default_integer, g95_default_integer);      
      
  library_real_4_power =      
    g95_library_decl(PREFIX "real_4_power", g95_real4_type_node, 2,  
		     g95_real4_type_node, g95_default_integer);  
  
  library_real_8_power =      
    g95_library_decl(PREFIX "real_8_power", g95_real8_type_node, 2,    
		     g95_real8_type_node, g95_default_integer);   
   
  library_complex_4_power = 
    g95_library_decl(PREFIX "complex_4_power", void_type_node, 2,        
		     pvoid_type_node, g95_default_integer);

  library_complex_8_power =   
    g95_library_decl(PREFIX "complex_8_power", g95_real8_type_node, 2,   
		     pvoid_type_node, g95_default_integer); 
 
  library_pow_complex_4 =  
    g95_library_decl(PREFIX "pow_complex_4", void_type_node, 3,          
		     pvoid_type_node, pvoid_type_node, pvoid_type_node);         
         
  library_pow_complex_8 =    
    g95_library_decl(PREFIX "pow_complex_8", void_type_node, 3,          
		     pvoid_type_node, pvoid_type_node, pvoid_type_node);  
  
  gfor_fndecl_math_powf =   
    g95_library_decl("powf", g95_real4_type_node, 1, g95_real4_type_node);

  gfor_fndecl_math_pow =          
    g95_library_decl("pow", g95_real8_type_node, 1, g95_real8_type_node);        
        
  gfor_fndecl_math_cabsf =    
    g95_library_decl("cabsf", g95_real4_type_node, 1, g95_complex4_type_node);      
      
  gfor_fndecl_math_cabs = 
    g95_library_decl("cabs", g95_real8_type_node, 1, g95_complex8_type_node);    
    
  gfor_fndecl_math_ishftc4 =   
    g95_library_decl("_gfor_ishftc4", g95_default_integer, 3,         
		     g95_default_integer, g95_default_integer,  
		     g95_default_integer);     
     
  gfor_fndecl_math_ishftc8 =  
    g95_library_decl("_gfor_ishftc8", g95_int8_type_node, 3,
		     g95_int8_type_node, g95_int8_type_node,       
		     g95_int8_type_node);          
          
  g95_build_io_library_fndecls();        
}


        
        
/* g95_get_symbol_decl()-- Create the declaration for a symbol. */

void g95_get_symbol_decl(g95_symbol *symb) {  
  
  if (symb->attr.dummy)      
    get_parm_decl(symb);       
  else
    get_variable_decl(symb);       
}  
  
  
          
          
/* g95_get_extern_function_decl()-- Get a basic decl for an external
 * function. */    
    
tree g95_get_extern_function_decl(g95_symbol *symbol) {
g95_expr m, argexpr[G95_MAX_SPECIFIC_ARGS];
char g[G95_MAX_SYMBOL_LEN+15];     
g95_intrinsic_arg *frm;   
tree nm, type, fd;     
g95_intrinsic_sym *isym;      
int r;      
      
  if (symbol->backend_decl) return symbol->backend_decl;  
  
  if (!symbol->attr.intrinsic)    
    nm = g95_sym_identifier(symbol, NULL);   
  else {    
    /* Call the resolution function to get the actual name.  */   
    isym = g95_find_function(symbol->name);         
    assert (isym->resolve);     
     
    memset (&m, 0, sizeof(m));      
    memset (argexpr, 0, sizeof(argexpr));        
    m.type = EXPR_FUNCTION;     
    frm = NULL; 
    r = 0;

    for (frm=isym->formal, r=0; frm; frm=frm->next, r++) { 
      assert (r < G95_MAX_SPECIFIC_ARGS);
      argexpr[r].ts = frm->ts;    
    }

    switch (r) {        
    case 0: 
      isym->resolve(&m);     
      break;   
   
    case 1:      
      isym->resolve(&m, &argexpr[0]);        
      break; 
 
    case 2:        
      isym->resolve(&m, &argexpr[0], &argexpr[1]);          
      break;

    default:       
      abort();
    }  
  
    nm = get_identifier(m.value.function.name);         
  }          
          
  type = g95_procedure_type(symbol);       
  fd = build_decl(FUNCTION_DECL, nm, type);

  /* If the return type is a pointer, avoid alias issues by setting
   * DECL_IS_MALLOC to nonzero. This means that the function should be
   * treated as if it were a malloc, meaning it returns a pointer that
   * is not an alias.  */   
   
  if (POINTER_TYPE_P(type)) DECL_IS_MALLOC(fd) = 1;      
      
  /* Set up all attributes for the function.  */  
  
  DECL_CONTEXT(fd) = current_function_decl;          
  DECL_EXTERNAL(fd) = 1;       
       
  /* This specifies if a function is globally addressable, ie. it is
   * the opposite of declaring static in C. */       
       
  TREE_PUBLIC(fd) = 1;         
         
  /* Set attributes for PURE functions.  A call to PURE function in the
   * Fortran 95 sense is both pure and without side effects in the C
   * sense. */       
       
  if (symbol->attr.pure || symbol->attr.elemental) {         
    DECL_IS_PURE(fd) = 1;          
    TREE_SIDE_EFFECTS(fd) = 0;          
  }       
       
  symbol->backend_decl = fd;       
  return fd;        
}


       
       
/* g95_get_return_label()-- Returns the return label for the current
 * function. */          
          
tree g95_get_return_label(void) {  
char n[G95_MAX_SYMBOL_LEN + 20];  
  
  if (current_function_return_label)   
    return current_function_return_label;   
   
  sprintf(n, "__return_%s", 
	  IDENTIFIER_POINTER(DECL_NAME(current_function_decl)));  
  
  current_function_return_label = g95_build_label_decl(get_identifier(n));      
      
  DECL_ARTIFICIAL(current_function_return_label) = 1;        
        
  return current_function_return_label;          
}          
          
          
         
         
/* traverse_spec_expr()-- Traverse a specification expression, looking
 * for as yet undefined variables. */       
       
static void traverse_spec_expr(g95_expr *x) {         
g95_actual_arglist *a;     
     
  if (x == NULL) return;      
      
  switch(x->type) { 
  case EXPR_OP:       
    traverse_spec_expr(x->op1);     
    traverse_spec_expr(x->op2);
    break;         
         
  case EXPR_FUNCTION:         
    if (x->symbol->backend_decl == NULL)
      create_procedure_variable(x->symbol);

    for(a=x->value.function.actual; a; a=a->next)        
      traverse_spec_expr(a->u.expr);        
    break;      
      
  case EXPR_VARIABLE:       
    specification_variable(x);         
    break;     
     
  default:          
    break; 
  }       
}       
       
       
          
          
/* init_result_var_decl()-- Create the variable used to hold the
 * function return value.  This is whether the result variable is
 * explicit or not. */   
   
static void init_result_var_decl(g95_symbol *symb) {     
tree dtype, d, len, identifier;         
g95_symbol *res;          
g95_se s;

  g95_result_var_decl = NULL_TREE;          
  len = NULL_TREE; 
  res = symb->result;    
    
  if (symb->attr.subroutine) {     
    d = build_decl(VAR_DECL, NULL, g95_default_integer);      
    goto finish_decl;    
  }       
       
  find_dependent_vars(res);          
          
  /* Initialize a character return length */    
  if (res->ts.type == BT_CHARACTER)
    g95_typenode_for_spec(&res->ts);         
         
  identifier = (symb->result == symb)     
    ? g95_sym_identifier(symb, "result")  
    : g95_sym_identifier(symb->result, NULL);          
          
  if (res->attr.pointer && res->as != NULL) { /* Pointer array returns */          
    dtype = g95_get_array_desc(res->as->rank);          
    d = build_decl(VAR_DECL, identifier, dtype);  
    goto finish_decl;
  }          
          
  if (symb->attr.pointer || symb->as != NULL) goto pointer;

  switch(symb->ts.type) {    
  case BT_CHARACTER:   
    len = DECL_ARGUMENTS(symb->backend_decl); 
    len = build1(INDIRECT_REF, g95_default_integer, len);

    d = build_decl(VAR_DECL, identifier, pchar_type_node);          
          
    if (symb->ts.cl->length != NULL) {   
      g95_init_se(&s, NULL);         
      g95_conv_expr(&s, res->ts.cl->length);  
  
      g95_add_block_to_block(&g95_context->pre, &s.pre);          
      g95_add_modify_expr(&g95_context->pre, len, s.expr);  
      g95_add_block_to_block(&g95_context->pre, &s.post);        
    }    
    
    g95_call_temp_alloc(&g95_context->pre, d, len);          
    goto finish_decl;          
          
  case BT_COMPLEX:     
  case BT_DERIVED:    
    d = DECL_ARGUMENTS(symb->backend_decl);        
    break;        
        
  default:      
  pointer:         
    dtype = g95_result_type(symb);          
    d = build_decl(VAR_DECL, identifier, dtype);       
       
  finish_decl:       
    TREE_ADDRESSABLE(d) = 1;         
    DECL_ARTIFICIAL(d) = 1;    
    DECL_EXTERNAL(d) = 0;   
    TREE_PUBLIC(d) = 0;   
   
    layout_decl(d, 0);      
      
    pushdecl(d);  
    rest_of_decl_compilation(d, NULL, 1, 0);   
    TREE_USED(d) = 1;        
        
    g95_result_var_decl = d;
    break;        
  }     
     
  if (res != NULL && res != symb)          
    res->backend_decl = d;        
        
  if (len != NULL_TREE) 
    res->ts.cl->backend_decl = len;        
        
  if (symb->attr.function && !res->attr.pointer && res->as != NULL)          
    allocate_array_return(res);
}        
        
        
        
        
/* count_syms()-- Recursive function for counting the number of
 * symbols within a namespace.  This count is used to detect infinite
 * recursion during procedure variable creation. */      
      
static int count_syms(g95_symtree *st1) {      
      
  if (st1 == NULL) return 0;         
  return 1 + count_syms(st1->left) + count_syms(st1->right);   
}   
   
   
  
  
/* g95_generate_procedure()-- Generate code for a procedure.  */        
        
void g95_generate_procedure(g95_namespace *names) {      
tree fndecl, tmp1, saved_function_decl; 
g95_trans_context context;        
stmtblock_t list, blk;     
g95_namespace *child;       
g95_symbol *sym;       
       
  g95_current_ns = names;     
  sym = names->proc_name;   
  names->backend_decl = sym->backend_decl;         
         
  /* Create the declaration for functions with global scope */ 
 
  context.parent = g95_context;      
  g95_context = &context;

  context.saved_current_function = current_function_decl;         
  context.current_procedure = names;    
  context.result_var_decl = g95_result_var_decl; 
  context.frame_size = 0; 
 
  g95_init_block(&context.pre);
  g95_init_block(&context.post);         
         
  if (context.parent != NULL) push_function_context();        
        
  /* let GCC know the current scope is this function */         
         
  current_function_decl = fndecl = sym->backend_decl;   
  context.current_function_decl = fndecl; 
 
  /* print function name on the console at compile time unless this
   * feature was switched of by command line option "-quiet" */       
       
  announce_function(fndecl);

  /* create RTL for function declaration */

  if (DECL_CONTEXT(fndecl) == NULL_TREE)     
    rest_of_decl_compilation(fndecl, NULL, 1, 0);  
  
  /* create RTL for function definition */  
  make_decl_rtl(fndecl, NULL); 
 
  /* Set the line and filename.  sym->declared_at seems to point to the last
   * statement for subroutines, but it'll do for now. */    
    
  g95_set_backend_locus(&sym->declared_at);       
       
  /* line and file should not be 0 */  
  init_function_start(fndecl, input_filename, lineno);     
     
  /* We're in function-at-a-time mode. */        
  cfun->x_whole_function_mode_p = 1;   
   
  /* Even though we're inside a function body, we still don't want to
   * call expand_expr to calculate the size of a variable-sized array.
   * We haven't necessarily assigned RTL to all variables yet, so it's
   * not safe to try to expand expressions involving them.  */    
    
  immediate_size_expand = 0;   
  cfun->x_dont_save_pending_sizes_p = 1;     
     
  g95_start_block(&list);        
        
  /* function.c requires a push at the start of the function */    
  pushlevel(0);         
         
  /* Now generate the code for the body of this function */
  g95_init_block(&blk);    
    
  /* Procedure declarations for contained procedures */ 
  for(child=names->contained; child; child=child->sibling) {   
    if (child->parent != names) continue;         
    g95_build_procedure_decl(child->proc_name);       
  } 
 
  /* If we have a function without an explicit result variable, the
   * function name is the result variable.  Save the declaration and
   * replace it with the result variable.  When done, restore the
   * original declaration. */     
     
  init_result_var_decl(sym);         
  saved_function_decl = NULL_TREE;     
     
  if (sym->attr.function && sym->result == sym) { 
    saved_function_decl = sym->backend_decl;  
  
    sym->backend_decl = (g95_result_var_decl != NULL)         
      ? g95_result_var_decl 
      : DECL_ARGUMENTS(sym->backend_decl);         
  }    
    
  current_function_return_label = NULL; 
 
  /* Generate common blocks */         
         
  g95_trans_common(names);      
  g95_generate_procedure_variables(names);    
    
  tmp1 = g95_trans_code(names->code);     
     
  g95_add_block_to_block(&blk, &context.pre);       
  g95_add_expr_to_block(&blk, tmp1);     
     
  /* Add a return label if needed. */     
     
  if (current_function_return_label) {         
    tmp1 = build_v(LABEL_EXPR, current_function_return_label);     
    g95_add_expr_to_block(&blk, tmp1);     
  }   
   
  tmp1 = g95_finish_block(&blk);   
   
  /* Add code to create and cleanup arrays */    
  tmp1 = generate_entry_exit(tmp1);       
  g95_add_expr_to_block(&list, tmp1);  
  
  g95_add_block_to_block(&list, &context.post);   
   
  /* Generate code for contained procedures. */   
   
  for(child=names->contained; child; child=child->sibling) { 
    if (child->parent != names) continue;         
    g95_generate_procedure(child);          
  }  
  
  /* Restore the symbol declaration to the function version if we had
   * to replace it with the result variable. */

  if (saved_function_decl != NULL_TREE)    
    sym->backend_decl = saved_function_decl;         
         
  if (g95_result_var_decl != NULL_TREE) {   
    if (sym->attr.function && sym->result->attr.pointer && 
	sym->result->as != NULL)  
      tmp1 = pointer_array_return(&list);          
    else     
      tmp1 = g95_result_var_decl;         
         
    tmp1 = build(MODIFY_EXPR, TREE_TYPE(tmp1), DECL_RESULT(fndecl), tmp1);        
    tmp1 = build_v(RETURN_EXPR, tmp1);  
    g95_add_expr_to_block(&list, tmp1);          
  }    
    
  tmp1 = g95_finish_block(&list);         
  DECL_SAVED_TREE(fndecl) = tmp1; 
 
  /* Finish off this function and send it for code generation. */   
   
  poplevel(1, 0, 1);  
  BLOCK_SUPERCONTEXT(DECL_INITIAL(fndecl)) = fndecl;        
        
  {   /* Output the SIMPLE tree. */          
    FILE *dump_file;
    int dump_flags = 0;     
    tree fnbody;      
      
    fnbody = DECL_SAVED_TREE(fndecl);  
  
    dump_file = dump_begin(TDI_original, &dump_flags); 
    if (dump_file) {  
      fprintf(dump_file, "%s()\n", names->proc_name->name);      
      if (fnbody) {       
	if (dump_flags & TDF_RAW)       
	  dump_node(fnbody, TDF_SLIM | dump_flags, dump_file);       
	else        
	  print_c_tree(dump_file, fnbody);   
      }     
     
      fprintf(dump_file, "\n");      
      dump_end(TDI_original, dump_file);         
    }         
  }    
    
  free_after_parsing(cfun);       
  free_after_compilation(cfun);      
      
  /* RTL generation */          
          
  if (context.parent == NULL)      
    expand_function_body(fndecl, 0);         
  else          
    pop_function_context();       
       
  g95_context = context.parent;         
  current_function_decl = context.saved_current_function;   
  g95_result_var_decl = context.result_var_decl; 
}    
    
      
      
/* g95_generate_procedure_variables()-- Generate variables. */      
      
void g95_generate_procedure_variables(g95_namespace *n) {          
          
  symbol_count = count_syms(n->sym_root) + 10;   
   
  create_procvar(n->sym_root);       
       
  generate_entries(n);  
}  
  
  
         
         
#include "gt-f95-trans-decl.h"
