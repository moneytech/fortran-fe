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
 
static GTY(()) tree result_var_decl;         
static GTY(()) tree current_function_return_label;       
       
/* Function declarations for builtin library functions.  */  
  
tree library_temp_alloc;
tree library_temp_free;     
tree library_procedure_alloc;

tree library_allocate_pointer_array;          
tree library_allocate_allocatable_array;

tree library_select_string;        
tree gfor_fndecl_runtime_error;     
tree gfor_fndecl_repack[G95_MAX_DIMENSIONS]; 
 
/* Math functions.  Many other math functions are handled in
   trans-intrinsic.c.  */       
       
tree library_integer_4_power;    
tree library_real_4_power;      
tree library_real_8_power;

tree gfor_fndecl_math_powf;       
tree gfor_fndecl_math_pow;         
tree gfor_fndecl_math_cpowf;          
tree gfor_fndecl_math_cpow;        
tree gfor_fndecl_math_cabsf; 
tree gfor_fndecl_math_cabs;     
tree gfor_fndecl_math_sign4;      
tree gfor_fndecl_math_sign8;   
tree gfor_fndecl_math_ishftc4;      
tree gfor_fndecl_math_ishftc8;   
   
/* Other misc. runtime library functions.  */       
tree gfor_fndecl_size0;         
tree gfor_fndecl_size1;

g95_trans_context *g95_context = NULL; 
 
/* None of the specific intrinsics which can be passed as actual arguments
 * for dummy procedures has more then two parameters.  */   
   
#define G95_MAX_SPECIFIC_ARGS 2
   
   
   
        
        
/* g95_finish_decl()-- Finish processing of a declaration and install
 * its initial value. */       
       
static void g95_finish_decl(tree decl, tree init) {   
   
  if (TREE_CODE (decl) == PARM_DECL) 
    assert (init == NULL_TREE);     
     
  /* Remember that PARM_DECL doesn't have a DECL_INITIAL field per se
     -- it overlaps DECL_ARG_TYPE.  */     
     
  else if (init == NULL_TREE)    
    assert (DECL_INITIAL (decl) == NULL_TREE);        
  else         
    assert (DECL_INITIAL (decl) == error_mark_node);

  if (init != NULL_TREE) {      
    if (TREE_CODE (decl) != TYPE_DECL) 
      DECL_INITIAL (decl) = init; 
    else {
      /* typedef foo = bar; store the type of bar as the type of foo.  */
      TREE_TYPE (decl) = TREE_TYPE (init);   
      DECL_INITIAL (decl) = init = 0;         
    }  
  }     
     
  if (TREE_CODE(decl) == VAR_DECL) {   
    if (DECL_SIZE (decl) == NULL_TREE     
	&& TYPE_SIZE(TREE_TYPE(decl)) != NULL_TREE) 
      layout_decl(decl, 0); 
 
    if (DECL_SIZE(decl) == NULL_TREE && (TREE_STATIC(decl) ?      
          /* A static variable with an incomplete type is an error if it is
             initialized. Also if it is not file scope. Otherwise, let it
             through, but if it is not `extern' then it may cause an error
             message later.  */          
					 (DECL_INITIAL (decl) != 0   
					  || DECL_CONTEXT (decl) != 0) :  
          /* An automatic variable with an incomplete type is an error.  */   
					 !DECL_EXTERNAL (decl))) {   
      g95_fatal_error("storage size not known");
    }       
       
    if ((DECL_EXTERNAL(decl) || TREE_STATIC(decl))     
	&& (DECL_SIZE(decl) != 0)
	&& (TREE_CODE(DECL_SIZE(decl)) != INTEGER_CST)) {     
      g95_fatal_error("storage size not constant");          
    }
  }       
}       
       
       
       
       
/* g95_build_procedure_decl()-- Create a declaration for a procedure.
 * For external functions (in the C sense) use g95_get_extern_function_decl. */ 
 
void g95_build_procedure_decl(g95_symbol *symb) {         
tree proc_decl, type, result_decl, typelist, arglist, arglist_tail, 
     parm, result_pointer, result_length, tmp;      
symbol_attribute attr;        
g95_formal_arglist *h;
g95_symbol *formal;      
      
  if (symb->backend_decl) return;     
  assert(!symb->attr.external);     
     
  /* Allow only one nesting level.  Allow external declarations.  */     
     
  assert(current_function_decl == NULL_TREE  
	 || DECL_CONTEXT(current_function_decl) == NULL_TREE);         
         
  type = g95_procedure_type(symb); 
  proc_decl = build_decl(FUNCTION_DECL, g95_sym_identifier(symb, NULL), type);  
  
  /* Figure out the return type of the procedure, and build a
   * RESULT_DECL for it.  If this is subroutine with alternate
   * returns, build a RESULT_DECL for it. */       
       
  result_decl = build_decl(RESULT_DECL, NULL_TREE, g95_result_type(symb)); 
  DECL_CONTEXT(result_decl) = proc_decl;    
  DECL_RESULT(proc_decl) = result_decl;  
  
  /* Don't call layout_decl for a RESULT_DECL.
  layout_decl(result_decl, 0); */        
        
  /* If the return type is a pointer, avoid alias issues by setting
   * DECL_IS_MALLOC to nonzero. This means that the function should
   * be treated as if it were a malloc, meaning it returns a pointer
   * that is not an alias. */          
          
  if (POINTER_TYPE_P(type)) DECL_IS_MALLOC(proc_decl) = 1;

  /* Set up all attributes for the function. */        
        
  DECL_CONTEXT(proc_decl) = symb->ns->backend_decl; 
  DECL_EXTERNAL(proc_decl) = 0;    
    
  /* This specifies if a function is globally addressable, ie. it is
   * the opposite of declaring static in C.  */   
   
  if (current_function_decl == NULL_TREE || attr.external)         
    TREE_PUBLIC(proc_decl) = 1;        
        
  /* TREE_STATIC means the function body is defined here.  */ 
  if (!attr.external) TREE_STATIC(proc_decl) = 1;         
         
  /* Set attributes for PURE functions. A call to PURE function in the
   * Fortran 95 sense is both pure and without side effects in the C
   * sense */          
          
  if (attr.pure || attr.elemental) {
    DECL_IS_PURE(proc_decl) = 1;   
    TREE_SIDE_EFFECTS(proc_decl) = 0;   
  }         
         
  /* Layout the function declaration and put it in the binding level
   * of the current function.  */         
         
  if (!attr.external) { 
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

    if (symb->attr.function) {     
      if (symb->result->as != NULL)       
	g95_internal_error("Array returns");       
      else {   
	switch(symb->result->ts.type) {       
	case BT_CHARACTER:  
	case BT_COMPLEX:     
	case BT_DERIVED:         
	  type = TREE_VALUE(typelist);  
  
	  tmp = g95_sym_identifier(symb, "result");         
	  result_pointer = build_decl(PARM_DECL, tmp, type);          
          
	  DECL_CONTEXT(result_pointer) = proc_decl;          
	  DECL_ARG_TYPE(result_pointer) = type;
	  TREE_READONLY(result_pointer) = 1;

	  g95_finish_decl(result_pointer, NULL_TREE);        
        
	  arglist = chainon(arglist, result_pointer);    
	  typelist = TREE_CHAIN(typelist);   
   
	  if (symb->result->ts.type == BT_CHARACTER) {          
	    tmp = g95_sym_identifier(symb, "result_len"); 
	    result_length = build_decl(PARM_DECL, tmp, g95_default_integer);      
      
	    DECL_CONTEXT(result_length) = proc_decl;    
	    DECL_ARG_TYPE(result_length) = g95_default_integer;        
	    TREE_READONLY(result_length) = 1;       
       
	    g95_finish_decl(result_length, NULL_TREE);         
         
	    arglist = chainon(arglist, result_length);          
	    typelist = TREE_CHAIN(typelist); 
	  }  
  
	default:   
	  break;
	}   
      }        
    }  
  
    for(h=symb->formal; h; h=h->next) {  
      formal = h->sym;        
      if (formal == NULL) continue;   /* ignore alt return placeholders. */

      type = TREE_VALUE(typelist); 
 
      /* Build an argument declaration.  */ 
 
      parm = build_decl(PARM_DECL, g95_sym_identifier(formal, NULL), type);       
       
      /* Fill in argument information */        
        
      DECL_CONTEXT(parm) = proc_decl;      
      DECL_ARG_TYPE(parm) = type;         
      DECL_ARG_TYPE_AS_WRITTEN(parm) = type;    
    
      /* All implementation args are read-only.  */ 
      TREE_READONLY(parm) = 1;    
    
      g95_finish_decl(parm, NULL_TREE); 
      formal->backend_decl = parm;        
        
      arglist = chainon(arglist, parm);
      typelist = TREE_CHAIN(typelist);

      if (formal->ts.type == BT_CHARACTER) {       
	parm = build_decl(PARM_DECL, g95_sym_identifier(formal, "len"),   
			  g95_default_integer);      
      
	DECL_CONTEXT(parm) = proc_decl;     
	DECL_ARG_TYPE(parm) = g95_default_integer;    
	DECL_ARG_TYPE_AS_WRITTEN(parm) = g95_default_integer;        
	TREE_READONLY(parm) = 1; 
	g95_finish_decl(parm, NULL_TREE);     
     
	arglist_tail = chainon(arglist_tail, parm);

	/* Give an assumed length character its length, otherwise ignore. */   
   
	if (formal->ts.cl->length == NULL)         
	  G95_TYPE_STRING_LENGTH(type) = parm;    
      }  
    }

    arglist = chainon(arglist, arglist_tail);        
        
    DECL_ARGUMENTS(proc_decl) = arglist;   
   
    /* Restore the old context.  */   
    current_function_decl = DECL_CONTEXT(proc_decl);        
  }

  symb->backend_decl = proc_decl;      
}        
        
        
      
      
/* g95_library_decl()-- Builds a function decl.  The remaining
 * parameters are the types of the function arguments.  Negative nargs
 * indicates a varargs function.  */   
   
tree g95_library_decl VPARAMS((char *name, tree rettype, int nargs, ...)) {     
tree arglist, argtype, fntype, fndecl;     
int o;

  /* Library functions must be declared with global scope.  */         
  assert(current_function_decl == NULL_TREE);      
      
  VA_OPEN(g, nargs);
  VA_FIXEDARG(g, tree, get_identifier(name));   
  VA_FIXEDARG(g, tree, retval);
  VA_FIXEDARG(g, int, nargs);       
       
  /* Create a list of the argument types.  */       
  for(arglist=NULL_TREE, o=abs(nargs); o>0; o--) {          
    argtype = va_arg(g, tree);  
    arglist = g95_chainon_list(arglist, argtype);
  }          
          
  /* Terminate the list */

  if (nargs >= 0) arglist = g95_chainon_list(arglist, void_type_node);    
    
  /* Build the function type and decl */  
  
  fntype = build_function_type(rettype, arglist);          
  fndecl = build_decl(FUNCTION_DECL, get_identifier(name), fntype);    
    
  /* Mark this decl as external */  
  
  DECL_EXTERNAL(fndecl) = 1;          
  TREE_PUBLIC(fndecl) = 1;  
  VA_CLOSE(g);        
        
  pushdecl(fndecl);       
       
  rest_of_decl_compilation(fndecl, NULL, 1, 0);        
  return fndecl;     
}     
     
     
     
      
      
/* g95_get_extern_function_decl()-- Get a basic decl for an external
 * function.  */      
      
tree g95_get_extern_function_decl(g95_symbol *symb) {   
g95_expr v, argexpr[G95_MAX_SPECIFIC_ARGS];         
char w[G95_MAX_SYMBOL_LEN+15];     
g95_intrinsic_arg *formal;         
tree name, type, fndecl;          
g95_intrinsic_sym *isym;     
int x;         
         
  if (symb->backend_decl) return symb->backend_decl;      
      
  if (!symb->attr.intrinsic)       
    name = g95_sym_identifier(symb, NULL);     
  else {  
    /* Call the resolution function to get the actual name.  */ 
    isym = g95_find_function(symb->name);      
    assert (isym->resolve);      
      
    memset (&v, 0, sizeof(v));       
    memset (argexpr, 0, sizeof(argexpr));    
    v.type = EXPR_FUNCTION;        
    formal = NULL;    
    x = 0;      
      
    for (formal=isym->formal, x=0; formal; formal=formal->next, x++) {    
      assert (x < G95_MAX_SPECIFIC_ARGS);   
      argexpr[x].ts = formal->ts;  
    }       
       
    switch (x) {  
    case 0: 
      isym->resolve(&v);   
      break;     
     
    case 1: 
      isym->resolve(&v, &argexpr[0]);         
      break;          
          
    case 2:       
      isym->resolve(&v, &argexpr[0], &argexpr[1]);    
      break;         
         
    default:   
      abort();  
    }      
      
    sprintf(w, "specific%s", v.value.function.name);    
    name = get_identifier(w); 
  }   
   
  type = g95_procedure_type(symb);  
  fndecl = build_decl(FUNCTION_DECL, name, type);    
    
  /* If the return type is a pointer, avoid alias issues by setting
   * DECL_IS_MALLOC to nonzero. This means that the function should be
   * treated as if it were a malloc, meaning it returns a pointer that
   * is not an alias.  */         
         
  if (POINTER_TYPE_P(type)) DECL_IS_MALLOC(fndecl) = 1;   
   
  /* Set up all attributes for the function.  */         
         
  DECL_CONTEXT(fndecl) = current_function_decl;
  DECL_EXTERNAL(fndecl) = 1;        
        
  /* This specifies if a function is globally addressable, ie. it is
   * the opposite of declaring static in C. */      
      
  TREE_PUBLIC(fndecl) = 1;  
  
  /* Set attributes for PURE functions.  A call to PURE function in the
   * Fortran 95 sense is both pure and without side effects in the C
   * sense. */   
   
  if (symb->attr.pure || symb->attr.elemental) {      
    DECL_IS_PURE(fndecl) = 1;        
    TREE_SIDE_EFFECTS(fndecl) = 0;          
  }      
      
  symb->backend_decl = fndecl;       
  return fndecl;   
}     
     
     
   
   
/* finish_var_decl()-- Apply symbol attributes to a variable, and add
 * it to the function scope.  */          
          
static void finish_var_decl(tree decl, g95_symbol *symb) {        
        
  /* TREE_ADDRESSABLE means the address of this variable is needed for
   * a TARGET variable.  We also need to set this if the variable is
   * passed by reference in a CALL statement. */

  if (symb->attr.target) TREE_ADDRESSABLE(decl) = 1;        
        
  /* If it wasn't used we wouldn't be getting it.  */     
  TREE_USED(decl) = 1;  
  
  /* If a variable is USE associated, it's always external.  */        
        
  if (symb->attr.use_assoc) {      
    DECL_EXTERNAL(decl) = 1;    
    TREE_PUBLIC(decl) = 1;          
  } else if (g95_module_symbol(symb)) {     
    assert(current_function_decl == NULL_TREE);        
    /* This is the declaration of a module variable.  */         
    TREE_PUBLIC(decl) = 1;        
    TREE_STATIC(decl) = 1; 
  }        
        
  if ((symb->attr.save || symb->attr.data || symb->value)      
      && !symb->attr.use_assoc) TREE_STATIC(decl) = 1;  
  
  DECL_CONTEXT(decl) = symb->ns->backend_decl;   
}   
   
   
/* g95_initial_value()-- Return the initial value for a variable. */  
  
tree g95_initial_value(g95_symbol *symb) {  
  
  if (symb->value == NULL || symb->as != NULL) return NULL_TREE;      
      
  if (symb->ts.type == BT_DERIVED && !symb->attr.pointer)          
    g95_todo_error("Derived type initializer");     
     
  return g95_conv_constant(symb->value, NULL_TREE);     
}         
         
         
          
          
static void g95_build_intrinsic_function_decls(void) {   
   
  /* Power functions.  */  
  
  library_integer_4_power =        
    g95_library_decl(PREFIX "integer_4_power", g95_default_integer, 2,       
		     g95_default_integer, g95_default_integer);  
  
  library_real_4_power =          
    g95_library_decl(PREFIX "real_4_power", g95_real4_type_node, 2,        
		     g95_real4_type_node, g95_default_integer);         
         
  library_real_8_power =  
    g95_library_decl(PREFIX "real_8_power", g95_real8_type_node, 2,      
		     g95_real8_type_node, g95_default_integer);       
       
  gfor_fndecl_math_powf =    
    g95_library_decl("powf", g95_real4_type_node, 1, g95_real4_type_node);        
        
  gfor_fndecl_math_pow =     
    g95_library_decl("pow", g95_real8_type_node, 1, g95_real8_type_node);         
         
  gfor_fndecl_math_cpowf =   
    g95_library_decl("cpowf", g95_complex4_type_node, 1,       
		     g95_complex4_type_node);   
   
  gfor_fndecl_math_cpow =          
    g95_library_decl("cpow", g95_complex8_type_node, 1,
		     g95_complex8_type_node);    
    
  gfor_fndecl_math_cabsf =      
    g95_library_decl("cabsf", g95_real4_type_node, 1, g95_complex4_type_node);  
  
  gfor_fndecl_math_cabs =      
    g95_library_decl("cabs", g95_real8_type_node, 1, g95_complex8_type_node);      
      
  gfor_fndecl_math_sign4 =       
    g95_library_decl("copysignf", g95_real4_type_node, 1, g95_real4_type_node);          
            
  gfor_fndecl_math_sign8 =         
    g95_library_decl("copysign", g95_real8_type_node, 1, g95_real8_type_node);

  gfor_fndecl_math_ishftc4 =
    g95_library_decl("_gfor_ishftc4", g95_default_integer, 3,      
		     g95_default_integer, g95_default_integer,  
		     g95_default_integer); 
 
  gfor_fndecl_math_ishftc8 =     
    g95_library_decl("_gfor_ishftc8", g95_int8_type_node, 3,   
		     g95_int8_type_node, g95_int8_type_node,
		     g95_int8_type_node);
}       
       
       
       
/* Generate function entry and exit code, and add it to the function body. */          
          
static tree trans_deferred_vars(g95_symbol *proc_sym, tree fnbody) {  
stmtblock_t block;       
tree tmp;  
  
  g95_init_block(&block);      
      
  tmp = g95_call_library(void_type_node, PREFIX "push_context", NULL_TREE);        
  g95_add_expr_to_block(&block, tmp);     
     
  g95_add_expr_to_block(&block, fnbody);  
  
  tmp = g95_call_library(void_type_node, PREFIX "pop_context", NULL_TREE);     
  g95_add_expr_to_block(&block, tmp);

  return g95_finish_block(&block);        
}       
       
       
 
/* Make prototypes for runtime library functions.  */          
          
void g95_build_builtin_function_decls(void) {   
char name[16];     
int u;         
         
  library_procedure_alloc =          
    g95_library_decl(PREFIX "procedure_alloc", void_type_node, 2,   
		     ppvoid_type_node, g95_default_integer);    
    
  library_temp_alloc =      
    g95_library_decl(PREFIX "temp_alloc", pvoid_type_node, 1,          
		     g95_default_integer);    
    
  library_temp_free =     
    g95_library_decl(PREFIX "temp_free", void_type_node, 1,     
		     ppvoid_type_node);  
  
  library_allocate_pointer_array =     
    g95_library_decl(PREFIX "allocate_pointer_array", void_type_node, 2,
		     ppvoid_type_node, pvoid_type_node);          
          
  gfor_fndecl_runtime_error = 
    g95_library_decl("_gfor_runtime_error", void_type_node, 3,    
		     pchar_type_node, pchar_type_node, g95_default_integer);    
      
  for(u=0; u<G95_MAX_DIMENSIONS; u++) {      
    sprintf(name, "_gfor_repack_%d", u);       
    gfor_fndecl_repack[u] =
      g95_library_decl(name, void_type_node, 3,     
		       ppvoid_type_node, ppvoid_type_node,     
		       g95_default_integer);    
  }    
    
  g95_build_intrinsic_function_decls();      
  g95_build_intrinsic_lib_fndecls();      
  g95_build_io_library_fndecls();      
  g95_init_array_types();         
}          
          
          
          
          
/* g95_build_label_decl()-- Build a backend label declaration.  Set
 * TREE_USED for named lables.  For artificial labels it's up to the
 * caller to mark the label as used.  */         
         
tree g95_build_label_decl(tree label_id) {  
static unsigned int tmp_num = 1;    
char *label_name; 
tree label_decl;         
         
  if (label_id != NULL_TREE)       
    label_name = NULL; 
  else {    /* Build an internal label name. */     
    ASM_FORMAT_PRIVATE_NAME(label_name, "L", tmp_num++);   
    label_id = get_identifier(label_name);   
  }   
   
  /* Build the LABEL_DECL node. Labels have no type. */          
  label_decl = build_decl(LABEL_DECL, label_id, void_type_node);

  DECL_CONTEXT(label_decl) = current_function_decl;   
  DECL_MODE(label_decl) = VOIDmode;  
  
  if (label_name)  
    DECL_ARTIFICIAL(label_decl) = 1;  
  else
    TREE_USED(label_decl) = 1;
  
  /* We always define the label as used, even if the original source
   * file never references the label.  We don't want all kinds of
   * spurious warnings for old-style Fortran code with too many
   * labels.  */      
      
  return label_decl;   
}          
          
          


/* g95_get_label_decl()-- Return the backend label declaration for a
 * given label structure, or create it if it doesn't exist yet.  */       
       
tree g95_get_label_decl(g95_st_label *lp) {        
char label_name[20];       
tree label_decl;       
       
  if (lp->backend_decl) return lp->backend_decl;     
     
  sprintf(label_name, "__label_%.6d", lp->value);    
  label_decl = g95_build_label_decl(get_identifier(label_name));    
    
  if (lp->value <= MAX_LABEL_VALUE) {  
    DECL_SOURCE_LINE(label_decl) = lp->where.line;  
    DECL_SOURCE_FILE(label_decl) = lp->where.file->filename;     
  } else         
    DECL_ARTIFICIAL(label_decl) = 1;        
        
  lp->backend_decl = label_decl;        
  return label_decl;   
}        
        
        
       
       
/* g95_get_return_label()-- Returns the return label for the current
 * function. */     
     
tree g95_get_return_label(void) {
char name[G95_MAX_SYMBOL_LEN + 20]; 
 
  if (current_function_return_label)
    return current_function_return_label;    
    
  sprintf(name, "__return_%s",    
	  IDENTIFIER_POINTER(DECL_NAME(current_function_decl)));

  current_function_return_label = g95_build_label_decl(get_identifier(name));          
          
  DECL_ARTIFICIAL(current_function_return_label) = 1;  
  
  return current_function_return_label;  
}         
         
         


/* get_symbol_decl()-- Create the declaration for a symbol. */       
       
static void get_symbol_decl(g95_symbol *s) {   
variable_info vinfo; 
tree decl, tmp;         
         
  /* Catch function declarations.  Only used for actual parameters.  */      
      
  if (s->attr.flavor == FL_PROCEDURE) {     
    /* If a procedure is not known to be a function or subroutine,
     * assume that it is a subroutine for the purposes of getting a
     * declaration. */

    if (!s->attr.function && !s->attr.subroutine) 
      s->attr.subroutine = 1;      
      
    s->backend_decl = g95_get_extern_function_decl(s);     
    return;       
  }

  if (s->attr.intrinsic)       
    g95_internal_error("intrinsic variable which isn't a procedure");   
   
  g95_symbol_vinfo(s, &vinfo);        
  tmp = g95_get_descriptor(&vinfo);

  decl = build_decl(VAR_DECL, g95_sym_identifier(s, NULL), tmp);        
  pushdecl(decl);     
  finish_var_decl(decl, s);
  //rest_of_decl_compilation(decl, NULL, 1, 0);

  s->backend_decl = decl;       
       
  if (!G95_DESCRIPTOR_P(tmp))      
    DECL_INITIAL(s->backend_decl) = g95_initial_value(s);        
  else {   
    tmp = g95_get_storage(&vinfo, G95_TYPE_STRING_LENGTH(tmp));      
      
    if (tmp == NULL_TREE)   
      g95_init_descriptor(&vinfo, decl, tmp);     
    else {      
      tmp = build_decl(VAR_DECL, g95_sym_identifier(s, "data"), tmp);  
  
      pushdecl(tmp);         
      finish_var_decl(tmp, s);         
      g95_init_descriptor(&vinfo, decl, tmp);   
      //rest_of_decl_compilation(tmp, NULL, 1, 0);   
    }    
  }    
}         
         
         
      
      
/* g95_sym_identifier()-- Convert a name of a g95_symbol to an
 * identifier name. */

tree g95_sym_identifier(g95_symbol *s, char *suffix) {       
char mangled_name[3*G95_MAX_SYMBOL_LEN+4];         
         
  if (!g95_module_symbol(s) || strcmp(s->module, "(global)") == 0)
    strcpy(mangled_name, s->name);         
  else 
    sprintf(mangled_name, "%s.%s", s->module, s->name);         
         
  if (suffix != NULL) {    
    strcat(mangled_name, ".");   
    strcat(mangled_name, suffix);     
  }          
          
  return get_identifier(mangled_name);        
}  
  
  
 
 
/* init_result_var_decl()-- Create the variable used to hold the
 * function return value.  This is whether the result variable is
 * explicit or not. */         
         
static void init_result_var_decl(g95_symbol *symb) {   
tree type, decl;          
          
  result_var_decl = NULL_TREE;       
  if (symb->attr.subroutine) return;   
   
  if (symb->as != NULL)          
    g95_internal_error("Array return result");       
  else
    switch(symb->ts.type) {       
    case BT_CHARACTER: 
    case BT_COMPLEX:      
    case BT_DERIVED:          
      decl = DECL_ARGUMENTS(symb->backend_decl);         
         
      if (symb->ts.type == BT_CHARACTER)       
	G95_TYPE_STRING_LENGTH(TREE_TYPE(decl)) =    
	  (symb->result->ts.cl->length == NULL) 
	  ? TREE_CHAIN(decl)   
	  : g95_conv_constant(symb->result->ts.cl->length, NULL_TREE);        
        
      break;    
    
    default:        
      type = g95_result_type(symb); 
      decl = (symb->result == symb)        
	? g95_sym_identifier(symb, "result") 
	: g95_sym_identifier(symb->result, NULL);      
      
      decl = build_decl(VAR_DECL, decl, type);          
          
      DECL_ARTIFICIAL(decl) = 1;         
      DECL_EXTERNAL(decl) = 0; 
      TREE_PUBLIC(decl) = 0;       
       
      layout_decl(decl, 0);  
  
      pushdecl(decl);      
      rest_of_decl_compilation(decl, NULL, 1, 0);       
      TREE_USED(decl) = 1; 
 
      result_var_decl = decl; 
      break;       
    }        
        
  if (symb->result != symb) symb->result->backend_decl = decl;  
}          
          
          
         
         
/* create_procedure_variable()-- Create a variable */     
     
static void create_procedure_variable(g95_symbol *s) {

  if (s->ns != g95_current_ns || s->backend_decl != NULL) return;  
  
  if (s->attr.flavor == FL_PROCEDURE &&         
      s->attr.proc != PROC_INTRINSIC && 
      s->attr.proc != PROC_UNKNOWN) {
    g95_get_extern_function_decl(s); 
    return;         
  }       
       
  if (s->attr.flavor != FL_VARIABLE || s->attr.in_common ||  
      (!s->attr.used && !s->attr.set &&   
       s->ns->proc_name->attr.flavor != FL_MODULE)) 
    return;    
    
  get_symbol_decl(s);     
}


 
 
/* g95_generate_module_variables()-- Generate module variables. */       
       
void g95_generate_procedure_variables(g95_namespace *ns) {      
      
  g95_current_ns = ns;        
  g95_traverse_ns(ns, create_procedure_variable);       
} 
 
 
   
   
/* fix_actual()-- Given an actual argument, make any changes to it
 * that may be necessary.  The backend_decl field has the declaration
 * for the argument passed.  This might get modified. */     
     
static void fix_actual(g95_symbol *s) {

  if (s->attr.dimension) {      
    g95_fix_dummy_array(s);        
    return;    
  } 
}  
  
  
  
  
/* g95_generate_procedure()-- Generate code for a procedure.  */     
     
void g95_generate_procedure(g95_namespace *ns) {       
tree fndecl, tmp, saved_function_decl;  
g95_trans_context context;  
stmtblock_t block, body;  
g95_formal_arglist *q;      
g95_namespace *child;  
g95_symbol *symb;         
         
  symb = ns->proc_name;         
  ns->backend_decl = symb->backend_decl;    
    
  /* Create the declaration for functions with global scope */         
         
  context.parent = g95_context;     
  g95_context = &context;

  context.saved_current_function = current_function_decl;        
  context.current_procedure = ns;          
          
  g95_init_block(&context.pre);     
  g95_init_block(&context.post);       
       
  if (context.parent != NULL) push_function_context();          
          
  /* let GCC know the current scope is this function */          
          
  current_function_decl = fndecl = symb->backend_decl;        
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

  g95_set_backend_locus(&symb->declared_at);        
        
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
   
  g95_start_block(&block);       
       
  /* function.c requires a push at the start of the function */ 
  pushlevel(0);  
  
  /* If we have a function without an explicit result variable, the
   * function name is the result variable.  Save the declaration and
   * replace it with the result variable.  When done, restore the
   * original declaration. */ 
 
  init_result_var_decl(symb);   
  saved_function_decl = NULL_TREE;

  if (symb->attr.function && symb->result == symb) {      
    saved_function_decl = symb->backend_decl; 
 
    symb->backend_decl = (result_var_decl != NULL)   
      ? result_var_decl     
      : DECL_ARGUMENTS(symb->backend_decl);
  }      
      
  current_function_return_label = NULL;         
         
  /* Now generate the code for the body of this function */          
  g95_init_block(&body);    
    
  /* Procedure declarations for contained procedures */ 
  for(child=ns->contained; child; child=child->sibling) {      
    if (child->parent != ns) continue;       
    g95_build_procedure_decl(child->proc_name);   
  }        
        
  /* Generate common blocks */        
  g95_trans_common(ns);   
   
  g95_generate_procedure_variables(ns);  
  
  /* Massage the actual argument list */
  for(q=symb->formal; q; q=q->next)          
    fix_actual(q->sym); 
 
  tmp = g95_trans_code(ns->code);      
      
  g95_add_block_to_block(&body, &context.pre);       
  g95_add_expr_to_block(&body, tmp);          
          
  /* Restore the symbol declaration to the function version if we had
   * to replace it with the result variable. */          
          
  if (saved_function_decl != NULL_TREE)       
    symb->backend_decl = saved_function_decl;    
    
  /* Add a return label if needed. */   
   
  if (current_function_return_label) {        
    tmp = build_v(LABEL_EXPR, current_function_return_label);  
    g95_add_expr_to_block(&body, tmp);      
  }

  tmp = g95_finish_block(&body);          
          
  /* Add code to create and cleanup arrays */    
  tmp = trans_deferred_vars(symb, tmp);         
  g95_add_expr_to_block(&block, tmp);         
         
  g95_add_block_to_block(&block, &context.post);         
         
  if (result_var_decl != NULL_TREE) {     
    tmp = build(MODIFY_EXPR, TREE_TYPE(result_var_decl), DECL_RESULT(fndecl),    
		result_var_decl);
    tmp = build_v(RETURN_EXPR, tmp);
    g95_add_expr_to_block(&block, tmp);  
  }     
     
  /* Generate code for contained procedures. */          
          
  for(child=ns->contained; child; child=child->sibling) {      
    if (child->parent != ns) continue;   
    g95_generate_procedure(child);  
  }          
          
  tmp = g95_finish_block(&block);     
  DECL_SAVED_TREE(fndecl) = tmp;

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
      fprintf(dump_file, "%s()\n", ns->proc_name->name);  
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
}  
  
       
       
#include "gt-f95-trans-decl.h"
