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
         
g95_trans_context *g95_context = NULL;

/* None of the specific intrinsics which can be passed as actual arguments
 * for dummy procedures has more then two parameters.  */

#define G95_MAX_SPECIFIC_ARGS 2
   
   
   
    
    
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
 
 
  
  
/* g95_get_extern_function_decl()-- Get a basic decl for an external
 * function. */       
       
tree g95_get_extern_function_decl(g95_symbol *symb) {         
g95_expr z, argexpr[G95_MAX_SPECIFIC_ARGS];    
char u[G95_MAX_SYMBOL_LEN+15];     
g95_intrinsic_arg *frm;       
tree name, typ, decl;
g95_intrinsic_sym *is;      
int v;          
          
  if (symb->backend_decl) return symb->backend_decl;     
     
  if (!symb->attr.intrinsic) 
    name = g95_sym_identifier(symb, NULL); 
  else {   
    /* Call the resolution function to get the actual name.  */          
    is = g95_find_function(symb->name);          
    assert (is->resolve);       
       
    memset (&z, 0, sizeof(z));         
    memset (argexpr, 0, sizeof(argexpr));
    z.type = EXPR_FUNCTION;      
    frm = NULL;   
    v = 0;  
  
    for (frm=is->formal, v=0; frm; frm=frm->next, v++) {    
      assert (v < G95_MAX_SPECIFIC_ARGS); 
      argexpr[v].ts = frm->ts;     
    }    
    
    switch (v) {          
    case 0:       
      is->resolve(&z);          
      break;         
         
    case 1:    
      is->resolve(&z, &argexpr[0]);         
      break;    
    
    case 2:          
      is->resolve(&z, &argexpr[0], &argexpr[1]);
      break;        
        
    default:          
      abort();        
    }     
     
    sprintf(u, "specific%s", z.value.function.name);     
    name = get_identifier(u);     
  }        
        
  typ = g95_procedure_type(symb);   
  decl = build_decl(FUNCTION_DECL, name, typ);          
          
  /* If the return type is a pointer, avoid alias issues by setting
   * DECL_IS_MALLOC to nonzero. This means that the function should be
   * treated as if it were a malloc, meaning it returns a pointer that
   * is not an alias.  */        
        
  if (POINTER_TYPE_P(typ)) DECL_IS_MALLOC(decl) = 1;    
    
  /* Set up all attributes for the function.  */   
   
  DECL_CONTEXT(decl) = current_function_decl;         
  DECL_EXTERNAL(decl) = 1;         
         
  /* This specifies if a function is globally addressable, ie. it is
   * the opposite of declaring static in C. */    
    
  TREE_PUBLIC(decl) = 1;     
     
  /* Set attributes for PURE functions.  A call to PURE function in the
   * Fortran 95 sense is both pure and without side effects in the C
   * sense. */          
          
  if (symb->attr.pure || symb->attr.elemental) {         
    DECL_IS_PURE(decl) = 1;         
    TREE_SIDE_EFFECTS(decl) = 0;         
  }        
        
  symb->backend_decl = decl;      
  return decl;  
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
      
      
 
 
/* fix_actual()-- Given an actual argument, make any changes to it
 * that may be necessary.  The backend_decl field has the declaration
 * for the argument passed.  This might get modified. */   
   
static void fix_actual(g95_symbol *s) {  
  
  if (s->attr.dimension) {   
    g95_fix_dummy_array(s);    
    return;
  } 
}   
   
   
   
   
/* finish_var_decl()-- Apply symbol attributes to a variable, and add
 * it to the function scope.  */    
    
static void finish_var_decl(tree decl, g95_symbol *sy) {  
  
  /* TREE_ADDRESSABLE means the address of this variable is needed for
   * a TARGET variable.  We also need to set this if the variable is
   * passed by reference in a CALL statement. */    
    
  if (sy->attr.target) TREE_ADDRESSABLE(decl) = 1;      
      
  /* If it wasn't used we wouldn't be getting it.  */        
  TREE_USED(decl) = 1;          
          
  /* If a variable is USE associated, it's always external.  */         
         
  if (sy->attr.use_assoc) {   
    DECL_EXTERNAL(decl) = 1;
    TREE_PUBLIC(decl) = 1; 
  } else if (g95_module_symbol(sy)) {       
    assert(current_function_decl == NULL_TREE);
    /* This is the declaration of a module variable.  */       
    TREE_PUBLIC(decl) = 1;        
    TREE_STATIC(decl) = 1;          
  }   
   
  if ((sy->attr.save || sy->attr.data || sy->value)        
      && !sy->attr.use_assoc) TREE_STATIC(decl) = 1;   
   
  DECL_CONTEXT(decl) = sy->ns->backend_decl;      
}      
      
      
/* g95_initial_value()-- Return the initial value for a variable. */

tree g95_initial_value(g95_symbol *sy) {
variable_info vin;    
tree string_length;      
g95_se se0;   
   
  if (sy->value == NULL) return NULL_TREE;        
        
  if (sy->ts.type == BT_DERIVED && !sy->attr.pointer)   
    g95_todo_error("Derived type initializer");          
          
  g95_init_se(&se0, NULL);

  if (sy->as == NULL) {         
    string_length = (sy->ts.type == BT_CHARACTER)      
      ? sy->ts.cl->backend_decl       
      : NULL_TREE; 
 
    g95_conv_constant(&se0, sy->value, string_length); 
  } else {      
    g95_symbol_vinfo(sy, &vin); 
    se0.expr = g95_conv_array_initializer(&vin);    
  }

  return se0.expr;        
}          
          
          
        
        
/* g95_finish_decl()-- Finish processing of a declaration and install
 * its initial value. */     
     
static void g95_finish_decl(tree dec, tree i) {         
         
  if (TREE_CODE (dec) == PARM_DECL)        
    assert (i == NULL_TREE);

  /* Remember that PARM_DECL doesn't have a DECL_INITIAL field per se
     -- it overlaps DECL_ARG_TYPE.  */      
      
  else if (i == NULL_TREE)      
    assert (DECL_INITIAL (dec) == NULL_TREE);        
  else      
    assert (DECL_INITIAL (dec) == error_mark_node);        
        
  if (i != NULL_TREE) {   
    if (TREE_CODE (dec) != TYPE_DECL)      
      DECL_INITIAL (dec) = i;       
    else {     
      /* typedef foo = bar; store the type of bar as the type of foo.  */  
      TREE_TYPE (dec) = TREE_TYPE (i);    
      DECL_INITIAL (dec) = i = 0;        
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
     
     
        
        
/* g95_library_decl()-- Builds a function decl.  The remaining
 * parameters are the types of the function arguments.  Negative nargs
 * indicates a varargs function.  */      
      
tree g95_library_decl VPARAMS((char *name0, tree rettype, int nargs, ...)) {    
tree alist, argtype, fntype, f;  
int u;         
         
  /* Library functions must be declared with global scope.  */ 
  assert(current_function_decl == NULL_TREE);

  VA_OPEN(a, nargs);   
  VA_FIXEDARG(a, tree, get_identifier(name0));      
  VA_FIXEDARG(a, tree, retval); 
  VA_FIXEDARG(a, int, nargs);    
    
  /* Create a list of the argument types.  */     
  for(alist=NULL_TREE, u=abs(nargs); u>0; u--) { 
    argtype = va_arg(a, tree);
    alist = g95_chainon_list(alist, argtype);       
  }         
         
  /* Terminate the list */   
   
  if (nargs >= 0) alist = g95_chainon_list(alist, void_type_node);         
         
  /* Build the function type and decl */ 
 
  fntype = build_function_type(rettype, alist);  
  f = build_decl(FUNCTION_DECL, get_identifier(name0), fntype);         
         
  /* Mark this decl as external */    
    
  DECL_EXTERNAL(f) = 1;
  TREE_PUBLIC(f) = 1;          
  VA_CLOSE(a); 
 
  pushdecl(f); 
 
  rest_of_decl_compilation(f, NULL, 1, 0);       
  return f;   
}



        
        
/* g95_get_symbol_decl()-- Create the declaration for a symbol. */ 
 
void g95_get_symbol_decl(g95_symbol *symbol) {         
variable_info info;
tree declr, tmp0;         
         
  /* Catch function declarations.  Only used for actual parameters.  */    
    
  if (symbol->attr.flavor == FL_PROCEDURE) {    
    /* If a procedure is not known to be a function or subroutine,
     * assume that it is a subroutine for the purposes of getting a
     * declaration. */    
    
    if (!symbol->attr.function && !symbol->attr.subroutine)        
      symbol->attr.subroutine = 1;  
  
    symbol->backend_decl = g95_get_extern_function_decl(symbol);
    return; 
  }  
  
  if (symbol->attr.intrinsic)        
    g95_internal_error("intrinsic variable which isn't a procedure");     
     
  g95_symbol_vinfo(symbol, &info);         
  tmp0 = g95_get_descriptor(&info); 
 
  declr = build_decl(VAR_DECL, g95_sym_identifier(symbol, NULL), tmp0);      
  pushdecl(declr);
  finish_var_decl(declr, symbol);     
     
  symbol->backend_decl = declr; 
 
  if (!G95_DESCRIPTOR_P(tmp0))        
    DECL_INITIAL(symbol->backend_decl) = g95_initial_value(symbol); 
  else {      
    tmp0 = g95_get_storage(&info);     
     
    if (tmp0 == NULL_TREE)    
      g95_init_descriptor(&info, declr, tmp0);
    else {    
      tmp0 = build_decl(VAR_DECL, g95_sym_identifier(symbol, "data"), tmp0);  
  
      if (info.value) {     
	DECL_INITIAL(tmp0) = g95_conv_array_initializer(&info);    
	TREE_STATIC(tmp0) = 1;     
      }    
    
      pushdecl(tmp0);      
      finish_var_decl(tmp0, symbol);          
      g95_init_descriptor(&info, declr, tmp0);  
    }       
  }     
}  
  
  
     
     
/* g95_build_procedure_decl()-- Create a declaration for a procedure.
 * For external functions (in the C sense) use g95_get_extern_function_decl. */      
      
void g95_build_procedure_decl(g95_symbol *sym) { 
tree proc_decl, t, result_decl, typelist, arg, arglist_tail,          
     parm, result_pointer, result_length, tmp1;       
symbol_attribute a;     
g95_formal_arglist *h;       
g95_symbol *form;     
     
  if (sym->backend_decl) return; 
  assert(!sym->attr.external);        
        
  /* Allow only one nesting level.  Allow external declarations.  */         
         
  assert(current_function_decl == NULL_TREE        
	 || DECL_CONTEXT(current_function_decl) == NULL_TREE);

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
    
  DECL_CONTEXT(proc_decl) = sym->ns->backend_decl;        
  DECL_EXTERNAL(proc_decl) = 0;      
      
  /* This specifies if a function is globally addressable, ie. it is
   * the opposite of declaring static in C.  */   
   
  if (current_function_decl == NULL_TREE || a.external)          
    TREE_PUBLIC(proc_decl) = 1;

  /* TREE_STATIC means the function body is defined here.  */
  if (!a.external) TREE_STATIC(proc_decl) = 1;          
          
  /* Set attributes for PURE functions. A call to PURE function in the
   * Fortran 95 sense is both pure and without side effects in the C
   * sense */      
      
  if (a.pure || a.elemental) {     
    DECL_IS_PURE(proc_decl) = 1;  
    TREE_SIDE_EFFECTS(proc_decl) = 0;   
  }       
       
  /* Set return variable length */        
        
  if (sym->attr.function) g95_typenode_for_spec(&sym->result->ts);      
      
  /* Layout the function declaration and put it in the binding level
   * of the current function.  */     
     
  if (!a.external) {     
    pushdecl(proc_decl);          
          
    /* Build formal argument list. Make sure that their TREE_CONTEXT
     * is the new FUNCTION_DECL node. */       
       
    current_function_decl = proc_decl;         
    arg = NULL_TREE;          
    arglist_tail = NULL_TREE;         
         
    typelist = TYPE_ARG_TYPES(TREE_TYPE(proc_decl));    
    
    /* Declare prepended arguments used for returning non-simple values */

    result_pointer = NULL_TREE;  
    result_length  = NULL_TREE;          
          
    if (sym->attr.function) {     
      if (sym->result->as != NULL)       
	g95_internal_error("Array returns");     
      else {  
	switch(sym->result->ts.type) {         
	case BT_CHARACTER:     
	case BT_COMPLEX:      
	case BT_DERIVED:   
	  t = TREE_VALUE(typelist);   
   
	  tmp1 = g95_sym_identifier(sym, "result");    
	  result_pointer = build_decl(PARM_DECL, tmp1, t);        
        
	  DECL_CONTEXT(result_pointer) = proc_decl;      
	  DECL_ARG_TYPE(result_pointer) = t;          
	  TREE_READONLY(result_pointer) = 1;     
     
	  g95_finish_decl(result_pointer, NULL_TREE);        
        
	  arg = chainon(arg, result_pointer);
	  typelist = TREE_CHAIN(typelist);         
         
	  if (sym->result->ts.type == BT_CHARACTER) {  
	    tmp1 = g95_sym_identifier(sym, "result_len");
	    result_length = build_decl(PARM_DECL, tmp1, g95_default_integer);         
         
	    DECL_CONTEXT(result_length) = proc_decl; 
	    DECL_ARG_TYPE(result_length) = g95_default_integer;
	    TREE_READONLY(result_length) = 1;     
     
	    g95_finish_decl(result_length, NULL_TREE);          
          
	    arg = chainon(arg, result_length); 
	    typelist = TREE_CHAIN(typelist);    
	  }   
   
	default:   
	  break;      
	}          
      }          
    }   
   
    for(h=sym->formal; h; h=h->next) {     
      form = h->sym;        
      if (form == NULL) continue;   /* ignore alt return placeholders. */       
       
      t = TREE_VALUE(typelist);         
         
      /* Build an argument declaration.  */   
   
      parm = build_decl(PARM_DECL, g95_sym_identifier(form, NULL), t);         
         
      /* Fill in argument information */    
    
      DECL_CONTEXT(parm) = proc_decl;   
      DECL_ARG_TYPE(parm) = t; 
      DECL_ARG_TYPE_AS_WRITTEN(parm) = t;    
    
      /* All implementation args are read-only.  */     
      TREE_READONLY(parm) = 1;     
     
      g95_finish_decl(parm, NULL_TREE);  
      form->backend_decl = parm;     
     
      arg = chainon(arg, parm);    
      typelist = TREE_CHAIN(typelist);         
         
      if (form->ts.type == BT_CHARACTER) {    
	parm = build_decl(PARM_DECL, g95_sym_identifier(form, "len"),       
			  g95_default_integer);         
         
	DECL_CONTEXT(parm) = proc_decl;         
	DECL_ARG_TYPE(parm) = g95_default_integer;   
	DECL_ARG_TYPE_AS_WRITTEN(parm) = g95_default_integer; 
	TREE_READONLY(parm) = 1;    
	g95_finish_decl(parm, NULL_TREE);    
    
	arglist_tail = chainon(arglist_tail, parm);

	/* Give an assumed length character its length, otherwise ignore. */          
          
	if (form->ts.cl->length == NULL)  
	  form->ts.cl->backend_decl = parm;   
      }   
    }       
       
    arg = chainon(arg, arglist_tail);        
        
    DECL_ARGUMENTS(proc_decl) = arg;     
     
    /* Restore the old context.  */          
    current_function_decl = DECL_CONTEXT(proc_decl);
  }         
         
  sym->backend_decl = proc_decl;     
}         
         
         
          
          
/* init_result_var_decl()-- Create the variable used to hold the
 * function return value.  This is whether the result variable is
 * explicit or not. */       
       
static void init_result_var_decl(g95_symbol *sy) {       
tree typ, d;    
    
  result_var_decl = NULL_TREE;  
  if (sy->attr.subroutine) return; 
 
  if (sy->as != NULL) 
    g95_internal_error("Array return result"); 
  else    
    switch(sy->ts.type) {     
    case BT_CHARACTER:        
    case BT_COMPLEX:    
    case BT_DERIVED:          
      d = DECL_ARGUMENTS(sy->backend_decl);      
      break;         
         
    default:
      typ = g95_result_type(sy);      
      d = (sy->result == sy)        
	? g95_sym_identifier(sy, "result")
	: g95_sym_identifier(sy->result, NULL); 
 
      d = build_decl(VAR_DECL, d, typ);

      DECL_ARTIFICIAL(d) = 1;  
      DECL_EXTERNAL(d) = 0;        
      TREE_PUBLIC(d) = 0;       
       
      layout_decl(d, 0);  
  
      pushdecl(d);      
      rest_of_decl_compilation(d, NULL, 1, 0);
      TREE_USED(d) = 1;     
     
      result_var_decl = d;       
      break;         
    }     
     
  if (sy->result != sy) sy->result->backend_decl = d;          
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
       
static tree trans_deferred_vars(tree fnbody) {     
stmtblock_t blk;    
tree t;       
       
  g95_init_block(&blk);        
        
  t = g95_call_library(void_type_node, PREFIX "push_context", NULL_TREE);   
  g95_add_expr_to_block(&blk, t);      
      
  g95_add_expr_to_block(&blk, fnbody);     
     
  t = g95_call_library(void_type_node, PREFIX "pop_context", NULL_TREE); 
  g95_add_expr_to_block(&blk, t);       
       
  return g95_finish_block(&blk);  
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
          
          
 
 
/* create_procedure_variable()-- Create a variable */         
         
static void create_procedure_variable(g95_symbol *sym) {     
     
  if (sym->ns != g95_current_ns || sym->backend_decl != NULL) return;    
    
  if (sym->attr.flavor == FL_PROCEDURE && 
      sym->attr.proc != PROC_INTRINSIC &&         
      sym->attr.proc != PROC_UNKNOWN) {       
    g95_get_extern_function_decl(sym);     
    return;      
  }         
         
  if (sym->attr.flavor != FL_VARIABLE || sym->attr.in_common ||        
      (!sym->attr.used && !sym->attr.set &&  
       sym->ns->proc_name->attr.flavor != FL_MODULE))    
    return;       
       
  g95_get_symbol_decl(sym);  
}          
          
          
      
      
/* g95_build_builtin_function_decls()-- Make prototypes for runtime
 * library functions.  */ 
 
void g95_build_builtin_function_decls(void) {      
      
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
     
  g95_build_intrinsic_function_decls();     
  g95_build_intrinsic_lib_fndecls();    
  g95_build_io_library_fndecls();  
  g95_init_array_types();          
} 
 
 
       
       
/* g95_sym_identifier()-- Convert a name of a g95_symbol to an
 * identifier name. */          
          
tree g95_sym_identifier(g95_symbol *symbol, char *suffix) {      
char mangled_name[3*G95_MAX_SYMBOL_LEN+4];   
   
  if (!g95_module_symbol(symbol) || strcmp(symbol->module, "(global)") == 0)         
    strcpy(mangled_name, symbol->name);     
  else      
    sprintf(mangled_name, "%s.%s", symbol->module, symbol->name); 
 
  if (suffix != NULL) {     
    strcat(mangled_name, ".");          
    strcat(mangled_name, suffix);      
  }    
    
  return get_identifier(mangled_name);         
}     
     
     
       
       
/* g95_generate_procedure_variables()-- Generate variables. */         
         
void g95_generate_procedure_variables(g95_namespace *name) {     
     
  g95_current_ns = name;         
  g95_traverse_ns(name, create_procedure_variable);       
}       
       
       
 
 
/* g95_generate_procedure()-- Generate code for a procedure.  */        
        
void g95_generate_procedure(g95_namespace *ns) {  
tree fd, tmp, saved_function_decl;      
g95_trans_context context; 
stmtblock_t blk, list;       
g95_formal_arglist *y;      
g95_namespace *child;
g95_symbol *symbol;      
      
  symbol = ns->proc_name;       
  ns->backend_decl = symbol->backend_decl;      
      
  /* Create the declaration for functions with global scope */

  context.parent = g95_context;  
  g95_context = &context;       
       
  context.saved_current_function = current_function_decl;
  context.current_procedure = ns;       
       
  g95_init_block(&context.pre);     
  g95_init_block(&context.post);        
        
  if (context.parent != NULL) push_function_context(); 
 
  /* let GCC know the current scope is this function */   
   
  current_function_decl = fd = symbol->backend_decl;        
  context.current_function_decl = fd;

  /* print function name on the console at compile time unless this
   * feature was switched of by command line option "-quiet" */ 
 
  announce_function(fd);          
          
  /* create RTL for function declaration */        
        
  if (DECL_CONTEXT(fd) == NULL_TREE)  
    rest_of_decl_compilation(fd, NULL, 1, 0); 
 
  /* create RTL for function definition */ 
  make_decl_rtl(fd, NULL);      
      
  /* Set the line and filename.  sym->declared_at seems to point to the last
   * statement for subroutines, but it'll do for now. */         
         
  g95_set_backend_locus(&symbol->declared_at);     
     
  /* line and file should not be 0 */    
  init_function_start(fd, input_filename, lineno);  
  
  /* We're in function-at-a-time mode. */        
  cfun->x_whole_function_mode_p = 1;      
      
  /* Even though we're inside a function body, we still don't want to
   * call expand_expr to calculate the size of a variable-sized array.
   * We haven't necessarily assigned RTL to all variables yet, so it's
   * not safe to try to expand expressions involving them.  */ 
 
  immediate_size_expand = 0;  
  cfun->x_dont_save_pending_sizes_p = 1;  
  
  g95_start_block(&blk);    
    
  /* function.c requires a push at the start of the function */ 
  pushlevel(0);      
      
  /* If we have a function without an explicit result variable, the
   * function name is the result variable.  Save the declaration and
   * replace it with the result variable.  When done, restore the
   * original declaration. */    
    
  init_result_var_decl(symbol);        
  saved_function_decl = NULL_TREE;          
          
  if (symbol->attr.function && symbol->result == symbol) {    
    saved_function_decl = symbol->backend_decl;          
          
    symbol->backend_decl = (result_var_decl != NULL)    
      ? result_var_decl       
      : DECL_ARGUMENTS(symbol->backend_decl);     
  }     
     
  current_function_return_label = NULL;

  /* Now generate the code for the body of this function */    
  g95_init_block(&list);     
     
  /* Procedure declarations for contained procedures */     
  for(child=ns->contained; child; child=child->sibling) {     
    if (child->parent != ns) continue;      
    g95_build_procedure_decl(child->proc_name);      
  }

  /* Generate common blocks */          
  g95_trans_common(ns);   
   
  g95_generate_procedure_variables(ns);     
     
  /* Massage the actual argument list */   
  for(y=symbol->formal; y; y=y->next)         
    fix_actual(y->sym);   
   
  tmp = g95_trans_code(ns->code);         
         
  g95_add_block_to_block(&list, &context.pre);      
  g95_add_expr_to_block(&list, tmp); 
 
  /* Restore the symbol declaration to the function version if we had
   * to replace it with the result variable. */ 
 
  if (saved_function_decl != NULL_TREE) 
    symbol->backend_decl = saved_function_decl;     
     
  /* Add a return label if needed. */    
    
  if (current_function_return_label) {      
    tmp = build_v(LABEL_EXPR, current_function_return_label);    
    g95_add_expr_to_block(&list, tmp);  
  }  
  
  tmp = g95_finish_block(&list);     
     
  /* Add code to create and cleanup arrays */  
  tmp = trans_deferred_vars(tmp);       
  g95_add_expr_to_block(&blk, tmp);  
  
  g95_add_block_to_block(&blk, &context.post);     
     
  if (result_var_decl != NULL_TREE) {          
    tmp = build(MODIFY_EXPR, TREE_TYPE(result_var_decl), DECL_RESULT(fd),        
		result_var_decl);          
    tmp = build_v(RETURN_EXPR, tmp); 
    g95_add_expr_to_block(&blk, tmp);   
  }     
     
  /* Generate code for contained procedures. */    
    
  for(child=ns->contained; child; child=child->sibling) {   
    if (child->parent != ns) continue;        
    g95_generate_procedure(child);    
  }       
       
  tmp = g95_finish_block(&blk);      
  DECL_SAVED_TREE(fd) = tmp; 
 
  /* Finish off this function and send it for code generation. */        
        
  poplevel(1, 0, 1);   
  BLOCK_SUPERCONTEXT(DECL_INITIAL(fd)) = fd;  
  
  {   /* Output the SIMPLE tree. */     
    FILE *dump_file; 
    int dump_flags = 0; 
    tree fnbody;      
      
    fnbody = DECL_SAVED_TREE(fd);      
      
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
    expand_function_body(fd, 0);       
  else         
    pop_function_context();         
         
  g95_context = context.parent;         
  current_function_decl = context.saved_current_function;  
}  
  
        
        
#include "gt-f95-trans-decl.h"
