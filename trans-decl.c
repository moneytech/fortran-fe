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
tree cdesc_type_node, cdescp_type_node;  
  
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
     
     
     
     
/* get_parm_decl()-- Get a declaration associated with a dummy variable */       
       
static void get_parm_decl(g95_symbol *sy) {  
tree parm, typ;          
          
  typ = g95_dummy_arg_type(sy);     
     
  parm = build_decl(PARM_DECL, g95_sym_identifier(sy, NULL), typ); 
 
  DECL_ARG_TYPE(parm) = typ;    
  DECL_ARG_TYPE_AS_WRITTEN(parm) = typ; 
  TREE_READONLY(parm) = 1;        
        
  sy->backend_decl = parm;          
} 
 
 
          
          
/* g95_get_return_label()-- Returns the return label for the current
 * function. */        
        
tree g95_get_return_label(void) {      
char nam[G95_MAX_SYMBOL_LEN + 20];     
     
  if (current_function_return_label)       
    return current_function_return_label;    
    
  sprintf(nam, "__return_%s",    
	  IDENTIFIER_POINTER(DECL_NAME(current_function_decl)));

  current_function_return_label = g95_build_label_decl(get_identifier(nam));

  DECL_ARTIFICIAL(current_function_return_label) = 1;      
      
  return current_function_return_label;  
}  
  
  
         
         
/* g95_build_label_decl()-- Build a backend label declaration.  Set
 * TREE_USED for named lables.  For artificial labels it's up to the
 * caller to mark the label as used.  */     
     
tree g95_build_label_decl(tree label_id) {         
char *name;      
tree d;      
      
  if (label_id != NULL_TREE)         
    name = NULL;  
  else     /* Build an internal label name. */      
    label_id = g95_unique_identifier("label");    
    
  /* Build the LABEL_DECL node. Labels have no type. */         
  d = build_decl(LABEL_DECL, label_id, void_type_node);          
          
  DECL_CONTEXT(d) = current_function_decl;       
  DECL_MODE(d) = VOIDmode;          
          
  DECL_ARTIFICIAL(d) = 1;          
  TREE_USED(d) = 1;         
           
  /* We always define the label as used, even if the original source
   * file never references the label.  We don't want all kinds of
   * spurious warnings for old-style Fortran code with too many
   * labels. */         
         
  return d;  
}   
   
   


/* g95_initial_value()-- Return the initial value for a variable. */   
   
tree g95_initial_value(g95_symbol *sym) {          
variable_info vinfo;      
g95_se se;
tree leng;    
    
  if (sym->attr.use_assoc) return NULL_TREE; 
 
  if (sym->attr.data) return g95_generate_data(sym);          
          
  if (sym->value == NULL) return NULL_TREE;     
     
  g95_init_se(&se, NULL);         
         
  if (sym->as == NULL) {      
    g95_conv_constant(&se, sym->value);          
          
    if (STRING_P(se.expr)) {        
      leng = sym->ts.cl->backend_decl;          
      se.expr = g95_resize_string_constant(se.expr, leng);       
    } 
 
  } else { 
    g95_symbol_vinfo(sym, &vinfo);     
    se.expr = g95_conv_array_initializer(&vinfo);   
  }

  return se.expr;   
}      
      
      
       
       
/* g95_sym_identifier()-- Convert a name of a g95_symbol to an
 * identifier name. */        
        
tree g95_sym_identifier(g95_symbol *s, char *suffix) {
char mangled_name[3*G95_MAX_SYMBOL_LEN+4];       
       
  if (!g95_module_symbol(s) || strcmp(s->module, "(global)") == 0)  
    sprintf(mangled_name, "%s_", s->name);        
  else       
    sprintf(mangled_name, "%s.%s_", s->module, s->name); 
 
  if (suffix != NULL) {    
    strcat(mangled_name, ".");
    strcat(mangled_name, suffix);         
  } 
 
  return get_identifier(mangled_name);   
} 
 
 
      
      
/* g95_finish_decl()-- Finish processing of a declaration and install
 * its initial value. */

static void g95_finish_decl(tree dec, tree initial) {     
     
  if (TREE_CODE(dec) == PARM_DECL) 
    assert(initial == NULL_TREE);        
        
  /* Remember that PARM_DECL doesn't have a DECL_INITIAL field per se
     -- it overlaps DECL_ARG_TYPE.  */      
      
  else if (initial == NULL_TREE)   
    assert(DECL_INITIAL (dec) == NULL_TREE);   
  else      
    assert(DECL_INITIAL (dec) == error_mark_node);       
       
  if (initial != NULL_TREE) {      
    if (TREE_CODE(dec) != TYPE_DECL)
      DECL_INITIAL(dec) = initial;        
    else {       
      /* typedef foo = bar; store the type of bar as the type of foo.  */    
      TREE_TYPE(dec) = TREE_TYPE(initial);   
      DECL_INITIAL(dec) = initial = 0; 
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
 
 
       
       
/* allocate_array_return()-- Generate code to initialize an array
 * return.  The calling convention for allocate_pointer_array() is
 * described in init_section_info(). */   
   
static tree allocate_array_return(g95_symbol *rslt) {    
g95_array_spec *spec; 
stmtblock_t block;        
tree t, dtype;
int rank, p, k; 
g95_se se1;    
    
  g95_init_se(&se1, NULL);       
      
  k = 0;   
  spec = rslt->as;       
  rank = spec->rank;    
    
  t = build_int_2(rank, 0);
  g95_set_section_info(&se1, k++, t);      
      
  t = size_in_bytes(g95_typenode_for_spec(&rslt->ts));    
  g95_set_section_info(&se1, k++, t); 
 
  for(p=0; p<rank; p++) {      
    g95_conv_expr(&se1, spec->lower[p]);
    g95_set_section_info(&se1, k++, se1.expr); 
 
    g95_conv_expr(&se1, spec->upper[p]);   
    g95_set_section_info(&se1, k++, se1.expr);         
  }         
         
  dtype = build_pointer_type(TREE_TYPE(g95_result_var_decl));          
  t = build1(ADDR_EXPR, dtype, g95_result_var_decl);

  t = g95_call_library(void_type_node, PREFIX "allocate_pointer_array",          
			 t, null_pointer_node, NULL_TREE);   
   
  g95_init_block(&block);         
         
  g95_add_block_to_block(&block, &se1.pre);       
  g95_add_expr_to_block(&block, t); 
  g95_add_block_to_block(&block, &se1.post);    
    
  return g95_finish_block(&block);  
}       
       
       
    
    
/* finish_var_decl()-- Apply symbol attributes to a variable, and add
 * it to the function scope.  */ 
 
static void finish_var_decl(tree d, g95_symbol *s) {       
       
  /* TREE_ADDRESSABLE means the address of this variable is needed for
   * a TARGET variable.  We also need to set this if the variable is
   * passed by reference in a CALL statement. */     
     
  if (s->attr.target) TREE_ADDRESSABLE(d) = 1;      
      
  /* If it wasn't used we wouldn't be getting it.  */       
  TREE_USED(d) = 1;        
        
  /* If a variable is USE associated, it's always external.  */

  if (s->attr.use_assoc) {        
    DECL_EXTERNAL(d) = 1;          
    TREE_PUBLIC(d) = 1;        
  } else if (g95_module_symbol(s)) {       
       
    /* This is the declaration of a module variable.  */  
    TREE_PUBLIC(d) = 1;          
    TREE_STATIC(d) = 1;   
  }         
         
  if ((s->attr.save || s->attr.data || s->value)       
      && !s->attr.use_assoc) TREE_STATIC(d) = 1;          
          
  DECL_CONTEXT(d) = s->ns->backend_decl;         
}


        
        
/* find_dependent_var()-- Given a symbol that we are about to create,
 * traverse any array specification or character length, and find
 * other variables that need to be created before this one.  Mutually
 * recursive with create_procedure_variable().  If the stack depth
 * exceeds the number of symbols in the namespace, then we have found
 * variables that depend on one other, which shouldn't happen.  The
 * symbol is guaranteed to be a variable.  This recursive solution
 * avoids a nasty topological sort. */    
    
static void find_dependent_vars(g95_symbol *sy) {
g95_array_spec *ar;     
int e;        
        
  if (sy->ts.type == BT_CHARACTER && sy->ts.cl != NULL && 
      sy->ts.cl->length != NULL)    
    traverse_spec_expr(sy->ts.cl->length);

  ar = sy->as;      
  if (ar == NULL) return;     
     
  for(e=0; e<ar->rank; e++) {   
    traverse_spec_expr(ar->lower[e]);        
    traverse_spec_expr(ar->upper[e]);         
  }       
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
}     
     
     
        
        
/* get_variable_decl()-- Create the declaration for a variable */         
         
static void get_variable_decl(g95_symbol *s) {         
tree d, t, initial;   
variable_info info;       
       
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
      
  g95_symbol_vinfo(s, &info);
  t = g95_get_descriptor(&info);       
       
  d = build_decl(VAR_DECL, g95_sym_identifier(s, NULL), t);  
  
  pushdecl(d);  
  finish_var_decl(d, s); 
 
  s->backend_decl = d;    
    
  if (!G95_DESCRIPTOR_P(t))     
    DECL_INITIAL(s->backend_decl) = g95_initial_value(s);         
  else if (!s->attr.use_assoc) {    
    info.desc = d;   
    t = g95_get_storage(&info);     
    if (t != NULL) { 
      initial = g95_initial_value(s);       
       
      if (g95_context != NULL && initial == NULL &&       
	  !g95_stack_variable(t)) /* Var on heap */         
	t = NULL_TREE;   
      else { /* Variable not on heap */    
	t = build_decl(VAR_DECL, g95_sym_identifier(s, "data"), t);      
      
	DECL_INITIAL(t) = initial;         
	pushdecl(t);    
	finish_var_decl(t, s);          
      }  
    }        
        
    g95_init_descriptor(&info, d, t); 
  }
}  
  
  
     
     
/* init_result_var_decl()-- Create the variable used to hold the
 * function return value.  This is whether the result variable is
 * explicit or not. */

static void init_result_var_decl(g95_symbol *sym) {          
tree t, dec, l, identifier;         
g95_symbol *result;          
g95_se se;        
        
  find_dependent_vars(sym);          
         
  g95_result_var_decl = NULL_TREE;      
  l = NULL_TREE;     
  result = sym->result;        
        
  if (sym->attr.subroutine) {          
    dec = build_decl(VAR_DECL, NULL, g95_default_integer);      
    goto finish_decl;         
  }          
          
  if (sym->attr.pointer || sym->as != NULL) goto pointer;       
       
  identifier = (sym->result == sym)  
    ? g95_sym_identifier(sym, "result")         
    : g95_sym_identifier(sym->result, NULL);          
          
  switch(sym->ts.type) {      
  case BT_CHARACTER:   
    l = DECL_ARGUMENTS(sym->backend_decl); 
    l = build1(INDIRECT_REF, g95_default_integer, l);      
      
    dec = build_decl(VAR_DECL, identifier, pchar_type_node);         
         
    if (sym->ts.cl->length != NULL) {          
      g95_init_se(&se, NULL);          
      g95_conv_expr(&se, result->ts.cl->length);         
         
      g95_add_block_to_block(&g95_context->pre, &se.pre);       
      g95_add_modify_expr(&g95_context->pre, l, se.expr);      
      g95_add_block_to_block(&g95_context->pre, &se.post);          
    }

    g95_call_temp_alloc(&g95_context->pre, dec, l);       
    goto finish_decl;       
       
  case BT_COMPLEX:        
  case BT_DERIVED:        
    dec = DECL_ARGUMENTS(sym->backend_decl);  
    break;        
        
  default:       
  pointer:       
    t = g95_result_type(sym); 
    dec = build_decl(VAR_DECL, identifier, t);      
      
  finish_decl:    
    TREE_ADDRESSABLE(dec) = 1;
    DECL_ARTIFICIAL(dec) = 1;
    DECL_EXTERNAL(dec) = 0;   
    TREE_PUBLIC(dec) = 0;     
     
    layout_decl(dec, 0);       
       
    pushdecl(dec);   
    rest_of_decl_compilation(dec, NULL, 1, 0);   
    TREE_USED(dec) = 1;        
        
    g95_result_var_decl = dec;  
    break;          
  }      
      
  if (result != NULL && result != sym)  
    result->backend_decl = dec; 
 
  if (l != NULL_TREE)     
    result->ts.cl->backend_decl = l;    
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
        
  g95_build_intrinsic_function_decls();
  g95_build_io_library_fndecls();    
  g95_init_array_types();    
}        
        
        
  
  
/* specification_variable()-- Given a variable that is part of a
 * specification expression, create it if it needs to be created. */        
        
static void specification_variable(g95_expr *q) {    
g95_array_ref *spec;       
g95_symbol *symb;
g95_ref *ref;        
int g;   
   
  symb = q->symbol; 
  if (symb->ns != g95_current_ns) return;       
       
  if (symb->backend_decl == NULL || (symb->attr.dummy && !symb->mark)) {  
    symbol_count--; 
    if (symbol_count == 0)     
      g95_internal_error("traverse_spec_expr(): Circular specification");        
        
    create_procedure_variable(symb);  
    symbol_count++;
  }          
          
  /* Traverse variable references */    
    
  for(ref=q->ref; ref; ref=ref->next)      
    switch(ref->type) {      
    case REF_COMPONENT: 
      break;         
         
    case REF_SUBSTRING:    
      traverse_spec_expr(ref->u.ss.start);
      traverse_spec_expr(ref->u.ss.end);          
      break;         
         
    case REF_ARRAY:          
      spec = &ref->u.ar;         
         
      for(g=0; g<spec->dimen; g++) {
	traverse_spec_expr(spec->start[g]);
	traverse_spec_expr(spec->end[g]);   
	traverse_spec_expr(spec->stride[g]);   
      }	        
    }     
}        
        
        
         
         
/* g95_library_decl()-- Builds a function decl.  The remaining
 * parameters are the types of the function arguments.  Negative nargs
 * indicates a varargs function.  */       
       
tree g95_library_decl VPARAMS((char *name, tree rettype, int nargs, ...)) {   
tree args, argtype, fntype, decl;     
int x;          
          
  /* Library functions must be declared with global scope.  */        
  assert(current_function_decl == NULL_TREE);          
          
  VA_OPEN(l, nargs);     
  VA_FIXEDARG(l, tree, get_identifier(name));         
  VA_FIXEDARG(l, tree, retval);    
  VA_FIXEDARG(l, int, nargs);     
     
  /* Create a list of the argument types */   
  for(args=NULL_TREE, x=abs(nargs); x>0; x--) {    
    argtype = va_arg(l, tree); 
    args = g95_chainon_list(args, argtype);       
  }    
    
  /* Terminate the list */        
        
  if (nargs >= 0) args = g95_chainon_list(args, void_type_node);     
     
  /* Build the function type and decl */     
     
  fntype = build_function_type(rettype, args); 
  decl = build_decl(FUNCTION_DECL, get_identifier(name), fntype);    
    
  /* Mark this decl as external */   
   
  DECL_EXTERNAL(decl) = 1;        
  TREE_PUBLIC(decl) = 1;      
  VA_CLOSE(l);    
    
  pushdecl(decl);         
         
  rest_of_decl_compilation(decl, NULL, 1, 0);          
  return decl; 
}       
       
       
       
  
/* Generate entry and exit code, and add it to the function body. */  
  
static tree generate_entry_exit(g95_symbol *proc, tree fnbody) {  
g95_symbol *result;        
stmtblock_t block;      
tree tmp0;  
  
  g95_init_block(&block);       
       
  tmp0 = g95_call_library(void_type_node, PREFIX "push_context", NULL_TREE);   
  g95_add_expr_to_block(&block, tmp0);      
      
  result = proc->result;     
  if (proc->attr.function && !result->attr.pointer && result->as != NULL) {          
    tmp0 = allocate_array_return(result);        
    g95_add_expr_to_block(&block, tmp0);     
  }       
       
  g95_add_expr_to_block(&block, fnbody);        
        
  tmp0 = g95_call_library(void_type_node, PREFIX "pop_context", NULL_TREE);       
  g95_add_expr_to_block(&block, tmp0); 
 
  return g95_finish_block(&block);     
} 
 
 
     
     
/* g95_get_label_decl()-- Return the backend label declaration for a
 * given label structure, or create it if it doesn't exist yet.  */ 
 
tree g95_get_label_decl(g95_st_label *lp) {
char nam[20];     
tree declr;     
     
  if (lp->backend_decl) return lp->backend_decl;   
   
  sprintf(nam, "__label_%.6d", lp->value);
  declr = g95_build_label_decl(get_identifier(nam));          
          
  if (lp->value <= MAX_LABEL_VALUE) {       
    DECL_SOURCE_LINE(declr) = lp->where.line;     
    DECL_SOURCE_FILE(declr) = lp->where.file->filename;      
  } else      
    DECL_ARTIFICIAL(declr) = 1;    
    
  lp->backend_decl = declr;        
  return declr;          
}    
    
    
        
        
/* count_syms()-- Recursive function for counting the number of
 * symbols within a namespace.  This count is used to detect infinite
 * recursion during procedure variable creation. */ 
 
static int count_syms(g95_symtree *st1) {    
    
  if (st1 == NULL) return 0;
  return 1 + count_syms(st1->left) + count_syms(st1->right);      
}       
       
       
       
       
/* g95_generate_procedure_variables()-- Generate variables. */

void g95_generate_procedure_variables(g95_namespace *n) { 
 
  symbol_count = count_syms(n->sym_root) + 10;       
       
  g95_current_ns = n;     
  g95_traverse_ns(n, create_procedure_variable); 
}    
    
    


/* g95_build_procedure_decl()-- Create a declaration for a procedure.
 * For external functions (in the C sense) use g95_get_extern_function_decl. */          
          
void g95_build_procedure_decl(g95_symbol *sym) { 
tree proc_decl, type, result_decl, typelist, arglist, arglist_tail,
     parm, result_pointer, result_length, tmp;        
symbol_attribute attribute;        
g95_formal_arglist *s;   
g95_symbol *formal; 
 
  if (sym->backend_decl) return;        
  assert(!sym->attr.external);   
   
  attribute = sym->attr;         
         
  /* Allow only one nesting level.  Allow external declarations.  */ 
 
  assert(current_function_decl == NULL_TREE  
	 || DECL_CONTEXT(current_function_decl) == NULL_TREE);       
       
  type = g95_procedure_type(sym);    
  proc_decl = build_decl(FUNCTION_DECL, g95_sym_identifier(sym, NULL), type); 
 
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
   
  if (POINTER_TYPE_P(type)) DECL_IS_MALLOC(proc_decl) = 1;      
      
  /* Set up all attributes for the function. */     
     
  DECL_EXTERNAL(proc_decl) = 0;       
  DECL_CONTEXT(proc_decl) = sym->ns->backend_decl;       
       
  /* This specifies if a function is globally addressable, ie. it is
   * the opposite of declaring static in C.  */          
          
  if (current_function_decl == NULL_TREE) TREE_PUBLIC(proc_decl) = 1;

  /* TREE_STATIC means the function body is defined here.  */     
  TREE_STATIC(proc_decl) = 1;         
         
  /* Set attributes for PURE functions. A call to PURE function in the
   * Fortran 95 sense is both pure and without side effects in the C
   * sense */         
         
  if (attribute.pure || attribute.elemental) { 
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
	tmp = g95_sym_identifier(sym, "result_len");     
     
	type = build_pointer_type(g95_default_integer);  
	result_length = build_decl(PARM_DECL, tmp, type);  
  
	DECL_CONTEXT(result_length) = proc_decl;
	DECL_ARG_TYPE(result_length) = type;         
         
	g95_finish_decl(result_length, NULL_TREE);          
          
	arglist = chainon(arglist, result_length);  
	typelist = TREE_CHAIN(typelist);    
      } 
 
      break;

    case BT_COMPLEX:    
    case BT_DERIVED:   
      type = TREE_VALUE(typelist); 
 
      tmp = g95_sym_identifier(sym, "result");     
      result_pointer = build_decl(PARM_DECL, tmp, type);      
      
      DECL_CONTEXT(result_pointer) = proc_decl;        
      DECL_ARG_TYPE(result_pointer) = type;   
      TREE_READONLY(result_pointer) = 1;         
         
      g95_finish_decl(result_pointer, NULL_TREE);          
          
      arglist = chainon(arglist, result_pointer);      
      typelist = TREE_CHAIN(typelist);    
    
    
    default: 
      break; 
    }      
  }    
    
  for(s=sym->formal; s; s=s->next) { 
    formal = s->sym;        
    if (formal == NULL) continue;   /* ignore alt return placeholders. */ 
    formal->mark = 0;      
      
    if (formal->backend_decl == NULL) g95_get_symbol_decl(formal);    
    parm = formal->backend_decl;      
    DECL_CONTEXT(parm) = proc_decl; 
 
    g95_finish_decl(parm, NULL_TREE);      
      
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
	formal->ts.cl->backend_decl = parm;   
    }
  }  
  
  arglist = chainon(arglist, arglist_tail);  
  DECL_ARGUMENTS(proc_decl) = arglist;    
    
  /* Restore the old context.  */          
  current_function_decl = DECL_CONTEXT(proc_decl);

  sym->backend_decl = proc_decl;         
}       
       
       
    
    
/* create_procedure_variable()-- Create a variable */     
     
static void create_procedure_variable(g95_symbol *sy) {       
       
  if (sy->ns != g95_current_ns) return; 
 
  if (sy->attr.dummy && !sy->mark) {        
    sy->mark = 1;  
    find_dependent_vars(sy);
    if (sy->attr.dimension) g95_fix_dummy_array(sy);         
    return;       
  }     
     
  if (sy->backend_decl != NULL) return;    
    
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
          
          
         
         
/* g95_generate_procedure()-- Generate code for a procedure.  */          
          
void g95_generate_procedure(g95_namespace *namesp) {  
tree f, tmp0, saved_function_decl;       
g95_trans_context context;       
stmtblock_t list, blk;          
g95_namespace *child;      
g95_symbol *sym;      
      
  sym = namesp->proc_name;   
  namesp->backend_decl = sym->backend_decl; 
 
  /* Create the declaration for functions with global scope */       
       
  context.parent = g95_context;   
  g95_context = &context;     
     
  context.saved_current_function = current_function_decl;  
  context.current_procedure = namesp;       
  context.result_var_decl = g95_result_var_decl;  
  
  g95_init_block(&context.pre);    
  g95_init_block(&context.post);       
       
  if (context.parent != NULL) push_function_context(); 
 
  /* let GCC know the current scope is this function */      
      
  current_function_decl = f = sym->backend_decl;    
  context.current_function_decl = f;         
         
  /* print function name on the console at compile time unless this
   * feature was switched of by command line option "-quiet" */ 
 
  announce_function(f);

  /* create RTL for function declaration */  
  
  if (DECL_CONTEXT(f) == NULL_TREE)   
    rest_of_decl_compilation(f, NULL, 1, 0);     
     
  /* create RTL for function definition */   
  make_decl_rtl(f, NULL);    
    
  /* Set the line and filename.  sym->declared_at seems to point to the last
   * statement for subroutines, but it'll do for now. */         
         
  g95_set_backend_locus(&sym->declared_at);

  /* line and file should not be 0 */     
  init_function_start(f, input_filename, lineno);       
       
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
  for(child=namesp->contained; child; child=child->sibling) {       
    if (child->parent != namesp) continue;          
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
    
  g95_trans_common(namesp);   
  g95_generate_procedure_variables(namesp);          
          
  tmp0 = g95_trans_code(namesp->code);   
   
  g95_add_block_to_block(&blk, &context.pre);     
  g95_add_expr_to_block(&blk, tmp0);

  /* Add a return label if needed. */   
   
  if (current_function_return_label) {        
    tmp0 = build_v(LABEL_EXPR, current_function_return_label);    
    g95_add_expr_to_block(&blk, tmp0);        
  }    
    
  tmp0 = g95_finish_block(&blk);     
     
  /* Add code to create and cleanup arrays */     
  tmp0 = generate_entry_exit(sym, tmp0);     
  g95_add_expr_to_block(&list, tmp0);        
        
  g95_add_block_to_block(&list, &context.post);

  /* Generate code for contained procedures. */        
        
  for(child=namesp->contained; child; child=child->sibling) {          
    if (child->parent != namesp) continue;     
    g95_generate_procedure(child);      
  }          
          
  /* Restore the symbol declaration to the function version if we had
   * to replace it with the result variable. */    
    
  if (saved_function_decl != NULL_TREE)    
    sym->backend_decl = saved_function_decl;    
    
  if (g95_result_var_decl != NULL_TREE) {          
    tmp0 = build(MODIFY_EXPR, TREE_TYPE(g95_result_var_decl),      
		DECL_RESULT(f), g95_result_var_decl);         
    tmp0 = build_v(RETURN_EXPR, tmp0);        
    g95_add_expr_to_block(&list, tmp0); 
  }      
      
  tmp0 = g95_finish_block(&list);         
  DECL_SAVED_TREE(f) = tmp0;        
        
  /* Finish off this function and send it for code generation. */

  poplevel(1, 0, 1);    
  BLOCK_SUPERCONTEXT(DECL_INITIAL(f)) = f;       
       
  {   /* Output the SIMPLE tree. */        
    FILE *dump_file;          
    int dump_flags = 0;
    tree fnbody;    
    
    fnbody = DECL_SAVED_TREE(f); 
 
    dump_file = dump_begin(TDI_original, &dump_flags);  
    if (dump_file) {          
      fprintf(dump_file, "%s()\n", namesp->proc_name->name);          
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
    expand_function_body(f, 0);         
  else         
    pop_function_context();       
       
  g95_context = context.parent;     
  current_function_decl = context.saved_current_function;     
  g95_result_var_decl = context.result_var_decl;    
}  
  
      
      
/* traverse_spec_expr()-- Traverse a specification expression, looking
 * for as yet undefined variables. */   
   
static void traverse_spec_expr(g95_expr *l) {        
g95_actual_arglist *ap;      
      
  if (l == NULL) return;     
     
  switch(l->type) {          
  case EXPR_OP:     
    traverse_spec_expr(l->op1);     
    traverse_spec_expr(l->op2); 
    break;        
        
  case EXPR_FUNCTION:  
    for(ap=l->value.function.actual; ap; ap=ap->next)   
      traverse_spec_expr(ap->u.expr);      
    break;   
   
  case EXPR_VARIABLE: 
    specification_variable(l);        
    break; 
 
  default:    
    break;      
  } 
}


       
       
/* g95_get_extern_function_decl()-- Get a basic decl for an external
 * function. */

tree g95_get_extern_function_decl(g95_symbol *symbol) {      
g95_expr d, argexpr[G95_MAX_SPECIFIC_ARGS];
char k[G95_MAX_SYMBOL_LEN+15];
g95_intrinsic_arg *formal;      
tree nam, dtype, f;       
g95_intrinsic_sym *is;          
int o;

  if (symbol->backend_decl) return symbol->backend_decl;    
    
  if (!symbol->attr.intrinsic)         
    nam = g95_sym_identifier(symbol, NULL);  
  else {       
    /* Call the resolution function to get the actual name.  */  
    is = g95_find_function(symbol->name);        
    assert (is->resolve);          
          
    memset (&d, 0, sizeof(d));  
    memset (argexpr, 0, sizeof(argexpr));         
    d.type = EXPR_FUNCTION;  
    formal = NULL;      
    o = 0;   
   
    for (formal=is->formal, o=0; formal; formal=formal->next, o++) {      
      assert (o < G95_MAX_SPECIFIC_ARGS);          
      argexpr[o].ts = formal->ts;      
    }         
         
    switch (o) {    
    case 0:  
      is->resolve(&d);      
      break;          
          
    case 1:         
      is->resolve(&d, &argexpr[0]);    
      break;       
       
    case 2:       
      is->resolve(&d, &argexpr[0], &argexpr[1]);        
      break;         
         
    default:         
      abort();     
    }   
   
    sprintf(k, "specific%s", d.value.function.name);       
    nam = get_identifier(k);    
  }        
        
  dtype = g95_procedure_type(symbol);    
  f = build_decl(FUNCTION_DECL, nam, dtype);       
       
  /* If the return type is a pointer, avoid alias issues by setting
   * DECL_IS_MALLOC to nonzero. This means that the function should be
   * treated as if it were a malloc, meaning it returns a pointer that
   * is not an alias.  */  
  
  if (POINTER_TYPE_P(dtype)) DECL_IS_MALLOC(f) = 1;

  /* Set up all attributes for the function.  */ 
 
  DECL_CONTEXT(f) = current_function_decl;    
  DECL_EXTERNAL(f) = 1;          
          
  /* This specifies if a function is globally addressable, ie. it is
   * the opposite of declaring static in C. */         
         
  TREE_PUBLIC(f) = 1;          
          
  /* Set attributes for PURE functions.  A call to PURE function in the
   * Fortran 95 sense is both pure and without side effects in the C
   * sense. */

  if (symbol->attr.pure || symbol->attr.elemental) {    
    DECL_IS_PURE(f) = 1;        
    TREE_SIDE_EFFECTS(f) = 0;
  }   
   
  symbol->backend_decl = f;      
  return f;          
}          
          
          
   
   
/* g95_get_symbol_decl()-- Create the declaration for a symbol. */  
  
void g95_get_symbol_decl(g95_symbol *s) {    
    
  if (s->attr.dummy)    
    get_parm_decl(s);        
  else  
    get_variable_decl(s);       
}    
    
    


#include "gt-f95-trans-decl.h"
