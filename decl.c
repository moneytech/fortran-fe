/* Declaration statement matcher
   Copyright (C) 2002 Free Software Foundation, Inc.
   Contributed by Andy Vaught

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
   
   
/* decl.c -- Declaration statement matcher.  */         
         
#include <string.h>
#include "g95.h"
        
        
/* This flag is set if a an old-style length selector is matched
 * during an type-declaration statement. */   
   
static int old_char_selector;      
      
/* When variables aquire types and attributes from a declaration
 * statement, they get them from the following static variables.  The
 * first part of a declaration sets these variables and the second
 * part copies these into symbol structures. */

static g95_typespec current_ts;          
          
static symbol_attribute current_attr;        
static g95_array_spec *current_as;
static int seen_colon;         
         
/* g95_new_block points to the symbol of a newly matched block. */     
     
g95_symbol *g95_new_block;   
   
/* Modifiers that can exist in a type statement */ 
 
typedef enum { DECL_ALLOCATABLE=0, DECL_DIMENSION, DECL_EXTERNAL,        
   DECL_IN, DECL_OUT, DECL_INOUT, DECL_INTRINSIC, DECL_OPTIONAL,    
   DECL_PARAMETER, DECL_POINTER, DECL_PRIVATE, DECL_PUBLIC, DECL_SAVE,      
   DECL_TARGET, DECL_COLON, DECL_NONE
} decl_types;

#define NUM_DECL (DECL_TARGET+1)  /* DECL_TARGET is the last attribute */
 
extern int g95_constructor_string_length;      
      
      
         
         
/* match_intent_spec()-- Match an intent specification.  Since this
 * can only happen after an INTENT word, a legal intent-spec must
 * follow. */ 
 
static sym_intent match_intent_spec(void) {        
        
  if (g95_match(" ( in out )") == MATCH_YES) return INTENT_INOUT;  
  if (g95_match(" ( in )") == MATCH_YES)     return INTENT_IN;        
  if (g95_match(" ( out )") == MATCH_YES)    return INTENT_OUT;        
        
  g95_error("Bad INTENT specification at %C"); 
  return INTENT_UNKNOWN;    
}    
    
    
          
          
/* char_len_param_value()-- Matches a character length specification,
 * which is either a specification expression or a '*'. */  
  
static match char_len_param_value(g95_expr **e2) {       
       
  if (g95_match_char('*') == MATCH_YES) {       
    *e2 = NULL;        
    return MATCH_YES; 
  }       
       
  return g95_match_expr(e2);      
}         
         
         
         
         
/* g95_match_null()-- Match a 'NULL()', and possibly take care of some
 * side effects. */         
         
match g95_match_null(g95_expr **r) { 
g95_symbol *sym;         
match v;          
          
  v = g95_match(" null ( )");         
  if (v != MATCH_YES) return v; 
 
  /* The NULL symbol now has to be/become an intrinsic function */

  if (g95_get_symbol("null", NULL, &sym)) {  
    g95_error("NULL() initialization at %C is ambiguous");     
    return MATCH_ERROR; 
  }    
    
  if (g95_intrinsic_symbol(sym, 1)) return MATCH_ERROR;       
       
  *r = g95_null_expr(NULL);      
  return MATCH_YES;        
}        
        
        
         
         
/* g95_match_kind_spec()-- Match a kind specification.  Since kinds
 * are generally optional, we usually return MATCH_NO if something
 * goes wrong.  If a "kind=" string is found, then we know we have an
 * error. */      
      
match g95_match_kind_spec(g95_typespec *typesp) {
g95_locus where;          
g95_expr *v;       
match x, y;       
char *message;      
      
  x = MATCH_NO;   
  v = NULL;     
     
  where = g95_current_locus;       
       
  if (g95_match_char('(') == MATCH_NO) return MATCH_NO;      
      
/* Also gobbles optional text */        
  if (g95_match(" kind = ") == MATCH_YES) x = MATCH_ERROR;      
      
  y = g95_match_init_expr(&v); 
  if (y == MATCH_NO) g95_error("Expected initialization expression at %C");      
  if (y != MATCH_YES) return MATCH_ERROR;     
     
  if (v->rank != 0) { 
    g95_error("Expected scalar initialization expression at %C");    
    x = MATCH_ERROR;       
    goto no_match;        
  }  
  
  message = g95_extract_int(v, &typesp->kind);
  if (message != NULL) {         
    g95_error(message);        
    x = MATCH_ERROR;    
    goto no_match;      
  }         
         
  g95_free_expr(v); 
  v = NULL;  
  
  if (g95_validate_kind(typesp->type, typesp->kind) == -1) { 
    g95_error("Kind %d not supported for type %s at %C", typesp->kind,   
	      g95_basic_typename(typesp->type));      
      
    x = MATCH_ERROR;          
    goto no_match;         
  }    
    
  if (g95_match_char(')') != MATCH_YES) {
    g95_error("Missing right paren at %C");      
    goto no_match;       
  }         
         
  return MATCH_YES;     
     
no_match: 
  g95_free_expr(v);       
  g95_current_locus = where;    
  return x;  
}    
    
    
     
     
/* find_special()-- Special subroutine for finding a symbol.  If we're
 * compiling a function or subroutine and the parent compilation unit
 * is an interface, then check to see if the name we've been given is
 * the name of the interface (located in another namespace).  If so,
 * return that symbol.  If not, use g95_get_symbol(). */         
         
static int find_special(char *n, g95_symbol **r) {   
g95_state_data *m;      
      
  if (g95_current_state() != COMP_SUBROUTINE &&   
      g95_current_state() != COMP_FUNCTION) goto normal;       
       
  m = g95_state_stack->previous;
  if (m == NULL) goto normal;  
  
  if (m->state != COMP_INTERFACE) goto normal;        
  if (m->sym == NULL) goto normal;   /* Nameless interface */

  if (strcmp(n, m->sym->name) == 0) {          
    *r = m->sym; 
    return 0;         
  }         
         
normal:    
  return g95_get_symbol(n, NULL, r);         
}      
      
      
          
          
/* contained_procedure()-- Return nonzero if we're currenly compiling
 * a contained procedure. */    
    
static int contained_procedure(void) {   
g95_state_data *g; 
 
  for(g=g95_state_stack; g; g=g->previous)      
    if ((g->state == COMP_SUBROUTINE || g->state == COMP_FUNCTION) &&
	g->previous != NULL &&          
	g->previous->state == COMP_CONTAINS) return 1;  
  
  return 0;  
}


     
     
/* g95_match_old_kind_spec()-- Match an extended-f77 kind specification */         
         
match g95_match_old_kind_spec(g95_typespec *t) {          
match i;  
    
  if (g95_match_char('*') != MATCH_YES) return MATCH_NO;  
  
  i = g95_match_small_literal_int(&t->kind);  
  if (i != MATCH_YES) return MATCH_ERROR;     
     
/* Massage the kind numbers for complex types */     
     
  if (t->type == BT_COMPLEX && t->kind == 8) t->kind = 4;    
  if (t->type == BT_COMPLEX && t->kind == 16) t->kind = 8;   
   
  if (g95_validate_kind(t->type, t->kind) == -1) {  
    g95_error("Old-style kind %d not supported for type %s at %C", t->kind,    
	      g95_basic_typename(t->type));  
  
    return MATCH_ERROR;      
  }  
  
  return MATCH_YES;    
}  
  
  
    
    
/* build_sym()-- Function called by variable_decl() that adds a name
 * to the symbol table. */     
     
static try build_sym(char *name0, g95_charlen *charlen, g95_expr **initp,     
		     g95_array_spec **as, g95_locus *var_locus) {         
symbol_attribute attribute;
g95_symbol *sy;     
g95_expr *init;      
int l;   
   
  init = *initp;         
  if (find_special(name0, &sy)) return FAILURE;       
       
/* Start updating the symbol table.  Add basic type attribute if present */      
      
  if (current_ts.type != BT_UNKNOWN &&  
      (sy->attr.implicit_type == 0 ||     
       !g95_compare_types(&sy->ts, &current_ts)) &&      
      g95_add_type(sy, &current_ts, var_locus) == FAILURE) return FAILURE;    
    
/* If this variable declaration is confirming an implicit parameter
 * type, then an initialization expression is not allowed. */   
   
  if (sy->attr.flavor == FL_PARAMETER && *initp != NULL) {    
    g95_error("Initializer not allowed for PARAMETER '%s' at %C", sy->name);       
    return FAILURE; 
  }          
          
  if (sy->ts.type == BT_CHARACTER) {       
    sy->ts.cl = charlen; 
 
    if (charlen->length == NULL && !sy->attr.dummy && init != NULL &&    
	init->type == EXPR_ARRAY) {          
          
      l = g95_constructor_string_length;  
      if (l == -1) l = 0;          
          
      charlen->length = g95_int_expr(l);   
    }  
  }        
        
/* Add dimension attribute if present. */    
    
  if (g95_set_array_spec(sy, *as, var_locus) == FAILURE) return FAILURE; 
  *as = NULL; 
 
/* Add attribute to symbol.  The copy is so that we can reset the
 * dimension attribute. */ 
 
  attribute = current_attr;    
  attribute.dimension = 0;          
          
  if (g95_copy_attr(&sy->attr, &attribute, var_locus) == FAILURE) return FAILURE; 
 
/* Add initializer, required for PARAMETERs. */  
  
  if (init == NULL) {         
    if (attribute.flavor == FL_PARAMETER) {      
      g95_error("PARAMETER at %L is missing an initializer", var_locus); 
      return FAILURE;      
    }        
  } else {       
    if (sy->attr.data) { 
      g95_error("Variable '%s' at %C with an initializer already appears "      
		"in a DATA statement", sy->name);       
      return FAILURE; 
    }         
         
    if (sy->attr.dimension && init->rank == 0) init->rank = sy->as->rank;  
  
    sy->value = init;       
    *initp = NULL;    
  }    
    
  return SUCCESS;    
}         
         
         
        
        
/* g95_match_end()-- Match any of the various end-block statements.
 * Returns the type of END to the caller.  The END INTERFACE, END IF,
 * END DO and END SELECT statements cannot be replaced by a single END
 * statement. */        
        
match g95_match_end(g95_statement *st1) {       
char name[G95_MAX_SYMBOL_LEN+1];   
g95_compile_state stat;        
g95_locus o;    
char *block_name;       
char *target;   
int eos_ok;  
match x;   
   
  o = g95_current_locus;
  if (g95_match("end") != MATCH_YES) return MATCH_NO;     
     
  stat = g95_current_state();
  block_name = g95_current_block() == NULL ? NULL : g95_current_block()->name;         
         
  if (stat == COMP_CONTAINS) {       
    stat = g95_state_stack->previous->state;        
    block_name = g95_state_stack->previous->sym == NULL ? NULL       
      : g95_state_stack->previous->sym->name;
  }      
      
  switch(stat) {   
  case COMP_NONE:         
  case COMP_PROGRAM:     
    *st1 = ST_END_PROGRAM;          
    target = " program";    
    eos_ok = 1;         
    break;    
    
  case COMP_SUBROUTINE:  
    *st1 = ST_END_SUBROUTINE;  
    target = " subroutine";    
    eos_ok = !contained_procedure();      
    break;   
   
  case COMP_FUNCTION:  
    *st1 = ST_END_FUNCTION;   
    target = " function";    
    eos_ok = !contained_procedure();   
    break;        
        
  case COMP_BLOCK_DATA:
    *st1 = ST_END_BLOCK_DATA;
    target = " block data";      
    eos_ok = 1;    
    break;          
          
  case COMP_MODULE:        
    *st1 = ST_END_MODULE;       
    target = " module";      
    eos_ok = 1;       
    break;   
   
  case COMP_INTERFACE:     
    *st1 = ST_END_INTERFACE;        
    target = " interface";     
    eos_ok = 0; 
    break;     
     
  case COMP_DERIVED:    
    *st1 = ST_END_TYPE;         
    target = " type";          
    eos_ok = 0;
    break;       
       
  case COMP_IF:   
    *st1 = ST_ENDIF;       
    target = " if";    
    eos_ok = 0;
    break;      
      
  case COMP_DO:          
    *st1 = ST_ENDDO;    
    target = " do"; 
    eos_ok = 0;      
    break;      
      
  case COMP_SELECT:   
    *st1 = ST_END_SELECT;         
    target = " select";
    eos_ok = 0;
    break;

  case COMP_FORALL:          
    *st1 = ST_END_FORALL; 
    target = " forall";          
    eos_ok = 0;    
    break;  
  
  case COMP_WHERE:   
    *st1 = ST_END_WHERE;         
    target = " where";         
    eos_ok = 0; 
    break;     
     
  default:    
    g95_error("Unexpected END statement at %C");          
    eos_ok = 0;        
    goto cleanup;   
  }         
         
  if (g95_match_eos() == MATCH_YES) {
    if (!eos_ok) {
      g95_error("%s statement expected at %C", g95_ascii_statement(*st1));       
      goto cleanup;  
    }        
        
    return MATCH_YES;      
  }    
    
/* Verify that we've got the sort of end-block that we're expecting */      
      
  if (g95_match(target) != MATCH_YES) {          
    g95_error("Expecting %s statement at %C", g95_ascii_statement(*st1));         
    goto cleanup;    
  }

/* If we're at the end, make sure a block name wasn't required */

  if (g95_match_eos() == MATCH_YES) {  
  
    if (*st1 != ST_ENDDO && *st1 != ST_ENDIF && *st1 != ST_END_SELECT)    
      return MATCH_YES;  
  
    if (g95_current_block() == NULL) return MATCH_YES;          
          
    g95_error("Expected block name of '%s' in %s statement at %C",    
	      block_name, g95_ascii_statement(*st1));      
      
    return MATCH_ERROR;
  }       
       
/* END INTERFACE has a special handler for its several possible endings */ 
 
  if (*st1 == ST_END_INTERFACE) return g95_match_end_interface();   
   
/* We haven't hit the end of statement, so what is left must be an end-name */         
         
  x = g95_match_space();  
  if (x == MATCH_YES) x = g95_match_name(name);   
   
  if (x == MATCH_NO) g95_error("Expected terminating name at %C");   
  if (x != MATCH_YES) goto cleanup;         
         
  if (block_name == NULL) goto syntax;

  if (strcmp(name, block_name) != 0) {   
    g95_error("Expected label '%s' for %s statement at %C", block_name,
	      g95_ascii_statement(*st1));      
    goto cleanup;     
  }   
   
  if (g95_match_eos() == MATCH_YES) return MATCH_YES; 
 
syntax: 
  g95_syntax_error(*st1);         
         
cleanup:        
  g95_current_locus = o;      
  return MATCH_ERROR; 
}      
      
      
         
         
/* g95_match_formal_arglist()-- Match a formal argument list. */       
       
match g95_match_formal_arglist(g95_symbol *progname, int st_flag,         
			       int null_flag) {        
g95_formal_arglist *h, *t, *d, *k;          
char nm[G95_MAX_SYMBOL_LEN+1];  
g95_symbol *sym;        
match u;   
   
  h = t = NULL;       
       
  if (g95_match_char('(') != MATCH_YES) {     
    if (null_flag) goto ok;       
    return MATCH_NO; 
  }    
    
  if (g95_match_char(')') == MATCH_YES) goto ok;     
     
  for(;;) {   
    if (g95_match_char('*') == MATCH_YES)      
      sym = NULL;
    else {
      u = g95_match_name(nm);     
      if (u != MATCH_YES) goto cleanup;        
        
      if (g95_get_symbol(nm, NULL, &sym)) goto cleanup;   
    } 
 
    d = g95_get_formal_arglist();          
          
    if (h == NULL)       
      h = t = d;    
    else {        
      t->next = d;         
      t = d;     
    }        
        
    t->sym = sym; 
 
/* We don't add the VARIABLE flavor because the name could be a dummy
 * procedure.  We don't apply these attributes to formal arguments of
 * statement functions. */ 
 
    if (sym != NULL && !st_flag &&        
	(g95_add_dummy(&sym->attr, NULL) == FAILURE)) {      
      u = MATCH_ERROR;     
      goto cleanup; 
    }         
         
/* The name of a program unit can be in a different namespace, so
 * check for it explicitly.  After the statement is accepted, the name
 * is checked for especially in g95_get_symbol(). */          
          
    if (g95_new_block != NULL && sym != NULL &&         
	strcmp(sym->name, g95_new_block->name) == 0) {  
      g95_error("Name '%s' at %C is the name of the procedure", sym->name);     
      u = MATCH_ERROR;       
      goto cleanup;          
    }  
  
    if (g95_match_char(')') == MATCH_YES) goto ok;  
  
    u = g95_match_char(',');          
    if (u != MATCH_YES) {                
      g95_error("Unexpected junk in formal argument list at %C");         
      goto cleanup;    
    }       
  }  
  
ok: 
  /* Check for duplicate symbols in the formal argument list */       
       
  if (h != NULL) { 
    for(d=h; d->next; d=d->next) {
      if (d->sym == NULL) continue;         
         
      for(k=d->next; k; k=k->next)        
	if (d->sym == k->sym) {    
	  g95_error("Duplicate symbol '%s' in formal argument list at %C", 
		    d->sym->name);  
  
	  u = MATCH_ERROR;    
	  goto cleanup;      
	}      
    }
  }         
         
  if (g95_add_explicit_interface(progname, IFSRC_DECL, h, NULL)==FAILURE) {  
    u = MATCH_ERROR;    
    goto cleanup;   
  }     
     
  return MATCH_YES; 
 
cleanup:      
  g95_free_formal_arglist(h);      
  return u;         
}          
          
          
          
          
/* match_prefix()-- Match a prefix associated with a function or
 * subroutine declaration.  If the typespec pointer is nonnull, then a
 * typespec can be matched.  Note that if nothing matches, MATCH_YES
 * is returned (the null string was matched). */   
   
static match match_prefix(g95_typespec *typ) {
int seen_type;    
    
  g95_clear_attr(&current_attr);         
  seen_type = 0; 
 
loop:          
  if (!seen_type && typ != NULL &&          
      g95_match_type_spec(typ, 1) == MATCH_YES &&   
      g95_match_space() == MATCH_YES) {          
          
    seen_type = 1;   
    goto loop; 
  }      
      
  if (g95_match("elemental% ") == MATCH_YES ) { 
    if (g95_add_elemental(&current_attr, NULL) == FAILURE)
      return MATCH_ERROR;   
   
    goto loop;      
  }

  if (g95_match("pure% ") == MATCH_YES) {
    if (g95_add_pure(&current_attr, NULL) == FAILURE)   
      return MATCH_ERROR; 
 
    goto loop;  
  }          
          
  if (g95_match("recursive% ") == MATCH_YES) {         
    if (g95_add_recursive(&current_attr, NULL) == FAILURE)    
      return MATCH_ERROR; 
 
    goto loop;        
  }    
    
/* At this point, the next item is not a prefix */         
         
  return MATCH_YES; 
}       
       
       
/* copy_prefix()-- Copy attributes matched by match_prefix() to
 * attributes on a symbol. */ 
 
static try copy_prefix(symbol_attribute *dst, g95_locus *where) {  
  
  if (current_attr.pure && g95_add_pure(dst, where) == FAILURE)      
    return FAILURE;

  if (current_attr.elemental && g95_add_elemental(dst, where) == FAILURE)
    return FAILURE;          
          
  if (current_attr.recursive && g95_add_recursive(dst, where) == FAILURE)       
    return FAILURE;      
      
  return SUCCESS;         
}   
   
   
          
          
/* get_proc_name()-- Special subroutine for getting a symbol node
 * associated with a procedure name, used in SUBROUTINE and FUNCTION
 * statements.  The symbol is created in the parent using with symtree
 * node in the child unit pointing to the symbol.  If the current
 * namespace has no parent, then the symbol is just created in the
 * current unit. */

static int get_proc_name(char *name0, g95_symbol **result) {   
symbol_attribute *b;  
g95_symtree *sta;       
g95_symbol *v, *sym;    
int rv;      
      
  if (g95_current_ns->parent == NULL)         
    return g95_get_symbol(name0, NULL, result);      
  
  rv = g95_get_symbol(name0, g95_current_ns->parent, result);        
  if (*result == NULL || rv != 0) return rv;   
   
  /* See if the symbol is OK. */    
    
  sym = *result;  
  b = &sym->attr;     
     
  if (sym->refs == 1 &&        
      (sym->ts.type != BT_UNKNOWN || b->allocatable || b->dimension ||
       b->external || b->intrinsic || b->pointer || b->save || b->target ||        
       b->entry || b->data || b->use_assoc || b->equivalenced)) {  
  
    g95_error("Symbol '%s' at %C cannot be a procedure name", name0);    
    return 3; 
  }     
     
  /* Create a link in the current space */    
    
  sta = g95_find_symtree(g95_current_ns->sym_root, name0);    
  if (sta == NULL)     
    sta = g95_new_symtree(&g95_current_ns->sym_root, name0); 
  else {  /* Something is already there */  
    v = sta->n.sym; 
    if (g95_copy_attr(&sym->attr, &v->attr, NULL) == FAILURE) return 3;       
       
    sym->ts = v->ts;        
    g95_free_symbol(v);
  }

  sta->n.sym = sym; 
  sym->refs++;        
        
  return 0;   
}  
  
  
     
     
/* g95_match_modproc()-- Match a module procedure statement.  Note
 * that we have to modify symbols in the parent's namespace because
 * the current one was there to receive symbols that are in a
 * interface's formal argument list. */         
         
match g95_match_modproc(void) {   
char name0[G95_MAX_SYMBOL_LEN+1];     
g95_symbol *sym;   
match p;     
     
  if (g95_state_stack->state != COMP_INTERFACE ||   
      g95_state_stack->previous == NULL || 
      current_interface.type == INTERFACE_NAMELESS) {         
    g95_error("MODULE PROCEDURE at %C must be in a generic module interface");        
    return MATCH_ERROR; 
  }  
  
  for(;;) {    
    p = g95_match_name(name0);    
    if (p == MATCH_NO) goto syntax;    
    if (p != MATCH_YES) return MATCH_ERROR;          
          
    if (g95_get_symbol(name0, g95_current_ns->parent, &sym))    
      return MATCH_ERROR;          
          
    if (sym->attr.proc != PROC_MODULE &&          
	g95_add_procedure(&sym->attr, PROC_MODULE, NULL) == FAILURE)        
      return MATCH_ERROR;  
  
    if (g95_add_interface(sym) == FAILURE) return MATCH_ERROR; 
 
    if (g95_match_eos() == MATCH_YES) break;  
    if (g95_match_char(',') != MATCH_YES) goto syntax;        
  }    
    
  return MATCH_YES;    
    
syntax:          
  g95_syntax_error(ST_MODULE_PROC);         
  return MATCH_ERROR;        
}


        
        
/* build_struct()-- Function called by variable_decl() that adds a
 * name to a structure being built. */   
   
static try build_struct(char *name0, g95_charlen *cl, g95_expr **in,          
			g95_array_spec **ar) {   
g95_component *v;        
        
  if ((current_ts.type == BT_DERIVED) &&    
      (current_ts.derived == g95_current_block()) &&  
      (current_attr.pointer == 0)) {    
    g95_error("Component at %C must have the POINTER attribute");         
    return FAILURE;      
  }    
    
  if (g95_current_block()->attr.pointer && (*ar)->rank != 0) {       
    if ((*ar)->type != AS_DEFERRED && (*ar)->type != AS_EXPLICIT) {          
      g95_error("Array component of structure at %C must have explicit "   
		"or deferred shape");      
      return FAILURE;       
    }      
  }         
         
  if (g95_add_component(g95_current_block(), name0, &v) == FAILURE)
    return FAILURE;         
         
  v->ts = current_ts;     
  v->ts.cl = cl;  
  g95_set_component_attr(v, &current_attr);        
        
  v->initializer = *in; 
  *in = NULL;      
      
  v->as = *ar;   
  if (v->as != NULL) v->dimension = 1;       
  *ar = NULL;         
         
  /* Check array components */          
          
  if (!v->dimension) return SUCCESS;   
   
  if (v->pointer) {         
    if (v->as->type != AS_DEFERRED){ 
      g95_error("Pointer array component of structure at %C "
		"must have a deferred shape");       
      return FAILURE;
    }         
  } else {   
    if (v->as->type != AS_EXPLICIT) {  
      g95_error("Array component of structure at %C must have an explicit "
		"shape");   
      return FAILURE;   
    }     
  }         
         
  return SUCCESS;      
}         
         
         
   
   
/* match_attr_spec()-- Matches an attribute specification including
 * array specs.  If successful, leaves the variables current_attr and
 * current_as holding the specification.  Also sets the seen_colon
 * variable for later use by matchers associated with initializations.
 *
 * This subroutine is a little tricky in the sense that we don't know
 * if we really have an attr-spec until we hit the double colon.
 * Until that time, we can only return MATCH_NO.  This forces us to
 * check for duplicate specification at this level.  */        
        
static match match_attr_spec(void) {  
  
static mstring decls[] = {     
   minit(", allocatable",        DECL_ALLOCATABLE),        
   minit(", dimension",          DECL_DIMENSION),       
   minit(", external",           DECL_EXTERNAL),   
   minit(", intent ( in )",      DECL_IN),  
   minit(", intent ( out )",     DECL_OUT), 
   minit(", intent ( in out )",  DECL_INOUT),        
   minit(", intrinsic",          DECL_INTRINSIC),  
   minit(", optional",           DECL_OPTIONAL),          
   minit(", parameter",          DECL_PARAMETER),  
   minit(", pointer",            DECL_POINTER),        
   minit(", private",            DECL_PRIVATE),      
   minit(", public",             DECL_PUBLIC),
   minit(", save",               DECL_SAVE),    
   minit(", target",             DECL_TARGET),        
   minit("::",                   DECL_COLON),     
   minit(NULL, DECL_NONE)    
};        
        
g95_locus start, seen_at[NUM_DECL];        
int y, seen[NUM_DECL];   
decl_types g;    
char *attr;    
match o;
try s;       
       
  g95_clear_attr(&current_attr);    
  start = g95_current_locus;          
          
  current_as = NULL;  
  
/* See if we get all of the keywords up to the final double colon */

  for(y=0; y<NUM_DECL; y++)   
    seen[y] = 0;   
   
  for(;;) {  
    g = g95_match_strings(decls);    
    if (g == DECL_NONE || g == DECL_COLON) break;

    seen[g]++;         
    seen_at[g] = g95_current_locus;  
  
    if (g == DECL_DIMENSION) {   
      o = g95_match_array_spec(&current_as);

      if (o == MATCH_NO) { 
	g95_error("Missing dimension specification at %C"); 
	o = MATCH_ERROR;          
      }        
        
      if (o == MATCH_ERROR) goto cleanup;      
    }      
  }   
   
/* No double colon, so assume that we've been looking at something
 * else the whole time */  
  
  if (g == DECL_NONE) {
    o = MATCH_NO;   
    goto cleanup;         
  }       
       
/* Since we've seen a double colon, we have to be looking at an
 * attr-spec.  This means that we can now issue errors */      
      
  for(g=0; g<NUM_DECL; g++)    
    if (seen[g] > 1) {  
      switch(g) {         
      case DECL_ALLOCATABLE:  attr = "ALLOCATABLE";      break;     
      case DECL_DIMENSION:    attr = "DIMENSION";        break;      
      case DECL_EXTERNAL:     attr = "EXTERNAL";         break;      
      case DECL_IN:           attr = "INTENT (IN)";      break;       
      case DECL_OUT:          attr = "INTENT (OUT)";     break;  
      case DECL_INOUT:        attr = "INTENT (IN OUT)";  break; 
      case DECL_INTRINSIC:    attr = "INTRINSIC";        break;   
      case DECL_OPTIONAL:     attr = "OPTIONAL";         break;      
      case DECL_PARAMETER:    attr = "PARAMETER";        break;         
      case DECL_POINTER:      attr = "POINTER";          break;     
      case DECL_PRIVATE:      attr = "PRIVATE";          break;   
      case DECL_PUBLIC:       attr = "PUBLIC";           break;         
      case DECL_SAVE:         attr = "SAVE";             break;      
      case DECL_TARGET:       attr = "TARGET";           break;       
      default:  
	attr = NULL;  /* This shouldn't happen */  
      }       
       
      g95_error("Duplicate %s attribute at %L", attr, &seen_at[g]);  
      o = MATCH_ERROR;
      goto cleanup;  
    }   
   
/* Now that we've dealt with duplicate attributes, add the attributes to the 
 * current attribute. */         
         
  for(g=0; g<NUM_DECL; g++) {       
    if (seen[g] == 0) continue;         
         
    if (g95_current_state() == COMP_DERIVED &&
	g != DECL_DIMENSION && g != DECL_POINTER &&      
	g != DECL_COLON && g != DECL_NONE) {     
     
      g95_error("Attribute at %L is not allowed in a TYPE definition",          
		&seen_at[g]);    
      o = MATCH_ERROR;        
      goto cleanup;          
    }    
    
    switch(g) {  
    case DECL_ALLOCATABLE:         
      s = g95_add_allocatable(&current_attr, &seen_at[g]);         
      break;      
      
    case DECL_DIMENSION:          
      s = g95_add_dimension(&current_attr, &seen_at[g]); 
      break;        
        
    case DECL_EXTERNAL:    
      s = g95_add_external(&current_attr, &seen_at[g]);        
      break;   
   
    case DECL_IN:  
      s = g95_add_intent(&current_attr, INTENT_IN, &seen_at[g]);         
      break;          
          
    case DECL_OUT:        
      s = g95_add_intent(&current_attr, INTENT_OUT, &seen_at[g]);      
      break;       
       
    case DECL_INOUT:        
      s = g95_add_intent(&current_attr, INTENT_INOUT, &seen_at[g]);         
      break;

    case DECL_INTRINSIC:     
      s = g95_add_intrinsic(&current_attr, &seen_at[g]);      
      break; 
 
    case DECL_OPTIONAL:
      s = g95_add_optional(&current_attr, &seen_at[g]);   
      break;  
  
    case DECL_PARAMETER:   
      s = g95_add_flavor(&current_attr, FL_PARAMETER, &seen_at[g]);   
      break;  
  
    case DECL_POINTER:
      s = g95_add_pointer(&current_attr, &seen_at[g]);          
      break;          
          
    case DECL_PRIVATE:   
      s = g95_add_access(&current_attr, ACCESS_PRIVATE, &seen_at[g]);       
      break;      
      
    case DECL_PUBLIC:    
      s = g95_add_access(&current_attr, ACCESS_PUBLIC, &seen_at[g]);      
      break; 
 
    case DECL_SAVE:     
      s = g95_add_save(&current_attr, &seen_at[g]);     
      break;

    case DECL_TARGET:          
      s = g95_add_target(&current_attr, &seen_at[g]);      
      break;       
       
    default:        
      g95_internal_error("match_attr_spec(): Bad attribute");         
    }   
   
    if (s == FAILURE) { 
      o = MATCH_ERROR;   
      goto cleanup;   
    } 
  }   
   
  seen_colon = 1;        
  return MATCH_YES;        
        
cleanup: 
  g95_current_locus = start;
  g95_free_array_spec(current_as);
  current_as = NULL;  
  return o;      
}          
          
          
    
    
/* attr_decl1()-- Function that sets the attribute of a single variable */ 
 
static match attr_decl1(void) {       
char nm[G95_MAX_SYMBOL_LEN+1];   
g95_locus var_locus;         
g95_array_spec *ar;        
g95_symbol *symb;      
match q;       
       
  ar = NULL;

  q = g95_match_name(nm);      
  if (q != MATCH_YES) goto cleanup;       
       
  if (find_special(nm, &symb)) return MATCH_ERROR;        
        
  var_locus = g95_current_locus;      
      
/* Deal with possible array specification for certain attributes */         
         
  if (current_attr.dimension || current_attr.allocatable || 
      current_attr.pointer   || current_attr.target) {     
    q = g95_match_array_spec(&ar);         
    if (q == MATCH_ERROR) goto cleanup;

    if (current_attr.dimension && q == MATCH_NO) {         
	g95_error("Missing array specification at %L in DIMENSION statement", 
		  &var_locus);       
	q = MATCH_ERROR;    
	goto cleanup;    
    }    
    
    if ((current_attr.allocatable || current_attr.pointer) &&     
	  (q == MATCH_YES) && (ar->type != AS_DEFERRED)) {         
      g95_error("Array specification must be deferred at %L",     
		&var_locus);
      q = MATCH_ERROR; 
      goto cleanup;  
    }
  }

/* Update symbol table.  DIMENSION attribute is set in g95_set_array_spec(). */  
  
  if (current_attr.dimension == 0 &&         
      g95_copy_attr(&symb->attr, &current_attr, NULL) == FAILURE) {   
    q = MATCH_ERROR;        
    goto cleanup;    
  }        
        
  if (g95_set_array_spec(symb, ar, &var_locus) == FAILURE) {    
    q = MATCH_ERROR; 
    goto cleanup;   
  }          
          
  if ((current_attr.external || current_attr.intrinsic) &&     
      symb->attr.flavor != FL_PROCEDURE &&          
      g95_add_flavor(&symb->attr, FL_PROCEDURE, NULL) == FAILURE) {        
    q = MATCH_ERROR;     
    goto cleanup; 
  }          
          
  return MATCH_YES;

cleanup: 
  g95_free_array_spec(ar); 
  return q;        
}      
      
      
         
         
/* match_char_length()-- A character length is a '*' followed by a
 * literal integer or a char_len_param_value in parenthesis. */       
       
static match match_char_length(g95_expr **e2) {     
int length;    
match h; 
 
  h = g95_match_char('*');
  if (h != MATCH_YES) return h;          
          
  h = g95_match_small_literal_int(&length);        
  if (h == MATCH_ERROR) return h;   
   
  if (h == MATCH_YES) {  
    *e2 = g95_int_expr(length);        
    return h;         
  }

  if (g95_match_char('(') == MATCH_NO) goto syntax;     
     
  h = char_len_param_value(e2); 
  if (h == MATCH_ERROR) return h;   
  if (h == MATCH_NO) goto syntax;     
     
  if (g95_match_char(')') == MATCH_NO) {   
    g95_free_expr(*e2);
    *e2 = NULL;
    goto syntax;     
  }     
     
  return MATCH_YES;        
        
syntax: 
  g95_error("Syntax error in character length specification at %C"); 
  return MATCH_ERROR;  
}


        
        
/* match_result()-- Match a RESULT specification following a function
 * declaration or ENTRY statement.  Also matches the end-of-statement. */      
      
static match match_result(g95_symbol *function, g95_symbol **res) {         
char name[G95_MAX_SYMBOL_LEN+1];  
g95_symbol *i;    
match f;  
  
  if (g95_match(" result (") != MATCH_YES) return MATCH_NO;        
        
  f = g95_match_name(name);          
  if (f != MATCH_YES) return f;       
       
  if (g95_match(" )%t") != MATCH_YES) {   
    g95_error("Unexpected junk following RESULT variable at %C");       
    return MATCH_ERROR;    
  }       
       
  if (strcmp(function->name, name) == 0) {
    g95_error("RESULT variable at %C must be different than function name");     
    return MATCH_ERROR;   
  }      
      
  if (g95_get_symbol(name, NULL, &i)) return MATCH_ERROR;    
    
  if (g95_add_flavor(&i->attr, FL_VARIABLE, NULL) == FAILURE ||  
      g95_add_result(&i->attr, NULL) == FAILURE) return MATCH_ERROR;   
   
  *res = i;  
  
  return MATCH_YES;
}


         
         
/* g95_match_function_decl()-- Match a function declaration */  
  
match g95_match_function_decl(void) {    
char name[G95_MAX_SYMBOL_LEN+1];         
g95_symbol *symb, *res; 
g95_locus old; 
match m;      
      
  if (g95_current_state() != COMP_NONE &&  
      g95_current_state() != COMP_INTERFACE && 
      g95_current_state() != COMP_CONTAINS) return MATCH_NO;      
      
  g95_clear_ts(&current_ts);   
   
  old = g95_current_locus;  
  
  m = match_prefix(&current_ts);   
  if (m != MATCH_YES) {       
    g95_current_locus = old;          
    return m;    
  }    
    
  if (g95_match("function% %n", name) != MATCH_YES) {          
    g95_current_locus = old;  
    return MATCH_NO;      
  }   
   
  if (get_proc_name(name, &symb)) return MATCH_ERROR;       
  g95_new_block = symb;     
     
  m = g95_match_formal_arglist(symb, 0, 0);
  if (m == MATCH_NO)       
    g95_error("Expected formal argument list in function definition at %C");        
  if (m != MATCH_YES) goto cleanup;       
       
  res = NULL;

  if (g95_match_eos() != MATCH_YES) { /* See if a result variable is present */        
    m = match_result(symb, &res);       
    if (m == MATCH_NO)
      g95_error("Unexpected junk after function declaration at %C");       
       
    if (m != MATCH_YES) {         
      m = MATCH_ERROR;     
      goto cleanup;        
    } 
  }       
       
/* Make changes to the symbol */      
      
  m = MATCH_ERROR;   
   
  if (g95_add_function(&symb->attr, NULL) == FAILURE) goto cleanup;       
       
  if (copy_prefix(&symb->attr, &symb->declared_at) == FAILURE) goto cleanup;   
   
  if (current_ts.type != BT_UNKNOWN && symb->ts.type != BT_UNKNOWN) {   
    g95_error("Function '%s' at %C already has a type of %s", name,  
	      g95_basic_typename(symb->ts.type));      
    goto cleanup; 
  }          
          
  if (res == NULL) { 
    symb->ts = current_ts;       
    symb->result = symb;      
  } else {     
    res->ts = current_ts;      
    symb->result = res;   
  } 
 
  return MATCH_YES;   
   
cleanup:          
  g95_reject_statement();          
  g95_current_locus = old;       
  return m;      
}   
   
   
   
   
/* g95_match_subroutine()-- Match a subroutine statement, including
 * optional prefixes. */         
         
match g95_match_subroutine(void) { 
char name0[G95_MAX_SYMBOL_LEN+1];
g95_symbol *symbol;      
match q;      
      
  if (g95_current_state() != COMP_NONE &&     
      g95_current_state() != COMP_INTERFACE &&
      g95_current_state() != COMP_CONTAINS) return MATCH_NO;       
       
  q = match_prefix(NULL);          
  if (q != MATCH_YES) return q;      
      
  q = g95_match("subroutine% %n", name0);  
  if (q != MATCH_YES) return q;         
         
  if (get_proc_name(name0, &symbol)) return MATCH_ERROR; 
  g95_new_block = symbol;

  if (g95_add_subroutine(&symbol->attr, NULL) == FAILURE) return MATCH_ERROR;       
       
  if (g95_match_formal_arglist(symbol, 0, 1) != MATCH_YES) return MATCH_ERROR;

  if (g95_match_eos() != MATCH_YES) {  
    g95_syntax_error(ST_SUBROUTINE);       
    return MATCH_ERROR;
  }    
    
  if (copy_prefix(&symbol->attr, &symbol->declared_at) == FAILURE)     
    return MATCH_ERROR;        
        
  return MATCH_YES;         
}  
  
  
    
    
/* g95_match_entry()-- Match an ENTRY statement */

match g95_match_entry(void) {  
g95_symbol *function, *r, *entry; 
char name0[G95_MAX_SYMBOL_LEN+1];  
match z;

  z = g95_match_name(name0);
  if (z != MATCH_YES) return z; 
 
  if (g95_current_state() != COMP_SUBROUTINE &&          
      g95_current_state() != COMP_FUNCTION) {   
    g95_error("ENTRY statement at %C cannot appear within %s",      
	      g95_state_name(g95_current_state()));         
    return MATCH_ERROR;    
  }  
  
  if (g95_current_ns->parent != NULL &&   
      g95_current_ns->parent->proc_name->attr.flavor != FL_MODULE) {      
    g95_error("ENTRY statement at %C cannot appear in a contained procedure");      
    return MATCH_ERROR;   
  }         
         
  if (get_proc_name(name0, &entry)) return MATCH_ERROR;   
   
  if (g95_current_ns->proc_name->attr.subroutine) {  /* subroutine entry */          
    z = g95_match_formal_arglist(entry, 0, 1);
    if (z != MATCH_YES) return MATCH_ERROR;    
    
    if (g95_add_entry(&entry->attr, NULL) == FAILURE ||         
	g95_add_subroutine(&entry->attr, NULL) == FAILURE)     
      return MATCH_ERROR;        
        
  } else {      /* function entry */          
    z = g95_match_formal_arglist(entry, 0, 1);          
    if (z != MATCH_YES) return MATCH_ERROR;      
      
    function = g95_state_stack->sym;         
    r = NULL;        
        
    if (g95_match_eos() == MATCH_YES) { 
      if (g95_add_entry(&entry->attr, NULL) == FAILURE ||      
	  g95_add_function(&entry->attr, NULL) == FAILURE)          
	return MATCH_ERROR; 
 
      entry->result = entry;     
     
    } else {   
      z = match_result(function, &r);
      if (z == MATCH_NO) g95_syntax_error(ST_ENTRY);          
      if (z != MATCH_YES) return MATCH_ERROR;         
         
      if (g95_add_entry(&entry->attr, NULL) == FAILURE ||
	  g95_add_function(&entry->attr, NULL) == FAILURE)          
	return MATCH_ERROR;
    }    
    
    if (function->attr.recursive && r == NULL) {      
      g95_error("RESULT attribute required in ENTRY statement at %C");        
      return MATCH_ERROR;         
    }   
  }      
      
  if (g95_current_ns->parent != NULL &&
      g95_add_procedure(&entry->attr, PROC_MODULE, NULL) == FAILURE)          
    return MATCH_ERROR;         
         
  if (g95_match_eos() != MATCH_YES) {        
    g95_syntax_error(ST_ENTRY);       
    return MATCH_ERROR;       
  }        
        
  entry->attr.recursive = g95_current_ns->proc_name->attr.recursive;         
  entry->attr.elemental = g95_current_ns->proc_name->attr.elemental;      
  entry->attr.pure      = g95_current_ns->proc_name->attr.pure; 
 
  new_st.type = EXEC_ENTRY;    
  new_st.sym = entry; 
  return MATCH_YES;    
}    
    
    
  
  
/* attr_decl()-- Generic attribute declaration subroutine.  Used for
 * attributes that just have a list of names. */          
          
static match attr_decl(void) {      
match i;

  g95_match(" ::");   /* Gobble the optional double colon */     
   
  for(;;) {        
    i = attr_decl1();       
    if (i != MATCH_YES) break;      
      
    if (g95_match_eos() == MATCH_YES) {        
      i = MATCH_YES;   
      break; 
    }

    if (g95_match_char(',') != MATCH_YES) {   
      g95_error("Unexpected character in variable list at %C");       
      i = MATCH_ERROR;        
      break;  
    }      
  }   
   
  return i;       
}     
     
     
  
  
/* match_char_spec()-- Match the various kind/length specifications in
 * a CHARACTER declaration.  We don't return MATCH_NO. */   
   
static match match_char_spec(g95_typespec *ts) {  
int l, kind, seen_length;      
g95_charlen *c;  
g95_expr *leng;          
match j;  
  
  kind = g95_default_character_kind();      
  leng = NULL;  
  seen_length = 0; 
 
/* Try the old-style specification first */    
    
  old_char_selector = 0;  
  
  j = match_char_length(&leng);
  if (j != MATCH_NO) {     
    if (j == MATCH_YES) old_char_selector = 1;        
    seen_length = 1;        
    goto done;    
  }    
    
  j = g95_match_char('('); 
  if (j != MATCH_YES) {        
    j = MATCH_YES;  /* character without length is a single char */  
    goto done;
  }      
      
/* Try the weird case:  ( KIND = <int> [ , LEN = <len-param> ] )   */    
    
  if (g95_match(" kind =") == MATCH_YES) {   
    j = g95_match_small_int(&kind);       
    if (j == MATCH_ERROR) goto done;  
    if (j == MATCH_NO) goto syntax;    
    
    if (g95_match(" , len =") == MATCH_NO) goto rparen;

    j = char_len_param_value(&leng);         
    if (j == MATCH_NO) goto syntax;          
    if (j == MATCH_ERROR) goto done;      
    seen_length = 1;        
        
    goto rparen;   
  }   
   
/* Try to match ( LEN = <len-param> ) or ( LEN = <len-param>, KIND = <int> ) */    
    
  if (g95_match(" len =") == MATCH_YES) {       
    j = char_len_param_value(&leng);  
    if (j == MATCH_NO) goto syntax; 
    if (j == MATCH_ERROR) goto done;    
    seen_length = 1; 
 
    if (g95_match_char(')') == MATCH_YES) goto done;         
         
    if (g95_match(" , kind =") != MATCH_YES) goto syntax;        
        
    g95_match_small_int(&kind);         
         
    if (g95_validate_kind(BT_CHARACTER, kind) == -1) {   
      g95_error("Kind %d is not a CHARACTER kind at %C", kind);         
      return MATCH_YES;       
    }  
  
    goto rparen;          
  }          
          
/* Try to match   ( <len-param> ) or ( <len-param> , [ KIND = ] <int> ) */    
    
  j = char_len_param_value(&leng);      
  if (j == MATCH_NO) goto syntax;          
  if (j == MATCH_ERROR) goto done;     
  seen_length = 1;   
   
  j = g95_match_char(')');  
  if (j == MATCH_YES) goto done;         
         
  if (g95_match_char(',') != MATCH_YES) goto syntax;   
   
  g95_match(" kind =");   /* Gobble optional text */   
   
  j = g95_match_small_int(&kind);          
  if (j == MATCH_ERROR) goto done;  
  if (j == MATCH_NO) goto syntax;  
  
/* require a right-paren at this point */     
     
rparen:      
  j = g95_match_char(')');
  if (j == MATCH_YES) goto done;         
         
syntax:      
  g95_error("Syntax error in CHARACTER declaration at %C");    
  j = MATCH_ERROR;      
      
done:    
  if (j == MATCH_YES && g95_validate_kind(BT_CHARACTER, kind) == -1) {  
    g95_error("Kind %d is not a CHARACTER kind at %C", kind);      
    j = MATCH_ERROR;  
  }    
    
  if (j != MATCH_YES) {      
    g95_free_expr(leng); 
    leng = NULL;          
    return j;         
  }    
    
/* Do some final massaging of the length values */         
          
  c = g95_get_charlen();         
  c->next = g95_current_ns->cl_list;    
  g95_current_ns->cl_list = c;       
       
  if (seen_length == 0)        
    c->length = g95_int_expr(1);          
  else {
    if (leng == NULL || g95_extract_int(leng, &l) != NULL || l >= 0)       
      c->length = leng;          
    else {  
      g95_free_expr(leng);          
      c->length = g95_int_expr(0);       
    }     
  }        
        
  ts->cl = c;        
  ts->kind = kind;        
        
  return MATCH_YES; 
}       
       
       
   
   
match g95_match_pointer(void) {    
    
  g95_clear_attr(&current_attr);    
  g95_add_pointer(&current_attr, NULL);      
      
  return attr_decl();         
}  
  
  


match g95_match_optional(void) {     
     
  g95_clear_attr(&current_attr);       
  g95_add_optional(&current_attr, NULL);  
  
  return attr_decl();        
}          
          
          
   
   
/* variable_decl()-- Match a variable name with an optional
 * initializer.  When this subroutine is called, a variable is
 * expected to be parsed next.  Depending on what is happening at the
 * moment, updates either the symbol table or the current
 * interface. */        
        
static match variable_decl(void) { 
char name0[G95_MAX_SYMBOL_LEN+1];   
g95_expr *initializer, *char_len;   
g95_locus var_locus;    
g95_array_spec *as;       
g95_charlen *cl;        
match g;  
try s;   
   
  initializer = NULL;       
  as = NULL;   
   
  g = g95_match_name(name0);   
  if (g != MATCH_YES) goto cleanup;        
        
  var_locus = g95_current_locus;       
       
  g = g95_match_array_spec(&as);     
  if (g == MATCH_ERROR) goto cleanup;   
  if (g == MATCH_NO) as = g95_copy_array_spec(current_as);   
   
  char_len = NULL;  
  cl = NULL;         
         
  if (current_ts.type == BT_CHARACTER) {
    switch(match_char_length(&char_len)) {         
    case MATCH_YES:          
      cl = g95_get_charlen(); 
      cl->next = g95_current_ns->cl_list;    
      g95_current_ns->cl_list = cl;       
       
      cl->length = char_len;         
      break;       
       
    case MATCH_NO:          
      cl = current_ts.cl;

      /* Assumed length charlen nodes must be distinct */      
      if (cl->length == NULL) {     
	cl = g95_get_charlen();        
        
	cl->next = g95_current_ns->cl_list; 
	g95_current_ns->cl_list = cl;         
      }   
   
      break;  
  
    case MATCH_ERROR:
      goto cleanup;      
    }         
  }   
   
/* The double colon must be present in order to have initializers.
 * Otherwise the statement is ambiguous with an assignment statement. */    
    
  if (seen_colon) {          
    if (g95_match(" =>") == MATCH_YES) { 
 
      if (!current_attr.pointer) {    
	g95_error("Initialization at %C isn't for a pointer variable"); 
	g = MATCH_ERROR;         
	goto cleanup;   
      }   
   
      g = g95_match_null(&initializer);      
      if (g == MATCH_NO) {          
	g95_error("Pointer initialization requires a NULL at %C");        
	g = MATCH_ERROR;    
      }         
         
      if (g95_pure(NULL)) {        
	g95_error("Initialization of pointer at %C is not allowed in a "         
		  "PURE procedure");
	g = MATCH_ERROR;     
      }     
     
      if (g != MATCH_YES) goto cleanup; 
 
      initializer->ts = current_ts;          
          
    } else if (g95_match_char('=') == MATCH_YES) { 
      if (current_attr.pointer) {
	g95_error("Pointer initialization at %C requires '=>', not '='");  
	g = MATCH_ERROR;       
	goto cleanup;   
      }         
         
      g = g95_match_init_expr(&initializer);       
      if (g == MATCH_NO) {      
	g95_error("Expected an initialization expression at %C"); 
	g = MATCH_ERROR;        
      }   
   
      if (current_attr.flavor != FL_PARAMETER && g95_pure(NULL)) {         
	g95_error("Initialization of variable at %C is not allowed in a "      
		  "PURE procedure"); 
	g = MATCH_ERROR;        
      }         
               
      if (g != MATCH_YES) goto cleanup;   
    }      
  }          
          
/* In functions that have a RESULT variable defined, the function name
 * always refers to function calls.  Therefore, the name is not
 * allowed to appear in specification statements. */    
    
  if (g95_current_state() == COMP_FUNCTION && g95_current_block() != NULL &&          
      g95_current_block()->result != NULL &&       
      g95_current_block()->result != g95_current_block() &&  
      strcmp(g95_current_block()->name, name0) == 0) {        
    g95_error("Function name '%s' not allowed at %C", name0);          
    g = MATCH_ERROR; 
    goto cleanup;     
  }         
         
  if (g95_current_state() == COMP_DERIVED)     
    s = build_struct(name0, cl, &initializer, &as);          
  else          
    s = build_sym(name0, cl, &initializer, &as, &var_locus);  
  
  g = (s == SUCCESS) ? MATCH_YES : MATCH_ERROR;  
  
/* Free stuff up and return */     
     
cleanup:    
  g95_free_expr(initializer);          
  g95_free_array_spec(as);         
         
  return g;     
}


 
 
/* access_attr_decl()-- match the list of entities being specified in
 * a PUBLIC or PRIVATE statement. */         
         
static match access_attr_decl(g95_statement sta) {         
char name[G95_MAX_SYMBOL_LEN+1]; 
interface_type typ;    
g95_user_op *u;       
g95_symbol *symbol;        
int oper; 
match t;       
       
  if (g95_match(" ::") == MATCH_NO && g95_match_space() == MATCH_NO)  
    goto done;        
        
  for(;;) {        
    t = g95_match_generic_spec(&typ, name, &oper);          
    if (t == MATCH_NO) goto syntax;      
    if (t == MATCH_ERROR) return MATCH_ERROR;       
       
    switch(typ) {       
    case INTERFACE_NAMELESS: 
      goto syntax;      
      
    case INTERFACE_GENERIC:  
      if (g95_get_symbol(name, NULL, &symbol)) goto done;        
        
      if (g95_add_access(&symbol->attr,          
			 (sta == ST_PUBLIC) ? ACCESS_PUBLIC : ACCESS_PRIVATE,          
			 NULL) == FAILURE) return MATCH_ERROR;

      break;          
          
    case INTERFACE_INTRINSIC_OP:      
      if (g95_current_ns->operator_access[oper] == ACCESS_UNKNOWN) {    
	g95_current_ns->operator_access[oper] =  
	  (sta == ST_PUBLIC) ? ACCESS_PUBLIC : ACCESS_PRIVATE;         
      } else {      
	g95_error("Access specification of the %s operator at %C has "  
		  "already been specified", g95_op2string(oper));    
	goto done;       
      }          
          
      break;     
     
    case INTERFACE_USER_OP:    
      u = g95_get_uop(name); 
 
      if (u->access == ACCESS_UNKNOWN) {         
	u->access = (sta == ST_PUBLIC) ? ACCESS_PUBLIC : ACCESS_PRIVATE;  
      } else {          
	g95_error("Access specification of the .%s. operator at %C has "
		  "already been specified", symbol->name);      
	goto done;          
      }       
       
      break;          
    }       
       
    if (g95_match_char(',') == MATCH_NO) break;   
  }

  if (g95_match_eos() != MATCH_YES) goto syntax;
  return MATCH_YES;  
  
syntax:    
  g95_syntax_error(sta);     
     
done:      
  return MATCH_ERROR;   
}          
          
          
        
        
match g95_match_allocatable(void) {         
         
  g95_clear_attr(&current_attr);     
  g95_add_allocatable(&current_attr, NULL);     
     
  return attr_decl();         
}  
  
  
       
       
match g95_match_public(g95_statement *st) {         
         
  if (g95_match("public") != MATCH_YES) return MATCH_NO;         
         
  if (g95_match_eos() == MATCH_YES) { 
    *st = ST_PUBLIC;         
    return MATCH_YES;       
  } 
 
  *st = ST_ATTR_DECL;       
  return access_attr_decl(ST_PUBLIC);     
}          
          
          
     
     
match g95_match_target(void) {  
  
  g95_clear_attr(&current_attr);         
  g95_add_target(&current_attr, NULL);         
         
  return attr_decl();    
}        
        
        
   
   
match g95_match_external(void) {          
          
  g95_clear_attr(&current_attr);    
  g95_add_external(&current_attr, NULL);          
          
  return attr_decl();      
}         
         
         
         
          
          
/* g95_match_private()-- The PRIVATE statement is a bit weird in that
 * it can be a attribute declaration, but also works as a standlone
 * statement inside of a type declaration or a module. */ 
 
match g95_match_private(g95_statement *st0) { 
 
  if (g95_match("private") != MATCH_YES) return MATCH_NO;     
     
  if (g95_current_state() == COMP_DERIVED) {    
    if (g95_match_eos() == MATCH_YES) {         
      *st0 = ST_PRIVATE;     
      return MATCH_YES;         
    }    
    
    g95_syntax_error(ST_PRIVATE);    
    return MATCH_ERROR; 
  }    
    
  if (g95_match_eos() == MATCH_YES) {  
    *st0 = ST_PRIVATE;    
    return MATCH_YES;  
  }          
          
  *st0 = ST_ATTR_DECL;    
  return access_attr_decl(ST_PRIVATE);    
} 
 
 


/* g95_match_derived_decl()-- Match the beginning of a derived type
 * declaration.  If a type name was the result of a function, then it is
 * possible to have a symbol already to be known as a derived type yet
 * have no components. */       
       
match g95_match_derived_decl(void) {   
char n[G95_MAX_SYMBOL_LEN+1];    
symbol_attribute attribute;   
g95_symbol *sy;   
match l;   
   
  if (g95_current_state() == COMP_DERIVED) return MATCH_NO;    
    
  g95_clear_attr(&attribute); 
 
loop:  
  if (g95_match(" , private") == MATCH_YES) {    
    if (g95_find_state(COMP_MODULE) == FAILURE) {  
      g95_error("Derived type at %C can only be PRIVATE within a MODULE");         
      return MATCH_ERROR;  
    }    
    
    if (g95_add_access(&attribute, ACCESS_PRIVATE, NULL) == FAILURE)          
      return MATCH_ERROR;          
    goto loop;    
  }

  if (g95_match(" , public") == MATCH_YES) {      
    if (g95_find_state(COMP_MODULE) == FAILURE) {    
      g95_error("Derived type at %C can only be PUBLIC within a MODULE");     
      return MATCH_ERROR;    
    }   
   
    if (g95_add_access(&attribute, ACCESS_PUBLIC, NULL) == FAILURE)      
      return MATCH_ERROR;     
    goto loop;
  }         
         
  if (g95_match(" ::") != MATCH_YES && attribute.access != ACCESS_UNKNOWN) {         
    g95_error("Expected :: in TYPE definition at %C");          
    return MATCH_ERROR;  
  }  
  
  l = g95_match(" %n%t", n);          
  if (l != MATCH_YES) return l;       
       
/* Make sure the name isn't the name of an intrinsic type.  The
 * 'double precision' type doesn't get past the name matcher */ 
 
  if (strcmp(n, "integer") == 0   || strcmp(n, "real") == 0 || 
      strcmp(n, "character") == 0 || strcmp(n, "logical") == 0 ||    
      strcmp(n, "complex") == 0) {     
    g95_error("Type name '%s' at %C cannot be the same as an intrinsic type",      
	      n);  
    return MATCH_ERROR;        
  }         
         
  if (g95_get_symbol(n, NULL, &sy)) return MATCH_ERROR;          
          
  if (sy->ts.type != BT_UNKNOWN) {
    g95_error("Derived type name '%s' at %C already has a basic type "    
	      "of %s", sy->name, g95_typename(&sy->ts));       
    return MATCH_ERROR;         
  }       
       
/* The symbol may already have the derived attribute without the
 * components.  The ways this can happen is via a function definition,
 * an INTRINSIC statement or a subtype in another derived type that is
 * a pointer.  The first part of the AND clause is true if a the
 * symbol is not the return value of a function. */       
       
  if (sy->attr.flavor != FL_DERIVED && 
      g95_add_flavor(&sy->attr, FL_DERIVED, NULL) == FAILURE)          
    return MATCH_ERROR;       
       
  if (sy->components != NULL) {     
    g95_error("Derived type definition of '%s' at %C has already been defined",   
	      sy->name);    
    return MATCH_ERROR; 
  } 
 
  if (attribute.access != ACCESS_UNKNOWN &&         
      g95_add_access(&sy->attr, attribute.access, NULL) == FAILURE)  
    return MATCH_ERROR;       
       
  g95_new_block = sy;        
        
  return MATCH_YES;   
}

    
    
/* do_parm()-- Workhorse for g95_match_parameter */     
     
static match do_parm(void) {   
g95_symbol *s;  
g95_expr *i;      
match f;        
        
  f = g95_match_symbol(&s, 0);     
  if (f == MATCH_NO)      
    g95_error("Expected variable name at %C in PARAMETER statement");     
     
  if (f != MATCH_YES) return f;        
        
  if (g95_match_char('=') == MATCH_NO) {       
    g95_error("Expected = sign in PARAMETER statement at %C");       
    return MATCH_ERROR;         
  }         
         
  if (g95_add_flavor(&s->attr, FL_PARAMETER, NULL) == FAILURE)        
    return MATCH_ERROR;   
   
  f = g95_match_init_expr(&i); 
  if (f == MATCH_NO)       
    g95_error("Expected expression at %C in PARAMETER statement");  
  if (f != MATCH_YES) return f;         
         
  s->value = i;        
  return MATCH_YES;

  g95_free_expr(i);     
  return f;   
}     
     
     
 
 
/* g95_match_save()-- Save statements have a special syntax */   
   
match g95_match_save(void) {          
char nam[G95_MAX_SYMBOL_LEN+1];   
g95_common_head *p;      
g95_symbol *s;  
match o; 
 
  if (g95_match_eos() == MATCH_YES) {     
    if (g95_current_ns->seen_save) { 
      g95_error("Blanket SAVE statement at %C follows previous "   
		"SAVE statement");

      return MATCH_ERROR;  
    }    
    
    g95_current_ns->save_all = g95_current_ns->seen_save = 1;   
    return MATCH_YES;   
  }       
       
  if (g95_current_ns->save_all) {          
    g95_error("SAVE statement at %C follows blanket SAVE statement"); 
    return MATCH_ERROR; 
  }   
   
  g95_match(" ::");  
  
  for(;;) {     
    o = g95_match_symbol(&s, 0);       
    switch(o) {     
    case MATCH_YES:  
      if (g95_add_save(&s->attr, NULL) == FAILURE)    
	return MATCH_ERROR;     
      goto next_item;        
        
    case MATCH_NO:  
      break;     
     
    case MATCH_ERROR:     
      return MATCH_ERROR;
    }       
       
    o = g95_match(" / %n /", &nam);    
    if (o == MATCH_ERROR) return MATCH_ERROR;     
    if (o == MATCH_NO) goto syntax;          
          
    p = g95_get_common(nam);   
   
    if (p->use_assoc) {
      g95_error("COMMON block '%s' at %C is already USE associated", nam);  
      return MATCH_ERROR;       
    }          
          
    p->saved = 1;       
    g95_current_ns->seen_save = 1;       
       
  next_item:
    if (g95_match_eos() == MATCH_YES) break;       
    if (g95_match_char(',') != MATCH_YES) goto syntax;         
  }    
    
  return MATCH_YES;

syntax:    
  g95_error("Syntax error in SAVE statement at %C");        
  return MATCH_ERROR;  
} 
 
 
        
        
/* g95_match_data_decl()-- Match a data declaration statement */         
         
match g95_match_data_decl(void) {      
g95_symbol *s;     
match u;        
        
  u = g95_match_type_spec(&current_ts, 1);         
  if (u != MATCH_YES) return u;        
        
  if (current_ts.type == BT_DERIVED && g95_current_state() != COMP_DERIVED) {       
    s = g95_use_derived(current_ts.derived);

    if (s == NULL) {     
      u = MATCH_ERROR;         
      goto cleanup;      
    }          
          
    current_ts.derived = s;        
  }        
        
  seen_colon = 0;      
      
  u = match_attr_spec();        
  if (u == MATCH_ERROR) {        
    u = MATCH_NO;  
    goto cleanup; 
  }    
    
  if (current_ts.type == BT_DERIVED &&         
      current_ts.derived->components == NULL) {   
   
    if (current_attr.pointer && g95_current_state() == COMP_DERIVED) goto ok;     
     
    if (g95_find_symbol(current_ts.derived->name,    
			current_ts.derived->ns->parent, 1, &s) == 0) goto ok;    
    
    /* Hope that an ambiguous symbol is itself masked by a type definition */   
   
    if (s != NULL && s->attr.flavor == FL_DERIVED) goto ok;   
   
    g95_error("Derived type at %C has not been previously defined");        
    u = MATCH_ERROR; 
    goto cleanup;        
  }       
       
ok:      
      
/* Explanation is required here.  If we have an old-style character
 * declaration, and no new-style attribute specifications, then there
 * a comma is optional between the type specification and the variable
 * list. */      
      
  if (u == MATCH_NO && current_ts.type == BT_CHARACTER && old_char_selector)        
    g95_match_char(',');        
        
/* Give the types/attributes to symbols that follow */ 
 
  for(;;) {      
    u = variable_decl();    
    if (u == MATCH_ERROR) goto cleanup;
    if (u == MATCH_NO) break;          
          
    if (g95_match_eos() == MATCH_YES) goto cleanup;    
    if (g95_match_char(',') != MATCH_YES) break;  
  }       
         
  g95_error("Syntax error in data declaration at %C");     
  u = MATCH_ERROR;  
  
cleanup:    
  g95_free_array_spec(current_as);
  current_as = NULL;
  return u;     
}         
         
         
          
          
match g95_match_intrinsic(void) {  
  
  g95_clear_attr(&current_attr);    
  g95_add_intrinsic(&current_attr, NULL); 
 
  return attr_decl();   
}  
  
  
 
 
match g95_match_dimension(void) {

  g95_clear_attr(&current_attr);      
  g95_add_dimension(&current_attr, NULL);      
      
  return attr_decl();          
}    
    
    
     
     
match g95_match_intent(void) {      
sym_intent intent;    
    
  intent = match_intent_spec();       
  if (intent == INTENT_UNKNOWN) return MATCH_ERROR; 
 
  g95_clear_attr(&current_attr);        
  g95_add_intent(&current_attr, intent, NULL);   /* Can't fail */          
          
  return attr_decl();      
}    
    
    
 
 
/* g95_match_parameter()-- Match a parameter statement, with the weird
 * syntax that these have */      
      
match g95_match_parameter(void) {       
match m;  
  
  if (g95_match_char('(') == MATCH_NO) return MATCH_NO;   
   
  for(;;) {        
    m = do_parm();  
    if (m != MATCH_YES) break; 
 
    if (g95_match(" )%t") == MATCH_YES) break;      
      
    if (g95_match_char(',') != MATCH_YES) {      
      g95_error("Unexpected characters in PARAMETER statement at %C");   
      m = MATCH_ERROR;   
      break;   
    }        
  }        
        
  return m;       
}       
       
       
         
         
/* g95_match_type_spec()-- Matches a type specification.  If
 * successful, sets the ts structure to the matched specification.
 * This is necessary for FUNCTION and IMPLICIT statements.
 *
 * If kind_flag is nonzero, then we check for the optional kind
 * specification.  Not doing so is needed for matching an IMPLICIT
 * statement correctly. */       
       
match g95_match_type_spec(g95_typespec *t, int kind_flag) {    
char nam[G95_MAX_SYMBOL_LEN+1];      
g95_symbol *sym; 
match f;        
        
  g95_clear_ts(t);   
   
  if (g95_match(" integer") == MATCH_YES) {
    t->type = BT_INTEGER;        
    t->kind = g95_default_integer_kind(); 
    goto get_kind;       
  }    
    
  if (g95_match(" character") == MATCH_YES) {    
    t->type = BT_CHARACTER;         
    return match_char_spec(t); 
  }        
        
  if (g95_match(" real") == MATCH_YES) {
    t->type = BT_REAL;     
    t->kind = g95_default_real_kind();    
    goto get_kind;   
  } 
 
  if (g95_match(" double precision") == MATCH_YES) {         
    t->type = BT_REAL;   
    t->kind = g95_default_double_kind();      
    return MATCH_YES;          
  }  
  
  if (g95_match(" complex") == MATCH_YES) {          
    t->type = BT_COMPLEX;
    t->kind = g95_default_complex_kind();         
    goto get_kind;          
  }

  if (g95_match(" double complex") == MATCH_YES) {  
    t->type = BT_COMPLEX;  
    t->kind = g95_default_double_kind();         
    return MATCH_YES;          
  }  
  
  if (g95_match(" logical") == MATCH_YES) {    
    t->type = BT_LOGICAL; 
    t->kind = g95_default_logical_kind(); 
    goto get_kind;     
  }         
         
  f = g95_match(" type ( %n )", nam);   
  if (f != MATCH_YES) return f;

  /* Search for the name but allow the components to be defined later. */       
       
  if (g95_get_ha_symbol(nam, &sym)) {       
    g95_error("Type name '%s' at %C is ambiguous", nam);      
    return MATCH_ERROR; 
  }          
          
  if (sym->attr.flavor != FL_DERIVED &&  
      g95_add_flavor(&sym->attr, FL_DERIVED, NULL) == FAILURE)       
    return MATCH_ERROR;          
          
  t->type = BT_DERIVED;
  t->kind = 0;  
  t->derived = sym;  
  
  return MATCH_YES; 
 
/* For all types except double, derived and character, look for an
 * optional kind specifier.  MATCH_NO is actually OK at this point. */ 
 
get_kind: 
  if (kind_flag == 0) return MATCH_YES;   
   
  f = g95_match_kind_spec(t);        
  if (f == MATCH_NO && t->type != BT_CHARACTER)      
    f = g95_match_old_kind_spec(t);         
         
  if (f == MATCH_NO) f = MATCH_YES;  /* No kind specifier found */  
  
  return f;   
} 
 
 
