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
      
      


/* g95_match_null()-- Match a 'NULL()', and possibly take care of some
 * side effects. */     
     
match g95_match_null(g95_expr **r) {        
g95_symbol *sym;  
g95_expr *b;
match p;   
   
  p = g95_match(" null ( )");    
  if (p != MATCH_YES) return p;        
        
  /* The NULL symbol now has to be/become an intrinsic function */   
   
  if (g95_get_symbol("null", NULL, &sym)) {          
    g95_error("NULL() initialization at %C is ambiguous");
    return MATCH_ERROR;       
  }

  if (g95_intrinsic_symbol(sym, 1)) return MATCH_ERROR;    
    
  b = g95_get_expr();   
  b->where = *g95_current_locus();   
  b->type = EXPR_NULL;
  b->ts.type = BT_UNKNOWN;  
  
  *r = b;        
        
  return MATCH_YES; 
}    
    
    


/* find_special()-- Special subroutine for finding a symbol.  If we're
 * compiling a function or subroutine and the parent compilation unit
 * is an interface, then check to see if the name we've been given is
 * the name of the interface (located in another namespace).  If so,
 * return that symbol.  If not, use g95_get_symbol(). */    
    
static int find_special(char *name, g95_symbol **r) {
g95_state_data *u; 
 
  if (g95_current_state() != COMP_SUBROUTINE &&      
      g95_current_state() != COMP_FUNCTION) goto normal;          
          
  u = g95_state_stack->previous; 
  if (u == NULL) goto normal;    
    
  if (u->state != COMP_INTERFACE) goto normal; 
  if (u->sym == NULL) goto normal;   /* Nameless interface */          
          
  if (strcmp(name, u->sym->name) == 0) {
    *r = u->sym;    
    return 0;          
  }   
   
normal:         
  return g95_get_symbol(name, NULL, r);        
}        
        
        
         
         
/* char_len_param_value()-- Matches a character length specification,
 * which is either a specification expression or a '*'. */   
   
static match char_len_param_value(g95_expr **expr) {     
     
  if (g95_match_char('*') == MATCH_YES) {       
    *expr = NULL;       
    return MATCH_YES;      
  }         
         
  return g95_match_expr(expr);        
}


        
        
/* get_proc_name()-- Special subroutine for getting a symbol node
 * associated with a procedure name, used in SUBROUTINE and FUNCTION
 * statements.  The symbol is created in the parent using with symtree
 * node in the child unit pointing to the symbol.  If the current
 * namespace has no parent, then the symbol is just created in the
 * current unit. */ 
 
static int get_proc_name(char *nm, g95_symbol **res) { 
g95_symtree *st0;     
g95_symbol *sy;        
int rv;        
        
  if (g95_current_ns->parent == NULL)          
    return g95_get_symbol(nm, NULL, res);           
       
  rv = g95_get_symbol(nm, g95_current_ns->parent, res);       
  if (*res == NULL) return rv;       
       
  st0 = g95_new_symtree(&g95_current_ns->sym_root, nm);    
    
  sy = *res;       
  st0->n.sym = sy;        
  sy->refs++;         
         
  return rv;      
}     
     
     
      
      
/* match_char_length()-- A character length is a '*' followed by a
 * literal integer or a char_len_param_value in parenthesis. */     
     
static match match_char_length(g95_expr **expr) {        
int l;
match p;   
   
  p = g95_match_char('*');
  if (p != MATCH_YES) return p;   
   
  p = g95_match_small_literal_int(&l);     
  if (p == MATCH_ERROR) return p;     
     
  if (p == MATCH_YES) {  
    *expr = g95_int_expr(l);     
    return p;          
  } 
 
  if (g95_match_char('(') == MATCH_NO) goto syntax;    
    
  p = char_len_param_value(expr);    
  if (p == MATCH_ERROR) return p;    
  if (p == MATCH_NO) goto syntax;    
    
  if (g95_match_char(')') == MATCH_NO) {   
    g95_free_expr(*expr);      
    *expr = NULL;      
    goto syntax;       
  }    
    
  return MATCH_YES;          
          
syntax:      
  g95_error("Syntax error in character length specification at %C");   
  return MATCH_ERROR;    
}         
         
         
   
   
/* g95_match_old_kind_spec()-- Match an extended-f77 kind specification */ 
 
match g95_match_old_kind_spec(g95_typespec *ts) {       
match f;    
      
  if (g95_match_char('*') != MATCH_YES) return MATCH_NO;      
      
  f = g95_match_small_literal_int(&ts->kind);      
  if (f != MATCH_YES) return MATCH_ERROR;   
   
/* Massage the kind numbers for complex types */          
          
  if (ts->type == BT_COMPLEX && ts->kind == 8) ts->kind = 4;        
  if (ts->type == BT_COMPLEX && ts->kind == 16) ts->kind = 8;    
    
  if (g95_validate_kind(ts->type, ts->kind) == -1) {     
    g95_error("Old-style kind %d not supported for type %s at %C", ts->kind,
	      g95_basic_typename(ts->type));          
          
    return MATCH_ERROR;          
  }      
      
  return MATCH_YES;
}      
      
      
     
     
/* g95_match_modproc()-- Match a module procedure statement.  Note
 * that we have to modify symbols in the parent's namespace because
 * the current one was there to receive symbols that are in a
 * interface's formal argument list. */

match g95_match_modproc(void) {      
char nam[G95_MAX_SYMBOL_LEN+1];     
g95_symbol *symbol;          
match h;   
   
  if (g95_state_stack->state != COMP_INTERFACE ||       
      g95_state_stack->previous == NULL || 
      current_interface.type == INTERFACE_NAMELESS) {     
    g95_error("MODULE PROCEDURE at %C must be in a generic module interface");      
    return MATCH_ERROR; 
  }    
    
  for(;;) {        
    h = g95_match_name(nam);   
    if (h == MATCH_NO) goto syntax;        
    if (h != MATCH_YES) return MATCH_ERROR;      
      
    if (g95_get_symbol(nam, g95_current_ns->parent, &symbol))          
      return MATCH_ERROR;  
  
    if (symbol->attr.proc != PROC_MODULE &&      
	g95_add_procedure(&symbol->attr, PROC_MODULE, NULL) == FAILURE)     
      return MATCH_ERROR; 
 
    if (g95_add_interface(symbol) == FAILURE) return MATCH_ERROR;         
         
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
 
static try build_struct(char *nam, g95_charlen *charlen, g95_expr **i,   
			g95_array_spec **as) {    
g95_component *h;          
          
  if ((current_ts.type == BT_DERIVED) &&      
      (current_ts.derived == g95_current_block()) &&      
      (current_attr.pointer == 0)) {      
    g95_error("Component at %C must have the POINTER attribute");   
    return FAILURE;     
  }         
         
  if (g95_current_block()->attr.pointer && (*as)->rank != 0) {        
    if ((*as)->type != AS_DEFERRED && (*as)->type != AS_EXPLICIT) {        
      g95_error("Array component of structure at %C must have explicit "  
		"or deferred shape");   
      return FAILURE;          
    } 
  }   
   
  if (g95_add_component(g95_current_block(), nam, &h) == FAILURE)    
    return FAILURE; 
 
  h->ts = current_ts;          
  h->ts.cl = charlen;        
  g95_set_component_attr(h, &current_attr);  
  
  h->initializer = *i;        
  *i = NULL;         
         
  h->as = *as;     
  if (h->as != NULL) h->dimension = 1;   
  *as = NULL;

  /* Check array components */        
        
  if (!h->dimension) return SUCCESS;      
      
  if (h->pointer) {     
    if (h->as->type != AS_DEFERRED){
      g95_error("Pointer array component of structure at %C "     
		"must have a deferred shape");       
      return FAILURE;       
    }        
  } else {     
    if (h->as->type != AS_EXPLICIT) {
      g95_error("Array component of structure at %C must have an explicit "         
		"shape"); 
      return FAILURE;
    } 
  }    
    
  return SUCCESS;      
}          
          
          
   
   
/* g95_match_derived_decl()-- Match the beginning of a derived type
 * declaration.  If a type name was the result of a function, then it is
 * possible to have a symbol already to be known as a derived type yet
 * have no components. */   
   
match g95_match_derived_decl(void) {          
char name[G95_MAX_SYMBOL_LEN+1];     
symbol_attribute attribute;  
g95_symbol *symb;  
match w;  
  
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

  w = g95_match(" %n%t", name);
  if (w != MATCH_YES) return w;        
        
/* Make sure the name isn't the name of an intrinsic type.  The
 * 'double precision' type doesn't get past the name matcher */      
      
  if (strcmp(name, "integer") == 0   || strcmp(name, "real") == 0 ||          
      strcmp(name, "character") == 0 || strcmp(name, "logical") == 0 ||
      strcmp(name, "complex") == 0) {   
    g95_error("Type name '%s' at %C cannot be the same as an intrinsic type", 
	      name);          
    return MATCH_ERROR;   
  }        
        
  if (g95_get_symbol(name, NULL, &symb)) return MATCH_ERROR;   
   
  if (symb->ts.type != BT_UNKNOWN) {        
    g95_error("Derived type name '%s' at %C already has a basic type "      
	      "of %s", symb->name, g95_typename(&symb->ts));   
    return MATCH_ERROR;   
  }        
        
/* The symbol may already have the derived attribute without the
 * components.  The ways this can happen is via a function definition,
 * an INTRINSIC statement or a subtype in another derived type that is
 * a pointer.  The first part of the AND clause is true if a the
 * symbol is not the return value of a function. */      
      
  if (symb->attr.flavor != FL_DERIVED &&  
      g95_add_flavor(&symb->attr, FL_DERIVED, NULL) == FAILURE)      
    return MATCH_ERROR;          
          
  if (symb->components != NULL) {
    g95_error("Derived type definition of '%s' at %C has already been defined",         
	      symb->name);          
    return MATCH_ERROR;      
  }   
   
  if (attribute.access != ACCESS_UNKNOWN && 
      g95_add_access(&symb->attr, attribute.access, NULL) == FAILURE)
    return MATCH_ERROR;        
        
  g95_new_block = symb;          
          
  return MATCH_YES;
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
    
locus begin, seen_at[NUM_DECL];       
int k, seen[NUM_DECL];     
decl_types h;   
char *attribute;       
match b;       
try x;

  g95_clear_attr(&current_attr);        
  begin = *g95_current_locus();       
       
  current_as = NULL;       
       
/* See if we get all of the keywords up to the final double colon */     
     
  for(k=0; k<NUM_DECL; k++)
    seen[k] = 0;

  for(;;) {  
    h = g95_match_strings(decls);  
    if (h == DECL_NONE || h == DECL_COLON) break; 
 
    seen[h]++;   
    seen_at[h] = *g95_current_locus();       
       
    if (h == DECL_DIMENSION) {  
      b = g95_match_array_spec(&current_as);        
        
      if (b == MATCH_NO) {
	g95_error("Missing dimension specification at %C");          
	b = MATCH_ERROR;          
      } 
 
      if (b == MATCH_ERROR) goto cleanup;    
    }     
  }      
      
/* No double colon, so assume that we've been looking at something
 * else the whole time */       
       
  if (h == DECL_NONE) {      
    b = MATCH_NO;         
    goto cleanup;      
  }          
          
/* Since we've seen a double colon, we have to be looking at an
 * attr-spec.  This means that we can now issue errors */  
  
  for(h=0; h<NUM_DECL; h++)       
    if (seen[h] > 1) {
      switch(h) {   
      case DECL_ALLOCATABLE:  attribute = "ALLOCATABLE";      break;       
      case DECL_DIMENSION:    attribute = "DIMENSION";        break;        
      case DECL_EXTERNAL:     attribute = "EXTERNAL";         break;          
      case DECL_IN:           attribute = "INTENT (IN)";      break;         
      case DECL_OUT:          attribute = "INTENT (OUT)";     break;       
      case DECL_INOUT:        attribute = "INTENT (IN OUT)";  break;
      case DECL_INTRINSIC:    attribute = "INTRINSIC";        break;   
      case DECL_OPTIONAL:     attribute = "OPTIONAL";         break;          
      case DECL_PARAMETER:    attribute = "PARAMETER";        break;   
      case DECL_POINTER:      attribute = "POINTER";          break;        
      case DECL_PRIVATE:      attribute = "PRIVATE";          break;   
      case DECL_PUBLIC:       attribute = "PUBLIC";           break; 
      case DECL_SAVE:         attribute = "SAVE";             break; 
      case DECL_TARGET:       attribute = "TARGET";           break;        
      default:         
	attribute = NULL;  /* This shouldn't happen */       
      }  
  
      g95_error("Duplicate %s attribute at %L", attribute, &seen_at[h]);
      b = MATCH_ERROR;        
      goto cleanup;     
    }

/* Now that we've dealt with duplicate attributes, add the attributes to the 
 * current attribute. */        
        
  for(h=0; h<NUM_DECL; h++) { 
    if (seen[h] == 0) continue;  
  
    if (g95_current_state() == COMP_DERIVED &&       
	h != DECL_DIMENSION && h != DECL_POINTER &&     
	h != DECL_COLON && h != DECL_NONE) {      
      
      g95_error("Attribute at %L is not allowed in a TYPE definition",       
		&seen_at[h]);         
      b = MATCH_ERROR;    
      goto cleanup;     
    }    
    
    switch(h) {
    case DECL_ALLOCATABLE:    
      x = g95_add_allocatable(&current_attr, &seen_at[h]);     
      break;  
  
    case DECL_DIMENSION:
      x = g95_add_dimension(&current_attr, &seen_at[h]);       
      break;   
   
    case DECL_EXTERNAL:      
      x = g95_add_external(&current_attr, &seen_at[h]);   
      break;        
        
    case DECL_IN:          
      x = g95_add_intent(&current_attr, INTENT_IN, &seen_at[h]);
      break;         
         
    case DECL_OUT:    
      x = g95_add_intent(&current_attr, INTENT_OUT, &seen_at[h]);   
      break;          
          
    case DECL_INOUT:    
      x = g95_add_intent(&current_attr, INTENT_INOUT, &seen_at[h]);   
      break;         
         
    case DECL_INTRINSIC:     
      x = g95_add_intrinsic(&current_attr, &seen_at[h]);   
      break;  
  
    case DECL_OPTIONAL:     
      x = g95_add_optional(&current_attr, &seen_at[h]);        
      break;          
          
    case DECL_PARAMETER:         
      x = g95_add_flavor(&current_attr, FL_PARAMETER, &seen_at[h]);   
      break;         
         
    case DECL_POINTER:       
      x = g95_add_pointer(&current_attr, &seen_at[h]);        
      break;  
  
    case DECL_PRIVATE:
      x = g95_add_access(&current_attr, ACCESS_PRIVATE, &seen_at[h]);       
      break;    
    
    case DECL_PUBLIC:    
      x = g95_add_access(&current_attr, ACCESS_PUBLIC, &seen_at[h]);
      break;   
   
    case DECL_SAVE:      
      x = g95_add_save(&current_attr, &seen_at[h]);       
      break;     
     
    case DECL_TARGET:  
      x = g95_add_target(&current_attr, &seen_at[h]);       
      break;     
     
    default:    
      g95_internal_error("match_attr_spec(): Bad attribute");   
    }      
      
    if (x == FAILURE) {       
      b = MATCH_ERROR;         
      goto cleanup;   
    }        
  }       
       
  seen_colon = 1; 
  return MATCH_YES;         
         
cleanup:         
  g95_set_locus(&begin);  
  g95_free_array_spec(current_as);     
  current_as = NULL;      
  return b;  
}   
   
   
    
    
/* build_sym()-- Function called by variable_decl() that adds a name
 * to the symbol table. */  
  
static try build_sym(char *nm, g95_charlen *clen, g95_expr **initp,  
		     g95_array_spec **ref, locus *var_locus) {          
symbol_attribute attribute;  
g95_symbol *sym;          
g95_expr *init;     
     
  init = *initp;   
  if (find_special(nm, &sym)) return FAILURE; 
 
/* Start updating the symbol table.  Add basic type attribute if present */        
        
  if (current_ts.type != BT_UNKNOWN &&      
      (sym->attr.implicit_type == 0 ||         
       !g95_compare_types(&sym->ts, &current_ts)) && 
      g95_add_type(sym, &current_ts, var_locus) == FAILURE) return FAILURE;          
          
/* If this variable declaration is confirming an implicit parameter
 * type, then an initialization expression is not allowed. */       
       
  if (sym->attr.flavor == FL_PARAMETER && *initp != NULL) {
    g95_error("Initializer not allowed for PARAMETER '%s' at %C", sym->name);      
    return FAILURE;         
  }    
    
  if (sym->ts.type == BT_CHARACTER) sym->ts.cl = clen; 
 
/* Add dimension attribute if present. */ 
 
  if (g95_set_array_spec(sym, *ref, var_locus) == FAILURE) return FAILURE;          
  *ref = NULL;      
      
/* Add attribute to symbol.  The copy is so that we can reset the
 * dimension attribute. */          
          
  attribute = current_attr;  
  attribute.dimension = 0;       
       
  if (g95_copy_attr(&sym->attr, &attribute, var_locus) == FAILURE) return FAILURE;     
     
/* Add initializer, required for PARAMETERs. */        
        
  if (init == NULL) {   
    if (attribute.flavor == FL_PARAMETER) {
      g95_error("PARAMETER at %L is missing an initializer", var_locus);
      return FAILURE;  
    }    
  } else {  
  
    if (sym->attr.data) {    
      g95_error("Variable '%s' at %C with an initializer already appears "
		"in a DATA statement", sym->name);        
      return FAILURE;  
    }  
  
    /* Checking a derived type parameter has to be put off until later. */       
       
    if (sym->ts.type != BT_DERIVED && init->ts.type != BT_DERIVED &&          
	g95_check_assign_symbol(sym, init) == FAILURE) return FAILURE;          
          
    if (sym->attr.dimension && init->rank == 0) init->rank = sym->as->rank;         
         
    sym->value = init;         
    *initp = NULL;  
  }  
  
  return SUCCESS;      
}         
         
         
         
         
/* variable_decl()-- Match a variable name with an optional
 * initializer.  When this subroutine is called, a variable is
 * expected to be parsed next.  Depending on what is happening at the
 * moment, updates either the symbol table or the current
 * interface. */        
        
static match variable_decl(void) {      
char nm[G95_MAX_SYMBOL_LEN+1];
g95_expr *initializer, *char_len;     
g95_array_spec *ref;   
g95_charlen *c;       
locus var_locus;       
match h;         
try t;        
        
  initializer = NULL;  
  ref = NULL;      
      
  h = g95_match_name(nm);       
  if (h != MATCH_YES) goto cleanup;          
          
  var_locus = *g95_current_locus();         
         
  h = g95_match_array_spec(&ref);  
  if (h == MATCH_ERROR) goto cleanup;  
  if (h == MATCH_NO) ref = g95_copy_array_spec(current_as);        
        
  char_len = NULL;   
  c = NULL;  
  
  if (current_ts.type == BT_CHARACTER) {          
    switch(match_char_length(&char_len)) {   
    case MATCH_YES:   
      c = g95_get_charlen(); 
      c->next = g95_current_ns->cl_list;    
      g95_current_ns->cl_list = c;

      c->length = char_len;      
      break;    
    
    case MATCH_NO:   
      c = current_ts.cl;    
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
	h = MATCH_ERROR;  
	goto cleanup;   
      }          
          
      h = g95_match_null(&initializer);       
      if (h == MATCH_NO) {         
	g95_error("Pointer initialization requires a NULL at %C");      
	h = MATCH_ERROR;         
      }

      if (g95_pure(NULL)) {        
	g95_error("Initialization of pointer at %C is not allowed in a "
		  "PURE procedure");       
	h = MATCH_ERROR;        
      } 
 
      if (h != MATCH_YES) goto cleanup;    
    
      initializer->ts = current_ts;     
     
    } else if (g95_match_char('=') == MATCH_YES) {        
      if (current_attr.pointer) {      
	g95_error("Pointer initialization at %C requires '=>', not '='"); 
	h = MATCH_ERROR;    
	goto cleanup;      
      }    
    
      h = g95_match_init_expr(&initializer);  
      if (h == MATCH_NO) {      
	g95_error("Expected an initialization expression at %C");         
	h = MATCH_ERROR;       
      } 
 
      if (current_attr.flavor != FL_PARAMETER && g95_pure(NULL)) {  
	g95_error("Initialization of variable at %C is not allowed in a " 
		  "PURE procedure");  
	h = MATCH_ERROR;        
      } 
       
      if (h != MATCH_YES) goto cleanup;       
    }      
  }          
          
/* In functions that have a RESULT variable defined, the function name
 * always refers to function calls.  Therefore, the name is not
 * allowed to appear in specification statements. */   
   
  if (g95_current_state() == COMP_FUNCTION && g95_current_block() != NULL &&   
      g95_current_block()->result != NULL &&       
      g95_current_block()->result != g95_current_block() &&          
      strcmp(g95_current_block()->name, nm) == 0) {      
    g95_error("Function name '%s' not allowed at %C", nm);    
    h = MATCH_ERROR; 
    goto cleanup;
  } 
 
  if (g95_current_state() == COMP_DERIVED)         
    t = build_struct(nm, c, &initializer, &ref);  
  else   
    t = build_sym(nm, c, &initializer, &ref, &var_locus);     
     
  h = (t == SUCCESS) ? MATCH_YES : MATCH_ERROR;

/* Free stuff up and return */          
          
cleanup:       
  g95_free_expr(initializer);        
  g95_free_array_spec(ref);        
        
  return h;     
}   
   
   


/* g95_match_kind_spec()-- Match a kind specification.  Since kinds
 * are generally optional, we usually return MATCH_NO if something
 * goes wrong.  If a "kind=" string is found, then we know we have an
 * error. */ 
 
match g95_match_kind_spec(g95_typespec *ts) {
locus loc;         
g95_expr *f;     
match m, j;         
char *msg;      
      
  m = MATCH_NO;    
  f = NULL; 
 
  loc = *g95_current_locus();      
      
  if (g95_match_char('(') == MATCH_NO) return MATCH_NO;   
   
/* Also gobbles optional text */    
  if (g95_match(" kind = ") == MATCH_YES) m = MATCH_ERROR;     
     
  j = g95_match_init_expr(&f); 
  if (j == MATCH_NO) g95_error("Expected initialization expression at %C"); 
  if (j != MATCH_YES) return MATCH_ERROR;          
          
  if (f->rank != 0) {     
    g95_error("Expected scalar initialization expression at %C");       
    m = MATCH_ERROR;      
    goto no_match;   
  }       
       
  msg = g95_extract_int(f, &ts->kind);        
  if (msg != NULL) {          
    g95_error(msg);      
    m = MATCH_ERROR;    
    goto no_match;   
  }

  g95_free_expr(f);     
  f = NULL;      
      
  if (g95_validate_kind(ts->type, ts->kind) == -1) {      
    g95_error("Kind %d not supported for type %s at %C", ts->kind,     
	      g95_basic_typename(ts->type));       
       
    m = MATCH_ERROR;       
    goto no_match;      
  }  
  
  if (g95_match_char(')') != MATCH_YES) {          
    g95_error("Missing right paren at %C");     
    goto no_match;      
  }    
    
  return MATCH_YES;  
  
no_match:    
  g95_free_expr(f);          
  g95_set_locus(&loc);       
  return m;      
}      
      
      
         
         
/* attr_decl1()-- Function that sets the attribute of a single variable */ 
 
static match attr_decl1(void) {         
char nam[G95_MAX_SYMBOL_LEN+1]; 
g95_array_spec *spec;  
g95_symbol *sy;   
locus var_locus;
match f;       
       
  spec = NULL;

  f = g95_match_name(nam);   
  if (f != MATCH_YES) goto cleanup; 
 
  if (find_special(nam, &sy)) return MATCH_ERROR;      
      
  var_locus = *g95_current_locus();         
         
/* Deal with possible array specification for certain attributes */         
         
  if (current_attr.dimension || current_attr.allocatable ||      
      current_attr.pointer   || current_attr.target) {          
    f = g95_match_array_spec(&spec);
    if (f == MATCH_ERROR) goto cleanup;       
       
    if (current_attr.dimension && f == MATCH_NO) {    
	g95_error("Missing array specification at %L in DIMENSION statement",    
		  &var_locus); 
	f = MATCH_ERROR;       
	goto cleanup;          
    }     
     
    if ((current_attr.allocatable || current_attr.pointer) &&    
	  (f == MATCH_YES) && (spec->type != AS_DEFERRED)) {   
      g95_error("Array specification must be deferred at %L",
		&var_locus);       
      f = MATCH_ERROR;         
      goto cleanup;        
    }  
  }   
   
/* Update symbol table.  DIMENSION attribute is set in g95_set_array_spec(). */        
        
  if (current_attr.dimension == 0 && 
      g95_copy_attr(&sy->attr, &current_attr, NULL) == FAILURE) {         
    f = MATCH_ERROR; 
    goto cleanup;          
  }    
    
  if (g95_set_array_spec(sy, spec, &var_locus) == FAILURE) {  
    f = MATCH_ERROR;          
    goto cleanup;        
  }        
        
  if ((current_attr.external || current_attr.intrinsic) &&          
      sy->attr.flavor != FL_PROCEDURE &&         
      g95_add_flavor(&sy->attr, FL_PROCEDURE, NULL) == FAILURE) {
    f = MATCH_ERROR;      
    goto cleanup;       
  }        
        
  return MATCH_YES;       
       
cleanup:    
  g95_free_array_spec(spec);    
  return f;       
}  
  
  


/* match_prefix()-- Match a prefix associated with a function or
 * subroutine declaration.  If the typespec pointer is nonnull, then a
 * typespec can be matched.  Note that if nothing matches, MATCH_YES
 * is returned (the null string was matched). */   
   
static match match_prefix(g95_typespec *typesp) {      
int seen_type;

  g95_clear_attr(&current_attr);       
  seen_type = 0;   
   
loop:     
  if (!seen_type && typesp != NULL &&        
      g95_match_type_spec(typesp, 1) == MATCH_YES &&     
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
       
static try copy_prefix(symbol_attribute *d, locus *loc) {         
         
  if (current_attr.pure && g95_add_pure(d, loc) == FAILURE)          
    return FAILURE;          
          
  if (current_attr.elemental && g95_add_elemental(d, loc) == FAILURE)       
    return FAILURE;     
     
  if (current_attr.recursive && g95_add_recursive(d, loc) == FAILURE)
    return FAILURE;

  return SUCCESS;      
}          
          
          
      
      
/* match_char_spec()-- Match the various kind/length specifications in
 * a CHARACTER declaration.  We don't return MATCH_NO. */ 
 
static match match_char_spec(g95_typespec *typesp) {   
int z, k0, seen_length;         
g95_charlen *charlen;
g95_expr *l;
match w;    
    
  k0 = g95_default_character_kind();     
  l = NULL;          
  seen_length = 0;

/* Try the old-style specification first */       
       
  old_char_selector = 0; 
 
  w = match_char_length(&l);          
  if (w != MATCH_NO) {         
    if (w == MATCH_YES) old_char_selector = 1;     
    seen_length = 1;         
    goto done;    
  } 
 
  w = g95_match_char('(');         
  if (w != MATCH_YES) {  
    w = MATCH_YES;  /* character without length is a single char */        
    goto done;    
  } 
 
/* Try the weird case:  ( KIND = <int> [ , LEN = <len-param> ] )   */       
       
  if (g95_match(" kind =") == MATCH_YES) {          
    w = g95_match_small_int(&k0);       
    if (w == MATCH_ERROR) goto done; 
    if (w == MATCH_NO) goto syntax;          
          
    if (g95_match(" , len =") == MATCH_NO) goto rparen;   
   
    w = char_len_param_value(&l); 
    if (w == MATCH_NO) goto syntax;      
    if (w == MATCH_ERROR) goto done;         
    seen_length = 1;  
  
    goto rparen;    
  }     
     
/* Try to match ( LEN = <len-param> ) or ( LEN = <len-param>, KIND = <int> ) */          
          
  if (g95_match(" len =") == MATCH_YES) {         
    w = char_len_param_value(&l);          
    if (w == MATCH_NO) goto syntax;          
    if (w == MATCH_ERROR) goto done;    
    seen_length = 1;      
      
    if (g95_match_char(')') == MATCH_YES) goto done; 
 
    if (g95_match(" , kind =") != MATCH_YES) goto syntax;          
          
    g95_match_small_int(&k0);   
   
    if (g95_validate_kind(BT_CHARACTER, k0) == -1) {   
      g95_error("Kind %d is not a CHARACTER kind at %C", k0);
      return MATCH_YES;          
    }       
       
    goto rparen;      
  }     
     
/* Try to match   ( <len-param> ) or ( <len-param> , [ KIND = ] <int> ) */       
       
  w = char_len_param_value(&l);   
  if (w == MATCH_NO) goto syntax;
  if (w == MATCH_ERROR) goto done; 
  seen_length = 1;   
   
  w = g95_match_char(')');      
  if (w == MATCH_YES) goto done;         
         
  if (g95_match_char(',') != MATCH_YES) goto syntax;    
    
  g95_match(" kind =");   /* Gobble optional text */      
      
  w = g95_match_small_int(&k0);  
  if (w == MATCH_ERROR) goto done;         
  if (w == MATCH_NO) goto syntax;       
       
/* require a right-paren at this point */      
      
rparen:
  w = g95_match_char(')');   
  if (w == MATCH_YES) goto done;     
     
syntax:       
  g95_error("Syntax error in CHARACTER declaration at %C");          
  w = MATCH_ERROR;

done:
  if (w == MATCH_YES && g95_validate_kind(BT_CHARACTER, k0) == -1) {
    g95_error("Kind %d is not a CHARACTER kind at %C", k0);    
    w = MATCH_ERROR;         
  }          
          
  if (w != MATCH_YES) { 
    g95_free_expr(l);    
    l = NULL; 
    return w;       
  }  
  
  if (l != NULL && g95_specification_expr(l) == FAILURE) {
    g95_free_expr(l);   
    return MATCH_ERROR;          
  } 
 
/* Do some final massaging of the length values */        
         
  charlen = g95_get_charlen();      
  charlen->next = g95_current_ns->cl_list;       
  g95_current_ns->cl_list = charlen;    
    
  if (seen_length == 0)      
    charlen->length = g95_int_expr(1);     
  else {     
    if (l == NULL || g95_extract_int(l, &z) != NULL || z >= 0)
      charlen->length = l;  
    else {        
      g95_free_expr(l);   
      charlen->length = g95_int_expr(0); 
    }    
  }       
       
  typesp->cl = charlen;          
  typesp->kind = k0;       
       
  return MATCH_YES;      
}




/* g95_match_save()-- Save statements have a special syntax */ 
 
match g95_match_save(void) {        
char name[G95_MAX_SYMBOL_LEN+1];          
g95_common_head *n;  
g95_symbol *sy;        
match g;      
      
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
    g = g95_match_symbol(&sy, 0);       
    switch(g) { 
    case MATCH_YES:      
      if (g95_add_save(&sy->attr, g95_current_locus()) == FAILURE)      
	return MATCH_ERROR;          
      goto next_item;          
          
    case MATCH_NO:         
      break;       
       
    case MATCH_ERROR:      
      return MATCH_ERROR;       
    }    
    
    g = g95_match(" / %n /", &name);     
    if (g == MATCH_ERROR) return MATCH_ERROR; 
    if (g == MATCH_NO) goto syntax;   
   
    n = g95_get_common(name);   
   
    if (n->use_assoc) {  
      g95_error("COMMON block '%s' at %C is already USE associated", name);     
      return MATCH_ERROR;       
    } 
 
    n->saved = 1;       
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
 
 
 
 
/* attr_decl()-- Generic attribute declaration subroutine.  Used for
 * attributes that just have a list of names. */         
         
static match attr_decl(void) { 
match d;  
  
  g95_match(" ::");   /* Gobble the optional double colon */       
     
  for(;;) {         
    d = attr_decl1();         
    if (d != MATCH_YES) break;       
       
    if (g95_match_eos() == MATCH_YES) {     
      d = MATCH_YES;         
      break;   
    }          
          
    if (g95_match_char(',') != MATCH_YES) {        
      g95_error("Unexpected character in variable list at %C");   
      d = MATCH_ERROR;          
      break;  
    } 
  }     
     
  return d;  
}         
         
         
     
     
/* match_result()-- Match a RESULT specification following a function
 * declaration or ENTRY statement.  Also matches the end-of-statement. */      
      
static match match_result(g95_symbol *function, g95_symbol **rslt) {        
char name[G95_MAX_SYMBOL_LEN+1]; 
g95_symbol *i;     
match j;

  if (g95_match(" result (") != MATCH_YES) return MATCH_NO;       
       
  j = g95_match_name(name);         
  if (j != MATCH_YES) return j;      
      
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

  *rslt = i;

  return MATCH_YES;        
}    
    
    
          
          
match g95_match_pointer(void) {  
  
  g95_clear_attr(&current_attr);  
  g95_add_pointer(&current_attr, NULL);       
       
  return attr_decl();  
} 
 
 
      
      
match g95_match_external(void) {

  g95_clear_attr(&current_attr);        
  g95_add_external(&current_attr, NULL);      
      
  return attr_decl();  
}   
   
   
   
        
        
match g95_match_dimension(void) {        
        
  g95_clear_attr(&current_attr);       
  g95_add_dimension(&current_attr, NULL);      
      
  return attr_decl();          
}      
      
      
   
   
match g95_match_intrinsic(void) {    
    
  g95_clear_attr(&current_attr);        
  g95_add_intrinsic(&current_attr, NULL);          
          
  return attr_decl();     
} 
 
 
   
   
/* g95_match_subroutine()-- Match a subroutine statement, including
 * optional prefixes. */      
      
match g95_match_subroutine(void) {
char nm[G95_MAX_SYMBOL_LEN+1];  
g95_symbol *sy;      
match d; 
 
  if (g95_current_state() != COMP_NONE &&
      g95_current_state() != COMP_INTERFACE &&      
      g95_current_state() != COMP_CONTAINS) return MATCH_NO;   
   
  d = match_prefix(NULL);
  if (d != MATCH_YES) return d;      
      
  d = g95_match("subroutine% %n", nm);  
  if (d != MATCH_YES) return d; 
 
  if (get_proc_name(nm, &sy)) return MATCH_ERROR;         
  g95_new_block = sy;     
     
  if (g95_add_subroutine(&sy->attr, NULL) == FAILURE) return MATCH_ERROR;    
    
  if (g95_match_formal_arglist(sy, 0, 1) != MATCH_YES) return MATCH_ERROR;        
        
  if (g95_match_eos() != MATCH_YES) {   
    g95_syntax_error(ST_SUBROUTINE);          
    return MATCH_ERROR;      
  }        
        
  if (copy_prefix(&sy->attr, &sy->declared_at) == FAILURE)    
    return MATCH_ERROR;   
   
  return MATCH_YES; 
} 
 
 
   
   
/* g95_match_type_spec()-- Matches a type specification.  If
 * successful, sets the ts structure to the matched specification.
 * This is necessary for FUNCTION and IMPLICIT statements.
 *
 * If kind_flag is nonzero, then we check for the optional kind
 * specification.  Not doing so is needed for matching an IMPLICIT
 * statement correctly. */  
  
match g95_match_type_spec(g95_typespec *typesp, int kind_flag) {   
char n[G95_MAX_SYMBOL_LEN+1];   
g95_symbol *sym;    
match b;          
          
  g95_clear_ts(typesp); 
 
  if (g95_match(" integer") == MATCH_YES) {     
    typesp->type = BT_INTEGER;
    typesp->kind = g95_default_integer_kind();     
    goto get_kind; 
  }         
         
  if (g95_match(" character") == MATCH_YES) {   
    typesp->type = BT_CHARACTER;        
    return match_char_spec(typesp);   
  }  
  
  if (g95_match(" real") == MATCH_YES) {        
    typesp->type = BT_REAL;  
    typesp->kind = g95_default_real_kind();       
    goto get_kind;   
  } 
 
  if (g95_match(" double precision") == MATCH_YES) {    
    typesp->type = BT_REAL; 
    typesp->kind = g95_default_double_kind();        
    return MATCH_YES;         
  }   
   
  if (g95_match(" complex") == MATCH_YES) {          
    typesp->type = BT_COMPLEX;      
    typesp->kind = g95_default_complex_kind();
    goto get_kind;     
  }       
       
  if (g95_match(" double complex") == MATCH_YES) {  
    typesp->type = BT_COMPLEX;  
    typesp->kind = g95_default_double_kind();     
    return MATCH_YES; 
  }          
          
  if (g95_match(" logical") == MATCH_YES) {  
    typesp->type = BT_LOGICAL;    
    typesp->kind = g95_default_logical_kind(); 
    goto get_kind;        
  }       
       
  b = g95_match(" type ( %n )", n); 
  if (b != MATCH_YES) return b;    
    
  /* Search for the name but allow the components to be defined later. */      
      
  if (g95_get_ha_symbol(n, &sym)) {   
    g95_error("Type name '%s' at %C is ambiguous", n);    
    return MATCH_ERROR;         
  }       
       
  if (sym->attr.flavor != FL_DERIVED &&          
      g95_add_flavor(&sym->attr, FL_DERIVED, NULL) == FAILURE)    
    return MATCH_ERROR;

  typesp->type = BT_DERIVED;   
  typesp->kind = 0;   
  typesp->derived = sym;  
  
  return MATCH_YES;      
      
/* For all types except double, derived and character, look for an
 * optional kind specifier.  MATCH_NO is actually OK at this point. */          
          
get_kind:      
  if (kind_flag == 0) return MATCH_YES;       
       
  b = g95_match_kind_spec(typesp); 
  if (b == MATCH_NO && typesp->type != BT_CHARACTER)         
    b = g95_match_old_kind_spec(typesp);      
      
  if (b == MATCH_NO) b = MATCH_YES;  /* No kind specifier found */   
   
  return b; 
}          
          
          
        
        
/* access_attr_decl()-- match the list of entities being specified in
 * a PUBLIC or PRIVATE statement. */    
    
static match access_attr_decl(g95_statement st) {   
char name0[G95_MAX_SYMBOL_LEN+1];   
interface_type t; 
g95_user_op *u;
g95_symbol *symb;  
int op1;       
match o; 
 
  if (g95_match(" ::") == MATCH_NO && g95_match_space() == MATCH_NO)        
    goto done;          
          
  for(;;) {    
    o = g95_match_generic_spec(&t, name0, &op1);  
    if (o == MATCH_NO) goto syntax;
    if (o == MATCH_ERROR) return MATCH_ERROR;       
       
    switch(t) {     
    case INTERFACE_NAMELESS:     
      goto syntax;   
   
    case INTERFACE_GENERIC:    
      if (g95_get_symbol(name0, NULL, &symb)) goto done;      
      
      if (g95_add_access(&symb->attr,     
			 (st == ST_PUBLIC) ? ACCESS_PUBLIC : ACCESS_PRIVATE,          
			 NULL) == FAILURE) return MATCH_ERROR;       
       
      break;    
    
    case INTERFACE_INTRINSIC_OP: 
      if (g95_current_ns->operator_access[op1] == ACCESS_UNKNOWN) {    
	g95_current_ns->operator_access[op1] =  
	  (st == ST_PUBLIC) ? ACCESS_PUBLIC : ACCESS_PRIVATE;    
      } else {    
	g95_error("Access specification of the %s operator at %C has "
		  "already been specified", g95_op2string(op1));    
	goto done;     
      }        
        
      break;          
          
    case INTERFACE_USER_OP:     
      u = g95_get_uop(name0);   
   
      if (u->access == ACCESS_UNKNOWN) { 
	u->access = (st == ST_PUBLIC) ? ACCESS_PUBLIC : ACCESS_PRIVATE;
      } else {     
	g95_error("Access specification of the .%s. operator at %C has "          
		  "already been specified", symb->name);    
	goto done;          
      }      
      
      break;   
    }      
      
    if (g95_match_char(',') == MATCH_NO) break;      
  }         
         
  if (g95_match_eos() != MATCH_YES) goto syntax;
  return MATCH_YES;       
       
syntax:
  g95_syntax_error(st);        
        
done:          
  return MATCH_ERROR;         
}   
   
   
 
 
match g95_match_optional(void) {    
    
  g95_clear_attr(&current_attr);  
  g95_add_optional(&current_attr, NULL);

  return attr_decl();    
}


   
   
/* g95_match_entry()-- Match an ENTRY statement */      
      
match g95_match_entry(void) {    
g95_symbol *function, *r, *entry;  
char n[G95_MAX_SYMBOL_LEN+1]; 
g95_compile_state state; 
match g;          
          
  g = g95_match_name(n); 
  if (g != MATCH_YES) return g; 
 
  if (get_proc_name(n, &entry)) return MATCH_ERROR;         
         
  g95_enclosing_unit(&state);    
  switch(state) {      
  case COMP_SUBROUTINE:          
    g = g95_match_formal_arglist(entry, 0, 1);          
    if (g != MATCH_YES) return MATCH_ERROR;    
    
    if (g95_current_state() != COMP_SUBROUTINE) goto exec_construct;          
          
    if (g95_add_entry(&entry->attr, NULL) == FAILURE ||  
	g95_add_subroutine(&entry->attr, NULL) == FAILURE)
      return MATCH_ERROR;      
      
    break;   
   
  case COMP_FUNCTION:         
    g = g95_match_formal_arglist(entry, 0, 0);          
    if (g != MATCH_YES) return MATCH_ERROR;         
         
    if (g95_current_state() != COMP_FUNCTION) goto exec_construct;  
    function = g95_state_stack->sym;          
          
    r = NULL;    
    
    if (g95_match_eos() == MATCH_YES) {         
      if (g95_add_entry(&entry->attr, NULL) == FAILURE ||         
	  g95_add_function(&entry->attr, NULL) == FAILURE)
	return MATCH_ERROR;         
         
      entry->result = function->result;

    } else {    
      g = match_result(function, &r);  
      if (g == MATCH_NO) g95_syntax_error(ST_ENTRY);
      if (g != MATCH_YES) return MATCH_ERROR;     
     
      if (g95_add_result(&r->attr, NULL) == FAILURE ||          
	  g95_add_entry(&entry->attr, NULL) == FAILURE ||   
	  g95_add_function(&entry->attr, NULL) == FAILURE)      
	return MATCH_ERROR;        
    }     
     
    if (function->attr.recursive && r == NULL) {
      g95_error("RESULT attribute required in ENTRY statement at %C");    
      return MATCH_ERROR;
    }  
  
    break;     
     
  default:     
    goto exec_construct;    
  } 
 
  if (g95_match_eos() != MATCH_YES) {  
    g95_syntax_error(ST_ENTRY);    
    return MATCH_ERROR;    
  } 
 
  return MATCH_YES; 
 
exec_construct:     
  g95_error("ENTRY statement at %C cannot appear within %s",
	    g95_state_name(g95_current_state()));         
         
  return MATCH_ERROR; 
}       
       
       
    
    
/* g95_match_data_decl()-- Match a data declaration statement */        
        
match g95_match_data_decl(void) {    
g95_symbol *symbol;     
match p; 
 
  p = g95_match_type_spec(&current_ts, 1);      
  if (p != MATCH_YES) return p;        
        
  if (current_ts.type == BT_DERIVED && g95_current_state() != COMP_DERIVED) {   
    symbol = g95_use_derived(current_ts.derived);     
     
    if (symbol == NULL) {         
      p = MATCH_ERROR;   
      goto cleanup;   
    }    
    
    current_ts.derived = symbol;      
  }         
         
  seen_colon = 0;   
   
  p = match_attr_spec();         
  if (p == MATCH_ERROR) {
    p = MATCH_NO;          
    goto cleanup;     
  }   
   
  if (current_ts.type == BT_DERIVED &&   
      current_ts.derived->components == NULL) { 
 
    if (current_attr.pointer && g95_current_state() == COMP_DERIVED) goto ok;       
       
    if (g95_find_symbol(current_ts.derived->name,  
			current_ts.derived->ns->parent, 1, &symbol) == 0) goto ok;  
  
    /* Hope that an ambiguous symbol is itself masked by a type definition */          
          
    if (symbol != NULL && symbol->attr.flavor == FL_DERIVED) goto ok;   
   
    g95_error("Derived type at %C has not been previously defined");       
    p = MATCH_ERROR;        
    goto cleanup;      
  }     
     
ok:       
       
/* Explanation is required here.  If we have an old-style character
 * declaration, and no new-style attribute specifications, then there
 * a comma is optional between the type specification and the variable
 * list. */         
         
  if (p == MATCH_NO && current_ts.type == BT_CHARACTER && old_char_selector)
    g95_match_char(',');         
         
/* Give the types/attributes to symbols that follow */   
   
  for(;;) {       
    p = variable_decl();   
    if (p == MATCH_ERROR) goto cleanup;      
    if (p == MATCH_NO) break;         
         
    if (g95_match_eos() == MATCH_YES) goto cleanup;      
    if (g95_match_char(',') != MATCH_YES) break; 
  }       
         
  g95_error("Syntax error in data declaration at %C");     
  p = MATCH_ERROR;        
        
cleanup:         
  g95_free_array_spec(current_as);  
  current_as = NULL;  
  return p;   
}       
       
       
        
        
/* g95_match_function_decl()-- Match a function declaration */          
          
match g95_match_function_decl(void) {          
char n[G95_MAX_SYMBOL_LEN+1];  
g95_symbol *sy, *res;       
locus o;    
match j;          
          
  if (g95_current_state() != COMP_NONE &&    
      g95_current_state() != COMP_INTERFACE &&         
      g95_current_state() != COMP_CONTAINS) return MATCH_NO; 
 
  g95_clear_ts(&current_ts);       
       
  o = *g95_current_locus();     
     
  j = match_prefix(&current_ts);        
  if (j != MATCH_YES) { 
    g95_set_locus(&o);    
    return j;
  }          
          
  if (g95_match("function% %n", n) != MATCH_YES) {        
    g95_set_locus(&o);      
    return MATCH_NO;       
  }   
   
  if (get_proc_name(n, &sy)) return MATCH_ERROR;        
  g95_new_block = sy;    
    
  j = g95_match_formal_arglist(sy, 0, 0);  
  if (j == MATCH_NO)       
    g95_error("Expected formal argument list in function definition at %C");        
  if (j != MATCH_YES) goto cleanup;       
       
  res = NULL;         
         
  if (g95_match_eos() != MATCH_YES) { /* See if a result variable is present */ 
    j = match_result(sy, &res);     
    if (j == MATCH_NO)    
      g95_error("Unexpected junk after function declaration at %C");        
        
    if (j != MATCH_YES) {          
      j = MATCH_ERROR;  
      goto cleanup;        
    }  
  } 
 
/* Make changes to the symbol */      
      
  j = MATCH_ERROR;    
    
  if (g95_add_function(&sy->attr, NULL) == FAILURE) goto cleanup;       
       
  if (g95_missing_attr(&sy->attr, NULL) == FAILURE ||   
      copy_prefix(&sy->attr, &sy->declared_at) == FAILURE) goto cleanup; 
 
  if (current_ts.type != BT_UNKNOWN && sy->ts.type != BT_UNKNOWN) {         
    g95_error("Function '%s' at %C already has a type of %s", n,      
	      g95_basic_typename(sy->ts.type));     
    goto cleanup;   
  }     
     
  if (res == NULL) {
    sy->ts = current_ts;         
    sy->result = sy;   
  } else {    
    res->ts = current_ts;    
    sy->result = res;      
  }   
   
  return MATCH_YES;         
         
cleanup:         
  g95_reject_statement();     
  g95_set_locus(&o);       
  return j;        
}    
    
    
 
 
/* g95_match_formal_arglist()-- Match a formal argument list. */ 
 
match g95_match_formal_arglist(g95_symbol *progname, int st_flag,       
			       int null_flag) {
g95_formal_arglist *h, *t, *i, *w;          
char name0[G95_MAX_SYMBOL_LEN+1];       
g95_symbol *s;       
match l;      
      
  h = t = NULL;   
   
  if (g95_match_char('(') != MATCH_YES) {  
    if (null_flag) goto ok;      
    return MATCH_NO;        
  }        
        
  if (g95_match_char(')') == MATCH_YES) goto ok;

  for(;;) {    
    if (g95_match_char('*') == MATCH_YES)  
      s = NULL;  
    else {         
      l = g95_match_name(name0);     
      if (l != MATCH_YES) goto cleanup; 
 
      if (g95_get_symbol(name0, NULL, &s)) goto cleanup;     
    }   
   
    i = g95_get_formal_arglist();   
   
    if (h == NULL) 
      h = t = i;  
    else {     
      t->next = i;     
      t = i;     
    }       
       
    t->sym = s;          
          
/* We don't add the VARIABLE flavor because the name could be a dummy
 * procedure.  We don't apply these attributes to formal arguments of
 * statement functions. */          
          
    if (s != NULL && !st_flag &&        
	(g95_add_dummy(&s->attr, NULL) == FAILURE ||        
	 g95_missing_attr(&s->attr, NULL) == FAILURE)) {       
      l = MATCH_ERROR;         
      goto cleanup;         
    }      
      
/* The name of a program unit can be in a different namespace, so
 * check for it explicitly.  After the statement is accepted, the name
 * is checked for especially in g95_get_symbol(). */ 
 
    if (g95_new_block != NULL && s != NULL &&       
	strcmp(s->name, g95_new_block->name) == 0) {   
      g95_error("Name '%s' at %C is the name of the procedure", s->name);       
      l = MATCH_ERROR;        
      goto cleanup;         
    }    
    
    if (g95_match_char(')') == MATCH_YES) goto ok;        
        
    l = g95_match_char(',');  
    if (l != MATCH_YES) {      
      g95_error("Unexpected junk in formal argument list at %C");        
      goto cleanup;
    }   
  }      
      
ok:     
  /* Check for duplicate symbols in the formal argument list */        
        
  if (h != NULL) {         
    for(i=h; i->next; i=i->next) {       
      if (i->sym == NULL) continue;      
      
      for(w=i->next; w; w=w->next)          
	if (i->sym == w->sym) {        
	  g95_error("Duplicate symbol '%s' in formal argument list at %C",   
		    i->sym->name); 
 
	  l = MATCH_ERROR;   
	  goto cleanup;
	}        
    }          
  }

  if (g95_add_explicit_interface(progname, IFSRC_DECL, h, NULL)==FAILURE) {          
    l = MATCH_ERROR;       
    goto cleanup;
  }    
    
  return MATCH_YES;    
    
cleanup: 
  g95_free_formal_arglist(h);          
  return l;
}      
      
      
     
     
/* g95_match_end()-- Match any of the various end-block statements.
 * Returns the type of END to the caller.  The END INTERFACE, END IF,
 * END DO and END SELECT statements cannot be replaced by a single END
 * statement. */       
       
match g95_match_end(g95_statement *st0) {
char nam[G95_MAX_SYMBOL_LEN+1];       
g95_compile_state stat;      
char *block_name;
locus o;          
char *target;      
match h;       
       
  o = *g95_current_locus();       
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
    *st0 = ST_END_PROGRAM;       
    target = " program";    
    break; 
 
  case COMP_SUBROUTINE:    
    *st0 = ST_END_SUBROUTINE;   
    target = " subroutine";      
    break;

  case COMP_FUNCTION:      
    *st0 = ST_END_FUNCTION; 
    target = " function"; 
    break;      
      
  case COMP_BLOCK_DATA:
    *st0 = ST_END_BLOCK_DATA;          
    target = " block data";   
    break;          
          
  case COMP_MODULE:
    *st0 = ST_END_MODULE;     
    target = " module";       
    break; 
 
  case COMP_INTERFACE:       
    *st0 = ST_END_INTERFACE;
    target = " interface";
    break;    
    
  case COMP_DERIVED:    
    *st0 = ST_END_TYPE;
    target = " type"; 
    break;         
         
  case COMP_IF:          
    *st0 = ST_ENDIF;  
    target = " if";  
    break;     
     
  case COMP_DO: 
    *st0 = ST_ENDDO;   
    target = " do";        
    break;          
          
  case COMP_SELECT:     
    *st0 = ST_END_SELECT;         
    target = " select";          
    break;       
       
  case COMP_FORALL:      
    *st0 = ST_END_FORALL;       
    target = " forall";    
    break;  
  
  case COMP_WHERE:  
    *st0 = ST_END_WHERE;          
    target = " where";        
    break;          
          
  default:          
    g95_error("Unexpected END statement at %C");  
    goto cleanup;          
  }

  if (g95_match_eos() == MATCH_YES) {          
          
    if (*st0 == ST_ENDIF || *st0 == ST_ENDDO || *st0 == ST_END_SELECT ||          
	*st0 == ST_END_INTERFACE || *st0 == ST_END_FORALL ||   
	*st0 == ST_END_WHERE) {          
          
      g95_error("%s statement expected at %C", g95_ascii_statement(*st0));    
      goto cleanup;      
    }  
  
    return MATCH_YES;       
  }  
  
/* Verify that we've got the sort of end-block that we're expecting */    
    
  if (g95_match(target) != MATCH_YES) {    
    g95_error("Expecting %s statement at %C", g95_ascii_statement(*st0));   
    goto cleanup;        
  }    
    
/* If we're at the end, make sure a block name wasn't required */       
       
  if (g95_match_eos() == MATCH_YES) {       
       
    if (*st0 != ST_ENDDO && *st0 != ST_ENDIF && *st0 != ST_END_SELECT)         
      return MATCH_YES;  
  
    if (g95_current_block() == NULL) return MATCH_YES;  
  
    g95_error("Expected block name of '%s' in %s statement at %C",          
	      block_name, g95_ascii_statement(*st0));     
     
    return MATCH_ERROR;
  }         
         
/* END INTERFACE has a special handler for its several possible endings */

  if (*st0 == ST_END_INTERFACE) return g95_match_end_interface();  
  
/* We haven't hit the end of statement, so what is left must be an end-name */         
         
  h = g95_match_space(); 
  if (h == MATCH_YES) h = g95_match_name(nam);        
        
  if (h == MATCH_NO) g95_error("Expected terminating name at %C");         
  if (h != MATCH_YES) goto cleanup;   
   
  if (block_name == NULL) goto syntax;

  if (strcmp(nam, block_name) != 0) { 
    g95_error("Expected label '%s' for %s statement at %C", block_name,      
	      g95_ascii_statement(*st0));    
    goto cleanup;     
  }

  if (g95_match_eos() == MATCH_YES) return MATCH_YES;          
          
syntax:       
  g95_syntax_error(*st0);

cleanup: 
  g95_set_locus(&o);     
  return MATCH_ERROR;         
}          
          
          
        
        
match g95_match_allocatable(void) {  
  
  g95_clear_attr(&current_attr);    
  g95_add_allocatable(&current_attr, NULL);    
    
  return attr_decl();         
}          
          
          


match g95_match_target(void) {         
         
  g95_clear_attr(&current_attr);  
  g95_add_target(&current_attr, NULL);        
        
  return attr_decl();
}       
       
       
   
   
/* g95_match_private()-- The PRIVATE statement is a bit weird in that
 * it can be a attribute declaration, but also works as a standlone
 * statement inside of a type declaration or a module. */   
   
match g95_match_private(g95_statement *sta) {      
      
  if (g95_match("private") != MATCH_YES) return MATCH_NO;         
         
  if (g95_current_state() == COMP_DERIVED) {  
    if (g95_match_eos() == MATCH_YES) {   
      *sta = ST_PRIVATE;      
      return MATCH_YES;       
    }  
  
    g95_syntax_error(ST_PRIVATE); 
    return MATCH_ERROR;       
  }    
    
  if (g95_match_eos() == MATCH_YES) {  
    *sta = ST_PRIVATE;   
    return MATCH_YES;       
  }       
       
  *sta = ST_ATTR_DECL; 
  return access_attr_decl(ST_PRIVATE);   
} 
 
 
      
      
match g95_match_intent(void) {
sym_intent intent;          
          
  intent = match_intent_spec();   
  if (intent == INTENT_UNKNOWN) return MATCH_ERROR;

  g95_clear_attr(&current_attr); 
  g95_add_intent(&current_attr, intent, NULL);   /* Can't fail */      
      
  return attr_decl();        
}  
  
  
  
  
/* do_parm()-- Workhorse for g95_match_parameter */        
        
static match do_parm(void) {         
g95_symbol *s;      
g95_expr *i;     
match e;   
   
  e = g95_match_symbol(&s, 0);    
  if (e == MATCH_NO)
    g95_error("Expected variable name at %C in PARAMETER statement"); 
 
  if (e != MATCH_YES) return e;         
         
  if (g95_match_char('=') == MATCH_NO) {    
    g95_error("Expected = sign in PARAMETER statement at %C");     
    return MATCH_ERROR;      
  }   
   
  e = g95_match_init_expr(&i);  
  if (e == MATCH_NO)
    g95_error("Expected expression at %C in PARAMETER statement");
  if (e != MATCH_YES) return e;       
       
  if (s->ts.type == BT_UNKNOWN &&  
      g95_set_default_type(s, 1, NULL) == FAILURE) {   
    e = MATCH_ERROR;          
    goto cleanup;     
  }         
         
  if (g95_check_assign_symbol(s, i) == FAILURE ||        
      g95_add_flavor(&s->attr, FL_PARAMETER, NULL) == FAILURE) {       
    e = MATCH_ERROR;    
    goto cleanup;          
  }          
          
  s->value = i;   
  return MATCH_YES;        
        
cleanup:
  g95_free_expr(i);         
  return e;   
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
     
     
 
 
/* g95_match_parameter()-- Match a parameter statement, with the weird
 * syntax that these have */        
        
match g95_match_parameter(void) {    
match d;      
      
  if (g95_match_char('(') == MATCH_NO) return MATCH_NO;     
     
  for(;;) { 
    d = do_parm();          
    if (d != MATCH_YES) break;  
  
    if (g95_match(" )%t") == MATCH_YES) break;    
    
    if (g95_match_char(',') != MATCH_YES) {     
      g95_error("Unexpected characters in PARAMETER statement at %C");
      d = MATCH_ERROR;
      break;  
    }     
  }       
       
  return d;       
}        
        
        
