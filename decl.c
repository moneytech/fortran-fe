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
        
        
        
        
/* find_special()-- Special subroutine for finding a symbol.  If we're
 * compiling a function or subroutine and the parent compilation unit
 * is an interface, then check to see if the name we've been given is
 * the name of the interface (located in another namespace).  If so,
 * return that symbol.  If not, use g95_get_symbol(). */

static int find_special(char *n, g95_symbol **res) {         
g95_state_data *z;    
    
  if (g95_current_state() != COMP_SUBROUTINE &&       
      g95_current_state() != COMP_FUNCTION) goto normal;  
  
  z = g95_state_stack->previous;        
  if (z == NULL) goto normal;

  if (z->state != COMP_INTERFACE) goto normal;
  if (z->sym == NULL) goto normal;   /* Nameless interface */       
       
  if (strcmp(n, z->sym->name) == 0) {     
    *res = z->sym;        
    return 0;      
  }          
          
normal:      
  return g95_get_symbol(n, NULL, res);       
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
       
       
          
          
/* build_struct()-- Function called by variable_decl() that adds a
 * name to a structure being built. */      
      
static try build_struct(char *name, g95_charlen *charlen, g95_expr **init,    
			g95_array_spec **as) {
g95_component *a;         
         
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
     
  if (g95_add_component(g95_current_block(), name, &a) == FAILURE)   
    return FAILURE;  
  
  a->ts = current_ts;          
  a->ts.cl = charlen;       
  g95_set_component_attr(a, &current_attr);          
          
  a->initializer = *init;       
  *init = NULL;   
   
  a->as = *as;       
  if (a->as != NULL) a->dimension = 1;
  *as = NULL;     
     
  /* Check array components */          
          
  if (!a->dimension) return SUCCESS;      
      
  if (a->pointer) {
    if (a->as->type != AS_DEFERRED){          
      g95_error("Pointer array component of structure at %C "  
		"must have a deferred shape");   
      return FAILURE;        
    }         
  } else {  
    if (a->as->type != AS_EXPLICIT) { 
      g95_error("Array component of structure at %C must have an explicit "   
		"shape");   
      return FAILURE;  
    }  
  }         
         
  return SUCCESS;
}      
      
      
          
          
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
    
locus start, seen_at[NUM_DECL];        
int b, seen[NUM_DECL];          
decl_types p;   
char *a;       
match j;        
try v;    
    
  g95_clear_attr(&current_attr);      
  start = *g95_current_locus();          
          
  current_as = NULL;  
  
/* See if we get all of the keywords up to the final double colon */   
   
  for(b=0; b<NUM_DECL; b++)       
    seen[b] = 0;       
       
  for(;;) {   
    p = g95_match_strings(decls);         
    if (p == DECL_NONE || p == DECL_COLON) break;

    seen[p]++;   
    seen_at[p] = *g95_current_locus();          
          
    if (p == DECL_DIMENSION) {    
      j = g95_match_array_spec(&current_as);      
      
      if (j == MATCH_NO) {       
	g95_error("Missing dimension specification at %C");    
	j = MATCH_ERROR;  
      }        
        
      if (j == MATCH_ERROR) goto cleanup;       
    }    
  }

/* No double colon, so assume that we've been looking at something
 * else the whole time */         
         
  if (p == DECL_NONE) {    
    j = MATCH_NO; 
    goto cleanup;       
  }          
          
/* Since we've seen a double colon, we have to be looking at an
 * attr-spec.  This means that we can now issue errors */  
  
  for(p=0; p<NUM_DECL; p++)      
    if (seen[p] > 1) {     
      switch(p) {
      case DECL_ALLOCATABLE:  a = "ALLOCATABLE";      break;     
      case DECL_DIMENSION:    a = "DIMENSION";        break;        
      case DECL_EXTERNAL:     a = "EXTERNAL";         break;         
      case DECL_IN:           a = "INTENT (IN)";      break;         
      case DECL_OUT:          a = "INTENT (OUT)";     break;   
      case DECL_INOUT:        a = "INTENT (IN OUT)";  break;          
      case DECL_INTRINSIC:    a = "INTRINSIC";        break;
      case DECL_OPTIONAL:     a = "OPTIONAL";         break;      
      case DECL_PARAMETER:    a = "PARAMETER";        break;    
      case DECL_POINTER:      a = "POINTER";          break;       
      case DECL_PRIVATE:      a = "PRIVATE";          break;    
      case DECL_PUBLIC:       a = "PUBLIC";           break; 
      case DECL_SAVE:         a = "SAVE";             break;          
      case DECL_TARGET:       a = "TARGET";           break;
      default:     
	a = NULL;  /* This shouldn't happen */       
      }  
  
      g95_error("Duplicate %s attribute at %L", a, &seen_at[p]);      
      j = MATCH_ERROR;
      goto cleanup;       
    }          
          
/* Now that we've dealt with duplicate attributes, add the attributes to the 
 * current attribute. */        
        
  for(p=0; p<NUM_DECL; p++) {        
    if (seen[p] == 0) continue;          
          
    if (g95_current_state() == COMP_DERIVED &&
	p != DECL_DIMENSION && p != DECL_POINTER &&   
	p != DECL_COLON && p != DECL_NONE) {    
    
      g95_error("Attribute at %L is not allowed in a TYPE definition",        
		&seen_at[p]);         
      j = MATCH_ERROR;    
      goto cleanup;       
    }

    switch(p) {     
    case DECL_ALLOCATABLE:     
      v = g95_add_allocatable(&current_attr, &seen_at[p]); 
      break;          
          
    case DECL_DIMENSION:  
      v = g95_add_dimension(&current_attr, &seen_at[p]);          
      break;

    case DECL_EXTERNAL:  
      v = g95_add_external(&current_attr, &seen_at[p]);  
      break;    
    
    case DECL_IN:       
      v = g95_add_intent(&current_attr, INTENT_IN, &seen_at[p]);  
      break;         
         
    case DECL_OUT:   
      v = g95_add_intent(&current_attr, INTENT_OUT, &seen_at[p]);       
      break;  
  
    case DECL_INOUT:        
      v = g95_add_intent(&current_attr, INTENT_INOUT, &seen_at[p]);       
      break;        
        
    case DECL_INTRINSIC:        
      v = g95_add_intrinsic(&current_attr, &seen_at[p]);
      break; 
 
    case DECL_OPTIONAL:  
      v = g95_add_optional(&current_attr, &seen_at[p]);       
      break;        
        
    case DECL_PARAMETER: 
      v = g95_add_flavor(&current_attr, FL_PARAMETER, &seen_at[p]);        
      break;         
         
    case DECL_POINTER:   
      v = g95_add_pointer(&current_attr, &seen_at[p]);          
      break;      
      
    case DECL_PRIVATE:        
      v = g95_add_access(&current_attr, ACCESS_PRIVATE, &seen_at[p]);          
      break;        
        
    case DECL_PUBLIC:          
      v = g95_add_access(&current_attr, ACCESS_PUBLIC, &seen_at[p]);     
      break;      
      
    case DECL_SAVE:   
      v = g95_add_save(&current_attr, &seen_at[p]);         
      break;       
       
    case DECL_TARGET:
      v = g95_add_target(&current_attr, &seen_at[p]);          
      break;      
      
    default: 
      g95_internal_error("match_attr_spec(): Bad attribute");         
    }          
          
    if (v == FAILURE) {  
      j = MATCH_ERROR;    
      goto cleanup;
    }
  }    
    
  seen_colon = 1;  
  return MATCH_YES;          
          
cleanup:
  g95_set_locus(&start); 
  g95_free_array_spec(current_as);   
  current_as = NULL;    
  return j;
}        
        
        
          
          
/* get_proc_name()-- Special subroutine for getting a symbol node
 * associated with a procedure name, used in SUBROUTINE and FUNCTION
 * statements.  The symbol is created in the parent using with symtree
 * node in the child unit pointing to the symbol.  If the current
 * namespace has no parent, then the symbol is just created in the
 * current unit. */      
      
static int get_proc_name(char *name, g95_symbol **rslt) {          
g95_symtree *st1;         
g95_symbol *sy; 
int retval;      
      
  if (g95_current_ns->parent == NULL)    
    return g95_get_symbol(name, NULL, rslt);        
    
  retval = g95_get_symbol(name, g95_current_ns->parent, rslt);     
  if (*rslt == NULL) return retval;       
       
  st1 = g95_find_symtree(g95_current_ns->sym_root, name);   
  if (st1 == NULL) {         
    st1 = g95_new_symtree(&g95_current_ns->sym_root, name);        
        
    sy = *rslt;  
    st1->n.sym = sy;       
    sy->refs++;      
  }          
          
  return retval;   
}      
      
      
       
       
/* g95_match_null()-- Match a 'NULL()', and possibly take care of some
 * side effects. */       
       
match g95_match_null(g95_expr **r) {
g95_symbol *sy;     
g95_expr *v;          
match l;          
          
  l = g95_match(" null ( )");      
  if (l != MATCH_YES) return l;      
      
  /* The NULL symbol now has to be/become an intrinsic function */         
         
  if (g95_get_symbol("null", NULL, &sy)) { 
    g95_error("NULL() initialization at %C is ambiguous");     
    return MATCH_ERROR; 
  }   
   
  if (g95_intrinsic_symbol(sy, 1)) return MATCH_ERROR;       
       
  v = g95_get_expr();  
  v->where = *g95_current_locus();     
  v->type = EXPR_NULL;        
  v->ts.type = BT_UNKNOWN; 
 
  *r = v;          
          
  return MATCH_YES;  
}        
        
        
       
       
/* g95_match_save()-- Save statements have a special syntax */          
          
match g95_match_save(void) {          
char nm[G95_MAX_SYMBOL_LEN+1];    
g95_common_head *a;   
g95_symbol *symbol;      
match y;       
       
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
    y = g95_match_symbol(&symbol, 0);       
    switch(y) {         
    case MATCH_YES:
      if (g95_add_save(&symbol->attr, g95_current_locus()) == FAILURE)     
	return MATCH_ERROR;    
      goto next_item;       
       
    case MATCH_NO:     
      break; 
 
    case MATCH_ERROR:      
      return MATCH_ERROR;
    }       
       
    y = g95_match(" / %n /", &nm);         
    if (y == MATCH_ERROR) return MATCH_ERROR;         
    if (y == MATCH_NO) goto syntax;       
       
    a = g95_get_common(nm);          
          
    if (a->use_assoc) {          
      g95_error("COMMON block '%s' at %C is already USE associated", nm);     
      return MATCH_ERROR;         
    }   
   
    a->saved = 1;      
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
     
     
     
     
/* g95_match_old_kind_spec()-- Match an extended-f77 kind specification */        
        
match g95_match_old_kind_spec(g95_typespec *typ) {          
match j;
  
  if (g95_match_char('*') != MATCH_YES) return MATCH_NO;          
          
  j = g95_match_small_literal_int(&typ->kind);    
  if (j != MATCH_YES) return MATCH_ERROR;

/* Massage the kind numbers for complex types */    
    
  if (typ->type == BT_COMPLEX && typ->kind == 8) typ->kind = 4; 
  if (typ->type == BT_COMPLEX && typ->kind == 16) typ->kind = 8;       
       
  if (g95_validate_kind(typ->type, typ->kind) == -1) {    
    g95_error("Old-style kind %d not supported for type %s at %C", typ->kind,  
	      g95_basic_typename(typ->type));    
    
    return MATCH_ERROR;          
  }

  return MATCH_YES;  
}         
         
         


/* g95_match_derived_decl()-- Match the beginning of a derived type
 * declaration.  If a type name was the result of a function, then it is
 * possible to have a symbol already to be known as a derived type yet
 * have no components. */     
     
match g95_match_derived_decl(void) {  
char nm[G95_MAX_SYMBOL_LEN+1];   
symbol_attribute attr;         
g95_symbol *sy;     
match t;       
       
  if (g95_current_state() == COMP_DERIVED) return MATCH_NO;  
  
  g95_clear_attr(&attr);          
          
loop:   
  if (g95_match(" , private") == MATCH_YES) {          
    if (g95_find_state(COMP_MODULE) == FAILURE) {       
      g95_error("Derived type at %C can only be PRIVATE within a MODULE");   
      return MATCH_ERROR;     
    }     
     
    if (g95_add_access(&attr, ACCESS_PRIVATE, NULL) == FAILURE)
      return MATCH_ERROR;
    goto loop;      
  }        
        
  if (g95_match(" , public") == MATCH_YES) {      
    if (g95_find_state(COMP_MODULE) == FAILURE) {   
      g95_error("Derived type at %C can only be PUBLIC within a MODULE");         
      return MATCH_ERROR; 
    }   
   
    if (g95_add_access(&attr, ACCESS_PUBLIC, NULL) == FAILURE)          
      return MATCH_ERROR;      
    goto loop;     
  }       
       
  if (g95_match(" ::") != MATCH_YES && attr.access != ACCESS_UNKNOWN) {          
    g95_error("Expected :: in TYPE definition at %C");        
    return MATCH_ERROR;     
  } 
 
  t = g95_match(" %n%t", nm);    
  if (t != MATCH_YES) return t;        
        
/* Make sure the name isn't the name of an intrinsic type.  The
 * 'double precision' type doesn't get past the name matcher */     
     
  if (strcmp(nm, "integer") == 0   || strcmp(nm, "real") == 0 ||         
      strcmp(nm, "character") == 0 || strcmp(nm, "logical") == 0 ||     
      strcmp(nm, "complex") == 0) {    
    g95_error("Type name '%s' at %C cannot be the same as an intrinsic type",      
	      nm);       
    return MATCH_ERROR;         
  }          
          
  if (g95_get_symbol(nm, NULL, &sy)) return MATCH_ERROR;      
      
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

  if (attr.access != ACCESS_UNKNOWN &&         
      g95_add_access(&sy->attr, attr.access, NULL) == FAILURE)         
    return MATCH_ERROR;   
   
  g95_new_block = sy;        
        
  return MATCH_YES; 
}

      
      
/* match_prefix()-- Match a prefix associated with a function or
 * subroutine declaration.  If the typespec pointer is nonnull, then a
 * typespec can be matched.  Note that if nothing matches, MATCH_YES
 * is returned (the null string was matched). */   
   
static match match_prefix(g95_typespec *t) {      
int seen_type;     
     
  g95_clear_attr(&current_attr);   
  seen_type = 0;    
    
loop:   
  if (!seen_type && t != NULL &&         
      g95_match_type_spec(t, 1) == MATCH_YES && 
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
          
static try copy_prefix(symbol_attribute *dest, locus *old_loc) {         
         
  if (current_attr.pure && g95_add_pure(dest, old_loc) == FAILURE)        
    return FAILURE;

  if (current_attr.elemental && g95_add_elemental(dest, old_loc) == FAILURE)          
    return FAILURE;

  if (current_attr.recursive && g95_add_recursive(dest, old_loc) == FAILURE)    
    return FAILURE;          
          
  return SUCCESS;          
} 
 
 
          
          
/* g95_match_end()-- Match any of the various end-block statements.
 * Returns the type of END to the caller.  The END INTERFACE, END IF,
 * END DO and END SELECT statements cannot be replaced by a single END
 * statement. */        
        
match g95_match_end(g95_statement *st) {   
char name[G95_MAX_SYMBOL_LEN+1];     
g95_compile_state stat;      
char *block_name;  
locus o;        
char *target;     
match y;       
       
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
    *st = ST_END_PROGRAM;       
    target = " program";  
    break;          
          
  case COMP_SUBROUTINE:   
    *st = ST_END_SUBROUTINE;         
    target = " subroutine"; 
    break;          
          
  case COMP_FUNCTION:
    *st = ST_END_FUNCTION;         
    target = " function";    
    break;         
         
  case COMP_BLOCK_DATA:          
    *st = ST_END_BLOCK_DATA;   
    target = " block data";    
    break;        
        
  case COMP_MODULE:         
    *st = ST_END_MODULE; 
    target = " module";    
    break; 
 
  case COMP_INTERFACE:          
    *st = ST_END_INTERFACE;   
    target = " interface";         
    break;  
  
  case COMP_DERIVED:         
    *st = ST_END_TYPE;         
    target = " type";          
    break;          
          
  case COMP_IF:      
    *st = ST_ENDIF;          
    target = " if";     
    break;         
         
  case COMP_DO:       
    *st = ST_ENDDO;   
    target = " do";    
    break; 
 
  case COMP_SELECT:      
    *st = ST_END_SELECT;
    target = " select";         
    break;       
       
  case COMP_FORALL:      
    *st = ST_END_FORALL;       
    target = " forall";     
    break;    
    
  case COMP_WHERE:        
    *st = ST_END_WHERE;          
    target = " where";       
    break;       
       
  default:         
    g95_error("Unexpected END statement at %C");      
    goto cleanup;       
  }     
     
  if (g95_match_eos() == MATCH_YES) {        
        
    if (*st == ST_ENDIF || *st == ST_ENDDO || *st == ST_END_SELECT ||        
	*st == ST_END_INTERFACE || *st == ST_END_FORALL ||   
	*st == ST_END_WHERE) {   
   
      g95_error("%s statement expected at %C", g95_ascii_statement(*st));  
      goto cleanup;   
    }     
     
    return MATCH_YES;
  }     
     
/* Verify that we've got the sort of end-block that we're expecting */         
         
  if (g95_match(target) != MATCH_YES) {     
    g95_error("Expecting %s statement at %C", g95_ascii_statement(*st));   
    goto cleanup;    
  }          
          
/* If we're at the end, make sure a block name wasn't required */         
         
  if (g95_match_eos() == MATCH_YES) {

    if (*st != ST_ENDDO && *st != ST_ENDIF && *st != ST_END_SELECT)   
      return MATCH_YES;     
     
    if (g95_current_block() == NULL) return MATCH_YES;    
    
    g95_error("Expected block name of '%s' in %s statement at %C",
	      block_name, g95_ascii_statement(*st));   
   
    return MATCH_ERROR;   
  }   
   
/* END INTERFACE has a special handler for its several possible endings */     
     
  if (*st == ST_END_INTERFACE) return g95_match_end_interface();  
  
/* We haven't hit the end of statement, so what is left must be an end-name */  
  
  y = g95_match_space();        
  if (y == MATCH_YES) y = g95_match_name(name);          
          
  if (y == MATCH_NO) g95_error("Expected terminating name at %C");        
  if (y != MATCH_YES) goto cleanup;      
      
  if (block_name == NULL) goto syntax;  
  
  if (strcmp(name, block_name) != 0) {
    g95_error("Expected label '%s' for %s statement at %C", block_name,       
	      g95_ascii_statement(*st));     
    goto cleanup;       
  }       
       
  if (g95_match_eos() == MATCH_YES) return MATCH_YES;      
      
syntax:  
  g95_syntax_error(*st);

cleanup:   
  g95_set_locus(&o);     
  return MATCH_ERROR;      
}       
       
       
   
   
/* build_sym()-- Function called by variable_decl() that adds a name
 * to the symbol table. */          
          
static try build_sym(char *nm, g95_charlen *c, g95_expr **initp,       
		     g95_array_spec **ar, locus *var_locus) {      
symbol_attribute attribute;  
g95_symbol *symb;   
g95_expr *in;     
int u; 
 
  in = *initp;       
  if (find_special(nm, &symb)) return FAILURE;    
    
/* Start updating the symbol table.  Add basic type attribute if present */        
        
  if (current_ts.type != BT_UNKNOWN &&          
      (symb->attr.implicit_type == 0 ||   
       !g95_compare_types(&symb->ts, &current_ts)) &&    
      g95_add_type(symb, &current_ts, var_locus) == FAILURE) return FAILURE;

/* If this variable declaration is confirming an implicit parameter
 * type, then an initialization expression is not allowed. */   
   
  if (symb->attr.flavor == FL_PARAMETER && *initp != NULL) {      
    g95_error("Initializer not allowed for PARAMETER '%s' at %C", symb->name);  
    return FAILURE;      
  }       
       
  if (symb->ts.type == BT_CHARACTER) {         
    symb->ts.cl = c; 
 
    if (c->length == NULL && !symb->attr.dummy && in != NULL &&      
	in->type == EXPR_ARRAY) {         
         
      u = g95_constructor_string_length; 
      if (u == -1) u = 0;

      c->length = g95_int_expr(u);  
    }  
  }   
   
/* Add dimension attribute if present. */     
     
  if (g95_set_array_spec(symb, *ar, var_locus) == FAILURE) return FAILURE;  
  *ar = NULL;         
         
/* Add attribute to symbol.  The copy is so that we can reset the
 * dimension attribute. */         
         
  attribute = current_attr;      
  attribute.dimension = 0;          
          
  if (g95_copy_attr(&symb->attr, &attribute, var_locus) == FAILURE) return FAILURE;

/* Add initializer, required for PARAMETERs. */ 
 
  if (in == NULL) { 
    if (attribute.flavor == FL_PARAMETER) {
      g95_error("PARAMETER at %L is missing an initializer", var_locus);    
      return FAILURE;
    }      
  } else {    
    if (symb->attr.data) {     
      g95_error("Variable '%s' at %C with an initializer already appears " 
		"in a DATA statement", symb->name);  
      return FAILURE;
    }      
      
    if (symb->attr.dimension && in->rank == 0) in->rank = symb->as->rank;      
      
    symb->value = in;          
    *initp = NULL;        
  }         
         
  return SUCCESS;  
}    
    
    
       
       
/* g95_match_formal_arglist()-- Match a formal argument list. */         
         
match g95_match_formal_arglist(g95_symbol *progname, int st_flag,        
			       int null_flag) {       
g95_formal_arglist *h, *end, *p, *n;       
char nam[G95_MAX_SYMBOL_LEN+1];       
g95_symbol *sy;       
match k;      
      
  h = end = NULL;  
  
  if (g95_match_char('(') != MATCH_YES) {    
    if (null_flag) goto ok;    
    return MATCH_NO;     
  }       
       
  if (g95_match_char(')') == MATCH_YES) goto ok;     
     
  for(;;) {          
    if (g95_match_char('*') == MATCH_YES)         
      sy = NULL;    
    else {         
      k = g95_match_name(nam);     
      if (k != MATCH_YES) goto cleanup;    
    
      if (g95_get_symbol(nam, NULL, &sy)) goto cleanup; 
    }       
       
    p = g95_get_formal_arglist();   
   
    if (h == NULL)         
      h = end = p;       
    else { 
      end->next = p;
      end = p;       
    } 
 
    end->sym = sy;     
     
/* We don't add the VARIABLE flavor because the name could be a dummy
 * procedure.  We don't apply these attributes to formal arguments of
 * statement functions. */     
     
    if (sy != NULL && !st_flag && 
	(g95_add_dummy(&sy->attr, NULL) == FAILURE)) {       
      k = MATCH_ERROR;         
      goto cleanup;        
    }  
  
/* The name of a program unit can be in a different namespace, so
 * check for it explicitly.  After the statement is accepted, the name
 * is checked for especially in g95_get_symbol(). */

    if (g95_new_block != NULL && sy != NULL &&
	strcmp(sy->name, g95_new_block->name) == 0) {    
      g95_error("Name '%s' at %C is the name of the procedure", sy->name);      
      k = MATCH_ERROR;          
      goto cleanup;         
    }   
   
    if (g95_match_char(')') == MATCH_YES) goto ok;         
         
    k = g95_match_char(',');     
    if (k != MATCH_YES) {            
      g95_error("Unexpected junk in formal argument list at %C");   
      goto cleanup;  
    }   
  }  
  
ok:   
  /* Check for duplicate symbols in the formal argument list */ 
 
  if (h != NULL) {          
    for(p=h; p->next; p=p->next) {     
      if (p->sym == NULL) continue;       
       
      for(n=p->next; n; n=n->next)       
	if (p->sym == n->sym) { 
	  g95_error("Duplicate symbol '%s' in formal argument list at %C",   
		    p->sym->name);    
    
	  k = MATCH_ERROR; 
	  goto cleanup;         
	}   
    }    
  }       
       
  if (g95_add_explicit_interface(progname, IFSRC_DECL, h, NULL)==FAILURE) {    
    k = MATCH_ERROR;          
    goto cleanup;   
  }      
      
  return MATCH_YES;       
       
cleanup:
  g95_free_formal_arglist(h);  
  return k;         
}          
          
          
          
          
/* match_result()-- Match a RESULT specification following a function
 * declaration or ENTRY statement.  Also matches the end-of-statement. */       
       
static match match_result(g95_symbol *function, g95_symbol **res) {        
char n[G95_MAX_SYMBOL_LEN+1]; 
g95_symbol *t;
match e;    
    
  if (g95_match(" result (") != MATCH_YES) return MATCH_NO;  
  
  e = g95_match_name(n);      
  if (e != MATCH_YES) return e;  
  
  if (g95_match(" )%t") != MATCH_YES) {          
    g95_error("Unexpected junk following RESULT variable at %C");        
    return MATCH_ERROR;        
  }   
   
  if (strcmp(function->name, n) == 0) {  
    g95_error("RESULT variable at %C must be different than function name");    
    return MATCH_ERROR;
  }          
          
  if (g95_get_symbol(n, NULL, &t)) return MATCH_ERROR;  
  
  if (g95_add_flavor(&t->attr, FL_VARIABLE, NULL) == FAILURE ||        
      g95_add_result(&t->attr, NULL) == FAILURE) return MATCH_ERROR;         
         
  *res = t;         
         
  return MATCH_YES;   
}         
         
         
         
         
/* g95_match_entry()-- Match an ENTRY statement */     
     
match g95_match_entry(void) {   
g95_symbol *function, *rslt, *entry;   
char n[G95_MAX_SYMBOL_LEN+1];   
g95_compile_state state;  
match o;   
   
  o = g95_match_name(n);     
  if (o != MATCH_YES) return o;     
     
  if (get_proc_name(n, &entry)) return MATCH_ERROR;  
  
  g95_enclosing_unit(&state);   
  switch(state) {
  case COMP_SUBROUTINE:  
    o = g95_match_formal_arglist(entry, 0, 1);
    if (o != MATCH_YES) return MATCH_ERROR;

    if (g95_current_state() != COMP_SUBROUTINE) goto exec_construct;      
      
    if (g95_add_entry(&entry->attr, NULL) == FAILURE ||        
	g95_add_subroutine(&entry->attr, NULL) == FAILURE)     
      return MATCH_ERROR;

    break;          
          
  case COMP_FUNCTION:
    o = g95_match_formal_arglist(entry, 0, 0);         
    if (o != MATCH_YES) return MATCH_ERROR;      
      
    if (g95_current_state() != COMP_FUNCTION) goto exec_construct;          
    function = g95_state_stack->sym;     
     
    rslt = NULL;  
  
    if (g95_match_eos() == MATCH_YES) {  
      if (g95_add_entry(&entry->attr, NULL) == FAILURE ||         
	  g95_add_function(&entry->attr, NULL) == FAILURE)          
	return MATCH_ERROR;        
        
      entry->result = entry;

    } else {     
      o = match_result(function, &rslt); 
      if (o == MATCH_NO) g95_syntax_error(ST_ENTRY);  
      if (o != MATCH_YES) return MATCH_ERROR;   
   
      if (g95_add_entry(&entry->attr, NULL) == FAILURE ||   
	  g95_add_function(&entry->attr, NULL) == FAILURE)         
	return MATCH_ERROR;     
    }    
    
    if (function->attr.recursive && rslt == NULL) {    
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
   
   
      
      
/* do_parm()-- Workhorse for g95_match_parameter */       
       
static match do_parm(void) {   
g95_symbol *sym;       
g95_expr *in;    
match o;    
    
  o = g95_match_symbol(&sym, 0);          
  if (o == MATCH_NO)     
    g95_error("Expected variable name at %C in PARAMETER statement");         
         
  if (o != MATCH_YES) return o;         
         
  if (g95_match_char('=') == MATCH_NO) {     
    g95_error("Expected = sign in PARAMETER statement at %C");
    return MATCH_ERROR;        
  }          
          
  if (g95_add_flavor(&sym->attr, FL_PARAMETER, NULL) == FAILURE)        
    return MATCH_ERROR;       
       
  o = g95_match_init_expr(&in);    
  if (o == MATCH_NO)  
    g95_error("Expected expression at %C in PARAMETER statement");
  if (o != MATCH_YES) return o;       
       
  sym->value = in;    
  return MATCH_YES;  
  
  g95_free_expr(in);    
  return o;    
}   
   
   
       
       
/* g95_match_kind_spec()-- Match a kind specification.  Since kinds
 * are generally optional, we usually return MATCH_NO if something
 * goes wrong.  If a "kind=" string is found, then we know we have an
 * error. */

match g95_match_kind_spec(g95_typespec *typesp) {     
locus pos;  
g95_expr *t;      
match k, u;    
char *message;        
        
  k = MATCH_NO;      
  t = NULL;         
         
  pos = *g95_current_locus();

  if (g95_match_char('(') == MATCH_NO) return MATCH_NO;          
          
/* Also gobbles optional text */  
  if (g95_match(" kind = ") == MATCH_YES) k = MATCH_ERROR;      
      
  u = g95_match_init_expr(&t);        
  if (u == MATCH_NO) g95_error("Expected initialization expression at %C");         
  if (u != MATCH_YES) return MATCH_ERROR;      
      
  if (t->rank != 0) { 
    g95_error("Expected scalar initialization expression at %C");         
    k = MATCH_ERROR; 
    goto no_match; 
  }   
   
  message = g95_extract_int(t, &typesp->kind);         
  if (message != NULL) {    
    g95_error(message);    
    k = MATCH_ERROR;          
    goto no_match;  
  }        
        
  g95_free_expr(t);       
  t = NULL;     
     
  if (g95_validate_kind(typesp->type, typesp->kind) == -1) {    
    g95_error("Kind %d not supported for type %s at %C", typesp->kind,     
	      g95_basic_typename(typesp->type));         
         
    k = MATCH_ERROR;         
    goto no_match;  
  }     
     
  if (g95_match_char(')') != MATCH_YES) {   
    g95_error("Missing right paren at %C");     
    goto no_match;        
  } 
 
  return MATCH_YES;       
       
no_match:        
  g95_free_expr(t);    
  g95_set_locus(&pos);    
  return k; 
}    
    
    
          
          
/* g95_match_modproc()-- Match a module procedure statement.  Note
 * that we have to modify symbols in the parent's namespace because
 * the current one was there to receive symbols that are in a
 * interface's formal argument list. */  
  
match g95_match_modproc(void) {         
char nm[G95_MAX_SYMBOL_LEN+1];  
g95_symbol *symbol;     
match g;     
     
  if (g95_state_stack->state != COMP_INTERFACE ||     
      g95_state_stack->previous == NULL ||       
      current_interface.type == INTERFACE_NAMELESS) {          
    g95_error("MODULE PROCEDURE at %C must be in a generic module interface");      
    return MATCH_ERROR;    
  }   
   
  for(;;) {      
    g = g95_match_name(nm);  
    if (g == MATCH_NO) goto syntax;        
    if (g != MATCH_YES) return MATCH_ERROR;       
       
    if (g95_get_symbol(nm, g95_current_ns->parent, &symbol))      
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
          
          
 
 
/* g95_match_function_decl()-- Match a function declaration */

match g95_match_function_decl(void) {       
char nam[G95_MAX_SYMBOL_LEN+1];    
g95_symbol *symbol, *result; 
locus old_loc;        
match h;       
       
  if (g95_current_state() != COMP_NONE && 
      g95_current_state() != COMP_INTERFACE &&
      g95_current_state() != COMP_CONTAINS) return MATCH_NO;   
   
  g95_clear_ts(&current_ts); 
 
  old_loc = *g95_current_locus();     
     
  h = match_prefix(&current_ts);  
  if (h != MATCH_YES) {   
    g95_set_locus(&old_loc);        
    return h;    
  }        
        
  if (g95_match("function% %n", nam) != MATCH_YES) {   
    g95_set_locus(&old_loc);          
    return MATCH_NO;  
  }      
      
  if (get_proc_name(nam, &symbol)) return MATCH_ERROR;      
  g95_new_block = symbol;  
  
  h = g95_match_formal_arglist(symbol, 0, 0);        
  if (h == MATCH_NO)          
    g95_error("Expected formal argument list in function definition at %C");
  if (h != MATCH_YES) goto cleanup;   
   
  result = NULL;        
        
  if (g95_match_eos() != MATCH_YES) { /* See if a result variable is present */  
    h = match_result(symbol, &result);     
    if (h == MATCH_NO)   
      g95_error("Unexpected junk after function declaration at %C");   
   
    if (h != MATCH_YES) {         
      h = MATCH_ERROR;         
      goto cleanup;
    }      
  }     
     
/* Make changes to the symbol */    
    
  h = MATCH_ERROR;       
       
  if (g95_add_function(&symbol->attr, NULL) == FAILURE) goto cleanup;       
       
  if (copy_prefix(&symbol->attr, &symbol->declared_at) == FAILURE) goto cleanup;         
         
  if (current_ts.type != BT_UNKNOWN && symbol->ts.type != BT_UNKNOWN) {       
    g95_error("Function '%s' at %C already has a type of %s", nam,      
	      g95_basic_typename(symbol->ts.type));  
    goto cleanup;         
  }   
   
  if (result == NULL) {
    symbol->ts = current_ts;  
    symbol->result = symbol;   
  } else {    
    result->ts = current_ts;       
    symbol->result = result;  
  }         
         
  return MATCH_YES;          
          
cleanup:  
  g95_reject_statement();    
  g95_set_locus(&old_loc);  
  return h;         
}   
   
   
          
          
/* match_char_length()-- A character length is a '*' followed by a
 * literal integer or a char_len_param_value in parenthesis. */         
         
static match match_char_length(g95_expr **exp) {      
int len; 
match p;  
  
  p = g95_match_char('*');  
  if (p != MATCH_YES) return p;   
   
  p = g95_match_small_literal_int(&len); 
  if (p == MATCH_ERROR) return p; 
 
  if (p == MATCH_YES) {    
    *exp = g95_int_expr(len);         
    return p;
  } 
 
  if (g95_match_char('(') == MATCH_NO) goto syntax;      
      
  p = char_len_param_value(exp);
  if (p == MATCH_ERROR) return p;
  if (p == MATCH_NO) goto syntax;         
         
  if (g95_match_char(')') == MATCH_NO) {  
    g95_free_expr(*exp); 
    *exp = NULL;      
    goto syntax;   
  }     
     
  return MATCH_YES;        
        
syntax:      
  g95_error("Syntax error in character length specification at %C");    
  return MATCH_ERROR;       
}  
  
  
         
         
/* variable_decl()-- Match a variable name with an optional
 * initializer.  When this subroutine is called, a variable is
 * expected to be parsed next.  Depending on what is happening at the
 * moment, updates either the symbol table or the current
 * interface. */ 
 
static match variable_decl(void) {   
char name0[G95_MAX_SYMBOL_LEN+1];   
g95_expr *initializer, *char_len;       
g95_array_spec *spec;     
g95_charlen *c;
locus var_locus;        
match y;        
try k;         
         
  initializer = NULL;       
  spec = NULL;        
        
  y = g95_match_name(name0);          
  if (y != MATCH_YES) goto cleanup;   
   
  var_locus = *g95_current_locus();     
     
  y = g95_match_array_spec(&spec);  
  if (y == MATCH_ERROR) goto cleanup;     
  if (y == MATCH_NO) spec = g95_copy_array_spec(current_as);   
   
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
        
      /* Assumed length charlen nodes must be distinct */          
      if (c->length == NULL) {   
	c = g95_get_charlen();  
  
	c->next = g95_current_ns->cl_list;     
	g95_current_ns->cl_list = c;       
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
	y = MATCH_ERROR;         
	goto cleanup; 
      }          
          
      y = g95_match_null(&initializer);  
      if (y == MATCH_NO) {
	g95_error("Pointer initialization requires a NULL at %C");        
	y = MATCH_ERROR;      
      }        
        
      if (g95_pure(NULL)) {         
	g95_error("Initialization of pointer at %C is not allowed in a "       
		  "PURE procedure"); 
	y = MATCH_ERROR;
      }

      if (y != MATCH_YES) goto cleanup;      
      
      initializer->ts = current_ts; 
 
    } else if (g95_match_char('=') == MATCH_YES) {         
      if (current_attr.pointer) { 
	g95_error("Pointer initialization at %C requires '=>', not '='");          
	y = MATCH_ERROR;     
	goto cleanup; 
      }

      y = g95_match_init_expr(&initializer);         
      if (y == MATCH_NO) {     
	g95_error("Expected an initialization expression at %C");  
	y = MATCH_ERROR;       
      }   
   
      if (current_attr.flavor != FL_PARAMETER && g95_pure(NULL)) {   
	g95_error("Initialization of variable at %C is not allowed in a "    
		  "PURE procedure");         
	y = MATCH_ERROR;  
      } 
       
      if (y != MATCH_YES) goto cleanup;          
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
    y = MATCH_ERROR;        
    goto cleanup;
  }        
        
  if (g95_current_state() == COMP_DERIVED)   
    k = build_struct(name0, c, &initializer, &spec);
  else 
    k = build_sym(name0, c, &initializer, &spec, &var_locus);          
          
  y = (k == SUCCESS) ? MATCH_YES : MATCH_ERROR;          
          
/* Free stuff up and return */    
    
cleanup: 
  g95_free_expr(initializer); 
  g95_free_array_spec(spec);     
     
  return y;   
}  
  
  
       
       
/* attr_decl1()-- Function that sets the attribute of a single variable */ 
 
static match attr_decl1(void) {      
char name0[G95_MAX_SYMBOL_LEN+1];       
g95_array_spec *a;     
g95_symbol *s;       
locus var_locus; 
match h; 
 
  a = NULL;     
     
  h = g95_match_name(name0); 
  if (h != MATCH_YES) goto cleanup;    
    
  if (find_special(name0, &s)) return MATCH_ERROR;

  var_locus = *g95_current_locus();       
       
/* Deal with possible array specification for certain attributes */ 
 
  if (current_attr.dimension || current_attr.allocatable ||          
      current_attr.pointer   || current_attr.target) {   
    h = g95_match_array_spec(&a);         
    if (h == MATCH_ERROR) goto cleanup;          
          
    if (current_attr.dimension && h == MATCH_NO) {       
	g95_error("Missing array specification at %L in DIMENSION statement", 
		  &var_locus);     
	h = MATCH_ERROR;         
	goto cleanup;          
    }   
   
    if ((current_attr.allocatable || current_attr.pointer) &&    
	  (h == MATCH_YES) && (a->type != AS_DEFERRED)) {      
      g95_error("Array specification must be deferred at %L",       
		&var_locus);         
      h = MATCH_ERROR;        
      goto cleanup;        
    }    
  }  
  
/* Update symbol table.  DIMENSION attribute is set in g95_set_array_spec(). */      
      
  if (current_attr.dimension == 0 &&         
      g95_copy_attr(&s->attr, &current_attr, NULL) == FAILURE) {    
    h = MATCH_ERROR;  
    goto cleanup;     
  }      
      
  if (g95_set_array_spec(s, a, &var_locus) == FAILURE) {    
    h = MATCH_ERROR;   
    goto cleanup;         
  }

  if ((current_attr.external || current_attr.intrinsic) &&     
      s->attr.flavor != FL_PROCEDURE &&     
      g95_add_flavor(&s->attr, FL_PROCEDURE, NULL) == FAILURE) {    
    h = MATCH_ERROR;   
    goto cleanup;     
  }      
      
  return MATCH_YES;      
      
cleanup:   
  g95_free_array_spec(a);  
  return h;  
} 
 
 
          
          
/* match_char_spec()-- Match the various kind/length specifications in
 * a CHARACTER declaration.  We don't return MATCH_NO. */     
     
static match match_char_spec(g95_typespec *t) {   
int w, kind, seen_length;         
g95_charlen *charlen;         
g95_expr *leng;     
match u;    
    
  kind = g95_default_character_kind();  
  leng = NULL;       
  seen_length = 0;    
    
/* Try the old-style specification first */      
      
  old_char_selector = 0;      
      
  u = match_char_length(&leng);        
  if (u != MATCH_NO) {
    if (u == MATCH_YES) old_char_selector = 1; 
    seen_length = 1;
    goto done;    
  }

  u = g95_match_char('(');     
  if (u != MATCH_YES) {
    u = MATCH_YES;  /* character without length is a single char */       
    goto done;        
  }    
    
/* Try the weird case:  ( KIND = <int> [ , LEN = <len-param> ] )   */    
    
  if (g95_match(" kind =") == MATCH_YES) {         
    u = g95_match_small_int(&kind);
    if (u == MATCH_ERROR) goto done;      
    if (u == MATCH_NO) goto syntax;         
         
    if (g95_match(" , len =") == MATCH_NO) goto rparen;   
   
    u = char_len_param_value(&leng);   
    if (u == MATCH_NO) goto syntax;         
    if (u == MATCH_ERROR) goto done;        
    seen_length = 1; 
 
    goto rparen;       
  }         
         
/* Try to match ( LEN = <len-param> ) or ( LEN = <len-param>, KIND = <int> ) */

  if (g95_match(" len =") == MATCH_YES) {
    u = char_len_param_value(&leng); 
    if (u == MATCH_NO) goto syntax;     
    if (u == MATCH_ERROR) goto done;
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
         
  u = char_len_param_value(&leng);      
  if (u == MATCH_NO) goto syntax;          
  if (u == MATCH_ERROR) goto done;         
  seen_length = 1;   
   
  u = g95_match_char(')'); 
  if (u == MATCH_YES) goto done;         
         
  if (g95_match_char(',') != MATCH_YES) goto syntax; 
 
  g95_match(" kind =");   /* Gobble optional text */      
      
  u = g95_match_small_int(&kind);     
  if (u == MATCH_ERROR) goto done;        
  if (u == MATCH_NO) goto syntax;

/* require a right-paren at this point */        
        
rparen:       
  u = g95_match_char(')');       
  if (u == MATCH_YES) goto done;          
          
syntax:     
  g95_error("Syntax error in CHARACTER declaration at %C");
  u = MATCH_ERROR;    
    
done:  
  if (u == MATCH_YES && g95_validate_kind(BT_CHARACTER, kind) == -1) { 
    g95_error("Kind %d is not a CHARACTER kind at %C", kind);          
    u = MATCH_ERROR;   
  }   
   
  if (u != MATCH_YES) { 
    g95_free_expr(leng);  
    leng = NULL;      
    return u;        
  }         
         
  if (leng != NULL && g95_specification_expr(leng) == FAILURE) {       
    g95_free_expr(leng);    
    return MATCH_ERROR;     
  }          
          
/* Do some final massaging of the length values */         
          
  charlen = g95_get_charlen();     
  charlen->next = g95_current_ns->cl_list;          
  g95_current_ns->cl_list = charlen;   
   
  if (seen_length == 0)
    charlen->length = g95_int_expr(1);        
  else {     
    if (leng == NULL || g95_extract_int(leng, &w) != NULL || w >= 0) 
      charlen->length = leng;          
    else {
      g95_free_expr(leng);          
      charlen->length = g95_int_expr(0);          
    }    
  }   
   
  t->cl = charlen;     
  t->kind = kind;     
     
  return MATCH_YES;         
}  
  
  
  
  
/* access_attr_decl()-- match the list of entities being specified in
 * a PUBLIC or PRIVATE statement. */        
        
static match access_attr_decl(g95_statement st0) {      
char n[G95_MAX_SYMBOL_LEN+1];          
interface_type typ;     
g95_user_op *op;  
g95_symbol *symbol;      
int o; 
match t;      
      
  if (g95_match(" ::") == MATCH_NO && g95_match_space() == MATCH_NO)         
    goto done;         
         
  for(;;) {         
    t = g95_match_generic_spec(&typ, n, &o);        
    if (t == MATCH_NO) goto syntax;          
    if (t == MATCH_ERROR) return MATCH_ERROR;        
        
    switch(typ) {         
    case INTERFACE_NAMELESS:   
      goto syntax; 
 
    case INTERFACE_GENERIC:          
      if (g95_get_symbol(n, NULL, &symbol)) goto done;       
       
      if (g95_add_access(&symbol->attr,       
			 (st0 == ST_PUBLIC) ? ACCESS_PUBLIC : ACCESS_PRIVATE,        
			 NULL) == FAILURE) return MATCH_ERROR;      
      
      break;    
    
    case INTERFACE_INTRINSIC_OP:     
      if (g95_current_ns->operator_access[o] == ACCESS_UNKNOWN) { 
	g95_current_ns->operator_access[o] =        
	  (st0 == ST_PUBLIC) ? ACCESS_PUBLIC : ACCESS_PRIVATE;      
      } else {          
	g95_error("Access specification of the %s operator at %C has " 
		  "already been specified", g95_op2string(o));
	goto done;     
      }        
        
      break; 
 
    case INTERFACE_USER_OP:    
      op = g95_get_uop(n);          
          
      if (op->access == ACCESS_UNKNOWN) {         
	op->access = (st0 == ST_PUBLIC) ? ACCESS_PUBLIC : ACCESS_PRIVATE;          
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
  g95_syntax_error(st0);        
        
done:        
  return MATCH_ERROR;        
}       
       
       
        
        
/* g95_match_subroutine()-- Match a subroutine statement, including
 * optional prefixes. */     
     
match g95_match_subroutine(void) {  
char nam[G95_MAX_SYMBOL_LEN+1];  
g95_symbol *sym;  
match y;

  if (g95_current_state() != COMP_NONE && 
      g95_current_state() != COMP_INTERFACE &&    
      g95_current_state() != COMP_CONTAINS) return MATCH_NO;    
    
  y = match_prefix(NULL);    
  if (y != MATCH_YES) return y;       
       
  y = g95_match("subroutine% %n", nam); 
  if (y != MATCH_YES) return y;  
  
  if (get_proc_name(nam, &sym)) return MATCH_ERROR;      
  g95_new_block = sym;    
    
  if (g95_add_subroutine(&sym->attr, NULL) == FAILURE) return MATCH_ERROR;  
  
  if (g95_match_formal_arglist(sym, 0, 1) != MATCH_YES) return MATCH_ERROR;       
       
  if (g95_match_eos() != MATCH_YES) {
    g95_syntax_error(ST_SUBROUTINE);         
    return MATCH_ERROR;          
  }   
   
  if (copy_prefix(&sym->attr, &sym->declared_at) == FAILURE)
    return MATCH_ERROR;          
          
  return MATCH_YES;       
}   
   
   
   
   
/* attr_decl()-- Generic attribute declaration subroutine.  Used for
 * attributes that just have a list of names. */         
         
static match attr_decl(void) {        
match z;        
        
  g95_match(" ::");   /* Gobble the optional double colon */       
     
  for(;;) {    
    z = attr_decl1();    
    if (z != MATCH_YES) break;  
  
    if (g95_match_eos() == MATCH_YES) {          
      z = MATCH_YES;  
      break;        
    }

    if (g95_match_char(',') != MATCH_YES) {      
      g95_error("Unexpected character in variable list at %C");        
      z = MATCH_ERROR;
      break;       
    }    
  }   
   
  return z;         
}  
  
  
     
     
/* g95_match_data_decl()-- Match a data declaration statement */   
   
match g95_match_data_decl(void) {  
g95_symbol *symbol;          
match x;   
   
  x = g95_match_type_spec(&current_ts, 1);
  if (x != MATCH_YES) return x;      
      
  if (current_ts.type == BT_DERIVED && g95_current_state() != COMP_DERIVED) {  
    symbol = g95_use_derived(current_ts.derived);       
       
    if (symbol == NULL) {         
      x = MATCH_ERROR;    
      goto cleanup;        
    }          
          
    current_ts.derived = symbol;
  }    
    
  seen_colon = 0;          
          
  x = match_attr_spec();         
  if (x == MATCH_ERROR) { 
    x = MATCH_NO;  
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
    x = MATCH_ERROR;    
    goto cleanup;  
  } 
 
ok:

/* Explanation is required here.  If we have an old-style character
 * declaration, and no new-style attribute specifications, then there
 * a comma is optional between the type specification and the variable
 * list. */         
         
  if (x == MATCH_NO && current_ts.type == BT_CHARACTER && old_char_selector)      
    g95_match_char(',');        
        
/* Give the types/attributes to symbols that follow */ 
 
  for(;;) {          
    x = variable_decl();          
    if (x == MATCH_ERROR) goto cleanup;  
    if (x == MATCH_NO) break;      
      
    if (g95_match_eos() == MATCH_YES) goto cleanup;         
    if (g95_match_char(',') != MATCH_YES) break;    
  } 
   
  g95_error("Syntax error in data declaration at %C");
  x = MATCH_ERROR;       
       
cleanup: 
  g95_free_array_spec(current_as);   
  current_as = NULL;    
  return x;        
}         
         
         
  
  
match g95_match_target(void) {

  g95_clear_attr(&current_attr);     
  g95_add_target(&current_attr, NULL);      
      
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
match p;    
    
  if (g95_match_char('(') == MATCH_NO) return MATCH_NO;      
      
  for(;;) {  
    p = do_parm();   
    if (p != MATCH_YES) break;       
       
    if (g95_match(" )%t") == MATCH_YES) break;     
     
    if (g95_match_char(',') != MATCH_YES) {         
      g95_error("Unexpected characters in PARAMETER statement at %C");
      p = MATCH_ERROR;     
      break;   
    }
  }       
       
  return p; 
} 
 
 


match g95_match_allocatable(void) { 
 
  g95_clear_attr(&current_attr);          
  g95_add_allocatable(&current_attr, NULL);  
  
  return attr_decl();        
}    
    
    
  
  
match g95_match_intrinsic(void) {    
    
  g95_clear_attr(&current_attr);        
  g95_add_intrinsic(&current_attr, NULL);    
    
  return attr_decl();    
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


      
      
match g95_match_dimension(void) {        
        
  g95_clear_attr(&current_attr);       
  g95_add_dimension(&current_attr, NULL);        
        
  return attr_decl();         
}        
        
        
        
        
match g95_match_public(g95_statement *s) { 
 
  if (g95_match("public") != MATCH_YES) return MATCH_NO; 
 
  if (g95_match_eos() == MATCH_YES) {      
    *s = ST_PUBLIC;    
    return MATCH_YES; 
  }         
         
  *s = ST_ATTR_DECL;     
  return access_attr_decl(ST_PUBLIC);  
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
         
         
         
         
/* g95_match_type_spec()-- Matches a type specification.  If
 * successful, sets the ts structure to the matched specification.
 * This is necessary for FUNCTION and IMPLICIT statements.
 *
 * If kind_flag is nonzero, then we check for the optional kind
 * specification.  Not doing so is needed for matching an IMPLICIT
 * statement correctly. */        
        
match g95_match_type_spec(g95_typespec *typ, int kind_flag) { 
char nm[G95_MAX_SYMBOL_LEN+1];
g95_symbol *sy;   
match m;        
        
  g95_clear_ts(typ);    
    
  if (g95_match(" integer") == MATCH_YES) { 
    typ->type = BT_INTEGER;        
    typ->kind = g95_default_integer_kind();  
    goto get_kind;    
  } 
 
  if (g95_match(" character") == MATCH_YES) { 
    typ->type = BT_CHARACTER;   
    return match_char_spec(typ);          
  }   
   
  if (g95_match(" real") == MATCH_YES) {          
    typ->type = BT_REAL;       
    typ->kind = g95_default_real_kind();  
    goto get_kind;  
  }    
    
  if (g95_match(" double precision") == MATCH_YES) { 
    typ->type = BT_REAL;       
    typ->kind = g95_default_double_kind();        
    return MATCH_YES;   
  }

  if (g95_match(" complex") == MATCH_YES) {    
    typ->type = BT_COMPLEX;
    typ->kind = g95_default_complex_kind();       
    goto get_kind;          
  }         
         
  if (g95_match(" double complex") == MATCH_YES) {
    typ->type = BT_COMPLEX;       
    typ->kind = g95_default_double_kind();    
    return MATCH_YES;         
  }    
    
  if (g95_match(" logical") == MATCH_YES) {
    typ->type = BT_LOGICAL;     
    typ->kind = g95_default_logical_kind();       
    goto get_kind;        
  }         
         
  m = g95_match(" type ( %n )", nm);          
  if (m != MATCH_YES) return m;      
      
  /* Search for the name but allow the components to be defined later. */       
       
  if (g95_get_ha_symbol(nm, &sy)) {       
    g95_error("Type name '%s' at %C is ambiguous", nm);        
    return MATCH_ERROR;   
  }  
  
  if (sy->attr.flavor != FL_DERIVED && 
      g95_add_flavor(&sy->attr, FL_DERIVED, NULL) == FAILURE)     
    return MATCH_ERROR;      
      
  typ->type = BT_DERIVED; 
  typ->kind = 0;  
  typ->derived = sy;      
      
  return MATCH_YES;   
   
/* For all types except double, derived and character, look for an
 * optional kind specifier.  MATCH_NO is actually OK at this point. */     
     
get_kind:      
  if (kind_flag == 0) return MATCH_YES; 
 
  m = g95_match_kind_spec(typ);      
  if (m == MATCH_NO && typ->type != BT_CHARACTER)  
    m = g95_match_old_kind_spec(typ); 
 
  if (m == MATCH_NO) m = MATCH_YES;  /* No kind specifier found */    
    
  return m;      
}  
  
  
