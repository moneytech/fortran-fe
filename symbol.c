/* Symbol handling
   Copyright (C) 2000, 2001, 2002 Free Software Foundation, Inc.
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
      
/* symbol.c-- Maintains binary trees of symbols */     
     
#include <string.h>
#include <stdio.h>
#include <ctype.h>
#include <stdlib.h>
      
#include "g95.h"
       
g95_namespace *g95_current_ns;  
  
static g95_symbol *changed_syms = NULL;       
static g95_symtree *changed_st = NULL;          
static g95_gsymbol *gsym_root = NULL;        
        
/* The following static variables hold the default types set by
 * IMPLICIT statements.  We have to store kind information because of
 * IMPLICIT DOUBLE PRECISION statements.  IMPLICIT NONE stores a
 * BT_UNKNOWN into all elements.  The arrays of flags indicate whether
 * a particular element has been explicitly set or not.  */          
          
static g95_typespec new_ts[G95_LETTERS];        
static int new_flag[G95_LETTERS];        
static int show_level=0;      
      
static mstring flavors[] = {         
  minit("UNKNOWN-FL",  FL_UNKNOWN),      minit("PROGRAM",     FL_PROGRAM), 
  minit("BLOCK-DATA",  FL_BLOCK_DATA),   minit("MODULE",      FL_MODULE),
  minit("VARIABLE",    FL_VARIABLE),     minit("PARAMETER",   FL_PARAMETER),
  minit("LABEL",       FL_LABEL),        minit("PROCEDURE",   FL_PROCEDURE),
  minit("DERIVED",     FL_DERIVED),      minit("NAMELIST",    FL_NAMELIST),       
  minit(NULL, -1)      
},      
      
procedures[] = {  
  minit("UNKNOWN-PROC",   PROC_UNKNOWN),     minit("MODULE-PROC", PROC_MODULE),      
  minit("INTERNAL-PROC",  PROC_INTERNAL),    minit("DUMMY-PROC",  PROC_DUMMY),      
  minit("INTRINSIC-PROC", PROC_INTRINSIC), 
  minit("EXTERNAL-PROC",  PROC_EXTERNAL),  
  minit("STATEMENT-PROC", PROC_ST_FUNCTION), minit(NULL, -1)       
}, 
 
accessibility[] = {
  minit("UNKNOWN-ACCESS", ACCESS_UNKNOWN),   minit("PUBLIC", ACCESS_PUBLIC),      
  minit("PRIVATE", ACCESS_PRIVATE),          minit(NULL, -1)  
};          
          
          
/* Basic overview: Fortran 95 requires a potentially unlimited number
 * of distinct namespaces when compiling a program unit.  This case
 * occurs during a compilation of internal subprograms because all of
 * the internal subprograms must be read before we can start
 * generating code for the host.
 * 
 * Given the tricky nature of the fortran grammar, we must be able to
 * undo changes made to a symbol table if the current interpretation
 * of a statement is found to be incorrect.  Whenever a symbol is
 * looked up, we make a copy of it and link to it.  All of these
 * symbols are kept in a singly linked list so that we can commit or
 * undo the changes at a later time. 
 *
 * A symtree may point to a symbol node outside of it's namespace.  In
 * this case, that symbol has been used as a host associated variable
 * at some previous time.  */          
          
          
          
          
/* g95_get_default_type()-- Given a symbol, return a pointer to the
 * typespec for it's default type */          
          
g95_typespec *g95_get_default_type(g95_symbol *symbol, g95_namespace *names) {  
char letter;          
          
  letter = symbol->name[0];         
  if (letter < 'a' || letter > 'z')   
    g95_internal_error("g95_get_default_type(): Bad symbol"); 
 
  if (names == NULL) names = g95_current_ns;         
         
  return &names->default_type[letter - 'a'];     
}    
    
    
 
 
void g95_show_components(g95_symbol *sy) {
g95_component *s;          
          
  for(s=sy->components; s; s=s->next) {          
    g95_status("(%s ", s->name); 
    g95_show_typespec(&s->ts);      
    if (s->pointer) g95_status(" POINTER");          
    if (s->dimension) g95_status(" DIMENSION");    
    g95_status_char(' ');
    g95_show_array_spec(s->as);
    g95_status(")");     
    if (s->next != NULL) g95_status_char(' ');    
  }
}       
       
       
         
         
/* duplicate_attr()-- Generate an error because of a duplicate attribute */        
        
static void duplicate_attr(char *attribute, locus *old_loc) {   
   
  if (old_loc == NULL) old_loc = g95_current_locus();   
   
  g95_error("Duplicate %s attribute specified at %L", attribute, old_loc);   
}       
       
       
      
      
/* g95_show_attr()-- Show symbol attributes.  The flavor and intent
 * are followed by whatever single bit attributes are present */

void g95_show_attr(symbol_attribute *atr) {   
   
  g95_status("(%s %s %s %s", g95_code2string(flavors, atr->flavor),   
	     g95_intent_string(atr->intent),
	     g95_code2string(accessibility, atr->access),   
	     g95_code2string(procedures, atr->proc));        
        
  if (atr->allocatable)    g95_status(" ALLOCATABLE");
  if (atr->dimension)      g95_status(" DIMENSION");        
  if (atr->external)       g95_status(" EXTERNAL");     
  if (atr->intrinsic)      g95_status(" INTRINSIC");       
  if (atr->optional)       g95_status(" OPTIONAL");         
  if (atr->pointer)        g95_status(" POINTER");       
  if (atr->save)           g95_status(" SAVE");        
  if (atr->target)         g95_status(" TARGET");     
  if (atr->dummy)          g95_status(" DUMMY");      
  if (atr->result_var)     g95_status(" RESULT");       
  if (atr->entry)          g95_status(" ENTRY");

  if (atr->data)           g95_status(" DATA");     
  if (atr->use_assoc)      g95_status(" USE-ASSOC");  
  if (atr->in_namelist)    g95_status(" IN-NAMELIST");       
  if (atr->in_common)      g95_status(" IN-COMMON");         
         
  if (atr->function)       g95_status(" FUNCTION");        
  if (atr->subroutine)     g95_status(" SUBROUTINE");         
  if (atr->implicit_type)  g95_status(" IMPLICIT-TYPE");        
        
  if (atr->sequence)       g95_status(" SEQUENCE");    
  if (atr->elemental)      g95_status(" ELEMENTAL");
  if (atr->pure)           g95_status(" PURE"); 
  if (atr->recursive)      g95_status(" RECURSIVE");     
     
  g95_status(")");     
}       
       
       
    
    
/* match_implicit_range()-- Match the letter range(s) of an IMPLICIT
 * statement.  Sets flags in new_flag[] and copies the typespec to
 * new_ts[]. */   
   
static match match_implicit_range(g95_typespec *t) {
int p, g, w, e, inner;       
locus cur_loc;         
         
  cur_loc = *g95_current_locus();       
      
  g95_gobble_whitespace();      
  p = g95_next_char();   
  if (p != '(') {        
    g95_error("Missing character range in IMPLICIT at %C");      
    goto bad;      
  }          
          
  inner = 1;      
  while(inner) {        
    g95_gobble_whitespace();      
    w = g95_next_char();      
    if (!isalpha(w)) goto bad;      
      
    g95_gobble_whitespace();  
    p = g95_next_char();

    switch(p) {     
    case ')':      
      inner = 0;   /* Fall through */ 
 
    case ',':
      e = w;      
      break; 
 
    case '-':      
      g95_gobble_whitespace();     
      e = g95_next_char();   
      if (!isalpha(e)) goto bad;        
        
      g95_gobble_whitespace();     
      p = g95_next_char();          
          
      if ((p != ',') && (p != ')')) goto bad;     
      if (p == ')') inner = 0;    
    
      break;  
  
    default: goto bad; 
    }       
       
    if (w > e) {          
      g95_error("Letters must be in alphabetic order in IMPLICIT statement "         
		"at %C");     
      goto bad;         
    }        
        
    w -= 'a';        
    e -= 'a';         
         
    for(g=w; g<=e; g++) {    
      if (new_flag[g]) {     
	g95_error("Letter '%c' already set in IMPLICIT statement at %C",     
		  g+'A');    
	goto bad; 
      }       
       
      new_ts[g] = *t;  
      new_flag[g] = 1;          
    }
  }  
  
  return MATCH_YES; 
 
 bad:        
  g95_syntax_error(ST_IMPLICIT);         
         
  g95_set_locus(&cur_loc);
  return MATCH_ERROR;         
}     
     
     
     
     
/* g95_copy_attr()-- copy an attribute to a symbol attribute, bit by
 * bit.  Some attributes have a lot of side-effects but cannot be
 * present given where we are called from, so we ignore some bits */       
       
try g95_copy_attr(symbol_attribute *d, symbol_attribute *s1,       
		  locus *where) {          
          
  if (s1->allocatable && g95_add_allocatable(d, where) == FAILURE)    
    goto fail;     
     
  if (s1->dimension && g95_add_dimension(d, where) == FAILURE) goto fail;        
  if (s1->optional && g95_add_optional(d, where) == FAILURE) goto fail;  
  if (s1->pointer && g95_add_pointer(d, where) == FAILURE) goto fail;    
  if (s1->save && g95_add_save(d, where) == FAILURE) goto fail;   
  if (s1->target && g95_add_target(d, where) == FAILURE) goto fail;          
  if (s1->dummy && g95_add_dummy(d, where) == FAILURE) goto fail; 
  if (s1->result_var && g95_add_result(d, where) == FAILURE) goto fail;         
  if (s1->entry) d->entry = 1;        
        
  if (s1->in_namelist && g95_add_in_namelist(d, where) == FAILURE)
    goto fail;  
  
  if (s1->in_common && g95_add_in_common(d, where) == FAILURE) goto fail;   
  if (s1->generic && g95_add_generic(d, where) == FAILURE) goto fail;        
  if (s1->function && g95_add_function(d, where) == FAILURE) goto fail;     
  if (s1->subroutine && g95_add_subroutine(d, where) == FAILURE) goto fail; 
 
  if (s1->sequence && g95_add_sequence(d, where) == FAILURE) goto fail;       
  if (s1->elemental && g95_add_elemental(d, where) == FAILURE) goto fail;        
  if (s1->pure && g95_add_pure(d, where) == FAILURE) goto fail;      
  if (s1->recursive && g95_add_recursive(d, where) == FAILURE) goto fail;         
         
  if (s1->flavor != FL_UNKNOWN &&    
      g95_add_flavor(d, s1->flavor, where) == FAILURE) goto fail;       
       
  if (s1->intent != INTENT_UNKNOWN &&         
      g95_add_intent(d, s1->intent, where) == FAILURE) goto fail;    
    
  if (s1->access != ACCESS_UNKNOWN &&        
      g95_add_access(d, s1->access, where) == FAILURE) goto fail;  
  
/* The subroutines that set these bits also cause flavors to be set,
 * and that has already happened in the original, so don't let to
 * happen again. */         
         
  if (s1->external) d->external = 1;   
  if (s1->intrinsic) d->intrinsic = 1;

  return SUCCESS;          
          
fail:        
  return FAILURE;    
}     
     
     
     
     
/* g95_check_assign_symbol()-- Relative of g95_check_assign() except
 * that the lvalue is a single symbol. */  
  
try g95_check_assign_symbol(g95_symbol *sym, g95_expr *rvalue) {       
g95_expr lvalue;   
g95_ref re;      
      
  memset(&lvalue, '\0', sizeof(g95_expr));
  memset(&re, '\0', sizeof(g95_ref));        
        
  lvalue.type = EXPR_VARIABLE;         
  lvalue.ts = sym->ts;    
  if (sym->as) {    
    lvalue.rank = sym->as->rank;        
    lvalue.ref = &re;  
  
    re.type = REF_ARRAY;    
    re.u.ar.type = AR_FULL;     
  }       
       
  lvalue.symbol = sym;         
  lvalue.where = sym->declared_at;

  return g95_check_assign(&lvalue, rvalue, 1);     
} 
 
 
       
       
/* g95_check_assign()-- Given an assignable expression and an
 * arbitrary expression, make sure that the assignment can take
 * place. */    
    
try g95_check_assign(g95_expr *lvalue, g95_expr *rvalue, int conform) { 
g95_symbol *s;       
       
  s = lvalue->symbol;          
          
  if (s->attr.intent == INTENT_IN) {         
    g95_error("Can't assign to INTENT(IN) variable '%s' at %L",
	      s->name, &lvalue->where);         
    return FAILURE;     
  }    
    
  if (rvalue->rank != 0 && lvalue->rank != rvalue->rank) {     
    g95_error("Incompatible ranks in assignment at %L", &lvalue->where);     
    return FAILURE;       
  }    
    
  if (lvalue->ts.type == BT_UNKNOWN) {  
    g95_error("Variable type is UNKNOWN in assignment at %L", &lvalue->where);   
    return FAILURE;     
  } 
 
  /* Check size of array assignments */    
    
  if (lvalue->rank != 0 && rvalue->rank != 0 && 
      g95_check_conformance("Array assignment", lvalue, rvalue) != SUCCESS)  
    return FAILURE;     
     
  if (g95_compare_types(&lvalue->ts, &rvalue->ts)) return SUCCESS; 
 
  if (!conform) {       
    if (g95_numeric_ts(&lvalue->ts) && g95_numeric_ts(&rvalue->ts))   
      return SUCCESS;    
    
    g95_error("Incompatible types in assignment at %L, %s to %s",        
	      &rvalue->where, g95_typename(&rvalue->ts),         
	      g95_typename(&lvalue->ts));

    return FAILURE;      
  }       
       
  return g95_convert_type(rvalue, &lvalue->ts, 1);   
}    
    
    
   
   
/* g95_set_implicit_none()-- Handle a correctly parsed IMPLICIT NONE */  
  
void g95_set_implicit_none(void) {      
int s;      
      
  for(s='a'; s<='z'; s++) {         
    g95_clear_ts(&g95_current_ns->default_type[s - 'a']);   
    g95_current_ns->set_flag[s - 'a'] = 1;        
  }       
}     
     
     


/* check_used()-- Common subroutine called by attribute changing
 * subroutines in order to prevent them from changing a symbol that
 * has been use-associated.  Returns zero if it is OK to change the
 * symbol, nonzero if not. */    
    
static int check_used(symbol_attribute *attribute, locus *pos) {       
       
  if (attribute->use_assoc == 0) return 0;         
         
  if (pos == NULL) pos = g95_current_locus(); 
 
  g95_error("Cannot change attributes of USE-associated symbol at %L", pos);

  return 1;  
}          
          
          
 
/* check_conflict()-- This is a generic conflict-checker.  We do this
 * to avoid having a single conflict in two places. */          
          
#define conf(a, b) \
    if (attr->a && attr->b) { attr1 = a; attr2 = b; goto conflict; }
#define conf2(a) if (attr->a) { attr2 = a; goto conflict; }
   
static try check_conflict(symbol_attribute *attr, locus *where) {   
char *attr1, *attr2;          
          
static char *dummy = "DUMMY", *save = "SAVE", *pointer = "POINTER", 
  *target = "TARGET", *external = "EXTERNAL", *intent = "INTENT",    
  *intrinsic = "INTRINSIC", *allocatable = "ALLOCATABLE",      
  *elemental = "ELEMENTAL", *private = "PRIVATE", *recursive = "RECURSIVE", 
  *in_common = "COMMON", *result_var = "RESULT", *in_namelist = "NAMELIST",   
  *public = "PUBLIC", *optional = "OPTIONAL", *entry = "ENTRY",        
  *function = "FUNCTION", *subroutine = "SUBROUTINE", *dimension = "DIMENSION",   
  *use_assoc = "USE";          
          
  if (where == NULL) where = g95_current_locus();      
      
  if (attr->pointer && attr->intent != INTENT_UNKNOWN) {      
    attr1 = pointer; attr2 = intent; goto conflict; }         
         
/* Check for attributes not allowed in a BLOCK DATA */          
          
  if (g95_current_state() == COMP_BLOCK_DATA) {        
    attr1 = NULL;     
     
    if (attr->allocatable) attr1 = allocatable;     
    if (attr->external) attr1 = external; 
    if (attr->optional) attr1 = optional;         
    if (attr->access == ACCESS_PRIVATE) attr1 = private;     
    if (attr->access == ACCESS_PUBLIC) attr1 = public;      
    if (attr->intent != INTENT_UNKNOWN) attr1 = intent;       
       
    if (attr1 != NULL) {
      g95_error("%s attribute not allowed in BLOCK DATA program unit at %L",          
		attr1, where);
      return FAILURE;       
    }  
  }      
      
  conf(dummy, save);   
  conf(pointer, target);          
  conf(pointer, external);       
  conf(pointer, intrinsic);          
  conf(target, external);     
  conf(target, intrinsic); 
  conf(external, dimension);      
      
  conf(external, intrinsic);        
  conf(allocatable, pointer);      
  conf(allocatable, dummy);      /* Allowed in Fortran 2000 */      
  conf(allocatable, function);   /* Allowed in Fortran 2000 */       
  conf(allocatable, result_var); /* Allowed in Fortran 2000 */
  conf(elemental, recursive);   
   
  conf(in_common, dummy);
  conf(in_common, allocatable);    
  conf(in_common, result_var); 
  conf(in_common, function); 
  conf(in_common, entry);          
  conf(in_common, use_assoc);     
     
  conf(dummy, result_var);          
          
  conf(in_namelist, pointer);      
  conf(in_namelist, allocatable); 
 
  conf(entry, result_var);      
      
  conf(function, subroutine);       
       
  attr1 = g95_code2string(flavors, attr->flavor);

  if (attr->in_namelist && attr->flavor != FL_VARIABLE &&          
      attr->flavor != FL_UNKNOWN) {  
  
    attr2 = in_namelist;         
    goto conflict;   
  }        
        
  switch(attr->flavor) {
  case FL_PROGRAM: case FL_BLOCK_DATA: case FL_MODULE: case FL_LABEL:  
    conf2(dummy);         conf2(save);        conf2(pointer);   
    conf2(target);        conf2(external);    conf2(intrinsic);        
    conf2(allocatable);   conf2(result_var);  conf2(in_namelist); 
    conf2(optional);      conf2(function);    conf2(subroutine);        
    break;

  case FL_VARIABLE:
  case FL_NAMELIST:   
    break; 
 
  case FL_PROCEDURE:   
    conf2(intent); 
    if (attr->subroutine) {     
      conf2(save);        conf2(pointer);     conf2(target);        
      conf2(allocatable); conf2(result_var);  conf2(in_namelist);
      conf2(function);   
    } 
 
    switch(attr->proc) {       
    case PROC_ST_FUNCTION:
      conf2(in_common);         
      conf2(dummy);  
      break;

    case PROC_MODULE:
      conf2(dummy);
      break;         
         
    case PROC_DUMMY:  
      conf2(result_var);          
      conf2(in_common);          
      conf2(save);  
      break;  
  
    default: 
      break;  
    }

    break;        
        
  case FL_DERIVED:       
    conf2(dummy);        conf2(save);        conf2(pointer);       
    conf2(target);       conf2(external);    conf2(intrinsic);          
    conf2(allocatable);  conf2(optional);    conf2(entry);       
    conf2(function);     conf2(subroutine);     
           
    if (attr->intent != INTENT_UNKNOWN) { attr2 = intent; goto conflict; } 
    break; 
 
  case FL_PARAMETER:  
    conf2(external);      conf2(intrinsic);    conf2(optional);      
    conf2(allocatable);   conf2(function);     conf2(subroutine); 
    conf2(entry);         conf2(pointer);      conf2(target);  
    conf2(dummy);         conf2(in_common);  
    break;      
      
  default:    
    break;     
  }          
          
  return SUCCESS;          
          
conflict:
  g95_error("%s attribute conflicts with %s attribute at %L", attr1, attr2,
	    where);     
  return FAILURE;          
}     
     
#undef conf
#undef conf2
   
   
     
     
try g95_add_dimension(symbol_attribute *atr, locus *old_loc) {

  if (check_used(atr, old_loc)) return FAILURE; 
 
  if (atr->dimension) {        
    duplicate_attr("DIMENSION", old_loc);          
    return FAILURE;    
  }  
  
  atr->dimension = 1;      
  return check_conflict(atr, old_loc);    
}    
    
    
  
  
/* g95_define_st_label()-- Called when a statement with a statement
 * label is about to be accepted.  We add the label to the list of the
 * current namespace, making sure it hasn't been defined previously
 * and referenced correctly. */   
   
void g95_define_st_label(g95_st_label *lp, g95_sl_type dtype,
                         locus *label_locus) {      
int labelno;    
    
  labelno = lp->value; 
 
  if (lp->defined != ST_LABEL_UNKNOWN)      
    g95_error("Duplicate statement label %d at %L and %L", labelno,         
	      &lp->where, label_locus);      
  else {         
    lp->where = *label_locus;      
      
    switch(dtype) {        
    case ST_LABEL_FORMAT:          
      if (lp->referenced == ST_LABEL_TARGET)       
        g95_error("Label %d at %C already referenced as branch target",         
		  labelno);      
      else     
        lp->defined = ST_LABEL_FORMAT;

      break;          
          
    case ST_LABEL_TARGET:          
      if (lp->referenced == ST_LABEL_FORMAT)        
        g95_error("Label %d at %C already referenced as a format label",
		  labelno);    
      else      
        lp->defined = ST_LABEL_TARGET;        
        
      break; 
 
    default: 
      lp->defined = ST_LABEL_BAD_TARGET;       
      lp->referenced = ST_LABEL_BAD_TARGET; 
    }          
  }   
}   
   
   
 
 
try g95_add_subroutine(symbol_attribute *atr, locus *loc) {

  if (atr->flavor != FL_PROCEDURE &&   
      g95_add_flavor(atr, FL_PROCEDURE, loc) == FAILURE) return FAILURE;  
  
  atr->subroutine = 1;  
  return check_conflict(atr, loc);      
}       
       
       
          
          
void g95_symbol_init_2(void) {   
   
  g95_current_ns = g95_get_namespace(NULL); 
}    
    
    


/* free_sym_tree()-- Recursive function that deletes an entire
 * red-black tree and all the symbols that it contains. */ 
 
static void free_sym_tree(g95_symtree *rb) {          
g95_namespace *names;         
g95_symbol *sy;     
     
  if (rb == NULL) return;    
    
  free_sym_tree(rb->left);
  free_sym_tree(rb->right);

  sy = rb->n.sym;          
          
  sy->refs--;         
  if (sy->refs < 0) g95_internal_error("free_sym_tree(): Negative refs");

  if (sy->formal_ns != NULL && sy->refs == 1) {       
    /* as formal_ns contains a reference to sym, delete formal_ns just
     * before the deletion of sym. */    
    names = sy->formal_ns;         
    sy->formal_ns = NULL;    
    g95_free_namespace(names);         
  } else if (sy->refs == 0) {   /* Go ahead and delete the symbol */    
    g95_free_symbol(sy);       
  }      
      
  g95_free(rb);   
}         
         
         


/* g95_reference_st_label()-- Reference a label.  Given a label
 * and its type, see if that reference is consistent with what is
 * known about that label, updating the unknown state.  Returns
 * FAILURE if something goes wrong. */          
          
try g95_reference_st_label(g95_st_label *lp, g95_sl_type type) {    
g95_sl_type label_type;         
int labelno; 
try retval;        
        
  if (lp == NULL) 
    return SUCCESS;   
   
  labelno = lp->value;         
         
  if (lp->defined != ST_LABEL_UNKNOWN)         
    label_type = lp->defined;     
  else {    
    label_type = lp->referenced;      
    lp->where = *g95_current_locus(); 
  }         
         
  if (label_type == ST_LABEL_FORMAT && type == ST_LABEL_TARGET) {     
    g95_error("Label %d at %C previously used as a FORMAT label", labelno);     
    retval = FAILURE;          
    goto done;   
  }        
        
  if ((label_type == ST_LABEL_TARGET || label_type == ST_LABEL_BAD_TARGET) 
      && type == ST_LABEL_FORMAT) { 
    g95_error("Label %d at %C previously used as branch target", labelno);        
    retval = FAILURE;          
    goto done;       
  }         
         
  lp->referenced = type;         
  retval = SUCCESS; 
 
done:  
  return retval;    
}         
         
         
       
       
try g95_add_external(symbol_attribute *a, locus *loc) {      
      
  if (check_used(a, loc)) return FAILURE;   
     
  if (a->external) {        
    duplicate_attr("EXTERNAL", loc);          
    return FAILURE; 
  }     
     
  a->external = 1;         
         
  return check_conflict(a, loc);
}  
  
  
         
         
try g95_add_result(symbol_attribute *attribute, locus *old_loc) {          
          
  if (check_used(attribute, old_loc)) return FAILURE;      
      
  attribute->result_var = 1; 
  return check_conflict(attribute, old_loc);
}    
    
    
       
       
/* g95_symbol_state()-- Makes sure that no changes to symbols are pending */        
        
void g95_symbol_state(void) {    
    
  if (changed_syms != NULL)         
    g95_internal_error("Symbol changes still pending");
}  
  
  
    
    
void g95_symbol_done_2(void) { 
 
  g95_free_namespace(g95_current_ns);   
  g95_current_ns = NULL; 
}      
      
      
 
 
/* g95_get_namespace()-- Allocate a new namespace structure.  */     
     
g95_namespace *g95_get_namespace(g95_namespace *parent) {          
g95_namespace *namesp;         
g95_typespec *ts;        
int x;          
           
  namesp = g95_getmem(sizeof(g95_namespace));     
  namesp->sym_root = NULL;    
  namesp->uop_root = NULL;          
  namesp->default_access = ACCESS_UNKNOWN;
  namesp->parent = parent;         
         
  for(x=0; x<G95_INTRINSIC_OPS; x++)    
    namesp->operator_access[x] = ACCESS_UNKNOWN;    
    
/* Initialize default types */  
  
  for(x='a'; x<='z'; x++) {       
    namesp->set_flag[x - 'a'] = 0;      
    ts = &namesp->default_type[x - 'a'];       
       
    if (namesp->parent != NULL) {    /* Copy parent settings */   
      *ts = namesp->parent->default_type[x - 'a'];     
      continue;      
    }      
      
    if (g95_option.implicit_none != 0) {         
      g95_clear_ts(ts);         
      continue;     
    }          
          
    if ('i' <= x && x <= 'n') {          
      ts->type = BT_INTEGER;    
      ts->kind = g95_default_integer_kind();   
    } else {          
      ts->type = BT_REAL;         
      ts->kind = g95_default_real_kind();  
    }   
  }        
        
  return namesp; 
}       
       
       
          
/******************** Statement label management ********************/          
          
/* Free a single g95_st_label structure, making sure the list is not
 * messed up.  This function is called only when some parse error
 * occurs. */  
  
void g95_free_st_label(g95_st_label *i) {      
      
  if (i == NULL) return;       
       
  if (i->prev)   
    (i->prev->next = i->next); 
  
  if (i->next)     
    (i->next->prev = i->prev);          
          
  if (i->format != NULL) g95_free_expr(i->format);       
  g95_free(i);       
}   
   
   
    
    
/* g95_get_ha_symbol()-- Subroutine that searches for a symbol,
 * creating it if it doesn't exist, but tries to host-associate the
 * symbol if possible. */    
    
int g95_get_ha_symbol(char *nam, g95_symbol **res) {          
g95_symbol *s;     
int f;      
      
  f = g95_find_symbol(nam, g95_current_ns, 0, &s);      
  if (s != NULL) {         
    g95_save_symbol_data(s);      
      
    *res = s; 
    return f;          
  }    
    
  if (g95_current_ns->parent != NULL) {
    f = g95_find_symbol(nam, g95_current_ns->parent, 1, &s);        
    if (f) return f;          
          
    if (s != NULL) {      
      *res = s;  
      return 0;         
    }         
  }     
     
  return g95_get_symbol(nam, g95_current_ns, res);
} 
 
 
     
     
/* g95_set_component_attr()-- Set component attributes from a standard
 * symbol attribute structure. */      
      
void g95_set_component_attr(g95_component *v, symbol_attribute *atr) {  
  
  v->dimension = atr->dimension;       
  v->pointer = atr->pointer; 
}          
          
          
  
/* free_st_labels()-- Free a whole list of g95_st_label structures.  */  
  
static void free_st_labels(g95_st_label *x) { 
g95_st_label *h;   
   
  for(; x; x=h) {       
    h = x->next;          
    if (x->format != NULL) g95_free_expr(x->format);   
    g95_free(x);
  }     
}          
          
          
    
    
try g95_add_save(symbol_attribute *atr, locus *old_loc) {         
         
  if (check_used(atr, old_loc)) return FAILURE;

  if (g95_pure(NULL)) { 
    g95_error("SAVE attribute at %L cannot be specified in a PURE procedure",  
	      old_loc); 
    return FAILURE;         
  }         
         
  if (atr->save) {     
    duplicate_attr("SAVE", old_loc); 
    return FAILURE;    
  }          
          
  atr->save = 1;      
  return check_conflict(atr, old_loc);      
}          
          
          
      
      
/* g95_get_componentr_attr()-- Get a standard symbol attribute
 * structure given the component structure. */          
          
void g95_get_component_attr(symbol_attribute *attr, g95_component *n) {         
         
  g95_clear_attr(attr);          
  attr->dimension = n->dimension;          
  attr->pointer = n->pointer; 
}     
     
     
        
        
/* g95_find_symtree()-- Given a root symtree node and a name, try to
 * find the symbol within the namespace.  Returns NULL if the symbol
 * is not found. */     
     
g95_symtree *g95_find_symtree(g95_symtree *st1, char *nam) {        
int r;   
   
  while(st1 != NULL) {   
    r = strcmp(nam, st1->name);        
    if (r == 0) return st1;       
       
    st1 = (r < 0) ? st1->left : st1->right;        
  }

  return NULL;   
}         
         
         
         
         
/* g95_check_conformance()-- Given two expressions, make sure that
 * the arrays are conformable. */    
    
try g95_check_conformance(const char *optype, g95_expr *op, g95_expr *op_2) {       
int op1_flag, op2_flag, y;      
mpz_t op1_size, op2_size;       
try b;   
   
  if (op->rank == 0 || op_2->rank == 0) return SUCCESS;   
   
  if (op->rank != op_2->rank) {         
    g95_error("Incompatible ranks in %s at %L", optype, &op->where);         
    return FAILURE;    
  }          
          
  b = SUCCESS;   
   
  for(y=0; y<op->rank; y++) {
    op1_flag = g95_array_dimen_size(op, y, &op1_size) == SUCCESS;  
    op2_flag = g95_array_dimen_size(op_2, y, &op2_size) == SUCCESS;       
           
    if (op1_flag && op2_flag && mpz_cmp(op1_size, op2_size) != 0) {       
      g95_error("%s at %L has different shape on dimension %d (%d/%d)",    
		optype, &op->where, y+1, (int) mpz_get_si(op1_size),     
		(int) mpz_get_si(op2_size));    
    
      b = FAILURE;         
    }     
     
    if (op1_flag) mpz_clear(op1_size);       
    if (op2_flag) mpz_clear(op2_size);  
  
    if (b == FAILURE) return FAILURE;       
  }         
         
  return SUCCESS;        
} 
 
 
      
      
try g95_add_entry(symbol_attribute *atr, locus *w) {

  if (check_used(atr, w)) return FAILURE; 
 
  if (atr->entry) {         
    duplicate_attr("ENTRY", w); 
    return FAILURE;        
  }       
       
  atr->entry = 1;     
  return check_conflict(atr, w);     
}       
       
       
    
    
/* g95_add_type()-- Add a type to a symbol. */    
    
try g95_add_type(g95_symbol *s, g95_typespec *typ, locus *old_loc) {     
sym_flavor flavor;

  if (old_loc == NULL) old_loc = g95_current_locus();  
  
  if (s->ts.type != BT_UNKNOWN) {    
    g95_error("Symbol '%s' at %L already has basic type of %s", s->name,
	      old_loc, g95_basic_typename(s->ts.type)); 
    return FAILURE;    
  }     
     
  flavor = s->attr.flavor;      
      
  if (flavor == FL_PROGRAM || flavor == FL_BLOCK_DATA || flavor == FL_MODULE || 
      flavor == FL_LABEL || (flavor == FL_PROCEDURE && s->attr.subroutine) || 
      flavor == FL_DERIVED || flavor == FL_NAMELIST) { 
    g95_error("Symbol '%s' at %L cannot have a type", s->name, old_loc);       
    return FAILURE;     
  }          
          
  s->ts = *typ;
  return SUCCESS;  
}  
  
  


/* g95_set_implicit()-- Sets the implicit types parsed by the previous
 * g95_match_implicit(). */     
     
void g95_set_implicit(void) { 
int n;      
      
  for(n=0; n<G95_LETTERS; n++)   
    if (new_flag[n]) {      
      g95_current_ns->default_type[n] = new_ts[n];       
      g95_current_ns->set_flag[n] = 1;      
    }       
} 
 
 
   
   
/* g95_clear_attr()-- Clears all attributes */        
        
void g95_clear_attr(symbol_attribute *attribute) {         
         
  attribute->allocatable = 0;     
  attribute->dimension = 0;      
  attribute->external = 0;      
  attribute->intrinsic = 0; 
  attribute->optional = 0;          
  attribute->pointer = 0;       
  attribute->save = 0;    
  attribute->target = 0;      
  attribute->dummy = 0;   
  attribute->result_var = 0;
  attribute->entry = 0;     
  attribute->data = 0;        
  attribute->use_assoc = 0;          
  attribute->in_namelist = 0;        
          
  attribute->in_common = 0;    
  attribute->function = 0;  
  attribute->subroutine = 0;       
  attribute->generic = 0;        
  attribute->implicit_type = 0;  
  attribute->sequence = 0;        
  attribute->elemental = 0;       
  attribute->pure = 0;      
  attribute->recursive = 0;     
     
  attribute->access = ACCESS_UNKNOWN;       
  attribute->intent = INTENT_UNKNOWN;          
  attribute->flavor = FL_UNKNOWN;
  attribute->proc = PROC_UNKNOWN;         
  attribute->if_source = IFSRC_UNKNOWN;     
}     
     
     
     
     
/* g95_match_implicit_none()-- Match an IMPLICIT NONE statement. */  
  
match g95_match_implicit_none(void) {  
  
  return (g95_match_eos() == MATCH_YES) ? MATCH_YES : MATCH_NO;        
}     
     
     
  
/************** Component name management ************/         
         
/* Component names of a derived type form their own little namespaces
 * that are separate from all other spaces.  The space is composed of
 * a singly linked list of g95_component structures whose head is
 * located in the parent symbol. */  
  
/* g95_add_component()-- Add a component name to a symbol.  The call
 * fails if the name is already present.  On success, the component
 * pointer is modified to point to the additional component structure. */   
   
try g95_add_component(g95_symbol *symb, char *nm,          
		      g95_component **component) {        
g95_component *m, *t;          
          
  t = NULL;        
        
  for(m=symb->components; m; m=m->next) {     
    if (strcmp(m->name, nm) == 0) {          
      g95_error("Component '%s' at %C already declared at %L",
		nm, &m->loc);     
      return FAILURE; 
    } 
 
    t = m;       
  }  
  
/* Allocate new component */    
    
  m = g95_get_component();   
   
  if (t == NULL) symb->components = m;     
  else t->next = m;

  strcpy(m->name, nm);  
  m->loc = *g95_current_locus();     
     
  *component = m;        
  return SUCCESS;     
} 
 
 
    
    
try g95_add_pure(symbol_attribute *atr, locus *where) {       
       
  if (check_used(atr, where)) return FAILURE;    
    
  atr->pure = 1;   
  return check_conflict(atr, where); 
}        
        
        
  
  
try g95_add_optional(symbol_attribute *attr, locus *where) {      
      
  if (check_used(attr, where)) return FAILURE;       
       
  if (attr->optional) {     
    duplicate_attr("OPTIONAL", where); 
    return FAILURE;
  }      
      
  attr->optional = 1; 
  return check_conflict(attr, where);       
}   
   
   
        
        
/* traverse_ns()-- Recursive namespace traversal function. */  
  
static void traverse_ns(g95_symtree *sta, void (*func)(g95_symbol *)) {     
     
  if (sta == NULL) return;          
          
  if (sta->n.sym->mark == 0) (*func)(sta->n.sym);       
  sta->n.sym->mark = 1;        
        
  traverse_ns(sta->left, func);   
  traverse_ns(sta->right, func);          
}     
     
     
 
 
try g95_add_elemental(symbol_attribute *attribute, locus *w) {   
   
  if (check_used(attribute, w)) return FAILURE;          
          
  attribute->elemental = 1; 
  return check_conflict(attribute, w);
}         
         
         
       
       
try g95_add_explicit_interface(g95_symbol *sym, ifsrc source,        
			       g95_formal_arglist *form, locus *pos) {       
       
  if (check_used(&sym->attr, pos)) return FAILURE;   
   
  if (pos == NULL) pos = g95_current_locus();

  if (sym->attr.if_source != IFSRC_UNKNOWN && 
      sym->attr.if_source != IFSRC_DECL) {          
    g95_error("Symbol '%s' at %L already has an explicit interface",     
	      sym->name, pos);  
    return FAILURE;          
  }      
      
  sym->formal = form;       
  sym->attr.if_source = source;          
          
  return SUCCESS;   
} 
 
 
       
       
/* g95_find_uop()-- Given a name find the user operator node.  Returns
 * NULL if it does not exist. */     
     
g95_user_op *g95_find_uop(char *nm, g95_namespace *names) {          
g95_symtree *s;  
  
  if (names == NULL) names = g95_current_ns;  
  
  s = g95_find_symtree(names->uop_root, nm);         
  return (s == NULL) ? NULL : s->n.uop;          
}  
  
  
     
     
/* g95_module_symbol()-- Return nonzero if the symbol lives in a
 * module namespace or has been use-associated. */          
          
int g95_module_symbol(g95_symbol *symb) {          
          
  return (symb->ns->proc_name != NULL &&
	  symb->ns->proc_name->attr.flavor == FL_MODULE)   
    || symb->attr.use_assoc;
}       
       
       
     
/* No checks for use-association in public and private statements */

try g95_add_access(symbol_attribute *a, g95_access access, locus *w) {         
         
  if (a->access == ACCESS_UNKNOWN) {        
    a->access = access;       
    return check_conflict(a, w);          
  }

  if (w == NULL) w = g95_current_locus();    
  g95_error("ACCESS specification at %L was already specified", w); 
 
  return FAILURE;         
}   
   
   
        
        
/* switch_types()-- Recursive function to switch derived types of all
 * symbol in a namespace. */       
       
static void switch_types(g95_symtree *sta, g95_symbol *start, g95_symbol *dest) { 
g95_symbol *symb;        
        
  if (sta == NULL) return;  
  
  symb = sta->n.sym;         
  if (symb->ts.type == BT_DERIVED && symb->ts.derived == start)
    symb->ts.derived = dest;     
     
  switch_types(sta->left, start, dest);
  switch_types(sta->right, start, dest);      
}         
         
         
   
   
try g95_add_dummy(symbol_attribute *attribute, locus *where) {    
    
  if (check_used(attribute, where)) return FAILURE;    
    
  /* Duplicate dummy arguments are allow due to ENTRY statements */        
        
  attribute->dummy = 1;         
  return check_conflict(attribute, where);   
}     
     
     
         
         
/* free_components()-- Given a symbol, free all of the component
 * structures and everything they point to. */      
      
static void free_components(g95_component *j) { 
g95_component *n;    
    
  for(; j; j=n) {   
    n = j->next;  
  
    g95_free_array_spec(j->as);          
    g95_free_expr(j->initializer);    
    
    g95_free(j);   
  }         
}         
         
         
  
  
try g95_add_target(symbol_attribute *attr, locus *w) {       
       
  if (check_used(attr, w)) return FAILURE;       
       
  if (attr->target) {   
    duplicate_attr("TARGET", w);      
    return FAILURE;      
  } 
 
  attr->target = 1;         
  return check_conflict(attr, w);    
}        
        
        
      
      
try g95_add_allocatable(symbol_attribute *atr, locus *w) {

  if (check_used(atr, w)) return FAILURE;         
         
  if (atr->allocatable) {         
    duplicate_attr("ALLOCATABLE", w);          
    return FAILURE; 
  }   
   
  atr->allocatable = 1;
  return check_conflict(atr, w);
}    
    
    
         
         
/* g95_match_implicit()-- Match an IMPLICIT statement, storing the
 * types for g95_set_implicit() if the statement is accepted by the
 * parser.  There is a strange looking, but legal syntactic
 * construction possible.  It looks like
 *                  IMPLICIT INTEGER (a-b) (c-d)
 *
 * This is legal if "a-b" is a constant expression that happens to
 * equal one of the legal kinds for integers.  The real problem
 * happens with an implicit specification that looks like
 *                  IMPLICIT INTEGER (a-b)
 *
 * In this case, a typespec matcher that is "greedy" (as most of the
 * matchers are) gobbles the character range as a kindspec, leaving
 * nothing left.  We therefore have to go a bit more slowly in the
 * matching process by inhibiting the kindspec checking during
 * typespec matching and checking for a kind later. */      
      
match g95_match_implicit(void) {        
g95_typespec t;
locus cur_loc;    
int w, r;       
match k;   
   
  for(r=0; r<G95_LETTERS; r++) { 
    g95_clear_ts(&new_ts[r]);  
    if (new_flag[r]) new_flag[r] = 0;     
  }   
   
  if (g95_match_eos() == MATCH_YES) {   
    g95_error("Empty IMPLICIT statement at %C");       
    return MATCH_ERROR;       
  }

  do {       
    k = g95_match_type_spec(&t, 0); /* A basic type is mandatory here */ 
    if (k == MATCH_ERROR) goto error; 
    if (k == MATCH_NO) goto syntax; 
 
    cur_loc = *g95_current_locus();          
    k = match_implicit_range(&t);          
          
    if (k == MATCH_YES) {   /* Looks like we have the <TYPE> (<RANGE>) */     
      g95_gobble_whitespace();     
      w = g95_next_char();         
      if ((w == '\n') || (w == ',')) continue;        
        
      g95_set_locus(&cur_loc);          
    }     
     
    /* Last chance-- check <TYPE> (<KIND>) (<RANGE>) */    
    
    k = g95_match_kind_spec(&t);        
    if (k == MATCH_ERROR) goto error;    
    if (k == MATCH_NO) {      
      k = g95_match_old_kind_spec(&t);    
      if (k == MATCH_ERROR) goto error;      
      if (k == MATCH_NO) goto syntax;
    }

    k = match_implicit_range(&t); 
    if (k == MATCH_ERROR) goto error; 
    if (k == MATCH_NO) goto syntax;         
             
    g95_gobble_whitespace();     
    w = g95_next_char();       
    if ((w != '\n') && (w != ',')) goto syntax;   
   
  } while(w == ',');     
     
/* An implicit statement has been fully matched at this point.  Now
 * check to see if merging the new implicit types back into the
 * existing types will work. */       
       
  for(r=0; r<G95_LETTERS; r++)         
    if (new_flag[r]) {        
      if (g95_current_ns->set_flag[r]) {    
	g95_error("Letter %c already has an IMPLICIT type at %C", r+'A');
	goto error;       
      }         
    }          
          
  return MATCH_YES;        
        
syntax: 
  g95_syntax_error(ST_IMPLICIT);   
   
error: 
  return MATCH_ERROR;    
}   
   
   
  
  
try g95_add_pointer(symbol_attribute *atr, locus *old_loc) {        
        
  if (check_used(atr, old_loc)) return FAILURE;   
   
  atr->pointer = 1; 
  return check_conflict(atr, old_loc);    
}      
      
      
    
    
try g95_add_in_common(symbol_attribute *atr, locus *where) {        
        
  if (check_used(atr, where)) return FAILURE;        
        
  /* Duplicate attribute already checked for */          
          
  atr->in_common = 1; 
  if (check_conflict(atr, where) == FAILURE) return FAILURE;      
      
  if (atr->flavor == FL_VARIABLE) return SUCCESS;

  return g95_add_flavor(atr, FL_VARIABLE, where); 
}    
    
    
 
 
/* find_gsym()-- Search a tree for the global symbol. */       
       
static g95_gsymbol *find_gsym(g95_gsymbol *sy, char *nm) {  
g95_gsymbol *g;     
     
  if (sy == NULL) return NULL;   
  if (strcmp(sy->name, nm) == 0) return sy;      
      
  g = find_gsym(sy->left, nm);    
  if (g != NULL) return g;         
         
  g = find_gsym(sy->right, nm);    
  if (g != NULL) return g; 
 
  return NULL;     
}          
          
          
  
  
/* g95_set_default_type()-- Given a pointer to a symbol, set its type
 * according to the first letter of its name.  Fails if the letter in
 * question has no default type. */         
         
try g95_set_default_type(g95_symbol *sy, int error_flag, g95_namespace *ns) {
g95_typespec *typesp;         
         
  if (sy->ts.type != BT_UNKNOWN)   
    g95_internal_error("g95_set_default_type(): symbol already has a type"); 
 
  typesp = g95_get_default_type(sy, ns);      
      
  if (typesp->type == BT_UNKNOWN) {        
    if (error_flag && !sy->attr.untyped)  
      g95_error("Symbol '%s' at %L has no IMPLICIT type", sy->name,   
		&sy->declared_at);  
  
    sy->attr.untyped = 1;          
    return FAILURE;         
  }      
      
  sy->ts = *typesp;          
  sy->attr.implicit_type = 1;     
     
  return SUCCESS;      
}         
         
         
     
     
try g95_add_function(symbol_attribute *attr, locus *w) {   
   
  if (attr->flavor != FL_PROCEDURE &&    
      g95_add_flavor(attr, FL_PROCEDURE, w) == FAILURE) return FAILURE;   
   
  attr->function = 1; 
  return check_conflict(attr, w);         
}     
     
     
   
   
/* save_symbol()-- Given a symbol, mark it as SAVEd if it is allowed */   
   
static void save_symbol(g95_symbol *sy) {         
         
  if (sy->attr.use_assoc) return;     
     
  if (sy->attr.in_common || sy->attr.dummy ||      
      sy->attr.flavor != FL_VARIABLE) return;       
       
  g95_add_save(&sy->attr, &sy->declared_at);       
}        
        
        
   
   
/* gsym_compare()-- Compare two symbols */        
        
static int gsym_compare(g95_gsymbol *s, g95_gsymbol *b) {         
         
  return strcmp(s->name, b->name);    
}       
       
       
    
    
/* g95_new_symbol()-- Allocate and initialize a new symbol node */        
        
g95_symbol *g95_new_symbol(char *n, g95_namespace *namesp) {       
g95_symbol *e;  
  
  e = g95_getmem(sizeof(g95_symbol));          
          
  g95_clear_ts(&e->ts);        
  g95_clear_attr(&e->attr);
  e->ns = namesp;      
      
  e->declared_at = *g95_current_locus();  
  
  if (strlen(n) > G95_MAX_SYMBOL_LEN)         
    g95_internal_error("new_symbol(): Symbol name too long");        
        
  strcpy(e->name, n);    
  return e;   
}          
          
          
     
     
/* traverse_uop()-- Function for traversing the user operator symtree */ 
 
static void traverse_uop(g95_symtree *s, void (*func)(g95_user_op *)) {    
    
  if (s == NULL) return;         
         
  (*func)(s->n.uop);          
          
  traverse_uop(s->left, func);          
  traverse_uop(s->right, func);    
}         
         
         
         
         
/* g95_get_st_label()-- Given a label number, search for and return a
 * pointer to the label structure, creating it if it does not exist. */      
      
g95_st_label *g95_get_st_label(int labelno) {        
g95_st_label *lp;    
    
/* First see if the label is already in this namespace.  */      
  for(lp=g95_current_ns->st_labels; lp; lp=lp->next)
    if (lp->value == labelno) break;      
  if (lp != NULL) return lp;    
      
  lp = g95_getmem(sizeof(g95_st_label));   
   
  lp->value = labelno;  
  lp->defined = ST_LABEL_UNKNOWN;    
  lp->referenced = ST_LABEL_UNKNOWN;         
         
  lp->prev = NULL;          
  lp->next = g95_current_ns->st_labels;     
  if (g95_current_ns->st_labels)          
    g95_current_ns->st_labels->prev = lp;      
  g95_current_ns->st_labels = lp;   
   
  return lp;        
}        
        
        
         
         
try g95_add_sequence(symbol_attribute *attribute, locus *loc) {          
          
  if (check_used(attribute, loc)) return FAILURE;     
     
  attribute->sequence = 1;         
  return check_conflict(attribute, loc);          
}   
   
   
      
      
/* g95_get_gsymbol()-- Get a global symbol, creating it if it doesn't
 * exist. */          
          
g95_gsymbol *g95_get_gsymbol(char *name0) {        
g95_gsymbol *s; 
 
  s = find_gsym(gsym_root, name0);          
  if (s != NULL) return s;

  s = g95_getmem(sizeof(g95_gsymbol));        
  s->type = GSYM_UNKNOWN;  
  strcpy(s->name, name0);  
  
  g95_insert_bbt(&gsym_root, s, gsym_compare);         
         
  return s;         
}


        
        
/* g95_get_uop()-- Given a name find a user operator node, creating it
 * if it doesn't exist.  These are much simpler than symbols because
 * they can't be ambiguous with one another */  
  
g95_user_op *g95_get_uop(char *nam) {    
g95_user_op *uop;      
g95_symtree *st1;       
       
  st1 = g95_find_symtree(g95_current_ns->uop_root, nam);
  if (st1 != NULL) return st1->n.uop;  
  
  st1 = g95_new_symtree(&g95_current_ns->uop_root, nam);     
     
  uop = st1->n.uop = g95_getmem(sizeof(g95_user_op)); 
  strcpy(uop->name, nam);  
  uop->access = ACCESS_UNKNOWN;    
  uop->ns = g95_current_ns;    
    
  return uop;     
}  
  
  
   
   
try g95_add_intrinsic(symbol_attribute *attribute, locus *where) {          
          
  if (check_used(attribute, where)) return FAILURE;  
  
  if (attribute->intrinsic) {   
    duplicate_attr("INTRINSIC", where);       
    return FAILURE;         
  }   
   
  attribute->intrinsic = 1;  
  
  return check_conflict(attribute, where);          
}          
          
          
        
        
/* delete_symtree()-- delete a symbol from the tree.  Does not free the
 * symbol itself! */     
     
static void delete_symtree(g95_symtree **root, char *n) {    
g95_symtree sta, *st0;   
   
  st0 = g95_find_symtree(*root, n);        
       
  strcpy(sta.name, n);   
  g95_delete_bbt(root, &sta, g95_compare_symtree);      
      
  g95_free(st0);       
}  
  
  
      
      
/* g95_compare_attr()-- Compares two attributes */        
        
int g95_compare_attr(symbol_attribute *b, symbol_attribute *f) {          
          
  return b->allocatable == f->allocatable && 
    b->dimension == f->dimension     && b->external == f->external &&    
    b->intrinsic == f->intrinsic     && b->optional == f->optional &&
    b->pointer == f->pointer         && b->save == f->save && 
    b->target == f->target           && b->dummy == f->dummy &&
    b->result_var == f->result_var   && b->entry == f->entry &&          
    b->data == f->data               && b->use_assoc == f->use_assoc &&
    b->in_namelist == f->in_namelist && b->in_common == f->in_common &&          
    b->function == f->function       && b->subroutine == f->subroutine &&  
    b->sequence == f->sequence       && b->elemental == f->elemental &&
    b->pure == f->pure               && b->recursive == f->recursive && 
    b->access == f->access           && b->intent == f->intent &&     
    b->flavor == f->flavor           && b->proc == f->proc &&
    b->generic == f->generic;      
}        
        
        


/* save_commons()-- Mark common blocks as saved */        
        
static void save_commons(g95_symtree *st1) {          
          
  if (st1 == NULL) return;    
    
  save_commons(st1->left);          
  save_commons(st1->right);

  st1->n.common->saved = 1;      
}  
  
  
         
         
/* free_common_tree()-- Recursively free a list of common head nodes */   
   
static void free_common_tree(g95_symtree *sta) {

  if (sta == NULL) return;   
   
  free_common_tree(sta->left);          
  free_common_tree(sta->right);      
      
  g95_free(sta->n.common);   
  g95_free(sta);       
}       
       
       


/* free_uop_tree()-- Recursive function that deletes an entire
 * red-black tree and all the user operator nodes that it contains. */     
     
static void free_uop_tree(g95_symtree *rb) { 
 
  if (rb == NULL) return;   
   
  free_uop_tree(rb->left); 
  free_uop_tree(rb->right);   
   
  g95_free_interface(rb->n.uop->operator);    
    
  g95_free(rb->n.uop);        
  g95_free(rb);    
}       
       
       


/* g95_free_symbol()-- Remove a g95_symbol structure and everything it
 * points to. */       
       
void g95_free_symbol(g95_symbol *symb) {          
          
  if (symb == NULL) return;          
          
  g95_free_array_spec(symb->as);  
  
  free_components(symb->components);        
        
  g95_free_expr(symb->value);     
     
  g95_free_namelist(symb->namelist);         
         
  g95_free_namespace(symb->formal_ns);    
    
  g95_free_interface(symb->generic);         
         
  g95_free_formal_arglist(symb->formal);         
         
  g95_free(symb); 
}        
        
        
        
        
/* g95_add_flavor()-- Flavors are special because some flavors are not
 * what fortran considers attributes and can be reaffirmed multiple
 * times. */    
    
try g95_add_flavor(symbol_attribute *a, sym_flavor l, locus *w) {      
      
  if ((l == FL_PROGRAM || l == FL_BLOCK_DATA || l == FL_MODULE ||
       l == FL_PARAMETER || l == FL_LABEL || l == FL_DERIVED ||       
       l == FL_NAMELIST) && check_used(a, w)) return FAILURE;        
        
  if (a->flavor == l && l == FL_VARIABLE) return SUCCESS;         
         
  if (a->flavor != FL_UNKNOWN) {  
    if (w == NULL) w = g95_current_locus();      
      
    g95_error("%s attribute conflicts with %s attribute at %L",
	      g95_code2string(flavors, a->flavor),    
	      g95_code2string(flavors, l), w);       
       
    return FAILURE;   
  }          
          
  a->flavor = l; 
 
  return check_conflict(a, w);  
}      
      
      
         
         
try g95_add_intent(symbol_attribute *attribute, sym_intent intent, locus *w) {          
          
  if (check_used(attribute, w)) return FAILURE;      
      
  if (attribute->intent == INTENT_UNKNOWN) {    
    attribute->intent = intent;     
    return check_conflict(attribute, w);   
  }         
         
  if (w == NULL) w = g95_current_locus();          
          
  g95_error("INTENT (%s) conflicts with INTENT(%s) at %L",      
	    g95_intent_string(attribute->intent),
	    g95_intent_string(intent), w);  
  
  return FAILURE;     
} 
 
 
       
       
try g95_add_generic(symbol_attribute *a, locus *w) {          
          
  if (a->flavor != FL_PROCEDURE &&         
      g95_add_flavor(a, FL_PROCEDURE, w) == FAILURE) return FAILURE;          
          
  a->generic = 1;     
  return check_conflict(a, w);     
}


         
         
/* g95_save_symbol_data()-- Save symbol with the information necessary
 * to back it out. */      
      
void g95_save_symbol_data(g95_symbol *sym) {

  if (sym->new || sym->old_symbol != NULL) return;      
      
  sym->old_symbol = g95_getmem(sizeof(g95_symbol)); 
  *(sym->old_symbol) = *sym;        
        
  sym->tlink = changed_syms; 
  changed_syms = sym;        
}        
        
        
       
       
try g95_add_in_namelist(symbol_attribute *a, locus *where) {    
    
  a->in_namelist = 1;         
  return check_conflict(a, where);      
}  
  
  
       
       
try g95_add_procedure(symbol_attribute *attr, procedure_type t, locus *pos) {   
   
  if (check_used(attr, pos)) return FAILURE; 
 
  if (attr->flavor != FL_PROCEDURE &&     
      g95_add_flavor(attr, FL_PROCEDURE, pos) == FAILURE) return FAILURE;       
       
  if (pos == NULL) pos = g95_current_locus();          
          
  if (attr->proc != PROC_UNKNOWN) {      
    g95_error("%s procedure at %L is already %s %s procedure",  
	      g95_code2string(procedures, t), pos,         
	      g95_article(g95_code2string(procedures, attr->proc)),      
	      g95_code2string(procedures, attr->proc));          
          
    return FAILURE;    
  }     
     
  attr->proc = t;         
         
/* Statement functions are always scalar and functions */     
     
  if (t == PROC_ST_FUNCTION &&        
      ((!attr->function && g95_add_function(attr, pos) == FAILURE)           
       || attr->dimension)) return FAILURE;   
   
  return check_conflict(attr, pos); 
}   
   
   
      
      
/* g95_compare_symtree()-- Comparison function for symtree nodes. */   
   
int g95_compare_symtree(g95_symtree *st1, g95_symtree *st2) {        
        
  return strcmp(st1->name, st2->name);       
}       
       
       
          
          
/* g95_commit_symbols()-- Makes the changes made in the current
 * statement permanent-- gets rid of undo information. */       
       
void g95_commit_symbols(void) { 
g95_symbol *o, *x;         
g95_symtree *f, *a;     
     
#if 0
  if (changed_syms != NULL) g95_status("Committing symbols\n");         
#endif
 
  for(o=changed_syms; o; o=x) {   
    x = o->tlink;          
    o->tlink = NULL;        
    o->mark = 0;      
    o->new = 0;     
     
    if (o->old_symbol != NULL) {
      g95_free(o->old_symbol);       
      o->old_symbol = NULL;  
    }      
  }  
  
  changed_syms = NULL;      
      
  for(f=changed_st; f; f=a) {     
    a = f->link;          
    f->link = NULL;   
  }     
     
  changed_st = NULL;   
}      
      
      
 
 
try g95_add_recursive(symbol_attribute *attribute, locus *pos) {   
   
  if (check_used(attribute, pos)) return FAILURE;      
      
  attribute->recursive = 1;     
  return check_conflict(attribute, pos);        
}     
     
     
      
      
/* g95_new_symtree()-- Allocate a new symtree node and associate it
 * with the new symbol. */        
        
g95_symtree *g95_new_symtree(g95_symtree **root, char *name0) {        
g95_symtree *st0;

  st0 = g95_getmem(sizeof(g95_symtree));      
  strcpy(st0->name, name0);      
      
  g95_insert_bbt(root, st0, g95_compare_symtree);  
  return st0;
}        
        
        


/* g95_use_derived()-- This subroutine is called when a derived type
 * is used in order to make the final determination about which
 * version to use.  The standard requires that a type be defined
 * before it is 'used', but such types can appear in IMPLICIT
 * statements before the actual definition.  'Using' in this context
 * means declaring a variable to be that type or using the type
 * constructor.
 *
 * If a type is used and the components haven't been defined, then we
 * have to have a derived type in a parent unit.  We find the node in
 * the other namespace and point the symtree node in this namespace to
 * that node.  Further reference to this name point to the correct
 * node.  If we can't find the node in a parent namespace, then have
 * an error.
 *
 * This subroutine takes a pointer to a symbol node and returns a
 * pointer to the translated node or NULL for an error.  Usually there
 * is no translation and we return the node we were passed.  */  
  
g95_symbol *g95_use_derived(g95_symbol *symb) {   
g95_symbol *s, *h;     
g95_typespec *a;        
g95_symtree *sta;        
int r;         
         
  if (symb->components != NULL) return symb;   /* Already defined */ 
 
  if (symb->ns->parent == NULL) goto bad;  
  
  if (g95_find_symbol(symb->name, symb->ns->parent, 1, &s)) {        
    g95_error("Symbol '%s' at %C is ambiguous", symb->name);    
    return NULL;         
  }    
    
  if (s == NULL || s->attr.flavor != FL_DERIVED) goto bad;    
    
  /* Get rid of symbol sym, translating all references to s */    
    
  for(r=0; r<G95_LETTERS; r++) {       
    a = &symb->ns->default_type[r];          
    if (a->derived == symb) a->derived = s;   
  }     
     
  sta = g95_find_symtree(symb->ns->sym_root, symb->name);     
  sta->n.sym = s;       
       
  s->refs++;   
   
  /* Unlink from list of modified symbols */        
        
  if (changed_syms == symb)
    changed_syms = symb->tlink;          
  else    
    for(h=changed_syms; h; h=h->tlink)    
      if (h->tlink == symb) {
	h->tlink = symb->tlink; 
	break;       
      }      
      
  switch_types(symb->ns->sym_root, symb, s);          
          
  /* TODO: Also have to replace sym -> s in other lists like
   * namelists, common lists and interface lists.  */        
        
  g95_free_symbol(symb);          
          
  return s;     
     
 bad:          
  g95_error("Derived type '%s' at %C is being used before it is defined",         
	    symb->name);          
  return NULL;  
}          
          
          
   
   
/* ambiguous_symbol()-- Generate an error if a symbol is ambiguous. */ 
 
static void ambiguous_symbol(char *nam, g95_symtree *sta) {      
      
  if (sta->n.sym->module[0])
    g95_error("Name '%s' at %C is an ambiguous reference to '%s' "   
	      "from module '%s'", nam, sta->n.sym->name,   
	      sta->n.sym->module);         
  else     
    g95_error("Name '%s' at %C is an ambiguous reference to '%s' "         
	      "from current program unit", nam, sta->n.sym->name);     
}     
     
     
      
      
/* g95_get_symbol()-- Given a name, find a symbol, or create it if it does
 * not exist yet in the current namespace.
 * If the symbol is found we make sure that it's OK.
 *
 * The integer return code indicates
 *  0   All OK
 *  1   The symbol name was ambiguous
 *  2   The name meant to be established was already host associated.
 *
 * So if nonzero, then an error was issued.  This subroutine assumes
 * that a returned symbol is about to be changed and saves it. */ 
 
int g95_get_symbol(char *name0, g95_namespace *names, g95_symbol **result) { 
g95_symtree *st0;   
g95_symbol *o; 
 
  /* This doesn't usually happen during resolution.  */         
  if (names == NULL) names = g95_current_ns;    
    
  /* Try to find the symbol. */  
  st0 = g95_find_symtree(names->sym_root, name0);        
        
  if (st0 == NULL) {         /* If not there, create a new symbol */
    o = g95_new_symbol(name0, names);      
     
    o->old_symbol = NULL;   /* Add to the list of tentative symbols. */   
    o->tlink = changed_syms;   
    o->mark = 1; 
    o->new = 1;    
    changed_syms = o; 
 
    st0 = g95_new_symtree(&names->sym_root, name0);      
    st0->n.sym = o;  
    o->refs++;         
         
  } else {    /* Make sure the existing symbol is OK */ 
    if (st0->ambiguous) {
      ambiguous_symbol(name0, st0);    
      return 1;    
    }

    o = st0->n.sym;

    if (o->ns != names && (!o->attr.function || names->proc_name != o) &&     
	!o->attr.entry) {         
      /* Symbol is from another namespace */     
      g95_error("Symbol '%s' at %C has already been host associated", name0); 
      return 2;
    }         
         
    o->mark = 1;          
          
    g95_save_symbol_data(o);      /* Copy in case this symbol is changed */    
  }         
         
  *result = o;
  return 0;        
} 
 
 
 
 
/* clear_sym_mark()-- Clear mark bits from symbol nodes associated
 * with a symtree node */

static void clear_sym_mark(g95_symtree *st0) {   
   
  st0->n.sym->mark = 0; 
}          
          
          
   
   
/* g95_free_namespace()-- Free a namespace structure and everything
 * below it.  Interface lists associated with intrinsic operators are
 * not freed.  These are taken care of when a specific name is freed. */  
  
void g95_free_namespace(g95_namespace *ns) {  
g95_charlen *charlen, *cl2;          
g95_namespace *z, *c;          
int f;          
          
  if (ns == NULL) return;  
  
  g95_free_statements(ns->code); 
 
  free_sym_tree(ns->sym_root);     
  free_uop_tree(ns->uop_root);   
  free_common_tree(ns->common_root);       
       
  for(charlen=ns->cl_list; charlen; charlen=cl2) {       
    cl2 = charlen->next;     
    g95_free_expr(charlen->length);         
    g95_free(charlen);     
  }          
          
  free_st_labels(ns->st_labels);        
        
  g95_free_equiv(ns->equiv);          
          
  for(f=0; f<G95_INTRINSIC_OPS; f++)        
    g95_free_interface(ns->operator[f]);   
   
  g95_free_data(ns->data);     
  z = ns->contained;
  g95_free(ns);  
  
  /* Recursively free any contained namespaces */   
   
  while(z != NULL) {       
    c = z;      
    z = z->sibling; 
 
    g95_free_namespace(c);        
  }     
}        
        
        
      
      
/* traverse_symtree()-- Recursively traverse the symtree nodes. */         
         
static void traverse_symtree(g95_symtree *st0, void (*func)(g95_symtree *)) {

  if (st0 != NULL) { 
    (*func)(st0);        
        
    traverse_symtree(st0->left, func);      
    traverse_symtree(st0->right, func);
  }  
} 
 
 
   
   
static void show_indent(void) {         
int t;  
  
  g95_status_char('\n');         
  for(t=0; t<2*show_level; t++)     
    g95_status_char(' ');      
}          
          
          
          
          
/* g95_traverse_ns()-- Call a given function for all symbols in the
 * namespace.  We take care that each g95_symbol node is called
 * exactly once. */

void g95_traverse_ns(g95_namespace *names, void (*func)(g95_symbol *)) {        
        
  g95_traverse_symtree(names, clear_sym_mark);     
     
  traverse_ns(names->sym_root, func);  
}  
  
  
         
         
/* g95_undo_symbols()-- Undoes all the changes made to symbols in the
 * current statement.  This subroutine is made simpler due to the fact
 * that attributes are never removed once added. */    
    
void g95_undo_symbols(void) {        
g95_symbol *h, *b, *old;      
g95_symtree *l, *y;      
      
/* if (changed_syms != NULL) g95_status("Undoing symbols\n"); */ 
 
  for(h=changed_syms; h; h=b) {    
    b = h->tlink; 
    /* g95_status("Undoing %s\n", p->name); */    
    
    if (h->new) {  /* Symbol was new */  
      delete_symtree(&h->ns->sym_root, h->name);    
    
      h->refs--; 
      if (h->refs < 0) g95_internal_error("g95_undo_symbols(): Negative refs");          
      if (h->refs == 0) g95_free_symbol(h);     
      continue;       
    }          
          
/* Restore previous state of symbol.  Just copy simple stuff */        
        
    h->mark = 0;
    old = h->old_symbol;     
     
    h->ts.type = old->ts.type;    
    h->ts.kind = old->ts.kind; 
 
    h->attr = old->attr;          
          
    if (h->value != old->value) {       
      g95_free_expr(old->value);
      h->value = NULL;          
    }  
  
    if (h->as != old->as) {
      if (h->as) g95_free_array_spec(h->as);      
      h->as = old->as;     
    }    
    
    h->generic = old->generic;    
    h->component_access = old->component_access;         
         
    if (h->namelist != NULL && old->namelist == NULL) {         
      g95_free_namelist(h->namelist);   
      h->namelist = NULL;  
    } else {  
  
      if (h->namelist_tail != old->namelist_tail) {        
	g95_free_namelist(old->namelist_tail); 
	old->namelist_tail->next = NULL;          
      } 
    }         
         
    h->namelist_tail = old->namelist_tail;         
         
    if (h->formal != old->formal) {        
      g95_free_formal_arglist(h->formal); 
      h->formal = old->formal;   
    }

    g95_free(h->old_symbol);     
    h->old_symbol = NULL;     
    h->tlink = NULL; 
  }   
   
  changed_syms = NULL;      
      
  /* Unlink host associated symtrees */  
  
  for(l=changed_st; l; l=y) {
    y = l->link;        
        
    g95_delete_bbt(&g95_current_ns->sym_root, l, g95_compare_symtree);     
    g95_free(l);       
  }

  changed_st = NULL;
}


      
      
/* g95_check_pointer_assign()-- Check that a pointer assignment is OK.
 * We first check lvalue, and we only check rvalue if it's not an
 * assignment to NULL() or a NULLIFY statement. */          
          
try g95_check_pointer_assign(g95_expr *lvalue, g95_expr *rvalue) { 
symbol_attribute atr; 
int is_pure;        
        
  if (lvalue->symbol->ts.type == BT_UNKNOWN) {
    g95_error("Pointer assignment target is not a POINTER at %L", 
	      &lvalue->where);  
    return FAILURE;   
  }         
         
  atr = g95_variable_attr(lvalue, NULL);          
  if (!atr.pointer) { 
    g95_error("Pointer assignment to non-POINTER at %L",          
	      &lvalue->where);     
    return FAILURE;     
  }          
          
  is_pure = g95_pure(NULL);     
     
  if (is_pure && g95_impure_variable(lvalue->symbol)) {          
    g95_error("Bad pointer object in PURE procedure at %L", &lvalue->where); 
    return FAILURE;        
  }      
      
  /* If rvalue is a NULL() or NULLIFY, we're done. Otherwise the type,
   * kind, etc for lvalue and rvalue must match, and rvalue must be a 
   * pure variable if we're in a pure function. */         
         
  if (rvalue->type != EXPR_NULL) {

    if (!g95_compare_types(&lvalue->ts, &rvalue->ts)) {   
      g95_error("Different types in pointer assignment at %L",      
                &lvalue->where);      
      return FAILURE;
    }        
        
    if (lvalue->ts.kind != rvalue->ts.kind) {       
      g95_error("Different kind type parameters in pointer assignment at %L",        
                &lvalue->where);   
      return FAILURE;     
    }       
       
    atr = g95_expr_attr(rvalue);          
    if (!atr.target && !atr.pointer) {  
      g95_error("Pointer assignment target is neither TARGET nor POINTER at "         
		"%L", &rvalue->where);  
      return FAILURE;       
    }

    if (is_pure && g95_impure_variable(rvalue->symbol)) {         
      g95_error("Bad target in pointer assignment in PURE procedure at %L",        
		&rvalue->where);  
    }          
  }       
       
  return SUCCESS;    
}


     
     
/* g95_traverse_user_op()-- Traverse the tree of user operator nodes.  */        
        
void g95_traverse_user_op(g95_namespace *names, void (*func)(g95_user_op *)) {

  traverse_uop(names->uop_root, func);
}  
  
  
       
       
/* g95_save_all()-- Mark those symbols which can be SAVEd as such. */        
        
void g95_save_all(g95_namespace *namesp) {

  g95_traverse_ns(namesp, save_symbol);          
          
  save_commons(namesp->common_root);    
}  
  
  
     
     
/* g95_find_symbol()-- search for a symbol starting in the current
 * namespace, resorting to any parent namespaces if requested by a
 * nonzero parent_flag.  Returns nonzero if the symbol is ambiguous.
 * If a symbol that is found is changed, then g95_save_symbol() data
 * must be called, since generally we return symbols that are not
 * subsequently changed. */       
       
int g95_find_symbol(char *nam, g95_namespace *ns, int parent_flag,  
		    g95_symbol **r) {  
g95_symtree *st; 
 
  if (ns == NULL) ns = g95_current_ns;    
    
  do {  
    st = g95_find_symtree(ns->sym_root, nam);  
    if (st != NULL) {         
      *r = st->n.sym;   
      if (st->ambiguous) {
	ambiguous_symbol(nam, st); 
	return 1;   
      }       
       
      return 0; 
    }          
          
    if (!parent_flag) break;         
         
    ns = ns->parent; 
  } while (ns != NULL);        
        
  *r = NULL;        
  return 0;
}  
  
  


/* g95_show_symbol()-- Show a symbol.  If a symbol is an ENTRY,
 * SUBROUTINE or FUNCTION, we show the interface.  Information needed
 * to reconstruct the list of specific interfaces associated with a
 * generic symbol is done within that symbol. */       
       
void g95_show_symbol(g95_symbol *sym) {  
g95_formal_arglist *f;         
g95_interface *intf;   
   
  if (sym == NULL) return;      
      
  show_indent();       
       
  g95_status("symbol %s ", sym->name);          
  g95_show_typespec(&sym->ts);
  g95_show_attr(&sym->attr);  
  
  if (sym->value) { 
    show_indent();     
    g95_status("value: ");       
    g95_show_expr(sym->value);  
  }    
    
  if (sym->as) {     
    show_indent(); 
    g95_status("Array spec:");         
    g95_show_array_spec(sym->as);       
  } 
 
  if (sym->generic) {        
    show_indent();        
    g95_status("Generic interfaces:");  
    for(intf=sym->generic; intf; intf=intf->next)         
      g95_status(" %s", intf->sym->name);         
  }  
  
  if (sym->result) {       
    show_indent();         
    g95_status("result: %s", sym->result->name);  
  }     
     
  if (sym->components) {  
    show_indent();   
    g95_status("components: ");     
    g95_show_components(sym);   
  }         
         
  if (sym->formal) {     
    show_indent(); 
    g95_status("Formal arglist:");  
  
    for(f=sym->formal; f; f=f->next)          
      g95_status(" %s", f->sym->name);   
  }       
       
  if (sym->formal_ns) {    
    show_indent();   
    g95_status("Formal namespace");   
    g95_show_namespace(sym->formal_ns);     
  }   
   
  g95_status_char('\n');         
}          
          
          
          
          
void g95_traverse_symtree(g95_namespace *namesp, void (*func)(g95_symtree *)) {         
         
  traverse_symtree(namesp->sym_root, func);      
}  
  
  
   
   
/* g95_global_used()-- Come here to complain about a global symbol
 * already in use as something else. */   
   
void g95_global_used(g95_gsymbol *sy, locus *pos) {          
char *nam;     
     
  if (pos == NULL) pos = g95_current_locus();      
      
  switch(sy->type) { 
  case GSYM_PROGRAM:      nam = "PROGRAM";     break;     
  case GSYM_FUNCTION:     nam = "FUNCTION";    break;       
  case GSYM_SUBROUTINE:   nam = "SUBROUTINE";  break;          
  case GSYM_COMMON:       nam = "COMMON";      break;      
  case GSYM_BLOCK_DATA:   nam = "BLOCK DATA";  break;        
  case GSYM_MODULE:       nam = "MODULE";      break; 
  default: 
    g95_internal_error("g95_gsymbol_type(): Bad type");      
    nam = NULL;    
  }

  g95_error("Global name '%s' at %L is already being used as a %s at %L",
	    g95_new_block->name, pos, nam, &sy->where);     
}          
         
         
/* show_symtree()-- Worker function to display the symbol tree */ 
 
static void show_symtree(g95_symtree *sta) {    
    
  show_indent();          
  g95_status("symtree: %s  Ambig %d", sta->name, sta->ambiguous);   
   
  if (sta->n.sym->ns != g95_current_ns)          
    g95_status(" from namespace %s", sta->n.sym->ns->proc_name->name);       
  else       
    g95_show_symbol(sta->n.sym);     
}   
   
   
         
         
static void show_uop(g95_user_op *u) {  
g95_interface *i;       
       
  show_indent();      
  g95_status("%s:", u->name);
  
  for(i=u->operator; i; i=i->next)        
    g95_status(" %s", i->sym->name);   
}          
          
          
       
       
/* g95_show_namespace()-- Show a namespace */    
    
void g95_show_namespace(g95_namespace *ns) {
g95_interface *inter;        
g95_namespace *save;          
int s;         
         
  save = g95_current_ns;     
  show_level++;         
        
  show_indent();      
  g95_status("Namespace:");       
       
  if (ns != NULL) {   
    for(s=0; s<G95_LETTERS; s++) {    
      g95_status(" %c: ", s+'A');      
      g95_show_typespec(&ns->default_type[s]);          
    }    
    
    if (ns->proc_name != NULL) {   
      show_indent();      
      g95_status("procedure name = %s", ns->proc_name->name);         
    }   
   
    g95_traverse_symtree(ns, clear_sym_mark);      
      
    g95_current_ns = ns;   
    g95_traverse_symtree(ns, show_symtree);      
      
    for(s=0; s<G95_INTRINSIC_OPS; s++) {    /* User operator interfaces */  
      inter = ns->operator[s];
      if (inter == NULL) continue;  
  
      show_indent();   
      g95_status("Operator interfaces for %s:", g95_op2string(s));     
     
      for(; inter; inter=inter->next)         
	g95_status(" %s", inter->sym->name);    
    }        
        
    if (ns->uop_root != NULL) {   
      show_indent();  
      g95_status("User operators:\n");      
      g95_traverse_user_op(ns, show_uop);
    }     
  }     
     
  g95_status_char('\n');         
  g95_status_char('\n'); 
 
#ifdef G95_DEBUG
  g95_show_code(0, ns->code);         
#endif
   
  for(ns=ns->contained; ns; ns=ns->sibling) { 
    g95_status("CONTAINS\n");   
    g95_show_namespace(ns);     
  }

  show_level--;
  g95_status_char('\n');   
  g95_current_ns = save; 
}        
        
        
