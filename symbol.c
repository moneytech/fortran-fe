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
 
 
 
 
void g95_show_components(g95_symbol *s) {         
g95_component *e;       
       
  for(e=s->components; e; e=e->next) {
    g95_status("(%s ", e->name);   
    g95_show_typespec(&e->ts);        
    if (e->pointer) g95_status(" POINTER");      
    if (e->dimension) g95_status(" DIMENSION");  
    g95_status_char(' ');      
    g95_show_array_spec(e->as);        
    g95_status(")");     
    if (e->next != NULL) g95_status_char(' ');
  }     
}    
    
    
        
/* check_conflict()-- This is a generic conflict-checker.  We do this
 * to avoid having a single conflict in two places. */      
      
#define conf(a, b) \
    if (attr->a && attr->b) { attr1 = a; attr2 = b; goto conflict; }
#define conf2(a) if (attr->a) { attr2 = a; goto conflict; }
         
static try check_conflict(symbol_attribute *attr, locus *pos) {          
char *attr1, *attr2; 
 
static char *dummy = "DUMMY", *save = "SAVE", *pointer = "POINTER",   
  *target = "TARGET", *external = "EXTERNAL", *intent = "INTENT",         
  *intrinsic = "INTRINSIC", *allocatable = "ALLOCATABLE",         
  *elemental = "ELEMENTAL", *private = "PRIVATE", *recursive = "RECURSIVE",          
  *in_common = "COMMON", *result_var = "RESULT", *in_namelist = "NAMELIST",    
  *public = "PUBLIC", *optional = "OPTIONAL", *entry = "ENTRY",         
  *function = "FUNCTION", *subroutine = "SUBROUTINE", *dimension = "DIMENSION",  
  *use_assoc = "USE";      
      
  if (pos == NULL) pos = g95_current_locus();  
  
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
		attr1, pos);       
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
	    pos);       
  return FAILURE;
}

#undef conf
#undef conf2
         
         
     
     
/* g95_check_conformance()-- Given two expressions, make sure that
 * the arrays are conformable. */   
   
try g95_check_conformance(const char *optype, g95_expr *op1, g95_expr *op0) {   
int op1_flag, op2_flag, m;     
mpz_t op1_size, op2_size;    
try x; 
 
  if (op1->rank == 0 || op0->rank == 0) return SUCCESS;       
       
  if (op1->rank != op0->rank) {      
    g95_error("Incompatible ranks in %s at %L", optype, &op1->where);   
    return FAILURE;    
  }  
  
  x = SUCCESS;       
       
  for(m=0; m<op1->rank; m++) {         
    op1_flag = g95_array_dimen_size(op1, m, &op1_size) == SUCCESS;    
    op2_flag = g95_array_dimen_size(op0, m, &op2_size) == SUCCESS;         
             
    if (op1_flag && op2_flag && mpz_cmp(op1_size, op2_size) != 0) {
      g95_error("%s at %L has different shape on dimension %d (%d/%d)",          
		optype, &op1->where, m+1, (int) mpz_get_si(op1_size),
		(int) mpz_get_si(op2_size));  
  
      x = FAILURE;          
    }        
        
    if (op1_flag) mpz_clear(op1_size);  
    if (op2_flag) mpz_clear(op2_size);        
        
    if (x == FAILURE) return FAILURE;     
  }        
        
  return SUCCESS;  
}  
  
  
  
  
/* check_used()-- Common subroutine called by attribute changing
 * subroutines in order to prevent them from changing a symbol that
 * has been use-associated.  Returns zero if it is OK to change the
 * symbol, nonzero if not. */

static int check_used(symbol_attribute *a, locus *where) {       
       
  if (a->use_assoc == 0) return 0;

  if (where == NULL) where = g95_current_locus();      
      
  g95_error("Cannot change attributes of USE-associated symbol at %L", where);          
          
  return 1;  
}      
      
      
     
     
try g95_add_generic(symbol_attribute *a, locus *old_loc) {     
     
  if (a->flavor != FL_PROCEDURE &&
      g95_add_flavor(a, FL_PROCEDURE, old_loc) == FAILURE) return FAILURE;      
      
  a->generic = 1;     
  return check_conflict(a, old_loc);   
}         
         
         
      
      
/* traverse_uop()-- Function for traversing the user operator symtree */        
        
static void traverse_uop(g95_symtree *sta, void (*func)(g95_user_op *)) {        
        
  if (sta == NULL) return;        
        
  (*func)(sta->n.uop);         
         
  traverse_uop(sta->left, func);    
  traverse_uop(sta->right, func);     
}




/* free_common_tree()-- Recursively free a list of common head nodes */         
         
static void free_common_tree(g95_symtree *s) {      
      
  if (s == NULL) return; 
 
  free_common_tree(s->left);        
  free_common_tree(s->right); 
 
  g95_free(s->n.common);          
  g95_free(s);          
}       
       
       
 
 
/* g95_traverse_user_op()-- Traverse the tree of user operator nodes.  */    
    
void g95_traverse_user_op(g95_namespace *names, void (*func)(g95_user_op *)) {     
     
  traverse_uop(names->uop_root, func);       
} 
 
 
     
     
/* g95_compare_symtree()-- Comparison function for symtree nodes. */          
          
int g95_compare_symtree(g95_symtree *st1, g95_symtree *st2) {       
       
  return strcmp(st1->name, st2->name);   
}       
       
       
      
      
try g95_add_dummy(symbol_attribute *a, locus *where) {        
        
  if (check_used(a, where)) return FAILURE;

  /* Duplicate dummy arguments are allow due to ENTRY statements */

  a->dummy = 1;         
  return check_conflict(a, where); 
}          
          
          
       
       
/* g95_set_implicit_none()-- Handle a correctly parsed IMPLICIT NONE */     
     
void g95_set_implicit_none(void) {      
int d;    
    
  for(d='a'; d<='z'; d++) {          
    g95_clear_ts(&g95_current_ns->default_type[d - 'a']);    
    g95_current_ns->set_flag[d - 'a'] = 1;         
  }      
}        
        
        


/* g95_set_implicit()-- Sets the implicit types parsed by the previous
 * g95_match_implicit(). */  
  
void g95_set_implicit(void) {
int j;

  for(j=0; j<G95_LETTERS; j++)        
    if (new_flag[j]) {          
      g95_current_ns->default_type[j] = new_ts[j];     
      g95_current_ns->set_flag[j] = 1; 
    }         
}    
    
    
    
    
void g95_symbol_init_2(void) {        
        
  g95_current_ns = g95_get_namespace(NULL);     
}          
          
          
        
        
try g95_add_result(symbol_attribute *attribute, locus *where) {  
  
  if (check_used(attribute, where)) return FAILURE;  
  
  attribute->result_var = 1;      
  return check_conflict(attribute, where);       
}      
      
      
          
/* free_st_labels()-- Free a whole list of g95_st_label structures.  */ 
 
static void free_st_labels(g95_st_label *o) {  
g95_st_label *c;     
     
  for(; o; o=c) {
    c = o->next;       
    if (o->format != NULL) g95_free_expr(o->format);          
    g95_free(o);        
  }        
}          
          
          
        
        
/* g95_check_assign_symbol()-- Relative of g95_check_assign() except
 * that the lvalue is a single symbol. */       
       
try g95_check_assign_symbol(g95_symbol *s, g95_expr *rvalue) {  
g95_expr lvalue;         
         
  memset(&lvalue, '\0', sizeof(g95_expr));        
        
  lvalue.type = EXPR_VARIABLE;         
  lvalue.ts = s->ts;       
  if (s->as) lvalue.rank = s->as->rank;         
  lvalue.symbol = s;    
  lvalue.where = s->declared_at;         
         
  return g95_check_assign(&lvalue, rvalue, 1); 
}        
        
        
      
      
/* g95_commit_symbols()-- Makes the changes made in the current
 * statement permanent-- gets rid of undo information. */       
       
void g95_commit_symbols(void) {       
g95_symbol *y, *g;  
g95_symtree *f, *a;        
        
#if 0
  if (changed_syms != NULL) g95_status("Committing symbols\n");    
#endif
 
  for(y=changed_syms; y; y=g) {     
    g = y->tlink;
    y->tlink = NULL;        
    y->mark = 0;          
    y->new = 0;       
       
    if (y->old_symbol != NULL) {    
      g95_free(y->old_symbol);  
      y->old_symbol = NULL;        
    }         
  } 
 
  changed_syms = NULL;      
      
  for(f=changed_st; f; f=a) {      
    a = f->link; 
    f->link = NULL;         
  } 
 
  changed_st = NULL;     
}      
      
      
 
 
try g95_add_recursive(symbol_attribute *attribute, locus *loc) {   
   
  if (check_used(attribute, loc)) return FAILURE;

  attribute->recursive = 1;   
  return check_conflict(attribute, loc);     
}   
   
   
     
     
/* g95_add_type()-- Add a type to a symbol. */    
    
try g95_add_type(g95_symbol *symbol, g95_typespec *typ, locus *w) {          
sym_flavor flavor;        
        
  if (w == NULL) w = g95_current_locus();  
  
  if (symbol->ts.type != BT_UNKNOWN) {  
    g95_error("Symbol '%s' at %L already has basic type of %s", symbol->name,          
	      w, g95_basic_typename(symbol->ts.type));         
    return FAILURE;      
  }     
     
  flavor = symbol->attr.flavor;      
      
  if (flavor == FL_PROGRAM || flavor == FL_BLOCK_DATA || flavor == FL_MODULE ||    
      flavor == FL_LABEL || (flavor == FL_PROCEDURE && symbol->attr.subroutine) ||          
      flavor == FL_DERIVED || flavor == FL_NAMELIST) {         
    g95_error("Symbol '%s' at %L cannot have a type", symbol->name, w);    
    return FAILURE;    
  } 
 
  symbol->ts = *typ;       
  return SUCCESS;        
}         
         
         
          
          
try g95_add_intent(symbol_attribute *a, sym_intent intent, locus *old_loc) {  
  
  if (check_used(a, old_loc)) return FAILURE;  
  
  if (a->intent == INTENT_UNKNOWN) {         
    a->intent = intent;       
    return check_conflict(a, old_loc); 
  }    
    
  if (old_loc == NULL) old_loc = g95_current_locus();   
   
  g95_error("INTENT (%s) conflicts with INTENT(%s) at %L",   
	    g95_intent_string(a->intent),         
	    g95_intent_string(intent), old_loc);      
      
  return FAILURE;        
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
   
   
    
    
/* g95_new_symbol()-- Allocate and initialize a new symbol node */          
          
g95_symbol *g95_new_symbol(char *name, g95_namespace *ns) {
g95_symbol *z;         
         
  z = g95_getmem(sizeof(g95_symbol));      
      
  g95_clear_ts(&z->ts);        
  g95_clear_attr(&z->attr);   
  z->ns = ns;

  z->declared_at = *g95_current_locus();         
         
  if (strlen(name) > G95_MAX_SYMBOL_LEN) 
    g95_internal_error("new_symbol(): Symbol name too long");      
      
  strcpy(z->name, name);  
  return z;      
}        
        
        
        
        
/* g95_missing_attr()-- Check for missing attributes in the new
 * symbol.  Currently does nothing, but it's not clear that it is
 * unnecessary yet.  AEV 7/4/01 */   
   
try g95_missing_attr(symbol_attribute *attribute, locus *where) {      
      
  return SUCCESS;   
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
       
       
      
      
/* g95_check_pointer_assign()-- Check that a pointer assignment is OK.
 * We first check lvalue, and we only check rvalue if it's not an
 * assignment to NULL() or a NULLIFY statement. */         
         
try g95_check_pointer_assign(g95_expr *lvalue, g95_expr *rvalue) {      
symbol_attribute attribute;       
int is_pure; 
 
  if (lvalue->symbol->ts.type == BT_UNKNOWN) {    
    g95_error("Pointer assignment target is not a POINTER at %L", 
	      &lvalue->where);          
    return FAILURE;     
  }  
  
  attribute = g95_variable_attr(lvalue, NULL);   
  if (!attribute.pointer) {       
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
          
    attribute = g95_expr_attr(rvalue);  
    if (!attribute.target && !attribute.pointer) {    
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
   
   
   
   
/* g95_show_attr()-- Show symbol attributes.  The flavor and intent
 * are followed by whatever single bit attributes are present */          
          
void g95_show_attr(symbol_attribute *attr) {          
          
  g95_status("(%s %s %s %s", g95_code2string(flavors, attr->flavor),
	     g95_intent_string(attr->intent),         
	     g95_code2string(accessibility, attr->access),          
	     g95_code2string(procedures, attr->proc));        
        
  if (attr->allocatable)    g95_status(" ALLOCATABLE");   
  if (attr->dimension)      g95_status(" DIMENSION");  
  if (attr->external)       g95_status(" EXTERNAL"); 
  if (attr->intrinsic)      g95_status(" INTRINSIC");      
  if (attr->optional)       g95_status(" OPTIONAL");          
  if (attr->pointer)        g95_status(" POINTER");  
  if (attr->save)           g95_status(" SAVE");          
  if (attr->target)         g95_status(" TARGET");   
  if (attr->dummy)          g95_status(" DUMMY");
  if (attr->result_var)     g95_status(" RESULT");       
  if (attr->entry)          g95_status(" ENTRY");      
      
  if (attr->data)           g95_status(" DATA");
  if (attr->use_assoc)      g95_status(" USE-ASSOC");
  if (attr->in_namelist)    g95_status(" IN-NAMELIST");       
  if (attr->in_common)      g95_status(" IN-COMMON");     
     
  if (attr->function)       g95_status(" FUNCTION");        
  if (attr->subroutine)     g95_status(" SUBROUTINE");         
  if (attr->implicit_type)  g95_status(" IMPLICIT-TYPE");      
      
  if (attr->sequence)       g95_status(" SEQUENCE");       
  if (attr->elemental)      g95_status(" ELEMENTAL");    
  if (attr->pure)           g95_status(" PURE");      
  if (attr->recursive)      g95_status(" RECURSIVE");          
          
  g95_status(")"); 
}         
         
         
      
      
/* g95_match_implicit_none()-- Match an IMPLICIT NONE statement. */    
    
match g95_match_implicit_none(void) {        
        
  return (g95_match_eos() == MATCH_YES) ? MATCH_YES : MATCH_NO;       
}       
       
       
  
  
/* g95_module_symbol()-- Return nonzero if the symbol lives in a
 * module namespace or has been use-associated. */ 
 
int g95_module_symbol(g95_symbol *symbol) {    
    
  return symbol->ns->proc_name->attr.flavor == FL_MODULE || symbol->attr.use_assoc;     
} 
 
 
     
     
void g95_symbol_done_2(void) {          
          
  g95_free_namespace(g95_current_ns);     
  g95_current_ns = NULL;  
}       
       
       
     
     
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


         
         
/* g95_set_default_type()-- Given a pointer to a symbol, set its type
 * according to the first letter of its name.  Fails if the letter in
 * question has no default type. */          
          
try g95_set_default_type(g95_symbol *symbol, int error_flag, g95_namespace *n) {  
g95_typespec *ts;       
       
  if (symbol->ts.type != BT_UNKNOWN)   
    g95_internal_error("g95_set_default_type(): symbol already has a type");     
     
  ts = g95_get_default_type(symbol, n);      
      
  if (ts->type == BT_UNKNOWN) {         
    if (error_flag)       
      g95_error("Symbol '%s' at %L has no IMPLICIT type", symbol->name,   
		&symbol->declared_at);      
      
    return FAILURE;          
  }      
      
  symbol->ts = *ts;  
  symbol->attr.implicit_type = 1;        
        
  return SUCCESS;          
}




/* traverse_symtree()-- Recursively traverse the symtree nodes. */      
      
static void traverse_symtree(g95_symtree *st1, void (*func)(g95_symtree *)) {         
         
  if (st1 != NULL) {    
    (*func)(st1);   
   
    traverse_symtree(st1->left, func);
    traverse_symtree(st1->right, func);   
  }  
}    
    
    
     
     
void g95_traverse_symtree(g95_namespace *name, void (*func)(g95_symtree *)) {  
  
  traverse_symtree(name->sym_root, func);        
}         
         
         
      
/* No checks for use-association in public and private statements */         
         
try g95_add_access(symbol_attribute *attr, g95_access access, locus *loc) {      
      
  if (attr->access == ACCESS_UNKNOWN) { 
    attr->access = access;       
    return check_conflict(attr, loc);       
  }        
        
  if (loc == NULL) loc = g95_current_locus();   
  g95_error("ACCESS specification at %L was already specified", loc);     
     
  return FAILURE;
}        
        
        
          
          
/* g95_new_symtree()-- Allocate a new symtree node and associate it
 * with the new symbol. */     
     
g95_symtree *g95_new_symtree(g95_symtree **root, char *nm) {    
g95_symtree *st0;          
          
  st0 = g95_getmem(sizeof(g95_symtree));
  strcpy(st0->name, nm);    
    
  g95_insert_bbt(root, st0, g95_compare_symtree);          
  return st0;      
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
       
       
        
        
try g95_add_function(symbol_attribute *attribute, locus *old_loc) {        
        
  if (attribute->flavor != FL_PROCEDURE &&       
      g95_add_flavor(attribute, FL_PROCEDURE, old_loc) == FAILURE) return FAILURE;   
   
  attribute->function = 1;  
  return check_conflict(attribute, old_loc);
}   
   
   
    
    
try g95_add_procedure(symbol_attribute *attribute, procedure_type h, locus *loc) {   
   
  if (check_used(attribute, loc)) return FAILURE; 
 
  if (attribute->flavor != FL_PROCEDURE &&
      g95_add_flavor(attribute, FL_PROCEDURE, loc) == FAILURE) return FAILURE; 
 
  if (loc == NULL) loc = g95_current_locus(); 
 
  if (attribute->proc != PROC_UNKNOWN) {    
    g95_error("%s procedure at %L is already %s %s procedure",
	      g95_code2string(procedures, h), loc,      
	      g95_article(g95_code2string(procedures, attribute->proc)),    
	      g95_code2string(procedures, attribute->proc)); 
 
    return FAILURE;     
  }          
          
  attribute->proc = h;  
  
/* Statement functions are always scalar and functions */       
       
  if (h == PROC_ST_FUNCTION &&      
      ((!attribute->function && g95_add_function(attribute, loc) == FAILURE)  
       || attribute->dimension)) return FAILURE;       
       
  return check_conflict(attribute, loc);
}          
          
          
   
   
/* g95_define_st_label()-- Called when a statement with a statement
 * label is about to be accepted.  We add the label to the list of the
 * current namespace, making sure it hasn't been defined previously
 * and referenced correctly. */     
     
void g95_define_st_label(g95_st_label *lp, g95_sl_type typ,     
                         locus *label_locus) { 
int labelno; 
 
  labelno = lp->value;    
    
  if (lp->defined != ST_LABEL_UNKNOWN)
    g95_error("Duplicate statement label %d at %L and %L", labelno,    
	      &lp->where, label_locus);  
  else {  
    lp->where = *label_locus;   
   
    switch(typ) {      
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
     
     
      
      
/* match_implicit_range()-- Match the letter range(s) of an IMPLICIT
 * statement.  Sets flags in new_flag[] and copies the typespec to
 * new_ts[]. */      
      
static match match_implicit_range(g95_typespec *typ) { 
int t, q, j, w, inner;     
locus cur_loc;       
       
  cur_loc = *g95_current_locus();         
        
  g95_gobble_whitespace();    
  t = g95_next_char();     
  if (t != '(') {        
    g95_error("Missing character range in IMPLICIT at %C");  
    goto bad;         
  }         
         
  inner = 1;
  while(inner) {       
    g95_gobble_whitespace();   
    j = g95_next_char();  
    if (!isalpha(j)) goto bad;      
      
    g95_gobble_whitespace();        
    t = g95_next_char(); 
 
    switch(t) {       
    case ')':         
      inner = 0;   /* Fall through */    
    
    case ',':     
      w = j;    
      break;    
    
    case '-':    
      g95_gobble_whitespace();  
      w = g95_next_char();      
      if (!isalpha(w)) goto bad; 
 
      g95_gobble_whitespace();         
      t = g95_next_char(); 
 
      if ((t != ',') && (t != ')')) goto bad; 
      if (t == ')') inner = 0;         
         
      break;    
    
    default: goto bad;  
    }     
     
    if (j > w) {   
      g95_error("Letters must be in alphabetic order in IMPLICIT statement " 
		"at %C");   
      goto bad;         
    }      
      
    j -= 'a';   
    w -= 'a';    
    
    for(q=j; q<=w; q++) {     
      if (new_flag[q]) {      
	g95_error("Letter '%c' already set in IMPLICIT statement at %C",         
		  q+'A');
	goto bad;        
      }        
        
      new_ts[q] = *typ;     
      new_flag[q] = 1;    
    }          
  }          
          
  return MATCH_YES;  
  
 bad:     
  g95_syntax_error(ST_IMPLICIT);         
         
  g95_set_locus(&cur_loc);    
  return MATCH_ERROR;       
}       
       
       
   
   
try g95_add_pointer(symbol_attribute *attr, locus *old_loc) {   
   
  if (check_used(attr, old_loc)) return FAILURE;      
      
  attr->pointer = 1;          
  return check_conflict(attr, old_loc);     
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
int e, j; 
match g;          
          
  for(j=0; j<G95_LETTERS; j++) {     
    g95_clear_ts(&new_ts[j]);      
    if (new_flag[j]) new_flag[j] = 0;      
  }    
    
  if (g95_match_eos() == MATCH_YES) {        
    g95_error("Empty IMPLICIT statement at %C");    
    return MATCH_ERROR;          
  } 
 
  do {        
    g = g95_match_type_spec(&t, 0); /* A basic type is mandatory here */       
    if (g == MATCH_ERROR) goto error;       
    if (g == MATCH_NO) goto syntax;    
    
    cur_loc = *g95_current_locus();       
    g = match_implicit_range(&t);      
      
    if (g == MATCH_YES) {   /* Looks like we have the <TYPE> (<RANGE>) */         
      g95_gobble_whitespace();          
      e = g95_next_char();          
      if ((e == '\n') || (e == ',')) continue;        
        
      g95_set_locus(&cur_loc);       
    }     
     
    /* Last chance-- check <TYPE> (<KIND>) (<RANGE>) */      
      
    g = g95_match_kind_spec(&t);     
    if (g == MATCH_ERROR) goto error;       
    if (g == MATCH_NO) {   
      g = g95_match_old_kind_spec(&t);        
      if (g == MATCH_ERROR) goto error;   
      if (g == MATCH_NO) goto syntax;       
    }          
          
    g = match_implicit_range(&t);   
    if (g == MATCH_ERROR) goto error;  
    if (g == MATCH_NO) goto syntax;        
            
    g95_gobble_whitespace();         
    e = g95_next_char();  
    if ((e != '\n') && (e != ',')) goto syntax;       
       
  } while(e == ',');       
       
/* An implicit statement has been fully matched at this point.  Now
 * check to see if merging the new implicit types back into the
 * existing types will work. */        
        
  for(j=0; j<G95_LETTERS; j++)      
    if (new_flag[j]) {          
      if (g95_current_ns->set_flag[j]) {         
	g95_error("Letter %c already has an IMPLICIT type at %C", j+'A');        
	goto error;    
      }     
    }         
         
  return MATCH_YES;     
     
syntax:      
  g95_syntax_error(ST_IMPLICIT);  
  
error:          
  return MATCH_ERROR; 
}         
         
         
          
          
/* g95_copy_attr()-- copy an attribute to a symbol attribute, bit by
 * bit.  Some attributes have a lot of side-effects but cannot be
 * present given where we are called from, so we ignore some bits */     
     
try g95_copy_attr(symbol_attribute *des, symbol_attribute *s1,     
		  locus *loc) {     
     
  if (s1->allocatable && g95_add_allocatable(des, loc) == FAILURE)     
    goto fail; 
 
  if (s1->dimension && g95_add_dimension(des, loc) == FAILURE) goto fail;        
  if (s1->optional && g95_add_optional(des, loc) == FAILURE) goto fail;       
  if (s1->pointer && g95_add_pointer(des, loc) == FAILURE) goto fail;  
  if (s1->save && g95_add_save(des, loc) == FAILURE) goto fail;    
  if (s1->target && g95_add_target(des, loc) == FAILURE) goto fail;       
  if (s1->dummy && g95_add_dummy(des, loc) == FAILURE) goto fail;
  if (s1->result_var && g95_add_result(des, loc) == FAILURE) goto fail;
  if (s1->entry) des->entry = 1;      
      
  if (s1->in_namelist && g95_add_in_namelist(des, loc) == FAILURE)  
    goto fail;    
    
  if (s1->in_common && g95_add_in_common(des, loc) == FAILURE) goto fail; 
  if (s1->generic && g95_add_generic(des, loc) == FAILURE) goto fail;  
  if (s1->function && g95_add_function(des, loc) == FAILURE) goto fail; 
  if (s1->subroutine && g95_add_subroutine(des, loc) == FAILURE) goto fail;    
    
  if (s1->sequence && g95_add_sequence(des, loc) == FAILURE) goto fail;     
  if (s1->elemental && g95_add_elemental(des, loc) == FAILURE) goto fail;     
  if (s1->pure && g95_add_pure(des, loc) == FAILURE) goto fail;        
  if (s1->recursive && g95_add_recursive(des, loc) == FAILURE) goto fail;       
       
  if (s1->flavor != FL_UNKNOWN &&    
      g95_add_flavor(des, s1->flavor, loc) == FAILURE) goto fail;        
        
  if (s1->intent != INTENT_UNKNOWN &&
      g95_add_intent(des, s1->intent, loc) == FAILURE) goto fail;   
   
  if (s1->access != ACCESS_UNKNOWN &&      
      g95_add_access(des, s1->access, loc) == FAILURE) goto fail;       
       
  if (g95_missing_attr(des, loc) == FAILURE) goto fail;   
   
/* The subroutines that set these bits also cause flavors to be set,
 * and that has already happened in the original, so don't let to
 * happen again. */      
      
  if (s1->external) des->external = 1;       
  if (s1->intrinsic) des->intrinsic = 1;        
        
  return SUCCESS;   
   
fail:  
  return FAILURE;
}          
          
          


/* duplicate_attr()-- Generate an error because of a duplicate attribute */        
        
static void duplicate_attr(char *attribute, locus *old_loc) {

  if (old_loc == NULL) old_loc = g95_current_locus();

  g95_error("Duplicate %s attribute specified at %L", attribute, old_loc);
} 
 
 
         
         
try g95_add_external(symbol_attribute *attr, locus *w) {

  if (check_used(attr, w)) return FAILURE;   
     
  if (attr->external) {      
    duplicate_attr("EXTERNAL", w);     
    return FAILURE;   
  }  
  
  attr->external = 1;   
   
  return check_conflict(attr, w);        
} 
 
 
      
      
try g95_add_pure(symbol_attribute *attr, locus *old_loc) {

  if (check_used(attr, old_loc)) return FAILURE;         
         
  attr->pure = 1; 
  return check_conflict(attr, old_loc);      
}  
  
  
     
     
/* g95_symbol_state()-- Makes sure that no changes to symbols are pending */

void g95_symbol_state(void) {      
      
  if (changed_syms != NULL)         
    g95_internal_error("Symbol changes still pending");          
}     
          
          
/* g95_find_uop()-- Given a name find the user operator node.  Returns
 * NULL if it does not exist. */

g95_user_op *g95_find_uop(char *nam, g95_namespace *namesp) {          
g95_symtree *s;  
  
  if (namesp == NULL) namesp = g95_current_ns;   
   
  s = g95_find_symtree(namesp->uop_root, nam); 
  return (s == NULL) ? NULL : s->n.uop;        
}  
  
  
 
/******************** Statement label management ********************/ 
 
/* Free a single g95_st_label structure, making sure the list is not
 * messed up.  This function is called only when some parse error
 * occurs. */  
  
void g95_free_st_label(g95_st_label *m) {      
      
  if (m == NULL) return;        
        
  if (m->prev)    
    (m->prev->next = m->next);     
      
  if (m->next)    
    (m->next->prev = m->prev);

  if (m->format != NULL) g95_free_expr(m->format);          
  g95_free(m);    
}     
     
     
     
     
try g95_add_entry(symbol_attribute *attr, locus *loc) {      
      
  if (check_used(attr, loc)) return FAILURE;        
        
  if (attr->entry) {    
    duplicate_attr("ENTRY", loc);
    return FAILURE; 
  }  
  
  attr->entry = 1;         
  return check_conflict(attr, loc);      
}    
    
    
      
      
/* g95_find_component()-- Given a derived type node and a component
 * name, try to locate the component structure.  Returns the NULL
 * pointer if the component is not found or the components are
 * private. */         
          
g95_component *g95_find_component(g95_symbol *sy, char *n) {     
g95_component *v;        
        
  if (n == NULL) return NULL; 
 
  sy = g95_use_derived(sy);      
      
  if (sy == NULL) return NULL;     
     
  for(v=sy->components; v; v=v->next)         
    if (strcmp(v->name, n) == 0) break;      
      
  if (v == NULL) 
    g95_error("'%s' at %C is not a member of the '%s' structure",  
	      n, sy->name);    
  else {
    if (sy->attr.use_assoc && sy->component_access == ACCESS_PRIVATE) {
      g95_error("Component '%s' at %C is a PRIVATE component of '%s'",        
		n, sy->name);      
      v = NULL;     
    }          
  }          
          
  return v;         
}    
    
    
        
        
/* ambiguous_symbol()-- Generate an error if a symbol is ambiguous. */   
   
static void ambiguous_symbol(char *name0, g95_symtree *st0) {

  if (st0->n.sym->module[0])          
    g95_error("Name '%s' at %C is an ambiguous reference to '%s' "        
	      "from module '%s'", name0, st0->n.sym->name,       
	      st0->n.sym->module);  
  else         
    g95_error("Name '%s' at %C is an ambiguous reference to '%s' "         
	      "from current program unit", name0, st0->n.sym->name); 
}    
    
    


try g95_add_explicit_interface(g95_symbol *sy, ifsrc source,      
			       g95_formal_arglist *form, locus *loc) {          
          
  if (check_used(&sy->attr, loc)) return FAILURE;    
    
  if (loc == NULL) loc = g95_current_locus();         
         
  if (sy->attr.if_source != IFSRC_UNKNOWN &&    
      sy->attr.if_source != IFSRC_DECL) {    
    g95_error("Symbol '%s' at %L already has an explicit interface", 
	      sy->name, loc);        
    return FAILURE;       
  }

  sy->formal = form;    
  sy->attr.if_source = source;          
          
  return SUCCESS; 
}


   
   
/* delete_symtree()-- delete a symbol from the tree.  Does not free the
 * symbol itself! */   
   
static void delete_symtree(g95_symtree **root, char *name0) {       
g95_symtree s, *st0; 
 
  st0 = g95_find_symtree(*root, name0);       
      
  strcpy(s.name, name0);          
  g95_delete_bbt(root, &s, g95_compare_symtree);     
     
  g95_free(st0);      
}    
    
    
          
          
/* switch_types()-- Recursive function to switch derived types of all
 * symbol in a namespace. */         
         
static void switch_types(g95_symtree *st0, g95_symbol *f, g95_symbol *dest) {   
g95_symbol *symbol;         
         
  if (st0 == NULL) return;      
      
  symbol = st0->n.sym;
  if (symbol->ts.type == BT_DERIVED && symbol->ts.derived == f)        
    symbol->ts.derived = dest;          
          
  switch_types(st0->left, f, dest);      
  switch_types(st0->right, f, dest);     
}       
       
       
        
        
/* traverse_ns()-- Recursive namespace traversal function. */ 
 
static void traverse_ns(g95_symtree *st0, void (*func)(g95_symbol *)) {

  if (st0 == NULL) return;  
  
  if (st0->n.sym->mark == 0) (*func)(st0->n.sym);     
  st0->n.sym->mark = 1; 
 
  traverse_ns(st0->left, func);         
  traverse_ns(st0->right, func);   
}       
       
       
 
 
try g95_add_elemental(symbol_attribute *attr, locus *w) {  
  
  if (check_used(attr, w)) return FAILURE;

  attr->elemental = 1;      
  return check_conflict(attr, w); 
}       
       
       


/* save_symbol()-- Given a symbol, mark it as SAVEd if it is allowed */ 
 
static void save_symbol(g95_symbol *symb) {       
       
  if (symb->attr.use_assoc) return;         
         
  if (symb->attr.in_common || symb->attr.dummy ||  
      symb->attr.flavor != FL_VARIABLE) return;     
     
  g95_add_save(&symb->attr, &symb->declared_at);   
}   
   
   
      
      
/* g95_reference_st_label()-- Reference a label.  Given a label
 * and its type, see if that reference is consistent with what is
 * known about that label, updating the unknown state.  Returns
 * FAILURE if something goes wrong. */         
         
try g95_reference_st_label(g95_st_label *lp, g95_sl_type dtype) {     
g95_sl_type label_type;   
int labelno;  
try rc;   
   
  if (lp == NULL) 
    return SUCCESS;

  labelno = lp->value;       
       
  if (lp->defined != ST_LABEL_UNKNOWN)   
    label_type = lp->defined;         
  else {         
    label_type = lp->referenced;        
    lp->where = *g95_current_locus();         
  }        
        
  if (label_type == ST_LABEL_FORMAT && dtype == ST_LABEL_TARGET) {         
    g95_error("Label %d at %C previously used as a FORMAT label", labelno);        
    rc = FAILURE;          
    goto done;  
  }      
      
  if ((label_type == ST_LABEL_TARGET || label_type == ST_LABEL_BAD_TARGET)      
      && dtype == ST_LABEL_FORMAT) {
    g95_error("Label %d at %C previously used as branch target", labelno);      
    rc = FAILURE;  
    goto done;       
  }         
         
  lp->referenced = dtype;     
  rc = SUCCESS;

done:          
  return rc;
}


      
      
/* g95_compare_attr()-- Compares two attributes */   
   
int g95_compare_attr(symbol_attribute *a1, symbol_attribute *f) {         
         
  return a1->allocatable == f->allocatable &&     
    a1->dimension == f->dimension     && a1->external == f->external &&
    a1->intrinsic == f->intrinsic     && a1->optional == f->optional && 
    a1->pointer == f->pointer         && a1->save == f->save &&    
    a1->target == f->target           && a1->dummy == f->dummy &&         
    a1->result_var == f->result_var   && a1->entry == f->entry &&
    a1->data == f->data               && a1->use_assoc == f->use_assoc &&
    a1->in_namelist == f->in_namelist && a1->in_common == f->in_common &&    
    a1->function == f->function       && a1->subroutine == f->subroutine &&        
    a1->sequence == f->sequence       && a1->elemental == f->elemental &&
    a1->pure == f->pure               && a1->recursive == f->recursive &&    
    a1->access == f->access           && a1->intent == f->intent &&      
    a1->flavor == f->flavor           && a1->proc == f->proc &&         
    a1->generic == f->generic;
}         
         
         
 
 
/* g95_get_namespace()-- Allocate a new namespace structure.  */

g95_namespace *g95_get_namespace(g95_namespace *parent) { 
g95_namespace *names;  
g95_typespec *typesp;   
int d;      
       
  names = g95_getmem(sizeof(g95_namespace));   
  names->sym_root = NULL;   
  names->uop_root = NULL;     
  names->default_access = ACCESS_UNKNOWN; 
  names->parent = parent;

  for(d=0; d<G95_INTRINSIC_OPS; d++)     
    names->operator_access[d] = ACCESS_UNKNOWN;      
      
/* Initialize default types */          
          
  for(d='a'; d<='z'; d++) {      
    names->set_flag[d - 'a'] = 0;          
    typesp = &names->default_type[d - 'a'];       
       
    if (names->parent != NULL) {    /* Copy parent settings */          
      *typesp = names->parent->default_type[d - 'a'];      
      continue;   
    }         
         
    if (g95_option.implicit_none != 0) {      
      g95_clear_ts(typesp);         
      continue;          
    }          
          
    if ('i' <= d && d <= 'n') {        
      typesp->type = BT_INTEGER;        
      typesp->kind = g95_default_integer_kind();   
    } else {      
      typesp->type = BT_REAL;        
      typesp->kind = g95_default_real_kind();      
    }
  }        
        
  return names;  
}      
      
      
          
          
try g95_add_dimension(symbol_attribute *attr, locus *loc) {   
   
  if (check_used(attr, loc)) return FAILURE;

  if (attr->dimension) {     
    duplicate_attr("DIMENSION", loc);
    return FAILURE;  
  }      
      
  attr->dimension = 1;    
  return check_conflict(attr, loc);      
}


  
  
/* g95_set_component_attr()-- Set component attributes from a standard
 * symbol attribute structure. */

void g95_set_component_attr(g95_component *u, symbol_attribute *attr) {       
       
  u->dimension = attr->dimension;   
  u->pointer = attr->pointer;       
} 
 
 
    
    
/* free_sym_tree()-- Recursive function that deletes an entire
 * red-black tree and all the symbols that it contains. */          
          
static void free_sym_tree(g95_symtree *rb) {  
g95_namespace *namesp;     
g95_symbol *s;      
      
  if (rb == NULL) return; 
 
  free_sym_tree(rb->left);      
  free_sym_tree(rb->right);         
         
  s = rb->n.sym;        
        
  s->refs--;         
  if (s->refs < 0) g95_internal_error("free_sym_tree(): Negative refs");        
        
  if (s->formal_ns != NULL && s->refs == 1) {        
    /* as formal_ns contains a reference to sym, delete formal_ns just
     * before the deletion of sym. */   
    namesp = s->formal_ns;         
    s->formal_ns = NULL; 
    g95_free_namespace(namesp);         
  } else if (s->refs == 0) {   /* Go ahead and delete the symbol */      
    g95_free_symbol(s);        
  }   
   
  g95_free(rb);         
}  
  
  
         
         
/* free_components()-- Given a symbol, free all of the component
 * structures and everything they point to. */      
      
static void free_components(g95_component *x) {      
g95_component *y;   
   
  for(; x; x=y) {  
    y = x->next;      
      
    g95_free_array_spec(x->as);          
    g95_free_expr(x->initializer);         
         
    g95_free(x);        
  }        
}      
      
      
   
   
try g95_add_in_namelist(symbol_attribute *attr, locus *loc) {

  attr->in_namelist = 1;         
  return check_conflict(attr, loc);
}


         
         
try g95_add_in_common(symbol_attribute *atr, locus *old_loc) {          
          
  if (check_used(atr, old_loc)) return FAILURE;     
     
  /* Duplicate attribute already checked for */        
        
  atr->in_common = 1;      
  if (check_conflict(atr, old_loc) == FAILURE) return FAILURE;          
          
  if (atr->flavor == FL_VARIABLE) return SUCCESS;         
         
  return g95_add_flavor(atr, FL_VARIABLE, old_loc);    
}          
          
          
      
      
try g95_add_allocatable(symbol_attribute *attr, locus *where) {   
   
  if (check_used(attr, where)) return FAILURE; 
 
  if (attr->allocatable) {        
    duplicate_attr("ALLOCATABLE", where); 
    return FAILURE;    
  }  
  
  attr->allocatable = 1;         
  return check_conflict(attr, where);          
}    
    
    
      
      
try g95_add_target(symbol_attribute *a, locus *pos) {    
    
  if (check_used(a, pos)) return FAILURE;   
   
  if (a->target) {    
    duplicate_attr("TARGET", pos);         
    return FAILURE;        
  }    
    
  a->target = 1;       
  return check_conflict(a, pos);     
}        
        
        
     
     
/* g95_get_uop()-- Given a name find a user operator node, creating it
 * if it doesn't exist.  These are much simpler than symbols because
 * they can't be ambiguous with one another */         
         
g95_user_op *g95_get_uop(char *name0) { 
g95_user_op *u;       
g95_symtree *s;         
         
  s = g95_find_symtree(g95_current_ns->uop_root, name0);
  if (s != NULL) return s->n.uop;      
      
  s = g95_new_symtree(&g95_current_ns->uop_root, name0);    
    
  u = s->n.uop = g95_getmem(sizeof(g95_user_op));       
  strcpy(u->name, name0);   
  u->access = ACCESS_UNKNOWN;          
  u->ns = g95_current_ns; 
 
  return u;      
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
         
         


/* g95_undo_symbols()-- Undoes all the changes made to symbols in the
 * current statement.  This subroutine is made simpler due to the fact
 * that attributes are never removed once added. */ 
 
void g95_undo_symbols(void) {  
g95_symbol *p, *s, *old;        
g95_symtree *i, *d;   
   
/* if (changed_syms != NULL) g95_status("Undoing symbols\n"); */

  for(p=changed_syms; p; p=s) {         
    s = p->tlink;      
    /* g95_status("Undoing %s\n", p->name); */   
   
    if (p->new) {  /* Symbol was new */    
      delete_symtree(&p->ns->sym_root, p->name);         
         
      p->refs--;          
      if (p->refs < 0) g95_internal_error("g95_undo_symbols(): Negative refs");   
      if (p->refs == 0) g95_free_symbol(p);        
      continue;  
    } 
 
/* Restore previous state of symbol.  Just copy simple stuff */      
      
    p->mark = 0;          
    old = p->old_symbol;    
    
    p->ts.type = old->ts.type;       
    p->ts.kind = old->ts.kind;       
       
    p->attr = old->attr;

    if (p->value != old->value) {      
      g95_free_expr(old->value);          
      p->value = NULL;         
    }   
   
    if (p->as != old->as) {
      if (p->as) g95_free_array_spec(p->as);        
      p->as = old->as;
    }          
          
    p->generic = old->generic; 
    p->component_access = old->component_access;

    if (p->namelist != NULL && old->namelist == NULL) {         
      g95_free_namelist(p->namelist);   
      p->namelist = NULL;
    } else {         
         
      if (p->namelist_tail != old->namelist_tail) {      
	g95_free_namelist(old->namelist_tail);  
	old->namelist_tail->next = NULL;   
      }          
    } 
 
    p->namelist_tail = old->namelist_tail;        
        
    if (p->formal != old->formal) { 
      g95_free_formal_arglist(p->formal);  
      p->formal = old->formal;     
    }      
      
    g95_free(p->old_symbol);    
    p->old_symbol = NULL;    
    p->tlink = NULL; 
  }        
        
  changed_syms = NULL;   
   
  /* Unlink host associated symtrees */      
      
  for(i=changed_st; i; i=d) { 
    d = i->link;       
       
    g95_delete_bbt(&g95_current_ns->sym_root, i, g95_compare_symtree);      
    g95_free(i);        
  }     
     
  changed_st = NULL;          
}


     
     
/* g95_get_componentr_attr()-- Get a standard symbol attribute
 * structure given the component structure. */

void g95_get_component_attr(symbol_attribute *a, g95_component *q) {

  g95_clear_attr(a);      
  a->dimension = q->dimension;      
  a->pointer = q->pointer;    
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
    
    
     
     
/* g95_find_symtree()-- Given a root symtree node and a name, try to
 * find the symbol within the namespace.  Returns NULL if the symbol
 * is not found. */   
   
g95_symtree *g95_find_symtree(g95_symtree *s, char *nam) {
int j;       
       
  while(s != NULL) {       
    j = strcmp(nam, s->name);  
    if (j == 0) return s; 
 
    s = (j < 0) ? s->left : s->right;        
  }

  return NULL;          
}


      
      
/* save_commons()-- Mark common blocks as saved */        
        
static void save_commons(g95_symtree *s) {  
  
  if (s == NULL) return;         
         
  save_commons(s->left);    
  save_commons(s->right);          
          
  s->n.common->saved = 1; 
}         
         
         
    
/************** Component name management ************/

/* Component names of a derived type form their own little namespaces
 * that are separate from all other spaces.  The space is composed of
 * a singly linked list of g95_component structures whose head is
 * located in the parent symbol. */   
   
/* g95_add_component()-- Add a component name to a symbol.  The call
 * fails if the name is already present.  On success, the component
 * pointer is modified to point to the additional component structure. */  
  
try g95_add_component(g95_symbol *symb, char *name,  
		      g95_component **component) {  
g95_component *m, *end; 
 
  end = NULL; 
 
  for(m=symb->components; m; m=m->next) {          
    if (strcmp(m->name, name) == 0) { 
      g95_error("Component '%s' at %C already declared at %L",    
		name, &m->loc);       
      return FAILURE;     
    }  
  
    end = m; 
  }   
   
/* Allocate new component */   
   
  m = g95_get_component();  
  
  if (end == NULL) symb->components = m;          
  else end->next = m;    
    
  strcpy(m->name, name);       
  m->loc = *g95_current_locus();

  *component = m;    
  return SUCCESS;       
}         
         
         


try g95_add_save(symbol_attribute *attribute, locus *w) {  
  
  if (check_used(attribute, w)) return FAILURE;          
          
  if (g95_pure(NULL)) {
    g95_error("SAVE attribute at %L cannot be specified in a PURE procedure",    
	      w); 
    return FAILURE;   
  }  
  
  if (attribute->save) { 
    duplicate_attr("SAVE", w);     
    return FAILURE;  
  }        
        
  attribute->save = 1;        
  return check_conflict(attribute, w);  
} 
 
 
   
   
try g95_add_sequence(symbol_attribute *attr, locus *where) {      
      
  if (check_used(attr, where)) return FAILURE;       
       
  attr->sequence = 1;
  return check_conflict(attr, where);          
} 
 
 
     
     
try g95_add_optional(symbol_attribute *atr, locus *old_loc) {         
         
  if (check_used(atr, old_loc)) return FAILURE;        
        
  if (atr->optional) { 
    duplicate_attr("OPTIONAL", old_loc);       
    return FAILURE;   
  }       
       
  atr->optional = 1;    
  return check_conflict(atr, old_loc);         
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
       
g95_symbol *g95_use_derived(g95_symbol *sym) {  
g95_symbol *g, *o;   
g95_typespec *d;    
g95_symtree *st1;  
int m;      
      
  if (sym->components != NULL) return sym;   /* Already defined */    
    
  if (sym->ns->parent == NULL) goto bad;   
   
  if (g95_find_symbol(sym->name, sym->ns->parent, 1, &g)) {  
    g95_error("Symbol '%s' at %C is ambiguous", sym->name);    
    return NULL;
  }       
       
  if (g == NULL || g->attr.flavor != FL_DERIVED) goto bad;      
      
  /* Get rid of symbol sym, translating all references to s */        
        
  for(m=0; m<G95_LETTERS; m++) {
    d = &sym->ns->default_type[m];
    if (d->derived == sym) d->derived = g;    
  }      
      
  st1 = g95_find_symtree(sym->ns->sym_root, sym->name);          
  st1->n.sym = g;  
  
  g->refs++;        
        
  /* Unlink from list of modified symbols */      
      
  if (changed_syms == sym)   
    changed_syms = sym->tlink;    
  else  
    for(o=changed_syms; o; o=o->tlink)      
      if (o->tlink == sym) {    
	o->tlink = sym->tlink;    
	break;  
      }

  switch_types(sym->ns->sym_root, sym, g);   
   
  /* TODO: Also have to replace sym -> s in other lists like
   * namelists, common lists and interface lists.  */     
     
  g95_free_symbol(sym);

  return g;     
     
 bad:      
  g95_error("Derived type '%s' at %C is being used before it is defined",   
	    sym->name);         
  return NULL;          
}      
      
      
      
      
/* g95_find_symbol()-- search for a symbol starting in the current
 * namespace, resorting to any parent namespaces if requested by a
 * nonzero parent_flag.  Returns nonzero if the symbol is
 * ambiguous. */         
         
int g95_find_symbol(char *nm, g95_namespace *ns, int parent_flag,      
		    g95_symbol **rslt) {
g95_symtree *st1;        
        
  if (ns == NULL) ns = g95_current_ns;       
       
  do {        
    st1 = g95_find_symtree(ns->sym_root, nm);         
    if (st1 != NULL) {        
      *rslt = st1->n.sym;    
      if (st1->ambiguous) {   
	ambiguous_symbol(nm, st1);
	return 1;    
      }   
   
      return 0;    
    }      
      
    if (!parent_flag) break;      
      
    ns = ns->parent; 
  } while (ns != NULL);      
      
  *rslt = NULL;       
  return 0;      
}   
   
   
       
       
/* g95_free_namespace()-- Free a namespace structure and everything
 * below it.  Interface lists associated with intrinsic operators are
 * not freed.  These are taken care of when a specific name is freed. */    
    
void g95_free_namespace(g95_namespace *n) {     
g95_charlen *clen, *cl2;  
g95_namespace *t, *v; 
int z;     
     
  if (n == NULL) return;        
        
  g95_free_statements(n->code);     
     
  free_sym_tree(n->sym_root);      
  free_uop_tree(n->uop_root);    
  free_common_tree(n->common_root);      
      
  for(clen=n->cl_list; clen; clen=cl2) {          
    cl2 = clen->next;  
    g95_free_expr(clen->length);   
    g95_free(clen);      
  }         
         
  free_st_labels(n->st_labels);  
  
  g95_free_equiv(n->equiv);          
          
  for(z=0; z<G95_INTRINSIC_OPS; z++)  
    g95_free_interface(n->operator[z]);  
  
  g95_free_data(n->data);       
  t = n->contained; 
  g95_free(n); 
 
  /* Recursively free any contained namespaces */  
  
  while(t != NULL) {    
    v = t;
    t = t->sibling;         
         
    g95_free_namespace(v);  
  }     
} 
 
 
      
      
/* save_symbol_data()-- Save symbol with the information necessary to
 * back it out. */      
      
static void save_symbol_data(g95_symbol *symbol) {         
         
  if (symbol->new || symbol->old_symbol != NULL) return;

  symbol->old_symbol = g95_getmem(sizeof(g95_symbol));        
  *(symbol->old_symbol) = *symbol; 
 
  symbol->tlink = changed_syms;         
  changed_syms = symbol;        
}          
          
          
   
   
/* clear_sym_mark()-- Clear mark bits from symbol nodes associated
 * with a symtree node */         
         
static void clear_sym_mark(g95_symtree *s) {  
  
  s->n.sym->mark = 0;          
}    
    
    
 
 
/* g95_add_flavor()-- Flavors are special because some flavors are not
 * what fortran considers attributes and can be reaffirmed multiple
 * times. */

try g95_add_flavor(symbol_attribute *attr, sym_flavor t, locus *old_loc) {    
    
  if ((t == FL_PROGRAM || t == FL_BLOCK_DATA || t == FL_MODULE ||          
       t == FL_PARAMETER || t == FL_LABEL || t == FL_DERIVED ||        
       t == FL_NAMELIST) && check_used(attr, old_loc)) return FAILURE;         
         
  if (attr->flavor == t && t == FL_VARIABLE) return SUCCESS;        
        
  if (attr->flavor != FL_UNKNOWN) {         
    if (old_loc == NULL) old_loc = g95_current_locus();    
    
    g95_error("%s attribute conflicts with %s attribute at %L",  
	      g95_code2string(flavors, attr->flavor),      
	      g95_code2string(flavors, t), old_loc);          
          
    return FAILURE;
  }      
      
  attr->flavor = t;  
  
  return check_conflict(attr, old_loc);        
}      
      
      


/* g95_get_ha_symbol()-- Subroutine that searches for a symbol,
 * creating it if it doesn't exist, but tries to host-associate the
 * symbol if possible. */     
     
int g95_get_ha_symbol(char *name, g95_symbol **res) { 
g95_symbol *symb;          
int y;   
   
  y = g95_find_symbol(name, g95_current_ns, 0, &symb);
  if (symb != NULL) {       
    save_symbol_data(symb);         
         
    *res = symb;      
    return y;
  }          
          
  if (g95_current_ns->parent != NULL) { 
    y = g95_find_symbol(name, g95_current_ns->parent, 1, &symb);      
    if (y) return y;    
    
    if (symb != NULL) {     
      *res = symb;    
      return 0;         
    }          
  }   
   
  return g95_get_symbol(name, g95_current_ns, res);
}    
    
    


/* g95_traverse_ns()-- Call a given function for all symbols in the
 * namespace.  We take care that each g95_symbol node is called
 * exactly once. */ 
 
void g95_traverse_ns(g95_namespace *n, void (*func)(g95_symbol *)) {      
      
  g95_traverse_symtree(n, clear_sym_mark);       
       
  traverse_ns(n->sym_root, func);     
}       
       
       
        
        
try g95_add_subroutine(symbol_attribute *atr, locus *where) { 
 
  if (atr->flavor != FL_PROCEDURE &&          
      g95_add_flavor(atr, FL_PROCEDURE, where) == FAILURE) return FAILURE;    
    
  atr->subroutine = 1;     
  return check_conflict(atr, where);     
}


   
   
static void show_indent(void) {      
int p;    
    
  g95_status_char('\n');
  for(p=0; p<2*show_level; p++)       
    g95_status_char(' ');          
}    
    
    
          
          
static void show_uop(g95_user_op *uop) {          
g95_interface *intr; 
 
  show_indent();         
  g95_status("%s:", uop->name);       
         
  for(intr=uop->operator; intr; intr=intr->next)   
    g95_status(" %s", intr->sym->name);   
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
 * So if nonzero, then an error was issued.  */  
  
int g95_get_symbol(char *nm, g95_namespace *namesp, g95_symbol **result) {      
g95_symtree *st0;       
g95_symbol *s;

  /* This doesn't usually happen during resolution.  */
  if (namesp == NULL) namesp = g95_current_ns; 
 
  /* Try to find the symbol. */     
  st0 = g95_find_symtree(namesp->sym_root, nm);

  if (st0 == NULL) {         /* If not there, create a new symbol */     
    s = g95_new_symbol(nm, namesp);          
         
    s->old_symbol = NULL;   /* Add to the list of tentative symbols. */      
    s->tlink = changed_syms; 
    s->mark = 1;       
    s->new = 1;  
    changed_syms = s;     
     
    st0 = g95_new_symtree(&namesp->sym_root, nm);          
    st0->n.sym = s; 
    s->refs++; 
 
  } else {    /* Make sure the existing symbol is OK */      
    if (st0->ambiguous) {          
      ambiguous_symbol(nm, st0);     
      return 1;  
    }      
      
    s = st0->n.sym;     
     
    if (s->ns != namesp && (!s->attr.function || namesp->proc_name != s) &&        
	!s->attr.entry) {    
      /* Symbol is from another namespace */ 
      g95_error("Symbol '%s' at %C has already been host associated", nm);
      return 2;  
    }         
         
    s->mark = 1;  
  
    save_symbol_data(s);      /* Copy in case this symbol is changed */ 
  } 
 
  *result = s;          
  return 0;  
}   
   
   


/* g95_save_all()-- Mark those symbols which can be SAVEd as such. */        
        
void g95_save_all(g95_namespace *names) {      
      
  g95_traverse_ns(names, save_symbol);        
        
  save_commons(names->common_root);   
} 
 
 
          
          
/* g95_show_symbol()-- Show a symbol.  If a symbol is an ENTRY,
 * SUBROUTINE or FUNCTION, we show the interface.  Information needed
 * to reconstruct the list of specific interfaces associated with a
 * generic symbol is done within that symbol. */      
      
void g95_show_symbol(g95_symbol *sy) {     
g95_formal_arglist *formal;     
g95_interface *inter;      
      
  if (sy == NULL) return;     
     
  show_indent();       
       
  g95_status("symbol %s ", sy->name);     
  g95_show_typespec(&sy->ts);   
  g95_show_attr(&sy->attr);          
          
  if (sy->value) {        
    show_indent();          
    g95_status("value: ");          
    g95_show_expr(sy->value);        
  }    
    
  if (sy->as) {         
    show_indent();      
    g95_status("Array spec:");  
    g95_show_array_spec(sy->as);      
  }       
       
  if (sy->generic) {        
    show_indent();       
    g95_status("Generic interfaces:");    
    for(inter=sy->generic; inter; inter=inter->next)
      g95_status(" %s", inter->sym->name);   
  }  
  
  if (sy->result) {     
    show_indent();     
    g95_status("result: %s", sy->result->name);     
  }  
  
  if (sy->components) {          
    show_indent();
    g95_status("components: ");   
    g95_show_components(sy);   
  }         
         
  if (sy->formal) {   
    show_indent();          
    g95_status("Formal arglist:");

    for(formal=sy->formal; formal; formal=formal->next)        
      g95_status(" %s", formal->sym->name);        
  }      
      
  if (sy->formal_ns) {     
    show_indent();          
    g95_status("Formal namespace");        
    g95_show_namespace(sy->formal_ns);     
  }   
   
  g95_status_char('\n');         
}     
     
     
        
        
/* show_symtree()-- Worker function to display the symbol tree */      
      
static void show_symtree(g95_symtree *st) {         
         
  show_indent();
  g95_status("symtree: %s  Ambig %d", st->name, st->ambiguous);       
       
  if (st->n.sym->ns != g95_current_ns) 
    g95_status(" from namespace %s", st->n.sym->ns->proc_name->name);         
  else 
    g95_show_symbol(st->n.sym);        
}        
        
        
        
        
try g95_add_intrinsic(symbol_attribute *a, locus *old_loc) {    
    
  if (check_used(a, old_loc)) return FAILURE;        
        
  if (a->intrinsic) {     
    duplicate_attr("INTRINSIC", old_loc);
    return FAILURE;  
  }        
        
  a->intrinsic = 1;   
   
  return check_conflict(a, old_loc);
}


       
       
/* g95_show_namespace()-- Show a namespace */    
    
void g95_show_namespace(g95_namespace *n) {
g95_interface *intr;   
g95_namespace *save;       
int w;          
          
  save = g95_current_ns;   
  show_level++;  
 
  show_indent();     
  g95_status("Namespace:");       
       
  if (n != NULL) {        
    for(w=0; w<G95_LETTERS; w++) {   
      g95_status(" %c: ", w+'A');     
      g95_show_typespec(&n->default_type[w]);    
    }  
  
    if (n->proc_name != NULL) { 
      show_indent();         
      g95_status("procedure name = %s", n->proc_name->name);          
    }  
  
    g95_traverse_symtree(n, clear_sym_mark);   
   
    g95_current_ns = n;          
    g95_traverse_symtree(n, show_symtree);          
          
    for(w=0; w<G95_INTRINSIC_OPS; w++) {    /* User operator interfaces */         
      intr = n->operator[w];       
      if (intr == NULL) continue;         
         
      show_indent(); 
      g95_status("Operator interfaces for %s:", g95_op2string(w));  
  
      for(; intr; intr=intr->next)         
	g95_status(" %s", intr->sym->name);         
    }          
          
    if (n->uop_root != NULL) {
      show_indent();  
      g95_status("User operators:\n");         
      g95_traverse_user_op(n, show_uop);   
    }      
  }      
      
  g95_status_char('\n');          
  g95_status_char('\n');      
      
#ifdef G95_DEBUG
  g95_show_code(0, n->code);    
#endif
     
  for(n=n->contained; n; n=n->sibling) {  
    g95_status("CONTAINS\n");
    g95_show_namespace(n);         
  }

  show_level--;      
  g95_status_char('\n');    
  g95_current_ns = save;        
}   
   
   
