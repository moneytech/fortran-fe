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
     
     
 
 
/* g95_copy_attr()-- copy an attribute to a symbol attribute, bit by
 * bit.  Some attributes have a lot of side-effects but cannot be
 * present given where we are called from, so we ignore some bits */         
         
try g95_copy_attr(symbol_attribute *dest, symbol_attribute *src,     
		  locus *where) {  
  
  if (src->allocatable && g95_add_allocatable(dest, where) == FAILURE)     
    goto fail;    
    
  if (src->dimension && g95_add_dimension(dest, where) == FAILURE) goto fail;     
  if (src->optional && g95_add_optional(dest, where) == FAILURE) goto fail;  
  if (src->pointer && g95_add_pointer(dest, where) == FAILURE) goto fail;  
  if (src->save && g95_add_save(dest, where) == FAILURE) goto fail;      
  if (src->target && g95_add_target(dest, where) == FAILURE) goto fail;        
  if (src->dummy && g95_add_dummy(dest, where) == FAILURE) goto fail;  
  if (src->common && g95_add_common(dest, where) == FAILURE) goto fail;    
  if (src->result_var && g95_add_result(dest, where) == FAILURE) goto fail;
  if (src->entry) dest->entry = 1;

  if (src->in_namelist && g95_add_in_namelist(dest, where) == FAILURE)   
    goto fail;     
     
  if (src->in_common && g95_add_in_common(dest, where) == FAILURE) goto fail;  
  if (src->saved_common && g95_add_saved_common(dest, where)==FAILURE)    
    goto fail; 
 
  if (src->generic && g95_add_generic(dest, where) == FAILURE) goto fail;   
  if (src->function && g95_add_function(dest, where) == FAILURE) goto fail;        
  if (src->subroutine && g95_add_subroutine(dest, where) == FAILURE) goto fail;          
          
  if (src->sequence && g95_add_sequence(dest, where) == FAILURE) goto fail; 
  if (src->elemental && g95_add_elemental(dest, where) == FAILURE) goto fail; 
  if (src->pure && g95_add_pure(dest, where) == FAILURE) goto fail;
  if (src->recursive && g95_add_recursive(dest, where) == FAILURE) goto fail;     
     
  if (src->flavor != FL_UNKNOWN &&         
      g95_add_flavor(dest, src->flavor, where) == FAILURE) goto fail;       
       
  if (src->intent != INTENT_UNKNOWN &&          
      g95_add_intent(dest, src->intent, where) == FAILURE) goto fail;     
     
  if (src->access != ACCESS_UNKNOWN &&        
      g95_add_access(dest, src->access, where) == FAILURE) goto fail;      
      
  if (g95_missing_attr(dest, where) == FAILURE) goto fail; 
 
/* The subroutines that set these bits also cause flavors to be set,
 * and that has already happened in the original, so don't let to
 * happen again. */         
         
  if (src->external) dest->external = 1;        
  if (src->intrinsic) dest->intrinsic = 1;       
       
  return SUCCESS;

fail:          
  return FAILURE;   
}   
   
   
        
        
/* g95_define_st_label()-- Called when a statement with a statement
 * label is about to be accepted.  We add the label to the list of the
 * current namespace, making sure it hasn't been defined previously
 * and referenced correctly. */      
      
void g95_define_st_label(g95_st_label *lp, g95_sl_type type,    
                         locus *label_locus) {        
int labelno;       
       
  labelno = lp->value;         
         
  if (lp->defined != ST_LABEL_UNKNOWN)        
    g95_error("Duplicate statement label %d at %L and %L", labelno, 
	      &lp->where, label_locus);     
  else {   
    lp->where = *label_locus;      
      
    switch(type) {        
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
       
       
 
 
/* g95_clear_attr()-- Clears all attributes */  
  
void g95_clear_attr(symbol_attribute *attr) {        
        
  attr->allocatable = 0;    
  attr->dimension = 0;        
  attr->external = 0; 
  attr->intrinsic = 0;        
  attr->optional = 0;
  attr->pointer = 0;          
  attr->save = 0; 
  attr->target = 0;      
  attr->dummy = 0; 
  attr->common = 0;          
  attr->result_var = 0; 
  attr->entry = 0;       
  attr->data = 0;          
  attr->use_assoc = 0;     
  attr->in_namelist = 0;
  
  attr->in_common = 0;        
  attr->saved_common = 0;        
  attr->function = 0;  
  attr->subroutine = 0;    
  attr->generic = 0; 
  attr->implicit_type = 0; 
  attr->sequence = 0;      
  attr->elemental = 0;     
  attr->pure = 0;          
  attr->recursive = 0;         
         
  attr->access = ACCESS_UNKNOWN;
  attr->intent = INTENT_UNKNOWN;    
  attr->flavor = FL_UNKNOWN;          
  attr->proc = PROC_UNKNOWN;   
  attr->if_source = IFSRC_UNKNOWN;   
}          
          
          
     
     
/* free_components()-- Given a symbol, free all of the component
 * structures and everything they point to. */         
         
static void free_components(g95_component *i) {     
g95_component *r;     
     
  for(; i; i=r) { 
    r = i->next;          
          
    g95_free_array_spec(i->as);    
    g95_free_expr(i->initializer);         
         
    g95_free(i);        
  }    
}    
    
    
         
         
/* g95_set_implicit_none()-- Handle a correctly parsed IMPLICIT NONE */          
          
void g95_set_implicit_none(void) {       
int n;     
     
  for(n='a'; n<='z'; n++) {       
    g95_clear_ts(&g95_current_ns->default_type[n - 'a']);      
    g95_current_ns->set_flag[n - 'a'] = 1;         
  }          
} 
 
 
     
     
/* delete_symtree()-- delete a symbol from the tree.  Does not free the
 * symbol itself! */  
  
static void delete_symtree(g95_symtree **root, char *name) {        
g95_symtree st, *st0; 
 
  st0 = g95_find_symtree(*root, name);         
        
  strcpy(st.name, name);     
  g95_delete_bbt(root, &st, g95_compare_symtree); 
 
  g95_free(st0);   
}




/* save_symbol_data()-- Save symbol with the information necessary to
 * back it out. */

static void save_symbol_data(g95_symbol *symb) {

  if (symb->new || symb->old_symbol != NULL) return;        
        
  symb->old_symbol = g95_getmem(sizeof(g95_symbol));   
  *(symb->old_symbol) = *symb;

  symb->tlink = changed_syms;     
  changed_syms = symb;          
}      
      
      
         
         
/* g95_match_implicit_none()-- Match an IMPLICIT NONE statement. */        
        
match g95_match_implicit_none(void) {         
         
  return (g95_match_eos() == MATCH_YES) ? MATCH_YES : MATCH_NO; 
}      
      
      
      
      
/* duplicate_attr()-- Generate an error because of a duplicate attribute */         
         
static void duplicate_attr(char *attr, locus *where) {     
     
  if (where == NULL) where = g95_current_locus();      
      
  g95_error("Duplicate %s attribute specified at %L", attr, where);    
}  
  
  
          
          
try g95_add_in_namelist(symbol_attribute *attr, locus *where) { 
 
  attr->in_namelist = 1;    
  return check_conflict(attr, where);      
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
    
    
       
       
/* g95_add_type()-- Add a type to a symbol. */     
     
try g95_add_type(g95_symbol *symbol, g95_typespec *ts, locus *where) {          
sym_flavor flavor;   
   
  if (where == NULL) where = g95_current_locus();  
  
  if (symbol->ts.type != BT_UNKNOWN) {          
    g95_error("Symbol '%s' at %L already has basic type of %s", symbol->name,          
	      where, g95_basic_typename(symbol->ts.type));        
    return FAILURE;          
  }          
          
  flavor = symbol->attr.flavor;     
     
  if (flavor == FL_PROGRAM || flavor == FL_BLOCK_DATA || flavor == FL_MODULE || 
      flavor == FL_LABEL || (flavor == FL_PROCEDURE && symbol->attr.subroutine) ||          
      flavor == FL_DERIVED || flavor == FL_NAMELIST) {   
    g95_error("Symbol '%s' at %L cannot have a type", symbol->name, where);    
    return FAILURE;      
  } 
 
  symbol->ts = *ts;   
  return SUCCESS;       
}


  
  
/* g95_check_conformance()-- Given two expressions, make sure that
 * the arrays are conformable. */          
          
try g95_check_conformance(const char *optype, g95_expr *op1, g95_expr *op2) { 
int op1_flag, op2_flag, a;         
mpz_t op1_size, op2_size; 
try i;         
         
  if (op1->rank == 0 || op2->rank == 0) return SUCCESS;          
          
  if (op1->rank != op2->rank) {    
    g95_error("Incompatible ranks in %s at %L", optype, &op1->where);    
    return FAILURE; 
  }

  i = SUCCESS;     
     
  for(a=0; a<op1->rank; a++) {       
    op1_flag = g95_array_dimen_size(op1, a, &op1_size) == SUCCESS;          
    op2_flag = g95_array_dimen_size(op2, a, &op2_size) == SUCCESS;      
          
    if (op1_flag && op2_flag && mpz_cmp(op1_size, op2_size) != 0) {
      g95_error("%s at %L has different shape on dimension %d (%d/%d)",         
		optype, &op1->where, a+1, (int) mpz_get_si(op1_size),
		(int) mpz_get_si(op2_size));      
      
      i = FAILURE;    
    }     
     
    if (op1_flag) mpz_clear(op1_size);         
    if (op2_flag) mpz_clear(op2_size);   
   
    if (i == FAILURE) return FAILURE;    
  }          
          
  return SUCCESS;       
}         
         
         
       
       
/* g95_missing_attr()-- Check for missing attributes in the new
 * symbol.  Currently does nothing, but it's not clear that it is
 * unnecessary yet.  AEV 7/4/01 */         
         
try g95_missing_attr(symbol_attribute *attr, locus *where) {      
      
  return SUCCESS;          
}         
         
         
    
    
/* save_symbol()-- Given a symbol, mark it as SAVEd if it is allowed */       
       
static void save_symbol(g95_symbol *symbol) {

  if (symbol->attr.use_assoc) return;          
          
  if (symbol->attr.common) {   
    g95_add_saved_common(&symbol->attr, &symbol->declared_at);  
    return;          
  } 
 
  if (symbol->attr.in_common || symbol->attr.dummy ||     
      symbol->attr.flavor != FL_VARIABLE) return;          
          
  g95_add_save(&symbol->attr, &symbol->declared_at);        
} 
 
 
  
  
/* g95_check_assign_symbol()-- Relative of g95_check_assign() except
 * that the lvalue is a single symbol. */          
          
try g95_check_assign_symbol(g95_symbol *symb, g95_expr *rvalue) {    
g95_expr lvalue;  
  
  memset(&lvalue, '\0', sizeof(g95_expr));    
    
  lvalue.type = EXPR_VARIABLE;   
  lvalue.ts = symb->ts;        
  if (symb->as) lvalue.rank = symb->as->rank;         
  lvalue.symbol = symb;         
  lvalue.where = symb->declared_at;          
          
  return g95_check_assign(&lvalue, rvalue, 1);    
}        
        
        
          
          
/* g95_compare_symtree()-- Comparison function for symtree nodes. */    
    
int g95_compare_symtree(g95_symtree *st1, g95_symtree *st2) {   
   
  return strcmp(st1->name, st2->name);       
}    
    
    
      
/* No checks for use-association in public and private statements */    
    
try g95_add_access(symbol_attribute *attr, g95_access access, locus *where) {      
      
  if (attr->access == ACCESS_UNKNOWN) {        
    attr->access = access;      
    return check_conflict(attr, where);     
  }   
   
  if (where == NULL) where = g95_current_locus(); 
  g95_error("ACCESS specification at %L was already specified", where);      
      
  return FAILURE;    
}    
    
    
        
        
/* match_implicit_range()-- Match the letter range(s) of an IMPLICIT
 * statement.  Sets flags in new_flag[] and copies the typespec to
 * new_ts[]. */    
    
static match match_implicit_range(g95_typespec *ts) { 
int c, m, j, p, inner;      
locus cur_loc;   
   
  cur_loc = *g95_current_locus();   
  
  g95_gobble_whitespace();      
  c = g95_next_char();
  if (c != '(') { 
    g95_error("Missing character range in IMPLICIT at %C");      
    goto bad;       
  }  
  
  inner = 1;
  while(inner) {       
    g95_gobble_whitespace(); 
    j = g95_next_char();          
    if (!isalpha(j)) goto bad;

    g95_gobble_whitespace();    
    c = g95_next_char();      
      
    switch(c) { 
    case ')':
      inner = 0;   /* Fall through */    
    
    case ',':
      p = j;
      break;          
          
    case '-':          
      g95_gobble_whitespace();    
      p = g95_next_char();     
      if (!isalpha(p)) goto bad; 
 
      g95_gobble_whitespace();         
      c = g95_next_char();          
          
      if ((c != ',') && (c != ')')) goto bad;  
      if (c == ')') inner = 0;         
         
      break;       
       
    default: goto bad;       
    }         
         
    if (j > p) {        
      g95_error("Letters must be in alphabetic order in IMPLICIT statement "         
		"at %C");      
      goto bad;          
    }     
     
    j -= 'a';  
    p -= 'a';  
  
    for(m=j; m<=p; m++) { 
      if (new_flag[m]) {          
	g95_error("Letter '%c' already set in IMPLICIT statement at %C", 
		  m+'A');       
	goto bad;    
      }    
    
      new_ts[m] = *ts;        
      new_flag[m] = 1;          
    }        
  }

  return MATCH_YES;     
     
 bad: 
  g95_syntax_error(ST_IMPLICIT);     
     
  g95_set_locus(&cur_loc);       
  return MATCH_ERROR;
}  
  
  
        
        
/* g95_find_symtree()-- Given a root symtree node and a name, try to
 * find the symbol within the namespace.  Returns NULL if the symbol
 * is not found. */          
          
g95_symtree *g95_find_symtree(g95_symtree *st, char *name) {
int i;          
          
  while(st != NULL) {          
    i = strcmp(name, st->name);      
    if (i == 0) return st;       
       
    st = (i < 0) ? st->left : st->right;          
  }     
     
  return NULL;     
}


 
 
/* g95_check_assign()-- Given an assignable expression and an
 * arbitrary expression, make sure that the assignment can take
 * place. */      
      
try g95_check_assign(g95_expr *lvalue, g95_expr *rvalue, int conform) {
g95_symbol *sym;    
    
  sym = lvalue->symbol;         
         
  if (sym->attr.intent == INTENT_IN) {          
    g95_error("Can't assign to INTENT(IN) variable '%s' at %L",      
	      sym->name, &lvalue->where);         
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
  
  
  
  
/* g95_set_implicit()-- Sets the implicit types parsed by the previous
 * g95_match_implicit(). */       
       
void g95_set_implicit(void) {     
int k;       
       
  for(k=0; k<G95_LETTERS; k++)    
    if (new_flag[k]) {     
      g95_current_ns->default_type[k] = new_ts[k];        
      g95_current_ns->set_flag[k] = 1;       
    }     
}    
    
    
         
         
/* g95_set_component_attr()-- Set component attributes from a standard
 * symbol attribute structure. */

void g95_set_component_attr(g95_component *p, symbol_attribute *attr) {       
       
  p->dimension = attr->dimension;  
  p->pointer = attr->pointer;  
}   
   
   
  
  
/* g95_set_default_type()-- Given a pointer to a symbol, set its type
 * according to the first letter of its name.  Fails if the letter in
 * question has no default type. */        
        
try g95_set_default_type(g95_symbol *s, int error_flag, g95_namespace *ns) {   
g95_typespec *ts;      
      
  if (s->ts.type != BT_UNKNOWN)     
    g95_internal_error("g95_set_default_type(): symbol already has a type");       
       
  ts = g95_get_default_type(s, ns);         
         
  if (ts->type == BT_UNKNOWN) {  
    if (error_flag)
      g95_error("Symbol '%s' at %L has no IMPLICIT type", s->name,          
		&s->declared_at);  
  
    return FAILURE;   
  } 
 
  s->ts = *ts;  
  s->attr.implicit_type = 1;    
    
  return SUCCESS;       
}        
        
        
     
     
/* g95_save_all()-- Mark those symbols which can be SAVEd as such. */          
          
void g95_save_all(g95_namespace *ns) {  
  
  g95_traverse_ns(ns, save_symbol);  
}  
  
  
   
   
/* check_used()-- Common subroutine called by attribute changing
 * subroutines in order to prevent them from changing a symbol that
 * has been use-associated.  Returns zero if it is OK to change the
 * symbol, nonzero if not.  */          
          
static int check_used(symbol_attribute *attr, locus *where) {    
    
  if (attr->use_assoc == 0) return 0;      
      
  if (where == NULL) where = g95_current_locus();   
   
  g95_error("Cannot change attributes of USE-associated symbol at %L", where);  
  
  return 1;        
}


         
         
/* g95_new_symbol()-- Allocate and initialize a new symbol node */

g95_symbol *g95_new_symbol(char *name, g95_namespace *ns) {          
g95_symbol *q;       
       
  q = g95_getmem(sizeof(g95_symbol));        
        
  g95_clear_ts(&q->ts);
  g95_clear_attr(&q->attr);        
  q->ns = ns;          
          
  q->declared_at = *g95_current_locus();    
    
  if (strlen(name) > G95_MAX_SYMBOL_LEN)          
    g95_internal_error("new_symbol(): Symbol name too long");         
         
  strcpy(q->name, name);          
  return q;      
}


  
/************** Component name management ************/   
   
/* Component names of a derived type form their own little namespaces
 * that are separate from all other spaces.  The space is composed of
 * a singly linked list of g95_component structures whose head is
 * located in the parent symbol. */

/* g95_add_component()-- Add a component name to a symbol.  The call
 * fails if the name is already present.  On success, the component
 * pointer is modified to point to the additional component structure. */

try g95_add_component(g95_symbol *s, char *name,   
		      g95_component **component) {        
g95_component *h, *tail;    
    
  tail = NULL;    
    
  for(h=s->components; h; h=h->next) {          
    if (strcmp(h->name, name) == 0) {         
      g95_error("Component '%s' at %C already declared at %L",      
		name, &h->loc);    
      return FAILURE;  
    }   
   
    tail = h;       
  }         
         
/* Allocate new component */

  h = g95_get_component();     
     
  if (tail == NULL) s->components = h;      
  else tail->next = h;     
     
  strcpy(h->name, name);       
  h->loc = *g95_current_locus();  
  
  *component = h;
  return SUCCESS;   
}


  
  
try g95_add_intent(symbol_attribute *attr, sym_intent intent, locus *where) {        
        
  if (check_used(attr, where)) return FAILURE; 
 
  if (attr->intent == INTENT_UNKNOWN) {     
    attr->intent = intent;  
    return check_conflict(attr, where);     
  }  
  
  if (where == NULL) where = g95_current_locus(); 
 
  g95_error("INTENT (%s) conflicts with INTENT(%s) at %L",          
	    g95_intent_string(attr->intent),    
	    g95_intent_string(intent), where);  
  
  return FAILURE;        
}   
   
   
 
 
try g95_add_elemental(symbol_attribute *attr, locus *where) {        
        
  if (check_used(attr, where)) return FAILURE;         
         
  attr->elemental = 1;
  return check_conflict(attr, where);       
}      
      
      
     
     
try g95_add_explicit_interface(g95_symbol *symbol, ifsrc source,          
			       g95_formal_arglist *formal, locus *where) {        
        
  if (check_used(&symbol->attr, where)) return FAILURE;  
  
  if (where == NULL) where = g95_current_locus(); 
 
  if (symbol->attr.if_source != IFSRC_UNKNOWN &&        
      symbol->attr.if_source != IFSRC_DECL) {    
    g95_error("Symbol '%s' at %L already has an explicit interface",         
	      symbol->name, where);      
    return FAILURE;   
  }          
          
  symbol->formal = formal;       
  symbol->attr.if_source = source;    
    
  return SUCCESS;     
}   
   
   
         
         
/* g95_get_namespace()-- Allocate a new namespace structure.  */        
        
g95_namespace *g95_get_namespace(g95_namespace *parent) {       
g95_namespace *ns;       
g95_typespec *ts;      
int l;   
    
  ns = g95_getmem(sizeof(g95_namespace));
  ns->sym_root = NULL;
  ns->uop_root = NULL;        
  ns->default_access = ACCESS_UNKNOWN;     
  ns->parent = parent;     
     
  for(l=0; l<G95_INTRINSIC_OPS; l++)        
    ns->operator_access[l] = ACCESS_UNKNOWN;  
  
/* Initialize default types */     
     
  for(l='a'; l<='z'; l++) {  
    ns->set_flag[l - 'a'] = 0;  
    ts = &ns->default_type[l - 'a'];          
          
    if (ns->parent != NULL) {    /* Copy parent settings */       
      *ts = ns->parent->default_type[l - 'a'];    
      continue; 
    }     
     
    if (g95_option.implicit_none != 0) { 
      g95_clear_ts(ts);         
      continue;       
    }     
     
    if ('i' <= l && l <= 'n') {
      ts->type = BT_INTEGER;          
      ts->kind = g95_default_integer_kind();        
    } else {   
      ts->type = BT_REAL;          
      ts->kind = g95_default_real_kind();      
    }         
  }         
         
  return ns;   
}        
        
        
      
      
try g95_add_target(symbol_attribute *attr, locus *where) {    
    
  if (check_used(attr, where)) return FAILURE; 
 
  if (attr->target) {          
    duplicate_attr("TARGET", where);        
    return FAILURE;         
  }    
    
  attr->target = 1;
  return check_conflict(attr, where);          
}       
       
       
     
     
/* clear_sym_mark()-- Clear mark bits from symbol nodes associated
 * with a symtree node */

static void clear_sym_mark(g95_symtree *st) {  
  
  st->n.sym->mark = 0;   
}


        
        
/* g95_compare_attr()-- Compares two attributes */         
         
int g95_compare_attr(symbol_attribute *l, symbol_attribute *i) {  
  
  return l->allocatable == i->allocatable &&   
    l->dimension == i->dimension && l->external == i->external &&      
    l->intrinsic == i->intrinsic && l->optional == i->optional &&      
    l->pointer == i->pointer     && l->save == i->save &&      
    l->target == i->target       && l->dummy == i->dummy &&   
    l->common == i->common       && l->result_var == i->result_var &&     
    l->entry == i->entry         && l->data == i->data &&   
    l->use_assoc == i->use_assoc && l->in_namelist == i->in_namelist &&    
    l->in_common == i->in_common && l->saved_common == i->saved_common &&         
    l->function == i->function   && l->subroutine == i->subroutine &&       
    l->sequence == i->sequence   && l->elemental == i->elemental &&       
    l->pure == i->pure           && l->recursive == i->recursive &&        
    l->access == i->access       && l->intent == i->intent &&      
    l->flavor == i->flavor       && l->proc == i->proc &&      
    l->generic == i->generic;         
}      
      
      
     
     
/* g95_new_symtree()-- Allocate a new symtree node and associate it
 * with the new symbol. */ 
 
g95_symtree *g95_new_symtree(g95_symtree **root, char *name) { 
g95_symtree *st;     
     
  st = g95_getmem(sizeof(g95_symtree));    
  strcpy(st->name, name);         
         
  g95_insert_bbt(root, st, g95_compare_symtree);  
  return st;       
}   
   
   
          
          
try g95_add_result(symbol_attribute *attr, locus *where) {    
    
  if (check_used(attr, where)) return FAILURE;         
         
  attr->result_var = 1;     
  return check_conflict(attr, where);       
}  
  
  
     
     
try g95_add_entry(symbol_attribute *attr, locus *where) {  
  
  if (check_used(attr, where)) return FAILURE;       
       
  if (attr->entry) {   
    duplicate_attr("ENTRY", where);       
    return FAILURE;        
  }         
         
  attr->entry = 1;          
  return check_conflict(attr, where);        
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
    
    
          
          
/* free_sym_tree()-- Recursive function that deletes an entire
 * red-black tree and all the symbols that it contains. */      
      
static void free_sym_tree(g95_symtree *rb) {   
g95_namespace *ns; 
g95_symbol *symbol;  
  
  if (rb == NULL) return;          
          
  free_sym_tree(rb->left); 
  free_sym_tree(rb->right);     
     
  symbol = rb->n.sym;  
  
  symbol->refs--;   
  if (symbol->refs < 0) g95_internal_error("free_sym_tree(): Negative refs");

  if (symbol->formal_ns != NULL && symbol->refs == 1) {       
    /* as formal_ns contains a reference to sym, delete formal_ns just
     * before the deletion of sym. */          
    ns = symbol->formal_ns;  
    symbol->formal_ns = NULL;        
    g95_free_namespace(ns);          
  } else if (symbol->refs == 0) {   /* Go ahead and delete the symbol */        
    g95_free_symbol(symbol); 
  }   
   
  g95_free(rb);   
}


    
    
/* g95_check_pointer_assign()-- Check that a pointer assignment is OK.
 * We first check lvalue, and we only check rvalue if it's not an
 * assignment to NULL() or a NULLIFY statement. */      
      
try g95_check_pointer_assign(g95_expr *lvalue, g95_expr *rvalue) {   
symbol_attribute attr;          
int is_pure;

  if (lvalue->symbol->ts.type == BT_UNKNOWN) {
    g95_error("Pointer assignment target is not a POINTER at %L",        
	      &lvalue->where);      
    return FAILURE;
  }       
       
  attr = g95_variable_attr(lvalue, NULL);     
  if (!attr.pointer) {   
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
 
    attr = g95_expr_attr(rvalue);  
    if (!attr.target && !attr.pointer) {      
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
        
        


/* switch_types()-- Recursive function to switch derived types of all
 * symbol in a namespace. */          
          
static void switch_types(g95_symtree *st, g95_symbol *from, g95_symbol *to) {   
g95_symbol *symbol;   
   
  if (st == NULL) return;       
       
  symbol = st->n.sym;          
  if (symbol->ts.type == BT_DERIVED && symbol->ts.derived == from)         
    symbol->ts.derived = to;        
        
  switch_types(st->left, from, to);          
  switch_types(st->right, from, to);        
}        
        
        
       
       
/* g95_add_flavor()-- Flavors are special because some flavors are not
 * what fortran considers attributes and can be reaffirmed multiple
 * times. */ 
 
try g95_add_flavor(symbol_attribute *attr, sym_flavor d, locus *where) {     
     
  if ((d == FL_PROGRAM || d == FL_BLOCK_DATA || d == FL_MODULE || 
       d == FL_PARAMETER || d == FL_LABEL || d == FL_DERIVED ||   
       d == FL_NAMELIST) && check_used(attr, where)) return FAILURE; 
 
  if (attr->flavor == d && d == FL_VARIABLE) return SUCCESS;      
      
  if (attr->flavor != FL_UNKNOWN) {     
    if (where == NULL) where = g95_current_locus();       
       
    g95_error("%s attribute conflicts with %s attribute at %L",         
	      g95_code2string(flavors, attr->flavor),    
	      g95_code2string(flavors, d), where);      
      
    return FAILURE; 
  }

  attr->flavor = d;     
     
  return check_conflict(attr, where);      
}     
     
     
          
          
void g95_show_components(g95_symbol *symb) {     
g95_component *e;

  for(e=symb->components; e; e=e->next) {          
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
 
 
   
   
try g95_add_saved_common(symbol_attribute *attr, locus *where) {

  if (check_used(attr, where)) return FAILURE;     
     
  if (attr->saved_common) {
    duplicate_attr("SAVE", where);   
    return FAILURE;  
  } 
 
  attr->saved_common = 1;     
  return check_conflict(attr, where);      
}        
        
        
    
    
/* g95_get_componentr_attr()-- Get a standard symbol attribute
 * structure given the component structure. */     
     
void g95_get_component_attr(symbol_attribute *attr, g95_component *n) {    
    
  g95_clear_attr(attr);        
  attr->dimension = n->dimension;        
  attr->pointer = n->pointer;  
}         
         
         
          
          
/* g95_undo_symbols()-- Undoes all the changes made to symbols in the
 * current statement.  This subroutine is made simpler due to the fact
 * that attributes are never removed once added. */     
     
void g95_undo_symbols(void) {  
g95_symbol *o, *u, *old;  
g95_symtree *x, *k;   
   
/* if (changed_syms != NULL) g95_status("Undoing symbols\n"); */ 
 
  for(o=changed_syms; o; o=u) {         
    u = o->tlink;       
    /* g95_status("Undoing %s\n", p->name); */       
       
    if (o->new) {  /* Symbol was new */     
      delete_symtree(&o->ns->sym_root, o->name);         
         
      o->refs--;          
      if (o->refs < 0) g95_internal_error("g95_undo_symbols(): Negative refs");         
      if (o->refs == 0) g95_free_symbol(o);     
      continue;
    }     
     
/* Restore previous state of symbol.  Just copy simple stuff */  
  
    o->mark = 0;      
    old = o->old_symbol;         
         
    o->ts.type = old->ts.type;        
    o->ts.kind = old->ts.kind; 
 
    o->attr = old->attr; 
 
    if (o->value != old->value) {         
      g95_free_expr(old->value);   
      o->value = NULL; 
    }     
     
    if (o->as != old->as) {
      if (o->as) g95_free_array_spec(o->as);      
      o->as = old->as;        
    }      
      
    o->generic = old->generic;       
    o->component_access = old->component_access;        
        
    if (o->namelist != NULL && old->namelist == NULL) {  
      g95_free_namelist(o->namelist);     
      o->namelist = NULL;       
    } else {

      if (o->namelist_tail != old->namelist_tail) { 
	g95_free_namelist(old->namelist_tail); 
	old->namelist_tail->next = NULL;
      }          
    }          
          
    o->namelist_tail = old->namelist_tail;

    if (o->formal != old->formal) {   
      g95_free_formal_arglist(o->formal);        
      o->formal = old->formal;      
    }         
         
    g95_free(o->old_symbol);     
    o->old_symbol = NULL;       
    o->tlink = NULL;
  } 
 
  changed_syms = NULL;    
    
  /* Unlink host associated symtrees */   
   
  for(x=changed_st; x; x=k) {        
    k = x->link; 
 
    g95_delete_bbt(&g95_current_ns->sym_root, x, g95_compare_symtree);   
    g95_free(x);  
  }       
       
  changed_st = NULL;
} 
 
 
   
   
/* g95_symbol_state()-- Makes sure that no changes to symbols are pending */    
    
void g95_symbol_state(void) {  
  
  if (changed_syms != NULL)      
    g95_internal_error("Symbol changes still pending");          
}   
 
 
try g95_add_subroutine(symbol_attribute *attr, locus *where) {

  if (attr->flavor != FL_PROCEDURE &&
      g95_add_flavor(attr, FL_PROCEDURE, where) == FAILURE) return FAILURE;    
    
  attr->subroutine = 1;
  return check_conflict(attr, where);    
}          
          
          
 
 
try g95_add_recursive(symbol_attribute *attr, locus *where) {          
          
  if (check_used(attr, where)) return FAILURE;    
    
  attr->recursive = 1;         
  return check_conflict(attr, where);   
}  
  
  
 
 
/* g95_commit_symbols()-- Makes the changes made in the current
 * statement permanent-- gets rid of undo information. */          
          
void g95_commit_symbols(void) {
g95_symbol *p, *q;   
g95_symtree *x, *u; 
 
#if 0
  if (changed_syms != NULL) g95_status("Committing symbols\n");        
#endif
  
  for(p=changed_syms; p; p=q) {         
    q = p->tlink;     
    p->tlink = NULL;   
    p->mark = 0;      
    p->new = 0;

    if (p->old_symbol != NULL) {          
      g95_free(p->old_symbol); 
      p->old_symbol = NULL;   
    }      
  }         
         
  changed_syms = NULL;        
        
  for(x=changed_st; x; x=u) {     
    u = x->link;   
    x->link = NULL;   
  }        
        
  changed_st = NULL;          
}    
    
    
    
    
/* g95_module_symbol()-- Return nonzero if the symbol lives in a
 * module namespace or has been use-associated. */  
  
int g95_module_symbol(g95_symbol *symbol) {

  return symbol->ns->proc_name->attr.flavor == FL_MODULE || symbol->attr.use_assoc;      
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
         
         
         
         
/* g95_find_component()-- Given a derived type node and a component
 * name, try to locate the component structure.  Returns the NULL
 * pointer if the component is not found or the components are
 * private. */          
           
g95_component *g95_find_component(g95_symbol *s, char *name) {          
g95_component *p;         
         
  if (name == NULL) return NULL;          
          
  s = g95_use_derived(s);         
         
  if (s == NULL) return NULL;          
          
  for(p=s->components; p; p=p->next)        
    if (strcmp(p->name, name) == 0) break;    
    
  if (p == NULL)          
    g95_error("'%s' at %C is not a member of the '%s' structure",        
	      name, s->name);     
  else {   
    if (s->attr.use_assoc && s->component_access == ACCESS_PRIVATE) {         
      g95_error("Component '%s' at %C is a PRIVATE component of '%s'",
		name, s->name);     
      p = NULL;          
    }      
  }      
      
  return p;        
}        
        
        
    
    
void g95_symbol_init_2(void) {    
    
  g95_current_ns = g95_get_namespace(NULL); 
}    
    
    
 
 
try g95_add_pointer(symbol_attribute *attr, locus *where) {    
    
  if (check_used(attr, where)) return FAILURE;         
         
  attr->pointer = 1;     
  return check_conflict(attr, where);   
}


    
    
/* g95_get_default_type()-- Given a symbol, return a pointer to the
 * typespec for it's default type */  
  
g95_typespec *g95_get_default_type(g95_symbol *symb, g95_namespace *ns) {         
char letter;  
  
  letter = symb->name[0];     
  if (letter < 'a' || letter > 'z')  
    g95_internal_error("g95_get_default_type(): Bad symbol");

  if (ns == NULL) ns = g95_current_ns;     
     
  return &ns->default_type[letter - 'a'];    
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
g95_typespec ts;       
locus cur_loc;  
int g, f;        
match v;    
    
  for(f=0; f<G95_LETTERS; f++) {      
    g95_clear_ts(&new_ts[f]);       
    if (new_flag[f]) new_flag[f] = 0;       
  }     
     
  if (g95_match_eos() == MATCH_YES) {
    g95_error("Empty IMPLICIT statement at %C");  
    return MATCH_ERROR;       
  }    
    
  do {       
    v = g95_match_type_spec(&ts, 0); /* A basic type is mandatory here */     
    if (v == MATCH_ERROR) goto error;  
    if (v == MATCH_NO) goto syntax;  
  
    cur_loc = *g95_current_locus();          
    v = match_implicit_range(&ts);          
          
    if (v == MATCH_YES) {   /* Looks like we have the <TYPE> (<RANGE>) */   
      g95_gobble_whitespace();         
      g = g95_next_char();  
      if ((g == '\n') || (g == ',')) continue;    
    
      g95_set_locus(&cur_loc);      
    }       
       
    /* Last chance-- check <TYPE> (<KIND>) (<RANGE>) */

    v = g95_match_kind_spec(&ts);        
    if (v == MATCH_ERROR) goto error;         
    if (v == MATCH_NO) {         
      v = g95_match_old_kind_spec(&ts);       
      if (v == MATCH_ERROR) goto error; 
      if (v == MATCH_NO) goto syntax;          
    }        
        
    v = match_implicit_range(&ts);    
    if (v == MATCH_ERROR) goto error;         
    if (v == MATCH_NO) goto syntax;       
           
    g95_gobble_whitespace();      
    g = g95_next_char();  
    if ((g != '\n') && (g != ',')) goto syntax;      
      
  } while(g == ',');  
  
/* An implicit statement has been fully matched at this point.  Now
 * check to see if merging the new implicit types back into the
 * existing types will work. */   
   
  for(f=0; f<G95_LETTERS; f++)
    if (new_flag[f]) {        
      if (g95_current_ns->set_flag[f]) {          
	g95_error("Letter %c already has an IMPLICIT type at %C", f+'A');  
	goto error;      
      }   
    }   
   
  return MATCH_YES;

syntax: 
  g95_syntax_error(ST_IMPLICIT);

error:          
  return MATCH_ERROR;
}     
     
     
   
   
/* g95_find_uop()-- Given a name find the user operator node.  Returns
 * NULL if it does not exist. */  
  
g95_user_op *g95_find_uop(char *name, g95_namespace *ns) {  
g95_symtree *st;       
       
  if (ns == NULL) ns = g95_current_ns;          
          
  st = g95_find_symtree(ns->uop_root, name);       
  return (st == NULL) ? NULL : st->n.uop;        
}


      
      
/* traverse_ns()-- Recursive namespace traversal function. */ 
 
static void traverse_ns(g95_symtree *st, void (*func)(g95_symbol *)) {  
  
  if (st == NULL) return;

  if (st->n.sym->mark == 0) (*func)(st->n.sym);  
  st->n.sym->mark = 1;          
          
  traverse_ns(st->left, func); 
  traverse_ns(st->right, func);         
}     
     
     


/* g95_get_ha_symbol()-- Subroutine that searches for a symbol,
 * creating it if it doesn't exist, but tries to host-associate the
 * symbol if possible. */

int g95_get_ha_symbol(char *name, g95_symbol **result) {      
g95_symbol *symb;   
int d;        
        
  d = g95_find_symbol(name, g95_current_ns, 0, &symb);   
  if (symb != NULL) {
    save_symbol_data(symb); 
 
    *result = symb;     
    return d;   
  }        
        
  if (g95_current_ns->parent != NULL) {       
    d = g95_find_symbol(name, g95_current_ns->parent, 1, &symb);        
    if (d) return d;    
    
    if (symb != NULL) {      
      *result = symb;    
      return 0;         
    }      
  }    
    
  return g95_get_symbol(name, g95_current_ns, result);        
}    
    
    
  
  
static void show_indent(void) {     
int s;         
         
  g95_status_char('\n');   
  for(s=0; s<2*show_level; s++)
    g95_status_char(' ');        
}    
    
    
       
       
/* traverse_symtree()-- Recursively traverse the symtree nodes. */       
       
static void traverse_symtree(g95_symtree *st, void (*func)(g95_symtree *)) {   
   
  if (st != NULL) {        
    (*func)(st);         
         
    traverse_symtree(st->left, func);  
    traverse_symtree(st->right, func); 
  }
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
  if (attr->common)         g95_status(" COMMON");     
  if (attr->result_var)     g95_status(" RESULT");  
  if (attr->entry)          g95_status(" ENTRY");         
         
  if (attr->data)           g95_status(" DATA");          
  if (attr->use_assoc)      g95_status(" USE-ASSOC");          
  if (attr->in_namelist)    g95_status(" IN-NAMELIST");          
  if (attr->in_common)      g95_status(" IN-COMMON");          
  if (attr->saved_common)   g95_status(" SAVED-COMMON");

  if (attr->function)       g95_status(" FUNCTION"); 
  if (attr->subroutine)     g95_status(" SUBROUTINE");
  if (attr->implicit_type)  g95_status(" IMPLICIT-TYPE");      
      
  if (attr->sequence)       g95_status(" SEQUENCE");          
  if (attr->elemental)      g95_status(" ELEMENTAL");          
  if (attr->pure)           g95_status(" PURE");       
  if (attr->recursive)      g95_status(" RECURSIVE");          
          
  g95_status(")");     
}  
  
  
      
      
try g95_add_external(symbol_attribute *attr, locus *where) {  
  
  if (check_used(attr, where)) return FAILURE;  
    
  if (attr->external) {  
    duplicate_attr("EXTERNAL", where);       
    return FAILURE;     
  }         
         
  attr->external = 1;  
  
  return check_conflict(attr, where);  
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
  
  


try g95_add_in_common(symbol_attribute *attr, locus *where) {      
      
  if (check_used(attr, where)) return FAILURE;      
      
  /* Duplicate attribute already checked for */    
    
  attr->in_common = 1;         
  if (check_conflict(attr, where) == FAILURE) return FAILURE;

  if (attr->flavor == FL_VARIABLE) return SUCCESS;  
  
  return g95_add_flavor(attr, FL_VARIABLE, where);    
}          
          
          
        
        
try g95_add_procedure(symbol_attribute *attr, procedure_type z, locus *where) {    
    
  if (check_used(attr, where)) return FAILURE;   
   
  if (attr->flavor != FL_PROCEDURE &&        
      g95_add_flavor(attr, FL_PROCEDURE, where) == FAILURE) return FAILURE; 
 
  if (where == NULL) where = g95_current_locus();         
         
  if (attr->proc != PROC_UNKNOWN) {        
    g95_error("%s procedure at %L is already %s %s procedure",      
	      g95_code2string(procedures, z), where,     
	      g95_article(g95_code2string(procedures, attr->proc)),
	      g95_code2string(procedures, attr->proc)); 
 
    return FAILURE;     
  }    
    
  attr->proc = z; 
 
/* Statement functions are always scalar and functions */      
      
  if (z == PROC_ST_FUNCTION &&   
      ((!attr->function && g95_add_function(attr, where) == FAILURE)       
       || attr->dimension)) return FAILURE;     
     
  return check_conflict(attr, where);       
}       
       
       
  
  
/* ambiguous_symbol()-- Generate an error if a symbol is ambiguous. */    
    
static void ambiguous_symbol(char *name, g95_symtree *st) {         
         
  if (st->n.sym->module[0])
    g95_error("Name '%s' at %C is an ambiguous reference to '%s' "       
	      "from module '%s'", name, st->n.sym->name,    
	      st->n.sym->module);     
  else     
    g95_error("Name '%s' at %C is an ambiguous reference to '%s' "  
	      "from current program unit", name, st->n.sym->name);         
}      
      
      
  
  
try g95_add_sequence(symbol_attribute *attr, locus *where) {    
    
  if (check_used(attr, where)) return FAILURE;       
       
  attr->sequence = 1; 
  return check_conflict(attr, where);     
}         
         
         
        
        
/* g95_show_symbol()-- Show a symbol.  If a symbol is an ENTRY,
 * SUBROUTINE or FUNCTION, we show the interface.  Information needed
 * to reconstruct the list of specific interfaces associated with a
 * generic symbol is done within that symbol. */

void g95_show_symbol(g95_symbol *sym) {
g95_formal_arglist *formal;          
g95_interface *intr;   
g95_symbol *g;         
         
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
    for(intr=sym->generic; intr; intr=intr->next)        
      g95_status(" %s", intr->sym->name);          
  }  
  
  if (sym->common_head) {  
    show_indent(); 
    g95_status("Common members:");       
    for(g=sym->common_head; g; g=g->common_next)     
      g95_status(" %s", g->name);   
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
       
    for(formal=sym->formal; formal; formal=formal->next)         
      g95_status(" %s", formal->sym->name);       
  }

  if (sym->formal_ns) {          
    show_indent();          
    g95_status("Formal namespace");    
    g95_show_namespace(sym->formal_ns);  
  }        
        
  g95_status_char('\n');    
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
        
        
          
          
try g95_add_dimension(symbol_attribute *attr, locus *where) {

  if (check_used(attr, where)) return FAILURE;   
   
  if (attr->dimension) {         
    duplicate_attr("DIMENSION", where);  
    return FAILURE;     
  }         
         
  attr->dimension = 1;    
  return check_conflict(attr, where);     
}    
    
    
     
     
try g95_add_save(symbol_attribute *attr, locus *where) {      
      
  if (check_used(attr, where)) return FAILURE;      
      
  if (g95_pure(NULL)) {       
    g95_error("SAVE attribute at %L cannot be specified in a PURE procedure",
	      where);   
    return FAILURE;    
  }          
          
  if (attr->save) {  
    duplicate_attr("SAVE", where);          
    return FAILURE;        
  }         
         
  attr->save = 1;         
  return check_conflict(attr, where);     
}         
         
         
          
/* free_st_labels()-- Free a whole list of g95_st_label structures.  */     
     
static void free_st_labels(g95_st_label *r) {
g95_st_label *x;          
          
  for(; r; r=x) {          
    x = r->next;     
    if (r->format != NULL) g95_free_expr(r->format);    
    g95_free(r); 
  }       
}      
      
      
     
     
/* g95_reference_st_label()-- Reference a label.  Given a label
 * and its type, see if that reference is consistent with what is
 * known about that label, updating the unknown state.  Returns
 * FAILURE if something goes wrong. */    
    
try g95_reference_st_label(g95_st_label *lp, g95_sl_type type) {     
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
  
  if (label_type == ST_LABEL_FORMAT && type == ST_LABEL_TARGET) {  
    g95_error("Label %d at %C previously used as a FORMAT label", labelno);       
    rc = FAILURE;
    goto done;       
  }         
         
  if ((label_type == ST_LABEL_TARGET || label_type == ST_LABEL_BAD_TARGET)   
      && type == ST_LABEL_FORMAT) {       
    g95_error("Label %d at %C previously used as branch target", labelno); 
    rc = FAILURE;
    goto done; 
  }   
   
  lp->referenced = type;          
  rc = SUCCESS;   
   
done:       
  return rc;  
}      
      
      
   
   
/* g95_free_namespace()-- Free a namespace structure and everything
 * below it.  Interface lists associated with intrinsic operators are
 * not freed.  These are taken care of when a specific name is freed. */      
      
void g95_free_namespace(g95_namespace *ns) {       
g95_charlen *cl, *cl2;      
g95_namespace *m, *w;  
int r;  
  
  if (ns == NULL) return; 
 
  g95_free_statements(ns->code);       
       
  free_sym_tree(ns->sym_root);     
  free_uop_tree(ns->uop_root);     
     
  for(cl=ns->cl_list; cl; cl=cl2) {      
    cl2 = cl->next;       
    g95_free_expr(cl->length);
    g95_free(cl);     
  } 
 
  free_st_labels(ns->st_labels);   
   
  g95_free_equiv(ns->equiv);     
     
  for(r=0; r<G95_INTRINSIC_OPS; r++)
    g95_free_interface(ns->operator[r]);          
          
  g95_free_data(ns->data);      
  m = ns->contained;        
  g95_free(ns);

  /* Recursively free any contained namespaces */   
   
  while(m != NULL) {        
    w = m;          
    m = m->sibling;

    g95_free_namespace(w);   
  }    
}


   
   
try g95_add_dummy(symbol_attribute *attr, locus *where) {

  if (check_used(attr, where)) return FAILURE;     
     
  /* Duplicate dummy arguments are allow due to ENTRY statements */  
  
  attr->dummy = 1;
  return check_conflict(attr, where);    
}




/* traverse_uop()-- Function for traversing the user operator symtree */   
   
static void traverse_uop(g95_symtree *st, void (*func)(g95_user_op *)) {   
   
  if (st == NULL) return;        
        
  (*func)(st->n.uop);   
   
  traverse_uop(st->left, func);  
  traverse_uop(st->right, func);    
}


       
       
try g95_add_intrinsic(symbol_attribute *attr, locus *where) {        
        
  if (check_used(attr, where)) return FAILURE;  
  
  if (attr->intrinsic) {        
    duplicate_attr("INTRINSIC", where);  
    return FAILURE;   
  }         
         
  attr->intrinsic = 1; 
 
  return check_conflict(attr, where);      
}         
         
         
 
 
/* g95_get_uop()-- Given a name find a user operator node, creating it
 * if it doesn't exist.  These are much simpler than symbols because
 * they can't be ambiguous with one another */    
    
g95_user_op *g95_get_uop(char *name) {
g95_user_op *uop;      
g95_symtree *st;         
         
  st = g95_find_symtree(g95_current_ns->uop_root, name);
  if (st != NULL) return st->n.uop;        
        
  st = g95_new_symtree(&g95_current_ns->uop_root, name);     
     
  uop = st->n.uop = g95_getmem(sizeof(g95_user_op));   
  strcpy(uop->name, name); 
  uop->access = ACCESS_UNKNOWN;         
  uop->ns = g95_current_ns;     
     
  return uop;    
}      
      
      
 
 
/* g95_traverse_ns()-- Call a given function for all symbols in the
 * namespace.  We take care that each g95_symbol node is called
 * exactly once. */       
       
void g95_traverse_ns(g95_namespace *ns, void (*func)(g95_symbol *)) {        
        
  g95_traverse_symtree(ns, clear_sym_mark);

  traverse_ns(ns->sym_root, func);        
}      
      
      
         
         
try g95_add_common(symbol_attribute *attr, locus *where) { 
 
  if (check_used(attr, where)) return FAILURE;     
     
  attr->common = 1;        
  return check_conflict(attr, where);   
}        
        
        
    
    
try g95_add_function(symbol_attribute *attr, locus *where) {      
      
  if (attr->flavor != FL_PROCEDURE && 
      g95_add_flavor(attr, FL_PROCEDURE, where) == FAILURE) return FAILURE; 
 
  attr->function = 1;   
  return check_conflict(attr, where);         
}  
  
  
   
   
/* g95_find_symbol()-- search for a symbol starting in the current
 * namespace, resorting to any parent namespaces if requested by a
 * nonzero parent_flag.  Returns nonzero if the symbol is
 * ambiguous. */        
        
int g95_find_symbol(char *name, g95_namespace *ns, int parent_flag,
		    g95_symbol **result) {
g95_symtree *st;   
   
  if (ns == NULL) ns = g95_current_ns;         
         
  do {          
    st = g95_find_symtree(ns->sym_root, name);    
    if (st != NULL) { 
      *result = st->n.sym; 
      if (st->ambiguous) {    
	ambiguous_symbol(name, st);     
	return 1;         
      } 
 
      return 0;  
    } 
 
    if (!parent_flag) break;          
          
    ns = ns->parent;   
  } while (ns != NULL);         
         
  *result = NULL;  
  return 0;        
} 
 
 
         
         
/* g95_traverse_user_op()-- Traverse the tree of user operator nodes.  */    
    
void g95_traverse_user_op(g95_namespace *ns, void (*func)(g95_user_op *)) {     
     
  traverse_uop(ns->uop_root, func);
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
    
g95_symbol *g95_use_derived(g95_symbol *symbol) {    
g95_symbol *h, *p;         
g95_typespec *d;       
g95_symtree *st;       
int f;     
     
  if (symbol->components != NULL) return symbol;   /* Already defined */ 
 
  if (symbol->ns->parent == NULL) goto bad;   
   
  if (g95_find_symbol(symbol->name, symbol->ns->parent, 1, &h)) {
    g95_error("Symbol '%s' at %C is ambiguous", symbol->name);       
    return NULL;         
  }

  if (h == NULL || h->attr.flavor != FL_DERIVED) goto bad;          
          
  /* Get rid of symbol sym, translating all references to s */      
      
  for(f=0; f<G95_LETTERS; f++) {  
    d = &symbol->ns->default_type[f];    
    if (d->derived == symbol) d->derived = h;    
  }   
   
  st = g95_find_symtree(symbol->ns->sym_root, symbol->name);  
  st->n.sym = h;      
      
  h->refs++;      
      
  /* Unlink from list of modified symbols */         
         
  if (changed_syms == symbol)       
    changed_syms = symbol->tlink;         
  else        
    for(p=changed_syms; p; p=p->tlink)         
      if (p->tlink == symbol) {    
	p->tlink = symbol->tlink;          
	break;     
      }

  switch_types(symbol->ns->sym_root, symbol, h);    
    
  /* TODO: Also have to replace sym -> s in other lists like
   * namelists, common lists and interface lists.  */ 
 
  g95_free_symbol(symbol);        
        
  return h;     
     
 bad:    
  g95_error("Derived type '%s' at %C is being used before it is defined",
	    symbol->name);         
  return NULL;       
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
  
int g95_get_symbol(char *name, g95_namespace *ns, g95_symbol **result) { 
g95_symtree *st;  
g95_symbol *c;         
         
  /* This doesn't usually happen during resolution.  */        
  if (ns == NULL) ns = g95_current_ns;      
      
  /* Try to find the symbol. */         
  st = g95_find_symtree(ns->sym_root, name);      
      
  if (st == NULL) {         /* If not there, create a new symbol */  
    c = g95_new_symbol(name, ns);     
    
    c->old_symbol = NULL;   /* Add to the list of tentative symbols. */        
    c->tlink = changed_syms; 
    c->mark = 1;
    c->new = 1;
    changed_syms = c;      
      
    st = g95_new_symtree(&ns->sym_root, name);         
    st->n.sym = c;       
    c->refs++;      
      
  } else {    /* Make sure the existing symbol is OK */   
    if (st->ambiguous) {        
      ambiguous_symbol(name, st);          
      return 1;         
    }       
       
    c = st->n.sym;       
       
    if (c->ns != ns && (!c->attr.function || ns->proc_name != c) &&       
	!c->attr.entry) {     
      /* Symbol is from another namespace */  
      g95_error("Symbol '%s' at %C has already been host associated", name);          
      return 2; 
    }        
        
    c->mark = 1;    
    
    save_symbol_data(c);      /* Copy in case this symbol is changed */    
  }

  *result = c;         
  return 0;         
}      
      
      
       
       
void g95_traverse_symtree(g95_namespace *ns, void (*func)(g95_symtree *)) {    
    
  traverse_symtree(ns->sym_root, func);    
}       
       
       
      
      
try g95_add_pure(symbol_attribute *attr, locus *where) {    
    
  if (check_used(attr, where)) return FAILURE;          
          
  attr->pure = 1;   
  return check_conflict(attr, where);     
}     
     
     


void g95_symbol_done_2(void) {     
     
  g95_free_namespace(g95_current_ns);    
  g95_current_ns = NULL;       
}         
         
         
          
          
/* g95_show_namespace()-- Show a namespace */         
         
void g95_show_namespace(g95_namespace *ns) {  
g95_interface *intr;   
g95_namespace *save;
int h;          
          
  save = g95_current_ns;     
  show_level++;         
        
  show_indent();      
  g95_status("Namespace:");  
  
  if (ns != NULL) {     
    for(h=0; h<G95_LETTERS; h++) {        
      g95_status(" %c: ", h+'A');     
      g95_show_typespec(&ns->default_type[h]);     
    }         
         
    if (ns->proc_name != NULL) {       
      show_indent();     
      g95_status("procedure name = %s", ns->proc_name->name);         
    }        
        
    g95_traverse_symtree(ns, clear_sym_mark); 
 
    g95_current_ns = ns;       
    g95_traverse_symtree(ns, show_symtree); 
 
    for(h=0; h<G95_INTRINSIC_OPS; h++) {    /* User operator interfaces */         
      intr = ns->operator[h];         
      if (intr == NULL) continue;      
      
      show_indent();         
      g95_status("Operator interfaces for %s:", g95_op2string(h));          
          
      for(; intr; intr=intr->next)
	g95_status(" %s", intr->sym->name);   
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
  
  
         
         
try g95_add_generic(symbol_attribute *attr, locus *where) {   
   
  if (attr->flavor != FL_PROCEDURE && 
      g95_add_flavor(attr, FL_PROCEDURE, where) == FAILURE) return FAILURE;     
     
  attr->generic = 1;         
  return check_conflict(attr, where);   
}  
  
  
