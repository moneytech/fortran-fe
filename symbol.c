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
g95_gsymbol *g95_gsym_root = NULL;          
          
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
        
        


/* g95_set_implicit()-- Sets the implicit types parsed by the previous
 * g95_match_implicit(). */  
  
void g95_set_implicit(void) {
int g;          
          
  for(g=0; g<G95_LETTERS; g++) 
    if (new_ts[g].type != BT_UNKNOWN) {  
      g95_current_ns->default_type[g] = new_ts[g];
      g95_current_ns->set_flag[g] = 1;      
    } 
}




/* g95_get_default_type()-- Given a symbol, return a pointer to the
 * typespec for it's default type */       
       
g95_typespec *g95_get_default_type(g95_symbol *s, g95_namespace *n) {  
char letter;     
     
  letter = s->name[0];
  if (letter < 'a' || letter > 'z')         
    g95_internal_error("g95_get_default_type(): Bad symbol");

  if (n == NULL) n = g95_current_ns;         
         
  return &n->default_type[letter - 'a'];  
}         
         
         
      
      
/* save_symbol()-- Given a symbol, mark it as SAVEd if it is allowed */

static void save_symbol(g95_symbol *s) {       
       
  if (s->attr.use_assoc) return;  
  
  if (s->attr.in_common || s->attr.dummy ||   
      s->attr.flavor != FL_VARIABLE) return;          
          
  g95_add_save(&s->attr, &s->declared_at);        
}      
      
      


/* g95_set_implicit_none()-- Handle a correctly parsed IMPLICIT NONE */     
     
void g95_set_implicit_none(void) {          
int v;     
     
  for(v='a'; v<='z'; v++) {     
    g95_clear_ts(&g95_current_ns->default_type[v - 'a']);     
    g95_current_ns->set_flag[v - 'a'] = 1;       
  }         
}    
    
    
        
        
/* g95_match_implicit_none()-- Match an IMPLICIT NONE statement. */  
  
match g95_match_implicit_none(void) {     
     
  return (g95_match_eos() == MATCH_YES) ? MATCH_YES : MATCH_NO; 
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
    g95_error("Incompatible ranks in assignment at %L (%d/%d)",          
	      &lvalue->where, lvalue->rank, rvalue->rank);      
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
       
       
    
    
/* g95_get_st_label()-- Given a label number, search for and return a
 * pointer to the label structure, creating it if it does not exist.
 * A label number of zero means return a unique label. */       
       
g95_st_label *g95_get_st_label(int labelno) {          
static int unique_label = 100000;        
g95_st_label *lp; 
 
  if (labelno == 0) labelno = unique_label++;  
  
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
      
      
 
 
/* match_implicit_range()-- Match the letter range(s) of an IMPLICIT
 * statement.  The new_flag[] array is updated. */      
      
static match match_implicit_range(void) {      
int f, q, r, s, flag; 
g95_locus cur_loc;  
  
  for(q=0; q<G95_LETTERS; q++)    
    new_flag[q] = 0;         
         
  cur_loc = g95_current_locus;    
   
  g95_gobble_whitespace();
  f = g95_next_char(); 
  if (f != '(') { 
    g95_error("Missing character range in IMPLICIT at %C");
    goto bad;      
  } 
 
  flag = 1;  
  while(flag) {         
    g95_gobble_whitespace();        
    r = s = g95_next_char();         
    if (!isalpha(r)) goto bad;          
          
    g95_gobble_whitespace();       
    f = g95_next_char();   
   
    switch(f) { 
    case ')':      
      flag = 0;       
      break;      
      
    case ',':    
      break;          
          
    case '-':     
      g95_gobble_whitespace();
      s = g95_next_char(); 
      if (!isalpha(s)) goto bad;     
     
      g95_gobble_whitespace();  
      f = g95_next_char();        
        
      if (f != ',' && f != ')') goto bad;    
      if (f == ')') flag = 0; 
 
      break;          
          
    default: goto bad;          
    }   
   
    if (r > s) {
      g95_error("Letters must be in alphabetic order in IMPLICIT statement "     
		"at %C");     
      goto bad;       
    }      
      
    r -= 'a';         
    s -= 'a'; 
 
    for(q=r; q<=s; q++) {   
      if (new_flag[q] || g95_current_ns->set_flag[q]) {    
	g95_error("Letter '%c' already set in IMPLICIT statement at %C",        
		  q+'A');     
	goto bad;         
      }    
    
      new_flag[q] = 1;  
    }
  }    
    
  return MATCH_YES;  
  
bad:       
  g95_syntax_error(ST_IMPLICIT);          
          
  g95_current_locus = cur_loc;  
  return MATCH_ERROR;
}      
      
      
 
 
/* free_components()-- Given a symbol, free all of the component
 * structures and everything they point to. */ 
 
static void free_components(g95_component *f) {         
g95_component *s;  
  
  for(; f; f=s) {       
    s = f->next;          
          
    g95_free_array_spec(f->as);          
    g95_free_expr(f->initializer);          
          
    g95_free(f);   
  }  
} 
 
 
   
   
/* g95_get_uop()-- Given a name find a user operator node, creating it
 * if it doesn't exist.  These are much simpler than symbols because
 * they can't be ambiguous with one another */     
     
g95_user_op *g95_get_uop(char *nm) {  
g95_user_op *operator;    
g95_symtree *s;       
       
  s = g95_find_symtree(g95_current_ns->uop_root, nm);       
  if (s != NULL) return s->n.uop;   
   
  s = g95_new_symtree(&g95_current_ns->uop_root, nm);

  operator = s->n.uop = g95_getmem(sizeof(g95_user_op));      
  strcpy(operator->name, nm);         
  operator->access = ACCESS_UNKNOWN; 
  operator->ns = g95_current_ns;       
       
  return operator;         
}     
     
     
          
          
/* g95_new_symbol()-- Allocate and initialize a new symbol node */       
       
g95_symbol *g95_new_symbol(char *name, g95_namespace *names) {          
g95_symbol *d;

  d = g95_getmem(sizeof(g95_symbol));      
      
  g95_clear_ts(&d->ts);        
  g95_clear_attr(&d->attr);      
  d->ns = names;        
        
  d->declared_at = g95_current_locus;        
        
  if (strlen(name) > G95_MAX_SYMBOL_LEN)         
    g95_internal_error("new_symbol(): Symbol name too long");    
    
  strcpy(d->name, name);        
  return d;     
}          
          
          
      
      
/* g95_module_symbol()-- Return nonzero if the symbol lives in a
 * module namespace or has been use-associated. */       
       
int g95_module_symbol(g95_symbol *sy) {   
   
  return (sy->ns->proc_name != NULL &&
	  sy->ns->proc_name->attr.flavor == FL_MODULE)       
    || sy->attr.use_assoc;    
}   
   
   
  
  
/* update_implicit()-- Update the implicit variables of the current
 * namespace. */  
  
static void update_implicit(g95_typespec *t) {
int g;          
          
  for(g=0; g<G95_LETTERS; g++) 
    if (new_flag[g])  
      new_ts[g] = *t;     
}   
   
   
      
      
/* g95_find_gsymbol()-- Search a tree for the global symbol. */       
       
g95_gsymbol *g95_find_gsymbol(g95_gsymbol *symbol, char *n) {       
g95_gsymbol *z;         
         
  if (symbol == NULL) return NULL;      
  if (strcmp(symbol->name, n) == 0) return symbol;       
       
  z = g95_find_gsymbol(symbol->left, n);  
  if (z != NULL) return z;       
       
  z = g95_find_gsymbol(symbol->right, n); 
  if (z != NULL) return z;       
       
  return NULL;
}          
          
          
  
  
/* delete_symtree()-- delete a symbol from the tree.  Does not free the
 * symbol itself! */         
         
static void delete_symtree(g95_symtree **root, char *n) { 
g95_symtree st, *st0;      
      
  st0 = g95_find_symtree(*root, n);     
    
  strcpy(st.name, n);   
  g95_delete_bbt(root, &st, g95_compare_symtree);    
    
  g95_free(st0);      
}         
         
         
   
   
/* g95_check_pointer_assign()-- Check that a pointer assignment is OK.
 * We first check lvalue, and we only check rvalue if it's not an
 * assignment to NULL() or a NULLIFY statement. */   
   
try g95_check_pointer_assign(g95_expr *lvalue, g95_expr *rvalue) {          
symbol_attribute a;   
int is_pure;   
   
  if (lvalue->symbol->ts.type == BT_UNKNOWN) {      
    g95_error("Pointer assignment target is not a POINTER at %L",     
	      &lvalue->where);       
    return FAILURE;         
  }      
      
  a = g95_variable_attr(lvalue, NULL); 
  if (!a.pointer) {
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
          
    a = g95_expr_attr(rvalue);      
    if (!a.target && !a.pointer) {         
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
        
        
   
   
/* g95_define_st_label()-- Called when a statement with a statement
 * label is about to be accepted.  We add the label to the list of the
 * current namespace, making sure it hasn't been defined previously
 * and referenced correctly. */       
       
void g95_define_st_label(g95_st_label *lp, g95_sl_type t,     
                         g95_locus *label_locus) {       
int labelno; 
 
  labelno = lp->value;         
         
  if (lp->defined != ST_LABEL_UNKNOWN)  
    g95_error("Duplicate statement label %d at %L and %L", labelno,          
	      &lp->where, label_locus);   
  else {          
    lp->where = *label_locus;         
         
    switch(t) {       
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
   
   
          
          
/* g95_set_default_type()-- Given a pointer to a symbol, set its type
 * according to the first letter of its name.  Fails if the letter in
 * question has no default type. */  
  
try g95_set_default_type(g95_symbol *sy, int error_flag, g95_namespace *n) {         
g95_typespec *typesp;    
    
  if (sy->ts.type != BT_UNKNOWN)  
    g95_internal_error("g95_set_default_type(): symbol already has a type");         
         
  typesp = g95_get_default_type(sy, n);      
      
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
   
   
 
 
/* g95_find_symtree()-- Given a root symtree node and a name, try to
 * find the symbol within the namespace.  Returns NULL if the symbol
 * is not found. */      
      
g95_symtree *g95_find_symtree(g95_symtree *sta, char *n) {    
int f;

  while(sta != NULL) {         
    f = strcmp(n, sta->name);
    if (f == 0) return sta;        
        
    sta = (f < 0) ? sta->left : sta->right;   
  }        
        
  return NULL; 
} 
 
 
        
        
/* traverse_symtree()-- Recursively traverse the symtree nodes. */         
         
static void traverse_symtree(g95_symtree *sta, void (*func)(g95_symtree *)) {          
          
  if (sta != NULL) {      
    (*func)(sta);          
          
    traverse_symtree(sta->left, func);  
    traverse_symtree(sta->right, func);    
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
    
    
    
    
/* save_commons()-- Mark common blocks as saved */  
  
static void save_commons(g95_symtree *st0) {  
  
  if (st0 == NULL) return;        
        
  save_commons(st0->left); 
  save_commons(st0->right);     
     
  st0->n.common->saved = 1;         
}        
        
        
  
  
/* check_used()-- Common subroutine called by attribute changing
 * subroutines in order to prevent them from changing a symbol that
 * has been use-associated.  Returns zero if it is OK to change the
 * symbol, nonzero if not. */   
   
static int check_used(symbol_attribute *attribute, g95_locus *where) { 
 
  if (attribute->use_assoc == 0) return 0;   
   
  if (where == NULL) where = &g95_current_locus;       
       
  g95_error("Cannot change attributes of USE-associated symbol at %L", where);    
    
  return 1;
} 
 
 
      
      
/* duplicate_attr()-- Generate an error because of a duplicate attribute */   
   
static void duplicate_attr(char *a, g95_locus *where) {   
   
  if (where == NULL) where = &g95_current_locus;  
  
  g95_error("Duplicate %s attribute specified at %L", a, where);    
}        
        
        
   
   
/* g95_save_all()-- Mark those symbols which can be SAVEd as such. */      
      
void g95_save_all(g95_namespace *name) {       
       
  g95_traverse_ns(name, save_symbol);         
         
  save_commons(name->common_root);       
}       
       
       
       
       
/* g95_check_conformance()-- Given two expressions, make sure that
 * the arrays are conformable. */         
         
try g95_check_conformance(const char *optype, g95_expr *op1, g95_expr *op0) {
int op1_flag, op2_flag, x;       
mpz_t op1_size, op2_size; 
try h;          
          
  if (op1->rank == 0 || op0->rank == 0) return SUCCESS;   
   
  if (op1->rank != op0->rank) {   
    g95_error("Incompatible ranks in %s at %L", optype, &op1->where);    
    return FAILURE;       
  }     
     
  h = SUCCESS;   
   
  for(x=0; x<op1->rank; x++) {       
    op1_flag = g95_array_dimen_size(op1, x, &op1_size) == SUCCESS;     
    op2_flag = g95_array_dimen_size(op0, x, &op2_size) == SUCCESS;  
      
    if (op1_flag && op2_flag && mpz_cmp(op1_size, op2_size) != 0) { 
      g95_error("%s at %L has different shape on dimension %d (%d/%d)",   
		optype, &op1->where, x+1, (int) mpz_get_si(op1_size),          
		(int) mpz_get_si(op2_size)); 
 
      h = FAILURE;         
    }      
      
    if (op1_flag) mpz_clear(op1_size);          
    if (op2_flag) mpz_clear(op2_size);         
         
    if (h == FAILURE) return FAILURE;        
  }          
          
  return SUCCESS;   
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
g95_locus cur_loc; 
g95_typespec t;        
int e, f;         
match r;   
   
  for(f=0; f<G95_LETTERS; f++)         
    g95_clear_ts(&new_ts[f]);  
  
  if (g95_match_eos() == MATCH_YES) { 
    g95_error("Empty IMPLICIT statement at %C");      
    return MATCH_ERROR;        
  }         
         
  do {    
    r = g95_match_type_spec(&t, 0); /* A basic type is mandatory here */ 
    if (r == MATCH_ERROR) goto error;   
    if (r == MATCH_NO) goto syntax;          
          
    cur_loc = g95_current_locus;    
    r = match_implicit_range();          
          
    if (r == MATCH_YES) {   /* Looks like we have the <TYPE> (<RANGE>) */   
      g95_gobble_whitespace(); 
      e = g95_next_char();
      if (e == '\n' || e == ',') {        
	update_implicit(&t);         
	continue;          
      }       
       
      g95_current_locus = cur_loc;          
    }        
        
    /* Last chance-- check <TYPE> (<KIND>) (<RANGE>) */          
          
    r = g95_match_kind_spec(&t);
    if (r == MATCH_ERROR) goto error;  
    if (r == MATCH_NO) {          
      r = g95_match_old_kind_spec(&t);      
      if (r == MATCH_ERROR) goto error;     
      if (r == MATCH_NO) goto syntax;   
    }  
  
    r = match_implicit_range();       
    if (r == MATCH_ERROR) goto error;  
    if (r == MATCH_NO) goto syntax;          
    update_implicit(&t);       
       
    g95_gobble_whitespace();         
    e = g95_next_char();       
    if ((e != '\n') && (e != ',')) goto syntax;          
          
  } while(e == ',');   
   
  return MATCH_YES;  
  
syntax:   
  g95_syntax_error(ST_IMPLICIT);      
      
error:     
  return MATCH_ERROR;  
} 
 
 
 
 
/* gsym_compare()-- Compare two symbols */   
   
static int gsym_compare(g95_gsymbol *l, g95_gsymbol *c) {       
       
  return strcmp(l->name, c->name);         
} 
 
 
    
/* check_conflict()-- This is a generic conflict-checker.  We do this
 * to avoid having a single conflict in two places. */

#define conf(a, b) \
    if (attr->a && attr->b) { attr1 = a; attr2 = b; goto conflict; }
#define conf2(a) if (attr->a) { attr2 = a; goto conflict; }
         
static try check_conflict(symbol_attribute *attr, g95_locus *where) {   
char *attr1, *attr2;  
  
static char *dummy = "DUMMY", *save = "SAVE", *pointer = "POINTER",       
  *target = "TARGET", *external = "EXTERNAL", *intent = "INTENT",  
  *intrinsic = "INTRINSIC", *allocatable = "ALLOCATABLE",  
  *elemental = "ELEMENTAL", *private = "PRIVATE", *recursive = "RECURSIVE", 
  *in_common = "COMMON", *result_var = "RESULT", *in_namelist = "NAMELIST", 
  *public = "PUBLIC", *optional = "OPTIONAL", *entry = "ENTRY",
  *function = "FUNCTION", *subroutine = "SUBROUTINE", *dimension = "DIMENSION",      
  *use_assoc = "USE";   
   
  if (where == NULL) where = &g95_current_locus;        
        
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
         
         


try g95_add_sequence(symbol_attribute *attribute, g95_locus *where) {    
    
  if (check_used(attribute, where)) return FAILURE;    
    
  attribute->sequence = 1;
  return check_conflict(attribute, where);    
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
     
     
      
      
/* switch_types()-- Recursive function to switch derived types of all
 * symbol in a namespace. */    
    
static void switch_types(g95_symtree *st0, g95_symbol *frm, g95_symbol *to) { 
g95_symbol *symb;   
   
  if (st0 == NULL) return;     
     
  symb = st0->n.sym;  
  if (symb->ts.type == BT_DERIVED && symb->ts.derived == frm)  
    symb->ts.derived = to;     
     
  switch_types(st0->left, frm, to);   
  switch_types(st0->right, frm, to);
}        
        
        
      
      
try g95_add_function(symbol_attribute *attribute, g95_locus *where) {       
       
  if (attribute->flavor != FL_PROCEDURE && 
      g95_add_flavor(attribute, FL_PROCEDURE, where) == FAILURE) return FAILURE;     
     
  attribute->function = 1; 
  return check_conflict(attribute, where);        
}          
          
          
         
         
try g95_add_subroutine(symbol_attribute *atr, g95_locus *where) {

  if (atr->flavor != FL_PROCEDURE &&       
      g95_add_flavor(atr, FL_PROCEDURE, where) == FAILURE) return FAILURE;

  atr->subroutine = 1;  
  return check_conflict(atr, where); 
}       
       
       


try g95_add_dummy(symbol_attribute *a, g95_locus *where) { 
 
  if (check_used(a, where)) return FAILURE;      
      
  /* Duplicate dummy arguments are allow due to ENTRY statements */    
    
  a->dummy = 1;      
  return check_conflict(a, where); 
}  
  
  
      
      
/* g95_new_symtree()-- Allocate a new symtree node and associate it
 * with the new symbol. */      
      
g95_symtree *g95_new_symtree(g95_symtree **root, char *nam) {
g95_symtree *s;          
          
  s = g95_getmem(sizeof(g95_symtree));         
  strcpy(s->name, nam);  
  
  g95_insert_bbt(root, s, g95_compare_symtree); 
  return s;
}    
    
    
 
 
/* g95_get_gsymbol()-- Get a global symbol, creating it if it doesn't
 * exist. */

g95_gsymbol *g95_get_gsymbol(char *name) {        
g95_gsymbol *a;     
     
  a = g95_find_gsymbol(g95_gsym_root, name);   
  if (a != NULL) return a;

  a = g95_getmem(sizeof(g95_gsymbol)); 
  a->type = GSYM_UNKNOWN;       
  strcpy(a->name, name);      
      
  g95_insert_bbt(&g95_gsym_root, a, gsym_compare);         
         
  return a;  
}          
          
          
       
       
try g95_add_dimension(symbol_attribute *a, g95_locus *where) {   
   
  if (check_used(a, where)) return FAILURE;       
       
  if (a->dimension) {
    duplicate_attr("DIMENSION", where);       
    return FAILURE;  
  }      
      
  a->dimension = 1;     
  return check_conflict(a, where);     
}        
        
        
         
         
/* g95_compare_symtree()-- Comparison function for symtree nodes. */        
        
int g95_compare_symtree(g95_symtree *st1, g95_symtree *st2) {         
         
  return strcmp(st1->name, st2->name);    
}


     
     
/* g95_get_ha_symbol()-- Subroutine that searches for a symbol,
 * creating it if it doesn't exist, but tries to host-associate the
 * symbol if possible. */    
    
int g95_get_ha_symbol(char *name0, g95_symbol **res) { 
g95_symbol *symb;
int r;     
     
  r = g95_find_symbol(name0, g95_current_ns, 0, &symb);     
  if (symb != NULL) { 
    g95_save_symbol_data(symb);    
    
    *res = symb;    
    return r;
  }    
    
  if (g95_current_ns->parent != NULL) {          
    r = g95_find_symbol(name0, g95_current_ns->parent, 1, &symb);        
    if (r) return r;     
     
    if (symb != NULL) {          
      *res = symb;         
      return 0;    
    }         
  }

  return g95_get_symbol(name0, g95_current_ns, res);       
}   
   
   
    
    
try g95_add_intent(symbol_attribute *a, sym_intent intent,   
		   g95_locus *where) {          
          
  if (check_used(a, where)) return FAILURE;          
          
  if (a->intent == INTENT_UNKNOWN) {     
    a->intent = intent;        
    return check_conflict(a, where); 
  }         
         
  if (where == NULL) where = &g95_current_locus;   
   
  g95_error("INTENT (%s) conflicts with INTENT(%s) at %L",  
	    g95_intent_string(a->intent),          
	    g95_intent_string(intent), where);

  return FAILURE;          
}      
      
      
    
    
/* g95_copy_attr()-- copy an attribute to a symbol attribute, bit by
 * bit.  Some attributes have a lot of side-effects but cannot be
 * present given where we are called from, so we ignore some bits */ 
 
try g95_copy_attr(symbol_attribute *des, symbol_attribute *s1, 
		  g95_locus *where) {      
      
  if (s1->allocatable && g95_add_allocatable(des, where) == FAILURE)
    goto fail;        
        
  if (s1->dimension && g95_add_dimension(des, where) == FAILURE) goto fail;         
  if (s1->optional && g95_add_optional(des, where) == FAILURE) goto fail; 
  if (s1->pointer && g95_add_pointer(des, where) == FAILURE) goto fail;        
  if (s1->save && g95_add_save(des, where) == FAILURE) goto fail; 
  if (s1->target && g95_add_target(des, where) == FAILURE) goto fail;      
  if (s1->dummy && g95_add_dummy(des, where) == FAILURE) goto fail;        
  if (s1->result_var && g95_add_result(des, where) == FAILURE) goto fail;  
  if (s1->entry) des->entry = 1;          
          
  if (s1->in_namelist && g95_add_in_namelist(des, where) == FAILURE)        
    goto fail;      
      
  if (s1->in_common && g95_add_in_common(des, where) == FAILURE) goto fail;   
  if (s1->generic && g95_add_generic(des, where) == FAILURE) goto fail;      
  if (s1->function && g95_add_function(des, where) == FAILURE) goto fail;       
  if (s1->subroutine && g95_add_subroutine(des, where) == FAILURE) goto fail;        
        
  if (s1->sequence && g95_add_sequence(des, where) == FAILURE) goto fail;  
  if (s1->elemental && g95_add_elemental(des, where) == FAILURE) goto fail;        
  if (s1->pure && g95_add_pure(des, where) == FAILURE) goto fail;       
  if (s1->recursive && g95_add_recursive(des, where) == FAILURE) goto fail;

  if (s1->flavor != FL_UNKNOWN && 
      g95_add_flavor(des, s1->flavor, where) == FAILURE) goto fail;      
      
  if (s1->intent != INTENT_UNKNOWN && 
      g95_add_intent(des, s1->intent, where) == FAILURE) goto fail;    
    
  if (s1->access != ACCESS_UNKNOWN &&          
      g95_add_access(des, s1->access, where) == FAILURE) goto fail;    
    
/* The subroutines that set these bits also cause flavors to be set,
 * and that has already happened in the original, so don't let to
 * happen again. */  
  
  if (s1->external) des->external = 1;  
  if (s1->intrinsic) des->intrinsic = 1;          
          
  return SUCCESS;

fail:        
  return FAILURE;    
}      
      
      
  
/******************** Statement label management ********************/       
       
/* Free a single g95_st_label structure, making sure the list is not
 * messed up.  This function is called only when some parse error
 * occurs. */  
  
void g95_free_st_label(g95_st_label *f) {   
   
  if (f == NULL) return;   
   
  if (f->prev)
    (f->prev->next = f->next);
 
  if (f->next)      
    (f->next->prev = f->prev);        
        
  if (f->format != NULL) g95_free_expr(f->format);    
  g95_free(f);     
}   
   
   
  
  
void g95_symbol_init_2(void) {     
     
  g95_current_ns = g95_get_namespace(NULL, 0);    
}       
       
       
         
         
/* g95_get_componentr_attr()-- Get a standard symbol attribute
 * structure given the component structure. */        
        
void g95_get_component_attr(symbol_attribute *attr, g95_component *h) {  
  
  g95_clear_attr(attr);   
  attr->dimension = h->dimension;       
  attr->pointer = h->pointer;
}        
        
        
       
       
void g95_show_components(g95_symbol *s) {        
g95_component *q;        
        
  for(q=s->components; q; q=q->next) {  
    g95_status("(%s ", q->name);        
    g95_show_typespec(&q->ts);       
    if (q->pointer) g95_status(" POINTER");    
    if (q->dimension) g95_status(" DIMENSION");  
    g95_status_char(' ');
    g95_show_array_spec(q->as);      
    g95_status(")");          
    if (q->next != NULL) g95_status_char(' ');   
  }        
}  
  
  
         
         
try g95_add_procedure(symbol_attribute *atr, procedure_type r,       
		      g95_locus *where) {   
   
  if (check_used(atr, where)) return FAILURE;        
        
  if (atr->flavor != FL_PROCEDURE &&      
      g95_add_flavor(atr, FL_PROCEDURE, where) == FAILURE) return FAILURE;         
         
  if (where == NULL) where = &g95_current_locus;         
         
  if (atr->proc != PROC_UNKNOWN) { 
    g95_error("%s procedure at %L is already %s %s procedure",         
	      g95_code2string(procedures, r), where, 
	      g95_article(g95_code2string(procedures, atr->proc)),
	      g95_code2string(procedures, atr->proc));        
        
    return FAILURE;  
  } 
 
  atr->proc = r;    
    
/* Statement functions are always scalar and functions */       
       
  if (r == PROC_ST_FUNCTION &&    
      ((!atr->function && g95_add_function(atr, where) == FAILURE)          
       || atr->dimension)) return FAILURE;

  return check_conflict(atr, where);       
}


 
 
/* g95_symbol_state()-- Makes sure that no changes to symbols are pending */ 
 
void g95_symbol_state(void) {     
     
  if (changed_syms != NULL)          
    g95_internal_error("Symbol changes still pending");    
}


 
 
try g95_add_elemental(symbol_attribute *attr, g95_locus *where) { 
 
  if (check_used(attr, where)) return FAILURE; 
 
  attr->elemental = 1;   
  return check_conflict(attr, where);         
}      
      
      
          
          
/* g95_add_flavor()-- Flavors are special because some flavors are not
 * what fortran considers attributes and can be reaffirmed multiple
 * times. */    
    
try g95_add_flavor(symbol_attribute *attr, sym_flavor i, g95_locus *where) {     
     
  if ((i == FL_PROGRAM || i == FL_BLOCK_DATA || i == FL_MODULE || 
       i == FL_PARAMETER || i == FL_LABEL || i == FL_DERIVED ||  
       i == FL_NAMELIST) && check_used(attr, where)) return FAILURE;

  if (attr->flavor == i && i == FL_VARIABLE) return SUCCESS; 
 
  if (attr->flavor != FL_UNKNOWN) {         
    if (where == NULL) where = &g95_current_locus;          
          
    g95_error("%s attribute conflicts with %s attribute at %L",      
	      g95_code2string(flavors, attr->flavor),      
	      g95_code2string(flavors, i), where);    
    
    return FAILURE;    
  }         
         
  attr->flavor = i;  
  
  return check_conflict(attr, where);       
}      
      
      
   
   
try g95_add_save(symbol_attribute *attr, g95_locus *where) {      
      
  if (where == NULL) where = &g95_current_locus; 
 
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
  
  
    
    
try g95_add_in_common(symbol_attribute *a, g95_locus *where) {  
  
  if (check_used(a, where)) return FAILURE;         
         
  /* Duplicate attribute already checked for */    
    
  a->in_common = 1;       
  if (check_conflict(a, where) == FAILURE) return FAILURE;         
         
  if (a->flavor == FL_VARIABLE) return SUCCESS;          
          
  return g95_add_flavor(a, FL_VARIABLE, where);
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
    lp->where = g95_current_locus;    
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
         
         
   
   
/* g95_check_assign_symbol()-- Relative of g95_check_assign() except
 * that the lvalue is a single symbol. */     
     
try g95_check_assign_symbol(g95_symbol *symb, g95_expr *rvalue) {  
g95_expr lvalue;  
g95_ref ref;     
     
  memset(&lvalue, '\0', sizeof(g95_expr));      
  memset(&ref, '\0', sizeof(g95_ref));         
         
  lvalue.type = EXPR_VARIABLE;       
  lvalue.ts = symb->ts;     
  if (symb->as) {          
    lvalue.rank = symb->as->rank;   
    lvalue.ref = &ref;     
     
    ref.type = REF_ARRAY;         
    ref.u.ar.type = AR_FULL;     
  }  
  
  lvalue.symbol = symb;         
  lvalue.where = symb->declared_at;         
         
  return g95_check_assign(&lvalue, rvalue, 1);  
}         
         
         
  
  
try g95_add_pure(symbol_attribute *attribute, g95_locus *where) { 
 
  if (check_used(attribute, where)) return FAILURE;     
     
  attribute->pure = 1;        
  return check_conflict(attribute, where);     
}       
       
       
     
     
try g95_add_external(symbol_attribute *attr, g95_locus *where) {      
      
  if (check_used(attr, where)) return FAILURE;         
           
  if (attr->external) {      
    duplicate_attr("EXTERNAL", where);       
    return FAILURE;     
  }         
         
  attr->external = 1;

  return check_conflict(attr, where);  
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
		      g95_component **m) {  
g95_component *x, *tail; 
 
  tail = NULL;  
  
  for(x=symb->components; x; x=x->next) {     
    if (strcmp(x->name, nm) == 0) {          
      g95_error("Component '%s' at %C already declared at %L",
		nm, &x->loc);  
      return FAILURE;   
    }    
    
    tail = x;      
  }

/* Allocate new component */      
      
  x = g95_get_component();    
    
  if (tail == NULL) symb->components = x;
  else tail->next = x;  
  
  strcpy(x->name, nm);  
  x->loc = g95_current_locus;     
     
  *m = x;        
  return SUCCESS;    
}


          
          
try g95_add_result(symbol_attribute *atr, g95_locus *where) {    
    
  if (check_used(atr, where)) return FAILURE;       
       
  atr->result_var = 1;  
  return check_conflict(atr, where); 
}          
          
          
    
    
/* g95_undo_symbols()-- Undoes all the changes made to symbols in the
 * current statement.  This subroutine is made simpler due to the fact
 * that attributes are never removed once added. */        
        
void g95_undo_symbols(void) {     
g95_symbol *c, *r, *o;       
g95_symtree *b, *t; 
 
/* if (changed_syms != NULL) g95_status("Undoing symbols\n"); */   
   
  for(c=changed_syms; c; c=r) {
    r = c->tlink;  
    /* g95_status("Undoing %s\n", p->name); */          
          
    if (c->new) {  /* Symbol was new */
      delete_symtree(&c->ns->sym_root, c->name);        
        
      c->refs--;         
      if (c->refs < 0) g95_internal_error("g95_undo_symbols(): Negative refs");      
      if (c->refs == 0) g95_free_symbol(c);   
      continue;   
    }    
    
/* Restore previous state of symbol.  Just copy simple stuff */    
    
    c->mark = 0;
    o = c->old_symbol;      
      
    c->ts.type = o->ts.type;         
    c->ts.kind = o->ts.kind;      
      
    c->attr = o->attr;       
       
    if (c->value != o->value) {     
      g95_free_expr(o->value);    
      c->value = NULL;       
    }        
        
    if (c->as != o->as) {       
      if (c->as) g95_free_array_spec(c->as);     
      c->as = o->as;         
    } 
 
    c->generic = o->generic;  
    c->component_access = o->component_access;

    if (c->namelist != NULL && o->namelist == NULL) {         
      g95_free_namelist(c->namelist);        
      c->namelist = NULL;     
    } else {   
   
      if (c->namelist_tail != o->namelist_tail) {    
	g95_free_namelist(o->namelist_tail);  
	o->namelist_tail->next = NULL; 
      }   
    }

    c->namelist_tail = o->namelist_tail;         
         
    if (c->formal != o->formal) {        
      g95_free_formal_arglist(c->formal);      
      c->formal = o->formal;   
    }       
       
    g95_free(c->old_symbol);    
    c->old_symbol = NULL;    
    c->tlink = NULL;    
  } 
 
  changed_syms = NULL;          
          
  /* Unlink host associated symtrees */        
        
  for(b=changed_st; b; b=t) {      
    t = b->link;  
  
    g95_delete_bbt(&g95_current_ns->sym_root, b, g95_compare_symtree);          
    g95_free(b);     
  } 
 
  changed_st = NULL;       
}       
       
       
        
        
try g95_add_intrinsic(symbol_attribute *attribute, g95_locus *where) {          
          
  if (check_used(attribute, where)) return FAILURE; 
 
  if (attribute->intrinsic) {     
    duplicate_attr("INTRINSIC", where);  
    return FAILURE; 
  }

  attribute->intrinsic = 1;     
     
  return check_conflict(attribute, where);     
}     
     
     
 
 
/* free_common_tree()-- Recursively free a list of common head nodes */

static void free_common_tree(g95_symtree *sta) {   
   
  if (sta == NULL) return;   
   
  free_common_tree(sta->left);     
  free_common_tree(sta->right);   
   
  g95_free(sta->n.common);  
  g95_free(sta);      
}    
    
    
      
      
/* g95_set_component_attr()-- Set component attributes from a standard
 * symbol attribute structure. */       
       
void g95_set_component_attr(g95_component *i, symbol_attribute *attr) {     
     
  i->dimension = attr->dimension;        
  i->pointer = attr->pointer;    
}      
      
      
      
      
/* free_sym_tree()-- Recursive function that deletes an entire
 * red-black tree and all the symbols that it contains. */        
        
static void free_sym_tree(g95_symtree *rb) {         
g95_namespace *name;          
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
    name = s->formal_ns;         
    s->formal_ns = NULL;  
    g95_free_namespace(name);   
  } else if (s->refs == 0) {   /* Go ahead and delete the symbol */        
    g95_free_symbol(s);        
  }      
      
  g95_free(rb);     
}


 
 
/* ambiguous_symbol()-- Generate an error if a symbol is ambiguous. */          
          
static void ambiguous_symbol(char *nam, g95_symtree *st1) {     
     
  if (st1->n.sym->module[0])
    g95_error("Name '%s' at %C is an ambiguous reference to '%s' "     
	      "from module '%s'", nam, st1->n.sym->name,      
	      st1->n.sym->module);        
  else        
    g95_error("Name '%s' at %C is an ambiguous reference to '%s' "         
	      "from current program unit", nam, st1->n.sym->name);
}       
       
       
        
        
/* g95_find_symbol()-- search for a symbol starting in the current
 * namespace, resorting to any parent namespaces if requested by a
 * nonzero parent_flag.  Returns nonzero if the symbol is ambiguous.
 * If a symbol that is found is changed, then g95_save_symbol() data
 * must be called, since generally we return symbols that are not
 * subsequently changed. */     
     
int g95_find_symbol(char *name, g95_namespace *namesp, int parent_flag,
		    g95_symbol **rslt) {  
g95_symtree *s;       
       
  if (namesp == NULL) namesp = g95_current_ns;          
          
  do {
    s = g95_find_symtree(namesp->sym_root, name);      
    if (s != NULL) {         
      *rslt = s->n.sym;     
      if (s->ambiguous) {   
	ambiguous_symbol(name, s);        
	return 1;    
      }         
         
      return 0;        
    }      
      
    if (!parent_flag) break;

    namesp = namesp->parent;      
  } while (namesp != NULL);         
         
  *rslt = NULL;     
  return 0;    
}          
          
          


/* g95_find_uop()-- Given a name find the user operator node.  Returns
 * NULL if it does not exist. */     
     
g95_user_op *g95_find_uop(char *name, g95_namespace *ns) {         
g95_symtree *sta;  
  
  if (ns == NULL) ns = g95_current_ns;

  sta = g95_find_symtree(ns->uop_root, name);      
  return (sta == NULL) ? NULL : sta->n.uop;       
}          
          
          
          
/* No checks for use-association in public and private statements */  
  
try g95_add_access(symbol_attribute *atr, g95_access access,       
		   g95_locus *where) {         
         
  if (atr->access == ACCESS_UNKNOWN) {        
    atr->access = access; 
    return check_conflict(atr, where);       
  }  
  
  if (where == NULL) where = &g95_current_locus;       
  g95_error("ACCESS specification at %L was already specified", where);    
    
  return FAILURE;        
}        
        
        
      
      
static void show_indent(void) {      
int s;  
  
  g95_status_char('\n');
  for(s=0; s<2*show_level; s++)       
    g95_status_char(' ');         
}


       
       
try g95_add_allocatable(symbol_attribute *attribute, g95_locus *where) {         
         
  if (check_used(attribute, where)) return FAILURE;       
       
  if (attribute->allocatable) {   
    duplicate_attr("ALLOCATABLE", where); 
    return FAILURE;    
  }         
         
  attribute->allocatable = 1; 
  return check_conflict(attribute, where);   
}      
      
      
     
/* free_st_labels()-- Free a whole list of g95_st_label structures.  */   
   
static void free_st_labels(g95_st_label *j) {        
g95_st_label *f;

  for(; j; j=f) {    
    f = j->next;
    if (j->format != NULL) g95_free_expr(j->format);        
    g95_free(j); 
  }    
}          
          
          
    
    
/* g95_show_symbol()-- Show a symbol.  If a symbol is an ENTRY,
 * SUBROUTINE or FUNCTION, we show the interface.  Information needed
 * to reconstruct the list of specific interfaces associated with a
 * generic symbol is done within that symbol. */

void g95_show_symbol(g95_symbol *symbol) {         
g95_formal_arglist *f;   
g95_interface *i;   
   
  if (symbol == NULL) return;        
        
  show_indent();         
         
  g95_status("symbol %s ", symbol->name);
  g95_show_typespec(&symbol->ts);
  g95_show_attr(&symbol->attr);   
   
  if (symbol->value) {     
    show_indent();          
    g95_status("value: ");          
    g95_show_expr(symbol->value);  
  }    
    
  if (symbol->as) { 
    show_indent();       
    g95_status("Array spec:"); 
    g95_show_array_spec(symbol->as);         
  }         
         
  if (symbol->generic) {        
    show_indent(); 
    g95_status("Generic interfaces:");      
    for(i=symbol->generic; i; i=i->next)      
      g95_status(" %s", i->sym->name); 
  }    
    
  if (symbol->result) {       
    show_indent();       
    g95_status("result: %s", symbol->result->name);    
  }    
    
  if (symbol->components) { 
    show_indent();
    g95_status("components: ");      
    g95_show_components(symbol);  
  }   
   
  if (symbol->formal) {        
    show_indent();
    g95_status("Formal arglist:");  
  
    for(f=symbol->formal; f; f=f->next)      
      g95_status(" %s", f->sym->name);     
  }        
        
  if (symbol->formal_ns) {      
    show_indent();   
    g95_status("Formal namespace");        
    g95_show_namespace(symbol->formal_ns);          
  }   
   
  g95_status_char('\n');     
} 
 
 
        
        
/* clear_sym_mark()-- Clear mark bits from symbol nodes associated
 * with a symtree node */     
     
static void clear_sym_mark(g95_symtree *sta) {     
     
  sta->n.sym->mark = 0;    
}   
   
   
       
       
/* g95_free_symbol()-- Remove a g95_symbol structure and everything it
 * points to. */          
          
void g95_free_symbol(g95_symbol *symbol) {      
      
  if (symbol == NULL) return;

  g95_free_array_spec(symbol->as);         
         
  free_components(symbol->components);

  g95_free_expr(symbol->value);         
         
  g95_free_namelist(symbol->namelist);  
  
  g95_free_namespace(symbol->formal_ns);       
       
  g95_free_interface(symbol->generic);     
     
  g95_free_formal_arglist(symbol->formal);     
     
  g95_free(symbol);     
}          
          
          
   
   
/* g95_add_type()-- Add a type to a symbol. */

try g95_add_type(g95_symbol *symb, g95_typespec *ts, g95_locus *where) {  
sym_flavor flavor;    
    
  if (where == NULL) where = &g95_current_locus;    
    
  if (symb->ts.type != BT_UNKNOWN) {       
    g95_error("Symbol '%s' at %L already has basic type of %s", symb->name, 
	      where, g95_basic_typename(symb->ts.type));     
    return FAILURE;         
  }      
      
  flavor = symb->attr.flavor;     
     
  if (flavor == FL_PROGRAM || flavor == FL_BLOCK_DATA || flavor == FL_MODULE ||
      flavor == FL_LABEL || (flavor == FL_PROCEDURE && symb->attr.subroutine) ||
      flavor == FL_DERIVED || flavor == FL_NAMELIST) {        
    g95_error("Symbol '%s' at %L cannot have a type", symb->name, where);       
    return FAILURE;    
  }   
   
  symb->ts = *ts;     
  return SUCCESS;    
}  
  
  
       
       
try g95_add_target(symbol_attribute *attribute, g95_locus *where) { 
 
  if (check_used(attribute, where)) return FAILURE;        
        
  if (attribute->target) {      
    duplicate_attr("TARGET", where);         
    return FAILURE;    
  }   
   
  attribute->target = 1;         
  return check_conflict(attribute, where);    
}     
     
     
    
    
void g95_traverse_symtree(g95_namespace *name, void (*func)(g95_symtree *)) {         
         
  traverse_symtree(name->sym_root, func); 
}  
  
  
   
   
/* g95_global_used()-- Come here to complain about a global symbol
 * already in use as something else. */         
         
void g95_global_used(g95_gsymbol *symb, g95_locus *where) {          
char *name;   
   
  if (where == NULL) where = &g95_current_locus;

  switch(symb->type) { 
  case GSYM_PROGRAM:      name = "PROGRAM";     break;      
  case GSYM_FUNCTION:     name = "FUNCTION";    break;     
  case GSYM_SUBROUTINE:   name = "SUBROUTINE";  break;   
  case GSYM_COMMON:       name = "COMMON";      break; 
  case GSYM_BLOCK_DATA:   name = "BLOCK DATA";  break; 
  case GSYM_MODULE:       name = "MODULE";      break;         
  default:  
    g95_internal_error("g95_gsymbol_type(): Bad type");          
    name = NULL;     
  }     
     
  g95_error("Global name '%s' at %L is already being used as a %s at %L",        
	    g95_new_block->name, where, name, &symb->where);        
} 
       
       
try g95_add_entry(symbol_attribute *attr, g95_locus *where) {      
      
  if (check_used(attr, where)) return FAILURE;     
     
  if (attr->entry) {  
    duplicate_attr("ENTRY", where);    
    return FAILURE;
  }          
          
  attr->entry = 1;          
  return check_conflict(attr, where);       
}


 
 
/* g95_get_namespace()-- Allocate a new namespace structure. */      
      
g95_namespace *g95_get_namespace(g95_namespace *parent, int parent_types) {       
g95_namespace *names;        
g95_typespec *typesp;       
int v;     
      
  names = g95_getmem(sizeof(g95_namespace));     
  names->sym_root = NULL;   
  names->uop_root = NULL;          
  names->default_access = ACCESS_UNKNOWN; 
  names->parent = parent;    
    
  for(v=0; v<G95_INTRINSIC_OPS; v++)          
    names->operator_access[v] = ACCESS_UNKNOWN;  
  
/* Initialize default types */

  for(v='a'; v<='z'; v++) {     
    names->set_flag[v - 'a'] = 0;     
    typesp = &names->default_type[v - 'a'];     
     
    if (parent_types && names->parent != NULL) {    /* Copy parent settings */  
      *typesp = names->parent->default_type[v - 'a'];       
      continue;    
    }   
   
    if (g95_option.implicit_none != 0) {   
      g95_clear_ts(typesp);      
      continue;
    }    
    
    if ('i' <= v && v <= 'n') {  
      typesp->type = BT_INTEGER;
      typesp->kind = g95_default_integer_kind();         
    } else {        
      typesp->type = BT_REAL;        
      typesp->kind = g95_default_real_kind();   
    }      
  }      
      
  return names;
} 
 
 
       
       
/* g95_free_namespace()-- Free a namespace structure and everything
 * below it.  Interface lists associated with intrinsic operators are
 * not freed.  These are taken care of when a specific name is freed. */    
    
void g95_free_namespace(g95_namespace *n) {          
g95_charlen *cl, *cl2;       
g95_namespace *s, *w;  
int l;     
     
  if (n == NULL) return; 
 
  g95_free_statements(n->code); 
 
  free_sym_tree(n->sym_root);          
  free_uop_tree(n->uop_root);      
  free_common_tree(n->common_root);       
       
  for(cl=n->cl_list; cl; cl=cl2) {      
    cl2 = cl->next;   
    g95_free_expr(cl->length);    
    g95_free(cl);   
  }      
      
  free_st_labels(n->st_labels);         
         
  g95_free_equiv(n->equiv);          
          
  for(l=0; l<G95_INTRINSIC_OPS; l++)     
    g95_free_interface(n->operator[l]);          
          
  g95_free_data(n->data);         
  s = n->contained;  
  g95_free(n);  
  
  /* Recursively free any contained namespaces */       
       
  while(s != NULL) {  
    w = s;    
    s = s->sibling;    
    
    g95_free_namespace(w);     
  }       
}        
        
        
  
  
try g95_add_generic(symbol_attribute *atr, g95_locus *where) { 
 
  if (atr->flavor != FL_PROCEDURE &&        
      g95_add_flavor(atr, FL_PROCEDURE, where) == FAILURE) return FAILURE;  
  
  atr->generic = 1;          
  return check_conflict(atr, where);   
}   
   
   
   
   
/* g95_compare_attr()-- Compares two attributes */         
         
int g95_compare_attr(symbol_attribute *u, symbol_attribute *h) {          
          
  return u->allocatable == h->allocatable &&        
    u->dimension == h->dimension     && u->external == h->external &&  
    u->intrinsic == h->intrinsic     && u->optional == h->optional &&         
    u->pointer == h->pointer         && u->save == h->save &&   
    u->target == h->target           && u->dummy == h->dummy &&       
    u->result_var == h->result_var   && u->entry == h->entry &&       
    u->data == h->data               && u->use_assoc == h->use_assoc &&  
    u->in_namelist == h->in_namelist && u->in_common == h->in_common &&     
    u->function == h->function       && u->subroutine == h->subroutine &&         
    u->sequence == h->sequence       && u->elemental == h->elemental &&          
    u->pure == h->pure               && u->recursive == h->recursive &&     
    u->access == h->access           && u->intent == h->intent &&
    u->flavor == h->flavor           && u->proc == h->proc && 
    u->generic == h->generic;       
}


       
       
try g95_add_optional(symbol_attribute *attribute, g95_locus *where) {         
         
  if (check_used(attribute, where)) return FAILURE; 
 
  if (attribute->optional) {    
    duplicate_attr("OPTIONAL", where); 
    return FAILURE;   
  }    
    
  attribute->optional = 1; 
  return check_conflict(attribute, where);
}   
   
   
   
   
/* traverse_ns()-- Recursive namespace traversal function. */    
    
static void traverse_ns(g95_symtree *s, void (*func)(g95_symbol *)) {      
      
  if (s == NULL) return; 
 
  if (s->n.sym->mark == 0) (*func)(s->n.sym);       
  s->n.sym->mark = 1;

  traverse_ns(s->left, func);
  traverse_ns(s->right, func);       
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
         
int g95_get_symbol(char *nam, g95_namespace *ns, g95_symbol **r) {          
g95_symtree *st0;  
g95_symbol *b;      
      
  /* This doesn't usually happen during resolution.  */ 
  if (ns == NULL) ns = g95_current_ns;     
     
  /* Try to find the symbol. */     
  st0 = g95_find_symtree(ns->sym_root, nam);   
   
  if (st0 == NULL) {         /* If not there, create a new symbol */  
    b = g95_new_symbol(nam, ns);      
     
    b->old_symbol = NULL;   /* Add to the list of tentative symbols. */  
    b->tlink = changed_syms;          
    b->mark = 1;    
    b->new = 1;         
    changed_syms = b; 
 
    st0 = g95_new_symtree(&ns->sym_root, nam);         
    st0->n.sym = b;          
    b->refs++;          
          
  } else {    /* Make sure the existing symbol is OK */      
    if (st0->ambiguous) {          
      ambiguous_symbol(nam, st0);   
      return 1;         
    }     
     
    b = st0->n.sym;          
          
    if (b->ns != ns && (!b->attr.function || ns->proc_name != b) &&  
	!b->attr.entry) {        
      /* Symbol is from another namespace */  
      g95_error("Symbol '%s' at %C has already been host associated", nam);         
      return 2;      
    }      
      
    b->mark = 1;   
   
    g95_save_symbol_data(b);      /* Copy in case this symbol is changed */ 
  }   
   
  *r = b;  
  return 0;  
}         
         
         
          
          
/* g95_commit_symbols()-- Makes the changes made in the current
 * statement permanent-- gets rid of undo information. */          
          
void g95_commit_symbols(void) {   
g95_symbol *r, *y;      
g95_symtree *a, *e;          
          
#if 0
  if (changed_syms != NULL) g95_status("Committing symbols\n");  
#endif
  
  for(r=changed_syms; r; r=y) {        
    y = r->tlink;       
    r->tlink = NULL;    
    r->mark = 0;          
    r->new = 0;  
  
    if (r->old_symbol != NULL) {   
      g95_free(r->old_symbol);      
      r->old_symbol = NULL;          
    }     
  }        
        
  changed_syms = NULL;         
         
  for(a=changed_st; a; a=e) {     
    e = a->link;       
    a->link = NULL; 
  }    
    
  changed_st = NULL;   
}      
      
      
     
     
/* show_symtree()-- Worker function to display the symbol tree */      
      
static void show_symtree(g95_symtree *s) {        
        
  show_indent();
  g95_status("symtree: %s  Ambig %d", s->name, s->ambiguous);   
   
  if (s->n.sym->ns != g95_current_ns)      
    g95_status(" from namespace %s", s->n.sym->ns->proc_name->name);   
  else         
    g95_show_symbol(s->n.sym);    
} 
 
 
        
        
try g95_add_explicit_interface(g95_symbol *symbol, ifsrc source,    
			       g95_formal_arglist *formal, g95_locus *where) {          
          
  if (check_used(&symbol->attr, where)) return FAILURE;          
          
  if (where == NULL) where = &g95_current_locus;     
     
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
 
 
   
   
/* traverse_uop()-- Function for traversing the user operator symtree */    
    
static void traverse_uop(g95_symtree *st, void (*func)(g95_user_op *)) { 
 
  if (st == NULL) return;         
         
  (*func)(st->n.uop);       
       
  traverse_uop(st->left, func);         
  traverse_uop(st->right, func);      
} 
 
 
      
      
/* g95_save_symbol_data()-- Save symbol with the information necessary
 * to back it out. */  
  
void g95_save_symbol_data(g95_symbol *symb) {  
  
  if (symb->new || symb->old_symbol != NULL) return;      
      
  symb->old_symbol = g95_getmem(sizeof(g95_symbol));       
  *(symb->old_symbol) = *symb;        
        
  symb->tlink = changed_syms;   
  changed_syms = symb;  
}      
      
      
          
          
/* g95_traverse_user_op()-- Traverse the tree of user operator nodes.  */ 
 
void g95_traverse_user_op(g95_namespace *name, void (*func)(g95_user_op *)) {         
         
  traverse_uop(name->uop_root, func);        
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
g95_symbol *u, *r;   
g95_typespec *l;         
g95_symtree *sta; 
int m;          
          
  if (symbol->components != NULL) return symbol;   /* Already defined */       
       
  if (symbol->ns->parent == NULL) goto bad;          
          
  if (g95_find_symbol(symbol->name, symbol->ns->parent, 1, &u)) {
    g95_error("Symbol '%s' at %C is ambiguous", symbol->name);   
    return NULL;      
  }       
       
  if (u == NULL || u->attr.flavor != FL_DERIVED) goto bad;        
        
  /* Get rid of symbol sym, translating all references to s */ 
 
  for(m=0; m<G95_LETTERS; m++) {         
    l = &symbol->ns->default_type[m];   
    if (l->derived == symbol) l->derived = u;         
  }          
          
  sta = g95_find_symtree(symbol->ns->sym_root, symbol->name);     
  sta->n.sym = u;    
    
  u->refs++;          
          
  /* Unlink from list of modified symbols */

  if (changed_syms == symbol)        
    changed_syms = symbol->tlink; 
  else     
    for(r=changed_syms; r; r=r->tlink)  
      if (r->tlink == symbol) {  
	r->tlink = symbol->tlink;    
	break;        
      }       
       
  switch_types(symbol->ns->sym_root, symbol, u);        
        
  /* TODO: Also have to replace sym -> s in other lists like
   * namelists, common lists and interface lists.  */

  g95_free_symbol(symbol);      
      
  return u;         
         
 bad:       
  g95_error("Derived type '%s' at %C is being used before it is defined",      
	    symbol->name);    
  return NULL;
} 
 
 
  
  
try g95_add_recursive(symbol_attribute *a, g95_locus *where) {  
  
  if (check_used(a, where)) return FAILURE;        
        
  a->recursive = 1;          
  return check_conflict(a, where);     
}      
      
      
     
     
static void show_uop(g95_user_op *op) {  
g95_interface *intr;     
     
  show_indent();         
  g95_status("%s:", op->name);   
     
  for(intr=op->operator; intr; intr=intr->next)  
    g95_status(" %s", intr->sym->name);        
}         
         
         
     
     
/* g95_traverse_ns()-- Call a given function for all symbols in the
 * namespace.  We take care that each g95_symbol node is called
 * exactly once. */

void g95_traverse_ns(g95_namespace *names, void (*func)(g95_symbol *)) { 
 
  g95_traverse_symtree(names, clear_sym_mark);        
        
  traverse_ns(names->sym_root, func);        
}    
    
    
 
 
/* g95_show_namespace()-- Show a namespace */     
     
void g95_show_namespace(g95_namespace *ns) { 
g95_interface *inter;  
g95_namespace *save;
int r; 
 
  save = g95_current_ns;  
  show_level++;        
       
  show_indent();        
  g95_status("Namespace:");     
     
  if (ns != NULL) {        
    for(r=0; r<G95_LETTERS; r++) {      
      g95_status(" %c: ", r+'A');
      g95_show_typespec(&ns->default_type[r]);     
    }   
   
    if (ns->proc_name != NULL) {         
      show_indent();   
      g95_status("procedure name = %s", ns->proc_name->name);      
    }    
    
    g95_traverse_symtree(ns, clear_sym_mark);       
       
    g95_current_ns = ns; 
    g95_traverse_symtree(ns, show_symtree);          
          
    for(r=0; r<G95_INTRINSIC_OPS; r++) {    /* User operator interfaces */     
      inter = ns->operator[r];      
      if (inter == NULL) continue;          
          
      show_indent();        
      g95_status("Operator interfaces for %s:", g95_op2string(r));   
   
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
         
         
         
         
try g95_add_pointer(symbol_attribute *attribute, g95_locus *where) {  
  
  if (check_used(attribute, where)) return FAILURE;          
          
  attribute->pointer = 1;    
  return check_conflict(attribute, where);      
}     
     
     
        
        
try g95_add_in_namelist(symbol_attribute *attribute, g95_locus *where) {        
        
  attribute->in_namelist = 1;      
  return check_conflict(attribute, where);          
}          
          
          
