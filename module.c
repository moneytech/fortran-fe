/* Module module
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
  
/* module.c-- Handle modules, which amounts to loading and saving
 * symbols and their attendant structures.  */    
    
/* The syntax of g95 modules resembles that of lisp lists, ie a
 * sequence of atoms, which can be left or right parenthesis, names,
 * integers or strings.  Parenthesis are always matched which allows
 * us to skip over sections at high speed without having to know
 * anything about the internal structure of the lists.  A "name" is
 * usually a fortran 95 identifier, but can also start with '@' in
 * order to reference a hidden symbol.
 *
 * The first line of a module is an informational message about what
 * created the module, the file it came from and when it was created.
 * The second line is a warning for people not to edit the module.
 * The rest of the module looks like:
 *
 * ( ( <Interface info for UPLUS> )
 *   ( <Interface info for UMINUS> )
 *   ...
 * )
 * ( ( <name of operator interface> <module of op interface> <i/f1> ... )
 *   ...
 * )
 * ( ( <name of generic interface> <module of generic interface> <i/f1> ... )
 *   ...
 * )
 * ( <Symbol Number (in no particular order)>
 *   <True name of symbol>
 *   <Module name of symbol>
 *   ( <symbol information> )
 *   ...
 * )
 * ( <Symtree name>
 *   <Ambiguous flag>
 *   <Symbol number>
 *   ...
 * )
 *
 * In general, symbols refer to other symbols by their symbol number,
 * which are zero based.  Symbols are written to the module in no
 * particular order. */        
        
#include <string.h>
#include <stdio.h>
#include <ctype.h>
#include <errno.h>
#include <unistd.h>
#include <time.h>
  
#include "g95.h"

#define MODULE_EXTENSION ".mod"


/* The ifdefs for __GLIBC__ are for debugging on Andy's system and
 * need to be removed someday (perhaps when he upgrades glibc) */     
     
#ifdef __GLIBC__
#include <mcheck.h>
#endif
  
/* Structure that descibes a position within a module file */     
     
typedef struct {  
  int column, line;
  fpos_t pos; 
} module_locus;          
          
          
typedef enum {
  P_UNKNOWN=0, P_OTHER, P_NAMESPACE, P_COMPONENT, P_SYMBOL } pointer_t;          
          
/* The fixup structure lists pointers to pointers that have to be
 * updated when a pointer value becomes known. */     
     
typedef struct fixup_t {      
  char **pointer;          
  struct fixup_t *next; 
} fixup_t;         
         
         
/* Structure for holding extra info needed for pointers being read */

typedef struct pointer_info {      
  BBT_HEADER(pointer_info) 
 
  int integer;
  pointer_t type;          
          
  /* The first component of each member of the union is the pointer
   * being stored */          
          
  fixup_t *fixup;          
          
  union {      
    char *pointer;         /* Member for doing pointer searches */

    struct { 
      g95_symbol *sym;         
      char true_name[G95_MAX_SYMBOL_LEN+1], module[G95_MAX_SYMBOL_LEN+1];    
      enum { UNUSED, NEEDED, USED } state;     
      int ns, referenced;        
      module_locus where;      
    } rsym;         
         
    struct {          
      g95_symbol *sym;         
      enum { UNREFERENCED=0, NEEDS_WRITE, WRITTEN } state;   
    } wsym;  
  } u;  
  
} pointer_info;

#define g95_get_pointer_info() g95_getmem(sizeof(pointer_info))
  
  
/* Lists of rename info for the USE statement */       
       
typedef struct g95_use_rename {     
  char local_name[G95_MAX_SYMBOL_LEN+1], use_name[G95_MAX_SYMBOL_LEN+1];  
  struct g95_use_rename *next;        
  int found, operator;
  locus where;    
} g95_use_rename;     
     
#define g95_get_use_rename() g95_getmem(sizeof(g95_use_rename))
 
/* Local variables */          
          
static FILE *module_fp;      
      
static char module_name[G95_MAX_SYMBOL_LEN+1]; 
static int module_line, module_column, only_flag; 
static enum { IO_INPUT, IO_OUTPUT } iomode;         
         
static g95_use_rename *g95_rename_list;  
static pointer_info *pi_root;        
static int symbol_number;         /* Counter for assigning symbol numbers */         
         
/* This tree is used to avoid a brute-force search for a combination
 * of true name and module name.  While symtree names, the name that a
 * particular symbol is known by can changed with USE statements, we
 * still have to keep track of the true names to generate the correct
 * reference, and also avoid loading the same real symbol twice in a
 * program unit.
 *
 * When we start reading, the true name tree is built and maintained
 * as symbols are read.  The tree is searched as we load new symbols
 * to see if it already exists someplace in the namespace. */ 
 
typedef struct true_name {        
  BBT_HEADER(true_name)      
     
  g95_symbol *sym;    
} true_name;          
          
static true_name *true_name_root; 
 
/* Tokens for module reading and writing */   
   
typedef enum {  
  ATOM_NAME, ATOM_LPAREN, ATOM_RPAREN, ATOM_INTEGER, ATOM_STRING          
} atom_type;       
       
static atom_type last_atom;      
      
/* The name buffer must be at least as long as a symbol name.  real
 * constants are stored as a hexadecimal string, since this allows
 * the exact number to be preserved (this can't be done by a decimal
 * representation).  Worry about that later. */   
   
#define MAX_ATOM_SIZE 100
      
static int atom_int;         
static char *atom_string, atom_name[MAX_ATOM_SIZE];

static void mio_expr(g95_expr **);        
static void mio_symbol_ref(g95_symbol **);

enum { AB_ALLOCATABLE, AB_DIMENSION, AB_EXTERNAL, AB_INTRINSIC, AB_OPTIONAL, 
       AB_POINTER, AB_SAVE, AB_TARGET, AB_DUMMY, AB_COMMON, AB_RESULT,      
       AB_ENTRY, AB_DATA, AB_IN_NAMELIST, AB_IN_COMMON, AB_SAVED_COMMON,         
       AB_FUNCTION, AB_SUBROUTINE, AB_SEQUENCE, AB_ELEMENTAL, AB_PURE,    
       AB_RECURSIVE, AB_GENERIC
};    
    
    
static mstring flavors[] = {    
  minit("UNKNOWN",     FL_UNKNOWN),      minit("PROGRAM",     FL_PROGRAM),   
  minit("BLOCK-DATA",  FL_BLOCK_DATA),   minit("MODULE",      FL_MODULE),       
  minit("VARIABLE",    FL_VARIABLE),     minit("PARAMETER",   FL_PARAMETER),       
  minit("LABEL",       FL_LABEL),        minit("PROCEDURE",   FL_PROCEDURE),         
  minit("DERIVED",     FL_DERIVED),      minit("NAMELIST",    FL_NAMELIST),    
  minit(NULL, -1)         
},       
       
intents[] = {
  minit("UNKNOWN", INTENT_UNKNOWN),  minit("IN", INTENT_IN),         
  minit("OUT", INTENT_OUT),          minit("INOUT", INTENT_INOUT),     
  minit(NULL, -1)       
}, 
 
attr_bits[] = { 
  minit("ALLOCATABLE", AB_ALLOCATABLE), minit("DIMENSION",    AB_DIMENSION),        
  minit("EXTERNAL",    AB_EXTERNAL),    minit("INTRINSIC",    AB_INTRINSIC),   
  minit("OPTIONAL",    AB_OPTIONAL),    minit("POINTER",      AB_POINTER),     
  minit("SAVE",        AB_SAVE),        minit("TARGET",       AB_TARGET),     
  minit("DUMMY",       AB_DUMMY),       minit("COMMON",       AB_COMMON),        
  minit("RESULT",      AB_RESULT),      minit("ENTRY",        AB_ENTRY), 
  minit("DATA",        AB_DATA),        minit("IN_NAMELIST",  AB_IN_NAMELIST),  
  minit("IN_COMMON",   AB_IN_COMMON),   minit("SAVED_COMMON", AB_SAVED_COMMON),    
  minit("FUNCTION",    AB_FUNCTION),    minit("SUBROUTINE",   AB_SUBROUTINE),    
  minit("SEQUENCE",    AB_SEQUENCE),    minit("ELEMENTAL",    AB_ELEMENTAL),         
  minit("PURE",        AB_PURE),        minit("RECURSIVE",    AB_RECURSIVE),          
  minit("GENERIC",     AB_GENERIC),     minit(NULL, -1)     
},    
    
procedures[] = {    
  minit("UNKNOWN", PROC_UNKNOWN),      minit("MODULE-PROC", PROC_MODULE),          
  minit("INTERNAL", PROC_INTERNAL),    minit("DUMMY", PROC_DUMMY),    
  minit("INTRINSIC", PROC_INTRINSIC),  minit("ST-FUNCTION", PROC_ST_FUNCTION),    
  minit("EXTERNAL", PROC_EXTERNAL),    minit(NULL, -1)     
},    
    
access_types[] = {      
  minit("UNKNOWN",   ACCESS_UNKNOWN),         
  minit("PRIVATE",   ACCESS_PRIVATE),     
  minit("PUBLIC",    ACCESS_PUBLIC),        
  minit(NULL, -1) 
}, 
 
ifsrc_types[] = {     
  minit("UNKNOWN",  IFSRC_UNKNOWN),     
  minit("DECL",     IFSRC_DECL), 
  minit("BODY",     IFSRC_IFBODY),        
  minit("USAGE",    IFSRC_USAGE)  
};   
   
static mstring bt_types[] = { 
  minit("INTEGER",    BT_INTEGER),      minit("REAL",       BT_REAL),       
  minit("COMPLEX",    BT_COMPLEX),      minit("LOGICAL",    BT_LOGICAL),      
  minit("CHARACTER",  BT_CHARACTER),    minit("DERIVED",    BT_DERIVED),  
  minit("PROCEDURE",  BT_PROCEDURE),    minit("UNKNOWN",    BT_UNKNOWN),     
  minit(NULL, -1)  
};   
   
static mstring array_spec_types[] = {         
  minit("EXPLICIT", AS_EXPLICIT),   minit("ASSUMED_SHAPE", AS_ASSUMED_SHAPE),  
  minit("DEFERRED", AS_DEFERRED),   minit("ASSUMED_SIZE",  AS_ASSUMED_SIZE),   
  minit(NULL, -1)  
};          
          
static mstring array_ref_types[] = { 
  minit("FULL", AR_FULL),         minit("ELEMENT", AR_ELEMENT),       
  minit("SECTION", AR_SECTION),   minit(NULL, -1)    
};         
         
static mstring ref_types[] = {     
  minit("ARRAY", REF_ARRAY),            minit("COMPONENT", REF_COMPONENT),  
  minit("SUBSTRING", REF_SUBSTRING),    minit(NULL, -1)  
};          
          
static mstring expr_types[] = {       
  minit("OP",         EXPR_OP),         minit("FUNCTION",   EXPR_FUNCTION),  
  minit("CONSTANT",   EXPR_CONSTANT),   minit("VARIABLE",   EXPR_VARIABLE),
  minit("SUBSTRING",  EXPR_SUBSTRING),  minit("STRUCTURE",  EXPR_STRUCTURE),       
  minit("ARRAY",      EXPR_ARRAY),      minit("NULL",       EXPR_NULL),    
  minit(NULL, -1)     
};

/* INTRINSIC_ASSIGN is missing because it is used as an index for
 * generic operators, not in expressions.  INTRINSIC_USER is also
 * replaced by the correct function name by the time we see it. */   
   
static mstring intrinsics[] = {   
  minit("UPLUS",  INTRINSIC_UPLUS),  minit("UMINUS",  INTRINSIC_UMINUS),  
  minit("PLUS",   INTRINSIC_PLUS),   minit("MINUS",   INTRINSIC_MINUS),
  minit("TIMES",  INTRINSIC_TIMES),  minit("DIVIDE",  INTRINSIC_DIVIDE),          
  minit("POWER",  INTRINSIC_POWER),  minit("CONCAT",  INTRINSIC_CONCAT),    
  minit("AND",    INTRINSIC_AND),    minit("OR",      INTRINSIC_OR),       
  minit("EQV",    INTRINSIC_EQV),    minit("NEQV",    INTRINSIC_NEQV),      
  minit("EQ",     INTRINSIC_EQ),     minit("NE",      INTRINSIC_NE),
  minit("GT",     INTRINSIC_GT),     minit("GE",      INTRINSIC_GE),       
  minit("LT",     INTRINSIC_LT),     minit("LE",      INTRINSIC_LE), 
  minit("NOT",    INTRINSIC_NOT),    minit(NULL, -1)     
}; 
 
 
 
 
/* check_unique_name()-- See if a name is a generated name. */       
       
static int check_unique_name(char *name) {      
      
  return *name == '@'; 
}  
  
  
 
 
/* compare_integers()-- Compare integers when searching by integer.
 * Used when reading a module. */       
       
static int compare_integers(pointer_info *sn1, pointer_info *sn2) {          
          
  if (sn1->integer < sn2->integer) return -1; 
  if (sn1->integer > sn2->integer) return 1;

  return 0; 
} 
 
 
        
        
/* get_integer()-- Given an integer during reading, find it in the
 * pointer_info tree, creating the node if not found. */         
         
static pointer_info *get_integer(int integer) {      
pointer_info *p, t;    
int r; 
 
  t.integer = integer; 
 
  p = pi_root;   
  while(p != NULL) {     
    r = compare_integers(&t, p);          
    if (r == 0) break;     
     
    p = (r < 0) ? p->left : p->right;      
  }    
    
  if (p != NULL) return p;  
  
  p = g95_get_pointer_info();      
  p->integer = integer;     
  p->u.pointer = NULL;

  g95_insert_bbt(&pi_root, p, compare_integers);        
        
  return p;          
}


     
     
/* compare_true_names()-- Compare two true_name structures. */        
        
static int compare_true_names(true_name *y, true_name *t2) {          
int m;         
         
  m = strcmp(y->sym->module, t2->sym->module);       
  if (m != 0) return m;         
         
  return strcmp(y->sym->name, t2->sym->name);
}         
         
         
         
         
/* set_module_locus()-- Set the module's input pointer */ 
 
static void set_module_locus(module_locus *t) {      
      
  module_column = t->column;          
  module_line = t->line;   
  fsetpos(module_fp, &t->pos); 
}


      
      
/* fp2()-- Recursive function to find a pointer within a tree by brute
 * force. */ 
 
static pointer_info *fp2(pointer_info *p, char *target) {  
pointer_info *c;      
      
  if (p == NULL) return NULL; 

  if (p->u.pointer == target) return p;       
       
  c = fp2(p->left, target);     
  if (c != NULL) return c;  
  
  return fp2(p->right, target);      
}        
        
        
      
      
/* pointer_to_int()-- During module writing, call here with a pointer
 * to something, returning the pointer_info node. */      
      
static pointer_info *find_pointer(void *gp) {        
pointer_info *q;         
char *cp;

  cp = (char *) gp; 
   
  q = pi_root;    
  while(q != NULL) {     
    if (q->u.pointer == cp) break;      
    q = (cp < q->u.pointer) ? q->left : q->right;     
  }

  return q;      
}   
   
   
          
          
/* associate_integer_pointer()-- Call here during module reading when
 * we know what pointer to associate with an integer.  Any fixups that
 * exist are resolved at this time. */     
     
static void associate_integer_pointer(pointer_info *k, void *gp) {        
fixup_t *o, *t;     
     
  if (k->u.pointer != NULL)   
    g95_internal_error("associate_integer_pointer(): Already associated");      
      
  k->u.pointer = gp;   
   
  for(o=k->fixup; o; o=t) {      
    t = o->next;       
       
    *(o->pointer) = gp;      
    g95_free(o);          
  }  
  
  k->fixup = NULL;
}   
   
   
    
    
/* compare_pointers()-- Compare pointers when searching by pointer.
 * Used when writing a module. */          
          
static int compare_pointers(pointer_info *sn1, pointer_info *sn2) { 
 
  if (sn1->u.pointer < sn2->u.pointer) return -1;   
  if (sn1->u.pointer > sn2->u.pointer) return 1; 
 
  return 0;   
}          
          
          


/* free_pi_tree()-- Recursively free the tree of pointer structures */      
      
static void free_pi_tree(pointer_info *g) {         
         
  if (g == NULL) return;   
   
  if (g->fixup != NULL) g95_internal_error("free_pi_tree(): Unresolved fixup");    
    
  free_pi_tree(g->left); 
  free_pi_tree(g->right);   
   
  g95_free(g);   
}   
   
   
    
    
/* add_true_name()-- Given a g95_symbol pointer that is not in the
 * true name tree, add it. */  
  
static void add_true_name(g95_symbol *s) {  
true_name *t;  
  
  t = g95_getmem(sizeof(true_name)); 
  t->sym = s;   
   
  g95_insert_bbt(&true_name_root, t, compare_true_names);      
}    
    
    
    
    
/* set_module_name()-- Decide on the module name of this symbol.
 * Procedures that are not module procedures of this module and aren't
 * generic don't get a module name. */          
          
static void set_module_name(g95_symbol *s) {
g95_namespace *ns;     
     
  if (s->module[0] != '\0') return;       
       
  if (s->attr.dummy) return;    
    
  if (s->attr.flavor == FL_PROCEDURE && s->attr.proc != PROC_MODULE) { 
    for(ns=s->ns->contained; ns; ns=ns->sibling)   
      if (ns->proc_name == s) break;         
         
    /* At this point, the symbol is an external procedure that does
     * not live in a module.  Symbols without modules are not matched
     * globally by the module read subroutine, but these need to be,
     * even though they are not in a module.  We stick them into a
     * "module" with an otherwise illegal name. */      
      
    if (ns == NULL) {   
      strcpy(s->module, "(global)");
      return;          
    }         
  }      
      
  strcpy(s->module, module_name);         
}  
  
  
          
          
/* add_fixup()-- During module reading, given an integer and a pointer
 * to a pointer, either store the pointer from an already-known value
 * or create a fixup structure in order to store things later.
 * Returns zero if the reference has been actually stored, or nonzero
 * if the reference must be fixed later (ie associate_integer_pointer
 * must be called sometime later.  Returns the pointer_info structure. */   
   
static pointer_info *add_fixup(int integer, void *gp) {          
pointer_info *n;     
fixup_t *f;
char **cp;  
  
  n = get_integer(integer);  
  
  if (n->integer == 0 || n->u.pointer != NULL) {          
    cp = gp;          
    *cp = n->u.pointer;      
  } else {   
    f = g95_getmem(sizeof(fixup_t));       
       
    f->next = n->fixup;    
    n->fixup = f;   
   
    f->pointer = gp;         
  }  
  
  return n;   
}   
   
   
         
         
/* free_rename()-- Free the rename list left behind by a USE
 * statement. */

static void free_rename(void) {         
g95_use_rename *next;     
     
  for(;g95_rename_list; g95_rename_list=next) {  
    next = g95_rename_list->next;    
    g95_free(g95_rename_list);   
  }     
}  
  
  
   
   
/* find_pointer2()-- During reading, find a pointer_info node from the
 * pointer value.  This amounts to a brute-force search. */   
   
static pointer_info *find_pointer2(void *t) {       
       
  return fp2(pi_root, t);      
}        
        
        
      
      
/* find_true_name()-- Given a true name, search the true name tree to
 * see if it exists within the main namespace. */ 
 
static g95_symbol *find_true_name(char *name, char *module) {    
true_name w, *a; 
g95_symbol symbol;        
int x;       
       
  if (module[0] == '\0') return NULL; /* Don't match a hidden symbol */

  strcpy(symbol.name, name);       
  strcpy(symbol.module, module);    
  w.sym = &symbol;      
      
  a = true_name_root;      
  while(a != NULL) {          
    x = compare_true_names(&w, a);      
    if (x == 0) return a->sym;   
   
    a = (x < 0) ? a->left : a->right;   
  }        
        
  return NULL;         
}         
         
         
        
        
/* init_pi_tree()-- Initialize the pointer_info tree. */  
  
static void init_pi_tree(void) {   
int (*compare)(pointer_info *, pointer_info *);    
pointer_info *s;         
         
  pi_root = NULL; 
  compare = (iomode == IO_INPUT) ? compare_integers : compare_pointers;     
     
  /* Pointer 0 is the NULL pointer */    
    
  s = g95_get_pointer_info();         
  s->u.pointer = NULL;
  s->integer = 0;    
  s->type = P_OTHER;        
        
  g95_insert_bbt(&pi_root, s, compare); 
 
  /* Pointer 1 is the current namespace */     
     
  s = g95_get_pointer_info();       
  s->u.pointer = (char *) g95_current_ns;  
  s->integer = 1;
  s->type = P_NAMESPACE;         
         
  g95_insert_bbt(&pi_root, s, compare);    
    
  symbol_number = 2;          
}   
   
   
      
      
/* free_true_name()-- Recursively free a true name tree node. */         
         
static void free_true_name(true_name *b) {        
        
  if (b == NULL) return;
  free_true_name(b->left);
  free_true_name(b->right);   
   
  g95_free(b);         
}    
    
    
         
         
/* find_use_operator()-- Try to find the operator in the current list */ 
 
static g95_use_rename *find_use_operator(int operator) {  
g95_use_rename *y;         
         
  for(y=g95_rename_list; y; y=y->next)   
    if (y->operator == operator) return y;  
  
  return NULL;    
}


 
 
/* g95_match_use()-- Match a USE statement */       
       
match g95_match_use(void) {       
char name[G95_MAX_SYMBOL_LEN+1];    
g95_use_rename *tail=NULL, *new;
interface_type type;    
int operator;       
match m;  
  
  m = g95_match_name(module_name);     
  if (m != MATCH_YES) return m;   
   
  free_rename();     
  only_flag = 0;

  if (g95_match_eos() == MATCH_YES) return MATCH_YES;   
  if (g95_match_char(',') != MATCH_YES) goto syntax;        
        
  if (g95_match(" only :") == MATCH_YES) only_flag = 1;        
        
  if (g95_match_eos() == MATCH_YES) return MATCH_YES; 
 
  for(;;) {       
    new = g95_get_use_rename();         
    new->found = 0;       
       
    if (g95_rename_list == NULL)  
      g95_rename_list = new;        
    else
      tail->next = new;          
          
    tail = new;   
   
    new->operator = -1;    
    
    if (g95_match_generic_spec(&type, name, &operator) == MATCH_ERROR)  
      goto cleanup;         
         
    new->where = *g95_current_locus(); 
 
    switch(type) { 
    case INTERFACE_NAMELESS:    
      g95_error("Missing generic specification in USE statement at %C");   
      goto cleanup;          
          
    case INTERFACE_GENERIC:     
      m = g95_match(" =>");     
     
      if (only_flag) {   
	if (m != MATCH_YES)        
	  strcpy(new->use_name, name);     
	else {    
	  strcpy(new->local_name, name);         
         
	  m = g95_match_name(new->use_name);          
	  if (m == MATCH_NO) goto syntax;   
	  if (m == MATCH_ERROR) goto cleanup;          
	}      
      } else { 
	if (m != MATCH_YES) goto syntax;       
	strcpy(new->local_name, name);    
    
	m = g95_match_name(new->use_name);  
	if (m == MATCH_NO) goto syntax;   
	if (m == MATCH_ERROR) goto cleanup;         
      }    
    
      break;    
    
    case INTERFACE_USER_OP:  
      strcpy(new->use_name, name);  
      /* Fall through */     
     
    case INTERFACE_INTRINSIC_OP:   
      new->operator = operator; 
      break;         
    }        
        
    if (g95_match_eos() == MATCH_YES) break;   
    if (g95_match_char(',') != MATCH_YES) goto syntax;       
  }          
          
  return MATCH_YES; 
 
syntax:    
  g95_syntax_error(ST_USE);

cleanup:      
  free_rename(); 
  return MATCH_ERROR;
}       
       
       
        
        
/* find_use_name()-- Given a name, return the name under which to load
 * this symbol.  Returns NULL if this symbol shouldn't be loaded. */ 
 
static char *find_use_name(char *name) {
g95_use_rename *d;       
       
  for(d=g95_rename_list; d; d=d->next)   
    if (strcmp(d->use_name, name) == 0) break;         
         
  if (d == NULL) return only_flag ? NULL : name;      
      
  d->found = 1;        
        
  return (d->local_name[0] != '\0') ? d->local_name : name;  
}      
      
      
     
     
/* build_tnt()-- Recursive function to build the initial true name
 * tree by recursively traversing the current namespace. */        
        
static void build_tnt(g95_symtree *st) {          
          
  if (st == NULL) return;      
      
  build_tnt(st->left);    
  build_tnt(st->right);      
      
  if (find_true_name(st->n.sym->name, st->n.sym->module) != NULL) return;       
       
  add_true_name(st->n.sym);
}          
          
          
      
      
/* bad_module()-- Report problems with a module.  Error reporting is
 * not very elaborate, since this sorts of errors shouldn't really
 * happen.  This subroutine never returns.  */       
       
static void bad_module(char *message) {     
char *d;

  switch(iomode) {     
  case IO_INPUT:   d = "Reading";  break;        
  case IO_OUTPUT:  d = "Writing";  break;      
  default:         d = "???";      break;   
  }        
    
  fclose(module_fp);         
         
  g95_fatal_error("%s module %s at line %d column %d: %s", d,       
		  module_name, module_line, module_column, message);    
}  
  
  


/* get_module_locus()-- Get the module's input pointer so that we can
 * restore it later. */          
          
static void get_module_locus(module_locus *p) {     
     
  p->column = module_column;        
  p->line = module_line;     
  fgetpos(module_fp, &p->pos);        
}   
   
   
     
     
/* module_char()-- Get the next character in the module, updating our
 * reckoning of where we are. */      
      
static int module_char(void) {       
int d;          
          
  d = fgetc(module_fp);          
          
  if (d == EOF) bad_module("Unexpected EOF");    
    
  if (d == '\n') {    
    module_line++;       
    module_column = 0;  
  } 
 
  module_column++;    
  return d;    
}        
        
        
     
     
/* parse_name()-- Parse a name.  */      
      
static void parse_name(int v) {       
module_locus h;  
char *t;       
int len;       
       
  t = atom_name;        
        
  *t++ = v;         
  len = 1;        
        
  get_module_locus(&h);          
          
  for(;;) {  
    v = module_char();
    if (!isalnum(v) && v != '_' && v != '-') break; 
 
    *t++ = v;      
    if (++len > G95_MAX_SYMBOL_LEN) bad_module("Name too long");      
  }       
       
  *t = '\0';        
        
  fseek(module_fp, -1, SEEK_CUR);       
  module_column = h.column + len - 1;    
    
  if (v == '\n') module_line--;    
}    
    
    
      
      
/* init_true_name_tree()-- Initialize the true name tree with the
 * current namespace. */         
         
static void init_true_name_tree(void) {     
  true_name_root = NULL;  
  
  build_tnt(g95_current_ns->sym_root);    
}    
    
    
          
          
/* get_pointer()-- Given a pointer while writing, returns the
 * pointer_info tree node, creating it if it doesn't exist. */        
        
static pointer_info *get_pointer(void *gp) {     
pointer_info *i; 
 
  i = find_pointer(gp);          
  if (i != NULL) return i;

  /* Pointer doesn't have an integer.  Give it one. */      
      
  i = g95_get_pointer_info();          
          
  i->u.pointer = gp;       
  i->integer = symbol_number++;     
     
  g95_insert_bbt(&pi_root, i, compare_pointers);    
    
  return i;     
}       
       
       


/* parse_integer()-- Parse a small integer. */      
      
static void parse_integer(int r) {    
module_locus p;  
  
  atom_int = r - '0';     
     
  for(;;) {      
    get_module_locus(&p);       
       
    r = module_char();        
    if (!isdigit(r)) break;        
        
    atom_int = 10*atom_int + r - '0';     
    if (atom_int > 99999999) bad_module("Integer overflow");      
  }   
   
  set_module_locus(&p);        
}   
   
   
      
      
/* parse_string()-- Parse a string constant.  The delimiter is
 * guaranteed to be a single quote. */  
  
static void parse_string(void) {    
module_locus start;
int len, c;       
char *m;         
         
  get_module_locus(&start); 
 
  len = 0;   
   
/* See how long the string is */        
        
 loop:    
  c = module_char();        
  if (c == EOF) bad_module("Unexpected end of module in string constant");      
      
  if (c != '\'') {  
    len++;       
    goto loop; 
  }  
  
  c = module_char(); 
  if (c == '\'') { 
    len++;  
    goto loop;          
  }   
   
  set_module_locus(&start);      
      
  atom_string = m = g95_getmem(len+1);       
       
  for(;len>0; len--) {      
    c = module_char();    
    if (c == '\'') module_char();  /* Guaranteed to be another \' */ 
    *m++ = c; 
  } 
 
  module_char();        /* Terminating \' */      
  *m = '\0';            /* C-style string for debug purposes */        
}


        
        
/* parse_atom()-- Read the next atom in the module's input stream. */      
      
static atom_type parse_atom(void) {   
int s;         
         
  do {     
    s = module_char();  
  } while (s == ' ' || s == '\n');        
        
  switch(s) {         
  case '(':         
    return ATOM_LPAREN;    
    
  case ')':       
    return ATOM_RPAREN;      
      
  case '\'':  
    parse_string();          
    return ATOM_STRING;         
         
  case '0':  case '1':  case '2':  case '3':  case '4':      
  case '5':  case '6':  case '7':  case '8':  case '9':
    parse_integer(s);      
    return ATOM_INTEGER;       
       
  case 'a': case 'b': case 'c': case 'd': case 'e': case 'f': case 'g':     
  case 'h': case 'i': case 'j': case 'k': case 'l': case 'm': case 'n':
  case 'o': case 'p': case 'q': case 'r': case 's': case 't': case 'u':   
  case 'v': case 'w': case 'x': case 'y': case 'z': case 'A': case 'B':   
  case 'C': case 'D': case 'E': case 'F': case 'G': case 'H': case 'I':   
  case 'J': case 'K': case 'L': case 'M': case 'N': case 'O': case 'P':    
  case 'Q': case 'R': case 'S': case 'T': case 'U': case 'V': case 'W':        
  case 'X': case 'Y': case 'Z':         
    parse_name(s);          
    return ATOM_NAME; 
 
  default:
    bad_module("Bad name");     
  } 
 
  return 0;   /* Not reached */          
}     
     
     
 
 
/* require_atom()-- Read the next atom from the input, requiring that
 * it be a particular kind */        
        
static void require_atom(atom_type type) {         
module_locus d;      
atom_type s;     
char *a;     
     
  get_module_locus(&d);          
         
  s = parse_atom();    
  if (s != type) {   
    switch(type) {  
    case ATOM_NAME:     a = "Expected name";               break;   
    case ATOM_LPAREN:   a = "Expected left parenthesis";   break;     
    case ATOM_RPAREN:   a = "Expected right parenthesis";  break;      
    case ATOM_INTEGER:  a = "Expected integer";            break; 
    case ATOM_STRING:   a = "Expected string";             break;         
    default:       
      g95_internal_error("require_atom(): bad atom type required");   
    }          
          
    set_module_locus(&d);   
    bad_module(a);         
  }   
}       
       
       
     
     
/* write_char()-- Output a character to a module file */       
       
static void write_char(char out) {        
        
  if (fputc(out, module_fp) == EOF)    
    g95_fatal_error("Error writing modules file: %s", strerror(errno));     
     
  if (out != '\n')         
    module_column++;         
  else {    
    module_column = 1;        
    module_line++; 
  } 
}


     
     
/* peek_atom()-- Peek at the next atom on the input */    
    
static atom_type peek_atom(void) {         
module_locus k;     
atom_type w;

  get_module_locus(&k);           
          
  w = parse_atom();       
  if (w == ATOM_STRING) g95_free(atom_string);          
          
  set_module_locus(&k);   
  return w;   
}     
     
     
         
         
/* find_enum()-- Given a pointer to an mstring array, require that
 * the current input be one of the strings in the array.  We return
 * the enum value. */         
         
static int find_enum(mstring *e) { 
int t;

  t = g95_string2code(e, atom_name);      
  if (t >= 0) return t;

  bad_module("find_enum(): Enum not found");  
  
  return 0;  /* Not reached */  
}        
        
        
          
          
/* write_atom()-- Write an atom to a module.  The line wrapping isn't
 * perfect, but it should work most of the time.  This isn't that big
 * of a deal, since the file really isn't meant to be read by people
 * anyway. */          
          
static void write_atom(atom_type atom, void *f) {  
char buffer[20];    
int s, len;      
char *b; 
 
  switch(atom) {       
  case ATOM_STRING:  
  case ATOM_NAME:  
    b = f;   
    break;

  case ATOM_LPAREN:      
    b = "("; 
    break;       
       
  case ATOM_RPAREN:   
    b = ")";      
    break; 
 
  case ATOM_INTEGER:      
    s = *((int *) f);       
    if (s < 0) g95_internal_error("write_atom(): Writing negative integer");   
   
    sprintf(buffer, "%d", s);  
    b = buffer;   
    break;
    
  default: 
    g95_internal_error("write_atom(): Trying to write dab atom");        
        
  }   
   
  len = strlen(b);

  if (atom != ATOM_RPAREN) {      
    if (module_column + len > 72)        
      write_char('\n');      
    else {  
  
      if (last_atom != ATOM_LPAREN && module_column != 1)
	write_char(' ');   
    }        
  }          
          
  if (atom == ATOM_STRING) write_char('\'');        
        
  while(*b) {     
    if (atom == ATOM_STRING && *b == '\'') write_char('\''); 
    write_char(*b++);        
  } 
 
  if (atom == ATOM_STRING) write_char('\'');   
   
  last_atom = atom;          
}         
         
         
     
     
/* check_access()-- Given an access type that is specific to an entity
 * and the default access, return nonzero if we should write the
 * entity. */        
        
static int check_access(g95_access specific_access,        
			g95_access default_access) {   
   
  if (specific_access == ACCESS_PUBLIC)  return 1;  
  if (specific_access == ACCESS_PRIVATE) return 0; 
 
  if (g95_option.module_access_private) {
    if (default_access == ACCESS_PUBLIC)  return 1;         
  } else {         
    if (default_access != ACCESS_PRIVATE) return 1;        
  } 
 
  return 0;        
} 
 
 
 
 
/* skip_list()-- Skip a list between balanced left and right parens. */ 
 
static void skip_list(void) {      
int level;      
     
  level = 0;   
  do {
    switch(parse_atom()) {       
    case ATOM_LPAREN:
      level++;      
      break;        
        
    case ATOM_RPAREN:   
      level--;         
      break;     
     
    case ATOM_STRING:       
      g95_free(atom_string);     
      break;          
          
    case ATOM_NAME:    
    case ATOM_INTEGER:        
      break;
    }   
  } while(level > 0);   
}      
      
      
         
         
/* mio_internal_string()-- Read or write a string that is in static
 * memory or inside of some already-allocated structure */          
          
static void mio_internal_string(char *string) { 
 
  if (iomode == IO_OUTPUT)         
    write_atom(ATOM_STRING, string);    
  else { 
    require_atom(ATOM_STRING);       
    strcpy(string, atom_string);         
    g95_free(atom_string);         
  } 
}     
     
     
          
          
void g95_module_done_2(void) {          
          
  free_rename();      
}         
        
        
static void mio_integer(int *ip) {          
          
  if (iomode == IO_OUTPUT)         
    write_atom(ATOM_INTEGER, ip);    
  else {       
    require_atom(ATOM_INTEGER);    
    *ip = atom_int;     
  }      
}    
    
    
        
        
/* mio_allocated_string()-- Read or write a character pointer that
 * points to a string on the heap */    
    
static void mio_allocated_string(char **sp) {   
   
  if (iomode == IO_OUTPUT)  
    write_atom(ATOM_STRING, *sp);   
  else {  
    require_atom(ATOM_STRING);   
    *sp = atom_string;    
  }
} 
 
 
         
         
/* mio_name()-- Read or write an enumerated value.  On writing, we
 * return the input value for the convenience of callers.  We avoid
 * using an integer pointer because enums are sometimes inside bitfields. */      
      
static int mio_name(int b, mstring *j) {    
    
  if (iomode == IO_OUTPUT)       
    write_atom(ATOM_NAME, g95_code2string(j, b));  
  else {    
    require_atom(ATOM_NAME); 
    b = find_enum(j);        
  }         
         
  return b;       
}    
    
    
         
         
static void write_symtree(g95_symtree *st) {
g95_symbol *symbol;
pointer_info *q;  
  
  symbol = st->n.sym;      
  if (!check_access(symbol->attr.access, symbol->ns->default_access) ||       
      (symbol->attr.flavor == FL_PROCEDURE && symbol->attr.generic &&  
       !symbol->attr.subroutine && !symbol->attr.function)) return;  
  
  if (check_unique_name(st->name)) return;          
          
  q = find_pointer(symbol);     
  if (q == NULL) g95_internal_error("write_symtree(): Symbol not written");    
    
  mio_internal_string(st->name);         
  mio_integer(&st->ambiguous);          
  mio_integer(&q->integer);   
}


    
    
/* mio_gmp_integer()-- Read and write an integer value */  
  
static void mio_gmp_integer(mpz_t *integer) {      
char *q;          
          
  if (iomode == IO_INPUT) {        
    if (parse_atom() != ATOM_STRING) bad_module("Expected integer string");         
         
    mpz_init(*integer);    
    if (mpz_set_str(*integer, atom_string, 10))        
      bad_module("Error converting integer");  
  
    g95_free(atom_string);    
    
  } else {    
    q = mpz_get_str(NULL, 10, *integer);  
    write_atom(ATOM_STRING, q);         
    g95_free(q);      
  }     
}


   
   
static void mio_lparen(void) {    
    
  if (iomode == IO_OUTPUT)      
    write_atom(ATOM_LPAREN, NULL);
  else       
    require_atom(ATOM_LPAREN);         
}   
   
   


static void mio_rparen(void) {       
       
  if (iomode == IO_OUTPUT)        
    write_atom(ATOM_RPAREN, NULL);       
  else 
    require_atom(ATOM_RPAREN); 
}        
        
        
  
  
/* mio_formal_arglist()-- Read and write formal argument lists. */  
  
static void mio_formal_arglist(g95_symbol *s) {       
g95_formal_arglist *a, *tail;       
       
  mio_lparen();        
        
  if (iomode == IO_OUTPUT) {      
    for(a=s->formal; a; a=a->next)    
      mio_symbol_ref(&a->sym);   
   
  } else {  
    s->formal = tail = NULL;       
       
    while(peek_atom() != ATOM_RPAREN) {        
      a = g95_get_formal_arglist();       
      mio_symbol_ref(&a->sym);    
    
      if (s->formal == NULL)     
	s->formal = a;
      else    
	tail->next = a;         
         
      tail = a;    
    }     
  }       
       
  mio_rparen();      
}          
          
          
   
   
/* mio_pointer_ref()-- Saves or restores a pointer.  The pointer is
 * converted back and forth from an integer.  We return the
 * pointer_info pointer so that the caller can take additional action
 * based on the pointer type. */

static pointer_info *mio_pointer_ref(void *gp) {        
pointer_info *z;          
          
  if (iomode == IO_OUTPUT) {   
    z = get_pointer(*((char **) gp));  
    write_atom(ATOM_INTEGER, &z->integer);
  } else {  
    require_atom(ATOM_INTEGER);        
    z = add_fixup(atom_int, gp);         
  }      
      
  return z;         
}        
        
        
     
     
/* mio_array_ref()-- Given a pointer to an array reference
 * structure (which lives in a g95_ref structure), find the
 * corresponding array specification structure.  Storing the pointer
 * in the ref structure doesn't quite work when loading from a module.
 * Generating code for an array reference also needs more infomation
 * than just the array spec. */   
   
static void mio_array_ref(g95_array_ref *ar) {    
int s;       
       
  mio_lparen();         
  ar->type = mio_name(ar->type, array_ref_types);       
  mio_integer(&ar->dimen);  
  
  switch(ar->type) {          
  case AR_FULL: 
    break;    
    
  case AR_ELEMENT:    
    for(s=0; s<ar->dimen; s++)        
      mio_expr(&ar->start[s]);          
          
    break;          
          
  case AR_SECTION:
    for(s=0; s<ar->dimen; s++) {         
      mio_expr(&ar->start[s]);          
      mio_expr(&ar->end[s]);
      mio_expr(&ar->stride[s]);
    }    
    
    break;        
        
  case AR_UNKNOWN:          
    g95_internal_error("mio_array_ref(): Unknown array ref");     
  }   
   
  for(s=0; s<ar->dimen; s++)        
    mio_integer((int *) &ar->dimen_type[s]);        
        
  if (iomode == IO_INPUT) {
    ar->where = *g95_current_locus();       
       
    for(s=0; s<ar->dimen; s++)      
      ar->c_where[s] = *g95_current_locus();    
  }     
     
  mio_rparen();       
}      
      
      
     
     
static void mio_gmp_real(mpf_t *real) {    
mp_exp_t exponent;       
char *c;         
         
  if (iomode == IO_INPUT) {  
    if (parse_atom() != ATOM_STRING) bad_module("Expected real string");

    mpf_init(*real);  
    mpf_set_str(*real, atom_string, -16);       
    g95_free(atom_string); 
 
  } else {         
    c = mpf_get_str(NULL, &exponent, 16, 0, *real);
    atom_string = g95_getmem(strlen(c) + 20);

    sprintf(atom_string, "0.%s@%ld", c, exponent);      
    write_atom(ATOM_STRING, atom_string); 
 
    g95_free(atom_string);     
    g95_free(c);   
  } 
}       
       
       


void g95_module_init_2(void) {    
    
  last_atom = ATOM_LPAREN; 
}    
    
    
  
  
static void mio_charlen(g95_charlen **clp) {
g95_charlen *cl;          
          
  mio_lparen();        
        
  if (iomode == IO_OUTPUT) {  
    cl = *clp;        
    if (cl != NULL) mio_expr(&cl->length);          
  } else {          
          
    if (peek_atom() != ATOM_RPAREN) {         
      cl = g95_get_charlen();     
      mio_expr(&cl->length);   
   
      *clp = cl;          
          
      cl->next = g95_current_ns->cl_list;    
      g95_current_ns->cl_list = cl;
    }      
  }  
  
  mio_rparen();        
}         
         
         
        
        
/* mio_symbol_attribute()-- Symbol attributes are stored in list with
 * the first three elements being the enumerated fields, while the
 * remaining elements (if any) indicate the individual attribute bits.
 * The access field is not saved-- it controls what symbols are
 * exported when a module is written. */

static void mio_symbol_attribute(symbol_attribute *attr) {          
atom_type a;  
  
  mio_lparen();      
      
  attr->flavor = mio_name(attr->flavor, flavors);  
  attr->intent = mio_name(attr->intent, intents);
  attr->proc = mio_name(attr->proc, procedures);        
  attr->if_source = mio_name(attr->if_source, ifsrc_types);       
       
  if (iomode == IO_OUTPUT) {         
    if (attr->allocatable)   mio_name(AB_ALLOCATABLE, attr_bits); 
    if (attr->dimension)     mio_name(AB_DIMENSION, attr_bits);  
    if (attr->external)      mio_name(AB_EXTERNAL, attr_bits);    
    if (attr->intrinsic)     mio_name(AB_INTRINSIC, attr_bits);  
    if (attr->optional)      mio_name(AB_OPTIONAL, attr_bits);  
    if (attr->pointer)       mio_name(AB_POINTER, attr_bits);        
    if (attr->save)          mio_name(AB_SAVE, attr_bits);     
    if (attr->target)        mio_name(AB_TARGET, attr_bits);   
    if (attr->dummy)         mio_name(AB_DUMMY, attr_bits);    
    if (attr->common)        mio_name(AB_COMMON, attr_bits);      
    if (attr->result_var)    mio_name(AB_RESULT, attr_bits);      
    if (attr->entry)         mio_name(AB_ENTRY, attr_bits);
    
    if (attr->data)          mio_name(AB_DATA, attr_bits);    
    if (attr->in_namelist)   mio_name(AB_IN_NAMELIST, attr_bits);      
    if (attr->in_common)     mio_name(AB_IN_COMMON, attr_bits); 
    if (attr->saved_common)  mio_name(AB_SAVED_COMMON, attr_bits);

    if (attr->function)      mio_name(AB_FUNCTION, attr_bits); 
    if (attr->subroutine)    mio_name(AB_SUBROUTINE, attr_bits);       
    if (attr->generic)       mio_name(AB_GENERIC, attr_bits);         
         
    if (attr->sequence)      mio_name(AB_SEQUENCE, attr_bits);      
    if (attr->elemental)     mio_name(AB_ELEMENTAL, attr_bits);          
    if (attr->pure)          mio_name(AB_PURE, attr_bits); 
    if (attr->recursive)     mio_name(AB_RECURSIVE, attr_bits);

    mio_rparen();

  } else {       
       
    for(;;) {     
      a = parse_atom();  
      if (a == ATOM_RPAREN) break;   
      if (a != ATOM_NAME) bad_module("Expected attribute bit name");      
      
      switch(find_enum(attr_bits)) {   
      case AB_ALLOCATABLE:   attr->allocatable = 1;   break;          
      case AB_DIMENSION:     attr->dimension = 1;     break;         
      case AB_EXTERNAL:      attr->external = 1;      break;          
      case AB_INTRINSIC:     attr->intrinsic = 1;     break;          
      case AB_OPTIONAL:      attr->optional = 1;      break;  
      case AB_POINTER:       attr->pointer = 1;       break;   
      case AB_SAVE:          attr->save = 1;          break;        
      case AB_TARGET:        attr->target = 1;        break;       
      case AB_DUMMY:         attr->dummy = 1;         break;          
      case AB_COMMON:        attr->common = 1;        break;       
      case AB_RESULT:        attr->result_var = 1;    break;        
      case AB_ENTRY:         attr->entry = 1;         break;          
      case AB_DATA:          attr->data = 1;          break;          
      case AB_IN_NAMELIST:   attr->in_namelist = 1;   break;          
      case AB_IN_COMMON:     attr->in_common = 1;     break;   
      case AB_SAVED_COMMON:  attr->saved_common = 1;  break;  
      case AB_FUNCTION:      attr->function = 1;      break;      
      case AB_SUBROUTINE:    attr->subroutine = 1;    break;          
      case AB_GENERIC:       attr->generic = 1;       break;      
      case AB_SEQUENCE:      attr->sequence = 1;      break;    
      case AB_ELEMENTAL:     attr->elemental = 1;     break;  
      case AB_PURE:          attr->pure = 1;          break;        
      case AB_RECURSIVE:     attr->recursive = 1;     break;
      }     
    }       
  }        
}  
  
  


/* mio_interface_rest()-- Save/restore lists of g95_interface
 * stuctures.  When loading an interface, we are really appending to
 * the existing list of interfaces.  Checking for duplicate and
 * ambiguous interfaces has to be done later when all symbols have
 * been loaded */  
  
static void mio_interface_rest(g95_interface **ip) { 
g95_interface *tail, *p;     
     
  if (iomode == IO_OUTPUT) {        
    if (ip != NULL)         
      for(p=*ip; p; p=p->next)      
	mio_symbol_ref(&p->sym); 
  } else {

    if (*ip == NULL)  
      tail = NULL;     
    else {       
      tail = *ip;         
      while(tail->next)      
	tail = tail->next; 
    }          
          
    for(;;) {      
      if (peek_atom() == ATOM_RPAREN) break;   
   
      p = g95_get_interface();        
      p->where = *g95_current_locus();       
      mio_symbol_ref(&p->sym);     
     
      if (tail == NULL)         
	*ip = p; 
      else    
	tail->next = p;     
     
      tail = p;     
    }       
  } 
 
  mio_rparen();     
}    
    
    
          
          
/* get_unique_symtree()-- Return a symtree node with a name that is
 * guaranteed to be unique within the namespace and corresponds to an
 * illegal fortran name */  
  
static g95_symtree *get_unique_symtree(g95_namespace *ns) {       
char name[G95_MAX_SYMBOL_LEN+1];         
static int serial=0;  
  
  sprintf(name, "@%d", serial++); 
  return g95_new_symtree(&ns->sym_root, name);   
}  
  
  


/* mio_component_ref()-- Save and load references to components that
 * occur within expressions.  We have to describe these references by
 * a number and by name.  The number is necessary for forward
 * references during reading, and the name is necessary if the symbol
 * already exists in the namespace and is not loaded again. */       
       
static void mio_component_ref(g95_component **cp, g95_symbol *s) {      
char name[G95_MAX_SYMBOL_LEN+1];  
g95_component *m;       
pointer_info *n; 
 
  n = mio_pointer_ref(cp);       
  if (n->type == P_UNKNOWN) n->type = P_COMPONENT;       
       
  if (iomode == IO_OUTPUT) 
    mio_internal_string((*cp)->name);         
  else {
    mio_internal_string(name);         
         
    if (s->components != NULL && n->u.pointer == NULL) {   
      /* Symbol already loaded, so search by name */      
      
      for(m=s->components; m; m=m->next)    
	if (strcmp(m->name, name) == 0) break;      
      
      if (m == NULL)       
	g95_internal_error("mio_component_ref(): Component not found"); 
 
      associate_integer_pointer(n, m);       
    }

    /* Make sure this symbol will eventually be loaded */ 
 
    n = find_pointer2(s);      
    if (n->u.rsym.state == UNUSED) n->u.rsym.state = NEEDED;    
  }         
}


   
   
static void mio_array_spec(g95_array_spec **asp) {         
g95_array_spec *as;       
int f;   
   
  mio_lparen();        
        
  if (iomode == IO_OUTPUT) {       
    if (*asp == NULL) goto done;     
    as = *asp; 
  } else {  
    if (peek_atom() == ATOM_RPAREN) {    
      *asp = NULL;    
      goto done;   
    }    
    
    *asp = as = g95_get_array_spec();     
  }      
      
  mio_integer(&as->rank);      
  as->type = mio_name(as->type, array_spec_types);   
   
  for(f=0; f<as->rank; f++) {   
    mio_expr(&as->lower[f]);          
    mio_expr(&as->upper[f]);          
  }        
        
done: 
  mio_rparen(); 
}       
       
       
  
  
static void mio_namespace_ref(g95_namespace **nsp) {      
g95_namespace *ns;    
pointer_info *a;  
  
  a = mio_pointer_ref(nsp);          
          
  if (a->type == P_UNKNOWN) a->type = P_NAMESPACE;   
   
  if (iomode == IO_INPUT && a->integer != 0 && a->u.pointer == NULL) { 
    ns = g95_get_namespace(NULL);          
    associate_integer_pointer(a, ns);   
  }   
} 
 
 
  
  
/* mio_interface()-- Save/restore a nameless operator interface */    
    
static void mio_interface(g95_interface **ip) {     
     
  mio_lparen(); 
  mio_interface_rest(ip);
}         
         
         
        
        
static void mio_typespec(g95_typespec *ts) {

  mio_lparen();   
     
  ts->type = mio_name(ts->type, bt_types);     
     
  if (ts->type != BT_DERIVED)         
    mio_integer(&ts->kind); 
  else        
    mio_symbol_ref(&ts->derived);  
  
  mio_charlen(&ts->cl);      
      
  mio_rparen();      
}         
         
         
 
 
static void mio_component(g95_component *w) {  
pointer_info *e;    
int y;

  mio_lparen();         
         
  if (iomode == IO_OUTPUT) { 
    e = get_pointer(w);
    mio_integer(&e->integer);
  } else {   
    mio_integer(&y);          
    e = get_integer(y);         
    associate_integer_pointer(e, w); 
  }      
      
  if (e->type == P_UNKNOWN) e->type = P_COMPONENT;      
      
  mio_internal_string(w->name);   
  mio_typespec(&w->ts);     
  mio_array_spec(&w->as);       
       
  mio_integer(&w->dimension);  
  mio_integer(&w->pointer);    
    
  mio_expr(&w->initializer);  
  mio_rparen();
}         
         
         
     
     
static void mio_component_list(g95_component **cp) {       
g95_component *j, *tail;   
   
  mio_lparen(); 
 
  if (iomode == IO_OUTPUT) {  
    for(j=*cp; j; j=j->next)          
      mio_component(j);         
  } else {

    *cp = NULL;          
    tail = NULL;     
     
    for(;;) {   
      if (peek_atom() == ATOM_RPAREN) break;

      j = g95_get_component();     
      mio_component(j);      
      
      if (tail == NULL)      
	*cp = j;         
      else      
	tail->next = j; 
 
      tail = j;         
    }    
  }

  mio_rparen();       
}        
        
        
  
  
/* mio_actual_arg()-- Save/restore an actual argument.  The argument
 * can't be part of a subroutine call, so we don't have to worry about
 * alternate return specs. */   
   
static void mio_actual_arg(g95_actual_arglist *w) { 
 
  mio_lparen();   
  mio_internal_string(w->name);

  mio_integer((int *) &w->type);  
  mio_expr(&w->u.expr);  
  
  mio_rparen();    
}         
         
         
   
   
/* mio_symbol_ref()-- Save or restore a reference to a symbol node */         
         
void mio_symbol_ref(g95_symbol **symp) {        
pointer_info *r;  
  
  r = mio_pointer_ref(symp);         
  if (r->type == P_UNKNOWN) r->type = P_SYMBOL;      
      
  if (iomode == IO_OUTPUT) {  
    if (r->u.wsym.state == UNREFERENCED) r->u.wsym.state = NEEDS_WRITE;
  } else {        
    if (r->u.rsym.state == UNUSED) r->u.rsym.state = NEEDED;          
  } 
}     
     
     
       
       
/* mio_symbol()-- Unlike most other routines, the address of the
 * symbol node is already fixed on input and the name/module has
 * already been filled in. */         
         
static void mio_symbol(g95_symbol *symbol) {      
      
  mio_lparen();   
   
  mio_symbol_attribute(&symbol->attr);        
  if (iomode == IO_INPUT && symbol->attr.flavor == FL_DERIVED) symbol->attr.set = 1;  
  
  mio_typespec(&symbol->ts);
  mio_namespace_ref(&symbol->formal_ns);          
          
  mio_symbol_ref(&symbol->common_head);  /* Save/restore common block links */         
  mio_symbol_ref(&symbol->common_next);  
  
  mio_formal_arglist(symbol);        
  mio_expr(&symbol->value);         
         
  mio_array_spec(&symbol->as);        
        
  if (iomode == IO_INPUT && symbol->attr.function) symbol->result = symbol;

/* Note that components are always saved, even if they are supposed
 * to be private.  Component access is checked during searching */

  mio_component_list(&symbol->components);   
   
  if (symbol->components != NULL)         
    symbol->component_access = mio_name(symbol->component_access, access_types);         
         
  mio_symbol_ref(&symbol->common_head);        
  mio_symbol_ref(&symbol->common_next);       
       
  mio_rparen();   
}    
    
    
     
     
static void mio_ref(g95_ref **rp) {
g95_ref *k;   
   
  mio_lparen();

  k = *rp;           
  k->type = mio_name(k->type, ref_types);      
      
  switch(k->type) { 
  case REF_ARRAY:          
    mio_array_ref(&k->u.ar);  
    break;

  case REF_COMPONENT:        
    mio_symbol_ref(&k->u.c.sym);      
    mio_component_ref(&k->u.c.component, k->u.c.sym); 
    break;         
     
  case REF_SUBSTRING:       
    mio_expr(&k->u.ss.start);         
    mio_expr(&k->u.ss.end);     
    mio_charlen(&k->u.ss.length);         
    break;    
  }    
    
  mio_rparen();          
}          
          
          
    
    
static void mio_ref_list(g95_ref **rp) {   
g95_ref *r, *head, *tail;       
       
  mio_lparen();

  if (iomode == IO_OUTPUT) {         
    for(r=*rp; r; r=r->next)  
      mio_ref(&r);          
  } else {      
    head = tail = NULL;        
        
    while(peek_atom() != ATOM_RPAREN) {        
      if (head == NULL)         
	head = tail = g95_get_ref();   
      else {  
	tail->next = g95_get_ref();
	tail = tail->next;     
      }  
  
      mio_ref(&tail);        
    }      
      
    *rp = head;    
  }   
   
  mio_rparen(); 
}       
       
       
       
       
/* load_generic_interfaces()-- Load interfaces from the module.
 * Interfaces are unusual in that they attach themselves to existing
 * symbols.  */  
  
static void load_generic_interfaces(void) {     
char *f, name[G95_MAX_SYMBOL_LEN+1], module[G95_MAX_SYMBOL_LEN+1];
g95_symbol *symb;        
        
  mio_lparen();   
   
  while(peek_atom() != ATOM_RPAREN) {          
    mio_lparen(); 
 
    mio_internal_string(name);       
    mio_internal_string(module);  
  
    /* Decide if we need to load this one or not */       
       
    f = find_use_name(name);          
          
    if (f == NULL || g95_find_symbol(f, NULL, 0, &symb)) {
      while(parse_atom() != ATOM_RPAREN);          
      continue;    
    }          
          
    if (symb == NULL) {    
      g95_get_symbol(f, NULL, &symb);  
  
      symb->attr.flavor = FL_PROCEDURE;          
      symb->attr.generic = 1;    
      symb->attr.use_assoc = 1;       
    }          
          
    mio_interface_rest(&symb->generic);       
  }     
     
  mio_rparen();   
}   
   
   
  
  
/* load_needed()-- Recursive function to traverse the pointer_info
 * tree and load a needed symbol.  We return nonzero if we load a
 * symbol and stop the traversal, because the act of loading can alter
 * the tree. */         
         
static int load_needed(pointer_info *w) {          
g95_namespace *ns;        
pointer_info *i;
g95_symbol *symbol;      
      
  if (w == NULL) return 0;         
  if (load_needed(w->left)) return 1; 
  if (load_needed(w->right)) return 1; 
 
  if (w->type != P_SYMBOL || w->u.rsym.state != NEEDED) return 0;    
    
  w->u.rsym.state = USED;   
   
  set_module_locus(&w->u.rsym.where);          
          
  symbol = w->u.rsym.sym; 
  if (symbol == NULL) {      
    i = get_integer(w->u.rsym.ns);   
   
    ns = (g95_namespace *) i->u.pointer;       
    if (ns == NULL) {    
    
      /* Create an interface namespace if necessary.  These are the
       * namespaces that hold the formal parameters of module procedures. */     
     
      ns = g95_get_namespace(NULL);       
      associate_integer_pointer(i, ns);    
    
      ns->sibling = g95_current_ns->contained;          
      g95_current_ns->contained = ns;         
    }    
    
    symbol = g95_new_symbol(w->u.rsym.true_name, ns);   
    strcpy(symbol->module, w->u.rsym.module);          
          
    associate_integer_pointer(w, symbol);       
  }    
    
  mio_symbol(symbol);         
  symbol->attr.use_assoc = 1; 
 
  return 1; 
}    
    
    
         
         
/* write_symbol()-- Write a symbol to the module. */     
     
static void write_symbol(int t, g95_symbol *s) {       
       
  if (s->attr.flavor == FL_UNKNOWN || s->attr.flavor == FL_LABEL)  
    g95_internal_error("write_symbol(): bad module symbol '%s'", s->name);      
      
  mio_integer(&t);      
  mio_internal_string(s->name);  
  
  mio_internal_string(s->module); 
  mio_pointer_ref(&s->ns);        
        
  mio_symbol(s);      
  write_char('\n');     
} 
 
 
     
     
static void mio_iterator(g95_iterator **ip) {        
g95_iterator *iter;

  mio_lparen();    
    
  if (iomode == IO_OUTPUT) {      
    if (*ip == NULL) goto done;   
  } else {        
    if (peek_atom() == ATOM_RPAREN) {    
      *ip = NULL;       
      goto done;      
    }        
        
    *ip = g95_get_iterator();   
  }        
        
  iter = *ip;       
       
  mio_expr(&iter->var); 
  mio_expr(&iter->start);   
  mio_expr(&iter->end);    
  mio_expr(&iter->step);    
    
done:
  mio_rparen();     
}   
   
   
 
 
static void mio_actual_arglist(g95_actual_arglist **ap) {    
g95_actual_arglist *p, *tail;     
     
  mio_lparen();          
          
  if (iomode == IO_OUTPUT) {       
    for(p=*ap; p; p=p->next)        
      mio_actual_arg(p);     
     
  } else { 
    tail = NULL;          
          
    for(;;) {       
      if (peek_atom() != ATOM_LPAREN) break;        
        
      p = g95_get_actual_arglist();       
       
      if (tail == NULL)       
	*ap = p;      
      else      
	tail->next = p;       
       
      tail = p;          
      mio_actual_arg(p);        
    }     
  }       
       
  mio_rparen();         
}   
   
   
     
     
/* load_operator_interfaces()-- Load operator interfaces from the
 * module.  Interfaces are unusual in that they attach themselves to
 * existing symbols. */       
       
static void load_operator_interfaces(void) {
char *i, name[G95_MAX_SYMBOL_LEN+1], module[G95_MAX_SYMBOL_LEN+1]; 
g95_user_op *uop;

  mio_lparen(); 
 
  while(peek_atom() != ATOM_RPAREN) {
    mio_lparen(); 
 
    mio_internal_string(name);    
    mio_internal_string(module);

    /* Decide if we need to load this one or not */

    i = find_use_name(name);   
    if (i == NULL) {          
      while(parse_atom() != ATOM_RPAREN);
    } else {    
      uop = g95_get_uop(i);      
      mio_interface_rest(&uop->operator);      
    }         
  }     
     
  mio_rparen(); 
}         
         
         
 
 
static void mio_constructor(g95_constructor **cp) {     
g95_constructor *e, *tail;  
  
  mio_lparen();

  if (iomode == IO_OUTPUT) {         
    for(e=*cp; e; e=e->next) { 
      mio_lparen();  
      mio_expr(&e->expr);    
      mio_iterator(&e->iterator);         
      mio_rparen();   
    }         
  } else {    
    
    *cp = NULL;       
    tail = NULL;

    while(peek_atom() != ATOM_RPAREN) {   
      e = g95_get_constructor();   
   
      if (tail == NULL)         
	*cp = e;      
      else 
	tail->next = e;    
    
      tail = e;         
         
      mio_lparen();   
      mio_expr(&e->expr);   
      mio_iterator(&e->iterator);          
      mio_rparen();   
    }       
  }       
       
  mio_rparen(); 
}  
  
  
 
 
/* mio_shape()-- Save and restore the shape of an array constructor. */    
    
static void mio_shape(mpz_t **pshape, int rank) {      
mpz_t *shape;       
atom_type f;       
int g; 
 
  /* A NULL shape is represented by ().  */       
  mio_lparen (); 
 
  if (iomode == IO_OUTPUT) {     
    shape = *pshape;     
    if (!shape) {     
      mio_rparen();    
      return;       
    }          
  } else {   
    f = peek_atom(); 
    if (f == ATOM_RPAREN) {   
      *pshape = NULL;  
      mio_rparen();        
      return;      
    }       
       
    shape = g95_get_shape(rank);        
    *pshape = shape;  
  }        
        
  for(g=0; g<rank; g++)  
    mio_gmp_integer (&shape[g]);        
        
  mio_rparen();    
}     
     
     
     
     
/* write_symbol1()-- Recursive traversal function to write the
 * secondary set of symbols to the module file.  These are symbols
 * that were not public yet are needed by the public symbols or
 * another dependent symbol.  The act of writing a symbol can modify
 * the pointer_info tree, so we cease traversal if we find a symbol to
 * write.  We return nonzero if a symbol was written and pass that
 * information upwards. */

static int write_symbol1(pointer_info *f) {    
    
  if (f == NULL) return 0;    
    
  if (write_symbol1(f->left)) return 1;     
  if (write_symbol1(f->right)) return 1;        
        
  if (f->type != P_SYMBOL || f->u.wsym.state != NEEDS_WRITE) return 0;       
       
  f->u.wsym.state = WRITTEN;    
  write_symbol(f->integer, f->u.wsym.sym);         
         
  return 1; 
}        
        
        
  
  
/* mio_symbol_interface()-- Save/restore a named operator interface */      
      
static void mio_symbol_interface(char *name, char *module,
				 g95_interface **ip) {    
    
  mio_lparen();   
   
  mio_internal_string(name);   
  mio_internal_string(module);    
    
  mio_interface_rest(ip); 
}         
         
         
         
         
/* read_cleanup()-- Recursive function for cleaning up things after a
 * module has been read. */      
      
static void read_cleanup(pointer_info *f) {       
g95_symtree *st;    
pointer_info *h;       
       
  if (f == NULL) return; 
 
  read_cleanup(f->left);     
  read_cleanup(f->right);

  if (f->type == P_SYMBOL && f->u.rsym.state == USED &&          
      !f->u.rsym.referenced) {      
      
    h = get_integer(f->u.rsym.ns);        
    st = get_unique_symtree((g95_namespace *) h->u.pointer);   
   
    st->n.sym = f->u.rsym.sym;   
    st->n.sym->refs++;      
  }

  if (f->type == P_SYMBOL && f->u.rsym.state == UNUSED)    
    g95_free_symbol(f->u.rsym.sym);  
}         
         
         
        
        
/* read_module()-- Read a module file */

static void read_module(void) {        
module_locus operator_interfaces, user_operators;      
char *h, name[G95_MAX_SYMBOL_LEN+1];    
int o, ambiguous, symbol; 
pointer_info *info;     
g95_use_rename *y;      
g95_symtree *st;    
g95_symbol *symb;     
     
  get_module_locus(&operator_interfaces);  /* Skip these for now */          
  skip_list();        
        
  get_module_locus(&user_operators); 
  skip_list();
  skip_list();       
       
  mio_lparen();     
     
  while(peek_atom()!=ATOM_RPAREN) {  
    require_atom(ATOM_INTEGER);
    info = get_integer(atom_int);         
         
    info->type = P_SYMBOL;          
    info->u.rsym.state = UNUSED;     
     
    mio_internal_string(info->u.rsym.true_name);   
    mio_internal_string(info->u.rsym.module);  
  
    require_atom(ATOM_INTEGER); 
    info->u.rsym.ns = atom_int;    
    
    get_module_locus(&info->u.rsym.where);    
    skip_list();   
   
    /* See if the symbol has already been loaded by a previous module.
     * If so, we reference the existing symbol and prevent it from
     * being loaded again. */

    symb = find_true_name(info->u.rsym.true_name, info->u.rsym.module);          
    if (symb == NULL) continue;    
    
    info->u.rsym.state = USED;         
    info->u.rsym.referenced = 1;         
    info->u.rsym.sym = symb;      
  }   
   
  mio_rparen();

  /* Parse the symtree lists.  This lets us mark which symbols need to
   * be loaded.  Renaming is also done at this point by replacing the
   * symtree name. */     
     
  mio_lparen();

  while(peek_atom() != ATOM_RPAREN) {
    mio_internal_string(name);      
    mio_integer(&ambiguous); 
    mio_integer(&symbol);   
   
    info = get_integer(symbol);      
      
    /* See what we need to do with this name. */       
       
    h = find_use_name(name);     
    if (h == NULL) continue;  
  
    st = g95_find_symtree(g95_current_ns->sym_root, h);        
        
    if (st != NULL) {  
      if (st->n.sym != info->u.rsym.sym) st->ambiguous = 1;         
    } else {     
      st = check_unique_name(h) ? get_unique_symtree(g95_current_ns) :    
	g95_new_symtree(&g95_current_ns->sym_root, h);       
       
      st->ambiguous = ambiguous;

      symb = info->u.rsym.sym;     
     
      if (symb == NULL) {         
	symb = info->u.rsym.sym =         
	  g95_new_symbol(info->u.rsym.true_name, g95_current_ns);         
         
	strcpy(symb->module, info->u.rsym.module);   
      }          
          
      st->n.sym = symb;       
      st->n.sym->refs++; 
 
      if (info->u.rsym.state == UNUSED) info->u.rsym.state = NEEDED;   
      info->u.rsym.referenced = 1;       
    }  
  }       
       
  mio_rparen();      
      
  /* Load intrinsic operator interfaces. */    
    
  set_module_locus(&operator_interfaces);
  mio_lparen();         
         
  for(o=0; o<G95_INTRINSIC_OPS; o++) {      
    if (o == INTRINSIC_USER) continue;        
        
    if (only_flag) { 
      y = find_use_operator(o);          
          
      if (y == NULL) {        
	skip_list();   
	continue;  
      }   
   
      y->found = 1;       
    }      
      
    mio_interface(&g95_current_ns->operator[o]);   
  }       
       
  mio_rparen();   
   
/* Load generic and user operator interfaces.  These must follow the
 * loading of symtree because otherwise symbols can be marked as
 * ambiguous */          
          
  set_module_locus(&user_operators);   
   
  load_operator_interfaces(); 
  load_generic_interfaces();       
       
  /* At this point, we read those symbols that are needed but haven't
   * been loaded yet.  If one symbol requires another, the other gets
   * marked as NEEDED if its previous state was UNUSED. */  
  
  while(load_needed(pi_root));          
          
/* Make sure all elements of the rename-list were found in the module */       
       
  for(y=g95_rename_list; y; y=y->next) {   
    if (y->found) continue;     
     
    if (y->operator == -1) {          
      g95_error("Symbol '%s' referenced at %L not found in module '%s'",      
		y->use_name, &y->where, module_name);      
      continue;    
    } 
        
    if (y->operator == INTRINSIC_USER) {          
      g95_error("User operator '%s' referenced at %L not found in module '%s'",       
		y->use_name, &y->where, module_name);
      continue; 
    }       
       
    g95_error("Intrinsic operator '%s' referenced at %L not found in module "  
	      "'%s'", g95_op2string(y->operator), &y->where, module_name);     
  } 
 
  g95_check_interfaces(g95_current_ns); 
 
/* Clean up symbol nodes that were never loaded, create references to
 * hidden symbols. */        
        
  read_cleanup(pi_root);    
}     
     
     
       
       
/* write_generic()-- Write generic interfaces associated with a symbol. */    
    
static void write_generic(g95_symbol *s) {  
  
  if (s->generic == NULL ||      
      !check_access(s->attr.access, s->ns->default_access)) return;         
         
  mio_symbol_interface(s->name, s->module, &s->generic);     
}        
        
        


/* g95_use_module()-- Process a USE directive. */          
          
void g95_use_module(void) {   
char filename[G95_MAX_SYMBOL_LEN+5];  
g95_state_data *w;       
int a, line;         
         
  strcpy(filename, module_name);      
  strcat(filename, MODULE_EXTENSION);    
    
  module_fp = g95_open_included_file(filename);         
  if (module_fp == NULL)       
    g95_fatal_error("Can't open module file '%s' for reading: %s",    
		    filename, strerror(errno));         
         
  iomode = IO_INPUT;   
  module_line = 1;         
  module_column = 1;

  /* Skip the first two lines of the module */   
   
  line = 0;       
  while(line < 2) {         
    a = module_char();  
    if (a == EOF) bad_module("Unexpected end of module");       
    if (a == '\n') line++;     
  }    
    
  /* Make sure we're not reading the same module that we may be building */         
         
  for(w=g95_state_stack; w; w=w->previous)      
    if (w->state == COMP_MODULE && strcmp(w->sym->name, module_name) == 0)      
      g95_fatal_error("Can't USE the same module we're building!");         
         
  init_pi_tree();   
  init_true_name_tree();  
  
  read_module();   
   
  free_true_name(true_name_root);        
  true_name_root = NULL;    
    
  free_pi_tree(pi_root);        
  pi_root = NULL;     
     
  fclose(module_fp);      
}    
    
    
      
      
/* write_symbol0()-- Recursive traversal function to write the initial
 * set of symbols to the module.  We check to see if the symbol should
 * be written according to the access specification. */          
          
static void write_symbol0(g95_symtree *st) {  
g95_symbol *symbol;     
pointer_info *w;      
      
  if (st == NULL) return;          
          
  write_symbol0(st->left);         
  write_symbol0(st->right);

  symbol = st->n.sym;    
  set_module_name(symbol);     
     
  if (symbol->attr.flavor == FL_PROCEDURE && symbol->attr.generic &&        
      !symbol->attr.subroutine && !symbol->attr.function) return;    
    
  if (!check_access(symbol->attr.access, symbol->ns->default_access)) return;  
   
  w = get_pointer(symbol);     
  if (w->type == P_UNKNOWN) w->type = P_SYMBOL;  
  
  if (w->u.wsym.state == WRITTEN) return;    
    
  write_symbol(w->integer, symbol);   
  w->u.wsym.state = WRITTEN;      
      
  return;       
}


      
      
/* write_operator()-- Write operator interfaces associated with a symbol. */  
  
static void write_operator(g95_user_op *uop) {      
static char nullstring[] = "";     
     
  if (uop->operator == NULL ||         
      !check_access(uop->access, uop->ns->default_access)) return;       
       
  mio_symbol_interface(uop->name, nullstring, &uop->operator);  
}          
          
          
      
      
/* mio_expr()-- Read and write expressions.  The form "()" is allowed
 * to indicate a NULL expression */        
        
static void mio_expr(g95_expr **ep) { 
g95_expr *y;     
atom_type q;     
int flag;

  mio_lparen();     
     
  if (iomode == IO_OUTPUT) { 
    if (*ep == NULL) {
      mio_rparen();   
      return;        
    }     
     
    y = *ep;     
    mio_name(y->type, expr_types);         
         
  } else {       
    q = parse_atom();        
    if (q == ATOM_RPAREN) {
      *ep = NULL;          
      return; 
    }  
  
    if (q != ATOM_NAME) bad_module("Expected expression type"); 
 
    y = *ep = g95_get_expr();          
    y->where = *g95_current_locus();     
    y->type = find_enum(expr_types);
  } 
 
  mio_typespec(&y->ts);   
  mio_integer(&y->rank);        
        
  switch(y->type) {    
  case EXPR_OP:
    y->operator = mio_name(y->operator, intrinsics);     
     
    switch(y->operator) {      
    case INTRINSIC_UPLUS:   case INTRINSIC_UMINUS:  case INTRINSIC_NOT:         
      mio_expr(&y->op1);         
      break;         
         
    case INTRINSIC_PLUS:    case INTRINSIC_MINUS:   case INTRINSIC_TIMES:    
    case INTRINSIC_DIVIDE:  case INTRINSIC_POWER:   case INTRINSIC_CONCAT:         
    case INTRINSIC_AND:     case INTRINSIC_OR:      case INTRINSIC_EQV:          
    case INTRINSIC_NEQV:    case INTRINSIC_EQ:      case INTRINSIC_NE:  
    case INTRINSIC_GT:      case INTRINSIC_GE:      case INTRINSIC_LT:        
    case INTRINSIC_LE:          
      mio_expr(&y->op1); 
      mio_expr(&y->op2); 
      break;          
          
    default:    
      bad_module("Bad operator"); 
    }  
  
    break;        
        
  case EXPR_FUNCTION:  
    mio_symbol_ref(&y->symbol);         
    mio_actual_arglist(&y->value.function.actual);         
         
    if (iomode == IO_OUTPUT) {  
      mio_allocated_string(&y->value.function.name);         
      flag = y->value.function.isym == NULL;       
      mio_integer(&flag);          
      if (flag)          
	mio_symbol_ref(&y->symbol);     
      else       
	write_atom(ATOM_STRING, y->value.function.isym->name);          
          
    } else {  
      require_atom(ATOM_STRING);      
      y->value.function.name = g95_get_string(atom_string);  
      g95_free(atom_string);       
       
      mio_integer(&flag);  
      if (flag)          
	mio_symbol_ref(&y->symbol);   
      else { 
	require_atom(ATOM_STRING); 
	y->value.function.isym = g95_find_function(atom_string);    
	g95_free(atom_string);       
      }
    }     
     
    break;          
          
  case EXPR_VARIABLE:      
    mio_symbol_ref(&y->symbol);    
    mio_ref_list(&y->ref);  
    break;         
         
  case EXPR_SUBSTRING:      
    mio_allocated_string(&y->value.character.string);
    mio_expr(&y->op1);         
    mio_expr(&y->op2); 
    break;    
    
  case EXPR_STRUCTURE:      
    mio_symbol_ref(&y->symbol);     
    /* Fall through */     
     
  case EXPR_ARRAY: 
    mio_constructor(&y->value.constructor);       
    mio_shape(&y->shape, y->rank); 
    break;        
        
  case EXPR_CONSTANT: 
    switch(y->ts.type) {
    case BT_INTEGER:       
      mio_gmp_integer(&y->value.integer);
      break;      
      
    case BT_REAL:          
      mio_gmp_real(&y->value.real); 
      break;    
    
    case BT_COMPLEX: 
      mio_gmp_real(&y->value.complex.r);  
      mio_gmp_real(&y->value.complex.i);   
      break;      
      
    case BT_LOGICAL:          
      mio_integer(&y->value.logical);
      break;   
   
    case BT_CHARACTER:     
      mio_integer(&y->value.character.length); 
      mio_allocated_string(&y->value.character.string);     
      break;         
         
    default:   
      bad_module("Bad type in constant expression");         
    }     
     
    break;  
  
  case EXPR_NULL:   
    break;    
  }  
  
  mio_rparen();     
}    
    
    
         
         
static void write_module(void) {      
int c;  
  
  /* Write the operator interfaces */      
      
  mio_lparen();     
     
  for(c=0; c<G95_INTRINSIC_OPS; c++) {   
    if (c == INTRINSIC_USER) continue;      
      
    mio_interface(check_access(g95_current_ns->operator_access[c],         
			       g95_current_ns->default_access)   
		  ? &g95_current_ns->operator[c] : NULL);      
  }   
   
  mio_rparen();
  write_char('\n');  write_char('\n');  
  
  mio_lparen();       
  g95_traverse_user_op(g95_current_ns, write_operator);          
  mio_rparen(); 
  write_char('\n');  write_char('\n');   
   
  mio_lparen();      
  g95_traverse_ns(g95_current_ns, write_generic);       
  mio_rparen();   
  write_char('\n');  write_char('\n');       
       
  /* Write symbol information.  First we traverse all symbols in the
   * primary namespace, writing those that need to be written.
   * Sometimes writing one symbol will cause another to need to be
   * written.  A list of these symbols ends up on the write stack, and
   * we end by popping the bottom of the stack and writing the symbol
   * until the stack is empty.  */   
   
  mio_lparen();   
   
  write_symbol0(g95_current_ns->sym_root); 
  while(write_symbol1(pi_root));         
         
  mio_rparen();    
    
  write_char('\n');  write_char('\n');   
   
  mio_lparen();         
  g95_traverse_symtree(g95_current_ns, write_symtree);       
  mio_rparen();          
}         
         
         


/* g95_dump_module()-- Given module, dump it to disk.  If there was an
 * error while processing the module, dump_flag will be set to zero
 * and we delete the module file, even if it was already there. */

void g95_dump_module(char *name, int dump_flag) {          
char filename[PATH_MAX], *f;         
g95_file *u;       
time_t now;     
     
  filename[0] = '\0';          
  if (g95_option.module_dir != NULL) strcpy(filename, g95_option.module_dir);         
         
  strcat(filename, name);       
  strcat(filename, MODULE_EXTENSION);    
    
  if (!dump_flag) {    
    unlink(filename);
    return; 
  }          
          
  module_fp = fopen(filename, "w");  
  if (module_fp == NULL)          
    g95_fatal_error("Can't open module file '%s' for writing: %s",  
		    filename, strerror(errno));

  /* Find the top level filename */          
          
  u = g95_current_file;        
  while(u->next)  
    u = u->next;  
  
#ifdef __GLIBC__
  muntrace();        
#endif
    
  now = time(NULL);         
  f = ctime(&now);  /* GLIBC 2.1 has a memory leak here */        
        
#ifdef __GLIBC__
  mtrace();  
#endif
  
  *strchr(f, '\n') = '\0';         
         
  fprintf(module_fp, "G95 module created from %s on %s\n", u->filename, f);    
  fputs("If you edit this, you'll get what you deserve.\n\n", module_fp);

  iomode = IO_OUTPUT;     
  strcpy(module_name, name); 
 
  init_pi_tree();  
  
  write_module();         
         
  free_pi_tree(pi_root);
  pi_root = NULL; 
 
  write_char('\n');   
   
  if (fclose(module_fp))  
    g95_fatal_error("Error writing module file '%s' for writing: %s",        
		    filename, strerror(errno));         
}       
       
       
