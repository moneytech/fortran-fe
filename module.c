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
 * ( ( <common name> <symbol> <saved flag>)
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
  int found, mark, operator;       
  g95_locus where;      
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
       AB_POINTER, AB_SAVE, AB_TARGET, AB_DUMMY, AB_DATA, AB_IN_NAMELIST, 
       AB_IN_COMMON, AB_FUNCTION, AB_SUBROUTINE, AB_SEQUENCE, AB_ELEMENTAL,     
       AB_PURE, AB_RECURSIVE, AB_GENERIC    
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
  minit("ALLOCATABLE", AB_ALLOCATABLE),   minit("DIMENSION",   AB_DIMENSION),       
  minit("EXTERNAL",    AB_EXTERNAL),      minit("INTRINSIC",   AB_INTRINSIC),          
  minit("OPTIONAL",    AB_OPTIONAL),      minit("POINTER",     AB_POINTER), 
  minit("SAVE",        AB_SAVE),          minit("TARGET",      AB_TARGET),   
  minit("DUMMY",       AB_DUMMY),         minit("DATA",        AB_DATA),         
  minit("IN_NAMELIST",   AB_IN_NAMELIST), minit("IN_COMMON",   AB_IN_COMMON),        
  minit("FUNCTION",      AB_FUNCTION),    minit("SUBROUTINE",  AB_SUBROUTINE),        
  minit("SEQUENCE",      AB_SEQUENCE),    minit("ELEMENTAL",   AB_ELEMENTAL),
  minit("PURE",          AB_PURE),        minit("RECURSIVE",   AB_RECURSIVE),          
  minit("GENERIC",       AB_GENERIC),     minit(NULL, -1)      
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
       
       
  
  
/* compare_pointers()-- Compare pointers when searching by pointer.
 * Used when writing a module. */    
    
static int compare_pointers(pointer_info *sn1, pointer_info *sn2) {          
          
  if (sn1->u.pointer < sn2->u.pointer) return -1;      
  if (sn1->u.pointer > sn2->u.pointer) return 1; 
 
  return 0;       
}


     
     
/* compare_true_names()-- Compare two true_name structures. */ 
 
static int compare_true_names(true_name *u, true_name *x) {  
int h;    
    
  h = strcmp(u->sym->module, x->sym->module);   
  if (h != 0) return h;      
      
  return strcmp(u->sym->name, x->sym->name);      
}  
  
  
       
       
/* compare_integers()-- Compare integers when searching by integer.
 * Used when reading a module. */     
     
static int compare_integers(pointer_info *sn1, pointer_info *sn2) {          
          
  if (sn1->integer < sn2->integer) return -1;          
  if (sn1->integer > sn2->integer) return 1;          
          
  return 0;
} 
 
 
   
   
/* add_true_name()-- Given a g95_symbol pointer that is not in the
 * true name tree, add it. */     
     
static void add_true_name(g95_symbol *symb) {         
true_name *i;

  i = g95_getmem(sizeof(true_name));     
  i->sym = symb; 
 
  g95_insert_bbt(&true_name_root, i, compare_true_names);          
}


         
         
/* get_integer()-- Given an integer during reading, find it in the
 * pointer_info tree, creating the node if not found. */   
   
static pointer_info *get_integer(int integer) {  
pointer_info *o, n;   
int f; 
 
  n.integer = integer;   
   
  o = pi_root;      
  while(o != NULL) {    
    f = compare_integers(&n, o);     
    if (f == 0) break;    
    
    o = (f < 0) ? o->left : o->right;  
  }         
         
  if (o != NULL) return o;

  o = g95_get_pointer_info();  
  o->integer = integer;       
  o->u.pointer = NULL;          
          
  g95_insert_bbt(&pi_root, o, compare_integers);     
     
  return o;
}      
      
      
      
      
/* free_true_name()-- Recursively free a true name tree node. */          
          
static void free_true_name(true_name *b) {      
      
  if (b == NULL) return;         
  free_true_name(b->left);        
  free_true_name(b->right);   
   
  g95_free(b);          
}          
          
          


/* get_unique_symtree()-- Return a symtree node with a name that is
 * guaranteed to be unique within the namespace and corresponds to an
 * illegal fortran name */     
     
static g95_symtree *get_unique_symtree(g95_namespace *ns) {          
char n[G95_MAX_SYMBOL_LEN+1];  
static int serial=0;   
   
  sprintf(n, "@%d", serial++);           
  return g95_new_symtree(&ns->sym_root, n);    
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
       
       
   
   
/* get_module_locus()-- Get the module's input pointer so that we can
 * restore it later. */

static void get_module_locus(module_locus *h) { 
 
  h->column = module_column; 
  h->line = module_line;   
  fgetpos(module_fp, &h->pos);       
}




/* init_pi_tree()-- Initialize the pointer_info tree. */  
  
static void init_pi_tree(void) {      
int (*compare)(pointer_info *, pointer_info *);  
pointer_info *q;  
  
  pi_root = NULL;    
  compare = (iomode == IO_INPUT) ? compare_integers : compare_pointers;      
      
  /* Pointer 0 is the NULL pointer */         
         
  q = g95_get_pointer_info();       
  q->u.pointer = NULL;          
  q->integer = 0;
  q->type = P_OTHER;   
   
  g95_insert_bbt(&pi_root, q, compare);       
       
  /* Pointer 1 is the current namespace */        
        
  q = g95_get_pointer_info();      
  q->u.pointer = (char *) g95_current_ns;   
  q->integer = 1;    
  q->type = P_NAMESPACE;        
        
  g95_insert_bbt(&pi_root, q, compare);    
    
  symbol_number = 2;   
}     
     
     
    
    
/* associate_integer_pointer()-- Call here during module reading when
 * we know what pointer to associate with an integer.  Any fixups that
 * exist are resolved at this time. */          
          
static void associate_integer_pointer(pointer_info *a, void *gp) {  
fixup_t *d, *y;    
    
  if (a->u.pointer != NULL)          
    g95_internal_error("associate_integer_pointer(): Already associated");        
        
  a->u.pointer = gp;         
         
  for(d=a->fixup; d; d=y) {     
    y = d->next;      
      
    *(d->pointer) = gp;
    g95_free(d);    
  }   
   
  a->fixup = NULL;   
}         
         
         
        
        
/* pointer_to_int()-- During module writing, call here with a pointer
 * to something, returning the pointer_info node. */      
      
static pointer_info *find_pointer(void *gp) {
pointer_info *f;  
char *cp; 
 
  cp = (char *) gp;
  
  f = pi_root;       
  while(f != NULL) {         
    if (f->u.pointer == cp) break;    
    f = (cp < f->u.pointer) ? f->left : f->right;       
  }      
      
  return f;  
}        
        
        
          
          
/* find_true_name()-- Given a true name, search the true name tree to
 * see if it exists within the main namespace. */      
      
static g95_symbol *find_true_name(char *name0, char *modname) {    
true_name k, *d;       
g95_symbol symb;      
int z;    
    
  if (modname[0] == '\0') return NULL; /* Don't match a hidden symbol */       
       
  strcpy(symb.name, name0);   
  strcpy(symb.module, modname);       
  k.sym = &symb;

  d = true_name_root;   
  while(d != NULL) { 
    z = compare_true_names(&k, d);   
    if (z == 0) return d->sym;        
        
    d = (z < 0) ? d->left : d->right;    
  } 
 
  return NULL;
} 
 
 
  
  
void g95_module_init_2(void) {    
    
  last_atom = ATOM_LPAREN;   
}


 
 
/* add_fixup()-- During module reading, given an integer and a pointer
 * to a pointer, either store the pointer from an already-known value
 * or create a fixup structure in order to store things later.
 * Returns zero if the reference has been actually stored, or nonzero
 * if the reference must be fixed later (ie associate_integer_pointer
 * must be called sometime later.  Returns the pointer_info structure. */ 
 
static pointer_info *add_fixup(int integer, void *gp) {        
pointer_info *k;     
fixup_t *j;          
char **cp;     
     
  k = get_integer(integer);         
         
  if (k->integer == 0 || k->u.pointer != NULL) {   
    cp = gp;  
    *cp = k->u.pointer;    
  } else { 
    j = g95_getmem(sizeof(fixup_t));   
   
    j->next = k->fixup;        
    k->fixup = j;     
     
    j->pointer = gp;    
  }   
   
  return k;  
}         
         
         
   
   
void g95_module_done_2(void) {  
  
  free_rename();  
}     
       
       
/* get_pointer()-- Given a pointer while writing, returns the
 * pointer_info tree node, creating it if it doesn't exist. */          
          
static pointer_info *get_pointer(void *gp) {          
pointer_info *t;  
  
  t = find_pointer(gp);         
  if (t != NULL) return t;      
      
  /* Pointer doesn't have an integer.  Give it one. */      
      
  t = g95_get_pointer_info();         
         
  t->u.pointer = gp;     
  t->integer = symbol_number++;

  g95_insert_bbt(&pi_root, t, compare_pointers);      
      
  return t;  
}    
    
    
         
         
/* g95_match_use()-- Match a USE statement */         
         
match g95_match_use(void) {         
char name0[G95_MAX_SYMBOL_LEN+1];         
g95_use_rename *t=NULL, *n;      
interface_type typ;        
int o;  
match x;          
          
  x = g95_match_name(module_name);      
  if (x != MATCH_YES) return x;    
    
  free_rename();  
  only_flag = 0;      
      
  if (g95_match_eos() == MATCH_YES) return MATCH_YES; 
  if (g95_match_char(',') != MATCH_YES) goto syntax;       
       
  if (g95_match(" only :") == MATCH_YES) only_flag = 1;      
      
  if (g95_match_eos() == MATCH_YES) return MATCH_YES;          
          
  for(;;) {   
    n = g95_get_use_rename();          
    n->found = 0;     
     
    if (g95_rename_list == NULL)   
      g95_rename_list = n;          
    else   
      t->next = n;

    t = n;  
  
    n->operator = -1;      
      
    if (g95_match_generic_spec(&typ, name0, &o) == MATCH_ERROR)     
      goto cleanup;          
          
    n->where = g95_current_locus;   
   
    switch(typ) {      
    case INTERFACE_NAMELESS:         
      g95_error("Missing generic specification in USE statement at %C");          
      goto cleanup;

    case INTERFACE_GENERIC:         
      x = g95_match(" =>");     
     
      if (only_flag) { 
	if (x != MATCH_YES)      
	  strcpy(n->use_name, name0);          
	else {   
	  strcpy(n->local_name, name0);

	  x = g95_match_name(n->use_name); 
	  if (x == MATCH_NO) goto syntax; 
	  if (x == MATCH_ERROR) goto cleanup;        
	}         
      } else {  
	if (x != MATCH_YES) goto syntax;       
	strcpy(n->local_name, name0);          
          
	x = g95_match_name(n->use_name);          
	if (x == MATCH_NO) goto syntax;    
	if (x == MATCH_ERROR) goto cleanup;          
      }    
    
      break;         
         
    case INTERFACE_USER_OP:      
      strcpy(n->use_name, name0);         
      /* Fall through */     
     
    case INTERFACE_INTRINSIC_OP:          
      n->operator = o;  
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
  
  
     
     
/* build_tnt()-- Recursive function to build the initial true name
 * tree by recursively traversing the current namespace. */

static void build_tnt(g95_symtree *st1) {    
    
  if (st1 == NULL) return;     
     
  build_tnt(st1->left);        
  build_tnt(st1->right);      
      
  if (find_true_name(st1->n.sym->name, st1->n.sym->module) != NULL) return;

  add_true_name(st1->n.sym);         
}          
          
          
      
      
/* bad_module()-- Report problems with a module.  Error reporting is
 * not very elaborate, since this sorts of errors shouldn't really
 * happen.  This subroutine never returns.  */     
     
static void bad_module(char *msg) {         
char *r;

  switch(iomode) {   
  case IO_INPUT:   r = "Reading";  break;     
  case IO_OUTPUT:  r = "Writing";  break;    
  default:         r = "???";      break;
  }             
         
  fclose(module_fp);         
         
  g95_fatal_error("%s module %s at line %d column %d: %s", r, 
		  module_name, module_line, module_column, msg); 
}          
          
          


/* find_use_operator()-- Try to find the operator in the current list */

static g95_use_rename *find_use_operator(int oper) {
g95_use_rename *o;  
  
  for(o=g95_rename_list; o; o=o->next)   
    if (o->operator == oper) return o;          
          
  return NULL; 
}    
    
    
      
      
/* set_module_locus()-- Set the module's input pointer */       
       
static void set_module_locus(module_locus *z) {   
   
  module_column = z->column; 
  module_line = z->line;      
  fsetpos(module_fp, &z->pos); 
}  
  
  
      
      
/* free_pi_tree()-- Recursively free the tree of pointer structures */       
       
static void free_pi_tree(pointer_info *o) {       
       
  if (o == NULL) return;          
          
  if (o->fixup != NULL) g95_internal_error("free_pi_tree(): Unresolved fixup");       
       
  free_pi_tree(o->left);
  free_pi_tree(o->right);    
    
  g95_free(o);       
}         
         
         


/* module_char()-- Get the next character in the module, updating our
 * reckoning of where we are. */       
       
static int module_char(void) {       
int f;         
         
  f = fgetc(module_fp);    
    
  if (f == EOF) bad_module("Unexpected EOF"); 
 
  if (f == '\n') { 
    module_line++;        
    module_column = 0;        
  }      
      
  module_column++;   
  return f;      
}     
     
     
        
        
/* init_true_name_tree()-- Initialize the true name tree with the
 * current namespace. */   
   
static void init_true_name_tree(void) {       
  true_name_root = NULL;  
  
  build_tnt(g95_current_ns->sym_root);          
}   
   
   
  
  
/* find_enum()-- Given a pointer to an mstring array, require that
 * the current input be one of the strings in the array.  We return
 * the enum value. */

static int find_enum(mstring *x) { 
int h;        
        
  h = g95_string2code(x, atom_name); 
  if (h >= 0) return h;          
          
  bad_module("find_enum(): Enum not found");

  return 0;  /* Not reached */  
}         
         
         
  
  
/* check_unique_name()-- See if a name is a generated name. */         
         
static int check_unique_name(char *n) {          
          
  return *n == '@'; 
}         
         
         
    
    
/* find_use_name()-- Given a name, return the name under which to load
 * this symbol.  Returns NULL there are no more entries with this
 * name. */  
  
static char *find_use_name(char *nam) {
static int first=1; 
g95_use_rename *c;    
int seen;          
char *l;   
   
  seen = 0;   
   
  for(c=g95_rename_list; c; c=c->next) {        
    if (strcmp(c->use_name, nam) != 0) continue;       
    seen = 1; 
 
    if (c->mark) continue;

    c->mark = 1;
    break;      
  }

  if (c != NULL) {      
    c->found = 1;
    l = (c->local_name[0] != '\0') ? c->local_name : nam;   
  } else if (seen || only_flag) {        
    l = NULL;        
  } else        
    l = first ? nam : NULL; 
 
  if (l != NULL)   
    first = 0;  
  else {     
    first = 1;    
    for(c=g95_rename_list; c; c=c->next)   
      c->mark = 0;          
  } 
 
  return l;        
}         
         
         
        
        
/* parse_integer()-- Parse a small integer. */

static void parse_integer(int p) { 
module_locus s;        
        
  atom_int = p - '0';     
     
  for(;;) {       
    get_module_locus(&s);   
   
    p = module_char();        
    if (!isdigit(p)) break; 
 
    atom_int = 10*atom_int + p - '0';          
    if (atom_int > 99999999) bad_module("Integer overflow");     
  }        
        
  set_module_locus(&s);       
}        
        
        
          
          
/* parse_string()-- Parse a string constant.  The delimiter is
 * guaranteed to be a single quote. */   
   
static void parse_string(void) {         
module_locus s;          
int len, i;    
char *t;     
     
  get_module_locus(&s);     
     
  len = 0; 
 
/* See how long the string is */    
    
 loop:
  i = module_char(); 
  if (i == EOF) bad_module("Unexpected end of module in string constant");      
      
  if (i != '\'') {  
    len++;          
    goto loop; 
  }  
  
  i = module_char();      
  if (i == '\'') {     
    len++;  
    goto loop;          
  }   
   
  set_module_locus(&s);    
    
  atom_string = t = g95_getmem(len+1);        
        
  for(;len>0; len--) { 
    i = module_char();    
    if (i == '\'') module_char();  /* Guaranteed to be another \' */      
    *t++ = i; 
  }      
      
  module_char();        /* Terminating \' */ 
  *t = '\0';            /* C-style string for debug purposes */  
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


         
         
/* parse_name()-- Parse a name.  */ 
 
static void parse_name(int i) {  
module_locus b;  
char *r;   
int length;       
       
  r = atom_name;     
     
  *r++ = i;   
  length = 1;      
      
  get_module_locus(&b);  
  
  for(;;) {  
    i = module_char();        
    if (!isalnum(i) && i != '_' && i != '-') break;

    *r++ = i;      
    if (++length > G95_MAX_SYMBOL_LEN) bad_module("Name too long");     
  }

  *r = '\0'; 
 
  fseek(module_fp, -1, SEEK_CUR); 
  module_column = b.column + length - 1;         
         
  if (i == '\n') module_line--;     
}


        
        
/* parse_atom()-- Read the next atom in the module's input stream. */         
         
static atom_type parse_atom(void) {  
int g;     
     
  do {
    g = module_char();    
  } while (g == ' ' || g == '\n');   
   
  switch(g) {     
  case '(':        
    return ATOM_LPAREN;         
         
  case ')':        
    return ATOM_RPAREN;      
      
  case '\'':      
    parse_string();     
    return ATOM_STRING;  
  
  case '0':  case '1':  case '2':  case '3':  case '4':     
  case '5':  case '6':  case '7':  case '8':  case '9':    
    parse_integer(g); 
    return ATOM_INTEGER;     
     
  case 'a': case 'b': case 'c': case 'd': case 'e': case 'f': case 'g':  
  case 'h': case 'i': case 'j': case 'k': case 'l': case 'm': case 'n':        
  case 'o': case 'p': case 'q': case 'r': case 's': case 't': case 'u':     
  case 'v': case 'w': case 'x': case 'y': case 'z': case 'A': case 'B':          
  case 'C': case 'D': case 'E': case 'F': case 'G': case 'H': case 'I': 
  case 'J': case 'K': case 'L': case 'M': case 'N': case 'O': case 'P':    
  case 'Q': case 'R': case 'S': case 'T': case 'U': case 'V': case 'W':   
  case 'X': case 'Y': case 'Z':       
    parse_name(g);   
    return ATOM_NAME;     
     
  default:       
    bad_module("Bad name");          
  }       
       
  return 0;   /* Not reached */
}          
          
          
         
         
/* peek_atom()-- Peek at the next atom on the input */    
    
static atom_type peek_atom(void) {          
module_locus s;   
atom_type q;      
      
  get_module_locus(&s);    
   
  q = parse_atom();       
  if (q == ATOM_STRING) g95_free(atom_string);         
         
  set_module_locus(&s);         
  return q;
} 
 
 
     
     
/* require_atom()-- Read the next atom from the input, requiring that
 * it be a particular kind */ 
 
static void require_atom(atom_type dtype) {    
module_locus w;     
atom_type x;
char *n;      
      
  get_module_locus(&w);          
         
  x = parse_atom(); 
  if (x != dtype) {      
    switch(dtype) {  
    case ATOM_NAME:     n = "Expected name";               break;
    case ATOM_LPAREN:   n = "Expected left parenthesis";   break;    
    case ATOM_RPAREN:   n = "Expected right parenthesis";  break;       
    case ATOM_INTEGER:  n = "Expected integer";            break;  
    case ATOM_STRING:   n = "Expected string";             break;       
    default:           
      g95_internal_error("require_atom(): bad atom type required");   
    }     
     
    set_module_locus(&w);
    bad_module(n);         
  }   
}    
    
    
         
         
/* fp2()-- Recursive function to find a pointer within a tree by brute
 * force. */   
   
static pointer_info *fp2(pointer_info *z, char *target) {     
pointer_info *m;          
          
  if (z == NULL) return NULL;  
 
  if (z->u.pointer == target) return z; 
 
  m = fp2(z->left, target); 
  if (m != NULL) return m; 
 
  return fp2(z->right, target);         
}     
     
     
          
          
/* write_atom()-- Write an atom to a module.  The line wrapping isn't
 * perfect, but it should work most of the time.  This isn't that big
 * of a deal, since the file really isn't meant to be read by people
 * anyway. */ 
 
static void write_atom(atom_type atom, void *t) {          
char buffer[20];  
int c, length;   
char *q;  
  
  switch(atom) {          
  case ATOM_STRING:  
  case ATOM_NAME:      
    q = t;       
    break;  
  
  case ATOM_LPAREN:  
    q = "(";       
    break;      
      
  case ATOM_RPAREN:       
    q = ")";    
    break;   
   
  case ATOM_INTEGER:          
    c = *((int *) t); 
    if (c < 0) g95_internal_error("write_atom(): Writing negative integer");       
       
    sprintf(buffer, "%d", c);      
    q = buffer;
    break;  
      
  default:        
    g95_internal_error("write_atom(): Trying to write dab atom");  
  
  }    
    
  length = strlen(q);         
         
  if (atom != ATOM_RPAREN) {    
    if (module_column + length > 72)  
      write_char('\n');  
    else {

      if (last_atom != ATOM_LPAREN && module_column != 1)          
	write_char(' ');  
    }   
  }

  if (atom == ATOM_STRING) write_char('\'');          
          
  while(*q) { 
    if (atom == ATOM_STRING && *q == '\'') write_char('\'');  
    write_char(*q++);       
  }  
  
  if (atom == ATOM_STRING) write_char('\'');     
     
  last_atom = atom;   
}   
   
   
       
       
static void mio_lparen(void) {     
     
  if (iomode == IO_OUTPUT)   
    write_atom(ATOM_LPAREN, NULL);
  else
    require_atom(ATOM_LPAREN);   
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
       
       
          
          
/* mio_internal_string()-- Read or write a string that is in static
 * memory or inside of some already-allocated structure */      
      
static void mio_internal_string(char *str) {          
          
  if (iomode == IO_OUTPUT)        
    write_atom(ATOM_STRING, str); 
  else {         
    require_atom(ATOM_STRING);          
    strcpy(str, atom_string);      
    g95_free(atom_string);  
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
       
       
 
 
/* mio_gmp_integer()-- Read and write an integer value */    
    
static void mio_gmp_integer(mpz_t *integer) {      
char *v;          
          
  if (iomode == IO_INPUT) {     
    if (parse_atom() != ATOM_STRING) bad_module("Expected integer string"); 
 
    mpz_init(*integer);    
    if (mpz_set_str(*integer, atom_string, 10))        
      bad_module("Error converting integer");      
      
    g95_free(atom_string);     
     
  } else {         
    v = mpz_get_str(NULL, 10, *integer);  
    write_atom(ATOM_STRING, v);   
    g95_free(v);  
  }        
}         
         
         
      
      
/* mio_pointer_ref()-- Saves or restores a pointer.  The pointer is
 * converted back and forth from an integer.  We return the
 * pointer_info pointer so that the caller can take additional action
 * based on the pointer type. */       
       
static pointer_info *mio_pointer_ref(void *gp) {        
pointer_info *x;  
  
  if (iomode == IO_OUTPUT) {    
    x = get_pointer(*((char **) gp));         
    write_atom(ATOM_INTEGER, &x->integer);          
  } else {  
    require_atom(ATOM_INTEGER);    
    x = add_fixup(atom_int, gp);  
  }    
    
  return x;    
}


          
          
static void mio_gmp_real(mpf_t *real) {    
mp_exp_t exponent;        
char *h;        
        
  if (iomode == IO_INPUT) {  
    if (parse_atom() != ATOM_STRING) bad_module("Expected real string");        
        
    mpf_init(*real); 
    mpf_set_str(*real, atom_string, -16);  
    g95_free(atom_string);      
      
  } else {     
    h = mpf_get_str(NULL, &exponent, 16, 0, *real);  
    atom_string = g95_getmem(strlen(h) + 20);    
    
    sprintf(atom_string, "0.%s@%ld", h, exponent); 
    write_atom(ATOM_STRING, atom_string);          
          
    g95_free(atom_string);          
    g95_free(h);      
  }     
}    
    
    
         
         
/* mio_name()-- Read or write an enumerated value.  On writing, we
 * return the input value for the convenience of callers.  We avoid
 * using an integer pointer because enums are sometimes inside bitfields. */  
  
static int mio_name(int e, mstring *s) {

  if (iomode == IO_OUTPUT)      
    write_atom(ATOM_NAME, g95_code2string(s, e)); 
  else {
    require_atom(ATOM_NAME);   
    e = find_enum(s);    
  }  
  
  return e;
}


 
 
static void mio_rparen(void) {      
      
  if (iomode == IO_OUTPUT)       
    write_atom(ATOM_RPAREN, NULL);         
  else
    require_atom(ATOM_RPAREN);          
}


  
  
/* mio_formal_arglist()-- Read and write formal argument lists. */    
    
static void mio_formal_arglist(g95_symbol *sy) {  
g95_formal_arglist *e, *t;  
  
  mio_lparen();          
          
  if (iomode == IO_OUTPUT) {     
    for(e=sy->formal; e; e=e->next)
      mio_symbol_ref(&e->sym);   
   
  } else {          
    sy->formal = t = NULL;      
      
    while(peek_atom() != ATOM_RPAREN) {          
      e = g95_get_formal_arglist();         
      mio_symbol_ref(&e->sym);    
    
      if (sy->formal == NULL)
	sy->formal = e;         
      else       
	t->next = e;     
     
      t = e;        
    }   
  } 
 
  mio_rparen();    
}        
        
        
    
    
/* mio_symbol_attribute()-- Symbol attributes are stored in list with
 * the first three elements being the enumerated fields, while the
 * remaining elements (if any) indicate the individual attribute bits.
 * The access field is not saved-- it controls what symbols are
 * exported when a module is written. */   
   
static void mio_symbol_attribute(symbol_attribute *atr) {          
atom_type k;     
     
  mio_lparen();   
   
  atr->flavor = mio_name(atr->flavor, flavors);        
  atr->intent = mio_name(atr->intent, intents);      
  atr->proc = mio_name(atr->proc, procedures);     
  atr->if_source = mio_name(atr->if_source, ifsrc_types);    
    
  if (iomode == IO_OUTPUT) {
    if (atr->allocatable)   mio_name(AB_ALLOCATABLE, attr_bits);         
    if (atr->dimension)     mio_name(AB_DIMENSION, attr_bits);         
    if (atr->external)      mio_name(AB_EXTERNAL, attr_bits);   
    if (atr->intrinsic)     mio_name(AB_INTRINSIC, attr_bits);     
    if (atr->optional)      mio_name(AB_OPTIONAL, attr_bits);
    if (atr->pointer)       mio_name(AB_POINTER, attr_bits);       
    if (atr->save)          mio_name(AB_SAVE, attr_bits);      
    if (atr->target)        mio_name(AB_TARGET, attr_bits);        
    if (atr->dummy)         mio_name(AB_DUMMY, attr_bits);         
             
    if (atr->data)          mio_name(AB_DATA, attr_bits);
    if (atr->in_namelist)   mio_name(AB_IN_NAMELIST, attr_bits);      
    if (atr->in_common)     mio_name(AB_IN_COMMON, attr_bits);          
          
    if (atr->function)      mio_name(AB_FUNCTION, attr_bits);       
    if (atr->subroutine)    mio_name(AB_SUBROUTINE, attr_bits);     
    if (atr->generic)       mio_name(AB_GENERIC, attr_bits); 
 
    if (atr->sequence)      mio_name(AB_SEQUENCE, attr_bits);     
    if (atr->elemental)     mio_name(AB_ELEMENTAL, attr_bits);     
    if (atr->pure)          mio_name(AB_PURE, attr_bits);        
    if (atr->recursive)     mio_name(AB_RECURSIVE, attr_bits);   
   
    mio_rparen(); 
 
  } else {       
       
    for(;;) {  
      k = parse_atom();      
      if (k == ATOM_RPAREN) break;          
      if (k != ATOM_NAME) bad_module("Expected attribute bit name");

      switch(find_enum(attr_bits)) {      
      case AB_ALLOCATABLE:   atr->allocatable = 1;   break;
      case AB_DIMENSION:     atr->dimension = 1;     break;          
      case AB_EXTERNAL:      atr->external = 1;      break;          
      case AB_INTRINSIC:     atr->intrinsic = 1;     break;     
      case AB_OPTIONAL:      atr->optional = 1;      break;
      case AB_POINTER:       atr->pointer = 1;       break;   
      case AB_SAVE:          atr->save = 1;          break;    
      case AB_TARGET:        atr->target = 1;        break; 
      case AB_DUMMY:         atr->dummy = 1;         break;          
      case AB_DATA:          atr->data = 1;          break;        
      case AB_IN_NAMELIST:   atr->in_namelist = 1;   break;       
      case AB_IN_COMMON:     atr->in_common = 1;     break;       
      case AB_FUNCTION:      atr->function = 1;      break;       
      case AB_SUBROUTINE:    atr->subroutine = 1;    break;      
      case AB_GENERIC:       atr->generic = 1;       break;   
      case AB_SEQUENCE:      atr->sequence = 1;      break;          
      case AB_ELEMENTAL:     atr->elemental = 1;     break;     
      case AB_PURE:          atr->pure = 1;          break; 
      case AB_RECURSIVE:     atr->recursive = 1;     break;     
      }        
    }  
  }        
} 
 
 
 
 
static void mio_integer(int *interp) {

  if (iomode == IO_OUTPUT)          
    write_atom(ATOM_INTEGER, interp);          
  else {     
    require_atom(ATOM_INTEGER);  
    *interp = atom_int; 
  }  
} 
 
 
  
  
static void write_symtree(g95_symtree *st) {  
g95_symbol *symbol;      
pointer_info *m;        
        
  symbol = st->n.sym;         
  if (!check_access(symbol->attr.access, symbol->ns->default_access) ||   
      (symbol->attr.flavor == FL_PROCEDURE && symbol->attr.generic &&          
       !symbol->attr.subroutine && !symbol->attr.function)) return; 
 
  if (check_unique_name(st->name)) return;          
          
  m = find_pointer(symbol);      
  if (m == NULL) g95_internal_error("write_symtree(): Symbol not written");  
  
  mio_internal_string(st->name);          
  mio_integer(&st->ambiguous);        
  mio_integer(&m->integer);        
}


        
        
/* mio_symbol_ref()-- Save or restore a reference to a symbol node */

void mio_symbol_ref(g95_symbol **symp) {          
pointer_info *w;      
      
  w = mio_pointer_ref(symp);      
  if (w->type == P_UNKNOWN) w->type = P_SYMBOL;        
        
  if (iomode == IO_OUTPUT) {
    if (w->u.wsym.state == UNREFERENCED) w->u.wsym.state = NEEDS_WRITE;   
  } else {    
    if (w->u.rsym.state == UNUSED) w->u.rsym.state = NEEDED;         
  } 
}  
  
  
          
          
/* mio_array_ref()-- Given a pointer to an array reference
 * structure (which lives in a g95_ref structure), find the
 * corresponding array specification structure.  Storing the pointer
 * in the ref structure doesn't quite work when loading from a module.
 * Generating code for an array reference also needs more infomation
 * than just the array spec. */   
   
static void mio_array_ref(g95_array_ref *as) {     
int w;        
        
  mio_lparen();          
  as->type = mio_name(as->type, array_ref_types);      
  mio_integer(&as->dimen);          
          
  switch(as->type) {      
  case AR_FULL:          
    break;        
        
  case AR_ELEMENT:         
    for(w=0; w<as->dimen; w++)
      mio_expr(&as->start[w]);     
     
    break;      
      
  case AR_SECTION:     
    for(w=0; w<as->dimen; w++) {
      mio_expr(&as->start[w]);    
      mio_expr(&as->end[w]); 
      mio_expr(&as->stride[w]);
    }   
   
    break;  
  
  case AR_UNKNOWN:      
    g95_internal_error("mio_array_ref(): Unknown array ref"); 
  } 
 
  for(w=0; w<as->dimen; w++)  
    mio_integer((int *) &as->dimen_type[w]);    
    
  if (iomode == IO_INPUT) {    
    for(w=0; w<as->dimen; w++)          
      as->c_where[w] = g95_current_locus;
  }        
        
  mio_rparen();       
}      
      
      
     
     
static void mio_iterator(g95_iterator **ifp) {     
g95_iterator *i; 
 
  mio_lparen();        
        
  if (iomode == IO_OUTPUT) {     
    if (*ifp == NULL) goto done;   
  } else {   
    if (peek_atom() == ATOM_RPAREN) {     
      *ifp = NULL;     
      goto done;  
    }          
          
    *ifp = g95_get_iterator();         
  }        
        
  i = *ifp;

  mio_expr(&i->var);       
  mio_expr(&i->start);  
  mio_expr(&i->end);  
  mio_expr(&i->step);   
   
done:         
  mio_rparen();     
}      
      
      
       
       
static void mio_charlen(g95_charlen **clp) {     
g95_charlen *clen;  
  
  mio_lparen();        
        
  if (iomode == IO_OUTPUT) {
    clen = *clp; 
    if (clen != NULL) mio_expr(&clen->length);
  } else {         
         
    if (peek_atom() != ATOM_RPAREN) {      
      clen = g95_get_charlen();      
      mio_expr(&clen->length);  
  
      *clp = clen;  
  
      clen->next = g95_current_ns->cl_list;    
      g95_current_ns->cl_list = clen;         
    }          
  }  
  
  mio_rparen();      
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
 
 
     
     
static void mio_array_spec(g95_array_spec **asp) {  
g95_array_spec *a;   
int u;     
     
  mio_lparen();        
        
  if (iomode == IO_OUTPUT) {       
    if (*asp == NULL) goto done;       
    a = *asp;    
  } else { 
    if (peek_atom() == ATOM_RPAREN) {     
      *asp = NULL;
      goto done;        
    }        
        
    *asp = a = g95_get_array_spec(); 
  } 
 
  mio_integer(&a->rank);    
  a->type = mio_name(a->type, array_spec_types); 
 
  for(u=0; u<a->rank; u++) {  
    mio_expr(&a->lower[u]); 
    mio_expr(&a->upper[u]);      
  }      
      
done:  
  mio_rparen();  
}    
    
    
 
 
/* mio_actual_arg()-- Save/restore an actual argument.  The argument
 * can't be part of a subroutine call, so we don't have to worry about
 * alternate return specs. */        
        
static void mio_actual_arg(g95_actual_arglist *o) {     
     
  mio_lparen();       
  mio_internal_string(o->name);     
     
  mio_integer((int *) &o->type);          
  mio_expr(&o->u.expr);     
     
  mio_rparen();        
}  
  
  
     
     
static void mio_actual_arglist(g95_actual_arglist **actual) { 
g95_actual_arglist *w, *end;        
        
  mio_lparen();    
    
  if (iomode == IO_OUTPUT) {          
    for(w=*actual; w; w=w->next)          
      mio_actual_arg(w);   
   
  } else {       
    end = NULL; 
 
    for(;;) {       
      if (peek_atom() != ATOM_LPAREN) break;     
     
      w = g95_get_actual_arglist();     
     
      if (end == NULL)      
	*actual = w;  
      else     
	end->next = w;    
    
      end = w;     
      mio_actual_arg(w);   
    }          
  }     
     
  mio_rparen(); 
}         
         
         
       
       
static void mio_component(g95_component *g) {    
pointer_info *v;      
int f;    
    
  mio_lparen();  
  
  if (iomode == IO_OUTPUT) {       
    v = get_pointer(g);   
    mio_integer(&v->integer);         
  } else { 
    mio_integer(&f);     
    v = get_integer(f);      
    associate_integer_pointer(v, g);         
  }       
       
  if (v->type == P_UNKNOWN) v->type = P_COMPONENT;       
       
  mio_internal_string(g->name);      
  mio_typespec(&g->ts);          
  mio_array_spec(&g->as);       
       
  mio_integer(&g->dimension);         
  mio_integer(&g->pointer);

  mio_expr(&g->initializer);
  mio_rparen();        
}       
       
       
       
       
/* read_cleanup()-- Recursive function for cleaning up things after a
 * module has been read. */     
     
static void read_cleanup(pointer_info *k) {
g95_symtree *st;   
pointer_info *v;     
     
  if (k == NULL) return;

  read_cleanup(k->left);      
  read_cleanup(k->right);        
        
  if (k->type == P_SYMBOL && k->u.rsym.state == USED &&       
      !k->u.rsym.referenced) {   
   
    v = get_integer(k->u.rsym.ns);
    st = get_unique_symtree((g95_namespace *) v->u.pointer);      
      
    st->n.sym = k->u.rsym.sym;    
    st->n.sym->refs++;         
  }      
      
  if (k->type == P_SYMBOL && k->u.rsym.state == UNUSED)        
    g95_free_symbol(k->u.rsym.sym);     
}          
          
          
        
        
/* mio_shape()-- Save and restore the shape of an array constructor. */          
          
static void mio_shape(mpz_t **pshape, int rnk) {        
mpz_t *shape;         
atom_type s;      
int c;         
         
  /* A NULL shape is represented by ().  */        
  mio_lparen ();       
       
  if (iomode == IO_OUTPUT) {
    shape = *pshape; 
    if (!shape) {    
      mio_rparen();         
      return;       
    }      
  } else {      
    s = peek_atom();       
    if (s == ATOM_RPAREN) {       
      *pshape = NULL;          
      mio_rparen();    
      return; 
    }         
         
    shape = g95_get_shape(rnk);       
    *pshape = shape; 
  }        
        
  for(c=0; c<rnk; c++)      
    mio_gmp_integer (&shape[c]);        
        
  mio_rparen();      
}        
        
        
         
         
static void mio_component_list(g95_component **cp) {        
g95_component *l, *end;         
         
  mio_lparen();          
          
  if (iomode == IO_OUTPUT) {      
    for(l=*cp; l; l=l->next)      
      mio_component(l);  
  } else {         
         
    *cp = NULL;    
    end = NULL;        
        
    for(;;) {       
      if (peek_atom() == ATOM_RPAREN) break;      
      
      l = g95_get_component();       
      mio_component(l); 
 
      if (end == NULL)         
	*cp = l;          
      else         
	end->next = l;          
          
      end = l;          
    }   
  }    
    
  mio_rparen();
}         
         
         
          
          
static void mio_ref(g95_ref **rp) {
g95_ref *a;        
        
  mio_lparen();

  a = *rp;      
  a->type = mio_name(a->type, ref_types);    
    
  switch(a->type) {  
  case REF_ARRAY:
    mio_array_ref(&a->u.ar);          
    break;   
   
  case REF_COMPONENT:        
    mio_symbol_ref(&a->u.c.sym); 
    mio_internal_string(a->u.c.name);     
     
    /* For input, r->u.c.component pointer fixed during resolution */
    break; 
 
  case REF_SUBSTRING:  
    mio_expr(&a->u.ss.start);       
    mio_expr(&a->u.ss.end);    
    mio_charlen(&a->u.ss.length);         
    break;          
  }      
      
  mio_rparen();        
}       
       
       
  
  
/* mio_interface_rest()-- Save/restore lists of g95_interface
 * stuctures.  When loading an interface, we are really appending to
 * the existing list of interfaces.  Checking for duplicate and
 * ambiguous interfaces has to be done later when all symbols have
 * been loaded */

static void mio_interface_rest(g95_interface **i) {          
g95_interface *tail, *b;  
  
  if (iomode == IO_OUTPUT) { 
    if (i != NULL)  
      for(b=*i; b; b=b->next)          
	mio_symbol_ref(&b->sym);          
  } else {   
   
    if (*i == NULL) 
      tail = NULL;  
    else {    
      tail = *i;  
      while(tail->next)     
	tail = tail->next;   
    }  
  
    for(;;) {  
      if (peek_atom() == ATOM_RPAREN) break;      
      
      b = g95_get_interface();        
      b->where = g95_current_locus;      
      mio_symbol_ref(&b->sym);          
          
      if (tail == NULL)   
	*i = b;        
      else      
	tail->next = b;   
   
      tail = b;       
    } 
  }         
         
  mio_rparen();  
}    
    
    
         
         
static void mio_namespace_ref(g95_namespace **nsp) {  
g95_namespace *namesp;          
pointer_info *c;  
  
  c = mio_pointer_ref(nsp); 
 
  if (c->type == P_UNKNOWN) c->type = P_NAMESPACE;  
  
  if (iomode == IO_INPUT && c->integer != 0 && c->u.pointer == NULL) {         
    namesp = g95_get_namespace(NULL, 0);          
    associate_integer_pointer(c, namesp);   
  }   
}  
  
  
     
     
/* skip_list()-- Skip a list between balanced left and right parens. */   
   
static void skip_list(void) { 
int lvl;       
      
  lvl = 0;         
  do {          
    switch(parse_atom()) {          
    case ATOM_LPAREN:       
      lvl++;       
      break;   
   
    case ATOM_RPAREN:   
      lvl--;    
      break;          
          
    case ATOM_STRING:        
      g95_free(atom_string);          
      break;         
         
    case ATOM_NAME:         
    case ATOM_INTEGER:      
      break;      
    } 
  } while(lvl > 0);         
}   
   
   
  
  
/* load_generic_interfaces()-- Load interfaces from the module.
 * Interfaces are unusual in that they attach themselves to existing
 * symbols.  */        
        
static void load_generic_interfaces(void) {   
char *v, name[G95_MAX_SYMBOL_LEN+1], modname[G95_MAX_SYMBOL_LEN+1];
g95_symbol *s;       
module_locus t; 
 
  mio_lparen();         
         
  while(peek_atom() != ATOM_RPAREN) {   
    mio_lparen();         
         
    mio_internal_string(name);    
    mio_internal_string(modname);  
  
    get_module_locus(&t);     
     
    for(;;) {          
      v = find_use_name(name);     
      if (v == NULL) break; 
 
      if (g95_find_symbol(v, NULL, 0, &s)) continue;         
         
      if (s == NULL) {      
	g95_get_symbol(v, NULL, &s);

	s->attr.flavor = FL_PROCEDURE;        
	s->attr.generic = 1;   
	s->attr.use_assoc = 1;        
      }   
   
      mio_interface_rest(&s->generic);        
      set_module_locus(&t);  
    }   
   
    while(parse_atom() != ATOM_RPAREN);   
  }        
        
  mio_rparen();         
}


      
      
static void mio_constructor(g95_constructor **cp) {          
g95_constructor *o, *t;      
      
  mio_lparen();          
          
  if (iomode == IO_OUTPUT) {         
    for(o=*cp; o; o=o->next) {   
      mio_lparen();          
      mio_expr(&o->expr);      
      mio_iterator(&o->iterator);      
      mio_rparen();       
    }     
  } else {      
      
    *cp = NULL;
    t = NULL; 
 
    while(peek_atom() != ATOM_RPAREN) {      
      o = g95_get_constructor();  
  
      if (t == NULL)       
	*cp = o;         
      else   
	t->next = o;          
          
      t = o;    
    
      mio_lparen();          
      mio_expr(&o->expr); 
      mio_iterator(&o->iterator);    
      mio_rparen();  
    }      
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


     
     
/* mio_symbol()-- Unlike most other routines, the address of the
 * symbol node is already fixed on input and the name/module has
 * already been filled in. */  
  
static void mio_symbol(g95_symbol *symbol) {        
        
  mio_lparen();        
        
  mio_symbol_attribute(&symbol->attr);  
  if (iomode == IO_INPUT && symbol->attr.flavor == FL_DERIVED) symbol->attr.set = 1;

  mio_typespec(&symbol->ts);  
  mio_namespace_ref(&symbol->formal_ns);        
        
  mio_symbol_ref(&symbol->common_next);  /* Save/restore common block links */  
  
  mio_formal_arglist(symbol);    
  mio_expr(&symbol->value);  
  
  mio_array_spec(&symbol->as);          
          
  if (iomode == IO_INPUT && symbol->attr.function) symbol->result = symbol;  
  
/* Note that components are always saved, even if they are supposed
 * to be private.  Component access is checked during searching */      
      
  mio_component_list(&symbol->components); 
 
  if (symbol->components != NULL)   
    symbol->component_access = mio_name(symbol->component_access, access_types);

  mio_rparen();       
}         
         
         
    
    
/* set_module_name()-- Decide on the module name of this symbol.
 * Procedures that are not module procedures of this module and aren't
 * generic don't get a module name. */  
  
static void set_module_name(g95_symbol *sy) {     
g95_namespace *names;        
        
  if (sy->module[0] != '\0') return;     
     
  if (sy->attr.dummy) return;

  if (sy->attr.flavor == FL_PROCEDURE && sy->attr.proc != PROC_MODULE) {         
    for(names=sy->ns->contained; names; names=names->sibling)        
      if (names->proc_name == sy) break;

    /* At this point, the symbol is an external procedure that does
     * not live in a module.  Symbols without modules are not matched
     * globally by the module read subroutine, but these need to be,
     * even though they are not in a module.  We stick them into a
     * "module" with an otherwise illegal name. */ 
 
    if (names == NULL) {      
      strcpy(sy->module, "(global)");         
      return;       
    }        
  }

  strcpy(sy->module, module_name);
}  
  
  
 
 
/* mio_interface()-- Save/restore a nameless operator interface */      
      
static void mio_interface(g95_interface **i) {        
        
  mio_lparen(); 
  mio_interface_rest(i);         
}       
       
       
     
     
/* write_symbol()-- Write a symbol to the module. */ 
 
static void write_symbol(int d, g95_symbol *symbol) {          
          
  if (symbol->attr.flavor == FL_UNKNOWN || symbol->attr.flavor == FL_LABEL)        
    g95_internal_error("write_symbol(): bad module symbol '%s'", symbol->name); 
 
  mio_integer(&d);    
  mio_internal_string(symbol->name);        
        
  mio_internal_string(symbol->module);     
  mio_pointer_ref(&symbol->ns);        
        
  mio_symbol(symbol);          
  write_char('\n');         
}        
        
        


/* mio_symbol_interface()-- Save/restore a named operator interface */      
      
static void mio_symbol_interface(char *name0, char *mod,  
				 g95_interface **interp) {  
  
  mio_lparen();    
    
  mio_internal_string(name0);          
  mio_internal_string(mod);    
    
  mio_interface_rest(interp);      
}     
     
     
  
  
/* write_generic()-- Write generic interfaces associated with a symbol. */  
  
static void write_generic(g95_symbol *sy) {         
         
  if (sy->generic == NULL ||
      !check_access(sy->attr.access, sy->ns->default_access)) return; 
 
  mio_symbol_interface(sy->name, sy->module, &sy->generic);          
}


       
       
/* write_symbol1()-- Recursive traversal function to write the
 * secondary set of symbols to the module file.  These are symbols
 * that were not public yet are needed by the public symbols or
 * another dependent symbol.  The act of writing a symbol can modify
 * the pointer_info tree, so we cease traversal if we find a symbol to
 * write.  We return nonzero if a symbol was written and pass that
 * information upwards. */   
   
static int write_symbol1(pointer_info *h) {      
      
  if (h == NULL) return 0; 
 
  if (write_symbol1(h->left)) return 1;       
  if (write_symbol1(h->right)) return 1;   
   
  if (h->type != P_SYMBOL || h->u.wsym.state != NEEDS_WRITE) return 0;    
    
  h->u.wsym.state = WRITTEN;  
  write_symbol(h->integer, h->u.wsym.sym);      
      
  return 1;       
}


    
    
/* write_common()-- Write a common block to the module */        
        
static void write_common(g95_symtree *st) {         
g95_common_head *w;  
  
  if (st == NULL) return;  
  
  write_common(st->left);        
  write_common(st->right);

  mio_lparen();         
  mio_internal_string(st->name);    
    
  w = st->n.common;   
  mio_symbol_ref(&w->head);        
  mio_integer(&w->saved);      
      
  mio_rparen();          
}    
    
    
   
   
/* mio_expr()-- Read and write expressions.  The form "()" is allowed
 * to indicate a NULL expression */        
        
static void mio_expr(g95_expr **ep) {    
g95_expr *a;  
atom_type l;  
int flag; 
 
  mio_lparen();      
      
  if (iomode == IO_OUTPUT) {   
    if (*ep == NULL) {         
      mio_rparen();         
      return;      
    }        
        
    a = *ep;      
    mio_name(a->type, expr_types);

  } else {         
    l = parse_atom();      
    if (l == ATOM_RPAREN) {       
      *ep = NULL;    
      return;  
    }        
        
    if (l != ATOM_NAME) bad_module("Expected expression type");        
        
    a = *ep = g95_get_expr();     
    a->where = g95_current_locus;   
    a->type = find_enum(expr_types);       
  }          
          
  mio_typespec(&a->ts);          
  mio_integer(&a->rank);        
        
  switch(a->type) {          
  case EXPR_OP:  
    a->operator = mio_name(a->operator, intrinsics);       
       
    switch(a->operator) {
    case INTRINSIC_UPLUS:   case INTRINSIC_UMINUS:  case INTRINSIC_NOT:     
      mio_expr(&a->op1);        
      break;       
       
    case INTRINSIC_PLUS:    case INTRINSIC_MINUS:   case INTRINSIC_TIMES:  
    case INTRINSIC_DIVIDE:  case INTRINSIC_POWER:   case INTRINSIC_CONCAT:      
    case INTRINSIC_AND:     case INTRINSIC_OR:      case INTRINSIC_EQV:        
    case INTRINSIC_NEQV:    case INTRINSIC_EQ:      case INTRINSIC_NE:  
    case INTRINSIC_GT:      case INTRINSIC_GE:      case INTRINSIC_LT:      
    case INTRINSIC_LE:        
      mio_expr(&a->op1);         
      mio_expr(&a->op2);     
      break;    
    
    default:  
      bad_module("Bad operator"); 
    }       
       
    break; 
 
  case EXPR_FUNCTION:  
    mio_symbol_ref(&a->symbol);         
    mio_actual_arglist(&a->value.function.actual);       
       
    if (iomode == IO_OUTPUT) {        
      mio_allocated_string(&a->value.function.name); 
      flag = a->value.function.isym == NULL;       
      mio_integer(&flag);     
      if (flag)
	mio_symbol_ref(&a->symbol);          
      else  
	write_atom(ATOM_STRING, a->value.function.isym->name);  
  
    } else {        
      require_atom(ATOM_STRING);         
      a->value.function.name = g95_get_string(atom_string);    
      g95_free(atom_string);    
    
      mio_integer(&flag);     
      if (flag) 
	mio_symbol_ref(&a->symbol);  
      else {     
	require_atom(ATOM_STRING);       
	a->value.function.isym = g95_find_function(atom_string);     
	g95_free(atom_string);      
      }     
    }

    break;          
          
  case EXPR_VARIABLE:
    mio_symbol_ref(&a->symbol);   
    mio_ref_list(&a->ref);     
    break; 
 
  case EXPR_SUBSTRING: 
    mio_allocated_string(&a->value.character.string);
    mio_ref_list(&a->ref);       
    break;        
        
  case EXPR_STRUCTURE:   
    mio_symbol_ref(&a->symbol);   
    /* Fall through */      
      
  case EXPR_ARRAY:          
    mio_constructor(&a->value.constructor);       
    mio_shape(&a->shape, a->rank);      
    break; 
 
  case EXPR_CONSTANT:          
    switch(a->ts.type) {
    case BT_INTEGER:   
      mio_gmp_integer(&a->value.integer);        
      break;         
         
    case BT_REAL:       
      mio_gmp_real(&a->value.real);
      break;        
        
    case BT_COMPLEX: 
      mio_gmp_real(&a->value.complex.r);   
      mio_gmp_real(&a->value.complex.i);
      break;

    case BT_LOGICAL:     
      mio_integer(&a->value.logical);    
      break;  
  
    case BT_CHARACTER:  
      mio_integer(&a->value.character.length);     
      mio_allocated_string(&a->value.character.string);     
      break;   
   
    default:          
      bad_module("Bad type in constant expression");   
    }

    break;     
     
  case EXPR_NULL:      
    break;

  case EXPR_UNKNOWN:   
    g95_internal_error("mio_expr(): Unknown expression");    
  }     
     
  mio_rparen();         
} 
 
 
  
  
/* write_symbol0()-- Recursive traversal function to write the initial
 * set of symbols to the module.  We check to see if the symbol should
 * be written according to the access specification. */    
    
static void write_symbol0(g95_symtree *s) {  
g95_symbol *symbol;        
pointer_info *r;         
         
  if (s == NULL) return;  
  
  write_symbol0(s->left); 
  write_symbol0(s->right);    
    
  symbol = s->n.sym;     
  set_module_name(symbol);

  if (symbol->attr.flavor == FL_PROCEDURE && symbol->attr.generic &&         
      !symbol->attr.subroutine && !symbol->attr.function) return;       
       
  if (!check_access(symbol->attr.access, symbol->ns->default_access)) return;        
         
  r = get_pointer(symbol);   
  if (r->type == P_UNKNOWN) r->type = P_SYMBOL;     
     
  if (r->u.wsym.state == WRITTEN) return;     
     
  write_symbol(r->integer, symbol);  
  r->u.wsym.state = WRITTEN;   
   
  return;
}  
  
  
   
   
/* write_operator()-- Write operator interfaces associated with a symbol. */          
          
static void write_operator(g95_user_op *u) { 
static char nullstring[] = "";      
      
  if (u->operator == NULL || 
      !check_access(u->access, u->ns->default_access)) return;  
  
  mio_symbol_interface(u->name, nullstring, &u->operator);        
}      
      
      
          
          
/* load_commons()-- Load common blocks */ 
 
static void load_commons(void) { 
char name0[G95_MAX_SYMBOL_LEN+1];        
g95_common_head *u;     
     
  mio_lparen(); 
 
  while(peek_atom() != ATOM_RPAREN) {        
    mio_lparen();   
    mio_internal_string(name0);       
       
    u = g95_get_common(name0);     
     
    mio_symbol_ref(&u->head);        
    mio_integer(&u->saved);          
    u->use_assoc = 1;     
     
    mio_rparen();    
  }          
          
  mio_rparen();          
}

  
  
/* load_operator_interfaces()-- Load operator interfaces from the
 * module.  Interfaces are unusual in that they attach themselves to
 * existing symbols. */

static void load_operator_interfaces(void) {      
char *k, nam[G95_MAX_SYMBOL_LEN+1], modname[G95_MAX_SYMBOL_LEN+1];  
g95_user_op *u;        
module_locus s;  
  
  mio_lparen();       
       
  while(peek_atom() != ATOM_RPAREN) { 
    mio_lparen();        
        
    mio_internal_string(nam);       
    mio_internal_string(modname);      
      
    get_module_locus(&s);       
       
    for(;;) {       
      k = find_use_name(nam);       
      if (k == NULL) break;         
         
      u = g95_get_uop(k);       
      mio_interface_rest(&u->operator);

      set_module_locus(&s);         
    }     
     
    while(parse_atom() != ATOM_RPAREN);      
  }   
   
  mio_rparen();         
}    
    
    


static void write_module(void) {
int f;     
     
  /* Write the operator interfaces */      
      
  mio_lparen();       
       
  for(f=0; f<G95_INTRINSIC_OPS; f++) {        
    if (f == INTRINSIC_USER) continue;

    mio_interface(check_access(g95_current_ns->operator_access[f],    
			       g95_current_ns->default_access)    
		  ? &g95_current_ns->operator[f] : NULL);         
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
  
  mio_lparen();  
  write_common(g95_current_ns->common_root);          
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
    
    
      
      
/* load_needed()-- Recursive function to traverse the pointer_info
 * tree and load a needed symbol.  We return nonzero if we load a
 * symbol and stop the traversal, because the act of loading can alter
 * the tree. */   
   
static int load_needed(pointer_info *w) {
g95_namespace *namesp;   
pointer_info *i;         
g95_symbol *sym;     
     
  if (w == NULL) return 0;
  if (load_needed(w->left)) return 1;  
  if (load_needed(w->right)) return 1; 
 
  if (w->type != P_SYMBOL || w->u.rsym.state != NEEDED) return 0;      
      
  w->u.rsym.state = USED;   
   
  set_module_locus(&w->u.rsym.where);      
      
  sym = w->u.rsym.sym;         
  if (sym == NULL) {  
    i = get_integer(w->u.rsym.ns); 
 
    namesp = (g95_namespace *) i->u.pointer;         
    if (namesp == NULL) {  
  
      /* Create an interface namespace if necessary.  These are the
       * namespaces that hold the formal parameters of module procedures. */        
        
      namesp = g95_get_namespace(NULL, 0);      
      associate_integer_pointer(i, namesp);          
          
      namesp->sibling = g95_current_ns->contained;       
      g95_current_ns->contained = namesp;
    }          
          
    sym = g95_new_symbol(w->u.rsym.true_name, namesp);   
    strcpy(sym->module, w->u.rsym.module);      
      
    associate_integer_pointer(w, sym);      
  }       
       
  mio_symbol(sym);         
  sym->attr.use_assoc = 1;

  return 1;      
}  
  
  
  
  
/* g95_dump_module()-- Given module, dump it to disk.  If there was an
 * error while processing the module, dump_flag will be set to zero
 * and we delete the module file, even if it was already there. */     
     
void g95_dump_module(char *name0, int dump_flag) {        
char filename[PATH_MAX], *t;       
time_t now;    
    
  filename[0] = '\0'; 
  if (g95_option.module_dir != NULL) strcpy(filename, g95_option.module_dir);         
         
  strcat(filename, name0);       
  strcat(filename, MODULE_EXTENSION);

  if (!dump_flag) {         
    unlink(filename); 
    return;  
  }  
  
  module_fp = fopen(filename, "w");       
  if (module_fp == NULL)  
    g95_fatal_error("Can't open module file '%s' for writing: %s",          
		    filename, strerror(errno));

#ifdef __GLIBC__
  muntrace();     
#endif
        
  now = time(NULL);   
  t = ctime(&now);  /* GLIBC 2.1 has a memory leak here */ 
 
#ifdef __GLIBC__
  mtrace();  
#endif

  *strchr(t, '\n') = '\0';      
      
  fprintf(module_fp, "G95 module created from %s on %s\n", g95_source_file, t);    
  fputs("If you edit this, you'll get what you deserve.\n\n", module_fp);         
         
  iomode = IO_OUTPUT;     
  strcpy(module_name, name0);         
         
  init_pi_tree();   
   
  write_module();    
    
  free_pi_tree(pi_root);  
  pi_root = NULL;     
     
  write_char('\n');     
     
  if (fclose(module_fp)) 
    g95_fatal_error("Error writing module file '%s' for writing: %s",         
		    filename, strerror(errno));     
}          
          
          
 
 
/* read_module()-- Read a module file */  
  
static void read_module(void) {
module_locus operator_interfaces, user_operators;         
char *d, name[G95_MAX_SYMBOL_LEN+1];    
int j, ambiguous, symbol;          
pointer_info *info;    
g95_use_rename *m;  
g95_symtree *s;   
g95_symbol *sym; 
 
  get_module_locus(&operator_interfaces);  /* Skip these for now */      
  skip_list();       
       
  get_module_locus(&user_operators);         
  skip_list();        
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
        
    sym = find_true_name(info->u.rsym.true_name, info->u.rsym.module);      
    if (sym == NULL) continue;     
     
    info->u.rsym.state = USED;         
    info->u.rsym.referenced = 1;  
    info->u.rsym.sym = sym;       
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
       
    for(;;) {
      d = find_use_name(name);  
      if (d == NULL) break;

      s = g95_find_symtree(g95_current_ns->sym_root, d);

      if (s != NULL) {       
	if (s->n.sym != info->u.rsym.sym && info->u.rsym.sym != NULL)
	  s->ambiguous = 1;  
      } else {   
	s = check_unique_name(d) ? get_unique_symtree(g95_current_ns) :
	  g95_new_symtree(&g95_current_ns->sym_root, d);        
        
	s->ambiguous = ambiguous;        
        
	sym = info->u.rsym.sym;          
          
	if (sym == NULL) {     
	  sym = info->u.rsym.sym =          
	    g95_new_symbol(info->u.rsym.true_name, g95_current_ns);     
     
	  strcpy(sym->module, info->u.rsym.module);    
	}     
     
	s->n.sym = sym;  
	s->n.sym->refs++;       
       
	if (info->u.rsym.state == UNUSED) info->u.rsym.state = NEEDED;   
	info->u.rsym.referenced = 1;         
      }    
    }         
  }          
          
  mio_rparen();    
    
  /* Load intrinsic operator interfaces. */         
         
  set_module_locus(&operator_interfaces);   
  mio_lparen();       
       
  for(j=0; j<G95_INTRINSIC_OPS; j++) {       
    if (j == INTRINSIC_USER) continue; 
 
    if (only_flag) {       
      m = find_use_operator(j);   
   
      if (m == NULL) {       
	skip_list();      
	continue;         
      }          
          
      m->found = 1;       
    }    
    
    mio_interface(&g95_current_ns->operator[j]); 
  }    
    
  mio_rparen();          
          
/* Load generic and user operator interfaces.  These must follow the
 * loading of symtree because otherwise symbols can be marked as
 * ambiguous */        
        
  set_module_locus(&user_operators);

  load_operator_interfaces();      
  load_generic_interfaces();

  load_commons(); 
 
  /* At this point, we read those symbols that are needed but haven't
   * been loaded yet.  If one symbol requires another, the other gets
   * marked as NEEDED if its previous state was UNUSED. */         
         
  while(load_needed(pi_root));   
   
/* Make sure all elements of the rename-list were found in the module */   
   
  for(m=g95_rename_list; m; m=m->next) {         
    if (m->found) continue;  
  
    if (m->operator == -1) { 
      g95_error("Symbol '%s' referenced at %L not found in module '%s'",        
		m->use_name, &m->where, module_name);   
      continue;          
    }  
         
    if (m->operator == INTRINSIC_USER) {   
      g95_error("User operator '%s' referenced at %L not found in module '%s'", 
		m->use_name, &m->where, module_name);      
      continue;  
    }    
    
    g95_error("Intrinsic operator '%s' referenced at %L not found in module "    
	      "'%s'", g95_op2string(m->operator), &m->where, module_name);      
  }  
  
  g95_check_interfaces(g95_current_ns);

/* Clean up symbol nodes that were never loaded, create references to
 * hidden symbols. */          
          
  read_cleanup(pi_root);      
}   
   
   
 
 
/* g95_use_module()-- Process a USE directive. */        
        
void g95_use_module(void) {  
char filename[G95_MAX_SYMBOL_LEN+5];  
g95_state_data *p;          
int v, line;

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
    v = module_char();      
    if (v == EOF) bad_module("Unexpected end of module");       
    if (v == '\n') line++;        
  }         
         
  /* Make sure we're not reading the same module that we may be building */

  for(p=g95_state_stack; p; p=p->previous)          
    if (p->state == COMP_MODULE && strcmp(p->sym->name, module_name) == 0)   
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
  
  
