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
       AB_POINTER, AB_SAVE, AB_TARGET, AB_DUMMY, AB_ENTRY, AB_DATA,        
       AB_IN_NAMELIST, AB_IN_COMMON, AB_FUNCTION, AB_SUBROUTINE, AB_SEQUENCE,    
       AB_ELEMENTAL, AB_PURE, AB_RECURSIVE, AB_GENERIC         
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
  minit("DUMMY",       AB_DUMMY),       minit("ENTRY",        AB_ENTRY), 
  minit("DATA",        AB_DATA),       minit("IN_NAMELIST",   AB_IN_NAMELIST),        
  minit("IN_COMMON",   AB_IN_COMMON),  minit("FUNCTION",      AB_FUNCTION),         
  minit("SUBROUTINE",  AB_SUBROUTINE), minit("SEQUENCE",      AB_SEQUENCE),   
  minit("ELEMENTAL",   AB_ELEMENTAL),  minit("PURE",          AB_PURE),        
  minit("RECURSIVE",   AB_RECURSIVE),  minit("GENERIC",       AB_GENERIC),     
  minit(NULL, -1)     
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
      
      
      
      
/* free_true_name()-- Recursively free a true name tree node. */ 
 
static void free_true_name(true_name *k) {     
     
  if (k == NULL) return;         
  free_true_name(k->left);         
  free_true_name(k->right);        
        
  g95_free(k);
}  
  
  
        
        
/* compare_integers()-- Compare integers when searching by integer.
 * Used when reading a module. */      
      
static int compare_integers(pointer_info *sn1, pointer_info *sn2) {     
     
  if (sn1->integer < sn2->integer) return -1;          
  if (sn1->integer > sn2->integer) return 1;

  return 0;        
}    
    
    
      
      
/* free_rename()-- Free the rename list left behind by a USE
 * statement. */      
      
static void free_rename(void) { 
g95_use_rename *next1;     
     
  for(;g95_rename_list; g95_rename_list=next1) {      
    next1 = g95_rename_list->next;  
    g95_free(g95_rename_list);      
  }   
} 
 
 


/* find_use_operator()-- Try to find the operator in the current list */ 
 
static g95_use_rename *find_use_operator(int operator) {       
g95_use_rename *q;       
       
  for(q=g95_rename_list; q; q=q->next)    
    if (q->operator == operator) return q;  
  
  return NULL;          
}         
         
         
       
       
/* get_unique_symtree()-- Return a symtree node with a name that is
 * guaranteed to be unique within the namespace and corresponds to an
 * illegal fortran name */ 
 
static g95_symtree *get_unique_symtree(g95_namespace *names) {    
char nm[G95_MAX_SYMBOL_LEN+1];        
static int serial=0;  
  
  sprintf(nm, "@%d", serial++);      
  return g95_new_symtree(&names->sym_root, nm);      
}  
  
  
     
     
/* pointer_to_int()-- During module writing, call here with a pointer
 * to something, returning the pointer_info node. */  
  
static pointer_info *find_pointer(void *gp) {      
pointer_info *g;         
char *cp;       
       
  cp = (char *) gp;  
    
  g = pi_root;
  while(g != NULL) {
    if (g->u.pointer == cp) break;  
    g = (cp < g->u.pointer) ? g->left : g->right;
  }     
     
  return g;       
}   
   
   
  
  
/* fp2()-- Recursive function to find a pointer within a tree by brute
 * force. */         
         
static pointer_info *fp2(pointer_info *r, char *target) {    
pointer_info *a;    
    
  if (r == NULL) return NULL;       
      
  if (r->u.pointer == target) return r; 
 
  a = fp2(r->left, target);    
  if (a != NULL) return a;          
          
  return fp2(r->right, target);  
}          
          
          


/* find_use_name()-- Given a name, return the name under which to load
 * this symbol.  Returns NULL there are no more entries with this
 * name. */        
        
static char *find_use_name(char *name) {  
static int first=1; 
g95_use_rename *t;   
int seen; 
char *d; 
 
  seen = 0;      
      
  for(t=g95_rename_list; t; t=t->next) {   
    if (strcmp(t->use_name, name) != 0) continue;        
    seen = 1;          
          
    if (t->mark) continue;

    t->mark = 1; 
    break;          
  }

  if (t != NULL) {      
    t->found = 1;    
    d = (t->local_name[0] != '\0') ? t->local_name : name; 
  } else if (seen || only_flag) {          
    d = NULL;    
  } else      
    d = first ? name : NULL;       
       
  if (d != NULL) 
    first = 0;
  else {        
    first = 1;
    for(t=g95_rename_list; t; t=t->next)      
      t->mark = 0;
  }        
        
  return d;      
}  
  
  
    
    
/* get_integer()-- Given an integer during reading, find it in the
 * pointer_info tree, creating the node if not found. */         
         
static pointer_info *get_integer(int integer) {         
pointer_info *j, n;    
int e;        
        
  n.integer = integer;  
  
  j = pi_root;
  while(j != NULL) { 
    e = compare_integers(&n, j);         
    if (e == 0) break;    
    
    j = (e < 0) ? j->left : j->right;   
  }       
       
  if (j != NULL) return j;       
       
  j = g95_get_pointer_info();       
  j->integer = integer;        
  j->u.pointer = NULL;        
        
  g95_insert_bbt(&pi_root, j, compare_integers);       
       
  return j;          
}      
      
      
       
       
/* associate_integer_pointer()-- Call here during module reading when
 * we know what pointer to associate with an integer.  Any fixups that
 * exist are resolved at this time. */    
    
static void associate_integer_pointer(pointer_info *c, void *gp) {        
fixup_t *w, *g;      
      
  if (c->u.pointer != NULL)     
    g95_internal_error("associate_integer_pointer(): Already associated");

  c->u.pointer = gp; 
 
  for(w=c->fixup; w; w=g) {        
    g = w->next;       
       
    *(w->pointer) = gp;   
    g95_free(w); 
  }  
  
  c->fixup = NULL;   
}          
          
          
   
   
/* compare_pointers()-- Compare pointers when searching by pointer.
 * Used when writing a module. */

static int compare_pointers(pointer_info *sn1, pointer_info *sn2) {     
     
  if (sn1->u.pointer < sn2->u.pointer) return -1;       
  if (sn1->u.pointer > sn2->u.pointer) return 1;         
         
  return 0;          
}          
          
          
          
          
/* compare_true_names()-- Compare two true_name structures. */  
  
static int compare_true_names(true_name *r, true_name *n) {   
int u;       
       
  u = strcmp(r->sym->module, n->sym->module);          
  if (u != 0) return u;       
       
  return strcmp(r->sym->name, n->sym->name);          
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
    
    
   
   
/* find_true_name()-- Given a true name, search the true name tree to
 * see if it exists within the main namespace. */    
    
static g95_symbol *find_true_name(char *nm, char *mod) { 
true_name y, *k;  
g95_symbol sym;   
int v;   
   
  if (mod[0] == '\0') return NULL; /* Don't match a hidden symbol */         
         
  strcpy(sym.name, nm);    
  strcpy(sym.module, mod);   
  y.sym = &sym;      
      
  k = true_name_root;
  while(k != NULL) {    
    v = compare_true_names(&y, k);    
    if (v == 0) return k->sym;     
     
    k = (v < 0) ? k->left : k->right;       
  }        
        
  return NULL;      
}  
  
  
         
         
/* get_module_locus()-- Get the module's input pointer so that we can
 * restore it later. */          
          
static void get_module_locus(module_locus *p) {     
     
  p->column = module_column;        
  p->line = module_line;       
  fgetpos(module_fp, &p->pos);          
}         
         
         
        
        
/* add_true_name()-- Given a g95_symbol pointer that is not in the
 * true name tree, add it. */     
     
static void add_true_name(g95_symbol *s) {          
true_name *i;         
         
  i = g95_getmem(sizeof(true_name));          
  i->sym = s;

  g95_insert_bbt(&true_name_root, i, compare_true_names);    
} 
 
 
  
  
/* free_pi_tree()-- Recursively free the tree of pointer structures */   
   
static void free_pi_tree(pointer_info *g) {         
         
  if (g == NULL) return;

  if (g->fixup != NULL) g95_internal_error("free_pi_tree(): Unresolved fixup");  
  
  free_pi_tree(g->left);    
  free_pi_tree(g->right);     
     
  g95_free(g);   
}    
    
    


/* init_pi_tree()-- Initialize the pointer_info tree. */ 
 
static void init_pi_tree(void) {  
int (*compare)(pointer_info *, pointer_info *); 
pointer_info *h;   
   
  pi_root = NULL;        
  compare = (iomode == IO_INPUT) ? compare_integers : compare_pointers;          
          
  /* Pointer 0 is the NULL pointer */         
         
  h = g95_get_pointer_info();          
  h->u.pointer = NULL; 
  h->integer = 0;     
  h->type = P_OTHER;   
   
  g95_insert_bbt(&pi_root, h, compare); 
 
  /* Pointer 1 is the current namespace */   
   
  h = g95_get_pointer_info();     
  h->u.pointer = (char *) g95_current_ns;
  h->integer = 1;  
  h->type = P_NAMESPACE;    
    
  g95_insert_bbt(&pi_root, h, compare);         
         
  symbol_number = 2;        
}       
       
       
       
       
/* bad_module()-- Report problems with a module.  Error reporting is
 * not very elaborate, since this sorts of errors shouldn't really
 * happen.  This subroutine never returns.  */          
          
static void bad_module(char *m) { 
char *n;    
    
  switch(iomode) {          
  case IO_INPUT:   n = "Reading";  break;     
  case IO_OUTPUT:  n = "Writing";  break;      
  default:         n = "???";      break;      
  }    

  fclose(module_fp);        
        
  g95_fatal_error("%s module %s at line %d column %d: %s", n,  
		  module_name, module_line, module_column, m);         
}  
  
  
   
   
/* add_fixup()-- During module reading, given an integer and a pointer
 * to a pointer, either store the pointer from an already-known value
 * or create a fixup structure in order to store things later.
 * Returns zero if the reference has been actually stored, or nonzero
 * if the reference must be fixed later (ie associate_integer_pointer
 * must be called sometime later.  Returns the pointer_info structure. */

static pointer_info *add_fixup(int integer, void *gp) {          
pointer_info *k;     
fixup_t *q;      
char **cp;    
    
  k = get_integer(integer);    
    
  if (k->integer == 0 || k->u.pointer != NULL) {
    cp = gp;  
    *cp = k->u.pointer;        
  } else {   
    q = g95_getmem(sizeof(fixup_t));  
  
    q->next = k->fixup;  
    k->fixup = q; 
 
    q->pointer = gp;      
  }      
      
  return k;      
}       
       
       
    
    
/* get_pointer()-- Given a pointer while writing, returns the
 * pointer_info tree node, creating it if it doesn't exist. */ 
 
static pointer_info *get_pointer(void *gp) {     
pointer_info *n;    
    
  n = find_pointer(gp);   
  if (n != NULL) return n;       
       
  /* Pointer doesn't have an integer.  Give it one. */     
     
  n = g95_get_pointer_info();          
          
  n->u.pointer = gp; 
  n->integer = symbol_number++; 
 
  g95_insert_bbt(&pi_root, n, compare_pointers);    
    
  return n;  
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
     
     
        
        
/* init_true_name_tree()-- Initialize the true name tree with the
 * current namespace. */       
       
static void init_true_name_tree(void) {          
  true_name_root = NULL;     
     
  build_tnt(g95_current_ns->sym_root);          
}    
    
    


/* module_char()-- Get the next character in the module, updating our
 * reckoning of where we are. */        
        
static int module_char(void) {      
int h;        
        
  h = fgetc(module_fp);   
   
  if (h == EOF) bad_module("Unexpected EOF");    
    
  if (h == '\n') { 
    module_line++;  
    module_column = 0;          
  }       
       
  module_column++;   
  return h;       
}        
        
        
          
          
/* g95_match_use()-- Match a USE statement */   
   
match g95_match_use(void) {      
char name0[G95_MAX_SYMBOL_LEN+1];       
g95_use_rename *tail=NULL, *new;   
interface_type dtype;          
int operator;  
match j;        
        
  j = g95_match_name(module_name);  
  if (j != MATCH_YES) return j;         
         
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
     
    if (g95_match_generic_spec(&dtype, name0, &operator) == MATCH_ERROR)          
      goto cleanup;

    new->where = *g95_current_locus();   
   
    switch(dtype) {   
    case INTERFACE_NAMELESS:       
      g95_error("Missing generic specification in USE statement at %C");         
      goto cleanup;     
     
    case INTERFACE_GENERIC:    
      j = g95_match(" =>");

      if (only_flag) {
	if (j != MATCH_YES)  
	  strcpy(new->use_name, name0); 
	else {
	  strcpy(new->local_name, name0);     
     
	  j = g95_match_name(new->use_name);      
	  if (j == MATCH_NO) goto syntax;        
	  if (j == MATCH_ERROR) goto cleanup;        
	}          
      } else {          
	if (j != MATCH_YES) goto syntax;         
	strcpy(new->local_name, name0);        
        
	j = g95_match_name(new->use_name); 
	if (j == MATCH_NO) goto syntax;          
	if (j == MATCH_ERROR) goto cleanup;       
      }    
    
      break;         
         
    case INTERFACE_USER_OP:    
      strcpy(new->use_name, name0);      
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
    
    
      
      
/* set_module_locus()-- Set the module's input pointer */  
  
static void set_module_locus(module_locus *e) {       
       
  module_column = e->column;
  module_line = e->line;         
  fsetpos(module_fp, &e->pos);       
}      
      
      
  
  
/* read_cleanup()-- Recursive function for cleaning up things after a
 * module has been read. */          
          
static void read_cleanup(pointer_info *b) {         
g95_symtree *st;        
pointer_info *x; 
 
  if (b == NULL) return;

  read_cleanup(b->left);     
  read_cleanup(b->right);      
      
  if (b->type == P_SYMBOL && b->u.rsym.state == USED &&     
      !b->u.rsym.referenced) {

    x = get_integer(b->u.rsym.ns);         
    st = get_unique_symtree((g95_namespace *) x->u.pointer);    
    
    st->n.sym = b->u.rsym.sym;      
    st->n.sym->refs++;
  }      
      
  if (b->type == P_SYMBOL && b->u.rsym.state == UNUSED)       
    g95_free_symbol(b->u.rsym.sym);        
}          
          
          
     
     
void g95_module_init_2(void) {        
        
  last_atom = ATOM_LPAREN;    
}




/* parse_string()-- Parse a string constant.  The delimiter is
 * guaranteed to be a single quote. */

static void parse_string(void) {  
module_locus s;          
int len, t; 
char *d;    
    
  get_module_locus(&s); 
 
  len = 0;      
      
/* See how long the string is */ 
 
 loop:          
  t = module_char();     
  if (t == EOF) bad_module("Unexpected end of module in string constant");      
      
  if (t != '\'') {        
    len++; 
    goto loop;      
  }     
     
  t = module_char();         
  if (t == '\'') {     
    len++;       
    goto loop;          
  }     
     
  set_module_locus(&s);       
       
  atom_string = d = g95_getmem(len+1);      
      
  for(;len>0; len--) { 
    t = module_char(); 
    if (t == '\'') module_char();  /* Guaranteed to be another \' */        
    *d++ = t;   
  } 
 
  module_char();        /* Terminating \' */     
  *d = '\0';            /* C-style string for debug purposes */
}      
      
      
     
     
/* parse_integer()-- Parse a small integer. */          
          
static void parse_integer(int k) {     
module_locus h;   
   
  atom_int = k - '0';

  for(;;) {          
    get_module_locus(&h);         
         
    k = module_char();          
    if (!isdigit(k)) break;      
      
    atom_int = 10*atom_int + k - '0'; 
    if (atom_int > 99999999) bad_module("Integer overflow");          
  }     
     
  set_module_locus(&h);        
}  
  
  
     
     
/* find_enum()-- Given a pointer to an mstring array, require that
 * the current input be one of the strings in the array.  We return
 * the enum value. */         
         
static int find_enum(mstring *p) { 
int y;   
   
  y = g95_string2code(p, atom_name); 
  if (y >= 0) return y;  
  
  bad_module("find_enum(): Enum not found");         
         
  return 0;  /* Not reached */         
}        
        
        
         
         
/* write_atom()-- Write an atom to a module.  The line wrapping isn't
 * perfect, but it should work most of the time.  This isn't that big
 * of a deal, since the file really isn't meant to be read by people
 * anyway. */          
          
static void write_atom(atom_type atom, void *s) {       
char buffer[20];     
int w, leng;   
char *x;  
  
  switch(atom) {       
  case ATOM_STRING:     
  case ATOM_NAME:
    x = s;       
    break;        
        
  case ATOM_LPAREN:  
    x = "(";
    break;        
        
  case ATOM_RPAREN:   
    x = ")";         
    break;  
  
  case ATOM_INTEGER:        
    w = *((int *) s); 
    if (w < 0) g95_internal_error("write_atom(): Writing negative integer");      
      
    sprintf(buffer, "%d", w);  
    x = buffer;     
    break; 
     
  default:       
    g95_internal_error("write_atom(): Trying to write dab atom");       
       
  }

  leng = strlen(x);   
   
  if (atom != ATOM_RPAREN) {         
    if (module_column + leng > 72)         
      write_char('\n');         
    else {       
       
      if (last_atom != ATOM_LPAREN && module_column != 1) 
	write_char(' '); 
    }       
  }    
    
  if (atom == ATOM_STRING) write_char('\'');    
    
  while(*x) {        
    if (atom == ATOM_STRING && *x == '\'') write_char('\'');
    write_char(*x++);     
  }

  if (atom == ATOM_STRING) write_char('\'');  
  
  last_atom = atom;         
} 
 
 
   
   
/* parse_name()-- Parse a name.  */         
         
static void parse_name(int o) {  
module_locus w;    
char *x;       
int len;   
   
  x = atom_name;        
        
  *x++ = o; 
  len = 1;      
      
  get_module_locus(&w);     
     
  for(;;) {  
    o = module_char();
    if (!isalnum(o) && o != '_' && o != '-') break;        
        
    *x++ = o;     
    if (++len > G95_MAX_SYMBOL_LEN) bad_module("Name too long");   
  }        
        
  *x = '\0';         
         
  fseek(module_fp, -1, SEEK_CUR);     
  module_column = w.column + len - 1;    
    
  if (o == '\n') module_line--;   
}     
     
     
   
   
/* parse_atom()-- Read the next atom in the module's input stream. */   
   
static atom_type parse_atom(void) { 
int a;          
          
  do {        
    a = module_char(); 
  } while (a == ' ' || a == '\n');    
    
  switch(a) {   
  case '(':      
    return ATOM_LPAREN;     
     
  case ')': 
    return ATOM_RPAREN;         
         
  case '\'':  
    parse_string();       
    return ATOM_STRING;       
       
  case '0':  case '1':  case '2':  case '3':  case '4':
  case '5':  case '6':  case '7':  case '8':  case '9':        
    parse_integer(a);  
    return ATOM_INTEGER;        
        
  case 'a': case 'b': case 'c': case 'd': case 'e': case 'f': case 'g':
  case 'h': case 'i': case 'j': case 'k': case 'l': case 'm': case 'n':        
  case 'o': case 'p': case 'q': case 'r': case 's': case 't': case 'u':     
  case 'v': case 'w': case 'x': case 'y': case 'z': case 'A': case 'B':       
  case 'C': case 'D': case 'E': case 'F': case 'G': case 'H': case 'I':         
  case 'J': case 'K': case 'L': case 'M': case 'N': case 'O': case 'P': 
  case 'Q': case 'R': case 'S': case 'T': case 'U': case 'V': case 'W':     
  case 'X': case 'Y': case 'Z':      
    parse_name(a);  
    return ATOM_NAME;   
   
  default:        
    bad_module("Bad name");  
  }          
          
  return 0;   /* Not reached */          
} 
 
 
  
  
/* require_atom()-- Read the next atom from the input, requiring that
 * it be a particular kind */  
  
static void require_atom(atom_type dtype) {   
module_locus i;      
atom_type x;    
char *u;

  get_module_locus(&i);         
        
  x = parse_atom();          
  if (x != dtype) {  
    switch(dtype) {     
    case ATOM_NAME:     u = "Expected name";               break;      
    case ATOM_LPAREN:   u = "Expected left parenthesis";   break;        
    case ATOM_RPAREN:   u = "Expected right parenthesis";  break;    
    case ATOM_INTEGER:  u = "Expected integer";            break;     
    case ATOM_STRING:   u = "Expected string";             break;   
    default:         
      g95_internal_error("require_atom(): bad atom type required"); 
    }   
   
    set_module_locus(&i);       
    bad_module(u);      
  }          
}       
       
       
      
      
/* skip_list()-- Skip a list between balanced left and right parens. */  
  
static void skip_list(void) {     
int m;           
          
  m = 0;          
  do {
    switch(parse_atom()) {
    case ATOM_LPAREN:        
      m++;          
      break;      
      
    case ATOM_RPAREN:      
      m--;      
      break;  
  
    case ATOM_STRING:     
      g95_free(atom_string);         
      break;   
   
    case ATOM_NAME:  
    case ATOM_INTEGER:     
      break; 
    }    
  } while(m > 0);  
}          
          
          
     
     
static void mio_lparen(void) {        
        
  if (iomode == IO_OUTPUT)   
    write_atom(ATOM_LPAREN, NULL);   
  else         
    require_atom(ATOM_LPAREN);   
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
       
       
 
 
/* mio_internal_string()-- Read or write a string that is in static
 * memory or inside of some already-allocated structure */   
   
static void mio_internal_string(char *st) {          
          
  if (iomode == IO_OUTPUT)         
    write_atom(ATOM_STRING, st);     
  else {         
    require_atom(ATOM_STRING);        
    strcpy(st, atom_string);     
    g95_free(atom_string);  
  } 
}  
  
  


static void mio_gmp_real(mpf_t *real) {   
mp_exp_t exponent;          
char *t;          
          
  if (iomode == IO_INPUT) {    
    if (parse_atom() != ATOM_STRING) bad_module("Expected real string");          
          
    mpf_init(*real);   
    mpf_set_str(*real, atom_string, -16);     
    g95_free(atom_string);         
         
  } else {
    t = mpf_get_str(NULL, &exponent, 16, 0, *real);     
    atom_string = g95_getmem(strlen(t) + 20);      
      
    sprintf(atom_string, "0.%s@%ld", t, exponent);        
    write_atom(ATOM_STRING, atom_string); 
 
    g95_free(atom_string);     
    g95_free(t);     
  }        
}  
  
  
         
         
/* peek_atom()-- Peek at the next atom on the input */        
        
static atom_type peek_atom(void) {         
module_locus y;        
atom_type c;     
     
  get_module_locus(&y);   
  
  c = parse_atom();
  if (c == ATOM_STRING) g95_free(atom_string);       
       
  set_module_locus(&y);
  return c;
}    
    
    
         
         
/* check_unique_name()-- See if a name is a generated name. */  
  
static int check_unique_name(char *nm) {     
     
  return *nm == '@';          
}  
  
  
 
 
static void mio_integer(int *ifp) {     
     
  if (iomode == IO_OUTPUT) 
    write_atom(ATOM_INTEGER, ifp);         
  else {
    require_atom(ATOM_INTEGER);    
    *ifp = atom_int;        
  }
}


        
        
/* mio_name()-- Read or write an enumerated value.  On writing, we
 * return the input value for the convenience of callers.  We avoid
 * using an integer pointer because enums are sometimes inside bitfields. */   
   
static int mio_name(int e, mstring *o) {     
     
  if (iomode == IO_OUTPUT) 
    write_atom(ATOM_NAME, g95_code2string(o, e));    
  else {
    require_atom(ATOM_NAME);        
    e = find_enum(o);    
  }        
        
  return e;  
} 
 
 
       
       
static void mio_rparen(void) {   
   
  if (iomode == IO_OUTPUT)          
    write_atom(ATOM_RPAREN, NULL);
  else     
    require_atom(ATOM_RPAREN);        
}        
        
        
     
     
/* mio_symbol_attribute()-- Symbol attributes are stored in list with
 * the first three elements being the enumerated fields, while the
 * remaining elements (if any) indicate the individual attribute bits.
 * The access field is not saved-- it controls what symbols are
 * exported when a module is written. */

static void mio_symbol_attribute(symbol_attribute *attribute) {     
atom_type p;    
    
  mio_lparen();          
          
  attribute->flavor = mio_name(attribute->flavor, flavors);     
  attribute->intent = mio_name(attribute->intent, intents); 
  attribute->proc = mio_name(attribute->proc, procedures);   
  attribute->if_source = mio_name(attribute->if_source, ifsrc_types);  
  
  if (iomode == IO_OUTPUT) {        
    if (attribute->allocatable)   mio_name(AB_ALLOCATABLE, attr_bits);       
    if (attribute->dimension)     mio_name(AB_DIMENSION, attr_bits); 
    if (attribute->external)      mio_name(AB_EXTERNAL, attr_bits);     
    if (attribute->intrinsic)     mio_name(AB_INTRINSIC, attr_bits); 
    if (attribute->optional)      mio_name(AB_OPTIONAL, attr_bits);          
    if (attribute->pointer)       mio_name(AB_POINTER, attr_bits); 
    if (attribute->save)          mio_name(AB_SAVE, attr_bits);      
    if (attribute->target)        mio_name(AB_TARGET, attr_bits);         
    if (attribute->dummy)         mio_name(AB_DUMMY, attr_bits);        
    if (attribute->entry)         mio_name(AB_ENTRY, attr_bits);     
         
    if (attribute->data)          mio_name(AB_DATA, attr_bits);       
    if (attribute->in_namelist)   mio_name(AB_IN_NAMELIST, attr_bits);  
    if (attribute->in_common)     mio_name(AB_IN_COMMON, attr_bits); 
 
    if (attribute->function)      mio_name(AB_FUNCTION, attr_bits); 
    if (attribute->subroutine)    mio_name(AB_SUBROUTINE, attr_bits);          
    if (attribute->generic)       mio_name(AB_GENERIC, attr_bits);     
     
    if (attribute->sequence)      mio_name(AB_SEQUENCE, attr_bits);       
    if (attribute->elemental)     mio_name(AB_ELEMENTAL, attr_bits);    
    if (attribute->pure)          mio_name(AB_PURE, attr_bits);
    if (attribute->recursive)     mio_name(AB_RECURSIVE, attr_bits);        
        
    mio_rparen();  
  
  } else { 
 
    for(;;) { 
      p = parse_atom();    
      if (p == ATOM_RPAREN) break; 
      if (p != ATOM_NAME) bad_module("Expected attribute bit name"); 
 
      switch(find_enum(attr_bits)) {     
      case AB_ALLOCATABLE:   attribute->allocatable = 1;   break;     
      case AB_DIMENSION:     attribute->dimension = 1;     break;   
      case AB_EXTERNAL:      attribute->external = 1;      break;      
      case AB_INTRINSIC:     attribute->intrinsic = 1;     break;  
      case AB_OPTIONAL:      attribute->optional = 1;      break;      
      case AB_POINTER:       attribute->pointer = 1;       break;
      case AB_SAVE:          attribute->save = 1;          break;    
      case AB_TARGET:        attribute->target = 1;        break;   
      case AB_DUMMY:         attribute->dummy = 1;         break; 
      case AB_ENTRY:         attribute->entry = 1;         break; 
      case AB_DATA:          attribute->data = 1;          break;   
      case AB_IN_NAMELIST:   attribute->in_namelist = 1;   break;       
      case AB_IN_COMMON:     attribute->in_common = 1;     break;    
      case AB_FUNCTION:      attribute->function = 1;      break;       
      case AB_SUBROUTINE:    attribute->subroutine = 1;    break;   
      case AB_GENERIC:       attribute->generic = 1;       break;  
      case AB_SEQUENCE:      attribute->sequence = 1;      break;        
      case AB_ELEMENTAL:     attribute->elemental = 1;     break;        
      case AB_PURE:          attribute->pure = 1;          break;          
      case AB_RECURSIVE:     attribute->recursive = 1;     break;        
      }  
    }   
  }        
}


 
 
/* load_commons()-- Load common blocks */        
        
static void load_commons(void) {       
char name0[G95_MAX_SYMBOL_LEN+1];      
g95_common_head *z;      
      
  mio_lparen();  
  
  while(peek_atom() != ATOM_RPAREN) {   
    mio_lparen();         
    mio_internal_string(name0);        
        
    z = g95_get_common(name0);       
       
    mio_symbol_ref(&z->head);   
    mio_integer(&z->saved);         
    z->use_assoc = 1;   
   
    mio_rparen();
  }   
   
  mio_rparen();   
}       
       
   
   
/* mio_pointer_ref()-- Saves or restores a pointer.  The pointer is
 * converted back and forth from an integer.  We return the
 * pointer_info pointer so that the caller can take additional action
 * based on the pointer type. */        
        
static pointer_info *mio_pointer_ref(void *gp) {    
pointer_info *w;       
       
  if (iomode == IO_OUTPUT) {        
    w = get_pointer(*((char **) gp));      
    write_atom(ATOM_INTEGER, &w->integer);       
  } else {  
    require_atom(ATOM_INTEGER);      
    w = add_fixup(atom_int, gp);  
  }       
       
  return w;        
}         
         
         
  
  
void g95_module_done_2(void) {   
   
  free_rename();     
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
        
        
     
     
/* mio_interface_rest()-- Save/restore lists of g95_interface
 * stuctures.  When loading an interface, we are really appending to
 * the existing list of interfaces.  Checking for duplicate and
 * ambiguous interfaces has to be done later when all symbols have
 * been loaded */      
      
static void mio_interface_rest(g95_interface **ifp) {    
g95_interface *tail, *a;          
          
  if (iomode == IO_OUTPUT) {
    if (ifp != NULL)   
      for(a=*ifp; a; a=a->next)       
	mio_symbol_ref(&a->sym);          
  } else {         
         
    if (*ifp == NULL)
      tail = NULL;  
    else {       
      tail = *ifp;      
      while(tail->next) 
	tail = tail->next;   
    }       
       
    for(;;) {       
      if (peek_atom() == ATOM_RPAREN) break;      
      
      a = g95_get_interface();        
      a->where = *g95_current_locus();         
      mio_symbol_ref(&a->sym);   
   
      if (tail == NULL)   
	*ifp = a; 
      else        
	tail->next = a;          
          
      tail = a;      
    }     
  }   
   
  mio_rparen();       
}          
          
          
         
         
/* mio_formal_arglist()-- Read and write formal argument lists. */      
      
static void mio_formal_arglist(g95_symbol *sym) { 
g95_formal_arglist *o, *end;         
         
  mio_lparen();

  if (iomode == IO_OUTPUT) {   
    for(o=sym->formal; o; o=o->next)       
      mio_symbol_ref(&o->sym);  
  
  } else {   
    sym->formal = end = NULL;         
         
    while(peek_atom() != ATOM_RPAREN) {        
      o = g95_get_formal_arglist();         
      mio_symbol_ref(&o->sym);          
          
      if (sym->formal == NULL)   
	sym->formal = o;     
      else     
	end->next = o;   
   
      end = o;    
    }     
  }   
   
  mio_rparen();      
}         
         
         
   
   
static void mio_array_spec(g95_array_spec **asp) { 
g95_array_spec *spec;      
int z;    
    
  mio_lparen();     
     
  if (iomode == IO_OUTPUT) {        
    if (*asp == NULL) goto done;
    spec = *asp;         
  } else {    
    if (peek_atom() == ATOM_RPAREN) {          
      *asp = NULL;      
      goto done;          
    }   
   
    *asp = spec = g95_get_array_spec();        
  }          
          
  mio_integer(&spec->rank);   
  spec->type = mio_name(spec->type, array_spec_types);    
    
  for(z=0; z<spec->rank; z++) {    
    mio_expr(&spec->lower[z]);         
    mio_expr(&spec->upper[z]);     
  }   
   
done:       
  mio_rparen();    
}


  
  
static void mio_typespec(g95_typespec *typesp) {     
     
  mio_lparen();         
           
  typesp->type = mio_name(typesp->type, bt_types);      
      
  if (typesp->type != BT_DERIVED)
    mio_integer(&typesp->kind);  
  else         
    mio_symbol_ref(&typesp->derived);       
       
  mio_charlen(&typesp->cl);

  mio_rparen();    
}    
    
    
         
         
static void mio_iterator(g95_iterator **i) {       
g95_iterator *iter;    
    
  mio_lparen();        
        
  if (iomode == IO_OUTPUT) {        
    if (*i == NULL) goto done;         
  } else {   
    if (peek_atom() == ATOM_RPAREN) {  
      *i = NULL;          
      goto done;       
    }    
    
    *i = g95_get_iterator(); 
  } 
 
  iter = *i;         
         
  mio_expr(&iter->var);        
  mio_expr(&iter->start);        
  mio_expr(&iter->end);        
  mio_expr(&iter->step); 
 
done:      
  mio_rparen();     
}         
         
         
          
          
/* load_operator_interfaces()-- Load operator interfaces from the
 * module.  Interfaces are unusual in that they attach themselves to
 * existing symbols. */  
  
static void load_operator_interfaces(void) {       
char *f, nm[G95_MAX_SYMBOL_LEN+1], module[G95_MAX_SYMBOL_LEN+1];  
g95_user_op *op;    
module_locus t;        
        
  mio_lparen();    
    
  while(peek_atom() != ATOM_RPAREN) {      
    mio_lparen();    
    
    mio_internal_string(nm); 
    mio_internal_string(module); 
 
    get_module_locus(&t);       
       
    for(;;) {          
      f = find_use_name(nm);         
      if (f == NULL) break;       
       
      op = g95_get_uop(f); 
      mio_interface_rest(&op->operator);          
          
      set_module_locus(&t);  
    }  
  
    while(parse_atom() != ATOM_RPAREN); 
  }          
          
  mio_rparen();    
}          
          
          
   
   
/* mio_gmp_integer()-- Read and write an integer value */ 
 
static void mio_gmp_integer(mpz_t *integer) {         
char *d;         
         
  if (iomode == IO_INPUT) {        
    if (parse_atom() != ATOM_STRING) bad_module("Expected integer string");     
     
    mpz_init(*integer);       
    if (mpz_set_str(*integer, atom_string, 10))       
      bad_module("Error converting integer");       
       
    g95_free(atom_string);   
   
  } else {        
    d = mpz_get_str(NULL, 10, *integer); 
    write_atom(ATOM_STRING, d);       
    g95_free(d);          
  }          
}  
  
  
        
        
/* write_common()-- Write a common block to the module */         
         
static void write_common(g95_symtree *sta) {   
g95_common_head *v;       
       
  if (sta == NULL) return;      
      
  write_common(sta->left);     
  write_common(sta->right);

  mio_lparen(); 
  mio_internal_string(sta->name);          
          
  v = sta->n.common;  
  mio_symbol_ref(&v->head);    
  mio_integer(&v->saved);   
   
  mio_rparen();   
}    
    
    
  
  
/* mio_actual_arg()-- Save/restore an actual argument.  The argument
 * can't be part of a subroutine call, so we don't have to worry about
 * alternate return specs. */      
      
static void mio_actual_arg(g95_actual_arglist *g) {        
        
  mio_lparen();         
  mio_internal_string(g->name);  
  
  mio_integer((int *) &g->type);          
  mio_expr(&g->u.expr);    
    
  mio_rparen();
}      
      
      
   
   
/* mio_symbol_ref()-- Save or restore a reference to a symbol node */    
    
void mio_symbol_ref(g95_symbol **symp) {         
pointer_info *c;   
   
  c = mio_pointer_ref(symp); 
  if (c->type == P_UNKNOWN) c->type = P_SYMBOL;       
       
  if (iomode == IO_OUTPUT) {  
    if (c->u.wsym.state == UNREFERENCED) c->u.wsym.state = NEEDS_WRITE;      
  } else { 
    if (c->u.rsym.state == UNUSED) c->u.rsym.state = NEEDED;
  }        
}       
       
       
       
       
static void mio_component(g95_component *w) {   
pointer_info *u;  
int i;

  mio_lparen();     
     
  if (iomode == IO_OUTPUT) {    
    u = get_pointer(w);         
    mio_integer(&u->integer);        
  } else {         
    mio_integer(&i);       
    u = get_integer(i); 
    associate_integer_pointer(u, w);       
  } 
 
  if (u->type == P_UNKNOWN) u->type = P_COMPONENT;     
     
  mio_internal_string(w->name);      
  mio_typespec(&w->ts);          
  mio_array_spec(&w->as);

  mio_integer(&w->dimension);          
  mio_integer(&w->pointer); 
 
  mio_expr(&w->initializer);        
  mio_rparen();          
}         
         
         
       
       
static void mio_namespace_ref(g95_namespace **nsp) {   
g95_namespace *n;     
pointer_info *x;   
   
  x = mio_pointer_ref(nsp);     
     
  if (x->type == P_UNKNOWN) x->type = P_NAMESPACE; 
 
  if (iomode == IO_INPUT && x->integer != 0 && x->u.pointer == NULL) {          
    n = g95_get_namespace(NULL);   
    associate_integer_pointer(x, n);         
  }   
}


       
       
/* mio_array_ref()-- Given a pointer to an array reference
 * structure (which lives in a g95_ref structure), find the
 * corresponding array specification structure.  Storing the pointer
 * in the ref structure doesn't quite work when loading from a module.
 * Generating code for an array reference also needs more infomation
 * than just the array spec. */      
      
static void mio_array_ref(g95_array_ref *ref) {  
int b;       
       
  mio_lparen();     
  ref->type = mio_name(ref->type, array_ref_types);    
  mio_integer(&ref->dimen);          
          
  switch(ref->type) {       
  case AR_FULL:       
    break;         
         
  case AR_ELEMENT:     
    for(b=0; b<ref->dimen; b++)         
      mio_expr(&ref->start[b]); 
 
    break;     
     
  case AR_SECTION:      
    for(b=0; b<ref->dimen; b++) {       
      mio_expr(&ref->start[b]);
      mio_expr(&ref->end[b]);         
      mio_expr(&ref->stride[b]);  
    }      
      
    break;        
        
  case AR_UNKNOWN:         
    g95_internal_error("mio_array_ref(): Unknown array ref");
  }        
        
  for(b=0; b<ref->dimen; b++)      
    mio_integer((int *) &ref->dimen_type[b]);        
        
  if (iomode == IO_INPUT) {        
    for(b=0; b<ref->dimen; b++)        
      ref->c_where[b] = *g95_current_locus();       
  }       
       
  mio_rparen();
}         
         
         
     
     
static void mio_component_list(g95_component **cp) {
g95_component *k, *end;        
        
  mio_lparen();      
      
  if (iomode == IO_OUTPUT) {   
    for(k=*cp; k; k=k->next)  
      mio_component(k);    
  } else {         
         
    *cp = NULL;        
    end = NULL;  
  
    for(;;) {  
      if (peek_atom() == ATOM_RPAREN) break;     
     
      k = g95_get_component();    
      mio_component(k);    
    
      if (end == NULL)         
	*cp = k;
      else       
	end->next = k; 
 
      end = k;       
    }  
  }         
         
  mio_rparen();    
}


  
  
/* load_generic_interfaces()-- Load interfaces from the module.
 * Interfaces are unusual in that they attach themselves to existing
 * symbols.  */          
          
static void load_generic_interfaces(void) {   
char *z, nm[G95_MAX_SYMBOL_LEN+1], mod[G95_MAX_SYMBOL_LEN+1]; 
g95_symbol *symbol; 
module_locus t;          
          
  mio_lparen();    
    
  while(peek_atom() != ATOM_RPAREN) {     
    mio_lparen(); 
 
    mio_internal_string(nm);        
    mio_internal_string(mod); 
 
    get_module_locus(&t);    
    
    for(;;) {          
      z = find_use_name(nm);   
      if (z == NULL) break;          
          
      if (g95_find_symbol(z, NULL, 0, &symbol)) continue;     
     
      if (symbol == NULL) {         
	g95_get_symbol(z, NULL, &symbol);         
         
	symbol->attr.flavor = FL_PROCEDURE;   
	symbol->attr.generic = 1;       
	symbol->attr.use_assoc = 1;  
      }       
       
      mio_interface_rest(&symbol->generic);      
      set_module_locus(&t);
    }        
        
    while(parse_atom() != ATOM_RPAREN);     
  }

  mio_rparen();  
}      
      
      
       
       
/* mio_symbol()-- Unlike most other routines, the address of the
 * symbol node is already fixed on input and the name/module has
 * already been filled in. */ 
 
static void mio_symbol(g95_symbol *s) { 
 
  mio_lparen();          
          
  mio_symbol_attribute(&s->attr);          
  if (iomode == IO_INPUT && s->attr.flavor == FL_DERIVED) s->attr.set = 1;          
          
  mio_typespec(&s->ts);  
  mio_namespace_ref(&s->formal_ns);   
   
  mio_symbol_ref(&s->common_next);  /* Save/restore common block links */         
         
  mio_formal_arglist(s);      
  mio_expr(&s->value);

  mio_array_spec(&s->as);    
    
  if (iomode == IO_INPUT && s->attr.function) s->result = s;       
       
/* Note that components are always saved, even if they are supposed
 * to be private.  Component access is checked during searching */

  mio_component_list(&s->components);  
  
  if (s->components != NULL)       
    s->component_access = mio_name(s->component_access, access_types);          
          
  mio_rparen();
}  
  
  
  
  
/* mio_symbol_interface()-- Save/restore a named operator interface */  
  
static void mio_symbol_interface(char *nam, char *m, 
				 g95_interface **ifp) {  
  
  mio_lparen();   
   
  mio_internal_string(nam);       
  mio_internal_string(m); 
 
  mio_interface_rest(ifp); 
}    
    
    
  
  
static void mio_ref(g95_ref **rp) {       
g95_ref *y; 
 
  mio_lparen();

  y = *rp;       
  y->type = mio_name(y->type, ref_types);    
    
  switch(y->type) {         
  case REF_ARRAY:
    mio_array_ref(&y->u.ar);          
    break;        
        
  case REF_COMPONENT:  
    mio_symbol_ref(&y->u.c.sym); 
    mio_internal_string(y->u.c.name);      
      
    /* For input, r->u.c.component pointer fixed during resolution */         
    break;         
         
  case REF_SUBSTRING:         
    mio_expr(&y->u.ss.start);          
    mio_expr(&y->u.ss.end);   
    mio_charlen(&y->u.ss.length);     
    break;  
  }         
         
  mio_rparen();     
}


 
 
/* load_needed()-- Recursive function to traverse the pointer_info
 * tree and load a needed symbol.  We return nonzero if we load a
 * symbol and stop the traversal, because the act of loading can alter
 * the tree. */          
          
static int load_needed(pointer_info *f) {          
g95_namespace *ns;    
pointer_info *z;
g95_symbol *symbol;       
       
  if (f == NULL) return 0; 
  if (load_needed(f->left)) return 1; 
  if (load_needed(f->right)) return 1;    
    
  if (f->type != P_SYMBOL || f->u.rsym.state != NEEDED) return 0;         
         
  f->u.rsym.state = USED;         
         
  set_module_locus(&f->u.rsym.where);

  symbol = f->u.rsym.sym;  
  if (symbol == NULL) {     
    z = get_integer(f->u.rsym.ns);    
    
    ns = (g95_namespace *) z->u.pointer; 
    if (ns == NULL) { 
 
      /* Create an interface namespace if necessary.  These are the
       * namespaces that hold the formal parameters of module procedures. */          
          
      ns = g95_get_namespace(NULL);       
      associate_integer_pointer(z, ns);       
       
      ns->sibling = g95_current_ns->contained;          
      g95_current_ns->contained = ns;   
    }  
  
    symbol = g95_new_symbol(f->u.rsym.true_name, ns); 
    strcpy(symbol->module, f->u.rsym.module); 
 
    associate_integer_pointer(f, symbol);    
  }       
       
  mio_symbol(symbol);  
  symbol->attr.use_assoc = 1;        
        
  return 1; 
}


   
   
static void mio_actual_arglist(g95_actual_arglist **actualp) {    
g95_actual_arglist *n, *end;  
  
  mio_lparen();         
         
  if (iomode == IO_OUTPUT) {      
    for(n=*actualp; n; n=n->next)      
      mio_actual_arg(n);        
        
  } else {      
    end = NULL;  
  
    for(;;) {     
      if (peek_atom() != ATOM_LPAREN) break;          
          
      n = g95_get_actual_arglist();  
  
      if (end == NULL)   
	*actualp = n;         
      else   
	end->next = n;          
          
      end = n;      
      mio_actual_arg(n);      
    }
  }       
       
  mio_rparen();     
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
          
          
   
   
/* mio_shape()-- Save and restore the shape of an array constructor. */          
          
static void mio_shape(mpz_t **pshape, int r) {    
mpz_t *extent;  
atom_type s;      
int d;   
   
  /* A NULL shape is represented by ().  */  
  mio_lparen ();

  if (iomode == IO_OUTPUT) {   
    extent = *pshape;         
    if (!extent) {        
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
       
    extent = g95_get_shape(r);     
    *pshape = extent; 
  } 
 
  for(d=0; d<r; d++)          
    mio_gmp_integer (&extent[d]);     
     
  mio_rparen();        
}        
        
        
     
     
/* mio_interface()-- Save/restore a nameless operator interface */         
         
static void mio_interface(g95_interface **i) {       
       
  mio_lparen();     
  mio_interface_rest(i);         
}          
          
          
          
          
/* read_module()-- Read a module file */     
     
static void read_module(void) {   
module_locus operator_interfaces, user_operators;  
char *x, name[G95_MAX_SYMBOL_LEN+1];         
int g, ambiguous, symbol;  
pointer_info *info;      
g95_use_rename *y;   
g95_symtree *sta;      
g95_symbol *sy; 
 
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
    
    sy = find_true_name(info->u.rsym.true_name, info->u.rsym.module);
    if (sy == NULL) continue;    
    
    info->u.rsym.state = USED;         
    info->u.rsym.referenced = 1;        
    info->u.rsym.sym = sy; 
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
      x = find_use_name(name); 
      if (x == NULL) break;     
     
      sta = g95_find_symtree(g95_current_ns->sym_root, x);         
         
      if (sta != NULL) {
	if (sta->n.sym != info->u.rsym.sym && info->u.rsym.sym != NULL) 
	  sta->ambiguous = 1;        
      } else {        
	sta = check_unique_name(x) ? get_unique_symtree(g95_current_ns) : 
	  g95_new_symtree(&g95_current_ns->sym_root, x);         
         
	sta->ambiguous = ambiguous;  
  
	sy = info->u.rsym.sym;        
        
	if (sy == NULL) {  
	  sy = info->u.rsym.sym =      
	    g95_new_symbol(info->u.rsym.true_name, g95_current_ns);

	  strcpy(sy->module, info->u.rsym.module);      
	}         
         
	sta->n.sym = sy;      
	sta->n.sym->refs++;     
     
	if (info->u.rsym.state == UNUSED) info->u.rsym.state = NEEDED;      
	info->u.rsym.referenced = 1; 
      }  
    }         
  }       
       
  mio_rparen();    
    
  /* Load intrinsic operator interfaces. */      
      
  set_module_locus(&operator_interfaces);    
  mio_lparen();       
       
  for(g=0; g<G95_INTRINSIC_OPS; g++) {   
    if (g == INTRINSIC_USER) continue;          
          
    if (only_flag) {         
      y = find_use_operator(g);

      if (y == NULL) {         
	skip_list();          
	continue;          
      }

      y->found = 1;     
    }    
    
    mio_interface(&g95_current_ns->operator[g]);      
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
    
    
   
   
static void mio_constructor(g95_constructor **cp) {     
g95_constructor *o, *tail;       
       
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
    tail = NULL;

    while(peek_atom() != ATOM_RPAREN) {      
      o = g95_get_constructor();    
    
      if (tail == NULL)  
	*cp = o; 
      else       
	tail->next = o;

      tail = o;        
        
      mio_lparen();        
      mio_expr(&o->expr);      
      mio_iterator(&o->iterator);        
      mio_rparen();
    }
  }    
    
  mio_rparen();  
}         
         
         
    
    
static void mio_ref_list(g95_ref **rp) {          
g95_ref *ref, *head, *tail;  
  
  mio_lparen();       
       
  if (iomode == IO_OUTPUT) { 
    for(ref=*rp; ref; ref=ref->next)          
      mio_ref(&ref);          
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
        
        
       
       
/* write_operator()-- Write operator interfaces associated with a symbol. */         
         
static void write_operator(g95_user_op *u) {
static char nullstring[] = "";         
         
  if (u->operator == NULL ||  
      !check_access(u->access, u->ns->default_access)) return;          
          
  mio_symbol_interface(u->name, nullstring, &u->operator);
}


  
  
/* write_generic()-- Write generic interfaces associated with a symbol. */     
     
static void write_generic(g95_symbol *s) {    
    
  if (s->generic == NULL ||         
      !check_access(s->attr.access, s->ns->default_access)) return;     
     
  mio_symbol_interface(s->name, s->module, &s->generic);        
}         
         
         
 
 
/* mio_expr()-- Read and write expressions.  The form "()" is allowed
 * to indicate a NULL expression */          
          
static void mio_expr(g95_expr **ep) {   
g95_expr *u;    
atom_type l;
int flag;         
         
  mio_lparen(); 
 
  if (iomode == IO_OUTPUT) {      
    if (*ep == NULL) {  
      mio_rparen();          
      return;          
    } 
 
    u = *ep;          
    mio_name(u->type, expr_types);     
     
  } else {  
    l = parse_atom();          
    if (l == ATOM_RPAREN) {          
      *ep = NULL;       
      return;        
    } 
 
    if (l != ATOM_NAME) bad_module("Expected expression type");     
     
    u = *ep = g95_get_expr();    
    u->where = *g95_current_locus();
    u->type = find_enum(expr_types);        
  }       
       
  mio_typespec(&u->ts); 
  mio_integer(&u->rank);       
       
  switch(u->type) {   
  case EXPR_OP:       
    u->operator = mio_name(u->operator, intrinsics);

    switch(u->operator) {         
    case INTRINSIC_UPLUS:   case INTRINSIC_UMINUS:  case INTRINSIC_NOT:  
      mio_expr(&u->op1); 
      break;   
   
    case INTRINSIC_PLUS:    case INTRINSIC_MINUS:   case INTRINSIC_TIMES:         
    case INTRINSIC_DIVIDE:  case INTRINSIC_POWER:   case INTRINSIC_CONCAT:       
    case INTRINSIC_AND:     case INTRINSIC_OR:      case INTRINSIC_EQV:   
    case INTRINSIC_NEQV:    case INTRINSIC_EQ:      case INTRINSIC_NE:      
    case INTRINSIC_GT:      case INTRINSIC_GE:      case INTRINSIC_LT:         
    case INTRINSIC_LE:         
      mio_expr(&u->op1);         
      mio_expr(&u->op2);
      break;

    default:         
      bad_module("Bad operator");        
    } 
 
    break; 
 
  case EXPR_FUNCTION:    
    mio_symbol_ref(&u->symbol);   
    mio_actual_arglist(&u->value.function.actual);    
    
    if (iomode == IO_OUTPUT) {   
      mio_allocated_string(&u->value.function.name);      
      flag = u->value.function.isym == NULL;         
      mio_integer(&flag);     
      if (flag)    
	mio_symbol_ref(&u->symbol);          
      else    
	write_atom(ATOM_STRING, u->value.function.isym->name);    
    
    } else {
      require_atom(ATOM_STRING);      
      u->value.function.name = g95_get_string(atom_string);
      g95_free(atom_string);        
        
      mio_integer(&flag);     
      if (flag)      
	mio_symbol_ref(&u->symbol);          
      else {
	require_atom(ATOM_STRING);          
	u->value.function.isym = g95_find_function(atom_string);         
	g95_free(atom_string);
      }        
    }      
      
    break;    
    
  case EXPR_VARIABLE:          
    mio_symbol_ref(&u->symbol);  
    mio_ref_list(&u->ref);        
    break;  
  
  case EXPR_SUBSTRING:          
    mio_allocated_string(&u->value.character.string);  
    mio_ref_list(&u->ref); 
    break;         
         
  case EXPR_STRUCTURE:          
    mio_symbol_ref(&u->symbol);    
    /* Fall through */  
  
  case EXPR_ARRAY:  
    mio_constructor(&u->value.constructor);
    mio_shape(&u->shape, u->rank);          
    break;     
     
  case EXPR_CONSTANT:          
    switch(u->ts.type) { 
    case BT_INTEGER: 
      mio_gmp_integer(&u->value.integer); 
      break;         
         
    case BT_REAL: 
      mio_gmp_real(&u->value.real);  
      break;   
   
    case BT_COMPLEX:       
      mio_gmp_real(&u->value.complex.r);
      mio_gmp_real(&u->value.complex.i); 
      break;      
      
    case BT_LOGICAL:
      mio_integer(&u->value.logical);          
      break;   
   
    case BT_CHARACTER:         
      mio_integer(&u->value.character.length);     
      mio_allocated_string(&u->value.character.string);      
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
    
    
        
        
/* set_module_name()-- Decide on the module name of this symbol.
 * Procedures that are not module procedures of this module and aren't
 * generic don't get a module name. */        
        
static void set_module_name(g95_symbol *sy) {        
g95_namespace *ns;      
      
  if (sy->module[0] != '\0') return; 
 
  if (sy->attr.dummy) return;         
         
  if (sy->attr.flavor == FL_PROCEDURE && sy->attr.proc != PROC_MODULE) {      
    for(ns=sy->ns->contained; ns; ns=ns->sibling)         
      if (ns->proc_name == sy) break;     
     
    /* At this point, the symbol is an external procedure that does
     * not live in a module.  Symbols without modules are not matched
     * globally by the module read subroutine, but these need to be,
     * even though they are not in a module.  We stick them into a
     * "module" with an otherwise illegal name. */ 
 
    if (ns == NULL) {  
      strcpy(sy->module, "(global)");      
      return;   
    } 
  }         
         
  strcpy(sy->module, module_name);  
}   
   
   
         
         
static void write_symtree(g95_symtree *sta) {   
g95_symbol *sy;         
pointer_info *e;        
        
  sy = sta->n.sym;  
  if (!check_access(sy->attr.access, sy->ns->default_access) ||    
      (sy->attr.flavor == FL_PROCEDURE && sy->attr.generic &&   
       !sy->attr.subroutine && !sy->attr.function)) return;       
       
  if (check_unique_name(sta->name)) return;          
          
  e = find_pointer(sy);   
  if (e == NULL) g95_internal_error("write_symtree(): Symbol not written");        
        
  mio_internal_string(sta->name);   
  mio_integer(&sta->ambiguous);
  mio_integer(&e->integer);     
}


         
         
/* write_symbol()-- Write a symbol to the module. */   
   
static void write_symbol(int y, g95_symbol *symbol) { 
 
  if (symbol->attr.flavor == FL_UNKNOWN || symbol->attr.flavor == FL_LABEL)   
    g95_internal_error("write_symbol(): bad module symbol '%s'", symbol->name);      
      
  mio_integer(&y);        
  mio_internal_string(symbol->name); 
 
  mio_internal_string(symbol->module);    
  mio_pointer_ref(&symbol->ns);          
          
  mio_symbol(symbol);          
  write_char('\n');       
}     
     
     
       
       
/* write_symbol1()-- Recursive traversal function to write the
 * secondary set of symbols to the module file.  These are symbols
 * that were not public yet are needed by the public symbols or
 * another dependent symbol.  The act of writing a symbol can modify
 * the pointer_info tree, so we cease traversal if we find a symbol to
 * write.  We return nonzero if a symbol was written and pass that
 * information upwards. */      
      
static int write_symbol1(pointer_info *x) {   
   
  if (x == NULL) return 0;       
       
  if (write_symbol1(x->left)) return 1;    
  if (write_symbol1(x->right)) return 1; 
 
  if (x->type != P_SYMBOL || x->u.wsym.state != NEEDS_WRITE) return 0; 
 
  x->u.wsym.state = WRITTEN; 
  write_symbol(x->integer, x->u.wsym.sym);         
         
  return 1;        
}         
         
         
     
     
/* write_symbol0()-- Recursive traversal function to write the initial
 * set of symbols to the module.  We check to see if the symbol should
 * be written according to the access specification. */

static void write_symbol0(g95_symtree *st1) {    
g95_symbol *symbol;        
pointer_info *y;       
       
  if (st1 == NULL) return;    
    
  write_symbol0(st1->left);         
  write_symbol0(st1->right);  
  
  symbol = st1->n.sym;       
  set_module_name(symbol);      
      
  if (symbol->attr.flavor == FL_PROCEDURE && symbol->attr.generic &&        
      !symbol->attr.subroutine && !symbol->attr.function) return;        
        
  if (!check_access(symbol->attr.access, symbol->ns->default_access)) return;   
    
  y = get_pointer(symbol);          
  if (y->type == P_UNKNOWN) y->type = P_SYMBOL;         
         
  if (y->u.wsym.state == WRITTEN) return;    
    
  write_symbol(y->integer, symbol);     
  y->u.wsym.state = WRITTEN;   
   
  return;    
} 
 
 
   
   
static void write_module(void) {     
int i;   
   
  /* Write the operator interfaces */      
      
  mio_lparen();   
   
  for(i=0; i<G95_INTRINSIC_OPS; i++) {      
    if (i == INTRINSIC_USER) continue;  
  
    mio_interface(check_access(g95_current_ns->operator_access[i],  
			       g95_current_ns->default_access)         
		  ? &g95_current_ns->operator[i] : NULL);       
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
      
      
          
          
/* g95_dump_module()-- Given module, dump it to disk.  If there was an
 * error while processing the module, dump_flag will be set to zero
 * and we delete the module file, even if it was already there. */      
      
void g95_dump_module(char *n, int dump_flag) {        
char filename[PATH_MAX], *y;
g95_file *s;         
time_t now; 
 
  filename[0] = '\0';         
  if (g95_option.module_dir != NULL) strcpy(filename, g95_option.module_dir); 
 
  strcat(filename, n);      
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
          
  s = g95_current_file;       
  while(s->next)          
    s = s->next;       
       
#ifdef __GLIBC__
  muntrace();          
#endif

  now = time(NULL);        
  y = ctime(&now);  /* GLIBC 2.1 has a memory leak here */        
        
#ifdef __GLIBC__
  mtrace();          
#endif
   
  *strchr(y, '\n') = '\0';          
          
  fprintf(module_fp, "G95 module created from %s on %s\n", s->filename, y);
  fputs("If you edit this, you'll get what you deserve.\n\n", module_fp);    
    
  iomode = IO_OUTPUT;          
  strcpy(module_name, n);     
     
  init_pi_tree();       
       
  write_module();          
          
  free_pi_tree(pi_root);     
  pi_root = NULL;        
        
  write_char('\n'); 
 
  if (fclose(module_fp))     
    g95_fatal_error("Error writing module file '%s' for writing: %s",       
		    filename, strerror(errno));       
}    
    
    
          
          
/* g95_use_module()-- Process a USE directive. */   
   
void g95_use_module(void) {    
char filename[G95_MAX_SYMBOL_LEN+5];     
g95_state_data *d;    
int k, line;

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
    k = module_char();   
    if (k == EOF) bad_module("Unexpected end of module");  
    if (k == '\n') line++;     
  }      
      
  /* Make sure we're not reading the same module that we may be building */  
  
  for(d=g95_state_stack; d; d=d->previous)
    if (d->state == COMP_MODULE && strcmp(d->sym->name, module_name) == 0)         
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


