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
       AB_POINTER, AB_SAVE, AB_TARGET, AB_DUMMY, AB_RESULT, AB_ENTRY, AB_DATA,    
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
  minit("DUMMY",       AB_DUMMY),       minit("RESULT",       AB_RESULT),  
  minit("ENTRY",       AB_ENTRY),       minit("DATA",         AB_DATA),        
  minit("IN_NAMELIST", AB_IN_NAMELIST), minit("IN_COMMON",    AB_IN_COMMON),      
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
         
         
   
   
/* fp2()-- Recursive function to find a pointer within a tree by brute
 * force. */  
  
static pointer_info *fp2(pointer_info *x, char *target) {
pointer_info *i;   
   
  if (x == NULL) return NULL;     
    
  if (x->u.pointer == target) return x;     
     
  i = fp2(x->left, target);
  if (i != NULL) return i;       
       
  return fp2(x->right, target);   
}


   
   
/* find_use_operator()-- Try to find the operator in the current list */      
      
static g95_use_rename *find_use_operator(int op1) { 
g95_use_rename *u;      
      
  for(u=g95_rename_list; u; u=u->next)          
    if (u->operator == op1) return u;       
       
  return NULL;     
}  
  
  
 
 
/* bad_module()-- Report problems with a module.  Error reporting is
 * not very elaborate, since this sorts of errors shouldn't really
 * happen.  This subroutine never returns.  */        
        
static void bad_module(char *message) {          
char *z;  
  
  switch(iomode) {
  case IO_INPUT:   z = "Reading";  break;     
  case IO_OUTPUT:  z = "Writing";  break;      
  default:         z = "???";      break;     
  }           
       
  fclose(module_fp);         
         
  g95_fatal_error("%s module %s at line %d column %d: %s", z,    
		  module_name, module_line, module_column, message);
}    
    
    
 
 
/* module_char()-- Get the next character in the module, updating our
 * reckoning of where we are. */         
         
static int module_char(void) {         
int l;     
     
  l = fgetc(module_fp); 
 
  if (l == EOF) bad_module("Unexpected EOF");        
        
  if (l == '\n') {        
    module_line++;          
    module_column = 0;     
  }      
      
  module_column++;   
  return l; 
} 
 
 
       
       
/* find_pointer2()-- During reading, find a pointer_info node from the
 * pointer value.  This amounts to a brute-force search. */   
   
static pointer_info *find_pointer2(void *o) {  
  
  return fp2(pi_root, o);       
}   
   
   
        
        
/* compare_integers()-- Compare integers when searching by integer.
 * Used when reading a module. */       
       
static int compare_integers(pointer_info *sn1, pointer_info *sn2) { 
 
  if (sn1->integer < sn2->integer) return -1;      
  if (sn1->integer > sn2->integer) return 1;      
      
  return 0;      
}     
     
     
  
  
void g95_module_init_2(void) { 
 
  last_atom = ATOM_LPAREN;     
}          
          
          
      
      
/* associate_integer_pointer()-- Call here during module reading when
 * we know what pointer to associate with an integer.  Any fixups that
 * exist are resolved at this time. */       
       
static void associate_integer_pointer(pointer_info *d, void *gp) {         
fixup_t *f, *n;      
      
  if (d->u.pointer != NULL)       
    g95_internal_error("associate_integer_pointer(): Already associated");  
  
  d->u.pointer = gp;          
          
  for(f=d->fixup; f; f=n) {         
    n = f->next;        
        
    *(f->pointer) = gp;         
    g95_free(f);       
  }

  d->fixup = NULL;    
} 
 
 
    
    
/* compare_true_names()-- Compare two true_name structures. */      
      
static int compare_true_names(true_name *o, true_name *l) {   
int b;   
   
  b = strcmp(o->sym->module, l->sym->module);      
  if (b != 0) return b;      
      
  return strcmp(o->sym->name, l->sym->name);       
}     
     
     
        
        
/* pointer_to_int()-- During module writing, call here with a pointer
 * to something, returning the pointer_info node. */   
   
static pointer_info *find_pointer(void *gp) { 
pointer_info *y;  
char *cp;   
   
  cp = (char *) gp;          
            
  y = pi_root;      
  while(y != NULL) {      
    if (y->u.pointer == cp) break;          
    y = (cp < y->u.pointer) ? y->left : y->right;         
  }         
         
  return y;         
}   
   
   
          
          
/* check_unique_name()-- See if a name is a generated name. */  
  
static int check_unique_name(char *n) {          
          
  return *n == '@';         
}   
   
   
    
    
/* free_rename()-- Free the rename list left behind by a USE
 * statement. */    
    
static void free_rename(void) {    
g95_use_rename *nxt;          
          
  for(;g95_rename_list; g95_rename_list=nxt) {     
    nxt = g95_rename_list->next;      
    g95_free(g95_rename_list);
  }       
}       
       
       
      
      
/* free_pi_tree()-- Recursively free the tree of pointer structures */   
   
static void free_pi_tree(pointer_info *p) {      
      
  if (p == NULL) return; 
 
  if (p->fixup != NULL) g95_internal_error("free_pi_tree(): Unresolved fixup"); 
 
  free_pi_tree(p->left);       
  free_pi_tree(p->right);        
        
  g95_free(p); 
}      
      
      
 
 
/* add_true_name()-- Given a g95_symbol pointer that is not in the
 * true name tree, add it. */  
  
static void add_true_name(g95_symbol *sym) {    
true_name *y;         
         
  y = g95_getmem(sizeof(true_name));       
  y->sym = sym;     
     
  g95_insert_bbt(&true_name_root, y, compare_true_names);      
}         
         
         
 
 
/* find_true_name()-- Given a true name, search the true name tree to
 * see if it exists within the main namespace. */        
        
static g95_symbol *find_true_name(char *name0, char *mod) {      
true_name e, *x;  
g95_symbol symbol;    
int j;     
     
  if (mod[0] == '\0') return NULL; /* Don't match a hidden symbol */  
  
  strcpy(symbol.name, name0);          
  strcpy(symbol.module, mod);
  e.sym = &symbol;      
      
  x = true_name_root;          
  while(x != NULL) {     
    j = compare_true_names(&e, x);  
    if (j == 0) return x->sym;  
  
    x = (j < 0) ? x->left : x->right; 
  }         
         
  return NULL;      
}        
        
        
    
    
/* get_integer()-- Given an integer during reading, find it in the
 * pointer_info tree, creating the node if not found. */

static pointer_info *get_integer(int integer) {     
pointer_info *s, t;      
int h;          
          
  t.integer = integer;    
    
  s = pi_root;         
  while(s != NULL) {
    h = compare_integers(&t, s);      
    if (h == 0) break;   
   
    s = (h < 0) ? s->left : s->right;  
  }         
         
  if (s != NULL) return s;  
  
  s = g95_get_pointer_info();       
  s->integer = integer;       
  s->u.pointer = NULL;        
        
  g95_insert_bbt(&pi_root, s, compare_integers);      
      
  return s;    
}    
    
    
   
   
/* get_unique_symtree()-- Return a symtree node with a name that is
 * guaranteed to be unique within the namespace and corresponds to an
 * illegal fortran name */  
  
static g95_symtree *get_unique_symtree(g95_namespace *names) {
char name[G95_MAX_SYMBOL_LEN+1];    
static int serial=0;       
       
  sprintf(name, "@%d", serial++);       
  return g95_new_symtree(&names->sym_root, name);       
}      
      
      
 
 
/* get_module_locus()-- Get the module's input pointer so that we can
 * restore it later. */    
    
static void get_module_locus(module_locus *t) { 
 
  t->column = module_column;     
  t->line = module_line;
  fgetpos(module_fp, &t->pos);     
}


    
    
/* set_module_name()-- Decide on the module name of this symbol.
 * Procedures that are not module procedures of this module and aren't
 * generic don't get a module name. */     
     
static void set_module_name(g95_symbol *symbol) {
g95_namespace *names;     
     
  if (symbol->module[0] != '\0') return;        
        
  if (symbol->attr.dummy) return;     
     
  if (symbol->attr.flavor == FL_PROCEDURE && symbol->attr.proc != PROC_MODULE) {     
    for(names=symbol->ns->contained; names; names=names->sibling)     
      if (names->proc_name == symbol) break;      
      
    /* At this point, the symbol is an external procedure that does
     * not live in a module.  Symbols without modules are not matched
     * globally by the module read subroutine, but these need to be,
     * even though they are not in a module.  We stick them into a
     * "module" with an otherwise illegal name. */       
       
    if (names == NULL) {       
      strcpy(symbol->module, "(global)");         
      return;   
    }   
  }     
     
  strcpy(symbol->module, module_name);          
}     
     
     


/* parse_name()-- Parse a name.  */          
          
static void parse_name(int a) {      
module_locus j;
char *h;     
int l;    
    
  h = atom_name;        
        
  *h++ = a;      
  l = 1;      
      
  get_module_locus(&j);   
   
  for(;;) { 
    a = module_char();  
    if (!isalnum(a) && a != '_' && a != '-') break;          
          
    *h++ = a; 
    if (++l > G95_MAX_SYMBOL_LEN) bad_module("Name too long");          
  } 
 
  *h = '\0'; 
 
  fseek(module_fp, -1, SEEK_CUR);   
  module_column = j.column + l - 1;      
      
  if (a == '\n') module_line--;     
}        
        
        
        
        
/* build_tnt()-- Recursive function to build the initial true name
 * tree by recursively traversing the current namespace. */       
       
static void build_tnt(g95_symtree *st0) {      
      
  if (st0 == NULL) return;        
        
  build_tnt(st0->left);         
  build_tnt(st0->right);     
     
  if (find_true_name(st0->n.sym->name, st0->n.sym->module) != NULL) return;         
         
  add_true_name(st0->n.sym);        
} 
 
 
  
  
/* g95_match_use()-- Match a USE statement */   
   
match g95_match_use(void) {
char name[G95_MAX_SYMBOL_LEN+1];  
g95_use_rename *end=NULL, *n1;   
interface_type type;
int oper;       
match n;

  n = g95_match_name(module_name);      
  if (n != MATCH_YES) return n;  
  
  free_rename();         
  only_flag = 0;     
     
  if (g95_match_eos() == MATCH_YES) return MATCH_YES;      
  if (g95_match_char(',') != MATCH_YES) goto syntax;

  if (g95_match(" only :") == MATCH_YES) only_flag = 1;      
      
  if (g95_match_eos() == MATCH_YES) return MATCH_YES;         
         
  for(;;) {      
    n1 = g95_get_use_rename();     
    n1->found = 0;  
  
    if (g95_rename_list == NULL)        
      g95_rename_list = n1;
    else       
      end->next = n1;  
  
    end = n1; 
 
    n1->operator = -1; 
 
    if (g95_match_generic_spec(&type, name, &oper) == MATCH_ERROR)
      goto cleanup; 
 
    n1->where = *g95_current_locus();     
     
    switch(type) {     
    case INTERFACE_NAMELESS:       
      g95_error("Missing generic specification in USE statement at %C");  
      goto cleanup;  
  
    case INTERFACE_GENERIC: 
      n = g95_match(" =>");      
      
      if (only_flag) {       
	if (n != MATCH_YES)   
	  strcpy(n1->use_name, name);      
	else {         
	  strcpy(n1->local_name, name);

	  n = g95_match_name(n1->use_name); 
	  if (n == MATCH_NO) goto syntax;     
	  if (n == MATCH_ERROR) goto cleanup;  
	}      
      } else {
	if (n != MATCH_YES) goto syntax; 
	strcpy(n1->local_name, name);         
         
	n = g95_match_name(n1->use_name);   
	if (n == MATCH_NO) goto syntax;     
	if (n == MATCH_ERROR) goto cleanup;     
      }      
      
      break;

    case INTERFACE_USER_OP:        
      strcpy(n1->use_name, name);       
      /* Fall through */    
    
    case INTERFACE_INTRINSIC_OP:          
      n1->operator = oper; 
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
         
         
    
    
/* init_true_name_tree()-- Initialize the true name tree with the
 * current namespace. */      
      
static void init_true_name_tree(void) {        
  true_name_root = NULL;         
         
  build_tnt(g95_current_ns->sym_root);       
}         
         
         


/* compare_pointers()-- Compare pointers when searching by pointer.
 * Used when writing a module. */        
        
static int compare_pointers(pointer_info *sn1, pointer_info *sn2) {      
      
  if (sn1->u.pointer < sn2->u.pointer) return -1;   
  if (sn1->u.pointer > sn2->u.pointer) return 1;       
       
  return 0;    
}          
          
          
          
          
/* add_fixup()-- During module reading, given an integer and a pointer
 * to a pointer, either store the pointer from an already-known value
 * or create a fixup structure in order to store things later.
 * Returns zero if the reference has been actually stored, or nonzero
 * if the reference must be fixed later (ie associate_integer_pointer
 * must be called sometime later.  Returns the pointer_info structure. */  
  
static pointer_info *add_fixup(int integer, void *gp) {     
pointer_info *v;
fixup_t *l;
char **cp; 
 
  v = get_integer(integer);     
     
  if (v->integer == 0 || v->u.pointer != NULL) {
    cp = gp;
    *cp = v->u.pointer;      
  } else {         
    l = g95_getmem(sizeof(fixup_t));          
          
    l->next = v->fixup;   
    v->fixup = l;         
         
    l->pointer = gp;   
  }    
    
  return v;   
}  
  
  
 
 
/* free_true_name()-- Recursively free a true name tree node. */ 
 
static void free_true_name(true_name *m) {      
      
  if (m == NULL) return;      
  free_true_name(m->left);        
  free_true_name(m->right);    
    
  g95_free(m);       
}     
     
     
       
       
/* init_pi_tree()-- Initialize the pointer_info tree. */          
          
static void init_pi_tree(void) {        
int (*compare)(pointer_info *, pointer_info *); 
pointer_info *b;       
       
  pi_root = NULL; 
  compare = (iomode == IO_INPUT) ? compare_integers : compare_pointers; 
 
  /* Pointer 0 is the NULL pointer */  
  
  b = g95_get_pointer_info();      
  b->u.pointer = NULL;         
  b->integer = 0;          
  b->type = P_OTHER; 
 
  g95_insert_bbt(&pi_root, b, compare);   
   
  /* Pointer 1 is the current namespace */   
   
  b = g95_get_pointer_info();   
  b->u.pointer = (char *) g95_current_ns; 
  b->integer = 1;   
  b->type = P_NAMESPACE;  
  
  g95_insert_bbt(&pi_root, b, compare);      
      
  symbol_number = 2;  
}    
    
    
      
      
/* set_module_locus()-- Set the module's input pointer */  
  
static void set_module_locus(module_locus *p) {          
          
  module_column = p->column;     
  module_line = p->line;          
  fsetpos(module_fp, &p->pos); 
}


      
      
/* parse_string()-- Parse a string constant.  The delimiter is
 * guaranteed to be a single quote. */    
    
static void parse_string(void) { 
module_locus start; 
int len, g;   
char *t;  
  
  get_module_locus(&start);      
      
  len = 0;       
       
/* See how long the string is */    
    
 loop:          
  g = module_char();    
  if (g == EOF) bad_module("Unexpected end of module in string constant");      
      
  if (g != '\'') {    
    len++;
    goto loop;   
  }

  g = module_char();          
  if (g == '\'') { 
    len++;     
    goto loop;  
  }          
          
  set_module_locus(&start); 
 
  atom_string = t = g95_getmem(len+1);          
          
  for(;len>0; len--) {
    g = module_char();        
    if (g == '\'') module_char();  /* Guaranteed to be another \' */          
    *t++ = g;   
  }        
        
  module_char();        /* Terminating \' */   
  *t = '\0';            /* C-style string for debug purposes */        
}          
          
          
   
   
/* get_pointer()-- Given a pointer while writing, returns the
 * pointer_info tree node, creating it if it doesn't exist. */     
     
static pointer_info *get_pointer(void *gp) {          
pointer_info *d;  
  
  d = find_pointer(gp);  
  if (d != NULL) return d;    
    
  /* Pointer doesn't have an integer.  Give it one. */       
       
  d = g95_get_pointer_info();    
    
  d->u.pointer = gp;          
  d->integer = symbol_number++;   
   
  g95_insert_bbt(&pi_root, d, compare_pointers);

  return d;          
} 
 
 
       
       
/* find_use_name()-- Given a name, return the name under which to load
 * this symbol.  Returns NULL if this symbol shouldn't be loaded. */          
          
static char *find_use_name(char *name0) { 
g95_use_rename *v;      
      
  for(v=g95_rename_list; v; v=v->next)     
    if (strcmp(v->use_name, name0) == 0) break;     
     
  if (v == NULL) return only_flag ? NULL : name0;   
   
  v->found = 1;    
    
  return (v->local_name[0] != '\0') ? v->local_name : name0;       
}    
    
    
    
    
/* find_enum()-- Given a pointer to an mstring array, require that
 * the current input be one of the strings in the array.  We return
 * the enum value. */   
   
static int find_enum(mstring *a) {   
int i; 
 
  i = g95_string2code(a, atom_name);     
  if (i >= 0) return i;    
    
  bad_module("find_enum(): Enum not found");        
        
  return 0;  /* Not reached */
}         
         
         
          
          
/* parse_integer()-- Parse a small integer. */          
          
static void parse_integer(int x) {       
module_locus k;          
          
  atom_int = x - '0';  
  
  for(;;) {         
    get_module_locus(&k);         
         
    x = module_char(); 
    if (!isdigit(x)) break;     
     
    atom_int = 10*atom_int + x - '0';  
    if (atom_int > 99999999) bad_module("Integer overflow"); 
  }      
      
  set_module_locus(&k);       
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
         
         


/* parse_atom()-- Read the next atom in the module's input stream. */          
          
static atom_type parse_atom(void) { 
int p; 
 
  do {  
    p = module_char();     
  } while (p == ' ' || p == '\n');          
          
  switch(p) {        
  case '(': 
    return ATOM_LPAREN;   
   
  case ')':         
    return ATOM_RPAREN;          
          
  case '\'':     
    parse_string();   
    return ATOM_STRING;        
        
  case '0':  case '1':  case '2':  case '3':  case '4':       
  case '5':  case '6':  case '7':  case '8':  case '9':          
    parse_integer(p);   
    return ATOM_INTEGER;   
   
  case 'a': case 'b': case 'c': case 'd': case 'e': case 'f': case 'g':          
  case 'h': case 'i': case 'j': case 'k': case 'l': case 'm': case 'n':   
  case 'o': case 'p': case 'q': case 'r': case 's': case 't': case 'u':         
  case 'v': case 'w': case 'x': case 'y': case 'z': case 'A': case 'B':
  case 'C': case 'D': case 'E': case 'F': case 'G': case 'H': case 'I':     
  case 'J': case 'K': case 'L': case 'M': case 'N': case 'O': case 'P':          
  case 'Q': case 'R': case 'S': case 'T': case 'U': case 'V': case 'W':          
  case 'X': case 'Y': case 'Z':   
    parse_name(p);    
    return ATOM_NAME;       
       
  default:     
    bad_module("Bad name");  
  }          
          
  return 0;   /* Not reached */      
}    
    
    
          
          
/* peek_atom()-- Peek at the next atom on the input */       
       
static atom_type peek_atom(void) {
module_locus k;          
atom_type u;    
    
  get_module_locus(&k);  
 
  u = parse_atom(); 
  if (u == ATOM_STRING) g95_free(atom_string);   
   
  set_module_locus(&k);       
  return u;  
}         
         
         
       
       
/* require_atom()-- Read the next atom from the input, requiring that
 * it be a particular kind */     
     
static void require_atom(atom_type dtype) {         
module_locus m;       
atom_type a;   
char *y;

  get_module_locus(&m);     
    
  a = parse_atom();       
  if (a != dtype) {   
    switch(dtype) {
    case ATOM_NAME:     y = "Expected name";               break;        
    case ATOM_LPAREN:   y = "Expected left parenthesis";   break;      
    case ATOM_RPAREN:   y = "Expected right parenthesis";  break;       
    case ATOM_INTEGER:  y = "Expected integer";            break;      
    case ATOM_STRING:   y = "Expected string";             break;          
    default:     
      g95_internal_error("require_atom(): bad atom type required"); 
    }    
    
    set_module_locus(&m);  
    bad_module(y);    
  }  
}     
     
     
   
   
/* write_atom()-- Write an atom to a module.  The line wrapping isn't
 * perfect, but it should work most of the time.  This isn't that big
 * of a deal, since the file really isn't meant to be read by people
 * anyway. */         
         
static void write_atom(atom_type atom, void *s) {          
char buffer[20];         
int y, len;      
char *b;     
     
  switch(atom) {     
  case ATOM_STRING:         
  case ATOM_NAME:      
    b = s;    
    break;       
       
  case ATOM_LPAREN:   
    b = "(";   
    break; 
 
  case ATOM_RPAREN:    
    b = ")";      
    break;

  case ATOM_INTEGER:        
    y = *((int *) s);
    if (y < 0) g95_internal_error("write_atom(): Writing negative integer"); 
 
    sprintf(buffer, "%d", y);     
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
   
static int mio_name(int l, mstring *g) {   
   
  if (iomode == IO_OUTPUT)        
    write_atom(ATOM_NAME, g95_code2string(g, l));  
  else { 
    require_atom(ATOM_NAME);      
    l = find_enum(g);         
  }         
         
  return l;          
} 
 
 
          
          
static void mio_lparen(void) {        
        
  if (iomode == IO_OUTPUT)  
    write_atom(ATOM_LPAREN, NULL);  
  else       
    require_atom(ATOM_LPAREN); 
} 
 
 
      
      
/* mio_gmp_integer()-- Read and write an integer value */  
  
static void mio_gmp_integer(mpz_t *integer) {
char *u;   
   
  if (iomode == IO_INPUT) {       
    if (parse_atom() != ATOM_STRING) bad_module("Expected integer string");       
       
    mpz_init(*integer);   
    if (mpz_set_str(*integer, atom_string, 10))
      bad_module("Error converting integer");

    g95_free(atom_string);       
       
  } else {        
    u = mpz_get_str(NULL, 10, *integer);
    write_atom(ATOM_STRING, u); 
    g95_free(u);      
  }      
}       
       
       
        
        
static void mio_rparen(void) {          
          
  if (iomode == IO_OUTPUT)     
    write_atom(ATOM_RPAREN, NULL);     
  else
    require_atom(ATOM_RPAREN);      
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
      
      
/* mio_symbol_attribute()-- Symbol attributes are stored in list with
 * the first three elements being the enumerated fields, while the
 * remaining elements (if any) indicate the individual attribute bits.
 * The access field is not saved-- it controls what symbols are
 * exported when a module is written. */       
       
static void mio_symbol_attribute(symbol_attribute *attribute) {  
atom_type k;    
    
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
    if (attribute->result_var)    mio_name(AB_RESULT, attr_bits);     
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
      k = parse_atom();
      if (k == ATOM_RPAREN) break;    
      if (k != ATOM_NAME) bad_module("Expected attribute bit name");    
    
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
      case AB_RESULT:        attribute->result_var = 1;    break;        
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
        
        
       
       
/* mio_formal_arglist()-- Read and write formal argument lists. */  
  
static void mio_formal_arglist(g95_symbol *symb) {
g95_formal_arglist *s, *t;    
    
  mio_lparen();

  if (iomode == IO_OUTPUT) {   
    for(s=symb->formal; s; s=s->next)        
      mio_symbol_ref(&s->sym);         
         
  } else {  
    symb->formal = t = NULL;        
        
    while(peek_atom() != ATOM_RPAREN) {       
      s = g95_get_formal_arglist(); 
      mio_symbol_ref(&s->sym);    
    
      if (symb->formal == NULL)        
	symb->formal = s;        
      else     
	t->next = s;   
   
      t = s; 
    }        
  } 
 
  mio_rparen(); 
}      
      
      
    
    
/* mio_shape()-- Save and restore the shape of an array constructor. */      
      
static void mio_shape(mpz_t **pshape, int dim) {  
mpz_t *shap;   
atom_type y;  
int c;          
          
  /* A NULL shape is represented by ().  */     
  mio_lparen ();        
        
  if (iomode == IO_OUTPUT) {       
    shap = *pshape;   
    if (!shap) {      
      mio_rparen();  
      return;
    }          
  } else { 
    y = peek_atom();        
    if (y == ATOM_RPAREN) {        
      *pshape = NULL;        
      mio_rparen();   
      return;   
    }        
        
    shap = g95_get_shape(dim);     
    *pshape = shap;   
  }   
   
  for(c=0; c<dim; c++)       
    mio_gmp_integer (&shap[c]);    
    
  mio_rparen();   
}    
    
    
      
      
static void mio_integer(int *ifp) {

  if (iomode == IO_OUTPUT)   
    write_atom(ATOM_INTEGER, ifp);        
  else {   
    require_atom(ATOM_INTEGER);        
    *ifp = atom_int;          
  }     
}       
       
       


/* mio_array_ref()-- Given a pointer to an array reference
 * structure (which lives in a g95_ref structure), find the
 * corresponding array specification structure.  Storing the pointer
 * in the ref structure doesn't quite work when loading from a module.
 * Generating code for an array reference also needs more infomation
 * than just the array spec. */   
   
static void mio_array_ref(g95_array_ref *a) {
int e;          
          
  mio_lparen();     
  a->type = mio_name(a->type, array_ref_types);    
  mio_integer(&a->dimen);     
     
  switch(a->type) {     
  case AR_FULL:        
    break;     
     
  case AR_ELEMENT:      
    for(e=0; e<a->dimen; e++)   
      mio_expr(&a->start[e]);         
         
    break;       
       
  case AR_SECTION:         
    for(e=0; e<a->dimen; e++) {     
      mio_expr(&a->start[e]);     
      mio_expr(&a->end[e]);      
      mio_expr(&a->stride[e]);  
    }        
        
    break;       
       
  case AR_UNKNOWN:          
    g95_internal_error("mio_array_ref(): Unknown array ref");  
  }      
      
  for(e=0; e<a->dimen; e++) 
    mio_integer((int *) &a->dimen_type[e]);      
      
  if (iomode == IO_INPUT) {       
    a->where = *g95_current_locus();    
    
    for(e=0; e<a->dimen; e++)          
      a->c_where[e] = *g95_current_locus();         
  }     
     
  mio_rparen(); 
}        
        
        
   
   
static void mio_iterator(g95_iterator **ip) {        
g95_iterator *i;      
      
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
  
  i = *ip;      
      
  mio_expr(&i->var);        
  mio_expr(&i->start); 
  mio_expr(&i->end);      
  mio_expr(&i->step);

done:  
  mio_rparen();   
}        
        
        


static void mio_charlen(g95_charlen **clp) {    
g95_charlen *c;      
      
  mio_lparen();  
  
  if (iomode == IO_OUTPUT) {   
    c = *clp;  
    if (c != NULL) mio_expr(&c->length);      
  } else {          
          
    if (peek_atom() != ATOM_RPAREN) {
      c = g95_get_charlen();       
      mio_expr(&c->length);   
   
      *clp = c;          
          
      c->next = g95_current_ns->cl_list;
      g95_current_ns->cl_list = c; 
    }   
  }        
        
  mio_rparen();        
}     
     
     
        
        
/* mio_pointer_ref()-- Saves or restores a pointer.  The pointer is
 * converted back and forth from an integer.  We return the
 * pointer_info pointer so that the caller can take additional action
 * based on the pointer type. */  
  
static pointer_info *mio_pointer_ref(void *gp) {    
pointer_info *m;  
  
  if (iomode == IO_OUTPUT) { 
    m = get_pointer(*((char **) gp));       
    write_atom(ATOM_INTEGER, &m->integer);     
  } else {
    require_atom(ATOM_INTEGER);          
    m = add_fixup(atom_int, gp);       
  }       
       
  return m;
} 
 
 
      
      
static void mio_array_spec(g95_array_spec **asp) {          
g95_array_spec *a;          
int q;   
   
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
     
  for(q=0; q<a->rank; q++) {  
    mio_expr(&a->lower[q]);    
    mio_expr(&a->upper[q]);
  }

done: 
  mio_rparen();          
}


  
  
/* mio_component_ref()-- Save and load references to components that
 * occur within expressions.  We have to describe these references by
 * a number and by name.  The number is necessary for forward
 * references during reading, and the name is necessary if the symbol
 * already exists in the namespace and is not loaded again. */ 
 
static void mio_component_ref(g95_component **cp, g95_symbol *sy) {
char nam[G95_MAX_SYMBOL_LEN+1];        
g95_component *j;          
pointer_info *y; 
 
  y = mio_pointer_ref(cp);       
  if (y->type == P_UNKNOWN) y->type = P_COMPONENT;      
      
  if (iomode == IO_OUTPUT)          
    mio_internal_string((*cp)->name);       
  else {        
    mio_internal_string(nam);          
          
    if (sy->components != NULL && y->u.pointer == NULL) { 
      /* Symbol already loaded, so search by name */        
        
      for(j=sy->components; j; j=j->next)         
	if (strcmp(j->name, nam) == 0) break;         
         
      if (j == NULL)         
	g95_internal_error("mio_component_ref(): Component not found");     
     
      associate_integer_pointer(y, j);
    }          
          
    /* Make sure this symbol will eventually be loaded */      
      
    y = find_pointer2(sy);          
    if (y->u.rsym.state == UNUSED) y->u.rsym.state = NEEDED; 
  }        
}          
          
          
   
   
/* mio_symbol_ref()-- Save or restore a reference to a symbol node */ 
 
void mio_symbol_ref(g95_symbol **symp) { 
pointer_info *h; 
 
  h = mio_pointer_ref(symp);     
  if (h->type == P_UNKNOWN) h->type = P_SYMBOL;      
      
  if (iomode == IO_OUTPUT) {          
    if (h->u.wsym.state == UNREFERENCED) h->u.wsym.state = NEEDS_WRITE;      
  } else {        
    if (h->u.rsym.state == UNUSED) h->u.rsym.state = NEEDED;      
  }        
}      
      
      
        
        
/* read_cleanup()-- Recursive function for cleaning up things after a
 * module has been read. */          
          
static void read_cleanup(pointer_info *e) {          
g95_symtree *st; 
pointer_info *k;

  if (e == NULL) return;  
  
  read_cleanup(e->left);  
  read_cleanup(e->right);     
     
  if (e->type == P_SYMBOL && e->u.rsym.state == USED &&       
      !e->u.rsym.referenced) {     
     
    k = get_integer(e->u.rsym.ns);      
    st = get_unique_symtree((g95_namespace *) k->u.pointer);

    st->n.sym = e->u.rsym.sym;  
    st->n.sym->refs++;        
  }      
      
  if (e->type == P_SYMBOL && e->u.rsym.state == UNUSED)     
    g95_free_symbol(e->u.rsym.sym);         
}       
       
       
       
       
static void mio_typespec(g95_typespec *typ) {         
         
  mio_lparen();     
       
  typ->type = mio_name(typ->type, bt_types);  
  
  if (typ->type != BT_DERIVED)          
    mio_integer(&typ->kind);   
  else      
    mio_symbol_ref(&typ->derived);        
        
  mio_charlen(&typ->cl);        
        
  mio_rparen();         
}     
     
     
     
     
/* load_commons()-- Load common blocks */          
          
static void load_commons(void) {          
char nm[G95_MAX_SYMBOL_LEN+1];     
g95_common_head *p;    
    
  mio_lparen();     
     
  while(peek_atom() != ATOM_RPAREN) {       
    mio_lparen();    
    mio_internal_string(nm);     
     
    p = g95_get_common(nm);        
    if (p->head != NULL) 
      g95_internal_error("Common block %s already loaded", nm);   
   
    mio_symbol_ref(&p->head);   
    mio_integer(&p->saved);    
    p->use_assoc = 1;

    mio_rparen();  
  }     
     
  mio_rparen();
}        
        
         
         
static void mio_component(g95_component *c) {         
pointer_info *t;    
int r;         
         
  mio_lparen(); 
 
  if (iomode == IO_OUTPUT) {     
    t = get_pointer(c);         
    mio_integer(&t->integer);      
  } else {          
    mio_integer(&r);     
    t = get_integer(r);       
    associate_integer_pointer(t, c);       
  }   
   
  if (t->type == P_UNKNOWN) t->type = P_COMPONENT;        
        
  mio_internal_string(c->name);      
  mio_typespec(&c->ts);
  mio_array_spec(&c->as);  
  
  mio_integer(&c->dimension);        
  mio_integer(&c->pointer); 
 
  mio_expr(&c->initializer);  
  mio_rparen();   
}  
  
  
        
        
/* mio_actual_arg()-- Save/restore an actual argument.  The argument
 * can't be part of a subroutine call, so we don't have to worry about
 * alternate return specs. */    
    
static void mio_actual_arg(g95_actual_arglist *a) {    
    
  mio_lparen();     
  mio_internal_string(a->name);       
       
  mio_integer((int *) &a->type);   
  mio_expr(&a->u.expr);         
         
  mio_rparen();       
}     
     
     
        
        
static void mio_ref(g95_ref **rp) {  
g95_ref *n;        
        
  mio_lparen();    
    
  n = *rp;       
  n->type = mio_name(n->type, ref_types);    
    
  switch(n->type) {         
  case REF_ARRAY:     
    mio_array_ref(&n->u.ar);          
    break; 
 
  case REF_COMPONENT:   
    mio_symbol_ref(&n->u.c.sym);
    mio_component_ref(&n->u.c.component, n->u.c.sym);  
    break;         
     
  case REF_SUBSTRING: 
    mio_expr(&n->u.ss.start);        
    mio_expr(&n->u.ss.end);
    mio_charlen(&n->u.ss.length);    
    break;     
  }       
       
  mio_rparen();          
}          
          
          
   
   
static void mio_component_list(g95_component **cp) {         
g95_component *i, *tail;      
      
  mio_lparen();         
         
  if (iomode == IO_OUTPUT) { 
    for(i=*cp; i; i=i->next)   
      mio_component(i);         
  } else {  
  
    *cp = NULL;  
    tail = NULL;   
   
    for(;;) {     
      if (peek_atom() == ATOM_RPAREN) break;          
          
      i = g95_get_component();       
      mio_component(i);      
      
      if (tail == NULL)    
	*cp = i;      
      else     
	tail->next = i;   
   
      tail = i;          
    }  
  }  
  
  mio_rparen(); 
}   
   
   


static void mio_namespace_ref(g95_namespace **nsp) {       
g95_namespace *ns; 
pointer_info *e;       
       
  e = mio_pointer_ref(nsp); 
 
  if (e->type == P_UNKNOWN) e->type = P_NAMESPACE;         
         
  if (iomode == IO_INPUT && e->integer != 0 && e->u.pointer == NULL) {         
    ns = g95_get_namespace(NULL);    
    associate_integer_pointer(e, ns); 
  }    
}          
          
          
       
       
static void mio_ref_list(g95_ref **rp) {        
g95_ref *re, *h, *tail;        
        
  mio_lparen();       
       
  if (iomode == IO_OUTPUT) { 
    for(re=*rp; re; re=re->next)         
      mio_ref(&re);    
  } else {        
    h = tail = NULL;     
     
    while(peek_atom() != ATOM_RPAREN) {
      if (h == NULL) 
	h = tail = g95_get_ref();          
      else {  
	tail->next = g95_get_ref();        
	tail = tail->next;          
      }        
        
      mio_ref(&tail);       
    }   
   
    *rp = h;       
  }          
          
  mio_rparen();         
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
      
      
    
    
static void mio_constructor(g95_constructor **cp) { 
g95_constructor *w, *tail; 
 
  mio_lparen(); 
 
  if (iomode == IO_OUTPUT) {        
    for(w=*cp; w; w=w->next) {   
      mio_lparen();       
      mio_expr(&w->expr); 
      mio_iterator(&w->iterator); 
      mio_rparen();         
    }         
  } else {       
       
    *cp = NULL;     
    tail = NULL;       
       
    while(peek_atom() != ATOM_RPAREN) { 
      w = g95_get_constructor();    
    
      if (tail == NULL)      
	*cp = w; 
      else        
	tail->next = w;     
     
      tail = w;   
   
      mio_lparen();       
      mio_expr(&w->expr);
      mio_iterator(&w->iterator);      
      mio_rparen();       
    }    
  }         
         
  mio_rparen();          
}




static void mio_gmp_real(mpf_t *real) {          
mp_exp_t exponent;     
char *l;      
      
  if (iomode == IO_INPUT) {       
    if (parse_atom() != ATOM_STRING) bad_module("Expected real string");        
        
    mpf_init(*real);
    mpf_set_str(*real, atom_string, -16);      
    g95_free(atom_string); 
 
  } else {         
    l = mpf_get_str(NULL, &exponent, 16, 0, *real);     
    atom_string = g95_getmem(strlen(l) + 20);        
        
    sprintf(atom_string, "0.%s@%ld", l, exponent);          
    write_atom(ATOM_STRING, atom_string);        
        
    g95_free(atom_string); 
    g95_free(l);   
  } 
}          
          
          


/* mio_interface_rest()-- Save/restore lists of g95_interface
 * stuctures.  When loading an interface, we are really appending to
 * the existing list of interfaces.  Checking for duplicate and
 * ambiguous interfaces has to be done later when all symbols have
 * been loaded */   
   
static void mio_interface_rest(g95_interface **ip) {    
g95_interface *t, *i;    
    
  if (iomode == IO_OUTPUT) {          
    if (ip != NULL)   
      for(i=*ip; i; i=i->next)  
	mio_symbol_ref(&i->sym);     
  } else {       
       
    if (*ip == NULL)         
      t = NULL;
    else { 
      t = *ip;       
      while(t->next)      
	t = t->next;     
    }        
        
    for(;;) {  
      if (peek_atom() == ATOM_RPAREN) break; 
 
      i = g95_get_interface();      
      i->where = *g95_current_locus();    
      mio_symbol_ref(&i->sym);  
  
      if (t == NULL)        
	*ip = i;    
      else      
	t->next = i; 
 
      t = i;     
    } 
  }        
        
  mio_rparen(); 
}        
        
        
    
    
/* mio_symbol()-- Unlike most other routines, the address of the
 * symbol node is already fixed on input and the name/module has
 * already been filled in. */       
       
static void mio_symbol(g95_symbol *symb) {        
        
  mio_lparen();    
    
  mio_symbol_attribute(&symb->attr);          
  if (iomode == IO_INPUT && symb->attr.flavor == FL_DERIVED) symb->attr.set = 1;   
   
  mio_typespec(&symb->ts);      
  mio_namespace_ref(&symb->formal_ns);       
       
  mio_symbol_ref(&symb->common_next);  /* Save/restore common block links */

  mio_formal_arglist(symb);  
  mio_expr(&symb->value);        
        
  mio_array_spec(&symb->as);          
          
  if (iomode == IO_INPUT && symb->attr.function) symb->result = symb;       
       
/* Note that components are always saved, even if they are supposed
 * to be private.  Component access is checked during searching */       
       
  mio_component_list(&symb->components);     
     
  if (symb->components != NULL)
    symb->component_access = mio_name(symb->component_access, access_types);

  mio_rparen(); 
}     
     
     
  
  
/* load_generic_interfaces()-- Load interfaces from the module.
 * Interfaces are unusual in that they attach themselves to existing
 * symbols.  */   
   
static void load_generic_interfaces(void) {          
char *b, nam[G95_MAX_SYMBOL_LEN+1], m[G95_MAX_SYMBOL_LEN+1];   
g95_symbol *sy;  
  
  mio_lparen();      
      
  while(peek_atom() != ATOM_RPAREN) {      
    mio_lparen();     
     
    mio_internal_string(nam);  
    mio_internal_string(m);    
    
    /* Decide if we need to load this one or not */  
  
    b = find_use_name(nam);   
   
    if (b == NULL || g95_find_symbol(b, NULL, 0, &sy)) {  
      while(parse_atom() != ATOM_RPAREN);         
      continue;      
    }          
          
    if (sy == NULL) {          
      g95_get_symbol(b, NULL, &sy); 
 
      sy->attr.flavor = FL_PROCEDURE;          
      sy->attr.generic = 1;  
      sy->attr.use_assoc = 1;    
    } 
 
    mio_interface_rest(&sy->generic);       
  }

  mio_rparen();  
}    
    
    


/* write_symbol()-- Write a symbol to the module. */         
         
static void write_symbol(int b, g95_symbol *symb) {      
      
  if (symb->attr.flavor == FL_UNKNOWN || symb->attr.flavor == FL_LABEL)      
    g95_internal_error("write_symbol(): bad module symbol '%s'", symb->name);         
         
  mio_integer(&b);         
  mio_internal_string(symb->name); 
 
  mio_internal_string(symb->module);        
  mio_pointer_ref(&symb->ns);        
        
  mio_symbol(symb); 
  write_char('\n');
}         
         
         
 
 
/* mio_symbol_interface()-- Save/restore a named operator interface */     
     
static void mio_symbol_interface(char *nam, char *m,    
				 g95_interface **ifp) {      
      
  mio_lparen();   
   
  mio_internal_string(nam); 
  mio_internal_string(m);     
     
  mio_interface_rest(ifp);     
}  
  
  
       
       
/* load_needed()-- Recursive function to traverse the pointer_info
 * tree and load a needed symbol.  We return nonzero if we load a
 * symbol and stop the traversal, because the act of loading can alter
 * the tree. */          
          
static int load_needed(pointer_info *a) {       
g95_namespace *name;        
pointer_info *f;   
g95_symbol *sym;   
   
  if (a == NULL) return 0;         
  if (load_needed(a->left)) return 1;     
  if (load_needed(a->right)) return 1;         
         
  if (a->type != P_SYMBOL || a->u.rsym.state != NEEDED) return 0;

  a->u.rsym.state = USED;         
         
  set_module_locus(&a->u.rsym.where);  
  
  sym = a->u.rsym.sym;          
  if (sym == NULL) {      
    f = get_integer(a->u.rsym.ns);     
     
    name = (g95_namespace *) f->u.pointer;          
    if (name == NULL) {  
  
      /* Create an interface namespace if necessary.  These are the
       * namespaces that hold the formal parameters of module procedures. */  
  
      name = g95_get_namespace(NULL);  
      associate_integer_pointer(f, name);        
        
      name->sibling = g95_current_ns->contained;      
      g95_current_ns->contained = name;
    }  
  
    sym = g95_new_symbol(a->u.rsym.true_name, name);         
    strcpy(sym->module, a->u.rsym.module);       
       
    associate_integer_pointer(a, sym);
  }         
         
  mio_symbol(sym);    
  sym->attr.use_assoc = 1; 
 
  return 1;     
}  
  
  
    
    
static void mio_actual_arglist(g95_actual_arglist **actualp) {     
g95_actual_arglist *r, *tail;  
  
  mio_lparen();       
       
  if (iomode == IO_OUTPUT) {     
    for(r=*actualp; r; r=r->next)
      mio_actual_arg(r);          
          
  } else {         
    tail = NULL;

    for(;;) {         
      if (peek_atom() != ATOM_LPAREN) break;   
   
      r = g95_get_actual_arglist();

      if (tail == NULL)   
	*actualp = r;  
      else
	tail->next = r; 
 
      tail = r;    
      mio_actual_arg(r);          
    } 
  }       
       
  mio_rparen();  
}    
    
    
       
       
/* mio_expr()-- Read and write expressions.  The form "()" is allowed
 * to indicate a NULL expression */   
   
static void mio_expr(g95_expr **ep) {         
g95_expr *x;         
atom_type y;       
int flag;       
       
  mio_lparen();       
       
  if (iomode == IO_OUTPUT) {    
    if (*ep == NULL) {        
      mio_rparen();     
      return;    
    }      
      
    x = *ep;       
    mio_name(x->type, expr_types);

  } else {   
    y = parse_atom();    
    if (y == ATOM_RPAREN) {
      *ep = NULL;      
      return;          
    }          
          
    if (y != ATOM_NAME) bad_module("Expected expression type");   
   
    x = *ep = g95_get_expr();      
    x->where = *g95_current_locus();       
    x->type = find_enum(expr_types);      
  } 
 
  mio_typespec(&x->ts);  
  mio_integer(&x->rank); 
 
  switch(x->type) {        
  case EXPR_OP:
    x->operator = mio_name(x->operator, intrinsics);          
          
    switch(x->operator) {        
    case INTRINSIC_UPLUS:   case INTRINSIC_UMINUS:  case INTRINSIC_NOT:       
      mio_expr(&x->op1);       
      break;     
     
    case INTRINSIC_PLUS:    case INTRINSIC_MINUS:   case INTRINSIC_TIMES:     
    case INTRINSIC_DIVIDE:  case INTRINSIC_POWER:   case INTRINSIC_CONCAT:    
    case INTRINSIC_AND:     case INTRINSIC_OR:      case INTRINSIC_EQV:       
    case INTRINSIC_NEQV:    case INTRINSIC_EQ:      case INTRINSIC_NE:      
    case INTRINSIC_GT:      case INTRINSIC_GE:      case INTRINSIC_LT: 
    case INTRINSIC_LE:  
      mio_expr(&x->op1);       
      mio_expr(&x->op2);         
      break;  
  
    default:   
      bad_module("Bad operator");     
    }     
     
    break;  
  
  case EXPR_FUNCTION:        
    mio_symbol_ref(&x->symbol);      
    mio_actual_arglist(&x->value.function.actual);      
      
    if (iomode == IO_OUTPUT) {     
      mio_allocated_string(&x->value.function.name);        
      flag = x->value.function.isym == NULL;  
      mio_integer(&flag);  
      if (flag) 
	mio_symbol_ref(&x->symbol);          
      else   
	write_atom(ATOM_STRING, x->value.function.isym->name);          
          
    } else {        
      require_atom(ATOM_STRING); 
      x->value.function.name = g95_get_string(atom_string);       
      g95_free(atom_string);       
       
      mio_integer(&flag);       
      if (flag)  
	mio_symbol_ref(&x->symbol);      
      else {
	require_atom(ATOM_STRING);     
	x->value.function.isym = g95_find_function(atom_string);
	g95_free(atom_string);   
      }  
    }      
      
    break;      
      
  case EXPR_VARIABLE:         
    mio_symbol_ref(&x->symbol);     
    mio_ref_list(&x->ref); 
    break; 
 
  case EXPR_SUBSTRING: 
    mio_allocated_string(&x->value.character.string);     
    mio_expr(&x->op1);   
    mio_expr(&x->op2); 
    break;         
         
  case EXPR_STRUCTURE:     
    mio_symbol_ref(&x->symbol); 
    /* Fall through */      
      
  case EXPR_ARRAY:       
    mio_constructor(&x->value.constructor);          
    mio_shape(&x->shape, x->rank);      
    break;          
          
  case EXPR_CONSTANT:       
    switch(x->ts.type) {   
    case BT_INTEGER:    
      mio_gmp_integer(&x->value.integer);       
      break;      
      
    case BT_REAL:  
      mio_gmp_real(&x->value.real);    
      break;

    case BT_COMPLEX:      
      mio_gmp_real(&x->value.complex.r);       
      mio_gmp_real(&x->value.complex.i);    
      break;          
          
    case BT_LOGICAL:       
      mio_integer(&x->value.logical);          
      break;          
          
    case BT_CHARACTER: 
      mio_integer(&x->value.character.length); 
      mio_allocated_string(&x->value.character.string);    
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
          
          
      
      
/* write_common()-- Write a common block to the module */     
     
static void write_common(g95_symtree *st0) {         
g95_common_head *v;   
   
  if (st0 == NULL) return;    
    
  write_common(st0->left);   
  write_common(st0->right);      
      
  mio_lparen();  
  mio_internal_string(st0->name);

  v = st0->n.common;  
  mio_symbol_ref(&v->head);      
  mio_integer(&v->saved);        
        
  mio_rparen();        
}  
  
  
     
     
/* write_symbol1()-- Recursive traversal function to write the
 * secondary set of symbols to the module file.  These are symbols
 * that were not public yet are needed by the public symbols or
 * another dependent symbol.  The act of writing a symbol can modify
 * the pointer_info tree, so we cease traversal if we find a symbol to
 * write.  We return nonzero if a symbol was written and pass that
 * information upwards. */         
         
static int write_symbol1(pointer_info *l) {  
  
  if (l == NULL) return 0;

  if (write_symbol1(l->left)) return 1;    
  if (write_symbol1(l->right)) return 1;          
          
  if (l->type != P_SYMBOL || l->u.wsym.state != NEEDS_WRITE) return 0;        
        
  l->u.wsym.state = WRITTEN;      
  write_symbol(l->integer, l->u.wsym.sym);         
         
  return 1;      
}     
     
     
     
     
/* mio_interface()-- Save/restore a nameless operator interface */        
        
static void mio_interface(g95_interface **i) {  
  
  mio_lparen();    
  mio_interface_rest(i);          
}         
         
         
 
 
/* load_operator_interfaces()-- Load operator interfaces from the
 * module.  Interfaces are unusual in that they attach themselves to
 * existing symbols. */     
     
static void load_operator_interfaces(void) {    
char *f, n[G95_MAX_SYMBOL_LEN+1], module[G95_MAX_SYMBOL_LEN+1];      
g95_user_op *u;      
      
  mio_lparen();      
      
  while(peek_atom() != ATOM_RPAREN) {    
    mio_lparen();       
       
    mio_internal_string(n);     
    mio_internal_string(module);          
          
    /* Decide if we need to load this one or not */ 
 
    f = find_use_name(n);
    if (f == NULL) {       
      while(parse_atom() != ATOM_RPAREN); 
    } else {        
      u = g95_get_uop(f);   
      mio_interface_rest(&u->operator);        
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
       
       
    
    
/* write_operator()-- Write operator interfaces associated with a symbol. */  
  
static void write_operator(g95_user_op *op) {    
static char nullstring[] = "";   
   
  if (op->operator == NULL ||          
      !check_access(op->access, op->ns->default_access)) return;     
     
  mio_symbol_interface(op->name, nullstring, &op->operator);          
}       
       
       
        
        
/* write_symbol0()-- Recursive traversal function to write the initial
 * set of symbols to the module.  We check to see if the symbol should
 * be written according to the access specification. */ 
 
static void write_symbol0(g95_symtree *sta) {    
g95_symbol *symb;      
pointer_info *k;          
          
  if (sta == NULL) return;      
      
  write_symbol0(sta->left);      
  write_symbol0(sta->right);   
   
  symb = sta->n.sym;          
  set_module_name(symb);      
      
  if (symb->attr.flavor == FL_PROCEDURE && symb->attr.generic &&      
      !symb->attr.subroutine && !symb->attr.function) return;          
          
  if (!check_access(symb->attr.access, symb->ns->default_access)) return;    
     
  k = get_pointer(symb);      
  if (k->type == P_UNKNOWN) k->type = P_SYMBOL; 
 
  if (k->u.wsym.state == WRITTEN) return;     
     
  write_symbol(k->integer, symb);
  k->u.wsym.state = WRITTEN;   
   
  return;    
}     
     
     
    
    
static void write_symtree(g95_symtree *st1) {
g95_symbol *sym;         
pointer_info *u;  
  
  sym = st1->n.sym;          
  if (!check_access(sym->attr.access, sym->ns->default_access) ||          
      (sym->attr.flavor == FL_PROCEDURE && sym->attr.generic &&        
       !sym->attr.subroutine && !sym->attr.function)) return;          
          
  if (check_unique_name(st1->name)) return;      
      
  u = find_pointer(sym);          
  if (u == NULL) g95_internal_error("write_symtree(): Symbol not written");         
         
  mio_internal_string(st1->name);          
  mio_integer(&st1->ambiguous);          
  mio_integer(&u->integer);        
}         
         
         
         
         
/* read_module()-- Read a module file */         
         
static void read_module(void) {       
module_locus operator_interfaces, user_operators;     
char *e, name[G95_MAX_SYMBOL_LEN+1];          
int t, ambiguous, symbol;         
pointer_info *info;        
g95_use_rename *f; 
g95_symtree *st; 
g95_symbol *symb;    
    
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
    
    e = find_use_name(name);
    if (e == NULL) continue; 
 
    st = g95_find_symtree(g95_current_ns->sym_root, e);         
         
    if (st != NULL) {         
      if (st->n.sym != info->u.rsym.sym) st->ambiguous = 1;       
    } else {     
      st = check_unique_name(e) ? get_unique_symtree(g95_current_ns) :          
	g95_new_symtree(&g95_current_ns->sym_root, e);

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
   
  for(t=0; t<G95_INTRINSIC_OPS; t++) { 
    if (t == INTRINSIC_USER) continue;      
      
    if (only_flag) {        
      f = find_use_operator(t);          
          
      if (f == NULL) {  
	skip_list();  
	continue;
      }         
         
      f->found = 1; 
    }

    mio_interface(&g95_current_ns->operator[t]);          
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
   
  for(f=g95_rename_list; f; f=f->next) {  
    if (f->found) continue;  
  
    if (f->operator == -1) {
      g95_error("Symbol '%s' referenced at %L not found in module '%s'",        
		f->use_name, &f->where, module_name);  
      continue;         
    }    
           
    if (f->operator == INTRINSIC_USER) { 
      g95_error("User operator '%s' referenced at %L not found in module '%s'",          
		f->use_name, &f->where, module_name);         
      continue;   
    }   
   
    g95_error("Intrinsic operator '%s' referenced at %L not found in module "        
	      "'%s'", g95_op2string(f->operator), &f->where, module_name);   
  }          
          
  g95_check_interfaces(g95_current_ns);       
       
/* Clean up symbol nodes that were never loaded, create references to
 * hidden symbols. */       
       
  read_cleanup(pi_root);      
}


  
  
/* write_generic()-- Write generic interfaces associated with a symbol. */      
      
static void write_generic(g95_symbol *sy) {        
        
  if (sy->generic == NULL ||      
      !check_access(sy->attr.access, sy->ns->default_access)) return;          
          
  mio_symbol_interface(sy->name, sy->module, &sy->generic);      
}         
         
         


/* g95_use_module()-- Process a USE directive. */      
      
void g95_use_module(void) {        
char filename[G95_MAX_SYMBOL_LEN+5];         
g95_state_data *j;  
int x, line;      
      
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
    x = module_char();          
    if (x == EOF) bad_module("Unexpected end of module");    
    if (x == '\n') line++;     
  }     
     
  /* Make sure we're not reading the same module that we may be building */  
  
  for(j=g95_state_stack; j; j=j->previous)     
    if (j->state == COMP_MODULE && strcmp(j->sym->name, module_name) == 0)      
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
          
          
      
      
static void write_module(void) {   
int x;      
      
  /* Write the operator interfaces */         
         
  mio_lparen();   
   
  for(x=0; x<G95_INTRINSIC_OPS; x++) {     
    if (x == INTRINSIC_USER) continue;         
         
    mio_interface(check_access(g95_current_ns->operator_access[x],  
			       g95_current_ns->default_access)     
		  ? &g95_current_ns->operator[x] : NULL); 
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
        
void g95_dump_module(char *nm, int dump_flag) {      
char filename[PATH_MAX], *q;       
g95_file *g;   
time_t now;   
   
  filename[0] = '\0';      
  if (g95_option.module_dir != NULL) strcpy(filename, g95_option.module_dir);       
       
  strcat(filename, nm);          
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
  
  g = g95_current_file;    
  while(g->next)     
    g = g->next;     
     
#ifdef __GLIBC__
  muntrace();    
#endif
       
  now = time(NULL);          
  q = ctime(&now);  /* GLIBC 2.1 has a memory leak here */     
     
#ifdef __GLIBC__
  mtrace();     
#endif
        
  *strchr(q, '\n') = '\0';     
     
  fprintf(module_fp, "G95 module created from %s on %s\n", g->filename, q); 
  fputs("If you edit this, you'll get what you deserve.\n\n", module_fp);      
      
  iomode = IO_OUTPUT;    
  strcpy(module_name, nm); 
 
  init_pi_tree(); 
 
  write_module();    
    
  free_pi_tree(pi_root);      
  pi_root = NULL;    
    
  write_char('\n');     
     
  if (fclose(module_fp))          
    g95_fatal_error("Error writing module file '%s' for writing: %s",   
		    filename, strerror(errno));      
} 
 
 
