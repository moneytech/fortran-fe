/* Module module
   Copyright (C) 2000 Free Software Foundation, Inc.
   Contributed by Andy Vaught

This file is part of GNU G95.

GNU G95 is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU G95 is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU G95; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

/* module.c-- Handle modules, which amount to loading and saving symbols */

#include <string.h>
#include <stdio.h>
#include <ctype.h>
#include <errno.h>

#include "g95.h"


#define MODULE_EXTENSION ".mod"

/* Lists of rename info for the USE statement */

typedef struct g95_use_rename {
  char local_name[G95_MAX_SYMBOL_LEN+1], use_name[G95_MAX_SYMBOL_LEN+1];
  struct g95_use_rename *next;
} g95_use_rename;

#define g95_get_use_rename() g95_getmem(sizeof(g95_use_rename))


/* Local variables */

static FILE *module_fp;

static char module_name[G95_MAX_SYMBOL_LEN+1];
static int module_line, module_column, sym_num, only_flag;
static enum { IO_INPUT, IO_OUTPUT } iomode;

static g95_use_rename *g95_rename_list;
static g95_symbol **sym_table;


/* g95_free_rename()-- Free the rename list left behind by a USE
 * statement. */

void g95_free_rename(void) {
g95_use_rename *next;

  for(;g95_rename_list; g95_rename_list=next) {
    next = g95_rename_list->next;
    g95_free(g95_rename_list);
  }
}


/* g95_match_use()-- Match a USE statement */

match g95_match_use(void) {
char name[G95_MAX_SYMBOL_LEN+1];
g95_use_rename *tail, *new;
match m;

  m = g95_match(" %n", module_name);
  if (m != MATCH_YES) return m;

  g95_free_rename();
  only_flag = 0;

  if (g95_match_eos() == MATCH_YES) return MATCH_YES;
  if (g95_match(" ,") != MATCH_YES) goto syntax;

  if (g95_match(" only :") == MATCH_YES) only_flag = 1;

  for(;;) {
    new = g95_get_use_rename();

    if (g95_rename_list == NULL)
      g95_rename_list = new;
    else
      tail->next = new;

    tail = new;

    m = g95_match(" %n", name);
    if (m == MATCH_NO) goto syntax;
    if (m == MATCH_ERROR) goto cleanup;

    m = g95_match(" =>");

    if (only_flag) {
      if (m != MATCH_YES)
	strcpy(new->use_name, name);
      else {
	strcpy(new->local_name, name);

	m = g95_match(" %n", new->use_name);
	if (m == MATCH_NO) goto syntax;
	if (m == MATCH_ERROR) goto cleanup;
      }
    } else {
      if (m != MATCH_YES) goto syntax;
      strcpy(new->local_name, name);

      m = g95_match(" %n", new->use_name);
      if (m == MATCH_NO) goto syntax;
      if (m == MATCH_ERROR) goto cleanup;
    }

    if (g95_match_eos() == MATCH_YES) break;
    if (g95_match(" ,") != MATCH_YES) goto syntax;
  }

  return MATCH_YES;

syntax:
  g95_syntax_error(ST_USE);

cleanup:
  g95_free_rename();
  return MATCH_ERROR;
}


/* find_use_name()-- Try to find the use name in the current list */

static g95_use_rename *find_use_name(char *name) {
g95_use_rename *u;

  for(u=g95_rename_list; u; u=u->next)
    if (strcmp(u->use_name, name) == 0) return u;

  return NULL;
}


/*****************************************************************/

/* The next couple of subroutines keep track of which modules have
 * been loaded in the current program unit.  Modules that haven't been
 * seen before can be loaded much faster the first time. */

typedef struct g95_module {
  char name[G95_MAX_SYMBOL_LEN+1];
  struct g95_module *next;
} g95_module;

static g95_module *loaded_modules, *new_modules;


/* find_module()-- See if a module can be found in a list of modules.
 * Returns zero if not, nonzero if so. */

static int find_module(g95_module *m, char *name) {

  for(; m; m=m->next)
    if (strcmp(name, m->name) == 0) return 1;

  return 0;
}


/* check_module()-- See if a we've seen a particular module name
 * before.  Returns zero if not, nonzero if we have.  As a side
 * effect, not-seen before modules are placed in the new_modules list. */

static int check_module(char *name) {
g95_module *m;

  if (find_module(loaded_modules, name)) return 1;

  if (!find_module(new_modules, name)) {
    m = g95_getmem(sizeof(g95_module));

    strcpy(m->name, name);
    m->next = new_modules;

    new_modules = m;
  }

  return 0;
}


/* save_module_names()-- Concatenate the new_modules list with the
 * loaded_modules lists.  Called after a USE is complete. */

static void save_module_names(void) {
g95_module *m;

  if (new_modules == NULL) return; 

  m = new_modules;
  while(m->next != NULL)
    m = m->next;

  m->next = loaded_modules;

  loaded_modules = new_modules;
  new_modules = NULL;
}


static void free_module_list(g95_module *m) {
g95_module *next;

  for(;m; m=next) {
    next = m->next;
    g95_free(m);
  }
}



/*****************************************************************/


/* Module reading and writing */

typedef enum {
  ATOM_NAME, ATOM_LPAREN, ATOM_RPAREN, ATOM_INTEGER, ATOM_STRING, ATOM_EOF
} atom_type;


static atom_type last_atom;


typedef struct {
  int column, line;
  fpos_t pos;
} module_locus;


/* The name buffer must be at least as long as a symbol name.  Right
 * now it's not clear how we're going to store numeric constants--
 * probably as a hexadecimal string, since this will allow the exact
 * numeric representation to be preserved (this can't be done by
 * a decimal representation).  Worry about that later. */

#define MAX_ATOM_SIZE 100

static int atom_int;
static char *atom_string, atom_name[MAX_ATOM_SIZE];



/* bad_module()-- Report problems with a module.  Error reporting is
 * not very elaborate, since this sorts of errors shouldn't really
 * happen.  This subroutine never returns.  */

static void bad_module(char *message) {

  g95_fatal_error("Reading module %s at line %d column %d: %s",
		  module_name, module_line, module_column, message);
}


/* set_module_locus()-- Set the module's input pointer */

static void set_module_locus(module_locus *m) {

  module_column = m->column;
  module_line = m->line;
  fsetpos(module_fp, &m->pos);
}


/* get_module_locus()-- Get the module's input pointer so that we can
 * restore it later. */

static void get_module_locus(module_locus *m) {

  m->column = module_column;
  m->line = module_line;
  fgetpos(module_fp, &m->pos);
}


/* module_char()-- Get the next character in the module, updating our
 * reckoning of where we are. */

static int module_char(void) {
int c;

  c = fgetc(module_fp);

  if (c == '\n') {
    module_line++;
    module_column = 0;
  }

  module_column++;
  return c;
}


/* parse_string()-- Parse a string constant.  The delimiter is
 * guaranteed to be a single quote. */

void parse_string(void) {
module_locus start;
int len, c;
char *p;

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

  atom_string = p = g95_getmem(len+1);

  while(len > 0) {
    c = module_char();
    if (c == '\'') module_char();  /* Guaranteed to be another \' */
    *p++ = c;
  }

  module_char();        /* Terminating \' */
  *p++ = '\0';          /* C-style string for debug purposes */
}


/* parse_integer()-- Parse a small integer. */

static void parse_integer(int c) {
module_locus m;

  atom_int = c - '0';

  for(;;) {
    get_module_locus(&m);

    c = module_char();
    if (!isdigit(c)) break;

    atom_int = 10*atom_int + c - '0';
    if (atom_int > 99999999) bad_module("Integer overflow");
  }

  set_module_locus(&m);
}


/* parse_name()-- Parse a name.  */

static void parse_name(int c) {
module_locus m;
char *p;
int len;

  p = atom_name;

  *p++ = c;
  len = 1;

  for(;;) {
    get_module_locus(&m);

    c = module_char();
    if (!isalnum(c) && c != '_') break;

    *p++ = c;
    if (++len > G95_MAX_SYMBOL_LEN) bad_module("Name too long");
  }

  set_module_locus(&m);
}


/* parse_atom()-- Read the next atom in the module's input stream. */

static atom_type parse_atom(void) {
int c;

  do {
    c = module_char();
  } while (c == ' ' || c == '\n');

  if (c == EOF) return ATOM_EOF;
  if (c == '(') return ATOM_LPAREN;
  if (c == ')') return ATOM_RPAREN;

  if (c == '\'') {
    parse_string();
    return ATOM_STRING;
  }

  if (c >= '0' && c <= '9') {
    parse_integer(c);
    return ATOM_INTEGER;
  }

  if (!isalnum(c)) bad_module("Bad name");

  parse_name(c);
  return ATOM_NAME;
}


/* peek_atom()-- Peek at the next atom on the input */

static atom_type peek_atom(void) {
module_locus m;
atom_type a;

  get_module_locus(&m); 

  a = parse_atom();
  if (a == ATOM_STRING) g95_free(atom_string);

  set_module_locus(&m);
  return a;
}


/* require_atom()-- Read the next atom from the input, requiring that
 * it be a particular kind */

static void require_atom(atom_type type) {
module_locus m;
char *p;

  get_module_locus(&m); 

  if (parse_atom() != type) {
    switch(type) {
    case ATOM_NAME:     p = "Expected name";               break;
    case ATOM_LPAREN:   p = "Expected left parenthesis";   break;
    case ATOM_RPAREN:   p = "Expected right parenthesis";  break;
    case ATOM_INTEGER:  p = "Expected integer";            break;
    case ATOM_STRING:   p = "Expected string";             break;
    case ATOM_EOF:      p = "Expected end of file";        break;
    }

    set_module_locus(&m);
    bad_module(p);
  }
}


/* find_enum()-- Given a pointer to an mstring array, require that
 * the current input be one of the strings in the array.  We return
 * the enum value. */

static int find_enum(mstring *m) {
char msg[100];
int i;

  i = g95_string2code(m, atom_name);
  if (i < 0) {
    strcat(msg, "Expected one of ");
    for(i=0; i<3; i++) {
      strcat(msg, m->string);

      m++;
      if (m->string == NULL) break;
      strcat(msg, ", ");
    }

    if (m->string != NULL) strcat(msg, ", ...");
    bad_module(msg);
  }

  return i;
}


/* Module output subroutines */


/* write_char()-- Output a character to a module file */

static void write_char(char out) {

  if (fputc(out, module_fp) == EOF)
    g95_fatal_error("Error writing modules file: %s", strerror(errno));

  if (out == '\n')
    module_column = 1;
  else {
    module_column++;
    module_line++;
  }
}


/* write_atom()-- Write an atom to a module.  The line wrapping isn't
 * perfect, but it should work most of the time.  This isn't that big
 * of a deal, since the file really isn't meant to be read by people
 * anyway. */

static void write_atom(atom_type atom, void *v) {
char *p, buffer[20];
int len;

  switch(atom) {
  case ATOM_STRING:
  case ATOM_NAME:
    p = v;
    break;

  case ATOM_LPAREN:
    p = "(";
    break;

  case ATOM_RPAREN:
    p = ")";
    break;

  case ATOM_INTEGER:
    sprintf(buffer, "%d", *((int *) v));
    p = buffer;
    break;

  case ATOM_EOF:
    break;
  }

  len = strlen(p);

  if (atom != ATOM_RPAREN) {
    if (module_column + len > 72)
      write_char('\n');
    else {

      if (last_atom != ATOM_LPAREN)
	write_char(' ');
    }
  }

  if (atom == ATOM_STRING) write_char('\'');

  while(*p) {
    if (atom == ATOM_STRING && *p == '\'') write_char('\'');
    write_char(*p++);
  }

  if (atom == ATOM_STRING) write_char('\'');

  last_atom = atom;
}



/***************** Mid-level I/O subroutines *****************/

/* These subroutines let their caller read or write atoms without
 * caring about which of the two is actually happening.  This lets a
 * subroutine concentrate on the actual format of the data being written. */

static void mio_expr(g95_expr **);


/* mio_name()-- Read or write an enumerated value.  On writing, we
 * return the input value for the convenience of callers.  We avoid
 * using an integer pointer because enums are sometimes inside bitfields. */

static int mio_name(int t, mstring *m) {

  if (iomode == IO_OUTPUT)
    write_atom(ATOM_NAME, g95_code2string(m, t));
  else {
    require_atom(ATOM_NAME);
    t = find_enum(m);
  }

  return t;
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


/* mio_internal_string()-- Read or write a string that is in static
 * memory or inside of some already-allocated structure */

static void mio_internal_string(char *string) {

  if (iomode == IO_OUTPUT)
    write_atom(ATOM_STRING, string);
  else {
    require_atom(ATOM_STRING);
    strcpy(string, atom_string);
  }
}




enum { AB_ALLOCATABLE, AB_DIMENSION, AB_EXTERNAL, AB_INTRINSIC, AB_OPTIONAL,
       AB_POINTER, AB_PRIVATE, AB_PUBLIC, AB_SAVE, AB_TARGET, AB_DUMMY,
       AB_COMMON, AB_RESULT, AB_ENTRY, AB_DATA, AB_IN_NAMELIST,
       AB_IN_COMMON, AB_SAVED_COMMON, AB_FUNCTION, AB_SUBROUTINE,
       AB_SEQUENCE, AB_ELEMENTAL, AB_PURE, AB_RECURSIVE
} attribute_bits;


static mstring flavors[] = {
  minit("UNKNOWN",     FL_UNKNOWN),      minit("PROGRAM",     FL_PROGRAM),
  minit("BLOCK-DATA",  FL_BLOCK_DATA),   minit("MODULE",      FL_MODULE),
  minit("VARIABLE",    FL_VARIABLE),     minit("PARAMETER",   FL_PARAMETER),
  minit("LABEL",       FL_LABEL),        minit("ST-FUNCTION", FL_ST_FUNCTION),
  minit("MODULE-PROC", FL_MODULE_PROC),  minit("DUMMY-PROC",  FL_DUMMY_PROC),
  minit("PROCEDURE",   FL_PROCEDURE),    minit("DERIVED",     FL_DERIVED),
  minit("NAMELIST",    FL_NAMELIST),     minit("GENERIC",     FL_GENERIC),
  minit(NULL, -1) },

intents[] = {
  minit("UNKNOWN", INTENT_UNKNOWN),  minit("IN", INTENT_IN),
  minit("OUT", INTENT_OUT),          minit("INOUT", INTENT_INOUT),
  minit(NULL, -1)
},

scopes[] = {
  minit("UNKNOWN",    SCOPE_UNKNOWN),    minit("EXTERNAL",   SCOPE_EXTERNAL),
  minit("INTERNAL",   SCOPE_INTERNAL),   minit("INTRINSIC",  SCOPE_INTRINSIC),
  minit(NULL, -1)
},

attr_bits[] = {
  minit("ALLOCATABLE", AB_ALLOCATABLE), minit("DIMENSION",    AB_DIMENSION),
  minit("EXTERNAL",    AB_EXTERNAL),    minit("INTRINSIC",    AB_INTRINSIC),
  minit("OPTIONAL",    AB_OPTIONAL),    minit("POINTER",      AB_POINTER),
  minit("PRIVATE",     AB_PRIVATE),     minit("PUBLIC",       AB_PUBLIC),
  minit("SAVE",        AB_SAVE),        minit("TARGET",       AB_TARGET),
  minit("DUMMY",       AB_DUMMY),       minit("COMMON",       AB_COMMON),
  minit("RESULT",      AB_RESULT),      minit("ENTRY",        AB_ENTRY),
  minit("DATA",        AB_DATA),        minit("IN_NAMELIST",  AB_IN_NAMELIST),
  minit("IN_COMMON",   AB_IN_COMMON),   minit("SAVED_COMMON", AB_SAVED_COMMON),
  minit("FUNCTION",    AB_FUNCTION),    minit("SUBROUTINE",   AB_SUBROUTINE),
  minit("SEQUENCE",    AB_SEQUENCE),    minit("ELEMENTAL",    AB_ELEMENTAL),
  minit("PURE",        AB_PURE),        minit("RECURSIVE",    AB_RECURSIVE),
  minit(NULL, -1)
};


/* mio_symbol_attribute()-- Symbol attributes are stored in list with
 * the first three elements being the enumerated fields, while the
 * remaining elements (if any) indicate the individual attribute bits */

void mio_symbol_attribute(symbol_attribute *attr) {
atom_type t;

  mio_lparen();

  attr->flavor = mio_name(attr->flavor, flavors);
  attr->intent = mio_name(attr->intent, intents);
  attr->scope = mio_name(attr->scope, scopes);

  if (iomode == IO_OUTPUT) {
    if (attr->allocatable)   mio_name(AB_ALLOCATABLE, attr_bits);
    if (attr->dimension)     mio_name(AB_DIMENSION, attr_bits);
    if (attr->external)      mio_name(AB_EXTERNAL, attr_bits);
    if (attr->intrinsic)     mio_name(AB_INTRINSIC, attr_bits);
    if (attr->optional)      mio_name(AB_OPTIONAL, attr_bits);
    if (attr->pointer)       mio_name(AB_POINTER, attr_bits);
    if (attr->private)       mio_name(AB_PRIVATE, attr_bits);
    if (attr->public)        mio_name(AB_PUBLIC, attr_bits);
    if (attr->save)          mio_name(AB_SAVE, attr_bits);
    if (attr->target)        mio_name(AB_TARGET, attr_bits);
    if (attr->dummy)         mio_name(AB_DUMMY, attr_bits);
    if (attr->common)        mio_name(AB_COMMON, attr_bits);
    if (attr->result)        mio_name(AB_RESULT, attr_bits);
    if (attr->entry)         mio_name(AB_ENTRY, attr_bits);
    
    if (attr->data)          mio_name(AB_DATA, attr_bits);
    if (attr->in_namelist)   mio_name(AB_IN_NAMELIST, attr_bits);
    if (attr->in_common)     mio_name(AB_IN_COMMON, attr_bits);
    if (attr->saved_common)  mio_name(AB_SAVED_COMMON, attr_bits);

    if (attr->function)      mio_name(AB_FUNCTION, attr_bits);
    if (attr->subroutine)    mio_name(AB_SUBROUTINE, attr_bits);

    if (attr->sequence)      mio_name(AB_SEQUENCE, attr_bits);
    if (attr->elemental)     mio_name(AB_ELEMENTAL, attr_bits);
    if (attr->pure)          mio_name(AB_PURE, attr_bits);
    if (attr->recursive)     mio_name(AB_RECURSIVE, attr_bits);

    mio_rparen();

  } else {

    for(;;) {
      t = parse_atom();
      if (t == ATOM_RPAREN) break;
      if (t != ATOM_NAME) bad_module("Expected attribute bit name");

      switch(find_enum(attr_bits)) {
      case AB_ALLOCATABLE:   attr->allocatable = 1;   break;
      case AB_DIMENSION:     attr->dimension = 1;     break;
      case AB_EXTERNAL:      attr->external = 1;      break;
      case AB_INTRINSIC:     attr->intrinsic = 1;     break;
      case AB_OPTIONAL:      attr->optional = 1;      break;
      case AB_POINTER:       attr->pointer = 1;       break;
      case AB_PRIVATE:       attr->private = 1;       break;
      case AB_PUBLIC:        attr->public = 1;        break;
      case AB_SAVE:          attr->save = 1;          break;
      case AB_TARGET:        attr->target = 1;        break;
      case AB_DUMMY:         attr->dummy = 1;         break;
      case AB_COMMON:        attr->common = 1;        break;
      case AB_RESULT:        attr->result = 1;        break;
      case AB_ENTRY:         attr->entry = 1;         break;
      case AB_DATA:          attr->data = 1;          break;
      case AB_IN_NAMELIST:   attr->in_namelist = 1;   break;
      case AB_IN_COMMON:     attr->in_common = 1;     break;
      case AB_SAVED_COMMON:  attr->saved_common = 1;  break;
      case AB_FUNCTION:      attr->function = 1;      break;
      case AB_SUBROUTINE:    attr->subroutine = 1;    break;
      case AB_SEQUENCE:      attr->sequence = 1;      break;
      case AB_ELEMENTAL:     attr->elemental = 1;     break;
      case AB_PURE:          attr->pure = 1;          break;
      case AB_RECURSIVE:     attr->recursive = 1;     break;
      }
    }
  }
}




/* BT_UNKNOWN has been omitted from this list on purpose-- it should
 * never appear in an expression being written. */

static mstring bt_types[] = {
  minit("INTEGER",    BT_INTEGER),      minit("REAL",       BT_REAL),
  minit("COMPLEX",    BT_COMPLEX),      minit("LOGICAL",    BT_LOGICAL),
  minit("CHARACTER",  BT_CHARACTER),    minit("DERIVED",    BT_DERIVED),
  minit("PROCEDURE",  BT_PROCEDURE),    minit(NULL, -1)
};


static void mio_typespec(g95_typespec *ts) {

  mio_lparen();
  
  ts->type = mio_name(ts->type, bt_types);

  mio_integer(&ts->kind);
  
  /* Store symbol pointers */

  /* mio_expr(&ts->charlen); */

  mio_rparen();
}



static mstring array_spec_types[] = {
  minit("EXPLICIT", AS_EXPLICIT),   minit("ASSUMED_SHAPE", AS_ASSUMED_SHAPE),
  minit("DEFERRED", AS_DEFERRED),   minit("ASSUMED_SIZE",  AS_ASSUMED_SIZE),
  minit(NULL, -1)
};


static void mio_array_spec(g95_array_spec **asp) {
g95_array_spec *as;
int i;

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

  for(i=0; i<as->rank; i++) {
    mio_expr(&as->shape[i].lower);
    mio_expr(&as->shape[i].upper);
  }

done:
  mio_rparen();
}



static mstring array_ref_types[] = {
  minit("FULL", AR_FULL),         minit("ELEMENT", AR_ELEMENT),
  minit("SECTION", AR_SECTION),   minit(NULL, -1)
};


static void mio_array_ref(g95_array_ref *ar) {
int i;

  mio_lparen();
  ar->type = mio_name(ar->type, array_ref_types);
  mio_integer(&ar->rank);

  switch(ar->type) {
  case AR_FULL:
    break;

  case AR_ELEMENT:
    for(i=0; i<ar->rank; i++)
      mio_expr(&ar->shape[i].start);

    break;

  case AR_SECTION:
    for(i=0; i<ar->rank; i++) {
      mio_expr(&ar->shape[i].start);
      mio_expr(&ar->shape[i].end);
      mio_expr(&ar->shape[i].stride);
    }

    break;
  }

  mio_rparen();
}



static void mio_component(g95_component *c) {

  mio_lparen();

  mio_internal_string(c->name);
  mio_typespec(&c->ts);
  mio_array_spec(&c->as);
  mio_symbol_attribute(&c->attr);

  /* locus?? */

  mio_expr(&c->initializer);
  mio_rparen();
}


static void mio_component_list(g95_component **cp) {
g95_component *c, *tail;

  mio_lparen();

  if (iomode == IO_OUTPUT) {
    for(c=*cp; c; c=c->next)
      mio_component(c);
  } else {

    *cp = NULL;
    tail = NULL;

    for(;;) {
      if (peek_atom() == ATOM_RPAREN) break;

      c = g95_getmem(sizeof(g95_component));
      mio_component(c);

      if (tail == NULL)
	*cp = c;
      else
	tail->next = c;

      tail = c;
    }
  }

  mio_rparen();
}



static void mio_actual_arg(g95_actual_arglist *a) {

  mio_lparen();
  mio_internal_string(a->name);
  mio_integer(&a->arg_number);
  mio_expr(&a->expr);
  mio_rparen();
}


static void mio_actual_arglist(g95_actual_arglist **ap) {
g95_actual_arglist *a, *tail;

  mio_lparen();

  if (iomode == IO_OUTPUT) {
    for(a=*ap; a; a=a->next)
      mio_actual_arg(a);

  } else {
    tail = NULL;

    for(;;) {
      if (peek_atom() != ATOM_LPAREN) break;

      a = g95_get_actual_arglist();

      if (tail == NULL)
	*ap = a;
      else
	tail->next = a;

      tail = a;
      mio_actual_arg(a);
    }
  }

  mio_rparen();
}



/* mio_namespace()-- Read and write namespaces.  This subroutine is
 * for reading and writing namespaces that contain formal parameters,
 * not top-level module namespaces */

void mio_namespace(g95_namespace *ns) {

  mio_lparen();


  mio_rparen();

}



static void mio_interface(g95_interface **p) {


}


/* mio_symbol_ref()-- Saves a *reference* to a symbol.  An entity's
 * real name is its address in memory, which is guaranteed to be
 * unique when it needs to be and the same for multiply named things.
 * During writing, we spit out the address.  During reading, this
 * address is matched definition of the entity */

static void mio_symbol_ref(g95_symbol **symp) {

  if (iomode == IO_OUTPUT) {
    if (*symp != NULL)
      write_atom(ATOM_INTEGER, &((*symp)->serial));
    else {
      mio_lparen();
      mio_rparen();
    }
  } else {

    if (peek_atom() == ATOM_LPAREN) {
      mio_lparen();
      mio_rparen();
      *symp = NULL;
    } else {
      require_atom(ATOM_INTEGER);
      if (atom_int >= sym_num) bad_module("Symbol number out of range");
      *symp = sym_table[atom_int];
    }
  }
}



static void mio_iterator(g95_iterator **ip) {
g95_iterator *iter;

  mio_lparen();

  if (iomode == IO_OUTPUT) {
    if (*ip != NULL) {
      mio_rparen();
      goto done;
    }
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



static void mio_constructor(g95_constructor **cp) {
g95_constructor *c, *tail;

  mio_lparen();

  if (iomode == IO_OUTPUT) {
    for(c=*cp; c; c=c->next) {
      mio_lparen();
      mio_expr(&c->expr);
      mio_iterator(&c->iter);
      mio_constructor(&c->child);
      mio_rparen();
    }
  } else {

    *cp = NULL;
    tail = NULL;

    while(peek_atom() != ATOM_RPAREN) {
      c = g95_get_constructor();

      if (tail == NULL)
	*cp = c;
      else
	tail->next = c;

      tail = c;

      mio_lparen();
      mio_expr(&c->expr);
      mio_iterator(&c->iter);
      mio_constructor(&c->child);
      mio_rparen();
    }
  }

  mio_rparen();
}


static mstring ref_types[] = {
  minit("ARRAY", REF_ARRAY),            minit("COMPONENT", REF_COMPONENT),
  minit("SUBSTRING", REF_SUBSTRING),    minit(NULL, -1)
};



static void mio_ref(g95_ref **r) {

#if 0
char name[G95_MAX_SYMBOL_LEN+1];

  mio_lparen();

  r->type = mio_name(r->type, ref_types);

  //  mio_symbol_ref(&r->symbol);

  if (iomode == IO_OUTPUT) {

    write_atom(ATOM_NAME, r->component->name);
  } else {
    

  }

  mio_rparen();
#endif
}



static void mio_ref_list(g95_ref **rp) {


}



/* mio_gmp_integer()-- Read and write an integer value */

static void mio_gmp_integer(mpz_t *integer) {
char *p;

  if (iomode == IO_INPUT) {
    if (parse_atom() != ATOM_STRING) bad_module("Expected integer string");

    mpz_init(*integer);
    if (mpz_set_str(*integer, atom_string, 10))
      bad_module("Error converting integer");

    g95_free(atom_string);

  } else {
    p = mpz_get_str(NULL, 10, *integer);
    write_atom(ATOM_STRING, p);
    g95_free(p);
  }
}


static void mio_gmp_real(mpf_t *real) {
int exponent;
char *p;

  if (iomode == IO_INPUT) {
    if (parse_atom() != ATOM_STRING) bad_module("Expected real string");

    mpf_init(*real);
    mpf_set_str(*real, atom_string, -16);
    g95_free(atom_string);

  } else {
    p = mpf_get_str(NULL, (mp_exp_t *) &exponent, 16, 0, *real);
    atom_string = g95_getmem(strlen(p) + 20);

    sprintf(atom_string, "0.%s@%d", p, exponent);
    write_atom(ATOM_STRING, atom_string);

    g95_free(atom_string);
    g95_free(p);
  }
}


static mstring expr_types[] = {
  minit("OP",         EXPR_OP),         minit("FUNCTION",   EXPR_FUNCTION),
  minit("CONSTANT",   EXPR_CONSTANT),   minit("VARIABLE",   EXPR_VARIABLE),
  minit("SUBSTRING",  EXPR_SUBSTRING),  minit("STRUCTURE",  EXPR_STRUCTURE),
  minit("ARRAY",      EXPR_ARRAY),      minit(NULL, -1)
},

/* INTRINSIC_ASSIGN is missing because it is used as an index for
 * generic operators, not in expressions.  INTRINSIC_USER is also
 * replaced by the correct function name by the time we see it. */

intrinsics[] = {
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


/* mio_expr()-- Read and write expressions.  The form "()" is allowed
 * to indicate a NULL expression */

static void mio_expr(g95_expr **ep) {
atom_type t;
g95_expr *e;

  mio_lparen();

  if (iomode == IO_OUTPUT) {
    if (*ep == NULL) {
      mio_rparen();
      return;
    }

    e = *ep;
    mio_name(e->expr_type, expr_types);

  } else {
    t = parse_atom();
    if (t == ATOM_RPAREN) {
      *ep = NULL;
      return;
    }

    if (t != ATOM_NAME) bad_module("Expected expression type");

    e = *ep;
    e->expr_type = find_enum(expr_types);
  }

  e->expr_type = mio_name(e->expr_type, expr_types);
  mio_typespec(&e->ts);
  mio_integer(&e->rank);

  switch(e->expr_type) {
  case EXPR_OP:
    e->operator = mio_name(e->operator, intrinsics);

    switch(e->operator) {

    case INTRINSIC_UPLUS:   case INTRINSIC_UMINUS:  case INTRINSIC_NOT:
      mio_expr(&e->op1);
      break;

    case INTRINSIC_PLUS:    case INTRINSIC_MINUS:   case INTRINSIC_TIMES:
    case INTRINSIC_DIVIDE:  case INTRINSIC_POWER:   case INTRINSIC_CONCAT:
    case INTRINSIC_AND:     case INTRINSIC_OR:      case INTRINSIC_EQV:
    case INTRINSIC_NEQV:    case INTRINSIC_EQ:      case INTRINSIC_NE:
    case INTRINSIC_GT:      case INTRINSIC_GE:      case INTRINSIC_LT:
    case INTRINSIC_LE:
      mio_expr(&e->op1);
      mio_expr(&e->op2);
      break;

    default:
      bad_module("Bad operator");
    }

    break;

  case EXPR_FUNCTION:
    mio_symbol_ref(&e->symbol);
    mio_actual_arglist(&e->value.function.actual);
    mio_allocated_string(&e->value.function.name);
    break;

  case EXPR_VARIABLE:
    mio_symbol_ref(&e->symbol);
    mio_ref(&e->ref);
    break;

  case EXPR_SUBSTRING:
    mio_allocated_string(&e->value.character.string);
    mio_expr(&e->op1);
    mio_expr(&e->op2);
    break;

  case EXPR_STRUCTURE:
  case EXPR_ARRAY:
    mio_constructor(&e->value.constructor);
    break;

  case EXPR_CONSTANT:
    switch(e->ts.type) {
    case BT_INTEGER:
      mio_gmp_integer(&e->value.integer);
      break;

    case BT_REAL:
      mio_gmp_real(&e->value.real);
      break;

    case BT_COMPLEX:
      mio_gmp_real(&e->value.complex.r);
      mio_gmp_real(&e->value.complex.i);
      break;

    case BT_LOGICAL:
      mio_integer(&e->value.logical);
      break;

    case BT_CHARACTER:
      mio_integer(&e->value.character.length);
      mio_allocated_string(&e->value.character.string);
      break;

    default:
      bad_module("Bad type in constant expression");
    }

    break;
  }
}




/* mio_symbol()-- Unlike most other routines, the address of the
 * symbol node is already fixed on input and the name/module has
 * already been filled in */

static void mio_symbol(g95_symbol *sym) {

  mio_lparen();
  mio_integer(&sym->serial);

  mio_typespec(&sym->ts);
  mio_symbol_attribute(&sym->attr);

  mio_interface(&sym->operator);
  mio_interface(&sym->interface);
  mio_interface(&sym->generic);

  mio_expr(&sym->value);
  mio_array_spec(&sym->as);

  mio_symbol_ref(&sym->result);

  mio_component_list(&sym->components);

  mio_symbol_ref(&sym->common_head);
  mio_symbol_ref(&sym->common_next);

  //  sym->ns = g95_current_ns;

  mio_rparen();

}


/************************* Top level subroutines *************************/


static char true_name[G95_MAX_SYMBOL_LEN+1],
            true_module[G95_MAX_SYMBOL_LEN+1];

static g95_symbol *search_result;

static void find_true_name(g95_symtree *symtree) {
g95_symbol *sym;

  sym = symtree->sym;

  if (strcmp(sym->name, true_name) == 0 &&
      strcmp(sym->module, true_module) == 0) 
    search_result = sym;
}


/* skip_list()-- Skip a list between balenced left and right parens. */

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
    case ATOM_EOF:
      break;
    }
  } while(level > 0);
}


static void read_module(void) {
int serial, ambiguous, i, n, new_flag;
g95_symbol *sym, new;
g95_use_rename *u;
g95_symtree *st;

  mio_lparen(); 

  require_atom(ATOM_INTEGER);
  n = atom_int;

  sym_table = g95_getmem(n*sizeof(g95_symbol *));

  mio_lparen();

  for(i=0; i<n; i++) {
    require_atom(ATOM_NAME);
    strcpy(true_module, atom_name);

    require_atom(ATOM_NAME);
    strcpy(true_name, atom_name);

    sym = NULL;
    search_result = NULL;

    if (check_module(true_module)) {  /* Search via brute force traversal */
      g95_traverse_symtree(g95_current_ns, find_true_name);
      sym = search_result;
    }

    if (sym == NULL) {
      sym = g95_getmem(sizeof(g95_symbol));
      strcpy(sym->name, true_name);
      strcpy(sym->module, true_module);
    }

    sym_table[i] = sym;
    sym->mark = (search_result == NULL);
  }

  mio_rparen();

  /* Read the symtree definitions */

  mio_lparen();

  for(;;) {
    if (peek_atom() == ATOM_RPAREN) break;

    mio_lparen();

    require_atom(ATOM_NAME);

    require_atom(ATOM_INTEGER);
    serial = atom_int;

    require_atom(ATOM_INTEGER);
    ambiguous = atom_int;

    mio_rparen();

/* Figure out what to do with this name */

    u = find_use_name(atom_name);

    if (only_flag && u == NULL) continue;

    if (u != NULL && u->local_name[0] != '\0')
      strcpy(atom_name, u->local_name);

    st = g95_get_symtree(atom_name, &new_flag);

    if (new_flag) {
      if (st->sym != sym_table[serial]) st->ambiguous = 1;
    } else {
      st->sym = sym_table[serial];
      st->ambiguous = ambiguous;
      st->sym->mark = 0;
    }
  }

  mio_rparen();

/* Get the symbol info */

  mio_lparen();

  for(;;) {
    if (peek_atom() == ATOM_RPAREN) break;

    mio_integer(&i);

    if (sym_table[i]->mark)
      skip_list();
    else {
      memset(&new, '\0', sizeof(g95_symbol));
      mio_symbol(&new);

      new.mark = 0;
      *sym_table[i] = new;
    }
  }

  mio_rparen();

/* Clean up symbol nodes that were never loaded */

  for(i=0; i<n; i++)
    if (sym_table[i]->mark) g95_free(sym_table[i]);

  g95_free(sym_table);
}


/* find_writables()-- Worker function called by g95_traverse_ns to
 * find those symbols that should be written to the module file and
 * assign a number to them.  Symbols that should not be written are
 * assigned a number of -1.  */

static void find_writables(g95_symbol *sym) {

  sym->serial = -1;

  switch(sym->attr.flavor) {
  case FL_UNKNOWN:      case FL_PROGRAM:     case FL_BLOCK_DATA:
  case FL_MODULE:       case FL_LABEL:       case FL_ST_FUNCTION:
  case FL_DUMMY_PROC:  /* Can't happen or don't save cases */
    break;

  case FL_VARIABLE:     case FL_PARAMETER:   case FL_MODULE_PROC:
  case FL_PROCEDURE:    case FL_DERIVED:     case FL_NAMELIST:
  case FL_GENERIC:
    if (sym->attr.private == 0) sym->serial = sym_num++;
    break;
  }
}



/* write_symbol()-- Write a symbol to the module.  Different symbol
 * classes are written at different times in order to be able to
 * always reconstruct things. */

static int phase;

static void write_symbol(g95_symbol *sym) {

  if (sym->serial == -1) return;

  switch(sym->attr.flavor) {
  case FL_DERIVED:
    if (phase == 1) {
      mio_integer(&sym->serial);
      mio_symbol(sym);
    }
    break;

  case FL_VARIABLE:     case FL_PARAMETER:   case FL_MODULE_PROC:
  case FL_PROCEDURE:    case FL_NAMELIST:    case FL_GENERIC:
    if (phase == 2) {
      mio_integer(&sym->serial);
      mio_symbol(sym);
    }
    break;

  default:
    g95_internal_error("write_symbol(): Bad symbol class");
  }
}


/* write_true_name()-- Given a symbol, write it's true name.  For
 * symbols that don't have a module name, we give it the name of the
 * current module. */

static void write_true_name(g95_symbol *sym) {

  if (sym->serial == -1) return;

  if (sym->module[0] == '\0') strcpy(sym->module, g95_state_stack->sym->name);

  write_atom(ATOM_NAME, sym->module);
  write_atom(ATOM_NAME, sym->name);
}


/* write_symtree()-- Write a symtree node that points to a symbol that
 * we are going to save */

static void write_symtree(g95_symtree *st) {

  if (st->sym->serial == -1) return;

  mio_lparen();
  write_atom(ATOM_NAME, st->name);
  write_atom(ATOM_INTEGER, &st->sym->serial);
  write_atom(ATOM_INTEGER, &st->ambiguous);
  mio_rparen();
}


static void write_module(void) {

  mio_lparen();

  sym_num = 0;

  g95_traverse_ns(g95_current_ns, find_writables);

  write_atom(ATOM_INTEGER, &sym_num);

  mio_lparen();
  g95_traverse_ns(g95_current_ns, write_true_name);
  mio_rparen();

  mio_lparen();
  g95_traverse_symtree(g95_current_ns, write_symtree);
  mio_rparen();

/* Write symbols.  The only ordering issue I can think of at the
 * moment is that structure definitions have to be written first in
 * order to reconstruct a component reference in a g95_ref list.  It
 * appears to be OK to write the structure references themselves in
 * any order */

  mio_lparen();

  for(phase=1; phase<=2; phase++)
    g95_traverse_ns(g95_current_ns, write_symbol);

  mio_rparen();
  mio_rparen();
}



/* g95_dump_module()-- Given module, dump it to disk */

void g95_dump_module(char *name) {
char filename[PATH_MAX];

  filename[0] = '\0';
  if (g95_option.module_dir != NULL) strcpy(filename, g95_option.module_dir);

  strcat(filename, name);
  strcat(filename, MODULE_EXTENSION);

  module_fp = fopen(filename, "w");
  if (module_fp == NULL)
    g95_fatal_error("Can't open module file '%s' for writing: %s", 
		    filename, strerror(errno));

  fputs("G95 Module: Do not edit\n\n", module_fp);

  iomode = IO_OUTPUT;

  write_module();

  write_char('\n');

  if (fclose(module_fp))
    g95_fatal_error("Error writing module file '%s' for writing: %s", 
		    filename, strerror(errno));
}


/* g95_use_module()-- Process a USE directive. */

void g95_use_module() {
char filename[G95_MAX_SYMBOL_LEN+5];
g95_state_data *p;
int c;

  strcpy(filename, module_name);
  strcat(filename, MODULE_EXTENSION);

  module_fp = g95_open_included_file(filename);
  if (module_fp == NULL)
    g95_fatal_error("Can't open module file '%s' for reading: %s", 
		    filename, strerror(errno));

  iomode = IO_INPUT;
  module_line = 1;
  module_column = 1;

/* Skip the first line of the module */

  for(;;) {
    c = module_char();
    if (c == EOF) bad_module("Unexpected end of module");
    if (c == '\n') break;
  }

  /* Make sure we're not reading the same module that we may be building */

  for(p=g95_state_stack; p; p=p->previous)
    if (p->state == COMP_MODULE && strcmp(p->sym->name, module_name) == 0)
      g95_fatal_error("Can't USE the same module we're building!");

  read_module();

  fclose(module_fp);

  save_module_names();
}


void g95_module_init_2(void) {

  loaded_modules = NULL;
  new_modules = NULL;

  last_atom = ATOM_LPAREN;
}


void g95_module_done_2(void) {

  free_module_list(loaded_modules);
  free_module_list(new_modules);

  g95_free_rename();
}
