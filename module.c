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
 * The first line of a module is a line warning people not to edit the
 * module.  The rest of the module looks like:
 *
 * <Maximum number of symbols>
 * ( ( <Interface info for UPLUS> )
 *   ( <Interface info for UMINUS> )
 *   ...
 * )
 * ( <Symbol Number (not in order)>
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
 * particular order.  */


#include <string.h>
#include <stdio.h>
#include <ctype.h>
#include <errno.h>
#include <unistd.h>

#include "g95.h"

#define MODULE_EXTENSION ".mod"


/* Structure that descibes a position within a module file */

typedef struct {
  int column, line;
  fpos_t pos;
} module_locus;


/* Structure for holding extra info needed for symbols being read */

typedef struct {
  char true_name[G95_MAX_SYMBOL_LEN+1], module[G95_MAX_SYMBOL_LEN+1];
  g95_symbol *sym;
  enum { UNUSED, NEEDED, USED } state;
  int referenced;
  module_locus where;
} symbol_info;


static symbol_info *sym_table;

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
static int module_line, module_column, sym_num, only_flag;
static enum { IO_INPUT, IO_OUTPUT } iomode;

static g95_use_rename *g95_rename_list;


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
g95_use_rename *tail=NULL, *new;
interface_type type;
int operator;
match m;

  m = g95_match(" %n", module_name);
  if (m != MATCH_YES) return m;

  g95_free_rename();
  only_flag = 0;

  if (g95_match_eos() == MATCH_YES) return MATCH_YES;
  if (g95_match(" ,") != MATCH_YES) goto syntax;

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

      break;

    case INTERFACE_USER_OP:
      strcpy(new->use_name, name);
      /* Fall through */

    case INTERFACE_INTRINSIC_OP:
      new->operator = operator;
      break;
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

static g95_use_rename *find_use_name(const char *name) {
g95_use_rename *u;

  for(u=g95_rename_list; u; u=u->next)
    if (strcmp(u->use_name, name) == 0) return u;

  return NULL;
}


/* find_use_operator()-- Try to find the operator in the current list */

static g95_use_rename *find_use_operator(int operator) {
g95_use_rename *u;

  for(u=g95_rename_list; u; u=u->next)
    if (u->operator == operator) return u;

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

static int find_module(g95_module *m, const char *name) {

  for(; m; m=m->next)
    if (strcmp(name, m->name) == 0) return 1;

  return 0;
}


/* check_module()-- See if a we've seen a particular module name
 * before.  Returns zero if not, nonzero if we have.  As a side
 * effect, not-seen before modules are placed in the new_modules list. */

static int check_module(const char *name) {
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
  ATOM_NAME, ATOM_LPAREN, ATOM_RPAREN, ATOM_INTEGER, ATOM_STRING
} atom_type;

static atom_type last_atom;


/* The name buffer must be at least as long as a symbol name.  Right
 * now it's not clear how we're going to store numeric constants--
 * probably as a hexadecimal string, since this will allow the exact
 * number to be preserved (this can't be done by a decimal
 * representation).  Worry about that later. */

#define MAX_ATOM_SIZE 100

static int atom_int;
static char *atom_string, atom_name[MAX_ATOM_SIZE];



/* bad_module()-- Report problems with a module.  Error reporting is
 * not very elaborate, since this sorts of errors shouldn't really
 * happen.  This subroutine never returns.  */

static void bad_module(const char *message) {
const char *p;

  switch(iomode) {
  case IO_INPUT:   p = "Reading";  break;
  case IO_OUTPUT:  p = "Writing";  break;
  default:         p = "???";      break;
  }    

  fclose(module_fp);

  g95_fatal_error("%s module %s at line %d column %d: %s", p,
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

  if (c == EOF) bad_module("Unexpected EOF");

  if (c == '\n') {
    module_line++;
    module_column = 0;
  }

  module_column++;
  return c;
}


/* parse_string()-- Parse a string constant.  The delimiter is
 * guaranteed to be a single quote. */

static void parse_string(void) {
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

  for(;len>0; len--) {
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
    if (!isalnum(c) && c != '_' && c != '-') break;

    *p++ = c;
    if (++len > G95_MAX_SYMBOL_LEN) bad_module("Name too long");
  }

  *p = '\0';
  set_module_locus(&m);
}


/* parse_atom()-- Read the next atom in the module's input stream. */

static atom_type parse_atom(void) {
int c;

  do {
    c = module_char();
  } while (c == ' ' || c == '\n');

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
const char *p;
atom_type t;

  get_module_locus(&m); 

  t = parse_atom();
  if (t != type) {
    switch(type) {
    case ATOM_NAME:     p = "Expected name";               break;
    case ATOM_LPAREN:   p = "Expected left parenthesis";   break;
    case ATOM_RPAREN:   p = "Expected right parenthesis";  break;
    case ATOM_INTEGER:  p = "Expected integer";            break;
    case ATOM_STRING:   p = "Expected string";             break;
    default: 
      g95_internal_error("require_atom(): bad atom type required");
    }

    set_module_locus(&m);
    bad_module(p);
  }
}


/* find_enum()-- Given a pointer to an mstring array, require that
 * the current input be one of the strings in the array.  We return
 * the enum value. */

static int find_enum(mstring *m) {
int i;

  i = g95_string2code(m, atom_name);
  if (i >= 0) return i;

  bad_module("find_enum(): Enum not found");

  return 0;  /* Not reached */
}


/* Module output subroutines */


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


/* write_atom()-- Write an atom to a module.  The line wrapping isn't
 * perfect, but it should work most of the time.  This isn't that big
 * of a deal, since the file really isn't meant to be read by people
 * anyway. */

static void write_atom(atom_type atom, const void *v) {
char buffer[20];
const char *p;
int i, len;

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
    i = *((const int *) v);
    if (i < 0) g95_internal_error("write_atom(): Writing negative integer");

    sprintf(buffer, "%d", i);
    p = buffer;
    break;
    
  default:
    g95_internal_error("write_atom(): Trying to write dab atom");

  }

  len = strlen(p);

  if (atom != ATOM_RPAREN) {
    if (module_column + len > 72)
      write_char('\n');
    else {

      if (last_atom != ATOM_LPAREN && module_column != 1)
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
static void mio_symbol_ref(g95_symbol **);

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
    g95_free(atom_string);
  }
}



enum { AB_ALLOCATABLE, AB_DIMENSION, AB_EXTERNAL, AB_INTRINSIC, AB_OPTIONAL,
       AB_POINTER, AB_SAVE, AB_TARGET, AB_DUMMY, AB_COMMON, AB_RESULT,
       AB_ENTRY, AB_DATA, AB_IN_NAMELIST, AB_IN_COMMON, AB_SAVED_COMMON,
       AB_FUNCTION, AB_SUBROUTINE, AB_SEQUENCE, AB_ELEMENTAL, AB_PURE,
       AB_RECURSIVE, AB_GENERIC, AB_INTERFACE
} attribute_bits;


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
  minit("GENERIC",     AB_GENERIC),     minit("INTERFACE",    AB_INTERFACE),
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
};


/* mio_symbol_attribute()-- Symbol attributes are stored in list with
 * the first three elements being the enumerated fields, while the
 * remaining elements (if any) indicate the individual attribute bits.
 * The access field is not saved-- it controls what symbols are
 * exported when a module is written. */

static void mio_symbol_attribute(symbol_attribute *attr) {
atom_type t;

  mio_lparen();

  attr->flavor = mio_name(attr->flavor, flavors);
  attr->intent = mio_name(attr->intent, intents);
  attr->proc = mio_name(attr->proc, procedures);

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
    if (attr->result)        mio_name(AB_RESULT, attr_bits);
    if (attr->entry)         mio_name(AB_ENTRY, attr_bits);
    
    if (attr->data)          mio_name(AB_DATA, attr_bits);
    if (attr->in_namelist)   mio_name(AB_IN_NAMELIST, attr_bits);
    if (attr->in_common)     mio_name(AB_IN_COMMON, attr_bits);
    if (attr->saved_common)  mio_name(AB_SAVED_COMMON, attr_bits);
    if (attr->interface)     mio_name(AB_INTERFACE, attr_bits);

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
      case AB_GENERIC:       attr->generic = 1;       break;
      case AB_SEQUENCE:      attr->sequence = 1;      break;
      case AB_ELEMENTAL:     attr->elemental = 1;     break;
      case AB_PURE:          attr->pure = 1;          break;
      case AB_RECURSIVE:     attr->recursive = 1;     break;
      case AB_INTERFACE:     attr->interface = 1;     break;
      }
    }
  }
}


static mstring bt_types[] = {
  minit("INTEGER",    BT_INTEGER),      minit("REAL",       BT_REAL),
  minit("COMPLEX",    BT_COMPLEX),      minit("LOGICAL",    BT_LOGICAL),
  minit("CHARACTER",  BT_CHARACTER),    minit("DERIVED",    BT_DERIVED),
  minit("PROCEDURE",  BT_PROCEDURE),    minit("UNKNOWN",    BT_UNKNOWN),
  minit(NULL, -1)
};


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


/* get_unique_symtree()-- Return a symtree node with a name that is
 * guaranteed to be unique within the namespace and corresponds to an
 * illegal fortran name */

static g95_symtree *get_unique_symtree(g95_namespace *ns) {
char name[G95_MAX_SYMBOL_LEN+1]; 
static int serial=0;

  sprintf(name, "@%d", serial++); 
  return g95_new_symtree(ns, name);
}


/* check_unique_name()-- See if a name is a generated name. */

static int check_unique_name(const char *name) {

  return *name == '@';
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
    mio_expr(&as->lower[i]);
    mio_expr(&as->upper[i]);
  }

done:
  mio_rparen();
}


static void mio_array_shape(g95_array_shape **asp) {
g95_array_shape *as;
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

    *asp = as = g95_get_array_shape();
  }

  mio_integer(&as->rank);

  for(i=0; i<as->rank; i++)
    mio_expr(&as->shape[i]);

done:
  mio_rparen();
}


#if 0
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
#endif



static void mio_component(g95_component *c) {

  mio_lparen();

  mio_internal_string(c->name);
  mio_typespec(&c->ts);
  mio_array_spec(&c->as);

  mio_integer(&c->dimension);
  mio_integer(&c->pointer);

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

      c = g95_get_component();
      mio_component(c);

      if (c->ts.type == BT_DERIVED) c->ts.derived->mark = 0;

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



/* mio_formal_namespace()-- Read and write a namespace associated with
 * a formal argument list.  These generally won't be that large.  We
 * save symbols that are type definitions and dummy parameters.  Dummy
 * parameters that reference host associated names are saved and
 * restored as such.  */

static void mio_formal_namespace(g95_symbol *sym) {

  if (iomode == IO_OUTPUT) {


  } else {


  }
}



/* mio_formal_ns_name()-- Read and write a list of names within the
 * namespace.  This is necessary because derived type symbols can
 * point to themselves. */

static void mio_formal_ns_names(g95_symbol *sym) {

#if 0
  if (iomode == IO_OUTPUT) {
    g95_traverse_ns(sym->formal_ns) 
  }
#endif

}



/* mio_formal_arglist()-- Read and write formal argument lists.
 * Symbols associated with formal argument lists are never part of the
 * namespace being saved or loaded, so we can't just reference them as
 * symbol numbers like other symbols.  Rather, we have to expand such
 * symbols.
 *
 * The real difficuly is with derived types.  If an argument type is
 * defined by host association, we have to retain the association on
 * reloading the module.  If not, we have to actually expand the type
 * definition so that correct matching can occur if the type is a
 * SEQUENCE type.
 *
 * If there is nothing to be saved or loaded, we just have "()".  The
 * formal list contains three lists-- a list containing the symbol
 * names, a list containing the information for each symbols stored in
 * the namespace, followed by a third list containing the names in the
 * argument list.  This function is mutually recursive with
 * mio_formal_namespace(). */

static void mio_formal_arglist(g95_symbol *sym) {
g95_formal_arglist *f, *tail;
g95_symbol *s;

  mio_lparen();

  if (iomode == IO_OUTPUT) {
    if (sym->formal == NULL) goto done;

    mio_formal_ns_names(sym);
    mio_formal_namespace(sym);

    mio_lparen();
    for(f=sym->formal; f; f=f->next)
      write_atom(ATOM_STRING, f->sym->name);
    mio_rparen();

  } else {
    if (peek_atom() != ATOM_LPAREN) goto done;

    mio_formal_namespace(sym);
    tail = NULL;

    mio_lparen();
    
    while(peek_atom() != ATOM_RPAREN) {
      require_atom(ATOM_STRING);

      if (g95_find_symbol(atom_string, sym->formal_ns, 0, &s))
	bad_module("mio_formal_arglist(): Formal argument not found");

      f = g95_get_formal_arglist();
      f->sym = s;

      if (sym->formal == NULL)
	sym->formal = tail = f;
      else
	tail->next = f;
    }
  }

 done:
  mio_rparen();
}


/* mio_symbol_ref()-- Saves a *reference* to a symbol.  Symbols are
 * identified by symbol numbers stored in the 'serial' member of
 * symbol nodes that start at zero.  During writing, if we are
 * referencing a symbol without a number (ie sym->serial == -1), we
 * give it one.  This forces it to be written later if it hasn't
 * already been written. */

static void mio_symbol_ref(g95_symbol **symp) {
int i;

  if (iomode == IO_OUTPUT) {
    if (*symp != NULL) {
      i = (*symp)->serial;

      if (i == -1) (*symp)->serial = i = sym_num++;
      if (i < 0 || i >= sym_num) bad_module("Symbol number out of range");

      write_atom(ATOM_INTEGER, &i);
    } else {
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
      *symp = sym_table[atom_int].sym;

      if (sym_table[atom_int].state == UNUSED)
	sym_table[atom_int].state = NEEDED;
    }
  }
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



static void mio_constructor(g95_constructor **cp) {
g95_constructor *c, *tail;

  mio_lparen();

  if (iomode == IO_OUTPUT) {
    for(c=*cp; c; c=c->next) {
      mio_lparen();
      mio_expr(&c->expr);
      mio_iterator(&c->iterator);
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
      mio_iterator(&c->iterator);
      mio_rparen();
    }
  }

  mio_rparen();
}


#if 0
static mstring ref_types[] = {
  minit("ARRAY", REF_ARRAY),            minit("COMPONENT", REF_COMPONENT),
  minit("SUBSTRING", REF_SUBSTRING),    minit(NULL, -1)
};
#endif



static void mio_ref(g95_ref **r) {

#if 0
char name[G95_MAX_SYMBOL_LEN+1];

  mio_lparen();

  r->type = mio_name(r->type, ref_types);

#if 0
  mio_symbol_ref(&r->symbol);
#endif

  if (iomode == IO_OUTPUT) {

    write_atom(ATOM_NAME, r->component->name);
  } else {
    

  }

  mio_rparen();
#endif
}



#if 0
static void mio_ref_list(g95_ref **rp) {


}
#endif



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
  minit("ARRAY",      EXPR_ARRAY),      minit("NULL",       EXPR_NULL),
  minit(NULL, -1)
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

    e = *ep = g95_get_expr();
    e->expr_type = find_enum(expr_types);
  }

  mio_typespec(&e->ts);
  mio_array_shape(&e->shape);

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

  case EXPR_NULL:
    break;
  }

  mio_rparen();
}


/* mio_interface()-- Save/restore lists of g95_interface stuctures */

static void mio_interface(g95_interface **ip) {
g95_interface *head, *tail, *new;

  mio_lparen();

  if (iomode == IO_OUTPUT) {
    if (ip != NULL)
      for(head=*ip; head; head=head->next)
	mio_symbol_ref(&head->sym);
  } else {
    head = tail = NULL;

    for(;;) {
      if (peek_atom() == ATOM_RPAREN) break;

      new = g95_get_interface();
      mio_symbol_ref(&new->sym);

      if (tail == NULL)
	head = new;
      else
	tail->next = new;

      tail = new;
    }

    *ip = head;
  }

  mio_rparen();
}


/* mio_symbol()-- Unlike most other routines, the address of the
 * symbol node is already fixed on input and the name/module has
 * already been filled in */

static void mio_symbol(g95_symbol *sym) {

  mio_lparen();

  mio_symbol_attribute(&sym->attr);
  mio_typespec(&sym->ts);

  mio_interface(&sym->operator);
  mio_interface(&sym->generic);

  mio_symbol_ref(&sym->common_head);  /* Save/restore common block links */
  mio_symbol_ref(&sym->common_next);
  mio_formal_arglist(sym);

  mio_expr(&sym->value);
  mio_array_spec(&sym->as);

  mio_symbol_ref(&sym->result);

/* Note that components are always saved, even if they are supposed
 * to be private.  Component access is checked during searching */

  mio_component_list(&sym->components);

  if (sym->components != NULL)
    sym->component_access = mio_name(sym->component_access, access_types);

  mio_symbol_ref(&sym->common_head);
  mio_symbol_ref(&sym->common_next);

  /* Save/restore namespaces */

  mio_rparen();
}


/************************* Top level subroutines *************************/

static char *search_name, *search_module;
static g95_symbol *search_result;

static void find_true_name(g95_symtree *symtree) {
g95_symbol *sym;

  sym = symtree->sym;

  if (strcmp(sym->name, search_name) == 0 &&
      strcmp(sym->module, search_module) == 0) 
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
      break;
    }
  } while(level > 0);
}


static void read_namespace(g95_namespace *ns) {
int i, flag, sym_save, ambiguous, symbol;
symbol_info *sym_table_save, *info;
module_locus operator_interfaces;
char name[G95_MAX_SYMBOL_LEN+1];
g95_interface *head, *tail;
g95_use_rename *u;
g95_symtree *st;

  sym_table_save = sym_table; 
  sym_save = sym_num;

  mio_integer(&sym_num);

  get_module_locus(&operator_interfaces);  /* Skip these for now */
  skip_list();

  sym_table = g95_getmem(sym_num*sizeof(symbol_info));

  mio_lparen();

  for(i=0; peek_atom()!=ATOM_RPAREN; i++) {
    require_atom(ATOM_INTEGER);
    if (atom_int < 0 || atom_int > sym_num)
      bad_module("Symbol number out of range");

    info = &sym_table[atom_int];
    info->state = UNUSED;

    mio_internal_string(info->true_name);
    mio_internal_string(info->module);

    get_module_locus(&info->where);

    skip_list();

    /* See if the symbol has already been loaded by a previous module.
     * If so, we reference the existing symbol and prevent it from
     * being loaded again. */

    if (check_module(info->module)) {  /* Search via brute force traversal */
      search_name = info->true_name;
      search_module = info->module;
      search_result = NULL;

      g95_traverse_symtree(ns, find_true_name);

      if (search_result != NULL) {
	info->state = USED;
	info->sym = search_result;
	info->referenced = 1;
	continue;
      }
    }

    info->sym = g95_new_symbol(info->true_name, ns);
    strcpy(info->sym->module, info->module);
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

    info = sym_table + symbol;

    if (symbol < 0 || symbol >= sym_num)
      bad_module("Symbol number out of range");

    /* See what we need to do with this name */

    u = find_use_name(name);
    if (u != NULL) u->found = 1;

    if (only_flag && u == NULL) continue;

    if (u != NULL && u->local_name[0] != '\0')
      strcpy(name, u->local_name);

    st = g95_find_symtree(ns, name);

    if (st != NULL) {
      if (st->sym != info->sym)
	st->ambiguous = 1;
    } else {

      st = check_unique_name(name) ? get_unique_symtree(ns) :
	g95_new_symtree(ns, name);

      st->sym = info->sym;
      st->ambiguous = ambiguous;
      st->sym->refs++;

      if (info->state == UNUSED) info->state = NEEDED;
      info->referenced = 1;
    }
  }

  mio_rparen();

  /* Load intrinsic operator interfaces. */

  set_module_locus(&operator_interfaces);
  mio_lparen();

  for(i=0; i<G95_INTRINSIC_OPS; i++) {
    if (only_flag) {
      u = find_use_operator(i);

      if (u == NULL) {
	skip_list();
	continue;
      }

      u->found = 1;
    }

    mio_interface(&head);

    if (head != NULL) {
      for(tail=head; tail->next; tail=tail->next);

/* TODO: Make sure new interfaces don't conflict with others already loaded */

      tail->next = ns->operator[i];
      ns->operator[i] = head;
    }
  }

  /* At this point, we read those symbols that are needed but haven't
   * been loaded yet.  If one symbol requires another, the other gets
   * marked as NEEDED if it's previous state was UNUSED. */

  flag = 1;

  while(flag) {
    flag = 0;

    for(i=0; i<sym_num; i++) {
      info = sym_table + i;

      if (info->state != NEEDED) continue;

      flag = 1;
      info->state = USED;

      set_module_locus(&info->where);

      strcpy(info->sym->name, info->true_name);
      strcpy(info->sym->module, info->module);

      mio_symbol(info->sym);

      info->sym->attr.use_assoc = 1;
    }
  }

/* Make sure all elements of the rename-list were found in the module */

  for(u=g95_rename_list; u; u=u->next) {
    if (u->found) continue;

    if (u->operator == -1) {
      g95_error("Symbol '%s' referenced at %L not found in module '%s'",
		u->use_name, &u->where, module_name);
      continue;
    }
       
    if (u->operator == INTRINSIC_USER) {
      g95_error("User operator '%s' referenced at %L not found in module '%s'",
		u->use_name, &u->where, module_name);
      continue;
    }

    g95_error("Intrinsic operator '%s' referenced at %L not found in module "
	      "'%s'", g95_op2string(u->operator), &u->where, module_name);
  }

/* Clean up symbol nodes that were never loaded, create references to
 * hidden symbols. */

  for(i=0; i<sym_num; i++) {
    info = sym_table + i;

    switch(info->state) {
    case UNUSED:
      g95_free(info->sym);  /* Nothing was ever loaded into the symbol */
      break;

    case USED:
      if (info->referenced) break;

      st = get_unique_symtree(ns);
      st->sym = info->sym;
      break;

    default:
      bad_module("read_namespace(): Symbol in bad state");
    }
  }

  g95_free(sym_table);

  sym_table = sym_table_save;
  sym_num = sym_save;
}


/* check_access()-- Given an access type that is specific to an entity
 * and the default access, return nonzero if we should write the
 * entity. */

static int check_access(g95_access specific_access,
			g95_access default_access) {

  if (specific_access == ACCESS_PUBLIC) return 1;

  if (default_access != ACCESS_PRIVATE &&
      specific_access != ACCESS_PRIVATE) return 1;

  return 0;
}


/* write_symbol()-- Write a symbol to the module.  If the symbol is
 * unreferenced, we check the access settings to see if we should
 * write it. */

static int *symbol_written;    /* Array of flags */

static void write_symbol(g95_symbol *sym) {

  if (sym->attr.flavor == FL_UNKNOWN || sym->attr.flavor == FL_LABEL)
    g95_internal_error("write_symbol(): bad module symbol '%s'", sym->name);

  if (sym->serial == -1) {
    if (!check_access(sym->attr.access, sym->ns->default_access)) return;
    sym->serial = sym_num++;
  }

  if (symbol_written[sym->serial]) return;
  symbol_written[sym->serial] = 1;

  mio_integer(&sym->serial);
  mio_internal_string(sym->name);
  mio_internal_string(sym->module);
  mio_symbol(sym);
  write_char('\n');
}



static void write_symtree(g95_symtree *st) {
g95_symbol *sym;

  sym = st->sym;
  if (!check_access(sym->attr.access, sym->ns->default_access)) return;

  if (sym->serial == -1)
    g95_internal_error("write_symtree(): Symbol not written");

  mio_internal_string(st->name);
  mio_integer(&st->ambiguous);
  mio_integer(&sym->serial);
}


/* count_symbols()-- Work function to count symbols and initialize the
 * serial number and possibly module name. */

static void count_symbols(g95_symbol *sym) {

  sym_num++;
  sym->serial = -1;
  if (sym->module[0] == '\0') strcpy(sym->module, module_name);
}


static void write_namespace(g95_namespace *ns) {
module_locus m1, m2;
int i;

  sym_num = 0;
  g95_traverse_ns(ns, count_symbols);

  mio_integer(&sym_num);

  symbol_written = g95_getmem(sym_num * sizeof(int));

  for(i=0; i<sym_num; i++)
    symbol_written[i] = 0;

  write_char('\n');  write_char('\n');

  sym_num = 0;     /* This is now the counter for new symbol numbers */

  /* Write the operator interfaces */

  mio_lparen();

  for(i=0; i<G95_INTRINSIC_OPS; i++)
    mio_interface(check_access(ns->operator_access[i], ns->default_access)
		  ? &ns->operator[i] : NULL);

  mio_rparen();

  write_char('\n');  write_char('\n');

  /* Write symbol information.  We do a loop that traverses the
   * namespace writing symbols that need to be written.  Sometimes
   * writing one symbol will cause another to need to be written, so
   * we keep looping until we do a full traversal without writing any
   * symbols.  The reading algorithm doesn't care what order the
   * symbol appear in. */

  mio_lparen();
  get_module_locus(&m1);

  for(;;) {
    g95_traverse_ns(ns, write_symbol);

    get_module_locus(&m2);
    if (m1.pos == m2.pos) break;
    m1 = m2;
  }

  mio_rparen();
  g95_free(symbol_written);

  write_char('\n');  write_char('\n');

  mio_lparen();
  g95_traverse_symtree(ns, write_symtree);
  mio_rparen();
}


/* g95_dump_module()-- Given module, dump it to disk.  If there was an
 * error while processing the module, dump_flag will be set to zero
 * and we delete the module file, even if it was already there. */

void g95_dump_module(const char *name, int dump_flag) {
char filename[PATH_MAX];

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

  fputs("G95 Module: Do not edit\n\n", module_fp);

  iomode = IO_OUTPUT;
  strcpy(module_name, name);

  write_namespace(g95_current_ns);

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

  read_namespace(g95_current_ns);

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
