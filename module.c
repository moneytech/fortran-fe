/* Module module
   Copyright (C) 2000, 2001, 2002 Free Software Foundation, Inc.
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

/* Stack for storing lists of symbols that need to be written */

typedef struct sym_stack {
  g95_symbol *sym;
  struct sym_stack *next;
} sym_stack;


/* Local variables */

static FILE *module_fp;

static char module_name[G95_MAX_SYMBOL_LEN+1];
static int module_line, module_column, sym_num, only_flag;
static enum { IO_INPUT, IO_OUTPUT } iomode;

static g95_use_rename *g95_rename_list;
static sym_stack *write_stack;


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

  m = g95_match_name(module_name);
  if (m != MATCH_YES) return m;

  g95_free_rename();
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
  g95_free_rename();
  return MATCH_ERROR;
}


/* find_use_name()-- Given a name, return the name under which to load
 * this symbol.  Returns NULL if this symbol shouldn't be loaded. */

static char *find_use_name(char *name) {
g95_use_rename *u;

  for(u=g95_rename_list; u; u=u->next)
    if (strcmp(u->use_name, name) == 0) break;

  if (u == NULL) return only_flag ? NULL : name;

  u->found = 1;

  return (u->local_name[0] != '\0') ? u->local_name : name;
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

  get_module_locus(&m);

  for(;;) {
    c = module_char();
    if (!isalnum(c) && c != '_' && c != '-') break;

    *p++ = c;
    if (++len > G95_MAX_SYMBOL_LEN) bad_module("Name too long");
  }

  *p = '\0';

  fseek(module_fp, -1, SEEK_CUR);
  module_column = m.column + len - 1;

  if (c == '\n') module_line--;
}


/* parse_atom()-- Read the next atom in the module's input stream. */

static atom_type parse_atom(void) {
int c;

  do {
    c = module_char();
  } while (c == ' ' || c == '\n');

  switch(c) {
  case '(':
    return ATOM_LPAREN;

  case ')':
    return ATOM_RPAREN;

  case '\'':
    parse_string();
    return ATOM_STRING;

  case '0':  case '1':  case '2':  case '3':  case '4':
  case '5':  case '6':  case '7':  case '8':  case '9':
    parse_integer(c);
    return ATOM_INTEGER;

  case 'a': case 'b': case 'c': case 'd': case 'e': case 'f': case 'g':
  case 'h': case 'i': case 'j': case 'k': case 'l': case 'm': case 'n':
  case 'o': case 'p': case 'q': case 'r': case 's': case 't': case 'u':
  case 'v': case 'w': case 'x': case 'y': case 'z': case 'A': case 'B':
  case 'C': case 'D': case 'E': case 'F': case 'G': case 'H': case 'I':
  case 'J': case 'K': case 'L': case 'M': case 'N': case 'O': case 'P':
  case 'Q': case 'R': case 'S': case 'T': case 'U': case 'V': case 'W':
  case 'X': case 'Y': case 'Z':
    parse_name(c);
    return ATOM_NAME;

  default:
    bad_module("Bad name");
  }

  return 0;   /* Not reached */
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
    if (attr->result)        mio_name(AB_RESULT, attr_bits);
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
  return g95_new_symtree(&ns->sym_root, name);
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


/* g95_spec_from_ref()-- Given a pointer to an array reference
 * structure (which lives in a g95_ref structure), find the
 * corresponding array specification structure.  Storing the pointer
 * in the ref structure doesn't quite work when loading from a module.
 * Generating code for an array reference also needs more infomation
 * than just the array spec. */


static mstring array_ref_types[] = {
  minit("FULL", AR_FULL),         minit("ELEMENT", AR_ELEMENT),
  minit("SECTION", AR_SECTION),   minit(NULL, -1)
};


static void mio_array_ref(g95_array_ref *ar) {
int i;

  mio_lparen();
  ar->type = mio_name(ar->type, array_ref_types);
  mio_integer(&ar->dimen);

  switch(ar->type) {
  case AR_FULL:
    break;

  case AR_ELEMENT:
    for(i=0; i<ar->dimen; i++)
      mio_expr(&ar->start[i]);

    break;

  case AR_SECTION:
    for(i=0; i<ar->dimen; i++) {
      mio_expr(&ar->start[i]);
      mio_expr(&ar->end[i]);
      mio_expr(&ar->stride[i]);
    }

    break;

  case AR_UNKNOWN:
    g95_internal_error("mio_array_ref(): Unknown array ref");
  }

  for(i=0; i<ar->dimen; i++)
    mio_integer((int *) &ar->dimen_type[i]);

  if (iomode == IO_INPUT) {
    ar->where = *g95_current_locus();

    for(i=0; i<ar->dimen; i++)
      ar->c_where[i] = *g95_current_locus();
  }

  mio_rparen();
}


static void mio_component_ref(g95_component **cp, g95_symbol *sym) {
char name[G95_MAX_SYMBOL_LEN+1];
g95_component *c;

  if (iomode == IO_OUTPUT)
    mio_internal_string((*cp)->name);
  else {
    mio_internal_string(name);

    for(c=sym->components; c; c=c->next)
      if (strcmp(c->name, name) == 0) break;

    if (c == NULL) bad_module("mio_component_ref(): Can't find component");

    *cp = c;
  }
}


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


/* mio_formal_arglist()-- Read and write formal argument lists. */

static void mio_formal_arglist(g95_symbol *sym) {
g95_formal_arglist *f, *tail;

  mio_lparen();

  if (iomode == IO_OUTPUT) {
    for(f=sym->formal; f; f=f->next)
      mio_symbol_ref(&f->sym);

  } else {
    sym->formal = tail = NULL;

    while(peek_atom() != ATOM_RPAREN) {
      f = g95_get_formal_arglist();
      mio_symbol_ref(&f->sym);

      if (sym->formal == NULL)
	sym->formal = f;
      else
	tail->next = f;

      tail = f;
    }
  }

  mio_rparen();
}


/* mio_symbol_ref()-- Saves a *reference* to a symbol.  Symbols are
 * identified by symbol numbers stored in the 'serial' member of
 * symbol nodes that start at zero.  During writing, if we are
 * referencing a symbol without a number (ie sym->serial == -1), we
 * give it one.  This forces it to be written later if it hasn't
 * already been written. */

static void mio_symbol_ref(g95_symbol **symp) {
sym_stack *p;
int i;

  if (iomode == IO_OUTPUT) {
    if (*symp != NULL) {
      i = (*symp)->serial;

      if (i == -1) {   /* Assign new number, save on the stack */
	(*symp)->serial = i = sym_num++;
	p = g95_getmem(sizeof(sym_stack));
	p->sym = *symp;
	p->next = write_stack;
	write_stack = p;
      }

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



static mstring ref_types[] = {
  minit("ARRAY", REF_ARRAY),            minit("COMPONENT", REF_COMPONENT),
  minit("SUBSTRING", REF_SUBSTRING),    minit(NULL, -1)
};


static void mio_ref(g95_ref **rp) {
g95_ref *r;

  mio_lparen();

  r = *rp; 
  r->type = mio_name(r->type, ref_types);

  switch(r->type) {
  case REF_ARRAY:
    mio_array_ref(&r->u.ar);
    break;

  case REF_COMPONENT:
    mio_symbol_ref(&r->u.c.sym);
    mio_component_ref(&r->u.c.component, r->u.c.sym);
    break;    

  case REF_SUBSTRING:
    mio_expr(&r->u.ss.start);
    mio_expr(&r->u.ss.end);
    mio_charlen(&r->u.ss.length);
    break;
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
    e->where = *g95_current_locus();
    e->expr_type = find_enum(expr_types);
  }

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
    mio_ref_list(&e->ref);
    break;

  case EXPR_SUBSTRING:
    mio_allocated_string(&e->value.character.string);
    mio_expr(&e->op1);
    mio_expr(&e->op2);
    break;

  case EXPR_STRUCTURE:
    mio_symbol_ref(&e->symbol);
    /* Fall through */

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


/* mio_interface()-- Save/restore a nameless operator interface */

static void mio_interface(g95_interface **ip) {

  mio_lparen();
  mio_interface_rest(ip);
}


/* mio_symbol_interface()-- Save/restore a named operator interface */

static void mio_symbol_interface(char *name, char *module,
				 g95_interface **ip) {

  mio_lparen();

  mio_internal_string(name);
  mio_internal_string(module);

  mio_interface_rest(ip);
}


/* mio_symbol()-- Unlike most other routines, the address of the
 * symbol node is already fixed on input and the name/module has
 * already been filled in. */

static void mio_symbol(g95_symbol *sym) {

  mio_lparen();

  mio_symbol_attribute(&sym->attr);
  mio_typespec(&sym->ts);

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

  mio_rparen();
}


/************************* Top level subroutines *************************/

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



/* bf_search()-- Recursive function to search a namespace for a symbol
 * by name and module.  Returns a pointer to the symbol node if found,
 * NULL if not found. */

static g95_symbol *bf_search(g95_symtree *st, symbol_info *info) {
g95_symbol *result;

  if (st == NIL) return NULL;

  result = st->n.sym;

  if (strcmp(result->name, info->true_name) == 0 &&
      strcmp(result->module, info->module) == 0)
    return result;

  result = bf_search(st->left, info);
  if (result != NULL) return result;

  return bf_search(st->right, info);
}


/* bf_sym_search()-- Given a sym_info pointer, search the current and
 * parent namespaces for the symbol's true name.  Because of renaming,
 * the symbol might be named something different or be inaccessible,
 * so a brute-force traversal is required.  Returns a pointer to the
 * symbol, or NULL if not found. */

static g95_symbol *bf_sym_search(symbol_info *info) {
g95_symbol *result;
g95_namespace *ns;

  if (!check_module(info->module)) return NULL; /* Module not loaded */

  for(ns=g95_current_ns; ns; ns=ns->parent) {
    result = bf_search(ns->sym_root, info);
    if (result != NULL) return result;
  }

  return NULL;
}


/* load_operator_interfaces()-- Load operator interfaces from the
 * module.  Interfaces are unusual in that they attach themselves to
 * existing symbols. */

static void load_operator_interfaces(void) {
char *p, name[G95_MAX_SYMBOL_LEN+1], module[G95_MAX_SYMBOL_LEN+1];
g95_user_op *uop;

  mio_lparen();

  while(peek_atom() != ATOM_RPAREN) {
    mio_lparen();

    mio_internal_string(name);
    mio_internal_string(module);

    /* Decide if we need to load this one or not */

    p = find_use_name(name);
    if (p == NULL) {
      while(parse_atom() != ATOM_RPAREN);
    } else {
      uop = g95_get_uop(p);
      mio_interface_rest(&uop->operator);
    }
  }

  mio_rparen();
}


/* load_generic_interfaces()-- Load interfaces from the module.
 * Interfaces are unusual in that they attach themselves to existing
 * symbols.  */

static void load_generic_interfaces(void) {
char *p, name[G95_MAX_SYMBOL_LEN+1], module[G95_MAX_SYMBOL_LEN+1];
g95_symbol *sym;

  mio_lparen();

  while(peek_atom() != ATOM_RPAREN) {
    mio_lparen();

    mio_internal_string(name);
    mio_internal_string(module);

    /* Decide if we need to load this one or not */

    p = find_use_name(name);

    if (p == NULL || g95_find_symbol(p, NULL, 0, &sym)) {
      while(parse_atom() != ATOM_RPAREN);
      continue;
    }

    if (sym == NULL) {
      g95_get_symbol(p, NULL, 0, &sym);

      sym->attr.flavor = FL_PROCEDURE;
      sym->attr.generic = 1;
      sym->attr.use_assoc = 1;
    }

    mio_interface_rest(&sym->generic);
  }

  mio_rparen();
}


static void read_namespace(g95_namespace *ns) {
module_locus operator_interfaces, user_operators, symbols;
char *p, name[G95_MAX_SYMBOL_LEN+1];
int i, flag, ambiguous, symbol;
symbol_info *info;
g95_use_rename *u;
g95_symtree *st;
g95_symbol *sym;

  get_module_locus(&operator_interfaces);  /* Skip these for now */
  skip_list();

  get_module_locus(&user_operators);
  skip_list();
  skip_list();

  /* Figure out what the largest symbol number is */

  mio_lparen();

  get_module_locus(&symbols);

  sym_num = 0;

  while(peek_atom()!=ATOM_RPAREN) {
    require_atom(ATOM_INTEGER);
    if (atom_int > sym_num) sym_num = atom_int;

    require_atom(ATOM_STRING);
    g95_free(atom_string);

    require_atom(ATOM_STRING);
    g95_free(atom_string);

    skip_list();
  }

  sym_num++;
  sym_table = g95_getmem(sym_num*sizeof(symbol_info));

  set_module_locus(&symbols);

  while(peek_atom()!=ATOM_RPAREN) {
    require_atom(ATOM_INTEGER);
    if (atom_int < 0 || atom_int > sym_num)
      bad_module("Symbol number out of range");

    info = sym_table + atom_int;
    info->state = UNUSED;

    mio_internal_string(info->true_name);
    mio_internal_string(info->module);

    get_module_locus(&info->where);
    skip_list();

    /* See if the symbol has already been loaded by a previous module.
     * If so, we reference the existing symbol and prevent it from
     * being loaded again. */

    sym = bf_sym_search(info);

    if (sym != NULL) {
      info->state = USED;
      info->sym = sym;
      info->referenced = 1;
    } else {
      info->sym = g95_new_symbol(info->true_name, ns);
      strcpy(info->sym->module, info->module);
    }
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

    /* See what we need to do with this name. */

    p = find_use_name(name);
    if (p == NULL) continue;

    st = g95_find_symtree(ns->sym_root, p);

    if (st != NULL) {
      if (st->n.sym != info->sym) st->ambiguous = 1;
    } else {
      st = check_unique_name(p) ? get_unique_symtree(ns) :
	g95_new_symtree(&ns->sym_root, p);

      st->n.sym = info->sym;
      st->ambiguous = ambiguous;
      st->n.sym->refs++;

      if (info->state == UNUSED) info->state = NEEDED;
      info->referenced = 1;
    }
  }

  mio_rparen();

  /* Load intrinsic operator interfaces. */

  set_module_locus(&operator_interfaces);
  mio_lparen();

  for(i=0; i<G95_INTRINSIC_OPS; i++) {
    if (i == INTRINSIC_USER) continue;

    if (only_flag) {
      u = find_use_operator(i);

      if (u == NULL) {
	skip_list();
	continue;
      }

      u->found = 1;
    }

    mio_interface(&ns->operator[i]);
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

  g95_check_interfaces(g95_current_ns);

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
      st->n.sym = info->sym;
      break;

    default:
      bad_module("read_namespace(): Symbol in bad state");
    }
  }

  g95_free(sym_table);
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


/* write_symbol()-- Write a symbol to the module. */

static void write_symbol(g95_symbol *sym) {

  if (sym->attr.flavor == FL_UNKNOWN || sym->attr.flavor == FL_LABEL)
    g95_internal_error("write_symbol(): bad module symbol '%s'", sym->name);

  if (sym->written) return;
  sym->written = 1;

  mio_integer(&sym->serial);
  mio_internal_string(sym->name);

  if (sym->module[0] == '\0') strcpy(sym->module, module_name);
  mio_internal_string(sym->module);

  mio_symbol(sym);
  write_char('\n');
}


/* write_symbol0()-- Function called by recursive traversal to write
 * the initial set of symbols to the module.  We check to see if the
 * symbol should be written according to the access specification. */

static void write_symbol0(g95_symbol *sym) {

  if (sym->attr.flavor == FL_PROCEDURE && sym->attr.generic &&
      !sym->attr.subroutine && !sym->attr.function) return;

  if (sym->serial == -1) {
    if (!check_access(sym->attr.access, sym->ns->default_access)) return;
    sym->serial = sym_num++;
  }

  write_symbol(sym);
}


/* write_operator()-- Write operator interfaces associated with a symbol. */

static void write_operator(g95_user_op *uop) {
static char nullstring[] = "";

  if (uop->operator == NULL ||
      !check_access(uop->access, uop->ns->default_access)) return;

  mio_symbol_interface(uop->name, nullstring, &uop->operator);
}


/* write_generic()-- Write generic interfaces associated with a symbol. */

static void write_generic(g95_symbol *sym) {

  if (sym->generic == NULL ||
      !check_access(sym->attr.access, sym->ns->default_access)) return;

  mio_symbol_interface(sym->name, sym->module, &sym->generic);
}


static void write_symtree(g95_symtree *st) {
g95_symbol *sym;

  sym = st->n.sym;
  if (!check_access(sym->attr.access, sym->ns->default_access) ||
      (sym->attr.flavor == FL_PROCEDURE && sym->attr.generic &&
       !sym->attr.subroutine && !sym->attr.function)) return;

  if (sym->serial == -1)
    g95_internal_error("write_symtree(): Symbol not written");

  if (check_unique_name(st->name)) return;

  mio_internal_string(st->name);
  mio_integer(&st->ambiguous);
  mio_integer(&sym->serial);
}


static void write_namespace(g95_namespace *ns) {
g95_symbol *sym;
sym_stack *p;
int i;

  sym_num = 0;     /* Counter for new symbol numbers */

  /* Write the operator interfaces */

  mio_lparen();

  for(i=0; i<G95_INTRINSIC_OPS; i++) {
    if (i == INTRINSIC_USER) continue;

    mio_interface(check_access(ns->operator_access[i], ns->default_access)
		  ? &ns->operator[i] : NULL);
  }

  mio_rparen();
  write_char('\n');  write_char('\n');

  mio_lparen();
  g95_traverse_user_op(ns, write_operator);
  mio_rparen();
  write_char('\n');  write_char('\n');

  mio_lparen();
  g95_traverse_ns(ns, write_generic);
  mio_rparen();
  write_char('\n');  write_char('\n');

  /* Write symbol information.  First we traverse all symbols in the
   * primary namespace, writing those that need to be written.
   * Sometimes writing one symbol will cause another to need to be
   * written.  A list of these symbols ends up on the write stack, and
   * we end by popping the bottom of the stack and writing the symbol
   * until the stack is empty.  */

  mio_lparen();

  write_stack = NULL;

  g95_traverse_ns(ns, write_symbol0);

  while(write_stack != NULL) {
    sym = write_stack->sym;
    p = write_stack;
    write_stack = write_stack->next;
    g95_free(p);

    write_symbol(sym);
  }

  mio_rparen();

  write_char('\n');  write_char('\n');

  mio_lparen();
  g95_traverse_symtree(ns, write_symtree);
  mio_rparen();
}


/* g95_dump_module()-- Given module, dump it to disk.  If there was an
 * error while processing the module, dump_flag will be set to zero
 * and we delete the module file, even if it was already there. */

void g95_dump_module(const char *name, int dump_flag) {
char filename[PATH_MAX], *p;
g95_file *g;
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

  g = g95_current_file;
  while(g->next)
    g = g->next;

  now = time(NULL);
  p = ctime(&now);
  *strchr(p, '\n') = '\0';

  fprintf(module_fp, "G95 module created by %s on %s\n", g->filename, p);
  fputs("If you edit this, you'll get what you deserve.\n\n", module_fp);

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
int c, line;

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
    c = module_char();
    if (c == EOF) bad_module("Unexpected end of module");
    if (c == '\n') line++;
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
