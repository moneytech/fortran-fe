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

#include "g95.h"


#define MODULE_EXTENSION ".mod"

/* Lists of rename info for the USE statement */

typedef struct g95_use_rename {
  char local_name[G95_MAX_SYMBOL_LEN+1], use_name[G95_MAX_SYMBOL_LEN+1];
  struct g95_use_rename *next;
} g95_use_rename;

#define g95_get_use_rename() g95_getmem(sizeof(g95_use_rename))


static char module_name[G95_MAX_SYMBOL_LEN+1];
static g95_use_rename *g95_rename_list;
static int only_flag;


static FILE *input;
static int module_line, module_column;


/* g95_free_rename()-- Free the rename list left behind by a USE
 * statement. */

void g95_free_rename(void) {
g95_use_rename *next;

  for( ;g95_rename_list; g95_rename_list=next) {
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





/* Module reading and writing */

typedef enum {
  ATOM_NAME, ATOM_LPAREN, ATOM_RPAREN, ATOM_INTEGER, ATOM_STRING, ATOM_EOF
} atom_type;


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
static char *current_module, *atom_string, atom_name[MAX_ATOM_SIZE];



/* bad_module()-- Report problems with a module.  Error reporting is
 * not very elaborate, since this sorts of errors shouldn't really
 * happen. */

static void bad_module(char *message) {

  g95_fatal_error("Reading module %s at line %d column %d: %s",
		  current_module, module_line, module_column, message);
}


/* set_module_locus()-- Set the module's input pointer */

static void set_module_locus(module_locus *m) {

  module_column = m->column;
  module_line = m->line;
  fsetpos(input, &m->pos);
}


/* get_module_locus()-- Get the module's input pointer so that we can
 * restore it later. */

static void get_module_locus(module_locus *m) {

  m->column = module_column;
  m->line = module_line;
  fgetpos(input, &m->pos);
}


/* module_char()-- Get the next character in the module, updating our
 * reckoning of where we are. */

static int module_char(void) {
int c;

  c = fgetc(input);

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
  } while (c != ' ' && c != '\n');

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









/* g95_dump_module()-- Given module, dump it to disk */

void g95_dump_module(char *name) {
char filename[G95_MAX_SYMBOL_LEN+5];

  strcpy(filename, name);
  strcat(filename, MODULE_EXTENSION);

  if (g95_open_status(filename) == FAILURE) return;

  g95_status("G95 Module: Do not edit\n");


  g95_close_status();
}
