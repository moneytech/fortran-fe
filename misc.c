/* Miscellaneous things
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

/* misc.c-- Miscellaneous stuff that doesn't fit anywhere else */

#include <stdlib.h>
#include <string.h>
#include "g95.h"

g95_option_t g95_option;


/* g95_getmem()-- Get a block of memory.  Many callers assume that the
 * memory we return is zeroed. */

void *g95_getmem(int n) {
void *p;

  p = calloc(n, 1);
  if (p == NULL) g95_fatal_error("Out of memory-- malloc() failed");
  return p;
}


#define temp free
#undef free

void g95_free(void *p) {

  free(p);
}

#define free temp
#undef temp


/* add_path()-- adds path to the list pointed to by list */

static void add_path(g95_directorylist **list, const char *path) {
g95_directorylist *this;
const char *p;

  p = path; 
  while (*p == ' ' || *p == '\t') /* someone might do 'g95 "-I include"' */
    if (*p++ == '\0') return;

  this = *list; 
  if (!this) {
    this = *list = (g95_directorylist *)g95_getmem(sizeof(g95_directorylist));
  } else {
    while(this->next) {
      this = this->next;
    }
    this->next = (g95_directorylist *)g95_getmem(sizeof(g95_directorylist));
    this = this->next;
  }

  this->next = NULL;
  this->path = (char *)g95_getmem(strlen(p)+1);
  strcpy(this->path, p);
  strcpy(&this->path[strlen(p)], "/"); /* make '/' last character */ 
}



/* g95_open_file()-- opens file for reading, searching thorugh list if necessary */

FILE *g95_open_file(g95_directorylist *list, const char *name) {
g95_directorylist *p;
FILE *f;
char fullname[PATH_MAX];

  f = fopen(name, "r"); 
  if (f != NULL) return f;

  p = list;
  while(p != NULL) {
    strcpy(fullname, p->path);
    strcat(fullname, name);
    f = fopen(fullname, "r");
    if (f != NULL) return f;
    p = p->next;
  }

  return NULL;
}
    


/* g95_typename()-- Return a string for each type */

char *g95_typename(bt type) {
char *p;

  switch(type) {
  case BT_INTEGER:    p = "INTEGER";    break;
  case BT_REAL:       p = "REAL";       break;
  case BT_COMPLEX:    p = "COMPLEX";    break;
  case BT_LOGICAL:    p = "LOGICAL";    break;
  case BT_CHARACTER:  p = "CHARACTER";  break;
  case BT_DERIVED:    p = "DERIVED";    break;
  case BT_PROCEDURE:  p = "PROCEDURE";  break;
  case BT_UNKNOWN:    p = "UNKNOWN";    break;
  default:
    g95_internal_error("g95_typename(): Undefined type");
  }

  return p;
}


void g95_show_typespec(g95_typespec *ts) {

  g95_status("(%s ", g95_typename(ts->type));

  switch(ts->type) {
  case BT_DERIVED:
    g95_status("%s", ts->derived->name);
    break;

  case BT_CHARACTER:
    g95_show_expr(ts->cl->length);
    break;

  default:
    g95_status("%d", ts->kind);
    break;
  }

  g95_status(")");
}


/* g95_code2string()-- Given an mstring array and a code, locate the
 * code in the table, returning a pointer to the string. */

char *g95_code2string(mstring *m, int code) {

  while(m->string != NULL) {
    if (m->tag == code) return m->string;
    m++;
  }

  g95_internal_error("g95_code2string(): Bad code");
  return NULL;
}


/* g95_string2code()-- Given an mstring array and a string, returns
 * the value of the tag field.  Returns the final tag if no matches to
 * the string are found. */

int g95_string2code(mstring *m, char *string) {

  for(; m->string != NULL; m++)
    if (strcmp(m->string, string) == 0) return m->tag;

  return m->tag;
}


/* Initialization functions */

/* g95_init_1()-- Top level initialization */

void g95_init_1(void) {

  g95_error_init_1();
  g95_scanner_init_1();
  g95_arith_init_1();
  g95_intrinsic_init_1();
}


/* g95_init_2()-- Per program unit initialization */

void g95_init_2(void) {

  g95_symbol_init_2();
}


/* Destructor functions */

/* g95_done_1()-- Call all of the top level destructors */

void g95_done_1(void) {

  g95_scanner_done_1();
  g95_intrinsic_done_1();
}


/* display_help()-- Display help message and exit */

static void display_help(void) {

  g95_status("GNU Fortran 95 Compiler (C) 2000 Free Software Foundation\n"
    "Compiled " __DATE__ " " __TIME__ "\n\n"
    "Usage: g95 [options] file\n"
    "Options:\n"
    "  --help                  Display this information\n"
    "  -v                      Output namespace and code structures\n"
    "  -Wline-truncation       Warn about truncated source lines\n"
    "  -F                      Parse an F program\n"
    "  -ffixed-line-length-80  80 character line width in fixed mode\n"
    "  -pedantic               Warn about use of non-standard features\n"
    "  -r                      Run the resolution phase\n"
    "  -I[directory]           Add directory to the include file search path\n"
    "\n"
    "See http://g95.sourceforge.net for more information.\n\n");

  exit(0);
}


/* parse_arg()-- Parse an argument on the command line. */

static int parse_arg(int argc, char *argv[]) {
char *option;

  option = argv[0]; 

  if (strcmp(option, "-v") == 0) {
    g95_option.verbose = 1;
    return 1;
  }

  if (strcmp(option, "--help") == 0) display_help();

  if (strcmp(option, "-pedantic") == 0) {
    g95_option.pedantic = 1;
    g95_option.line_truncation = 1;
    return 1;
  }

  if (strcmp(option, "-r") == 0) {
    g95_option.resolve = 1;
    return 1;
  }

  if (strcmp(option, "-F") == 0) {
    g95_option.fmode = 1;
    return 1;
  }

  if (strcmp(option, "-Wline-truncation") == 0) {
    g95_option.line_truncation = 1;
    return 1;
  }

  if (strcmp(option, "-ffixed-line-length-80") == 0) {
    g95_option.fixed_80 = 1;
    return 1;
  }

  if (option[0] == '-' && option[1] == 'I') {
    if (option[2] != '\0') {
      add_path(&g95_option.include_dirs, &option[2]);
      return 1;
    } else {
      if (argv[1][0] == '-') {
	g95_status("g95: Directory required after -I\n");
	exit(3);
      }
      add_path(&g95_option.include_dirs, argv[1]);
      return 2;
    }
  }

  if (option[0] == '-') {
    g95_status("g95: Unrecognised option '%s'\n", option);
    exit(3);
  }

  if (g95_option.source != NULL) {
    g95_status("g95: Second source file '%s' found\n", option);
    exit(3);
  }

  g95_option.source = option;
  return 1;
}


/* init_options()-- Initialize the options structure */

static void init_options(void) {

  g95_option.source = NULL;
  g95_option.include_dirs = NULL;
  g95_option.module_dirs = NULL;
  g95_option.verbose = 0;
  g95_option.pedantic = 0;
  g95_option.resolve = 0;
  g95_option.line_truncation = 0;
  g95_option.fixed_80 = 0;
  g95_option.fmode = 0;
}


/* main()-- Compile a fortran program */

int main(int argc, char *argv[]) {
int errors, warnings, i;

  if (argc == 1) display_help();

  init_options();

  argv++;

  while(argc > 1) {
    i = parse_arg(argc, argv);

    argc -= i;
    argv += i;
  }

  if (g95_option.source == NULL) g95_fatal_error("Need a file to compile");

  g95_init_1();

  if (g95_new_file(g95_option.source, FORM_UNKNOWN) == SUCCESS)
    g95_parse_file();
  else
    return 3;

  g95_done_1();

  g95_get_errors(&warnings, &errors);
  g95_status("Warnings: %d  Errors: %d\n", warnings, errors);

  if (errors > 0) return 2;
  if (warnings > 0) return 1;
  return 0;
}
