/* Miscellaneous things
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

/* misc.c-- Miscellaneous stuff that doesn't fit anywhere else */

#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>

#include "g95.h"

#ifdef __GLIBC__
#include <mcheck.h>
#endif


/* g95_getmem()-- Get a block of memory.  Many callers assume that the
 * memory we return is zeroed. */

void *g95_getmem(size_t n) {
void *p;

  if (n == 0) return NULL;

  p = calloc(n, 1);
  if (p == NULL) g95_fatal_error("Out of memory-- malloc() failed");
  return p;
}


#define temp free
#undef free

void g95_free(void *p) {

  if (p != NULL) free(p);
}

#define free temp
#undef temp


/* g95_clear_ts()-- Initialize a typespec to unknown. */

void g95_clear_ts(g95_typespec *ts) {

  ts->type = BT_UNKNOWN;
  ts->kind = 0;
  ts->derived = NULL;
  ts->cl = NULL;
}


/* g95_open_file()-- Open a file for reading */

FILE *g95_open_file(char *name) {
struct stat statbuf;

  if (! *name) return stdin;

  if (stat(name, &statbuf) < 0) return NULL;

  if (!S_ISREG(statbuf.st_mode)) return NULL;

  return fopen(name, "r");
}


/* g95_open_included_file()-- opens file for reading, searching
 * through the include directories given if necessary */

FILE *g95_open_included_file(char *name) {
char fullname[PATH_MAX];
g95_directorylist *p;
FILE *f;

  f = g95_open_file(name);
  if (f != NULL) return f;

  for(p=g95_option.include_dirs; p; p=p->next) {
    if (strlen(p->path)+strlen(name)+1 > PATH_MAX) continue;

    strcpy(fullname, p->path);
    strcat(fullname, name);

    f = g95_open_file(fullname);
    if (f != NULL) return f;
  }

  return NULL;
}


/* g95_article()-- Given a word, return the correct article */

char *g95_article(char *word) {
char *p;

  switch(*word) {
  case 'a': case 'A':  case 'e': case 'E':  case 'i': case 'I':
  case 'o': case 'O':  case 'u': case 'U':
    p = "an";
    break;

  default:
    p = "a";
  }

  return p;
}


/* g95_typename()-- Return a string for each type */

char *g95_basic_typename(bt type) {
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
    g95_internal_error("g95_basic_typename(): Undefined type");
  }

  return p;
}


/* g95_typename()-- Return a string descibing the type and kind of a
 * typespec.  Because we return alternating buffers, this subroutine
 * can appear twice in the argument list of a single statement. */

char *g95_typename(g95_typespec *ts) {
static char buffer1[60], buffer2[60];
static int flag = 0;
char *buffer;

  buffer = flag ? buffer1 : buffer2;
  flag = !flag;

  switch(ts->type) {
  case BT_INTEGER:    sprintf(buffer, "INTEGER(%d)", ts->kind);    break;
  case BT_REAL:       sprintf(buffer, "REAL(%d)", ts->kind);       break;
  case BT_COMPLEX:    sprintf(buffer, "COMPLEX(%d)", ts->kind);    break;
  case BT_LOGICAL:    sprintf(buffer, "LOGICAL(%d)", ts->kind);    break;
  case BT_CHARACTER:  sprintf(buffer, "CHARACTER(%d)", ts->kind);  break;
  case BT_DERIVED:    sprintf(buffer, "TYPE(%s)", ts->derived->name); break;
  case BT_PROCEDURE:  strcpy(buffer, "PROCEDURE");  break;
  case BT_UNKNOWN:    strcpy(buffer, "UNKNOWN");    break;
  default:
    g95_internal_error("g95_typespec(): Undefined type");
  }

  return buffer;
}


void g95_show_typespec(g95_typespec *ts) {

  g95_status("(%s ", g95_basic_typename(ts->type));

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


/* g95_intent_string()-- Convert an intent code to a string. */

char *g95_intent_string(sym_intent i) {
static mstring intents[] = {
  minit("UNKNOWN-INTENT", INTENT_UNKNOWN),  minit("IN", INTENT_IN),
  minit("OUT", INTENT_OUT),                 minit("INOUT", INTENT_INOUT),
  minit(NULL, -1)
};

  return g95_code2string(intents, i);
}


/* Initialization functions */

/* g95_init_1()-- Top level initialization */

void g95_init_1(void) {

  g95_error_init_1();
  g95_scanner_init_1();
  g95_arith_init_1();
  g95_intrinsic_init_1();
  g95_iresolve_init_1();
  g95_simplify_init_1();
}


/* g95_init_2()-- Per program unit initialization */

void g95_init_2(void) {

  g95_symbol_init_2();
  g95_module_init_2();
}


/* Destructor functions */

/* g95_done_1()-- Call all of the top level destructors */

void g95_done_1(void) {

  g95_scanner_done_1();
  g95_intrinsic_done_1();
  g95_simplify_done_1();
  g95_iresolve_done_1();
  g95_arith_done_1();
}


/* g95_done_2()-- Per program unit destructors */

void g95_done_2(void) {

  g95_symbol_done_2();
  g95_module_done_2();
}


/* release_options()-- Release resources */

static void release_options(void) {
g95_directorylist *p;

  g95_free(g95_option.module_dir);
  while(g95_option.include_dirs != NULL) {
    p = g95_option.include_dirs;
    g95_option.include_dirs = g95_option.include_dirs->next;
    g95_free(p->path);
    g95_free(p);
  }
}


#ifndef IN_GCC
/* main()-- Compile a fortran program */

int main(int argc, char *argv[]) {
int errors, warnings, i;

  if (argc == 1) g95_display_help();

#ifdef __GLIBC__
  mtrace();
#endif

  g95_init_options();

  argv++;

  while(argc > 1) {
    i = g95_parse_arg(argc, argv);

    argc -= i;
    argv += i;
  }

  g95_init_1();

  if (g95_option.source == NULL) g95_fatal_error("Need a file to compile");

  if (g95_new_file(g95_option.source, g95_option.form) != SUCCESS) return 3;

  g95_parse_file();

  g95_done_1();
  release_options();

  g95_get_errors(&warnings, &errors);

  if (!g95_option.quiet)
    g95_status("Warnings: %d  Errors: %d\n", warnings, errors);

  if (errors > 0) return 2;
  if (warnings > 0) return 1;
  return 0;
}
#endif
