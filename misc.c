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

static struct 
{
  const char *option;
  const char *description;
}
lang_options[] = {
#include "lang-options.h"
    { " ", " " } /* Ugly hack to avoid compile error. Will be moved
                    to driver executable anyway */
};


g95_option_t g95_option;


/* g95_getmem()-- Get a block of memory.  Many callers assume that the
 * memory we return is zeroed. */

void *g95_getmem(size_t n) {
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


/* g95_clear_ts()-- Initialize a typespec to unknown. */

void g95_clear_ts(g95_typespec *ts) {

  ts->type = BT_UNKNOWN;
  ts->kind = 0;
  ts->derived = NULL;
  ts->cl = NULL;
}


/* add_path()-- adds path to the list pointed to by list */

static void add_path(g95_directorylist **list, const char *path) {
g95_directorylist *dir;
const char *p;

  p = path; 
  while (*p == ' ' || *p == '\t') /* someone might do 'g95 "-I include"' */
    if (*p++ == '\0') return;

  dir = *list; 
  if (!dir) {
    dir = *list = g95_getmem(sizeof(g95_directorylist));
  } else {
    while(dir->next)
      dir = dir->next;

    dir->next = g95_getmem(sizeof(g95_directorylist));
    dir = dir->next;
  }

  dir->next = NULL;
  dir->path = g95_getmem(strlen(p)+2);
  strcpy(dir->path, p);
  strcat(dir->path, "/");     /* make '/' last character */ 
}


/* g95_open_included_file()-- opens file for reading, searching
 * through the include directories given if necessary */

FILE *g95_open_included_file(const char *name) {
char fullname[PATH_MAX];
g95_directorylist *p;
FILE *f;

  f = fopen(name, "r"); 
  if (f != NULL) return f;

  for(p=g95_option.include_dirs; p; p=p->next) {
    if (strlen(p->path)+strlen(name)+1 > PATH_MAX) continue;

    strcpy(fullname, p->path);
    strcat(fullname, name);

    f = fopen(fullname, "r");
    if (f != NULL) return f;
  }

  return NULL;
}


/* g95_article()-- Given a word, return the correct article */

const char *g95_article(const char *word) {
const char *p;

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

const char *g95_basic_typename(bt type) {
const char *p;

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

const char *g95_code2string(mstring *m, int code) {

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

int g95_string2code(mstring *m, const char *string) {

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
}


/* g95_done_2()-- Per program unit destructors */

void g95_done_2(void) {

  g95_symbol_done_2();
  g95_module_done_2();
}


/* display_help()-- Display help message and exit */

#define ARRAY_SIZE(a) (sizeof (a) / sizeof ((a)[0]))

static void display_help(void) {
int i;

  g95_status("GNU Fortran 95 Compiler " VERSION
    " (C) 2000-2001 Free Software Foundation\n"
    "Compiled " __DATE__ " " __TIME__ "\n\n"
    "Usage: g95 [options] file\n"
    "Options:\n");


  if (ARRAY_SIZE (lang_options) > 0) {
    for (i = 0; i < ARRAY_SIZE (lang_options); i++) {
      printf ("  %-23.23s %s\n", lang_options[i].option, lang_options[i].description);
    }
  }

  g95_status("\nSee http://g95.sourceforge.net for more information.\n");
  exit(0);
}


/* parse_arg()-- Parse an argument on the command line. */

static int parse_arg(int argc, char *argv[]) {
char *option;
int i;

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

  if (strcmp(option, "-F") == 0) {
    g95_option.fmode = 1;
    return 1;
  }

  if (strcmp(option, "-Wline-truncation") == 0) {
    g95_option.line_truncation = 1;
    return 1;
  }

  if (strcmp(option, "-ffixed-line-length-80") == 0) {
    g95_option.fixed_line_length = 80;
    return 1;
  }

  if (strcmp(option, "-ffixed-line-length-132") == 0) {
    g95_option.fixed_line_length = 132;
    return 1;
  }

  if (strcmp(option, "-ffree-form") == 0) {
    g95_option.form = FORM_FREE;
    return 1;
  }

  if (strcmp(option, "-ffixed-form") == 0) {
    g95_option.form = FORM_FIXED;
    return 1;
  }

  if (strcmp(option, "-fdollar-ok") == 0) {
    g95_option.dollar = 1;
    return 1;
  }

  if (strncmp(option, "-fqkind=", 8) == 0) {
    i = atoi(option+8);
    if (g95_validate_kind(BT_REAL, i) < 0)
      g95_fatal_error("Argument to -fqkind isn't a valid real kind");

    g95_option.q_kind = i;
    return 1;
  }

  if (strcmp(option, "-i8") == 0) {
    g95_option.i8 = 1;
    return 1;
  }

  if (strcmp(option, "-r8") == 0) {
    g95_option.r8 = 1;
    return 1;
  }

  if (strcmp(option, "-l1") == 0) {
    g95_option.l1 = 1;
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

  if (option[0] == '-' && option[1] == 'M') {
    if (g95_option.module_dir!=NULL) {
      g95_status("g95: Only one -M option allowed\n");
      exit(3);
    }
    if (option[2] != '\0') {
      g95_option.module_dir = (char *)g95_getmem(strlen(&option[2])+2);
      strcpy(g95_option.module_dir, &option[2]);
      strcat(g95_option.module_dir, "/");
      return 1;
    } else {
      if (argv[1][0] == '-') {
	g95_status("g95: Directory required after -M\n");
	exit(3);
      }
      g95_option.module_dir = (char *)g95_getmem(strlen(argv[1])+2);
      strcpy(g95_option.module_dir, argv[1]);
      strcat(g95_option.module_dir, "/");
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
  g95_option.module_dir = NULL;
  g95_option.verbose = 0;
  g95_option.pedantic = 0;
  g95_option.line_truncation = 0;
  g95_option.fixed_line_length = 72;
  g95_option.fmode = 0;
  g95_option.form = FORM_UNKNOWN;
  g95_option.i8 = 0;
  g95_option.q_kind = g95_default_double_kind();
  g95_option.r8 = 0;
  g95_option.l1 = g95_default_logical_kind();
}


/* main()-- Compile a fortran program */

int main(int argc, char *argv[]) {
int errors, warnings, i;

  if (argc == 1) display_help();

  g95_init_1();

  init_options();

  argv++;

  while(argc > 1) {
    i = parse_arg(argc, argv);

    argc -= i;
    argv += i;
  }

  if (g95_option.source == NULL) g95_fatal_error("Need a file to compile");

  if (g95_new_file(g95_option.source, g95_option.form) == SUCCESS)
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
