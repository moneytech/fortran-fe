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

#ifdef __GLIBC__
#include <mcheck.h>
#endif

#include "g95.h"

static struct {
  char *option;
  char *description;
}

#ifndef BACKEND_CODE
#define N_(msg) (msg)
#endif

#define DEFINE_LANG_NAME(x)
lang_options[] = {
#include "lang-options.h"
    { " ", " " } /* Ugly hack to avoid compile error. Will be moved
                    to driver executable anyway */
};
#undef DEFINE_LANG_NAME


g95_option_t g95_option;


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


/* add_path()-- adds path to the list pointed to by list */

static void add_path(g95_directorylist **list, char *path) {
g95_directorylist *dir;
char *p;

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


/* display_help()-- Display help message and exit */

#define ARRAY_SIZE(a) (sizeof (a) / sizeof ((a)[0]))
#define WO 25
#define WD 52

static void display_help(void) {
int i, lo, ld, pld, no, nd, seennl; 
const char *co, *cd, *cdp;

  g95_status("GNU Fortran 95 Compiler " G95_VERSION
    " (C) 2000-2002 Free Software Foundation\n"
    "Compiled " __DATE__ " " __TIME__ "\n\n"
    "Usage: g95 [options] file\n"
    "Options:\n");

  for(i=0; i<ARRAY_SIZE(lang_options); i++) {
    no = nd = 0;
    seennl = 0;

    /* Print options list formatted as 2, WO, 1, WD */
    do {
      co = cd = "";
      if (no >= 0) co = &lang_options[i].option[no];

      if (nd >= 0) {  /* Skip leading blanks */ 
	for(;;) {
	  cd = &lang_options[i].description[nd];
	  if (cd[0] != ' ') break;
	  nd++;
	}

	if (seennl) {  /* Skip the first \n if already seen */
	  nd++;
	  cd = &lang_options[i].description[nd];
	  seennl = 0;
	}
      }

      lo = strlen(co);
      ld = strlen(cd);
      /* print at most WD letters with, if possible, non broken words */

      if ((pld = ld) > WD){
	pld = WD;
	/* break on the last non white space */
	while(cd[pld] != ' ' && pld > 0)
	  pld--;

	if (pld == 0) pld = WD; /* no white space, just print it all */
      }

      /* deal with \n */

      if ((cdp = strchr(cd,'\n')) != NULL && (cdp-cd) < pld) {
	pld = cdp-cd;
	seennl = 1;
      }

      g95_status("  %-*.*s %.*s\n", WO, WO, co, pld, cd);

	/* update pointers */
      if (lo > WO)
	no += WO;
      else
	no = -1;

      if (ld > WD || seennl)
	nd += pld;
      else
	nd = -1;

    } while(no >=0 || nd >=0);
#undef WO
#undef WD
  }

  g95_status("\nSee http://g95.sourceforge.net for more information.\n");
  exit(0);
}


/* g95_parse_arg()-- Parse an argument on the command line. */

int g95_parse_arg(int argc, char *argv[]) {
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

  if (strcmp(option, "-Wsurprising") == 0) {
    g95_option.surprising = 1;
    return 1;
  }

  if (strcmp(option, "-std=F") == 0) {
    g95_option.fmode = 1;
    return 1;
  }

  if (strcmp(option, "-Wline-truncation") == 0) {
    g95_option.line_truncation = 1;
    return 1;
  }

  if (strcmp(option, "-Waliasing") == 0) {
    g95_option.aliasing = 1;
    return 1;
  }

  if (strcmp(option, "-fimplicit-none") == 0 ||
      strcmp(option, "-Wimplicit") == 0) {
    g95_option.implicit_none = 1;
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

  if (strcmp(option, "-fmodule-private") == 0) {
    g95_option.module_access_private = 1;
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

  if (strcmp(option, "-fquiet") == 0) {
    g95_option.quiet = 1;
    return 1;
  }

  if (strcmp(option, "-fpack-derived") == 0) {
    g95_option.pack_derived = 1;
    return 1;
  }

  if (strncmp(option, "-fmax-stack-var-size=", 21) == 0) {
    i = atoi(option+21);

    g95_option.max_stack_var_size = i;
    return 1;
  }

  if (strncmp(option, "-fno-repack-arrays", 18) == 0) {
    g95_option.no_repack_arrays = 1;
    return 1;
  }

  if (strncmp(option, "-finline-repack-arrays", 22) == 0) {
    g95_option.inline_repack_arrays = 1;
    return 1;
  }

  if (strncmp(option, "-fno-inline-repack-arrays", 25) == 0) {
    g95_option.inline_repack_arrays = 0;
    return 1;
  }

  if (strncmp(option, "-fcheck-array-bounds", 20) == 0) {
    g95_option.check_array_bounds = 1;
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

  if (strcmp(option, "-d8") == 0) {
    g95_option.r8 = 1;
    g95_option.i8 = 1;
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

#ifdef IN_GCC
  return(0); /* Pass option to GCC */
#else
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
#endif
}


/* init_options()-- Initialize the options structure */

void g95_init_options(void) {

  g95_option.source = NULL;
  g95_option.include_dirs = NULL;
  g95_option.module_dir = NULL;
  g95_option.verbose = 0;
  g95_option.pedantic = 0;
  g95_option.surprising = 0;
  g95_option.aliasing = 0;
  g95_option.line_truncation = 0;
  g95_option.implicit_none = 0;
  g95_option.fixed_line_length = 72;
  g95_option.module_access_private = 0;
  g95_option.fmode = 0;
  g95_option.dollar = 0;
  g95_option.form = FORM_UNKNOWN;
  g95_option.q_kind = g95_default_double_kind();
  g95_option.quiet = 0;
  g95_option.pack_derived = 0;
  g95_option.i8 = 0;
  g95_option.r8 = 0;
  g95_option.d8 = 0;
  g95_option.l1 = g95_default_logical_kind();
  g95_option.max_stack_var_size = -1;
  g95_option.no_repack_arrays = 0;
  g95_option.inline_repack_arrays = 1;
  g95_option.check_array_bounds = 0;
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

  if (argc == 1) display_help();

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

