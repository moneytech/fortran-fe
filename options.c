/* Command line option related things
   Copyright (C) 2000-2002 Free Software Foundation, Inc.
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

/* options.c-- Parse and display command line options */

#include "g95.h"


#include <string.h>
#include <stdlib.h>

#ifdef IN_GCC
#include "intl.h"
#include "system.h"
#include "tree.h"
#include "flags.h"
#else
#define N_(msg) (msg)
#endif

#define DEFINE_LANG_NAME(x)
static struct {
  char *option;
  char *description;
} lang_options[] = {
#include "lang-options.h"
    { " ", " " } /* Ugly hack to avoid compile error. Will be moved
                    to driver executable anyway */
};
#undef DEFINE_LANG_NAME


g95_option_t g95_option;


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
}


/* g95_display_help()-- Display help message and exit */

void g95_display_help(void) {
int i, lo, ld, pld, no, nd, seennl, WO, WD, lo_size;
const char *co, *cd, *cdp;

  WO = 25;
  WD = 80 - WO - 3;

  g95_status("GNU Fortran 95 Compiler " G95_VERSION
    " (C) 2000-2002 Free Software Foundation\n"
    "Usage: g95 [options] file\n"
    "Options:\n");

  lo_size = (int) (sizeof(lang_options) / sizeof(lang_options[0]));

  for(i=0; i<lo_size; i++) {
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
  }

  g95_status("\nSee http://g95.sourceforge.net for more information.\n");
  exit(0);
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

/* Set the options for -Wall.  */
static void g95_set_Wall (int on) {

  g95_option.surprising = on;
  g95_option.line_truncation = on;
  g95_option.aliasing = on;

#ifdef IN_GCC
  set_Wunused (on);
  warn_return_type = on;
  warn_switch = on;

  /* We save the value of warn_uninitialized, since if they put
     -Wuninitialized on the command line, we need to generate a
     warning about not using it without also specifying -O.  */

  if (warn_uninitialized != 1) warn_uninitialized = (on ? 2 : 0);
#endif
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

  if (strcmp(option, "--help") == 0) g95_display_help();

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

  if (strcmp(option, "-Wall") == 0) g95_set_Wall(1);

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

#ifdef IN_GCC
  if (strncmp(option, "-fdump-", 7) == 0)
    return dump_switch_p (option + 2);
#endif

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
  return 0;
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

