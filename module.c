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


/* g95_dump_module()-- Given module, dump it to disk */

void g95_dump_module(char *name) {
char filename[G95_MAX_SYMBOL_LEN+5];

  strcpy(filename, name);
  strcat(filename, MODULE_EXTENSION);

  if (g95_open_status(filename) == FAILURE) return;

  g95_status("G95 Module: Do not edit\n");


  g95_close_status();
}
