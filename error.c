/* Handle errors
   Copyright (C) 2000 Free Software Foundation, Inc.
   Contributed by Andy Vaught & Niels Kristian Bech Jensen

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

/* error.c-- Handle the inevitable errors.  A major catch here is that
 * things flagged as errors in one match subroutine can conceivably be
 * legal elsewhere.  This means that error messages are recorded and
 * saved for possible use later.  If a line does not match a legal
 * construction, then the saved error message is reported.  */

#include <string.h>
#include <stdarg.h>
#include <stdio.h>

#include "g95.h"

static int terminal_width = 80, buffer_flag, errors,
           use_warning_buffer, warnings;

static char *error_ptr, *warning_ptr;

static g95_error_buf error_buffer, warning_buffer;


/* g95_error_init_1()-- Per file error initialization */

void g95_error_init_1(void) {

  errors = 0;
  warnings = 0;
  buffer_flag = 0;
}


/* g95_buffer_error()-- Sets the flag for buffering errors or not. */

void g95_buffer_error(int flag) {

  buffer_flag = flag;
}


/* error_char()-- Add a single character to the error buffer or output
 * depending on buffer_flag. */

static void error_char(char c) {

  if (buffer_flag) {
    if (use_warning_buffer) {
      *warning_ptr++ = c;
      if (warning_ptr - warning_buffer.message >= MAX_ERROR_MESSAGE)
	g95_internal_error("error_char(): Warning buffer overflow");
    }
    else {
      *error_ptr++ = c;
      if (error_ptr - error_buffer.message >= MAX_ERROR_MESSAGE)
	g95_internal_error("error_char(): Error buffer overflow");
    }
  } else {
    if (c != 0) fputc(c, stderr);
  }
}


/* error_string()-- Copy a string to wherever it needs to go. */

static void error_string(char *p) {

  while(*p)
    error_char(*p++);
}


/* show_locus()-- Show the file, where it was included and the source
 * line give a locus.  Calls error_printf() recursively, but the
 * recursion is at most one level deep.  */

static void error_printf(char *, ...);

static void show_locus(int offset, locus *l) {
g95_file *f;
char c, *p;
int i;

// TODO: Either limit the total length and number of included files displayed
// or add buffering of arbitrary number of characters in error messages.

  f = l->file;
  error_printf("In file %s:%d\n", f->filename, l->lp->start_line + l->line);

  f = f->included_by;
  while(f != NULL) {
    error_printf("    Included at %s:%d\n", f->filename,
		 f->loc.lp->start_line + f->loc.line);
    f = f->included_by;
  }

/* Show the line itself, taking care not to print more than what can
 * show up on the terminal.  Tabs are converted to spaces. */

  p = l->lp->line[l->line] + offset;
  i = strlen(p);
  if (i > terminal_width) i = terminal_width - 1;

  for(; i>0; i--) {
    c = *p++;
    if (c == '\t') c = ' ';
    error_char(c);
  }

  error_char('\n');
}


/* show_loci()-- As part of printing an error, we show the source
 * lines that caused the problem.  We show at least one, possibly two
 * loci.  If we're showing two loci and they both refer to the same
 * file and line, we only print the line once. */

static void show_loci(locus *l1, locus *l2) {
int offset, flag, i, m, c1, c2;

  if (l1 == NULL) {
    error_printf("<During initialization>\n");
    return;
  }

  c1 = l1->nextc - l1->lp->line[l1->line];
  if (l2 == NULL) goto separate;

  c2 = l2->nextc - l2->lp->line[l2->line];

  if (c1 < c2) m = c2 - c1; else m = c1 - c2;

  if (l1->lp != l2->lp || l1->line != l2->line || m > terminal_width - 10)
    goto separate;

  offset = (terminal_width - m) / 2;
  if (offset < 0) offset = 0;

  c1 = c1 - offset;
  c2 = c2 - offset;

  show_locus(offset, l1);

/* Arrange that '1' and '2' will show up even if the two columns are equal */

  for(i=0; i<=m; i++) {
    flag = 0;
    if (i == c1) { error_char('1'); flag = 1; }
    if (i == c2) { error_char('2'); flag = 1; }

    if (flag == 0) error_char(' ');
  }

  return;

separate:
  offset = 0;

  if (c1 > terminal_width - 20) {
    offset = c1 - 20;
    if (offset < 0) offset = 0;
    c1 = c1 - offset;
  }

  show_locus(offset, l1);
  for(i=1; i<c1; i++)
    error_char(' ');

  error_char('1');
  error_char('\n');

  if (l2 != NULL) {
    offset = 0;

    if (c2 > terminal_width - 20) {
      offset = c2 - 20;
      if (offset < 0) offset = 0;
      c2 = c2 - offset;
    }

    show_locus(offset, l2);

    for(i=1; i<c2; i++)
      error_char(' ');

    error_char('2'); 
    error_char('\n');
  }
}


/* error_print()-- Workhorse for the error printing subroutines.  This
 * subroutine is inspired by g77's error handling and is similar to
 * printf() with the following %-codes:
 *
 * %c Character, %d Integer, %s String, %% Percent
 * %L  Takes locus argument
 * %C  Current locus (no argument)
 *
 * If a locus pointer is given, the actual source line is printed out
 * and the column is indicated.  Since we want the error message at
 * the bottom of any source file information, we must scan the
 * argument list twice.  A maximum of two locus arguments are
 * permitted. */

#define IBUF_LEN 30
#define MAX_ARGS 10

static void error_print(char *type, char *format0, va_list argp) {

char *format, c, *p, int_buf[IBUF_LEN], c_arg[MAX_ARGS], *cp_arg[MAX_ARGS];
int i, n, have_l1, have_l2, i_arg[MAX_ARGS];
locus *l1, *l2, *loc;

  l1 = NULL;
  l2 = NULL;

  have_l1 = have_l2 = 0;

  n = 0;
  format = format0;

  while(*format) {
    c = *format++;
    if (c == '%') {
      c = *format++;

      switch(c) {
      case '%':
	break;

      case 'L':
	loc = va_arg(argp, locus *);
	/* Fall through */
	
      case 'C':
	if (c == 'C') loc = g95_current_locus();

	if (have_l1) {
	  l2 = loc;
	} else {
	  l1 = loc;
	  have_l1 = 1;
	}
	break;

      case 'd':
      case 'i':
	i_arg[n++] = va_arg(argp, int);
	break;

      case 'c':
	c_arg[n++] = va_arg(argp, char);
	break;

      case 's':
	cp_arg[n++] = va_arg(argp, char *);
	break;
      }
    }
  }

/* Show the current loci if we have to */

  if (have_l1) show_loci(l1, l2);
  error_string(type);
  error_char(' ');

  have_l1 = 0;
  format = format0;
  n = 0;

  for(; *format; format++) {
    if (*format != '%') {
      error_char(*format);
      continue;
    }

    format++;
    switch(*format) {
    case '%':
      error_char('%');
      break;

    case 'c':
      error_char(c_arg[n++]);
      break;

    case 's':
      error_string(cp_arg[n++]);
      break;

    case 'i': case 'd':
      i = i_arg[n++];

      if (i<0) { i = -i; error_char('-'); }

      p = int_buf + IBUF_LEN - 1;
      *p-- = '\0';

      if (i == 0) *p-- = '0';

      while(i > 0) {
	*p-- = i % 10 + '0';
	i = i / 10;
      }

      error_string(p+1);
      break;

    case 'C':  /* Current locus */
    case 'L':  /* Specified locus */
      error_string(have_l1 ? "(2)" : "(1)");
      have_l1 = 1;
      break;
    }
  }

  error_char('\n');
}


/* error_printf()-- Wrapper for error_print() */

static void error_printf(char *format, ...) {
va_list argp;

  va_start(argp, format);
  error_print("", format, argp);
  va_end(argp);
}


/* g95_warning()-- Issue a warning. */

void g95_warning(char *format, ...) {
va_list argp;

  warning_buffer.flag = 1;
  warning_ptr = warning_buffer.message;
  use_warning_buffer = 1;

  va_start(argp, format);
  error_print("Warning:", format, argp);
  va_end(argp);

  error_char('\0');
}


/* g95_warning_now()-- Immediate warning.  */

void g95_warning_now(char *format, ...) {
va_list argp;
int i;

  i = buffer_flag;
  buffer_flag = 0;

  va_start(argp, format);
  error_print("Warning:", format, argp);
  va_end(argp);

  error_char('\0');
  buffer_flag = i;
}


/* g95_clear_warning()-- Clear the warning flag. */

void g95_clear_warning(void) {

  warning_buffer.flag = 0;
}


/* g95_warning_check()-- Check to see if any warnings have been saved.  If
 * so, print the warning. */

void g95_warning_check(void) {

  if (warning_buffer.flag) {
    warnings++;
    fputs(warning_buffer.message, stderr);
    warning_buffer.flag = 0;
  }
}


/* g95_error()-- Issue an error */

void g95_error(char *format, ...) {
va_list argp;

  error_buffer.flag = 1;
  error_ptr = error_buffer.message;
  use_warning_buffer = 0;

  va_start(argp, format);
  error_print("Error:", format, argp);
  va_end(argp);

  error_char('\0');
}


/* g95_error_now()-- Immediate error.  */

void g95_error_now(char *format, ...) {
va_list argp;
int i;

  error_buffer.flag = 1;
  error_ptr = error_buffer.message;

  i = buffer_flag;
  buffer_flag = 0;

  va_start(argp, format);
  error_print("Error:", format, argp);
  va_end(argp);

  error_char('\0');
  buffer_flag = i;
}


/* g95_fatal_error()-- Fatal errors never return */

void g95_fatal_error(char *format, ...) {
va_list argp;

  buffer_flag = 0;

  va_start(argp, format);
  error_print("Fatal Error:", format, argp);
  va_end(argp);

  exit(-1);
}


/* g95_internal_error()-- This shouldn't happen... but sometimes does. */

void g95_internal_error(char *format, ...) {
va_list argp;

  buffer_flag = 0;

  va_start(argp, format);

  show_loci(g95_current_locus(), NULL);
  error_printf("Internal Error at (1):");

  error_print("", format, argp);
  va_end(argp);

  exit(-1);
}


/* g95_clear_error()-- Clear the error flag when we start to compile a
 * source line */

void g95_clear_error(void) {

  error_buffer.flag = 0;
}


/* g95_error_check()-- Check to see if any errors have been saved.  If
 * so, print the error.  Returns the state of error_flag. */

int g95_error_check(void) {
int rc;

  rc = error_buffer.flag;

  if (error_buffer.flag) {
    errors++;
    fputs(error_buffer.message, stderr);
    error_buffer.flag = 0;
  }

  return rc;
}


/* g95_syntax_error()-- A general syntax error subroutine */

void g95_syntax_error(g95_statement st) {

  g95_error("Syntax error in %s statement at %C", g95_ascii_statement(st));
}


/* g95_push_error()-- Save the existing error state */

void g95_push_error(g95_error_buf *err) {

  err->flag = error_buffer.flag;
  if (error_buffer.flag) strcpy(err->message, error_buffer.message);

  error_buffer.flag = 0;
}


/* g95_pop_error()-- Restore a previous pushed error state */

void g95_pop_error(g95_error_buf *err) {

  error_buffer.flag = err->flag;
  if (error_buffer.flag) strcpy(error_buffer.message, err->message);
}


static FILE *status_out = NULL;

/* g95_status()-- Debug wrapper for printf */

void g95_status(char *format, ...) {
va_list argp;

  va_start(argp, format);

  if (status_out) vfprintf(status_out, format, argp);
  else vprintf(format, argp);

  va_end(argp);
}


/* g95_status_char()-- Subroutine for outputting a single char so that
 * we don't have to go around creating a lot of 1-character strings */

void g95_status_char(char c) {

  if (status_out) fputc(c, status_out);
  else putchar(c);
}


/* g95_open_status()-- Open a file that becomes the new desination of
 * g95_status() writes. */

try g95_open_status(char *filename) {
FILE *fp;

  fp = fopen(filename, "w");

  if (fp == NULL) {
    g95_error("Unable to open file %s", filename);
    return FAILURE;
  }

  status_out = fp;
  return SUCCESS;
}


/* g95_close_status()-- Closes a previously opened status file.
 * Resets the stream back to standard output. */

try g95_close_status(void) {

  if (fclose(status_out) != 0) {
    g95_error("Error closing status file");
    return FAILURE;
  }

  status_out = NULL;

  return SUCCESS;
}
