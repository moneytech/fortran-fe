/* Handle errors
   Copyright (C) 2000, 2001, 2002 Free Software Foundation, Inc.
   Contributed by Andy Vaught & Niels Kristian Bech Jensen

This file is part of G95.

G95 is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

G95 is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with G95; see the file COPYING.  If not, write to
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
#include <stdlib.h>
#include <ctype.h>
    
#include "g95.h"
 
int g95_suppress_error=0; 
 
static int terminal_width = 80, buffer_flag, errors,   
           use_warning_buffer, warnings;   
   
static char *error_ptr, *warning_ptr;         
static g95_error_buf error_buffer, warning_buffer;
static FILE *status_out = NULL;

static void error_printf(char *, ...);     
     
  
  
/* g95_status_char()-- Subroutine for outputting a single char so that
 * we don't have to go around creating a lot of 1-character strings */     
     
void g95_status_char(char h) {   
   
  if (status_out) fputc(h, status_out); 
  else putchar(h);  
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
       
       


/* g95_push_error()-- Save the existing error state */    
    
void g95_push_error(g95_error_buf *err) {         
         
  err->flag = error_buffer.flag;         
  if (error_buffer.flag) strcpy(err->message, error_buffer.message);      
      
  error_buffer.flag = 0;          
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
          
          
      
      
/* g95_clear_warning()-- Clear the warning flag. */       
       
void g95_clear_warning(void) {     
     
  warning_buffer.flag = 0;
}   
   
   
    
    
/* g95_error_init_1()-- Per file error initialization */      
      
void g95_error_init_1(void) {         
         
  errors = 0;    
  warnings = 0;  
  buffer_flag = 0; 
}       
       
       
          
          
/* g95_status()-- Debug wrapper for printf */     
     
void g95_status(char *format, ...) {   
va_list argum;       
       
  va_start(argum, format);     
     
  if (status_out) vfprintf(status_out, format, argum);   
  else vprintf(format, argum);       
       
  va_end(argum);  
}    
    
    
    
    
/* g95_buffer_error()-- Sets the flag for buffering errors or not. */   
   
void g95_buffer_error(int flag) {         
         
  buffer_flag = flag;   
}     
     
     
          
          
/* g95_clear_error()-- Clear the error flag when we start to compile a
 * source line */         
         
void g95_clear_error(void) {      
      
  error_buffer.flag = 0; 
}  
  
  
         
         
/* g95_error_check()-- Check to see if any errors have been saved.  If
 * so, print the error.  Returns the state of error_flag. */         
         
int g95_error_check(void) {        
int retval;          
          
  retval = error_buffer.flag;   
   
  if (error_buffer.flag) {       
    errors++;   
    fputs(error_buffer.message, stderr);         
    error_buffer.flag = 0;    
  }     
     
  return retval;      
}       
       
       
   
   
/* error_char()-- Add a single character to the error buffer or output
 * depending on buffer_flag. */     
     
static void error_char(char u) {        
        
  if (buffer_flag) {        
    if (use_warning_buffer) {         
      *warning_ptr++ = u;          
      if (warning_ptr - warning_buffer.message >= MAX_ERROR_MESSAGE)         
	g95_internal_error("error_char(): Warning buffer overflow");   
    }    
    else {  
      *error_ptr++ = u; 
      if (error_ptr - error_buffer.message >= MAX_ERROR_MESSAGE)         
	g95_internal_error("error_char(): Error buffer overflow");  
    }    
  } else { 
    if (u != 0) fputc(u, stderr);     
  }          
}




/* show_locus()-- Show the file, where it was included and the source
 * line give a locus.  Calls error_printf() recursively, but the
 * recursion is at most one level deep.  */

static void show_locus(int off, g95_locus *o) {         
g95_linebuf *lb;  
g95_file *h;
char b, *a;          
int w, q;       
       
/* TODO: Either limit the total length and number of included files
 * displayed or add buffering of arbitrary number of characters in
 * error messages. */    
    
  lb = o->lb;          
  h = lb->file;    
  error_printf("In file %s:%d\n", h->filename, lb->linenum);       
       
  for(;;) {  
    q = h->inclusion_line;        
        
    h = h->included_by;      
    if (h == NULL) break;     
     
    error_printf("    Included at %s:%d\n", h->filename, q);     
  }  
  
/* Show the line itself, taking care not to print more than what can
 * show up on the terminal.  Tabs are converted to spaces. */ 
 
  a = lb->line + off;        
  w = strlen(a);        
  if (w > terminal_width) w = terminal_width - 1;

  for(; w>0; w--) {      
    b = *a++;         
    if (b == '\t') b = ' ';          
          
    if (isprint(b))     
      error_char(b);          
    else {      
      error_char('\\');       
      error_char('x');          
          
      q = ((b >> 4) & 0x0F) + '0';     
      if (q > '9') q += 'A' - '9' - 1; 
      error_char(q);    
    
      q = (b & 0x0F) + '0';     
      if (q > '9') q += 'A' - '9' - 1;   
      error_char(q);          
    }  
  }      
      
  error_char('\n');         
}    
    
    
        
        
/* check_warning()-- Given a warning number, return nonzero if it
 * should be issued. */  
  
static int check_warning(int v) {
g95_nowarn *c;  
  
  for(c=g95_option.nowarn; c; c=c->next)         
    if (c->warning == v) return 0;    
    
  return 1;  
}    
    
    
       
       
/* show_loci()-- As part of printing an error, we show the source
 * lines that caused the problem.  We show at least one, possibly two
 * loci.  If we're showing two loci and they both refer to the same
 * file and line, we only print the line once. */          
          
static void show_loci(g95_locus *g, g95_locus *d) {     
int o, flag, w, u, v, x, cmax;      
      
  if (g == NULL) {   
    error_printf("<During initialization>\n");      
    return;      
  }        
        
  v = g->nextc - g->lb->line;         
  x = 0;      
  if (d == NULL) goto separate;      
      
  x = d->nextc - d->lb->line;  
  
  if (v < x) u = x - v; else u = v - x;

  if (g->lb != d->lb || u > terminal_width - 10)
    goto separate; 
 
  o = 0; 
  cmax = (v < x) ? x : v;          
  if (cmax > terminal_width - 5) o = cmax - terminal_width + 5;      
      
  if (o < 0) o = 0;       
       
  v -= o;          
  x -= o;     
     
  show_locus(o, g);  
  
/* Arrange that '1' and '2' will show up even if the two columns are equal */    
    
  for(w=1; w<=cmax; w++) {      
    flag = 0;          
    if (w == v) { error_char('1'); flag = 1; }      
    if (w == x) { error_char('2'); flag = 1; } 
    if (flag == 0) error_char(' ');  
  }     
     
  error_char('\n');        
        
  return;     
     
separate:   
  o = 0; 
 
  if (v > terminal_width - 5) {         
    o = v - 5;         
    if (o < 0) o = 0;
    v = v - o;      
  }    
    
  show_locus(o, g);         
  for(w=1; w<v; w++)     
    error_char(' ');          
          
  error_char('1');          
  error_char('\n');   
   
  if (d != NULL) {
    o = 0;          
          
    if (x > terminal_width - 20) { 
      o = x - 20;     
      if (o < 0) o = 0; 
      x = x - o;        
    }  
  
    show_locus(o, d);          
          
    for(w=1; w<x; w++)      
      error_char(' '); 
 
    error_char('2');         
    error_char('\n');  
  }  
}         
         
         
      
      
/* g95_open_status()-- Open a file that becomes the new desination of
 * g95_status() writes. */   
   
try g95_open_status(char *filename) {  
FILE *fp1;  
  
  fp1 = fopen(filename, "w");    
    
  if (fp1 == NULL) {         
    g95_error("Unable to open file %s", filename);       
    return FAILURE;      
  } 
 
  status_out = fp1;          
  return SUCCESS;  
} 
 
 
       
       
/* g95_get_errors()-- Report warnings and errors to the caller */         
         
void g95_get_errors(int *s, int *z) {  
  
  if (s != NULL) *s = warnings;        
  if (z != NULL) *z = errors;
}     
   
   
/* error_string()-- Copy a string to wherever it needs to go. */  
  
static void error_string(char *x) {  
  
  while(*x)      
    error_char(*x++);      
}   
   
   
       
       
/* error_print()-- Workhorse for the error printing subroutines.  This
 * subroutine is inspired by g77's error handling and is similar to
 * printf() with the following %-codes:
 *
 * %c Character, %d Integer, %s String, %% Percent
 * %L  Takes g95_locus argument
 * %C  Current locus (no argument)
 *
 * If a locus pointer is given, the actual source line is printed out
 * and the column is indicated.  Since we want the error message at
 * the bottom of any source file information, we must scan the
 * argument list twice.  A maximum of two locus arguments are
 * permitted. */

#define IBUF_LEN 30
#define MAX_ARGS 10
        
static void error_print(char *typ, char *format0, va_list argum) {
char m, *f, int_buf[IBUF_LEN], c_arg[MAX_ARGS], *cp_arg[MAX_ARGS];
int k, x, have_l1, i_arg[MAX_ARGS];      
g95_locus *b, *q, *loc;       
char *format;         
         
  b = q = loc = NULL;    
    
  have_l1 = 0;       
       
  x = 0;  
  format = format0;     
     
  while(*format) {       
    m = *format++;         
    if (m == '%') {
      m = *format++;       
       
      switch(m) {  
      case '%':         
	break;   
   
      case 'L': 
	loc = va_arg(argum, g95_locus *); 
	/* Fall through */          
	          
      case 'C': 
	if (m == 'C') loc = &g95_current_locus;    
    
	if (have_l1) {       
	  q = loc;       
	} else { 
	  b = loc;   
	  have_l1 = 1;         
	}     
	break;  
  
      case 'd':   
      case 'i':     
	i_arg[x++] = va_arg(argum, int);
	break;    
    
      case 'c': 
	c_arg[x++] = va_arg(argum, int);       
	break;          
          
      case 's':  
	cp_arg[x++] = va_arg(argum, char *);   
	break;     
      }       
    }
  }     
     
/* Show the current loci if we have to */          
          
  if (have_l1) show_loci(b, q); 
  error_string(typ);   
   
  if (typ[0] != '\0')        
    error_char(' ');

  have_l1 = 0;        
  format = format0; 
  x = 0;         
         
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
      error_char(c_arg[x++]);    
      break;          
          
    case 's': 
      error_string(cp_arg[x++]);          
      break;          
          
    case 'i': case 'd':         
      k = i_arg[x++];       
       
      if (k<0) { k = -k; error_char('-'); }       
       
      f = int_buf + IBUF_LEN - 1;         
      *f-- = '\0';

      if (k == 0) *f-- = '0';

      while(k > 0) {   
	*f-- = k % 10 + '0';     
	k = k / 10;    
      } 
 
      error_string(f+1);      
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
 
 
        
        
/* g95_syntax_error()-- A general syntax error subroutine */       
       
void g95_syntax_error(g95_statement sta) {   
   
  g95_error("Syntax error in %s statement at %C", g95_ascii_statement(sta));         
}     
     
     
         
         
/* g95_error()-- Issue an error */      
      
void g95_error(char *format, ...) {          
va_list argp;   
   
  if (g95_suppress_error) return;      
      
  error_buffer.flag = 1;        
  error_ptr = error_buffer.message;   
  use_warning_buffer = 0;    
    
  va_start(argp, format); 
  if (buffer_flag == 0) errors++;     
  error_print("Error:", format, argp);  
  va_end(argp); 
 
  error_char('\0');      
}     
     
     
        
        
/* g95_warning_now()-- Immediate warning.  */  
  
void g95_warning_now(int warning, char *format, ...) {      
char buffer[80];          
va_list ap;
int g;         
         
  if (!check_warning(warning)) return; 
 
  g = buffer_flag;          
  buffer_flag = 0;     
  warnings++;         
         
  va_start(ap, format); 
 
  sprintf(buffer, "Warning (%d):", warning);      
  error_print(buffer, format, ap);      
  va_end(ap);          
          
  error_char('\0');        
  buffer_flag = g;
}         
         
         
 
 
/* g95_internal_error()-- This shouldn't happen... but sometimes does. */     
     
void g95_internal_error(char *format, ...) {
va_list argps;         
         
  buffer_flag = 0; 
 
  va_start(argps, format);         
         
  error_print("Internal error:", format, argps);          
  va_end(argps); 
 
  exit(4);          
}     
     
     
        
        
/* error_printf()-- Wrapper for error_print() */      
      
static void error_printf(char *format, ...) {         
va_list argps;  
  
  va_start(argps, format);    
  error_print("", format, argps); 
  va_end(argps);   
}      
      
      
        
        
/* g95_warning()-- Issue a warning. */  
  
void g95_warning(int warning, char *format, ...) {        
char buf[80];    
va_list argum;      
      
  if (!check_warning(warning)) return; 
 
  warning_buffer.flag = 1;          
  warning_ptr = warning_buffer.message;        
  use_warning_buffer = 1;         
         
  va_start(argum, format);  
  if (buffer_flag == 0) warnings++;      
      
  sprintf(buf, "Warning (%d):", warning);      
  error_print(buf, format, argum);      
  va_end(argum);    
    
  error_char('\0'); 
}     
     
     
 
 
/* g95_pop_error()-- Restore a previous pushed error state */          
          
void g95_pop_error(g95_error_buf *err) {     
     
  error_buffer.flag = err->flag;   
  if (error_buffer.flag) strcpy(error_buffer.message, err->message);          
}      
      
      


/* g95_error_now()-- Immediate error.  */         
         
void g95_error_now(char *format, ...) {       
va_list argps;      
int e;       
       
  error_buffer.flag = 1;  
  error_ptr = error_buffer.message;     
     
  e = buffer_flag;         
  buffer_flag = 0; 
  errors++;      
      
  va_start(argps, format); 
  error_print("Error:", format, argps);  
  va_end(argps);       
       
  error_char('\0');      
  buffer_flag = e;         
}


 
 
/* g95_fatal_error()-- Fatal errors never return */   
   
void g95_fatal_error(char *format, ...) {
va_list argp;    
    
  buffer_flag = 0;  
  
  va_start(argp, format);         
  error_print("Fatal Error:", format, argp); 
  va_end(argp);          
          
  exit(3);        
}       
       
       
