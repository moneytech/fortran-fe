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
       
         
         
/* error_char()-- Add a single character to the error buffer or output
 * depending on buffer_flag. */        
        
static void error_char(char q) {          
          
  if (buffer_flag) {         
    if (use_warning_buffer) {         
      *warning_ptr++ = q;      
      if (warning_ptr - warning_buffer.message >= MAX_ERROR_MESSAGE)  
	g95_internal_error("error_char(): Warning buffer overflow");    
    }          
    else { 
      *error_ptr++ = q; 
      if (error_ptr - error_buffer.message >= MAX_ERROR_MESSAGE)     
	g95_internal_error("error_char(): Error buffer overflow");  
    }   
  } else { 
    if (q != 0) fputc(q, stderr);         
  }        
}


        
        
/* g95_push_error()-- Save the existing error state */     
     
void g95_push_error(g95_error_buf *err) {    
    
  err->flag = error_buffer.flag;        
  if (error_buffer.flag) strcpy(err->message, error_buffer.message);   
   
  error_buffer.flag = 0;    
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
   
   


/* show_locus()-- Show the file, where it was included and the source
 * line give a locus.  Calls error_printf() recursively, but the
 * recursion is at most one level deep.  */   
   
static void show_locus(int offset, locus *e) {   
g95_file *f;          
char u, *y;     
int b, z; 
 
/* TODO: Either limit the total length and number of included files displayed */  
/* or add buffering of arbitrary number of characters in error messages. */ 
 
  f = e->file;          
  error_printf("In file %s:%d\n", f->filename, e->lp->start_line + e->line); 
 
  f = f->included_by; 
  while(f != NULL) {    
    error_printf("    Included at %s:%d\n", f->filename,      
		 f->loc.lp->start_line + f->loc.line);       
    f = f->included_by;    
  }         
         
/* Show the line itself, taking care not to print more than what can
 * show up on the terminal.  Tabs are converted to spaces. */         
         
  y = e->lp->line[e->line] + offset;     
  b = strlen(y);   
  if (b > terminal_width) b = terminal_width - 1;          
          
  for(; b>0; b--) {   
    u = *y++;         
    if (u == '\t') u = ' ';      
      
    if (isprint(u)) 
      error_char(u);      
    else {          
      error_char('\\');        
      error_char('x'); 
 
      z = ((u >> 4) & 0x0F) + '0';   
      if (z > '9') z += 'A' - '9' - 1;   
      error_char(z);         
         
      z = (u & 0x0F) + '0';  
      if (z > '9') z += 'A' - '9' - 1;          
      error_char(z);          
    }
  }         
         
  error_char('\n'); 
}   
   
   
         
         
/* g95_buffer_error()-- Sets the flag for buffering errors or not. */         
         
void g95_buffer_error(int flag) {     
     
  buffer_flag = flag;          
}  
  
  
    
    
/* show_loci()-- As part of printing an error, we show the source
 * lines that caused the problem.  We show at least one, possibly two
 * loci.  If we're showing two loci and they both refer to the same
 * file and line, we only print the line once. */ 
 
static void show_loci(locus *t, locus *x) {
int offs, flag, w, k, a, v, cmax;

  if (t == NULL) {  
    error_printf("<During initialization>\n");         
    return;      
  }      
      
  a = t->nextc - t->lp->line[t->line];        
  v = 0;       
  if (x == NULL) goto separate;       
       
  v = x->nextc - x->lp->line[x->line];     
     
  if (a < v) k = v - a; else k = a - v;   
   
   
  if (t->lp != x->lp || t->line != x->line || k > terminal_width - 10)
    goto separate;      
      
  offs = 0;   
  cmax = (a < v) ? v : a;        
  if (cmax > terminal_width - 5) offs = cmax - terminal_width + 5;   
   
  if (offs < 0) offs = 0;        
        
  a -= offs;         
  v -= offs;      
      
  show_locus(offs, t);        
        
/* Arrange that '1' and '2' will show up even if the two columns are equal */     
     
  for(w=1; w<=cmax; w++) {  
    flag = 0;  
    if (w == a) { error_char('1'); flag = 1; }  
    if (w == v) { error_char('2'); flag = 1; }        
    if (flag == 0) error_char(' '); 
  }  
  
  error_char('\n');     
     
  return;      
      
separate:       
  offs = 0;  
  
  if (a > terminal_width - 5) {    
    offs = a - 5;          
    if (offs < 0) offs = 0;       
    a = a - offs; 
  }        
        
  show_locus(offs, t);    
  for(w=1; w<a; w++)     
    error_char(' ');        
        
  error_char('1');  
  error_char('\n');         
         
  if (x != NULL) {        
    offs = 0;      
      
    if (v > terminal_width - 20) {     
      offs = v - 20;          
      if (offs < 0) offs = 0;
      v = v - offs;
    }          
          
    show_locus(offs, x);      
      
    for(w=1; w<v; w++)        
      error_char(' ');          
          
    error_char('2');      
    error_char('\n');      
  }       
}          
          
          
    
    
/* g95_pop_error()-- Restore a previous pushed error state */ 
 
void g95_pop_error(g95_error_buf *err) {      
      
  error_buffer.flag = err->flag;        
  if (error_buffer.flag) strcpy(error_buffer.message, err->message);      
}    
    
    
      
      
/* g95_error_init_1()-- Per file error initialization */     
     
void g95_error_init_1(void) {        
        
  errors = 0;
  warnings = 0; 
  buffer_flag = 0;       
}  
  
  
       
       
/* error_string()-- Copy a string to wherever it needs to go. */       
       
static void error_string(char *o) {     
     
  while(*o)    
    error_char(*o++);          
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
    
    
    
    
/* g95_clear_warning()-- Clear the warning flag. */          
          
void g95_clear_warning(void) { 
 
  warning_buffer.flag = 0;          
}       
       
       
          
          
/* g95_syntax_error()-- A general syntax error subroutine */         
         
void g95_syntax_error(g95_statement s) { 
 
  g95_error("Syntax error in %s statement at %C", g95_ascii_statement(s));     
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
      
static void error_print(char *t, char *format0, va_list argp) {    
char a, *b, int_buf[IBUF_LEN], c_arg[MAX_ARGS], *cp_arg[MAX_ARGS];          
int o, h, have_l1, i_arg[MAX_ARGS];   
locus *r, *d, *loc;     
char *format;      
      
  r = d = loc = NULL;

  have_l1 = 0;    
    
  h = 0;         
  format = format0;      
      
  while(*format) {        
    a = *format++;  
    if (a == '%') {     
      a = *format++;  
  
      switch(a) {          
      case '%':         
	break;        
        
      case 'L':  
	loc = va_arg(argp, locus *);   
	/* Fall through */
	
      case 'C':     
	if (a == 'C') loc = g95_current_locus();    
    
	if (have_l1) {       
	  d = loc;     
	} else {   
	  r = loc;
	  have_l1 = 1;          
	}         
	break;      
      
      case 'd':       
      case 'i':       
	i_arg[h++] = va_arg(argp, int);       
	break;         
         
      case 'c':     
	c_arg[h++] = va_arg(argp, int);        
	break;      
      
      case 's':       
	cp_arg[h++] = va_arg(argp, char *);
	break;      
      }     
    }    
  }         
         
/* Show the current loci if we have to */      
      
  if (have_l1) show_loci(r, d);    
  error_string(t); 
  error_char(' ');     
     
  have_l1 = 0;          
  format = format0;       
  h = 0;          
          
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
      error_char(c_arg[h++]);       
      break;       
       
    case 's':     
      error_string(cp_arg[h++]);
      break;    
    
    case 'i': case 'd':          
      o = i_arg[h++];       
       
      if (o<0) { o = -o; error_char('-'); }   
   
      b = int_buf + IBUF_LEN - 1;
      *b-- = '\0';     
     
      if (o == 0) *b-- = '0';  
  
      while(o > 0) {        
	*b-- = o % 10 + '0';         
	o = o / 10;  
      }     
     
      error_string(b+1);        
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
  
  
  
  
/* g95_status()-- Debug wrapper for printf */        
        
void g95_status(char *format, ...) { 
va_list argp;         
         
  va_start(argp, format);          
          
  if (status_out) vfprintf(status_out, format, argp);     
  else vprintf(format, argp);       
       
  va_end(argp);     
}        
        
        
      
      
/* g95_get_errors()-- Report warnings and errors to the caller */          
          
void g95_get_errors(int *j, int *y) {    
    
  if (j != NULL) *j = warnings;   
  if (y != NULL) *y = errors;
}    
         
         
/* g95_fatal_error()-- Fatal errors never return */          
          
void g95_fatal_error(char *format, ...) {     
va_list ap;    
    
  buffer_flag = 0;

  va_start(ap, format);          
  error_print("Fatal Error:", format, ap);      
  va_end(ap);

  exit(3);      
}


  
  
/* g95_error_now()-- Immediate error.  */        
        
void g95_error_now(char *format, ...) {
va_list argp; 
int d;       
       
  error_buffer.flag = 1;    
  error_ptr = error_buffer.message;   
   
  d = buffer_flag;    
  buffer_flag = 0;         
  errors++; 
 
  va_start(argp, format);     
  error_print("Error:", format, argp);
  va_end(argp);      
      
  error_char('\0');  
  buffer_flag = d;       
}          
          
          
       
       
/* g95_clear_error()-- Clear the error flag when we start to compile a
 * source line */

void g95_clear_error(void) {  
  
  error_buffer.flag = 0;  
}          
          
          
       
       
/* g95_warning()-- Issue a warning. */         
         
void g95_warning(char *format, ...) {         
va_list argps;     
     
  warning_buffer.flag = 1;         
  warning_ptr = warning_buffer.message;
  use_warning_buffer = 1;       
       
  va_start(argps, format);          
  if (buffer_flag == 0) warnings++;     
  error_print("Warning:", format, argps);
  va_end(argps);

  error_char('\0');          
}      
      
      
    
    
/* g95_internal_error()-- This shouldn't happen... but sometimes does. */    
    
void g95_internal_error(char *format, ...) {      
va_list ap;   
   
  buffer_flag = 0;    
    
  va_start(ap, format);     
     
  error_print("Internal error:", format, ap);    
  va_end(ap);         
         
  exit(4);    
}    
    
    
 
 
/* error_printf()-- Wrapper for error_print() */          
          
static void error_printf(char *format, ...) {       
va_list argp; 
 
  va_start(argp, format);      
  error_print("", format, argp);        
  va_end(argp);         
}


    
    
/* g95_status_char()-- Subroutine for outputting a single char so that
 * we don't have to go around creating a lot of 1-character strings */       
       
void g95_status_char(char v) {        
        
  if (status_out) fputc(v, status_out);         
  else putchar(v);   
}    
    
    


/* g95_error()-- Issue an error */   
   
void g95_error(char *format, ...) {       
va_list argum;         
         
  if (g95_suppress_error) return;         
         
  error_buffer.flag = 1;     
  error_ptr = error_buffer.message;    
  use_warning_buffer = 0;    
    
  va_start(argum, format);   
  if (buffer_flag == 0) errors++;  
  error_print("Error:", format, argum);          
  va_end(argum); 
 
  error_char('\0');       
}     
     
     
   
   
/* g95_warning_now()-- Immediate warning.  */       
       
void g95_warning_now(char *format, ...) {  
va_list ap;  
int z;   
   
  z = buffer_flag;    
  buffer_flag = 0;  
  warnings++;  
  
  va_start(ap, format);
  error_print("Warning:", format, ap);       
  va_end(ap);      
      
  error_char('\0');          
  buffer_flag = z;         
}    
    
    
