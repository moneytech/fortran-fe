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
   
static void error_char(char i) {     
     
  if (buffer_flag) {   
    if (use_warning_buffer) {
      *warning_ptr++ = i;      
      if (warning_ptr - warning_buffer.message >= MAX_ERROR_MESSAGE)        
	g95_internal_error("error_char(): Warning buffer overflow"); 
    }   
    else { 
      *error_ptr++ = i;        
      if (error_ptr - error_buffer.message >= MAX_ERROR_MESSAGE)
	g95_internal_error("error_char(): Error buffer overflow");      
    }    
  } else {       
    if (i != 0) fputc(i, stderr);
  }  
}      
      
      
     
     
/* g95_error_init_1()-- Per file error initialization */         
         
void g95_error_init_1(void) {          
          
  errors = 0; 
  warnings = 0;   
  buffer_flag = 0;       
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
          
          
 
 
/* show_locus()-- Show the file, where it was included and the source
 * line give a locus.  Calls error_printf() recursively, but the
 * recursion is at most one level deep.  */          
          
static void show_locus(int off, locus *l) {       
g95_file *t;     
char r, *g;       
int j, y;

/* TODO: Either limit the total length and number of included files displayed */
/* or add buffering of arbitrary number of characters in error messages. */

  t = l->file;   
  error_printf("In file %s:%d\n", t->filename, l->lp->start_line + l->line);       
       
  t = t->included_by;          
  while(t != NULL) {     
    error_printf("    Included at %s:%d\n", t->filename,     
		 t->loc.lp->start_line + t->loc.line);       
    t = t->included_by;        
  } 
 
/* Show the line itself, taking care not to print more than what can
 * show up on the terminal.  Tabs are converted to spaces. */         
         
  g = l->lp->line[l->line] + off;
  j = strlen(g);   
  if (j > terminal_width) j = terminal_width - 1;   
   
  for(; j>0; j--) {       
    r = *g++;         
    if (r == '\t') r = ' ';          
          
    if (isprint(r))       
      error_char(r);      
    else {   
      error_char('\\');     
      error_char('x');        
        
      y = ((r >> 4) & 0x0F) + '0';     
      if (y > '9') y += 'A' - '9' - 1;   
      error_char(y);       
       
      y = (r & 0x0F) + '0';   
      if (y > '9') y += 'A' - '9' - 1;  
      error_char(y);  
    }       
  }  
  
  error_char('\n');  
}    
    
    
 
 
/* g95_clear_warning()-- Clear the warning flag. */

void g95_clear_warning(void) {       
       
  warning_buffer.flag = 0;         
}       
       
       
         
         
/* g95_buffer_error()-- Sets the flag for buffering errors or not. */     
     
void g95_buffer_error(int flag) {   
   
  buffer_flag = flag;         
}          
          
          
      
      
/* show_loci()-- As part of printing an error, we show the source
 * lines that caused the problem.  We show at least one, possibly two
 * loci.  If we're showing two loci and they both refer to the same
 * file and line, we only print the line once. */ 
 
static void show_loci(locus *c, locus *t) {       
int o, flag, p, m, u, s, cmax;   
   
  if (c == NULL) {  
    error_printf("<During initialization>\n");      
    return; 
  }      
      
  u = c->nextc - c->lp->line[c->line]; 
  s = 0;       
  if (t == NULL) goto separate;

  s = t->nextc - t->lp->line[t->line];  
  
  if (u < s) m = s - u; else m = u - s;         
         
         
  if (c->lp != t->lp || c->line != t->line || m > terminal_width - 10)
    goto separate;

  o = 0;   
  cmax = (u < s) ? s : u;
  if (cmax > terminal_width - 5) o = cmax - terminal_width + 5;

  if (o < 0) o = 0;          
          
  u -= o;    
  s -= o; 
 
  show_locus(o, c);

/* Arrange that '1' and '2' will show up even if the two columns are equal */  
  
  for(p=1; p<=cmax; p++) {
    flag = 0;        
    if (p == u) { error_char('1'); flag = 1; } 
    if (p == s) { error_char('2'); flag = 1; }   
    if (flag == 0) error_char(' ');         
  }     
     
  error_char('\n');      
      
  return;     
     
separate:
  o = 0;  
  
  if (u > terminal_width - 5) {  
    o = u - 5; 
    if (o < 0) o = 0;       
    u = u - o;     
  }    
    
  show_locus(o, c);   
  for(p=1; p<u; p++)          
    error_char(' ');        
        
  error_char('1');     
  error_char('\n'); 
 
  if (t != NULL) {      
    o = 0;         
         
    if (s > terminal_width - 20) {         
      o = s - 20;   
      if (o < 0) o = 0; 
      s = s - o;    
    }        
        
    show_locus(o, t); 
 
    for(p=1; p<s; p++)      
      error_char(' ');     
     
    error_char('2');  
    error_char('\n');   
  } 
} 
 
 
   
   
/* error_string()-- Copy a string to wherever it needs to go. */  
  
static void error_string(char *v) {       
       
  while(*v)          
    error_char(*v++);      
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
     
     


/* g95_error_check()-- Check to see if any errors have been saved.  If
 * so, print the error.  Returns the state of error_flag. */         
         
int g95_error_check(void) {
int r;     
     
  r = error_buffer.flag; 
 
  if (error_buffer.flag) {         
    errors++;       
    fputs(error_buffer.message, stderr);  
    error_buffer.flag = 0;    
  }    
    
  return r;   
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
        
static void error_print(char *typ, char *format0, va_list argum) {
char s, *a, int_buf[IBUF_LEN], c_arg[MAX_ARGS], *cp_arg[MAX_ARGS];         
int i, l, have_l1, i_arg[MAX_ARGS];    
locus *w, *y, *loc;     
char *format;         
         
  w = y = loc = NULL;  
  
  have_l1 = 0;    
    
  l = 0;          
  format = format0;     
     
  while(*format) {       
    s = *format++;      
    if (s == '%') {          
      s = *format++;       
       
      switch(s) {      
      case '%':   
	break;          
          
      case 'L':  
	loc = va_arg(argum, locus *);        
	/* Fall through */ 
	 
      case 'C':   
	if (s == 'C') loc = g95_current_locus();    
    
	if (have_l1) {          
	  y = loc;
	} else {
	  w = loc;     
	  have_l1 = 1;    
	}   
	break;         
         
      case 'd':          
      case 'i':  
	i_arg[l++] = va_arg(argum, int);  
	break;         
         
      case 'c':          
	c_arg[l++] = va_arg(argum, int);     
	break; 
 
      case 's': 
	cp_arg[l++] = va_arg(argum, char *);
	break;         
      }    
    }     
  }    
    
/* Show the current loci if we have to */       
       
  if (have_l1) show_loci(w, y);      
  error_string(typ);
  error_char(' ');     
     
  have_l1 = 0;      
  format = format0;         
  l = 0;  
  
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
      error_char(c_arg[l++]);   
      break;     
     
    case 's':          
      error_string(cp_arg[l++]);
      break;   
   
    case 'i': case 'd':       
      i = i_arg[l++];

      if (i<0) { i = -i; error_char('-'); } 
 
      a = int_buf + IBUF_LEN - 1;     
      *a-- = '\0';

      if (i == 0) *a-- = '0';       
       
      while(i > 0) {   
	*a-- = i % 10 + '0';   
	i = i / 10;          
      }         
         
      error_string(a+1);    
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
   
   
   
   
/* g95_syntax_error()-- A general syntax error subroutine */     
     
void g95_syntax_error(g95_statement st0) {        
        
  g95_error("Syntax error in %s statement at %C", g95_ascii_statement(st0));         
}     
     
     
      
      
/* g95_fatal_error()-- Fatal errors never return */ 
 
void g95_fatal_error(char *format, ...) {  
va_list argps;    
    
  buffer_flag = 0;          
          
  va_start(argps, format);       
  error_print("Fatal Error:", format, argps);  
  va_end(argps);        
        
  exit(3);          
}     
     
     
    
    
/* g95_warning()-- Issue a warning. */   
   
void g95_warning(char *format, ...) {  
va_list argp;      
      
  warning_buffer.flag = 1;    
  warning_ptr = warning_buffer.message;     
  use_warning_buffer = 1;       
       
  va_start(argp, format);          
  if (buffer_flag == 0) warnings++;
  error_print("Warning:", format, argp);          
  va_end(argp);  
  
  error_char('\0');   
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
va_list argp;          
          
  va_start(argp, format);         
  error_print("", format, argp);          
  va_end(argp);          
}       
       
       
         
         
/* g95_clear_error()-- Clear the error flag when we start to compile a
 * source line */          
          
void g95_clear_error(void) {   
   
  error_buffer.flag = 0; 
}   
   
   
  
  
/* g95_get_errors()-- Report warnings and errors to the caller */ 
 
void g95_get_errors(int *g, int *k) {        
        
  if (g != NULL) *g = warnings;   
  if (k != NULL) *k = errors;  
}      


/* g95_status()-- Debug wrapper for printf */         
         
void g95_status(char *format, ...) {          
va_list argum;          
          
  va_start(argum, format);         
         
  if (status_out) vfprintf(status_out, format, argum);
  else vprintf(format, argum);       
       
  va_end(argum);         
}         
         
         
   
   
/* g95_error()-- Issue an error */          
          
void g95_error(char *format, ...) { 
va_list ap;  
  
  if (g95_suppress_error) return;          
          
  error_buffer.flag = 1;        
  error_ptr = error_buffer.message;    
  use_warning_buffer = 0;   
   
  va_start(ap, format);          
  if (buffer_flag == 0) errors++;    
  error_print("Error:", format, ap); 
  va_end(ap);

  error_char('\0');       
}         
         
         
      
      
/* g95_status_char()-- Subroutine for outputting a single char so that
 * we don't have to go around creating a lot of 1-character strings */        
        
void g95_status_char(char v) {   
   
  if (status_out) fputc(v, status_out);   
  else putchar(v);  
}   
   
   
 
 
/* g95_warning_now()-- Immediate warning.  */  
  
void g95_warning_now(char *format, ...) {          
va_list ap;     
int j;         
         
  j = buffer_flag;        
  buffer_flag = 0;       
  warnings++;       
       
  va_start(ap, format);   
  error_print("Warning:", format, ap);  
  va_end(ap);       
       
  error_char('\0');   
  buffer_flag = j;  
}   
   
   


/* g95_error_now()-- Immediate error.  */  
  
void g95_error_now(char *format, ...) {       
va_list argps;     
int s; 
 
  error_buffer.flag = 1;    
  error_ptr = error_buffer.message;       
       
  s = buffer_flag;        
  buffer_flag = 0;    
  errors++;  
  
  va_start(argps, format);    
  error_print("Error:", format, argps);
  va_end(argps);        
        
  error_char('\0');        
  buffer_flag = s;  
}      
      
      
