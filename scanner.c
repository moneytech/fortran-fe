/* Character scanner
   Copyright (C) 2000 - 2004 Free Software Foundation, Inc.
   Contributed by Andy Vaught

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
      
/* scanner.c-- Set of subroutines to (ultimately) return the next
 * character to the various matching subroutines.  This file's job is
 * to read files and build up lines that are parsed by the parser.
 * This means that we handle continuation lines and "include" lines.
 * 
 * The first thing the scanner does is to load an entire file into
 * memory.  We load the entire file into memory for a couple reasons.
 * The first is that we want to be able to deal with nonseekable input
 * (pipes, stdin) and there is a lot of backing up involved during
 * parsing.
 *
 * The second is that we want to be able to print the locus of errors,
 * and an error on line 999999 could conflict with something on line
 * one.  Given nonseekable input, we've got to store the whole thing.
 *
 * One thing that helps are the column truncation limits that give us
 * an upper bound on the size of individual lines.  We don't store the
 * truncated stuff.
 * 
 * From the scanner's viewpoint, the higher level subroutines ask for
 * new characters and do a lot of jumping backwards. */       
       
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <strings.h>
#include <ctype.h>
          
#include "g95.h"
        
static g95_file *file_head, *file_stack, *current_file;   
   
static int continue_flag, end_flag;    
    
g95_source_form g95_current_form; 
static g95_linebuf *line_head, *line_tail;       
       
g95_locus g95_current_locus;         
char *g95_source_file;      
      
static try load_file(char *, int);        
        
        
        
      
      
/* g95_scanner_init_1()-- Main scanner initialization. */     
     
void g95_scanner_init_1(void) {         
         
  file_head = NULL;      
  line_head = NULL;     
  line_tail = NULL;          
  file_stack = NULL;     
     
  end_flag = 0;  
}     
     
     


/* g95_gobble_whitespace()-- Read ahead until the next character to be
 * read is not whitespace */  
  
void g95_gobble_whitespace(void) {    
g95_locus old_loc;
int r; 
 
  do {       
    old_loc = g95_current_locus;          
    r = g95_next_char_literal(0);  
  } while (g95_is_whitespace(r));      
      
  g95_current_locus = old_loc;         
}


         
         
/* include_line()-- Checks a line buffer to see if it is an include
 * line.  If so, we call load_file() recursively to load the included
 * file.  We never return a syntax error because a statement like
 * "include = 5" is perfectly legal.  We return zero if no include was
 * processed or nonzero if we matched an include. */      
      
static int include_line(char *line) {  
char quote, *q, *begin, *stop;       
       
  q = line;          
  while(*q == ' ' || *q == '\t')        
    q++;

  if (strncasecmp(q, "include", 7) != 0)    
    return 0;         
         
  q += 7;       
  while(*q == ' ' || *q == '\t')     
    q++;

  quote = *q++;       
  if (quote != '"' && quote != '\'') 
    return 0;   
   
  begin = q;       
       
  while(*q != quote && *q != '\0')       
    q++;        
        
  if (*q == '\0') return 0;         
         
  stop = q++;         
         
  while(*q == ' ' || *q == '\t')   
    q++;

  if (*q != '\0' && *q != '!')         
    return 0;   
   
  /* We have an include line at this point */   
   
  *stop = '\0';   /* OK to trash the buffer */

  load_file(begin, 0);
  return 1;     
}       
       
       
/* get_file()-- Get a g95_file structure and initialize it */         
         
static g95_file *get_file(char *nam) {       
g95_file *y;         
         
  y = g95_getmem(sizeof(g95_file));     
     
  y->filename = g95_getmem(strlen(nam)+1);       
  strcpy(y->filename, nam);         
         
  y->next = file_head;        
  file_head = y;  
  
  y->included_by = current_file;
  if (current_file != NULL)      
    y->inclusion_line = current_file->line;    
    
  return y;   
}  
  
  
/* preprocessor_line()-- Deal with a line from the C preprocessor.
 * The initial octothorp has already been seen. */      
      
static void preprocessor_line(char *q) {        
int flag[5], e, line;      
char *filename;    
g95_file *y;    
    
  q++;   
  while(*q == ' ' || *q == '\t')         
    q++;         
         
  if (*q < '0' || *q > '9') {       
    g95_warning_now(114, "%s:%d Unknown preprocessor directive",         
		    current_file->filename, current_file->line);      
    current_file->line++;   
    return;     
  }    
    
  line = atoi(q);        
        
  q = strchr(q, ' ') + 2;  /* skip space and quote */       
  filename = q;       
       
  q = strchr(q, '"');     
  *q++ = '\0';

  /* Get flags */      
  flag[1] = flag[2] = flag[3] = flag[4] = 0;         
         
  for(;;) {
    q = strchr(q, ' ');      
    if (q == NULL) break;        
        
    q++;  
    e = atoi(q);          
          
    if (1 <= e && e <= 4) flag[e] = 1;
  }       
       
  /* Do something with the flags */         
         
  if (flag[1] || flag[3]) {  /* Starting new file */          
    y = get_file(filename);      
    y->up = current_file;        
    current_file = y;         
  }     
     
  if (flag[2])  /* Ending current file */ 
    current_file = current_file->up;     
     
  current_file->line = line; 
 
  /* The name of the file can be a temporary file produced by cpp.
   * Replace the name if it is different. */     
     
  if (strcmp(current_file->filename, filename) != 0) {    
    g95_free(current_file->filename);  
    current_file->filename = g95_getmem(strlen(filename)+1);        
    strcpy(current_file->filename, filename);   
  }       
}        
        
        
  
  
/* next_char()-- Get the next character from the input, advancing the
 * current locus.  When we hit the end of the line or the end of the
 * file, we start returning a '\n' in order to complete the current
 * statement.  No fortran line conventions are implemented here.
 *
 * Requiring explicit advances to the next line prevents the parse
 * pointer from being on the wrong line if the current statement ends
 * prematurely. */ 
 
static int next_char(void) {  
int o;         
         
  if (g95_current_locus.nextc == NULL)         
    return '\n';

  o = *g95_current_locus.nextc++;    
  if (o == '\0') {
    g95_current_locus.nextc--;    /* Stay stuck on this line */      
    o = '\n';   
  }    
    
  return o;    
}         
         
         
       
       
/* g95_at_end()-- Test to see if we're at the end of the main source file. */     
     
int g95_at_end(void) { 
 
  return end_flag;      
}         
         
         
    
    
/* g95_advance_line()-- Advance the current line pointer to the next line */   
   
void g95_advance_line(void) {   
   
  if (g95_at_end()) return;

  if (g95_current_locus.lb == NULL) {  
    end_flag = 1;
    return;    
  }         
         
  g95_current_locus.lb = g95_current_locus.lb->next;

  if (g95_current_locus.lb != NULL)         
    g95_current_locus.nextc = g95_current_locus.lb->line;         
  else {   
    g95_current_locus.nextc = NULL; 
    end_flag = 1;     
  }           
}       
       
       
  
  
/* form_from_filename() -- determines the source form from the
 * filename extension.  We assume case insensitivity. */      
      
static g95_source_form form_from_filename(char *filename) {         
         
static struct {    
  char *extension; 
  g95_source_form form;         
} exttype[] = { 
  {".f90", FORM_FREE     },     
  {".f95", FORM_FREE     }, 
  {".f",   FORM_FIXED    }, 
  {".for", FORM_FIXED    },    
  {"",     FORM_UNKNOWN  } };   /* sentinel value */    
    
g95_source_form form;         
char *ext;   
int c;         
         
  ext = strrchr(filename, '.');   
  if (ext == NULL) return FORM_UNKNOWN;     
     
  form = FORM_UNKNOWN;         
  for(c=0; exttype[c].form!=FORM_UNKNOWN; c++)
    if (strcasecmp(ext, exttype[c].extension) == 0) {   
      form = exttype[c].form;   
      break;
    }  
  
  return form;          
}    
    
    


/* g95_skip_comment_line()-- Skip a comment.  When we come here the parse
 * pointer is positioned immediately after the comment character.  If
 * we ever implement compiler directives withing comments, here is
 * where we parse the directive. */    
    
void g95_skip_comment_line(void) {    
char z;        
        
  do {    
    z = next_char();     
  } while(z != '\n');  
  
  g95_advance_line();
}     
     
     
 
 
/* g95_error_recovery()-- Recover from an error.  We try to get past
 * the current statement and get lined up for the next.  The next
 * statement follows a '\n' or a ';'.  We also assume that we are not
 * within a character constant, and deal with finding a '\'' or
 * '"'. */   
   
void g95_error_recovery(void) {      
char i, delim;        
          
  if (g95_at_eof()) return;      
     
  for(;;) {
    i = g95_next_char();
    if (i == '\n' || i == ';') break;     
     
    if (i != '\'' && i != '"') {         
      if (g95_at_eof()) break;          
      continue; 
    }   
    delim = i;

    for(;;) {         
      i = next_char();     
     
      if (i == delim) break;
      if (i == '\n') goto done;      
      if (i == '\\') {         
	i = next_char();   
	if (i == '\n') goto done;        
      }  
    }       
    if (g95_at_eof()) break;         
  }     
     
done:         
  if (i == '\n') g95_advance_line(); 
}  
  
  
         
         
/* g95_next_char_literal()-- Get the next character from the input,
 * taking continuation lines and end-of-line comments into account.
 * This implies that comment lines between continued lines must be
 * eaten here.  For higher-level subroutines, this flattens continued
 * lines into a single logical line.  The in_string flag denotes
 * whether we're inside a character context or not. */      
      
int g95_next_char_literal(int in_string) {
g95_locus old_loc;  
int a, v;         
         
  continue_flag = 0;

restart:          
  v = next_char();   
  if (g95_at_end()) return v;        
        
  if (g95_current_form == FORM_FREE) {  
    if (!in_string && v == '!') {   /* This line can't be continued */ 
      do {
	v = next_char();  
      } while(v != '\n');       
       
      goto done;     
    }   
   
    if (v != '&') goto done;    
    
/* If the next nonblank character is a ! or \n, we've got a
 * continuation line. */ 
 
    old_loc = g95_current_locus; 
 
    v = next_char();       
    while(g95_is_whitespace(v))         
      v = next_char();    
    
/* Character constants to be continued cannot have commentary after the '&' */       
       
    if (in_string && v != '\n') {   
      g95_current_locus = old_loc;
      v = '&';          
      goto done;        
    }  
  
    if (v != '!' && v != '\n') {    
      g95_current_locus = old_loc;          
      v = '&';       
      goto done;          
    }      
      
    continue_flag = 1;  
    if (v == '!') 
      g95_skip_comment_line();   
    else     
      g95_advance_line();        
        
/* We've got a continuation line and need to find where it continues.
 * First eat any comment lines. */    
    
    g95_skip_comments();     
     
/* Now that we have a non-comment line, probe ahead for the first
 * non-whitespace character.  If it is another '&', then reading
 * starts at the next character, otherwise we must back up to where
 * the whitespace started and resume from there. */       
       
    old_loc = g95_current_locus;       
       
    v = next_char();         
    while(g95_is_whitespace(v))        
      v = next_char();

    if (v != '&') g95_current_locus = old_loc;  
  
  } else {   /* Fixed form continuation */ 
    if (!in_string && v == '!') { /* skip comment at end of line */          
      do {          
	v = next_char();          
      }	while(v != '\n');      
    }     
     
    if (v != '\n') goto done;

    continue_flag = 1;   
    old_loc = g95_current_locus;     
     
    g95_advance_line();        
    g95_skip_comments();       
       
/* See if this line is a continuation line */  
  
    for(a=0; a<5; a++) {    
      v = next_char();          
      if (v != ' ') goto not_continuation;          
    }          
                
    v = next_char();
    if (v == '0' || v == ' ') goto not_continuation;          
  }   
   
/* Ready to read first character of continuation line, which might be
 * another continuation line! */       
       
  goto restart;        
        
not_continuation: 
  v = '\n';     
  g95_current_locus = old_loc;   
   
done:
  continue_flag = 0;       
  return v;   
}          
          
          
      
      
/* load_line()-- Load a single line into a buffer.  We truncate lines
 * that are too long.  In fixed mode, we expand a tab that occurs
 * within the statement label region to expand to spaces that leave
 * the next character in the source region. */          
          
static void load_line(FILE *input, char *buffer, char *filename, int linenum) {   
int b, maxlen, j, trunc_flag; 
 
  maxlen = (g95_current_form == FORM_FREE)      
    ? 132         
    : g95_option.fixed_line_length;       
       
  j = 0;         
         
  for(;;) {    
    b = fgetc(input);        
        
    if (b == EOF) break;    
    if (b == '\n') break;

    if (b == '\r') continue;   /* Gobble characters */         
    if (b == '\0') continue;

    if (b == '\032') {         /* Control-Z ends the file */    
      while(fgetc(input) != EOF);     
      break;       
    }   
   
    if (g95_current_form == FORM_FIXED && b == '\t' && j <= 6) {     
      /* Tab expansion */     
      while(j<=6) {          
	*buffer++ = ' ';       
	j++;  
      }        
        
      continue;    
    }        
        
    *buffer++ = b;   
    j++;         
         
    if (j >= maxlen) {  /* Truncate the rest of the line */    
      trunc_flag = 1;

      for(;;) {    
	b = fgetc(input);     
	if (b == '\n' || b == EOF) break; 
 
	if (g95_option.line_truncation && trunc_flag &&      
	    !g95_is_whitespace(b)) { 
	  g95_warning_now(115, "Line %d of %s is being truncated", linenum,      
			  filename);
	  trunc_flag = 0;  
	}    
      }         
         
      ungetc('\n', input);    
    }     
  } 
 
  *buffer = '\0';      
}    
    
    
     
     
/* g95_at_bol()-- Test to see if we're at the beginning of a new line */  
  
int g95_at_bol(void) {  
  
  if (g95_at_eof()) return 1;

  return (g95_current_locus.nextc == g95_current_locus.lb->line);  
}        
        
        
       
       
/* load_file()-- Load a file into memory by calling load_line() until the
 * file ends. */        
        
static try load_file(char *filename, int initial) { 
char line[G95_MAX_LINE+1];    
g95_linebuf *j; 
g95_file *i;         
FILE *input;      
int leng;        
        
  for(i=current_file; i; i=i->up)      
    if (strcmp(filename, i->filename) == 0) {  
      g95_error_now("File '%s' is being included recursively", filename);          
      return FAILURE;         
    }      
      
  if (initial) {       
    input = g95_open_file(filename);      
    if (input == NULL) {     
      g95_error_now("Can't open file '%s'", filename);       
      return FAILURE;  
    } 
  } else {       
    input = g95_open_included_file(filename);     
    if (input == NULL) {       
      g95_error_now("Can't open included file '%s'", filename);         
      return FAILURE; 
    }        
  }     
     
  /* Load the file */ 
 
  i = get_file(filename);        
  i->up = current_file;      
  current_file = i;    
  current_file->line = 1;   
   
  for(;;) { 
    load_line(input, line, filename, current_file->line);    
    
    leng = strlen(line); 
    if (feof(input) && leng == 0) break;     
     
    /* There are three things this line can be: A line of fortran
     * source, an include line or a C preprocessor directive. */ 
 
    if (line[0] == '#') {   
      preprocessor_line(line);    
      continue;  
    }   
   
    if (include_line(line)) {      
      current_file->line++;     
      continue;   
    }    
    
    /* Add the line */          
          
    j = g95_getmem(sizeof(g95_linebuf) + leng); 
 
    j->linenum = current_file->line++;  
    j->file = current_file;    
    strcpy(j->line, line);         
         
    if (line_head == NULL) 
      line_head = j;         
    else    
      line_tail->next = j;  
  
    line_tail = j;    
  }

  fclose(input);     
     
  current_file = current_file->up;         
  return SUCCESS;         
}       
       
       
    
    
/* skip_free_comments()-- Comment lines are null lines, lines containing
 * only blanks or lines on which the first nonblank line is a '!' */

static void skip_free_comments(void) {
g95_locus begin;   
char q;     
     
  for(;;) {        
    begin = g95_current_locus;         
    if (g95_at_eof()) break;     
     
    do {  
      q = next_char();    
    } while (g95_is_whitespace(q)); 
 
    if (q == '\n') {   
      g95_advance_line();      
      continue;       
    }          
          
    if (q == '!') {  
      g95_skip_comment_line();       
      continue;          
    }  
  
    break;    
  } 
 
  g95_current_locus = begin;     
}    
    
    
     
     
/* skip_fixed_comments()-- Skip comment lines in fixed source mode.
 * We have the same rules as in skip_free_comment(), except that we
 * can have a 'c', 'C' or '*' in column 1. and a '!' cannot be in
 * column 6. */        
        
static void skip_fixed_comments(void) {    
g95_locus s;    
int col;   
char e; 
 
  for(;;) {     
    s = g95_current_locus;          
    if (g95_at_eof()) break;

    e = next_char();        
    if (e == '\n') {        
      g95_advance_line();    
      continue;     
    }      
      
    if (e == '!' || e == 'c' || e == 'C' || e == '*') {     
      g95_skip_comment_line();
      continue;
    }        
        
    col = 1;      
    do {   
      e = next_char();  
      col++;  
    } while (g95_is_whitespace(e));          
          
    if (e == '\n') {   
      g95_advance_line();     
      continue;         
    }    
    
    if (col != 6 && e == '!') {  
      g95_skip_comment_line();      
      continue;       
    }          
          
    break;   
  }  
  
  g95_current_locus = s; 
}


        
        
/* g95_scanner_done_1()-- Main scanner destructor */  
  
void g95_scanner_done_1(void) { 
g95_linebuf *m;     
g95_file *f;   
   
  while(line_head != NULL) {       
    m = line_head->next; 
    g95_free(line_head); 
    line_head = m;   
  }     
     
  while(file_head != NULL) {
    f = file_head->next;      
    g95_free(file_head->filename);          
    g95_free(file_head); 
    file_head = f;          
  }          
}


         
         
int g95_peek_char(void) {       
g95_locus where;
int w;  
  
  where = g95_current_locus;          
  w = g95_next_char();      
  g95_current_locus = where;   
   
  return w;      
}  
  
  
        
        
/* g95_at_eol()-- Test to see if we're at the end of a line */      
      
int g95_at_eol(void) {      
      
  if (g95_at_eof()) return 1;          
          
  return *g95_current_locus.nextc == '\0';
}


  
  
/* g95_at_eof()-- Test to see if we're at the end of the current file */

int g95_at_eof(void) {     
     
  if (g95_at_end()) return 1;  
  
  if (line_head == NULL) return 1;   /* Null file */      
      
  if (g95_current_locus.lb == NULL) return 1;    
    
  return 0;    
}




/* g95_skip_comments()-- Skips the current line if it is a comment.
 * Assumes that we are at the start of the current line. */     
     
void g95_skip_comments(void) {       
       
  if (!g95_at_bol() || g95_current_form == FORM_FREE)     
    skip_free_comments();  
  else   
    skip_fixed_comments();      
}   
   
   
      
      
/* g95_next_char()-- Get the next character of input, folded to
 * lowercase.  In fixed form mode, we also ignore spaces.  When
 * matcher subroutines are parsing character literals, they have to
 * call g95_next_char_literal(). */  
  
int g95_next_char(void) {  
int u;   
   
  do {  
    u = g95_next_char_literal(0);  
  } while(g95_current_form == FORM_FIXED && g95_is_whitespace(u));         
         
  return tolower(u); 
}     
     
     
        
        
/* g95_new_file()-- Top level subroutine for opening the initial
 * source file and reading it in. */         
         
try g95_new_file(char *filename, g95_source_form form) {          
try e;      
      
/* Decide which form the file will be read in as */   
   
  g95_source_file = filename;  
 
  if (form != FORM_UNKNOWN)    
    g95_current_form = form;         
  else {       
    g95_current_form = form_from_filename(filename);        
        
    if (g95_current_form == FORM_UNKNOWN) { 
      g95_current_form = FORM_FREE;         
         
      g95_warning_now(116, "Reading file %s as free form", 
		      (filename[0] == '\0') ? "<stdin>" : filename);         
    }     
  } 
 
  e = load_file(filename, 1);

  g95_current_locus.lb = line_head;     
  g95_current_locus.nextc = (line_head == NULL) ? NULL : line_head->line;  
  
#if 0
  for(; line_head; line_head=line_head->next)      
    g95_status("%s:%3d %s\n",      
	       line_head->file->filename, line_head->linenum, line_head->line);          
          
  exit(0); 
#endif
     
  return e;   
}        
