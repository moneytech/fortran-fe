/* Character scanner
   Copyright (C) 2000, 2001, 2002 Free Software Foundation, Inc.
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
          
static g95_file *first_file, *first_duplicated_file;   
g95_file *g95_current_file;    
static int continue_flag, end_flag; 
 
 
     
     
/* form_from_filename() -- determines the source form from the
 * filename extension.  We assume case insensitivity. */       
       
static g95_source_form form_from_filename(char *filename) { 
 
static struct { char *extension; g95_source_form form; } exttype[] = {   
  {".f90", FORM_FREE },         
  {".f95", FORM_FREE },   
  {".f",   FORM_FIXED},         
  {".for", FORM_FIXED},         
  {"",     FORM_UNKNOWN} };   /* sentinel value */
       
g95_source_form  f_form;    
char *fileext; 
int q; 
 
  /* find end of file name */ 
  
  q = 0;     
  while ((q < PATH_MAX) && (filename[q] != '\0')) q++;    
    
  /* improperly terminated or too-long filename */       
       
  if (q == PATH_MAX) return FORM_UNKNOWN;            
         
  /* find last period */       
       
  while (q >= 0 && (filename[q] != '.')) q--;   
   
  /* no file extension! */     
     
  if (q < 0) return FORM_UNKNOWN;     
     
  /* get file extension and compare it to others. */         
         
  fileext = &(filename[q]); 

  q = -1;      
  f_form = FORM_UNKNOWN;          
  do {     
    q++;    
    if (strcasecmp(fileext,exttype[q].extension) == 0) {
      f_form = exttype[q].form;
      break; 
    }        
  } while (exttype[q].form != FORM_UNKNOWN);    
    
  return f_form;          
}       
       
       
       
       
/* g95_set_locus()-- Let a caller move the current read pointer (backwards) */   
   
void g95_set_locus(locus *lp) {        
        
  g95_current_file->loc = *lp; 
}      
      
      
     
     
/* load_line()-- Load a single line into the buffer.  We truncate
 * lines that are too long.  In fixed mode, we expand a tab that
 * occurs within the statement label region to expand to spaces that
 * leave the next character in the source region. */          
          
static void load_line(FILE *input, g95_source_form form, char *buffer,    
		      char *filename, int linenum) {    
int z, maxlen, r, trunc_flag;     
     
  maxlen = (form == FORM_FREE) ? 132 : g95_option.fixed_line_length;       
       
  r = 0;        
        
  for(;;) {    
    z = fgetc(input);    
    
    if (z == EOF) break;         
    if (z == '\n') break;      
      
    if (z == '\r') continue;   /* Gobble characters */        
    if (z == '\0') continue;      
      
    if (z == '\032') {         /* Control-Z ends the file */      
      while(fgetc(input) != EOF);        
      break;    
    }         
         
    if (form == FORM_FIXED && z == '\t' && r <= 6) { /* Tab expandsion */ 
      while(r<=6) {         
	*buffer++ = ' ';      
	r++;          
      }       
       
      continue;   
    } 
 
    *buffer++ = z; 
    r++;     
     
    if (r >= maxlen) {  /* Truncate the rest of the line */  
      trunc_flag = 1;         
         
      for(;;) {        
	z = fgetc(input); 
	if (z == '\n' || z == EOF) break;          
          
	if (g95_option.line_truncation && trunc_flag &&        
	    !g95_is_whitespace(z)) {         
	  g95_warning_now("Line %d of %s is being truncated", linenum,    
			  filename);        
	  trunc_flag = 0; 
	}     
      }

      ungetc('\n', input);     
    }
  }          
          
  *buffer = '\0';   
}   
   
   
   
   
/* g95_at_eol()-- Test to see if we're at the end of a line */ 
 
int g95_at_eol(void) {          
          
  if (g95_at_eof()) return 1;    
    
  return *g95_current_file->loc.nextc == '\0';       
}     
     
     
     
     
/* g95_scanner_done_1()-- Main scanner destructor */          
          
void g95_scanner_done_1(void) {      
      
linebuf *lp, *lp2;     
g95_file *fp, *fp2;      
      
  for(fp=first_file; fp; fp=fp2) {   
   
    if (fp->start != NULL) {  /* Free linebuf blocks */       
      for(fp2=fp->next; fp2; fp2 = fp2->next)  
	if (fp->start == fp2->start) fp2->start = NULL;       
       
      for(lp=fp->start; lp; lp = lp2) {   
	lp2 = lp->next;        
	g95_free(lp);      
      }         
    }

    fp2 = fp->next;  
    g95_free(fp);  
  }       
       
  for(fp=first_duplicated_file; fp; fp=fp2) { 
    fp2 = fp->next;          
    g95_free(fp);          
  }         
}    
    
    
 
 
/* g95_scanner_init_1()-- Main scanner initialization. */      
      
void g95_scanner_init_1(void) {          
          
  g95_current_file = NULL;  
  first_file = NULL;     
  first_duplicated_file = NULL;     
  end_flag = 0;       
}        
        
        
      
      
/* g95_next_char()-- Get the next character of input, folded to
 * lowercase.  In fixed form mode, we also ignore spaces.  When
 * matcher subroutines are parsing character literals, they have to
 * call g95_next_char_literal(). */     
     
int g95_next_char(void) {        
int a;    
    
  do {         
    a = g95_next_char_literal(0);      
  } while(g95_current_file->form == FORM_FIXED && g95_is_whitespace(a));        
        
  return tolower(a);         
}    
    
    
      
      
/* next_char()-- Get the next character from the input, advancing
 * g95_current_file's locus.  When we hit the end of the line or the
 * end of the file, we start returning a '\n' in order to complete the
 * current statement.  No fortran line conventions are implemented
 * here.
 *
 * Requiring explicit advances to the next line prevents the parse
 * pointer from being on the wrong line if the current statement ends
 * prematurely. */      
      
static int next_char(void) {        
locus *locp;        
int y;         
         
/* End the current include level, but not if we're in the middle of
 * processing a continuation. */ 
 
  if (g95_at_eof()) {      
    if (continue_flag != 0 || g95_at_end()) return '\n';          
          
    if (g95_current_file->included_by == NULL) end_flag = 1;       
       
    return '\n';       
  }        
        
  locp = &g95_current_file->loc;      
  if (locp->nextc == NULL) return '\n';      
      
  y = *locp->nextc++;      
  if (y == '\0') {   
    locp->nextc--;    /* Stay stuck on this line */  
    y = '\n';          
  }    
    
  return y;     
}         
         
         
 
 
/* g95_current_locus()-- Return a pointer to the current locus */      
      
locus *g95_current_locus(void) {    
    
  if (g95_current_file == NULL) return NULL;    
  return &g95_current_file->loc;         
}     
     
     
       
       
/* load_file()-- Load a file into memory by calling load_line until the
 * file ends. */          
          
static void load_file(FILE *input, g95_file *fp) {      
char *linep, line[G95_MAX_LINE+1];   
int len, linenum;   
linebuf *lp;      
      
  fp->start = lp = g95_getmem(sizeof(linebuf));     
     
  linenum = 1;        
  lp->lines = 0;    
  lp->start_line = 1;   
  lp->next = NULL;         
         
  linep = (char *) (lp + 1);     
     
/* Load the file */      
      
  for(;;) {      
    load_line(input, fp->form, line, fp->filename, linenum);        
    linenum++;      
      
    len = strlen(line);       
       
    if (feof(input) && len == 0) break;

/* See if we need another linebuf */ 
 
    if (((char *) &lp->line[lp->lines+2]) > linep - len - 1) {        
      lp->next = g95_getmem(sizeof(linebuf));        
        
      lp->next->start_line = lp->start_line + lp->lines;     
      lp = lp->next;       
      lp->lines = 0;   
   
      linep = (char *) (lp + 1);       
    }   
   
    linep = linep - len - 1;
    lp->line[lp->lines++] = linep; 
    strcpy(linep, line); 
  }        
}     
     
     
 
 
/* g95_at_eof()-- Test to see if we're at the end of the current file */       
       
int g95_at_eof(void) {    
    
  if (g95_at_end()) return 1;         
         
  if (g95_current_file->start->lines == 0) return 1;  /* Null file */

  if (g95_current_file->loc.lp == NULL) return 1;    
    
  return 0;    
}          
          
          
    
    
/* g95_next_char_literal()-- Get the next character from the input,
 * taking continuation lines and end-of-line comments into account.
 * This implies that comment lines between continued lines must be
 * eaten here.  For higher-level subroutines, this flattens continued
 * lines into a single logical line.  The in_string flag denotes
 * whether we're inside a character context or not. */         
         
int g95_next_char_literal(int in_string) {   
locus old_loc;        
int y, k;     
     
  continue_flag = 0;    
    
restart:   
  k = next_char();
  if (g95_at_end()) return k;

  if (g95_current_file->form == FORM_FREE) {  
  
    if (!in_string && k == '!') {   /* This line can't be continued */          
      do {   
	k = next_char();       
      } while(k != '\n'); 
 
      goto done;    
    }

    if (k != '&') goto done;     
     
/* If the next nonblank character is a ! or \n, we've got a
 * continuation line. */   
   
    old_loc = g95_current_file->loc;        
        
    k = next_char();       
    while(g95_is_whitespace(k))         
      k = next_char();  
  
/* Character constants to be continued cannot have commentary after the '&' */        
        
    if (in_string && k != '\n') {     
      g95_set_locus(&old_loc);  
      k = '&';     
      goto done;    
    }

    if (k != '!' && k != '\n') { 
      g95_set_locus(&old_loc);        
      k = '&';     
      goto done;   
    }    
    
    continue_flag = 1;   
    if (k == '!')          
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
   
    old_loc = *g95_current_locus();   
       
    k = next_char();       
    while(g95_is_whitespace(k))  
      k = next_char();  
  
    if (k != '&') g95_set_locus(&old_loc);      
      
  } else {   /* Fixed form continuation */ 
    if (!in_string && k == '!') { /* skip comment at end of line */        
      do {     
	k = next_char();        
      }	while(k != '\n');     
    } 
 
    if (k != '\n') goto done;          
          
    continue_flag = 1; 
    old_loc = *g95_current_locus();       
       
    g95_advance_line();       
    g95_skip_comments();      
      
/* See if this line is a continuation line */         
         
    for(y=0; y<5; y++) {     
      k = next_char(); 
      if (k != ' ') goto not_continuation;       
    }          
                
    k = next_char(); 
    if (k == '0' || k == ' ') goto not_continuation;        
  }          
          
/* Ready to read first character of continuation line, which might be
 * another continuation line! */     
     
  goto restart;  
  
not_continuation:         
  k = '\n';          
  g95_set_locus(&old_loc);       
       
done:   
  continue_flag = 0; 
  return k;       
}          
          
          
      
      
int g95_peek_char(void) {      
locus old_loc;       
int b;  
  
  old_loc = *g95_current_locus(); 
  b = g95_next_char();
  g95_set_locus(&old_loc);       
       
  return b;        
}  
  
  


/* skip_free_comments()-- Comment lines are null lines, lines containing
 * only blanks or lines on which the first nonblank line is a '!' */      
      
static void skip_free_comments(void) {    
locus start;    
char b;      
      
  for(;;) {    
    start = *g95_current_locus();        
    if (g95_at_eof()) break;  
  
    do {       
      b = next_char(); 
    } while (g95_is_whitespace(b)); 
 
    if (b == '\n') {   
      g95_advance_line();
      continue;        
    }

    if (b == '!') {     
      g95_skip_comment_line();  
      continue;       
    }    
    
    break;  
  }         
         
  g95_set_locus(&start); 
}        
        
        
    
    
/* g95_check_include()-- Checks the current line buffer to see if it
 * is an include line.  If so, we load the new file and prepare to
 * read from it.  Include lines happen at a lower level than regular
 * parsing because the string-matching subroutine is far simpler than
 * the normal one.
 *
 * We never return a syntax error because a statement like "include = 5"
 * is perfectly legal.  We return zero if no include was processed or
 * nonzero if we matched an include. */ 
 
int g95_check_include(void) { 
char w, quote, path[PATH_MAX+1];     
char *include;
locus start;         
int f;         
         
  include = "include";          
          
  start = *g95_current_locus();     
  g95_gobble_whitespace(); 
 
/* Match the 'include' */     
     
  while(*include != '\0')    
    if (*include++ != g95_next_char()) goto no_include;          
          
  g95_gobble_whitespace();   
     
  quote = next_char();   
  if (quote != '"' && quote != '\'') goto no_include;

/* Copy the filename */  
  
  for(f=0;;) {
    w = next_char();          
    if (w == '\n') goto no_include;   /* No close quote */    
    if (w == quote) break;

/* This shouldn't happen-- PATH_MAX should be way longer than the max
 * line length */    
    
    if (f >= PATH_MAX)    
      g95_internal_error("Pathname of include file is too long at %C");        
        
    path[f++] = w;        
  }     
     
  path[f] = '\0'; 
  if (f == 0) goto no_include;     /* No filename! */

/* At this point, we've got a filename to be included.  The rest of
 * the include line is ignored */

  g95_new_file(path, g95_current_file->form);  
  return 1;        
        
no_include: 
  g95_set_locus(&start);     
  return 0;       
}    
    
    
  
  
/* g95_advance_line()-- Advance the current line pointer to the next line */    
    
void g95_advance_line(void) { 
locus *locp;     
linebuf *lp;       
       
  if (g95_at_end()) return;  
  
  locp = &g95_current_file->loc;    
  lp = locp->lp;     
  if (lp == NULL) return;   
   
  if (++locp->line >= lp->lines) {      
    locp->lp = lp = lp->next;      
    if (lp == NULL) return;   /* End of this file */     
     
    locp->line = 0; 
  }      
      
  locp->nextc = lp->line[locp->line];       
}         
         
         
    
    
/* skip_fixed_comments()-- Skip comment lines in fixed source mode.
 * We have the same rules as in skip_free_comment(), except that we
 * can have a 'c', 'C' or '*' in column 1. and a '!' cannot be in
 * column 6. */          
          
static void skip_fixed_comments(void) {       
locus start;
int col;     
char p;    
    
  for(;;) {    
    start = *g95_current_locus();         
    if (g95_at_eof()) break; 
 
    p = next_char();        
    if (p == '\n') {      
      g95_advance_line();     
      continue;  
    }    
    
    if (p == '!' || p == 'c' || p == 'C' || p == '*') {  
      g95_skip_comment_line();        
      continue;     
    }       
       
    col = 1;
    do {     
      p = next_char();        
      col++;   
    } while (g95_is_whitespace(p));      
      
    if (p == '\n') {  
      g95_advance_line();       
      continue;
    }       
       
    if (col != 6 && p == '!') {   
      g95_skip_comment_line();        
      continue;
    }   
   
    break;   
  }          
          
  g95_set_locus(&start);        
}        
        
        
      
      
/* g95_new_file()-- Open a new file and start scanning from that file.
 * Every new file gets a g95_file node, even if it is a duplicate file.
 * Returns zero if everything went OK, nonzero otherwise. */          
          
try g95_new_file(char *filename, g95_source_form form) {     
g95_file *fp, *fp2;       
FILE *input;       
int len;       
       
  len = strlen(filename);       
  if (len > PATH_MAX) {         
    g95_error_now("Filename '%s' is too long- ignoring it", filename);      
    return FAILURE;      
  }        
        
  fp = g95_getmem(sizeof(g95_file));    
    
/* Make sure this file isn't being included recursively */   
   
  for(fp2=g95_current_file; fp2; fp2=fp2->included_by)       
    if (strcmp(filename, fp2->filename) == 0) { 
      g95_error_now("Recursive inclusion of file '%s' at %C- ignoring it",    
		    filename);   
      g95_free(fp); 
      return FAILURE;  
    }     
     
/* See if the file has already been included */ 
 
  for(fp2=first_file; fp2; fp2=fp2->next)
    if (strcmp(filename, fp2->filename) == 0) {  
      *fp = *fp2;     
      fp->next = first_duplicated_file;   
      first_duplicated_file = fp;        
      goto init_fp;     
    } 
 
  strcpy(fp->filename, filename);     
     
  if (g95_current_file == NULL) 
    input = g95_open_file(filename);          
  else       
    input = g95_open_included_file(filename);     
     
  if (input == NULL) {     
    if (g95_current_file == NULL) 
      g95_error_now("Can't open file '%s'", filename);          
    else   
      g95_error_now("Can't open file '%s' included at %C", filename);    
    
    g95_free(fp); 
    return FAILURE;        
  }  
  
/* Decide which form the file will be read in as */     
     
  if (form != FORM_UNKNOWN)  
    fp->form = form;        
  else {  
    fp->form = form_from_filename(filename);        
        
    if (fp->form == FORM_UNKNOWN) {  
      fp->form = FORM_FREE;         
      g95_warning_now("Reading file %s as free form", filename);  
    }          
  }  
  
  fp->next = first_file;         
  first_file = fp;   
   
  load_file(input, fp); 
  fclose(input); 
 
init_fp:
  fp->included_by = g95_current_file;        
  g95_current_file = fp;        
        
  fp->loc.line = 0;      
  fp->loc.lp = fp->start;        
  fp->loc.nextc = fp->start->line[0];     
  fp->loc.file = fp;   
   
  return SUCCESS;  
}       
 
 
/* g95_at_bol()-- Test to see if we're at the beginning of a new line */ 
 
int g95_at_bol(void) {
int b;      
      
  if (g95_at_eof()) return 1;

  b = g95_current_file->loc.line;        
        
  return g95_current_file->loc.nextc ==         
           g95_current_file->loc.lp->line[b];      
} 
 
 
  
  
/* g95_at_end()-- Test to see if we're at the end of the main source file. */ 
 
int g95_at_end(void) {   
   
  return end_flag; 
}        
        
        
 
 
/* g95_error_recovery()-- Recover from an error.  We try to get past
 * the current statement and get lined up for the next.  The next
 * statement follows a '\n' or a ';'.  We also assume that we are not
 * within a character constant, and deal with finding a '\'' or
 * '"'. */    
    
void g95_error_recovery(void) {  
char t, delim;     
       
  if (g95_at_eof()) return;   
  
  for(;;) { 
    t = g95_next_char();       
    if (t == '\n' || t == ';') break; 
 
    if (t != '\'' && t != '"') {
      if (g95_at_eof()) break;  
      continue;    
    }          
    delim = t;         
         
    for(;;) {
      t = next_char();        
        
      if (t == delim) break;       
      if (t == '\n') goto done;       
      if (t == '\\') {
	t = next_char(); 
	if (t == '\n') goto done;       
      }        
    }   
    if (g95_at_eof()) break;    
  }      
      
done:          
  if (t == '\n') g95_advance_line();  
}          
          
          
 
 
/* g95_gobble_whitespace()-- Read ahead until the next character to be
 * read is not whitespace */   
   
void g95_gobble_whitespace(void) {    
locus old_loc;  
int g;          
          
  do {
    old_loc = *g95_current_locus();  
    g = g95_next_char_literal(0);    
  } while (g95_is_whitespace(g));     
     
  g95_set_locus(&old_loc);       
}         
         
         
      
      
/* g95_skip_comments()-- Skips the current line if it is a comment.
 * Assumes that we are at the start of the current line. */      
      
void g95_skip_comments(void) { 
 
  if (!g95_at_bol() || g95_current_file->form == FORM_FREE)  
    skip_free_comments(); 
  else  
    skip_fixed_comments();
}       
       
       
 
 
/* g95_skip_comment_line()-- Skip a comment.  When we come here the parse
 * pointer is positioned immediately after the comment character.  If
 * we ever implement compiler directives withing comments, here is
 * where we parse the directive. */ 
 
void g95_skip_comment_line(void) {
char f;     
     
  do {  
    f = next_char();      
  } while(f != '\n');        
        
  g95_advance_line(); 
}


