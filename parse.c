/* Main parser
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
     
/* parse.c-- Main parser */

#include <ctype.h>
#include <string.h>
#include <setjmp.h>

#include "g95.h"

/* Current statement label.  NULL means no statement label.  Because
 * new_st can get wiped during statement matching, we have to keep it
 * separate. */  
  
g95_st_label *g95_statement_label;       
static locus label_locus;  
static jmp_buf eof; 
 
g95_state_data *g95_state_stack; 
 
/* Macros that expand to case-labels for various classes of
 * statements.  Start with executable statements that directly do
 * things. */    
    
#define case_executable case ST_ALLOCATE: case ST_BACKSPACE: case ST_CALL: \
  case ST_CLOSE: case ST_CONTINUE: case ST_DEALLOCATE: case ST_END_FILE: \
  case ST_GOTO: case ST_INQUIRE: case ST_NULLIFY: case ST_OPEN: \
  case ST_READ: case ST_RETURN: case ST_REWIND: case ST_SIMPLE_IF: \
  case ST_STOP: case ST_WRITE: case ST_ASSIGNMENT: \
  case ST_POINTER_ASSIGNMENT: case ST_EXIT: case ST_CYCLE: \
  case ST_ARITHMETIC_IF: case ST_WHERE: case ST_FORALL
 
/* Statements that mark other executable statements */ 
 
#define case_exec_markers case ST_DO: case ST_FORALL_BLOCK: case ST_IF_BLOCK: \
  case ST_WHERE_BLOCK: case ST_SELECT_CASE
  
/* Declaration statements */    
    
#define case_decl case ST_ATTR_DECL: case ST_COMMON: case ST_DATA_DECL: \
  case ST_EQUIVALENCE: case ST_NAMELIST: case ST_STATEMENT_FUNCTION: \
  case ST_TYPE: case ST_INTERFACE

/* Block end statements.  Errors associated with interchanging these
 * are detected in g95_match_end(). */  
  
#define case_end case ST_END_BLOCK_DATA: case ST_END_FUNCTION: \
                 case ST_END_PROGRAM: case ST_END_SUBROUTINE
     
typedef struct {   
enum { ORDER_START, ORDER_USE, ORDER_IMPLICIT_NONE, ORDER_IMPLICIT,   
       ORDER_SPEC, ORDER_EXEC } state;      
g95_statement last_statement;         
locus where;   
} st_state;          
          
static g95_statement parse_executable(g95_statement);     
static void parse_progunit(g95_statement);
static g95_statement parse_spec(g95_statement);        
        
        
/* match_word()-- A sort of half-matching function.  We try to match
 * the word on the input with the passed string.  If this succeeds, we
 * call the keyword-dependent matching function that will match the
 * rest of the statement.  For single keywords, the matching
 * subroutine is g95_match_eos(). */      
      
static match match_word(char *str, match (*subr)(void), locus *old_locus) {         
match m;   
   
  if (str != NULL) {          
    m = g95_match(str);     
    if (m != MATCH_YES) return m; 
  }      
      
  m = (*subr)();   
   
  if (m != MATCH_YES) {       
    g95_set_locus(old_locus);
    g95_reject_statement();        
  }       
       
  return m;    
}      
      
#define match(keyword, subr, st) \
    if (match_word(keyword, subr, &old_locus) == MATCH_YES) return st;
       
 
 
/* accept_statement()-- Do whatever is necessary to accept the last
 * statement */   
   
static g95_code *accept_statement(g95_statement st) { 
g95_code *m;       
       
  m = NULL;    
    
  switch(st) {     
  case ST_USE:       
    g95_use_module();
    break;      
      
  case ST_IMPLICIT_NONE:    
    g95_set_implicit_none();  
    break;    
    
  case ST_IMPLICIT:         
    g95_set_implicit();
    break;          
          
  case ST_FUNCTION: case ST_SUBROUTINE: case ST_MODULE:    
    g95_current_ns->proc_name = g95_new_block;         
    break;   
   
    /* If the statement is the end of a block, lay down a special code
     * that allows a branch to the end of the block from within the
     * construct. */ 
 
  case ST_ENDIF:  case ST_ENDDO:  case ST_END_SELECT:        
    if (g95_statement_label != NULL) {       
      new_st.type = EXEC_NOP;  
      m = g95_add_statement();   
    }   
   
    break;    
    
    /* The end-of-program unit statements do not get the special
     * marker and require a statement of some sort if they are a
     * branch target. */

  case ST_END_PROGRAM:  case ST_END_FUNCTION:  case ST_END_SUBROUTINE:  
    if (g95_statement_label != NULL) {       
      new_st.type = EXEC_RETURN; 
      m = g95_add_statement();    
    }         
         
    break;         
         
  case_executable:         
  case_exec_markers:  
    m = g95_add_statement();         
    break;      
      
  default:
    break;      
  }  
  
  g95_commit_symbols();        
  g95_warning_check();        
  g95_clear_new_st();     
     
  return m;     
}   
   
   
       
       
/* new_level()-- Starts a new level in the statement list. */       
       
static g95_code *new_level(g95_code *y) {  
g95_code *m;   
   
  m = y->block = g95_get_code();  
  
  g95_state_stack->head = NULL;          
  g95_state_stack->next = &m->next;     
     
  return m;      
}   
   
   
 
 
/* unexpected_statement()-- Generic complaint about an out of order
 * statement.  We also do whatever is necessary to clean up.  */   
   
static void unexpected_statement(g95_statement st) {    
    
  g95_error("Unexpected %s statement at %C", g95_ascii_statement(st)); 
 
  g95_reject_statement();     
}  
  
  
   
   
/* g95_ascii_statement()-- Translate a statement enum to a string. */    
    
char *g95_ascii_statement(g95_statement st) {   
char *h;     
     
  switch(st) {     
  case ST_ARITHMETIC_IF:  h = "arithmetic IF"; break;    
  case ST_ALLOCATE:       h = "ALLOCATE"; break;     
  case ST_ATTR_DECL:      h = "attribute declaration"; break;     
  case ST_BACKSPACE:      h = "BACKSPACE"; break;         
  case ST_BLOCK_DATA:     h = "BLOCK DATA"; break;     
  case ST_CALL:           h = "CALL"; break;   
  case ST_CASE:           h = "CASE"; break;         
  case ST_CLOSE:          h = "CLOSE"; break;          
  case ST_COMMON:         h = "COMMON"; break;     
  case ST_CONTINUE:       h = "CONTINUE"; break;       
  case ST_CONTAINS:       h = "CONTAINS"; break; 
  case ST_CYCLE:          h = "CYCLE"; break; 
  case ST_DATA_DECL:      h = "data declaration"; break;      
  case ST_DATA:           h = "DATA"; break;          
  case ST_DEALLOCATE:     h = "DEALLOCATE"; break;        
  case ST_DERIVED_DECL:   h = "Derived type declaration"; break;        
  case ST_DO:             h = "DO"; break;   
  case ST_ELSE:           h = "ELSE"; break;    
  case ST_ELSEIF:         h = "ELSE IF"; break;   
  case ST_ELSEWHERE:      h = "ELSEWHERE"; break;   
  case ST_END_BLOCK_DATA: h = "END BLOCK DATA"; break;        
  case ST_ENDDO:          h = "END DO"; break;     
  case ST_END_FILE:       h = "END FILE"; break; 
  case ST_END_FORALL:     h = "END FORALL"; break; 
  case ST_END_FUNCTION:   h = "END FUNCTION"; break;    
  case ST_ENDIF:          h = "END IF"; break;          
  case ST_END_INTERFACE:  h = "END INTERFACE"; break;  
  case ST_END_MODULE:     h = "END MODULE"; break;    
  case ST_END_PROGRAM:    h = "END PROGRAM"; break;          
  case ST_END_SELECT:     h = "END SELECT"; break;      
  case ST_END_SUBROUTINE: h = "END SUBROUTINE"; break;         
  case ST_END_WHERE:      h = "END WHERE"; break;        
  case ST_END_TYPE:       h = "END TYPE"; break;  
  case ST_ENTRY:          h = "ENTRY"; break;        
  case ST_EQUIVALENCE:    h = "EQUIVALENCE"; break;   
  case ST_EXIT:           h = "EXIT"; break;
  case ST_FORALL_BLOCK:   /* Fall through */
  case ST_FORALL:         h = "FORALL"; break;         
  case ST_FORMAT:         h = "FORMAT"; break;          
  case ST_FUNCTION:       h = "FUNCTION"; break;
  case ST_GOTO:           h = "GOTO"; break;         
  case ST_IF_BLOCK:       h = "block IF"; break;       
  case ST_IMPLICIT:       h = "IMPLICIT"; break;       
  case ST_IMPLICIT_NONE:  h = "IMPLICIT NONE"; break;       
  case ST_IMPLIED_ENDDO:  h = "implied END DO"; break;  
  case ST_INQUIRE:        h = "INQUIRE"; break;        
  case ST_INTERFACE:      h = "INTERFACE"; break; 
  case ST_PARAMETER:      h = "PARAMETER"; break; 
  case ST_PRIVATE:        h = "PRIVATE"; break;       
  case ST_PUBLIC:         h = "PUBLIC"; break;          
  case ST_MODULE:         h = "MODULE"; break;         
  case ST_MODULE_PROC:    h = "MODULE PROCEDURE"; break;
  case ST_NAMELIST:       h = "NAMELIST"; break;    
  case ST_NULLIFY:        h = "NULLIFY"; break;       
  case ST_OPEN:           h = "OPEN"; break; 
  case ST_PROGRAM:        h = "PROGRAM"; break;         
  case ST_READ:           h = "READ"; break;      
  case ST_RETURN:         h = "RETURN"; break;   
  case ST_REWIND:         h = "REWIND"; break;        
  case ST_STOP:           h = "STOP"; break;     
  case ST_SUBROUTINE:     h = "SUBROUTINE"; break; 
  case ST_TYPE:           h = "TYPE"; break; 
  case ST_USE:            h = "USE"; break;        
  case ST_WHERE_BLOCK:    /* Fall through */        
  case ST_WHERE:          h = "WHERE"; break;        
  case ST_WRITE:          h = "WRITE"; break;         
  case ST_ASSIGNMENT:           h = "assignment"; break;        
  case ST_POINTER_ASSIGNMENT:   h = "pointer assignment"; break;   
  case ST_SELECT_CASE:          h = "SELECT CASE"; break; 
  case ST_SEQUENCE:             h = "SEQUENCE"; break;  
  case ST_SIMPLE_IF:            h = "Simple IF"; break;   
  case ST_STATEMENT_FUNCTION:   h = "STATEMENT FUNCTION"; break;     
  default:         
    g95_internal_error("g95_ascii_statement(): Bad statement code");
  }      
      
  return h;   
} 
 
 


/* verify_st_order()-- Given the next statement seen by the matcher,
 * make sure that it is in proper order with the last.  This
 * subroutine is initialized by calling it with an argument of
 * ST_NONE.  If there is a problem, we issue an error and return
 * FAILURE.  Otherwise we return SUCCESS.
 *
 * Individual parsers need to verify that the statements seen are
 * valid before calling here, ie ENTRY statements are not allowed in
 * INTERFACE blocks.  The following diagram is taken from the standard:

            +---------------------------------------+
            | program  subroutine  function  module |
            +---------------------------------------+
            |                 use                   |
            |---------------------------------------+
            |        |        implicit none         |
            |        +-----------+------------------+
            |        | parameter |  implicit        |
            |        +-----------+------------------+
            | format |           |  derived type    |
            | entry  | parameter |  interface       |
            |        |   data    |  specification   |
            |        |           |  statement func  |
            |        +-----------+------------------+
            |        |   data    |    executable    |
            +--------+-----------+------------------+
            |                contains               |
            +---------------------------------------+
            |      internal module/subprogram       |
            +---------------------------------------+
            |                   end                 |
            +---------------------------------------+

*/          
          
static try verify_st_order(st_state *k, g95_statement st) {      
      
  switch(st) { 
  case ST_NONE:    
    k->state = ORDER_START; 
    break;

  case ST_USE:
    if (k->state > ORDER_USE) goto order;  
    k->state = ORDER_USE;    
    break;

  case ST_IMPLICIT_NONE:        
    if (k->state > ORDER_IMPLICIT_NONE) goto order;        
        
/* The '>' sign cannot be a '>=', because a FORMAT or ENTRY statement
 * disqualifies a USE but not an IMPLICIT NONE.  Duplicate IMPLICIT
 * NONEs are caught when the implicit types are set. */ 
 
    k->state = ORDER_IMPLICIT_NONE;        
    break;   
   
  case ST_IMPLICIT:      
    if (k->state > ORDER_IMPLICIT) goto order;
    k->state = ORDER_IMPLICIT; 
    break;   
   
  case ST_FORMAT:     
  case ST_ENTRY:       
    if (k->state < ORDER_IMPLICIT_NONE) k->state = ORDER_IMPLICIT_NONE;          
    break;   
   
  case ST_PARAMETER:          
    if (k->state >= ORDER_EXEC) goto order;  
    if (k->state < ORDER_IMPLICIT) k->state = ORDER_IMPLICIT;
    break;      
      
  case ST_DATA:       
    if (k->state < ORDER_SPEC) k->state = ORDER_SPEC;
    break;         
         
  case ST_PUBLIC:          case ST_PRIVATE:  
  case ST_DERIVED_DECL:         
  case_decl:       
    if (k->state >= ORDER_EXEC) goto order;
    if (k->state < ORDER_SPEC) k->state = ORDER_SPEC;   
    break;

  case_executable:
  case_exec_markers:        
    if (k->state < ORDER_EXEC) k->state = ORDER_EXEC; 
    break;        
        
  default:  
    g95_internal_error("Unexpected %s statement in verify_st_order() at %C", 
		       g95_ascii_statement(st));   
  }      
      
/* All is well, record the statement in case we need it next time. */       
       
  k->where = *g95_current_locus();       
  k->last_statement = st;          
  return SUCCESS;    
    
order:       
  g95_error("%s statement at %C cannot follow %s statement at %L", 
	    g95_ascii_statement(st), g95_ascii_statement(k->last_statement), 
	    &k->where);  
  
  return FAILURE;          
}       
       
       
  
  
/* pop_state()-- Pop the current state */   
   
static void pop_state(void) {    
    
  g95_state_stack = g95_state_stack->previous;    
}      
      
      
 
 
/* unexpected_eof()-- Handle an unexpected end of file.  This is a
 * show-stopper... */        
        
static void unexpected_eof(void) {     
g95_state_data *l;          
          
  g95_error("Unexpected end of file in '%s'", g95_current_file->filename);        
        
  /* Memory cleanup.  Move to "second to last" */   
   
  for(l=g95_state_stack; l && l->previous && l->previous->previous;         
      l=l->previous);        
        
  g95_current_ns->code = (l && l->previous) ? l->head : NULL;   
  g95_done_2();   
   
  longjmp(eof, 1);       
}


        
        
/* g95_state_name()-- Return the name of a compile state */   
   
char *g95_state_name(g95_compile_state state) {        
char *o;       
       
  switch(state) {
  case COMP_PROGRAM:     o = "a PROGRAM"; break;      
  case COMP_MODULE:      o = "a MODULE"; break;    
  case COMP_SUBROUTINE:  o = "a SUBROUTINE"; break;   
  case COMP_FUNCTION:    o = "a FUNCTION"; break;
  case COMP_BLOCK_DATA:  o = "a BLOCK DATA"; break;  
  case COMP_INTERFACE:   o = "an INTERFACE"; break;  
  case COMP_DERIVED:     o = "a DERIVED TYPE block"; break;      
  case COMP_IF:          o = "an IF-THEN block"; break;
  case COMP_DO:          o = "a DO block"; break;          
  case COMP_SELECT:      o = "a SELECT block"; break;       
  case COMP_FORALL:      o = "a FORALL block"; break; 
  case COMP_WHERE:       o = "a WHERE block"; break;    
  case COMP_CONTAINS:    o = "a contained subprogram"; break;        
        
  default:        
    g95_internal_error("g95_state_name(): Bad state");         
  } 
 
  return o;       
}          
          
          


/* decode_statement()-- Figure out what the next statement is,
 * (mostly) regardless of proper ordering */        
        
static g95_statement decode_statement(void) {     
g95_statement st;   
locus old_locus;      
int o;          
          
#ifdef G95_DEBUG
  g95_symbol_state();
#endif
  
  g95_clear_error();    /* Clear any stored errors */     
  g95_clear_warning();  /* Clear any stored warnings */   
   
  if (g95_match_eos() == MATCH_YES) return ST_NONE;   
   
  old_locus = *g95_current_locus();          
          
/* Try matching a data declaration or function declaration. The input
 * "REALFUNCTIONA(N)" can mean several things in different contexts,
 * so it (and its relatives) get special treatment. */        
        
  if (g95_current_state() == COMP_NONE ||      
      g95_current_state() == COMP_INTERFACE ||       
      g95_current_state() == COMP_CONTAINS) { 
    if (g95_match_function_decl() == MATCH_YES) return ST_FUNCTION; 
 
    g95_undo_symbols();   
    g95_set_locus(&old_locus);          
  }      
      
/* Match statements whose error messages are meant to be overwritten
 * by something better */          
          
  match(NULL, g95_match_assignment, ST_ASSIGNMENT);    
  match(NULL, g95_match_pointer_assignment, ST_POINTER_ASSIGNMENT);     
  match(NULL, g95_match_st_function, ST_STATEMENT_FUNCTION);   
   
  match(NULL, g95_match_data_decl, ST_DATA_DECL);        
        
/* Try to match a subroutine statement, which has the same optional
 * prefixes that functions can have. */    
    
  if (g95_match_subroutine() == MATCH_YES) return ST_SUBROUTINE;         
  g95_undo_symbols();          
  g95_set_locus(&old_locus); 
 
/* Check for the IF, DO, SELECT, WHERE and FORALL statements, which
 * might begin with a block label.  The match functions for these
 * statements are unusual in that their keyword is not seen before the
 * matcher is called */   
   
  if (g95_match_if(&st) == MATCH_YES) return st;        
  g95_undo_symbols();          
  g95_set_locus(&old_locus);   
   
  if (g95_match_where(&st) == MATCH_YES) return st;      
  g95_undo_symbols();        
  g95_set_locus(&old_locus);  
  
  if (g95_match_forall(&st) == MATCH_YES) return st;     
  g95_undo_symbols();          
  g95_set_locus(&old_locus);

  match(NULL, g95_match_do, ST_DO);    
  match(NULL, g95_match_select, ST_SELECT_CASE);          
          
/* General statement matching: Instead of testing every possible
 * statement, we eliminate most possibilities by peeking at the first
 * character. */          
          
  o = g95_peek_char();         
         
  switch(o) {     
    case 'a':        
      match("allocate", g95_match_allocate, ST_ALLOCATE);   
      match("allocatable", g95_match_allocatable, ST_ATTR_DECL);         
      match("assign", g95_match_assign, ST_NONE);    
      break;        
        
    case 'b':
      match("backspace", g95_match_backspace, ST_BACKSPACE); 
      match("block data", g95_match_block_data, ST_BLOCK_DATA);         
      break; 
 
    case 'c':
      match("call", g95_match_call, ST_CALL);         
      match("close", g95_match_close, ST_CLOSE);      
      match("continue", g95_match_continue, ST_CONTINUE);    
      match("cycle", g95_match_cycle, ST_CYCLE);  
      match("case", g95_match_case, ST_CASE); 
      match("common", g95_match_common, ST_COMMON);
      match("contains", g95_match_eos, ST_CONTAINS);     
      break;          
          
    case 'd':   
      match("deallocate", g95_match_deallocate, ST_DEALLOCATE);
      match("data", g95_match_data, ST_DATA);     
      match("dimension", g95_match_dimension, ST_ATTR_DECL);    
      break; 
 
    case 'e':
      match("end file", g95_match_endfile, ST_END_FILE);  
      match("exit", g95_match_exit, ST_EXIT); 
      match("else", g95_match_else, ST_ELSE);    
      match("else where", g95_match_elsewhere, ST_ELSEWHERE);
      match("else if", g95_match_elseif, ST_ELSEIF);

      if (g95_match_end(&st) == MATCH_YES) return st;  
  
      match("entry", g95_match_entry, ST_ENTRY);      
      match("equivalence", g95_match_equivalence, ST_EQUIVALENCE); 
      match("external", g95_match_external, ST_ATTR_DECL);      
      break;     
     
    case 'f': 
      match("format", g95_match_format, ST_FORMAT);      
      break;    
    
    case 'g': 
      match("go to", g95_match_goto, ST_GOTO);   
      break;        
        
    case 'i':
      match("inquire", g95_match_inquire, ST_INQUIRE);  
      match("implicit", g95_match_implicit, ST_IMPLICIT);       
      match("implicit% none", g95_match_implicit_none, ST_IMPLICIT_NONE);    
      match("interface", g95_match_interface, ST_INTERFACE);       
      match("intent", g95_match_intent, ST_ATTR_DECL);   
      match("intrinsic", g95_match_intrinsic, ST_ATTR_DECL);      
      break;    
    
    case 'm':   
      match("module% procedure", g95_match_modproc, ST_MODULE_PROC);    
      match("module", g95_match_module, ST_MODULE);
      break;    
    
    case 'n': 
      match("nullify", g95_match_nullify, ST_NULLIFY);    
      match("namelist", g95_match_namelist, ST_NAMELIST);      
      break; 
 
    case 'o':
      match("open", g95_match_open, ST_OPEN);          
      match("optional", g95_match_optional, ST_ATTR_DECL);     
      break;  
  
    case 'p': 
      match("print", g95_match_print, ST_WRITE);   
      match("parameter", g95_match_parameter, ST_PARAMETER);     
      match("pause", g95_match_pause, ST_NONE);         
      match("pointer", g95_match_pointer, ST_ATTR_DECL);     
      if (g95_match_private(&st) == MATCH_YES) return st;      
      match("program", g95_match_program, ST_PROGRAM);     
      if (g95_match_public(&st) == MATCH_YES) return st;       
      break;     
     
    case 'r':     
      match("read", g95_match_read, ST_READ);       
      match("return", g95_match_return, ST_RETURN);  
      match("rewind", g95_match_rewind, ST_REWIND);        
      break;    
    
    case 's':
      match("sequence", g95_match_eos, ST_SEQUENCE);      
      match("stop", g95_match_stop, ST_STOP);        
      match("save", g95_match_save, ST_ATTR_DECL); 
      break;  
  
    case 't': 
      match("target", g95_match_target, ST_ATTR_DECL);  
      match("type", g95_match_derived_decl, ST_DERIVED_DECL);          
      break;  
  
    case 'u': 
      match("use", g95_match_use, ST_USE);    
      break;    
    
    case 'w':    
      match("write", g95_match_write, ST_WRITE);        
      break;         
  }         
         
/* All else has failed, so give up.  See if any of the matchers has
 * stored an error message of some sort. */   
   
  if (g95_error_check() == 0) 
    g95_error_now("Unclassifiable statement at %C");       
       
  g95_reject_statement();          
          
  g95_error_recovery();     
     
  return ST_NONE;     
}    
    
#undef match
      
      
     
     
/* next_free()-- Get the next statement in free form source */   
   
static g95_statement next_free(void) {         
match m;     
int a;       
       
  g95_gobble_whitespace(); 
 
  a = g95_peek_char();         
         
  if (isdigit(a)) {        /* Found a statement label? */     
    m = g95_match_st_label(&g95_statement_label, 0);        
        
    if (m != MATCH_YES || ! g95_is_whitespace(g95_peek_char())) {    
      do { /* Skip the bad statement label. */    
        g95_warning_now("Ignoring bad statement label at %C"); 
        a = g95_next_char();    
      } while(isdigit(a));          
    } else {  
      label_locus = *g95_current_locus();   
   
      if (g95_statement_label->value == 0) { 
        g95_warning_now("Ignoring statement label of zero at %C");      
        g95_free_st_label(g95_statement_label); 
        g95_statement_label = NULL;     
      }        
        
      g95_gobble_whitespace();     
     
      if (g95_match_eos() == MATCH_YES) {  
        g95_warning_now("Ignoring statement label in empty statement at %C");          
        g95_free_st_label(g95_statement_label);  
        g95_statement_label = NULL;   
        return ST_NONE;
      }         
    }        
  }          
        
  return decode_statement();          
}    
    
    
      
      
/* g95_reject_statement()-- Undo anything tentative that has been built
 * for the current statement. */      
      
void g95_reject_statement(void) {  
  
  g95_undo_symbols();
  g95_undo_statement(); 
  g95_clear_warning();          
  g95_clear_new_st();    
}      
      
      
       
       
/* g95_find_state()-- Try to find the given state in the state stack. */  
  
try g95_find_state(g95_compile_state state) {         
g95_state_data *i; 
 
  for(i=g95_state_stack; i; i=i->previous)   
    if (i->state == state) break;       
       
  return (i == NULL) ? FAILURE : SUCCESS;      
}  
  
  


/* g95_check_do_variable()-- Given a symbol, make sure it is not an
 * iteration variable for a DO statement.  This subroutine is called
 * when the symbol is seen in a context that causes it to become
 * redefined.  If the symbol is an iterator, we generate an error
 * message.  */  
  
void g95_check_do_variable(g95_symbol *s) {         
g95_state_data *v;          
          
  for(v=g95_state_stack; v; v=v->previous)      
    if (v->do_variable == s) {
      g95_error_now("Variable '%s' at %C is a DO-iterator and cannot be "
		    "redefined", s->name);        
      break;          
    }       
} 
 
 


/* check_statement_label()-- If the current statement has a statement
 * label, make sure that it is allowed to have one. */    
    
static void check_statement_label(g95_statement st) {      
g95_sl_type type;     
     
  if (g95_statement_label == NULL) return;          
          
  switch(st) {         
  case ST_END_PROGRAM:    case ST_END_FUNCTION:  case ST_END_SUBROUTINE:         
  case ST_ENDDO:          case ST_ENDIF:         case ST_END_SELECT: 
  case_executable:       
  case_exec_markers: 
    type = ST_LABEL_TARGET;       
    break;       
       
  case ST_FORMAT:   
    type = ST_LABEL_FORMAT;      
    break;   
   
/* Statement labels are not restricted from appearing on a particular
 * line.  However, there are plenty of situations where the resulting
 * label can't be referenced. */    
    
  default:      
    type = ST_LABEL_BAD_TARGET;          
    break; 
  }       
       
  g95_define_st_label(g95_statement_label, type, &label_locus); 
  new_st.here = g95_statement_label;       
}        
        
        
  
  
/* g95_enclosing_unit()-- Figures out what the enclosing program unit
 * is.  This will be a function, subroutine, program, block data or
 * module. */ 
 
g95_state_data *g95_enclosing_unit(g95_compile_state *result) {     
g95_state_data *i;    
    
  for(i=g95_state_stack; i; i=i->previous) 
    if (i->state == COMP_FUNCTION || i->state == COMP_SUBROUTINE ||      
	i->state == COMP_MODULE || i->state == COMP_BLOCK_DATA ||
	i->state == COMP_PROGRAM) {      
      
      if (result != NULL) *result = i->state;          
      return i;      
    }      
      
  if (result != NULL) *result = COMP_PROGRAM;   
  return NULL;      
}   
   
   
        
        
/* next_fixed()-- Get the next statement in fixed-form source */          
          
static g95_statement next_fixed(void) {  
int label, digit_flag, v;  
locus loc;         
char o;

  if (!g95_at_bol()) return decode_statement();    
    
/* Skip past the current label field, parsing a statement label if one
 * is there.  This is a weird number parser, since the number is
 * contained within five columns and can have any kind of embedded
 * spaces.  We also check for characters that make the rest of the
 * line a comment */     
     
  label = 0;
  digit_flag = 0;         
         
  for(v=0; v<5; v++) {    
    o = g95_next_char_literal(0);          
          
    switch(o) {
    case ' ':       
      break;   
   
    case '0': case '1': case '2': case '3': case '4':      
    case '5': case '6': case '7': case '8': case '9':    
      label = label*10 + o - '0';         
      label_locus = *g95_current_locus();     
      digit_flag = 1;   
      break;

/* Comments have already been skipped by the time we get here so don't
 * bother checking for them. */     
     
    default:   
      g95_buffer_error(0); 
      g95_error("Non-numeric character in statement label at %C");         
      return ST_NONE;        
    }      
  }

  if (digit_flag) {       
    if (label == 0)      
      g95_warning_now("Zero is not a valid statement label at %C");        
    else { /* We've found a valid statement label.  */   
      g95_statement_label = g95_get_st_label(label);    
    }        
  }  
  
/* Since this line starts a statement, it cannot be a continuation of
 * a previous statement.  Hence we mostly ignore column 6. */   
   
  if (g95_next_char_literal(0) == '\n') goto blank_line;          
          
/* Now that we've taken care of the statement label columns, we have
 * to make sure that the first nonblank character is not a '!'.  If it
 * is, the rest of the line is a comment. */   
   
  do {         
    loc = *g95_current_locus();      
    o = g95_next_char_literal(0);       
  } while(g95_is_whitespace(o));  
  
  if (o == '!') goto blank_line;        
  g95_set_locus(&loc);          
          
  if (g95_match_eos() == MATCH_YES) goto blank_line; 
 
/* At this point, we've got a nonblank statement to parse. */       
       
  return decode_statement();      
      
blank_line:         
  if (digit_flag) g95_warning("Statement label in blank line will be "       
			      "ignored at %C");    
  g95_advance_line();     
  return ST_NONE;      
}


      
      
/* push_state()-- Push a new state onto the stack */         
         
static void push_state(g95_state_data *j, g95_compile_state new_state,        
		       g95_symbol *s) {   
   
  j->state = new_state;   
  j->previous = g95_state_stack;       
  j->sym = s;  
  j->head = NULL;     
  j->next = NULL;          
          
  g95_state_stack = j;     
}        
        
        
       
       
/* parse_if_block()-- parse the statements of an IF-THEN-ELSEIF-ELSE-ENDIF
 * block. */          
          
static void parse_if_block(void) {      
g95_code *top, *m;  
g95_statement st;         
locus else_locus;    
g95_state_data n;  
int seen_else;      
      
  seen_else = 0;   
  top = accept_statement(ST_IF_BLOCK);      
      
  push_state(&n, COMP_IF, g95_new_block);   
  g95_state_stack->next = &top->block;     
     
  do {      
    st = parse_executable(ST_NONE);        
        
    switch(st) {        
    case ST_NONE:       
      unexpected_eof();  
  
    case ST_ELSEIF:         
      if (seen_else) {
	g95_error("ELSE IF statement at %C cannot follow ELSE statement at %L",         
		  &else_locus);       
       
	g95_reject_statement();     
	break;         
      }

      m = g95_get_code();    
      m->type = EXEC_IF;
      m->expr = new_st.expr;      
      
      top->ext.block = m;          
      top = m;  
  
      g95_state_stack->next = &top->block;         
      accept_statement(st);        
      break;     
     
    case ST_ELSE:          
      if (seen_else) {
	g95_error("Duplicate ELSE statements at %L and %C", &else_locus);          
	g95_reject_statement();  
	break; 
      }      
      
      seen_else = 1;
      else_locus = *g95_current_locus();     
     
      g95_state_stack->next = &top->ext.block;          
          
      accept_statement(st);          
      break;     
     
    case ST_ENDIF:        
      break;      
      
    default: 
      unexpected_statement(st); 
      break;       
    }      
  } while(st != ST_ENDIF);   
   
  pop_state();    
  accept_statement(st);      
}


    
    
/* next_statement()-- Return the next non-ST_NONE statement to the
 * caller.  We also worry about including files and the ends of
 * include files at this stage */        
        
static g95_statement next_statement(void) { 
g95_statement st;      
      
  g95_new_block = NULL;       
       
  for(;;) {      
    g95_statement_label = NULL;        
    g95_buffer_error(1);         
         
    if (g95_at_eol()) g95_advance_line();     
     
    g95_skip_comments();   
   
    if (g95_at_bol() && g95_check_include()) continue; 
 
    if (g95_at_eof() && g95_current_file->included_by != NULL) {      
      g95_current_file = g95_current_file->included_by;         
      g95_advance_line();          
      continue;      
    }        
        
    if (g95_at_end()) {      
      st = ST_NONE;   
      break;          
    }         
         
    st = (g95_current_file->form == FORM_FIXED) ? next_fixed() : next_free();     
    if (st != ST_NONE) break;     
  }  
  
  g95_buffer_error(0);

  if (st != ST_NONE) check_statement_label(st);

  return st;     
}       
       
       


/* parse_block_data()-- Parse a block data program unit */        
        
static void parse_block_data(void) {    
g95_statement st;

  st = parse_spec(ST_NONE);         
         
  while(st != ST_END_BLOCK_DATA) {        
    g95_error("Unexpected %s statement in BLOCK DATA at %C",          
	      g95_ascii_statement(st));   
    g95_reject_statement();      
    st = next_statement();
  }       
}   
   
   


/* check_do_closure()-- Checks to see if the current statement label
 * closes an enddo.  Returns 0 if not, 1 if closes an ENDDO correctly,
 * or 2 (and issues an error) if it incorrectly closes an ENDDO. */      
      
static int check_do_closure(void) {  
g95_state_data *t;     
     
  if (g95_statement_label == NULL) return 0;     
     
  for(t=g95_state_stack; t; t=t->previous)     
    if (t->state == COMP_DO) break;     
     
  if (t == NULL) return 0;  /* No loops to close */       
       
  if (t->ext.end_do_label == g95_statement_label) {        
        
    if (t == g95_state_stack) return 1;  
  
    g95_error("End of nonblock DO statement at %C is within another block");       
    return 2;       
  }      
      
/* At this point, the label doesn't terminate the innermost loop.
 * Make sure it doesn't terminate another one. */         
         
  for(; t; t=t->previous)    
    if (t->state == COMP_DO    
        && t->ext.end_do_label == g95_statement_label) {          
      g95_error("End of nonblock DO statement at %C is interwoven "  
		"with another DO loop");      
      return 2;
    }          
          
  return 0;         
}         
         
         
          
          
/* parse_interface()-- Parse an interface.  We must be able to deal
 * with the possibility of recursive interfaces.  The parse_spec()
 * subroutine is mutually recursive with parse_interface(). */          
          
static void parse_interface(void) {
g95_compile_state new_state, current_state;       
g95_symbol *prog_unit, *symbol;         
g95_interface_info save;     
g95_state_data j, i;          
g95_statement st;       
int seen_body;  
  
  accept_statement(ST_INTERFACE);         
         
  current_interface.ns = g95_current_ns;   
  save = current_interface;    
    
  symbol = (current_interface.type == INTERFACE_GENERIC ||         
	 current_interface.type == INTERFACE_USER_OP) ? g95_new_block : NULL;   
   
  push_state(&j, COMP_INTERFACE, symbol);  
  seen_body = 0;
  current_state = COMP_NONE;       
       
loop:          
  g95_current_ns = g95_get_namespace(current_interface.ns);          
          
  st = next_statement();     
  switch(st) {    
  case ST_NONE: 
    unexpected_eof();          
          
  case ST_SUBROUTINE:     
    new_state = COMP_SUBROUTINE;         
    g95_add_explicit_interface(g95_new_block, IFSRC_IFBODY,   
			       g95_new_block->formal, NULL);     
    break;      
      
  case ST_FUNCTION:        
    new_state = COMP_FUNCTION;    
    g95_add_explicit_interface(g95_new_block, IFSRC_IFBODY,
			       g95_new_block->formal, NULL);  
    break;          
          
  case ST_MODULE_PROC:  /* The module procedure matcher makes sure the
			 * context is correct */          
    seen_body = 1;         
    accept_statement(st);       
    g95_free_namespace(g95_current_ns); 
    goto loop;  
  
  case ST_END_INTERFACE:    
    g95_free_namespace(g95_current_ns);   
    g95_current_ns = current_interface.ns; 
    goto done;      
      
  default:  
    g95_error("Unexpected %s statement in INTERFACE block at %C",          
	      g95_ascii_statement(st));     
    g95_reject_statement();   
    g95_free_namespace(g95_current_ns);
    goto loop;   
  }          
          
          
/* Make sure that a generic interface has only subroutines or
 * functions and that the generic name has the right attribute */   
   
  if (current_interface.type == INTERFACE_GENERIC) {          
    if (current_state == COMP_NONE) {        
      if (new_state == COMP_FUNCTION) g95_add_function(&symbol->attr, NULL);       
      if (new_state == COMP_SUBROUTINE) g95_add_subroutine(&symbol->attr, NULL);  
  
      current_state = new_state;  
    } else {          
      if (new_state != current_state) { 
	if (new_state == COMP_SUBROUTINE)  
	  g95_error("SUBROUTINE at %C does not belong in a generic function "        
		    "interface");       
       
	if (new_state == COMP_FUNCTION)
	  g95_error("FUNCTION at %C does not belong in a generic subroutine "       
		    "interface");          
      }
    }       
  }      
      
  push_state(&i, new_state, g95_new_block);          
  accept_statement(st);    
  prog_unit = g95_new_block;       
  prog_unit->formal_ns = g95_current_ns;        
        
/* Read data declaration statements */     
     
 decl:         
  st = parse_spec(ST_NONE);         
         
  if (st != ST_END_SUBROUTINE && st != ST_END_FUNCTION) {        
    g95_error("Unexpected %s statement at %C in INTERFACE body",
	      g95_ascii_statement(st));  
    g95_reject_statement();          
    goto decl;      
  }    
    
  seen_body = 1;  
  
  current_interface = save;
  g95_add_interface(prog_unit);      
      
  pop_state();     
  goto loop;

done:          
  if (!seen_body) g95_error("INTERFACE block at %C is empty");  
  
  pop_state();          
}         
         
         
     
     
/* parse_select_block()-- Parse a SELECT block */     
     
static void parse_select_block(void) {   
g95_statement st;      
g95_state_data s;   
g95_code *u;         
         
  u = accept_statement(ST_SELECT_CASE);     
  push_state(&s, COMP_SELECT, g95_new_block);       
       
/* Make sure that the next statement is a CASE or END SELECT */ 
 
  for(;;) {       
    st = next_statement();       
    if (st == ST_NONE) unexpected_eof();          
    if (st == ST_END_SELECT) { /* empty SELECT CASE is OK */
      accept_statement(st);       
      pop_state(); 
      return;      
    }    
    if (st == ST_CASE) break;

    g95_error("Expected a CASE or END SELECT statement following SELECT CASE "         
	      "at %C");      
      
    g95_reject_statement();   
  }  
  
/* At this point, we're got a nonempty select block */   
   
  u = new_level(u);      
  *u = new_st;   
   
  accept_statement(st);     
     
  do {  
    st = parse_executable(ST_NONE); 
    switch(st) {        
    case ST_NONE:   
      unexpected_eof();          
          
    case ST_CASE:          
      u = new_level(u);       
      *u = new_st;     
      g95_clear_new_st();        
      accept_statement(st);      
      break;    
    
    case ST_END_SELECT:      
      break;         
         
/* Can't have an executable statement because of parse_executable() */      
      
    default:  
      unexpected_statement(st);       
      break;    
    }         
  } while(st != ST_END_SELECT);     
     
  pop_state();
  accept_statement(st);         
}        
        
        
         
         
/* parse_derived()-- Parse a derived type */     
     
static void parse_derived(void) {  
int compiling_type, seen_private, seen_sequence, seen_component, error_flag;    
g95_statement st;       
g95_component *y;   
g95_state_data h;         
         
  error_flag = 0;          
          
  accept_statement(ST_DERIVED_DECL); 
  push_state(&h, COMP_DERIVED, g95_new_block);     
     
  g95_new_block->component_access = ACCESS_PUBLIC;         
  seen_private = 0;        
  seen_sequence = 0;          
  seen_component = 0;

  compiling_type = 1;
  g95_new_block->attr.set = 1;   /* Type has been defined */  
  
  while(compiling_type) {
    st = next_statement();      
    switch(st) {      
    case ST_NONE:          
      unexpected_eof();        
        
    case ST_DATA_DECL:     
      accept_statement(st);     
      seen_component = 1;          
      break;          
          
    case ST_END_TYPE:   
      compiling_type = 0;       
       
      if (!seen_component) {      
	g95_error("Derived type definition at %C has no components");   
	error_flag = 1;    
      }  
  
      accept_statement(ST_END_TYPE);        
      break;   
   
    case ST_PRIVATE:    
      if (g95_find_state(COMP_MODULE) == FAILURE) {  
	g95_error("PRIVATE statement in TYPE at %C must be inside a MODULE");         
	error_flag = 1;     
	break;   
      }       
       
      if (seen_component) {        
	g95_error("PRIVATE statement at %C must precede "  
		   "structure components");   
	error_flag = 1;         
	break;     
      }          
          
      if (seen_private) {          
	g95_error("Duplicate PRIVATE statement at %C");      
	error_flag = 1;          
      }

      h.sym->component_access = ACCESS_PRIVATE;      
      accept_statement(ST_PRIVATE);         
      seen_private = 1;     
      break;    
    
    case ST_SEQUENCE:      
      if (seen_component) {        
	g95_error("SEQUENCE statement at %C must precede "   
		   "structure components");
	error_flag = 1;        
	break;        
      } 
 
      if (g95_current_block()->attr.sequence)        
	g95_warning("SEQUENCE attribute at %C already specified in "     
		    "TYPE statement");      
      
      if (seen_sequence) {        
	g95_error("Duplicate SEQUENCE statement at %C"); 
	error_flag = 1; 
      }       
       
      seen_sequence = 1; 
      g95_add_sequence(&g95_current_block()->attr, NULL);        
      break; 
 
    default: 
      unexpected_statement(st);       
      break;     
    }       
  }  
  
/* Sanity checks on the structure.  If the structure has the SEQUENCE
 * attribute, then all component structures must also have SEQUENCE. */        
        
  if (error_flag == 0 && g95_current_block()->attr.sequence)     
    for(y=g95_current_block()->components; y; y=y->next) {    
      if (y->ts.type == BT_DERIVED && 
	  y->ts.derived->attr.sequence == 0) {
	g95_error("Component %s of SEQUENCE type declared at %C does not "   
		  "have the SEQUENCE attribute", y->ts.derived->name);     
      }          
    }    
    
  pop_state();  
}


  
  
/* parse_spec()-- Parse a set of specification statements.  Returns
 * the statement that doesn't fit. */   
   
static g95_statement parse_spec(g95_statement st) {      
st_state ss;  
  
  verify_st_order(&ss, ST_NONE); 
  if (st == ST_NONE) st = next_statement();      
      
loop:    
  switch(st) {         
  case ST_NONE:        
    unexpected_eof();

  case ST_FORMAT: case ST_ENTRY: case ST_DATA: /* Not allowed in interfaces */     
    if (g95_current_state() == COMP_INTERFACE) break;       
       
    /* Fall through */  
  
  case ST_USE:           case ST_IMPLICIT_NONE:   case ST_IMPLICIT:     
  case ST_PARAMETER:     case ST_PUBLIC:          case ST_PRIVATE:
  case ST_DERIVED_DECL:      
  case_decl:          
    if (verify_st_order(&ss, st) == FAILURE) { 
      g95_reject_statement();
      st = next_statement();  
      goto loop; 
    }   
   
    switch(st) {          
    case ST_INTERFACE:      
      parse_interface();
      break;     
     
    case ST_DERIVED_DECL:   
      parse_derived(); 
      break;         
         
    case ST_PUBLIC:  case ST_PRIVATE:      
      if (g95_current_state() != COMP_MODULE) {        
	g95_error("%s statement must appear in a MODULE",
		  g95_ascii_statement(st));        
	break;   
      }   
   
      if (g95_current_ns->default_access != ACCESS_UNKNOWN) {
	g95_error("%s statement at %C follows another accessibility "         
		  "specification", g95_ascii_statement(st));        
	break;   
      }

      g95_current_ns->default_access = (st == ST_PUBLIC)          
	  ? ACCESS_PUBLIC : ACCESS_PRIVATE;          
          
      break;         
         
    default:  
      break;     
    }   
   
    accept_statement(st);        
    st = next_statement();    
    goto loop;    
    
  default:
    break;
  }

  return st; 
}  
  
  


/* parse_where_block()-- Parse a WHERE block, (not a simple WHERE statement) */          
          
static void parse_where_block(void) {        
int seen_empty_else;       
g95_code *top, *v;   
g95_state_data c;        
g95_statement st;     
     
  top = accept_statement(ST_WHERE_BLOCK);      
  push_state(&c, COMP_WHERE, g95_new_block);          
  g95_state_stack->next = &top->next;    
    
  v = g95_get_code();       
  v->type = EXEC_WHERE;          
          
  v->expr = top->expr;  
  top->expr = NULL;         
  top->block = v;  
  top = v;     
     
  g95_state_stack->next = &v->next;     
  seen_empty_else = 0;        
        
  do {       
    st = next_statement();       
    switch(st) {    
    case ST_NONE:     
      unexpected_eof();   
   
    case ST_WHERE_BLOCK:          
      parse_where_block(); 
      /* Fall through */  
  
    case ST_ASSIGNMENT:   
    case ST_WHERE:       
      accept_statement(st);        
      break; 
 
    case ST_ELSEWHERE:          
      if (seen_empty_else) {          
	g95_error("ELSEWHERE statement at %C follows previous unmasked " 
		  "ELSEWHERE"); 
	break;      
      }   
   
      if (new_st.expr == NULL) seen_empty_else = 1; 
 
      top->block = g95_get_code();         
      top = top->block;      
      top->type = EXEC_WHERE;  
      top->expr = new_st.expr;    
    
      g95_state_stack->next = &top->next; 
      accept_statement(st);       
       
      break;          
          
    case ST_END_WHERE:     
      accept_statement(st);          
      break;         
         
    default:    
      g95_error("Unexpected %s statement in WHERE block at %C",      
		g95_ascii_statement(st));       
      g95_reject_statement();        
      break;         
    }  
  
  } while(st != ST_END_WHERE); 
 
  pop_state();         
}   
   
   


/* parse_do_block()-- Parse a DO loop.  Note that the ST_CYCLE and
 * ST_EXIT statements are handled inside of parse_executable(),
 * because they aren't really loop statements. */         
         
static void parse_do_block(void) {    
g95_statement st;     
g95_state_data z;      
g95_code *top;         
         
  z.ext.end_do_label = new_st.label;  
  if (new_st.ext.iterator != NULL)        
    z.do_variable = new_st.ext.iterator->var->symbol;      
      
  top = accept_statement(ST_DO);     
  push_state(&z, COMP_DO, g95_new_block);        
        
  g95_state_stack->next = &top->block;       
  g95_state_stack->top = top;  
  
loop:
  st = parse_executable(ST_NONE);          
          
  switch(st) {         
  case ST_NONE:  
    unexpected_eof();

  case ST_ENDDO:     
    if (z.ext.end_do_label != NULL        
        && z.ext.end_do_label != g95_statement_label)      
      g95_error_now("Statement label in ENDDO at %C doesn't match DO label");  
    /* Fall through */         
         
  case ST_IMPLIED_ENDDO:     
    break;   
   
  default:
    unexpected_statement(st);         
    goto loop;  
  }      
      
  pop_state(); 
  accept_statement(st);   
}   
   
   
     
     
/* parse_forall_block()-- Parse a FORALL block (not a simple FORALL
 * statement) */

static void parse_forall_block(void) {      
g95_state_data x;    
g95_statement st;   
g95_code *top;  
  
  top = accept_statement(ST_FORALL_BLOCK);   
   
  push_state(&x, COMP_FORALL, g95_new_block);   
  g95_state_stack->next = &top->block;    
    
  do {        
    st = next_statement();
    switch(st) {    
    
    case ST_ASSIGNMENT:    
    case ST_POINTER_ASSIGNMENT:   
    case ST_WHERE:    
    case ST_FORALL:      
      accept_statement(st);   
      break; 
 
    case ST_WHERE_BLOCK:       
      parse_where_block();    
      break;

    case ST_FORALL_BLOCK:   
      parse_forall_block();   
      break;       
       
    case ST_END_FORALL:          
      accept_statement(st);       
      break;          
          
    case ST_NONE:    
      unexpected_eof();   
   
    default:  
      g95_error("Unexpected %s statement in FORALL block at %C",          
		g95_ascii_statement(st)); 
 
      g95_reject_statement();
      break; 
    }  
  } while(st != ST_END_FORALL);     
     
  pop_state();    
}    
    
    
          
          
/* parse_contained()-- Parse a series of contained program units */    
    
static void parse_contained(int module) {         
g95_namespace *ns, *parent_ns;        
g95_state_data f, a;   
g95_statement st;
g95_symbol *symbol;         
         
  push_state(&f, COMP_CONTAINS, NULL);         
  parent_ns = g95_current_ns;      
      
  do { 
    g95_current_ns = g95_get_namespace(parent_ns);   
   
    g95_current_ns->sibling = parent_ns->contained;  
    parent_ns->contained = g95_current_ns;        
        
    st = next_statement(); 
 
    switch(st) {          
    case ST_NONE:
      unexpected_eof();          
          
    case ST_FUNCTION:         
    case ST_SUBROUTINE:        
      accept_statement(st);          
          
      push_state(&a, (st == ST_FUNCTION) ? COMP_FUNCTION : COMP_SUBROUTINE,
		 g95_new_block);         
         
      /* For contained procedures, create/update the symbol in the
       * parent namespace */  
  
      if (g95_get_symbol(g95_new_block->name, parent_ns, &symbol)) {     
	g95_error("Contained procedure '%s' at %C is already ambiguous",         
		  g95_new_block->name);
	goto parse;      
      }  
  
      if (symbol->attr.proc == PROC_UNKNOWN &&      
	  g95_add_procedure(&symbol->attr, module ? PROC_MODULE : PROC_INTERNAL,      
			    &g95_new_block->declared_at)) goto parse;   
   
      if (st == ST_FUNCTION) {     
	if (!symbol->attr.function)   
	  g95_add_function(&symbol->attr, &g95_new_block->declared_at);     
     
      } else {   
	if (!symbol->attr.subroutine) 
	  g95_add_subroutine(&symbol->attr, &g95_new_block->declared_at);     
      } 
 
    parse:         
      g95_commit_symbols();   
      parse_progunit(ST_NONE);         
         
      g95_current_ns->code = a.head;  
      g95_current_ns = parent_ns;         
         
      pop_state();    
      break;    
    
/* These statements are associated with the end of the host unit */        
        
    case ST_END_FUNCTION:   case ST_END_MODULE:     
    case ST_END_PROGRAM:    case ST_END_SUBROUTINE:        
      accept_statement(st);  
      break;     
     
    default:   
      g95_error("Unexpected %s statement in CONTAINS section at %C",      
 		g95_ascii_statement(st));      
      g95_reject_statement();  
      break;          
    }     
  } while(st != ST_END_FUNCTION && st != ST_END_SUBROUTINE &&   
	  st != ST_END_MODULE   && st != ST_END_PROGRAM);  
  
  /* The first namespace in the list is guaranteed to not have
   * anything (worthwhile) in it. */       
       
  g95_current_ns = parent_ns;         
         
  ns = g95_current_ns->contained; 
  g95_current_ns->contained = ns->sibling;        
  g95_free_namespace(ns);       
       
  pop_state();    
} 
 
 
       
       
/* parse_module()-- Parse a module subprogram */         
         
static void parse_module(void) {  
g95_statement st;  
  
  st = parse_spec(ST_NONE);   
   
loop:       
  switch(st) {   
  case ST_NONE:      
    unexpected_eof();     
     
  case ST_CONTAINS:       
    parse_contained(1);  
    break;   
   
  case ST_END_MODULE:  
    accept_statement(st);       
    break;   
   
  default:
    g95_error("Unexpected %s statement in MODULE at %C",     
	      g95_ascii_statement(st));  
  
    g95_reject_statement();         
    st = next_statement();  
    goto loop;        
  }  
}       
       
       
       
       
/* g95_parse_file()-- Top level parser. */

try g95_parse_file(void) { 
int seen_program, errors_before, errors;     
g95_state_data top, b;        
g95_statement st;       
locus prog_locus; 
 
  top.state = COMP_NONE;       
  top.sym = NULL;     
  top.previous = NULL; 
  top.head = NULL;        
  top.next = NULL;       
       
  g95_state_stack = &top;   
   
  g95_clear_new_st();         
         
  g95_statement_label = NULL;

  if (setjmp(eof)) return FAILURE;   /* Come here on unexpected EOF */

  seen_program = 0;

loop:    
  g95_init_2(); 
  st = next_statement();       
  switch(st) {        
  case ST_NONE:         
    g95_done_2(); 
    goto done;    
    
  case ST_PROGRAM:   
    if (seen_program) goto duplicate_main;     
    seen_program = 1;        
    prog_locus = *g95_current_locus();          
          
    push_state(&b, COMP_PROGRAM, g95_new_block);        
    accept_statement(st);         
    parse_progunit(ST_NONE);
    break;       
       
  case ST_SUBROUTINE:        
    push_state(&b, COMP_SUBROUTINE, g95_new_block);          
    accept_statement(st);  
    parse_progunit(ST_NONE); 
    break;      
      
  case ST_FUNCTION:     
    push_state(&b, COMP_FUNCTION, g95_new_block);          
    accept_statement(st);
    parse_progunit(ST_NONE);   
    break;         
         
  case ST_BLOCK_DATA:       
    push_state(&b, COMP_BLOCK_DATA, g95_new_block);       
    accept_statement(st); 
    parse_block_data();         
    break;    
    
  case ST_MODULE:      
    push_state(&b, COMP_MODULE, g95_new_block);      
    accept_statement(st);          
          
    g95_get_errors(NULL, &errors_before);       
    parse_module();      
    break;   
   
/* Anything else starts a nameless main program block */     
     
  default:  
    if (seen_program) goto duplicate_main;
    seen_program = 1;   
    prog_locus = *g95_current_locus();    
    
    push_state(&b, COMP_PROGRAM, g95_new_block);
    parse_progunit(st);     
    break;       
  }  
  
  g95_current_ns->code = b.head;

  g95_get_errors(NULL, &errors);       
  if (errors == 0) {    
    g95_resolve(g95_current_ns);         
         
    g95_get_errors(NULL, &errors);     
    if (errors == 0) {     
      g95_scalarize(g95_current_ns);    
    
      if (b.state == COMP_MODULE) { 
	g95_dump_module(b.sym->name, errors_before == errors);       
#ifdef IN_GCC
	g95_generate_module_code(g95_current_ns);      
      } else {  
	g95_generate_code(g95_current_ns);      
#endif
      }  
    }          
  }

  if (g95_option.verbose) g95_show_namespace(g95_current_ns);         
         
  pop_state();     
  g95_done_2();   
  goto loop;  
  
done:      
  return SUCCESS;        
        
/* If we see a duplicate main program, shut down.  If the second
 * instance is an implied main program, ie data decls or executable
 * statements, we're in for lots of errors. */  
  
duplicate_main:          
  g95_error("Two main PROGRAMs at %L and %C", &prog_locus);   
  g95_reject_statement();     
  g95_done_2();      
  return SUCCESS; 
}   
  
  
/* parse_executable()-- Accept a series of executable statements.  We
 * return the first statement that doesn't fit to the caller.  Any
 * block statements are passed on to the correct handler, which
 * usually passes the buck right back here. */         
         
static g95_statement parse_executable(g95_statement st) {          
int close_flag;         
         
  if (st == ST_NONE) st = next_statement();        
        
  for(;; st = next_statement()) {     
     
    close_flag = check_do_closure();    
    if (close_flag)  
      switch(st) {        
      case ST_GOTO:  case ST_END_PROGRAM:     case ST_RETURN: 
      case ST_EXIT:  case ST_END_FUNCTION:    case ST_CYCLE:            
      case ST_STOP:  case ST_END_SUBROUTINE:  
  
      case ST_DO: case ST_FORALL: case ST_WHERE: case ST_SELECT_CASE:       
	g95_error("%s statement at %C cannot terminate a non-block DO loop",          
		  g95_ascii_statement(st));        
	break;  
  
      default:  
	break;       
      }       
       
    switch(st) { 
    case ST_NONE:       
      unexpected_eof();        
        
    case ST_FORMAT:    case ST_DATA:    case ST_ENTRY:  
    case_executable: 
      accept_statement(st);
      if (close_flag == 1) return ST_IMPLIED_ENDDO;    
      continue;       
       
    case ST_IF_BLOCK:      
      parse_if_block(); 
      continue;     
     
    case ST_SELECT_CASE:        
      parse_select_block();  
      continue;     
     
    case ST_DO:
      parse_do_block();      
      if (check_do_closure() == 1) return ST_IMPLIED_ENDDO;   
      continue;  
  
    case ST_WHERE_BLOCK:        
      parse_where_block();   
      continue;   
   
    case ST_FORALL_BLOCK:   
      parse_forall_block();       
      continue;      
      
    default:  
      break;       
    }        
            
    break;         
  }        
        
  return st;  
}      
      
      


/* parse_progunit()-- Parse a PROGRAM, SUBROUTINE or FUNCTION unit */

static void parse_progunit(g95_statement st) {     
g95_state_data *s;         
int v;         
         
  g95_state_stack->next = &g95_current_ns->code;  
  
  st = parse_spec(st);     
  switch(st) {      
  case ST_NONE:     
    unexpected_eof();      
      
  case ST_CONTAINS:      
    goto contains;

  case_end:   
    accept_statement(st);
    goto done;  
  
  default:    
    break;  
  }          
          
loop:       
  for(;;) {          
    st = parse_executable(st);

    switch(st) {          
    case ST_NONE: 
      unexpected_eof();  
  
    case ST_CONTAINS:   
      goto contains;     
       
    case_end:
      accept_statement(st);        
      goto done;

    default:      
      break;          
    }   
   
    unexpected_statement(st);     
    g95_reject_statement();        
    st = next_statement();          
  }       
       
contains:    
  v = 0;       
       
  for(s=g95_state_stack; s; s=s->previous)    
    if (s->state == COMP_CONTAINS) v++;      
      
  if (g95_find_state(COMP_MODULE) == SUCCESS) v--;        
        
  if (v > 0) {    
    g95_error("CONTAINS statement at %C is already in a contained "          
	      "program unit");   
    st = next_statement();     
    goto loop; 
  }       
       
  parse_contained(0);

done:      
  return;        
}     
     
     
