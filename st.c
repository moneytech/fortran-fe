/* Build executable statement trees
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
     
/* st.c-- Executable statements are strung together into a singly
 * linked list of code structures.  These structures are later
 * translated into GBE tree structures and from there to executable
 * code for a target.  */   
   
#include "g95.h"
#include <string.h>
 
g95_code new_st;          
          
          
 
 
/* g95_add_statement()-- Takes the current new_st code structure and
 * adds it to the current program unit.  As a side-effect, it zeroes
 * the new_st. */    
    
g95_code *g95_add_statement(void) {    
g95_code *x;  
  
  x = g95_get_code();     
  *x = new_st;    
    
  x->loc = *g95_current_locus();

  if (g95_state_stack->head == NULL) g95_state_stack->head = x; 
  *g95_state_stack->next = x;  
  
  while(x->next != NULL)   
    x = x->next;  
  
  g95_state_stack->next = &x->next;      
      
  g95_clear_new_st();        
        
  return x; 
}          
          
          
         
         
/* g95_get_code()-- Get a g95_code structure */  
  
g95_code *g95_get_code(void) {          
g95_code *o;   
   
  o = g95_getmem(sizeof(g95_code));    
  o->loc = *g95_current_locus();  
  return o;
}       
       
       


/* g95_clear_new_st()-- Zeroes out the new_st structure. */         
         
void g95_clear_new_st(void) {       
       
  memset(&new_st, '\0', sizeof(new_st));   
  new_st.type = EXEC_NOP;     
}    
    
    
    
    
/* code_indent()-- Do indentation for a specific level */       
       
static void code_indent(int level, g95_st_label *label) {   
int d;     
     
  if (label != NULL)        
    g95_status("%-5d ", label->value); 
  else          
    g95_status("      ");         
         
  for(d=0; d<2*level; d++)     
    g95_status_char(' ');          
}  
  
  
       
       
/* free_statement()-- Free a single code structure, but not the actual
 * structure itself. */          
          
static void free_statement(g95_code *i) {    
    
  if (i->expr) g95_free_expr(i->expr);     
  if (i->expr2) g95_free_expr(i->expr2);         
         
  switch(i->type) {     
  case EXEC_NOP:      case EXEC_ASSIGN:     case EXEC_GOTO:  case EXEC_CYCLE:       
  case EXEC_RETURN:   case EXEC_STOP:  case EXEC_EXIT:    
  case EXEC_WHERE:    case EXEC_IOLENGTH:   case EXEC_POINTER_ASSIGN:  
  case EXEC_DO_WHILE: case EXEC_CONTINUE:   case EXEC_TRANSFER: 
  case EXEC_ARITHMETIC_IF:    
    break;          
          
  case EXEC_CALL: 
    g95_free_actual_arglist(i->ext.actual);     
    break;

  case EXEC_SELECT: 
    if (i->ext.case_list) g95_free_case_list(i->ext.case_list); 
    break;        
        
  case EXEC_DO:          
    g95_free_iterator(i->ext.iterator, 1);          
    break;   
   
  case EXEC_ALLOCATE:     
  case EXEC_DEALLOCATE:       
    g95_free_alloc_list(i->ext.alloc_list);          
    break;  
  
  case EXEC_OPEN:       
    g95_free_open(i->ext.open);  
    break;    
    
  case EXEC_CLOSE:
    g95_free_close(i->ext.close);     
    break;       
       
  case EXEC_BACKSPACE:    
  case EXEC_ENDFILE:  
  case EXEC_REWIND:         
    g95_free_filepos(i->ext.filepos);        
    break;         
         
  case EXEC_IF:
    g95_free_statements(i->ext.block);         
    break;     
     
  case EXEC_INQUIRE:        
    g95_free_inquire(i->ext.inquire);          
    break;          
          
  case EXEC_READ:        
  case EXEC_WRITE: 
    g95_free_dt(i->ext.dt);   
    break;         
         
  case EXEC_DT_END:
    /* The ext.dt member is a duplicate pointer and doesn't need to be freed */          
    break;         
         
  case EXEC_FORALL:        
    g95_free_forall_iterator(i->ext.forall_iterator);    
    break;

  default:          
    g95_internal_error("free_statement(): Bad statement");  
  }  
}      
      
      
         
         
/* g95_append_code()-- Given some part of a g95_code structure, append
 * a set of code to its tail, returning a pointer to the new tail. */ 
 
g95_code *g95_append_code(g95_code *tail, g95_code *new) {      
      
  if (tail != NULL) { 
    while(tail->next != NULL)         
      tail = tail->next;

    tail->next = new;          
  }     
     
  while(new->next != NULL)          
    new = new->next;       
       
  return new;         
}         
         
         
         
         
/* g95_undo_statement()-- Frees everything associated with the current
 * statement.  */      
      
void g95_undo_statement(void) {    
    
  g95_free_statements(new_st.block); 
  g95_free_statements(new_st.next);    
    
  free_statement(&new_st);  
  g95_clear_new_st();  
} 
 
 
 
 
/* g95_show_code_node()-- Show a single code node and everything
 * underneath it if necessary. */          
          
static void g95_show_code_node(int level, g95_code *s) {        
g95_forall_iterator *fa;        
g95_open *open;  
g95_case *cp;          
g95_alloc *w;   
g95_code *o;       
g95_close *close;        
g95_filepos *fp;     
g95_inquire *v;          
g95_dt *dt;   
   
  code_indent(level, s->here);     
    
  switch(s->type) {          
  case EXEC_NOP:       
    g95_status("NOP"); 
    break; 
 
  case EXEC_CONTINUE: 
    g95_status("CONTINUE");         
    break;       
       
  case EXEC_ASSIGN:  
    g95_status("ASSIGN ");
    g95_show_expr(s->expr);    
    g95_status_char(' ');     
    g95_show_expr(s->expr2); 
    break;          
          
  case EXEC_POINTER_ASSIGN:          
    g95_status("POINTER ASSIGN ");    
    g95_show_expr(s->expr);    
    g95_status_char(' ');  
    g95_show_expr(s->expr2); 
    break;        
        
  case EXEC_GOTO:       
    g95_status("GOTO %d", s->label->value);   
    break;      
      
  case EXEC_CALL:        
    g95_status("CALL (%s:%s) %s ", s->sym->module, s->sym->name,       
	       s->sub_name);   
    g95_show_actual_arglist(s->ext.actual);  
    break;         
         
  case EXEC_RETURN:         
    g95_status("RETURN ");    
    if (s->expr) g95_show_expr(s->expr);          
    break;       
       
  case EXEC_STOP:         
    g95_status("STOP ");      
      
    if (s->expr != NULL)        
      g95_show_expr(s->expr);          
    else         
      g95_status("%d", s->ext.stop_code);         
         
    break;   
   
  case EXEC_ARITHMETIC_IF:         
    g95_status("IF ");   
    g95_show_expr(s->expr);   
    g95_status(" %d, %d, %d",          
	       s->label->value, s->label2->value, s->label3->value); 
    break;          
          
  case EXEC_IF:
    g95_status("IF ");    
    g95_show_expr(s->expr);         
    g95_status_char('\n');       
    g95_show_code(level+1, s->block);     
     
    if (s->ext.block != NULL) {      
      code_indent(level, 0);       
      g95_status("ELSE\n");       
      g95_show_code(level+1, s->ext.block);         
    }          
          
    code_indent(level, s->label);
    g95_status("ENDIF");         
    break;         
         
  case EXEC_SELECT:
    o = s->block;  
    g95_status("SELECT CASE "); 
    g95_show_expr((s->expr != NULL) ? s->expr : s->expr2);     
    g95_status_char('\n');

    for(;o ;o=o->block) {       
      code_indent(level, 0);         
         
      g95_status("CASE ");         
      for(cp=o->ext.case_list; cp; cp=cp->next) {       
	g95_status_char('(');
	g95_show_expr(cp->low);          
	g95_status_char(' ');
	g95_show_expr(cp->high);         
	g95_status_char(')');         
	g95_status_char(' ');
      } 
      g95_status_char('\n');   
   
      g95_show_code(level+1, o->next);      
    }       
       
    code_indent(level, s->label);          
    g95_status("END SELECT");          
    break;      
      
  case EXEC_WHERE:        
    g95_status("WHERE ");         
         
    o = s->block;     
    g95_show_expr(o->expr);       
    g95_status_char('\n');         
         
    g95_show_code(level+1, o->next);          
          
    for(o=o->block; o; o=o->block) {     
      code_indent(level, 0);
      g95_status("ELSE WHERE ");
      g95_show_expr(o->expr);     
      g95_status_char('\n');         
      g95_show_code(level+1, o->next);          
    }    
    
    code_indent(level, 0);  
    g95_status("END WHERE");         
    break;     
     
     
  case EXEC_FORALL:      
    g95_status("FORALL ");     
    for(fa=s->ext.forall_iterator; fa; fa=fa->next) {     
      g95_show_expr(fa->var);          
      g95_status_char(' ');        
      g95_show_expr(fa->start);          
      g95_status_char(':'); 
      g95_show_expr(fa->end);     
      g95_status_char(':');  
      g95_show_expr(fa->stride);          
          
      if (fa->next != NULL) g95_status_char(',');      
    }      
      
    if (s->expr != NULL) {
      g95_status_char(',');      
      g95_show_expr(s->expr);   
    }         
    g95_status_char('\n');  
  
    g95_show_code(level+1, s->block->next);    
    
    code_indent(level, 0);   
    g95_status("END FORALL");       
    break;
    
  case EXEC_DO:          
    g95_status("DO ");

    g95_show_expr(s->ext.iterator->var);        
    g95_status_char('=');     
    g95_show_expr(s->ext.iterator->start);    
    g95_status_char(' ');      
    g95_show_expr(s->ext.iterator->end);
    g95_status_char(' ');        
    g95_show_expr(s->ext.iterator->step);    
    g95_status_char('\n');  
  
    g95_show_code(level+1, s->block);     
     
    code_indent(level, 0); 
    g95_status("END DO");  
    break;  
  
  case EXEC_DO_WHILE:
    g95_status("DO WHILE ");          
    g95_show_expr(s->expr); 
    g95_status_char('\n');      
      
    g95_show_code(level+1, s->block);

    code_indent(level, s->label);        
    g95_status("END DO");        
    break;  
  
  case EXEC_CYCLE:   
    g95_status("CYCLE");         
    if (s->sym) g95_status(" %s", s->sym->name);     
    break;    
    
  case EXEC_EXIT:     
    g95_status("EXIT");  
    if (s->sym) g95_status(" %s", s->sym->name);
    break;        
        
  case EXEC_ALLOCATE:   
    g95_status("ALLOCATE ");  
    if (s->expr) {
      g95_status(" STAT=");    
      g95_show_expr(s->expr);      
    }    
    
    for(w=s->ext.alloc_list; w; w=w->next) {
      g95_status_char(' '); 
      g95_show_expr(w->expr);     
    }    
    
    break;        
        
  case EXEC_DEALLOCATE:    
    g95_status("DEALLOCATE ");    
    if (s->expr) {        
      g95_status(" STAT=");          
      g95_show_expr(s->expr);   
    }   
   
    for(w=s->ext.alloc_list; w; w=w->next) {
      g95_status_char(' ');      
      g95_show_expr(w->expr);     
    }     
     
    break;          
          
  case EXEC_OPEN:      
    g95_status("OPEN");          
    open = s->ext.open;     
     
    if (open->unit) { g95_status(" UNIT="); g95_show_expr(open->unit); }      
    if (open->iostat) { g95_status(" IOSTAT="); g95_show_expr(open->iostat); }       
    if (open->file) { g95_status(" FILE="); g95_show_expr(open->file); }       
    if (open->status) { g95_status(" STATUS="); g95_show_expr(open->status); }    
    if (open->access) { g95_status(" ACCESS="); g95_show_expr(open->access); }     
    if (open->form) { g95_status(" FORM="); g95_show_expr(open->form); }     
    if (open->recl) { g95_status(" RECL="); g95_show_expr(open->recl); }  
    if (open->blank) { g95_status(" BLANK="); g95_show_expr(open->blank); }     
    if (open->position) {      
      g95_status(" POSITION="); g95_show_expr(open->position); }
    if (open->action) { g95_status(" ACTION="); g95_show_expr(open->action); }       
    if (open->delim) { g95_status(" DELIM="); g95_show_expr(open->delim); }         
    if (open->pad) { g95_status(" PAD="); g95_show_expr(open->pad); }        
    if (open->err != NULL) g95_status(" ERR=%d", open->err->value);

    break;        
        
  case EXEC_CLOSE:  
    g95_status("CLOSE");    
    close = s->ext.close;   
   
    if (close->unit) { g95_status(" UNIT="); g95_show_expr(close->unit); }        
    if (close->iostat) { g95_status(" IOSTAT="); g95_show_expr(close->iostat);}          
    if (close->status) { g95_status(" STATUS=");g95_show_expr(close->status); }         
    if (close->err != NULL) g95_status(" ERR=%d", close->err->value);     
    break;      
      
  case EXEC_BACKSPACE:          
    g95_status("BACKSPACE");   
    goto show_filepos;

  case EXEC_ENDFILE:         
    g95_status("ENDFILE");         
    goto show_filepos;     
     
  case EXEC_REWIND:        
    g95_status("REWIND");         
         
  show_filepos:
    fp = s->ext.filepos;  
  
    if (fp->unit) { g95_status(" UNIT="); g95_show_expr(fp->unit); }        
    if (fp->iostat) { g95_status(" IOSTAT="); g95_show_expr(fp->iostat); }       
    if (fp->err != NULL) g95_status(" ERR=%d", fp->err->value);         
    break;   
   
  case EXEC_INQUIRE:   
    g95_status("INQUIRE");  
    v = s->ext.inquire;

    if (v->unit) { g95_status(" UNIT="); g95_show_expr(v->unit); }      
    if (v->file) { g95_status(" FILE="); g95_show_expr(v->file); }          
          
    if (v->iostat) { g95_status(" IOSTAT="); g95_show_expr(v->iostat); }        
    if (v->exist) { g95_status(" EXIST="); g95_show_expr(v->exist); }
    if (v->opened) { g95_status(" OPENED="); g95_show_expr(v->opened); }    
    if (v->number) { g95_status(" NUMBER="); g95_show_expr(v->number); }   
    if (v->named) { g95_status(" NAMED="); g95_show_expr(v->named); }  
    if (v->name) { g95_status(" NAME="); g95_show_expr(v->name); }   
    if (v->access) { g95_status(" ACCESS="); g95_show_expr(v->access); } 
    if (v->sequential) { g95_status(" SEQUENTIAL=");     
      g95_show_expr(v->sequential); }   
   
    if (v->direct) { g95_status(" DIRECT="); g95_show_expr(v->direct); }        
    if (v->form) { g95_status(" FORM="); g95_show_expr(v->form); }       
    if (v->formatted) { g95_status(" FORMATTED"); g95_show_expr(v->formatted);}     
    if (v->unformatted) { g95_status(" UNFORMATTED="); 
      g95_show_expr(v->unformatted); }  
    if (v->recl) { g95_status(" RECL="); g95_show_expr(v->recl); }    
    if (v->nextrec) { g95_status(" NEXTREC="); g95_show_expr(v->nextrec); }         
    if (v->blank) { g95_status(" BLANK="); g95_show_expr(v->blank); }     
    if (v->position) { g95_status(" POSITION="); g95_show_expr(v->position); } 
    if (v->action) { g95_status(" ACTION="); g95_show_expr(v->action); }          
    if (v->read) { g95_status(" READ="); g95_show_expr(v->read); }       
    if (v->write) { g95_status(" WRITE="); g95_show_expr(v->write); }  
    if (v->readwrite) {g95_status(" READWRITE="); g95_show_expr(v->readwrite);}         
    if (v->delim) { g95_status(" DELIM="); g95_show_expr(v->delim); }     
    if (v->pad) { g95_status(" PAD="); g95_show_expr(v->pad); } 
 
    if (v->err != NULL) g95_status(" ERR=%d", v->err->value); 
    break; 
 
  case EXEC_IOLENGTH:    
    g95_status("IOLENGTH ");
    g95_show_expr(s->expr);          
    break;   
   
  case EXEC_READ:       
    g95_status("READ");  
    goto show_dt;          
          
  case EXEC_WRITE:        
    g95_status("WRITE");   
   
  show_dt:   
    dt = s->ext.dt;   
    if (dt->io_unit) {   
      g95_status(" UNIT="); g95_show_expr(dt->io_unit); }        
        
    if (dt->format_expr) {         
      g95_status(" FMT="); g95_show_expr(dt->format_expr); }       
       
    if (dt->format_label != NULL)          
      g95_status(" FMT=%d", dt->format_label->value);  
    if (dt->namelist) g95_status(" NML=%s", dt->namelist->name); 
    if (dt->iostat) { g95_status(" IOSTAT="); g95_show_expr(dt->iostat); }   
    if (dt->size) { g95_status(" SIZE="); g95_show_expr(dt->size); }      
    if (dt->rec) { g95_status(" REC="); g95_show_expr(dt->rec); }     
    if (dt->advance) { g95_status(" ADVANCE="); g95_show_expr(dt->advance); }   
   
    break;       
       
  case EXEC_TRANSFER:  
    g95_status("TRANSFER ");         
    g95_show_expr(s->expr);        
    break;          
          
  case EXEC_DT_END:  
    g95_status("DT_END");  
    dt = s->ext.dt;         
         
    if (dt->err != NULL) g95_status(" ERR=%d", dt->err->value);   
    if (dt->end != NULL) g95_status(" END=%d", dt->end->value);         
    if (dt->eor != NULL) g95_status(" EOR=%d", dt->eor->value);    
    break;          
          
  default:       
    g95_internal_error("g95_show_code_node(): Bad statement code");      
  } 
 
  g95_status_char('\n');
}          
          
          
   
   
/* g95_free_statements()-- Free a code statement and all other code
 * structures linked to it. */      
      
void g95_free_statements(g95_code *j) {      
g95_code *y;        
        
  for(; j; j=y) { 
    y = j->next;        
        
    if (j->block) g95_free_statements(j->block);         
    free_statement(j);     
    g95_free(j);         
  }     
}        
        
        
    
    
/* g95_show_code()-- Show a list of code structures. */         
         
void g95_show_code(int level, g95_code *m) {     
     
  for(;m ; m=m->next)     
    g95_show_code_node(level, m);
}  
  
