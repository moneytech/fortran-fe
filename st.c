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
    
    
      
      
/* free_statement()-- Free a single code structure, but not the actual
 * structure itself. */   
   
static void free_statement(g95_code *f) {    
    
  if (f->expr) g95_free_expr(f->expr); 
  if (f->expr2) g95_free_expr(f->expr2);  
  
  switch(f->type) {    
  case EXEC_NOP:      case EXEC_ASSIGN:     case EXEC_GOTO:  case EXEC_CYCLE:       
  case EXEC_RETURN:   case EXEC_STOP:  case EXEC_EXIT: 
  case EXEC_WHERE:    case EXEC_IOLENGTH:   case EXEC_POINTER_ASSIGN:   
  case EXEC_DO_WHILE: case EXEC_CONTINUE:   case EXEC_TRANSFER:        
  case EXEC_ARITHMETIC_IF:     
    break;          
          
  case EXEC_CALL:         
    g95_free_actual_arglist(f->ext.actual);      
    break; 
 
  case EXEC_SELECT:          
    if (f->ext.case_list) g95_free_case_list(f->ext.case_list);          
    break;     
     
  case EXEC_DO:        
    g95_free_iterator(f->ext.iterator, 1);    
    break; 
 
  case EXEC_ALLOCATE: 
  case EXEC_DEALLOCATE:         
    g95_free_alloc_list(f->ext.alloc_list);    
    break; 
 
  case EXEC_OPEN:  
    g95_free_open(f->ext.open);     
    break;    
    
  case EXEC_CLOSE:    
    g95_free_close(f->ext.close);   
    break;

  case EXEC_BACKSPACE: 
  case EXEC_ENDFILE:     
  case EXEC_REWIND:
    g95_free_filepos(f->ext.filepos);         
    break;      
      
  case EXEC_IF:      
    g95_free_statements(f->ext.block);  
    break; 
 
  case EXEC_INQUIRE:      
    g95_free_inquire(f->ext.inquire);   
    break;         
         
  case EXEC_READ:
  case EXEC_WRITE:        
    g95_free_dt(f->ext.dt);     
    break;   
   
  case EXEC_DT_END:   
    /* The ext.dt member is a duplicate pointer and doesn't need to be freed */        
    break;       
       
  case EXEC_FORALL:       
    g95_free_forall_iterator(f->ext.forall_iterator);       
    break;        
        
  default:       
    g95_internal_error("free_statement(): Bad statement");
  }    
}          
          
          
 
 
/* g95_get_code()-- Get a g95_code structure */        
        
g95_code *g95_get_code(void) {         
g95_code *w;   
   
  w = g95_getmem(sizeof(g95_code));       
  w->loc = *g95_current_locus();          
  return w;     
}        
        
        
   
   
/* g95_undo_statement()-- Frees everything associated with the current
 * statement.  */      
      
void g95_undo_statement(void) {    
    
  g95_free_statements(new_st.block); 
  g95_free_statements(new_st.next);      
      
  free_statement(&new_st);  
  g95_clear_new_st();    
}   
   
   


/* g95_add_statement()-- Takes the current new_st code structure and
 * adds it to the current program unit.  As a side-effect, it zeroes
 * the new_st. */   
   
g95_code *g95_add_statement(void) {   
g95_code *d;      
      
  d = g95_get_code();     
  *d = new_st;    
    
  d->loc = *g95_current_locus();  
  
  if (g95_state_stack->head == NULL) g95_state_stack->head = d;        
  *g95_state_stack->next = d;

  while(d->next != NULL)  
    d = d->next;   
   
  g95_state_stack->next = &d->next;   
   
  g95_clear_new_st();       
       
  return d;
}


          
          
/* code_indent()-- Do indentation for a specific level */         
         
static void code_indent(int lvl, g95_st_label *labl) {     
int w; 
 
  if (labl != NULL)     
    g95_status("%-5d ", labl->value);        
  else  
    g95_status("      ");      
      
  for(w=0; w<2*lvl; w++)    
    g95_status_char(' ');
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
  
  
         
         
/* g95_free_statements()-- Free a code statement and all other code
 * structures linked to it. */       
       
void g95_free_statements(g95_code *s) {       
g95_code *v;        
        
  for(; s; s=v) {       
    v = s->next;     
     
    if (s->block) g95_free_statements(s->block);       
    free_statement(s);        
    g95_free(s); 
  }
}      
      
      
          
          
/* g95_show_code_node()-- Show a single code node and everything
 * underneath it if necessary. */        
        
static void g95_show_code_node(int m, g95_code *w) {        
g95_forall_iterator *fa;     
g95_open *open;     
g95_case *cp;     
g95_alloc *t;        
g95_code *r;   
g95_close *close;
g95_filepos *fp1; 
g95_inquire *o; 
g95_dt *datat;  
  
  code_indent(m, w->here);       
      
  switch(w->type) {  
  case EXEC_NOP:      
    g95_status("NOP");          
    break;       
       
  case EXEC_CONTINUE:        
    g95_status("CONTINUE");         
    break;

  case EXEC_ASSIGN:   
    g95_status("ASSIGN ");  
    g95_show_expr(w->expr);       
    g95_status_char(' ');     
    g95_show_expr(w->expr2);       
    break;  
  
  case EXEC_POINTER_ASSIGN:       
    g95_status("POINTER ASSIGN ");  
    g95_show_expr(w->expr);        
    g95_status_char(' ');         
    g95_show_expr(w->expr2);
    break;          
          
  case EXEC_GOTO:    
    g95_status("GOTO %d", w->label->value);  
    break;     
     
  case EXEC_CALL:       
    g95_status("CALL (%s:%s) %s ", w->sym->module, w->sym->name,     
	       w->sub_name);      
    g95_show_actual_arglist(w->ext.actual);          
    break;   
   
  case EXEC_RETURN:
    g95_status("RETURN ");   
    if (w->expr) g95_show_expr(w->expr);        
    break;        
        
  case EXEC_STOP:  
    g95_status("STOP ");

    if (w->expr != NULL)          
      g95_show_expr(w->expr);   
    else   
      g95_status("%d", w->ext.stop_code);       
       
    break;        
        
  case EXEC_ARITHMETIC_IF:        
    g95_status("IF "); 
    g95_show_expr(w->expr);         
    g95_status(" %d, %d, %d",    
	       w->label->value, w->label2->value, w->label3->value);          
    break;   
   
  case EXEC_IF:      
    g95_status("IF ");          
    g95_show_expr(w->expr);  
    g95_status_char('\n');         
    g95_show_code(m+1, w->block);

    if (w->ext.block != NULL) {         
      code_indent(m, 0);   
      g95_status("ELSE\n");        
      g95_show_code(m+1, w->ext.block);   
    }         
         
    code_indent(m, w->label);  
    g95_status("ENDIF");     
    break;   
   
  case EXEC_SELECT:   
    r = w->block;      
    g95_status("SELECT CASE ");   
    g95_show_expr((w->expr != NULL) ? w->expr : w->expr2);     
    g95_status_char('\n');    
    
    for(;r ;r=r->block) { 
      code_indent(m, 0);   
   
      g95_status("CASE ");
      for(cp=r->ext.case_list; cp; cp=cp->next) { 
	g95_status_char('(');     
	g95_show_expr(cp->low);       
	g95_status_char(' ');       
	g95_show_expr(cp->high);          
	g95_status_char(')');    
	g95_status_char(' ');       
      }
      g95_status_char('\n');    
    
      g95_show_code(m+1, r->next);        
    }       
       
    code_indent(m, w->label);    
    g95_status("END SELECT");          
    break;        
        
  case EXEC_WHERE:        
    g95_status("WHERE ");       
       
    r = w->block;
    g95_show_expr(r->expr);
    g95_status_char('\n');   
   
    g95_show_code(m+1, r->next);

    for(r=r->block; r; r=r->block) {
      code_indent(m, 0);        
      g95_status("ELSE WHERE ");         
      g95_show_expr(r->expr);    
      g95_status_char('\n');  
      g95_show_code(m+1, r->next); 
    }   
   
    code_indent(m, 0);
    g95_status("END WHERE");     
    break;    
    
    
  case EXEC_FORALL:        
    g95_status("FORALL ");         
    for(fa=w->ext.forall_iterator; fa; fa=fa->next) {  
      g95_show_expr(fa->var);
      g95_status_char(' ');    
      g95_show_expr(fa->start);         
      g95_status_char(':');     
      g95_show_expr(fa->end); 
      g95_status_char(':');
      g95_show_expr(fa->stride);     
     
      if (fa->next != NULL) g95_status_char(',');       
    }        
        
    if (w->expr != NULL) {     
      g95_status_char(',');      
      g95_show_expr(w->expr); 
    }        
    g95_status_char('\n');      
      
    g95_show_code(m+1, w->block->next);

    code_indent(m, 0);  
    g95_status("END FORALL");  
    break;      
          
  case EXEC_DO:       
    g95_status("DO ");    
    
    g95_show_expr(w->ext.iterator->var);         
    g95_status_char('=');
    g95_show_expr(w->ext.iterator->start);      
    g95_status_char(' ');      
    g95_show_expr(w->ext.iterator->end);        
    g95_status_char(' ');       
    g95_show_expr(w->ext.iterator->step);         
    g95_status_char('\n');       
       
    g95_show_code(m+1, w->block);        
        
    code_indent(m, 0);       
    g95_status("END DO");      
    break;   
   
  case EXEC_DO_WHILE:  
    g95_status("DO WHILE ");        
    g95_show_expr(w->expr);  
    g95_status_char('\n');     
     
    g95_show_code(m+1, w->block);    
    
    code_indent(m, w->label); 
    g95_status("END DO");         
    break;      
      
  case EXEC_CYCLE:  
    g95_status("CYCLE"); 
    if (w->sym) g95_status(" %s", w->sym->name);   
    break;      
      
  case EXEC_EXIT:        
    g95_status("EXIT");          
    if (w->sym) g95_status(" %s", w->sym->name);        
    break; 
 
  case EXEC_ALLOCATE:        
    g95_status("ALLOCATE ");        
    if (w->expr) {        
      g95_status(" STAT="); 
      g95_show_expr(w->expr);  
    }    
    
    for(t=w->ext.alloc_list; t; t=t->next) {     
      g95_status_char(' ');    
      g95_show_expr(t->expr);       
    }    
    
    break;    
    
  case EXEC_DEALLOCATE: 
    g95_status("DEALLOCATE ");   
    if (w->expr) {       
      g95_status(" STAT=");      
      g95_show_expr(w->expr);         
    }     
     
    for(t=w->ext.alloc_list; t; t=t->next) {         
      g95_status_char(' ');    
      g95_show_expr(t->expr);  
    }      
      
    break;         
         
  case EXEC_OPEN:      
    g95_status("OPEN");          
    open = w->ext.open;     
     
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
    close = w->ext.close;     
     
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
    fp1 = w->ext.filepos;         
         
    if (fp1->unit) { g95_status(" UNIT="); g95_show_expr(fp1->unit); }      
    if (fp1->iostat) { g95_status(" IOSTAT="); g95_show_expr(fp1->iostat); } 
    if (fp1->err != NULL) g95_status(" ERR=%d", fp1->err->value);     
    break;      
      
  case EXEC_INQUIRE:    
    g95_status("INQUIRE");  
    o = w->ext.inquire;          
          
    if (o->unit) { g95_status(" UNIT="); g95_show_expr(o->unit); }
    if (o->file) { g95_status(" FILE="); g95_show_expr(o->file); } 
 
    if (o->iostat) { g95_status(" IOSTAT="); g95_show_expr(o->iostat); }        
    if (o->exist) { g95_status(" EXIST="); g95_show_expr(o->exist); }      
    if (o->opened) { g95_status(" OPENED="); g95_show_expr(o->opened); }
    if (o->number) { g95_status(" NUMBER="); g95_show_expr(o->number); }      
    if (o->named) { g95_status(" NAMED="); g95_show_expr(o->named); }       
    if (o->name) { g95_status(" NAME="); g95_show_expr(o->name); }   
    if (o->access) { g95_status(" ACCESS="); g95_show_expr(o->access); } 
    if (o->sequential) { g95_status(" SEQUENTIAL=");    
      g95_show_expr(o->sequential); }  
  
    if (o->direct) { g95_status(" DIRECT="); g95_show_expr(o->direct); }        
    if (o->form) { g95_status(" FORM="); g95_show_expr(o->form); }   
    if (o->formatted) { g95_status(" FORMATTED"); g95_show_expr(o->formatted);}          
    if (o->unformatted) { g95_status(" UNFORMATTED="); 
      g95_show_expr(o->unformatted); }    
    if (o->recl) { g95_status(" RECL="); g95_show_expr(o->recl); }  
    if (o->nextrec) { g95_status(" NEXTREC="); g95_show_expr(o->nextrec); }      
    if (o->blank) { g95_status(" BLANK="); g95_show_expr(o->blank); }       
    if (o->position) { g95_status(" POSITION="); g95_show_expr(o->position); }
    if (o->action) { g95_status(" ACTION="); g95_show_expr(o->action); }    
    if (o->read) { g95_status(" READ="); g95_show_expr(o->read); }   
    if (o->write) { g95_status(" WRITE="); g95_show_expr(o->write); }
    if (o->readwrite) {g95_status(" READWRITE="); g95_show_expr(o->readwrite);}     
    if (o->delim) { g95_status(" DELIM="); g95_show_expr(o->delim); }         
    if (o->pad) { g95_status(" PAD="); g95_show_expr(o->pad); }   
   
    if (o->err != NULL) g95_status(" ERR=%d", o->err->value);      
    break;       
       
  case EXEC_IOLENGTH:  
    g95_status("IOLENGTH ");   
    g95_show_expr(w->expr);  
    break;      
      
  case EXEC_READ:        
    g95_status("READ");         
    goto show_dt; 
 
  case EXEC_WRITE:         
    g95_status("WRITE");       
       
  show_dt:       
    datat = w->ext.dt;     
    if (datat->io_unit) {  
      g95_status(" UNIT="); g95_show_expr(datat->io_unit); }          
          
    if (datat->format_expr) {      
      g95_status(" FMT="); g95_show_expr(datat->format_expr); }   
   
    if (datat->format_label != NULL)          
      g95_status(" FMT=%d", datat->format_label->value);
    if (datat->namelist) g95_status(" NML=%s", datat->namelist->name);       
    if (datat->iostat) { g95_status(" IOSTAT="); g95_show_expr(datat->iostat); }     
    if (datat->size) { g95_status(" SIZE="); g95_show_expr(datat->size); }         
    if (datat->rec) { g95_status(" REC="); g95_show_expr(datat->rec); }
    if (datat->advance) { g95_status(" ADVANCE="); g95_show_expr(datat->advance); }   
   
    break;      
      
  case EXEC_TRANSFER:         
    g95_status("TRANSFER "); 
    g95_show_expr(w->expr);         
    break;      
      
  case EXEC_DT_END:    
    g95_status("DT_END");  
    datat = w->ext.dt;  
  
    if (datat->err != NULL) g95_status(" ERR=%d", datat->err->value);        
    if (datat->end != NULL) g95_status(" END=%d", datat->end->value);          
    if (datat->eor != NULL) g95_status(" EOR=%d", datat->eor->value);          
    break;  
  
  default:           
    g95_internal_error("g95_show_code_node(): Bad statement code");        
  }        
        
  g95_status_char('\n'); 
} 
 
 
        
        
/* g95_show_code()-- Show a list of code structures. */     
     
void g95_show_code(int level, g95_code *a) {     
     
  for(;a ; a=a->next)         
    g95_show_code_node(level, a);   
}   
   
      
      
/* g95_clear_new_st()-- Zeroes out the new_st structure. */      
      
void g95_clear_new_st(void) { 
 
  memset(&new_st, '\0', sizeof(new_st));  
  new_st.type = EXEC_NOP; 
}        
        
        
