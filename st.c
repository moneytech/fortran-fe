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
g95_code *l;     
     
  l = g95_get_code();
  *l = new_st;        
        
  l->where = g95_current_locus;         
         
  if (g95_state_stack->head == NULL) g95_state_stack->head = l;          
  *g95_state_stack->next = l;   
   
  while(l->next != NULL) 
    l = l->next;       
       
  g95_state_stack->next = &l->next;      
      
  g95_clear_new_st();     
     
  return l;    
}


   
   
/* g95_clear_new_st()-- Zeroes out the new_st structure. */ 
 
void g95_clear_new_st(void) {          
          
  memset(&new_st, '\0', sizeof(new_st));
  new_st.type = EXEC_NOP;
}        
        
        


/* g95_get_code()-- Get a g95_code structure */ 
 
g95_code *g95_get_code(void) {
g95_code *h;   
   
  h = g95_getmem(sizeof(g95_code));   
  h->where = g95_current_locus; 
  return h;         
}      
      
      
 
 
/* g95_append_code()-- Given some part of a g95_code structure, append
 * a set of code to its tail, returning a pointer to the new tail. */    
    
g95_code *g95_append_code(g95_code *t, g95_code *new) {     
     
  if (t != NULL) {   
    while(t->next != NULL)       
      t = t->next;      
      
    t->next = new;  
  }        
        
  while(new->next != NULL)    
    new = new->next;        
        
  return new;        
}        
        
        
        
        
/* free_statement()-- Free a single code structure, but not the actual
 * structure itself. */     
     
static void free_statement(g95_code *b) {          
          
  if (b->expr) g95_free_expr(b->expr);      
  if (b->expr2) g95_free_expr(b->expr2);      
      
  switch(b->type) {
  case EXEC_NOP:    case EXEC_ASSIGN:   case EXEC_GOTO:     case EXEC_CYCLE:   
  case EXEC_RETURN: case EXEC_STOP:     case EXEC_EXIT:     case EXEC_PAUSE:    
  case EXEC_WHERE:  case EXEC_IOLENGTH: case EXEC_CONTINUE: case EXEC_ENTRY:          
  case EXEC_DO_WHILE: case EXEC_ARITHMETIC_IF: case EXEC_POINTER_ASSIGN:          
  case EXEC_TRANSFER:    
    break;        
        
  case EXEC_CALL:          
    g95_free_actual_arglist(b->ext.actual);        
    break;        
        
  case EXEC_SELECT:     
    if (b->ext.case_list) g95_free_case_list(b->ext.case_list);        
    break;          
          
  case EXEC_DO:      
    g95_free_iterator(b->ext.iterator, 1);         
    break;    
    
  case EXEC_ALLOCATE:       
  case EXEC_DEALLOCATE:   
    g95_free_alloc_list(b->ext.alloc_list);    
    break;         
         
  case EXEC_OPEN:  
    g95_free_open(b->ext.open);        
    break;

  case EXEC_CLOSE: 
    g95_free_close(b->ext.close);
    break;   
   
  case EXEC_BACKSPACE:     
  case EXEC_ENDFILE:    
  case EXEC_REWIND:        
    g95_free_filepos(b->ext.filepos);          
    break;      
      
  case EXEC_IF:          
    g95_free_statements(b->ext.block); 
    break;

  case EXEC_INQUIRE:    
    g95_free_inquire(b->ext.inquire);      
    break;     
     
  case EXEC_READ:    
  case EXEC_WRITE:        
    g95_free_dt(b->ext.dt);    
    break;       
       
  case EXEC_DT_END:         
    /* The ext.dt member is a duplicate pointer and doesn't need to be freed */  
    break;

  case EXEC_FORALL:        
    g95_free_forall_iterator(b->ext.forall_iterator);       
    break;    
    
  default: 
    g95_internal_error("free_statement(): Bad statement");       
  }
}     
     
     
 
 
/* g95_free_statements()-- Free a code statement and all other code
 * structures linked to it. */        
        
void g95_free_statements(g95_code *u) {      
g95_code *k;     
     
  for(; u; u=k) {  
    k = u->next;         
         
    if (u->block) g95_free_statements(u->block);    
    free_statement(u);    
    g95_free(u);        
  }         
}


     
     
/* code_indent()-- Do indentation for a specific level */ 
 
static void code_indent(int l, g95_st_label *labl) {     
int q;    
    
  if (labl != NULL) 
    g95_status("%-6d ", labl->value);       
  else 
    g95_status("       ");   
   
  for(q=0; q<2*l; q++)      
    g95_status_char(' ');   
}          
          
          
       
       
/* g95_show_code_node()-- Show a single code node and everything
 * underneath it if necessary. */   
   
static void g95_show_code_node(int lev, g95_code *g) {
g95_forall_iterator *fa;  
char *m, *name0;        
g95_open *open;       
g95_case *cp;        
g95_alloc *w;        
g95_code *t;   
g95_close *close; 
g95_filepos *fp2;        
g95_inquire *h;         
g95_dt *dt;        
        
  code_indent(lev, g->here);     
    
  switch(g->type) {          
  case EXEC_NOP:   
    g95_status("NOP");          
    break;

  case EXEC_CONTINUE:         
    g95_status("CONTINUE");      
    break;

  case EXEC_ASSIGN:   
    g95_status("ASSIGN ");    
    g95_show_expr(g->expr);          
    g95_status_char(' ');     
    g95_show_expr(g->expr2);        
    break; 
 
  case EXEC_POINTER_ASSIGN:  
    g95_status("POINTER ASSIGN ");       
    g95_show_expr(g->expr);       
    g95_status_char(' ');
    g95_show_expr(g->expr2);          
    break;

  case EXEC_GOTO: 
    g95_status("GOTO %d", g->label->value);         
    break;   
   
  case EXEC_CALL:   
    if (g->sym == NULL)         
      m = name0 = "";          
    else {         
      m = g->sym->module;     
      name0   = g->sym->name;     
    }  
  
    g95_status("CALL (%s:%s) %s ", m, name0, g->sub_name);    
    g95_show_actual_arglist(g->ext.actual);  
    break;          
          
  case EXEC_RETURN:     
    g95_status("RETURN ");  
    if (g->expr) g95_show_expr(g->expr);       
    break;          
          
  case EXEC_STOP:        
    g95_status("STOP ");         
         
    if (g->expr != NULL)    
      g95_show_expr(g->expr);        
    else          
      g95_status("%d", g->ext.stop_code);         
         
    break;          
          
  case EXEC_PAUSE:        
    g95_status("PAUSE ");       
    if (g->expr != NULL) g95_show_expr(g->expr);        
    break; 
 
  case EXEC_ARITHMETIC_IF: 
    g95_status("IF ");
    g95_show_expr(g->expr);
    g95_status(" %d, %d, %d",  
	       g->label->value, g->label2->value, g->label3->value);     
    break;    
    
  case EXEC_IF:      
    g95_status("IF ");        
    g95_show_expr(g->expr);       
    g95_status_char('\n');  
    g95_show_code(lev+1, g->block);        
        
    if (g->ext.block != NULL) {        
      code_indent(lev, 0);    
      g95_status("ELSE\n");       
      g95_show_code(lev+1, g->ext.block);       
    }       
       
    code_indent(lev, g->label);      
    g95_status("ENDIF");
    break; 
 
  case EXEC_SELECT:  
    t = g->block;      
    g95_status("SELECT CASE ");
    g95_show_expr((g->expr != NULL) ? g->expr : g->expr2);        
    g95_status_char('\n');    
    
    for(;t ;t=t->block) {   
      code_indent(lev, 0);          
          
      g95_status("CASE ");        
      for(cp=t->ext.case_list; cp; cp=cp->next) {     
	g95_status_char('(');   
	g95_show_expr(cp->low);        
	g95_status_char(' ');     
	g95_show_expr(cp->high);       
	g95_status_char(')');        
	g95_status_char(' ');       
      }      
      g95_status_char('\n');   
   
      g95_show_code(lev+1, t->next);          
    }   
   
    code_indent(lev, g->label);  
    g95_status("END SELECT");
    break; 
 
  case EXEC_WHERE: 
    g95_status("WHERE ");        
        
    t = g->block;   
    g95_show_expr(t->expr);      
    g95_status_char('\n');      
      
    g95_show_code(lev+1, t->next);       
       
    for(t=t->block; t; t=t->block) {       
      code_indent(lev, 0);       
      g95_status("ELSE WHERE ");
      g95_show_expr(t->expr);        
      g95_status_char('\n');         
      g95_show_code(lev+1, t->next);       
    }  
  
    code_indent(lev, 0);         
    g95_status("END WHERE");          
    break;      
      
      
  case EXEC_FORALL:          
    g95_status("FORALL ");    
    for(fa=g->ext.forall_iterator; fa; fa=fa->next) {  
      g95_show_expr(fa->var);    
      g95_status_char(' ');       
      g95_show_expr(fa->start); 
      g95_status_char(':');     
      g95_show_expr(fa->end);   
      g95_status_char(':');          
      g95_show_expr(fa->stride);

      if (fa->next != NULL) g95_status_char(',');         
    }          
          
    if (g->expr != NULL) {
      g95_status_char(',');    
      g95_show_expr(g->expr);     
    }   
    g95_status_char('\n'); 
 
    g95_show_code(lev+1, g->block);    
    
    code_indent(lev, 0);         
    g95_status("END FORALL");        
    break;         
             
  case EXEC_DO:         
    g95_status("DO ");    
    
    g95_show_expr(g->ext.iterator->var);  
    g95_status_char('=');
    g95_show_expr(g->ext.iterator->start);
    g95_status_char(' ');     
    g95_show_expr(g->ext.iterator->end);      
    g95_status_char(' '); 
    g95_show_expr(g->ext.iterator->step);       
    g95_status_char('\n');    
    
    g95_show_code(lev+1, g->block);       
       
    code_indent(lev, 0);    
    g95_status("END DO");
    break; 
 
  case EXEC_DO_WHILE:     
    g95_status("DO WHILE ");
    g95_show_expr(g->expr);    
    g95_status_char('\n');     
     
    g95_show_code(lev+1, g->block);      
      
    code_indent(lev, g->label);         
    g95_status("END DO");  
    break;      
      
  case EXEC_CYCLE:       
    g95_status("CYCLE");  
    if (g->sym) g95_status(" %s", g->sym->name);  
    break; 
 
  case EXEC_EXIT: 
    g95_status("EXIT");      
    if (g->sym) g95_status(" %s", g->sym->name);  
    break;   
   
  case EXEC_ALLOCATE:         
    g95_status("ALLOCATE ");   
    if (g->expr) {
      g95_status(" STAT=");       
      g95_show_expr(g->expr);   
    }    
    
    for(w=g->ext.alloc_list; w; w=w->next) {         
      g95_status_char(' ');      
      g95_show_expr(w->expr);          
    }          
          
    break;          
          
  case EXEC_DEALLOCATE: 
    g95_status("DEALLOCATE ");       
    if (g->expr) {   
      g95_status(" STAT=");    
      g95_show_expr(g->expr);  
    }   
   
    for(w=g->ext.alloc_list; w; w=w->next) {     
      g95_status_char(' ');        
      g95_show_expr(w->expr);   
    }    
    
    break;  
  
  case EXEC_OPEN:     
    g95_status("OPEN");  
    open = g->ext.open;          
          
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
    close = g->ext.close;  
  
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
    fp2 = g->ext.filepos;        
        
    if (fp2->unit) { g95_status(" UNIT="); g95_show_expr(fp2->unit); } 
    if (fp2->iostat) { g95_status(" IOSTAT="); g95_show_expr(fp2->iostat); }          
    if (fp2->err != NULL) g95_status(" ERR=%d", fp2->err->value);          
    break;   
   
  case EXEC_INQUIRE:       
    g95_status("INQUIRE");      
    h = g->ext.inquire;   
   
    if (h->unit) { g95_status(" UNIT="); g95_show_expr(h->unit); } 
    if (h->file) { g95_status(" FILE="); g95_show_expr(h->file); }      
      
    if (h->iostat) { g95_status(" IOSTAT="); g95_show_expr(h->iostat); }          
    if (h->exist) { g95_status(" EXIST="); g95_show_expr(h->exist); }      
    if (h->opened) { g95_status(" OPENED="); g95_show_expr(h->opened); }          
    if (h->number) { g95_status(" NUMBER="); g95_show_expr(h->number); }  
    if (h->named) { g95_status(" NAMED="); g95_show_expr(h->named); }       
    if (h->name) { g95_status(" NAME="); g95_show_expr(h->name); } 
    if (h->access) { g95_status(" ACCESS="); g95_show_expr(h->access); }     
    if (h->sequential) { g95_status(" SEQUENTIAL=");
      g95_show_expr(h->sequential); }       
       
    if (h->direct) { g95_status(" DIRECT="); g95_show_expr(h->direct); }     
    if (h->form) { g95_status(" FORM="); g95_show_expr(h->form); }        
    if (h->formatted) { g95_status(" FORMATTED"); g95_show_expr(h->formatted);}         
    if (h->unformatted) { g95_status(" UNFORMATTED=");       
      g95_show_expr(h->unformatted); }
    if (h->recl) { g95_status(" RECL="); g95_show_expr(h->recl); }    
    if (h->nextrec) { g95_status(" NEXTREC="); g95_show_expr(h->nextrec); } 
    if (h->blank) { g95_status(" BLANK="); g95_show_expr(h->blank); } 
    if (h->position) { g95_status(" POSITION="); g95_show_expr(h->position); }       
    if (h->action) { g95_status(" ACTION="); g95_show_expr(h->action); }
    if (h->read) { g95_status(" READ="); g95_show_expr(h->read); }
    if (h->write) { g95_status(" WRITE="); g95_show_expr(h->write); }  
    if (h->readwrite) {g95_status(" READWRITE="); g95_show_expr(h->readwrite);} 
    if (h->delim) { g95_status(" DELIM="); g95_show_expr(h->delim); }     
    if (h->pad) { g95_status(" PAD="); g95_show_expr(h->pad); } 
 
    if (h->err != NULL) g95_status(" ERR=%d", h->err->value);     
    break;        
        
  case EXEC_IOLENGTH:        
    g95_status("IOLENGTH ");    
    g95_show_expr(g->expr);       
    break;     
     
  case EXEC_READ:       
    g95_status("READ");   
    goto show_dt;   
   
  case EXEC_WRITE:     
    g95_status("WRITE");      
      
  show_dt:          
    dt = g->ext.dt;       
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
    g95_show_expr(g->expr); 
    break; 
 
  case EXEC_DT_END:
    g95_status("DT_END");      
      
    dt = g->ext.dt;   
    if (dt != NULL) {     
      if (dt->err != NULL) g95_status(" ERR=%d", dt->err->value); 
      if (dt->end != NULL) g95_status(" END=%d", dt->end->value);    
      if (dt->eor != NULL) g95_status(" EOR=%d", dt->eor->value);  
    }    
    
    break;          
          
  case EXEC_ENTRY:        
    g95_status("ENTRY %s ", g->sym->name);        
    break;        
        
  default:        
    g95_internal_error("g95_show_code_node(): Bad statement code");         
  }    
    
  g95_status_char('\n');   
}  
  
  


/* g95_undo_statement()-- Frees everything associated with the current
 * statement.  */          
          
void g95_undo_statement(void) {

  g95_free_statements(new_st.block); 
  g95_free_statements(new_st.next);       
       
  free_statement(&new_st);   
  g95_clear_new_st();     
}     
     
     
     
     
/* g95_show_code()-- Show a list of code structures. */          
          
void g95_show_code(int lvl, g95_code *c) {  
  
  for(;c ; c=c->next)        
    g95_show_code_node(lvl, c);     
}         
         
