/* Build executable statement trees
   Copyright (C) 2000, 2001, 2002 Free Software Foundation, Inc.
   Contributed by Andy Vaught

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

/* st.c-- Executable statements are strung together into a singly
 * linked list of code structures.  These structures are later
 * translated into GBE tree structures and from there to executable
 * code for a target.  */

#include "g95.h"
#include <string.h>

g95_code new_st;

/* g95_clear_new_st()-- Zeroes out the new_st structure. */

void g95_clear_new_st(void) {

  memset(&new_st, '\0', sizeof(new_st));
  new_st.op = EXEC_NOP;
}


/* g95_get_code()-- Get a g95_code structure */

g95_code *g95_get_code(void) {
g95_code *c;

  c = g95_getmem(sizeof(g95_code)); 
  c->loc = *g95_current_locus();
  return c;
}


/* g95_add_statement()-- Takes the current new_st code structure and
 * adds it to the current program unit.  As a side-effect, it zeroes
 * the new_st. */

g95_code *g95_add_statement(void) {
g95_code *p;

  p = g95_get_code();
  *p = new_st;

  p->loc = *g95_current_locus();

  if (g95_state_stack->head == NULL)
    g95_state_stack->head = p;
  else
    g95_state_stack->tail->next = p;

  while(p->next != NULL)
    p = p->next;

  g95_state_stack->tail = p;

  g95_clear_new_st();

  return p;
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


/* g95_new_level()-- Starts a new level in the statement list. */

g95_code *g95_new_level(g95_code *q) {
g95_code *p;

  p = q->block = g95_get_code();

  g95_state_stack->head = g95_state_stack->tail = p;

  return p;
}


/* free_statement()-- Free a single code structure, but not the actual
 * structure itself. */

static void free_statement(g95_code *p) {

  if (p->expr) g95_free_expr(p->expr);
  if (p->expr2) g95_free_expr(p->expr2);

  switch(p->op) {
  case EXEC_NOP:      case EXEC_ASSIGN:     case EXEC_GOTO:  case EXEC_CYCLE:
  case EXEC_RETURN:   case EXEC_IF:         case EXEC_STOP:  case EXEC_EXIT: 
  case EXEC_WHERE:    case EXEC_IOLENGTH:   case EXEC_POINTER_ASSIGN:
  case EXEC_DO_WHILE: case EXEC_CONTINUE:   case EXEC_TRANSFER:
  case EXEC_ARITHMETIC_IF:   
    break;

  case EXEC_CALL:
    g95_free_actual_arglist(p->ext.actual);
    break;

  case EXEC_SELECT:
    if (p->ext.case_list) g95_free_case_list(p->ext.case_list);
    break;

  case EXEC_DO:
    g95_free_iterator(p->ext.iterator, 1);
    break;

  case EXEC_ALLOCATE:
  case EXEC_DEALLOCATE:
    g95_free_alloc_list(p->ext.alloc_list);
    break;

  case EXEC_OPEN:
    g95_free_open(p->ext.open);
    break;

  case EXEC_CLOSE:
    g95_free_close(p->ext.close);
    break;

  case EXEC_BACKSPACE:
  case EXEC_ENDFILE:
  case EXEC_REWIND:
    g95_free_filepos(p->ext.filepos);
    break;

  case EXEC_INQUIRE:
    g95_free_inquire(p->ext.inquire);
    break;

  case EXEC_READ:
  case EXEC_WRITE:
    g95_free_dt(p->ext.dt);
    break;

  case EXEC_DT_END:
    /* The ext.dt member is a duplicate pointer and doesn't need to be freed */
    break;

  case EXEC_FORALL:
    g95_free_forall_iterator(p->ext.forall_iterator);
    break;

  default:
    g95_internal_error("free_statement(): Bad statement");
  }
}


/* g95_free_statements()-- Free a code statement and all other code
 * structures linked to it. */

void g95_free_statements(g95_code *p) {
g95_code *q;

  for(; p; p=q) {
    q = p->next;

    if (p->block) g95_free_statements(p->block);
    free_statement(p);
    g95_free(p);
  }
}


/* g95_undo_statement()-- Frees everything associated with the current
 * statement.  */

void g95_undo_statement(void) {

  g95_free_statements(new_st.block);
  g95_free_statements(new_st.next);

  free_statement(&new_st);
  g95_clear_new_st();
}



#ifdef G95_DEBUG

/* code_indent()-- Do indentation for a specific level */

static void code_indent(int level, g95_st_label *label) {
int i;

  if (label != NULL)
    g95_status("%-5d ", label->value);
  else
    g95_status("      ");

  for(i=0; i<2*level; i++)
    g95_status_char(' ');
}


/* g95_show_code_node()-- Show a single code node and everything
 * underneath it if necessary. */

static void g95_show_code_node(int level, g95_code *c) {
g95_forall_iterator *fa;
g95_open *open;
g95_case *cp;
g95_alloc *a;
g95_code *d;
g95_close *close;
g95_filepos *fp;
g95_inquire *i;
g95_dt *dt;

  code_indent(level, c->here); 

  switch(c->op) {
  case EXEC_NOP:
    g95_status("NOP");
    break;

  case EXEC_CONTINUE:
    g95_status("CONTINUE");
    break;

  case EXEC_ASSIGN:
    g95_status("ASSIGN ");
    g95_show_expr(c->expr);
    g95_status_char(' ');
    g95_show_expr(c->expr2);
    break;

  case EXEC_POINTER_ASSIGN:
    g95_status("POINTER ASSIGN ");
    g95_show_expr(c->expr);
    g95_status_char(' ');
    g95_show_expr(c->expr2);
    break;

  case EXEC_GOTO:
    g95_status("GOTO %d", c->label->value);
    break;

  case EXEC_CALL:
    g95_status("CALL %s ", c->sub_name);
    g95_show_actual_arglist(c->ext.actual);
    break;

  case EXEC_RETURN:
    g95_status("RETURN ");
    if (c->expr) g95_show_expr(c->expr);
    break;

  case EXEC_STOP:
    g95_status("STOP ");

    if (c->expr != NULL)
      g95_show_expr(c->expr);
    else
      g95_status("%d", c->ext.stop_code);

    break;

  case EXEC_ARITHMETIC_IF:
    g95_status("IF ");
    g95_show_expr(c->expr);
    g95_status(" %d, %d, %d",
	       c->label->value, c->label2->value, c->label3->value);
    break;

  case EXEC_IF:
    d = c->block;
    g95_status("IF ");
    g95_show_expr(d->expr);
    g95_status_char('\n');
    g95_show_code(level+1, d->next);

    d = d->block;
    for(; d; d=d->block) {
      code_indent(level, 0);

      if (d->expr == NULL) g95_status("ELSE\n");
      else {
	g95_status("ELSE IF ");
	g95_show_expr(d->expr);
	g95_status_char('\n');
      }

      g95_show_code(level+1, d->next);
    }

    code_indent(level, c->label);

    g95_status("ENDIF");
    break;

  case EXEC_SELECT:
    d = c->block;
    g95_status("SELECT CASE ");
    g95_show_expr((c->expr != NULL) ? c->expr : c->expr2);
    g95_status_char('\n');

    for(;d ;d=d->block) {
      code_indent(level, 0);

      g95_status("CASE ");
      for(cp=d->ext.case_list; cp; cp=cp->next) {
	g95_status_char('(');
	g95_show_expr(cp->low);
	g95_status_char(' ');
	g95_show_expr(cp->high);
	g95_status_char(')');
	g95_status_char(' ');
      }
      g95_status_char('\n');

      g95_show_code(level+1, d->next);
    }

    code_indent(level, c->label);
    g95_status("END SELECT");
    break;

  case EXEC_WHERE:
    g95_status("WHERE ");

    d = c->block;
    g95_show_expr(d->expr);
    g95_status_char('\n');

    g95_show_code(level+1, d->next);

    for(d=d->block; d; d=d->block) {
      code_indent(level, 0);
      g95_status("ELSE WHERE ");
      g95_show_expr(d->expr);
      g95_status_char('\n');
      g95_show_code(level+1, d->next);
    }

    code_indent(level, 0);
    g95_status("END WHERE");
    break;


  case EXEC_FORALL:
    g95_status("FORALL ");
    for(fa=c->ext.forall_iterator; fa; fa=fa->next) {
      g95_show_expr(fa->var);
      g95_status_char(' ');
      g95_show_expr(fa->start);
      g95_status_char(':');
      g95_show_expr(fa->end);
      g95_status_char(':');
      g95_show_expr(fa->stride);

      if (fa->next != NULL) g95_status_char(',');
    }

    if (c->expr != NULL) {
      g95_status_char(',');
      g95_show_expr(c->expr);
    }
    g95_status_char('\n');

    g95_show_code(level+1, c->block->next);

    code_indent(level, 0);
    g95_status("END FORALL");
    break;
    
  case EXEC_DO:
    g95_status("DO ");

    g95_show_expr(c->ext.iterator->var);
    g95_status_char('=');
    g95_show_expr(c->ext.iterator->start);
    g95_status_char(' ');
    g95_show_expr(c->ext.iterator->end);
    g95_status_char(' ');
    g95_show_expr(c->ext.iterator->step);
    g95_status_char('\n');

    g95_show_code(level+1, c->block->next);

    code_indent(level, 0);
    g95_status("END DO");
    break;

  case EXEC_DO_WHILE:
    g95_status("DO WHILE ");
    g95_show_expr(c->expr);
    g95_status_char('\n');

    g95_show_code(level+1, c->block->next);

    code_indent(level, c->label);
    g95_status("END DO");
    break;

  case EXEC_CYCLE:
    g95_status("CYCLE");
    if (c->sym) g95_status(" %s", c->sym->name);
    break;

  case EXEC_EXIT:
    g95_status("EXIT");
    if (c->sym) g95_status(" %s", c->sym->name);
    break;

  case EXEC_ALLOCATE:
    g95_status("ALLOCATE ");
    if (c->expr) {
      g95_status(" STAT=");
      g95_show_expr(c->expr);
    }

    for(a=c->ext.alloc_list; a; a=a->next) {
      g95_status_char(' ');
      g95_show_expr(a->expr);
    }

    break;

  case EXEC_DEALLOCATE:
    g95_status("DEALLOCATE ");
    if (c->expr) {
      g95_status(" STAT=");
      g95_show_expr(c->expr);
    }

    for(a=c->ext.alloc_list; a; a=a->next) {
      g95_status_char(' ');
      g95_show_expr(a->expr);
    }

    break;

  case EXEC_OPEN:
    g95_status("OPEN");
    open = c->ext.open;

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
    close = c->ext.close;

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
    fp = c->ext.filepos;

    if (fp->unit) { g95_status(" UNIT="); g95_show_expr(fp->unit); }
    if (fp->iostat) { g95_status(" IOSTAT="); g95_show_expr(fp->iostat); }
    if (fp->err != NULL) g95_status(" ERR=%d", fp->err->value);
    break;

  case EXEC_INQUIRE:
    g95_status("INQUIRE");
    i = c->ext.inquire;

    if (i->unit) { g95_status(" UNIT="); g95_show_expr(i->unit); }
    if (i->file) { g95_status(" FILE="); g95_show_expr(i->file); }

    if (i->iostat) { g95_status(" IOSTAT="); g95_show_expr(i->iostat); }
    if (i->exist) { g95_status(" EXIST="); g95_show_expr(i->exist); }
    if (i->opened) { g95_status(" OPENED="); g95_show_expr(i->opened); }
    if (i->number) { g95_status(" NUMBER="); g95_show_expr(i->number); }
    if (i->named) { g95_status(" NAMED="); g95_show_expr(i->named); }
    if (i->name) { g95_status(" NAME="); g95_show_expr(i->name); }
    if (i->access) { g95_status(" ACCESS="); g95_show_expr(i->access); }
    if (i->sequential) { g95_status(" SEQUENTIAL=");
      g95_show_expr(i->sequential); }

    if (i->direct) { g95_status(" DIRECT="); g95_show_expr(i->direct); }
    if (i->form) { g95_status(" FORM="); g95_show_expr(i->form); }
    if (i->formatted) { g95_status(" FORMATTED"); g95_show_expr(i->formatted);}
    if (i->unformatted) { g95_status(" UNFORMATTED=");
      g95_show_expr(i->unformatted); }
    if (i->recl) { g95_status(" RECL="); g95_show_expr(i->recl); }
    if (i->nextrec) { g95_status(" NEXTREC="); g95_show_expr(i->nextrec); }
    if (i->blank) { g95_status(" BLANK="); g95_show_expr(i->blank); }
    if (i->position) { g95_status(" POSITION="); g95_show_expr(i->position); }
    if (i->action) { g95_status(" ACTION="); g95_show_expr(i->action); }
    if (i->read) { g95_status(" READ="); g95_show_expr(i->read); }
    if (i->write) { g95_status(" WRITE="); g95_show_expr(i->write); }
    if (i->readwrite) {g95_status(" READWRITE="); g95_show_expr(i->readwrite);}
    if (i->delim) { g95_status(" DELIM="); g95_show_expr(i->delim); }
    if (i->pad) { g95_status(" PAD="); g95_show_expr(i->pad); }

    if (i->err != NULL) g95_status(" ERR=%d", i->err->value);
    break;

  case EXEC_IOLENGTH:
    g95_status("IOLENGTH ");
    g95_show_expr(c->expr);
    break;

  case EXEC_READ:
    g95_status("READ");
    goto show_dt;

  case EXEC_WRITE:
    g95_status("WRITE");

  show_dt:
    dt = c->ext.dt;
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
    g95_show_expr(c->expr);
    break;

  case EXEC_DT_END:
    g95_status("DT_END");
    dt = c->ext.dt;

    if (dt->err != NULL) g95_status(" ERR=%d", dt->err->value);
    if (dt->end != NULL) g95_status(" END=%d", dt->end->value);
    if (dt->eor != NULL) g95_status(" EOR=%d", dt->eor->value);
    break;

  default:    
    g95_internal_error("g95_show_code_node(): Bad statement code");
  }

  g95_status_char('\n');
}


/* g95_show_code()-- Show a list of code structures. */

void g95_show_code(int level, g95_code *c) {

  for(;c ; c=c->next)
    g95_show_code_node(level, c);
}

#endif
