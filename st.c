/* Build executable statement trees
   Copyright (C) 2000 Free Software Foundation, Inc.
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


/* g95_add_statement()-- Takes the current new_st code structure and
 * adds it to the current program unit.  As a side-effect, it zeroes
 * the new_st. */

g95_code *g95_add_statement(void) {
g95_code *p;

  p = g95_getmem(sizeof(g95_code));
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
  case EXEC_DO_WHILE: case EXEC_ARITHMETIC_IF:   
    break;

  case EXEC_CALL:
    g95_free_actual_arglist(p->ext);
    break;

  case EXEC_SELECT:
    if (p->ext) g95_free_case_list(p->ext);
    break;

  case EXEC_DO:
    g95_free_iterator(p->ext, 1);
    break;

  case EXEC_ALLOCATE:
  case EXEC_DEALLOCATE:
  case EXEC_NULLIFY:
    g95_free_alloc_list(p->ext);
    break;

  case EXEC_OPEN:
    g95_free_open(p->ext);
    break;

  case EXEC_CLOSE:
    g95_free_close(p->ext);
    break;

  case EXEC_BACKSPACE:
  case EXEC_ENDFILE:
  case EXEC_REWIND:
    g95_free_filepos(p->ext);
    break;

  case EXEC_INQUIRE:
    g95_free_inquire(p->ext);
    break;

  case EXEC_READ:
  case EXEC_WRITE:
    g95_free_dt(p->ext);
    break;

  case EXEC_FORALL:
    g95_free_forall_iterator(p->ext);
    break;

  default:
    g95_internal_error("free_statement(): Bad statement");
  }
}


/* g95_free_statements()-- Free a code statement and all other code
 * structures linked to it. */

void g95_free_statements(g95_code *p) {
g95_code *q;

  if (p == NULL) return; 
  if (p->block) g95_free_statements(p->block);

  for(; p; p=q) {
    q = p->next;
    free_statement(p);
    g95_free(p);
  }
}


/* g95_undo_statement()-- Frees everything associated with the current
 * statement.  */

void g95_undo_statement(void) {

  g95_free_statements(new_st.block);

  free_statement(&new_st);
  g95_clear_new_st();
}


/******************** Name Resolution **********************/


static void resolve_call(g95_symbol *sym, g95_actual_arglist **arg) {

}


/* g95_resolve_iterator()-- Resolve the expressions in an iterator
 * structure and require that they all be of integer type. */

try g95_resolve_iterator(g95_iterator *iter) {
try t;

  t = SUCCESS; 

  if (g95_resolve_expr(iter->var) == SUCCESS &&
      iter->var->ts.type != BT_INTEGER) {
    g95_error("Loop variable at %L must be INTEGER in Fortran 95",
	      &iter->var->where);
    t = FAILURE;
  }

  if (g95_resolve_expr(iter->start) == SUCCESS &&
      iter->start->ts.type != BT_INTEGER) {
    g95_error("Start expression in DO loop at %L must be INTEGER in Fortran 95",
	      &iter->start->where);
    t = FAILURE;
  }

  if (g95_resolve_expr(iter->end) == SUCCESS &&
      iter->end->ts.type != BT_INTEGER) {
    g95_error("End expression in DO loop at %L must be INTEGER in Fortran 95",
	      &iter->end->where);
    t = FAILURE;
  }

  if (g95_resolve_expr(iter->step) == SUCCESS &&
      iter->step->ts.type != BT_INTEGER) {
    g95_error("Step expression in DO loop at %L must be INTEGER in Fortran 95",
	      &iter->step->where);
    t = FAILURE;
  }

  return t;
}


/* resolve_forall_iterators()-- Resolve a list of FORALL iterators */

static void resolve_forall_iterators(g95_forall_iterator *iter) {

  while(iter) {
    if (g95_resolve_expr(iter->var) == SUCCESS &&
	iter->var->ts.type != BT_INTEGER)
      g95_error("FORALL Iteration variable at %L must be INTEGER",
		&iter->var->where);

    if (g95_resolve_expr(iter->start) == SUCCESS &&
	iter->start->ts.type != BT_INTEGER)
      g95_error("FORALL start expression at %L must be INTEGER",
		&iter->start->where);

    if (g95_resolve_expr(iter->end) == SUCCESS &&
	iter->end->ts.type != BT_INTEGER)
      g95_error("FORALL end expression at %L must be INTEGER",
		&iter->end->where);

    if (g95_resolve_expr(iter->stride) == SUCCESS &&
	iter->stride->ts.type != BT_INTEGER)
      g95_error("FORALL Stride expression at %L must be INTEGER",
		&iter->stride->where);

    iter = iter->next;
  }
}


/* g95_resolve_code()-- Given a block of code, recursively resolve
 * everything pointed to by this code block */

void g95_resolve_code(g95_code *code) {
g95_alloc *a;

  for(; code; code=code->next) {
    if (code->op != EXEC_SELECT && code->block != NULL)
      g95_resolve_code(code->block);

    g95_resolve_expr(code->expr);
    g95_resolve_expr(code->expr2);

    switch(code->op) {
    case EXEC_NOP:  case EXEC_GOTO:    case EXEC_CYCLE:  case EXEC_IOLENGTH:
    case EXEC_STOP: case EXEC_NULLIFY: case EXEC_EXIT:
      break;

    case EXEC_RETURN:
      if (code->expr != NULL && code->expr->ts.type != BT_INTEGER)
	g95_error("Alternate RETURN statement at %L requires an INTEGER "
		  "return specifier", &code->expr->where);
      break;

    case EXEC_ASSIGN: 
      g95_check_assign(code->expr, code->expr2);
      break;

    case EXEC_POINTER_ASSIGN:
      g95_check_pointer_assign(code->expr, code->expr2);
      break;

    case EXEC_ARITHMETIC_IF:
      if (code->expr->ts.type != BT_INTEGER && code->expr->ts.type != BT_REAL)
	g95_error("Arithmetic IF statement at %L requires a numeric "
		  "expression", &code->expr->where);
      break;

    case EXEC_IF:
      if (code->expr != NULL && code->expr->ts.type != BT_LOGICAL)
	g95_error("IF/ELSE IF clause at %L requires a LOGICAL expression",
		  &code->expr->where);
      break;

    case EXEC_WHERE:
      if (code->expr != NULL && code->expr->ts.type != BT_LOGICAL)
	g95_error("WHERE/ELSEWHERE clause at %L requires a LOGICAL expression",
		  &code->expr->where);
      break;

    case EXEC_CALL:
      resolve_call(code->sym, (g95_actual_arglist **) &code->ext);
      break;

    case EXEC_SELECT:
      g95_resolve_select(code);      /* Select is complicated */
      break;

    case EXEC_DO:
      g95_reference_st_label(code->label, ST_LABEL_TARGET);

      if (code->ext != NULL) g95_resolve_iterator(code->ext);
      break;

    case EXEC_DO_WHILE:
      if (code->expr != NULL && code->expr->ts.type != BT_LOGICAL)
	g95_error("Argument of DO WHILE loop at %L must be of type LOGICAL",
		  &code->expr->where);
      break;

    case EXEC_ALLOCATE:
      if (code->expr != NULL && code->expr->ts.type != BT_INTEGER)
	g95_error("STAT tag in ALLOCATE statement at %L must be "
		  "of type INTEGER", &code->expr->where);

      for(a=code->ext; a; a=a->next)
	g95_resolve_expr(a->expr);
      break;

    case EXEC_DEALLOCATE:
      if (code->expr != NULL && code->expr->ts.type != BT_INTEGER)
	g95_error("STAT tag in DEALLOCATE statement at %L must be of type "
		  "INTEGER", &code->expr->where);

      for(a=code->ext; a; a=a->next)
	g95_resolve_expr(a->expr);
      break;

    case EXEC_OPEN:
      g95_resolve_open(code->ext);
      break;

    case EXEC_CLOSE:
      g95_resolve_close(code->ext);
      break;

    case EXEC_BACKSPACE:
    case EXEC_ENDFILE:
    case EXEC_REWIND:
      g95_resolve_filepos(code->ext);
      break;

    case EXEC_INQUIRE:
      g95_resolve_inquire(code->ext);
      break;

    case EXEC_READ:
    case EXEC_WRITE:
      g95_resolve_dt(code->ext);
      break;

    case EXEC_FORALL:
      resolve_forall_iterators(code->ext);
      if (code->expr != NULL && code->expr->ts.type != BT_LOGICAL)
	g95_error("FORALL mask clause at %L requires a LOGICAL expression",
		  &code->expr->where);
      break;

    default:    
      g95_internal_error("g95_resolve_code(): Bad statement code");
    }
  }
}


/* resolve_symbol()-- Do anything necessary to resolve a symbol.
 * Right now, we just assume that an otherwise unknown symbol is a
 * variable.  This sort of thing commonly happens for symbols in module. */

static void resolve_symbol(g95_symbol *sym) {

  if (sym->attr.flavor == FL_UNKNOWN &&
      sym->attr.external == 0 && sym->attr.intrinsic == 0)
    sym->attr.flavor = FL_VARIABLE;

  if (sym->as != NULL && sym->as->type == AS_ASSUMED_SIZE &&
      sym->attr.dummy == 0) {
    g95_error("Assumed size array at %L must be a dummy argument",
	      &sym->declared_at);
    return;
  }

  /* Make sure the types of derived parameters are consistent.  This
   * type checking is deferred until resolution because the type may
   * refer to a derived type from the host. */

  if (sym->attr.flavor == FL_PARAMETER && sym->ts.type == BT_DERIVED &&
      !g95_compare_types(&sym->ts, &sym->value->ts))
    g95_error("Incompatible derived type in PARAMETER at %L",
	      &sym->value->where);
}


/* g95_resolve()-- This function is called after a complete
 * program unit has been compiled.  Its purpose is to examine all of
 * the expressions associated with the program unit, assign types to
 * all intermediate expressions, make sure that all assignments are to
 * compatible types and figure out which names refer to which functions
 * or subroutines. */

void g95_resolve(g95_namespace *ns) {
g95_charlen *cl;

  if (ns->save_all) g95_save_all(ns);

  g95_set_sym_defaults(ns);

  g95_resolve_code(ns->code);

  for(cl=ns->cl_list; cl; cl=cl->next) {
    if (cl->length == NULL || g95_resolve_expr(cl->length) == FAILURE)
      continue;

    if (cl->length->ts.type != BT_INTEGER)
      g95_error("Character length specification at %L must be of type INTEGER",
		&cl->length->where);
  }

  g95_check_st_labels(ns);

  g95_traverse_ns(ns, resolve_symbol);
}


#ifdef G95_DEBUG

/* code_indent()-- Do indentation for a specific level */

static void code_indent(int level, int label) {
int i;

  if (label != 0)
    g95_status("%-5d ", label);
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
    g95_status("GOTO %d", c->label);
    break;

  case EXEC_CALL:
    g95_status("CALL %s ", c->sym->name);
    g95_show_actual_arglist(c->ext);
    break;

  case EXEC_RETURN:
    g95_status("RETURN ");
    if (c->expr) g95_show_expr(c->expr);
    break;

  case EXEC_STOP:
    g95_status("STOP ");

    if (c->label != -1) {
      if (c->expr != NULL)
	g95_show_expr(c->expr);
      else
	g95_status("%d", c->label);
    }

    break;

  case EXEC_ARITHMETIC_IF:
    g95_status("IF ");
    g95_show_expr(c->expr);
    g95_status(" %d, %d, %d", c->label, c->label2, c->label3);
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

    code_indent(level, 0);

    g95_status("ENDIF");
    break;

  case EXEC_SELECT:
    d = c->block;
    g95_status("SELECT CASE ");
    g95_show_expr(c->expr);
    g95_status_char('\n');

    for(;d ;d=d->block) {
      code_indent(level, 0);

      g95_status("CASE ");
      for(cp=d->ext; cp; cp=cp->next) {
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

    code_indent(level, 0);
    g95_status("END SELECT");
    break;

  case EXEC_WHERE:
    g95_status("WHERE ");

    if (c->expr != NULL) {
      g95_show_expr(c->expr);
      g95_status_char('\n');
      g95_show_code(level+1, c->block);
      break;
    }

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
    for(fa=c->ext; fa; fa=fa->next) {
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
    g95_show_expr(((g95_iterator *) (c->ext))->var);
    g95_status_char('=');
    g95_show_expr(((g95_iterator *) (c->ext))->start);
    g95_status_char(' ');
    g95_show_expr(((g95_iterator *) (c->ext))->end);
    g95_status_char(' ');
    g95_show_expr(((g95_iterator *) (c->ext))->step);
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

    code_indent(level, 0);
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

    for(a=c->ext; a; a=a->next) {
      g95_status_char(' ');
      g95_show_expr(a->expr);
    }

    break;

  case EXEC_NULLIFY:
    g95_status("NULLIFY");
    for(a=c->ext; a; a=a->next) {
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

    for(a=c->ext; a; a=a->next) {
      g95_status_char(' ');
      g95_show_expr(a->expr);
    }

    break;

  case EXEC_OPEN:
    g95_status("OPEN");
    open = c->ext;

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
    if (open->err != 0) g95_status(" ERR=%d", open->err);

    break;

  case EXEC_CLOSE:
    g95_status("CLOSE");
    close = c->ext;

    if (close->unit) { g95_status(" UNIT="); g95_show_expr(close->unit); }
    if (close->iostat) { g95_status(" IOSTAT="); g95_show_expr(close->iostat);}
    if (close->status) { g95_status(" STATUS=");g95_show_expr(close->status); }
    if (close->err != 0) g95_status(" ERR=%d", close->err);
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
    fp = c->ext;

    if (fp->unit) { g95_status(" UNIT="); g95_show_expr(fp->unit); }
    if (fp->iostat) { g95_status(" IOSTAT="); g95_show_expr(fp->iostat); }
    if (fp->err != 0) g95_status(" ERR=%d", fp->err);
    break;

  case EXEC_INQUIRE:
    g95_status("INQURE");
    i = c->ext;

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

    if (i->err != 0) g95_status(" ERR=%d", i->err);
    break;

  case EXEC_IOLENGTH:
    g95_status("IOLENGTH ");
    g95_show_expr(c->expr);
    g95_status_char('\n');

    g95_show_code(level+1, c->block);
    break;

  case EXEC_READ:
    g95_status("READ");
    goto show_dt;

  case EXEC_WRITE:
    g95_status("WRITE");

  show_dt:
    dt = c->ext;
    if (dt->io_unit) {
      g95_status(" UNIT="); g95_show_expr(dt->io_unit); }

    if (dt->format_expr) {
      g95_status(" FMT="); g95_show_expr(dt->format_expr); }

    if (dt->format_label != 0) g95_status(" FMT=%d", dt->format_label);
    if (dt->namelist) g95_status(" NML=%s", dt->namelist->name);
    if (dt->iostat) { g95_status(" IOSTAT="); g95_show_expr(dt->iostat); }
    if (dt->size) { g95_status(" SIZE="); g95_show_expr(dt->size); }
    if (dt->rec) { g95_status(" REC="); g95_show_expr(dt->rec); }
    if (dt->advance) { g95_status(" ADVANCE="); g95_show_expr(dt->advance); }
    if (dt->err) g95_status(" ERR=%d", dt->err);
    if (dt->end) g95_status(" END=%d", dt->end);
    if (dt->eor) g95_status(" EOR=%d", dt->eor);

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
