/* I/O related subroutines
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

/* io.c-- Deal with input/output statements */

#include "g95.h"
#include <string.h>


typedef struct { 
  const char *name, *spec;
  bt type;
} io_tag;

static io_tag
  tag_file = { "FILE", " file = %e", BT_CHARACTER },
  tag_status = { "STATUS", " status = %e", BT_CHARACTER },
  tag_e_access = { "ACCESS", " access = %e", BT_CHARACTER },
  tag_e_form = { "FORM", " form = %e", BT_CHARACTER },
  tag_e_recl = { "RECL", " recl = %e", BT_INTEGER },
  tag_e_blank = { "BLANK", " blank = %e", BT_CHARACTER },
  tag_e_position = { "POSITION", " position = %e", BT_CHARACTER },
  tag_e_action = { "ACTION", " action = %e", BT_CHARACTER },
  tag_e_delim = { "DELIM", " delim = %e", BT_CHARACTER },
  tag_e_pad = { "PAD", " pad = %e", BT_CHARACTER },
  tag_unit = { "UNIT", " unit = %e", BT_INTEGER },
  tag_advance = { "ADVANCE", " advance = %e", BT_CHARACTER },
  tag_rec = { "REC", " rec = %e", BT_INTEGER },
  tag_format = { "FORMAT", NULL, BT_CHARACTER },

  tag_iostat = { "IOSTAT", " iostat = %v", BT_INTEGER },
  tag_size = { "SIZE", " size = %v", BT_INTEGER },
  tag_exist = { "EXIST", " exist = %v", BT_LOGICAL },
  tag_opened = { "OPENED", " opened = %v", BT_LOGICAL },
  tag_named = { "NAMED", " named = %v", BT_LOGICAL },
  tag_name = { "NAME", " name = %v", BT_CHARACTER },
  tag_number = { "NUMBER", " number = %v", BT_INTEGER },
  tag_s_access = { "ACCESS", " access = %v", BT_CHARACTER },
  tag_sequential = { "SEQUENTIAL", " sequential = %v", BT_CHARACTER },
  tag_direct = { "DIRECT", " direct = %v", BT_CHARACTER },
  tag_s_form = { "FORM", " form = %v", BT_CHARACTER },
  tag_formatted = { "FORMATTED", " formatted = %v", BT_CHARACTER },
  tag_unformatted = { "UNFORMATTED", " unformatted = %v", BT_CHARACTER },
  tag_s_recl = { "RECL", " recl = %v", BT_INTEGER },
  tag_nextrec = { "NEXTREC", " nextrec = %v", BT_INTEGER },
  tag_s_blank = { "BLANK", " blank = %v", BT_CHARACTER },
  tag_s_position = { "POSITION", " position = %v", BT_CHARACTER },
  tag_s_action = { "ACTION", " action = %v", BT_CHARACTER },
  tag_read = { "READ", " read = %v", BT_CHARACTER },
  tag_write = { "WRITE", " write = %v", BT_CHARACTER },
  tag_readwrite = { "READWRITE", " readwrite = %v", BT_CHARACTER },
  tag_s_delim = { "DELIM", " delim = %v", BT_CHARACTER },
  tag_s_pad = { "PAD", " pad = %v", BT_CHARACTER },
  tag_iolength = { "IOLENGTH", " iolength = %v", BT_INTEGER },

  tag_err = { "ERR", " err = %l", BT_UNKNOWN },
  tag_end = { "END", " end = %l", BT_UNKNOWN },
  tag_eor = { "EOR", " eor = %l", BT_UNKNOWN };
  

/* match_etag()-- Match an expression I/O tag of some sort. */

static match match_etag(io_tag *tag, g95_expr **v) {
g95_expr *result;
match m;

  m = g95_match(tag->spec, &result);
  if (m != MATCH_YES) return m;

  if (*v != NULL) {
    g95_error("Duplicate %s specification at %C", tag->name);
    g95_free_expr(result);
    return MATCH_ERROR;
  }

  *v = result;
  return MATCH_YES;
}


/* match_vtag()-- Match a variable I/O tag of some sort. */

static match match_vtag(io_tag *tag, g95_expr **v) {
g95_expr *result;
match m;

  m = g95_match(tag->spec, &result);
  if (m != MATCH_YES) return m;

  if (*v != NULL) {
    g95_error("Duplicate %s specification at %C", tag->name);
    g95_free_expr(result);
    return MATCH_ERROR;
  }

  *v = result;
  return MATCH_YES;
}


/* match_ltag()-- Match a label I/O tag */

static match match_ltag(io_tag *tag, int *label) {
match m;
int old;

  old = *label;
  m = g95_match(tag->spec, label);
  if (m == MATCH_YES && old != 0) {
    g95_error("Duplicate %s label specification at %C", tag->name);
    return MATCH_ERROR;
  }

  return m;
}


/* resolve_tag()-- Do expression resolution and type-checking on an
 * expression tag */

static void resolve_tag(io_tag *tag, g95_expr *e) {

  if (e == NULL || g95_resolve_expr(e) == FAILURE) return;

  if (e->ts.type != tag->type)
    g95_error("%s tag at %L must be of type %s", tag->name, &e->where,
	      g95_typename(tag->type));
}


/* match_open_element()-- Match a single tag of an OPEN statement */

static match match_open_element(g95_open *open) {
match m;

  m = match_etag(&tag_unit, &open->unit);          if (m != MATCH_NO) return m;
  m = match_vtag(&tag_iostat, &open->iostat);      if (m != MATCH_NO) return m;
  m = match_etag(&tag_file, &open->file);          if (m != MATCH_NO) return m;
  m = match_etag(&tag_status, &open->status);      if (m != MATCH_NO) return m;
  m = match_etag(&tag_e_access, &open->access);    if (m != MATCH_NO) return m;
  m = match_etag(&tag_e_form, &open->form);        if (m != MATCH_NO) return m;
  m = match_etag(&tag_e_recl, &open->recl);        if (m != MATCH_NO) return m;
  m = match_etag(&tag_e_blank, &open->blank);      if (m != MATCH_NO) return m;
  m = match_etag(&tag_e_position, &open->position);if (m != MATCH_NO) return m;
  m = match_etag(&tag_e_action, &open->action);    if (m != MATCH_NO) return m;
  m = match_etag(&tag_e_delim, &open->delim);      if (m != MATCH_NO) return m;
  m = match_etag(&tag_e_pad, &open->pad);          if (m != MATCH_NO) return m;
  m = match_ltag(&tag_err, &open->err);            if (m != MATCH_NO) return m;

  return MATCH_NO;
}


/* g95_free_open()-- Free the g95_open structure and all the
 * expressions it contains. */

void g95_free_open(g95_open *open) {

  if (open == NULL) return;

  g95_free_expr(open->unit);
  g95_free_expr(open->iostat);
  g95_free_expr(open->file);
  g95_free_expr(open->status);
  g95_free_expr(open->access);
  g95_free_expr(open->form);
  g95_free_expr(open->recl);
  g95_free_expr(open->blank);
  g95_free_expr(open->position);
  g95_free_expr(open->action);
  g95_free_expr(open->delim);
  g95_free_expr(open->pad);

  g95_free(open);
}


/* g95_resolve_open()-- resolve everything in a g95_open structure */

void g95_resolve_open(g95_open *open) {

  resolve_tag(&tag_unit, open->unit);
  resolve_tag(&tag_iostat, open->iostat);
  resolve_tag(&tag_file, open->file);
  resolve_tag(&tag_status, open->status);
  resolve_tag(&tag_e_form, open->form);
  resolve_tag(&tag_e_recl, open->recl);
  
  resolve_tag(&tag_e_blank, open->blank);
  resolve_tag(&tag_e_position, open->position);
  resolve_tag(&tag_e_action, open->action);
  resolve_tag(&tag_e_delim, open->delim);
  resolve_tag(&tag_e_pad, open->pad);

  g95_reference_st_label(open->err, ST_LABEL_TARGET);
}


/* g95_match_open()-- Match an OPEN statmement */

match g95_match_open(void) {
g95_open *open;
match m;

  m = g95_match_char('(');
  if (m == MATCH_NO) return m;

  open = g95_getmem(sizeof(g95_open));

  m = match_open_element(open);

  if (m == MATCH_ERROR) goto cleanup;
  if (m == MATCH_NO) {
    m = g95_match_expr(&open->unit);
    if (m == MATCH_NO) goto syntax;
    if (m == MATCH_ERROR) goto cleanup;
  }

  for(;;) {
    if (g95_match_char(')') == MATCH_YES) break;
    if (g95_match_char(',') != MATCH_YES) goto syntax;

    m = match_open_element(open);
    if (m == MATCH_ERROR) goto cleanup;
    if (m == MATCH_NO) goto syntax;
  }

  if (g95_match_eos() == MATCH_NO) goto syntax;

  new_st.op = EXEC_OPEN;
  new_st.ext.open = open;
  return MATCH_YES;

syntax:
  g95_syntax_error(ST_OPEN);

cleanup:
  g95_free_open(open);
  return MATCH_ERROR;
}


/* g95_free_close()-- Free a g95_close structure an all its expressions */

void g95_free_close(g95_close *close) {

  if (close == NULL) return;

  g95_free_expr(close->unit);
  g95_free_expr(close->iostat);
  g95_free_expr(close->status);

  g95_free(close);
}


/* match_close_element()-- Match elements of a CLOSE statment */

static match match_close_element(g95_close *close) {
match m;

  m = match_etag(&tag_unit, &close->unit);       if (m != MATCH_NO) return m;
  m = match_etag(&tag_status, &close->status);   if (m != MATCH_NO) return m;
  m = match_vtag(&tag_iostat, &close->iostat);   if (m != MATCH_NO) return m;
  m = match_ltag(&tag_err, &close->err);         if (m != MATCH_NO) return m;

  return MATCH_NO;
}


/* g95_match_close()-- Match a CLOSE statement */

match g95_match_close(void) {
g95_close *close;
match m;

  m = g95_match_char('(');
  if (m == MATCH_NO) return m;

  close = g95_getmem(sizeof(g95_close));

  m = match_close_element(close);

  if (m == MATCH_ERROR) goto cleanup;
  if (m == MATCH_NO) {
    m = g95_match_expr(&close->unit);
    if (m == MATCH_NO) goto syntax;
    if (m == MATCH_ERROR) goto cleanup;
  }

  for(;;) {
    if (g95_match_char(')') == MATCH_YES) break;
    if (g95_match_char(',') != MATCH_YES) goto syntax;

    m = match_close_element(close);
    if (m == MATCH_ERROR) goto cleanup;
    if (m == MATCH_NO) goto syntax;
  }

  if (g95_match_eos() == MATCH_NO) goto syntax;

  new_st.op = EXEC_CLOSE;
  new_st.ext.close = close;
  return MATCH_YES;

syntax:
  g95_syntax_error(ST_CLOSE);

cleanup:
  g95_free_close(close);
  return MATCH_ERROR;
}


/* g95_resolve_close()-- Resolve everything in a g95_close structure */

void g95_resolve_close(g95_close *close) {

  resolve_tag(&tag_unit, close->unit);
  resolve_tag(&tag_iostat, close->iostat);
  resolve_tag(&tag_status, close->status);

  g95_reference_st_label(close->err, ST_LABEL_TARGET);
}


/* g95_free_filepos()-- Free a g95_filepos structure */

void g95_free_filepos(g95_filepos *fp) {

  g95_free_expr(fp->unit);
  g95_free_expr(fp->iostat);
  g95_free(fp);
}


/* match_file_element()-- Match elements of a REWIND, BACKSPACE or
 * ENDFILE statement */

static match match_file_element(g95_filepos *fp) {
match m;

  m = match_etag(&tag_unit, &fp->unit);      if (m != MATCH_NO) return m;
  m = match_vtag(&tag_iostat, &fp->iostat);  if (m != MATCH_NO) return m;
  m = match_ltag(&tag_err, &fp->err);        if (m != MATCH_NO) return m;

  return MATCH_NO;
}


/* match_filepos()-- Match the second half of the file-positioning
 * statements, REWIND, BACKSPACE or ENDFILE. */

static match match_filepos(g95_statement st, g95_exec_op op) {
g95_filepos *fp;
match m;

  fp = g95_getmem(sizeof(g95_filepos));

  if (g95_match_char('(') == MATCH_NO) {
    m = g95_match_expr(&fp->unit);
    if (m == MATCH_ERROR) goto cleanup;
    if (m == MATCH_NO) goto syntax;

    goto done;
  }

  m = match_file_element(fp);
  if (m == MATCH_ERROR) goto done;
  if (m == MATCH_NO) {
    m = g95_match_expr(&fp->unit);
    if (m == MATCH_ERROR) goto done;
    if (m == MATCH_NO) goto syntax;
  }

  for(;;) {
    if (g95_match_char(')') == MATCH_YES) break;
    if (g95_match_char(',') != MATCH_YES) goto syntax;

    m = match_file_element(fp);
    if (m == MATCH_ERROR) goto cleanup;
    if (m == MATCH_NO) goto syntax;
  }

done:
  if (g95_match_eos() != MATCH_YES) goto syntax;

  new_st.op = op;
  new_st.ext.filepos = fp;
  return MATCH_YES;

syntax:
  g95_syntax_error(st);

cleanup:
  g95_free_filepos(fp);
  return MATCH_ERROR;
}


void g95_resolve_filepos(g95_filepos *fp) {

  resolve_tag(&tag_unit, fp->unit);
  g95_reference_st_label(fp->err, ST_LABEL_TARGET);
}


/* Match the file positioning statements: ENDFILE, BACKSPACE or REWIND */

match g95_match_endfile(void) {

  return match_filepos(ST_END_FILE, EXEC_ENDFILE);
}

match g95_match_backspace(void) {

  return match_filepos(ST_BACKSPACE, EXEC_BACKSPACE);
}

match g95_match_rewind(void) {

  return match_filepos(ST_REWIND, EXEC_REWIND);
}


/******************** Data Transfer Statments *********************/

typedef enum { M_READ, M_WRITE, M_PRINT, M_INQUIRE } io_kind;


/* default_unit()-- Return a default unit number */

static g95_expr *default_unit(io_kind k) {
int unit;

  if (k == M_READ) unit = 5;
  else unit = 6;

  return g95_int_expr(unit);
}


/* match_dt_unit()-- Match a unit specification for a data transfer
 * statement */

static match match_dt_unit(io_kind k, g95_dt *dt) {
g95_expr *e;

  if (g95_match_char('*') == MATCH_YES) {
    if (dt->io_unit != NULL) goto conflict;

    dt->io_unit = default_unit(k);
    return MATCH_YES;
  }

  if (g95_match_expr(&e) == MATCH_YES) {
    if (dt->io_unit != NULL) {
      g95_free_expr(e);
      goto conflict;
    }

    dt->io_unit = e;
    return MATCH_YES;
  }

  return MATCH_NO;

conflict:
  g95_error("Duplicate UNIT specification at %C");
  return MATCH_ERROR;
}


/* match_dt_format()-- Match a format specification */

static match match_dt_format(g95_dt *dt) {
locus where;
g95_expr *e;
int label;

  where = *g95_current_locus(); 

  if (g95_match_char('*') == MATCH_YES) {
    if (dt->format_expr != NULL || dt->format_label != 0) goto conflict;

    dt->format_label = -1;
    return MATCH_YES;
  }

  if (g95_match_st_label(&label) == MATCH_YES) {
    if (dt->format_expr != NULL || dt->format_label != 0) goto conflict;

    if (g95_reference_st_label(label, ST_LABEL_FORMAT) == FAILURE)
      return MATCH_ERROR;

    dt->format_label = label;
    return MATCH_YES;
  }

  if (g95_match_expr(&e) == MATCH_YES) {
    if (dt->format_expr != NULL || dt->format_label != 0) {
      g95_free_expr(e);
      goto conflict;
    }

    dt->format_expr = e;
    return MATCH_YES;
  }

  g95_set_locus(&where);    /* The only case where we have to restore */

  return MATCH_NO;

conflict:
  g95_error("Duplicate format specification at %C");
  return MATCH_ERROR;
}


/* match_dt_element()-- Match a single data transfer element */

static match match_dt_element(io_kind k, g95_dt *dt) {
g95_symbol *sym;
match m;

  if (g95_match(" unit =") == MATCH_YES) {
    m = match_dt_unit(k, dt);
    if (m != MATCH_NO) return m;
  }

  if (g95_match(" fmt =") == MATCH_YES) {
    m = match_dt_format(dt);
    if (m != MATCH_NO) return m;
  }

  if (g95_match(" nml = %s", &sym) == MATCH_YES) {
    if (dt->namelist != NULL) {
      g95_error("Duplicate NML specification at %C");
      return MATCH_ERROR;
    }

    if (sym->attr.flavor != FL_NAMELIST) {
      g95_error("Symbol at %C must be a NAMELIST group name");
      return MATCH_ERROR;
    }

    dt->namelist = sym;
    return MATCH_YES;
  }

  m = match_etag(&tag_rec, &dt->rec);         if (m != MATCH_NO) return m;
  m = match_vtag(&tag_iostat, &dt->iostat);   if (m != MATCH_NO) return m;
  m = match_ltag(&tag_err, &dt->err);         if (m != MATCH_NO) return m;
  m = match_etag(&tag_advance, &dt->advance); if (m != MATCH_NO) return m;
  m = match_vtag(&tag_size, &dt->size);       if (m != MATCH_NO) return m;

  m = match_ltag(&tag_end, &dt->end);
  if (m == MATCH_YES) dt->end_where = *g95_current_locus();
  if (m != MATCH_NO) return m;

  m = match_ltag(&tag_eor, &dt->eor);
  if (m == MATCH_YES) dt->eor_where = *g95_current_locus();
  if (m != MATCH_NO) return m;

  return MATCH_NO;
}


/* g95_free_dt()-- Free a data transfer structure and everything below it */

void g95_free_dt(g95_dt *dt) {

  if (dt == NULL) return;

  g95_free_expr(dt->io_unit);
  g95_free_expr(dt->format_expr);
  g95_free_expr(dt->rec);
  g95_free_expr(dt->advance);
  g95_free_expr(dt->iostat);
  g95_free_expr(dt->size);

  g95_free(dt);
}


/* g95_resolve_dt()-- Resolve everything in a g95_dt structure */

void g95_resolve_dt(g95_dt *dt) {
g95_expr *e;

  resolve_tag(&tag_format, dt->format_expr);
  resolve_tag(&tag_rec, dt->rec);
  resolve_tag(&tag_advance, dt->advance);

  e = dt->io_unit;
  if (g95_resolve_expr(e) == SUCCESS) {
    if (e->ts.type != BT_INTEGER &&
	(e->ts.type != BT_CHARACTER || e->expr_type != EXPR_VARIABLE))
      g95_error("UNIT specification at %L must be INTEGER expression or "
		"CHARACTER variable", &e->where);
  }

  resolve_tag(&tag_iostat, dt->iostat);
  resolve_tag(&tag_size, dt->size);

/* Sanity checks on data transfer statements */

  if (e->ts.type == BT_CHARACTER) {
    if (dt->rec != NULL)
      g95_error("REC tag at %L is incompatible with internal file",
		&dt->rec->where);

    if (dt->namelist != NULL)
      g95_error("Internal file at %L is incompatible with namelist",
		&dt->io_unit->where);

    if (dt->advance != NULL)
      g95_error("ADVANCE tag at %L is incompatible with internal file",
		&dt->advance->where);
  }

  if (dt->rec != NULL) {
    if (dt->end != 0) g95_error("REC tag at %L is incompatible with END tag",
				&dt->rec->where);

    if (dt->format_label == -1)
      g95_error("END tag at %L is incompatible with list directed format (*)",
		&dt->end_where);

    if (dt->namelist != NULL)
      g95_error("REC tag at %L is incompatible with namelist",
		&dt->rec->where);
  }

  if (dt->advance != NULL && dt->format_label == -1)
    g95_error("ADVANCE tag at %L is incompatible with list directed "
	      "format (*)", &dt->advance->where);

  if (dt->eor != 0 && dt->advance == NULL)
    g95_error("EOR tag at %L requires an ADVANCE tag", &dt->eor_where);

  if (dt->size != NULL && dt->advance == NULL)
    g95_error("SIZE tag at %L requries an ADVANCE tag", &dt->size->where);

/* TODO: Make sure the ADVANCE tag is 'yes' or 'no' if it is a string
 * constant */

  g95_reference_st_label(dt->err, ST_LABEL_TARGET);
  g95_reference_st_label(dt->end, ST_LABEL_TARGET);
  g95_reference_st_label(dt->eor, ST_LABEL_TARGET);
}


/* io_kind_name()-- Given an io_kind, return its name */

static const char *io_kind_name(io_kind k) {
const char *name;

  switch(k) {
  case M_READ:     name = "READ";     break;
  case M_WRITE:    name = "WRITE";    break;
  case M_PRINT:    name = "PRINT";    break;
  case M_INQUIRE:  name = "INQUIRE";  break;
  default:
    g95_internal_error("io_kind_name(): bad I/O-kind");
  }

  return name;
}


static g95_symbol io_integer = { "_io_integer" },
  io_real = { "_io_real" }, 
  io_logical = { "_io_logical" },
  io_character = { "_io_character" },
  io_complex = { "_io_complex" },
  io_done = { "_io_done" },
  io_unknown = { "_io_unknown" };

/* gen_io_pointer()-- Given an expression, generate code structure(s)
 * that call the IO library with a pointer(s) to the correct thing. 
 * This possibly generates a store to a temporary */

static g95_code *gen_io_pointer(g95_expr *e) {
g95_code *c;

  switch(e->ts.type) {
  case BT_UNKNOWN:
    c = g95_build_call(&io_unknown, e, NULL);
    break;

  case BT_INTEGER:
    c = g95_build_call(&io_integer, e, NULL);
    break;

  case BT_REAL:
    c = g95_build_call(&io_real, e, NULL);
    break;

  case BT_LOGICAL:
    c = g95_build_call(&io_logical, e, NULL);
    break;

  case BT_CHARACTER:
    c = g95_build_call(&io_character, e, NULL);
    break;

/* We can't just call io_real twice here because complex numbers
 * require special formatting for list-directed IO */

  case BT_COMPLEX:
    c = g95_build_call(&io_complex, e, NULL);
    break;

  case BT_DERIVED:
    /* TODO: We have to be careful for derived types as well.  In
     * particular, we don't want to recompute the base address for
     * each component.  The idea will be to calculate the base of the
     * structure, save it to a temp and then traverse the component
     * list, calling the various subroutines with the base+offset for
     * each member. */

    c = g95_build_call(&io_unknown, e, NULL);
    break;

  default:
    g95_internal_error("gen_io_pointer(): Bad type");

  }

  return c;
}


/* match_io_iterator()-- Match an IO iteration statement of the form:
 *   ( [<IO element> ,] <IO element>, I = <expr>, <expr> [, <expr> ] )
 *
 * Which is equivalent to a single IO element.  This function is mutually
 * recursive with match_io_element().  */

static match match_io_element(io_kind k, g95_code **);

static match match_io_iterator(io_kind k, g95_code **result) {
g95_code *head, *tail, *new;
g95_iterator *iter;
locus old_loc;
match m;

  head = tail = NULL; 
  old_loc = *g95_current_locus();

  if (g95_match_char('(') != MATCH_YES) return MATCH_NO;

  m = match_io_element(k, &head);
  tail = head;

  if (m != MATCH_YES || g95_match_char(',') != MATCH_YES) {
    g95_set_locus(&old_loc);     /* Can't be an IO iterator */
    g95_free_statements(head);
    return MATCH_NO;
  }

/* Can't be anything but an IO iterator.  Build a list */

  iter = g95_get_iterator();

  for(;;) {
    m = g95_match_iterator(iter);
    if (m == MATCH_ERROR) goto cleanup;
    if (m == MATCH_YES) break;

    m = match_io_element(k, &new);
    if (m == MATCH_NO) goto syntax;
    if (m == MATCH_ERROR) goto cleanup;

    tail = g95_append_code(tail, new);

    if (g95_match_char(',') != MATCH_YES) goto syntax;
  }

  if (g95_match_char(')') != MATCH_YES) goto syntax;

  new = g95_get_code();
  new->op = EXEC_DO;
  new->ext.iterator = iter;

  new->block = g95_get_code();
  new->block->op = EXEC_DO;
  new->block->next = head;

  *result = new;
  return MATCH_YES;

syntax:
  g95_error("Syntax error in I/O iterator at %C");

cleanup:
  g95_free_iterator(iter, 1);
  g95_free_statements(head);
  return MATCH_ERROR;
}


/* match_io_element()-- Match a single element of an IO list, which is
 * either a single expression or an IO Iterator */

static match match_io_element(io_kind k, g95_code **c) {
g95_expr *expr;
match m;

  m = match_io_iterator(k, c);
  if (m == MATCH_YES) return MATCH_YES;

  if (k == M_READ) {
    m = g95_match_variable(&expr, 0);
    if (m == MATCH_NO) g95_error("Expected variable in READ statement at %C");
  } else {
    m = g95_match_expr(&expr);
    if (m == MATCH_NO) g95_error("Expected expression in %s statement at %C",
				 io_kind_name(k));
  }

  if (m != MATCH_YES) return MATCH_ERROR;

  *c = gen_io_pointer(expr);
  return MATCH_YES;
}


/* match_io_list()-- Match an I/O list, building g95_code structures
 * as we go. */

static match match_io_list(io_kind k, g95_code **head_p) {
g95_code *head, *tail, *new;
match m;

  *head_p = head = tail = NULL; 
  if (g95_match_eos() == MATCH_YES) return MATCH_YES;

  for(;;) {
    m = match_io_element(k, &new);
    if (m == MATCH_ERROR) goto cleanup;
    if (m == MATCH_NO) goto syntax;

    tail = g95_append_code(tail, new);
    if (head == NULL) head = new;

    if (g95_match_eos() == MATCH_YES) break;
    if (g95_match_char(',') != MATCH_YES) goto syntax;
  }

  *head_p = head;
  return MATCH_YES;

syntax:
  g95_error("Syntax error in %s statement at %C", io_kind_name(k));

cleanup:
  g95_free_statements(head);
  return MATCH_ERROR;
}


/* match_io()-- Match a READ, WRITE or PRINT statement. */

static match match_io(io_kind k) {
g95_code *io_code, *term;
g95_symbol *sym;
g95_expr *expr;
int comma_flag;
locus where;
g95_dt *dt;
match m;
 
  comma_flag = 0;
  dt = g95_getmem(sizeof(g95_dt));

  if (g95_match_char('(') == MATCH_NO) {
    if (k == M_WRITE) goto syntax;

    m = match_dt_format(dt);
    if (m == MATCH_ERROR) goto cleanup;
    if (m == MATCH_NO) goto syntax;

    comma_flag = 1;
    dt->io_unit = default_unit(k);
    goto get_io_list;
  }

/* Match a control list */

  if (match_dt_element(k, dt) == MATCH_YES) goto next;
  if (match_dt_unit(k, dt) != MATCH_YES) goto loop;

  if (g95_match_char(')') == MATCH_YES) goto get_io_list;
  if (g95_match_char(',') != MATCH_YES) goto syntax; 

  if (match_dt_element(k, dt) == MATCH_YES) goto next;

  m = match_dt_format(dt);
  if (m == MATCH_YES) goto next;
  if (m == MATCH_ERROR) goto cleanup;

  where = *g95_current_locus();

  if (g95_match_symbol(&sym) == MATCH_YES && sym->attr.flavor == FL_NAMELIST) {
    dt->namelist = sym;
    goto next;
  }

  g95_set_locus(&where);

  goto loop;   /* No matches, try regular elements */

next:
  if (g95_match_char(')') == MATCH_YES) goto get_io_list;
  if (g95_match_char(',') != MATCH_YES) goto syntax; 

loop:
  for(;;) {
    m = match_dt_element(k, dt);
    if (m == MATCH_NO) goto syntax;
    if (m == MATCH_ERROR) goto cleanup;

    if (g95_match_char(')') == MATCH_YES) break;
    if (g95_match_char(',') != MATCH_YES) goto syntax;
  }

get_io_list:
  if (!comma_flag) g95_match_char(',');
     /* Optional leading comma (non-standard) */

  io_code = NULL;
  if (g95_match_eos() != MATCH_YES) {
    if (comma_flag && g95_match_char(',') != MATCH_YES) {
      g95_error("Expected comma in I/O list at %C");
      m = MATCH_ERROR;
      goto cleanup;
    }

    m = match_io_list(k, &io_code);
    if (m == MATCH_ERROR) goto cleanup;
    if (m == MATCH_NO) goto syntax;
  }

  term = g95_build_call(&io_done, NULL);

  if (io_code == NULL) io_code = term;
  else g95_append_code(io_code, term);

/* A full IO statement has been matched */

  expr = dt->format_expr;

  if (expr != NULL && expr->expr_type == EXPR_CONSTANT)
    g95_check_format_string(expr);

  new_st.op = (k == M_READ) ? EXEC_READ : EXEC_WRITE;
  new_st.ext.dt = dt;
  new_st.next = io_code;

  return MATCH_YES;

syntax:
  g95_error("Syntax error in %s statement at %C", io_kind_name(k));
  m = MATCH_ERROR;

cleanup:
  g95_free_dt(dt);
  return m;
}


match g95_match_read(void)  { return match_io(M_READ);  }

match g95_match_write(void) { return match_io(M_WRITE); }

match g95_match_print(void) { return match_io(M_PRINT); }



/* g95_free_inquire()-- Free a g95_inquire structure */

void g95_free_inquire(g95_inquire *inquire) {

  if (inquire == NULL) return;

  g95_free_expr(inquire->unit);
  g95_free_expr(inquire->file);
  g95_free_expr(inquire->iostat);
  g95_free_expr(inquire->exist);
  g95_free_expr(inquire->opened);
  g95_free_expr(inquire->number);
  g95_free_expr(inquire->named);
  g95_free_expr(inquire->name);
  g95_free_expr(inquire->access);
  g95_free_expr(inquire->sequential);
  g95_free_expr(inquire->direct);
  g95_free_expr(inquire->form);
  g95_free_expr(inquire->formatted);
  g95_free_expr(inquire->unformatted);
  g95_free_expr(inquire->recl);
  g95_free_expr(inquire->nextrec);
  g95_free_expr(inquire->blank);
  g95_free_expr(inquire->position);
  g95_free_expr(inquire->action);
  g95_free_expr(inquire->read);
  g95_free_expr(inquire->write);
  g95_free_expr(inquire->readwrite);
  g95_free_expr(inquire->delim);
  g95_free_expr(inquire->pad);
  g95_free_expr(inquire->iolength);

  g95_free(inquire);
}


/* match_inquire_element()-- Match an element of an INQUIRE statement */

#define RETM   if (m != MATCH_NO) return m; 

static match match_inquire_element(g95_inquire *inquire) {
match m;

  m = match_etag(&tag_unit, &inquire->unit);        RETM
  m = match_etag(&tag_file, &inquire->file);        RETM
  m = match_ltag(&tag_err, &inquire->err);          RETM
  m = match_vtag(&tag_iostat, &inquire->iostat);    RETM

  m = match_vtag(&tag_exist, &inquire->exist);              RETM
  m = match_vtag(&tag_opened, &inquire->opened);            RETM
  m = match_vtag(&tag_named, &inquire->named);              RETM
  m = match_vtag(&tag_name, &inquire->name);                RETM
  m = match_vtag(&tag_number, &inquire->number);            RETM
  m = match_vtag(&tag_s_access, &inquire->access);          RETM
  m = match_vtag(&tag_sequential, &inquire->sequential);    RETM
  m = match_vtag(&tag_direct, &inquire->direct);            RETM
  m = match_vtag(&tag_s_form, &inquire->form);              RETM
  m = match_vtag(&tag_formatted, &inquire->formatted);      RETM
  m = match_vtag(&tag_unformatted, &inquire->unformatted);  RETM
  m = match_vtag(&tag_s_recl, &inquire->recl);              RETM
  m = match_vtag(&tag_nextrec, &inquire->nextrec);          RETM
  m = match_vtag(&tag_s_blank, &inquire->blank);            RETM
  m = match_vtag(&tag_s_position, &inquire->position);      RETM
  m = match_vtag(&tag_s_action, &inquire->action);          RETM
  m = match_vtag(&tag_read, &inquire->read);                RETM
  m = match_vtag(&tag_write, &inquire->write);              RETM
  m = match_vtag(&tag_readwrite, &inquire->readwrite);      RETM
  m = match_vtag(&tag_s_delim, &inquire->delim);            RETM
  m = match_vtag(&tag_s_pad, &inquire->pad);                RETM
  m = match_vtag(&tag_iolength, &inquire->iolength);        RETM

  return MATCH_NO;
}

#undef RETM


match g95_match_inquire(void) {
g95_inquire *inquire;
g95_code *code;
match m;

  m = g95_match_char('(');
  if (m == MATCH_NO) return m;

  inquire = g95_getmem(sizeof(g95_inquire));

  m = match_inquire_element(inquire);
  if (m == MATCH_ERROR) goto cleanup;
  if (m == MATCH_NO) {
    m = g95_match_expr(&inquire->unit);
    if (m == MATCH_ERROR) goto cleanup;
    if (m == MATCH_NO) goto syntax;
  }

/* See if we have the IOLENGTH form of the inquire statement */

  if (inquire->iolength != NULL) {
    if (g95_match_char(')') != MATCH_YES) goto syntax;

    m = match_io_list(M_INQUIRE, &code);
    if (m == MATCH_ERROR) goto cleanup;
    if (m == MATCH_NO) goto syntax;

    new_st.op = EXEC_IOLENGTH;
    new_st.expr = inquire->iolength;
    g95_free(inquire);

    new_st.block = code;
    return MATCH_YES;
  }

/* At this point, we have the non-IOLENGTH inquire statement */

  for(;;) {
    if (g95_match_char(')') == MATCH_YES) break;
    if (g95_match_char(',') != MATCH_YES) goto syntax;

    m = match_inquire_element(inquire);
    if (m == MATCH_ERROR) goto cleanup;
    if (m == MATCH_NO) goto syntax;

    if (inquire->iolength != NULL) {
      g95_error("IOLENGTH tag invalid in INQUIRE statement at %C");
      goto cleanup;
    }
  }

  if (g95_match_eos() != MATCH_YES) goto syntax;

  new_st.op = EXEC_INQUIRE;
  new_st.ext.inquire = inquire;
  return MATCH_YES;

syntax:
  g95_syntax_error(ST_INQUIRE);

cleanup:
  g95_free_inquire(inquire);
  return MATCH_ERROR;
}


/* g95_resolve_inquire()-- Resolve everything in a g95_inquire structure */

void g95_resolve_inquire(g95_inquire *inquire) {
  resolve_tag(&tag_unit, inquire->unit);
  resolve_tag(&tag_file, inquire->file);
  resolve_tag(&tag_iostat, inquire->iostat);
  resolve_tag(&tag_exist, inquire->exist);
  resolve_tag(&tag_opened, inquire->opened);
  resolve_tag(&tag_number, inquire->number);
  resolve_tag(&tag_named, inquire->named);
  resolve_tag(&tag_name, inquire->name);
  resolve_tag(&tag_s_access, inquire->access);
  resolve_tag(&tag_sequential, inquire->sequential);
  resolve_tag(&tag_direct, inquire->direct);
  resolve_tag(&tag_s_form, inquire->form);
  resolve_tag(&tag_formatted, inquire->formatted);
  resolve_tag(&tag_unformatted, inquire->unformatted);
  resolve_tag(&tag_s_recl, inquire->recl);
  resolve_tag(&tag_nextrec, inquire->nextrec);
  resolve_tag(&tag_s_blank, inquire->blank);
  resolve_tag(&tag_s_position, inquire->position);
  resolve_tag(&tag_s_action, inquire->action);
  resolve_tag(&tag_read, inquire->read);
  resolve_tag(&tag_write, inquire->write);
  resolve_tag(&tag_readwrite, inquire->readwrite);
  resolve_tag(&tag_s_delim, inquire->delim);
  resolve_tag(&tag_s_pad, inquire->pad);

  g95_reference_st_label(inquire->err, ST_LABEL_TARGET);
}
