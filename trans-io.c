/* IO Code translation/library interface
   Copyright (C) 2000, 2001, 2002 Free Software Foundation, Inc.
   Contributed by Paul Brook

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

/* trans-io.c-- generate GCC trees from g95_code */

#include "config.h"
#include "system.h"
#include "tree.h"
#include "tree-simple.h"
#include <stdio.h>
#include "c-common.h"
#include "ggc.h"
#include "rtl.h"
#include "toplev.h"
#include "function.h"
#include "expr.h"
#include "real.h"
#include "tree-inline.h"

#include <assert.h>
#include <gmp.h>
#define BACKEND_CODE
#include "g95.h"
#include "trans.h"
#include "trans-stmt.h"
#include "trans-array.h"
#include "trans-types.h"
#include "trans-const.h"


static GTY(()) tree g95_pint4_type_node;

/* Members of the ioparm structure */

static GTY(()) tree ioparm_unit;
static GTY(()) tree ioparm_err;
static GTY(()) tree ioparm_end;
static GTY(()) tree ioparm_eor;
static GTY(()) tree ioparm_list_format;
static GTY(()) tree ioparm_library_return;
static GTY(()) tree ioparm_iostat;
static GTY(()) tree ioparm_exist;
static GTY(()) tree ioparm_opened;
static GTY(()) tree ioparm_number;
static GTY(()) tree ioparm_named;
static GTY(()) tree ioparm_rec;
static GTY(()) tree ioparm_nextrec;
static GTY(()) tree ioparm_size;
static GTY(()) tree ioparm_recl_in;
static GTY(()) tree ioparm_recl_out;
static GTY(()) tree ioparm_file;
static GTY(()) tree ioparm_file_len;
static GTY(()) tree ioparm_status;
static GTY(()) tree ioparm_status_len;
static GTY(()) tree ioparm_access;
static GTY(()) tree ioparm_access_len;
static GTY(()) tree ioparm_form;
static GTY(()) tree ioparm_form_len;
static GTY(()) tree ioparm_blank;
static GTY(()) tree ioparm_blank_len;
static GTY(()) tree ioparm_position;
static GTY(()) tree ioparm_position_len;
static GTY(()) tree ioparm_action;
static GTY(()) tree ioparm_action_len;
static GTY(()) tree ioparm_delim;
static GTY(()) tree ioparm_delim_len;
static GTY(()) tree ioparm_pad;
static GTY(()) tree ioparm_pad_len;
static GTY(()) tree ioparm_format;
static GTY(()) tree ioparm_format_len;
static GTY(()) tree ioparm_advance;
static GTY(()) tree ioparm_advance_len;
static GTY(()) tree ioparm_name;
static GTY(()) tree ioparm_name_len;
static GTY(()) tree ioparm_internal_unit;
static GTY(()) tree ioparm_internal_unit_len;
static GTY(()) tree ioparm_sequential;
static GTY(()) tree ioparm_sequential_len;
static GTY(()) tree ioparm_direct;
static GTY(()) tree ioparm_direct_len;
static GTY(()) tree ioparm_formatted;
static GTY(()) tree ioparm_formatted_len;
static GTY(()) tree ioparm_unformatted;
static GTY(()) tree ioparm_unformatted_len;
static GTY(()) tree ioparm_read;
static GTY(()) tree ioparm_read_len;
static GTY(()) tree ioparm_write;
static GTY(()) tree ioparm_write_len;
static GTY(()) tree ioparm_readwrite;
static GTY(()) tree ioparm_readwrite_len;

/* The global I/O variables */

static GTY(()) tree ioparm_var;
static GTY(()) tree locus_file;
static GTY(()) tree locus_line;


/* Library I/O subroutines */

static GTY(()) tree iocall_read;
static GTY(()) tree iocall_read_done;
static GTY(()) tree iocall_write;
static GTY(()) tree iocall_write_done;
static GTY(()) tree iocall_x_integer;
static GTY(()) tree iocall_x_logical;
static GTY(()) tree iocall_x_character;
static GTY(()) tree iocall_x_real;
static GTY(()) tree iocall_x_complex;
static GTY(()) tree iocall_open;
static GTY(()) tree iocall_close;
static GTY(()) tree iocall_inquire;
static GTY(()) tree iocall_rewind;
static GTY(()) tree iocall_backspace;
static GTY(()) tree iocall_endfile;

/* Variable for keeping track of what the last data transfer statement
 * was.  Used for deciding which subroutine to call when the data
 * transfer is complete. */

static enum { READ, WRITE } last_dt;


/* add_field()-- Add a field to the ioparm structure. */

static tree add_field(tree stype, char *name, tree type) {
tree decl;

  decl = build_decl(FIELD_DECL, get_identifier(name), type);

  DECL_CONTEXT(decl) = stype;
  DECL_INITIAL(decl) = 0;
  DECL_ALIGN(decl) = 0;
  DECL_USER_ALIGN(decl) = 0;
  TYPE_FIELDS(stype) = chainon(TYPE_FIELDS(stype), decl);

  return decl;
}


#define PREFIX "_g95_"

#define ADD_FIELD(name, type) \
  ioparm_ ## name = add_field(ioparm_type, stringize(name), type)

#define ADD_STRING(name) \
  ioparm_ ## name = add_field(ioparm_type, stringize(name), pchar_type_node);\
  ioparm_ ## name ## _len = \
        add_field(ioparm_type, stringize(name) "_len", g95_int4_type_node)


/* Create function decls for IO library functions.  */

void g95_build_io_library_fndecls (void) {
tree ioparm_type;

  g95_pint4_type_node = build_pointer_type(g95_int4_type_node);

/* Build the st_parameter structure.  Information associated with I/O
 * calls are transferred here.  This must match the one defined in the
 * library exactly. */

  ioparm_type = make_node(RECORD_TYPE);
  TYPE_NAME(ioparm_type) = get_identifier("_g95_ioparm");

  ADD_FIELD(unit,            g95_int4_type_node);
  ADD_FIELD(err,             g95_int4_type_node);
  ADD_FIELD(end,             g95_int4_type_node);
  ADD_FIELD(eor,             g95_int4_type_node);
  ADD_FIELD(list_format,     g95_int4_type_node);
  ADD_FIELD(library_return,  g95_int4_type_node);

  ADD_FIELD(iostat,    g95_pint4_type_node);
  ADD_FIELD(exist,     g95_pint4_type_node);
  ADD_FIELD(opened,    g95_pint4_type_node);
  ADD_FIELD(number,    g95_pint4_type_node);
  ADD_FIELD(named,     g95_pint4_type_node);
  ADD_FIELD(rec,       g95_pint4_type_node);
  ADD_FIELD(nextrec,   g95_pint4_type_node);
  ADD_FIELD(size,      g95_pint4_type_node);

  ADD_FIELD(recl_in,   g95_pint4_type_node);
  ADD_FIELD(recl_out,  g95_pint4_type_node);

  ADD_STRING(file);
  ADD_STRING(status);

  ADD_STRING(access);
  ADD_STRING(form);
  ADD_STRING(blank);
  ADD_STRING(position);
  ADD_STRING(action);
  ADD_STRING(delim);
  ADD_STRING(pad);
  ADD_STRING(format);
  ADD_STRING(advance);
  ADD_STRING(name);
  ADD_STRING(internal_unit);
  ADD_STRING(sequential);

  ADD_STRING(direct);
  ADD_STRING(formatted);
  ADD_STRING(unformatted);
  ADD_STRING(read);
  ADD_STRING(write);
  ADD_STRING(readwrite);

  g95_finish_type(ioparm_type);

  ioparm_var = build_decl(VAR_DECL, get_identifier(PREFIX "ioparm"),
			  ioparm_type);
  DECL_EXTERNAL(ioparm_var) = 1;

  locus_line = build_decl(VAR_DECL, get_identifier(PREFIX "line"),
			  g95_int4_type_node);
  DECL_EXTERNAL(locus_line) = 1;

  locus_file = build_decl(VAR_DECL, get_identifier(PREFIX "filename"),
			  pchar_type_node);
  DECL_EXTERNAL(locus_file) = 1;

  /* Define the transfer functions */

  iocall_x_integer =
    g95_build_library_function_decl(get_identifier(PREFIX "transfer_integer"),
						   void_type_node, 2,
						   pvoid_type_node,
						   g95_int4_type_node);

  iocall_x_logical =
    g95_build_library_function_decl(get_identifier(PREFIX "transfer_logical"),
						   void_type_node, 2,
						   pvoid_type_node,
						   g95_int4_type_node);

  iocall_x_character =
    g95_build_library_function_decl(
	      get_identifier(PREFIX "transfer_character"),
						   void_type_node, 2,
						   pvoid_type_node,
						   g95_int4_type_node);

  iocall_x_real =
    g95_build_library_function_decl(get_identifier(PREFIX "transfer_real"),
						 void_type_node, 2,
						 pvoid_type_node,
						 g95_int4_type_node);

  iocall_x_complex =
    g95_build_library_function_decl(get_identifier(PREFIX "transfer_complex"),
						   void_type_node, 2,
						   pvoid_type_node,
						   g95_int4_type_node);

  /* Library entry points */

  iocall_read =
    g95_build_library_function_decl(get_identifier(PREFIX "st_read"),
						   void_type_node, 0);

  iocall_write =
    g95_build_library_function_decl(get_identifier(PREFIX "st_write"),
						   void_type_node, 0);
  iocall_open =
    g95_build_library_function_decl(get_identifier(PREFIX "st_open"),
						   g95_int4_type_node, 0);

  iocall_close =
    g95_build_library_function_decl(get_identifier(PREFIX "st_close"),
						   g95_int4_type_node, 0);

  iocall_inquire =
    g95_build_library_function_decl(get_identifier(PREFIX "st_inquire"),
						   g95_int4_type_node, 0);

  iocall_rewind =
    g95_build_library_function_decl(get_identifier(PREFIX "st_rewind"),
						   g95_int4_type_node, 0);

  iocall_backspace =
    g95_build_library_function_decl(get_identifier(PREFIX "st_backspace"),
						   g95_int4_type_node, 0);

  iocall_endfile =
    g95_build_library_function_decl(get_identifier(PREFIX "st_endfile"),
						   g95_int4_type_node, 0);
  /* Library helpers */

  iocall_read_done = 
    g95_build_library_function_decl(get_identifier(PREFIX "st_read_done"),
						   g95_int4_type_node, 0);

  iocall_write_done = 
    g95_build_library_function_decl(get_identifier(PREFIX "st_write_done"),
						   g95_int4_type_node, 0);
}


/* set_parameter_value()-- Generate code to store an non-string I/O
 * parameter into the ioparm structure.  This is a pass by value. */

static void set_parameter_value(stmtblock_t *block, tree var, g95_expr *e) {
g95_se se;
tree tmp;

  g95_init_se(&se, NULL);
  g95_conv_expr_type(&se, e, TREE_TYPE(var));
  g95_add_block_to_block(block, &se.pre);

  tmp = build(COMPONENT_REF, TREE_TYPE(var), ioparm_var, var);
  tmp = build(MODIFY_EXPR, TREE_TYPE(var), tmp, se.expr);
  g95_add_expr_to_block(block, tmp);
}


/* set_parameter_ref()-- Generate code to store an non-string I/O
 * parameter into the ioparm structure.  This is pass by reference. */

static void set_parameter_ref(stmtblock_t *block, tree var, g95_expr *e) {
g95_se se;
tree tmp;

  g95_init_se(&se, NULL);
  se.want_pointer = 1;

  g95_conv_expr_type(&se, e, TREE_TYPE(var));
  g95_add_block_to_block(block, &se.pre);

  tmp = build(COMPONENT_REF, TREE_TYPE(var), ioparm_var, var);
  tmp = build(MODIFY_EXPR, TREE_TYPE(var), tmp, se.expr);
  g95_add_expr_to_block(block, tmp);
}


/* set_string()-- Generate code to store a string and its length into
 * the ioparm structure. */

static void set_string(stmtblock_t *block, stmtblock_t *postblock, tree var,
		       tree var_len, g95_expr *e) {
g95_se se;
tree tmp;

  g95_init_se(&se, NULL);
  g95_conv_expr(&se, e);
  g95_conv_string_parameter(&se);

  g95_add_block_to_block(block, &se.pre);
  g95_add_block_to_block(postblock, &se.post);

  tmp = build(COMPONENT_REF, TREE_TYPE(var), ioparm_var, var);
  tmp = build(MODIFY_EXPR, TREE_TYPE(tmp), tmp, se.expr);
  g95_add_expr_to_block(block, tmp);

  tmp = build(COMPONENT_REF, TREE_TYPE(var_len), ioparm_var, var_len);
  tmp = build(MODIFY_EXPR, TREE_TYPE(tmp), tmp, se.string_length);
  g95_add_expr_to_block(block, tmp);
}


/* add_case()-- Add a case to a IO-result switch */

static void add_case(int label_value, g95_st_label *label, stmtblock_t *body) {
tree tmp, value;

  if (label == NULL) return;   /* No label, no case */

  value = build_int_2(label_value, 0);

  tmp = build_v(CASE_LABEL_EXPR, value, NULL_TREE);
  g95_add_expr_to_block(body, tmp);

  tmp = build_v(GOTO_EXPR, g95_get_label_decl(label));
  g95_add_expr_to_block(body, tmp);
}


/* io_result()-- Generate a switch statement that branches to the
 * correct I/O result label.  The last statement of an I/O call stores
 * the result into a variable because there is often cleanup that must
 * be done before the switch, so a temporary would have to be created
 * anyway. */

static void io_result(stmtblock_t *block, g95_st_label *err_label,
		      g95_st_label *end_label, g95_st_label *eor_label) {
stmtblock_t body;
tree tmp, rc;

  /* If no labels are specified, ignore the result instead of building
   * an empty switch. */

  if (err_label == NULL && end_label == NULL && eor_label == NULL) return;

  /* Build a switch statement */

  g95_start_block(&body);

  /* The label values here must be the same as the values in the
   * library_return enum in the runtime library */

  add_case(1, err_label, &body);
  add_case(2, end_label, &body);
  add_case(3, eor_label, &body);

  tmp = g95_finish_block(&body);

  rc = build(COMPONENT_REF, TREE_TYPE(ioparm_library_return), ioparm_var,
	     ioparm_library_return);

  tmp = build_v(SWITCH_EXPR, rc, tmp, NULL_TREE);

  g95_add_expr_to_block(block, tmp);
}


/* set_runtime_locus()-- Store the current file and line number to
 * variables so that if a library call goes awry, we can tell the user
 * where the problem is. */

static void set_error_locus(stmtblock_t *block, locus *where) {
g95_file *f;
tree tmp;
int line;

  f = where->file;
  tmp = g95_build_string_const(strlen(f->filename)+1, f->filename);

  tmp = build1(ADDR_EXPR, pchar_type_node, tmp);
  tmp = build(MODIFY_EXPR, pchar_type_node, locus_file, tmp);
  g95_add_expr_to_block(block, tmp);

  line = where->lp->start_line + where->line;
  tmp = build(MODIFY_EXPR, g95_int4_type_node, locus_line,
	      build_int_2(line, 0));

  g95_add_expr_to_block(block, tmp);
}


/* g95_trans_open()-- Translate an OPEN statement */

tree g95_trans_open(g95_code *code) {
stmtblock_t block, post_block;
g95_open *p;
tree tmp;

  g95_init_block(&block);
  g95_init_block(&post_block);

  set_error_locus(&block, &code->loc);
  p = code->ext.open;

  if (p->unit) set_parameter_value(&block, ioparm_unit, p->unit);

  if (p->file) set_string(&block, &post_block, ioparm_file, ioparm_file_len,
			  p->file);

  if (p->status) set_string(&block, &post_block, ioparm_status,
			    ioparm_status_len, p->status);

  if (p->access) set_string(&block, &post_block, ioparm_access,
			    ioparm_access_len, p->access);

  if (p->form) set_string(&block, &post_block, ioparm_form, ioparm_form_len,
			  p->form);

  if (p->recl) set_parameter_ref(&block, ioparm_recl_in, p->recl);

  if (p->blank) set_string(&block, &post_block, ioparm_blank, ioparm_blank_len,
			   p->blank);

  if (p->position) set_string(&block, &post_block, ioparm_position,
			      ioparm_position_len, p->position);

  if (p->action) set_string(&block, &post_block, ioparm_action,
			    ioparm_action_len, p->action);

  if (p->delim) set_string(&block, &post_block, ioparm_delim, ioparm_delim_len,
			   p->delim);

  if (p->pad) set_string(&block, &post_block, ioparm_pad, ioparm_pad_len,
			 p->pad);

  if (p->iostat) set_parameter_ref(&block, ioparm_iostat, p->iostat);

  tmp = g95_build_function_call(iocall_open, NULL_TREE);
  g95_add_expr_to_block(&block, tmp);

  g95_add_block_to_block(&block, &post_block);

  io_result(&block, p->err, NULL, NULL);

  return g95_finish_block(&block);
}


/* g95_trans_close()-- Translate a CLOSE statement */

tree g95_trans_close(g95_code *code) {
stmtblock_t block, post_block;
g95_close *p;
tree tmp;

  g95_init_block(&block);
  g95_init_block(&post_block);

  set_error_locus(&block, &code->loc);
  p = code->ext.close;

  if (p->unit) set_parameter_value(&block, ioparm_unit, p->unit);

  if (p->status) set_string(&block, &post_block, ioparm_status,
			    ioparm_status_len, p->status);

  if (p->iostat) set_parameter_ref(&block, ioparm_iostat, p->iostat);

  tmp = g95_build_function_call(iocall_close, NULL_TREE);
  g95_add_expr_to_block(&block, tmp);

  g95_add_block_to_block(&block, &post_block);

  io_result(&block, p->err, NULL, NULL);

  return g95_finish_block(&block);
}


/* build_filepos()-- Common subroutine for building a file positioning
 * statement */

static tree build_filepos(tree function, g95_code *code) {
stmtblock_t block;
g95_filepos *p;
tree tmp;

  p = code->ext.filepos; 

  g95_init_block(&block);

  set_error_locus(&block, &code->loc);

  if (p->unit) set_parameter_value(&block, ioparm_unit, p->unit);

  if (p->iostat) set_parameter_ref(&block, ioparm_iostat, p->iostat);

  tmp = g95_build_function_call(function, NULL);
  g95_add_expr_to_block(&block, tmp);

  io_result(&block, p->err, NULL, NULL);

  return g95_finish_block(&block);
}


/* g95_trans_backspace()-- Translate a BACKSPACE statement */

tree g95_trans_backspace(g95_code *code) {

  return build_filepos(iocall_backspace, code);
}


/* g95_trans_endfile()-- Translate an ENDFILE statement */

tree g95_trans_endfile(g95_code *code) {

  return build_filepos(iocall_endfile, code);
}


/* g95_trans_rewind()-- Translate a REWIND statement */

tree g95_trans_rewind(g95_code *code) {

  return build_filepos(iocall_rewind, code);
}


/* g95_trans_inquire()-- Translate the non-IOLENGTH form of an INQUIRE
 * statement */

tree g95_trans_inquire(g95_code *code) {
stmtblock_t block, post_block;
g95_inquire *p;
tree tmp;

  g95_init_block(&block);
  g95_init_block(&post_block);

  set_error_locus(&block, &code->loc);
  p = code->ext.inquire;

  if (p->unit) set_parameter_value(&block, ioparm_unit, p->unit);

  if (p->file) set_string(&block, &post_block, ioparm_file, ioparm_file_len,
			  p->file);

  if (p->iostat) set_parameter_ref(&block, ioparm_iostat, p->iostat);

  if (p->exist) set_parameter_ref(&block, ioparm_exist, p->exist);

  if (p->opened) set_parameter_ref(&block, ioparm_opened, p->opened);

  if (p->number) set_parameter_ref(&block, ioparm_number, p->number);

  if (p->named) set_parameter_ref(&block, ioparm_named, p->named);

  if (p->name) set_string(&block, &post_block, ioparm_name, ioparm_name_len,
			  p->name);

  if (p->access) set_string(&block, &post_block, ioparm_access,
			    ioparm_access_len, p->access);

  if (p->sequential) set_string(&block, &post_block, ioparm_sequential,
				ioparm_sequential_len, p->sequential);

  if (p->direct) set_string(&block, &post_block, ioparm_direct,
			    ioparm_direct_len, p->direct);

  if (p->form) set_string(&block, &post_block, ioparm_form, ioparm_form_len,
			  p->form);

  if (p->formatted) set_string(&block, &post_block, ioparm_formatted,
			       ioparm_formatted_len, p->formatted);

  if (p->unformatted) set_string(&block, &post_block, ioparm_unformatted,
				 ioparm_unformatted_len, p->unformatted);

  if (p->recl) set_parameter_ref(&block, ioparm_recl_out, p->recl);

  if (p->nextrec) set_parameter_ref(&block, ioparm_nextrec, p->nextrec);

  if (p->blank) set_string(&block, &post_block, ioparm_blank, ioparm_blank_len,
			   p->blank);

  if (p->position) set_string(&block, &post_block, ioparm_position,
			      ioparm_position_len, p->position);

  if (p->action) set_string(&block, &post_block, ioparm_action,
			    ioparm_action_len, p->action);

  if (p->read) set_string(&block, &post_block, ioparm_read, ioparm_read_len,
			  p->read);

  if (p->write) set_string(&block, &post_block, ioparm_write,
			   ioparm_write_len, p->write);

  if (p->readwrite) set_string(&block, &post_block, ioparm_readwrite,
			       ioparm_readwrite_len, p->readwrite);

  if (p->delim) set_string(&block, &post_block, ioparm_delim, ioparm_delim_len,
			   p->delim);

  tmp = g95_build_function_call(iocall_inquire, NULL);
  g95_add_expr_to_block(&block, tmp);

  g95_add_block_to_block(&block, &post_block);

  io_result(&block, p->err, NULL, NULL);

  return g95_finish_block(&block);
}


/* g95_trans_iolength()-- Translate the IOLENGTH form of an INQUIRE
 * statement.  We treat this a third sort of data transfer statement,
 * except that lengths are summed instead of actually transfering any
 * data. */

tree g95_trans_iolength(g95_code *c) {

  return NULL_TREE;
}


/* build_dt()-- Create a data transfer statement.  Not all of the
 * fields are valid for both reading and writing, but improper use has
 * been filtered out by now. */

extern g95_st_label format_asterisk;

static tree build_dt(tree *function, g95_code *code) {
stmtblock_t block, post_block;
g95_expr *unity;
g95_dt *dt;
tree tmp;

  g95_init_block(&block); 
  g95_init_block(&post_block);

  set_error_locus(&block, &code->loc);
  dt = code->ext.dt;

  if (dt->io_unit) set_parameter_value(&block, ioparm_unit, dt->io_unit);

  if (dt->rec) set_parameter_ref(&block, ioparm_rec, dt->rec);

  if (dt->advance)
    set_string(&block, &post_block, ioparm_advance, ioparm_advance_len,
	       dt->advance);

  if (dt->format_expr)
    set_string(&block, &post_block, ioparm_format, ioparm_format_len,
	       dt->format_expr);

  if (dt->format_label) {   /* Create the format string and point to it */

  }

  if (dt->format_expr == NULL &&
      (dt->format_label == NULL || dt->format_label == &format_asterisk)) {
    unity = g95_int_expr(1);
    set_parameter_value(&block, ioparm_list_format, unity);
    g95_free_expr(unity);
  }

  if (dt->iostat) set_parameter_ref(&block, ioparm_iostat, dt->iostat);

  if (dt->size) set_parameter_ref(&block, ioparm_size, dt->size);

  tmp = g95_build_function_call(*function, NULL_TREE);
  g95_add_expr_to_block(&block, tmp);

  g95_add_block_to_block(&block, &post_block);

  return g95_finish_block(&block);
}


/* g95_trans_read()-- Translate a READ statement */

tree g95_trans_read(g95_code *code) {

  last_dt = READ;
  return build_dt(&iocall_read, code);
}


/* g95_trans_write()-- Translate a WRITE statement */

tree g95_trans_write(g95_code *code) {
 
  last_dt = WRITE;
  return build_dt(&iocall_write, code);
}


/* g95_trans_dt_end()-- Finish a data transfer statement */

tree g95_trans_dt_end(g95_code *code) {
tree function, tmp;
stmtblock_t block;

  g95_init_block(&block);

  function = (last_dt == READ) ? iocall_read_done : iocall_write_done;

  tmp = g95_build_function_call(function, NULL);
  g95_add_expr_to_block(&block, tmp);

  io_result(&block, code->ext.dt->err, code->ext.dt->end, code->ext.dt->eor);

  return g95_finish_block(&block);
}


/* transfer_expr()-- Generate the call for a scalar transfer node. */

static void transfer_expr(g95_se *se, g95_typespec *ts) {
tree args, tmp, function, arg2;
int kind;

  kind = ts->kind;
  function = NULL;
  arg2 = NULL;

  switch(ts->type) {
  case BT_INTEGER:
    arg2 = build_int_2(kind, 0);
    function = iocall_x_integer;
    break;

  case BT_REAL:
    arg2 = build_int_2(kind, 0);
    function = iocall_x_real;
    break;

  case BT_COMPLEX:
    arg2 = build_int_2(kind, 0);
    function = iocall_x_complex;
    break;

  case BT_LOGICAL:
    arg2 = build_int_2(kind, 0);
    function = iocall_x_logical;
    break;

  case BT_CHARACTER:
    arg2 = se->string_length;
    function = iocall_x_character;
    break;

  case BT_DERIVED:
    g95_todo_error("IO of derived types");

    /* Store the address to a temporary, then recurse for each element
     * of the type. */

    break;

  default:
    internal_error("Bad IO basetype (%d)", ts->type);
  }

  args = g95_chainon_list(NULL_TREE, se->expr);
  args = g95_chainon_list(args, arg2);

  tmp = g95_build_function_call(function, args);
  g95_add_expr_to_block(&se->pre, tmp);
  g95_add_block_to_block(&se->pre, &se->post);
}


/* g95_trans_transfer()-- Translate a TRANSFER code node */

tree g95_trans_transfer(g95_code *code) {
stmtblock_t block, body;
g95_loopinfo loop;
g95_expr *expr;
g95_ss *ss;
g95_se se;
tree tmp;

  g95_start_block(&block);

  expr = code->expr;
  ss = g95_walk_expr(g95_ss_terminator, expr);

  g95_init_se(&se, NULL);

  if (ss == g95_ss_terminator)
    g95_init_block(&body);
  else {
    /* Initialize the scalarizer.  */
    g95_init_loopinfo(&loop);
    g95_add_ss_to_loop(&loop, ss);

    /* Initialize the loop.  */
    g95_conv_ss_startstride(&loop);
    g95_conv_loop_setup(&loop);

    /* The main loop body.  */
    g95_mark_ss_chain_used(ss, 1);
    g95_start_scalarized_body(&loop, &body);

    g95_copy_loopinfo_to_se(&se, &loop);
    se.ss = ss;
  }

  g95_conv_expr_reference(&se, expr);

  transfer_expr(&se, &expr->ts);

  g95_add_block_to_block(&body, &se.pre);
  g95_add_block_to_block(&body, &se.post);

  if (se.ss == NULL)
    tmp = g95_finish_block(&body);
  else {
    assert(se.ss == g95_ss_terminator);
    g95_trans_scalarizing_loops(&loop, &body);

    g95_add_block_to_block(&loop.pre, &loop.post);

    tmp = g95_finish_block(&loop.pre);
  }

  g95_add_expr_to_block(&block, tmp);

  return g95_finish_block(&block);;
}

#include "gt-f95-trans-io.h"
