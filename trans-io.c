/* IO Code translation/library interface
   Copyright (C) 2000, 2001, 2002 Free Software Foundation, Inc.
   Contributed by Paul Brook

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
        
/* trans-io.c-- generate GCC trees from g95_code */  
  
#include "trans.h"
          
          
static GTY(()) tree default_integer_pointer;          
          
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
static GTY(()) tree ioparm_filename;      
static GTY(()) tree ioparm_filename_len;          
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
        
#define ADD_FIELD(name, type) \
  ioparm_ ## name = g95_add_field(ioparm_type, stringize(name), type)
          
#define ADD_STRING(name) \
 ioparm_ ## name = \
    g95_add_field(ioparm_type, stringize(name), pchar_type_node); \
 ioparm_ ## name ## _len = \
    g95_add_field(ioparm_type, stringize(name ## _len), g95_default_integer);
      
/* Variable for keeping track of what the last data transfer statement
 * was.  Used for deciding which subroutine to call when the data
 * transfer is complete. */       
       
static enum { READ, WRITE } last_dt;


     
     
/* set_parameter_ref()-- Generate code to store an non-string I/O
 * parameter into the ioparm structure.  This is pass by reference. */       
       
static void set_parameter_ref(stmtblock_t *blk, stmtblock_t *postblock,  
			      tree v, g95_expr *i) {      
g95_se se;        
tree t;          
          
  g95_init_se(&se, NULL);   
  se.reflevel = 1;         
  g95_conv_expr(&se, i);         
         
  g95_add_block_to_block(blk, &se.pre);       
  g95_add_block_to_block(postblock, &se.post);          
          
  t = build(COMPONENT_REF, TREE_TYPE(v), ioparm_var, v);
  g95_add_modify_expr(blk, t, se.expr);     
}    
    
    
 
 
/* g95_set_runtime_locus()-- Store the current file and line number to
 * variables so that if a library call goes awry, we can tell the user
 * where the problem is. */  
  
void g95_set_error_locus(stmtblock_t *body, locus *old_loc) {         
g95_file *m;         
tree tmp1;
int line;      
      
  m = old_loc->file;     
  tmp1 = g95_build_string_const(strlen(m->filename)+1, m->filename);     
     
  tmp1 = build1(ADDR_EXPR, pchar_type_node, tmp1); 
  g95_add_modify_expr(body, locus_file, tmp1);        
        
  line = old_loc->lp->start_line + old_loc->line;  
  g95_add_modify_expr(body, locus_line, build_int_2(line, 0));    
}


        
        
/* set_string_parameter()-- Store a string parameter to the right
 * place. */ 
 
static void set_string_parameter(stmtblock_t *list, stmtblock_t *postblock,
				 tree var, tree len_var, g95_expr *l) {   
g95_se s;  
tree tmp; 
 
  g95_init_se(&s, NULL);    
    
  s.reflevel = 1;   
  g95_conv_expr(&s, l);          
          
  g95_add_block_to_block(list, &s.pre);
  g95_add_block_to_block(postblock, &s.post);

  tmp = build(COMPONENT_REF, TREE_TYPE(var), ioparm_var, var);    
  g95_add_modify_expr(list, tmp, s.expr);

  tmp = build(COMPONENT_REF, TREE_TYPE(var), ioparm_var, len_var);       
  g95_add_modify_expr(list, tmp, s.string_length);       
}         
         
         
     
     
/* set_parameter_value()-- Generate code to store an non-string I/O
 * parameter into the ioparm structure.  This is a pass by value. */

static void set_parameter_value(stmtblock_t *list, stmtblock_t *post_block,
				tree variable, g95_expr *h) {        
g95_se se0;      
tree tmp0;  
  
  g95_init_se(&se0, NULL);    
  g95_conv_expr_type(&se0, h, TREE_TYPE(variable)); 
 
  g95_add_block_to_block(list, &se0.pre);  
  
  tmp0 = build(COMPONENT_REF, TREE_TYPE(variable), ioparm_var, variable);          
  g95_add_modify_expr(list, tmp0, se0.expr);  
  
  g95_add_block_to_block(post_block, &se0.post); 
}      
      
      
      
      
/* transfer_scalar_expr()-- Generate the call for a scalar transfer node. */

static void transfer_scalar_expr(g95_se *se1, g95_typespec *ts) {  
tree tmp1, arg2;     
char *function;        
int k;       
       
  k = ts->kind;    
  function = NULL;       
  arg2 = NULL;      
      
  switch(ts->type) {    
  case BT_INTEGER:         
    arg2 = build_int_2(k, 0);          
    function = PREFIX "transfer_integer";       
    break;    
    
  case BT_REAL:    
    arg2 = build_int_2(k, 0);         
    function = PREFIX "transfer_real";        
    break;   
   
  case BT_COMPLEX:     
    arg2 = build_int_2(k, 0);   
    function = PREFIX "transfer_complex";  
    break;      
      
  case BT_LOGICAL:         
    arg2 = build_int_2(k, 0);    
    function = PREFIX "transfer_logical";
    break;    
    
  case BT_CHARACTER:        
    arg2 = se1->string_length;    
    function = PREFIX "transfer_character";   
    break;

  case BT_DERIVED:     
    g95_todo_error("IO of derived types");  
  
    /* Store the address to a temporary, then recurse for each element
     * of the type. */  
  
    break;      
  default:   
    internal_error("Bad IO basetype (%d)", ts->type);     
  } 
 
  tmp1 = g95_call_library(void_type_node, function, se1->expr, arg2, NULL_TREE);          
  g95_add_expr_to_block(&se1->pre, tmp1);  
  g95_add_block_to_block(&se1->pre, &se1->post);          
}


       
       
/* set_flag()-- Set a member of the ioparm structure to one. */        
        
static void set_flag(stmtblock_t *body, tree var0) {    
tree tmp0; 
 
  tmp0 = build(COMPONENT_REF, TREE_TYPE(var0), ioparm_var, var0);   
  g95_add_modify_expr(body, tmp0, integer_one_node);          
}   
   
   
         
/* g95_build_io_library_fndecls()-- Create function decls for IO
 * library functions.  */ 
 
void g95_build_io_library_fndecls(void) {         
tree ioparm_type;         
         
  default_integer_pointer = build_pointer_type(g95_default_integer);       
       
/* Build the st_parameter structure.  Information associated with I/O
 * calls are transferred here.  This must match the one defined in the
 * library exactly. */ 
 
  ioparm_type = make_node(RECORD_TYPE);       
  TYPE_NAME(ioparm_type) = get_identifier(PREFIX "ioparm"); 
 
  ADD_FIELD(unit,            g95_default_integer);     
  ADD_FIELD(err,             g95_default_integer);         
  ADD_FIELD(end,             g95_default_integer);
  ADD_FIELD(eor,             g95_default_integer);         
  ADD_FIELD(list_format,     g95_default_integer);        
  ADD_FIELD(library_return,  g95_default_integer);  
  
  ADD_FIELD(iostat,          default_integer_pointer);          
  ADD_FIELD(exist,           default_integer_pointer); 
  ADD_FIELD(opened,          default_integer_pointer);         
  ADD_FIELD(number,          default_integer_pointer);    
  ADD_FIELD(named,           default_integer_pointer);          
  ADD_FIELD(rec,             default_integer_pointer);         
  ADD_FIELD(nextrec,         default_integer_pointer);          
  ADD_FIELD(size,            default_integer_pointer);

  ADD_FIELD(recl_in,         default_integer_pointer);
  ADD_FIELD(recl_out,        default_integer_pointer);         
         
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
  ADD_STRING(filename);      
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
			  g95_default_integer);   
  DECL_EXTERNAL(locus_line) = 1;          
          
  locus_file = build_decl(VAR_DECL, get_identifier(PREFIX "filename"),       
			  pchar_type_node); 
  DECL_EXTERNAL(locus_file) = 1;          
}         
         
         
        
        
/* add_case()-- Add a case to a IO-result switch */         
         
static void add_case(int label_value, g95_st_label *labl, stmtblock_t *blk) {    
tree t, value;         
         
  if (labl == NULL) return;   /* No label, no case */    
    
  value = build_int_2(label_value, 0);      
      
  t = build_v(CASE_LABEL_EXPR, value, NULL_TREE);    
  g95_add_expr_to_block(blk, t);         
         
  t = build_v(GOTO_EXPR, g95_get_label_decl(labl));          
  g95_add_expr_to_block(blk, t);   
}     
     
     
 
 
/* g95_add_field()-- Add a field to the ioparm structure. */        
        
tree g95_add_field(tree stype, char *name0, tree typ) {
tree declr;    
    
  declr = build_decl(FIELD_DECL, get_identifier(name0), typ);    
    
  DECL_CONTEXT(declr) = stype;        
  DECL_INITIAL(declr) = 0;      
  DECL_ALIGN(declr) = 0;   
  DECL_USER_ALIGN(declr) = 0;
  TYPE_FIELDS(stype) = chainon(TYPE_FIELDS(stype), declr);     
     
  return declr;
}    
    
    
          
          
/* io_result()-- Generate a switch statement that branches to the
 * correct I/O result label.  The last statement of an I/O call stores
 * the result into a variable because there is often cleanup that must
 * be done before the switch, so a temporary would have to be created
 * anyway. */       
       
static void io_result(stmtblock_t *blk, g95_st_label *err_label,
		      g95_st_label *end_label, g95_st_label *eor_label) {   
stmtblock_t list; 
tree tmp0, rc;      
      
  /* If no labels are specified, ignore the result instead of building
   * an empty switch. */ 
 
  if (err_label == NULL && end_label == NULL && eor_label == NULL) return;        
        
  /* Build a switch statement */  
  
  g95_start_block(&list);     
     
  /* The label values here must be the same as the values in the
   * library_return enum in the runtime library */

  add_case(1, err_label, &list);   
  add_case(2, end_label, &list); 
  add_case(3, eor_label, &list); 
 
  tmp0 = g95_finish_block(&list);      
      
  rc = build(COMPONENT_REF, TREE_TYPE(ioparm_library_return), ioparm_var,         
	     ioparm_library_return);         
         
  tmp0 = build_v(SWITCH_EXPR, rc, tmp0, NULL_TREE);     
     
  g95_add_expr_to_block(blk, tmp0); 
}          
          
          
  
  
/* build_dt()-- Create a data transfer statement.  Not all of the
 * fields are valid for both reading and writing, but improper use has
 * been filtered out by now. */        
        
static tree build_dt(char *function, g95_code *code) {          
stmtblock_t block, post_block;         
g95_dt *dt; 
tree tmp1;      
      
  g95_init_block(&block);          
  g95_init_block(&post_block);    
    
  g95_set_error_locus(&block, &code->loc);         
  dt = code->ext.dt; 
 
  if (dt->io_unit) {      
    if (dt->io_unit->ts.type == BT_INTEGER)         
      set_parameter_value(&block, &post_block, ioparm_unit, dt->io_unit);       
    else          
      set_string_parameter(&block, &post_block, ioparm_internal_unit,         
			   ioparm_internal_unit_len, dt->io_unit);    
  }          
          
  if (dt->rec) set_parameter_ref(&block, &post_block, ioparm_rec, dt->rec);          
          
  if (dt->advance)     
    set_parameter_ref(&block, &post_block, ioparm_advance, dt->advance);        
        
  if (dt->format_expr)    
    set_string_parameter(&block, &post_block, ioparm_format,  
			 ioparm_format_len, dt->format_expr);   
   
  if (dt->format_label) {  
    if (dt->format_label == &g95_format_asterisk)     
      set_flag(&block, ioparm_list_format); 
    else          
      set_string_parameter(&block, &post_block, ioparm_format,     
			   ioparm_format_len, dt->format_label->format);         
  } 
 
  if (dt->iostat)         
    set_parameter_ref(&block, &post_block, ioparm_iostat, dt->iostat);      
      
  if (dt->size) set_parameter_ref(&block, &post_block, ioparm_size, dt->size);       
       
  if (dt->err) set_flag(&block, ioparm_err);    
    
  if (dt->eor) set_flag(&block, ioparm_eor);  
  
  if (dt->end) set_flag(&block, ioparm_end);     
     
  tmp1 = g95_call_library(void_type_node, function, NULL_TREE);       
  g95_add_expr_to_block(&block, tmp1);  
  g95_add_block_to_block(&block, &post_block);        
        
  return g95_finish_block(&block);   
}  
  
  
  
  
/* g95_trans_read()-- Translate a READ statement */       
       
tree g95_trans_read(g95_code *codep) {      
      
  last_dt = READ;    
  return build_dt(PREFIX "st_read", codep);          
}      
      
      
          
          
/* g95_trans_close()-- Translate a CLOSE statement */    
    
tree g95_trans_close(g95_code *cp) {    
stmtblock_t body, post_block;  
g95_close *a;       
tree t;   
   
  g95_init_block(&body);        
  g95_init_block(&post_block);       
       
  g95_set_error_locus(&body, &cp->loc);    
  a = cp->ext.close;       
       
  if (a->unit) set_parameter_value(&body, &post_block, ioparm_unit, a->unit);         
         
  if (a->status) set_parameter_ref(&body, &post_block, ioparm_status, 
				   a->status);      
      
  if (a->iostat) set_parameter_ref(&body, &post_block, ioparm_iostat,       
				   a->iostat);         
         
  if (a->err) set_flag(&body, ioparm_err); 
 
  t = g95_call_library(void_type_node, PREFIX "st_close", NULL_TREE);          
  g95_add_expr_to_block(&body, t);         
         
  g95_add_block_to_block(&body, &post_block);        
        
  io_result(&body, a->err, NULL, NULL);     
     
  return g95_finish_block(&body);    
}  
  
  
  
  
/* build_filepos()-- Common subroutine for building a file positioning
 * statement */

static tree build_filepos(char *function, g95_code *c) {     
stmtblock_t body, post_block;        
g95_filepos *v;      
tree t;         
         
  v = c->ext.filepos;       
      
  g95_init_block(&body);  
  g95_init_block(&post_block);     
     
  g95_set_error_locus(&body, &c->loc);  
  
  if (v->unit) set_parameter_value(&body, &post_block, ioparm_unit, v->unit);  
  
  if (v->iostat) set_parameter_ref(&body, &post_block, ioparm_iostat,    
				   v->iostat); 
 
  if (v->err) set_flag(&body, ioparm_err);    
    
  t = g95_call_library(void_type_node, function, NULL_TREE);   
  g95_add_expr_to_block(&body, t); 
  g95_add_block_to_block(&body, &post_block); 
 
  io_result(&body, v->err, NULL, NULL);        
        
  return g95_finish_block(&body);         
}        
        
        
   
   
/* g95_trans_open()-- Translate an OPEN statement */       
       
tree g95_trans_open(g95_code *codep) {
stmtblock_t blk, post_block;
g95_open *w;       
tree tmp0;      
      
  g95_init_block(&blk);       
  g95_init_block(&post_block);   
   
  g95_set_error_locus(&blk, &codep->loc);      
  w = codep->ext.open; 
 
  if (w->unit) set_parameter_value(&blk, &post_block, ioparm_unit, w->unit);  
  
  if (w->file)      
    set_string_parameter(&blk, &post_block, ioparm_file, ioparm_file_len,         
			 w->file);         
         
  if (w->status)  
    set_string_parameter(&blk, &post_block, ioparm_status, 
			 ioparm_status_len, w->status);      
      
  if (w->access) 
    set_string_parameter(&blk, &post_block, ioparm_access, 
			 ioparm_access_len, w->access);         
         
  if (w->form)          
    set_string_parameter(&blk, &post_block, ioparm_form,     
			 ioparm_form_len, w->form);        
        
  if (w->recl) set_parameter_ref(&blk, &post_block, ioparm_recl_in, w->recl);       
       
  if (w->blank)
    set_string_parameter(&blk, &post_block, ioparm_blank,       
			 ioparm_blank_len, w->blank); 
 
  if (w->position)  
    set_string_parameter(&blk, &post_block, ioparm_position,        
			 ioparm_position_len, w->position);     
     
  if (w->action)
    set_string_parameter(&blk, &post_block, ioparm_action,      
			 ioparm_action_len, w->action);  
  
  if (w->delim)        
    set_string_parameter(&blk, &post_block, ioparm_delim,        
			 ioparm_delim_len, w->delim);    
    
  if (w->pad)      
    set_string_parameter(&blk, &post_block, ioparm_pad,        
			 ioparm_pad_len, w->pad); 
 
  if (w->iostat) set_parameter_ref(&blk, &post_block, ioparm_iostat,      
				   w->iostat);         
         
  if (w->err) set_flag(&blk, ioparm_err);      
      
  tmp0 = g95_call_library(void_type_node, PREFIX "st_open", NULL_TREE);        
  g95_add_expr_to_block(&blk, tmp0);        
        
  g95_add_block_to_block(&blk, &post_block);         
         
  io_result(&blk, w->err, NULL, NULL);    
    
  return g95_finish_block(&blk);    
}         
         
         
          
          
/* g95_trans_inquire()-- Translate the non-IOLENGTH form of an INQUIRE
 * statement */    
    
tree g95_trans_inquire(g95_code *cp) {    
stmtblock_t b, post_block;  
g95_inquire *o; 
tree tmp;       
       
  g95_init_block(&b);         
  g95_init_block(&post_block);      
      
  g95_set_error_locus(&b, &cp->loc);        
  o = cp->ext.inquire; 
 
  if (o->unit) set_parameter_value(&b, &post_block, ioparm_unit, o->unit);      
      
  if (o->file) set_parameter_ref(&b, &post_block, ioparm_file, o->file);    
    
  if (o->iostat)  
    set_parameter_ref(&b, &post_block, ioparm_iostat, o->iostat);    
    
  if (o->exist) set_parameter_ref(&b, &post_block, ioparm_exist, o->exist);      
      
  if (o->opened) 
    set_parameter_ref(&b, &post_block, ioparm_opened, o->opened);        
        
  if (o->number)       
    set_parameter_ref(&b, &post_block, ioparm_number, o->number);

  if (o->named) set_parameter_ref(&b, &post_block, ioparm_named, o->named);  
  
  if (o->name)   
    set_string_parameter(&b, &post_block, ioparm_filename,         
			 ioparm_filename_len, o->name);      
      
  if (o->access)   
    set_string_parameter(&b, &post_block, ioparm_access,     
			 ioparm_access_len, o->access);       
       
  if (o->sequential)
    set_string_parameter(&b, &post_block, ioparm_sequential,         
			 ioparm_sequential_len, o->sequential);   
   
  if (o->direct)  
    set_string_parameter(&b, &post_block, ioparm_direct,          
			 ioparm_direct_len, o->direct);    
    
  if (o->form)   
    set_string_parameter(&b, &post_block, ioparm_form,
			 ioparm_form_len, o->form);  
  
  if (o->formatted)    
    set_string_parameter(&b, &post_block, ioparm_formatted, 
			 ioparm_formatted_len, o->formatted);   
   
  if (o->unformatted)
    set_string_parameter(&b, &post_block, ioparm_unformatted,     
			 ioparm_unformatted_len, o->unformatted);       
       
  if (o->recl)          
    set_parameter_ref(&b, &post_block, ioparm_recl_out, o->recl);     
     
  if (o->nextrec)     
    set_parameter_ref(&b, &post_block, ioparm_nextrec, o->nextrec);       
       
  if (o->blank)         
    set_string_parameter(&b, &post_block, ioparm_blank,          
			 ioparm_blank_len, o->blank);        
        
  if (o->position)    
    set_string_parameter(&b, &post_block, ioparm_position,        
			 ioparm_position_len, o->position);          
          
  if (o->action)       
    set_string_parameter(&b, &post_block, ioparm_action,
			 ioparm_action_len, o->action);         
         
  if (o->read)      
    set_string_parameter(&b, &post_block, ioparm_read,          
			 ioparm_read_len, o->read);    
    
  if (o->write)       
    set_string_parameter(&b, &post_block, ioparm_write,      
			 ioparm_write_len, o->write);  
  
  if (o->readwrite)   
    set_string_parameter(&b, &post_block, ioparm_readwrite,     
			 ioparm_readwrite_len, o->readwrite);    
    
  if (o->delim)          
    set_string_parameter(&b, &post_block, ioparm_delim,  
			 ioparm_delim_len, o->delim);   
   
  if (o->err) set_flag(&b, ioparm_err);        
        
  tmp = g95_call_library(void_type_node, PREFIX "st_inquire", NULL_TREE);      
  g95_add_expr_to_block(&b, tmp);   
   
  g95_add_block_to_block(&b, &post_block);          
          
  io_result(&b, o->err, NULL, NULL);       
       
  return g95_finish_block(&b);
} 
 
 
       
       
/* transfer_array_expr()-- Generate the call for an array transfer node. */       
       
static void transfer_array_expr(g95_se *s, g95_typespec *typesp) {       
tree tmp, arg2; 
char *function;  
int k0;   
   
  k0 = typesp->kind;
  function = NULL;       
  arg2 = NULL;      
      
  switch(typesp->type) {          
  case BT_INTEGER:        
    arg2 = build_int_2(k0, 0);     
    function = PREFIX "transfer_integer_array";      
    break;    
    
  case BT_REAL:      
    arg2 = build_int_2(k0, 0);      
    function = PREFIX "transfer_real_array"; 
    break;      
      
  case BT_COMPLEX:  
    arg2 = build_int_2(k0, 0);     
    function = PREFIX "transfer_complex_array";       
    break;    
    
  case BT_LOGICAL:    
    arg2 = build_int_2(k0, 0);         
    function = PREFIX "transfer_logical_array";  
    break;

  case BT_CHARACTER:          
    arg2 = s->string_length;          
    function = PREFIX "transfer_character_array"; 
    break;      
      
  case BT_DERIVED:
    g95_todo_error("IO of derived types");      
      
    /* Store the address to a temporary, then recurse for each element
     * of the type. */ 
 
    break;    
  default:      
    internal_error("Bad IO basetype (%d)", typesp->type);      
  }      
      
  tmp = g95_call_library(void_type_node, function, s->expr, arg2, NULL_TREE);

  g95_add_expr_to_block(&s->pre, tmp);     
  g95_add_block_to_block(&s->pre, &s->post);     
}


      
      
/* g95_trans_endfile()-- Translate an ENDFILE statement */   
   
tree g95_trans_endfile(g95_code *codep) {      
      
  return build_filepos(PREFIX "st_endfile", codep);
}  
  
  
        
        
/* g95_trans_iolength()-- Translate the IOLENGTH form of an INQUIRE
 * statement.  We treat this a third sort of data transfer statement,
 * except that lengths are summed instead of actually transfering any
 * data. */  
  
tree g95_trans_iolength(ATTRIBUTE_UNUSED g95_code *x) {   
   
  return NULL_TREE;  
}       
       
       
          
          
/* g95_trans_transfer()-- Translate a TRANSFER code node */  
  
tree g95_trans_transfer(g95_code *codep) {
stmtblock_t body;
g95_expr *exp; 
g95_se se;  
  
  exp = codep->expr;  
  
  g95_init_block(&body);       
       
  g95_init_se(&se, NULL);         
  se.reflevel = 1;         
  g95_conv_expr(&se, exp);  
  
  if (exp->rank == 0)          
    transfer_scalar_expr(&se, &exp->ts);     
  else      
    transfer_array_expr(&se, &exp->ts);    
    
  g95_add_block_to_block(&body, &se.pre);  
  g95_add_block_to_block(&body, &se.post);      
      
  return g95_finish_block(&body);       
} 
 
 
       
       
/* g95_trans_write()-- Translate a WRITE statement */     
     
tree g95_trans_write(g95_code *cp) {
 
  last_dt = WRITE;     
  return build_dt(PREFIX "st_write", cp);   
}     
     
     
       
       
/* g95_trans_backspace()-- Translate a BACKSPACE statement */     
     
tree g95_trans_backspace(g95_code *c) {    
    
  return build_filepos(PREFIX "st_backspace", c);          
}     
     
     


/* g95_trans_rewind()-- Translate a REWIND statement */         
         
tree g95_trans_rewind(g95_code *c) {          
          
  return build_filepos(PREFIX "st_rewind", c);      
}          
          
          
 
 
/* g95_trans_dt_end()-- Finish a data transfer statement */   
   
tree g95_trans_dt_end(g95_code *codep) { 
stmtblock_t b;      
char *function;       
tree tmp;         
         
  g95_init_block(&b);  
  
  function = (last_dt == READ)     
    ? PREFIX "st_read_done"   
    : PREFIX "st_write_done";          
          
  tmp = g95_call_library(void_type_node, function, NULL_TREE);          
  g95_add_expr_to_block(&b, tmp);       
       
  io_result(&b, codep->ext.dt->err, codep->ext.dt->end, codep->ext.dt->eor);         
         
  return g95_finish_block(&b);  
}     
     
     
        
        
#include "gt-f95-trans-io.h"
