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
			  g95_default_integer);     
  DECL_EXTERNAL(locus_line) = 1;       
       
  locus_file = build_decl(VAR_DECL, get_identifier(PREFIX "filename"), 
			  pchar_type_node);         
  DECL_EXTERNAL(locus_file) = 1;  
}




/* set_string_parameter()-- Store a string parameter to the right
 * place. */  
  
static void set_string_parameter(stmtblock_t *block, stmtblock_t *postblock,          
				 tree var, tree len_var, g95_expr *d) { 
g95_se se;
tree tmp;      
      
  g95_init_se(&se, NULL);  
  
  se.reflevel = 1;        
  g95_conv_expr(&se, d);  
  
  g95_add_block_to_block(block, &se.pre);       
  g95_add_block_to_block(postblock, &se.post);

  tmp = build(COMPONENT_REF, TREE_TYPE(var), ioparm_var, var);   
  g95_add_modify_expr(block, tmp, se.expr);  
  
  tmp = build(COMPONENT_REF, TREE_TYPE(var), ioparm_var, len_var);        
  g95_add_modify_expr(block, tmp, se.string_length);      
}   
   
   
  
  
/* g95_add_field()-- Add a field to the ioparm structure. */    
    
tree g95_add_field(tree stype, char *name, tree type) {       
tree decl;      
      
  decl = build_decl(FIELD_DECL, get_identifier(name), type);       
       
  DECL_CONTEXT(decl) = stype;
  DECL_INITIAL(decl) = 0;         
  DECL_ALIGN(decl) = 0;     
  DECL_USER_ALIGN(decl) = 0;         
  TYPE_FIELDS(stype) = chainon(TYPE_FIELDS(stype), decl); 
 
  return decl; 
}      
      
      
  
  
/* g95_trans_iolength()-- Translate the IOLENGTH form of an INQUIRE
 * statement.  We treat this a third sort of data transfer statement,
 * except that lengths are summed instead of actually transfering any
 * data. */    
    
tree g95_trans_iolength(ATTRIBUTE_UNUSED g95_code *w) {     
     
  return NULL_TREE;   
}          
          
          
     
     
/* set_parameter_ref()-- Generate code to store an non-string I/O
 * parameter into the ioparm structure.  This is pass by reference. */   
   
static void set_parameter_ref(stmtblock_t *block, stmtblock_t *postblock,       
			      tree var, g95_expr *f) {         
g95_se se;  
tree tmp;    
    
  g95_init_se(&se, NULL);        
  se.reflevel = 1;  
  g95_conv_expr(&se, f);          
          
  g95_add_block_to_block(block, &se.pre);   
  g95_add_block_to_block(postblock, &se.post);         
         
  tmp = build(COMPONENT_REF, TREE_TYPE(var), ioparm_var, var);       
  g95_add_modify_expr(block, tmp, se.expr);       
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
       
       
       
       
/* set_flag()-- Set a member of the ioparm structure to one. */      
      
static void set_flag(stmtblock_t *block, tree var) {         
tree tmp;     
     
  tmp = build(COMPONENT_REF, TREE_TYPE(var), ioparm_var, var);   
  g95_add_modify_expr(block, tmp, integer_one_node); 
}


       
       
/* set_parameter_value()-- Generate code to store an non-string I/O
 * parameter into the ioparm structure.  This is a pass by value. */ 
 
static void set_parameter_value(stmtblock_t *block, stmtblock_t *post_block,      
				tree var, g95_expr *k) { 
g95_se se;   
tree tmp;      
      
  g95_init_se(&se, NULL);          
  g95_conv_expr_type(&se, k, TREE_TYPE(var));         
         
  g95_add_block_to_block(block, &se.pre);     
     
  tmp = build(COMPONENT_REF, TREE_TYPE(var), ioparm_var, var); 
  g95_add_modify_expr(block, tmp, se.expr); 
 
  g95_add_block_to_block(post_block, &se.post);     
}         
         
         
 
 
/* transfer_scalar_expr()-- Generate the call for a scalar transfer node. */        
        
static void transfer_scalar_expr(g95_se *se, g95_typespec *ts) {       
tree tmp, arg2;         
char *function;     
int kind;

  kind = ts->kind;         
  function = NULL;       
  arg2 = NULL;       
       
  switch(ts->type) {        
  case BT_INTEGER:
    arg2 = build_int_2(kind, 0);  
    function = PREFIX "transfer_integer";
    break;   
   
  case BT_REAL:          
    arg2 = build_int_2(kind, 0);         
    function = PREFIX "transfer_real";        
    break;     
     
  case BT_COMPLEX:     
    arg2 = build_int_2(kind, 0);    
    function = PREFIX "transfer_complex";    
    break;

  case BT_LOGICAL:          
    arg2 = build_int_2(kind, 0);  
    function = PREFIX "transfer_logical";          
    break;       
       
  case BT_CHARACTER:  
    arg2 = se->string_length;
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
       
  tmp = g95_call_library(void_type_node, function, se->expr, arg2, NULL_TREE);       
  g95_add_expr_to_block(&se->pre, tmp);    
  g95_add_block_to_block(&se->pre, &se->post);         
}         
         
         
       
       
/* build_filepos()-- Common subroutine for building a file positioning
 * statement */      
      
static tree build_filepos(char *function, g95_code *code) {         
stmtblock_t block, post_block;   
g95_filepos *e;        
tree tmp;  
  
  e = code->ext.filepos;          
         
  g95_init_block(&block);     
  g95_init_block(&post_block);        
        
  g95_set_error_locus(&block, &code->loc);        
        
  if (e->unit) set_parameter_value(&block, &post_block, ioparm_unit, e->unit);       
       
  if (e->iostat) set_parameter_ref(&block, &post_block, ioparm_iostat,        
				   e->iostat);    
    
  if (e->err) set_flag(&block, ioparm_err);        
        
  tmp = g95_call_library(void_type_node, function, NULL_TREE);          
  g95_add_expr_to_block(&block, tmp);         
  g95_add_block_to_block(&block, &post_block);

  io_result(&block, e->err, NULL, NULL);          
          
  return g95_finish_block(&block);      
}


      
      
/* transfer_array_expr()-- Generate the call for an array transfer node. */

static void transfer_array_expr(g95_se *se, g95_typespec *ts) {        
tree tmp, arg2;       
char *function; 
int kind; 
 
  kind = ts->kind;   
  function = NULL;  
  arg2 = NULL;      
      
  switch(ts->type) {         
  case BT_INTEGER:  
    arg2 = build_int_2(kind, 0);          
    function = PREFIX "transfer_integer_array";      
    break;          
          
  case BT_REAL:      
    arg2 = build_int_2(kind, 0);       
    function = PREFIX "transfer_real_array";     
    break;    
    
  case BT_COMPLEX:       
    arg2 = build_int_2(kind, 0);    
    function = PREFIX "transfer_complex_array";      
    break;     
     
  case BT_LOGICAL:
    arg2 = build_int_2(kind, 0);         
    function = PREFIX "transfer_logical_array";       
    break;     
     
  case BT_CHARACTER:     
    arg2 = se->string_length;       
    function = PREFIX "transfer_character_array";      
    break;   
   
  case BT_DERIVED:         
    g95_todo_error("IO of derived types");    
    
    /* Store the address to a temporary, then recurse for each element
     * of the type. */ 
 
    break;     
  default:
    internal_error("Bad IO basetype (%d)", ts->type);   
  }   
   
  tmp = g95_call_library(void_type_node, function, se->expr, arg2, NULL_TREE);  
  
  g95_add_expr_to_block(&se->pre, tmp);   
  g95_add_block_to_block(&se->pre, &se->post);  
}          
          
          
   
   
/* g95_set_runtime_locus()-- Store the current file and line number to
 * variables so that if a library call goes awry, we can tell the user
 * where the problem is. */     
     
void g95_set_error_locus(stmtblock_t *block, locus *where) {         
g95_file *c;        
tree tmp;        
int line;        
        
  c = where->file;         
  tmp = g95_build_string_const(strlen(c->filename)+1, c->filename);       
       
  tmp = build1(ADDR_EXPR, pchar_type_node, tmp);          
  g95_add_modify_expr(block, locus_file, tmp);      
      
  line = where->lp->start_line + where->line;     
  g95_add_modify_expr(block, locus_line, build_int_2(line, 0));          
}  
  
  
    
    
/* g95_trans_open()-- Translate an OPEN statement */

tree g95_trans_open(g95_code *code) {
stmtblock_t block, post_block;    
g95_open *u;         
tree tmp;  
  
  g95_init_block(&block);       
  g95_init_block(&post_block);  
  
  g95_set_error_locus(&block, &code->loc);       
  u = code->ext.open;

  if (u->unit) set_parameter_value(&block, &post_block, ioparm_unit, u->unit);     
     
  if (u->file)         
    set_string_parameter(&block, &post_block, ioparm_file, ioparm_file_len,    
			 u->file);     
     
  if (u->status)
    set_string_parameter(&block, &post_block, ioparm_status,      
			 ioparm_status_len, u->status);     
     
  if (u->access)        
    set_string_parameter(&block, &post_block, ioparm_access,          
			 ioparm_access_len, u->access);    
    
  if (u->form)       
    set_string_parameter(&block, &post_block, ioparm_form,         
			 ioparm_form_len, u->form);

  if (u->recl) set_parameter_ref(&block, &post_block, ioparm_recl_in, u->recl);        
        
  if (u->blank)
    set_string_parameter(&block, &post_block, ioparm_blank,        
			 ioparm_blank_len, u->blank);   
   
  if (u->position)          
    set_string_parameter(&block, &post_block, ioparm_position,      
			 ioparm_position_len, u->position);  
  
  if (u->action) 
    set_string_parameter(&block, &post_block, ioparm_action,     
			 ioparm_action_len, u->action);     
     
  if (u->delim)
    set_string_parameter(&block, &post_block, ioparm_delim,         
			 ioparm_delim_len, u->delim);          
          
  if (u->pad)          
    set_string_parameter(&block, &post_block, ioparm_pad,        
			 ioparm_pad_len, u->pad);      
      
  if (u->iostat) set_parameter_ref(&block, &post_block, ioparm_iostat,         
				   u->iostat);        
        
  if (u->err) set_flag(&block, ioparm_err);       
       
  tmp = g95_call_library(void_type_node, PREFIX "st_open", NULL_TREE); 
  g95_add_expr_to_block(&block, tmp);          
          
  g95_add_block_to_block(&block, &post_block);          
          
  io_result(&block, u->err, NULL, NULL);       
       
  return g95_finish_block(&block);     
}    
    
    
         
         
/* g95_trans_endfile()-- Translate an ENDFILE statement */       
       
tree g95_trans_endfile(g95_code *code) {  
  
  return build_filepos(PREFIX "st_endfile", code);      
} 
 
 
     
     
/* g95_trans_transfer()-- Translate a TRANSFER code node */

tree g95_trans_transfer(g95_code *code) {         
stmtblock_t body;   
g95_expr *expr;    
g95_se se;    
    
  expr = code->expr; 
 
  g95_init_block(&body);  
  
  g95_init_se(&se, NULL); 
  se.reflevel = 1;
  g95_conv_expr(&se, expr);        
        
  if (expr->rank == 0)       
    transfer_scalar_expr(&se, &expr->ts);    
  else 
    transfer_array_expr(&se, &expr->ts);    
    
  g95_add_block_to_block(&body, &se.pre); 
  g95_add_block_to_block(&body, &se.post);   
   
  return g95_finish_block(&body);    
}     
     
     


/* g95_trans_backspace()-- Translate a BACKSPACE statement */   
   
tree g95_trans_backspace(g95_code *code) {  
  
  return build_filepos(PREFIX "st_backspace", code);    
}        
        
        


/* g95_trans_rewind()-- Translate a REWIND statement */   
   
tree g95_trans_rewind(g95_code *code) {       
       
  return build_filepos(PREFIX "st_rewind", code);        
}    
    
    
       
       
/* g95_trans_inquire()-- Translate the non-IOLENGTH form of an INQUIRE
 * statement */   
   
tree g95_trans_inquire(g95_code *code) {          
stmtblock_t block, post_block;     
g95_inquire *x;
tree tmp;       
       
  g95_init_block(&block);          
  g95_init_block(&post_block);    
    
  g95_set_error_locus(&block, &code->loc);         
  x = code->ext.inquire;     
     
  if (x->unit) set_parameter_value(&block, &post_block, ioparm_unit, x->unit);          
          
  if (x->file) set_parameter_ref(&block, &post_block, ioparm_file, x->file);

  if (x->iostat)        
    set_parameter_ref(&block, &post_block, ioparm_iostat, x->iostat);         
         
  if (x->exist) set_parameter_ref(&block, &post_block, ioparm_exist, x->exist);  
  
  if (x->opened)         
    set_parameter_ref(&block, &post_block, ioparm_opened, x->opened);  
  
  if (x->number)    
    set_parameter_ref(&block, &post_block, ioparm_number, x->number);          
          
  if (x->named) set_parameter_ref(&block, &post_block, ioparm_named, x->named);     
     
  if (x->name)          
    set_string_parameter(&block, &post_block, ioparm_name,         
			 ioparm_name_len, x->name);       
       
  if (x->access)
    set_string_parameter(&block, &post_block, ioparm_access,
			 ioparm_access_len, x->access);

  if (x->sequential)          
    set_string_parameter(&block, &post_block, ioparm_sequential,      
			 ioparm_sequential_len, x->sequential); 
 
  if (x->direct)   
    set_string_parameter(&block, &post_block, ioparm_direct,    
			 ioparm_direct_len, x->direct);    
    
  if (x->form)       
    set_string_parameter(&block, &post_block, ioparm_form,      
			 ioparm_form_len, x->form);  
  
  if (x->formatted)          
    set_string_parameter(&block, &post_block, ioparm_formatted,    
			 ioparm_formatted_len, x->formatted);         
         
  if (x->unformatted)
    set_string_parameter(&block, &post_block, ioparm_unformatted, 
			 ioparm_unformatted_len, x->unformatted);  
  
  if (x->recl)         
    set_parameter_ref(&block, &post_block, ioparm_recl_out, x->recl);         
         
  if (x->nextrec)         
    set_parameter_ref(&block, &post_block, ioparm_nextrec, x->nextrec);      
      
  if (x->blank)     
    set_string_parameter(&block, &post_block, ioparm_blank,         
			 ioparm_blank_len, x->blank);        
        
  if (x->position)       
    set_string_parameter(&block, &post_block, ioparm_position,          
			 ioparm_position_len, x->position); 
 
  if (x->action)        
    set_string_parameter(&block, &post_block, ioparm_action, 
			 ioparm_action_len, x->action); 
 
  if (x->read)        
    set_string_parameter(&block, &post_block, ioparm_read,         
			 ioparm_read_len, x->read);      
      
  if (x->write)          
    set_string_parameter(&block, &post_block, ioparm_write,       
			 ioparm_write_len, x->write);      
      
  if (x->readwrite) 
    set_string_parameter(&block, &post_block, ioparm_readwrite, 
			 ioparm_readwrite_len, x->readwrite);

  if (x->delim)       
    set_string_parameter(&block, &post_block, ioparm_delim, 
			 ioparm_delim_len, x->delim);       
       
  if (x->err) set_flag(&block, ioparm_err);      
      
  tmp = g95_call_library(void_type_node, PREFIX "st_inquire", NULL_TREE);        
  g95_add_expr_to_block(&block, tmp);         
         
  g95_add_block_to_block(&block, &post_block);   
   
  io_result(&block, x->err, NULL, NULL);   
   
  return g95_finish_block(&block);        
}        
        
        
          
          
/* g95_trans_close()-- Translate a CLOSE statement */       
       
tree g95_trans_close(g95_code *code) {  
stmtblock_t block, post_block;
g95_close *d;         
tree tmp;     
     
  g95_init_block(&block);
  g95_init_block(&post_block);      
      
  g95_set_error_locus(&block, &code->loc);         
  d = code->ext.close; 
 
  if (d->unit) set_parameter_value(&block, &post_block, ioparm_unit, d->unit);    
    
  if (d->status) set_parameter_ref(&block, &post_block, ioparm_status,          
				   d->status);       
       
  if (d->iostat) set_parameter_ref(&block, &post_block, ioparm_iostat,         
				   d->iostat);      
      
  if (d->err) set_flag(&block, ioparm_err); 
 
  tmp = g95_call_library(void_type_node, PREFIX "st_close", NULL_TREE);      
  g95_add_expr_to_block(&block, tmp);  
  
  g95_add_block_to_block(&block, &post_block);       
       
  io_result(&block, d->err, NULL, NULL);      
      
  return g95_finish_block(&block);          
}        
        
        
     
     
/* build_dt()-- Create a data transfer statement.  Not all of the
 * fields are valid for both reading and writing, but improper use has
 * been filtered out by now. */     
     
static tree build_dt(char *function, g95_code *code) {    
stmtblock_t block, post_block;    
g95_dt *dt;         
tree tmp;         
         
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
          
  tmp = g95_call_library(void_type_node, function, NULL_TREE);         
  g95_add_expr_to_block(&block, tmp);          
  g95_add_block_to_block(&block, &post_block);   
   
  return g95_finish_block(&block); 
}    
    
    
  
  
/* g95_trans_dt_end()-- Finish a data transfer statement */      
      
tree g95_trans_dt_end(g95_code *code) {         
stmtblock_t block;       
char *function;    
tree tmp;    
    
  g95_init_block(&block);    
    
  function = (last_dt == READ)  
    ? PREFIX "st_read_done"      
    : PREFIX "st_write_done"; 
 
  tmp = g95_call_library(void_type_node, function, NULL_TREE);  
  g95_add_expr_to_block(&block, tmp);   
   
  io_result(&block, code->ext.dt->err, code->ext.dt->end, code->ext.dt->eor);          
          
  return g95_finish_block(&block);      
}  
  
  
   
   
/* g95_trans_read()-- Translate a READ statement */  
  
tree g95_trans_read(g95_code *code) {  
  
  last_dt = READ;    
  return build_dt(PREFIX "st_read", code);      
}   
   
   
     
     
/* g95_trans_write()-- Translate a WRITE statement */ 
 
tree g95_trans_write(g95_code *code) {
 
  last_dt = WRITE;      
  return build_dt(PREFIX "st_write", code);      
}   
   
   
     
     
#include "gt-f95-trans-io.h"
