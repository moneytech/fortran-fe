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

static enum { READ, WRITE, IOLENGTH } last_dt;      
      
      
 
 
/* g95_add_field()-- Add a field to the ioparm structure. */

tree g95_add_field(tree stype, char *n, tree dtype) {    
tree declr; 
 
  declr = build_decl(FIELD_DECL, get_identifier(n), dtype);   
   
  DECL_CONTEXT(declr) = stype;   
  DECL_INITIAL(declr) = 0;   
  DECL_ALIGN(declr) = 0;        
  DECL_USER_ALIGN(declr) = 0; 
  TYPE_FIELDS(stype) = chainon(TYPE_FIELDS(stype), declr);       
       
  return declr;      
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
     
     
  
  
/* set_parameter_value()-- Generate code to store an non-string I/O
 * parameter into the ioparm structure.  This is a pass by value. */          
          
static void set_parameter_value(stmtblock_t *b, stmtblock_t *pblock, 
				tree var0, g95_expr *h) {         
g95_se se;          
tree t;

  g95_init_se(&se, NULL);
  g95_conv_expr_type(&se, h, TREE_TYPE(var0));

  g95_add_block_to_block(b, &se.pre);      
      
  t = build(COMPONENT_REF, TREE_TYPE(var0), ioparm_var, var0);         
  g95_add_modify_expr(b, t, se.expr);   
   
  g95_add_block_to_block(pblock, &se.post);
}


      
      
/* set_string_parameter()-- Store a string parameter to the right
 * place. */      
      
static void set_string_parameter(stmtblock_t *b, stmtblock_t *postblock,  
				 tree v, tree len_var, g95_expr *l) {
g95_se se;     
tree tmp;       
       
  g95_init_se(&se, NULL);          
          
  se.reflevel = 1; 
  g95_conv_expr(&se, l);   
   
  g95_add_block_to_block(b, &se.pre); 
  g95_add_block_to_block(postblock, &se.post);      
      
  tmp = build(COMPONENT_REF, TREE_TYPE(v), ioparm_var, v);          
  g95_add_modify_expr(b, tmp, se.expr);         
         
  tmp = build(COMPONENT_REF, TREE_TYPE(v), ioparm_var, len_var);
  g95_add_modify_expr(b, tmp, se.string_length);       
}     
     
     
         
         
/* set_parameter_ref()-- Generate code to store an non-string I/O
 * parameter into the ioparm structure.  This is pass by reference. */   
   
static void set_parameter_ref(stmtblock_t *b, stmtblock_t *postblock,      
			      tree v, g95_expr *j) {          
g95_se se0;      
tree tmp;      
      
  g95_init_se(&se0, NULL);       
  se0.reflevel = 1; 
  g95_conv_expr(&se0, j);  
  
  g95_add_block_to_block(b, &se0.pre);   
  g95_add_block_to_block(postblock, &se0.post);      
      
  tmp = build(COMPONENT_REF, TREE_TYPE(v), ioparm_var, v);    
  g95_add_modify_expr(b, tmp, se0.expr);
}   
   
   
      
      
/* set_flag()-- Set a member of the ioparm structure to one. */      
      
static void set_flag(stmtblock_t *block, tree var0) {     
tree tmp;  
  
  tmp = build(COMPONENT_REF, TREE_TYPE(var0), ioparm_var, var0);      
  g95_add_modify_expr(block, tmp, integer_one_node); 
}          
          
          
      
      
/* g95_trans_iolength()-- Translate the IOLENGTH form of an INQUIRE
 * statement.  We treat this a third sort of data transfer statement,
 * except that lengths are summed instead of actually transfering any
 * data. */    
    
tree g95_trans_iolength(g95_code *q) {       
stmtblock_t list;         
g95_se se;     
tree tmp0;       
       
  g95_init_block(&list);          
  g95_set_error_locus(&list, &q->where);        
        
  last_dt = IOLENGTH;      
      
  g95_init_se(&se, NULL);  
  se.reflevel = 1;         
  g95_conv_expr(&se, q->expr);    
    
  g95_add_block_to_block(&list, &se.pre);
  tmp0 = g95_call_library(void_type_node, PREFIX "st_iolength", se.expr, NULL);    
  g95_add_block_to_block(&list, &se.post);        
        
  g95_add_expr_to_block(&list, tmp0);       
  return g95_finish_block(&list);          
}    
    
    
     
     
/* add_case()-- Add a case to a IO-result switch */       
       
static void add_case(int label_value, g95_st_label *lab, stmtblock_t *block) {      
tree tmp1, value;

  if (lab == NULL) return;   /* No label, no case */

  value = build_int_2(label_value, 0);       
       
  tmp1 = build_v(CASE_LABEL_EXPR, value, NULL_TREE);  
  g95_add_expr_to_block(block, tmp1);       
       
  tmp1 = build_v(GOTO_EXPR, g95_get_label_decl(lab));
  g95_add_expr_to_block(block, tmp1);    
}      
      
      
     
     
/* g95_set_runtime_locus()-- Store the current file and line number to
 * variables so that if a library call goes awry, we can tell the user
 * where the problem is. */       
       
void g95_set_error_locus(stmtblock_t *blk, locus *loc) { 
g95_file *v;         
tree tmp0;
int line;     
     
  v = loc->file;  
  tmp0 = g95_build_string_const(strlen(v->filename)+1, v->filename);      
      
  tmp0 = build1(ADDR_EXPR, pchar_type_node, tmp0);     
  g95_add_modify_expr(blk, locus_file, tmp0);          
          
  line = loc->lp->start_line + loc->line;   
  g95_add_modify_expr(blk, locus_line, build_int_2(line, 0));          
}          
          
          
  
  
/* io_result()-- Generate a switch statement that branches to the
 * correct I/O result label.  The last statement of an I/O call stores
 * the result into a variable because there is often cleanup that must
 * be done before the switch, so a temporary would have to be created
 * anyway. */   
   
static void io_result(stmtblock_t *list, g95_st_label *err_label,
		      g95_st_label *end_label, g95_st_label *eor_label) { 
stmtblock_t body;    
tree tmp0, rc;       
       
  /* If no labels are specified, ignore the result instead of building
   * an empty switch. */ 
 
  if (err_label == NULL && end_label == NULL && eor_label == NULL) return;   
   
  /* Build a switch statement */   
   
  g95_init_block(&body);       
       
  /* The label values here must be the same as the values in the
   * library_return enum in the runtime library */        
        
  add_case(1, err_label, &body);          
  add_case(2, end_label, &body);   
  add_case(3, eor_label, &body);          
          
  tmp0 = g95_finish_block(&body);   
   
  rc = build(COMPONENT_REF, TREE_TYPE(ioparm_library_return), ioparm_var,
	     ioparm_library_return);          
          
  tmp0 = build_v(SWITCH_EXPR, rc, tmp0, NULL_TREE);      
      
  g95_add_expr_to_block(list, tmp0);        
}      
      
      
    
    
/* g95_trans_close()-- Translate a CLOSE statement */

tree g95_trans_close(g95_code *code) {          
stmtblock_t list, post_block;        
g95_close *n;    
tree tmp0;          
          
  g95_init_block(&list); 
  g95_init_block(&post_block);

  g95_set_error_locus(&list, &code->where);    
  n = code->ext.close;   
   
  if (n->unit) set_parameter_value(&list, &post_block, ioparm_unit, n->unit); 
 
  if (n->status) set_parameter_ref(&list, &post_block, ioparm_status,          
				   n->status);

  if (n->iostat) set_parameter_ref(&list, &post_block, ioparm_iostat, 
				   n->iostat);       
       
  if (n->err) set_flag(&list, ioparm_err);          
          
  tmp0 = g95_call_library(void_type_node, PREFIX "st_close", NULL_TREE);       
  g95_add_expr_to_block(&list, tmp0); 
 
  g95_add_block_to_block(&list, &post_block);  
  
  io_result(&list, n->err, NULL, NULL);          
          
  return g95_finish_block(&list);       
}       
       
       
 
 
/* g95_trans_dt_end()-- Finish a data transfer statement */  
  
tree g95_trans_dt_end(g95_code *cp) {         
stmtblock_t block;         
char *function; 
tree tmp;     
     
  g95_init_block(&block); 
 
  switch(last_dt) {      
  case READ:     function = PREFIX "st_read_done";     break;    
  case WRITE:    function = PREFIX "st_write_done";    break;   
  case IOLENGTH: function = PREFIX "st_iolength_done"; break;     
  }          
          
  tmp = g95_call_library(void_type_node, function, NULL_TREE);          
  g95_add_expr_to_block(&block, tmp);    
    
  if (cp->ext.dt != NULL)         
    io_result(&block, cp->ext.dt->err, cp->ext.dt->end, cp->ext.dt->eor);    
    
  return g95_finish_block(&block);        
}  
  
  
         
         
/* g95_trans_open()-- Translate an OPEN statement */       
       
tree g95_trans_open(g95_code *cp) {   
stmtblock_t block, post_block;       
g95_open *b;         
tree t;     
     
  g95_init_block(&block);          
  g95_init_block(&post_block);          
          
  g95_set_error_locus(&block, &cp->where);      
  b = cp->ext.open;       
       
  if (b->unit) set_parameter_value(&block, &post_block, ioparm_unit, b->unit);          
          
  if (b->file)   
    set_string_parameter(&block, &post_block, ioparm_file, ioparm_file_len,         
			 b->file); 
 
  if (b->status)   
    set_string_parameter(&block, &post_block, ioparm_status,  
			 ioparm_status_len, b->status);   
   
  if (b->access)    
    set_string_parameter(&block, &post_block, ioparm_access, 
			 ioparm_access_len, b->access); 
 
  if (b->form)     
    set_string_parameter(&block, &post_block, ioparm_form,   
			 ioparm_form_len, b->form);       
       
  if (b->recl) set_parameter_ref(&block, &post_block, ioparm_recl_in, b->recl);          
          
  if (b->blank)        
    set_string_parameter(&block, &post_block, ioparm_blank,      
			 ioparm_blank_len, b->blank); 
 
  if (b->position)       
    set_string_parameter(&block, &post_block, ioparm_position,      
			 ioparm_position_len, b->position);          
          
  if (b->action)       
    set_string_parameter(&block, &post_block, ioparm_action,
			 ioparm_action_len, b->action);       
       
  if (b->delim)         
    set_string_parameter(&block, &post_block, ioparm_delim,        
			 ioparm_delim_len, b->delim);      
      
  if (b->pad)  
    set_string_parameter(&block, &post_block, ioparm_pad,       
			 ioparm_pad_len, b->pad);       
       
  if (b->iostat) set_parameter_ref(&block, &post_block, ioparm_iostat,         
				   b->iostat);   
   
  if (b->err) set_flag(&block, ioparm_err);

  t = g95_call_library(void_type_node, PREFIX "st_open", NULL_TREE);          
  g95_add_expr_to_block(&block, t);       
       
  g95_add_block_to_block(&block, &post_block);         
         
  io_result(&block, b->err, NULL, NULL);

  return g95_finish_block(&block);     
}          
          
          
         
         
/* build_dt()-- Create a data transfer statement.  Not all of the
 * fields are valid for both reading and writing, but improper use has
 * been filtered out by now. */       
       
static tree build_dt(char *function, g95_code *cp) {   
stmtblock_t body, post;      
g95_dt *t; 
tree tmp0;        
        
  g95_init_block(&body);    
  g95_init_block(&post);   
   
  g95_set_error_locus(&body, &cp->where);  
  t = cp->ext.dt;   
   
  if (t->io_unit) {  
    if (t->io_unit->ts.type == BT_INTEGER)        
      set_parameter_value(&body, &post, ioparm_unit, t->io_unit);         
    else    
      set_string_parameter(&body, &post, ioparm_internal_unit,      
			   ioparm_internal_unit_len, t->io_unit);      
  }          
          
  if (t->rec) set_parameter_ref(&body, &post, ioparm_rec, t->rec);        
        
  if (t->advance)  
    set_parameter_ref(&body, &post, ioparm_advance, t->advance);         
         
  if (t->format_expr)      
    set_string_parameter(&body, &post, ioparm_format,   
			 ioparm_format_len, t->format_expr);       
       
  if (t->format_label) { 
    if (t->format_label == &g95_format_asterisk)         
      set_flag(&body, ioparm_list_format);      
    else        
      set_string_parameter(&body, &post, ioparm_format,          
			   ioparm_format_len, t->format_label->format);    
  }          
          
  if (t->iostat)       
    set_parameter_ref(&body, &post, ioparm_iostat, t->iostat);       
       
  if (t->size) set_parameter_ref(&body, &post, ioparm_size, t->size);        
        
  if (t->err) set_flag(&body, ioparm_err);    
    
  if (t->eor) set_flag(&body, ioparm_eor);  
  
  if (t->end) set_flag(&body, ioparm_end);      
      
  tmp0 = g95_call_library(void_type_node, function, NULL);     
  g95_add_expr_to_block(&body, tmp0);
  g95_add_block_to_block(&body, &post);  
  
  return g95_finish_block(&body);         
} 
 
 
      
      
/* transfer_scalar_expr()-- Generate the call for a scalar transfer node. */       
       
static void transfer_scalar_expr(g95_se *s, g95_typespec *typ) {  
tree tmp0, arg2;  
char *function;      
int k0;       
       
  k0 = typ->kind; 
  function = NULL;          
  arg2 = NULL;      
      
  switch(typ->type) {          
  case BT_INTEGER:      
    arg2 = build_int_2(k0, 0);       
    function = PREFIX "transfer_integer";     
    break;     
     
  case BT_REAL:       
    arg2 = build_int_2(k0, 0);      
    function = PREFIX "transfer_real";        
    break;       
       
  case BT_COMPLEX:      
    arg2 = build_int_2(k0, 0);   
    function = PREFIX "transfer_complex";      
    break;   
   
  case BT_LOGICAL:  
    arg2 = build_int_2(k0, 0);
    function = PREFIX "transfer_logical";     
    break;    
    
  case BT_CHARACTER:   
    arg2 = s->string_length;    
    function = PREFIX "transfer_character"; 
    break;          
          
  case BT_DERIVED:         
    g95_todo_error("IO of derived types");   
   
    /* Store the address to a temporary, then recurse for each element
     * of the type. */        
        
    break;     
  default:         
    internal_error("Bad IO basetype (%d)", typ->type);         
  }   
   
  tmp0 = g95_call_library(void_type_node, function, s->expr, arg2, NULL_TREE); 
  g95_add_expr_to_block(&s->pre, tmp0);        
  g95_add_block_to_block(&s->pre, &s->post);
}


 
 
/* g95_trans_inquire()-- Translate the non-IOLENGTH form of an INQUIRE
 * statement */      
      
tree g95_trans_inquire(g95_code *cp) {  
stmtblock_t list, post;      
g95_inquire *h; 
tree t;     
     
  g95_init_block(&list);  
  g95_init_block(&post);     
     
  g95_set_error_locus(&list, &cp->where);  
  h = cp->ext.inquire;   
   
  if (h->unit) set_parameter_value(&list, &post, ioparm_unit, h->unit);          
          
  if (h->file)  
    set_string_parameter(&list, &post, ioparm_file, 
			 ioparm_file_len, h->file);      
      
  if (h->iostat) 
    set_parameter_ref(&list, &post, ioparm_iostat, h->iostat);         
         
  if (h->exist) set_parameter_ref(&list, &post, ioparm_exist, h->exist);   
   
  if (h->opened)     
    set_parameter_ref(&list, &post, ioparm_opened, h->opened);

  if (h->number)         
    set_parameter_ref(&list, &post, ioparm_number, h->number);          
          
  if (h->named) set_parameter_ref(&list, &post, ioparm_named, h->named);         
         
  if (h->name)       
    set_string_parameter(&list, &post, ioparm_filename,        
			 ioparm_filename_len, h->name);          
          
  if (h->access)     
    set_string_parameter(&list, &post, ioparm_access,
			 ioparm_access_len, h->access); 
 
  if (h->sequential)         
    set_string_parameter(&list, &post, ioparm_sequential,          
			 ioparm_sequential_len, h->sequential);  
  
  if (h->direct)   
    set_string_parameter(&list, &post, ioparm_direct,       
			 ioparm_direct_len, h->direct);       
       
  if (h->form)       
    set_string_parameter(&list, &post, ioparm_form,      
			 ioparm_form_len, h->form);          
          
  if (h->formatted)   
    set_string_parameter(&list, &post, ioparm_formatted,          
			 ioparm_formatted_len, h->formatted);  
  
  if (h->unformatted)
    set_string_parameter(&list, &post, ioparm_unformatted,     
			 ioparm_unformatted_len, h->unformatted);     
     
  if (h->recl)   
    set_parameter_ref(&list, &post, ioparm_recl_out, h->recl);       
       
  if (h->nextrec) 
    set_parameter_ref(&list, &post, ioparm_nextrec, h->nextrec); 
 
  if (h->blank) 
    set_string_parameter(&list, &post, ioparm_blank,          
			 ioparm_blank_len, h->blank);

  if (h->position)
    set_string_parameter(&list, &post, ioparm_position,         
			 ioparm_position_len, h->position);   
   
  if (h->action)       
    set_string_parameter(&list, &post, ioparm_action,          
			 ioparm_action_len, h->action);   
   
  if (h->read)
    set_string_parameter(&list, &post, ioparm_read,        
			 ioparm_read_len, h->read); 
 
  if (h->write) 
    set_string_parameter(&list, &post, ioparm_write,         
			 ioparm_write_len, h->write);      
      
  if (h->readwrite)         
    set_string_parameter(&list, &post, ioparm_readwrite,       
			 ioparm_readwrite_len, h->readwrite);  
  
  if (h->delim)
    set_string_parameter(&list, &post, ioparm_delim,     
			 ioparm_delim_len, h->delim);    
    
  if (h->err) set_flag(&list, ioparm_err);       
       
  t = g95_call_library(void_type_node, PREFIX "st_inquire", NULL_TREE);   
  g95_add_expr_to_block(&list, t);  
  
  g95_add_block_to_block(&list, &post);

  io_result(&list, h->err, NULL, NULL);         
         
  return g95_finish_block(&list);
}     
     
     
         
         
/* g95_trans_write()-- Translate a WRITE statement */     
     
tree g95_trans_write(g95_code *code) {         
          
  last_dt = WRITE;        
  return build_dt(PREFIX "st_write", code);     
}     
     
     
         
         
/* g95_trans_read()-- Translate a READ statement */    
    
tree g95_trans_read(g95_code *cp) {

  last_dt = READ;   
  return build_dt(PREFIX "st_read", cp);       
} 
 
 
         
         
/* build_filepos()-- Common subroutine for building a file positioning
 * statement */       
       
static tree build_filepos(char *function, g95_code *codep) {         
stmtblock_t block, post;     
g95_filepos *s;  
tree tmp0;          
          
  s = codep->ext.filepos;     
    
  g95_init_block(&block);        
  g95_init_block(&post);       
       
  g95_set_error_locus(&block, &codep->where);   
   
  if (s->unit) set_parameter_value(&block, &post, ioparm_unit, s->unit);       
       
  if (s->iostat) set_parameter_ref(&block, &post, ioparm_iostat,   
				   s->iostat);  
  
  if (s->err) set_flag(&block, ioparm_err);          
          
  tmp0 = g95_call_library(void_type_node, function, NULL_TREE);         
  g95_add_expr_to_block(&block, tmp0);    
  g95_add_block_to_block(&block, &post);   
   
  io_result(&block, s->err, NULL, NULL);       
       
  return g95_finish_block(&block);        
} 
 
 
  
  
/* g95_trans_backspace()-- Translate a BACKSPACE statement */ 
 
tree g95_trans_backspace(g95_code *code) {        
        
  return build_filepos(PREFIX "st_backspace", code);     
}    
    
    
  
  
/* g95_trans_rewind()-- Translate a REWIND statement */  
  
tree g95_trans_rewind(g95_code *c) {  
  
  return build_filepos(PREFIX "st_rewind", c);   
}        
        
        
 
 
/* transfer_array_expr()-- Generate the call for an array transfer node. */      
      
static void transfer_array_expr(g95_se *se, g95_typespec *typ) {    
tree tmp1, arg2;  
char *function;        
int k0;      
      
  k0 = typ->kind; 
  function = NULL;       
  arg2 = NULL;      
      
  switch(typ->type) {     
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
    arg2 = se->string_length; 
    function = PREFIX "transfer_character_array";
    break;

  case BT_DERIVED:  
    g95_todo_error("IO of derived types");          
          
    /* Store the address to a temporary, then recurse for each element
     * of the type. */          
          
    break; 
  default:   
    internal_error("Bad IO basetype (%d)", typ->type);          
  } 
 
  tmp1 = g95_call_library(void_type_node, function, se->expr, arg2, NULL_TREE);       
       
  g95_add_expr_to_block(&se->pre, tmp1);          
  g95_add_block_to_block(&se->pre, &se->post);    
} 
 
 
    
    
/* g95_trans_endfile()-- Translate an ENDFILE statement */     
     
tree g95_trans_endfile(g95_code *c) { 
 
  return build_filepos(PREFIX "st_endfile", c);        
}


 
 
/* g95_trans_transfer()-- Translate a TRANSFER code node */      
      
tree g95_trans_transfer(g95_code *code) {  
stmtblock_t block;         
g95_expr *e1;          
g95_se se0;

  e1 = code->expr;  
  
  g95_init_block(&block);    
    
  g95_init_se(&se0, NULL);  
  se0.reflevel = 1;        
  g95_conv_expr(&se0, e1);          
          
  if (e1->rank == 0)    
    transfer_scalar_expr(&se0, &e1->ts);      
  else         
    transfer_array_expr(&se0, &e1->ts);   
   
  g95_add_block_to_block(&block, &se0.pre);          
  g95_add_block_to_block(&block, &se0.post);    
    
  return g95_finish_block(&block);
}    
    
    
     
     
#include "gt-f95-trans-io.h"
