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
static GTY(()) tree ioparm_internal_array; 
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
      
typedef struct desc_list {    
  g95_symbol *sym;          
  tree desc;
  struct desc_list *next;    
} desc_list;       
       
static desc_list *io_descriptors;  
  
  
  
          
          
/* set_flag()-- Set a member of the ioparm structure to one. */         
         
static void set_flag(stmtblock_t *list, tree var0) {
tree tmp; 
 
  tmp = build(COMPONENT_REF, TREE_TYPE(var0), ioparm_var, var0);       
  g95_add_modify_expr(list, tmp, integer_one_node);          
} 
 
 
    
    
/* array_format()-- Deal with a format expression that is a character
 * array. */       
       
static void array_format(stmtblock_t *block, stmtblock_t *pblock,   
			 g95_expr *format) {
tree t, variable, v, j;       
g95_se se1;        
        
  g95_init_se(&se1, NULL);   
  se1.reflevel = 1;         
         
  g95_conv_expr(&se1, format);    
    
  g95_set_section_info(&se1, 0, integer_zero_node);          
          
  g95_add_block_to_block(block, &se1.pre);   
  g95_add_block_to_block(pblock, &se1.post);    
    
  variable = g95_create_var(pchar_type_node);          
  TREE_ADDRESSABLE(variable) = 1;      
  t = build1(ADDR_EXPR, pvoid_type_node, variable);          
          
  j = build(COMPONENT_REF, g95_default_integer, ioparm_var, ioparm_format_len);  
  j = build1(ADDR_EXPR, build_pointer_type(TREE_TYPE(j)), j);      
      
  t = g95_call_library(pvoid_type_node, PREFIX "contiguous_array",   
			 se1.expr, t, j, NULL_TREE);      
      
  v = build(COMPONENT_REF, pchar_type_node, ioparm_var, ioparm_format);       
  g95_add_modify_expr(block, v, t);      
      
  t = g95_call_library(void_type_node, PREFIX "contiguous_array_done",     
			 variable, integer_zero_node, NULL_TREE);        
        
  g95_add_expr_to_block(pblock, t);          
}     
     
     
       
       
/* set_string_parameter()-- Store a string parameter to the right
 * place. */          
          
static void set_string_parameter(stmtblock_t *list, stmtblock_t *postblock,    
				 tree variable, tree len_var, g95_expr *c) {     
g95_se se1;        
tree tmp0;          
          
  g95_init_se(&se1, NULL);    
    
  se1.reflevel = 1; 
  g95_conv_expr(&se1, c);

  g95_add_block_to_block(list, &se1.pre);         
  g95_add_block_to_block(postblock, &se1.post);  
  
  tmp0 = build(COMPONENT_REF, TREE_TYPE(variable), ioparm_var, variable);          
  g95_add_modify_expr(list, tmp0, se1.expr);  
  
  tmp0 = build(COMPONENT_REF, TREE_TYPE(variable), ioparm_var, len_var);
  g95_add_modify_expr(list, tmp0, se1.string_length);     
}




/* g95_set_runtime_locus()-- Store the current file and line number to
 * variables so that if a library call goes awry, we can tell the user
 * where the problem is. */         
         
void g95_set_error_locus(stmtblock_t *body, g95_locus *where) {       
g95_linebuf *lb;        
char *filename;    
tree tmp0;  
int line;         
         
  lb = where->lb; 
  filename = lb->file->filename;      
  line = lb->linenum;  
  
  tmp0 = g95_build_string_const(strlen(filename)+1, filename);
  tmp0 = build1(ADDR_EXPR, pchar_type_node, tmp0);        
        
  g95_add_modify_expr(body, locus_file, tmp0); 
  g95_add_modify_expr(body, locus_line, build_int_2(line, 0));         
}     
     
     


/* set_parameter_value()-- Generate code to store an non-string I/O
 * parameter into the ioparm structure.  This is a pass by value. */       
       
static void set_parameter_value(stmtblock_t *block, stmtblock_t *post_block,  
				tree variable, g95_expr *j) {   
g95_se se0;  
tree tmp1;          
          
  g95_init_se(&se0, NULL);  
  g95_conv_expr_type(&se0, j, TREE_TYPE(variable));        
        
  g95_add_block_to_block(block, &se0.pre);      
      
  tmp1 = build(COMPONENT_REF, TREE_TYPE(variable), ioparm_var, variable);      
  g95_add_modify_expr(block, tmp1, se0.expr);    
    
  g95_add_block_to_block(post_block, &se0.post);     
}  
  
  
     
     
/* set_parameter_ref()-- Generate code to store an non-string I/O
 * parameter into the ioparm structure.  This is pass by reference. */        
        
static void set_parameter_ref(stmtblock_t *list, stmtblock_t *postblock,     
			      tree variable, g95_expr *f) {        
g95_se s;   
tree tmp;        
        
  g95_init_se(&s, NULL);         
  s.reflevel = 1;    
  g95_conv_expr(&s, f);        
        
  g95_add_block_to_block(list, &s.pre);   
  g95_add_block_to_block(postblock, &s.post);     
     
  tmp = build(COMPONENT_REF, TREE_TYPE(variable), ioparm_var, variable);    
  g95_add_modify_expr(list, tmp, s.expr);       
}


    
    
/* add_case()-- Add a case to a IO-result switch */        
        
static void add_case(int label_value, g95_st_label *labl, stmtblock_t *block) {     
tree tmp1, value;       
       
  if (labl == NULL) return;   /* No label, no case */          
          
  value = build_int_2(label_value, 0);  
  
  tmp1 = build_v(CASE_LABEL_EXPR, value, NULL_TREE);      
  g95_add_expr_to_block(block, tmp1);      
      
  tmp1 = build_v(GOTO_EXPR, g95_get_label_decl(labl));  
  g95_add_expr_to_block(block, tmp1);      
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
  ADD_FIELD(internal_array, pvoid_type_node);         
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
          
          
        
        
/* g95_add_field()-- Add a field to the ioparm structure. */    
    
tree g95_add_field(tree stype, char *name0, tree t) {
tree d;      
      
  d = build_decl(FIELD_DECL, get_identifier(name0), t);      
      
  DECL_CONTEXT(d) = stype;      
  DECL_INITIAL(d) = 0;    
  DECL_ALIGN(d) = 0; 
  DECL_USER_ALIGN(d) = 0;      
  TYPE_FIELDS(stype) = chainon(TYPE_FIELDS(stype), d);  
  
  return d;    
}         
         
         
        
        
/* build_dt()-- Create a data transfer statement.  Not all of the
 * fields are valid for both reading and writing, but improper use has
 * been filtered out by now. */     
     
static tree build_dt(char *function, g95_code *codep) {     
stmtblock_t block, post_block;   
g95_dt *t;     
tree tmp0;

  g95_init_block(&block);        
  g95_init_block(&post_block);    
    
  g95_set_error_locus(&block, &codep->where);      
  t = codep->ext.dt;      
      
  if (t->io_unit) {     
    if (t->io_unit->ts.type == BT_INTEGER)   
      set_parameter_value(&block, &post_block, ioparm_unit, t->io_unit);
    else {       
      if (t->io_unit->rank == 0)       
	set_string_parameter(&block, &post_block, ioparm_internal_unit,          
			     ioparm_internal_unit_len, t->io_unit);    
      else 
	set_parameter_ref(&block, &post_block, ioparm_internal_array,  
			  t->io_unit);       
    }    
  }        
        
  if (t->rec) set_parameter_ref(&block, &post_block, ioparm_rec, t->rec);        
        
  if (t->advance)       
    set_parameter_ref(&block, &post_block, ioparm_advance, t->advance);

  if (t->format_expr) {         
    if (t->format_expr->rank == 0)   
      set_string_parameter(&block, &post_block, ioparm_format,   
			   ioparm_format_len, t->format_expr);  
    else 
      array_format(&block, &post_block, t->format_expr);          
  }   
   
  if (t->format_label) {    
    if (t->format_label == &g95_format_asterisk) 
      set_flag(&block, ioparm_list_format);       
    else          
      set_string_parameter(&block, &post_block, ioparm_format,    
			   ioparm_format_len, t->format_label->format);     
  }      
      
  if (t->iostat)     
    set_parameter_ref(&block, &post_block, ioparm_iostat, t->iostat); 
 
  if (t->size) set_parameter_ref(&block, &post_block, ioparm_size, t->size);

  if (t->err) set_flag(&block, ioparm_err);    
    
  if (t->eor) set_flag(&block, ioparm_eor);       
       
  if (t->end) set_flag(&block, ioparm_end);         
         
  tmp0 = g95_call_library(void_type_node, function, NULL); 
  g95_add_expr_to_block(&block, tmp0);      
  g95_add_block_to_block(&block, &post_block);       
       
  return g95_finish_block(&block);  
} 
 
 
         
         
/* io_result()-- Generate a switch statement that branches to the
 * correct I/O result label.  The last statement of an I/O call stores
 * the result into a variable because there is often cleanup that must
 * be done before the switch, so a temporary would have to be created
 * anyway. */   
   
static void io_result(stmtblock_t *list, g95_st_label *err_label,
		      g95_st_label *end_label, g95_st_label *eor_label) {   
stmtblock_t b;       
tree tmp1, rc;       
       
  /* If no labels are specified, ignore the result instead of building
   * an empty switch. */        
        
  if (err_label == NULL && end_label == NULL && eor_label == NULL) return;        
        
  /* Build a switch statement */   
   
  g95_init_block(&b);       
       
  /* The label values here must be the same as the values in the
   * library_return enum in the runtime library */  
  
  add_case(1, err_label, &b);        
  add_case(2, end_label, &b);      
  add_case(3, eor_label, &b);      
      
  tmp1 = g95_finish_block(&b);  
  
  rc = build(COMPONENT_REF, TREE_TYPE(ioparm_library_return), ioparm_var,         
	     ioparm_library_return);   
   
  tmp1 = build_v(SWITCH_EXPR, rc, tmp1, NULL_TREE);     
     
  g95_add_expr_to_block(list, tmp1);     
}         
         
         
         
         
/* transfer_array_expr()-- Generate the call for an array transfer node. */     
     
static void transfer_array_expr(g95_se *s, g95_typespec *typesp) {          
tree tmp0, arg2; 
char *function;      
int kind;          
          
  kind = typesp->kind;    
  function = NULL;      
  arg2 = NULL;         
         
  switch(typesp->type) {    
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
        
  tmp0 = g95_call_library(void_type_node, function, s->expr, arg2, NULL_TREE);

  g95_add_expr_to_block(&s->pre, tmp0);         
  g95_add_block_to_block(&s->pre, &s->post);        
}


     
     
/* build_filepos()-- Common subroutine for building a file positioning
 * statement */

static tree build_filepos(char *function, g95_code *code) { 
stmtblock_t list, post;       
g95_filepos *c;          
tree tmp1;         
         
  c = code->ext.filepos;    
   
  g95_init_block(&list);  
  g95_init_block(&post);   
   
  g95_set_error_locus(&list, &code->where);         
         
  if (c->unit) set_parameter_value(&list, &post, ioparm_unit, c->unit);

  if (c->iostat) set_parameter_ref(&list, &post, ioparm_iostat,
				   c->iostat);

  if (c->err) set_flag(&list, ioparm_err);      
      
  tmp1 = g95_call_library(void_type_node, function, NULL_TREE); 
  g95_add_expr_to_block(&list, tmp1); 
  g95_add_block_to_block(&list, &post);

  io_result(&list, c->err, NULL, NULL);          
          
  return g95_finish_block(&list);  
}    
    
    
 
 
/* g95_trans_close()-- Translate a CLOSE statement */  
  
tree g95_trans_close(g95_code *code) {        
stmtblock_t b, post_block;
g95_close *w;       
tree tmp1;       
       
  g95_init_block(&b);    
  g95_init_block(&post_block);     
     
  g95_set_error_locus(&b, &code->where);
  w = code->ext.close;      
      
  if (w->unit) set_parameter_value(&b, &post_block, ioparm_unit, w->unit);    
    
  if (w->status) set_parameter_ref(&b, &post_block, ioparm_status,      
				   w->status);        
        
  if (w->iostat) set_parameter_ref(&b, &post_block, ioparm_iostat,
				   w->iostat);     
     
  if (w->err) set_flag(&b, ioparm_err);

  tmp1 = g95_call_library(void_type_node, PREFIX "st_close", NULL_TREE);          
  g95_add_expr_to_block(&b, tmp1);          
          
  g95_add_block_to_block(&b, &post_block); 
 
  io_result(&b, w->err, NULL, NULL);       
       
  return g95_finish_block(&b);     
}       
       
       
     
     
/* g95_trans_backspace()-- Translate a BACKSPACE statement */        
        
tree g95_trans_backspace(g95_code *code) {        
        
  return build_filepos(PREFIX "st_backspace", code);
}  
  
  
      
      
/* g95_trans_write()-- Translate a WRITE statement */ 
 
tree g95_trans_write(g95_code *codep) {          
           
  last_dt = WRITE; 
  return build_dt(PREFIX "st_write", codep);     
}          
          
          
      
      
/* g95_trans_endfile()-- Translate an ENDFILE statement */

tree g95_trans_endfile(g95_code *codep) {  
  
  return build_filepos(PREFIX "st_endfile", codep);          
}  
  
  
          
          
/* g95_trans_iolength()-- Translate the IOLENGTH form of an INQUIRE
 * statement.  We treat this a third sort of data transfer statement,
 * except that lengths are summed instead of actually transfering any
 * data. */  
  
tree g95_trans_iolength(g95_code *s) {   
stmtblock_t list;        
g95_se se1;       
tree tmp;   
   
  g95_init_block(&list); 
  g95_set_error_locus(&list, &s->where);         
         
  last_dt = IOLENGTH;   
   
  g95_init_se(&se1, NULL); 
  se1.reflevel = 1;   
  g95_conv_expr(&se1, s->expr);   
   
  g95_add_block_to_block(&list, &se1.pre);  
  tmp = g95_call_library(void_type_node, PREFIX "st_iolength", se1.expr, NULL);       
  g95_add_block_to_block(&list, &se1.post);  
  
  g95_add_expr_to_block(&list, tmp);
  return g95_finish_block(&list);  
}          
          
          
   
   
/* g95_trans_dt_end()-- Finish a data transfer statement */          
          
tree g95_trans_dt_end(g95_code *codep) {
stmtblock_t body;         
char *function;        
tree tmp;         
         
  g95_init_block(&body);  
  
  switch(last_dt) {        
  case READ:     function = PREFIX "st_read_done";     break; 
  case WRITE:    function = PREFIX "st_write_done";    break; 
  case IOLENGTH: function = PREFIX "st_iolength_done"; break;
  }       
       
  tmp = g95_call_library(void_type_node, function, NULL_TREE);         
  g95_add_expr_to_block(&body, tmp);      
      
  if (codep->ext.dt != NULL)       
    io_result(&body, codep->ext.dt->err, codep->ext.dt->end, codep->ext.dt->eor);

  return g95_finish_block(&body);         
}          
          
          
         
         
/* find_iodesc()-- Given a symbol, return the desc_list structure that
 * contains that symbol.  Returns NULL if not found. */

static desc_list *find_iodesc(g95_symbol *sym) {         
desc_list *v;     
     
  for(v=io_descriptors; v; v=v->next)    
    if (sym == v->sym) return v; 
 
  return NULL;  
}       
       
       
       
       
/* g95_trans_open()-- Translate an OPEN statement */ 
 
tree g95_trans_open(g95_code *codep) {  
stmtblock_t b, pblock;      
g95_open *u;    
tree t; 
 
  g95_init_block(&b);          
  g95_init_block(&pblock);  
  
  g95_set_error_locus(&b, &codep->where);   
  u = codep->ext.open; 
 
  if (u->unit) set_parameter_value(&b, &pblock, ioparm_unit, u->unit);      
      
  if (u->file)      
    set_string_parameter(&b, &pblock, ioparm_file, ioparm_file_len,  
			 u->file);    
    
  if (u->status)       
    set_string_parameter(&b, &pblock, ioparm_status,          
			 ioparm_status_len, u->status); 
 
  if (u->access)
    set_string_parameter(&b, &pblock, ioparm_access, 
			 ioparm_access_len, u->access); 
 
  if (u->form) 
    set_string_parameter(&b, &pblock, ioparm_form,      
			 ioparm_form_len, u->form);    
    
  if (u->recl) set_parameter_ref(&b, &pblock, ioparm_recl_in, u->recl);

  if (u->blank)        
    set_string_parameter(&b, &pblock, ioparm_blank,   
			 ioparm_blank_len, u->blank);      
      
  if (u->position)     
    set_string_parameter(&b, &pblock, ioparm_position,        
			 ioparm_position_len, u->position);        
        
  if (u->action)         
    set_string_parameter(&b, &pblock, ioparm_action,
			 ioparm_action_len, u->action);      
      
  if (u->delim)        
    set_string_parameter(&b, &pblock, ioparm_delim,      
			 ioparm_delim_len, u->delim);       
       
  if (u->pad)        
    set_string_parameter(&b, &pblock, ioparm_pad,       
			 ioparm_pad_len, u->pad);        
        
  if (u->iostat) set_parameter_ref(&b, &pblock, ioparm_iostat,      
				   u->iostat);    
    
  if (u->err) set_flag(&b, ioparm_err);       
       
  t = g95_call_library(void_type_node, PREFIX "st_open", NULL_TREE);    
  g95_add_expr_to_block(&b, t);    
    
  g95_add_block_to_block(&b, &pblock);     
     
  io_result(&b, u->err, NULL, NULL);          
          
  return g95_finish_block(&b); 
}         
         
         
      
      
/* transfer_scalar_expr()-- Generate the call for a scalar transfer node. */         
         
static void transfer_scalar_expr(g95_se *s, g95_typespec *typesp) {          
tree tmp, arg2;
char *function;   
int knd;  
  
  knd = typesp->kind;     
  function = NULL;       
  arg2 = NULL;        
        
  switch(typesp->type) {        
  case BT_INTEGER:          
    arg2 = build_int_2(knd, 0);   
    function = PREFIX "transfer_integer";          
    break; 
 
  case BT_REAL:     
    arg2 = build_int_2(knd, 0);     
    function = PREFIX "transfer_real";          
    break;        
        
  case BT_COMPLEX:         
    arg2 = build_int_2(knd, 0);   
    function = PREFIX "transfer_complex";        
    break;     
     
  case BT_LOGICAL:        
    arg2 = build_int_2(knd, 0);        
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
    internal_error("Bad IO basetype (%d)", typesp->type);   
  }          
          
  tmp = g95_call_library(void_type_node, function, s->expr, arg2, NULL_TREE);         
  g95_add_expr_to_block(&s->pre, tmp); 
  g95_add_block_to_block(&s->pre, &s->post);   
}       
       
       
         
         
/* g95_trans_transfer()-- Translate a TRANSFER code node */        
        
tree g95_trans_transfer(g95_code *c) { 
stmtblock_t list;  
g95_expr *exp; 
g95_se se0;         
         
  exp = c->expr;       
       
  g95_init_block(&list);    
    
  g95_init_se(&se0, NULL);          
  se0.reflevel = 1;         
  g95_conv_expr(&se0, exp);     
     
  if (exp->rank == 0) 
    transfer_scalar_expr(&se0, &exp->ts);       
  else  
    transfer_array_expr(&se0, &exp->ts);

  g95_add_block_to_block(&list, &se0.pre);          
  g95_add_block_to_block(&list, &se0.post);      
      
  return g95_finish_block(&list);   
}         
         
         
        
        
/* g95_trans_read()-- Translate a READ statement */         
         
tree g95_trans_read(g95_code *code) { 
 
  last_dt = READ;  
  return build_dt(PREFIX "st_read", code);   
} 
 
 
      
      
/* g95_trans_inquire()-- Translate the non-IOLENGTH form of an INQUIRE
 * statement */      
      
tree g95_trans_inquire(g95_code *c) {       
stmtblock_t block, pblock;     
g95_inquire *z;      
tree tmp;        
        
  g95_init_block(&block);      
  g95_init_block(&pblock); 
 
  g95_set_error_locus(&block, &c->where);        
  z = c->ext.inquire;      
      
  if (z->unit) set_parameter_value(&block, &pblock, ioparm_unit, z->unit);        
        
  if (z->file)        
    set_string_parameter(&block, &pblock, ioparm_file,        
			 ioparm_file_len, z->file);    
    
  if (z->iostat)    
    set_parameter_ref(&block, &pblock, ioparm_iostat, z->iostat);        
        
  if (z->exist) set_parameter_ref(&block, &pblock, ioparm_exist, z->exist);     
     
  if (z->opened)         
    set_parameter_ref(&block, &pblock, ioparm_opened, z->opened);         
         
  if (z->number) 
    set_parameter_ref(&block, &pblock, ioparm_number, z->number);      
      
  if (z->named) set_parameter_ref(&block, &pblock, ioparm_named, z->named); 
 
  if (z->name)        
    set_string_parameter(&block, &pblock, ioparm_filename,         
			 ioparm_filename_len, z->name);     
     
  if (z->access)         
    set_string_parameter(&block, &pblock, ioparm_access,          
			 ioparm_access_len, z->access);          
          
  if (z->sequential) 
    set_string_parameter(&block, &pblock, ioparm_sequential,
			 ioparm_sequential_len, z->sequential);         
         
  if (z->direct)       
    set_string_parameter(&block, &pblock, ioparm_direct,      
			 ioparm_direct_len, z->direct);         
         
  if (z->form)   
    set_string_parameter(&block, &pblock, ioparm_form,         
			 ioparm_form_len, z->form);    
    
  if (z->formatted)
    set_string_parameter(&block, &pblock, ioparm_formatted,  
			 ioparm_formatted_len, z->formatted);   
   
  if (z->unformatted)       
    set_string_parameter(&block, &pblock, ioparm_unformatted,
			 ioparm_unformatted_len, z->unformatted);       
       
  if (z->recl)        
    set_parameter_ref(&block, &pblock, ioparm_recl_out, z->recl);     
     
  if (z->nextrec)      
    set_parameter_ref(&block, &pblock, ioparm_nextrec, z->nextrec);   
   
  if (z->blank)   
    set_string_parameter(&block, &pblock, ioparm_blank,        
			 ioparm_blank_len, z->blank);      
      
  if (z->position)    
    set_string_parameter(&block, &pblock, ioparm_position,
			 ioparm_position_len, z->position);        
        
  if (z->action)          
    set_string_parameter(&block, &pblock, ioparm_action,         
			 ioparm_action_len, z->action);        
        
  if (z->read)  
    set_string_parameter(&block, &pblock, ioparm_read,        
			 ioparm_read_len, z->read);     
     
  if (z->write)       
    set_string_parameter(&block, &pblock, ioparm_write,    
			 ioparm_write_len, z->write);       
       
  if (z->readwrite)     
    set_string_parameter(&block, &pblock, ioparm_readwrite,          
			 ioparm_readwrite_len, z->readwrite);     
     
  if (z->delim)       
    set_string_parameter(&block, &pblock, ioparm_delim,   
			 ioparm_delim_len, z->delim);  
  
  if (z->err) set_flag(&block, ioparm_err);       
       
  tmp = g95_call_library(void_type_node, PREFIX "st_inquire", NULL_TREE);         
  g95_add_expr_to_block(&block, tmp);      
      
  g95_add_block_to_block(&block, &pblock);

  io_result(&block, z->err, NULL, NULL);       
       
  return g95_finish_block(&block);  
}   
   
   
     
     
/* generate_iodesc()-- Given a derived type, generate a descriptor
 * describing the layout of the type.  Returns the descriptor, or
 * NULL_TREE if a recursive structure has been detected. */   
   
static tree generate_iodesc(g95_symbol *s) {
desc_list *f;  
  
  f = find_iodesc(s);   
  if (f != NULL) return f->desc;       
       
  /* Create a new descriptor right away.  By inserting the node with a
   * descriptor that is NULL_TREE, we can detect recursive structures
   * that are nontransferable. */    
    
  f = g95_getmem(sizeof(desc_list));
  f->sym = s; 
  f->desc = NULL_TREE;   
  f->next = io_descriptors;

  io_descriptors = f;    
    
    
    
    
  return NULL;          
}        
        
        
         
         
/* g95_trans_rewind()-- Translate a REWIND statement */

tree g95_trans_rewind(g95_code *c) {    
    
  return build_filepos(PREFIX "st_rewind", c); 
}   
   
   
 
 
#include "gt-f95-trans-io.h"
