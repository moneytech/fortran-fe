/* I/O related subroutines
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
       
/* io.c-- Deal with input/output statements */     
     
#include "g95.h"
      
g95_st_label g95_format_asterisk;   
   
   
typedef struct {   
  char *name, *spec;
  bt type;          
} io_tag;   
   
static io_tag   
  tag_file =        { "FILE",        " file = %e",         BT_CHARACTER },        
  tag_status =      { "STATUS",      " status = %e",       BT_CHARACTER },       
  tag_e_access =    { "ACCESS",      " access = %e",       BT_CHARACTER },     
  tag_e_form =      { "FORM",        " form = %e",         BT_CHARACTER },         
  tag_e_recl =      { "RECL",        " recl = %e",         BT_INTEGER   },        
  tag_e_blank =     { "BLANK",       " blank = %e",        BT_CHARACTER },      
  tag_e_position =  { "POSITION",    " position = %e",     BT_CHARACTER },  
  tag_e_action =    { "ACTION",      " action = %e",       BT_CHARACTER },       
  tag_e_delim =     { "DELIM",       " delim = %e",        BT_CHARACTER },       
  tag_e_pad =       { "PAD",         " pad = %e",          BT_CHARACTER },  
  tag_unit =        { "UNIT",        " unit = %e",         BT_INTEGER   },         
  tag_advance =     { "ADVANCE",     " advance = %e",      BT_CHARACTER },      
  tag_rec =         { "REC",         " rec = %e",          BT_INTEGER   }, 
  tag_format =      { "FORMAT",      NULL,                 BT_CHARACTER },      
      
  tag_iostat =      { "IOSTAT",      " iostat = %v",       BT_INTEGER   },    
  tag_size =        { "SIZE",        " size = %v",         BT_INTEGER   },      
  tag_exist =       { "EXIST",       " exist = %v",        BT_LOGICAL   },     
  tag_opened =      { "OPENED",      " opened = %v",       BT_LOGICAL   },          
  tag_named =       { "NAMED",       " named = %v",        BT_LOGICAL   }, 
  tag_name =        { "NAME",        " name = %v",         BT_CHARACTER },    
  tag_number =      { "NUMBER",      " number = %v",       BT_INTEGER   },
  tag_s_access =    { "ACCESS",      " access = %v",       BT_CHARACTER },     
  tag_sequential =  { "SEQUENTIAL",  " sequential = %v",   BT_CHARACTER }, 
  tag_direct =      { "DIRECT",      " direct = %v",       BT_CHARACTER },    
  tag_s_form =      { "FORM",        " form = %v",         BT_CHARACTER },   
  tag_formatted =   { "FORMATTED",   " formatted = %v",    BT_CHARACTER },          
  tag_unformatted = { "UNFORMATTED", " unformatted = %v",  BT_CHARACTER },
  tag_s_recl =      { "RECL",        " recl = %v",         BT_INTEGER   },      
  tag_nextrec =     { "NEXTREC",     " nextrec = %v",      BT_INTEGER   },     
  tag_s_blank =     { "BLANK",       " blank = %v",        BT_CHARACTER },      
  tag_s_position =  { "POSITION",    " position = %v",     BT_CHARACTER },  
  tag_s_action =    { "ACTION",      " action = %v",       BT_CHARACTER },       
  tag_read =        { "READ",        " read = %v",         BT_CHARACTER },       
  tag_write =       { "WRITE",       " write = %v",        BT_CHARACTER },       
  tag_readwrite =   { "READWRITE",   " readwrite = %v",    BT_CHARACTER }, 
  tag_s_delim =     { "DELIM",       " delim = %v",        BT_CHARACTER },    
  tag_s_pad =       { "PAD",         " pad = %v",          BT_CHARACTER },         
  tag_iolength =    { "IOLENGTH",    " iolength = %v",     BT_INTEGER   },

  tag_err = { "ERR", " err = %l", BT_UNKNOWN },    
  tag_end = { "END", " end = %l", BT_UNKNOWN },     
  tag_eor = { "EOR", " eor = %l", BT_UNKNOWN };   
   
static g95_dt *current_dt;          
          
#define RESOLVE_TAG(x, y, flag) \
    if (resolve_tag(x, y, flag) == FAILURE) return FAILURE;
    
typedef enum { M_READ, M_WRITE, M_PRINT, M_INQUIRE } io_kind;        
static match match_io_element(io_kind k, g95_code **);       
static try resolve_tag(io_tag *, g95_expr *, int);


          
          
/* match_etag()-- Match an expression I/O tag of some sort. */         
         
static match match_etag(io_tag *tag, g95_expr **b) {    
g95_expr *result;         
match x;       
       
  x = g95_match(tag->spec, &result);    
  if (x != MATCH_YES) return x;      
      
  if (*b != NULL) {    
    g95_error("Duplicate %s specification at %C", tag->name);      
    g95_free_expr(result);         
    return MATCH_ERROR;       
  }        
        
  *b = result;          
  return MATCH_YES;    
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
   
   
          
          
/* default_unit()-- Return the default unit number.  The runtime
 * library translates this into the real unit number. */        
        
static g95_expr *default_unit(void) {  
  
  return g95_int_expr(-1);       
}          
          
          
       
       
/* resolve_tag()-- Do expression resolution and type-checking on an
 * expression tag */          
          
static try resolve_tag(io_tag *tag, g95_expr *e, int output) {  
  
  if (e == NULL) return SUCCESS;      
      
  if (g95_resolve_expr(e) == FAILURE) return FAILURE; 
 
  if (output && e->type == EXPR_VARIABLE) e->symbol->attr.set = 1;          
          
  if (e->ts.type != tag->type) { 
    g95_error("%s tag at %L must be of type %s", tag->name, &e->where,      
	      g95_basic_typename(tag->type));         
    return FAILURE;      
  }

  if (tag == &tag_format) {        
    if (e->rank != 1 && e->rank != 0) {      
      g95_error("FORMAT tag at %L cannot be array of strings", &e->where);
      return FAILURE;   
    }      
  } else {
    if (e->rank != 0) {  
      g95_error("%s tag at %L must be scalar", tag->name, &e->where);  
      return FAILURE;  
    }        
  }          
          
  return SUCCESS;  
}       
       
       
     
     
/* match_vtag()-- Match a variable I/O tag of some sort. */     
     
static match match_vtag(io_tag *tag, g95_expr **k) {     
g95_expr *result;       
match x;      
      
  x = g95_match(tag->spec, &result);   
  if (x != MATCH_YES) return x;        
        
  if (*k != NULL) {        
    g95_error("Duplicate %s specification at %C", tag->name);    
    g95_free_expr(result);      
    return MATCH_ERROR;     
  }

  if (result->symbol->attr.intent == INTENT_IN) {       
    g95_error("Variable tag cannot be INTENT(IN) at %C");          
    g95_free_expr(result);     
    return MATCH_ERROR;  
  }        
        
  if (g95_pure(NULL) && g95_impure_variable(result->symbol)) {        
    g95_error("Variable tag cannot be assigned in PURE procedure at %C");  
    g95_free_expr(result);       
    return MATCH_ERROR;     
  }          
          
  *k = result;    
  return MATCH_YES;  
} 
 
 
      
      
/* check_namelist()-- Traverse a namelist that is part of a READ
 * statement to make sure that none of the variables in the namelist
 * are INTENT(IN).  Returns nonzero if we find such a variable */   
   
static int check_namelist(g95_symbol *symb) {       
g95_namelist *q;          
          
  for(q=symb->namelist; q; q=q->next) 
    if (q->sym->attr.intent == INTENT_IN) {         
      g95_error("Symbol '%s' in namelist '%s' is INTENT(IN) at %C",      
		q->sym->name, symb->name);   
      return 1;       
    }      
      
  return 0;          
}        
        
        
        
        
try g95_resolve_filepos(g95_filepos *fp) {    
    
  RESOLVE_TAG(&tag_unit,   fp->unit,   0);        
  RESOLVE_TAG(&tag_iostat, fp->iostat, 1);  
  if (g95_reference_st_label(fp->err, ST_LABEL_TARGET) == FAILURE)          
    return FAILURE; 
 
  return SUCCESS;
}        
        
        
        
        
/* match_out_tag()-- Match I/O tags that cause variables to become
 * redefined. */ 
 
static match match_out_tag(io_tag *tag, g95_expr **result) {   
match g;  
  
  g = match_vtag(tag, result);
  if (g == MATCH_YES) g95_check_do_variable((*result)->symbol);     
     
  return g;        
}     
     
     
    
    
/* terminate_io()-- Attach the data transfer end node */ 
 
static void terminate_io(g95_code *io_code) {        
g95_code *w;   
   
  if (io_code == NULL) io_code = &new_st;   
   
  w = g95_get_code();       
  w->type = EXEC_DT_END;         
  w->ext.dt = new_st.ext.dt;     /* Point to structure that is already there */    
    
  g95_append_code(io_code, w);      
}    
    
    
    
    
/* g95_free_close()-- Free a g95_close structure an all its expressions */

void g95_free_close(g95_close *close) {    
    
  if (close == NULL) return;       
       
  g95_free_expr(close->unit);       
  g95_free_expr(close->iostat);    
  g95_free_expr(close->status);

  g95_free(close);      
} 
 
 
      
      
/* match_ltag()-- Match a label I/O tag */    
    
static match match_ltag(io_tag *tag, g95_st_label **label) {
match j;  
g95_st_label *old;          
          
  old = *label;          
  j = g95_match(tag->spec, label);  
  if (j == MATCH_YES && old != 0) {  
    g95_error("Duplicate %s label specification at %C", tag->name);      
    return MATCH_ERROR;     
  }    
    
  return j;        
}     
     
     


/* match_open_element()-- Match a single tag of an OPEN statement */  
  
static match match_open_element(g95_open *open) {   
match e;         
         
  e = match_etag(&tag_unit, &open->unit);          if (e != MATCH_NO) return e;          
  e = match_out_tag(&tag_iostat, &open->iostat);   if (e != MATCH_NO) return e;    
  e = match_etag(&tag_file, &open->file);          if (e != MATCH_NO) return e;         
  e = match_etag(&tag_status, &open->status);      if (e != MATCH_NO) return e;        
  e = match_etag(&tag_e_access, &open->access);    if (e != MATCH_NO) return e;   
  e = match_etag(&tag_e_form, &open->form);        if (e != MATCH_NO) return e; 
  e = match_etag(&tag_e_recl, &open->recl);        if (e != MATCH_NO) return e;        
  e = match_etag(&tag_e_blank, &open->blank);      if (e != MATCH_NO) return e;       
  e = match_etag(&tag_e_position, &open->position);if (e != MATCH_NO) return e; 
  e = match_etag(&tag_e_action, &open->action);    if (e != MATCH_NO) return e;
  e = match_etag(&tag_e_delim, &open->delim);      if (e != MATCH_NO) return e;   
  e = match_etag(&tag_e_pad, &open->pad);          if (e != MATCH_NO) return e;        
  e = match_ltag(&tag_err, &open->err);            if (e != MATCH_NO) return e;      
      
  return MATCH_NO;        
}  
  
  
   
   
/* g95_resolve_dt()-- Resolve everything in a g95_dt structure */ 
 
try g95_resolve_dt(g95_dt *dt) {  
g95_expr *c; 
 
  RESOLVE_TAG(&tag_format,   dt->format_expr,  0);        
  RESOLVE_TAG(&tag_rec,      dt->rec,          0);     
  RESOLVE_TAG(&tag_advance,  dt->advance,      0);
  RESOLVE_TAG(&tag_iostat,   dt->iostat,       1);      
  RESOLVE_TAG(&tag_size,     dt->size,         1);       
       
  c = dt->io_unit;          
  if (g95_resolve_expr(c) == SUCCESS &&       
      (c->ts.type != BT_INTEGER &&     
       (c->ts.type != BT_CHARACTER || c->type != EXPR_VARIABLE))) {
    g95_error("UNIT specification at %L must be an INTEGER expression or a " 
	      "CHARACTER variable", &c->where);   
    return FAILURE;          
  }         
         
/* Sanity checks on data transfer statements */          
          
  if (c->ts.type == BT_CHARACTER) {        
    if (dt->rec != NULL) { 
      g95_error("REC tag at %L is incompatible with internal file",   
		&dt->rec->where);  
      return FAILURE;   
    }   
   
    if (dt->namelist != NULL) { 
      g95_error("Internal file at %L is incompatible with namelist", 
		&dt->io_unit->where);          
      return FAILURE;     
    }      
      
    if (dt->advance != NULL) {   
      g95_error("ADVANCE tag at %L is incompatible with internal file",        
		&dt->advance->where);       
      return FAILURE;     
    }   
  } 
 
  if (dt->rec != NULL) {     
    if (dt->end != NULL) {   
      g95_error("REC tag at %L is incompatible with END tag", &dt->rec->where);
      return FAILURE;         
    }     
     
    if (dt->format_label == &g95_format_asterisk) {         
      g95_error("END tag at %L is incompatible with list directed format (*)",    
		&dt->end_where);        
      return FAILURE;          
    }    
    
    if (dt->namelist != NULL) {  
      g95_error("REC tag at %L is incompatible with namelist",    
		&dt->rec->where);  
      return FAILURE;        
    }
  }    
    
  if (dt->advance != NULL && dt->format_label == &g95_format_asterisk) {        
    g95_error("ADVANCE tag at %L is incompatible with list directed "  
	      "format (*)", &dt->advance->where);     
    return FAILURE;
  }   
   
  if (dt->eor != 0 && dt->advance == NULL) {      
    g95_error("EOR tag at %L requires an ADVANCE tag", &dt->eor_where);         
    return FAILURE;       
  }       
       
  if (dt->size != NULL && dt->advance == NULL) {
    g95_error("SIZE tag at %L requires an ADVANCE tag", &dt->size->where);    
    return FAILURE;     
  }    
    
/* TODO: Make sure the ADVANCE tag is 'yes' or 'no' if it is a string
 * constant */ 
 
  if (g95_reference_st_label(dt->err, ST_LABEL_TARGET) == FAILURE)        
    return FAILURE;       
       
  if (g95_reference_st_label(dt->end, ST_LABEL_TARGET) == FAILURE)     
    return FAILURE;      
      
  if (g95_reference_st_label(dt->eor, ST_LABEL_TARGET) == FAILURE) 
    return FAILURE;         
         
  if (dt->format_label != NULL &&
      dt->format_label->defined == ST_LABEL_UNKNOWN) {    
    g95_error("Format label '%d' at %L is not defined",     
	      dt->format_label->value, &dt->format_label->where); 
    return FAILURE;  
  }          
          
  return SUCCESS;   
}       
       
       
 
 
/* match_io_iterator()-- Match an IO iteration statement of the form:
 *   ( [<IO element> ,] <IO element>, I = <expr>, <expr> [, <expr> ] )
 *
 * Which is equivalent to a single IO element.  This function is mutually
 * recursive with match_io_element().  */  
  
static match match_io_iterator(io_kind a, g95_code **result) {       
g95_code *head, *tail, *new;  
g95_iterator *iter;         
locus old_loc;     
match q;      
int z; 
 
  iter = NULL; 
  head = tail = NULL;    
  old_loc = *g95_current_locus();     
     
  if (g95_match_char('(') != MATCH_YES) return MATCH_NO;        
        
  q = match_io_element(a, &head);
  tail = head;      
      
  if (q != MATCH_YES || g95_match_char(',') != MATCH_YES) {         
    q = MATCH_NO;          
    goto cleanup;        
  }         
         
/* Can't be anything but an IO iterator.  Build a list */

  iter = g95_get_iterator();          
          
  for(z=1;; z++) {          
    q = g95_match_iterator(iter, 0);
    if (q == MATCH_ERROR) goto cleanup;  
    if (q == MATCH_YES) {   
      g95_check_do_variable(iter->var->symbol);
      break; 
    }     
     
    q = match_io_element(a, &new);         
    if (q == MATCH_ERROR) goto cleanup;   
    if (q == MATCH_NO) {  
      if (z > 2) goto syntax;   
      goto cleanup;          
    }         
         
    tail = g95_append_code(tail, new);       
       
    if (g95_match_char(',') != MATCH_YES) {          
      if (z > 2) goto syntax;    
      q = MATCH_NO;     
      goto cleanup;      
    }        
  }         
         
  if (g95_match_char(')') != MATCH_YES) goto syntax;   
   
  new = g95_get_code();      
  new->type = EXEC_DO;          
  new->ext.iterator = iter;         
  new->block = head;      
      
  *result = new; 
  return MATCH_YES;    
    
syntax:     
  g95_error("Syntax error in I/O iterator at %C");
  q = MATCH_ERROR;         
         
cleanup:      
  g95_free_iterator(iter, 1);
  g95_free_statements(head); 
  g95_set_locus(&old_loc);  
  return q;        
}   
   
   
      
      
/* match_file_element()-- Match elements of a REWIND, BACKSPACE or
 * ENDFILE statement */         
         
static match match_file_element(g95_filepos *fp) { 
match u;   
   
  u = match_etag(&tag_unit, &fp->unit);          if (u != MATCH_NO) return u;         
  u = match_out_tag(&tag_iostat, &fp->iostat);   if (u != MATCH_NO) return u;          
  u = match_ltag(&tag_err, &fp->err);            if (u != MATCH_NO) return u;          
          
  return MATCH_NO;     
}


      
      
/* g95_resolve_open()-- resolve everything in a g95_open structure */         
         
try g95_resolve_open(g95_open *open) {          
          
  RESOLVE_TAG(&tag_unit,    open->unit,    0);         
  RESOLVE_TAG(&tag_iostat,  open->iostat,  1);  
  RESOLVE_TAG(&tag_file,    open->file,    0);     
  RESOLVE_TAG(&tag_status,  open->status,  0);  
  RESOLVE_TAG(&tag_e_form,  open->form,    0);       
  RESOLVE_TAG(&tag_e_recl,  open->recl,    0);     
     
  RESOLVE_TAG(&tag_e_blank,     open->blank,     0); 
  RESOLVE_TAG(&tag_e_position,  open->position,  0);         
  RESOLVE_TAG(&tag_e_action,    open->action,    0);         
  RESOLVE_TAG(&tag_e_delim,     open->delim,     0);    
  RESOLVE_TAG(&tag_e_pad,       open->pad,       0); 
 
  if (g95_reference_st_label(open->err, ST_LABEL_TARGET) == FAILURE)          
    return FAILURE;  
  
  return SUCCESS;    
}      
      
      


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


   
   
/* match_dt_unit()-- Match a unit specification for a data transfer
 * statement */    
    
static match match_dt_unit(g95_dt *dt) {        
g95_expr *v;  
  
  if (g95_match_char('*') == MATCH_YES) {         
    if (dt->io_unit != NULL) goto conflict;     
     
    dt->io_unit = default_unit();  
    return MATCH_YES;         
  }     
     
  if (g95_match_expr(&v) == MATCH_YES) {  
    if (dt->io_unit != NULL) { 
      g95_free_expr(v);  
      goto conflict;  
    } 
 
    dt->io_unit = v;        
    return MATCH_YES;          
  }

  return MATCH_NO;    
    
conflict:     
  g95_error("Duplicate UNIT specification at %C");        
  return MATCH_ERROR;        
}         
         
         
     
     
/* match_close_element()-- Match elements of a CLOSE statment */      
      
static match match_close_element(g95_close *close) {        
match j;         
         
  j = match_etag(&tag_unit, &close->unit);        if (j != MATCH_NO) return j;       
  j = match_etag(&tag_status, &close->status);    if (j != MATCH_NO) return j; 
  j = match_out_tag(&tag_iostat, &close->iostat); if (j != MATCH_NO) return j;        
  j = match_ltag(&tag_err, &close->err);          if (j != MATCH_NO) return j;         
         
  return MATCH_NO;    
}         
         
         


/* g95_match_close()-- Match a CLOSE statement */    
    
match g95_match_close(void) {          
g95_close *close;      
match g;

  g = g95_match_char('(');   
  if (g == MATCH_NO) return g;     
     
  close = g95_getmem(sizeof(g95_close));

  g = match_close_element(close);      
      
  if (g == MATCH_ERROR) goto cleanup;          
  if (g == MATCH_NO) {
    g = g95_match_expr(&close->unit);    
    if (g == MATCH_NO) goto syntax;     
    if (g == MATCH_ERROR) goto cleanup;        
  }

  for(;;) {          
    if (g95_match_char(')') == MATCH_YES) break;   
    if (g95_match_char(',') != MATCH_YES) goto syntax;     
     
    g = match_close_element(close);   
    if (g == MATCH_ERROR) goto cleanup;   
    if (g == MATCH_NO) goto syntax;    
  }   
   
  if (g95_match_eos() == MATCH_NO) goto syntax;      
      
  if (g95_pure(NULL)) {  
    g95_error("CLOSE statement not allowed in PURE procedure at %C");
    return MATCH_ERROR;  
  }   
   
  new_st.type = EXEC_CLOSE;          
  new_st.ext.close = close;      
  return MATCH_YES;

syntax:     
  g95_syntax_error(ST_CLOSE);       
       
cleanup:      
  g95_free_close(close);   
  return MATCH_ERROR;      
}        
        
        
     
     
/* g95_free_filepos()-- Free a g95_filepos structure */         
         
void g95_free_filepos(g95_filepos *fp) {  
  
  g95_free_expr(fp->unit);
  g95_free_expr(fp->iostat);    
  g95_free(fp); 
}       
       
       
  
  
/* g95_match_open()-- Match an OPEN statmement */        
        
match g95_match_open(void) {    
g95_open *open;     
match o;       
       
  o = g95_match_char('(');       
  if (o == MATCH_NO) return o;

  open = g95_getmem(sizeof(g95_open));         
         
  o = match_open_element(open);

  if (o == MATCH_ERROR) goto cleanup;      
  if (o == MATCH_NO) {   
    o = g95_match_expr(&open->unit);     
    if (o == MATCH_NO) goto syntax;   
    if (o == MATCH_ERROR) goto cleanup;        
  }   
   
  for(;;) {       
    if (g95_match_char(')') == MATCH_YES) break; 
    if (g95_match_char(',') != MATCH_YES) goto syntax; 
 
    o = match_open_element(open);   
    if (o == MATCH_ERROR) goto cleanup;  
    if (o == MATCH_NO) goto syntax;        
  }    
    
  if (g95_match_eos() == MATCH_NO) goto syntax;       
       
  if (g95_pure(NULL)) {    
    g95_error("OPEN statement not allowed in PURE procedure at %C");      
    return MATCH_ERROR;         
  }         
         
  new_st.type = EXEC_OPEN;          
  new_st.ext.open = open;     
  return MATCH_YES; 
 
syntax:   
  g95_syntax_error(ST_OPEN);  
  
cleanup:      
  g95_free_open(open);      
  return MATCH_ERROR; 
}  
  
  
        
        
/* match_dt_format()-- Match a format specification */  
  
static match match_dt_format(g95_dt *dt) {   
locus where;     
g95_expr *c;      
g95_st_label *label;

  where = *g95_current_locus();   
   
  if (g95_match_char('*') == MATCH_YES) {
    if (dt->format_expr != NULL || dt->format_label != NULL) goto conflict;          
          
    dt->format_label = &g95_format_asterisk;  
    return MATCH_YES; 
  }  
  
  if (g95_match_st_label(&label, 0) == MATCH_YES) {
    if (dt->format_expr != NULL || dt->format_label != NULL) {  
      g95_free_st_label(label);          
      goto conflict;    
    }

    if (g95_reference_st_label(label, ST_LABEL_FORMAT) == FAILURE)       
      return MATCH_ERROR;         
         
    dt->format_label = label;         
    return MATCH_YES;      
  }      
      
  if (g95_match_expr(&c) == MATCH_YES) {    
    if (dt->format_expr != NULL || dt->format_label != NULL) {      
      g95_free_expr(c);   
      goto conflict; 
    }        
        
    dt->format_expr = c;         
    return MATCH_YES;
  }     
     
  g95_set_locus(&where);    /* The only case where we have to restore */    
    
  return MATCH_NO;       
       
conflict:        
  g95_error("Duplicate format specification at %C");    
  return MATCH_ERROR;    
}          
          
          
   
   
/* match_filepos()-- Match the second half of the file-positioning
 * statements, REWIND, BACKSPACE or ENDFILE. */  
  
static match match_filepos(g95_statement st, g95_exec_op op) {       
g95_filepos *fp;        
match s;     
     
  fp = g95_getmem(sizeof(g95_filepos));        
        
  if (g95_match_char('(') == MATCH_NO) {      
    s = g95_match_expr(&fp->unit);      
    if (s == MATCH_ERROR) goto cleanup; 
    if (s == MATCH_NO) goto syntax;  
  
    goto done;          
  }   
   
  s = match_file_element(fp);          
  if (s == MATCH_ERROR) goto done;     
  if (s == MATCH_NO) {  
    s = g95_match_expr(&fp->unit);        
    if (s == MATCH_ERROR) goto done;        
    if (s == MATCH_NO) goto syntax;         
  }      
      
  for(;;) {         
    if (g95_match_char(')') == MATCH_YES) break;         
    if (g95_match_char(',') != MATCH_YES) goto syntax; 
 
    s = match_file_element(fp);     
    if (s == MATCH_ERROR) goto cleanup;       
    if (s == MATCH_NO) goto syntax;      
  }          
          
done:        
  if (g95_match_eos() != MATCH_YES) goto syntax;        
        
  if (g95_pure(NULL)) {
    g95_error("%s statement not allowed in PURE procedure at %C",       
	      g95_ascii_statement(st));         
         
    return MATCH_ERROR;         
  }   
   
  new_st.type = op;   
  new_st.ext.filepos = fp;  
  return MATCH_YES;  
  
syntax:       
  g95_syntax_error(st);    
    
cleanup:         
  g95_free_filepos(fp);     
  return MATCH_ERROR;  
}       
       
       
       
       
match g95_match_endfile(void) {    
    
  return match_filepos(ST_END_FILE, EXEC_ENDFILE);        
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
     
     
        
        
/* g95_resolve_inquire()-- Resolve everything in a g95_inquire structure */          
          
try g95_resolve_inquire(g95_inquire *inquire) {       
       
  RESOLVE_TAG(&tag_unit,         inquire->unit,         0);          
  RESOLVE_TAG(&tag_file,         inquire->file,         0);          
  RESOLVE_TAG(&tag_iostat,       inquire->iostat,       1);   
  RESOLVE_TAG(&tag_exist,        inquire->exist,        1);        
  RESOLVE_TAG(&tag_opened,       inquire->opened,       1);  
  RESOLVE_TAG(&tag_number,       inquire->number,       1);
  RESOLVE_TAG(&tag_named,        inquire->named,        1);   
  RESOLVE_TAG(&tag_name,         inquire->name,         1);
  RESOLVE_TAG(&tag_s_access,     inquire->access,       1);  
  RESOLVE_TAG(&tag_sequential,   inquire->sequential,   1);         
  RESOLVE_TAG(&tag_direct,       inquire->direct,       1);       
  RESOLVE_TAG(&tag_s_form,       inquire->form,         1);          
  RESOLVE_TAG(&tag_formatted,    inquire->formatted,    1);
  RESOLVE_TAG(&tag_unformatted,  inquire->unformatted,  1); 
  RESOLVE_TAG(&tag_s_recl,       inquire->recl,         1);
  RESOLVE_TAG(&tag_nextrec,      inquire->nextrec,      1);     
  RESOLVE_TAG(&tag_s_blank,      inquire->blank,        1);    
  RESOLVE_TAG(&tag_s_position,   inquire->position,     1);  
  RESOLVE_TAG(&tag_s_action,     inquire->action,       1);        
  RESOLVE_TAG(&tag_read,         inquire->read,         1);       
  RESOLVE_TAG(&tag_write,        inquire->write,        1);         
  RESOLVE_TAG(&tag_readwrite,    inquire->readwrite,    1);          
  RESOLVE_TAG(&tag_s_delim,      inquire->delim,        1);    
  RESOLVE_TAG(&tag_s_pad,        inquire->pad,          1);   
   
  if (g95_reference_st_label(inquire->err, ST_LABEL_TARGET) == FAILURE) 
    return FAILURE;          
          
  return FAILURE;        
}   
   
   
     
     
/* match_dt_element()-- Match a single data transfer element */  
  
static match match_dt_element(io_kind u, g95_dt *dt) {
char name[G95_MAX_SYMBOL_LEN+1];     
g95_symbol *symbol;
match z; 
 
  if (g95_match(" unit =") == MATCH_YES) {       
    z = match_dt_unit(dt);  
    if (z != MATCH_NO) return z;   
  }          
          
  if (g95_match(" fmt =") == MATCH_YES) {      
    z = match_dt_format(dt);       
    if (z != MATCH_NO) return z;  
  }   
   
  if (g95_match(" nml = %n", name) == MATCH_YES) {  
    if (dt->namelist != NULL) {    
      g95_error("Duplicate NML specification at %C");      
      return MATCH_ERROR;  
    }       
       
    if (g95_find_symbol(name, NULL, 1, &symbol)) return MATCH_ERROR;   
   
    if (symbol == NULL || symbol->attr.flavor != FL_NAMELIST) {  
      g95_error("Symbol '%s' at %C must be a NAMELIST group name",
		symbol != NULL ? symbol->name : name );  
      return MATCH_ERROR;      
    }          
          
    dt->namelist = symbol; 
    if (u == M_READ && check_namelist(symbol)) return MATCH_ERROR;   
   
    return MATCH_YES;      
  }    
    
  z = match_etag(&tag_rec, &dt->rec);           if (z != MATCH_NO) return z;  
  z = match_out_tag(&tag_iostat, &dt->iostat);  if (z != MATCH_NO) return z;     
  z = match_ltag(&tag_err, &dt->err);           if (z != MATCH_NO) return z; 
  z = match_etag(&tag_advance, &dt->advance);   if (z != MATCH_NO) return z;      
  z = match_out_tag(&tag_size, &dt->size);      if (z != MATCH_NO) return z;      
      
  z = match_ltag(&tag_end, &dt->end);       
  if (z == MATCH_YES) dt->end_where = *g95_current_locus();    
  if (z != MATCH_NO) return z;   
   
  z = match_ltag(&tag_eor, &dt->eor);          
  if (z == MATCH_YES) dt->eor_where = *g95_current_locus();     
  if (z != MATCH_NO) return z;         
         
  return MATCH_NO;     
}    
    
    
       
       
/* io_kind_name()-- Given an io_kind, return its name */   
   
static char *io_kind_name(io_kind c) {   
char *name;       
       
  switch(c) {        
  case M_READ:     name = "READ";     break;   
  case M_WRITE:    name = "WRITE";    break;     
  case M_PRINT:    name = "PRINT";    break;         
  case M_INQUIRE:  name = "INQUIRE";  break;      
  default:          
    g95_internal_error("io_kind_name(): bad I/O-kind");  
  }      
      
  return name;   
}          
          
          
        
        
/* match_io_element()-- Match a single element of an IO list, which is
 * either a single expression or an IO Iterator */  
  
static match match_io_element(io_kind t, g95_code **cpp) {      
g95_expr *expr;          
g95_code *cp;       
match n; 
 
  expr = NULL;          
          
  n = match_io_iterator(t, cpp);    
  if (n == MATCH_YES) return MATCH_YES;         
         
  if (t == M_READ) {       
    n = g95_match_variable(&expr, 0);     
    if (n == MATCH_NO) g95_error("Expected variable in READ statement at %C");     
  } else {  
    n = g95_match_expr(&expr);    
    if (n == MATCH_NO) g95_error("Expected expression in %s statement at %C",    
				 io_kind_name(t));   
  }     
     
  if (n == MATCH_YES)          
    switch(t) {   
    case M_READ:        
      if (expr->symbol->attr.intent == INTENT_IN) {       
	g95_error("Variable '%s' in input list at %C cannot be INTENT(IN)",  
		  expr->symbol->name);   
	n = MATCH_ERROR;
      }  
  
      if (g95_pure(NULL) && g95_impure_variable(expr->symbol) &&    
	  current_dt->io_unit->ts.type == BT_CHARACTER) { 
	g95_error("Cannot read to variable '%s' in PURE procedure at %C",       
		  expr->symbol->name);      
	n = MATCH_ERROR;        
      }   
   
      break;        
        
    case M_WRITE:         
      if (current_dt->io_unit->ts.type == BT_CHARACTER &&   
	  g95_pure(NULL) && current_dt->io_unit->type == EXPR_VARIABLE &&        
	  g95_impure_variable(current_dt->io_unit->symbol)) {     
	g95_error("Cannot write to internal file unit '%s' at %C inside a "      
		  "PURE procedure", current_dt->io_unit->symbol->name);    
	n = MATCH_ERROR;  
      } 
 
      break;  
  
    default:     
      break;
    }    
    
  if (n != MATCH_YES) {          
    g95_free_expr(expr);     
    return MATCH_ERROR;          
  }      
      
  cp = g95_get_code();
  cp->type = EXEC_TRANSFER;         
  cp->expr = expr;     
     
  *cpp = cp;        
  return MATCH_YES;   
}       
       
       
        
        
/* match_io_list()-- Match an I/O list, building g95_code structures
 * as we go. */   
   
static match match_io_list(io_kind o, g95_code **head_p) {          
g95_code *head, *tail, *new;   
match s;          
          
  *head_p = head = tail = NULL;
  if (g95_match_eos() == MATCH_YES) return MATCH_YES;

  for(;;) {
    s = match_io_element(o, &new);     
    if (s == MATCH_ERROR) goto cleanup;    
    if (s == MATCH_NO) goto syntax;      
      
    tail = g95_append_code(tail, new);   
    if (head == NULL) head = new;   
   
    if (g95_match_eos() == MATCH_YES) break;         
    if (g95_match_char(',') != MATCH_YES) goto syntax;        
  }   
   
  *head_p = head;       
  return MATCH_YES;       
       
syntax:   
  g95_error("Syntax error in %s statement at %C", io_kind_name(o));       
       
cleanup:      
  g95_free_statements(head);  
  return MATCH_ERROR;    
}          
          
          
         
         
/* g95_resolve_close()-- Resolve everything in a g95_close structure */ 
 
try g95_resolve_close(g95_close *close) {

  RESOLVE_TAG(&tag_unit,    close->unit,    0); 
  RESOLVE_TAG(&tag_iostat,  close->iostat,  1);     
  RESOLVE_TAG(&tag_status,  close->status,  0);  
  
  if (g95_reference_st_label(close->err, ST_LABEL_TARGET) == FAILURE)      
    return FAILURE;  
  
  return SUCCESS;    
}


   
   
/* match_inquire_element()-- Match an element of an INQUIRE statement */   
   
static match match_inquire_element(g95_inquire *inquire) {      
match w;       
       
  w = match_etag(&tag_unit, &inquire->unit);          
  if (w != MATCH_NO) return w;        
        
  w = match_etag(&tag_file, &inquire->file);
  if (w != MATCH_NO) return w;         
         
  w = match_ltag(&tag_err, &inquire->err);  
  if (w != MATCH_NO) return w;     
     
  w = match_out_tag(&tag_iostat, &inquire->iostat);  
  if (w != MATCH_NO) return w;   
   
  w = match_vtag(&tag_exist, &inquire->exist);        
  if (w != MATCH_NO) return w;      
      
  w = match_vtag(&tag_opened, &inquire->opened);     
  if (w != MATCH_NO) return w;     
     
  w = match_vtag(&tag_named, &inquire->named);        
  if (w != MATCH_NO) return w;       
       
  w = match_vtag(&tag_name, &inquire->name);         
  if (w != MATCH_NO) return w;    
    
  w = match_out_tag(&tag_number, &inquire->number);         
  if (w != MATCH_NO) return w;         
         
  w = match_vtag(&tag_s_access, &inquire->access); 
  if (w != MATCH_NO) return w;  
  
  w = match_vtag(&tag_sequential, &inquire->sequential); 
  if (w != MATCH_NO) return w; 
 
  w = match_vtag(&tag_direct, &inquire->direct);       
  if (w != MATCH_NO) return w;    
    
  w = match_vtag(&tag_s_form, &inquire->form);    
  if (w != MATCH_NO) return w;         
         
  w = match_vtag(&tag_formatted, &inquire->formatted);   
  if (w != MATCH_NO) return w;     
     
  w = match_vtag(&tag_unformatted, &inquire->unformatted);         
  if (w != MATCH_NO) return w;  
  
  w = match_out_tag(&tag_s_recl, &inquire->recl);   
  if (w != MATCH_NO) return w;       
       
  w = match_out_tag(&tag_nextrec, &inquire->nextrec);       
  if (w != MATCH_NO) return w;   
   
  w = match_vtag(&tag_s_blank, &inquire->blank);   
  if (w != MATCH_NO) return w;       
       
  w = match_vtag(&tag_s_position, &inquire->position);  
  if (w != MATCH_NO) return w; 
 
  w = match_vtag(&tag_s_action, &inquire->action);  
  if (w != MATCH_NO) return w;       
       
  w = match_vtag(&tag_read, &inquire->read);        
  if (w != MATCH_NO) return w;   
   
  w = match_vtag(&tag_write, &inquire->write); 
  if (w != MATCH_NO) return w;    
    
  w = match_vtag(&tag_readwrite, &inquire->readwrite); 
  if (w != MATCH_NO) return w;    
    
  w = match_vtag(&tag_s_delim, &inquire->delim);  
  if (w != MATCH_NO) return w;     
     
  w = match_vtag(&tag_s_pad, &inquire->pad);
  if (w != MATCH_NO) return w;

  w = match_vtag(&tag_iolength, &inquire->iolength);         
  if (w != MATCH_NO) return w;      
      
  return MATCH_NO;
}


   
   
match g95_match_rewind(void) {        
        
  return match_filepos(ST_REWIND, EXEC_REWIND);  
}        
        
        
  
  
/* g95_io_init()-- Initialize I/O things. */    
    
void g95_io_init(void) {        
        
  g95_format_asterisk.value = -1;         
  g95_format_asterisk.format = NULL;       
  g95_format_asterisk.length = 0;       
}    
       
       
match g95_match_inquire(void) { 
g95_inquire *inquire; 
g95_code *code;   
match o;  
  
  o = g95_match_char('('); 
  if (o == MATCH_NO) return o;    
    
  inquire = g95_getmem(sizeof(g95_inquire));  
  
  o = match_inquire_element(inquire);       
  if (o == MATCH_ERROR) goto cleanup;
  if (o == MATCH_NO) {  
    o = g95_match_expr(&inquire->unit);          
    if (o == MATCH_ERROR) goto cleanup; 
    if (o == MATCH_NO) goto syntax;  
  }   
   
/* See if we have the IOLENGTH form of the inquire statement */       
       
  if (inquire->iolength != NULL) {  
    if (g95_match_char(')') != MATCH_YES) goto syntax;       
       
    o = match_io_list(M_INQUIRE, &code);       
    if (o == MATCH_ERROR) goto cleanup;    
    if (o == MATCH_NO) goto syntax;       
       
    terminate_io(code);     
     
    new_st.type = EXEC_IOLENGTH;        
    new_st.expr = inquire->iolength;     
    g95_free(inquire); 
 
    if (g95_pure(NULL)) {   
      g95_free_statements(code);     
      g95_error("INQUIRE statement not allowed in PURE procedure at %C");      
      return MATCH_ERROR;         
    }         
         
    new_st.next = code;
    return MATCH_YES;  
  }

/* At this point, we have the non-IOLENGTH inquire statement */      
      
  for(;;) {  
    if (g95_match_char(')') == MATCH_YES) break;    
    if (g95_match_char(',') != MATCH_YES) goto syntax;      
      
    o = match_inquire_element(inquire);        
    if (o == MATCH_ERROR) goto cleanup;      
    if (o == MATCH_NO) goto syntax;         
         
    if (inquire->iolength != NULL) {  
      g95_error("IOLENGTH tag invalid in INQUIRE statement at %C");
      goto cleanup;     
    }        
  }          
          
  if (g95_match_eos() != MATCH_YES) goto syntax;         
         
  if (g95_pure(NULL)) {          
    g95_error("INQUIRE statement not allowed in PURE procedure at %C");   
    return MATCH_ERROR;
  }      
      
  new_st.type = EXEC_INQUIRE;       
  new_st.ext.inquire = inquire;         
  return MATCH_YES;          
          
syntax: 
  g95_syntax_error(ST_INQUIRE);    
    
cleanup:     
  g95_free_inquire(inquire); 
  return MATCH_ERROR; 
}       
       
       
         
         
/* match_io()-- Match a READ, WRITE or PRINT statement. */     
     
static match match_io(io_kind w) { 
char name[G95_MAX_SYMBOL_LEN+1];         
g95_code *io_code;  
g95_symbol *symb;     
g95_expr *expr;       
int comma_flag;         
locus where;      
g95_dt *dt;    
match n;

  n = MATCH_NO;      
      
  comma_flag = 0;        
  current_dt = dt = g95_getmem(sizeof(g95_dt)); 
 
  if (g95_match_char('(') == MATCH_NO) {          
    if (w == M_WRITE) goto syntax; 
 
    n = match_dt_format(dt);       
    if (n == MATCH_ERROR) goto cleanup;          
    if (n == MATCH_NO) goto syntax;         
         
    comma_flag = 1;    
    dt->io_unit = default_unit();    
    goto get_io_list; 
  }     
     
/* Match a control list */          
          
  if (match_dt_element(w, dt) == MATCH_YES) goto next;          
  if (match_dt_unit(dt) != MATCH_YES) goto loop; 
 
  if (g95_match_char(')') == MATCH_YES) goto get_io_list;          
  if (g95_match_char(',') != MATCH_YES) goto syntax;

  n = match_dt_element(w, dt);      
  if (n == MATCH_YES) goto next; 
  if (n == MATCH_ERROR) goto cleanup;       
       
  n = match_dt_format(dt); 
  if (n == MATCH_YES) goto next;   
  if (n == MATCH_ERROR) goto cleanup;       
       
  where = *g95_current_locus();       
       
  if (g95_match_name(name) == MATCH_YES &&       
      !g95_find_symbol(name, NULL, 1, &symb) &&       
      symb->attr.flavor == FL_NAMELIST) {   
    dt->namelist = symb;        
    if (w == M_READ && check_namelist(symb)) {        
      n = MATCH_ERROR;      
      goto cleanup;
    }          
          
    goto next;         
  }    
    
  g95_set_locus(&where);      
      
  goto loop;   /* No matches, try regular elements */

next:       
  if (g95_match_char(')') == MATCH_YES) goto get_io_list;        
  if (g95_match_char(',') != MATCH_YES) goto syntax;    
    
loop:      
  for(;;) {          
    n = match_dt_element(w, dt); 
    if (n == MATCH_NO) goto syntax;
    if (n == MATCH_ERROR) goto cleanup;  
  
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
      n = MATCH_ERROR;        
      goto cleanup;    
    }       
       
    n = match_io_list(w, &io_code);  
    if (n == MATCH_ERROR) goto cleanup;         
    if (n == MATCH_NO) goto syntax;          
  }

/* A full IO statement has been matched */     
     
  if (dt->io_unit->type == EXPR_VARIABLE && w == M_WRITE &&    
      dt->io_unit->ts.type == BT_CHARACTER &&
      dt->io_unit->symbol->attr.intent == INTENT_IN) {         
    g95_error("Internal file '%s' at %L is INTENT(IN)", 
	      dt->io_unit->symbol->name, &dt->io_unit->where);
    n = MATCH_ERROR;     
    goto cleanup;       
  }      
      
  expr = dt->format_expr; 
 
  if (expr != NULL && expr->type == EXPR_CONSTANT)  
    g95_check_format_string(expr);      
      
  if (g95_pure(NULL) && (w == M_READ || w == M_WRITE) &&          
      dt->io_unit->ts.type != BT_CHARACTER) {   
    g95_error("io-unit in %s statement at %C must be an internal file in a "     
	      "PURE procedure", io_kind_name(w)); 
    n = MATCH_ERROR;    
    goto cleanup;          
  }         
         
  new_st.type = (w == M_READ) ? EXEC_READ : EXEC_WRITE;
  new_st.ext.dt = dt;     
  new_st.next = io_code;        
        
  terminate_io(io_code);     
     
  return MATCH_YES;  
  
syntax:       
  g95_error("Syntax error in %s statement at %C", io_kind_name(w));       
  n = MATCH_ERROR;       
       
cleanup:  
  g95_free_dt(dt);  
  return n; 
}        
        
        
       
       
match g95_match_backspace(void) {  
  
  return match_filepos(ST_BACKSPACE, EXEC_BACKSPACE); 
}         
         
         
    
    
match g95_match_print(void) {    
match q;   
   
  q = match_io(M_PRINT);       
  if (q != MATCH_YES) return q;

  if (g95_pure(NULL)) { 
    g95_error("PRINT statement at %C not allowed within PURE procedure");   
    return MATCH_ERROR; 
  }       
       
  return MATCH_YES;  
}         
         
         
    
    
match g95_match_read(void)  { return match_io(M_READ);  }   
   
       
       
match g95_match_write(void) { return match_io(M_WRITE); }       
       
