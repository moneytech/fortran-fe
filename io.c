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
        
        
    
    
/* g95_free_filepos()-- Free a g95_filepos structure */        
        
void g95_free_filepos(g95_filepos *fp) {  
  
  g95_free_expr(fp->unit);       
  g95_free_expr(fp->iostat); 
  g95_free(fp);          
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
       
       
     
     
/* g95_io_init()-- Initialize I/O things. */ 
 
void g95_io_init(void) { 
 
  g95_format_asterisk.value = -1;      
  g95_format_asterisk.format = NULL;  
  g95_format_asterisk.length = 0; 
}          
    
    
/* resolve_tag()-- Do expression resolution and type-checking on an
 * expression tag */ 
 
static try resolve_tag(io_tag *tag, g95_expr *w, int output) {

  if (w == NULL) return SUCCESS;

  if (g95_resolve_expr(w) == FAILURE) return FAILURE;   
   
  if (output && w->type == EXPR_VARIABLE) w->symbol->attr.set = 1;        
        
  if (w->ts.type != tag->type) {  
    g95_error("%s tag at %L must be of type %s", tag->name, &w->where,      
	      g95_basic_typename(tag->type));        
    return FAILURE;  
  }      
      
  if (tag == &tag_format) {     
    if (w->rank != 1 && w->rank != 0) {         
      g95_error("FORMAT tag at %L cannot be array of strings", &w->where);    
      return FAILURE;  
    }          
  } else {      
    if (w->rank != 0) {         
      g95_error("%s tag at %L must be scalar", tag->name, &w->where);
      return FAILURE;  
    }       
  }        
        
  return SUCCESS; 
}          
          
          


/* match_vtag()-- Match a variable I/O tag of some sort. */    
    
static match match_vtag(io_tag *tag, g95_expr **i) {   
g95_expr *res; 
match b;     
     
  b = g95_match(tag->spec, &res); 
  if (b != MATCH_YES) return b;    
    
  if (*i != NULL) {    
    g95_error("Duplicate %s specification at %C", tag->name);  
    g95_free_expr(res); 
    return MATCH_ERROR;         
  }

  if (res->symbol->attr.intent == INTENT_IN) { 
    g95_error("Variable tag cannot be INTENT(IN) at %C"); 
    g95_free_expr(res);      
    return MATCH_ERROR;         
  }

  if (g95_pure(NULL) && g95_impure_variable(res->symbol)) {       
    g95_error("Variable tag cannot be assigned in PURE procedure at %C");  
    g95_free_expr(res);  
    return MATCH_ERROR;    
  }          
          
  *i = res; 
  return MATCH_YES;          
}          
          
          
     
     
/* g95_resolve_dt()-- Resolve everything in a g95_dt structure */ 
 
try g95_resolve_dt(g95_dt *d) {   
g95_expr *h; 
 
  RESOLVE_TAG(&tag_format,   d->format_expr,  0);   
  RESOLVE_TAG(&tag_rec,      d->rec,          0);     
  RESOLVE_TAG(&tag_advance,  d->advance,      0);       
  RESOLVE_TAG(&tag_iostat,   d->iostat,       1);     
  RESOLVE_TAG(&tag_size,     d->size,         1);  
  
  h = d->io_unit;  
  if (g95_resolve_expr(h) == SUCCESS &&         
      (h->ts.type != BT_INTEGER &&         
       (h->ts.type != BT_CHARACTER || h->type != EXPR_VARIABLE))) {  
    g95_error("UNIT specification at %L must be an INTEGER expression or a "         
	      "CHARACTER variable", &h->where);
    return FAILURE;     
  }     
     
/* Sanity checks on data transfer statements */  
  
  if (h->ts.type == BT_CHARACTER) { 
    if (d->rec != NULL) {   
      g95_error("REC tag at %L is incompatible with internal file",   
		&d->rec->where);   
      return FAILURE;        
    } 
 
    if (d->namelist != NULL) {        
      g95_error("Internal file at %L is incompatible with namelist",          
		&d->io_unit->where); 
      return FAILURE;       
    }

    if (d->advance != NULL) {      
      g95_error("ADVANCE tag at %L is incompatible with internal file",          
		&d->advance->where);        
      return FAILURE;         
    }
  }      
      
  if (d->rec != NULL) {        
    if (d->end != NULL) {         
      g95_error("REC tag at %L is incompatible with END tag", &d->rec->where);   
      return FAILURE;     
    }    
    
    if (d->format_label == &g95_format_asterisk) {      
      g95_error("END tag at %L is incompatible with list directed format (*)",    
		&d->end_where); 
      return FAILURE;    
    }         
         
    if (d->namelist != NULL) {      
      g95_error("REC tag at %L is incompatible with namelist",    
		&d->rec->where);  
      return FAILURE;         
    }   
  } 
 
  if (d->advance != NULL && d->format_label == &g95_format_asterisk) {         
    g95_error("ADVANCE tag at %L is incompatible with list directed "       
	      "format (*)", &d->advance->where);  
    return FAILURE; 
  } 
 
  if (d->eor != 0 && d->advance == NULL) {    
    g95_error("EOR tag at %L requires an ADVANCE tag", &d->eor_where);          
    return FAILURE;         
  } 
 
  if (d->size != NULL && d->advance == NULL) {       
    g95_error("SIZE tag at %L requires an ADVANCE tag", &d->size->where);         
    return FAILURE;          
  }  
  
/* TODO: Make sure the ADVANCE tag is 'yes' or 'no' if it is a string
 * constant */        
        
  if (g95_reference_st_label(d->err, ST_LABEL_TARGET) == FAILURE)         
    return FAILURE;         
         
  if (g95_reference_st_label(d->end, ST_LABEL_TARGET) == FAILURE)         
    return FAILURE;        
        
  if (g95_reference_st_label(d->eor, ST_LABEL_TARGET) == FAILURE)       
    return FAILURE;   
   
  if (d->format_label != NULL && 
      d->format_label->defined == ST_LABEL_UNKNOWN) {
    g95_error("Format label '%d' at %L is not defined",   
	      d->format_label->value, &d->format_label->where);   
    return FAILURE;      
  }       
       
  return SUCCESS;   
}


   
   
/* match_out_tag()-- Match I/O tags that cause variables to become
 * redefined. */

static match match_out_tag(io_tag *tag, g95_expr **rslt) { 
match t;          
          
  t = match_vtag(tag, rslt); 
  if (t == MATCH_YES) g95_check_do_variable((*rslt)->symbol);     
     
  return t;      
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
          
          
       
       
/* match_ltag()-- Match a label I/O tag */    
    
static match match_ltag(io_tag *tag, g95_st_label **l) {      
match y;  
g95_st_label *o;   
   
  o = *l;    
  y = g95_match(tag->spec, l);    
  if (y == MATCH_YES && o != 0) { 
    g95_error("Duplicate %s label specification at %C", tag->name);  
    return MATCH_ERROR;         
  }      
      
  return y;          
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
         
         
  
  
try g95_resolve_filepos(g95_filepos *fp2) {  
  
  RESOLVE_TAG(&tag_unit,   fp2->unit,   0);          
  RESOLVE_TAG(&tag_iostat, fp2->iostat, 1);      
  if (g95_reference_st_label(fp2->err, ST_LABEL_TARGET) == FAILURE)         
    return FAILURE;     
     
  return SUCCESS;       
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
          
          


/* match_etag()-- Match an expression I/O tag of some sort. */      
      
static match match_etag(io_tag *tag, g95_expr **u) {          
g95_expr *result;     
match j;    
    
  j = g95_match(tag->spec, &result);     
  if (j != MATCH_YES) return j;          
          
  if (*u != NULL) {        
    g95_error("Duplicate %s specification at %C", tag->name);  
    g95_free_expr(result);         
    return MATCH_ERROR;   
  }          
          
  *u = result;          
  return MATCH_YES;          
}         
         
         
       
       
/* match_open_element()-- Match a single tag of an OPEN statement */    
    
static match match_open_element(g95_open *open) {        
match m;     
     
  m = match_etag(&tag_unit, &open->unit);          if (m != MATCH_NO) return m;  
  m = match_out_tag(&tag_iostat, &open->iostat);   if (m != MATCH_NO) return m;     
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
  
  
 
 
/* terminate_io()-- Attach the data transfer end node */        
        
static void terminate_io(g95_code *io_code) {     
g95_code *x; 
 
  if (io_code == NULL) io_code = &new_st;          
          
  x = g95_get_code();    
  x->type = EXEC_DT_END;
  x->ext.dt = new_st.ext.dt;     /* Point to structure that is already there */       
       
  g95_append_code(io_code, x); 
}         
         
         
       
       
/* g95_match_open()-- Match an OPEN statmement */          
          
match g95_match_open(void) {       
g95_open *open;        
match x;  
  
  x = g95_match_char('('); 
  if (x == MATCH_NO) return x;        
        
  open = g95_getmem(sizeof(g95_open));         
         
  x = match_open_element(open);      
      
  if (x == MATCH_ERROR) goto cleanup;      
  if (x == MATCH_NO) {     
    x = g95_match_expr(&open->unit);  
    if (x == MATCH_NO) goto syntax;  
    if (x == MATCH_ERROR) goto cleanup;      
  }        
        
  for(;;) {     
    if (g95_match_char(')') == MATCH_YES) break;         
    if (g95_match_char(',') != MATCH_YES) goto syntax;          
          
    x = match_open_element(open);  
    if (x == MATCH_ERROR) goto cleanup; 
    if (x == MATCH_NO) goto syntax;    
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


        
        
/* g95_free_close()-- Free a g95_close structure an all its expressions */        
        
void g95_free_close(g95_close *close) {  
  
  if (close == NULL) return;

  g95_free_expr(close->unit);         
  g95_free_expr(close->iostat);  
  g95_free_expr(close->status);         
         
  g95_free(close);   
}         
         
         


/* check_namelist()-- Traverse a namelist that is part of a READ
 * statement to make sure that none of the variables in the namelist
 * are INTENT(IN).  Returns nonzero if we find such a variable */     
     
static int check_namelist(g95_symbol *symbol) {    
g95_namelist *p;      
      
  for(p=symbol->namelist; p; p=p->next) 
    if (p->sym->attr.intent == INTENT_IN) {    
      g95_error("Symbol '%s' in namelist '%s' is INTENT(IN) at %C",          
		p->sym->name, symbol->name);        
      return 1;          
    }          
          
  return 0;        
}          
          
          
          
          
/* io_kind_name()-- Given an io_kind, return its name */        
        
static char *io_kind_name(io_kind f) {   
char *n;       
       
  switch(f) {   
  case M_READ:     n = "READ";     break;          
  case M_WRITE:    n = "WRITE";    break;         
  case M_PRINT:    n = "PRINT";    break;         
  case M_INQUIRE:  n = "INQUIRE";  break;          
  default:          
    g95_internal_error("io_kind_name(): bad I/O-kind");
  }     
     
  return n;  
}         
         
         
  
  
/* match_close_element()-- Match elements of a CLOSE statment */          
          
static match match_close_element(g95_close *close) {   
match y;          
          
  y = match_etag(&tag_unit, &close->unit);        if (y != MATCH_NO) return y;    
  y = match_etag(&tag_status, &close->status);    if (y != MATCH_NO) return y;          
  y = match_out_tag(&tag_iostat, &close->iostat); if (y != MATCH_NO) return y; 
  y = match_ltag(&tag_err, &close->err);          if (y != MATCH_NO) return y;  
  
  return MATCH_NO;  
}         
         
         
       
       
/* match_io_iterator()-- Match an IO iteration statement of the form:
 *   ( [<IO element> ,] <IO element>, I = <expr>, <expr> [, <expr> ] )
 *
 * Which is equivalent to a single IO element.  This function is mutually
 * recursive with match_io_element().  */      
      
static match match_io_iterator(io_kind x, g95_code **result) {
g95_code *head, *end, *old;  
g95_iterator *it;    
locus old_loc;      
match a;     
int w;        
        
  it = NULL;     
  head = end = NULL;       
  old_loc = *g95_current_locus();    
    
  if (g95_match_char('(') != MATCH_YES) return MATCH_NO;        
        
  a = match_io_element(x, &head);       
  end = head;      
      
  if (a != MATCH_YES || g95_match_char(',') != MATCH_YES) {    
    a = MATCH_NO;   
    goto cleanup;       
  }     
     
/* Can't be anything but an IO iterator.  Build a list */   
   
  it = g95_get_iterator(); 
 
  for(w=1;; w++) {    
    a = g95_match_iterator(it, 0);  
    if (a == MATCH_ERROR) goto cleanup;   
    if (a == MATCH_YES) {      
      g95_check_do_variable(it->var->symbol);       
      break;         
    }  
  
    a = match_io_element(x, &old);
    if (a == MATCH_ERROR) goto cleanup;    
    if (a == MATCH_NO) {       
      if (w > 2) goto syntax;      
      goto cleanup;  
    }     
     
    end = g95_append_code(end, old);   
   
    if (g95_match_char(',') != MATCH_YES) {   
      if (w > 2) goto syntax;  
      a = MATCH_NO; 
      goto cleanup;         
    }  
  }          
          
  if (g95_match_char(')') != MATCH_YES) goto syntax;        
        
  old = g95_get_code();         
  old->type = EXEC_DO;         
  old->ext.iterator = it;
  old->block = head;

  *result = old;       
  return MATCH_YES;         
         
syntax:    
  g95_error("Syntax error in I/O iterator at %C");       
  a = MATCH_ERROR;  
  
cleanup:      
  g95_free_iterator(it, 1);    
  g95_free_statements(head);          
  g95_set_locus(&old_loc);        
  return a;  
}


    
    
/* match_inquire_element()-- Match an element of an INQUIRE statement */     
     
static match match_inquire_element(g95_inquire *inquire) {         
match n; 
 
  n = match_etag(&tag_unit, &inquire->unit);   
  if (n != MATCH_NO) return n;    
    
  n = match_etag(&tag_file, &inquire->file);     
  if (n != MATCH_NO) return n;         
         
  n = match_ltag(&tag_err, &inquire->err);  
  if (n != MATCH_NO) return n;      
      
  n = match_out_tag(&tag_iostat, &inquire->iostat);  
  if (n != MATCH_NO) return n;        
        
  n = match_vtag(&tag_exist, &inquire->exist);       
  if (n != MATCH_NO) return n;     
     
  n = match_vtag(&tag_opened, &inquire->opened);         
  if (n != MATCH_NO) return n;  
  
  n = match_vtag(&tag_named, &inquire->named);          
  if (n != MATCH_NO) return n;        
        
  n = match_vtag(&tag_name, &inquire->name);       
  if (n != MATCH_NO) return n;         
         
  n = match_out_tag(&tag_number, &inquire->number);        
  if (n != MATCH_NO) return n;      
      
  n = match_vtag(&tag_s_access, &inquire->access);        
  if (n != MATCH_NO) return n;          
          
  n = match_vtag(&tag_sequential, &inquire->sequential);     
  if (n != MATCH_NO) return n;         
         
  n = match_vtag(&tag_direct, &inquire->direct);    
  if (n != MATCH_NO) return n;

  n = match_vtag(&tag_s_form, &inquire->form);          
  if (n != MATCH_NO) return n;

  n = match_vtag(&tag_formatted, &inquire->formatted);         
  if (n != MATCH_NO) return n;     
     
  n = match_vtag(&tag_unformatted, &inquire->unformatted);
  if (n != MATCH_NO) return n;     
     
  n = match_out_tag(&tag_s_recl, &inquire->recl);       
  if (n != MATCH_NO) return n; 
 
  n = match_out_tag(&tag_nextrec, &inquire->nextrec);   
  if (n != MATCH_NO) return n;  
  
  n = match_vtag(&tag_s_blank, &inquire->blank);     
  if (n != MATCH_NO) return n;     
     
  n = match_vtag(&tag_s_position, &inquire->position);
  if (n != MATCH_NO) return n;  
  
  n = match_vtag(&tag_s_action, &inquire->action); 
  if (n != MATCH_NO) return n;

  n = match_vtag(&tag_read, &inquire->read);     
  if (n != MATCH_NO) return n; 
 
  n = match_vtag(&tag_write, &inquire->write);        
  if (n != MATCH_NO) return n;       
       
  n = match_vtag(&tag_readwrite, &inquire->readwrite);    
  if (n != MATCH_NO) return n; 
 
  n = match_vtag(&tag_s_delim, &inquire->delim);         
  if (n != MATCH_NO) return n;  
  
  n = match_vtag(&tag_s_pad, &inquire->pad);    
  if (n != MATCH_NO) return n;    
    
  n = match_vtag(&tag_iolength, &inquire->iolength);       
  if (n != MATCH_NO) return n;         
         
  return MATCH_NO;   
}         
         
         
        
        
/* default_unit()-- Return the default unit number.  The runtime
 * library translates this into the real unit number. */        
        
static g95_expr *default_unit(void) {   
   
  return g95_int_expr(-1);
}  
  
  
          
          
/* match_io_element()-- Match a single element of an IO list, which is
 * either a single expression or an IO Iterator */          
          
static match match_io_element(io_kind q, g95_code **cpp) {       
g95_expr *e2;   
g95_code *cp;     
match a;         
         
  e2 = NULL;  
  
  a = match_io_iterator(q, cpp);    
  if (a == MATCH_YES) return MATCH_YES;        
        
  if (q == M_READ) {
    a = g95_match_variable(&e2, 0);     
    if (a == MATCH_NO) g95_error("Expected variable in READ statement at %C");    
  } else {    
    a = g95_match_expr(&e2);         
    if (a == MATCH_NO) g95_error("Expected expression in %s statement at %C",         
				 io_kind_name(q));    
  }          
          
  if (a == MATCH_YES)  
    switch(q) {   
    case M_READ:          
      if (e2->symbol->attr.intent == INTENT_IN) {       
	g95_error("Variable '%s' in input list at %C cannot be INTENT(IN)",          
		  e2->symbol->name); 
	a = MATCH_ERROR;    
      }       
       
      if (g95_pure(NULL) && g95_impure_variable(e2->symbol) &&   
	  current_dt->io_unit->ts.type == BT_CHARACTER) {   
	g95_error("Cannot read to variable '%s' in PURE procedure at %C", 
		  e2->symbol->name);         
	a = MATCH_ERROR;       
      }      
      
      break;     
     
    case M_WRITE:         
      if (current_dt->io_unit->ts.type == BT_CHARACTER &&      
	  g95_pure(NULL) && current_dt->io_unit->type == EXPR_VARIABLE &&     
	  g95_impure_variable(current_dt->io_unit->symbol)) {       
	g95_error("Cannot write to internal file unit '%s' at %C inside a "    
		  "PURE procedure", current_dt->io_unit->symbol->name);     
	a = MATCH_ERROR; 
      }       
       
      break;         
         
    default:    
      break;    
    }      
      
  if (a != MATCH_YES) {          
    g95_free_expr(e2);
    return MATCH_ERROR;      
  }    
    
  cp = g95_get_code();        
  cp->type = EXEC_TRANSFER;  
  cp->expr = e2;         
         
  *cpp = cp;         
  return MATCH_YES;          
}    
    
    
       
       
/* match_dt_format()-- Match a format specification */     
     
static match match_dt_format(g95_dt *datat) {          
locus w;
g95_expr *x;     
g95_st_label *l;          
          
  w = *g95_current_locus();    
    
  if (g95_match_char('*') == MATCH_YES) {
    if (datat->format_expr != NULL || datat->format_label != NULL) goto conflict;          
          
    datat->format_label = &g95_format_asterisk;          
    return MATCH_YES;          
  }    
    
  if (g95_match_st_label(&l, 0) == MATCH_YES) { 
    if (datat->format_expr != NULL || datat->format_label != NULL) {          
      g95_free_st_label(l); 
      goto conflict;       
    }         
         
    if (g95_reference_st_label(l, ST_LABEL_FORMAT) == FAILURE)         
      return MATCH_ERROR;     
     
    datat->format_label = l;    
    return MATCH_YES;      
  }       
       
  if (g95_match_expr(&x) == MATCH_YES) {        
    if (datat->format_expr != NULL || datat->format_label != NULL) {        
      g95_free_expr(x);      
      goto conflict;
    }        
        
    datat->format_expr = x;      
    return MATCH_YES;          
  }

  g95_set_locus(&w);    /* The only case where we have to restore */      
      
  return MATCH_NO;   
   
conflict:      
  g95_error("Duplicate format specification at %C");  
  return MATCH_ERROR;        
}     
     
     
     
     
/* match_io_list()-- Match an I/O list, building g95_code structures
 * as we go. */         
         
static match match_io_list(io_kind x, g95_code **head_p) {    
g95_code *h, *tail, *n;
match d;  
  
  *head_p = h = tail = NULL;    
  if (g95_match_eos() == MATCH_YES) return MATCH_YES;          
          
  for(;;) {      
    d = match_io_element(x, &n);   
    if (d == MATCH_ERROR) goto cleanup;
    if (d == MATCH_NO) goto syntax;          
          
    tail = g95_append_code(tail, n);  
    if (h == NULL) h = n;       
       
    if (g95_match_eos() == MATCH_YES) break;          
    if (g95_match_char(',') != MATCH_YES) goto syntax;       
  }       
       
  *head_p = h;        
  return MATCH_YES;       
       
syntax:      
  g95_error("Syntax error in %s statement at %C", io_kind_name(x));     
     
cleanup:        
  g95_free_statements(h);
  return MATCH_ERROR; 
}         
         
         
  
  
/* g95_match_close()-- Match a CLOSE statement */        
        
match g95_match_close(void) { 
g95_close *close;        
match u;      
      
  u = g95_match_char('(');      
  if (u == MATCH_NO) return u;          
          
  close = g95_getmem(sizeof(g95_close));      
      
  u = match_close_element(close);          
          
  if (u == MATCH_ERROR) goto cleanup;      
  if (u == MATCH_NO) {     
    u = g95_match_expr(&close->unit);  
    if (u == MATCH_NO) goto syntax;        
    if (u == MATCH_ERROR) goto cleanup;          
  }      
      
  for(;;) {  
    if (g95_match_char(')') == MATCH_YES) break;    
    if (g95_match_char(',') != MATCH_YES) goto syntax; 
 
    u = match_close_element(close);      
    if (u == MATCH_ERROR) goto cleanup;       
    if (u == MATCH_NO) goto syntax;     
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
        
        
 
 
/* match_file_element()-- Match elements of a REWIND, BACKSPACE or
 * ENDFILE statement */  
  
static match match_file_element(g95_filepos *fp2) {         
match x;

  x = match_etag(&tag_unit, &fp2->unit);          if (x != MATCH_NO) return x;        
  x = match_out_tag(&tag_iostat, &fp2->iostat);   if (x != MATCH_NO) return x;  
  x = match_ltag(&tag_err, &fp2->err);            if (x != MATCH_NO) return x;   
   
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
     
     
 
 
/* match_filepos()-- Match the second half of the file-positioning
 * statements, REWIND, BACKSPACE or ENDFILE. */    
    
static match match_filepos(g95_statement sta, g95_exec_op operand) {    
g95_filepos *fp;        
match z;     
     
  fp = g95_getmem(sizeof(g95_filepos)); 
 
  if (g95_match_char('(') == MATCH_NO) {          
    z = g95_match_expr(&fp->unit);      
    if (z == MATCH_ERROR) goto cleanup;      
    if (z == MATCH_NO) goto syntax;

    goto done;    
  }          
          
  z = match_file_element(fp);   
  if (z == MATCH_ERROR) goto done; 
  if (z == MATCH_NO) {          
    z = g95_match_expr(&fp->unit);          
    if (z == MATCH_ERROR) goto done;          
    if (z == MATCH_NO) goto syntax;         
  }

  for(;;) {    
    if (g95_match_char(')') == MATCH_YES) break;         
    if (g95_match_char(',') != MATCH_YES) goto syntax; 
 
    z = match_file_element(fp);          
    if (z == MATCH_ERROR) goto cleanup;        
    if (z == MATCH_NO) goto syntax;  
  } 
 
done:     
  if (g95_match_eos() != MATCH_YES) goto syntax;      
      
  if (g95_pure(NULL)) {          
    g95_error("%s statement not allowed in PURE procedure at %C",          
	      g95_ascii_statement(sta));          
          
    return MATCH_ERROR;    
  }          
          
  new_st.type = operand;     
  new_st.ext.filepos = fp;   
  return MATCH_YES;     
     
syntax:    
  g95_syntax_error(sta); 
 
cleanup:          
  g95_free_filepos(fp);       
  return MATCH_ERROR;    
}       
       
       
         
         
/* match_dt_unit()-- Match a unit specification for a data transfer
 * statement */  
  
static match match_dt_unit(g95_dt *datat) {          
g95_expr *n;       
       
  if (g95_match_char('*') == MATCH_YES) {        
    if (datat->io_unit != NULL) goto conflict;   
   
    datat->io_unit = default_unit();    
    return MATCH_YES;         
  }  
  
  if (g95_match_expr(&n) == MATCH_YES) {         
    if (datat->io_unit != NULL) {     
      g95_free_expr(n);      
      goto conflict;      
    } 
 
    datat->io_unit = n;      
    return MATCH_YES;         
  }     
     
  return MATCH_NO;

conflict:   
  g95_error("Duplicate UNIT specification at %C");        
  return MATCH_ERROR; 
}    
    
    


match g95_match_endfile(void) {      
      
  return match_filepos(ST_END_FILE, EXEC_ENDFILE);         
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
   
   
    
    
match g95_match_rewind(void) {      
      
  return match_filepos(ST_REWIND, EXEC_REWIND);         
}        
        
        
    
    
match g95_match_backspace(void) {        
        
  return match_filepos(ST_BACKSPACE, EXEC_BACKSPACE);     
}  
  
  
       
       
/* match_dt_element()-- Match a single data transfer element */

static match match_dt_element(io_kind r, g95_dt *d) {   
char name[G95_MAX_SYMBOL_LEN+1];       
g95_symbol *sy;    
match w;

  if (g95_match(" unit =") == MATCH_YES) {  
    w = match_dt_unit(d);    
    if (w != MATCH_NO) return w;         
  } 
 
  if (g95_match(" fmt =") == MATCH_YES) {   
    w = match_dt_format(d);          
    if (w != MATCH_NO) return w;         
  }        
        
  if (g95_match(" nml = %n", name) == MATCH_YES) {        
    if (d->namelist != NULL) {  
      g95_error("Duplicate NML specification at %C");   
      return MATCH_ERROR;    
    } 
 
    if (g95_find_symbol(name, NULL, 1, &sy)) return MATCH_ERROR;       
       
    if (sy == NULL || sy->attr.flavor != FL_NAMELIST) {
      g95_error("Symbol '%s' at %C must be a NAMELIST group name",   
		sy != NULL ? sy->name : name );      
      return MATCH_ERROR;      
    }         
         
    d->namelist = sy; 
    if (r == M_READ && check_namelist(sy)) return MATCH_ERROR;         
         
    return MATCH_YES;
  }        
        
  w = match_etag(&tag_rec, &d->rec);           if (w != MATCH_NO) return w;          
  w = match_out_tag(&tag_iostat, &d->iostat);  if (w != MATCH_NO) return w;         
  w = match_ltag(&tag_err, &d->err);           if (w != MATCH_NO) return w;       
  w = match_etag(&tag_advance, &d->advance);   if (w != MATCH_NO) return w;   
  w = match_out_tag(&tag_size, &d->size);      if (w != MATCH_NO) return w;

  w = match_ltag(&tag_end, &d->end);   
  if (w == MATCH_YES) d->end_where = *g95_current_locus(); 
  if (w != MATCH_NO) return w;    
    
  w = match_ltag(&tag_eor, &d->eor);  
  if (w == MATCH_YES) d->eor_where = *g95_current_locus();      
  if (w != MATCH_NO) return w; 
 
  return MATCH_NO;
}        
        
        
         
         
/* match_io()-- Match a READ, WRITE or PRINT statement. */        
        
static match match_io(io_kind x) {         
char nm[G95_MAX_SYMBOL_LEN+1];         
g95_code *io_code;     
g95_symbol *sym;     
g95_expr *e2;      
int comma_flag;       
locus w;        
g95_dt *dt;      
match b;          
          
  b = MATCH_NO;    
    
  comma_flag = 0;        
  current_dt = dt = g95_getmem(sizeof(g95_dt));  
  
  if (g95_match_char('(') == MATCH_NO) {
    if (x == M_WRITE) goto syntax;  
  
    b = match_dt_format(dt);
    if (b == MATCH_ERROR) goto cleanup;       
    if (b == MATCH_NO) goto syntax;       
       
    comma_flag = 1;      
    dt->io_unit = default_unit();       
    goto get_io_list;        
  }       
       
/* Match a control list */     
     
  if (match_dt_element(x, dt) == MATCH_YES) goto next;          
  if (match_dt_unit(dt) != MATCH_YES) goto loop;      
      
  if (g95_match_char(')') == MATCH_YES) goto get_io_list;       
  if (g95_match_char(',') != MATCH_YES) goto syntax;       
       
  b = match_dt_element(x, dt);
  if (b == MATCH_YES) goto next;  
  if (b == MATCH_ERROR) goto cleanup;      
      
  b = match_dt_format(dt);     
  if (b == MATCH_YES) goto next;  
  if (b == MATCH_ERROR) goto cleanup;  
  
  w = *g95_current_locus();  
  
  if (g95_match_name(nm) == MATCH_YES &&     
      !g95_find_symbol(nm, NULL, 1, &sym) &&         
      sym->attr.flavor == FL_NAMELIST) {
    dt->namelist = sym;         
    if (x == M_READ && check_namelist(sym)) {   
      b = MATCH_ERROR;
      goto cleanup;     
    } 
 
    goto next;    
  }   
   
  g95_set_locus(&w);      
      
  goto loop;   /* No matches, try regular elements */ 
 
next:   
  if (g95_match_char(')') == MATCH_YES) goto get_io_list;  
  if (g95_match_char(',') != MATCH_YES) goto syntax;    
    
loop:  
  for(;;) {      
    b = match_dt_element(x, dt);       
    if (b == MATCH_NO) goto syntax;        
    if (b == MATCH_ERROR) goto cleanup;     
     
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
      b = MATCH_ERROR;        
      goto cleanup;        
    }     
     
    b = match_io_list(x, &io_code);       
    if (b == MATCH_ERROR) goto cleanup;      
    if (b == MATCH_NO) goto syntax;        
  } 
 
/* A full IO statement has been matched */   
   
  if (dt->io_unit->type == EXPR_VARIABLE && x == M_WRITE &&
      dt->io_unit->ts.type == BT_CHARACTER &&      
      dt->io_unit->symbol->attr.intent == INTENT_IN) {       
    g95_error("Internal file '%s' at %L is INTENT(IN)",          
	      dt->io_unit->symbol->name, &dt->io_unit->where);     
    b = MATCH_ERROR;         
    goto cleanup;   
  }       
       
  e2 = dt->format_expr;          
          
  if (e2 != NULL && e2->type == EXPR_CONSTANT)
    g95_check_format_string(e2);  
  
  if (g95_pure(NULL) && (x == M_READ || x == M_WRITE) &&          
      dt->io_unit->ts.type != BT_CHARACTER) {     
    g95_error("io-unit in %s statement at %C must be an internal file in a "       
	      "PURE procedure", io_kind_name(x));       
    b = MATCH_ERROR; 
    goto cleanup;         
  }

  new_st.type = (x == M_READ) ? EXEC_READ : EXEC_WRITE;    
  new_st.ext.dt = dt;  
  new_st.next = io_code;    
    
  terminate_io(io_code);         
         
  return MATCH_YES;      
      
syntax:         
  g95_error("Syntax error in %s statement at %C", io_kind_name(x));     
  b = MATCH_ERROR;     
     
cleanup:        
  g95_free_dt(dt);  
  return b;  
}          
          
          
       
       
match g95_match_write(void) { return match_io(M_WRITE); }

          
          
match g95_match_print(void) {  
match r;    
    
  r = match_io(M_PRINT);      
  if (r != MATCH_YES) return r;  
  
  if (g95_pure(NULL)) {     
    g95_error("PRINT statement at %C not allowed within PURE procedure"); 
    return MATCH_ERROR; 
  }   
   
  return MATCH_YES;
}     
     
     
    
    
match g95_match_read(void)  { return match_io(M_READ);  } 
 
   
   
match g95_match_inquire(void) {     
g95_inquire *inquire;         
g95_code *codep;       
match l;    
    
  l = g95_match_char('(');     
  if (l == MATCH_NO) return l;        
        
  inquire = g95_getmem(sizeof(g95_inquire));         
         
  l = match_inquire_element(inquire);      
  if (l == MATCH_ERROR) goto cleanup;        
  if (l == MATCH_NO) {      
    l = g95_match_expr(&inquire->unit);    
    if (l == MATCH_ERROR) goto cleanup;      
    if (l == MATCH_NO) goto syntax;     
  }   
   
/* See if we have the IOLENGTH form of the inquire statement */ 
 
  if (inquire->iolength != NULL) {    
    if (g95_match_char(')') != MATCH_YES) goto syntax;   
   
    l = match_io_list(M_INQUIRE, &codep);
    if (l == MATCH_ERROR) goto cleanup;     
    if (l == MATCH_NO) goto syntax;         
         
    terminate_io(codep);          
          
    new_st.type = EXEC_IOLENGTH; 
    new_st.expr = inquire->iolength;       
    g95_free(inquire);   
   
    if (g95_pure(NULL)) {  
      g95_free_statements(codep);       
      g95_error("INQUIRE statement not allowed in PURE procedure at %C");  
      return MATCH_ERROR;
    }      
      
    new_st.next = codep;    
    return MATCH_YES;  
  }       
       
/* At this point, we have the non-IOLENGTH inquire statement */      
      
  for(;;) {     
    if (g95_match_char(')') == MATCH_YES) break;          
    if (g95_match_char(',') != MATCH_YES) goto syntax;       
       
    l = match_inquire_element(inquire);          
    if (l == MATCH_ERROR) goto cleanup;   
    if (l == MATCH_NO) goto syntax;         
         
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
          
          
