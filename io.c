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
     
#include <string.h>
#include "g95.h"
         
g95_st_label g95_format_asterisk;  
  
  
typedef struct {   
  char *name, *spec;   
  bt type; 
  char **values;       
} io_tag;


char *open_opt[]   = { "unknown", "old", "new", "replace", "scratch", NULL };   
char *access_opt[] = { "sequential",  "direct", NULL }; 
char *form_opt[]   = { "formatted",  "unformatted", NULL };   
char *blank_opt[]  = { "null", "zero", NULL };
char *pos_opt[]    = { "asis",  "rewind", "append", NULL };          
char *action_opt[] = { "read",  "write", "readwrite", NULL };
char *delim_opt[]  = { "none", "apostrophe", "quote", NULL };      
char *yesno_opt[]  = { "yes", "no" , NULL };          
char *close_opt[]  = { "keep", "delete", NULL };    
    
    
static io_tag        
  tag_file =        { "FILE",     " file = %e",     BT_CHARACTER, NULL }, 
  tag_ostatus =     { "STATUS",   " status = %e",   BT_CHARACTER, open_opt },       
  tag_cstatus =     { "STATUS",   " status = %e",   BT_CHARACTER, close_opt },      
  tag_e_access =    { "ACCESS",   " access = %e",   BT_CHARACTER, access_opt },       
  tag_e_form =      { "FORM",     " form = %e",     BT_CHARACTER, form_opt },       
  tag_e_recl =      { "RECL",     " recl = %e",     BT_INTEGER,   NULL },     
  tag_e_blank =     { "BLANK",    " blank = %e",    BT_CHARACTER, blank_opt },       
  tag_e_position =  { "POSITION", " position = %e", BT_CHARACTER, pos_opt },      
  tag_e_action =    { "ACTION",   " action = %e",   BT_CHARACTER, action_opt },   
  tag_e_delim =     { "DELIM",    " delim = %e",    BT_CHARACTER, delim_opt },    
  tag_e_pad =       { "PAD",      " pad = %e",      BT_CHARACTER, yesno_opt },      
      
  tag_unit =        { "UNIT",     " unit = %e",     BT_INTEGER,   NULL },          
  tag_advance =     { "ADVANCE",  " advance = %e",  BT_CHARACTER, yesno_opt },      
      
  tag_rec =         { "REC",      " rec = %e",      BT_INTEGER,   NULL },
  tag_format =      { "FORMAT",    NULL,            BT_CHARACTER, NULL }, 
 
  tag_iostat =      { "IOSTAT",      " iostat = %v",      BT_INTEGER,   NULL },    
  tag_size =        { "SIZE",        " size = %v",        BT_INTEGER,   NULL },     
  tag_exist =       { "EXIST",       " exist = %v",       BT_LOGICAL,   NULL }, 
  tag_opened =      { "OPENED",      " opened = %v",      BT_LOGICAL,   NULL },        
  tag_named =       { "NAMED",       " named = %v",       BT_LOGICAL,   NULL },       
  tag_name =        { "NAME",        " name = %v",        BT_CHARACTER, NULL },        
  tag_number =      { "NUMBER",      " number = %v",      BT_INTEGER,   NULL },        
  tag_s_access =    { "ACCESS",      " access = %v",      BT_CHARACTER, NULL },          
  tag_sequential =  { "SEQUENTIAL",  " sequential = %v",  BT_CHARACTER, NULL }, 
  tag_direct =      { "DIRECT",      " direct = %v",      BT_CHARACTER, NULL },     
  tag_s_form =      { "FORM",        " form = %v",        BT_CHARACTER, NULL },      
  tag_formatted =   { "FORMATTED",   " formatted = %v",   BT_CHARACTER, NULL },  
  tag_unformatted = { "UNFORMATTED", " unformatted = %v", BT_CHARACTER, NULL },  
  tag_s_recl =      { "RECL",        " recl = %v",        BT_INTEGER,   NULL },    
  tag_nextrec =     { "NEXTREC",     " nextrec = %v",     BT_INTEGER,   NULL },         
  tag_s_blank =     { "BLANK",       " blank = %v",       BT_CHARACTER, NULL },   
  tag_s_position =  { "POSITION",    " position = %v",    BT_CHARACTER, NULL },         
  tag_s_action =    { "ACTION",      " action = %v",      BT_CHARACTER, NULL }, 
  tag_read =        { "READ",        " read = %v",        BT_CHARACTER, NULL },          
  tag_write =       { "WRITE",       " write = %v",       BT_CHARACTER, NULL },     
  tag_readwrite =   { "READWRITE",   " readwrite = %v",   BT_CHARACTER, NULL },     
  tag_s_delim =     { "DELIM",       " delim = %v",       BT_CHARACTER, NULL },  
  tag_s_pad =       { "PAD",         " pad = %v",         BT_CHARACTER, NULL },      
  tag_iolength =    { "IOLENGTH",    " iolength = %v",    BT_INTEGER,   NULL },  
  
  tag_err = { "ERR", " err = %l", BT_UNKNOWN, NULL },          
  tag_end = { "END", " end = %l", BT_UNKNOWN, NULL },      
  tag_eor = { "EOR", " eor = %l", BT_UNKNOWN, NULL }; 
 
static g95_dt *current_dt; 
 
#define RESOLVE_TAG(x, y, flag) \
    if (resolve_tag(x, y, flag) == FAILURE) return FAILURE;

typedef enum { M_READ, M_WRITE, M_PRINT, M_INQUIRE } io_kind;  
static match match_io_element(io_kind k, g95_code **);  
static try resolve_tag(io_tag *, g95_expr *, int);          
          
          
       
       
/* match_io_iterator()-- Match an IO iteration statement of the form:
 *   ( [<IO element> ,] <IO element>, I = <expr>, <expr> [, <expr> ] )
 *
 * Which is equivalent to a single IO element.  This function is mutually
 * recursive with match_io_element().  */         
         
static match match_io_iterator(io_kind b, g95_code **rslt) {          
g95_code *h, *end, *new;     
g95_iterator *it;     
g95_locus old;          
match c;   
int q;     
     
  it = NULL;         
  h = end = NULL;       
  old = g95_current_locus;          
          
  if (g95_match_char('(') != MATCH_YES) return MATCH_NO;       
       
  c = match_io_element(b, &h);      
  end = h;     
     
  if (c != MATCH_YES || g95_match_char(',') != MATCH_YES) {       
    c = MATCH_NO;         
    goto cleanup;       
  }      
      
/* Can't be anything but an IO iterator.  Build a list */          
          
  it = g95_get_iterator();       
       
  for(q=1;; q++) {         
    c = g95_match_iterator(it, 0);     
    if (c == MATCH_ERROR) goto cleanup;       
    if (c == MATCH_YES) {      
      g95_check_do_variable(it->var->symbol);
      break;        
    }          
          
    c = match_io_element(b, &new); 
    if (c == MATCH_ERROR) goto cleanup;      
    if (c == MATCH_NO) {          
      if (q > 2) goto syntax;          
      goto cleanup;    
    }  
  
    end = g95_append_code(end, new);       
       
    if (g95_match_char(',') != MATCH_YES) {     
      if (q > 2) goto syntax;  
      c = MATCH_NO;      
      goto cleanup; 
    }     
  }        
        
  if (g95_match_char(')') != MATCH_YES) goto syntax;   
   
  new = g95_get_code();   
  new->type = EXEC_DO;
  new->ext.iterator = it;   
  new->block = h;       
       
  *rslt = new;
  return MATCH_YES;       
       
syntax:    
  g95_error("Syntax error in I/O iterator at %C");    
  c = MATCH_ERROR;   
   
cleanup:    
  g95_free_iterator(it, 1);    
  g95_free_statements(h);     
  g95_current_locus = old;       
  return c; 
}  
  
  
     
     
/* check_namelist()-- Traverse a namelist that is part of a READ
 * statement to make sure that none of the variables in the namelist
 * are INTENT(IN).  Returns nonzero if we find such a variable */   
   
static int check_namelist(g95_symbol *sym) { 
g95_namelist *a;      
      
  for(a=sym->namelist; a; a=a->next)
    if (a->sym->attr.intent == INTENT_IN) {      
      g95_error("Symbol '%s' in namelist '%s' is INTENT(IN) at %C",      
		a->sym->name, sym->name);        
      return 1;    
    }   
   
  return 0;     
}     
     
     
     
     
/* g95_resolve_open()-- resolve everything in a g95_open structure */          
          
try g95_resolve_open(g95_open *open) {        
        
  RESOLVE_TAG(&tag_unit,    open->unit,    0);        
  RESOLVE_TAG(&tag_iostat,  open->iostat,  1);    
  RESOLVE_TAG(&tag_file,    open->file,    0);  
  RESOLVE_TAG(&tag_ostatus, open->status,  0);       
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
     
     


/* g95_free_close()-- Free a g95_close structure an all its expressions */ 
 
void g95_free_close(g95_close *close) {

  if (close == NULL) return;         
         
  g95_free_expr(close->unit);      
  g95_free_expr(close->iostat);      
  g95_free_expr(close->status);    
    
  g95_free(close);    
}     
     
     
    
    
/* match_ltag()-- Match a label I/O tag */      
      
static match match_ltag(io_tag *tag, g95_st_label **lab) {        
match e;    
g95_st_label *n;  
  
  n = *lab;      
  e = g95_match(tag->spec, lab);          
  if (e == MATCH_YES && n != 0) {        
    g95_error("Duplicate %s label specification at %C", tag->name);        
    return MATCH_ERROR;  
  }         
         
  return e;     
}    
    
    
 
 
/* match_etag()-- Match an expression I/O tag of some sort. */          
          
static match match_etag(io_tag *tag, g95_expr **d) {      
g95_expr *res;     
match j;      
      
  j = g95_match(tag->spec, &res);         
  if (j != MATCH_YES) return j; 
 
  if (*d != NULL) {       
    g95_error("Duplicate %s specification at %C", tag->name);  
    g95_free_expr(res);    
    return MATCH_ERROR;       
  }       
       
  *d = res;       
  return MATCH_YES;         
}   
   
   
    
    
/* match_vtag()-- Match a variable I/O tag of some sort. */ 
 
static match match_vtag(io_tag *tag, g95_expr **g) {  
g95_expr *r;  
match m;     
     
  m = g95_match(tag->spec, &r);        
  if (m != MATCH_YES) return m;       
       
  if (*g != NULL) {         
    g95_error("Duplicate %s specification at %C", tag->name);    
    g95_free_expr(r);   
    return MATCH_ERROR; 
  } 
 
  if (r->symbol->attr.intent == INTENT_IN) {  
    g95_error("Variable tag cannot be INTENT(IN) at %C");    
    g95_free_expr(r);   
    return MATCH_ERROR;        
  }     
     
  if (g95_pure(NULL) && g95_impure_variable(r->symbol)) {         
    g95_error("Variable tag cannot be assigned in PURE procedure at %C");         
    g95_free_expr(r);
    return MATCH_ERROR;        
  }         
         
  *g = r;    
  return MATCH_YES;  
}        
        
        
      
      
/* match_dt_format()-- Match a format specification */          
          
static match match_dt_format(g95_dt *dt) {          
g95_st_label *label; 
g95_locus where;          
g95_expr *n;  
  
  where = g95_current_locus;  
  
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
    
  if (g95_match_expr(&n) == MATCH_YES) {       
    if (dt->format_expr != NULL || dt->format_label != NULL) {          
      g95_free_expr(n);          
      goto conflict;          
    }

    dt->format_expr = n;  
    return MATCH_YES;      
  }  
  
  g95_current_locus = where;    /* The only case where we have to restore */          
          
  return MATCH_NO;        
        
conflict:   
  g95_error("Duplicate format specification at %C"); 
  return MATCH_ERROR;     
}   
   
   
       
       
/* g95_resolve_close()-- Resolve everything in a g95_close structure */   
   
try g95_resolve_close(g95_close *close) {      
      
  RESOLVE_TAG(&tag_unit,    close->unit,    0);
  RESOLVE_TAG(&tag_iostat,  close->iostat,  1);         
  RESOLVE_TAG(&tag_cstatus, close->status,  0);  
  
  if (g95_reference_st_label(close->err, ST_LABEL_TARGET) == FAILURE)     
    return FAILURE;      
      
  return SUCCESS;  
}      
      
      
    
    
/* g95_free_dt()-- Free a data transfer structure and everything below it */          
          
void g95_free_dt(g95_dt *datat) {    
    
  if (datat == NULL) return;        
        
  g95_free_expr(datat->io_unit);      
  g95_free_expr(datat->format_expr);   
  g95_free_expr(datat->rec);
  g95_free_expr(datat->advance);
  g95_free_expr(datat->iostat);  
  g95_free_expr(datat->size);         
         
  g95_free(datat);   
}  
  
  
      
      
/* default_unit()-- Return the default unit number.  The runtime
 * library translates this into the real unit number. */ 
 
static g95_expr *default_unit(void) {

  return g95_int_expr(-1);     
}   
   
   
    
    
/* g95_free_filepos()-- Free a g95_filepos structure */        
        
void g95_free_filepos(g95_filepos *fp) {          
          
  g95_free_expr(fp->unit);         
  g95_free_expr(fp->iostat);  
  g95_free(fp);   
}      
      
      
  
  
/* match_out_tag()-- Match I/O tags that cause variables to become
 * redefined. */        
        
static match match_out_tag(io_tag *tag, g95_expr **rslt) {
match m;     
     
  m = match_vtag(tag, rslt); 
  if (m == MATCH_YES) g95_check_do_variable((*rslt)->symbol);        
        
  return m;          
}          
          
          
      
      
/* resolve_tag()-- Do expression resolution and type-checking on an
 * expression tag */    
    
static try resolve_tag(io_tag *tag, g95_expr *k, int output) {
char **x; 
int j;   
   
  if (k == NULL) return SUCCESS;     
     
  if (g95_resolve_expr(k) == FAILURE) return FAILURE;          
          
  if (output && k->type == EXPR_VARIABLE) k->symbol->attr.set = 1;    
    
  if (k->ts.type != tag->type) {          
    g95_error("%s tag at %L must be of type %s", tag->name, &k->where,          
	      g95_basic_typename(tag->type));  
    return FAILURE;         
  }    
    
  if (tag == &tag_format) {   
    if (k->rank != 1 && k->rank != 0) {  
      g95_error("FORMAT tag at %L cannot be array of strings", &k->where);       
      return FAILURE;     
    }  
  } else {        
    if (k->rank != 0) {        
      g95_error("%s tag at %L must be scalar", tag->name, &k->where);   
      return FAILURE;    
    }    
  }      
      
  if (tag->values == NULL || k->type != EXPR_CONSTANT) return SUCCESS;   
   
  for(x=tag->values; *x; x++) {         
    j = strlen(*x);
    if (j == k->value.character.length &&  
	strncasecmp(*x, k->value.character.string, j) == 0)    
      return SUCCESS; 
  }       
       
  g95_error("Invalid value for tag '%s' at %L", tag->name, &k->where); 
  return FAILURE;    
}     
     
     
        
        
/* match_open_element()-- Match a single tag of an OPEN statement */

static match match_open_element(g95_open *open) {         
match v;    
    
  v = match_etag(&tag_unit, &open->unit);          if (v != MATCH_NO) return v;          
  v = match_out_tag(&tag_iostat, &open->iostat);   if (v != MATCH_NO) return v;   
  v = match_etag(&tag_file, &open->file);          if (v != MATCH_NO) return v;        
  v = match_etag(&tag_ostatus, &open->status);     if (v != MATCH_NO) return v; 
  v = match_etag(&tag_e_access, &open->access);    if (v != MATCH_NO) return v;  
  v = match_etag(&tag_e_form, &open->form);        if (v != MATCH_NO) return v;    
  v = match_etag(&tag_e_recl, &open->recl);        if (v != MATCH_NO) return v;     
  v = match_etag(&tag_e_blank, &open->blank);      if (v != MATCH_NO) return v;   
  v = match_etag(&tag_e_position, &open->position);if (v != MATCH_NO) return v;       
  v = match_etag(&tag_e_action, &open->action);    if (v != MATCH_NO) return v;         
  v = match_etag(&tag_e_delim, &open->delim);      if (v != MATCH_NO) return v;         
  v = match_etag(&tag_e_pad, &open->pad);          if (v != MATCH_NO) return v;
  v = match_ltag(&tag_err, &open->err);            if (v != MATCH_NO) return v;          
          
  return MATCH_NO; 
}      
      
      
         
         
/* match_file_element()-- Match elements of a REWIND, BACKSPACE or
 * ENDFILE statement */    
    
static match match_file_element(g95_filepos *f) {        
match h;   
   
  h = match_etag(&tag_unit, &f->unit);          if (h != MATCH_NO) return h;         
  h = match_out_tag(&tag_iostat, &f->iostat);   if (h != MATCH_NO) return h;         
  h = match_ltag(&tag_err, &f->err);            if (h != MATCH_NO) return h;        
        
  return MATCH_NO;       
}     
     
     
       
       
try g95_resolve_filepos(g95_filepos *fp2) {    
    
  RESOLVE_TAG(&tag_unit,   fp2->unit,   0);          
  RESOLVE_TAG(&tag_iostat, fp2->iostat, 1);        
  if (g95_reference_st_label(fp2->err, ST_LABEL_TARGET) == FAILURE) 
    return FAILURE;      
      
  return SUCCESS;          
}         
         
         
          
          
/* io_kind_name()-- Given an io_kind, return its name */      
      
static char *io_kind_name(io_kind i) {          
char *nam;        
        
  switch(i) {  
  case M_READ:     nam = "READ";     break;         
  case M_WRITE:    nam = "WRITE";    break;         
  case M_PRINT:    nam = "PRINT";    break;          
  case M_INQUIRE:  nam = "INQUIRE";  break;        
  default:   
    g95_internal_error("io_kind_name(): bad I/O-kind");    
  }

  return nam;         
}       
       
       
 
 
/* match_inquire_element()-- Match an element of an INQUIRE statement */

static match match_inquire_element(g95_inquire *inquire) {
match j;         
         
  j = match_etag(&tag_unit, &inquire->unit);      
  if (j != MATCH_NO) return j;          
          
  j = match_etag(&tag_file, &inquire->file);          
  if (j != MATCH_NO) return j;          
          
  j = match_ltag(&tag_err, &inquire->err);          
  if (j != MATCH_NO) return j;        
        
  j = match_out_tag(&tag_iostat, &inquire->iostat);       
  if (j != MATCH_NO) return j;         
         
  j = match_vtag(&tag_exist, &inquire->exist);
  if (j != MATCH_NO) return j;

  j = match_vtag(&tag_opened, &inquire->opened);  
  if (j != MATCH_NO) return j;    
    
  j = match_vtag(&tag_named, &inquire->named);          
  if (j != MATCH_NO) return j;          
          
  j = match_vtag(&tag_name, &inquire->name); 
  if (j != MATCH_NO) return j;    
    
  j = match_out_tag(&tag_number, &inquire->number);        
  if (j != MATCH_NO) return j; 
 
  j = match_vtag(&tag_s_access, &inquire->access);
  if (j != MATCH_NO) return j;         
         
  j = match_vtag(&tag_sequential, &inquire->sequential);        
  if (j != MATCH_NO) return j;       
       
  j = match_vtag(&tag_direct, &inquire->direct); 
  if (j != MATCH_NO) return j;      
      
  j = match_vtag(&tag_s_form, &inquire->form);     
  if (j != MATCH_NO) return j;         
         
  j = match_vtag(&tag_formatted, &inquire->formatted);       
  if (j != MATCH_NO) return j;  
  
  j = match_vtag(&tag_unformatted, &inquire->unformatted);       
  if (j != MATCH_NO) return j;        
        
  j = match_out_tag(&tag_s_recl, &inquire->recl);    
  if (j != MATCH_NO) return j; 
 
  j = match_out_tag(&tag_nextrec, &inquire->nextrec);  
  if (j != MATCH_NO) return j;        
        
  j = match_vtag(&tag_s_blank, &inquire->blank);     
  if (j != MATCH_NO) return j;       
       
  j = match_vtag(&tag_s_position, &inquire->position);          
  if (j != MATCH_NO) return j;   
   
  j = match_vtag(&tag_s_action, &inquire->action); 
  if (j != MATCH_NO) return j;   
   
  j = match_vtag(&tag_read, &inquire->read);       
  if (j != MATCH_NO) return j;

  j = match_vtag(&tag_write, &inquire->write);
  if (j != MATCH_NO) return j;   
   
  j = match_vtag(&tag_readwrite, &inquire->readwrite);
  if (j != MATCH_NO) return j;    
    
  j = match_vtag(&tag_s_delim, &inquire->delim);   
  if (j != MATCH_NO) return j;

  j = match_vtag(&tag_s_pad, &inquire->pad);     
  if (j != MATCH_NO) return j;         
         
  j = match_vtag(&tag_iolength, &inquire->iolength);
  if (j != MATCH_NO) return j;

  return MATCH_NO;   
}         
         
         
          
          
/* g95_resolve_dt()-- Resolve everything in a g95_dt structure */       
       
try g95_resolve_dt(g95_dt *d) {  
g95_expr *o;  
  
  RESOLVE_TAG(&tag_format,   d->format_expr,  0);   
  RESOLVE_TAG(&tag_rec,      d->rec,          0);  
  RESOLVE_TAG(&tag_advance,  d->advance,      0);    
  RESOLVE_TAG(&tag_iostat,   d->iostat,       1);
  RESOLVE_TAG(&tag_size,     d->size,         1);        
        
  o = d->io_unit; 
  if (g95_resolve_expr(o) == SUCCESS &&   
      (o->ts.type != BT_INTEGER &&     
       (o->ts.type != BT_CHARACTER || o->type != EXPR_VARIABLE))) {    
    g95_error("UNIT specification at %L must be an INTEGER expression or a "   
	      "CHARACTER variable", &o->where);     
    return FAILURE;    
  }      
      
/* Sanity checks on data transfer statements */          
          
  if (o->ts.type == BT_CHARACTER) {          
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
    
    
    
    
/* match_filepos()-- Match the second half of the file-positioning
 * statements, REWIND, BACKSPACE or ENDFILE. */    
    
static match match_filepos(g95_statement st0, g95_exec_op operand) { 
g95_filepos *fp1;          
match y;     
     
  fp1 = g95_getmem(sizeof(g95_filepos));   
   
  if (g95_match_char('(') == MATCH_NO) {  
    y = g95_match_expr(&fp1->unit);   
    if (y == MATCH_ERROR) goto cleanup;   
    if (y == MATCH_NO) goto syntax;    
    
    goto done;          
  }

  y = match_file_element(fp1);       
  if (y == MATCH_ERROR) goto done;         
  if (y == MATCH_NO) {
    y = g95_match_expr(&fp1->unit);      
    if (y == MATCH_ERROR) goto done; 
    if (y == MATCH_NO) goto syntax;      
  }       
       
  for(;;) {      
    if (g95_match_char(')') == MATCH_YES) break;   
    if (g95_match_char(',') != MATCH_YES) goto syntax;  
  
    y = match_file_element(fp1);        
    if (y == MATCH_ERROR) goto cleanup; 
    if (y == MATCH_NO) goto syntax;    
  }          
          
done:          
  if (g95_match_eos() != MATCH_YES) goto syntax;

  if (g95_pure(NULL)) {         
    g95_error("%s statement not allowed in PURE procedure at %C",       
	      g95_ascii_statement(st0));         
         
    return MATCH_ERROR;     
  }          
          
  new_st.type = operand;    
  new_st.ext.filepos = fp1;        
  return MATCH_YES;      
      
syntax:       
  g95_syntax_error(st0); 
 
cleanup:          
  g95_free_filepos(fp1);  
  return MATCH_ERROR;  
} 
 
 
         
         
/* match_dt_unit()-- Match a unit specification for a data transfer
 * statement */        
        
static match match_dt_unit(g95_dt *t) { 
g95_expr *h;        
        
  if (g95_match_char('*') == MATCH_YES) {         
    if (t->io_unit != NULL) goto conflict;   
   
    t->io_unit = default_unit();        
    return MATCH_YES; 
  } 
 
  if (g95_match_expr(&h) == MATCH_YES) {        
    if (t->io_unit != NULL) {  
      g95_free_expr(h);       
      goto conflict;        
    }    
    
    t->io_unit = h;        
    return MATCH_YES;        
  }

  return MATCH_NO;        
        
conflict:
  g95_error("Duplicate UNIT specification at %C");    
  return MATCH_ERROR;         
}       
       
       
 
 
match g95_match_backspace(void) {          
          
  return match_filepos(ST_BACKSPACE, EXEC_BACKSPACE); 
}         
         
         
   
   
match g95_match_endfile(void) {          
          
  return match_filepos(ST_END_FILE, EXEC_ENDFILE);
} 
 
 
 
 
match g95_match_rewind(void) {       
       
  return match_filepos(ST_REWIND, EXEC_REWIND);
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
  /* The IOLENGTH form generates an EXEC_IOLENGTH node */     
     
  if (g95_reference_st_label(inquire->err, ST_LABEL_TARGET) == FAILURE)     
    return FAILURE; 
 
  return SUCCESS;   
}   
   
   
 
 
/* match_close_element()-- Match elements of a CLOSE statment */ 
 
static match match_close_element(g95_close *close) {    
match l;        
        
  l = match_etag(&tag_unit, &close->unit);        if (l != MATCH_NO) return l;       
  l = match_etag(&tag_cstatus, &close->status);   if (l != MATCH_NO) return l;        
  l = match_out_tag(&tag_iostat, &close->iostat); if (l != MATCH_NO) return l;  
  l = match_ltag(&tag_err, &close->err);          if (l != MATCH_NO) return l;

  return MATCH_NO;    
}  
  
  
      
      
/* match_io_element()-- Match a single element of an IO list, which is
 * either a single expression or an IO Iterator */

static match match_io_element(io_kind t, g95_code **cpp) {   
g95_expr *exp;       
g95_code *cp;        
match z;        
        
  exp = NULL;    
    
  z = match_io_iterator(t, cpp);     
  if (z == MATCH_YES) return MATCH_YES;       
       
  if (t == M_READ) {    
    z = g95_match_variable(&exp, 0);         
    if (z == MATCH_NO) g95_error("Expected variable in READ statement at %C");      
  } else {
    z = g95_match_expr(&exp);     
    if (z == MATCH_NO) g95_error("Expected expression in %s statement at %C",       
				 io_kind_name(t));       
  }         
         
  if (z == MATCH_YES)   
    switch(t) {
    case M_READ:   
      if (exp->symbol->attr.intent == INTENT_IN) {       
	g95_error("Variable '%s' in input list at %C cannot be INTENT(IN)",  
		  exp->symbol->name);      
	z = MATCH_ERROR;         
	break;          
      } 
 
      if (exp->symbol->attr.flavor == FL_PARAMETER) {   
	g95_error("Variable '%s' in input list at %C cannot be a PARAMETER",      
		  exp->symbol->name); 
	z = MATCH_ERROR;
	break; 
      }          
          
      if (g95_pure(NULL) && g95_impure_variable(exp->symbol) &&    
	  current_dt->io_unit->ts.type == BT_CHARACTER) {   
	g95_error("Cannot read to variable '%s' in PURE procedure at %C",      
		  exp->symbol->name);         
	z = MATCH_ERROR;    
	break;          
      } 
 
      break;

    case M_WRITE:          
      if (current_dt->io_unit->ts.type == BT_CHARACTER &&      
	  g95_pure(NULL) && current_dt->io_unit->type == EXPR_VARIABLE &&  
	  g95_impure_variable(current_dt->io_unit->symbol)) { 
	g95_error("Cannot write to internal file unit '%s' at %C inside a "
		  "PURE procedure", current_dt->io_unit->symbol->name);  
	z = MATCH_ERROR;        
      }       
       
      break;   
   
    default:         
      break;      
    }    
    
  if (z != MATCH_YES) {
    g95_free_expr(exp);         
    return MATCH_ERROR;  
  }       
       
  cp = g95_get_code(); 
  cp->type = EXEC_TRANSFER;      
  cp->expr = exp;         
  cp->ext.reading = (t == M_READ);

  *cpp = cp;    
  return MATCH_YES;          
}     
     
     
      
      
/* match_io_list()-- Match an I/O list, building g95_code structures
 * as we go. */     
     
static match match_io_list(io_kind y, g95_code **head_p) { 
g95_code *start, *end, *new;      
match f;     
     
  *head_p = start = end = NULL;    
  if (g95_match_eos() == MATCH_YES) return MATCH_YES;   
   
  for(;;) {         
    f = match_io_element(y, &new);       
    if (f == MATCH_ERROR) goto cleanup;     
    if (f == MATCH_NO) goto syntax;         
         
    end = g95_append_code(end, new);        
    if (start == NULL) start = new;      
      
    if (g95_match_eos() == MATCH_YES) break;  
    if (g95_match_char(',') != MATCH_YES) goto syntax;          
  }       
       
  *head_p = start;       
  return MATCH_YES;         
         
syntax:        
  g95_error("Syntax error in %s statement at %C", io_kind_name(y));      
      
cleanup:   
  g95_free_statements(start);        
  return MATCH_ERROR;  
}        
        
        
       
       
/* g95_match_close()-- Match a CLOSE statement */       
       
match g95_match_close(void) {
g95_close *close;       
match e;   
   
  e = g95_match_char('(');  
  if (e == MATCH_NO) return e;    
    
  close = g95_getmem(sizeof(g95_close)); 
 
  e = match_close_element(close);    
    
  if (e == MATCH_ERROR) goto cleanup;     
  if (e == MATCH_NO) {  
    e = g95_match_expr(&close->unit);          
    if (e == MATCH_NO) goto syntax;          
    if (e == MATCH_ERROR) goto cleanup;
  }  
  
  for(;;) {   
    if (g95_match_char(')') == MATCH_YES) break;  
    if (g95_match_char(',') != MATCH_YES) goto syntax; 
 
    e = match_close_element(close);       
    if (e == MATCH_ERROR) goto cleanup;          
    if (e == MATCH_NO) goto syntax;    
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
      
      
 
 
/* terminate_io()-- Attach the data transfer end node */        
        
static void terminate_io(g95_code *io_code) {   
g95_code *g;    
    
  if (io_code == NULL) io_code = &new_st;  
  
  g = g95_get_code();    
  g->type = EXEC_DT_END;          
  g->ext.dt = new_st.ext.dt;     /* Point to structure that is already there */  
  
  g95_append_code(io_code, g);       
}  
  
  
 
 
/* g95_match_open()-- Match an OPEN statmement */    
    
match g95_match_open(void) {       
g95_open *open;
match d;     
     
  d = g95_match_char('(');    
  if (d == MATCH_NO) return d;  
  
  open = g95_getmem(sizeof(g95_open));  
  
  d = match_open_element(open);      
      
  if (d == MATCH_ERROR) goto cleanup;          
  if (d == MATCH_NO) {
    d = g95_match_expr(&open->unit);         
    if (d == MATCH_NO) goto syntax;        
    if (d == MATCH_ERROR) goto cleanup; 
  }      
      
  for(;;) {
    if (g95_match_char(')') == MATCH_YES) break;       
    if (g95_match_char(',') != MATCH_YES) goto syntax;  
  
    d = match_open_element(open);         
    if (d == MATCH_ERROR) goto cleanup;     
    if (d == MATCH_NO) goto syntax;
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
     
     
          
          
/* match_dt_element()-- Match a single data transfer element */  
  
static match match_dt_element(io_kind u, g95_dt *d) {
char name0[G95_MAX_SYMBOL_LEN+1];
g95_symbol *sy;     
match o;

  if (g95_match(" unit =") == MATCH_YES) {         
    o = match_dt_unit(d);          
    if (o != MATCH_NO) return o;         
  }    
    
  if (g95_match(" fmt =") == MATCH_YES) {          
    o = match_dt_format(d);      
    if (o != MATCH_NO) return o;
  }   
   
  if (g95_match(" nml = %n", name0) == MATCH_YES) {    
    if (d->namelist != NULL) {   
      g95_error("Duplicate NML specification at %C");  
      return MATCH_ERROR;        
    }          
          
    if (g95_find_symbol(name0, NULL, 1, &sy)) return MATCH_ERROR;      
      
    if (sy == NULL || sy->attr.flavor != FL_NAMELIST) {       
      g95_error("Symbol '%s' at %C must be a NAMELIST group name",
		sy != NULL ? sy->name : name0 );         
      return MATCH_ERROR;         
    }

    d->namelist = sy;      
    if (u == M_READ && check_namelist(sy)) return MATCH_ERROR;       
       
    return MATCH_YES;         
  }      
      
  o = match_etag(&tag_rec, &d->rec);           if (o != MATCH_NO) return o;         
  o = match_out_tag(&tag_iostat, &d->iostat);  if (o != MATCH_NO) return o;      
  o = match_ltag(&tag_err, &d->err);           if (o != MATCH_NO) return o;     
  o = match_etag(&tag_advance, &d->advance);   if (o != MATCH_NO) return o;   
  o = match_out_tag(&tag_size, &d->size);      if (o != MATCH_NO) return o;          
          
  o = match_ltag(&tag_end, &d->end);          
  if (o == MATCH_YES) d->end_where = g95_current_locus;     
  if (o != MATCH_NO) return o;    
    
  o = match_ltag(&tag_eor, &d->eor);       
  if (o == MATCH_YES) d->eor_where = g95_current_locus; 
  if (o != MATCH_NO) return o;      
      
  return MATCH_NO;  
}  
  
  
     
     
/* match_io()-- Match a READ, WRITE or PRINT statement. */  
  
static match match_io(io_kind a) {         
char nam[G95_MAX_SYMBOL_LEN+1];
g95_code *io_code;      
g95_symbol *s;          
g95_locus where;
g95_expr *exp;   
int comma_flag;          
g95_dt *t;       
match e;        
        
  e = MATCH_NO;     
     
  comma_flag = 0;       
  current_dt = t = g95_getmem(sizeof(g95_dt));  
  
  if (g95_match_char('(') == MATCH_NO) {  
    if (a == M_WRITE) goto syntax;

    e = match_dt_format(t);          
    if (e == MATCH_ERROR) goto cleanup;          
    if (e == MATCH_NO) goto syntax;         
         
    comma_flag = 1;   
    t->io_unit = default_unit();     
    goto get_io_list;
  }   
   
/* Match a control list */        
        
  if (match_dt_element(a, t) == MATCH_YES) goto n;    
  if (match_dt_unit(t) != MATCH_YES) goto loop;    
    
  if (g95_match_char(')') == MATCH_YES) goto get_io_list;    
  if (g95_match_char(',') != MATCH_YES) goto syntax;     
     
  e = match_dt_element(a, t);      
  if (e == MATCH_YES) goto n;  
  if (e == MATCH_ERROR) goto cleanup;   
   
  e = match_dt_format(t); 
  if (e == MATCH_YES) goto n;          
  if (e == MATCH_ERROR) goto cleanup;        
        
  where = g95_current_locus;          
          
  if (g95_match_name(nam) == MATCH_YES && 
      !g95_find_symbol(nam, NULL, 1, &s) &&      
      s->attr.flavor == FL_NAMELIST) {          
    t->namelist = s;   
    if (a == M_READ && check_namelist(s)) {  
      e = MATCH_ERROR;        
      goto cleanup;   
    }    
    
    goto n;
  }          
          
  g95_current_locus = where;          
          
  goto loop;   /* No matches, try regular elements */        
        
n:      
  if (g95_match_char(')') == MATCH_YES) goto get_io_list;      
  if (g95_match_char(',') != MATCH_YES) goto syntax;   
   
loop:   
  for(;;) {       
    e = match_dt_element(a, t);       
    if (e == MATCH_NO) goto syntax;        
    if (e == MATCH_ERROR) goto cleanup; 
 
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
      e = MATCH_ERROR;
      goto cleanup;
    }    
    
    e = match_io_list(a, &io_code); 
    if (e == MATCH_ERROR) goto cleanup;       
    if (e == MATCH_NO) goto syntax;    
  }       
       
/* A full IO statement has been matched */      
      
  if (t->io_unit->type == EXPR_VARIABLE && a == M_WRITE &&
      t->io_unit->ts.type == BT_CHARACTER &&  
      t->io_unit->symbol->attr.intent == INTENT_IN) {        
    g95_error("Internal file '%s' at %L is INTENT(IN)",     
	      t->io_unit->symbol->name, &t->io_unit->where);
    e = MATCH_ERROR;        
    goto cleanup;         
  }      
      
  exp = t->format_expr;     
     
  if (exp != NULL && exp->type == EXPR_CONSTANT)       
    g95_check_format_string(exp); 
 
  if (g95_pure(NULL) && (a == M_READ || a == M_WRITE) &&        
      t->io_unit->ts.type != BT_CHARACTER) {      
    g95_error("io-unit in %s statement at %C must be an internal file in a "          
	      "PURE procedure", io_kind_name(a));       
    e = MATCH_ERROR;       
    goto cleanup;          
  }      
      
  new_st.type = (a == M_READ) ? EXEC_READ : EXEC_WRITE; 
  new_st.ext.dt = t; 
  new_st.next = io_code;

  terminate_io(io_code);       
       
  return MATCH_YES;     
     
syntax:   
  g95_error("Syntax error in %s statement at %C", io_kind_name(a));  
  e = MATCH_ERROR;   
   
cleanup:  
  g95_free_dt(t);        
  return e;       
}  
  
  
       
       
match g95_match_read(void)  { return match_io(M_READ);  }     
     


match g95_match_write(void) { return match_io(M_WRITE); }  
  
    
    
/* g95_io_init()-- Initialize I/O things. */    
    
void g95_io_init(void) {       
       
  g95_format_asterisk.value = -1;        
  g95_format_asterisk.format = NULL;    
  g95_format_asterisk.length = 0; 
}     
    
    
match g95_match_inquire(void) {     
g95_inquire *inquire;   
g95_code *c; 
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
 
    m = match_io_list(M_INQUIRE, &c);
    if (m == MATCH_ERROR) goto cleanup;  
    if (m == MATCH_NO) goto syntax; 
 
    terminate_io(c);     
     
    new_st.type = EXEC_IOLENGTH;    
    new_st.expr = inquire->iolength; 
    g95_free(inquire);        
        
    if (g95_pure(NULL)) {    
      g95_free_statements(c);        
      g95_error("INQUIRE statement not allowed in PURE procedure at %C");          
      return MATCH_ERROR;         
    }          
          
    new_st.next = c;      
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
  
  
          
          
match g95_match_print(void) {       
match c; 
 
  c = match_io(M_PRINT);   
  if (c != MATCH_YES) return c;      
      
  if (g95_pure(NULL)) {
    g95_error("PRINT statement at %C not allowed within PURE procedure");         
    return MATCH_ERROR;    
  }       
       
  return MATCH_YES;    
}        
        
        
