/* Set up intrinsic functions
   Copyright (C) 2000, 2001, 2002 Free Software Foundation, Inc.
   Contributed by Andy Vaught & Katherine Holcomb

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
    
    
/* intrinsic.c-- Build up a list of intrinsic subroutines and
 * functions for the name-resolution stage. */   
   
#include <stdio.h>
#include <stdarg.h>
#include <string.h>
#include <gmp.h>
        
#include "g95.h"
#include "intrinsic.h"
         
         
int g95_intrinsic_extension, g95_init_expr=0;       
       
/* Pointers to a intrinsic function and its argument names being
 * checked. */          
          
char *g95_current_intrinsic, *g95_current_intrinsic_arg[MAX_INTRINSIC_ARGS];      
g95_locus *g95_current_intrinsic_where;

static g95_intrinsic_sym *functions, *subroutines, *conversion, *next_sym; 
static g95_intrinsic_arg *next_arg; 
 
static int nfunc, nsub, nargs, nconv;       
       
static enum { SZ_NOTHING=0, SZ_SUBS, SZ_FUNCS, SZ_CONVS } sizing;

static try do_simplify(g95_intrinsic_sym *, g95_expr *);          
          
          
  
  
/* g95_generic_intrinsic()-- Given a string, figure out if it is
 * the name of a generic intrinsic function or not. */  
  
int g95_generic_intrinsic(char *n) {         
g95_intrinsic_sym *sym;         
         
  sym = g95_find_function(n);      
  return (sym == NULL) ? 0 : sym->generic;
}        
        
        
        
        
/* check_arglist()-- Compare an actual argument list with an
 * intrinsic's formal argument list.  The lists are checked for
 * agreement of type.  We don't check for arrayness here. */        
        
static try check_arglist(g95_actual_arglist **ap, g95_intrinsic_sym *symbol,         
			 int error_flag) {   
g95_actual_arglist *a;       
g95_intrinsic_arg *frm;    
int w;         
         
  frm = symbol->formal;     
  a = *ap;         
         
  w = 0;       
  for(; frm; frm=frm->next, a=a->next, w++) {          
    if (a->u.expr == NULL) continue;        
        
    if (!g95_compare_types(&frm->ts, &a->u.expr->ts)) {    
      if (error_flag)        
	g95_error("Type of argument '%s' in call to '%s' at %L should be "       
		  "%s, not %s", g95_current_intrinsic_arg[w],     
		  g95_current_intrinsic, &a->u.expr->where,
		  g95_typename(&frm->ts),     
		  g95_typename(&a->u.expr->ts));
      return FAILURE;   
    }         
  }    
    
  return SUCCESS;  
}    
    
    
   
   
/* convert_constant()-- Master function to convert one constant to
 * another.  While this is used as a simplification function, it
 * requires the destination type and kind information which is
 * supplied by a special case in do_simplify(). */

static g95_expr *convert_constant(g95_expr *i, bt dtype, int knd) { 
g95_expr *result, *(*k)(g95_expr *, int);    
    
  switch(i->ts.type) {        
  case BT_INTEGER:        
    switch(dtype) { 
    case BT_INTEGER:  k = g95_int2int;          break;       
    case BT_REAL:     k = g95_int2real;         break;     
    case BT_COMPLEX:  k = g95_int2complex;      break;          
    default: goto oops;  
    }          
    break;  
  
  case BT_REAL:  
    switch(dtype) {
    case BT_INTEGER:  k = g95_real2int;         break;        
    case BT_REAL:     k = g95_real2real;        break;    
    case BT_COMPLEX:  k = g95_real2complex;     break;         
    default: goto oops;        
    } 
    break;        
        
  case BT_COMPLEX: 
    switch(dtype) {  
    case BT_INTEGER:  k = g95_complex2int;      break;        
    case BT_REAL:     k = g95_complex2real;     break;      
    case BT_COMPLEX:  k = g95_complex2complex;  break;   
   
    default: goto oops; 
    }
    break; 
 
  case BT_LOGICAL:    
    if (dtype != BT_LOGICAL) goto oops; 
    k = g95_log2log;      
    break;

  default: oops:         
    g95_internal_error("convert_constant(): Unexpected type");        
  } 
 
  result = NULL; 
 
  /* Convert a constant */        
        
  if (i->type == EXPR_CONSTANT) {  
    result = k(i, knd);      
    if (result == NULL) return &g95_bad_expr;      
  }   
   
  return result;
}         
         
         
    
    
/* g95_has_alt_return()-- Checks an actual argument list to see if
 * there is an alternate return buried someplace in it.  Returns
 * nonzero if so, zero if an alternate return is present. */  
  
int g95_has_alt_return(g95_actual_arglist *u) {         
         
  while(u != NULL) { 
    if (u->type == ALT_RETURN) return 1; 
    u = u->next;          
  } 
 
  return 0;  
}     
     
     
     
     
/* make_generic()-- Collect a set of intrinsic functions into a
 * generic collection.  The first argument is the name of the generic
 * function, which is also the name of a specific function.  The rest
 * of the specifics currently in the table are placed into the list of
 * specific functions associated with that generic.  */ 
 
static void make_generic(char *name0, enum g95_generic_isym_id generic_id) {      
g95_intrinsic_sym *s;     
     
  if (sizing != SZ_NOTHING) return;        
       
  s = g95_find_function(name0);     
  if (s == NULL)     
    g95_internal_error("make_generic(): Can't find generic symbol '%s'", name0);

  s->generic = 1;    
  s->specific = 1;        
  s->generic_id = generic_id; 
  if ((s+1)->name[0] != '\0') s->specific_head = s + 1;  
  s++; 
   
  while(s->name[0] != '\0') {      
    s->next = s + 1;          
    s->specific = 1;         
    s->generic_id = generic_id;          
    s++;   
  }      
      
  s--;     
  s->next = NULL; 
}         
         
         
         
         
/* g95_type_letter()-- Return a letter based on the passed type.  Used
 * to construct the name of a type-dependent subroutine. */       
       
char g95_type_letter(bt dtype) {     
char l;   
   
  switch(dtype) { 
  case BT_LOGICAL:    l = 'l';  break;  
  case BT_CHARACTER:  l = 'c';  break;   
  case BT_INTEGER:    l = 'i';  break;     
  case BT_REAL:       l = 'r';  break;        
  case BT_COMPLEX:    l = 'z';  break;        
  case BT_DERIVED:    l = 'd';  break; 
 
  default:            l = 'u';  break;       
  }    
    
  return l;    
}       
       
       
         
         
/* find_sym()-- Locate an intrinsic symbol given a base pointer,
 * number of elements in the table and a pointer to a name.  Returns
 * the NULL pointer if a name is not found. */        
        
static g95_intrinsic_sym *find_sym(g95_intrinsic_sym *st, int k,          
				   char *name) {     
     
  while(k > 0) {
    if (strcmp(name, st->name) == 0) return st; 
 
    st++;    
    k--;       
  }

  return NULL;         
} 
 
 
      
      
/* find_subroutine()-- Given a name, find a function in the intrinsic
 * subroutine table.  Returns NULL if not found. */        
        
static g95_intrinsic_sym *find_subroutine(char *name0) {         
         
  return find_sym(subroutines, nsub, name0);        
}    
    
    
  
  
/* resolve_intrinsic()-- Given a pointer to an intrinsic symbol and an
 * expression node that represent the function call to that
 * subroutine, figure out the type of the result.  This may involve
 * calling a resolution subroutine */      
      
static void resolve_intrinsic(g95_intrinsic_sym *specific, g95_expr *z) {   
g95_expr *j, *a2, *l, *y, *m;      
g95_actual_arglist *arg;        
        
  if (specific->resolve == NULL) {         
    if (z->value.function.name == NULL)        
      z->value.function.name = specific->lib_name;      
      
    if (z->ts.type == BT_UNKNOWN) z->ts = specific->ts;          
    return;       
  }      
      
  arg = z->value.function.actual; 
 
/* At present only the iargc extension intrinsic takes no arguments,
 * and it doesn't need a resolution function, but this is here for
 * generality */   
   
  if (arg == NULL) {   
    (*specific->resolve)(z); 
    return;         
  }     
     
  j = arg->u.expr;         
  arg = arg->next;       
       
  if (specific->resolve == g95_resolve_min_max) {        
    g95_resolve_min_max(z, specific, j);         
    return;
  }   
   
  if (arg == NULL) {   
    (*specific->resolve)(z, j);       
    return;         
  }

  a2 = arg->u.expr;      
  arg = arg->next;       
       
  if (arg == NULL) {  
    (*specific->resolve)(z, j, a2);      
    return;  
  } 
 
  l = arg->u.expr; 
  arg = arg->next; 
       
  if (arg == NULL) {        
    (*specific->resolve)(z, j, a2, l);  
    return;
  }        
        
  y = arg->u.expr;          
  arg = arg->next; 
 
  if (arg == NULL) {          
    (*specific->resolve)(z, j, a2, l, y);         
    return; 
  }        
        
  m = arg->u.expr;    
  arg = arg->next;        
        
  if (arg == NULL) {          
    (*specific->resolve)(z, j, a2, l, y, m);       
    return;   
  }          
          
  g95_internal_error("resolve_intrinsic(): Too many args for intrinsic");          
}      
      
      
   
   
/* make_alias()-- Create a duplicate intrinsic function entry for the
 * current function, the only difference being the alternate name.
 * Note that we use argument lists more than once, but all argument
 * lists are freed as a single block.  */

static void make_alias(char *nam) {       
       
  switch(sizing) { 
  case SZ_FUNCS:    
    nfunc++; 
    break;     
     
  case SZ_SUBS:   
    nsub++; 
    break;       
       
  case SZ_NOTHING:        
    next_sym[0] = next_sym[-1];  
    strcpy(next_sym->name, nam);          
    next_sym++;      
    break;        
        
  default:         
    break;      
  }
} 
 
 
        
        
/* g95_intrinsic_name()-- Given a string, figure out if it is the name
 * of an intrinsic subroutine or function.  There are no generic
 * intrinsic subroutines, they are all specific. */       
       
int g95_intrinsic_name(char *nam, int subroutine_flag) {     
     
  return subroutine_flag ?   
    find_subroutine(nam) != NULL :      
    g95_find_function(nam) != NULL;       
}


 
 
/* do_check()-- Interface to the check functions.  We break apart an
 * argument list and call the proper check function rather than
 * forcing each function to manipulate the argument list */          
          
static try do_check(g95_intrinsic_sym *specific, g95_actual_arglist *ap) {        
g95_expr *s, *r, *b, *y, *v;     
try x;   
   
  s = ap->u.expr;
  ap = ap->next;       
       
  if (ap == NULL)  
    x = (*specific->check)(s);      
  else {       
    r = ap->u.expr;       
    ap = ap->next;        
        
    if (ap == NULL)   
      x = (*specific->check)(s, r);         
    else { 
      b = ap->u.expr; 
      ap = ap->next;       
             
      if (ap == NULL)        
	x = (*specific->check)(s, r, b);  
      else {          
	y = ap->u.expr;          
	ap = ap->next;   
   
	if (ap == NULL)         
	  x = (*specific->check)(s, r, b, y);        
	else { 
	  v = ap->u.expr;       
	  ap = ap->next;        
        
	  if (ap == NULL) 
	    x = (*specific->check)(s, r, b, y, v);        
	  else {   
	    g95_internal_error("do_check(): too many args");    
	  }         
	}     
      }      
    }
  }        
        
  return x;    
} 
 
 
      
      
/* g95_find_function()-- Given a name, find a function in the intrinsic
 * function table.  Returns NULL if not found. */        
        
g95_intrinsic_sym *g95_find_function(char *nm) {  
  
  return find_sym(functions, nfunc, nm);      
}    
    
    
 
 
/* conv_name()-- Return a pointer to the name of a conversion function
 * given two typespecs. */     
     
static char *conv_name(g95_typespec *from, g95_typespec *d) {   
static char nm[30];      
      
  sprintf(nm, "__convert_%c%d_%c%d", g95_type_letter(from->type), from->kind,       
	  g95_type_letter(d->type), d->kind);     
     
  return nm;      
}       
       
       
    
    
void g95_intrinsic_done_1(void) {        
        
  g95_free(functions);   
  g95_free(conversion);        
}   
   
   
        
/*********** Subroutines to build the intrinsic list ****************/     
     
/* add_sym()-- Add a single intrinsic symbol to the current list. 
 * Argument list:
 *    char *     name of function
 *    int        whether function is elemental
 *    int        If the function can be used as an actual argument
 *    bt         return type of function
 *    int        kind of return type of function
 *    check      pointer to check function
 *    simplify   pointer to simplification function
 *    resolve    pointer to resolution function
 * Optional arguments come in multiples of four:
 *    char *    name of argument
 *    bt        type of argument
 *    int       kind of argument
 *    int       arg optional flag (1=optional, 0=required)
 *
 * the sequence is terminated by a NULL name. */        
        
static void add_sym(char *nam, int elemental, int actual_ok, bt t,      
		    int knd, try (*check)(), g95_expr *(*simplify)(),    
		    void (*resolve)(), ...) {      
      
int optional, first_flag; 
va_list argp;       
       
  switch(sizing) {          
  case SZ_SUBS:         
    nsub++;      
    break;       
       
  case SZ_FUNCS:     
    nfunc++;
    break;      
      
  case SZ_NOTHING:       
    strcpy(next_sym->name, nam);   
   
    strcpy(next_sym->lib_name, "_g95_");       
    strcat(next_sym->lib_name, nam);   
   
    next_sym->elemental = elemental;        
    next_sym->ts.type = t;          
    next_sym->ts.kind = knd;  
    next_sym->simplify = simplify;        
    next_sym->check = check;     
    next_sym->resolve = resolve;  
    next_sym->specific = 0;   
    next_sym->generic = 0; 
    break;       
       
  default:     
    g95_internal_error("add_sym(): Bad sizing mode");     
  }          
          
  va_start(argp, resolve);        
        
  first_flag = 1;       
       
  for(;;) {
    nam = va_arg(argp, char *);       
    if (nam == NULL) break;       
       
    t = (bt) va_arg(argp, int);         
    knd = va_arg(argp, int);  
    optional = va_arg(argp, int);         
         
    if (sizing != SZ_NOTHING)    
      nargs++;        
    else {   
      next_arg++;     
     
      if (first_flag) 
	next_sym->formal = next_arg;        
      else          
	(next_arg-1)->next = next_arg; 
 
      first_flag = 0;      
      
      strcpy(next_arg->name, nam);  
      next_arg->ts.type = t;
      next_arg->ts.kind = knd;        
      next_arg->optional = optional; 
    }    
  }     
     
  va_end(argp);

  next_sym++;
}  
  
  
   
   
/* find_conv()-- Given a pair of typespecs, find the g95_intrinsic_sym
 * node that corresponds to the conversion.  Returns NULL if the
 * conversion isn't found. */         
         
static g95_intrinsic_sym *find_conv(g95_typespec *t, g95_typespec *d) {
g95_intrinsic_sym *symbol;       
char *target;       
int q; 
 
  target = conv_name(t, d);        
  symbol = conversion;    
    
  for(q=0; q<nconv; q++, symbol++)        
    if (strcmp(target, symbol->name) == 0) return symbol;  
  
  return NULL; 
}          
          
          
  
  
/* g95_specific_intrinsic()-- Given a string, figure out if it is the
 * name of a specific intrinsic function or not. */          
          
int g95_specific_intrinsic(char *nam) {
g95_intrinsic_sym *s;      
      
  s = g95_find_function(nam);       
  return (s == NULL) ? 0 : s->specific;        
}       
       
       
      
      
/* add_subroutines()-- Add intrinsic subroutines */         
         
static void add_subroutines(void) {         
         
/* Argument names as in the standard (to be used as argument keywords) */        
char   b[] = "harvest", dt[] = "date", vl[] = "values",  pt[] = "put",      
       j[] = "count",   tm[] = "time", tp[] = "topos",   gt[] = "get",      
       d[] = "to",      zn[] = "zone", fp1[] = "frompos", cm[] = "count_max",  
       u[] = "from",    sz[] = "size", ln[] = "len",     cr[] = "count_rate";

int di, dr, dc;  
  
  di = g95_default_integer_kind();    
  dr = g95_default_real_kind();        
  dc = g95_default_character_kind();         
         
  add_sym("cpu_time", 0, 0, BT_UNKNOWN, 0,       
	  g95_check_cpu_time, NULL, g95_resolve_cpu_time,      
	  tm, BT_REAL, dr, 0, NULL);        
        
  add_sym("date_and_time", 0, 0, BT_UNKNOWN, 0,         
	  g95_check_date_and_time, NULL, NULL,         
	  dt, BT_CHARACTER, dc, 1,   tm, BT_CHARACTER, dc, 1,        
	  zn, BT_CHARACTER, dc, 1,   vl, BT_INTEGER,   di, 1, NULL);        
        
  add_sym("mvbits", 1, 0, BT_UNKNOWN, 0, 
	  g95_check_mvbits, g95_simplify_mvbits, NULL, 
	  u, BT_INTEGER, di, 0,   fp1, BT_INTEGER, di, 0,  
	  ln, BT_INTEGER, di, 0,   d, BT_INTEGER, di, 0,     
	  tp, BT_INTEGER, di, 0, NULL); 
 
  add_sym("random_number", 1, 0, BT_UNKNOWN, 0,        
	  g95_check_random_number, NULL, g95_resolve_random_number, 
	  b, BT_REAL, dr, 0, NULL);       
       
  add_sym("random_seed", 0, 1, BT_UNKNOWN, 0,   
	  g95_check_random_seed, NULL, NULL,
	  sz, BT_INTEGER, di, 1,   pt, BT_INTEGER, di, 1,         
	  gt, BT_INTEGER, di, 1, NULL);         
         
  add_sym("system_clock", 0, 0, BT_UNKNOWN, 0,      
	  NULL, NULL, NULL,     
	  j,  BT_INTEGER, di, 1,   cr, BT_INTEGER, di, 1,    
	  cm, BT_INTEGER, di, 1, NULL);      
}       
       
       
          
          
/* add_functions()-- Add intrinsic functions */          
          
static void add_functions(void) {

/* Argument names as in the standard (to be used as argument keywords) */          
          
char   a[] = "a",  g[] = "field",      pt[] = "pointer",   tg[] = "target",     
       q[] = "b",  m[] = "matrix",     ma[] = "matrix_a",  mb[] = "matrix_b",
       h[] = "c",  n[] = "ncopies",   pos[] = "pos",      bck[] = "back",    
       i[] = "i",  v[] = "vector",     va[] = "vector_a",  vb[] = "vector_b", 
       u[] = "j", t[] = "a1",         fs[] = "fsource",   ts[] = "tsource",      
       o[] = "l", d[] = "a2",         mo[] = "mold",     ord[] = "order",          
       p[] = "p", ar[] = "array",     shp[] = "shape",    source[] = "source",        
       k[] = "r", bd[] = "boundary",  pad[] = "pad",      set[] = "set",    
       s[] = "s", dm[] = "dim",      knd[] = "kind",     msk[] = "mask",   
       x[] = "x", sh[] = "shift",     stg[] = "string",   ssg[] = "substring",      
       w[] = "y", sz[] = "size",      sta[] = "string_a", stb[] = "string_b",        
       e[] = "z", ln[] = "len"; 
 
int di, dr, dd, dl, dc, dz;   
   
  di = g95_default_integer_kind();     
  dr = g95_default_real_kind();        
  dd = g95_default_double_kind();
  dl = g95_default_logical_kind();          
  dc = g95_default_character_kind();     
  dz = g95_default_complex_kind();   
   
  add_sym("abs", 1, 1, BT_REAL, dr,          
	  g95_check_abs, g95_simplify_abs, g95_resolve_abs,    
	  a, BT_REAL, dr, 0, NULL);

  add_sym("iabs", 1, 1, BT_INTEGER, di,   
	  NULL, g95_simplify_abs, g95_resolve_abs,       
	  a, BT_INTEGER, di, 0, NULL);         
         
  add_sym("dabs", 1, 1, BT_REAL, dd,        
	  NULL, g95_simplify_abs, g95_resolve_abs,      
	  a, BT_REAL, dd, 0, NULL);     
     
  add_sym("cabs", 1, 1, BT_REAL, dr,   
	  NULL, g95_simplify_abs, g95_resolve_abs,          
	  a, BT_COMPLEX, dz, 0, NULL);          
          
  add_sym("zabs", 1, 1, BT_REAL, dd,         
	  NULL, g95_simplify_abs, g95_resolve_abs,  
	  a, BT_COMPLEX, dd, 0, NULL);   /* Extension */       
       
  if (g95_option.fmode == 0)         
    make_alias("cdabs");        
        
  make_generic("abs", G95_ISYM_ABS);  
  
  add_sym("achar", 1, 1, BT_CHARACTER, dc,         
	  NULL, g95_simplify_achar, g95_resolve_achar,    
	  i, BT_INTEGER, di, 0, NULL);   
   
  make_generic("achar", G95_ISYM_ACHAR);        
        
  add_sym("acos", 1, 1, BT_REAL, dr,    
	  NULL, g95_simplify_acos, g95_resolve_acos,          
	  x, BT_REAL, dr, 0, NULL);   
   
  add_sym("dacos", 1, 1, BT_REAL, dd,     
	  NULL, g95_simplify_acos, g95_resolve_acos,
	  x, BT_REAL, dd, 0, NULL); 
 
  make_generic("acos", G95_ISYM_ACOS);    
    
  add_sym("adjustl", 1, 1, BT_CHARACTER, dc, 
	  NULL, g95_simplify_adjustl, NULL,
	  stg, BT_CHARACTER, dc, 0, NULL);   
   
  make_generic("adjustl", G95_ISYM_ADJUSTL); 
 
  add_sym("adjustr", 1, 1, BT_CHARACTER, dc,   
	  NULL, g95_simplify_adjustr, NULL,      
	  stg, BT_CHARACTER, dc, 0, NULL);      
      
  make_generic("adjustr", G95_ISYM_ADJUSTR);  
  
  add_sym("aimag", 1, 1, BT_REAL, dr,     
	  NULL, g95_simplify_aimag, g95_resolve_aimag, 
	  e, BT_COMPLEX, dz, 0, NULL);         
         
  if (g95_option.fmode == 0)         
    make_alias("imag");

  add_sym("dimag", 1, 1, BT_REAL, dd,  
	  NULL, g95_simplify_aimag, g95_resolve_aimag,        
	  e, BT_COMPLEX, dd, 0, NULL);    /* Extension */         
         
  make_generic("aimag", G95_ISYM_AIMAG);     
     
  add_sym("aint", 1, 1, BT_REAL, dr, 
	  g95_check_a_xkind, g95_simplify_aint, g95_resolve_aint,       
	  a, BT_REAL, dr, 0,   knd, BT_INTEGER, di, 1, NULL);   
   
  add_sym("dint", 1, 1, BT_REAL, dd,    
	  NULL, g95_simplify_dint, NULL,          
	  a, BT_REAL, dd, 0, NULL);       
       
  make_generic("aint", G95_ISYM_AINT);  
  
  add_sym("all", 0, 1, BT_UNKNOWN, 0,         
	  g95_check_all_any, NULL, g95_resolve_all,    
	  msk, BT_LOGICAL, dl, 0,   dm, BT_INTEGER, di, 1, NULL);          
          
  make_generic("all", G95_ISYM_ALL);        
        
  add_sym("allocated", 0, 1, BT_LOGICAL, dl,         
	  g95_check_allocated, NULL, NULL,    
	  ar, BT_UNKNOWN, 0, 0, NULL);  
  
  make_generic("allocated", G95_ISYM_ALLOCATED);   
   
  add_sym("anint", 1, 1, BT_REAL, dr,        
	  g95_check_a_xkind, g95_simplify_anint, g95_resolve_anint,       
	  a, BT_REAL, dr, 0,   knd, BT_INTEGER, di, 1, NULL);      
      
  add_sym("dnint", 1, 1, BT_REAL, dd,        
	  NULL, g95_simplify_dnint, NULL,
	  a, BT_REAL, dd, 0, NULL);      
      
  make_generic("anint", G95_ISYM_ANINT);     
     
  add_sym("any", 0, 1, BT_UNKNOWN, 0,       
	  g95_check_all_any, NULL, g95_resolve_any, 
	  msk, BT_LOGICAL, dl, 0, dm, BT_INTEGER, di, 1, NULL);     
     
  make_generic("any", G95_ISYM_ANY);      
      
  add_sym("asin", 1, 1, BT_REAL, dr,    
	  NULL, g95_simplify_asin, g95_resolve_asin,    
	  x, BT_REAL, dr, 0, NULL);     
     
  add_sym("dasin", 1, 1, BT_REAL, dd,   
	  NULL, g95_simplify_asin, g95_resolve_asin,    
	  x, BT_REAL, dd, 0, NULL);   
   
  make_generic("asin", G95_ISYM_ASIN);     
     
  add_sym("associated", 0, 1, BT_LOGICAL, dl,       
	  g95_check_associated, NULL, NULL,   
	  pt, BT_UNKNOWN, 0, 0,   tg, BT_INTEGER, di, 1, NULL);     
     
  make_generic("associated", G95_ISYM_ASSOCIATED);          
          
  add_sym("atan", 1, 1, BT_REAL, dr,  
	  NULL, g95_simplify_atan, g95_resolve_atan,     
	  x, BT_REAL, dr, 0, NULL);    
    
  add_sym("datan", 1, 1, BT_REAL, dd,   
	  NULL, g95_simplify_atan, g95_resolve_atan,        
	  x, BT_REAL, dd, 0, NULL);         
         
  make_generic("atan", G95_ISYM_ATAN);     
     
  add_sym("atan2", 1, 1, BT_REAL, dr,         
	  NULL, g95_simplify_atan2, g95_resolve_atan2,  
	  w, BT_REAL, dr, 0,   x, BT_REAL, dr, 0, NULL);     
     
  add_sym("datan2", 1, 1, BT_REAL, dd, 
	  NULL, g95_simplify_atan2, g95_resolve_atan2,          
	  w, BT_REAL, dd, 0,   x, BT_REAL, dd, 0, NULL);

  make_generic("atan2", G95_ISYM_ATAN2);

  add_sym("bit_size", 0, 1, BT_INTEGER, di,   
	  g95_check_i, g95_simplify_bit_size, NULL,        
	  i, BT_INTEGER, di, 0, NULL);      
      
  make_generic("bit_size", G95_ISYM_NONE);     
     
  add_sym("btest", 1, 1, BT_LOGICAL, dl,   
	  g95_check_btest, g95_simplify_btest, g95_resolve_btest,       
	  i, BT_INTEGER, di, 0,   pos, BT_INTEGER, di, 0, NULL);       
       
  make_generic("btest", G95_ISYM_BTEST);     
     
  add_sym("ceiling", 1, 1, BT_INTEGER, di,      
	  g95_check_a_ikind, g95_simplify_ceiling, g95_resolve_ceiling,          
	  a, BT_REAL, dr, 0,   knd, BT_INTEGER, di, 1, NULL);       
       
  make_generic("ceiling", G95_ISYM_CEILING);        
        
  add_sym("char", 1, 0, BT_CHARACTER, dc,         
	  g95_check_char, g95_simplify_char, g95_resolve_char, 
	  i, BT_INTEGER, di, 0,   knd, BT_INTEGER, di, 1, NULL);

  make_generic("char", G95_ISYM_CHAR);          
          
  add_sym("cmplx", 1, 1, BT_COMPLEX, dz,        
	  g95_check_cmplx, g95_simplify_cmplx, g95_resolve_cmplx,      
	  x, BT_UNKNOWN, dr, 0,   w, BT_UNKNOWN, dr, 1,
	  knd, BT_INTEGER, di, 1, NULL);         
         
  make_generic("cmplx", G95_ISYM_CMPLX);     
     
  /* Making dcmplx a specific of cmplx causes cmplx to return a double
   * complex instead of the default complex.  */       
       
  if (g95_option.fmode == 0) {   
    add_sym("dcmplx", 1, 1, BT_COMPLEX, dd,
	    g95_check_dcmplx, g95_simplify_dcmplx, NULL, 
	    x, BT_REAL, dd, 0,   w, BT_REAL, dd, 1, NULL);  /* Extension */  
  
    make_generic("dcmplx", G95_ISYM_CMPLX);         
  } 
 
  add_sym("conjg", 1, 1, BT_COMPLEX, dz,       
	  NULL, g95_simplify_conjg, g95_resolve_conjg, 
	  e, BT_COMPLEX, dz, 0, NULL);      
      
  if (g95_option.fmode == 0)    
    add_sym("dconjg", 1, 1, BT_COMPLEX, dd,       
	    NULL, g95_simplify_conjg, g95_resolve_conjg,    
	    e, BT_COMPLEX, dd, 0, NULL);          
          
  make_generic("conjg", G95_ISYM_CONJG);      
      
  add_sym("cos", 1, 1, BT_REAL, dr,   
	  NULL, g95_simplify_cos, g95_resolve_cos,          
	  x, BT_REAL, dr, 0, NULL); 
 
  add_sym("dcos", 1, 1, BT_REAL, dd,         
	  NULL, g95_simplify_cos, g95_resolve_cos,    
	  x, BT_REAL, dd, 0, NULL);      
      
  add_sym("ccos", 1, 1, BT_COMPLEX, dz,  
	  NULL, g95_simplify_cos, g95_resolve_cos,         
	  x, BT_COMPLEX, dz, 0, NULL);        
        
  if (g95_option.fmode == 0) {      
    add_sym("zcos", 1, 1, BT_COMPLEX, dd,      
	    NULL, g95_simplify_cos, g95_resolve_cos,   
	    x, BT_COMPLEX, dd, 0, NULL);  
  
    make_alias("cdcos");        
  }

  make_generic("cos", G95_ISYM_COS);   
   
  add_sym("cosh", 1, 1, BT_REAL, dr, 
	  NULL, g95_simplify_cosh, g95_resolve_cosh, 
	  x, BT_REAL, dr, 0, NULL); 
 
  add_sym("dcosh", 1, 1, BT_REAL, dd,       
	  NULL, g95_simplify_cosh, g95_resolve_cosh,  
	  x, BT_REAL, dd, 0, NULL);         
         
  make_generic("cosh", G95_ISYM_COSH);        
        
  add_sym("count", 0, 1, BT_INTEGER, di,          
	  g95_check_count, NULL, g95_resolve_count,
	  msk, BT_LOGICAL, dl, 0,   dm, BT_INTEGER, di, 1, NULL);

  make_generic("count", G95_ISYM_COUNT);        
        
  add_sym("cshift", 0, 1, BT_REAL, dr,          
	  g95_check_cshift, NULL, g95_resolve_cshift,       
	  ar, BT_REAL, dr, 0,   sh, BT_INTEGER, di, 0,      
	  dm, BT_INTEGER, di, 1, NULL);     
     
  make_generic("cshift", G95_ISYM_CSHIFT);

  add_sym("dble", 1, 1, BT_REAL, dd, 
	  g95_check_dble, g95_simplify_dble, g95_resolve_dble,        
	  a, BT_REAL, dr, 0, NULL);

  if (g95_option.fmode == 0)   
    make_alias("dfloat");    
    
  make_generic("dble", G95_ISYM_DBLE);     
     
  add_sym("digits", 0, 1, BT_INTEGER, di,        
	  g95_check_digits, g95_simplify_digits, NULL,         
	  x, BT_UNKNOWN, dr, 0, NULL);    
    
  make_generic("digits", G95_ISYM_NONE);        
        
  add_sym("dim", 1, 1, BT_REAL, dr,         
	  g95_check_a_p, g95_simplify_dim, g95_resolve_dim,    
	  x, BT_UNKNOWN, dr, 0,   w, BT_UNKNOWN, dr, 0, NULL);         
         
  add_sym("idim", 1, 1, BT_INTEGER, di,         
	  NULL, g95_simplify_dim, g95_resolve_dim,       
	  x, BT_INTEGER, di, 0,   w, BT_INTEGER, di, 0, NULL);

  add_sym("ddim", 1, 1, BT_REAL, dd,  
	  NULL, g95_simplify_dim, g95_resolve_dim,
	  x, BT_REAL, dd, 0,   w, BT_REAL, dd, 0, NULL);       
       
  make_generic("dim", G95_ISYM_DIM);  
  
  add_sym("dot_product", 0, 1, BT_UNKNOWN, 0,      
	  g95_check_dot_product, NULL, g95_resolve_dot_product,       
	  va, BT_REAL, dr, 0,   vb, BT_REAL, dr, 0, NULL);       
       
  make_generic("dot_product", G95_ISYM_DOT_PRODUCT);      
      
  add_sym("dprod", 1, 1, BT_REAL, dd,       
	  NULL, g95_simplify_dprod, NULL,         
	  x, BT_REAL, dr, 0,   w, BT_REAL, dr, 0, NULL); 
 
  make_generic("dprod", G95_ISYM_DPROD);     
     
  if (g95_option.fmode == 0)      
    add_sym("dreal", 1, 0, BT_REAL, dd,    
	    NULL, NULL, NULL,          
	    a, BT_COMPLEX, dd, 0,  NULL);         
         
  add_sym("eoshift", 0, 1, BT_REAL, dr,  
	  g95_check_eoshift, NULL, g95_resolve_eoshift,
	  ar, BT_REAL, dr, 0,   sh, BT_INTEGER, di, 0,    
	  bd, BT_REAL, dr, 1,   dm, BT_INTEGER, di, 1, NULL);         
         
  make_generic("eoshift", G95_ISYM_EOSHIFT);         
         
  add_sym("epsilon", 0, 1, BT_REAL, dr,          
	  g95_check_x_ni, g95_simplify_epsilon, NULL, 
	  x, BT_REAL, dr, 0, NULL);  
  
  make_generic("epsilon", G95_ISYM_NONE);          
          
  add_sym("exp", 1, 1, BT_REAL, dr,      
	  NULL, g95_simplify_exp, g95_resolve_exp,      
	  x, BT_REAL, dr, 0, NULL);     
     
  add_sym("dexp", 1, 1, BT_REAL, dd,    
	  NULL, g95_simplify_exp, g95_resolve_exp,      
	  x, BT_REAL, dd, 0, NULL);          
          
  add_sym("cexp", 1, 1, BT_COMPLEX, dz,  
	  NULL, g95_simplify_exp, g95_resolve_exp,         
	  x, BT_COMPLEX, dz, 0, NULL);        
        
  if (g95_option.fmode == 0) {    
    add_sym("zexp", 1, 1, BT_COMPLEX, dd, 
	    NULL, g95_simplify_exp, g95_resolve_exp,       
	    x, BT_COMPLEX, dd, 0, NULL);       
       
    make_alias("cdexp");    
  }     
     
  make_generic("exp", G95_ISYM_EXP);     
     
  add_sym("exponent", 1, 1, BT_INTEGER, di, 
	  g95_check_x, g95_simplify_exponent, g95_resolve_exponent,          
	  x, BT_REAL, dr, 0, NULL);     
     
  make_generic("exponent", G95_ISYM_EXPONENT);         
         
  add_sym("floor", 1, 1, BT_INTEGER, di,          
	  g95_check_a_ikind, g95_simplify_floor, g95_resolve_floor,    
	  a, BT_REAL, dr, 0,   knd, BT_INTEGER, di, 1, NULL);      
      
  make_generic("floor", G95_ISYM_FLOOR);

  add_sym("fraction", 1, 1, BT_REAL, dr, 
	  g95_check_x, g95_simplify_fraction, g95_resolve_fraction,    
	  x, BT_REAL, dr, 0, NULL);    
    
  make_generic("fraction", G95_ISYM_FRACTION);         
         
  add_sym("huge", 0, 1, BT_REAL, dr,         
	  g95_check_huge, g95_simplify_huge, NULL,       
	  x, BT_UNKNOWN, dr, 0,  NULL);    
    
  make_generic("huge", G95_ISYM_NONE);    
    
  add_sym("iachar", 1, 1, BT_INTEGER, di,
	  NULL, g95_simplify_iachar, g95_resolve_ichar,          
	  h, BT_CHARACTER, dc, 0, NULL);  
  
  make_generic("iachar", G95_ISYM_IACHAR);        
        
  add_sym("iand", 1, 1, BT_INTEGER, di,       
	  g95_check_iand, g95_simplify_iand, g95_resolve_iand,     
	  i, BT_INTEGER, di, 0,   u, BT_INTEGER, di, 0, NULL); 
 
  make_generic("iand", G95_ISYM_IAND);          
          
  add_sym("ibclr", 1, 1, BT_INTEGER, di,   
	  g95_check_ibclr, g95_simplify_ibclr, g95_resolve_ibclr,
	  i, BT_INTEGER, di, 0,   pos, BT_INTEGER, di, 0, NULL);        
        
  make_generic("ibclr", G95_ISYM_IBCLR);     
     
  add_sym("ibits", 1, 1, BT_INTEGER, di,   
	  g95_check_ibits, g95_simplify_ibits, g95_resolve_ibits,         
	  i, BT_INTEGER, di, 0,   pos, BT_INTEGER, di, 0,         
	  ln, BT_INTEGER, di, 0, NULL);       
       
  make_generic("ibits", G95_ISYM_IBITS);     
     
  add_sym("ibset", 1, 1, BT_INTEGER, di,         
	  g95_check_ibset, g95_simplify_ibset, g95_resolve_ibset,     
	  i, BT_INTEGER, di, 0, pos,   BT_INTEGER, di, 0, NULL);     
     
  make_generic("ibset", G95_ISYM_IBSET);    
    
  add_sym("ichar", 1, 0, BT_INTEGER, di,
	  NULL, g95_simplify_ichar, g95_resolve_ichar,         
	  h, BT_CHARACTER, dc, 0, NULL);          
          
  make_generic("ichar", G95_ISYM_ICHAR);   
   
  add_sym("ieor", 1, 1, BT_INTEGER, di,         
	  g95_check_ieor, g95_simplify_ieor, g95_resolve_ieor, 
	  i, BT_INTEGER, di, 0,   u, BT_INTEGER, di, 0, NULL);  
  
  make_generic("ieor", G95_ISYM_IEOR);  
  
  add_sym("index", 1, 1, BT_INTEGER, di,       
	  g95_check_index, g95_simplify_index, NULL,        
	  stg, BT_CHARACTER, dc, 0,   ssg, BT_CHARACTER, dc, 0,        
	  bck, BT_LOGICAL, dl, 1, NULL);         
         
  make_generic("index", G95_ISYM_INDEX);  
  
  add_sym("int", 1, 1, BT_INTEGER, di,      
	  g95_check_int, g95_simplify_int, g95_resolve_int,     
	  a, BT_REAL, dr, 0,   knd, BT_INTEGER, di, 1, NULL);        
        
  add_sym("ifix", 1, 0, BT_INTEGER, di,       
	  NULL, g95_simplify_ifix, NULL,
	  a, BT_REAL, dr, 0, NULL);    
    
  add_sym("idint", 1, 0, BT_INTEGER, di,    
	  NULL, g95_simplify_idint, NULL, 
	  a, BT_REAL, dd, 0, NULL);          
          
  make_generic("int", G95_ISYM_INT);        
        
  add_sym("ior", 1, 1, BT_INTEGER, di,   
	  g95_check_ior, g95_simplify_ior, g95_resolve_ior,          
	  i, BT_INTEGER, di, 0, u,   BT_INTEGER, di, 0, NULL);   
   
  make_generic("ior", G95_ISYM_IOR);          
          
  add_sym("ishft", 1, 1, BT_INTEGER, di,      
	  g95_check_ishft, g95_simplify_ishft, g95_resolve_ishft,     
	  i, BT_INTEGER, di, 0,   sh, BT_INTEGER, di, 0, NULL);    
    
  make_generic("ishft", G95_ISYM_ISHFT); 
 
  add_sym("ishftc", 1, 1, BT_INTEGER, di,   
	  g95_check_ishftc, g95_simplify_ishftc, g95_resolve_ishftc,        
	  i, BT_INTEGER, di, 0,   sh, BT_INTEGER, di, 0,      
	  sz, BT_INTEGER, di, 1, NULL);      
      
  make_generic("ishftc", G95_ISYM_ISHFTC);     
     
  add_sym("kind", 0, 1, BT_INTEGER, di,    
	  g95_check_kind, g95_simplify_kind, NULL,        
	  x, BT_REAL, dr, 0, NULL);   
   
  make_generic("kind", G95_ISYM_NONE);  
  
  add_sym("lbound", 0, 1, BT_INTEGER, di,       
	  g95_check_lbound, NULL, g95_resolve_lbound,     
	  ar, BT_REAL, dr, 0,   dm, BT_INTEGER, di, 1, NULL);         
         
  make_generic("lbound", G95_ISYM_LBOUND);        
        
  add_sym("len", 0, 1, BT_INTEGER, di,          
	  NULL, g95_simplify_len, g95_resolve_len,  
	  stg, BT_CHARACTER, dc, 0, NULL);    
    
  make_generic("len", G95_ISYM_LEN);  
  
  add_sym("len_trim", 1, 1, BT_INTEGER, di,       
	  NULL, g95_simplify_len_trim, g95_resolve_len_trim,    
	  stg, BT_CHARACTER, dc, 0, NULL);      
      
  make_generic("len_trim", G95_ISYM_LEN_TRIM);         
         
  add_sym("lge", 1, 0, BT_LOGICAL, dl,     
	  NULL, g95_simplify_lge, NULL,        
	  sta, BT_CHARACTER, dc, 0,   stb, BT_CHARACTER, dc, 0, NULL); 
 
  make_generic("lge", G95_ISYM_LGE);         
         
  add_sym("lgt", 1, 0, BT_LOGICAL, dl,         
	  NULL, g95_simplify_lgt, NULL,         
	  sta, BT_CHARACTER, dc, 0,   stb, BT_CHARACTER, dc, 0, NULL);       
       
  make_generic("lgt", G95_ISYM_LGT);  
  
  add_sym("lle", 1, 0, BT_LOGICAL, dl,         
	  NULL, g95_simplify_lle, NULL, 
	  sta, BT_CHARACTER, dc, 0,   stb, BT_CHARACTER, dc, 0, NULL);      
      
  make_generic("lle", G95_ISYM_LLE); 
 
  add_sym("llt", 1, 0, BT_LOGICAL, dl,          
	  NULL, g95_simplify_llt, NULL,
	  sta, BT_CHARACTER, dc, 0,   stb, BT_CHARACTER, dc, 0, NULL);      
      
  make_generic("llt", G95_ISYM_LLT);   
   
  add_sym("log", 1, 1, BT_REAL, dr,          
	  NULL, g95_simplify_log, g95_resolve_log,      
	  x, BT_REAL, dr, 0, NULL);   
   
  add_sym("alog", 1, 1, BT_REAL, dr,       
	  NULL, g95_simplify_log, g95_resolve_log, 
	  x, BT_REAL, dr, 0, NULL);      
      
  add_sym("dlog", 1, 1, BT_REAL, dd, 
	  NULL, g95_simplify_log, g95_resolve_log,      
	  x, BT_REAL, dd, 0, NULL); 
 
  add_sym("clog", 1, 1, BT_COMPLEX, dz,    
	  NULL, g95_simplify_log, g95_resolve_log,          
	  x, BT_COMPLEX, dz, 0, NULL);         
         
  if (g95_option.fmode == 0) {          
    add_sym("zlog", 1, 1, BT_COMPLEX, dd,     
	    NULL, g95_simplify_log, g95_resolve_log,      
	    x, BT_COMPLEX, dd, 0, NULL);

    make_alias("cdlog");         
  }         
         
  make_generic("log", G95_ISYM_LOG);      
      
  add_sym("log10", 1, 1, BT_REAL, dr,        
	  NULL, g95_simplify_log10, g95_resolve_log10,         
	  x, BT_REAL, dr, 0, NULL);

  add_sym("alog10", 1, 1, BT_REAL, dr,  
	  NULL, g95_simplify_log10, g95_resolve_log10,  
	  x, BT_REAL, dr, 0, NULL);          
          
  add_sym("dlog10", 1, 1, BT_REAL, dd,
	  NULL, g95_simplify_log10, g95_resolve_log10, 
	  x, BT_REAL, dd, 0, NULL);     
     
  make_generic("log10", G95_ISYM_LOG10);     
     
  add_sym("logical", 0, 1, BT_LOGICAL, dl,
	  g95_check_logical, g95_simplify_logical, g95_resolve_logical,     
	  o, BT_LOGICAL, dl, 0,   knd, BT_INTEGER, di, 1, NULL);       
       
  make_generic("logical", G95_ISYM_LOGICAL);         
         
  add_sym("matmul", 0, 1, BT_REAL, dr, 
	  g95_check_matmul, NULL, g95_resolve_matmul,         
	  ma, BT_REAL, dr, 0,   mb, BT_REAL, dr, 0, NULL);          
          
  make_generic("matmul", G95_ISYM_MATMUL);       
       
/* Note: amax0 is equivalent to real(max), max1 is equivalent to
 * int(max).  The max function must have at least two arguments. */ 
 
  add_sym("max", 1, 0, BT_UNKNOWN, 0,      
	  g95_check_min_max, g95_simplify_max, g95_resolve_min_max,      
	  t, BT_UNKNOWN, dr, 0,   d, BT_UNKNOWN, dr, 0, NULL);       
       
  add_sym("max0", 1, 0, BT_INTEGER, di,   
	  g95_check_min_max_integer, g95_simplify_max, g95_resolve_min_max,   
	  t, BT_INTEGER, di, 0,   d, BT_INTEGER, di, 0, NULL);   
   
  add_sym("amax0", 1, 0, BT_REAL, dr,        
	  g95_check_min_max_integer, g95_simplify_max, g95_resolve_min_max,       
	  t, BT_INTEGER, di, 0,   d, BT_INTEGER, di, 0, NULL);   
   
  add_sym("amax1", 1, 0, BT_REAL, dr,          
	  g95_check_min_max_real, g95_simplify_max, g95_resolve_min_max,    
	  t, BT_REAL, dr, 0,   d, BT_REAL, dr, 0, NULL);

  add_sym("max1", 1, 0, BT_INTEGER, di,   
	  g95_check_min_max_real, g95_simplify_max, g95_resolve_min_max,         
	  t, BT_REAL, dr, 0,   d, BT_REAL, dr, 0, NULL);      
      
  add_sym("dmax1", 1, 0, BT_REAL, dd,  
	  g95_check_min_max_double, g95_simplify_max, g95_resolve_min_max, 
	  t, BT_REAL, dd, 0,   d, BT_REAL, dd, 0, NULL);          
          
  make_generic("max", G95_ISYM_MAX);

  add_sym("maxexponent", 0, 1, BT_INTEGER, di,        
	  g95_check_x_ni, g95_simplify_maxexponent, NULL,    
	  x, BT_UNKNOWN, dr, 0, NULL);    
    
  make_generic("maxexponent", G95_ISYM_NONE);

  add_sym("maxloc", 0, 1, BT_INTEGER, di,      
	  g95_check_minloc_maxloc, NULL, g95_resolve_maxloc, 
	  ar, BT_REAL, dr, 0,   dm, BT_INTEGER, di, 1, 
	  msk, BT_LOGICAL, dl, 1, NULL);         
         
  make_generic("maxloc", G95_ISYM_MAXLOC);     
     
  add_sym("maxval", 0, 1, BT_REAL, dr,    
	  g95_check_minval_maxval, NULL, g95_resolve_maxval,   
	  ar, BT_REAL, dr, 0,   dm, BT_INTEGER, di, 1,    
	  msk, BT_LOGICAL, dl, 1, NULL);     
     
  make_generic("maxval", G95_ISYM_MAXVAL);     
     
  add_sym("merge", 1, 1, BT_REAL, dr,          
	  g95_check_merge, NULL, g95_resolve_merge,     
	  ts, BT_REAL, dr, 0,	  fs, BT_REAL, dr, 0,     
	  msk, BT_LOGICAL, dl, 0, NULL);        
        
  make_generic("merge", G95_ISYM_MERGE);  
  
/* Note: amin0 is equivalent to real(min), min1 is equivalent to int(min). */   
   
  add_sym("min", 1, 0, BT_UNKNOWN, 0,
	  g95_check_min_max, g95_simplify_min, g95_resolve_min_max,        
	  t, BT_REAL, dr, 0,   d, BT_REAL, dr, 0, NULL);    
    
  add_sym("min0", 1, 0, BT_INTEGER, di,  
	  g95_check_min_max_integer, g95_simplify_min, g95_resolve_min_max,    
	  t, BT_INTEGER, di, 0,   d, BT_INTEGER, di, 0, NULL);     
     
  add_sym("amin0", 1, 0, BT_REAL, dr,         
	  g95_check_min_max_integer, g95_simplify_min, g95_resolve_min_max,        
	  t, BT_INTEGER, di, 0,   d, BT_INTEGER, di, 0, NULL);       
       
  add_sym("amin1", 1, 0, BT_REAL, dr,  
	  g95_check_min_max_real, g95_simplify_min, g95_resolve_min_max,          
	  t, BT_REAL, dr, 0,   d, BT_REAL, dr, 0, NULL);      
      
  add_sym("min1", 1, 0, BT_INTEGER, di,   
	  g95_check_min_max_real, g95_simplify_min, g95_resolve_min_max,     
	  t, BT_REAL, dr, 0,   d, BT_REAL, dr, 0, NULL);  
  
  add_sym("dmin1", 1, 0, BT_REAL, dd,         
	  g95_check_min_max_double, g95_simplify_min, g95_resolve_min_max,
	  t, BT_REAL, dd, 0,   d, BT_REAL, dd, 0, NULL);     
     
  make_generic("min", G95_ISYM_MIN);     
     
  add_sym("minexponent", 0, 1, BT_INTEGER, di,    
	  g95_check_x_ni, g95_simplify_minexponent, NULL,
	  x, BT_UNKNOWN, dr, 0, NULL);  
  
  make_generic("minexponent", G95_ISYM_NONE);       
       
  add_sym("minloc", 0, 1, BT_INTEGER, di,      
	  g95_check_minloc_maxloc, NULL, g95_resolve_minloc, 
	  ar, BT_REAL, dr, 0,   dm, BT_INTEGER, di, 1,          
	  msk, BT_LOGICAL, dl, 1, NULL);   
   
  make_generic("minloc", G95_ISYM_MINLOC);  
  
  add_sym("minval", 0, 1, BT_REAL, dr,         
	  g95_check_minval_maxval, NULL, g95_resolve_minval, 
	  ar, BT_REAL, dr, 0,   dm, BT_INTEGER, di, 1,   
	  msk, BT_LOGICAL, dl, 1, NULL);    
    
  make_generic("minval", G95_ISYM_MINVAL);   
   
  add_sym("mod", 1, 1, BT_INTEGER, di,          
	  g95_check_a_p, g95_simplify_mod, g95_resolve_mod,    
	  a, BT_INTEGER, di, 0,   p, BT_INTEGER, di, 0, NULL);  
  
  add_sym("amod", 1, 1, BT_REAL, dr,  
	  NULL, g95_simplify_mod, g95_resolve_mod,          
	  a, BT_REAL, dr, 0,   p, BT_REAL, dr, 0, NULL);

  add_sym("dmod", 1, 1, BT_REAL, dd,
	  NULL, g95_simplify_mod, g95_resolve_mod,
	  a, BT_REAL, dd, 0,   p, BT_REAL, dd, 0, NULL);    
    
  make_generic("mod", G95_ISYM_MOD);     
     
  add_sym("modulo", 1, 1, BT_REAL, di,  
	  g95_check_a_p, g95_simplify_modulo, g95_resolve_modulo, 
	  a, BT_REAL, di, 0,   p, BT_REAL, di, 0, NULL);   
   
  make_generic("modulo", G95_ISYM_MODULO);   
   
  add_sym("nearest", 1, 1, BT_REAL, dr,        
	  g95_check_nearest, g95_simplify_nearest, g95_resolve_nearest,   
	  x, BT_REAL, dr, 0,   s, BT_REAL, dr, 0, NULL);         
         
  make_generic("nearest", G95_ISYM_NEAREST); 
 
  add_sym("nint", 1, 1, BT_INTEGER, di,  
	  g95_check_a_ikind, g95_simplify_nint, g95_resolve_nint,       
	  a, BT_REAL, dr, 0,   knd, BT_INTEGER, di, 1, NULL);    
    
  add_sym("idnint", 1, 1, BT_INTEGER, di,  
	  g95_check_idnint, g95_simplify_idnint, g95_resolve_idnint,      
	  a, BT_REAL, dd, 0, NULL);       
       
  make_generic("nint", G95_ISYM_NINT);    
    
  add_sym("not", 1, 1, BT_INTEGER, di, 
	  g95_check_i, g95_simplify_not, g95_resolve_not, 
	  i, BT_INTEGER, di, 0, NULL);        
        
  make_generic("not", G95_ISYM_NOT);

  add_sym("null", 0, 1, BT_INTEGER, di,         
	  g95_check_null, g95_simplify_null, NULL,       
	  mo, BT_INTEGER, di, 1, NULL);

  make_generic("null", G95_ISYM_NONE);          
          
  add_sym("pack", 0, 1, BT_REAL, dr,          
	  g95_check_pack, NULL, g95_resolve_pack,        
	  ar, BT_REAL, dr, 0,   msk, BT_LOGICAL, dl, 0,  
	  v, BT_REAL, dr, 1, NULL);

  make_generic("pack", G95_ISYM_PACK);      
      
  add_sym("precision", 0, 1, BT_INTEGER, di,      
	  g95_check_precision, g95_simplify_precision, NULL,  
	  x, BT_UNKNOWN, 0, 0, NULL);     
     
  make_generic("precision", G95_ISYM_NONE);

  add_sym("present", 0, 1, BT_LOGICAL, dl,    
	  g95_check_present, NULL, NULL,        
	  a, BT_REAL, dr, 0, NULL);  
  
  make_generic("present", G95_ISYM_PRESENT);      
      
  add_sym("product", 0, 1, BT_REAL, dr,      
	  g95_check_product, NULL, g95_resolve_product,   
	  ar, BT_REAL, dr, 0,   dm, BT_INTEGER, di, 1,      
	  msk, BT_LOGICAL, dl, 1, NULL); 
 
  make_generic("product", G95_ISYM_PRODUCT);   
   
  add_sym("radix", 0, 1, BT_INTEGER, di,  
	  g95_check_radix, g95_simplify_radix, NULL,   
	  x, BT_UNKNOWN, 0, 0, NULL);

  make_generic("radix", G95_ISYM_NONE);        
        
  add_sym("range", 0, 1, BT_INTEGER, di,         
	  g95_check_range, g95_simplify_range, NULL,   
	  x, BT_REAL, dr, 0, NULL);       
       
  make_generic("range", G95_ISYM_NONE);      
      
  add_sym("real", 1, 0, BT_REAL, dr,          
	  g95_check_real, g95_simplify_real, g95_resolve_real,       
	  a, BT_UNKNOWN, dr, 0,   knd, BT_INTEGER, di, 1, NULL);      
      
  add_sym("float", 1, 0, BT_REAL, dr,   
	  NULL, g95_simplify_float, NULL,      
	  a, BT_INTEGER, di, 0, NULL);         
         
  add_sym("sngl", 1, 0, BT_REAL, dr,
	  NULL, g95_simplify_sngl, NULL,          
	  a, BT_REAL, dd, 0, NULL);  
  
  make_generic("real", G95_ISYM_REAL);      
      
  add_sym("repeat", 0, 1, BT_CHARACTER, dc,    
	  g95_check_repeat, g95_simplify_repeat, g95_resolve_repeat,        
	  stg, BT_CHARACTER, dc, 0,   n, BT_INTEGER, di, 0, NULL);         
         
  make_generic("repeat", G95_ISYM_REPEAT);       
       
  add_sym("reshape", 0, 1, BT_REAL, dr, 
	  g95_check_reshape, g95_simplify_reshape, g95_resolve_reshape,     
	  source, BT_REAL, dr, 0,   shp, BT_INTEGER, di, 0,        
	  pad, BT_REAL, dr, 1,   ord, BT_INTEGER, di, 1, NULL);   
   
  make_generic("reshape", G95_ISYM_RESHAPE);     
     
  add_sym("rrspacing", 1, 1, BT_REAL, dr, 
	  g95_check_x, g95_simplify_rrspacing, g95_resolve_rrspacing,      
	  x, BT_REAL, dr, 0, NULL);        
        
  make_generic("rrspacing", G95_ISYM_NONE);         
         
  add_sym("scale", 1, 1, BT_REAL, dr,     
	  g95_check_scale, g95_simplify_scale, g95_resolve_scale,        
	  x, BT_REAL, dr, 0,   i, BT_INTEGER, di, 0, NULL);

  make_generic("scale", G95_ISYM_NONE);    
    
  add_sym("scan", 1, 1, BT_INTEGER, di,     
	  g95_check_scan, g95_simplify_scan, g95_resolve_scan,       
	  stg, BT_CHARACTER, dc, 0,  set, BT_CHARACTER, dc, 0,     
	  bck, BT_LOGICAL, dl, 1, NULL);     
     
  make_generic("scan", G95_ISYM_SCAN);

  add_sym("selected_int_kind", 0, 1, BT_INTEGER, di,        
	  NULL, g95_simplify_selected_int_kind, NULL,          
	  k, BT_INTEGER, di, 0, NULL);     
     
  make_generic("selected_int_kind", G95_ISYM_SELECTED_INT_KIND);    
    
  add_sym("selected_real_kind", 0, 1, BT_INTEGER, di,      
	  g95_check_selected_real_kind, g95_simplify_selected_real_kind, NULL,        
	  p, BT_INTEGER, di, 1,   k, BT_INTEGER, di, 1, NULL);

  make_generic("selected_real_kind", G95_ISYM_SELECTED_REAL_KIND);  
  
  add_sym("set_exponent", 1, 1, BT_REAL, dr,          
	  g95_check_set_exponent, g95_simplify_set_exponent, 
	  g95_resolve_set_exponent,          
	  x, BT_REAL, dr, 0,   i, BT_INTEGER, di, 0, NULL);    
    
  make_generic("set_exponent", G95_ISYM_SET_EXPONENT);         
         
  add_sym("shape", 0, 1, BT_INTEGER, di,       
	  g95_check_shape, g95_simplify_shape, g95_resolve_shape,        
	  source, BT_REAL, dr, 0, NULL); 
 
  make_generic("shape", G95_ISYM_SHAPE);  
  
  add_sym("sign", 1, 1, BT_REAL, dr,        
	  g95_check_sign, g95_simplify_sign, g95_resolve_sign,       
	  a, BT_REAL, dr, 0,   q, BT_REAL, dr, 0, NULL);  
  
  add_sym("isign", 1, 1, BT_INTEGER, di, 
	  NULL, g95_simplify_sign, g95_resolve_sign,     
	  a, BT_INTEGER, di, 0,   q, BT_INTEGER, di, 0, NULL);          
          
  add_sym("dsign", 1, 1, BT_REAL, dd,         
	  NULL, g95_simplify_sign, g95_resolve_sign,
	  a, BT_REAL, dd, 0,   q, BT_REAL, dd, 0, NULL); 
 
  make_generic("sign", G95_ISYM_SIGN);     
     
  add_sym("sin", 1, 1, BT_REAL, dr, 
	  NULL, g95_simplify_sin, g95_resolve_sin,  
	  x, BT_REAL, dr, 0, NULL);      
      
  add_sym("dsin", 1, 1, BT_REAL, dd,     
	  NULL, g95_simplify_sin, g95_resolve_sin,
	  x, BT_REAL, dd, 0, NULL);     
     
  add_sym("csin", 1, 1, BT_COMPLEX, dz,          
	  NULL, g95_simplify_sin, g95_resolve_sin,  
	  x, BT_COMPLEX, dz, 0, NULL);   
   
  if (g95_option.fmode == 0) {  
    add_sym("zsin", 1, 1, BT_COMPLEX, dd,      
	    NULL, g95_simplify_sin, g95_resolve_sin,    
	    x, BT_COMPLEX, dd, 0, NULL); 
 
    make_alias("cdsin");        
  }  
  
  make_generic("sin", G95_ISYM_SIN);        
        
  add_sym("sinh", 1, 1, BT_REAL, dr,   
	  NULL, g95_simplify_sinh, g95_resolve_sinh,      
	  x, BT_REAL, dr, 0, NULL);      
      
  add_sym("dsinh", 1, 1, BT_REAL, dd,         
	  NULL, g95_simplify_sinh, g95_resolve_sinh,          
	  x, BT_REAL, dd, 0, NULL);     
     
  make_generic("sinh", G95_ISYM_SINH);  
  
  add_sym("size", 0, 1, BT_INTEGER, di,  
	  g95_check_size, g95_simplify_size, NULL,
	  ar, BT_REAL, dr, 0,   dm, BT_INTEGER, di, 1, NULL);        
        
  make_generic("size", G95_ISYM_SIZE);         
         
  add_sym("spacing", 1, 1, BT_REAL, dr,        
	  g95_check_x, g95_simplify_spacing, g95_resolve_spacing,     
	  x, BT_REAL, dr, 0, NULL);    
    
  make_generic("spacing", G95_ISYM_NONE);          
          
  add_sym("spread", 0, 1, BT_REAL, dr,    
	  g95_check_spread, NULL, g95_resolve_spread, 
	  source, BT_REAL, dr, 0,   dm, BT_INTEGER, di, 0,
	  n, BT_INTEGER, di, 0, NULL);          
          
  make_generic("spread", G95_ISYM_SPREAD);       
       
  add_sym("sqrt", 1, 1, BT_REAL, dr,        
	  NULL, g95_simplify_sqrt, g95_resolve_sqrt,          
	  x, BT_REAL, dr, 0, NULL); 
 
  add_sym("dsqrt", 1, 1, BT_REAL, dd,   
	  NULL, g95_simplify_sqrt, g95_resolve_sqrt,  
	  x, BT_REAL, dd, 0, NULL);

  add_sym("csqrt", 1, 1, BT_COMPLEX, dz,  
	  NULL, g95_simplify_sqrt, g95_resolve_sqrt,      
	  x, BT_COMPLEX, dz, 0, NULL);

  if (g95_option.fmode == 0) {     
    add_sym("zsqrt", 1, 1, BT_COMPLEX, dd,          
	    NULL, g95_simplify_sqrt, g95_resolve_sqrt, 
	    x, BT_COMPLEX, dd, 0, NULL);  
  
    make_alias("cdsqrt");     
  }        
        
  make_generic("sqrt", G95_ISYM_SQRT);  
  
  add_sym("sum", 0, 1, BT_UNKNOWN, 0,      
	  g95_check_sum, NULL, g95_resolve_sum,
	  ar, BT_REAL, dr, 0,   dm, BT_INTEGER, di, 1,        
	  msk, BT_LOGICAL, dl, 1, NULL);     
     
  make_generic("sum", G95_ISYM_SUM);         
         
  add_sym("tan", 1, 1, BT_REAL, dr,          
	  NULL, g95_simplify_tan, g95_resolve_tan,      
	  x, BT_REAL, dr, 0, NULL);      
      
  add_sym("dtan", 1, 1, BT_REAL, dd,        
	  NULL, g95_simplify_tan, g95_resolve_tan,       
	  x, BT_REAL, dd, 0, NULL);     
     
  make_generic("tan", G95_ISYM_TAN);   
   
  add_sym("tanh", 1, 1, BT_REAL, dr,       
	  NULL, g95_simplify_tanh, g95_resolve_tanh,        
	  x, BT_REAL, dr, 0, NULL);     
     
  add_sym("dtanh", 1, 1, BT_REAL, dd,         
	  NULL, g95_simplify_tanh, g95_resolve_tanh,
	  x, BT_REAL, dd, 0, NULL);    
    
  make_generic("tanh", G95_ISYM_TANH);        
        
  add_sym("tiny", 0, 1, BT_REAL, dr,         
	  g95_check_x_ni, g95_simplify_tiny, NULL,         
	  x, BT_REAL, dr, 0, NULL);

  make_generic("tiny", G95_ISYM_NONE); 
 
  add_sym("transfer", 0, 1, BT_REAL, dr,  
	  g95_check_transfer, NULL, g95_resolve_transfer,
	  source, BT_REAL, dr, 0,    mo, BT_REAL, dr, 0,     
	  sz, BT_INTEGER, di, 1,  NULL);     
     
  make_generic("transfer", G95_ISYM_TRANSFER);          
          
  add_sym("transpose", 0, 1, BT_REAL, dr,    
	  g95_check_transpose, NULL, g95_resolve_transpose,         
	  m, BT_REAL, dr, 0, NULL);       
       
  make_generic("transpose", G95_ISYM_TRANSPOSE);      
      
  add_sym("trim", 0, 1, BT_CHARACTER, dc,        
	  NULL, g95_simplify_trim, g95_resolve_trim,      
	  stg, BT_CHARACTER, dc, 0, NULL);  
  
  make_generic("trim", G95_ISYM_TRIM);   
   
  add_sym("ubound", 0, 1, BT_INTEGER, di,    
	  g95_check_ubound, NULL, g95_resolve_ubound,        
	  ar, BT_REAL, dr, 0,   dm, BT_INTEGER, di, 1, NULL);     
     
  make_generic("ubound", G95_ISYM_UBOUND);      
      
  add_sym("unpack", 0, 1, BT_REAL, dr,       
	  g95_check_unpack, NULL, g95_resolve_unpack,    
	  v, BT_REAL, dr, 0,   msk, BT_LOGICAL, dl, 0, 
	  g, BT_REAL, dr, 0, NULL);       
       
  make_generic("unpack", G95_ISYM_UNPACK);    
    
  add_sym("verify", 1, 1, BT_INTEGER, di, 
	  g95_check_verify, g95_simplify_verify, g95_resolve_verify,
	  stg, BT_CHARACTER, dc, 0,   set, BT_CHARACTER, dc, 0,       
	  bck, BT_LOGICAL, dl, 1, NULL);          
          
  make_generic("verify", G95_ISYM_VERIFY);    
}         
         
         
  
  
/* g95_intrinsic_symbol()-- Given a symbol that we have decided is
 * intrinsic, mark it as such by placing it into a special module that
 * is otherwise impossible to read or write.  Returns nonzero if
 * something goes wrong. */    
    
int g95_intrinsic_symbol(g95_symbol *sym, int function) {         
         
  if (sym->attr.generic) return 0;        
  strcpy(sym->module, "(intrinsic)");          
          
  if (sym->attr.proc != PROC_INTRINSIC &&   
      (g95_add_procedure(&sym->attr, PROC_INTRINSIC, NULL) == FAILURE))  
      return 1;         
         
  if (function) { 
    if (!sym->attr.function && g95_add_function(&sym->attr, NULL) == FAILURE)    
      return 1;      
      
    if (sym->result == NULL) sym->result = sym;   
  } else {      
    if (!sym->attr.subroutine &&        
	g95_add_subroutine(&sym->attr, NULL) == FAILURE) 
      return 1;          
  }         
         
  return 0;        
}        
        
        


/* add_conv()-- Add a function to the list of conversion symbols */

static void add_conv(bt from_type, int from_kind, bt to_type, int to_kind,       
		     g95_expr *(*simplify)()) {         
         
g95_typespec frm, end;
g95_intrinsic_sym *s;  
  
  if (sizing == SZ_CONVS) {
    nconv++;    
    return;       
  }       
       
  g95_clear_ts(&frm);         
  frm.type = from_type;          
  frm.kind = from_kind;

  g95_clear_ts(&end); 
  end.type = to_type; 
  end.kind = to_kind; 
 
  s = conversion + nconv;       
       
  strcpy(s->name, conv_name(&frm, &end));  
  strcpy(s->lib_name, s->name);        
  s->simplify = simplify; 
  s->elemental = 1;         
  s->ts = end;         
  s->generic_id = G95_ISYM_CONVERSION;

  nconv++;       
}       
       
       
   
   
/* examine_arglist()-- Examine the argument list.  Returns 0 if all
 * constructor arguments have no elements left, 1 if all constructor
 * arguments have at least one element left, and 2 otherwise (which is
 * an error). */    
    
static int examine_arglist(g95_actual_arglist *r) {     
int e, retval, seen_cons;       
       
  seen_cons = 0; 
  retval = 2;

  for(; r; r=r->next) {     
    if (r->u.expr == NULL || r->u.expr->type != EXPR_ARRAY) continue;        
        
    e = (r->u.expr->value.constructor == NULL) ? 0 : 1;          
          
    if (!seen_cons) {
      seen_cons = 1; 
      retval = e;  
    } else {    
      if (retval != e) {          
	g95_error("Arguments of elemental intrinisc at %L have differing "     
		  "numbers of elements", &r->u.expr->where);
	return 2;       
      }         
    }      
  }  
  
  return retval;    
}       
       
       
       
       
/* add_conversions()-- Create g95_intrinsic_sym nodes for all intrinsic
 * conversion functions by looping over the kind tables. */  
  
static void add_conversions(void) {    
int l, k; 
 
  /* Integer-Integer conversions */  
  
  for(l=0; g95_integer_kinds[l].kind != 0; l++)   
    for(k=0; g95_integer_kinds[k].kind != 0; k++) { 
      if (l == k) continue;

      add_conv(BT_INTEGER, g95_integer_kinds[l].kind, 
	       BT_INTEGER, g95_integer_kinds[k].kind, convert_constant);       
    }

  /* Integer-Real/Complex conversions */         
         
  for(l=0; g95_integer_kinds[l].kind != 0; l++)    
    for(k=0; g95_real_kinds[k].kind != 0; k++) {   
      add_conv(BT_INTEGER, g95_integer_kinds[l].kind,  
	       BT_REAL,    g95_real_kinds[k].kind, convert_constant);          
          
      add_conv(BT_REAL,    g95_real_kinds[k].kind,      
	       BT_INTEGER, g95_integer_kinds[l].kind, convert_constant);

      add_conv(BT_INTEGER, g95_integer_kinds[l].kind,
	       BT_COMPLEX, g95_real_kinds[k].kind, convert_constant);      
      
      add_conv(BT_COMPLEX, g95_real_kinds[k].kind,    
	       BT_INTEGER, g95_integer_kinds[l].kind, convert_constant);          
    }     
     
  /* Real/Complex - Real/Complex conversions */          
          
  for(l=0; g95_real_kinds[l].kind != 0; l++)        
    for(k=0; g95_real_kinds[k].kind != 0; k++) {     
      if (l != k) {   
	add_conv(BT_REAL, g95_real_kinds[l].kind,     
		 BT_REAL, g95_real_kinds[k].kind, convert_constant);       
       
	add_conv(BT_COMPLEX, g95_real_kinds[l].kind,          
		 BT_COMPLEX, g95_real_kinds[k].kind, convert_constant);  
      } 
 
      add_conv(BT_REAL,    g95_real_kinds[l].kind,
	       BT_COMPLEX, g95_real_kinds[k].kind, convert_constant);  
  
      add_conv(BT_COMPLEX, g95_real_kinds[l].kind,   
	       BT_REAL,    g95_real_kinds[k].kind, convert_constant);        
    }    
    
  /* Logical/Logical kind conversion */     
     
  for(l=0; g95_logical_kinds[l].kind; l++)  
    for(k=0; g95_logical_kinds[k].kind; k++) {      
      if (l == k) continue;      
      
      add_conv(BT_LOGICAL, g95_logical_kinds[l].kind,   
	       BT_LOGICAL, g95_logical_kinds[k].kind, convert_constant);    
    } 
}    
    
    
       
       
/* g95_intrinsic_init_1()-- Initialize the table of intrinsics */       
       
void g95_intrinsic_init_1(void) {          
int p;  
  
  nargs = nfunc = nsub = nconv = 0;       
       
  sizing = SZ_FUNCS;   
  add_functions();   
  sizing = SZ_SUBS;        
  add_subroutines();        
  sizing = SZ_CONVS;   
  add_conversions();        
        
  functions = g95_getmem(sizeof(g95_intrinsic_sym)*(nfunc+nsub)  
			 + sizeof(g95_intrinsic_arg)*nargs);    
    
  next_sym = functions;    
  subroutines = functions + nfunc;   
   
  conversion = g95_getmem(sizeof(g95_intrinsic_sym)*nconv);

  next_arg = ((g95_intrinsic_arg *) (subroutines + nsub)) - 1;    
    
  sizing = SZ_NOTHING; 
  nconv = 0;

  add_functions();       
  add_subroutines();  
  add_conversions(); 
 
  /* Set the pure flag.  All intrinsic functions are pure, and
   * intrinsic subroutines are pure if they are elemental. */   
   
  for(p=0; p<nfunc; p++)    
    functions[p].pure = 1;    
    
  for(p=0; p<nsub; p++)
    subroutines[p].pure = subroutines[p].elemental;       
}         
         
         

/******** Subroutines to check intrinsic interfaces ***********/    
    
/* remove_nullargs()-- Given an actual argument list, remove any
 * NULL arguments that may have been left behind by a sort against
 * some formal argument list. */         
         
static void remove_nullargs(g95_actual_arglist **actual) {         
g95_actual_arglist *h, *t, *n;  
  
  t = NULL; 
 
  for(h=*actual; h; h=n) {
    n = h->next; 
 
    if (h->type == EXPR && h->u.expr == NULL) {
      h->next = NULL;     
      g95_free_actual_arglist(h);        
    } else {       
      if (t == NULL)   
	*actual = h;      
      else  
	t->next = h;  
  
      t = h;     
      t->next = NULL;          
    }        
  }        
        
  if (t == NULL) *actual = NULL;    
}        
        
        
     
     
/* simplify_elemental_intrinsic()-- Simplify an elemental intrinsic
 * that has an array constructor(s) as actual arguments.  Constructors
 * are expanded, and a new constructor is built the elements of which
 * are the function calls.  Returns nonzero if the caller does not
 * have anything more to do. */  
  
static int simplify_elemental_intrinsic(g95_intrinsic_sym *symbol, g95_expr *h,        
					try *status) {
g95_actual_arglist *argum, *d, *tail_a;   
g95_constructor *c, *start, *t;  
g95_expr *x, *q;          
int dim;          
          
  if (!symbol->elemental) return 0;          
          
  dim = 0; 
 
  for(argum=h->value.function.actual; argum; argum=argum->next) {      
    x = argum->u.expr;          
    if (x == NULL) continue;          
          
    if (x->rank > 0 && x->type != EXPR_ARRAY) return 0;        
        
    if (x->type == EXPR_ARRAY) dim = x->rank;   
  }  
  
  if (dim == 0) return 0;    
    
  /* Expand the individual constructors */         
         
  for(argum=h->value.function.actual; argum; argum=argum->next) {   
    x = argum->u.expr;     
    if (x == NULL) continue;  
  
    if (x->type == EXPR_ARRAY &&   
	g95_expand_constructor(x) == FAILURE) return 0;     
  }         
         
  /* Build a new constructor */         
         
  switch(examine_arglist(h->value.function.actual)) {        
  case 0:   
    start = NULL;       
    goto done;         
         
  case 2:   
    *status = FAILURE;   
    return 1;          
          
  case 1:    
    break;     
  }     
     
  start = t = NULL;  
  
  for(;;) {      
    q = g95_get_expr();         
    q->type = EXPR_FUNCTION;  
    q->where = h->where;  
  
    q->value.function.isym = symbol;  
    q->value.function.name = symbol->name;     
    q->ts = symbol->ts;         
         
    tail_a = NULL;       
       
    for(argum=h->value.function.actual; argum; argum=argum->next) {          
      d = g95_get_actual_arglist();          
          
      if (q->value.function.actual == NULL)   
	q->value.function.actual = d;        
      else    
	tail_a->next = d;         
         
      tail_a = d;     
     
      if (argum->u.expr->type != EXPR_ARRAY) 
	d->u.expr = g95_copy_expr(argum->u.expr);          
      else {
	c = argum->u.expr->value.constructor;
	d->u.expr = c->expr;
	argum->u.expr->value.constructor = c->next;

	g95_free(c);
      }         
    }  
  
    if (start == NULL) {    
      start = t = g95_get_constructor(); 
    } else {      
      t->next = g95_get_constructor();          
      t = t->next;      
    }         
         
    t->expr = q;        
        
    switch(examine_arglist(h->value.function.actual)) {          
    case 0:     
      goto done; 
 
    case 1: 
      break;  
  
    case 2:  
      *status = FAILURE;  
      return 1;   
    }        
  }      
          
done:        
  q = g95_get_expr();
  q->type = EXPR_ARRAY;     
  q->ts = symbol->ts;          
  q->where = h->where;
  q->value.constructor = start; 
  q->rank = dim;     
     
  *status = SUCCESS;   
   
  while(start != NULL) { 
    if (do_simplify(symbol, start->expr) == FAILURE) {
      *status = FAILURE;
      break;         
    }         
         
    start = start->next; 
  }     
     
  g95_replace_expr(h, q);
  return 1;  
}          
          
          
 
 
/* do_simplify()-- Given an intrinsic symbol node and an expression
 * node, call the simplification function (if there is one), perhaps
 * replacing the expression with something simpler.  We return FAILURE
 * on an error of the simplification, SUCCESS if the simplification
 * worked, even if nothing has changed in the expression itself */

static try do_simplify(g95_intrinsic_sym *specific, g95_expr *x) {     
g95_expr *res, *d, *y, *k, *c, *h;          
g95_actual_arglist *a;         
try i; 
 
  if (simplify_elemental_intrinsic(specific, x, &i)) return i; 
 
/* Max and min require special handling due to the variable number of args */ 
 
  if (specific->simplify == g95_simplify_min) {   
    res = g95_simplify_min(x); 
    goto finish;
  }

  if (specific->simplify == g95_simplify_max) {    
    res = g95_simplify_max(x); 
    goto finish;         
  }      
      
  if (specific->simplify == NULL) {      
    res = NULL;  
    goto finish;   
  }          
          
  a = x->value.function.actual;          
          
  d = a->u.expr;    
  a = a->next;    
    
  if (specific->simplify == convert_constant) { 
    res = convert_constant(d, specific->ts.type, specific->ts.kind);       
    goto finish;      
  }        
        
  if (a == NULL)
    res = (*specific->simplify)(d);   
  else {    
    y = a->u.expr;       
    a = a->next;          
          
    if (a == NULL)   
      res = (*specific->simplify)(d, y);     
    else { 
      k = a->u.expr;          
      a = a->next;  
        
      if (a == NULL)       
	res = (*specific->simplify)(d, y, k);          
      else {    
	c = a->u.expr;       
	a = a->next;    
    
	if (a == NULL)  
	  res = (*specific->simplify)(d, y, k, c);        
  	else { 
	  h = a->u.expr;          
	  a = a->next;

	  if (a == NULL)
	    res = (*specific->simplify)(d, y, k, c, h);         
	  else     
	    g95_internal_error("do_simplify(): Too many args for intrinsic");
	}        
      }  
    }          
  }      
      
 finish:  
  if (res == &g95_bad_expr) return FAILURE;

  if (res == NULL)
    resolve_intrinsic(specific, x);         /* Must call at run-time */       
  else {  
    res->where = x->where;          
    g95_replace_expr(x, res); 
  }         
         
  return SUCCESS;       
}


 
 
/* init_arglist()-- Initialize the g95_current_intrinsic_arg[] array
 * for the benefit of error messages.  This subroutine returns FAILURE
 * if a subroutine has more than MAX_INTRINSIC_ARGS, in which case the
 * actual argument list cannot match any intrinsic. */         
         
static void init_arglist(g95_intrinsic_sym *is) {   
g95_intrinsic_arg *frm;   
int f;        
        
  g95_current_intrinsic = is->name;     
     
  f = 0;           
  for(frm=is->formal; frm; frm=frm->next) {        
    if (f >= MAX_INTRINSIC_ARGS) 
      g95_internal_error("init_arglist(): too many arguments");    
    g95_current_intrinsic_arg[f++] = frm->name;   
  } 
}         
         
         
 
 
/* sort_actual()-- Given an actual arglist and a formal arglist, sort
 * the actual arglist so that its arguments are in a one-to-one
 * correspondence with the format arglist.  Arguments that are not
 * present are given a blank g95_actual_arglist structure.  If
 * something is obviously wrong (say, a missing required argument) we
 * abort sorting and return FAILURE. */  
  
static try sort_actual(char *name, g95_actual_arglist **ap,          
		       g95_intrinsic_arg *form, g95_locus *where) {    
    
g95_actual_arglist *act, *s;        
g95_intrinsic_arg *x;       
int v;         
         
  remove_nullargs(ap);
  act = *ap; 
 
  for(x=form; x; x=x->next)     
    x->actual = NULL;      
      
  x = form;
  s = act;         
         
  if ( x == NULL && s == NULL ) /* No arguments */      
    return SUCCESS; 
 
  for(;;) {     /* Put the nonkeyword arguments in a 1:1 correspondence */        
    if (x == NULL) break;          
    if (s == NULL) goto optional;    
    
    if (s->name[0] != '\0') goto keywords;       
       
    x->actual = s;  
  
    x = x->next;          
    s = s->next;         
  }  
  
  if (s == NULL) goto do_sort;  
  
  g95_error("Too many arguments in call to '%s' at %L", name, where);
  return FAILURE;  
  
/* Associate the remaining actual arguments, all of which have to be
 * keyword arguments. */  
  
keywords:    
  for(; s; s=s->next) {    
    for(x=form, v=0; x; x=x->next, v++)         
      if (strcmp(s->name, x->name) == 0) break;

    if (x == NULL) {        
      g95_error("Can't find keyword named '%s' in call to '%s' at %L", 
		s->name, name, where); 
      return FAILURE;          
    }    
    
    if (x->actual != NULL) {    
      g95_error("Argument '%s' is appears twice in call to '%s' at %L",   
		x->name, name, where);       
      return FAILURE;      
    }          
          
    x->actual = s;  
  }      
      
/* At this point, all unmatched formal args must be optional */  
  
optional:          
  for(x=form; x; x=x->next) {         
    if (x->actual == NULL && x->optional == 0) {  
      g95_error("Missing actual argument '%s' in call to '%s' at %L",
		x->name, name, where);     
      return FAILURE;     
    }         
  }    
    
/* Using the formal argument list, string the actual argument list
 * together in a way that corresponds with the formal list. */  
  
do_sort:        
  act = NULL;

  for(x=form; x; x=x->next) {
    s = (x->actual == NULL) ? g95_get_actual_arglist() : x->actual; 
 
    if (act == NULL)
      *ap = s;        
    else          
      act->next = s;       
       
    act = s;      
  }    
    
  act->next = NULL;  /* End the sorted argument list. */        
        
  for(s=act, x=form; s; s=s->next, x=x->next) 
    if (s->type != ALT_RETURN && s->u.expr == NULL)  
      s->missing_arg_type = x->ts.type;     
           
  return SUCCESS;     
}          
          
          
   
   
/* g95_convert_type()-- Tries to convert an expression (in place) from
 * one type to another.  The eflag control the behavior on error.
 * The possible values are:
 *   1    Generate a g95_error()
 *   2    Generate a g95_internal_error() */      
      
try g95_convert_type(g95_expr *exp, g95_typespec *ts, int eflag) {       
g95_intrinsic_sym *symb;         
g95_typespec from_ts;         
g95_locus old_where;         
g95_expr *n1;     
int rank;

  from_ts = exp->ts;        /* expr->ts gets clobbered */   
   
  if (ts->type == BT_UNKNOWN) goto bad;         
         
  if (exp->type == EXPR_NULL ||       
      g95_zero_size_array(exp)) { /* Sometimes the RHS acquires the type */        
    exp->ts = *ts;  
    return SUCCESS;   
  }         
         
  if (exp->ts.type == BT_UNKNOWN) goto bad;

  if (g95_compare_types(&exp->ts, ts)) return SUCCESS;    
    
  symb = find_conv(&exp->ts, ts);          
  if (symb == NULL) goto bad;    
    
/* Insert a pre-resolved function call to the right function */       
       
  old_where = exp->where;         
  rank = exp->rank; 
  n1 = g95_get_expr();          
  *n1 = *exp;       
       
  n1 = g95_build_funcall(NULL, n1, NULL);  
  n1->value.function.name = symb->lib_name;     
  n1->value.function.isym = symb;        
  n1->where = old_where;    
  n1->rank = rank;

  *exp = *n1;

  g95_free(n1);   
  exp->ts = *ts;     
     
  if (g95_is_constant_expr(exp->value.function.actual->u.expr) &&        
      do_simplify(symb, exp) == FAILURE) {     
     
    if (eflag == 2) goto bad;     
    return FAILURE;   /* Error already generated in do_simplify() */        
  }     
     
  return SUCCESS;          
        
bad:        
  if (eflag == 1) {       
    g95_error("Can't convert %s to %s at %L",      
	      g95_typename(&from_ts), g95_typename(ts), &exp->where);          
    return FAILURE; 
  }       
       
  g95_internal_error("Can't convert %s to %s at %L",     
		     g95_typename(&from_ts), g95_typename(ts), &exp->where);          
          
  return FAILURE;        
}   
       
       
/* g95_intrinsic_sub_interface()-- see if a CALL statement corresponds
 * to an intrinsic subroutine.  Returns MATCH_YES if the subroutine
 * corresponds to an intrinsic, MATCH_NO if not, and MATCH_ERROR if
 * there was an error (but did correspond). */

match g95_intrinsic_sub_interface(g95_code *r, int error_flag) {        
g95_intrinsic_sym *isym; 
char *nam;         
         
  if (g95_has_alt_return(r->ext.actual)) return MATCH_NO;           
          
  nam = r->sym->name;   
   
  isym = find_subroutine(nam);   
  if (isym == NULL) return MATCH_NO;      
      
  g95_suppress_error = !error_flag;  
  
  init_arglist(isym);     
     
  if (sort_actual(nam, &r->ext.actual, isym->formal, &r->where) == FAILURE)   
    goto fail;        
        
  if (isym->check != NULL) {    
    if (do_check(isym, r->ext.actual) == FAILURE) goto fail;          
  } else {      
    if (check_arglist(&r->ext.actual, isym, 1) == FAILURE) goto fail;        
  }        
        
  /* The subroutine corresponds to an intrinsic.  Allow errors to be
   * seen at this point. */  
  
  g95_suppress_error = 0;          
          
  r->isym = isym;        
  if (isym->resolve != NULL)    
    isym->resolve(r);     
  else          
    r->sub_name = isym->lib_name;

  if (g95_pure(NULL) && !isym->elemental) {    
    g95_error("Subroutine call to intrinsic '%s' at %L is not PURE", nam,        
	      &r->where);          
    return MATCH_ERROR;     
  }     
     
  if (g95_intrinsic_symbol(r->sym, 0)) return MATCH_ERROR;

  return MATCH_YES;     
     
fail:          
  g95_suppress_error = 0;         
  return MATCH_NO; 
}       
       
       
     
     
/* check_specific()-- Given a pointer to an intrinsic symbol and an
 * expression consisting of a function call, see if the function call
 * is consistent with the intrinsic's formal argument list.  Return
 * SUCCESS if the expression and intrinsic match, FAILURE otherwise.  */  
  
static try check_specific(g95_intrinsic_sym *specific, g95_expr *exp,     
			  int error_flag) { 
g95_actual_arglist *arg, **ap;      
int z;         
try d;

  ap = &exp->value.function.actual;          
          
  init_arglist(specific);   
   
/* Don't attempt to sort the argument list for min or max */         
         
  if (specific->check == g95_check_min_max ||  
      specific->check == g95_check_min_max_integer ||      
      specific->check == g95_check_min_max_real ||     
      specific->check == g95_check_min_max_double)          
    return (*specific->check)(*ap);

  if (sort_actual(specific->name, ap, specific->formal,     
		  &exp->where) == FAILURE) return FAILURE;  
  
  /* sort_actual() can get minloc() and maxloc() slightly wrong, so
   * this is fixed in g95_check_minloc_maxloc() */      
      
  if (specific->check == g95_check_minloc_maxloc)        
    d = g95_check_minloc_maxloc(*ap);      
  else {    
    if (specific->check == NULL) {          
      d = check_arglist(ap, specific, error_flag);      
      if (d == SUCCESS) exp->ts = specific->ts;          
    } else  
      d = do_check(specific, *ap);          
  }    
    
  /* Check ranks for elemental intrinsics */          
          
  if (d == SUCCESS && specific->elemental) {       
    z = 0; 
    for(arg=exp->value.function.actual; arg; arg=arg->next) {          
      if (arg->u.expr == NULL || arg->u.expr->rank == 0) continue;     
      if (z == 0) {          
	z = arg->u.expr->rank;      
	continue;  
      }          
          
      if (arg->u.expr->rank != z) {   
	g95_error("Ranks of arguments to elemental intrinsic '%s' differ "         
		  "at %L", specific->name, &arg->u.expr->where);       
	return FAILURE;        
      } 
    }  
  }       
       
  if (d == FAILURE) remove_nullargs(ap);       
       
  return d;    
}       
       
       


/* g95_intrinsic_func_interface()-- see if a function call corresponds
 * to an intrinsic function call.  We return:
 *  MATCH_YES    if the call corresponds to an intrinsic, simplification
 *               is done if possible.
 *
 *  MATCH_NO     if the call does not correspond to an intrinsic
 *
 *  MATCH_ERROR  if the call corresponds to an intrinsic but there was an
 *               error during the simplification process.
 *
 * The error_flag parameter enables an error reporting.
 */          
          
match g95_intrinsic_func_interface(g95_expr *e, int error_flag) { 
g95_intrinsic_sym *isym, *specific; 
g95_actual_arglist *a;
char *nm; 
int flag;          
          
  if (g95_has_alt_return(e->value.function.actual)) return MATCH_NO;   
  
  if (e->value.function.isym != NULL)        
    return (do_simplify(e->value.function.isym, e) == FAILURE)         
      ? MATCH_ERROR : MATCH_YES;   
   
  g95_suppress_error = !error_flag;   
  g95_intrinsic_extension = 1;    
  flag = 0;          
          
  for(a=e->value.function.actual; a; a=a->next)       
    if (a->u.expr != NULL)        
      flag |= (a->u.expr->ts.type != BT_INTEGER &&        
	       a->u.expr->ts.type != BT_CHARACTER);        
        
  nm = e->symbol->name;          
          
  isym = specific = g95_find_function(nm);
  if (isym == NULL) {      
    g95_suppress_error = 0;        
    return MATCH_NO;        
  } 
 
  g95_current_intrinsic_where = &e->where;       
       
/* Bypass the generic list for min and max */          
          
  if (isym->resolve == g95_resolve_min_max) {  
    init_arglist(isym);          
          
    if (g95_check_min_max(e->value.function.actual) == SUCCESS) {
      resolve_intrinsic(isym, e);         
      goto got_specific;        
    }      
      
    g95_suppress_error = 0;        
    return MATCH_NO;  
  }     
     
/* If the function is generic, check all of its specific incarnations.
 * If the generic name is also a specific, we check that name last, so
 * that any error message will correspond to the specific */     
     
  g95_suppress_error = 1;    
    
  if (isym->generic) {   
    for(specific=isym->specific_head; specific; specific=specific->next) {  
      if (specific == isym) continue;  
      if (check_specific(specific, e, 0) == SUCCESS) goto got_specific;  
    }         
  }    
    
  g95_suppress_error = !error_flag;       
       
  if (check_specific(isym, e, error_flag) == FAILURE) {         
    g95_suppress_error = 0; 
    return MATCH_NO;      
  }    
    
  specific = isym;    
    
got_specific:      
  e->value.function.isym = specific;

  if (g95_intrinsic_symbol(e->symbol, 1)) {      
    g95_suppress_error = 0;     
    return MATCH_ERROR;  
  }   
   
  if (do_simplify(specific, e) == FAILURE) {  
    g95_suppress_error = 0;       
    return MATCH_ERROR;      
  }          
          
  flag |= (e->ts.type != BT_INTEGER && e->ts.type != BT_CHARACTER);    
    
  if (flag && g95_intrinsic_extension && g95_option.fmode != 0 &&      
      g95_init_expr)  
    g95_warning(102, "Evaluation of initialization expression at %L is "        
		"nonstandard", &e->where);      
      
  g95_suppress_error = 0;    
  return MATCH_YES;       
} 
 
 
