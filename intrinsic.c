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
locus *g95_current_intrinsic_where;

static g95_intrinsic_sym *functions, *subroutines, *conversion, *next_sym;    
static g95_intrinsic_arg *next_arg;  
  
static int nfunc, nsub, nargs, nconv;  
  
static enum { SZ_NOTHING=0, SZ_SUBS, SZ_FUNCS, SZ_CONVS } sizing;       
       
static try do_simplify(g95_intrinsic_sym *, g95_expr *);       
       
       


/* make_generic()-- Collect a set of intrinsic functions into a
 * generic collection.  The first argument is the name of the generic
 * function, which is also the name of a specific function.  The rest
 * of the specifics currently in the table are placed into the list of
 * specific functions associated with that generic.  */   
   
static void make_generic(char *name, enum g95_generic_isym_id generic_id) {      
g95_intrinsic_sym *t;  
  
  if (sizing != SZ_NOTHING) return;      
     
  t = g95_find_function(name);
  if (t == NULL)
    g95_internal_error("make_generic(): Can't find generic symbol '%s'", name);   
   
  t->generic = 1;        
  t->specific = 1;      
  t->generic_id = generic_id;
  if ((t+1)->name[0] != '\0') t->specific_head = t + 1;    
  t++;          
            
  while(t->name[0] != '\0') {         
    t->next = t + 1;    
    t->specific = 1;    
    t->generic_id = generic_id;   
    t++;         
  }  
  
  t--;         
  t->next = NULL;        
}       
       
       
    
    
/* do_check()-- Interface to the check functions.  We break apart an
 * argument list and call the proper check function rather than
 * forcing each function to manipulate the argument list */          
          
static try do_check(g95_intrinsic_sym *specific, g95_actual_arglist *arg) {      
g95_expr *b, *k, *n, *a4, *i;  
try r;    
    
  b = arg->u.expr;  
  arg = arg->next;       
       
  if (arg == NULL)   
    r = (*specific->check)(b);        
  else { 
    k = arg->u.expr;  
    arg = arg->next;    
    
    if (arg == NULL)      
      r = (*specific->check)(b, k);  
    else {
      n = arg->u.expr;       
      arg = arg->next;     
           
      if (arg == NULL) 
	r = (*specific->check)(b, k, n);  
      else {      
	a4 = arg->u.expr;         
	arg = arg->next;         
         
	if (arg == NULL)         
	  r = (*specific->check)(b, k, n, a4);       
	else {   
	  i = arg->u.expr;          
	  arg = arg->next;         
         
	  if (arg == NULL)      
	    r = (*specific->check)(b, k, n, a4, i);      
	  else {         
	    g95_internal_error("do_check(): too many args");   
	  }       
	}      
      }       
    }        
  } 
 
  return r;      
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
   
static void add_sym(char *nam, int elemental, int actual_ok, bt type,   
		    int k, try (*check)(), g95_expr *(*simplify)(),  
		    void (*resolve)(), ...) {       
       
int optional, first_flag; 
va_list ap;   
   
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
    next_sym->ts.type = type;         
    next_sym->ts.kind = k; 
    next_sym->simplify = simplify;    
    next_sym->check = check;        
    next_sym->resolve = resolve;     
    next_sym->specific = 0;         
    next_sym->generic = 0;
    break;       
       
  default:        
    g95_internal_error("add_sym(): Bad sizing mode");  
  }

  va_start(ap, resolve);          
          
  first_flag = 1; 
 
  for(;;) {     
    nam = va_arg(ap, char *); 
    if (nam == NULL) break;       
       
    type = (bt) va_arg(ap, int);          
    k = va_arg(ap, int);       
    optional = va_arg(ap, int);          
          
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
      next_arg->ts.type = type; 
      next_arg->ts.kind = k; 
      next_arg->optional = optional;          
    }      
  }    
    
  va_end(ap);        
        
  next_sym++;   
}       
       
       
     
     
/* g95_type_letter()-- Return a letter based on the passed type.  Used
 * to construct the name of a type-dependent subroutine. */ 
 
char g95_type_letter(bt t) {    
char x;    
    
  switch(t) {
  case BT_LOGICAL:    x = 'l';  break;         
  case BT_CHARACTER:  x = 'c';  break;       
  case BT_INTEGER:    x = 'i';  break;    
  case BT_REAL:       x = 'r';  break;   
  case BT_COMPLEX:    x = 'z';  break;      
  case BT_DERIVED:    x = 'd';  break;      
      
  default:            x = 'u';  break;        
  }

  return x;     
}         
         
         
    
    
/* g95_specific_intrinsic()-- Given a string, figure out if it is the
 * name of a specific intrinsic function or not. */         
         
int g95_specific_intrinsic(char *name0) {
g95_intrinsic_sym *sy;    
    
  sy = g95_find_function(name0);   
  return (sy == NULL) ? 0 : sy->specific;  
}      
      
      
  
  
/* g95_has_alt_return()-- Checks an actual argument list to see if
 * there is an alternate return buried someplace in it.  Returns
 * nonzero if so, zero if an alternate return is present. */    
    
int g95_has_alt_return(g95_actual_arglist *c) {         
         
  while(c != NULL) {  
    if (c->type == ALT_RETURN) return 1;       
    c = c->next;   
  }         
         
  return 0;          
} 
 
 
  
/******** Subroutines to check intrinsic interfaces ***********/         
         
/* remove_nullargs()-- Given an actual argument list, remove any
 * NULL arguments that may have been left behind by a sort against
 * some formal argument list. */        
        
static void remove_nullargs(g95_actual_arglist **actual) {
g95_actual_arglist *start, *end, *next1;      
      
  end = NULL;

  for(start=*actual; start; start=next1) {
    next1 = start->next;   
   
    if (start->type == EXPR && start->u.expr == NULL) {          
      start->next = NULL;         
      g95_free_actual_arglist(start);   
    } else {         
      if (end == NULL)    
	*actual = start;      
      else
	end->next = start;     
     
      end = start;         
      end->next = NULL;   
    }       
  }    
    
  if (end == NULL) *actual = NULL;    
}          
          
          
      
      
/* g95_intrinsic_symbol()-- Given a symbol that we have decided is
 * intrinsic, mark it as such by placing it into a special module that
 * is otherwise impossible to read or write.  Returns nonzero if
 * something goes wrong. */

int g95_intrinsic_symbol(g95_symbol *symbol, int function) {          
          
  if (symbol->attr.generic) return 0;         
  strcpy(symbol->module, "(intrinsic)");  
  
  if (symbol->attr.proc != PROC_INTRINSIC && 
      (g95_add_procedure(&symbol->attr, PROC_INTRINSIC, NULL) == FAILURE))  
      return 1;          
          
  if (function) {   
    if (!symbol->attr.function && g95_add_function(&symbol->attr, NULL) == FAILURE)   
      return 1; 
 
    if (symbol->result == NULL) symbol->result = symbol;      
  } else {         
    if (!symbol->attr.subroutine &&         
	g95_add_subroutine(&symbol->attr, NULL) == FAILURE)          
      return 1;        
  }     
     
  return 0;       
}      
      
      
         
         
/* find_sym()-- Locate an intrinsic symbol given a base pointer,
 * number of elements in the table and a pointer to a name.  Returns
 * the NULL pointer if a name is not found. */     
     
static g95_intrinsic_sym *find_sym(g95_intrinsic_sym *s, int a,  
				   char *nm) {      
      
  while(a > 0) {    
    if (strcmp(nm, s->name) == 0) return s;  
  
    s++;     
    a--;  
  }          
          
  return NULL;
}       
       
       


/* g95_find_function()-- Given a name, find a function in the intrinsic
 * function table.  Returns NULL if not found. */      
      
g95_intrinsic_sym *g95_find_function(char *name) {       
       
  return find_sym(functions, nfunc, name);       
}   
   
   
          
          
/* init_arglist()-- Initialize the g95_current_intrinsic_arg[] array
 * for the benefit of error messages.  This subroutine returns FAILURE
 * if a subroutine has more than MAX_INTRINSIC_ARGS, in which case the
 * actual argument list cannot match any intrinsic. */         
         
static void init_arglist(g95_intrinsic_sym *is) {         
g95_intrinsic_arg *form; 
int s;    
    
  g95_current_intrinsic = is->name;      
      
  s = 0;       
  for(form=is->formal; form; form=form->next) {        
    if (s >= MAX_INTRINSIC_ARGS)  
      g95_internal_error("init_arglist(): too many arguments");          
    g95_current_intrinsic_arg[s++] = form->name;
  }  
}        
        
        


/* check_arglist()-- Compare an actual argument list with an
 * intrinsic's formal argument list.  The lists are checked for
 * agreement of type.  We don't check for arrayness here. */

static try check_arglist(g95_actual_arglist **ap, g95_intrinsic_sym *symbol,  
			 int error_flag) {         
g95_actual_arglist *f; 
g95_intrinsic_arg *frm;      
int b; 
 
  frm = symbol->formal;       
  f = *ap;       
       
  b = 0;    
  for(; frm; frm=frm->next, f=f->next, b++) {         
    if (f->u.expr == NULL) continue;          
          
    if (!g95_compare_types(&frm->ts, &f->u.expr->ts)) {       
      if (error_flag)       
	g95_error("Type of argument '%s' in call to '%s' at %L should be "          
		  "%s, not %s", g95_current_intrinsic_arg[b],  
		  g95_current_intrinsic, &f->u.expr->where,          
		  g95_typename(&frm->ts),        
		  g95_typename(&f->u.expr->ts));
      return FAILURE;
    }  
  }       
       
  return SUCCESS;      
}        
        
        
          
          
/* make_alias()-- Create a duplicate intrinsic function entry for the
 * current function, the only difference being the alternate name.
 * Note that we use argument lists more than once, but all argument
 * lists are freed as a single block.  */   
   
static void make_alias(char *name0) {          
          
  switch(sizing) { 
  case SZ_FUNCS:       
    nfunc++;      
    break;  
  
  case SZ_SUBS:    
    nsub++;  
    break;          
          
  case SZ_NOTHING:  
    next_sym[0] = next_sym[-1];         
    strcpy(next_sym->name, name0);      
    next_sym++;     
    break;          
          
  default: 
    break;         
  }        
}


         
         
/* examine_arglist()-- Examine the argument list.  Returns 0 if all
 * constructor arguments have no elements left, 1 if all constructor
 * arguments have at least one element left, and 2 otherwise (which is
 * an error). */   
   
static int examine_arglist(g95_actual_arglist *a) {   
int k, r, seen_cons;      
      
  seen_cons = 0;        
  r = 2;         
         
  for(; a; a=a->next) {  
    if (a->u.expr == NULL || a->u.expr->type != EXPR_ARRAY) continue;    
    
    k = (a->u.expr->value.constructor == NULL) ? 0 : 1; 
 
    if (!seen_cons) { 
      seen_cons = 1;        
      r = k;       
    } else {       
      if (r != k) {
	g95_error("Arguments of elemental intrinisc at %L have differing " 
		  "numbers of elements", &a->u.expr->where);         
	return 2;       
      }       
    }    
  }     
     
  return r;        
}       
       
       
          
          
/* g95_generic_intrinsic()-- Given a string, figure out if it is
 * the name of a generic intrinsic function or not. */ 
 
int g95_generic_intrinsic(char *n) {       
g95_intrinsic_sym *sym;        
        
  sym = g95_find_function(n);    
  return (sym == NULL) ? 0 : sym->generic; 
}       
       
       
 
 
/* find_subroutine()-- Given a name, find a function in the intrinsic
 * subroutine table.  Returns NULL if not found. */

static g95_intrinsic_sym *find_subroutine(char *n) {

  return find_sym(subroutines, nsub, n);      
}        
        
        
 
 
/* simplify_elemental_intrinsic()-- Simplify an elemental intrinsic
 * that has an array constructor(s) as actual arguments.  Constructors
 * are expanded, and a new constructor is built the elements of which
 * are the function calls.  Returns nonzero if the caller does not
 * have anything more to do. */    
    
static int simplify_elemental_intrinsic(g95_intrinsic_sym *s, g95_expr *q,   
					try *status) {       
g95_actual_arglist *arg, *o, *tail_a;       
g95_constructor *v, *start, *tail;     
g95_expr *r, *p;     
int flag;        
        
  if (!s->elemental) return 0;    
    
  flag = 0; 
 
  for(arg=q->value.function.actual; arg; arg=arg->next) {       
    r = arg->u.expr;    
    if (r == NULL) continue;    
    
    if (r->rank > 0 && r->type != EXPR_ARRAY) return 0;        
        
    if (r->type == EXPR_ARRAY) flag = 1;   
  }         
         
  if (!flag) return 0;         
         
  /* Expand the individual constructors */       
       
  for(arg=q->value.function.actual; arg; arg=arg->next) { 
    r = arg->u.expr;       
    if (r == NULL) continue;          
          
    if (r->type == EXPR_ARRAY &&
	g95_expand_constructor(r) == FAILURE) return 0;     
  } 
 
  /* Build a new constructor */     
     
  switch(examine_arglist(q->value.function.actual)) {        
  case 0:   
    start = NULL;      
    goto done;        
        
  case 2:      
    *status = FAILURE;  
    return 1;       
       
  case 1:    
    break;       
  }   
   
  start = tail = NULL;        
        
  for(;;) {  
    p = g95_get_expr();     
    p->type = EXPR_FUNCTION;  
    p->where = q->where;     
     
    p->value.function.isym = s; 
    p->value.function.name = s->name;          
    p->ts = s->ts;      
      
    tail_a = NULL;  
  
    for(arg=q->value.function.actual; arg; arg=arg->next) {
      o = g95_get_actual_arglist();         
         
      if (p->value.function.actual == NULL)      
	p->value.function.actual = o;        
      else        
	tail_a->next = o; 
 
      tail_a = o;         
         
      if (arg->u.expr->type != EXPR_ARRAY) 
	o->u.expr = g95_copy_expr(arg->u.expr);   
      else {       
	v = arg->u.expr->value.constructor;         
	o->u.expr = v->expr; 
	arg->u.expr->value.constructor = v->next;

	g95_free(v);    
      }          
    }     
     
    if (start == NULL) {        
      start = tail = g95_get_constructor();          
    } else {    
      tail->next = g95_get_constructor(); 
      tail = tail->next;       
    }          
          
    tail->expr = p;   
   
    switch(examine_arglist(q->value.function.actual)) {         
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
  p = g95_get_expr();  
  p->type = EXPR_ARRAY; 
  p->ts = s->ts;      
  p->where = q->where;       
  p->value.constructor = start;
  p->rank = 1;  
  
  *status = SUCCESS;   
   
  while(start != NULL) {      
    if (do_simplify(s, start->expr) == FAILURE) {       
      *status = FAILURE;       
      break;          
    } 
 
    start = start->next;     
  }      
      
  g95_replace_expr(q, p);          
  return 1;     
}   
   
   
        
        
/* conv_name()-- Return a pointer to the name of a conversion function
 * given two typespecs. */ 
 
static char *conv_name(g95_typespec *f, g95_typespec *d) {    
static char name[30];          
          
  sprintf(name, "__convert_%c%d_%c%d", g95_type_letter(f->type), f->kind, 
	  g95_type_letter(d->type), d->kind);     
     
  return name;        
}         
         
         
  
  
/* resolve_intrinsic()-- Given a pointer to an intrinsic symbol and an
 * expression node that represent the function call to that
 * subroutine, figure out the type of the result.  This may involve
 * calling a resolution subroutine */    
    
static void resolve_intrinsic(g95_intrinsic_sym *specific, g95_expr *g) {       
g95_expr *y, *h, *v, *p, *a5;       
g95_actual_arglist *a;  
  
  if (specific->resolve == NULL) {
    if (g->value.function.name == NULL)         
      g->value.function.name = specific->lib_name;         
         
    if (g->ts.type == BT_UNKNOWN) g->ts = specific->ts;
    return;       
  }        
        
  a = g->value.function.actual;

/* At present only the iargc extension intrinsic takes no arguments,
 * and it doesn't need a resolution function, but this is here for
 * generality */         
         
  if (a == NULL) {    
    (*specific->resolve)(g);      
    return;      
  }        
        
  y = a->u.expr;      
  a = a->next;         
         
  if (specific->resolve == g95_resolve_min_max) {    
    g95_resolve_min_max(g, specific, y);         
    return; 
  }          
          
  if (a == NULL) {    
    (*specific->resolve)(g, y);      
    return;       
  }        
        
  h = a->u.expr;
  a = a->next; 
 
  if (a == NULL) {  
    (*specific->resolve)(g, y, h); 
    return;
  }       
       
  v = a->u.expr; 
  a = a->next;         
               
  if (a == NULL) {         
    (*specific->resolve)(g, y, h, v);      
    return; 
  }        
        
  p = a->u.expr;  
  a = a->next;        
        
  if (a == NULL) {     
    (*specific->resolve)(g, y, h, v, p);
    return; 
  }

  a5 = a->u.expr;        
  a = a->next;         
         
  if (a == NULL) {       
    (*specific->resolve)(g, y, h, v, p, a5);
    return;        
  }         
         
  g95_internal_error("resolve_intrinsic(): Too many args for intrinsic");       
}  
  
  
      
      
/* add_conv()-- Add a function to the list of conversion symbols */

static void add_conv(bt from_type, int from_kind, bt to_type, int to_kind,   
		     g95_expr *(*simplify)()) {  
  
g95_typespec f, t;  
g95_intrinsic_sym *s;         
         
  if (sizing == SZ_CONVS) {    
    nconv++;         
    return;   
  } 
 
  g95_clear_ts(&f);        
  f.type = from_type;         
  f.kind = from_kind;       
       
  g95_clear_ts(&t); 
  t.type = to_type;  
  t.kind = to_kind;     
     
  s = conversion + nconv;  
  
  strcpy(s->name, conv_name(&f, &t));     
  strcpy(s->lib_name, s->name);          
  s->simplify = simplify;    
  s->elemental = 1;     
  s->ts = t;
  s->generic_id = G95_ISYM_CONVERSION;  
  
  nconv++;
} 
 
 
         
         
void g95_intrinsic_done_1(void) {    
    
  g95_free(functions);      
  g95_free(conversion);         
}       
       
       
   
   
/* add_subroutines()-- Add intrinsic subroutines */       
       
static void add_subroutines(void) {        
        
/* Argument names as in the standard (to be used as argument keywords) */ 
char   w[] = "harvest", dt[] = "date", vl[] = "values",  pt[] = "put",
       y[] = "count",   tm[] = "time", tp[] = "topos",   gt[] = "get",   
       x[] = "to",      zn[] = "zone", fp2[] = "frompos", cm[] = "count_max",     
       d[] = "from",    sz[] = "size", ln[] = "len",     cr[] = "count_rate";          
          
int di, dr, dc;      
      
  di = g95_default_integer_kind();       
  dr = g95_default_real_kind();     
  dc = g95_default_character_kind();  
  
  add_sym("cpu_time", 1, 1, BT_UNKNOWN, 0,
	  g95_check_cpu_time, NULL, g95_resolve_cpu_time,  
	  tm, BT_REAL, dr, 0, NULL);      
      
  add_sym("date_and_time", 1, 1, BT_UNKNOWN, 0,          
	  g95_check_date_and_time, NULL, NULL,          
	  dt, BT_CHARACTER, dc, 1,   tm, BT_CHARACTER, dc, 1,     
	  zn, BT_CHARACTER, dc, 1,   vl, BT_INTEGER,   di, 1, NULL);    
    
  add_sym("mvbits", 1, 1, BT_UNKNOWN, 0,       
	  g95_check_mvbits, g95_simplify_mvbits, NULL,  
	  d, BT_INTEGER, di, 0,   fp2, BT_INTEGER, di, 0,     
	  ln, BT_INTEGER, di, 0,   x, BT_INTEGER, di, 0,       
	  tp, BT_INTEGER, di, 0, NULL);      
      
  add_sym("random_number", 0, 1, BT_UNKNOWN, 0,     
	  g95_check_random_number, NULL, g95_resolve_random_number,         
	  w, BT_REAL, dr, 0, NULL);          
          
  add_sym("random_seed", 0, 1, BT_UNKNOWN, 0,   
	  g95_check_random_seed, NULL, NULL,          
	  sz, BT_INTEGER, di, 1,   pt, BT_INTEGER, di, 1,        
	  gt, BT_INTEGER, di, 1, NULL);  
  
  add_sym("system_clock", 1, 1, BT_UNKNOWN, 0,   
	  NULL, NULL, NULL,        
	  y,  BT_INTEGER, di, 1,   cr, BT_INTEGER, di, 1,       
	  cm, BT_INTEGER, di, 1, NULL);  
}   
   
   
         
         
/* add_functions()-- Add intrinsic functions */        
        
static void add_functions(void) {   
   
/* Argument names as in the standard (to be used as argument keywords) */   
   
char   a[] = "a",  d[] = "field",      pt[] = "pointer",   tg[] = "target",      
       e[] = "b",  u[] = "matrix",     ma[] = "matrix_a",  mb[] = "matrix_b",
       c[] = "c",  q[] = "ncopies",   pos[] = "pos",      bck[] = "back",  
       o[] = "i",  v[] = "vector",     va[] = "vector_a",  vb[] = "vector_b",       
       w[] = "j", h[] = "a1",         fs[] = "fsource",   typesp[] = "tsource",      
       l[] = "l", g[] = "a2",         mo[] = "mold",     ord[] = "order",     
       k[] = "p", ar[] = "array",     shp[] = "shape",    s0[] = "source", 
       t[] = "r", bd[] = "boundary",  pad[] = "pad",      set[] = "set",          
       s[] = "s", dm[] = "dim",      k0[] = "kind",     msk[] = "mask",  
       x[] = "x", sh[] = "shift",     stg[] = "string",   ssg[] = "substring",  
       y[] = "y", sz[] = "size",      sta[] = "string_a", stb[] = "string_b",
       z[] = "z", ln[] = "len";

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
         
  make_alias("cdabs");   
   
  make_generic("abs", G95_ISYM_ABS);          
          
  add_sym("achar", 1, 1, BT_CHARACTER, dc,        
	  NULL, g95_simplify_achar, g95_resolve_achar,    
	  o, BT_INTEGER, di, 0, NULL);     
     
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
	  z, BT_COMPLEX, dz, 0, NULL);    
    
  add_sym("dimag", 1, 1, BT_REAL, dd,
	  NULL, g95_simplify_aimag, g95_resolve_aimag,          
	  z, BT_COMPLEX, dd, 0, NULL);    /* Extension */  
  
  make_generic("aimag", G95_ISYM_AIMAG);        
        
  add_sym("aint", 1, 1, BT_REAL, dr,        
	  g95_check_a_xkind, g95_simplify_aint, g95_resolve_aint,         
	  a, BT_REAL, dr, 0,   k0, BT_INTEGER, di, 1, NULL);        
        
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
	  a, BT_REAL, dr, 0,   k0, BT_INTEGER, di, 1, NULL);

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
	  y, BT_REAL, dr, 0,   x, BT_REAL, dr, 0, NULL); 
 
  add_sym("datan2", 1, 1, BT_REAL, dd,        
	  NULL, g95_simplify_atan2, g95_resolve_atan2,  
	  y, BT_REAL, dd, 0,   x, BT_REAL, dd, 0, NULL);   
   
  make_generic("atan2", G95_ISYM_ATAN2);        
        
  add_sym("bit_size", 0, 1, BT_INTEGER, di,          
	  g95_check_i, g95_simplify_bit_size, NULL,   
	  o, BT_INTEGER, di, 0, NULL);    
    
  make_generic("bit_size", G95_ISYM_NONE);

  add_sym("btest", 1, 1, BT_LOGICAL, dl,  
	  g95_check_btest, g95_simplify_btest, g95_resolve_btest,        
	  o, BT_INTEGER, di, 0,   pos, BT_INTEGER, di, 0, NULL); 
 
  make_generic("btest", G95_ISYM_BTEST);   
   
  add_sym("ceiling", 1, 1, BT_INTEGER, di, 
	  g95_check_a_ikind, g95_simplify_ceiling, g95_resolve_ceiling,  
	  a, BT_REAL, dr, 0,   k0, BT_INTEGER, di, 1, NULL);

  make_generic("ceiling", G95_ISYM_CEILING);          
          
  add_sym("char", 1, 0, BT_CHARACTER, dc,        
	  g95_check_char, g95_simplify_char, g95_resolve_char,          
	  o, BT_INTEGER, di, 0,   k0, BT_INTEGER, di, 1, NULL);

  make_generic("char", G95_ISYM_CHAR);         
         
  add_sym("cmplx", 1, 1, BT_COMPLEX, dz,
	  g95_check_cmplx, g95_simplify_cmplx, g95_resolve_cmplx,       
	  x, BT_UNKNOWN, dr, 0,   y, BT_UNKNOWN, dr, 1,        
	  k0, BT_INTEGER, di, 1, NULL);          
          
  make_generic("cmplx", G95_ISYM_CMPLX);         
         
  /* Making dcmplx a specific of cmplx causes cmplx to return a double
   * complex instead of the default complex.  */         
         
  add_sym("dcmplx", 1, 1, BT_COMPLEX, dd,         
	  g95_check_dcmplx, g95_simplify_dcmplx, NULL,  
	  x, BT_REAL, dd, 0,   y, BT_REAL, dd, 1, NULL);  /* Extension */     
     
  make_generic("dcmplx", G95_ISYM_CMPLX);       
       
  add_sym("conjg", 1, 1, BT_COMPLEX, dz,   
	  NULL, g95_simplify_conjg, g95_resolve_conjg,          
	  z, BT_COMPLEX, dz, 0, NULL);      
      
  add_sym("dconjg", 1, 1, BT_COMPLEX, dd,          
	  NULL, g95_simplify_conjg, g95_resolve_conjg,      
	  z, BT_COMPLEX, dd, 0, NULL);   /* Extension */     
     
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
 
  add_sym("zcos", 1, 1, BT_COMPLEX, dd,   
	  NULL, g95_simplify_cos, g95_resolve_cos,      
	  x, BT_COMPLEX, dd, 0, NULL);   /* Extension */       
       
  make_alias("cdcos");      
      
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
       
  make_alias("dfloat");           /* Extension */       
       
  make_generic("dble", G95_ISYM_DBLE);    
    
  add_sym("digits", 0, 1, BT_INTEGER, di,        
	  g95_check_digits, g95_simplify_digits, NULL,       
	  x, BT_UNKNOWN, dr, 0, NULL);     
     
  make_generic("digits", G95_ISYM_NONE);         
         
  add_sym("dim", 1, 1, BT_REAL, dr,          
	  g95_check_a_p, g95_simplify_dim, g95_resolve_dim,     
	  x, BT_UNKNOWN, dr, 0,   y, BT_UNKNOWN, dr, 0, NULL);  
  
  add_sym("idim", 1, 1, BT_INTEGER, di,       
	  NULL, g95_simplify_dim, g95_resolve_dim,   
	  x, BT_INTEGER, di, 0,   y, BT_INTEGER, di, 0, NULL);    
    
  add_sym("ddim", 1, 1, BT_REAL, dd,     
	  NULL, g95_simplify_dim, g95_resolve_dim,     
	  x, BT_REAL, dd, 0,   y, BT_REAL, dd, 0, NULL);        
        
  make_generic("dim", G95_ISYM_DIM);         
         
  add_sym("dot_product", 0, 1, BT_UNKNOWN, 0,
	  g95_check_dot_product, NULL, g95_resolve_dot_product,   
	  va, BT_REAL, dr, 0,   vb, BT_REAL, dr, 0, NULL); 
 
  make_generic("dot_product", G95_ISYM_DOT_PRODUCT);

  add_sym("dprod", 1, 1, BT_REAL, dd,      
	  NULL, g95_simplify_dprod, NULL,  
	  x, BT_REAL, dr, 0,   y, BT_REAL, dr, 0, NULL);  
  
  make_generic("dprod", G95_ISYM_DPROD);     
     
  add_sym("dreal", 1, 0, BT_REAL, dd,   
	  NULL, NULL, NULL,      
	  a, BT_COMPLEX, dd, 0,  NULL);    /* Extension */       
       
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
      
  add_sym("zexp", 1, 1, BT_COMPLEX, dd,     
	  NULL, g95_simplify_exp, g95_resolve_exp, 
	  x, BT_COMPLEX, dd, 0, NULL);   /* Extension */          
          
  make_alias("cdexp");       
       
  make_generic("exp", G95_ISYM_EXP);       
       
  add_sym("exponent", 1, 1, BT_INTEGER, di, 
	  g95_check_x, g95_simplify_exponent, g95_resolve_exponent,   
	  x, BT_REAL, dr, 0, NULL);   
   
  make_generic("exponent", G95_ISYM_EXPONENT);  
  
  add_sym("floor", 1, 1, BT_INTEGER, di,  
	  g95_check_a_ikind, g95_simplify_floor, g95_resolve_floor,          
	  a, BT_REAL, dr, 0,   k0, BT_INTEGER, di, 1, NULL);          
          
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
	  c, BT_CHARACTER, dc, 0, NULL);          
          
  make_generic("iachar", G95_ISYM_IACHAR);  
  
  add_sym("iand", 1, 1, BT_INTEGER, di,         
	  g95_check_iand, g95_simplify_iand, g95_resolve_iand,    
	  o, BT_INTEGER, di, 0,   w, BT_INTEGER, di, 0, NULL);          
          
  make_generic("iand", G95_ISYM_IAND);        
        
  add_sym("ibclr", 1, 1, BT_INTEGER, di,   
	  g95_check_ibclr, g95_simplify_ibclr, g95_resolve_ibclr, 
	  o, BT_INTEGER, di, 0,   pos, BT_INTEGER, di, 0, NULL);    
    
  make_generic("ibclr", G95_ISYM_IBCLR);    
    
  add_sym("ibits", 1, 1, BT_INTEGER, di,      
	  g95_check_ibits, g95_simplify_ibits, g95_resolve_ibits, 
	  o, BT_INTEGER, di, 0,   pos, BT_INTEGER, di, 0,         
	  ln, BT_INTEGER, di, 0, NULL);          
          
  make_generic("ibits", G95_ISYM_IBITS);

  add_sym("ibset", 1, 1, BT_INTEGER, di,  
	  g95_check_ibset, g95_simplify_ibset, g95_resolve_ibset,         
	  o, BT_INTEGER, di, 0, pos,   BT_INTEGER, di, 0, NULL);  
  
  make_generic("ibset", G95_ISYM_IBSET);          
          
  add_sym("ichar", 1, 0, BT_INTEGER, di,  
	  NULL, g95_simplify_ichar, g95_resolve_ichar,     
	  c, BT_CHARACTER, dc, 0, NULL);      
      
  make_generic("ichar", G95_ISYM_ICHAR); 
 
  add_sym("ieor", 1, 1, BT_INTEGER, di,
	  g95_check_ieor, g95_simplify_ieor, g95_resolve_ieor,    
	  o, BT_INTEGER, di, 0,   w, BT_INTEGER, di, 0, NULL);         
         
  make_generic("ieor", G95_ISYM_IEOR);      
      
  add_sym("index", 1, 1, BT_INTEGER, di,      
	  g95_check_index, g95_simplify_index, NULL,     
	  stg, BT_CHARACTER, dc, 0,   ssg, BT_CHARACTER, dc, 0,      
	  bck, BT_LOGICAL, dl, 1, NULL);  
  
  make_generic("index", G95_ISYM_INDEX);   
   
  add_sym("int", 1, 1, BT_INTEGER, di, 
	  g95_check_int, g95_simplify_int, g95_resolve_int,     
	  a, BT_REAL, dr, 0,   k0, BT_INTEGER, di, 1, NULL);      
      
  add_sym("ifix", 1, 0, BT_INTEGER, di,        
	  NULL, g95_simplify_ifix, NULL,        
	  a, BT_REAL, dr, 0, NULL);        
        
  add_sym("idint", 1, 0, BT_INTEGER, di,        
	  NULL, g95_simplify_idint, NULL,       
	  a, BT_REAL, dd, 0, NULL); 
 
  make_generic("int", G95_ISYM_INT); 
 
  add_sym("ior", 1, 1, BT_INTEGER, di,  
	  g95_check_ior, g95_simplify_ior, g95_resolve_ior,         
	  o, BT_INTEGER, di, 0, w,   BT_INTEGER, di, 0, NULL);          
          
  make_generic("ior", G95_ISYM_IOR);        
        
  add_sym("ishft", 1, 1, BT_INTEGER, di,          
	  g95_check_ishft, g95_simplify_ishft, g95_resolve_ishft, 
	  o, BT_INTEGER, di, 0,   sh, BT_INTEGER, di, 0, NULL);    
    
  make_generic("ishft", G95_ISYM_ISHFT);  
  
  add_sym("ishftc", 1, 1, BT_INTEGER, di,         
	  g95_check_ishftc, g95_simplify_ishftc, g95_resolve_ishftc,
	  o, BT_INTEGER, di, 0,   sh, BT_INTEGER, di, 0,
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
 
  add_sym("zlog", 1, 1, BT_COMPLEX, dd, 
	  NULL, g95_simplify_log, g95_resolve_log,   
	  x, BT_COMPLEX, dd, 0, NULL);   /* Extension */         
         
  make_alias("cdlog");        
        
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
	  l, BT_LOGICAL, dl, 0,   k0, BT_INTEGER, di, 1, NULL);        
        
  make_generic("logical", G95_ISYM_LOGICAL);          
          
  add_sym("matmul", 0, 1, BT_REAL, dr,       
	  g95_check_matmul, NULL, g95_resolve_matmul,      
	  ma, BT_REAL, dr, 0,   mb, BT_REAL, dr, 0, NULL); 
 
  make_generic("matmul", G95_ISYM_MATMUL);      
      
/* Note: amax0 is equivalent to real(max), max1 is equivalent to
 * int(max).  The max function must have at least two arguments. */        
        
  add_sym("max", 1, 0, BT_UNKNOWN, 0, 
	  g95_check_min_max, g95_simplify_max, g95_resolve_min_max,          
	  h, BT_UNKNOWN, dr, 0,   g, BT_UNKNOWN, dr, 0, NULL);        
        
  add_sym("max0", 1, 0, BT_INTEGER, di,     
	  g95_check_min_max_integer, g95_simplify_max, g95_resolve_min_max,        
	  h, BT_INTEGER, di, 0,   g, BT_INTEGER, di, 0, NULL);    
    
  add_sym("amax0", 1, 0, BT_REAL, dr,        
	  g95_check_min_max_integer, g95_simplify_max, g95_resolve_min_max,   
	  h, BT_INTEGER, di, 0,   g, BT_INTEGER, di, 0, NULL);     
     
  add_sym("amax1", 1, 0, BT_REAL, dr,      
	  g95_check_min_max_real, g95_simplify_max, g95_resolve_min_max,      
	  h, BT_REAL, dr, 0,   g, BT_REAL, dr, 0, NULL);     
     
  add_sym("max1", 1, 0, BT_INTEGER, di, 
	  g95_check_min_max_real, g95_simplify_max, g95_resolve_min_max,         
	  h, BT_REAL, dr, 0,   g, BT_REAL, dr, 0, NULL);  
  
  add_sym("dmax1", 1, 0, BT_REAL, dd,   
	  g95_check_min_max_double, g95_simplify_max, g95_resolve_min_max,       
	  h, BT_REAL, dd, 0,   g, BT_REAL, dd, 0, NULL);          
          
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
	  typesp, BT_REAL, dr, 0,	  fs, BT_REAL, dr, 0,         
	  msk, BT_LOGICAL, dl, 0, NULL);   
   
  make_generic("merge", G95_ISYM_MERGE);

/* Note: amin0 is equivalent to real(min), min1 is equivalent to int(min). */    
    
  add_sym("min", 1, 0, BT_UNKNOWN, 0,   
	  g95_check_min_max, g95_simplify_min, g95_resolve_min_max,          
	  h, BT_REAL, dr, 0,   g, BT_REAL, dr, 0, NULL);       
       
  add_sym("min0", 1, 0, BT_INTEGER, di,         
	  g95_check_min_max_integer, g95_simplify_min, g95_resolve_min_max,         
	  h, BT_INTEGER, di, 0,   g, BT_INTEGER, di, 0, NULL);  
  
  add_sym("amin0", 1, 0, BT_REAL, dr,        
	  g95_check_min_max_integer, g95_simplify_min, g95_resolve_min_max,          
	  h, BT_INTEGER, di, 0,   g, BT_INTEGER, di, 0, NULL);

  add_sym("amin1", 1, 0, BT_REAL, dr,    
	  g95_check_min_max_real, g95_simplify_min, g95_resolve_min_max,         
	  h, BT_REAL, dr, 0,   g, BT_REAL, dr, 0, NULL);    
    
  add_sym("min1", 1, 0, BT_INTEGER, di, 
	  g95_check_min_max_real, g95_simplify_min, g95_resolve_min_max,         
	  h, BT_REAL, dr, 0,   g, BT_REAL, dr, 0, NULL);         
         
  add_sym("dmin1", 1, 0, BT_REAL, dd,        
	  g95_check_min_max_double, g95_simplify_min, g95_resolve_min_max,      
	  h, BT_REAL, dd, 0,   g, BT_REAL, dd, 0, NULL);

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
	  a, BT_INTEGER, di, 0,   k, BT_INTEGER, di, 0, NULL);    
    
  add_sym("amod", 1, 1, BT_REAL, dr,   
	  NULL, g95_simplify_mod, g95_resolve_mod, 
	  a, BT_REAL, dr, 0,   k, BT_REAL, dr, 0, NULL);  
  
  add_sym("dmod", 1, 1, BT_REAL, dd,          
	  NULL, g95_simplify_mod, g95_resolve_mod,     
	  a, BT_REAL, dd, 0,   k, BT_REAL, dd, 0, NULL);      
      
  make_generic("mod", G95_ISYM_MOD);   
   
  add_sym("modulo", 1, 1, BT_REAL, di,         
	  g95_check_a_p, g95_simplify_modulo, g95_resolve_modulo,          
	  a, BT_REAL, di, 0,   k, BT_REAL, di, 0, NULL);     
     
  make_generic("modulo", G95_ISYM_MODULO);  
  
  add_sym("nearest", 1, 1, BT_REAL, dr,
	  g95_check_nearest, g95_simplify_nearest, g95_resolve_nearest,        
	  x, BT_REAL, dr, 0,   s, BT_REAL, dr, 0, NULL);

  make_generic("nearest", G95_ISYM_NEAREST);     
     
  add_sym("nint", 1, 1, BT_INTEGER, di,    
	  g95_check_a_ikind, g95_simplify_nint, g95_resolve_nint,    
	  a, BT_REAL, dr, 0,   k0, BT_INTEGER, di, 1, NULL);  
  
  add_sym("idnint", 1, 1, BT_INTEGER, di,          
	  g95_check_idnint, g95_simplify_idnint, g95_resolve_idnint,  
	  a, BT_REAL, dd, 0, NULL);         
         
  make_generic("nint", G95_ISYM_NINT);       
       
  add_sym("not", 1, 1, BT_INTEGER, di,         
	  g95_check_i, g95_simplify_not, g95_resolve_not, 
	  o, BT_INTEGER, di, 0, NULL); 
 
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
	  a, BT_UNKNOWN, dr, 0,   k0, BT_INTEGER, di, 1, NULL);

  add_sym("float", 1, 0, BT_REAL, dr,   
	  NULL, g95_simplify_float, NULL, 
	  a, BT_INTEGER, di, 0, NULL);    
    
  add_sym("sngl", 1, 0, BT_REAL, dr,
	  NULL, g95_simplify_sngl, NULL,         
	  a, BT_REAL, dd, 0, NULL);    
    
  make_generic("real", G95_ISYM_REAL);

  add_sym("repeat", 0, 1, BT_CHARACTER, dc,    
	  g95_check_repeat, g95_simplify_repeat, g95_resolve_repeat,
	  stg, BT_CHARACTER, dc, 0,   q, BT_INTEGER, di, 0, NULL);    
    
  make_generic("repeat", G95_ISYM_REPEAT);          
          
  add_sym("reshape", 0, 1, BT_REAL, dr,       
	  g95_check_reshape, g95_simplify_reshape, g95_resolve_reshape,     
	  s0, BT_REAL, dr, 0,   shp, BT_INTEGER, di, 0,      
	  pad, BT_REAL, dr, 1,   ord, BT_INTEGER, di, 1, NULL);     
     
  make_generic("reshape", G95_ISYM_RESHAPE);   
   
  add_sym("rrspacing", 1, 1, BT_REAL, dr,         
	  g95_check_x, g95_simplify_rrspacing, g95_resolve_rrspacing,    
	  x, BT_REAL, dr, 0, NULL);          
          
  make_generic("rrspacing", G95_ISYM_NONE);        
        
  add_sym("scale", 1, 1, BT_REAL, dr,        
	  g95_check_scale, g95_simplify_scale, g95_resolve_scale,     
	  x, BT_REAL, dr, 0,   o, BT_INTEGER, di, 0, NULL);

  make_generic("scale", G95_ISYM_NONE);          
          
  add_sym("scan", 1, 1, BT_INTEGER, di,         
	  g95_check_scan, g95_simplify_scan, g95_resolve_scan, 
	  stg, BT_CHARACTER, dc, 0,  set, BT_CHARACTER, dc, 0,        
	  bck, BT_LOGICAL, dl, 1, NULL); 
 
  make_generic("scan", G95_ISYM_SCAN);         
         
  add_sym("selected_int_kind", 0, 1, BT_INTEGER, di,        
	  NULL, g95_simplify_selected_int_kind, NULL,   
	  t, BT_INTEGER, di, 0, NULL);   
   
  make_generic("selected_int_kind", G95_ISYM_SELECTED_INT_KIND);

  add_sym("selected_real_kind", 0, 1, BT_INTEGER, di,    
	  g95_check_selected_real_kind, g95_simplify_selected_real_kind, NULL,      
	  k, BT_INTEGER, di, 1,   t, BT_INTEGER, di, 1, NULL);

  make_generic("selected_real_kind", G95_ISYM_SELECTED_REAL_KIND);   
   
  add_sym("set_exponent", 1, 1, BT_REAL, dr,    
	  g95_check_set_exponent, g95_simplify_set_exponent,   
	  g95_resolve_set_exponent,          
	  x, BT_REAL, dr, 0,   o, BT_INTEGER, di, 0, NULL);  
  
  make_generic("set_exponent", G95_ISYM_SET_EXPONENT);    
    
  add_sym("shape", 0, 1, BT_INTEGER, di,  
	  g95_check_shape, g95_simplify_shape, g95_resolve_shape,         
	  s0, BT_REAL, dr, 0, NULL);          
          
  make_generic("shape", G95_ISYM_SHAPE);          
          
  add_sym("sign", 1, 1, BT_REAL, dr,     
	  g95_check_sign, g95_simplify_sign, g95_resolve_sign,          
	  a, BT_REAL, dr, 0,   e, BT_REAL, dr, 0, NULL);   
   
  add_sym("isign", 1, 1, BT_INTEGER, di, 
	  NULL, g95_simplify_sign, g95_resolve_sign,  
	  a, BT_INTEGER, di, 0,   e, BT_INTEGER, di, 0, NULL);        
        
  add_sym("dsign", 1, 1, BT_REAL, dd,   
	  NULL, g95_simplify_sign, g95_resolve_sign,  
	  a, BT_REAL, dd, 0,   e, BT_REAL, dd, 0, NULL); 
 
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
       
  add_sym("zsin", 1, 1, BT_COMPLEX, dd, 
	  NULL, g95_simplify_sin, g95_resolve_sin,
	  x, BT_COMPLEX, dd, 0, NULL);   /* Extension */ 
 
  make_alias("cdsin");        
        
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
	  s0, BT_REAL, dr, 0,   dm, BT_INTEGER, di, 0,          
	  q, BT_INTEGER, di, 0, NULL);          
          
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

  add_sym("zsqrt", 1, 1, BT_COMPLEX, dd,  
	  NULL, g95_simplify_sqrt, g95_resolve_sqrt,      
	  x, BT_COMPLEX, dd, 0, NULL);   /* Extension */     
     
  make_alias("cdsqrt");       
       
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
	  s0, BT_REAL, dr, 0,    mo, BT_REAL, dr, 0,         
	  sz, BT_INTEGER, di, 1,  NULL);       
       
  make_generic("transfer", G95_ISYM_TRANSFER);        
        
  add_sym("transpose", 0, 1, BT_REAL, dr, 
	  g95_check_transpose, NULL, g95_resolve_transpose,    
	  u, BT_REAL, dr, 0, NULL);         
         
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
	  d, BT_REAL, dr, 0, NULL);       
       
  make_generic("unpack", G95_ISYM_UNPACK);

  add_sym("verify", 1, 1, BT_INTEGER, di,        
	  g95_check_verify, g95_simplify_verify, g95_resolve_verify,      
	  stg, BT_CHARACTER, dc, 0,   set, BT_CHARACTER, dc, 0,   
	  bck, BT_LOGICAL, dl, 1, NULL);

  make_generic("verify", G95_ISYM_VERIFY);       
}



        
        
/* convert_constant()-- Master function to convert one constant to
 * another.  While this is used as a simplification function, it
 * requires the destination type and kind information which is
 * supplied by a special case in do_simplify(). */ 
 
static g95_expr *convert_constant(g95_expr *g, bt dtype, int kind) {     
g95_expr *result, *(*k)(g95_expr *, int);  
  
  switch(g->ts.type) {  
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
   
  if (g->type == EXPR_CONSTANT) {         
    result = k(g, kind);   
    if (result == NULL) return &g95_bad_expr;      
  }        
        
  return result; 
}   
   
   
         
         
/* find_conv()-- Given a pair of typespecs, find the g95_intrinsic_sym
 * node that corresponds to the conversion.  Returns NULL if the
 * conversion isn't found. */

static g95_intrinsic_sym *find_conv(g95_typespec *t, g95_typespec *to) {         
g95_intrinsic_sym *sy;     
char *target;     
int h;      
      
  target = conv_name(t, to);  
  sy = conversion;     
     
  for(h=0; h<nconv; h++, sy++)     
    if (strcmp(target, sy->name) == 0) return sy;      
      
  return NULL;    
}       
       
       
        
        
/* g95_intrinsic_name()-- Given a string, figure out if it is the name
 * of an intrinsic subroutine or function.  There are no generic
 * intrinsic subroutines, they are all specific. */ 
 
int g95_intrinsic_name(char *nm, int subroutine_flag) {

  return subroutine_flag ?        
    find_subroutine(nm) != NULL :         
    g95_find_function(nm) != NULL;  
}       
       
       
  
  
/* sort_actual()-- Given an actual arglist and a formal arglist, sort
 * the actual arglist so that its arguments are in a one-to-one
 * correspondence with the format arglist.  Arguments that are not
 * present are given a blank g95_actual_arglist structure.  If
 * something is obviously wrong (say, a missing required argument) we
 * abort sorting and return FAILURE. */         
         
static try sort_actual(char *nam, g95_actual_arglist **ap, 
		       g95_intrinsic_arg *form, locus *old_loc) {   
   
g95_actual_arglist *act, *v;     
g95_intrinsic_arg *s;       
int g;       
       
  remove_nullargs(ap);     
  act = *ap;

  for(s=form; s; s=s->next)          
    s->actual = NULL; 
 
  s = form;       
  v = act; 
 
  if ( s == NULL && v == NULL ) /* No arguments */  
    return SUCCESS;         
         
  for(;;) {     /* Put the nonkeyword arguments in a 1:1 correspondence */       
    if (s == NULL) break;    
    if (v == NULL) goto optional;          
          
    if (v->name[0] != '\0') goto keywords;       
       
    s->actual = v;      
      
    s = s->next;         
    v = v->next;  
  }         
         
  if (v == NULL) goto do_sort;         
         
  g95_error("Too many arguments in call to '%s' at %L", nam, old_loc);    
  return FAILURE;         
         
/* Associate the remaining actual arguments, all of which have to be
 * keyword arguments. */         
         
keywords:       
  for(; v; v=v->next) {     
    for(s=form, g=0; s; s=s->next, g++)       
      if (strcmp(v->name, s->name) == 0) break;        
        
    if (s == NULL) { 
      g95_error("Can't find keyword named '%s' in call to '%s' at %L",    
		v->name, nam, old_loc);
      return FAILURE;        
    }      
      
    if (s->actual != NULL) {          
      g95_error("Argument '%s' is appears twice in call to '%s' at %L",         
		s->name, nam, old_loc);      
      return FAILURE;      
    }

    s->actual = v;      
  }

/* At this point, all unmatched formal args must be optional */         
         
optional:        
  for(s=form; s; s=s->next) {   
    if (s->actual == NULL && s->optional == 0) {     
      g95_error("Missing actual argument '%s' in call to '%s' at %L",        
		s->name, nam, old_loc); 
      return FAILURE;      
    }  
  } 
 
/* Using the formal argument list, string the actual argument list
 * together in a way that corresponds with the formal list. */ 
 
do_sort:  
  act = NULL;         
         
  for(s=form; s; s=s->next) {     
    v = (s->actual == NULL) ? g95_get_actual_arglist() : s->actual;     
     
    if (act == NULL)
      *ap = v;       
    else         
      act->next = v;   
   
    act = v;        
  }         
         
  act->next = NULL;  /* End the sorted argument list. */   
   
  for(v=act, s=form; v; v=v->next, s=s->next)
    if (v->type != ALT_RETURN && v->u.expr == NULL)       
      v->missing_arg_type = s->ts.type;     
           
  return SUCCESS;       
}    
    
    
        
        
/* add_conversions()-- Create g95_intrinsic_sym nodes for all intrinsic
 * conversion functions by looping over the kind tables. */      
      
static void add_conversions(void) {       
int x, h;

  /* Integer-Integer conversions */      
      
  for(x=0; g95_integer_kinds[x].kind != 0; x++)     
    for(h=0; g95_integer_kinds[h].kind != 0; h++) {  
      if (x == h) continue;         
         
      add_conv(BT_INTEGER, g95_integer_kinds[x].kind,       
	       BT_INTEGER, g95_integer_kinds[h].kind, convert_constant); 
    }   
   
  /* Integer-Real/Complex conversions */        
        
  for(x=0; g95_integer_kinds[x].kind != 0; x++)
    for(h=0; g95_real_kinds[h].kind != 0; h++) {       
      add_conv(BT_INTEGER, g95_integer_kinds[x].kind,   
	       BT_REAL,    g95_real_kinds[h].kind, convert_constant);    
    
      add_conv(BT_REAL,    g95_real_kinds[h].kind,  
	       BT_INTEGER, g95_integer_kinds[x].kind, convert_constant);  
  
      add_conv(BT_INTEGER, g95_integer_kinds[x].kind,        
	       BT_COMPLEX, g95_real_kinds[h].kind, convert_constant);     
     
      add_conv(BT_COMPLEX, g95_real_kinds[h].kind,
	       BT_INTEGER, g95_integer_kinds[x].kind, convert_constant);         
    }         
         
  /* Real/Complex - Real/Complex conversions */      
      
  for(x=0; g95_real_kinds[x].kind != 0; x++)    
    for(h=0; g95_real_kinds[h].kind != 0; h++) { 
      if (x != h) { 
	add_conv(BT_REAL, g95_real_kinds[x].kind,         
		 BT_REAL, g95_real_kinds[h].kind, convert_constant);

	add_conv(BT_COMPLEX, g95_real_kinds[x].kind,         
		 BT_COMPLEX, g95_real_kinds[h].kind, convert_constant);    
      }      
      
      add_conv(BT_REAL,    g95_real_kinds[x].kind,      
	       BT_COMPLEX, g95_real_kinds[h].kind, convert_constant); 
 
      add_conv(BT_COMPLEX, g95_real_kinds[x].kind,  
	       BT_REAL,    g95_real_kinds[h].kind, convert_constant);          
    }      
      
  /* Logical/Logical kind conversion */    
    
  for(x=0; g95_logical_kinds[x].kind; x++)      
    for(h=0; g95_logical_kinds[h].kind; h++) {    
      if (x == h) continue; 
 
      add_conv(BT_LOGICAL, g95_logical_kinds[x].kind, 
	       BT_LOGICAL, g95_logical_kinds[h].kind, convert_constant); 
    }  
}          
          
          
         
         
/* g95_convert_type()-- Tries to convert an expression (in place) from
 * one type to another.  The eflag control the behavior on error.
 * The possible values are:
 *   1    Generate a g95_error()
 *   2    Generate a g95_internal_error() */   
   
try g95_convert_type(g95_expr *e, g95_typespec *ts, int eflag) { 
g95_intrinsic_sym *sy;     
g95_typespec from_ts;     
locus old_where;         
g95_expr *new;   
int rnk;          
          
  from_ts = e->ts;        /* expr->ts gets clobbered */  
  
  if (ts->type == BT_UNKNOWN) goto bad;       
       
  if (e->type == EXPR_NULL ||      
      g95_zero_size_array(e)) { /* Sometimes the RHS acquires the type */        
    e->ts = *ts;          
    return SUCCESS; 
  }

  if (e->ts.type == BT_UNKNOWN) goto bad;    
    
  if (g95_compare_types(&e->ts, ts)) return SUCCESS;  
  
  sy = find_conv(&e->ts, ts);       
  if (sy == NULL) goto bad; 
 
/* Insert a pre-resolved function call to the right function */  
  
  old_where = e->where;       
  rnk = e->rank;          
  new = g95_get_expr();        
  *new = *e;  
  
  new = g95_build_funcall(NULL, new, NULL);
  new->value.function.name = sy->lib_name;       
  new->value.function.isym = sy;     
  new->where = old_where;     
  new->rank = rnk; 
 
  *e = *new;      
      
  g95_free(new); 
  e->ts = *ts;    
    
  if (g95_is_constant_expr(e->value.function.actual->u.expr) && 
      do_simplify(sy, e) == FAILURE) {    
    
    if (eflag == 2) goto bad;  
    return FAILURE;   /* Error already generated in do_simplify() */     
  }          
          
  return SUCCESS;   
 
bad:  
  if (eflag == 1) {         
    g95_error("Can't convert %s to %s at %L",
	      g95_typename(&from_ts), g95_typename(ts), &e->where);     
    return FAILURE;       
  }     
     
  g95_internal_error("Can't convert %s to %s at %L",      
		     g95_typename(&from_ts), g95_typename(ts), &e->where);      
      
  return FAILURE;   
}        


/* g95_intrinsic_sub_interface()-- see if a CALL statement corresponds
 * to an intrinsic subroutine.  Returns MATCH_YES if the subroutine
 * corresponds to an intrinsic, MATCH_NO if not, and MATCH_ERROR if
 * there was an error (but did correspond). */        
        
match g95_intrinsic_sub_interface(g95_code *q, int error_flag) {      
g95_intrinsic_sym *isym;     
char *nam;       
       
  if (g95_has_alt_return(q->ext.actual)) return MATCH_NO;       
      
  nam = q->sym->name;      
      
  isym = find_subroutine(nam); 
  if (isym == NULL) return MATCH_NO;

  g95_suppress_error = !error_flag;

  init_arglist(isym); 
 
  if (sort_actual(nam, &q->ext.actual, isym->formal, &q->where) == FAILURE)
    goto fail;    
    
  if (isym->check != NULL) {        
    if (do_check(isym, q->ext.actual) == FAILURE) goto fail;         
  } else {
    if (check_arglist(&q->ext.actual, isym, 1) == FAILURE) goto fail;        
  }

  /* The subroutine corresponds to an intrinsic.  Allow errors to be
   * seen at this point. */  
  
  g95_suppress_error = 0;     
     
  q->isym = isym;  
  if (isym->resolve != NULL)        
    isym->resolve(q);     
  else       
    q->sub_name = isym->lib_name;   
   
  if (g95_pure(NULL) && !isym->elemental) {
    g95_error("Subroutine call to intrinsic '%s' at %L is not PURE", nam,
	      &q->where);       
    return MATCH_ERROR;          
  }  
  
  if (g95_intrinsic_symbol(q->sym, 0)) return MATCH_ERROR;  
  
  return MATCH_YES;       
       
fail:    
  g95_suppress_error = 0;        
  return MATCH_NO;   
}   
   
   
         
         
/* check_specific()-- Given a pointer to an intrinsic symbol and an
 * expression consisting of a function call, see if the function call
 * is consistent with the intrinsic's formal argument list.  Return
 * SUCCESS if the expression and intrinsic match, FAILURE otherwise.  */      
      
static try check_specific(g95_intrinsic_sym *specific, g95_expr *e,       
			  int error_flag) {     
g95_actual_arglist *argum, **actualp;         
int k;   
try a;          
          
  actualp = &e->value.function.actual;    
    
  init_arglist(specific);       
       
/* Don't attempt to sort the argument list for min or max */      
      
  if (specific->check == g95_check_min_max || 
      specific->check == g95_check_min_max_integer ||   
      specific->check == g95_check_min_max_real ||  
      specific->check == g95_check_min_max_double)     
    return (*specific->check)(*actualp);

  if (sort_actual(specific->name, actualp, specific->formal, 
		  &e->where) == FAILURE) return FAILURE;    
    
  /* sort_actual() can get minloc() and maxloc() slightly wrong, so
   * this is fixed in g95_check_minloc_maxloc() */

  if (specific->check == g95_check_minloc_maxloc)  
    a = g95_check_minloc_maxloc(*actualp);        
  else {          
    if (specific->check == NULL) {      
      a = check_arglist(actualp, specific, error_flag);    
      if (a == SUCCESS) e->ts = specific->ts;  
    } else 
      a = do_check(specific, *actualp);    
  }

  /* Check ranks for elemental intrinsics */          
          
  if (a == SUCCESS && specific->elemental) {  
    k = 0; 
    for(argum=e->value.function.actual; argum; argum=argum->next) {          
      if (argum->u.expr == NULL || argum->u.expr->rank == 0) continue;   
      if (k == 0) {          
	k = argum->u.expr->rank;    
	continue;   
      }        
        
      if (argum->u.expr->rank != k) {          
	g95_error("Ranks of arguments to elemental intrinsic '%s' differ "       
		  "at %L", specific->name, &argum->u.expr->where);    
	return FAILURE;   
      }    
    }  
  }          
          
  if (a == FAILURE) remove_nullargs(actualp);     
     
  return a;          
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
    
match g95_intrinsic_func_interface(g95_expr *expr, int error_flag) {      
g95_intrinsic_sym *sym, *specific;          
g95_actual_arglist *real;        
char *n;
int flag;     
     
  if (g95_has_alt_return(expr->value.function.actual)) return MATCH_NO;    
   
  if (expr->value.function.isym != NULL)         
    return (do_simplify(expr->value.function.isym, expr) == FAILURE)   
      ? MATCH_ERROR : MATCH_YES; 
 
  g95_suppress_error = !error_flag;         
  g95_intrinsic_extension = 1;      
  flag = 0;

  for(real=expr->value.function.actual; real; real=real->next)      
    if (real->u.expr != NULL)   
      flag |= (real->u.expr->ts.type != BT_INTEGER &&    
	       real->u.expr->ts.type != BT_CHARACTER);      
      
  n = expr->symbol->name;        
        
  sym = specific = g95_find_function(n);         
  if (sym == NULL) {      
    g95_suppress_error = 0;   
    return MATCH_NO;
  }          
          
  g95_current_intrinsic_where = &expr->where;   
   
/* Bypass the generic list for min and max */

  if (sym->resolve == g95_resolve_min_max) { 
    init_arglist(sym);       
       
    if (g95_check_min_max(expr->value.function.actual) == SUCCESS) {       
      resolve_intrinsic(sym, expr);    
      goto got_specific;      
    }    
    
    g95_suppress_error = 0;        
    return MATCH_NO;          
  }          
          
/* If the function is generic, check all of its specific incarnations.
 * If the generic name is also a specific, we check that name last, so
 * that any error message will correspond to the specific */    
    
  g95_suppress_error = 1;  
  
  if (sym->generic) {       
    for(specific=sym->specific_head; specific; specific=specific->next) {       
      if (specific == sym) continue; 
      if (check_specific(specific, expr, 0) == SUCCESS) goto got_specific;
    }    
  }         
         
  g95_suppress_error = !error_flag;        
        
  if (check_specific(sym, expr, error_flag) == FAILURE) {   
    g95_suppress_error = 0;    
    return MATCH_NO;      
  }        
        
  specific = sym;       
       
got_specific: 
  expr->value.function.isym = specific;     
     
  if (g95_intrinsic_symbol(expr->symbol, 1)) {          
    g95_suppress_error = 0;        
    return MATCH_ERROR;       
  }          
          
  if (do_simplify(specific, expr) == FAILURE) {
    g95_suppress_error = 0;          
    return MATCH_ERROR;          
  } 
 
  flag |= (expr->ts.type != BT_INTEGER && expr->ts.type != BT_CHARACTER);       
       
  if (flag && g95_intrinsic_extension && g95_option.pedantic && g95_init_expr)     
    g95_warning("Evaluation of initialization expression at %L is nonstandard", 
		&expr->where);  
  
  g95_suppress_error = 0;          
  return MATCH_YES; 
}     
     
     
       
       
/* g95_intrinsic_init_1()-- Initialize the table of intrinsics */         
         
void g95_intrinsic_init_1(void) {        
int i;         
         
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
      
  for(i=0; i<nfunc; i++)       
    functions[i].pure = 1;      
      
  for(i=0; i<nsub; i++)          
    subroutines[i].pure = subroutines[i].elemental;  
}


        
        
/* do_simplify()-- Given an intrinsic symbol node and an expression
 * node, call the simplification function (if there is one), perhaps
 * replacing the expression with something simpler.  We return FAILURE
 * on an error of the simplification, SUCCESS if the simplification
 * worked, even if nothing has changed in the expression itself */          
          
static try do_simplify(g95_intrinsic_sym *specific, g95_expr *x) {     
g95_expr *result, *q, *m, *o, *r, *z;      
g95_actual_arglist *args;         
try t;       
       
  if (simplify_elemental_intrinsic(specific, x, &t)) return t;

/* Max and min require special handling due to the variable number of args */         
         
  if (specific->simplify == g95_simplify_min) {   
    result = g95_simplify_min(x);    
    goto finish; 
  }  
  
  if (specific->simplify == g95_simplify_max) {
    result = g95_simplify_max(x); 
    goto finish;      
  }    
    
  if (specific->simplify == NULL) {  
    result = NULL;
    goto finish;   
  }          
          
  args = x->value.function.actual;   
   
  q = args->u.expr;       
  args = args->next;          
          
  if (specific->simplify == convert_constant) {         
    result = convert_constant(q, specific->ts.type, specific->ts.kind);         
    goto finish;          
  }       
       
  /* TODO: Warn if -pedantic and initialization expression and arg
   * types not integer or character */  
  
  if (args == NULL)        
    result = (*specific->simplify)(q);      
  else {     
    m = args->u.expr;     
    args = args->next;       
       
    if (args == NULL) 
      result = (*specific->simplify)(q, m);    
    else {
      o = args->u.expr;   
      args = args->next;   
         
      if (args == NULL) 
	result = (*specific->simplify)(q, m, o);   
      else {     
	r = args->u.expr;       
	args = args->next;          
          
	if (args == NULL)   
	  result = (*specific->simplify)(q, m, o, r);       
  	else {
	  z = args->u.expr;         
	  args = args->next;   
   
	  if (args == NULL)
	    result = (*specific->simplify)(q, m, o, r, z);        
	  else        
	    g95_internal_error("do_simplify(): Too many args for intrinsic");      
	}         
      }
    }   
  }       
       
 finish: 
  if (result == &g95_bad_expr) return FAILURE;        
        
  if (result == NULL) 
    resolve_intrinsic(specific, x);         /* Must call at run-time */      
  else {  
    result->where = x->where;          
    g95_replace_expr(x, result);    
  }     
     
  return SUCCESS;        
}   
   
   
