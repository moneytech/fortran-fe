/* Check functions
   Copyright (C) 2002 Free Software Foundation, Inc.
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
       
       
/* check.c-- These functions check to see if an argument list is
 * compatible with a particular intrinsic function or subroutine.
 * Presence of required arguments has already been established, the
 * argument list has been sorted into the right order and has NULL
 * arguments in the correct places for missing optional arguments.  */     
     
     
#include <stdlib.h>
#include <stdarg.h>
      
#include "g95.h"
#include "intrinsic.h"
  
  
        
        
/* must_be()-- The fundamental complaint function of this source file.
 * This function can be called in all kinds of ways. */        
        
static void must_be(g95_expr *f, int b, char *thing) { 
 
  g95_error("'%s' argument of '%s' intrinsic at %L must be %s",  
	    g95_current_intrinsic_arg[b], g95_current_intrinsic, &f->where,         
	    thing);          
}          
          
          
     
     
/* dim_rank_check()-- If a DIM parameter is a constant, make sure that
 * it is greater than zero and less than the rank of the given
 * array. */    
    
static try dim_rank_check(g95_expr *d, g95_expr *ap) { 
g95_array_spec *a;         
int r;    
    
  if (d->type != EXPR_CONSTANT ||         
      ap->type != EXPR_VARIABLE) return SUCCESS; 
 
  g95_find_array_ref(ap, NULL, &a);
  r = ap->rank;   
  if (a->type == AS_ASSUMED_SIZE) r--;      
      
  if (mpz_cmp_ui(d->value.integer, 1) < 0 ||   
      mpz_cmp_ui(d->value.integer, r) > 0) {  
    g95_error("'dim' argument of '%s' intrinsic at %L is not a valid "        
	      "dimension index", g95_current_intrinsic, &d->where);   
   
    return FAILURE;    
  }  
  
  return SUCCESS;  
}          
          
          
      
      
/* nonoptional_check()-- Make sure a variable expression is not an
 * optional dummy argument */

static try nonoptional_check(g95_expr *m, int b) {          
          
  if (m->type == EXPR_VARIABLE && m->symbol->attr.optional) {
    g95_error("'%s' argument of '%s' intrinsic at %L must not be OPTIONAL",          
	      g95_current_intrinsic_arg[b], g95_current_intrinsic, &m->where);       
    return FAILURE;      
  }   
   
  /* TODO: Recursive check on nonoptional variables? */   
   
  return SUCCESS;         
}        
        
        
          
          
/* same_type_check()-- Make sure two expression have the same type */     
     
static try same_type_check(g95_expr *v, int o, g95_expr *g, int a) {         
char message[100];    
    
  if (g95_compare_types(&v->ts, &g->ts)) return SUCCESS;  
  
  sprintf(message, "the same type and kind as '%s'",  
	  g95_current_intrinsic_arg[o]);  
  
  must_be(g, a, message);    
    
  return FAILURE;     
}     
     
     
     
     
/* numeric_check()-- Check that the expression is a numeric type */    
    
static try numeric_check(g95_expr *l, int m) {          
          
  if (g95_numeric_ts(&l->ts)) return SUCCESS;     
     
  must_be(l, m, "a numeric type");    
    
  return FAILURE; 
}     
     
     
         
         
/* array_check()-- Make sure an expression is an array */    
    
static try array_check(g95_expr *t, int z) {

  if (t->rank != 0) return SUCCESS;        
        
  must_be(t, z, "an array");        
        
  return FAILURE; 
}         
         
         


static try min_max_args(g95_actual_arglist *args) {          
          
  if (args == NULL || args->next == NULL) {        
    g95_error("Intrinsic '%s' at %L must have at least two arguments",      
	      g95_current_intrinsic, g95_current_intrinsic_where);    
    return FAILURE;         
  }         
         
  return SUCCESS;       
}         
         
         
       
       
/* scalar_check()-- Make sure an expression is a scalar */       
       
static try scalar_check(g95_expr *u, int x) { 
 
  if (u->rank == 0) return SUCCESS;          
          
  must_be(u, x, "a scalar");    
    
  return FAILURE; 
}   
   
   
   
   
/* variable_check()-- Make sure an expression is a variable (can be
 * assigned to). */       
       
static try variable_check(g95_expr *p, int v) { 
 
  if (p->type == EXPR_VARIABLE && p->symbol->attr.flavor != FL_PARAMETER)   
    return SUCCESS;   
   
  if (p->type == EXPR_VARIABLE && p->symbol->attr.intent == INTENT_IN) {         
    g95_error("'%s' argument of '%s' intrinsic at %L cannot be INTENT(IN)",        
	      g95_current_intrinsic_arg[v], g95_current_intrinsic, &p->where);       
    return FAILURE;       
  } 
 
  must_be(p, v, "a variable");         
         
  return FAILURE;    
}      
      
      
   
   
try g95_check_associated(g95_expr *pointer, g95_expr *target) {     
symbol_attribute attribute;   
   
  if (pointer->type == EXPR_FUNCTION) {         
    attribute.pointer = (pointer->value.function.isym != NULL)        
      ? 0          
      : pointer->symbol->result->attr.pointer; 
  } else {    
    if (variable_check(pointer, 0) == FAILURE) return FAILURE;        
    attribute = g95_variable_attr(pointer, NULL);    
  }        
        
  if (!attribute.pointer) {      
    must_be(pointer, 0, "a POINTER");       
    return FAILURE;         
  }

  if (target == NULL) return SUCCESS;          
          
  /* Target argument is optional */         
         
  attribute = g95_variable_attr(target, NULL);
  if (!attribute.pointer && !attribute.target) {
    must_be(target, 1, "a POINTER or a TARGET");         
    return FAILURE;        
  }        
        
  return SUCCESS;  
}     
     
     
    
    
/* type_check()-- Check the type of an expression */     
     
static try type_check(g95_expr *c, int m, bt dtype) {       
       
  if (c->ts.type == dtype) return SUCCESS;   
   
  must_be(c, m, g95_basic_typename(dtype)); 
 
  return FAILURE;     
}     
     
          
          
/* g95_check_i()-- Check that the single argument is an integer */         
         
try g95_check_i(g95_expr *i) {    
    
  if (type_check(i, 0, BT_INTEGER) == FAILURE) return FAILURE;     
     
  return SUCCESS;         
} 
 
 
   
   
/* rank_check()-- Make sure that an expression has a certain (nonzero) rank */     
     
static try rank_check(g95_expr *r, int x, int dim) {    
char m[100];   
   
  if (r->rank == dim) return SUCCESS;    
    
  sprintf(m, "of rank %d", dim);

  must_be(r, x, m);     
     
  return FAILURE;  
}      
      
      


try g95_check_abs(g95_expr *q) {         
         
  if (numeric_check(q, 0) == FAILURE) return FAILURE;         
         
  return SUCCESS;    
}          
          
          
      
      
try g95_check_iand(g95_expr *i, g95_expr *o) {    
    
  if (type_check(i, 0, BT_INTEGER) == FAILURE ||     
      type_check(o, 1, BT_INTEGER) == FAILURE) return FAILURE;        
        
  if (same_type_check(i, 0, o, 1) == FAILURE) return FAILURE;       
       
  return SUCCESS;          
}         
         
         
          
          
try g95_check_scale(g95_expr *h, g95_expr *m) {         
         
  if (type_check(h, 0, BT_REAL) == FAILURE) return FAILURE;       
       
  if (type_check(m, 1, BT_INTEGER) == FAILURE) return FAILURE;     
     
  return SUCCESS;      
}        
        
        
         
         
try g95_check_index(g95_expr *str, g95_expr *substring, g95_expr *back) {

  if (type_check(str,    0, BT_CHARACTER) == FAILURE ||        
      type_check(substring, 1, BT_CHARACTER) == FAILURE) return FAILURE;  
  
  
  if (back != NULL && type_check(back, 2, BT_LOGICAL) == FAILURE) 
    return FAILURE;          
          
  if (str->ts.kind != substring->ts.kind) {          
    must_be(substring, 1, "the same kind as 'string'");        
    return FAILURE;          
  }  
  
  return SUCCESS;        
}  
  
  


/* dim_check()-- Check the common DIM parameter for correctness */  
  
static try dim_check(g95_expr *dim, int x, int optional) {          
          
  if (optional) { 
    if (dim == NULL) return SUCCESS;         
         
    if (nonoptional_check(dim, x) == FAILURE) return FAILURE;       
  } else {          
    if (dim == NULL) {      
      g95_error("Missing DIM parameter in intrinsic '%s' at %L",         
		g95_current_intrinsic, g95_current_intrinsic_where);
      return FAILURE;    
    }   
  }   
   
  if (type_check(dim, x, BT_INTEGER) == FAILURE) return FAILURE;

  if (scalar_check(dim, x) == FAILURE) return FAILURE;         
         
  return SUCCESS;        
}  
  
  
     
     
/* double_check()-- Make sure the expression is a double precision real */ 
 
static try double_check(g95_expr *z, int g) {          
          
  if (type_check(z, g, BT_REAL) == FAILURE) return FAILURE;   
   
  if (z->ts.kind != g95_default_double_kind()) {   
    must_be(z, g, "double precision"); 
    return FAILURE;   
  }        
        
  return SUCCESS;     
}    
    
    
   
   
try g95_check_dble(g95_expr *x) {    
    
  if (numeric_check(x, 0) == FAILURE) return FAILURE;     
     
  return SUCCESS;       
}         
         
         
 
 
try g95_check_spread(g95_expr *source, g95_expr *dim, g95_expr *ncopies) {          
          
  if (source->rank >= G95_MAX_DIMENSIONS) {          
    must_be(source, 0, "less than rank " stringize(G95_MAX_DIMENSIONS));  
    return FAILURE;  
  }       
       
  if (dim_check(dim, 1, 0) == FAILURE) return FAILURE;

  if (type_check(ncopies, 2, BT_INTEGER) == FAILURE) return FAILURE;          
  if (scalar_check(ncopies, 2) == FAILURE) return FAILURE;       
       
  return SUCCESS;      
}         
         
         
 
 
/* int_or_real_check()-- Check that an expression is integer or real */         
         
static try int_or_real_check(g95_expr *l, int q) {       
       
  if (l->ts.type != BT_INTEGER && l->ts.type != BT_REAL) {       
    must_be(l, q, "INTEGER or REAL");  
    return FAILURE;          
  }    
    
  return SUCCESS;      
}     
     
     
          
          
try g95_check_matmul(g95_expr *matrix_a, g95_expr *matrix_b) {     
     
  if ((matrix_a->ts.type != BT_LOGICAL) && !g95_numeric_ts(&matrix_b->ts)) {
    must_be(matrix_a, 0, "numeric or LOGICAL");  
    return FAILURE;     
  }        
        
  if ((matrix_b->ts.type != BT_LOGICAL) && !g95_numeric_ts(&matrix_a->ts)) {   
    must_be(matrix_b, 0, "numeric or LOGICAL");        
    return FAILURE;       
  }          
          
  switch(matrix_a->rank) { 
  case 1:   
    if (rank_check(matrix_b, 1, 2) == FAILURE) return FAILURE;     
    break;          
          
  case 2:      
    if (matrix_b->rank == 2) break;  
    if (rank_check(matrix_b, 1, 1) == FAILURE) return FAILURE;   
    break;       
       
  default: 
    must_be(matrix_a, 0, "of rank 1 or 2");       
    return FAILURE;          
  }       
       
  return SUCCESS; 
}      
      
      
          
          
/* Common check function where the first argument must be real or
 * integer and the second argument must be the same as the first. */          
          
try g95_check_a_p(g95_expr *b, g95_expr *e) {

  if (int_or_real_check(b, 0) == FAILURE) return FAILURE;        
        
  if (same_type_check(b, 0, e, 1) == FAILURE) return FAILURE;      
      
  return SUCCESS;   
}  
  
  
        
        
try g95_check_reshape(g95_expr *s1, g95_expr *shap,      
		      g95_expr *pad, g95_expr *order) {  
mpz_t s;      
int t; 
 
  if (array_check(s1, 0) == FAILURE) return FAILURE;        
        
  if (rank_check(shap, 1, 1) == FAILURE) return FAILURE; 
 
  if (type_check(shap, 1, BT_INTEGER) == FAILURE) return FAILURE;       
       
  if (g95_array_size(shap, &s) != SUCCESS) {      
    g95_error("'shape' argument of 'reshape' intrinsic at %L must be an "         
	      "array of constant size", &shap->where);  
    return FAILURE;         
  }   
   
  t = mpz_cmp_ui(s, G95_MAX_DIMENSIONS);    
  mpz_clear(s);

  if (t > 0) {      
    g95_error("'shape' argument of 'reshape' intrinsic at %L has more than "  
	      stringize(G95_MAX_DIMENSIONS) " elements", &shap->where);     
    return FAILURE;
  }        
        
  if (pad != NULL) {  
    if (same_type_check(s1, 0, pad, 2) == FAILURE) return FAILURE; 
    if (array_check(pad, 2) == FAILURE) return FAILURE;       
  }

  if (order != NULL && array_check(order, 3) == FAILURE) return FAILURE;   
   
  return SUCCESS;         
}      
      
      
        
        
try g95_check_date_and_time(g95_expr *date, g95_expr *time,     
			    g95_expr *zone, g95_expr *values) {  
  
  if (date != NULL) {          
    if (type_check(date, 0, BT_CHARACTER) == FAILURE) return FAILURE;       
    if (scalar_check(date, 0) == FAILURE) return FAILURE;        
    if (variable_check(date, 0) == FAILURE) return FAILURE;
  }     
     
  if (time != NULL) {   
    if (type_check(time, 1, BT_CHARACTER) == FAILURE) return FAILURE;        
    if (scalar_check(time, 1) == FAILURE) return FAILURE;          
    if (variable_check(time, 1) == FAILURE) return FAILURE; 
  }  
  
  if (zone != NULL) {       
    if (type_check(zone, 2, BT_CHARACTER) == FAILURE) return FAILURE;      
    if (scalar_check(zone, 2) == FAILURE) return FAILURE;        
    if (variable_check(zone, 2) == FAILURE) return FAILURE;       
  }

  if (values != NULL) {         
    if (type_check(values, 3, BT_INTEGER) == FAILURE) return FAILURE;    
    if (array_check(values, 3) == FAILURE) return FAILURE;          
    if (rank_check(values, 3, 1) == FAILURE) return FAILURE;      
    if (variable_check(values, 3) == FAILURE) return FAILURE;  
  }

  return SUCCESS;     
}   
   
   
       
       
/* logical_array_check()-- Make sure the expression is a logical array */

static try logical_array_check(g95_expr *arr, int c) {

  if (arr->ts.type != BT_LOGICAL || arr->rank == 0) {  
    must_be(arr, c, "a logical array");   
    return FAILURE;     
  }         
         
  return SUCCESS;
}      
      
      
      
      
try g95_check_transfer(g95_expr *s1, g95_expr *mold, g95_expr *sz) {      
      
  if (sz != NULL) {
    if (type_check(sz, 2, BT_INTEGER) == FAILURE) return FAILURE;     
     
    if (scalar_check(sz, 2) == FAILURE) return FAILURE; 
 
    if (nonoptional_check(sz, 2) == FAILURE) return FAILURE;
  }  
  
  return SUCCESS;         
}    
    
    
     
     
try g95_check_btest(g95_expr *c, g95_expr *posit) { 
 
  if (type_check(c, 0, BT_INTEGER) == FAILURE) return FAILURE;         
  if (type_check(posit, 1, BT_INTEGER) == FAILURE) return FAILURE;      
      
  return SUCCESS;      
}        
        
        
   
   
try g95_check_radix(g95_expr *b) {      
      
  if (int_or_real_check(b, 0) == FAILURE) return FAILURE;     
     
  g95_intrinsic_extension = 0;

  return SUCCESS;      
}          
          
          


try g95_check_nearest(g95_expr *t, g95_expr *m) {        
        
  if (type_check(t, 0, BT_REAL) == FAILURE) return FAILURE;        
        
  if (type_check(m, 1, BT_REAL) == FAILURE) return FAILURE;         
         
  return SUCCESS;       
}          
          
          
          
          
try g95_check_all_any(g95_expr *mask, g95_expr *rank) {          
          
  if (logical_array_check(mask, 0) == FAILURE) return FAILURE;

  if (dim_check(rank, 1, 1) == FAILURE) return FAILURE;          
          
  return SUCCESS;     
}          
          
          
         
         
try g95_check_dot_product(g95_expr *vector_a, g95_expr *vector_b) {  
  
  switch(vector_a->ts.type) {     
  case BT_LOGICAL:  
    if (type_check(vector_b, 1, BT_LOGICAL) == FAILURE) return FAILURE;     
    break;   
   
  case BT_INTEGER: 
  case BT_REAL:          
  case BT_COMPLEX:
    if (numeric_check(vector_b, 1) == FAILURE) return FAILURE;    
    break;         
         
  default:  
    must_be(vector_a, 0, "numeric or LOGICAL");      
    return FAILURE;     
  }  
  
  if (rank_check(vector_a, 0, 1) == FAILURE) return FAILURE;      
      
  if (rank_check(vector_b, 1, 1) == FAILURE) return FAILURE;        
        
  return SUCCESS;         
}       
       
       
    
    
try g95_check_precision(g95_expr *k) {       
       
  if (k->ts.type != BT_REAL && k->ts.type != BT_COMPLEX) {          
    must_be(k, 0, "of type REAL or COMPLEX"); 
    return FAILURE;          
  }   
   
  g95_intrinsic_extension = 0;         
         
  return SUCCESS; 
}       
       
       
    
    
/* kind_check()-- that the expression is an optional constant integer
 * and that it specifies a valid kind for that type. */      
      
static try kind_check(g95_expr *y, int e, bt type) {    
int k0;

  if (y == NULL) return SUCCESS;   
   
  if (type_check(y, e, BT_INTEGER) == FAILURE) return FAILURE; 
 
  if (y->type != EXPR_CONSTANT) {   
    must_be(y, e, "a constant");
    return FAILURE;      
  }         
         
  if (g95_extract_int(y, &k0) != NULL ||        
      g95_validate_kind(type, k0) == -1) {     
    g95_error("Invalid kind for %s at %L", g95_basic_typename(type),     
	      &y->where);       
    return FAILURE;
  }          
          
  return SUCCESS;    
} 
 
 
      
      
try g95_check_kind(g95_expr *n) {

  if (n->ts.type == BT_DERIVED) {    
    must_be(n, 0, "a non-derived type");      
    return FAILURE;         
  }      
      
  g95_intrinsic_extension = 0;         
         
  return SUCCESS;    
}   
   
   
       
       
try g95_check_real(g95_expr *a, g95_expr *knd) {     
     
  if (numeric_check(a, 0) == FAILURE) return FAILURE;         
         
  if (kind_check(knd, 1, BT_REAL) == FAILURE) return FAILURE;  
  
  return SUCCESS;        
}


 
 
try g95_check_count(g95_expr *mask, g95_expr *dim) {  
  
  if (logical_array_check(mask, 0) == FAILURE) return FAILURE;        
  if (dim_check(dim, 1, 1) == FAILURE) return FAILURE; 
 
  return SUCCESS;       
}    
    
    
     
     
/* check_a_kind()-- Check subroutine suitable for intrinsics 
 * taking a real argument and a kind argument for the result. */ 
 
static try check_a_kind(g95_expr *h, g95_expr *knd, bt t) { 
 
  if (type_check(h, 0, BT_REAL) == FAILURE) return FAILURE;          
  if (kind_check(knd, 1, t) == FAILURE) return FAILURE;     
     
  return SUCCESS;          
}          
          
          
          
          
try g95_check_selected_real_kind(g95_expr *k, g95_expr *x) {        
        
  if (k == NULL && x == NULL) {       
    g95_error("Missing arguments to %s intrinsic at %L", g95_current_intrinsic,          
	      g95_current_intrinsic_where);       
       
    return FAILURE;        
  }

  if (k != NULL && type_check(k, 0, BT_INTEGER) == FAILURE) return FAILURE;       
       
  if (x != NULL && type_check(x, 1, BT_INTEGER) == FAILURE) return FAILURE;

  g95_intrinsic_extension = 0;        
        
  return SUCCESS;      
}  
  
  
          
          
try g95_check_present(g95_expr *n) {

  if (variable_check(n, 0) == FAILURE) return FAILURE;

  if (!n->symbol->attr.dummy) {         
    must_be(n, 0, "a dummy variable");         
    return FAILURE;          
  }   
   
  if (!n->symbol->attr.optional) {    
    must_be(n, 0, "an OPTIONAL dummy variable");      
    return FAILURE;      
  }      
      
  g95_intrinsic_extension = 0;     
     
  return SUCCESS;    
}   
   
   
   
   
try g95_check_allocated(g95_expr *ap) {

  if (variable_check(ap, 0) == FAILURE) return FAILURE;        
        
  if (array_check(ap, 0) == FAILURE) return FAILURE;          
          
  if (!ap->symbol->attr.allocatable) {   
    must_be(ap, 0, "ALLOCATABLE");          
    return FAILURE;         
  }    
    
  return SUCCESS;     
}  
  
  
      
      
try g95_check_char(g95_expr *p, g95_expr *k) {   
   
  if (type_check(p, 0, BT_INTEGER) == FAILURE) return FAILURE;    
  if (kind_check(k, 1, BT_CHARACTER) == FAILURE) return FAILURE;       
       
  return SUCCESS;    
}   
   
   
  
  
try g95_check_cmplx(g95_expr *f, g95_expr *h, g95_expr *k) {    
    
  if (numeric_check(f, 0) == FAILURE) return FAILURE;       
       
  if (h != NULL) {         
    if (numeric_check(h, 1) == FAILURE) return FAILURE;          
          
    if (f->ts.type == BT_COMPLEX) {   
      must_be(h, 1, "not be present if 'x' is COMPLEX");    
      return FAILURE;      
    }       
  }  
  
  if (kind_check(k, 2, BT_COMPLEX) == FAILURE) return FAILURE;

  return SUCCESS;    
}  
  
  
 
 
try g95_check_digits(g95_expr *e) {          
          
  if (int_or_real_check(e, 0) == FAILURE) return FAILURE;  
  
  g95_intrinsic_extension = 0;  
  
  return SUCCESS;         
}


   
   
try g95_check_random_number(g95_expr *harvest) {       
       
  if (type_check(harvest, 0, BT_REAL) == FAILURE) return FAILURE;       
       
  if (variable_check(harvest, 0) == FAILURE) return FAILURE;          
          
  return SUCCESS;        
}    
    
    


try g95_check_eoshift(g95_expr *array, g95_expr *shift, g95_expr *boundary,      
		      g95_expr *d) {     
     
  if (array_check(array, 0) == FAILURE) return FAILURE; 
 
  if (type_check(shift, 1, BT_INTEGER) == FAILURE) return FAILURE;         
         
  if (shift->rank != 0 && shift->rank != array->rank - 1) {     
    g95_error("Bad rank for SHIFT parameter of EOSHIFT intrinsic at %L",          
	      &shift->where);    
    return FAILURE;      
  }        
        
  if (boundary != NULL) {
    if (same_type_check(array, 0, boundary, 2) == FAILURE) return FAILURE;          
  } else {
    if (array->ts.type == BT_DERIVED) {      
      g95_error("Derived-type EOSHIFT requires the BOUNDARY parameter at %L",        
		&array->where);          
      return FAILURE;       
    }      
  }          
          
  if (dim_check(d, 3, 1) == FAILURE) return FAILURE;      
      
  return SUCCESS;   
}       
       
       


/* g95_check_minloc_maxloc()-- Whoever came up with this interface was
 * probably on something.  If the (array, mask) form is given,
 * sort_actual() will have 'mask' in the 'dim' position. */        
        
try g95_check_minloc_maxloc(g95_actual_arglist *y) { 
g95_expr *ap, *m, *d;   
   
  ap = y->u.expr;          
  if (int_or_real_check(ap, 0) == FAILURE ||      
      array_check(ap, 0) == FAILURE) 
    return FAILURE;  
  
  d = y->next->u.expr;        
  m = y->next->next->u.expr;     
     
  if (m == NULL && d != NULL && d->ts.type == BT_LOGICAL &&   
      y->next->name[0] == '\0') {
    m = d;
    d = NULL;         
         
    y->next->u.expr = NULL;     
    y->next->next->u.expr = m;       
  }        
        
  if (d != NULL &&
      (scalar_check(d, 1) == FAILURE ||     
       type_check(d, 1, BT_INTEGER) == FAILURE))   
    return FAILURE;  
  
  if (m != NULL && type_check(m, 2, BT_LOGICAL) == FAILURE)     
    return FAILURE; 
     
  return SUCCESS;    
}         
         
         


try g95_check_ubound(g95_expr *ap, g95_expr *d) {  
  
  if (array_check(ap, 0) == FAILURE) return FAILURE;  
  
  if (dim_check(d, 1, 1) == FAILURE) return FAILURE;

  return SUCCESS;         
}


   
   
/* g95_check_x_ni()-- Common check functions for numeric inquiry
 * functions that have a single real argument. */   
   
try g95_check_x_ni(g95_expr *t) {        
        
  if (type_check(t, 0, BT_REAL) == FAILURE) return FAILURE;     
     
  g95_intrinsic_extension = 0;         
         
  return SUCCESS;       
}          
          
          
      
      
static try check_rest(bt dtype, int k, g95_actual_arglist *argum) {        
g95_expr *p;       
int m;  
  
  if (min_max_args(argum) == FAILURE) return FAILURE;          
         
  m = 1;   
   
  for(; argum; argum=argum->next, m++) {          
    p = argum->u.expr;
    if (p->ts.type != dtype || p->ts.kind != k) { 
      g95_error("'a%d' argument of '%s' intrinsic at %L must be %s(%d)", 
		m, g95_current_intrinsic, &p->where,  
		g95_basic_typename(dtype), k);         
      return FAILURE;       
    }   
  }      
      
  return SUCCESS;          
}    
    
    
    
    
try g95_check_idnint(g95_expr *f) {  
  
  if (double_check(f, 0) == FAILURE) return FAILURE;  
  
  return SUCCESS;    
}      
      
      
          
          
try g95_check_ieor(g95_expr *k, g95_expr *h) {    
    
  if (type_check(k, 0, BT_INTEGER) == FAILURE ||        
      type_check(h, 1, BT_INTEGER) == FAILURE) return FAILURE;        
        
  if (same_type_check(k, 0, h, 1) == FAILURE) return FAILURE;   
   
  return SUCCESS;       
}


   
   
try g95_check_verify(g95_expr *a, g95_expr *g, g95_expr *i) {  
  
  if (type_check(a, 0, BT_CHARACTER) == FAILURE) return FAILURE;

  if (same_type_check(a, 0, g, 1) == FAILURE) return FAILURE;          
          
  if (i != NULL && type_check(i, 2, BT_LOGICAL) == FAILURE) return FAILURE;     
     
  return SUCCESS; 
}  
  
  
  
  
try g95_check_min_max_integer(g95_actual_arglist *ap) {    
    
  return check_rest(BT_INTEGER, g95_default_integer_kind(), ap);          
} 
 
 


try g95_check_sum(g95_expr *block, g95_expr *r, g95_expr *m) {          
          
  if (array_check(block, 0) == FAILURE) return FAILURE;        
        
  if (numeric_check(block, 0) == FAILURE) return FAILURE;

  if (dim_check(r, 1, 1) == FAILURE) return FAILURE;    
    
  if (m != NULL && logical_array_check(m, 2) == FAILURE) return FAILURE;  
  
  return SUCCESS;       
}  
  
  
       
       
try g95_check_cpu_time(g95_expr *time) {    
    
  if (scalar_check(time, 0) == FAILURE) return FAILURE;    
    
  if (type_check(time, 0, BT_REAL) == FAILURE) return FAILURE;   
   
  if (variable_check(time, 0) == FAILURE) return FAILURE;    
    
  return SUCCESS;    
}


   
   
try g95_check_cshift(g95_expr *a, g95_expr *shift, g95_expr *r) {  
  
  if (array_check(a, 0) == FAILURE) return FAILURE;

  if (a->rank == 1) {      
    if (scalar_check(shift, 1) == FAILURE) return FAILURE;
  } else {     
    /* TODO: more requirements on shift parameter */     
  }        
        
  if (dim_check(r, 2, 1) == FAILURE) return FAILURE;     
     
  return SUCCESS; 
} 
 
 
         
         
try g95_check_lbound(g95_expr *array, g95_expr *d) {          
          
  if (array_check(array, 0) == FAILURE) return FAILURE;    
  if (dim_check(d, 1, 1) == FAILURE) return FAILURE;      
      
  return SUCCESS;     
}      
      
      
        
        
/* g95_check_a_ikind()-- Check subroutine suitable for ceiling,
 * floor and nint. */       
       
try g95_check_a_ikind(g95_expr *t, g95_expr *k0) {     
     
  return check_a_kind(t, k0, BT_INTEGER);      
}          
          
          
 
 
try g95_check_unpack(g95_expr *vector, g95_expr *mask, g95_expr *field) {        
        
  if (rank_check(vector, 0, 1) == FAILURE) return FAILURE;          
          
  if (array_check(mask, 1) == FAILURE) return FAILURE;  
  
  if (type_check(mask, 1, BT_LOGICAL) == FAILURE) return FAILURE;

  if (same_type_check(vector, 0, field, 2) == FAILURE) return FAILURE;    
    
  return SUCCESS;     
}  
  
  
     
     
try g95_check_set_exponent(g95_expr *a, g95_expr *v) {         
         
  if (type_check(a, 0, BT_REAL) == FAILURE) return FAILURE;

  if (type_check(v, 1, BT_INTEGER) == FAILURE) return FAILURE;        
        
  return SUCCESS;        
}


   
   
try g95_check_dcmplx(g95_expr *e, g95_expr *s) { 
 
  if (numeric_check(e, 0) == FAILURE) return FAILURE;  
  
  if (s != NULL) { 
    if (numeric_check(s, 1) == FAILURE) return FAILURE;      
      
    if (e->ts.type == BT_COMPLEX) { 
      must_be(s, 1, "not be present if 'x' is COMPLEX");         
      return FAILURE;       
    }  
  }        
        
  return SUCCESS; 
}   
   
   
         
         
try g95_check_min_max(g95_actual_arglist *arg) {
g95_expr *b;       
       
  if (min_max_args(arg) == FAILURE) return FAILURE;       
      
  b = arg->u.expr;    
    
  if (b->ts.type != BT_INTEGER && b->ts.type != BT_REAL) {   
    g95_error("'a1' argument of '%s' intrinsic at %L must be INTEGER or REAL",    
	      g95_current_intrinsic, &b->where);  
    return FAILURE; 
  }  
  
  return check_rest(b->ts.type, b->ts.kind, arg);          
}


  
  
try g95_check_ior(g95_expr *f, g95_expr *k) {    
    
  if (type_check(f, 0, BT_INTEGER) == FAILURE ||       
      type_check(k, 1, BT_INTEGER) == FAILURE) return FAILURE;   
   
  if (same_type_check(f, 0, k, 1) == FAILURE) return FAILURE;         
         
  return SUCCESS;        
}         
         
         


/* kind_value_check()-- Check that an expression has a particular kind */     
     
static try kind_value_check(g95_expr *m, int f, int s) {    
char msg[100];          
          
  if (m->ts.kind == s) return SUCCESS; 
 
  sprintf(msg, "of kind %d", s); 
 
  must_be(m, f, msg);     
  return FAILURE;      
}


         
         
try g95_check_min_max_double(g95_actual_arglist *a) {     
     
  return check_rest(BT_REAL, g95_default_double_kind(), a);      
}        
        
        
          
          
try g95_check_huge(g95_expr *q) {    
    
  if (int_or_real_check(q, 0) == FAILURE) return FAILURE;       
       
  g95_intrinsic_extension = 0;          
          
  return SUCCESS;         
}  
  
  
        
        
/* g95_check_x()-- Common check function for the half a dozen
 * intrinsics that have a single real argument */   
   
try g95_check_x(g95_expr *o) {    
    
  if (type_check(o, 0, BT_REAL) == FAILURE) return FAILURE;       
       
  return SUCCESS;      
}   
   
   
          
          
try g95_check_sign(g95_expr *x, g95_expr *e) { 
 
  if (int_or_real_check(x, 0) == FAILURE) return FAILURE;        
        
  if (same_type_check(x, 0, e, 1) == FAILURE) return FAILURE;     
     
  return SUCCESS; 
} 
 
 
    
    
try g95_check_min_max_real(g95_actual_arglist *argum) {

  return check_rest(BT_REAL, g95_default_real_kind(), argum);     
}    
    
    
       
       
try g95_check_null(g95_expr *mold) {
symbol_attribute a;

  if (mold == NULL) return SUCCESS;         
         
  if (variable_check(mold, 0) == FAILURE) return FAILURE;  
  
  a = g95_variable_attr(mold, NULL);     
     
  if (!a.pointer) {
    must_be(mold, 0, "a POINTER"); 
    return FAILURE;   
  }   
   
  return SUCCESS;       
}


 
 
try g95_check_int(g95_expr *p, g95_expr *knd) {

  if (numeric_check(p, 0) == FAILURE ||      
      kind_check(knd, 1, BT_INTEGER) == FAILURE) return FAILURE;         
         
  return SUCCESS;   
}      
      
      
  
  
try g95_check_transpose(g95_expr *matrix) {

  if (rank_check(matrix, 0, 2) == FAILURE) return FAILURE;       
       
  return SUCCESS;         
}   
   
   


try g95_check_logical(g95_expr *f, g95_expr *kind) {   
   
  if (type_check(f, 0, BT_LOGICAL) == FAILURE) return FAILURE;        
  if (kind_check(kind, 1, BT_LOGICAL) == FAILURE) return FAILURE;        
        
  return SUCCESS;    
}       
       
       
  
  
try g95_check_merge(g95_expr *tsource, g95_expr *fsource, g95_expr *mask) {

  if (same_type_check(tsource, 0, fsource, 1) == FAILURE) return FAILURE;         
         
  if (type_check(mask, 2, BT_LOGICAL) == FAILURE) return FAILURE;  
  
  return SUCCESS;       
}    
    
    
     
     
try g95_check_shape(g95_expr *s) { 
g95_array_spec *as;    
    
  if (s->rank == 0 || s->type != EXPR_VARIABLE) return SUCCESS;    
    
  g95_find_array_ref(s, NULL, &as);

  if (as->type == AS_ASSUMED_SIZE) {      
    g95_error("'source' argument of 'shape' intrinsic at %L must not be " 
	      "an assumed size array", &s->where);        
    return FAILURE;   
  }   
   
  return SUCCESS; 
}         
         
         
        
        
try g95_check_minval_maxval(g95_expr *arr, g95_expr *r, g95_expr *m) {       
       
  if (array_check(arr, 0) == FAILURE) return FAILURE;

  if (int_or_real_check(arr, 0) == FAILURE) return FAILURE;       
       
  if (dim_check(r, 1, 1) == FAILURE) return FAILURE; 
 
  if (m != NULL && logical_array_check(m, 2) == FAILURE) return FAILURE;          
          
  return SUCCESS; 
}        
        
        
   
   
try g95_check_ibset(g95_expr *c, g95_expr *posit) {     
     
  if (type_check(c,   0, BT_INTEGER) == FAILURE || 
      type_check(posit, 1, BT_INTEGER) == FAILURE ||  
      kind_value_check(posit, 1, g95_default_integer_kind()) == FAILURE)       
    return FAILURE;   
   
  return SUCCESS;       
}     
     
     
   
   
try g95_check_ibits(g95_expr *f, g95_expr *p, g95_expr *l) {     
     
  if (type_check(f,   0, BT_INTEGER) == FAILURE ||
      type_check(p, 1, BT_INTEGER) == FAILURE ||  
      kind_value_check(p, 1, g95_default_integer_kind()) == FAILURE ||          
      type_check(l, 2, BT_INTEGER) == FAILURE)          
    return FAILURE;          
          
  return SUCCESS;
}  
  
  
      
      
try g95_check_scan(g95_expr *j, g95_expr *r, g95_expr *z) {  
  
  if (type_check(j, 0, BT_CHARACTER) == FAILURE) return FAILURE;     
     
  if (type_check(r, 1, BT_CHARACTER) == FAILURE) return FAILURE;  
  
  if (z != NULL && type_check(z, 2, BT_LOGICAL) == FAILURE) return FAILURE;          
          
  if (same_type_check(j, 0, r, 1) == FAILURE) return FAILURE;         
         
  return SUCCESS; 
}


 
 
try g95_check_random_seed(g95_expr *size, g95_expr *put, g95_expr *get) {        
        
  if (size != NULL) {
    if (scalar_check(size, 0) == FAILURE) return FAILURE;     
     
    if (type_check(size, 0, BT_INTEGER) == FAILURE) return FAILURE;     
     
    if (variable_check(size, 0) == FAILURE) return FAILURE;     
     
    if (kind_value_check(size, 0, g95_default_integer_kind()) == FAILURE)      
      return FAILURE;      
  }      
      
  if (put != NULL) {        
    if (array_check(put, 1) == FAILURE) return FAILURE;
    if (rank_check(put, 1, 1) == FAILURE) return FAILURE;     
     
    if (type_check(put, 1, BT_INTEGER) == FAILURE) return FAILURE;

    if (kind_value_check(put, 1, g95_default_integer_kind()) == FAILURE) 
      return FAILURE;          
  }        
        
  if (get != NULL) {         
    if (array_check(get, 2) == FAILURE) return FAILURE;   
    if (rank_check(get, 2, 1) == FAILURE) return FAILURE;       
       
    if (type_check(get, 2, BT_INTEGER) == FAILURE) return FAILURE;   
   
    if (variable_check(get, 2) == FAILURE) return FAILURE;   
   
    if (kind_value_check(get, 2, g95_default_integer_kind()) == FAILURE)     
      return FAILURE;    
  } 
 
  return SUCCESS;        
}         
         
         
try g95_check_ishftc(g95_expr *q, g95_expr *shift, g95_expr *sz) { 
 
  if (type_check(q, 0, BT_INTEGER) == FAILURE ||       
      type_check(shift, 1, BT_INTEGER) == FAILURE) return FAILURE;       
       
  if (sz != NULL &&       
      type_check(sz, 2, BT_INTEGER) == FAILURE) return FAILURE; 
 
  return SUCCESS;   
}   
   
   
  
  
try g95_check_size(g95_expr *array, g95_expr *rank) {      
      
  if (array_check(array, 0) == FAILURE) return FAILURE;          
          
  if (rank != NULL) {         
    if (type_check(rank, 1, BT_INTEGER) == FAILURE) return FAILURE;

    if (kind_value_check(rank, 1, g95_default_integer_kind()) == FAILURE)        
      return FAILURE;    
    
    if (dim_rank_check(rank, array) == FAILURE) return FAILURE;      
  }        
        
  return SUCCESS;        
}       
       
       
 
 
try g95_check_mvbits(g95_expr *frm, g95_expr *frompos, g95_expr *leng,   
		     g95_expr *dest, g95_expr *topos) {

  if (type_check(frm, 0, BT_INTEGER) == FAILURE) return FAILURE; 
 
  if (type_check(frompos, 1, BT_INTEGER) == FAILURE) return FAILURE;   
   
  if (type_check(leng, 2, BT_INTEGER) == FAILURE) return FAILURE; 
 
  if (same_type_check(frm, 0, dest, 3) == FAILURE) return FAILURE;         
         
  if (variable_check(dest, 3) == FAILURE) return FAILURE;   
   
  if (type_check(topos, 4, BT_INTEGER) == FAILURE) return FAILURE;  
  
  return SUCCESS;       
}    
    
    
      
      
try g95_check_product(g95_expr *arr, g95_expr *d, g95_expr *mask) { 
 
  if (array_check(arr, 0) == FAILURE) return FAILURE;       
       
  if (numeric_check(arr, 0) == FAILURE) return FAILURE;

  if (dim_check(d, 1, 1) == FAILURE) return FAILURE;        
        
  if (mask != NULL && logical_array_check(mask, 2) == FAILURE) return FAILURE;

  return SUCCESS; 
}     
     
     
         
         
try g95_check_repeat(g95_expr *t, g95_expr *z) { 
 
  if (type_check(t, 0, BT_CHARACTER) == FAILURE) return FAILURE;       
       
  if (type_check(z, 0, BT_INTEGER) == FAILURE) return FAILURE;          
          
  return SUCCESS;
}        
        
        
   
   
try g95_check_ishft(g95_expr *o, g95_expr *shift) {

  if (type_check(o, 0, BT_INTEGER) == FAILURE ||
      type_check(shift, 1, BT_INTEGER) == FAILURE) return FAILURE;          
          
  return SUCCESS;       
}   
   
   
   
   
try g95_check_ibclr(g95_expr *r, g95_expr *posit) {        
        
  if (type_check(r,   0, BT_INTEGER) == FAILURE ||       
      type_check(posit, 1, BT_INTEGER) == FAILURE ||  
      kind_value_check(posit, 1, g95_default_integer_kind()) == FAILURE)    
    return FAILURE; 
 
  return SUCCESS;    
}


 
 
try g95_check_range(g95_expr *h) { 
 
  if (numeric_check(h, 0) == FAILURE) return FAILURE;      
      
  g95_intrinsic_extension = 0;        
        
  return SUCCESS;   
}      
      
      
    
    
/* g95_check_a_xkind()-- Check subroutine suitable for aint,
 * anint. */          
          
try g95_check_a_xkind(g95_expr *y, g95_expr *k) { 
 
  return check_a_kind(y, k, BT_REAL);     
}        
        
        
 
 
try g95_check_pack(g95_expr *ap, g95_expr *mask, g95_expr *vector) {    
    
  if (array_check(ap, 0) == FAILURE) return FAILURE;    
    
  if (type_check(mask, 1, BT_LOGICAL) == FAILURE) return FAILURE;      
      
  if (mask->rank != 0 && mask->rank != ap->rank) {     
    must_be(ap, 0, "conformable with 'mask' argument");
    return FAILURE;   
  }       
       
  if (vector != NULL) {      
    if (same_type_check(ap, 0, vector, 2) == FAILURE) return FAILURE;  
  
    if (rank_check(vector, 2, 1) == FAILURE) return FAILURE;     
     
    /* TODO: More constraints here */         
  }      
      
  return SUCCESS; 
}          
          
          
