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
     
static void must_be(g95_expr *z, int g, char *thing) {   
   
  g95_error("'%s' argument of '%s' intrinsic at %L must be %s",    
	    g95_current_intrinsic_arg[g], g95_current_intrinsic, &z->where,       
	    thing);   
}    
    
    
   
   
/* int_or_real_check()-- Check that an expression is integer or real */          
          
static try int_or_real_check(g95_expr *j, int n) {

  if (j->ts.type != BT_INTEGER && j->ts.type != BT_REAL) {     
    must_be(j, n, "INTEGER or REAL");       
    return FAILURE;       
  }   
   
  return SUCCESS;      
}         
         
         
    
    
/* logical_array_check()-- Make sure the expression is a logical array */    
    
static try logical_array_check(g95_expr *ap, int z) {      
      
  if (ap->ts.type != BT_LOGICAL || ap->rank == 0) {   
    must_be(ap, 0, "a logical array");   
    return FAILURE;      
  }         
         
  return SUCCESS;  
}        
        
        
     
     
/* type_check()-- Check the type of an expression */   
   
static try type_check(g95_expr *b, int v, bt typ) {    
    
  if (b->ts.type == typ) return SUCCESS;         
         
  must_be(b, v, g95_basic_typename(typ));     
     
  return FAILURE;         
}   
   
          
          
/* kind_value_check()-- Check that an expression has a particular kind */  
  
static try kind_value_check(g95_expr *x, int a, int g) {     
char message[100];  
  
  if (x->ts.kind == g) return SUCCESS; 
 
  sprintf(message, "of kind %d", g);         
         
  must_be(x, a, message);    
  return FAILURE;       
}        
        
        


/* double_check()-- Make sure the expression is a double precision real */  
  
static try double_check(g95_expr *c, int q) { 
 
  if (type_check(c, q, BT_REAL) == FAILURE) return FAILURE;       
       
  if (c->ts.kind != g95_default_double_kind()) {        
    must_be(c, q, "double precision");    
    return FAILURE;       
  }     
     
  return SUCCESS;
}        
        
        
       
       
/* scalar_check()-- Make sure an expression is a scalar */

static try scalar_check(g95_expr *u, int t) {

  if (u->rank == 0) return SUCCESS;   
   
  must_be(u, t, "a scalar");  
  
  return FAILURE;
}    
    
    
    
    
/* nonoptional_check()-- Make sure a variable expression is not an
 * optional dummy argument */      
      
static try nonoptional_check(g95_expr *x, int c) {       
       
  if (x->type == EXPR_VARIABLE && x->symbol->attr.optional) {   
    g95_error("'%s' argument of '%s' intrinsic at %L must not be OPTIONAL",     
	      g95_current_intrinsic_arg[c], g95_current_intrinsic, &x->where); 
 
  }  
  
  /* TODO: Recursive check on nonoptional variables? */ 
 
  return SUCCESS;    
}      
      
      


try g95_check_digits(g95_expr *w) {      
      
  if (int_or_real_check(w, 0) == FAILURE) return FAILURE;  
  
  g95_intrinsic_extension = 0;    
    
  return SUCCESS;   
}    
    
    
    
    
/* rank_check()-- Make sure that an expression has a certain (nonzero) rank */  
  
static try rank_check(g95_expr *j, int f, int rnk) {          
char mesg[100]; 
 
  if (j->rank == rnk) return SUCCESS;          
          
  sprintf(mesg, "of rank %d", rnk);    
    
  must_be(j, f, mesg);   
   
  return FAILURE;   
}       
       
       
          
          
/* g95_check_x()-- Common check function for the half a dozen
 * intrinsics that have a single real argument */     
     
try g95_check_x(g95_expr *g) {

  if (type_check(g, 0, BT_REAL) == FAILURE) return FAILURE;         
         
  return SUCCESS;        
}    
    
    
       
       
/* same_type_check()-- Make sure two expression have the same type */         
         
static try same_type_check(g95_expr *e, int y, g95_expr *b, int x) {     
char message[100];

  if (g95_compare_types(&e->ts, &b->ts)) return SUCCESS;    
    
  sprintf(message, "the same type and kind as '%s'",
	  g95_current_intrinsic_arg[y]);    
    
  must_be(b, x, message);

  return FAILURE;         
}       
       
       
     
     
try g95_check_ibset(g95_expr *z, g95_expr *posit) { 
 
  if (type_check(z,   0, BT_INTEGER) == FAILURE ||  
      type_check(posit, 1, BT_INTEGER) == FAILURE ||  
      kind_value_check(posit, 1, g95_default_integer_kind()) == FAILURE)   
    return FAILURE;          
          
  return SUCCESS;    
}          
          
          
       
       
try g95_check_index(g95_expr *string, g95_expr *substring, g95_expr *back) {

  if (type_check(string,    0, BT_CHARACTER) == FAILURE || 
      type_check(substring, 1, BT_CHARACTER) == FAILURE) return FAILURE;


  if (back != NULL && type_check(back, 2, BT_LOGICAL) == FAILURE)        
    return FAILURE;     
     
  if (string->ts.kind != substring->ts.kind) { 
    must_be(substring, 1, "the same kind as 'string'");         
    return FAILURE;    
  }   
   
  return SUCCESS;       
}     
     
     
    
    
/* array_check()-- Make sure an expression is an array */  
  
static try array_check(g95_expr *u, int q) {       
       
  if (u->rank != 0) return SUCCESS;        
        
  must_be(u, q, "an array");   
   
  return FAILURE;  
}   
   
   
  
  
/* dim_rank_check()-- If a DIM parameter is a constant, make sure that
 * it is greater than zero and less than the rank of the given
 * array. */        
        
static try dim_rank_check(g95_expr *r, g95_expr *block) {        
g95_array_ref *ar;
int rnk; 
 
  if (r->type != EXPR_CONSTANT ||         
      block->type != EXPR_VARIABLE) return SUCCESS;  
  
  ar = g95_find_array_ref(block);       
  rnk = block->rank;        
  if (ar->as->type == AS_ASSUMED_SIZE) rnk--;        
        
  if (mpz_cmp_ui(r->value.integer, 1) < 0 ||  
      mpz_cmp_ui(r->value.integer, rnk) > 0) {   
    g95_error("'dim' argument of '%s' intrinsic at %L is not a valid "        
	      "dimension index", g95_current_intrinsic, &r->where);   
   
    return FAILURE;          
  }      
      
  return SUCCESS;        
}       
       
       


/* numeric_check()-- Check that the expression is a numeric type */

static try numeric_check(g95_expr *h, int q) {

  if (g95_numeric_ts(&h->ts)) return SUCCESS; 
 
  must_be(h, q, "a numeric type");          
          
  return FAILURE;         
}


   
   
try g95_check_ieor(g95_expr *u, g95_expr *v) {   
   
  if (type_check(u, 0, BT_INTEGER) == FAILURE ||        
      type_check(v, 1, BT_INTEGER) == FAILURE) return FAILURE;        
        
  if (same_type_check(u, 0, v, 1) == FAILURE) return FAILURE;      
      
  return SUCCESS;      
}       
       
       


try g95_check_nearest(g95_expr *x, g95_expr *s) {    
    
  if (type_check(x, 0, BT_REAL) == FAILURE) return FAILURE;   
   
  if (type_check(s, 1, BT_REAL) == FAILURE) return FAILURE; 
 
  return SUCCESS;          
}


     
     
try g95_check_transpose(g95_expr *matrix) {       
       
  if (rank_check(matrix, 0, 2) == FAILURE) return FAILURE;        
        
  return SUCCESS; 
}


  
  
/* variable_check()-- Make sure an expression is a variable. */          
          
static try variable_check(g95_expr *w, int p) {   
   
  if ((w->type == EXPR_VARIABLE &&      
       w->symbol->attr.flavor != FL_PARAMETER) || 
      (w->type == EXPR_FUNCTION && w->symbol->result == w->symbol))         
    return SUCCESS;        
        
  if (w->type == EXPR_VARIABLE && w->symbol->attr.intent == INTENT_IN) {         
    g95_error("'%s' argument of '%s' intrinsic at %L cannot be INTENT(IN)", 
	      g95_current_intrinsic_arg[p], g95_current_intrinsic, &w->where);    
    return FAILURE;      
  }  
  
  must_be(w, p, "a variable");  
  
  return FAILURE;         
}        
        
        
  
  
try g95_check_kind(g95_expr *h) {          
          
  if (h->ts.type == BT_DERIVED) {         
    must_be(h, 0, "a non-derived type");  
    return FAILURE;    
  }

  g95_intrinsic_extension = 0;      
      
  return SUCCESS;     
}          
          
          
         
         
/* kind_check()-- that the expression is an optional constant integer
 * and that it specifies a valid kind for that type. */   
   
static try kind_check(g95_expr *q, int m, bt typ) {     
int kind;      
      
  if (q == NULL) return SUCCESS;

  if (type_check(q, m, BT_INTEGER) == FAILURE) return FAILURE;      
      
  if (q->type != EXPR_CONSTANT) {  
    must_be(q, m, "a constant");          
    return FAILURE;
  }      
      
  if (g95_extract_int(q, &kind) != NULL ||        
      g95_validate_kind(typ, kind) == -1) { 
    g95_error("Invalid kind for %s at %L", g95_basic_typename(typ),     
	      &q->where);       
    return FAILURE;    
  }          
          
  return SUCCESS;     
}       
       
       
     
     
/* check_a_kind()-- Check subroutine suitable for intrinsics 
 * taking a real argument and a kind argument for the result. */       
       
static try check_a_kind(g95_expr *o, g95_expr *k0, bt dtype) {          
          
  if (type_check(o, 0, BT_REAL) == FAILURE) return FAILURE;     
  if (kind_check(k0, 1, dtype) == FAILURE) return FAILURE;

  return SUCCESS;    
}   
   
   
 
 
/* g95_check_x_ni()-- Common check functions for numeric inquiry
 * functions that have a single real argument. */        
        
try g95_check_x_ni(g95_expr *k) {         
         
  if (type_check(k, 0, BT_REAL) == FAILURE) return FAILURE;    
    
  g95_intrinsic_extension = 0;

  return SUCCESS;       
}


          
          
try g95_check_btest(g95_expr *s, g95_expr *posit) { 
 
  if (type_check(s, 0, BT_INTEGER) == FAILURE) return FAILURE;     
  if (type_check(posit, 1, BT_INTEGER) == FAILURE) return FAILURE;     
     
  return SUCCESS; 
}    
    
    
   
   
/* g95_check_a_ikind()-- Check subroutine suitable for ceiling,
 * floor and nint. */        
        
try g95_check_a_ikind(g95_expr *k, g95_expr *k0) {  
  
  return check_a_kind(k, k0, BT_INTEGER);
} 
 
 
   
   
try g95_check_scale(g95_expr *l, g95_expr *b) {    
    
  if (type_check(l, 0, BT_REAL) == FAILURE) return FAILURE;

  if (type_check(b, 1, BT_INTEGER) == FAILURE) return FAILURE;   
   
  return SUCCESS;  
}          
          
          
        
        
try g95_check_ishft(g95_expr *d, g95_expr *shift) {     
     
  if (type_check(d, 0, BT_INTEGER) == FAILURE ||    
      type_check(shift, 1, BT_INTEGER) == FAILURE) return FAILURE;          
          
  return SUCCESS;       
}  
  
  
     
     
try g95_check_dcmplx(g95_expr *l, g95_expr *k) {       
       
  if (numeric_check(l, 0) == FAILURE) return FAILURE;        
        
  if (k != NULL) {       
    if (numeric_check(k, 1) == FAILURE) return FAILURE;

    if (l->ts.type == BT_COMPLEX) {   
      must_be(k, 1, "not be present if 'x' is COMPLEX"); 
      return FAILURE;
    }     
  }     
     
  return SUCCESS;     
}          
          
          
   
   
try g95_check_precision(g95_expr *h) {        
        
  if (h->ts.type != BT_REAL && h->ts.type != BT_COMPLEX) {     
    must_be(h, 0, "of type REAL or COMPLEX");      
    return FAILURE;    
  }          
          
  g95_intrinsic_extension = 0;  
  
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
          
          
        
        
try g95_check_cpu_time(g95_expr *time) {          
          
  if (scalar_check(time, 0) == FAILURE) return FAILURE;     
     
  if (type_check(time, 0, BT_REAL) == FAILURE) return FAILURE;        
        
  if (variable_check(time, 0) == FAILURE) return FAILURE; 
 
  return SUCCESS;   
}        
        
        
  
  
try g95_check_ibits(g95_expr *o, g95_expr *position, g95_expr *leng) { 
 
  if (type_check(o,   0, BT_INTEGER) == FAILURE ||     
      type_check(position, 1, BT_INTEGER) == FAILURE ||  
      kind_value_check(position, 1, g95_default_integer_kind()) == FAILURE ||        
      type_check(leng, 2, BT_INTEGER) == FAILURE)        
    return FAILURE; 
 
  return SUCCESS;        
}       
       
       
        
        
try g95_check_pack(g95_expr *ap, g95_expr *m, g95_expr *vector) {     
     
  if (array_check(ap, 0) == FAILURE) return FAILURE;          
          
  if (type_check(m, 1, BT_LOGICAL) == FAILURE) return FAILURE;     
     
  if (m->rank != 0 && m->rank != ap->rank) {
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
   
   
   
   
try g95_check_int(g95_expr *u, g95_expr *kind) {          
          
  if (numeric_check(u, 0) == FAILURE ||          
      kind_check(kind, 1, BT_INTEGER) == FAILURE) return FAILURE;    
    
  return SUCCESS; 
}      
      
      
       
       
try g95_check_radix(g95_expr *z) {

  if (int_or_real_check(z, 0) == FAILURE) return FAILURE;        
        
  g95_intrinsic_extension = 0; 
 
  return SUCCESS; 
}    
    
    
 
 
try g95_check_dble(g95_expr *a) {         
         
  if (numeric_check(a, 0) == FAILURE) return FAILURE;  
  
  return SUCCESS;    
}  
  
  
      
      
/* g95_check_a_xkind()-- Check subroutine suitable for aint,
 * anint. */  
  
try g95_check_a_xkind(g95_expr *t, g95_expr *knd) {     
     
  return check_a_kind(t, knd, BT_REAL);          
}        
        
        
         
         
try g95_check_shape(g95_expr *source) {    
g95_array_ref *ref; 
 
  if (source->rank == 0 || source->type != EXPR_VARIABLE) return SUCCESS;

  ref = g95_find_array_ref(source);          
          
  if (ref->as && ref->as->type == AS_ASSUMED_SIZE) { 
    g95_error("'source' argument of 'shape' intrinsic at %L must not be "          
	      "an assumed size array", &source->where);     
    return FAILURE;      
  }        
        
  return SUCCESS;       
}         
         
         
     
     
try g95_check_associated(g95_expr *pointer, g95_expr *target) {     
symbol_attribute attr;       
       
  if (variable_check(pointer, 0) == FAILURE) return FAILURE;    
    
  attr = g95_variable_attr(pointer, NULL);      
  if (!attr.pointer) {   
    must_be(pointer, 0, "a POINTER");       
    return FAILURE; 
  }          
          
  if (target == NULL) return SUCCESS;    
    
  /* Target argument is optional */        
        
  attr = g95_variable_attr(target, NULL);      
  if (!attr.pointer && !attr.target) {        
    must_be(target, 1, "a POINTER or a TARGET");          
    return FAILURE;         
  }         
         
  return SUCCESS;   
}    
    
    


try g95_check_real(g95_expr *h, g95_expr *k) {     
     
  if (numeric_check(h, 0) == FAILURE) return FAILURE;         
         
  if (kind_check(k, 1, BT_REAL) == FAILURE) return FAILURE;          
          
  return SUCCESS;          
}


 
 
/* minloc/maxloc().  Whoever came up with this interface was probably
 * on something.  The possibilities for the occupation of the second
 * and third parameters are:
 *       Arg #2     Arg #3
 *       NULL       NULL
 *       DIM        NULL
 *       MASK       NULL
 *       NULL       MASK             minloc(array, mask=m)
 *       DIM        MASK
 */  
  
try g95_check_minloc_maxloc(g95_expr *array, g95_expr *d, g95_expr *f) {        
        
  if (int_or_real_check(array, 0) == FAILURE) return FAILURE;      
      
  if (array_check(array, 0) == FAILURE) return FAILURE;     
     
  if (f != NULL) {    
    if (logical_array_check(f, 2) == FAILURE) return FAILURE;         
         
    if (d != NULL) {          
      if (scalar_check(d, 1) == FAILURE) return FAILURE;   
      if (type_check(d, 1, BT_INTEGER) == FAILURE) return FAILURE;
    }         
  } else {
    if (d != NULL) { 
      switch(d->ts.type) {  
      case BT_INTEGER:        
	if (scalar_check(d, 1) == FAILURE) return FAILURE;      
	break;     
     
      case BT_LOGICAL:  /* The '2' makes the error message correct */ 
	if (logical_array_check(d, 2) == FAILURE) return FAILURE;          
	break;   
   
      default:       
	type_check(d, 1, BT_INTEGER);  /* Guaranteed to fail */     
	return FAILURE; 
      } 
    }          
  }  
  
  return SUCCESS; 
}          
          
          
 
 
try g95_check_char(g95_expr *v, g95_expr *k) {

  if (type_check(v, 0, BT_INTEGER) == FAILURE) return FAILURE;       
  if (kind_check(k, 1, BT_CHARACTER) == FAILURE) return FAILURE;          
          
  return SUCCESS;          
}     
     
     
       
       
/* dim_check()-- Check the common DIM parameter for correctness */     
     
static try dim_check(g95_expr *d, int s, int optional) {       
       
  if (optional) {
    if (d == NULL) return SUCCESS;         
         
    if (nonoptional_check(d, s) == FAILURE) return FAILURE;       
       
    return SUCCESS;      
  } 
 
  if (d == NULL) {   
    g95_error("Missing DIM parameter in intrinsic '%s' at %L",      
	      g95_current_intrinsic, g95_current_intrinsic_where);
    return FAILURE;  
  } 
 
  if (type_check(d, s, BT_INTEGER) == FAILURE) return FAILURE;

  if (scalar_check(d, s) == FAILURE) return FAILURE; 
 
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
 
 
   
   
try g95_check_ubound(g95_expr *array, g95_expr *dim) {        
        
  if (array_check(array, 0) == FAILURE) return FAILURE;    
    
  if (dim_check(dim, 1, 1) == FAILURE) return FAILURE;

  return SUCCESS;       
}  
  
  
      
      
try g95_check_cmplx(g95_expr *n, g95_expr *f, g95_expr *knd) {         
         
  if (numeric_check(n, 0) == FAILURE) return FAILURE;    
    
  if (f != NULL) {       
    if (numeric_check(f, 1) == FAILURE) return FAILURE; 
 
    if (n->ts.type == BT_COMPLEX) { 
      must_be(f, 1, "not be present if 'x' is COMPLEX");     
      return FAILURE;        
    }    
  }   
   
  if (kind_check(knd, 2, BT_COMPLEX) == FAILURE) return FAILURE;        
        
  return SUCCESS; 
}      
      
      
   
   
try g95_check_selected_real_kind(g95_expr *c, g95_expr *v) {      
      
  if (c == NULL && v == NULL) {       
    g95_error("Missing arguments to %s intrinsic at %L", g95_current_intrinsic,    
	      g95_current_intrinsic_where);        
        
    return FAILURE;    
  }     
     
  if (c != NULL && type_check(c, 0, BT_INTEGER) == FAILURE) return FAILURE;     
     
  if (v != NULL && type_check(v, 1, BT_INTEGER) == FAILURE) return FAILURE;        
        
  g95_intrinsic_extension = 0;  
  
  return SUCCESS;          
}   
   
   
         
         
try g95_check_range(g95_expr *p) {    
    
  if (numeric_check(p, 0) == FAILURE) return FAILURE;

  g95_intrinsic_extension = 0;      
      
  return SUCCESS;         
}        
        
        
 
 
try g95_check_lbound(g95_expr *array, g95_expr *r) {     
     
  if (array_check(array, 0) == FAILURE) return FAILURE;     
  if (dim_check(r, 1, 1) == FAILURE) return FAILURE;   
   
  return SUCCESS;     
}  
  
  


try g95_check_huge(g95_expr *a) {  
  
  if (int_or_real_check(a, 0) == FAILURE) return FAILURE;

  g95_intrinsic_extension = 0;

  return SUCCESS;
}   
   
   
 
 
try g95_check_sum(g95_expr *ap, g95_expr *d, g95_expr *mask) {  
  
  if (array_check(ap, 0) == FAILURE) return FAILURE;         
         
  if (numeric_check(ap, 0) == FAILURE) return FAILURE;   
   
  if (dim_check(d, 1, 1) == FAILURE) return FAILURE;          
          
  if (mask != NULL && logical_array_check(mask, 2) == FAILURE) return FAILURE;

  return SUCCESS;       
} 
 
 
   
   
try g95_check_logical(g95_expr *c, g95_expr *knd) {        
        
  if (type_check(c, 0, BT_LOGICAL) == FAILURE) return FAILURE; 
  if (kind_check(knd, 1, BT_LOGICAL) == FAILURE) return FAILURE;   
   
  return SUCCESS; 
}


 
 
try g95_check_ior(g95_expr *c, g95_expr *t) {   
   
  if (type_check(c, 0, BT_INTEGER) == FAILURE || 
      type_check(t, 1, BT_INTEGER) == FAILURE) return FAILURE;          
          
  if (same_type_check(c, 0, t, 1) == FAILURE) return FAILURE;         
         
  return SUCCESS;
}         
         
         
   
   
try g95_check_cshift(g95_expr *a, g95_expr *shift, g95_expr *dim) {        
        
  if (array_check(a, 0) == FAILURE) return FAILURE;      
      
  if (a->rank == 1) {     
    if (scalar_check(shift, 1) == FAILURE) return FAILURE;    
  } else {      
    /* TODO: more requirements on shift parameter */    
  }    
    
  if (dim_check(dim, 2, 1) == FAILURE) return FAILURE; 
 
  return SUCCESS;    
}       
       
       
      
      
try g95_check_spread(g95_expr *s1, g95_expr *d, g95_expr *ncopies) {          
          
  if (s1->rank >= G95_MAX_DIMENSIONS) {          
    must_be(s1, 0, "less than rank " stringize(G95_MAX_DIMENSIONS));  
    return FAILURE;         
  }         
         
  if (dim_check(d, 1, 0) == FAILURE) return FAILURE;        
        
  if (type_check(ncopies, 2, BT_INTEGER) == FAILURE) return FAILURE;        
  if (scalar_check(ncopies, 2) == FAILURE) return FAILURE;       
       
  return SUCCESS;        
}     
     
     
       
       
try g95_check_sign(g95_expr *w, g95_expr *h) { 
 
  if (int_or_real_check(w, 0) == FAILURE) return FAILURE;

  if (same_type_check(w, 0, h, 1) == FAILURE) return FAILURE; 
 
  return SUCCESS;    
}


 
 
try g95_check_verify(g95_expr *a, g95_expr *g, g95_expr *c) {          
          
  if (type_check(a, 0, BT_CHARACTER) == FAILURE) return FAILURE;     
     
  if (same_type_check(a, 0, g, 1) == FAILURE) return FAILURE;

  if (c != NULL && type_check(c, 2, BT_LOGICAL) == FAILURE) return FAILURE;    
    
  return SUCCESS;
}       
       
       
  
  
try g95_check_mvbits(g95_expr *frm, g95_expr *frompos, g95_expr *len,    
		     g95_expr *end, g95_expr *topos) {     
     
  if (type_check(frm, 0, BT_INTEGER) == FAILURE) return FAILURE;        
        
  if (type_check(frompos, 1, BT_INTEGER) == FAILURE) return FAILURE;       
       
  if (type_check(len, 2, BT_INTEGER) == FAILURE) return FAILURE;       
       
  if (same_type_check(frm, 0, end, 3) == FAILURE) return FAILURE; 
 
  if (variable_check(end, 3) == FAILURE) return FAILURE;  
  
  if (type_check(topos, 4, BT_INTEGER) == FAILURE) return FAILURE;          
          
  return SUCCESS;    
}         
         
         


try g95_check_abs(g95_expr *l) {

  if (numeric_check(l, 0) == FAILURE) return FAILURE;

  return SUCCESS;      
}         
         
         
 
 
try g95_check_iand(g95_expr *h, g95_expr *c) {        
        
  if (type_check(h, 0, BT_INTEGER) == FAILURE ||   
      type_check(c, 1, BT_INTEGER) == FAILURE) return FAILURE;

  if (same_type_check(h, 0, c, 1) == FAILURE) return FAILURE;

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
     
     
        
        
try g95_check_merge(g95_expr *tsource, g95_expr *fsource, g95_expr *maski) {       
       
  if (same_type_check(tsource, 0, fsource, 1) == FAILURE) return FAILURE;         
         
  if (type_check(maski, 2, BT_LOGICAL) == FAILURE) return FAILURE;  
  
  return SUCCESS;   
}      
      
      
       
       
static try min_max_args(g95_actual_arglist *ap) {         
         
  if (ap == NULL || ap->next == NULL) {     
    g95_error("Intrinsic '%s' at %L must have at least two arguments",         
	      g95_current_intrinsic, g95_current_intrinsic_where);          
    return FAILURE;      
  }   
   
  return SUCCESS;         
}       
       
       
   
   
try g95_check_product(g95_expr *array, g95_expr *dim, g95_expr *maski) {         
         
  if (array_check(array, 0) == FAILURE) return FAILURE;         
         
  if (numeric_check(array, 0) == FAILURE) return FAILURE;         
         
  if (dim_check(dim, 1, 1) == FAILURE) return FAILURE;   
   
  if (maski != NULL && logical_array_check(maski, 2) == FAILURE) return FAILURE;     
     
  return SUCCESS;      
}


  
  
static try check_rest(bt type, int knd, g95_actual_arglist *args) {  
g95_expr *x;
int f;       
       
  if (min_max_args(args) == FAILURE) return FAILURE; 

  f = 1;    
    
  for(; args; args=args->next, f++) {  
    x = args->u.expr;     
    if (x->ts.type != type || x->ts.kind != knd) {
      g95_error("'a%d' argument of '%s' intrinsic at %L must be %s(%d)",    
		f, g95_current_intrinsic, &x->where,      
		g95_basic_typename(type), knd);      
      return FAILURE;    
    }        
  }          
          
  return SUCCESS;      
}


     
     
try g95_check_minval_maxval(g95_expr *array, g95_expr *dim, g95_expr *mask) {          
          
  if (array_check(array, 0) == FAILURE) return FAILURE; 
 
  if (int_or_real_check(array, 0) == FAILURE) return FAILURE; 
 
  if (dim_check(dim, 1, 1) == FAILURE) return FAILURE;       
       
  if (mask != NULL && logical_array_check(mask, 2) == FAILURE) return FAILURE;         
         
  return SUCCESS;     
}        
        
        
 
 
try g95_check_transfer(g95_expr *s1, g95_expr *mold, g95_expr *s) { 
 
  if (s != NULL) {
    if (type_check(s, 2, BT_INTEGER) == FAILURE) return FAILURE;       
       
    if (scalar_check(s, 2) == FAILURE) return FAILURE;     
     
    if (nonoptional_check(s, 2) == FAILURE) return FAILURE; 
  }       
       
  return SUCCESS;          
} 
 
 
   
   
try g95_check_present(g95_expr *p) {     
     
  if (variable_check(p, 0) == FAILURE) return FAILURE; 
 
  if (!p->symbol->attr.dummy) { 
    must_be(p, 0, "a dummy variable");  
    return FAILURE;         
  }        
        
  if (!p->symbol->attr.optional) {       
    must_be(p, 0, "an OPTIONAL dummy variable"); 
    return FAILURE;    
  }         
         
  g95_intrinsic_extension = 0;          
          
  return SUCCESS;  
}   
   
   
       
       
try g95_check_ibclr(g95_expr *n, g95_expr *position) {          
          
  if (type_check(n,   0, BT_INTEGER) == FAILURE ||          
      type_check(position, 1, BT_INTEGER) == FAILURE ||         
      kind_value_check(position, 1, g95_default_integer_kind()) == FAILURE)    
    return FAILURE;         
         
  return SUCCESS;   
}     
     
     
       
       
try g95_check_all_any(g95_expr *m, g95_expr *dim) {      
      
  if (logical_array_check(m, 0) == FAILURE) return FAILURE;          
          
  if (dim_check(dim, 1, 1) == FAILURE) return FAILURE;     
     
  return SUCCESS;  
} 
 
 
        
        
try g95_check_min_max_integer(g95_actual_arglist *args) {         
         
  return check_rest(BT_INTEGER, g95_default_integer_kind(), args);     
} 
 
 
 
 
try g95_check_min_max_real(g95_actual_arglist *args) {    
    
  return check_rest(BT_REAL, g95_default_real_kind(), args);  
}     
     
     
    
    
try g95_check_scan(g95_expr *a, g95_expr *u, g95_expr *k) {    
    
  if (type_check(a, 0, BT_CHARACTER) == FAILURE) return FAILURE;       
       
  if (type_check(u, 1, BT_CHARACTER) == FAILURE) return FAILURE;      
      
  if (k != NULL && type_check(k, 2, BT_LOGICAL) == FAILURE) return FAILURE;          
          
  if (same_type_check(a, 0, u, 1) == FAILURE) return FAILURE;     
     
  return SUCCESS;         
}


  
  
try g95_check_random_seed(g95_expr *siz, g95_expr *put, g95_expr *get) {       
       
  if (siz != NULL) {       
    if (scalar_check(siz, 0) == FAILURE) return FAILURE;          
          
    if (type_check(siz, 0, BT_INTEGER) == FAILURE) return FAILURE;      
      
    if (variable_check(siz, 0) == FAILURE) return FAILURE;

    if (kind_value_check(siz, 0, g95_default_integer_kind()) == FAILURE)          
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


try g95_check_allocated(g95_expr *block) {        
        
  if (variable_check(block, 0) == FAILURE) return FAILURE;   
   
  if (array_check(block, 0) == FAILURE) return FAILURE;       
       
  if (!block->symbol->attr.allocatable) {
    must_be(block, 0, "ALLOCATABLE");       
    return FAILURE; 
  }         
         
  return SUCCESS;        
}       
       
       
          
          
/* g95_check_i()-- Check that the single argument is an integer */      
      
try g95_check_i(g95_expr *u) {          
          
  if (type_check(u, 0, BT_INTEGER) == FAILURE) return FAILURE;        
        
  return SUCCESS;   
}   
   
   
          
          
try g95_check_min_max_double(g95_actual_arglist *args) {          
          
  return check_rest(BT_REAL, g95_default_double_kind(), args);     
} 
 
 
    
    
try g95_check_min_max(g95_actual_arglist *args) {  
g95_expr *m;     
     
  if (min_max_args(args) == FAILURE) return FAILURE; 

  m = args->u.expr; 
 
  if (m->ts.type != BT_INTEGER && m->ts.type != BT_REAL) {
    g95_error("'a1' argument of '%s' intrinsic at %L must be INTEGER or REAL",
	      g95_current_intrinsic, &m->where);       
    return FAILURE;    
  }   
   
  return check_rest(m->ts.type, m->ts.kind, args);  
}     
     
     
        
        
try g95_check_unpack(g95_expr *vector, g95_expr *mask, g95_expr *field) {      
      
  if (rank_check(vector, 0, 1) == FAILURE) return FAILURE;       
       
  if (array_check(mask, 1) == FAILURE) return FAILURE;   
   
  if (type_check(mask, 1, BT_LOGICAL) == FAILURE) return FAILURE;  
  
  if (same_type_check(vector, 0, field, 2) == FAILURE) return FAILURE;    
    
  return SUCCESS;     
}


     
     
try g95_check_random_number(g95_expr *harvest) { 
 
  if (type_check(harvest, 0, BT_REAL) == FAILURE) return FAILURE;          
          
  if (variable_check(harvest, 0) == FAILURE) return FAILURE;         
         
  return SUCCESS;        
}     
     
     
 
 
/* Common check function where the first argument must be real or
 * integer and the second argument must be the same as the first. */

try g95_check_a_p(g95_expr *u, g95_expr *b) {     
     
  if (int_or_real_check(u, 0) == FAILURE) return FAILURE;

  if (same_type_check(u, 0, b, 1) == FAILURE) return FAILURE;          
          
  return SUCCESS;        
}       
       
       
         
         
try g95_check_eoshift(g95_expr *ap, g95_expr *shift, g95_expr *boundary,          
		      g95_expr *r) {          
          
  if (array_check(ap, 0) == FAILURE) return FAILURE;  
  
  if (type_check(shift, 1, BT_INTEGER) == FAILURE) return FAILURE; 
 
  if (ap->rank == 1) { 
    if (scalar_check(shift, 2) == FAILURE) return FAILURE;         
  } else {   
    /* TODO: more weird restrictions on shift */       
  }          
          
  if (boundary != NULL) {  
    if (same_type_check(ap, 0, boundary, 2) == FAILURE) return FAILURE;       
       
    /* TODO: more restrictions on boundary */        
  }      
      
  if (dim_check(r, 1, 1) == FAILURE) return FAILURE;          
          
  return SUCCESS;       
}         
         
         
      
      
try g95_check_size(g95_expr *block, g95_expr *d) {        
        
  if (array_check(block, 0) == FAILURE) return FAILURE;       
       
  if (d != NULL) {         
    if (type_check(d, 1, BT_INTEGER) == FAILURE) return FAILURE;      
      
    if (kind_value_check(d, 1, g95_default_integer_kind()) == FAILURE) 
      return FAILURE;         
         
    if (dim_rank_check(d, block) == FAILURE) return FAILURE;
  }      
      
  return SUCCESS;       
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
  
  


try g95_check_count(g95_expr *msk, g95_expr *d) {   
   
  if (logical_array_check(msk, 0) == FAILURE) return FAILURE;       
  if (dim_check(d, 1, 1) == FAILURE) return FAILURE;       
       
  return SUCCESS; 
}        
        
        
  
  
try g95_check_repeat(g95_expr *a, g95_expr *l) {     
     
  if (type_check(a, 0, BT_CHARACTER) == FAILURE) return FAILURE; 
 
  if (type_check(l, 0, BT_INTEGER) == FAILURE) return FAILURE;         
         
  return SUCCESS;     
}        
        
        
         
         
try g95_check_reshape(g95_expr *s0, g95_expr *s,
		      g95_expr *pad, g95_expr *order) {   
mpz_t sz;  
int j;     
     
  if (array_check(s0, 0) == FAILURE) return FAILURE;  
  
  if (rank_check(s, 1, 1) == FAILURE) return FAILURE;

  if (type_check(s, 1, BT_INTEGER) == FAILURE) return FAILURE;     
     
  if (g95_array_size(s, &sz) != SUCCESS) {      
    g95_error("'shape' argument of 'reshape' intrinsic at %L must be an " 
	      "array of constant size", &s->where);   
    return FAILURE;     
  }

  j = mpz_cmp_ui(sz, G95_MAX_DIMENSIONS);   
  mpz_clear(sz);       
       
  if (j > 0) {
    g95_error("'shape' argument of 'reshape' intrinsic at %L has more than "          
	      stringize(G95_MAX_DIMENSIONS) " elements", &s->where);  
    return FAILURE;       
  }

  if (pad != NULL) {  
    if (same_type_check(s0, 0, pad, 2) == FAILURE) return FAILURE;    
    if (array_check(pad, 2) == FAILURE) return FAILURE;  
  }    
    
  if (order != NULL && array_check(order, 3) == FAILURE) return FAILURE; 
 
  return SUCCESS;  
}     
     
     
 
 
try g95_check_set_exponent(g95_expr *r, g95_expr *m) {  
  
  if (type_check(r, 0, BT_REAL) == FAILURE) return FAILURE;     
     
  if (type_check(m, 1, BT_INTEGER) == FAILURE) return FAILURE;

  return SUCCESS;    
}      
      
      
  
  
try g95_check_ishftc(g95_expr *r, g95_expr *shift, g95_expr *size) { 
 
  if (type_check(r, 0, BT_INTEGER) == FAILURE ||          
      type_check(shift, 1, BT_INTEGER) == FAILURE) return FAILURE;   
   
  if (size != NULL &&        
      type_check(size, 2, BT_INTEGER) == FAILURE) return FAILURE;         
         
  return SUCCESS;         
}        
        
        
    
    
try g95_check_idnint(g95_expr *c) {

  if (double_check(c, 0) == FAILURE) return FAILURE;    
    
  return SUCCESS;    
}  
  
  
