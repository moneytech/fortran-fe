/* Dependency analysis
   Copyright (C) 2000, 2001, 2002 Free Software Foundation, Inc.
   Contributed by Paul Brook <paul@nowt.org>

This file is part of GNU G95.

GNU G95 is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU G95 is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU G95; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

/* dependency.c -- Expression dependency analysis code.  */

#include "g95.h"
#include "dependency.h"
#include <assert.h>

/* Return true if both symbols could refer to the same data object.  Does not
   take account of aliasing due to equivalence statements.  */
int
g95_symbols_could_alias (g95_symbol *lsym, g95_symbol *rsym)
{
  /* Aliasing isn't possible if the symbols have different base types.  */
  if (g95_compare_types (&lsym->ts, &rsym->ts) == 0)
    return 0;

  /* Pointers can point to other pointers, target objects and allocatable
     objects.  Two allocatable objects cannot share the same storage.  */
  if (lsym->attr.pointer
      && (rsym->attr.pointer || rsym->attr.allocatable || rsym->attr.target))
    return 1;
  if (lsym->attr.target
      && rsym->attr.pointer)
    return 1;
  if (lsym->attr.allocatable
      && rsym->attr.pointer)
    return 1;

  return 0;
}

/* Returns 1 if the expr is an integer constant value 1, 0 if it is not or
   def if the value could not be determined.  */
int
g95_expr_is_one (g95_expr * expr, int def)
{
  assert (expr != NULL);

  if (expr->expr_type != EXPR_CONSTANT)
    return def;

  if (expr->ts.type != BT_INTEGER)
    return def;

  return mpz_cmp_si (expr->value.integer, 1) == 0;
}

/* Compare two values.  Returns 0 if e1 == e2, -1 if e1 < e2, +1 if e1 > e2,
   and -2 if the relationship could not be determined.  */
int
g95_dep_compare_expr (g95_expr * e1, g95_expr * e2)
{
  int i;

  if (e1->expr_type != e2->expr_type)
    return -2;

  switch (e1->expr_type)
    {
    case EXPR_CONSTANT:
      if (e1->ts.type != BT_INTEGER || e2->ts.type != BT_INTEGER)
        return -2;

      i = mpz_cmp (e1->value.integer, e2->value.integer);
      if (i == 0)
        return 0;
      else if (i < 0)
        return -1;
      return 1;

    case EXPR_VARIABLE:
      if (e1->ref || e2->ref)
        return -2;
      if (e1->symbol == e2->symbol)
        return 0;
      return -2;

    default:
      return -2;
    }
}

/* Returns 1 if the two ranges are the same, 0 if they are not, and def
   if the results are indeterminate.  N is the dimension to compare.  */
int
g95_is_same_range (g95_array_ref * ar1, g95_array_ref * ar2, int n, int def)
{
  g95_expr *e1;
  g95_expr *e2;
  int i;

  /* TODO: More sophisticated range comparison.  */
  assert (ar1 && ar2);

  assert (ar1->dimen_type[n] == ar2->dimen_type[n]);

  e1 = ar1->stride[n];
  e2 = ar2->stride[n];
  /* Check for mismatching strides.  A NULL stride means a stride of 1.  */
  if (e1 && ! e2)
    {
      i = g95_expr_is_one (e1, -1);
      if (i == -1)
        return def;
      else if (i == 0)
        return 0;
    }
  else if (e2 && ! e1)
    {
      i = g95_expr_is_one (e2, -1);
      if (i == -1)
        return def;
      else if (i == 0)
        return 0;
    }
  else if (e1 && e2)
    {
      i = g95_dep_compare_expr (e1, e2);
      if (i == -2)
        return def;
      else if (i != 0)
        return 0;
    }
  /* The strides match.  */

  /* Check the range start.  */
  e1 = ar1->start[n];
  e2 = ar2->start[n];

  if (! (e1 || e2))
    return 1;

  /* Use the bound of the array if no bound is specified.  */
  if (ar1->as && ! e1)
    e1 = ar1->as->lower[n];

  if (ar2->as && ! e2)
    e2 = ar2->as->upper[n];

  /* Check we have values for both.  */
  if (! (e1 && e2))
    return def;

  i = g95_dep_compare_expr (e1, e2);

  if (i == -2)
    return def;
  else if (i == 0)
    return 1;
  return 0;
}

/* Dependency checking for direct function return by reference.  Returns true
   if the arguments of the function depend on the destination.  This is
   considerably less conservative than other dependencies because many
   function arguments will already be copied into a temporary.  */
int
g95_check_fncall_dependency (g95_expr * dest, g95_expr * fncall)
{
  g95_actual_arglist *actual;
  g95_ref *ref;
  g95_expr *expr;
  int n;

  assert (dest->expr_type == EXPR_VARIABLE
          && fncall->expr_type == EXPR_FUNCTION);
  assert (fncall->rank > 0);

  for (actual = fncall->value.function.actual; actual; actual = actual->next)
    {
      expr = actual->expr;

      /* Skip args which are not present.  */
      if (! expr)
        continue;

      /* Non-variable expressions will be allocated temporaries anyway.  */
      switch (expr->expr_type)
        {
        case EXPR_VARIABLE:
          if (expr->rank > 1)
            {
              /* This is an array section.  */
              for (ref = expr->ref; ref; ref = ref->next)
                {
                  if (ref->type == REF_ARRAY
                      && ref->u.ar.type != AR_ELEMENT)
                    break;
                }
              assert (ref);
              /* AR_FULL can't contain vector subscripts.  */
              if (ref->u.ar.type == AR_SECTION)
                {
                  for (n = 0; n < ref->u.ar.dimen; n++)
                    {
                      if (ref->u.ar.dimen_type[n] == DIMEN_VECTOR)
                        break;
                    }
                  /* Vector subscript array sections will be copied to a
                     temporary.  */
                  if (n != ref->u.ar.dimen)
                    continue;
                }
            }

          if (g95_check_dependency (dest, actual->expr, NULL, 0))
            return 1;
          break;

        case EXPR_ARRAY:
          if (g95_check_dependency (dest, expr, NULL, 0))
            return 1;
          break;

        default:
          break;
        }
    }

  return 0;
}

/* Return true if the statement body redefines the condition.  Returns
 * true if expr2 depends on expr1.  expr1 should be a single term
 * suitable for the lhs of an assignment.  The symbols listed in VARS
 * must be considered to have all possible values. All other scalar
 * variables may be considered constant.  Used for forall and where
 * statements.  Also used with functions returning arrays without a
 * temporary. */

int
g95_check_dependency (g95_expr * expr1, g95_expr * expr2, g95_expr ** vars,
                      int nvars)
{
  g95_ref *ref;
  int n;
  g95_actual_arglist *actual;

  assert (expr1->expr_type == EXPR_VARIABLE);
  /* TODO: -fassume-no-pointer-aliasing */
  if (expr1->symbol->attr.pointer)
    return 1;
  for (ref = expr1->ref; ref; ref = ref->next)
    {
      if (ref->type == REF_COMPONENT
          && ref->u.c.component->pointer)
        return 1;
    }

  switch (expr2->expr_type)
    {
    case EXPR_OP:
      n = g95_check_dependency (expr1, expr2->op1, vars, nvars);
      if (n)
        return n;
      if (expr2->op2)
        return g95_check_dependency (expr1, expr2->op2, vars, nvars);
      return 0;

    case EXPR_VARIABLE:
      if (expr2->symbol->attr.pointer)
        return 1;

      for (ref = expr2->ref; ref; ref = ref->next)
        {
          if (ref->type == REF_COMPONENT
              && ref->u.c.component->pointer)
                return 1;
        }

      if (expr1->symbol != expr2->symbol)
        return 0;

      for (ref = expr2->ref; ref; ref = ref->next)
        {
          /* Identical ranges return 0, overlapping ranges return 1.  */
          if (ref->type == REF_ARRAY)
            return 1;
        }
      return 1;

    case EXPR_FUNCTION:
      /* Remember possible differences betweeen elemental and transformational
         functions.  All functions inside a forall will be pure.  */
      for (actual = expr2->value.function.actual;
           actual;
           actual = actual->next)
        {
          if (! actual->expr)
            continue;
          n = g95_check_dependency (expr1, actual->expr, vars, nvars);
          if (n)
            return n;
        }
      return 0;

    case EXPR_CONSTANT:
      return 0;

    case EXPR_ARRAY:
      /* Probably ok in the majority of (constant) cases.  */
      return 1;

    default:
      return 1;
    }
}

/* Finds if two array references are overlapping or not. 
 * Return value
 * 	1 : array references are overlapping
 * 	0 : array references are not overlapping
 *
 *
 */

int
g95_dep_resolver(g95_ref* lref, g95_ref *rref, int* dep)
{
  int	n;
  int fin_dep=1;


  /* Check if the array reference has derived type */
  lref = g95_get_array_from_component(lref);
  rref = g95_get_array_from_component(rref);

  if( NULL == lref ||NULL == rref )
    return 1;
	
  for(n=0; n < lref->u.ar.dimen; n++) {
    /* We assume dependency when one of array reference is vector subscript */
    
    if (lref->u.ar.dimen_type[n] == DIMEN_VECTOR ||
	rref->u.ar.dimen_type[n] == DIMEN_VECTOR) return 1;

    if (lref->u.ar.dimen_type[n] == DIMEN_RANGE &&
	rref->u.ar.dimen_type[n] == DIMEN_RANGE)
      dep[n] = g95_check_range_range(lref,rref,n);	

    else if (lref->u.ar.dimen_type[n] == DIMEN_ELEMENT &&
	     rref->u.ar.dimen_type[n] == DIMEN_RANGE)
      dep[n] = g95_check_element_vs_section(lref,rref,n);

    else if (rref->u.ar.dimen_type[n] == DIMEN_ELEMENT &&
	     lref->u.ar.dimen_type[n] == DIMEN_RANGE )
      dep[n] = g95_check_element_vs_section(rref,lref,n);

    else if (rref->u.ar.dimen_type[n] == DIMEN_ELEMENT &&
	     lref->u.ar.dimen_type[n] == DIMEN_ELEMENT )
      dep[n] = g95_check_element_vs_element(rref,lref,n);
  }		

  for(n=lref->u.ar.dimen-1; n>=0; n--)
    if (dep[n] > fin_dep) fin_dep = dep[n];

  return (fin_dep == 3);
}


static void Get_X(mpf_t *x, g95_expr *l1, g95_expr *l2, g95_expr *s1,
		  g95_expr *s2,int no_of_elements) {
mpf_t expr1, expr2, n, ll1, ll2, ss1, ss2;
	
  mpf_init(ll1);
  mpf_init(ll2);
  mpf_init(ss1);
  mpf_init(ss2);
  mpf_init(n);
  mpf_init(expr1);
  mpf_init(expr2);

  mpf_set_z(ll1, l1->value.integer);
  mpf_set_z(ll2, l2->value.integer);

  /* If stride is null assume 1 */

  if (NULL == s1)
     mpf_set_si(ss1, 1);
  else
     mpf_set_z(ss1, s1->value.integer);

  if (NULL == s2)
    mpf_set_si(ss2, 1);
  else
    mpf_set_z(ss2, s2->value.integer);

  mpf_sub(expr1, ll2, ll1);
  mpf_mul_ui(expr2, ss2, no_of_elements);
  mpf_add(n, expr1, expr2);
  mpf_div(*x, n, ss1);

  mpf_clear(ll1);
  mpf_clear(ll2);
  mpf_clear(ss1);
  mpf_clear(ss2);
  mpf_clear(n);
  mpf_clear(expr1);
  mpf_clear(expr2);
}


int 
g95_check_range_range(g95_ref* lref, g95_ref* rref, int n)
{
  g95_expr *l_start;
  g95_expr *l_end;
  g95_expr *l_stride;

  g95_expr *r_start;
  g95_expr *r_stride;

  g95_array_ref	l_ar;
  g95_array_ref	r_ar;

  int no_of_elements;
  enum range x1_value, x2_value;
  mpf_t	X1, X2;
  int fin_dep;

  if (lref->type == REF_ARRAY && rref->type == REF_ARRAY) {
    l_ar = lref->u.ar;
    r_ar = rref->u.ar;

    l_start = l_ar.start[n] ;
    l_end = l_ar.end[n] ;
    l_stride = l_ar.stride[n] ;
    r_start = r_ar.start[n] ;
    r_stride = r_ar.stride[n] ;

    /* if l_start is NULL take it from array specifier */
    if (NULL == l_start && IS_ARRAY_EXPLICIT(l_ar.as))
      l_start = l_ar.as->lower[n];

    /* if l_end is NULL take it from array specifier */
    if (NULL == l_end && IS_ARRAY_EXPLICIT(l_ar.as))
      l_end = l_ar.as->upper[n];

    /* if r_start is NULL take it from array specifier */
    if (NULL == r_start && IS_ARRAY_EXPLICIT(r_ar.as))
      r_start = r_ar.as->lower[n];

    if (NULL == l_start || NULL == l_end || NULL == r_start)
      return 3;

    /* TODO : following constraint can be relaxed in case of variables
     * appearing in start,end or stride for exmple a(i:j:k) = a(i:j:k).
     * Currently we check the dependency only when start, end and
     * stride are constant, if * stride is NULL we assume it as one. */

    if (l_end->expr_type != EXPR_CONSTANT ||
	l_start->expr_type != EXPR_CONSTANT ||
	r_start->expr_type != EXPR_CONSTANT ||
	((NULL != l_stride) && (l_stride->expr_type != EXPR_CONSTANT)) ||
	((NULL != r_stride) && (r_stride->expr_type != EXPR_CONSTANT)))
      return 3;

     no_of_elements=get_no_of_elements(l_end,l_start,l_stride);
     mpf_init(X1);
     mpf_init(X2);

     Get_X(&X1, l_start,r_start,l_stride,r_stride,0);
     Get_X(&X2, l_start,r_start,l_stride,r_stride,no_of_elements);

     x1_value = get_range(X1,no_of_elements);
     x2_value = get_range(X2,no_of_elements);
     fin_dep = get_deps(x1_value,x2_value,X1,X2,no_of_elements);
     return fin_dep;
  }

  return 3;
}


int
get_no_of_elements(g95_expr *u1,g95_expr *l1,g95_expr *s1)
{
  mpz_t expr1,expr2,stride;
  int N;

  /* nNoOfEle = (u1-l1)/s1; */

  mpz_init(expr1);
  mpz_init(expr2);
  mpz_init(stride);
  mpz_sub(expr1, u1->value.integer, l1->value.integer);

  if (NULL == s1) {
    mpz_set_si(stride,1);
    mpz_div(expr2,expr1, stride );
  } else
    mpz_div(expr2,expr1, s1->value.integer );

  N = mpz_get_si(expr2);
  return  N;
}



enum range get_range(mpf_t temp,int N)
{
  int cmp;
	
  cmp = mpf_cmp_ui(temp,0);
  if (cmp < 0) return LHS;

  cmp = mpf_cmp_ui(temp,N);
  if (cmp >0) return RHS;

  return MID;
}

int
get_deps(enum range x1, enum range x2, mpf_t X1, mpf_t X2, int N)
{
  mpz_t x1_trun;
  mpz_t x2_trun;
  mpz_init(x1_trun);
  mpz_init(x2_trun);
  mpz_set_f(x1_trun,X1);
  mpz_set_f(x2_trun,X2);

  if ((mpf_cmp_ui(X1,0)==0) && (mpf_cmp_ui(X2,N)==0)) return 1;

  /* check if the arrays overlap exactly */

  else if (mpz_cmp(x1_trun,x2_trun)==0)
    return 4;  /* If both values of second arrays are in same region */
  else if ((x1 == LHS && x2 == LHS) || (x1 == RHS && x2 == RHS))
    return 4;
  else if (x1 == MID && x2== RHS) 
    return 2;
  else
    return 3;
}


int
g95_check_element_vs_section( g95_ref *lref, g95_ref *rref ,int n )
{
  g95_array_ref	l_ar;
  g95_array_ref	r_ar;
  g95_expr	*l_start;
  g95_expr	*r_start;
  g95_expr	*r_end;
  int 		greater;
  int		nIsDep = 3;

  if ( lref->type == REF_ARRAY && rref->type == REF_ARRAY) {
    l_ar = lref->u.ar;
    r_ar = rref->u.ar;
    l_start = l_ar.start[n] ;
    r_start = r_ar.start[n] ;
    r_end = r_ar.end[n] ;

    if (NULL == r_start && IS_ARRAY_EXPLICIT(r_ar.as))
      r_start = r_ar.as->lower[n];

    if (NULL == r_end && IS_ARRAY_EXPLICIT(r_ar.as))
      r_end = r_ar.as->upper[n];

    if (NULL == r_start || NULL == r_end)
      return  3;

    greater=g95_dep_compare_expr(r_start,r_end);
	
    if (greater >0) 
      nIsDep = g95_is_inside_range(l_start, r_end, r_start);
    else
      nIsDep = g95_is_inside_range(l_start, r_start, r_end);

  }
  else
    nIsDep = 3;

  return nIsDep;	
}


int
g95_check_element_vs_element( g95_ref *lref, g95_ref *rref ,int n )
{
  g95_array_ref   l_ar;
  g95_array_ref   r_ar;
  g95_expr        *l_start;
  g95_expr        *r_start;
  int nIsDep=3;	

  if (lref->type == REF_ARRAY && rref->type == REF_ARRAY) {
    l_ar = lref->u.ar;
    r_ar = rref->u.ar;
    l_start = l_ar.start[n] ;
    r_start = r_ar.start[n] ;
    if (g95_dep_compare_expr(r_start,l_start) ==0)
      nIsDep = 1;
    else
      nIsDep = 4;
  } else
    nIsDep = 4;

  return nIsDep;	
}


/* left	<= check <= right */

int
g95_is_inside_range( g95_expr *chk, g95_expr *left, g95_expr *right)
{
  int	nIsInside = 3;	/* 3 <- inside 4 <- outside */

  if( NULL == chk )
    nIsInside = 3;

  else if ((g95_dep_compare_expr(chk, left) > -1))  /* chk >= left */
    if (g95_dep_compare_expr(right,chk) > -1)       /* chk <= end */
      nIsInside = 3;
    else
      nIsInside = 4;
  else
    nIsInside = 4;	

  return nIsInside;
}


g95_ref *g95_get_array_from_component(g95_ref *ref) {

  if ( ref->type == REF_ARRAY ) {
    /* Such cases should have detected by front end */

    if (NULL != ref->next && ref->next->type == REF_COMPONENT) {
      if( g95_check_another_arrayref(ref->next) )
	g95_fatal_error("More than one part references");
    }

    return ref;
  }

  while(NULL != ref && ref->type != REF_ARRAY) {
    if (ref->type == REF_ARRAY) {
      if (ref->u.ar.type != AR_ELEMENT || ref->u.ar.type != AR_UNKNOWN) {
	/* Such cases should have detected by front end */

	if (g95_check_another_arrayref(ref))
	  g95_fatal_error("More than one part references");

	return ref;
      }
    }

    ref = ref->next;
  }

  return ref;
}


int g95_check_another_arrayref(g95_ref *ref) {

  for (; ref!=NULL; ref = ref->next)
    if (ref->type == REF_ARRAY && ref->u.ar.type != AR_ELEMENT) return 1;

  return 0;
}
