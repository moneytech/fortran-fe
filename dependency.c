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

/* Return true if the statement body redefines the condition.  Returns true if
   expr2 depends on expr1.  expr1 should be a single term suitable for the lhs
   of an assignment.  The symbols listed in VARS must be considered to have
   all possible values. All other scalar variables may be considered constant.
   Used for forall and where statements.  Also used with functions returning
   arrays without a temporary.  */
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

