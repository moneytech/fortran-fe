/* Array things
   Copyright (C) 2000 Free Software Foundation, Inc.
   Contributed by Andy Vaught

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

#include "g95.h"

/**************** Array reference matching subroutines *****************/

/* g95_free_array_ref()-- Free an array reference structure and
 * everything it points to. */

void g95_free_array_ref(g95_array_ref *ar) {
int i;

  for(i=0; i<G95_MAX_DIMENSIONS; i++) {
    g95_free_expr(ar->shape[i].start);
    g95_free_expr(ar->shape[i].end);
    g95_free_expr(ar->shape[i].stride);
  }

  g95_free(ar);
}


/* g95_copy_array_ref()-- Copy an array reference structure */

g95_array_ref *g95_copy_array_ref(g95_array_ref *src) {
g95_array_ref *dest;
int i;

  dest = g95_get_array_ref();

  dest->type = src->type;
  dest->rank = src->rank;

  for(i=0; i<G95_MAX_DIMENSIONS; i++) {
    dest->shape[i].start = g95_copy_expr(src->shape[i].start);
    dest->shape[i].end = g95_copy_expr(src->shape[i].end);
    dest->shape[i].stride = g95_copy_expr(src->shape[i].stride);
  }

  dest->offset = g95_copy_expr(src->offset);

  return dest;
}


/* g95_show_array_ref()-- Show an array reference */

void g95_show_array_ref(g95_array_ref *ar) {
int i;

  g95_status_char('('); 

  switch(ar->type) {
  case AR_FULL:
    g95_status("FULL");
    break;

  case AR_SECTION: 
    for(i=0; i<ar->rank; i++) {
      if (ar->shape[i].start != NULL)
	g95_show_expr(ar->shape[i].start);

      g95_status_char(':');

      if (ar->shape[i].end != NULL)
	g95_show_expr(ar->shape[i].end);

      if (ar->shape[i].stride != NULL) {
	g95_status_char(':');
	g95_show_expr(ar->shape[i].stride);
      }

      if (i != ar->rank-1) g95_status(" , ");
    }
    break;

  case AR_ELEMENT:
    for(i=0; i<ar->rank; i++) {
      g95_show_expr(ar->shape[i].start);
      if (i != ar->rank - 1) g95_status(" , ");
    }
    break;

  default: g95_internal_error("g95_show_array_ref(): Unknown array reference");
  }

  g95_status_char(')');
}



/* check_dimension()-- Compare a single dimension of array reference
 * to array specification. */

static try check_dimension(int i, g95_array_ref *ar, g95_array_spec *as) {
int start_v, end_v, stride_v, lower_v, upper_v, start, end, stride,
    lower, upper;
g95_expr *e;

  lower = as->shape[i].lower != NULL &&
    as->shape[i].lower->expr_type == EXPR_CONSTANT;

  upper = as->shape[i].upper != NULL &&
    (((i+1 == as->rank && as->type == AS_ASSUMED_SIZE)) ? 0
    : as->shape[i].upper->expr_type == EXPR_CONSTANT);

  e = ar->shape[i].start;
  start = (e != NULL) && (e->expr_type == EXPR_CONSTANT);

  e = ar->shape[i].end;
  end = (e != NULL) && (e->expr_type == EXPR_CONSTANT);

  e = ar->shape[i].stride;
  stride = (e != NULL) && (e->expr_type == EXPR_CONSTANT);

  if (lower && g95_extract_int(as->shape[i].lower, &lower_v) != NULL)
    goto oops;

  if (upper && g95_extract_int(as->shape[i].upper, &upper_v) != NULL)
    goto oops;

  if (start && g95_extract_int(ar->shape[i].start, &start_v) != NULL)
    goto oops;

  if (end && g95_extract_int(ar->shape[i].end, &end_v) != NULL) goto oops;

  if (stride && g95_extract_int(ar->shape[i].stride, &stride_v) != NULL)
    goto oops;

/* Given start, end and stride values, calculate the minimum and
 * maximum referenced indexes. */

  switch(ar->type) {
  case AR_FULL:
    break;

  case AR_ELEMENT:
    if (lower && start && start_v < lower_v) goto bound;
    if (upper && start && start_v > upper_v) goto bound;
    break;

  case AR_SECTION:
    if (stride && stride_v == 0) {
      g95_error("Illegal stride of zero at %L", &ar->shape[i].where);
      return FAILURE;
    }

    break;
  }

  return SUCCESS;

bound:
  g95_warning("Array reference at %L is out of bounds", &ar->shape[i].where);
  return SUCCESS;

oops:
  g95_internal_error("check_dimension(): Bad integer conversion");
  return FAILURE;
}


/* compare_spec_to_ref()-- Compare an array reference with an
 * array specification. */

try compare_spec_to_ref(g95_array_ref *ar, g95_array_spec *as) {
try t;
int i;

  if (ar->type == AR_FULL) return SUCCESS;

  if (as->rank != ar->rank) {
    g95_error("Array reference at %L is of rank %d but specified as rank %d",
              &ar->where, ar->rank, as->rank);
    return FAILURE;
  }

  t = SUCCESS;

  for(i=0; i<as->rank; i++)
    if (check_dimension(i, ar, as) == FAILURE) {
      t = FAILURE;
      break;
    }

  return t;
}


/* match_subscript()-- Match a single dimension of an array reference.
 * This can be a single element or an array section.  Any
 * modifications we've made to the ar structure are cleaned up by the
 * caller. */

static match match_subscript(g95_array_ref *ar, int init) {
g95_expr *e;
match m;
int i;

  i = ar->rank;

  ar->shape[i].where = *g95_current_locus();
  ar->shape[i].start = ar->shape[i].end = ar->shape[i].stride = NULL;

  if (g95_match(" :") == MATCH_YES) goto end_element;

  /* Get start element */

  if (init)
    m = g95_match_init_expr(&ar->shape[i].start);
  else
    m = g95_match_expr(&ar->shape[i].start);

  if (m == MATCH_NO) g95_error("Expected array subscript at %C");
  if (m != MATCH_YES) return MATCH_ERROR;

  e = ar->shape[i].start;
  if (e->as != NULL) {
    if (e->as->rank != 1) {
      g95_error("Vector subscript at %C must have rank of one");
      return MATCH_ERROR;
    }

    ar->type = AR_SECTION;
    return MATCH_YES;
  }

  if (g95_match(" :") == MATCH_NO) goto done;

/* Get an optional end element */

end_element:
  ar->type = AR_SECTION;

  if (init)
    m = g95_match_init_expr(&ar->shape[i].end);
  else
    m = g95_match_expr(&ar->shape[i].end);

  if (m == MATCH_ERROR) return MATCH_ERROR;

// Build UBOUND expression

/* See if we have an optional stride */

  if (g95_match(" :") == MATCH_NO)
    ar->shape[i].stride = g95_int_expr(1);
  else {
    if (init)
      m = g95_match_init_expr(&ar->shape[i].stride);
    else
      m = g95_match_expr(&ar->shape[i].stride);

    if (m == MATCH_NO) g95_error("Expected array subscript stride at %C");
    if (m != MATCH_YES) return MATCH_ERROR;
  }

done:
  return MATCH_YES;
}


/* g95_match_array_ref()-- Match an array reference, whether it is the
 * whole array or a particular elements or a section. */

match g95_match_array_ref(g95_array_ref *ar, g95_array_spec *as, int init) {
match m;

  memset(ar, '\0', sizeof(ar));

  ar->where = *g95_current_locus(); 
  ar->as = as;

  if (g95_match(" (") != MATCH_YES) {
    ar->type = AR_FULL;
    return MATCH_YES;
  }

/* The type gets changed by match_subscript() if it finds a section
 * reference */

  ar->type = AR_ELEMENT;

  for(ar->rank=0; ar->rank<G95_MAX_DIMENSIONS; ar->rank++) {
    m = match_subscript(ar, init);
    if (m == MATCH_ERROR) goto error;

    if (g95_match(" )") == MATCH_YES) goto matched;

    if (g95_match(" ,") != MATCH_YES) {
      g95_error("Invalid form of array reference at %C");
      goto error;
    }
  }

  g95_error("Array reference at %C cannot have more than "
	    stringize(G95_MAX_DIMENSIONS) " dimensions");

error:
  return MATCH_ERROR;

matched:
  ar->rank++;

  return MATCH_YES;
}


/* resolve_index()-- Resolve a single array index */

static try resolve_index(g95_expr *index) {
g95_typespec ts;

  if (g95_resolve_expr(index) == FAILURE) return FAILURE;

  if (index == NULL) return SUCCESS;

  if (!g95_numeric_ts(&index->ts)) {
    g95_error("Array index at %L must be of numeric type", &index->where);
    return FAILURE;
  }

  if (index->ts.type != BT_INTEGER ||
      index->ts.kind != g95_default_integer_kind()) {
    ts.type = BT_INTEGER;
    ts.kind = g95_default_integer_kind();

    g95_convert_type(index, &ts, 2);
  }

  return SUCCESS;
}


/* g95_resolve_array_ref()-- Resolve an array reference */

try g95_resolve_array_ref(g95_array_ref *ar, g95_array_spec *as) {
try t;
int i;

  t = SUCCESS; 
  for(i=0; i<G95_MAX_DIMENSIONS; i++) {
    if (resolve_index(ar->shape[i].start) == FAILURE) t = FAILURE;
    if (resolve_index(ar->shape[i].end) == FAILURE) t = FAILURE;
    if (resolve_index(ar->shape[i].stride) == FAILURE) t = FAILURE;
  }

  if (compare_spec_to_ref(ar, as) == FAILURE) t = FAILURE;

  return t;
}


/************** Array specification matching subroutines ***************/

/* g95_free_array_spec()-- Free all of the expressions associated with
 * array bounds specifications */

void g95_free_array_spec(g95_array_spec *as) {
int i;

  if (as == NULL) return; 

  for(i=0; i<as->rank; i++) {
    g95_free_expr(as->shape[i].lower);
    g95_free_expr(as->shape[i].upper);
  }

  g95_free(as);
}


/* g95_resolve_array_spec()-- Takes an array specification, resolves
 * the expressions that make up the shape and make sure everything is
 * integral. */

void g95_resolve_array_spec(g95_array_spec *as) {
g95_expr *e;
int i;

  for(i=0; i<as->rank; i++) {
    e = as->shape[i].lower;

    if (e != NULL) {
      g95_resolve_expr(e);
      if (e->ts.type != BT_INTEGER)
	g95_error("Array specification at %L must be of INTEGER type",
		  &e->where);
    }

    e = as->shape[i].upper;

    if (e != NULL) {
      g95_resolve_expr(e);
      if (e->ts.type != BT_INTEGER)
	g95_error("Array specification at %L must be of INTEGER type",
		  &e->where);
    }
  }
}


/* match_array_element_spec()-- Match a single array element
 * specification.  The return values as well as the upper and lower
 * bounds of the array spec are filled in according to what we see on
 * the input.  The caller makes sure individual specifications make
 * sense as a whole.
 *
 *       Parsed       Lower   Upper  Returned
 *       ------------------------------------
 *         :          NULL    NULL   AS_DEFERRED
 *         x           1       x     AS_EXPLICIT
 *         x:          x      NULL   AS_ASSUMED_SHAPE
 *         x:y         x       y     AS_EXPLICIT
 *         x:*         x      NULL   AS_ASSUMED_SIZE
 *         *           1      NULL   AS_ASSUMED_SIZE
 * Anything else AS_UNKNOWN */

static array_type match_array_element_spec(g95_array_spec *as) {
g95_expr **upper, **lower;
match m;

  lower = &as->shape[as->rank - 1].lower;
  upper = &as->shape[as->rank - 1].upper;

  if (g95_match(" *") == MATCH_YES) {
    *lower = g95_int_expr(1);
    return AS_ASSUMED_SIZE;
  }

  if (g95_match(" :") == MATCH_YES) return AS_DEFERRED;

  m = g95_match(" %e", upper);
  if (m == MATCH_NO)
    g95_error("Expected expression in array specification at %C");
  if (m != MATCH_YES) return AS_UNKNOWN;

  if (g95_match(" :") == MATCH_NO) {
    *lower = g95_int_expr(1);
    return AS_EXPLICIT;
  }

  *lower = *upper;
  *upper = NULL;

  if (g95_match(" *") == MATCH_YES) return AS_ASSUMED_SIZE;

  m = g95_match(" %e", upper);
  if (m == MATCH_ERROR) {
    g95_free_expr(*lower);
    return AS_UNKNOWN;
  }

  if (m == MATCH_NO) return AS_ASSUMED_SHAPE;

  return AS_EXPLICIT;
}


/* g95_match_array_spec()-- Matches an array specification,
 * incidentally figuring out what sort it is.  */

match g95_match_array_spec(g95_array_spec **asp) {
array_type current_type;
g95_array_spec *as;
int i;

  if (g95_match(" (") != MATCH_YES) {
    *asp = NULL;
    return MATCH_NO;
  }

  as = g95_get_array_spec();

  for(i=0; i<G95_MAX_DIMENSIONS; i++) {
    as->shape[i].lower = NULL;
    as->shape[i].upper = NULL;
  }

  as->rank = 1;

  for(;;) {
    current_type = match_array_element_spec(as);

    if (as->rank == 1) {
      if (current_type == AS_UNKNOWN) goto cleanup;
      as->type = current_type;
    } else
      switch(as->type) { /* See how current spec meshes with the existing */
        case AS_UNKNOWN:
	  goto cleanup;

        case AS_EXPLICIT:
	  if (current_type == AS_ASSUMED_SIZE) {
	    as->type = AS_ASSUMED_SIZE;
	    break;
	  }

	  if (current_type == AS_EXPLICIT) break;

	  g95_error("Bad array specification for an explicitly shaped array"
		    " at %C");

	  goto cleanup;
	
        case AS_ASSUMED_SHAPE:
	  if ((current_type == AS_ASSUMED_SHAPE) ||
	      (current_type == AS_DEFERRED)) break;

	  g95_error("Bad array specification for assumed shape array at %C");
	  goto cleanup;

        case AS_DEFERRED:
	  if (current_type == AS_DEFERRED) break;

	  if (current_type == AS_ASSUMED_SHAPE) {
	    as->type = AS_ASSUMED_SHAPE;
	    break;
	  }

	  g95_error("Bad specification for deferred shape array at %C");
	  goto cleanup;
	  
        case AS_ASSUMED_SIZE:
	  g95_error("Bad specification for assumed size array at %C");
	  goto cleanup;	  
      }

    if (g95_match(" )") == MATCH_YES) break;

    if (g95_match(" ,") != MATCH_YES) {
      g95_error("Expected another dimension in array declaration at %C");
      goto cleanup;
    }

    if (as->rank >= G95_MAX_DIMENSIONS) {
      g95_error("Array specification at %C has more than "
		stringize(G95_MAX_DIMENSIONS) " dimensions");
      goto cleanup;
    }

    as->rank++;
  }

/* If a lower bounds of an assumed shape array is blank, put in one. */

  if (as->type == AS_ASSUMED_SHAPE) {
    for(i=0; i<as->rank; i++) {
      if (as->shape[i].lower == NULL)
	as->shape[i].lower = g95_int_expr(1);
    }
  }

  *asp = as;
  return MATCH_YES;

/* Something went wrong */

cleanup:
  g95_free_array_spec(as);

  return MATCH_ERROR;
}


/* g95_set_array_spec()-- Given a symbol and an array specification,
 * modify the symbol to have that array specification.  The error
 * locus is needed in case something goes wrong.  On failure, the
 * caller must free the spec. */

try g95_set_array_spec(g95_symbol *sym, g95_array_spec *as, locus *error_loc) {

  if (as == NULL) return SUCCESS;

  if (g95_add_dimension(&sym->attr, error_loc) == FAILURE) return FAILURE;

  sym->as = as;

  return SUCCESS;
}


/* copy_array_spec()-- Copy an array specification. */

g95_array_spec *g95_copy_array_spec(g95_array_spec *src) {
g95_array_spec *dest;
int i;

  if (src == NULL) return NULL;

  dest = g95_get_array_spec();

  *dest = *src;

  for(i=0; i<dest->rank; i++) {
    dest->shape[i].lower = g95_copy_expr(dest->shape[i].lower);
    dest->shape[i].upper = g95_copy_expr(dest->shape[i].upper);
  }

  return dest;
}



static mstring array_specs[] = {
  minit("AS_EXPLICIT", AS_EXPLICIT),
  minit("AS_ASSUMED_SHAPE", AS_ASSUMED_SHAPE),
  minit("AS_DEFERRED", AS_DEFERRED),
  minit("AS_ASSUMED_SIZE", AS_ASSUMED_SIZE),
  minit(NULL, 0) };

void g95_show_array_spec(g95_array_spec *as) {
int i;

  if (as == NULL) {
    g95_status("()");
    return;
  }

  g95_status("(%d", as->rank);

  if (as->rank != 0) {
    g95_status(" %s ", g95_code2string(array_specs, as->type));

    for(i=0; i<as->rank; i++) {
      g95_show_expr(as->shape[i].lower);
      g95_status_char(' ');
      g95_show_expr(as->shape[i].upper);
      g95_status_char(' ');
    }
  }

  g95_status(")");
}


/* g95_compare_array_spec()-- Does what it says.  MATCH_ERROR is never
 * returned. */

int g95_compare_array_spec(g95_array_spec *as1, g95_array_spec *as2) {
int i, a1, a2;

  if (as1 == NULL && as2 == NULL) return 1;

  if (as1 == NULL || as2 == NULL) return 0;

  if (as1->rank != as2->rank) return 0;

  if (as1->rank == 0) return 1;

  if (as1->type != as2->type) return 0;

  if (as1->type == AS_EXPLICIT)
    for(i=0; i<as1->rank; i++) {
      if (g95_extract_int(as1->shape[i].lower, &a1) != NULL) goto error;
      if (g95_extract_int(as2->shape[i].lower, &a2) != NULL) goto error;
      if (a1 != a2) return 0;

      if (g95_extract_int(as1->shape[i].upper, &a1) != NULL) goto error;
      if (g95_extract_int(as2->shape[i].upper, &a2) != NULL) goto error;
      if (a1 != a2) return 0;
    }

  return 1;

error:
  g95_internal_error("g95_compare_type(): Array spec clobbered");
  return 0;        /* Keep the compiler happy */
}



/****************** Array constructor functions ******************/

/* g95_free_constructor()-- Free chains of g95_constructor structures */

void g95_free_constructor(g95_constructor *p) {
g95_constructor *next;

  if (p == NULL) return;

  for(;p ;p=next) {
    next = p->next;

    g95_free_constructor(p->child);
    g95_free_expr(p->expr);
    if (p->iterator != NULL) g95_free_iterator(p->iterator, 1);
    g95_free(p);
  }
}


/* check_iterators()-- Given a constructor node and a symbol, make
 * sure than no iterators in this or child constructors use the symbol
 * as an implied-DO iterator.  Returns nonzero if a duplicate was found. */

static int check_duplicate_iterator(g95_constructor *cons,
				    g95_symbol *master) {

  for(; cons; cons=cons->next) {
    if (check_duplicate_iterator(cons->child, master)) return 1;
    if (cons->iterator == NULL) continue;

    if (cons->iterator->var->symbol == master) {
      g95_error("DO-iterator '%s' at %L is inside iterator of the same name",
		master->name, &cons->where);

      return 1;
    }
  }

  return 0;
}


/* match_array_list()-- Match a list of array elements. */

static match match_array_cons_element(g95_constructor **);

static match match_array_list(g95_constructor **result) {
g95_constructor *p, *head, *tail, *new;
g95_iterator iter;
locus old_loc;
match m;

  old_loc = *g95_current_locus();

  if (g95_match(" (") == MATCH_NO) return MATCH_NO;

  memset(&iter, '\0', sizeof(g95_iterator));

  m = match_array_cons_element(&head);
  if (m != MATCH_YES) {
    g95_set_locus(&old_loc);
    return m;
  }

  tail = head;

  if (g95_match(" ,") != MATCH_YES) {
    g95_free_constructor(head);
    g95_set_locus(&old_loc);
    return MATCH_NO;
  }

  for(;;) {
    m = g95_match_iterator(&iter);
    if (m == MATCH_YES) break;
    if (m == MATCH_ERROR) goto cleanup;

    m = match_array_cons_element(&new);
    if (m == MATCH_ERROR) goto cleanup;
    if (m == MATCH_NO) goto syntax;

    tail->next = new;
    tail = new;

    if (g95_match(" ,") != MATCH_YES) break;
  }

  if (g95_match(" )") != MATCH_YES) goto syntax;

  if (check_duplicate_iterator(head, iter.var->symbol)) goto cleanup;

  p = g95_get_constructor();
  p->where = *g95_current_locus();
  p->iterator = g95_get_iterator();
  *p->iterator = iter;

  p->child = head;
  *result = p;

  return MATCH_YES;

syntax:
  g95_error("Syntax error in array constructor at %C");

cleanup:
  g95_free_constructor(head);
  g95_free_iterator(&iter, 0);
  return MATCH_ERROR;
}


/* check_constructor_type()-- Given an expression, compare it's type
 * with the type of the constructor.  Returns nonzero if an error was
 * issued.  The cons_state variable keeps track of whether the type of
 * the constructor being read or resolved is known to be good, bad or
 * just starting out. */

static g95_typespec constructor_ts;
static enum { CONS_START, CONS_GOOD, CONS_BAD } cons_state;

static int check_constructor_type(g95_expr *expr) {

  if (cons_state == CONS_BAD) return 0;  /* Supress further errors */

  if (cons_state == CONS_START) {
    if (expr->ts.type == BT_UNKNOWN)
      cons_state = CONS_BAD;
    else {
      cons_state = CONS_GOOD;
      constructor_ts = expr->ts;
    }

    return 0;
  }

  if (constructor_ts.type != expr->ts.type) {
    g95_error("Element in %s array constructor at %L is %s",
	      g95_typename(constructor_ts.type), &expr->where,
	      g95_typename(expr->ts.type));

    cons_state = CONS_BAD;
    return 1;
  }

  if (constructor_ts.type == BT_DERIVED) {
    if (constructor_ts.derived == expr->ts.derived) return 0;

    g95_error("Element in DERIVED %s array constructor at %L is "
	      "DERIVED %s", constructor_ts.derived->name, &expr->where,
	      expr->ts.derived->name);

    cons_state = CONS_BAD;
    return 1;
  }

  if (constructor_ts.kind != expr->ts.kind) {
    g95_error("Element in %s kind %d array constructor at %L is "
	      "%s kind %d", g95_typename(constructor_ts.type),
	      constructor_ts.kind, &expr->where,
	      g95_typename(expr->ts.type), expr->ts.kind);

    cons_state = CONS_BAD;
    return 1;
  }

  return 0;
}


/* match_array_cons_element()-- match a single element of an array
 * constructor, which can be a single expression or a list of
 * elements. */

static match match_array_cons_element(g95_constructor **result) {
g95_constructor *p;
g95_expr *expr;
match m;

  m = match_array_list(result);
  if (m != MATCH_NO) return m;

  m = g95_match_expr(&expr);
  if (m != MATCH_YES) return m;

  p = g95_get_constructor();
  p->where = *g95_current_locus();
  p->expr = expr;

  *result = p;
  return MATCH_YES;  
}


/* g95_match_array_constructor()-- Match an array constructor */

match g95_match_array_constructor(g95_expr **result) {
g95_constructor *head, *tail, *new;
g95_expr *expr;
locus where;
match m;

  if (g95_match(" (/") == MATCH_NO) return MATCH_NO;

  where = *g95_current_locus();
  head = NULL;

  if (g95_match(" /)") == MATCH_YES) goto empty;   /* Special case */

  for(;;) {
    m = match_array_cons_element(&new);
    if (m == MATCH_ERROR) goto cleanup;
    if (m == MATCH_NO) goto syntax;

    if (head == NULL)
      head = new;
    else
      tail->next = new;

    tail = new;

    if (g95_match(" ,") == MATCH_NO) break;
  }

  if (g95_match(" /)") == MATCH_NO) goto syntax;

empty:
  expr = g95_get_expr();

  expr->expr_type = EXPR_ARRAY;

  expr->value.constructor = head;
  expr->where = where;

  *result = expr;
  return MATCH_YES;

syntax:
  g95_error("Syntax error in array constructor at %C");

cleanup:
  g95_free_constructor(head);
  return MATCH_ERROR;
}


typedef struct cons_stack {
  g95_iterator *iterator;
  struct cons_stack *previous;
} cons_stack;

static cons_stack *base;
static int array_constructor_flag;


/* check_constructor()-- Recursive work function for
 * g95_check_constructor().  This amounts to calling the check
 * function for each expression in the constructor, giving variables
 * with the names of iterators a pass.  */

static try check_constructor(g95_constructor *cons,
			     match (*check_function)(g95_expr *)) {
cons_stack *c, element;
g95_symbol *sym;

  for(; cons; cons=cons->next) {
    if (cons->expr != NULL) {
      if (cons->expr->expr_type == EXPR_VARIABLE) {
	sym = cons->expr->symbol;

	for(c=base; c; c=c->previous)
	  if (sym == c->iterator->var->symbol) goto ok;

	g95_error("Variable '%s' at %L isn't an implied DO-iterator.  "
		  "Constructor is not constant", sym->name, &cons->where);

	return FAILURE;
      }

      if ((*check_function)(cons->expr) == FAILURE) return FAILURE;

      if (array_constructor_flag && check_constructor_type(cons->expr))
	return FAILURE;
    }

  ok:
    if (cons->iterator == NULL) continue;

    element.previous = base;
    element.iterator = cons->iterator;

    base = &element;
    check_constructor(cons->child, check_function);
    base = element.previous;
  }

/* Nothing went wrong, so all OK */

  return SUCCESS;
}


/* g95_check_constructor()-- Checks a constructor to see if it is a
 * particular kind of expression-- specification, restricted,
 * or initialization as determined by the check_function.  */

try g95_check_constructor(g95_expr *expr, int array_constructor,
			  match (*check_function)(g95_expr *)) {
cons_stack *base_save;
int old_flag;
try t;

  base_save = base;
  base = NULL;

  old_flag = array_constructor_flag;
  array_constructor_flag = array_constructor;

  cons_state = CONS_START;

  t = check_constructor(expr->value.constructor, check_function);

  if (array_constructor && t == SUCCESS && expr->ts.type == BT_UNKNOWN)
    expr->ts = constructor_ts;

  base = base_save;
  array_constructor_flag = old_flag;

  return t;
}



/**************** Simplification of array constructors ****************/

typedef struct iterator_stack {
  g95_symbol *variable;
  mpz_t value;
  struct iterator_stack *prev;
} iterator_stack;

static iterator_stack *iter_stack;
static g95_constructor *new_head, *new_tail;

static try expand_constructor(g95_constructor *);

/* g95_simplify_iteration_var()-- Given an initialization expression
 * that is a variable reference, substitute the current value of the
 * iteration variable. */

void g95_simplify_iterator_var(g95_expr *e) {
iterator_stack *p;

  for(p=iter_stack; p; p=p->prev)
    if (e->symbol == p->variable) break;

  if (p == NULL)
    g95_internal_error("simplify_iteration_expr(): Variable '%s' not found",
		       e->symbol->name);

  g95_replace_expr(e, g95_int_expr(0));

  mpz_set(e->value.integer, p->value);
}


static try expand_iterator(g95_constructor *c) {
g95_expr *start, *end, *step;
iterator_stack frame;
mpz_t trip;
try t;

  start = end = step = NULL;

  t = FAILURE;

  mpz_init(trip);
  mpz_init(frame.value);

  start = g95_copy_expr(c->iterator->start);
  if (g95_simplify_expr(start, 1) == FAILURE) goto cleanup;

  if (start->expr_type != EXPR_CONSTANT || start->ts.type != BT_INTEGER) {
    g95_error("Iterator start at %L must be a constant integer",
	      &start->where);
    goto cleanup;
  }

  end = g95_copy_expr(c->iterator->end);
  if (g95_simplify_expr(end, 1) == FAILURE) goto cleanup;

  if (end->expr_type != EXPR_CONSTANT || end->ts.type != BT_INTEGER) {
    g95_error("Iterator end at %L must be a constant integer",
	      &end->where);
    goto cleanup;
  }

  step = g95_copy_expr(c->iterator->step);
  if (g95_simplify_expr(step, 1) == FAILURE) goto cleanup;

  if (step->expr_type != EXPR_CONSTANT || step->ts.type != BT_INTEGER) {
    g95_error("Iterator step at %L must be a constant integer",
	      &step->where);
    goto cleanup;
  }

  if (mpz_sgn(step->value.integer) == 0) {
    g95_error("Iterator step at %L cannot be zero",
	      &step->where);
    goto cleanup;
  }

  /* Calculate the trip count of the loop */

  mpz_sub(trip, end->value.integer, start->value.integer);
  mpz_add(trip, trip, step->value.integer);
  mpz_tdiv_q(trip, trip, step->value.integer);

  mpz_set(frame.value, start->value.integer);

  frame.prev = iter_stack;
  frame.variable = c->iterator->var->symbol;
  iter_stack = &frame;

  while(mpz_sgn(trip) > 0) {
    if (expand_constructor(c->child) == FAILURE) goto cleanup;

    mpz_add(frame.value, frame.value, step->value.integer);
    mpz_sub_ui(trip, trip, 1);
  }

  t = SUCCESS;

cleanup:
  g95_free_expr(start);
  g95_free_expr(end);
  g95_free_expr(step);

  mpz_clear(trip);
  mpz_clear(frame.value);

  iter_stack = frame.prev;

  return t;
}


/* expand_constructor()-- Expand a constructor into constant
 * constructors without any iterators. */

static try expand_constructor(g95_constructor *c) {

  for(; c; c=c->next) {
    if (c->iterator != NULL) {
      if (expand_iterator(c) == FAILURE) return FAILURE;
      continue;
    }

    if (c->child != NULL) {
      if (expand_constructor(c->child) == FAILURE) return FAILURE;
      continue;
    }

    if (c->expr != NULL) {
      if (new_head == NULL)
	new_head = new_tail = g95_get_constructor();
      else {
	new_tail->next = g95_get_constructor();
	new_tail = new_tail->next;
      }

      new_tail->where = c->where;
      new_tail->expr = g95_copy_expr(c->expr);

      if (g95_simplify_expr(new_tail->expr, 1) == FAILURE) return FAILURE;
      continue;
    }

    g95_internal_error("expand_constructor(): Bad constructor node");
  }

  return SUCCESS;
}


/* g95_expand_constructor()-- Top level subroutine for expanding
 * constructors. */

try g95_expand_constructor(g95_constructor **cp) {

  new_head = new_tail = NULL;
  iter_stack = NULL;

  if (expand_constructor(*cp) == FAILURE) {
    g95_free_constructor(new_head);
    return FAILURE;
  }

  g95_free_constructor(*cp);

  *cp = new_head;

  return SUCCESS;
}


/* count_elements()-- Recursive functions to count the number of
 * elements in a constructor.  If we hit an iterator, we give up and
 * return -1.  */

static int count_elements(g95_constructor *c) {
g95_expr *e;
int i, total;

  total = 0;

  for(; c; c=c->next) {
    e = c->expr;
    i = 0;

    if (e != NULL) {
      if (e->expr_type == EXPR_ARRAY)
	i = count_elements(e->value.constructor);
      else
	i = 1;
    } else if (c->iterator != NULL) i = -1;

    if (i == -1) {
      total = -1;
      break;
    }

    total += i;
  }

  return total;
}


/* g95_size_constuctor()-- Given an expression node that represents an
 * array constructor, attempt to figure out how large the array is.
 * Constructors are always rank-1 arrays. */

void g95_size_constructor(g95_expr *e) {
g95_array_spec *as;
int size;

  size = count_elements(e->value.constructor);

  if (e->as != NULL) g95_free_array_spec(e->as);

  as = g95_get_array_spec();
  e->as = as;

  if (size == -1)
    as->type = AS_UNKNOWN;
  else {
    as->type = AS_EXPLICIT;
    as->rank = 1;

    as->shape[0].lower = g95_int_expr(1);
    as->shape[0].upper = g95_int_expr(size);
  }    
}


/* resolve_array_list()-- Recursive array list resolution function.
 * All of the elements must be of the same type. */

static try resolve_array_list(g95_constructor *p) {
try t;

  for(;p ;p=p->next) {

    if (p->child != NULL) {
      if (resolve_array_list(p->child) == FAILURE) t = FAILURE;
      if (g95_resolve_iterator(p->iterator) == FAILURE) t = FAILURE;
    }

    if (p->expr == NULL) continue;

    if (g95_resolve_expr(p->expr) == FAILURE) {
      t = FAILURE;
      continue;
    }

    if (check_constructor_type(p->expr)) t = FAILURE;
  }

  return t;
}


/* g95_resolve_array_constructor()-- Resolve all of the expressions in
 * an array list.  TODO: String lengths. */

try g95_resolve_array_constructor(g95_expr *expr) {
try t;

  g95_clear_ts(&constructor_ts); 

  t = resolve_array_list(expr->value.constructor);

  if (cons_state == CONS_GOOD) expr->ts = constructor_ts;
  expr->expr_type = EXPR_ARRAY;

  return t;
}


/* copy_iterator()-- Copy an iterator structure */

static g95_iterator *copy_iterator(g95_iterator *src) {
g95_iterator *dest;

  if (src == NULL) return NULL;

  dest = g95_get_iterator();
  
  dest->var = g95_copy_expr(src->var);
  dest->start = g95_copy_expr(src->start);
  dest->end = g95_copy_expr(src->end);
  dest->step = g95_copy_expr(src->step);

  return dest;
}


/* g95_copy_constructor()-- Copy a constructor structure. */

g95_constructor *g95_copy_constructor(g95_constructor *src) {
g95_constructor *dest;

  if (src == NULL) return NULL;

  dest = g95_get_constructor();
  dest->where = src->where;
  dest->expr = g95_copy_expr(src->expr);
  dest->iterator = copy_iterator(src->iterator);

  dest->next = g95_copy_constructor(src->next);
  dest->child = g95_copy_constructor(dest->child);

  return dest;
}

