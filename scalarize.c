  
/* Array Scalarization
   Copyright (C) 2000 - 2003 Free Software Foundation, Inc.
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
          
#include "g95.h"
       
/* Scalarization is the process of converting a single vector
 * expression to multiple scalar expressions.  The process works by
 * traversing all code nodes, examining expressions within these
 * nodes.  A single code node can be expanded to multiple code nodes
 * depending on the expressions it contains.  New nodes are inserted
 * before and after the original node.
 *
 * There are three cases that we have to deal with:
 *     Vector subexpressions inside scalar expressions
 *     Assigning a scalar to a vector
 *     Assigning a vector to a vector
 *
 * Vector subexpressions within scalar expressions can only happen as
 * actual arguments.  The back end handles cases of passing whole
 * arrays or sections.  We transform a general array expression into
 * an assignment to a temporary then pass the full array.
 *
 * Assigning a scalar to a vector amounts to computing the RHS to a
 * temporary, then looping over the array and assigning the scalar.
 *
 * The vector to vector assignment is more involved and has several
 * subcases. */      
      
      
/* When nodes are inserted prior to the current node, the current node
 * has to be moved.  This pointer points to the node being scalarized
 * and can change during the process. */        
        
static g95_code *current_code;  
  
#ifdef IN_GCC

static void scalarize_scalar_expr(g95_expr *);         
static void traverse_code(g95_code *);  
  
  
   
   
/* scalarize_actual_arg()-- Given an actual argument, see if it is an
 * array expression that can be passed directly or not.  If not,
 * generate the assignment to a temporary, pass the temporary and
 * generate any cleanup code.  The temporary assignment is scalarized
 * separately. */    
    
static void scalarize_actual_arg(g95_expr *arg) {

  if (arg == NULL || arg->rank == 0) return;    
    
  /*TODO */      
      
  /* scalarize_vector_assignment(); */       
} 
 
 
    
    
/* get_variable_expr()-- Given a symbol, create an expression node with that
 * symbol as a variable. */          
          
static g95_expr *get_variable_expr(g95_symbol *var) {     
g95_expr *e;  
  
  e = g95_get_expr();     
  e->type = EXPR_VARIABLE;       
  e->symbol = var;   
   
  e->rank = (var->as == NULL) ? 0 : var->as->rank;      
  e->ts = var->ts;

  return e;         
}         
         
         
          
          
/* bound_expr()-- Given an array, a dimension and a upper/lower flag,
 * generate an expression that is the upper or lower bound intrinsic
 * for that dimension. */          
          
static g95_expr *bound_expr(g95_expr *array, int dimension, int upper_flag) {        
g95_actual_arglist *b;        
g95_expr *c;  
  
  c = g95_get_expr();   
  c->type = EXPR_FUNCTION;     
  c->value.function.isym = g95_find_function(upper_flag ? "ubound" : "lbound");        
  c->value.function.name = c->value.function.isym->name;         
         
  c->value.function.actual = b = g95_get_actual_arglist();  
  b->type = FULL_ARRAY;
  b->u.expr = array;
  /* a->pointer = ??? */ 
 
  b->next = g95_get_actual_arglist();       
  b = b->next;         
         
  b->type = EXPR;    
  b->u.expr = g95_int_expr(dimension+1);        
        
  return c; 
}      
      
      
        
        
/* scalarize_actual_arglist()-- Given an actual argument list, go
 * through it and replace vector expressions (besides full array
 * references) with a precalculated temporary. */          
          
static void scalarize_actual_arglist(g95_actual_arglist *actual) {       
       
  for(; actual; actual=actual->next)     
    if (actual->type != EXPR)    
      scalarize_actual_arg(actual->u.expr);     
}    
    
    
/* scalarize_scalar_ref()-- Scalarize a variable reference. */ 
 
        
        
/* full_array()-- Given an expression, return the full array part of
 * the reference.  The expression node is a variable node with a set
 * of reference structures.  We keep everything up to the array
 * section reference. */   
   
static g95_expr *full_array(g95_expr *c) {    
g95_ref *ref, *b; 
 
  c = g95_copy_expr(c);   
   
  if (c->ref->type == REF_ARRAY &&       
      (c->ref->u.ar.type == AR_SECTION ||  
       c->ref->u.ar.type == AR_FULL)) {   
    ref = c->ref;   
    c->ref = NULL;   
  } else {       
    b = c->ref;      
    while(b->next->type != REF_ARRAY ||         
	  (b->u.ar.type != AR_SECTION && b->u.ar.type != AR_FULL)) 
      b = b->next;      
      
    ref = b->next; 
    b->next = NULL;       
  }        
        
  g95_free_ref_list(ref);     
  return c; 
}          
          
          
         
         
static void scalarize_scalar_ref(g95_ref *reference) { 
int p;         
         
  for(; reference; reference=reference->next) {   
    switch(reference->type) {      
    case REF_ARRAY:          
      for(p=0; p<reference->u.ar.dimen; p++) {         
	scalarize_scalar_expr(reference->u.ar.start[p]);        
	scalarize_scalar_expr(reference->u.ar.end[p]);
	scalarize_scalar_expr(reference->u.ar.stride[p]);        
      }       
       
      break;    
    
    case REF_COMPONENT:    
      break;         
         
    case REF_SUBSTRING:      
      scalarize_scalar_expr(reference->u.ss.start);          
      scalarize_scalar_expr(reference->u.ss.end);  
      break;          
    }      
  }        
}    
    
    
  
  
/* insert_code_node()-- Given a code node, insert it into the current
 * list.  If pre is nonzero, the node is inserted before the current
 * node, otherwise it is inserted after the current node. */       
       
static void insert_code_node(g95_code *new, int pre) {        
g95_code temp;          
          
  if (pre) {       
    temp = *current_code; 
    *current_code = *new;  
    *new = temp;         
         
    current_code->next = new;    
    current_code = new;    
  } else {    
    new->next = current_code->next;         
    current_code->next = new;          
  }        
}        
        
        


/* get_temporary()-- Creates a temporary symbol which is a
 * variable of a particular type and rank.  If the variable has
 * nonzero rank, it is marked as allocatable with a deferred array
 * specification. */  
  
static g95_symbol *get_temporary(g95_typespec *ts, int rank) {  
char name[G95_MAX_SYMBOL_LEN+1];     
static int serial = 0;     
g95_array_spec *as;
g95_symtree *st;     
g95_symbol *symbol; 
 
  sprintf(name, "SC.%d", serial++);        
  symbol = g95_new_symbol(name, g95_current_ns); 
 
  symbol->ts = *ts;  
  symbol->attr.flavor = FL_VARIABLE;          
  symbol->attr.used = 1;    
  symbol->refs = 1;          
          
  if (rank > 0) {     
    as = symbol->as = g95_get_array_spec();       
    as->rank = rank;          
    as->type = AS_DEFERRED;

    symbol->attr.allocatable = 1;       
  }       
       
  st = g95_new_symtree(&g95_current_ns->sym_root, name);         
  st->n.sym = symbol;   
   
  return symbol;         
}     
     
     
       
       
/* scalarize_scalar_expr()-- Scalarize an expression that is
 * ultimately scalar.  The only way an array expression can appear in
 * these expressions is inside an actual argument. */       
       
static void scalarize_scalar_expr(g95_expr *q) {     
     
  if (q == NULL) return;

  switch(q->type) {      
  case EXPR_SUBSTRING:          
  case EXPR_OP: 
    scalarize_scalar_expr(q->op1);    
    scalarize_scalar_expr(q->op2);   
    break;   
   
  case EXPR_FUNCTION:        
    scalarize_actual_arglist(q->value.function.actual);     
    break;  
  
  case EXPR_VARIABLE:    
    scalarize_scalar_ref(q->ref);         
    break;       
       
  default:  
    break;       
  }       
}  
  
  
       
       
/* get_loopvar()-- Allocate an integer loop variable */          
          
static g95_symbol *get_loopvar(void) {  
g95_typespec ts;        
        
  ts.type = BT_INTEGER;    
  ts.kind = g95_default_integer_kind(); 
 
  return get_temporary(&ts, 0); 
}      
      
      
  
  
/* transform_range()-- Given an array reference structure and a
 * dimension of that structure which is a DIMEN_RANGE, replace the
 * range with the equivalent loop.  Returns a code node corresponding
 * to the loop. */        
        
static g95_code *transform_range(g95_array_ref *ar, int dimension) {   
g95_symbol *loop_var;   
g95_iterator *iter;    
g95_expr *array;
g95_code *v;

  loop_var = get_loopvar();

  iter = g95_get_iterator();
  iter->var = get_variable_expr(loop_var);         
         
  v = g95_get_code();          
  v->type = EXEC_DO;    
  v->ext.iterator = iter;          
          
  if (ar->start[dimension] == NULL) {       
    array = full_array(current_code->expr);        
    iter->start = bound_expr(array, dimension, 0); 
  } else { 
    iter->start = ar->start[dimension];       
    ar->start[dimension] = NULL;   
  }          
          
  if (ar->end[dimension] == NULL) {      
    array = full_array(current_code->expr);      
    iter->end = bound_expr(array, dimension, 1);         
  } else {         
    iter->end = ar->end[dimension];      
    ar->end[dimension] = NULL;       
  }        
        
  if (ar->stride[dimension] == NULL)   
    iter->step = g95_int_expr(1);   
  else {         
    iter->step = ar->stride[dimension];    
    ar->stride[dimension] = NULL;  
  }

  ar->dimen_type[dimension] = DIMEN_ELEMENT;          
  ar->start[dimension] = get_variable_expr(loop_var);        
        
  return v;  
} 
 
 
 
/*@static void scalarize_vector_assignment*/      
      
static void scalarize_vector_assignment(void) { }         
         
         
 
 
/* scalarize_scalar_assignment()-- Scalarize an assignment statement
 * that assigns a scalar to an array. */  
  
static void scalarize_scalar_assignment(void) {     
g95_code *p, *s, *loops;       
g95_symbol *temp_var;         
g95_ref *reference;         
int rank, v;  
  
  temp_var = get_temporary(&current_code->expr2->ts, 0);    
  rank = current_code->expr->rank;   
   
  p = g95_get_code();        
  p->type = EXEC_ASSIGN;    
  p->expr = get_variable_expr(temp_var);   
  p->expr2 = current_code->expr2;

  insert_code_node(p, 1);   
   
  p = g95_get_code();    
  p->type = EXEC_ASSIGN;  
  p->expr = current_code->expr; 
  p->expr2 = get_variable_expr(temp_var);      
  p->expr->rank = 0;        
        
  /* Find the section specification. */  
  
  reference = current_code->expr->ref;        
  while(reference->type != REF_ARRAY && reference->u.ar.type != AR_SECTION)    
    reference = reference->next;

  loops = NULL;    
    
  switch(reference->u.ar.type) {      
  case AR_SECTION:   
    rank = reference->u.ar.dimen;     
     
    for(v=0; v<rank; v++)    
      switch(reference->u.ar.dimen_type[v]) {    
      case DIMEN_ELEMENT:  
	break;     /* Do nothing */   
   
      case DIMEN_RANGE:         
	s = transform_range(&reference->u.ar, v);          
	s->block = (loops == NULL) ? p : loops;   
	loops = s;
	break;        
        
      case DIMEN_VECTOR:        
	g95_internal_error("Can't do array subscripts yet");          
          
      default:   
	g95_internal_error("scalarize_scalar_assignment(): Bad dimension");  
      } 
 
    break;

  case AR_FULL:      
    reference->u.ar.dimen = rank;        
        
    for(v=0; v<rank; v++) {        
      s = transform_range(&reference->u.ar, v);      
      s->block = (loops == NULL) ? p : loops;         
      loops = s;         
    }         
         
    break;    
    
  default:          
    g95_internal_error("scalarize_scalar_assignment(): Bad ref");     
  }      
      
  reference->u.ar.type = AR_ELEMENT;    
    
  /* Now graft the top loop where the original expression is */       
       
  loops->next = current_code->next;    
  *current_code = *loops;  
  
  g95_free(loops);         
}         
         
         
  
  
/* g95_scalarize()-- Scalarize a namepace, it's child and siblings
 * namespaces */          
          
void g95_scalarize(g95_namespace *ns) {    
g95_namespace *save;          
          
  if (ns == NULL) return;     
     
  save = g95_current_ns;          
  g95_current_ns = ns;   
  traverse_code(ns->code);      
      
  for(ns=ns->contained; ns; ns=ns->sibling)         
    g95_scalarize(ns);       
       
  g95_current_ns = save;       
}

        
        
/* scalarize_code()-- Scalarize a single code node. */ 
 
static void scalarize_code(g95_code *code) {  
g95_alloc *y;     
     
  if (code == NULL) return; 
 
  current_code = code;          
          
  /* Vector expressions here are in assignment nodes and are taken
   * care of elsewhere. */         
         
  if (code->expr != NULL && code->expr->rank == 0)     
    scalarize_scalar_expr(code->expr);     
     
  if (code->expr2 != NULL && code->expr2->rank == 0) 
    scalarize_scalar_expr(code->expr2);   
   
  switch(code->type) {  
  case EXEC_CALL:  
    scalarize_actual_arglist(code->ext.actual);        
    break;    
    
  case EXEC_DO:      
    scalarize_scalar_expr(code->ext.iterator->start); 
    scalarize_scalar_expr(code->ext.iterator->end);       
    scalarize_scalar_expr(code->ext.iterator->step);         
    break;    
    
  case EXEC_ASSIGN:    
    if (code->expr->rank == 0) {  /* Scalar on the left */      
      if (code->expr2->rank)        
	g95_internal_error("scalarize_code(): scalar <- array assignment");   
   
      break;   /* scalar <- scalar assignment */    
    }

    /* Array expression on the left */   
   
    if (code->expr2->rank == 0) 
      scalarize_scalar_assignment(); 
    else      
      scalarize_vector_assignment();

    break;         
         
  case EXEC_IF:    
    traverse_code(code->block);   
    traverse_code(code->ext.block);         
    break;  
  
  case EXEC_DO_WHILE:    
    traverse_code(code->block);
    break;   
   
  case EXEC_SELECT:   
    for(; code; code=code->block)          
      traverse_code(code->next);

    break;      
      
  case EXEC_WHERE:     
  case EXEC_FORALL:          
    traverse_code(code->block);
    break;

  case EXEC_ALLOCATE:     
  case EXEC_DEALLOCATE:          
    for(y=code->ext.alloc_list; y; y=y->next)       
      scalarize_scalar_expr(y->expr);   
   
    break;      
      
  case EXEC_OPEN:   
    scalarize_scalar_expr(code->ext.open->unit);     
    scalarize_scalar_expr(code->ext.open->file);     
    scalarize_scalar_expr(code->ext.open->status);        
    scalarize_scalar_expr(code->ext.open->access);     
    scalarize_scalar_expr(code->ext.open->form); 
    scalarize_scalar_expr(code->ext.open->recl); 
    scalarize_scalar_expr(code->ext.open->blank);     
    scalarize_scalar_expr(code->ext.open->position);         
    scalarize_scalar_expr(code->ext.open->action);  
    scalarize_scalar_expr(code->ext.open->delim);          
    scalarize_scalar_expr(code->ext.open->pad);          
    scalarize_scalar_expr(code->ext.open->iostat);       
    break;        
        
  case EXEC_CLOSE:     
    scalarize_scalar_expr(code->ext.close->unit);        
    scalarize_scalar_expr(code->ext.close->status);         
    scalarize_scalar_expr(code->ext.close->iostat);
    break; 
 
  case EXEC_BACKSPACE:    
  case EXEC_ENDFILE:
  case EXEC_REWIND:   
    scalarize_scalar_expr(code->ext.filepos->unit);         
    scalarize_scalar_expr(code->ext.filepos->iostat);       
    break;    
    
  case EXEC_READ: 
  case EXEC_WRITE:          
    scalarize_scalar_expr(code->ext.dt->io_unit);
    scalarize_scalar_expr(code->ext.dt->format_expr); 
    scalarize_scalar_expr(code->ext.dt->rec); 
    scalarize_scalar_expr(code->ext.dt->advance);        
    scalarize_scalar_expr(code->ext.dt->iostat);    
    scalarize_scalar_expr(code->ext.dt->size);        
    break;  
  
  case EXEC_INQUIRE:  
    scalarize_scalar_expr(code->ext.inquire->unit);         
    scalarize_scalar_expr(code->ext.inquire->file);    
    scalarize_scalar_expr(code->ext.inquire->iostat);    
    scalarize_scalar_expr(code->ext.inquire->exist);        
    scalarize_scalar_expr(code->ext.inquire->opened);     
    scalarize_scalar_expr(code->ext.inquire->number);        
    scalarize_scalar_expr(code->ext.inquire->named); 
    scalarize_scalar_expr(code->ext.inquire->name);   
    scalarize_scalar_expr(code->ext.inquire->access);
    scalarize_scalar_expr(code->ext.inquire->sequential);  
    scalarize_scalar_expr(code->ext.inquire->direct); 
    scalarize_scalar_expr(code->ext.inquire->form);    
    scalarize_scalar_expr(code->ext.inquire->formatted);       
    scalarize_scalar_expr(code->ext.inquire->unformatted);         
    scalarize_scalar_expr(code->ext.inquire->recl);
    scalarize_scalar_expr(code->ext.inquire->nextrec); 
    scalarize_scalar_expr(code->ext.inquire->blank);
    scalarize_scalar_expr(code->ext.inquire->position);          
    scalarize_scalar_expr(code->ext.inquire->action);    
    scalarize_scalar_expr(code->ext.inquire->read);      
    scalarize_scalar_expr(code->ext.inquire->write);      
    scalarize_scalar_expr(code->ext.inquire->readwrite);
    scalarize_scalar_expr(code->ext.inquire->delim);         
    scalarize_scalar_expr(code->ext.inquire->pad);         
    scalarize_scalar_expr(code->ext.inquire->iolength);         
    break;  
  
  default:  
    break;          
  }          
}         
         
         
      
      
/* traverse_code()-- Traverse the code tree, scalarizing as we go.
 * The 'next' member can change if scalarize_code() generates new
 * nodes. */

static void traverse_code(g95_code *code) {
g95_code *next;

  while(code != NULL) {  
    scalarize_code(code->block);       
       
    next = code->next;     
    scalarize_code(code);      
      
    code = next; 
  }      
}     
     
     
      
      
#else
void g95_scalarize(g95_namespace *ns) {}    
    
#endif
