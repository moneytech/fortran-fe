          
/* Masked assignment
   Copyright (C) 2003 Free Software Foundation, Inc.
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
        
        
/* This module is responsible for expanding the masked assignment
 * statements WHERE and FORALL into sets of simpler statements. */   
   
#include "g95.h"
        
        
static g95_code *current_node; 
 
static int find_mask_symbol(g95_expr *, g95_symbol *);

static g95_intrinsic_sym forall_get, forall_save, forall_start, forall_done;     
     
     
  
  
/* build_loops()-- Recursive function for building FORALL loops. */ 
 
static g95_code *build_loops(g95_forall_iterator *u, g95_expr *msk,          
			     g95_code *bottom) {    
g95_iterator *iter;    
g95_code *m, *y;         
g95_expr *a;       
       
  if (u != NULL) {        
    y = build_loops(u->next, msk, bottom);

    m = g95_get_code();      
    m->type = EXEC_DO;     
    m->where = current_node->where;      
    m->block = y;    
    m->ext.iterator = iter = g95_get_iterator();  
  
    iter->var   = g95_copy_expr(u->var);  
    iter->start = g95_copy_expr(u->start);    
    iter->end   = g95_copy_expr(u->end);   
    iter->step  = g95_copy_expr(u->stride);   
   
    if (u->next == NULL && msk != NULL)       
      m->block->block->ext.block = m;  /* Point CYCLE to its DO-loop */      
      
  } else {  /* Bottom level */   
    if (msk == NULL)     
      m = bottom;         
    else { 
      a = g95_get_expr();     
     
      a->type = EXPR_OP; 
      a->where = current_node->where;   
      a->operator = INTRINSIC_NOT;
      a->ts.type = BT_LOGICAL;      
      a->ts.kind = g95_default_logical_kind();    
      a->op1 = msk;

      m = g95_get_code();      
      m->where = current_node->where;      
      m->type = EXEC_IF;        
      m->expr = a;      
      
      m->block = y = g95_get_code();     
      y->where = current_node->where;
      y->type = EXEC_CYCLE;        
        
      m->next = bottom;      
    }
  }         
         
  return m;       
}     
     
     
        
        
/* forall_temp_array()-- Fill out a g95_array_ref structure meant for
 * allocating a temporary array. */    
    
static void forall_temp_array(g95_array_ref *ar, int c,     
			      g95_forall_iterator *w) {         
g95_expr *min_expr, *max_expr;      
      
  min_expr = g95_build_funcall(NULL, g95_copy_expr(w->start),         
			       g95_copy_expr(w->end), NULL); 
 
  min_expr->value.function.isym = g95_find_function("min0");
  min_expr->ts.type = BT_INTEGER;       
  min_expr->ts.kind = g95_default_integer_kind();    
    
  max_expr = g95_build_funcall(NULL, g95_copy_expr(w->start),      
			       g95_copy_expr(w->end), NULL);      
      
  max_expr->value.function.isym = g95_find_function("max0");   
  max_expr->ts.type = BT_INTEGER;        
  max_expr->ts.kind = g95_default_integer_kind();

  g95_simplify_expr(min_expr, 0); 
  g95_simplify_expr(max_expr, 0);

  ar->start[c]      = min_expr;         
  ar->end[c]        = max_expr;     
  ar->dimen_type[c] = DIMEN_RANGE;   
}


   
   
/* forall_temp_expr()-- Create an expression node that references the
 * temporary array.*/     
     
static g95_expr *forall_temp_expr(g95_symbol *v, g95_forall_iterator *s) {         
g95_ref *reference;          
g95_expr *c;        
int h;         
         
  c = g95_get_expr();   
  c->type = EXPR_VARIABLE;     
  c->where = current_node->where;      
  c->ts = v->ts;        
  c->symbol = v;          
          
  c->ref = reference = g95_get_ref();  
  
  reference->type = REF_ARRAY;   
  reference->u.ar.type = AR_ELEMENT;      
      
  h = 0;
  for(; s; s=s->next)       
    reference->u.ar.start[h++] = g95_copy_expr(s->var);      
      
  reference->u.ar.dimen = h;          
          
  return c;         
} 
 
 


/* find_mask_ref()-- See if we can find a mask symbol in a g95_ref
 * structure. */     
     
static int find_mask_ref(g95_ref *ref, g95_symbol *target) {       
int y, r; 
 
  r = 0;          
  switch(ref->type) {     
  case REF_ARRAY: 
    for(y=0; y<G95_MAX_DIMENSIONS; y++)     
      if (find_mask_symbol(ref->u.ar.start[y],  target) ||        
	  find_mask_symbol(ref->u.ar.end[y],    target) ||     
	  find_mask_symbol(ref->u.ar.stride[y], target)) return 1;       
       
    break;          
          
  case REF_COMPONENT:       
    break;        
        
  case REF_SUBSTRING:        
    if (find_mask_symbol(ref->u.ss.start, target) ||   
	find_mask_symbol(ref->u.ss.end, target)) return 1;     
     
    break;         
  }         
         
  return 0;   
}         
         
         
         
         
/* find_mask_constructor()-- See if we can find a mask symbol in a
 * constructor */ 
 
static int find_mask_constructor(g95_constructor *c, g95_symbol *target) {
g95_iterator *iter;

  if (c == NULL) return 0;

  for(; c; c=c->next) {  
    if (find_mask_symbol(c->expr, target)) return 1;         
         
    if (c->iterator != NULL) {     
      iter = c->iterator;      
      if (find_mask_symbol(iter->start, target) ||    
	  find_mask_symbol(iter->end,   target) ||      
	  find_mask_symbol(iter->step,  target)) return 1;       
    }  
  }     
     
  return 0;        
}          
          
          
         
         
/* find_mask_symbol()-- Search through an expression tree to find the
 * target symbol.  If found, we return nonzero. */       
       
static int find_mask_symbol(g95_expr *j, g95_symbol *target) {      
g95_actual_arglist *ap;     
g95_ref *r;    
int retval; 
 
  if (j == NULL) return 0;      
  retval = 0;      
      
  switch(j->type) {  
  case EXPR_OP:          
    retval = find_mask_symbol(j->op1, target) ||  
         find_mask_symbol(j->op2, target);      
    break;         
         
  case EXPR_CONSTANT:
  case EXPR_NULL:        
    break;     
     
  case EXPR_FUNCTION:       
    for(ap=j->value.function.actual; ap; ap=ap->next) 
      if (find_mask_symbol(ap->u.expr, target)) {     
	retval = 1;   
	break;    
      }        
        
    break;     
     
  case EXPR_VARIABLE:      
    if (j->symbol == target) { 
      retval = 1;       
      break; 
    }   
   
    for(r=j->ref; r; r=r->next) 
      retval |= find_mask_ref(r, target);         
         
    break;      
      
  case EXPR_SUBSTRING:  
    retval = find_mask_ref(j->ref, target);  
    break;     
     
  case EXPR_STRUCTURE:          
  case EXPR_ARRAY:      
    retval = find_mask_constructor(j->value.constructor, target);
    break;    
    
  default:        
    g95_internal_error("find_mask_symbol(): Bad expression");         
  }   
   
  return retval;         
}          
          
          
     
     
/* insert_post()-- Insert a given node after the current node.  It
 * becomes the new current node. */     
     
static void insert_post(g95_code *t) {        
g95_code *end;   
   
  end = t;          
  while(end->next != NULL)
    end = end->next;       
       
  end->next = current_node->next;         
  current_node->next = t;        
        
  current_node = t;  
}       
       
       
  
  
/* forall_temp()-- Create a temporary for a forall loop specification */   
   
static void forall_temp(g95_expr **o) {      
g95_symbol *sym;
g95_expr *b;    
g95_code *g;     
     
  b = *o;   
  if (b->type == EXPR_CONSTANT) return; 
 
  sym = g95_get_temporary_int(); 
   
  g = g95_get_code();  
  g->type = EXEC_ASSIGN;         
  g->where = current_node->where;      
  g->expr  = g95_get_variable_expr(sym);         
  g->expr2 = b;       
       
  insert_post(g);         
         
  *o = g95_get_variable_expr(sym);
}


static void forall_preamble(g95_code *g) {  
g95_actual_arglist *arg;  
g95_forall_iterator *b;  
g95_code *a;        
        
  /* Save loop indeces */       
       
  for(b=g->ext.forall_iterator; b; b=b->next) {     
    b->save = g95_get_temporary_int();          
          
    a = g95_get_code();       
    a->type = EXEC_ASSIGN;
    a->where = current_node->where; 
 
    a->expr  = g95_get_variable_expr(b->save); 
    a->expr2 = g95_copy_expr(b->var);          
    insert_post(a);         
  }       
       
  for(b=g->ext.forall_iterator; b; b=b->next) {    
    forall_temp(&b->start);        
    forall_temp(&b->end);     
    forall_temp(&b->stride);        
  } 
 
  if (g->expr != NULL) { 
    a = g95_get_code();       
    a->where = current_node->where;          
    a->type = EXEC_CALL;     
    a->isym = &forall_start;         
    a->sub_name = PREFIX "forall_start";     
    insert_post(a);    
    
    a = g95_get_code();          
    a->type = EXEC_CALL;     
    a->where = current_node->where;        
    a->isym = &forall_save;    
    a->sub_name = PREFIX "forall_save";        
    a->ext.actual = arg = g95_get_actual_arglist();

    arg->type = EXPR;          
    arg->u.expr = g->expr;     /* The mask */   
   
    a = build_loops(g->ext.forall_iterator, NULL, a);    
    insert_post(a);          
  }  
}       
       
       
 
 
/* forall_body()-- Process a single code node in the body of a FORALL
 * statement. */     
     
static void forall_body(g95_forall_iterator *m, int msk, g95_code *c) {    
g95_ref *re, *alloc_ref;         
g95_expr *e, *mask_expr;      
g95_forall_iterator *p;   
g95_symbol *v;
g95_code *r; 
int k, rank;  
  
  if (!msk)       
    mask_expr = NULL;
  else {     
    mask_expr = g95_build_funcall(NULL, NULL);          
    mask_expr->value.function.isym = &forall_get;         
    mask_expr->value.function.name = PREFIX "forall_get";   
    mask_expr->ts.type = BT_INTEGER;         
    mask_expr->ts.kind = g95_default_integer_kind();    
  }      
      
  switch(c->type) {     
  case EXEC_FORALL:     
    g95_expand_forall(c);         
    r = build_loops(m, mask_expr, c);
    insert_post(r);    
    return;   
   
  case EXEC_WHERE:         
    g95_expand_where(&c);   
    r = build_loops(m, mask_expr, c);      
    insert_post(c); 
    return;     
     
  case EXEC_ASSIGN:
  case EXEC_POINTER_ASSIGN:       
    break;   
   
  default:
    g95_internal_error("g95_expand_forall(): Bad code node");         
  }  
  
  if (!find_mask_symbol(c->expr2, c->expr->symbol)) {    
    r = build_loops(m, mask_expr, c); 
    insert_post(r); 
  } else {        
    rank = 0;    
    for(p=m; p; p=p->next)          
      rank++;       
       
    v = g95_get_temporary(&c->expr->ts, rank);         
         
    r = g95_get_code();
    r->type = EXEC_ALLOCATE;
    r->where = c->expr->where;          
          
    r->ext.alloc_list = g95_get_alloc(); 
    r->ext.alloc_list->expr = e = g95_get_expr();   
   
    e->type = EXPR_VARIABLE; 
    e->where = current_node->where; 
    e->ts = v->ts; 
    e->symbol = v;
    e->ref = alloc_ref = g95_get_ref();    
    e->where = current_node->where;       
       
    alloc_ref->type = REF_ARRAY;   
    alloc_ref->u.ar.type = AR_SECTION;      
      
    p = m;    
    for(k=0; k<rank; k++) {    
      forall_temp_array(&alloc_ref->u.ar, k, p); 
      p = p->next;        
    }          
          
    alloc_ref->u.ar.dimen = rank;   
   
    insert_post(r);         
         
    e = c->expr;          
    c->expr = forall_temp_expr(v, m);    
    
    r = build_loops(m, g95_copy_expr(mask_expr), c);     
    insert_post(r);   
   
    /* Copy temp back */     
     
    r = g95_get_code();
    r->type = EXEC_ASSIGN;        
    r->where = current_node->where;  
    r->expr = e;      
    r->expr2 = forall_temp_expr(v, m);      
      
    r = build_loops(m, g95_copy_expr(mask_expr), r);     
    insert_post(r);   
   
    r = g95_get_code();   
    r->type = EXEC_DEALLOCATE;        
    r->where = c->where;         
    r->ext.alloc_list = g95_get_alloc();          
    r->ext.alloc_list->expr = e = g95_get_expr();

    e->type = EXPR_VARIABLE;
    e->where = current_node->where;      
    e->ts = v->ts;  
    e->symbol = v;    
    e->ref = re = g95_get_ref(); 
    e->rank = rank;  
    e->where = c->where; 
 
    re->type = REF_ARRAY; 
    re->u.ar.type = AR_FULL;        
        
    insert_post(r);       
  }          
}    
    
    
 
 
/* g95_expand_forall()-- Process a FORALL node */         
         
void g95_expand_forall(g95_code *r) {     
g95_code *save, *old, *p, *g;          
g95_forall_iterator *w;   
   
  save = current_node;       
  current_node = r;   
   
  forall_preamble(r);

  for(p=r->block; p; p=g) {
    g = p->next;
    p->next = NULL;  
  
    forall_body(r->ext.forall_iterator, r->expr != NULL, p); 
  }

  /* Restore loop indeces.  Free the forall_iterator without getting
   * rid of the expression nodes which are now used elsewhere. */          
          
  for(w=r->ext.forall_iterator; w; w=w->next) {         
    old = g95_get_code();   
    old->type = EXEC_ASSIGN;         
    old->where = current_node->where;       
       
    old->expr  = g95_copy_expr(w->var);        
    old->expr2 = g95_get_variable_expr(w->save);         
    insert_post(old);         
         
    g95_free(w);          
  }   
   
  if (r->expr != NULL) {     
    p = g95_get_code();  
    p->type = EXEC_CALL;         
    p->where = current_node->where;    
    p->isym = &forall_done;        
    p->sub_name = PREFIX "forall_done";   
    insert_post(p);  
  } 
 
  r->type = EXEC_NOP;         
  r->block = NULL;        
  r->ext.forall_iterator = NULL;  
  r->expr = NULL;        
  r->where = current_node->where; 
  g95_free_forall_iterator(r->ext.forall_iterator);     
     
  current_node = save;     
}

