       
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
          
static g95_intrinsic_sym forall_get;        
        
        
       
       
/* insert_post()-- Insert a given node after the current node.  It
 * becomes the new current node. */

static void insert_post(g95_code *s) {     
g95_code *t;      
      
  t = s;        
  while(t->next != NULL) 
    t = t->next;  
  
  t->next = current_node->next;          
  current_node->next = s;   
   
  current_node = s;      
}  
  
  
  
  
/* find_mask_constructor()-- See if we can find a mask symbol in a
 * constructor */       
       
static int find_mask_constructor(g95_constructor *n, g95_symbol *target) {      
g95_iterator *it;

  if (n == NULL) return 0;          
          
  for(; n; n=n->next) {       
    if (find_mask_symbol(n->expr, target)) return 1;        
        
    if (n->iterator != NULL) {        
      it = n->iterator;   
      if (find_mask_symbol(it->start, target) ||  
	  find_mask_symbol(it->end,   target) ||         
	  find_mask_symbol(it->step,  target)) return 1;        
    } 
  }       
       
  return 0;          
}      
      
      
   
   
/* forall_temp_expr()-- Create an expression node that references the
 * temporary array.*/       
       
static g95_expr *forall_temp_expr(g95_symbol *var0, g95_forall_iterator *c) {         
g95_ref *re;         
g95_expr *p;       
int g;         
         
  p = g95_get_expr();  
  p->type = EXPR_VARIABLE;
  p->where = current_node->where;    
  p->ts = var0->ts;      
  p->symbol = var0;

  p->ref = re = g95_get_ref(); 
 
  re->type = REF_ARRAY; 
  re->u.ar.type = AR_ELEMENT;        
        
  g = 0;          
  for(; c; c=c->next)          
    re->u.ar.start[g++] = g95_copy_expr(c->var);

  re->u.ar.dimen = g;   
   
  return p; 
}   
   
   
        
        
/* find_mask_ref()-- See if we can find a mask symbol in a g95_ref
 * structure. */    
    
static int find_mask_ref(g95_ref *reference, g95_symbol *target) {   
int x, rc;   
   
  rc = 0;   
  switch(reference->type) {        
  case REF_ARRAY:   
    for(x=0; x<G95_MAX_DIMENSIONS; x++)  
      if (find_mask_symbol(reference->u.ar.start[x],  target) ||
	  find_mask_symbol(reference->u.ar.end[x],    target) ||     
	  find_mask_symbol(reference->u.ar.stride[x], target)) return 1;     
     
    break;    
    
  case REF_COMPONENT:    
    break;  
  
  case REF_SUBSTRING:     
    if (find_mask_symbol(reference->u.ss.start, target) ||         
	find_mask_symbol(reference->u.ss.end, target)) return 1; 
 
    break;      
  }  
  
  return 0;         
}    
    
    
   
   
/* find_mask_symbol()-- Search through an expression tree to find the
 * target symbol.  If found, we return nonzero. */         
         
static int find_mask_symbol(g95_expr *c, g95_symbol *target) {  
g95_actual_arglist *argum;      
g95_ref *re;     
int r; 
 
  if (c == NULL) return 0;
  r = 0;

  switch(c->type) {
  case EXPR_OP: 
    r = find_mask_symbol(c->op1, target) ||      
         find_mask_symbol(c->op2, target); 
    break;

  case EXPR_CONSTANT:      
  case EXPR_NULL:
    break;       
       
  case EXPR_FUNCTION:    
    for(argum=c->value.function.actual; argum; argum=argum->next)      
      if (find_mask_symbol(argum->u.expr, target)) {        
	r = 1;     
	break;  
      }     
     
    break;  
  
  case EXPR_VARIABLE:          
    if (c->symbol == target) {      
      r = 1;
      break;          
    }       
       
    for(re=c->ref; re; re=re->next)   
      r |= find_mask_ref(re, target);         
         
    break;      
      
  case EXPR_SUBSTRING:   
    r = find_mask_ref(c->ref, target);         
    break;          
          
  case EXPR_STRUCTURE:     
  case EXPR_ARRAY: 
    r = find_mask_constructor(c->value.constructor, target);
    break;  
  
  default:       
    g95_internal_error("find_mask_symbol(): Bad expression");       
  }   
   
  return r;    
}        
        
        
   
   
/* build_loops()-- Recursive function for building FORALL loops. */         
         
static g95_code *build_loops(g95_forall_iterator *n, g95_expr *msk,   
			     g95_code *base) { 
g95_iterator *i;  
g95_code *s, *u;        
g95_expr *p;         
         
  if (n != NULL) {   
    u = build_loops(n->next, msk, base);        
        
    s = g95_get_code();      
    s->type = EXEC_DO;      
    s->where = current_node->where;       
    s->block = u;          
    s->ext.iterator = i = g95_get_iterator();       
       
    i->var   = g95_copy_expr(n->var);  
    i->start = g95_copy_expr(n->start);     
    i->end   = g95_copy_expr(n->end);   
    i->step  = g95_copy_expr(n->stride);          
          
    if (n->next == NULL && msk != NULL)    
      s->block->block->ext.block = s;  /* Point CYCLE to its DO-loop */   
   
  } else {  /* Bottom level */  
    if (msk == NULL)          
      s = base;       
    else {     
      p = g95_get_expr();     
     
      p->type = EXPR_OP;          
      p->where = current_node->where;       
      p->operator = INTRINSIC_NOT;
      p->ts.type = BT_LOGICAL;  
      p->ts.kind = g95_default_logical_kind();       
      p->op1 = msk;   
   
      s = g95_get_code();        
      s->where = current_node->where;     
      s->type = EXEC_IF;      
      s->expr = p;         
         
      s->block = u = g95_get_code();       
      u->where = current_node->where;        
      u->type = EXEC_CYCLE;    
    
      s->next = base;        
    }  
  }  
  
  return s;     
}     
     
     
         
         
/* forall_temp()-- Create a temporary for a forall loop specification */      
      
static void forall_temp(g95_expr **q) {   
g95_symbol *sy;
g95_expr *f; 
g95_code *k;   
   
  f = *q;    
  if (f->type == EXPR_CONSTANT) return;  
  
  sy = g95_get_temporary_int();  
    
  k = g95_get_code();   
  k->type = EXEC_ASSIGN;    
  k->where = current_node->where;     
  k->expr  = g95_get_variable_expr(sy);      
  k->expr2 = f;   
   
  insert_post(k);        
        
  *q = g95_get_variable_expr(sy);  
}   
   
   
static void forall_preamble(g95_code *k) {    
g95_actual_arglist *ap;    
g95_forall_iterator *f;   
g95_code *v;         
         
  /* Save loop indeces */     
     
  for(f=k->ext.forall_iterator; f; f=f->next) {         
    f->save = g95_get_temporary_int();      
      
    v = g95_get_code(); 
    v->type = EXEC_ASSIGN;     
    v->where = current_node->where;        
        
    v->expr  = g95_get_variable_expr(f->save); 
    v->expr2 = g95_copy_expr(f->var);         
    insert_post(v);       
  }       
       
  for(f=k->ext.forall_iterator; f; f=f->next) {
    forall_temp(&f->start); 
    forall_temp(&f->end);        
    forall_temp(&f->stride);     
  }        
        
  if (k->expr != NULL) {    
    v = g95_get_code();      
    v->where = current_node->where;          
    v->type = EXEC_CALL;   
    v->sub_name = PREFIX "forall_start";         
    insert_post(v);      
      
    v = g95_get_code();      
    v->type = EXEC_CALL;        
    v->where = current_node->where;  
    v->sub_name = PREFIX "forall_save"; 
    v->ext.actual = ap = g95_get_actual_arglist();          
          
    ap->type = EXPR;         
    ap->u.expr = k->expr;     /* The mask */     
     
    v = build_loops(k->ext.forall_iterator, NULL, v); 
    insert_post(v);        
  }     
}  
  
  
   
   
/* forall_temp_array()-- Fill out a g95_array_ref structure meant for
 * allocating a temporary array. */ 
 
static void forall_temp_array(g95_array_ref *as, int a,          
			      g95_forall_iterator *o) {
g95_expr *min_expr, *max_expr;   
   
  min_expr = g95_build_funcall(NULL, g95_copy_expr(o->start),       
			       g95_copy_expr(o->end), NULL);        
        
  min_expr->value.function.isym = g95_find_function("min0");        
  min_expr->ts.type = BT_INTEGER;         
  min_expr->ts.kind = g95_default_integer_kind();        
        
  max_expr = g95_build_funcall(NULL, g95_copy_expr(o->start),    
			       g95_copy_expr(o->end), NULL); 
 
  max_expr->value.function.isym = g95_find_function("max0");         
  max_expr->ts.type = BT_INTEGER;  
  max_expr->ts.kind = g95_default_integer_kind();  
  
  g95_simplify_expr(min_expr, 0);         
  g95_simplify_expr(max_expr, 0);         
         
  as->start[a]      = min_expr;        
  as->end[a]        = max_expr;  
  as->dimen_type[a] = DIMEN_RANGE;      
}        
        
        
     
     
/* forall_body()-- Process a single code node in the body of a FORALL
 * statement. */ 
 
static void forall_body(g95_forall_iterator *t, int msk, g95_code *r) {    
g95_ref *ref, *alloc_ref;          
g95_expr *v, *mask_expr;          
g95_forall_iterator *w;        
g95_symbol *variable;          
int z, rank;    
g95_code *o;     
     
  if (!msk) 
    mask_expr = NULL;       
  else {    
    mask_expr = g95_build_funcall(NULL, NULL);  
    mask_expr->value.function.isym = &forall_get; 
    mask_expr->value.function.name = PREFIX "forall_get";        
    mask_expr->ts.type = BT_INTEGER;   
    mask_expr->ts.kind = g95_default_integer_kind();       
  }    
    
    
  switch(r->type) {          
  case EXEC_FORALL:         
    g95_expand_forall(r); 
    o = build_loops(t, mask_expr, r); 
    insert_post(o);     
    return;   
   
  case EXEC_WHERE:  
    g95_expand_where(r);     
    o = build_loops(t, mask_expr, r);
    insert_post(r);      
    return;       
       
  case EXEC_ASSIGN:
  case EXEC_POINTER_ASSIGN:  
    break;

  default:  
    g95_internal_error("g95_expand_forall(): Bad code node");          
  }  
  
  if (!find_mask_symbol(r->expr2, r->expr->symbol)) {        
    o = build_loops(t, mask_expr, r); 
    insert_post(o);   
  } else { 
    rank = 0;    
    for(w=t; w; w=w->next)        
      rank++;  
  
    variable = g95_get_temporary(&r->expr->ts, rank);     
     
    o = g95_get_code();
    o->type = EXEC_ALLOCATE;  
    o->where = r->expr->where;  
  
    o->ext.alloc_list = g95_get_alloc();      
    o->ext.alloc_list->expr = v = g95_get_expr();       
       
    v->type = EXPR_VARIABLE; 
    v->where = current_node->where;
    v->ts = variable->ts;  
    v->symbol = variable;          
    v->ref = alloc_ref = g95_get_ref();
    v->where = current_node->where;  
  
    alloc_ref->type = REF_ARRAY;    
    alloc_ref->u.ar.type = AR_SECTION;

    w = t;    
    for(z=0; z<rank; z++) { 
      forall_temp_array(&alloc_ref->u.ar, z, w);   
      w = w->next;   
    }          
          
    alloc_ref->u.ar.dimen = rank;

    insert_post(o); 
 
    v = r->expr;  
    r->expr = forall_temp_expr(variable, t);   
   
    o = build_loops(t, g95_copy_expr(mask_expr), r);      
    insert_post(o); 
 
    /* Copy temp back */     
     
    o = g95_get_code(); 
    o->type = EXEC_ASSIGN;  
    o->where = current_node->where;
    o->expr = v;       
    o->expr2 = forall_temp_expr(variable, t);         
         
    o = build_loops(t, g95_copy_expr(mask_expr), o); 
    insert_post(o);    
    
    o = g95_get_code();         
    o->type = EXEC_DEALLOCATE;        
    o->where = r->where;       
    o->ext.alloc_list = g95_get_alloc();     
    o->ext.alloc_list->expr = v = g95_get_expr();          
          
    v->type = EXPR_VARIABLE;
    v->where = current_node->where;         
    v->ts = variable->ts;    
    v->symbol = variable; 
    v->ref = ref = g95_get_ref();     
    v->rank = rank;   
    v->where = r->where;          
          
    ref->type = REF_ARRAY;         
    ref->u.ar.type = AR_FULL;

    insert_post(o);    
  }         
}   
   
   
    
    
/* g95_expand_where()-- Expand a WHERE node */   
   
void g95_expand_where(g95_code *z) { 
 
}     
 
 
/* g95_expand_forall()-- Process a FORALL node */       
       
void g95_expand_forall(g95_code *i) {       
g95_code *save, *new, *h, *s; 
g95_forall_iterator *y;   
   
  save = current_node;     
  current_node = i;  
  
  forall_preamble(i);

  for(h=i->block; h; h=s) {  
    s = h->next; 
    h->next = NULL;  
  
    forall_body(i->ext.forall_iterator, i->expr != NULL, h);      
  }       
       
  /* Restore loop indeces.  Free the forall_iterator without getting
   * rid of the expression nodes which are now used elsewhere. */        
        
  for(y=i->ext.forall_iterator; y; y=y->next) {  
    new = g95_get_code();   
    new->type = EXEC_ASSIGN;      
    new->where = current_node->where; 
 
    new->expr  = g95_copy_expr(y->var);      
    new->expr2 = g95_get_variable_expr(y->save);        
    insert_post(new);      
      
    g95_free(y);
  } 
 
  if (i->expr != NULL) {     
    h = g95_get_code();          
    h->type = EXEC_CALL;         
    h->where = current_node->where;          
    h->sub_name = PREFIX "forall_done";  
    insert_post(h);     
  }        
        
  i->type = EXEC_NOP;          
  i->block = NULL; 
  i->ext.forall_iterator = NULL;       
  i->expr = NULL;         
  i->where = current_node->where;          
  g95_free_forall_iterator(i->ext.forall_iterator);   
   
  current_node = save;   
}    
    
    
    
