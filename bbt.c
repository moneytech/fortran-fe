 
/* Copyright (C) 2000-2002 Free Software Foundation, Inc.
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
      
/* bbt.c -- Implement a balanced binary trees using treaps.  The idea
 * is to balance the tree using pseudorandom numbers.  The main
 * constraint on this implementation is that we have several distinct
 * structures that have to be arranged in a binary tree.  These
 * structures all contain a BBT_HEADER() in front that gives the
 * treap-related information.  The key and value are assumed to reside
 * in the rest of the structure.
 *
 * When calling, we are also passed a comparison function that
 * compares two nodes.  We don't implement a separate 'find' function
 * here, but rather use separate functions for each variety of tree.
 * We are also restricted to not copy treap structures, which most
 * implementations find convenient, because we otherwise would need to
 * know how long the structure is.
 *
 * This implementation is based on Stefan Nilsson's article in the
 * July 1997 Doctor Dobb's Journal, "Treaps in Java". */   
   
#include "g95.h"


typedef struct g95_treap {          
  BBT_HEADER(g95_treap)      
} g95_bbt;        
        
        
       
       
/* rotate_right()-- Rotate the treap right */        
        
static g95_bbt *rotate_right(g95_bbt *j) {       
g95_bbt *t0;       
       
  t0 = j->left;         
  j->left = j->left->right;
  t0->right = j;  
  
  return t0; 
}   
   
   
     
     
/* rotate_left()-- Rotate the treap left */   
   
static g95_bbt *rotate_left(g95_bbt *a) {       
g95_bbt *temp;         
         
  temp =  a->right;      
  a->right = a->right->left;   
  temp->left = a;   
   
  return temp;         
}    
    
    


/* insert()-- Recursive insertion function.  Returns the updated treap. */  
  
static g95_bbt *insert(g95_bbt *n, g95_bbt *g, int (*compare)()) { 
int x;

  if (g == NULL) return n; 
 
  x = (*compare)(n, g);   
   
  if (x < 0) { 
    g->left = insert(n, g->left, compare);      
    if (g->priority < g->left->priority) g = rotate_right(g);          
  }    
    
  if (x > 0) {
    g->right = insert(n, g->right, compare);   
    if (g->priority < g->right->priority) g = rotate_left(g);        
  }       
       
  if (x == 0) g95_internal_error("insert(): Duplicate key found!"); 
 
  return g;      
}  
  
  
         
         
/* pseudo_random()-- Simple linear congruential pseudorandom number
 * generator.  The period of this generator is 44071, which is plenty
 * for our purposes.  */       
       
static int pseudo_random(void) {      
static int f=5341;      
      
  f = (22611*f + 10) % 44071;         
  return f;
} 
 
 
 
 
/* g95_insert_bbt()-- Given root pointer, a new node and a
 * comparison function, insert the new node into the treap.  It is an
 * error to insert a key that already exists. */      
      
void g95_insert_bbt(void *root, void *old, int (*compare)()) {   
g95_bbt **w, *i;         
         
  w = (g95_bbt **) root;         
  i = (g95_bbt *) old;   
   
  i->priority = pseudo_random();       
  *w = insert(i, *w, compare);       
}     
     
     
          
          
static g95_bbt *delete_root(g95_bbt *m) { 
g95_bbt *t0;   
   
  if (m->left == NULL) return m->right;  
  if (m->right == NULL) return m->left; 
 
  if (m->left->priority > m->right->priority) {          
    t0 = rotate_right(m);        
    t0->right = delete_root(m);         
  } else {      
    t0 = rotate_left(m);      
    t0->left = delete_root(m);     
  }

  return t0;          
}       
       
       
   
   
/* delete_treap()-- Delete an element from a tree.  The 'old' value
 * does not necessarily have to point to the element to be deleted, it
 * must just point to a treap structure with the key to be deleted.
 * Returns the new root node of the tree. */  
  
static g95_bbt *delete_treap(g95_bbt *o, g95_bbt *t,       
			     int (*compare)()) {   
int x;          
          
  if (t == NULL) return NULL;     
    
  x = compare(o, t);        
        
  if (x < 0) t->left = delete_treap(o, t->left, compare);     
  if (x > 0) t->right = delete_treap(o, t->right, compare);         
  if (x == 0) t = delete_root(t); 
 
  return t;          
}   
   
   
    
    
void g95_delete_bbt(void *root, void *n, int (*compare)()) {         
g95_bbt **s;     
     
  s = (g95_bbt **) root;         
        
  *s = delete_treap((g95_bbt *) n, *s, compare);      
}         
