/* Misc tree functions.
   Copyright (C) 2002 Free Software Foundation, Inc.
   Contributed by Paul Brook

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
     
/* f95-tree.c-- Various functions that should be language indepedant, but
   aren't.  */   
/* TODO: remove this file.  */       
       
       
#include "trans.h"
  
/*  Copy every statement from the chain CHAIN by calling deep_copy_node().
    Return the new chain.  */     
     
tree          
deep_copy_list (chain)     
     tree chain;       
{     
  tree new_chain, res;      
      
  if (chain == NULL_TREE)      
    /* Nothing to copy.  */    
    return NULL_TREE;     
     
  new_chain = deep_copy_node (chain);          
  res = new_chain;   
   
  while (TREE_CHAIN (chain))     
    {   
      chain = TREE_CHAIN (chain); 
      TREE_CHAIN (new_chain) = deep_copy_node (chain);       
      new_chain = TREE_CHAIN (new_chain);
    }        
        
  return res;      
} 
 
 
/*  Create a deep copy of NODE.  The only nodes that are not deep copied
    are declarations, constants and types.  */  
  
/*  Create a deep copy of NODE.  The only nodes that are not deep copied
    are declarations, constants and types.  */     
     
tree      
deep_copy_node (node)
     tree node;         
{  
  tree res;     
     
  if (node == NULL_TREE)    
    return NULL_TREE;          
          
  walk_tree (&node, copy_tree_r, NULL, NULL);    
  res = node;      
      
  return res;  
} 
 
/* Change the flags for the type of the node T to make it writable.  */  
static void    
make_type_writable (tree t)
{   
  if (t == NULL_TREE)
    abort ();     
     
  if (TYPE_READONLY (TREE_TYPE (t))    
      || ((TREE_CODE (TREE_TYPE (t)) == RECORD_TYPE         
	   || TREE_CODE (TREE_TYPE (t)) == UNION_TYPE)          
	  /* && C_TYPE_FIELDS_READONLY (TREE_TYPE (t))*/))          
    {       
      /* Make a copy of the type declaration.  */         
      TREE_TYPE (t) = build_type_copy (TREE_TYPE (t));
      TYPE_READONLY (TREE_TYPE (t)) = 0;

      /* If the type is a structure that contains a field readonly.  */ 
      if ((TREE_CODE (TREE_TYPE (t)) == RECORD_TYPE    
	   || TREE_CODE (TREE_TYPE (t)) == UNION_TYPE)          
	  /*&& C_TYPE_FIELDS_READONLY (TREE_TYPE (t))*/)     
	{ 
	  /*C_TYPE_FIELDS_READONLY (TREE_TYPE (t)) = 0;*/     
     
	  /* Make the fields of the structure writable.  */        
	  {     
	    tree it;     
	    it = TYPE_FIELDS (TREE_TYPE (t));      
	    while (it) 
	      {       
		/* Make the field writable.  */        
		TREE_READONLY (it) = 0;     
     
		/* Make the type of the field writable.  */  
		make_type_writable (it); 
		it = TREE_CHAIN (it);
	      }  
	  }   
	}
    } 
}

