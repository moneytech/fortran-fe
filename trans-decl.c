/* Backend function setup
   Copyright (C) 2000, 2001, 2002 Free Software Foundation, Inc.
   Contributed by Paul Brook

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

/* be-function.c -- handling of backend function declarations, etc */

#include "config.h"
#include "system.h"
#include "tree.h"
#include <stdio.h>
#include "c-common.h"
#include "ggc.h"
#include "rtl.h"
#include "toplev.h"
#include "function.h"
#include "expr.h"
#include "errors.h"
#include <assert.h>
#define BACKEND_CODE
#include "g95.h"
#include "trans.h"
#include "trans-types.h"
/* Only for g95_trans_code.  Souldn't neet to include this.  */
#include "trans-stmt.h"

#define MAX_LABEL_VALUE 99999

/* Holds the result of the function if no result variable specified.  */
static GTY(()) tree current_fake_result_decl;

static GTY(()) tree current_function_return_label;

/* Holds the variable DECLs for the current function.  */
static GTY (()) tree saved_function_decls = NULL_TREE;

/* Function declarations for builtin library functions.  */
static GTY(()) tree g95_fndecl_push_context;
static GTY(()) tree g95_fndecl_pop_context;
static GTY(()) tree g95_fndecl_internal_alloc;
static GTY(()) tree g95_fndecl_array_mismatch;

/* TODO : - Move generic build functions to either trans.c or a renamed
            and cleanded up support.c */

/* Build a  backend label declaration.
   Set TREE_USED for named lables.  For atrificial labels it's up to the
   caller to mark the label as used.  */
tree
g95_build_label_decl (tree label_id)
{
  static unsigned int tmp_num = 1; /* 2^32 temporaries should be enough.  */
  tree label_decl;
  char *label_name;

  if (label_id == NULL_TREE)
    {
      /* Build an internal label name.  */
      ASM_FORMAT_PRIVATE_NAME (label_name, "L", tmp_num++);
      label_id = get_identifier (label_name);
    }
  else
    label_name = NULL;

  /* Build the LABEL_DECL node. Labels have no type.  */
  label_decl = build_decl (LABEL_DECL,
                           label_id,
                           void_type_node);
  DECL_CONTEXT (label_decl) = current_function_decl;
  DECL_MODE (label_decl) = VOIDmode;

  if (label_name)
  {
    DECL_ARTIFICIAL (label_decl) = 1;
  }
  else
    {
      /* We always define the label as used, even if the original source
         file never references the label.  We don't want all kinds of
         spurious warnings for old-style Fortran code with too many
         labels.  */
      TREE_USED (label_decl) = 1;
    }

  return label_decl;
}

/* Returns the return label for the current function.  */
tree
g95_get_return_label (void)
{
  char name[G95_MAX_SYMBOL_LEN + 10];

  if (current_function_return_label)
    return current_function_return_label;

  sprintf (name, "__return_%s",
        IDENTIFIER_POINTER (DECL_NAME (current_function_decl)));

  current_function_return_label = g95_build_label_decl (get_identifier (name));

  DECL_ARTIFICIAL (current_function_return_label) = 1;

  return current_function_return_label;
}

/* Return the backend label declaration for a given label structure,
   or create it if it doesn't exist yet.  */
tree
g95_get_label_decl (g95_st_label *lp)
{

  if (lp->backend_decl)
    return lp->backend_decl;
  else
    {
      char label_name[G95_MAX_SYMBOL_LEN+1];
      tree label_decl;

      /* Validate the label declaration from the front end.  */
      assert (lp != NULL && lp->value <= MAX_LABEL_VALUE);

      /* Build a mangled name for the label.  */
      sprintf (label_name, "__label_%.6d", lp->value);

      /* Build the LABEL_DECL node.  */
      label_decl = g95_build_label_decl (get_identifier (label_name));

      /* Tell the debugger where the label came from.  */
      if (lp->value <= MAX_LABEL_VALUE) /* An internal label */
        {
          DECL_SOURCE_LINE (label_decl) = lp->where.line;
          DECL_SOURCE_FILE (label_decl) = lp->where.file->filename;
        }
      else
        DECL_ARTIFICIAL (label_decl) = 1;

      /* Store the label in the label list and return the LABEL_DECL.  */
      lp->backend_decl = label_decl;
      return label_decl;
    }
}

/* Convert a g95_symbol to an identifier of the same name.  */
static tree
g95_sym_identifier (g95_symbol * sym)
{
  return (get_identifier (sym->name));
}

/* Construct mangled name from symbol name should be at least
   G95_MAX_MANGLED_SYMBOL_LENGTH+1 chars long*/
static tree
g95_sym_mangled_identifier (g95_symbol * sym)
{
  char name[G95_MAX_MANGLED_SYMBOL_LEN + 1];

  if (sym->module[0] == 0)
    return g95_sym_identifier (sym);
  else
    {
      sprintf (name, "__%s__%s", sym->module, sym->name);
      return get_identifier (name);
    }
}

/* Finish processing of a declaration and install its initial value.  */
static void
g95_finish_decl (tree decl, tree init)
{
  if (TREE_CODE (decl) == PARM_DECL)
    assert (init == NULL_TREE);
  /* Remember that PARM_DECL doesn't have a DECL_INITIAL field per se
     -- it overlaps DECL_ARG_TYPE.  */
  else if (init == NULL_TREE)
    assert (DECL_INITIAL (decl) == NULL_TREE);
  else
    assert (DECL_INITIAL (decl) == error_mark_node);

  if (init != NULL_TREE)
    {
      if (TREE_CODE (decl) != TYPE_DECL)
        DECL_INITIAL (decl) = init;
      else
        {
          /* typedef foo = bar; store the type of bar as the type of foo.  */
          TREE_TYPE (decl) = TREE_TYPE (init);
          DECL_INITIAL (decl) = init = 0;
        }
    }

  if (TREE_CODE (decl) == VAR_DECL)
    {
      if (DECL_SIZE (decl) == NULL_TREE
          && TYPE_SIZE (TREE_TYPE (decl)) != NULL_TREE)
        layout_decl (decl, 0);

      if (DECL_SIZE (decl) == NULL_TREE && (TREE_STATIC (decl) ?
          /* A static variable with an incomplete type is an error if it is
             initialized. Also if it is not file scope. Otherwise, let it
             through, but if it is not `extern' then it may cause an error
             message later.  */
          (DECL_INITIAL (decl) != 0
           || DECL_CONTEXT (decl) != 0) :
          /* An automatic variable with an incomplete type is an error.  */
          !DECL_EXTERNAL (decl)))
        {
          g95_fatal_error ("storage size not known");
        }

      if ((DECL_EXTERNAL (decl) || TREE_STATIC (decl))
          && (DECL_SIZE (decl) != 0)
          && (TREE_CODE (DECL_SIZE (decl)) != INTEGER_CST))
        {
          g95_fatal_error ("storage size not constant");
        }
    }

#if 0
  /* Output the assembler code and/or RTL code for variables and functions,
     unless the type is an undefined structure or union. If not, it will get
     done when the type is completed.  */

  if (TREE_CODE (decl) == VAR_DECL
      || TREE_CODE (decl) == FUNCTION_DECL || TREE_CODE (decl) == TYPE_DECL)
  {
    rest_of_decl_compilation (decl, NULL, DECL_CONTEXT (decl) == 0, 0);
  }
#endif
}

/* Return the decl for a g95_symbol, create it if it doesn't allready
   exist.  */
/* TODO : - sanitize.
          - this function should *only* be used for variables.
          - also see all those todo_errors...  */
tree
g95_get_symbol_decl (g95_symbol * sym)
{
  tree decl;

  if (sym->attr.dummy)
    {
      assert (sym->backend_decl);
      TREE_USED (sym->backend_decl) = 1;
    }

  if (sym->backend_decl)
    return sym->backend_decl;

  if (sym->attr.in_common)
    g95_todo_error ("common variables");
  else if (sym->attr.entry)
     g95_todo_error ("alternate entry");
  else if (sym->attr.intrinsic)
    g95_todo_error ("intrinsics");

  decl = build_decl (VAR_DECL,
                     g95_sym_identifier (sym),
                     g95_sym_type (sym));
  /* TREE_ADDRESSABLE meand the address of this variable is acualy needed.
     This is the equivalent of the TARGET variables.
     We also need to set this if the variable is passed by reference in a
     CALL statement.  */
  if (sym->attr.target)
    TREE_ADDRESSABLE (decl)=1;
  /* If it wasn't used we wouldn't be getting it.  */
  TREE_USED (decl)=1;

  /* Mark symbol as external if it's declared external in the parser.  */
  if (sym->attr.external)
    DECL_EXTERNAL (decl) = 1;

  /* If a variable is USE associated, it's always external, and
     its assembler name should be manged.  */
  if (sym->attr.use_assoc)
    {
      DECL_EXTERNAL (decl) = 1;
      SET_DECL_ASSEMBLER_NAME (decl, g95_sym_mangled_identifier (sym));
    }

  if (sym->attr.dimension)
    {
      /* Remember this variable for allocation/cleanup.  */
      sym->tlink = sym->ns->proc_name->tlink;
      sym->ns->proc_name->tlink = sym;
    }

  if (sym->ns->proc_name->backend_decl != current_function_decl)
    g95_todo_error ("Use of parent variable in nested subroutine");

  /* Chain this decl to the pending declarations.  Don't do pushdecl()
     because this would add them to the current scope rather than the
     function scope.  */

  saved_function_decls =
    chainon (build_stmt (DECL_STMT, decl), saved_function_decls);

  if (sym->attr.save)
    TREE_STATIC (decl) = 1;

  sym->backend_decl = decl;

  return decl;
}

/* Get a function declaration.  Create it if if deosn't exist.  */
tree
g95_get_function_decl (g95_symbol * sym)
{
  tree fndecl, type, result_decl, typelist, arglist;
  symbol_attribute attr;
  g95_formal_arglist *f;

  if (sym->backend_decl)
    return sym->backend_decl;

  /* make sure this symbol is a function or a subroutine.  */
  assert (sym->attr.function || sym->attr.subroutine);

  /* Allow only one nesting level.  */
  /* We may need two for external declarations  within nested functions.  */
  assert (current_function_decl == NULL_TREE
          || DECL_CONTEXT (current_function_decl) == NULL_TREE);

  type = NULL_TREE;
  typelist = NULL_TREE;
  /* Build the argument types for the function */
  for (f = sym->formal; f; f = f->next)
    {
      if (f->sym)
        {
          type = g95_sym_type (f->sym);
          /* Parameter Passing Convention

             We currently pass all parameters by reference.
             Parameters with INTENT(IN) voud be passed by value.
             The problem arises if a function is called vai and implicit
             prototypes. In this situation the INTENT is not known.
             For this reason all parameters to global functions must be
             passed by reference.  Passing by valie would potentialy
             generate bad code, worse there would be no way of telling that
             this code wad bed, except that it would give incorrect results.

             Module and contained procedures could pass by value as these are
             never used without and explicit interface.
           */
          typelist = chainon (typelist, listify (type));
        }
    }

  typelist = chainon (typelist, listify (void_type_node));

  if (sym->attr.subroutine)
    type=void_type_node;
  else
    type=g95_sym_type (sym);

  type = build_function_type (type, typelist);
  fndecl = build_decl (FUNCTION_DECL, g95_sym_identifier (sym), type);

  /* Figure out the return type of the declared function, and build a
     RESULT_DECL for it.  If this is subroutine with alternate
     returns, build a RESULT_DECL for it.  */
  attr = sym->attr;
  result_decl = NULL_TREE;
  if (attr.function)
    {
      if (sym->result != sym)
        result_decl = g95_sym_identifier (sym->result);

      type = TREE_TYPE (TREE_TYPE (fndecl));
    }
  else
    {
      /* Look for an alternate return placeholders.  */
      int has_alternate_returns = 0;
      for (f = sym->formal; f; f = f->next)
        {
          if (f->sym == NULL)
            {
              has_alternate_returns = 1;
              break;
            }
        }

      if (has_alternate_returns)
        type = integer_type_node;
      else
        type = void_type_node;
    }

  result_decl = build_decl (RESULT_DECL, result_decl, type);
  DECL_CONTEXT (result_decl) = fndecl;
  DECL_RESULT (fndecl) = result_decl;

  /* Don't call layout_decl for a RESULT_DECL.
  layout_decl (result_decl, 0); */

  /* If the return type is a pointer, avoid alias issues by setting
     DECL_IS_MALLOC to nonzero. This means that the function should be
     treated as if it were a malloc, meaning it returns a pointer that
     is not an alias.  */
  if (POINTER_TYPE_P (type))
    DECL_IS_MALLOC (fndecl) = 1;

  /* Set up all attributes for the function.  */
  DECL_CONTEXT (fndecl) = current_function_decl;
  DECL_EXTERNAL (fndecl) = sym->attr.external;

  /* This specifies if a function is globaly addressable, ie. it is
     the opposite of decalring static  in C.  */
  TREE_PUBLIC (fndecl) = (DECL_CONTEXT (fndecl) == NULL_TREE);
  /* This should be 0 for external declarations.  */
  TREE_STATIC (fndecl) = 1;

  /* Set attributes for PURE functions. A call to PURE function in the
     Fortran 95 sense is both pure and without side effects in the C
     sense.  */
  if (attr.pure || attr.elemental)
    {
      DECL_IS_PURE (fndecl) = 1;
      TREE_SIDE_EFFECTS (fndecl) = 0;
    }

  /* Layout the function declaration and put it in the binding level
     of the current function.  */
  /* not done for FUNCTION_DECL nades
  layout_decl (fndecl, 0);*/
  pushdecl (fndecl);

  /* Build formal argument list. Make sure that their TREE_CONTEXT is
     the new FUNCTION_DECL node.  */
  current_function_decl = fndecl;
  arglist = NULL_TREE;
  typelist = TYPE_ARG_TYPES (TREE_TYPE (fndecl));
  for (f = sym->formal; f; f = f->next)
    {
      if (f->sym != NULL)       /* ignore alternate returns. */
        {
          tree parm;

          type = TREE_VALUE (typelist);
          /* Build a the argument declaration.  */
          parm = build_decl (PARM_DECL,
                             g95_sym_identifier (f->sym),
                             type);

          /* Fill in arg stuff.  */
          DECL_CONTEXT (parm) = fndecl;
          DECL_ARG_TYPE (parm) = type;
          DECL_ARG_TYPE_AS_WRITTEN (parm) = type;
          /* All implementation args are read-only.  */
          TREE_READONLY (parm) = 1;

          //parm = pushdecl (parm);
          g95_finish_decl (parm, NULL_TREE);

          f->sym->backend_decl=parm;

          arglist = chainon (arglist, parm);
          typelist = TREE_CHAIN (typelist);
        }
    }

  DECL_ARGUMENTS (fndecl) = arglist;

  /* Restore the old context.  */
  current_function_decl = DECL_CONTEXT (fndecl);

  sym->backend_decl = fndecl;

  return fndecl;
}

/* Return the decl used to hold the function return value.  */
tree
g95_get_fake_result_decl (void)
{
  tree decl;
  char name[G95_MAX_SYMBOL_LEN+10];

  if (current_fake_result_decl != NULL_TREE)
    return current_fake_result_decl;

  sprintf (name, "__result_%s",
        IDENTIFIER_POINTER (DECL_NAME (current_function_decl)));

  decl = build_decl (VAR_DECL, get_identifier (name),
              TREE_TYPE (TREE_TYPE (current_function_decl)));

  DECL_ARTIFICIAL (decl) = 1;
  DECL_EXTERNAL (decl) = 0;
  TREE_PUBLIC (decl) = 0;
  TREE_USED (decl) = 1;

  layout_decl (decl, 0);

  saved_function_decls = chainon (saved_function_decls,
        build_stmt (DECL_STMT, decl));

  current_fake_result_decl = decl;

  return decl;
}

/* Build a CALL_EXPR.  */
static tree
g95_build_function_call (tree fndecl, tree arglist)
{
  tree fn;
  tree call;

  fn = build1 (ADDR_EXPR, build_pointer_type (TREE_TYPE (fndecl)), fndecl);
  call = build (CALL_EXPR, TREE_TYPE (fndecl), fn, arglist);
  TREE_SIDE_EFFECTS (call) = 1;

  return call;
}

/* Generates setup and cleanup code for arrays, and adds it to function body.
   End result is something like:

    subroutine foo (...)
    {
      __g95_push_context ()
      foreach (local_array)
        {
          array.bounds = <calculated bounds of array>
          array.stride = <clculated stride>
          size = <number of elements in array> * <size of single array element>
          __g95_internal_alloc (array.<data|block>, size);
    #ifdef g95_use_gcc_arrays
          array.data = &array.block[-sum(array.lbound[]*array.stride[])]
    #endif
        }
      foreach (non-pointer array parameter)
        {
          <adjust bounds to match assumed lower bound>
        }

      <function body code>
    __return_foo:
      foreach (non-pointer array parameter)
        {
          <restore bounds>
        }
      __g95_pop_context ()
    }

   TODO: Place small arrays on the stack rather than the heap.  */
/* Helper Macro. Adds a STMT_EXPR to list(head, tail).  */
#define ADD_EXPR_STMT(expr) {tree stmt__tmp = build_stmt (EXPR_STMT, expr); \
                    g95_add_stmt_to_list (&head, &tail, stmt__tmp, stmt__tmp);}

static tree
g95_trans_auto_array_allocation (g95_symbol * sym)
{
  g95_se ubound;
  g95_se lbound;
  int n;
  tree head;
  tree tail;
  tree size;
  tree num;
  tree tmp;
  tree type;
  tree data_pointer_type;
  tree element_type;
  tree field;
  tree stride;
  tree offset;

  g95_start_stmt ();

  head = tail = NULL_TREE;

  /* Work out types that we will need later.  */
  type = TREE_TYPE (sym->backend_decl);
  assert (TREE_CODE (type) == RECORD_TYPE);

  field = g95_get_data_component (type);
  data_pointer_type = TREE_TYPE (field);

  /* Temp vars to hold and calculate the size of the array data.  */
  size=g95_create_tmp_var (g95_array_index_type);
  num = g95_create_tmp_var (g95_array_index_type);
  if (g95_use_gcc_arrays)
    {
      stride = NULL_TREE;
      offset = NULL_TREE;
    }
  else
    {
      stride = g95_create_tmp_var(g95_array_index_type);
      offset = g95_create_tmp_var(g95_array_index_type);
    }

  /* Calculate the number of elements in the array.  Also fill in the
     descriptor lbound and ubound members.  */
  for (n = 0 ; n < sym->as->rank ; n++)
    {
      g95_init_se (&lbound, NULL);
      g95_conv_simple_val (&lbound, sym->as->lower[n]);
      g95_init_se (&ubound, NULL);
      g95_conv_simple_val (&ubound, sym->as->upper[n]);

      /* Store the lower bound of the array in the descriptor.  */
      g95_add_stmt_to_list (&head, &tail, lbound.pre, lbound.pre_tail);
      field = g95_get_lbound_component (type, n);
      tmp = build (COMPONENT_REF, g95_array_index_type,
                    sym->backend_decl, field);
      tmp = build (MODIFY_EXPR, g95_array_index_type, tmp, lbound.expr);
      ADD_EXPR_STMT (tmp);

      /* Do the same for the upper bound.  */
      g95_add_stmt_to_list (&head, &tail, ubound.pre, ubound.pre_tail);
      field = g95_get_ubound_component (type, n);
      tmp = build (COMPONENT_REF, g95_array_index_type,
                    sym->backend_decl, field);
      tmp = build (MODIFY_EXPR, g95_array_index_type, tmp, ubound.expr);
      ADD_EXPR_STMT (tmp);

      /* Number of elements in this dimension = ubound + 1 - lbound  */
      /*TODO: constant folding - This may be done by the backend anyway.  */
      tmp = build (MINUS_EXPR, g95_array_index_type,
                    integer_one_node, lbound.expr);
      tmp = build (MODIFY_EXPR, g95_array_index_type, num, tmp);
      /* num = 1 - lbound */
      ADD_EXPR_STMT (tmp);

      tmp = build (PLUS_EXPR, g95_array_index_type, ubound.expr, num);
      tmp = build (MODIFY_EXPR, g95_array_index_type, num, tmp);
      /* num = ubound + num */
      ADD_EXPR_STMT (tmp);

      if (n == 0)
        tmp = num;
      else
        tmp = build (PLUS_EXPR, g95_array_index_type, size, num);
      tmp = build (MODIFY_EXPR, g95_array_index_type, size, tmp);
      /* size = size + num */
      ADD_EXPR_STMT (tmp);

      if (! g95_use_gcc_arrays)
        {
          /* Work out the array stride.  */
          if (n == 0)
            tmp = integer_one_node;
          else
            tmp = build (MULT_EXPR, g95_array_index_type, stride, num);
          tmp = build (MODIFY_EXPR, g95_array_index_type, stride, tmp);
          ADD_EXPR_STMT (tmp);
          /* Save it in the descriptor.  */
          field = g95_get_stride_component (type, n);
          tmp = build (COMPONENT_REF, g95_array_index_type,
                        sym->backend_decl, field);
          tmp = build (MODIFY_EXPR, g95_array_index_type, tmp, stride);
          ADD_EXPR_STMT (tmp);

          if (n > 0)
            {
              /* Calculate the offset (stride*lbound).  */
              tmp = build (MULT_EXPR, g95_array_index_type,
                            stride, lbound.expr);
              tmp = build (MODIFY_EXPR, g95_array_index_type, num, tmp);
              ADD_EXPR_STMT (tmp);

              /* Add to the total offset.  */
              tmp = build (PLUS_EXPR, g95_array_index_type, offset, num);
            }
          else
            tmp = lbound.expr;

          /* Set the new offset.  */
          tmp = build (MODIFY_EXPR, g95_array_index_type, offset, tmp);
          ADD_EXPR_STMT (tmp);
        }
    } /* For (rank).  */

  /* Get the type of a sigle element.  */
  element_type = TREE_TYPE (data_pointer_type);
  while (TREE_CODE (element_type) == ARRAY_TYPE)
    element_type = TREE_TYPE (element_type);

  /* Multiply by the size of a single element.  */
  tmp = build (MULT_EXPR, g95_array_index_type, size,
          TYPE_SIZE_UNIT (element_type));
  tmp = build (MODIFY_EXPR, g95_array_index_type, size, tmp);
  ADD_EXPR_STMT (tmp);

  /* For GCC arrays store the pointer in the first component (data),
     otherwise use the last component (block).  */
  if (g95_use_gcc_arrays)
    type = g95_get_data_component (type);
  else
    field = g95_get_block_component (type);
  /* Now allocate the memory.  Parameters to __g95_internal_alloc
     are address of data pointer and size of block.  */
  tmp = build (COMPONENT_REF, data_pointer_type,
                sym->backend_decl, field);
  tmp = build1 (ADDR_EXPR,
                build_pointer_type (build_pointer_type (void_type_node)),
                tmp);
  tmp = tree_cons (NULL_TREE, tmp, NULL_TREE);
  tmp = chainon (tmp, tree_cons (NULL_TREE, size, NULL_TREE));
  tmp = g95_build_function_call (g95_fndecl_internal_alloc, tmp);
  /* __g95_internal_alloc (&descriptor->data, size) */
  ADD_EXPR_STMT (tmp);

  /* For Type B arrays we need to bias the data pointer.  */
  if (! g95_use_gcc_arrays)
    {
      tree ref;
      tree pointer;

      assert (TREE_TYPE( TREE_TYPE (data_pointer_type)) == element_type);
      /* offset = -offset */
      tmp = build1 (NEGATE_EXPR, g95_array_index_type, offset);
      tmp = build (MODIFY_EXPR, g95_array_index_type, offset, tmp);
      ADD_EXPR_STMT (tmp);

      /* pointer = array.block */
      pointer = g95_create_tmp_var (data_pointer_type);
      tmp = build (COMPONENT_REF, data_pointer_type,
                    sym->backend_decl, field);
      tmp = build (MODIFY_EXPR, data_pointer_type, num, tmp);
      ADD_EXPR_STMT (tmp);

      /* array.data = &pointer[offset] */
      field = g95_get_data_component (type);
      tmp = build1 (INDIRECT_REF, TREE_TYPE (data_pointer_type), pointer);
      tmp = build (ARRAY_REF, element_type, tmp, offset);
      tmp = build1 (ADDR_EXPR, data_pointer_type, tmp);
      ref = build (COMPONENT_REF, data_pointer_type,
                    sym->backend_decl, field);
      tmp = build (MODIFY_EXPR, data_pointer_type, ref, tmp);
      ADD_EXPR_STMT (tmp);
    }

  /* That's all the initialisation code for this array.  */
  head = g95_finish_stmt (head, tail);

  return head;
}

/* Modify the descriptor for an array parameter so that it has the
   correct lower bound.  Also move the upper bound accordingly
   Bias the data pointer for Type B arrays.  */
/* TODO: Check that the bounds match for AS_EXPLICIT parameters.  */
static tree
g95_trans_dummy_array_bias (g95_symbol * sym, tree body)
{
  tree stmt;
  tree saved;
  tree type;
  tree descriptor;
  tree tmp;
  tree offset;
  tree num;
  tree delta;
  tree head;
  tree tail;
  tree field;
  tree saved_field;
  tree dest;
  g95_se lbound;
  int n;

  assert (TREE_CODE (TREE_TYPE (sym->backend_decl)) = REFERENCE_TYPE);
  /* Descriptor type.  */
  type = TREE_TYPE (TREE_TYPE (sym->backend_decl));
  assert (TREE_CODE (type) == RECORD_TYPE);

  saved = g95_create_tmp_var (g95_get_descriptorsave_type (sym->as->rank));
  saved_field = TYPE_FIELDS (TREE_TYPE (saved));
  descriptor = build1 (INDIRECT_REF, type, sym->backend_decl);

  /* Wrap up the initialisation in its own scope.  Note that saved must be
     created outside this block so that it has function scope.  */
  g95_start_stmt ();

  head = tail = NULL_TREE;

  num = g95_create_tmp_var (g95_array_index_type);
  delta = g95_create_tmp_var (g95_array_index_type);
  if (! g95_use_gcc_arrays)
    offset = g95_create_tmp_var (g95_array_index_type);
  else
    offset = NULL_TREE;

  /* First check that it's the corret type of descriptor.  If we will  call
     __g95_array_mismatch () which throws a runtime error.  */
  /* num = descriptor.stride00 */
  field = g95_get_stride_component(type, 0);
  tmp = build (COMPONENT_REF, g95_array_index_type, descriptor, field);
  tmp = build (MODIFY_EXPR, g95_array_index_type, num, tmp);
  ADD_EXPR_STMT (tmp);

  tmp = g95_build_function_call (g95_fndecl_array_mismatch, NULL_TREE);
  stmt = build_stmt (EXPR_STMT, tmp);

  tmp = build (NE_EXPR, boolean_type_node, num,
                g95_use_gcc_arrays ? integer_zero_node : integer_one_node);
  stmt = build_stmt (IF_STMT, tmp, stmt, NULL_TREE);
  g95_add_stmt_to_list (&head, &tail, stmt, stmt);

  if (! g95_use_gcc_arrays)
    {
      /* Save the data pointer.  */
      field = g95_get_data_component (type);
      assert (TREE_CODE (TREE_TYPE (saved_field)) == POINTER_TYPE);

      tmp = build (COMPONENT_REF, TREE_TYPE (field), descriptor, field);
      dest = build (COMPONENT_REF, TREE_TYPE (field), saved, saved_field);
      tmp = build (MODIFY_EXPR, TREE_TYPE (field), dest, tmp);
      ADD_EXPR_STMT (tmp);

      saved_field = TREE_CHAIN (saved_field);
    }

  for (n = 0 ; n < sym->as->rank ; n++)
    {
      g95_init_se (&lbound, NULL);

      g95_conv_simple_val (&lbound, sym->as->lower[n]);
      g95_make_safe_expr (&lbound);
      g95_add_stmt_to_list (&head, &tail, lbound.pre, lbound.pre_tail);

      /* Get passed lower bound.  */
      field = g95_get_lbound_component (type, n);
      tmp = build (COMPONENT_REF, g95_array_index_type, descriptor, field);
      tmp = build (MODIFY_EXPR, g95_array_index_type, num, tmp);
      ADD_EXPR_STMT (tmp);

      /* How much do we need to change the bound, store in delta.  */
      tmp = build (MINUS_EXPR, g95_array_index_type, lbound.expr, num);
      tmp = build (MODIFY_EXPR, g95_array_index_type, delta, tmp);
      ADD_EXPR_STMT (tmp);

      /* Save this.  */
      tmp = build (COMPONENT_REF, g95_array_index_type, saved, saved_field);
      tmp = build (MODIFY_EXPR, g95_array_index_type, tmp, delta);
      ADD_EXPR_STMT (tmp);

      saved_field = TREE_CHAIN (saved_field);

      /* Store the new bound.  */
      field = g95_get_lbound_component (type, n);
      tmp = build (COMPONENT_REF, g95_array_index_type, descriptor, field);
      tmp = build (MODIFY_EXPR, g95_array_index_type, tmp, lbound.expr);
      ADD_EXPR_STMT (tmp);

      /* Get the upper bound.  */
      field = g95_get_ubound_component (type, n);
      tmp = build (COMPONENT_REF, g95_array_index_type, descriptor, field);
      tmp = build (MODIFY_EXPR, g95_array_index_type, num, tmp);
      ADD_EXPR_STMT (tmp);

      /* Adjust and store back.  */
      tmp = build (PLUS_EXPR, g95_array_index_type, num, delta);
      dest = build (COMPONENT_REF, g95_array_index_type, descriptor, field);
      tmp = build (MODIFY_EXPR, g95_array_index_type, dest, tmp);
      ADD_EXPR_STMT (tmp);


      /* For Type A arrays data is correct.  For Type B arrays we need to bias
         the data pointer.  */
      if (! g95_use_gcc_arrays)
        {
          /* Stride == 1 for first dimension.  */
          if (n > 0)
            {
              /* Get the stide.  */
              field = g95_get_stride_component (type, n);
              tmp = build (COMPONENT_REF, g95_array_index_type,
                            descriptor, field);
              tmp = build (MODIFY_EXPR, g95_array_index_type, num, tmp);
              ADD_EXPR_STMT (tmp);

              /* Multiply by delta */
              tmp = build (MULT_EXPR, g95_array_index_type, delta, num);
              tmp = build (MODIFY_EXPR, g95_array_index_type, num, tmp);
              ADD_EXPR_STMT (tmp);

              /* Add to total offset.  */
              tmp = build (PLUS_EXPR, g95_array_index_type, offset, num);
            }
          else
            tmp = delta;
          /* Store offset.  */
          tmp = build (MODIFY_EXPR, g95_array_index_type, offset, tmp);
          ADD_EXPR_STMT (tmp);
        }
    }

  if (! g95_use_gcc_arrays)
    {
      tree pointer;

      /* Get the data component.  */
      field = g95_get_data_component (type);
      pointer = g95_create_tmp_var (TREE_TYPE (field));

      tmp = build (COMPONENT_REF, TREE_TYPE (field), descriptor, field);
      tmp = build (MODIFY_EXPR, TREE_TYPE (field), pointer, tmp);
      ADD_EXPR_STMT (tmp);

      /* array->data = &array->data[offset] */
      tmp = build1 (INDIRECT_REF, TREE_TYPE (TREE_TYPE (pointer)), pointer);
      tmp = build (ARRAY_REF, TREE_TYPE (TREE_TYPE (tmp)), tmp, offset);
      tmp = build1 (ADDR_EXPR, TREE_TYPE (pointer), tmp);
      dest = build (COMPONENT_REF, TREE_TYPE (field), descriptor, field);
      tmp = build (MODIFY_EXPR, TREE_TYPE (field), dest, tmp);
      ADD_EXPR_STMT (tmp);
    }

  /* Add code to start of function.  */
  head = g95_finish_stmt (head, tail);
  body = chainon (head, body);

  /* Cleanup.  */
  g95_start_stmt ();

  head = tail = NULL_TREE;
  num = g95_create_tmp_var (g95_array_index_type);
  delta = g95_create_tmp_var (g95_array_index_type);

  saved_field = TYPE_FIELDS (TREE_TYPE (saved));

  if (! g95_use_gcc_arrays)
    {
      /* Restore the data pointer.  */
      field = g95_get_data_component (type);
      dest = build (COMPONENT_REF, TREE_TYPE (field), descriptor, field);
      tmp = build (COMPONENT_REF, TREE_TYPE (field), saved, saved_field);
      tmp = build (MODIFY_EXPR, TREE_TYPE (field), dest, tmp);
      ADD_EXPR_STMT (tmp);

      saved_field = TREE_CHAIN (saved_field);
    }

  /* Restore the bounds.  */
  for (n = 0 ; n < sym->as->rank ; n++)
    {
      tmp = build (COMPONENT_REF, TREE_TYPE (field), saved, saved_field);
      tmp = build (MODIFY_EXPR, TREE_TYPE (field), delta, tmp);
      ADD_EXPR_STMT (tmp);

      /* Get lower bound.  */
      field = g95_get_lbound_component (type, n);
      tmp = build (COMPONENT_REF, TREE_TYPE (field), descriptor, field);
      tmp = build (MODIFY_EXPR, TREE_TYPE (field), num, tmp);
      ADD_EXPR_STMT (tmp);
      /* Adjust and store back.  */
      tmp = build (MINUS_EXPR, TREE_TYPE (field), num, delta);
      dest = build (COMPONENT_REF, TREE_TYPE (field), descriptor, field);
      tmp = build (MODIFY_EXPR, TREE_TYPE (field), dest, tmp);
      ADD_EXPR_STMT (tmp);

      /* Same for upper bound.  */
      field = g95_get_ubound_component (type, n);
      tmp = build (COMPONENT_REF, TREE_TYPE (field), descriptor, field);
      tmp = build (MODIFY_EXPR, TREE_TYPE (field), num, tmp);
      ADD_EXPR_STMT (tmp);
      tmp = build (MINUS_EXPR, TREE_TYPE (field), num, delta);
      dest = build (COMPONENT_REF, TREE_TYPE (field), descriptor, field);
      tmp = build (MODIFY_EXPR, TREE_TYPE (field), dest, tmp);
      ADD_EXPR_STMT (tmp);

      saved_field = TREE_CHAIN (saved_field);
    }

  /* Add cleanup code to end of function.  */
  head = g95_finish_stmt (head, tail);
  body = chainon (body, head);

  return body;
}

static tree
g95_trans_array_vars (g95_symbol * sym, tree body)
{
  tree tmp;
  tree stmt;

  for ( ; sym ; sym = sym->tlink)
    {
      switch (sym->as->type)
        {
        case AS_EXPLICIT:
          if (sym->attr.dummy)
            body = g95_trans_dummy_array_bias (sym, body);
          else
            {
              stmt = g95_trans_auto_array_allocation (sym);

              /* Add to the start of the function body.  */
              body = chainon (stmt, body);
            }
          break;

        case AS_ASSUMED_SHAPE:
        case AS_ASSUMED_SIZE:
          /* Must be dummy parameters.  */
          assert (sym->attr.dummy);

          body = g95_trans_dummy_array_bias (sym, body);
          break;

        default:
          g95_todo_error("Array type %d initialisation", sym->as->type);
          break;
        }
    }

  /* Build a call to __g95_push_context ().  */
  tmp = g95_build_function_call (g95_fndecl_push_context, NULL_TREE);
  stmt = build_stmt (EXPR_STMT, tmp);

  /* Add to start of function body.  */
  body = chainon (stmt, body);

  /* Build a call to __g95_pop_context ().  */
  tmp = g95_build_function_call (g95_fndecl_pop_context, NULL_TREE);
  stmt = build_stmt (EXPR_STMT, tmp);

  /* Add to end of function body.  */
  body = chainon (body, stmt);

  return body;
}

/* Builds a function decl.  The remaining parameters are the types of the
   function arguments.  */
static tree
g95_build_library_function_decl VPARAMS((tree name, tree rettype, int nargs, ...))
{
  tree arglist;
  tree argtype;
  tree fntype;
  tree fndecl;

  /* Library functions must be declared with global scope.  */
  assert (current_function_decl == NULL_TREE);

  VA_OPEN (p, nargs);
  VA_FIXEDARG (p, tree, name);
  VA_FIXEDARG (p, tree, retval);
  VA_FIXEDARG (p, int, nargs);

  /* Create a list of the argument types.  */
  for (arglist = NULL_TREE ; nargs > 0 ; nargs--)
    {
      argtype = va_arg (p, tree);
      arglist = chainon (arglist, listify (argtype));
    }
  /* Terminate the list.  */
  arglist = chainon (arglist, listify (void_type_node));

  /* Build the function type and decl.  */
  fntype = build_function_type (rettype, arglist);
  fndecl = build_decl (FUNCTION_DECL, name, fntype);

  /* Mark this decl as external.  */
  DECL_EXTERNAL (fndecl) = 1;
  TREE_PUBLIC (fndecl) = 1;

  VA_CLOSE (p);

  pushdecl (fndecl);

  rest_of_decl_compilation (fndecl, NULL, 1, 0);

  return fndecl;
}

/* Make prototypes for runtime library functions.

   void __g95_push_context ()
   void __g95_pop_context ()
   void __g95_internal_alloc (void **, size_t)
*/
void
g95_build_builtin_function_decls (void)
{
  g95_fndecl_internal_alloc = g95_build_library_function_decl (
            get_identifier ("__g95_internal_alloc"),
            void_type_node,
            2,
            build_pointer_type (build_pointer_type (void_type_node)),
            g95_array_index_type);

  g95_fndecl_push_context = g95_build_library_function_decl (
            get_identifier ("__g95_push_context"),
            void_type_node,
            0);

  g95_fndecl_pop_context = g95_build_library_function_decl (
            get_identifier ("__g95_pop_context"),
            void_type_node,
            0);

  g95_fndecl_array_mismatch = g95_build_library_function_decl (
            get_identifier ("__g95_array_mismatch"),
            void_type_node,
            0);
}

/* This should really be in trans.c.  */
/* Generate code for a function.  */
void
g95_generate_function_code (g95_namespace * ns)
{
  tree fndecl;
  tree old_context;
  tree body;
  tree result;
  g95_symbol *sym;
  g95_formal_arglist *f;

  old_context = current_function_decl;

  sym = ns->proc_name;

  fndecl = sym->backend_decl;

  /* let GCC know the current scope is this function */
  current_function_decl = fndecl;

  /* print function name on the console at compile time
    (unless this feature was switched of by command line option "-quiet" */
  announce_function (fndecl);

  if (DECL_CONTEXT (fndecl) == NULL_TREE)
  {
    /* create RTL for function declaration */
    rest_of_decl_compilation (fndecl, NULL, 1, 0);
  }

  /* create RTL for function definition */
  make_decl_rtl (fndecl, NULL);

  /* line and file should not be 0 */
  init_function_start (fndecl, input_filename, /*line*/ 1);

  /* We're in function-at-a-time mode. */
  cfun->x_whole_function_mode_p = 1;

  /* Even though we're inside a function body, we still don't want to
     call expand_expr to calculate the size of a variable-sized array.
     We haven't necessarily assigned RTL to all variables yet, so it's
     not safe to try to expand expressions involving them.  */
  immediate_size_expand = 0;
  cfun->x_dont_save_pending_sizes_p = 1;

  /* Will be created as needed.  */
  current_fake_result_decl = NULL_TREE;

  /* function.c requires a push at the start of the function */
  pushlevel (0);

  /* Check that the frontend isn't still using this.  */
  assert (sym->tlink == NULL);

  g95_start_stmt ();

  current_function_return_label = NULL;

  /* Now generate SIMPLE code for this function.  */
  body = g95_trans_code (ns->code);

  /* Add a return label if needed.  */
  if (current_function_return_label)
    {
      body = chainon (body,
                      build_stmt (LABEL_STMT, current_function_return_label));
    }

  /* Add array parameters to the list of arrays which need cleanup.  */
  for (f = sym->formal ; f ; f = f->next)
    {
      if (f->sym->attr.dimension && TREE_USED (f->sym->backend_decl))
        {
          assert (f->sym->tlink == NULL);
          f->sym->tlink = sym->tlink;
          sym->tlink = f->sym;
        }
    }
  /* Add code to create and cleanup arrays.  */
  if (sym->tlink != NULL)
    {
      body = g95_trans_array_vars (sym->tlink, body);
      sym->tlink = NULL;
    }

  if (TREE_TYPE (DECL_RESULT (fndecl)) != void_type_node)
    {
      if (sym == sym->result)
        {
          result = current_fake_result_decl;
          current_fake_result_decl = NULL_TREE;
        }
      else
        result = sym->result->backend_decl;

      if (result == NULL_TREE)
        warning ("Function return value not set");
      else
        {
          /* Return the result.  */
          body = chainon (body, build_stmt (RETURN_STMT, build (MODIFY_EXPR,
                    TREE_TYPE (result), DECL_RESULT (fndecl), result)));
        }
    }

  /* Add all the decls we created during processing.  */
  body = chainon (saved_function_decls, body);
  saved_function_decls = NULL_TREE;

  body = g95_finish_stmt (body, NULL_TREE);

  DECL_SAVED_TREE (fndecl) = body;

  /* Finish off this function and send it for code generation.  */
  poplevel (1,0,1);
  BLOCK_SUPERCONTEXT (DECL_INITIAL (fndecl)) = fndecl;

  /* Output the SIMPLE tree.  */
  {
    FILE *dump_file;
    int dump_flags;

    dump_switch_p ("dump-tree-simple");
    dump_file = dump_begin (TDI_simple, &dump_flags);
    if (dump_file)
      {
        warning ("dumping %s", ns->proc_name->name);
        fprintf (dump_file, "dumping %s\n", ns->proc_name->name);
        print_c_tree (dump_file, DECL_SAVED_TREE (fndecl));
        dump_end (TDI_simple, dump_file);
      }
  }

  free_after_parsing (cfun);
  free_after_compilation (cfun);

  /* RTL generation.  */
  expand_function_body (fndecl);

  current_function_decl = old_context;
}

#include "gt-f95-trans-decl.h"
