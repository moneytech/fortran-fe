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
#include "tree-simple.h"
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
#include "trans-array.h"
#include "trans-const.h"
/* Only for g95_trans_code.  Souldn't need to include this.  */
#include "trans-stmt.h"

#define MAX_LABEL_VALUE 99999

/* Holds the result of the function if no result variable specified.  */
static GTY(()) tree current_fake_result_decl;

static GTY(()) tree current_function_return_label;

/* Holds the variable DECLs for the current function.  */
static GTY (()) tree saved_function_decls = NULL_TREE;

/* List of static constructor functions.  */
tree g95_static_ctors;

/* Function declarations for builtin library functions.  */
tree g95_fndecl_push_context;
tree g95_fndecl_pop_context;
tree g95_fndecl_internal_malloc;
tree g95_fndecl_internal_malloc64;
tree g95_fndecl_internal_free;
tree g95_fndecl_allocate;
tree g95_fndecl_allocate64;
tree g95_fndecl_deallocate;
tree g95_fndecl_stop;
tree g95_fndecl_runtime_error;
tree g95_fndecl_repack[G95_MAX_DIMENSIONS];

/* String functions.  */
tree g95_fndecl_copy_string;
tree g95_fndecl_compare_string;
tree g95_fndecl_concat_string;

/* IO library decls.  */
tree g95_fndecl_write_begin;
tree g95_fndecl_write_character;

/* These must be consistent with g95_io_fndec_enum.  */
g95_io_fndecl_t g95_io_fndecls[GFORIO_NUM_FNDECLS] =
{
  {"int4", &g95_int4_type_node, NULL_TREE, NULL_TREE},
  {"int8", &g95_int8_type_node, NULL_TREE, NULL_TREE},
  {"real4", &g95_real4_type_node, NULL_TREE, NULL_TREE},
  {"real8", &g95_real8_type_node, NULL_TREE, NULL_TREE},
  {"complex4", &g95_complex4_type_node, NULL_TREE, NULL_TREE},
  {"complex8", &g95_complex8_type_node, NULL_TREE, NULL_TREE},
  {"logical4", &g95_logical4_type_node, NULL_TREE, NULL_TREE}
};

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

/* Construct mangled name from symbol name.  */
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

}

/* Apply symbol attributes to a variable, and add it to the function scope.  */
static void
g95_finish_var_decl (tree decl, g95_symbol * sym)
{
  /* TREE_ADDRESSABLE means the address of this variable is acualy needed.
     This is the equivalent of the TARGET variables.
     We also need to set this if the variable is passed by reference in a
     CALL statement.  */
  if (sym->attr.target)
    TREE_ADDRESSABLE (decl)=1;
  /* If it wasn't used we wouldn't be getting it.  */
  TREE_USED (decl)=1;

  /* Chain this decl to the pending declarations.  Don't do pushdecl()
     because this would add them to the current scope rather than the
     function scope.  */
  if (current_function_decl != NULL_TREE)
  {
    saved_function_decls =
      chainon (build_stmt (DECL_STMT, decl), saved_function_decls);
  }

  /* If a variable is USE associated, it's always external.  */
  if (sym->attr.use_assoc)
    DECL_EXTERNAL (decl) = 1;
  else if (sym->module[0])
    {
      assert (current_function_decl == NULL_TREE);
      /* This is the declaration of a module variable.  */
      TREE_PUBLIC (decl) = 1;
      TREE_STATIC (decl) = 1;
    }

  if (sym->attr.save)
    TREE_STATIC (decl) = 1;
}

/* Get a temporary decl for a dummy array parameter.  */
static tree
g95_build_dummy_array_decl (g95_symbol * sym, tree dummy)
{
  tree decl;
  tree type;
  tree stmt;
  char *name;

  if (sym->attr.pointer || sym->attr.allocatable)
    return dummy;

  type = TREE_TYPE (dummy);
  assert (TREE_CODE (dummy) == PARM_DECL
         && TREE_CODE (type) == REFERENCE_TYPE);
  type = TREE_TYPE (type);
  assert (G95_DESCRIPTOR_TYPE_P (type));
  ASM_FORMAT_PRIVATE_NAME (name,
      IDENTIFIER_POINTER (DECL_NAME (dummy)), 0);
  decl = build_decl (VAR_DECL, get_identifier (name), type);

  DECL_ARTIFICIAL (decl) = 1;
  TREE_PUBLIC (decl) = 0;
  TREE_STATIC (decl) = 0;
  DECL_EXTERNAL (decl) = 0;

  if (sym->as->type == AS_DEFERRED)
    internal_error("possible g95 frontend bug: deferred shape dummy array");

  if (! g95_option.no_repack_arrays)
    {
      if (sym->as->type == AS_EXPLICIT)
        G95_DECL_PACKED_ARRAY (decl) = 1;
      else
        G95_DECL_PARTIAL_PACKED_ARRAY (decl) = 1;
    }

  if (DECL_LANG_SPECIFIC (dummy))
    DECL_LANG_SPECIFIC (decl) = DECL_LANG_SPECIFIC (dummy);
  else
    {
      DECL_LANG_SPECIFIC (decl) = (struct lang_decl *)
        ggc_alloc_cleared (sizeof (struct lang_decl));
    }
  G95_DECL_SAVED_DESCRIPTOR (decl) = dummy;
  G95_DECL_STRING (decl) = G95_DECL_STRING (dummy);

  /* Add to list of variables if not a fake result variable.  */
  if (sym->attr.result || sym->attr.dummy)
    {
      sym->tlink = sym->ns->proc_name->tlink;
      sym->ns->proc_name->tlink = sym;
    }

  stmt = build_stmt (DECL_STMT, decl);
  TREE_CHAIN (stmt) = saved_function_decls;
  saved_function_decls = stmt;

  return decl;
}

/* Return the decl for a g95_symbol, create it if it doesn't already
   exist.  */
tree
g95_get_symbol_decl (g95_symbol * sym)
{
  tree decl;
  tree length;

  if (sym->attr.dummy || sym->attr.result)
    {
      assert (sym->backend_decl);

      /* Return via extra parameter.  */
      if (sym->attr.result && g95_return_by_reference (sym->ns->proc_name)
          && ! sym->backend_decl)
        {
          sym->backend_decl =
            DECL_ARGUMENTS (sym->ns->proc_name->backend_decl);

        }

      /* Use a copy of the descriptor for dummy arrays.  */
      if (sym->attr.dimension
          && TREE_CODE (sym->backend_decl) == PARM_DECL)
        {
          TREE_USED (sym->backend_decl) = 1;
          sym->backend_decl =
            g95_build_dummy_array_decl (sym, sym->backend_decl);
          return sym->backend_decl;
        }
      /* Dummy variables should already have been created.  */
      TREE_USED (sym->backend_decl) = 1;
      return sym->backend_decl;
    }

  if (sym->backend_decl)
    return sym->backend_decl;

  if (sym->attr.in_common)
    g95_todo_error ("common variables");
  else if (sym->attr.entry)
     g95_todo_error ("alternate entry");
  else if (sym->attr.intrinsic)
    g95_todo_error ("intrinsics");

  /* Catch external function declarations.  */
  assert (! (sym->attr.function || sym->attr.subroutine));

  decl = build_decl (VAR_DECL,
                     g95_sym_identifier (sym),
                     g95_sym_type (sym));

  /* Symbold from modules have its assembler name should be manged.
     This is done here rather than in g95_finish_var_decl because it
     is different for string length variables.  */
  if (sym->module[0])
    SET_DECL_ASSEMBLER_NAME (decl, g95_sym_mangled_identifier (sym));

  if (sym->attr.dimension)
    {
      /* Remember this variable for allocation/cleanup.  */
      sym->tlink = sym->ns->proc_name->tlink;
      sym->ns->proc_name->tlink = sym;

      if (sym->attr.pointer
          || sym->attr.allocatable
          || sym->attr.target
          || ! sym->attr.dummy)
        G95_DECL_PACKED_ARRAY (decl) = 1;
    }

  if (sym->ns->proc_name->backend_decl != current_function_decl)
    g95_todo_error ("Use of parent variable in nested subroutine");

  g95_finish_var_decl (decl, sym);

  /* Character variables need special handling.  */
  if (sym->ts.type == BT_CHARACTER)
    {
      /*Character lengths are common for a whole array.  */

      DECL_LANG_SPECIFIC (decl) = (struct lang_decl *)
        ggc_alloc_cleared (sizeof (struct lang_decl));
      G95_DECL_STRING (decl) = 1;

      if (sym->ts.cl->length->expr_type == EXPR_CONSTANT)
        {
          length = g95_conv_mpz_to_tree (sym->ts.cl->length->value.integer, 4);
        }
      else
        {
          char name[G95_MAX_MANGLED_SYMBOL_LEN+2];

          /* Create annother variable to hold the length.  Prefix the name
             to avoid conflicts.  */
          strcpy (&name[1], sym->name);
          name[0]='.';
          length = build_decl (VAR_DECL, get_identifier(name),
                              g95_strlen_type_node);

          DECL_ARTIFICIAL (decl) = 1;
          /* Also prefix the mangled name for symbols from modules.  */
          if (sym->module[0])
            {
              strcpy (&name[1],
                     IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME (length)));
              SET_DECL_ASSEMBLER_NAME (decl, get_identifier (name));
            }
          g95_finish_var_decl (length, sym);
          /* Remember this variable for allocation/cleanup.  */
          sym->tlink = sym->ns->proc_name->tlink;
          sym->ns->proc_name->tlink = sym;
        }

      G95_DECL_STRING_LENGTH (decl) = length;
    }

  sym->backend_decl = decl;

  return decl;
}

/* Get a basic decl for an external function.  */
tree
g95_get_extern_function_decl (g95_symbol *sym)
{
  tree type;
  tree fndecl;

  if (sym->backend_decl)
    return sym->backend_decl;

  type = g95_get_function_type (sym);
  fndecl = build_decl (FUNCTION_DECL, g95_sym_identifier (sym), type);

  /* If the return type is a pointer, avoid alias issues by setting
     DECL_IS_MALLOC to nonzero. This means that the function should be
     treated as if it were a malloc, meaning it returns a pointer that
     is not an alias.  */
  if (POINTER_TYPE_P (type))
    DECL_IS_MALLOC (fndecl) = 1;

  /* Set up all attributes for the function.  */
  DECL_CONTEXT (fndecl) = current_function_decl;
  DECL_EXTERNAL (fndecl) = 1;

  /* This specifies if a function is globaly addressable, ie. it is
     the opposite of decalring static  in C.  */
  TREE_PUBLIC (fndecl) = 1;

  /* Set attributes for PURE functions. A call to PURE function in the
     Fortran 95 sense is both pure and without side effects in the C
     sense.  */
  if (sym->attr.pure || sym->attr.elemental)
    {
      DECL_IS_PURE (fndecl) = 1;
      TREE_SIDE_EFFECTS (fndecl) = 0;
    }

  sym->backend_decl = fndecl;

  return fndecl;
}

/* Get a function declaration.  Create it if it doesn't exist.  For external
   functions (in the C sense) use g95_get_extern_function_decl.  */
tree
g95_get_function_decl (g95_symbol * sym)
{
  tree fndecl, type, result_decl, typelist, arglist;
  symbol_attribute attr;
  g95_formal_arglist *f;

  if (sym->backend_decl)
    return sym->backend_decl;

  assert (! sym->attr.external);

  /* Allow only one nesting level.  Allow external declarations.  */
  assert (current_function_decl == NULL_TREE
          || DECL_CONTEXT (current_function_decl) == NULL_TREE);

  type = g95_get_function_type (sym);
  fndecl = build_decl (FUNCTION_DECL, g95_sym_identifier (sym), type);

  /* Figure out the return type of the declared function, and build a
     RESULT_DECL for it.  If this is subroutine with alternate
     returns, build a RESULT_DECL for it.  */
  attr = sym->attr;

  result_decl = NULL_TREE;
  if (attr.function)
    {
      if (g95_return_by_reference (sym))
        type = void_type_node;
      else
        {
          if (sym->result != sym)
            result_decl = g95_sym_identifier (sym->result);

          type = TREE_TYPE (TREE_TYPE (fndecl));
        }
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
  DECL_EXTERNAL (fndecl) = 0;

  /* This specifies if a function is globaly addressable, ie. it is
     the opposite of decalring static  in C.  */
  if (DECL_CONTEXT (fndecl) == NULL_TREE || attr.external)
    TREE_PUBLIC (fndecl) = 1;

  /* TREE_STATIC means the function body is defined here.  */
  if (! attr.external)
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
  if (! attr.external)
    {
      tree parm;

      pushdecl (fndecl);
      /* Build formal argument list. Make sure that their TREE_CONTEXT is
         the new FUNCTION_DECL node.  */
      current_function_decl = fndecl;
      arglist = NULL_TREE;
      typelist = TYPE_ARG_TYPES (TREE_TYPE (fndecl));
      if (g95_return_by_reference (sym))
        {
          type = TREE_VALUE (typelist);
          parm = build_decl (PARM_DECL, get_identifier ("__return"), type);

          DECL_CONTEXT (parm) = fndecl;
          DECL_ARG_TYPE (parm) = type;
          TREE_READONLY (parm) = 1;
          G95_DECL_PACKED_ARRAY (parm) = 1;
          g95_finish_decl (parm, NULL_TREE);

          arglist = chainon (arglist, parm);
          typelist = TREE_CHAIN (typelist);
        }

      for (f = sym->formal; f; f = f->next)
        {
          if (f->sym != NULL)       /* ignore alternate returns. */
            {
              tree length = NULL_TREE;

              type = TREE_VALUE (typelist);

              /* Character strings require an extra length parameter.  */
              if (f->sym->ts.type == BT_CHARACTER)
                {
                  char name[G95_MAX_SYMBOL_LEN + 2];

                  assert (type == g95_strlen_type_node);

                  strcpy (&name[1], f->sym->name);
                  name[0]='.';
                  length = build_decl (PARM_DECL, get_identifier(name), type);

                  DECL_CONTEXT (length) = fndecl;
                  DECL_ARG_TYPE (length) = type;
                  TREE_READONLY (length) = 1;
                  g95_finish_decl (length, NULL_TREE);

                  arglist = chainon (arglist, length);
                  typelist = TREE_CHAIN (typelist);
                  type = TREE_VALUE (typelist);
                }
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

              if (f->sym->ts.type == BT_CHARACTER)
                {
                  DECL_LANG_SPECIFIC (parm) = (struct lang_decl *)
                    ggc_alloc_cleared (sizeof (struct lang_decl));
                  G95_DECL_STRING (parm) = 1;

                  if (f->sym->ts.cl
                      && f->sym->ts.cl->length
                      && f->sym->ts.cl->length->expr_type == EXPR_CONSTANT)
                    {
                      length = g95_conv_mpz_to_tree (
                          f->sym->ts.cl->length->value.integer, 4);
                    }
                  else
                    TREE_USED (length) = 1;
                  G95_DECL_STRING_LENGTH (parm) = length;
                }
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
    }
  sym->backend_decl = fndecl;

  return fndecl;
}

/* Return the decl used to hold the function return value.  */
tree
g95_get_fake_result_decl (g95_symbol * sym)
{
  tree decl;
  char name[G95_MAX_SYMBOL_LEN+10];

  if (current_fake_result_decl != NULL_TREE)
    return current_fake_result_decl;

  if (g95_return_by_reference (sym))
    {
      decl = DECL_ARGUMENTS (sym->backend_decl);

      TREE_USED (decl) = 1;
      decl = g95_build_dummy_array_decl (sym, decl);
    }
  else
    {
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
    }

  current_fake_result_decl = decl;

  return decl;
}

/* Builds a function decl.  The remaining parameters are the types of the
   function arguments.  Negative nargs indicates a varargs function.  */
static tree
g95_build_library_function_decl VPARAMS((tree name, tree rettype, int nargs, ...))
{
  tree arglist;
  tree argtype;
  tree fntype;
  tree fndecl;
  int n;

  /* Library functions must be declared with global scope.  */
  assert (current_function_decl == NULL_TREE);

  VA_OPEN (p, nargs);
  VA_FIXEDARG (p, tree, name);
  VA_FIXEDARG (p, tree, retval);
  VA_FIXEDARG (p, int, nargs);


  /* Create a list of the argument types.  */
  for (arglist = NULL_TREE, n = abs (nargs); n > 0; n--)
    {
      argtype = va_arg (p, tree);
      arglist = chainon (arglist, listify (argtype));
    }

  if (nargs >= 0)
    {
      /* Terminate the list.  */
      arglist = chainon (arglist, listify (void_type_node));
    }

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

static void
g95_build_io_library_fndecls (void)
{
  int i;
  char name[G95_MAX_SYMBOL_LEN+1];

  for (i = 0; i < GFORIO_NUM_FNDECLS; i++)
    {
      sprintf (name, "_gforio_write_%s", g95_io_fndecls[i].name);
      g95_io_fndecls[i].write =
        g95_build_library_function_decl (get_identifier (name),
                                        void_type_node,
                                        1, *g95_io_fndecls[i].ptype);
      sprintf (name, "_gforio_read_%s", g95_io_fndecls[i].name);
      g95_io_fndecls[i].read =
        g95_build_library_function_decl (get_identifier (name),
                                        void_type_node,
                                        1, *g95_io_fndecls[i].ptype);
    }
}

static void
g95_build_intrinsic_function_decls (void)
{
  /* String functions.  */
  g95_fndecl_copy_string =
    g95_build_library_function_decl (get_identifier ("__g95_copy_string"),
                                    void_type_node,
                                    4,
                                    g95_strlen_type_node, pchar_type_node,
                                    g95_strlen_type_node, pchar_type_node);

  g95_fndecl_compare_string =
    g95_build_library_function_decl (get_identifier ("__g95_compare_string"),
                                    g95_int4_type_node,
                                    4,
                                    g95_strlen_type_node, pchar_type_node,
                                    g95_strlen_type_node, pchar_type_node);

  g95_fndecl_concat_string =
    g95_build_library_function_decl (get_identifier ("__g95_concat_string"),
                                    void_type_node,
                                    6,
                                    g95_strlen_type_node, pchar_type_node,
                                    g95_strlen_type_node, pchar_type_node,
                                    g95_strlen_type_node, pchar_type_node);

  /* IO library declarations.  */
  g95_fndecl_write_begin =
    g95_build_library_function_decl (get_identifier ("_gforio_write_begin"),
                                    void_type_node,
                                    -1, long_unsigned_type_node);

  g95_fndecl_write_character =
    g95_build_library_function_decl (get_identifier ("_gforio_write_character"),
                                    void_type_node,
                                    2, g95_strlen_type_node, pchar_type_node);
}

/* Make prototypes for runtime library functions.  */
void
g95_build_builtin_function_decls (void)
{
  int n;

  g95_fndecl_internal_malloc = g95_build_library_function_decl (
            get_identifier ("__g95_internal_malloc"),
            void_type_node,
            2,
            ppvoid_type_node,
            g95_int4_type_node);

  g95_fndecl_internal_malloc64 = g95_build_library_function_decl (
            get_identifier ("__g95_internal_malloc64"),
            void_type_node,
            2,
            ppvoid_type_node,
            g95_int8_type_node);

  g95_fndecl_internal_free = g95_build_library_function_decl (
            get_identifier ("__g95_internal_free"),
            void_type_node,
            1,
            ppvoid_type_node);

  g95_fndecl_push_context = g95_build_library_function_decl (
            get_identifier ("__g95_push_context"),
            void_type_node,
            0);

  g95_fndecl_pop_context = g95_build_library_function_decl (
            get_identifier ("__g95_pop_context"),
            void_type_node,
            0);

  g95_fndecl_allocate = g95_build_library_function_decl (
            get_identifier ("__g95_allocate"),
            void_type_node,
            2,
            ppvoid_type_node,
            g95_int4_type_node);

  g95_fndecl_allocate64 = g95_build_library_function_decl (
            get_identifier ("__g95_allocate64"),
            void_type_node,
            2,
            ppvoid_type_node,
            g95_int8_type_node);

  g95_fndecl_deallocate = g95_build_library_function_decl (
            get_identifier ("__g95_deallocate"),
            void_type_node,
            1,
            ppvoid_type_node);

  g95_fndecl_stop = g95_build_library_function_decl (
            get_identifier ("__g95_stop"),
            void_type_node,
            1,
            g95_int4_type_node);

  g95_fndecl_runtime_error =
    g95_build_library_function_decl (get_identifier ("__g95_runtime_error"),
                                    void_type_node,
                                    3,
                                    pchar_type_node, pchar_type_node,
                                    g95_int4_type_node);

  for (n = 0; n < G95_MAX_DIMENSIONS; n++)
    {
      char name[16];
      sprintf (name, "__g95_repack_%d", n);
      g95_fndecl_repack[n] =
        g95_build_library_function_decl (get_identifier (name),
                                        void_type_node,
                                        3, ppvoid_type_node, ppvoid_type_node,
                                        g95_int4_type_node);
    }

  g95_build_intrinsic_function_decls ();
  g95_build_io_library_fndecls ();
}

/* Allocate and cleanup an automatic character variable.  */
static tree
g95_trans_auto_character_variable (g95_symbol * sym, tree body)
{
  tree stmt;
  tree tmp;
  tree args;
  g95_se se;

  assert (sym->ts.cl && sym->ts.cl->length);
  assert (sym->backend_decl != NULL_TREE);

  g95_start_stmt ();

  g95_init_se (&se, NULL);
  g95_conv_simple_val_type (&se, sym->ts.cl->length, g95_int4_type_node);

  tmp = G95_DECL_STRING_LENGTH (sym->backend_decl);
  tmp = build (MODIFY_EXPR, g95_int4_type_node, tmp, se.expr);
  stmt = build_stmt (EXPR_STMT, tmp);
  g95_add_stmt_to_pre (&se, stmt, stmt);

  TREE_ADDRESSABLE (sym->backend_decl) = 1;
  tmp = build1 (ADDR_EXPR, ppvoid_type_node, sym->backend_decl);
  tmp = g95_simple_fold (tmp, &se.pre, &se.pre_tail, NULL);

  args = g95_chainon_list (NULL_TREE, tmp);
  args = g95_chainon_list (args, se.expr);
  tmp = g95_build_function_call (g95_fndecl_internal_malloc, args);
  stmt = build_stmt (EXPR_STMT, tmp);
  g95_add_stmt_to_pre (&se, stmt, stmt);

  stmt = g95_finish_stmt (se.pre, se.pre_tail);
  body = chainon (stmt, body);

  g95_start_stmt ();

  se.pre = se.pre_tail = NULL_TREE;
  tmp = build1 (ADDR_EXPR, ppvoid_type_node, sym->backend_decl);
  tmp = g95_simple_fold (tmp, &se.pre, &se.pre_tail, NULL);

  args = tree_cons (NULL_TREE, tmp, NULL_TREE);
  tmp = g95_build_function_call (g95_fndecl_internal_free, args);
  stmt = build_stmt (EXPR_STMT, tmp);
  g95_add_stmt_to_pre (&se, stmt, stmt);

  stmt = g95_finish_stmt (se.pre, se.pre_tail);
  body = chainon (body, stmt);

  return body;
}

/* Generate function entry and exit code, and add it to the function body.
   This includes:
    Allocation and initialisation of array variables.
    Allocation of character string variables.
    Initialization and possibly repacking of dummy arrays.  */
static tree
g95_trans_deferred_vars (g95_symbol * sym, tree body)
{
  tree tmp;
  tree stmt;
  locus loc;

  /* Deal with implicit return variables.  Explicit return variables will
     already have been added.  */
  if (g95_return_by_reference (sym) && sym->result == sym)
    {
      if (! current_fake_result_decl)
        {
          warning ("Function does not return a value");
          return body;
        }
      body = g95_trans_dummy_array_bias (sym, current_fake_result_decl, body);
    }

  for (sym = sym->tlink; sym; sym = sym->tlink)
    {
      /* For now this is only array variables, but may get extended to
         derived types.  */
      if (sym->attr.dimension)
        {
          if (sym->ts.type == BT_CHARACTER)
            g95_todo_error ("Arrays of character strings");
          switch (sym->as->type)
            {
            case AS_EXPLICIT:
              if (sym->attr.dummy)
                body =
                  g95_trans_dummy_array_bias (sym, sym->backend_decl, body);
              else
                {
                  g95_get_backend_locus (&loc);
                  g95_set_backend_locus (&sym->declared_at);
                  stmt = g95_trans_auto_array_allocation (sym->backend_decl,
                                                          sym->as);
                  g95_set_backend_locus (&loc);

                  /* Add to the start of the function body.  */
                  body = chainon (stmt, body);
                }
              break;

            case AS_ASSUMED_SHAPE:
            case AS_ASSUMED_SIZE:
              /* These must be dummy parameters.  */
              assert (sym->attr.dummy);

              body = g95_trans_dummy_array_bias (sym, sym->backend_decl, body);
              break;

            case AS_DEFERRED:
              body = g95_trans_deferred_array (sym, body);
              break;

            default:
              internal_error("Bad array type (%d)", sym->as->type);
              break;
            }
        }
      else if (sym->ts.type == BT_CHARACTER)
        {
          g95_get_backend_locus (&loc);
          g95_set_backend_locus (&sym->declared_at);
          body = g95_trans_auto_character_variable (sym, body);
          g95_set_backend_locus (&loc);
        }
      else
        abort();
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

/* Generate code for a function.  */
void
g95_generate_function_code (g95_namespace * ns)
{
  tree fndecl;
  tree old_context;
  tree body;
  tree result;
  g95_symbol *sym;

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

  /* Set the line and filename.  sym->decalred_at seems to point to the last
     statement for subroutines, but it'll do for now.  */
  g95_set_backend_locus (&sym->declared_at);

  /* line and file should not be 0 */
  init_function_start (fndecl, input_filename, lineno);

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

  /* Add code to create and cleanup arrays.  */
  body = g95_trans_deferred_vars (sym, body);

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
  saved_function_decls = NULL;

  body = g95_finish_stmt (body, NULL_TREE);

  DECL_SAVED_TREE (fndecl) = body;

  /* Finish off this function and send it for code generation.  */
  poplevel (1, 0, 1);
  BLOCK_SUPERCONTEXT (DECL_INITIAL (fndecl)) = fndecl;

  /* Output the SIMPLE tree.  */
  {
    FILE *dump_file;
    static int dump_flags = 0;
    /* TODO: make tree dumping a commandline switch.  */
#if 1
    static int first = 0;
    if (! first)
      {
        dump_switch_p ("dump-tree-simple");
        first = 1;
      }
#endif

    dump_file = dump_begin (TDI_simple, &dump_flags);
    if (dump_file)
      {
        fprintf (dump_file, "dumping %s\n", ns->proc_name->name);
        if (DECL_SAVED_TREE (fndecl) != NULL_TREE)
          {
            print_c_tree (dump_file, DECL_SAVED_TREE (fndecl));
            dump_node (DECL_SAVED_TREE (fndecl), dump_flags, dump_file);
          }
        dump_end (TDI_simple, dump_file);
      }
  }

  free_after_parsing (cfun);
  free_after_compilation (cfun);

  /* RTL generation.  */
  expand_function_body (fndecl);

  current_function_decl = old_context;
}

void
g95_generate_constructors ()
{
  tree fnname;
  tree type;
  tree fndecl;
  tree decl;
  tree tmp;

  if (g95_static_ctors == NULL_TREE)
    return;

  fnname = get_file_function_name ('I');
  type = build_function_type (void_type_node,
                             g95_chainon_list (NULL_TREE, void_type_node));

  fndecl = build_decl (FUNCTION_DECL, fnname, type);
  TREE_PUBLIC (fndecl) = 1;

  decl = build_decl (RESULT_DECL, NULL_TREE, void_type_node);
  DECL_CONTEXT (decl) = fndecl;
  DECL_RESULT (fndecl) = decl;

  pushdecl (fndecl);

  current_function_decl = fndecl;

  rest_of_decl_compilation (fndecl, NULL, 1, 0);

  make_decl_rtl (fndecl, NULL);

  init_function_start (fndecl, input_filename, lineno);

  cfun->x_whole_function_mode_p = 1;

  immediate_size_expand = 0;

  pushlevel (0);

  for (; g95_static_ctors; g95_static_ctors = TREE_CHAIN (g95_static_ctors))
    {
      tmp = g95_build_function_call (TREE_VALUE (g95_static_ctors), NULL_TREE);
      DECL_SAVED_TREE (fndecl) = build_stmt (EXPR_STMT, tmp);
    }

  poplevel (1, 0, 1);

  BLOCK_SUPERCONTEXT (DECL_INITIAL (fndecl)) = fndecl;

  free_after_parsing (cfun);
  free_after_compilation (cfun);

  expand_function_body (fndecl);

  current_function_decl = NULL_TREE;
}

#include "gt-f95-trans-decl.h"
