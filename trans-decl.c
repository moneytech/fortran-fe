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

/* trans-decl.c -- Handling of backend function and variable decls, etc */

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
/* Only for g95_trans_code.  Shouldn't need to include this.  */
#include "trans-stmt.h"

#define MAX_LABEL_VALUE 99999

/* Holds the result of the function if no result variable specified.  */
static GTY(()) tree current_fake_result_decl;

static GTY(()) tree current_function_return_label;

/* Holds the variable DECLs for the current function.  */
static GTY (()) tree saved_function_decls = NULL_TREE;
static GTY (()) tree saved_parent_function_decls = NULL_TREE;

/* The namespace of the module we're currently generating.  Only used while
   outputting decls for module variables.  Do not rely on this being set.  */
static g95_namespace *module_namespace;

/* List of static constructor functions.  */
tree g95_static_ctors;

/* Function declarations for builtin library functions.  */
tree gfor_fndecl_push_context;
tree gfor_fndecl_pop_context;
tree gfor_fndecl_internal_malloc;
tree gfor_fndecl_internal_malloc64;
tree gfor_fndecl_internal_free;
tree gfor_fndecl_allocate;
tree gfor_fndecl_allocate64;
tree gfor_fndecl_deallocate;
tree gfor_fndecl_stop;
tree gfor_fndecl_runtime_error;
tree gfor_fndecl_repack[G95_MAX_DIMENSIONS];

/* Math functions.  Many other math functions are handled in
   trans-intrinsic.c.  */
tree gfor_fndecl_math_powf;
tree gfor_fndecl_math_pow;
tree gfor_fndecl_math_cpowf;
tree gfor_fndecl_math_cpow;
tree gfor_fndecl_math_cabsf;
tree gfor_fndecl_math_cabs;
tree gfor_fndecl_math_sign4;
tree gfor_fndecl_math_sign8;
tree gfor_fndecl_math_ishftc4;
tree gfor_fndecl_math_ishftc8;

/* String functions.  */
tree gfor_fndecl_copy_string;
tree gfor_fndecl_compare_string;
tree gfor_fndecl_concat_string;
tree gfor_fndecl_string_len_trim;

/* Other misc. runtime library functions.  */
tree gfor_fndecl_size0;
tree gfor_fndecl_size1;

static void
g95_add_decl_to_parent_function (tree decl)
{
  assert (decl);
  DECL_CONTEXT (decl) = DECL_CONTEXT (current_function_decl);
  DECL_NONLOCAL (decl) = 1;
  TREE_CHAIN (decl) = saved_parent_function_decls;
  saved_parent_function_decls = decl;
}

static void
g95_add_decl_to_function (tree decl)
{
  assert (decl);
  TREE_USED (decl) = 1;
  DECL_CONTEXT (decl) = current_function_decl;
  TREE_CHAIN (decl) = saved_function_decls;
  saved_function_decls = decl;
}

/* Build a  backend label declaration.
   Set TREE_USED for named lables.  For artificial labels it's up to the
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
      if (sym->ns->proc_name->backend_decl == current_function_decl)
        g95_add_decl_to_function (decl);
      else
        g95_add_decl_to_parent_function (decl);
    }

  /* If a variable is USE associated, it's always external.  */
  if (sym->attr.use_assoc)
    {
      DECL_EXTERNAL (decl) = 1;
      TREE_PUBLIC (decl) = 1;
    }
  else if (sym->module[0])
    {
      assert (current_function_decl == NULL_TREE);
      /* This is the declaration of a module variable.  */
      TREE_PUBLIC (decl) = 1;
      TREE_STATIC (decl) = 1;
    }

  if ((sym->attr.save || sym->attr.data || sym->value)
      && ! sym->attr.use_assoc)
    TREE_STATIC (decl) = 1;
}

/* Allocate the lang-specific part of a decl.  */
void
g95_allocate_lang_decl (tree decl)
{
  DECL_LANG_SPECIFIC (decl) = (struct lang_decl *)
    ggc_alloc_cleared (sizeof (struct lang_decl));
}

/* Remember a symbol to generate initialization/cleanup code at function
   entry/exit.  */
static void
g95_defer_symbol_init (g95_symbol * sym)
{
  /* Don't add a symbol twice.  */
  if (sym->tlink)
    return;
  sym->tlink = sym->ns->proc_name->tlink;
  sym->ns->proc_name->tlink = sym;
}

/* Get a temporary decl for a dummy array parameter.  */
static tree
g95_build_dummy_array_decl (g95_symbol * sym, tree dummy)
{
  tree decl;
  tree type;
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
    g95_allocate_lang_decl (decl);
  G95_DECL_SAVED_DESCRIPTOR (decl) = dummy;
  G95_DECL_STRING (decl) = G95_DECL_STRING (dummy);

  /* Add to list of variables if not a fake result variable.  */
  if (sym->attr.result || sym->attr.dummy)
    g95_defer_symbol_init (sym);

  g95_add_decl_to_function (decl);

  return decl;
}

/* Return the decl for a g95_symbol, create it if it doesn't already
   exist.  */
tree
g95_get_symbol_decl (g95_symbol * sym)
{
  tree decl;
  tree length;
  g95_se se;

  if (sym->attr.dummy || sym->attr.result)
    {
      /* Return via extra parameter.  */
      if (sym->attr.result && g95_return_by_reference (sym->ns->proc_name)
          && ! sym->backend_decl)
        {
          sym->backend_decl =
            DECL_ARGUMENTS (sym->ns->proc_name->backend_decl);

        }

      assert (sym->backend_decl);

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

  /* Catch function declarations.  Only used for actual parameters.  */
  if (sym->attr.flavor == FL_PROCEDURE)
    {
      /* TODO: Frontend bug: no function or subroutine flag.  */
      if (! (sym->attr.function || sym->attr.subroutine))
        sym->attr.subroutine = 1;

      decl = g95_get_extern_function_decl (sym);
      return decl;
    }

  if (sym->attr.intrinsic)
    internal_error ("intrinsic variable which isn't a procedure");

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
      g95_defer_symbol_init (sym);

      if ((sym->attr.allocatable
           || ! sym->attr.dummy)
          && ! sym->attr.pointer)
        G95_DECL_PACKED_ARRAY (decl) = 1;
    }

  g95_finish_var_decl (decl, sym);

  /* TODO: Initialization of pointer variables.  */
  switch (sym->ts.type)
    {
    case BT_CHARACTER:
      /* Character variables need special handling.  */
      /* Character lengths are common for a whole array.  */

      g95_allocate_lang_decl (decl);
      G95_DECL_STRING (decl) = 1;

      if (sym->ts.cl->length->expr_type == EXPR_CONSTANT)
        {
          length = g95_conv_mpz_to_tree (sym->ts.cl->length->value.integer, 4);
          /* Static initializer.  */
          if (sym->value)
            {
              assert (TREE_STATIC (decl));
              if (sym->attr.pointer)
                g95_todo_error ("initialization of pointers");
              DECL_INITIAL (decl) = g95_conv_string_init (length, sym->value);
            }
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
          g95_defer_symbol_init (sym);
          assert (! sym->value);
        }

      G95_DECL_STRING_LENGTH (decl) = length;
      break;

    case BT_DERIVED:
      //g95_defer_symbol_init (sym);
      if (sym->value)
        g95_todo_error ("Derived type initializer");
      break;

    default:
      /* Static initializers for SAVEd variables.  Arrays have already been
         remembered.  */
      if (sym->value && ! sym->attr.dimension)
        {
          assert (TREE_STATIC (decl));
          g95_init_se (&se, NULL);
          g95_conv_constant (&se, sym->value);
          DECL_INITIAL (decl) = se.expr;
        }
      break;
    }
  sym->backend_decl = decl;

  return decl;
}

/* None of the specific intrinsics which can be passed as actual arguments
   for dummy procedures has more then two parameters.  */
#define G95_MAX_SPECIFIC_ARGS 2
/* Get a basic decl for an external function.  */
tree
g95_get_extern_function_decl (g95_symbol *sym)
{
  tree type;
  tree fndecl;
  g95_expr e;
  g95_intrinsic_sym *isym;
  g95_intrinsic_arg *formal;
  g95_expr argexpr[G95_MAX_SPECIFIC_ARGS];
  int n;
  char s[G95_MAX_SYMBOL_LEN];
  tree name;

  if (sym->backend_decl)
    return sym->backend_decl;

  if (sym->attr.intrinsic)
    {
      /* Call the resolution function to get the actual name.  */
      isym = g95_find_function (sym->name);
      assert (isym->resolve);

      memset (&e, 0, sizeof(e));
      memset (argexpr, 0, sizeof(argexpr));
      e.expr_type = EXPR_FUNCTION;
      formal = NULL;
      n = 0;

      for (formal = isym->formal, n = 0;
           formal;
           formal = formal->next, n++)
        {
          assert (n < G95_MAX_SPECIFIC_ARGS);
          argexpr[n].ts = formal->ts;
        }

      switch (n)
        {
        case 0:
          isym->resolve(&e);
          break;

        case 1:
          isym->resolve(&e, &argexpr[0]);
          break;

        case 2:
          isym->resolve(&e, &argexpr[0], &argexpr[1]);
          break;

        default:
          abort ();
        }
      sprintf (s, "specific%s", e.value.function.name);
      name = get_identifier (s);
    }
  else
    name = g95_sym_identifier (sym);

  type = g95_get_function_type (sym);
  fndecl = build_decl (FUNCTION_DECL, name, type);

  if (sym->module[0])
    SET_DECL_ASSEMBLER_NAME (fndecl, g95_sym_mangled_identifier (sym));
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

/* Create a declaration for a procedure.  For external functions (in the C
   sense) use g95_get_extern_function_decl.  */
static void
g95_build_function_decl (g95_symbol * sym)
{
  tree fndecl, type, result_decl, typelist, arglist;
  symbol_attribute attr;
  g95_formal_arglist *f;

  assert (! sym->backend_decl);
  assert (! sym->attr.external);

  /* Allow only one nesting level.  Allow external declarations.  */
  assert (current_function_decl == NULL_TREE
          || DECL_CONTEXT (current_function_decl) == NULL_TREE);

  type = g95_get_function_type (sym);
  fndecl = build_decl (FUNCTION_DECL, g95_sym_identifier (sym), type);

  if (sym->module[0])
    SET_DECL_ASSEMBLER_NAME (fndecl, g95_sym_mangled_identifier (sym));
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
          parm = build_decl (PARM_DECL, get_identifier ("__result"), type);

          DECL_CONTEXT (parm) = fndecl;
          DECL_ARG_TYPE (parm) = type;
          TREE_READONLY (parm) = 1;
          if (sym->ts.type == BT_CHARACTER)
            {
              g95_allocate_lang_decl (parm);
              G95_DECL_STRING (parm) = 1;

              assert (sym->ts.cl && sym->ts.cl->length
                      && sym->ts.cl->length->expr_type == EXPR_CONSTANT);
              G95_DECL_STRING_LENGTH (parm) =
                g95_conv_mpz_to_tree (sym->ts.cl->length->value.integer, 4);

            }
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
                  g95_allocate_lang_decl (parm);
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
      if (sym->as)
        decl = g95_build_dummy_array_decl (sym, decl);
    }
  else
    {
      sprintf (name, "__result_%.20s",
               IDENTIFIER_POINTER (DECL_NAME (current_function_decl)));

      decl = build_decl (VAR_DECL, get_identifier (name),
                         TREE_TYPE (TREE_TYPE (current_function_decl)));

      DECL_ARTIFICIAL (decl) = 1;
      DECL_EXTERNAL (decl) = 0;
      TREE_PUBLIC (decl) = 0;
      TREE_USED (decl) = 1;

      layout_decl (decl, 0);

      g95_add_decl_to_function (decl);
    }

  current_fake_result_decl = decl;

  return decl;
}

/* Builds a function decl.  The remaining parameters are the types of the
   function arguments.  Negative nargs indicates a varargs function.  */
tree
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
      arglist = g95_chainon_list (arglist, argtype);
    }

  if (nargs >= 0)
    {
      /* Terminate the list.  */
      arglist = g95_chainon_list (arglist, void_type_node);
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
g95_build_intrinsic_function_decls (void)
{
  /* String functions.  */
  gfor_fndecl_copy_string =
    g95_build_library_function_decl (get_identifier ("_gfor_copy_string"),
                                    void_type_node,
                                    4,
                                    g95_strlen_type_node, pchar_type_node,
                                    g95_strlen_type_node, pchar_type_node);

  gfor_fndecl_compare_string =
    g95_build_library_function_decl (get_identifier ("_gfor_compare_string"),
                                    g95_int4_type_node,
                                    4,
                                    g95_strlen_type_node, pchar_type_node,
                                    g95_strlen_type_node, pchar_type_node);

  gfor_fndecl_concat_string =
    g95_build_library_function_decl (get_identifier ("_gfor_concat_string"),
                                    void_type_node,
                                    6,
                                    g95_strlen_type_node, pchar_type_node,
                                    g95_strlen_type_node, pchar_type_node,
                                    g95_strlen_type_node, pchar_type_node);

  gfor_fndecl_string_len_trim =
    g95_build_library_function_decl (get_identifier ("_gfor_string_len_trim"),
                                    g95_int4_type_node,
                                    2, g95_strlen_type_node, pchar_type_node);

  /* Power functions.  */
  gfor_fndecl_math_powf =
    g95_build_library_function_decl (get_identifier ("powf"),
                                     g95_real4_type_node,
                                     1, g95_real4_type_node);
  gfor_fndecl_math_pow =
    g95_build_library_function_decl (get_identifier ("pow"),
                                     g95_real8_type_node,
                                     1, g95_real8_type_node);
  gfor_fndecl_math_cpowf =
    g95_build_library_function_decl (get_identifier ("cpowf"),
                                     g95_complex4_type_node,
                                     1, g95_complex4_type_node);
  gfor_fndecl_math_cpow =
    g95_build_library_function_decl (get_identifier ("cpow"),
                                     g95_complex8_type_node,
                                     1, g95_complex8_type_node);
  gfor_fndecl_math_cabsf =
    g95_build_library_function_decl (get_identifier ("cabsf"),
                                     g95_real4_type_node,
                                     1, g95_complex4_type_node);
  gfor_fndecl_math_cabs =
    g95_build_library_function_decl (get_identifier ("cabs"),
                                     g95_real8_type_node,
                                     1, g95_complex8_type_node);
  gfor_fndecl_math_sign4 =
    g95_build_library_function_decl (get_identifier ("copysignf"),
                                     g95_real4_type_node,
                                     1, g95_real4_type_node);
  gfor_fndecl_math_sign8 =
    g95_build_library_function_decl (get_identifier ("copysign"),
                                     g95_real8_type_node,
                                     1, g95_real8_type_node);
  gfor_fndecl_math_ishftc4 =
    g95_build_library_function_decl (get_identifier ("_gfor_ishftc4"),
                                     g95_int4_type_node,
                                     3, g95_int4_type_node,
                                     g95_int4_type_node, g95_int4_type_node);
  gfor_fndecl_math_ishftc8 =
    g95_build_library_function_decl (get_identifier ("_gfor_ishftc8"),
                                     g95_int8_type_node,
                                     3, g95_int8_type_node,
                                     g95_int8_type_node, g95_int8_type_node);
  /* Other functions.  */
  gfor_fndecl_size0 =
    g95_build_library_function_decl (get_identifier ("_gfor_size0"),
                                     g95_array_index_type,
                                     1, pvoid_type_node);
  gfor_fndecl_size1 =
    g95_build_library_function_decl (get_identifier ("_gfor_size1"),
                                     g95_array_index_type,
                                     2, pvoid_type_node, g95_array_index_type);
}

/* Make prototypes for runtime library functions.  */
void
g95_build_builtin_function_decls (void)
{
  int n;

  gfor_fndecl_internal_malloc = g95_build_library_function_decl (
            get_identifier ("_gfor_internal_malloc"),
            void_type_node,
            2,
            ppvoid_type_node,
            g95_int4_type_node);

  gfor_fndecl_internal_malloc64 = g95_build_library_function_decl (
            get_identifier ("_gfor_internal_malloc64"),
            void_type_node,
            2,
            ppvoid_type_node,
            g95_int8_type_node);

  gfor_fndecl_internal_free = g95_build_library_function_decl (
            get_identifier ("_gfor_internal_free"),
            void_type_node,
            1,
            ppvoid_type_node);

  gfor_fndecl_push_context = g95_build_library_function_decl (
            get_identifier ("_gfor_push_context"),
            void_type_node,
            0);

  gfor_fndecl_pop_context = g95_build_library_function_decl (
            get_identifier ("_gfor_pop_context"),
            void_type_node,
            0);

  gfor_fndecl_allocate = g95_build_library_function_decl (
            get_identifier ("_gfor_allocate"),
            void_type_node,
            2,
            ppvoid_type_node,
            g95_int4_type_node);

  gfor_fndecl_allocate64 = g95_build_library_function_decl (
            get_identifier ("_gfor_allocate64"),
            void_type_node,
            2,
            ppvoid_type_node,
            g95_int8_type_node);

  gfor_fndecl_deallocate = g95_build_library_function_decl (
            get_identifier ("_gfor_deallocate"),
            void_type_node,
            1,
            ppvoid_type_node);

  gfor_fndecl_stop = g95_build_library_function_decl (
            get_identifier ("_gfor_stop"),
            void_type_node,
            1,
            g95_int4_type_node);

  gfor_fndecl_runtime_error =
    g95_build_library_function_decl (get_identifier ("_gfor_runtime_error"),
                                    void_type_node,
                                    3,
                                    pchar_type_node, pchar_type_node,
                                    g95_int4_type_node);

  for (n = 0; n < G95_MAX_DIMENSIONS; n++)
    {
      char name[16];
      sprintf (name, "_gfor_repack_%d", n);
      gfor_fndecl_repack[n] =
        g95_build_library_function_decl (get_identifier (name),
                                        void_type_node,
                                        3, ppvoid_type_node, ppvoid_type_node,
                                        g95_int4_type_node);
    }

  g95_build_intrinsic_function_decls ();
  g95_build_intrinsic_lib_fndecls ();
  g95_build_io_library_fndecls ();
}

/* Allocate and cleanup an automatic character variable.  */
static tree
g95_trans_auto_character_variable (g95_symbol * sym, tree fnbody)
{
  tree tmp;
  tree args;
  tree len;
  stmtblock_t block;
  stmtblock_t body;

  assert (sym->ts.cl && sym->ts.cl->length);
  assert (sym->backend_decl != NULL_TREE);

  g95_start_block (&body);
  g95_start_block (&block);

  len = g95_conv_init_string_length (sym, &block);

  TREE_ADDRESSABLE (sym->backend_decl) = 1;
  tmp = build1 (ADDR_EXPR, ppvoid_type_node, sym->backend_decl);

  args = g95_chainon_list (NULL_TREE, tmp);
  args = g95_chainon_list (args, len);
  tmp = g95_build_function_call (gfor_fndecl_internal_malloc, args);
  g95_add_expr_to_block (&block, tmp);

  tmp = g95_finish_block (&block);
  g95_add_expr_to_block (&body, tmp);

  g95_add_expr_to_block (&body, fnbody);

  g95_start_block (&block);

  tmp = build1 (ADDR_EXPR, ppvoid_type_node, sym->backend_decl);

  args = tree_cons (NULL_TREE, tmp, NULL_TREE);
  tmp = g95_build_function_call (gfor_fndecl_internal_free, args);
  g95_add_expr_to_block (&block, tmp);

  tmp = g95_finish_block (&block);
  g95_add_expr_to_block (&body, tmp);

  return g95_finish_block (&body);
}

/* Generate function entry and exit code, and add it to the function body.
   This includes:
    Allocation and initialisation of array variables.
    Allocation of character string variables.
    Initialization and possibly repacking of dummy arrays.  */
static tree
g95_trans_deferred_vars (g95_symbol * proc_sym, tree fnbody)
{
  tree tmp;
  stmtblock_t block;
  locus loc;
  g95_symbol * sym;

  /* Deal with implicit return variables.  Explicit return variables will
     already have been added.  */
  if (g95_return_by_reference (proc_sym) && proc_sym->result == proc_sym)
    {
      if (! current_fake_result_decl)
        {
          warning ("Function does not return a value");
          return fnbody;
        }

      if (proc_sym->as)
        {
          fnbody = g95_trans_dummy_array_bias (proc_sym,
              current_fake_result_decl, fnbody);
        }
      else if (proc_sym->ts.type != BT_CHARACTER)
        g95_todo_error ("Deferred non-array return by reference");
    }

  for (sym = proc_sym->tlink; sym != proc_sym; sym = sym->tlink)
    {
      /* For now this is only array variables, but may get extended to
         derived types.  */
      if (sym->attr.dimension)
        {
          switch (sym->as->type)
            {
            case AS_EXPLICIT:
              if (sym->attr.dummy || sym->attr.result)
                fnbody =
                  g95_trans_dummy_array_bias (sym, sym->backend_decl, fnbody);
              else if (sym->attr.pointer || sym->attr.allocatable)
                {
                  if (TREE_STATIC (sym->backend_decl))
                    g95_trans_static_array_pointer (sym);
                  else
                    fnbody = g95_trans_deferred_array (sym, fnbody);
                }
              else
                {
                  g95_get_backend_locus (&loc);
                  g95_set_backend_locus (&sym->declared_at);
                  g95_init_block (&block);
                  tmp = g95_trans_auto_array_allocation (sym->backend_decl,
                      sym);
                  g95_add_expr_to_block (&block, tmp);
                  g95_add_expr_to_block (&block, fnbody);
                  fnbody = g95_finish_block (&block);
                  g95_set_backend_locus (&loc);
                }
              break;

            case AS_ASSUMED_SHAPE:
            case AS_ASSUMED_SIZE:
              /* These must be dummy parameters.  */
              assert (sym->attr.dummy);

              fnbody = g95_trans_dummy_array_bias (sym, sym->backend_decl,
                                                   fnbody);
              break;

            case AS_DEFERRED:
              fnbody = g95_trans_deferred_array (sym, fnbody);
              break;

            default:
              abort ();
            }
        }
      else if (sym->ts.type == BT_CHARACTER)
        {
          g95_get_backend_locus (&loc);
          g95_set_backend_locus (&sym->declared_at);
          fnbody = g95_trans_auto_character_variable (sym, fnbody);
          g95_set_backend_locus (&loc);
        }
      else
        abort ();
    }

  g95_init_block (&block);
  /* Build a call to _gfor_push_context ().  */
  tmp = g95_build_function_call (gfor_fndecl_push_context, NULL_TREE);
  g95_add_expr_to_block (&block, tmp);

  g95_add_expr_to_block (&block, fnbody);
  /* Build a call to _gfor_pop_context ().  */
  tmp = g95_build_function_call (gfor_fndecl_pop_context, NULL_TREE);
  g95_add_expr_to_block (&block, tmp);

  return g95_finish_block (&block);
}

static void
g95_create_module_variable (g95_symbol * sym)
{
  tree decl;
  g95_se se;

  /* Only output symbols from this module.  */
  if (sym->ns != module_namespace)
    {
      /* I don't think this should ever happen.  */
      internal_error ("module symbol %d in wrong namespace", sym->name);
    }

  /* Only output variables.  */
  if (sym->attr.flavor != FL_VARIABLE)
    return;

  /* Don't generate variables from other modules.  */
  if (sym->attr.use_assoc)
    return;

  if (sym->backend_decl)
    internal_error ("backend decl for module variable %s already exists");

  /* Create the decl.  */
  decl = g95_get_symbol_decl (sym);

  /* We want to allocate storage for this variable.  */
  TREE_STATIC (decl) = 1;

  if (sym->attr.dimension)
    {
      assert (sym->attr.pointer || sym->attr.allocatable
              || G95_ARRAY_TYPE_P (TREE_TYPE (sym->backend_decl)));
      if (sym->attr.pointer || sym->attr.allocatable)
        g95_trans_static_array_pointer (sym);
      else
        g95_trans_auto_array_allocation (sym->backend_decl, sym);
    }
  else if (sym->ts.type == BT_DERIVED)
    {
      if (sym->value)
        g95_todo_error ("Initialization of derived type module variables");
    }
  else
    {
      if (sym->value)
        {
          g95_init_se (&se, NULL);
          g95_conv_constant (&se, sym->value);
          DECL_INITIAL (decl) = se.expr;
        }
    }

  /* Create the variable.  */
  pushdecl (decl);
  rest_of_decl_compilation (decl, NULL, 1, 0);

  /* Also add length of strings.  */
  if (G95_DECL_STRING (decl))
    {
      tree length;

      length = G95_DECL_STRING_LENGTH (decl);
      pushdecl (length);
      rest_of_decl_compilation (length, NULL, 1, 0);
    }
}

/* Generate all the required code for module variables.  */
void
g95_generate_module_vars (g95_namespace * ns)
{
  module_namespace = ns;

  /* Check the frontend left the namespace in a reasonable state.  */
  assert (ns->proc_name && ! ns->proc_name->tlink);

  /* Create decls for all the module varuiables.  */
  g95_traverse_ns (ns, g95_create_module_variable);
}

static void
g95_generate_contained_functions (g95_namespace * parent)
{
  g95_namespace * ns;

  /* We create all the prototypes before generating any code.  */
  for (ns = parent->contained; ns; ns = ns->sibling)
    {
      /* Skip namespaces from used modules.  */
      if (ns->parent != parent)
        continue;

      g95_build_function_decl (ns->proc_name);
    }

  for (ns = parent->contained; ns; ns = ns->sibling)
    {
      /* Skip namespaces from used modules.  */
      if (ns->parent != parent)
        continue;

      g95_generate_function_code (ns);
    }
}

/* Generate code for a function.  */
void
g95_generate_function_code (g95_namespace * ns)
{
  tree fndecl;
  tree old_context;
  tree decl;
  tree tmp;
  stmtblock_t block;
  stmtblock_t body;
  tree result;
  g95_symbol *sym;

  /* Create the declaration for functions with global scope.  */
  if (! current_function_decl)
    g95_build_function_decl (ns->proc_name);

  old_context = current_function_decl;

  if (old_context)
    {
      push_function_context ();
      saved_parent_function_decls = saved_function_decls;
    }

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

  g95_start_block (&block);

  g95_generate_contained_functions (ns);

  /* Check that the frontend isn't still using this.  */
  assert (sym->tlink == NULL);
  sym->tlink = sym;

  current_function_return_label = NULL;

  /* Now generate the code for the body of this function.  */
  g95_init_block (&body);

  tmp = g95_trans_code (ns->code);
  g95_add_expr_to_block (&body, tmp);

  /* Add a return label if needed.  */
  if (current_function_return_label)
    {
      tmp = build_v (LABEL_EXPR, current_function_return_label);
      g95_add_expr_to_block (&body, tmp);
    }

  tmp = g95_finish_block (&body);
  /* Add code to create and cleanup arrays.  */
  tmp = g95_trans_deferred_vars (sym, tmp);
  g95_add_expr_to_block (&block, tmp);

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
          /* Set the return value to the the dummy result variable.  */
          tmp = build (MODIFY_EXPR, TREE_TYPE (result),
                          DECL_RESULT (fndecl), result);
          tmp = build_v (RETURN_EXPR, tmp);
          g95_add_expr_to_block (&block, tmp);
        }
    }

  if (g95_current_io_state)
    {
      g95_add_decl_to_function (g95_current_io_state);
      g95_current_io_state = NULL_TREE;
    }

  /* Add all the decls we created during processing.  */
  decl = saved_function_decls;
  while (decl)
    {
      tree next;

      next = TREE_CHAIN (decl);
      TREE_CHAIN (decl) = NULL_TREE;
      pushdecl (decl);
      decl = next;
    }
  saved_function_decls = NULL_TREE;

  DECL_SAVED_TREE (fndecl) = g95_finish_block (&block);

  /* Finish off this function and send it for code generation.  */
  poplevel (1, 0, 1);
  BLOCK_SUPERCONTEXT (DECL_INITIAL (fndecl)) = fndecl;

  /* Output the SIMPLE tree.  */
  {
    FILE *dump_file;
    int dump_flags = 0;
    tree fnbody;

    fnbody = DECL_SAVED_TREE (fndecl);

    dump_file = dump_begin (TDI_simple, &dump_flags);
    if (dump_file)
      {
        fprintf (dump_file, "%s()\n", ns->proc_name->name);
        if (fnbody)
          {
            if (dump_flags & TDF_RAW)
              dump_node (fnbody, TDF_SLIM | dump_flags, dump_file);
            else
              print_c_tree (dump_file, fnbody);
          }
        fprintf (dump_file, "\n");
        dump_end (TDI_simple, dump_file);
      }
  }

  free_after_parsing (cfun);
  free_after_compilation (cfun);

  /* RTL generation.  */
  if (! old_context)
    expand_function_body (fndecl, 0);

  if (old_context)
    {
      pop_function_context ();
      saved_function_decls = saved_parent_function_decls;
    }

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

  abort ();
#if 0
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

  expand_function_body (fndecl, 0);

  current_function_decl = NULL_TREE;
#endif
}

#include "gt-f95-trans-decl.h"
