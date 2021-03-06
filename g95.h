/* G95 header file
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

/* g95.h-- It's probably insane to have this large of a header file,
 * but it seemed like everything had to be recompiled anyway when a
 * change was made to a header file, and there were ordering issues
 * with multiple header files.  Besides, Microsoft's winnt.h was 250k
 * last time I looked, so by comparison this is perfectly
 * reasonable. */


#ifndef IN_GCC         /* Defined only if included by backend code. */
#include "g95-config.h"
#endif

/* The following ifdefs are recommended by the autoconf documentation
 * for any code using alloca */

/* AIX requires this to be the first thing in the file. */
#ifdef __GNUC__
#else /* not __GNUC__ */
#ifdef HAVE_ALLOCA_H
#include <alloca.h>
#else /* do not HAVE_ALLOCA_H */
#ifdef _AIX
 #pragma alloca
#else
#ifndef alloca /* predefined by HP cc +Olibcalls */
char *alloca ();
#endif /* not predefined */
#endif /* not _AIX */
#endif /* do not HAVE_ALLOCA_H */
#endif /* not __GNUC__ */


#include <stdio.h> /* need FILE * here */

/* Major control parameters */

#define G95_VERSION "0.25"

#define G95_MAX_SYMBOL_LEN 31

#define G95_REAL_BITS 100  /* Number of bits in g95's floating point numbers */

#define G95_MAX_LINE 132        /* Characters beyond this are not seen */

#define G95_MAX_DIMENSIONS 7    /* Maximum dimensions in an array */

#define G95_LETTERS 26          /* Number of letters in the alphabet */

#define MAX_ERROR_MESSAGE 1000  /* Maximum length of an error message */

#define PREFIX "_g95_"


#define free(x) Use_g95_free_instead_of_free()
#define g95_is_whitespace(c) ((c==' ') || (c=='\t'))

#ifndef NULL
#define NULL ((void *) 0)
#endif

#ifndef GCC_TREE_H
typedef void *tree; /* Just a dummy place holder. */
#endif

/* Stringization */

#define stringize(x) expand_macro(x)
#define expand_macro(x) # x

/* Macro to initialize an mstring structure */

#define minit(s, t) { s, NULL, t }


/*************************** Enums *****************************/

/* The author remains confused to this day about the convention of
 * returning '0' for 'SUCCESS'... or was it the other way around?  The
 * following enum makes things much more readable.  We also start
 * values off at one instead of zero. */

typedef enum { SUCCESS=1, FAILURE } try;

/* Matchers return one of these three values.  The difference between
 * MATCH_NO and MATCH_ERROR is that MATCH_ERROR means that a match was
 * successful, but that something non-syntactic is wrong and an error
 * has already been issued. */

typedef enum { MATCH_NO=1, MATCH_YES, MATCH_ERROR } match;

typedef enum { FORM_FREE, FORM_FIXED, FORM_UNKNOWN } g95_source_form;

typedef enum { BT_UNKNOWN=1, BT_INTEGER, BT_REAL, BT_COMPLEX,
	       BT_LOGICAL, BT_CHARACTER, BT_DERIVED, BT_PROCEDURE
} bt;

/* Expression node types */

typedef enum { EXPR_UNKNOWN=1, EXPR_OP, EXPR_FUNCTION, EXPR_CONSTANT,
	       EXPR_VARIABLE, EXPR_SUBSTRING, EXPR_STRUCTURE, EXPR_ARRAY,
	       EXPR_NULL
} expr_t;

/* Intent types */

typedef enum { INTENT_UNKNOWN=0, INTENT_IN, INTENT_OUT, INTENT_INOUT
} sym_intent;

/* Access types */

typedef enum { ACCESS_PUBLIC=1, ACCESS_PRIVATE, ACCESS_UNKNOWN
} g95_access;

/* Array types */

typedef enum { AS_EXPLICIT=1, AS_ASSUMED_SHAPE, AS_DEFERRED,
	       AS_ASSUMED_SIZE, AS_UNKNOWN
} array_type;

typedef enum { AR_FULL=1, AR_ELEMENT, AR_SECTION, AR_UNKNOWN } ar_type;

/* Statement label types */

typedef enum { ST_LABEL_UNKNOWN=1, ST_LABEL_TARGET, ST_LABEL_BAD_TARGET,
	       ST_LABEL_BAD_TARGET2, ST_LABEL_FORMAT
} g95_sl_type;

/* Intrinsic operators */

typedef enum { INTRINSIC_NONE=-1, INTRINSIC_UPLUS=0, INTRINSIC_UMINUS,
	       INTRINSIC_PLUS, INTRINSIC_MINUS, INTRINSIC_TIMES,
	       INTRINSIC_DIVIDE, INTRINSIC_POWER, INTRINSIC_CONCAT,
	       INTRINSIC_AND, INTRINSIC_OR, INTRINSIC_EQV, INTRINSIC_NEQV,
	       INTRINSIC_EQ, INTRINSIC_NE, INTRINSIC_GT, INTRINSIC_GE,
	       INTRINSIC_LT, INTRINSIC_LE, INTRINSIC_NOT, INTRINSIC_USER,
	       INTRINSIC_ASSIGN
} g95_intrinsic_op;

/* This macro is the number of intrinsic operators that exist.
 * Assumptions are made about the numbering of the interface_op enums. */

#define G95_INTRINSIC_OPS (INTRINSIC_ASSIGN+1)

/* Arithmetic results */

typedef enum { ARITH_OK=1, ARITH_OVERFLOW, ARITH_UNDERFLOW,
               ARITH_DIV0, ARITH_0TO0, ARITH_INCOMMENSURATE
} arith;

/* Statements */

typedef enum {
  ST_ARITHMETIC_IF, ST_ALLOCATE, ST_ATTR_DECL, ST_BACKSPACE, ST_BLOCK_DATA,
  ST_CALL, ST_CASE, ST_CLOSE, ST_COMMON, ST_CONTINUE, ST_CONTAINS, ST_CYCLE,
  ST_DATA, ST_DATA_DECL, ST_DEALLOCATE, ST_DO, ST_ELSE, ST_ELSEIF,
  ST_ELSEWHERE, ST_END_BLOCK_DATA, ST_ENDDO, ST_IMPLIED_ENDDO,
  ST_END_FILE, ST_END_FORALL, ST_END_FUNCTION, ST_ENDIF, ST_END_INTERFACE,
  ST_END_MODULE, ST_END_PROGRAM, ST_END_SELECT, ST_END_SUBROUTINE,
  ST_END_WHERE, ST_END_TYPE, ST_ENTRY, ST_EQUIVALENCE, ST_EXIT, ST_FORALL,
  ST_FORALL_BLOCK, ST_FORMAT, ST_FUNCTION, ST_GOTO, ST_IF_BLOCK, ST_IMPLICIT,
  ST_IMPLICIT_NONE, ST_INQUIRE, ST_INTERFACE, ST_PARAMETER, ST_MODULE,
  ST_MODULE_PROC, ST_NAMELIST, ST_NULLIFY, ST_OPEN, ST_PAUSE, ST_PRIVATE,
  ST_PROGRAM, ST_PUBLIC, ST_READ, ST_RETURN, ST_REWIND, ST_STOP, ST_SUBROUTINE,
  ST_TYPE, ST_USE, ST_WHERE_BLOCK, ST_WHERE, ST_WRITE, ST_ASSIGNMENT,
  ST_POINTER_ASSIGNMENT, ST_SELECT_CASE, ST_SEQUENCE, ST_SIMPLE_IF,
  ST_STATEMENT_FUNCTION, ST_DERIVED_DECL, ST_NONE
} g95_statement;


/* Enum for what the compiler is currently doing */

typedef enum {
  COMP_NONE, COMP_PROGRAM, COMP_MODULE, COMP_SUBROUTINE, COMP_FUNCTION,
  COMP_BLOCK_DATA, COMP_INTERFACE, COMP_DERIVED, COMP_IF, COMP_DO,
  COMP_SELECT, COMP_FORALL, COMP_WHERE, COMP_CONTAINS
} g95_compile_state;


/* Types of interfaces that we can have.  Assignment interfaces are
 * considered to be intrinsic operators */

typedef enum {
  INTERFACE_NAMELESS=1,    INTERFACE_GENERIC,
  INTERFACE_INTRINSIC_OP,  INTERFACE_USER_OP
} interface_type;


/* Symbol flavors: these are all mutually exclusive.  */

typedef enum {
  FL_UNKNOWN=0, FL_PROGRAM, FL_BLOCK_DATA, FL_MODULE, FL_VARIABLE,
  FL_PARAMETER, FL_LABEL, FL_PROCEDURE, FL_DERIVED, FL_NAMELIST
} sym_flavor;   /* 10 elements = 4 bits */


/* Procedure types */

typedef enum { PROC_UNKNOWN, PROC_MODULE, PROC_INTERNAL, PROC_DUMMY,
	       PROC_INTRINSIC, PROC_ST_FUNCTION, PROC_EXTERNAL
} procedure_type;   /* 7 elements = 3 bits */


typedef enum { IFSRC_UNKNOWN=0, IFSRC_DECL, IFSRC_IFBODY, IFSRC_USAGE
} ifsrc;   /* 4 elements = 2 bits */


/* Enumeration of all the generic intrinsic functions.  Used for
 * identification of a function.  */

enum g95_generic_isym_id {
  /* G95_ISYM_NONE is used for intrinsics which will never be seen by
   * the backend (eg. KIND). */

  G95_ISYM_NONE=0, G95_ISYM_ABS, G95_ISYM_ACHAR, G95_ISYM_ACOS,
  G95_ISYM_ADJUSTL, G95_ISYM_ADJUSTR, G95_ISYM_AIMAG, G95_ISYM_AINT,
  G95_ISYM_ANINT, G95_ISYM_ALL, G95_ISYM_ALLOCATED, G95_ISYM_ANINIT,
  G95_ISYM_ANY, G95_ISYM_ASIN, G95_ISYM_ASSOCIATED, G95_ISYM_ATAN,
  G95_ISYM_ATAN2, G95_ISYM_BTEST, G95_ISYM_CEILING, G95_ISYM_CHAR,
  G95_ISYM_CMPLX, G95_ISYM_CONJG, G95_ISYM_COS, G95_ISYM_COSH,
  G95_ISYM_COUNT, G95_ISYM_CSHIFT, G95_ISYM_DBLE, G95_ISYM_DIM,
  G95_ISYM_DOT_PRODUCT, G95_ISYM_DPROD, G95_ISYM_EOSHIFT, G95_ISYM_EXP,
  G95_ISYM_EXPONENT, G95_ISYM_FLOOR, G95_ISYM_FRACTION, G95_ISYM_IACHAR,
  G95_ISYM_IAND, G95_ISYM_IBCLR, G95_ISYM_IBITS, G95_ISYM_IBSET,
  G95_ISYM_ICHAR, G95_ISYM_IEOR, G95_ISYM_INDEX, G95_ISYM_INT,
  G95_ISYM_IOR, G95_ISYM_ISHFT, G95_ISYM_ISHFTC, G95_ISYM_LBOUND,
  G95_ISYM_LEN, G95_ISYM_LEN_TRIM, G95_ISYM_LGE, G95_ISYM_LGT,
  G95_ISYM_LLE, G95_ISYM_LLT, G95_ISYM_LOG,  G95_ISYM_LOG10,
  G95_ISYM_LOGICAL, G95_ISYM_MATMUL, G95_ISYM_MAX, G95_ISYM_MAXLOC,
  G95_ISYM_MAXVAL, G95_ISYM_MERGE, G95_ISYM_MIN, G95_ISYM_MINLOC,
  G95_ISYM_MINVAL, G95_ISYM_MOD, G95_ISYM_MODULO, G95_ISYM_NEAREST,
  G95_ISYM_NINT, G95_ISYM_NOT, G95_ISYM_PACK, G95_ISYM_PRESENT,
  G95_ISYM_PRODUCT, G95_ISYM_REAL, G95_ISYM_REPEAT, G95_ISYM_RESHAPE,
  G95_ISYM_SCAN, G95_ISYM_SELECTED_INT_KIND, G95_ISYM_SELECTED_REAL_KIND,
  G95_ISYM_SET_EXPONENT, G95_ISYM_SHAPE, G95_ISYM_SIGN, G95_ISYM_SIN,
  G95_ISYM_SINH, G95_ISYM_SIZE, G95_ISYM_SPREAD, G95_ISYM_SQRT,
  G95_ISYM_SUM, G95_ISYM_TAN, G95_ISYM_TANH, G95_ISYM_TRANSFER,
  G95_ISYM_TRANSPOSE, G95_ISYM_TRIM, G95_ISYM_UBOUND, G95_ISYM_UNPACK,
  G95_ISYM_VERIFY, G95_ISYM_CONVERSION,

  G95_ISYM_CMOV
};


/************************* Structures *****************************/

/* Symbol attribute structure. */

typedef struct {

/* Variable attributes */
  unsigned allocatable:1, dimension:1,  external:1,    intrinsic:1,
           optional:1,    pointer:1,    save:1,        target:1,
           dummy:1,       result_var:1, entry:1;

  unsigned data:1,          /* Symbol is named in a DATA statement */
           use_assoc:1,     /* Symbol has been use-associated */
           equivalenced:1;  /* symbol appears in an EQUIVALENCE statement */

  unsigned implicit_type:1, untyped:1;    /* Type defined via implicit rules */
  unsigned used:1, set:1, artificial:1;

  unsigned in_namelist:1, in_common:1;
  unsigned function:1, subroutine:1, generic:1;

/* Function/subroutine attributes */

  unsigned sequence:1, elemental:1, pure:1, recursive:1;
  unsigned resolved:1;

/* Mutually exclusive multibit attributes */

  procedure_type proc:3;

  sym_flavor flavor:4;
  g95_access access:2;
  ifsrc if_source:2;
  sym_intent intent:2;

} symbol_attribute;


typedef struct g95_file {
  struct g95_file *included_by, *next, *up;
  int inclusion_line, line;
  char *filename;
} g95_file;


typedef struct g95_linebuf {
  int linenum;
  struct g95_file *file;
  struct g95_linebuf *next;

  char line[1];  /* Expands */
} g95_linebuf;


typedef struct {
  char *nextc;
  g95_linebuf *lb;
} g95_locus;


#include <limits.h>
#ifndef PATH_MAX
# include <sys/param.h>
# define PATH_MAX MAXPATHLEN
#endif


/* Structure for storing strings to be matched by g95_match_string */

typedef struct {
  char *string, *mp;
  int tag;
} mstring;


typedef struct {
  int flag;
  char message[MAX_ERROR_MESSAGE];
} g95_error_buf;

extern int g95_suppress_error;


/* Character length structures hold the expression that gives the
 * length of a character variable.  We avoid putting these into
 * g95_typespec because doing so prevents us from doing structure
 * copies and forces us to deallocate any typespecs we create, as well
 * as structures that contain typespecs.  They also can have multiple
 * character typespecs pointing to them.
 *
 * These structures form a singly linked list within the current
 * namespace and are deallocated with the namespace.  It is possible to
 * end up with g95_charlen structures that have nothing pointing to them. */

typedef struct g95_charlen {
  struct g95_expr *length;
  struct g95_charlen *next;
  tree backend_decl;
} g95_charlen;

#define g95_get_charlen() g95_getmem(sizeof(g95_charlen))

/* Type specification structure */

typedef struct {
  bt type;
  int kind;
  struct g95_symbol *derived;
  g95_charlen *cl;      /* For character types */
} g95_typespec;

/* Array specification */

typedef struct {
  int rank;          /* A rank of zero means that a variable is a scalar */
  array_type type;

  struct g95_expr *lower[G95_MAX_DIMENSIONS], *upper[G95_MAX_DIMENSIONS];
} g95_array_spec;

#define g95_get_array_spec() g95_getmem(sizeof(g95_array_spec))


/* Components of derived types */

typedef struct g95_component {
  char name[G95_MAX_SYMBOL_LEN+1];
  g95_typespec ts;

  int pointer, dimension, offset;
  g95_array_spec *as;

  tree backend_decl;
  g95_locus loc;
  struct g95_expr *initializer;
  struct g95_component *next;
} g95_component;

#define g95_get_component() g95_getmem(sizeof(g95_component))

/* Formal argument lists are lists of symbols.  */

typedef struct g95_formal_arglist {
  struct g95_symbol *sym;
  struct g95_formal_arglist *next;
} g95_formal_arglist;

#define g95_get_formal_arglist() g95_getmem(sizeof(g95_formal_arglist))


/* The g95_actual_arglist structure is for actual arguments */

typedef struct g95_actual_arglist {
  char name[G95_MAX_SYMBOL_LEN+1];

  enum {
    EXPR, ALT_RETURN, FULL_ARRAY, ARRAY_ELEMENT, ARRAY_DESC
  } type;
  int pointer;

  union {
    struct g95_st_label *label;
    struct g95_expr *expr;
  } u;

  bt missing_arg_type;

  struct g95_actual_arglist *next;
} g95_actual_arglist;

#define g95_get_actual_arglist() g95_getmem(sizeof(g95_actual_arglist))


/* Because a symbol can belong to multiple namelists, they must be
 * linked externally to the symbol itself. */

typedef struct g95_namelist {
  struct g95_symbol *sym;
  struct g95_namelist *next;
} g95_namelist;

#define g95_get_namelist() g95_getmem(sizeof(g95_namelist))

/* The g95_st_label structure is a doubly linked list attached to a
 * namespace that records the usage of statement labels within that space */

typedef struct g95_st_label {
  int value;

  g95_sl_type defined, referenced;

  struct g95_expr *format;
  int length;
  g95_locus where;

  tree backend_decl;

  struct g95_st_label *prev, *next;
} g95_st_label;


/* g95_interface()-- Interfaces are lists of symbols strung together */

typedef struct g95_interface {
  struct g95_symbol *sym;
  g95_locus where;
  struct g95_interface *next;
} g95_interface;

#define g95_get_interface() g95_getmem(sizeof(g95_interface))

/* User operator nodes.  These are like stripped down symbols */

typedef struct {
  char name[G95_MAX_SYMBOL_LEN+1];

  g95_interface *operator;
  struct g95_namespace *ns;
  g95_access access;
} g95_user_op;


/* Symbol nodes.  These are important things.  They are what the
 * standard refers to as "entities".  The possibly multiple names that
 * refer to the same entity are accomplished by a binary tree of
 * symtree structures that is balanced by the red-black method-- more
 * than one symtree node can point to any given symbol. */

typedef struct g95_symbol {
  char name[G95_MAX_SYMBOL_LEN+1],    /* Primary name, before renaming */
       module[G95_MAX_SYMBOL_LEN+1];  /* Module this symbol came from */
  g95_locus declared_at;

  g95_typespec ts;
  symbol_attribute attr;

/* the interface member points to the formal argument list if the
 * symbol is a function or subroutine name.  If the symbol is a
 * generic name, the generic member points to the list of interfaces. */

  g95_interface *generic;
  g95_access component_access;

  g95_formal_arglist *formal;
  struct g95_namespace *formal_ns;

  struct g95_expr *value;           /* Parameter/Initializer value */
  g95_array_spec *as;
  struct g95_symbol *result;        /* function result symbol */
  g95_component *components;        /* Derived type components */

  struct g95_symbol *common_next;   /* Links for COMMON syms */

  g95_namelist *namelist, *namelist_tail;

/* Change management fields.  Symbols that might be modified by the
 * current statement have the mark member nonzero and are kept in a
 * singly linked list through the tlink field.  Of these symbols,
 * symbols with old_symbol equal to NULL are symbols created within
 * the current statement.  Otherwise, old_symbol points to a copy of
 * the old symbol. */

  struct g95_symbol *old_symbol, *tlink;
  unsigned mark:1, new:1;
  int refs;
  struct g95_namespace *ns;    /* namespace containing this symbol */

  tree backend_decl;

} g95_symbol;


typedef struct {
  g95_locus where;
  int use_assoc, saved;
  g95_symbol *head;
} g95_common_head;

#define g95_get_common_head() g95_getmem(sizeof(g95_common_head))


/* Within a namespace, symbols are pointed to by symtree nodes that
 * are linked together in a balanced binary tree.  There can be
 * several symtrees pointing to the same symbol node via USE
 * statements. */

#define BBT_HEADER(self) int priority; struct self *left, *right;

typedef struct g95_symtree {
  BBT_HEADER(g95_symtree)

  char name[G95_MAX_SYMBOL_LEN+1];
  int ambiguous;
  union {
    g95_symbol *sym;             /* Symbol associated with this node */
    g95_user_op *uop;
    g95_common_head *common;
  } n;

  struct g95_symtree *link;

} g95_symtree;


typedef struct g95_namespace {
  g95_symtree *sym_root, *uop_root, *common_root;   /* Roots of symbol trees */

  int set_flag[G95_LETTERS];
  g95_typespec default_type[G95_LETTERS];    /* IMPLICIT typespecs */

  struct g95_symbol *proc_name;
  g95_interface *operator[G95_INTRINSIC_OPS];
  struct g95_namespace *parent, *contained, *sibling;
  struct g95_code *code;
  g95_common_head blank_common;
  struct g95_equiv *equiv;
  g95_access default_access, operator_access[G95_INTRINSIC_OPS];

  g95_st_label *st_labels;
  struct g95_data *data;

  g95_charlen *cl_list;
  g95_compile_state state;
  int save_all, seen_save, interface;

  tree backend_decl;
} g95_namespace;

extern g95_namespace *g95_current_ns;


/* Information on interfaces being built */

typedef struct {
  interface_type type;
  g95_symbol *sym;
  g95_namespace *ns;
  g95_user_op *uop;
  int op;
} g95_interface_info;

extern g95_interface_info current_interface;


/* Stack element for the current compilation state.  These structures
 * are allocated as automatic variables.  */

typedef struct g95_state_data {
  g95_compile_state state;
  g95_symbol *sym;            /* Block name associated with this level */
  g95_symbol *do_variable;    /* For DO blocks, the iterator variable. */

  struct g95_code *top, *head, **next;
  struct g95_state_data *previous;

/* Block-specific state data. */

  union {
    g95_st_label *end_do_label; 
  } ext;
} g95_state_data;

extern g95_state_data *g95_state_stack;

#define g95_current_block() (g95_state_stack->sym)
#define g95_current_state() (g95_state_stack->state)


/* Array reference */

typedef struct g95_array_ref {
  ar_type type;
  int dimen;                  /* # of components in the reference */

  g95_locus c_where[G95_MAX_DIMENSIONS];     /* All expressions can be NULL */
  struct g95_expr *start[G95_MAX_DIMENSIONS], *end[G95_MAX_DIMENSIONS],
                  *stride[G95_MAX_DIMENSIONS];

  enum { DIMEN_ELEMENT=1, DIMEN_RANGE, DIMEN_VECTOR, DIMEN_UNKNOWN }
    dimen_type[G95_MAX_DIMENSIONS];

} g95_array_ref;

#define g95_get_array_ref() g95_getmem(sizeof(g95_array_ref))


/* Component reference nodes.  A variable is stored as an expression
 * node that points to the base symbol.  After that, a singly linked
 * list of component reference nodes gives the variable's complete
 * resolution.  The array_ref component may be present and comes
 * before the component component.  */

typedef struct g95_ref {
  enum { REF_ARRAY, REF_COMPONENT, REF_SUBSTRING } type;

  union {
    struct g95_array_ref ar;

    struct {
      char name[G95_MAX_SYMBOL_LEN+1];
      g95_symbol *sym;
      g95_component *component;
    } c;

    struct {
      struct g95_expr *start, *end;       /* Substring */
      g95_charlen *length;
    } ss;

  } u;

  g95_locus where;
  struct g95_ref *next;
} g95_ref;

#define g95_get_ref() g95_getmem(sizeof(g95_ref))

/* Structures representing intrinsic symbols and their arguments lists */

typedef struct g95_intrinsic_arg {
  char name[G95_MAX_SYMBOL_LEN+1];

  g95_typespec ts;
  int optional;
  g95_actual_arglist *actual;

  struct g95_intrinsic_arg *next;

} g95_intrinsic_arg;


typedef struct g95_intrinsic_sym {
  char name[G95_MAX_SYMBOL_LEN+1], lib_name[G95_MAX_SYMBOL_LEN+1];
  g95_intrinsic_arg *formal;
  g95_typespec ts;
  int elemental, pure, generic, specific, actual_ok;

  struct g95_expr *(*simplify)();
  try (*check)();
  void (*resolve)();
  struct g95_intrinsic_sym *specific_head, *next;
  int generic_id;

} g95_intrinsic_sym;


/* Expression nodes.  The expression node types deserve explanations, since
 * the last couple can be easily misconstrued:
 *
 * EXPR_OP         Operator node pointing to one or two other nodes
 * EXPR_FUNCTION   Function call, symbol points to function's name
 * EXPR_CONSTANT   A scalar constant: Logical, String, Real, Int or Complex
 * EXPR_VARIABLE   An Lvalue with a root symbol and possible reference list
 *                 which expresses structure, array and substring refs.
 * EXPR_NULL       The NULL pointer value (which also has a basic type).
 * EXPR_SUBSTRING  A substring of a constant string
 * EXPR_STRUCTURE  A structure constructor
 * EXPR_ARRAY      An array constructor
 */

#include <gmp.h>

typedef struct g95_expr {
  expr_t type;
  g95_typespec ts;         /* These two refer to the overall expression */

  int rank;
  mpz_t *shape;    /* Can be NULL if shape is unknown at compile time */

  g95_intrinsic_op operator;
  g95_symbol *symbol;   /* Nonnull for functions and structure constructors */
  g95_user_op *uop;
  g95_ref *ref;

  struct g95_expr *op1, *op2;
  g95_locus where;

  union {
    mpz_t integer;
    mpf_t real;
    int logical;

    struct {
      mpf_t r, i;
    } complex;

    struct {
      g95_actual_arglist *actual;
      char *name;   /* Points to the ultimate name of the function */
      g95_intrinsic_sym *isym;
    } function;

    struct {
      int length;
      char *string;
    } character;

    struct g95_constructor *constructor;
  } value;

} g95_expr;


#define g95_get_shape(rank) ((mpz_t *) g95_getmem(rank*sizeof(mpz_t)))

/* Structures for information associated with different kinds of
 * numbers.  The first set of integer parameters define all there is
 * to know about a particular kind.  The rest of the elements are
 * computed from the first elements.  */

typedef struct {
  int kind, radix, digits, bit_size;

  int range;
  mpz_t huge;

  mpz_t min_int, max_int;  /* Values really representable by the target */
} g95_integer_info;

extern g95_integer_info g95_integer_kinds[];

typedef struct {
  int kind, bit_size;

} g95_logical_info;

extern g95_logical_info g95_logical_kinds[];

typedef struct {
  int kind, radix, digits, min_exponent, max_exponent;

  int range, precision;
  mpf_t epsilon, huge, tiny;
} g95_real_info;

extern g95_real_info g95_real_kinds[];

/* Equivalence structures.  Equivalent lvalues are linked along the
 * *eq pointer, equivalence sets are strung along the *next node.  */

typedef struct g95_equiv {
  struct g95_equiv *next, *eq;
  g95_expr *expr;
  int used;
} g95_equiv;

#define g95_get_equiv() g95_getmem(sizeof(g95_equiv))

/* g95_case stores the selector list of a case statement.  The *low
 * and *high pointers can point to the same expression in the case of
 * a single value.  If *high is NULL, the selection is from *low
 * upwards, if *low is NULL the selection is *high downwards.  */

typedef struct g95_case {
  g95_expr *low, *high;

  g95_locus where;
  int n;

  struct g95_case *next, *cprev, *cnext;
  struct g95_code *code; /* Code block for this case */
} g95_case;

#define g95_get_case() g95_getmem(sizeof(g95_case))


typedef struct {
  g95_expr *var, *start, *end, *step;
} g95_iterator;

#define g95_get_iterator() g95_getmem(sizeof(g95_iterator))


/* Allocation structure for ALLOCATE, DEALLOCATE and NULLIFY statements. */

typedef struct g95_alloc {
  g95_expr *expr;
  struct g95_alloc *next;
} g95_alloc;

#define g95_get_alloc() g95_getmem(sizeof(g95_alloc))


typedef struct {
  g95_expr *unit, *file, *status, *access, *form, *recl,
           *blank, *position, *action, *delim, *pad, *iostat;
  g95_st_label *err;
} g95_open;


typedef struct {
  g95_expr *unit, *status, *iostat;
  g95_st_label *err;
} g95_close;


typedef struct {
  g95_expr *unit, *iostat;
  g95_st_label *err;
} g95_filepos;


typedef struct {
  g95_expr *unit, *file, *iostat, *exist, *opened, *number, *named,
    *name, *access, *sequential, *direct, *form, *formatted,
    *unformatted, *recl, *nextrec, *blank, *position, *action, *read,
    *write, *readwrite, *delim, *pad, *iolength;

  g95_st_label *err;

} g95_inquire;


typedef struct {
  g95_expr *io_unit, *format_expr, *rec, *advance, *iostat, *size;

  g95_symbol *namelist;
/* A format_label of `g95_format_asterisk' indicates the "*" format */
  g95_st_label *format_label;
  g95_st_label *err, *end, *eor;

  g95_locus eor_where, end_where;
} g95_dt;


typedef struct g95_forall_iterator {
  g95_expr *var, *start, *end, *stride;
  g95_symbol *save;
  struct g95_forall_iterator *next;
} g95_forall_iterator;


/* Executable statements that fill g95_code structures */

typedef enum {
  EXEC_NOP=1, EXEC_ASSIGN, EXEC_POINTER_ASSIGN, EXEC_GOTO, EXEC_CALL,
  EXEC_RETURN, EXEC_STOP, EXEC_PAUSE, EXEC_CONTINUE, EXEC_ENTRY,
  EXEC_IF, EXEC_ARITHMETIC_IF, EXEC_DO, EXEC_DO_WHILE, EXEC_SELECT,
  EXEC_FORALL, EXEC_WHERE, EXEC_CYCLE, EXEC_EXIT,
  EXEC_ALLOCATE, EXEC_DEALLOCATE,
  EXEC_OPEN, EXEC_CLOSE,
  EXEC_READ, EXEC_WRITE, EXEC_IOLENGTH, EXEC_TRANSFER, EXEC_DT_END,
  EXEC_BACKSPACE, EXEC_ENDFILE, EXEC_INQUIRE, EXEC_REWIND
} g95_exec_op;


typedef struct g95_code {
  g95_exec_op type;

  struct g95_code *block, *next;
  g95_locus where;

  g95_st_label *here, *label, *label2, *label3;
  g95_symbol *sym;
  g95_expr *expr, *expr2;

  g95_intrinsic_sym *isym;   /* Subroutine references */
  char *sub_name;

  union {
    g95_actual_arglist *actual;
    g95_case *case_list;
    g95_iterator *iterator;
    g95_alloc *alloc_list;
    g95_open *open;
    g95_close *close;
    g95_filepos *filepos;
    g95_inquire *inquire;
    g95_dt *dt;
    g95_forall_iterator *forall_iterator;
    struct g95_code *block;
    int stop_code, end_code, reading;
  } ext;     /* Points to additional structures required by statement */

  /* Backend_decl is used for cycle and break labels in do loops, and
   * probably for other constructs as well, once we translate them.  */
  tree backend_decl;
} g95_code;


/* Storage for DATA statements */

typedef struct g95_data_variable {
  g95_expr *expr;
  g95_iterator iter;
  struct g95_data_variable *list, *next;
} g95_data_variable;


typedef struct g95_data_value {
  int repeat;
  g95_expr *expr;

  struct g95_data_value *next;
} g95_data_value;


typedef struct g95_data {
  g95_data_variable *var;
  g95_data_value *value;
  g95_locus where;

  struct g95_data *next;
} g95_data;

#define g95_get_data_variable() g95_getmem(sizeof(g95_data_variable))
#define g95_get_data_value() g95_getmem(sizeof(g95_data_value))
#define g95_get_data() g95_getmem(sizeof(g95_data))


/* Structure for holding module and include file search path */

typedef struct g95_directorylist {
  char *path;
  struct g95_directorylist *next;
} g95_directorylist;


typedef struct g95_nowarn {
  int warning;
  struct g95_nowarn *next;
} g95_nowarn;


/* Structure for holding compile options */

typedef struct {
  char *source;
  int verbose, pedantic, aliasing, unused_label, line_truncation,
    implicit_none, fixed_line_length, module_access_private, fmode, dollar,
    q_kind, quiet, r8, i8, d8, l1, pack_derived, max_frame_size, bounds_check;

  g95_directorylist *include_dirs;
  char *module_dir;
  g95_source_form form;
  g95_nowarn *nowarn;
} g95_option_t;

extern g95_option_t g95_option;


/* Constructor nodes for array and structure constructors. */

typedef struct g95_constructor {
  g95_expr *expr;
  g95_iterator *iterator;
  g95_locus where;
  struct g95_constructor *next;
} g95_constructor;

#define g95_get_constructor() g95_getmem(sizeof(g95_constructor))


typedef struct g95_gsymbol {
  BBT_HEADER(g95_gsymbol)

  char name[G95_MAX_SYMBOL_LEN+1];
  enum { GSYM_UNKNOWN=1, GSYM_PROGRAM, GSYM_FUNCTION, GSYM_SUBROUTINE,
	 GSYM_MODULE, GSYM_COMMON, GSYM_BLOCK_DATA } type;

  int size, defined, used;
  g95_locus where;
  tree backend_decl;
} g95_gsymbol;


/************************ Function prototypes *************************/

/* scanner.c */

void g95_scanner_done_1(void);
void g95_scanner_init_1(void);

int g95_at_end(void);
int g95_at_eof(void);
int g95_at_bol(void);
int g95_at_eol(void);
void g95_advance_line(void);

void g95_skip_comment_line(void);
void g95_skip_comments(void);
int g95_next_char_literal(int);
int g95_next_char(void);
int g95_peek_char(void);
void g95_error_recovery(void);
void g95_gobble_whitespace(void);
try g95_new_file(char *, g95_source_form);

extern g95_file *g95_current_file;
extern g95_source_form g95_current_form;
extern char *g95_source_file;
extern g95_locus g95_current_locus;

/* misc.c */

void *g95_getmem(size_t);
void g95_free(void *);
void g95_clear_ts(g95_typespec *);
FILE *g95_open_file(char *);
FILE *g95_open_included_file(char *);
char *g95_article(char *);
char *g95_basic_typename(bt);
char *g95_typename(g95_typespec *);
void g95_show_typespec(g95_typespec *);

char *g95_code2string(mstring *, int);
int g95_string2code(mstring *, char *);
char *g95_intent_string(sym_intent);

void g95_init_1(void);
void g95_init_2(void);
void g95_done_1(void);
void g95_done_2(void);

/* options.c */

void g95_display_help(void);
int g95_parse_arg(int argc, char *argv[]);
void g95_init_options(void);
void g95_options_done(void);

/* iresolve.c */

char *g95_get_string(char *, ...)
#ifdef __GNUC__
__attribute__ ((format (printf, 1, 2)))
#endif
;

void g95_iresolve_init_1(void);
void g95_iresolve_done_1(void);

/* error.c */

void g95_error_init_1(void);
void g95_buffer_error(int);

void g95_warning(int, char *, ...);
void g95_warning_now(int, char *, ...);
void g95_clear_warning(void);
void g95_warning_check(void);

void g95_error(char *, ...);
void g95_error_now(char *, ...);
void g95_fatal_error(char *, ...)
#ifdef __GNUC__
 __attribute__ ((noreturn))
#endif
;

void g95_internal_error(char *, ...)
#ifdef __GNUC__
 __attribute__ ((noreturn))
#endif
;

void g95_clear_error(void);
int g95_error_check(void);
void g95_syntax_error(g95_statement st);
void g95_push_error(g95_error_buf *);
void g95_pop_error(g95_error_buf *);

void g95_status(char *, ...)
#ifdef __GNUC__
__attribute__ ((format (printf, 1, 2)))
#endif
;
void g95_status_char(char);
try g95_open_status(char *);
try g95_close_status(void);

void g95_get_errors(int *, int *);

/* parse.c */

try g95_find_state(g95_compile_state);
g95_state_data *g95_enclosing_unit(g95_compile_state *);
char *g95_ascii_statement(g95_statement);
char *g95_state_name(g95_compile_state);
void g95_reject_statement(void);
void g95_check_do_variable(g95_symbol *);
try g95_parse_file(void);

extern g95_st_label * g95_statement_label;

/* arith.c */

extern mpf_t pi, half_pi, minus_half_hpi, two_pi;

char *g95_arith_error(arith);
void g95_arith_init_1(void);
void g95_arith_done_1(void);

void natural_logarithm(mpf_t *, mpf_t *);
void common_logarithm(mpf_t *, mpf_t *);
void exponential(mpf_t *, mpf_t *);
void sine(mpf_t *, mpf_t *);
void cosine(mpf_t *, mpf_t *);
void arctangent(mpf_t *, mpf_t *);
void hypercos(mpf_t *, mpf_t *);
void hypersine(mpf_t *, mpf_t *);

int g95_default_integer_kind(void);
int g95_default_real_kind(void);
int g95_default_double_kind(void);
int g95_default_character_kind(void);
int g95_default_logical_kind(void);
int g95_default_complex_kind(void);

g95_expr *g95_constant_result(bt, int, g95_locus *);
int g95_validate_kind(bt, int);
arith g95_range_check(g95_expr *);

int g95_zero_size_array(g95_expr *);

arith g95_arith_uminus(g95_expr *, g95_expr **);
arith g95_arith_plus(g95_expr *, g95_expr *, g95_expr **);
arith g95_arith_minus(g95_expr *, g95_expr *, g95_expr **);
arith g95_arith_times(g95_expr *, g95_expr *, g95_expr **);
arith g95_arith_divide(g95_expr *, g95_expr *, g95_expr **);
arith g95_arith_power(g95_expr *, g95_expr *, g95_expr **);
arith g95_arith_concat(g95_expr *, g95_expr *, g95_expr **);
arith g95_arith_not(g95_expr *, g95_expr **);
arith g95_arith_and(g95_expr *, g95_expr *, g95_expr **);
arith g95_arith_or(g95_expr *, g95_expr *, g95_expr **);
arith g95_arith_eqv(g95_expr *, g95_expr *, g95_expr **);
arith g95_arith_neqv(g95_expr *, g95_expr *, g95_expr **);
arith g95_arith_uplus(g95_expr *, g95_expr **);

int g95_compare_expr(g95_expr *, g95_expr *);
int g95_compare_string(g95_expr *, g95_expr *, int *);
arith g95_arith_eq(g95_expr *, g95_expr *, g95_expr **);
arith g95_arith_ne(g95_expr *, g95_expr *, g95_expr **);
arith g95_arith_gt(g95_expr *, g95_expr *, g95_expr **);
arith g95_arith_ge(g95_expr *, g95_expr *, g95_expr **);
arith g95_arith_lt(g95_expr *, g95_expr *, g95_expr **);
arith g95_arith_le(g95_expr *, g95_expr *, g95_expr **);

g95_expr *g95_uplus(g95_expr *op);
g95_expr *g95_uminus(g95_expr *op);
g95_expr *g95_add(g95_expr *, g95_expr *);
g95_expr *g95_subtract(g95_expr *, g95_expr *);
g95_expr *g95_multiply(g95_expr *, g95_expr *);
g95_expr *g95_divide(g95_expr *, g95_expr *);
g95_expr *g95_power(g95_expr *, g95_expr *);
g95_expr *g95_concat(g95_expr *, g95_expr *);
g95_expr *g95_and(g95_expr *, g95_expr *);
g95_expr *g95_or(g95_expr *, g95_expr *);
g95_expr *g95_not(g95_expr *);
g95_expr *g95_eqv(g95_expr *, g95_expr *);
g95_expr *g95_neqv(g95_expr *, g95_expr *);
g95_expr *g95_eq(g95_expr *, g95_expr *);
g95_expr *g95_ne(g95_expr *, g95_expr *);
g95_expr *g95_gt(g95_expr *, g95_expr *);
g95_expr *g95_ge(g95_expr *, g95_expr *);
g95_expr *g95_lt(g95_expr *, g95_expr *);
g95_expr *g95_le(g95_expr *, g95_expr *);

g95_expr *g95_convert_integer(char *, int, int, g95_locus *);
g95_expr *g95_convert_real(char *, int, g95_locus *);
g95_expr *g95_convert_complex(g95_expr *, g95_expr *, int);

g95_expr *g95_int2int(g95_expr *, int);
g95_expr *g95_int2real(g95_expr *, int);
g95_expr *g95_int2complex(g95_expr *, int);
g95_expr *g95_real2int(g95_expr *, int);
g95_expr *g95_real2real(g95_expr *, int);
g95_expr *g95_real2complex(g95_expr *, int);
g95_expr *g95_complex2int(g95_expr *, int);
g95_expr *g95_complex2real(g95_expr *, int);
g95_expr *g95_complex2complex(g95_expr *, int);
g95_expr *g95_log2log(g95_expr *, int);


/* symbol.c */
extern g95_gsymbol *g95_gsym_root;

match g95_match_implicit_none(void);
void g95_set_implicit_none(void);
match g95_match_implicit(void);
void g95_set_implicit(void);
g95_typespec *g95_get_default_type(g95_symbol *, g95_namespace *);
try g95_set_default_type(g95_symbol *, int, g95_namespace *);
try g95_check_conformance(const char *, g95_expr *, g95_expr *);
try g95_check_assign(g95_expr *, g95_expr *, int);
try g95_check_pointer_assign(g95_expr *, g95_expr *);
try g95_check_assign_symbol(g95_symbol *, g95_expr *);
void g95_show_attr(symbol_attribute *);
void g95_set_component_attr(g95_component *, symbol_attribute *);
void g95_get_component_attr(symbol_attribute *, g95_component *);

try g95_add_allocatable(symbol_attribute *, g95_locus *);
try g95_add_dimension(symbol_attribute *, g95_locus *);
try g95_add_external(symbol_attribute *, g95_locus *);
try g95_add_intrinsic(symbol_attribute *, g95_locus *);
try g95_add_optional(symbol_attribute *, g95_locus *);
try g95_add_pointer(symbol_attribute *, g95_locus *);
try g95_add_result(symbol_attribute *, g95_locus *);
try g95_add_save(symbol_attribute *, g95_locus *);
try g95_add_target(symbol_attribute *, g95_locus *);
try g95_add_dummy(symbol_attribute *, g95_locus *);
try g95_add_generic(symbol_attribute *, g95_locus *);
try g95_add_in_common(symbol_attribute *, g95_locus *);
try g95_add_in_namelist(symbol_attribute *, g95_locus *);
try g95_add_sequence(symbol_attribute *, g95_locus *);
try g95_add_elemental(symbol_attribute *, g95_locus *);
try g95_add_pure(symbol_attribute *, g95_locus *);
try g95_add_recursive(symbol_attribute *, g95_locus *);
try g95_add_function(symbol_attribute *, g95_locus *);
try g95_add_subroutine(symbol_attribute *, g95_locus *);

try g95_add_access(symbol_attribute *, g95_access, g95_locus *);
try g95_add_flavor(symbol_attribute *, sym_flavor, g95_locus *);
try g95_add_entry(symbol_attribute *, g95_locus *);
try g95_add_procedure(symbol_attribute *, procedure_type, g95_locus *);
try g95_add_intent(symbol_attribute *, sym_intent, g95_locus *);
try g95_add_explicit_interface(g95_symbol *, ifsrc, g95_formal_arglist *,
			       g95_locus *);
try g95_add_type(g95_symbol *, g95_typespec *, g95_locus *);

int g95_compare_attr(symbol_attribute *, symbol_attribute *);
void g95_clear_attr(symbol_attribute *);
try g95_copy_attr(symbol_attribute *, symbol_attribute *, g95_locus *);

try g95_add_component(g95_symbol *, char *, g95_component **);
g95_symbol *g95_use_derived(g95_symbol *);
void g95_show_components(g95_symbol *);

g95_st_label *g95_get_st_label(int);
void g95_free_st_label(g95_st_label *);
g95_st_label *g95_new_internal_label(void);
void g95_define_st_label(g95_st_label *, g95_sl_type, g95_locus *);
try g95_reference_st_label(g95_st_label *, g95_sl_type);

g95_namespace *g95_get_namespace(g95_namespace *, int);
int g95_compare_symtree(g95_symtree *, g95_symtree *);
g95_symtree *g95_new_symtree(g95_symtree **, char *);
g95_symtree *g95_find_symtree(g95_symtree *, char *);
g95_user_op *g95_get_uop(char *);
g95_user_op *g95_find_uop(char *, g95_namespace *);
void g95_free_symbol(g95_symbol *);
g95_symbol *g95_new_symbol(char *, g95_namespace *);
int g95_find_symbol(char *, g95_namespace *, int, g95_symbol **);
int g95_get_symbol(char *, g95_namespace *, g95_symbol **);
int g95_get_ha_symbol(char *, g95_symbol **);

void g95_save_symbol_data(g95_symbol *);
void g95_undo_symbols(void);
void g95_commit_symbols(void);
void g95_free_namespace(g95_namespace *);

void g95_symbol_init_2(void);
void g95_show_symbol(g95_symbol *);

void g95_traverse_symtree(g95_namespace *, void (*)(g95_symtree *));
void g95_traverse_ns(g95_namespace *, void (*)(g95_symbol *));
void g95_traverse_user_op(g95_namespace *, void (*)(g95_user_op *));
void g95_save_all(g95_namespace *);
int g95_module_symbol(g95_symbol *);
void g95_show_namespace(g95_namespace *);
void g95_symbol_state(void);
g95_gsymbol *g95_get_gsymbol(char *);
g95_gsymbol *g95_find_gsymbol(g95_gsymbol *, char *);
void g95_global_used(g95_gsymbol *, g95_locus *);

/* intrinsic.c */

extern int g95_init_expr;

void g95_intrinsic_init_1(void);
void g95_intrinsic_done_1(void);
int g95_intrinsic_symbol(g95_symbol *sym, int);

char g95_type_letter(bt);
try g95_convert_type(g95_expr *, g95_typespec *, int);
int g95_generic_intrinsic(char *);
int g95_specific_intrinsic(char *);
int g95_intrinsic_name(char *, int);
g95_intrinsic_sym *g95_find_function(char *);
int g95_has_alt_return(g95_actual_arglist *);
match g95_intrinsic_func_interface(g95_expr *, int);
match g95_intrinsic_sub_interface(g95_code *, int);

/* simplify.c */

void g95_simplify_init_1(void);
void g95_simplify_done_1(void);

/* match.c */
/* Generic match subroutines */

match g95_match_space(void);
match g95_match_eos(void);
match g95_match_small_literal_int(int *);
match g95_match_st_label(g95_st_label **, int);
match g95_match_label(void);
match g95_match_small_int(int *);
int g95_match_strings(mstring *);
match g95_match_name(char *);
match g95_match_symbol(g95_symbol **, int);
match g95_match_intrinsic_op(g95_intrinsic_op *);
char *g95_op2string(int);
match g95_match_char(char);
match g95_match(char *, ...);
match g95_match_iterator(g95_iterator *, int);
void g95_free_iterator(g95_iterator *, int);
void g95_free_forall_iterator(g95_forall_iterator *);

/* Statement matchers */

match g95_match_program(void);
match g95_match_pointer_assignment(void);
match g95_match_assignment(void);
match g95_match_if(g95_statement *);
match g95_match_else(void);
match g95_match_elseif(void);
match g95_match_do(void);
match g95_match_cycle(void);
match g95_match_exit(void);
match g95_match_pause(void);
match g95_match_stop(void);
match g95_match_continue(void);
match g95_match_assign(void);
match g95_match_goto(void);

void g95_free_alloc_list(g95_alloc *);
match g95_match_allocate(void);
match g95_match_nullify(void);
match g95_match_deallocate(void);
match g95_match_return(void);
match g95_match_call(void);
g95_common_head *g95_get_common(char *);
match g95_match_common(void);
match g95_match_block_data(void);
void g95_free_namelist(g95_namelist *);
match g95_match_namelist(void);
match g95_match_module(void);
void g95_free_equiv(g95_equiv *);
match g95_match_equivalence(void);
match g95_match_st_function(void);
void g95_free_data(g95_data *);
match g95_match_data(void);
match g95_match_where(g95_statement *);
match g95_match_elsewhere(void);
match g95_match_forall(g95_statement *);

/* decl.c */

extern g95_symbol *g95_new_block;

match g95_match_null(g95_expr **);
match g95_match_kind_spec(g95_typespec *);
match g95_match_old_kind_spec(g95_typespec *);
match g95_match_type_spec(g95_typespec *, int);

match g95_match_end(g95_statement *);
match g95_match_data_decl(void);
match g95_match_formal_arglist(g95_symbol *, int, int);
match g95_match_function_decl(void);
match g95_match_entry(void);
match g95_match_subroutine(void);
match g95_match_derived_decl(void);

/* Matchers for attribute declarations */

match g95_match_allocatable(void);
match g95_match_dimension(void);
match g95_match_external(void);
match g95_match_intent(void);
match g95_match_intrinsic(void);
match g95_match_optional(void);
match g95_match_parameter(void);
match g95_match_pointer(void);
match g95_match_private(g95_statement *);
match g95_match_public(g95_statement *);
match g95_match_save(void);
match g95_match_modproc(void);
match g95_match_target(void);

/* primary.c */

match g95_match_actual_arglist(int, g95_actual_arglist **);
int g95_next_string_char(char);
match g95_match_literal_constant(g95_expr **, int);
symbol_attribute g95_variable_attr(g95_expr *, g95_typespec *);
symbol_attribute g95_expr_attr(g95_expr *);
int g95_local_symbol(g95_symbol *);
match g95_match_structure_constructor(g95_symbol *, g95_expr **);
g95_ref *g95_extend_ref(g95_expr *, int);
match g95_match_rvalue(g95_expr **);
match g95_match_variable(g95_expr **, int);

/* expr.c */

void g95_show_actual_arglist(g95_actual_arglist *);
void g95_free_actual_arglist(g95_actual_arglist *);
g95_actual_arglist *g95_copy_actual_arglist(g95_actual_arglist *);
char *g95_extract_int(g95_expr *, int *);
g95_expr *g95_null_expr(g95_locus *);
g95_symbol *g95_get_temporary(g95_typespec *ts, int);
g95_symbol *g95_get_temporary_int(void);
g95_expr *g95_get_variable_expr(g95_symbol *sym);
g95_expr *g95_build_funcall(g95_symbol *func, ...);
void g95_free_ref_list(g95_ref *);
void g95_type_convert_binary(g95_expr *);
int g95_is_constant_expr(g95_expr *);
try g95_check_parameter(g95_symbol *);
try g95_simplify_expr(g95_expr *, int);

g95_expr *g95_get_expr(void);
void g95_free_expr(g95_expr *);
void g95_replace_expr(g95_expr *, g95_expr *);
g95_expr *g95_int_expr(int);
g95_expr *g95_logical_expr(int, g95_locus *);
g95_expr *g95_char_expr(int, int, g95_locus *);
g95_formal_arglist *g95_copy_formal_arglist(g95_formal_arglist *);
g95_ref *g95_full_ref(void);
mpz_t *g95_copy_shape(mpz_t *, int);
g95_expr *g95_copy_expr(g95_expr *);

match g95_match_init_expr(g95_expr **);
try g95_specification_expr(g95_expr *);
void g95_show_expr(g95_expr *);

int g95_numeric_ts(g95_typespec *);
int g95_kind_max(g95_expr *, g95_expr *);

/* st.c */

extern g95_code new_st;

void g95_clear_new_st(void);
g95_code *g95_get_code(void);
g95_code *g95_add_statement(void);
g95_code *g95_append_code(g95_code *, g95_code *);
void g95_free_statements(g95_code *);
void g95_undo_statement(void);
void g95_show_code(int, g95_code *);

/* resolve.c */

try g95_resolve_expr(g95_expr *);
int g95_impure_variable(g95_symbol *);
int g95_pure(g95_symbol *);
int g95_elemental(g95_symbol *);
try g95_resolve_iterator(g95_iterator *);
g95_symbol *g95_find_entries(g95_symtree *, int);
void g95_process_entry(g95_namespace *);
try g95_resolve(g95_namespace *);

/* array.c */

void g95_free_array_spec(g95_array_spec *);
void g95_show_array_spec(g95_array_spec *);
void g95_free_array_ref(g95_array_ref *);
void g95_show_array_ref(g95_array_ref *);
g95_array_ref *g95_copy_array_ref(g95_array_ref *);

try g95_set_array_spec(g95_symbol *, g95_array_spec *, g95_locus *);
g95_array_spec *g95_copy_array_spec(g95_array_spec *);
try g95_resolve_array_spec(g95_array_spec *, int);
match g95_match_array_spec(g95_array_spec **);
match g95_match_array_ref(g95_array_ref *, int);
int g95_compare_array_spec(g95_array_spec *, g95_array_spec *);

g95_expr *g95_start_constructor(bt, int, g95_locus *);
void g95_append_constructor(g95_expr *, g95_expr *);
void g95_free_constructor(g95_constructor *);
match g95_match_array_constructor(g95_expr **);
try g95_simplify_iterator_var(g95_expr *);
try g95_expand_iterator(g95_iterator *, try (*)(void *), void *);
try g95_expand_constructor(g95_expr *);
try g95_expand_data_constructor(g95_expr *);
int g95_constant_ac(g95_expr *);
int g95_expanded_ac(g95_expr *);
try g95_resolve_array_constructor(g95_expr *);
try g95_check_constructor_type(g95_expr *);
try g95_check_iter_variable(g95_expr *);
try g95_check_constructor(g95_expr *, try (*)(g95_expr *));
g95_constructor *g95_copy_constructor(g95_constructor *src);
g95_expr *g95_get_array_element(g95_expr *, int);
try g95_array_size(g95_expr *, mpz_t *);
try g95_array_spec_size(g95_array_spec *, mpz_t *);
try g95_array_dimen_size(g95_expr *, int, mpz_t *);
try g95_array_ref_shape(g95_array_ref *, g95_array_spec *, mpz_t *);
void g95_find_array_ref(g95_expr *, g95_array_ref **, g95_array_spec **);

/* interface.c */

void g95_free_interface(g95_interface *);
match g95_match_generic_spec(interface_type *, char *, int *);
match g95_match_interface(void);
match g95_match_end_interface(void);
int g95_compare_types(g95_typespec *, g95_typespec *);
void g95_check_interfaces(g95_namespace *);
void g95_procedure_use(g95_symbol *, g95_actual_arglist **, g95_locus *);
g95_symbol *g95_search_interface(g95_interface *, int, g95_actual_arglist **);
try g95_extend_expr(g95_expr *);
void g95_free_formal_arglist(g95_formal_arglist *);
try g95_extend_assign(g95_code *, g95_namespace *);
try g95_add_interface(g95_symbol *sym);

/* select.c */

void g95_free_case_list(g95_case *);
match g95_match_case(void);
match g95_match_select(void);
try g95_resolve_select(g95_code *);

/* io.c */

extern g95_st_label g95_format_asterisk;

void g95_free_open(g95_open *);
match g95_match_open(void);
try g95_resolve_open(g95_open *);

void g95_free_close(g95_close *);
match g95_match_close(void);
try g95_resolve_close(g95_close *);

void g95_free_filepos(g95_filepos *);
try g95_resolve_filepos(g95_filepos *);
match g95_match_endfile(void);
match g95_match_backspace(void);
match g95_match_rewind(void);

void g95_free_inquire(g95_inquire *);
match g95_match_inquire(void);
try g95_resolve_inquire(g95_inquire *);

void g95_free_dt(g95_dt *);
try g95_resolve_dt(g95_dt *);
match g95_match_read(void);
match g95_match_write(void);
match g95_match_print(void);
void g95_io_init(void);

/* format.c */

void g95_check_format_string(g95_expr *);
match g95_match_format(void);

/* matchexp.c */

match g95_match_defined_op_name(char *, int);
match g95_match_expr(g95_expr **);

/* module.c */

void g95_module_init_2(void);
void g95_module_done_2(void);
match g95_match_use(void);
void g95_dump_module(char *, int);
void g95_use_module(void);

/* trans.c */

void g95_generate_code(g95_namespace *);

/* bbt.c */

void g95_insert_bbt(void *, void *, int (*)());
void g95_delete_bbt(void *, void *, int (*)());

/* scalarize.c */

void g95_expand_where(g95_code **);
void g95_scalarize(g95_namespace *);

/* data.c */

try g95_expand_ac_element(g95_expr *);

/* assign.c */

void g95_expand_forall(g95_code *);
