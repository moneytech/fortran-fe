/* g95 header file
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

/* g95.h-- It's probably insane to have this large of a header file,
 * but it seemed like everything had to be recompiled anyway when a
 * change was made to a header file, and there were ordering issues
 * with multiple header files.  Besides, Microsoft's winnt.h was 250k
 * last time I looked, so by comparison this is perfectly
 * reasonable. */


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
#include "config.h"

/* Major control parameters */

#define G95_MAX_SYMBOL_LEN 31

#define G95_REAL_BITS 100  /* Number of bits in g95's floating point numbers */

#define G95_MAX_LINE 132        /* Characters beyond this are not seen */

#define G95_MAX_DIMENSIONS 7    /* Maximum dimensions in an array */

#define G95_LETTERS 26          /* Number of letters in the alphabet */

#define MAX_ERROR_MESSAGE 1000  /* Maximum length of an error message */


#define free(x) Use_g95_free_instead_of_free()
#define g95_is_whitespace(c) ((c==' ') || (c=='\t'))

#ifndef NULL
#define NULL ((void *) 0)
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

typedef enum { EXPR_OP=1, EXPR_FUNCTION, EXPR_CONSTANT, EXPR_VARIABLE,
	       EXPR_SUBSTRING, EXPR_STRUCTURE, EXPR_ARRAY, EXPR_NULL
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

typedef enum { AR_FULL=1, AR_ELEMENT, AR_SECTION } ar_type;

/* Statement label types */

typedef enum { ST_LABEL_UNKNOWN=1, ST_LABEL_TARGET,
               ST_LABEL_BAD_TARGET, ST_LABEL_FORMAT
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
               ARITH_DIV0, ARITH_0TO0
} arith;

/* Statements */

typedef enum { 
  ST_ARITHMETIC_IF, ST_ALLOCATE, ST_ATTR_DECL, ST_BACKSPACE, ST_BLOCK_DATA,
  ST_CALL, ST_CASE, ST_CLOSE, ST_COMMON, ST_CONTINUE, ST_CONTAINS, ST_CYCLE,
  ST_DATA, ST_DATA_DECL, ST_DEALLOCATE, ST_DO, ST_ELSE, ST_ELSEIF,
  ST_ELSEWHERE, ST_END, ST_END_BLOCK_DATA, ST_ENDDO, ST_IMPLIED_ENDDO,
  ST_END_FILE, ST_END_FORALL, ST_END_FUNCTION, ST_ENDIF, ST_END_INTERFACE,
  ST_END_MODULE, ST_END_PROGRAM, ST_END_SELECT, ST_END_SUBROUTINE,
  ST_END_WHERE, ST_END_TYPE, ST_ENTRY, ST_EQUIVALENCE, ST_EXIT, ST_FORALL,
  ST_FORALL_BLOCK, ST_FORMAT, ST_FUNCTION, ST_GOTO, ST_IF_BLOCK, ST_IMPLICIT,
  ST_IMPLICIT_NONE, ST_INQUIRE, ST_INTERFACE, ST_PARAMETER, ST_MODULE,
  ST_MODULE_PROC, ST_NAMELIST, ST_NULLIFY, ST_OPEN, ST_PRIVATE,
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


/************************* Structures *****************************/

/* Symbol attribute structure. */

typedef struct {

/* Variable attributes */
  unsigned allocatable:1, dimension:1, external:1,  intrinsic:1,
           optional:1,    pointer:1,   save:1,      target:1,
           dummy:1,       common:1,    result:1,    entry:1;

  unsigned data:1,        /* Symbol is named in a DATA statement */
           use_assoc:1,   /* Symbol has been use-associated */
           interface:1;   /* Symbol appears in an interface body */

  unsigned in_namelist:1, in_common:1, saved_common:1;
  unsigned function:1, subroutine:1, generic:1;
  unsigned implicit_type:1;    /* Type defined via implicit rules */

/* Function/subroutine attributes */

  unsigned sequence:1, elemental:1, pure:1, recursive:1;

/* Mutually exclusive multibit attributes */

  g95_access access:2;
  sym_intent intent:2;
  sym_flavor flavor:4;

  procedure_type proc:3;

} symbol_attribute;


typedef struct {
  char *nextc;
  int line;                /* line within the lp structure */
  struct linebuf *lp;
  struct g95_file *file;
} locus;

/* The linebuf structure deserves some explanation.  This is the
 * primary structure for holding lines.  A source file is stored in a
 * singly linked list of these structures.  Each structure holds an
 * integer number of lines.  The line[] member is actually an array of
 * pointers that point to the NUL-terminated lines.  This list grows
 * upwards, and the actual lines are stored at the top of the
 * structure and grow downward.  Each structure is packed with as many
 * lines as it can hold, then another linebuf is allocated.  */

#define LINEBUF_SIZE 4080
          /* Chosen so that sizeof(linebuf) = 4096 on most machines */

typedef struct linebuf {
  int start_line, lines;
  struct linebuf *next;
  char *line[1];
  char buf[LINEBUF_SIZE];
} linebuf;


#include <limits.h>
#ifndef PATH_MAX
# include <sys/param.h>
# define PATH_MAX MAXPATHLEN
#endif


typedef struct g95_file {
  char filename[PATH_MAX+1];
  g95_source_form form;
  struct g95_file *included_by, *next;
  locus loc; 
  struct linebuf *start;
} g95_file;

/* Structure for storing strings to be matched by g95_match_string */

typedef struct {
  const char *string, *mp;
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

/* Array shape */

typedef struct {
  int rank;
  struct g95_expr *shape[G95_MAX_DIMENSIONS];
} g95_array_shape;

#define g95_get_array_shape() g95_getmem(sizeof(g95_array_shape))

/* Components of derived types */

typedef struct g95_component {
  char name[G95_MAX_SYMBOL_LEN+1];
  g95_typespec ts;

  int pointer, dimension;
  g95_array_spec *as;

  locus loc;
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
  int arg_number;
  int label;    /* Alternate return label when the expr member is null */
  struct g95_expr *expr;
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


/* The g95_st_label structure is a singly linked list attached to a
 * namespace that records the usage of statement labels within that space */

typedef struct g95_st_label {
  int label;
  int block_no;
  
  g95_sl_type defined, referenced;

  char *format;
  int length;
  locus where;

  struct g95_st_label *next;
} g95_st_label;

#define g95_get_st_label() g95_getmem(sizeof(g95_st_label))


/* Stack of block numbers for validating GOTO statements */

typedef struct g95_block_stack {
  int block_no;
  struct g95_block_stack *prev;
} g95_block_stack;

#define g95_get_block_stack() g95_getmem(sizeof(g95_block_stack))


/* g95_interface()-- Interfaces are lists of symbols strung together */

typedef struct g95_interface {
  struct g95_symbol *sym;
  locus where;
  struct g95_interface *next;
} g95_interface;

#define g95_get_interface() g95_getmem(sizeof(g95_interface))


/* Symbol nodes.  These are important things.  They are what the
 * standard refers to as "entities".  The possibly multiple names that
 * refer to the same entity are accomplished by a binary tree of
 * symtree structures that is balenced by the red-black method-- more
 * than one symtree node can point to any given symbol. */

typedef struct g95_symbol {
  char name[G95_MAX_SYMBOL_LEN+1],    /* Primary name, before renaming */
       module[G95_MAX_SYMBOL_LEN+1];  /* Module this symbol came from */
  locus declared_at;

  g95_typespec ts;
  symbol_attribute attr;

/* the interface member points to the formal argument list if the
 * symbol is a function or subroutine name.  If the symbol is a
 * generic name, the generic member points to the list of interfaces. */

  g95_interface *operator, *generic;
  g95_access operator_access, component_access;

  g95_formal_arglist *formal;
  struct g95_namespace *formal_ns;

  struct g95_expr *value;           /* Parameter/Initializer value */
  g95_array_spec *as;
  struct g95_symbol *result;        /* function result symbol */
  g95_component *components;        /* Derived type components */

  struct g95_symbol *common_head, *common_next;  /* Links for COMMON syms */

  g95_namelist *namelist, *namelist_tail;

/* Change management fields.  Symbols that might be modified by the
 * current statement have the mark member nonzero and are kept in a
 * singly linked list through the tlink field.  Of these symbols,
 * symbols with old_symbol equal to NULL are symbols created within
 * the current statement.  Otherwise, old_symbol points to a copy of
 * the old symbol. */

  struct g95_symbol *old_symbol, *tlink;
  unsigned mark:1, written:1, new:1;
  int serial, refs;
  struct g95_namespace *ns;    /* namespace containing this symbol */

} g95_symbol;


/* Within a namespace, symbols are pointed to by symtree nodes that
 * are linked together in a Red-Black balenced binary tree.  The tree
 * information is not stored within a symbol structure because like
 * many other binary tree implementations, deleting a node can cause
 * other nodes to be moved around.  Since symbols can have lots of
 * things pointing to them, they can't be moved.  Because the only
 * pointers to red-black nodes are other red-black nodes, its OK if
 * they are moved.
 *
 * Besides the many-to-one relationship between symbol nodes and
 * symtree nodes, this is also why there is no pointer from symbol
 * nodes to symtree nodes.  If it turns out this is needed, the
 * delete_node() subroutine must be modified to update such a pointer
 * when nodes are moved.
 *
 * The "key" of the red-black structures points to the symbol node
 * which contain the symbol name, which is the actual key used to
 * balence the tree.  The red-black code is due to Thomas Niemann. */

typedef struct g95_symtree {
  char name[G95_MAX_SYMBOL_LEN+1];
  int ambiguous;
  g95_symbol *sym;             /* Symbol associated with this node */

  struct g95_symtree *left, *right, *parent;
  enum { BLACK, RED } color;   /* node color (BLACK, RED) */
} g95_symtree;

extern g95_symtree g95_st_sentinel;
#define NIL &g95_st_sentinel


typedef struct g95_namespace {
  g95_symtree *root;    /* Root of the red/black symbol tree */

  int set_flag[G95_LETTERS];
  g95_typespec default_type[G95_LETTERS];    /* IMPLICIT typespecs */

  struct g95_symbol *proc_name;
  g95_interface *operator[G95_INTRINSIC_OPS];
  struct g95_namespace *parent, *contained, *sibling;
  struct g95_code *code;
  g95_symbol *blank_common;
  struct g95_equiv *equiv;
  g95_access default_access, operator_access[G95_INTRINSIC_OPS];

  g95_st_label *st_labels;
  struct g95_data *data;

  g95_charlen *cl_list;

  int save_all, seen_save;
} g95_namespace;

extern g95_namespace *g95_current_ns;


/* Information on interfaces being built */

typedef struct {
  interface_type type;
  g95_symbol *sym;
  g95_namespace *ns;
  int op;
} g95_interface_info;

extern g95_interface_info current_interface;


/* Stack element for the current compilation state.  These structures
 * are allocated as automatic variables.  */

typedef struct g95_state_data {
  g95_compile_state state;
  g95_symbol *sym;            /* Block name associated with this level */
  int this_block_no;
  struct g95_code *head, *tail;
  struct g95_state_data *previous;
} g95_state_data;

extern g95_state_data *g95_state_stack;

#define g95_current_block() (g95_state_stack->sym)
#define g95_current_state() (g95_state_stack->state)


/* Array reference */

typedef struct g95_array_ref {
  ar_type type;
  int rank;
  locus where;
  g95_array_spec *as;

  locus c_where[G95_MAX_DIMENSIONS];     /* All expressions can be NULL */
  struct g95_expr *start[G95_MAX_DIMENSIONS], *end[G95_MAX_DIMENSIONS],
                  *stride[G95_MAX_DIMENSIONS];

  enum { DIMEN_ELEMENT=1, DIMEN_RANGE, DIMEN_VECTOR }
    dimen_type[G95_MAX_DIMENSIONS];

  struct g95_expr *offset;
} g95_array_ref;

#define g95_get_array_ref() g95_getmem(sizeof(g95_array_ref))


/* Component reference nodes.  A variable is stored as an expression
 * node that points to the base symbol.  After that, a singly linked
 * list of component reference nodes gives the variable's complete
 * resolution.  The array_ref component may be present and comes
 * before the component component.  */

typedef struct g95_ref {
  enum { REF_ARRAY, REF_COMPONENT, REF_SUBSTRING } type;

  struct g95_array_ref ar;
  g95_component *component;
  g95_symbol *sym;

  struct g95_expr *start, *end;       /* Substring */

  struct g95_ref *next;
} g95_ref;

#define g95_get_ref() g95_getmem(sizeof(g95_ref))


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
  expr_t expr_type;

  g95_typespec ts;         /* These two refer to the overall expression */
  int rank;

  g95_intrinsic_op operator;
  g95_symbol *symbol;   /* Nonnull for functions and structure constructors */
  g95_ref *ref;

  struct g95_expr *op1, *op2;
  locus where;

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
      struct intrinsic_sym *isym;
    } function;

    struct {
      int length;
      char *string;
    } character;

    struct g95_constructor *constructor;
  } value;

} g95_expr;


/* Structures for information associated with different kinds of
 * numbers.  The first set of integer parameters define all there is
 * to know about a particular kind.  The rest of the elements are
 * computed from the first elements.  */

typedef struct {
  int kind, radix, digits, bit_size;

  int range;
  mpz_t huge;
} g95_integer_info;

extern g95_integer_info g95_integer_kinds[];

typedef struct {
  int kind, bit_size;

} g95_logical_info;

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
} g95_equiv;

#define g95_get_equiv() g95_getmem(sizeof(g95_equiv))

/* g95_case stores the selector list of a case statement.  The *low
 * and *high pointers can point to the same expression in the case of
 * a single value.  If *high is NULL, the selection is from *low
 * upwards, if *low is NULL the selection is *high downwards.  */

typedef struct g95_case {
  g95_expr *low, *high;

  struct g95_case *link[2], *next;
  struct g95_code *code; /* back link to g95_code block for this case */

  int balance;
  char cache;		 /* used during insertion in AVL tree */

  int label;             /* used during character select resolution */
 
} g95_case;

#define g95_get_case() g95_getmem(sizeof(g95_case))


typedef struct {
  g95_state_data d;

  bt selector_type;
  /* root; */

} g95_select;


typedef struct {
  g95_state_data d;

  int label;
} g95_do;


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
  int err;
} g95_open;


typedef struct {
  g95_expr *unit, *status, *iostat;
  int err;
} g95_close;


typedef struct {
  g95_expr *unit, *iostat;
  int err;
} g95_filepos;


typedef struct {
  g95_expr *unit, *file, *iostat, *exist, *opened, *number, *named,
    *name, *access, *sequential, *direct, *form, *formatted,
    *unformatted, *recl, *nextrec, *blank, *position, *action, *read,
    *write, *readwrite, *delim, *pad, *iolength;

  int err;

} g95_inquire;


typedef struct {
  g95_expr *io_unit, *format_expr, *rec, *advance, *iostat, *size;

  g95_symbol *namelist;
  int format_label;  /* A format_label of -1 indicates the * format */
  int err, end, eor;

  locus eor_where, end_where;
} g95_dt;


typedef struct g95_forall_iterator {
  g95_expr *var, *start, *end, *stride;
  struct g95_forall_iterator *next;
} g95_forall_iterator;


/* Executable statements that fill g95_code structures */

typedef enum {
  EXEC_NOP=1, EXEC_ASSIGN, EXEC_POINTER_ASSIGN, EXEC_GOTO, EXEC_CALL,
  EXEC_RETURN, EXEC_STOP,
  EXEC_IF, EXEC_ARITHMETIC_IF, EXEC_DO, EXEC_DO_WHILE, EXEC_SELECT,
  EXEC_FORALL, EXEC_WHERE, EXEC_CYCLE, EXEC_EXIT,
  EXEC_ALLOCATE, EXEC_DEALLOCATE, EXEC_NULLIFY,
  EXEC_OPEN, EXEC_CLOSE, EXEC_READ, EXEC_WRITE, EXEC_IOLENGTH,
  EXEC_BACKSPACE, EXEC_ENDFILE, EXEC_INQUIRE, EXEC_REWIND
} g95_exec_op;

typedef struct g95_code {
  g95_exec_op op;

  struct g95_code *block, *next;
  locus loc;
  int block_no;

  int here, label, label2, label3;
  g95_symbol *sym;
  g95_expr *expr, *expr2;

  union {
    g95_actual_arglist *arglist;
    g95_case *case_list;
    g95_iterator *iterator;
    g95_alloc *alloc_list;
    g95_open *open;
    g95_close *close;
    g95_filepos *filepos;
    g95_inquire *inquire;
    g95_dt *dt;
    g95_forall_iterator *forall_iterator;
  } ext;     /* Points to additional structures required by statement */

} g95_code;

extern g95_code new_st;


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

/* Structure for holding compile options */

typedef struct {
  char *source, *object;
  int verbose, pedantic, line_truncation, fixed_line_length, fmode, dollar,
    q_kind, r8, i8, l1;
  g95_directorylist *include_dirs;
  char *module_dir;
  g95_source_form form;
} g95_option_t;

extern g95_option_t g95_option;


/* Constructor nodes for array and structure constructors. */

typedef struct g95_constructor {
  g95_expr *expr;
  g95_iterator *iterator;
  locus where;
  struct g95_constructor *next;
} g95_constructor;

#define g95_get_constructor() g95_getmem(sizeof(g95_constructor))

/************************ Function prototypes *************************/

/* scanner.c */

void g95_scanner_done_1(void);
void g95_scanner_init_1(void);

locus *g95_current_locus(void);
void g95_set_locus(locus *);

int g95_at_end(void);
int g95_at_eof(void);
int g95_at_bol(void);
int g95_at_eol(void);
void g95_advance_line(void);
int g95_check_include(void);

void g95_skip_comment_line(void);
void g95_skip_comments(void);
int g95_next_char_literal(int);
int g95_next_char(void);
int g95_peek_char(void);
void g95_error_recovery(void);
void g95_gobble_whitespace(void);
try g95_new_file(const char *, g95_source_form);

extern g95_file *g95_current_file;

/* misc.c */

void *g95_getmem(size_t);
void g95_free(void *);
void g95_clear_ts(g95_typespec *);
FILE *g95_open_included_file(const char *);
const char *g95_article(const char *);
const char *g95_typename(bt);
void g95_show_typespec(g95_typespec *);

const char *g95_code2string(mstring *, int);
int g95_string2code(mstring *, const char *);

void g95_init_1(void);
void g95_init_2(void);
void g95_done_1(void);
void g95_done_2(void);

void g95_iresolve_init_1(void);
void g95_iresolve_done_1(void);

/* error.c */

void g95_error_init_1(void);
void g95_buffer_error(int);

void g95_warning(const char *, ...);
void g95_warning_now(const char *, ...);
void g95_clear_warning(void);
void g95_warning_check(void);

void g95_error(const char *, ...);
void g95_error_now(const char *, ...);
void g95_fatal_error(const char *, ...)
#ifdef __GNUC__
 __attribute__ ((noreturn))
#endif
;

void g95_internal_error(const char *, ...)
#ifdef __GNUC__
 __attribute__ ((noreturn))
#endif
;

void g95_clear_error(void);
int g95_error_check(void);
void g95_syntax_error(g95_statement st);
void g95_push_error(g95_error_buf *);
void g95_pop_error(g95_error_buf *);

void g95_status(const char *, ...)
#ifdef __GNUC__
__attribute__ ((format (printf, 1, 2)))
#endif
;
void g95_status_char(char);
try g95_open_status(const char *);
try g95_close_status(void);

void g95_get_errors(int *, int *);

/* parse.c */

try g95_find_state(g95_compile_state);
g95_state_data *g95_enclosing_unit(g95_compile_state *);
const char *g95_ascii_statement(g95_statement);
const char *g95_state_name(g95_compile_state);
void g95_reject_statement(void);
try g95_parse_file(void);

extern int g95_statement_label;

/* arith.c */

const char *g95_arith_error(arith);
void g95_arith_init_1(void);

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

g95_expr *g95_constant_result(bt, int);
int g95_validate_kind(bt, int);
arith g95_check_integer_range(mpz_t, int);
arith g95_check_real_range(mpf_t, int);

arith g95_arith_uminus(g95_expr *, g95_expr **);
arith g95_arith_plus(g95_expr *, g95_expr *, g95_expr **);
arith g95_arith_minus(g95_expr *, g95_expr *, g95_expr **);
arith g95_arith_times(g95_expr *, g95_expr *, g95_expr **);
arith g95_arith_divide(g95_expr *, g95_expr *, g95_expr **);
arith g95_arith_power(g95_expr *, g95_expr *, g95_expr **);
arith g95_arith_concat(g95_expr *, g95_expr *, g95_expr **);

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
g95_expr *g95_unary_user(g95_expr *, g95_expr *);
g95_expr *g95_user(g95_expr *, g95_expr *);

g95_expr *g95_convert_integer(const char *, int, int);
g95_expr *g95_convert_real(const char *, int);
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

/* symbol.c */

match g95_match_implicit_none(void);
void g95_set_implicit_none(void);
match g95_match_implicit(void);
void g95_set_implicit(void);
try g95_set_default_type(g95_symbol *, int, g95_namespace *);
try g95_check_assign(g95_expr *, g95_expr *);
try g95_check_pointer_assign(g95_expr *, g95_expr *);
try g95_check_assign_symbol(g95_symbol *, g95_expr *);
void g95_show_attr(symbol_attribute *);
void g95_set_component_attr(g95_component *, symbol_attribute *);
void g95_get_component_attr(symbol_attribute *, g95_component *);

try g95_add_allocatable(symbol_attribute *, locus *);
try g95_add_dimension(symbol_attribute *, locus *);
try g95_add_external(symbol_attribute *, locus *);
try g95_add_intrinsic(symbol_attribute *, locus *);
try g95_add_optional(symbol_attribute *, locus *);
try g95_add_pointer(symbol_attribute *, locus *);
try g95_add_result(symbol_attribute *, locus *);
try g95_add_save(symbol_attribute *, locus *);
try g95_add_saved_common(symbol_attribute *, locus *);
try g95_add_target(symbol_attribute *, locus *);
try g95_add_dummy(symbol_attribute *, locus *);
try g95_add_data(symbol_attribute *, locus *);
try g95_add_generic(symbol_attribute *, locus *);
try g95_add_common(symbol_attribute *, locus *);
try g95_add_in_common(symbol_attribute *, locus *);
try g95_add_in_namelist(symbol_attribute *, locus *);
try g95_add_sequence(symbol_attribute *, locus *);
try g95_add_elemental(symbol_attribute *, locus *);
try g95_add_pure(symbol_attribute *, locus *);
try g95_add_recursive(symbol_attribute *, locus *);
try g95_add_function(symbol_attribute *, locus *);
try g95_add_subroutine(symbol_attribute *, locus *);
try g95_add_generic(symbol_attribute *, locus *);

try g95_add_access(symbol_attribute *, g95_access, locus *);
try g95_add_flavor(symbol_attribute *, sym_flavor, locus *);
try g95_add_entry(symbol_attribute *, locus *);
try g95_add_procedure(symbol_attribute *, procedure_type, locus *);
try g95_add_intent(symbol_attribute *, sym_intent, locus *);

int g95_compare_attr(symbol_attribute *, symbol_attribute *);
void g95_clear_attr(symbol_attribute *);
try g95_missing_attr(symbol_attribute *, locus *);
try g95_copy_attr(symbol_attribute *, symbol_attribute *, locus *);

try g95_add_component(g95_symbol *, const char *, g95_component **);
g95_symbol *g95_use_derived(g95_symbol *);
g95_component *g95_find_component(g95_symbol *, const char *);
void g95_show_components(g95_symbol *);

void g95_check_st_labels(g95_namespace *);
int g95_new_internal_label();
void g95_define_st_label(int, locus *, int, g95_sl_type);
try g95_reference_st_label(int, g95_sl_type);

g95_namespace *g95_get_namespace(void);
g95_symtree *g95_new_symtree(g95_namespace *, const char *);
g95_symtree *g95_find_symtree(g95_namespace *, const char *);
void g95_free_symbol(g95_symbol *);
g95_symbol *g95_new_symbol(const char *, g95_namespace *);
int g95_find_symbol(const char *, g95_namespace *, int, g95_symbol **);
int g95_get_symbol(const char *, g95_namespace *, int, g95_symbol **);
int g95_findget_symbol(const char *, g95_namespace *, int, g95_symbol **);

void g95_undo_symbols(void);
void g95_commit_symbols(void);
void g95_free_namespace(g95_namespace *);

void g95_symbol_init_2(void);
void g95_symbol_done_2(void);
void g95_show_symbol(g95_symbol *);

void g95_traverse_symtree(g95_namespace *, void (*)(g95_symtree *));
void g95_traverse_ns(g95_namespace *, void (*)(g95_symbol *));
void g95_save_all(g95_namespace *);

void g95_set_sym_defaults(g95_namespace *);
void g95_show_namespace(g95_namespace *);
void g95_symbol_state(void);

/* intrinsic.c */

void g95_intrinsic_init_1(void);
void g95_intrinsic_done_1(void);
void g95_intrinsic_symbol(g95_symbol *sym);

char g95_type_letter(bt);
try g95_convert_type(g95_expr *, g95_typespec *, int);
int g95_generic_intrinsic(char *);
int g95_specific_intrinsic(char *);
int g95_intrinsic_name(char *, int);

/* simplify.c */

void g95_simplify_init_1(void);
void g95_simplify_done_1(void);

/* match.c */

/* Generic match subroutines */

match g95_match_space(void);
match g95_match_eos(void);
match g95_match_small_literal_int(int *);
match g95_match_st_label(int *);
match g95_match_label(void);
match g95_match_small_int(int *);
int g95_match_strings(mstring *);
match g95_match_name(char *);
match g95_match_symbol(g95_symbol **);
match g95_match_intrinsic_op(g95_intrinsic_op *);
const char *g95_op2string(int);
match g95_match_char(char);
match g95_match(const char *, ...);
match g95_match_iterator(g95_iterator *);
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
match g95_match_common(void);
match g95_match_block_data(void);
void g95_free_namelist(g95_namelist *);
match g95_match_namelist(void);
void g95_free_equiv(g95_equiv *);
match g95_match_equivalence(void);
match g95_match_st_function(void);
void g95_free_data(g95_data *);
match g95_match_data(void);
match g95_match_where(g95_statement *);
match g95_match_elsewhere(void);
match g95_match_forall(g95_statement *);

/* decl.c */

extern g95_symbol *g95_new_derived;
extern g95_symbol *g95_new_block;

match g95_match_null(g95_expr **);
match g95_match_kind_spec(g95_typespec *);
match g95_match_old_kind_spec(g95_typespec *);
match g95_match_type_spec(g95_typespec *, int, int);

match g95_match_end(g95_statement *);
match g95_match_data_decl(void);
match g95_match_formal_arglist(g95_symbol *, int);
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

match g95_match_substring(g95_ref **, int);
match g95_match_rvalue(g95_expr **);
match g95_match_variable(g95_expr **, int);
match g95_match_expr_type(bt, g95_expr **);
match g95_match_scalar_expr(g95_expr **);
match g95_match_actual_arglist(int, g95_actual_arglist **);
void g95_free_actual_arglist(g95_actual_arglist *);
int g95_next_string_char(char);
match g95_match_literal_constant(g95_expr **, int);
symbol_attribute g95_variable_attr(g95_expr *, g95_typespec *);
void g95_show_actual_arglist(g95_actual_arglist *);
match g95_match_init_expr(g95_expr **);

/* expr.c */

g95_actual_arglist *g95_copy_actual_arglist(g95_actual_arglist *);
const char *g95_extract_int(g95_expr *, int *);

g95_expr *g95_build_funcall(g95_symbol *func, ...);
void g95_free_ref_list(g95_ref *);
void g95_type_convert_binary(g95_expr *);
try g95_simplify_expr(g95_expr *, int);
void g95_expr_init_1(void);

g95_expr *g95_get_expr(void);
void g95_free_expr(g95_expr *);
void g95_replace_expr(g95_expr *, g95_expr *);
g95_expr *g95_int_expr(int);
g95_expr *g95_logical_expr(int, locus *);
g95_code *g95_build_call(g95_symbol *, ...);
void g95_free_array_shape(g95_array_shape *);
g95_array_shape *g95_copy_array_shape(g95_array_shape *p);
g95_expr *g95_copy_expr(g95_expr *);

void g95_show_expr(g95_expr *);

int g95_numeric_ts(g95_typespec *);
int g95_kind_max(g95_expr *, g95_expr *);

/* st.c */

extern g95_code new_st, *program_tail;

void g95_clear_new_st(void);
g95_code *g95_get_code(void);
g95_code *g95_new_level(g95_code *);
g95_code *g95_add_statement(void);
g95_code *g95_append_code(g95_code *, g95_code *);
void g95_free_statements(g95_code *);
void g95_undo_statement(void);
void g95_show_code(int, g95_code *);

/* resolve.c */

void g95_resolve_formal_arglist(g95_formal_arglist *);
try g95_resolve_expr(g95_expr *);
void g95_resolve_code(g95_code *, g95_namespace *);
void g95_resolve(g95_namespace *);
try g95_resolve_iterator(g95_iterator *);

/* array.c */

void g95_free_array_spec(g95_array_spec *);
void g95_show_array_spec(g95_array_spec *);
void g95_free_array_ref(g95_array_ref *);
void g95_show_array_ref(g95_array_ref *);
g95_array_ref *g95_copy_array_ref(g95_array_ref *);

try g95_set_array_spec(g95_symbol *, g95_array_spec *, locus *);
g95_array_spec *g95_copy_array_spec(g95_array_spec *);
void g95_resolve_array_spec(g95_array_spec *);
match g95_match_array_spec(g95_array_spec **);

match g95_match_array_ref(g95_array_ref *, g95_array_spec *, int);
try g95_resolve_array_ref(g95_array_ref *, g95_array_spec *);
int g95_compare_array_spec(g95_array_spec *, g95_array_spec *);

void g95_free_constructor(g95_constructor *);
match g95_match_array_constructor(g95_expr **);
void g95_simplify_iterator_var(g95_expr *);
try g95_expand_constructor(g95_expr *);
try g95_resolve_array_constructor(g95_expr *);
try g95_check_constructor_type(g95_expr *);
try g95_check_iter_variable(g95_expr *);
try g95_check_constructor(g95_expr *, try (*)(g95_expr *));
g95_constructor *g95_copy_constructor(g95_constructor *src);
g95_expr *g95_get_array_element(g95_expr *, int);

/* interface.c */

void g95_free_interface(g95_interface *);
match g95_match_generic_spec(interface_type *, char *, int *);
match g95_match_interface(void);
match g95_match_end_interface(void);
void g95_start_interface(void);
void g95_check_operator_interfaces(g95_namespace *);
int g95_compare_actual_formal(g95_actual_arglist *, g95_formal_arglist *);
int g95_compare_types(g95_typespec *, g95_typespec *);
int g95_compare_interfaces(g95_symbol *, g95_symbol *);
try g95_check_interface(g95_interface *, g95_symbol *);
g95_symbol *g95_search_interface(g95_interface *, int, g95_actual_arglist *);
try g95_extend_expr(g95_expr *);
void g95_free_formal_arglist(g95_formal_arglist *);
try g95_extend_assign(g95_code *, g95_namespace *);
try g95_add_interface(g95_symbol *sym);
try g95_parent_procedure(g95_symbol *sym, int);

/* select.c */

void g95_free_case_list(g95_case *);
match g95_match_case(void);
match g95_match_select(void);
void g95_resolve_select(g95_code *, g95_namespace *ns);

/* io.c */

void g95_free_open(g95_open *);
match g95_match_open(void);
void g95_resolve_open(g95_open *);

void g95_free_close(g95_close *);
match g95_match_close(void);
void g95_resolve_close(g95_close *);

void g95_free_filepos(g95_filepos *);
void g95_resolve_filepos(g95_filepos *);
match g95_match_endfile(void);
match g95_match_backspace(void);
match g95_match_rewind(void);

void g95_free_inquire(g95_inquire *);
match g95_match_inquire(void);
void g95_resolve_inquire(g95_inquire *);

void g95_free_dt(g95_dt *);
void g95_resolve_dt(g95_dt *);
match g95_match_read(void);
match g95_match_write(void);
match g95_match_print(void);

/* intrinsic.c */

match g95_intrinsic_func_interface(g95_expr *, int);
try g95_intrinsic_sub_interface(g95_code *);

/* format.c */

void g95_check_format_string(g95_expr *);
match g95_match_format(void);

/* matchexp.c */

match g95_match_defined_op_name(char *, int);
match g95_match_expr(g95_expr **);

/* module.c */

void g95_module_init_2(void);
void g95_module_done_2(void);
match g95_match_module(void);
void g95_free_rename(void);
match g95_match_use(void);
void g95_dump_module(const char *, int);
void g95_use_module(void);
