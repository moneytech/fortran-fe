/* G95 Backend interface
   Copyright (C) 2000, 2001, 2002 Free Software Foundation, Inc.
   Disowned by Paul Brook.

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
 
/* support.c-- misc hacks */

/* This file and everything in it should not exist.
 * It is here for two reasons.
 * a) The code generator is still not completely independant
 *   of the C frontend so we have to provide some functions.
 * b) I haven't got round to figuring out what some bits do, so just blindly
 *   copied from other frontends, mnly from c-typeck.c and c-common.c.
 * DO NOT use any of these functions.
 * If you change this file, be prepared to fix the resulting mess.
 * You have been warned.
 */         
         
         
#include "trans.h"
#include "g95-support.h"
        
tree c_global_trees[CTI_MAX];     
     
void g95_init_c_decl_hacks(void) 
{       
  boolean_type_node = integer_type_node;     
  boolean_true_node = integer_one_node;        
  boolean_false_node = integer_zero_node;  
  
  intmax_type_node = integer_type_node;      
  uintmax_type_node = unsigned_type_node;         
  string_type_node = pchar_type_node;    
  const_string_type_node = build_pointer_type (build_qualified_type      
      (char_type_node, TYPE_QUAL_CONST));      
      
  void_zero_node = build_int_2 (0, 0);     
}    
    
tree build_modify_expr PARAMS((tree, enum tree_code, tree));     
static int comp_target_types PARAMS((tree, tree));       
static int comptypes PARAMS((tree, tree));          
static int function_types_compatible_p PARAMS((tree, tree));  
static void pedantic_lvalue_warning (enum tree_code);       
static tree convert_for_assignment PARAMS((tree, tree, const char *, tree, tree, int));       
//tree common_type PARAMS((tree, tree));
tree default_conversion PARAMS((tree));   
void c_expand_asm_operands PARAMS((tree, tree, tree, tree, int, const char *, int));      
tree decl_constant_value PARAMS((tree));    
static void warn_for_assignment PARAMS((const char *, const char *, tree, int));       
int lvalue_or_else PARAMS((tree, const char *));         
int lvalue_p PARAMS((tree));     
     
/*dunno if/when this should be set*/        
static int skip_evaluation = 0;        
        
/*TODO: these are just here to keep the copied code happy!
 * should either remove them or implement properly */  
static int warn_conversion = 1;     
     
static int constant_fits_type_p PARAMS((tree, tree));         
         
/* Nonzero if constant C has a value that is permissible
   for type TYPE (an INTEGER_TYPE).  */      
      
static int         
constant_fits_type_p (c, type)         
     tree c, type;      
{        
  if (TREE_CODE (c) == INTEGER_CST)      
    return int_fits_type_p (c, type);  
  
  c = convert (type, c);  
  return !TREE_OVERFLOW (c);
}         
         
/* Print a warning if an expression had overflow in folding.
   Invoke this function on every expression that
   (1) appears in the source code, and
   (2) might be a constant expression that overflowed, and
   (3) is not already checked by convert_and_check;
   however, do not invoke this function on operands of explicit casts.  */ 
 
void        
overflow_warning (value)      
     tree value;    
{    
  if ((TREE_CODE (value) == INTEGER_CST
       || (TREE_CODE (value) == COMPLEX_CST  
	   && TREE_CODE (TREE_REALPART (value)) == INTEGER_CST))          
      && TREE_OVERFLOW (value))    
    { 
      TREE_OVERFLOW (value) = 0;         
      if (skip_evaluation == 0)     
	warning ("integer overflow in expression");
    }        
  else if ((TREE_CODE (value) == REAL_CST          
	    || (TREE_CODE (value) == COMPLEX_CST 
		&& TREE_CODE (TREE_REALPART (value)) == REAL_CST))       
	   && TREE_OVERFLOW (value))
    {      
      TREE_OVERFLOW (value) = 0;       
      if (skip_evaluation == 0)     
	warning ("floating point overflow in expression");      
    }     
  else if (TREE_CODE (value) == VECTOR_CST && TREE_OVERFLOW (value))        
    {         
      TREE_OVERFLOW (value) = 0;        
      if (skip_evaluation == 0)     
	warning ("vector overflow in expression");
    }  
}   
   
/* Convert EXPR to TYPE, warning about conversion problems with constants.
   Invoke this function on every expression that is converted implicitly,
   i.e. because of language rules and not because of an explicit cast.  */         
         
tree     
convert_and_check (type, expr)        
     tree type, expr;          
{     
  tree t = convert (type, expr);         
  if (TREE_CODE (t) == INTEGER_CST)    
    {         
      if (TREE_OVERFLOW (t))     
	{  
	  TREE_OVERFLOW (t) = 0; 
 
	  /* Do not diagnose overflow in a constant expression merely
	     because a conversion overflowed.  */     
	  TREE_CONSTANT_OVERFLOW (t) = TREE_CONSTANT_OVERFLOW (expr);          
          
	  /* No warning for converting 0x80000000 to int.  */ 
	  if (!(TREE_UNSIGNED (type) < TREE_UNSIGNED (TREE_TYPE (expr))         
		&& TREE_CODE (TREE_TYPE (expr)) == INTEGER_TYPE   
		&& TYPE_PRECISION (type) == TYPE_PRECISION (TREE_TYPE (expr))))  
	    /* If EXPR fits in the unsigned version of TYPE,
	       don't warn unless pedantic.  */   
	    if ((pedantic         
		 || TREE_UNSIGNED (type)        
		 || ! constant_fits_type_p (expr, g95_unsigned_type (type)))        
	        && skip_evaluation == 0)       
	      warning ("overflow in implicit constant conversion");        
	}  
      else   
	unsigned_conversion_warning (t, expr);          
    } 
  return t;    
}     
     
/* Print a warning if a large constant is truncated to unsigned,
   or if -Wconversion is used and a constant < 0 is converted to unsigned.
   Invoke this function on every expression that might be implicitly
   converted to an unsigned type.  */    
    
void
unsigned_conversion_warning (result, operand)    
     tree result, operand;       
{          
  if (TREE_CODE (operand) == INTEGER_CST     
      && TREE_CODE (TREE_TYPE (result)) == INTEGER_TYPE      
      && TREE_UNSIGNED (TREE_TYPE (result))     
      && skip_evaluation == 0        
      && !int_fits_type_p (operand, TREE_TYPE (result))) 
    {     
      if (!int_fits_type_p (operand, g95_signed_type (TREE_TYPE (result)))) 
	/* This detects cases like converting -129 or 256 to unsigned char.  */         
	warning ("large integer implicitly truncated to unsigned type"); 
      else if (warn_conversion)    
	warning ("negative integer implicitly converted to unsigned type");        
    }
}   
   
/* Print a warning using MSGID.
   It gets OPNAME as its one parameter.
   If OPNAME is null, it is replaced by "passing arg ARGNUM of `FUNCTION'".
   FUNCTION and ARGNUM are handled specially if we are building an
   Objective-C selector.  */ 
 
static void 
warn_for_assignment (msgid, opname, function, argnum)  
     const char *msgid;        
     const char *opname;          
     tree function;  
     int argnum;    
{
  if (opname == 0)        
    {  
      /* tree selector = maybe_building_objc_message_expr (); */  
      tree selector = NULL;          
      char * new_opname;      
      
      if (selector && argnum > 2)         
	{    
	  function = selector; 
	  argnum -= 2;          
	}     
      if (function)         
	{   
	  /* Function name is known; supply it.  */        
	  const char *const argstring = _("passing arg %d of `%s'");    
	  new_opname = (char *) alloca (IDENTIFIER_LENGTH (function)         
					+ strlen (argstring) + 1 + 25          
					/*%d*/ + 1);        
	  sprintf (new_opname, argstring, argnum,
		   IDENTIFIER_POINTER (function)); 
	}          
      else    
	{     
	  /* Function name unknown (call through ptr); just give arg number.  */        
	  const char *const argnofun = _("passing arg %d of pointer to function");         
	  new_opname = (char *) alloca (strlen (argnofun) + 1 + 25 /*%d*/ + 1);   
	  sprintf (new_opname, argnofun, argnum);    
	}
      opname = new_opname;        
    }  
  pedwarn (msgid, opname); 
}          
          
/* If pedantic, warn about improper lvalue.   CODE is either COND_EXPR
   COMPOUND_EXPR, or CONVERT_EXPR (for casts).  */        
        
static void 
pedantic_lvalue_warning (code)         
     enum tree_code code;      
{   
  if (pedantic)   
    switch (code)     
      {   
      case COND_EXPR:    
	pedwarn ("ISO C forbids use of conditional expressions as lvalues");  
	break;    
      case COMPOUND_EXPR:  
	pedwarn ("ISO C forbids use of compound expressions as lvalues");
	break;     
      default:         
	pedwarn ("ISO C forbids use of cast expressions as lvalues");         
	break;
      }    
}      
      
/* Subroutines of `comptypes'.  */

/* Return 1 if two function types F1 and F2 are compatible.
   If either type specifies no argument types,
   the other must specify a fixed number of self-promoting arg types.
   Otherwise, if one type specifies only the number of arguments,
   the other must specify that number of self-promoting arg types.
   Otherwise, the argument types must match.  */      
      
static int     
function_types_compatible_p (f1, f2)  
     tree f1, f2;       
{ 
#if 0
  tree args1, args2;    
  /* 1 if no need for warning yet, 2 if warning cause has been seen.  */     
  int val1;      
#endif
  int val = 1;   
   
  if (!(TREE_TYPE (f1) == TREE_TYPE (f2)  
	|| (val = comptypes (TREE_TYPE (f1), TREE_TYPE (f2)))))          
    return 0;    
    
  return val;          
#if 0
  args1 = TYPE_ARG_TYPES (f1);
  args2 = TYPE_ARG_TYPES (f2); 
 
  /* An unspecified parmlist matches any specified parmlist
     whose argument types don't need default promotions.  */         
         
  if (args1 == 0)
    { 
      if (!self_promoting_args_p (args2))   
	return 0;        
      /* If one of these types comes from a non-prototype fn definition,
	 compare that with the other type's arglist.
	 If they don't match, ask for a warning (but no error).  */
      if (TYPE_ACTUAL_ARG_TYPES (f1)          
	  && 1 != type_lists_compatible_p (args2, TYPE_ACTUAL_ARG_TYPES (f1)))       
	val = 2;
      return val;    
    }      
  if (args2 == 0)       
    {       
      if (!self_promoting_args_p (args1))      
	return 0;    
      if (TYPE_ACTUAL_ARG_TYPES (f2)
	  && 1 != type_lists_compatible_p (args1, TYPE_ACTUAL_ARG_TYPES (f2)))       
	val = 2;        
      return val;         
    }         
         
  /* Both types have argument lists: compare them and propagate results.  */        
  val1 = type_lists_compatible_p (args1, args2); 
  return val1 != 1 ? val1 : val;       
#endif
}   
   
/* Return 1 if TYPE1 and TYPE2 are compatible types for assignment
   or various other operations.  Return 2 if they are compatible
   but a warning may be needed if you use them together.  */ 
 
static int       
comptypes (type1, type2)
     tree type1, type2;    
{         
  tree t1 = type1;
  tree t2 = type2;        
  int attrval, val;       
       
  /* Suppress errors caused by previously reported errors.  */       
       
  if (t1 == t2 || !t1 || !t2 
      || TREE_CODE (t1) == ERROR_MARK || TREE_CODE (t2) == ERROR_MARK)          
    return 1; 
 
  /* If either type is the internal version of sizetype, return the
     language version.  */  
  if (TREE_CODE (t1) == INTEGER_TYPE && TYPE_IS_SIZETYPE (t1)      
      && TYPE_DOMAIN (t1) != 0)  
    t1 = TYPE_DOMAIN (t1);         
         
  if (TREE_CODE (t2) == INTEGER_TYPE && TYPE_IS_SIZETYPE (t2)       
      && TYPE_DOMAIN (t2) != 0)   
    t2 = TYPE_DOMAIN (t2);         
         
  /* Treat an enum type as the integer type of the same width and
     signedness.  */    
    
  if (TREE_CODE (t1) == ENUMERAL_TYPE)    
    t1 = g95_type_for_size (TYPE_PRECISION (t1), TREE_UNSIGNED (t1));   
  if (TREE_CODE (t2) == ENUMERAL_TYPE)         
    t2 = g95_type_for_size (TYPE_PRECISION (t2), TREE_UNSIGNED (t2));    
    
  if (t1 == t2)        
    return 1;    
    
  /* Different classes of types can't be compatible.  */  
  
  if (TREE_CODE (t1) != TREE_CODE (t2)) return 0;       
       
  /* Qualifiers must match.  */      
      
  if (TYPE_QUALS (t1) != TYPE_QUALS (t2))       
    return 0;        
        
  /* Allow for two different type nodes which have essentially the same
     definition.  Note that we already checked for equality of the type
     qualifiers (just above).  */ 
 
  if (TYPE_MAIN_VARIANT (t1) == TYPE_MAIN_VARIANT (t2))
    return 1;          
          
  /* 1 if no need for warning yet, 2 if warning cause has been seen.  */  
  if (! (attrval = (*targetm.comp_type_attributes) (t1, t2)))  
     return 0;

  /* 1 if no need for warning yet, 2 if warning cause has been seen.  */  
  val = 0;          
          
  switch (TREE_CODE (t1)) 
    {          
    case POINTER_TYPE:          
      val = (TREE_TYPE (t1) == TREE_TYPE (t2)  
	      ? 1 : comptypes (TREE_TYPE (t1), TREE_TYPE (t2)));  
      break;      
      
    case FUNCTION_TYPE:        
      val = function_types_compatible_p (t1, t2); 
      break;          
          
    case ARRAY_TYPE:         
      {       
	tree d1 = TYPE_DOMAIN (t1);  
	tree d2 = TYPE_DOMAIN (t2);      
	bool d1_variable, d2_variable;   
	bool d1_zero, d2_zero;        
	val = 1;     
     
	/* Target types must match incl. qualifiers.  */ 
	if (TREE_TYPE (t1) != TREE_TYPE (t2)   
	    && 0 == (val = comptypes (TREE_TYPE (t1), TREE_TYPE (t2))))     
	  return 0;      
      
	/* Sizes must match unless one is missing or variable.  */          
	if (d1 == 0 || d2 == 0 || d1 == d2)        
	  break;      
      
	d1_zero = ! TYPE_MAX_VALUE (d1);
	d2_zero = ! TYPE_MAX_VALUE (d2);          
          
	d1_variable = (! d1_zero          
		       && (TREE_CODE (TYPE_MIN_VALUE (d1)) != INTEGER_CST
			   || TREE_CODE (TYPE_MAX_VALUE (d1)) != INTEGER_CST));        
	d2_variable = (! d2_zero     
		       && (TREE_CODE (TYPE_MIN_VALUE (d2)) != INTEGER_CST        
			   || TREE_CODE (TYPE_MAX_VALUE (d2)) != INTEGER_CST)); 
 
	if (d1_variable || d2_variable)          
	  break;        
	if (d1_zero && d2_zero)     
	  break;          
	if (d1_zero || d2_zero    
	    || ! tree_int_cst_equal (TYPE_MIN_VALUE (d1), TYPE_MIN_VALUE (d2))        
	    || ! tree_int_cst_equal (TYPE_MAX_VALUE (d1), TYPE_MAX_VALUE (d2)))  
	  val = 0;     
     
        break;
      }   
   
    case RECORD_TYPE:       
/*      if (maybe_objc_comptypes (t1, t2, 0) == 1)
	val = 1; */ 
      break;    
    
    default:   
      break;  
    }        
  return attrval == 2 && val == 1 ? 2 : val;
}   
   
/* Return 1 if TTL and TTR are pointers to types that are equivalent,
   ignoring their qualifiers.  */

static int      
comp_target_types (ttl, ttr)
     tree ttl, ttr;      
{
  int val;    
    
  val = comptypes (TYPE_MAIN_VARIANT (TREE_TYPE (ttl)),      
		   TYPE_MAIN_VARIANT (TREE_TYPE (ttr)));      
      
  if (val == 2 && pedantic)          
    pedwarn ("types are not quite compatible");          
  return val;        
}         
         
void     
c_expand_asm_operands (string, outputs, inputs, clobbers, vol, filename, line)     
     tree string ATTRIBUTE_UNUSED, outputs ATTRIBUTE_UNUSED;
     tree inputs ATTRIBUTE_UNUSED, clobbers ATTRIBUTE_UNUSED;       
     int vol ATTRIBUTE_UNUSED;        
     const char *filename ATTRIBUTE_UNUSED;        
     int line ATTRIBUTE_UNUSED;         
{          
  internal_error("fortran shouldn't contain asm statements!");  
}     
     
/* Given a list of expressions, return a compound expression
   that performs them all and returns the value of the last of them.  */   
   
tree        
build_modify_expr (lhs, modifycode, rhs)  
     tree lhs, rhs; 
     enum tree_code modifycode;      
{        
  tree result;     
  tree newrhs;     
  tree lhstype = TREE_TYPE (lhs);   
  tree olhstype = lhstype;     
     
  /* Avoid duplicate error messages from operands that had errors.  */         
  if (TREE_CODE (lhs) == ERROR_MARK || TREE_CODE (rhs) == ERROR_MARK)         
    return error_mark_node;

  /* Strip NON_LVALUE_EXPRs since we aren't using as an lvalue.  */          
  /* Do not use STRIP_NOPS here.  We do not want an enumerator
     whose value is 0 to count as a null pointer constant.  */         
  if (TREE_CODE (rhs) == NON_LVALUE_EXPR)          
    rhs = TREE_OPERAND (rhs, 0);       
       
  newrhs = rhs;

  /* Handle control structure constructs used as "lvalues".  */      
      
  switch (TREE_CODE (lhs))      
    {          
      /* Handle (a, b) used as an "lvalue".  */     
    case COMPOUND_EXPR:      
      pedantic_lvalue_warning (COMPOUND_EXPR);        
      newrhs = build_modify_expr (TREE_OPERAND (lhs, 1), modifycode, rhs);
      if (TREE_CODE (newrhs) == ERROR_MARK)       
	return error_mark_node;  
      return build (COMPOUND_EXPR, lhstype,      
		    TREE_OPERAND (lhs, 0), newrhs);       
    default:   
      break;    
    }   
   
  /* If a binary op has been requested, combine the old LHS value with the RHS
     producing the value we should actually store into the LHS.  */    
    
  if (modifycode != NOP_EXPR)  
    {  
      internal_error("Fortran shouldn't call build_modify_exprwith modifycode!=NOP_EXPR");
    }    
    
  /* Handle a cast used as an "lvalue".
     We have already performed any binary operator using the value as cast.
     Now convert the result to the cast type of the lhs,
     and then true type of the lhs and store it there;
     then convert result back to the cast type to be the value
     of the assignment.  */   
   
  switch (TREE_CODE (lhs)) 
    {
    case NOP_EXPR:          
    case CONVERT_EXPR:          
    case FLOAT_EXPR:       
    case FIX_TRUNC_EXPR: 
    case FIX_FLOOR_EXPR:    
    case FIX_ROUND_EXPR:        
    case FIX_CEIL_EXPR:      
      error("lhs is a cast"); 
      break;
#if 0
      newrhs = default_function_array_conversion (newrhs); 
      { 
	tree inner_lhs = TREE_OPERAND (lhs, 0);        
	tree result;     
	result = build_modify_expr (inner_lhs, NOP_EXPR,    
				    convert (TREE_TYPE (inner_lhs),    
					     convert (lhstype, newrhs)));          
	if (TREE_CODE (result) == ERROR_MARK)     
	  return result;        
	pedantic_lvalue_warning (CONVERT_EXPR);      
	return convert (TREE_TYPE (lhs), result);
      }        
#endif
   
    default:     
      break;    
    }

  /* Now we have handled acceptable kinds of LHS that are not truly lvalues.
     Reject anything strange now.  */   
   
  if (!lvalue_or_else (lhs, "invalid lvalue in assignment"))        
    return error_mark_node;     
     
#if 0
  /* Warn about storing in something that is `const'.  */         
         
  if (TREE_READONLY (lhs) || TYPE_READONLY (lhstype)        
      || ((TREE_CODE (lhstype) == RECORD_TYPE 
	   || TREE_CODE (lhstype) == UNION_TYPE)
	   && C_TYPE_FIELDS_READONLY (lhstype)))
    readonly_warning (lhs, "assignment");        
#endif
          
  /* If storing into a structure or union member,
     it has probably been given type `int'.
     Compute the type that would go with
     the actual amount of storage the member occupies.  */      
      
  if (TREE_CODE (lhs) == COMPONENT_REF   
      && (TREE_CODE (lhstype) == INTEGER_TYPE      
	  || TREE_CODE (lhstype) == BOOLEAN_TYPE  
	  || TREE_CODE (lhstype) == REAL_TYPE   
	  || TREE_CODE (lhstype) == ENUMERAL_TYPE))          
    lhstype = TREE_TYPE (get_unwidened (lhs, 0));        
        
  /* If storing in a field that is in actuality a short or narrower than one,
     we must store in the field in its actual type.  */        
        
  if (lhstype != TREE_TYPE (lhs))  
    {         
      lhs = copy_node (lhs);     
      TREE_TYPE (lhs) = lhstype;          
    }       
       
  /* Convert new value to destination type.  */      
      
  newrhs = convert_for_assignment (lhstype, newrhs, _("assignment"),     
				   NULL_TREE, NULL_TREE, 0);    
  if (TREE_CODE (newrhs) == ERROR_MARK)  
    return error_mark_node; 
 
  /* Scan operands */      
      
  result = build (MODIFY_EXPR, lhstype, lhs, newrhs);   
  TREE_SIDE_EFFECTS (result) = 1;         
         
  /* If we got the LHS in a different type for storing in,
     convert the result back to the nominal type of LHS
     so that the value we return always has the same type
     as the LHS argument.  */        
        
  if (olhstype == TREE_TYPE (result))   
    return result;    
  return convert_for_assignment (olhstype, result, _("assignment"),
				 NULL_TREE, NULL_TREE, 0);  
}         
         
/* Return nonzero if REF is an lvalue valid for this language.
   Lvalues can be assigned, unless their type has TYPE_READONLY.
   Lvalues can have their address taken, unless they have DECL_REGISTER.  */        
        
int    
lvalue_p (ref)
     tree ref;     
{      
  enum tree_code code = TREE_CODE (ref);         
         
  switch (code)  
    {  
    case REALPART_EXPR:       
    case IMAGPART_EXPR:
    case COMPONENT_REF:         
      return lvalue_p (TREE_OPERAND (ref, 0));      
      
//    case COMPOUND_LITERAL_EXPR: 
    case STRING_CST:
      return 1;         
         
    case INDIRECT_REF:   
    case ARRAY_REF:       
    case VAR_DECL:   
    case PARM_DECL:          
    case RESULT_DECL:    
    case ERROR_MARK:          
      return (TREE_CODE (TREE_TYPE (ref)) != FUNCTION_TYPE
	      && TREE_CODE (TREE_TYPE (ref)) != METHOD_TYPE);        
        
    case BIND_EXPR:
    case RTL_EXPR:  
      return TREE_CODE (TREE_TYPE (ref)) == ARRAY_TYPE;          
          
    default: 
      return 0;       
    }      
}        
        
/* Return nonzero if REF is an lvalue valid for this language;
   otherwise, print an error message and return zero.  */      
      
int       
lvalue_or_else (ref, msgid) 
     tree ref;         
     const char *msgid;       
{   
  int win = lvalue_p (ref);        
        
  if (! win)          
    error ("%s", msgid);  
  
  return win;     
}         
         
/* Convert value RHS to type TYPE as preparation for an assignment
   to an lvalue of type TYPE.
   The real work of conversion is done by `convert'.
   The purpose of this function is to generate error messages
   for assignments that are not allowed.
   ERRTYPE is a string to use in error messages:
   "assignment", "return", etc.  If it is null, this is parameter passing
   for a function call (and different error messages are output).

   FUNNAME is the name of the function being called,
   as an IDENTIFIER_NODE, or null.
   PARMNUM is the number of the argument, for printing in error messages.  */ 
 
static tree          
convert_for_assignment (type, rhs, errtype, fundecl, funname, parmnum)          
     tree type, rhs;         
     const char *errtype;          
     tree fundecl, funname;     
     int parmnum;          
{    
  enum tree_code codel = TREE_CODE (type);      
  tree rhstype;         
  enum tree_code coder;   
   
  /* Strip NON_LVALUE_EXPRs since we aren't using as an lvalue.  */  
  /* Do not use STRIP_NOPS here.  We do not want an enumerator
     whose value is 0 to count as a null pointer constant.  */    
  if (TREE_CODE (rhs) == NON_LVALUE_EXPR)    
    rhs = TREE_OPERAND (rhs, 0);    
    
  if (TREE_CODE (TREE_TYPE (rhs)) == ARRAY_TYPE         
      || TREE_CODE (TREE_TYPE (rhs)) == FUNCTION_TYPE)       
    rhs = default_conversion (rhs);          
  else if (optimize && TREE_CODE (rhs) == VAR_DECL)       
    rhs = decl_constant_value (rhs);          
          
  rhstype = TREE_TYPE (rhs);
  coder = TREE_CODE (rhstype);  
  
  if (coder == ERROR_MARK)
    return error_mark_node;      
      
  if (TYPE_MAIN_VARIANT (type) == TYPE_MAIN_VARIANT (rhstype))   
    {      
      overflow_warning (rhs);
      return rhs;        
    }

  if (coder == VOID_TYPE)         
    {   
      error ("void value not ignored as it ought to be");          
      return error_mark_node;   
    }      
  /* A type converts to a reference to it.
     This code doesn't fully support references, it's just for the
     special case of va_start and va_copy.  */  
  if (codel == REFERENCE_TYPE    
      && comptypes (TREE_TYPE (type), TREE_TYPE (rhs)) == 1)
    {        
      if (g95_mark_addressable (rhs) == 0)        
	return error_mark_node;
      rhs = build1 (ADDR_EXPR, build_pointer_type (TREE_TYPE (rhs)), rhs);       
       
      /* We already know that these two types are compatible, but they
	 may not be exactly identical.  In fact, `TREE_TYPE (type)' is
	 likely to be __builtin_va_list and `TREE_TYPE (rhs)' is
	 likely to be va_list, a typedef to __builtin_va_list, which
	 is different enough that it will cause problems later.  */
      if (TREE_TYPE (TREE_TYPE (rhs)) != TREE_TYPE (type))       
	rhs = build1 (NOP_EXPR, build_pointer_type (TREE_TYPE (type)), rhs);      
      
      rhs = build1 (NOP_EXPR, type, rhs);   
      return rhs;   
    }      
  /* Arithmetic types all interconvert, and enum is treated like int.  */     
  else if ((codel == INTEGER_TYPE || codel == REAL_TYPE         
	    || codel == ENUMERAL_TYPE || codel == COMPLEX_TYPE 
	    || codel == BOOLEAN_TYPE)  
	   && (coder == INTEGER_TYPE || coder == REAL_TYPE    
	       || coder == ENUMERAL_TYPE || coder == COMPLEX_TYPE
	       || coder == BOOLEAN_TYPE))          
    return convert_and_check (type, rhs);          
          
  /* Conversion to a transparent union from its member types.
     This applies only to function arguments.  */          
  else if (codel == UNION_TYPE && TYPE_TRANSPARENT_UNION (type) && ! errtype)        
    {          
      tree memb_types;        
      tree marginal_memb_type = 0;  
  
      for (memb_types = TYPE_FIELDS (type); memb_types;         
	   memb_types = TREE_CHAIN (memb_types))        
	{        
	  tree memb_type = TREE_TYPE (memb_types);  
  
	  if (comptypes (TYPE_MAIN_VARIANT (memb_type),          
			 TYPE_MAIN_VARIANT (rhstype)))         
	    break;       
       
	  if (TREE_CODE (memb_type) != POINTER_TYPE) 
	    continue;     
     
	  if (coder == POINTER_TYPE)         
	    {         
	      tree ttl = TREE_TYPE (memb_type);         
	      tree ttr = TREE_TYPE (rhstype);        
        
	      /* Any non-function converts to a [const][volatile] void *
		 and vice versa; otherwise, targets must be the same.
		 Meanwhile, the lhs target must have all the qualifiers of
		 the rhs.  */
	      if (VOID_TYPE_P (ttl) || VOID_TYPE_P (ttr)     
		  || comp_target_types (memb_type, rhstype))      
		{    
		  /* If this type won't generate any warnings, use it.  */    
		  if (TYPE_QUALS (ttl) == TYPE_QUALS (ttr)       
		      || ((TREE_CODE (ttr) == FUNCTION_TYPE  
			   && TREE_CODE (ttl) == FUNCTION_TYPE)     
			  ? ((TYPE_QUALS (ttl) | TYPE_QUALS (ttr)) 
			     == TYPE_QUALS (ttr))     
			  : ((TYPE_QUALS (ttl) | TYPE_QUALS (ttr))         
			     == TYPE_QUALS (ttl))))    
		    break; 
 
		  /* Keep looking for a better type, but remember this one.  */        
		  if (! marginal_memb_type)          
		    marginal_memb_type = memb_type;      
		}      
	    }   
   
	  /* Can convert integer zero to any pointer type.  */       
	  if (integer_zerop (rhs)
	      || (TREE_CODE (rhs) == NOP_EXPR      
		  && integer_zerop (TREE_OPERAND (rhs, 0))))         
	    {       
	      rhs = null_pointer_node;   
	      break; 
	    }     
	}     
     
      if (memb_types || marginal_memb_type) 
	{ 
	  if (! memb_types)       
	    { 
	      /* We have only a marginally acceptable member type;
		 it needs a warning.  */     
	      tree ttl = TREE_TYPE (marginal_memb_type);       
	      tree ttr = TREE_TYPE (rhstype);        
        
	      /* Const and volatile mean something different for function
		 types, so the usual warnings are not appropriate.  */      
	      if (TREE_CODE (ttr) == FUNCTION_TYPE         
		  && TREE_CODE (ttl) == FUNCTION_TYPE)     
		{          
		  /* Because const and volatile on functions are
		     restrictions that say the function will not do
		     certain things, it is okay to use a const or volatile
		     function where an ordinary one is wanted, but not
		     vice-versa.  */  
		  if (TYPE_QUALS (ttl) & ~TYPE_QUALS (ttr))         
		    warn_for_assignment ("%s makes qualified function pointer from unqualified", 
					 errtype, funname, parmnum);   
		}
	      else if (TYPE_QUALS (ttr) & ~TYPE_QUALS (ttl))   
		warn_for_assignment ("%s discards qualifiers from pointer target type",        
				     errtype, funname,      
				     parmnum);
	    } 
 
	  if (pedantic && ! DECL_IN_SYSTEM_HEADER (fundecl)) 
	    pedwarn ("ISO C prohibits argument conversion to union type");      
      
	  return build1 (NOP_EXPR, type, rhs);  
	}        
    }    
    
  /* Conversions among pointers */   
  else if ((codel == POINTER_TYPE || codel == REFERENCE_TYPE)     
	   && (coder == POINTER_TYPE || coder == REFERENCE_TYPE))  
    {  
      tree ttl = TREE_TYPE (type);
      tree ttr = TREE_TYPE (rhstype); 
 
      /* Any non-function converts to a [const][volatile] void *
	 and vice versa; otherwise, targets must be the same.
	 Meanwhile, the lhs target must have all the qualifiers of the rhs.  */    
      if (VOID_TYPE_P (ttl) || VOID_TYPE_P (ttr) 
	  || comp_target_types (type, rhstype)         
	  || (g95_unsigned_type (TYPE_MAIN_VARIANT (ttl))
	      == g95_unsigned_type (TYPE_MAIN_VARIANT (ttr))))   
	{ 
	  if (pedantic          
	      && ((VOID_TYPE_P (ttl) && TREE_CODE (ttr) == FUNCTION_TYPE)  
		  ||   
		  (VOID_TYPE_P (ttr)        
		   /* Check TREE_CODE to catch cases like (void *) (char *) 0
		      which are not ANSI null ptr constants.  */  
		   && (!integer_zerop (rhs) || TREE_CODE (rhs) == NOP_EXPR) 
		   && TREE_CODE (ttl) == FUNCTION_TYPE)))         
	    warn_for_assignment ("ISO C forbids %s between function pointer and `void *'",    
				 errtype, funname, parmnum);      
	  /* Const and volatile mean something different for function types,
	     so the usual warnings are not appropriate.  */  
	  else if (TREE_CODE (ttr) != FUNCTION_TYPE       
		   && TREE_CODE (ttl) != FUNCTION_TYPE)          
	    {
	      if (TYPE_QUALS (ttr) & ~TYPE_QUALS (ttl)) 
		warn_for_assignment ("%s discards qualifiers from pointer target type",  
				     errtype, funname, parmnum); 
	      /* If this is not a case of ignoring a mismatch in signedness,
		 no warning.  */         
	      else if (VOID_TYPE_P (ttl) || VOID_TYPE_P (ttr)         
		       || comp_target_types (type, rhstype))        
		;  
	      /* If there is a mismatch, do warn.  */      
	      else if (pedantic)    
		warn_for_assignment ("pointer targets in %s differ in signedness",   
				     errtype, funname, parmnum);      
	    }   
	  else if (TREE_CODE (ttl) == FUNCTION_TYPE        
		   && TREE_CODE (ttr) == FUNCTION_TYPE)          
	    {          
	      /* Because const and volatile on functions are restrictions
		 that say the function will not do certain things,
		 it is okay to use a const or volatile function
		 where an ordinary one is wanted, but not vice-versa.  */
	      if (TYPE_QUALS (ttl) & ~TYPE_QUALS (ttr))       
		warn_for_assignment ("%s makes qualified function pointer from unqualified",          
				     errtype, funname, parmnum);         
	    }    
	}        
      else     
	warn_for_assignment ("%s from incompatible pointer type",          
			     errtype, funname, parmnum);
      return convert (type, rhs);     
    }     
  else if (codel == POINTER_TYPE && coder == INTEGER_TYPE)      
    {    
      /* An explicit constant 0 can convert to a pointer,
	 or one that results from arithmetic, even including
	 a cast to integer type.  */     
      if (! (TREE_CODE (rhs) == INTEGER_CST && integer_zerop (rhs))      
	  &&
	  ! (TREE_CODE (rhs) == NOP_EXPR
	     && TREE_CODE (TREE_TYPE (rhs)) == INTEGER_TYPE   
	     && TREE_CODE (TREE_OPERAND (rhs, 0)) == INTEGER_CST 
	     && integer_zerop (TREE_OPERAND (rhs, 0))))       
	{     
	  warn_for_assignment ("%s makes pointer from integer without a cast",   
			       errtype, funname, parmnum);   
	  return convert (type, rhs);    
	}      
      return null_pointer_node;        
    }        
  else if (codel == INTEGER_TYPE && coder == POINTER_TYPE)
    {       
      warn_for_assignment ("%s makes integer from pointer without a cast",    
			   errtype, funname, parmnum);
      return convert (type, rhs);
    }      
  else if (codel == BOOLEAN_TYPE && coder == POINTER_TYPE)     
    return convert (type, rhs);     
     
  if (!errtype)       
    {  
      if (funname)
 	{    
 	  /* tree selector = maybe_building_objc_message_expr ();*/     
	  tree selector = NULL;

 	  if (selector && parmnum > 2)  
 	    error ("incompatible type for argument %d of `%s'",        
		   parmnum - 2, IDENTIFIER_POINTER (selector));   
 	  else      
	    error ("incompatible type for argument %d of `%s'",    
		   parmnum, IDENTIFIER_POINTER (funname));  
	}        
      else          
	error ("incompatible type for argument %d of indirect function call",       
	       parmnum);   
    }  
  else    
    error ("incompatible types in %s", errtype);   
   
  return error_mark_node;       
}         
         
/* Perform default promotions for C data used in expressions.
   Arrays and functions are converted to pointers;
   enumeral types or short or char, to int.
   In addition, manifest constants symbols are replaced by their values.  */     
     
tree  
default_conversion (exp)          
     tree exp;         
{          
  tree orig_exp;  
  tree type = TREE_TYPE (exp); 
  enum tree_code code = TREE_CODE (type);          
          
  if (code == FUNCTION_TYPE || code == ARRAY_TYPE)    
  { 
    error("taking address of function or array is not hacked yet");  
    return(error_mark_node);   
    /* return default_function_array_conversion (exp);*/ 
  }       
       
  /* Constants can be used directly unless they're not loadable.  */    
  if (TREE_CODE (exp) == CONST_DECL)        
    exp = DECL_INITIAL (exp);  
  
  /* Replace a nonvolatile const static variable with its value unless
     it is an array, in which case we must be sure that taking the
     address of the array produces consistent results.  */   
  else if (optimize && TREE_CODE (exp) == VAR_DECL && code != ARRAY_TYPE)    
    {       
      exp = decl_constant_value (exp);         
      type = TREE_TYPE (exp);         
    }          
          
  /* Strip NON_LVALUE_EXPRs and no-op conversions, since we aren't using as
     an lvalue.

     Do not use STRIP_NOPS here!  It will remove conversions from pointer
     to integer and cause infinite recursion.  */       
  orig_exp = exp;    
  while (TREE_CODE (exp) == NON_LVALUE_EXPR     
	 || (TREE_CODE (exp) == NOP_EXPR  
	     && TREE_TYPE (TREE_OPERAND (exp, 0)) == TREE_TYPE (exp)))        
    exp = TREE_OPERAND (exp, 0);     
     
#if 0
  /* Preserve the original expression code.  */    
  if (IS_EXPR_CODE_CLASS (TREE_CODE_CLASS (TREE_CODE (exp))))     
    C_SET_EXP_ORIGINAL_CODE (exp, C_EXP_ORIGINAL_CODE (orig_exp));          
#endif
      
  /* Normally convert enums to int,
     but convert wide enums to something wider.  */  
  if (code == ENUMERAL_TYPE)          
    {
      type = g95_type_for_size (MAX (TYPE_PRECISION (type),  
				 TYPE_PRECISION (integer_type_node)),
			    ((/*flag_traditional
			      || */(TYPE_PRECISION (type)          
				  >= TYPE_PRECISION (integer_type_node)))  
			     && TREE_UNSIGNED (type)));       
       
      return convert (type, exp);   
    }      
      
/* commented oit cos I don't really understand it... */  
#if 0
  if (TREE_CODE (exp) == COMPONENT_REF
      && DECL_C_BIT_FIELD (TREE_OPERAND (exp, 1))   
      /* If it's thinner than an int, promote it like a
	 c_promoting_integer_type_p, otherwise leave it alone.  */         
      && 0 > compare_tree_int (DECL_SIZE (TREE_OPERAND (exp, 1)),  
			       TYPE_PRECISION (integer_type_node)))         
    return convert (flag_traditional && TREE_UNSIGNED (type)   
		    ? unsigned_type_node : integer_type_node,     
		    exp);    
    
  if (c_promoting_integer_type_p (type)) 
    { 
      /* Traditionally, unsignedness is preserved in default promotions.
         Also preserve unsignedness if not really getting any wider.  */    
      if (TREE_UNSIGNED (type)        
	  && (flag_traditional  
	      || TYPE_PRECISION (type) == TYPE_PRECISION (integer_type_node)))         
	return convert (unsigned_type_node, exp);  
  
      return convert (integer_type_node, exp);         
    }    
    
  if (flag_traditional && !flag_allow_single_precision         
      && TYPE_MAIN_VARIANT (type) == float_type_node)     
    return convert (double_type_node, exp);    
#endif
         
  if (code == VOID_TYPE)         
    {     
      error ("void value not ignored as it ought to be");  
      return error_mark_node;   
    }         
  return exp;    
}      
      
/* Return either DECL or its known constant value (if it has one).  */ 
 
tree
decl_constant_value (decl)  
     tree decl; 
{       
  if (/* Don't change a variable array bound or initial value to a constant
	 in a place where a variable is invalid.  */       
      current_function_decl != 0     
      && ! TREE_THIS_VOLATILE (decl)  
      && TREE_READONLY (decl)    
      && DECL_INITIAL (decl) != 0         
      && TREE_CODE (DECL_INITIAL (decl)) != ERROR_MARK 
      /* This is invalid if initial value is not constant.
	 If it has either a function call, a memory reference,
	 or a variable, then re-evaluating it could give different results.  */     
      && TREE_CONSTANT (DECL_INITIAL (decl))        
      /* Check for cases where this is sub-optimal, even though valid.  */
      && TREE_CODE (DECL_INITIAL (decl)) != CONSTRUCTOR) 
    return DECL_INITIAL (decl);       
  return decl;        
}  
  
