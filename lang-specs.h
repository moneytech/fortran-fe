/* Definitions for specs for the GNU Compiler Collection for g95,
   the GNU Fortran 95 compiler.
   Copyright (C) 2002 Free Software Foundation, Inc.

This file is licensed under the GPL.

*/

/* This is the contribution to the `default_compilers' array in gcc.c for
   the f95 language.  */

  {".f90",   "@f95" },
  {".f95",   "@f95" },
  {"@f95",
   "%{!M:%{!MM:%{!E:f951 %i %(cc1_options) %{I*}\
         %{!fsyntax-only:%(invoke_as)}}}}", 0},
		

