/* Definitions for specs for g95
   Copyright (C) 2004 Free Software Foundation, Inc.

   This file is licensed under the GPL.

*/

/* This is the contribution to the `default_compilers' array in gcc.c for
   the f95 language.  */

  {".f",     "@f95", 0},
  {".for",   "@f95", 0},
  {".f90",   "@f95", 0},
  {".f95",   "@f95", 0},

  {".F",     "@f95-cpp-input-fixed", 0},
  {".FOR",   "@f95-cpp-input-fixed", 0},

  {".F90",   "@f95-cpp-input-free", 0},
  {".F95",   "@f95-cpp-input-free", 0},

  {"@f95",
   "%{!M:%{!MM:%{!E:f951 %i %(cc1_options) %{I*}\
         %{!fsyntax-only:%(invoke_as)}}}}", 0},

  {"@f95-cpp-input-free",
   "cpp -traditional-cpp -D_LANGUAGE_FORTRAN %(cpp_options) \
         %{E|M|MM:%(cpp_debug_options)}\
         %{!M:%{!MM:%{!E: %|.f |\n\
    f951 -ffree-form %|.f %(cc1_options) %{I*} \
         %{!fsyntax-only:%(invoke_as)}}}}", 0},

  {"@f95-cpp-input-fixed",
   "cpp -traditional-cpp -D_LANGUAGE_FORTRAN %(cpp_options) \
         %{E|M|MM:%(cpp_debug_options)}\
         %{!M:%{!MM:%{!E: %|.f |\n\
    f951 -ffixed-form %|.f %(cc1_options) %{I*} \
         %{!fsyntax-only:%(invoke_as)}}}}", 0},
