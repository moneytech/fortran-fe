/* lang-options.h file for GNU Fortran 95
   Copyright (C) 2002 Free Software Foundation, Inc.
   Contributed by Steven Bosscher

This file is part of GNU Fortran.

*/

/* This is the contribution to the `documented_lang_options' array in
   GCC's toplev.c for g95.  It is included in a GCC file called
   options.h (generated during configure). */

/* The N_ stuff is used for i18n. We don't need it, so I've created
   in a dummy macro. */

#ifdef __STDC__	/* To be consistent with lang-specs.h.  Maybe avoid
		           overflowing some old compiler's tables, etc. */

DEFINE_LANG_NAME ("GNU Fortran 95")

{ "--help",
      N_("Display this information") },
{ "-v",
      N_("Output namespace and code structures") },
{ "-Wline-truncation",
      N_("Warn about truncated source lines") },
{ "-Waliasing",
      N_("Warn about possible aliasing of dummy arguments") },
{ "-Wimplicit",
      N_("Same as -fimplicit-none") },
{ "-Wsurprising",
      N_("Warn about \"suspicious\" constructs") },
{ "-Wall",
      N_("Enable most warning messages") },
{ "-std=F",
      N_("Parse an F program") },
{ "-d8",
      N_("Set the default real and integer kinds to double precision") },
{ "-ffixed-line-length-80",
      N_("80 character line width in fixed mode") },
{ "-ffixed-line-length-132",
      N_("132 character line width in fixed mode") },
{ "-fdollar-ok",
      N_("Allow dollar signs in entity names") },
{ "-ffree-form",
      N_("Assume that the source file is free form") },
{ "-ffixed-form",
      N_("Assume that the source file is fixed form") },
{ "-fimplicit-none",
      N_("Specify that no implicit typing is allowed, unless overridden by "
	 "explicit IMPLICIT statements") },
{ "-fmodule-private",
      N_("Set default accessibility of module entities to PRIVATE") },
{ "-fqkind=<n>",
      N_("Set the kind for a real with the 'q' exponent to 'n'") },
{ "-fpack-derived",
      N_("Try to layout derived types as compact as possible") },
{ "-fmax-stack-var-size=<n>",
      N_("Size in bytes of the largest array that will be put on the stack") },
{ "-fno-repack-arrays",
      N_("Copy array sections into a contiguous block on procedure entry") },
{ "-finline-repack-arrays",
      N_("Generate inline code to repack array parameters") },
{ "-fno-inline-repack-arrays",
      N_("Generate library calls to repack array parameters") },
{ "-i8",
      N_("Set the default integer kind to double precision") },
{ "-pedantic",
      N_("Warn about use of non-standard features") },
{ "-r8",
      N_("Set the default real kind to double precision") },
{ "-I[directory]",
      N_("Append directory to the include file search path") },
{ "-M[directory]",
      N_("put and search module files in directory") },

#endif








