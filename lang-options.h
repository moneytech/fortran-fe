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

#define N_(I18N_TEXT) I18N_TEXT

/*  DEFINE_LANG_NAME ("GNU Fortran 95") # remove when we are part of GCC */

{ "--help", 
	N_("Display this information") },
{ "-v", 
	N_("Output namespace and code structures") },
{ "-Wline-truncation",
	N_("Warn about truncated source lines") },
{ "-F",
	N_("Parse an F program") },
{ "-ffixed-line-length-80",
      N_("80 character line width in fixed mode") },
{ "-fdollar-ok",
      N_("Allow dollar signs in entity names") },
{ "-ffree-form",
      N_("Assume that the source file is free form") },
{ "-ffixed-form",
      N_("Assume that the source file is fixed form") },
{ "-fqkind=<n>",
      N_("Set the kind for a real with the 'q' exponent") },
{ "-i8",
      N_("Set the default integer kind to double precision") },
{ "-pedantic",
      N_("Warn about use of non-standard features") },
{ "-r",
      N_("Run the resolution phase") },
{ "-r8",
      N_("Set the default real kind to double precision") },
{ "-I[directory]",
      N_("Append directory to the include file search path") },
{ "-M[directory]",
      N_("put and search module files in directory") },

#undef N_

#endif








