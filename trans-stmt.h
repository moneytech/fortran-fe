/* Header for statement translation functions
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

/* Statement translators (g95_trans_*) return a fully translated tree.
   Usually it will be a COMPOUND_STMT node because most statements need a
   new scope, hence a new compound.  */

/* Returns NULL if code==NULL, otherwise a single COMPOND_STMT
Calls g95_trans_*.  */
tree g95_trans_code (g95_code *);

/* All other g95_trans_* should only need be called by g95_trans_code */

/* trans-expr.c */
tree g95_trans_assign(g95_code *);
tree g95_trans_pointer_assign(g95_code *);

/* trans-stmt.c */
tree g95_trans_cycle (g95_code *);
tree g95_trans_exit (g95_code *);
tree g95_trans_label_here(g95_code *);
tree g95_trans_goto (g95_code *);
tree g95_trans_stop (g95_code *);
tree g95_trans_call (g95_code *);
tree g95_trans_return (g95_code *);
tree g95_trans_if (g95_code *);
tree g95_trans_arithmetic_if (g95_code *);
tree g95_trans_do (g95_code *);
tree g95_trans_do_while (g95_code *);
tree g95_trans_select (g95_code *);
tree g95_trans_forall (g95_code *);
tree g95_trans_where (g95_code *);
tree g95_trans_allocate (g95_code *);
tree g95_trans_deallocate (g95_code *);
tree g95_trans_deallocate_array (tree);

/* trans-io.c */
tree g95_trans_open (g95_code *);
tree g95_trans_close (g95_code *);
tree g95_trans_read (g95_code *);
tree g95_trans_write (g95_code *);
tree g95_trans_iolength (g95_code *);
tree g95_trans_backspace (g95_code *);
tree g95_trans_endfile (g95_code *);
tree g95_trans_inquire (g95_code *);
tree g95_trans_rewind (g95_code *);

tree g95_trans_io_call (g95_code *);
