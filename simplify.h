/* Header file for simplification function prototypes
   Copyright (C) 2000 Free Software Foundation, Inc.
   Contributed by Andy Vaught & Katherine Holcomb

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


/* Prototypes for the simplification functions */

g95_expr *g95_simplify_iabs(g95_expr *);
g95_expr *g95_simplify_rabs(g95_expr *);
g95_expr *g95_simplify_cabs(g95_expr *);
g95_expr *g95_simplify_achar(g95_expr *);
g95_expr *g95_simplify_achar(g95_expr *);
g95_expr *g95_simplify_acos(g95_expr *);
g95_expr *g95_simplify_adjustl(g95_expr *);
g95_expr *g95_simplify_adjustr(g95_expr *);
g95_expr *g95_simplify_aimag(g95_expr *);
g95_expr *g95_simplify_aint(g95_expr *, g95_expr *);
g95_expr *g95_simplify_anint(g95_expr *, g95_expr *);
g95_expr *g95_simplify_asin(g95_expr *);
g95_expr *g95_simplify_atan2(g95_expr *, g95_expr *);
g95_expr *g95_simplify_bit_size(g95_expr *);
g95_expr *g95_simplify_btest(g95_expr *, g95_expr *);
g95_expr *g95_simplify_ceiling(g95_expr *, g95_expr *);
g95_expr *g95_simplify_char(g95_expr *, g95_expr *);
g95_expr *g95_simplify_cmplx(g95_expr *, g95_expr *, g95_expr *);
g95_expr *g95_simplify_conjg(g95_expr *);
g95_expr *g95_simplify_dble(g95_expr *);
g95_expr *g95_simplify_dim(g95_expr *, g95_expr *);
g95_expr *g95_simplify_dprod(g95_expr *, g95_expr *);
g95_expr *g95_simplify_epsilon(g95_expr *);
g95_expr *g95_simplify_exp(g95_expr *);
g95_expr *g95_simplify_exponent(g95_expr *);
g95_expr *g95_simplify_floor(g95_expr *, g95_expr *);
g95_expr *g95_simplify_fraction(g95_expr *);
g95_expr *g95_simplify_huge(g95_expr *);
g95_expr *g95_simplify_iachar(g95_expr *);
g95_expr *g95_simplify_iand(g95_expr *);
g95_expr *g95_simplify_ibclr(g95_expr *);
g95_expr *g95_simplify_ibits(g95_expr *);
g95_expr *g95_simplify_ibset(g95_expr *);
g95_expr *g95_simplify_ichar(g95_expr *);
g95_expr *g95_simplify_ieor(g95_expr *);
g95_expr *g95_simplify_index(g95_expr *);
g95_expr *g95_simplify_int(g95_expr *);
g95_expr *g95_simplify_ior(g95_expr *);
g95_expr *g95_simplify_ishft(g95_expr *);
g95_expr *g95_simplify_ishftc(g95_expr *);
g95_expr *g95_simplify_kind(g95_expr *);
g95_expr *g95_simplify_len_trim(g95_expr *);
g95_expr *g95_simplify_lge(g95_expr *);
g95_expr *g95_simplify_lgt(g95_expr *);
g95_expr *g95_simplify_lle(g95_expr *);
g95_expr *g95_simplify_llt(g95_expr *);
g95_expr *g95_simplify_log(g95_expr *);
g95_expr *g95_simplify_log10(g95_expr *);
g95_expr *g95_simplify_logical(g95_expr *);
g95_expr *g95_simplify_max(g95_expr *);
g95_expr *g95_simplify_maxexponent(g95_expr *);
g95_expr *g95_simplify_min(g95_expr *);
g95_expr *g95_simplify_minexponent(g95_expr *);
g95_expr *g95_simplify_mod(g95_expr *);
g95_expr *g95_simplify_modulo(g95_expr *);
g95_expr *g95_simplify_mvbits(g95_expr *);
g95_expr *g95_simplify_nearest(g95_expr *);
g95_expr *g95_simplify_nint(g95_expr *);
g95_expr *g95_simplify_not(g95_expr *);
g95_expr *g95_simplify_precision(g95_expr *);
g95_expr *g95_simplify_radix(g95_expr *);
g95_expr *g95_simplify_range(g95_expr *);
g95_expr *g95_simplify_real(g95_expr *, g95_expr *);
g95_expr *g95_simplify_rrspacing(g95_expr *);
g95_expr *g95_simplify_scale(g95_expr *);
g95_expr *g95_simplify_scan(g95_expr *);
g95_expr *g95_simplify_selected_int_kind(g95_expr *);
g95_expr *g95_simplify_selected_real_kind(g95_expr *, g95_expr *);
g95_expr *g95_simplify_set_exponent(g95_expr *);
g95_expr *g95_simplify_sign(g95_expr *);
g95_expr *g95_simplify_spacing(g95_expr *);
g95_expr *g95_simplify_sqrt(g95_expr *);
g95_expr *g95_simplify_verify(g95_expr *);

