/* Header file for simplification function prototypes
   Copyright (C) 2000, 2001, 2002 Free Software Foundation, Inc.
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

/* Expression returned when simplification fails */

extern g95_expr g95_bad_expr;


/* Check functions for intrinsic functions */

try g95_check_a_ikind(g95_expr *, g95_expr *);
try g95_check_a_xkind(g95_expr *, g95_expr *);
try g95_check_a_p(g95_expr *, g95_expr *);

try g95_check_abs(g95_expr *);
try g95_check_all_any(g95_expr *, g95_expr *);
try g95_check_allocated(g95_expr *);
try g95_check_associated(g95_expr *, g95_expr *);
try g95_check_btest(g95_expr *, g95_expr *);
try g95_check_char(g95_expr *, g95_expr *);
try g95_check_cmplx(g95_expr *, g95_expr *, g95_expr *);
try g95_check_count(g95_expr *, g95_expr *);
try g95_check_cshift(g95_expr *, g95_expr *, g95_expr *);
try g95_check_dcmplx(g95_expr *, g95_expr *);
try g95_check_dble(g95_expr *);
try g95_check_digits(g95_expr *);
try g95_check_dot_product(g95_expr *, g95_expr *);
try g95_check_eoshift(g95_expr *, g95_expr *, g95_expr *, g95_expr *);
try g95_check_huge(g95_expr *);
try g95_check_i(g95_expr *);
try g95_check_iand(g95_expr *, g95_expr *);
try g95_check_ibclr(g95_expr *, g95_expr *);
try g95_check_ibits(g95_expr *, g95_expr *, g95_expr *);
try g95_check_ibset(g95_expr *, g95_expr *);
try g95_check_idnint(g95_expr *);
try g95_check_ieor(g95_expr *, g95_expr *);
try g95_check_index(g95_expr *, g95_expr *, g95_expr *);
try g95_check_int(g95_expr *, g95_expr *);
try g95_check_ior(g95_expr *, g95_expr *);
try g95_check_ishft(g95_expr *, g95_expr *);
try g95_check_ishftc(g95_expr *, g95_expr *, g95_expr *);
try g95_check_kind(g95_expr *);
try g95_check_lbound(g95_expr *, g95_expr *);
try g95_check_logical(g95_expr *, g95_expr *);
try g95_check_min_max(g95_actual_arglist *);
try g95_check_min_max_integer(g95_actual_arglist *);
try g95_check_min_max_real(g95_actual_arglist *);
try g95_check_min_max_double(g95_actual_arglist *);
try g95_check_matmul(g95_expr *, g95_expr *);
try g95_check_merge(g95_expr *, g95_expr *, g95_expr *);
try g95_check_minloc_maxloc(g95_expr *, g95_expr *, g95_expr *);
try g95_check_minval_maxval(g95_expr *, g95_expr *, g95_expr *);
try g95_check_nearest(g95_expr *, g95_expr *);
try g95_check_null(g95_expr *);
try g95_check_pack(g95_expr *, g95_expr *, g95_expr *);
try g95_check_precision(g95_expr *);
try g95_check_present(g95_expr *);
try g95_check_product(g95_expr *, g95_expr *, g95_expr *);
try g95_check_radix(g95_expr *);
try g95_check_range(g95_expr *);
try g95_check_real(g95_expr *, g95_expr *);
try g95_check_repeat(g95_expr *, g95_expr *);
try g95_check_reshape(g95_expr *, g95_expr *, g95_expr *, g95_expr *);
try g95_check_scale(g95_expr *, g95_expr *);
try g95_check_scan(g95_expr *, g95_expr *, g95_expr *);
try g95_check_selected_real_kind(g95_expr *, g95_expr *);
try g95_check_set_exponent(g95_expr *, g95_expr *);
try g95_check_shape(g95_expr *);
try g95_check_size(g95_expr *, g95_expr *);
try g95_check_sign(g95_expr *, g95_expr *);
try g95_check_spread(g95_expr *, g95_expr *, g95_expr *);
try g95_check_sum(g95_expr *, g95_expr *, g95_expr *);
try g95_check_transfer(g95_expr *, g95_expr *, g95_expr *);
try g95_check_transpose(g95_expr *);
try g95_check_ubound(g95_expr *, g95_expr *);
try g95_check_unpack(g95_expr *, g95_expr *, g95_expr *);
try g95_check_verify(g95_expr *, g95_expr *, g95_expr *);
try g95_check_x(g95_expr *);
try g95_check_x_ni(g95_expr *);

/* intrinsic subroutines */

try g95_check_cpu_time(g95_expr *);
try g95_check_date_and_time(g95_expr *, g95_expr *, g95_expr *, g95_expr *);
try g95_check_mvbits(g95_expr *, g95_expr *, g95_expr *, g95_expr *,
		     g95_expr *);
try g95_check_random_number(g95_expr *);
try g95_check_random_seed(g95_expr *, g95_expr *, g95_expr *);

/* Prototypes for the simplification functions */

g95_expr *g95_simplify_abs(g95_expr *);
g95_expr *g95_simplify_achar(g95_expr *);
g95_expr *g95_simplify_acos(g95_expr *);
g95_expr *g95_simplify_adjustl(g95_expr *);
g95_expr *g95_simplify_adjustr(g95_expr *);
g95_expr *g95_simplify_aimag(g95_expr *);
g95_expr *g95_simplify_aint(g95_expr *, g95_expr *);
g95_expr *g95_simplify_dint(g95_expr *);
g95_expr *g95_simplify_anint(g95_expr *, g95_expr *);
g95_expr *g95_simplify_dnint(g95_expr *);
g95_expr *g95_simplify_asin(g95_expr *);
g95_expr *g95_simplify_atan(g95_expr *);
g95_expr *g95_simplify_atan2(g95_expr *, g95_expr *);
g95_expr *g95_simplify_bit_size(g95_expr *);
g95_expr *g95_simplify_btest(g95_expr *, g95_expr *);
g95_expr *g95_simplify_ceiling(g95_expr *, g95_expr *);
g95_expr *g95_simplify_char(g95_expr *, g95_expr *);
g95_expr *g95_simplify_cmplx(g95_expr *, g95_expr *, g95_expr *);
g95_expr *g95_simplify_conjg(g95_expr *);
g95_expr *g95_simplify_cos(g95_expr *);
g95_expr *g95_simplify_cosh(g95_expr *);
g95_expr *g95_simplify_dcmplx(g95_expr *, g95_expr *);
g95_expr *g95_simplify_dble(g95_expr *);
g95_expr *g95_simplify_digits(g95_expr *);
g95_expr *g95_simplify_dim(g95_expr *, g95_expr *);
g95_expr *g95_simplify_dprod(g95_expr *, g95_expr *);
g95_expr *g95_simplify_epsilon(g95_expr *);
g95_expr *g95_simplify_exp(g95_expr *);
g95_expr *g95_simplify_exponent(g95_expr *);
g95_expr *g95_simplify_float(g95_expr *);
g95_expr *g95_simplify_floor(g95_expr *, g95_expr *);
g95_expr *g95_simplify_fraction(g95_expr *);
g95_expr *g95_simplify_huge(g95_expr *);
g95_expr *g95_simplify_iachar(g95_expr *);
g95_expr *g95_simplify_iand(g95_expr *, g95_expr *);
g95_expr *g95_simplify_ibclr(g95_expr *, g95_expr *);
g95_expr *g95_simplify_ibits(g95_expr *, g95_expr *, g95_expr *);
g95_expr *g95_simplify_ibset(g95_expr *, g95_expr *);
g95_expr *g95_simplify_ichar(g95_expr *);
g95_expr *g95_simplify_ieor(g95_expr *, g95_expr *);
g95_expr *g95_simplify_index(g95_expr *, g95_expr *, g95_expr *);
g95_expr *g95_simplify_int(g95_expr *, g95_expr *);
g95_expr *g95_simplify_ifix(g95_expr *);
g95_expr *g95_simplify_idint(g95_expr *);
g95_expr *g95_simplify_ior(g95_expr *, g95_expr *);
g95_expr *g95_simplify_ishft(g95_expr *, g95_expr *);
g95_expr *g95_simplify_ishftc(g95_expr *, g95_expr *, g95_expr *);
g95_expr *g95_simplify_kind(g95_expr *);
g95_expr *g95_simplify_len(g95_expr *);
g95_expr *g95_simplify_len_trim(g95_expr *);
g95_expr *g95_simplify_lge(g95_expr *, g95_expr *);
g95_expr *g95_simplify_lgt(g95_expr *, g95_expr *);
g95_expr *g95_simplify_lle(g95_expr *, g95_expr *);
g95_expr *g95_simplify_llt(g95_expr *, g95_expr *);
g95_expr *g95_simplify_log(g95_expr *);
g95_expr *g95_simplify_log10(g95_expr *);
g95_expr *g95_simplify_logical(g95_expr *, g95_expr *);
g95_expr *g95_simplify_min(g95_expr *);
g95_expr *g95_simplify_max(g95_expr *);
g95_expr *g95_simplify_maxexponent(g95_expr *);
g95_expr *g95_simplify_minexponent(g95_expr *);
g95_expr *g95_simplify_mod(g95_expr *, g95_expr *);
g95_expr *g95_simplify_modulo(g95_expr *, g95_expr *);
g95_expr *g95_simplify_mvbits(g95_expr *, g95_expr *, g95_expr *, g95_expr *,
			      g95_expr *);
g95_expr *g95_simplify_nearest(g95_expr *, g95_expr *);
g95_expr *g95_simplify_nint(g95_expr *, g95_expr *);
g95_expr *g95_simplify_null(g95_expr *);
g95_expr *g95_simplify_idnint(g95_expr *);
g95_expr *g95_simplify_not(g95_expr *);
g95_expr *g95_simplify_precision(g95_expr *);
g95_expr *g95_simplify_radix(g95_expr *);
g95_expr *g95_simplify_range(g95_expr *);
g95_expr *g95_simplify_real(g95_expr *, g95_expr *);
g95_expr *g95_simplify_repeat(g95_expr *, g95_expr *);
g95_expr *g95_simplify_reshape(g95_expr *, g95_expr *, g95_expr *, g95_expr *);
g95_expr *g95_simplify_rrspacing(g95_expr *);
g95_expr *g95_simplify_scale(g95_expr *, g95_expr *);
g95_expr *g95_simplify_scan(g95_expr *, g95_expr *, g95_expr *);
g95_expr *g95_simplify_selected_int_kind(g95_expr *);
g95_expr *g95_simplify_selected_real_kind(g95_expr *, g95_expr *);
g95_expr *g95_simplify_set_exponent(g95_expr *, g95_expr *);
g95_expr *g95_simplify_sign(g95_expr *, g95_expr *);
g95_expr *g95_simplify_shape(g95_expr *);
g95_expr *g95_simplify_sin(g95_expr *);
g95_expr *g95_simplify_sinh(g95_expr *);
g95_expr *g95_simplify_size(g95_expr *, g95_expr *);
g95_expr *g95_simplify_sngl(g95_expr *);
g95_expr *g95_simplify_spacing(g95_expr *);
g95_expr *g95_simplify_sqrt(g95_expr *);
g95_expr *g95_simplify_tan(g95_expr *);
g95_expr *g95_simplify_tanh(g95_expr *);
g95_expr *g95_simplify_tiny(g95_expr *);
g95_expr *g95_simplify_trim(g95_expr *);
g95_expr *g95_simplify_verify(g95_expr *, g95_expr *, g95_expr *);


/* Resolution functions */

void g95_resolve_abs(g95_expr *, g95_expr *);
void g95_resolve_acos(g95_expr *, g95_expr *);
void g95_resolve_aimag(g95_expr *, g95_expr *);
void g95_resolve_aint(g95_expr *, g95_expr *, g95_expr *);
void g95_resolve_all(g95_expr *, g95_expr *, g95_expr *);
void g95_resolve_anint(g95_expr *, g95_expr *, g95_expr *);
void g95_resolve_any(g95_expr *, g95_expr *, g95_expr *);
void g95_resolve_asin(g95_expr *, g95_expr *);
void g95_resolve_atan(g95_expr *, g95_expr *);
void g95_resolve_atan2(g95_expr *, g95_expr *, g95_expr *);
void g95_resolve_btest(g95_expr *, g95_expr *, g95_expr *);
void g95_resolve_ceiling(g95_expr *, g95_expr *, g95_expr *);
void g95_resolve_char(g95_expr *, g95_expr *, g95_expr *);
void g95_resolve_cmplx(g95_expr *, g95_expr *, g95_expr *, g95_expr *);
void g95_resolve_conjg(g95_expr *, g95_expr *);
void g95_resolve_cos(g95_expr *, g95_expr *);
void g95_resolve_cosh(g95_expr *, g95_expr *);
void g95_resolve_count(g95_expr *, g95_expr *, g95_expr *);
void g95_resolve_cshift(g95_expr *, g95_expr *, g95_expr *, g95_expr *);
void g95_resolve_dble(g95_expr *, g95_expr *);
void g95_resolve_dim(g95_expr *, g95_expr *, g95_expr *);
void g95_resolve_dot_product(g95_expr *, g95_expr *, g95_expr *);
void g95_resolve_eoshift(g95_expr *, g95_expr *, g95_expr *, g95_expr *,
		         g95_expr *);
void g95_resolve_exp(g95_expr *, g95_expr *);
void g95_resolve_exponent(g95_expr *, g95_expr *);
void g95_resolve_floor(g95_expr *, g95_expr *, g95_expr *);
void g95_resolve_fraction(g95_expr *, g95_expr *);
void g95_resolve_iand(g95_expr *, g95_expr *, g95_expr *);
void g95_resolve_ibclr(g95_expr *, g95_expr *, g95_expr *);
void g95_resolve_ibits(g95_expr *, g95_expr *, g95_expr *, g95_expr *);
void g95_resolve_ibset(g95_expr *, g95_expr *, g95_expr *);
void g95_resolve_ieor(g95_expr *, g95_expr *, g95_expr *);
void g95_resolve_ichar(g95_expr *, g95_expr *);
void g95_resolve_idnint(g95_expr *, g95_expr *);
void g95_resolve_int(g95_expr *, g95_expr *, g95_expr *);
void g95_resolve_ior(g95_expr *, g95_expr *, g95_expr *);
void g95_resolve_ishft(g95_expr *, g95_expr *, g95_expr *);
void g95_resolve_ishftc(g95_expr *, g95_expr *, g95_expr *, g95_expr *);
void g95_resolve_lbound(g95_expr *, g95_expr *, g95_expr *);
void g95_resolve_len(g95_expr *, g95_expr *);
void g95_resolve_len_trim(g95_expr *, g95_expr *);
void g95_resolve_log(g95_expr *, g95_expr *);
void g95_resolve_log10(g95_expr *, g95_expr *);
void g95_resolve_logical(g95_expr *, g95_expr *, g95_expr *);
void g95_resolve_matmul(g95_expr *, g95_expr *, g95_expr *);
void g95_resolve_max(g95_expr *, g95_expr *);
void g95_resolve_maxloc(g95_expr *, g95_expr *, g95_expr *, g95_expr *);
void g95_resolve_maxval(g95_expr *, g95_expr *, g95_expr *, g95_expr *);
void g95_resolve_merge(g95_expr *, g95_expr *, g95_expr *, g95_expr *);
void g95_resolve_min(g95_expr *, g95_expr *);
void g95_resolve_minloc(g95_expr *, g95_expr *, g95_expr *, g95_expr *);
void g95_resolve_minval(g95_expr *, g95_expr *, g95_expr *, g95_expr *);
void g95_resolve_mod(g95_expr *, g95_expr *, g95_expr *);
void g95_resolve_modulo(g95_expr *, g95_expr *, g95_expr *);
void g95_resolve_nint(g95_expr *, g95_expr *, g95_expr *);
void g95_resolve_not(g95_expr *, g95_expr *);
void g95_resolve_pack(g95_expr *, g95_expr *, g95_expr *, g95_expr *);
void g95_resolve_product(g95_expr *, g95_expr *, g95_expr *, g95_expr *);
void g95_resolve_real(g95_expr *, g95_expr *, g95_expr *);
void g95_resolve_repeat(g95_expr *, g95_expr *, g95_expr *);
void g95_resolve_reshape(g95_expr *, g95_expr *, g95_expr *, g95_expr *,
			 g95_expr *);
void g95_resolve_rrspacing(g95_expr *, g95_expr *);
void g95_resolve_scale(g95_expr *, g95_expr *, g95_expr *);
void g95_resolve_scan(g95_expr *, g95_expr *, g95_expr *, g95_expr *);
void g95_resolve_set_exponent(g95_expr *, g95_expr *, g95_expr *);
void g95_resolve_sign(g95_expr *, g95_expr *, g95_expr *);
void g95_resolve_sin(g95_expr *, g95_expr *);
void g95_resolve_sinh(g95_expr *, g95_expr *);
void g95_resolve_spacing(g95_expr *, g95_expr *);
void g95_resolve_spread(g95_expr *, g95_expr *, g95_expr *, g95_expr *);
void g95_resolve_sqrt(g95_expr *, g95_expr *);
void g95_resolve_sum(g95_expr *, g95_expr *, g95_expr *, g95_expr *);
void g95_resolve_tan(g95_expr *, g95_expr *);
void g95_resolve_tanh(g95_expr *, g95_expr *);
void g95_resolve_transfer(g95_expr *, g95_expr *, g95_expr *, g95_expr *);
void g95_resolve_transpose(g95_expr *, g95_expr *);
void g95_resolve_trim(g95_expr *, g95_expr *);
void g95_resolve_ubound(g95_expr *, g95_expr *, g95_expr *);
void g95_resolve_unpack(g95_expr *, g95_expr *, g95_expr *, g95_expr *);
void g95_resolve_verify(g95_expr *, g95_expr *, g95_expr *, g95_expr *);

/* Enumeration of all the generic intrinsic functions.  Used by the backend
   for identification of a function.  */
enum g95_generic_isym_id
{
  /* G95_ISYM_NONE is used for intrinsics which will never be seen by the
     backend (eg. KIND).  */
  G95_ISYM_NONE = 0,
  G95_ISYM_ABS,
  G95_ISYM_ACHAR,
  G95_ISYM_ACOS,
  G95_ISYM_ADJUSTL,
  G95_ISYM_ADJUSTR,
  G95_ISYM_AIMAG,
  G95_ISYM_AINT,
  G95_ISYM_ANINT,
  G95_ISYM_ALL,
  G95_ISYM_ALLOCATED,
  G95_ISYM_ANINIT,
  G95_ISYM_ANY,
  G95_ISYM_ASIN,
  G95_ISYM_ASSOCIATED,
  G95_ISYM_ATAN,
  G95_ISYM_ATAN2,
  G95_ISYM_BTEST,
  G95_ISYM_CEILING,
  G95_ISYM_CHAR,
  G95_ISYM_CMPLX,
  G95_ISYM_CONJG,
  G95_ISYM_COS,
  G95_ISYM_COSH,
  G95_ISYM_COUNT,
  G95_ISYM_CPU_TIME,
  G95_ISYM_CSHIFT,
  G95_ISYM_DATE_AND_TIME,
  G95_ISYM_DBLE,
  G95_ISYM_DIM,
  G95_ISYM_DOT_PRODUCT,
  G95_ISYM_DPROD,
  G95_ISYM_EOSHIFT,
  G95_ISYM_EXP,
  G95_ISYM_EXPONENT,
  G95_ISYM_FLOOR,
  G95_ISYM_FRACTION,
  G95_ISYM_IACHAR,
  G95_ISYM_IAND,
  G95_ISYM_IBCLR,
  G95_ISYM_IBITS,
  G95_ISYM_IBSET,
  G95_ISYM_ICHAR,
  G95_ISYM_IEOR,
  G95_ISYM_INDEX,
  G95_ISYM_INT,
  G95_ISYM_IOR,
  G95_ISYM_ISHFT,
  G95_ISYM_ISHFTC,
  G95_ISYM_LBOUND,
  G95_ISYM_LEN,
  G95_ISYM_LEN_TRIM,
  G95_ISYM_LGE,
  G95_ISYM_LGT,
  G95_ISYM_LLE,
  G95_ISYM_LLT,
  G95_ISYM_LOG,
  G95_ISYM_LOG10,
  G95_ISYM_LOGICAL,
  G95_ISYM_MATMUL,
  G95_ISYM_MAX,
  G95_ISYM_MAXLOC,
  G95_ISYM_MAXVAL,
  G95_ISYM_MERGE,
  G95_ISYM_MIN,
  G95_ISYM_MINLOC,
  G95_ISYM_MINVAL,
  G95_ISYM_MOD,
  G95_ISYM_MODULO,
  G95_ISYM_MVBITS,
  G95_ISYM_NEAREST,
  G95_ISYM_NINT,
  G95_ISYM_NOT,
  G95_ISYM_PACK,
  G95_ISYM_PRESENT,
  G95_ISYM_PRODUCT,
  G95_ISYM_RANDOM_NUMBER,
  G95_ISYM_RANDOM_SEED,
  G95_ISYM_REAL,
  G95_ISYM_REPEAT,
  G95_ISYM_RESHAPE,
  G95_ISYM_SCAN,
  G95_ISYM_SET_EXPONENT,
  G95_ISYM_SHAPE,
  G95_ISYM_SIGN,
  G95_ISYM_SIN,
  G95_ISYM_SINH,
  G95_ISYM_SIZE,
  G95_ISYM_SPREAD,
  G95_ISYM_SQRT,
  G95_ISYM_SUM,
  G95_ISYM_SYSTEM_CLOCK,
  G95_ISYM_TAN,
  G95_ISYM_TANH,
  G95_ISYM_TRANSFER,
  G95_ISYM_TRANSPOSE,
  G95_ISYM_TRIM,
  G95_ISYM_UBOUND,
  G95_ISYM_UNPACK,
  G95_ISYM_VERIFY,
  G95_ISYM_CONVERSION
};

/* The mvbits() subroutine requires the most arguments-- five. */

#define MAX_INTRINSIC_ARGS 5

extern int g95_intrinsic_extension;
extern char *g95_current_intrinsic,
  *g95_current_intrinsic_arg[MAX_INTRINSIC_ARGS];
extern locus *g95_current_intrinsic_where;
