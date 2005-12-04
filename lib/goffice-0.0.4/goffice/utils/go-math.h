#ifndef __GO_MATH_H
#define __GO_MATH_H

#include <math.h>
#include <goffice/goffice-features.h>
#include <glib/gmacros.h>

G_BEGIN_DECLS

/* ------------------------------------------------------------------------- */

void go_math_init (void);
void go_continued_fraction (double val, int max_denom, int *res_num, int *res_denom);
void go_stern_brocot	   (double val, int max_denom, int *res_num, int *res_denom);

/* ------------------------------------------------------------------------- */

extern double go_nan;
extern double go_pinf;
extern double go_ninf;

double go_add_epsilon (double x);
double go_sub_epsilon (double x);
double go_fake_floor (double x);
double go_fake_ceil (double x);
double go_fake_round (double x);
double go_fake_trunc (double x);
double go_rint (double x);

int go_finite (double x);
double go_pow2 (int n);
double go_pow10 (int n);

/*
 * We provide working versions of these functions for doubles.
 */ 
#ifdef GOFFICE_SUPPLIED_ASINH
double asinh (double x);
#endif
#ifdef GOFFICE_SUPPLIED_ACOSH
double acosh (double x);
#endif
#ifdef GOFFICE_SUPPLIED_ATANH
double atanh (double x);
#endif
#ifdef GOFFICE_SUPPLIED_LOG1P
double log1p (double x);
#endif
#ifdef GOFFICE_SUPPLIED_EXPM1
double expm1 (double x);
#endif

/* ------------------------------------------------------------------------- */

#ifdef GOFFICE_WITH_LONG_DOUBLE

extern long double go_nanl;
extern long double go_pinfl;
extern long double go_ninfl;

long double go_add_epsilonl (long double x);
long double go_sub_epsilonl (long double x);
long double go_fake_floorl (long double x);
long double go_fake_ceill (long double x);
long double go_fake_roundl (long double x);
long double go_fake_truncl (long double x);

#define go_finitel finitel
long double go_pow2l (int n);
long double go_pow10l (int n);

/*
 * We provide working versions of these functions for long doubles.
 */ 
#ifdef GOFFICE_SUPPLIED_LDEXPL
long double ldexpl (long double x, int exp);
#endif
#ifdef GOFFICE_SUPPLIED_FREXPL
long double frexpl (long double x, int *exp);
#endif
#ifdef GOFFICE_SUPPLIED_STRTOLD
long double strtold (const char *, char **);
#endif
#ifdef GOFFICE_SUPPLIED_MODFL
long double modfl (long double x, long double *iptr);
#endif

#endif

/* ------------------------------------------------------------------------- */

G_END_DECLS

#endif	/* __GO_MATH_H */
