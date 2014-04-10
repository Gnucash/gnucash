#ifndef __GO_MATH_H
#define __GO_MATH_H

#include <math.h>
#ifdef HAVE_IEEEFP_H
#include <ieeefp.h>
#endif
#ifdef HAVE_IEEE754_H
#include <ieee754.h>
#endif

#include "config.h"

/* What a circus!  */
#ifdef HAVE_FINITE
#define go_finite finite
#elif defined(HAVE_ISFINITE)
#define go_finite isfinite
#elif defined(FINITE)
#define go_finite FINITE
#else
#error "I don't know an equivalent of finite for your system; you lose"
#endif

/* ------------------------------------------------------------------------- */

extern double go_nan;
extern double go_pinf;
extern double go_ninf;

/* ------------------------------------------------------------------------- */

double go_add_epsilon (double x);
double go_sub_epsilon (double x);

/* ------------------------------------------------------------------------- */

double go_fake_floor (double x);
double go_fake_ceil (double x);
double go_fake_trunc (double x);

/* ------------------------------------------------------------------------- */

void go_math_init (void);

/* ------------------------------------------------------------------------- */

#endif	/* __GO_MATH_H */
