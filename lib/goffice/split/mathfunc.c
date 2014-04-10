/*
 * mathfunc.c:  Mathematical functions.
 *
 * Authors:
 *   Ross Ihaka.  (See note 1.)
 *   The R Development Core Team.  (See note 1.)
 *   Morten Welinder <terra@gnome.org>
 *   Miguel de Icaza (miguel@gnu.org)
 *   Jukka-Pekka Iivonen (iivonen@iki.fi)
 *   James Theiler.  (See note 2.)
 *   Brian Gough.  (See note 2.)
 *   Makoto Matsumoto and Takuji Nishimura (Mersenne Twister, see note in code)
 *   Ian Smith (iandjmsmith@aol.com).  (See note 3.)
 */

/*
 * NOTE 1: most of this file comes from the "R" package, notably version 2
 * or newer (we re-sync from time to time).
 * "R" is distributed under GPL licence, see file COPYING.
 * The relevant parts are copyright (C) 1998 Ross Ihaka and
 * 2000-2004 The R Development Core Team.
 *
 * Thank you!
 */

/*
 * NOTE 2: most of the random distribution code comes from the GNU Scientific
 * Library (GSL), notably version 1.1.1.  GSL is distributed under GPL licence,
 * see COPYING. The relevant parts are copyright (C) 1996, 1997, 1998, 1999,
 * 2000 James Theiler and Brian Gough.
 *
 * Thank you!
 */

/*
 * NOTE 3: the pbeta (and support) code comes from Ian Smith.  (Translated
 * into C, adapted to Gnumeric naming convensions, and R's API conventions
 * by Morten Welinder.  Blame me for problems.)
 *
 * Copyright © Ian Smith 2002-2003
 * Version 1.0.24
 * Thanks to Jerry W. Lewis for help with testing of and improvements to the code. 
 *
 * Thank you!
 */


/* for random() */
#define _SVID_SOURCE 1
#define _BSD_SOURCE 1

#include <config.h>
#include "gnumeric.h"
#include "mathfunc.h"

#include <math.h>
#include <errno.h>
#include <stdlib.h>
#include <stdio.h>
#include <float.h>
#include <fcntl.h>
#include <unistd.h>
#include <locale.h>
#include <string.h>
#include <goffice/utils/go-math.h>

#if defined (HAVE_IEEEFP_H) || defined (HAVE_IEEE754_H)
/* Make sure we have this symbol defined, since the existance of either
   header file implies it.  */
#ifndef IEEE_754
#define IEEE_754
#endif
#endif

#define M_LN_SQRT_2PI   GNM_const(0.918938533204672741780329736406)  /* log(sqrt(2*pi)) */
#define M_SQRT_32       GNM_const(5.656854249492380195206754896838)  /* sqrt(32) */
#define M_1_SQRT_2PI    GNM_const(0.398942280401432677939946059934)  /* 1/sqrt(2pi) */
#define M_SQRT_2dPI     GNM_const(0.797884560802865355879892119869)  /* sqrt(2/pi) */
#define M_2PIgnum       (2 * M_PIgnum)
#define	M_Egnum         GNM_const(2.718281828459045235360287471352662497757247)

#define ML_UNDERFLOW (GNUM_EPSILON * GNUM_EPSILON)
#define ML_ERROR(cause) /* Nothing */
#define MATHLIB_ERROR g_error
#define MATHLIB_WARNING g_warning
#define MATHLIB_WARNING2 g_warning
#define MATHLIB_WARNING4 g_warning

static inline gnm_float fmin2 (gnm_float x, gnm_float y) { return MIN (x, y); }
static inline gnm_float fmax2 (gnm_float x, gnm_float y) { return MAX (x, y); }
static inline int imin2 (int x, int y) { return MIN (x, y); }
static inline int imax2 (int x, int y) { return MAX (x, y); }

#define MATHLIB_STANDALONE
#define ML_ERR_return_NAN { return gnm_nan; }
static void pnorm_both (gnm_float x, gnm_float *cum, gnm_float *ccum, int i_tail, gboolean log_p);

#define SQR(x) ((x)*(x))
/* Scale factor for continued fractions.  ==2^256.  */
static const gnm_float scalefactor = SQR(SQR(SQR(GNM_const(4294967296.0))));
#undef SQR

/* MW ---------------------------------------------------------------------- */

gnm_float gnm_nan;
gnm_float gnm_pinf;
gnm_float gnm_ninf;

void
mathfunc_init (void)
{
	const char *bug_url = "http://bugzilla.gnome.org/enter_bug.cgi?product=gnumeric";

	gnm_pinf = go_pinf;
	if (finitegnum (gnm_pinf) || !(gnm_pinf > 0)) {
		g_error ("Failed to generate +Inf.  Please report at %s",
			 bug_url);
		abort ();
	}

	gnm_ninf = go_ninf;
	if (finitegnum (gnm_ninf) || !(gnm_ninf < 0)) {
		g_error ("Failed to generate -Inf.  Please report at %s",
			 bug_url);
		abort ();
	}

	gnm_nan = go_nan;
	if (!isnangnum (gnm_nan)) {
		g_error ("Failed to generate NaN.  Please report at %s",
			 bug_url);
		abort ();
	}
}

/*
 * In preparation for truncation, make the value a tiny bit larger (seen
 * absolutely).  This makes ROUND (etc.) behave a little closer to what
 * people want, even if it is a bit bogus.
 */
gnm_float
gnumeric_add_epsilon (gnm_float x)
{
	if (!finitegnum (x) || x == 0)
		return x;
	else {
		int exp;
		gnm_float mant = frexpgnum (gnumabs (x), &exp);
		gnm_float absres = ldexpgnum (mant + GNUM_EPSILON, exp);
		return (x < 0) ? -absres : absres;
	}
}

gnm_float
gnumeric_sub_epsilon (gnm_float x)
{
	if (!finitegnum (x) || x == 0)
		return x;
	else {
		int exp;
		gnm_float mant = frexpgnum (gnumabs (x), &exp);
		gnm_float absres = ldexpgnum (mant - GNUM_EPSILON, exp);
		return (x < 0) ? -absres : absres;
	}
}

gnm_float
gnumeric_fake_floor (gnm_float x)
{
	return floorgnum (gnumeric_add_epsilon (x));
}

gnm_float
gnumeric_fake_ceil (gnm_float x)
{
	return ceilgnum (gnumeric_sub_epsilon (x));
}

gnm_float
gnumeric_fake_round (gnm_float x)
{
	return (x >= 0)
		? gnumeric_fake_floor (x + 0.5)
		: -gnumeric_fake_floor (-x + 0.5);
}

gnm_float
gnumeric_fake_trunc (gnm_float x)
{
	return (x >= 0)
		? gnumeric_fake_floor (x)
		: -gnumeric_fake_floor (-x);
}

/* ------------------------------------------------------------------------- */
/* --- BEGIN MAGIC R SOURCE MARKER --- */

/* The following source code was imported from the R project.  */
/* It was automatically transformed by tools/import-R.  */

/* Imported src/nmath/dpq.h from R.  */
	/* Utilities for `dpq' handling (density/probability/quantile) */

/* give_log in "d";  log_p in "p" & "q" : */
#define give_log log_p
							/* "DEFAULT" */
							/* --------- */
#define R_D__0	(log_p ? gnm_ninf : 0.)		/* 0 */
#define R_D__1	(log_p ? 0. : 1.)			/* 1 */
#define R_DT_0	(lower_tail ? R_D__0 : R_D__1)		/* 0 */
#define R_DT_1	(lower_tail ? R_D__1 : R_D__0)		/* 1 */

#define R_D_Lval(p)	(lower_tail ? (p) : (1 - (p)))	/*  p  */
#define R_D_Cval(p)	(lower_tail ? (1 - (p)) : (p))	/*  1 - p */

#define R_D_val(x)	(log_p	? loggnum(x) : (x))		/*  x  in pF(x,..) */
#define R_D_qIv(p)	(log_p	? expgnum(p) : (p))		/*  p  in qF(p,..) */
#define R_D_exp(x)	(log_p	?  (x)	 : expgnum(x))	/* expgnum(x) */
#define R_D_log(p)	(log_p	?  (p)	 : loggnum(p))	/* loggnum(p) */
#define R_D_Clog(p)	(log_p	? log1pgnum(-(p)) : (1 - (p)))/* [log](1-p) */

/* loggnum(1-expgnum(x)):  R_D_LExp(x) == (log1pgnum(- R_D_qIv(x))) but even more stable:*/
#define R_D_LExp(x)     (log_p ? swap_log_tail(x) : log1pgnum(-x))

/*till 1.8.x:
 * #define R_DT_val(x)	R_D_val(R_D_Lval(x))
 * #define R_DT_Cval(x)	R_D_val(R_D_Cval(x)) */
#define R_DT_val(x)	(lower_tail ? R_D_val(x)  : R_D_Clog(x))
#define R_DT_Cval(x)	(lower_tail ? R_D_Clog(x) : R_D_val(x))

/*#define R_DT_qIv(p)	R_D_Lval(R_D_qIv(p))		 *  p  in qF ! */
#define R_DT_qIv(p)	(log_p ? (lower_tail ? expgnum(p) : - expm1gnum(p)) \
			       : R_D_Lval(p))

/*#define R_DT_CIv(p)	R_D_Cval(R_D_qIv(p))		 *  1 - p in qF */
#define R_DT_CIv(p)	(log_p ? (lower_tail ? -expm1gnum(p) : expgnum(p)) \
			       : R_D_Cval(p))

#define R_DT_exp(x)	R_D_exp(R_D_Lval(x))		/* expgnum(x) */
#define R_DT_Cexp(x)	R_D_exp(R_D_Cval(x))		/* expgnum(1 - x) */

#define R_DT_log(p)	(lower_tail? R_D_log(p) : R_D_LExp(p))/* loggnum(p) in qF */
#define R_DT_Clog(p)	(lower_tail? R_D_LExp(p): R_D_log(p))/* log1pgnum (-p) in qF*/
#define R_DT_Log(p)	(lower_tail? (p) : swap_log_tail(p))
/* ==   R_DT_log when we already "know" log_p == TRUE :*/


#define R_Q_P01_check(p)			\
    if ((log_p	&& p > 0) ||			\
	(!log_p && (p < 0 || p > 1)) )		\
	ML_ERR_return_NAN


/* additions for density functions (C.Loader) */
#define R_D_fexp(f,x)     (give_log ? -0.5*loggnum(f)+(x) : expgnum(x)/sqrtgnum(f))
#define R_D_forceint(x)   floorgnum((x) + 0.5)
#define R_D_nonint(x) 	  (gnumabs((x) - floorgnum((x)+0.5)) > 1e-7)
/* [neg]ative or [non int]eger : */
#define R_D_negInonint(x) (x < 0. || R_D_nonint(x))

#define R_D_nonint_check(x) 				\
   if(R_D_nonint(x)) {					\
	MATHLIB_WARNING("non-integer x = %" GNUM_FORMAT_f "", x);	\
	return R_D__0;					\
   }

/* ------------------------------------------------------------------------ */
/* Imported src/nmath/ftrunc.c from R.  */
/*
 *  Mathlib : A C Library of Special Functions
 *  Copyright (C) 1998 Ross Ihaka
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307 USA.
 *
 *  SYNOPSIS
 *
 *    #include <Rmath.h>
 *    double ftrunc(double x);
 *
 *  DESCRIPTION
 *
 *    Truncation toward zero.
 */


gnm_float gnm_trunc(gnm_float x)
{
	if(x >= 0) return floorgnum(x);
	else return ceilgnum(x);
}

/* ------------------------------------------------------------------------ */
/* Imported src/nmath/dnorm.c from R.  */
/*
 *  Mathlib : A C Library of Special Functions
 *  Copyright (C) 1998 Ross Ihaka
 *  Copyright (C) 2000	    The R Development Core Team
 *  Copyright (C) 2003	    The R Foundation
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307 USA.
 *
 *  SYNOPSIS
 *
 *	double dnorm4(double x, double mu, double sigma, int give_log)
 *	      {dnorm (..) is synonymous and preferred inside R}
 *
 *  DESCRIPTION
 *
 *	Compute the density of the normal distribution.
 */


gnm_float dnorm(gnm_float x, gnm_float mu, gnm_float sigma, gboolean give_log)
{
#ifdef IEEE_754
    if (isnangnum(x) || isnangnum(mu) || isnangnum(sigma))
	return x + mu + sigma;
#endif
    if(!finitegnum(sigma)) return R_D__0;
    if(!finitegnum(x) && mu == x) return gnm_nan;/* x-mu is NaN */
    if (sigma <= 0) {
	if (sigma < 0) ML_ERR_return_NAN;
	/* sigma == 0 */
	return (x == mu) ? gnm_pinf : R_D__0;
    }
    x = (x - mu) / sigma;

    if(!finitegnum(x)) return R_D__0;
    return (give_log ?
	    -(M_LN_SQRT_2PI  +	0.5 * x * x + loggnum(sigma)) :
	    M_1_SQRT_2PI * expgnum(-0.5 * x * x)  /	  sigma);
    /* M_1_SQRT_2PI = 1 / sqrtgnum(2 * pi) */
}

/* ------------------------------------------------------------------------ */
/* Imported src/nmath/pnorm.c from R.  */
/*
 *  Mathlib : A C Library of Special Functions
 *  Copyright (C) 1998	    Ross Ihaka
 *  Copyright (C) 2000-2002 The R Development Core Team
 *  Copyright (C) 2003	    The R Foundation
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307 USA.
 *
 *  SYNOPSIS
 *
 *   #include <Rmath.h>
 *
 *   double pnorm5(double x, double mu, double sigma, int lower_tail,int log_p);
 *	   {pnorm (..) is synonymous and preferred inside R}
 *
 *   void   pnorm_both(double x, double *cum, double *ccum,
 *		       int i_tail, int log_p);
 *
 *  DESCRIPTION
 *
 *	The main computation evaluates near-minimax approximations derived
 *	from those in "Rational Chebyshev approximations for the error
 *	function" by W. J. Cody, Math. Comp., 1969, 631-637.  This
 *	transportable program uses rational functions that theoretically
 *	approximate the normal distribution function to at least 18
 *	significant decimal digits.  The accuracy achieved depends on the
 *	arithmetic system, the compiler, the intrinsic functions, and
 *	proper selection of the machine-dependent constants.
 *
 *  REFERENCE
 *
 *	Cody, W. D. (1993).
 *	ALGORITHM 715: SPECFUN - A Portable FORTRAN Package of
 *	Special Function Routines and Test Drivers".
 *	ACM Transactions on Mathematical Software. 19, 22-32.
 *
 *  EXTENSIONS
 *
 *  The "_both" , lower, upper, and log_p  variants were added by
 *  Martin Maechler, Jan.2000;
 *  as well as log1p() and similar improvements later on.
 *
 *  James M. Rath contributed bug report PR#699 and patches correcting SIXTEN
 *  and if() clauses {with a bug: "|| instead of &&" -> PR #2883) more in line
 *  with the original Cody code.
 */

gnm_float pnorm(gnm_float x, gnm_float mu, gnm_float sigma, gboolean lower_tail, gboolean log_p)
{
    gnm_float p, cp = 0;

    /* Note: The structure of these checks has been carefully thought through.
     * For example, if x == mu and sigma == 0, we get the correct answer 1.
     */
#ifdef IEEE_754
    if(isnangnum(x) || isnangnum(mu) || isnangnum(sigma))
	return x + mu + sigma;
#endif
    if(!finitegnum(x) && mu == x) return gnm_nan;/* x-mu is NaN */
    if (sigma <= 0) {
	if(sigma < 0) ML_ERR_return_NAN;
	/* sigma = 0 : */
	return (x < mu) ? R_DT_0 : R_DT_1;
    }
    p = (x - mu) / sigma;
    if(!finitegnum(p))
	return (x < mu) ? R_DT_0 : R_DT_1;
    x = p;

    pnorm_both(x, &p, &cp, (lower_tail ? 0 : 1), log_p);

    return(lower_tail ? p : cp);
}

#define SIXTEN	16 /* Cutoff allowing exact "*" and "/" */

void pnorm_both(gnm_float x, gnm_float *cum, gnm_float *ccum, int i_tail, gboolean log_p)
{
/* i_tail in {0,1,2} means: "lower", "upper", or "both" :
   if(lower) return  *cum := P[X <= x]
   if(upper) return *ccum := P[X >  x] = 1 - P[X <= x]
*/
    static const gnm_float a[5] = {
	GNM_const(2.2352520354606839287),
	GNM_const(161.02823106855587881),
	GNM_const(1067.6894854603709582),
	GNM_const(18154.981253343561249),
	GNM_const(0.065682337918207449113)
    };
    static const gnm_float b[4] = {
	GNM_const(47.20258190468824187),
	GNM_const(976.09855173777669322),
	GNM_const(10260.932208618978205),
	GNM_const(45507.789335026729956)
    };
    static const gnm_float c[9] = {
	GNM_const(0.39894151208813466764),
	GNM_const(8.8831497943883759412),
	GNM_const(93.506656132177855979),
	GNM_const(597.27027639480026226),
	GNM_const(2494.5375852903726711),
	GNM_const(6848.1904505362823326),
	GNM_const(11602.651437647350124),
	GNM_const(9842.7148383839780218),
	GNM_const(1.0765576773720192317e-8)
    };
    static const gnm_float d[8] = {
	GNM_const(22.266688044328115691),
	GNM_const(235.38790178262499861),
	GNM_const(1519.377599407554805),
	GNM_const(6485.558298266760755),
	GNM_const(18615.571640885098091),
	GNM_const(34900.952721145977266),
	GNM_const(38912.003286093271411),
	GNM_const(19685.429676859990727)
    };
    static const gnm_float p[6] = {
	GNM_const(0.21589853405795699),
	GNM_const(0.1274011611602473639),
	GNM_const(0.022235277870649807),
	GNM_const(0.001421619193227893466),
	GNM_const(2.9112874951168792e-5),
	GNM_const(0.02307344176494017303)
    };
    static const gnm_float q[5] = {
	GNM_const(1.28426009614491121),
	GNM_const(0.468238212480865118),
	GNM_const(0.0659881378689285515),
	GNM_const(0.00378239633202758244),
	GNM_const(7.29751555083966205e-5)
    };

    gnm_float xden, xnum, temp, del, eps, xsq, y;
#ifdef NO_DENORMS
    gnm_float min = GNUM_MIN;
#endif
    int i, lower, upper;

#ifdef IEEE_754
    if(isnangnum(x)) { *cum = *ccum = x; return; }
#endif

    /* Consider changing these : */
    eps = GNUM_EPSILON * 0.5;

    /* i_tail in {0,1,2} =^= {lower, upper, both} */
    lower = i_tail != 1;
    upper = i_tail != 0;

    y = gnumabs(x);
    if (y <= 0.67448975) { /* qnorm(3/4) = .6744.... -- earlier had 0.66291 */
	if (y > eps) {
	    xsq = x * x;
	    xnum = a[4] * xsq;
	    xden = xsq;
	    for (i = 0; i < 3; ++i) {
		xnum = (xnum + a[i]) * xsq;
		xden = (xden + b[i]) * xsq;
	    }
	} else xnum = xden = 0.0;

	temp = x * (xnum + a[3]) / (xden + b[3]);
	if(lower)  *cum = 0.5 + temp;
	if(upper) *ccum = 0.5 - temp;
	if(log_p) {
	    if(lower)  *cum = loggnum(*cum);
	    if(upper) *ccum = loggnum(*ccum);
	}
    }
    else if (y <= M_SQRT_32) {

	/* Evaluate pnorm for 0.674.. = qnorm(3/4) < |x| <= sqrtgnum(32) ~= 5.657 */

	xnum = c[8] * y;
	xden = y;
	for (i = 0; i < 7; ++i) {
	    xnum = (xnum + c[i]) * y;
	    xden = (xden + d[i]) * y;
	}
	temp = (xnum + c[7]) / (xden + d[7]);

#define do_del(X)							\
	xsq = gnm_trunc(X * SIXTEN) / SIXTEN;				\
	del = (X - xsq) * (X + xsq);					\
	if(log_p) {							\
	    *cum = (-xsq * xsq * 0.5) + (-del * 0.5) + loggnum(temp);	\
	    if((lower && x > 0.) || (upper && x <= 0.))			\
		  *ccum = log1pgnum(-expgnum(-xsq * xsq * 0.5) *		\
				expgnum(-del * 0.5) * temp);		\
	}								\
	else {								\
	    *cum = expgnum(-xsq * xsq * 0.5) * expgnum(-del * 0.5) * temp;	\
	    *ccum = 1.0 - *cum;						\
	}

#define swap_tail						\
	if (x > 0.) {/* swap  ccum <--> cum */			\
	    temp = *cum; if(lower) *cum = *ccum; *ccum = temp;	\
	}

	do_del(y);
	swap_tail;
    }

/* else	  |x| > sqrtgnum(32) = 5.657 :
 * the next two case differentiations were really for lower=T, log=F
 * Particularly	 *not*	for  log_p !

 * Cody had (-37.5193 < x  &&  x < 8.2924) ; R originally had y < 50
 *
 * Note that we do want symmetry(0), lower/upper -> hence use y
 */
    else if(log_p
	/*  ^^^^^ MM FIXME: can speedup for log_p and much larger |x| !
	 * Then, make use of  Abramowitz & Stegun, 26.2.13, something like

	 xsq = x*x;

	 if(xsq * GNUM_EPSILON < 1.)
	    del = (1. - (1. - 5./(xsq+6.)) / (xsq+4.)) / (xsq+2.);
	 else
	    del = 0.;
	 *cum = -.5*xsq - M_LN_SQRT_2PI - loggnum(x) + log1pgnum(-del);
	 *ccum = log1pgnum(-expgnum(*cum)); /.* ~ loggnum(1) = 0 *./

 	 swap_tail;

	*/
	    || (lower && -37.5193 < x  &&  x < 8.2924)
	    || (upper && -8.2924  < x  &&  x < 37.5193)
	) {

	/* Evaluate pnorm for x in (-37.5, -5.657) union (5.657, 37.5) */
	xsq = 1.0 / (x * x);
	xnum = p[5] * xsq;
	xden = xsq;
	for (i = 0; i < 4; ++i) {
	    xnum = (xnum + p[i]) * xsq;
	    xden = (xden + q[i]) * xsq;
	}
	temp = xsq * (xnum + p[4]) / (xden + q[4]);
	temp = (M_1_SQRT_2PI - temp) / y;

	do_del(x);
	swap_tail;
    }
    else { /* no log_p , large x such that probs are 0 or 1 */
	if(x > 0) {	*cum = 1.; *ccum = 0.;	}
	else {	        *cum = 0.; *ccum = 1.;	}
    }


#ifdef NO_DENORMS
    /* do not return "denormalized" -- we do in R */
    if(log_p) {
	if(*cum > -min)	 *cum = -0.;
	if(*ccum > -min)*ccum = -0.;
    }
    else {
	if(*cum < min)	 *cum = 0.;
	if(*ccum < min)	*ccum = 0.;
    }
#endif
    return;
}
/* Cleaning up done by tools/import-R:  */
#undef SIXTEN
#undef do_del
#undef swap_tail

/* ------------------------------------------------------------------------ */
/* Imported src/nmath/qnorm.c from R.  */
/*
 *  Mathlib : A C Library of Special Functions
 *  Copyright (C) 1998 Ross Ihaka
 *  Copyright (C) 2000 The R Development Core Team
 *  based on AS 111 (C) 1977 Royal Statistical Society
 *  and   on AS 241 (C) 1988 Royal Statistical Society
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307 USA.
 *
 *  SYNOPSIS
 *
 *	double qnorm5(double p, double mu, double sigma,
 *		      int lower_tail, int log_p)
 *            {qnorm (..) is synonymous and preferred inside R}
 *
 *  DESCRIPTION
 *
 *	Compute the quantile function for the normal distribution.
 *
 *	For small to moderate probabilities, algorithm referenced
 *	below is used to obtain an initial approximation which is
 *	polished with a final Newton step.
 *
 *	For very large arguments, an algorithm of Wichura is used.
 *
 *  REFERENCE
 *
 *	Beasley, J. D. and S. G. Springer (1977).
 *	Algorithm AS 111: The percentage points of the normal distribution,
 *	Applied Statistics, 26, 118-121.
 *
 *      Wichura, M.J. (1988).
 *      Algorithm AS 241: The Percentage Points of the Normal Distribution.
 *      Applied Statistics, 37, 477-484.
 */


gnm_float qnorm(gnm_float p, gnm_float mu, gnm_float sigma, gboolean lower_tail, gboolean log_p)
{
    gnm_float p_, q, r, val;

#ifdef IEEE_754
    if (isnangnum(p) || isnangnum(mu) || isnangnum(sigma))
	return p + mu + sigma;
#endif
    if (p == R_DT_0)	return gnm_ninf;
    if (p == R_DT_1)	return gnm_pinf;
    R_Q_P01_check(p);

    if(sigma  < 0)	ML_ERR_return_NAN;
    if(sigma == 0)	return mu;

    p_ = R_DT_qIv(p);/* real lower_tail prob. p */
    q = p_ - 0.5;

#ifdef DEBUG_qnorm
    REprintf("qnorm(p=%10.7" GNUM_FORMAT_g ", m=%" GNUM_FORMAT_g ", s=%" GNUM_FORMAT_g ", l.t.= %d, log= %d): q = %" GNUM_FORMAT_g "\n",
	     p,mu,sigma, lower_tail, log_p, q);
#endif


/*-- use AS 241 --- */
/* gnm_float ppnd16_(gnm_float *p, long *ifault)*/
/*      ALGORITHM AS241  APPL. STATIST. (1988) VOL. 37, NO. 3

        Produces the normal deviate Z corresponding to a given lower
        tail area of P; Z is accurate to about 1 part in 10**16.

        (original fortran code used PARAMETER(..) for the coefficients
         and provided hash codes for checking them...)
*/
    if (gnumabs(q) <= .425) {/* 0.075 <= p <= 0.925 */
        r = .180625 - q * q;
	val =
            q * (((((((r * GNM_const(2509.0809287301226727) +
                       GNM_const(33430.575583588128105)) * r + GNM_const(67265.770927008700853)) * r +
                     GNM_const(45921.953931549871457)) * r + GNM_const(13731.693765509461125)) * r +
                   GNM_const(1971.5909503065514427)) * r + GNM_const(133.14166789178437745)) * r +
                 GNM_const(3.387132872796366608))
            / (((((((r * GNM_const(5226.495278852854561) +
                     GNM_const(28729.085735721942674)) * r + GNM_const(39307.89580009271061)) * r +
                   GNM_const(21213.794301586595867)) * r + GNM_const(5394.1960214247511077)) * r +
                 GNM_const(687.1870074920579083)) * r + GNM_const(42.313330701600911252)) * r + 1.);
    }
    else { /* closer than 0.075 from {0,1} boundary */

	/* r = min(p, 1-p) < 0.075 */
	if (q > 0)
	    r = R_DT_CIv(p);/* 1-p */
	else
	    r = p_;/* = R_DT_Iv(p) ^=  p */

	r = sqrtgnum(- ((log_p &&
		     ((lower_tail && q <= 0) || (!lower_tail && q > 0))) ?
		    p : /* else */ loggnum(r)));
        /* r = sqrtgnum(-loggnum(r))  <==>  min(p, 1-p) = expgnum( - r^2 ) */
#ifdef DEBUG_qnorm
	REprintf("\t close to 0 or 1: r = %7" GNUM_FORMAT_g "\n", r);
#endif

        if (r <= 5.) { /* <==> min(p,1-p) >= expgnum(-25) ~= 1.3888e-11 */
            r += -1.6;
            val = (((((((r * GNM_const(7.7454501427834140764e-4) +
                       GNM_const(.0227238449892691845833)) * r + GNM_const(.24178072517745061177)) *
                     r + GNM_const(1.27045825245236838258)) * r +
                    GNM_const(3.64784832476320460504)) * r + GNM_const(5.7694972214606914055)) *
                  r + GNM_const(4.6303378461565452959)) * r +
                 GNM_const(1.42343711074968357734))
                / (((((((r *
                         GNM_const(1.05075007164441684324e-9) + GNM_const(5.475938084995344946e-4)) *
                        r + GNM_const(.0151986665636164571966)) * r +
                       GNM_const(.14810397642748007459)) * r + GNM_const(.68976733498510000455)) *
                     r + GNM_const(1.6763848301838038494)) * r +
                    GNM_const(2.05319162663775882187)) * r + 1.);
        }
        else { /* very close to  0 or 1 */
            r += -5.;
            val = (((((((r * GNM_const(2.01033439929228813265e-7) +
                       GNM_const(2.71155556874348757815e-5)) * r +
                      GNM_const(.0012426609473880784386)) * r + GNM_const(.026532189526576123093)) *
                    r + GNM_const(.29656057182850489123)) * r +
                   GNM_const(1.7848265399172913358)) * r + GNM_const(5.4637849111641143699)) *
                 r + GNM_const(6.6579046435011037772))
                / (((((((r *
                         GNM_const(2.04426310338993978564e-15) + GNM_const(1.4215117583164458887e-7))*
                        r + GNM_const(1.8463183175100546818e-5)) * r +
                       GNM_const(7.868691311456132591e-4)) * r + GNM_const(.0148753612908506148525))
                     * r + GNM_const(.13692988092273580531)) * r +
                    GNM_const(.59983220655588793769)) * r + 1.);
        }

	if(q < 0.0)
	    val = -val;
        /* return (q >= 0.)? r : -r ;*/
    }
    return mu + sigma * val;
}




/* ------------------------------------------------------------------------ */
/* Imported src/nmath/plnorm.c from R.  */
/*
 *  Mathlib : A C Library of Special Functions
 *  Copyright (C) 1998 Ross Ihaka
 *  Copyright (C) 2000 The R Development Core Team
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307 USA.
 *
 *  DESCRIPTION
 *
 *    The lognormal distribution function.
 */


gnm_float plnorm(gnm_float x, gnm_float logmean, gnm_float logsd, gboolean lower_tail, gboolean log_p)
{
#ifdef IEEE_754
    if (isnangnum(x) || isnangnum(logmean) || isnangnum(logsd))
	return x + logmean + logsd;
#endif
    if (logsd <= 0) ML_ERR_return_NAN;

    if (x > 0)
	return pnorm(loggnum(x), logmean, logsd, lower_tail, log_p);
    return 0;
}

/* ------------------------------------------------------------------------ */
/* Imported src/nmath/qlnorm.c from R.  */
/*
 *  Mathlib : A C Library of Special Functions
 *  Copyright (C) 1998 Ross Ihaka
 *  Copyright (C) 2000 The R Development Core Team
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307 USA.
 *
 *  DESCRIPTION
 *
 *    This the lognormal quantile function.
 */


gnm_float qlnorm(gnm_float p, gnm_float logmean, gnm_float logsd, gboolean lower_tail, gboolean log_p)
{
#ifdef IEEE_754
    if (isnangnum(p) || isnangnum(logmean) || isnangnum(logsd))
	return p + logmean + logsd;
#endif
    R_Q_P01_check(p);

    if (p == R_DT_1)	return gnm_pinf;
    if (p == R_DT_0)	return 0;
    return expgnum(qnorm(p, logmean, logsd, lower_tail, log_p));
}

/* ------------------------------------------------------------------------ */
/* Imported src/nmath/ppois.c from R.  */
/*
 *  Mathlib : A C Library of Special Functions
 *  Copyright (C) 1998 Ross Ihaka
 *  Copyright (C) 2000 The R Development Core Team
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307 USA.
 *
 *  DESCRIPTION
 *
 *    The distribution function of the Poisson distribution.
 */


gnm_float ppois(gnm_float x, gnm_float lambda, gboolean lower_tail, gboolean log_p)
{
#ifdef IEEE_754
    if (isnangnum(x) || isnangnum(lambda))
	return x + lambda;
#endif
    if(lambda < 0.) ML_ERR_return_NAN;

    x = floorgnum(x + 1e-7);
    if (x < 0)		return R_DT_0;
    if (lambda == 0.)	return R_DT_1;
    if (!finitegnum(x))	return R_DT_1;

    return pgamma(lambda, x + 1, 1., !lower_tail, log_p);
}

/* ------------------------------------------------------------------------ */
/* Imported src/nmath/qpois.c from R.  */
/*
 *  Mathlib : A C Library of Special Functions
 *  Copyright (C) 1998 Ross Ihaka
 *  Copyright (C) 2000 The R Development Core Team
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307 USA.
 *
 *  DESCRIPTION
 *
 *	The quantile function of the Poisson distribution.
 *
 *  METHOD
 *
 *	Uses the Cornish-Fisher Expansion to include a skewness
 *	correction to a normal approximation.  This gives an
 *	initial value which never seems to be off by more than
 *	1 or 2.	 A search is then conducted of values close to
 *	this initial start point.
 */


gnm_float qpois(gnm_float p, gnm_float lambda, gboolean lower_tail, gboolean log_p)
{
    gnm_float mu, sigma, gamma, z, y;
#ifdef IEEE_754
    if (isnangnum(p) || isnangnum(lambda))
	return p + lambda;
#endif
    if(!finitegnum(lambda))
	ML_ERR_return_NAN;
    R_Q_P01_check(p);
    if(lambda < 0) ML_ERR_return_NAN;

    if (p == R_DT_0) return 0;
    if (p == R_DT_1) return gnm_pinf;

    if(lambda == 0) return 0;

    mu = lambda;
    sigma = sqrtgnum(lambda);
    gamma = sigma;

    /* Note : "same" code in qpois.c, qbinom.c, qnbinom.c --
     * FIXME: This is far from optimal [cancellation for p ~= 1, etc]: */
    if(!lower_tail || log_p) {
	p = R_DT_qIv(p); /* need check again (cancellation!): */
	if (p == 0.) return 0;
	if (p == 1.) return gnm_pinf;
    }
    /* temporary hack --- FIXME --- */
    if (p + 1.01*GNUM_EPSILON >= 1.) return gnm_pinf;

    /* y := approx.value (Cornish-Fisher expansion) :  */
    z = qnorm(p, 0., 1., /*lower_tail*/TRUE, /*log_p*/FALSE);
    y = floorgnum(mu + sigma * (z + gamma * (z*z - 1) / 6) + 0.5);

    z = ppois(y, lambda, /*lower_tail*/TRUE, /*log_p*/FALSE);

    /* fuzz to ensure left continuity; 1 - 1e-7 may lose too much : */
    p *= 1 - 64*GNUM_EPSILON;

/*-- Fixme, here y can be way off --
  should use interval search instead of primitive stepping down or up */

#ifdef maybe_future
    if((lower_tail && z >= p) || (!lower_tail && z <= p)) {
#else
    if(z >= p) {
#endif
			/* search to the left */
	for(;;) {
	    if(y == 0 ||
	       (z = ppois(y - 1, lambda, /*l._t.*/TRUE, /*log_p*/FALSE)) < p)
		return y;
	    y = y - 1;
	}
    }
    else {		/* search to the right */
	for(;;) {
	    y = y + 1;
	    if((z = ppois(y, lambda, /*l._t.*/TRUE, /*log_p*/FALSE)) >= p)
		return y;
	}
    }
}

/* ------------------------------------------------------------------------ */
/* Imported src/nmath/stirlerr.c from R.  */
/*
 *  AUTHOR
 *    Catherine Loader, catherine@research.bell-labs.com.
 *    October 23, 2000.
 *
 *  Merge in to R:
 *	Copyright (C) 2000, The R Core Development Team
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307 USA.
 *
 *
 *  DESCRIPTION
 *
 *    Computes the log of the error term in Stirling's formula.
 *      For n > 15, uses the series 1/12n - 1/360n^3 + ...
 *      For n <=15, integers or half-integers, uses stored values.
 *      For other n < 15, uses lgamma directly (don't use this to
 *        write lgamma!)
 *
 * Merge in to R:
 * Copyright (C) 2000, The R Core Development Team
 * R has lgammafn, and lgamma is not part of ISO C
 */


/* stirlerr(n) = loggnum(n!) - loggnum( sqrtgnum(2*pi*n)*(n/e)^n )
 *             = loggnum Gamma(n+1) - 1/2 * [loggnum(2*pi) + loggnum(n)] - n*[loggnum(n) - 1]
 *             = loggnum Gamma(n+1) - (n + 1/2) * loggnum(n) + n - loggnum(2*pi)/2
 *
 * see also lgammacor() in ./lgammacor.c  which computes almost the same!
 */

static gnm_float stirlerr(gnm_float n)
{

#define S0 GNM_const(0.083333333333333333333)       /* 1/12 */
#define S1 GNM_const(0.00277777777777777777778)     /* 1/360 */
#define S2 GNM_const(0.00079365079365079365079365)  /* 1/1260 */
#define S3 GNM_const(0.000595238095238095238095238) /* 1/1680 */
#define S4 GNM_const(0.0008417508417508417508417508)/* 1/1188 */

/*
  error for 0, 0.5, 1.0, 1.5, ..., 14.5, 15.0.
*/
    static const gnm_float sferr_halves[31] = {
	0.0, /* n=0 - wrong, place holder only */
	GNM_const(0.1534264097200273452913848),  /* 0.5 */
	GNM_const(0.0810614667953272582196702),  /* 1.0 */
	GNM_const(0.0548141210519176538961390),  /* 1.5 */
	GNM_const(0.0413406959554092940938221),  /* 2.0 */
	GNM_const(0.03316287351993628748511048), /* 2.5 */
	GNM_const(0.02767792568499833914878929), /* 3.0 */
	GNM_const(0.02374616365629749597132920), /* 3.5 */
	GNM_const(0.02079067210376509311152277), /* 4.0 */
	GNM_const(0.01848845053267318523077934), /* 4.5 */
	GNM_const(0.01664469118982119216319487), /* 5.0 */
	GNM_const(0.01513497322191737887351255), /* 5.5 */
	GNM_const(0.01387612882307074799874573), /* 6.0 */
	GNM_const(0.01281046524292022692424986), /* 6.5 */
	GNM_const(0.01189670994589177009505572), /* 7.0 */
	GNM_const(0.01110455975820691732662991), /* 7.5 */
	GNM_const(0.010411265261972096497478567), /* 8.0 */
	GNM_const(0.009799416126158803298389475), /* 8.5 */
	GNM_const(0.009255462182712732917728637), /* 9.0 */
	GNM_const(0.008768700134139385462952823), /* 9.5 */
	GNM_const(0.008330563433362871256469318), /* 10.0 */
	GNM_const(0.007934114564314020547248100), /* 10.5 */
	GNM_const(0.007573675487951840794972024), /* 11.0 */
	GNM_const(0.007244554301320383179543912), /* 11.5 */
	GNM_const(0.006942840107209529865664152), /* 12.0 */
	GNM_const(0.006665247032707682442354394), /* 12.5 */
	GNM_const(0.006408994188004207068439631), /* 13.0 */
	GNM_const(0.006171712263039457647532867), /* 13.5 */
	GNM_const(0.005951370112758847735624416), /* 14.0 */
	GNM_const(0.005746216513010115682023589), /* 14.5 */
	GNM_const(0.005554733551962801371038690)  /* 15.0 */
    };
    gnm_float nn;

    if (n <= 15.0) {
	nn = n + n;
	if (nn == (int)nn) return(sferr_halves[(int)nn]);
	return(lgamma1p (n) - (n + 0.5)*loggnum(n) + n - M_LN_SQRT_2PI);
    }

    nn = n*n;
    if (n>500) return((S0-S1/nn)/n);
    if (n> 80) return((S0-(S1-S2/nn)/nn)/n);
    if (n> 35) return((S0-(S1-(S2-S3/nn)/nn)/nn)/n);
    /* 15 < n <= 35 : */
    return((S0-(S1-(S2-(S3-S4/nn)/nn)/nn)/nn)/n);
}
/* Cleaning up done by tools/import-R:  */
#undef S0
#undef S1
#undef S2
#undef S3
#undef S4

/* ------------------------------------------------------------------------ */
/* Imported src/nmath/bd0.c from R.  */
/*
 *  AUTHOR
 *	Catherine Loader, catherine@research.bell-labs.com.
 *	October 23, 2000.
 *
 *  Merge in to R:
 *	Copyright (C) 2000, The R Core Development Team
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307 USA.
 *
 *
 *  DESCRIPTION
 *	Evaluates the "deviance part"
 *	bd0(x,M) :=  M * D0(x/M) = M*[ x/M * log(x/M) + 1 - (x/M) ] =
 *		  =  x * log(x/M) + M - x
 *	where M = E[X] = n*p (or = lambda), for	  x, M > 0
 *
 *	in a manner that should be stable (with small relative error)
 *	for all x and np. In particular for x/np close to 1, direct
 *	evaluation fails, and evaluation is based on the Taylor series
 *	of log((1+v)/(1-v)) with v = (x-np)/(x+np).
 */

static gnm_float bd0(gnm_float x, gnm_float np)
{
    gnm_float ej, s, s1, v;
    int j;

    if (gnumabs(x-np) < 0.1*(x+np)) {
	v = (x-np)/(x+np);
	s = (x-np)*v;/* s using v -- change by MM */
	ej = 2*x*v;
	v = v*v;
	for (j=1; ; j++) { /* Taylor series */
	    ej *= v;
	    s1 = s+ej/((j<<1)+1);
	    if (s1==s) /* last term was effectively 0 */
		return(s1);
	    s = s1;
	}
    }
    /* else:  | x - np |  is not too small */
    return(x*loggnum(x/np)+np-x);
}

/* ------------------------------------------------------------------------ */
/* Imported src/nmath/dpois.c from R.  */
/*
 *  AUTHOR
 *    Catherine Loader, catherine@research.bell-labs.com.
 *    October 23, 2000.
 *
 *  Merge in to R:
 *	Copyright (C) 2000, The R Core Development Team
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307 USA.
 *
 *
 * DESCRIPTION
 *
 *    dpois() checks argument validity and calls dpois_raw().
 *
 *    dpois_raw() computes the Poisson probability  lb^x exp(-lb) / x!.
 *      This does not check that x is an integer, since dgamma() may
 *      call this with a fractional x argument. Any necessary argument
 *      checks should be done in the calling function.
 *
 */


static gnm_float dpois_raw(gnm_float x, gnm_float lambda, gboolean give_log)
{
    if (lambda == 0) return( (x == 0) ? R_D__1 : R_D__0 );
    if (x == 0) return( R_D_exp(-lambda) );
    if (x < 0)  return( R_D__0 );

    return(R_D_fexp( M_2PIgnum*x, -stirlerr(x)-bd0(x,lambda) ));
}

gnm_float dpois(gnm_float x, gnm_float lambda, gboolean give_log)
{
#ifdef IEEE_754
    if(isnangnum(x) || isnangnum(lambda))
        return x + lambda;
#endif

    if (lambda < 0) ML_ERR_return_NAN;
    R_D_nonint_check(x);
    if (x < 0 || !finitegnum(x)) return R_D__0;
    x = R_D_forceint(x);

    return( dpois_raw(x,lambda,give_log) );
}

/* ------------------------------------------------------------------------ */
/* Imported src/nmath/dgamma.c from R.  */
/*
 *  AUTHOR
 *    Catherine Loader, catherine@research.bell-labs.com.
 *    October 23, 2000.
 *
 *  Merge in to R:
 *	Copyright (C) 2000 The R Core Development Team
 *	Copyright (C) 2004 The R Foundation
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307 USA.
 *
 *
 * DESCRIPTION
 *
 *   Computes the density of the gamma distribution,
 *
 *                   1/s (x/s)^{a-1} exp(-x/s)
 *        p(x;a,s) = -----------------------
 *                            (a-1)!
 *
 *   where `s' is the scale (= 1/lambda in other parametrizations)
 *     and `a' is the shape parameter ( = alpha in other contexts).
 *
 * The old (R 1.1.1) version of the code is available via `#define D_non_pois'
 */


gnm_float dgamma(gnm_float x, gnm_float shape, gnm_float scale, gboolean give_log)
{
    gnm_float pr;
#ifdef IEEE_754
    if (isnangnum(x) || isnangnum(shape) || isnangnum(scale))
        return x + shape + scale;
#endif
    if (shape <= 0 || scale <= 0) ML_ERR_return_NAN;
    if (x < 0)
	return R_D__0;
    if (x == 0) {
	if (shape < 1) return gnm_pinf;
	if (shape > 1) return R_D__0;
	/* else */
	return give_log ? -loggnum(scale) : 1 / scale;
    }

    if (shape < 1) {
	pr = dpois_raw(shape, x/scale, give_log);
	return give_log ?  pr + loggnum(shape/x) : pr*shape/x;
    }
    /* else  shape >= 1 */
    pr = dpois_raw(shape-1, x/scale, give_log);
    return give_log ? pr - loggnum(scale) : pr/scale;
}

/* ------------------------------------------------------------------------ */
/* Imported src/nmath/pgamma.c from R.  */
/*
 *  Mathlib : A C Library of Special Functions
 *  Copyright (C) 1998		Ross Ihaka
 *  Copyright (C) 1999-2000	The R Development Core Team
 *  Copyright (C) 2003-2004     The R Foundation
 *  Copyright (C) 2004          Morten Welinder
 *  Copyright (C) 2002-2003     Ian Smith
 *
 *  Formerly based on AS 239 (C) 1988 Royal Statistical Society
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307 USA.
 *
 *  SYNOPSIS
 *
 *	#include <Rmath.h>
 *	double pgamma(double x, double alph, double scale,
 *		      int lower_tail, int log_p)
 *
 *  DESCRIPTION
 *
 *	This function computes the distribution function for the
 *	gamma distribution with shape parameter alph and scale parameter
 *	scale.	This is also known as the incomplete gamma function.
 *	See Abramowitz and Stegun (6.5.1) for example.
 *
 *  NOTES
 *
 *	This function is an adaptation of Algorithm 239 from the
 *	Applied Statistics Series.  The algorithm is faster than
 *	those by W. Fullerton in the FNLIB library and also the
 *	TOMS 542 alorithm of W. Gautschi.  It provides comparable
 *	accuracy to those algorithms and is considerably simpler.
 *
 *  REFERENCES
 *
 *	Algorithm AS 239, Incomplete Gamma Function
 *	Applied Statistics 37, 1988.
 */




/*
 * Compute the log of a sum from logs of terms, i.e.,
 *
 *     log (exp (logx) + exp (logy))
 *
 * without causing overflows and without throwing away large handfuls
 * of accuracy.
 */
static gnm_float
logspace_add (gnm_float logx, gnm_float logy)
{
     return fmax2 (logx, logy) + log1pgnum (expgnum (-gnumabs (logx - logy)));
}


/*
 * Compute the log of a difference from logs of terms, i.e.,
 *
 *     log (exp (logx) - exp (logy))
 *
 * without causing overflows and without throwing away large handfuls
 * of accuracy.
 */
static gnm_float
logspace_sub (gnm_float logx, gnm_float logy)
{
     return logx + log1pgnum (-expgnum (logy - logx));
}


static gnm_float
dpois_wrap (gnm_float x_plus_1, gnm_float lambda, gboolean give_log)
{
#if 0
     printf ("x+1=%.14" GNUM_FORMAT_g "  lambda=%.14" GNUM_FORMAT_g "\n", x_plus_1, lambda);
#endif

     if (x_plus_1 > 1)
	  return dpois_raw (x_plus_1 - 1, lambda, give_log);
     else {
 	  gnm_float d = dpois_raw (x_plus_1, lambda, give_log);
	  return give_log
	       ? d + loggnum (x_plus_1 / lambda)
	       : d * (x_plus_1 / lambda);
     }
}

/*
 * Abramowitz and Stegun 6.5.29 [right]
 */
static gnm_float
pgamma_smallx (gnm_float x, gnm_float alph, gboolean lower_tail, gboolean log_p)
{
     gnm_float sum = 0, c = alph, n = 0, term;

#if 0
     printf ("x:%.14" GNUM_FORMAT_g "  alph:%.14" GNUM_FORMAT_g "\n", x, alph);
#endif

     /*
      * Relative to 6.5.29 all terms have been multiplied by alph
      * and the first, thus being 1, is omitted.
      */

     do {
	  n++;
	  c *= -x / n;
	  term = c / (alph + n);
	  sum += term;
     } while (gnumabs (term) > GNUM_EPSILON * gnumabs (sum));

     if (lower_tail) {
	  gnm_float f1 = log_p ? log1pgnum (sum) : 1 + sum;
	  gnm_float f2;
	  if (alph > 1) {
	       f2 = dpois_raw (alph, x, log_p);
	       f2 = log_p ? f2 + x : f2 * expgnum (x);
	  } else if (log_p)
	       f2 = alph * loggnum (x) - lgamma1p (alph);
	  else
	       f2 = powgnum (x, alph) / expgnum (lgamma1p (alph));

	  return log_p ? f1 + f2 : f1 * f2;
     } else {
	  gnm_float lf2 = alph * loggnum (x) - lgamma1p (alph);
#if 0
	  printf ("1:%.14" GNUM_FORMAT_g "  2:%.14" GNUM_FORMAT_g "\n", alph * loggnum (x), lgamma1p (alph));
	  printf ("sum=%.14" GNUM_FORMAT_g "  log1pgnum (sum)=%.14" GNUM_FORMAT_g "  lf2=%.14" GNUM_FORMAT_g "\n", sum, log1pgnum (sum), lf2);
#endif
	  if (log_p)
	       return swap_log_tail (log1pgnum (sum) + lf2);
	  else {
	       gnm_float f1m1 = sum;
	       gnm_float f2m1 = expm1gnum (lf2);
	       return -(f1m1 + f2m1 + f1m1 * f2m1);
	  }
     }
}

static gnm_float
pd_upper_series (gnm_float x, gnm_float y, gboolean log_p)
{
     gnm_float term = x / y;
     gnm_float sum = term;

     do {
	  y++;
	  term *= x / y;
	  sum += term;
     } while (term > sum * GNUM_EPSILON);

     return log_p ? loggnum (sum) : sum;
}

static gnm_float
pd_lower_cf (gnm_float i, gnm_float d)
{
     gnm_float f = 0, of;

     gnm_float a1 = 0;
     gnm_float b1 = 1;
     gnm_float a2 = i;
     gnm_float b2 = d;
     gnm_float c1 = 0;
     gnm_float c2 = a2;
     gnm_float c3;
     gnm_float c4 = b2;

     while (1) {
	  c1++;
	  c2--;
	  c3 = c1 * c2;
	  c4 += 2;
	  a1 = c4 * a2 + c3 * a1;
	  b1 = c4 * b2 + c3 * b1;

	  c1++;
	  c2--;
	  c3 = c1 * c2;
	  c4 += 2;
	  a2 = c4 * a1 + c3 * a2;
	  b2 = c4 * b1 + c3 * b2;

	  if (b2 > scalefactor) {
	       a1 = a1 / scalefactor;
	       b1 = b1 / scalefactor;
	       a2 = a2 / scalefactor;
	       b2 = b2 / scalefactor;
	  }

	  if (b2 != 0) {
	       of = f;
	       f = a2 / b2;
	       if (gnumabs (f - of) <= GNUM_EPSILON * fmin2 (1.0, gnumabs (f)))
		    return f;
	  }
     }
}

static gnm_float
pd_lower_series (gnm_float lambda, gnm_float y)
{
     gnm_float term = 1, sum = 0;

     while (y >= 1 && term > sum * GNUM_EPSILON) {
	  term *= y / lambda;
	  sum += term;
	  y--;
     }

     if (y != floorgnum (y)) {
	  /*
	   * The series does not converge as the terms start getting
	   * bigger (besides flipping sign) for y < -lambda.
	   */
	  gnm_float f = pd_lower_cf (y, lambda + 1 - y);
	  sum += term * f;
     }

     return sum;
}

/*
 * Asymptotic expansion to calculate the probability that poisson variate
 * has value <= x.
 */
static gnm_float
ppois_asymp (gnm_float x, gnm_float lambda,
	     gboolean lower_tail, gboolean log_p)
{
     static const gnm_float coef15 = 1 / GNM_const(12.0);
     static const gnm_float coef25 = 1 / GNM_const(288.0);
     static const gnm_float coef35 = -139 / GNM_const(51840.0);
     static const gnm_float coef45 = -571 / GNM_const(2488320.0);
     static const gnm_float coef55 = 163879 / GNM_const(209018880.0);
     static const gnm_float coef65 =  5246819 / GNM_const(75246796800.0);
     static const gnm_float coef75 = -534703531 / GNM_const(902961561600.0);
     static const gnm_float coef1 = 2 / GNM_const(3.0);
     static const gnm_float coef2 = -4 / GNM_const(135.0);
     static const gnm_float coef3 = 8 / GNM_const(2835.0);
     static const gnm_float coef4 = 16 / GNM_const(8505.0);
     static const gnm_float coef5 = -8992 / GNM_const(12629925.0);
     static const gnm_float coef6 = -334144 / GNM_const(492567075.0);
     static const gnm_float coef7 = 698752 / GNM_const(1477701225.0);
     static const gnm_float two = 2;

     gnm_float dfm, pt,s2pt,res1,res2,elfb,term;
     gnm_float ig2,ig3,ig4,ig5,ig6,ig7,ig25,ig35,ig45,ig55,ig65,ig75;
     gnm_float f, np, nd;

     dfm = lambda - x;
     pt = -x * log1pmx (dfm / x);
     s2pt = sqrtgnum (2 * pt);
     if (dfm < 0) s2pt = -s2pt;

     ig2 = 1.0 + pt;
     term = pt * pt * 0.5;
     ig3 = ig2 + term;
     term *= pt / 3;
     ig4 = ig3 + term;
     term *= pt / 4;
     ig5 = ig4 + term;
     term *= pt / 5;
     ig6 = ig5 + term;
     term *= pt / 6;
     ig7 = ig6 + term;

     term = pt * (two / 3);
     ig25 = 1.0 + term;
     term *= pt * (two / 5);
     ig35 = ig25 + term;
     term *= pt * (two / 7);
     ig45 = ig35 + term;
     term *= pt * (two / 9);
     ig55 = ig45 + term;
     term *= pt * (two / 11);
     ig65 = ig55 + term;
     term *= pt * (two / 13);
     ig75 = ig65 + term;

     elfb = ((((((coef75/x + coef65)/x + coef55)/x + coef45)/x + coef35)/x + coef25)/x + coef15) + x;
     res1 = ((((((ig7*coef7/x + ig6*coef6)/x + ig5*coef5)/x + ig4*coef4)/x + ig3*coef3)/x + ig2*coef2)/x + coef1)*sqrtgnum(x);
     res2 = ((((((ig75*coef75/x + ig65*coef65)/x + ig55*coef55)/x + ig45*coef45)/x + ig35*coef35)/x + ig25*coef25)/x + coef15)*s2pt;

     if (!lower_tail) elfb = -elfb;
     f = (res1 + res2) / elfb;

     np = pnorm (s2pt, 0.0, 1.0, !lower_tail, log_p);
     nd = dnorm (s2pt, 0.0, 1.0, log_p);

#if 0
     printf ("f=%.14" GNUM_FORMAT_g "  np=%.14" GNUM_FORMAT_g "  nd=%.14" GNUM_FORMAT_g "  f*nd=%.14" GNUM_FORMAT_g "\n", f, np, nd, f * nd);
#endif

     if (log_p)
	  return (f >= 0)
	       ? logspace_add (np, loggnum (gnumabs (f)) + nd)
	       : logspace_sub (np, loggnum (gnumabs (f)) + nd);
     else
	  return np + f * nd;
}


static gnm_float
pgamma_raw (gnm_float x, gnm_float alph, gboolean lower_tail, gboolean log_p)
{
     gnm_float res;

#if 0
     printf ("x=%.14" GNUM_FORMAT_g "  alph=%.14" GNUM_FORMAT_g "  low=%d  log=%d\n", x, alph, lower_tail, log_p);
#endif

     if (x < 1) {
	  res = pgamma_smallx (x, alph, lower_tail, log_p);
     } else if (x <= alph - 1 && x < 0.8 * (alph + 50)) {
	  gnm_float sum = pd_upper_series (x, alph, log_p);
	  gnm_float d = dpois_wrap (alph, x, log_p);

	  if (!lower_tail)
	       res = log_p
		    ? swap_log_tail (d + sum)
		    : 1 - d * sum;
	  else
	       res = log_p ? sum + d : sum * d;
     } else if (alph - 1 < x && alph < 0.8 * (x + 50)) {
	  gnm_float sum;
	  gnm_float d = dpois_wrap (alph, x, log_p);

	  if (alph < 1) {
	       gnm_float f = pd_lower_cf (alph, x - (alph - 1))
		    * x / alph;
	       sum = log_p ? loggnum (f) : f;
	  } else {
	       sum = pd_lower_series (x, alph - 1);
	       sum = log_p ? log1pgnum (sum) : 1 + sum;
	  }

	  if (!lower_tail)
	       res = log_p ? sum + d : sum * d;
	  else
	       res = log_p
		    ? swap_log_tail (d + sum)
		    : 1 - d * sum;
     } else {
	  res = ppois_asymp (alph - 1, x, !lower_tail, log_p);
     }

     /*
      * We lose a fair amount of accuracy to underflow in the cases
      * where the final result is very close to DBL_MIN.  In those
      * cases, simply redo via log space.
      */
     if (!log_p && res < GNUM_MIN / GNUM_EPSILON)
	  return expgnum (pgamma_raw (x, alph, lower_tail, 1));
     else
	  return res;
}


gnm_float pgamma(gnm_float x, gnm_float alph, gnm_float scale, gboolean lower_tail, gboolean log_p)
{
#ifdef IEEE_754
     if (isnangnum(x) || isnangnum(alph) || isnangnum(scale))
	  return x + alph + scale;
#endif
     if(alph <= 0. || scale <= 0.)
	  ML_ERR_return_NAN;
     if (x <= 0.)
	  return R_DT_0;
     x /= scale;
#ifdef IEEE_754
     if (isnangnum(x)) /* eg. original x = scale = +Inf */
	  return x;
#endif

     return pgamma_raw (x, alph, lower_tail, log_p);
}

/* ------------------------------------------------------------------------ */
/* Imported src/nmath/chebyshev.c from R.  */
/*
 *  Mathlib : A C Library of Special Functions
 *  Copyright (C) 1998 Ross Ihaka
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 *  SYNOPSIS
 *
 *    int chebyshev_init(double *dos, int nos, double eta)
 *    double chebyshev_eval(double x, double *a, int n)
 *
 *  DESCRIPTION
 *
 *    "chebyshev_init" determines the number of terms for the
 *    double precision orthogonal series "dos" needed to insure
 *    the error is no larger than "eta".  Ordinarily eta will be
 *    chosen to be one-tenth machine precision.
 *
 *    "chebyshev_eval" evaluates the n-term Chebyshev series
 *    "a" at "x".
 *
 *  NOTES
 *
 *    These routines are translations into C of Fortran routines
 *    by W. Fullerton of Los Alamos Scientific Laboratory.
 *
 *    Based on the Fortran routine dcsevl by W. Fullerton.
 *    Adapted from R. Broucke, Algorithm 446, CACM., 16, 254 (1973).
 */


/* NaNs propagated correctly */

#if 0
static int chebyshev_init(gnm_float *dos, int nos, gnm_float eta)
{
    int i, ii;
    gnm_float err;

    if (nos < 1)
	return 0;

    err = 0.0;
    i = 0;			/* just to avoid compiler warnings */
    for (ii=1; ii<=nos; ii++) {
	i = nos - ii;
	err += gnumabs(dos[i]);
	if (err > eta) {
	    return i;
	}
    }
    return i;
}
#endif // 0 -- jsled, unused


static gnm_float chebyshev_eval(gnm_float x, const gnm_float *a, const int n)
{
    gnm_float b0, b1, b2, twox;
    int i;

    if (n < 1 || n > 1000) ML_ERR_return_NAN;

    if (x < -1.1 || x > 1.1) ML_ERR_return_NAN;

    twox = x * 2;
    b2 = b1 = 0;
    b0 = 0;
    for (i = 1; i <= n; i++) {
	b2 = b1;
	b1 = b0;
	b0 = twox * b1 - b2 + a[n - i];
    }
    return (b0 - b2) * 0.5;
}

/* ------------------------------------------------------------------------ */
/* Imported src/nmath/lgammacor.c from R.  */
/*
 *  Mathlib : A C Library of Special Functions
 *  Copyright (C) 1998 Ross Ihaka
 *  Copyright (C) 2000-2001 The R Development Core Team
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307 USA.
 *
 *  SYNOPSIS
 *
 *    #include <Rmath.h>
 *    double lgammacor(double x);
 *
 *  DESCRIPTION
 *
 *    Compute the log gamma correction factor for x >= 10 so that
 *
 *    log(gamma(x)) = .5*log(2*pi) + (x-.5)*log(x) -x + lgammacor(x)
 *
 *    [ lgammacor(x) is called	Del(x)	in other contexts (e.g. dcdflib)]
 *
 *  NOTES
 *
 *    This routine is a translation into C of a Fortran subroutine
 *    written by W. Fullerton of Los Alamos Scientific Laboratory.
 *
 *  SEE ALSO
 *
 *    Loader(1999)'s stirlerr() {in ./stirlerr.c} is *very* similar in spirit,
 *    is faster and cleaner, but is only defined "fast" for half integers.
 */


static gnm_float lgammacor(gnm_float x)
{
    static const gnm_float algmcs[15] = {
	GNM_const(+.1666389480451863247205729650822e+0),
	GNM_const(-.1384948176067563840732986059135e-4),
	GNM_const(+.9810825646924729426157171547487e-8),
	GNM_const(-.1809129475572494194263306266719e-10),
	GNM_const(+.6221098041892605227126015543416e-13),
	GNM_const(-.3399615005417721944303330599666e-15),
	GNM_const(+.2683181998482698748957538846666e-17),
	GNM_const(-.2868042435334643284144622399999e-19),
	GNM_const(+.3962837061046434803679306666666e-21),
	GNM_const(-.6831888753985766870111999999999e-23),
	GNM_const(+.1429227355942498147573333333333e-24),
	GNM_const(-.3547598158101070547199999999999e-26),
	GNM_const(+.1025680058010470912000000000000e-27),
	GNM_const(-.3401102254316748799999999999999e-29),
	GNM_const(+.1276642195630062933333333333333e-30)
    };

    gnm_float tmp;

#ifdef NOMORE_FOR_THREADS
    static int nalgm = 0;
    static gnm_float xbig = 0, xmax = 0;

    /* Initialize machine dependent constants, the first time gamma() is called.
	FIXME for threads ! */
    if (nalgm == 0) {
	/* For IEEE gnm_float precision : nalgm = 5 */
	nalgm = chebyshev_init(algmcs, 15, GNUM_EPSILON/2);/*was d1mach(3)*/
	xbig = 1 / sqrtgnum(GNUM_EPSILON/2); /* ~ 94906265.6 for IEEE gnm_float */
	xmax = expgnum(fmin2(loggnum(GNUM_MAX / 12), -loggnum(12 * GNUM_MIN)));
	/*   = GNUM_MAX / 48 ~= 3.745e306 for IEEE gnm_float */
    }
#else
/* For IEEE gnm_float precision GNUM_EPSILON = 2^-52 = GNM_const(2.220446049250313e-16) :
 *   xbig = 2 ^ 26.5
 *   xmax = GNUM_MAX / 48 =  2^1020 / 3 */
# define nalgm 5
# define xbig  94906265.62425156
# define xmax  GNM_const(3.745194030963158e306)
#endif

    if (x < 10)
	ML_ERR_return_NAN
    else if (x >= xmax) {
	ML_ERROR(ME_UNDERFLOW);
	return ML_UNDERFLOW;
    }
    else if (x < xbig) {
	tmp = 10 / x;
	return chebyshev_eval(tmp * tmp * 2 - 1, algmcs, nalgm) / x;
    }
    else return 1 / (x * 12);
}
/* Cleaning up done by tools/import-R:  */
#undef nalgm
#undef xbig
#undef xmax

/* ------------------------------------------------------------------------ */
/* Imported src/nmath/lbeta.c from R.  */
/*
 *  Mathlib : A C Library of Special Functions
 *  Copyright (C) 1998 Ross Ihaka
 *  Copyright (C) 2000 The R Development Core Team
 *  Copyright (C) 2003 The R Foundation
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307 USA.
 *
 *  SYNOPSIS
 *
 *    #include <Rmath.h>
 *    double lbeta(double a, double b);
 *
 *  DESCRIPTION
 *
 *    This function returns the value of the log beta function.
 *
 *  NOTES
 *
 *    This routine is a translation into C of a Fortran subroutine
 *    by W. Fullerton of Los Alamos Scientific Laboratory.
 */


static gnm_float lbeta(gnm_float a, gnm_float b)
{
    gnm_float corr, p, q;

    p = q = a;
    if(b < p) p = b;/* := min(a,b) */
    if(b > q) q = b;/* := max(a,b) */

#ifdef IEEE_754
    if(isnangnum(a) || isnangnum(b))
	return a + b;
#endif

    /* both arguments must be >= 0 */

    if (p < 0)
	ML_ERR_return_NAN
    else if (p == 0) {
	return gnm_pinf;
    }
    else if (!finitegnum(q)) {
	return gnm_ninf;
    }

    if (p >= 10) {
	/* p and q are big. */
	corr = lgammacor(p) + lgammacor(q) - lgammacor(p + q);
	return loggnum(q) * -0.5 + M_LN_SQRT_2PI + corr
		+ (p - 0.5) * loggnum(p / (p + q)) + q * log1pgnum(-p / (p + q));
    }
    else if (q >= 10) {
	/* p is small, but q is big. */
	corr = lgammacor(q) - lgammacor(p + q);
	return lgammagnum(p) + corr + p - p * loggnum(p + q)
		+ (q - 0.5) * log1pgnum(-p / (p + q));
    }
    else
	/* p and q are small: p <= q < 10. */
	return lgammagnum(p) + lgammagnum(q) - lgammagnum(p + q);
}

/* ------------------------------------------------------------------------ */
/* Imported src/nmath/dt.c from R.  */
/*
 *  AUTHOR
 *    Catherine Loader, catherine@research.bell-labs.com.
 *    October 23, 2000.
 *
 *  Merge in to R:
 *	Copyright (C) 2000, The R Core Development Team
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307 USA.
 *
 *
 * DESCRIPTION
 *
 *    The t density is evaluated as
 *         sqrt(n/2) / ((n+1)/2) * Gamma((n+3)/2) / Gamma((n+2)/2).
 *             * (1+x^2/n)^(-n/2)
 *             / sqrt( 2 pi (1+x^2/n) )
 *
 *    This form leads to a stable computation for all
 *    values of n, including n -> 0 and n -> infinity.
 */


gnm_float dt(gnm_float x, gnm_float n, gboolean give_log)
{
    gnm_float t, u;
#ifdef IEEE_754
    if (isnangnum(x) || isnangnum(n))
	return x + n;
#endif

    if (n <= 0) ML_ERR_return_NAN;
    if(!finitegnum(x))
	return R_D__0;
    if(!finitegnum(n))
	return dnorm(x, 0., 1., give_log);

    t = -bd0(n/2.,(n+1)/2.) + stirlerr((n+1)/2.) - stirlerr(n/2.);
    if ( x*x > 0.2*n )
	u = log1pgnum (x*x/n ) * n/2;
    else
	u = -bd0(n/2.,(n+x*x)/2.) + x*x/2.;

    return R_D_fexp(M_2PIgnum*(1+x*x/n), t-u);
}

/* ------------------------------------------------------------------------ */
/* Imported src/nmath/pt.c from R.  */
/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 2000 The R Development Core Team
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307 USA.
 */


gnm_float pt(gnm_float x, gnm_float n, gboolean lower_tail, gboolean log_p)
{
/* return  P[ T <= x ]	where
 * T ~ t_{n}  (t distrib. with n degrees of freedom).

 *	--> ./pnt.c for NON-central
 */
    gnm_float val;
#ifdef IEEE_754
    if (isnangnum(x) || isnangnum(n))
	return x + n;
#endif
    if (n <= 0.0) ML_ERR_return_NAN;

    if(!finitegnum(x))
	return (x < 0) ? R_DT_0 : R_DT_1;
    if(!finitegnum(n))
	return pnorm(x, 0.0, 1.0, lower_tail, log_p);
    if (0 && n > 4e5) { /*-- Fixme(?): test should depend on `n' AND `x' ! */
	/* Approx. from	 Abramowitz & Stegun 26.7.8 (p.949) */
	val = 1./(4.*n);
	return pnorm(x*(1. - val)/sqrtgnum(1. + x*x*2.*val), 0.0, 1.0,
		     lower_tail, log_p);
    }

    val =  (n > x * x)
	? pbeta (x * x / (n + x * x), 0.5, n / 2, /*lower_tail*/0, log_p)
	: pbeta (n / (n + x * x), n / 2.0, 0.5, /*lower_tail*/1, log_p);

    /* Use "1 - v"  if	lower_tail  and	 x > 0 (but not both):*/
    if(x <= 0.)
	lower_tail = !lower_tail;

    if(log_p) {
	if(lower_tail) return log1pgnum(-0.5*expgnum(val));
	else return val - M_LN2gnum; /* = loggnum(.5* pbeta(....)) */
    }
    else {
	val /= 2.;
	return R_D_Cval(val);
    }
}

/* ------------------------------------------------------------------------ */
/* Imported src/nmath/qt.c from R.  */
/*
 *  Mathlib : A C Library of Special Functions
 *  Copyright (C) 1998 Ross Ihaka
 *  Copyright (C) 2000-2002 The R Development Core Team
 *  Copyright (C) 2003	    The R Foundation
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307 USA.
 *
 *  DESCRIPTION
 *
 *	The "Student" t distribution quantile function.
 *
 *  NOTES
 *
 *	This is a C translation of the Fortran routine given in:
 *	Hill, G.W (1970) "Algorithm 396: Student's t-quantiles"
 *	CACM 13(10), 619-620.
 *
 *  ADDITIONS:
 *	- lower_tail, log_p
 *	- using	 expm1() : takes care of  Lozy (1979) "Remark on Algo.", TOMS
 *	- Apply 2-term Taylor expansion as in
 *	  Hill, G.W (1981) "Remark on Algo.396", ACM TOMS 7, 250-1
 *	- Improve the formula decision for 1 < df < 2
 */


gnm_float qt(gnm_float p, gnm_float ndf, gboolean lower_tail, gboolean log_p)
{
    const gnm_float eps = 1.e-12;

    gnm_float a, b, c, d, p_, P, q, x, y;
    gboolean neg;

#ifdef IEEE_754
    if (isnangnum(p) || isnangnum(ndf))
	return p + ndf;
#endif
    if (p == R_DT_0) return gnm_ninf;
    if (p == R_DT_1) return gnm_pinf;
    R_Q_P01_check(p);

    if (ndf < 1) /* FIXME:  not yet treated here */
	ML_ERR_return_NAN;

    /* FIXME: This test should depend on  ndf  AND p  !!
     * -----  and in fact should be replaced by
     * something like Abramowitz & Stegun 26.7.5 (p.949)
     */
    if (ndf > 1e20) return qnorm(p, 0., 1., lower_tail, log_p);

    p_ = R_D_qIv(p); /* note: expgnum(p) may underflow to 0; fix later */

    if((lower_tail && p_ > 0.5) || (!lower_tail && p_ < 0.5)) {
	neg = FALSE; P = 2 * R_D_Cval(p_);
    } else {
	neg = TRUE;  P = 2 * R_D_Lval(p_);
    } /* 0 <= P <= 1 ; P = 2*min(p_, 1 - p_)  in all cases */

    if (gnumabs(ndf - 2) < eps) {	/* df ~= 2 */
	if(P > 0)
	    q = sqrtgnum(2 / (P * (2 - P)) - 2);
	else { /* P = 0, but maybe = expgnum(p) ! */
	    if(log_p) q = M_SQRT2gnum * expgnum(- .5 * R_D_Lval(p));
	    else q = gnm_pinf;
	}
    }
    else if (ndf < 1 + eps) { /* df ~= 1  (df < 1 excluded above): Cauchy */
	if(P > 0)
	    q = - tangnum((P+1) * M_PI_2gnum);

	else { /* P = 0, but maybe p_ = expgnum(p) ! */
	    if(log_p) q = M_1_PI * expgnum(-R_D_Lval(p));/* cot(e) ~ 1/e */
	    else q = gnm_pinf;
	}
    }
    else {		/*-- usual case;  including, e.g.,  df = 1.1 */
	a = 1 / (ndf - 0.5);
	b = 48 / (a * a);
	c = ((20700 * a / b - 98) * a - 16) * a + 96.36;
	d = ((94.5 / (b + c) - 3) / b + 1) * sqrtgnum(a * M_PI_2gnum) * ndf;
	if(P > 0 || !log_p)
	    y = powgnum(d * P, 2 / ndf);
	else /* P = 0 && log_p;	 P = 2*expgnum(p) */
	    y = expgnum(2 / ndf * (loggnum(d) + M_LN2gnum + R_D_Lval(p)));

	if ((ndf < 2.1 && P > 0.5) || y > 0.05 + a) { /* P > P0(df) */
	    /* Asymptotic inverse expansion about normal */
	    if(P > 0 || !log_p)
		x = qnorm(0.5 * P, 0., 1., /*lower_tail*/TRUE, /*log_p*/FALSE);
	    else /* P = 0 && log_p;  P = 2*expgnum(p') */
		x = qnorm( p,	   0., 1., lower_tail,	       /*log_p*/TRUE);

	    y = x * x;
	    if (ndf < 5)
		c += 0.3 * (ndf - 4.5) * (x + 0.6);
	    c = (((0.05 * d * x - 5) * x - 7) * x - 2) * x + b + c;
	    y = (((((0.4 * y + 6.3) * y + 36) * y + 94.5) / c
		  - y - 3) / b + 1) * x;
	    y = expm1gnum(a * y * y);
	} else {
	    y = ((1 / (((ndf + 6) / (ndf * y) - 0.089 * d - 0.822)
		       * (ndf + 2) * 3) + 0.5 / (ndf + 4))
		 * y - 1) * (ndf + 1) / (ndf + 2) + 1 / y;
	}
	q = sqrtgnum(ndf * y);

	/* Now apply 2-term Taylor expansion improvement (1-term = Newton):
	 * as by Hill (1981) [ref.above] */

	/* FIXME: This is can be far from optimal when log_p = TRUE !
	 *	  and probably also improvable when  lower_tail = FALSE */
	x = (pt(q, ndf, /*lower_tail = */FALSE, /*log_p = */FALSE) - P/2) /
	    dt(q, ndf, /* give_log = */FALSE);
	/* Newton (=Taylor 1 term):
	 *  q += x;
	 * Taylor 2-term : */
	q += x * (1. + x * q * (ndf + 1) / (2 * (q * q + ndf)));
    }
    if(neg) q = -q;
    return q;
}

/* ------------------------------------------------------------------------ */
/* Imported src/nmath/qf.c from R.  */
/*
 *  Mathlib : A C Library of Special Functions
 *  Copyright (C) 1998 Ross Ihaka
 *  Copyright (C) 2000 The R Development Core Team
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307 USA.
 *
 *  DESCRIPTION
 *
 *    The quantile function of the F distribution.
*/


gnm_float qf(gnm_float p, gnm_float n1, gnm_float n2, gboolean lower_tail, gboolean log_p)
{
#ifdef IEEE_754
    if (isnangnum(p) || isnangnum(n1) || isnangnum(n2))
	return p + n1 + n2;
#endif
    if (n1 <= 0. || n2 <= 0.) ML_ERR_return_NAN;

    R_Q_P01_check(p);
    if (p == R_DT_0)
	return 0;

    /* fudge the extreme DF cases -- qbeta doesn't do this well */

    if (n2 > 4e5)
	return qchisq(p, n1, lower_tail, log_p) / n1;

    if (n1 > 4e5)
	return 1/qchisq(p, n2, !lower_tail, log_p) * n2;

    p = (1. / qbeta(R_DT_CIv(p), n2/2, n1/2, TRUE, FALSE) - 1.) * (n2 / n1);
    return !isnangnum(p) ? p : gnm_nan;
}

/* ------------------------------------------------------------------------ */
/* Imported src/nmath/pchisq.c from R.  */
/*
 *  Mathlib : A C Library of Special Functions
 *  Copyright (C) 1998	Ross Ihaka
 *  Copyright (C) 2000	The R Development Core Team
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful, but
 *  WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
 *  USA.
 *
 *  DESCRIPTION
 *
 *     The distribution function of the chi-squared distribution.
 */


gnm_float pchisq(gnm_float x, gnm_float df, gboolean lower_tail, gboolean log_p)
{
    return pgamma(x, df/2., 2., lower_tail, log_p);
}

/* ------------------------------------------------------------------------ */
/* Imported src/nmath/qchisq.c from R.  */
/*
 *  Mathlib : A C Library of Special Functions
 *  Copyright (C) 1998 Ross Ihaka
 *  Copyright (C) 2000 The R Development Core Team
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307 USA.
 *
 *  DESCRIPTION
 *
 *	The quantile function of the chi-squared distribution.
 */


gnm_float qchisq(gnm_float p, gnm_float df, gboolean lower_tail, gboolean log_p)
{
    return qgamma(p, 0.5 * df, 2.0, lower_tail, log_p);
}

/* ------------------------------------------------------------------------ */
/* Imported src/nmath/dweibull.c from R.  */
/*
 *  Mathlib : A C Library of Special Functions
 *  Copyright (C) 1998 Ross Ihaka
 *  Copyright (C) 2000 The R Development Core Team
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307 USA.
 *
 *  DESCRIPTION
 *
 *    The density function of the Weibull distribution.
 */


gnm_float dweibull(gnm_float x, gnm_float shape, gnm_float scale, gboolean give_log)
{
    gnm_float tmp1, tmp2;
#ifdef IEEE_754
    if (isnangnum(x) || isnangnum(shape) || isnangnum(scale))
	return x + shape + scale;
#endif
    if (shape <= 0 || scale <= 0) ML_ERR_return_NAN;

    if (x < 0) return R_D__0;
    if (!finitegnum(x)) return R_D__0;
    tmp1 = powgnum(x / scale, shape - 1);
    tmp2 = tmp1 * (x / scale);
    return  give_log ?
	-tmp2 + loggnum(shape * tmp1 / scale) :
	shape * tmp1 * expgnum(-tmp2) / scale;
}

/* ------------------------------------------------------------------------ */
/* Imported src/nmath/pweibull.c from R.  */
/*
 *  Mathlib : A C Library of Special Functions
 *  Copyright (C) 1998 Ross Ihaka
 *  Copyright (C) 2000-2002 The R Development Core Team
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307 USA.
 *
 *  DESCRIPTION
 *
 *    The distribution function of the Weibull distribution.
 */


gnm_float pweibull(gnm_float x, gnm_float shape, gnm_float scale, gboolean lower_tail, gboolean log_p)
{
#ifdef IEEE_754
    if (isnangnum(x) || isnangnum(shape) || isnangnum(scale))
	return x + shape + scale;
#endif
    if(shape <= 0 || scale <= 0) ML_ERR_return_NAN;

    if (x <= 0)
	return R_DT_0;
    x = -powgnum(x / scale, shape);
    if (lower_tail)
	return (log_p
		/* loggnum(1 - expgnum(x))  for x < 0 : */
		? (x > -M_LN2gnum ? loggnum(-expm1gnum(x)) : log1pgnum(-expgnum(x)))
		: -expm1gnum(x));
    /* else:  !lower_tail */
    return R_D_exp(x);
}

/* ------------------------------------------------------------------------ */
/* Imported src/nmath/pbinom.c from R.  */
/*
 *  Mathlib : A C Library of Special Functions
 *  Copyright (C) 1998 Ross Ihaka
 *  Copyright (C) 2000, 2002 The R Development Core Team
 *  Copyright (C) 2004       The R Foundation
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307 USA.
 *
 *  DESCRIPTION
 *
 *    The distribution function of the binomial distribution.
 */

gnm_float pbinom(gnm_float x, gnm_float n, gnm_float p, gboolean lower_tail, gboolean log_p)
{
#ifdef IEEE_754
    if (isnangnum(x) || isnangnum(n) || isnangnum(p))
	return x + n + p;
    if (!finitegnum(n) || !finitegnum(p)) ML_ERR_return_NAN;

#endif
    if(R_D_nonint(n)) ML_ERR_return_NAN;
    n = R_D_forceint(n);
    if(n <= 0 || p < 0 || p > 1) ML_ERR_return_NAN;

    x = floorgnum(x + 1e-7);
    if (x < 0.0) return R_DT_0;
    if (n <= x) return R_DT_1;
    return pbeta(p, x + 1, n - x, !lower_tail, log_p);
}

/* ------------------------------------------------------------------------ */
/* Imported src/nmath/dbinom.c from R.  */
/*
 * AUTHOR
 *   Catherine Loader, catherine@research.bell-labs.com.
 *   October 23, 2000.
 *
 *  Merge in to R:
 *	Copyright (C) 2000, The R Core Development Team
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307 USA.
 *
 *
 * DESCRIPTION
 *
 *   To compute the binomial probability, call dbinom(x,n,p).
 *   This checks for argument validity, and calls dbinom_raw().
 *
 *   dbinom_raw() does the actual computation; note this is called by
 *   other functions in addition to dbinom()).
 *     (1) dbinom_raw() has both p and q arguments, when one may be represented
 *         more accurately than the other (in particular, in df()).
 *     (2) dbinom_raw() does NOT check that inputs x and n are integers. This
 *         should be done in the calling function, where necessary.
 *     (3) Also does not check for 0 <= p <= 1 and 0 <= q <= 1 or NaN's.
 *         Do this in the calling function.
 */


static gnm_float dbinom_raw(gnm_float x, gnm_float n, gnm_float p, gnm_float q, gboolean give_log)
{
    gnm_float f, lc;

    if (p == 0) return((x == 0) ? R_D__1 : R_D__0);
    if (q == 0) return((x == n) ? R_D__1 : R_D__0);

    if (x == 0) {
	if(n == 0) return R_D__1;
	lc = (p < 0.1) ? -bd0(n,n*q) - n*p : n*loggnum(q);
	return( R_D_exp(lc) );
    }
    if (x == n) {
	lc = (q < 0.1) ? -bd0(n,n*p) - n*q : n*loggnum(p);
	return( R_D_exp(lc) );
    }
    if (x < 0 || x > n) return( R_D__0 );

    lc = stirlerr(n) - stirlerr(x) - stirlerr(n-x) - bd0(x,n*p) - bd0(n-x,n*q);
    f = (M_2PIgnum*x*(n-x))/n;

    return R_D_fexp(f,lc);
}

gnm_float dbinom(gnm_float x, gnm_float n, gnm_float p, gboolean give_log)
{
#ifdef IEEE_754
    /* NaNs propagated correctly */
    if (isnangnum(x) || isnangnum(n) || isnangnum(p)) return x + n + p;
#endif

    if (p < 0 || p > 1 || R_D_negInonint(n))
	ML_ERR_return_NAN;
    R_D_nonint_check(x);

    n = R_D_forceint(n);
    x = R_D_forceint(x);

    return dbinom_raw(x,n,p,1-p,give_log);
}

/* ------------------------------------------------------------------------ */
/* Imported src/nmath/qbinom.c from R.  */
/*
 *  Mathlib : A C Library of Special Functions
 *  Copyright (C) 1998 Ross Ihaka
 *  Copyright (C) 2000, 2002 The R Development Core Team
 *  Copyright (C) 2003--2004 The R Foundation
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307 USA.
 *
 *  DESCRIPTION
 *
 *	The quantile function of the binomial distribution.
 *
 *  METHOD
 *
 *	Uses the Cornish-Fisher Expansion to include a skewness
 *	correction to a normal approximation.  This gives an
 *	initial value which never seems to be off by more than
 *	1 or 2.	 A search is then conducted of values close to
 *	this initial start point.
 */


gnm_float qbinom(gnm_float p, gnm_float n, gnm_float pr, gboolean lower_tail, gboolean log_p)
{
    gnm_float q, mu, sigma, gamma, z, y;

#ifdef IEEE_754
    if (isnangnum(p) || isnangnum(n) || isnangnum(pr))
	return p + n + pr;
#endif
    if(!finitegnum(p) || !finitegnum(n) || !finitegnum(pr))
	ML_ERR_return_NAN;
    R_Q_P01_check(p);

    if(n != floorgnum(n + 0.5)) ML_ERR_return_NAN;
    if (pr < 0 || pr > 1 || n < 0)
	ML_ERR_return_NAN;

    if (pr == 0. || n == 0) return 0.;
    if (p == R_DT_0) return 0.;
    if (p == R_DT_1) return n;

    q = 1 - pr;
    if(q == 0.) return n; /* covers the full range of the distribution */
    mu = n * pr;
    sigma = sqrtgnum(n * pr * q);
    gamma = (q - pr) / sigma;

#ifdef DEBUG_qbinom
    REprintf("qbinom(p=%7" GNUM_FORMAT_g ", n=%" GNUM_FORMAT_g ", pr=%7" GNUM_FORMAT_g ", l.t.=%d, log=%d): sigm=%" GNUM_FORMAT_g ", gam=%" GNUM_FORMAT_g "\n",
	     p,n,pr, lower_tail, log_p, sigma, gamma);
#endif
    /* Note : "same" code in qpois.c, qbinom.c, qnbinom.c --
     * FIXME: This is far from optimal [cancellation for p ~= 1, etc]: */
    if(!lower_tail || log_p) {
	p = R_DT_qIv(p); /* need check again (cancellation!): */
	if (p == 0.) return 0.;
	if (p == 1.) return n;
    }
    /* temporary hack --- FIXME --- */
    if (p + 1.01*GNUM_EPSILON >= 1.) return n;

    /* y := approx.value (Cornish-Fisher expansion) :  */
    z = qnorm(p, 0., 1., /*lower_tail*/TRUE, /*log_p*/FALSE);
    y = floorgnum(mu + sigma * (z + gamma * (z*z - 1) / 6) + 0.5);
    if(y > n) /* way off */ y = n;

#ifdef DEBUG_qbinom
    REprintf("  new (p,1-p)=(%7" GNUM_FORMAT_g ",%7" GNUM_FORMAT_g "), z=qnorm(..)=%7" GNUM_FORMAT_g ", y=%5" GNUM_FORMAT_g "\n", p, 1-p, z, y);
#endif
    z = pbinom(y, n, pr, /*lower_tail*/TRUE, /*log_p*/FALSE);

    /* fuzz to ensure left continuity: */
    p *= 1 - 64*GNUM_EPSILON;

/*-- Fixme, here y can be way off --
  should use interval search instead of primitive stepping down or up */

#ifdef maybe_future
    if((lower_tail && z >= p) || (!lower_tail && z <= p)) {
#else
    if(z >= p) {
#endif
			/* search to the left */
#ifdef DEBUG_qbinom
	REprintf("\tnew z=%7" GNUM_FORMAT_g " >= p = %7" GNUM_FORMAT_g "  --> search to left (y--) ..\n", z,p);
#endif
	for(;;) {
	    if(y == 0 ||
	       (z = pbinom(y - 1, n, pr, /*l._t.*/TRUE, /*log_p*/FALSE)) < p)
		return y;
	    y = y - 1;
	}
    }
    else {		/* search to the right */
#ifdef DEBUG_qbinom
	REprintf("\tnew z=%7" GNUM_FORMAT_g " < p = %7" GNUM_FORMAT_g "  --> search to right (y++) ..\n", z,p);
#endif
	for(;;) {
	    y = y + 1;
	    if(y == n ||
	       (z = pbinom(y, n, pr, /*l._t.*/TRUE, /*log_p*/FALSE)) >= p)
		return y;
	}
    }
}

/* ------------------------------------------------------------------------ */
/* Imported src/nmath/dnbinom.c from R.  */
/*
 *  AUTHOR
 *    Catherine Loader, catherine@research.bell-labs.com.
 *    October 23, 2000 and Feb, 2001.
 *
 *  Merge in to R:
 *	Copyright (C) 2000--2001, The R Core Development Team
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307 USA.
 *
 *
 * DESCRIPTION
 *
 *   Computes the negative binomial distribution. For integer n,
 *   this is probability of x failures before the nth success in a
 *   sequence of Bernoulli trials. We do not enforce integer n, since
 *   the distribution is well defined for non-integers,
 *   and this can be useful for e.g. overdispersed discrete survival times.
 */


gnm_float dnbinom(gnm_float x, gnm_float n, gnm_float p, gboolean give_log)
{
    gnm_float prob;

#ifdef IEEE_754
    if (isnangnum(x) || isnangnum(n) || isnangnum(p))
        return x + n + p;
#endif

    if (p < 0 || p > 1 || n <= 0) ML_ERR_return_NAN;
    R_D_nonint_check(x);
    if (x < 0 || !finitegnum(x)) return R_D__0;
    x = R_D_forceint(x);

    prob = dbinom_raw(n, x+n, p, 1-p, give_log);
    p = ((gnm_float)n)/(n+x);
    return((give_log) ? loggnum(p) + prob : p * prob);
}

/* ------------------------------------------------------------------------ */
/* Imported src/nmath/pnbinom.c from R.  */
/*
 *  Mathlib : A C Library of Special Functions
 *  Copyright (C) 1998 Ross Ihaka
 *  Copyright (C) 2000 The R Development Core Team
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307 USA.
 *
 *  DESCRIPTION
 *
 *	The distribution function of the negative binomial distribution.
 *
 *  NOTES
 *
 *	x = the number of failures before the n-th success
 */


gnm_float pnbinom(gnm_float x, gnm_float n, gnm_float p, gboolean lower_tail, gboolean log_p)
{
#ifdef IEEE_754
    if (isnangnum(x) || isnangnum(n) || isnangnum(p))
	return x + n + p;
    if(!finitegnum(n) || !finitegnum(p))	ML_ERR_return_NAN;
#endif
    if (n <= 0 || p <= 0 || p >= 1)	ML_ERR_return_NAN;

    x = floorgnum(x + 1e-7);
    if (x < 0) return R_DT_0;
    if (!finitegnum(x)) return R_DT_1;
    return pbeta(p, n, x + 1, lower_tail, log_p);
}

/* ------------------------------------------------------------------------ */
/* Imported src/nmath/qnbinom.c from R.  */
/*
 *  Mathlib : A C Library of Special Functions
 *  Copyright (C) 1998 Ross Ihaka
 *  Copyright (C) 2000 The R Development Core Team
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307 USA.
 *
 *  SYNOPSIS
 *
 *	#include <Rmath.h>
 *	double qnbinom(double p, double n, double pr, int lower_tail, int log_p)
 *
 *  DESCRIPTION
 *
 *	The quantile function of the negative binomial distribution.
 *
 *  NOTES
 *
 *	x = the number of failures before the n-th success
 *
 *  METHOD
 *
 *	Uses the Cornish-Fisher Expansion to include a skewness
 *	correction to a normal approximation.  This gives an
 *	initial value which never seems to be off by more than
 *	1 or 2.	 A search is then conducted of values close to
 *	this initial start point.
 */


gnm_float qnbinom(gnm_float p, gnm_float n, gnm_float pr, gboolean lower_tail, gboolean log_p)
{
    gnm_float P, Q, mu, sigma, gamma, z, y;

#ifdef IEEE_754
    if (isnangnum(p) || isnangnum(n) || isnangnum(pr))
	return p + n + pr;
#endif
    R_Q_P01_check(p);
    if (pr <= 0 || pr >= 1 || n <= 0) ML_ERR_return_NAN;

    if (p == R_DT_0) return 0;
    if (p == R_DT_1) return gnm_pinf;
    Q = 1.0 / pr;
    P = (1.0 - pr) * Q;
    mu = n * P;
    sigma = sqrtgnum(n * P * Q);
    gamma = (Q + P)/sigma;

    /* Note : "same" code in qpois.c, qbinom.c, qnbinom.c --
     * FIXME: This is far from optimal [cancellation for p ~= 1, etc]: */
    if(!lower_tail || log_p) {
	p = R_DT_qIv(p); /* need check again (cancellation!): */
	if (p == R_DT_0) return 0;
	if (p == R_DT_1) return gnm_pinf;
    }
    /* temporary hack --- FIXME --- */
    if (p + 1.01*GNUM_EPSILON >= 1.) return gnm_pinf;

    /* y := approx.value (Cornish-Fisher expansion) :  */
    z = qnorm(p, 0., 1., /*lower_tail*/TRUE, /*log_p*/FALSE);
    y = floorgnum(mu + sigma * (z + gamma * (z*z - 1) / 6) + 0.5);

    z = pnbinom(y, n, pr, /*lower_tail*/TRUE, /*log_p*/FALSE);

    /* fuzz to ensure left continuity: */
    p *= 1 - 64*GNUM_EPSILON;

/*-- Fixme, here y can be way off --
  should use interval search instead of primitive stepping down or up */

#ifdef maybe_future
    if((lower_tail && z >= p) || (!lower_tail && z <= p)) {
#else
    if(z >= p) {
#endif
			/* search to the left */
	for(;;) {
	    if(y == 0 ||
	       (z = pnbinom(y - 1, n, pr, /*l._t.*/TRUE, /*log_p*/FALSE)) < p)
		return y;
	    y = y - 1;
	}
    }
    else {		/* search to the right */

	for(;;) {
	    y = y + 1;
	    if((z = pnbinom(y, n, pr, /*l._t.*/TRUE, /*log_p*/FALSE)) >= p)
		return y;
	}
    }
}

/* ------------------------------------------------------------------------ */
/* Imported src/nmath/dbeta.c from R.  */
/*
 *  AUTHOR
 *    Catherine Loader, catherine@research.bell-labs.com.
 *    October 23, 2000.
 *
 *  Merge in to R:
 *	Copyright (C) 2000, The R Core Development Team
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307 USA.
 *
 *
 *  DESCRIPTION
 *    Beta density,
 *                   (a+b-1)!     a-1       b-1
 *      p(x;a,b) = ------------ x     (1-x)
 *                 (a-1)!(b-1)!
 *
 *               = (a+b-1) dbinom(a-1; a+b-2,x)
 *
 *    We must modify this when a<1 or b<1, to avoid passing negative
 *    arguments to dbinom_raw. Note that the modifications require
 *    division by x and/or 1-x, so cannot be used except where necessary.
 */


gnm_float dbeta(gnm_float x, gnm_float a, gnm_float b, gboolean give_log)
{
    gnm_float f, p;
    volatile gnm_float am1, bm1; /* prevent roundoff trouble on some
                                 platforms */

#ifdef IEEE_754
    /* NaNs propagated correctly */
    if (isnangnum(x) || isnangnum(a) || isnangnum(b)) return x + a + b;
#endif

    if (a <= 0 || b <= 0) ML_ERR_return_NAN;
    if (x < 0 || x > 1) return(R_D__0);
    if (x == 0) {
	if(a > 1) return(R_D__0);
	if(a < 1) return(gnm_pinf);
	/* a == 1 : */ return(R_D_val(b));
    }
    if (x == 1) {
	if(b > 1) return(R_D__0);
	if(b < 1) return(gnm_pinf);
	/* b == 1 : */ return(R_D_val(a));
    }
    if (a < 1) {
	if (b < 1) {		/* a,b < 1 */
	    f = a*b/((a+b)*x*(1-x));
	    p = dbinom_raw(a,a+b, x,1-x, give_log);
	}
	else {			/* a < 1 <= b */
	    f = a/x;
	    bm1 = b - 1;
	    p = dbinom_raw(a,a+bm1, x,1-x, give_log);
	}
    }
    else {
	if (b < 1) {		/* a >= 1 > b */
	    f = b/(1-x);
	    am1 = a - 1;
	    p = dbinom_raw(am1,am1+b, x,1-x, give_log);
	}
	else {			/* a,b >= 1 */
	    f = a+b-1;
	    am1 = a - 1;
	    bm1 = b - 1;
	    p = dbinom_raw(am1,am1+bm1, x,1-x, give_log);
	}
    }
    return( (give_log) ? p + loggnum(f) : p*f );
}

/* ------------------------------------------------------------------------ */
/* Imported src/nmath/dhyper.c from R.  */
/*
 *  AUTHOR
 *    Catherine Loader, catherine@research.bell-labs.com.
 *    October 23, 2000.
 *
 *  Merge in to R:
 *	Copyright (C) 2000, 2001 The R Core Development Team
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307 USA.
 *
 *
 * DESCRIPTION
 *
 *    Given a sequence of r successes and b failures, we sample n (\le b+r)
 *    items without replacement. The hypergeometric probability is the
 *    probability of x successes:
 *
 *		       choose(r, x) * choose(b, n-x)
 *	p(x; r,b,n) =  -----------------------------  =
 *			       choose(r+b, n)
 *
 *		      dbinom(x,r,p) * dbinom(n-x,b,p)
 *		    = --------------------------------
 *			       dbinom(n,r+b,p)
 *
 *    for any p. For numerical stability, we take p=n/(r+b); with this choice,
 *    the denominator is not exponentially small.
 */


gnm_float dhyper(gnm_float x, gnm_float r, gnm_float b, gnm_float n, gboolean give_log)
{
    gnm_float p, q, p1, p2, p3;

#ifdef IEEE_754
    if (isnangnum(x) || isnangnum(r) || isnangnum(b) || isnangnum(n))
	return x + r + b + n;
#endif

    if (R_D_negInonint(r) || R_D_negInonint(b) || R_D_negInonint(n) || n > r+b)
	ML_ERR_return_NAN;
    if (R_D_negInonint(x))
	return(R_D__0);

    x = R_D_forceint(x);
    r = R_D_forceint(r);
    b = R_D_forceint(b);
    n = R_D_forceint(n);

    if (n < x || r < x || n - x > b) return(R_D__0);
    if (n == 0) return((x == 0) ? R_D__1 : R_D__0);

    p = ((gnm_float)n)/((gnm_float)(r+b));
    q = ((gnm_float)(r+b-n))/((gnm_float)(r+b));

    p1 = dbinom_raw(x,	r, p,q,give_log);
    p2 = dbinom_raw(n-x,b, p,q,give_log);
    p3 = dbinom_raw(n,r+b, p,q,give_log);

    return( (give_log) ? p1 + p2 - p3 : p1*p2/p3 );
}

/* ------------------------------------------------------------------------ */
/* Imported src/nmath/dexp.c from R.  */
/*
 *  Mathlib : A C Library of Special Functions
 *  Copyright (C) 1998 Ross Ihaka
 *  Copyright (C) 2000 The R Development Core Team
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 *  DESCRIPTION
 *
 *	The density of the exponential distribution.
 */


gnm_float dexp(gnm_float x, gnm_float scale, gboolean give_log)
{
#ifdef IEEE_754
    /* NaNs propagated correctly */
    if (isnangnum(x) || isnangnum(scale)) return x + scale;
#endif
    if (scale <= 0.0) ML_ERR_return_NAN;

    if (x < 0.)
	return R_D__0;
    return (give_log ?
	    (-x / scale) - loggnum(scale) :
	    expgnum(-x / scale) / scale);
}

/* ------------------------------------------------------------------------ */
/* Imported src/nmath/pexp.c from R.  */
/*
 *  Mathlib : A C Library of Special Functions
 *  Copyright (C) 1998 Ross Ihaka
 *  Copyright (C) 2000-2002 The R Development Core Team
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307 USA.
 *
 *  DESCRIPTION
 *
 *	The distribution function of the exponential distribution.
 */

gnm_float pexp(gnm_float x, gnm_float scale, gboolean lower_tail, gboolean log_p)
{
#ifdef IEEE_754
    if (isnangnum(x) || isnangnum(scale))
	return x + scale;
    if (scale < 0) ML_ERR_return_NAN;
#else
    if (scale <= 0) ML_ERR_return_NAN;
#endif

    if (x <= 0.)
	return R_DT_0;
    /* same as weibull( shape = 1): */
    x = -(x / scale);
    if (lower_tail)
	return (log_p
		/* loggnum(1 - expgnum(x))  for x < 0 : */
		? (x > -M_LN2gnum ? loggnum(-expm1gnum(x)) : log1pgnum(-expgnum(x)))
		: -expm1gnum(x));
    /* else:  !lower_tail */
    return R_D_exp(x);
}

/* ------------------------------------------------------------------------ */
/* Imported src/nmath/dgeom.c from R.  */
/*
 *  AUTHOR
 *    Catherine Loader, catherine@research.bell-labs.com.
 *    October 23, 2000.
 *
 *  Merge in to R:
 *	Copyright (C) 2000, 2001 The R Core Development Team
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307 USA.
 *
 *
 *  DESCRIPTION
 *
 *    Computes the geometric probabilities, Pr(X=x) = p(1-p)^x.
 */


gnm_float dgeom(gnm_float x, gnm_float p, gboolean give_log)
{
    gnm_float prob;

#ifdef IEEE_754
    if (isnangnum(x) || isnangnum(p)) return x + p;
#endif

    if (p < 0 || p > 1) ML_ERR_return_NAN;

    R_D_nonint_check(x);
    if (x < 0 || !finitegnum(x) || p == 0) return R_D__0;
    x = R_D_forceint(x);

    /* prob = (1-p)^x, stable for small p */
    prob = dbinom_raw(0.,x, p,1-p, give_log);

    return((give_log) ? loggnum(p) + prob : p*prob);
}

/* ------------------------------------------------------------------------ */
/* Imported src/nmath/pgeom.c from R.  */
/*
 *  Mathlib : A C Library of Special Functions
 *  Copyright (C) 1998 Ross Ihaka
 *  Copyright (C) 2000, 2001 The R Development Core Team
 *  Copyright (C) 2004	    The R Foundation
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307 USA.
 *
 *  DESCRIPTION
 *
 *    The distribution function of the geometric distribution.
 */


gnm_float pgeom(gnm_float x, gnm_float p, gboolean lower_tail, gboolean log_p)
{
#ifdef IEEE_754
    if (isnangnum(x) || isnangnum(p))
	return x + p;
#endif
    x = floorgnum(x +1e-7);
    if(p < 0 || p > 1) ML_ERR_return_NAN;

    if (x < 0. || p == 0.) return R_DT_0;
    if (!finitegnum(x)) return R_DT_1;

    if(p == 1.) { /* we cannot assume IEEE */
	x = lower_tail ? 1: 0;
	return log_p ? loggnum(x) : x;
    }
    x = log1pgnum(-p) * (x + 1);
    if (log_p)
	return R_DT_Clog(x);
    else
	return lower_tail ? -expm1gnum(x) : expgnum(x);
}

/* ------------------------------------------------------------------------ */
/* Imported src/nmath/dcauchy.c from R.  */
/*
 *  Mathlib : A C Library of Special Functions
 *  Copyright (C) 1998 Ross Ihaka
 *  Copyright (C) 2000 The R Development Core Team
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 *  DESCRIPTION
 *
 *    The density of the Cauchy distribution.
 */


gnm_float dcauchy(gnm_float x, gnm_float location, gnm_float scale, gboolean give_log)
{
    gnm_float y;
#ifdef IEEE_754
    /* NaNs propagated correctly */
    if (isnangnum(x) || isnangnum(location) || isnangnum(scale))
	return x + location + scale;
#endif
    if (scale <= 0) ML_ERR_return_NAN;

    y = (x - location) / scale;
    return give_log ?
	- loggnum(M_PIgnum * scale * (1. + y * y)) :
	1. / (M_PIgnum * scale * (1. + y * y));
}

/* ------------------------------------------------------------------------ */
/* Imported src/nmath/bessel.h from R.  */

/* Constants und Documentation that apply to several of the
 * ./bessel_[ijky].c  files */

/* *******************************************************************

 Explanation of machine-dependent constants

   beta	  = Radix for the floating-point system
   minexp = Smallest representable power of beta
   maxexp = Smallest power of beta that overflows
   it = p = Number of bits (base-beta digits) in the mantissa
	    (significand) of a working precision (floating-point) variable
   NSIG	  = Decimal significance desired.  Should be set to
	    INT(LOG10(2)*it+1).	 Setting NSIG lower will result
	    in decreased accuracy while setting NSIG higher will
	    increase CPU time without increasing accuracy.  The
	    truncation error is limited to a relative error of
	    T=.5*10^(-NSIG).
   ENTEN  = 10 ^ K, where K is the largest long such that
	    ENTEN is machine-representable in working precision
   ENSIG  = 10 ^ NSIG
   RTNSIG = 10 ^ (-K) for the smallest long K such that
	    K >= NSIG/4
   ENMTEN = Smallest ABS(X) such that X/4 does not underflow
   XINF	  = Largest positive machine number; approximately beta ^ maxexp
	    == GNUM_MAX (defined in  #include <float.h>)
   SQXMIN = Square root of beta ^ minexp = sqrtgnum(GNUM_MIN)

   EPS	  = The smallest positive floating-point number such that 1.0+EPS > 1.0
	  = beta ^ (-p)	 == GNUM_EPSILON


  For I :

   EXPARG = Largest working precision argument that the library
	    EXP routine can handle and upper limit on the
	    magnitude of X when IZE=1; approximately LOG(beta ^ maxexp)

  For I and J :

   xlrg_IJ = (was = XLARGE). Upper limit on the magnitude of X (when
	    IZE=2 for I()).  Bear in mind that if ABS(X)=N, then at least
	    N iterations of the backward recursion will be executed.
	    The value of 10 ^ 4 is used on every machine.

  For j :
   XMIN_J  = Smallest acceptable argument for RBESY; approximately
	    max(2*beta ^ minexp, 2/XINF), rounded up

  For Y :

   xlrg_Y =  (was = XLARGE). Upper bound on X;
	    approximately 1/DEL, because the sine and cosine functions
	    have lost about half of their precision at that point.

   EPS_SINC = Machine number below which singnum(x)/x = 1; approximately SQRT(EPS).
   THRESH = Lower bound for use of the asymptotic form;
	    approximately AINT(-LOG10(EPS/2.0))+1.0


  For K :

   xmax_k =  (was = XMAX). Upper limit on the magnitude of X when ize = 1;
	    i.e. maximal x for UNscaled answer.

	    Solution to equation:
	       W(X) * (1 -1/8 X + 9/128 X^2) = beta ^ minexp
	    where  W(X) = EXP(-X)*SQRT(PI/2X)

 --------------------------------------------------------------------

     Approximate values for some important machines are:

		  beta minexp maxexp it NSIG ENTEN ENSIG RTNSIG ENMTEN	 EXPARG
 IEEE (IBM/XT,
   SUN, etc.) (S.P.)  2	  -126	128  24	  8  1e38   1e8	  1e-2	4.70e-38     88
 IEEE	(...) (D.P.)  2	 -1022 1024  53	 16  1e308  1e16  1e-4	8.90e-308   709
 CRAY-1	      (S.P.)  2	 -8193 8191  48	 15  1e2465 1e15  1e-4	1.84e-2466 5677
 Cyber 180/855
   under NOS  (S.P.)  2	  -975 1070  48	 15  1e322  1e15  1e-4	1.25e-293   741
 IBM 3033     (D.P.) 16	   -65	 63  14	  5  1e75   1e5	  1e-2	2.16e-78    174
 VAX	      (S.P.)  2	  -128	127  24	  8  1e38   1e8	  1e-2	1.17e-38     88
 VAX D-Format (D.P.)  2	  -128	127  56	 17  1e38   1e17  1e-5	1.17e-38     88
 VAX G-Format (D.P.)  2	 -1024 1023  53	 16  1e307  1e16  1e-4	2.22e-308   709


And routine specific :

		    xlrg_IJ xlrg_Y xmax_k EPS_SINC XMIN_J    XINF   THRESH
 IEEE (IBM/XT,
   SUN, etc.) (S.P.)	1e4  1e4   85.337  1e-4	 2.36e-38   3.40e38	8.
 IEEE	(...) (D.P.)	1e4  1e8  705.342  1e-8	 4.46e-308  1.79e308   16.
 CRAY-1	      (S.P.)	1e4  2e7 5674.858  5e-8	 3.67e-2466 5.45e2465  15.
 Cyber 180/855
   under NOS  (S.P.)	1e4  2e7  672.788  5e-8	 6.28e-294  1.26e322   15.
 IBM 3033     (D.P.)	1e4  1e8  177.852  1e-8	 2.77e-76   7.23e75    17.
 VAX	      (S.P.)	1e4  1e4   86.715  1e-4	 1.18e-38   1.70e38	8.
 VAX e-Format (D.P.)	1e4  1e9   86.715  1e-9	 1.18e-38   1.70e38    17.
 VAX G-Format (D.P.)	1e4  1e8  706.728  1e-8	 2.23e-308  8.98e307   16.

*/
#define nsig_BESS	16
#define ensig_BESS	1e16
#define rtnsig_BESS	1e-4
#define enmten_BESS	8.9e-308
#define enten_BESS	1e308

#define exparg_BESS	709.
#define xlrg_BESS_IJ	1e4
#define xlrg_BESS_Y	1e8
#define thresh_BESS_Y	16.

#define xmax_BESS_K	705.342/* maximal x for UNscaled answer */


/* sqrtgnum(GNUM_MIN) =	1.491668e-154 */
#define sqxmin_BESS_K	1.49e-154

/* x < eps_sinc	 <==>  singnum(x)/x == 1 (particularly "==>");
  Linux (around 2001-02) gives GNM_const(2.14946906753213e-08)
  Solaris 2.5.1		 gives GNM_const(2.14911933289084e-08)
*/
#define M_eps_sinc	2.149e-8

/* ------------------------------------------------------------------------ */
/* Imported src/nmath/bessel_i.c from R.  */
/*
 *  Mathlib : A C Library of Special Functions
 *  Copyright (C) 1998-2001 Ross Ihaka and the R Development Core team.
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 */

/*  DESCRIPTION --> see below */


/* From http://www.netlib.org/specfun/ribesl	Fortran translated by f2c,...
 *	------------------------------=#----	Martin Maechler, ETH Zurich
 */

#ifndef MATHLIB_STANDALONE
#endif

static void I_bessel(gnm_float *x, gnm_float *alpha, long *nb,
		     long *ize, gnm_float *bi, long *ncalc);

gnm_float bessel_i(gnm_float x, gnm_float alpha, gnm_float expo)
{
    long nb, ncalc, ize;
    gnm_float *bi;
#ifndef MATHLIB_STANDALONE
    char *vmax;
#endif

#ifdef IEEE_754
    /* NaNs propagated correctly */
    if (isnangnum(x) || isnangnum(alpha)) return x + alpha;
#endif
    if (x < 0) {
	ML_ERROR(ME_RANGE);
	return gnm_nan;
    }
    ize = (long)expo;
    if (alpha < 0) {
	/* Using Abramowitz & Stegun  9.6.2
	 * this may not be quite optimal (CPU and accuracy wise) */
	return(bessel_i(x, -alpha, expo) +
	       bessel_k(x, -alpha, expo) * ((ize == 1)? 2. : 2.*expgnum(-x))/M_PIgnum
	       * singnum(-M_PIgnum * alpha));
    }
    nb = 1+ (long)floorgnum(alpha);/* nb-1 <= alpha < nb */
    alpha -= (nb-1);
#ifdef MATHLIB_STANDALONE
    bi = (gnm_float *) calloc(nb, sizeof(gnm_float));
    if (!bi) MATHLIB_ERROR("%s", "bessel_i allocation error");
#else
    vmax = vmaxget();
    bi = (gnm_float *) R_alloc(nb, sizeof(gnm_float));
#endif
    I_bessel(&x, &alpha, &nb, &ize, bi, &ncalc);
    if(ncalc != nb) {/* error input */
	if(ncalc < 0)
	    MATHLIB_WARNING4("bessel_i(%" GNUM_FORMAT_g "): ncalc (=%ld) != nb (=%ld); alpha=%" GNUM_FORMAT_g "."
			     " Arg. out of range?\n",
			     x, ncalc, nb, alpha);
	else
	    MATHLIB_WARNING2("bessel_i(%" GNUM_FORMAT_g ",nu=%" GNUM_FORMAT_g "): precision lost in result\n",
			     x, alpha+nb-1);
    }
    x = bi[nb-1];
#ifdef MATHLIB_STANDALONE
    free(bi);
#else
    vmaxset(vmax);
#endif
    return x;
}

static void I_bessel(gnm_float *x, gnm_float *alpha, long *nb,
		     long *ize, gnm_float *bi, long *ncalc)
{
/* -------------------------------------------------------------------

 This routine calculates Bessel functions I_(N+ALPHA) (X)
 for non-negative argument X, and non-negative order N+ALPHA,
 with or without exponential scaling.


 Explanation of variables in the calling sequence

 X     - Non-negative argument for which
	 I's or exponentially scaled I's (I*EXP(-X))
	 are to be calculated.	If I's are to be calculated,
	 X must be less than EXPARG_BESS (see bessel.h).
 ALPHA - Fractional part of order for which
	 I's or exponentially scaled I's (I*EXP(-X)) are
	 to be calculated.  0 <= ALPHA < 1.0.
 NB    - Number of functions to be calculated, NB > 0.
	 The first function calculated is of order ALPHA, and the
	 last is of order (NB - 1 + ALPHA).
 IZE   - Type.	IZE = 1 if unscaled I's are to be calculated,
		    = 2 if exponentially scaled I's are to be calculated.
 BI    - Output vector of length NB.	If the routine
	 terminates normally (NCALC=NB), the vector BI contains the
	 functions I(ALPHA,X) through I(NB-1+ALPHA,X), or the
	 corresponding exponentially scaled functions.
 NCALC - Output variable indicating possible errors.
	 Before using the vector BI, the user should check that
	 NCALC=NB, i.e., all orders have been calculated to
	 the desired accuracy.	See error returns below.


 *******************************************************************
 *******************************************************************

 Error returns

  In case of an error,	NCALC != NB, and not all I's are
  calculated to the desired accuracy.

  NCALC < 0:  An argument is out of range. For example,
     NB <= 0, IZE is not 1 or 2, or IZE=1 and ABS(X) >= EXPARG_BESS.
     In this case, the BI-vector is not calculated, and NCALC is
     set to MIN0(NB,0)-1 so that NCALC != NB.

  NB > NCALC > 0: Not all requested function values could
     be calculated accurately.	This usually occurs because NB is
     much larger than ABS(X).  In this case, BI[N] is calculated
     to the desired accuracy for N <= NCALC, but precision
     is lost for NCALC < N <= NB.  If BI[N] does not vanish
     for N > NCALC (because it is too small to be represented),
     and BI[N]/BI[NCALC] = 10**(-K), then only the first NSIG-K
     significant figures of BI[N] can be trusted.


 Intrinsic functions required are:

     DBLE, EXP, gamma_cody, INT, MAX, MIN, REAL, SQRT


 Acknowledgement

  This program is based on a program written by David J.
  Sookne (2) that computes values of the Bessel functions J or
  I of float argument and long order.  Modifications include
  the restriction of the computation to the I Bessel function
  of non-negative float argument, the extension of the computation
  to arbitrary positive order, the inclusion of optional
  exponential scaling, and the elimination of most underflow.
  An earlier version was published in (3).

 References: "A Note on Backward Recurrence Algorithms," Olver,
	      F. W. J., and Sookne, D. J., Math. Comp. 26, 1972,
	      pp 941-947.

	     "Bessel Functions of Real Argument and Integer Order,"
	      Sookne, D. J., NBS Jour. of Res. B. 77B, 1973, pp
	      125-132.

	     "ALGORITHM 597, Sequence of Modified Bessel Functions
	      of the First Kind," Cody, W. J., Trans. Math. Soft.,
	      1983, pp. 242-245.

  Latest modification: May 30, 1989

  Modified by: W. J. Cody and L. Stoltz
	       Applied Mathematics Division
	       Argonne National Laboratory
	       Argonne, IL  60439
*/

    /*-------------------------------------------------------------------
      Mathematical constants
      -------------------------------------------------------------------*/
    const gnm_float const__ = 1.585;

    /* Local variables */
    long nend, intx, nbmx, k, l, n, nstart;
    gnm_float pold, test,	p, em, en, empal, emp2al, halfx,
	aa, bb, cc, psave, plast, tover, psavel, sum, nu, twonu;

    /*Parameter adjustments */
    --bi;
    nu = *alpha;
    twonu = nu + nu;

    /*-------------------------------------------------------------------
      Check for X, NB, OR IZE out of range.
      ------------------------------------------------------------------- */
    if (*nb > 0 && *x >= 0. &&	(0. <= nu && nu < 1.) &&
	(1 <= *ize && *ize <= 2) ) {

	*ncalc = *nb;
	if((*ize == 1 && *x > exparg_BESS) ||
	   (*ize == 2 && *x > xlrg_BESS_IJ)) {
	    ML_ERROR(ME_RANGE);
	    for(k=1; k <= *nb; k++)
		bi[k]=gnm_pinf;
	    return;
	}
	intx = (long) (*x);/* --> we will probably fail when *x > LONG_MAX */
	if (*x >= rtnsig_BESS) { /* "non-small" x */
/* -------------------------------------------------------------------
   Initialize the forward sweep, the P-sequence of Olver
   ------------------------------------------------------------------- */
	    nbmx = *nb - intx;
	    n = intx + 1;
	    en = (gnm_float) (n + n) + twonu;
	    plast = 1.;
	    p = en / *x;
	    /* ------------------------------------------------
	       Calculate general significance test
	       ------------------------------------------------ */
	    test = ensig_BESS + ensig_BESS;
	    if (intx << 1 > nsig_BESS * 5) {
		test = sqrtgnum(test * p);
	    } else {
		test /= powgnum(const__, (gnm_float)intx);
	    }
	    if (nbmx >= 3) {
		/* --------------------------------------------------
		   Calculate P-sequence until N = NB-1
		   Check for possible overflow.
		   ------------------------------------------------ */
		tover = enten_BESS / ensig_BESS;
		nstart = intx + 2;
		nend = *nb - 1;
		for (k = nstart; k <= nend; ++k) {
		    n = k;
		    en += 2.;
		    pold = plast;
		    plast = p;
		    p = en * plast / *x + pold;
		    if (p > tover) {
			/* ------------------------------------------------
			   To avoid overflow, divide P-sequence by TOVER.
			   Calculate P-sequence until ABS(P) > 1.
			   ---------------------------------------------- */
			tover = enten_BESS;
			p /= tover;
			plast /= tover;
			psave = p;
			psavel = plast;
			nstart = n + 1;
			do {
			    ++n;
			    en += 2.;
			    pold = plast;
			    plast = p;
			    p = en * plast / *x + pold;
			}
			while (p <= 1.);

			bb = en / *x;
			/* ------------------------------------------------
			   Calculate backward test, and find NCALC,
			   the highest N such that the test is passed.
			   ------------------------------------------------ */
			test = pold * plast / ensig_BESS;
			test *= .5 - .5 / (bb * bb);
			p = plast * tover;
			--n;
			en -= 2.;
			nend = imin2(*nb,n);
			for (l = nstart; l <= nend; ++l) {
			    *ncalc = l;
			    pold = psavel;
			    psavel = psave;
			    psave = en * psavel / *x + pold;
			    if (psave * psavel > test) {
				goto L90;
			    }
			}
			*ncalc = nend + 1;
L90:
			--(*ncalc);
			goto L120;
		    }
		}
		n = nend;
		en = (gnm_float)(n + n) + twonu;
		/*---------------------------------------------------
		  Calculate special significance test for NBMX > 2.
		  --------------------------------------------------- */
		test = fmax2(test,sqrtgnum(plast * ensig_BESS) * sqrtgnum(p + p));
	    }
	    /* --------------------------------------------------------
	       Calculate P-sequence until significance test passed.
	       -------------------------------------------------------- */
	    do {
		++n;
		en += 2.;
		pold = plast;
		plast = p;
		p = en * plast / *x + pold;
	    } while (p < test);

L120:
/* -------------------------------------------------------------------
 Initialize the backward recursion and the normalization sum.
 ------------------------------------------------------------------- */
	    ++n;
	    en += 2.;
	    bb = 0.;
	    aa = 1. / p;
	    em = (gnm_float) n - 1.;
	    empal = em + nu;
	    emp2al = em - 1. + twonu;
	    sum = aa * empal * emp2al / em;
	    nend = n - *nb;
	    if (nend < 0) {
		/* -----------------------------------------------------
		   N < NB, so store BI[N] and set higher orders to 0..
		   ----------------------------------------------------- */
		bi[n] = aa;
		nend = -nend;
		for (l = 1; l <= nend; ++l) {
		    bi[n + l] = 0.;
		}
	    } else {
		if (nend > 0) {
		    /* -----------------------------------------------------
		       Recur backward via difference equation,
		       calculating (but not storing) BI[N], until N = NB.
		       --------------------------------------------------- */
		    for (l = 1; l <= nend; ++l) {
			--n;
			en -= 2.;
			cc = bb;
			bb = aa;
			aa = en * bb / *x + cc;
			em -= 1.;
			emp2al -= 1.;
			if (n == 1) {
			    break;
			}
			if (n == 2) {
			    emp2al = 1.;
			}
			empal -= 1.;
			sum = (sum + aa * empal) * emp2al / em;
		    }
		}
		/* ---------------------------------------------------
		   Store BI[NB]
		   --------------------------------------------------- */
		bi[n] = aa;
		if (*nb <= 1) {
		    sum = sum + sum + aa;
		    goto L230;
		}
		/* -------------------------------------------------
		   Calculate and Store BI[NB-1]
		   ------------------------------------------------- */
		--n;
		en -= 2.;
		bi[n] = en * aa / *x + bb;
		if (n == 1) {
		    goto L220;
		}
		em -= 1.;
		if (n == 2)
		    emp2al = 1.;
		else
		    emp2al -= 1.;

		empal -= 1.;
		sum = (sum + bi[n] * empal) * emp2al / em;
	    }
	    nend = n - 2;
	    if (nend > 0) {
		/* --------------------------------------------
		   Calculate via difference equation
		   and store BI[N], until N = 2.
		   ------------------------------------------ */
		for (l = 1; l <= nend; ++l) {
		    --n;
		    en -= 2.;
		    bi[n] = en * bi[n + 1] / *x + bi[n + 2];
		    em -= 1.;
		    if (n == 2)
			emp2al = 1.;
		    else
			emp2al -= 1.;
		    empal -= 1.;
		    sum = (sum + bi[n] * empal) * emp2al / em;
		}
	    }
	    /* ----------------------------------------------
	       Calculate BI[1]
	       -------------------------------------------- */
	    bi[1] = 2. * empal * bi[2] / *x + bi[3];
L220:
	    sum = sum + sum + bi[1];

L230:
	    /* ---------------------------------------------------------
	       Normalize.  Divide all BI[N] by sum.
	       --------------------------------------------------------- */
	    if (nu != 0.)
		sum *= (expgnum(lgamma1p (nu)) * powgnum(*x * .5, -nu));
	    if (*ize == 1)
		sum *= expgnum(-(*x));
	    aa = enmten_BESS;
	    if (sum > 1.)
		aa *= sum;
	    for (n = 1; n <= *nb; ++n) {
		if (bi[n] < aa)
		    bi[n] = 0.;
		else
		    bi[n] /= sum;
	    }
	    return;
	} else {
	    /* -----------------------------------------------------------
	       Two-term ascending series for small X.
	       -----------------------------------------------------------*/
	    aa = 1.;
	    empal = 1. + nu;
	    if (*x > enmten_BESS)
		halfx = .5 * *x;
	    else
		halfx = 0.;
	    if (nu != 0.)
		aa = powgnum(halfx, nu) / expgnum(lgamma1p(nu));
	    if (*ize == 2)
		aa *= expgnum(-(*x));
	    if (*x + 1. > 1.)
		bb = halfx * halfx;
	    else
		bb = 0.;

	    bi[1] = aa + aa * bb / empal;
	    if (*x != 0. && bi[1] == 0.)
		*ncalc = 0;
	    if (*nb > 1) {
		if (*x == 0.) {
		    for (n = 2; n <= *nb; ++n) {
			bi[n] = 0.;
		    }
		} else {
		    /* -------------------------------------------------
		       Calculate higher-order functions.
		       ------------------------------------------------- */
		    cc = halfx;
		    tover = (enmten_BESS + enmten_BESS) / *x;
		    if (bb != 0.)
			tover = enmten_BESS / bb;
		    for (n = 2; n <= *nb; ++n) {
			aa /= empal;
			empal += 1.;
			aa *= cc;
			if (aa <= tover * empal)
			    bi[n] = aa = 0.;
			else
			    bi[n] = aa + aa * bb / empal;
			if (bi[n] == 0. && *ncalc > n)
			    *ncalc = n - 1;
		    }
		}
	    }
	}
    } else {
	*ncalc = imin2(*nb,0) - 1;
    }
}

/* ------------------------------------------------------------------------ */
/* Imported src/nmath/bessel_k.c from R.  */
/*
 *  Mathlib : A C Library of Special Functions
 *  Copyright (C) 1998-2001 Ross Ihaka and the R Development Core team.
 *  Copyright (C) 2002-3    The R Foundation
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 */

/*  DESCRIPTION --> see below */


/* From http://www.netlib.org/specfun/rkbesl	Fortran translated by f2c,...
 *	------------------------------=#----	Martin Maechler, ETH Zurich
 */

#ifndef MATHLIB_STANDALONE
#endif

static void K_bessel(gnm_float *x, gnm_float *alpha, long *nb,
		     long *ize, gnm_float *bk, long *ncalc);

gnm_float bessel_k(gnm_float x, gnm_float alpha, gnm_float expo)
{
    long nb, ncalc, ize;
    gnm_float *bk;
#ifndef MATHLIB_STANDALONE
    char *vmax;
#endif

#ifdef IEEE_754
    /* NaNs propagated correctly */
    if (isnangnum(x) || isnangnum(alpha)) return x + alpha;
#endif
    if (x < 0) {
	ML_ERROR(ME_RANGE);
	return gnm_nan;
    }
    ize = (long)expo;
    if(alpha < 0)
	alpha = -alpha;
    nb = 1+ (long)floorgnum(alpha);/* nb-1 <= |alpha| < nb */
    alpha -= (nb-1);
#ifdef MATHLIB_STANDALONE
    bk = (gnm_float *) calloc(nb, sizeof(gnm_float));
    if (!bk) MATHLIB_ERROR("%s", "bessel_k allocation error");
#else
    vmax = vmaxget();
    bk = (gnm_float *) R_alloc(nb, sizeof(gnm_float));
#endif
    K_bessel(&x, &alpha, &nb, &ize, bk, &ncalc);
    if(ncalc != nb) {/* error input */
      if(ncalc < 0)
	MATHLIB_WARNING4("bessel_k(%" GNUM_FORMAT_g "): ncalc (=%ld) != nb (=%ld); alpha=%" GNUM_FORMAT_g ". Arg. out of range?\n",
			 x, ncalc, nb, alpha);
      else
	MATHLIB_WARNING2("bessel_k(%" GNUM_FORMAT_g ",nu=%" GNUM_FORMAT_g "): precision lost in result\n",
			 x, alpha+nb-1);
    }
    x = bk[nb-1];
#ifdef MATHLIB_STANDALONE
    free(bk);
#else
    vmaxset(vmax);
#endif
    return x;
}

static void K_bessel(gnm_float *x, gnm_float *alpha, long *nb,
		     long *ize, gnm_float *bk, long *ncalc)
{
/*-------------------------------------------------------------------

  This routine calculates modified Bessel functions
  of the third kind, K_(N+ALPHA) (X), for non-negative
  argument X, and non-negative order N+ALPHA, with or without
  exponential scaling.

  Explanation of variables in the calling sequence

 X     - Non-negative argument for which
	 K's or exponentially scaled K's (K*EXP(X))
	 are to be calculated.	If K's are to be calculated,
	 X must not be greater than XMAX_BESS_K.
 ALPHA - Fractional part of order for which
	 K's or exponentially scaled K's (K*EXP(X)) are
	 to be calculated.  0 <= ALPHA < 1.0.
 NB    - Number of functions to be calculated, NB > 0.
	 The first function calculated is of order ALPHA, and the
	 last is of order (NB - 1 + ALPHA).
 IZE   - Type.	IZE = 1 if unscaled K's are to be calculated,
		    = 2 if exponentially scaled K's are to be calculated.
 BK    - Output vector of length NB.	If the
	 routine terminates normally (NCALC=NB), the vector BK
	 contains the functions K(ALPHA,X), ... , K(NB-1+ALPHA,X),
	 or the corresponding exponentially scaled functions.
	 If (0 < NCALC < NB), BK(I) contains correct function
	 values for I <= NCALC, and contains the ratios
	 K(ALPHA+I-1,X)/K(ALPHA+I-2,X) for the rest of the array.
 NCALC - Output variable indicating possible errors.
	 Before using the vector BK, the user should check that
	 NCALC=NB, i.e., all orders have been calculated to
	 the desired accuracy.	See error returns below.


 *******************************************************************

 Error returns

  In case of an error, NCALC != NB, and not all K's are
  calculated to the desired accuracy.

  NCALC < -1:  An argument is out of range. For example,
	NB <= 0, IZE is not 1 or 2, or IZE=1 and ABS(X) >= XMAX_BESS_K.
	In this case, the B-vector is not calculated,
	and NCALC is set to MIN0(NB,0)-2	 so that NCALC != NB.
  NCALC = -1:  Either  K(ALPHA,X) >= XINF  or
	K(ALPHA+NB-1,X)/K(ALPHA+NB-2,X) >= XINF.	 In this case,
	the B-vector is not calculated.	Note that again
	NCALC != NB.

  0 < NCALC < NB: Not all requested function values could
	be calculated accurately.  BK(I) contains correct function
	values for I <= NCALC, and contains the ratios
	K(ALPHA+I-1,X)/K(ALPHA+I-2,X) for the rest of the array.


 Intrinsic functions required are:

     ABS, AINT, EXP, INT, LOG, MAX, MIN, SINH, SQRT


 Acknowledgement

	This program is based on a program written by J. B. Campbell
	(2) that computes values of the Bessel functions K of float
	argument and float order.  Modifications include the addition
	of non-scaled functions, parameterization of machine
	dependencies, and the use of more accurate approximations
	for SINH and SIN.

 References: "On Temme's Algorithm for the Modified Bessel
	      Functions of the Third Kind," Campbell, J. B.,
	      TOMS 6(4), Dec. 1980, pp. 581-586.

	     "A FORTRAN IV Subroutine for the Modified Bessel
	      Functions of the Third Kind of Real Order and Real
	      Argument," Campbell, J. B., Report NRC/ERB-925,
	      National Research Council, Canada.

  Latest modification: May 30, 1989

  Modified by: W. J. Cody and L. Stoltz
	       Applied Mathematics Division
	       Argonne National Laboratory
	       Argonne, IL  60439

 -------------------------------------------------------------------
*/
    /*---------------------------------------------------------------------
     * Mathematical constants
     *	A = LOG(2) - Euler's constant
     *	D = SQRT(2/PI)
     ---------------------------------------------------------------------*/
    const gnm_float a = GNM_const(.11593151565841244881);

    /*---------------------------------------------------------------------
      P, Q - Approximation for LOG(GAMMA(1+ALPHA))/ALPHA + Euler's constant
      Coefficients converted from hex to decimal and modified
      by W. J. Cody, 2/26/82 */
    static const gnm_float p[8] = { GNM_const(.805629875690432845),GNM_const(20.4045500205365151),
	    GNM_const(157.705605106676174),GNM_const(536.671116469207504),GNM_const(900.382759291288778),
	    GNM_const(730.923886650660393),GNM_const(229.299301509425145),GNM_const(.822467033424113231) };
    static const gnm_float q[7] = { GNM_const(29.4601986247850434),GNM_const(277.577868510221208),
	    GNM_const(1206.70325591027438),GNM_const(2762.91444159791519),GNM_const(3443.74050506564618),
	    GNM_const(2210.63190113378647),GNM_const(572.267338359892221) };
    /* R, S - Approximation for (1-ALPHA*PI/SIN(ALPHA*PI))/(2.D0*ALPHA) */
    static const gnm_float r[5] = { GNM_const(-.48672575865218401848),GNM_const(13.079485869097804016),
	    GNM_const(-101.96490580880537526),GNM_const(347.65409106507813131),
	    GNM_const(3.495898124521934782e-4) };
    static const gnm_float s[4] = { GNM_const(-25.579105509976461286),GNM_const(212.57260432226544008),
	    GNM_const(-610.69018684944109624),GNM_const(422.69668805777760407) };
    /* T    - Approximation for SINH(Y)/Y */
    static const gnm_float t[6] = { GNM_const(1.6125990452916363814e-10),
	    GNM_const(2.5051878502858255354e-8),GNM_const(2.7557319615147964774e-6),
	    GNM_const(1.9841269840928373686e-4),GNM_const(.0083333333333334751799),
	    GNM_const(.16666666666666666446) };
    /*---------------------------------------------------------------------*/
    static const gnm_float estm[6] = { 52.0583,5.7607,2.7782,14.4303,185.3004, 9.3715 };
    static const gnm_float estf[7] = { 41.8341,7.1075,6.4306,42.511,1.35633,84.5096,20.};

    /* Local variables */
    long iend, i, j, k, m, ii, mplus1;
    gnm_float x2by4, twox, c, blpha, ratio, wminf;
    gnm_float d1, d2, d3, f0, f1, f2, p0, q0, t1, t2, twonu;
    gnm_float dm, ex, bk1, bk2, nu;

    ii = 0; /* -Wall */

    ex = *x;
    nu = *alpha;
    *ncalc = imin2(*nb,0) - 2;
    if (*nb > 0 && (0. <= nu && nu < 1.) && (1 <= *ize && *ize <= 2)) {
	if(ex <= 0 || (*ize == 1 && ex > xmax_BESS_K)) {
	    if(ex <= 0) {
		ML_ERROR(ME_RANGE);
		for(i=0; i < *nb; i++)
		    bk[i] = gnm_pinf;
	    } else /* would only have underflow */
		for(i=0; i < *nb; i++)
		    bk[i] = 0.;
	    *ncalc = *nb;
	    return;
	}
	k = 0;
	if (nu < sqxmin_BESS_K) {
	    nu = 0.;
	} else if (nu > .5) {
	    k = 1;
	    nu -= 1.;
	}
	twonu = nu + nu;
	iend = *nb + k - 1;
	c = nu * nu;
	d3 = -c;
	if (ex <= 1.) {
	    /* ------------------------------------------------------------
	       Calculation of P0 = GAMMA(1+ALPHA) * (2/X)**ALPHA
			      Q0 = GAMMA(1-ALPHA) * (X/2)**ALPHA
	       ------------------------------------------------------------ */
	    d1 = 0.; d2 = p[0];
	    t1 = 1.; t2 = q[0];
	    for (i = 2; i <= 7; i += 2) {
		d1 = c * d1 + p[i - 1];
		d2 = c * d2 + p[i];
		t1 = c * t1 + q[i - 1];
		t2 = c * t2 + q[i];
	    }
	    d1 = nu * d1;
	    t1 = nu * t1;
	    f1 = loggnum(ex);
	    f0 = a + nu * (p[7] - nu * (d1 + d2) / (t1 + t2)) - f1;
	    q0 = expgnum(-nu * (a - nu * (p[7] + nu * (d1-d2) / (t1-t2)) - f1));
	    f1 = nu * f0;
	    p0 = expgnum(f1);
	    /* -----------------------------------------------------------
	       Calculation of F0 =
	       ----------------------------------------------------------- */
	    d1 = r[4];
	    t1 = 1.;
	    for (i = 0; i < 4; ++i) {
		d1 = c * d1 + r[i];
		t1 = c * t1 + s[i];
	    }
	    /* d2 := sinhgnum(f1)/ nu = sinhgnum(f1)/(f1/f0)
	     *	   = f0 * sinhgnum(f1)/f1 */
	    if (gnumabs(f1) <= .5) {
		f1 *= f1;
		d2 = 0.;
		for (i = 0; i < 6; ++i) {
		    d2 = f1 * d2 + t[i];
		}
		d2 = f0 + f0 * f1 * d2;
	    } else {
		d2 = sinhgnum(f1) / nu;
	    }
	    f0 = d2 - nu * d1 / (t1 * p0);
	    if (ex <= 1e-10) {
		/* ---------------------------------------------------------
		   X <= 1.0E-10
		   Calculation of K(ALPHA,X) and X*K(ALPHA+1,X)/K(ALPHA,X)
		   --------------------------------------------------------- */
		bk[0] = f0 + ex * f0;
		if (*ize == 1) {
		    bk[0] -= ex * bk[0];
		}
		ratio = p0 / f0;
		c = ex * GNUM_MAX;
		if (k != 0) {
		    /* ---------------------------------------------------
		       Calculation of K(ALPHA,X)
		       and  X*K(ALPHA+1,X)/K(ALPHA,X),	ALPHA >= 1/2
		       --------------------------------------------------- */
		    *ncalc = -1;
		    if (bk[0] >= c / ratio) {
			return;
		    }
		    bk[0] = ratio * bk[0] / ex;
		    twonu += 2.;
		    ratio = twonu;
		}
		*ncalc = 1;
		if (*nb == 1)
		    return;

		/* -----------------------------------------------------
		   Calculate  K(ALPHA+L,X)/K(ALPHA+L-1,X),
		   L = 1, 2, ... , NB-1
		   ----------------------------------------------------- */
		*ncalc = -1;
		for (i = 1; i < *nb; ++i) {
		    if (ratio >= c)
			return;

		    bk[i] = ratio / ex;
		    twonu += 2.;
		    ratio = twonu;
		}
		*ncalc = 1;
		goto L420;
	    } else {
		/* ------------------------------------------------------
		   10^-10 < X <= 1.0
		   ------------------------------------------------------ */
		c = 1.;
		x2by4 = ex * ex / 4.;
		p0 = .5 * p0;
		q0 = .5 * q0;
		d1 = -1.;
		d2 = 0.;
		bk1 = 0.;
		bk2 = 0.;
		f1 = f0;
		f2 = p0;
		do {
		    d1 += 2.;
		    d2 += 1.;
		    d3 = d1 + d3;
		    c = x2by4 * c / d2;
		    f0 = (d2 * f0 + p0 + q0) / d3;
		    p0 /= d2 - nu;
		    q0 /= d2 + nu;
		    t1 = c * f0;
		    t2 = c * (p0 - d2 * f0);
		    bk1 += t1;
		    bk2 += t2;
		} while (gnumabs(t1 / (f1 + bk1)) > GNUM_EPSILON ||
			 gnumabs(t2 / (f2 + bk2)) > GNUM_EPSILON);
		bk1 = f1 + bk1;
		bk2 = 2. * (f2 + bk2) / ex;
		if (*ize == 2) {
		    d1 = expgnum(ex);
		    bk1 *= d1;
		    bk2 *= d1;
		}
		wminf = estf[0] * ex + estf[1];
	    }
	} else if (GNUM_EPSILON * ex > 1.) {
	    /* -------------------------------------------------
	       X > 1./EPS
	       ------------------------------------------------- */
	    *ncalc = *nb;
	    bk1 = 1. / (M_SQRT_2dPI * sqrtgnum(ex));
	    for (i = 0; i < *nb; ++i)
		bk[i] = bk1;
	    return;

	} else {
	    /* -------------------------------------------------------
	       X > 1.0
	       ------------------------------------------------------- */
	    twox = ex + ex;
	    blpha = 0.;
	    ratio = 0.;
	    if (ex <= 4.) {
		/* ----------------------------------------------------------
		   Calculation of K(ALPHA+1,X)/K(ALPHA,X),  1.0 <= X <= 4.0
		   ----------------------------------------------------------*/
		d2 = gnm_trunc(estm[0] / ex + estm[1]);
		m = (long) d2;
		d1 = d2 + d2;
		d2 -= .5;
		d2 *= d2;
		for (i = 2; i <= m; ++i) {
		    d1 -= 2.;
		    d2 -= d1;
		    ratio = (d3 + d2) / (twox + d1 - ratio);
		}
		/* -----------------------------------------------------------
		   Calculation of I(|ALPHA|,X) and I(|ALPHA|+1,X) by backward
		   recurrence and K(ALPHA,X) from the wronskian
		   -----------------------------------------------------------*/
		d2 = gnm_trunc(estm[2] * ex + estm[3]);
		m = (long) d2;
		c = gnumabs(nu);
		d3 = c + c;
		d1 = d3 - 1.;
		f1 = GNUM_MIN;
		f0 = (2. * (c + d2) / ex + .5 * ex / (c + d2 + 1.)) * GNUM_MIN;
		for (i = 3; i <= m; ++i) {
		    d2 -= 1.;
		    f2 = (d3 + d2 + d2) * f0;
		    blpha = (1. + d1 / d2) * (f2 + blpha);
		    f2 = f2 / ex + f1;
		    f1 = f0;
		    f0 = f2;
		}
		f1 = (d3 + 2.) * f0 / ex + f1;
		d1 = 0.;
		t1 = 1.;
		for (i = 1; i <= 7; ++i) {
		    d1 = c * d1 + p[i - 1];
		    t1 = c * t1 + q[i - 1];
		}
		p0 = expgnum(c * (a + c * (p[7] - c * d1 / t1) - loggnum(ex))) / ex;
		f2 = (c + .5 - ratio) * f1 / ex;
		bk1 = p0 + (d3 * f0 - f2 + f0 + blpha) / (f2 + f1 + f0) * p0;
		if (*ize == 1) {
		    bk1 *= expgnum(-ex);
		}
		wminf = estf[2] * ex + estf[3];
	    } else {
		/* ---------------------------------------------------------
		   Calculation of K(ALPHA,X) and K(ALPHA+1,X)/K(ALPHA,X), by
		   backward recurrence, for  X > 4.0
		   ----------------------------------------------------------*/
		dm = gnm_trunc(estm[4] / ex + estm[5]);
		m = (long) dm;
		d2 = dm - .5;
		d2 *= d2;
		d1 = dm + dm;
		for (i = 2; i <= m; ++i) {
		    dm -= 1.;
		    d1 -= 2.;
		    d2 -= d1;
		    ratio = (d3 + d2) / (twox + d1 - ratio);
		    blpha = (ratio + ratio * blpha) / dm;
		}
		bk1 = 1. / ((M_SQRT_2dPI + M_SQRT_2dPI * blpha) * sqrtgnum(ex));
		if (*ize == 1)
		    bk1 *= expgnum(-ex);
		wminf = estf[4] * (ex - gnumabs(ex - estf[6])) + estf[5];
	    }
	    /* ---------------------------------------------------------
	       Calculation of K(ALPHA+1,X)
	       from K(ALPHA,X) and  K(ALPHA+1,X)/K(ALPHA,X)
	       --------------------------------------------------------- */
	    bk2 = bk1 + bk1 * (nu + .5 - ratio) / ex;
	}
	/*--------------------------------------------------------------------
	  Calculation of 'NCALC', K(ALPHA+I,X),	I  =  0, 1, ... , NCALC-1,
	  &	  K(ALPHA+I,X)/K(ALPHA+I-1,X),	I = NCALC, NCALC+1, ... , NB-1
	  -------------------------------------------------------------------*/
	*ncalc = *nb;
	bk[0] = bk1;
	if (iend == 0)
	    return;

	j = 1 - k;
	if (j >= 0)
	    bk[j] = bk2;

	if (iend == 1)
	    return;

	m = imin2((long) (wminf - nu),iend);
	for (i = 2; i <= m; ++i) {
	    t1 = bk1;
	    bk1 = bk2;
	    twonu += 2.;
	    if (ex < 1.) {
		if (bk1 >= GNUM_MAX / twonu * ex)
		    break;
	    } else {
		if (bk1 / ex >= GNUM_MAX / twonu)
		    break;
	    }
	    bk2 = twonu / ex * bk1 + t1;
	    ii = i;
	    ++j;
	    if (j >= 0) {
		bk[j] = bk2;
	    }
	}

	m = ii;
	if (m == iend) {
	    return;
	}
	ratio = bk2 / bk1;
	mplus1 = m + 1;
	*ncalc = -1;
	for (i = mplus1; i <= iend; ++i) {
	    twonu += 2.;
	    ratio = twonu / ex + 1./ratio;
	    ++j;
	    if (j >= 1) {
		bk[j] = ratio;
	    } else {
		if (bk2 >= GNUM_MAX / ratio)
		    return;

		bk2 *= ratio;
	    }
	}
	*ncalc = imax2(1, mplus1 - k);
	if (*ncalc == 1)
	    bk[0] = bk2;
	if (*nb == 1)
	    return;

L420:
	for (i = *ncalc; i < *nb; ++i) { /* i == *ncalc */
#ifndef IEEE_754
	    if (bk[i-1] >= GNUM_MAX / bk[i])
		return;
#endif
	    bk[i] *= bk[i-1];
	    (*ncalc)++;
	}
    }
}

/* ------------------------------------------------------------------------ */
/* --- END MAGIC R SOURCE MARKER --- */


/* --- BEGIN IANDJMSMITH SOURCE MARKER --- */

/* Continued fraction for calculation of
 *    1/i + x/(i+d) + x*x/(i+2*d) + x*x*x/(i+3*d) + ...
 */
static gnm_float
logcf (gnm_float x, gnm_float i, gnm_float d)
{
	gnm_float c1 = 2 * d;
	gnm_float c2 = i + d;
	gnm_float c4 = c2 + d;
	gnm_float a1 = c2;
	gnm_float b1 = i * (c2 - i * x);
	gnm_float b2 = d * d * x;
	gnm_float a2 = c4 * c2 - b2;
	const gnm_float cfVSmall = 1.0e-14;

#if 0
	assert (i > 0);
	assert (d >= 0);
#endif

	b2 = c4 * b1 - i * b2;

	while (gnumabs (a2 * b1 - a1 * b2) > gnumabs (cfVSmall * b1 * b2)) {
		gnm_float c3 = c2*c2*x;
		c2 += d;
		c4 += d;
		a1 = c4 * a2 - c3 * a1;
		b1 = c4 * b2 - c3 * b1;

		c3 = c1 * c1 * x;
		c1 += d;
		c4 += d;
		a2 = c4 * a1 - c3 * a2;
		b2 = c4 * b1 - c3 * b2;

		if (gnumabs (b2) > scalefactor) {
			a1 *= 1 / scalefactor;
			b1 *= 1 / scalefactor;
			a2 *= 1 / scalefactor;
			b2 *= 1 / scalefactor;
		} else if (gnumabs (b2) < 1 / scalefactor) {
			a1 *= scalefactor;
			b1 *= scalefactor;
			a2 *= scalefactor;
			b2 *= scalefactor;
		}
	}

	return a2 / b2;
}


/* Accurate calculation of log(1+x)-x, particularly for small x.  */
gnm_float
log1pmx (gnm_float x)
{
	static const gnm_float minLog1Value = -0.79149064;
	static const gnm_float two = 2;

	if (gnumabs (x) < 1.0e-2) {
		gnm_float term = x / (2 + x);
		gnm_float y = term * term;
		return term * ((((two / 9 * y + two / 7) * y + two / 5) * y + two / 3) * y - x);
	} else if (x < minLog1Value || x > 1) {
		return log1pgnum (x) - x;
	} else {
		gnm_float term = x / (2 + x);
		gnm_float y = term * term;
		return term * (2 * y * logcf (y, 3, 2) - x);
	}
}

/* Accurate  loggnum (1 - expgnum (p))  for  p <= 0.  */
gnm_float
swap_log_tail (gnm_float lp)
{
	if (lp > -1 / loggnum (2))
		return loggnum (-expm1gnum (lp));  /* Good formula for lp near zero.  */
	else
		return log1pgnum (-expgnum (lp));  /* Good formula for small lp.  */
}


/* Calculation of logfbit(x)-logfbit(1+x). y2 must be < 1.  */
static gnm_float
logfbitdif (gnm_float x)
{
	gnm_float y = 1 / (2 * x + 3);
	gnm_float y2 = y * y;
	return y2 * logcf (y2, 3, 2);
}

/*
 * lfbc{1-7} from this Mathematica program:
 *
 * Table[Numerator[BernoulliB[2n]/(2n(2n - 1))], {n, 1, 22}]
 * Table[Denominator[BernoulliB[2n]/(2n(2n - 1))], {n, 1, 22}]
 */
static const gnm_float lfbc1 = GNM_const (1.0) / 12;
static const gnm_float lfbc2 = GNM_const (1.0) / 30;
static const gnm_float lfbc3 = GNM_const (1.0) / 105;
static const gnm_float lfbc4 = GNM_const (1.0) / 140;
static const gnm_float lfbc5 = GNM_const (1.0) / 99;
static const gnm_float lfbc6 = GNM_const (691.0) / 30030;
static const gnm_float lfbc7 = GNM_const (1.0) / 13;
/* lfbc{8,9} to make logfbit(6) and logfbit(7) exact.  */
static const gnm_float lfbc8 = GNM_const (3.5068606896459316479e-01);
static const gnm_float lfbc9 = GNM_const (1.6769998201671114808);

/* This is also stirlerr(x+1).  */
gnm_float
logfbit (gnm_float x)
{
	/*
	 * Error part of Stirling's formula where
	 *   log(x!) = log(sqrt(twopi))+(x+0.5)*log(x+1)-(x+1)+logfbit(x).
	 *
	 * Are we ever concerned about the relative error involved in this
	 * function? I don't think so.
	 */
	if (x >= 1e10) return 1 / (12 * (x + 1));
	else if (x >= 6) {
		gnm_float x1 = x + 1;
		gnm_float x2 = 1 / (x1 * x1);
		gnm_float x3 =
			x2 * (lfbc2 - x2 *
			      (lfbc3 - x2 *
			       (lfbc4 - x2 *
				(lfbc5 - x2 *
				 (lfbc6 - x2 *
				  (lfbc7 - x2 *
				   (lfbc8 - x2 *
				    lfbc9)))))));
		return lfbc1 * (1 - x3) / x1;
	}
	else if (x == 5) return GNM_const (0.13876128823070747998745727023762908562e-1);
	else if (x == 4) return GNM_const (0.16644691189821192163194865373593391145e-1);
	else if (x == 3) return GNM_const (0.20790672103765093111522771767848656333e-1);
	else if (x == 2) return GNM_const (0.27677925684998339148789292746244666596e-1);
	else if (x == 1) return GNM_const (0.41340695955409294093822081407117508025e-1);
	else if (x == 0) return GNM_const (0.81061466795327258219670263594382360138e-1);
	else if (x > -1) {
		gnm_float x1 = x;
		gnm_float x2 = 0;
		while (x1 < 6) {
			x2 += logfbitdif (x1);
			x1++;
		}
		return x2 + logfbit (x1);
	}
	else return gnm_pinf;
}

/* Calculation of logfbit1(x)-logfbit1(1+x).  */
static gnm_float
logfbit1dif (gnm_float x)
{
	return (logfbitdif (x) - 1 / (4 * (x + 1) * (x + 2))) / (x + 1.5);
}

/* Derivative logfbit.  */
static gnm_float
logfbit1 (gnm_float x)
{
	if (x >= 1e10) return -lfbc1 * powgnum (x + 1, -2);
	else if (x >= 6) {
		gnm_float x1 = x + 1;
		gnm_float x2 = 1 / (x1 * x1);
		gnm_float x3 =
			x2 * (3 * lfbc2 - x2*
			      (5 * lfbc3 - x2 *
			       (7 * lfbc4 - x2 *
				(9 * lfbc5 - x2 *
				 (11 * lfbc6 - x2 *
				  (13 * lfbc7 - x2 *
				   (15 * lfbc8 - x2 *
				    17 * lfbc9)))))));
		return -lfbc1 * (1 - x3) * x2;
	}
	else if (x > -1) {
		gnm_float x1 = x;
		gnm_float x2 = 0;
		while (x1 < 6) {
			x2 += logfbit1dif (x1);
			x1++;
		}
		return x2 + logfbit1 (x1);
	}
	else return gnm_ninf;
}


/* Calculation of logfbit3(x)-logfbit3(1+x). */
static gnm_float
logfbit3dif (gnm_float x)
{
	return -(2 * x + 3) * powgnum ((x + 1) * (x + 2), -3);
}


/* Third derivative logfbit.  */
static gnm_float
logfbit3 (gnm_float x)
{
	if (x >= 1e10) return -0.5 * powgnum (x + 1, -4);
	else if (x >= 6) {
		gnm_float x1 = x + 1;
		gnm_float x2 = 1 / (x1 * x1);
		gnm_float x3 =
			x2 * (60 * lfbc2 - x2 *
			      (210 * lfbc3 - x2 *
			       (504 * lfbc4 - x2 *
				(990 * lfbc5 - x2 *
				 (1716 * lfbc6 - x2 *
				  (2730 * lfbc7 - x2 *
				   (4080 * lfbc8 - x2 *
				    5814 * lfbc9)))))));
		return -lfbc1 * (6 - x3) * x2 * x2;
	}
	else if (x > -1) {
		gnm_float x1 = x;
		gnm_float x2 = 0;
		while (x1 < 6) {
			x2 += logfbit3dif (x1);
			x1++;
		}
		return x2 + logfbit3 (x1);
	}
	else return gnm_ninf;
}

/* Calculation of logfbit5(x)-logfbit5(1+x).  */
static gnm_float
logfbit5dif (gnm_float x)
{
	return -6 * (2 * x + 3) * ((5 * x + 15) * x + 12) *
		powgnum ((x + 1) * (x + 2), -5);
}

/* Fifth derivative logfbit.  */
static gnm_float
logfbit5 (gnm_float x)
{
	if (x >= 1e10) return -10 * powgnum (x + 1, -6);
	else if (x >= 6) {
		gnm_float x1 = x + 1;
		gnm_float x2 = 1 / (x1 * x1);
		gnm_float x3 =
			x2 * (2520 * lfbc2 - x2 *
			      (15120 * lfbc3 - x2 *
			       (55440 * lfbc4 - x2 *
				(154440 * lfbc5 - x2 *
				 (360360 * lfbc6 - x2 *
				  (742560 * lfbc7 - x2 *
				   (1395360 * lfbc8 - x2 *
				    2441880 * lfbc9)))))));
		return -lfbc1 * (120 - x3) * x2 * x2 * x2;
	} else if (x > -1) {
		gnm_float x1 = x;
		gnm_float x2 = 0;
		while (x1 < 6) {
			x2 += logfbit5dif (x1);
			x1++;
		}
		return x2 + logfbit5 (x1);
	}
	else return gnm_ninf;
}

/* Calculation of logfbit7(x)-logfbit7(1+x).  */
static gnm_float
logfbit7dif (gnm_float x)
{
	return -120 *
		(2 * x + 3) *
		((((14 * x + 84) * x + 196) * x + 210) * x + 87) *
		powgnum ((x + 1) * (x + 2), -7);
}

/* Seventh derivative logfbit.  */
static gnm_float
logfbit7 (gnm_float x)
{
	if (x >= 1e10) return -420 * powgnum (x + 1, -8);
	else if (x >= 6) {
		gnm_float x1 = x + 1;
		gnm_float x2 = 1 / (x1 * x1);
		gnm_float x3 =
			x2 * (181440 * lfbc2 - x2 *
			      (1663200 * lfbc3 - x2 *
			       (8648640 * lfbc4 - x2 *
				(32432400 * lfbc5 - x2 *
				 (98017920 * lfbc6 - x2 *
				  (253955520 * lfbc7 - x2 *
				   (586051200 * lfbc8 - x2 *
				    1235591280 * lfbc9)))))));
		return -lfbc1 * (5040 - x3) * x2 * x2 * x2 * x2;
	} else if (x > -1) {
		gnm_float x1 = x;
		gnm_float x2 = 0;
		while (x1 < 6) {
			x2 += logfbit7dif (x1);
			x1++;
		}
		return x2 + logfbit7 (x1);
	}
	else return gnm_ninf;
}


static gnm_float
lfbaccdif (gnm_float a, gnm_float b)
{
	if (a > 0.03 * (a + b))
		return logfbit (a + b) - logfbit (b);
	else {
		gnm_float a2 = a * a;
		gnm_float ab2 = a / 2 + b;
		return a * (logfbit1 (ab2) + a2 / 24 *
			    (logfbit3 (ab2) + a2 / 80 *
			     (logfbit5 (ab2) + a2 / 168 *
			      logfbit7 (ab2))));
	}
}

/* Calculates log(gamma(a+1) accurately for for small a (0 < a & a < 0.5). */
gnm_float
lgamma1p (gnm_float a)
{
	static const gnm_float eulers_const = GNM_const (0.57721566490153286060651209008240243104215933593992);

	/* coeffs[i] holds (zeta(i+2)-1)/(i+2)  */
	static const gnm_float coeffs[40] = {
		GNM_const (0.3224670334241132182362075833230126e-0),
		GNM_const (0.6735230105319809513324605383715000e-1),
		GNM_const (0.2058080842778454787900092413529198e-1),
		GNM_const (0.7385551028673985266273097291406834e-2),
		GNM_const (0.2890510330741523285752988298486755e-2),
		GNM_const (0.1192753911703260977113935692828109e-2),
		GNM_const (0.5096695247430424223356548135815582e-3),
		GNM_const (0.2231547584535793797614188036013401e-3),
		GNM_const (0.9945751278180853371459589003190170e-4),
		GNM_const (0.4492623673813314170020750240635786e-4),
		GNM_const (0.2050721277567069155316650397830591e-4),
		GNM_const (0.9439488275268395903987425104415055e-5),
		GNM_const (0.4374866789907487804181793223952411e-5),
		GNM_const (0.2039215753801366236781900709670839e-5),
		GNM_const (0.9551412130407419832857179772951265e-6),
		GNM_const (0.4492469198764566043294290331193655e-6),
		GNM_const (0.2120718480555466586923135901077628e-6),
		GNM_const (0.1004322482396809960872083050053344e-6),
		GNM_const (0.4769810169363980565760193417246730e-7),
		GNM_const (0.2271109460894316491031998116062124e-7),
		GNM_const (0.1083865921489695409107491757968159e-7),
		GNM_const (0.5183475041970046655121248647057669e-8),
		GNM_const (0.2483674543802478317185008663991718e-8),
		GNM_const (0.1192140140586091207442548202774640e-8),
		GNM_const (0.5731367241678862013330194857961011e-9),
		GNM_const (0.2759522885124233145178149692816341e-9),
		GNM_const (0.1330476437424448948149715720858008e-9),
		GNM_const (0.6422964563838100022082448087644648e-10),
		GNM_const (0.3104424774732227276239215783404066e-10),
		GNM_const (0.1502138408075414217093301048780668e-10),
		GNM_const (0.7275974480239079662504549924814047e-11),
		GNM_const (0.3527742476575915083615072228655483e-11),
		GNM_const (0.1711991790559617908601084114443031e-11),
		GNM_const (0.8315385841420284819798357793954418e-12),
		GNM_const (0.4042200525289440065536008957032895e-12),
		GNM_const (0.1966475631096616490411045679010286e-12),
		GNM_const (0.9573630387838555763782200936508615e-13),
		GNM_const (0.4664076026428374224576492565974577e-13),
		GNM_const (0.2273736960065972320633279596737272e-13),
		GNM_const (0.1109139947083452201658320007192334e-13)
	};
	const int N = sizeof (coeffs) / sizeof (coeffs[0]);

	const gnm_float c = GNM_const (0.2273736845824652515226821577978691e-12);  /* zeta(N+2)-1 */
	gnm_float lgam;
	int i;

	if (gnumabs (a) >= 0.5)
		return lgammagnum (a + 1);

	/* Abramowitz & Stegun 6.1.33 */
	/* http://functions.wolfram.com/06.11.06.0008.01 */
	lgam = c * logcf (-a / 2, N + 2, 1);
	for (i = N - 1; i >= 0; i--)
		lgam = coeffs[i] - a * lgam;

	return (a * lgam - eulers_const) * a - log1pmx (a);
}

static gnm_float
compbfunc (gnm_float x, gnm_float a, gnm_float b)
{
	const gnm_float sumAcc = 5E-16;
	gnm_float term = x;
	gnm_float d = 2;
	gnm_float sum = term / (a + 1);
	while (gnumabs (term) > gnumabs (sum * sumAcc)) {
		term *= (d - b) * x / d;
		sum += term / (a + d);
		d++;
	}
	return a * (b - 1) * sum;
}

static gnm_float
pbeta_smalla (gnm_float x, gnm_float a, gnm_float b, gboolean lower_tail, gboolean log_p)
{
	gnm_float r;

#if 0
	assert (a >= 0 && b >= 0);
	assert (a < 1);
	assert (b < 1 || (1 + b) * x <= 1);
#endif

	if (x > 0.5) {
		gnm_float olda = a;
		a = b;
		b = olda;
		x = 1 - x;
		lower_tail = !lower_tail;
	}

	r = (a + b + 0.5) * log1pmx (a / (1 + b)) +
		a * (a - 0.5) / (1 + b) +
		lfbaccdif (a, b);
	r += a * loggnum ((1 + b) * x) - lgamma1p (a);
	if (lower_tail) {
		if (log_p)
			return r + log1pgnum (-compbfunc (x, a, b)) + loggnum (b / (a + b));
		else
			return expgnum (r) * (1 - compbfunc (x, a, b)) * (b / (a + b));
	} else {
		/* x=0.500000001 a=0.5  b=0.000001 ends up here [swapped]
		 * with r=-7.94418987455065e-08 and cbf=-3.16694087508444e-07.
		 *
		 * x=0.0000001 a=0.999999 b=0.02 end up here with
		 * r=-16.098276918385 and cbf=-4.89999787339858e-08.
		 */
		if (log_p) {
			return swap_log_tail (r + log1pgnum (-compbfunc (x, a, b)) + loggnum (b / (a + b)));
		} else {
			r = -expm1gnum (r);
			r += compbfunc (x, a, b) * (1 - r);
			r += (a / (a + b)) * (1 - r);
			return r;
		}
	}
}

/* Cumulative Students t-distribution, with odd parameterisation for
 * use with binApprox.
 * p is x*x/(k+x*x)
 * q is 1-p
 * logqk2 is LN(q)*k/2
 * approxtdistDens returns with approx density function, for use in
 * binApprox
 */
static gnm_float
tdistexp (gnm_float p, gnm_float q, gnm_float logqk2, gnm_float k,
	  gnm_float *approxtdistDens)
{
	const gnm_float sumAcc = 5E-16;
	const gnm_float cfVSmall = 1.0e-14;
	const gnm_float lstpi = loggnum (2 * M_PIgnum) / 2;

	if (floorgnum (k / 2) * 2 == k)
		*approxtdistDens = expgnum (logqk2 + logfbit (k - 1) - 2 * logfbit (k * 0.5 - 1) - lstpi);
	else
		*approxtdistDens = expgnum (logqk2 + k * log1pmx (1 / k) + 2 * logfbit ((k - 1) * 0.5) - logfbit (k - 1) - lstpi);

	if (k * p < 4 * q) {
		gnm_float sum = 1;
		gnm_float aki = k + 1;
		gnm_float ai = 3;
		gnm_float term = aki * p / ai;

		while (term > sumAcc * sum) {
			sum += term;
			ai += 2;
			aki += 2;
			term *= aki * p / ai;
		}
		sum += term;

		return 0.5 - *approxtdistDens * sum * sqrtgnum (k * p);
	} else {
		gnm_float q1 = 2 * (1 + q);
		gnm_float q8 = 8 * q;
		gnm_float a1 = 0;
		gnm_float b1 = 1;
		gnm_float c1 = -6 * q;
		gnm_float a2 = 1;
		gnm_float b2 = (k - 1) * p + 3;
		gnm_float cadd = -14 * q;
		gnm_float c2 = b2 + q1;

		while (gnumabs (a2 * b1 - a1 * b2) > gnumabs (cfVSmall * b1 * b2)) {
			a1 = c2 * a2 + c1 * a1;
			b1 = c2 * b2 + c1 * b1;
			c1 += cadd;
			cadd -= q8;
			c2 += q1;
			a2 = c2 * a1 + c1 * a2;
			b2 = c2 * b1 + c1 * b2;
			c1 += cadd;
			cadd -= q8;
			c2 += q1;

			if (gnumabs (b2) > scalefactor) {
				a1 *= 1 / scalefactor;
				b1 *= 1 / scalefactor;
				a2 *= 1 / scalefactor;
				b2 *= 1 / scalefactor;
			} else if (gnumabs (b2) < 1 / scalefactor) {
				a1 *= scalefactor;
				b1 *= scalefactor;
				a2 *= scalefactor;
				b2 *= scalefactor;
			}
		}

		return *approxtdistDens * (1 - q * a2 / b2) / sqrtgnum (k * p);
	}
}


/* Asymptotic expansion to calculate the probability that binomial variate
 * has value <= a.
 * (diffFromMean = (a+b)*p-a).
 */
static gnm_float
binApprox (gnm_float a, gnm_float b, gnm_float diffFromMean,
           gboolean lower_tail, gboolean log_p)
{
	gnm_float pq1, res, comt, comf, t;
	gnm_float ib05, ib15, ib25, ib35, ib3;
	gnm_float elfb, coef15, coef25, coef35;
	gnm_float approxtdistDens;

	gnm_float n = a + b;
	gnm_float n1 = n + 1;
	gnm_float lvv = b - n * diffFromMean;
	gnm_float lval = (a * log1pmx (lvv / (a * n1)) +
			  b * log1pmx (-lvv / (b * n1))) / n;
	gnm_float tp = -expm1gnum (lval);
	gnm_float mfac = n1 * tp;
	gnm_float ib2 = 1 + mfac;
	gnm_float t1 = (n + 2) * tp;

	mfac = 2 * mfac;

	ib3 = ib2 + mfac*t1;
	ib05 = tdistexp (tp, 1 - tp, n1 * lval, 2 * n1, &approxtdistDens);
	ib15 = sqrtgnum (mfac);
	mfac = t1 * (GNM_const (2.0) / 3);
	ib25 = 1 + mfac;
	ib35 = ib25 + mfac * (n + 3) * tp * (GNM_const (2.0) / 5);

	pq1 = (n * n) / (a * b);

	res = (ib2 * (1 + 2 * pq1) / 135 - 2 * ib3 * ((2 * pq1 - 43) * pq1 - 22) / (2835 * (n + 3))) / (n + 2);
	res = (GNM_const (1.0) / 3 - res) * 2 * sqrtgnum (pq1 / n1) * (a - b) / n;

	n1 = (n + 1.5) * (n + 2.5);
	coef15 = (-17 + 2 * pq1) / (24 * (n + 1.5));
	coef25 = (-503 + 4 * pq1 * (19 + pq1)) / (1152 * n1);
	coef35 = (-315733 + pq1 * (53310 + pq1 * (8196 - 1112 * pq1))) /
		(414720 * n1 * (n + 3.5));
	elfb = ((coef35 + coef25) + coef15) + 1;

	comt = ib15 * ((coef35 * ib35 + coef25 * ib25) + coef15);
	comf = approxtdistDens / elfb;

	if (lvv > 0)
		t = ib05 - (res - comt) * comf;
	else
		t = ib05 + (res + comt) * comf;

	return (!lower_tail != (lvv > 0)) ? R_D_Clog (t) : R_D_val (t);
}

/* Probability that binomial variate with sample size i+j and
 * event prob p (=1-q) has value i (diffFromMean = (i+j)*p-i)
 */
static gnm_float
binomialTerm (gnm_float i, gnm_float j, gnm_float p, gnm_float q,
	      gnm_float diffFromMean, gboolean log_p)
{
	const gnm_float minLog1Value = -0.79149064;
	gnm_float c1,c2,c3;
	gnm_float c4,c5,c6,ps,logbinomialTerm,dfm;
	gnm_float t;

	if (i == 0 && j <= 0)
		return R_D__1;

	if (i <= -1 || j < 0)
		return R_D__0;

	c1 = (i + 1) + j;
	if (p < q) {
		c2 = i;
		c3 = j;
		ps = p;
		dfm = diffFromMean;
	} else {
		c3 = i;
		c2 = j;
		ps = q;
		dfm = -diffFromMean;
	}

	c5 = (dfm - (1 - ps)) / (c2 + 1);
	c6 = -(dfm + ps) / (c3 + 1);

	if (c5 < minLog1Value) {
		if (c2 == 0) {
			logbinomialTerm = c3 * log1pgnum (-ps);
			return log_p ? logbinomialTerm : expgnum (logbinomialTerm);
		} else if (ps == 0 && c2 > 0) {
			return R_D__0;
		} else {
			t = loggnum ((ps * c1) / (c2 + 1)) - c5;
		}
	} else {
		t = log1pmx (c5);
	}

	c4 = logfbit (i + j) - logfbit (i) - logfbit (j);
	logbinomialTerm = c4 + c2 * t - c5 + (c3 * log1pmx (c6) - c6);

	return log_p
		? logbinomialTerm + 0.5 * loggnum (c1 / ((c2 + 1) * (c3 + 1) * 2 * M_PIgnum))
		: expgnum (logbinomialTerm) * sqrtgnum (c1 / ((c2 + 1) * (c3 + 1) * 2 * M_PIgnum));
}


/*
 * Probability that binomial variate with sample size ii+jj
 * and event prob pp (=1-qq) has value <=i.
 * (diffFromMean = (ii+jj)*pp-ii).
 */
static gnm_float
binomialcf (gnm_float ii, gnm_float jj, gnm_float pp, gnm_float qq,
	    gnm_float diffFromMean, gboolean lower_tail, gboolean log_p)
{
	const gnm_float sumAlways = 0;
	const gnm_float sumFactor = 6;
	const gnm_float cfSmall = 1.0e-12;

	gnm_float prob,p,q,a1,a2,b1,b2,c1,c2,c3,c4,n1,q1,dfm;
	gnm_float i,j,ni,nj,numb,ip1;
	gboolean swapped;

	ip1 = ii + 1;
	if (ii > -1 && (jj <= 0 || pp == 0)) {
		return R_DT_1;
	} else if (ii > -1 && ii < 0) {
		ii = -ii;
		ip1 = ii;
		prob = binomialTerm (ii, jj, pp, qq, (ii + jj) * pp - ii, log_p) *
			ii / ((ii + jj) * pp);
		ii--;
		diffFromMean = (ii + jj) * pp - ii;
	} else
		prob = binomialTerm (ii, jj, pp, qq, diffFromMean, log_p);

	n1 = (ii + 3) + jj;
	if (ii < 0)
		swapped = FALSE;
	else if (pp > qq)
		swapped = (n1 * qq >= jj + 1);
	else
		swapped = (n1 * pp <= ii + 2);

	if (prob == R_D__0) {
		if (swapped == !lower_tail)
			return R_D__0;
		else
			return R_D__1;
	}

	if (swapped) {
		j = ip1;
		ip1 = jj;
		i = jj - 1;
		p = qq;
		q = pp;
		dfm = 1 - diffFromMean;
	} else {
		i = ii;
		j = jj;
		p = pp;
		q = qq;
		dfm = diffFromMean;
	}

	if (i > sumAlways) {
		numb = floorgnum (sumFactor * sqrtgnum (p + 0.5) * expgnum (loggnum (n1 * p * q) / 3));
		numb = floorgnum (numb - dfm);
		if (numb > i) numb = floorgnum (i);
	} else
		numb = floorgnum (i);
	if (numb < 0) numb = 0;

	a1 = 0;
	b1 = 1;
	q1 = q + 1;
	a2 = (i - numb) * q;
	b2 = dfm + numb + 1;
	c1 = 0;

	c2 = a2;
	c4 = b2;

	while (gnumabs (a2 * b1 - a1 * b2) > gnumabs (cfSmall * b1 * b2)) {
		c1++;
		c2 -= q;
		c3 = c1 * c2;
		c4 += q1;
		a1 = c4 * a2 + c3 * a1;
		b1 = c4 * b2 + c3 * b1;
		c1++;
		c2 -= q;
		c3 = c1 * c2;
		c4 += q1;
		a2 = c4 * a1 + c3 * a2;
		b2 = c4 * b1 + c3 * b2;

		if (gnumabs (b2) > scalefactor) {
			a1 *= 1 / scalefactor;
			b1 *= 1 / scalefactor;
			a2 *= 1 / scalefactor;
			b2 *= 1 / scalefactor;
		} else if (gnumabs (b2) < 1 / scalefactor) {
			a1 *= scalefactor;
			b1 *= scalefactor;
			a2 *= scalefactor;
			b2 *= scalefactor;
		}
	}

	a1 = a2 / b2;

	ni = (i - numb + 1) * q;
	nj = (j + numb) * p;
	while (numb > 0) {
		a1 = (1 + a1) * (ni / nj);
		ni = ni + q;
		nj = nj - p;
		numb--;
	}

	prob = log_p ? prob + log1pgnum (a1) : prob * (1 + a1);

	if (swapped) {
		if (log_p)
			prob += loggnum (ip1 * q / nj);
		else
			prob *= ip1 * q / nj;
	}

	if (swapped == !lower_tail)
		return prob;
	else
		return log_p ? swap_log_tail (prob) : 1 - prob;
}

static gnm_float
binomial (gnm_float ii, gnm_float jj, gnm_float pp, gnm_float qq,
          gnm_float diffFromMean, gboolean lower_tail, gboolean log_p)
{
	gnm_float mij = fmin2 (ii, jj);

	if (mij > 500 && gnumabs (diffFromMean) < 0.01 * mij)
		return binApprox (jj - 1, ii, diffFromMean, lower_tail, log_p);

	return binomialcf (ii, jj, pp, qq, diffFromMean, lower_tail, log_p);
}


gnm_float
pbeta (gnm_float x, gnm_float a, gnm_float b, gboolean lower_tail, gboolean log_p)
{
	if (isnangnum (x) || isnangnum (a) || isnangnum (b))
		return x + a + b;

	if (x <= 0) return R_DT_0;
	if (x >= 1) return R_DT_1;

	if (a < 1 && (b < 1 || (1 + b) * x <= 1))
		return pbeta_smalla (x, a, b, lower_tail, log_p);

	if (b < 1 && (1 + a) * (1 - x) <= 1)  /* a/(1+a) <= x ??? */
		return pbeta_smalla (1 - x, b, a, !lower_tail, log_p);

	if (a < 1)
		return binomial (-a, b, x, 1 - x, 0, !lower_tail, log_p);

	if (b < 1)
		return binomial (-b, a, 1 - x, x, 0, lower_tail, log_p);

	return binomial (a - 1, b, x, 1 - x, (a + b - 1) * x - a + 1,
			 !lower_tail, log_p);
}

/* --- END IANDJMSMITH SOURCE MARKER --- */
/* ------------------------------------------------------------------------ */

/*
 * New phyper implementation.  Copyright 2004 Morten Welinder.
 * Distributed under the GNU General Public License.
 *
 * Thanks to Ian Smith for ideas.
 */
/*
 * Calculate
 *
 *          phyper (i, NR, NB, n, TRUE, FALSE)
 *   [log]  ----------------------------------
 *             dhyper (i, NR, NB, n, FALSE)
 *
 * without actually calling phyper.  This assumes that
 *
 *     i * (NR + NB) <= n * NR
 *
 */
static gnm_float
pdhyper (gnm_float i, gnm_float NR, gnm_float NB, gnm_float n, gboolean log_p)
{
  gnm_float sum = 0;
  gnm_float term = 1;

  while (i > 0 && term >= GNUM_EPSILON * sum) {
	  term *= i * (NB - n + i) / (n + 1 - i) / (NR + 1 - i);
	  sum += term;
	  i--;
  }

  return log_p ? log1pgnum (sum) : 1 + sum;
}


gnm_float
phyper (gnm_float i, gnm_float NR, gnm_float NB, gnm_float n, int lower_tail, int log_p)
{
	gnm_float d, pd;

#ifdef IEEE_754
	if (isnangnum (i) || isnangnum (NR) || isnangnum (NB) || isnangnum (n))
		return i + NR + NB + n;
#endif

	i = floorgnum (i + 1e-7);
	NR = floorgnum (NR + 0.5);
	NB = floorgnum (NB + 0.5);
	n = floorgnum (n + 0.5);

	if (NR < 0 || NB < 0 || !finitegnum (NR + NB) || n < 0 || n > NR + NB)
		ML_ERR_return_NAN;

	if (i * (NR + NB) > n * NR) {
		/* Swap tails.  */
		gnm_float oldNB = NB;
		NB = NR;
		NR = oldNB;
		i = n - i - 1;
		lower_tail = !lower_tail;
	}

	if (i < 0)
		return R_DT_0;

	d = dhyper (i, NR, NB, n, log_p);
	pd = pdhyper (i, NR, NB, n, log_p);

	return log_p ? R_DT_log (d + pd) : R_D_Lval (d * pd);
}


gnm_float
pcauchy (gnm_float x, gnm_float location, gnm_float scale,
	 gboolean lower_tail, gboolean log_p)
{
#ifdef IEEE_754
    if (isnangnum(x) || isnangnum(location) || isnangnum(scale))
	    return x + location + scale;
#endif
    if (scale <= 0) ML_ERR_return_NAN;

    x = (x - location) / scale;
    if (isnangnum(x)) ML_ERR_return_NAN;
#ifdef IEEE_754
    if (!finitegnum(x)) {
	    if(x < 0) return R_DT_0;
	    else return R_DT_1;
    }
#endif

    if (!lower_tail)
	    x = -x;

    if (gnumabs (x) > 1) {
	    gnm_float temp = atangnum (1 / x) / M_PIgnum;
	    return (x > 0) ? R_D_Clog (temp) : R_D_val (-temp);
    } else
	    return R_D_val (0.5 + atangnum (x) / M_PIgnum);
}

gnm_float
pf (gnm_float x, gnm_float n1, gnm_float n2, gboolean lower_tail, gboolean log_p)
{
#ifdef IEEE_754
    if (isnangnum (x) || isnangnum (n1) || isnangnum (n2))
	return x + n2 + n1;
#endif
    if (n1 <= 0 || n2 <= 0) ML_ERR_return_NAN;

    if (x <= 0)
	return R_DT_0;

    /* Avoid squeezing pbeta's first parameter against 1.  */
    if (n1 * x > n2)
	    return pbeta (n2 / (n2 + n1 * x), n2 / 2, n1 / 2,
			  !lower_tail, log_p);
    else
	    return pbeta (n1 * x / (n2 + n1 * x), n1 / 2, n2 / 2,
			  lower_tail, log_p);
}

/* ------------------------------------------------------------------------ */

typedef gnm_float (*PFunc) (gnm_float x, const gnm_float shape[],
			    gboolean lower_tail, gboolean log_p);
typedef gnm_float (*DPFunc) (gnm_float x, const gnm_float shape[],
			     gboolean log_p);

static gnm_float
pfuncinverter (gnm_float p, const gnm_float shape[],
	       gboolean lower_tail, gboolean log_p,
	       gnm_float xlow, gnm_float xhigh, gnm_float x0,
	       PFunc pfunc, DPFunc dpfunc_dx)
{
	gboolean have_xlow = finitegnum (xlow);
	gboolean have_xhigh = finitegnum (xhigh);
	gnm_float exlow, exhigh;
	gnm_float x = 0, e = 0;
	int i;

	if (p == R_DT_0) return xlow;
	if (p == R_DT_1) return xhigh;

	exlow = R_DT_0 - p;
	exhigh = R_DT_1 - p;
	if (!lower_tail) {
		exlow = -exlow;
		exhigh = -exhigh;
	}

#ifdef DEBUG_pfuncinverter
	printf ("p=%.15g\n", p);
#endif

	for (i = 0; i < 100; i++) {
		if (i == 0) {
			/* Use supplied guess.  */
			x = x0;
			if (x0 <= xlow || x0 >= xhigh) {
				if (have_xlow || have_xhigh)
					x0 = ((have_xhigh ? xhigh : xlow + 1) +
					      (have_xlow ? xlow : xhigh - 1)) / 2;
				else
					x0 = 0;
			}
		} else if (i == 1) {
			/*
			 * Under the assumption that the initial guess was
			 * good, pick a nearby point that is hopefully on
			 * the other side.  If we already have both sides,
			 * just bisect.
			 */
			if (have_xlow && have_xhigh)
				x = (xlow + xhigh) / 2;
			else if (have_xlow)
				x = xlow * 1.1;
			else
				x = xhigh / 1.1;
		} else if (have_xlow && have_xhigh) {
			switch (i % 8) {
			case 0:
			case 4:
				x = xhigh - (xhigh - xlow) *
					(exhigh / (exhigh - exlow));
				break;
			case 2:
				x = (xhigh + 1000 * xlow) / 1001;
				break;
			case 6:
				x = (1000 * xhigh + xlow) / 1001;
				break;
			default:
				x = (xhigh + xlow) / 2;
			}
		} else if (have_xlow) {
			/* Agressively seek right in search of xhigh.  */
			x = (xlow < 1) ? 1 : (2 * i) * xlow;
		} else {
			/* Agressively seek left in search of xlow.  */
			x = (xhigh > -1) ? -1 : (2 * i) * xhigh;
		}

	newton_retry:
		if ((have_xlow && x <= xlow) || (have_xhigh && x >= xhigh))
			continue;

		e = pfunc (x, shape, lower_tail, log_p) - p;
		if (!lower_tail) e = -e;

#ifdef DEBUG_pfuncinverter
		printf ("  x=%.15g  e=%.15g  l=%.15g  h=%.15g\n",
			x, e, xlow, xhigh);
#endif

		if (e == 0)
			goto done;
		else if (e > 0) {
			xhigh = x;
			exhigh = e;
			have_xhigh = TRUE;
		} else {
			xlow = x;
			exlow = e;
			have_xlow = TRUE;
		}

		if (have_xlow && have_xhigh) {
			gnm_float prec = (xhigh - xlow) /
				(gnumabs (xlow) + gnumabs (xhigh));
			if (prec < GNUM_EPSILON * 4) {
				x = (xhigh + xlow) / 2;
				e = pfunc (x, shape, lower_tail, log_p) - p;
				if (!lower_tail) e = -e;
				goto done;
			}

			if (i % 3 < 2 && (i == 0 || prec < 0.05)) {
				gnm_float d = dpfunc_dx (x, shape, log_p);
				if (d) {
					/*
					 * Deliberately overshoot a bit to help
					 * with getting good points on both
					 * sides of the root.
					 */
					x = x - e / d * 1.000001;
					if (x > xlow && x < xhigh) {
#ifdef DEBUG_pfuncinverter
						printf ("Newton ok\n");
#endif
						i++;
						goto newton_retry;
					}
				}
			}
		}
	}

	ML_ERROR(ME_PRECISION);
 done:
	/* Make sure to keep a lucky near-hit.  */

	if (have_xhigh && gnumabs (e) > exhigh)
		e = exhigh, x = xhigh;
	if (gnumabs (e) > -exlow)
		e = exlow, x = xlow;

#ifdef DEBUG_pfuncinverter
	printf ("--> %.15g\n\n", x);
#endif
	return x;
}

/* ------------------------------------------------------------------------ */

static gnm_float
dgamma1 (gnm_float x, const gnm_float *palpha, gboolean log_p)
{
	if (log_p)
		return 0; /* i.e., bail.  */
	else
		return dgamma (x, *palpha, 1, log_p);
}

static gnm_float
pgamma1 (gnm_float x, const gnm_float *palpha,
	 gboolean lower_tail, gboolean log_p)
{
	return pgamma (x, *palpha, 1, lower_tail, log_p);
}


gnm_float
qgamma (gnm_float p, gnm_float alpha, gnm_float scale,
	gboolean lower_tail, gboolean log_p)
{
	gnm_float res1, x0, v;

#ifdef IEEE_754
	if (isnangnum(p) || isnangnum(alpha) || isnangnum(scale))
		return p + alpha + scale;
#endif
	R_Q_P01_check(p);
	if (alpha <= 0) ML_ERR_return_NAN;

	/* Make an initial guess, x0, assuming scale==1.  */
	v = 2 * alpha;
	if (v < -1.24 * R_DT_log (p))
		x0 = powgnum (R_DT_qIv (p) * alpha * expgnum (lgammagnum (alpha) + alpha * M_LN2gnum),
			      1 / alpha) / 2;
	else {
		gnm_float x1 = qnorm (p, 0, 1, lower_tail, log_p);
		gnm_float p1 = 0.222222 / v;
		x0 = v * powgnum (x1 * sqrtgnum (p1) + 1 - p1, 3) / 2;
	}

	res1 = pfuncinverter (p, &alpha, lower_tail, log_p, 0, gnm_pinf, x0,
			      pgamma1, dgamma1);

	return res1 * scale;
}

/* ------------------------------------------------------------------------ */

static gnm_float
dbeta1 (gnm_float x, const gnm_float shape[], gboolean log_p)
{
	if (log_p)
		return 0; /* i.e., bail.  */
	else
		return dbeta (x, shape[0], shape[1], log_p);
}

static gnm_float
pbeta1 (gnm_float x, const gnm_float shape[],
	gboolean lower_tail, gboolean log_p)
{
	return pbeta (x, shape[0], shape[1], lower_tail, log_p);
}


gnm_float
qbeta (gnm_float p, gnm_float pin, gnm_float qin, gboolean lower_tail, gboolean log_p)
{
	gnm_float x0, shape[2];

#ifdef IEEE_754
	if (isnangnum (pin) || isnangnum (qin) || isnangnum (p))
		return pin + qin + p;
#endif
	R_Q_P01_check (p);

	if (pin < 0. || qin < 0.) ML_ERR_return_NAN;

	/*
	 * For small pin, p seems to have exponent 1, for large pin, it
	 * seems more like 0.
	 *
	 * For small pin, pin itself seems to have exponent 2, for large pin,
	 * it is more like 1.
	 */
	x0 = powgnum (R_DT_qIv (p), 1 / (pin + 1)) *
		powgnum (pin, (pin + 2) / (pin + 1)) /
		qin;
	x0 /= (1 + x0);

	shape[0] = pin;
	shape[1] = qin;
	return pfuncinverter (p, shape, lower_tail, log_p, 0, 1, x0,
			      pbeta1, dbeta1);
}

/* ------------------------------------------------------------------------ */
/* http://www.math.keio.ac.jp/matumoto/CODES/MT2002/mt19937ar.c  */
/* Imported by hand -- MW.  */

/* 
   A C-program for MT19937, with initialization improved 2002/1/26.
   Coded by Takuji Nishimura and Makoto Matsumoto.

   Before using, initialize the state by using init_genrand(seed)  
   or init_by_array(init_key, key_length).

   Copyright (C) 1997 - 2002, Makoto Matsumoto and Takuji Nishimura,
   All rights reserved.                          

   Redistribution and use in source and binary forms, with or without
   modification, are permitted provided that the following conditions
   are met:

     1. Redistributions of source code must retain the above copyright
        notice, this list of conditions and the following disclaimer.

     2. Redistributions in binary form must reproduce the above copyright
        notice, this list of conditions and the following disclaimer in the
        documentation and/or other materials provided with the distribution.

     3. The names of its contributors may not be used to endorse or promote 
        products derived from this software without specific prior written 
        permission.

   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
   A PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE COPYRIGHT OWNER OR
   CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
   EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
   PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
   PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
   LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
   NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
   SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.


   Any feedback is very welcome.
   http://www.math.keio.ac.jp/matumoto/emt.html
   email: matumoto@math.keio.ac.jp
*/

#if 0
#include <stdio.h>
#endif

/* Period parameters */  
#define N 624
#define M 397
#define MATRIX_A 0x9908b0dfUL   /* constant vector a */
#define UPPER_MASK 0x80000000UL /* most significant w-r bits */
#define LOWER_MASK 0x7fffffffUL /* least significant r bits */

static unsigned long mt[N]; /* the array for the state vector  */
static int mti=N+1; /* mti==N+1 means mt[N] is not initialized */

/* initializes mt[N] with a seed */
static void init_genrand(unsigned long s)
{
    mt[0]= s & 0xffffffffUL;
    for (mti=1; mti<N; mti++) {
        mt[mti] = 
	    (1812433253UL * (mt[mti-1] ^ (mt[mti-1] >> 30)) + mti); 
        /* See Knuth TAOCP Vol2. 3rd Ed. P.106 for multiplier. */
        /* In the previous versions, MSBs of the seed affect   */
        /* only MSBs of the array mt[].                        */
        /* 2002/01/09 modified by Makoto Matsumoto             */
        mt[mti] &= 0xffffffffUL;
        /* for >32 bit machines */
    }
}

/* initialize by an array with array-length */
/* init_key is the array for initializing keys */
/* key_length is its length */
static void mt_init_by_array(unsigned long init_key[], int key_length)
{
    int i, j, k;
    init_genrand(19650218UL);
    i=1; j=0;
    k = (N>key_length ? N : key_length);
    for (; k; k--) {
        mt[i] = (mt[i] ^ ((mt[i-1] ^ (mt[i-1] >> 30)) * 1664525UL))
          + init_key[j] + j; /* non linear */
        mt[i] &= 0xffffffffUL; /* for WORDSIZE > 32 machines */
        i++; j++;
        if (i>=N) { mt[0] = mt[N-1]; i=1; }
        if (j>=key_length) j=0;
    }
    for (k=N-1; k; k--) {
        mt[i] = (mt[i] ^ ((mt[i-1] ^ (mt[i-1] >> 30)) * 1566083941UL))
          - i; /* non linear */
        mt[i] &= 0xffffffffUL; /* for WORDSIZE > 32 machines */
        i++;
        if (i>=N) { mt[0] = mt[N-1]; i=1; }
    }

    mt[0] = 0x80000000UL; /* MSB is 1; assuring non-zero initial array */ 
}

/* generates a random number on [0,0xffffffff]-interval */
static unsigned long genrand_int32(void)
{
    unsigned long y;
    static unsigned long mag01[2]={0x0UL, MATRIX_A};
    /* mag01[x] = x * MATRIX_A  for x=0,1 */

    if (mti >= N) { /* generate N words at one time */
        int kk;

        if (mti == N+1)   /* if init_genrand() has not been called, */
            init_genrand(5489UL); /* a default initial seed is used */

        for (kk=0;kk<N-M;kk++) {
            y = (mt[kk]&UPPER_MASK)|(mt[kk+1]&LOWER_MASK);
            mt[kk] = mt[kk+M] ^ (y >> 1) ^ mag01[y & 0x1UL];
        }
        for (;kk<N-1;kk++) {
            y = (mt[kk]&UPPER_MASK)|(mt[kk+1]&LOWER_MASK);
            mt[kk] = mt[kk+(M-N)] ^ (y >> 1) ^ mag01[y & 0x1UL];
        }
        y = (mt[N-1]&UPPER_MASK)|(mt[0]&LOWER_MASK);
        mt[N-1] = mt[M-1] ^ (y >> 1) ^ mag01[y & 0x1UL];

        mti = 0;
    }
  
    y = mt[mti++];

    /* Tempering */
    y ^= (y >> 11);
    y ^= (y << 7) & 0x9d2c5680UL;
    y ^= (y << 15) & 0xefc60000UL;
    y ^= (y >> 18);

    return y;
}

#if 0
/* generates a random number on [0,0x7fffffff]-interval */
long genrand_int31(void)
{
    return (long)(genrand_int32()>>1);
}

/* generates a random number on [0,1]-real-interval */
double genrand_real1(void)
{
    return genrand_int32()*(1.0/4294967295.0); 
    /* divided by 2^32-1 */ 
}

/* generates a random number on [0,1)-real-interval */
double genrand_real2(void)
{
    return genrand_int32()*(1.0/4294967296.0); 
    /* divided by 2^32 */
}

/* generates a random number on (0,1)-real-interval */
double genrand_real3(void)
{
    return (((double)genrand_int32()) + 0.5)*(1.0/4294967296.0); 
    /* divided by 2^32 */
}
#endif

/* generates a random number on [0,1) with 53-bit resolution*/
static double genrand_res53(void) 
{ 
    unsigned long a=genrand_int32()>>5, b=genrand_int32()>>6; 
    return(a*67108864.0+b)*(1.0/9007199254740992.0); 
} 
/* These real versions are due to Isaku Wada, 2002/01/09 added */

#if 0
int main(void)
{
    int i;
    unsigned long init[4]={0x123, 0x234, 0x345, 0x456}, length=4;
    init_by_array(init, length);
    printf("1000 outputs of genrand_int32()\n");
    for (i=0; i<1000; i++) {
      printf("%10lu ", genrand_int32());
      if (i%5==4) printf("\n");
    }
    printf("\n1000 outputs of genrand_real2()\n");
    for (i=0; i<1000; i++) {
      printf("%10.8f ", genrand_real2());
      if (i%5==4) printf("\n");
    }
    return 0;
}
#endif


#undef N
#undef M
#undef MATRIX_A
#undef UPPER_MASK
#undef LOWER_MASK

/* ------------------------------------------------------------------------ */

/* FIXME: we need something that catches partials and EAGAIN.  */
#define fullread read

#define RANDOM_DEVICE "/dev/urandom"

/*
 * Conservative random number generator.  The result is (supposedly) uniform
 * and between 0 and 1.	 (0 possible, 1 not.)  The result should have about
 * 64 bits randomness.
 */
gnm_float
random_01 (void)
{
	static int device_fd = -2;
	static int seeded = -2;

	while (seeded) {
		if (seeded == -2) {
			const char *seed = g_getenv ("GNUMERIC_PRNG_SEED");
			if (seed) {
				int len = strlen (seed);
				int i;
				unsigned long *longs = g_new (unsigned long, len + 1);

				/* We drop only one character into each long.  */
				for (i = 0; i < len; i++)
					longs[i] = (unsigned char)seed[i];
				mt_init_by_array (longs, len);
				g_free (longs);
				seeded = 1;

				g_warning ("Using pseudo-random numbers.");
			} else {
				seeded = 0;
				break;
			}
		}

		/*
		 * Only 52-bit precision.  But hey, if you are using pseudo
		 * random numbers that ought to be good enough to you.
		 */
		return genrand_res53 ();
	}

	if (device_fd == -2) {
		device_fd = open (RANDOM_DEVICE, O_RDONLY);
		/*
		 * We could check that we really have a device, but it hard
		 * to come up with a non-paranoid reason to.
		 */
	}

	if (device_fd >= 0) {
		static ssize_t bytes_left = 0;
		static unsigned char data[32 * sizeof (gnm_float)];
		gnm_float res = 0;
		size_t i;

		if (bytes_left < (ssize_t)sizeof (gnm_float)) {
			ssize_t got = fullread (device_fd, &data, sizeof (data));
			if (got < (ssize_t)sizeof (gnm_float))
				goto failure;
			bytes_left += got;
		}

		bytes_left -= sizeof (gnm_float);
		for (i = 0; i < sizeof (gnm_float); i++)
			res = (res + data[bytes_left + i]) / 256;
		return res;

	failure:
		/* It failed when it shouldn't.	 Disable.  */
		g_warning ("Reading from %s failed; reverting to pseudo-random.",
			   RANDOM_DEVICE);
		close (device_fd);
		device_fd = -1;
	}

#ifdef HAVE_RANDOM
	{
		int r1, r2;

		r1 = random () & 2147483647;
		r2 = random () & 2147483647;

		return (r1 + (r2 / 2147483648.0)) / 2147483648.0;
	}
#elif defined (HAVE_DRAND48)
	return drand48 ();
#else
	{
		/*
		 * We try to work around lack of randomness in rand's
		 * lower bits.
		 */
		const int prime = 65537;
		int r1, r2, r3, r4;

		g_assert (RAND_MAX > ((1 << 12) - 1));

		r1 = (rand () ^ (rand () << 12)) % prime;
		r2 = (rand () ^ (rand () << 12)) % prime;
		r3 = (rand () ^ (rand () << 12)) % prime;
		r4 = (rand () ^ (rand () << 12)) % prime;

		return (r1 + (r2 + (r3 + r4 / (gnm_float)prime) / prime) / prime) / prime;
	}
#endif
}

/*
 * Generate a N(0,1) distributed number.
 */
gnm_float
random_normal (void)
{
	static gboolean  has_saved = FALSE;
	static gnm_float saved;

	if (has_saved) {
		has_saved = FALSE;
		return saved;
	} else {
		gnm_float u, v, r2, rsq;
		do {
			u = 2 * random_01 () - 1;
			v = 2 * random_01 () - 1;
			r2 = u * u + v * v;
		} while (r2 > 1 || r2 == 0);

		rsq = sqrtgnum (-2 * loggnum (r2) / r2);

		has_saved = TRUE;
		saved = v * rsq;

		return u * rsq;
	}
}

gnm_float
random_lognormal (gnm_float zeta, gnm_float sigma)
{
	return expgnum (sigma * random_normal () + zeta);
}

static gnm_float
random_gaussian (gnm_float sigma)
{
	return sigma * random_normal ();
}

/*
 * Generate a poisson distributed number.
 */
gnm_float
random_poisson (gnm_float lambda)
{
	/*
	 * This may not be optimal code, but it sure is easy to
	 * understand compared to R's code.
	 */
	return qpois (random_01 (), lambda, TRUE, FALSE);
}

/*
 * Generate a binomial distributed number.
 */
gnm_float
random_binomial (gnm_float p, int trials)
{
	return qbinom (random_01 (), trials, p, TRUE, FALSE);
}

/*
 * Generate a negative binomial distributed number.
 */
gnm_float
random_negbinom (gnm_float p, int f)
{
	return qnbinom (random_01 (), f, p, TRUE, FALSE);
}

/*
 * Generate an exponential distributed number.
 */
gnm_float
random_exponential (gnm_float b)
{
	return -b * loggnum (random_01 ());
}

/*
 * Generate a bernoulli distributed number.
 */
gnm_float
random_bernoulli (gnm_float p)
{
	gnm_float r = random_01 ();

	return (r <= p) ? 1.0 : 0.0;
}

/*
 * Generate a cauchy distributed number. From the GNU Scientific library 1.1.1.
 * Copyright (C) 1996, 1997, 1998, 1999, 2000 James Theiler, Brian Gough.
 */
gnm_float
random_cauchy (gnm_float a)
{
	gnm_float u;

	do {
		u = random_01 ();
	} while (u == 0.5);

	return a * tangnum (M_PIgnum * u);
}

/*
 * Generate a Weibull distributed number. From the GNU Scientific
 * library 1.1.1.
 * Copyright (C) 1996, 1997, 1998, 1999, 2000 James Theiler, Brian Gough.
 */
gnm_float
random_weibull (gnm_float a, gnm_float b)
{
	gnm_float x, z;

	do {
		x = random_01 ();
	} while (x == 0.0);

	z = powgnum (-loggnum (x), 1 / b);

	return a * z;
}

/*
 * Generate a Laplace (two-sided exponential probability) distributed number.
 * From the GNU Scientific library 1.1.1.
 * Copyright (C) 1996, 1997, 1998, 1999, 2000 James Theiler, Brian Gough.
 */
gnm_float
random_laplace (gnm_float a)
{
	gnm_float u;

	do {
		u = 2 * random_01 () - 1.0;
	} while (u == 0.0);

	if (u < 0)
		return a * loggnum (-u);
	else
		return -a * loggnum (u);
}

gnm_float
random_laplace_pdf (gnm_float x, gnm_float a)
{
	return (1 / (2 * a)) * expgnum (-gnumabs (x) / a);
}

/*
 * Generate a Rayleigh distributed number.  From the GNU Scientific library
 * 1.1.1.
 * Copyright (C) 1996, 1997, 1998, 1999, 2000 James Theiler, Brian Gough.
 */
gnm_float
random_rayleigh (gnm_float sigma)
{
	gnm_float u;

	do {
		u = random_01 ();
	} while (u == 0.0);

	return sigma * sqrtgnum (-2.0 * loggnum (u));
}

/*
 * Generate a Rayleigh tail distributed number.	 From the GNU Scientific library
 * 1.1.1.  The Rayleigh tail distribution has the form
 *   p(x) dx = (x / sigma^2) exp((a^2 - x^2)/(2 sigma^2)) dx
 *
 *   for x = a ... +infty
 *
 * Copyright (C) 1996, 1997, 1998, 1999, 2000 James Theiler, Brian Gough.
 */
gnm_float
random_rayleigh_tail (gnm_float a, gnm_float sigma)
{
	gnm_float u;

	do {
		u = random_01 ();
	} while (u == 0.0);

	return sqrtgnum (a * a - 2.0 * sigma * sigma * loggnum (u));
}

/* The Gamma distribution of order a>0 is defined by:
 *
 *  p(x) dx = {1 / \Gamma(a) b^a } x^{a-1} e^{-x/b} dx
 *
 *  for x>0.  If X and Y are independent gamma-distributed random
 *   variables of order a1 and a2 with the same scale parameter b, then
 *   X+Y has gamma distribution of order a1+a2.
 *
 *  The algorithms below are from Knuth, vol 2, 2nd ed, p. 129.
 */

static gnm_float
gamma_frac (gnm_float a)
{
	/* This is exercise 16 from Knuth; see page 135, and the solution is
	 * on page 551.	 */

	gnm_float x, q;
	gnm_float p = M_Egnum / (a + M_Egnum);
	do {
		gnm_float v;
		gnm_float u = random_01 ();
		do {
			v = random_01 ();
		} while (v == 0.0);

		if (u < p) {
			x = powgnum (v, 1 / a);
			q = expgnum (-x);
		} else {
			x = 1 - loggnum (v);
			q = powgnum (x, a - 1);
		}
	} while (random_01 () >= q);

	return x;
}

static gnm_float
gamma_large (gnm_float a)
{
	/*
	 * Works only if a > 1, and is most efficient if a is large
	 *
	 * This algorithm, reported in Knuth, is attributed to Ahrens.	A
	 * faster one, we are told, can be found in: J. H. Ahrens and
	 * U. Dieter, Computing 12 (1974) 223-246.
	 */

	gnm_float sqa, x, y, v;
	sqa = sqrtgnum (2 * a - 1);
	do {
		do {
			y = tangnum (M_PIgnum * random_01 ());
			x = sqa * y + a - 1;
		} while (x <= 0);
		v = random_01 ();
	} while (v > (1 + y * y) * expgnum ((a - 1) * loggnum (x / (a - 1)) -
					    sqa * y));

	return x;
}

static gnm_float
ran_gamma_int (unsigned int a)
{
	if (a < 12) {
		gnm_float prod;

		do {
			unsigned int i;
			prod = 1;

			for (i = 0; i < a; i++)
				prod *= random_01 ();

			/*
			 * This handles the 0-probability event of getting
			 * an actual zero as well as the possibility of
			 * underflow.
			 */
		} while (prod == 0);

		return -loggnum (prod);
	} else
		return gamma_large (a);
}

/*
 * Generate a Gamma distributed number.	 From the GNU Scientific library 1.1.1.
 * Copyright (C) 1996, 1997, 1998, 1999, 2000 James Theiler, Brian Gough.
 */
gnm_float
random_gamma (gnm_float a, gnm_float b)
{
	/* assume a > 0 */
	/* FIXME: why not simply a gnm_float?	*/
	unsigned int na = floorgnum (a);

	if (a == na)
		return b * ran_gamma_int (na);
	else if (na == 0)
		return b * gamma_frac (a);
	else
		return b * (ran_gamma_int (na) + gamma_frac (a - na));
}

/*
 * Generate a Pareto distributed number. From the GNU Scientific library 1.1.1.
 * Copyright (C) 1996, 1997, 1998, 1999, 2000 James Theiler, Brian Gough.
 */
gnm_float
random_pareto (gnm_float a, gnm_float b)
{
	gnm_float x;

	do {
		x = random_01 ();
	} while (x == 0.0);

	return b * powgnum (x, -1 / a);
}

/*
 * Generate a F-distributed number. From the GNU Scientific library 1.1.1.
 * Copyright (C) 1996, 1997, 1998, 1999, 2000 James Theiler, Brian Gough.
 */
gnm_float
random_fdist (gnm_float nu1, gnm_float nu2)
{
	gnm_float Y1 = random_gamma (nu1 / 2, 2.0);
	gnm_float Y2 = random_gamma (nu2 / 2, 2.0);

	return (Y1 * nu2) / (Y2 * nu1);
}

/*
 * Generate a Beta-distributed number. From the GNU Scientific library 1.1.1.
 * Copyright (C) 1996, 1997, 1998, 1999, 2000 James Theiler, Brian Gough.
 */
gnm_float
random_beta (gnm_float a, gnm_float b)
{
	gnm_float x1 = random_gamma (a, 1.0);
	gnm_float x2 = random_gamma (b, 1.0);

	return x1 / (x1 + x2);
}

/*
 * Generate a Chi-Square-distributed number. From the GNU Scientific library
 * 1.1.1.
 * Copyright (C) 1996, 1997, 1998, 1999, 2000 James Theiler, Brian Gough.
 */
gnm_float
random_chisq (gnm_float nu)
{
	return 2 * random_gamma (nu / 2, 1.0);
}

/*
 * Generate a logistic-distributed number. From the GNU Scientific library
 * 1.1.1.
 * Copyright (C) 1996, 1997, 1998, 1999, 2000 James Theiler, Brian Gough.
 */
gnm_float
random_logistic (gnm_float a)
{
	gnm_float x;

	do {
		x = random_01 ();
	} while (x == 0 || x == 1);

	return a * loggnum (x / (1 - x));
}

/*
 * Generate a geometric-distributed number. From the GNU Scientific library
 * 1.1.1.
 * Copyright (C) 1996, 1997, 1998, 1999, 2000 James Theiler, Brian Gough.
 */
gnm_float
random_geometric (gnm_float p)
{
	gnm_float u;

	if (p == 1)
		return 1;
	do {
		u = random_01 ();
	} while (u == 0);

	return floorgnum (loggnum (u) / log1pgnum (-p) + 1);
}

/*
 * Generate a hypergeometric-distributed number. From the GNU Scientific
 * library 1.1.1.
 * Copyright (C) 1996, 1997, 1998, 1999, 2000 James Theiler, Brian Gough.
 */
gnm_float
random_hypergeometric (unsigned int n1, unsigned int n2, unsigned int t)
{
	unsigned int n = n1 + n2;

	unsigned int i = 0;
	unsigned int a = n1;
	unsigned int b = n1 + n2;
	unsigned int k = 0;

	/* FIXME: performance for large t?  */

	if (t > n)
		t = n;

	if (t < n / 2) {
		for (i = 0 ; i < t ; i++) {
			gnm_float u = random_01 ();

			if (b * u < a) {
				k++;
				if (k == n1)
					return k ;
				a-- ;
			}
			b--;
		}
		return k;
	} else {
		for (i = 0 ; i < n - t ; i++) {
			gnm_float u = random_01 ();

			if (b * u < a) {
				k++;
				if (k == n1)
					return n1 - k;
				a--;
			}
			b-- ;
		}
		return n1 - k;
	}
}


/*
 * Generate a logarithmic-distributed number. From the GNU Scientific library
 * 1.1.1.
 * Copyright (C) 1996, 1997, 1998, 1999, 2000 James Theiler, Brian Gough.
 */
gnm_float
random_logarithmic (gnm_float p)
{
	gnm_float c, v;

	c = log1pgnum (-p);
	do {
		v = random_01 ();
	} while (v == 0);

	if (v >= p)
		return 1;
	else {
		gnm_float u, q;

		do {
			u = random_01 ();
		} while (u == 0);
		q = expm1gnum (c * u);

		if (v <= q * q)
			return floorgnum (1 + loggnum (v) / loggnum (q));
		else if (v <= q)
			return 2;
		else
			return 1;
	}
}

/*
 * Generate a T-distributed number. From the GNU Scientific library 1.1.1.
 * Copyright (C) 1996, 1997, 1998, 1999, 2000 James Theiler, Brian Gough.
 */
gnm_float
random_tdist (gnm_float nu)
{
	if (nu <= 2) {
		gnm_float Y1 = random_normal ();
		gnm_float Y2 = random_chisq (nu);

		gnm_float t = Y1 / sqrtgnum (Y2 / nu);

		return t;
	} else {
		gnm_float Y1, Y2, Z, t;
		do {
			Y1 = random_normal ();
			Y2 = random_exponential (1 / (nu / 2 - 1));

			Z = Y1 * Y1 / (nu - 2);
		} while (1 - Z < 0 || expgnum (-Y2 - Z) > (1 - Z));

		/* Note that there is a typo in Knuth's formula, the line below
		 * is taken from the original paper of Marsaglia, Mathematics
		 * of Computation, 34 (1980), p 234-256. */

		t = Y1 / sqrtgnum ((1 - 2 / nu) * (1 - Z));
		return t;
	}
}

/*
 * Generate a Type I Gumbel-distributed random number. From the GNU
 * Scientific library 1.1.1.
 * Copyright (C) 1996, 1997, 1998, 1999, 2000 James Theiler, Brian Gough.
 */
gnm_float
random_gumbel1 (gnm_float a, gnm_float b)
{
	gnm_float x;

	do {
		x = random_01 ();
	} while (x == 0.0);

	return (loggnum (b) - loggnum (-loggnum (x))) / a;
}

/*
 * Generate a Type II Gumbel-distributed random number. From the GNU
 * Scientific library 1.1.1.
 * Copyright (C) 1996, 1997, 1998, 1999, 2000 James Theiler, Brian Gough.
 */
gnm_float
random_gumbel2 (gnm_float a, gnm_float b)
{
	gnm_float x;

	do {
		x = random_01 ();
	} while (x == 0.0);

	return powgnum (-b / loggnum (x), 1 / a);
}

/*
 * Generate a stable Levy-distributed random number. From the GNU
 * Scientific library 1.1.1.
 * Copyright (C) 1996, 1997, 1998, 1999, 2000 James Theiler, Brian Gough.
 *
 * The stable Levy probability distributions have the form
 *
 * p(x) dx = (1/(2 pi)) \int dt exp(- it x - |c t|^alpha)
 *
 * with 0 < alpha <= 2.
 *
 * For alpha = 1, we get the Cauchy distribution
 * For alpha = 2, we get the Gaussian distribution with sigma = sqrt(2) c.
 *
 * Fromn Chapter 5 of Bratley, Fox and Schrage "A Guide to
 * Simulation". The original reference given there is,
 *
 * J.M. Chambers, C.L. Mallows and B. W. Stuck. "A method for
 * simulating stable random variates". Journal of the American
 * Statistical Association, JASA 71 340-344 (1976).
 */
gnm_float
random_levy (gnm_float c, gnm_float alpha)
{
	gnm_float u, v, t, s;

	do {
		u = random_01 ();
	} while (u == 0.0);

	u = M_PIgnum * (u - 0.5);

	if (alpha == 1) {	      /* cauchy case */
		t = tangnum (u);
		return c * t;
	}

	do {
		v = random_exponential (1.0);
	} while (v == 0);

	if (alpha == 2) {	     /* gaussian case */
		t = 2 * singnum (u) * sqrtgnum (v);
		return c * t;
	}

	/* general case */

	t = singnum (alpha * u) / powgnum (cosgnum (u), 1 / alpha);
	s = powgnum (cosgnum ((1 - alpha) * u) / v, (1 - alpha) / alpha);

	return c * t * s;
}

/*
 * The following routine for the skew-symmetric case was provided by
 * Keith Briggs.
 *
 * The stable Levy probability distributions have the form
 *
 * 2*pi* p(x) dx
 *
 *  = int dt exp(mu*i*t-|sigma*t|^alpha*(1-i*beta*sign(t)*tan(pi*alpha/2))) for
 *    alpha != 1
 *  = int dt exp(mu*i*t-|sigma*t|^alpha*(1+i*beta*sign(t)*2/pi*log(|t|)))   for
      alpha == 1
 *
 *  with 0<alpha<=2, -1<=beta<=1, sigma>0.
 *
 *  For beta=0, sigma=c, mu=0, we get gsl_ran_levy above.
 *
 *  For alpha = 1, beta=0, we get the Lorentz distribution
 *  For alpha = 2, beta=0, we get the Gaussian distribution
 *
 *  See A. Weron and R. Weron: Computer simulation of Lévy alpha-stable
 *  variables and processes, preprint Technical University of Wroclaw.
 *  http://www.im.pwr.wroc.pl/~hugo/Publications.html
 */
gnm_float
random_levy_skew (gnm_float c, gnm_float alpha, gnm_float beta)
{
	gnm_float V, W, X;

	if (beta == 0) /* symmetric case */
		return random_levy (c, alpha);

	do {
		V = random_01 ();
	} while (V == 0.0);

	V = M_PIgnum * (V - 0.5);

	do {
		W = random_exponential (1.0);
	} while (W == 0);

	if (alpha == 1) {
		X = ((M_PI_2gnum + beta * V) * tangnum (V) -
		     beta * loggnum (M_PI_2gnum * W * cosgnum (V) /
				     (M_PI_2gnum + beta * V))) / M_PI_2gnum;
		return c * (X + beta * loggnum (c) / M_PI_2gnum);
	} else {
		gnm_float t = beta * tangnum (M_PI_2gnum * alpha);
		gnm_float B = atangnum (t) / alpha;
		gnm_float S = pow1p (t * t, 1 / (2 * alpha));

		X = S * singnum (alpha * (V + B)) / powgnum (cosgnum (V),
							     1 / alpha)
			* powgnum (cosgnum (V - alpha * (V + B)) / W,
				   (1 - alpha) / alpha);
		return c * X;
	}
}

gnm_float
random_exppow_pdf (gnm_float x, gnm_float a, gnm_float b)
{
	gnm_float lngamma = lgamma1p (1 / b);

	return (1 / (2 * a)) * expgnum (-powgnum (gnumabs (x / a), b) - lngamma);
}

/*
 * The exponential power probability distribution is
 *
 *  p(x) dx = (1/(2 a Gamma(1+1/b))) * exp(-|x/a|^b) dx
 *
 * for -infty < x < infty. For b = 1 it reduces to the Laplace
 * distribution.
 *
 * The exponential power distribution is related to the gamma
 * distribution by E = a * pow(G(1/b),1/b), where E is an exponential
 * power variate and G is a gamma variate.
 *
 * We use this relation for b < 1. For b >=1 we use rejection methods
 * based on the laplace and gaussian distributions which should be
 *  faster.
 *
 * See P. R. Tadikamalla, "Random Sampling from the Exponential Power
 * Distribution", Journal of the American Statistical Association,
 * September 1980, Volume 75, Number 371, pages 683-686.
 *
 * Copyright (C) 1996, 1997, 1998, 1999, 2000 James Theiler, Brian Gough
 */

gnm_float
random_exppow (gnm_float a, gnm_float b)
{
	if (b < 1) {
		gnm_float u = random_01 ();
		gnm_float v = random_gamma (1 / b, 1.0);
		gnm_float z = a * powgnum (v, 1 / b) ;

		if (u > 0.5)
			return z;
		else
			return -z;
	} else if (b == 1)
		return random_laplace (a);   /* Laplace distribution */
	else if (b < 2) {
		/* Use laplace distribution for rejection method */
		gnm_float x, y, h, ratio, u;

		/* Scale factor chosen by upper bound on ratio at b = 2 */
		gnm_float s = 1.4489;
		do {
			x     = random_laplace (a);
			y     = random_laplace_pdf (x, a);
			h     = random_exppow_pdf (x, a, b);
			ratio = h / (s * y);
			u     = random_01 ();
		} while (u > ratio);

		return x ;
	} else if (b == 2)   /* Gaussian distribution */
		return random_gaussian (a / sqrtgnum (2.0));
	else {
		/* Use gaussian for rejection method */
		gnm_float x, y, h, ratio, u;
		const gnm_float sigma = a / sqrtgnum (2.0);

		/* Scale factor chosen by upper bound on ratio at b = infinity.
		 * This could be improved by using a rational function
		 * approximation to the bounding curve. */

		gnm_float s = 2.4091 ;	 /* this is sqrt(pi) e / 2 */

		do {
			x     = random_gaussian (sigma) ;
			y     = dnorm (x, 0.0, gnumabs (sigma), FALSE) ;
			h     = random_exppow_pdf (x, a, b) ;
			ratio = h / (s * y) ;
			u     = random_01 ();
		} while (u > ratio);

		return x;
	}
}

/*
 * Generate a Gaussian tail-distributed random number. From the GNU
 * Scientific library 1.1.1.
 * Copyright (C) 1996, 1997, 1998, 1999, 2000 James Theiler, Brian Gough
 */
gnm_float
random_gaussian_tail (gnm_float a, gnm_float sigma)
{
	/*
	 * Returns a gaussian random variable larger than a
	 * This implementation does one-sided upper-tailed deviates.
	 */

	gnm_float s = a / sigma;

	if (s < 1) {
		/* For small s, use a direct rejection method. The limit s < 1
		 * can be adjusted to optimise the overall efficiency */

		gnm_float x;

		do {
			x = random_gaussian (1.0);
		} while (x < s);
		return x * sigma;
	} else {
		/* Use the "supertail" deviates from the last two steps
		 * of Marsaglia's rectangle-wedge-tail method, as described
		 * in Knuth, v2, 3rd ed, pp 123-128.  (See also exercise 11,
		 * p139, and the solution, p586.)
		 */

		gnm_float u, v, x;

		do {
			u = random_01 ();
			do {
				v = random_01 ();
			} while (v == 0.0);
			x = sqrtgnum (s * s - 2 * loggnum (v));
		} while (x * u > s);
		return x * sigma;
	}
}

/*
 * Generate a Landau-distributed random number. From the GNU Scientific
 * library 1.1.1.
 *
 * Copyright (C) 2001 David Morrison
 *
 * Adapted from the CERN library routines DENLAN, RANLAN, and DISLAN
 * as described in http://consult.cern.ch/shortwrups/g110/top.html.
 * Original author: K.S. K\"olbig.
 *
 * The distribution is given by the complex path integral,
 *
 *  p(x) = (1/(2 pi i)) \int_{c-i\inf}^{c+i\inf} ds exp(s log(s) + x s)
 *
 * which can be converted into a real integral over [0,+\inf]
 *
 *  p(x) = (1/pi) \int_0^\inf dt \exp(-t log(t) - x t) sin(pi t)
 */

gnm_float
random_landau (void)
{
	static gnm_float F[982] = {
		00.000000, 00.000000, 00.000000, 00.000000, 00.000000,
		-2.244733, -2.204365, -2.168163, -2.135219, -2.104898,
		-2.076740, -2.050397, -2.025605, -2.002150, -1.979866,
		-1.958612, -1.938275, -1.918760, -1.899984, -1.881879,
		-1.864385, -1.847451, -1.831030, -1.815083, -1.799574,
		-1.784473, -1.769751, -1.755383, -1.741346, -1.727620,
		-1.714187, -1.701029, -1.688130, -1.675477, -1.663057,
		-1.650858, -1.638868, -1.627078, -1.615477, -1.604058,
		-1.592811, -1.581729, -1.570806, -1.560034, -1.549407,
		-1.538919, -1.528565, -1.518339, -1.508237, -1.498254,
		-1.488386, -1.478628, -1.468976, -1.459428, -1.449979,
		-1.440626, -1.431365, -1.422195, -1.413111, -1.404112,
		-1.395194, -1.386356, -1.377594, -1.368906, -1.360291,
		-1.351746, -1.343269, -1.334859, -1.326512, -1.318229,
		-1.310006, -1.301843, -1.293737, -1.285688, -1.277693,
		-1.269752, -1.261863, -1.254024, -1.246235, -1.238494,
		-1.230800, -1.223153, -1.215550, -1.207990, -1.200474,
		-1.192999, -1.185566, -1.178172, -1.170817, -1.163500,
		-1.156220, -1.148977, -1.141770, -1.134598, -1.127459,
		-1.120354, -1.113282, -1.106242, -1.099233, -1.092255,
		-1.085306, -1.078388, -1.071498, -1.064636, -1.057802,
		-1.050996, -1.044215, -1.037461, -1.030733, -1.024029,
		-1.017350, -1.010695, -1.004064, -0.997456, -0.990871,
		-0.984308, -0.977767, -0.971247, -0.964749, -0.958271,
		-0.951813, -0.945375, -0.938957, -0.932558, -0.926178,
		-0.919816, -0.913472, -0.907146, -0.900838, -0.894547,
		-0.888272, -0.882014, -0.875773, -0.869547, -0.863337,
		-0.857142, -0.850963, -0.844798, -0.838648, -0.832512,
		-0.826390, -0.820282, -0.814187, -0.808106, -0.802038,
		-0.795982, -0.789940, -0.783909, -0.777891, -0.771884,
		-0.765889, -0.759906, -0.753934, -0.747973, -0.742023,
		-0.736084, -0.730155, -0.724237, -0.718328, -0.712429,
		-0.706541, -0.700661, -0.694791, -0.688931, -0.683079,
		-0.677236, -0.671402, -0.665576, -0.659759, -0.653950,
		-0.648149, -0.642356, -0.636570, -0.630793, -0.625022,
		-0.619259, -0.613503, -0.607754, -0.602012, -0.596276,
		-0.590548, -0.584825, -0.579109, -0.573399, -0.567695,
		-0.561997, -0.556305, -0.550618, -0.544937, -0.539262,
		-0.533592, -0.527926, -0.522266, -0.516611, -0.510961,
		-0.505315, -0.499674, -0.494037, -0.488405, -0.482777,
		-0.477153, -0.471533, -0.465917, -0.460305, -0.454697,
		-0.449092, -0.443491, -0.437893, -0.432299, -0.426707,
		-0.421119, -0.415534, -0.409951, -0.404372, -0.398795,
		-0.393221, -0.387649, -0.382080, -0.376513, -0.370949,
		-0.365387, -0.359826, -0.354268, -0.348712, -0.343157,
		-0.337604, -0.332053, -0.326503, -0.320955, -0.315408,
		-0.309863, -0.304318, -0.298775, -0.293233, -0.287692,
		-0.282152, -0.276613, -0.271074, -0.265536, -0.259999,
		-0.254462, -0.248926, -0.243389, -0.237854, -0.232318,
		-0.226783, -0.221247, -0.215712, -0.210176, -0.204641,
		-0.199105, -0.193568, -0.188032, -0.182495, -0.176957,
		-0.171419, -0.165880, -0.160341, -0.154800, -0.149259,
		-0.143717, -0.138173, -0.132629, -0.127083, -0.121537,
		-0.115989, -0.110439, -0.104889, -0.099336, -0.093782,
		-0.088227, -0.082670, -0.077111, -0.071550, -0.065987,
		-0.060423, -0.054856, -0.049288, -0.043717, -0.038144,
		-0.032569, -0.026991, -0.021411, -0.015828, -0.010243,
		-0.004656, 00.000934, 00.006527, 00.012123, 00.017722,
		00.023323, 00.028928, 00.034535, 00.040146, 00.045759,
		00.051376, 00.056997, 00.062620, 00.068247, 00.073877,
		00.079511, 00.085149, 00.090790, 00.096435, 00.102083,
		00.107736, 00.113392, 00.119052, 00.124716, 00.130385,
		00.136057, 00.141734, 00.147414, 00.153100, 00.158789,
		00.164483, 00.170181, 00.175884, 00.181592, 00.187304,
		00.193021, 00.198743, 00.204469, 00.210201, 00.215937,
		00.221678, 00.227425, 00.233177, 00.238933, 00.244696,
		00.250463, 00.256236, 00.262014, 00.267798, 00.273587,
		00.279382, 00.285183, 00.290989, 00.296801, 00.302619,
		00.308443, 00.314273, 00.320109, 00.325951, 00.331799,
		00.337654, 00.343515, 00.349382, 00.355255, 00.361135,
		00.367022, 00.372915, 00.378815, 00.384721, 00.390634,
		00.396554, 00.402481, 00.408415, 00.414356, 00.420304,
		00.426260, 00.432222, 00.438192, 00.444169, 00.450153,
		00.456145, 00.462144, 00.468151, 00.474166, 00.480188,
		00.486218, 00.492256, 00.498302, 00.504356, 00.510418,
		00.516488, 00.522566, 00.528653, 00.534747, 00.540850,
		00.546962, 00.553082, 00.559210, 00.565347, 00.571493,
		00.577648, 00.583811, 00.589983, 00.596164, 00.602355,
		00.608554, 00.614762, 00.620980, 00.627207, 00.633444,
		00.639689, 00.645945, 00.652210, 00.658484, 00.664768,
		00.671062, 00.677366, 00.683680, 00.690004, 00.696338,
		00.702682, 00.709036, 00.715400, 00.721775, 00.728160,
		00.734556, 00.740963, 00.747379, 00.753807, 00.760246,
		00.766695, 00.773155, 00.779627, 00.786109, 00.792603,
		00.799107, 00.805624, 00.812151, 00.818690, 00.825241,
		00.831803, 00.838377, 00.844962, 00.851560, 00.858170,
		00.864791, 00.871425, 00.878071, 00.884729, 00.891399,
		00.898082, 00.904778, 00.911486, 00.918206, 00.924940,
		00.931686, 00.938446, 00.945218, 00.952003, 00.958802,
		00.965614, 00.972439, 00.979278, 00.986130, 00.992996,
		00.999875, 01.006769, 01.013676, 01.020597, 01.027533,
		01.034482, 01.041446, 01.048424, 01.055417, 01.062424,
		01.069446, 01.076482, 01.083534, 01.090600, 01.097681,
		01.104778, 01.111889, 01.119016, 01.126159, 01.133316,
		01.140490, 01.147679, 01.154884, 01.162105, 01.169342,
		01.176595, 01.183864, 01.191149, 01.198451, 01.205770,
		01.213105, 01.220457, 01.227826, 01.235211, 01.242614,
		01.250034, 01.257471, 01.264926, 01.272398, 01.279888,
		01.287395, 01.294921, 01.302464, 01.310026, 01.317605,
		01.325203, 01.332819, 01.340454, 01.348108, 01.355780,
		01.363472, 01.371182, 01.378912, 01.386660, 01.394429,
		01.402216, 01.410024, 01.417851, 01.425698, 01.433565,
		01.441453, 01.449360, 01.457288, 01.465237, 01.473206,
		01.481196, 01.489208, 01.497240, 01.505293, 01.513368,
		01.521465, 01.529583, 01.537723, 01.545885, 01.554068,
		01.562275, 01.570503, 01.578754, 01.587028, 01.595325,
		01.603644, 01.611987, 01.620353, 01.628743, 01.637156,
		01.645593, 01.654053, 01.662538, 01.671047, 01.679581,
		01.688139, 01.696721, 01.705329, 01.713961, 01.722619,
		01.731303, 01.740011, 01.748746, 01.757506, 01.766293,
		01.775106, 01.783945, 01.792810, 01.801703, 01.810623,
		01.819569, 01.828543, 01.837545, 01.846574, 01.855631,
		01.864717, 01.873830, 01.882972, 01.892143, 01.901343,
		01.910572, 01.919830, 01.929117, 01.938434, 01.947781,
		01.957158, 01.966566, 01.976004, 01.985473, 01.994972,
		02.004503, 02.014065, 02.023659, 02.033285, 02.042943,
		02.052633, 02.062355, 02.072110, 02.081899, 02.091720,
		02.101575, 02.111464, 02.121386, 02.131343, 02.141334,
		02.151360, 02.161421, 02.171517, 02.181648, 02.191815,
		02.202018, 02.212257, 02.222533, 02.232845, 02.243195,
		02.253582, 02.264006, 02.274468, 02.284968, 02.295507,
		02.306084, 02.316701, 02.327356, 02.338051, 02.348786,
		02.359562, 02.370377, 02.381234, 02.392131, 02.403070,
		02.414051, 02.425073, 02.436138, 02.447246, 02.458397,
		02.469591, 02.480828, 02.492110, 02.503436, 02.514807,
		02.526222, 02.537684, 02.549190, 02.560743, 02.572343,
		02.583989, 02.595682, 02.607423, 02.619212, 02.631050,
		02.642936, 02.654871, 02.666855, 02.678890, 02.690975,
		02.703110, 02.715297, 02.727535, 02.739825, 02.752168,
		02.764563, 02.777012, 02.789514, 02.802070, 02.814681,
		02.827347, 02.840069, 02.852846, 02.865680, 02.878570,
		02.891518, 02.904524, 02.917588, 02.930712, 02.943894,
		02.957136, 02.970439, 02.983802, 02.997227, 03.010714,
		03.024263, 03.037875, 03.051551, 03.065290, 03.079095,
		03.092965, 03.106900, 03.120902, 03.134971, 03.149107,
		03.163312, 03.177585, 03.191928, 03.206340, 03.220824,
		03.235378, 03.250005, 03.264704, 03.279477, 03.294323,
		03.309244, 03.324240, 03.339312, 03.354461, 03.369687,
		03.384992, 03.400375, 03.415838, 03.431381, 03.447005,
		03.462711, 03.478500, 03.494372, 03.510328, 03.526370,
		03.542497, 03.558711, 03.575012, 03.591402, 03.607881,
		03.624450, 03.641111, 03.657863, 03.674708, 03.691646,
		03.708680, 03.725809, 03.743034, 03.760357, 03.777779,
		03.795300, 03.812921, 03.830645, 03.848470, 03.866400,
		03.884434, 03.902574, 03.920821, 03.939176, 03.957640,
		03.976215, 03.994901, 04.013699, 04.032612, 04.051639,
		04.070783, 04.090045, 04.109425, 04.128925, 04.148547,
		04.168292, 04.188160, 04.208154, 04.228275, 04.248524,
		04.268903, 04.289413, 04.310056, 04.330832, 04.351745,
		04.372794, 04.393982, 04.415310, 04.436781, 04.458395,
		04.480154, 04.502060, 04.524114, 04.546319, 04.568676,
		04.591187, 04.613854, 04.636678, 04.659662, 04.682807,
		04.706116, 04.729590, 04.753231, 04.777041, 04.801024,
		04.825179, 04.849511, 04.874020, 04.898710, 04.923582,
		04.948639, 04.973883, 04.999316, 05.024942, 05.050761,
		05.076778, 05.102993, 05.129411, 05.156034, 05.182864,
		05.209903, 05.237156, 05.264625, 05.292312, 05.320220,
		05.348354, 05.376714, 05.405306, 05.434131, 05.463193,
		05.492496, 05.522042, 05.551836, 05.581880, 05.612178,
		05.642734, 05.673552, 05.704634, 05.735986, 05.767610,
		05.799512, 05.831694, 05.864161, 05.896918, 05.929968,
		05.963316, 05.996967, 06.030925, 06.065194, 06.099780,
		06.134687, 06.169921, 06.205486, 06.241387, 06.277630,
		06.314220, 06.351163, 06.388465, 06.426130, 06.464166,
		06.502578, 06.541371, 06.580553, 06.620130, 06.660109,
		06.700495, 06.741297, 06.782520, 06.824173, 06.866262,
		06.908795, 06.951780, 06.995225, 07.039137, 07.083525,
		07.128398, 07.173764, 07.219632, 07.266011, 07.312910,
		07.360339, 07.408308, 07.456827, 07.505905, 07.555554,
		07.605785, 07.656608, 07.708035, 07.760077, 07.812747,
		07.866057, 07.920019, 07.974647, 08.029953, 08.085952,
		08.142657, 08.200083, 08.258245, 08.317158, 08.376837,
		08.437300, 08.498562, 08.560641, 08.623554, 08.687319,
		08.751955, 08.817481, 08.883916, 08.951282, 09.019600,
		09.088889, 09.159174, 09.230477, 09.302822, 09.376233,
		09.450735, 09.526355, 09.603118, 09.681054, 09.760191,
		09.840558, 09.922186, 10.005107, 10.089353, 10.174959,
		10.261958, 10.350389, 10.440287, 10.531693, 10.624646,
		10.719188, 10.815362, 10.913214, 11.012789, 11.114137,
		11.217307, 11.322352, 11.429325, 11.538283, 11.649285,
		11.762390, 11.877664, 11.995170, 12.114979, 12.237161,
		12.361791, 12.488946, 12.618708, 12.751161, 12.886394,
		13.024498, 13.165570, 13.309711, 13.457026, 13.607625,
		13.761625, 13.919145, 14.080314, 14.245263, 14.414134,
		14.587072, 14.764233, 14.945778, 15.131877, 15.322712,
		15.518470, 15.719353, 15.925570, 16.137345, 16.354912,
		16.578520, 16.808433, 17.044929, 17.288305, 17.538873,
		17.796967, 18.062943, 18.337176, 18.620068, 18.912049,
		19.213574, 19.525133, 19.847249, 20.180480, 20.525429,
		20.882738, 21.253102, 21.637266, 22.036036, 22.450278,
		22.880933, 23.329017, 23.795634, 24.281981, 24.789364,
		25.319207, 25.873062, 26.452634, 27.059789, 27.696581,
		28.365274, 29.068370, 29.808638, 30.589157, 31.413354,
		32.285060, 33.208568, 34.188705, 35.230920, 36.341388,
		37.527131, 38.796172, 40.157721, 41.622399, 43.202525,
		44.912465, 46.769077, 48.792279, 51.005773, 53.437996,
		56.123356, 59.103894
	};
	gnm_float X, U, V, RANLAN;
	int I, i;

	do {
		X = random_01 ();
	} while (X == 0.0);
	U = 1000.0 * X;
	i = U;
	U = U - i;

	/* Account for difference between C and Fortran convention for lower
	 * bound. */
	I = i - 1;

	if (I >= 70 && I <= 800)
		RANLAN = F[I] + U * (F[I + 1] - F[I]);
	else if (I >= 7 && I <= 980)
		RANLAN = F[I] + U * (F[I + 1] - F[I] -
				     0.25 * (1 - U) * (F[I + 2] - F[I + 1] -
						       F[I] + F[I - 1]));
	else if (I < 7) {
		V = loggnum (X);
		U = 1 / V;
		RANLAN = ((0.99858950 + (3.45213058E1 + 1.70854528E1 * U) * U) /
			  (1 + (3.41760202E1 + 4.01244582 * U) * U)) *
			( -loggnum ( -0.91893853 - V) - 1);
	} else {
		U = 1 - X;
		V = U * U;
		if (X <= 0.999)
			RANLAN = (1.00060006 + 2.63991156E2 *
				  U + 4.37320068E3 * V) /
			  ((1 + 2.57368075E2 * U + 3.41448018E3 * V) * U);
		else
			RANLAN = (1.00001538 + 6.07514119E3 * U +
				  7.34266409E5 * V) /
			  ((1 + 6.06511919E3 * U + 6.94021044E5 * V) * U);
	}

	return RANLAN;
}


/* ------------------------------------------------------------------------ */

/*
 * Generate 2^n being careful not to overflow
 */
gnm_float
gpow2 (int n)
{
#ifdef NEED_FAKE_LDEXPGNUM
	g_assert (FLT_RADIX == 2);

	/* gpow2 is used in our implementation of ldexpgnum.  */
	if (n >= DBL_MIN_EXP && n <= DBL_MAX_EXP)
		return (gnm_float) ldexp (1.0, n);
	else if (n >= GNUM_MIN_EXP && n <= GNUM_MAX_EXP) {
		gnm_float res = 1.0;
		gnm_float p = (n >= 0) ? GNM_const (2) : GNM_const (0.5);

		n = abs (n);
		while (n > 0) {
			if (n & 1) res *= p;
			p *= p;
			n >>= 1;
		}
		return res;
	} else
		return (n > 0) ? gnm_pinf : ML_UNDERFLOW;
#else
	return ldexpgnum (1.0, n);
#endif
}


/*
 * Generate 10^n being careful not to overflow
 */
gnm_float
gpow10 (int n)
{
	gnm_float res = 1.0;
	gnm_float p;
	const int maxn = GNUM_MAX_EXP;

	static const gnm_float fast[] = {
		GNM_const (1e-20),
		GNM_const (1e-19),
		GNM_const (1e-18),
		GNM_const (1e-17),
		GNM_const (1e-16),
		GNM_const (1e-15),
		GNM_const (1e-14),
		GNM_const (1e-13),
		GNM_const (1e-12),
		GNM_const (1e-11),
		GNM_const (1e-10),
		GNM_const (1e-9),
		GNM_const (1e-8),
		GNM_const (1e-7),
		GNM_const (1e-6),
		GNM_const (1e-5),
		GNM_const (1e-4),
		GNM_const (1e-3),
		GNM_const (1e-2),
		GNM_const (1e-1),
		GNM_const (1e0),
		GNM_const (1e1),
		GNM_const (1e2),
		GNM_const (1e3),
		GNM_const (1e4),
		GNM_const (1e5),
		GNM_const (1e6),
		GNM_const (1e7),
		GNM_const (1e8),
		GNM_const (1e9),
		GNM_const (1e10),
		GNM_const (1e11),
		GNM_const (1e12),
		GNM_const (1e13),
		GNM_const (1e14),
		GNM_const (1e15),
		GNM_const (1e16),
		GNM_const (1e17),
		GNM_const (1e18),
		GNM_const (1e19),
		GNM_const (1e20)
	};

	if (n >= -20 && n <= 20)
		return (fast + 20)[n];

	if (n >= 0) {
		p = 10.0;
		n = (n > maxn) ? maxn : n;
	} else {
		p = GNM_const (0.1);
		/* Note carefully that we avoid overflow.  */
		n = (n < -maxn) ? maxn : -n;
	}
	while (n > 0) {
		if (n & 1) res *= p;
		p *= p;
		n >>= 1;
	}
	return res;
}


/*
 * Euclid's Algorithm.	Assumes non-negative numbers.
 */
int
gcd (int a, int b)
{
	while (b != 0) {
		int r = a % b;
		a = b;
		b = r;
	}
	return a;
}


gnm_float
combin (int n, int k)
{
	if (n >= 15) {
		return floorgnum (0.5 + expgnum (lgammagnum (n + 1) - lgammagnum (k + 1) - lgammagnum (n - k + 1)));
	} else {
		return fact (n) / fact (k) / fact (n - k);
	}
}

gnm_float
permut (int n, int k)
{
	if (n >= 15) {
		return floorgnum (0.5 + expgnum (lgammagnum (n + 1) - lgammagnum (n - k + 1)));
	} else {
		return fact (n) / fact (n - k);
	}
}

gnm_float
fact (int n)
{
	static gnm_float table[100];
	static gboolean init = FALSE;

	if (n < 0)
		return gnm_nan;

	if (n < (int)G_N_ELEMENTS (table)) {
		if (!init) {
			int i;
			table[0] = 1;
			for (i = 1; i < (int)G_N_ELEMENTS (table); i++)
				table[i] = table[i - 1] * i;
			init = TRUE;
		}
		return table[n];
	} else
		return floorgnum (0.5 + expgnum (lgammagnum (n + 1)));
}

gnm_float
beta (gnm_float a, gnm_float b)
{
	int sign;
	gnm_float absres = expgnum (lbeta3 (a, b, &sign));

	return sign == -1 ? -absres : absres;
}

gnm_float
lbeta3 (gnm_float a, gnm_float b, int *sign)
{
	int sign_a, sign_b, sign_ab;
	gnm_float ab = a + b;
	gnm_float res_a, res_b, res_ab;

	*sign = 1;
	if (a > 0 && b > 0)
		return lbeta (a, b);

#ifdef IEEE_754
	if (isnangnum(ab))
		return ab;
#endif

	if ((a <= 0 && a == floorgnum (a)) ||
	    (b <= 0 && b == floorgnum (b)) ||
	    (ab <= 0 && ab == floorgnum (ab)))
		return gnm_nan;

	res_a = lgamma_rgnum (a, &sign_a);
	res_b = lgamma_rgnum (b, &sign_b);
	res_ab = lgamma_rgnum (ab, &sign_ab);

	*sign = sign_a * sign_b * sign_ab;
	return res_a + res_b - res_ab;
}


/* Calculate (1+x)^r accurately.  */
gnm_float
pow1p (gnm_float x, gnm_float y)
{
	if (gnumabs (x) > 0.5)
		return powgnum (1 + x, y);
	else
		return expgnum (y * log1pgnum (x));
}

/* Calculate ((1+x)^r)-1 accurately.  */
gnm_float
pow1pm1 (gnm_float x, gnm_float y)
{
	if (x <= -1)
		return powgnum (1 + x, y) - 1;
	else
		return expm1gnum (y * log1pgnum (x));
}


/*
 ---------------------------------------------------------------------
  Matrix functions
 ---------------------------------------------------------------------
 */

/* Calculates the product of two matrixes.
 */
void
mmult (gnm_float *A, gnm_float *B, int cols_a, int rows_a, int cols_b,
       gnm_float *product)
{
	gnm_float tmp;
	int	c, r, i;

	for (c = 0; c < cols_b; ++c) {
		for (r = 0; r < rows_a; ++r) {
			tmp = 0;
			for (i = 0; i < cols_a; ++i)
				tmp += A[r + i * rows_a] * B[i + c * cols_a];
			product[r + c * rows_a] = tmp;
		}
	}
}

void
continued_fraction (gnm_float val, int max_denom, int *res_num, int *res_denom)
{
	int n1, n2, d1, d2;
	gnm_float x, y;

	if (val < 0) {
		continued_fraction (gnumabs (val), max_denom, res_num, res_denom);
		*res_num = -*res_num;
		return;
	}

	n1 = 0; d1 = 1;
	n2 = 1; d2 = 0;

	x = val;
	y = 1;

	do {
		int a = (int) (x / y);
		gnm_float newy = x - a * y;
		int n3, d3;

		if ((n2 && a > (INT_MAX - n1) / n2) ||
		    (d2 && a > (INT_MAX - d1) / d2) ||
		    a * d2 + d1 > max_denom) {
			*res_num = n2;
			*res_denom = d2;
			return;
		}

		n3 = a * n2 + n1;
		d3 = a * d2 + d1;

		x = y;
		y = newy;

		n1 = n2; n2 = n3;
		d1 = d2; d2 = d3;
	} while (y > 1e-10);

	*res_num = n2;
	*res_denom = d2;
}


void
stern_brocot (float val, int max_denom, int *res_num, int *res_denom)
{
	int an = 0, ad = 1;
	int bn = 1, bd = 1;
	int n, d;
	float sp, delta;

	while ((d = ad + bd) <= max_denom) {
		sp = 1e-5 * d;/* Quick and dirty,  do adaptive later */
		n = an + bn;
		delta = val * d - n;
		if (delta > sp) {
			an = n;
			ad = d;
		} else if (delta < -sp) {
			bn = n;
			bd = d;
		} else {
			*res_num = n;
			*res_denom = d;
			return;
		}
	}
	if (bd > max_denom || gnumabs (val * ad - an) < gnumabs (val * bd - bn)) {
		*res_num = an;
		*res_denom = ad;
	} else {
		*res_num = bn;
		*res_denom = bd;
	}
}
