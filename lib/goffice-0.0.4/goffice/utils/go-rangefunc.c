/*
 * rangefunc.c: Functions on ranges (data sets).
 *
 * Authors:
 *   Morten Welinder <terra@gnome.org>
 *   Andreas J. Guelzow  <aguelzow@taliesin.ca>
 */

#include <goffice/goffice-config.h>
#include "go-rangefunc.h"

#include <math.h>
#include <stdlib.h>
#include <string.h>

#ifndef DOUBLE

#define DOUBLE double
#define SUFFIX(_n) _n

#ifdef GOFFICE_WITH_LONG_DOUBLE
#include "go-rangefunc.c"
#undef DOUBLE
#undef SUFFIX

#ifdef HAVE_SUNMATH_H
#include <sunmath.h>
#endif
#define DOUBLE long double
#define SUFFIX(_n) _n ## l
#endif

#endif

/* Arithmetic sum.  */
int
SUFFIX(go_range_sum) (const DOUBLE *xs, int n, DOUBLE *res)
{
	/* http://bugzilla.gnome.org/show_bug.cgi?id=131588 */
#ifdef HAVE_LONG_DOUBLE
	long double sum = 0;
#else
	DOUBLE sum = 0;
#endif
	int i;

	for (i = 0; i < n; i++)
		sum += xs[i];

	*res = sum;
	return 0;
}

/* Arithmetic sum of squares.  */
int
SUFFIX(go_range_sumsq) (const DOUBLE *xs, int n, DOUBLE *res)
{
	/* http://bugzilla.gnome.org/show_bug.cgi?id=131588 */
#ifdef HAVE_LONG_DOUBLE
	long double sum = 0;
#else
	DOUBLE sum = 0;
#endif
	int i;

	for (i = 0; i < n; i++)
		sum += xs[i] * xs[i];

	*res = sum;
	return 0;
}

/* Arithmetic average.  */
int
SUFFIX(go_range_average) (const DOUBLE *xs, int n, DOUBLE *res)
{
	if (n <= 0 || SUFFIX(go_range_sum) (xs, n, res))
		return 1;

	*res /= n;
	return 0;
}

/* Minimum element.  */
int
SUFFIX(go_range_min) (const DOUBLE *xs, int n, DOUBLE *res)
{
	if (n > 0) {
		DOUBLE min = xs[0];
		int i;

		for (i = 1; i < n; i++)
			if (xs[i] < min)
				min = xs[i];
		*res = min;
		return 0;
	} else
		return 1;
}

/* Maximum element.  */
int
SUFFIX(go_range_max) (const DOUBLE *xs, int n, DOUBLE *res)
{
	if (n > 0) {
		DOUBLE max = xs[0];
		int i;

		for (i = 1; i < n; i++)
			if (xs[i] > max)
				max = xs[i];
		*res = max;
		return 0;
	} else
		return 1;
}

/* Maximum absolute element.  */
int
SUFFIX(go_range_maxabs) (const DOUBLE *xs, int n, DOUBLE *res)
{
	if (n > 0) {
		DOUBLE max = SUFFIX(fabs) (xs[0]);
		int i;

		for (i = 1; i < n; i++)
			if (SUFFIX(fabs) (xs[i]) > max)
				max = SUFFIX(fabs) (xs[i]);
		*res = max;
		return 0;
	} else
		return 1;
}

/* Sum of square deviations from mean.  */
int
SUFFIX(go_range_devsq) (const DOUBLE *xs, int n, DOUBLE *res)
{
	DOUBLE m, dx, q = 0;
	if (n > 0) {
		int i;

		SUFFIX(go_range_average) (xs, n, &m);
		for (i = 0; i < n; i++) {
			dx = xs[i] - m;
			q += dx * dx;
		}
	}
	*res = q;
	return 0;
}
