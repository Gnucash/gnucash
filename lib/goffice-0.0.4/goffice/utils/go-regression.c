/*
 * regression.c:  Statistical regression functions.
 *
 * Authors:
 *   Morten Welinder <terra@gnome.org>
 *   Andrew Chatham <andrew.chatham@duke.edu>
 *   Daniel Carrera <dcarrera@math.toronto.edu>
 */

#include <goffice/goffice-config.h>
#include "go-regression.h"
#include "go-rangefunc.h"
#include "go-math.h"

#include <glib/gmem.h>
#include <glib/gmessages.h>

#include <math.h>
#include <string.h>
#include <stdlib.h>
#include <stdio.h>

#ifndef DOUBLE

#define DEFINE_COMMON
#define DOUBLE double
#define DOUBLE_MAX DBL_MAX
#define DOUBLE_MANT_DIG DBL_MANT_DIG
#define SUFFIX(_n) _n
#define FORMAT_g "g"

#define ALLOC_MATRIX(var,dim1,dim2)			\
  do { int _i, _d1, _d2;				\
       _d1 = (dim1);					\
       _d2 = (dim2);					\
       (var) = g_new (double *, _d1);		\
       for (_i = 0; _i < _d1; _i++)			\
	       (var)[_i] = g_new (double, _d2);	\
  } while (0)

#ifdef GOFFICE_WITH_LONG_DOUBLE
#include "go-regression.c"
#undef DEFINE_COMMON
#undef DOUBLE
#undef DOUBLE_MAX
#undef DOUBLE_MANT_DIG
#undef SUFFIX
#undef FORMAT_g
#undef ALLOC_MATRIX
#ifdef HAVE_SUNMATH_H
#include <sunmath.h>
#endif
#define DOUBLE long double
#define DOUBLE_MAX LDBL_MAX
#define DOUBLE_MANT_DIG LDBL_MANT_DIG
#define SUFFIX(_n) _n ## l
#define FORMAT_g "Lg"

#define ALLOC_MATRIX(var,dim1,dim2)			\
  do { int _i, _d1, _d2;				\
       _d1 = (dim1);					\
       _d2 = (dim2);					\
       (var) = g_new (long double *, _d1);		\
       for (_i = 0; _i < _d1; _i++)			\
	       (var)[_i] = g_new (long double, _d2);	\
  } while (0)
#endif

#endif

#ifdef DEFINE_COMMON

#undef DEBUG_NEAR_SINGULAR

#define FREE_MATRIX(var,dim1,dim2)			\
  do { int _i, _d1;					\
       _d1 = (dim1);					\
       for (_i = 0; _i < _d1; _i++)			\
	       g_free ((var)[_i]);			\
       g_free (var);					\
  } while (0)

#define COPY_MATRIX(dst,src,dim1,dim2)		\
  do { int _i, _j, _d1, _d2;			\
       _d1 = (dim1);				\
       _d2 = (dim2);				\
       for (_i = 0; _i < _d1; _i++)		\
	 for (_j = 0; _j < _d2; _j++)		\
	   (dst)[_i][_j] = (src)[_i][_j];	\
  } while (0)

#endif

#undef PRINT_MATRIX
#define PRINT_MATRIX(var,dim1,dim2)					\
  do {									\
	int _i, _j, _d1, _d2;						\
	_d1 = (dim1);							\
	_d2 = (dim2);							\
	for (_i = 0; _i < _d1; _i++)					\
	  {								\
	    for (_j = 0; _j < _d2; _j++)				\
	      fprintf (stderr, " %19.10" FORMAT_g, (var)[_i][_j]);	\
	    fprintf (stderr, "\n");					\
	  }								\
  } while (0)

/*
 *       ---> j
 *
 *  |    ********
 *  |    ********
 *  |    ********        A[i][j]
 *  v    ********
 *       ********
 *  i    ********
 *       ********
 *       ********
 *
 */

/* ------------------------------------------------------------------------- */

/* Returns in res the solution to the equation L * U * res = P * b.

   This function is adapted from pseudocode in
	Introduction to Algorithms_. Cormen, Leiserson, and Rivest.
	p. 753. MIT Press, 1990.
*/
static void
SUFFIX(backsolve) (DOUBLE **LU, int *P, DOUBLE *b, int n, DOUBLE *res)
{
	int i, j;

	for (i = 0; i < n; i++) {
		res[i] = b[P[i]];
		for (j = 0; j < i; j++)
			res[i] -= LU[i][j] * res[j];
	}

	for (i = n - 1; i >= 0; i--) {
		for (j = i + 1; j < n; j++)
			res[i] -= LU[i][j] * res[j];
		res[i] /= LU[i][i];
	}
}

static RegressionResult
SUFFIX(rescale) (DOUBLE **A, DOUBLE *b, int n, DOUBLE *pdet)
{
	int i;

	*pdet = 1;
	for (i = 0; i < n; i++) {
		int j, expn;
		DOUBLE scale, max;

		(void)SUFFIX(go_range_maxabs) (A[i], n, &max);

		if (max == 0)
			return REG_singular;

		/* Use a power of 2 near sqrt (max) as scale.  */
		(void)SUFFIX(frexp) (SUFFIX(sqrt) (max), &expn);
		scale = SUFFIX(ldexp) (1, expn);
#ifdef DEBUG_NEAR_SINGULAR
		printf ("scale[%d]=%" FORMAT_g "\n",
			i, scale);
#endif

		*pdet *= scale;
		b[i] /= scale;
		for (j = 0; j < n; j++)
			A[i][j] /= scale;
	}
	return REG_ok;
}


/*
 * Performs an LUP Decomposition; LU and P must already be allocated.
 * A is not destroyed.
 *
 * This function is adapted from pseudocode in
 *   _Introduction to Algorithms_. Cormen, Leiserson, and Rivest.
 *   p 759. MIT Press, 1990.
 *
 * A rescaling of rows is done and the b_scaled vector is scaled
 * accordingly.
 */
static RegressionResult
SUFFIX(LUPDecomp) (DOUBLE **A, DOUBLE **LU, int *P, int n, DOUBLE *b_scaled,
	   DOUBLE *pdet)
{
	int i, j, k, tempint;
	DOUBLE highest = 0;
	DOUBLE lowest = DOUBLE_MAX;
	DOUBLE cond;
	gboolean odd_parity = FALSE;
	DOUBLE det = 1;

	COPY_MATRIX (LU, A, n, n);
	for (j = 0; j < n; j++)
		P[j] = j;


	*pdet = 0;

#ifdef DEBUG_NEAR_SINGULAR
	PRINT_MATRIX (LU, n, n);
#endif
	{
		RegressionResult err = SUFFIX(rescale) (LU, b_scaled, n, &det);
		if (err != REG_ok)
			return err;
	}

	for (i = 0; i < n; i++) {
		DOUBLE max = 0;
		int mov = -1;

		for (j = i; j < n; j++)
			if (SUFFIX(fabs) (LU[j][i]) > max) {
				max = SUFFIX(fabs) (LU[j][i]);
				mov = j;
			}
#ifdef DEBUG_NEAR_SINGULAR
		PRINT_MATRIX (LU, n, n);
		printf ("max[%d]=%" FORMAT_g " at %d\n",
			i, max, mov);
#endif
		if (max == 0)
			return REG_singular;
		if (max > highest)
			highest = max;
		if (max < lowest)
			lowest = max;
		if (i != mov) {
			/*swap the two rows */

			odd_parity = !odd_parity;
			tempint = P[i];
			P[i] = P[mov];
			P[mov] = tempint;
			for (j = 0; j < n; j++) {
				DOUBLE temp = LU[i][j];
				LU[i][j] = LU[mov][j];
				LU[mov][j] = temp;
			}
		}

		for (j = i + 1; j < n; j++) {
			LU[j][i] /= LU[i][i];
			for (k = i + 1; k < n; k++)
				LU[j][k] -= LU[j][i] * LU[i][k];
		}
	}

	/* Calculate the determinant.  */
	if (odd_parity) det = -det;
	for (i = 0; i < n; i++)
		det *= LU[i][i];
	*pdet = det;

	cond = (SUFFIX(log) (highest) - SUFFIX(log) (lowest)) / SUFFIX(log) (2);
#ifdef DEBUG_NEAR_SINGULAR
	printf ("cond=%.20" FORMAT_g "\n", cond);
#endif

	/* FIXME: make some science out of this.  */
	if (cond > DOUBLE_MANT_DIG * 0.75)
		return REG_near_singular_bad;
	else if (cond > DOUBLE_MANT_DIG * 0.50)
		return REG_near_singular_good;
	else
		return REG_ok;
}


static RegressionResult
SUFFIX(linear_solve) (DOUBLE **A, DOUBLE *b, int n, DOUBLE *res)
{
	RegressionResult err;
	DOUBLE **LU, *b_scaled;
	int *P;
	DOUBLE det;

	if (n < 1)
		return REG_not_enough_data;

	/* Special case.  */
	if (n == 1) {
		DOUBLE d = A[0][0];
		if (d == 0)
			return REG_singular;

		res[0] = b[0] / d;
		return REG_ok;
	}

	/* Special case.  */
	if (n == 2) {
		DOUBLE d = SUFFIX(go_matrix_determinant) (A, n);
		if (d == 0)
			return REG_singular;

		res[0] = (A[1][1] * b[0] - A[1][0] * b[1]) / d;
		res[1] = (A[0][0] * b[1] - A[0][1] * b[0]) / d;
		return REG_ok;
	}

	/*
	 * Otherwise, use LUP-decomposition to find res such that
	 * A res = b
	 */
	ALLOC_MATRIX (LU, n, n);
	P = g_new (int, n);

	b_scaled = g_new (DOUBLE, n);
	memcpy (b_scaled, b, n * sizeof (DOUBLE));

	err = SUFFIX(LUPDecomp) (A, LU, P, n, b_scaled, &det);

	if (err == REG_ok || err == REG_near_singular_good)
		SUFFIX(backsolve) (LU, P, b_scaled, n, res);

	FREE_MATRIX (LU, n, n);
	g_free (P);
	g_free (b_scaled);
	return err;
}


gboolean
SUFFIX(go_matrix_invert) (DOUBLE **A, int n)
{
	RegressionResult err;
	DOUBLE **LU, *b_scaled, det;
	int *P;
	int i;
	gboolean res;

	if (n < 1)
		return FALSE;

	/*
	 * Otherwise, use LUP-decomposition to find res such that
	 * A res = b
	 */
	ALLOC_MATRIX (LU, n, n);
	P = g_new (int, n);

	b_scaled = g_new (DOUBLE, n);
	for (i = 0; i < n; i++)
		b_scaled[i] = 1;

	err = SUFFIX(LUPDecomp) (A, LU, P, n, b_scaled, &det);

	if (err == REG_ok || err == REG_near_singular_good) {
		int i, j;
		DOUBLE *b = g_new (DOUBLE, n);
		DOUBLE *w = g_new (DOUBLE, n);

		for (i = 0; i < n; i++) {
			memset (b, 0, sizeof (DOUBLE) * n);
			b[i] = b_scaled[i];
			SUFFIX(backsolve) (LU, P, b, n, w);
			for (j = 0; j < n; j++)
				A[j][i] = w[j];
		}
		g_free (w);
		g_free (b);
		res = TRUE;
	} else
		res = FALSE;

	FREE_MATRIX (LU, n, n);
	g_free (P);
	g_free (b_scaled);

	return res;
}

DOUBLE
SUFFIX(go_matrix_determinant) (DOUBLE **A, int n)
{
	RegressionResult err;
	DOUBLE **LU, *b_scaled, det;
	int *P;

	if (n < 1)
		return 0;

	/* Special case.  */
	if (n == 1)
		return A[0][0];

	/* Special case.  */
	if (n == 2)
		return A[0][0] * A[1][1] - A[1][0] * A[0][1];

	/*
	 * Otherwise, use LUP-decomposition to find res such that
	 * A res = b
	 */
	ALLOC_MATRIX (LU, n, n);
	P = g_new (int, n);
	b_scaled = g_new0 (DOUBLE, n);

	err = SUFFIX(LUPDecomp) (A, LU, P, n, b_scaled, &det);

	FREE_MATRIX (LU, n, n);
	g_free (P);
	g_free (b_scaled);

	return det;
}

/* ------------------------------------------------------------------------- */

static RegressionResult
SUFFIX(general_linear_regression) (DOUBLE **xss, int xdim,
			   const DOUBLE *ys, int n,
			   DOUBLE *result,
			   SUFFIX(regression_stat_t) *regression_stat, gboolean affine)
{
	DOUBLE *xTy, **xTx;
	int i,j;
	RegressionResult regerr;

	if (regression_stat)
		memset (regression_stat, 0, sizeof (SUFFIX(regression_stat_t)));

	if (xdim > n)
		return REG_not_enough_data;

	xTy = g_new (DOUBLE, xdim);
	for (i = 0; i < xdim; i++) {
		const DOUBLE *xs = xss[i];
		register DOUBLE res = 0;
		int j;
		if (xs == NULL)
			/* NULL represents a 1-vector.  */
			for (j = 0; j < n; j++)
				res += ys[j];
		else
			for (j = 0; j < n; j++)
				res += xs[j] * ys[j];
		xTy[i] = res;
	}

	ALLOC_MATRIX (xTx, xdim, xdim);

	for (i = 0; i < xdim; i++) {
		const DOUBLE *xs1 = xss[i];
		int j;
		for (j = 0; j <= i; j++) {
			const DOUBLE *xs2 = xss[j];
			DOUBLE res = 0;
			int k;

			if (xs1 == NULL && xs2 == NULL)
				res = n;
			else if (xs1 == NULL)
				for (k = 0; k < n; k++)
					res += xs2[k];
			else if (xs2 == NULL)
				for (k = 0; k < n; k++)
					res += xs1[k];
			else
				for (k = 0; k < n; k++)
					res += xs1[k] * xs2[k];

			xTx[i][j] = xTx[j][i] = res;
		}
	}

	regerr = SUFFIX(linear_solve) (xTx, xTy, xdim, result);

	if (regression_stat &&
	    (regerr == REG_ok || regerr == REG_near_singular_good)) {
		RegressionResult err2;
		DOUBLE *residuals = g_new (DOUBLE, n);
		DOUBLE **LU, *one_scaled, det;
		int *P;
		int err;

		/* This should not fail since n >= 1.  */
		err = SUFFIX(go_range_average) (ys, n, &regression_stat->ybar);
		g_assert (err == 0);

		/* FIXME: we ought to have a devsq variant that does not
		   recompute the mean.  */
		if (affine)
			err = SUFFIX(go_range_devsq) (ys, n, &regression_stat->ss_total);
		else
			err = SUFFIX(go_range_sumsq) (ys, n, &regression_stat->ss_total);
		g_assert (err == 0);

		regression_stat->xbar = g_new (DOUBLE, n);
		for (i = 0; i < xdim; i++) {
			if (xss[i]) {
				int err = SUFFIX(go_range_average) (xss[i], n, &regression_stat->xbar[i]);
				g_assert (err == 0);
			} else {
				regression_stat->xbar[i] = 1;
			}
		}

		for (i = 0; i < n; i++) {
			residuals[i] = 0;
			for (j = 0; j < xdim; j++) {
				if (xss[j])
					residuals[i] += xss[j][i] * result[j];
				else
					residuals[i] += result[j]; /* If NULL, constant factor */
			}
			residuals[i] = ys[i] - residuals[i];
		}

		err = SUFFIX(go_range_sumsq) (residuals, n, &regression_stat->ss_resid);
		g_assert (err == 0);

		regression_stat->sqr_r = (regression_stat->ss_total == 0)
			? 1
			: 1 - regression_stat->ss_resid / regression_stat->ss_total;
		/* FIXME: we want to guard against division by zero.  */
		regression_stat->adj_sqr_r = 1 - regression_stat->ss_resid * (n - 1) /
			((n - xdim) * regression_stat->ss_total);
		regression_stat->var = (n == xdim)
			? 0
			: regression_stat->ss_resid / (n - xdim);

		ALLOC_MATRIX (LU, xdim, xdim);
		one_scaled = g_new (DOUBLE, xdim);
		for (i = 0; i < xdim; i++) one_scaled[i] = 1;
		P = g_new (int, xdim);

		err2 = SUFFIX(LUPDecomp) (xTx, LU, P, xdim, one_scaled, &det);
		regression_stat->se = g_new (DOUBLE, xdim);
		if (err2 == REG_ok || err2 == REG_near_singular_good) {
			DOUBLE *e = g_new (DOUBLE, xdim); /* Elementary vector */
			DOUBLE *inv = g_new (DOUBLE, xdim);
			for (i = 0; i < xdim; i++)
				e[i] = 0;
			for (i = 0; i < xdim; i++) {
				e[i] = one_scaled[i];
				SUFFIX(backsolve) (LU, P, e, xdim, inv);

				if (inv[i] < 0) {
					/*
					 * If this happens, something is really
					 * wrong, numerically.
					 */
					regerr = REG_near_singular_bad;
				}
				regression_stat->se[i] =
					SUFFIX(sqrt) (regression_stat->var * inv[i]);
				e[i] = 0;
			}
			g_free (e);
			g_free (inv);
		} else {
			/*
			 * This can happen for xdim == 2 as linear_solve does
			 * not use LUPDecomp in that case.
			 */
			regerr = err2;
			for (i = 0; i < xdim; i++)
				regression_stat->se[i] = 0;
		}
		FREE_MATRIX (LU, xdim, xdim);
		g_free (P);
		g_free (one_scaled);

		regression_stat->t = g_new (DOUBLE, xdim);

		for (i = 0; i < xdim; i++)
			regression_stat->t[i] = (regression_stat->se[i] == 0)
				? SUFFIX(go_pinf)
				: result[i] / regression_stat->se[i];

		regression_stat->df_resid = n - xdim;
		regression_stat->df_reg = xdim - (affine ? 1 : 0);
		regression_stat->df_total = regression_stat->df_resid + regression_stat->df_reg;

		regression_stat->F = (regression_stat->sqr_r == 1)
			? SUFFIX(go_pinf)
			: ((regression_stat->sqr_r / regression_stat->df_reg) /
			   (1 - regression_stat->sqr_r) * regression_stat->df_resid);

		regression_stat->ss_reg =  regression_stat->ss_total - regression_stat->ss_resid;
		regression_stat->se_y = SUFFIX(sqrt) (regression_stat->ss_total / n);
		regression_stat->ms_reg = (regression_stat->df_reg == 0)
			? 0
			: regression_stat->ss_reg / regression_stat->df_reg;
		regression_stat->ms_resid = (regression_stat->df_resid == 0)
			? 0
			: regression_stat->ss_resid / regression_stat->df_resid;

		g_free (residuals);
	}

	FREE_MATRIX (xTx, xdim, xdim);
	g_free (xTy);

	return regerr;
}

/* ------------------------------------------------------------------------- */

typedef struct {
  DOUBLE min_x;
  DOUBLE max_x;
  DOUBLE min_y;
  DOUBLE max_y;
  DOUBLE mean_y;
} SUFFIX(point_cloud_measure_type);

/* Takes the current 'sign' (res[0]) and 'c' (res[3]) from the calling
 * function, transforms xs to ln(sign*(x-c)), performs a simple
 * linear regression to find the best fitting 'a' (res[1]) and 'b'
 * (res[2]) for ys and transformed xs, and computes the sum of squared
 * residuals.
 * Needs 'sign' (i.e. +1 or -1) and 'c' so adjusted that (sign*(x-c)) is
 * positive for all xs. n must be > 0. These conditions are trusted to be
 * checked by the calling functions.
 * Is called often, so do not make it too slow.
 */

static int
SUFFIX(transform_x_and_linear_regression_log_fitting) (DOUBLE *xs,
					       DOUBLE *transf_xs,
					       const DOUBLE *ys, int n,
					       DOUBLE *res,
					       SUFFIX(point_cloud_measure_type)
					       *point_cloud)
{
        int i;
	int result = REG_ok;
	DOUBLE mean_transf_x, diff_x, resid_y;
	DOUBLE sum1 = 0;
	DOUBLE sum2 = 0;

	/* log (always > 0) */
	for (i=0; i<n; i++)
	        transf_xs[i] = SUFFIX(log) (res[0] * (xs[i] - res[3]));
	SUFFIX(go_range_average) (transf_xs, n, &mean_transf_x);
	for (i=0; i<n; i++) {
	        diff_x = transf_xs[i] - mean_transf_x;
		sum1 += diff_x * (ys[i] - point_cloud->mean_y);
		sum2 += diff_x * diff_x;
	}
	res[2] = sum1 / sum2;
	res[1] = point_cloud->mean_y - (res[2] * mean_transf_x);
	res[4] = 0;
	for (i=0; i<n; i++) {
	        resid_y = res[1] + (res[2] * transf_xs[i]) - ys[i];
		res[4] += resid_y * resid_y;
	}
	return result; /* not used for error checking for the sake of speed */
}

static int
SUFFIX(log_fitting) (DOUBLE *xs, const DOUBLE *ys, int n,
	     DOUBLE *res, SUFFIX(point_cloud_measure_type) *point_cloud)
{
        int result = REG_ok;
	gboolean sign_plus_ok = 1, sign_minus_ok = 1;
	DOUBLE x_range, c_step, c_accuracy_int, c_offset, c_accuracy;
	DOUBLE c_range, c_start, c_end, c_dist;
	DOUBLE *temp_res;
        DOUBLE *transf_xs;

	temp_res = g_new (DOUBLE, 5);
	x_range = (point_cloud->max_x) - (point_cloud->min_x);
	/* Not needed here, but allocate it once for all subfunction calls */
	transf_xs = g_new (DOUBLE, n);
	/* Choose final accuracy of c with respect to range of xs.
	 * Make accuracy be a whole power of 10. */
	c_accuracy = SUFFIX(log10) (x_range);
	if (c_accuracy < 0)
	        if (SUFFIX(modf) (c_accuracy, &c_accuracy_int) != 0)
		        c_accuracy--;
	SUFFIX(modf) (c_accuracy, &c_accuracy_int);
	c_accuracy = c_accuracy_int;
	c_accuracy = SUFFIX(pow) (10, c_accuracy);
	c_accuracy *= LOGFIT_C_ACCURACY;

	/* Determine sign. Take a c which is ``much to small'' since the part
	 * of the curve cutting the point cloud is almost not bent.
	 * If making c still smaller does not make things still worse,
	 * assume that we have to change the direction of curve bending
	 * by changing sign.
	 */
	c_step = x_range * LOGFIT_C_STEP_FACTOR;
	c_range = x_range * LOGFIT_C_RANGE_FACTOR;
	res[0] = 1; /* sign */
	res[3] = point_cloud->min_x - c_range;
	temp_res[0] = 1;
	temp_res[3] = res[3] - c_step;
	SUFFIX(transform_x_and_linear_regression_log_fitting) (xs, transf_xs, ys, n,
						       res, point_cloud);
	SUFFIX(transform_x_and_linear_regression_log_fitting) (xs, transf_xs, ys, n,
						       temp_res, point_cloud);
	if (temp_res[4] <= res[4])
	        sign_plus_ok = 0;
        /* check again with new sign */
	res[0] = -1; /* sign */
	res[3] = point_cloud->max_x + c_range;
	temp_res[0] = -1;
	temp_res[3] = res[3] + c_step;
	SUFFIX(transform_x_and_linear_regression_log_fitting) (xs, transf_xs, ys, n,
						       res, point_cloud);
	SUFFIX(transform_x_and_linear_regression_log_fitting) (xs, transf_xs, ys, n,
						       temp_res, point_cloud);
	if (temp_res[4] <= res[4])
	        sign_minus_ok = 0;
	/* If not exactly one of plus or minus works, give up.
	 * This happens in point clouds which are very weakly bent.
	 */
	if (sign_plus_ok && !sign_minus_ok)
	        res[0] = 1;
	else if (sign_minus_ok && !sign_plus_ok)
	        res[0] = -1;
	else {
	        result = REG_invalid_data;
		goto out;
	}

	/* Start of fitted c-range. Rounded to final accuracy of c. */
	c_offset = (res[0] == 1) ? point_cloud->min_x : point_cloud->max_x;
	c_offset = c_accuracy * ((res[0] == 1) ?
				 SUFFIX(floor) (c_offset / c_accuracy)
				 : SUFFIX(ceil) (c_offset /c_accuracy));

	/* Now the adapting of c starts. Find a local minimum of sum
	 * of squared residuals. */

	/* First, catch some unsuitably shaped point clouds. */
	res[3] = c_offset - res[0] * c_accuracy;
	temp_res[3] = c_offset - res[0] * 2 * c_accuracy;
	temp_res[0] = res[0];
	SUFFIX(transform_x_and_linear_regression_log_fitting) (xs, transf_xs, ys, n,
						       res, point_cloud);
	SUFFIX(transform_x_and_linear_regression_log_fitting) (xs, transf_xs, ys, n,
						       temp_res, point_cloud);
	if (temp_res[4] >= res[4]) {
	        result = REG_invalid_data;
		goto out;
	}
	/* After the above check, any minimum reached will be NOT  at
	 * the start of c-range (c_offset - sign * c_accuracy) */
	c_start = c_offset;
	c_end = c_start - res[0] * c_range;
	c_dist = res[0] * (c_start - c_end) / 2;
	res[3] = c_end + res[0] * c_dist;
	do {
	    c_dist /= 2;
		SUFFIX(transform_x_and_linear_regression_log_fitting) (xs, transf_xs,
							       ys, n, res,
							       point_cloud);
		temp_res[3] = res[3] + res[0] * c_dist;
		SUFFIX(transform_x_and_linear_regression_log_fitting) (xs, transf_xs,
							       ys, n, temp_res,
							       point_cloud);
		if (temp_res[4] <= res[4])
		        memcpy (res, temp_res, 5 * sizeof (DOUBLE));
		else {
		        temp_res[3] = res[3] - res[0] * c_dist;
			SUFFIX(transform_x_and_linear_regression_log_fitting) (xs,
							          transf_xs,
								  ys, n,
								  temp_res,
							          point_cloud);
			if (temp_res[4] <= res[4])
			        memcpy (res, temp_res, 5*sizeof (DOUBLE));
		}
	} while (c_dist > c_accuracy);

	res[3] = c_accuracy * SUFFIX(go_fake_round) (res[3] / c_accuracy);
	SUFFIX(transform_x_and_linear_regression_log_fitting) (xs, transf_xs, ys, n,
						       res, point_cloud);

	if ((res[0] * (res[3] - c_end)) < (1.1 * c_accuracy)) {
	        /* Allowing for some inaccuracy, we are at the end of the
		 * range, so this is probably no local minimum.
		 * The start of the range has been checked above. */
	        result = REG_invalid_data;
		goto out;
	}

 out:
	g_free (transf_xs);
	g_free (temp_res);
	return result;
}

/* ------------------------------------------------------------------------- */
/* Please refer to description in regression.h.  */

RegressionResult
SUFFIX(go_linear_regression) (DOUBLE **xss, int dim,
		   const DOUBLE *ys, int n,
		   gboolean affine,
		   DOUBLE *res,
		   SUFFIX(regression_stat_t) *regression_stat)
{
	RegressionResult result;

	g_return_val_if_fail (dim >= 1, REG_invalid_dimensions);
	g_return_val_if_fail (n >= 1, REG_invalid_dimensions);

	if (affine) {
		DOUBLE **xss2;
		xss2 = g_new (DOUBLE *, dim + 1);
		xss2[0] = NULL;  /* Substitute for 1-vector.  */
		memcpy (xss2 + 1, xss, dim * sizeof (DOUBLE *));

		result = SUFFIX(general_linear_regression) (xss2, dim + 1, ys, n,
						    res, regression_stat, affine);
		g_free (xss2);
	} else {
		res[0] = 0;
		result = SUFFIX(general_linear_regression) (xss, dim, ys, n,
						    res + 1, regression_stat, affine);
	}
	return result;
}

/* ------------------------------------------------------------------------- */
/* Please refer to description in regression.h.  */

RegressionResult
SUFFIX(go_exponential_regression) (DOUBLE **xss, int dim,
			const DOUBLE *ys, int n,
			gboolean affine,
			DOUBLE *res,
			SUFFIX(regression_stat_t) *regression_stat)
{
	DOUBLE *log_ys;
	RegressionResult result;
	int i;

	g_return_val_if_fail (dim >= 1, REG_invalid_dimensions);
	g_return_val_if_fail (n >= 1, REG_invalid_dimensions);

	log_ys = g_new (DOUBLE, n);
	for (i = 0; i < n; i++)
		if (ys[i] > 0)
			log_ys[i] = SUFFIX(log) (ys[i]);
		else {
			result = REG_invalid_data;
			goto out;
		}

	if (affine) {
		DOUBLE **xss2;
		xss2 = g_new (DOUBLE *, dim + 1);
		xss2[0] = NULL;  /* Substitute for 1-vector.  */
		memcpy (xss2 + 1, xss, dim * sizeof (DOUBLE *));

		result = SUFFIX(general_linear_regression) (xss2, dim + 1, log_ys,
						    n, res, regression_stat, affine);
		g_free (xss2);
	} else {
		res[0] = 0;
		result = SUFFIX(general_linear_regression) (xss, dim, log_ys, n,
						    res + 1, regression_stat, affine);
	}

	if (result == 0)
		for (i = 0; i < dim + 1; i++)
			res[i] = SUFFIX(exp) (res[i]);

 out:
	g_free (log_ys);
	return result;
}

/* ------------------------------------------------------------------------- */
/* Please refer to description in regression.h.  */

RegressionResult
SUFFIX(go_logarithmic_regression) (DOUBLE **xss, int dim,
			const DOUBLE *ys, int n,
			gboolean affine,
			DOUBLE *res,
			SUFFIX(regression_stat_t) *regression_stat)
{
        DOUBLE **log_xss;
	RegressionResult result;
	int i, j;

	g_return_val_if_fail (dim >= 1, REG_invalid_dimensions);
	g_return_val_if_fail (n >= 1, REG_invalid_dimensions);

	ALLOC_MATRIX (log_xss, dim, n);
	for (i = 0; i < dim; i++)
	        for (j = 0; j < n; j++)
		        if (xss[i][j] > 0)
		                log_xss[i][j] = SUFFIX(log) (xss[i][j]);
			else {
			        result = REG_invalid_data;
				goto out;
			}


	if (affine) {
		DOUBLE **log_xss2;
		log_xss2 = g_new (DOUBLE *, dim + 1);
		log_xss2[0] = NULL;  /* Substitute for 1-vector.  */
		memcpy (log_xss2 + 1, log_xss, dim * sizeof (DOUBLE *));

		result = SUFFIX(general_linear_regression) (log_xss2, dim + 1, ys, n,
						    res, regression_stat,
						    affine);
		g_free (log_xss2);
	} else {
		res[0] = 0;
		result = SUFFIX(general_linear_regression) (log_xss, dim, ys, n,
						    res + 1, regression_stat,
						    affine);
	}

 out:
	FREE_MATRIX (log_xss, dim, n);
	return result;
}

/* ------------------------------------------------------------------------- */
/* Please refer to description in regression.h.  */

RegressionResult
SUFFIX(go_logarithmic_fit) (DOUBLE *xs, const DOUBLE *ys, int n, DOUBLE *res)
{
    SUFFIX(point_cloud_measure_type) point_cloud_measures;
	int i, result;
	gboolean more_2_y = 0, more_2_x = 0;

	/* Store useful measures for using them here and in subfunctions.
	 * The checking of n is paranoid -- the calling function should
	 * have cared for that. */
	g_return_val_if_fail (n > 2, REG_invalid_dimensions);
	result = SUFFIX(go_range_min) (xs, n, &(point_cloud_measures.min_x));
	result = SUFFIX(go_range_max) (xs, n, &(point_cloud_measures.max_x));
	result = SUFFIX(go_range_min) (ys, n, &(point_cloud_measures.min_y));
	result = SUFFIX(go_range_max) (ys, n, &(point_cloud_measures.max_y));
	result = SUFFIX(go_range_average) (ys, n, &(point_cloud_measures.mean_y));
	/* Checking of error conditions. */
	/* less than 2 different ys or less than 2 different xs */
	g_return_val_if_fail (((point_cloud_measures.min_y !=
				point_cloud_measures.max_y) &&
			       (point_cloud_measures.min_x !=
				point_cloud_measures.max_x)),
			      REG_invalid_data);
	/* less than 3 different ys */
	for (i=0; i<n; i++) {
	        if ((ys[i] != point_cloud_measures.min_y) &&
		    (ys[i] != point_cloud_measures.max_y)) {
		        more_2_y = 1;
			break;
		}
	}
	g_return_val_if_fail (more_2_y, REG_invalid_data);
	/* less than 3 different xs */
	for (i=0; i<n; i++) {
	        if ((xs[i] != point_cloud_measures.min_x) &&
		    (xs[i] != point_cloud_measures.max_x)) {
		        more_2_x = 1;
			break;
		}
	}
	g_return_val_if_fail (more_2_x, REG_invalid_data);

	/* no errors */
	result = SUFFIX(log_fitting) (xs, ys, n, res, &point_cloud_measures);
	return result;
}

/* ------------------------------------------------------------------------- */

SUFFIX(regression_stat_t) *
SUFFIX(go_regression_stat_new) (void)
{
	SUFFIX(regression_stat_t) * regression_stat = g_new0 (SUFFIX(regression_stat_t), 1);

	regression_stat->se = NULL;
	regression_stat->t = NULL;
	regression_stat->xbar = NULL;

	return regression_stat;
}

/* ------------------------------------------------------------------------- */

void
SUFFIX(go_regression_stat_destroy) (SUFFIX(regression_stat_t) *regression_stat)
{
	g_return_if_fail (regression_stat != NULL);

	if (regression_stat->se)
		g_free(regression_stat->se);
	if (regression_stat->t)
		g_free(regression_stat->t);
	if (regression_stat->xbar)
		g_free(regression_stat->xbar);
	g_free (regression_stat);
}

/* ------------------------------------------------------------------------- */

#ifdef DEFINE_COMMON
#define DELTA      0.01
/* FIXME:  I pulled this number out of my hat.
 * I need some testing to pick a sensible value.
 */
#define MAX_STEPS   200
#endif

/*
 * SYNOPSIS:
 *      result = derivative( f, &df, x, par, i)
 *
 * Approximates the partial derivative of a given function, at (x;params)
 * with respect to the parameter indicated by ith parameter.  The resulst
 * is stored in 'df'.
 *
 * See the header file for more information.
 */
static RegressionResult
SUFFIX(derivative) (SUFFIX(RegressionFunction) f,
	    DOUBLE *df,
	    DOUBLE *x, /* Only one point, not the whole data set. */
	    DOUBLE *par,
	    int index)
{
	DOUBLE y1, y2;
	RegressionResult result;
	DOUBLE par_save = par[index];

	par[index] = par_save - DELTA;
	result = (*f) (x, par, &y1);
	if (result != REG_ok) {
		par[index] = par_save;
		return result;
	}

	par[index] = par_save + DELTA;
	result = (*f) (x, par, &y2);
	if (result != REG_ok) {
		par[index] = par_save;
		return result;
	}

#ifdef DEBUG
	printf ("y1 = %lf\n", y1);
	printf ("y2 = %lf\n", y2);
	printf ("DELTA = %lf\n",DELTA);
#endif

	*df = (y2 - y1) / (2 * DELTA);
	par[index] = par_save;
	return REG_ok;
}

/*
 * SYNOPSIS:
 *   result = chi_squared (f, xvals, par, yvals, sigmas, x_dim, &chisq)
 *
 *                            /  y  - f(x ; par) \ 2
 *            2              |    i      i        |
 *         Chi   ==   Sum (  | ------------------ |   )
 *                            \     sigma        /
 *                                       i
 *
 * sigmas  -> Measurement errors in the dataset (along the y-axis).
 *            NULL means "no errors available", so they are all set to 1.
 *
 * x_dim   -> Number of data points.
 *
 * This value is not very meaningful without the sigmas.  However, it is
 * still useful for the fit.
 */
static RegressionResult
SUFFIX(chi_squared) (SUFFIX(RegressionFunction) f,
	     DOUBLE ** xvals, /* The entire data set. */
	     DOUBLE *par,
	     DOUBLE *yvals,   /* Ditto. */
	     DOUBLE *sigmas,  /* Ditto. */
	     int x_dim,          /* Number of data points. */
	     DOUBLE *chisq)   /* Chi Squared */
{
	int i;
	RegressionResult result;
	DOUBLE tmp, y;
	*chisq = 0;

	for (i = 0; i < x_dim; i++) {
		result = f (xvals[i], par, &y);
		if (result != REG_ok)
			return result;

		tmp = (yvals[i] - y ) / (sigmas ? sigmas[i] : 1);

		*chisq += tmp * tmp;
	}

	return REG_ok;
}


/*
 * SYNOPSIS:
 *      result = chi_derivative (f, &dchi, xvals, par, i, yvals,
 *                               sigmas, x_dim)
 *
 * This is a simple adaptation of the derivative() function specific to
 * the Chi Squared.
 */
static RegressionResult
SUFFIX(chi_derivative) (SUFFIX(RegressionFunction) f,
		DOUBLE *dchi,
		DOUBLE **xvals, /* The entire data set. */
		DOUBLE *par,
		int index,
		DOUBLE *yvals,  /* Ditto. */
		DOUBLE *sigmas, /* Ditto. */
		int x_dim)
{
	DOUBLE y1, y2;
	RegressionResult result;
	DOUBLE par_save = par[index];

	par[index] = par_save - DELTA;
	result = SUFFIX(chi_squared) (f, xvals, par, yvals, sigmas, x_dim, &y1);
	if (result != REG_ok) {
		par[index] = par_save;
		return result;
	}

	par[index] = par_save + DELTA;
	result = SUFFIX(chi_squared) (f, xvals, par, yvals, sigmas, x_dim, &y2);
	if (result != REG_ok) {
                par[index] = par_save;
		return result;
	}

#ifdef DEBUG
	printf ("y1 = %lf\n", y1);
	printf ("y2 = %lf\n", y2);
	printf ("DELTA = %lf\n", DELTA);
#endif

	*dchi = (y2 - y1) / (2 * DELTA);
	par[index] = par_save;
	return REG_ok;
}

/*
 * SYNOPSIS:
 *   result = coefficient_matrix (A, f, xvals, par, yvals, sigmas,
 *                                x_dim, p_dim, r)
 *
 * RETURNS:
 *   The coefficient matrix of the LM method.
 *
 * DETAIS:
 *   The coefficient matrix matrix is defined by
 *
 *            N        1      df  df
 *     A   = Sum  ( -------   --  --  ( i == j ? 1 + r : 1 ) a)
 *      ij   k=1    sigma^2   dp  dp
 *                       k      i   j
 *
 * A      -> p_dim X p_dim coefficient matrix.  MUST ALREADY BE ALLOCATED.
 *
 * sigmas -> Measurement errors in the dataset (along the y-axis).
 *           NULL means "no errors available", so they are all set to 1.
 *
 * x_dim  -> Number of data points.
 *
 * p_dim  -> Number of parameters.
 *
 * r      -> Positive constant.  It's value is altered during the LM procedure.
 */

static RegressionResult
SUFFIX(coefficient_matrix) (DOUBLE **A, /* Output matrix. */
		    SUFFIX(RegressionFunction) f,
		    DOUBLE **xvals, /* The entire data set. */
		    DOUBLE *par,
		    DOUBLE *yvals,  /* Ditto. */
		    DOUBLE *sigmas, /* Ditto. */
		    int x_dim,          /* Number of data points. */
		    int p_dim,          /* Number of parameters.  */
		    DOUBLE r)
{
	int i, j, k;
	RegressionResult result;
	DOUBLE df_i, df_j;
	DOUBLE sum, sigma;

	/* Notice that the matrix is symetric.  */
	for (i = 0; i < p_dim; i++) {
		for (j = 0; j <= i; j++) {
			sum = 0;
			for (k = 0; k < x_dim; k++) {
				result = SUFFIX(derivative) (f, &df_i, xvals[k],
						     par, i);
				if (result != REG_ok)
					return result;

				result = SUFFIX(derivative) (f, &df_j, xvals[k],
						     par, j);
				if (result != REG_ok)
					return result;

				sigma = (sigmas ? sigmas[k] : 1);

				sum += (df_i * df_j) / (sigma * sigma) *
					(i == j ? 1 + r : 1) ;
			}
			A[i][j] = A[j][i] = sum;
		}
	}

	return REG_ok;
}


/*
 * SYNOPSIS:
 *   result = parameter_errors (f, xvals, par, yvals, sigmas,
 *                              x_dim, p_dim, errors)
 *
 * Returns the errors associated with the parameters.
 * If an error is infinite, it is set to -1.
 *
 * sigmas  -> Measurement errors in the dataset (along the y-axis).
 *            NULL means "no errors available", so they are all set to 1.
 *
 * x_dim   -> Number of data points.
 *
 * p_dim   -> Number of parameters.
 *
 * errors  -> MUST ALREADY BE ALLOCATED.
 */

/* FIXME:  I am not happy with the behaviour with infinite errors.  */
static RegressionResult
SUFFIX(parameter_errors) (SUFFIX(RegressionFunction) f,
		  DOUBLE **xvals, /* The entire data set. */
		  DOUBLE *par,
		  DOUBLE *yvals,  /* Ditto. */
		  DOUBLE *sigmas, /* Ditto. */
		  int x_dim,          /* Number of data points. */
		  int p_dim,          /* Number of parameters.  */
		  DOUBLE *errors)
{
	RegressionResult result;
	DOUBLE **A;
	int i;

	ALLOC_MATRIX (A, p_dim, p_dim);

	result = SUFFIX(coefficient_matrix) (A, f, xvals, par, yvals, sigmas,
				     x_dim, p_dim, 0);
	if (result == REG_ok) {
		for (i = 0; i < p_dim; i++)
			/* FIXME: these were "[i][j]" which makes no sense.  */
			errors[i] = (A[i][i] != 0
				     ? 1 / SUFFIX(sqrt) (A[i][i])
				     : -1);
	}

	FREE_MATRIX (A, p_dim, p_dim);
	return result;
}


/*
 * SYNOPSIS:
 *   result = non_linear_regression (f, xvals, par, yvals, sigmas,
 *                                   x_dim, p_dim, &chi, errors)
 *
 * Returns the results of the non-linear regression from the given initial
 * values.
 * The resulting parameters are placed back into 'par'.
 *
 * PARAMETERS:
 *
 * sigmas  -> Measurement errors in the dataset (along the y-axis).
 *            NULL means "no errors available", so they are all set to 1.
 *
 * x_dim   -> Number of data points.
 *
 * p_dim   -> Number of parameters.
 *
 * errors  -> MUST ALREADY BE ALLOCATED.  These are the approximated standard
 *            deviation for each parameter.
 *
 * chi     -> Chi Squared of the final result.  This value is not very
 *            meaningful without the sigmas.
 */
RegressionResult
SUFFIX(go_non_linear_regression) (SUFFIX(RegressionFunction) f,
		       DOUBLE **xvals, /* The entire data set. */
		       DOUBLE *par,
		       DOUBLE *yvals,  /* Ditto. */
		       DOUBLE *sigmas, /* Ditto. */
		       int x_dim,          /* Number of data points. */
		       int p_dim,          /* Number of parameters.  */
		       DOUBLE *chi,
		       DOUBLE *errors)
{
	DOUBLE r = 0.001; /* Pick a conservative initial value. */
	DOUBLE *b, **A;
	DOUBLE *dpar;
	DOUBLE *tmp_par;
	DOUBLE chi_pre, chi_pos, dchi;
	RegressionResult result;
	int i, count;

	result = SUFFIX(chi_squared) (f, xvals, par, yvals, sigmas, x_dim, &chi_pre);
	if (result != REG_ok)
		return result;

	ALLOC_MATRIX (A, p_dim, p_dim);
	dpar    = g_new (DOUBLE, p_dim);
	tmp_par = g_new (DOUBLE, p_dim);
	b       = g_new (DOUBLE, p_dim);
#ifdef DEBUG
	printf ("Chi Squared : %lf", chi_pre);
#endif

	for (count = 0; count < MAX_STEPS; count++) {
		for (i = 0; i < p_dim; i++) {
			/*
			 *          d Chi
			 *  b   ==  -----
			 *   k       d p
			 *              k
			 */
			result = SUFFIX(chi_derivative) (f, &dchi, xvals, par, i,
						 yvals, sigmas, x_dim);
			if (result != REG_ok)
				goto out;

			b[i] = - dchi;
		}

		result = SUFFIX(coefficient_matrix) (A, f, xvals, par, yvals,
					     sigmas, x_dim, p_dim, r);
		if (result != REG_ok)
			goto out;

		result = SUFFIX(linear_solve) (A, b, p_dim, dpar);
		if (result != REG_ok)
			goto out;

		for(i = 0; i < p_dim; i++)
			tmp_par[i] = par[i] + dpar[i];

		result = SUFFIX(chi_squared) (f, xvals, par, yvals, sigmas,
				      x_dim, &chi_pos);
		if (result != REG_ok)
			goto out;

#ifdef DEBUG
		printf ("Chi Squared : %lf", chi_pre);
		printf ("Chi Squared : %lf", chi_pos);
		printf ("r  :  %lf", r);
#endif

		if (chi_pos <= chi_pre + DELTA / 2) {
			/* There is improvement */
			r /= 10;
			par = tmp_par;

			if (SUFFIX(fabs) (chi_pos - chi_pre) < DELTA)
				break;

			chi_pre = chi_pos;
		} else {
			r *= 10;
		}
	}

	result = SUFFIX(parameter_errors) (f, xvals, par, yvals, sigmas,
				   x_dim, p_dim, errors);
	if (result != REG_ok)
		goto out;

	*chi = chi_pos;

 out:
	FREE_MATRIX (A, p_dim, p_dim);
	g_free (dpar);
	g_free (tmp_par);
	g_free (b);

	return result;
}
