#ifndef GO_RANGEFUNC_H
#define GO_RANGEFUNC_H

int go_range_sum (const double *xs, int n, double *res);
int go_range_sumsq (const double *xs, int n, double *res);
int go_range_average (const double *xs, int n, double *res);
int go_range_min (const double *xs, int n, double *res);
int go_range_max (const double *xs, int n, double *res);
int go_range_maxabs (const double *xs, int n, double *res);
int go_range_devsq (const double *xs, int n, double *res);

#ifdef GOFFICE_WITH_LONG_DOUBLE
int go_range_suml (const long double *xs, int n, long double *res);
int go_range_sumsql (const long double *xs, int n, long double *res);
int go_range_averagel (const long double *xs, int n, long double *res);
int go_range_minl (const long double *xs, int n, long double *res);
int go_range_maxl (const long double *xs, int n, long double *res);
int go_range_maxabsl (const long double *xs, int n, long double *res);
int go_range_devsql (const long double *xs, int n, long double *res);
#endif

#endif	/* GO_RANGEFUNC_H */
