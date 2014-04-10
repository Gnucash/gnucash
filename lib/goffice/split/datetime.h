#ifndef GNUMERIC_DATETIME_H
#define GNUMERIC_DATETIME_H

#include "gnumeric.h"
#include "numbers.h"
#include <time.h>

struct _GnmDateConventions {
	gboolean use_1904;	/* Use MacOffice 1904 based date convention,
				 * Rather than the Win32 style 1900 */
};

/*
 * Naming conventions:
 *
 * "g": a GDate *.
 * "timet": Unix' time_t.
 * "serial": Excel serial day number.
 * "serial_raw": serial plus time as fractional day.
 */

/* Week numbering methods */
/* 1:   Week starts on Sunday. Days before first Sunday are in week 0. */
/* 2:   Week starts on Monday. Days before first Monday are in week 0. */
/* 150: ISO 8601 week number. */
#define WEEKNUM_METHOD_SUNDAY 1
#define WEEKNUM_METHOD_MONDAY 2
#define WEEKNUM_METHOD_ISO    150

/* These do not round and produces fractional values, i.e., includes time.  */
gnm_float datetime_value_to_serial_raw (GnmValue const *v, GnmDateConventions const *conv);
gnm_float datetime_timet_to_serial_raw (time_t t,	GnmDateConventions const *conv);

/* These are date-only, no time.  */
int      datetime_value_to_serial	(GnmValue const *v, GnmDateConventions const *conv);
int      datetime_timet_to_serial	(time_t t,		GnmDateConventions const *conv);
gboolean datetime_value_to_g		(GDate *res, GnmValue const *v, GnmDateConventions const *conv);
int      datetime_g_to_serial		(GDate const *date,	 GnmDateConventions const *conv);
void     datetime_serial_to_g		(GDate *res, int serial, GnmDateConventions const *conv);
time_t   datetime_serial_to_timet	(int serial,		 GnmDateConventions const *conv);
int      datetime_serial_raw_to_serial	(gnm_float raw);

/* These are time-only assuming a 24h day.  It probably loses completely on */
/* days with summer time ("daylight savings") changes.  */
int datetime_value_to_seconds (GnmValue const *v);
int datetime_timet_to_seconds (time_t t);
int datetime_serial_raw_to_seconds (gnm_float raw);

int datetime_g_days_between (GDate const *date1, GDate const *date2);

/* Number of full months between date1 and date2. */
/* largest value s.t. g_date_add_months (date1, result) <= date2 */
/* except that if the day is decreased in g_date_add_monts, treat
   that as > the date it is decreased to. */
/* ( datetime_g_months_between ( March 31, April 30 ) == 0
     even though g_date_add_months ( Mar 31, 1 ) <= Apr 30.... */
int datetime_g_months_between (GDate const *date1, GDate const *date2);
/* Number of full years between date1 and date2. */
/* (g_date_add_years (date1, result) <= date2; largest such value. */
/*  treat add_years (29-feb, x) > 28-feb ) */
int datetime_g_years_between (GDate const *date1, GDate const *date2);
/* week number according to the given method. */
int datetime_weeknum (GDate const *date, int method);

typedef enum { /* see doc/fn-financial-basis.txt for details */
	BASIS_MSRB_30_360     = 0,
	BASIS_ACT_ACT         = 1,
	BASIS_ACT_360         = 2,
	BASIS_ACT_365         = 3,
	BASIS_30E_360         = 4,
	BASIS_30Ep_360        = 5,
	BASIS_MSRB_30_360_SYM = 6         /* Gnumeric extension.  */
} basis_t;

gint32  days_between_basis (GDate const *from, GDate const *to, basis_t basis);
gnm_float yearfrac         (GDate const *from, GDate const *to, basis_t basis);
int     annual_year_basis  (GnmValue const *value_date, basis_t basis,
			    GnmDateConventions const *date_conv);

typedef struct {
	int	 freq;
	basis_t  basis;
	gboolean eom;
	GnmDateConventions const *date_conv;
} GnmCouponConvention;

void	  coup_cd    (GDate *res, GDate const *settle, GDate const *mat,
		      int freq, gboolean eom, gboolean next);
gnm_float coupdays   (GDate const *settle, GDate const *mat,
		      GnmCouponConvention const *conv);
gnm_float coupdaybs  (GDate const *settle, GDate const *mat,
		      GnmCouponConvention const *conv);
gnm_float coupdaysnc (GDate const *settle, GDate const *mat,
		      GnmCouponConvention const *conv);

int gnm_date_convention_base (GnmDateConventions const *conv);

#endif /* GNUMERIC_DATETIME_H */
