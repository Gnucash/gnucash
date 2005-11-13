#ifndef _GO_DATETIME_H_
#define _GO_DATETIME_H_

#include <goffice/utils/goffice-utils.h>
#include <glib.h>
#include <time.h>

G_BEGIN_DECLS

struct _GODateConventions {
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
enum {
	WEEKNUM_METHOD_SUNDAY = 1,
	WEEKNUM_METHOD_MONDAY = 2,
	WEEKNUM_METHOD_ISO = 150
};

/* These do not round and produces fractional values, i.e., includes time.  */
double	datetime_timet_to_serial_raw  (time_t t, GODateConventions const *conv);

/* These are date-only, no time.  */
int	datetime_timet_to_serial      (time_t t, GODateConventions const *conv);
int	datetime_g_to_serial	      (GDate const *date, GODateConventions const *conv);
void	datetime_serial_to_g	      (GDate *res, int serial, GODateConventions const *conv);
time_t	datetime_serial_to_timet      (int serial, GODateConventions const *conv);
int	datetime_serial_raw_to_serial (double raw);

/* These are time-only assuming a 24h day.  It probably loses completely on */
/* days with summer time ("daylight savings") changes.  */
int datetime_timet_to_seconds (time_t t);
int datetime_serial_raw_to_seconds (double raw);

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

typedef struct {
	int	 freq;
	basis_t  basis;
	gboolean eom;
	GODateConventions const *date_conv;
} GnmCouponConvention;

void	  coup_cd    (GDate *res, GDate const *settle, GDate const *mat,
		      int freq, gboolean eom, gboolean next);
double coupdays   (GDate const *settle, GDate const *mat,
		      GnmCouponConvention const *conv);
double coupdaybs  (GDate const *settle, GDate const *mat,
		      GnmCouponConvention const *conv);
double coupdaysnc (GDate const *settle, GDate const *mat,
		      GnmCouponConvention const *conv);

int gnm_date_convention_base (GODateConventions const *conv);

G_END_DECLS

#endif /* _GO_DATETIME_H_ */
