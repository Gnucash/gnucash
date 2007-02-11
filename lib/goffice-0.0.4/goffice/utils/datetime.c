/* vim: set sw=8: -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/*
 * datetime.c: Date and time routines grabbed from elsewhere.
 *
 * Authors:
 *   Miguel de Icaza (miguel@gnu.org)
 *   Morten Welinder <terra@gnome.org>
 *   Jukka-Pekka Iivonen <iivonen@iki.fi>
 *   Andreas J. Guelzow <aguelzow@taliesin.ca>
 */

#include <goffice/goffice-config.h>
#include "datetime.h"

#include <math.h>

#define SECS_PER_DAY (24 * 60 * 60)
#define HALF_SEC (0.5 / SECS_PER_DAY)

/* ------------------------------------------------------------------------- */

/* One less that the Julian day number of 19000101.  */
static int date_origin = 0;
/* Julian day number of 19040101.  */
static int date_origin_1904 = 0;

/*
 * The serial number of 19000228.  Excel allocates a serial number for
 * the non-existing date 19000229.
 */
static int const date_serial_19000228 = 59;

static void
date_init (void)
{
	/* Day 1 means 1st of January of 1900 */
	GDate* date = g_date_new_dmy (1, 1, 1900);
	date_origin = g_date_get_julian (date) - 1;

	/* Day 0 means 1st of January of 1904 */
	g_date_set_dmy (date, 1, 1, 1904);
	date_origin_1904 = g_date_get_julian (date);
	g_date_free (date);
}

/* ------------------------------------------------------------------------- */

int
datetime_g_to_serial (GDate const *date, GODateConventions const *conv)
{
	int day;

	if (!date_origin)
		date_init ();

	if (conv && conv->use_1904)
		return g_date_get_julian (date) - date_origin_1904;
	day = g_date_get_julian (date) - date_origin;
	return day + (day > date_serial_19000228);
}

/* ------------------------------------------------------------------------- */

void
datetime_serial_to_g (GDate *res, int serial, GODateConventions const *conv)
{
	if (!date_origin)
		date_init ();

	g_date_clear (res, 1);
	if (conv && conv->use_1904)
		g_date_set_julian (res, serial + date_origin_1904);
	else if (serial > date_serial_19000228) {
		if (serial == date_serial_19000228 + 1)
			g_warning ("Request for date 19000229.");
		g_date_set_julian (res, serial + date_origin - 1);
	} else
		g_date_set_julian (res, serial + date_origin);
}

/* ------------------------------------------------------------------------- */

double
datetime_timet_to_serial_raw (time_t t, GODateConventions const *conv)
{
	struct tm *tm = localtime (&t);
	int secs;
	GDate date;

        g_date_clear (&date, 1);
	g_date_set_time (&date, t);
	secs = tm->tm_hour * 3600 + tm->tm_min * 60 + tm->tm_sec;
	return datetime_g_to_serial (&date, conv) +
		secs / (double)SECS_PER_DAY;
}

/* ------------------------------------------------------------------------- */

int
datetime_serial_raw_to_serial (double raw)
{
	return (int) floor (raw + HALF_SEC);
}

/* ------------------------------------------------------------------------- */

int
datetime_timet_to_serial (time_t t, GODateConventions const *conv)
{
	return datetime_serial_raw_to_serial (datetime_timet_to_serial_raw (t, conv));
}

/* ------------------------------------------------------------------------- */

time_t
datetime_serial_to_timet (int serial, GODateConventions const *conv)
{
	GDate gd;
	struct tm tm;

	datetime_serial_to_g (&gd, serial, conv);
	g_date_to_struct_tm (&gd, &tm);

	return mktime (&tm);
}

/* ------------------------------------------------------------------------- */
/* This is time-only assuming a 24h day.  It probably loses completely on */
/* days with summer time ("daylight savings") changes.  */

int
datetime_serial_raw_to_seconds (double raw)
{
	raw += HALF_SEC;
	return (raw - floor (raw)) * SECS_PER_DAY;
}

/* ------------------------------------------------------------------------- */

int
datetime_timet_to_seconds (time_t t)
{
	/* we just want the seconds, actual date does not matter. So we can ignore
	 * the date convention (1900 vs 1904) */
	return datetime_serial_raw_to_seconds (datetime_timet_to_serial_raw (t, NULL));
}

/* ------------------------------------------------------------------------- */

int
datetime_g_days_between (GDate const* date1, GDate const *date2)
{
	g_return_val_if_fail (g_date_valid (date1), 0);
	g_return_val_if_fail (g_date_valid (date2), 0);

	return (int) (g_date_get_julian (date2) - g_date_get_julian (date1));
}

/* ------------------------------------------------------------------------- */

int
datetime_g_months_between (GDate const *date1, GDate const *date2)
{
	g_return_val_if_fail (g_date_valid (date1), 0);
	g_return_val_if_fail (g_date_valid (date2), 0);

	/* find the difference according to the month and year ordinals,
	   but discount the last month if there are not enough days. */
	return 12 * (g_date_get_year (date2) - g_date_get_year (date1))
		+ g_date_get_month (date2) - g_date_get_month (date1)
		- (g_date_get_day (date2) >= g_date_get_day (date1) ? 0 : 1);
}

/* ------------------------------------------------------------------------- */

int
datetime_g_years_between (GDate const *date1, GDate const *date2)
{
	int months;

	g_return_val_if_fail (g_date_valid (date1), 0);
	g_return_val_if_fail (g_date_valid (date2), 0);

	months = datetime_g_months_between (date1, date2);
	return months > 0 ? months / 12 : -(-months / 12);
}

/* ------------------------------------------------------------------------- */

/**
 * datetime_weeknum (GDate *date, int method)
 * @date      date
 * @method    week numbering method
 *
 * Returns week number according to the given method.
 * 1:   Week starts on Sunday. Days before first Sunday are in week 0.
 * 2:   Week starts on Monday. Days before first Monday are in week 0.
 * 150: ISO 8601 week number. See datetime_isoweeknum.
 */
int
datetime_weeknum (GDate const *date, int method)
{
	int res;

	g_return_val_if_fail (g_date_valid (date), -1);
	g_return_val_if_fail (method == WEEKNUM_METHOD_SUNDAY ||
			      method == WEEKNUM_METHOD_MONDAY ||
			      method == WEEKNUM_METHOD_ISO,
			      -1);

	switch (method) {
	case WEEKNUM_METHOD_SUNDAY:
		res = g_date_get_sunday_week_of_year (date); break;
	case WEEKNUM_METHOD_MONDAY:
		res = g_date_get_monday_week_of_year (date); break;
	case WEEKNUM_METHOD_ISO:
		res = g_date_get_iso8601_week_of_year (date); break;
	default: res = -1;
	}

	return res;
}

/* ------------------------------------------------------------------------- */

static gint32
days_between_BASIS_MSRB_30_360 (GDate const *from, GDate const *to)
{
	int y1, m1, d1, y2, m2, d2;

	y1 = g_date_get_year (from);
	m1 = g_date_get_month (from);
	d1 = g_date_get_day (from);
	y2 = g_date_get_year (to);
	m2 = g_date_get_month (to);
	d2 = g_date_get_day (to);

	if (m1 == 2 && g_date_is_last_of_month (from))
		d1 = 30;
	if (d2 == 31 && d1 >= 30)
		d2 = 30;
	if (d1 == 31)
		d1 = 30;

	return (y2 - y1) * 360 + (m2 - m1) * 30 + (d2 - d1);
}

static gint32
days_between_BASIS_MSRB_30_360_SYM (GDate const *from, GDate const *to)
{
	int y1, m1, d1, y2, m2, d2;

	y1 = g_date_get_year (from);
	m1 = g_date_get_month (from);
	d1 = g_date_get_day (from);
	y2 = g_date_get_year (to);
	m2 = g_date_get_month (to);
	d2 = g_date_get_day (to);

	if (m1 == 2 && g_date_is_last_of_month (from))
		d1 = 30;
	if (m2 == 2 && g_date_is_last_of_month (to))
		d2 = 30;
	if (d2 == 31 && d1 >= 30)
		d2 = 30;
	if (d1 == 31)
		d1 = 30;

	return (y2 - y1) * 360 + (m2 - m1) * 30 + (d2 - d1);
}

static gint32
days_between_BASIS_30E_360 (GDate const *from, GDate const *to)
{
	int y1, m1, d1, y2, m2, d2;

	y1 = g_date_get_year (from);
	m1 = g_date_get_month (from);
	d1 = g_date_get_day (from);
	y2 = g_date_get_year (to);
	m2 = g_date_get_month (to);
	d2 = g_date_get_day (to);

	if (d1 == 31)
		d1 = 30;
	if (d2 == 31)
		d2 = 30;

	return (y2 - y1) * 360 + (m2 - m1) * 30 + (d2 - d1);
}

static gint32
days_between_BASIS_30Ep_360 (GDate const *from, GDate const *to)
{
	int y1, m1, d1, y2, m2, d2;

	y1 = g_date_get_year (from);
	m1 = g_date_get_month (from);
	d1 = g_date_get_day (from);
	y2 = g_date_get_year (to);
	m2 = g_date_get_month (to);
	d2 = g_date_get_day (to);

	if (d1 == 31)
		d1 = 30;
	if (d2 == 31) {
		d2 = 1;
		m2++;
		/* No need to check for m2 == 13 since 12*30 == 360 */
	}

	return (y2 - y1) * 360 + (m2 - m1) * 30 + (d2 - d1);
}

/*
 * days_between_basis
 *
 * @from      : GDate *
 * @to        : GDate *
 * @basis     : basis_t
 * see datetime.h and doc/fn-financial-basis.txt for details
 *
 * @in_order  : dates are considered in order
 *
 * returns    : Number of days strictly between from and to +1
 *
 */

gint32
days_between_basis (GDate const *from, GDate const *to, basis_t basis)
{
	int sign = 1;

	if (g_date_compare (from, to) == 1) {
		GDate const *tmp = from;
		from = to;
		to = tmp;
		sign = -1;
	}

	switch (basis) {
	case BASIS_ACT_ACT:
	case BASIS_ACT_360:
	case BASIS_ACT_365:
		return sign * (g_date_get_julian (to) - g_date_get_julian (from));
	case BASIS_30E_360:
		return sign * days_between_BASIS_30E_360 (from, to);
	case BASIS_30Ep_360:
		return sign * days_between_BASIS_30Ep_360 (from, to);
	case BASIS_MSRB_30_360_SYM:
		return sign * days_between_BASIS_MSRB_30_360_SYM (from, to);
	case BASIS_MSRB_30_360:
	default:
		return sign * days_between_BASIS_MSRB_30_360 (from, to);
	}
}

/* ------------------------------------------------------------------------- */

/*
 * coup_cd
 *
 * @res	      :
 * @settlement: GDate *
 * @maturity  : GDate *  must follow settlement strictly
 * @freq      : int      divides 12 evenly
 * @eom       : gboolean whether to do special end of month
 *                       handling
 * @next      : gboolean whether next or previous date
 *
 * returns    : GDate *  next  or previous coupon date
 *
 * this function does not depend on the basis of counting!
 */
void
coup_cd (GDate *result, GDate const *settlement, GDate const *maturity,
	 int freq, gboolean eom, gboolean next)
{
        int        months, periods;
	gboolean   is_eom_special;

	is_eom_special = eom && g_date_is_last_of_month (maturity);

	g_date_clear (result, 1);

	months = 12 / freq;
	periods = (g_date_get_year(maturity) - g_date_get_year (settlement));
	if (periods > 0)
		periods = (periods - 1) * freq;

	do {
		g_date_set_julian (result, g_date_get_julian (maturity));
		periods++;
		g_date_subtract_months (result, periods * months);
		if (is_eom_special) {
			int ndays = g_date_get_days_in_month
				(g_date_get_month (result),
				 g_date_get_year (result));
			g_date_set_day (result, ndays);
		}
	} while (g_date_compare (settlement, result) < 0 );

	if (next) {
		g_date_set_julian (result, g_date_get_julian (maturity));
		periods--;
		g_date_subtract_months (result, periods * months);
		if (is_eom_special) {
			int ndays = g_date_get_days_in_month
				(g_date_get_month (result),
				 g_date_get_year (result));
			g_date_set_day (result, ndays);
		}
	}
}

/* ------------------------------------------------------------------------- */


/*
 * Returns the number of days in the coupon period of the settlement date.
 * Currently, returns negative numbers if the branch is not implemented.
 */
double
coupdays (GDate const *settlement, GDate const *maturity,
	  GnmCouponConvention const *conv)
{
	GDate prev, next;

        switch (conv->basis) {
	case BASIS_MSRB_30_360:
	case BASIS_ACT_360:
        case BASIS_30E_360:
        case BASIS_30Ep_360:
		return 360 / conv->freq;
	case BASIS_ACT_365:
		return 365.0 / conv->freq;
	case BASIS_ACT_ACT:
	default:
		coup_cd (&next, settlement, maturity, conv->freq, conv->eom, TRUE);
		coup_cd (&prev, settlement, maturity, conv->freq, conv->eom, FALSE);
		return days_between_basis (&prev, &next, BASIS_ACT_ACT);
        }
}

/* ------------------------------------------------------------------------- */


/*
 * Returns the number of days from the beginning of the coupon period to
 * the settlement date.
 */
double
coupdaybs (GDate const *settlement, GDate const *maturity,
	   GnmCouponConvention const *conv)
{
	GDate prev_coupon;
	coup_cd (&prev_coupon, settlement, maturity, conv->freq, conv->eom, FALSE);
	return days_between_basis (&prev_coupon, settlement, conv->basis);
}

/**
 * coupdaysnc :
 * @settlement :
 * @maturity :
 * @freq :
 * @basis :
 * @eom :
 *
 * Returns the number of days from the settlement date to the next
 * coupon date.
 **/
double
coupdaysnc (GDate const *settlement, GDate const *maturity,
	    GnmCouponConvention const *conv)
{
	GDate next_coupon;
	coup_cd (&next_coupon, settlement, maturity, conv->freq, conv->eom, TRUE);
	return days_between_basis (settlement, &next_coupon, conv->basis);
}

int
gnm_date_convention_base (GODateConventions const *conv)
{
	g_return_val_if_fail (conv != NULL, 1900);
	return conv->use_1904 ? 1904 : 1900;
}
