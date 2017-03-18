/*
 * gnc-gdate-utils.c -- utility functions for manipulating
 *              GDate data structures from GLib
 * Copyright (C) 2005 David Hampton <hampton@employees.org>
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; either version 2 of
 * the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, contact:
 *
 * Free Software Foundation           Voice:  +1-617-542-5942
 * 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652
 * Boston, MA  02110-1301,  USA       gnu@gnu.org
 */

#include "config.h"
#include <glib.h>

#include "gnc-gdate-utils.h"

void
gnc_gdate_set_today (GDate* gd)
{
    GDate *today = gnc_g_date_new_today ();
    g_date_set_julian (gd, g_date_get_julian (today));
    g_date_free (today);
}

void
gnc_gdate_set_time64 (GDate* gd, time64 time)
{
    struct tm tm;
    gnc_localtime_r(&time, &tm);
    g_date_set_dmy (gd, tm.tm_mday, tm.tm_mon + 1, tm.tm_year + 1900);

}

gboolean
gnc_gdate_equal(gconstpointer gda, gconstpointer gdb)
{
    return (g_date_compare( (GDate*)gda, (GDate*)gdb ) == 0 ? TRUE : FALSE);
}

guint
gnc_gdate_hash( gconstpointer gd )
{
    gint val = (g_date_get_year( (GDate*)gd ) * 10000)
               + (g_date_get_month( (GDate*)gd ) * 100)
               + g_date_get_day( (GDate*)gd );
    return g_int_hash( &val );
}


time64
gnc_time64_get_day_start_gdate (const GDate *date)
{
    struct tm stm;
    time64 secs;

    /* First convert to a 'struct tm' */
    g_date_to_struct_tm (date, &stm);

    /* Then convert to number of seconds */
    secs = gnc_mktime (&stm);
    return secs;
}

time64
gnc_time64_get_day_end_gdate (const GDate *date)
{
    struct tm stm;
    time64 secs;

    /* First convert to a 'struct tm' */
    g_date_to_struct_tm(date, &stm);

    /* Force to th last second of the day */
    stm.tm_hour = 23;
    stm.tm_min = 59;
    stm.tm_sec = 59;
    stm.tm_isdst = -1;

    /* Then convert to number of seconds */
    secs = gnc_mktime (&stm);
    return secs;
}


void
gnc_gdate_set_month_start (GDate *date)
{
    g_date_set_day(date, 1);
}


/** Convert a GDate to the last day of the month.  This routine has no
 *  knowledge of how many days are in a month, whether its a leap
 *  year, etc.  All that information is contained in the glib date
 *  functions.
 *
 *  @param date The GDate to modify.
 */
void
gnc_gdate_set_month_end (GDate *date)
{
    /* First set the start of next month. */
    g_date_set_day(date, 1);
    g_date_add_months(date, 1);

    /* Then back up one day */
    g_date_subtract_days(date, 1);
}


/** Convert a GDate to the first day of the prebvious month.  This
 *  routine has no knowledge of how many days are in a month, whether
 *  its a leap year, etc.  All that information is contained in the
 *  glib date functions.
 *
 *  @param date The GDate to modify.
 */
void
gnc_gdate_set_prev_month_start (GDate *date)
{
    g_date_set_day(date, 1);
    g_date_subtract_months(date, 1);
}


/** Convert a GDate to the last day of the prebvious month.  This
 *  routine has no knowledge of how many days are in a month, whether
 *  its a leap year, etc.  All that information is contained in the
 *  glib date functions.
 *
 *  @param date The GDate to modify.
 */
void
gnc_gdate_set_prev_month_end (GDate *date)
{
    /* This will correctly handle the varying month lengths */
    g_date_set_day(date, 1);
    g_date_subtract_days(date, 1);
}

/* ========== */

void
gnc_gdate_set_quarter_start (GDate *date)
{
    gint months;

    /* Set the date to the first day of the specified month. */
    g_date_set_day(date, 1);

    /* Back up 0-2 months */
    months = (g_date_get_month(date) - G_DATE_JANUARY) % 3;
    g_date_subtract_months(date, months);
}


void
gnc_gdate_set_quarter_end (GDate *date)
{
    gint months;

    /* Set the date to the first day of the specified month. */
    g_date_set_day(date, 1);

    /* Add 1-3 months to get the first day of the next quarter.*/
    months = (g_date_get_month(date) - G_DATE_JANUARY) % 3;
    g_date_add_months(date, 3 - months);

    /* Now back up one day */
    g_date_subtract_days(date, 1);
}


void
gnc_gdate_set_prev_quarter_start (GDate *date)
{
    gnc_gdate_set_quarter_start(date);
    g_date_subtract_months(date, 3);
}


void
gnc_gdate_set_prev_quarter_end (GDate *date)
{
    gnc_gdate_set_quarter_end(date);
    g_date_subtract_months(date, 3);
}

/* ========== */

void
gnc_gdate_set_year_start (GDate *date)
{
    g_date_set_month(date, G_DATE_JANUARY);
    g_date_set_day(date, 1);
}


void
gnc_gdate_set_year_end (GDate *date)
{
    g_date_set_month(date, G_DATE_DECEMBER);
    g_date_set_day(date, 31);
}


void
gnc_gdate_set_prev_year_start (GDate *date)
{
    gnc_gdate_set_year_start(date);
    g_date_subtract_years(date, 1);
}


void
gnc_gdate_set_prev_year_end (GDate *date)
{
    gnc_gdate_set_year_end(date);
    g_date_subtract_years(date, 1);
}

/* ========== */

void
gnc_gdate_set_fiscal_year_start (GDate *date,
                                 const GDate *fy_end)
{
    GDate temp;
    gboolean new_fy;

    g_return_if_fail(date);
    g_return_if_fail(fy_end);

    /* Compute the FY end that occurred this CY */
    temp = *fy_end;
    g_date_set_year(&temp, g_date_get_year(date));

    /* Has it already passed? */
    new_fy = (g_date_compare(date, &temp) > 0);

    /* Set start date */
    *date = temp;
    g_date_add_days(date, 1);
    if (!new_fy)
        g_date_subtract_years(date, 1);
}

void
gnc_gdate_set_fiscal_year_end (GDate *date,
                               const GDate *fy_end)
{
    GDate temp;
    gboolean new_fy;

    g_return_if_fail(date);
    g_return_if_fail(fy_end);

    /* Compute the FY end that occurred this CY */
    temp = *fy_end;
    g_date_set_year(&temp, g_date_get_year(date));

    /* Has it already passed? */
    new_fy = (g_date_compare(date, &temp) > 0);

    /* Set end date */
    *date = temp;
    if (new_fy)
        g_date_add_years(date, 1);
}

void
gnc_gdate_set_prev_fiscal_year_start (GDate *date,
                                      const GDate *fy_end)
{
    g_return_if_fail(date);
    g_return_if_fail(fy_end);

    gnc_gdate_set_fiscal_year_start(date, fy_end);
    g_date_subtract_years(date, 1);
}

void
gnc_gdate_set_prev_fiscal_year_end (GDate *date,
                                    const GDate *fy_end)
{
    g_return_if_fail(date);
    g_return_if_fail(fy_end);

    gnc_gdate_set_fiscal_year_end(date, fy_end);
    g_date_subtract_years(date, 1);
}
