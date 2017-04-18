/*
 * gnc-gdate-utils.h -- utility functions for manipulating
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

/** @addtogroup GLib
    @{ */
/** @addtogroup GDate GDate Utilities

    This file provides routines that help make it easier to use GDates
    from within Gnucash.  A GDate is a data strucutre provided by GLib
    that handles dates from year 1 to year 9999.

    @{ */
/** @file gnc-gdate-utils.h
 *  @brief GDate helper routines.
 *  @author Copyright (C) 2005 David Hampton <hampton@employees.org>
 */

#ifndef GNC_GDATE_UTILS_H
#define GNC_GDATE_UTILS_H

#include <gnc-date.h>

/** @name GDate time64 setters
    @{ */
/** Set a GDate to the current day
 * @param theGDate: The date to act on
 */
void gnc_gdate_set_today (GDate* gd);

/** Set a GDate to a time64
 * @param theGDate: the date to act on
 * @param time: the time to set it to.
 */
void gnc_gdate_set_time64 (GDate* gd, time64 time);

/** @} */

/** @name GDate hash table support
    @{ */

/** Compares two GDate*'s for equality; useful for using GDate*'s as
 *  GHashTable keys. */
gint gnc_gdate_equal(gconstpointer gda, gconstpointer gdb);


/** Provides a "hash" of a GDate* value; useful for using GDate*'s as
 *  GHashTable keys. */
guint gnc_gdate_hash( gconstpointer gd );

/** @} */

/** @name GDate to time64 conversions
    @{ */

/** The gnc_time64_get_day_start() routine will take the given time in
 *  GLib GDate format and adjust it to the first second of that day.
 */
time64 gnc_time64_get_day_start_gdate (const GDate *date);

/** The gnc_time64_get_day_end() routine will take the given time in
 *  GLib GDate format and adjust it to the last second of that day.
 */
time64 gnc_time64_get_day_end_gdate (const GDate *date);

/** @} */

/** @name Date Manipulation
    @{ */

/** This function modifies a GDate to set it to the first day of the
 *  month in which it falls.  For example, if this function is called
 *  with a date of 2003-09-24 the date will be modified to 2003-09-01.
 *
 *  @param date The GDate to modify. */
void gnc_gdate_set_month_start (GDate *date);


/** This function modifies a GDate to set it to the last day of the
 *  month in which it falls.  For example, if this function is called
 *  with a date of 2003-09-24 the date will be modified to 2003-09-30.
 *
 *  @param date The GDate to modify. */
void gnc_gdate_set_month_end (GDate *date);


/** This function modifies a GDate to set it to the first day of the
 *  month prior to the one in which it falls.  For example, if this
 *  function is called with a date of 2003-09-24 the date will be
 *  modified to 2003-08-01.
 *
 *  @param date The GDate to modify. */
void gnc_gdate_set_prev_month_start (GDate *date);


/** This function modifies a GDate to set it to the last day of the
 *  month prior to the one in which it falls.  For example, if this
 *  function is called with a date of 2003-09-24 the date will be
 *  modified to 2003-08-31.
 *
 *  @param date The GDate to modify. */
void gnc_gdate_set_prev_month_end (GDate *date);


/** This function modifies a GDate to set it to the first day of the
 *  quarter in which it falls.  For example, if this function is called
 *  with a date of 2003-09-24 the date will be modified to 2003-09-01.
 *
 *  @param date The GDate to modify. */
void gnc_gdate_set_quarter_start (GDate *date);


/** This function modifies a GDate to set it to the last day of the
 *  quarter in which it falls.  For example, if this function is called
 *  with a date of 2003-09-24 the date will be modified to 2003-12-31.
 *
 *  @param date The GDate to modify. */
void gnc_gdate_set_quarter_end (GDate *date);


/** This function modifies a GDate to set it to the first day of the
 *  quarter prior to the one in which it falls.  For example, if this
 *  function is called with a date of 2003-09-24 the date will be
 *  modified to 2003-06-01.
 *
 *  @param date The GDate to modify. */
void gnc_gdate_set_prev_quarter_start (GDate *date);


/** This function modifies a GDate to set it to the last day of the
 *  quarter prior to the one in which it falls.  For example, if this
 *  function is called with a date of 2003-09-24 the date will be
 *  modified to 2003-07-31.
 *
 *  @param date The GDate to modify. */
void gnc_gdate_set_prev_quarter_end (GDate *date);


/** This function modifies a GDate to set it to the first day of the
 *  year in which it falls.  For example, if this function is called
 *  with a date of 2003-09-24 the date will be modified to 2003-01-01.
 *
 *  @param date The GDate to modify. */
void gnc_gdate_set_year_start (GDate *date);


/** This function modifies a GDate to set it to the last day of the
 *  year in which it falls.  For example, if this function is called
 *  with a date of 2003-09-24 the date will be modified to 2003-12-31.
 *
 *  @param date The GDate to modify. */
void gnc_gdate_set_year_end (GDate *date);


/** This function modifies a GDate to set it to the first day of the
 *  year prior to the one in which it falls.  For example, if this
 *  function is called with a date of 2003-09-24 the date will be
 *  modified to 2002-01-01.
 *
 *  @param date The GDate to modify. */
void gnc_gdate_set_prev_year_start (GDate *date);


/** This function modifies a GDate to set it to the last day of the
 *  year prior to the one in which it falls.  For example, if this
 *  function is called with a date of 2003-09-24 the date will be
 *  modified to 2002-12-31.
 *
 *  @param date The GDate to modify. */
void gnc_gdate_set_prev_year_end (GDate *date);


/** This function modifies a GDate to set it to the first day of the
 *  fiscal year in which it falls.  For example, if this function is
 *  called with a date of 2003-09-24 and a fiscal year ending July
 *  31st, the date will be modified to 2003-08-01.
 *
 *  @param date The GDate to modify.
 *
 *  @param year_end A GDate containing the last month and day of the
 *  fiscal year.  The year field of this argument is ignored. */
void gnc_gdate_set_fiscal_year_start (GDate *date, const GDate *year_end);


/** This function modifies a GDate to set it to the last day of the
 *  fiscal year in which it falls.  For example, if this function is
 *  called with a date of 2003-09-24 and a fiscal year ending July
 *  31st, the date will be modified to 2004-07-31.
 *
 *  @param date The GDate to modify.
 *
 *  @param year_end A GDate containing the last month and day of the
 *  fiscal year.  The year field of this argument is ignored. */
void gnc_gdate_set_fiscal_year_end (GDate *date, const GDate *year_end);


/** This function modifies a GDate to set it to the first day of the
 *  fiscal year prior to the one in which it falls.  For example, if
 *  this function is called with a date of 2003-09-24 and a fiscal
 *  year ending July 31st, the date will be modified to 2002-08-01.
 *
 *  @param date The GDate to modify.
 *
 *  @param year_end A GDate containing the last month and day of the
 *  fiscal year.  The year field of this argument is ignored. */
void gnc_gdate_set_prev_fiscal_year_start (GDate *date, const GDate *year_end);


/** This function modifies a GDate to set it to the last day of the
 *  fiscal year prior to the one in which it falls.  For example, if
 *  this function is called with a date of 2003-09-24 and a fiscal
 *  year ending July 31st, the date will be modified to 2003-07-31.
 *
 *  @param date The GDate to modify.
 *
 *  @param year_end A GDate containing the last month and day of the
 *  fiscal year.  The year field of this argument is ignored. */
void gnc_gdate_set_prev_fiscal_year_end (GDate *date, const GDate *year_end);


// used in Calendar widget

/// gnc_gdate_next_month - calculate the next month based Selected Calendar,
///           the input is a Gregorian Date and the output will be in Gregorian
/// \param date GDate Object , a gregorian date
void gnc_gdate_next_month(GDate *date);

/// gnc_gdate_prev_month - calculate the previous month base on selected calendar,
///           the input is in a Gregorian Date and the output will be in Gregorian
/// \param date GDate Object , a gregorian date
void gnc_gdate_prev_month(GDate *date);


// used in Recurrence

///gnc_date_subtract_days - subtract days from the specefic date,
///         the input date is a gregorian date and the return value will be a gregorian date
/// \param date   GDate Object,
/// \param n_days guint , the number of days that want to subtract from date
void         gnc_date_subtract_days         (GDate       *date,
                                             guint        n_days);
/// gnc_date_add_days - add days to the specefic date,
///         the input date is a gregorian date and the return value will be a gregorian date
/// \param date   GDate Object,
/// \param n_days guint , the number of days that want to added to date
void         gnc_date_add_days              (GDate       *date,
                                            guint        n_days);
/// gnc_date_set_day - set a masked day to the gregorian date
///                 this routin set a day that valid in the
///                 masked date in the Gregorian format date
///                 that passed to this routin
/// \param date  GDate    , a Gregorian Date
/// \param day   GDateDay , a day that valid in Masked Date
void         gnc_date_set_day               (GDate       *date,
                                             GDateDay     day);
/// gnc_date_get_day - get a masked day from the gregorian date
///                 this routin get return the day in Masked
///                 Calendar format
/// \param date  GDate Object , a gregorian date
/// \return       GDateDay    , return the day in masked format
GDateDay     gnc_date_get_day               (const GDate *date);

/// gnc_date_get_days_in_month - get the month in masked calendar
/// \param day      GDateDay  , day part from Gregorian Date
/// \param month    GDateMonth, month part from Gregorian Date
/// \param year     GDateYear , year part from Gregorian Date
/// \return         guint8    , the month in Masked Date format
guint8       gnc_date_get_days_in_month     (GDateDay     day,
                                             GDateMonth   month,
                                             GDateYear    year) G_GNUC_CONST;

///gnc_date_subtract_months - subtract n months from
///         the gregorian date in Masked Calendar format
/// \param date GDate , a gregorian date
/// \param n_months   , n months that should be subtract
void         gnc_date_subtract_months       (GDate       *date,
                                             guint        n_months);

/// gnc_date_add_months - add n months to
///         the gregorian date in Masked Calendar format
/// \param date    GDate , a gregorian date
/// \param n_months guint , n months that should be add
void         gnc_date_add_months            (GDate       *date,
                                             guint        n_months);
/// gnc_date_is_last_of_month - check that the passed date
///                     is last day of month in Masked format
/// \param date GDate    , a gregorian date
/// \return     GBoolean , true if the passed date is last day in masked calendar
gboolean     gnc_date_is_last_of_month      (const GDate *date);

/** @} */

#endif /* GNC_GDATE_UTILS_H */
/** @} */
/** @} */
