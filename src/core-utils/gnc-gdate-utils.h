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


/** @name GDate hash table support
    @{ */

/** Compares two GDate*'s for equality; useful for using GDate*'s as
 *  GHashTable keys. */
gint gnc_gdate_equal(gconstpointer gda, gconstpointer gdb);


/** Provides a "hash" of a GDate* value; useful for using GDate*'s as
 *  GHashTable keys. */
guint gnc_gdate_hash( gconstpointer gd );

/** @} */

/** @name GDate to time_t conversions
    @{ */

/** The gnc_timet_get_day_start() routine will take the given time in
 *  GLib GDate format and adjust it to the first second of that day.
 */
time_t gnc_timet_get_day_start_gdate (GDate *date);

/** The gnc_timet_get_day_end() routine will take the given time in
 *  GLib GDate format and adjust it to the last second of that day.
 */
time_t gnc_timet_get_day_end_gdate (GDate *date);

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

/** @} */

#endif /* GNC_GDATE_UTILS_H */
/** @} */
/** @} */
