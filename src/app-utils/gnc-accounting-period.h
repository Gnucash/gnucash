/*
 * gnc-accounting-period.h --
 *
 * Copyright (c) 2005 David Hampton <hampton@employees.org>
 * All rights reserved.
 *
 * GnuCash is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Library General Public License as
 * published by the Free Software Foundation; either version 2 of
 * the License, or (at your option) any later version.
 *
 * Gnucash is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, contact:
 *
 * Free Software Foundation           Voice:  +1-617-542-5942
 * 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652
 * Boston, MA  02110-1301,  USA       gnu@gnu.org
 */

/** @addtogroup GUI
    @{ */
/** @file gnc-accounting-period.h
    @brief General utilities for dealing with accounting periods.
    @author David Hampton <hampton@employees.org>

    These are general utility functions for specifying an accounting
    period and converting it to a value usable by the gnucash engine.
    The choice of src/app-utils is arbitrary as these utilities don't
    fit well anywhere else.  They are at a higher level than a GDate,
    so they don't fit in src/core-utils/gnc-gdate-utils.c.  They don't
    operate on engine data structures, so they don't belong in
    src/engine/Period.c.  Putting them into src/engine/gnc-date.c
    would be the best place for them, but then that creates a new
    dependancy from the src/engine directory to the src/core-utils
    directory that doesn't currently exist.  Since that might be a
    problem for CashUtils, the app-file directory was chosen.
*/

#ifndef GNC_ACCOUNTING_PERIOD_H
#define GNC_ACCOUNTING_PERIOD_H

#include <glib.h>
#include "glib-compat.h"
#include <time.h>

/**
 * This specifies a time interval.
 */
typedef enum
{
    GNC_ACCOUNTING_PERIOD_TODAY,
    GNC_ACCOUNTING_PERIOD_MONTH,
    GNC_ACCOUNTING_PERIOD_MONTH_PREV,
    GNC_ACCOUNTING_PERIOD_QUARTER,
    GNC_ACCOUNTING_PERIOD_QUARTER_PREV,
    GNC_ACCOUNTING_PERIOD_CYEAR,
    GNC_ACCOUNTING_PERIOD_CYEAR_PREV,
    GNC_ACCOUNTING_PERIOD_CYEAR_LAST,

    GNC_ACCOUNTING_PERIOD_FYEAR = GNC_ACCOUNTING_PERIOD_CYEAR_LAST,
    GNC_ACCOUNTING_PERIOD_FYEAR_PREV,
    GNC_ACCOUNTING_PERIOD_FYEAR_LAST,
    GNC_ACCOUNTING_PERIOD_LAST = GNC_ACCOUNTING_PERIOD_FYEAR_LAST,
} GncAccountingPeriod;


/** \name Accounting Periods */
/** @{ */

/** This function returns the starting date for an accounting period.
 *  The date will be computed based upon the type of accounting
 *  interval requested, an optional fiscal year end value, and an
 *  optional time value.
 *
 *  @param which An enum specifying the type of accounting period.
 *
 *  @param fy_end This argument specifies the month and day of the
 *  fiscal year end.  If the accounting period specified in the
 *  'which' parameter is not a fiscal accounting period, this variable
 *  is ignored and may be NULL.  Note: the year field of this argument
 *  is always ignored.
 *
 *  @param contains This argument specifies the origin time value used
 *  by the calculations in this function.  If this value is NULL, the
 *  origin will be the current time.
 *
 *  @return The starting day of the specified time interval, as a
 *  GDate. */
GDate *gnc_accounting_period_start_gdate (GncAccountingPeriod which,
        const GDate *fy_end,
        const GDate *contains);


/** This function returns the starting time for an accounting period.
 *  The time will be computed based upon the type of accounting
 *  interval requested, an optional fiscal year end value, and an
 *  optional time value.
 *
 *  @param which An enum specifying the type of accounting period.
 *
 *  @param fy_end This argument specifies the month and day of the
 *  fiscal year end.  If the accounting period specified in the
 *  'which' parameter is not a fiscal accounting period, this variable
 *  is ignored and may be NULL.  Note: the year field of this argument
 *  is always ignored.
 *
 *  @param contains This argument specifies the origin time value used
 *  by the calculations in this function.  If this value is NULL, the
 *  origin will be the current time.
 *
 *  @return The starting second of the specified time interval, based
 *  on a zero value of January 1st, 1970. */
time_t gnc_accounting_period_start_timet (GncAccountingPeriod which,
        const GDate *fy_end,
        const GDate *contains);


/** This function returns the ending date for an accounting period.
 *  The date will be computed based upon the type of accounting
 *  interval requested, an optional fiscal year end value, and an
 *  optional time value.
 *
 *  @param which An enum specifying the type of accounting period.
 *
 *  @param fy_end This argument specifies the month and day of the
 *  fiscal year end.  If the accounting period specified in the
 *  'which' parameter is not a fiscal accounting period, this variable
 *  is ignored and may be NULL.  Note: the year field of this argument
 *  is always ignored.
 *
 *  @param contains This argument specifies the origin time value used
 *  by the calculations in this function.  If this value is NULL, the
 *  origin will be the current time.
 *
 *  @return The final day of the specified time interval, as a
 *  GDate. */
GDate *gnc_accounting_period_end_gdate (GncAccountingPeriod which,
                                        const GDate *fy_end,
                                        const GDate *contains);


/** This function returns the ending time for an accounting period.
 *  The time will be computed based upon the type of accounting
 *  interval requested, an optional fiscal year end value, and an
 *  optional time value.
 *
 *  @param which An enum specifying the type of accounting period.
 *
 *  @param fy_end This argument specifies the month and day of the
 *  fiscal year end.  If the accounting period specified in the
 *  'which' parameter is not a fiscal accounting period, this variable
 *  is ignored and may be NULL.  Note: the year field of this argument
 *  is always ignored.
 *
 *  @param contains This argument specifies the origin time value used
 *  by the calculations in this function.  If this value is NULL, the
 *  origin will be the current time.
 *
 *  @return The ending second of the specified time interval, based
 *  on a zero value of January 1st, 1970. */
time_t gnc_accounting_period_end_timet (GncAccountingPeriod which,
                                        const GDate *fy_end,
                                        const GDate *contains);


/* Get the fiscal accounting period from the preferences and return
   the start and end times. */
time_t gnc_accounting_period_fiscal_start(void);
time_t gnc_accounting_period_fiscal_end(void);

/** @} */

#endif /* GNC_ACCOUNTING_PERIOD_H */

/** @} */

