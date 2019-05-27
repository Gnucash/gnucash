/*
 * gnc-accounting-period.c --
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
/** @file gnc-accounting-period.c
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
    dependency from the src/engine directory to the src/core-utils
    directory that doesn't currently exist.  Since that might be a
    problem for CashUtils, the app-file directory was chosen.
*/

#include <config.h>
#include <string.h>
#include "gnc-accounting-period.h"
#include "gnc-date.h"
#include "gnc-prefs.h"
#include "qof.h"
#include "gnc-ui-util.h"

static time64 gnc_accounting_period_start_time64 (GncAccountingPeriod which,
                                                  const GDate *fy_end,
                                                  const GDate *contains);
static time64 gnc_accounting_period_end_time64 (GncAccountingPeriod which,
                                                const GDate *fy_end,
                                                const GDate *contains);

static time64
lookup_start_date_option (GDate *fy_end)
{
    time64 time;
    int which;

    if (gnc_prefs_get_bool (GNC_PREFS_GROUP_ACCT_SUMMARY, GNC_PREF_START_CHOICE_ABS))
        time = gnc_time64_get_day_start (gnc_prefs_get_int64
                                        (GNC_PREFS_GROUP_ACCT_SUMMARY, GNC_PREF_START_DATE));
    else
    {
        which = gnc_prefs_get_int (GNC_PREFS_GROUP_ACCT_SUMMARY, GNC_PREF_START_PERIOD);
        time = gnc_accounting_period_start_time64 (which, fy_end, NULL);
    }
    /* we will need the balance of the last transaction before the start
       date, so subtract 1 from start date */
    /* CAS: we don't actually do what this comment says.  I think that's
       because a bug in the engine has been fixed. */
    return time;
}

static time64
lookup_end_date_option (GDate *fy_end)
{
    time64 time;
    int which;

    if (gnc_prefs_get_bool (GNC_PREFS_GROUP_ACCT_SUMMARY, GNC_PREF_END_CHOICE_ABS))
        time = gnc_time64_get_day_end (gnc_prefs_get_int64
                                      (GNC_PREFS_GROUP_ACCT_SUMMARY, GNC_PREF_END_DATE));
    else
    {
        which = gnc_prefs_get_int (GNC_PREFS_GROUP_ACCT_SUMMARY, GNC_PREF_END_PERIOD);
        time = gnc_accounting_period_end_time64 (which, fy_end, NULL);
    }
    if (time == 0)
        time = -1;
    return time;
}

static GDate *
get_fy_end (void)
{
    QofBook *book;
    GDate *date = NULL;

    book = gnc_get_current_book();
    qof_instance_get (QOF_INSTANCE (book), "fy-end", &date, NULL);
    return date;
}

time64
gnc_accounting_period_fiscal_start (void)
{
    time64 t;
    GDate *fy_end = get_fy_end();
    t = lookup_start_date_option (fy_end);
    if (fy_end)
        g_date_free (fy_end);
    return t;
}

time64
gnc_accounting_period_fiscal_end (void)
{
    time64 t;
    GDate *fy_end = get_fy_end();

    t = lookup_end_date_option (fy_end);
    if (fy_end)
        g_date_free (fy_end);
    return t;
}

GDate *
gnc_accounting_period_start_gdate (GncAccountingPeriod which,
                                   const GDate *fy_end,
                                   const GDate *contains)
{
    GDate *date;

    if (contains)
    {
        date = g_date_new_dmy (g_date_get_day (contains),
                               g_date_get_month (contains),
                               g_date_get_year (contains));
    }
    else
    {
        date = g_date_new ();
        gnc_gdate_set_today (date);
    }

    switch (which)
    {
    default:
        g_message ("Undefined relative time constant %d", which);
        g_date_free (date);
        return NULL;

    case GNC_ACCOUNTING_PERIOD_TODAY:
        /* Already have today's date */
        break;

    case GNC_ACCOUNTING_PERIOD_MONTH:
        gnc_gdate_set_month_start (date);
        break;

    case GNC_ACCOUNTING_PERIOD_MONTH_PREV:
        gnc_gdate_set_prev_month_start (date);
        break;

    case GNC_ACCOUNTING_PERIOD_QUARTER:
        gnc_gdate_set_quarter_start (date);
        break;

    case GNC_ACCOUNTING_PERIOD_QUARTER_PREV:
        gnc_gdate_set_prev_quarter_start (date);
        break;

    case GNC_ACCOUNTING_PERIOD_CYEAR:
        gnc_gdate_set_year_start (date);
        break;

    case GNC_ACCOUNTING_PERIOD_CYEAR_PREV:
        gnc_gdate_set_prev_year_start (date);
        break;

    case GNC_ACCOUNTING_PERIOD_FYEAR:
        if (fy_end == NULL)
        {
            g_message ("Request for fisal year value but no fiscal year end value provided.");
            g_date_free (date);
            return NULL;
        }
        gnc_gdate_set_fiscal_year_start (date, fy_end);
        break;

    case GNC_ACCOUNTING_PERIOD_FYEAR_PREV:
        if (fy_end == NULL)
        {
            g_message ("Request for fisal year value but no fiscal year end value provided.");
            g_date_free (date);
            return NULL;
        }
        gnc_gdate_set_prev_fiscal_year_start (date, fy_end);
        break;
    }
    return date;
}

static time64
gnc_accounting_period_start_time64 (GncAccountingPeriod which,
                                    const GDate *fy_end,
                                    const GDate *contains)
{
    GDate *date;
    time64 secs;

    date = gnc_accounting_period_start_gdate (which, fy_end, contains);
    if (!date)
        return 0;

    secs = gnc_time64_get_day_start_gdate (date);
    g_date_free (date);
    return secs;
}

GDate *
gnc_accounting_period_end_gdate (GncAccountingPeriod which,
                                 const GDate *fy_end,
                                 const GDate *contains)
{
    GDate *date;

    if (contains)
    {
        date = g_date_new_dmy (g_date_get_day (contains),
                               g_date_get_month (contains),
                               g_date_get_year (contains));
    }
    else
    {
        date = g_date_new ();
        gnc_gdate_set_today (date);
    }

    switch (which)
    {
    default:
        g_message ("Undefined relative time constant %d", which);
        g_date_free (date);
        return 0;

    case GNC_ACCOUNTING_PERIOD_TODAY:
        /* Already have today's date */
        break;

    case GNC_ACCOUNTING_PERIOD_MONTH:
        gnc_gdate_set_month_end (date);
        break;

    case GNC_ACCOUNTING_PERIOD_MONTH_PREV:
        gnc_gdate_set_prev_month_end (date);
        break;

    case GNC_ACCOUNTING_PERIOD_QUARTER:
        gnc_gdate_set_quarter_end (date);
        break;

    case GNC_ACCOUNTING_PERIOD_QUARTER_PREV:
        gnc_gdate_set_prev_quarter_end (date);
        break;

    case GNC_ACCOUNTING_PERIOD_CYEAR:
        gnc_gdate_set_year_end (date);
        break;

    case GNC_ACCOUNTING_PERIOD_CYEAR_PREV:
        gnc_gdate_set_prev_year_end (date);
        break;

    case GNC_ACCOUNTING_PERIOD_FYEAR:
        if (fy_end == NULL)
        {
            g_message ("Request for fisal year value but no fiscal year end value provided.");
            g_date_free (date);
            return 0;
        }
        gnc_gdate_set_fiscal_year_end (date, fy_end);
        break;

    case GNC_ACCOUNTING_PERIOD_FYEAR_PREV:
        if (fy_end == NULL)
        {
            g_message ("Request for fisal year value but no fiscal year end value provided.");
            g_date_free (date);
            return 0;
        }
        gnc_gdate_set_prev_fiscal_year_end (date, fy_end);
        break;
    }

    return date;
}

static time64
gnc_accounting_period_end_time64 (GncAccountingPeriod which,
                                  const GDate *fy_end,
                                  const GDate *contains)
{
    GDate *date;
    time64 secs;

    date = gnc_accounting_period_end_gdate (which, fy_end, contains);
    if (!date)
        return 0;

    secs = gnc_time64_get_day_end_gdate (date);
    g_date_free (date);
    return secs ;
}

/** @} */
