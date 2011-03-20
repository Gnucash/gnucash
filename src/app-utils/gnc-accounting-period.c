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
    dependancy from the src/engine directory to the src/core-utils
    directory that doesn't currently exist.  Since that might be a
    problem for CashUtils, the app-file directory was chosen.
*/

#include "config.h"
#include <string.h>
#include "gnc-accounting-period.h"
#include "gnc-gdate-utils.h"
#include "gnc-date.h"
#include "gnc-gconf-utils.h"
#include "qof.h"
#include "gnc-ui-util.h"

/* TODO: This should probably be changed eventually. */
#define GCONF_SECTION    "window/pages/account_tree/summary"
#define KEY_START_CHOICE "start_choice"
#define KEY_START_DATE   "start_date"
#define KEY_START_PERIOD "start_period"
#define KEY_END_CHOICE 	 "end_choice"
#define KEY_END_DATE   	 "end_date"
#define KEY_END_PERIOD 	 "end_period"

static time_t
lookup_start_date_option(const gchar *section,
                         const gchar *key_choice,
                         const gchar *key_absolute,
                         const gchar *key_relative,
                         GDate *fy_end)
{
    gchar *choice;
    time_t time;
    int which;

    choice = gnc_gconf_get_string(section, key_choice, NULL);
    if (choice && strcmp(choice, "absolute") == 0)
    {
        time = gnc_gconf_get_int(section, key_absolute, NULL);
    }
    else
    {
        which = gnc_gconf_get_int(section, key_relative, NULL);
        time = gnc_accounting_period_start_timet(which, fy_end, NULL);
    }
    g_free(choice);
    /* we will need the balance of the last transaction before the start
       date, so subtract 1 from start date */
    /* CAS: we don't actually do what this comment says.  I think that's
       because a bug in the engine has been fixed. */
    return time;
}


static time_t
lookup_end_date_option(const gchar *section,
                       const gchar *key_choice,
                       const gchar *key_absolute,
                       const gchar *key_relative,
                       GDate *fy_end)
{
    gchar *choice;
    time_t time;
    int which;

    choice = gnc_gconf_get_string(section, key_choice, NULL);
    if (choice && strcmp(choice, "absolute") == 0)
    {
        time = gnc_gconf_get_int(section, key_absolute, NULL);
        time = gnc_timet_get_day_end(time);
    }
    else
    {
        which = gnc_gconf_get_int(section, key_relative, NULL);
        time = gnc_accounting_period_end_timet(which, fy_end, NULL);
    }
    g_free(choice);
    if (time == 0)
        time = -1;
    return time;
}

static GDate *
get_fy_end(void)
{
    QofBook *book;
    KvpFrame *book_frame;
    gint64 month, day;

    book = gnc_get_current_book();
    book_frame = qof_book_get_slots(book);
    month = kvp_frame_get_gint64(book_frame, "/book/fyear_end/month");
    day = kvp_frame_get_gint64(book_frame, "/book/fyear_end/day");
    if (g_date_valid_dmy(day, month, 2005 /* not leap year */))
        return g_date_new_dmy(day, month, G_DATE_BAD_YEAR);
    return NULL;
}

time_t
gnc_accounting_period_fiscal_start(void)
{
    time_t t;
    GDate *fy_end = get_fy_end();
    t = lookup_start_date_option(GCONF_SECTION, KEY_START_CHOICE,
                                 KEY_START_DATE, KEY_START_PERIOD, fy_end);
    if (fy_end)
        g_date_free(fy_end);
    return t;
}

time_t
gnc_accounting_period_fiscal_end(void)
{
    time_t t;
    GDate *fy_end = get_fy_end();

    t = lookup_end_date_option(GCONF_SECTION, KEY_END_CHOICE,
                               KEY_END_DATE, KEY_END_PERIOD, fy_end);
    if (fy_end)
        g_date_free(fy_end);
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
        date = g_date_new_dmy(g_date_get_day(contains),
                              g_date_get_month(contains),
                              g_date_get_year(contains));
    }
    else
    {
        date = g_date_new();
        g_date_set_time_t(date, time(NULL));
    }

    switch (which)
    {
    default:
        g_message("Undefined relative time constant %d", which);
        g_date_free(date);
        return NULL;

    case GNC_ACCOUNTING_PERIOD_TODAY:
        /* Already have today's date */
        break;

    case GNC_ACCOUNTING_PERIOD_MONTH:
        gnc_gdate_set_month_start(date);
        break;

    case GNC_ACCOUNTING_PERIOD_MONTH_PREV:
        gnc_gdate_set_prev_month_start(date);
        break;

    case GNC_ACCOUNTING_PERIOD_QUARTER:
        gnc_gdate_set_quarter_start(date);
        break;

    case GNC_ACCOUNTING_PERIOD_QUARTER_PREV:
        gnc_gdate_set_prev_quarter_start(date);
        break;

    case GNC_ACCOUNTING_PERIOD_CYEAR:
        gnc_gdate_set_year_start(date);
        break;

    case GNC_ACCOUNTING_PERIOD_CYEAR_PREV:
        gnc_gdate_set_prev_year_start(date);
        break;

    case GNC_ACCOUNTING_PERIOD_FYEAR:
        if (fy_end == NULL)
        {
            g_message("Request for fisal year value but no fiscal year end value provided.");
            g_date_free(date);
            return NULL;
        }
        gnc_gdate_set_fiscal_year_start(date, fy_end);
        break;

    case GNC_ACCOUNTING_PERIOD_FYEAR_PREV:
        if (fy_end == NULL)
        {
            g_message("Request for fisal year value but no fiscal year end value provided.");
            g_date_free(date);
            return NULL;
        }
        gnc_gdate_set_prev_fiscal_year_start(date, fy_end);
        break;
    }
    return date;
}

time_t
gnc_accounting_period_start_timet (GncAccountingPeriod which,
                                   const GDate *fy_end,
                                   const GDate *contains)
{
    GDate *date;
    time_t secs;

    date = gnc_accounting_period_start_gdate(which, fy_end, contains);
    if (!date)
        return 0;

    secs = gnc_timet_get_day_start_gdate(date);
    g_date_free(date);
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
        date = g_date_new_dmy(g_date_get_day(contains),
                              g_date_get_month(contains),
                              g_date_get_year(contains));
    }
    else
    {
        date = g_date_new();
        g_date_set_time_t(date, time(NULL));
    }

    switch (which)
    {
    default:
        g_message("Undefined relative time constant %d", which);
        g_date_free(date);
        return 0;

    case GNC_ACCOUNTING_PERIOD_TODAY:
        /* Already have today's date */
        break;

    case GNC_ACCOUNTING_PERIOD_MONTH:
        gnc_gdate_set_month_end(date);
        break;

    case GNC_ACCOUNTING_PERIOD_MONTH_PREV:
        gnc_gdate_set_prev_month_end(date);
        break;

    case GNC_ACCOUNTING_PERIOD_QUARTER:
        gnc_gdate_set_quarter_end(date);
        break;

    case GNC_ACCOUNTING_PERIOD_QUARTER_PREV:
        gnc_gdate_set_prev_quarter_end(date);
        break;

    case GNC_ACCOUNTING_PERIOD_CYEAR:
        gnc_gdate_set_year_end(date);
        break;

    case GNC_ACCOUNTING_PERIOD_CYEAR_PREV:
        gnc_gdate_set_prev_year_end(date);
        break;

    case GNC_ACCOUNTING_PERIOD_FYEAR:
        if (fy_end == NULL)
        {
            g_message("Request for fisal year value but no fiscal year end value provided.");
            g_date_free(date);
            return 0;
        }
        gnc_gdate_set_fiscal_year_end(date, fy_end);
        break;

    case GNC_ACCOUNTING_PERIOD_FYEAR_PREV:
        if (fy_end == NULL)
        {
            g_message("Request for fisal year value but no fiscal year end value provided.");
            g_date_free(date);
            return 0;
        }
        gnc_gdate_set_prev_fiscal_year_end(date, fy_end);
        break;
    }

    return date;
}

time_t
gnc_accounting_period_end_timet (GncAccountingPeriod which,
                                 const GDate *fy_end,
                                 const GDate *contains)
{
    GDate *date;
    time_t secs;

    date = gnc_accounting_period_end_gdate(which, fy_end, contains);
    if (!date)
        return 0;

    secs = gnc_timet_get_day_end_gdate(date);
    g_date_free(date);
    return secs ;
}


/** @} */
