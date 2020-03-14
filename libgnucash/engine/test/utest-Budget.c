/********************************************************************
 * utest-Budget.c: GLib g_test test suite for gnc-budget.c.         *
 * Copyright 2012 Phil Longstaff <phil.longstaff@yahoo.ca>          *
 *                                                                  *
 * This program is free software; you can redistribute it and/or    *
 * modify it under the terms of the GNU General Public License as   *
 * published by the Free Software Foundation; either version 2 of   *
 * the License, or (at your option) any later version.              *
 *                                                                  *
 * This program is distributed in the hope that it will be useful,  *
 * but WITHOUT ANY WARRANTY; without even the implied warranty of   *
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the    *
 * GNU General Public License for more details.                     *
 *                                                                  *
 * You should have received a copy of the GNU General Public License*
 * along with this program; if not, contact:                        *
 *                                                                  *
 * Free Software Foundation           Voice:  +1-617-542-5942       *
 * 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652       *
 * Boston, MA  02110-1301,  USA       gnu@gnu.org                   *
********************************************************************/
#include <config.h>
#include <string.h>
#include <glib.h>
#include <unittest-support.h>
#include <gnc-event.h>
/* Add specific headers for this class */
#include "gnc-budget.h"

static const gchar *suitename = "/engine/Budget";
void test_suite_budget(void);

static void
test_gnc_set_budget_name()
{
    QofBook *book = qof_book_new();
    GncBudget* budget = gnc_budget_new(book);

    g_assert(strcmp(gnc_budget_get_name(budget), "Unnamed Budget") == 0);
    gnc_budget_set_name(budget, "New Budget");
    g_assert(strcmp(gnc_budget_get_name(budget), "New Budget") == 0);

    gnc_budget_destroy(budget);
}

static void
test_gnc_set_budget_description()
{
    QofBook *book = qof_book_new();
    GncBudget* budget = gnc_budget_new(book);

    g_assert(strcmp(gnc_budget_get_description(budget), "") == 0);
    gnc_budget_set_description(budget, "New Budget");
    g_assert(strcmp(gnc_budget_get_description(budget), "New Budget") == 0);

    gnc_budget_destroy(budget);
}

static void
test_gnc_set_budget_num_periods()
{
    QofBook *book = qof_book_new();
    GncBudget* budget = gnc_budget_new(book);

    g_assert_cmpint(gnc_budget_get_num_periods(budget), ==, 12);
    gnc_budget_set_num_periods(budget, 20);
    g_assert_cmpint(gnc_budget_get_num_periods(budget), ==, 20);

    gnc_budget_destroy(budget);
    qof_book_destroy(book);
}

static void
test_gnc_set_budget_recurrence()
{
    QofBook *book = qof_book_new();
    GncBudget* budget = gnc_budget_new(book);
    Recurrence new_r;
    GDate start_date;
    const Recurrence* r;
    GDate period_start;
    GDate period_end;
    int i;
    typedef struct
    {
        GDateMonth month;
        int end_day;
    } PeriodInfo;
    PeriodInfo period_info[] = { { G_DATE_JANUARY, 31 }, { G_DATE_FEBRUARY, 29 }, { G_DATE_MARCH, 31 }, { G_DATE_APRIL, 30 }, { G_DATE_MAY, 31 }, { G_DATE_JUNE, 30 },
        { G_DATE_JULY, 31 }, { G_DATE_AUGUST, 31 }, { G_DATE_SEPTEMBER, 30 }, { G_DATE_OCTOBER, 31 }, { G_DATE_NOVEMBER, 30 }, { G_DATE_DECEMBER, 31 }
    };

    r = gnc_budget_get_recurrence(budget);
    g_assert_cmpint(r->ptype, ==, PERIOD_MONTH);
    g_assert_cmpint(r->mult, ==, 1);
    g_assert_cmpint(r->wadj, ==, WEEKEND_ADJ_NONE);

    g_date_set_dmy(&start_date, 1, G_DATE_JANUARY, 2012);
    recurrenceSet(&new_r, 1, PERIOD_MONTH, &start_date, WEEKEND_ADJ_NONE);
    gnc_budget_set_recurrence(budget, &new_r);

    r = gnc_budget_get_recurrence(budget);
    g_assert_cmpint(r->ptype, ==, PERIOD_MONTH);
    g_assert_cmpint(r->mult, ==, 1);
    g_assert_cmpint(r->wadj, ==, WEEKEND_ADJ_NONE);

    for (i = 0; i < 12; ++i)
    {
        period_start = time64_to_gdate(gnc_budget_get_period_start_date(budget, i));
        period_end = time64_to_gdate(gnc_budget_get_period_end_date(budget, i));

        g_assert_cmpint(g_date_get_day(&period_start), ==, 1);
        g_assert_cmpint(g_date_get_day(&period_end), ==, period_info[i].end_day);
        g_assert_cmpint(g_date_get_month(&period_start), ==, period_info[i].month);
        g_assert_cmpint(g_date_get_month(&period_end), ==, period_info[i].month);
        g_assert_cmpint(g_date_get_year(&period_start), ==, 2012);
        g_assert_cmpint(g_date_get_year(&period_end), ==, 2012);
    }

    gnc_budget_destroy(budget);
    qof_book_destroy(book);
}

static void
test_gnc_set_budget_account_period_value()
{
    QofBook *book = qof_book_new();
    GncBudget* budget = gnc_budget_new(book);
    Account *acc;
    gnc_numeric val;

    guint log_level = G_LOG_LEVEL_WARNING | G_LOG_FLAG_FATAL;
    gchar *log_domain = "gnc.engine";
    gchar *msg = "[gnc_budget_set_account_period_value()] Period 12 does not exist";
    guint hdlr;
    TestErrorStruct check = { log_level, log_domain, msg, 0 };
    GLogFunc oldlogger;

    acc = gnc_account_create_root(book);

    g_assert(!gnc_budget_is_account_period_value_set(budget, acc, 0));
    gnc_budget_set_account_period_value(budget, acc, 0, gnc_numeric_create(100,1));
    g_assert(gnc_budget_is_account_period_value_set(budget, acc, 0));
    val = gnc_budget_get_account_period_value(budget, acc, 0);
    g_assert (gnc_numeric_equal (val, gnc_numeric_create (100, 1)));

    /* Budget has 12 periods by default, numbered from 0 to 11. Setting
     * period 12 should throw an error. */
    oldlogger = g_log_set_default_handler ((GLogFunc)test_null_handler, &check);
    g_test_log_set_fatal_handler ((GTestLogFatalFunc)test_checked_handler, &check);
    gnc_budget_set_account_period_value(budget, acc, 12, gnc_numeric_create(100,1));
    g_assert_cmpint (check.hits, ==, 1);
    g_log_set_default_handler (oldlogger, NULL);

    gnc_budget_destroy(budget);
    qof_book_destroy(book);
}

void
test_suite_budget(void)
{

    GNC_TEST_ADD_FUNC(suitename, "gnc_budget_set_name()", test_gnc_set_budget_name);
    GNC_TEST_ADD_FUNC(suitename, "gnc_budget_set_description()", test_gnc_set_budget_description);
    GNC_TEST_ADD_FUNC(suitename, "gnc_budget_set_num_periods()", test_gnc_set_budget_num_periods);
    GNC_TEST_ADD_FUNC(suitename, "gnc_budget_set_recurrence()", test_gnc_set_budget_recurrence);
    GNC_TEST_ADD_FUNC(suitename, "gnc_budget_set_account_period_value()", test_gnc_set_budget_account_period_value);

}
