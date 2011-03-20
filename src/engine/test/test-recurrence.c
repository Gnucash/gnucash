/* Copyright (C) 2005, Chris Shoemaker <c.shoemaker@cox.net>
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

#include <stdio.h>
#include <stdlib.h>
#include <glib.h>

#include "test-stuff.h"
#include "Recurrence.h"
#include "gnc-engine.h"

static QofBook *book;

static void check_valid(GDate *next, GDate *ref, GDate *start,
                        guint16 mult, PeriodType pt, WeekendAdjust wadj)
{
    gboolean valid;
    gint startToNext;
    /* FIXME: The WeekendAdjust argument is completely ignored for
       now. */

    valid = g_date_valid(next);
    if (pt == PERIOD_ONCE && g_date_compare(start, ref) <= 0)
        do_test(!valid, "incorrectly valid");
    else
        do_test(valid, "incorrectly invalid");

    if (!valid) return;

    do_test(g_date_compare(ref, next) < 0,
            "next date not strictly later than ref date");
    startToNext = g_date_get_julian(next) - g_date_get_julian(start);

    // Phase test
    switch (pt)
    {
    case PERIOD_YEAR:
        do_test((g_date_get_year(next) - g_date_get_year(start)) % mult == 0,
                "year period phase wrong"); // redundant
        mult *= 12;
        // fall-through
    case PERIOD_END_OF_MONTH:
        if (pt == PERIOD_END_OF_MONTH)
            do_test(g_date_is_last_of_month(next), "end of month phase wrong");
        // fall-through
    case PERIOD_LAST_WEEKDAY:
    case PERIOD_NTH_WEEKDAY:
    case PERIOD_MONTH:
    {
        gint monthdiff;
        GDateDay day_start, day_next;

        monthdiff = (g_date_get_month(next) - g_date_get_month(start)) +
                    12 * (g_date_get_year(next) - g_date_get_year(start));
        do_test(monthdiff % mult == 0, "month or year phase wrong");

        if (pt == PERIOD_NTH_WEEKDAY || pt == PERIOD_LAST_WEEKDAY)
        {
            guint sweek, nweek;

            do_test(g_date_get_weekday(next) == g_date_get_weekday(start),
                    "weekday phase wrong");
            sweek = (g_date_get_day(start) - 1) / 7;
            nweek = (g_date_get_day(next) - 1) / 7;

            /* 3 cases: either the weeks agree, OR 'next' didn't have
               5 of the weekday that 'start' did, so it's only the
               4th, OR 'start' didn't have 5 of the weekday that
               'next' does and we want the LAST weekday, so it's the
               5th of that weekday */
            do_test(sweek == nweek ||
                    (sweek == 4 && nweek == 3 && (g_date_get_day(next) + 7) >
                     g_date_get_days_in_month(
                         g_date_get_month(next), g_date_get_year(next))) ||
                    (sweek == 3 && nweek == 4 && (pt == PERIOD_LAST_WEEKDAY)),
                    "week of month phase wrong");

        }
        else
        {
            day_start = g_date_get_day(start);
            day_next = g_date_get_day(next);
            if (day_start < 28)
                do_test(day_start == day_next, "dom don't match");
            else if (pt != PERIOD_END_OF_MONTH)
            {
                // the end of month case was already checked above.  near
                // the end of the month, the days should still agree,
                // unless they can't because of a short month.
                do_test(day_start == day_next || g_date_is_last_of_month(next),
                        "dom don't match and next is not eom");
            }
        }
    }
    break;
    case PERIOD_WEEK:
        mult *= 7;
        // fall-through
    case PERIOD_DAY:
        do_test((startToNext % mult) == 0, "week or day period phase wrong");
        break;
    case PERIOD_ONCE:
        do_test(startToNext == 0, "period once not on start date");
        break;
    default:
        do_test(FALSE, "invalid PeriodType");
    }

}

#define NUM_DATES_TO_TEST 300
#define NUM_DATES_TO_TEST_REF 300
#define NUM_MULT_TO_TEST 10
#define JULIAN_START 2003*365     // years have to be < 10000

/* Mult of zero is usually not valid, but it gets regularized to 1, so
   the effect is just that we end up testing mult of 1 twice, plus the
   regularization. */
static void test_all()
{
    Recurrence r;
    GDate d_start, d_start_reg;
    GDate d_ref, d_next;
    guint16 mult, mult_reg;
    PeriodType pt, pt_reg;
    WeekendAdjust wadj, wadj_reg;
    gint32 j1, j2;
    gint i_ref;

    for (pt = PERIOD_ONCE; pt < NUM_PERIOD_TYPES; pt++)
    {
        for (wadj = WEEKEND_ADJ_NONE; wadj < NUM_WEEKEND_ADJS; wadj++)
        {
            for (j1 = JULIAN_START; j1 < JULIAN_START + NUM_DATES_TO_TEST; j1++)
            {
                g_date_set_julian(&d_start, j1);
                for (i_ref = 0; i_ref < NUM_DATES_TO_TEST_REF; i_ref++)
                {
                    j2 = (guint32) get_random_int_in_range(1, 1 << 19);
                    g_date_set_julian(&d_ref, j2);

                    for (mult = 0; mult < NUM_MULT_TO_TEST; mult++)
                    {
                        recurrenceSet(&r, mult, pt, &d_start, wadj);
                        pt_reg = recurrenceGetPeriodType(&r);
                        d_start_reg = recurrenceGetDate(&r);
                        mult_reg = recurrenceGetMultiplier(&r);
                        wadj_reg = recurrenceGetWeekendAdjust(&r);

                        recurrenceNextInstance(&r, &d_ref, &d_next);
                        check_valid(&d_next, &d_ref, &d_start_reg,
                                    mult_reg, pt_reg, wadj_reg);

                    }
                }
            }
        }
    }
}

static gboolean test_equal(GDate *d1, GDate *d2)
{
    if (!do_test(g_date_compare(d1, d2) == 0, "dates don't match"))
    {
        gchar s1[21];
        gchar s2[21];
        g_date_strftime(s1, 20, "%x", d1);
        g_date_strftime(s2, 20, "%x", d2);

        printf("%s != %s\n", s1, s2);
        return FALSE;
    }
    return TRUE;
}


static void test_specific(PeriodType pt, guint16 mult,
                          GDateMonth sm, GDateDay sd, GDateYear sy,
                          GDateMonth rm, GDateDay rd, GDateYear ry,
                          GDateMonth nm, GDateDay nd, GDateYear ny)
{
    GDate start;
    GDate ref, next, true_next;
    Recurrence r;

    g_date_set_dmy(&start, sd, sm, sy);
    g_date_set_dmy(&ref, rd, rm, ry);
    g_date_set_dmy(&true_next, nd, nm, ny);


    recurrenceSet(&r, mult, pt, &start, WEEKEND_ADJ_NONE);
    recurrenceNextInstance(&r, &ref, &next);

    check_valid(&next, &ref, &start, mult, pt, WEEKEND_ADJ_NONE);
    if (!test_equal(&next, &true_next))
    {
        gchar s1[21], s2[21], s3[21];
        g_date_strftime(s1, 20, "%x", &start);
        g_date_strftime(s2, 20, "%x", &ref);
        g_date_strftime(s3, 20, "%x", &true_next);
        printf("pt = %d; mult = %d; start = %s; ref = %s; true_next = %s\n",
               pt, mult, s1, s2, s3);
    }
}

#if 0
static void test_nth(GDateMonth sm, GDateDay sd, GDateYear sy,
                     GDateMonth nm, GDateDay nd, GDateYear ny,
                     gint diff, PeriodType pt)
{
    GDate start, next;
    gint d;

    g_date_set_dmy(&start, sd, sm, sy);
    g_date_set_dmy(&next, nd, nm, ny);

    d = nth_weekday_compare(&start, &next, pt);
    do_test(d == diff, "nth");
}

static void test_nth_compare()
{
    test_nth(4, 1, 2005,   4, 2, 2005, -1, PERIOD_NTH_WEEKDAY);
    test_nth(4, 1, 2005,   4, 4, 2005, -3, PERIOD_NTH_WEEKDAY);
    test_nth(4, 1, 2005,   4, 7, 2005, -6, PERIOD_NTH_WEEKDAY);
    test_nth(4, 1, 2005,   4, 8, 2005, -7, PERIOD_NTH_WEEKDAY);
    test_nth(4, 1, 2005,   4, 14, 2005, -13, PERIOD_NTH_WEEKDAY);
    test_nth(4, 1, 2005,   4, 30, 2005, -29, PERIOD_NTH_WEEKDAY);
    test_nth(4, 1, 2005,   5, 1, 2005, 5, PERIOD_NTH_WEEKDAY);
    test_nth(4, 1, 2005,   5, 5, 2005, 1, PERIOD_NTH_WEEKDAY);
    test_nth(4, 1, 2005,   5, 6, 2005, 0, PERIOD_NTH_WEEKDAY);
    test_nth(4, 1, 2005,   5, 7, 2005, -1, PERIOD_NTH_WEEKDAY);
    test_nth(4, 1, 2005,   5, 8, 2005, -2, PERIOD_NTH_WEEKDAY);
    test_nth(4, 1, 2005,   5, 21, 2005, -15, PERIOD_NTH_WEEKDAY);


    test_nth(4, 6, 2005,   4, 1, 2005, 5, PERIOD_NTH_WEEKDAY);
    test_nth(4, 6, 2005,   4, 4, 2005, 2, PERIOD_NTH_WEEKDAY);
    test_nth(4, 6, 2005,   4, 6, 2005, 0, PERIOD_NTH_WEEKDAY);
    test_nth(4, 6, 2005,   4, 9, 2005, -3, PERIOD_NTH_WEEKDAY);
    test_nth(4, 6, 2005,   4, 11, 2005, -5, PERIOD_NTH_WEEKDAY);
    test_nth(4, 6, 2005,   4, 13, 2005, -7, PERIOD_NTH_WEEKDAY);
    test_nth(4, 6, 2005,   4, 14, 2005, -8, PERIOD_NTH_WEEKDAY);
    test_nth(4, 6, 2005,   4, 29, 2005, -23, PERIOD_NTH_WEEKDAY);

    test_nth(4, 12, 2005,   4, 1, 2005, 11, PERIOD_NTH_WEEKDAY);
    test_nth(4, 12, 2005,   4, 4, 2005, 8, PERIOD_NTH_WEEKDAY);
    test_nth(4, 12, 2005,   4, 11, 2005, 1, PERIOD_NTH_WEEKDAY);
    test_nth(4, 12, 2005,   4, 12, 2005, 0, PERIOD_NTH_WEEKDAY);
    test_nth(4, 12, 2005,   4, 13, 2005, -1, PERIOD_NTH_WEEKDAY);
    test_nth(4, 12, 2005,   4, 17, 2005, -5, PERIOD_NTH_WEEKDAY);
    test_nth(4, 12, 2005,   4, 19, 2005, -7, PERIOD_NTH_WEEKDAY);
    test_nth(4, 12, 2005,   4, 28, 2005, -16, PERIOD_NTH_WEEKDAY);

    test_nth(4, 29, 2005,   4, 30, 2005, -1, PERIOD_LAST_WEEKDAY);
    test_nth(4, 29, 2005,   5, 1, 2005, 26, PERIOD_LAST_WEEKDAY);
    test_nth(4, 29, 2005,   7, 9, 2005, 20, PERIOD_LAST_WEEKDAY);
    test_nth(4, 29, 2005,   7, 31, 2005, -2, PERIOD_LAST_WEEKDAY);

    test_nth(4, 28, 2005,   4, 30, 2005, -2, PERIOD_LAST_WEEKDAY);
    test_nth(4, 28, 2005,   5, 1, 2005, 25, PERIOD_LAST_WEEKDAY);
    test_nth(4, 28, 2005,   7, 9, 2005, 19, PERIOD_LAST_WEEKDAY);
    test_nth(4, 28, 2005,   7, 31, 2005, -3, PERIOD_LAST_WEEKDAY);
    test_nth(4, 28, 2005,   9, 21, 2005, 8, PERIOD_LAST_WEEKDAY);

}
#endif
static void test_some()
{
    test_specific(PERIOD_NTH_WEEKDAY, 1, 4, 1, 2005,    4, 2, 2005,  5, 6, 2005);
    test_specific(PERIOD_NTH_WEEKDAY, 1, 7, 14, 2005,   11, 15, 2005,  12, 8, 2005);
    test_specific(PERIOD_NTH_WEEKDAY, 1, 7, 14, 2005,   11, 5, 2005,  11, 10, 2005);
    test_specific(PERIOD_NTH_WEEKDAY, 1, 4, 1, 2005,    4, 2, 2005,  5, 6, 2005);
    test_specific(PERIOD_NTH_WEEKDAY, 1, 4, 1, 2005,    4, 2, 2005,  5, 6, 2005);

    test_specific(PERIOD_LAST_WEEKDAY, 1, 4, 29, 2005,    4, 30, 2005,  5, 27, 2005);
    test_specific(PERIOD_LAST_WEEKDAY, 1, 4, 29, 2005,    5, 1, 2005,  5, 27, 2005);
    test_specific(PERIOD_LAST_WEEKDAY, 1, 4, 29, 2005,    7, 9, 2005,  7, 29, 2005);
    test_specific(PERIOD_LAST_WEEKDAY, 1, 4, 29, 2005,    6, 30, 2005,  7, 29, 2005);
    test_specific(PERIOD_LAST_WEEKDAY, 1, 4, 29, 2005,    7, 31, 2005,  8, 26, 2005);

    test_specific(PERIOD_NTH_WEEKDAY, 2, 4, 27, 2005,    4, 27, 2005,  6, 22, 2005);
    //exit(1);
    //return;
    test_specific(PERIOD_YEAR,          3,   9, 8, 838,    6, 30, 1094,  9, 8, 1096);
    test_specific(PERIOD_YEAR,          2,   9, 8, 838,    6, 30, 1094,  9, 8, 1094);
    test_specific(PERIOD_YEAR,          1,   1, 10, 1000,  1, 5, 1002,  1, 10, 1002);
    //return;
    test_specific(PERIOD_MONTH, 1,     1, 12, 1,    2, 6, 1,    2, 12, 1);

    test_specific(PERIOD_MONTH, 1,     1, 12, 1,    2, 12, 1,   3, 12, 1);
    test_specific(PERIOD_MONTH, 1,     1, 12, 1,    2, 20, 1,   3, 12, 1);
    test_specific(PERIOD_MONTH, 1,     1, 30, 1,    2, 28, 1,   3, 30, 1);
    test_specific(PERIOD_MONTH, 1,     1, 30, 1,    2, 27, 1,   2, 28, 1);
    test_specific(PERIOD_MONTH, 1,     2, 28, 1,    3, 30, 1,   4, 28, 1);

    test_specific(PERIOD_END_OF_MONTH, 1,   2, 28, 1,    3, 30, 1,   3, 31, 1);
    test_specific(PERIOD_END_OF_MONTH, 5,   4, 30, 1,    4, 21, 1,  4, 30, 1);
    test_specific(PERIOD_END_OF_MONTH, 5,   2, 28, 1,    5, 21, 1,  7, 31, 1);

    test_specific(PERIOD_YEAR,          7,   6, 8, 199,    9, 10, 1338,  6, 8, 1340);
    test_specific(PERIOD_YEAR,          2,   9, 8, 838,    6, 30, 1094,  9, 8, 1094);

    test_specific(PERIOD_YEAR, 1,    5, 2, 13, 1, 11, 101,   5, 2, 101);
    test_specific(PERIOD_DAY, 7,    4, 1, 2000,    4, 8, 2000,  4, 15, 2000);
}

static void test_use()
{
    Recurrence *r;

    r = g_new(Recurrence, 1);
    do_test(r != NULL, "allocation");
    g_free(r);
}

static void test_main()
{

    book = qof_book_new ();

    test_use();

    test_some();

    test_all();

    qof_book_destroy (book);
}


int
main (int argc, char **argv)
{
    qof_init();

    g_log_set_always_fatal( G_LOG_LEVEL_CRITICAL | G_LOG_LEVEL_WARNING );

#if 0
    set_success_print(TRUE);
#endif

    test_main();

    print_test_results();
    return get_rv();
}
