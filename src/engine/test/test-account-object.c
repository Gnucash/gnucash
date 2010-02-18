/***************************************************************************
 *            test-account-object.c
 *
 *  Copyright (C) 2007 David Hampton <hampton@employees.org>
 ****************************************************************************/
/*
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 *  02110-1301, USA.
 */
/**
 * @file test-account-object.c
 * @brief Minimal test of reading/writing account parameters
 * @author David Hampton <hampton@employees.org>
 */

#include "config.h"
#include <unistd.h>
#include <glib.h>
#include "qof.h"
#include "Account.h"
#include "cashobjects.h"
#include "test-stuff.h"
#include "test-engine-stuff.h"

static void
run_test (void)
{
    QofSession *sess;
    QofBook *book;
    Account *acc;
    gnc_numeric *start, *end, end2, delta, zero, five;

    sess = get_random_session ();
    book = qof_session_get_book (sess);
    do_test ((NULL != book), "create random data");
    acc = get_random_account(book);

    /*****/

    g_object_get(acc, "start-balance", &start, "end-balance", &end, NULL);
    end2 = xaccAccountGetBalance(acc);
    delta = gnc_numeric_sub(*end, *start, GNC_DENOM_AUTO, GNC_HOW_DENOM_FIXED);

    do_test (gnc_numeric_zero_p(*start), "start balance is zero");
    do_test (gnc_numeric_zero_p(*end), "end balance is zero");
    do_test (gnc_numeric_zero_p(delta), "delta is zero");
    do_test (gnc_numeric_zero_p(end2), "end2 balance is zero");

    /*****/

    five = gnc_numeric_create(5, 1);
    g_object_set(acc, "start-balance", &five, NULL);
    xaccAccountRecomputeBalance(acc);
    g_object_get(acc, "start-balance", &start, "end-balance", &end, NULL);
    end2 = xaccAccountGetBalance(acc);

    delta = gnc_numeric_sub(*end, five, GNC_DENOM_AUTO, GNC_HOW_DENOM_FIXED);
    do_test (gnc_numeric_zero_p(delta), "end balance matches");
    delta = gnc_numeric_sub(end2, five, GNC_DENOM_AUTO, GNC_HOW_DENOM_FIXED);
    do_test (gnc_numeric_zero_p(delta), "end2 balance matches");

    /*****/

    qof_session_end (sess);

}

int
main (int argc, char **argv)
{
    gint i;

    qof_init();
    if (!cashobjects_register())
        exit(1);

    /* Any tests that cause an error or warning to be printed
     * automatically fail! */
    g_log_set_always_fatal( G_LOG_LEVEL_CRITICAL | G_LOG_LEVEL_WARNING );
    /* Set up a reproducible test-case */
    srand(0);

    /* Run the tests */
    run_test ();

    print_test_results();

    qof_close();
    return get_rv();
}
