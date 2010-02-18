/***************************************************************************
 *            test-lots.c
 *
 *  Copyright (C) 2003 Linas Vepstas <linas@linas.org>
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
 * @file test-lots.c
 * @brief Minimal test to see if automatic lot scrubbing works.
 * @author Linas Vepstas <linas@linas.org>
 */

#include "config.h"
#include <ctype.h>
#include <glib.h>
#include "qof.h"
#include "Account.h"
#include "Scrub3.h"
#include "cashobjects.h"
#include "test-stuff.h"
#include "test-engine-stuff.h"
#include "Transaction.h"

static gint transaction_num = 720;
static gint	max_iterate = 30;

static void
run_test (void)
{
    QofSession *sess;
    QofBook *book;
    Account *root;

    /* --------------------------------------------------------- */
    /* In the first test, we will merely try to see if we can run
     * without crashing.  We don't check to see if data is good. */
    sess = get_random_session ();
    book = qof_session_get_book (sess);
    do_test ((NULL != book), "create random data");

    add_random_transactions_to_book (book, transaction_num);

    root = gnc_book_get_root_account (book);
    xaccAccountTreeScrubLots (root);

    /* --------------------------------------------------------- */
    /* In the second test, we create an account with unrealized gains,
     * and see if that gets fixed correctly, with the correct balances,
     * and etc.
     * XXX not implemented
     */
    success ("automatic lot scrubbing lightly tested and seem to work");
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
    /* Iterate the test a number of times */
    for (i = 0; i < max_iterate; i++)
    {
        fprintf(stdout, " Lots: %d of %d paired tests . . . \r",
                (i + 1) * 2, max_iterate * 2);
        fflush(stdout);
        run_test ();
    }
    /* 'erase' the recurring tag line with dummy spaces. */
    fprintf(stdout, "Lots: Test series complete.         \n");
    fflush(stdout);
    print_test_results();

    qof_close();
    return get_rv();
}
