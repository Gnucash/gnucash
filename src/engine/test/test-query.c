/***************************************************************************
 *            test-query.c
 *
 *  Tue Sep 27 19:12:41 2005
 *  Copyright  2005 GnuCash team
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

#include "config.h"
#include <glib.h>
#include "qof.h"
#include "cashobjects.h"
#include "Transaction.h"
#include "TransLog.h"
#include "gnc-engine.h"
#include "test-engine-stuff.h"
#include "test-stuff.h"

static int
test_trans_query (Transaction *trans, gpointer data)
{
    QofBook *book = data;
    GList *list;
    Query *q;

    q = make_trans_query (trans, ALL_QT);
    xaccQuerySetBook (q, book);

    list = xaccQueryGetTransactions (q, QUERY_TXN_MATCH_ANY);
    if (g_list_length (list) != 1)
    {
        failure_args ("test number returned", __FILE__, __LINE__,
                      "number of matching transactions %d not 1",
                      g_list_length (list));
        g_list_free (list);
        return 13;
    }

    if (list->data != trans)
    {
        failure ("matching transaction is wrong");
        g_list_free (list);
        return 13;
    }

    success ("found right transaction");
    xaccFreeQuery (q);
    g_list_free (list);

    return 0;
}

static void
run_test (void)
{
    QofSession *session;
    Account *root;
    QofBook *book;

    session = get_random_session ();
    book = qof_session_get_book (session);
    root = gnc_book_get_root_account (book);

    add_random_transactions_to_book (book, 20);

    xaccAccountTreeForEachTransaction (root, test_trans_query, book);

    qof_session_end (session);
}

int
main (int argc, char **argv)
{
    int i;

    qof_init();
    g_log_set_always_fatal( G_LOG_LEVEL_CRITICAL | G_LOG_LEVEL_WARNING );

    xaccLogDisable ();

    /* Always start from the same random seed so we fail consistently */
    srand(0);
    if (!cashobjects_register())
    {
        failure("can't register cashbojects");
        goto cleanup;
    }

    /* Loop the test. */
    for (i = 0; i < 10; i++)
    {
        run_test ();
    }
    success("queries seem to work");

cleanup:
    qof_close();
    return get_rv();
}
