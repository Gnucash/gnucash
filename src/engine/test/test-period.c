/***************************************************************************
 *            test-period.c
 *
 *  December 2001
 *  Copyright  2001 Linas Vepstas <linas@linas.org
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
/*
* Minimal test to see if a book can be split into two periods
* without crashing.
*/

#include "config.h"
#include <ctype.h>
#include <glib.h>
#include "qof.h"
#include <time.h>
#include "cashobjects.h"
#include "Account.h"
#include "Period.h"
#include "test-stuff.h"
#include "test-engine-stuff.h"
#include "Transaction.h"

static int num_trans = 0;
static void
run_test (void)
{
    QofSession *sess1, *sess2;
    QofBook *openbook, *closedbook;
    GList *acclist, *anode;
    Account *root, *acc, *equity;
    SplitList *splist;
    Split *sfirst, *slast;
    Transaction *tfirst, *tlast;
    Timespec tsfirst, tslast, tsmiddle;

    sess1 = get_random_session ();
    openbook = qof_session_get_book (sess1);
    sess2 = get_random_session ();
    closedbook = qof_session_get_book(sess2);
    acc = NULL;
    equity = get_random_account(openbook);
    if (!openbook)
    {
        failure("book not created");
        exit(get_rv());
    }

    add_random_transactions_to_book (openbook, num_trans);

    root = gnc_book_get_root_account (openbook);

    acclist = gnc_account_get_descendants (root);
    for (anode = acclist; anode; anode = anode->next)
    {
        int ns;
        acc = anode->data;
        ns = g_list_length (xaccAccountGetSplitList (acc));
        if (2 <= ns) break;
        acc = NULL;
    }
    g_list_free(acclist);

    if (!acc)
    {
        failure("book didn't have accounts with enough splits");
        exit(get_rv());
    }

    splist = xaccAccountGetSplitList(acc);
    if (!splist)
    {
        failure("account has no transactions");
        exit(get_rv());
    }

    sfirst = splist->data;
    slast = g_list_last(splist) ->data;
    if (sfirst == slast)
    {
        failure("account doesn't have enough transactions");
        exit(get_rv());
    }

    tfirst = xaccSplitGetParent (sfirst);
    tlast = xaccSplitGetParent (slast);

    if (!tfirst || !tlast)
    {
        failure("malformed transactions in account");
        exit(get_rv());
    }

    tsfirst = xaccTransRetDatePostedTS (tfirst);
    tslast = xaccTransRetDatePostedTS (tlast);

    if (tsfirst.tv_sec == tslast.tv_sec)
    {
        failure("transactions not time separated");
        exit(get_rv());
    }

    tsmiddle = tsfirst;
    tsmiddle.tv_sec = (tsfirst.tv_sec + tslast.tv_sec) / 2;
    closedbook = gnc_book_close_period (openbook, tsmiddle,
                                        equity, "this is opening balance dude");

    if (!closedbook)
    {
        failure("closed book not created");
        exit(get_rv());
    }

    success ("periods lightly tested and seem to work");
}

int
main (int argc, char **argv)
{
    if (argc == 2)
        num_trans = atoi(argv[1]);
    else num_trans = 120;

    qof_init();
    g_log_set_always_fatal( G_LOG_LEVEL_CRITICAL | G_LOG_LEVEL_WARNING );

    if (cashobjects_register())
    {
        srand(num_trans);
        run_test ();
        print_test_results();
    }
    qof_close();
    return get_rv();
}
