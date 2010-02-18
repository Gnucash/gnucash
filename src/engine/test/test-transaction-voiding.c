/***************************************************************************
 *            test-transaction-voiding.c
 *
 *  Modified to run without Guile: Mon Aug 22 11:24:44 2005
 *  Copyright  2005  Neil Williams
 *  linux@codehelp.co.uk
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
#include <string.h>
#include "cashobjects.h"
#include "Account.h"
#include "TransLog.h"
#include "test-engine-stuff.h"
#include "test-stuff.h"
#include "Transaction.h"

#define print_gnc_numeric(num) fprintf(stderr, "%s\n", gnc_numeric_to_string(num))

static void
transaction_set_splits_to_accounts(Transaction *tr, Account *a1, Account *a2)
{

    Split *split;

    split  = xaccTransGetSplit(tr, 0);

    xaccAccountInsertSplit(a1, split);

    split = xaccTransGetSplit(tr, 1);
    xaccAccountInsertSplit(a2, split);
    return;
}

static void
run_test (void)
{
    Account *acc1, *acc2;
    Transaction *transaction;
    gnc_numeric old_amt, new_amt, old_val, new_val;
    QofBook *book;
    Timespec ts;
    time_t now;

    char *reason = "because I can";

    book = qof_book_new();

    acc1 = get_random_account(book);
    acc2 = get_random_account(book);

    if (!acc1 || !acc2)
    {
        failure("accounts not created");
    }

    do
    {
        transaction = get_random_transaction (book);
        if (xaccTransGetVoidStatus (transaction))
        {
            xaccTransBeginEdit (transaction);
            xaccTransDestroy (transaction);
            xaccTransCommitEdit (transaction);
            transaction = NULL;
        }
    }
    while (!transaction);

    transaction_set_splits_to_accounts(transaction, acc1, acc2);

    /*  Compromise, check amount on one and value on the other */

    old_amt = xaccSplitGetAmount(xaccTransGetSplit(transaction, 0));
    old_val = xaccSplitGetValue(xaccTransGetSplit(transaction, 1));

    now = time (NULL);

    xaccTransVoid(transaction, reason);

    ts = xaccTransGetVoidTime (transaction);

    /* figure at most 2 seconds difference */
    if ((ts.tv_sec < now) || ((ts.tv_sec - now) > 2))
    {
        failure("bad void time");
    }

    if (!xaccTransGetVoidStatus(transaction))
    {
        failure("void status reports false after setting void");
    }

    if (strcmp(reason, xaccTransGetVoidReason(transaction)) != 0)
    {
        failure("Reasons didn't match");
    }

    new_amt = xaccSplitGetAmount(xaccTransGetSplit(transaction, 0));
    /* print_gnc_numeric(new_amt); */

    if (!gnc_numeric_zero_p( new_amt))
    {
        failure("Amount of split0 not zero after voiding");
    }

    new_val = xaccSplitGetValue(xaccTransGetSplit(transaction, 1));

    if (!(gnc_numeric_zero_p(new_val)))
    {
        failure("Value of split1 not zero after voiding");
    }


    if (!(gnc_numeric_eq(old_amt, xaccSplitVoidFormerAmount(xaccTransGetSplit(transaction, 0)))))
    {
        failure("former amount (after voiding) didn't match actual old amount");
    }

    if (!(gnc_numeric_eq(old_val, xaccSplitVoidFormerValue(xaccTransGetSplit(transaction, 1)))))
    {
        failure("former value (after voiding) didn't match actual old value");
    }

    /*
     * Retore the transaction to its former glory.
     */
    xaccTransUnvoid(transaction);

    ts = xaccTransGetVoidTime (transaction);

    /* figure at most 2 seconds difference */
    if ((ts.tv_sec != 0) || (ts.tv_sec != 0))
    {
        failure("void time not zero after restore");
    }

    if (xaccTransGetVoidStatus(transaction))
    {
        failure("void status reports trus after restoring transaction");
    }

    if (xaccTransGetVoidReason(transaction))
    {
        failure("void reason exists after restoring transaction");
    }

    new_amt = xaccSplitGetAmount(xaccTransGetSplit(transaction, 0));
    /* print_gnc_numeric(new_amt); */

    if (!(gnc_numeric_eq(old_amt, new_amt)))
    {
        failure("Amount of split0 not correct after restoring transaction");
    }

    new_val = xaccSplitGetValue(xaccTransGetSplit(transaction, 1));

    if (!(gnc_numeric_eq(old_val, new_val)))
    {
        failure("Value of split1 not correct after restoring transaction");
    }


    if (!(gnc_numeric_zero_p(xaccSplitVoidFormerAmount(xaccTransGetSplit(transaction, 0)))))
    {
        failure("former amount (after restore) should be zero");
    }

    if (!(gnc_numeric_zero_p(xaccSplitVoidFormerValue(xaccTransGetSplit(transaction, 1)))))
    {
        failure("former value (after restore) should be zero");
    }

    return;
}

int
main (int argc, char **argv)
{
    qof_init();
    if (cashobjects_register())
    {
        xaccLogDisable ();
        run_test ();
        success("transaction voiding seems OK");
        print_test_results();
    }
    qof_close();
    return get_rv();
}
