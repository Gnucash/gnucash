/***************************************************************************
 *            test-transaction-reversal.c
 *
 *  Modified to run without Guile: Mon Aug 22 11:19:56 2005
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
#include "Transaction.h"
#include "Account.h"
#include "TransLog.h"
#include "test-engine-stuff.h"
#include "test-stuff.h"

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
    Transaction *transaction, *new_trans;
    gnc_numeric old, new, result;
    QofBook *book;
    char *msg;
    int i;

    book = qof_book_new();

    acc1 = get_random_account(book);
    acc2 = get_random_account(book);

    if (!acc1 || !acc2)
    {
        failure("accounts not created");
        return;
    }

    /* Find a transaction that isn't voided */
    do
    {
        gboolean voyd;

        transaction = get_random_transaction (book);
        voyd = xaccTransGetVoidStatus (transaction);
        if (voyd)
        {
            xaccTransBeginEdit (transaction);
            xaccTransDestroy (transaction);
            xaccTransCommitEdit (transaction);
            transaction = NULL;
        }
    }
    while (!transaction);
    transaction_set_splits_to_accounts(transaction, acc1, acc2);
    xaccTransSortSplits(transaction);

    new_trans = xaccTransReverse(transaction);
    for (i = 0; i < 2; i++)
    {
        old = xaccSplitGetAmount(xaccTransGetSplit(transaction, i));
        new = xaccSplitGetAmount(xaccTransGetSplit(new_trans, i));
        result = gnc_numeric_add(old, new, GNC_DENOM_AUTO, GNC_DENOM_FIXED);
        if (gnc_numeric_eq(old, gnc_numeric_neg(new)))
        {
            msg = g_strdup_printf("Amount of split %d wrong after reversal\n", i);
            failure(msg);
        }

        old = xaccSplitGetValue(xaccTransGetSplit(transaction, i));
        new = xaccSplitGetValue(xaccTransGetSplit(new_trans, i));
        result = gnc_numeric_add(old, new, GNC_DENOM_AUTO, GNC_DENOM_FIXED);
        if (gnc_numeric_eq(old, gnc_numeric_neg(new)))
        {
            msg = g_strdup_printf("Value of split %d wrong after reversal\n", i);
            failure(msg);
        }

    }
    return;
}

int
main (int argc, char **argv)
{
    qof_init();
    if (cashobjects_register())
    {
        set_success_print (TRUE);
        run_test ();
        success("transaction voiding seems OK");
        print_test_results();
    }
    qof_close();
    return get_rv();
}
