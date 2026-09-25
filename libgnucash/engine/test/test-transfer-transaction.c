/* test-transfer-transaction.c -- Transfer creation without a GTK dialog.
 *
 * Copyright (C) 2026 GnuCash contributors
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 */

#include <config.h>

#include "cashobjects.h"
#include "gnc-commodity.h"
#include "gnc-session.h"
#include "gnc-transfer-transaction.h"
#include "qofbook.h"

static Account *
make_account (QofBook *book, Account *root, gnc_commodity *commodity,
              const char *name)
{
    Account *account = xaccMallocAccount (book);

    xaccAccountBeginEdit (account);
    xaccAccountSetName (account, name);
    xaccAccountSetType (account, ACCT_TYPE_BANK);
    xaccAccountSetCommodity (account, commodity);
    gnc_account_append_child (root, account);
    xaccAccountCommitEdit (account);
    return account;
}

static void
test_transfer_transaction (gconstpointer data)
{
    gboolean split_action_num = GPOINTER_TO_INT (data);
    QofSession *session = qof_session_new (qof_book_new ());
    QofSession *unrelated = qof_session_new (qof_book_new ());
    QofBook *book = qof_session_get_book (session);
    gnc_commodity_table *table = gnc_commodity_table_get_table (book);
    gnc_commodity *usd = gnc_commodity_table_lookup (table,
                                                      GNC_COMMODITY_NS_CURRENCY,
                                                      "USD");
    gnc_commodity *eur = gnc_commodity_table_lookup (table,
                                                      GNC_COMMODITY_NS_CURRENCY,
                                                      "EUR");
    Account *root = gnc_account_create_root (book);
    Account *from_account;
    Account *to_account;
    Transaction *transaction;
    Split *from_split = NULL;
    Split *to_split = NULL;
    GncTransferTransactionInfo info;
    GDate posted;
    gint index;

    g_assert_nonnull (usd);
    g_assert_nonnull (eur);
    gnc_set_current_session (unrelated);
    if (split_action_num)
    {
        qof_book_begin_edit (book);
        qof_instance_set (QOF_INSTANCE (book), "split-action-num-field", "t",
                          NULL);
        qof_book_commit_edit (book);
    }
    else
    {
        QofBook *unrelated_book = qof_session_get_book (unrelated);
        qof_book_begin_edit (unrelated_book);
        qof_instance_set (QOF_INSTANCE (unrelated_book),
                          "split-action-num-field", "t", NULL);
        qof_book_commit_edit (unrelated_book);
    }

    from_account = make_account (book, root, usd, "From");
    to_account = make_account (book, root, eur, "To");
    info = (GncTransferTransactionInfo) {
        book, from_account, to_account, usd, eur, 1609502400,
        gnc_numeric_create (10, 1), gnc_numeric_create (9, 1),
        "123", "Transfer", "Notes", "Memo"
    };
    transaction = gnc_transfer_transaction_create (&info);

    g_assert_nonnull (transaction);
    g_assert_true (xaccTransGetCurrency (transaction) == usd);
    g_assert_cmpstr (xaccTransGetDescription (transaction), ==, "Transfer");
    g_assert_cmpstr (xaccTransGetNotes (transaction), ==, "Notes");
    g_assert_cmpint (xaccTransCountSplits (transaction), ==, 2);
    posted = xaccTransGetDatePostedGDate (transaction);
    g_assert_cmpuint (g_date_get_year (&posted), ==, 2021);
    g_assert_cmpuint (g_date_get_month (&posted), ==, G_DATE_JANUARY);
    g_assert_cmpuint (g_date_get_day (&posted), ==, 1);

    for (index = 0; index < 2; ++index)
    {
        Split *split = xaccTransGetSplit (transaction, index);
        if (xaccSplitGetAccount (split) == from_account)
            from_split = split;
        else if (xaccSplitGetAccount (split) == to_account)
            to_split = split;
    }
    g_assert_nonnull (from_split);
    g_assert_nonnull (to_split);
    g_assert_true (gnc_numeric_equal (xaccSplitGetValue (from_split),
                                      gnc_numeric_create (-10, 1)));
    g_assert_true (gnc_numeric_equal (xaccSplitGetAmount (from_split),
                                      gnc_numeric_create (-10, 1)));
    g_assert_true (gnc_numeric_equal (xaccSplitGetValue (to_split),
                                      gnc_numeric_create (10, 1)));
    g_assert_true (gnc_numeric_equal (xaccSplitGetAmount (to_split),
                                      gnc_numeric_create (9, 1)));
    g_assert_cmpstr (xaccSplitGetMemo (from_split), ==, "Memo");
    g_assert_cmpstr (xaccSplitGetMemo (to_split), ==, "Memo");
    g_assert_cmpstr (xaccTransGetNum (transaction), ==,
                     split_action_num ? "" : "123");
    g_assert_cmpstr (xaccSplitGetAction (from_split), ==,
                     split_action_num ? "123" : "");

    gnc_clear_current_session ();
    qof_session_destroy (session);
}

int
main (int argc, char **argv)
{
    g_test_init (&argc, &argv, NULL);
    qof_init ();
    g_assert_true (cashobjects_register ());
    g_test_add_data_func ("/engine/transfer/transaction-number",
                          GINT_TO_POINTER (FALSE), test_transfer_transaction);
    g_test_add_data_func ("/engine/transfer/split-action-number",
                          GINT_TO_POINTER (TRUE), test_transfer_transaction);
    gint result = g_test_run ();
    qof_close ();
    return result;
}
