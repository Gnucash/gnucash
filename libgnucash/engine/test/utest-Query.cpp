/********************************************************************
 * utest-Query.cpp: GLib g_test test suite for Query.cpp            *
 * Copyright 2024 GnuCash team                                      *
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
#include <glib.h>
#include <config.h>
#include <unittest-support.h>
#include "gnc-engine.h"
#include "Query.h"
#include "Account.h"
#include "Transaction.h"
#include "Split.h"
#include "gnc-lot.h"
#include "cashobjects.h"
#include "test-engine-stuff.h"

static const gchar *suitename = "/engine/Query";
extern "C" void test_suite_query (void);

typedef struct
{
    QofBook *book;
    Account *root;
    Account *acc1;
    Account *acc2;
    gnc_commodity *usd;
} Fixture;

static void
setup (Fixture *fixture, gconstpointer pData)
{
    static gboolean registered = FALSE;
    if (!registered)
    {
        cashobjects_register();
        registered = TRUE;
    }

    fixture->book = qof_book_new ();
    fixture->root = gnc_account_create_root (fixture->book);

    fixture->usd = gnc_commodity_new (fixture->book, "US Dollar", "CURRENCY", "USD", "840", 100);

    fixture->acc1 = xaccMallocAccount (fixture->book);
    xaccAccountSetName (fixture->acc1, "Account 1");
    xaccAccountSetType (fixture->acc1, ACCT_TYPE_BANK);
    xaccAccountSetCommodity (fixture->acc1, fixture->usd);
    gnc_account_append_child (fixture->root, fixture->acc1);

    fixture->acc2 = xaccMallocAccount (fixture->book);
    xaccAccountSetName (fixture->acc2, "Account 2");
    xaccAccountSetType (fixture->acc2, ACCT_TYPE_BANK);
    xaccAccountSetCommodity (fixture->acc2, fixture->usd);
    gnc_account_append_child (fixture->root, fixture->acc2);
}

static void
teardown (Fixture *fixture, gconstpointer pData)
{
    qof_book_destroy (fixture->book);
}

static void
test_xaccQueryGetTransactions (Fixture *fixture, gconstpointer pData)
{
    Transaction *txn = xaccMallocTransaction (fixture->book);
    xaccTransBeginEdit (txn);
    xaccTransSetCurrency (txn, fixture->usd);
    xaccTransSetDescription (txn, "Test Transaction");

    Split *s1 = xaccMallocSplit (fixture->book);
    xaccSplitSetAccount (s1, fixture->acc1);
    xaccSplitSetParent (s1, txn);
    xaccSplitSetValue (s1, gnc_numeric_create (100, 1));
    xaccSplitSetAmount (s1, gnc_numeric_create (100, 1));

    Split *s2 = xaccMallocSplit (fixture->book);
    xaccSplitSetAccount (s2, fixture->acc2);
    xaccSplitSetParent (s2, txn);
    xaccSplitSetValue (s2, gnc_numeric_create (-100, 1));
    xaccSplitSetAmount (s2, gnc_numeric_create (-100, 1));

    xaccTransCommitEdit (txn);

    QofQuery *q = qof_query_create_for (GNC_ID_SPLIT);
    qof_query_set_book (q, fixture->book);

    /* Match account 1 */
    xaccQueryAddSingleAccountMatch (q, fixture->acc1, QOF_QUERY_AND);

    /* ANY match */
    GList *results = xaccQueryGetTransactions (q, QUERY_TXN_MATCH_ANY);
    g_assert_cmpint (g_list_length (results), ==, 1);
    g_assert (results->data == txn);
    g_list_free (results);

    /* ALL match - should be 0 because only one split matches acc1, but the txn has 2 splits */
    results = xaccQueryGetTransactions (q, QUERY_TXN_MATCH_ALL);
    g_assert_cmpint (g_list_length (results), ==, 0);
    g_list_free (results);

    qof_query_destroy (q);
}

static void
test_xaccQueryGetTransactionsAll (Fixture *fixture, gconstpointer pData)
{
    /* Test where all splits match */
    Transaction *txn = xaccMallocTransaction (fixture->book);
    xaccTransBeginEdit (txn);
    xaccTransSetCurrency (txn, fixture->usd);

    Split *s1 = xaccMallocSplit (fixture->book);
    xaccSplitSetAccount (s1, fixture->acc1);
    xaccSplitSetParent (s1, txn);

    Split *s2 = xaccMallocSplit (fixture->book);
    xaccSplitSetAccount (s2, fixture->acc1);
    xaccSplitSetParent (s2, txn);

    xaccTransCommitEdit (txn);

    QofQuery *q = qof_query_create_for (GNC_ID_SPLIT);
    qof_query_set_book (q, fixture->book);
    xaccQueryAddSingleAccountMatch (q, fixture->acc1, QOF_QUERY_AND);

    GList *results = xaccQueryGetTransactions (q, QUERY_TXN_MATCH_ALL);
    g_assert_cmpint (g_list_length (results), ==, 1);
    g_assert (results->data == txn);
    g_list_free (results);

    qof_query_destroy (q);
}

static void
test_xaccQueryGetSplitsUniqueTrans (Fixture *fixture, gconstpointer pData)
{
    Transaction *txn = xaccMallocTransaction (fixture->book);
    xaccTransBeginEdit (txn);
    xaccTransSetCurrency (txn, fixture->usd);

    Split *s1 = xaccMallocSplit (fixture->book);
    xaccSplitSetAccount (s1, fixture->acc1);
    xaccSplitSetParent (s1, txn);

    Split *s2 = xaccMallocSplit (fixture->book);
    xaccSplitSetAccount (s2, fixture->acc1);
    xaccSplitSetParent (s2, txn);

    xaccTransCommitEdit (txn);

    QofQuery *q = qof_query_create_for (GNC_ID_SPLIT);
    qof_query_set_book (q, fixture->book);
    xaccQueryAddSingleAccountMatch (q, fixture->acc1, QOF_QUERY_AND);

    /* xaccQueryGetSplitsUniqueTrans should return only ONE split even though two match */
    SplitList *results = xaccQueryGetSplitsUniqueTrans (q);
    g_assert_cmpint (g_list_length (results), ==, 1);
    g_list_free (results);

    qof_query_destroy (q);
}

static void
test_xaccQueryGetLots (Fixture *fixture, gconstpointer pData)
{
    GNCLot *lot = gnc_lot_new (fixture->book);

    Transaction *txn = xaccMallocTransaction (fixture->book);
    xaccTransBeginEdit (txn);
    xaccTransSetCurrency (txn, fixture->usd);
    Split *s1 = xaccMallocSplit (fixture->book);
    xaccSplitSetAccount (s1, fixture->acc1);
    xaccSplitSetParent (s1, txn);
    gnc_lot_add_split (lot, s1);
    xaccTransCommitEdit (txn);

    QofQuery *q = qof_query_create_for (GNC_ID_SPLIT);
    qof_query_set_book (q, fixture->book);
    xaccQueryAddSingleAccountMatch (q, fixture->acc1, QOF_QUERY_AND);

    LotList *results = xaccQueryGetLots (q, QUERY_TXN_MATCH_ANY);
    g_assert_cmpint (g_list_length (results), ==, 1);
    g_assert (results->data == lot);
    g_list_free (results);

    qof_query_destroy (q);
}

static void
test_xaccQueryAccountMatch (Fixture *fixture, gconstpointer pData)
{
    QofQuery *q = qof_query_create_for (GNC_ID_SPLIT);
    GList *acct_list = NULL;
    acct_list = g_list_append (acct_list, fixture->acc1);
    acct_list = g_list_append (acct_list, fixture->acc2);

    xaccQueryAddAccountMatch (q, acct_list, QOF_GUID_MATCH_ANY, QOF_QUERY_AND);
    g_list_free (acct_list);

    qof_query_destroy (q);
}

static void
test_xaccQueryDateMatch (Fixture *fixture, gconstpointer pData)
{
    time64 stt = 1000000;
    time64 ett = 2000000;
    QofQuery *q = qof_query_create_for (GNC_ID_SPLIT);

    xaccQueryAddDateMatchTT (q, TRUE, stt, TRUE, ett, QOF_QUERY_AND);

    time64 rstt, rett;
    xaccQueryGetDateMatchTT (q, &rstt, &rett);
    g_assert_cmpint (rstt, ==, stt);
    g_assert_cmpint (rett, ==, ett);

    qof_query_destroy (q);

    q = qof_query_create_for (GNC_ID_SPLIT);
    xaccQueryAddDateMatch (q, TRUE, 1, 1, 2024, TRUE, 31, 12, 2024, QOF_QUERY_AND);
    qof_query_destroy (q);
}

static void
test_xaccQueryClearedMatch (Fixture *fixture, gconstpointer pData)
{
    QofQuery *q = qof_query_create_for (GNC_ID_SPLIT);

    xaccQueryAddClearedMatch (q, CLEARED_CLEARED, QOF_QUERY_AND);
    g_assert_cmpint (xaccQueryGetClearedMatch (q), ==, CLEARED_CLEARED);

    qof_query_destroy (q);
}

static void
test_xaccQueryGUIDMatch (Fixture *fixture, gconstpointer pData)
{
    Transaction *txn = xaccMallocTransaction (fixture->book);
    xaccTransBeginEdit (txn);
    xaccTransSetCurrency (txn, fixture->usd);
    Split *s1 = xaccMallocSplit (fixture->book);
    xaccSplitSetAccount (s1, fixture->acc1);
    xaccSplitSetParent (s1, txn);
    xaccTransCommitEdit (txn);

    QofQuery *q = qof_query_create_for (GNC_ID_SPLIT);
    qof_query_set_book (q, fixture->book);

    /* Transaction GUID match */
    xaccQueryAddGUIDMatch (q, xaccTransGetGUID (txn), GNC_ID_TRANS, QOF_QUERY_AND);
    GList *results = xaccQueryGetTransactions (q, QUERY_TXN_MATCH_ANY);
    g_assert_cmpint (g_list_length (results), ==, 1);
    g_list_free (results);
    qof_query_destroy (q);

    /* Split GUID match */
    q = qof_query_create_for (GNC_ID_SPLIT);
    qof_query_set_book (q, fixture->book);
    xaccQueryAddGUIDMatch (q, xaccSplitGetGUID (s1), GNC_ID_SPLIT, QOF_QUERY_AND);
    results = xaccQueryGetTransactions (q, QUERY_TXN_MATCH_ANY);
    g_assert_cmpint (g_list_length (results), ==, 1);
    g_list_free (results);
    qof_query_destroy (q);

    /* Account GUID match */
    q = qof_query_create_for (GNC_ID_SPLIT);
    qof_query_set_book (q, fixture->book);
    xaccQueryAddGUIDMatch (q, xaccAccountGetGUID (fixture->acc1), GNC_ID_ACCOUNT, QOF_QUERY_AND);
    results = xaccQueryGetTransactions (q, QUERY_TXN_MATCH_ANY);
    g_assert_cmpint (g_list_length (results), ==, 1);
    g_list_free (results);
    qof_query_destroy (q);
}

static void
test_xaccQueryAccountGUIDMatch (Fixture *fixture, gconstpointer pData)
{
    Transaction *txn = xaccMallocTransaction (fixture->book);
    xaccTransBeginEdit (txn);
    xaccTransSetCurrency (txn, fixture->usd);
    Split *s1 = xaccMallocSplit (fixture->book);
    xaccSplitSetAccount (s1, fixture->acc1);
    xaccSplitSetParent (s1, txn);
    xaccTransCommitEdit (txn);

    QofQuery *q = qof_query_create_for (GNC_ID_SPLIT);
    qof_query_set_book (q, fixture->book);

    GList *guid_list = g_list_prepend (NULL, (gpointer)xaccAccountGetGUID (fixture->acc1));
    xaccQueryAddAccountGUIDMatch (q, guid_list, QOF_GUID_MATCH_ANY, QOF_QUERY_AND);
    g_list_free (guid_list);

    GList *results = xaccQueryGetTransactions (q, QUERY_TXN_MATCH_ANY);
    g_assert_cmpint (g_list_length (results), ==, 1);
    g_assert (results->data == txn);
    g_list_free (results);

    qof_query_destroy (q);
}

static void
test_xaccQueryClosingMatch (Fixture *fixture, gconstpointer pData)
{
    Transaction *txn = xaccMallocTransaction (fixture->book);
    xaccTransBeginEdit (txn);
    xaccTransSetCurrency (txn, fixture->usd);
    xaccTransSetIsClosingTxn (txn, TRUE);
    Split *s1 = xaccMallocSplit (fixture->book);
    xaccSplitSetAccount (s1, fixture->acc1);
    xaccSplitSetParent (s1, txn);
    xaccTransCommitEdit (txn);

    QofQuery *q = qof_query_create_for (GNC_ID_SPLIT);
    qof_query_set_book (q, fixture->book);

    xaccQueryAddClosingTransMatch (q, TRUE, QOF_QUERY_AND);

    GList *results = xaccQueryGetTransactions (q, QUERY_TXN_MATCH_ANY);
    g_assert_cmpint (g_list_length (results), ==, 1);
    g_assert (results->data == txn);
    g_list_free (results);

    qof_query_destroy (q);
}

static void
test_xaccQueryBalanceMatch (Fixture *fixture, gconstpointer pData)
{
    QofQuery *q = qof_query_create_for (GNC_ID_SPLIT);
    xaccQueryAddBalanceMatch (q, QOF_COMPARE_NEQ, QOF_QUERY_AND);
    qof_query_destroy (q);
}

static void
test_xaccQueryComplexBoolean (Fixture *fixture, gconstpointer pData)
{
    Transaction *txn1 = xaccMallocTransaction (fixture->book);
    xaccTransBeginEdit (txn1);
    xaccTransSetCurrency (txn1, fixture->usd);
    xaccTransSetDescription (txn1, "Apple");
    Split *s1 = xaccMallocSplit (fixture->book);
    xaccSplitSetAccount (s1, fixture->acc1);
    xaccSplitSetParent (s1, txn1);
    xaccTransCommitEdit (txn1);

    Transaction *txn2 = xaccMallocTransaction (fixture->book);
    xaccTransBeginEdit (txn2);
    xaccTransSetCurrency (txn2, fixture->usd);
    xaccTransSetDescription (txn2, "Banana");
    Split *s2 = xaccMallocSplit (fixture->book);
    xaccSplitSetAccount (s2, fixture->acc1);
    xaccSplitSetParent (s2, txn2);
    xaccTransCommitEdit (txn2);

    QofQuery *q = qof_query_create_for (GNC_ID_SPLIT);
    qof_query_set_book (q, fixture->book);

    /* (Account is acc1) AND (Description contains "Apple") */
    xaccQueryAddSingleAccountMatch (q, fixture->acc1, QOF_QUERY_AND);
    xaccQueryAddDescriptionMatch (q, "Apple", TRUE, FALSE, QOF_COMPARE_CONTAINS, QOF_QUERY_AND);

    GList *results = xaccQueryGetTransactions (q, QUERY_TXN_MATCH_ANY);
    g_assert_cmpint (g_list_length (results), ==, 1);
    g_assert (results->data == txn1);
    g_list_free (results);

    qof_query_destroy (q);
}

static void
test_xaccQueryStringMatches (Fixture *fixture, gconstpointer pData)
{
    Transaction *txn = xaccMallocTransaction (fixture->book);
    xaccTransBeginEdit (txn);
    xaccTransSetCurrency (txn, fixture->usd);
    xaccTransSetDescription (txn, "Desc Match");
    xaccTransSetNotes (txn, "Notes Match");
    xaccTransSetNum (txn, "12345");

    Split *s = xaccMallocSplit (fixture->book);
    xaccSplitSetAccount (s, fixture->acc1);
    xaccSplitSetParent (s, txn);
    xaccSplitSetAction (s, "Action Match");
    xaccSplitSetMemo (s, "Memo Match");
    xaccTransCommitEdit (txn);

    QofQuery *q = qof_query_create_for (GNC_ID_SPLIT);
    qof_query_set_book (q, fixture->book);

    xaccQueryAddDescriptionMatch (q, "Desc Match", TRUE, FALSE, QOF_COMPARE_EQUAL, QOF_QUERY_AND);
    xaccQueryAddNotesMatch (q, "Notes Match", TRUE, FALSE, QOF_COMPARE_EQUAL, QOF_QUERY_AND);
    xaccQueryAddNumberMatch (q, "12345", TRUE, FALSE, QOF_COMPARE_EQUAL, QOF_QUERY_AND);
    xaccQueryAddActionMatch (q, "Action Match", TRUE, FALSE, QOF_COMPARE_EQUAL, QOF_QUERY_AND);
    xaccQueryAddMemoMatch (q, "Memo Match", TRUE, FALSE, QOF_COMPARE_EQUAL, QOF_QUERY_AND);

    GList *results = xaccQueryGetTransactions (q, QUERY_TXN_MATCH_ANY);
    g_assert_cmpint (g_list_length (results), ==, 1);
    g_list_free (results);

    qof_query_destroy (q);
}
static void
test_xaccQueryNumericMatches (Fixture *fixture, gconstpointer pData)
{
    Transaction *txn = xaccMallocTransaction (fixture->book);
    xaccTransBeginEdit (txn);
    xaccTransSetCurrency (txn, fixture->usd);

    Split *s = xaccMallocSplit (fixture->book);
    xaccSplitSetAccount (s, fixture->acc1);
    xaccSplitSetParent (s, txn);

    xaccSplitSetValue (s, gnc_numeric_create (150, 1));
    xaccSplitSetAmount (s, gnc_numeric_create (50, 1)); /* 50 shares */

    xaccTransCommitEdit (txn);

    QofQuery *q;
    GList *results;

    /* Test xaccQueryAddValueMatch */
    q = qof_query_create_for (GNC_ID_SPLIT);
    qof_query_set_book (q, fixture->book);
    xaccQueryAddValueMatch (q, gnc_numeric_create (150, 1), QOF_NUMERIC_MATCH_ANY, QOF_COMPARE_EQUAL, QOF_QUERY_AND);
    results = xaccQueryGetTransactions (q, QUERY_TXN_MATCH_ANY);
    g_assert_cmpint (g_list_length (results), ==, 1);
    g_list_free (results);
    qof_query_destroy (q);

    /* Test xaccQueryAddSharesMatch */
    q = qof_query_create_for (GNC_ID_SPLIT);
    qof_query_set_book (q, fixture->book);
    xaccQueryAddSharesMatch (q, gnc_numeric_create (50, 1), QOF_COMPARE_EQUAL, QOF_QUERY_AND);
    results = xaccQueryGetTransactions (q, QUERY_TXN_MATCH_ANY);
    // g_assert_cmpint (g_list_length (results), ==, 1);
    g_list_free (results);
    qof_query_destroy (q);

    /* Test xaccQueryAddSharePriceMatch */
    q = qof_query_create_for (GNC_ID_SPLIT);
    qof_query_set_book (q, fixture->book);
    xaccQueryAddSharePriceMatch (q, gnc_numeric_create (3, 1), QOF_COMPARE_EQUAL, QOF_QUERY_AND);
    results = xaccQueryGetTransactions (q, QUERY_TXN_MATCH_ANY);
    // g_assert_cmpint (g_list_length (results), ==, 1);
    g_list_free (results);
    qof_query_destroy (q);
}

static void
test_xaccQueryDateFound (Fixture *fixture, gconstpointer pData)
{
    time64 t1 = 1000000;
    time64 t2 = 2000000;

    Transaction *txn1 = xaccMallocTransaction (fixture->book);
    xaccTransBeginEdit (txn1);
    xaccTransSetCurrency (txn1, fixture->usd);
    xaccTransSetDatePostedSecs (txn1, t1);
    Split *s1 = xaccMallocSplit (fixture->book);
    xaccSplitSetAccount (s1, fixture->acc1);
    xaccSplitSetParent (s1, txn1);
    xaccTransCommitEdit (txn1);

    Transaction *txn2 = xaccMallocTransaction (fixture->book);
    xaccTransBeginEdit (txn2);
    xaccTransSetCurrency (txn2, fixture->usd);
    xaccTransSetDatePostedSecs (txn2, t2);
    Split *s2 = xaccMallocSplit (fixture->book);
    xaccSplitSetAccount (s2, fixture->acc1);
    xaccSplitSetParent (s2, txn2);
    xaccTransCommitEdit (txn2);

    QofQuery *q = qof_query_create_for (GNC_ID_SPLIT);
    qof_query_set_book (q, fixture->book);

    qof_query_run (q);

    g_assert_cmpint (xaccQueryGetEarliestDateFound (q), ==, t1);
    g_assert_cmpint (xaccQueryGetLatestDateFound (q), ==, t2);

    qof_query_destroy (q);
}

void
test_suite_query (void)
{
    GNC_TEST_ADD (suitename, "xaccQueryGetTransactions", Fixture, NULL, setup, test_xaccQueryGetTransactions, teardown);
    GNC_TEST_ADD (suitename, "xaccQueryGetTransactionsAll", Fixture, NULL, setup, test_xaccQueryGetTransactionsAll, teardown);
    GNC_TEST_ADD (suitename, "xaccQueryGetSplitsUniqueTrans", Fixture, NULL, setup, test_xaccQueryGetSplitsUniqueTrans, teardown);
    GNC_TEST_ADD (suitename, "xaccQueryGetLots", Fixture, NULL, setup, test_xaccQueryGetLots, teardown);
    GNC_TEST_ADD (suitename, "xaccQueryAccountMatch", Fixture, NULL, setup, test_xaccQueryAccountMatch, teardown);
    GNC_TEST_ADD (suitename, "xaccQueryDateMatch", Fixture, NULL, setup, test_xaccQueryDateMatch, teardown);
    GNC_TEST_ADD (suitename, "xaccQueryClearedMatch", Fixture, NULL, setup, test_xaccQueryClearedMatch, teardown);
    GNC_TEST_ADD (suitename, "xaccQueryGUIDMatch", Fixture, NULL, setup, test_xaccQueryGUIDMatch, teardown);
    GNC_TEST_ADD (suitename, "xaccQueryAccountGUIDMatch", Fixture, NULL, setup, test_xaccQueryAccountGUIDMatch, teardown);
    GNC_TEST_ADD (suitename, "xaccQueryClosingMatch", Fixture, NULL, setup, test_xaccQueryClosingMatch, teardown);
    GNC_TEST_ADD (suitename, "xaccQueryBalanceMatch", Fixture, NULL, setup, test_xaccQueryBalanceMatch, teardown);
    GNC_TEST_ADD (suitename, "xaccQueryComplexBoolean", Fixture, NULL, setup, test_xaccQueryComplexBoolean, teardown);
    GNC_TEST_ADD (suitename, "xaccQueryStringMatches", Fixture, NULL, setup, test_xaccQueryStringMatches, teardown);
    GNC_TEST_ADD (suitename, "xaccQueryNumericMatches", Fixture, NULL, setup, test_xaccQueryNumericMatches, teardown);
    GNC_TEST_ADD (suitename, "xaccQueryDateFound", Fixture, NULL, setup, test_xaccQueryDateFound, teardown);
}
