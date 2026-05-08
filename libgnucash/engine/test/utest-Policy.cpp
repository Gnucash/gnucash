/********************************************************************
 * utest-Policy.cpp: GLib g_test test suite for policy.cpp.         *
 ********************************************************************/

#include <glib.h>

#include <config.h>
#include <unittest-support.h>

#include "Account.h"
#include "Transaction.h"
#include "Split.h"
#include "SplitP.hpp"
#include "gnc-lot.h"
#include "policy.h"
#include "policy-p.h"

static const gchar *suitename = "/engine/Policy";
extern "C" void test_suite_policy (void);

typedef struct
{
    QofBook *book;
    Account *acc;
    gnc_commodity *usd;
} Fixture;

static void
setup (Fixture *fixture, gconstpointer pData)
{
    fixture->book = qof_book_new ();
    fixture->acc = xaccMallocAccount (fixture->book);
    fixture->usd = gnc_commodity_new (fixture->book, "US Dollar", "CURRENCY", "USD", "0", 100);
    xaccAccountSetCommodity (fixture->acc, fixture->usd);
}

static void
teardown (Fixture *fixture, gconstpointer pData)
{
    qof_book_destroy (fixture->book);
}

static Split*
create_test_split (Fixture *fixture, gnc_numeric amount, time64 posted)
{
    Transaction *txn = xaccMallocTransaction (fixture->book);
    Split *split = xaccMallocSplit (fixture->book);

    xaccTransBeginEdit (txn);
    xaccTransSetCurrency (txn, fixture->usd);
    xaccTransSetDatePostedSecs (txn, posted);

    split->acc = fixture->acc;
    xaccSplitSetParent (split, txn);

    xaccSplitSetAmount (split, amount);
    xaccSplitSetValue (split, amount);

    xaccTransCommitEdit (txn);
    gnc_account_insert_split (fixture->acc, split);
    return split;
}

static void
test_fifo_get_policy (Fixture *fixture, gconstpointer pData)
{
    GNCPolicy *pcy = xaccGetFIFOPolicy ();
    g_assert_nonnull (pcy);
    g_assert_nonnull (pcy->PolicyGetLot);
    g_assert_nonnull (pcy->PolicyGetSplit);
    g_assert_nonnull (pcy->PolicyGetLotOpening);
    g_assert_nonnull (pcy->PolicyIsOpeningSplit);
}

static void
test_fifo_policy_get_lot (Fixture *fixture, gconstpointer pData)
{
    GNCPolicy *pcy = xaccGetFIFOPolicy ();

    /* Test with NULL split */
    g_assert_null (pcy->PolicyGetLot (pcy, NULL));

    /* Setup a split (SELL) */
    Split *split = create_test_split (fixture, gnc_numeric_create (-50, 1), 2000);

    /* FIFOPolicyGetLot calls xaccAccountFindEarliestOpenLot
       Since there are no lots, it should return NULL */
    GNCLot *lot = pcy->PolicyGetLot (pcy, split);
    g_assert_null (lot);

    /* Create a lot and add an earlier split (BUY) */
    GNCLot *new_lot = gnc_lot_new (fixture->book);
    xaccAccountInsertLot (fixture->acc, new_lot);

    Split *split2 = create_test_split (fixture, gnc_numeric_create (100, 1), 1000);
    gnc_lot_add_split (new_lot, split2);

    gnc_numeric bal = gnc_lot_get_balance (new_lot);
    g_assert_false (gnc_numeric_zero_p (bal));
    g_assert_false (gnc_lot_is_closed (new_lot));

    lot = pcy->PolicyGetLot (pcy, split);
    g_assert_nonnull (lot);
    g_assert_true (lot == new_lot);
}

static void
test_fifo_policy_get_split (Fixture *fixture, gconstpointer pData)
{
    GNCPolicy *pcy = xaccGetFIFOPolicy ();

    /* Test with NULL lot */
    g_assert_null (pcy->PolicyGetSplit (pcy, NULL));

    GNCLot *lot = gnc_lot_new (fixture->book);
    xaccAccountInsertLot (fixture->acc, lot);

    /* Empty lot */
    g_assert_null (pcy->PolicyGetSplit (pcy, lot));

    /* Add a split to the lot to establish balance and currency (BUY) */
    Split *split1 = create_test_split (fixture, gnc_numeric_create (100, 1), 1000);
    gnc_lot_add_split (lot, split1);

    /* Add an unassigned split to the account (SELL) */
    Split *split2 = create_test_split (fixture, gnc_numeric_create (-50, 1), 2000);

    Split *found = pcy->PolicyGetSplit (pcy, lot);
    g_assert_nonnull (found);
    g_assert_true (found == split2);
}

static void
test_fifo_policy_lot_opening (Fixture *fixture, gconstpointer pData)
{
    GNCPolicy *pcy = xaccGetFIFOPolicy ();
    GNCLot *lot = gnc_lot_new (fixture->book);

    gnc_numeric amt = gnc_numeric_create (100, 1);
    Split *split = create_test_split (fixture, amt, 1000);
    gnc_lot_add_split (lot, split);

    gnc_numeric ret_amt, ret_val;
    gnc_commodity *ret_curr;

    pcy->PolicyGetLotOpening (pcy, lot, &ret_amt, &ret_val, &ret_curr);

    g_assert_true (gnc_numeric_equal (ret_amt, xaccSplitGetAmount(split)));
    g_assert_true (gnc_numeric_equal (ret_val, xaccSplitGetValue(split)));
    g_assert_true (ret_curr == fixture->usd);
}

static void
test_fifo_policy_is_opening (Fixture *fixture, gconstpointer pData)
{
    GNCPolicy *pcy = xaccGetFIFOPolicy ();
    GNCLot *lot = gnc_lot_new (fixture->book);

    Split *split1 = create_test_split (fixture, gnc_numeric_create (100, 1), 1000);
    gnc_lot_add_split (lot, split1);

    Split *split2 = create_test_split (fixture, gnc_numeric_create (50, 1), 2000);
    gnc_lot_add_split (lot, split2);

    g_assert_true (pcy->PolicyIsOpeningSplit (pcy, lot, split1));
    g_assert_false (pcy->PolicyIsOpeningSplit (pcy, lot, split2));
}

extern "C"
{
void
test_suite_policy (void)
{
    GNC_TEST_ADD (suitename, "fifo get policy", Fixture, NULL, setup, test_fifo_get_policy, teardown);
    GNC_TEST_ADD (suitename, "fifo policy get lot", Fixture, NULL, setup, test_fifo_policy_get_lot, teardown);
    GNC_TEST_ADD (suitename, "fifo policy get split", Fixture, NULL, setup, test_fifo_policy_get_split, teardown);
    GNC_TEST_ADD (suitename, "fifo policy lot opening", Fixture, NULL, setup, test_fifo_policy_lot_opening, teardown);
    GNC_TEST_ADD (suitename, "fifo policy is opening", Fixture, NULL, setup, test_fifo_policy_is_opening, teardown);
}
}
