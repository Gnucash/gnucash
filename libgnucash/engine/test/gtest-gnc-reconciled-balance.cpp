/********************************************************************
 * gtest-gnc-reconciled-balance.cpp: Test reconciled balances.        *
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
\********************************************************************/

#include <config.h>

#include "../Account.h"
#include "../Split.h"
#include "../Transaction.h"
#include "../gnc-reconciled-balance.h"
#include "../gnc-commodity.h"
#include "../gnc-features.h"
#include "../cashobjects.h"

#include <qof.h>

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wcpp"
#include <gtest/gtest.h>
#pragma GCC diagnostic pop

/* qof_init and cashobjects_register are process-wide, and calling them
 * twice fails. Both fixtures come through here. */
static void
ensure_engine_ready ()
{
    static bool ready = false;

    if (!ready)
    {
        qof_init ();
        g_assert_true (cashobjects_register ());
        ready = true;
    }
}

static const time64 JAN_15 = 1673740800; /* 2023-01-15 00:00:00 UTC */
static const time64 FEB_15 = 1676419200; /* 2023-02-15 00:00:00 UTC */

class ReconciledBalanceTest : public testing::Test
{
protected:
    void SetUp () override
    {
        ensure_engine_ready ();

        m_book = qof_book_new ();
        m_root = gnc_account_create_root (m_book);
        m_curr = gnc_commodity_new (m_book, "Dollar", "CURRENCY", "USD",
                                    nullptr, 100);

        m_bank = make_account ("Bank", ACCT_TYPE_BANK);
        m_income = make_account ("Income", ACCT_TYPE_INCOME);
    }

    void TearDown () override
    {
        qof_book_destroy (m_book);
        m_book = nullptr;
    }

    Account *make_account (const char *name, GNCAccountType type)
    {
        Account *acc = xaccMallocAccount (m_book);
        xaccAccountBeginEdit (acc);
        xaccAccountSetName (acc, name);
        xaccAccountSetType (acc, type);
        xaccAccountSetCommodity (acc, m_curr);
        xaccAccountCommitEdit (acc);
        gnc_account_append_child (m_root, acc);
        return acc;
    }

    /* Post `amount' into m_bank from m_income on `date'. Returns the
     * bank-side split, so a test can reconcile it. */
    Split *add_transaction (time64 date, gnc_numeric amount)
    {
        Transaction *trans = xaccMallocTransaction (m_book);
        xaccTransBeginEdit (trans);
        xaccTransSetCurrency (trans, m_curr);
        xaccTransSetDatePostedSecs (trans, date);

        Split *to = xaccMallocSplit (m_book);
        xaccSplitSetParent (to, trans);
        xaccSplitSetAccount (to, m_bank);
        xaccSplitSetValue (to, amount);
        xaccSplitSetAmount (to, amount);

        Split *from = xaccMallocSplit (m_book);
        xaccSplitSetParent (from, trans);
        xaccSplitSetAccount (from, m_income);
        xaccSplitSetValue (from, gnc_numeric_neg (amount));
        xaccSplitSetAmount (from, gnc_numeric_neg (amount));

        xaccTransCommitEdit (trans);

        return to;
    }

    /* Mark a split reconciled as the reconcile window does: state 'y',
     * with the *statement* date as the split's reconcile date. */
    void reconcile_split (Split *split, time64 statement_date)
    {
        auto trans = xaccSplitGetParent (split);
        xaccTransBeginEdit (trans);
        xaccSplitSetReconcile (split, YREC);
        xaccSplitSetDateReconciledSecs (split, statement_date);
        xaccTransCommitEdit (trans);
    }

    GncReconciledBalance *make_record (Account *acc, time64 date,
                                         gnc_numeric amount)
    {
        GncReconciledBalance *ba = gnc_reconciled_balance_new (m_book);
        gnc_reconciled_balance_set_account (ba, acc);
        gnc_reconciled_balance_set_date (ba, date);
        gnc_reconciled_balance_set_amount (ba, amount);
        return ba;
    }

    static gnc_numeric dollars (int n) { return gnc_numeric_create (n * 100, 100); }

    QofBook *m_book = nullptr;
    Account *m_root = nullptr;
    Account *m_bank = nullptr;
    Account *m_income = nullptr;
    gnc_commodity *m_curr = nullptr;
};

TEST_F (ReconciledBalanceTest, NewRecordRoundTripsItsFields)
{
    GncReconciledBalance *ba = make_record (m_bank, JAN_15, dollars (50));

    EXPECT_EQ (m_bank, gnc_reconciled_balance_get_account (ba));
    EXPECT_TRUE (gnc_numeric_equal (dollars (50),
                                    gnc_reconciled_balance_get_amount (ba)));

    gnc_reconciled_balance_set_notes (ba, "statement 3");
    EXPECT_STREQ ("statement 3", gnc_reconciled_balance_get_notes (ba));

    /* The date is stored day-neutral, so it need not come back
     * bit-identical -- but it must stay on the same day. */
    EXPECT_EQ (gnc_time64_get_day_start (JAN_15),
               gnc_time64_get_day_start (gnc_reconciled_balance_get_date (ba)));
}

TEST_F (ReconciledBalanceTest, LookupFindsRecordInBook)
{
    GncReconciledBalance *ba = make_record (m_bank, JAN_15, dollars (50));
    const GncGUID *guid = gnc_reconciled_balance_get_guid (ba);

    EXPECT_EQ (ba, gnc_reconciled_balance_lookup (guid, m_book));
}

TEST_F (ReconciledBalanceTest, CreatingARecordFlagsTheBookFeature)
{
    EXPECT_FALSE (gnc_features_check_used (m_book,
                                           GNC_FEATURE_RECONCILED_BALANCES));
    make_record (m_bank, JAN_15, dollars (50));
    EXPECT_TRUE (gnc_features_check_used (m_book,
                                          GNC_FEATURE_RECONCILED_BALANCES));
}

TEST_F (ReconciledBalanceTest, MatchingBalanceHolds)
{
    add_transaction (JAN_15, dollars (50));
    auto rb = make_record (m_bank, JAN_15, dollars (50));

    EXPECT_EQ (GNC_RECONCILED_BALANCE_HOLDS,
               gnc_reconciled_balance_get_status (rb));
    EXPECT_FALSE (gnc_reconciled_balance_is_broken (rb));
    EXPECT_TRUE (gnc_numeric_zero_p (gnc_reconciled_balance_get_delta (rb)));
}

TEST_F (ReconciledBalanceTest, MismatchedBalanceIsBroken)
{
    add_transaction (JAN_15, dollars (50));
    auto rb = make_record (m_bank, JAN_15, dollars (40));

    EXPECT_EQ (GNC_RECONCILED_BALANCE_BROKEN,
               gnc_reconciled_balance_get_status (rb));
    EXPECT_TRUE (gnc_numeric_equal (dollars (10),
                                    gnc_reconciled_balance_get_delta (rb)));
}

/* The seal is on what the book held, so reconcile state is beside the
 * point -- an uncleared transaction counts exactly as much as a
 * reconciled one. */
TEST_F (ReconciledBalanceTest, EverythingPostedCountsWhateverItsState)
{
    reconcile_split (add_transaction (JAN_15, dollars (50)), JAN_15);
    add_transaction (JAN_15, dollars (30));      /* left unreconciled */

    auto rb = make_record (m_bank, JAN_15, dollars (80));
    EXPECT_EQ (GNC_RECONCILED_BALANCE_HOLDS,
               gnc_reconciled_balance_get_status (rb));
}

TEST_F (ReconciledBalanceTest, LaterDatedTransactionsDoNotAffectIt)
{
    add_transaction (JAN_15, dollars (50));
    auto rb = make_record (m_bank, JAN_15, dollars (50));

    add_transaction (FEB_15, dollars (25));

    EXPECT_EQ (GNC_RECONCILED_BALANCE_HOLDS,
               gnc_reconciled_balance_get_status (rb));
}

TEST_F (ReconciledBalanceTest, DeletingAnEarlierTransactionBreaksIt)
{
    auto split = add_transaction (JAN_15, dollars (50));
    auto rb = make_record (m_bank, JAN_15, dollars (50));
    ASSERT_EQ (GNC_RECONCILED_BALANCE_HOLDS,
               gnc_reconciled_balance_get_status (rb));

    auto trans = xaccSplitGetParent (split);
    xaccTransBeginEdit (trans);
    xaccTransDestroy (trans);
    xaccTransCommitEdit (trans);

    EXPECT_EQ (GNC_RECONCILED_BALANCE_BROKEN,
               gnc_reconciled_balance_get_status (rb));
}

TEST_F (ReconciledBalanceTest, EditingAnEarlierTransactionBreaksIt)
{
    auto split = add_transaction (JAN_15, dollars (50));
    auto rb = make_record (m_bank, JAN_15, dollars (50));
    ASSERT_EQ (GNC_RECONCILED_BALANCE_HOLDS,
               gnc_reconciled_balance_get_status (rb));

    auto trans = xaccSplitGetParent (split);
    xaccTransBeginEdit (trans);
    xaccSplitSetValue (split, dollars (55));
    xaccSplitSetAmount (split, dollars (55));
    xaccTransCommitEdit (trans);

    EXPECT_EQ (GNC_RECONCILED_BALANCE_BROKEN,
               gnc_reconciled_balance_get_status (rb));
}

/* A duplicate arriving from a CSV or OFX import lands dated inside an
 * already-sealed period and is CREC, never YREC. Placing splits by
 * reconcile state would miss it entirely; this is the case the seal
 * exists for. */
TEST_F (ReconciledBalanceTest, ABackDatedInsertBreaksIt)
{
    add_transaction (JAN_15, dollars (50));
    auto rb = make_record (m_bank, JAN_15, dollars (50));
    ASSERT_EQ (GNC_RECONCILED_BALANCE_HOLDS,
               gnc_reconciled_balance_get_status (rb));

    auto dupe = add_transaction (JAN_15, dollars (50));
    auto trans = xaccSplitGetParent (dupe);
    xaccTransBeginEdit (trans);
    xaccSplitSetReconcile (dupe, CREC);
    xaccTransCommitEdit (trans);

    EXPECT_EQ (GNC_RECONCILED_BALANCE_BROKEN,
               gnc_reconciled_balance_get_status (rb));
}

/* The regression test for the whole design. The importer stamps every
 * matched split's reconcile date with today (import-backend.cpp), so a
 * rule that placed splits by that date would break every record in the
 * book after a routine import. Nothing about the balance changed here,
 * so nothing may break. */
TEST_F (ReconciledBalanceTest, ImportRestampingReconcileDatesDoesNotBreakIt)
{
    auto split = add_transaction (JAN_15, dollars (50));
    reconcile_split (split, JAN_15);

    auto rb = make_record (m_bank, JAN_15, dollars (50));
    ASSERT_EQ (GNC_RECONCILED_BALANCE_HOLDS,
               gnc_reconciled_balance_get_status (rb));

    reconcile_split (split, gnc_time (nullptr));   /* as the importer does */

    EXPECT_EQ (GNC_RECONCILED_BALANCE_HOLDS,
               gnc_reconciled_balance_get_status (rb));
}

TEST_F (ReconciledBalanceTest, UnreconcilingASplitDoesNotBreakIt)
{
    auto split = add_transaction (JAN_15, dollars (50));
    reconcile_split (split, JAN_15);

    auto rb = make_record (m_bank, JAN_15, dollars (50));
    ASSERT_EQ (GNC_RECONCILED_BALANCE_HOLDS,
               gnc_reconciled_balance_get_status (rb));

    auto trans = xaccSplitGetParent (split);
    xaccTransBeginEdit (trans);
    xaccSplitSetReconcile (split, NREC);
    xaccTransCommitEdit (trans);

    /* The money is still there; only its state changed. */
    EXPECT_EQ (GNC_RECONCILED_BALANCE_HOLDS,
               gnc_reconciled_balance_get_status (rb));
}

TEST_F (ReconciledBalanceTest, VoidingRemovesTheAmountAndBreaksIt)
{
    auto split = add_transaction (JAN_15, dollars (50));
    auto rb = make_record (m_bank, JAN_15, dollars (50));
    ASSERT_EQ (GNC_RECONCILED_BALANCE_HOLDS,
               gnc_reconciled_balance_get_status (rb));

    auto trans = xaccSplitGetParent (split);
    xaccTransBeginEdit (trans);
    xaccTransVoid (trans, "test");
    xaccTransCommitEdit (trans);

    EXPECT_EQ (GNC_RECONCILED_BALANCE_BROKEN,
               gnc_reconciled_balance_get_status (rb));
}

/* One transaction entered after the fact with an earlier date breaks
 * every record dated on or after it, all by the same amount. That equal
 * delta is what the GUI reads to tell a late entry from real damage. */
TEST_F (ReconciledBalanceTest, ALateEntryBreaksEverySealOnOrAfterIt)
{
    add_transaction (JAN_15, dollars (100));
    auto jan = make_record (m_bank, JAN_15, dollars (100));
    auto feb = make_record (m_bank, FEB_15, dollars (100));

    ASSERT_EQ (GNC_RECONCILED_BALANCE_HOLDS,
               gnc_reconciled_balance_get_status (jan));
    ASSERT_EQ (GNC_RECONCILED_BALANCE_HOLDS,
               gnc_reconciled_balance_get_status (feb));

    /* The cheque was written on the 15th of January and entered now. */
    add_transaction (JAN_15, dollars (-20));

    EXPECT_EQ (GNC_RECONCILED_BALANCE_BROKEN,
               gnc_reconciled_balance_get_status (jan));
    EXPECT_EQ (GNC_RECONCILED_BALANCE_BROKEN,
               gnc_reconciled_balance_get_status (feb));
    EXPECT_TRUE (gnc_numeric_equal (gnc_reconciled_balance_get_delta (jan),
                                    gnc_reconciled_balance_get_delta (feb)));
    EXPECT_TRUE (gnc_numeric_equal (dollars (-20),
                                    gnc_reconciled_balance_get_delta (jan)));
}

TEST_F (ReconciledBalanceTest, ResealAdoptsTheNewBalanceAndReturnsTheOld)
{
    add_transaction (JAN_15, dollars (100));
    auto rb = make_record (m_bank, JAN_15, dollars (100));

    add_transaction (JAN_15, dollars (-20));
    ASSERT_EQ (GNC_RECONCILED_BALANCE_BROKEN,
               gnc_reconciled_balance_get_status (rb));

    auto was = gnc_reconciled_balance_reseal (rb);

    EXPECT_TRUE (gnc_numeric_equal (dollars (100), was));
    EXPECT_TRUE (gnc_numeric_equal (dollars (80),
                                    gnc_reconciled_balance_get_amount (rb)));
    EXPECT_EQ (GNC_RECONCILED_BALANCE_HOLDS,
               gnc_reconciled_balance_get_status (rb));
}

TEST_F (ReconciledBalanceTest, RecordWithNoAccountIsUnknownNotBroken)
{
    auto rb = gnc_reconciled_balance_new (m_book);
    gnc_reconciled_balance_set_date (rb, JAN_15);
    gnc_reconciled_balance_set_amount (rb, dollars (50));

    EXPECT_EQ (nullptr, gnc_reconciled_balance_get_account (rb));
    EXPECT_EQ (GNC_RECONCILED_BALANCE_UNKNOWN,
               gnc_reconciled_balance_get_status (rb));
    EXPECT_FALSE (gnc_reconciled_balance_is_broken (rb));
}

TEST_F (ReconciledBalanceTest, ListsAreSortedByDateAndFilteredByAccount)
{
    make_record (m_bank, FEB_15, dollars (75));
    make_record (m_bank, JAN_15, dollars (50));
    make_record (m_income, JAN_15, dollars (0));

    GList *all = gnc_reconciled_balance_get_all (m_book);
    ASSERT_EQ (3u, g_list_length (all));
    g_list_free (all);

    GList *bank = gnc_reconciled_balance_get_for_account (m_bank);
    ASSERT_EQ (2u, g_list_length (bank));
    EXPECT_EQ (gnc_time64_get_day_start (JAN_15),
               gnc_time64_get_day_start (gnc_reconciled_balance_get_date
                                         (GNC_RECONCILED_BALANCE (bank->data))));
    g_list_free (bank);
}

TEST_F (ReconciledBalanceTest, CountsOnlyBrokenRecords)
{
    add_transaction (JAN_15, dollars (50));
    make_record (m_bank, JAN_15, dollars (50)); /* holds */
    make_record (m_bank, FEB_15, dollars (99)); /* broken */

    EXPECT_EQ (1u, gnc_reconciled_balance_count_broken (m_book));
    EXPECT_EQ (1u, gnc_reconciled_balance_count_broken_for_account (m_bank));
    EXPECT_EQ (0u, gnc_reconciled_balance_count_broken_for_account (m_income));

    GList *broken = gnc_reconciled_balance_get_broken (m_book);
    EXPECT_EQ (1u, g_list_length (broken));
    g_list_free (broken);
}

TEST_F (ReconciledBalanceTest, DestroyRemovesRecordFromTheBook)
{
    auto rb = make_record (m_bank, JAN_15, dollars (50));
    GncGUID guid = *gnc_reconciled_balance_get_guid (rb);

    gnc_reconciled_balance_destroy (rb);

    EXPECT_EQ (nullptr, gnc_reconciled_balance_lookup (&guid, m_book));
    EXPECT_EQ (nullptr, gnc_reconciled_balance_get_all (m_book));
}

/* A record about an account that no longer exists could never be
 * checked again, so it goes when the account does. */
TEST_F (ReconciledBalanceTest, DeletingTheAccountRemovesItsRecords)
{
    make_record (m_bank, JAN_15, dollars (50));
    make_record (m_income, JAN_15, dollars (0));

    GList *before = gnc_reconciled_balance_get_all (m_book);
    ASSERT_EQ (2u, g_list_length (before));
    g_list_free (before);

    xaccAccountBeginEdit (m_bank);
    xaccAccountDestroy (m_bank);
    m_bank = nullptr;

    GList *remaining = gnc_reconciled_balance_get_all (m_book);
    EXPECT_EQ (1u, g_list_length (remaining));
    if (remaining)
    {
        EXPECT_EQ (m_income, gnc_reconciled_balance_get_account
                   (GNC_RECONCILED_BALANCE (remaining->data)));
    }
    g_list_free (remaining);
}
