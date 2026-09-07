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
#include "../Account.hpp"
#include "../Split.h"
#include "../Transaction.h"
#include "../gnc-reconciled-balance.h"
#include "../gnc-commodity.h"
#include "../gnc-features.h"
#include "../cashobjects.h"

#include <qof.h>

#include <map>
#include <vector>

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

/* ================================================================ *
 * A whole small book, sealed at three statement dates, then damaged *
 * in each of the ways a reconciled period actually gets mangled.    *
 * ================================================================ */

class ReconciledBalanceBookTest : public testing::Test
{
protected:
    void SetUp () override
    {
        ensure_engine_ready ();

        m_book = qof_book_new ();
        m_root = gnc_account_create_root (m_book);
        m_usd = gnc_commodity_new (m_book, "Dollar", "CURRENCY", "USD",
                                   nullptr, 100);

        m_bank    = account ("Bank", ACCT_TYPE_BANK);
        m_cash    = account ("Cash", ACCT_TYPE_CASH);
        m_card    = account ("Credit Card", ACCT_TYPE_CREDIT);
        m_income  = account ("Salary", ACCT_TYPE_INCOME);
        m_expense = account ("Expenses", ACCT_TYPE_EXPENSE);

        build_book ();
        seal_statements ();
    }

    void TearDown () override
    {
        qof_book_destroy (m_book);
        m_book = nullptr;
    }

    Account *account (const char *name, GNCAccountType type)
    {
        Account *acc = xaccMallocAccount (m_book);
        xaccAccountBeginEdit (acc);
        xaccAccountSetName (acc, name);
        xaccAccountSetType (acc, type);
        xaccAccountSetCommodity (acc, m_usd);
        xaccAccountCommitEdit (acc);
        gnc_account_append_child (m_root, acc);
        return acc;
    }

    static time64 on (int d, int m, int y) { return gnc_dmy2time64 (d, m, y); }
    static gnc_numeric money (int cents) { return gnc_numeric_create (cents, 100); }

    /* Move `cents' out of `from' and into `to'. */
    Transaction *post (time64 date, Account *from, Account *to, int cents,
                       const char *desc)
    {
        auto amount = money (cents);
        auto trans = xaccMallocTransaction (m_book);

        xaccTransBeginEdit (trans);
        xaccTransSetCurrency (trans, m_usd);
        xaccTransSetDatePostedSecs (trans, date);
        xaccTransSetDescription (trans, desc);

        auto credit = xaccMallocSplit (m_book);
        xaccSplitSetParent (credit, trans);
        xaccSplitSetAccount (credit, to);
        xaccSplitSetValue (credit, amount);
        xaccSplitSetAmount (credit, amount);

        auto debit = xaccMallocSplit (m_book);
        xaccSplitSetParent (debit, trans);
        xaccSplitSetAccount (debit, from);
        xaccSplitSetValue (debit, gnc_numeric_neg (amount));
        xaccSplitSetAmount (debit, gnc_numeric_neg (amount));

        xaccTransCommitEdit (trans);
        return trans;
    }

    static Split *split_of (Transaction *trans, Account *acc)
    {
        for (auto n = xaccTransGetSplitList (trans); n; n = n->next)
            if (xaccSplitGetAccount (GNC_SPLIT (n->data)) == acc)
                return GNC_SPLIT (n->data);
        return nullptr;
    }

    /* Mark an account's side of a transaction reconciled, the way the
     * reconcile window does: state 'y', stamped with the statement date. */
    static void reconcile (Transaction *trans, Account *acc, time64 statement)
    {
        auto split = split_of (trans, acc);
        g_assert_nonnull (split);

        xaccTransBeginEdit (trans);
        xaccSplitSetReconcile (split, YREC);
        xaccSplitSetDateReconciledSecs (split, statement);
        xaccTransCommitEdit (trans);
    }

    /* Three months of an ordinary current account, a credit card and a
     * little cash, all reconciled through the January statement. */
    void build_book ()
    {
        auto nov = on (30, 11, 2025);
        auto dec = on (31, 12, 2025);
        auto jan = on (31,  1, 2026);

        /* November: +3000 -150 -80 = 2770 */
        reconcile (post (on ( 5, 11, 2025), m_income, m_bank, 300000, "Salary"),
                   m_bank, nov);
        m_nov_groceries = post (on (12, 11, 2025), m_bank, m_expense, 15000,
                                "Groceries");
        reconcile (m_nov_groceries, m_bank, nov);
        m_nov_utilities = post (on (20, 11, 2025), m_bank, m_expense, 8000,
                                "Utilities");
        reconcile (m_nov_utilities, m_bank, nov);

        /* December: 2770 +3000 -200 -120 = 5450 */
        m_dec_salary = post (on (5, 12, 2025), m_income, m_bank, 300000, "Salary");
        reconcile (m_dec_salary, m_bank, dec);
        reconcile (post (on (15, 12, 2025), m_bank, m_expense, 20000, "Groceries"),
                   m_bank, dec);
        reconcile (post (on (24, 12, 2025), m_bank, m_expense, 12000, "Restaurant"),
                   m_bank, dec);

        /* January: 5450 +3000 -180 -90 -200 = 7980 */
        m_jan_salary = post (on (5, 1, 2026), m_income, m_bank, 300000, "Salary");
        reconcile (m_jan_salary, m_bank, jan);
        reconcile (post (on (15, 1, 2026), m_bank, m_expense, 18000, "Groceries"),
                   m_bank, jan);
        reconcile (post (on (28, 1, 2026), m_bank, m_expense, 9000, "Utilities"),
                   m_bank, jan);

        /* Credit card: -200 charged in December, paid off in January. */
        reconcile (post (on (10, 12, 2025), m_card, m_expense, 20000, "Restaurant"),
                   m_card, dec);
        auto payment = post (on (10, 1, 2026), m_bank, m_card, 20000, "Card payment");
        reconcile (payment, m_card, jan);
        reconcile (payment, m_bank, jan);

        /* A little cash, never touched again. */
        reconcile (post (on (2, 11, 2025), m_income, m_cash, 10000, "Cash float"),
                   m_cash, nov);
    }

    /* Seal each account at each statement date at whatever the book says
     * -- exactly what finishing a reconciliation does. */
    void seal_statements ()
    {
        for (auto acc : { m_bank, m_card, m_cash })
            for (auto date : { on (30, 11, 2025), on (31, 12, 2025),
                               on (31, 1, 2026) })
                m_seals[acc].push_back (seal (acc, date));
    }

    GncReconciledBalance *seal (Account *acc, time64 date)
    {
        auto rb = gnc_reconciled_balance_new (m_book);
        gnc_reconciled_balance_set_account (rb, acc);
        gnc_reconciled_balance_set_date (rb, date);
        gnc_reconciled_balance_set_amount
            (rb, gnc_reconciled_balance_compute (acc, date));
        return rb;
    }

    GncReconciledBalance *nov (Account *acc) { return m_seals[acc][0]; }
    GncReconciledBalance *dec (Account *acc) { return m_seals[acc][1]; }
    GncReconciledBalance *jan (Account *acc) { return m_seals[acc][2]; }

    static bool holds (GncReconciledBalance *rb)
    {
        return gnc_reconciled_balance_get_status (rb) ==
               GNC_RECONCILED_BALANCE_HOLDS;
    }

    static gnc_numeric delta (GncReconciledBalance *rb)
    {
        return gnc_reconciled_balance_get_delta (rb);
    }

    void expect_all_hold ()
    {
        for (auto& entry : m_seals)
            for (auto rb : entry.second)
                EXPECT_TRUE (holds (rb))
                    << xaccAccountGetName (entry.first) << " seal is broken";
    }

    static void set_post_date (Transaction *trans, time64 date)
    {
        xaccTransBeginEdit (trans);
        xaccTransSetDatePostedSecs (trans, date);
        xaccTransCommitEdit (trans);
    }

    static void destroy (Transaction *trans)
    {
        xaccTransBeginEdit (trans);
        xaccTransDestroy (trans);
        xaccTransCommitEdit (trans);
    }

    QofBook *m_book = nullptr;
    Account *m_root = nullptr;
    Account *m_bank = nullptr, *m_cash = nullptr, *m_card = nullptr;
    Account *m_income = nullptr, *m_expense = nullptr;
    gnc_commodity *m_usd = nullptr;

    Transaction *m_nov_groceries = nullptr, *m_nov_utilities = nullptr;
    Transaction *m_dec_salary = nullptr, *m_jan_salary = nullptr;

    std::map<Account*, std::vector<GncReconciledBalance*>> m_seals;
};

/* The book as built is self-consistent, and the balances are the ones
 * the arithmetic says they are. */
TEST_F (ReconciledBalanceBookTest, TheBookStartsSound)
{
    expect_all_hold ();

    EXPECT_TRUE (gnc_numeric_equal (money (277000),
                                    gnc_reconciled_balance_get_amount (nov (m_bank))));
    EXPECT_TRUE (gnc_numeric_equal (money (545000),
                                    gnc_reconciled_balance_get_amount (dec (m_bank))));
    EXPECT_TRUE (gnc_numeric_equal (money (798000),
                                    gnc_reconciled_balance_get_amount (jan (m_bank))));
    EXPECT_TRUE (gnc_numeric_equal (money (-20000),
                                    gnc_reconciled_balance_get_amount (dec (m_card))));
    EXPECT_TRUE (gnc_numeric_equal (money (0),
                                    gnc_reconciled_balance_get_amount (jan (m_card))));
    EXPECT_TRUE (gnc_numeric_equal (money (10000),
                                    gnc_reconciled_balance_get_amount (nov (m_cash))));
    EXPECT_EQ (0u, gnc_reconciled_balance_count_broken (m_book));
}

/* ---- the four ways a reconciled period gets mangled ---- */

/* 1. Accidentally deleting an old reconciled transaction. */
TEST_F (ReconciledBalanceBookTest, DeletingAnOldReconciledTransactionIsCaught)
{
    destroy (m_nov_groceries);                  /* 150.00, dated 12 Nov */

    EXPECT_FALSE (holds (nov (m_bank)));
    EXPECT_FALSE (holds (dec (m_bank)));
    EXPECT_FALSE (holds (jan (m_bank)));

    /* The money came back, so every seal is out by the same +150. */
    EXPECT_TRUE (gnc_numeric_equal (money (15000), delta (nov (m_bank))));
    EXPECT_TRUE (gnc_numeric_equal (money (15000), delta (dec (m_bank))));
    EXPECT_TRUE (gnc_numeric_equal (money (15000), delta (jan (m_bank))));
}

/* 2. Accidentally entering an old date on a transaction. */
TEST_F (ReconciledBalanceBookTest, ABackDatedEntryIsCaught)
{
    post (on (20, 12, 2025), m_bank, m_expense, 7500, "Mistyped year");

    EXPECT_TRUE  (holds (nov (m_bank)));        /* before the damage */
    EXPECT_FALSE (holds (dec (m_bank)));
    EXPECT_FALSE (holds (jan (m_bank)));
    EXPECT_TRUE (gnc_numeric_equal (money (-7500), delta (dec (m_bank))));
    EXPECT_TRUE (gnc_numeric_equal (money (-7500), delta (jan (m_bank))));
}

/* 3. Importing old data that failed to match what was already there.
 *    The importer marks its splits CREC, never YREC, which is exactly
 *    why a check over reconciled splits alone could not see this. */
TEST_F (ReconciledBalanceBookTest, AnImportedDuplicateIsCaught)
{
    auto dupe = post (on (5, 1, 2026), m_income, m_bank, 300000, "Salary");
    auto split = split_of (dupe, m_bank);

    xaccTransBeginEdit (dupe);
    xaccSplitSetReconcile (split, CREC);
    xaccSplitSetDateReconciledSecs (split, gnc_time (nullptr));
    xaccTransCommitEdit (dupe);

    EXPECT_TRUE  (holds (nov (m_bank)));
    EXPECT_TRUE  (holds (dec (m_bank)));
    EXPECT_FALSE (holds (jan (m_bank)));
    EXPECT_TRUE (gnc_numeric_equal (money (300000), delta (jan (m_bank))));
}

/* 4. A cheque written in November and only entered now. Every seal on or
 *    after it breaks by the same amount -- the signature the GUI reads
 *    to tell a late entry from scattered damage. */
TEST_F (ReconciledBalanceBookTest, ALateEnteredChequeIsCaughtWithAUniformDelta)
{
    post (on (25, 11, 2025), m_bank, m_expense, 6000, "Cheque 1234");

    EXPECT_FALSE (holds (nov (m_bank)));
    EXPECT_FALSE (holds (dec (m_bank)));
    EXPECT_FALSE (holds (jan (m_bank)));

    auto d = delta (nov (m_bank));
    EXPECT_TRUE (gnc_numeric_equal (money (-6000), d));
    EXPECT_TRUE (gnc_numeric_equal (d, delta (dec (m_bank))));
    EXPECT_TRUE (gnc_numeric_equal (d, delta (jan (m_bank))));
}

/* ---- other routes to the same damage ---- */

TEST_F (ReconciledBalanceBookTest, EditingAReconciledAmountIsCaught)
{
    auto split = split_of (m_dec_salary, m_bank);

    xaccTransBeginEdit (m_dec_salary);
    xaccSplitSetValue (split, money (310000));
    xaccSplitSetAmount (split, money (310000));
    xaccTransCommitEdit (m_dec_salary);

    EXPECT_TRUE  (holds (nov (m_bank)));
    EXPECT_FALSE (holds (dec (m_bank)));
    EXPECT_FALSE (holds (jan (m_bank)));
    EXPECT_TRUE (gnc_numeric_equal (money (10000), delta (dec (m_bank))));
}

/* Dragging a transaction backwards into a sealed period. */
TEST_F (ReconciledBalanceBookTest, MovingATransactionIntoASealedPeriodIsCaught)
{
    set_post_date (m_jan_salary, on (28, 12, 2025));

    EXPECT_TRUE  (holds (nov (m_bank)));
    EXPECT_FALSE (holds (dec (m_bank)));
    EXPECT_TRUE (gnc_numeric_equal (money (300000), delta (dec (m_bank))));
    /* January's total is unchanged: the money is still inside it. */
    EXPECT_TRUE  (holds (jan (m_bank)));
}

/* And forwards, out of one. */
TEST_F (ReconciledBalanceBookTest, MovingATransactionOutOfASealedPeriodIsCaught)
{
    set_post_date (m_nov_utilities, on (5, 12, 2025));

    EXPECT_FALSE (holds (nov (m_bank)));
    EXPECT_TRUE (gnc_numeric_equal (money (8000), delta (nov (m_bank))));
    EXPECT_TRUE  (holds (dec (m_bank)));
    EXPECT_TRUE  (holds (jan (m_bank)));
}

/* Re-pointing a split at another account damages both sides. */
TEST_F (ReconciledBalanceBookTest, MovingASplitToAnotherAccountIsCaughtOnBothAccounts)
{
    xaccTransBeginEdit (m_nov_groceries);
    xaccSplitSetAccount (split_of (m_nov_groceries, m_bank), m_cash);
    xaccTransCommitEdit (m_nov_groceries);

    EXPECT_FALSE (holds (nov (m_bank)));
    EXPECT_FALSE (holds (nov (m_cash)));
    EXPECT_TRUE (gnc_numeric_equal (money (15000), delta (nov (m_bank))));
    EXPECT_TRUE (gnc_numeric_equal (money (-15000), delta (nov (m_cash))));
}

TEST_F (ReconciledBalanceBookTest, VoidingAnOldTransactionIsCaught)
{
    xaccTransBeginEdit (m_nov_utilities);
    xaccTransVoid (m_nov_utilities, "entered in error");
    xaccTransCommitEdit (m_nov_utilities);

    EXPECT_FALSE (holds (nov (m_bank)));
    EXPECT_TRUE (gnc_numeric_equal (money (8000), delta (nov (m_bank))));
}

/* Damage stays where it happened. */
TEST_F (ReconciledBalanceBookTest, DamageToOneAccountLeavesTheOthersIntact)
{
    destroy (m_nov_groceries);

    EXPECT_FALSE (holds (nov (m_bank)));
    for (auto rb : { nov (m_card), dec (m_card), jan (m_card),
                     nov (m_cash), dec (m_cash), jan (m_cash) })
        EXPECT_TRUE (holds (rb));

    EXPECT_EQ (3u, gnc_reconciled_balance_count_broken_for_account (m_bank));
    EXPECT_EQ (0u, gnc_reconciled_balance_count_broken_for_account (m_card));
    EXPECT_EQ (3u, gnc_reconciled_balance_count_broken (m_book));
}

/* Two unrelated pieces of damage at once, on two accounts. */
TEST_F (ReconciledBalanceBookTest, TwoKindsOfDamageAtOnceAreBothReported)
{
    destroy (m_nov_utilities);                                    /* deletion */
    post (on (15, 12, 2025), m_card, m_expense, 5000, "Duplicate charge");

    EXPECT_FALSE (holds (nov (m_bank)));
    EXPECT_FALSE (holds (dec (m_bank)));
    EXPECT_FALSE (holds (jan (m_bank)));
    EXPECT_TRUE  (holds (nov (m_card)));
    EXPECT_FALSE (holds (dec (m_card)));
    EXPECT_FALSE (holds (jan (m_card)));
    EXPECT_TRUE  (holds (nov (m_cash)));

    EXPECT_EQ (5u, gnc_reconciled_balance_count_broken (m_book));
}

/* ---- things that must NOT be reported ---- */

/* The regression test for the whole design. A CSV or OFX import stamps
 * every matched split's reconcile date with today (import-backend.cpp),
 * including splits reconciled long ago. A check that placed splits by
 * that date would break the entire book after a routine import. No
 * money moved here, so nothing may break. */
TEST_F (ReconciledBalanceBookTest, ImporterRestampingReconcileDatesReportsNothing)
{
    auto today = gnc_time (nullptr);

    for (auto acc : { m_bank, m_card, m_cash })
        for (auto split : xaccAccountGetSplits (acc))
            if (xaccSplitGetReconcile (split) == YREC)
            {
                auto trans = xaccSplitGetParent (split);
                xaccTransBeginEdit (trans);
                xaccSplitSetDateReconciledSecs (split, today);
                xaccTransCommitEdit (trans);
            }

    expect_all_hold ();
    EXPECT_EQ (0u, gnc_reconciled_balance_count_broken (m_book));
}

/* Un-reconciling changes a split's state, not the account's balance. */
TEST_F (ReconciledBalanceBookTest, UnreconcilingASplitReportsNothing)
{
    auto split = split_of (m_nov_groceries, m_bank);

    xaccTransBeginEdit (m_nov_groceries);
    xaccSplitSetReconcile (split, NREC);
    xaccTransCommitEdit (m_nov_groceries);

    expect_all_hold ();
}

/* Ordinary work after the last statement date. */
TEST_F (ReconciledBalanceBookTest, EntriesAfterTheLastSealReportNothing)
{
    post (on ( 5, 2, 2026), m_income, m_bank, 300000, "February salary");
    post (on (14, 2, 2026), m_bank, m_expense, 4500, "Groceries");
    post (on (20, 2, 2026), m_card, m_expense, 9900, "Restaurant");

    expect_all_hold ();
}

/* Editing what a transaction says, rather than what it is worth. */
TEST_F (ReconciledBalanceBookTest, NonFinancialEditsReportNothing)
{
    xaccTransBeginEdit (m_dec_salary);
    xaccTransSetDescription (m_dec_salary, "Salary (corrected payee)");
    xaccTransSetNum (m_dec_salary, "REF-99");
    xaccSplitSetMemo (split_of (m_dec_salary, m_bank), "a memo");
    xaccTransCommitEdit (m_dec_salary);

    expect_all_hold ();
}

/* A difference below the commodity's smallest unit is not a difference:
 * the delta is rounded to the account's SCU before it is judged. */
TEST_F (ReconciledBalanceBookTest, SubUnitDifferencesAreNotReported)
{
    auto rb = jan (m_bank);
    auto amount = gnc_reconciled_balance_get_amount (rb);

    /* Four tenths of a cent above the real balance. */
    gnc_reconciled_balance_set_amount
        (rb, gnc_numeric_add (amount, gnc_numeric_create (4, 1000),
                              GNC_DENOM_AUTO, GNC_HOW_DENOM_LCD));

    EXPECT_TRUE (holds (rb));
}

/* ---- recovery ---- */

/* The late cheque was real: accept it, and the seals stand again at the
 * balances the book now has. */
TEST_F (ReconciledBalanceBookTest, ResealingAfterALateEntryRestoresEverySeal)
{
    post (on (25, 11, 2025), m_bank, m_expense, 6000, "Cheque 1234");
    ASSERT_EQ (3u, gnc_reconciled_balance_count_broken_for_account (m_bank));

    for (auto rb : { nov (m_bank), dec (m_bank), jan (m_bank) })
        EXPECT_FALSE (gnc_numeric_zero_p (gnc_reconciled_balance_reseal (rb)));

    expect_all_hold ();
    EXPECT_EQ (0u, gnc_reconciled_balance_count_broken (m_book));

    /* November is now 2770 - 60. */
    EXPECT_TRUE (gnc_numeric_equal (money (271000),
                                    gnc_reconciled_balance_get_amount (nov (m_bank))));
}

/* Re-sealing one record says nothing about the others. */
TEST_F (ReconciledBalanceBookTest, ResealingOneSealLeavesTheRestBroken)
{
    post (on (25, 11, 2025), m_bank, m_expense, 6000, "Cheque 1234");

    gnc_reconciled_balance_reseal (nov (m_bank));

    EXPECT_TRUE  (holds (nov (m_bank)));
    EXPECT_FALSE (holds (dec (m_bank)));
    EXPECT_FALSE (holds (jan (m_bank)));
    EXPECT_EQ (2u, gnc_reconciled_balance_count_broken_for_account (m_bank));
}

/* Separate pieces of damage give unequal deltas, which is what tells the
 * user it is not one late entry. */
TEST_F (ReconciledBalanceBookTest, SeparateDamageGivesUnequalDeltas)
{
    post (on (25, 11, 2025), m_bank, m_expense, 6000, "Cheque 1234");
    destroy (m_dec_salary);

    ASSERT_FALSE (holds (nov (m_bank)));
    ASSERT_FALSE (holds (dec (m_bank)));

    EXPECT_FALSE (gnc_numeric_equal (delta (nov (m_bank)),
                                     delta (dec (m_bank))));
}

/* Deleting the account takes its seals with it and leaves the rest. */
TEST_F (ReconciledBalanceBookTest, DeletingAnAccountRemovesOnlyItsOwnSeals)
{
    ASSERT_EQ (9u, g_list_length (gnc_reconciled_balance_get_all (m_book)));

    xaccAccountBeginEdit (m_cash);
    xaccAccountDestroyAllTransactions (m_cash);
    xaccAccountDestroy (m_cash);
    m_cash = nullptr;

    auto remaining = gnc_reconciled_balance_get_all (m_book);
    EXPECT_EQ (6u, g_list_length (remaining));
    g_list_free (remaining);

    for (auto rb : { nov (m_bank), dec (m_bank), jan (m_bank) })
        EXPECT_TRUE (holds (rb));
}
