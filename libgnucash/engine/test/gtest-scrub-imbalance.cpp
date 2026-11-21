/********************************************************************\
 * gtest-scrub-imbalance.cpp -- Unit tests for the imbalance split  *
 *                              handling in xaccTransScrubImbalance *
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
 *                                                                  *
\********************************************************************/

#include <config.h>
#include <glib.h>
#include <glib/gi18n.h>
#include <qof.h>
#include "../Account.h"
#include "../Split.h"
#include "../Transaction.h"
#include "../cashobjects.h"
#include "../gnc-commodity.h"
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wcpp"
#include <gtest/gtest.h>
#pragma GCC diagnostic pop

#include <initializer_list>
#include <string>

/* Committing a transaction runs xaccTransScrubImbalance, which is what is
 * under test. */
class ScrubImbalanceTest : public testing::Test
{
protected:
    void SetUp() override
    {
        static const gboolean engine_ready = []
        {
            qof_init ();
            return cashobjects_register ();
        }();
        ASSERT_TRUE (engine_ready);

        book = qof_book_new ();
        root = gnc_account_create_root (book);
        currency = gnc_commodity_new (book, "Gnu Rand", "CURRENCY", "GNR",
                                      "", 100);

        bank = make_account ("Bank", ACCT_TYPE_BANK);
        expense = make_account ("Expense", ACCT_TYPE_EXPENSE);
        other_expense = make_account ("Other", ACCT_TYPE_EXPENSE);

        /* Same name, type and commodity that the scrub code looks for. */
        imbalance = make_account (std::string (_("Imbalance")) + "-GNR",
                                  ACCT_TYPE_BANK);
    }

    void TearDown() override
    {
        qof_book_destroy (book);
    }

    Account *make_account (const std::string& name, GNCAccountType type)
    {
        auto acc = xaccMallocAccount (book);
        xaccAccountBeginEdit (acc);
        xaccAccountSetName (acc, name.c_str ());
        xaccAccountSetType (acc, type);
        xaccAccountSetCommodity (acc, currency);
        xaccAccountCommitEdit (acc);
        gnc_account_append_child (root, acc);
        return acc;
    }

    struct SplitSpec { Account *acc; int cents; const char *memo; };

    /* Build a transaction in a single edit, so the commit is what scrubs it. */
    Transaction *make_txn (std::initializer_list<SplitSpec> specs)
    {
        auto trans = xaccMallocTransaction (book);
        xaccTransBeginEdit (trans);
        xaccTransSetCurrency (trans, currency);
        for (auto& s : specs)
        {
            auto split = xaccMallocSplit (book);
            xaccSplitSetParent (split, trans);
            xaccSplitSetAccount (split, s.acc);
            xaccSplitSetValue (split, gnc_numeric_create (s.cents, 100));
            xaccSplitSetAmount (split, gnc_numeric_create (s.cents, 100));
            if (s.memo)
                xaccSplitSetMemo (split, s.memo);
        }
        xaccTransCommitEdit (trans);
        return trans;
    }

    void set_cents (Transaction *trans, Account *acc, int cents)
    {
        auto split = xaccTransFindSplitByAccount (trans, acc);
        xaccSplitSetValue (split, gnc_numeric_create (cents, 100));
        xaccSplitSetAmount (split, gnc_numeric_create (cents, 100));
    }

    QofBook *book = nullptr;
    Account *root = nullptr;
    Account *bank = nullptr;
    Account *expense = nullptr;
    Account *other_expense = nullptr;
    Account *imbalance = nullptr;
    gnc_commodity *currency = nullptr;
};

/* The bug: the other splits balance and the imbalance split is zero. The
 * transaction counts as balanced, so the scrub used to return early and
 * leave the split behind. */
TEST_F (ScrubImbalanceTest, zero_imbalance_split_removed_when_balanced)
{
    auto trans = make_txn ({{bank, 10000, nullptr},
                            {expense, -10000, nullptr},
                            {imbalance, 0, nullptr}});

    EXPECT_EQ (xaccTransCountSplits (trans), 2);
    EXPECT_EQ (xaccTransFindSplitByAccount (trans, imbalance), nullptr);
}

/* The same through an edit of an existing transaction, as in the register:
 * the other splits get fixed and the imbalance split is zeroed. */
TEST_F (ScrubImbalanceTest, zeroed_imbalance_split_removed_on_edit)
{
    auto trans = make_txn ({{bank, 10000, nullptr},
                            {expense, -9000, nullptr}});
    ASSERT_NE (xaccTransFindSplitByAccount (trans, imbalance), nullptr);

    xaccTransBeginEdit (trans);
    set_cents (trans, expense, -10000);
    set_cents (trans, imbalance, 0);
    xaccTransCommitEdit (trans);

    EXPECT_EQ (xaccTransCountSplits (trans), 2);
    EXPECT_EQ (xaccTransFindSplitByAccount (trans, imbalance), nullptr);
}

/* Same rule as add_balance_split(): a memo keeps the split. */
TEST_F (ScrubImbalanceTest, zero_imbalance_split_with_memo_kept)
{
    auto trans = make_txn ({{bank, 10000, nullptr},
                            {expense, -10000, nullptr},
                            {imbalance, 0, "keep"}});

    EXPECT_EQ (xaccTransCountSplits (trans), 3);
    EXPECT_NE (xaccTransFindSplitByAccount (trans, imbalance), nullptr);
}

/* Only the imbalance account is special. */
TEST_F (ScrubImbalanceTest, zero_split_in_ordinary_account_kept)
{
    auto trans = make_txn ({{bank, 10000, nullptr},
                            {expense, -10000, nullptr},
                            {other_expense, 0, nullptr}});

    EXPECT_EQ (xaccTransCountSplits (trans), 3);
    EXPECT_NE (xaccTransFindSplitByAccount (trans, other_expense), nullptr);
}

/* An imbalance split that is doing its job stays. */
TEST_F (ScrubImbalanceTest, needed_imbalance_split_kept)
{
    auto trans = make_txn ({{bank, 10000, nullptr},
                            {expense, -9000, nullptr},
                            {imbalance, -1000, nullptr}});

    EXPECT_EQ (xaccTransCountSplits (trans), 3);
    EXPECT_NE (xaccTransFindSplitByAccount (trans, imbalance), nullptr);
    EXPECT_TRUE (xaccTransIsBalanced (trans));
}
