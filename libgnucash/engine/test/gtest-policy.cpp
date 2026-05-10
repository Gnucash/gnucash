/********************************************************************
 * gtest-policy.cpp: Google Test suite for policy.cpp.              *
 ********************************************************************/

#include <gtest/gtest.h>

#include <config.h>

#include "Account.h"
#include "Transaction.h"
#include "Split.h"
#include "SplitP.hpp"
#include "gnc-lot.h"
#include "policy.h"
#include "policy-p.h"

class PolicyTest : public ::testing::Test
{
protected:
    QofBook *book;
    Account *acc;
    gnc_commodity *usd;
    gnc_commodity *eur;

    void SetUp() override
    {
        book = qof_book_new();
        acc = xaccMallocAccount(book);
        usd = gnc_commodity_new(book, "US Dollar", "CURRENCY", "USD", "0", 100);
        eur = gnc_commodity_new(book, "Euro", "CURRENCY", "EUR", "0", 100);
        xaccAccountSetCommodity(acc, usd);
    }

    void TearDown() override
    {
        qof_book_destroy(book);
    }

    Split* create_test_split(gnc_numeric amount, time64 posted, gnc_commodity* curr = nullptr)
    {
        if (!curr) curr = usd;
        Transaction *txn = xaccMallocTransaction(book);
        Split *split = xaccMallocSplit(book);

        xaccTransBeginEdit(txn);
        xaccTransSetCurrency(txn, curr);
        xaccTransSetDatePostedSecs(txn, posted);

        split->acc = acc;
        xaccSplitSetParent(split, txn);

        xaccSplitSetAmount(split, amount);
        xaccSplitSetValue(split, amount);

        xaccTransCommitEdit(txn);
        gnc_account_insert_split(acc, split);
        return split;
    }
};

TEST_F(PolicyTest, FIFOGetPolicy)
{
    GNCPolicy *pcy = xaccGetFIFOPolicy();
    ASSERT_NE(pcy, nullptr);
    EXPECT_NE(pcy->PolicyGetLot, nullptr);
    EXPECT_NE(pcy->PolicyGetSplit, nullptr);
    EXPECT_NE(pcy->PolicyGetLotOpening, nullptr);
    EXPECT_NE(pcy->PolicyIsOpeningSplit, nullptr);
}

TEST_F(PolicyTest, FIFOPolicyGetLot)
{
    GNCPolicy *pcy = xaccGetFIFOPolicy();

    /* Test with NULL split */
    EXPECT_EQ(pcy->PolicyGetLot(pcy, nullptr), nullptr);

    /* Setup a split (SELL) */
    Split *split = create_test_split(gnc_numeric_create(-50, 1), 2000);

    /* FIFOPolicyGetLot calls xaccAccountFindEarliestOpenLot
       Since there are no lots, it should return NULL */
    GNCLot *lot = pcy->PolicyGetLot(pcy, split);
    EXPECT_EQ(lot, nullptr);

    /* Create a lot and add an earlier split (BUY) */
    GNCLot *new_lot = gnc_lot_new(book);
    xaccAccountInsertLot(acc, new_lot);

    Split *split2 = create_test_split(gnc_numeric_create(100, 1), 1000);
    gnc_lot_add_split(new_lot, split2);

    gnc_numeric bal = gnc_lot_get_balance(new_lot);
    EXPECT_FALSE(gnc_numeric_zero_p(bal));
    EXPECT_FALSE(gnc_lot_is_closed(new_lot));

    lot = pcy->PolicyGetLot(pcy, split);
    ASSERT_NE(lot, nullptr);
    EXPECT_EQ(lot, new_lot);
}

TEST_F(PolicyTest, FIFOPolicyGetSplit_Basic)
{
    GNCPolicy *pcy = xaccGetFIFOPolicy();

    /* Test with NULL lot */
    EXPECT_EQ(pcy->PolicyGetSplit(pcy, nullptr), nullptr);

    GNCLot *lot = gnc_lot_new(book);
    xaccAccountInsertLot(acc, lot);

    /* Empty lot */
    EXPECT_EQ(pcy->PolicyGetSplit(pcy, lot), nullptr);

    /* Add a split to the lot to establish balance and currency (BUY) */
    Split *split1 = create_test_split(gnc_numeric_create(100, 1), 1000);
    gnc_lot_add_split(lot, split1);

    /* Add an unassigned split to the account (SELL) */
    Split *split2 = create_test_split(gnc_numeric_create(-50, 1), 2000);

    Split *found = pcy->PolicyGetSplit(pcy, lot);
    ASSERT_NE(found, nullptr);
    EXPECT_EQ(found, split2);
}

TEST_F(PolicyTest, FIFOPolicyGetSplit_ClosedLot)
{
    GNCPolicy *pcy = xaccGetFIFOPolicy();
    GNCLot *lot = gnc_lot_new(book);
    xaccAccountInsertLot(acc, lot);

    Split *split1 = create_test_split(gnc_numeric_create(100, 1), 1000);
    gnc_lot_add_split(lot, split1);

    Split *split2 = create_test_split(gnc_numeric_create(-100, 1), 2000);
    gnc_lot_add_split(lot, split2);

    EXPECT_TRUE(gnc_lot_is_closed(lot));

    /* Even if there's another unassigned split, PolicyGetSplit should return nullptr for a closed lot */
    create_test_split(gnc_numeric_create(-50, 1), 3000);

    EXPECT_EQ(pcy->PolicyGetSplit(pcy, lot), nullptr);
}

TEST_F(PolicyTest, FIFOPolicyGetSplit_NoUnassignedSplits)
{
    GNCPolicy *pcy = xaccGetFIFOPolicy();
    GNCLot *lot = gnc_lot_new(book);
    xaccAccountInsertLot(acc, lot);

    Split *split1 = create_test_split(gnc_numeric_create(100, 1), 1000);
    gnc_lot_add_split(lot, split1);

    EXPECT_EQ(pcy->PolicyGetSplit(pcy, lot), nullptr);
}

TEST_F(PolicyTest, FIFOPolicyGetSplit_DifferentCurrency)
{
    GNCPolicy *pcy = xaccGetFIFOPolicy();
    GNCLot *lot = gnc_lot_new(book);
    xaccAccountInsertLot(acc, lot);

    Split *split1 = create_test_split(gnc_numeric_create(100, 1), 1000, usd);
    gnc_lot_add_split(lot, split1);

    /* Unassigned split with different currency */
    create_test_split(gnc_numeric_create(-50, 1), 2000, eur);

    EXPECT_EQ(pcy->PolicyGetSplit(pcy, lot), nullptr);
}

TEST_F(PolicyTest, FIFOPolicyGetSplit_NegativeBalanceLot)
{
    GNCPolicy *pcy = xaccGetFIFOPolicy();
    GNCLot *lot = gnc_lot_new(book);
    xaccAccountInsertLot(acc, lot);

    /* Lot opened with a SELL */
    Split *split1 = create_test_split(gnc_numeric_create(-100, 1), 1000);
    gnc_lot_add_split(lot, split1);

    /* Should find a BUY split to offset the SELL */
    Split *split2 = create_test_split(gnc_numeric_create(50, 1), 2000);

    Split *found = pcy->PolicyGetSplit(pcy, lot);
    ASSERT_NE(found, nullptr);
    EXPECT_EQ(found, split2);
}

TEST_F(PolicyTest, FIFOPolicyLotOpening)
{
    GNCPolicy *pcy = xaccGetFIFOPolicy();
    GNCLot *lot = gnc_lot_new(book);

    gnc_numeric amt = gnc_numeric_create(100, 1);
    Split *split = create_test_split(amt, 1000);
    gnc_lot_add_split(lot, split);

    gnc_numeric ret_amt, ret_val;
    gnc_commodity *ret_curr;

    pcy->PolicyGetLotOpening(pcy, lot, &ret_amt, &ret_val, &ret_curr);

    EXPECT_TRUE(gnc_numeric_equal(ret_amt, xaccSplitGetAmount(split)));
    EXPECT_TRUE(gnc_numeric_equal(ret_val, xaccSplitGetValue(split)));
    EXPECT_EQ(ret_curr, usd);
}

TEST_F(PolicyTest, FIFOPolicyIsOpening)
{
    GNCPolicy *pcy = xaccGetFIFOPolicy();
    GNCLot *lot = gnc_lot_new(book);

    Split *split1 = create_test_split(gnc_numeric_create(100, 1), 1000);
    gnc_lot_add_split(lot, split1);

    Split *split2 = create_test_split(gnc_numeric_create(50, 1), 2000);
    gnc_lot_add_split(lot, split2);

    EXPECT_TRUE(pcy->PolicyIsOpeningSplit(pcy, lot, split1));
    EXPECT_FALSE(pcy->PolicyIsOpeningSplit(pcy, lot, split2));
}
