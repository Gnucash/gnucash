/********************************************************************\
 * gtest-account-merge.cpp -- Test merging account trees.           *
 *                                                                  *
 * Copyright (C) 2026 Isak Adamssvärd                               *
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
 * along with this program; if not, see                             *
 * <https://www.gnu.org/licenses/>                                  *
\********************************************************************/
#include <glib.h>

#include <config.h>

#include <string>
#include <utility>
#include <vector>

#include <Account.h>
#include <qofbook.h>

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wcpp"
#include <gtest/gtest.h>
#pragma GCC diagnostic pop

#include "../gnc-account-merge.h"

/* The tests merge m_new_root, which stands for the tree that the account
 * hierarchy assistant builds from the selected templates, into
 * m_existing_root, the root account of the book.
 */
class AccountMergeTest : public testing::Test
{
protected:
    void SetUp () override
    {
        m_book = qof_book_new ();
        m_existing_root = gnc_account_create_root (m_book);
        m_new_root = xaccMallocAccount (m_book);
    }

    void TearDown () override
    {
        for (auto root : {m_new_root, m_existing_root})
        {
            xaccAccountBeginEdit (root);
            xaccAccountDestroy (root);
        }
        qof_book_destroy (m_book);
    }

    Account* add_account (Account* parent, const char* name, GNCAccountType type = ACCT_TYPE_ASSET)
    {
        auto account = xaccMallocAccount (m_book);
        xaccAccountSetName (account, name);
        xaccAccountSetType (account, type);
        gnc_account_append_child (parent, account);
        return account;
    }

    QofBook* m_book {};
    Account* m_existing_root {};
    Account* m_new_root {};
};

/* The child of parent with the given name, ignoring deeper descendants. */
static Account*
child_named (const Account* parent, const std::string& name)
{
    Account* result = nullptr;
    auto children = gnc_account_get_children (parent);
    for (auto node = children; node; node = g_list_next (node))
    {
        auto child = static_cast<Account*> (node->data);
        if (name == xaccAccountGetName (child))
        {
            result = child;
            break;
        }
    }
    g_list_free (children);
    return result;
}

TEST_F (AccountMergeTest, disposition_of_an_account_pair)
{
    auto existing_bank = add_account (m_existing_root, "Bank", ACCT_TYPE_BANK);
    auto new_bank = add_account (m_new_root, "Bank", ACCT_TYPE_BANK);

    EXPECT_EQ (determine_account_merge_disposition (existing_bank, new_bank),
               GNC_ACCOUNT_MERGE_DISPOSITION_USE_EXISTING);
    EXPECT_EQ (determine_account_merge_disposition (nullptr, new_bank),
               GNC_ACCOUNT_MERGE_DISPOSITION_CREATE_NEW);
}

TEST_F (AccountMergeTest, disposition_of_accounts_at_the_same_place)
{
    auto existing_assets = add_account (m_existing_root, "Assets");
    add_account (existing_assets, "Bank", ACCT_TYPE_BANK);

    auto new_assets = add_account (m_new_root, "Assets");
    auto new_bank = add_account (new_assets, "Bank", ACCT_TYPE_BANK);
    auto new_cash = add_account (new_assets, "Cash", ACCT_TYPE_CASH);
    auto new_expenses = add_account (m_new_root, "Expenses", ACCT_TYPE_EXPENSE);
    auto new_food = add_account (new_expenses, "Food", ACCT_TYPE_EXPENSE);

    EXPECT_EQ (determine_merge_disposition (m_existing_root, new_assets),
               GNC_ACCOUNT_MERGE_DISPOSITION_USE_EXISTING);
    EXPECT_EQ (determine_merge_disposition (m_existing_root, new_bank),
               GNC_ACCOUNT_MERGE_DISPOSITION_USE_EXISTING);
    EXPECT_EQ (determine_merge_disposition (m_existing_root, new_cash),
               GNC_ACCOUNT_MERGE_DISPOSITION_CREATE_NEW);
    EXPECT_EQ (determine_merge_disposition (m_existing_root, new_expenses),
               GNC_ACCOUNT_MERGE_DISPOSITION_CREATE_NEW);
    EXPECT_EQ (determine_merge_disposition (m_existing_root, new_food),
               GNC_ACCOUNT_MERGE_DISPOSITION_CREATE_NEW);
}

TEST_F (AccountMergeTest, merge_into_an_empty_tree)
{
    auto new_assets = add_account (m_new_root, "Assets");
    auto new_bank = add_account (new_assets, "Bank", ACCT_TYPE_BANK);
    auto new_cash = add_account (new_assets, "Cash", ACCT_TYPE_CASH);
    auto new_expenses = add_account (m_new_root, "Expenses", ACCT_TYPE_EXPENSE);

    account_trees_merge (m_existing_root, m_new_root);

    /* The accounts themselves are moved, with their children. */
    EXPECT_EQ (gnc_account_get_parent (new_assets), m_existing_root);
    EXPECT_EQ (gnc_account_get_parent (new_expenses), m_existing_root);
    EXPECT_EQ (gnc_account_get_parent (new_bank), new_assets);
    EXPECT_EQ (gnc_account_get_parent (new_cash), new_assets);
    EXPECT_EQ (gnc_account_n_descendants (m_existing_root), 4);
    EXPECT_EQ (gnc_account_n_descendants (m_new_root), 0);
}

TEST_F (AccountMergeTest, merge_adds_only_the_missing_accounts)
{
    auto existing_assets = add_account (m_existing_root, "Assets");
    auto existing_bank = add_account (existing_assets, "Bank", ACCT_TYPE_BANK);

    auto new_assets = add_account (m_new_root, "Assets");
    auto new_bank = add_account (new_assets, "Bank", ACCT_TYPE_BANK);
    auto new_cash = add_account (new_assets, "Cash", ACCT_TYPE_CASH);
    auto new_expenses = add_account (m_new_root, "Expenses", ACCT_TYPE_EXPENSE);
    auto new_food = add_account (new_expenses, "Food", ACCT_TYPE_EXPENSE);

    account_trees_merge (m_existing_root, m_new_root);

    /* Accounts that the book already had stay where they are... */
    EXPECT_EQ (child_named (m_existing_root, "Assets"), existing_assets);
    EXPECT_EQ (child_named (existing_assets, "Bank"), existing_bank);
    /* ...and their counterparts are left behind in the new tree. */
    EXPECT_EQ (gnc_account_get_parent (new_assets), m_new_root);
    EXPECT_EQ (gnc_account_get_parent (new_bank), new_assets);
    EXPECT_EQ (gnc_account_n_descendants (m_new_root), 2);

    /* The missing accounts are moved into the book. */
    EXPECT_EQ (gnc_account_get_parent (new_cash), existing_assets);
    EXPECT_EQ (gnc_account_get_parent (new_expenses), m_existing_root);
    EXPECT_EQ (gnc_account_get_parent (new_food), new_expenses);
    EXPECT_EQ (gnc_account_n_descendants (m_existing_root), 5);
}

TEST_F (AccountMergeTest, merge_leaves_existing_accounts_unchanged)
{
    auto existing_bank = add_account (m_existing_root, "Bank", ACCT_TYPE_BANK);
    xaccAccountSetCode (existing_bank, "1930");
    xaccAccountSetDescription (existing_bank, "My bank account");

    auto new_bank = add_account (m_new_root, "Bank", ACCT_TYPE_ASSET);
    xaccAccountSetCode (new_bank, "1940");
    xaccAccountSetDescription (new_bank, "Bank account from the template");

    account_trees_merge (m_existing_root, m_new_root);

    EXPECT_EQ (gnc_account_n_descendants (m_existing_root), 1);
    EXPECT_STREQ (xaccAccountGetCode (existing_bank), "1930");
    EXPECT_STREQ (xaccAccountGetDescription (existing_bank), "My bank account");
    EXPECT_EQ (xaccAccountGetType (existing_bank), ACCT_TYPE_BANK);
}

TEST_F (AccountMergeTest, merging_the_same_tree_twice_adds_nothing)
{
    auto build_tree = [this] (Account* root)
    {
        auto assets = add_account (root, "Assets");
        add_account (assets, "Bank", ACCT_TYPE_BANK);
        add_account (assets, "Cash", ACCT_TYPE_CASH);
        auto expenses = add_account (root, "Expenses", ACCT_TYPE_EXPENSE);
        add_account (expenses, "Food", ACCT_TYPE_EXPENSE);
    };

    build_tree (m_new_root);
    account_trees_merge (m_existing_root, m_new_root);
    EXPECT_EQ (gnc_account_n_descendants (m_existing_root), 5);

    build_tree (m_new_root);
    account_trees_merge (m_existing_root, m_new_root);
    EXPECT_EQ (gnc_account_n_descendants (m_existing_root), 5);
    EXPECT_EQ (gnc_account_n_descendants (m_new_root), 5);
}

TEST_F (AccountMergeTest, find_child_searches_only_the_immediate_children)
{
    auto assets = add_account (m_existing_root, "Assets");
    auto current = add_account (assets, "Current Assets");
    add_account (current, "Bank", ACCT_TYPE_BANK);

    EXPECT_EQ (gnc_account_merge_find_child (m_existing_root, "Assets"), assets);
    EXPECT_EQ (gnc_account_merge_find_child (assets, "Current Assets"), current);
    EXPECT_EQ (gnc_account_merge_find_child (assets, "Bank"), nullptr);
    EXPECT_EQ (gnc_account_merge_find_child (m_existing_root, "Bank"), nullptr);
    EXPECT_EQ (gnc_account_merge_find_child (nullptr, "Bank"), nullptr);
    EXPECT_EQ (gnc_account_merge_find_child (assets, nullptr), nullptr);
}

/* The book has Assets:Current Assets:Bank and the template Assets:Bank. The
 * preview shows the template's Bank as a new account, so the merge must add
 * it instead of using the deeper Bank. */
TEST_F (AccountMergeTest, merge_does_not_use_a_deeper_account_with_the_same_name)
{
    auto existing_assets = add_account (m_existing_root, "Assets");
    auto existing_current = add_account (existing_assets, "Current Assets");
    auto existing_bank = add_account (existing_current, "Bank", ACCT_TYPE_BANK);

    auto new_assets = add_account (m_new_root, "Assets");
    auto new_bank = add_account (new_assets, "Bank", ACCT_TYPE_BANK);
    auto new_checking = add_account (new_bank, "Checking", ACCT_TYPE_BANK);

    EXPECT_EQ (determine_merge_disposition (m_existing_root, new_bank),
               GNC_ACCOUNT_MERGE_DISPOSITION_CREATE_NEW);

    account_trees_merge (m_existing_root, m_new_root);

    EXPECT_EQ (gnc_account_get_parent (new_bank), existing_assets);
    EXPECT_EQ (gnc_account_get_parent (new_checking), new_bank);
    EXPECT_EQ (gnc_account_get_parent (existing_bank), existing_current);
    EXPECT_EQ (gnc_account_n_children (existing_bank), 0);
    EXPECT_EQ (gnc_account_n_descendants (m_existing_root), 5);
}

/* Accounts with the same name under different parents are different
 * accounts, as in charts of accounts that repeat a name in several classes. */
TEST_F (AccountMergeTest, merge_matches_the_name_under_the_same_parent)
{
    auto existing_income = add_account (m_existing_root, "Income", ACCT_TYPE_INCOME);
    auto existing_other_income = add_account (existing_income, "Other", ACCT_TYPE_INCOME);

    auto new_expenses = add_account (m_new_root, "Expenses", ACCT_TYPE_EXPENSE);
    auto new_other_expenses = add_account (new_expenses, "Other", ACCT_TYPE_EXPENSE);
    auto new_income = add_account (m_new_root, "Income", ACCT_TYPE_INCOME);
    auto new_other_income = add_account (new_income, "Other", ACCT_TYPE_INCOME);

    account_trees_merge (m_existing_root, m_new_root);

    EXPECT_EQ (gnc_account_get_parent (new_expenses), m_existing_root);
    EXPECT_EQ (gnc_account_get_parent (new_other_expenses), new_expenses);
    EXPECT_EQ (gnc_account_get_parent (new_other_income), new_income);
    EXPECT_EQ (gnc_account_n_children (existing_income), 1);
    EXPECT_EQ (child_named (existing_income, "Other"), existing_other_income);
    EXPECT_EQ (gnc_account_n_descendants (m_existing_root), 4);
}

/* Every account that the preview shows as new ends up in the book, and every
 * account that it shows as existing is left behind in the template tree. */
TEST_F (AccountMergeTest, merge_does_what_the_preview_shows)
{
    auto existing_assets = add_account (m_existing_root, "Assets");
    auto existing_current = add_account (existing_assets, "Current Assets");
    add_account (existing_current, "Bank", ACCT_TYPE_BANK);
    add_account (existing_current, "Cash", ACCT_TYPE_CASH);
    auto existing_expenses = add_account (m_existing_root, "Expenses", ACCT_TYPE_EXPENSE);
    add_account (existing_expenses, "Food", ACCT_TYPE_EXPENSE);

    auto new_assets = add_account (m_new_root, "Assets");
    auto new_bank = add_account (new_assets, "Bank", ACCT_TYPE_BANK);
    auto new_current = add_account (new_assets, "Current Assets");
    auto new_cash = add_account (new_current, "Cash", ACCT_TYPE_CASH);
    auto new_savings = add_account (new_current, "Savings", ACCT_TYPE_BANK);
    auto new_expenses = add_account (m_new_root, "Expenses", ACCT_TYPE_EXPENSE);
    auto new_groceries = add_account (new_expenses, "Groceries", ACCT_TYPE_EXPENSE);
    auto new_food = add_account (new_groceries, "Food", ACCT_TYPE_EXPENSE);

    std::vector<std::pair<Account*, GncAccountMergeDisposition>> preview;
    for (auto account : {new_assets, new_bank, new_current, new_cash, new_savings,
                         new_expenses, new_groceries, new_food})
        preview.emplace_back (account, determine_merge_disposition (m_existing_root, account));

    account_trees_merge (m_existing_root, m_new_root);

    for (const auto& [account, disposition] : preview)
    {
        auto in_book = gnc_account_get_root (account) == m_existing_root;
        EXPECT_EQ (in_book, disposition == GNC_ACCOUNT_MERGE_DISPOSITION_CREATE_NEW)
            << xaccAccountGetName (account);
    }
    EXPECT_EQ (gnc_account_n_descendants (m_existing_root), 10);
}
