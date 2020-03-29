/********************************************************************
 * gtest-import-account-matcher.cpp --                              *
 *                        unit tests import-account-matcher.        *
 * Copyright (C) 2020 John Ralls <jralls@ceridwen.us>               *
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
 *******************************************************************/

#include <gtest/gtest.h>
extern "C"
{
#include <config.h>
#include <import-account-matcher.h>
#include <gnc-session.h>
#include <qofbook.h>
#include <Account.h>
#include <gtk/gtk.h>
}
#include <vector>

using AccountV = std::vector<const Account*>;
using AccountTypeV = std::vector<GNCAccountType>;
using AccountPair = std::pair<AccountV&,
                              const AccountTypeV&>;

class ImportMatcherTest : public ::testing::Test
{
protected:
    ImportMatcherTest() :
        m_book{gnc_get_current_book()}, m_root{gnc_account_create_root(m_book)}
    {
        auto create_account = [this](Account* parent, GNCAccountType type,
                                     const char* name,
                                     const char* online)->Account* {
            auto account = xaccMallocAccount(this->m_book);
            xaccAccountBeginEdit(account);
            xaccAccountSetType(account, type);
            xaccAccountSetName(account, name);
            xaccAccountBeginEdit(parent);
            gnc_account_append_child(parent, account);
            if (online)
                qof_instance_set(QOF_INSTANCE(account), "online-id", online, NULL);
            xaccAccountCommitEdit(parent);
            xaccAccountCommitEdit(account);
            return account;
        };
        auto assets = create_account(m_root, ACCT_TYPE_ASSET,
                                     "Assets", nullptr);
        auto liabilities = create_account(m_root, ACCT_TYPE_LIABILITY,
                                          "Liabilities", nullptr);
        auto expenses = create_account(m_root, ACCT_TYPE_EXPENSE,
                                       "Expenses", nullptr);
        create_account(assets, ACCT_TYPE_BANK, "Bank", "Bank");
        auto broker = create_account(assets, ACCT_TYPE_ASSET,
                                     "Broker", "Broker");
        auto stocks = create_account(broker, ACCT_TYPE_STOCK,
                                     "Stocks", "BrokerStocks");
        create_account(stocks, ACCT_TYPE_STOCK, "AAPL", "BrokerStocksAAPL");
        create_account(stocks, ACCT_TYPE_STOCK, "MSFT", "BrokerStocksMSFT ");
        create_account(stocks, ACCT_TYPE_STOCK, "HPE", "BrokerStocksHPE");
        create_account(broker, ACCT_TYPE_BANK, "Cash Management",
                       "BrokerCash Management");
       create_account(expenses, ACCT_TYPE_EXPENSE, "Food", nullptr);
        create_account(expenses, ACCT_TYPE_EXPENSE, "Gas", nullptr);
        create_account(expenses, ACCT_TYPE_EXPENSE, "Rent", nullptr);
   }
    ~ImportMatcherTest()
    {
        xaccAccountBeginEdit(m_root);
        xaccAccountDestroy(m_root); //It does the commit
        gnc_clear_current_session();
    }

    QofBook* m_book;
    Account* m_root;
};

TEST_F(ImportMatcherTest, test_simple_match)
{
    auto found = gnc_import_select_account(nullptr, "Bank", FALSE, nullptr,
                                           nullptr, ACCT_TYPE_NONE, nullptr,
                                           nullptr);
    ASSERT_NE(nullptr, found);
    EXPECT_STREQ("Bank", xaccAccountGetName(found));
}

TEST_F(ImportMatcherTest, test_noisy_match)
{
    auto found = gnc_import_select_account(nullptr, "BankUSD", FALSE, nullptr,
                                           nullptr, ACCT_TYPE_NONE, nullptr,
                                           nullptr);
    ASSERT_NE(nullptr, found);
    EXPECT_STREQ("Bank", xaccAccountGetName(found));
}

TEST_F(ImportMatcherTest, test_match_with_subaccounts)
{
    auto found = gnc_import_select_account(nullptr, "BrokerStocks", FALSE,
                                           nullptr, nullptr, ACCT_TYPE_NONE,
                                           nullptr, nullptr);
    ASSERT_NE(nullptr, found);
    EXPECT_STREQ("Stocks", xaccAccountGetName(found));
}

TEST_F(ImportMatcherTest, test_subaccount_match)
{
    auto found = gnc_import_select_account(nullptr, "BrokerStocksHPE", FALSE,
                                           nullptr, nullptr, ACCT_TYPE_NONE,
                                           nullptr, nullptr);
    ASSERT_NE(nullptr, found);
    EXPECT_STREQ("HPE", xaccAccountGetName(found));
}

TEST_F(ImportMatcherTest, test_subaccount_match_trailing_noise)
{
    auto found = gnc_import_select_account(nullptr, "BrokerStocksHPEUSD", FALSE,
                                           nullptr, nullptr, ACCT_TYPE_NONE,
                                           nullptr, nullptr);
    ASSERT_NE(nullptr, found);
    EXPECT_STREQ("HPE", xaccAccountGetName(found));
}

TEST_F(ImportMatcherTest, test_subaccount_no_match)
{
    auto found = gnc_import_select_account(nullptr, "BrokerStocksINTC", FALSE,
                                           nullptr, nullptr, ACCT_TYPE_STOCK,
                                           nullptr, nullptr);
    ASSERT_EQ(nullptr, found);
}

TEST_F(ImportMatcherTest, test_subaccount_match_trailing_space)
{
    auto found = gnc_import_select_account(nullptr, "BrokerStocksMSFT ", FALSE,
                                           nullptr, nullptr, ACCT_TYPE_NONE,
                                           nullptr, nullptr);
    ASSERT_NE(nullptr, found);
    EXPECT_STREQ("MSFT", xaccAccountGetName(found));
}

TEST_F(ImportMatcherTest, test_subaccount_match_trim_trailing_space)
{
    auto found = gnc_import_select_account(nullptr, "BrokerStocksMSFT", FALSE,
                                           nullptr, nullptr, ACCT_TYPE_NONE,
                                           nullptr, nullptr);
    ASSERT_NE(nullptr, found);
    EXPECT_STREQ("MSFT", xaccAccountGetName(found));
}

TEST_F(ImportMatcherTest, test_subaccount_match_internal_space)
{
    auto found = gnc_import_select_account(nullptr, "BrokerCash Management",
                                           FALSE, nullptr, nullptr,
                                           ACCT_TYPE_NONE, nullptr, nullptr);
    ASSERT_NE(nullptr, found);
    EXPECT_STREQ("Cash Management", xaccAccountGetName(found));
}
