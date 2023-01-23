/********************************************************************
 * gtest-gnc-optiondb.cpp -- unit tests for GncOption class.        *
 * Copyright (C) 2019 John Ralls <jralls@ceridwen.us>               *
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
#include "gnc-optiondb.hpp"
#include "gnc-optiondb-impl.hpp"
#include "gnc-option-ui.hpp"
#include "kvp-value.hpp"
#include <glib-2.0/glib.h>

#include "gnc-session.h"

using GncOptionDBPtr = std::unique_ptr<GncOptionDB>;

class GncOptionDBTest : public ::testing::Test
{
protected:
    GncOptionDBTest() : m_db{std::make_unique<GncOptionDB>()} {}

    GncOptionDBPtr m_db;
};

TEST_F(GncOptionDBTest, test_ctor)
{
    EXPECT_NO_THROW ({ GncOptionDB optiondb; });
}

TEST_F(GncOptionDBTest, test_register_option)
{
    GncOption option1{"foo", "bar", "baz", "Phony Option",
                      std::string{"waldo"}};
    m_db->register_option("foo", std::move(option1));
    EXPECT_EQ(1u, m_db->num_sections());
}

TEST_F(GncOptionDBTest, test_lookup_string_option)
{
    GncOption option1{"foo", "bar", "baz", "Phony Option",
                      std::string{"waldo"}};
    m_db->register_option("foo", std::move(option1));
    EXPECT_STREQ("waldo", m_db->lookup_string_option("foo", "bar").c_str());
}

TEST_F(GncOptionDBTest, test_unregister_option)
{
    GncOption option1{"foo", "bar", "baz", "Phony Option",
                      std::string{"waldo"}};
    m_db->register_option("foo", std::move(option1));
    m_db->unregister_option("foo", "bar");
    EXPECT_TRUE(m_db->lookup_string_option("foo", "bar").empty());
}

TEST_F(GncOptionDBTest, test_register_string_option)
{
    gnc_register_string_option(m_db, "foo", "bar", "baz", "Phony Option",
                               std::string{"waldo"});
    EXPECT_STREQ("waldo", m_db->lookup_string_option("foo", "bar").c_str());
}

TEST_F(GncOptionDBTest, test_register_report_placement_option)
{
    uint32_t report_id = 456;
    uint32_t wide = 2, high = 2;
    GncOptionReportPlacementVec rp{{report_id, wide, high}};

    gnc_register_report_placement_option(m_db, "foo", "bar");
    auto option{m_db->find_option("foo", "bar")};
    option->set_value(rp);
    auto value{option->get_value<GncOptionReportPlacementVec>()};
    EXPECT_EQ(value.size(), 1u);
    auto [v_id, v_wide, v_height] = value.at(0);
    EXPECT_EQ(report_id, v_id);
}

/* Note: The following test-fixture code is also present in slightly different
 * form in gtest-gnc-option.cpp.
 */


struct GncOptionDBAccountTest : public ::testing::Test
{
    GncOptionDBAccountTest() :
        m_sess{gnc_get_current_session()},
        m_book{qof_session_get_book(gnc_get_current_session())},
        m_root{gnc_account_create_root(m_book)},
        m_db{std::make_unique<GncOptionDB>()}
    {
        auto create_account = [this](Account* parent, GNCAccountType type,
                                       const char* name)->Account* {
            auto account = xaccMallocAccount(this->m_book);
            xaccAccountBeginEdit(account);
            xaccAccountSetType(account, type);
            xaccAccountSetName(account, name);
            xaccAccountBeginEdit(parent);
            gnc_account_append_child(parent, account);
            xaccAccountCommitEdit(parent);
            xaccAccountCommitEdit(account);
            return account;
        };
        auto assets = create_account(m_root, ACCT_TYPE_ASSET, "Assets");
        auto liabilities = create_account(m_root, ACCT_TYPE_LIABILITY, "Liabilities");
        auto expenses = create_account(m_root, ACCT_TYPE_EXPENSE, "Expenses");
        create_account(assets, ACCT_TYPE_BANK, "Bank");
        auto broker = create_account(assets, ACCT_TYPE_ASSET, "Broker");
        auto stocks = create_account(broker, ACCT_TYPE_STOCK, "Stocks");
        create_account(stocks, ACCT_TYPE_STOCK, "AAPL");
        create_account(stocks, ACCT_TYPE_STOCK, "MSFT");
        create_account(stocks, ACCT_TYPE_STOCK, "HPE");
        create_account(broker, ACCT_TYPE_BANK, "Cash Management");
        create_account(expenses, ACCT_TYPE_EXPENSE, "Food");
        create_account(expenses, ACCT_TYPE_EXPENSE, "Gas");
        create_account(expenses, ACCT_TYPE_EXPENSE, "Rent");
   }
    ~GncOptionDBAccountTest()
    {
        xaccAccountBeginEdit(m_root);
        xaccAccountDestroy(m_root); //It does the commit
        gnc_clear_current_session();
    }

    QofSession* m_sess;
    QofBook* m_book;
    Account* m_root;
    GncOptionDBPtr m_db;
};

TEST_F(GncOptionDBAccountTest, test_register_account_list_option)
{
    auto acclist{gnc_account_list_from_types(m_book, {ACCT_TYPE_STOCK})};
    gnc_register_account_list_option(m_db, "foo", "bar", "baz",
                                     "Phony Option", acclist);
    EXPECT_EQ(4U, m_db->find_option("foo", "bar")->get_value<GncOptionAccountList>().size());
    EXPECT_EQ(acclist[3], m_db->find_option("foo", "bar")->get_value<GncOptionAccountList>().at(3));
}

TEST_F(GncOptionDBAccountTest, test_register_account_list_limited_option)
{
    auto acclist{gnc_account_list_from_types(m_book, {ACCT_TYPE_STOCK})};
    gnc_register_account_list_limited_option(m_db, "foo", "bar", "baz",
                                             "Phony Option", acclist,
                                             {ACCT_TYPE_STOCK});
    EXPECT_EQ(4u, m_db->find_option("foo", "bar")->get_value<GncOptionAccountList>().size());
    EXPECT_EQ(acclist[3], m_db->find_option("foo", "bar")->get_value<GncOptionAccountList>().at(3));
}

TEST_F(GncOptionDBAccountTest, test_register_account_sel_limited_option)
{
    auto acclist{gnc_account_list_from_types(m_book, {ACCT_TYPE_STOCK})};
    GncOptionAccountList accsel{acclist[2]};
    gnc_register_account_list_limited_option(m_db, "foo", "bar", "baz",
                                             "Phony Option", accsel,
                                             {ACCT_TYPE_STOCK});
    EXPECT_EQ(1u, m_db->find_option("foo", "bar")->get_value<GncOptionAccountList>().size());
    EXPECT_EQ(accsel[0], m_db->find_option("foo", "bar")->get_value<GncOptionAccountList>().at(0));
}

TEST_F(GncOptionDBAccountTest, test_register_account_sel_limited_option_fail_construct)
{
    auto acclist{gnc_account_list_from_types(m_book, {ACCT_TYPE_STOCK})};
    GncOptionAccountList accsel{acclist[2]};
    gnc_register_account_list_limited_option(m_db, "foo", "bar", "baz", "Phony Option",
                                     accsel, {ACCT_TYPE_BANK});
    EXPECT_FALSE(m_db->find_option("foo", "bar"));
    gnc_register_account_list_limited_option(m_db, "foo", "bar", "baz",
                                             "Phony Option", acclist,
                                             {ACCT_TYPE_BANK});
    EXPECT_FALSE(m_db->find_option("foo", "bar"));
}

using KT = GncOptionMultichoiceKeyType;
TEST_F(GncOptionDBTest, test_register_multichoice_option)
{
    GncMultichoiceOptionChoices choices{
        { "plugh", "xyzzy", KT::STRING},
        { "waldo", "pepper", KT::STRING},
        { "pork", "sausage", KT::STRING},
        { "corge", "grault", KT::STRING}};
    gnc_register_multichoice_option(m_db, "foo", "bar", "baz",
                                    "Phony Option", "waldo",
                                    std::move(choices));
    EXPECT_STREQ("waldo", m_db->lookup_string_option("foo", "bar").c_str());
    ASSERT_TRUE(m_db->set_option("foo", "bar", std::string{"corge"}));
    EXPECT_STREQ("corge", m_db->lookup_string_option("foo", "bar").c_str());
}

static time64
time64_from_gdate(const GDate* g_date, DayPart when)
{
    GncDate date{g_date_get_year(g_date), g_date_get_month(g_date),
            g_date_get_day(g_date)};
    GncDateTime time1{date, when};
    return static_cast<time64>(time1);
}


TEST_F(GncOptionDBTest, test_register_relative_date_option)
{
    gnc_register_date_option(m_db, "foo", "bar", "baz", "Phony Option",
                             RelativeDatePeriod::START_ACCOUNTING_PERIOD);
    GDate prev_year_start;
    g_date_set_time_t(&prev_year_start, time(nullptr));
    gnc_gdate_set_prev_year_start(&prev_year_start);
    time64 time1{time64_from_gdate(&prev_year_start, DayPart::start)};
    ASSERT_TRUE(m_db->set_option("foo", "bar", time1));
    EXPECT_EQ(time1, m_db->find_option("foo", "bar")->get_value<time64>());
}

TEST_F(GncOptionDBTest, test_register_absolute_date_option)
{
    time64 time1{static_cast<time64>(GncDateTime("2019-07-19 15:32:26 +05:00"))};

    gnc_register_date_option(m_db, "foo", "bar", "baz", "Phony Option",
                             time1);
    GDate prev_year_start;
    g_date_set_time_t(&prev_year_start, time(nullptr));
    gnc_gdate_set_prev_year_start(&prev_year_start);
    ASSERT_TRUE(m_db->set_option("foo", "bar", time1));
    EXPECT_EQ(time1,
              m_db->find_option("foo", "bar")->get_value<time64>());
}

/* Copied from gnc-optiondb.cpp for the purpose of finding the index of the
 * option in the following test.
 */
static const RelativeDatePeriodVec begin_dates
{
    RelativeDatePeriod::TODAY,
    RelativeDatePeriod::START_THIS_MONTH,
    RelativeDatePeriod::START_PREV_MONTH,
    RelativeDatePeriod::START_CURRENT_QUARTER,
    RelativeDatePeriod::START_PREV_QUARTER,
    RelativeDatePeriod::START_CAL_YEAR,
    RelativeDatePeriod::START_PREV_YEAR,
    RelativeDatePeriod::START_ACCOUNTING_PERIOD
};

TEST_F(GncOptionDBTest, test_register_start_date_option)
{
    gnc_register_start_date_option(m_db, "foo", "bar", "baz",
                                   "Phony Option");
    GDate prev_year_start;
    g_date_set_time_t(&prev_year_start, time(nullptr));
    gnc_gdate_set_prev_year_start(&prev_year_start);
    time64 time1{time64_from_gdate(&prev_year_start, DayPart::start)};
    EXPECT_EQ(RelativeDatePeriod::START_ACCOUNTING_PERIOD,
              m_db->find_option("foo", "bar")->get_value<RelativeDatePeriod>());
    ASSERT_TRUE(m_db->set_option("foo", "bar", time1));
    EXPECT_EQ(time1,
              m_db->find_option("foo", "bar")->get_value<time64>());
    EXPECT_EQ(RelativeDatePeriod::ABSOLUTE,
              m_db->find_option("foo", "bar")->get_value<RelativeDatePeriod>());
    m_db->set_option("foo", "bar", RelativeDatePeriod::START_THIS_MONTH);
    EXPECT_EQ(RelativeDatePeriod::START_THIS_MONTH,
              m_db->find_option("foo", "bar")->get_value<RelativeDatePeriod>());
    const RelativeDatePeriod start_this_month {RelativeDatePeriod::START_THIS_MONTH};
    auto index =
        std::find(begin_dates.begin(), begin_dates.end(), start_this_month);
     /* If this fails check that the begin_dates vector above matches the one in
     * gnc-optiondb.cpp.
     */
    EXPECT_EQ(static_cast<unsigned int>(std::distance(begin_dates.begin(), index)),
              m_db->find_option("foo", "bar")->get_value<uint16_t>());
    m_db->set_option("foo", "bar", RelativeDatePeriod::END_THIS_MONTH);
    EXPECT_EQ(RelativeDatePeriod::START_THIS_MONTH,
              m_db->find_option("foo", "bar")->get_value<RelativeDatePeriod>());
    m_db->set_option("foo", "bar", static_cast<uint16_t>(5));
    EXPECT_EQ(5u, m_db->find_option("foo", "bar")->get_value<uint16_t>());

}

static bool
operator==(const GncGUID& l, const GncGUID& r)
{
    return guid_equal(&l, &r);
}

class GncOptionDBIOTest : public ::testing::Test
{
protected:
    GncOptionDBIOTest() :
        m_book{qof_session_get_book(gnc_get_current_session())},
        m_root{gnc_account_create_root(m_book)},
        m_db{std::make_unique<GncOptionDB>()}
    {
        auto create_account = [this](Account* parent, GNCAccountType type,
                                       const char* name)->Account* {
            auto account = xaccMallocAccount(this->m_book);
            xaccAccountBeginEdit(account);
            xaccAccountSetType(account, type);
            xaccAccountSetName(account, name);
            xaccAccountBeginEdit(parent);
            gnc_account_append_child(parent, account);
            xaccAccountCommitEdit(parent);
            xaccAccountCommitEdit(account);
            return account;
        };
        auto assets = create_account(m_root, ACCT_TYPE_ASSET, "Assets");
        auto liabilities = create_account(m_root, ACCT_TYPE_LIABILITY, "Liabilities");
        auto expenses = create_account(m_root, ACCT_TYPE_EXPENSE, "Expenses");
        create_account(assets, ACCT_TYPE_BANK, "Bank");
        auto broker = create_account(assets, ACCT_TYPE_ASSET, "Broker");
        auto stocks = create_account(broker, ACCT_TYPE_STOCK, "Stocks");
        auto aapl = create_account(stocks, ACCT_TYPE_STOCK, "AAPL");
        create_account(stocks, ACCT_TYPE_STOCK, "MSFT");
        auto hpe = create_account(stocks, ACCT_TYPE_STOCK, "HPE");
        create_account(broker, ACCT_TYPE_BANK, "Cash Management");
        create_account(expenses, ACCT_TYPE_EXPENSE, "Food");
        create_account(expenses, ACCT_TYPE_EXPENSE, "Gas");
        create_account(expenses, ACCT_TYPE_EXPENSE, "Rent");

        gnc_register_string_option(m_db, "foo", "bar", "baz",
                                   "Phony Option", std::string{"waldo"});
        gnc_register_text_option(m_db, "foo", "sausage", "links",
                                 "Phony Option", std::string{"waldo"});
        gnc_register_string_option(m_db, "qux", "grault", "baz",
                                   "Phony Option", std::string{""});
        gnc_register_text_option(m_db, "qux", "garply", "fred",
                                   "Phony Option", std::string{"waldo"});
        gnc_register_date_option(m_db, "pork", "garply", "first",
                                 "Phony Date Option",
                                 RelativeDatePeriod::START_CURRENT_QUARTER);
        gnc_register_account_list_option(m_db, "quux", "xyzzy", "second",
                                         "Phony AccountList Option",
                                         {*qof_entity_get_guid(aapl),
                                          *qof_entity_get_guid(hpe)});
    }

    ~GncOptionDBIOTest()
    {
        xaccAccountBeginEdit(m_root);
        xaccAccountDestroy(m_root); //It does the commit
        gnc_clear_current_session();
    }

    QofBook* m_book;
    Account* m_root;
    GncOptionDBPtr m_db;
};

TEST_F(GncOptionDBIOTest, test_option_key_value_output)
{
    std::ostringstream oss;
    m_db->save_option_key_value(oss, "foo", "sausage");
    EXPECT_STREQ("", oss.str().c_str());
    m_db->set_option("foo", "sausage", std::string{"pepper"});
//    EXPECT_STREQ("pepper", m_db->lookup_string_option("foo", "sausage").c_str());
//    EXPECT_TRUE(m_db->find_option("foo", "sausage")->get().is_changed());
    m_db->save_option_key_value(oss, "foo", "sausage");
    EXPECT_STREQ("foo:sausage=pepper;", oss.str().c_str());
}

TEST_F(GncOptionDBIOTest, test_option_key_value_input)
{
    std::istringstream iss{"foo:sausage=pepper;"};
    EXPECT_STREQ("waldo", m_db->lookup_string_option("foo", "sausage").c_str());
    m_db->load_option_key_value(iss);
    EXPECT_STREQ("pepper", m_db->lookup_string_option("foo", "sausage").c_str());
}

TEST_F(GncOptionDBIOTest, test_option_kvp_save)
{
    m_db->save_to_kvp(m_book, false);
    auto foo = "foo";
    auto bar = "bar";
    auto sausage = "sausage";
    auto grault = "grault";
    auto garply = "garply";
    GSList foo_bar_tail{(void*)bar, nullptr};
    GSList foo_bar_head{(void*)foo, &foo_bar_tail};
    GSList foo_sausage_tail{(void*)sausage, nullptr};
    GSList foo_sausage_head{(void*)foo, &foo_sausage_tail};
    GSList qux_grault_tail{(void*)grault, nullptr};
    GSList qux_grault_head{(void*)foo, &qux_grault_tail};
    GSList qux_garply_tail{(void*)garply, nullptr};
    GSList qux_garply_head{(void*)foo, &qux_grault_tail};
    m_db->set_option("foo", "sausage", std::string{"pepper"});
    m_db->save_to_kvp(m_book, true);
    auto foo_bar = qof_book_get_option(m_book, &foo_bar_head);
    auto foo_sausage = qof_book_get_option(m_book, &foo_sausage_head);
    auto qux_garply = qof_book_get_option(m_book, &qux_garply_head);
    auto qux_grault = qof_book_get_option(m_book, &qux_grault_head);
    EXPECT_EQ(nullptr, foo_bar);
    EXPECT_EQ(nullptr, qux_garply);
    EXPECT_EQ(nullptr, qux_grault);
    EXPECT_STREQ("pepper", foo_sausage->get<const char*>());
}
