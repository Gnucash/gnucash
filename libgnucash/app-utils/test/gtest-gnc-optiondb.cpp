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
#include <gnc-optiondb.hpp>

class GncOptionDBTest : public ::testing::Test
{
protected:
    GncOptionDBTest() : m_db{gnc_option_db_new()} {}

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
    EXPECT_EQ(m_db->num_sections(), 1);
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

/* Note: The following test-fixture code is also present in slightly different
 * form in gtest-gnc-option.cpp.
 */


struct AccountTestBook
{
    AccountTestBook() :
        m_book{qof_book_new()}, m_root{gnc_account_create_root(m_book)}
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
    ~AccountTestBook()
    {
        xaccAccountBeginEdit(m_root);
        xaccAccountDestroy(m_root); //It does the commit
        qof_book_destroy(m_book);
    }

    QofBook* m_book;
    Account* m_root;
};

TEST_F(GncOptionDBTest, test_register_account_list_option)
{
    AccountTestBook book;
    auto acclist{gnc_account_list_from_types(book.m_book, {ACCT_TYPE_STOCK})};
    gnc_register_account_list_option(m_db, "foo", "bar", "baz", "Phony Option",
                                    acclist);
    EXPECT_EQ(4, m_db->find_option("foo", "bar")->get().get_value<GncOptionAccountList>().size());
    EXPECT_EQ(acclist[3], m_db->find_option("foo", "bar")->get().get_value<GncOptionAccountList>().at(3));
}

TEST_F(GncOptionDBTest, test_register_account_list_limited_option)
{
    AccountTestBook book;
    auto acclist{gnc_account_list_from_types(book.m_book, {ACCT_TYPE_STOCK})};
    gnc_register_account_list_limited_option(m_db, "foo", "bar", "baz",
                                             "Phony Option", acclist,
                                             {ACCT_TYPE_STOCK});
    EXPECT_EQ(4, m_db->find_option("foo", "bar")->get().get_value<GncOptionAccountList>().size());
    EXPECT_EQ(acclist[3], m_db->find_option("foo", "bar")->get().get_value<GncOptionAccountList>().at(3));
}

TEST_F(GncOptionDBTest, test_register_account_sel_limited_option)
{
    AccountTestBook book;
    auto acclist{gnc_account_list_from_types(book.m_book, {ACCT_TYPE_STOCK})};
    GncOptionAccountList accsel{acclist[2]};
    gnc_register_account_list_limited_option(m_db, "foo", "bar", "baz",
                                             "Phony Option", accsel,
                                             {ACCT_TYPE_STOCK});
    EXPECT_EQ(1, m_db->find_option("foo", "bar")->get().get_value<GncOptionAccountList>().size());
    EXPECT_EQ(accsel[0], m_db->find_option("foo", "bar")->get().get_value<GncOptionAccountList>().at(0));
}

TEST_F(GncOptionDBTest, test_register_account_sel_limited_option_fail_construct)
{
    AccountTestBook book;
    auto acclist{gnc_account_list_from_types(book.m_book, {ACCT_TYPE_STOCK})};
    GncOptionAccountList accsel{acclist[2]};
    gnc_register_account_list_limited_option(m_db, "foo", "bar", "baz", "Phony Option",
                                     accsel, {ACCT_TYPE_BANK});
    EXPECT_FALSE(m_db->find_option("foo", "bar"));
    gnc_register_account_list_limited_option(m_db, "foo", "bar", "baz",
                                             "Phony Option", acclist,
                                             {ACCT_TYPE_BANK});
    EXPECT_FALSE(m_db->find_option("foo", "bar"));
}

TEST_F(GncOptionDBTest, test_register_multichoice_option)
{
    GncMultiChoiceOptionChoices choices{
        { "plugh", "xyzzy", "thud"},
        { "waldo", "pepper", "salt"},
        { "pork", "sausage", "links"},
        { "corge", "grault", "garply"}};
    gnc_register_multichoice_option(m_db, "foo", "bar", "baz", "Phony Option",
                                    std::move(choices));
    ASSERT_TRUE(m_db->set_option("foo", "bar", std::string{"corge"}));
    EXPECT_STREQ("corge", m_db->lookup_string_option("foo", "bar").c_str());
}

static time64
time64_from_gdate(const GDate* g_date, DayPart when)
{
    GncDate date{g_date_get_year(g_date), g_date_get_month(g_date),
            g_date_get_day(g_date)};
    GncDateTime time{date, when};
    return static_cast<time64>(time);
}


TEST_F(GncOptionDBTest, test_register_date_interval_option)
{
    gnc_register_date_interval_option(m_db, "foo", "bar", "baz", "Phony Option",
                                      RelativeDatePeriod::START_ACCOUNTING_PERIOD);
    GDate prev_year_start;
    g_date_set_time_t(&prev_year_start, time(nullptr));
    gnc_gdate_set_prev_year_start(&prev_year_start);
    time64 time{time64_from_gdate(&prev_year_start, DayPart::start)};
    ASSERT_TRUE(m_db->set_option("foo", "bar", time));
    EXPECT_EQ(time, m_db->find_option("foo", "bar")->get().get_value<time64>());
}

class GncUIType
{
public:
    void set_value(const std::string& value) { m_value = value; }
    const std::string& get_value() const { return m_value; }
private:
    std::string m_value;
};

class GncOptionUIItem
{
public:
    GncOptionUIItem(GncUIType* widget) : m_widget{widget} {}
    GncUIType* m_widget;
};

class GncOptionUITest : public ::testing::Test
{
protected:
    GncOptionUITest() :
        m_option{"foo", "bar", "baz", "Phony Option", std::string{"waldo"},
            GncOptionUIType::STRING} {}

    GncOption m_option;
};

class GncOptionDBUITest : public ::testing::Test
{
protected:
    GncOptionDBUITest() : m_db{gnc_option_db_new()}
    {
        gnc_register_string_option(m_db, "foo", "bar", "baz", "Phony Option",
                                   std::string{"waldo"});
        gnc_register_text_option(m_db, "foo", "sausage", "links",
                                 "Phony Option", std::string{"waldo"});
        gnc_register_string_option(m_db, "qux", "grault", "baz", "Phony Option",
                                   std::string{""});
        gnc_register_text_option(m_db, "qux", "garply", "fred",
                                   "Phony Option", std::string{"waldo"});
    }

    GncOptionDBPtr m_db;
};

TEST_F(GncOptionDBUITest, test_set_ui_item)
{
    GncUIType entry;
    GncOptionUIItem ui_item(&entry);
    m_db->set_ui_item("foo", "bar", &ui_item);
    EXPECT_EQ(&entry, m_db->get_ui_item("foo", "bar")->m_widget);
}

TEST_F(GncOptionDBUITest, test_ui_value_from_option)
{
    GncUIType entry;
    GncOptionUIItem ui_item(&entry);
    const char* value{"waldo"};
    m_db->set_ui_item("foo", "bar", &ui_item);
    m_db->set_ui_from_option("foo", "bar", [](GncOption& option){
            auto new_ui_item = option.get_ui_item();
            new_ui_item->m_widget->set_value(option.get_value<std::string>());
        });
    EXPECT_STREQ(value, entry.get_value().c_str());
}

TEST_F(GncOptionDBUITest, test_option_value_from_ui)
{
    GncUIType entry;
    GncOptionUIItem ui_item(&entry);
    const char* value{"pepper"};
    m_db->set_ui_item("foo", "bar", &ui_item);
    entry.set_value(value);
    m_db->set_option_from_ui("foo", "bar", [](GncOption& option){
            auto new_ui_item = option.get_ui_item()->m_widget;
            option.set_value(new_ui_item->get_value());
        });
    EXPECT_STREQ(value, m_db->lookup_string_option("foo", "bar").c_str());
}

class GncOptionDBIOTest : public ::testing::Test
{
protected:
    GncOptionDBIOTest() : m_db{gnc_option_db_new()}
    {
        gnc_register_string_option(m_db, "foo", "bar", "baz", "Phony Option",
                                   std::string{"waldo"});
        gnc_register_text_option(m_db, "foo", "sausage", "links",
                                 "Phony Option", std::string{"waldo"});
        gnc_register_string_option(m_db, "qux", "grault", "baz", "Phony Option",
                                   std::string{""});
        gnc_register_text_option(m_db, "qux", "garply", "fred",
                                   "Phony Option", std::string{"waldo"});
    }

    GncOptionDBPtr m_db;
};

TEST_F(GncOptionDBIOTest, test_option_scheme_output)
{
    std::ostringstream oss;
    m_db->save_option_scheme(oss, "option", "foo", "sausage");
    EXPECT_STREQ("", oss.str().c_str());
    oss.clear();
    m_db->set_option("foo", "sausage", std::string{"pepper"});
    EXPECT_STREQ("pepper", m_db->lookup_string_option("foo", "sausage").c_str());
    EXPECT_TRUE(m_db->find_option("foo", "sausage")->get().is_changed());
    oss.flush();
    m_db->save_option_scheme(oss, "option", "foo", "sausage");
    EXPECT_STREQ("(let ((option (gnc:lookup-option option\n"
                 "                                 \"foo\"\n"
                 "                                 \"sausage\")))\n"
                 "   ((lambda (o) (if o (gnc:option-set-value o \"pepper\""
                 "))) option))\n\n", oss.str().c_str());
}

TEST_F(GncOptionDBIOTest, test_option_scheme_input)
{
    const char* input{"(let ((option (gnc:lookup-option option\n"
                 "                                 \"foo\"\n"
                 "                                 \"sausage\")))\n"
                 "   ((lambda (o) (if o (gnc:option-set-value o \"pepper\""
            "))) option))\n\n"};
    std::istringstream iss{input};
    EXPECT_STREQ("waldo", m_db->lookup_string_option("foo", "sausage").c_str());
    m_db->load_option_scheme(iss);
    EXPECT_STREQ("pepper", m_db->lookup_string_option("foo", "sausage").c_str());
}

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
