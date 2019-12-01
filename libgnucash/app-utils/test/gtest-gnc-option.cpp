/********************************************************************
 * gtest-gnc-option.cpp -- unit tests for GncOption class.          *
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
#include <gnc-option.hpp>
#include <guid.hpp>
extern "C"
{
#include <gnc-date.h>
#include <time.h>
#include <gnc-ui-util.h>
#include <gnc-session.h>
}

TEST(GncOption, test_string_ctor)
{
    EXPECT_NO_THROW({
            GncOption option("foo", "bar", "baz", "Phony Option",
                             std::string{"waldo"});
        });
}

TEST(GncOption, test_string_classifier_getters)
{
    GncOption option("foo", "bar", "baz", "Phony Option", std::string{"waldo"});
    EXPECT_STREQ("foo", option.get_section().c_str());
    EXPECT_STREQ("bar", option.get_name().c_str());
    EXPECT_STREQ("baz", option.get_key().c_str());
    EXPECT_STREQ("Phony Option", option.get_docstring().c_str());
}

TEST(GncOption, test_string_default_value)
{
    GncOption option("foo", "bar", "baz", "Phony Option", std::string{"waldo"});
    EXPECT_STREQ("waldo", option.get_default_value<std::string>().c_str());
    EXPECT_STREQ("waldo", option.get_value<std::string>().c_str());
    EXPECT_EQ(0, option.get_value<int64_t>());
}

TEST(GncOption, test_string_value)
{
    GncOption option("foo", "bar", "baz", "Phony Option", std::string{"waldo"});
    option.set_value(std::string{"pepper"});
    EXPECT_STREQ("waldo", option.get_default_value<std::string>().c_str());
    EXPECT_NO_THROW({
            EXPECT_STREQ("pepper", option.get_value<std::string>().c_str());
        });
}

TEST(GncOption, test_string_stream_out)
{
    GncOption option("foo", "bar", "baz", "Phony Option", std::string{"waldo"});
    std::ostringstream oss;
    oss << option;
    EXPECT_EQ(oss.str(), option.get_value<std::string>());
}

TEST(GncOption, test_string_stream_in)
{
    GncOption option("foo", "bar", "baz", "Phony Option", std::string{"waldo"});
    std::string pepper{"pepper"};
    std::istringstream iss{pepper};
    iss >> option;
    EXPECT_EQ(pepper, option.get_value<std::string>());
}

TEST(GncOption, test_int64_t_value)
{
    GncOption option("foo", "bar", "baz", "Phony Option", INT64_C(123456789));
    option.set_value(INT64_C(987654321));
    EXPECT_TRUE(option.get_default_value<std::string>().empty());
    EXPECT_EQ(INT64_C(987654321), option.get_value<int64_t>());
}

TEST(GncOption, test_int64_stream_out)
{
    GncOption option("foo", "bar", "baz", "Phony Option",  INT64_C(123456789));
    std::ostringstream oss;
    oss << option;
    EXPECT_STREQ(oss.str().c_str(), "123456789");
}

TEST(GncOption, test_int64_stream_in)
{
    GncOption option("foo", "bar", "baz", "Phony Option",  INT64_C(123456789));
    std::string number{"987654321"};
    std::istringstream iss{number};
    iss >> option;
    EXPECT_EQ(INT64_C(987654321), option.get_value<int64_t>());
}

TEST(GncOption, test_bool_stream_out)
{
    GncOption option("foo", "bar", "baz", "Phony Option", false);
    std::ostringstream oss;
    oss << option;
    EXPECT_STREQ(oss.str().c_str(), "#f");
    oss.str("");
    option.set_value(true);
    oss << option;
    EXPECT_STREQ(oss.str().c_str(), "#t");
}

TEST(GncOption, test_bool_stream_in)
{
    GncOption option("foo", "bar", "baz", "Phony Option", false);
    std::istringstream iss("#t");
    iss >> option;
    EXPECT_TRUE(option.get_value<bool>());
    iss.str("#f");
    iss >> option;
    EXPECT_FALSE(option.get_value<bool>());
}

class GncOptionTest : public ::testing::Test
{
protected:
    GncOptionTest() : m_session{gnc_get_current_session()}, m_book{gnc_get_current_book()} {}
    ~GncOptionTest() { gnc_clear_current_session(); }

    QofSession* m_session;
    QofBook* m_book;
};

TEST_F(GncOptionTest, test_budget_ctor)
{
    auto budget = gnc_budget_new(m_book);
    EXPECT_NO_THROW({
            GncOption option("foo", "bar", "baz", "Phony Option",
                             QOF_INSTANCE(budget));
        });
    gnc_budget_destroy(budget);
}

TEST_F(GncOptionTest, test_commodity_ctor)
{
    auto hpe = gnc_commodity_new(m_book, "Hewlett Packard Enterprise, Inc.",
                                    "NYSE", "HPE", NULL, 1);
    EXPECT_NO_THROW({
            GncOption option("foo", "bar", "baz", "Phony Option",
                             QOF_INSTANCE(hpe));
        });
    gnc_commodity_destroy(hpe);
}

class GncOptionCommodityTest : public ::testing::Test
{
protected:
    GncOptionCommodityTest() : m_session{gnc_get_current_session()},
                               m_book{gnc_get_current_book()},
                               m_table{gnc_commodity_table_new()}
    {
/* We can't initialize the commodities with their values because we first must
 * set the book's commodity table.
 */
        qof_book_set_data(m_book, GNC_COMMODITY_TABLE, m_table);
        m_eur = gnc_commodity_new(m_book, "Euro", "ISO4217", "EUR", NULL, 100);
        gnc_commodity_table_insert(m_table, m_eur);
        m_usd = gnc_commodity_new(m_book, "United States Dollar", "CURRENCY",
                                  "USD", NULL, 100);
        gnc_commodity_table_insert(m_table, m_usd);
        m_aapl = gnc_commodity_new(m_book, "Apple", "NASDAQ", "AAPL", NULL, 1);
        gnc_commodity_table_insert(m_table, m_aapl);
        m_hpe = gnc_commodity_new(m_book, "Hewlett Packard", "NYSE", "HPE",
                                  NULL, 1);
        gnc_commodity_table_insert(m_table, m_hpe);
    }
    ~GncOptionCommodityTest()
    {
        gnc_commodity_destroy(m_hpe);
        gnc_commodity_destroy(m_aapl);
        gnc_commodity_destroy(m_usd);
        gnc_commodity_destroy(m_eur);
        qof_book_set_data(m_book, GNC_COMMODITY_TABLE, nullptr);
        gnc_commodity_table_destroy(m_table);
        gnc_clear_current_session();
    }

    QofSession* m_session;
    QofBook* m_book;
    gnc_commodity_table * m_table;
    gnc_commodity *m_eur;
    gnc_commodity *m_usd;
    gnc_commodity *m_aapl;
    gnc_commodity *m_hpe;
};

static GncOption
make_currency_option (const char* section, const char* name,
                      const char* key, const char* doc_string,
                      gnc_commodity *value, bool is_currency=false)
{
    GncOption option{GncOptionValidatedValue<QofInstance*>{
        section, name, key, doc_string, QOF_INSTANCE(value),
        [](QofInstance* new_value) -> bool
            {
                return GNC_IS_COMMODITY (new_value) &&
                    gnc_commodity_is_currency(GNC_COMMODITY(new_value));
            }, is_currency ? GncOptionUIType::CURRENCY : GncOptionUIType::COMMODITY}
    };
    return option;
}

TEST_F(GncOptionCommodityTest, test_currency_ctor)
{
    EXPECT_THROW({
            auto option = make_currency_option("foo", "bar", "baz",
                                               "Phony Option", m_hpe, false);
        }, std::invalid_argument);
    EXPECT_NO_THROW({
            auto option = make_currency_option("foo", "bar", "baz",
                                               "Phony Option", m_eur, true);
        });
    EXPECT_NO_THROW({
            auto option = make_currency_option("foo", "bar", "baz",
                                               "Phony Option", m_usd, true);
        });
}

TEST_F(GncOptionCommodityTest, test_currency_setter)
{
    auto option = make_currency_option("foo", "bar", "baz", "Phony Option", m_eur, true);
    EXPECT_NO_THROW({
            option.set_value(QOF_INSTANCE(m_usd));
        });
    EXPECT_PRED2(gnc_commodity_equal, m_usd,
                 GNC_COMMODITY(option.get_value<QofInstance*>()));
    EXPECT_THROW({
            option.set_value(QOF_INSTANCE(m_hpe));
        }, std::invalid_argument);
    EXPECT_PRED2(gnc_commodity_equal, m_usd,
                 GNC_COMMODITY(option.get_value<QofInstance*>()));
}

TEST_F(GncOptionCommodityTest, test_currency_validator)
{
    auto option = make_currency_option("foo", "bar", "baz", "Phony Option",
                                       m_eur, true);
    EXPECT_TRUE(option.validate(QOF_INSTANCE(m_usd)));
    EXPECT_FALSE(option.validate(QOF_INSTANCE(m_aapl)));
}

}

TEST_F(GncOptionTest, test_qofinstance_out)
{
    auto table = gnc_commodity_table_new();
    qof_book_set_data(m_book, GNC_COMMODITY_TABLE, table);
    auto eur = gnc_commodity_new(m_book, "Euro", "ISO4217", "EUR", NULL, 100);
    auto option = make_currency_option("foo", "bar", "baz", "Phony Option", eur, true);

    std::string eur_guid{gnc::GUID{*qof_instance_get_guid(eur)}.to_string()};
    std::ostringstream oss;
    oss << option;
    EXPECT_EQ(eur_guid, oss.str());
    gnc_commodity_destroy(eur);
    qof_book_set_data(m_book, GNC_COMMODITY_TABLE, nullptr);
    gnc_commodity_table_destroy(table);
}

TEST_F(GncOptionTest, test_qofinstance_in)
{
    auto table = gnc_commodity_table_new();
    qof_book_set_data(m_book, GNC_COMMODITY_TABLE, table);
    auto eur = gnc_commodity_new(m_book, "Euro", "ISO4217", "EUR", NULL, 100);
    auto usd = gnc_commodity_new(m_book, "United States Dollar",
                                 "CURRENCY", "USD", NULL, 100);
    auto hpe = gnc_commodity_new(m_book, "Hewlett Packard Enterprise, Inc.",
                                    "NYSE", "HPE", NULL, 1);
    auto option = make_currency_option("foo", "bar", "baz", "Phony Option", eur, true);

    EXPECT_THROW({
            std::string hpe_guid{gnc::GUID{*qof_instance_get_guid(hpe)}.to_string()};
            std::istringstream iss{hpe_guid};
            iss >> option;
        }, std::invalid_argument);
    EXPECT_NO_THROW({
            std::string usd_guid{gnc::GUID{*qof_instance_get_guid(usd)}.to_string()};
            std::istringstream iss{usd_guid};
            iss >> option;
            EXPECT_EQ(QOF_INSTANCE(usd), option.get_value<QofInstance*>());
        });
    gnc_commodity_destroy(eur);
    gnc_commodity_destroy(usd);
    qof_book_set_data(m_book, GNC_COMMODITY_TABLE, nullptr);
    gnc_commodity_table_destroy(table);
}

class GncUIItem
{
public:
    void set_value(const std::string& value) { m_value = value; }
    const std::string& get_value() { return m_value; }
private:
    std::string m_value;
};

class GncOptionUIItem
{
public:
    GncOptionUIItem(GncUIItem* widget) : m_widget{widget} {}
    GncUIItem* m_widget;
};

class GncOptionUITest : public ::testing::Test
{
protected:
    GncOptionUITest() :
        m_option{"foo", "bar", "baz", "Phony Option", std::string{"waldo"},
            GncOptionUIType::STRING} {}

    GncOption m_option;
};

using GncOptionUI = GncOptionUITest;

TEST_F(GncOptionUI, test_option_ui_type)
{
    EXPECT_EQ(GncOptionUIType::STRING, m_option.get_ui_type());
}

TEST_F(GncOptionUI, test_set_option_ui_item)
{
    GncUIItem ui_item;
    GncOptionUIItem option_ui_item{&ui_item};
    m_option.set_ui_item(&option_ui_item);
    EXPECT_EQ(&ui_item, m_option.get_ui_item()->m_widget);
}

class GncOptionRangeTest : public ::testing::Test
{
protected:
    GncOptionRangeTest() :
        m_intoption{GncOptionRangeValue<int>{"foo", "bar", "baz",
                                             "Phony Option", 15, 1, 30, 1}},
        m_doubleoption{GncOptionRangeValue<double>{"waldo", "pepper", "salt",
                                                   "Phonier Option", 1.5, 1.0,
                                                   3.0, 0.1}} {}

    GncOption m_intoption;
    GncOption m_doubleoption;
};

using GncRangeOption = GncOptionRangeTest;

TEST_F(GncRangeOption, test_initialization)
{
    EXPECT_EQ(15, m_intoption.get_value<int>());
    EXPECT_EQ(1.5, m_doubleoption.get_value<double>());
    EXPECT_EQ(15, m_intoption.get_default_value<int>());
    EXPECT_EQ(1.5, m_doubleoption.get_default_value<double>());
}

TEST_F(GncRangeOption, test_setter)
{
    EXPECT_THROW({ m_intoption.set_value(45); }, std::invalid_argument);
    EXPECT_NO_THROW({ m_intoption.set_value(20); });
    EXPECT_EQ(20, m_intoption.get_value<int>());
    EXPECT_EQ(15, m_intoption.get_default_value<int>());
    EXPECT_THROW({ m_doubleoption.set_value(4.5); }, std::invalid_argument);
    EXPECT_NO_THROW({ m_doubleoption.set_value(2.0); });
    EXPECT_EQ(2.0, m_doubleoption.get_value<double>());
    EXPECT_EQ(1.5, m_doubleoption.get_default_value<double>());
}

TEST_F(GncRangeOption, test_range_out)
{
    std::ostringstream oss;
    oss << "Integer " << m_intoption << " Double " << m_doubleoption << ".";
    EXPECT_STREQ("Integer 15 Double 1.5.", oss.str().c_str());
}

TEST_F(GncRangeOption, test_range_in)
{
    std::istringstream iss{std::string{"45 4.5 20 2.0"}};
    EXPECT_THROW({ iss >> m_intoption; }, std::invalid_argument);
    EXPECT_THROW({ iss >> m_doubleoption; }, std::invalid_argument);
    EXPECT_NO_THROW({ iss >> m_intoption; });
    EXPECT_NO_THROW({ iss >> m_doubleoption; });
    EXPECT_EQ(20, m_intoption.get_value());
    EXPECT_EQ(2.0, m_doubleoption.get_value());
}

using AccountPair = std::pair<GncOptionAccountList&,
                              const GncOptionAccountTypeList&>;
static void
find_children(Account* account, void* data)
{
    auto datapair =
        (AccountPair*)data;
    GncOptionAccountList& list = datapair->first;
    const GncOptionAccountTypeList& types = datapair->second;
    if (std::find(types.begin(), types.end(),
                  xaccAccountGetType(account)) != types.end())
        list.push_back(account);
}

class GncOptionAccountTest : public ::testing::Test
{
protected:
    GncOptionAccountTest() :
        m_session{gnc_get_current_session()}, m_book{gnc_get_current_book()},
        m_root{gnc_account_create_root(m_book)}
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
    ~GncOptionAccountTest()
    {
        xaccAccountBeginEdit(m_root);
        xaccAccountDestroy(m_root); //It does the commit
        gnc_clear_current_session();
    }
    GncOptionAccountList list_of_types(const GncOptionAccountTypeList& types)
    {
        GncOptionAccountList list;
        AccountPair funcdata{list, types};
        gnc_account_foreach_descendant(m_root, (AccountCb)find_children,
                                       &funcdata);
        return list;
    }

    QofSession* m_session;
    QofBook* m_book;
    Account* m_root;
};

TEST_F(GncOptionAccountTest, test_test_constructor_and_destructor)
{
    EXPECT_TRUE(m_book != NULL);
    EXPECT_TRUE(QOF_IS_BOOK(m_book));
    EXPECT_TRUE(m_root != NULL);
    EXPECT_TRUE(GNC_IS_ACCOUNT(m_root));
    GncOptionAccountList list{list_of_types({ACCT_TYPE_BANK})};
    EXPECT_EQ(2, list.size());
    list = list_of_types({ACCT_TYPE_ASSET, ACCT_TYPE_STOCK});
    EXPECT_EQ(6, list.size());
}

TEST_F(GncOptionAccountTest, test_option_no_value_constructor)
{
    GncOptionAccountValue option{"foo", "bar", "baz", "Bogus Option",
            GncOptionUIType::ACCOUNT_LIST};
    EXPECT_TRUE(option.get_value().empty());
    EXPECT_TRUE(option.get_default_value().empty());
}

TEST_F(GncOptionAccountTest, test_option_value_constructor)
{
    GncOptionAccountList acclist{list_of_types({ACCT_TYPE_BANK})};
    GncOptionAccountValue option{"foo", "bar", "baz", "Bogus Option",
            GncOptionUIType::ACCOUNT_LIST, acclist};
    EXPECT_EQ(2, option.get_value().size());
    EXPECT_EQ(2, option.get_default_value().size());
    EXPECT_EQ(acclist[0], option.get_value()[0]);
}

TEST_F(GncOptionAccountTest, test_option_no_value_limited_constructor)
{
    GncOptionAccountList acclistgood{list_of_types({ACCT_TYPE_BANK})};
    GncOptionAccountList acclistbad{list_of_types({ACCT_TYPE_STOCK})};
    GncOptionAccountValue option{"foo", "bar", "baz", "Bogus Option",
            GncOptionUIType::ACCOUNT_LIST, {ACCT_TYPE_BANK}};
    EXPECT_TRUE(option.get_value().empty());
    EXPECT_TRUE(option.get_default_value().empty());
    EXPECT_EQ(true, option.validate(acclistgood));
    EXPECT_EQ(false, option.validate(acclistbad));
}

TEST_F(GncOptionAccountTest, test_option_value_limited_constructor)
{
    GncOptionAccountList acclistgood{list_of_types({ACCT_TYPE_BANK})};
    GncOptionAccountList acclistbad{list_of_types({ACCT_TYPE_STOCK})};
    EXPECT_THROW({
            GncOptionAccountValue option("foo", "bar", "baz", "Bogus Option",
                                         GncOptionUIType::ACCOUNT_LIST,
                                         acclistbad, {ACCT_TYPE_BANK});
        }, std::invalid_argument);

    EXPECT_THROW({
            GncOptionAccountValue option("foo", "bar", "baz", "Bogus Option",
                                         GncOptionUIType::ACCOUNT_SEL,
                                         acclistgood, {ACCT_TYPE_BANK});
        }, std::invalid_argument);

    EXPECT_NO_THROW({
            GncOptionAccountValue option("foo", "bar", "baz", "Bogus Option",
                                         GncOptionUIType::ACCOUNT_LIST,
                                         acclistgood, {ACCT_TYPE_BANK});
        });

    EXPECT_NO_THROW({
            GncOptionAccountList accsel{acclistgood[0]};
            GncOptionAccountValue option("foo", "bar", "baz", "Bogus Option",
                                         GncOptionUIType::ACCOUNT_LIST,
                                         accsel, {ACCT_TYPE_BANK});
        });
    GncOptionAccountValue option {"foo", "bar", "baz", "Bogus Option",
            GncOptionUIType::ACCOUNT_LIST, acclistgood, {ACCT_TYPE_BANK}};
    EXPECT_FALSE(option.get_value().empty());
    EXPECT_FALSE(option.get_default_value().empty());
    EXPECT_EQ(true, option.validate(acclistgood));
    EXPECT_EQ(false, option.validate(acclistbad));
}

TEST_F(GncOptionAccountTest, test_account_list_out)
{
    GncOptionAccountList acclist{list_of_types({ACCT_TYPE_BANK})};
    GncOptionAccountValue option{"foo", "bar", "baz", "Bogus Option",
            GncOptionUIType::ACCOUNT_LIST, acclist};
    std::ostringstream oss;
    std::string acc_guids{gnc::GUID{*qof_instance_get_guid(acclist[0])}.to_string()};
    acc_guids += " ";
    acc_guids += gnc::GUID{*qof_instance_get_guid(acclist[1])}.to_string();
    acc_guids += " ";

    oss << option;
    EXPECT_EQ(acc_guids, oss.str());

    GncOptionAccountList accsel{acclist[0]};
    GncOptionAccountValue sel_option("foo", "bar", "baz", "Bogus Option",
                                 GncOptionUIType::ACCOUNT_LIST,
                                 accsel, {ACCT_TYPE_BANK});
    acc_guids = gnc::GUID{*qof_instance_get_guid(accsel[0])}.to_string();
    acc_guids += " ";

    oss.str("");
    oss << sel_option;
    EXPECT_EQ(acc_guids, oss.str());
}

TEST_F(GncOptionAccountTest, test_account_list_in)
{
    GncOptionAccountList acclist{list_of_types({ACCT_TYPE_BANK})};
    GncOptionAccountValue option{"foo", "bar", "baz", "Bogus Option",
            GncOptionUIType::ACCOUNT_LIST, acclist};
    std::string acc_guids{gnc::GUID{*qof_instance_get_guid(acclist[0])}.to_string()};
    acc_guids += " ";
    acc_guids += gnc::GUID{*qof_instance_get_guid(acclist[1])}.to_string();
    acc_guids += " ";

    std::istringstream iss{acc_guids};
    iss >> option;
    EXPECT_EQ(acclist, option.get_value());

    GncOptionAccountList accsel{acclist[0]};
    GncOptionAccountValue sel_option("foo", "bar", "baz", "Bogus Option",
                                 GncOptionUIType::ACCOUNT_LIST,
                                 accsel, {ACCT_TYPE_BANK});
    GncOptionAccountList acclistbad{list_of_types({ACCT_TYPE_STOCK})};
    acc_guids = gnc::GUID{*qof_instance_get_guid(acclistbad[1])}.to_string();
    acc_guids += " ";

    iss.str(acc_guids);
    iss >> sel_option;
    EXPECT_EQ(accsel, sel_option.get_value());

    acc_guids = gnc::GUID{*qof_instance_get_guid(acclist[1])}.to_string();
    EXPECT_NO_THROW({
            iss.str(acc_guids);
            iss >> sel_option;
        });
    EXPECT_EQ(acclist[1], sel_option.get_value()[0]);
}

class GncOptionMultichoiceTest : public ::testing::Test
{
protected:
    GncOptionMultichoiceTest() :
        m_option{GncOptionMultichoiceValue{"foo", "bar", "baz",
                                           "Phony Option", "plugh",
            {
                {"plugh", "xyzzy", "thud"},
                {"waldo", "pepper", "salt"},
                {"pork", "sausage", "links"},
                {"corge", "grault", "garply"}
                                           }}} {}
    GncOption m_option;
};

using GncMultichoiceOption = GncOptionMultichoiceTest;

TEST_F(GncMultichoiceOption, test_option_ui_type)
{
    EXPECT_EQ(GncOptionUIType::MULTICHOICE, m_option.get_ui_type());
}

TEST_F(GncMultichoiceOption, test_validate)
{
    EXPECT_TRUE(
        m_option.validate(std::string{"waldo"})
        );
    EXPECT_FALSE(m_option.validate(std::string{"grault"}));
}

TEST_F(GncMultichoiceOption, test_set_value)
{
    EXPECT_NO_THROW({
            m_option.set_value(std::string{"pork"});
            EXPECT_STREQ("pork", m_option.get_value<std::string>().c_str());
        });
    EXPECT_THROW({ m_option.set_value(std::string{"salt"}); }, std::invalid_argument);
    EXPECT_STREQ("pork", m_option.get_value<std::string>().c_str());
}

TEST_F(GncMultichoiceOption, test_num_permissible)
{
    EXPECT_EQ(4, m_option.num_permissible_values());
}

TEST_F(GncMultichoiceOption, test_permissible_value_stuff)
{
    EXPECT_NO_THROW({
            EXPECT_EQ(3, m_option.permissible_value_index("corge"));
            EXPECT_STREQ("waldo", m_option.permissible_value(1).c_str());
            EXPECT_STREQ("sausage", m_option.permissible_value_name(2).c_str());
            EXPECT_STREQ("thud",
                         m_option.permissible_value_description(0).c_str());
        });
    EXPECT_THROW({ auto result = m_option.permissible_value(7); },
                 std::out_of_range);
    EXPECT_THROW({ auto result = m_option.permissible_value_name(9); },
        std::out_of_range);
    EXPECT_THROW({ auto result = m_option.permissible_value_description(4); },
        std::out_of_range);
    EXPECT_EQ(std::numeric_limits<std::size_t>::max(),
              m_option.permissible_value_index("xyzzy"));
}

class GncOptionDateOptionTest : public ::testing::Test
{
protected:
    GncOptionDateOptionTest() :
        m_option{GncOptionDateValue{"foo", "bar", "a", "Phony Date Option"}} {}

    GncOptionDateValue m_option;
};

using GncDateOption = GncOptionDateOptionTest;

static time64
time64_from_gdate(const GDate* g_date, DayPart when)
{
    GncDate date{g_date_get_year(g_date), g_date_get_month(g_date),
            g_date_get_day(g_date)};
    GncDateTime time{date, when};
    return static_cast<time64>(time);
}

TEST_F(GncDateOption, test_set_and_get_absolute)
{
    time64 time1{static_cast<time64>(GncDateTime("2019-07-19 15:32:26 +05:00"))};
    m_option.set_value(time1);
    EXPECT_EQ(time1, m_option.get_value());
}

TEST_F(GncDateOption, test_set_and_get_month_start)
{
    GDate month_start;
    g_date_set_time_t(&month_start, time(nullptr));
    gnc_gdate_set_month_start(&month_start);
    time64 time1{time64_from_gdate(&month_start, DayPart::start)};
    m_option.set_value(RelativeDatePeriod::START_THIS_MONTH);
    EXPECT_EQ(time1, m_option.get_value());
}

TEST_F(GncDateOption, test_set_and_get_month_end)
{
    GDate month_end;
    g_date_set_time_t(&month_end, time(nullptr));
    gnc_gdate_set_month_end(&month_end);
    time64 time1{time64_from_gdate(&month_end, DayPart::end)};
    m_option.set_value(RelativeDatePeriod::END_THIS_MONTH);
    EXPECT_EQ(time1, m_option.get_value());
}

TEST_F(GncDateOption, test_set_and_get_prev_month_start)
{
    GDate prev_month_start;
    g_date_set_time_t(&prev_month_start, time(nullptr));
    gnc_gdate_set_prev_month_start(&prev_month_start);
    time64 time1{time64_from_gdate(&prev_month_start, DayPart::start)};
    m_option.set_value(RelativeDatePeriod::START_PREV_MONTH);
    EXPECT_EQ(time1, m_option.get_value());
}

TEST_F(GncDateOption, test_set_and_get_prev_month_end)
{
    GDate prev_month_end;
    g_date_set_time_t(&prev_month_end, time(nullptr));
    gnc_gdate_set_prev_month_end(&prev_month_end);
    time64 time1{time64_from_gdate(&prev_month_end, DayPart::end)};
    m_option.set_value(RelativeDatePeriod::END_PREV_MONTH);
    EXPECT_EQ(time1, m_option.get_value());
}

TEST_F(GncDateOption, test_set_and_get_quarter_start)
{
    GDate quarter_start;
    g_date_set_time_t(&quarter_start, time(nullptr));
    gnc_gdate_set_quarter_start(&quarter_start);
    time64 time1{time64_from_gdate(&quarter_start, DayPart::start)};
    m_option.set_value(RelativeDatePeriod::START_CURRENT_QUARTER);
    EXPECT_EQ(time1, m_option.get_value());
}

TEST_F(GncDateOption, test_set_and_get_quarter_end)
{
    GDate quarter_end;
    g_date_set_time_t(&quarter_end, time(nullptr));
    gnc_gdate_set_quarter_end(&quarter_end);
    time64 time1{time64_from_gdate(&quarter_end, DayPart::end)};
    m_option.set_value(RelativeDatePeriod::END_CURRENT_QUARTER);
    EXPECT_EQ(time1, m_option.get_value());
}

TEST_F(GncDateOption, test_set_and_get_prev_quarter_start)
{
    GDate prev_quarter_start;
    g_date_set_time_t(&prev_quarter_start, time(nullptr));
    gnc_gdate_set_prev_quarter_start(&prev_quarter_start);
    time64 time1{time64_from_gdate(&prev_quarter_start, DayPart::start)};
    m_option.set_value(RelativeDatePeriod::START_PREV_QUARTER);
    EXPECT_EQ(time1, m_option.get_value());
}

TEST_F(GncDateOption, test_set_and_get_prev_quarter_end)
{
    GDate prev_quarter_end;
    g_date_set_time_t(&prev_quarter_end, time(nullptr));
    gnc_gdate_set_prev_quarter_end(&prev_quarter_end);
    time64 time1{time64_from_gdate(&prev_quarter_end, DayPart::end)};
    m_option.set_value(RelativeDatePeriod::END_PREV_QUARTER);
    EXPECT_EQ(time1, m_option.get_value());
}

TEST_F(GncDateOption, test_set_and_get_year_start)
{
    GDate year_start;
    g_date_set_time_t(&year_start, time(nullptr));
    gnc_gdate_set_year_start(&year_start);
    time64 time1{time64_from_gdate(&year_start, DayPart::start)};
    m_option.set_value(RelativeDatePeriod::START_CAL_YEAR);
    EXPECT_EQ(time1, m_option.get_value());
}

TEST_F(GncDateOption, test_set_and_get_year_end)
{
    GDate year_end;
    g_date_set_time_t(&year_end, time(nullptr));
    gnc_gdate_set_year_end(&year_end);
    time64 time1{time64_from_gdate(&year_end, DayPart::end)};
    m_option.set_value(RelativeDatePeriod::END_CAL_YEAR);
    EXPECT_EQ(time1, m_option.get_value());
}

TEST_F(GncDateOption, test_set_and_get_prev_year_start)
{
    GDate prev_year_start;
    g_date_set_time_t(&prev_year_start, time(nullptr));
    gnc_gdate_set_prev_year_start(&prev_year_start);
    time64 time1{time64_from_gdate(&prev_year_start, DayPart::start)};
    m_option.set_value(RelativeDatePeriod::START_PREV_YEAR);
    EXPECT_EQ(time1, m_option.get_value());
}

TEST_F(GncDateOption, test_set_and_get_prev_year_end)
{
    GDate prev_year_end;
    g_date_set_time_t(&prev_year_end, time(nullptr));
    gnc_gdate_set_prev_year_end(&prev_year_end);
    time64 time1{time64_from_gdate(&prev_year_end, DayPart::end)};
    m_option.set_value(RelativeDatePeriod::END_PREV_YEAR);
    EXPECT_EQ(time1, m_option.get_value());
}

TEST_F(GncDateOption, test_stream_out)
{
    time64 time1{static_cast<time64>(GncDateTime("2019-07-19 15:32:26 +05:00"))};
    m_option.set_value(time1);
    std::ostringstream oss;
    oss << time1;
    std::string timestr{"absolute . "};
    timestr += oss.str();
    oss.str("");
    oss << m_option;
    EXPECT_EQ(oss.str(), timestr);

    m_option.set_value(RelativeDatePeriod::TODAY);
    oss.str("");
    oss << m_option;
    EXPECT_STREQ(oss.str().c_str(), "relative . today");

    m_option.set_value(RelativeDatePeriod::START_THIS_MONTH);
    oss.str("");
    oss << m_option;
    EXPECT_STREQ(oss.str().c_str(), "relative . start-this-month");

    m_option.set_value(RelativeDatePeriod::END_THIS_MONTH);
    oss.str("");
    oss << m_option;
    EXPECT_STREQ(oss.str().c_str(), "relative . end-this-month");

    m_option.set_value(RelativeDatePeriod::START_PREV_MONTH);
    oss.str("");
    oss << m_option;
    EXPECT_STREQ(oss.str().c_str(), "relative . start-prev-month");

    m_option.set_value(RelativeDatePeriod::END_PREV_MONTH);
    oss.str("");
    oss << m_option;
    EXPECT_STREQ(oss.str().c_str(), "relative . end-prev-month");

    m_option.set_value(RelativeDatePeriod::START_CURRENT_QUARTER);
    oss.str("");
    oss << m_option;
    EXPECT_STREQ(oss.str().c_str(), "relative . start-current-quarter");

    m_option.set_value(RelativeDatePeriod::END_CURRENT_QUARTER);
    oss.str("");
    oss << m_option;
    EXPECT_STREQ(oss.str().c_str(), "relative . end-current-quarter");

    m_option.set_value(RelativeDatePeriod::START_PREV_QUARTER);
    oss.str("");
    oss << m_option;
    EXPECT_STREQ(oss.str().c_str(), "relative . start-prev-quarter");

    m_option.set_value(RelativeDatePeriod::END_PREV_QUARTER);
    oss.str("");
    oss << m_option;
    EXPECT_STREQ(oss.str().c_str(), "relative . end-prev-quarter");

    m_option.set_value(RelativeDatePeriod::START_CAL_YEAR);
    oss.str("");
    oss << m_option;
    EXPECT_STREQ(oss.str().c_str(), "relative . start-cal-year");

    m_option.set_value(RelativeDatePeriod::END_CAL_YEAR);
    oss.str("");
    oss << m_option;
    EXPECT_STREQ(oss.str().c_str(), "relative . end-cal-year");

    m_option.set_value(RelativeDatePeriod::START_PREV_YEAR);
    oss.str("");
    oss << m_option;
    EXPECT_STREQ(oss.str().c_str(), "relative . start-prev-year");

    m_option.set_value(RelativeDatePeriod::END_PREV_YEAR);
    oss.str("");
    oss << m_option;
    EXPECT_STREQ(oss.str().c_str(), "relative . end-prev-year");

    m_option.set_value(RelativeDatePeriod::START_ACCOUNTING_PERIOD);
    oss.str("");
    oss << m_option;
    EXPECT_STREQ(oss.str().c_str(), "relative . start-prev-fin-year");

    m_option.set_value(RelativeDatePeriod::END_ACCOUNTING_PERIOD);
    oss.str("");
    oss << m_option;
    EXPECT_STREQ(oss.str().c_str(), "relative . end-prev-fin-year");
}

TEST_F(GncDateOption, test_stream_in_absolute)
{
    time64 time1{static_cast<time64>(GncDateTime("2019-07-19 15:32:26 +05:00"))};
    std::ostringstream oss;
    oss << time1;
    std::string timestr{"absolute . "};
    timestr += oss.str();

    std::istringstream iss{timestr};
    iss >> m_option;
    EXPECT_EQ(time1, m_option.get_value());
}

TEST_F(GncDateOption, test_stream_in_month_start)
{
    GDate month_start;
    g_date_set_time_t(&month_start, time(nullptr));
    gnc_gdate_set_month_start(&month_start);
    time64 time1{time64_from_gdate(&month_start, DayPart::start)};
    std::istringstream iss{"relative . start-this-month"};
    iss >> m_option;
    EXPECT_EQ(time1, m_option.get_value());
}


TEST_F(GncDateOption, test_stream_in_month_end)
{
    GDate month_end;
    g_date_set_time_t(&month_end, time(nullptr));
    gnc_gdate_set_month_end(&month_end);
    time64 time1{time64_from_gdate(&month_end, DayPart::end)};
    std::istringstream iss{"relative . end-this-month"};
    iss >> m_option;
    EXPECT_EQ(time1, m_option.get_value());
}

TEST_F(GncDateOption, test_stream_in_prev_month_start)
{
    GDate prev_month_start;
    g_date_set_time_t(&prev_month_start, time(nullptr));
    gnc_gdate_set_prev_month_start(&prev_month_start);
    time64 time1{time64_from_gdate(&prev_month_start, DayPart::start)};
    std::istringstream iss{"relative . start-prev-month"};
    iss >> m_option;
    EXPECT_EQ(time1, m_option.get_value());
}

TEST_F(GncDateOption, test_stream_in_prev_month_end)
{
    GDate prev_month_end;
    g_date_set_time_t(&prev_month_end, time(nullptr));
    gnc_gdate_set_prev_month_end(&prev_month_end);
    time64 time1{time64_from_gdate(&prev_month_end, DayPart::end)};
    std::istringstream iss{"relative . end-prev-month"};
    iss >> m_option;
    EXPECT_EQ(time1, m_option.get_value());
}

TEST_F(GncDateOption, test_stream_in_quarter_start)
{
    GDate quarter_start;
    g_date_set_time_t(&quarter_start, time(nullptr));
    gnc_gdate_set_quarter_start(&quarter_start);
    time64 time1{time64_from_gdate(&quarter_start, DayPart::start)};
    std::istringstream iss{"relative . start-current-quarter"};
    iss >> m_option;
    EXPECT_EQ(time1, m_option.get_value());
}

TEST_F(GncDateOption, test_stream_in_quarter_end)
{
    GDate quarter_end;
    g_date_set_time_t(&quarter_end, time(nullptr));
    gnc_gdate_set_quarter_end(&quarter_end);
    time64 time1{time64_from_gdate(&quarter_end, DayPart::end)};
    std::istringstream iss{"relative . end-current-quarter"};
    iss >> m_option;
    EXPECT_EQ(time1, m_option.get_value());
}

TEST_F(GncDateOption, test_stream_in_prev_quarter_start)
{
    GDate prev_quarter_start;
    g_date_set_time_t(&prev_quarter_start, time(nullptr));
    gnc_gdate_set_prev_quarter_start(&prev_quarter_start);
    time64 time1{time64_from_gdate(&prev_quarter_start, DayPart::start)};
    std::istringstream iss{"relative . start-prev-quarter"};
    iss >> m_option;
    EXPECT_EQ(time1, m_option.get_value());
}

TEST_F(GncDateOption, test_stream_in_prev_quarter_end)
{
    GDate prev_quarter_end;
    g_date_set_time_t(&prev_quarter_end, time(nullptr));
    gnc_gdate_set_prev_quarter_end(&prev_quarter_end);
    time64 time1{time64_from_gdate(&prev_quarter_end, DayPart::end)};
    std::istringstream iss{"relative . end-prev-quarter"};
    iss >> m_option;
    EXPECT_EQ(time1, m_option.get_value());
}

TEST_F(GncDateOption, test_stream_in_year_start)
{
    GDate year_start;
    g_date_set_time_t(&year_start, time(nullptr));
    gnc_gdate_set_year_start(&year_start);
    time64 time1{time64_from_gdate(&year_start, DayPart::start)};
    std::istringstream iss{"relative . start-cal-year"};
    iss >> m_option;
    EXPECT_EQ(time1, m_option.get_value());
}

TEST_F(GncDateOption, test_stream_in_year_end)
{
    GDate year_end;
    g_date_set_time_t(&year_end, time(nullptr));
    gnc_gdate_set_year_end(&year_end);
    time64 time1{time64_from_gdate(&year_end, DayPart::end)};
    std::istringstream iss{"relative . end-cal-year"};
    iss >> m_option;
    EXPECT_EQ(time1, m_option.get_value());
}

TEST_F(GncDateOption, test_stream_in_prev_year_start)
{
    GDate prev_year_start;
    g_date_set_time_t(&prev_year_start, time(nullptr));
    gnc_gdate_set_prev_year_start(&prev_year_start);
    time64 time1{time64_from_gdate(&prev_year_start, DayPart::start)};
    std::istringstream iss{"relative . start-prev-year"};
    iss >> m_option;
    EXPECT_EQ(time1, m_option.get_value());
}

TEST_F(GncDateOption, test_stream_in_prev_year_end)
{
    GDate prev_year_end;
    g_date_set_time_t(&prev_year_end, time(nullptr));
    gnc_gdate_set_prev_year_end(&prev_year_end);
    time64 time1{time64_from_gdate(&prev_year_end, DayPart::end)};
    std::istringstream iss{"relative . end-prev-year"};
    iss >> m_option;
    EXPECT_EQ(time1, m_option.get_value());
}
