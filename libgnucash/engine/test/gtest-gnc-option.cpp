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
#include "gnc-option.hpp"
#include "gnc-option-impl.hpp"
#include "gnc-option-ui.hpp"
#include "guid.hpp"
#include <config.h>
#include "qof.h"
#include "Account.h"
#include "gnc-budget.h"
#include "gnc-commodity.h"
#include "gnc-date.h"
#include <time.h>
#include "gnc-session.h"

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
    EXPECT_STREQ(oss.str().c_str(), "False");
    oss.str("");
    option.set_value(true);
    oss << option;
    EXPECT_STREQ(oss.str().c_str(), "True");
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
    GncOptionTest() : m_session{gnc_get_current_session()},
                      m_book{qof_session_get_book(gnc_get_current_session())} {}
    ~GncOptionTest() { gnc_clear_current_session(); }

    QofSession* m_session;
    QofBook* m_book;
};

TEST_F(GncOptionTest, test_budget_ctor)
{
    auto budget = gnc_budget_new(m_book);
    EXPECT_NO_THROW({
            GncOption option(GncOptionQofInstanceValue{"foo", "bar", "baz",
                                                       "Phony Option",
                                                       (const QofInstance*)budget});
        });
    gnc_budget_destroy(budget);
}

TEST_F(GncOptionTest, test_budget_out)
{
    auto budget = gnc_budget_new(m_book);
    GncOption option{GncOptionQofInstanceValue{"foo", "bar", "baz", "Phony Option", (const QofInstance*)budget}};

    auto budget_guid{gnc::GUID{*qof_instance_get_guid(QOF_INSTANCE(budget))}.to_string()};
    std::ostringstream oss;
    oss << option;
    EXPECT_EQ(budget_guid, oss.str());
    gnc_budget_destroy(budget);
}

TEST_F(GncOptionTest, test_budget_in)
{
    auto budget = gnc_budget_new(m_book);
    auto budget_guid{gnc::GUID{*qof_instance_get_guid(QOF_INSTANCE(budget))}.to_string()};
    std::istringstream iss{budget_guid};
    GncOption option{GncOptionQofInstanceValue{"foo", "bar", "baz", "Phony Option", nullptr, GncOptionUIType::BUDGET}};
    iss >> option;
    EXPECT_EQ(QOF_INSTANCE(budget), option.get_value<const QofInstance*>());
    gnc_budget_destroy(budget);
}

TEST_F(GncOptionTest, test_commodity_ctor)
{
    auto hpe = gnc_commodity_new(m_book, "Hewlett Packard Enterprise, Inc.",
                                    "NYSE", "HPE", NULL, 1);
    EXPECT_NO_THROW({
            GncOption option(GncOptionQofInstanceValue{"foo", "bar", "baz",
                                                       "Phony Option",
                                                       (const QofInstance*)hpe});
    });
    gnc_commodity_destroy(hpe);
}

class GncOptionCommodityTest : public ::testing::Test
{
protected:
    GncOptionCommodityTest() : m_session{gnc_get_current_session()},
                               m_book{qof_session_get_book(gnc_get_current_session())},
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
    GncOption option{GncOptionCommodityValue{
        section, name, key, doc_string, value,
        is_currency ? GncOptionUIType::CURRENCY : GncOptionUIType::COMMODITY}
    };
    return option;
}

TEST_F(GncOptionCommodityTest, test_currency_ctor)
{
    EXPECT_THROW({
            auto option = make_currency_option("foo", "bar", "baz",
                                               "Phony Option", m_hpe, true);
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
    auto option = make_currency_option("foo", "bar", "baz", "Phony Option",
                                      m_eur, true);
    EXPECT_NO_THROW({
            option.set_value(m_usd);
        });
    EXPECT_PRED2(gnc_commodity_equal, m_usd,
                 GNC_COMMODITY(option.get_value<gnc_commodity*>()));
    EXPECT_THROW({
            option.set_value(m_hpe);
        }, std::invalid_argument);
    EXPECT_PRED2(gnc_commodity_equal, m_usd,
                 GNC_COMMODITY(option.get_value<gnc_commodity *>()));
}

TEST_F(GncOptionCommodityTest, test_currency_validator)
{
    auto option = make_currency_option("foo", "bar", "baz", "Phony Option",
                                      m_eur, true);
    EXPECT_TRUE(option.validate(m_usd));
    EXPECT_FALSE(option.validate(m_aapl));
}

static inline std::string make_currency_str(gnc_commodity* cur)
{
    std::string cur_str{gnc_commodity_get_mnemonic(cur)};
    return cur_str;
}

static inline std::string make_commodity_str(gnc_commodity* com)
{
    std::string com_str{gnc_commodity_get_namespace(com)};
    com_str += ":";
    com_str += gnc_commodity_get_mnemonic(com);
    return com_str;
}

TEST_F(GncOptionCommodityTest, test_currency_out)
{
    auto option = make_currency_option("foo", "bar", "baz", "Phony Option",
                                       m_eur, true);

    std::string eur_str{make_currency_str(m_eur)};
    std::ostringstream oss;
    oss << option;
    EXPECT_EQ(eur_str, oss.str());
}

TEST_F(GncOptionCommodityTest, test_commodity_out)
{
    GncOption option{GncOptionQofInstanceValue{"foo", "bar", "baz", "Phony Option", (const QofInstance*)m_hpe,
                     GncOptionUIType::COMMODITY}};
    std::string hpe_str{make_commodity_str(m_hpe)};
    std::ostringstream oss;
    oss << option;
    EXPECT_EQ(hpe_str, oss.str());
}

TEST_F(GncOptionCommodityTest, test_currency_in)
{

    auto option = make_currency_option("foo", "bar", "baz", "Phony Option",
                                       m_eur, true);

    EXPECT_THROW({
            std::string hpe_str{make_commodity_str(m_hpe)};
            std::istringstream iss{hpe_str};
            iss >> option;
        }, std::invalid_argument);
    EXPECT_NO_THROW({
            std::string usd_str{make_currency_str(m_usd)};
            std::istringstream iss{usd_str};
            iss >> option;
            EXPECT_EQ(m_usd, option.get_value<gnc_commodity*>());
        });
}

TEST_F(GncOptionCommodityTest, test_commodity_in)
{
    GncOption option{GncOptionCommodityValue{"foo", "bar", "baz", "Phony Option", m_aapl,
                     GncOptionUIType::COMMODITY}};

    std::string hpe_str{make_commodity_str(m_hpe)};
    std::istringstream iss{hpe_str};
    iss >> option;
    EXPECT_EQ(m_hpe, option.get_value<gnc_commodity*>());
}

class GncUIType
{
public:
    void set_value(const std::string& value) const noexcept { m_value = value; }
    const std::string& get_value() const noexcept { return m_value; }
    void clear() noexcept { m_value.clear(); }
private:
    mutable std::string m_value;
};


class OptionUIItem : public GncOptionUIItem
{
    GncUIType m_widget;
    bool m_dirty = false;
public:
    OptionUIItem() : GncOptionUIItem{GncOptionUIType::STRING} {}
    ~OptionUIItem() = default;
    void set_dirty(bool status) noexcept override { m_dirty = status; }
    bool get_dirty() const noexcept override { return m_dirty; }
    void set_selectable(bool selectable) const noexcept override {}
    void clear_ui_item() override { m_widget.clear(); }
    void set_ui_item_from_option(GncOption& option) noexcept override
    {
        m_widget.set_value(option.get_value<std::string>());
    }
    void set_option_from_ui_item(GncOption& option) noexcept override
    {
        option.set_value(m_widget.get_value());
    }
    void set_widget_value(const std::string& value) const noexcept
    {
        m_widget.set_value(value);
    }
    const std::string& get_widget_value() const noexcept
    {
        return m_widget.get_value();
    }
};

class GncOptionUITest : public ::testing::Test
{
protected:
    GncOptionUITest() :
        m_option{"foo", "bar", "baz", "Phony Option", std::string{"waldo"},
                 GncOptionUIType::STRING}
    {
        auto ui_item{std::make_unique<OptionUIItem>()};
        m_option.set_ui_item(std::move(ui_item));
    }
    GncOption m_option;
};

using GncOptionUI = GncOptionUITest;

TEST_F(GncOptionUI, test_option_ui_type)
{
    EXPECT_EQ(GncOptionUIType::STRING, m_option.get_ui_type());
}

TEST_F(GncOptionUI, test_ui_value_from_option)
{
    const char* value{"waldo"};

    m_option.set_value(value);
    m_option.set_ui_item_from_option();
    auto ui_item{dynamic_cast<const OptionUIItem*>(m_option.get_ui_item())};
    ASSERT_TRUE(ui_item != nullptr);
    EXPECT_STREQ(value, ui_item->get_widget_value().c_str());
}

TEST_F(GncOptionUI, test_option_value_from_ui)
{
    const char* value{"pepper"};
    auto ui_item{dynamic_cast<const OptionUIItem*>(m_option.get_ui_item())};
    ASSERT_TRUE(ui_item != nullptr);
    ui_item->set_widget_value(value);
    m_option.set_option_from_ui_item();
    EXPECT_STREQ(value, m_option.get_value<std::string>().c_str());
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

TEST_F(GncRangeOption, test_get_info)
{
    int imax{}, imin{}, istep{};
    double dmax{}, dmin{}, dstep{};
    m_intoption.get_limits(imax, imin, istep);
    m_doubleoption.get_limits(dmax, dmin, dstep);
    EXPECT_EQ(30, imax);
    EXPECT_EQ(1, imin);
    EXPECT_EQ(1, istep);
    EXPECT_EQ(1.0, dmin);
    EXPECT_EQ(3.0, dmax);
    EXPECT_EQ(0.1, dstep);
}

TEST_F(GncRangeOption, test_range_out)
{
    std::ostringstream oss;
    oss << "Integer " << m_intoption << " Double " << m_doubleoption << ".";
    EXPECT_STREQ("Integer 15 Double 1.500000.", oss.str().c_str());
}

TEST_F(GncRangeOption, test_range_in)
{
    std::istringstream iss{std::string{"45 4.5 20 2.0"}};
    EXPECT_THROW({ iss >> m_intoption; }, std::invalid_argument);
    EXPECT_THROW({ iss >> m_doubleoption; }, std::invalid_argument);
    EXPECT_NO_THROW({ iss >> m_intoption; });
    EXPECT_NO_THROW({ iss >> m_doubleoption; });
    EXPECT_EQ(20, m_intoption.get_value<int>());
    EXPECT_EQ(2.0, m_doubleoption.get_value<double>());
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
        list.push_back(*qof_entity_get_guid(account));
}

class GncOptionAccountTest : public ::testing::Test
{
protected:
    GncOptionAccountTest() :
        m_session{gnc_get_current_session()},
        m_book{qof_session_get_book(gnc_get_current_session())},
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

static bool
operator==(const GncGUID& l, const GncGUID& r)
{
    return guid_equal(&l, &r);
}

TEST_F(GncOptionAccountTest, test_test_constructor_and_destructor)
{
    EXPECT_TRUE(m_book != NULL);
    EXPECT_TRUE(QOF_IS_BOOK(m_book));
    EXPECT_TRUE(m_root != NULL);
    EXPECT_TRUE(GNC_IS_ACCOUNT(m_root));
    GncOptionAccountList list{list_of_types({ACCT_TYPE_BANK})};
    EXPECT_EQ(2U, list.size());
    list = list_of_types({ACCT_TYPE_ASSET, ACCT_TYPE_STOCK});
    EXPECT_EQ(6U, list.size());
}

TEST_F(GncOptionAccountTest, test_option_no_value_constructor)
{
    GncOptionAccountListValue option{"foo", "bar", "baz", "Bogus Option",
            GncOptionUIType::ACCOUNT_LIST};
    EXPECT_TRUE(option.get_value().empty());
    EXPECT_TRUE(option.get_default_value().empty());
}

TEST_F(GncOptionAccountTest, test_option_value_constructor)
{
    GncOptionAccountList acclist{list_of_types({ACCT_TYPE_BANK})};
    GncOptionAccountListValue option{"foo", "bar", "baz", "Bogus Option",
            GncOptionUIType::ACCOUNT_LIST, acclist};
    EXPECT_EQ(2U, option.get_value().size());
    EXPECT_EQ(2U, option.get_default_value().size());
    EXPECT_EQ(acclist[0], option.get_value()[0]);
}

TEST_F(GncOptionAccountTest, test_option_no_value_limited_constructor)
{
    GncOptionAccountList acclistgood{list_of_types({ACCT_TYPE_BANK})};
    GncOptionAccountList acclistbad{list_of_types({ACCT_TYPE_STOCK})};
    GncOptionAccountListValue option{"foo", "bar", "baz", "Bogus Option",
            GncOptionUIType::ACCOUNT_LIST,
            GncOptionAccountTypeList{ACCT_TYPE_BANK}};
    EXPECT_EQ(1U, option.get_value().size());
    EXPECT_EQ(1U, option.get_default_value().size());
    EXPECT_EQ(true, option.validate(acclistgood));
    EXPECT_EQ(false, option.validate(acclistbad));
}

TEST_F(GncOptionAccountTest, test_option_value_limited_constructor)
{
    GncOptionAccountList acclistgood{list_of_types({ACCT_TYPE_BANK})};
    GncOptionAccountList acclistbad{list_of_types({ACCT_TYPE_STOCK})};
    EXPECT_THROW({
            GncOptionAccountListValue option("foo", "bar", "baz", "Bogus Option",
                                         GncOptionUIType::ACCOUNT_LIST,
                                         acclistbad,
                                         GncOptionAccountTypeList{ACCT_TYPE_BANK});
        }, std::invalid_argument);

    EXPECT_THROW({
            GncOptionAccountListValue option("foo", "bar", "baz", "Bogus Option",
                                         GncOptionUIType::ACCOUNT_SEL,
                                         acclistgood,
                                         GncOptionAccountTypeList{ACCT_TYPE_BANK});
        }, std::invalid_argument);

    EXPECT_NO_THROW({
            GncOptionAccountListValue option("foo", "bar", "baz", "Bogus Option",
                                         GncOptionUIType::ACCOUNT_LIST,
                                         acclistgood,
                                         GncOptionAccountTypeList{ACCT_TYPE_BANK});
        });

    EXPECT_NO_THROW({
            GncOptionAccountList accsel{acclistgood[0]};
            GncOptionAccountListValue option("foo", "bar", "baz", "Bogus Option",
                                         GncOptionUIType::ACCOUNT_LIST,
                                         accsel,
                                         GncOptionAccountTypeList{ACCT_TYPE_BANK});
        });
    GncOptionAccountListValue option {"foo", "bar", "baz", "Bogus Option",
                                  GncOptionUIType::ACCOUNT_LIST, acclistgood,
                                  GncOptionAccountTypeList{ACCT_TYPE_BANK}};
    EXPECT_FALSE(option.get_value().empty());
    EXPECT_FALSE(option.get_default_value().empty());
    EXPECT_EQ(true, option.validate(acclistgood));
    EXPECT_EQ(false, option.validate(acclistbad));
}

TEST_F(GncOptionAccountTest, test_account_list_out)
{
    GncOptionAccountList acclist{list_of_types({ACCT_TYPE_BANK})};
    GncOption option{GncOptionAccountListValue{"foo", "bar", "baz", "Bogus Option",
                                           GncOptionUIType::ACCOUNT_LIST,
                                           acclist}};
    std::ostringstream oss;
    std::string acc_guids{gnc::GUID{acclist[0]}.to_string()};
    acc_guids += " ";
    acc_guids += gnc::GUID{acclist[1]}.to_string();

    oss << option;
    EXPECT_EQ(acc_guids, oss.str());

    GncOptionAccountList accsel{acclist[0]};
    GncOption sel_option{GncOptionAccountListValue{"foo", "bar", "baz",
                                               "Bogus Option",
                                               GncOptionUIType::ACCOUNT_LIST,
                                               accsel,
                                               GncOptionAccountTypeList{ACCT_TYPE_BANK}}};
    acc_guids = gnc::GUID{accsel[0]}.to_string();

    oss.str("");
    oss << sel_option;
    EXPECT_EQ(acc_guids, oss.str());
}

TEST_F(GncOptionAccountTest, test_account_list_in)
{
    GncOptionAccountList acclist{list_of_types({ACCT_TYPE_BANK})};
    GncOption option{GncOptionAccountListValue{"foo", "bar", "baz", "Bogus Option",
                                           GncOptionUIType::ACCOUNT_LIST,
                                           acclist}};
    std::string acc_guids{gnc::GUID{acclist[0]}.to_string()};
    acc_guids += " ";
    acc_guids += gnc::GUID{acclist[1]}.to_string();

    std::istringstream iss{acc_guids};
    iss >> option;
    EXPECT_EQ(acclist, option.get_value<GncOptionAccountList>());

    GncOptionAccountList accsel{acclist[0]};
    GncOption sel_option{GncOptionAccountListValue{"foo", "bar", "baz",
                                               "Bogus Option",
                                               GncOptionUIType::ACCOUNT_LIST,
                                               accsel,
                                               GncOptionAccountTypeList{ACCT_TYPE_BANK}}};
    GncOptionAccountList acclistbad{list_of_types({ACCT_TYPE_STOCK})};
    acc_guids = gnc::GUID{acclistbad[1]}.to_string();
    acc_guids += " ";

    iss.str(acc_guids);
    iss >> sel_option;
    EXPECT_EQ(accsel, sel_option.get_value<GncOptionAccountList>());

    iss.clear();  //Reset the failedbit from the invalid selection type.
    acc_guids = gnc::GUID{acclist[1]}.to_string();
    EXPECT_NO_THROW({
            iss.str(acc_guids);
            iss >> sel_option;
        });
    EXPECT_EQ(acclist[1], sel_option.get_value<GncOptionAccountList>()[0]);
}

using KT = GncOptionMultichoiceKeyType;
class GncOptionMultichoiceTest : public ::testing::Test
{
protected:
    GncOptionMultichoiceTest() :
        m_option{GncOptionMultichoiceValue
                 {"foo", "bar", "baz",
                  "Phony Option", "plugh",
                  {
                      {"plugh", "xyzzy", KT::STRING},
                      {"waldo", "pepper", KT::STRING},
                      {"pork", "sausage", KT::STRING},
                      {"corge", "grault", KT::STRING}
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
    EXPECT_EQ(4U, m_option.num_permissible_values());
}

TEST_F(GncMultichoiceOption, test_permissible_value_stuff)
{
    EXPECT_NO_THROW({
            EXPECT_EQ(3U, m_option.permissible_value_index("corge"));
            EXPECT_STREQ("waldo", m_option.permissible_value(1));
            EXPECT_STREQ("sausage", m_option.permissible_value_name(2));
        });
    EXPECT_THROW({ auto result = m_option.permissible_value(7); },
                 std::out_of_range);
    EXPECT_THROW({ auto result = m_option.permissible_value_name(9); },
        std::out_of_range);
    EXPECT_EQ(std::numeric_limits<uint16_t>::max(),
              m_option.permissible_value_index("xyzzy"));
}

TEST_F(GncMultichoiceOption, test_multichoice_out)
{
    std::ostringstream oss;
    oss << m_option;
    EXPECT_EQ(oss.str(), m_option.get_value<std::string>());
}

TEST_F(GncMultichoiceOption, test_multichoice_in)
{
    std::istringstream iss{"grault"};
    EXPECT_THROW({
            iss >> m_option;
        }, std::invalid_argument);
    iss.clear(); //reset failedbit
    iss.str("pork");
    iss >> m_option;
    EXPECT_EQ(iss.str(), m_option.get_value<std::string>());
}

class GncOptionListTest : public ::testing::Test
{
protected:
    GncOptionListTest() :
        m_option{GncOptionMultichoiceValue{
            "foo", "bar", "baz", "Phony Option",
            GncMultichoiceOptionIndexVec{0, 2},
            {
                {"plugh", "xyzzy", KT::STRING},
                {"waldo", "pepper", KT::STRING},
                {"pork", "sausage", KT::STRING},
                {"corge", "grault", KT::STRING}
            }}} {}
    GncOption m_option;
};

using GncListOption = GncOptionListTest;

TEST_F(GncListOption, test_option_ui_type)
{
    EXPECT_EQ(GncOptionUIType::LIST, m_option.get_ui_type());
}

TEST_F(GncListOption, test_validate)
{
    EXPECT_TRUE(m_option.validate(std::string{"pork"}));
    EXPECT_TRUE(m_option.validate(GncMultichoiceOptionIndexVec{1, 3}));
    EXPECT_FALSE(m_option.validate(GncMultichoiceOptionIndexVec{2, 6}));
}

TEST_F(GncListOption, test_set_value)
{
    EXPECT_NO_THROW({
            m_option.set_value(GncMultichoiceOptionIndexVec{1, 3});
            EXPECT_STREQ("multiple values",
                         m_option.get_value<std::string>().c_str());
            EXPECT_EQ(1U, m_option.get_value<uint16_t>());
            auto vec{m_option.get_value<GncMultichoiceOptionIndexVec>()};
            ASSERT_EQ(2U, vec.size());
            EXPECT_EQ(1U, vec[0]);
            EXPECT_EQ(3U, vec[1]);
        });
    EXPECT_THROW({ m_option.set_value(GncMultichoiceOptionIndexVec{2, 5}); }, std::invalid_argument);
    EXPECT_EQ(1U, m_option.get_value<uint16_t>());
}

TEST_F(GncListOption, test_list_out)
{
    auto vec{m_option.get_value<GncMultichoiceOptionIndexVec>()};
    std::string value{m_option.permissible_value(vec[0])};
    value += " ";
    value += m_option.permissible_value(vec[1]);
    std::ostringstream oss;
    oss << m_option;
    EXPECT_EQ(oss.str(), value);
}

TEST_F(GncListOption, test_list_in)
{
    std::istringstream iss{"grault"};
    EXPECT_THROW({
            iss >> m_option;
        }, std::invalid_argument);
    iss.clear(); //reset failedbit
    iss.str("pork");
    iss >> m_option;
    EXPECT_EQ(iss.str(), m_option.get_value<std::string>());
}

static time64
time64_from_gdate(const GDate* g_date, DayPart when)
{
    GncDate date{g_date_get_year(g_date), g_date_get_month(g_date),
            g_date_get_day(g_date)};
    GncDateTime time{date, when};
    return static_cast<time64>(time);
}

TEST(GncOptionDate, test_gnc_relative_date_is_single)
{
    EXPECT_FALSE(gnc_relative_date_is_single(RelativeDatePeriod::ABSOLUTE));
    EXPECT_TRUE(gnc_relative_date_is_single(RelativeDatePeriod::TODAY));
    EXPECT_TRUE(gnc_relative_date_is_single(RelativeDatePeriod::ONE_YEAR_AHEAD));
    EXPECT_FALSE(gnc_relative_date_is_single(RelativeDatePeriod::START_THIS_MONTH));
    EXPECT_FALSE(gnc_relative_date_is_single(RelativeDatePeriod::END_CURRENT_QUARTER));
    EXPECT_FALSE(gnc_relative_date_is_single(RelativeDatePeriod::START_ACCOUNTING_PERIOD));
    EXPECT_FALSE(gnc_relative_date_is_single(RelativeDatePeriod::END_ACCOUNTING_PERIOD));
}

TEST(GncOptionDate, test_gnc_relative_date_is_starting)
{
    EXPECT_FALSE(gnc_relative_date_is_starting(RelativeDatePeriod::ABSOLUTE));
    EXPECT_FALSE(gnc_relative_date_is_starting(RelativeDatePeriod::TODAY));
    EXPECT_FALSE(gnc_relative_date_is_starting(RelativeDatePeriod::ONE_YEAR_AHEAD));
    EXPECT_TRUE(gnc_relative_date_is_starting(RelativeDatePeriod::START_THIS_MONTH));
    EXPECT_FALSE(gnc_relative_date_is_starting(RelativeDatePeriod::END_CURRENT_QUARTER));
    EXPECT_TRUE(gnc_relative_date_is_starting(RelativeDatePeriod::START_ACCOUNTING_PERIOD));
    EXPECT_FALSE(gnc_relative_date_is_starting(RelativeDatePeriod::END_ACCOUNTING_PERIOD));
}

TEST(GncOptionDate, test_gnc_relative_date_is_ending)
{
    EXPECT_FALSE(gnc_relative_date_is_ending(RelativeDatePeriod::ABSOLUTE));
    EXPECT_FALSE(gnc_relative_date_is_ending(RelativeDatePeriod::TODAY));
    EXPECT_FALSE(gnc_relative_date_is_ending(RelativeDatePeriod::ONE_YEAR_AHEAD));
    EXPECT_FALSE(gnc_relative_date_is_ending(RelativeDatePeriod::START_CURRENT_QUARTER));
    EXPECT_TRUE(gnc_relative_date_is_ending(RelativeDatePeriod::END_CURRENT_QUARTER));
    EXPECT_FALSE(gnc_relative_date_is_ending(RelativeDatePeriod::START_ACCOUNTING_PERIOD));
    EXPECT_TRUE(gnc_relative_date_is_ending(RelativeDatePeriod::END_ACCOUNTING_PERIOD));
}

TEST(GncOptionDate, test_gnc_relative_date_storage_string)
{
    EXPECT_EQ(nullptr, gnc_relative_date_storage_string(RelativeDatePeriod::ABSOLUTE));
    EXPECT_STREQ("one-month-ago", gnc_relative_date_storage_string(RelativeDatePeriod::ONE_MONTH_AGO));
}

TEST(GncOptionDate, test_gnc_relative_date_display_string)
{
    EXPECT_EQ(nullptr, gnc_relative_date_display_string(RelativeDatePeriod::ABSOLUTE));
    EXPECT_STREQ("Start of next month", gnc_relative_date_display_string(RelativeDatePeriod::START_NEXT_MONTH));
}

TEST(GncOptionDate, test_gnc_relative_date_description)
{
    EXPECT_EQ(nullptr, gnc_relative_date_description(RelativeDatePeriod::ABSOLUTE));
    EXPECT_STREQ("First day of the next month.", gnc_relative_date_description(RelativeDatePeriod::START_NEXT_MONTH));
}

TEST(GncOptionDate, test_gnc_relative_date_from_storage_string)
{
    //   EXPECT_EQ(RelativeDatePeriod::ABSOLUTE, gnc_relative_date_from_storage_string("foo"));
    EXPECT_EQ(RelativeDatePeriod::ONE_MONTH_AHEAD,  gnc_relative_date_from_storage_string("one-month-ahead"));
    EXPECT_EQ(RelativeDatePeriod::START_CURRENT_QUARTER,  gnc_relative_date_from_storage_string("start-current-quarter"));
    EXPECT_EQ(RelativeDatePeriod::END_ACCOUNTING_PERIOD,  gnc_relative_date_from_storage_string("end-prev-fin-year"));
}

TEST(GncOptionDate, test_gnc_relative_date_to_time64)
{
    GDate date;
    g_date_set_time_t(&date, time(nullptr));
    gnc_gdate_set_month_start(&date);
    time64 time1{time64_from_gdate(&date, DayPart::start)};
    EXPECT_EQ(time1,
              gnc_relative_date_to_time64(RelativeDatePeriod::START_THIS_MONTH));
    g_date_set_time_t(&date, time(nullptr));
    gnc_gdate_set_month_end(&date);
    time1 = time64_from_gdate(&date, DayPart::end);
    EXPECT_EQ(time1,
              gnc_relative_date_to_time64(RelativeDatePeriod::END_THIS_MONTH));

    g_date_set_time_t(&date, time(nullptr));
    gnc_gdate_set_prev_month_start(&date);
    time1 = time64_from_gdate(&date, DayPart::start);
    EXPECT_EQ(time1,
              gnc_relative_date_to_time64(RelativeDatePeriod::START_PREV_MONTH));

    g_date_set_time_t(&date, time(nullptr));
    gnc_gdate_set_prev_month_end(&date);
    time1 = time64_from_gdate(&date, DayPart::end);
    EXPECT_EQ(time1,
              gnc_relative_date_to_time64(RelativeDatePeriod::END_PREV_MONTH));
    g_date_set_time_t(&date, time(nullptr));
    gnc_gdate_set_quarter_start(&date);
    time1 = time64_from_gdate(&date, DayPart::start);
    EXPECT_EQ(time1,
              gnc_relative_date_to_time64(RelativeDatePeriod::START_CURRENT_QUARTER));

    g_date_set_time_t(&date, time(nullptr));
    gnc_gdate_set_quarter_end(&date);
    time1 = time64_from_gdate(&date, DayPart::end);
    EXPECT_EQ(time1,
              gnc_relative_date_to_time64(RelativeDatePeriod::END_CURRENT_QUARTER));

    g_date_set_time_t(&date, time(nullptr));
    gnc_gdate_set_prev_quarter_start(&date);
    time1 = time64_from_gdate(&date, DayPart::start);
    EXPECT_EQ(time1,
              gnc_relative_date_to_time64(RelativeDatePeriod::START_PREV_QUARTER));

    g_date_set_time_t(&date, time(nullptr));
    gnc_gdate_set_prev_quarter_end(&date);
    time1 = time64_from_gdate(&date, DayPart::end);
    EXPECT_EQ(time1,
              gnc_relative_date_to_time64(RelativeDatePeriod::END_PREV_QUARTER));
    g_date_set_time_t(&date, time(nullptr));
    gnc_gdate_set_year_start(&date);
    time1 = time64_from_gdate(&date, DayPart::start);
    EXPECT_EQ(time1,
              gnc_relative_date_to_time64(RelativeDatePeriod::START_CAL_YEAR));
    g_date_set_time_t(&date, time(nullptr));
    gnc_gdate_set_year_end(&date);
    time1 = time64_from_gdate(&date, DayPart::end);
    EXPECT_EQ(time1,
              gnc_relative_date_to_time64(RelativeDatePeriod::END_CAL_YEAR));
    g_date_set_time_t(&date, time(nullptr));
    gnc_gdate_set_prev_year_start(&date);
    time1 = time64_from_gdate(&date, DayPart::start);
    EXPECT_EQ(time1,
              gnc_relative_date_to_time64(RelativeDatePeriod::START_PREV_YEAR));

    g_date_set_time_t(&date, time(nullptr));
    gnc_gdate_set_prev_year_end(&date);
    time1 = time64_from_gdate(&date, DayPart::end);
    EXPECT_EQ(time1,
              gnc_relative_date_to_time64(RelativeDatePeriod::END_PREV_YEAR));
}

class GncOptionDateOptionTest : public ::testing::Test
{
protected:
    GncOptionDateOptionTest() :
        m_option{GncOptionDateValue{"foo", "bar", "a", "Phony Date Option",
                                    GncOptionUIType::DATE_BOTH}} {}
    GncOption m_option;
};

class GncOptionDateOptionListTest : public ::testing::Test
{
protected:
    GncOptionDateOptionListTest() :
        m_option{GncOptionDateValue{"foo", "bar", "a", "Phony Date Option",
                                    GncOptionUIType::DATE_BOTH, c_begin_dates}} {}
    GncOption m_option;

    static const RelativeDatePeriodVec c_begin_dates;
};

const RelativeDatePeriodVec GncOptionDateOptionListTest::c_begin_dates
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

using GncDateOption = GncOptionDateOptionTest;
using GncDateOptionList = GncOptionDateOptionListTest;

TEST_F(GncDateOption, test_set_and_get_absolute)
{
    time64 time1{static_cast<time64>(GncDateTime("2019-07-19 15:32:26 +05:00"))};
    m_option.set_value(time1);
    EXPECT_EQ(time1, m_option.get_value<time64>());
}

TEST_F(GncDateOptionList, test_set_and_get_relative)
{
    GDate date;
    g_date_set_time_t(&date, time(nullptr));
    gnc_gdate_set_month_start(&date);
    time64 time1{time64_from_gdate(&date, DayPart::start)};
    EXPECT_EQ(RelativeDatePeriod::START_ACCOUNTING_PERIOD, m_option.get_value<RelativeDatePeriod>());
    m_option.set_value(RelativeDatePeriod::START_THIS_MONTH);
    EXPECT_EQ(time1, m_option.get_value<time64>());
    EXPECT_EQ(RelativeDatePeriod::START_THIS_MONTH, m_option.get_value<RelativeDatePeriod>());
    uint16_t index(std::find(c_begin_dates.begin(), c_begin_dates.end(),
                         RelativeDatePeriod::START_THIS_MONTH) - c_begin_dates.begin());
    EXPECT_EQ(index, m_option.get_value<uint16_t>());
    // And check that nothing happens when we try to set m_option to an end date
    m_option.set_value(RelativeDatePeriod::END_THIS_MONTH);
    EXPECT_EQ(RelativeDatePeriod::START_THIS_MONTH, m_option.get_value<RelativeDatePeriod>());
    m_option.set_value(static_cast<uint16_t>(5));
    EXPECT_EQ(RelativeDatePeriod::START_CAL_YEAR, m_option.get_value<RelativeDatePeriod>());
    EXPECT_EQ(5u, m_option.get_value<uint16_t>());
}

TEST_F(GncDateOption, test_stream_out)
{
    time64 time1{static_cast<time64>(GncDateTime("2019-07-19 15:32:26 +05:00"))};
    m_option.set_value(time1);
    std::ostringstream oss;
    oss << time1;
    std::string timestr{"(absolute . "};
    timestr += oss.str() + ")";
    oss.str("");
    oss << m_option;
    EXPECT_EQ(oss.str(), timestr);

    m_option.set_value(RelativeDatePeriod::TODAY);
    oss.str("");
    oss << m_option;
    EXPECT_STREQ(oss.str().c_str(), "(relative . today)");

    m_option.set_value(RelativeDatePeriod::START_THIS_MONTH);
    oss.str("");
    oss << m_option;
    EXPECT_STREQ(oss.str().c_str(), "(relative . start-this-month)");

    m_option.set_value(RelativeDatePeriod::END_THIS_MONTH);
    oss.str("");
    oss << m_option;
    EXPECT_STREQ(oss.str().c_str(), "(relative . end-this-month)");

    m_option.set_value(RelativeDatePeriod::START_PREV_MONTH);
    oss.str("");
    oss << m_option;
    EXPECT_STREQ(oss.str().c_str(), "(relative . start-prev-month)");

    m_option.set_value(RelativeDatePeriod::END_PREV_MONTH);
    oss.str("");
    oss << m_option;
    EXPECT_STREQ(oss.str().c_str(), "(relative . end-prev-month)");

    m_option.set_value(RelativeDatePeriod::START_CURRENT_QUARTER);
    oss.str("");
    oss << m_option;
    EXPECT_STREQ(oss.str().c_str(), "(relative . start-current-quarter)");

    m_option.set_value(RelativeDatePeriod::END_CURRENT_QUARTER);
    oss.str("");
    oss << m_option;
    EXPECT_STREQ(oss.str().c_str(), "(relative . end-current-quarter)");

    m_option.set_value(RelativeDatePeriod::START_PREV_QUARTER);
    oss.str("");
    oss << m_option;
    EXPECT_STREQ(oss.str().c_str(), "(relative . start-prev-quarter)");

    m_option.set_value(RelativeDatePeriod::END_PREV_QUARTER);
    oss.str("");
    oss << m_option;
    EXPECT_STREQ(oss.str().c_str(), "(relative . end-prev-quarter)");

    m_option.set_value(RelativeDatePeriod::START_CAL_YEAR);
    oss.str("");
    oss << m_option;
    EXPECT_STREQ(oss.str().c_str(), "(relative . start-cal-year)");

    m_option.set_value(RelativeDatePeriod::END_CAL_YEAR);
    oss.str("");
    oss << m_option;
    EXPECT_STREQ(oss.str().c_str(), "(relative . end-cal-year)");

    m_option.set_value(RelativeDatePeriod::START_PREV_YEAR);
    oss.str("");
    oss << m_option;
    EXPECT_STREQ(oss.str().c_str(), "(relative . start-prev-year)");

    m_option.set_value(RelativeDatePeriod::END_PREV_YEAR);
    oss.str("");
    oss << m_option;
    EXPECT_STREQ(oss.str().c_str(), "(relative . end-prev-year)");

    m_option.set_value(RelativeDatePeriod::START_ACCOUNTING_PERIOD);
    oss.str("");
    oss << m_option;
    EXPECT_STREQ(oss.str().c_str(), "(relative . start-prev-fin-year)");

    m_option.set_value(RelativeDatePeriod::END_ACCOUNTING_PERIOD);
    oss.str("");
    oss << m_option;
    EXPECT_STREQ(oss.str().c_str(), "(relative . end-prev-fin-year)");
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
    EXPECT_EQ(time1, m_option.get_value<time64>());
}

TEST_F(GncDateOption, test_stream_in_month_start)
{
    GDate month_start;
    g_date_set_time_t(&month_start, time(nullptr));
    gnc_gdate_set_month_start(&month_start);
    time64 time1{time64_from_gdate(&month_start, DayPart::start)};
    std::istringstream iss{"relative . start-this-month"};
    iss >> m_option;
    EXPECT_EQ(time1, m_option.get_value<time64>());
}


TEST_F(GncDateOption, test_stream_in_month_end)
{
    GDate date;
    g_date_set_time_t(&date, time(nullptr));
    gnc_gdate_set_month_end(&date);
    time64 time1{time64_from_gdate(&date, DayPart::end)};
    std::istringstream iss{"relative . end-this-month"};
    iss >> m_option;
    EXPECT_EQ(time1, m_option.get_value<time64>());
}

TEST_F(GncDateOption, test_stream_in_prev_month_start)
{
    GDate prev_month_start;
    g_date_set_time_t(&prev_month_start, time(nullptr));
    gnc_gdate_set_prev_month_start(&prev_month_start);
    time64 time1{time64_from_gdate(&prev_month_start, DayPart::start)};
    std::istringstream iss{"relative . start-prev-month"};
    iss >> m_option;
    EXPECT_EQ(time1, m_option.get_value<time64>());
}

TEST_F(GncDateOption, test_stream_in_prev_month_end)
{
    GDate date;
    g_date_set_time_t(&date, time(nullptr));
    gnc_gdate_set_prev_month_end(&date);
    time64 time1{time64_from_gdate(&date, DayPart::end)};
    std::istringstream iss{"relative . end-prev-month"};
    iss >> m_option;
    EXPECT_EQ(time1, m_option.get_value<time64>());
}

TEST_F(GncDateOption, test_stream_in_quarter_start)
{
    GDate date;
    g_date_set_time_t(&date, time(nullptr));
    gnc_gdate_set_quarter_start(&date);
    time64 time1{time64_from_gdate(&date, DayPart::start)};
    std::istringstream iss{"relative . start-current-quarter"};
    iss >> m_option;
    EXPECT_EQ(time1, m_option.get_value<time64>());
}

TEST_F(GncDateOption, test_stream_in_quarter_end)
{
    GDate date;
    g_date_set_time_t(&date, time(nullptr));
    gnc_gdate_set_quarter_end(&date);
    time64 time1{time64_from_gdate(&date, DayPart::end)};
    std::istringstream iss{"relative . end-current-quarter"};
    iss >> m_option;
    EXPECT_EQ(time1, m_option.get_value<time64>());
}

TEST_F(GncDateOption, test_stream_in_prev_quarter_start)
{
    GDate date;
    g_date_set_time_t(&date, time(nullptr));
    gnc_gdate_set_prev_quarter_start(&date);
    time64 time1{time64_from_gdate(&date, DayPart::start)};
    std::istringstream iss{"relative . start-prev-quarter"};
    iss >> m_option;
    EXPECT_EQ(time1, m_option.get_value<time64>());
}

TEST_F(GncDateOption, test_stream_in_prev_quarter_end)
{
    GDate date;
    g_date_set_time_t(&date, time(nullptr));
    gnc_gdate_set_prev_quarter_end(&date);
    time64 time1{time64_from_gdate(&date, DayPart::end)};
    std::istringstream iss{"relative . end-prev-quarter"};
    iss >> m_option;
    EXPECT_EQ(time1, m_option.get_value<time64>());
}

TEST_F(GncDateOption, test_stream_in_year_start)
{
    GDate date;
    g_date_set_time_t(&date, time(nullptr));
    gnc_gdate_set_year_start(&date);
    time64 time1{time64_from_gdate(&date, DayPart::start)};
    std::istringstream iss{"relative . start-cal-year"};
    iss >> m_option;
    EXPECT_EQ(time1, m_option.get_value<time64>());
}

TEST_F(GncDateOption, test_stream_in_year_end)
{
    GDate date;
    g_date_set_time_t(&date, time(nullptr));
    gnc_gdate_set_year_end(&date);
    time64 time1{time64_from_gdate(&date, DayPart::end)};
    std::istringstream iss{"relative . end-cal-year"};
    iss >> m_option;
    EXPECT_EQ(time1, m_option.get_value<time64>());
}

TEST_F(GncDateOption, test_stream_in_prev_year_start)
{
    GDate date;
    g_date_set_time_t(&date, time(nullptr));
    gnc_gdate_set_prev_year_start(&date);
    time64 time1{time64_from_gdate(&date, DayPart::start)};
    std::istringstream iss{"relative . start-prev-year"};
    iss >> m_option;
    EXPECT_EQ(time1, m_option.get_value<time64>());
}

TEST_F(GncDateOption, test_stream_in_prev_year_end)
{
    GDate date;
    g_date_set_time_t(&date, time(nullptr));
    gnc_gdate_set_prev_year_end(&date);
    time64 time1{time64_from_gdate(&date, DayPart::end)};
    std::istringstream iss{"relative . end-prev-year"};
    iss >> m_option;
    EXPECT_EQ(time1, m_option.get_value<time64>());
}

TEST(GncOption, test_create)
{
    uint32_t report_id = 123;
    uint32_t wide = 2, high = 2;
    GncOptionReportPlacementVec rp{{report_id, wide, high}};

    GncOptionValue<GncOptionReportPlacementVec> rpv("foo", "bar", "baz", "Phony Option", rp);
    GncOption option{rpv};
    auto value{option.get_value<GncOptionReportPlacementVec>()};
    EXPECT_EQ(value.size(), 1u);
    auto [sid, swide, shigh] = value.at(0);
    EXPECT_EQ(report_id, sid);
    EXPECT_EQ(wide, swide);
    EXPECT_EQ(high, shigh);

}
