/********************************************************************\
 * test-gnc-price-quotes.cpp -- Unit tests for GncQuotes            *
 *                                                                  *
 * Copyright 2022 John Ralls <jralls@ceridwen.us>                   *
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

extern "C"
{
#include <config.h>
#include <gnc-session.h>
#include <gnc-commodity.h>
#include <gnc-pricedb-p.h>
#include <qof.h>

/* gnc-quotes normally gets this from gnc-ui-util, but let's avoid the dependency. */
static gnc_commodity*
gnc_default_currency(void)
{
    auto book{qof_session_get_book(gnc_get_current_session())};
    auto table{gnc_commodity_table_get_table(book)};
    return gnc_commodity_table_lookup(table, GNC_COMMODITY_NS_CURRENCY, "USD");
}

} // extern "C"
#include <gtest/gtest.h>
#include "../gnc-quotes.cpp"

class GncQuotesTest : public ::testing::Test
{
protected:
    GncQuotesTest() : m_session{gnc_get_current_session()},
    m_book{qof_session_get_book(gnc_get_current_session())}
    {
        qof_init();
        gnc_commodity_table_register();
        gnc_pricedb_register();
        auto comm_table{gnc_commodity_table_new()};
        qof_book_set_data(m_book, GNC_COMMODITY_TABLE, comm_table);
        auto eur = gnc_commodity_new(m_book, "Euro", "ISO4217", "EUR", NULL, 100);
        auto source{gnc_quote_source_lookup_by_internal("currency")};
        gnc_commodity_begin_edit(eur);
        gnc_commodity_set_quote_flag(eur, TRUE);
        gnc_commodity_set_quote_source(eur, source);
        gnc_commodity_commit_edit(eur);
        gnc_commodity_table_insert(comm_table, eur);
        auto usd = gnc_commodity_new(m_book, "United States Dollar", "CURRENCY",
                                  "USD", NULL, 100);
        gnc_commodity_table_insert(comm_table, usd);
        source = gnc_quote_source_lookup_by_internal("yahoo_json");
        auto aapl = gnc_commodity_new(m_book, "Apple", "NASDAQ", "AAPL", NULL, 1);
        gnc_commodity_begin_edit(aapl);
        gnc_commodity_set_quote_flag(aapl, TRUE);
        gnc_commodity_set_quote_source(aapl, source);
        gnc_commodity_commit_edit(aapl);
        gnc_commodity_table_insert(comm_table, aapl);
        auto hpe = gnc_commodity_new(m_book, "Hewlett Packard", "NYSE", "HPE",
                                  NULL, 1);
        gnc_commodity_begin_edit(hpe);
        gnc_commodity_set_quote_flag(hpe, TRUE);
        gnc_commodity_set_quote_source(hpe, source);
        gnc_commodity_commit_edit(hpe);
        gnc_commodity_table_insert(comm_table, hpe);
        auto fkcm = gnc_commodity_new(m_book, "Fake Company", "NASDAQ", "FKCM", NULL, 1);
        gnc_commodity_begin_edit(fkcm);
        gnc_commodity_set_quote_flag(fkcm, TRUE);
        gnc_commodity_set_quote_source(fkcm, source);
        gnc_commodity_commit_edit(fkcm);
        gnc_commodity_table_insert(comm_table, fkcm);
        gnc_quote_source_set_fq_installed("TestSuite", g_list_prepend(nullptr, (void*)"yahoo_json"));
    }
    ~GncQuotesTest() {
        gnc_clear_current_session();
    }

    QofSession* m_session;
    QofBook* m_book;
};

TEST_F(GncQuotesTest, quote_sources)
{
    auto qs_cur{gnc_quote_source_lookup_by_internal("currency")};
    auto qs_yahoo{gnc_quote_source_lookup_by_internal("yahoo_json")};
    auto qs_alpha{gnc_quote_source_lookup_by_internal("alphavantage")};
    EXPECT_TRUE(qs_cur != nullptr);
    EXPECT_TRUE(qs_yahoo != nullptr);
    EXPECT_TRUE(qs_alpha != nullptr);
    EXPECT_TRUE(gnc_quote_source_get_supported(qs_cur));
    EXPECT_TRUE(gnc_quote_source_get_supported(qs_yahoo));
    EXPECT_FALSE(gnc_quote_source_get_supported(qs_alpha));
}

TEST_F(GncQuotesTest, quotable_commodities)
{
    auto commodities{gnc_quotes_get_quotable_commodities(gnc_commodity_table_get_table(m_book))};
    EXPECT_EQ(4u, commodities.size());
}
TEST_F(GncQuotesTest, wiggle)
{
    GncQuotes quotes;
    quotes.fetch(m_book);
    auto pricedb{gnc_pricedb_get_db(m_book)};
    EXPECT_EQ(3u, gnc_pricedb_get_num_prices(pricedb));
}
