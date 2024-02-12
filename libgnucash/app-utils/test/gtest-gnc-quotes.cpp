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

#include <config.h>
#include <gnc-session.h>
#include <gnc-commodity.h>
#include <gnc-pricedb-p.h>
#include <qof.h>

/* gnc-quotes normally gets this from gnc-ui-util, but let's avoid the dependency. */
extern "C" {
static gnc_commodity*
gnc_default_currency(void)
{
    auto book{qof_session_get_book(gnc_get_current_session())};
    auto table{gnc_commodity_table_get_table(book)};
    return gnc_commodity_table_lookup(table, GNC_COMMODITY_NS_CURRENCY, "USD");
}
}

#include <gtest/gtest.h>
#include "../gnc-quotes.cpp"

class GncMockQuoteSource final : public GncQuoteSource
{
    const std::string m_version{"9.99"};
    const StrVec m_sources{"currency", "alphavantage"};
    const StrVec m_quotes;
    const StrVec m_errors;
public:
    GncMockQuoteSource(StrVec&& quotes, StrVec&& errors) :
        m_quotes{std::move(quotes)}, m_errors{std::move(errors)}{}
    ~GncMockQuoteSource() override = default;
    const std::string& get_version() const noexcept override { return m_version; }
    const StrVec& get_sources() const noexcept override { return m_sources; }
    QuoteResult get_quotes(const std::string&) const override;
};

class GncFailedQuoteSource final : public GncQuoteSource
{

    const std::string m_version{"0"};
    const StrVec m_sources;
public:
    GncFailedQuoteSource()
        {
            std::string err{"Failed to initialize Finance::Quote: "};
            err += "missing_modules Mozilla::CA Try::Tiny";
            throw GncQuoteSourceError (err);
        }
    ~GncFailedQuoteSource() override = default;
    const std::string& get_version() const noexcept override { return m_version; }
    const StrVec& get_sources() const noexcept override { return m_sources; }
    QuoteResult get_quotes(const std::string&) const override {return {0, {}, {}}; }
};

QuoteResult
GncMockQuoteSource::get_quotes(const std::string& json_string) const
{
    if (m_errors.empty())
        return {0, m_quotes, m_errors};
    return {1, m_quotes, m_errors};
}

class GncQuotesTest : public ::testing::Test
{
protected:
    GncQuotesTest() : m_session{gnc_get_current_session()},
    m_book{qof_session_get_book(gnc_get_current_session())}
    {
        qof_init();

        /* By setting an empty commodity table on the book before registering
         * the commodity_table type we avoid adding the default commodities */
        auto comm_table{gnc_commodity_table_new()};
        qof_book_set_data(m_book, GNC_COMMODITY_TABLE, comm_table);

        gnc_commodity_table_register();
        gnc_pricedb_register();

        auto eur = gnc_commodity_new(m_book, "Euro", "ISO4217", "EUR", NULL, 100);
        auto source{gnc_quote_source_lookup_by_internal("currency")};
        gnc_commodity_begin_edit(eur);
        gnc_commodity_set_quote_flag(eur, TRUE);
        gnc_commodity_set_quote_source(eur, source);
        gnc_commodity_commit_edit(eur);
        gnc_commodity_table_insert(comm_table, eur);
        auto usd = gnc_commodity_new(m_book, "United States Dollar", "CURRENCY", "USD", NULL, 100);
        gnc_commodity_table_insert(comm_table, usd);
        source = gnc_quote_source_lookup_by_internal("alphavantage");
        auto aapl = gnc_commodity_new(m_book, "Apple", "NASDAQ", "AAPL", NULL, 1);
        gnc_commodity_begin_edit(aapl);
        gnc_commodity_set_quote_flag(aapl, TRUE);
        gnc_commodity_set_quote_source(aapl, source);
        gnc_commodity_commit_edit(aapl);
        gnc_commodity_table_insert(comm_table, aapl);
        auto hpe = gnc_commodity_new(m_book, "Hewlett Packard", "NYSE", "HPE", NULL, 1);
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
        std::vector<std::string> sources = {"alphavantage"};
        gnc_quote_source_set_fq_installed("TestSuite", sources);
    }
    ~GncQuotesTest() {
        gnc_clear_current_session();
        qof_close();
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
    EXPECT_FALSE(gnc_quote_source_get_supported(qs_yahoo));
    EXPECT_TRUE(gnc_quote_source_get_supported(qs_alpha));
}

TEST_F(GncQuotesTest, quotable_commodities)
{
    auto commodities{gnc_quotes_get_quotable_commodities(gnc_commodity_table_get_table(m_book))};
    EXPECT_EQ(4u, commodities.size());
}

#ifdef HAVE_F_Q
TEST_F(GncQuotesTest, online_wiggle)
{
    GncQuotes quotes;
    quotes.fetch(m_book);
    auto pricedb{gnc_pricedb_get_db(m_book)};
    auto failures{quotes.failures()};
    ASSERT_EQ(1u, failures.size());
    EXPECT_EQ(GncQuoteError::QUOTE_FAILED, std::get<2>(failures[0]));
//    EXPECT_EQ(GncQuoteError::QUOTE_FAILED, std::get<2>(failures[1]));
    EXPECT_EQ(3u, gnc_pricedb_get_num_prices(pricedb));
}
#else
TEST_F(GncQuotesTest, fq_failure)
{
    EXPECT_THROW(GncQuotes quotes;, GncQuoteException);
}
#endif

TEST_F(GncQuotesTest, offline_wiggle)
{
    StrVec quote_vec{
        "{"
        "\"EUR\":{\"symbol\":\"EUR\",\"currency\":\"USD\",\"success\":\"1\",\"inverted\":0,\"last\":1.0004},"
        "\"AAPL\":{\"eps\":6.05,\"success\":1,\"year_range\":\"      129.04 - 182.94\",\"currency\":\"USD\",\"exchange\":\"Sourced from Alphavantage\",\"volume\":73539475,\"close\":157.22,\"high\":158.39,\"open\":156.64,\"div_yield\":0.5660857,\"last\":157.96,\"isodate\":\"2022-09-01\",\"method\":\"alphavantage\",\"name\":\"AAPL (Apple Inc.)\",\"pe\":26.10909,\"low\":154.67,\"type\":\"EQUITY\",\"symbol\":\"AAPL\",\"date\":\"09/01/2022\"},"
        "\"HPE\":{\"symbol\":\"HPE\",\"date\":\"09/01/2022\",\"low\":13.13,\"type\":\"EQUITY\",\"method\":\"alphavantage\",\"name\":\"HPE (Hewlett Packard Enterprise Comp)\",\"isodate\":\"2022-09-01\",\"pe\":4.7921147,\"last\":13.37,\"high\":13.535,\"close\":13.6,\"open\":13.5,\"div_yield\":3.5294116,\"volume\":16370483,\"exchange\":\"Sourced from Alphavantage\",\"currency\":\"USD\",\"year_range\":\"        12.4 - 17.76\",\"eps\":2.79,\"success\":1},"
        "\"FKCM\":{\"success\":0,\"symbol\":\"FKCM\",\"errormsg\":\"Error retrieving quote for FKCM - no listing for this name found. Please check symbol and the two letter extension (if any)\"}"
        "}"
    };
    StrVec err_vec;
    GncQuotesImpl quotes(m_book, std::make_unique<GncMockQuoteSource>(std::move(quote_vec), std::move(err_vec)));
    quotes.fetch(m_book);
    auto failures{quotes.failures()};
    ASSERT_EQ(1u, failures.size());
    EXPECT_EQ(GncQuoteError::QUOTE_FAILED, std::get<2>(failures[0]));
    auto pricedb{gnc_pricedb_get_db(m_book)};
    EXPECT_EQ(3u, gnc_pricedb_get_num_prices(pricedb));
}

TEST_F(GncQuotesTest, offline_report)
{
    StrVec quote_vec{
        "{"
        "\"AAPL\":{\"eps\":6.05,\"success\":1,\"year_range\":\"      129.04 - 182.94\",\"currency\":\"USD\",\"exchange\":\"Sourced from Alphavantage\",\"volume\":73539475,\"close\":157.22,\"high\":158.39,\"open\":156.64,\"div_yield\":0.5660857,\"last\":157.96,\"isodate\":\"2022-09-01\",\"method\":\"alphavantage\",\"name\":\"AAPL (Apple Inc.)\",\"pe\":26.10909,\"low\":154.67,\"type\":\"EQUITY\",\"symbol\":\"AAPL\",\"date\":\"09/01/2022\"},"
        "\"HPE\":{\"symbol\":\"HPE\",\"date\":\"09/01/2022\",\"low\":13.13,\"type\":\"EQUITY\",\"method\":\"alphavantage\",\"name\":\"HPE (Hewlett Packard Enterprise Comp)\",\"isodate\":\"2022-09-01\",\"pe\":4.7921147,\"last\":13.37,\"high\":13.535,\"close\":13.6,\"open\":13.5,\"div_yield\":3.5294116,\"volume\":16370483,\"exchange\":\"Sourced from Alphavantage\",\"currency\":\"USD\",\"year_range\":\"        12.4 - 17.76\",\"eps\":2.79,\"success\":1},"
        "\"FKCM\":{\"success\":0,\"symbol\":\"FKCM\",\"errormsg\":\"Error retrieving quote for FKCM - no listing for this name found. Please check symbol and the two letter extension (if any)\"}"
        "}"
    };
    StrVec commodities{"AAPL", "HPE", "FKCM"};
    StrVec err_vec;
    GncQuotesImpl quotes(m_book, std::make_unique<GncMockQuoteSource>(std::move(quote_vec), std::move(err_vec)));
    quotes.report("alphavantage", commodities, false);
    quotes.report("alphavantage", commodities, true);
}

TEST_F(GncQuotesTest, offline_currency_report)
{
    StrVec quote_vec{
        "{"
        "\"EUR\":{\"symbol\":\"EUR\",\"currency\":\"USD\",\"success\":\"1\",\"inverted\":0,\"last\":1.0004}"
        "}"
    };
    StrVec commodities{"USD", "EUR"};
    StrVec err_vec;
    GncQuotesImpl quotes(m_book, std::make_unique<GncMockQuoteSource>(std::move(quote_vec), std::move(err_vec)));
    quotes.report("currency", commodities, false);
    quotes.report("currency", commodities, true);
}

TEST_F(GncQuotesTest, comvec_fetch)
{
     StrVec quote_vec{
        "{"
        "\"AAPL\":{\"eps\":6.05,\"success\":1,\"year_range\":\"      129.04 - 182.94\",\"currency\":\"USD\",\"exchange\":\"Sourced from Alphavantage\",\"volume\":73539475,\"close\":157.22,\"high\":158.39,\"open\":156.64,\"div_yield\":0.5660857,\"last\":157.96,\"isodate\":\"2022-09-01\",\"method\":\"alphavantage\",\"name\":\"AAPL (Apple Inc.)\",\"pe\":26.10909,\"low\":154.67,\"type\":\"EQUITY\",\"symbol\":\"AAPL\",\"date\":\"09/01/2022\"},"
        "\"HPE\":{\"symbol\":\"HPE\",\"date\":\"09/01/2022\",\"low\":13.13,\"type\":\"EQUITY\",\"method\":\"alphavantage\",\"name\":\"HPE (Hewlett Packard Enterprise Comp)\",\"isodate\":\"2022-09-01\",\"pe\":4.7921147,\"last\":13.37,\"high\":13.535,\"close\":13.6,\"open\":13.5,\"div_yield\":3.5294116,\"volume\":16370483,\"exchange\":\"Sourced from Alphavantage\",\"currency\":\"USD\",\"year_range\":\"        12.4 - 17.76\",\"eps\":2.79,\"success\":1}"
        "}"
    };
    StrVec err_vec;
    auto commtable{gnc_commodity_table_get_table(m_book)};
    auto hpe{gnc_commodity_table_lookup(commtable, "NYSE", "HPE")};
    auto aapl{gnc_commodity_table_lookup(commtable, "NASDAQ", "AAPL")};
    CommVec comms{hpe, aapl};
    GncQuotesImpl quotes(m_book, std::make_unique<GncMockQuoteSource>(std::move(quote_vec), std::move(err_vec)));
    quotes.fetch(comms);
    auto failures{quotes.failures()};
    EXPECT_TRUE(failures.empty());
    auto pricedb{gnc_pricedb_get_db(m_book)};
    EXPECT_EQ(2u, gnc_pricedb_get_num_prices(pricedb));
}

TEST_F(GncQuotesTest, fetch_one_commodity)
{
     StrVec quote_vec{
        "{"
        "\"HPE\":{\"date\":\"09/01/2022\",\"last\":13.37,\"currency\":\"USD\",\"success\":1}"
        "}"
    };
    StrVec err_vec;
    auto commtable{gnc_commodity_table_get_table(m_book)};
    auto hpe{gnc_commodity_table_lookup(commtable, "NYSE", "HPE")};
    auto usd{gnc_commodity_table_lookup(commtable, "ISO4217", "USD")};
    GncQuotesImpl quotes(m_book, std::make_unique<GncMockQuoteSource>(std::move(quote_vec), std::move(err_vec)));
    quotes.fetch(hpe);
    auto failures{quotes.failures()};
    EXPECT_TRUE(failures.empty());
    auto pricedb{gnc_pricedb_get_db(m_book)};
    auto price{gnc_pricedb_lookup_latest(pricedb, hpe, usd)};
    auto datetime{static_cast<time64>(GncDateTime("20220901105900"))};

    EXPECT_EQ(usd, gnc_price_get_currency(price));
    EXPECT_EQ(datetime, gnc_price_get_time64(price));
    EXPECT_EQ(PRICE_SOURCE_FQ, gnc_price_get_source(price));
    EXPECT_TRUE(gnc_numeric_equal(GncNumeric{1337, 100},
                                  gnc_price_get_value(price)));
    EXPECT_STREQ("Finance::Quote", gnc_price_get_source_string(price));
    EXPECT_STREQ("last", gnc_price_get_typestr(price));
    gnc_price_unref(price);
}

TEST_F(GncQuotesTest, fetch_one_currency)
{
     StrVec quote_vec{
         "{"
         "\"EUR\":{\"symbol\":\"EUR\",\"currency\":\"USD\",\"success\":\"1\",\"inverted\":0,\"last\":1.0004}"
         "}"
    };
    StrVec err_vec;
    auto commtable{gnc_commodity_table_get_table(m_book)};
    auto eur{gnc_commodity_table_lookup(commtable, "ISO4217", "EUR")};
    auto usd{gnc_commodity_table_lookup(commtable, "ISO4217", "USD")};
    GncQuotesImpl quotes(m_book, std::make_unique<GncMockQuoteSource>(std::move(quote_vec), std::move(err_vec)));
    quotes.fetch(eur);
    auto failures{quotes.failures()};
    EXPECT_TRUE(failures.empty());
    auto pricedb{gnc_pricedb_get_db(m_book)};
    auto price{gnc_pricedb_lookup_latest(pricedb, eur, usd)};
    EXPECT_EQ(1u, gnc_pricedb_get_num_prices(pricedb));
    auto datetime{static_cast<time64>(GncDateTime())};

    EXPECT_EQ(usd, gnc_price_get_currency(price));
    EXPECT_EQ(datetime, gnc_price_get_time64(price));
    EXPECT_EQ(PRICE_SOURCE_FQ, gnc_price_get_source(price));
    EXPECT_EQ(10004, gnc_price_get_value(price).num);
    EXPECT_TRUE(gnc_numeric_equal(GncNumeric{10004, 10000},
                                  gnc_price_get_value(price)));
    EXPECT_STREQ("Finance::Quote", gnc_price_get_source_string(price));
    EXPECT_STREQ("last", gnc_price_get_typestr(price));
    gnc_price_unref(price);
}

TEST_F(GncQuotesTest, no_currency)
{
     StrVec quote_vec{
        "{"
        "\"HPE\":{\"date\":\"09/01/2022\",\"last\":13.37,\"success\":1}"
        "}"
    };
    StrVec err_vec;
    auto commtable{gnc_commodity_table_get_table(m_book)};
    auto hpe{gnc_commodity_table_lookup(commtable, "NYSE", "HPE")};
    GncQuotesImpl quotes(m_book, std::make_unique<GncMockQuoteSource>(std::move(quote_vec), std::move(err_vec)));
    quotes.fetch(hpe);
    auto failures{quotes.failures()};
    ASSERT_EQ(1u, failures.size());
    EXPECT_EQ(GncQuoteError::NO_CURRENCY, std::get<2>(failures[0]));
    auto pricedb{gnc_pricedb_get_db(m_book)};
    EXPECT_EQ(0u, gnc_pricedb_get_num_prices(pricedb));
}

TEST_F(GncQuotesTest, bad_currency)
{
     StrVec quote_vec{
        "{"
        "\"HPE\":{\"date\":\"09/01/2022\",\"last\":13.37,\"currency\":\"BTC\",\"success\":1}"
        "}"
     };
    StrVec err_vec;
    auto commtable{gnc_commodity_table_get_table(m_book)};
    auto hpe{gnc_commodity_table_lookup(commtable, "NYSE", "HPE")};
    GncQuotesImpl quotes(m_book, std::make_unique<GncMockQuoteSource>(std::move(quote_vec), std::move(err_vec)));
    quotes.fetch(hpe);
    auto failures{quotes.failures()};
    ASSERT_EQ(1u, failures.size());
    EXPECT_EQ(GncQuoteError::UNKNOWN_CURRENCY, std::get<2>(failures[0]));
    auto pricedb{gnc_pricedb_get_db(m_book)};
    EXPECT_EQ(0u, gnc_pricedb_get_num_prices(pricedb));
}

TEST_F(GncQuotesTest, no_date)
{
     StrVec quote_vec{
        "{"
        "\"HPE\":{\"last\":13.37,\"currency\":\"USD\",\"success\":1}"
        "}"
    };
    StrVec err_vec;
    auto commtable{gnc_commodity_table_get_table(m_book)};
    auto hpe{gnc_commodity_table_lookup(commtable, "NYSE", "HPE")};
    auto usd{gnc_commodity_table_lookup(commtable, "ISO4217", "USD")};
    GncQuotesImpl quotes(m_book, std::make_unique<GncMockQuoteSource>(std::move(quote_vec), std::move(err_vec)));
    quotes.fetch(hpe);
    auto failures{quotes.failures()};
    EXPECT_TRUE(failures.empty());
    auto pricedb{gnc_pricedb_get_db(m_book)};
    auto price{gnc_pricedb_lookup_latest(pricedb, hpe, usd)};
    auto datetime{static_cast<time64>(GncDateTime())};

    EXPECT_EQ(usd, gnc_price_get_currency(price));
    EXPECT_EQ(datetime, gnc_price_get_time64(price));
    EXPECT_EQ(PRICE_SOURCE_FQ, gnc_price_get_source(price));
    EXPECT_TRUE(gnc_numeric_equal(GncNumeric{1337, 100},
                                  gnc_price_get_value(price)));
    EXPECT_STREQ("Finance::Quote", gnc_price_get_source_string(price));
    EXPECT_STREQ("last", gnc_price_get_typestr(price));
    gnc_price_unref(price);
}

TEST_F(GncQuotesTest, test_version)
{
    StrVec quote_vec, err_vec;
    GncQuotesImpl quotes(m_book, std::make_unique<GncMockQuoteSource>(std::move(quote_vec), std::move(err_vec)));
    EXPECT_STREQ("9.99", quotes.version().c_str());
}

TEST_F(GncQuotesTest, test_failure_invalid_json)
{
    StrVec quote_vec, err_vec{"invalid_json\n"};
    GncQuotesImpl quotes(m_book, std::make_unique<GncMockQuoteSource>(std::move(quote_vec), std::move(err_vec)));
    EXPECT_THROW(quotes.fetch(m_book), GncQuoteException);
    try
    {
        quotes.fetch(m_book);
    }
    catch (const GncQuoteException& err)
    {
        EXPECT_STREQ("GnuCash submitted invalid json to Finance::Quote. The details were logged.\n",
                     err.what());
    }

}

TEST_F(GncQuotesTest, test_failure_missing_modules)
{
    EXPECT_THROW(GncQuotesImpl quotes(m_book, std::make_unique<GncFailedQuoteSource>()),
                 GncQuoteSourceError);
    try
    {
        GncQuotesImpl quotes(m_book, std::make_unique<GncFailedQuoteSource>());
    }
    catch (const GncQuoteSourceError& err)
    {
        EXPECT_STREQ("Failed to initialize Finance::Quote: missing_modules Mozilla::CA Try::Tiny",
                     err.what());
    }

}
