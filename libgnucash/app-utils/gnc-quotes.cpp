/********************************************************************\
 * gnc-quotes.hpp -- proxy for Finance::Quote                       *
 * Copyright (C) 2021 Geert Janssens <geert@kobaltwit.be>           *
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
\ *******************************************************************/

#include <config.h>
#include <qoflog.h>

#include <algorithm>
#include <stdexcept>
#include <vector>
#include <string>
#include <iostream>
#include <boost/version.hpp>
#if BOOST_VERSION < 107600
// json_parser uses a deprecated version of bind.hpp
#define BOOST_BIND_GLOBAL_PLACEHOLDERS
#endif
#include <boost/algorithm/string.hpp>
#include <boost/filesystem.hpp>
#ifdef BOOST_WINDOWS_API
#include <boost/process/windows.hpp>
#endif
#include <boost/process.hpp>
#include <boost/regex.hpp>
#include <boost/property_tree/ptree.hpp>
#include <boost/property_tree/json_parser.hpp>
#include <boost/iostreams/device/array.hpp>
#include <boost/iostreams/stream_buffer.hpp>
#include <boost/locale.hpp>
#include <boost/asio.hpp>
#include <glib.h>
#include "gnc-commodity.hpp"
#include <gnc-datetime.hpp>
#include <gnc-numeric.hpp>
#include "gnc-quotes.hpp"

#include <gnc-commodity.h>
#include <gnc-path.h>
#include "gnc-ui-util.h"
#include <gnc-prefs.h>
#include <gnc-session.h>
#include <regex.h>
#include <qofbook.h>

static const QofLogModule log_module = "gnc.price-quotes";

namespace bl = boost::locale;
namespace bp = boost::process;
namespace bfs = boost::filesystem;
namespace bpt = boost::property_tree;
namespace bio = boost::iostreams;

using QuoteResult = std::tuple<int, StrVec, StrVec>;

struct GncQuoteSourceError : public std::runtime_error
{
    GncQuoteSourceError(const std::string& err) : std::runtime_error(err) {}
};

CommVec
gnc_quotes_get_quotable_commodities(const gnc_commodity_table * table);

class GncQuoteSource
{
public:
    virtual ~GncQuoteSource() = default;
    virtual const StrVec& get_sources() const noexcept = 0;
    virtual const std::string & get_version() const noexcept = 0;
    virtual QuoteResult get_quotes(const std::string& json_str) const = 0;
};


class GncQuotesImpl
{
public:
    // Constructor - checks for presence of Finance::Quote and import version and quote sources
    GncQuotesImpl ();
    explicit GncQuotesImpl (QofBook *book);
    GncQuotesImpl(QofBook*, std::unique_ptr<GncQuoteSource>);

    void fetch (QofBook *book);
    void fetch (CommVec& commodities);
    void fetch (gnc_commodity *comm);
    void report (const char* source, const StrVec& commodities, bool verbose);

    const std::string& version() noexcept { return m_quotesource->get_version(); }
    const QuoteSources& sources() noexcept { return m_sources; }
    bool had_failures() noexcept { return !m_failures.empty(); }
    const QFVec& failures() noexcept;
    std::string report_failures() noexcept;

private:
    std::string query_fq (const char* source, const StrVec& commoditites);
    std::string query_fq (const CommVec&);
    bpt::ptree parse_quotes (const std::string& quote_str);
    void create_quotes(const bpt::ptree& pt, const CommVec& comm_vec);
    std::string comm_vec_to_json_string(const CommVec&) const;
    GNCPrice* parse_one_quote(const bpt::ptree&, gnc_commodity*);

    std::unique_ptr<GncQuoteSource> m_quotesource;
    QuoteSources m_sources;
    QFVec m_failures;
    QofBook *m_book;
    gnc_commodity *m_dflt_curr;
};

class GncFQQuoteSource final : public GncQuoteSource
{
    const bfs::path c_cmd;
    std::string c_fq_wrapper;
    std::string m_version;
    StrVec m_sources;
    std::string m_api_key;
public:
    GncFQQuoteSource();
    ~GncFQQuoteSource() = default;
    const std::string& get_version() const noexcept override { return m_version; }
    const StrVec& get_sources() const noexcept override { return m_sources; }
    QuoteResult get_quotes(const std::string&) const override;
private:
    QuoteResult run_cmd (const StrVec& args, const std::string& json_string) const;

};

static void show_quotes(const bpt::ptree& pt, const StrVec& commodities, bool verbose);
static void show_currency_quotes(const bpt::ptree& pt, const StrVec& commodities, bool verbose);
static std::string parse_quotesource_error(const std::string& line);

static const std::string empty_string{};

GncFQQuoteSource::GncFQQuoteSource() :
c_cmd{bp::search_path("perl")},
m_version{}, m_sources{}, m_api_key{}
{
    char *bindir = gnc_path_get_bindir();
    c_fq_wrapper = std::string(bindir) + "/finance-quote-wrapper";
    g_free(bindir);
    StrVec args{"-w", c_fq_wrapper, "-v"};
    auto [rv, sources, errors] = run_cmd(args, empty_string);
    if (rv)
    {
        std::string err{bl::translate("Failed to initialize Finance::Quote: ")};
        for (const auto& err_line : errors)
            err += err_line.empty() ? "" : err_line + "\n";
        throw(GncQuoteSourceError(err));
    }
    if (!errors.empty())
    {
        std::string err{bl::translate("Finance::Quote check returned error ")};
        for(const auto& err_line : errors)
            err += err.empty() ? "" : err_line + "\n";
        throw(GncQuoteSourceError(err));
    }
    auto version{sources.front()};
    if (version.empty())
    {
        std::string err{bl::translate("No Finance::Quote Version")};
        throw(GncQuoteSourceError(err));
    }
    m_version = std::move(version);
    sources.erase(sources.begin());
    m_sources = std::move(sources);
    std::sort (m_sources.begin(), m_sources.end());

    auto av_key = gnc_prefs_get_string ("general.finance-quote", "alphavantage-api-key");
    if (!(av_key && *av_key))
    {
        g_free (av_key);
        av_key = g_strdup(getenv("ALPHAVANTAGE_API_KEY"));
    }

    if (av_key)
    {
        m_api_key = std::string(av_key);
        g_free (av_key);
    }
    else
        PWARN("No Alpha Vantage API key set, currency quotes and other AlphaVantage based quotes won't work.");
}

QuoteResult
GncFQQuoteSource::get_quotes(const std::string& json_str) const
{
    StrVec args{"-w", c_fq_wrapper, "-f" };
    return run_cmd(args, json_str);
}

QuoteResult
GncFQQuoteSource::run_cmd (const StrVec& args, const std::string& json_string) const
{
    StrVec out_vec, err_vec;
    int cmd_result;

    try
    {
        std::future<std::vector<char> > out_buf, err_buf;
        boost::asio::io_service svc;

        auto input_buf = bp::buffer (json_string);
	bp::child process;
	if (m_api_key.empty())
	    process = bp::child(c_cmd, args,
				bp::std_out > out_buf,
				bp::std_err > err_buf,
				bp::std_in < input_buf,
#ifdef BOOST_WINDOWS_API
                                bp::windows::create_no_window,
#endif
				svc);
	else
	    process = bp::child(c_cmd, args,
				bp::std_out > out_buf,
				bp::std_err > err_buf,
				bp::std_in < input_buf,
#ifdef BOOST_WINDOWS_API
                                bp::windows::create_no_window,
#endif
				bp::env["ALPHAVANTAGE_API_KEY"] = m_api_key,
				svc);

	svc.run();
	process.wait();

        {
            auto raw = out_buf.get();
            std::vector<std::string> data;
            std::string line;
            bio::stream_buffer<bio::array_source> sb(raw.data(), raw.size());
            std::istream is(&sb);

            while (std::getline(is, line) && !line.empty())
	    {
#ifdef __WIN32
		if (line.back() == '\r')
		    line.pop_back();
#endif
                out_vec.push_back (std::move(line));
	    }
            raw = err_buf.get();
            bio::stream_buffer<bio::array_source> eb(raw.data(), raw.size());
            std::istream es(&eb);

            while (std::getline(es, line) && !line.empty())
                err_vec.push_back (std::move(line));
        }
        cmd_result = process.exit_code();
    }
    catch (std::exception &e)
    {
        cmd_result = -1;
        err_vec.push_back(e.what());
    };

    return QuoteResult (cmd_result, std::move(out_vec), std::move(err_vec));
}

/* GncQuotes implementation */
GncQuotesImpl::GncQuotesImpl() : m_quotesource{new GncFQQuoteSource},
                                 m_sources{}, m_failures{},
                                 m_book{qof_session_get_book(gnc_get_current_session())},
                                 m_dflt_curr{gnc_default_currency()}
{
    m_sources = m_quotesource->get_sources();
}

GncQuotesImpl::GncQuotesImpl(QofBook* book) : m_quotesource{new GncFQQuoteSource},
m_sources{}, m_book{book},
m_dflt_curr{gnc_default_currency()}
{
    m_sources = m_quotesource->get_sources();
}

GncQuotesImpl::GncQuotesImpl(QofBook* book, std::unique_ptr<GncQuoteSource> quote_source) :
m_quotesource{std::move(quote_source)},
m_sources{}, m_book{book}, m_dflt_curr{gnc_default_currency()}
{
    m_sources = m_quotesource->get_sources();
}

void
GncQuotesImpl::fetch (QofBook *book)
{
    if (!book)
        throw (GncQuoteException(bl::translate("GncQuotes::Fetch called with no book.")));
    auto commodities = gnc_quotes_get_quotable_commodities (
        gnc_commodity_table_get_table (book));
    fetch (commodities);
}

void
GncQuotesImpl::fetch (gnc_commodity *comm)
{
    auto commodities = CommVec {comm};
    fetch (commodities);
}

void
GncQuotesImpl::fetch (CommVec& commodities)
{
    m_failures.clear();
    if (commodities.empty())
        throw (GncQuoteException(bl::translate("GncQuotes::Fetch called with no commodities.")));
    auto quote_str{query_fq (commodities)};
    auto ptree{parse_quotes (quote_str)};
    create_quotes(ptree, commodities);
}

void
GncQuotesImpl::report (const char* source, const StrVec& commodities,
                       bool verbose)
{
    if (!source)
        throw (GncQuoteException(bl::translate("GncQuotes::Report called with no source.")));

    bool is_currency{strcmp(source, "currency") == 0};
    m_failures.clear();
    if (commodities.empty())
    {
        std::cerr << _("There were no commodities for which to retrieve quotes.") << std::endl;
        return;
    }
    try
    {
        auto quote_str{query_fq (source, commodities)};
        auto ptree{parse_quotes (quote_str)};
        if (is_currency)
            show_currency_quotes(ptree, commodities, verbose);
        else
            show_quotes(ptree, commodities, verbose);
    }
    catch (const GncQuoteException& err)
    {
        std::cerr << _("Finance::Quote retrieval failed with error ") << err.what() << std::endl;
    }
}

const QFVec&
GncQuotesImpl::failures() noexcept
{
    return m_failures;
}

static std::string
explain(GncQuoteError err, const std::string& errmsg)
{
    std::string retval;
    switch (err)
    {
    case GncQuoteError::NO_RESULT:
        if (errmsg.empty())
            retval += _("Finance::Quote returned no data and set no error.");
        else
            retval += _("Finance::Quote returned an error: ") + errmsg;
        break;
    case GncQuoteError::QUOTE_FAILED:
        if (errmsg.empty())
            retval += _("Finance::Quote reported failure set no error.");
        else
            retval += _("Finance::Quote reported failure with error: ") + errmsg;
        break;
    case GncQuoteError::NO_CURRENCY:
        retval += _("Finance::Quote returned a quote with no currency.");
        break;
    case GncQuoteError::UNKNOWN_CURRENCY:
        retval += _("Finance::Quote returned a quote with a currency GnuCash doesn't know about.");
        break;
    case GncQuoteError::NO_PRICE:
        retval += _("Finance::Quote returned a quote with no price element.");
        break;
    case GncQuoteError::PRICE_PARSE_FAILURE:
        retval += _("Finance::Quote returned a quote with a price that GnuCash was unable to covert to a number.");
        break;
    case GncQuoteError::SUCCESS:
    default:
        retval += _("The quote has no error set.");
        break;
    }
    return retval;
}

std::string
GncQuotesImpl::report_failures() noexcept
{
    std::string retval{_("Quotes for the following commodities were unavailable or unusable:\n")};
    std::for_each(m_failures.begin(), m_failures.end(),
                  [&retval](auto failure)
                  {
                      auto [ns, sym, reason, err] = failure;
                      retval += "* " + ns + ":" + sym + " " +
                          explain(reason, err) + "\n";
                  });
    return retval;
}

/* **** Private function implementations ****/

using Path = bpt::ptree::path_type;
static inline Path make_quote_path(const std::string &name_space,
                                   const std::string &symbol)
{
  using Path = bpt::ptree::path_type;
  Path key{name_space, '|'};
  key /= Path{symbol, '|'};
  return key;
};

std::string
GncQuotesImpl::comm_vec_to_json_string(const CommVec &comm_vec) const
{
    bpt::ptree pt, pt_child;
    pt.put("defaultcurrency", gnc_commodity_get_mnemonic(m_dflt_curr));

    std::for_each (comm_vec.cbegin(), comm_vec.cend(),
                   [this, &pt] (auto comm)
                   {
                       auto comm_mnemonic = gnc_commodity_get_mnemonic (comm);
                       auto comm_ns = std::string("currency");
                       if (gnc_commodity_is_currency (comm))
                       {
                           if (gnc_commodity_equiv(comm, m_dflt_curr) ||
                               (!comm_mnemonic || (strcmp(comm_mnemonic, "XXX") == 0)))
                               return;
                       }
                       else
                           comm_ns = gnc_quote_source_get_internal_name(gnc_commodity_get_quote_source(comm));

                       pt.put (make_quote_path(comm_ns, comm_mnemonic), "");
                   }
    );

    std::ostringstream result;
    bpt::write_json(result, pt);
    return result.str();
}

static inline std::string
get_quotes(const std::string& json_str, const std::unique_ptr<GncQuoteSource>& qs)
{
    auto [rv, quotes, errors] = qs->get_quotes(json_str);
    std::string answer;

    if (rv == 0)
    {
        for (const auto& line : quotes)
            answer.append(line + "\n");
    }
    else
    {
        std::string err_str;
        for (const auto& line: errors)
        {
            if (line == "invalid_json\n")
                PERR("Finanace Quote Wrapper was unable to parse %s",
                     json_str.c_str());
            err_str += parse_quotesource_error(line);
        }
        throw(GncQuoteException(err_str));
    }

    return answer;
}

std::string
GncQuotesImpl::query_fq (const char* source, const StrVec& commodities)
{
    bpt::ptree pt;
    auto is_currency{strcmp(source, "currency") == 0};

    if (is_currency && commodities.size() < 2)
        throw(GncQuoteException(_("Currency quotes requires at least two currencies")));

    if (is_currency)
        pt.put("defaultcurrency", commodities[0].c_str());
    else
        pt.put("defaultcurrency", gnc_commodity_get_mnemonic(m_dflt_curr));

    std::for_each(is_currency ? ++commodities.cbegin() : commodities.cbegin(),
                  commodities.cend(),
                  [source, &pt](auto sym)
                      {
                          pt.put(make_quote_path(source, sym), "");
                      });
    std::ostringstream result;
    bpt::write_json(result, pt);
    auto result_str{result.str()};
    PINFO("Query JSON: %s\n", result_str.c_str());
    return get_quotes(result.str(), m_quotesource);
}

std::string
GncQuotesImpl::query_fq (const CommVec& comm_vec)
{
    auto json_str{comm_vec_to_json_string(comm_vec)};
    PINFO("Query JSON: %s\n", json_str.c_str());
    return get_quotes(json_str, m_quotesource);
}

struct PriceParams
{
    const char* ns;
    const char* mnemonic;
    bool success;
    std::string type;
    boost::optional<std::string> price;
    bool inverted;
    boost::optional<std::string> date;
    boost::optional<std::string> time;
    boost::optional<std::string> currency;
    boost::optional<std::string> errormsg;
};

static void
get_price_and_type(PriceParams& p, const bpt::ptree& comm_pt)
{
    p.type = "last";
    p.price = comm_pt.get_optional<std::string> (p.type);

    if (!p.price)
    {
        p.type = "nav";
        p.price = comm_pt.get_optional<std::string> (p.type);
    }

    if (!p.price)
    {
        p.type = "price";
        p.price = comm_pt.get_optional<std::string> (p.type);
        /* guile wrapper used "unknown" as price type when "price" was found,
         * reproducing here to keep same result for users in the pricedb */
        p.type = p.price ? "unknown" : "missing";
    }
}

static void
parse_quote_json(PriceParams& p, const bpt::ptree& comm_pt)
{
    auto success = comm_pt.get_optional<bool> ("success");
    p.success = success && *success;
    if (!p.success)
        p.errormsg = comm_pt.get_optional<std::string> ("errormsg");
    get_price_and_type(p, comm_pt);
    auto inverted = comm_pt.get_optional<bool> ("inverted");
    p.inverted = inverted && *inverted;
    p.date = comm_pt.get_optional<std::string> ("date");
    p.time = comm_pt.get_optional<std::string> ("time");
    p.currency = comm_pt.get_optional<std::string> ("currency");


    PINFO("Commodity: %s", p.mnemonic);
    PINFO("  Success: %s", (p.success ? "yes" : "no"));
    PINFO("     Date: %s", (p.date ? p.date->c_str() : "missing"));
    PINFO("     Time: %s", (p.time ? p.time->c_str() : "missing"));
    PINFO(" Currency: %s", (p.currency ? p.currency->c_str() : "missing"));
    PINFO("    Price: %s", (p.price ? p.price->c_str() : "missing"));
    PINFO(" Inverted: %s\n", (p.inverted ? "yes" : "no"));
}

static time64
calc_price_time(const PriceParams& p)
{
    /* Note that as of F::Q v. 1.52 the only sources that provide
     * quote times are ftfunds (aka ukfunds), morningstarch, and
     * mstaruk_fund, but it's faked with a comment "Set a dummy time
     * as gnucash insists on having a valid format". It's also wrong,
     * as it lacks seconds. Best ignored.
     */
    if (p.date && !p.date->empty())
    {
        try
        {
            auto quote_time{GncDateTime(GncDate(*p.date, "m-d-y"))};
            PINFO("Quote date included, using %s for %s:%s",
                  quote_time.format("%Y-%m-%d %H:%M:%S %z").c_str(), p.ns, p.mnemonic);
            return static_cast<time64>(quote_time);
          }
        catch (const std::exception &err)
        {
            auto now{GncDateTime()};
            PWARN("Warning: failed to parse quote date '%s' for %s:%s because %s - will use %s",
                  p.date->c_str(),  p.ns, p.mnemonic, err.what(), now.format("%Y-%m-%d %H:%M:%S %z").c_str());
            return static_cast<time64>(now);
        }
    }

    auto now{GncDateTime()};
    PINFO("No date  was returned for %s:%s - will use %s",
          p.ns, p.mnemonic, now.format("%Y-%m-%d %H:%M:%S %z").c_str());
    return static_cast<time64>(now);
}

static boost::optional<GncNumeric>
get_price(const PriceParams& p)
{
    boost::optional<GncNumeric> price;
    try
    {
        price = GncNumeric { *p.price };
    }
    catch (...)
    {
        PWARN("Skipped %s:%s - failed to parse returned price '%s'",
              p.ns, p.mnemonic, p.price->c_str());
    }

    if (price && p.inverted)
        *price = price->inv();

    return price;
}

static gnc_commodity*
get_currency(const PriceParams& p, QofBook* book, QFVec& failures)
{
    if (!p.currency)
    {
        failures.emplace_back(p.ns, p.mnemonic, GncQuoteError::NO_CURRENCY,
                              empty_string);
        PWARN("Skipped %s:%s - Finance::Quote returned a quote with no  currency",
              p.ns, p.mnemonic);
        return nullptr;
    }
    std::string curr_str = *p.currency;
    boost::to_upper (curr_str);
    auto commodity_table = gnc_commodity_table_get_table (book);
    auto currency = gnc_commodity_table_lookup (commodity_table, "ISO4217", curr_str.c_str());

    if (!currency)
    {
        failures.emplace_back(p.ns, p.mnemonic,
                              GncQuoteError::UNKNOWN_CURRENCY, empty_string);
        PWARN("Skipped %s:%s  - failed to parse returned currency '%s'",
              p.ns, p.mnemonic, p.currency->c_str());
        return nullptr;
    }

    return currency;
}

GNCPrice*
GncQuotesImpl::parse_one_quote(const bpt::ptree& pt, gnc_commodity* comm)
{
    PriceParams p;
    p.ns = gnc_commodity_get_namespace (comm);
    p.mnemonic = gnc_commodity_get_mnemonic (comm);
    if (gnc_commodity_equiv(comm, m_dflt_curr) ||
        (!p.mnemonic || (strcmp (p.mnemonic, "XXX") == 0)))
        return nullptr;
    auto comm_pt_ai{pt.find(p.mnemonic)};
    if (comm_pt_ai == pt.not_found())
    {
        m_failures.emplace_back(p.ns, p.mnemonic, GncQuoteError::NO_RESULT,
                                empty_string);
        PINFO("Skipped %s:%s - Finance::Quote didn't return any data.",
              p.ns, p.mnemonic);
        return nullptr;
    }

    auto comm_pt{comm_pt_ai->second};
    parse_quote_json(p, comm_pt);

    if (!p.success)
    {
        m_failures.emplace_back(p.ns, p.mnemonic, GncQuoteError::QUOTE_FAILED,
                                p.errormsg ? *p.errormsg : empty_string);
        PWARN("Skipped %s:%s - Finance::Quote returned fetch failure.\nReason %s",
              p.ns, p.mnemonic,
              (p.errormsg ? p.errormsg->c_str() : "unknown"));
        return nullptr;
    }

    if (!p.price)
    {
        m_failures.emplace_back(p.ns, p.mnemonic,
                                GncQuoteError::NO_PRICE, empty_string);
        PWARN("Skipped %s:%s - Finance::Quote didn't return a valid price",
              p.ns, p.mnemonic);
        return nullptr;
    }

    auto price{get_price(p)};
    if (!price)
    {
        m_failures.emplace_back(p.ns, p.mnemonic,
                                GncQuoteError::PRICE_PARSE_FAILURE,
                                empty_string);
        return nullptr;
    }

    auto currency{get_currency(p, m_book, m_failures)};
    if (!currency)
       return nullptr;

    auto quotedt{calc_price_time(p)};
    auto gnc_price = gnc_price_create (m_book);
    gnc_price_begin_edit (gnc_price);
    gnc_price_set_commodity (gnc_price, comm);
    gnc_price_set_currency (gnc_price, currency);
    gnc_price_set_time64 (gnc_price, static_cast<time64> (quotedt));
    gnc_price_set_source (gnc_price, PRICE_SOURCE_FQ);
    gnc_price_set_typestr (gnc_price, p.type.c_str());
    gnc_price_set_value (gnc_price, *price);
    gnc_price_commit_edit (gnc_price);
    return gnc_price;
}

bpt::ptree
GncQuotesImpl::parse_quotes (const std::string& quote_str)
{
    bpt::ptree pt;
    std::istringstream ss {quote_str};
    std::string what;

    try
    {
        bpt::read_json (ss, pt);
    }
    catch (bpt::json_parser_error &e) {
        what = e.what();
    }
    catch (const std::runtime_error& e)
    {
        what = e.what();
    }
    catch (const std::logic_error& e)
    {
        what = e.what();
    }
    catch (...) {
        std::string error_msg{_("Failed to parse result returned by Finance::Quote.")};
        error_msg += "\n";
        //Translators: This labels the return value of a query to Finance::Quote written in an error.
        error_msg += _("Result:");
        error_msg += "\n";
        error_msg += quote_str;
        throw(GncQuoteException(error_msg));
    }
    if (!what.empty())
    {
        std::string error_msg{_("Failed to parse result returned by Finance::Quote.")};
        error_msg += "\n";
        //Translators: This is the error message reported by the Online Quotes processing code.
        error_msg += _("Error message:");
        error_msg += "\n";
        error_msg += what;
        error_msg += "\n";
        //Translators: This labels the return value of a query to Finance::Quote written in an error.
        error_msg += _("Result:");
        error_msg += "\n";
        error_msg += quote_str;
        throw(GncQuoteException(error_msg));
    }
    return pt;
}

void
GncQuotesImpl::create_quotes (const bpt::ptree& pt, const CommVec& comm_vec)
{
    auto pricedb{gnc_pricedb_get_db(m_book)};
    for (auto comm : comm_vec)
    {
        auto price{parse_one_quote(pt, comm)};
        if (!price)
            continue;
// See the comment at gnc_pricedb_add_price
        gnc_pricedb_add_price(pricedb, price);
    }
}

static void
show_verbose_quote(const bpt::ptree& comm_pt)
{
    std::for_each(comm_pt.begin(), comm_pt.end(),
                  [](auto elem) {
                      std::cout << std::setw(12) << std::right << elem.first << " => " <<
                          std::left << elem.second.data() << "\n";
                  });
    std::cout << std::endl;
}

static void
show_gnucash_quote(const bpt::ptree& comm_pt)
{
    constexpr const char* ptr{"<=== "};
    constexpr const char* dptr{"<=\\ "};
    constexpr const char* uptr{"<=/ "};
    //Translators: Means that the preceding element is required
    const char* reqd{C_("Finance::Quote", "required")};
    //Translators: Means that the quote will work best if the preceding element is provided
    const char* rec{C_("Finance::Quote", "recommended")};
    //Translators: Means that one of the indicated elements is required
    const char* oot{C_("Finance::Quote", "one of these")};
    //Translators: Means that a required element wasn't reported. The *s are for emphasis.
    const char* miss{C_("Finance::Quote", "**missing**")};

    const std::string miss_str{miss};
    auto outline{[](const char* label, std::string value, const char* pointer, const char* req) {
                         std::cout << std::setw(12) << std::right << label  << std::setw(16) << std::left <<
       value << pointer << req << "\n";
                 }};
    std::cout << _("Finance::Quote fields GnuCash uses:") << "\n";
//Translators: The stock or Mutual Fund symbol, ISIN, CUSIP, etc.
    outline(C_("Finance::Quote", "symbol: "),  comm_pt.get<char>("symbol", miss), ptr, reqd);
//Translators: The date of the quote.
    outline(C_("Finance::Quote", "date: "),  comm_pt.get<char>("date", miss), ptr, rec);
//Translators: The quote currency
    outline(C_("Finance::Quote", "currency: "),  comm_pt.get<char>("currency", miss), ptr, reqd);
    auto last{comm_pt.get<char>("last", "")};
    auto nav{comm_pt.get<char>("nav", "")};
    auto price{comm_pt.get<char>("nav", "")};
    auto no_price{last.empty() && nav.empty() && price.empty()};
//Translators: The quote is for the most recent trade on the exchange
    outline(C_("Finance::Quote", "last: "),  no_price ? miss_str : last, dptr, "");
//Translators: The quote is for an open-ended mutual fund and represents the net asset value of one unit of the fund at the previous close of trading.
    outline(C_("Finance::Quote", "nav: "),  no_price ? miss_str : nav, ptr, oot);
//Translators: The quote is neither a last trade nor an NAV.
    outline(C_("Finance::Quote", "price: "),  no_price ? miss_str : price, uptr, "");
    std::cout << std::endl;
}
static const bpt::ptree empty_tree{};

static inline const bpt::ptree&
get_commodity_data(const bpt::ptree& pt, const std::string& comm)
{
    auto commdata{pt.find(comm)};
    if (commdata == pt.not_found())
    {
        std::cout << comm << " " << _("Finance::Quote returned no data and set no error.") << std::endl;
        return empty_tree;
    }
    auto& comm_pt{commdata->second};
    auto success = comm_pt.get_optional<bool> ("success");
    if (!(success && *success))
    {
        auto errormsg = comm_pt.get_optional<std::string> ("errormsg");
        if (errormsg && !errormsg->empty())
            std::cout << _("Finance::Quote reported a failure for symbol ") <<
                comm << ": " << *errormsg << std::endl;
        else
            std::cout << _("Finance::Quote failed silently to retrieve a quote for symbol ") <<
                comm << std::endl;
        return empty_tree;
    }
    return comm_pt;
}

static void
show_quotes(const bpt::ptree& pt, const StrVec& commodities, bool verbose)
{
    for (const auto& comm : commodities)
    {
        auto comm_pt{get_commodity_data(pt, comm)};

        if (comm_pt == empty_tree)
            continue;

        if (verbose)
        {
            std::cout << comm << ":\n";
            show_verbose_quote(comm_pt);
        }
        else
        {
            show_gnucash_quote(comm_pt);
        }
    }
}

static void
show_currency_quotes(const bpt::ptree& pt, const StrVec& commodities, bool verbose)
{
    auto to_cur{commodities.front()};
    for (const auto& comm : commodities)
    {
        if (comm == to_cur)
            continue;

        auto comm_pt{get_commodity_data(pt, comm)};

        if (comm_pt == empty_tree)
            continue;

        if (verbose)
        {
            std::cout << comm << ":\n";
            show_verbose_quote(comm_pt);
        }
        else
        {
            std::cout << "1 " << comm << " = " <<
                comm_pt.get<char>("last", "Not Found") << " " << to_cur  << "\n";
        }
        std::cout << std::endl;
    }
}

static std::string
parse_quotesource_error(const std::string& line)
{
    std::string err_str;
    if (line == "invalid_json\n")
    {
        err_str += _("GnuCash submitted invalid json to Finance::Quote. The details were logged.");
    }
    else if (line.substr(0, 15) == "missing_modules")
    {
        PERR("Missing Finance::Quote Dependencies: %s",
             line.substr(17).c_str());
        err_str += _("Perl is missing the following modules. Please see https://wiki.gnucash.org/wiki/Online_Quotes#Finance::Quote for detailed corrective action. ");
        err_str += line.substr(17);
    }
    else
    {
        PERR("Unrecognized Finance::Quote Error %s", line.c_str());
        err_str +=_("Unrecognized Finance::Quote Error: ");
        err_str += line;
    }
    err_str += "\n";
    return err_str;
}

/********************************************************************
 * gnc_quotes_get_quotable_commodities
 * list commodities in a given namespace that get price quotes
 ********************************************************************/
/* Helper function to be passed to g_list_for_each applied to the result
 * of gnc_commodity_namespace_get_commodity_list.
 */
static void
get_quotables_helper1 (gpointer value, gpointer data)
{
    auto l = static_cast<CommVec *> (data);
    auto comm = static_cast<gnc_commodity *> (value);
    auto quote_flag = gnc_commodity_get_quote_flag (comm);
    auto quote_source = gnc_commodity_get_quote_source (comm);
    auto quote_source_supported = gnc_quote_source_get_supported (quote_source);

    if (!quote_flag ||
        !quote_source || !quote_source_supported)
        return;
    l->push_back (comm);
}

// Helper function to be passed to gnc_commodity_table_for_each
static gboolean
get_quotables_helper2 (gnc_commodity *comm, gpointer data)
{
    auto l = static_cast<CommVec *> (data);
    auto quote_flag = gnc_commodity_get_quote_flag (comm);
    auto quote_source = gnc_commodity_get_quote_source (comm);
    auto quote_source_supported = gnc_quote_source_get_supported (quote_source);

    if (!quote_flag ||
        !quote_source || !quote_source_supported)
        return TRUE;
    l->push_back (comm);
    return TRUE;
}

CommVec
gnc_quotes_get_quotable_commodities (const gnc_commodity_table * table)
{
    gnc_commodity_namespace * ns = NULL;
    const char *name_space;
    GList * nslist, * tmp;
    CommVec l;
    regex_t pattern;
    const char *expression = gnc_prefs_get_namespace_regexp ();

    // ENTER("table=%p, expression=%s", table, expression);
    if (!table)
        return CommVec ();

    if (expression && *expression)
    {
        if (regcomp (&pattern, expression, REG_EXTENDED | REG_ICASE) != 0)
        {
            // LEAVE ("Cannot compile regex");
            return CommVec ();
        }

        nslist = gnc_commodity_table_get_namespaces (table);
        for (tmp = nslist; tmp; tmp = tmp->next)
        {
            name_space = static_cast<const char *> (tmp->data);
            if (regexec (&pattern, name_space, 0, NULL, 0) == 0)
            {
                // DEBUG ("Running list of %s commodities", name_space);
                ns = gnc_commodity_table_find_namespace (table, name_space);
                if (ns)
                {
                    auto cm_list = gnc_commodity_namespace_get_commodity_list (ns);
                    g_list_foreach (cm_list, &get_quotables_helper1, (gpointer) &l);
                }
            }
        }
        g_list_free (nslist);
        regfree (&pattern);
    }
    else
    {
        gnc_commodity_table_foreach_commodity (table, get_quotables_helper2,
                                               (gpointer) &l);
    }
    //LEAVE ("list head %p", &l);
    return l;
}

/* Public interface functions */
// Constructor - checks for presence of Finance::Quote and import version and quote sources
GncQuotes::GncQuotes ()
{
    try
    {
        m_impl = std::make_unique<GncQuotesImpl>();
    }
    catch (const GncQuoteSourceError& err)
    {
        throw(GncQuoteException(err.what()));
    }
}


void
GncQuotes::fetch (QofBook *book)
{
    m_impl->fetch (book);
}

void GncQuotes::fetch (CommVec& commodities)
{
    m_impl->fetch (commodities);
}

void GncQuotes::fetch (gnc_commodity *comm)
{
    m_impl->fetch (comm);
}

void GncQuotes::report (const char* source, const StrVec& commodities,
                        bool verbose)
{
    m_impl->report(source, commodities, verbose);
}

const std::string& GncQuotes::version() noexcept
{
    return m_impl->version ();
}

const QuoteSources& GncQuotes::sources() noexcept
{
    return m_impl->sources ();
}

GncQuotes::~GncQuotes() = default;

bool
GncQuotes::had_failures() noexcept
{
    return m_impl->had_failures();
}

const QFVec&
GncQuotes::failures() noexcept
{
    return m_impl->failures();
}

const std::string
GncQuotes::report_failures() noexcept
{
    return m_impl->report_failures();
}
