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

#include <algorithm>
#include <vector>
#include <string>
#include <iostream>
#include <sstream>
#include <boost/algorithm/string.hpp>
#include <boost/filesystem.hpp>
#include <boost/process.hpp>
#include <boost/property_tree/ptree.hpp>
#include <boost/property_tree/json_parser.hpp>
#include <boost/iostreams/device/array.hpp>
#include <boost/iostreams/stream_buffer.hpp>
#include <boost/asio.hpp>
#include <glib.h>
#include "gnc-commodity.hpp"
#include <gnc-datetime.hpp>
#include <gnc-numeric.hpp>
#include "gnc-quotes.hpp"

extern "C" {
#include "gnc-commodity.h"
#include "gnc-path.h"
#include "gnc-ui-util.h"
#include <gnc-prefs.h>
#include <regex.h>
#include <qofbook.h>
}

namespace bp = boost::process;
namespace bfs = boost::filesystem;
namespace bpt = boost::property_tree;
namespace bio = boost::iostreams;


CommVec
gnc_quotes_get_quotable_commodities(const gnc_commodity_table * table);

class GncQuotesImpl
{
public:
    // Constructor - checks for presence of Finance::Quote and import version and quote sources
    GncQuotesImpl ();
    GncQuotesImpl (QofBook *book);

    void fetch (QofBook *book);
    void fetch (CommVec& commodities);
    void fetch (gnc_commodity *comm);

    const int cmd_result() noexcept { return m_cmd_result; }
    const std::string& error_msg() noexcept { return m_error_msg; }
    const std::string& version() noexcept { return m_version.empty() ? not_found : m_version; }
    const QuoteSources& sources() noexcept { return m_sources; }
    GList* sources_as_glist ();

private:
    // Check if Finance::Quote is properly installed
    void check (QofBook *book);
    // Run the command specified. Returns two vectors for further processing by the caller
    // - one with the contents of stdout
    // - one with the contents of stderr
    // Will also set m_cmd_result
    template <typename BufferT> CmdOutput run_cmd (const bfs::path &cmd_name, StrVec args, BufferT input);

    void query_fq (void);
    void parse_quotes (void);


    CommVec m_comm_vec;
    std::string m_version;
    QuoteSources m_sources;
    int m_cmd_result;
    std::string m_error_msg;
    std::string m_fq_answer;
    QofBook *m_book;
    gnc_commodity *m_dflt_curr;
};

/* GncQuotes implementation */

GncQuotesImpl::GncQuotesImpl ()
{
    m_version.clear();
    m_sources.clear();
    m_error_msg.clear();
    m_cmd_result  = 0;
    m_book = nullptr;
    m_dflt_curr = gnc_default_currency();

    auto perl_executable = bp::search_path("perl");
    auto fq_wrapper = std::string(gnc_path_get_bindir()) + "/finance-quote-wrapper";
    StrVec args { "-w", fq_wrapper, "-v" };

    auto cmd_out = run_cmd (perl_executable.string(), args, StrVec());

    for (auto line : cmd_out.first)
        if (m_version.empty())
            std::swap (m_version, line);
        else
            m_sources.push_back (std::move(line));

    for (auto line : cmd_out.second)
        m_error_msg.append(std::move(line) + "\n");

    if (m_cmd_result == 0)
        std::sort (m_sources.begin(), m_sources.end());
}

GList*
GncQuotesImpl::sources_as_glist()
{
    GList* slist = nullptr;
    std::for_each (m_sources.rbegin(), m_sources.rend(),
                    [&slist](const std::string& source) { slist  = g_list_prepend (slist, g_strdup(source.c_str())); });
    return slist;
}


void
GncQuotesImpl::fetch (QofBook *book)
{
    if (!book)
    {
        m_cmd_result = 1;
        m_error_msg = _("No book set");
        m_error_msg += "\n";
        return;
    }
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
    if (commodities.empty())
        return;

    m_comm_vec = std::move (commodities);  // Store for later use
    m_book = qof_instance_get_book (m_comm_vec[0]);

    query_fq ();
    if (m_cmd_result == 0)
        parse_quotes ();
}

static const std::vector <std::string>
format_quotes (const std::vector<gnc_commodity*>)
{
    return std::vector <std::string>();
}


template <typename BufferT> CmdOutput
GncQuotesImpl::run_cmd (const bfs::path &cmd_name, StrVec args, BufferT input)
{
    StrVec out_vec, err_vec;

    auto av_key = gnc_prefs_get_string ("general.finance-quote", "alphavantage-api-key");
    if (!av_key)
        std::cerr << "No Alpha Vantage API key set, currency quotes and other AlphaVantage based quotes won't work.\n";

    try
    {
        std::future<std::vector<char> > out_buf, err_buf;
        boost::asio::io_service svc;

        auto input_buf = bp::buffer (input);
        bp::child process (cmd_name, args,
                           bp::std_out > out_buf,
                           bp::std_err > err_buf,
                           bp::std_in < input_buf,
                           bp::env["ALPHAVANTAGE_API_KEY"]= (av_key ? av_key : ""),
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
                out_vec.push_back (std::move(line));

            raw = err_buf.get();
            bio::stream_buffer<bio::array_source> eb(raw.data(), raw.size());
            std::istream es(&eb);

            while (std::getline(es, line) && !line.empty())
                err_vec.push_back (std::move(line));
        }
        m_cmd_result = process.exit_code();
    }
    catch (std::exception &e)
    {
        m_cmd_result = -1;
        m_error_msg = e.what();
    };

    return CmdOutput (std::move(out_vec), std::move(err_vec));
}

void
GncQuotesImpl::query_fq (void)
{
    bpt::ptree pt, pt_child;
    pt.put ("defaultcurrency", gnc_commodity_get_mnemonic (m_dflt_curr));

    std::for_each (m_comm_vec.cbegin(), m_comm_vec.cend(),
                   [this, &pt] (auto comm)
                   {
                       auto comm_mnemonic = gnc_commodity_get_mnemonic (comm);
                       auto comm_ns = std::string("currency");
                       if (gnc_commodity_is_currency (comm))
                       {
                           if (gnc_commodity_equiv(comm, m_dflt_curr) ||
                               (!comm_mnemonic || (strcmp (comm_mnemonic, "XXX") == 0)))
                               return;
                       }
                       else
                           comm_ns = gnc_quote_source_get_internal_name (gnc_commodity_get_quote_source (comm));

                       auto key = comm_ns + "." + comm_mnemonic;
                       pt.put (key, "");
                   }
    );

    std::ostringstream result;
    bpt::write_json(result, pt);

    auto perl_executable = bp::search_path("perl");
    auto fq_wrapper = std::string(gnc_path_get_bindir()) + "/finance-quote-wrapper";
    StrVec args { "-w", fq_wrapper, "-f" };

    auto cmd_out = run_cmd (perl_executable.string(), args, result.str());

    m_fq_answer.clear();
    if (m_cmd_result == 0)
        for (auto line : cmd_out.first)
            m_fq_answer.append(std::move(line) + "\n");
    else
        for (auto line : cmd_out.second)
            m_error_msg.append(std::move(line) + "\n");

//     for (auto line : cmd_out.first)
//         std::cerr << "Output line retrieved from wrapper:\n" << line << std::endl;
//
//     for (auto line : cmd_out.second)
//         std::cerr << "Error line retrieved from wrapper:\n" << line << std::endl;

}

void
GncQuotesImpl::parse_quotes (void)
{
    bpt::ptree pt;
    std::istringstream ss {m_fq_answer};

    try
    {
        bpt::read_json (ss, pt);
    }
    catch (bpt::json_parser_error &e) {
        m_cmd_result = -1;
        m_error_msg = m_error_msg +
                      _("Failed to parse result returned by Finance::Quote.") + "\n" +
                      _("Error message:") + "\n" +
                       e.what() + "\n";
        return;
    }
    catch (...) {
        m_cmd_result = -1;
        m_error_msg = m_error_msg +
                      _("Failed to parse result returned by Finance::Quote.") + "\n";
        return;
    }

    auto pricedb = gnc_pricedb_get_db (m_book);
    std::for_each(m_comm_vec.begin(), m_comm_vec.end(),
                  [this, &pt, &pricedb] (gnc_commodity *comm)
                {
                    auto comm_ns = gnc_commodity_get_namespace (comm);
                    auto comm_mnemonic = gnc_commodity_get_mnemonic (comm);
                    if (gnc_commodity_equiv(comm, m_dflt_curr) ||
                       (!comm_mnemonic || (strcmp (comm_mnemonic, "XXX") == 0)))
                        return;
                    if (pt.find (comm_mnemonic) == pt.not_found())
                    {
                        std::cerr << "Skipped " << comm_ns << ":" << comm_mnemonic << " - Finance::Quote didn't return any data.\n";
                        return;
                    }

                    std::string key = comm_mnemonic;
                    auto success = pt.get_optional<bool> (key + ".success");
                    std::string price_type = "last";
                    auto price_str = pt.get_optional<std::string> (key + "." + price_type);
                    if (!price_str)
                    {
                        price_type = "nav";
                        price_str = pt.get_optional<std::string> (key + "." + price_type);
                    }
                    if (!price_str)
                    {
                        price_type = "price";
                        price_str = pt.get_optional<std::string> (key + "." + price_type);
                        /* guile wrapper used "unknown" as price type when "price" was found,
                         * reproducing here to keep same result for users in the pricedb */
                        price_type = "unknown";
                    }

                    auto inverted_tmp = pt.get_optional<bool> (key + ".inverted");
                    auto inverted = inverted_tmp ? *inverted_tmp : false;
                    auto date_str = pt.get_optional<std::string> (key + ".date");
                    auto time_str = pt.get_optional<std::string> (key + ".time");
                    auto currency_str = pt.get_optional<std::string> (key + ".currency");


                    std::cout << "Commodity: " << comm_mnemonic << "\n";
                    std::cout << "     Date: " << (date_str ? *date_str : "missing") << "\n";
                    std::cout << "     Time: " << (time_str ? *time_str : "missing") << "\n";
                    std::cout << " Currency: " << (currency_str ? *currency_str : "missing") << "\n";
                    std::cout << "    Price: " << (price_str ? *price_str : "missing") << "\n";
                    std::cout << " Inverted: " << (inverted ? "yes" : "no") << "\n\n";

                    if (!success || !*success)
                    {
                        auto errmsg = pt.get_optional<std::string> (key + ".errormsg");
                        std::cerr << "Skipped " << comm_ns << ":" << comm_mnemonic << " - Finance::Quote returned fetch failure.\n";
                        std::cerr << "Reason: " << (errmsg ? *errmsg : "unknown") << "\n";
                        return;
                    }

                    if (!price_str)
                    {
                        std::cerr << "Skipped " << comm_ns << ":" << comm_mnemonic << " - Finance::Quote didn't return a valid price\n";
                        return;
                    }

                    GncNumeric price;
                    try
                    {
                        price = GncNumeric { *price_str };
                    }
                    catch (...)
                    {
                        std::cerr << "Skipped " << comm_ns << ":" << comm_mnemonic << " - failed to parse returned price '" << *price_str << "'\n";
                        return;
                    }

                    if (inverted)
                        price = price.inv();

                    if (!currency_str)
                    {
                        std::cerr << "Skipped " << comm_ns << ":" << comm_mnemonic << " - Finance::Quote didn't return a currency\n";
                        return;
                    }
                    boost::to_upper (*currency_str);
                    auto commodity_table = gnc_commodity_table_get_table (m_book);
                    auto currency = gnc_commodity_table_lookup (commodity_table, "ISO4217", currency_str->c_str());

                    if (!currency)
                    {
                        std::cerr << "Skipped " << comm_ns << ":" << comm_mnemonic << " - failed to parse returned currency '" << *currency_str << "'\n";
                        return;
                    }

                    std::string iso_date_str = GncDate().format ("%Y-%m-%d");
                    if (date_str)
                    {
                    // Returned date is always in MM/DD/YYYY format according to F::Q man page, transform it to simplify conversion to GncDateTime
                        auto date_tmp = *date_str;
                        iso_date_str = date_tmp.substr (6, 4) + "-" + date_tmp.substr (0, 2) + "-" + date_tmp.substr (3, 2);
                    }
                    else
                        std::cerr << "Info: no date  was returned for " << comm_ns << ":" << comm_mnemonic << " - will use today\n";
                    iso_date_str += " " + (time_str ? *time_str : "12:00:00");

                    auto can_convert = true;
                    try
                    {
                        GncDateTime testdt {iso_date_str};
                    }
                    catch (...)
                    {
                        std::cerr << "Warning: failed to parse quote date and time '" << iso_date_str << "' for " << comm_ns << ":" << comm_mnemonic << " - will use today\n";
                        can_convert = false;
                    }

                    /*  Bit of an odd construct: GncDateTimes can't be copied,
                        which makes it impossible to first create a temporary GncDateTime
                        based on whether the string is parsable and then assign that temporary
                        to our final GncDateTime. The creation has to happen in one go, so
                        below construct will pass a different constructor argument based on
                        whether a test conversion worked or not.
                    */
                    GncDateTime quotedt {can_convert ? iso_date_str : GncDateTime()};

                    auto gnc_price = gnc_price_create (m_book);
                    gnc_price_begin_edit (gnc_price);
                    gnc_price_set_commodity (gnc_price, comm);
                    gnc_price_set_currency (gnc_price, currency);
                    gnc_price_set_time64 (gnc_price, static_cast<time64> (quotedt));
                    gnc_price_set_source (gnc_price, PRICE_SOURCE_FQ);
                    gnc_price_set_typestr (gnc_price, price_type.c_str());
                    gnc_price_set_value (gnc_price, price);
                    gnc_pricedb_add_price (pricedb, gnc_price);
                    gnc_price_commit_edit (gnc_price);
                    gnc_price_unref (gnc_price);
                });

}



/********************************************************************
 * gnc_quotes_get_quotable_commodities
 * list commodities in a given namespace that get price quotes
 ********************************************************************/
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
    m_impl = std::make_unique<GncQuotesImpl> ();
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

const int GncQuotes::cmd_result() noexcept
{
    return m_impl->cmd_result ();
}

const std::string& GncQuotes::error_msg() noexcept
{
    return m_impl->error_msg ();
}

const std::string& GncQuotes::version() noexcept
{
    return m_impl->version ();
}

const QuoteSources& GncQuotes::sources() noexcept
{
    return m_impl->sources ();
}

GList* GncQuotes::sources_as_glist ()
{
    return m_impl->sources_as_glist ();
}

GncQuotes::~GncQuotes() = default;

