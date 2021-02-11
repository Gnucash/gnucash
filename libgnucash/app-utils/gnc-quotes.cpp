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
#include <boost/filesystem.hpp>
#include <boost/process.hpp>
#include <boost/property_tree/ptree.hpp>
#include <boost/property_tree/json_parser.hpp>
#include <boost/iostreams/device/array.hpp>
#include <boost/iostreams/stream_buffer.hpp>
#include <boost/asio.hpp>
#include <glib.h>
#include "gnc-commodity.hpp"
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
namespace bpt = boost::property_tree;
namespace bio = boost::iostreams;


CommVec
gnc_quotes_get_quotable_commodities(const gnc_commodity_table * table);

GncQuotes::GncQuotes()
{
    check();
}

void
GncQuotes::check (void)
{
    m_version.clear();
    m_sources.clear();
    m_error_msg.clear();
    m_cmd_result  = 0;

    auto perl_executable = bp::search_path("perl"); //or get it from somewhere else.
    auto fq_check = std::string(gnc_path_get_bindir()) + "/gnc-fq-check";

    try
    {
        std::future<std::vector<char> > output, error;
        boost::asio::io_service svc;

        bp::child process (perl_executable, "-w", fq_check, bp::std_out > output, bp::std_err > error, svc);
        svc.run();
        process.wait();

        {
            auto raw = output.get();
            std::vector<std::string> data;
            std::string line;
            bio::stream_buffer<bio::array_source> sb(raw.data(), raw.size());
            std::istream is(&sb);

            while (std::getline(is, line) && !line.empty())
                if (m_version.empty())
                    std::swap (m_version, line);
                else
                    m_sources.push_back (std::move(line));

            raw = error.get();
            bio::stream_buffer<bio::array_source> eb(raw.data(), raw.size());
            std::istream es(&eb);

            while (std::getline(es, line) && !line.empty())
                m_error_msg.append(std::move(line) + "\n");
        }
        m_cmd_result = process.exit_code();
    }
    catch (std::exception &e)
    {
        m_cmd_result = -1;
        m_error_msg = e.what();
    };

    if (m_cmd_result == 0)
        std::sort (m_sources.begin(), m_sources.end());
}

GList*
GncQuotes::sources_as_glist()
{
    GList* slist = nullptr;
    std::for_each (m_sources.rbegin(), m_sources.rend(),
                    [&slist](const std::string& source) { slist  = g_list_prepend (slist, g_strdup(source.c_str())); });
    return slist;
}


void
GncQuotes::fetch (const CommVec& commodities)
{
    auto dflt_curr = gnc_default_currency();
    bpt::ptree pt, pt_child;
    pt.put ("defaultcurrency", gnc_commodity_get_mnemonic (dflt_curr));

    std::for_each (commodities.cbegin(), commodities.cend(),
        [&pt, &dflt_curr] (auto comm)
        {
            auto comm_mnemonic = gnc_commodity_get_mnemonic (comm);
            auto comm_ns = std::string("currency");
            if (gnc_commodity_is_currency (comm))
            {
                if (gnc_commodity_equiv(comm, dflt_curr) ||
                    (!comm_mnemonic  || (strcmp (comm_mnemonic, "XXX") == 0)))
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
    std::cerr << "GncQuotes fetch_all - resulting json object\n" << result.str() << std::endl;

}


void
GncQuotes::fetch_all (QofBook *book)
{
    auto commodities = gnc_quotes_get_quotable_commodities (
        gnc_commodity_table_get_table (book));

    fetch (commodities);
}

static const std::vector <std::string>
format_quotes (const std::vector<gnc_commodity*>)
{
    return std::vector <std::string>();
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
