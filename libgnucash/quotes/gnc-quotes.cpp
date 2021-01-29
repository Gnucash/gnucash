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

#include <algorithm>
#include <vector>
#include <string>
#include <iostream>
#include <boost/filesystem.hpp>
#include <boost/process.hpp>
#include <glib.h>
#include "gnc-quotes.hpp"

extern "C" {
    #include "gnc-path.h"
}

namespace bp = boost::process;

static GncQuotes quotes_cached;

bool
GncQuotes::check (void)
{
    m_version.clear();
    m_sources.clear();
    m_error_msg.clear();
    m_cmd_result  = 0;

    auto perl_executable = bp::search_path("perl"); //or get it from somewhere else.
    auto fq_check = std::string(gnc_path_get_bindir()) + "/gnc-fq-check";

    bp::ipstream out_stream;
    bp::ipstream err_stream;
    bp::child process (perl_executable, "-w", fq_check, bp::std_out > out_stream, bp::std_err > err_stream);

    std::string stream_line;
    while (getline (out_stream, stream_line))
        if (m_version.empty())
            std::swap (m_version, stream_line);
        else
            m_sources.push_back (std::move(stream_line));
    while (getline (err_stream, stream_line))
        m_error_msg.append(stream_line + "\n");

    process.wait();
    m_cmd_result = process.exit_code();

    auto success = (m_cmd_result == 0);

    if (success)
        std::sort (m_sources.begin(), m_sources.end());

    return success;
}

GList*
GncQuotes::sources_as_glist()
{
    GList* slist = nullptr;
    std::for_each (m_sources.rbegin(), m_sources.rend(),
                    [&slist](const std::string& source) { slist  = g_list_prepend (slist, g_strdup(source.c_str())); });
    return slist;
}



const GncQuotes& gnc_get_quotes_instance()
{
    return quotes_cached;
}
