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
\********************************************************************/
#ifndef GNC_QUOTES_HPP
#define GNC_QUOTES_HPP

#include <string>
#include <vector>
#include <gnc-commodity.hpp>  // For CommVec alias

extern  "C" {
#include <glib.h>
#include <qofbook.h>
}

using StrVec = std::vector  <std::string>;
using QuoteSources = StrVec;
using CmdOutput = std::pair <StrVec, StrVec>;

const std::string not_found = std::string ("Not Found");


class GncQuotes
{
public:
    // Constructor - checks for presence of Finance::Quote and import version and quote sources
    GncQuotes();

    void fetch_all (QofBook *book);
    void fetch (const CommVec& commodities);

    const int cmd_result() noexcept { return m_cmd_result; }
    const std::string& error_msg() noexcept { return m_error_msg; }
    const std::string& version() noexcept { return m_version.empty() ? not_found : m_version; }
    const QuoteSources& sources() noexcept { return m_sources; }
    GList* sources_as_glist ();

private:
    // Check if Finance::Quote is properly installed
    void check (void);
    // Run the command specified. Returns two vectors for further processing by the caller
    // - one with the contents of stdout
    // - one with the contents of stderr
    // Will also set m_cmd_result
    CmdOutput run_cmd (std::string cmd_name, StrVec args, StrVec input_vec);


    std::string m_version;
    QuoteSources m_sources;
    int m_cmd_result;
    std::string m_error_msg;
};

#endif /* GNC_QUOTES_HPP */
