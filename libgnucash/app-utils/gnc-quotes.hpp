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

#include <memory>
#include <string>
#include <vector>
#include <gnc-commodity.hpp>  // For CommVec alias
#include <glib.h>

extern  "C" {
#include <qofbook.h>
}

using StrVec = std::vector  <std::string>;
using QuoteSources = StrVec;
using CmdOutput = std::pair <StrVec, StrVec>;

const std::string not_found = std::string ("Not Found");

class GncQuotesImpl;

class GncQuotes
{
public:
    // Constructor - checks for presence of Finance::Quote and import version and quote sources
    GncQuotes ();
    GncQuotes (QofBook *book);
    ~GncQuotes ();

    // Fetch quotes for all commodities in our db that have a quote source set
    void fetch (void);
    // Only fetch quotes for the commodities passed that have a quote source  set
    void fetch (CommVec& commodities);
    // Fetch quote for the commodity if it has a quote source  set
    void fetch (gnc_commodity *comm);

    const int cmd_result() noexcept;
    const std::string& error_msg() noexcept;
    const std::string& version() noexcept;
    const QuoteSources& sources() noexcept;
    GList* sources_as_glist ();

private:
    std::unique_ptr<GncQuotesImpl> m_impl;
};

#endif /* GNC_QUOTES_HPP */
