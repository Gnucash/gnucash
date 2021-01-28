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

extern  "C" {
#include <glib.h>
}

class GncQuotes
{
public:
    bool check (void);

    // Constructor - check for presence of Finance::Quote and import version and quote sources
    GncQuotes()  { check(); }

    // Function to check if Finance::Quote is properly installed
    int cmd_result() { return m_cmd_result; }
    std::string error_msg() { return m_error_msg; }
    std::string version() { return m_version.empty() ? "Not Found" : m_version; }
    std::vector <std::string> sources() { return m_sources; }
    GList* sources_as_glist ();
private:
    std::string m_version;
    std::vector <std::string> m_sources;
    int m_cmd_result;
    std::string m_error_msg;
};

GncQuotes& gnc_get_quotes_instance (void);

#endif /* GNC_QUOTES_HPP */
