/*******************************************************************\
 * csv-export-helpers.c -- Functions to assist csv export           *
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
/** @file csv-export-helprs.cpp
    @brief CSV Export helper functions
    @author Christopher Lam
*/
#include <config.h>

#include <cstring>
#include <cstdio>
#include <fstream>
#include <vector>

#include "gnc-ui-util.h"
#include "csv-export-helpers.hpp"

/* This static indicates the debugging module that this .o belongs to.  */
[[maybe_unused]] static QofLogModule log_module = GNC_MOD_ASSISTANT;

/* CSV spec requires CRLF line endings. Tweak the end-of-line string so this
 * true for each platform */
#ifdef G_OS_WIN32
# define EOLSTR "\n"
#else
# define EOLSTR "\r\n"
#endif

#define QUOTE '"'

bool
gnc_csv_add_line (std::ostream& ss, const StringVec& str_vec,
                  bool use_quotes, const char* sep)
{
    auto first{true};
    auto sep_view{std::string_view (sep ? sep : "")};
    for (const auto& str : str_vec)
    {
        auto need_quote = use_quotes
            || (!sep_view.empty() && str.find (sep_view) != std::string::npos)
            || str.find_first_of ("\"\n\r") != std::string::npos;

        if (first)
            first = false;
        else
            ss << sep_view;

        if (need_quote)
            ss << QUOTE;

        for (const char& p : str)
        {
            ss << p;
            if (p == QUOTE)
                ss << QUOTE;
        }

        if (need_quote)
            ss << QUOTE;

        if (ss.fail())
            return false;
    }
    ss << EOLSTR;

    return !ss.fail();
}

std::string
account_get_fullname_str (Account *account)
{
    auto name{gnc_account_get_full_name (account)};
    auto rv{std::string(name)};
    g_free (name);
    return rv;
}
