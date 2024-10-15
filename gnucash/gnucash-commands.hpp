/*
 * gnucash-commands.hpp -- Implementation of all the commands that
 *                         can be invoked via gnucash-cli
 *
 * Copyright (C) 2020 Geert Janssens <geert@kobaltwit.be>
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; either version 2 of
 * the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, contact:
 *
 * Free Software Foundation           Voice:  +1-617-542-5942
 * 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652
 * Boston, MA  02110-1301,  USA       gnu@gnu.org
 */

#ifndef GNUCASH_COMMANDS_HPP
#define GNUCASH_COMMANDS_HPP

#include <string>
#include <vector>
#include <boost/optional.hpp>

using bo_str = boost::optional <std::string>;
using StrVec = std::vector<std::string>;

namespace Gnucash {

    int check_finance_quote (void);
    int add_quotes (const bo_str& uri);
    int report_quotes (const char* source,
                       const StrVec& commodities,
                       bool verbose);
    int run_report (const bo_str& file_to_load,
                    const bo_str& run_report,
                    const bo_str& export_type,
                    const bo_str& output_file);
    int report_list (void);
    int report_show (const bo_str& file_to_load,
                     const bo_str& run_report);
    int run_scripting (std::vector<const char*> newArgv,
                       const bo_str& m_file_to_load,
                       std::string& m_language,
                       const bo_str& m_script,
                       bool m_open_readwrite,
                       bool m_interactive);
}
#endif
