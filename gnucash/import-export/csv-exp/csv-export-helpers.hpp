/*******************************************************************\
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

#ifndef CSV_EXPORT_HELPERS
#define CSV_EXPORT_HELPERS

#include <config.h>
#include "Account.h"

#include <string>
#include <cstdio>
#include <fstream>
#include <vector>

using StringVec = std::vector<std::string>;

// add a csv-formatted line onto output stream. charsvec is the vector
// of std::strings, sep is the separator string. use_quotes to always
// "quote"; some strings may be quoted anyway if contains separator
// string, quote, \r or \n. This function returns a bool indicating
// success.
bool gnc_csv_add_line (std::ostream& ss, const StringVec& charsvec,
                       bool use_quotes, const char* sep);

std::string account_get_fullname_str (Account*);

#endif

