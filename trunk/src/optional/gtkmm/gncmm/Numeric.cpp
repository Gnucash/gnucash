/*
 * Numeric.cpp
 * Copyright (C) 2011 Christian Stimming
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

#include "Numeric.hpp"
#include "Account.hpp"
#include "Split.hpp"

extern "C"
{
#include "app-utils/gnc-exp-parser.h"
}

namespace gnc
{

// These are in the cpp file to avoid circular dependency between the
// headers

PrintAmountInfo::PrintAmountInfo(const Glib::RefPtr<Account> account, bool use_symbol)
    : base_class(gnc_account_print_info(account->gobj(), use_symbol))
{}
PrintAmountInfo::PrintAmountInfo(const Glib::RefPtr<Split> split, bool use_symbol)
    : base_class(gnc_split_amount_print_info(split->gobj(), use_symbol))
{}

Glib::ustring Numeric::printAmount(const PrintAmountInfo& info) const
{
    char buf[256];
    if (!xaccSPrintAmount (buf, *this, info))
        buf[0] = '\0';
    return Glib::ustring(buf);
}

Glib::ustring Numeric::parse(const Glib::ustring& str)
{
    Glib::ustring errorString;

    const char* input = str.c_str();
    char *error_loc;
    Numeric result;
    gboolean p = gnc_exp_parser_parse(input, &result, &error_loc);
    if (p)
    {
        *this = result;
    }
    else
    {
        errorString = Glib::ustring(gnc_exp_parser_error_string());
    }

    return errorString;
}


} // END namespace gnc
