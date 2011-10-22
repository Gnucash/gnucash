/*
 * Numeric.cpp
 * Copyright (C) 2010 Christian Stimming
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
#include "gnc/Account.hpp"
#include "gnc/Split.hpp"

extern "C"
{
#include "app-utils/gnc-exp-parser.h"
}

namespace gnc
{

// These are in the cpp file to avoid circular dependency between the
// headers

PrintAmountInfo::PrintAmountInfo(const Account& account, bool use_symbol)
        : base_class(gnc_account_print_info(account.get(), use_symbol))
{}
PrintAmountInfo::PrintAmountInfo(const Split& split, bool use_symbol)
        : base_class(gnc_split_amount_print_info(split.get(), use_symbol))
{}

QString Numeric::printAmount(const PrintAmountInfo& info) const
{
    char buf[256];
    if (!xaccSPrintAmount (buf, *this, info))
        buf[0] = '\0';
    return QString::fromUtf8(buf);
}

QString Numeric::parse(const QString& str)
{
    QString errorString;

    const char* input = str.toUtf8();
    char *error_loc;
    Numeric result;
    gboolean p = gnc_exp_parser_parse(input, &result, &error_loc);
    if (p)
    {
        *this = result;
    }
    else
    {
        errorString = QString::fromUtf8(gnc_exp_parser_error_string());
    }

    return errorString;
}


} // END namespace gnc
