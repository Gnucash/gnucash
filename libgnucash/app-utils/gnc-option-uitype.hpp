/********************************************************************\
 * gnc-option-uitype.hpp -- UI Control Enum for GncOption           *
 * Copyright (C) 2020 John Ralls <jralls@ceridwen.us>               *
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
 *                                                                  *
\********************************************************************/
#ifndef GNC_OPTION_UITYPE_HPP__
#define GNC_OPTION_UITYPE_HPP__

enum GncOptionUIType
{
    INTERNAL,
    BOOLEAN,
    STRING,
    TEXT,
    CURRENCY,
    COMMODITY,
    MULTICHOICE,
    DATE_ABSOLUTE,
    DATE_RELATIVE_BEGIN,
    DATE_BOTH_BEGIN,
    DATE_RELATIVE_END,
    DATE_BOTH_END,
    ACCOUNT_LIST,
    ACCOUNT_SEL,
    LIST,
    NUMBER_RANGE,
    COLOR,
    FONT,
    BUDGET,
    PIXMAP,
    RADIOBUTTON,
    DATE_FORMAT,
    OWNER,
    CUSTOMER,
    VENDOR,
    EMPLOYEE,
    INVOICE,
    TAX_TABLE,
    QUERY,
};

#endif // GNC_OPTION_UITYPE_H__
