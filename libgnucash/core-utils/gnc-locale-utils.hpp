/********************************************************************\
 * gnc-locale-utils.hpp -- provide a default locale for C++         *
 * Copyright (C) 2019 John Ralls <jralls@ceridwen.us                *
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
#ifndef GNC_LOCALE_UTILS_HPP
#define GNC_LOCALE_UTILS_HPP

#include <locale>

/** Get the default application locale.
 *
 *  If we set std::locale::global we have to imbue every stream that
 *  we want in the C locale, and that's a lot more than we want imbued
 *  with the application locale. Calling std::locale("") is expensive,
 *  so call this instead.
 *
 *  @returns A static std::locale representing the one set with
 *  setlocale() in main().
 */
const std::locale& gnc_get_locale();

#endif /* GNC_LOCALE_UTILS_HPP */
