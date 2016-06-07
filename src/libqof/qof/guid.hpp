/********************************************************************
 * guid.hpp - GncGUID struct definition.                            *
 * Copyright 2016 Aaron Laws                                        *
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
 *******************************************************************/
#ifndef GUID_HPP_HEADER
#define GUID_HPP_HEADER

#include <boost/uuid/uuid.hpp>
#include <stdexcept>

struct guid_syntax_exception : public std::invalid_argument
{
    guid_syntax_exception () noexcept;
};

struct GncGUID : public boost::uuids::uuid
{
    GncGUID (boost::uuids::uuid const &) noexcept;
    GncGUID () noexcept = default;
    static GncGUID create_random () noexcept;
    static GncGUID const & null_guid () noexcept;
    static GncGUID from_string (std::string const &) throw (guid_syntax_exception);
    std::string to_string () const noexcept;
};

#endif
