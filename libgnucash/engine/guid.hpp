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
extern "C" {
#include "guid.h"
}

namespace gnc {
struct guid_syntax_exception : public std::invalid_argument
{
    guid_syntax_exception () noexcept;
};

struct GUID
{
    private:
    boost::uuids::uuid implementation;

    public:
    GUID (boost::uuids::uuid const &) noexcept;
    GUID (GncGUID const &) noexcept;
    GUID (GUID const &) noexcept = default;
    GUID () noexcept = default;
    GUID & operator = (GUID &&) noexcept;

    operator GncGUID () const noexcept;
    static GUID create_random () noexcept;
    static GUID const & null_guid () noexcept;
    static GUID from_string (std::string const &);
    static bool is_valid_guid (std::string const &);
    std::string to_string () const noexcept;
    auto begin () const noexcept -> decltype (implementation.begin ());
    auto end () const noexcept -> decltype (implementation.end ());
    bool operator < (GUID const &) noexcept;
    friend bool operator == (GUID const &, GncGUID const &) noexcept;
    friend bool operator != (GUID const &, GUID const &) noexcept;
};

bool operator != (GUID const &, GUID const &) noexcept;
bool operator == (GUID const &, GncGUID const &) noexcept;


}
#endif
