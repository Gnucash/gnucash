/********************************************************************
 * GLib test suite for the C++ interface to guid.cpp                *
 *
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
 * along with this program; if not, you can retrieve it from        *
 * https://www.gnu.org/licenses/old-licenses/gpl-2.0.html            *
 * or contact:                                                      *
 *                                                                  *
 * Free Software Foundation           Voice:  +1-617-542-5942       *
 * 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652       *
 * Boston, MA  02110-1301,  USA       gnu@gnu.org                   *
 ********************************************************************/

#include "../guid.hpp"

#include <random>
#include <sstream>
#include <iomanip>
#include <string>
#include <iostream>
#include <gtest/gtest.h>
#include <boost/version.hpp>

TEST (GncGUID, creation)
{
    auto guid = gnc::GUID::create_random ();
    EXPECT_NE (guid, gnc::GUID::null_guid ());
    // There should be a default constructor.
    GncGUID other;
}

TEST (GncGUID, copy)
{
    auto guid = gnc::GUID::create_random ();
    auto cpy = guid;
    EXPECT_EQ (guid, cpy);
    gnc::GUID cpy2 {cpy};
    EXPECT_EQ (guid, cpy2);
}

TEST (GncGUID, move)
{
    auto guid = gnc::GUID::create_random ();
    auto cpy = guid;
    auto mv = std::move(guid);
    EXPECT_EQ (cpy, mv);
}

TEST (GncGUID, to_string)
{
    std::string fixture (32, '0');
    auto str = gnc::GUID::null_guid ().to_string ();
    EXPECT_EQ (str, fixture);
}

TEST (GncGUID, from_string)
{
    std::string fixture (32, '0');
    auto guid = gnc::GUID::from_string (fixture);
    EXPECT_EQ (guid, gnc::GUID::null_guid ());

    guid = gnc::GUID::create_random ();
    std::string bogus {"Have a great big roast beef sandwich, if you please!"};
    bool fail = false;
    try
    {
        auto guid = gnc::GUID::from_string (bogus);
    }
    catch (gnc::guid_syntax_exception const &)
    {
        fail = true;
    }
    /* Currently, boost uuid string parsing is mostly very permissive, but it has some
     * odd pet peves. See https://svn.boost.org/trac/boost/ticket/12253 for more.*/
    if (BOOST_VERSION >= 106600)
        EXPECT_TRUE (fail) << "Parsing the bogus string should throw";
    else
        EXPECT_FALSE (fail) << "Perhaps boost uuid is fixed.";
}

TEST (GncGUID, round_trip)
{
    auto guid1 = gnc::GUID::create_random ();
    auto str = guid1.to_string ();
    auto guid2 = gnc::GUID::from_string (str);
    EXPECT_EQ (guid1, guid2);
}

