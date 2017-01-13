/********************************************************************
 * Gtest-gnc-int128.cpp -- unit tests for the GncInt128 class       *
 * Copyright (C) 2014 John Ralls <jralls@ceridwen.us>               *
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
 *******************************************************************/

#include <gtest/gtest.h>
#include <string>
#include "../gnc-timezone.hpp"

TEST(gnc_timezone_constructors, test_default_constructor)
{
    TimeZoneProvider tzp {};
    EXPECT_NO_THROW (tzp.get(2014));
    TZ_Ptr tz = tzp.get (2014);

//Can't really test anything explicit, we don't know what to expect
//from the default TZ.
    EXPECT_FALSE(tz->std_zone_abbrev().empty());
}

TEST(gnc_timezone_constructors, test_pacific_time_constructor)
{
#if PLATFORM(WINDOWS)
    std::string timzone("Pacific Standard Time");
#else
    std::string timezone("America/Los_Angeles");
#endif
    TimeZoneProvider tzp (timezone);
    EXPECT_NO_THROW (tzp.get(2006));
    TZ_Ptr tz = tzp.get (2006);

    EXPECT_FALSE(tz->std_zone_abbrev().empty());
#if PLATFORM(WINDOWS)
    EXPECT_TRUE(tz->std_zone_abbrev() == timezone);
#else
    EXPECT_TRUE(tz->std_zone_abbrev() == "PST");
    EXPECT_TRUE(tz->dst_zone_abbrev() == "PDT");
#endif
    EXPECT_TRUE(tz->base_utc_offset().hours() == -8);
}

#if !PLATFORM(WINDOWS)
TEST(gnc_timezone_constructors, test_posix_timezone)
{
    std::string timezone("FST08FDT07,M4.1.0,M10.31.0");
    TimeZoneProvider tzp(timezone);
    TZ_Ptr tz = tzp.get(2006);
    EXPECT_TRUE(tz->std_zone_abbrev() == "FST");
    EXPECT_TRUE(tz->dst_zone_abbrev() == "FDT");
    EXPECT_TRUE(tz->base_utc_offset().hours() == 8L);
    EXPECT_TRUE(tz->dst_offset().hours() == 7L);
}
#endif

TEST(gnc_timezone_constructors, test_bogus_time_constructor)
{
    TimeZoneProvider tzp ("New York Standard Time");
    TimeZoneProvider machine ("");
    EXPECT_EQ(machine.get(2006)->std_zone_abbrev(),
	      tzp.get(2006)->std_zone_abbrev());
}
