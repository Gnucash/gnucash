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
    std::string timezone("Pacific Standard Time");
#else
    std::string timezone("America/Los_Angeles");
#endif
    TimeZoneProvider tzp (timezone);
    EXPECT_NO_THROW (tzp.get(2012));
    TZ_Ptr tz = tzp.get (2012);

    EXPECT_FALSE(tz->std_zone_abbrev().empty());
#if PLATFORM(WINDOWS)
    EXPECT_TRUE(tz->std_zone_abbrev() == timezone);
#else
    EXPECT_TRUE(tz->std_zone_abbrev() == "PST");
    EXPECT_TRUE(tz->dst_zone_abbrev() == "PDT");
#endif
    EXPECT_EQ(-8, tz->base_utc_offset().hours());

    EXPECT_EQ(12, tz->dst_local_start_time (2017).date().day());
}

#if !PLATFORM(WINDOWS)
TEST(gnc_timezone_constructors, test_posix_timezone)
{
    std::string timezone("FST08FDT07,M4.1.0,M10.31.0");
    TimeZoneProvider tzp(timezone);
    TZ_Ptr tz = tzp.get(2006);
    EXPECT_EQ(tz->std_zone_abbrev(), "FST");
    EXPECT_EQ(tz->dst_zone_abbrev(), "FDT");
    EXPECT_TRUE(tz->has_dst());
    EXPECT_EQ(tz->base_utc_offset().hours(), 8L);
    EXPECT_EQ(tz->dst_offset().hours(), 7L);
}

TEST(gnc_timezone_constructors, test_gmt_timezone)
{
    std::string timezone("GMT");
    TimeZoneProvider tzp(timezone);
    TZ_Ptr tz = tzp.get(2006);
    EXPECT_EQ(tz->std_zone_abbrev(), "GMT");
    EXPECT_FALSE(tz->has_dst());
    EXPECT_EQ(tz->dst_zone_abbrev(), "");
    EXPECT_EQ(tz->base_utc_offset().hours(), 0L);
    EXPECT_EQ(tz->dst_offset().hours(), 0L);
}

TEST(gnc_timezone_constructors, test_GMT_plus_7_timezone)
{
    std::string timezone("Etc/GMT+7");
    TimeZoneProvider tzp(timezone);
    TZ_Ptr tz = tzp.get(2006);
    EXPECT_EQ(tz->std_zone_abbrev(), "-07");
    EXPECT_EQ(tz->dst_zone_abbrev(), "");
    EXPECT_FALSE(tz->has_dst());
    EXPECT_EQ(tz->base_utc_offset().hours(), -7);
    EXPECT_EQ(tz->dst_offset().hours(), 0L);
}

TEST(gnc_timezone_constructors, test_IANA_Belize_tz)
{
    TimeZoneProvider tzp("America/Belize");
    for (int year = 1908; year < 1990; ++year)
    {
        auto tz = tzp.get(year);
        if (year < 1912)
        {
            EXPECT_EQ(tz->std_zone_abbrev(), "LMT");
            EXPECT_FALSE(tz->has_dst());
            EXPECT_EQ(tz->base_utc_offset().total_seconds(), -21168);
        }
        else if (year < 1918)
        {
            EXPECT_EQ(tz->std_zone_abbrev(), "CST");
            EXPECT_FALSE(tz->has_dst());
            EXPECT_EQ(tz->base_utc_offset().total_seconds(), -21600);
        }
        else if (year < 1943)
        {
            EXPECT_EQ(tz->std_zone_abbrev(), "CST");
            EXPECT_TRUE(tz->has_dst());
            EXPECT_EQ(tz->base_utc_offset().total_seconds(), -21600);
            EXPECT_EQ(tz->dst_zone_abbrev(), "-0530");
            EXPECT_EQ(tz->dst_offset().total_seconds(), 1800);
        }
        else if (year == 1973 || year == 1982)
        {
            EXPECT_EQ(tz->std_zone_abbrev(), "CST");
            EXPECT_TRUE(tz->has_dst());
            EXPECT_EQ(tz->base_utc_offset().total_seconds(), -21600);
            EXPECT_EQ(tz->dst_zone_abbrev(), "CDT");
            EXPECT_EQ(tz->dst_offset().total_seconds(), 3600);
        }
        else
        {
            EXPECT_EQ(tz->std_zone_abbrev(), "CST");
            EXPECT_FALSE(tz->has_dst());
            EXPECT_EQ(tz->base_utc_offset().total_seconds(), -21600);
        }
   }
}

TEST(gnc_timezone_constructors, test_IANA_Perth_tz)
{
    TimeZoneProvider tzp("Australia/Perth");
    for (int year = 1916; year < 2048; ++year)
    {
        auto tz = tzp.get(year);
        if (year < 1917)
        {
            EXPECT_EQ(tz->std_zone_abbrev(), "AWST");
            EXPECT_TRUE(tz->has_dst());
            EXPECT_EQ(tz->base_utc_offset().total_seconds(), 28800);
            EXPECT_EQ(tz->dst_zone_abbrev(), "AWDT");
            EXPECT_EQ(tz->dst_offset().total_seconds(), 3600);
        }
        else if (year < 1941)
        {
            EXPECT_EQ(tz->std_zone_abbrev(), "AWST");
            EXPECT_FALSE(tz->has_dst());
            EXPECT_EQ(tz->base_utc_offset().total_seconds(), 28800);
        }
        else if (year < 1943)
        {
            EXPECT_EQ(tz->std_zone_abbrev(), "AWST");
            EXPECT_TRUE(tz->has_dst());
            EXPECT_EQ(tz->base_utc_offset().total_seconds(), 28800);
            EXPECT_EQ(tz->dst_zone_abbrev(), "AWDT");
            EXPECT_EQ(tz->dst_offset().total_seconds(), 3600);
        }
        else if (year == 1974 || year == 1983 || year == 1991 ||
                 (year > 2005 && year < 2009))
        {
            EXPECT_EQ(tz->std_zone_abbrev(), "AWST");
            EXPECT_TRUE(tz->has_dst());
            EXPECT_EQ(tz->base_utc_offset().total_seconds(), 28800);
            EXPECT_EQ(tz->dst_zone_abbrev(), "AWDT");
            EXPECT_EQ(tz->dst_offset().total_seconds(), 3600);
        }
        else
        {
            EXPECT_EQ(tz->std_zone_abbrev(), "AWST");
            EXPECT_FALSE(tz->has_dst());
            EXPECT_EQ(tz->base_utc_offset().total_seconds(), 28800);
        }
    }
}

TEST(gnc_timezone_constructors, test_IANA_Minsk_tz)
{
    TimeZoneProvider tzp("Europe/Minsk");
    for (int year = 1916; year < 2020; ++year)
    {
        auto tz = tzp.get(year);
        if (year < 1924)
        {
            EXPECT_EQ(tz->std_zone_abbrev(), "MMT");
            EXPECT_FALSE(tz->has_dst());
            EXPECT_EQ(tz->base_utc_offset().total_seconds(), 6600);
        }
        else if (year < 1930)
        {
            EXPECT_EQ(tz->std_zone_abbrev(), "EET");
            EXPECT_FALSE(tz->has_dst());
            EXPECT_EQ(tz->base_utc_offset().total_seconds(), 7200);
        }
        else if (year < 1941)
        {
            EXPECT_EQ(tz->std_zone_abbrev(), "MSK");
            EXPECT_FALSE(tz->has_dst());
            EXPECT_EQ(tz->base_utc_offset().total_seconds(), 10800);
        }
        /* The TZInfo says Minsk had DST from June 1941 - Nov
         * 1942. Boost::date_time doesn't know how to model that so we
         * just pretend that it was a weird standard time. Note that
         * Minsk was under German occupation and got shifted to Berlin
         * time, sort of.
         */
        else if (year < 1943)
        {
            EXPECT_EQ(tz->std_zone_abbrev(), "CEST");
            EXPECT_FALSE(tz->has_dst());
            EXPECT_EQ(tz->base_utc_offset().total_seconds(), 7200);
            EXPECT_EQ(tz->dst_zone_abbrev(), "");
            EXPECT_EQ(tz->dst_offset().total_seconds(), 0);
        }
        else if (year == 1943)
        {
            EXPECT_EQ(tz->std_zone_abbrev(), "CET");
            EXPECT_TRUE(tz->has_dst());
            EXPECT_EQ(tz->base_utc_offset().total_seconds(), 3600);
            EXPECT_EQ(tz->dst_zone_abbrev(), "CEST");
            EXPECT_EQ(tz->dst_offset().total_seconds(), 3600);
        }
        /* Minsk was "liberated" by the Soviets 2 Jul 1944 and went
         * back to a more reasonable local time with no DST. Another
         * case that's too hard for boost::timezone to model correctly
         * so we fudge.
         */
        else if (year == 1944)
        {
            EXPECT_EQ(tz->std_zone_abbrev(), "MSK");
            EXPECT_TRUE(tz->has_dst());
            EXPECT_EQ(tz->base_utc_offset().total_seconds(), 10800);
            EXPECT_EQ(tz->dst_zone_abbrev(), "CEST");
            EXPECT_EQ(tz->dst_offset().total_seconds(), -3600);
        }
        else if (year < 1981)
        {
            EXPECT_EQ(tz->std_zone_abbrev(), "MSK");
            EXPECT_FALSE(tz->has_dst());
            EXPECT_EQ(tz->base_utc_offset().total_seconds(), 10800);
        }
        else if (year < 1989)
        {
            EXPECT_EQ(tz->std_zone_abbrev(), "MSK");
            EXPECT_TRUE(tz->has_dst());
            EXPECT_EQ(tz->base_utc_offset().total_seconds(), 10800);
            EXPECT_EQ(tz->dst_zone_abbrev(), "MSD");
            EXPECT_EQ(tz->dst_offset().total_seconds(), 3600);
        }
        else if (year < 1991)
        {
            EXPECT_EQ(tz->std_zone_abbrev(), "MSK");
            EXPECT_FALSE(tz->has_dst());
            EXPECT_EQ(tz->base_utc_offset().total_seconds(), 10800);
        }
        else if (year < 2011)
        {
            EXPECT_EQ(tz->std_zone_abbrev(), "EET");
            EXPECT_TRUE(tz->has_dst());
            EXPECT_EQ(tz->base_utc_offset().total_seconds(), 7200);
            EXPECT_EQ(tz->dst_zone_abbrev(), "EEST");
            EXPECT_EQ(tz->dst_offset().total_seconds(), 3600);
        }
        else
        {
            EXPECT_EQ(tz->std_zone_abbrev(), "+03");
            EXPECT_FALSE(tz->has_dst());
            EXPECT_EQ(tz->base_utc_offset().total_seconds(), 10800);
        }
     }
}
#endif

TEST(gnc_timezone_constructors, test_bogus_time_constructor)
{
    TimeZoneProvider tzp ("New York Standard Time");
    TimeZoneProvider machine ("");
    EXPECT_EQ(machine.get(2006)->std_zone_abbrev(),
	      tzp.get(2006)->std_zone_abbrev());
}
