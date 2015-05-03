/********************************************************************\
 * test-gnc-datetime.cpp -- Unit tests for GncDate and GncDateTime  *
 *                                                                  *
 * Copyright 2015 John Ralls <jralls@ceridwen.us>                   *
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

#include "../gnc-datetime.hpp"
#include <gtest/gtest.h>

TEST(gnc_date_constructors, test_default_constructor)
{
    GncDate date;
    EXPECT_FALSE(date.isnull());
}

TEST(gnc_date_constructors, test_ymd_constructor)
{
    GncDate date(2045, 11, 13);
    EXPECT_FALSE(date.isnull());
}

TEST(gnc_datetime_constructors, test_default_constructor)
{
    GncDateTime atime;
    EXPECT_EQ(static_cast<time64>(atime), static_cast<time64>(INT64_C(0)));
}

TEST(gnc_datetime_constructors, test_time64_constructor)
{
    const time64 time = 2394187200; //2045-11-13 12:00:00 Z
    GncDateTime atime(time);
    EXPECT_EQ(static_cast<time64>(atime), time);
}

TEST(gnc_datetime_constructors, test_struct_tm_constructor)
{
#ifdef HAVE_STRUCT_TM_GMTOFF
    const struct tm tm {0, 0, 12, 13, 10, 145, 0, 0, 0, NULL, 0 };
#else
    const struct tm tm {0, 0, 12, 13, 10, 145, 0, 0, 0 };
#endif

    const time64 time = 2394187200; //2045-11-13 12:00:00 Z
    GncDateTime atime(tm);
    EXPECT_EQ(static_cast<time64>(atime), time);
    const struct tm tm1 = static_cast<struct tm>(atime);
    EXPECT_EQ(tm1.tm_year, tm.tm_year);
    EXPECT_EQ(tm1.tm_mon, tm.tm_mon);
    EXPECT_EQ(tm1.tm_mday, tm.tm_mday);
// We have to contort this a bit to handle offsets > 12, e.g. New Zealand during DST.
    EXPECT_EQ((24 + tm1.tm_hour - atime.offset() / 3600) % 24, tm.tm_hour);
    EXPECT_EQ(tm1.tm_min, tm.tm_min);
}

TEST(gnc_datetime_functions, test_format)
{
    GncDateTime atime(2394187200); //2045-11-13 12:00:00 Z
    //Date only to finesse timezone issues. It will still fail in +12 DST.
    EXPECT_EQ(atime.format("%d-%m-%Y"), "13-11-2045");
}

//This is a bit convoluted because it uses GncDate's GncDateImpl constructor and year_month_day() function. There's no good way to test the former without violating the privacy of the implementation.
TEST(gnc_datetime_functions, test_date)
{
    GncDateTime atime(2394187200); //2045-11-13 12:00:00 Z
    GncDate gncd = std::move(atime.date());
    auto ymd = gncd.year_month_day();
    EXPECT_EQ(ymd.year, 2045);
    EXPECT_EQ(ymd.month, 11);
    EXPECT_EQ(ymd.day, 13);
}
