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
    EXPECT_FALSE(atime.isnull());
}

TEST(gnc_datetime_constructors, test_time64_constructor)
{
    const time64 time = 2394187200; //2045-11-13 12:00:00 Z
    GncDateTime atime(time);
    EXPECT_FALSE(atime.isnull());
}
