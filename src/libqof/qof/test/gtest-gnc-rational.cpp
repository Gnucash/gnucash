/********************************************************************
 * Gtest-gnc-rational.cpp -- unit tests for the GncInt128 class       *
 * Copyright (C) 2017 John Ralls <jralls@ceridwen.us>               *
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
#include "../gnc-rational.hpp"

TEST(gncrational_constructors, test_default_constructor)
{
    GncRational value;
    EXPECT_EQ(value.m_num, 0);
    EXPECT_EQ(value.m_den, 1);
    EXPECT_EQ(value.m_error, GNC_ERROR_OK);
}

TEST(gncrational_constructors, test_gnc_numeric_constructor)
{
    gnc_numeric input = gnc_numeric_create(123, 456);
    GncRational value(input);
    EXPECT_EQ(input.num, value.m_num);
    EXPECT_EQ(input.denom, value.m_den);
    EXPECT_EQ(value.m_error, GNC_ERROR_OK);
}

TEST(gncrational_constructors, test_gnc_int128_constructor)
{
    GncInt128 num(123), denom(456);
    GncRational value(num, denom);
    EXPECT_EQ(value.m_num, 123);
    EXPECT_EQ(value.m_den, 456);
    EXPECT_EQ(value.m_error, GNC_ERROR_OK);
}

TEST(gncrational_constructors, test_implicit_int_constructor)
{
    int num(123), denom(456);
    GncRational value(num, denom);
    EXPECT_EQ(value.m_num, 123);
    EXPECT_EQ(value.m_den, 456);
    EXPECT_EQ(value.m_error, GNC_ERROR_OK);
}

TEST(gncrational_constructors, test_with_error_code)
{
    int num(123), denom(456);
    GncRational value(num, denom, GNC_ERROR_OVERFLOW);
    EXPECT_EQ(value.m_num, 123);
    EXPECT_EQ(value.m_den, 456);
    EXPECT_EQ(value.m_error, GNC_ERROR_OVERFLOW);
}
