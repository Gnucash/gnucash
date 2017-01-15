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
    EXPECT_EQ(123, value.m_num);
    EXPECT_EQ(456, value.m_den);
    EXPECT_EQ(GNC_ERROR_OK, value.m_error);
}

TEST(gncrational_constructors, test_implicit_int_constructor)
{
    int num(123), denom(456);
    GncRational value(num, denom);
    EXPECT_EQ(123, value.m_num);
    EXPECT_EQ(456, value.m_den);
    EXPECT_EQ(GNC_ERROR_OK, value.m_error);
}

TEST(gncrational_constructors, test_with_error_code)
{
    int num(123), denom(456);
    GncRational value(num, denom, GNC_ERROR_OVERFLOW);
    EXPECT_EQ(123, value.m_num);
    EXPECT_EQ(456, value.m_den);
    EXPECT_EQ(GNC_ERROR_OVERFLOW, value.m_error);
}

TEST(gncrational_operators, test_addition)
{
    GncRational a(123456789987654321, 1000000000);
    GncRational b(65432198765432198, 100000000);
    GncRational c = a + b;
    EXPECT_EQ (777778777641976301, c.m_num);
    EXPECT_EQ (1000000000, c.m_den);
    EXPECT_EQ (GNC_ERROR_OK, c.m_error);
    a += b;
    EXPECT_EQ (777778777641976301, a.m_num);
    EXPECT_EQ (1000000000, a.m_den);
    EXPECT_EQ (GNC_ERROR_OK, a.m_error);
}

TEST(gncrational_operators, test_subtraction)
{
    GncRational a(123456789987654321, 1000000000);
    GncRational b(65432198765432198, 100000000);
    GncRational c = a - b;
    EXPECT_EQ (530865197666667659, c.m_num);
    EXPECT_FALSE(c.m_num.isNeg());
    EXPECT_EQ (1000000000, c.m_den);
    EXPECT_EQ (GNC_ERROR_OK, c.m_error);
    c = b - a;
    EXPECT_EQ (-530865197666667659, c.m_num);
    EXPECT_TRUE(c.m_num.isNeg());
    EXPECT_EQ (1000000000, c.m_den);
    EXPECT_EQ (GNC_ERROR_OK, c.m_error);
    a -= b;
    EXPECT_EQ (530865197666667659, a.m_num);
    EXPECT_FALSE(a.m_num.isNeg());
    EXPECT_EQ (1000000000, a.m_den);
    EXPECT_EQ (GNC_ERROR_OK, a.m_error);
}

TEST(gncrational_operators, test_multiplication)
{
    GncRational a(123456789987654321, 1000000000);
    GncRational b(65432198765432198, 100000000);
    GncRational c = a * b;
    EXPECT_EQ (GncInt128(UINT64_C(437911925765117),
                         UINT64_C(8081008345983448486)), c.m_num);
    EXPECT_EQ (100000000000000000, c.m_den);
    EXPECT_EQ (GNC_ERROR_OK, c.m_error);
    a *= b;
    EXPECT_EQ (GncInt128(UINT64_C(437911925765117),
                         UINT64_C(8081008345983448486)), a.m_num);
    EXPECT_EQ (100000000000000000, a.m_den);
    EXPECT_EQ (GNC_ERROR_OK, a.m_error);
}

TEST(gncrational_operators, test_division)
{
    GncRational a(123456789987654321, 1000000000);
    GncRational b(65432198765432198, 100000000);
    GncRational c = a / b;
    EXPECT_EQ (GncInt128(UINT64_C(669260), UINT64_C(11059994577585475840)),
               c.m_num);
    EXPECT_EQ (GncInt128(UINT64_C(3547086), UINT64_C(11115994079396609024)),
               c.m_den);
    EXPECT_EQ (GNC_ERROR_OK, c.m_error);

    a /= b;
    EXPECT_EQ (GncInt128(UINT64_C(669260), UINT64_C(11059994577585475840)),
               a.m_num);
    EXPECT_EQ (GncInt128(UINT64_C(3547086), UINT64_C(11115994079396609024)),
               a.m_den);
    EXPECT_EQ (GNC_ERROR_OK, c.m_error);

}
