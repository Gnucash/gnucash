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
#include <random>
#include "../gnc-rational.hpp"
#include "../gnc-numeric.hpp" //for RoundType

TEST(gncrational_constructors, test_default_constructor)
{
    EXPECT_NO_THROW({
            GncRational value;
            EXPECT_EQ(value.num(), 0);
            EXPECT_EQ(value.denom(), 1);
        });
}

TEST(gncrational_constructors, test_gnc_numeric_constructor)
{
    gnc_numeric input = gnc_numeric_create(123, 456);
    EXPECT_NO_THROW({
            GncRational value(input);
            EXPECT_EQ(input.num, value.num());
            EXPECT_EQ(input.denom, value.denom());
        });
}

TEST(gncrational_constructors, test_gnc_int128_constructor)
{
    GncInt128 num(123), denom(456);
    EXPECT_NO_THROW({
            GncRational value(num, denom);
            EXPECT_EQ(123, value.num());
            EXPECT_EQ(456, value.denom());
        });
}

TEST(gncrational_constructors, test_implicit_int_constructor)
{
    int num(123), denom(456);
    EXPECT_NO_THROW({
            GncRational value(num, denom);
            EXPECT_EQ(123, value.num());
            EXPECT_EQ(456, value.denom());
        });
}

TEST(gncrational_operators, test_addition)
{
    EXPECT_NO_THROW({
            GncRational a(123456789987654321, 1000000000);
            GncRational b(65432198765432198, 100000000);
            GncRational c = a + b;
            EXPECT_EQ (777778777641976301, c.num());
            EXPECT_EQ (1000000000, c.denom());
            a += b;
            EXPECT_EQ (777778777641976301, a.num());
            EXPECT_EQ (1000000000, a.denom());
        });
}

TEST(gncrational_operators, test_subtraction)
{
    EXPECT_NO_THROW({
            GncRational a(123456789987654321, 1000000000);
            GncRational b(65432198765432198, 100000000);
            GncRational c = a - b;
            EXPECT_EQ (-530865197666667659, c.num());
            EXPECT_TRUE(c.num().isNeg());
            EXPECT_EQ (1000000000, c.denom());
            c = b - a;
            EXPECT_EQ (530865197666667659, c.num());
            EXPECT_FALSE(c.num().isNeg());
            EXPECT_EQ (1000000000, c.denom());
            a -= b;
            EXPECT_EQ (-530865197666667659, a.num());
            EXPECT_TRUE(a.num().isNeg());
            EXPECT_EQ (1000000000, a.denom());
        });
}

TEST(gncrational_operators, test_multiplication)
{
    EXPECT_NO_THROW({
            GncRational a(123456789987654321, 1000000000);
            GncRational b(65432198765432198, 100000000);
            GncRational c = a * b;
            EXPECT_EQ (GncInt128(UINT64_C(437911925765117),
                                 UINT64_C(8081008345983448486)), c.num());
            EXPECT_EQ (100000000000000000, c.denom());
            a *= b;
            EXPECT_EQ (GncInt128(UINT64_C(437911925765117),
                                 UINT64_C(8081008345983448486)), a.num());
            EXPECT_EQ (100000000000000000, a.denom());
        });
}

TEST(gncrational_operators, test_division)
{
    EXPECT_NO_THROW({
            GncRational a(123456789987654321, 1000000000);
            GncRational b(65432198765432198, 100000000);
            GncRational c = a / b;
            EXPECT_EQ (GncInt128(UINT64_C(669260),
                                 UINT64_C(11059994577585475840)), c.num());
            EXPECT_EQ (GncInt128(UINT64_C(3547086),
                                 UINT64_C(11115994079396609024)), c.denom());
            a /= b;
            EXPECT_EQ (GncInt128(UINT64_C(669260),
                                 UINT64_C(11059994577585475840)), a.num());
            EXPECT_EQ (GncInt128(UINT64_C(3547086),
                                 UINT64_C(11115994079396609024)), a.denom());
        });
}

static bool
rounding_predicate(GncInt128 expected, GncInt128 result)
{
    static const uint64_t threshold{0x1fffffffffffff};

    if (expected < threshold)
        return expected == result;
    auto difference = expected - result;
    if (difference >= -1 && difference <= 1)
        return true;
    return false;
}

TEST(gncrational_functions, test_round_to_numeric)
{
    std::default_random_engine dre;
    std::uniform_int_distribution<int64_t> di{INT64_C(0x1000000000000),
            INT64_C(0x7ffffffffffffff)};
    static const int reps{100};
    for (auto i = 0; i < reps; ++i)
    {
        GncRational a(di(dre), di(dre));
        GncRational b(di(dre), 100);
        try {
            auto c = a * b;
            auto expected = c;
            expected = expected.convert<RoundType::bankers>(100);
            auto rounded = c.round_to_numeric();
            rounded = rounded.convert<RoundType::bankers>(100);
            if (rounded.is_big())
            {
                --i;
                continue;
            }
            EXPECT_PRED2(rounding_predicate, expected.num(), rounded.num());
            EXPECT_TRUE(rounded.valid());
        }
        catch (const std::overflow_error& err)
        {
            std::cerr << "Overflow error from " << a << " * " << b << ".\n";
        }

    }
}
