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
#include "../gnc-int128.hpp"

static constexpr uint64_t UPPER_MAX{2305843009213693951};

TEST(GncInt128_constructors, test_default_constructor)
{
    GncInt128 value {};
    EXPECT_TRUE (value.isZero());
    EXPECT_FALSE (value.isNeg());
    EXPECT_FALSE (value.isBig());
    EXPECT_FALSE (value.isOverflow());
    EXPECT_FALSE (value.isNan());
}

TEST(GncInt128_constructors, test_single_arg_constructor)
{
    EXPECT_NO_THROW({
            GncInt128 value1 (INT64_C(0));
            EXPECT_TRUE (value1.isZero());
            EXPECT_FALSE (value1.isNeg());
            EXPECT_FALSE (value1.isBig());
            EXPECT_FALSE (value1.isOverflow());
            EXPECT_FALSE (value1.isNan());
        });

    EXPECT_NO_THROW({
            GncInt128 value2 (INT64_C(567894392130486208));
            EXPECT_FALSE (value2.isZero());
            EXPECT_FALSE (value2.isNeg());
            EXPECT_FALSE (value2.isBig());
            EXPECT_FALSE (value2.isOverflow());
            EXPECT_FALSE (value2.isNan());
        });

    EXPECT_NO_THROW({
            GncInt128 value3 (INT64_C(-567894392130486208));
            EXPECT_FALSE (value3.isZero());
            EXPECT_TRUE (value3.isNeg());
            EXPECT_FALSE (value3.isBig());
            EXPECT_FALSE (value3.isOverflow());
            EXPECT_FALSE (value3.isNan());
        });

    EXPECT_NO_THROW({
            GncInt128 value4 (UINT64_C(13567894392130486208));
            EXPECT_FALSE (value4.isZero());
            EXPECT_FALSE (value4.isNeg());
            EXPECT_TRUE (value4.isBig());
            EXPECT_FALSE (value4.isOverflow());
            EXPECT_FALSE (value4.isNan());
        });
}

TEST(GncInt128_constructors, test_double_arg_constructor)
{
    EXPECT_NO_THROW({
            GncInt128 value1 (INT64_C(0), INT64_C(0));
            EXPECT_TRUE (value1.isZero());
            EXPECT_FALSE (value1.isNeg());
            EXPECT_FALSE (value1.isBig());
            EXPECT_FALSE (value1.isOverflow());
            EXPECT_FALSE (value1.isNan());
        });

    EXPECT_NO_THROW({
            GncInt128 value2 (INT64_C(0), INT64_C(567894392130486208));
            EXPECT_FALSE (value2.isZero());
            EXPECT_FALSE (value2.isNeg());
            EXPECT_FALSE (value2.isBig());
            EXPECT_FALSE (value2.isOverflow());
            EXPECT_FALSE (value2.isNan());
        });

    EXPECT_NO_THROW({
            GncInt128 value3 (INT64_C(567894392130486208), INT64_C(0));
            EXPECT_FALSE (value3.isZero());
            EXPECT_FALSE (value3.isNeg());
            EXPECT_TRUE (value3.isBig());
            EXPECT_FALSE (value3.isOverflow());
            EXPECT_FALSE (value3.isNan());
        });

    EXPECT_NO_THROW({
            GncInt128 value4 (INT64_C(567894392130486208),
                              INT64_C(567894392130486208));
            EXPECT_FALSE (value4.isZero());
            EXPECT_FALSE (value4.isNeg());
            EXPECT_TRUE (value4.isBig());
            EXPECT_FALSE (value4.isOverflow());
            EXPECT_FALSE (value4.isNan());
        });

    EXPECT_NO_THROW({
            GncInt128 value5 (INT64_C(567894392130486208),
                              INT64_C(-567894392130486208));
            EXPECT_FALSE (value5.isZero());
            EXPECT_FALSE (value5.isNeg());
            EXPECT_TRUE (value5.isBig());
            EXPECT_FALSE (value5.isOverflow());
            EXPECT_FALSE (value5.isNan());
        });

    EXPECT_NO_THROW({
            GncInt128 value6 (INT64_C(-567894392130486208),
                              INT64_C(567894392130486208));
            EXPECT_FALSE (value6.isZero());
            EXPECT_TRUE (value6.isNeg());
            EXPECT_TRUE (value6.isBig());
            EXPECT_FALSE (value6.isOverflow());
            EXPECT_FALSE (value6.isNan());
        });

    EXPECT_NO_THROW({
            GncInt128 value7 (UINT64_C(1695986799016310843),
                              UINT64_C(13567894392130486208), GncInt128::pos);
            EXPECT_FALSE (value7.isZero());
            EXPECT_FALSE (value7.isNeg());
            EXPECT_TRUE (value7.isBig());
            EXPECT_FALSE (value7.isOverflow());
            EXPECT_FALSE (value7.isNan());
        });

    EXPECT_NO_THROW({
            GncInt128 value8 (UINT64_C(1695986799016310843),
                              UINT64_C(13567894392130486208), GncInt128::neg);
            EXPECT_FALSE (value8.isZero());
            EXPECT_TRUE (value8.isNeg());
            EXPECT_TRUE (value8.isBig());
            EXPECT_FALSE (value8.isOverflow());
            EXPECT_FALSE (value8.isNan());
        });

    EXPECT_NO_THROW({
            GncInt128 value9 (UINT64_C(1695986799016310843),
                              UINT64_C(13567894392130486208),
                              GncInt128::overflow);
            EXPECT_FALSE (value9.isZero());
            EXPECT_FALSE (value9.isNeg());
            EXPECT_TRUE (value9.isBig());
            EXPECT_TRUE (value9.isOverflow());
            EXPECT_FALSE (value9.isNan());
        });

    EXPECT_NO_THROW({
            GncInt128 value10 (UINT64_C(1695986799016310843),
                               UINT64_C(13567894392130486208), GncInt128::NaN);
            EXPECT_FALSE (value10.isZero());
            EXPECT_FALSE (value10.isNeg());
            EXPECT_TRUE (value10.isBig());
            EXPECT_FALSE (value10.isOverflow());
            EXPECT_TRUE (value10.isNan());
        });

    EXPECT_THROW(GncInt128 value10 (UINT64_C(13567894392130486208),
                                    UINT64_C(13567894392130486208)),
                 std::overflow_error);
}

TEST(GncInt128_functions, test_int_functions)
{
    int64_t arg {INT64_C(567894392130486208)};
    int64_t narg {INT64_C(-567894392130486208)};
    uint64_t uarg {UINT64_C(13567894392130486208)};

    EXPECT_NO_THROW({
            GncInt128 value1 (INT64_C(0), arg);
            EXPECT_EQ (arg, static_cast<int64_t>(value1));
            EXPECT_EQ (static_cast<uint64_t>(arg),
                       static_cast<uint64_t>(value1));
        });

    GncInt128 value2 (UINT64_C(0), uarg);
    EXPECT_THROW (auto v = static_cast<int64_t>(value2), std::overflow_error);
    EXPECT_EQ (uarg, static_cast<uint64_t>(value2));

    GncInt128 value3 (UINT64_C(0), uarg, GncInt128::neg);
    EXPECT_THROW (auto v = static_cast<int64_t>(value3), std::underflow_error);
    EXPECT_THROW (auto v = static_cast<uint64_t>(value3), std::underflow_error);

    GncInt128 value4 (UINT64_C(0), uarg, GncInt128::overflow);
    EXPECT_THROW (auto v = static_cast<int64_t>(value4), std::overflow_error);
    EXPECT_THROW (auto v = static_cast<uint64_t>(value4), std::overflow_error);

    GncInt128 value5 (UINT64_C(0), uarg, GncInt128::NaN);
    EXPECT_THROW (auto v = static_cast<int64_t>(value5), std::overflow_error);
    EXPECT_THROW (auto v = static_cast<uint64_t>(value5), std::overflow_error);

    GncInt128 value6 (INT64_C(1), arg);
    EXPECT_THROW (auto v = static_cast<int64_t>(value6), std::overflow_error);
    EXPECT_EQ (arg + (UINT64_C(0x1) << 63), static_cast<uint64_t>(value6));

    GncInt128 value7 (INT64_C(-1), arg);
    EXPECT_EQ (-static_cast<int64_t>((UINT64_C(0x1) << 63) - arg),
               static_cast<int64_t>(value7));
    EXPECT_THROW (auto v = static_cast<uint64_t>(value7), std::underflow_error);

    GncInt128 value8 (INT64_C(0), narg);
    EXPECT_EQ (narg, static_cast<int64_t>(value8));
    EXPECT_THROW (auto v = static_cast<uint64_t>(value8), std::underflow_error);

    GncInt128 value9 (INT64_C(1), narg);
    EXPECT_EQ (static_cast<int64_t>((UINT64_C(0x1) << 63) + narg),
               static_cast<int64_t>(value9));
    EXPECT_EQ ((UINT64_C(0x1) << 63) + narg, static_cast<uint64_t>(value9));

    GncInt128 value10 (INT64_C(-2), arg);
    EXPECT_THROW (auto v = static_cast<int64_t>(value10), std::underflow_error);
    EXPECT_THROW (auto v = static_cast<uint64_t>(value10),
                  std::underflow_error);

}

TEST(GncInt128_functions, test_compare)
{
    int64_t barg {INT64_C(567894392130486208)};
    int64_t nbarg {INT64_C(-567894392130486208)};
    int64_t sarg {INT64_C(567894392130486207)};
    int64_t nsarg {INT64_C(-567894392130486207)};

    GncInt128 big (barg);
    GncInt128 small (sarg);
    GncInt128 neg_big (nbarg);
    GncInt128 neg_small (nsarg);

    GncInt128 really_big (barg, sarg);
    GncInt128 slightly_bigger (barg, barg);
    GncInt128 not_as_big (sarg, barg);
    GncInt128 a_little_smaller (sarg, sarg);

    GncInt128 neg_really_big (barg, sarg, GncInt128::neg);
    GncInt128 neg_slightly_bigger (barg, barg, GncInt128::neg);
    GncInt128 neg_not_as_big (sarg, barg, GncInt128::neg);
    GncInt128 neg_a_little_smaller (sarg, sarg, GncInt128::neg);

    GncInt128 overflowed (INT64_C(0), INT64_C(0), GncInt128::overflow);
    GncInt128 not_a_number (INT64_C(0), INT64_C(0), GncInt128::NaN);

    EXPECT_EQ (-1, overflowed.cmp (big));
    EXPECT_EQ (-1, not_a_number.cmp (big));
    EXPECT_EQ (1, big.cmp (overflowed));
    EXPECT_EQ (1, big.cmp (not_a_number));

    EXPECT_EQ (-1, neg_big.cmp(big));
    EXPECT_EQ (-1, neg_big.cmp(small));
    EXPECT_EQ (-1, neg_small.cmp(big));
    EXPECT_EQ (-1, neg_big.cmp(neg_small));
    EXPECT_EQ (1, neg_small.cmp(neg_big));

    EXPECT_EQ (-1, small.cmp(big));
    EXPECT_EQ (1, big.cmp(small));
    EXPECT_EQ (1, small.cmp(neg_big));
    EXPECT_EQ (1, big.cmp(neg_small));
    EXPECT_EQ (-1, big.cmp(a_little_smaller));
    EXPECT_EQ (1, big.cmp(neg_a_little_smaller));

    EXPECT_EQ (-1, really_big.cmp(slightly_bigger));
    EXPECT_EQ (1, really_big.cmp(not_as_big));
    EXPECT_EQ (-1, big.cmp(really_big));

    EXPECT_EQ (-1, neg_really_big.cmp(big));
    EXPECT_EQ (-1, neg_really_big.cmp(not_as_big));
    EXPECT_EQ (-1, neg_really_big.cmp(slightly_bigger));
    EXPECT_EQ (-1, neg_really_big.cmp(neg_not_as_big));
    EXPECT_EQ (-1, neg_really_big.cmp(neg_a_little_smaller));
    EXPECT_EQ (1, neg_really_big.cmp(neg_slightly_bigger));
    EXPECT_EQ (1, neg_not_as_big.cmp(neg_really_big));
    EXPECT_EQ (-1, neg_not_as_big.cmp(neg_a_little_smaller));
    EXPECT_EQ (-1, neg_really_big.cmp(neg_a_little_smaller));

    EXPECT_EQ (0, neg_really_big.cmp(GncInt128(barg, sarg, GncInt128::neg)));
    EXPECT_EQ (0, really_big.cmp(GncInt128(barg, sarg)));
    EXPECT_EQ (0, really_big.cmp(GncInt128(barg, sarg,  GncInt128::pos)));

    EXPECT_EQ (0, really_big.cmp(-neg_really_big));
    EXPECT_EQ (0, neg_really_big.cmp(-really_big));
}

TEST(GncInt128_functions, stream_output)
{
    int64_t barg {INT64_C(567894392130486208)};
    int64_t sarg {INT64_C(567894392130486207)};
    int64_t nsarg {INT64_C(-567894392130486207)};

    GncInt128 small (sarg);
    GncInt128 neg_small (nsarg);

    GncInt128 really_big (barg, sarg);
    GncInt128 neg_really_big (barg, sarg, GncInt128::neg);

    GncInt128 overflowed (INT64_C(0), INT64_C(0), GncInt128::overflow);
    GncInt128 not_a_number (INT64_C(0), INT64_C(0), GncInt128::NaN);
    GncInt128 boundary_value (UINT64_C(1), UINT64_MAX);

    static const uint8_t char_buf_size {41};
    char buf[char_buf_size] {};

    EXPECT_STREQ("567894392130486207", small.asCharBufR (buf));
    EXPECT_STREQ("-567894392130486207", neg_small.asCharBufR (buf));
    EXPECT_STREQ("5237901256262967342410779070006542271", really_big.asCharBufR (buf));
    EXPECT_STREQ("-5237901256262967342410779070006542271", neg_really_big.asCharBufR (buf));
    EXPECT_STREQ("36893488147419103231", boundary_value.asCharBufR (buf));
    EXPECT_STREQ("Overflow", overflowed.asCharBufR (buf));
    EXPECT_STREQ("NaN", not_a_number.asCharBufR (buf));
}

TEST(GncInt128_functions, add_and_subtract)
{
    /* UINT64_MAX = 18,446,744,073,709,551,615
     * INT64_MAX  =  9,223,372,036,854,775,807
     * upper leg max = 2,305,843,009,213,693,951
     * barg + marg = INT64_MAX
     * barg + uarg = UINT64_MAX
     * marg + sarg = upper leg max
     */
    int64_t barg {INT64_C(4878849681579065407)};
    int64_t marg {INT64_C(4344522355275710400)};
    int64_t sarg {INT64_C(267163663151677502)};
    uint64_t uarg {UINT64_C(13567894392130486208)};

    GncInt128 smallest (marg + 100);
    GncInt128 smaller (barg + 500);
    GncInt128 small (uarg);
    GncInt128 big (sarg, barg);
    GncInt128 bigger (marg, barg);
    GncInt128 biggest (sarg, static_cast<uint64_t>(barg) + uarg);
    GncInt128 nsmall (UINT64_C(0), uarg, GncInt128::neg);

    EXPECT_EQ (GncInt128(INT64_C(2), INT64_C(499)), small += smaller);
    EXPECT_EQ (GncInt128(INT64_C(2), INT64_C(499), GncInt128::neg),
               nsmall -= smaller);

    EXPECT_EQ (GncInt128(uarg), small -= smaller);
    EXPECT_EQ (GncInt128(UINT64_C(2305843009213693951),
                         UINT64_C(9757699363158130814)),
               bigger += big);
    EXPECT_EQ (GncInt128(marg, barg), bigger -= big);
    bigger += biggest;
    EXPECT_TRUE (bigger.isOverflow());
    bigger -= biggest;
    EXPECT_TRUE (bigger.isOverflow());
 }

TEST(GncInt128_functions, multiply)
{
    int64_t barg {INT64_C(4878849681579065407)};
    int64_t marg {INT64_C(4344522355275710400)};
    int64_t sarg {INT64_C(267163663151677502)};
    uint64_t uarg {UINT64_C(13567894392130486208)};

    GncInt128 smallest (marg);
    GncInt128 smaller (barg);
    GncInt128 small (uarg);
    GncInt128 big (sarg, barg);
    GncInt128 bigger (sarg, uarg);
    GncInt128 nsmaller (-barg);
    GncInt128 nsmallest (-marg);
    GncInt128 tiny (53);
    GncInt128 teensy (37);

    EXPECT_NO_THROW({
            small *= big;
            EXPECT_TRUE (small.isOverflow());
            big *= bigger;
            EXPECT_TRUE (big.isOverflow());
            EXPECT_EQ(1961, tiny * teensy);
            EXPECT_EQ(-1961, -tiny * teensy);
            EXPECT_EQ(-1961, tiny * -teensy);
            EXPECT_EQ(1961, -tiny * -teensy);
            EXPECT_EQ (GncInt128(INT64_C(1149052180967758316),
                                 UINT64_C(6323251814974894144)),
                       smallest * smaller);
            EXPECT_EQ (GncInt128(INT64_C(-1149052180967758316),
                                 UINT64_C(6323251814974894144)),
                       -smallest * smaller);
            EXPECT_EQ (GncInt128(INT64_C(-1149052180967758316),
                                 UINT64_C(6323251814974894144)),
                       smallest * -smaller);
            EXPECT_EQ (GncInt128(INT64_C(1149052180967758316),
                                 UINT64_C(6323251814974894144)),
                       -smallest * -smaller);
            EXPECT_EQ (GncInt128(INT64_C(-1149052180967758316),
                                 UINT64_C(6323251814974894144)),
                       nsmallest * smaller);
            EXPECT_EQ (GncInt128(INT64_C(-1149052180967758316),
                                 UINT64_C(6323251814974894144)),
                       smallest * nsmaller);
            EXPECT_EQ (GncInt128(INT64_C(1149052180967758316),
                                 UINT64_C(6323251814974894144)),
                       nsmallest * nsmaller);
            EXPECT_FALSE (smallest.isOverflow());
        });

}

TEST(GncInt128_functions, divide)
{
    int64_t barg {INT64_C(4878849681579065407)};
    int64_t marg {INT64_C(4344522355275710400)};
    int64_t sarg {INT64_C(267163663151677502)};
    uint64_t uarg {UINT64_C(13567894392130486208)};

    GncInt128 zero (INT64_C(0));
    GncInt128 one (INT64_C(1));
    GncInt128 two (INT64_C(2));
    GncInt128 smallest (marg);
    GncInt128 smaller (barg);
    GncInt128 small (uarg);
    GncInt128 big (sarg, barg);
    GncInt128 bigger (static_cast<uint64_t>(sarg), uarg);
    GncInt128 nsmall = -small;
    GncInt128 nbigger = -bigger;

    EXPECT_EQ (GncInt128(INT64_C(0)), zero /= smallest);
    EXPECT_EQ (GncInt128(INT64_C(0)), zero %= smallest);
    smallest /= zero;
    EXPECT_TRUE (smallest.isNan());

    GncInt128 r {}, q {};

    small.div (smaller, q, r);
    EXPECT_EQ (two, q);
    EXPECT_EQ (GncInt128(INT64_C(3810195028972355394)), r);

    small.div (-smaller, q, r);
    EXPECT_EQ (-two, q);
    EXPECT_EQ (GncInt128(INT64_C(3810195028972355394)), r);

    nsmall.div (smaller, q, r);
    EXPECT_EQ (-two, q);
    EXPECT_EQ (GncInt128(INT64_C(-3810195028972355394)), r);

    nsmall.div (-smaller, q, r);
    EXPECT_EQ (two, q);
    EXPECT_EQ (GncInt128(INT64_C(-3810195028972355394)), r);

    smaller.div (small, q, r);
    EXPECT_EQ (zero, q);
    EXPECT_EQ (smaller, r);

    smaller.div (nsmall, q, r);
    EXPECT_EQ (zero, q);
    EXPECT_TRUE (q.isNeg());
    EXPECT_EQ (smaller, r);

    bigger.div (bigger, q, r);
    EXPECT_EQ (one, q);
    EXPECT_EQ (zero, r);

    bigger.div (one, q, r);
    EXPECT_EQ (bigger, q);
    EXPECT_EQ (zero, r);

    big.div (smaller, q, r);
    EXPECT_EQ (GncInt128(INT64_C(505067796878574019)), q);
    EXPECT_EQ (GncInt128(INT64_C(1659575984014676290)), r);

    bigger.div (big, q, r);
    EXPECT_EQ (two, q);
    EXPECT_EQ (GncInt128(UINT64_C(3810195028972355394)), r);

    bigger.div (-big, q, r);
    EXPECT_EQ (-two, q);
    EXPECT_EQ (GncInt128(UINT64_C(3810195028972355394)), r);

    nbigger.div (-big, q, r);
    EXPECT_EQ (two, q);
    EXPECT_EQ (GncInt128(0, UINT64_C(3810195028972355394), GncInt128::neg), r);

    nbigger.div (-big, q, r);
    EXPECT_EQ (two, q);
    EXPECT_EQ (GncInt128(0, UINT64_C(3810195028972355394), GncInt128::neg), r);

    big.div (bigger, q, r);
    EXPECT_EQ (zero, q);
    EXPECT_EQ (big, r);

    big.div (nbigger, q, r);
    EXPECT_EQ (zero, q);
    EXPECT_TRUE (q.isNeg());
    EXPECT_EQ (big, r);

    big.div (big - 1, q, r);
    EXPECT_EQ(one, q);
    EXPECT_EQ(one, r);

    EXPECT_EQ (big, big %= bigger);
    EXPECT_EQ (two, bigger /= big);
    EXPECT_NO_THROW({
            GncInt128 a(2, INT64_C(5501995774277214867));
            GncInt128 b(0, INT64_C(2086443244332180413));
            GncInt128 c = a / b;
            EXPECT_EQ(11, c);
            EXPECT_FALSE(c.isBig());
            GncInt128 d(-2, INT64_C(5501995774277214867));
            GncInt128 e(0, INT64_C(1995728320665563874));
            c = d / e;
            EXPECT_EQ(-15, c);
            EXPECT_FALSE(c.isBig());
            GncInt128 f(2, INT64_C(-5501995774277214867));
            GncInt128 g(0, INT64_C(2086443244332180413));
            c = f / g;
            EXPECT_EQ(15, c);
            EXPECT_FALSE(c.isBig());
            GncInt128 h(2, INT64_C(5501995774277214867));
            GncInt128 i(0, INT64_C(-2086443244332180413));
            c = h / i;
            EXPECT_EQ(-11, c);
            EXPECT_FALSE(c.isBig());
            GncInt128 j(-2, INT64_C(-5501995774277214867));
            GncInt128 k(0, INT64_C(2086443244332180413));
            c = j / k;
            EXPECT_EQ(-11, c);
            EXPECT_FALSE(c.isBig());
            GncInt128 l(-2, INT64_C(-5501995774277214867));
            GncInt128 m(0, INT64_C(-2086443244332180413));
            c = l / m;
            EXPECT_EQ(11, c);
            EXPECT_FALSE(c.isBig());
            GncInt128 n(2, INT64_C(5501995774277214867), GncInt128::neg);
            GncInt128 o(0, INT64_C(2086443244332180413));
            c = n / o;
            EXPECT_EQ(-11, c);
            EXPECT_FALSE(c.isBig());
            GncInt128 p(2, INT64_C(5501995774277214867));
            GncInt128 q(0, INT64_C(2086443244332180413), GncInt128::neg);
            c = p / q;
            EXPECT_EQ(-11, c);
            EXPECT_FALSE(c.isBig());
            GncInt128 r(2, INT64_C(5501995774277214867), GncInt128::neg);
            GncInt128 s(0, INT64_C(2086443244332180413), GncInt128::neg);
            c = r / s;
            EXPECT_EQ(11, c);
            EXPECT_FALSE(c.isBig());
      });
}

TEST(GncInt128_functions, GCD)
{
    int64_t barg {INT64_C(4878849681579065407)};
    int64_t marg {INT64_C(4344522355275710400)};
    int64_t sarg {INT64_C(267163663151677502)};
    uint64_t uarg {UINT64_C(13567894392130486208)};

    GncInt128 one (INT64_C(1));
    GncInt128 smallest (sarg);
    GncInt128 smaller (marg);
    GncInt128 small (barg);

    EXPECT_NO_THROW({
            GncInt128 big = smaller * smallest;
            GncInt128 bigger = small * smaller;

            EXPECT_EQ (smaller, big.gcd(smaller));
            EXPECT_EQ (smallest, big.gcd(smallest));
            EXPECT_EQ (small, bigger.gcd(small));
            EXPECT_EQ (smaller, bigger.gcd(smaller));
            EXPECT_EQ (one, big.gcd (small));
            EXPECT_EQ (2, bigger.gcd (smallest));
            EXPECT_EQ (big >> 1, smaller.lcm (smallest));
        });
}

TEST(GncInt128_functions, pow)
{

    int64_t sarg {INT64_C(53309)};
    int64_t barg {INT64_C(4878849681579065407)};
    GncInt128 little (sarg);
    GncInt128 big (barg);
    auto minus = -little;

    EXPECT_EQ (GncInt128(1), little.pow(0));
    EXPECT_EQ (GncInt128(0), GncInt128(0).pow(123));
    auto really_big = big * big;
    EXPECT_EQ (big * big, big.pow(2));
    EXPECT_EQ (GncInt128(UINT64_C(66326033898754),
                         UINT64_C(10251549987585143605)), little.pow(7));
    EXPECT_EQ (GncInt128(UINT64_C(66326033898754),
                         UINT64_C(10251549987585143605), GncInt128::neg),
               minus.pow(7));
    auto over = minus.pow(9);
    EXPECT_TRUE(over.isOverflow());
}

TEST(GncInt128_functions, shift)
{
    GncInt128 a (UINT64_C(0xabcdabcd), UINT64_C(0xfe89fe89fe89fe89), 0);
    EXPECT_EQ(GncInt128(UINT64_C(0xabcdabcdfe89), UINT64_C(0xfe89fe89fe890000), 0), a << 16);
    EXPECT_EQ(GncInt128(UINT64_C(0xabcd), UINT64_C(0xabcdfe89fe89fe89), 0), a >> 16);
    EXPECT_EQ(GncInt128(UINT64_C(0x1e89fe89fe89fe89), UINT64_C(0), 0), a << 64);
    EXPECT_EQ(GncInt128(UINT64_C(0), UINT64_C(0xabcdabcd), 0), a >> 64);
    GncInt128 b (UINT64_C(0xabcdabcd), UINT64_C(0xfe89fe89fe89fe89), 1);
    EXPECT_EQ(GncInt128(UINT64_C(0xabcdabcdfe89), UINT64_C(0xfe89fe89fe890000), 1), b << 16);
    EXPECT_EQ(GncInt128(UINT64_C(0xabcd), UINT64_C(0xabcdfe89fe89fe89), 1), b >> 16);
    EXPECT_EQ(GncInt128(UINT64_C(0x1e89fe89fe89fe89), UINT64_C(0), 1), b << 64);
    EXPECT_EQ(GncInt128(UINT64_C(0), UINT64_C(0xabcdabcd), 1), b >> 64);
}
