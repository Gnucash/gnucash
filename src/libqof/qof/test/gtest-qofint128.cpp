/********************************************************************
 * gtest-qofmath128.cpp -- unit tests for the QofInt128 class       *
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
#include "../qofint128.hpp"

TEST(qofint128_constructors, test_default_constructor)
{
    QofInt128 value {};
    EXPECT_EQ (true, value.isZero());
    EXPECT_EQ (false, value.isNeg());
    EXPECT_EQ (false, value.isBig());
    EXPECT_EQ (false, value.isOverflow());
    EXPECT_EQ (false, value.isNan());
}

TEST(qofint128_constructors, test_single_arg_constructor)
{
    QofInt128 value1 (INT64_C(0));
    EXPECT_EQ (true, value1.isZero());
    EXPECT_EQ (false, value1.isNeg());
    EXPECT_EQ (false, value1.isBig());
    EXPECT_EQ (false, value1.isOverflow());
    EXPECT_EQ (false, value1.isNan());

    QofInt128 value2 (INT64_C(567894392130486208));
    EXPECT_EQ (false, value2.isZero());
    EXPECT_EQ (false, value2.isNeg());
    EXPECT_EQ (false, value2.isBig());
    EXPECT_EQ (false, value2.isOverflow());
    EXPECT_EQ (false, value2.isNan());

    QofInt128 value3 (INT64_C(-567894392130486208));
    EXPECT_EQ (false, value3.isZero());
    EXPECT_EQ (true, value3.isNeg());
    EXPECT_EQ (false, value3.isBig());
    EXPECT_EQ (false, value3.isOverflow());
    EXPECT_EQ (false, value3.isNan());

    QofInt128 value4 (UINT64_C(13567894392130486208));
    EXPECT_EQ (false, value4.isZero());
    EXPECT_EQ (false, value4.isNeg());
    EXPECT_EQ (true, value4.isBig());
    EXPECT_EQ (false, value4.isOverflow());
    EXPECT_EQ (false, value4.isNan());
}

TEST(qofint128_constructors, test_double_arg_constructor)
{
    QofInt128 value1 (INT64_C(0), INT64_C(0));
    EXPECT_EQ (true, value1.isZero());
    EXPECT_EQ (false, value1.isNeg());
    EXPECT_EQ (false, value1.isBig());
    EXPECT_EQ (false, value1.isOverflow());
    EXPECT_EQ (false, value1.isNan());

    QofInt128 value2 (INT64_C(0), INT64_C(567894392130486208));
    EXPECT_EQ (false, value2.isZero());
    EXPECT_EQ (false, value2.isNeg());
    EXPECT_EQ (false, value2.isBig());
    EXPECT_EQ (false, value2.isOverflow());
    EXPECT_EQ (false, value2.isNan());

    QofInt128 value3 (INT64_C(567894392130486208), INT64_C(0));
    EXPECT_EQ (false, value3.isZero());
    EXPECT_EQ (false, value3.isNeg());
    EXPECT_EQ (true, value3.isBig());
    EXPECT_EQ (false, value3.isOverflow());
    EXPECT_EQ (false, value3.isNan());

    QofInt128 value4 (INT64_C(567894392130486208), INT64_C(567894392130486208));
    EXPECT_EQ (false, value4.isZero());
    EXPECT_EQ (false, value4.isNeg());
    EXPECT_EQ (true, value4.isBig());
    EXPECT_EQ (false, value4.isOverflow());
    EXPECT_EQ (false, value4.isNan());

    QofInt128 value5 (INT64_C(567894392130486208),
                      INT64_C(-567894392130486208));
    EXPECT_EQ (false, value5.isZero());
    EXPECT_EQ (false, value5.isNeg());
    EXPECT_EQ (true, value5.isBig());
    EXPECT_EQ (false, value5.isOverflow());
    EXPECT_EQ (false, value5.isNan());

    QofInt128 value6 (INT64_C(-567894392130486208),
                      INT64_C(567894392130486208));
    EXPECT_EQ (false, value6.isZero());
    EXPECT_EQ (true, value6.isNeg());
    EXPECT_EQ (true, value6.isBig());
    EXPECT_EQ (false, value6.isOverflow());
    EXPECT_EQ (false, value6.isNan());

    QofInt128 value7 (UINT64_C(13567894392130486208),
                      UINT64_C(13567894392130486208), QofInt128::pos);
    EXPECT_EQ (false, value7.isZero());
    EXPECT_EQ (false, value7.isNeg());
    EXPECT_EQ (true, value7.isBig());
    EXPECT_EQ (false, value7.isOverflow());
    EXPECT_EQ (false, value7.isNan());

    QofInt128 value8 (UINT64_C(13567894392130486208),
                      UINT64_C(13567894392130486208), QofInt128::neg);
    EXPECT_EQ (false, value8.isZero());
    EXPECT_EQ (true, value8.isNeg());
    EXPECT_EQ (true, value8.isBig());
    EXPECT_EQ (false, value8.isOverflow());
    EXPECT_EQ (false, value8.isNan());

    QofInt128 value9 (UINT64_C(13567894392130486208),
                      UINT64_C(13567894392130486208), QofInt128::overflow);
    EXPECT_EQ (false, value9.isZero());
    EXPECT_EQ (false, value9.isNeg());
    EXPECT_EQ (true, value9.isBig());
    EXPECT_EQ (true, value9.isOverflow());
    EXPECT_EQ (false, value9.isNan());

   QofInt128 value10 (UINT64_C(13567894392130486208),
                      UINT64_C(13567894392130486208), QofInt128::NaN);
    EXPECT_EQ (false, value10.isZero());
    EXPECT_EQ (false, value10.isNeg());
    EXPECT_EQ (true, value10.isBig());
    EXPECT_EQ (false, value10.isOverflow());
    EXPECT_EQ (true, value10.isNan());
}

TEST(qofint128_functions, test_int_functions)
{
    int64_t arg {INT64_C(567894392130486208)};
    int64_t narg {INT64_C(-567894392130486208)};
    uint64_t uarg {UINT64_C(13567894392130486208)};
    QofInt128 value1 (INT64_C(0), arg);
    EXPECT_EQ (arg, static_cast<int64_t>(value1));
    EXPECT_EQ (static_cast<uint64_t>(arg), static_cast<uint64_t>(value1));

    QofInt128 value2 (UINT64_C(0), uarg);
    EXPECT_THROW (static_cast<int64_t>(value2), std::overflow_error);
    EXPECT_EQ (uarg, static_cast<uint64_t>(value2));

    QofInt128 value3 (UINT64_C(0), uarg, QofInt128::neg);
    EXPECT_THROW (static_cast<int64_t>(value3), std::underflow_error);
    EXPECT_THROW (static_cast<uint64_t>(value3), std::underflow_error);

    QofInt128 value4 (UINT64_C(0), uarg, QofInt128::overflow);
    EXPECT_THROW (static_cast<int64_t>(value4), std::overflow_error);
    EXPECT_THROW (static_cast<uint64_t>(value4), std::overflow_error);

    QofInt128 value5 (UINT64_C(0), uarg, QofInt128::NaN);
    EXPECT_THROW (static_cast<int64_t>(value5), std::overflow_error);
    EXPECT_THROW (static_cast<uint64_t>(value5), std::overflow_error);

    QofInt128 value6 (INT64_C(1), arg);
    EXPECT_THROW (static_cast<int64_t>(value6), std::overflow_error);
    EXPECT_EQ (arg + (UINT64_C(0x1) << 63), static_cast<uint64_t>(value6));

    QofInt128 value7 (INT64_C(-1), arg);
    EXPECT_EQ (-static_cast<int64_t>((UINT64_C(0x1) << 63) - arg),
               static_cast<int64_t>(value7));
    EXPECT_THROW (static_cast<uint64_t>(value7), std::underflow_error);

    QofInt128 value8 (INT64_C(0), narg);
    EXPECT_EQ (narg, static_cast<int64_t>(value8));
    EXPECT_THROW (static_cast<uint64_t>(value8), std::underflow_error);

    QofInt128 value9 (INT64_C(1), narg);
    EXPECT_EQ (static_cast<int64_t>((UINT64_C(0x1) << 63) + narg),
               static_cast<int64_t>(value9));
    EXPECT_EQ ((UINT64_C(0x1) << 63) + narg, static_cast<uint64_t>(value9));

    QofInt128 value10 (INT64_C(-2), arg);
    EXPECT_THROW (static_cast<int64_t>(value10), std::underflow_error);
    EXPECT_THROW (static_cast<uint64_t>(value10), std::underflow_error);

}

TEST(qofint128_functions, test_compare)
{
    int64_t barg {INT64_C(567894392130486208)};
    int64_t nbarg {INT64_C(-567894392130486208)};
    int64_t sarg {INT64_C(567894392130486207)};
    int64_t nsarg {INT64_C(-567894392130486207)};

    QofInt128 big (barg);
    QofInt128 small (sarg);
    QofInt128 neg_big (nbarg);
    QofInt128 neg_small (nsarg);

    QofInt128 really_big (barg, sarg);
    QofInt128 slightly_bigger (barg, barg);
    QofInt128 not_as_big (sarg, barg);
    QofInt128 a_little_smaller (sarg, sarg);

    QofInt128 neg_really_big (barg, sarg, QofInt128::neg);
    QofInt128 neg_slightly_bigger (barg, barg, QofInt128::neg);
    QofInt128 neg_not_as_big (sarg, barg, QofInt128::neg);
    QofInt128 neg_a_little_smaller (sarg, sarg, QofInt128::neg);

    QofInt128 overflowed (INT64_C(0), INT64_C(0), QofInt128::overflow);
    QofInt128 not_a_number (INT64_C(0), INT64_C(0), QofInt128::NaN);

    EXPECT_EQ (-9, overflowed.cmp (big));
    EXPECT_EQ (-9, not_a_number.cmp (big));
    EXPECT_EQ (9, big.cmp (overflowed));
    EXPECT_EQ (9, big.cmp (not_a_number));

    EXPECT_EQ (-1, neg_big.cmp(big));
    EXPECT_EQ (-1, neg_big.cmp(small));
    EXPECT_EQ (-1, neg_small.cmp(big));
    EXPECT_EQ (-2, neg_big.cmp(neg_small));
    EXPECT_EQ (2, neg_small.cmp(neg_big));

    EXPECT_EQ (-4, small.cmp(big));
    EXPECT_EQ (4, big.cmp(small));
    EXPECT_EQ (1, small.cmp(neg_big));
    EXPECT_EQ (1, big.cmp(neg_small));
    EXPECT_EQ (-5, big.cmp(a_little_smaller));
    EXPECT_EQ (1, big.cmp(neg_a_little_smaller));

    EXPECT_EQ (-4, really_big.cmp(slightly_bigger));
    EXPECT_EQ (5, really_big.cmp(not_as_big));
    EXPECT_EQ (-5, big.cmp(really_big));

    EXPECT_EQ (-1, neg_really_big.cmp(big));
    EXPECT_EQ (-1, neg_really_big.cmp(not_as_big));
    EXPECT_EQ (-1, neg_really_big.cmp(slightly_bigger));
    EXPECT_EQ (-3, neg_really_big.cmp(neg_not_as_big));
    EXPECT_EQ (-3, neg_really_big.cmp(neg_a_little_smaller));
    EXPECT_EQ (2, neg_really_big.cmp(neg_slightly_bigger));
    EXPECT_EQ (3, neg_not_as_big.cmp(neg_really_big));
    EXPECT_EQ (-2, neg_not_as_big.cmp(neg_a_little_smaller));
    EXPECT_EQ (-3, neg_really_big.cmp(neg_a_little_smaller));

    EXPECT_EQ (0, neg_really_big.cmp(QofInt128(barg, sarg, QofInt128::neg)));
    EXPECT_EQ (0, really_big.cmp(QofInt128(barg, sarg)));
    EXPECT_EQ (0, really_big.cmp(QofInt128(barg, sarg,  QofInt128::pos)));

    EXPECT_EQ (0, really_big.cmp(-neg_really_big));
    EXPECT_EQ (0, neg_really_big.cmp(-really_big));
}

TEST(qofint128_functions, stream_output)
{
    int64_t barg {INT64_C(567894392130486208)};
    int64_t sarg {INT64_C(567894392130486207)};
    int64_t nsarg {INT64_C(-567894392130486207)};

    QofInt128 small (sarg);
    QofInt128 neg_small (nsarg);

    QofInt128 really_big (barg, sarg);
    QofInt128 neg_really_big (barg, sarg, QofInt128::neg);

    QofInt128 overflowed (INT64_C(0), INT64_C(0), QofInt128::overflow);
    QofInt128 not_a_number (INT64_C(0), INT64_C(0), QofInt128::NaN);
    QofInt128 boundary_value (UINT64_C(1), UINT64_MAX);

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

TEST(qofint128_functions, add_and_subtract)
{
    /* UINT64_MAX = 18,446,744,073,709,551,615
     * INT64_MAX  =  9,223,372,036,854,775,807
     * barg + sarg = INT64_MAX
     * barg + uarg = UINT64_MAX
     */
    int64_t barg {INT64_C(4878849681579065407)};
    int64_t sarg {INT64_C(4344522355275710400)};
    uint64_t uarg {UINT64_C(13567894392130486208)};

    QofInt128 smallest (sarg + 100);
    QofInt128 smaller (barg + 500);
    QofInt128 small (uarg);
    QofInt128 big (sarg, barg);
    QofInt128 bigger (static_cast<uint64_t>(barg), uarg);
    QofInt128 biggest (uarg, static_cast<uint64_t>(barg));

    EXPECT_EQ (QofInt128(INT64_C(2), INT64_C(499)), small += smaller);
    EXPECT_EQ (QofInt128(uarg), small -= smaller);
    EXPECT_EQ (QofInt128(static_cast<uint64_t>(barg + sarg/2), UINT64_MAX),
               bigger += big);
    EXPECT_EQ (QofInt128(static_cast<uint64_t>(barg), uarg), bigger -= big);
    bigger += biggest;
    EXPECT_EQ (QofInt128(UINT64_MAX, UINT64_MAX), bigger);
    bigger += smallest;
    EXPECT_TRUE (bigger.isOverflow());
    bigger -= biggest;
    EXPECT_TRUE (bigger.isOverflow());
 }

TEST(qofint128_functions, multiply)
{
    int64_t barg {INT64_C(4878849681579065407)};
    int64_t sarg {INT64_C(4344522355275710400)};
    uint64_t uarg {UINT64_C(13567894392130486208)};

    QofInt128 smallest (sarg);
    QofInt128 smaller (barg);
    QofInt128 small (uarg);
    QofInt128 big (sarg, barg);
    QofInt128 bigger (static_cast<uint64_t>(barg), uarg);

    small *= big;
    EXPECT_TRUE (small.isOverflow());
    big *= bigger;
    EXPECT_TRUE (big.isOverflow());
    EXPECT_EQ (QofInt128(UINT64_C(1149052180967758316), UINT64_C(6323251814974894144)), smallest *= smaller);
    EXPECT_FALSE (smallest.isOverflow());

}
