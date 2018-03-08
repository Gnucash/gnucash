/********************************************************************
 * gtest-gnc-numeric.cpp -- unit tests for the GncNumeric class     *
 * Copyright 2017 John Ralls <jralls@ceridwen.us>                   *
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
\********************************************************************/

#include <gtest/gtest.h>
#include "../gnc-numeric.hpp"
#include "../gnc-rational.hpp"

TEST(gncnumeric_constructors, test_default_constructor)
{
    GncNumeric value;
    EXPECT_EQ(value.num(), 0);
    EXPECT_EQ(value.denom(), 1);
}

TEST(gncnumeric_constructors, test_gnc_rational_constructor)
{
    GncInt128 num128(INT64_C(123456789), INT64_C(567894392130486208));
    GncInt128 den128(INT64_C(456789123), INT64_C(543210987651234567));
    GncRational in (num128, den128);
    GncNumeric out;
    ASSERT_NO_THROW(out = GncNumeric(in));
    EXPECT_EQ(INT64_C(1246404345742679464), out.num());
    EXPECT_EQ(INT64_C(4611686019021985017), out.denom());
}

TEST(gncnumeric_constructors, test_gnc_numeric_constructor)
{
    gnc_numeric input = gnc_numeric_create(123, 456);
    GncNumeric value(input);
    EXPECT_EQ(input.num, value.num());
    EXPECT_EQ(input.denom, value.denom());

    input = gnc_numeric_create(123, 0);
    ASSERT_THROW(GncNumeric throw_val(input), std::invalid_argument);
}

TEST(gncnumeric_constructors, test_int64_constructor)
{
    int64_t num(123), denom(456);
    GncNumeric value(num, denom);
    EXPECT_EQ(123, value.num());
    EXPECT_EQ(456, value.denom());
    denom = INT64_C(0);
    ASSERT_THROW(GncNumeric throw_val(num, denom), std::invalid_argument);
}

TEST(gncnumeric_constructors, test_implicit_int_constructor)
{
    int num(123), denom(456);
    GncNumeric value(num, denom);
    EXPECT_EQ(123, value.num());
    EXPECT_EQ(456, value.denom());
    EXPECT_THROW(GncNumeric throw_val(123, 0), std::invalid_argument);

}

TEST(gncnumeric_constructors, test_double_constructor)
{
    GncNumeric a(123456.789000);
    EXPECT_EQ(123456789, a.num());
    EXPECT_EQ(1000, a.denom());
    GncNumeric b(-123456.789000);
    EXPECT_EQ(-123456789, b.num());
    EXPECT_EQ(1000, b.denom());
    GncNumeric c(static_cast<double>(0.0000000123456789000));
    EXPECT_EQ(123456789, c.num());
    EXPECT_EQ(INT64_C(10000000000000000), c.denom());
    GncNumeric d(1.23456789e-19);
    EXPECT_EQ(0, d.num());
    EXPECT_EQ(1, d.denom());
    EXPECT_THROW(GncNumeric e(123456789e18), std::invalid_argument);
    auto f = GncNumeric(1.1234567890123).convert_sigfigs<RoundType::bankers>(6);
    EXPECT_EQ(112346, f.num());
    EXPECT_EQ(100000, f.denom());
    auto g = GncNumeric(0.011234567890123).convert_sigfigs<RoundType::bankers>(6);
    EXPECT_EQ(112346, g.num());
    EXPECT_EQ(10000000, g.denom());
    auto h = GncNumeric(1123.4567890123).convert_sigfigs<RoundType::bankers>(6);
    EXPECT_EQ(112346, h.num());
    EXPECT_EQ(100, h.denom());
    auto i = GncNumeric(1.1234567890123e-5).convert_sigfigs<RoundType::bankers>(6);
    EXPECT_EQ(112346, i.num());
    EXPECT_EQ(10000000000, i.denom());
}

TEST(gncnumeric_constructors, test_string_constructor)
{
    EXPECT_NO_THROW(GncNumeric simple_num("123/456"));
    GncNumeric simple_num("123/456");
    EXPECT_EQ(123, simple_num.num());
    EXPECT_EQ(456, simple_num.denom());
    EXPECT_NO_THROW(GncNumeric neg_simple_num("-123/456"));
    GncNumeric neg_simple_num("-123/456");
    EXPECT_EQ(-123, neg_simple_num.num());
    EXPECT_EQ(456, neg_simple_num.denom());
    ASSERT_NO_THROW(GncNumeric with_spaces("123 / 456"));
    GncNumeric with_spaces("123 / 456");
    EXPECT_EQ(123, with_spaces.num());
    EXPECT_EQ(456, with_spaces.denom());
    ASSERT_NO_THROW(GncNumeric neg_with_spaces("-123 / 456"));
    GncNumeric neg_with_spaces("-123 / 456");
    EXPECT_EQ(-123, neg_with_spaces.num());
    EXPECT_EQ(456, neg_with_spaces.denom());
    ASSERT_NO_THROW(GncNumeric simple_int("123456"));
    GncNumeric simple_int("123456");
    EXPECT_EQ(123456, simple_int.num());
    EXPECT_EQ(1, simple_int.denom());
    ASSERT_NO_THROW(GncNumeric neg_simple_int("-123456"));
    GncNumeric neg_simple_int("-123456");
    EXPECT_EQ(-123456, neg_simple_int.num());
    EXPECT_EQ(1, neg_simple_int.denom());
    ASSERT_NO_THROW(GncNumeric simple_hex("0x1e240"));
    GncNumeric simple_hex("0x1e240");
    EXPECT_EQ(123456, simple_int.num());
    EXPECT_EQ(1, simple_int.denom());
    ASSERT_NO_THROW(GncNumeric simple_decimal("123.456"));
    GncNumeric simple_decimal("123.456");
    EXPECT_EQ(123456, simple_decimal.num());
    EXPECT_EQ(1000, simple_decimal.denom());
    ASSERT_NO_THROW(GncNumeric neg_simple_decimal("-123.456"));
    GncNumeric neg_simple_decimal("-123.456");
    EXPECT_EQ(-123456, neg_simple_decimal.num());
    EXPECT_EQ(1000, neg_simple_decimal.denom());
    ASSERT_NO_THROW(GncNumeric continental_decimal("123,456"));
    GncNumeric continental_decimal("123,456");
    EXPECT_EQ(123456, continental_decimal.num());
    EXPECT_EQ(1000, continental_decimal.denom());
    ASSERT_NO_THROW(GncNumeric neg_continental_decimal("-123,456"));
    GncNumeric neg_continental_decimal("-123,456");
    EXPECT_EQ(-123456, neg_continental_decimal.num());
    EXPECT_EQ(1000, neg_continental_decimal.denom());
    ASSERT_NO_THROW(GncNumeric hex_rational("0x1e240/0x1c8"));
    GncNumeric hex_rational("0x1e240/0x1c8");
    EXPECT_EQ(123456, hex_rational.num());
    EXPECT_EQ(456, hex_rational.denom());
    ASSERT_NO_THROW(GncNumeric hex_over_num("0x1e240/456"));
    GncNumeric hex_over_num("0x1e240/456");
    EXPECT_EQ(123456, hex_over_num.num());
    EXPECT_EQ(456, hex_over_num.denom());
    ASSERT_NO_THROW(GncNumeric num_over_hex("123456/0x1c8"));
    GncNumeric num_over_hex("123456/0x1c8");
    EXPECT_EQ(123456, num_over_hex.num());
    EXPECT_EQ(456, num_over_hex.denom());
    ASSERT_NO_THROW(GncNumeric embedded("The number is 123456/456"));
    GncNumeric embedded("The number is 123456/456");
    EXPECT_EQ(123456, embedded.num());
    EXPECT_EQ(456, embedded.denom());
    ASSERT_NO_THROW(GncNumeric embedded("The number is -123456/456"));
    GncNumeric neg_embedded("The number is -123456/456");
    EXPECT_EQ(-123456, neg_embedded.num());
    EXPECT_EQ(456, neg_embedded.denom());
    EXPECT_THROW(GncNumeric throw_zero_denom("123/0"), std::invalid_argument);
    EXPECT_THROW(GncNumeric overflow("12345678987654321.123456"),
                 std::overflow_error);
    EXPECT_NO_THROW(GncNumeric overflow("12345678987654321.123456", true));
    GncNumeric overflow("12345678987654321.123456", true);
    EXPECT_EQ(6028163568190586486, overflow.num());
    EXPECT_EQ(488, overflow.denom());
    EXPECT_THROW(GncNumeric auto_round("12345678987654321234/256", true),
                 std::out_of_range);
    EXPECT_THROW(GncNumeric bad_string("Four score and seven"),
                 std::invalid_argument);
}

TEST(gncnumeric_output, string_output)
{
    GncNumeric simple_int(123456, 1);
    EXPECT_EQ("123456", simple_int.to_string());
    GncNumeric neg_simple_int(-123456, 1);
    EXPECT_EQ("-123456", neg_simple_int.to_string());
    GncNumeric decimal_string(123456, 1000);
    EXPECT_EQ("123.456", decimal_string.to_string());
    GncNumeric neg_decimal_string(-123456, 1000);
    EXPECT_EQ("-123.456", neg_decimal_string.to_string());
    GncNumeric rational_string(123, 456);
    EXPECT_EQ("123/456", rational_string.to_string());
    GncNumeric neg_rational_string(-123, 456);
    EXPECT_EQ("-123/456", neg_rational_string.to_string());
}

TEST(gncnumeric_stream, output_stream)
{
    std::ostringstream output;
    GncNumeric simple_int(INT64_C(123456));
    output << simple_int;
    EXPECT_EQ("123456", output.str());
    output.str("");
    GncNumeric decimal_string(123456, 1000);
    output << decimal_string;
    EXPECT_EQ("123.456", output.str());
    output.str("");
    GncNumeric rational_string(123, 456);
    output << rational_string;
    EXPECT_EQ("123/456", output.str());
    try
    {
        auto loc = std::locale("fr_FR.utf8");
        auto thou_sep = std::use_facet<std::numpunct<wchar_t>>(loc).thousands_sep();
        output.imbue(loc);
        output.str("");
        output << simple_int;
        if (thou_sep == L' ')
            EXPECT_EQ("123 456", output.str());
        else
            EXPECT_EQ("123\xe2\x80\xaf""456", output.str());
    }
    catch (std::runtime_error)
    {
        output.imbue(std::locale("fr_FR"));
        output.str("");
        output << simple_int;
        EXPECT_EQ("123456", output.str());
    }
    output.str("");
    output << decimal_string;
    EXPECT_EQ("123,456", output.str());
    output.str("");
    output << rational_string;
    EXPECT_EQ("123/456", output.str());
}

TEST(gncnumeric_stream, input_stream)
{
    std::istringstream input("123456");
    GncNumeric numeric;
    EXPECT_NO_THROW(input >> numeric);
    EXPECT_EQ(123456, numeric.num());
    EXPECT_EQ(1, numeric.denom());
    input.clear();
    input.str("123456/456");
    EXPECT_NO_THROW(input >> numeric);
    EXPECT_EQ(123456, numeric.num());
    EXPECT_EQ(456, numeric.denom());
    input.clear();
    input.str("123456 / 456");
    EXPECT_NO_THROW(input >> numeric);
    EXPECT_EQ(123456, numeric.num());
    EXPECT_EQ(1, numeric.denom());
    input.clear();
    input.str("0x1e240/0x1c8");
    EXPECT_NO_THROW(input >> std::hex >> numeric);
    EXPECT_EQ(123456, numeric.num());
    EXPECT_EQ(456, numeric.denom());
    input.clear();
    input.str("0x1e240/456");
    EXPECT_NO_THROW(input >> numeric);
    EXPECT_EQ(123456, numeric.num());
    EXPECT_EQ(456, numeric.denom());
    input.clear();
    input.str("123456/0x1c8");
    EXPECT_NO_THROW(input >> numeric);
    EXPECT_EQ(123456, numeric.num());
    EXPECT_EQ(456, numeric.denom());
    input.clear();
    input.str("123/0");
    EXPECT_THROW(input >> std::dec >> numeric, std::invalid_argument);
    input.clear();
    input.str("12345678987654321.123456");
    EXPECT_NO_THROW(input >> numeric);
    EXPECT_EQ(6028163568190586486, numeric.num());
    EXPECT_EQ(488, numeric.denom());
    input.clear();
    input.str("12345678987654321234/256");
    EXPECT_THROW(input >> numeric, std::out_of_range);
    input.clear();
    input.str("Four score and seven");
    EXPECT_THROW(input >> numeric, std::invalid_argument);
}

TEST(gncnumeric_operators, gnc_numeric_conversion)
{
    GncNumeric a(123456789, 9876);
    gnc_numeric b = static_cast<decltype(b)>(a);
    EXPECT_EQ(123456789, b.num);
    EXPECT_EQ(9876, b.denom);
}

TEST(gncnumeric_operators, double_conversion)
{
    GncNumeric a(123456789, 9876);
    double b = static_cast<decltype(b)>(a);
    EXPECT_DOUBLE_EQ(12500.687424058324, b);
}

TEST(gncnumeric_operators, test_addition)
{
    GncNumeric a(123456789987654321, 1000000000);
    GncNumeric b(65432198765432198, 100000000);
    GncNumeric c = a + b;
    EXPECT_EQ (777778777641976301, c.num());
    EXPECT_EQ (1000000000, c.denom());
    a += b;
    EXPECT_EQ (777778777641976301, a.num());
    EXPECT_EQ (1000000000, a.denom());
}

TEST(gncnumeric_operators, test_subtraction)
{
    GncNumeric a(123456789987654321, 1000000000);
    GncNumeric b(65432198765432198, 100000000);
    GncNumeric c = a - b;
    EXPECT_EQ (-530865197666667659, c.num());
    EXPECT_EQ (1000000000, c.denom());
    c = b - a;
    EXPECT_EQ (530865197666667659, c.num());
    EXPECT_EQ (1000000000, c.denom());
    a -= b;
    EXPECT_EQ (-530865197666667659, a.num());
    EXPECT_EQ (1000000000, a.denom());
    GncNumeric d(2, 6), e(1, 4);
    c = d - e;
    EXPECT_EQ(1, c.num());
    EXPECT_EQ(12, c.denom());
}

TEST(gncnumeric_operators, test_multiplication)
{
    GncNumeric a(123456789987654321, 1000000000);
    GncNumeric b(65432198765432198, 100000000);
    GncNumeric c = a * b;
    EXPECT_EQ (9208976112412435614, c.num());
    EXPECT_EQ (114, c.denom());
    a *= b;
    EXPECT_EQ (9208976112412435614, a.num());
    EXPECT_EQ (114, a.denom());

    GncNumeric d(215815575996, 269275978715);
    GncNumeric e(1002837599929, 1);
    GncNumeric f, g;
    EXPECT_NO_THROW(f = d * e);
    EXPECT_NO_THROW(g = f.convert<RoundType::half_up>(1));
    GncNumeric h(133499999999, 136499999999);
    GncNumeric i(60000000003, 100000000);
    GncNumeric j;
    EXPECT_NO_THROW(j = gnc_numeric_mul(h, i, 100000000, GNC_HOW_RND_ROUND));
    EXPECT_EQ(58681318684, j.num());

}

TEST(gncnumeric_operators, test_division)
{
    GncNumeric a(123456789987654321, 1000000000);
    GncNumeric b(65432198765432198, 100000000);
    GncNumeric c = a / b;
    EXPECT_EQ (123456789987654321, c.num());
    EXPECT_EQ (654321987654321980, c.denom());

    a /= b;
    EXPECT_EQ (123456789987654321, a.num());
    EXPECT_EQ (654321987654321980, a.denom());

}

TEST(gncnumeric_functions, test_cmp)
{
    GncNumeric a(123456789, 9876), b(567894321, 6543);
    auto c = a;
    EXPECT_EQ(0, a.cmp(c));
    EXPECT_EQ(-1, a.cmp(b));
    EXPECT_EQ(1, b.cmp(a));
    EXPECT_EQ(-1, b.cmp(INT64_C(88888)));
    EXPECT_EQ(1, a.cmp(INT64_C(12500)));
}

TEST(gncnumeric_functions, test_invert)
{
    GncNumeric a(123456789, 9876), b, c;
    ASSERT_NO_THROW(c = b.inv());
    EXPECT_EQ(0, c.num());
    EXPECT_EQ(1, c.denom());
    ASSERT_NO_THROW(b = a.inv());
    EXPECT_EQ(9876, b.num());
    EXPECT_EQ(123456789, b.denom());
}

TEST(gncnumeric_functions, test_reduce)
{
    GncNumeric a(123456789, 5202504), b;
    ASSERT_NO_THROW(b = a.reduce());
    EXPECT_EQ(3607, b.num());
    EXPECT_EQ(152, b.denom());
}

TEST(gncnumeric_functions, test_convert)
{
    GncNumeric a(12345678, 456), b(-12345678, 456), c;
    ASSERT_NO_THROW(c = a.convert<RoundType::never>(456));
    EXPECT_EQ(12345678, c.num());
    EXPECT_EQ(456, c.denom());
    EXPECT_THROW(c = a.convert<RoundType::never>(128), std::domain_error);
    ASSERT_NO_THROW(c = a.convert<RoundType::floor>(128));
    EXPECT_EQ(3465453, c.num());
    EXPECT_EQ(128, c.denom());
    ASSERT_NO_THROW(c = b.convert<RoundType::floor>(128));
    EXPECT_EQ(-3465452, c.num());
    EXPECT_EQ(128, c.denom());
    ASSERT_NO_THROW(c = a.convert<RoundType::ceiling>(128));
    EXPECT_EQ(3465454, c.num());
    EXPECT_EQ(128, c.denom());
    ASSERT_NO_THROW(c = b.convert<RoundType::ceiling>(128));
    EXPECT_EQ(-3465453, c.num());
    EXPECT_EQ(128, c.denom());
    ASSERT_NO_THROW(c = a.convert<RoundType::truncate>(128));
    EXPECT_EQ(3465453, c.num());
    EXPECT_EQ(128, c.denom());
    ASSERT_NO_THROW(c = b.convert<RoundType::truncate>(128));
    EXPECT_EQ(-3465453, c.num());
    EXPECT_EQ(128, c.denom());
    ASSERT_NO_THROW(c = a.convert<RoundType::promote>(128));
    EXPECT_EQ(3465454, c.num());
    EXPECT_EQ(128, c.denom());
    ASSERT_NO_THROW(c = b.convert<RoundType::promote>(128));
    EXPECT_EQ(-3465454, c.num());
    EXPECT_EQ(128, c.denom());
    ASSERT_NO_THROW(c = a.convert<RoundType::half_down>(128));
    EXPECT_EQ(3465453, c.num());
    EXPECT_EQ(128, c.denom());
    ASSERT_NO_THROW(c = a.convert<RoundType::half_down>(114));
    EXPECT_EQ(3086419, c.num());
    EXPECT_EQ(114, c.denom());
    ASSERT_NO_THROW(c = a.convert<RoundType::half_down>(118));
    EXPECT_EQ(3194715, c.num());
    EXPECT_EQ(118, c.denom());
    ASSERT_NO_THROW(c = b.convert<RoundType::half_down>(128));
    EXPECT_EQ(-3465453, c.num());
    EXPECT_EQ(128, c.denom());
    ASSERT_NO_THROW(c = b.convert<RoundType::half_down>(114));
    EXPECT_EQ(-3086419, c.num());
    EXPECT_EQ(114, c.denom());
    ASSERT_NO_THROW(c = b.convert<RoundType::half_down>(121));
    EXPECT_EQ(-3275936, c.num());
    EXPECT_EQ(121, c.denom());
    ASSERT_NO_THROW(c = a.convert<RoundType::half_up>(128));
    EXPECT_EQ(3465453, c.num());
    EXPECT_EQ(128, c.denom());
    ASSERT_NO_THROW(c = a.convert<RoundType::half_up>(114));
    EXPECT_EQ(3086420, c.num());
    EXPECT_EQ(114, c.denom());
    ASSERT_NO_THROW(c = a.convert<RoundType::half_up>(118));
    EXPECT_EQ(3194715, c.num());
    EXPECT_EQ(118, c.denom());
    ASSERT_NO_THROW(c = b.convert<RoundType::half_up>(128));
    EXPECT_EQ(-3465453, c.num());
    EXPECT_EQ(128, c.denom());
    ASSERT_NO_THROW(c = b.convert<RoundType::half_up>(114));
    EXPECT_EQ(-3086420, c.num());
    EXPECT_EQ(114, c.denom());
    ASSERT_NO_THROW(c = b.convert<RoundType::half_up>(121));
    EXPECT_EQ(-3275936, c.num());
    EXPECT_EQ(121, c.denom());
    ASSERT_NO_THROW(c = a.convert<RoundType::bankers>(128));
    EXPECT_EQ(3465453, c.num());
    EXPECT_EQ(128, c.denom());
    ASSERT_NO_THROW(c = a.convert<RoundType::bankers>(114));
    EXPECT_EQ(3086420, c.num());
    EXPECT_EQ(114, c.denom());
    ASSERT_NO_THROW(c = a.convert<RoundType::bankers>(118));
    EXPECT_EQ(3194715, c.num());
    EXPECT_EQ(118, c.denom());
    ASSERT_NO_THROW(c = b.convert<RoundType::bankers>(127));
    EXPECT_EQ(-3438380, c.num());
    EXPECT_EQ(127, c.denom());
    ASSERT_NO_THROW(c = b.convert<RoundType::bankers>(114));
    EXPECT_EQ(-3086420, c.num());
    EXPECT_EQ(114, c.denom());
    ASSERT_NO_THROW(c = b.convert<RoundType::bankers>(121));
    EXPECT_EQ(-3275936, c.num());
    EXPECT_EQ(121, c.denom());
    GncNumeric o(123456789123456789, 128);
    EXPECT_THROW(c = o.convert<RoundType::bankers>(54321098),
                 std::overflow_error);
    GncNumeric d(7, 16);
    ASSERT_NO_THROW(c = d.convert<RoundType::floor>(100));
    EXPECT_EQ(43, c.num());
    EXPECT_EQ(100, c.denom());
    ASSERT_NO_THROW(c = d.convert<RoundType::ceiling>(100));
    EXPECT_EQ(44, c.num());
    EXPECT_EQ(100, c.denom());
    ASSERT_NO_THROW(c = d.convert<RoundType::truncate>(100));
    EXPECT_EQ(43, c.num());
    EXPECT_EQ(100, c.denom());
    ASSERT_NO_THROW(c = d.convert<RoundType::bankers>(100));
    EXPECT_EQ(44, c.num());
    EXPECT_EQ(100, c.denom());
    ASSERT_NO_THROW(c = GncNumeric(1511, 1000).convert<RoundType::bankers>(100));
    EXPECT_EQ(151, c.num());
    EXPECT_EQ(100, c.denom());
    ASSERT_NO_THROW(c = GncNumeric(1516, 1000).convert<RoundType::bankers>(100));
    EXPECT_EQ(152, c.num());
    EXPECT_EQ(100, c.denom());
    ASSERT_NO_THROW(c = GncNumeric(1515, 1000).convert<RoundType::bankers>(100));
    EXPECT_EQ(152, c.num());
    EXPECT_EQ(100, c.denom());
    ASSERT_NO_THROW(c = GncNumeric(1525, 1000).convert<RoundType::bankers>(100));
    EXPECT_EQ(152, c.num());
    EXPECT_EQ(100, c.denom());
    ASSERT_NO_THROW(c = GncNumeric(1535, 1000).convert<RoundType::bankers>(100));
    EXPECT_EQ(154, c.num());
    EXPECT_EQ(100, c.denom());
    ASSERT_NO_THROW(c = GncNumeric(1545, 1000).convert<RoundType::bankers>(100));
    EXPECT_EQ(154, c.num());
    EXPECT_EQ(100, c.denom());
}

TEST(gnc_numeric_functions, test_is_decimal)
{
    EXPECT_TRUE(GncNumeric(123, 1).is_decimal());
    EXPECT_FALSE(GncNumeric(123, 3).is_decimal());
    EXPECT_TRUE(GncNumeric(123, 1000).is_decimal());
    EXPECT_FALSE(GncNumeric(123, 3200).is_decimal());
}

TEST(gnc_numeric_functions, test_conversion_to_decimal)
{
    GncNumeric a(123456789, 1000), r;
    EXPECT_NO_THROW(r = a.to_decimal());
    EXPECT_EQ(123456789, r.num());
    EXPECT_EQ(1000, r.denom());
    EXPECT_THROW(r = a.to_decimal(2), std::range_error);
    GncNumeric b(123456789, 456);
    EXPECT_THROW(r = b.to_decimal(), std::domain_error);
    GncNumeric c(123456789, 450);
    EXPECT_NO_THROW(r = c.to_decimal());
    EXPECT_EQ(27434842, r.num());
    EXPECT_EQ(100, r.denom());
}
