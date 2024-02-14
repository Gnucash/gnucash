/********************************************************************\
 * gtest-gnc-imp-props-tx.cpp - Tests using fixture                 *
 *                              GncImpPropsTxTest                   *
 *                                                                  *
 * Copyright (c) 2015 Geert Janssens <geert@kobaltwit.be>           *
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

#include <config.h>

#include <gnc-datetime.hpp>

#include <gnc-imp-props-tx.hpp>
#include <qofbook.h>
#include <engine-helpers.h>
#include <gnc-ui-util.h>

// Test fixture for tests without bayesian matching
class GncImpPropsTxTest : public testing::Test
{
protected:
    void SetUp()
    {
    }

    void TearDown()
    {
    }

};



/* Tests using fixture GncImpPropsTxTest */

extern GncNumeric parse_monetary (const std::string &str, int currency_format);

//! Test for function parse_monetary (const std::string &str, int currency_format)
TEST_F(GncImpPropsTxTest, ParseMonetary)
{

    using namespace testing;

    /* Empty string */
    auto value = parse_monetary ("", 1);
    EXPECT_EQ (value, (GncNumeric {0, 1}));

    /* No digits in string */
    EXPECT_THROW (parse_monetary ("abc", 1), std::invalid_argument);

    /* Tests using currency_format "Period" (1) */
    value = parse_monetary ("1,000.00", 1);
    EXPECT_EQ (value, (GncNumeric {100000, 100}));
    value = parse_monetary ("-1,001.00", 1);
    EXPECT_EQ (value, (GncNumeric {-100100, 100}));
    value = parse_monetary ("-1,002.00$", 1);
    EXPECT_EQ (value, (GncNumeric {-100200, 100}));
    // 798334 - Importing transactions from CSV with space as thousand separator
    value = parse_monetary ("1 000 003.00", 1);
    EXPECT_EQ (value, (GncNumeric {100000300, 100}));
    auto input = "1\u202F000\u202F004.00"; // Using Polish thousand separator
    value = parse_monetary (input, 1);
    EXPECT_EQ (value, (GncNumeric {100000400, 100}));
    // Bug 798572 - Parse numbers with two minus signs as a positive numbers
    value = parse_monetary ("--1,005.00", 1);
    EXPECT_EQ (value, (GncNumeric {100500, 100}));

    /* Tests using currency_format "Comma" (2) */
    value = parse_monetary ("2.000,00", 2);
    EXPECT_EQ (value, (GncNumeric {200000, 100}));
    // 798334 - Importing transactions from CSV with space as thousand separator
    value = parse_monetary ("2 000 001,00", 2);
    EXPECT_EQ (value, (GncNumeric {200000100, 100}));
    input = "2\u202F000\u202F002,00"; // Using Polish thousand separator
    value = parse_monetary (input, 2);
    EXPECT_EQ (value, (GncNumeric {200000200, 100}));

    /* Things that will throw */
    EXPECT_THROW (parse_monetary ("3000.00.01", 1), std::invalid_argument);
};
