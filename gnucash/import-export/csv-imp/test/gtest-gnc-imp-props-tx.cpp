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
