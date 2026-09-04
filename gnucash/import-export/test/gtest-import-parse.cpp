/********************************************************************
 * gtest-import-parse.cpp -- unit tests for import-parse.cpp        *
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

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wcpp"
#include <gtest/gtest.h>
#pragma GCC diagnostic pop

#include <config.h>
#include "import-parse.h"

/* ------------------------------------------------------------------ *
 * gnc_import_test_numeric
 * ------------------------------------------------------------------ */

class ImportTestNumeric : public ::testing::TestWithParam<
    std::tuple<const char*, GncImportFormat>>
{
};

TEST_P (ImportTestNumeric, Cases)
{
    static constexpr auto fmts =
        static_cast<GncImportFormat>(GNCIF_NUM_PERIOD | GNCIF_NUM_COMMA);
    const auto& [str, expected] = GetParam();
    EXPECT_EQ (gnc_import_test_numeric (str, fmts), expected) << "str=" << str;
}

INSTANTIATE_TEST_SUITE_P (Unambiguous, ImportTestNumeric, ::testing::Values(
    std::make_tuple ("123.45", GNCIF_NUM_PERIOD),
    std::make_tuple ("123,45", GNCIF_NUM_COMMA),
    std::make_tuple ("1.182.183,1827", GNCIF_NUM_COMMA),
    std::make_tuple ("1,182,183.1827", GNCIF_NUM_PERIOD)
));

INSTANTIATE_TEST_SUITE_P (Ambiguous, ImportTestNumeric, ::testing::Values(
    /* a plain integer, or a thousands-group without a decimal part, is
     * valid under either convention */
    std::make_tuple ("2000", static_cast<GncImportFormat>(GNCIF_NUM_PERIOD | GNCIF_NUM_COMMA)),
    std::make_tuple ("1,000", static_cast<GncImportFormat>(GNCIF_NUM_PERIOD | GNCIF_NUM_COMMA)),
    std::make_tuple ("1.000", static_cast<GncImportFormat>(GNCIF_NUM_PERIOD | GNCIF_NUM_COMMA)),
    std::make_tuple ("-$1,000", static_cast<GncImportFormat>(GNCIF_NUM_PERIOD | GNCIF_NUM_COMMA)),
    /* a 3-digit thousands-group with no decimal part reads equally well
     * as "one hundred point two hundred seventy-seven thousand" under
     * the other convention's grouping rules */
    std::make_tuple ("100.277", static_cast<GncImportFormat>(GNCIF_NUM_PERIOD | GNCIF_NUM_COMMA)),
    std::make_tuple ("100,277", static_cast<GncImportFormat>(GNCIF_NUM_PERIOD | GNCIF_NUM_COMMA))
));

INSTANTIATE_TEST_SUITE_P (SignsAndCurrency, ImportTestNumeric, ::testing::Values(
    std::make_tuple ("-123.45", GNCIF_NUM_PERIOD),
    std::make_tuple ("+123.45", GNCIF_NUM_PERIOD),
    std::make_tuple ("$123.45", GNCIF_NUM_PERIOD),
    std::make_tuple ("$-123.45", GNCIF_NUM_PERIOD),
    std::make_tuple ("-$123.45", GNCIF_NUM_PERIOD),
    std::make_tuple (" $+2000.00", GNCIF_NUM_PERIOD),
    /* the pattern only allows a leading '$', never a trailing one */
    std::make_tuple ("123.45$", GNCIF_NONE)
));

INSTANTIATE_TEST_SUITE_P (Invalid, ImportTestNumeric, ::testing::Values(
    std::make_tuple ("abc", GNCIF_NONE),
    std::make_tuple ("123.45.67", GNCIF_NONE),
    std::make_tuple ("12,34,5", GNCIF_NONE),
    std::make_tuple ("1-2", GNCIF_NONE)
));

/* ------------------------------------------------------------------ *
 * gnc_import_test_date
 * ------------------------------------------------------------------ */

class ImportTestDate : public ::testing::TestWithParam<
    std::tuple<const char*, GncImportFormat>>
{
};

TEST_P (ImportTestDate, Cases)
{
    static constexpr auto fmts = static_cast<GncImportFormat>(
        GNCIF_DATE_DMY | GNCIF_DATE_MDY | GNCIF_DATE_YMD | GNCIF_DATE_YDM);
    const auto& [str, expected] = GetParam();
    EXPECT_EQ (gnc_import_test_date (str, fmts), expected) << "str=" << str;
}

INSTANTIATE_TEST_SUITE_P (Unambiguous, ImportTestDate, ::testing::Values(
    std::make_tuple ("1999/12/31", GNCIF_DATE_YMD),
    std::make_tuple ("2001-6-17", GNCIF_DATE_YMD),
    std::make_tuple ("20020726", GNCIF_DATE_YMD),
    std::make_tuple ("1999/31/12", GNCIF_DATE_YDM),
    std::make_tuple ("2001-17-6", GNCIF_DATE_YDM),
    std::make_tuple ("1/16/2001", GNCIF_DATE_MDY),
    std::make_tuple ("12-31-1999", GNCIF_DATE_MDY),
    std::make_tuple ("16/1/2001", GNCIF_DATE_DMY),
    std::make_tuple ("31-12-1999", GNCIF_DATE_DMY),
    /* the apostrophe and '.' separators are also accepted, not just '-' and '/' */
    std::make_tuple ("1999.12.31", GNCIF_DATE_YMD),
    std::make_tuple ("1999'12'31", GNCIF_DATE_YMD)
));

INSTANTIATE_TEST_SUITE_P (Ambiguous, ImportTestDate, ::testing::Values(
    std::make_tuple ("99/1/6", static_cast<GncImportFormat>(GNCIF_DATE_YMD | GNCIF_DATE_YDM)),
    std::make_tuple ("1/3/99", static_cast<GncImportFormat>(GNCIF_DATE_DMY | GNCIF_DATE_MDY))
));

INSTANTIATE_TEST_SUITE_P (OutOfRange, ImportTestDate, ::testing::Values(
    /* first field > 31: can't be a day or month, and as a year it's
     * outside [1930, 2100], so nothing survives */
    std::make_tuple ("40/1/2020", GNCIF_NONE),
    /* year (1929) below the accepted [1930, 2100] range */
    std::make_tuple ("1929-01-01", GNCIF_NONE),
    /* year (2101) above the accepted range */
    std::make_tuple ("2101-01-01", GNCIF_NONE),
    /* a single-digit first field can't be a 4-digit year */
    std::make_tuple ("1/2/2020", static_cast<GncImportFormat>(GNCIF_DATE_DMY | GNCIF_DATE_MDY)),
    /* month field (13) out of range for every format that needs a
     * month there */
    std::make_tuple ("13/13/2020", GNCIF_NONE)
));

INSTANTIATE_TEST_SUITE_P (Invalid, ImportTestDate, ::testing::Values(
    std::make_tuple ("not a date", GNCIF_NONE),
    std::make_tuple ("2020", GNCIF_NONE)
));

/* ------------------------------------------------------------------ *
 * gnc_import_parse_date
 * ------------------------------------------------------------------ */

struct YMD { int y; int m; int d; };

class ImportParseDate : public ::testing::TestWithParam<
    std::tuple<const char*, GncImportFormat, YMD>>
{
};

TEST_P (ImportParseDate, Cases)
{
    const auto& [str, fmt, expected] = GetParam();
    time64 val;
    ASSERT_TRUE (gnc_import_parse_date (str, fmt, &val)) << "str=" << str;
    time64 expected_val = gnc_dmy2time64 (expected.d, expected.m, expected.y);
    EXPECT_EQ (val, expected_val) << "str=" << str;
}

INSTANTIATE_TEST_SUITE_P (Formats, ImportParseDate, ::testing::Values(
    std::make_tuple ("1999/12/31", GNCIF_DATE_YMD, YMD{1999, 12, 31}),
    std::make_tuple ("2001-6-17", GNCIF_DATE_YMD, YMD{2001, 6, 17}),
    std::make_tuple ("20020726", GNCIF_DATE_YMD, YMD{2002, 7, 26}),
    std::make_tuple ("1999/31/12", GNCIF_DATE_YDM, YMD{1999, 12, 31}),
    std::make_tuple ("20012311", GNCIF_DATE_YDM, YMD{2001, 11, 23}),
    std::make_tuple ("1/16/2001", GNCIF_DATE_MDY, YMD{2001, 1, 16}),
    std::make_tuple ("01171983", GNCIF_DATE_MDY, YMD{1983, 1, 17}),
    std::make_tuple ("16/1/2001", GNCIF_DATE_DMY, YMD{2001, 1, 16}),
    std::make_tuple ("17011976", GNCIF_DATE_DMY, YMD{1976, 1, 17})
));

/* Y2K fixups performed by the fix_year() helper: two-digit years below
 * 70 are 20xx, otherwise 19xx. (A literal "00" year can't reach fix_year
 * at all - the v2 > 0 format-validation check above rejects it first,
 * same as it always has - so it's not exercised here.)
 */
INSTANTIATE_TEST_SUITE_P (Y2K, ImportParseDate, ::testing::Values(
    std::make_tuple ("01/02/01", GNCIF_DATE_MDY, YMD{2001, 1, 2}),
    std::make_tuple ("01/02/69", GNCIF_DATE_MDY, YMD{2069, 1, 2}),
    std::make_tuple ("01/02/70", GNCIF_DATE_MDY, YMD{1970, 1, 2}),
    std::make_tuple ("01/02/99", GNCIF_DATE_MDY, YMD{1999, 1, 2})
));

TEST (ImportParseDate, InvalidFormatCombination)
{
    time64 val;
    /* out-of-range month for the given format */
    EXPECT_FALSE (gnc_import_parse_date ("13/13/2020", GNCIF_DATE_MDY, &val));
    /* not a date at all */
    EXPECT_FALSE (gnc_import_parse_date ("not a date", GNCIF_DATE_MDY, &val));
}

/* ------------------------------------------------------------------ *
 * gnc_import_parse_numeric
 * ------------------------------------------------------------------ */

TEST (ImportParseNumeric, PeriodAndCommaAgree)
{
    gnc_numeric pval, cval;
    ASSERT_TRUE (gnc_import_parse_numeric ("1,182,183.1827", GNCIF_NUM_PERIOD, &pval));
    ASSERT_TRUE (gnc_import_parse_numeric ("1.182.183,1827", GNCIF_NUM_COMMA, &cval));
    EXPECT_TRUE (gnc_numeric_equal (pval, cval));
}

TEST (ImportParseNumeric, InvalidString)
{
    gnc_numeric val;
    EXPECT_FALSE (gnc_import_parse_numeric ("not a number", GNCIF_NUM_PERIOD, &val));
}
