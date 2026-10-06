/********************************************************************\
 * gtest-ui-util.cpp -- Unit tests for gnc-ui-util.cpp              *
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
#include <gtest/gtest.h>
#include <glib.h>
#include <cstdint>
#include <memory>
#include <ostream>
#include <string>
#include "../gnc-ui-util.cpp"

struct WordsCase
{
    int64_t     num, denom;
    const char* expected;
};

static std::string
words (int64_t num, int64_t denom)
{
    std::unique_ptr<char, void (*)(gpointer)> p {numeric_to_words (gnc_numeric_create (num, denom)), g_free};
    return p ? std::string {p.get()} : std::string {"<null>"};
}

class NumberToWords : public ::testing::TestWithParam<WordsCase> {};

TEST_P (NumberToWords, SpellsAmount)
{
    const auto& c = GetParam();
    EXPECT_EQ (words (c.num, c.denom), c.expected);
}

INSTANTIATE_TEST_SUITE_P (
    Amounts, NumberToWords,
    ::testing::Values (
        WordsCase {0, 100,              "zero and 00/100"},
        WordsCase {2000, 100,           "Twenty and 00/100"},
        WordsCase {7000, 30,            "Two Hundred Thirty Three and 10/30"},
        WordsCase {40,   80,            "zero and 40/80"},
        WordsCase {0,   300,            "zero and 00/300"},
        WordsCase {900, 300,            "Three and 00/300"},
        WordsCase {8,     8,            "One and 0/8"},
        WordsCase {2500, 12,            "Two Hundred Eight and 4/12"},
        WordsCase {2100, 100,           "Twenty One and 00/100"},
        WordsCase {10000, 100,          "One Hundred and 00/100"},
        WordsCase {100000, 100,         "One Thousand and 00/100"},
        WordsCase {100100, 100,         "One Thousand One and 00/100"},
        /* old infinite-loop case: just below 10^6 */
        WordsCase {99999900, 100,       "Nine Hundred Ninety Nine Thousand Nine Hundred Ninety Nine and 00/100"},
        /* old int-overflow case */
        WordsCase {1000000000000, 1,    "One Trillion"},
        WordsCase {INT64_MAX, 1,        "Nine Quintillion Two Hundred Twenty Three Quadrillion Three Hundred Seventy Two Trillion Thirty Six Billion Eight Hundred Fifty Four Million Seven Hundred Seventy Five Thousand Eight Hundred Seven"},
        WordsCase {INT64_MIN, 1,        "Nine Quintillion Two Hundred Twenty Three Quadrillion Three Hundred Seventy Two Trillion Thirty Six Billion Eight Hundred Fifty Four Million Seven Hundred Seventy Five Thousand Eight Hundred Eight"}
    ));
