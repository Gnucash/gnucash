/********************************************************************\
 * test-icu-locale.cpp -- Unit tests for GncQuotes            *
 *                                                                  *
 * Copyright 2025 John Ralls <jralls@ceridwen.us>                   *
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

#include <gnc-unicode.h>
#include <stdbool.h>
#include <gtest/gtest.h>

TEST(GncUnicode, test_ss_base_chars)
{
    int pos = 0, len = 0;

    auto result = gnc_unicode_has_substring_base_chars("besi", "Necklace for Bessie",
                                         &pos, &len);
    EXPECT_FALSE(result);
    EXPECT_EQ(0, pos);
    EXPECT_EQ(0, len);

    result = gnc_unicode_has_substring_base_chars("bessi", "Necklace for Bessie",
                                         &pos, &len);
    EXPECT_TRUE(result);
    EXPECT_EQ(13, pos);
    EXPECT_EQ(5, len);
    }
TEST(GncUnicode, test_ss_accented)
{
    int pos = 0, len = 0;
    auto result = gnc_unicode_has_substring_accented_chars("bessi", "Necklace for Bessie",
                                         &pos, &len);
    EXPECT_TRUE(result);
    EXPECT_EQ(13, pos);
    EXPECT_EQ(5, len);
}

TEST(GncUnicode, test_ss_accented_case_sensitive)
{
    int pos = 0, len = 0;
    auto result = gnc_unicode_has_substring_accented_case_sensitive("bessi", "Necklace for Bessie",
                                         &pos, &len);
    EXPECT_FALSE(result);
    EXPECT_EQ(0, pos);
    EXPECT_EQ(0, len);

    result = gnc_unicode_has_substring_accented_case_sensitive("Bessi", "Necklace for Bessie",
                                         &pos, &len);
    EXPECT_TRUE(result);
    EXPECT_EQ(13, pos);
    EXPECT_EQ(5, len);
}

TEST(GncUnicode, test_german_ss_literal)
{
    int pos = 0, len = 0;
    auto result = gnc_unicode_has_substring_base_chars("be\xc3\x9fi", "Necklace for Be\xc3\x9fie",
                                         &pos, &len);
    EXPECT_TRUE(result);
    EXPECT_EQ(13, pos);
    EXPECT_EQ(4, len);

    pos = 0, len = 0;
    result = gnc_unicode_has_substring_accented_chars("be\xc3\x9fi", "Necklace for Be\xc3\x9fie",
                                         &pos, &len);
    EXPECT_TRUE(result);
    EXPECT_EQ(13, pos);
    EXPECT_EQ(4, len);

    pos = 0, len = 0;
    result = gnc_unicode_has_substring_accented_case_sensitive("be\xc3\x9fi", "Necklace for Be\xc3\x9fie",
                                         &pos, &len);
    EXPECT_FALSE(result);
    EXPECT_EQ(0, pos);
    EXPECT_EQ(0, len);

    result = gnc_unicode_has_substring_accented_case_sensitive("Be\xc3\x9fi", "Necklace for Be\xc3\x9fie",
                                         &pos, &len);
    EXPECT_TRUE(result);
    EXPECT_EQ(13, pos);
    EXPECT_EQ(4, len);
}

TEST(GncUnicode, test_german_ss_decorated_base_chars_nocap)
{
    int pos = 0, len = 0;
    auto result = gnc_unicode_has_substring_base_chars("bessi", "Necklace for Be\xc3\x9fie",
                                         &pos, &len);
    EXPECT_TRUE(result);
    EXPECT_EQ(13, pos);
    EXPECT_EQ(4, len);
}

TEST(GncUnicode, test_german_ss_decorated_accented_cap)
{
    int pos = 0, len = 0;
    auto result = gnc_unicode_has_substring_accented_case_sensitive("bessi", "Necklace for Be\xc3\x9fie",
                                         &pos, &len);
    EXPECT_FALSE(result);
    EXPECT_EQ(0, pos);
    EXPECT_EQ(0, len);

    result = gnc_unicode_has_substring_accented_case_sensitive("Bessi", "Necklace for Be\xc3\x9fie",
                                         &pos, &len);
    EXPECT_FALSE(result);
    EXPECT_EQ(0, pos);
    EXPECT_EQ(0, len);
    }

TEST(GncUnicode, test_german_ss_decorated_accented_nocap)
{
    int pos = 0, len = 0;
    auto result = gnc_unicode_has_substring_accented_chars("bessi", "Necklace for Be\xc3\x9fie",
                                         &pos, &len);
    EXPECT_FALSE(result);
    EXPECT_EQ(0, pos);
    EXPECT_EQ(0, len);
}

TEST (GncUnicode, test_simple_identical)
{
    for (int i = 0; i < 5000000; ++i)
    {
        EXPECT_EQ (gnc_unicode_compare_identical ("alice", "alice"), 0);
        EXPECT_EQ (gnc_unicode_compare_identical ("alice", "bob"), -1);
        EXPECT_EQ (gnc_unicode_compare_identical ("bob", "alice"), 1);
    }
}
