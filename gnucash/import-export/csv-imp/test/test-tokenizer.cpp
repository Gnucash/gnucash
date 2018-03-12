/********************************************************************
 * test-tokenizer.cpp: test suite for the csv tokenizer class and   *
 *                     its child classes.                           *
 * Copyright 2016 Geert Janssens <geert.gnucash@kobaltwit.be>       *
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
 * along with this program; if not, you can retrieve it from        *
 * http://www.gnu.org/licenses/old-licenses/gpl-2.0.html            *
 * or contact:                                                      *
 *                                                                  *
 * Free Software Foundation           Voice:  +1-617-542-5942       *
 * 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652       *
 * Boston, MA  02110-1301,  USA       gnu@gnu.org                   *
 ********************************************************************/


#include <guid.hpp>
#include "../gnc-tokenizer.hpp"
#include "../gnc-tokenizer-csv.hpp"
#include "../gnc-tokenizer-fw.hpp"
#include <gtest/gtest.h>
#include <iostream>
#include <fstream>      // fstream

#include <string>
#include <stdlib.h>     /* getenv */


typedef struct
{
    const char *csv_line;
    uint        num_fields;
    const char *fields [8];
} tokenize_csv_test_data;

typedef struct
{
    const char *fw_line;
    uint        num_fields;
    uint        field_widths[8];
    const char *fields [8];
} tokenize_fw_test_data;

class GncTokenizerTest : public ::testing::Test
{
public:
    GncTokenizerTest()
    {
        fw_tok = gnc_tokenizer_factory(GncImpFileFormat::FIXED_WIDTH);
        csv_tok = gnc_tokenizer_factory(GncImpFileFormat::CSV);
    }

    std::string get_filepath(const std::string& filename);

protected:
    std::string& get_utf8_contents(std::unique_ptr<GncTokenizer> &tokenizer)
    { return tokenizer->m_utf8_contents; }
    void set_utf8_contents(std::unique_ptr<GncTokenizer> &tokenizer, const std::string& newcontents)
    { tokenizer->m_utf8_contents = newcontents; }
    void test_gnc_tokenize_helper (const std::string& separators, tokenize_csv_test_data* test_data); // for csv tokenizer
    void test_gnc_tokenize_helper (tokenize_fw_test_data* test_data); // for csv tokenizer

    std::unique_ptr<GncTokenizer> fw_tok;
    std::unique_ptr<GncTokenizer> csv_tok;
};

std::string GncTokenizerTest::get_filepath(const std::string& filename)
{

    auto srcdir = getenv("SRCDIR");
    if (!srcdir)
        return filename;
    else
        return std::string(srcdir) + "/" + filename;
}

TEST_F (GncTokenizerTest, load_file_nonexisting)
{

    auto file1 = get_filepath ("notexist.csv");

    /* Test loading of a non-existing file */
    EXPECT_THROW (fw_tok->load_file (file1), std::ios_base::failure);
    EXPECT_THROW (csv_tok->load_file (file1), std::ios_base::failure);
}

TEST_F (GncTokenizerTest, load_file_existing)
{

    auto file = get_filepath ("sample1.csv");
    auto expected_contents = std::string(
            "Date,Num,Description,Notes,Account,Deposit,Withdrawal,Balance\n"
            "05/01/15,45,Acme Inc.,,Miscellaneous,,\"1,100.00\",\n");

    ASSERT_NO_THROW (fw_tok->load_file (file))
        << "File " << file << " not found. Perhaps you should set the SRCDIR environment variable to point to its containing directory ?";
    ASSERT_NO_THROW (csv_tok->load_file (file))
        << "File " << file << " not found. Perhaps you should set the SRCDIR environment variable to point to its containing directory ?";

    EXPECT_EQ(expected_contents, get_utf8_contents (fw_tok));
    EXPECT_EQ(expected_contents, get_utf8_contents (csv_tok));
}

TEST_F (GncTokenizerTest, tokenize_from_csv_file)
{

    auto file = get_filepath ("sample1.csv");
    auto expected_contents = std::string(
            "Date,Num,Description,Notes,Account,Deposit,Withdrawal,Balance\n"
            "05/01/15,45,Acme Inc.,,Miscellaneous,,\"1,100.00\",\n");

    csv_tok->load_file (file);
    csv_tok->tokenize();
    auto tokens = csv_tok->get_tokens();
    EXPECT_EQ(2ul, tokens.size());
    EXPECT_EQ(8ul, tokens[0].size());
    EXPECT_EQ(8ul, tokens[1].size());
    EXPECT_EQ(std::string("Date"), tokens.at(0).at(0));
    EXPECT_EQ(std::string("1,100.00"), tokens.at(1).at(6));
}

/* Test parsing for several different prepared strings
 * These tests bypass file loading, rather taking a
 * prepared set of strings as input. This makes it
 * easier to add test cases without having to create new test files
 * each time to load from.
 * Note this bypasses encoding configuration, which should be tested
 * independently.
 */

/* First test whether we're properly catching boost::tokenizer throws
 * This happens when the input data has invalid escape sequences */
TEST_F (GncTokenizerTest, tokenize_binary_data)
{
    GncCsvTokenizer *csvtok = dynamic_cast<GncCsvTokenizer*>(csv_tok.get());
    csvtok->set_separators (",");

    set_utf8_contents (csv_tok, R"(\764Test,Something)");
    EXPECT_THROW (csv_tok->tokenize(), std::range_error);
}

/* This helper function will run the parse step on the given data
 * with the parser as configured by the calling test function.
 * This allows the same code to be used with different csv test strings
 * and parser option combinations.
 */
void
GncTokenizerTest::test_gnc_tokenize_helper (const std::string& separators, tokenize_csv_test_data* test_data)
{

    GncCsvTokenizer *csvtok = dynamic_cast<GncCsvTokenizer*>(csv_tok.get());
    csvtok->set_separators (separators);

    int i = 0;
    while (test_data[i].csv_line)
    {

        tokenize_csv_test_data cur_line = test_data[i];
        set_utf8_contents (csv_tok, std::string(cur_line.csv_line));
        csv_tok->tokenize();

        // The tests only come with one line, so get the first row only
        auto line_tok = csv_tok->get_tokens().front();
        EXPECT_EQ (cur_line.num_fields, line_tok.size());
        for (auto j = 0ul; j < cur_line.num_fields; j++)
        {
            EXPECT_EQ (std::string (cur_line.fields[j]), line_tok[j]);
        }

        i++;
    }
}

static tokenize_csv_test_data comma_separated [] = {
        { "Date,Num,Description,Notes,Account,Deposit,Withdrawal,Balance", 8, { "Date","Num","Description","Notes","Account","Deposit","Withdrawal","Balance" } },
        { "05/01/15,45,Acme Inc.,,Miscellaneous,,\"1,100.00\",", 8, { "05/01/15","45","Acme Inc.","","Miscellaneous","","1,100.00","" } },
        { "05/01/15,45,Acme Inc.,,Miscellaneous,", 6, { "05/01/15","45","Acme Inc.","","Miscellaneous","",NULL,NULL } },
        { NULL, 0, { NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL } },
};

TEST_F (GncTokenizerTest, tokenize_comma_sep)
{
    test_gnc_tokenize_helper (",", comma_separated);
}

static tokenize_csv_test_data semicolon_separated [] = {
        { "Date;Num;Description;Notes;Account;Deposit;Withdrawal;Balance", 8, { "Date","Num","Description","Notes","Account","Deposit","Withdrawal","Balance" } },
        { "05/01/15;45;Acme Inc.;;Miscellaneous;;\"1,100.00\";", 8, { "05/01/15","45","Acme Inc.","","Miscellaneous","","1,100.00","" } },
        { "05/01/15;45;Acme Inc.;;Miscellaneous;", 6, { "05/01/15","45","Acme Inc.","","Miscellaneous","",NULL,NULL } },
        { NULL, 0, { NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL } },
};
TEST_F (GncTokenizerTest, tokenize_semicolon_sep)
{
    test_gnc_tokenize_helper (";", semicolon_separated);
}



void
GncTokenizerTest::test_gnc_tokenize_helper (tokenize_fw_test_data* test_data)
{

    GncFwTokenizer *fwtok = dynamic_cast<GncFwTokenizer*>(fw_tok.get());

    int i = 0;
    while (test_data[i].fw_line)
    {
        tokenize_fw_test_data cur_line = test_data[i];
        set_utf8_contents (fw_tok, std::string(cur_line.fw_line));
        auto columns = std::vector<uint>();
        for (auto j = 0ul; j < cur_line.num_fields; j++)
            columns.push_back (cur_line.field_widths[j]);
        fwtok->columns (columns);
        fw_tok->tokenize();

        // The tests only come with one line, so get the first row only
        auto line_tok = fw_tok->get_tokens().front();
        EXPECT_EQ (cur_line.num_fields, line_tok.size());
        for (auto j = 0ul; j < cur_line.num_fields; j++)
        {
            EXPECT_EQ (std::string (cur_line.fields[j]), line_tok[j]);
        }

        i++;
    }
}

static tokenize_fw_test_data fixed_width [] = {
        { "Date    NumDescriptionAccountDeposit", 5,
                { 8,3,11,7,7,0,0,0 },
                { "Date","Num","Description","Account","Deposit",NULL,NULL,NULL } },
        { "05/01/1545Acme Inc.Miscellaneous1,100.00", 5,
                { 8,2,9,13,8,0,0,0 },
                { "05/01/15","45","Acme Inc.","Miscellaneous","1,100.00",NULL,NULL,NULL } },
        { "05/01/15  45  Acme Inc.Miscellaneous        1,100.00", 6,
                { 10,4,9,13,8,8,0,0 },
                { "05/01/15","45","Acme Inc.","Miscellaneous","","1,100.00",NULL,NULL } },
        { NULL, 0,
                { 0,0,0,0,0,0,0,0 },
                { NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL } },
};

TEST_F (GncTokenizerTest, tokenize_fw)
{
    test_gnc_tokenize_helper (fixed_width);
}
