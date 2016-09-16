/********************************************************************
 * test-tx-import.cpp: test suite for the GncTxImport class.        *
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
#include "../gnc-csv-tokenizer.hpp"
#include "../gnc-fw-tokenizer.hpp"
#include <gtest/gtest.h>
#include <iostream>
#include <fstream>      // fstream

#include <string>
#include <stdlib.h>     /* getenv */

/* Add specific headers for this class */
#include "../gnc-tx-import.hpp"

//typedef struct
//{
//    GncTxImport* parse_data;
//} Fixture;

typedef struct
{
    int          date_fmt;
    const gchar *date_str;
    int          exp_year;
    int          exp_month;
    int          exp_day;
} parse_date_data;

class GncTxImportTest : public ::testing::Test
{
public:
    GncTxImportTest()
    {
        tx_importer = std::unique_ptr<GncTxImport>(new GncTxImport);
    }


protected:
    std::unique_ptr<GncTxImport> tx_importer;
};


/* parse_date
time64 parse_date (const char* date_str, int format)// C: 14 in 7 SCM: 9 in 2 Local: 1:0:0
*/
TEST(GncTxImportTest, parse_date)
{
    time64 rawtime = gnc_time (NULL);
    struct tm *tm = gnc_gmtime (&rawtime);
    int curr_year = tm->tm_year;


    /* Note: tm_year = year - 1900 and tm_mon = 0-11
     * I'm writing the expected values as subtractions for easier
     * comparison with the date string under test
     */
    parse_date_data test_dates[] =
    {
        // supported combinations  -/.'
        { 0, "2013-08-01", 2013 - 1900,  8 - 1,  1},
        { 0,  "2013-8-01", 2013 - 1900,  8 - 1,  1},
        { 0,  "2013-08-1", 2013 - 1900,  8 - 1,  1},
        { 0,   "2013-8-1", 2013 - 1900,  8 - 1,  1},
        { 0,   "13-08-01", 2013 - 1900,  8 - 1,  1},
        { 0,    "13-8-01", 2013 - 1900,  8 - 1,  1},
        { 0,    "13-08-1", 2013 - 1900,  8 - 1,  1},
        { 0,     "13-8-1", 2013 - 1900,  8 - 1,  1},
        { 0, "2009/11/04", 2009 - 1900, 11 - 1,  4},
        { 0,  "1985.3.12", 1985 - 1900,  3 - 1, 12},
        { 0,      "3'6'8", 2003 - 1900,  6 - 1,  8},
        { 0,   "20130801", 2013 - 1900,  8 - 1,  1},
        { 1, "01-08-2013", 2013 - 1900,  8 - 1,  1},
        { 1,  "01-8-2013", 2013 - 1900,  8 - 1,  1},
        { 1,  "1-08-2013", 2013 - 1900,  8 - 1,  1},
        { 1,   "1-8-2013", 2013 - 1900,  8 - 1,  1},
        { 1,   "01-08-13", 2013 - 1900,  8 - 1,  1},
        { 1,    "01-8-13", 2013 - 1900,  8 - 1,  1},
        { 1,    "1-08-13", 2013 - 1900,  8 - 1,  1},
        { 1,     "1-8-13", 2013 - 1900,  8 - 1,  1},
        { 1, "04/11/2009", 2009 - 1900, 11 - 1,  4},
        { 1,  "12.3.1985", 1985 - 1900,  3 - 1, 12},
        { 1,      "8'6'3", 2003 - 1900,  6 - 1,  8},
        { 1,   "01082013", 2013 - 1900,  8 - 1,  1},
        { 2, "08-01-2013", 2013 - 1900,  8 - 1,  1},
        { 2,  "8-01-2013", 2013 - 1900,  8 - 1,  1},
        { 2,  "08-1-2013", 2013 - 1900,  8 - 1,  1},
        { 2,   "8-1-2013", 2013 - 1900,  8 - 1,  1},
        { 2,   "08-01-13", 2013 - 1900,  8 - 1,  1},
        { 2,    "8-01-13", 2013 - 1900,  8 - 1,  1},
        { 2,    "08-1-13", 2013 - 1900,  8 - 1,  1},
        { 2,     "8-1-13", 2013 - 1900,  8 - 1,  1},
        { 2, "11/04/2009", 2009 - 1900, 11 - 1,  4},
        { 2,  "3.12.1985", 1985 - 1900,  3 - 1, 12},
        { 2,      "6'8'3", 2003 - 1900,  6 - 1,  8},
        { 2,   "08012013", 2013 - 1900,  8 - 1,  1},
        { 3,      "01-08",   curr_year,  8 - 1,  1},
        { 3,       "01-8",   curr_year,  8 - 1,  1},
        { 3,       "1-08",   curr_year,  8 - 1,  1},
        { 3,        "1-8",   curr_year,  8 - 1,  1},
        { 3,      "04/11",   curr_year, 11 - 1,  4},
        { 3,       "12.3",   curr_year,  3 - 1, 12},
        { 3,        "8'6",   curr_year,  6 - 1,  8},
        { 3,       "0108",   curr_year,  8 - 1,  1},
        { 4,      "08-01",   curr_year,  8 - 1,  1},
        { 4,       "8-01",   curr_year,  8 - 1,  1},
        { 4,       "08-1",   curr_year,  8 - 1,  1},
        { 4,        "8-1",   curr_year,  8 - 1,  1},
        { 4,      "11/04",   curr_year, 11 - 1,  4},
        { 4,       "3.12",   curr_year,  3 - 1, 12},
        { 4,        "6'8",   curr_year,  6 - 1,  8},
        { 4,       "0801",   curr_year,  8 - 1,  1},

        // ambiguous date formats
        // current parser doesn't know how to disambiguate
        // and hence refuses to parse
        // can possibly improved with a smarter parser
        { 0,     "130801",          -1,     -1, -1},
        { 1,     "010813",          -1,     -1, -1},
        { 2,     "080113",          -1,     -1, -1},

        // Combinations that don't make sense
        // but can still be entered by a user
        // Should ideally all result in refusal to parse...
        { 0,      "08-01",          -1,     -1, -1},
        { 0,       "0801",          -1,     -1, -1},
        { 1,      "01-08",          -1,     -1, -1},
        { 1,       "0108",          -1,     -1, -1},
        { 2,      "08-01",          -1,     -1, -1},
        { 2,       "0801",          -1,     -1, -1},
        { 3, "01-08-2013",          -1,     -1, -1},
        { 3,   "01-08-13",          -1,     -1, -1},
        { 3,   "08-08-08",          -1,     -1, -1},
        { 3,   "01082013",          -1,     -1, -1},
        { 3,     "010813",          -1,     -1, -1},
        { 3,   "20130108",          -1,     -1, -1},
        { 4, "08-01-2013",          -1,     -1, -1},
        { 4,   "08-01-13",          -1,     -1, -1},
        { 4, "2013-08-01",          -1,     -1, -1},
        { 4,   "09-08-01",          -1,     -1, -1},
        { 4,   "08012013",          -1,     -1, -1},
        { 4,     "080113",          -1,     -1, -1},
        { 4,   "20130801",          -1,     -1, -1},

        // Sentinel to mark the end of available tests
        { 0,         NULL,           0,      0,  0},

    };
    int i = 0;

    gnc_tm_free(tm);
    while (test_dates[i].date_str)
    {
        gboolean success = TRUE;
        int got_year = 0, got_month = 0, got_day = 0;

        rawtime = parse_date (std::string(test_dates[i].date_str), test_dates[i].date_fmt);
        if (rawtime == -1)
            got_year = got_month = got_day = -1;
        else
        {
            tm = gnc_gmtime (&rawtime);
            got_year = tm->tm_year;
            got_month = tm->tm_mon;
            got_day = tm->tm_mday;
            gnc_tm_free(tm);
        }

        if ((got_year  != test_dates[i].exp_year) ||
            (got_month != test_dates[i].exp_month) ||
            (got_day   != test_dates[i].exp_day))
        {
            g_error ("Parse_date failed for date '%s' and format '%d'.\n"
                            "Expected result: year %d, month %d, day %d\n"
                            "Obtained result: year %d, month %d, day %d",
                            test_dates[i].date_str,
                            test_dates[i].date_fmt,
                            test_dates[i].exp_year,
                            test_dates[i].exp_month,
                            test_dates[i].exp_day,
                            got_year, got_month, got_day);
        }

        i++;
    }
}
