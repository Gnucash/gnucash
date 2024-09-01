/********************************************************************\
 * test-gnc-datetime.cpp -- Unit tests for GncDate and GncDateTime  *
 *                                                                  *
 * Copyright 2015 John Ralls <jralls@ceridwen.us>                   *
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

#include "../gnc-datetime.hpp"
#include "../gnc-date.h"
#include <gtest/gtest.h>

/* Backdoor to enable unittests to temporarily override the timezone: */
class TimeZoneProvider;
void _set_tzp(TimeZoneProvider& tz);
void _reset_tzp();

TEST(gnc_date_constructors, test_default_constructor)
{
    GncDate date;
    EXPECT_FALSE(date.isnull());
}

TEST(gnc_date_constructors, test_ymd_constructor)
{
    GncDate date(2045, 11, 13);
    EXPECT_FALSE(date.isnull());
}

TEST(gnc_date_constructors, test_copy_constructor)
{
    GncDate a(2045, 11, 13);
    GncDate b(a);
    EXPECT_FALSE(a.isnull());
    EXPECT_TRUE (a == b);
}

TEST(gnc_date_constructors, test_move_constructor)
{
    GncDate a(2045, 11, 13);
    GncDate b(std::move(a));
    EXPECT_TRUE(a.isnull());
    EXPECT_TRUE (b.format("%Y-%m-%d") == "2045-11-13");
}

typedef struct
{
    const char* date_fmt;
    const char* date_str;
    int         exp_year;
    int         exp_month;
    int         exp_day;
} parse_date_data;

TEST(gnc_date_constructors, test_str_format_constructor)
{
    auto today = GncDate();
    // auto today_ymd = today.year_month_day();
    // auto curr_year = today_ymd.year;

    parse_date_data test_dates[] =
    {
        // supported combinations  -/.'
        { "en_GB", "01-08-2013", 2013,  8,  1},
        { "en_GB",  "01-8-2013", 2013,  8,  1},
        { "en_GB",  "1-08-2013", 2013,  8,  1},
        { "en_GB",   "1-8-2013", 2013,  8,  1},
        { "en_GB",   "01-08-13", 2013,  8,  1},
        { "en_GB",    "01-8-13", 2013,  8,  1},
        { "en_GB",    "1-08-13", 2013,  8,  1},
        { "en_GB",     "1-8-13", 2013,  8,  1},
        { "en_GB", "04/11/2009", 2009, 11,  4},
        { "en_GB",  "12.3.1985", 1985,  3, 12},
        // { "en_GB",      "8'6'3", 2003,  6,  8},
        // { "en_GB",   "01082013", 2013,  8,  1},
        { "en_US", "08-01-2013", 2013,  8,  1},
        { "en_US",  "8-01-2013", 2013,  8,  1},
        { "en_US",  "08-1-2013", 2013,  8,  1},
        { "en_US",   "8-1-2013", 2013,  8,  1},
        { "en_US",   "08-01-13", 2013,  8,  1},
        { "en_US",    "8-01-13", 2013,  8,  1},
        { "en_US",    "08-1-13", 2013,  8,  1},
        { "en_US",     "8-1-13", 2013,  8,  1},
        { "en_US", "11/04/2009", 2009, 11,  4},
        { "en_US",  "3.12.1985", 1985,  3, 12},
        // { "en_US",      "6'8'3", 2003,  6,  8},
        // { "en_US",   "08012013", 2013,  8,  1},
        // {   "d-m",      "01-08",   curr_year,  8,  1},
        // {   "d-m",       "01-8",   curr_year,  8,  1},
        // {   "d-m",       "1-08",   curr_year,  8,  1},
        // {   "d-m",        "1-8",   curr_year,  8,  1},
        // {   "d-m",      "04/11",   curr_year, 11,  4},
        // {   "d-m",       "12.3",   curr_year,  3, 12},
        // {   "d-m",        "8'6",   curr_year,  6,  8},
        // {   "d-m",       "0108",   curr_year,  8,  1},
        // {   "m-d",      "08-01",   curr_year,  8,  1},
        // {   "m-d",       "8-01",   curr_year,  8,  1},
        // {   "m-d",       "08-1",   curr_year,  8,  1},
        // {   "m-d",        "8-1",   curr_year,  8,  1},
        // {   "m-d",      "11/04",   curr_year, 11,  4},
        // {   "m-d",       "3.12",   curr_year,  3, 12},
        // {   "m-d",        "6'8",   curr_year,  6,  8},
        // {   "m-d",       "0801",   curr_year,  8,  1},

        // ambiguous date formats
        // current parser doesn't know how to disambiguate
        // and hence refuses to parse
        // can possibly improved with a smarter parser
        // { "y-m-d",     "130801",          -1,     -1, -1},
        { "en_GB",     "010813",          -1,     -1, -1},
        { "en_US",     "080113",          -1,     -1, -1},

        // Combinations that don't make sense
        // but can still be entered by a user
        // Should ideally all result in refusal to parse...
        // { "y-m-d",      "08-01",          -1,     -1, -1},
        // { "y-m-d",       "0801",          -1,     -1, -1},
        { "en_GB",      "01-08",          -1,     -1, -1},
        { "en_GB",       "0108",          -1,     -1, -1},
        { "en_US",      "08-01",          -1,     -1, -1},
        { "en_US",       "0801",          -1,     -1, -1},
        // {   "d-m", "01-08-2013",          -1,     -1, -1},
        // {   "d-m",   "01-08-13",          -1,     -1, -1},
        // {   "d-m",   "08-08-08",          -1,     -1, -1},
        // {   "d-m",   "01082013",          -1,     -1, -1},
        // {   "d-m",     "010813",          -1,     -1, -1},
        // {   "d-m",   "20130108",          -1,     -1, -1},
        // {   "m-d", "08-01-2013",          -1,     -1, -1},
        // {   "m-d",   "08-01-13",          -1,     -1, -1},
        // {   "m-d", "2013-08-01",          -1,     -1, -1},
        // {   "m-d",   "09-08-01",          -1,     -1, -1},
        // {   "m-d",   "08012013",          -1,     -1, -1},
        // {   "m-d",     "080113",          -1,     -1, -1},
        // {   "m-d",   "20130801",          -1,     -1, -1},

        // Unknown date format specifier should also trigger an exception
        {   "y-d-m H:M:S",   "20130801",          -1,     -1, -1},

        // Sentinel to mark the end of available tests
        { "y-m-d",         NULL,           0,      0,  0},

    };
    int i = 0;

    while (test_dates[i].date_str)
    {
        int got_year = 0, got_month = 0, got_day = 0;

        try
        {
            auto test_date = GncDate (std::string(test_dates[i].date_str), test_dates[i].date_fmt);
            auto test_ymd = test_date.year_month_day();
            got_year = test_ymd.year;
            got_month = test_ymd.month;
            got_day = test_ymd.day;
        }
        catch (const std::invalid_argument& e)
        {
            got_year = got_month = got_day = -1;
        }

        EXPECT_TRUE ((got_year  == test_dates[i].exp_year) &&
                     (got_month == test_dates[i].exp_month) &&
                     (got_day   == test_dates[i].exp_day))
            << "GncDate constructor failed for str " << test_dates[i].date_str
            << " and fmt " << test_dates[i].date_fmt << ".\n"
            << "Expected: year " << test_dates[i].exp_year
                   << ", month " << test_dates[i].exp_month
                     << ", day " << test_dates[i].exp_day << "\n"
            << "Actual:   year " << got_year << ", month "
                    << got_month << ", day " << got_day << "\n";

        i++;
    }
}

TEST(gnc_date_operators, test_equality)
{
    GncDate a(2017, 1, 6);
    GncDate b(2017, 1, 6);
    GncDate c(2015, 6, 13);
    EXPECT_TRUE (a == b);
    EXPECT_FALSE (a == c);
    EXPECT_TRUE (a != c);
    EXPECT_FALSE (a != b);
}

TEST(gnc_date_operators, test_more_less_than)
{
    GncDate a(2017, 1, 6);
    GncDate b(2017, 1, 6);
    GncDate c(2015, 6, 13);
    EXPECT_TRUE (a >= b);
    EXPECT_TRUE (a <= b);
    EXPECT_FALSE (a > b);
    EXPECT_FALSE (a < b);

    EXPECT_TRUE (a > c);
    EXPECT_TRUE (a >= c);
    EXPECT_FALSE (a < c);
    EXPECT_FALSE (a <= c);

    EXPECT_TRUE (c < a);
    EXPECT_TRUE (c <= a);
    EXPECT_FALSE (c > a);
    EXPECT_FALSE (c >= a);
}

TEST(gnc_date_operators, test_copy_assignment)
{
    GncDate a(2017, 1, 6);
    GncDate b;
    b = a;
    EXPECT_TRUE (a == b);
}

TEST(gnc_date_operators, test_move_assignment)
{
    GncDate a(2045, 11, 13);
    GncDate b;
    b = std::move(a);
    EXPECT_TRUE(a.isnull());
    EXPECT_TRUE (b.format("%Y-%m-%d") == "2045-11-13");
}

TEST(gnc_datetime_constructors, test_default_constructor)
{
    GncDateTime atime;
    time64 time_now = gnc_time(nullptr);
    EXPECT_EQ(static_cast<time64>(atime), static_cast<time64>(time_now));
}

TEST(gnc_datetime_constructors, test_time64_constructor)
{
    const time64 time = 2394187200; //2045-11-13 12:00:00 Z
    GncDateTime atime(time);
    EXPECT_EQ(static_cast<time64>(atime), time);
}

TEST(gnc_datetime_constructors, test_string_constructor)
{
/* Plain UTC date-time */
    std::string timestr("2015-12-05 11:57:03");
    GncDateTime time1(timestr);
    auto tm = time1.utc_tm();
    EXPECT_EQ(tm.tm_year, 115);
    EXPECT_EQ(tm.tm_mon, 11);
    EXPECT_EQ(tm.tm_mday, 5);
    EXPECT_EQ(tm.tm_hour,11);
    EXPECT_EQ(tm.tm_min, 57);
    EXPECT_EQ(tm.tm_sec, 3);

/* Datetime with an offset */
    timestr = "1993-07-22 15:21:19 +0300";
    GncDateTime time2(timestr);
    tm = time2.utc_tm();
    EXPECT_EQ(tm.tm_year, 93);
    EXPECT_EQ(tm.tm_mon, 6);
    EXPECT_EQ(tm.tm_mday, 22);
    EXPECT_EQ(tm.tm_hour, 12);
    EXPECT_EQ(tm.tm_min, 21);
    EXPECT_EQ(tm.tm_sec, 19);

/* Bug 767824 date-time */
    timestr = "1993-07-22 15:21:19 +0013";
    GncDateTime time3(timestr);
    tm = time3.utc_tm();
    EXPECT_EQ(tm.tm_year, 93);
    EXPECT_EQ(tm.tm_mon, 6);
    EXPECT_EQ(tm.tm_mday, 22);
    EXPECT_EQ(tm.tm_hour, 15);
    EXPECT_EQ(tm.tm_min, 8);
    EXPECT_EQ(tm.tm_sec, 19);
/* Squashed format from SQLite3 databases */
    timestr = "20151205115703";
    GncDateTime time4(timestr);
    tm = time4.utc_tm();
    EXPECT_EQ(tm.tm_year, 115);
    EXPECT_EQ(tm.tm_mon, 11);
    EXPECT_EQ(tm.tm_mday, 5);
    EXPECT_EQ(tm.tm_hour,11);
    EXPECT_EQ(tm.tm_min, 57);
    EXPECT_EQ(tm.tm_sec, 3);
}

TEST(gnc_datetime_constructors, test_struct_tm_constructor)
{
#ifdef HAVE_STRUCT_TM_GMTOFF
    const struct tm tm {0, 0, 12, 13, 10, 145, 0, 0, 0, NULL, 0 };
#else
    const struct tm tm {0, 0, 12, 13, 10, 145, 0, 0, 0 };
#endif

    const time64 time = 2394187200; //2045-11-13 12:00:00 Z
    GncDateTime atime(tm);
    EXPECT_EQ(static_cast<time64>(atime) + atime.offset(), time);
    const struct tm tm1 = static_cast<struct tm>(atime);
    EXPECT_EQ(tm1.tm_year, tm.tm_year);
    EXPECT_EQ(tm1.tm_mon, tm.tm_mon);
// We have to contort these a bit to handle offsets > 12, e.g. New Zealand during DST.
//    EXPECT_EQ(tm1.tm_mday - (11 + atime.offset() / 3600) / 24 , tm.tm_mday);
//    EXPECT_EQ((24 + tm1.tm_hour - atime.offset() / 3600) % 24, tm.tm_hour);
    EXPECT_EQ(tm1.tm_mday, tm.tm_mday);
    EXPECT_EQ(tm1.tm_hour, tm.tm_hour);
    EXPECT_EQ(tm1.tm_min, tm.tm_min);
}

/* Note: the following tests for the constructor taking a GncDate as input parameter
 * use GncDateTime's format() member function to simplify the result checking.
 * If there's a bug in this member function, these tests may fail in addition
 * to the format test later in the test suite. Be sure to check that later
 * test as well in case any of the below constructor tests fails. */

TEST(gnc_datetime_constructors, test_gncdate_start_constructor)
{
    const gnc_ymd aymd = { 2017, 04, 20 };
    GncDateTime atime(GncDate(aymd.year, aymd.month, aymd.day), DayPart::start);
    //Skipping timezone information as this can't be controlled.
    EXPECT_EQ(atime.format("%d-%m-%Y %H:%M:%S"), "20-04-2017 00:00:00");
}

/* Putting this here is a bit weird but it includes
 * boost/date_time/local_time/local_time.hpp and that redefines struct
 * tm in a way that breaks initializing the tm in
 * test_struct_tm_constructor on Linux.
 */
#include "../gnc-timezone.hpp"
/* Summertime transitions have been a recurring problem. At the time of adding
 * this test the GncDateEditor was refusing to set the date to 28 October 2018
 * in the Europe/London timezone.
 */
TEST(gnc_datetime_constructors, test_gncdate_BST_transition)
{
    const gnc_ymd begins = {2018, 03, 25};
    const gnc_ymd ends = {2018, 10, 28};
#ifdef __MINGW32__
    TimeZoneProvider tzp{"GMT Standard Time"};
#else
    TimeZoneProvider tzp("Europe/London");
#endif
    _set_tzp(tzp);
    GncDateTime btime(GncDate(begins.year, begins.month, begins.day), DayPart::start);
    GncDateTime etime(GncDate(ends.year, ends.month, ends.day), DayPart::start);
    _reset_tzp();
    EXPECT_EQ(btime.format("%d-%m-%Y %H:%M:%S"), "25-03-2018 00:00:00");
    EXPECT_EQ(etime.format("%d-%m-%Y %H:%M:%S"), "28-10-2018 00:00:00");
}

TEST(gnc_datetime_constructors, test_gncdate_end_constructor)
{
    const gnc_ymd aymd = { 2046, 11, 06 };
    GncDateTime atime(GncDate(aymd.year, aymd.month, aymd.day), DayPart::end);
    //Skipping timezone information as this can't be controlled.
    EXPECT_EQ(atime.format("%d-%m-%Y %H:%M:%S"), "06-11-2046 23:59:59");
}

static ::testing::AssertionResult
test_offset(time64 start_time, int hour, int offset1, int offset2,
            const char* zone)
{
    GncDateTime gdt{start_time + hour * 3600};
    if ((hour < 2 && gdt.offset() == offset1) ||
        (hour >= 2 && gdt.offset() == offset2))
        return ::testing::AssertionSuccess();
    else
        return ::testing::AssertionFailure() << zone << ": " << gdt.format("%D %T %z %q") << " hour " << hour;
}

TEST(gnc_datetime_constructors, test_DST_start_transition_time)
{
#ifdef __MINGW32__
    TimeZoneProvider tzp_can{"A.U.S Eastern Standard Time"};
    TimeZoneProvider tzp_la{"Pacific Standard Time"};
#else
    TimeZoneProvider tzp_can("Australia/Canberra");
    TimeZoneProvider tzp_la("America/Los_Angeles");
#endif
    _set_tzp(tzp_la);
    for (auto hours = 0; hours < 23; ++hours)
        EXPECT_TRUE(test_offset(1583657940, hours, -28800, -25200, "Los Angeles"));

    _reset_tzp();
    _set_tzp(tzp_can);
    for (auto hours = 0; hours < 23; ++hours)
        EXPECT_TRUE(test_offset(1601737140, hours, 36000, 39600, "Canberra"));
    _reset_tzp();
}

TEST(gnc_datetime_constructors, test_DST_end_transition_time)
{
#ifdef __MINGW32__
    TimeZoneProvider tzp_can{"A.U.S Eastern Standard Time"};
    TimeZoneProvider tzp_la{"Pacific Standard Time"};
#else
    TimeZoneProvider tzp_can("Australia/Canberra");
    TimeZoneProvider tzp_la("America/Los_Angeles");
#endif
    _set_tzp(tzp_la);
    for (auto hours = 0; hours < 23; ++hours)
        EXPECT_TRUE(test_offset(1604217540, hours, -25200, -28800, "Los Angeles"));
    _reset_tzp();
    _set_tzp(tzp_can);
    for (auto hours = 0; hours < 23; ++hours)
        EXPECT_TRUE(test_offset(1586008740, hours, 39600, 36000, "Canberra"));
    _reset_tzp();
}

TEST(gnc_datetime_constructors, test_create_in_transition)
{
#ifdef __MINGW32__
    TimeZoneProvider tzp_br{"E. South America Standard Time"};
#else
    TimeZoneProvider tzp_br("America/Sao_Paulo");
#endif
    _set_tzp(tzp_br);
    /* Test Daylight Savings start: When Sao Paolo had daylight
     * savings time it ended at 23:59:59 and the next second was
     * 01:00:00 so that's when the day starts.
     */
    GncDate date0{"03-11-2018", "en_GB"};
    GncDateTime gncdt0{date0, DayPart::end};
    EXPECT_EQ(gncdt0.format_zulu("%Y-%m-%d %H:%M:%S %Z"), "2018-11-04 02:59:59 UTC");
    EXPECT_EQ(gncdt0.format("%Y-%m-%d %H:%M:%S %Z"), "2018-11-03 23:59:59 -03");
    GncDate date1{"04-11-2018", "en_GB"};
    GncDateTime gncdt1{date1, DayPart::start};
    EXPECT_EQ(gncdt1.format_zulu("%Y-%m-%d %H:%M:%S %Z"), "2018-11-04 03:00:00 UTC");
    EXPECT_EQ(gncdt1.format("%Y-%m-%d %H:%M:%S %Z"), "2018-11-04 01:00:00 -02");
    /* End of day, end of DST. We want one second before midnight in
     * std time, i.e. -03. Unfortunately sometimes boost::date_time
     * decides that it's still DST and returns the first one.
     */
    GncDate date2{"17-02-2018", "en_GB"};
    GncDateTime gncdt2{date2, DayPart::end};
    if (gncdt2.offset() == -7200)
    {
        EXPECT_EQ(gncdt2.format_zulu("%Y-%m-%d %H:%M:%S %Z"), "2018-02-18 01:59:59 UTC");
        EXPECT_EQ(gncdt2.format("%Y-%m-%d %H:%M:%S %Z"), "2018-02-17 23:59:59 -02");
    }
    else
    {
        EXPECT_EQ(gncdt2.format_zulu("%Y-%m-%d %H:%M:%S %Z"), "2018-02-18 02:59:59 UTC");
        EXPECT_EQ(gncdt2.format("%Y-%m-%d %H:%M:%S %Z"), "2018-02-17 23:59:59 -03");
    }
   /* After February 2019 Sao Paulo discontinued Daylight
     * Savings. This test checks to ensure that GncTimeZone doesn't
     * try to project 2018's rule forward.
     */
    GncDate date3{"01-11-2019", "en_GB"};
    GncDateTime gncdt3{date3, DayPart::start};
    EXPECT_EQ(gncdt3.format_zulu("%Y-%m-%d %H:%M:%S %Z"), "2019-11-01 03:00:00 UTC");
    EXPECT_EQ(gncdt3.format("%Y-%m-%d %H:%M:%S %Z"), "2019-11-01 00:00:00 -03");
}

TEST(gnc_datetime_constructors, test_gncdate_neutral_constructor)
{
#ifdef __MINGW32__
    TimeZoneProvider tzp_la{"Pacific Standard Time"};
#else
    TimeZoneProvider tzp_la("America/Los_Angeles");
#endif
    _set_tzp(tzp_la);
    const gnc_ymd aymd = { 2017, 04, 20 };
    GncDateTime atime(GncDate(aymd.year, aymd.month, aymd.day), DayPart::neutral);
    time64 date{1492685940};
    GncDateTime gncdt(date); /* 20 Apr 2017 10:59:00 Z */
    /* The gnc_ymd constructor sets the time of day at 10:59:00 for
     * timezones between UTC-10 and UTC+13. For other timezones the
     * time of day is adjusted to ensure a consistent date and the
     * adjustment invalidates the test, so skip it.
     */
    constexpr time64 max_western_offset = -10 * 3600;
    constexpr time64 max_eastern_offset = 13 * 3600;
    if (gncdt.offset() >= max_western_offset &&
        gncdt.offset() <= max_eastern_offset)
    {
        EXPECT_EQ(atime.format_zulu("%d-%m-%Y %H:%M:%S %Z"),
                  "20-04-2017 10:59:00 UTC");
        EXPECT_EQ(date, static_cast<time64>(gncdt));
        EXPECT_EQ(date, static_cast<time64>(atime));
    }
}

TEST(gnc_datetime_constructors, test_neutral_across_timezones)
{
    const gnc_ymd begins = {2018, 03, 05};
    const gnc_ymd ends = {2018, 03, 15};
    const time64 ten_days = 864000;
#ifdef __MINGW32__
    TimeZoneProvider tzp_lon{"GMT Standard Time"};
    TimeZoneProvider tzp_perth{"W. Australia Standard Time"};
    TimeZoneProvider tzp_la{"Pacific Standard Time"};
#else
    TimeZoneProvider tzp_lon("Europe/London");
    TimeZoneProvider tzp_perth("Australia/Perth");
    TimeZoneProvider tzp_la("America/Los_Angeles");
#endif
    _set_tzp(tzp_lon);
    GncDateTime btime_lon(GncDate(begins.year, begins.month, begins.day), DayPart::neutral);
    GncDateTime etime_lon(GncDate(ends.year, ends.month, ends.day), DayPart::neutral);
    _reset_tzp();
    _set_tzp(tzp_perth);
    GncDateTime btime_perth(GncDate(begins.year, begins.month, begins.day), DayPart::neutral);
    GncDateTime etime_perth(GncDate(ends.year, ends.month, ends.day), DayPart::neutral);
    _reset_tzp();
    _set_tzp(tzp_la);
    GncDateTime btime_la(GncDate(begins.year, begins.month, begins.day), DayPart::neutral);
    GncDateTime etime_la(GncDate(ends.year, ends.month, ends.day), DayPart::neutral);
    _reset_tzp();

    EXPECT_EQ(static_cast<time64>(btime_lon), static_cast<time64>(btime_perth));
    EXPECT_EQ(static_cast<time64>(btime_lon), static_cast<time64>(btime_la));
    EXPECT_EQ(static_cast<time64>(etime_lon), static_cast<time64>(etime_perth));
    EXPECT_EQ(static_cast<time64>(etime_lon), static_cast<time64>(etime_la));
    EXPECT_EQ(ten_days, static_cast<time64>(etime_lon) - static_cast<time64>(btime_lon));
    EXPECT_EQ(ten_days, static_cast<time64>(etime_perth) - static_cast<time64>(btime_perth));
    EXPECT_EQ(ten_days, static_cast<time64>(etime_la) - static_cast<time64>(btime_la));
}

TEST(gnc_datetime_functions, test_format)
{
    GncDateTime atime(2394187200); //2045-11-13 12:00:00 Z
    if ((atime.offset() / 3600) > 11)
        EXPECT_EQ(atime.format("%d-%m-%Y"), "14-11-2045");
    else
        EXPECT_EQ(atime.format("%d-%m-%Y"), "13-11-2045");
}

TEST(gnc_datetime_functions, test_format_zulu)
{
    GncDateTime atime(2394187200); //2045-11-13 12:00:00 Z
    //Date only to finesse timezone issues. It will still fail in +12 DST.
    EXPECT_EQ(atime.format_zulu("%d-%m-%Y %H:%M:%S"), "13-11-2045 12:00:00");
}

//This is a bit convoluted because it uses GncDate's GncDateImpl constructor and year_month_day() function. There's no good way to test the former without violating the privacy of the implementation.
TEST(gnc_datetime_functions, test_date)
{
    GncDateTime atime(2394187200); //2045-11-13 12:00:00 Z
    GncDate gncd = atime.date();
    auto ymd = gncd.year_month_day();
    EXPECT_EQ(ymd.year, 2045);
    EXPECT_EQ(ymd.month, 11);
    EXPECT_EQ(ymd.day - (12 + atime.offset() / 3600) / 24, 13);
}
/* This test works only in the America/LosAngeles time zone and
 * there's no straightforward way to make it more flexible. It ensures
 * that DST in that timezone transitions correctly for each day of the
 * week in which March begins.

 TEST(gnc_datetime_functions, test_timezone_offset)
{
    struct Timepair
    {
        time64 before;
        time64 after;
    };
    std::array<Timepair, 7> years{
        Timepair{1615633140, 1615719540}, //2021, Monday
        Timepair{1457780340, 1457866740}, //2016, Tuesday
        Timepair{1489229940, 1489316340}, //2017, Wednesday
        Timepair{1520679540, 1520765940}, //2018, Thursday
        Timepair{1552129140, 1552215540}, //2019, Friday
        Timepair{1741431540, 1741517940}, //2025, Saturday
        Timepair{1583578740, 1583665140}  //2020, Sunday
    };
    for (auto year : years)
    {
        GncDateTime before{year.before};
        GncDateTime after{year.after};
//        std::cerr << before.format_iso8601() << std::endl;
        EXPECT_EQ(-28800, before.offset());
        EXPECT_EQ(-25200, after.offset());
    }

}
*/
