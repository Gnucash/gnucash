/********************************************************************
 * test-gnc-date.cpp: Google test test suite for gnc-date.c.        *
 * Copyright 2012 John Ralls <jralls@ceridwen.us>                   *
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
 * https://www.gnu.org/licenses/old-licenses/gpl-2.0.html            *
 * or contact:                                                      *
 *                                                                  *
 * Free Software Foundation           Voice:  +1-617-542-5942       *
 * 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652       *
 * Boston, MA  02110-1301,  USA       gnu@gnu.org                   *
 ********************************************************************/
#include <gtest/gtest.h>
#include <config.h>
#include "platform.h"
#include <string.h>
#include <stdlib.h>
#include <sys/time.h>
#include <glib.h>
#include <unittest-support.h>
/* Add specific headers for this class */

#include "../gnc-date.h"
#include "../gnc-date-p.h"
#include <locale.h>
#include <glib/gprintf.h>
#include <inttypes.h>
#ifndef HAVE_STRPTIME
#  include "strptime.h"
#endif

static const time64 max_secs = (INT64_C(3600) * (INT64_C(24) * INT64_C(365) + 6)) * (INT64_C(9999) - INT64_C(1970));

typedef struct
{
    short hours;
    short minutes;
} TZOffset;

class GNCDateFixA: public ::testing::Test
{
public:
    GNCDateFixA();
protected:
    TZOffset m_off_zulu;
    TZOffset m_off_05w;
    TZOffset m_off_0840e;
    time64 m_t0;
    time64 m_t1;
    time64 m_t2;
    time64 m_t3;
    time64 m_t4;
    time64 m_t5;
};

GNCDateFixA::GNCDateFixA()
    : m_off_zulu{0, 0},
      m_off_05w{-5, 0},
      m_off_0840e{8, 40},
      m_t0(gnc_time(nullptr)),
      m_t1( 607009407), //1989-3-27 13:43:27 Z
      m_t2(1604748079), //2020-11-7 06:21:19 -05:00
      m_t3(1341398864), //2012-07-04 19:27:44 +08:40
      m_t4(-261104801), //1961-09-22 17:53:19 -05:00
      m_t5(2873938879LL) //2061-01-25 23:21:19 -05:00
{
}

typedef struct
{
    int yr;
    int mon;
    int day;
    time64 secs;
} TimeMap;

class GNCDateFixB: public ::testing::Test
{
protected:
    std::array<TimeMap, 9> m_test;
};

class GNCDateFixBegin : public GNCDateFixB
{
public:
    GNCDateFixBegin();
};

GNCDateFixBegin::GNCDateFixBegin()
{
    m_test = std::array<TimeMap, 9>(
        {TimeMap{1999, 7, 21, INT64_C(932515200)},
         TimeMap{1918, 3, 31, INT64_C(-1633305600)},
         TimeMap{1918, 4, 1, INT64_C(-1633219200)},
         TimeMap{2057, 11, 20, INT64_C(2773440000)},
         TimeMap{1257, 07, 02, INT64_MAX}, /*invalid year*/
         TimeMap{2017, 02, 29, INT64_MAX}, /*invalid day*/
         TimeMap{2017, 02, 33, INT64_MAX}, /*invalid day*/
         TimeMap{2017, 13, 29, INT64_MAX}, /*invalid month*/
         TimeMap{2017, 03, 16, INT64_C(1489622400)}});

}

class GNCDateFixNeurtal : public GNCDateFixB
{
public:
    GNCDateFixNeurtal();
};

GNCDateFixNeurtal::GNCDateFixNeurtal()
{
    m_test = std::array<TimeMap, 9>(
        {TimeMap{1999, 7, 21, INT64_C(932554740)},
         TimeMap{1918, 3, 31, INT64_C(-1633266060)},
         TimeMap{1918, 4, 1, INT64_C(-1633179660)},
         TimeMap{2057, 11, 20, INT64_C(2773479540)},
         TimeMap{1257, 07, 02, INT64_MAX},
         TimeMap{2017, 02, 29, INT64_MAX},
         TimeMap{2017, 02, 33, INT64_MAX},
         TimeMap{2017, 13, 29, INT64_MAX},
         TimeMap{2017, 03, 16, INT64_C(1489661940)}});
}

class GNCDateFixEnd : public GNCDateFixB
{
public:
    GNCDateFixEnd();
};

GNCDateFixEnd::GNCDateFixEnd()
{
    m_test = std::array<TimeMap, 9>(
        {TimeMap{1999, 7, 21, INT64_C(932601599)},
         TimeMap{1918, 3, 31, INT64_C(-1633219201)},
         TimeMap{1918, 4, 1, INT64_C(-1633132801)},
         TimeMap{2057, 11, 20, INT64_C(2773526399)},
         TimeMap{1257, 07, 02, INT64_MAX},
         TimeMap{2017, 02, 29, INT64_MAX},
         TimeMap{2017, 02, 33, INT64_MAX},
         TimeMap{2017, 13, 29, INT64_MAX},
         TimeMap{2017, 03, 16, INT64_C(1489708799)}});
}

static GTimeZone *tz;
#define MAXTIME INT64_C(253402214400)
#define MINTIME INT64_C(-17987443200)

/* gnc_localtime just creates a tm on the heap and calls
 * gnc_localtime_r with it, so this suffices to test both.
 */
TEST(GNCDate, gnc_localtime)
{
    time64 secs[] = {-15767956734LL, -1123692LL, 432761LL,
                      723349832LL, 887326459367LL,
                      1364160236LL};
    guint ind;
    if (sizeof(time_t) < sizeof(time64))
        secs[0] = -432761LL;

    for (ind = 0; ind < G_N_ELEMENTS (secs); ind++)
    {
        struct tm* time = gnc_localtime (&secs[ind]);
        time_t tsecs;
        struct tm* ans;
        if (secs[ind] > max_secs)
        {
            EXPECT_EQ(time, nullptr);
            continue;
        }
        tsecs = (time_t)(secs[ind]);
        ans = localtime(&tsecs);
        EXPECT_EQ(time->tm_year, ans->tm_year);
        EXPECT_EQ(time->tm_mon, ans->tm_mon);
        EXPECT_EQ(time->tm_mday, ans->tm_mday);
        EXPECT_EQ(time->tm_hour, ans->tm_hour);
        EXPECT_EQ(time->tm_min, ans->tm_min);
        EXPECT_EQ(time->tm_sec, ans->tm_sec);
        EXPECT_EQ(time->tm_wday, ans->tm_wday);
        EXPECT_EQ(time->tm_yday, ans->tm_yday);
        EXPECT_EQ(time->tm_isdst, ans->tm_isdst);
#ifdef HAVE_STRUCT_TM_GMTOFF
        EXPECT_EQ(time->tm_gmtoff, ans->tm_gmtoff);
#endif
        gnc_tm_free (time);
    }
}

TEST(GNCDate, gnc_gmtime)
{
    time64 secs[6] = {-15767956734LL, -1123692LL, 432761LL,
                      723349832LL, 887326459367LL, 1175964426LL
                     };
    struct tm answers[6] =
    {
#ifdef HAVE_STRUCT_TM_GMTOFF
        { 6, 1, 12, 2, 4, -430, 1, 121, 0, 0, nullptr },
        { 48, 51, 23, 18, 11, 69, 4, 351, 0, 0, nullptr },
        { 41, 12, 0, 6, 0, 70, 2, 5, 0, 0, nullptr },
        { 32, 30, 2, 3, 11, 92, 4, 337, 0, 0, nullptr },
        { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, nullptr },
        { 6, 47, 16, 7, 3, 107, 6, 96, 0, 0, nullptr },
#else
        { 6, 1, 12, 2, 4, -430, 1, 121, 0 },
        { 48, 51, 23, 18, 11, 69, 4, 351, 0 },
        { 41, 12, 0, 6, 0, 70, 2, 5, 0 },
        { 32, 30, 2, 3, 11, 92, 4, 337, 0 },
        { 0, 0, 0, 0, 0, 0, 0, 0, 0 },
        { 6, 47, 16, 7, 3, 107, 6, 96, 0 },
#endif
    };
    guint ind;
    for (ind = 0; ind < G_N_ELEMENTS (secs); ind++)
    {
        struct tm* time = gnc_gmtime (&secs[ind]);
        if ((secs[ind] > max_secs))
        {
            EXPECT_EQ(time, nullptr);
            continue;
        }

        EXPECT_EQ(time->tm_year, answers[ind].tm_year);
        EXPECT_EQ(time->tm_mon, answers[ind].tm_mon);
        EXPECT_EQ(time->tm_mday, answers[ind].tm_mday);
        EXPECT_EQ(time->tm_hour, answers[ind].tm_hour);
        EXPECT_EQ(time->tm_min, answers[ind].tm_min);
        EXPECT_EQ(time->tm_sec, answers[ind].tm_sec);
        EXPECT_EQ(time->tm_wday, answers[ind].tm_wday);
        EXPECT_EQ(time->tm_yday, answers[ind].tm_yday);
        EXPECT_EQ(time->tm_isdst, -1);
#ifdef HAVE_STRUCT_TM_GMTOFF
        EXPECT_EQ(time->tm_gmtoff, 0);
#endif
        gnc_tm_free (time);
    }
}

TEST(GNCDate, gnc_mktime)
{
    time64 ans[5] =
        { -15752870334LL, -1123692LL, 432761LL, 723349832LL, 1175964426LL};

    struct tm time[5] =
    {
#ifdef HAVE_STRUCT_TM_GMTOFF
        { 6, 41, 2, 24, 9, -430, 0, 0, 1, 0, nullptr },
        { 48, 51, 23, 18, 11, 69, 0, 0, -1, 0, nullptr },
        { 41, 12, 0, 6, 0, 70, 0, 0, -1, 0, nullptr },
        { 32, 30, 2, 3, 11, 92, 0, 0, -1, 0, nullptr },
        { 6, 47, 16, 7, 3, 107, 0, 0, -1, 0, nullptr },
#else
        { 6, 41, 2, 24, 9, -430, 0, 0, 1 },
        { 48, 51, 23, 18, 11, 69, 0, 0, -1 },
        { 41, 12, 0, 6, 0, 70, 0, 0, -1 },
        { 32, 30, 2, 3, 11, 92, 0, 0, -1 },
        { 6, 47, 16, 7, 3, 107, 0, 0, -1 },
#endif
    };
    guint ind;

    for (ind = 0; ind < G_N_ELEMENTS (time); ind++)
    {
        int offset = timegm(&time[ind]) - mktime(&time[ind]);
        time64 secs = gnc_mktime (&time[ind]);
#if !PLATFORM(WINDOWS)
	//The timezone database uses local time for some
	//timezones before 1900, which screws up the offset.
	if (time[ind].tm_year < 0)
            continue;
#endif
        EXPECT_EQ(secs, ans[ind] - offset);

    }
}

/* In addition to computing a time offset from a struct tm, mktime is
 * supposed to normalize struct tms with out-of-range values. This
 * second test exercises that facility in gnc_mktime.
 */
TEST(GNCDate, gnc_mktime_normalization)
{
    time64 ans = 723349832LL;

    struct tm normal_time =
#ifdef HAVE_STRUCT_TM_GMTOFF
    {
        32, 30, 2, 3, 11, 92, 0, 0, -1, 0, nullptr
    };
#else
    {
        32, 30, 2, 3, 11, 92, 0, 0, -1
    };
#endif

    struct tm time[4] =
    {
#ifdef HAVE_STRUCT_TM_GMTOFF
        { 92, -31, 27, -29, 24, 91, 0, 0, -1, 0, nullptr },
        { -28, 91, -47, 35, -2, 93, 0, 0, -1, 0, nullptr },
        { -28, 91, -47, 66, -3, 93, 0, 0, -1, 0, nullptr },
        { -28, 91, -47, 35, -26, 95, 0, 0, -1, 0, nullptr },
#else
        { 92, -31, 27, -29, 24, 91, 0, 0, -1 },
        { -28, 91, -47, 35, -2, 93, 0, 0, -1 },
        { -28, 91, -47, 66, -3, 93, 0, 0, -1 },
        { -28, 91, -47, 35, -26, 95, 0, 0, -1 },
#endif
    };
    guint ind;
    struct tm other_time = normal_time;
    time_t calc_timegm = timegm(&other_time);
    time_t calc_time = mktime(&normal_time);
    for (ind = 0; ind < G_N_ELEMENTS (time); ind++)
    {
        time64 secs = gnc_mktime (&time[ind]);

        EXPECT_EQ(time[ind].tm_sec, normal_time.tm_sec);
        EXPECT_EQ(time[ind].tm_min, normal_time.tm_min);
        EXPECT_EQ(time[ind].tm_hour, normal_time.tm_hour);
        EXPECT_EQ(time[ind].tm_mday, normal_time.tm_mday);
        EXPECT_EQ(time[ind].tm_mon, normal_time.tm_mon);
        EXPECT_EQ(time[ind].tm_year, normal_time.tm_year);
        EXPECT_EQ(secs, ans - (calc_timegm - calc_time));
    }
}

TEST(GNCDate, gnc_ctime)
{
    time64 secs[5] = {-15767956734LL, -1123692LL, 432761LL,
                      723349832LL, 1175964426LL
                     };
    guint ind;
    for (ind = 0; ind < G_N_ELEMENTS (secs); ind++)
    {
	time_t time;
	char *datestr;
	char check_str[80];
        if (secs[ind] < INT32_MIN)
            continue;
        time = (time_t)secs[ind];
        datestr = gnc_ctime (&secs[ind]);
	strftime (check_str, 80, "%a %b %d %H:%M:%S %Y", localtime(&time));
        EXPECT_STREQ(datestr, check_str);
        g_free (datestr);
    }
}

TEST(GNCDate, gnc_time)
{
    time64 secs1, secs2;
    secs1 = gnc_time (&secs2);
    EXPECT_EQ(secs1, secs2);
    EXPECT_EQ(secs1, gnc_time(nullptr));
}

TEST(GNCDate, gnc_date_dateformat_to_string)
{
    EXPECT_STREQ(gnc_date_dateformat_to_string (QOF_DATE_FORMAT_US), "us");
    EXPECT_STREQ(gnc_date_dateformat_to_string (QOF_DATE_FORMAT_UK), "uk");
    EXPECT_STREQ(gnc_date_dateformat_to_string (QOF_DATE_FORMAT_CE), "ce");
    EXPECT_STREQ(gnc_date_dateformat_to_string (QOF_DATE_FORMAT_ISO), "iso");
    EXPECT_STREQ(gnc_date_dateformat_to_string (QOF_DATE_FORMAT_UTC), "utc");
    EXPECT_STREQ(gnc_date_dateformat_to_string (QOF_DATE_FORMAT_LOCALE), "locale");
    EXPECT_STREQ(gnc_date_dateformat_to_string (QOF_DATE_FORMAT_CUSTOM), "custom");
    EXPECT_STREQ(gnc_date_dateformat_to_string (QOF_DATE_FORMAT_UNSET), "unset");

}

TEST(GNCDate, gnc_date_string_to_dateformat)
{
    QofDateFormat fmt = QofDateFormat(QOF_DATE_FORMAT_CUSTOM);
    EXPECT_TRUE(gnc_date_string_to_dateformat (nullptr, &fmt));
    EXPECT_EQ(fmt, QOF_DATE_FORMAT_CUSTOM);
    EXPECT_TRUE(!gnc_date_string_to_dateformat ("us", &fmt));
    EXPECT_EQ(fmt, QOF_DATE_FORMAT_US);
    EXPECT_TRUE(!gnc_date_string_to_dateformat ("uk", &fmt));
    EXPECT_EQ(fmt, QOF_DATE_FORMAT_UK);
    EXPECT_TRUE(!gnc_date_string_to_dateformat ("ce", &fmt));
    EXPECT_EQ(fmt, QOF_DATE_FORMAT_CE);
    EXPECT_TRUE(!gnc_date_string_to_dateformat ("iso", &fmt));
    EXPECT_EQ(fmt, QOF_DATE_FORMAT_ISO);
    EXPECT_TRUE(!gnc_date_string_to_dateformat ("utc", &fmt));
    EXPECT_EQ(fmt, QOF_DATE_FORMAT_UTC);
    EXPECT_TRUE(!gnc_date_string_to_dateformat ("locale", &fmt));
    EXPECT_EQ(fmt, QOF_DATE_FORMAT_LOCALE);
    EXPECT_TRUE(!gnc_date_string_to_dateformat ("custom", &fmt));
    EXPECT_EQ(fmt, QOF_DATE_FORMAT_CUSTOM);
    EXPECT_TRUE(!gnc_date_string_to_dateformat ("unset", &fmt));
    EXPECT_EQ(fmt, QOF_DATE_FORMAT_UNSET);
    fmt = QofDateFormat(QOF_DATE_FORMAT_CUSTOM);
    EXPECT_TRUE(gnc_date_string_to_dateformat ("", &fmt));
    EXPECT_EQ(fmt, QOF_DATE_FORMAT_CUSTOM);
    EXPECT_TRUE(gnc_date_string_to_dateformat ("foo", &fmt));
    EXPECT_EQ(fmt, QOF_DATE_FORMAT_CUSTOM);

}

TEST(GNCDate, gnc_date_monthformat_to_string)
{
    EXPECT_STREQ(gnc_date_monthformat_to_string (GNCDATE_MONTH_NUMBER), "number");
    EXPECT_STREQ(gnc_date_monthformat_to_string (GNCDATE_MONTH_ABBREV), "abbrev");
    EXPECT_STREQ(gnc_date_monthformat_to_string (GNCDATE_MONTH_NAME), "name");
    EXPECT_EQ(gnc_date_monthformat_to_string (GNCDateMonthFormat(93)), nullptr);
}

TEST(GNCDate, gnc_date_string_to_monthformat)
{
    GNCDateMonthFormat fmt = GNCDateMonthFormat(GNCDATE_MONTH_NAME);
    EXPECT_TRUE(gnc_date_string_to_monthformat (nullptr, &fmt));
    EXPECT_EQ(fmt, GNCDATE_MONTH_NAME);
    EXPECT_TRUE(!gnc_date_string_to_monthformat ("number", &fmt));
    EXPECT_EQ(fmt, GNCDATE_MONTH_NUMBER);
    EXPECT_TRUE(!gnc_date_string_to_monthformat ("abbrev", &fmt));
    EXPECT_EQ(fmt, GNCDATE_MONTH_ABBREV);
    EXPECT_TRUE(!gnc_date_string_to_monthformat ("name", &fmt));
    EXPECT_EQ(fmt, GNCDATE_MONTH_NAME);
    fmt = GNCDateMonthFormat(GNCDATE_MONTH_NAME);
    EXPECT_TRUE(gnc_date_string_to_monthformat ("", &fmt));
    EXPECT_EQ(fmt, GNCDATE_MONTH_NAME);
    EXPECT_TRUE(gnc_date_string_to_monthformat ("foo", &fmt));
    EXPECT_EQ(fmt, GNCDATE_MONTH_NAME);
}

static void
test_gnc_setlocale (int category, const gchar *locale)
{
    const gchar *suffixes[] = {"utf8", "UTF-8"};
    guint i;
    /* Msys defines a different set of locales */
#ifdef G_OS_WIN32
    if (g_strcmp0 (locale, "en_US") == 0
            && setlocale (category, "English_US"))
        return;
    if (g_strcmp0 (locale, "en_GB") == 0
            && setlocale (category, "English_UK"))
        return;
    if (g_strcmp0 (locale, "fr_FR") == 0
            && setlocale (category, "French_France"))
        return;

#endif
    if (setlocale (category, locale) != nullptr)
        return;

    for (i = 0; i < G_N_ELEMENTS (suffixes); i++)
    {
        gchar * modlocale = g_strdup_printf ("%s.%s", locale, suffixes[i]);
        gchar *localeval = setlocale (category, modlocale);
        g_free (modlocale);
        if (localeval != nullptr)
            return;
    }
    g_fprintf (stderr, "There are some differences between distros in the way they name "
              "locales, and this can cause trouble with the locale-based "
              "formatting. If you get the assert in this function, run locale -a "
              "and make sure that en_US, en_GB, and fr_FR are installed and that "
              "if a suffix is needed it's in the suffixes array.");
    EXPECT_FALSE(true); // Assert not reached.
}

static time64
compute_noon_of_day (const time64 *t)
{
    struct tm time;
    gnc_localtime_r(t, &time);
    time.tm_hour = 12;
    time.tm_min = 0;
    time.tm_sec = 0;
    return gnc_mktime(&time);
}

TEST(GNCDate, time64CanonicalDayTime)
{
    const int sec_per_day = 24 * 3600;
    const int sec_per_mo = 30 * sec_per_day;
    const time64 sec_per_yr = 365 * sec_per_day;
    const time64 secs = 8 * 3600 + 43 * 60 + 11; /* 1970-01-01 08:43:11 Z */
    const time64 secs1 = 23 * sec_per_yr + 5 * sec_per_mo +
        11 * sec_per_day + 8 * 3600 + 43 * 60 + 11; /* 1993-05-11 08:43:60 Z */
    const time64 secs2 = 21 * sec_per_yr + 11 * sec_per_mo +
        19 * sec_per_day + 21 * 3600 + 9 * 60 + 48; /* 1991-11-19 21:09:48 Z */

    time64 n0 = compute_noon_of_day (&secs);
    time64 na = compute_noon_of_day (&secs1);
    time64 nb = compute_noon_of_day (&secs2);

    time64 r0 = time64CanonicalDayTime (secs);
    time64 ra = time64CanonicalDayTime (secs1);
    time64 rb = time64CanonicalDayTime (secs2);

    EXPECT_EQ(n0, r0);
    EXPECT_EQ(na, ra);
    EXPECT_EQ(nb, rb);

}

/* gnc_date_get_last_mday
int gnc_date_get_last_mday (int month, int year)// C: 1  Local: 1:0:0
*/
TEST(GNCDate, gnc_date_get_last_mday)
{
    EXPECT_EQ(gnc_date_get_last_mday (0, 1975), 31);
    EXPECT_EQ(gnc_date_get_last_mday (0, 1980), 31);
    EXPECT_EQ(gnc_date_get_last_mday (1, 1975), 28);
    EXPECT_EQ(gnc_date_get_last_mday (1, 1980), 29);
    EXPECT_EQ(gnc_date_get_last_mday (2, 1975), 31);
    EXPECT_EQ(gnc_date_get_last_mday (2, 1980), 31);
    EXPECT_EQ(gnc_date_get_last_mday (3, 1975), 30);
    EXPECT_EQ(gnc_date_get_last_mday (3, 1980), 30);
    EXPECT_EQ(gnc_date_get_last_mday (4, 1975), 31);
    EXPECT_EQ(gnc_date_get_last_mday (4, 1980), 31);
    EXPECT_EQ(gnc_date_get_last_mday (5, 1975), 30);
    EXPECT_EQ(gnc_date_get_last_mday (5, 1980), 30);
    EXPECT_EQ(gnc_date_get_last_mday (6, 1975), 31);
    EXPECT_EQ(gnc_date_get_last_mday (6, 1980), 31);
    EXPECT_EQ(gnc_date_get_last_mday (7, 1975), 31);
    EXPECT_EQ(gnc_date_get_last_mday (7, 1980), 31);
    EXPECT_EQ(gnc_date_get_last_mday (8, 1975), 30);
    EXPECT_EQ(gnc_date_get_last_mday (8, 1980), 30);
    EXPECT_EQ(gnc_date_get_last_mday (9, 1975), 31);
    EXPECT_EQ(gnc_date_get_last_mday (9, 1980), 31);
    EXPECT_EQ(gnc_date_get_last_mday (10, 1975), 30);
    EXPECT_EQ(gnc_date_get_last_mday (10, 1980), 30);
    EXPECT_EQ(gnc_date_get_last_mday (11, 1975), 31);
    EXPECT_EQ(gnc_date_get_last_mday (11, 1980), 31);
    EXPECT_EQ(gnc_date_get_last_mday (1, 2000), 29);
    EXPECT_EQ(gnc_date_get_last_mday (1, 2100), 28);
    EXPECT_EQ(gnc_date_get_last_mday (1, 2400), 29);
}

TEST(GNCDate, qof_date_format_set)
{
    gchar msg[] = "[qof_date_format_set()] non-existent date format set attempted. Setting ISO default";
    auto loglevel = G_LOG_LEVEL_CRITICAL;
    gchar logdomain[] = "qof.engine";
    TestErrorStruct check = {loglevel, logdomain, msg, GLogLevelFlags(0)};
    auto hdlr = g_log_set_handler (logdomain, loglevel, (GLogFunc)test_checked_handler, &check);
    qof_date_format_set ((QofDateFormat)((guint)DATE_FORMAT_LAST + 97));
    EXPECT_EQ(qof_date_format_get(),  (int)QOF_DATE_FORMAT_ISO);
    EXPECT_EQ(check.hits,1u);

    qof_date_format_set (QOF_DATE_FORMAT_UK);
    EXPECT_EQ(qof_date_format_get (), QOF_DATE_FORMAT_UK);
    EXPECT_EQ(check.hits,1u);
    g_log_remove_handler (logdomain, hdlr);
}

#ifdef HAVE_LANGINFO_D_FMT
#include <langinfo.h>
#  define GNC_D_FMT (nl_langinfo (D_FMT))
#  define GNC_D_T_FMT (nl_langinfo (D_T_FMT))
#  define GNC_T_FMT (nl_langinfo (T_FMT))
#elif defined(G_OS_WIN32)
#  define GNC_D_FMT (qof_win32_get_time_format(QOF_WIN32_PICTURE_DATE))
#  define GNC_T_FMT (qof_win32_get_time_format(QOF_WIN32_PICTURE_TIME))
#  define GNC_D_T_FMT (qof_win32_get_time_format(QOF_WIN32_PICTURE_DATETIME))
#else
#  define GNC_D_FMT "%Y-%m-%d"
#  define GNC_D_T_FMT "%Y-%m-%d %r"
#  define GNC_T_FMT "%r"
#endif

static void tm_set_dmy (struct tm *tm, gint year, gint month, gint mday)
{
    tm->tm_year = year - 1900;
    tm->tm_mon = month - 1;
    tm->tm_mday = mday;
}

TEST(GNCDate, qof_print_date_dmy_buff)
{
    gchar buff[MAX_DATE_LENGTH + 1], t_buff[MAX_DATE_LENGTH + 1];
    gchar *locale = g_strdup (setlocale (LC_TIME, nullptr));
    struct tm tm = { 0, 0, 0, 0, 0, 0, 0, 0, 0
#ifdef HAVE_STRUCT_TM_GMTOFF
        , 0, 0
#endif
    };

    qof_date_format_set (QOF_DATE_FORMAT_UK);
    memset ((gpointer)buff, 0, sizeof (buff));
    auto len = qof_print_date_dmy_buff (buff, MAX_DATE_LENGTH, 23, 11, 1974);
    EXPECT_EQ(len, strlen (buff));
    EXPECT_STREQ(buff, "23/11/1974");
    memset ((gpointer)buff, 0, sizeof (buff));
    len = qof_print_date_dmy_buff (buff, MAX_DATE_LENGTH, 2, 2, 1961);
    EXPECT_EQ(len, strlen (buff));
    EXPECT_STREQ(buff, "02/02/1961");
    memset ((gpointer)buff, 0, sizeof (buff));
    len = qof_print_date_dmy_buff (buff, MAX_DATE_LENGTH, 16, 6, 2045);
    EXPECT_EQ(len, strlen (buff));
    EXPECT_STREQ(buff, "16/06/2045");

    qof_date_format_set (QOF_DATE_FORMAT_CE);
    memset ((gpointer)buff, 0, sizeof (buff));
    len = qof_print_date_dmy_buff (buff, MAX_DATE_LENGTH, 23, 11, 1974);
    EXPECT_EQ(len, strlen (buff));
    EXPECT_STREQ(buff, "23.11.1974");
    memset ((gpointer)buff, 0, sizeof (buff));
    len = qof_print_date_dmy_buff (buff, MAX_DATE_LENGTH, 2, 2, 1961);
    EXPECT_EQ(len, strlen (buff));
    EXPECT_STREQ(buff, "02.02.1961");
    memset ((gpointer)buff, 0, sizeof (buff));
    len = qof_print_date_dmy_buff (buff, MAX_DATE_LENGTH, 16, 6, 2045);
    EXPECT_EQ(len, strlen (buff));
    EXPECT_STREQ(buff, "16.06.2045");

    qof_date_format_set (QOF_DATE_FORMAT_US);
    memset ((gpointer)buff, 0, sizeof (buff));
    len = qof_print_date_dmy_buff (buff, MAX_DATE_LENGTH, 23, 11, 1974);
    EXPECT_EQ(len, strlen (buff));
    EXPECT_STREQ(buff, "11/23/1974");
    memset ((gpointer)buff, 0, sizeof (buff));
    len = qof_print_date_dmy_buff (buff, MAX_DATE_LENGTH, 2, 2, 1961);
    EXPECT_EQ(len, strlen (buff));
    EXPECT_STREQ(buff, "02/02/1961");
    memset ((gpointer)buff, 0, sizeof (buff));
    len = qof_print_date_dmy_buff (buff, MAX_DATE_LENGTH, 16, 6, 2045);
    EXPECT_EQ(len, strlen (buff));
    EXPECT_STREQ(buff, "06/16/2045");

    qof_date_format_set (QOF_DATE_FORMAT_ISO);
    memset ((gpointer)buff, 0, sizeof (buff));
    len = qof_print_date_dmy_buff (buff, MAX_DATE_LENGTH, 23, 11, 1974);
    EXPECT_EQ(len, strlen (buff));
    EXPECT_STREQ(buff, "1974-11-23");
    memset ((gpointer)buff, 0, sizeof (buff));
    len = qof_print_date_dmy_buff (buff, MAX_DATE_LENGTH, 2, 2, 1961);
    EXPECT_EQ(len, strlen (buff));
    EXPECT_STREQ(buff, "1961-02-02");
    memset ((gpointer)buff, 0, sizeof (buff));
    len = qof_print_date_dmy_buff (buff, MAX_DATE_LENGTH, 16, 6, 2045);
    EXPECT_EQ(len, strlen (buff));
    EXPECT_STREQ(buff, "2045-06-16");

    qof_date_format_set (QOF_DATE_FORMAT_LOCALE);
    test_gnc_setlocale (LC_TIME, "en_US");
    tm_set_dmy (&tm, 1974, 11, 23);
    strftime(t_buff, MAX_DATE_LENGTH, GNC_D_FMT, &tm);
    memset ((gpointer)buff, 0, sizeof (buff));
    len = qof_print_date_dmy_buff (buff, MAX_DATE_LENGTH, tm.tm_mday, tm.tm_mon + 1, tm.tm_year + 1900);
    EXPECT_EQ(len, strlen (buff));
    EXPECT_STREQ(buff, t_buff);


    tm_set_dmy (&tm, 1961, 2, 2);
    strftime(t_buff, MAX_DATE_LENGTH, GNC_D_FMT, &tm);
    memset ((gpointer)buff, 0, sizeof (buff));
    len = qof_print_date_dmy_buff (buff, MAX_DATE_LENGTH, tm.tm_mday, tm.tm_mon + 1, tm.tm_year + 1900);
    EXPECT_EQ(len, strlen (buff));
    EXPECT_STREQ(buff, t_buff);
    memset ((gpointer)buff, 0, sizeof (buff));
    tm_set_dmy (&tm, 2045, 6, 16);
    strftime(t_buff, MAX_DATE_LENGTH, GNC_D_FMT, &tm);
    memset ((gpointer)buff, 0, sizeof (buff));
    len = qof_print_date_dmy_buff (buff, MAX_DATE_LENGTH, tm.tm_mday, tm.tm_mon + 1, tm.tm_year + 1900);
    EXPECT_EQ(len, strlen (buff));
    EXPECT_STREQ(buff, t_buff);

    test_gnc_setlocale (LC_TIME, "en_GB");
    tm_set_dmy (&tm, 1974, 11, 23);
    strftime(t_buff, MAX_DATE_LENGTH, GNC_D_FMT, &tm);
    memset ((gpointer)buff, 0, sizeof (buff));
    len = qof_print_date_dmy_buff (buff, MAX_DATE_LENGTH, tm.tm_mday, tm.tm_mon + 1, tm.tm_year + 1900);
    EXPECT_EQ(len, strlen (buff));
    EXPECT_STREQ(buff, t_buff);
    tm_set_dmy (&tm, 1961, 2, 2);
    strftime(t_buff, MAX_DATE_LENGTH, GNC_D_FMT, &tm);
    memset ((gpointer)buff, 0, sizeof (buff));
    len = qof_print_date_dmy_buff (buff, MAX_DATE_LENGTH, tm.tm_mday, tm.tm_mon + 1, tm.tm_year + 1900);
    EXPECT_EQ(len, strlen (buff));
    EXPECT_STREQ(buff, t_buff);
    memset ((gpointer)buff, 0, sizeof (buff));
    tm_set_dmy (&tm, 2045, 6, 16);
    strftime(t_buff, MAX_DATE_LENGTH, GNC_D_FMT, &tm);
    memset ((gpointer)buff, 0, MAX_DATE_LENGTH);
    len = qof_print_date_dmy_buff (buff, MAX_DATE_LENGTH, tm.tm_mday, tm.tm_mon + 1, tm.tm_year + 1900);
    EXPECT_EQ(len, strlen (buff));
    EXPECT_STREQ(buff, t_buff);

    test_gnc_setlocale (LC_TIME, "fr_FR");
    tm_set_dmy (&tm, 1974, 11, 23);
    strftime(t_buff, MAX_DATE_LENGTH, GNC_D_FMT, &tm);
    memset ((gpointer)buff, 0, sizeof (buff));
    len = qof_print_date_dmy_buff (buff, MAX_DATE_LENGTH, tm.tm_mday, tm.tm_mon + 1, tm.tm_year + 1900);
    EXPECT_EQ(len, strlen (buff));
    EXPECT_STREQ(buff, t_buff);
    tm_set_dmy (&tm, 1961, 2, 2);
    strftime(t_buff, MAX_DATE_LENGTH, GNC_D_FMT, &tm);
    memset ((gpointer)buff, 0, sizeof (buff));
    len = qof_print_date_dmy_buff (buff, MAX_DATE_LENGTH, tm.tm_mday, tm.tm_mon + 1, tm.tm_year + 1900);
    EXPECT_EQ(len, strlen (buff));
    EXPECT_STREQ(buff, t_buff);
    memset ((gpointer)buff, 0, sizeof (buff));
    tm_set_dmy (&tm, 2045, 6, 16);
    strftime(t_buff, MAX_DATE_LENGTH, GNC_D_FMT, &tm);
    memset ((gpointer)buff, 0, sizeof (buff));
    qof_print_date_dmy_buff (buff, MAX_DATE_LENGTH, tm.tm_mday, tm.tm_mon + 1, tm.tm_year + 1900);
    EXPECT_EQ(len, strlen (buff));
    EXPECT_STREQ(buff, t_buff);

    setlocale (LC_TIME, locale);
    g_free (locale);
}

/* Different distros/OSes define localization date formats. Apple, for
 * example, uses %d.%m.%Y for fr_FR and %d/%m/%Y for en_GB, while
 * Debian uses %d/%m/%Y and %d/%m/%y respectively. So to get a test
 * that works on all of them, we need to check the localized
 * strftime().
 *
 * This is a macro so that the line number in the assert message will
 * be right.
 */

#define test_assert_localized_timestring(time, datestr)                 \
    {                                                                   \
        gchar t_buff[MAX_DATE_LENGTH + 1];                                  \
        struct tm *ltime = gnc_localtime ((time64 *)(&time));           \
        strftime (t_buff, sizeof (t_buff), GNC_D_FMT, ltime);           \
        gnc_tm_free (ltime);                                            \
        EXPECT_STREQ(datestr, t_buff);                          \
    }


/* qof_print_date_buff
size_t
qof_print_date_buff (char * buff, size_t len, time64 t)// C: 3 in 1  Local: 2:0:0
*/
TEST(GNCDate, qof_print_date_buff)
{
    gchar buff[MAX_DATE_LENGTH + 1], ans[MAX_DATE_LENGTH + 1];
    gchar *locale = g_strdup (setlocale (LC_TIME, nullptr));

    struct tm tm1 = {0, 0, 12, 23, 10, 74};
    struct tm tm2 = {0, 0, 12, 2, 1, 61};
    struct tm tm3 = {0, 0, 12, 16, 5, 145};
    time64 time1 = gnc_mktime(&tm1);
    time64 time2 = gnc_mktime(&tm2);
    time64 time3 = gnc_mktime(&tm3);

    qof_date_format_set (QOF_DATE_FORMAT_UK);
    memset ((gpointer)buff, 0, sizeof (buff));
    auto len = qof_print_date_buff (buff, MAX_DATE_LENGTH, time1);
    EXPECT_EQ(len, strlen (buff));
    EXPECT_STREQ(buff, "23/11/1974");
    memset ((gpointer)buff, 0, sizeof (buff));
    len = qof_print_date_buff (buff, MAX_DATE_LENGTH, time2);
    EXPECT_EQ(len, strlen (buff));
    EXPECT_STREQ(buff, "02/02/1961");

    memset ((gpointer)buff, 0, sizeof (buff));
    len = qof_print_date_buff (buff, MAX_DATE_LENGTH, time3);
    EXPECT_EQ(len, strlen (buff));
    EXPECT_STREQ(buff, "16/06/2045");

    qof_date_format_set (QOF_DATE_FORMAT_CE);
    memset ((gpointer)buff, 0, sizeof (buff));
    len = qof_print_date_buff (buff, MAX_DATE_LENGTH, time1);
    EXPECT_EQ(len, strlen (buff));
    EXPECT_STREQ(buff, "23.11.1974");
    memset ((gpointer)buff, 0, sizeof (buff));
    len = qof_print_date_buff (buff, MAX_DATE_LENGTH, time2);
    EXPECT_EQ(len, strlen (buff));
    EXPECT_STREQ(buff, "02.02.1961");

    memset ((gpointer)buff, 0, sizeof (buff));
    len = qof_print_date_buff (buff, MAX_DATE_LENGTH, time3);
    EXPECT_EQ(len, strlen (buff));
    EXPECT_STREQ(buff, "16.06.2045");

    qof_date_format_set (QOF_DATE_FORMAT_US);
    memset ((gpointer)buff, 0, sizeof (buff));
    len = qof_print_date_buff (buff, MAX_DATE_LENGTH, time1);
    EXPECT_EQ(len, strlen (buff));
    EXPECT_STREQ(buff, "11/23/1974");
    memset ((gpointer)buff, 0, sizeof (buff));
    len = qof_print_date_buff (buff, MAX_DATE_LENGTH, time2);
    EXPECT_EQ(len, strlen (buff));
    EXPECT_STREQ(buff, "02/02/1961");

    memset ((gpointer)buff, 0, sizeof (buff));
    len = qof_print_date_buff (buff, MAX_DATE_LENGTH, time3);
    EXPECT_EQ(len, strlen (buff));
    EXPECT_STREQ(buff, "06/16/2045");

    qof_date_format_set (QOF_DATE_FORMAT_ISO);
    memset ((gpointer)buff, 0, sizeof (buff));
    len = qof_print_date_buff (buff, MAX_DATE_LENGTH, time1);
    EXPECT_EQ(len, strlen (buff));
    EXPECT_STREQ(buff, "1974-11-23");
    memset ((gpointer)buff, 0, sizeof (buff));
    len = qof_print_date_buff (buff, MAX_DATE_LENGTH, time2);
    EXPECT_EQ(len, strlen (buff));
    EXPECT_STREQ(buff, "1961-02-02");

    memset ((gpointer)buff, 0, sizeof (buff));
    len = qof_print_date_buff (buff, MAX_DATE_LENGTH, time3);
    EXPECT_EQ(len, strlen (buff));
    EXPECT_STREQ(buff, "2045-06-16");

    qof_date_format_set (QOF_DATE_FORMAT_LOCALE);
    test_gnc_setlocale (LC_TIME, "en_US");
    memset ((gpointer)buff, 0, sizeof (buff));
    len = qof_print_date_buff (buff, MAX_DATE_LENGTH, time1);
    EXPECT_EQ(len, strlen (buff));
    EXPECT_STREQ(buff, "11/23/1974");
    memset ((gpointer)buff, 0, sizeof (buff));
    len = qof_print_date_buff (buff, MAX_DATE_LENGTH, time2);
    EXPECT_EQ(len, strlen (buff));
    EXPECT_STREQ(buff, "02/02/1961");

    memset ((gpointer)buff, 0, sizeof (buff));
    len = qof_print_date_buff (buff, MAX_DATE_LENGTH, time3);
    EXPECT_EQ(len, strlen (buff));
    EXPECT_STREQ(buff, "06/16/2045");

    test_gnc_setlocale (LC_TIME, "en_GB");
    memset ((gpointer)buff, 0, sizeof (buff));
    len = qof_print_date_buff (buff, MAX_DATE_LENGTH, time1);
    EXPECT_EQ(len, strlen (buff));
    strftime(ans, MAX_DATE_LENGTH, GNC_D_FMT, &tm1);
    EXPECT_STREQ(buff, ans);
    memset ((gpointer)buff, 0, sizeof (buff));
    len = qof_print_date_buff (buff, MAX_DATE_LENGTH, time2);
    EXPECT_EQ(len, strlen (buff));
    strftime(ans, MAX_DATE_LENGTH, GNC_D_FMT, &tm2);
    EXPECT_STREQ(buff, ans);
    memset ((gpointer)buff, 0, sizeof (buff));
    len = qof_print_date_buff (buff, MAX_DATE_LENGTH, time3);
    EXPECT_EQ(len, strlen (buff));
    strftime(ans, MAX_DATE_LENGTH, GNC_D_FMT, &tm3);
    EXPECT_STREQ(buff, ans);

    test_gnc_setlocale (LC_TIME, "fr_FR");
    memset ((gpointer)buff, 0, sizeof (buff));
    len = qof_print_date_buff (buff, MAX_DATE_LENGTH, time1);
    EXPECT_EQ(len, strlen (buff));
    strftime(ans, MAX_DATE_LENGTH, GNC_D_FMT, &tm1);
    EXPECT_STREQ(buff, ans);
    memset ((gpointer)buff, 0, sizeof (buff));
    len = qof_print_date_buff (buff, MAX_DATE_LENGTH, time2);
    EXPECT_EQ(len, strlen (buff));
    strftime(ans, MAX_DATE_LENGTH, GNC_D_FMT, &tm2);
    EXPECT_STREQ(buff, ans);
    memset ((gpointer)buff, 0, sizeof (buff));
    len = qof_print_date_buff (buff, MAX_DATE_LENGTH, time3);
    EXPECT_EQ(len, strlen (buff));
    strftime(ans, MAX_DATE_LENGTH, GNC_D_FMT, &tm3);
    EXPECT_STREQ(buff, ans);

    setlocale (LC_TIME, locale);
    g_free (locale);
}
/* qof_print_gdate
size_t
qof_print_gdate( char *buf, size_t len, const GDate *gd )// C: 6 in 5  Local: 0:0:0
*/
TEST(GNCDate, qof_print_gdate)
{
    gchar buff[MAX_DATE_LENGTH + 1], t_buff[MAX_DATE_LENGTH + 1];
    gchar *locale = g_strdup (setlocale (LC_TIME, nullptr));
    GDate *gd1 = g_date_new_dmy (23, GDateMonth(11), 1974);
    GDate *gd2 = g_date_new_dmy (2, GDateMonth(2), 1961);
    GDate *gd3 = g_date_new_dmy (16, GDateMonth(6), 2045);

    qof_date_format_set (QOF_DATE_FORMAT_UK);
    memset ((gpointer)buff, 0, sizeof (buff));
    auto len = qof_print_gdate (buff, MAX_DATE_LENGTH, gd1);
    EXPECT_EQ(len, strlen (buff));
    EXPECT_STREQ(buff, "23/11/1974");
    memset ((gpointer)buff, 0, sizeof (buff));
    len = qof_print_gdate (buff, MAX_DATE_LENGTH, gd2);
    EXPECT_EQ(len, strlen (buff));
    EXPECT_STREQ(buff, "02/02/1961");
    memset ((gpointer)buff, 0, sizeof (buff));
    len = qof_print_gdate (buff, MAX_DATE_LENGTH, gd3);
    EXPECT_EQ(len, strlen (buff));
    EXPECT_STREQ(buff, "16/06/2045");

    qof_date_format_set (QOF_DATE_FORMAT_CE);
    memset ((gpointer)buff, 0, sizeof (buff));
    len = qof_print_gdate (buff, MAX_DATE_LENGTH, gd1);
    EXPECT_EQ(len, strlen (buff));
    EXPECT_STREQ(buff, "23.11.1974");
    memset ((gpointer)buff, 0, sizeof (buff));
    len = qof_print_gdate (buff, MAX_DATE_LENGTH, gd2);
    EXPECT_EQ(len, strlen (buff));
    EXPECT_STREQ(buff, "02.02.1961");
    memset ((gpointer)buff, 0, sizeof (buff));
    len = qof_print_gdate (buff, MAX_DATE_LENGTH, gd3);
    EXPECT_EQ(len, strlen (buff));
    EXPECT_STREQ(buff, "16.06.2045");


    qof_date_format_set (QOF_DATE_FORMAT_US);
    memset ((gpointer)buff, 0, sizeof (buff));
    len = qof_print_gdate (buff, MAX_DATE_LENGTH, gd1);
    EXPECT_EQ(len, strlen (buff));
    EXPECT_STREQ(buff, "11/23/1974");
    memset ((gpointer)buff, 0, sizeof (buff));
    len = qof_print_gdate (buff, MAX_DATE_LENGTH, gd2);
    EXPECT_EQ(len, strlen (buff));
    EXPECT_STREQ(buff, "02/02/1961");
    memset ((gpointer)buff, 0, sizeof (buff));
    len = qof_print_gdate (buff, MAX_DATE_LENGTH, gd3);
    EXPECT_EQ(len, strlen (buff));
    EXPECT_STREQ(buff, "06/16/2045");


    qof_date_format_set (QOF_DATE_FORMAT_ISO);
    memset ((gpointer)buff, 0, sizeof (buff));
    len = qof_print_gdate (buff, MAX_DATE_LENGTH, gd1);
    EXPECT_EQ(len, strlen (buff));
    EXPECT_STREQ(buff, "1974-11-23");
    memset ((gpointer)buff, 0, sizeof (buff));
    len = qof_print_gdate (buff, MAX_DATE_LENGTH, gd2);
    EXPECT_EQ(len, strlen (buff));
    EXPECT_STREQ(buff, "1961-02-02");
    memset ((gpointer)buff, 0, sizeof (buff));
    qof_print_gdate (buff, MAX_DATE_LENGTH, gd3);
    EXPECT_EQ(len, strlen (buff));
    EXPECT_STREQ(buff, "2045-06-16");


    qof_date_format_set (QOF_DATE_FORMAT_LOCALE);
    test_gnc_setlocale (LC_TIME, "en_US");
    memset ((gpointer)buff, 0, sizeof (buff));
    len = qof_print_gdate (buff, MAX_DATE_LENGTH, gd1);
    EXPECT_EQ(len, strlen (buff));
    g_date_strftime (t_buff, MAX_DATE_LENGTH, GNC_D_FMT, gd1);
    EXPECT_STREQ(buff, t_buff);
    memset ((gpointer)buff, 0, sizeof (buff));
    len = qof_print_gdate (buff, MAX_DATE_LENGTH, gd2);
    EXPECT_EQ(len, strlen (buff));
    g_date_strftime (t_buff, MAX_DATE_LENGTH, GNC_D_FMT, gd2);
    EXPECT_STREQ(buff, t_buff);
    memset ((gpointer)buff, 0, sizeof (buff));
    len = qof_print_gdate (buff, MAX_DATE_LENGTH, gd3);
    EXPECT_EQ(len, strlen (buff));
    g_date_strftime (t_buff, MAX_DATE_LENGTH, GNC_D_FMT, gd3);
    EXPECT_STREQ(buff, t_buff);

    test_gnc_setlocale (LC_TIME, "en_GB");
    memset ((gpointer)buff, 0, sizeof (buff));
    len = qof_print_gdate (buff, MAX_DATE_LENGTH, gd1);
    EXPECT_EQ(len, strlen (buff));
    g_date_strftime (t_buff, MAX_DATE_LENGTH, GNC_D_FMT, gd1);
    EXPECT_STREQ(buff, t_buff);
    memset ((gpointer)buff, 0, sizeof (buff));
    len = qof_print_gdate (buff, MAX_DATE_LENGTH, gd2);
    EXPECT_EQ(len, strlen (buff));
    g_date_strftime (t_buff, MAX_DATE_LENGTH, GNC_D_FMT, gd2);
    EXPECT_STREQ(buff, t_buff);
    memset ((gpointer)buff, 0, sizeof (buff));
    len = qof_print_gdate (buff, MAX_DATE_LENGTH, gd3);
    EXPECT_EQ(len, strlen (buff));
    g_date_strftime (t_buff, MAX_DATE_LENGTH, GNC_D_FMT, gd3);
    EXPECT_STREQ(buff, t_buff);


    test_gnc_setlocale (LC_TIME, "fr_FR");
    memset ((gpointer)buff, 0, sizeof (buff));
    len = qof_print_gdate (buff, MAX_DATE_LENGTH, gd1);
    EXPECT_EQ(len, strlen (buff));
    g_date_strftime (t_buff, MAX_DATE_LENGTH, GNC_D_FMT, gd1);
    EXPECT_STREQ(buff, t_buff);
    memset ((gpointer)buff, 0, sizeof (buff));
    len = qof_print_gdate (buff, MAX_DATE_LENGTH, gd2);
    EXPECT_EQ(len, strlen (buff));
    g_date_strftime (t_buff, MAX_DATE_LENGTH, GNC_D_FMT, gd2);
    EXPECT_STREQ(buff, t_buff);
    memset ((gpointer)buff, 0, sizeof (buff));
    len = qof_print_gdate (buff, MAX_DATE_LENGTH, gd3);
    EXPECT_EQ(len, strlen (buff));
    g_date_strftime (t_buff, MAX_DATE_LENGTH, GNC_D_FMT, gd3);
    EXPECT_STREQ(buff, t_buff);

    setlocale (LC_TIME, locale);
    g_free (locale);
    g_date_free (gd1);
    g_date_free (gd2);
    g_date_free (gd3);
}

#define test_assert_qof_print_date(time, datestr)  \
    {                                              \
        gchar *buf = qof_print_date (time);        \
        EXPECT_STREQ(buf, datestr);        \
        g_free (buf);                              \
    }

#define test_assert_qof_print_date_outside_range(time, datestr)  \
    {                                              \
        gchar *buf = qof_print_date (time);        \
        EXPECT_STREQ(buf, datestr);        \
        g_free (buf);                              \
    }

#define test_assert_localized_qof_print_date(time)           \
    {                                                        \
        gchar *buf = qof_print_date (time);                  \
        test_assert_localized_timestring (time, buf);        \
        g_free (buf);                                        \
    }

/* qof_print_date
char *
qof_print_date (time64 t)// C: 29 in 13  Local: 0:0:0
*/
TEST(GNCDate, qof_print_date)
{
    gchar *locale = g_strdup (setlocale (LC_TIME, nullptr));
    char ans[MAX_DATE_LENGTH];
    struct tm tm1 = {0, 0, 12, 23, 10, 74};
    struct tm tm2 = {0, 0, 12, 2, 1, 61};
    struct tm tm3 = {0, 0, 12, 16, 5, 145};
    time64 time1 = gnc_mktime(&tm1);
    time64 time2 = gnc_mktime(&tm2);
    time64 time3 = gnc_mktime(&tm3);

    qof_date_format_set (QOF_DATE_FORMAT_UK);
    test_assert_qof_print_date (time1, "23/11/1974");
    test_assert_qof_print_date_outside_range (time2, "02/02/1961");
    test_assert_qof_print_date_outside_range (time3, "16/06/2045");

    qof_date_format_set (QOF_DATE_FORMAT_CE);
    test_assert_qof_print_date (time1, "23.11.1974");
    test_assert_qof_print_date_outside_range (time2, "02.02.1961");
    test_assert_qof_print_date_outside_range (time3, "16.06.2045");

    qof_date_format_set (QOF_DATE_FORMAT_US);
    test_assert_qof_print_date (time1, "11/23/1974");
    test_assert_qof_print_date_outside_range (time2, "02/02/1961");
    test_assert_qof_print_date_outside_range (time3, "06/16/2045");

    qof_date_format_set (QOF_DATE_FORMAT_ISO);
    test_assert_qof_print_date (time1, "1974-11-23");
    test_assert_qof_print_date_outside_range (time2, "1961-02-02");
    test_assert_qof_print_date_outside_range (time3, "2045-06-16");

    qof_date_format_set (QOF_DATE_FORMAT_LOCALE);
    test_gnc_setlocale (LC_TIME, "en_US");
    strftime(ans, MAX_DATE_LENGTH, GNC_D_FMT, &tm1);
    test_assert_qof_print_date (time1,ans);
    strftime(ans, MAX_DATE_LENGTH, GNC_D_FMT, &tm2);
    test_assert_qof_print_date_outside_range (time2, ans);
    strftime(ans, MAX_DATE_LENGTH, GNC_D_FMT, &tm3);
    test_assert_qof_print_date_outside_range (time3, ans);

    test_gnc_setlocale (LC_TIME, "en_GB");
    strftime(ans, MAX_DATE_LENGTH, GNC_D_FMT, &tm1);
    test_assert_qof_print_date (time1, ans);
    strftime(ans, MAX_DATE_LENGTH, GNC_D_FMT, &tm2);
    test_assert_qof_print_date_outside_range (time2, ans);
    strftime(ans, MAX_DATE_LENGTH, GNC_D_FMT, &tm3);
    test_assert_qof_print_date_outside_range (time3, ans);

    test_gnc_setlocale (LC_TIME, "fr_FR");
    strftime(ans, MAX_DATE_LENGTH, GNC_D_FMT, &tm1);
    test_assert_qof_print_date (time1, ans);
    strftime(ans, MAX_DATE_LENGTH, GNC_D_FMT, &tm2);
    test_assert_qof_print_date_outside_range (time2, ans);
    strftime(ans, MAX_DATE_LENGTH, GNC_D_FMT, &tm3);
    test_assert_qof_print_date_outside_range (time3,ans);

    setlocale (LC_TIME, locale);
    g_free (locale);
}
/* qof_scan_date
gboolean
qof_scan_date (const char *buff, int *day, int *month, int *year)// C: 7 in 3  Local: 0:0:0
*/
TEST(GNCDate, qof_scan_date)
{
    gchar *locale = g_strdup (setlocale (LC_TIME, nullptr));
    int day = 0, mo = 0, yr = 0;
    gint year, month;
    time64 now = gnc_time(nullptr);
    gchar buff[MAX_DATE_LENGTH];
    struct tm tm = { 0, 0, 0, 0, 0, 0, 0, 0, 0
#ifdef HAVE_STRUCT_TM_GMTOFF
        , 0, 0
#endif
    };
    gnc_localtime_r(&now, &tm);
    year = tm.tm_year + 1900;
    month = tm.tm_mon + 1;

    EXPECT_TRUE(!qof_scan_date (nullptr, &day, &mo, &yr));
    EXPECT_EQ(day, 0);
    EXPECT_EQ(mo, 0);
    EXPECT_EQ(yr, 0);

    qof_date_format_set (QOF_DATE_FORMAT_UTC);
    EXPECT_TRUE(qof_scan_date ("1974-11-23", &day, &mo, &yr));
    EXPECT_EQ(day, 23);
    EXPECT_EQ(mo, 11);
    EXPECT_EQ(yr, 1974);

    EXPECT_TRUE(qof_scan_date ("1961-2-2", &day, &mo, &yr));
    EXPECT_EQ(day, 2);
    EXPECT_EQ(mo, 2);
    EXPECT_EQ(yr, 1961);

    EXPECT_TRUE(qof_scan_date ("2045-6-16", &day, &mo, &yr));
    EXPECT_EQ(day, 16);
    EXPECT_EQ(mo, 6);
    EXPECT_EQ(yr, 2045);

    qof_date_format_set (QOF_DATE_FORMAT_US);
    EXPECT_TRUE(qof_scan_date ("11/23/1974", &day, &mo, &yr));
    EXPECT_EQ(day, 23);
    EXPECT_EQ(mo, 11);
    EXPECT_EQ(yr, 1974);

    EXPECT_TRUE(qof_scan_date ("2/2/1961", &day, &mo, &yr));
    EXPECT_EQ(day, 2);
    EXPECT_EQ(mo, 2);
    EXPECT_EQ(yr, 1961);

    EXPECT_TRUE(qof_scan_date ("6/16/2045", &day, &mo, &yr));
    EXPECT_EQ(day, 16);
    EXPECT_EQ(mo, 6);
    EXPECT_EQ(yr, 2045);

    EXPECT_TRUE(qof_scan_date ("11월23년1974", &day, &mo, &yr));
    EXPECT_EQ(day, 23);
    EXPECT_EQ(mo, 11);
    EXPECT_EQ(yr, 1974);

    EXPECT_TRUE(qof_scan_date ("11月23年1974", &day, &mo, &yr));
    EXPECT_EQ(day, 23);
    EXPECT_EQ(mo, 11);
    EXPECT_EQ(yr, 1974);

    qof_date_completion_set (QOF_DATE_COMPLETION_THISYEAR, 0);

    EXPECT_TRUE(qof_scan_date ("11-23", &day, &mo, &yr));
    EXPECT_EQ(day, 23);
    EXPECT_EQ(mo, 11);
    EXPECT_EQ(yr, year);

    EXPECT_TRUE(qof_scan_date ("23-11", &day, &mo, &yr));
    EXPECT_EQ(day, 23);
    EXPECT_EQ(mo, 11);
    EXPECT_EQ(yr, year);

    if (month < 10) /* Sliding window won't test well after October */
    {
        qof_date_completion_set (QOF_DATE_COMPLETION_SLIDING, month + 1);

        EXPECT_TRUE(qof_scan_date ("12-23", &day, &mo, &yr));
        EXPECT_EQ(day, 23);
        EXPECT_EQ(mo, 12);
        EXPECT_EQ(yr, year - 1);

        qof_date_completion_set (QOF_DATE_COMPLETION_THISYEAR, 0);
    }

    qof_date_format_set (QOF_DATE_FORMAT_UK);
    EXPECT_TRUE(qof_scan_date ("23/11/1974", &day, &mo, &yr));
    EXPECT_EQ(day, 23);
    EXPECT_EQ(mo, 11);
    EXPECT_EQ(yr, 1974);

    EXPECT_TRUE(qof_scan_date ("2/2/1961", &day, &mo, &yr));
    EXPECT_EQ(day, 2);
    EXPECT_EQ(mo, 2);
    EXPECT_EQ(yr, 1961);

    EXPECT_TRUE(qof_scan_date ("16/6/2045", &day, &mo, &yr));
    EXPECT_EQ(day, 16);
    EXPECT_EQ(mo, 6);
    EXPECT_EQ(yr, 2045);

    qof_date_format_set (QOF_DATE_FORMAT_LOCALE);
    test_gnc_setlocale (LC_TIME, "en_GB");
    tm_set_dmy (&tm, 1974, 11, 23);
    strftime (buff, MAX_DATE_LENGTH, GNC_D_FMT, &tm);
    EXPECT_TRUE(qof_scan_date (buff, &day, &mo, &yr));
    EXPECT_EQ(day, tm.tm_mday);
    EXPECT_EQ(mo, tm.tm_mon + 1);
    EXPECT_EQ(yr, tm.tm_year + 1900);

    tm_set_dmy (&tm, 1961,2, 2);
    strftime (buff, MAX_DATE_LENGTH, GNC_D_FMT, &tm);
    EXPECT_TRUE(qof_scan_date (buff, &day, &mo, &yr));
    EXPECT_EQ(day, tm.tm_mday);
    EXPECT_EQ(mo, tm.tm_mon + 1);
    /* Some locale date formats result in a 2-digit year, which strptime
     * interprets as being in the current century.
     */
    EXPECT_EQ(yr % 100, tm.tm_year % 100);

    tm_set_dmy (&tm, 2045, 6, 16);
    strftime (buff, MAX_DATE_LENGTH, GNC_D_FMT, &tm);
    EXPECT_TRUE(qof_scan_date (buff, &day, &mo, &yr));
    EXPECT_EQ(day, tm.tm_mday);
    EXPECT_EQ(mo, tm.tm_mon + 1);
    EXPECT_EQ(yr, tm.tm_year + 1900);

    setlocale (LC_TIME, locale);
    g_free (locale);
}
/* gnc_date_timestamp
gchar *
gnc_date_timestamp (void)// C: 2 in 2  Local: 0:0:0
*/
TEST(GNCDate, gnc_date_timestamp)
{
    time64 now = gnc_time(nullptr);
    gchar *timestr = gnc_date_timestamp ();
    struct tm tm0, tm1;
    gnc_localtime_r(&now, &tm0);
    EXPECT_TRUE(strptime (timestr, "%Y%m%d%H%M%S", &tm1));
    EXPECT_EQ(tm0.tm_year, tm1.tm_year);
    EXPECT_EQ(tm0.tm_mon, tm1.tm_mon);
    EXPECT_EQ(tm0.tm_mday, tm1.tm_mday);
    EXPECT_EQ(tm0.tm_hour, tm1.tm_hour);
    EXPECT_EQ(tm0.tm_min, tm1.tm_min);
    EXPECT_EQ(tm0.tm_sec, tm1.tm_sec);

    g_free (timestr);
}

TEST_F(GNCDateFixA, gnc_iso8601_to_time64_gmt)
{
    time64 t = gnc_iso8601_to_time64_gmt (nullptr);
    EXPECT_EQ(t, INT64_MAX);

    t = gnc_iso8601_to_time64_gmt ("");
    EXPECT_EQ(t, 0);

    t = gnc_iso8601_to_time64_gmt ("1989-03-27 13:43:27");
    EXPECT_EQ(t, m_t1);

    t = gnc_iso8601_to_time64_gmt ("2020-11-07 06:21:19 -05");
    EXPECT_EQ(t, m_t2);

    t = gnc_iso8601_to_time64_gmt ("2012-07-04 19:27:44.0+08:40");
    EXPECT_EQ(t, m_t3);

    t = gnc_iso8601_to_time64_gmt ("1961-09-22 17:53:19 -05");
    EXPECT_EQ(t, m_t4);

    t = gnc_iso8601_to_time64_gmt ("2061-01-25 23:21:19.0 -05:00");
    EXPECT_EQ(t, m_t5);
}
#define ISO8601_SIZE MAX_DATE_LENGTH + 4
static gchar*
format_timestring (time64 t, TZOffset tz)
{
    static const unsigned tzlen = MAX_DATE_LENGTH - 26;
    char fmt[] = "%Y-%m-%d %H:%M:%S";
    struct tm *tm;
    char buf[MAX_DATE_LENGTH + 1];
    char tzbuf[tzlen];
    memset(tzbuf, 0, sizeof(tzbuf));
    tm = gnc_gmtime(&t);
    memset (buf, 0, sizeof(buf));
    strftime(buf, sizeof(buf), fmt, tm);
    free(tm);
    return g_strdup(buf);
}

TEST_F(GNCDateFixA, gnc_time64_to_iso8601_buff)
{

    gchar buff[ISO8601_SIZE];
    gchar *time_str;
    time64 t = 0;
    gchar *end;

    memset (buff, 0, sizeof buff);

    end = gnc_time64_to_iso8601_buff (t, nullptr);
    EXPECT_EQ(end, nullptr);

    end = gnc_time64_to_iso8601_buff (m_t0, buff);
    EXPECT_EQ(end - buff, (int)strlen (buff));
    time_str = format_timestring (m_t0, m_off_zulu);
    EXPECT_STREQ(buff, time_str);
    g_free (time_str);

    end = gnc_time64_to_iso8601_buff (m_t1, buff);
    time_str = format_timestring (m_t1, m_off_zulu);
    EXPECT_STREQ(buff, time_str);
    g_free (time_str);


    end = gnc_time64_to_iso8601_buff (m_t2, buff);
    time_str = format_timestring (m_t2, m_off_05w);
    EXPECT_STREQ(buff, time_str);
    g_free (time_str);

    end = gnc_time64_to_iso8601_buff (m_t3, buff);
    time_str = format_timestring (m_t3, m_off_0840e);
    EXPECT_STREQ(buff, time_str);
    g_free (time_str);

    end = gnc_time64_to_iso8601_buff (m_t4, buff);
    time_str = format_timestring (m_t4, m_off_05w);
    EXPECT_STREQ(buff, time_str);
    g_free (time_str);

    end = gnc_time64_to_iso8601_buff (m_t5, buff);
    time_str = format_timestring (m_t5, m_off_05w);
    EXPECT_STREQ(buff, time_str);
    g_free (time_str);
}

/* gnc_dmy2time64
time64
gnc_dmy2time64 (int day, int month, int year)// C: 8 in 5  Local: 1:0:0
*/
TEST_F(GNCDateFixBegin, gnc_dmy2time64)
{
    gchar msg1[] = "[qof_dmy2time64()] Date computation error from Y-M-D 1257-7-2: Time value is outside the supported year range.";
    auto loglevel = GLogLevelFlags(G_LOG_LEVEL_WARNING | G_LOG_FLAG_FATAL);
    gchar logdomain[] = "qof.engine";
    TestErrorStruct check = {loglevel, logdomain, msg1, 0};
    GLogFunc hdlr = g_log_set_default_handler ((GLogFunc)test_null_handler, &check);
    g_test_log_set_fatal_handler ((GTestLogFatalFunc)test_checked_handler, &check);
    for (size_t i = 0; i < sizeof(m_test)/sizeof(TimeMap); ++i)
    {
#ifdef HAVE_STRUCT_TM_GMTOFF
        struct tm tm = {0, 0, 0, m_test[i].day, m_test[i].mon - 1,
                        m_test[i].yr - 1900, 0, 0, -1, 0, nullptr};
#else
        struct tm tm = {0, 0, 0, m_test[i].day, m_test[i].mon - 1,
                        m_test[i].yr - 1900, 0, 0, -1};
#endif
        time64 r_t = gnc_dmy2time64 (m_test[i].day, m_test[i].mon,
                                         m_test[i].yr);
        struct tm time1 = tm, time2 = tm;
        int offset = gnc_mktime(&time1) - gnc_timegm(&time2);
        if (m_test[i].secs == INT64_MAX)
            /* We use INT64_MAX as invalid time64.secs.
             * As we can't *add* to the max, we can ignore the tz offset in this case. */
            EXPECT_EQ(r_t, INT64_MAX);
        else
            EXPECT_EQ(r_t, m_test[i].secs - offset);
    }
    g_log_set_default_handler (hdlr, 0);
}
/* gnc_dmy2time64_end
time64
gnc_dmy2time64_end (int day, int month, int year)// C: 1  Local: 0:0:0
*/
TEST_F(GNCDateFixEnd, gnc_dmy2time64_end)
{
    gchar msg1[] = "[qof_dmy2time64_end()] Date computation error from Y-M-D 1257-7-2: Time value is outside the supported year range.";
    auto loglevel = GLogLevelFlags(G_LOG_LEVEL_CRITICAL | G_LOG_FLAG_FATAL);
    gchar logdomain[] = "qof.engine";
    TestErrorStruct check = {loglevel, logdomain, msg1, 0};
    GLogFunc hdlr = g_log_set_default_handler ((GLogFunc)test_null_handler, &check);
    g_test_log_set_fatal_handler ((GTestLogFatalFunc)test_checked_handler, &check);
    for (size_t i = 0; i < sizeof(m_test)/sizeof(TimeMap); ++i)
    {
#ifdef HAVE_STRUCT_TM_GMTOFF
        struct tm tm = {59, 59, 23, m_test[i].day, m_test[i].mon - 1,
                        m_test[i].yr - 1900, 0, 0, -1, 0, nullptr};
#else
        struct tm tm = {59, 59, 23, m_test[i].day, m_test[i].mon - 1,
                        m_test[i].yr - 1900, 0, 0, -1};
#endif
        time64 r_t = gnc_dmy2time64_end (m_test[i].day, m_test[i].mon,
                                             m_test[i].yr);
        int offset = gnc_mktime(&tm) - gnc_timegm(&tm);
        if (m_test[i].secs == INT64_MAX)
            /* We use INT64_MAX as invalid time64.secs.
             * As we can't *add* to the max, we can ignore the tz offset in this case. */
            EXPECT_EQ(r_t, INT64_MAX);
        else
            EXPECT_EQ(r_t, m_test[i].secs - offset);
    }
    g_log_set_default_handler (hdlr, 0);
}

/*gnc_dmy2time64_neutral*/
TEST_F(GNCDateFixNeurtal, gnc_dmy2time64_neutral)
{
    gchar msg1[] = "[qof_dmy2time64_neutral()] Date computation error from Y-M-D 1257-7-2: Time value is outside the supported year range.";
    auto loglevel = GLogLevelFlags(G_LOG_LEVEL_CRITICAL | G_LOG_FLAG_FATAL);
    gchar logdomain[] = "qof.engine";
    TestErrorStruct check = {loglevel, logdomain, msg1, 0};
    GLogFunc hdlr = g_log_set_default_handler ((GLogFunc)test_null_handler, &check);
    struct tm check_tz;
    gnc_localtime_r(&(m_test[0].secs), &check_tz);
    /* gnc_dmy2time64_neutral returns the time64 for 10:59:00 Z
     * for timezones in the range -11 to +13. If the timezone being
     * tested is outside that range then the day of the month will be
     * different from the one in the test fixture and we skip the
     * test.
     */
    if (check_tz.tm_mday == m_test[0].day)
    {
         g_test_log_set_fatal_handler ((GTestLogFatalFunc)test_checked_handler, &check);
         for (size_t i = 0; i < sizeof(m_test)/sizeof(TimeMap); ++i)
         {
              time64 r_t = gnc_dmy2time64_neutral (m_test[i].day, m_test[i].mon,
                                                       m_test[i].yr);

              EXPECT_EQ(r_t, m_test[i].secs);
         }
    }
    g_log_set_default_handler (hdlr, 0);
}

/* time64_to_gdate
GDate time64_to_gdate (time64 t)// C: 5 in 4  Local: 0:0:0
*/
TEST_F(GNCDateFixA, time64_to_gdate)
{
    GDate date1, date2;
    struct tm tm;

    g_date_clear (&date2, 1);

    date1 = time64_to_gdate (m_t0);
    gnc_localtime_r(&m_t0, &tm);
    g_date_set_dmy (&date2, tm.tm_mday, GDateMonth(tm.tm_mon + 1), tm.tm_year + 1900);
    EXPECT_EQ(g_date_get_julian (&date1),
                     g_date_get_julian (&date2));

    date1 = time64_to_gdate (m_t1);
    gnc_localtime_r(&m_t1, &tm);
    g_date_set_dmy (&date2, tm.tm_mday, GDateMonth(tm.tm_mon + 1), tm.tm_year + 1900);
    EXPECT_EQ(g_date_get_julian (&date1),
                     g_date_get_julian (&date2));

    date1 = time64_to_gdate (m_t2);
    gnc_localtime_r(&m_t2, &tm);
    g_date_set_dmy (&date2, tm.tm_mday, GDateMonth(tm.tm_mon + 1), tm.tm_year + 1900);
    EXPECT_EQ(g_date_get_julian (&date1),
                     g_date_get_julian (&date2));

    date1 = time64_to_gdate (m_t3);
    gnc_localtime_r(&m_t3, &tm);
    g_date_set_dmy (&date2, tm.tm_mday, GDateMonth(tm.tm_mon + 1), tm.tm_year + 1900);
    EXPECT_EQ(g_date_get_julian (&date1),
                     g_date_get_julian (&date2));

    date1 = time64_to_gdate (m_t4);
    gnc_localtime_r(&m_t4, &tm);
    g_date_set_dmy (&date2, tm.tm_mday, GDateMonth(tm.tm_mon + 1), tm.tm_year + 1900);
    EXPECT_EQ(g_date_get_julian (&date1),
                     g_date_get_julian (&date2));

    date1 = time64_to_gdate (m_t5);
    gnc_localtime_r(&m_t5, &tm);
    g_date_set_dmy (&date2, tm.tm_mday, GDateMonth(tm.tm_mon + 1), tm.tm_year + 1900);
    EXPECT_EQ(g_date_get_julian (&date1),
                     g_date_get_julian (&date2));
}

/* gdate_to_time64
time64 gdate_to_time64 (GDate d)// C: 7 in 6  Local: 0:0:0
*/
TEST_F(GNCDateFixNeurtal, gdate_to_time64)
{

    gchar msg[] = "g_date_set_dmy: assertion 'g_date_valid_dmy (day, m, y)' failed";
    auto loglevel = GLogLevelFlags(G_LOG_LEVEL_CRITICAL | G_LOG_FLAG_FATAL);
    TestErrorStruct check = {loglevel, G_LOG_DOMAIN, msg, 0};
    GLogFunc hdlr = g_log_set_default_handler ((GLogFunc)test_null_handler, &check);
    struct tm check_tz;
    gnc_localtime_r(&(m_test[0].secs), &check_tz);
    /* gdate_to_time64 returns the time64 for 10:59:00 Z
     * for timezones in the range -11 to +13. If the timezone being
     * tested is outside that range then the day of the month will be
     * different from the one in the test fixture and we skip the
     * test.
     */
    if (check_tz.tm_mday == m_test[0].day)
    {
         g_test_log_set_fatal_handler ((GTestLogFatalFunc)test_checked_handler, &check);
         for (size_t i = 0; i < sizeof(m_test)/sizeof(TimeMap); ++i)
         {
              GDate gd, gd2;
              time64 r_t;
              g_date_clear(&gd, 1);
              g_date_clear(&gd2, 1);
              g_date_set_dmy(&gd, m_test[i].day, GDateMonth(m_test[i].mon), m_test[i].yr);
              r_t = gdate_to_time64(gd);
              EXPECT_EQ(r_t, m_test[i].secs);
              if (m_test[i].secs < INT64_MAX)
              {
                   gd2 = time64_to_gdate(r_t);
                   EXPECT_EQ(g_date_compare (&gd2, &gd), 0);
              }
         }
    }
    g_log_set_default_handler (hdlr, 0);
}

static void
tm_day_begin(struct tm *tm)
{
    tm->tm_hour = 0;
    tm->tm_min = 0;
    tm->tm_sec = 0;
}

TEST_F(GNCDateFixA, gnc_time64_get_day_start)
{
    struct tm tm;
    time64 t_time, r_time;

    gnc_localtime_r(&m_t0, &tm);
    tm_day_begin(&tm);
    t_time = gnc_mktime(&tm);
    r_time = gnc_time64_get_day_start (m_t0);
    EXPECT_EQ(t_time, r_time);

    gnc_localtime_r(&m_t1, &tm);
    tm_day_begin(&tm);
    t_time = gnc_mktime(&tm);
    r_time = gnc_time64_get_day_start (m_t1);
    EXPECT_EQ(t_time, r_time);

    gnc_localtime_r(&m_t2, &tm);
    tm_day_begin(&tm);
    t_time = gnc_mktime(&tm);
    r_time = gnc_time64_get_day_start (m_t2);
    EXPECT_EQ(t_time, r_time);

    gnc_localtime_r(&m_t3, &tm);
    tm_day_begin(&tm);
    t_time = gnc_mktime(&tm);
    r_time = gnc_time64_get_day_start (m_t3);
    EXPECT_EQ(t_time, r_time);

    gnc_localtime_r(&m_t4, &tm);
    tm_day_begin(&tm);
    t_time = gnc_mktime(&tm);
    r_time = gnc_time64_get_day_start (m_t4);
    EXPECT_EQ(t_time, r_time);

    gnc_localtime_r(&m_t5, &tm);
    tm_day_begin(&tm);
    t_time = gnc_mktime(&tm);
    r_time = gnc_time64_get_day_start (m_t5);
    EXPECT_EQ(t_time, r_time);

}
/* gnc_time64_get_day_end
time64
gnc_time64_get_day_end (time64 time_val)// C: 12 in 8  Local: 0:0:0
*/
static void
tm_day_end(struct tm *tm)
{
    tm->tm_hour = 23;
    tm->tm_min = 59;
    tm->tm_sec = 59;
}

TEST_F(GNCDateFixA, gnc_time64_get_day_end)
{
    struct tm tm;
    time64 t_time, r_time;

    gnc_localtime_r(&m_t0, &tm);
    tm_day_end(&tm);
    t_time = gnc_mktime(&tm);
    r_time = gnc_time64_get_day_end (m_t0);
    EXPECT_EQ(t_time, r_time);

    gnc_localtime_r(&m_t1, &tm);
    tm_day_end(&tm);
    t_time = gnc_mktime(&tm);
    r_time = gnc_time64_get_day_end (m_t1);
    EXPECT_EQ(t_time, r_time);

    gnc_localtime_r(&m_t2, &tm);
    tm_day_end(&tm);
    t_time = gnc_mktime(&tm);
    r_time = gnc_time64_get_day_end (m_t2);
    EXPECT_EQ(t_time, r_time);

    gnc_localtime_r(&m_t3, &tm);
    tm_day_end(&tm);
    t_time = gnc_mktime(&tm);
    r_time = gnc_time64_get_day_end (m_t3);
    EXPECT_EQ(t_time, r_time);

    gnc_localtime_r(&m_t4, &tm);
    tm_day_end(&tm);
    t_time = gnc_mktime(&tm);
    r_time = gnc_time64_get_day_end (m_t4);
    EXPECT_EQ(t_time, r_time);

    gnc_localtime_r(&m_t5, &tm);
    tm_day_end(&tm);
    t_time = gnc_mktime(&tm);
    r_time = gnc_time64_get_day_end (m_t5);
    EXPECT_EQ(t_time, r_time);

}

int main(int argc, char **argv) {
    testing::InitGoogleTest(&argc, argv);

    setlocale (LC_ALL, "");
    qof_init(); 			/* Initialize the GObject system */
    qof_log_init_filename_special("stderr"); /* Init the log system */
    tz = g_time_zone_new_local();

    auto ret = RUN_ALL_TESTS();
    g_time_zone_unref(tz);
    return ret;
}
