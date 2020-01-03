/********************************************************************
 * utest-gnc-date.c: GLib g_test test suite for gnc-date.c.         *
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
#ifdef __cplusplus
extern "C"
{
#endif

#include <config.h>
#include "platform.h"
#include <string.h>
#include <stdlib.h>
#include <sys/time.h>
#include <glib.h>
#include <unittest-support.h>
/* Add specific headers for this class */

#ifdef __cplusplus
}
#endif
#include "../gnc-date.h"
#include "../gnc-date-p.h"
#include <locale.h>
#include <glib/gprintf.h>
#include <inttypes.h>
#ifndef HAVE_STRPTIME
#  include "strptime.h"
#endif

static const gchar *suitename = "/qof/gnc-date";
static const time64 secs_per_year = INT64_C(3600) * (INT64_C(24) * INT64_C(365) + 6);
static const time64 max_secs = (INT64_C(3600) * (INT64_C(24) * INT64_C(365) + 6)) * (INT64_C(9999) - INT64_C(1970));

typedef struct
{
    short hours;
    short minutes;
} TZOffset;

typedef struct
{
    TZOffset off_zulu;
    TZOffset off_05w;
    TZOffset off_0840e;
    time64 t0;
    time64 t1;
    time64 t2;
    time64 t3;
    time64 t4;
    time64 t5;
} FixtureA;

static int
offset_secs (TZOffset tz)
{
    return 3600 * tz.hours + 60 * tz.minutes;
}

static char*
offset_string (TZOffset tz)
{
    return g_strdup_printf("%+02d%02d", tz.hours, tz.minutes);
}

static void setup (FixtureA *f, gconstpointer pData)
{
    f->t0 = gnc_time(NULL);
    f->off_zulu = (TZOffset){0, 0};
    f->off_05w = (TZOffset){-5, 0};
    f->off_0840e = (TZOffset){8, 40};
    f->t1 = 607009407; //1989-3-27 13:43:27 Z
    f->t2 = 1604748079; //2020-11-7 06:21:19 -05:00
    f->t3 = 1341398864; //2012-07-04 19:27:44 +08:40
    f->t4 = -261104801; //1961-09-22 17:53:19 -05:00
    f->t5 = 2873938879LL; //2061-01-25 23:21:19 -05:00
}

typedef struct
{
    int yr;
    int mon;
    int day;
    time64 secs;
} TimeMap;

typedef struct
{
    TimeMap test[9];
} FixtureB;

static void
setup_begin(FixtureB *f, gconstpointer pData)
{
    f->test[0] = (TimeMap){1999, 7, 21, INT64_C(932515200)};
    f->test[1] = (TimeMap){1918, 3, 31, INT64_C(-1633305600)};
    f->test[2] = (TimeMap){1918, 4, 1, INT64_C(-1633219200)};
    f->test[3] = (TimeMap){2057, 11, 20, INT64_C(2773440000)};
    f->test[4] = (TimeMap){1257, 07, 02, INT64_MAX}; /*invalid year*/
    f->test[5] = (TimeMap){2017, 02, 29, INT64_MAX}; /*invalid day*/
    f->test[6] = (TimeMap){2017, 02, 33, INT64_MAX}; /*invalid day*/
    f->test[7] = (TimeMap){2017, 13, 29, INT64_MAX}; /*invalid month*/
    f->test[8] = (TimeMap){2017, 03, 16, INT64_C(1489622400)};
}

static void
setup_neutral(FixtureB *f, gconstpointer pData)
{
    f->test[0] = (TimeMap){1999, 7, 21, INT64_C(932554740)};
    f->test[1] = (TimeMap){1918, 3, 31, INT64_C(-1633266060)};
    f->test[2] = (TimeMap){1918, 4, 1, INT64_C(-1633179660)};
    f->test[3] = (TimeMap){2057, 11, 20, INT64_C(2773479540)};
    f->test[4] = (TimeMap){1257, 07, 02, INT64_MAX};
    f->test[5] = (TimeMap){2017, 02, 29, INT64_MAX};
    f->test[6] = (TimeMap){2017, 02, 33, INT64_MAX};
    f->test[7] = (TimeMap){2017, 13, 29, INT64_MAX};
    f->test[8] = (TimeMap){2017, 03, 16, INT64_C(1489661940)};
}

static void
setup_end(FixtureB *f, gconstpointer pData)
{
    f->test[0] = (TimeMap){1999, 7, 21, INT64_C(932601599)};
    f->test[1] = (TimeMap){1918, 3, 31, INT64_C(-1633219201)};
    f->test[2] = (TimeMap){1918, 4, 1, INT64_C(-1633132801)};
    f->test[3] = (TimeMap){2057, 11, 20, INT64_C(2773526399)};
    f->test[4] = (TimeMap){1257, 07, 02, INT64_MAX};
    f->test[5] = (TimeMap){2017, 02, 29, INT64_MAX};
    f->test[6] = (TimeMap){2017, 02, 33, INT64_MAX};
    f->test[7] = (TimeMap){2017, 13, 29, INT64_MAX};
    f->test[8] = (TimeMap){2017, 03, 16, INT64_C(1489708799)};
}

void test_suite_gnc_date ( void );
static GTimeZone *tz;
#define MAXTIME INT64_C(253402214400)
#define MINTIME INT64_C(-17987443200)

/* gnc_localtime just creates a tm on the heap and calls
 * gnc_localtime_r with it, so this suffices to test both.
 */
static void
test_gnc_localtime (void)
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
            g_assert (time == NULL);
            continue;
        }
        tsecs = (time_t)(secs[ind]);
        ans = localtime(&tsecs);
        g_assert_cmpint (time->tm_year, ==, ans->tm_year);
        g_assert_cmpint (time->tm_mon, ==, ans->tm_mon);
        g_assert_cmpint (time->tm_mday, ==, ans->tm_mday);
        g_assert_cmpint (time->tm_hour, ==, ans->tm_hour);
        g_assert_cmpint (time->tm_min, ==, ans->tm_min);
        g_assert_cmpint (time->tm_sec, ==, ans->tm_sec);
        g_assert_cmpint (time->tm_wday, ==, ans->tm_wday);
        g_assert_cmpint (time->tm_yday, ==, ans->tm_yday);
        g_assert_cmpint (time->tm_isdst, ==, ans->tm_isdst);
#ifdef HAVE_STRUCT_TM_GMTOFF
        g_assert_cmpint (time->tm_gmtoff, ==, ans->tm_gmtoff);
#endif
        gnc_tm_free (time);
    }
}

static void
test_gnc_gmtime (void)
{
    time64 secs[6] = {-15767956734LL, -1123692LL, 432761LL,
                      723349832LL, 887326459367LL, 1175964426LL
                     };
    struct tm answers[6] =
    {
#ifdef HAVE_STRUCT_TM_GMTOFF
        { 6, 1, 12, 2, 4, -430, 1, 121, 0, 0, NULL },
        { 48, 51, 23, 18, 11, 69, 4, 351, 0, 0, NULL },
        { 41, 12, 0, 6, 0, 70, 2, 5, 0, 0, NULL },
        { 32, 30, 2, 3, 11, 92, 4, 337, 0, 0, NULL },
        { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, NULL },
        { 6, 47, 16, 7, 3, 107, 6, 96, 0, 0, NULL },
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
            g_assert (time == NULL);
            continue;
        }

        g_assert_cmpint (time->tm_year, ==, answers[ind].tm_year);
        g_assert_cmpint (time->tm_mon, ==, answers[ind].tm_mon);
        g_assert_cmpint (time->tm_mday, ==, answers[ind].tm_mday);
        g_assert_cmpint (time->tm_hour, ==, answers[ind].tm_hour);
        g_assert_cmpint (time->tm_min, ==, answers[ind].tm_min);
        g_assert_cmpint (time->tm_sec, ==, answers[ind].tm_sec);
        g_assert_cmpint (time->tm_wday, ==, answers[ind].tm_wday);
        g_assert_cmpint (time->tm_yday, ==, answers[ind].tm_yday);
        g_assert_cmpint (time->tm_isdst, ==, -1);
#ifdef HAVE_STRUCT_TM_GMTOFF
        g_assert_cmpint (time->tm_gmtoff, ==, 0);
#endif
        gnc_tm_free (time);
    }
}

static void
test_gnc_mktime (void)
{
    time64 ans[5] =
        { -15752870334LL, -1123692LL, 432761LL, 723349832LL, 1175964426LL};

    struct tm time[5] =
    {
#ifdef HAVE_STRUCT_TM_GMTOFF
        { 6, 41, 2, 24, 9, -430, 0, 0, 1, 0, NULL },
        { 48, 51, 23, 18, 11, 69, 0, 0, -1, 0, NULL },
        { 41, 12, 0, 6, 0, 70, 0, 0, -1, 0, NULL },
        { 32, 30, 2, 3, 11, 92, 0, 0, -1, 0, NULL },
        { 6, 47, 16, 7, 3, 107, 0, 0, -1, 0, NULL },
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
        g_assert_cmpint (secs, ==, ans[ind] - offset);

    }
}

/* In addition to computing a time offset from a struct tm, mktime is
 * supposed to normalize struct tms with out-of-range values. This
 * second test exercises that facility in gnc_mktime.
 */
static void
test_gnc_mktime_normalization (void)
{
    time64 ans = 723349832LL;

    struct tm normal_time =
#ifdef HAVE_STRUCT_TM_GMTOFF
    {
        32, 30, 2, 3, 11, 92, 0, 0, -1, 0, NULL
    };
#else
    {
        32, 30, 2, 3, 11, 92, 0, 0, -1
    };
#endif

    struct tm time[4] =
    {
#ifdef HAVE_STRUCT_TM_GMTOFF
        { 92, -31, 27, -29, 24, 91, 0, 0, -1, 0, NULL },
        { -28, 91, -47, 35, -2, 93, 0, 0, -1, 0, NULL },
        { -28, 91, -47, 66, -3, 93, 0, 0, -1, 0, NULL },
        { -28, 91, -47, 35, -26, 95, 0, 0, -1, 0, NULL },
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

        g_assert_cmpfloat (time[ind].tm_sec, ==, normal_time.tm_sec);
        g_assert_cmpint (time[ind].tm_min, ==, normal_time.tm_min);
        g_assert_cmpint (time[ind].tm_hour, ==, normal_time.tm_hour);
        g_assert_cmpint (time[ind].tm_mday, ==, normal_time.tm_mday);
        g_assert_cmpint (time[ind].tm_mon, ==, normal_time.tm_mon);
        g_assert_cmpint (time[ind].tm_year, ==, normal_time.tm_year);
        g_assert_cmpint (secs, ==, ans - (calc_timegm - calc_time));
    }
}

static void
test_gnc_ctime (void)
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
        g_assert_cmpstr (datestr, ==, check_str);
        g_free (datestr);
    }
}

static void
test_gnc_time (void)
{
    time64 secs1, secs2;
    secs1 = gnc_time (&secs2);
    g_assert_cmpint (secs1, ==, secs2);
    g_assert_cmpint (secs1, ==, time(0));
}

/* gnc_difftime and gnc_tm_free are just too simple to bother testing. */

/* gnc_date_dateformat_to_string
const char *gnc_default_strftime_date_format =
const char*
gnc_date_dateformat_to_string(QofDateFormat format)// C: 1  Local: 0:0:0
*/

static void
test_gnc_date_dateformat_to_string (void)
{
    g_assert_cmpstr (gnc_date_dateformat_to_string (QOF_DATE_FORMAT_US), ==, "us");
    g_assert_cmpstr (gnc_date_dateformat_to_string (QOF_DATE_FORMAT_UK), ==, "uk");
    g_assert_cmpstr (gnc_date_dateformat_to_string (QOF_DATE_FORMAT_CE), ==, "ce");
    g_assert_cmpstr (gnc_date_dateformat_to_string (QOF_DATE_FORMAT_ISO), ==, "iso");
    g_assert_cmpstr (gnc_date_dateformat_to_string (QOF_DATE_FORMAT_UTC), ==, "utc");
    g_assert_cmpstr (gnc_date_dateformat_to_string (QOF_DATE_FORMAT_LOCALE), ==, "locale");
    g_assert_cmpstr (gnc_date_dateformat_to_string (QOF_DATE_FORMAT_CUSTOM), ==, "custom");
    g_assert_cmpstr (gnc_date_dateformat_to_string (QOF_DATE_FORMAT_UNSET), ==, "unset");

}
/* gnc_date_string_to_dateformat
gboolean
gnc_date_string_to_dateformat(const char* fmt_str, QofDateFormat *format)// C: 3 in 3  Local: 0:0:0
*/
static void
test_gnc_date_string_to_dateformat (void)
{
    QofDateFormat fmt = 123;
    g_assert (gnc_date_string_to_dateformat (NULL, &fmt));
    g_assert_cmpint (fmt, ==, 123);
    g_assert (!gnc_date_string_to_dateformat ("us", &fmt));
    g_assert_cmpint (fmt, ==, QOF_DATE_FORMAT_US);
    g_assert (!gnc_date_string_to_dateformat ("uk", &fmt));
    g_assert_cmpint (fmt, ==, QOF_DATE_FORMAT_UK);
    g_assert (!gnc_date_string_to_dateformat ("ce", &fmt));
    g_assert_cmpint (fmt, ==, QOF_DATE_FORMAT_CE);
    g_assert (!gnc_date_string_to_dateformat ("iso", &fmt));
    g_assert_cmpint (fmt, ==, QOF_DATE_FORMAT_ISO);
    g_assert (!gnc_date_string_to_dateformat ("utc", &fmt));
    g_assert_cmpint (fmt, ==, QOF_DATE_FORMAT_UTC);
    g_assert (!gnc_date_string_to_dateformat ("locale", &fmt));
    g_assert_cmpint (fmt, ==, QOF_DATE_FORMAT_LOCALE);
    g_assert (!gnc_date_string_to_dateformat ("custom", &fmt));
    g_assert_cmpint (fmt, ==, QOF_DATE_FORMAT_CUSTOM);
    g_assert (!gnc_date_string_to_dateformat ("unset", &fmt));
    g_assert_cmpint (fmt, ==, QOF_DATE_FORMAT_UNSET);
    fmt = 123;
    g_assert (gnc_date_string_to_dateformat ("", &fmt));
    g_assert_cmpint (fmt, ==, 123);
    g_assert (gnc_date_string_to_dateformat ("foo", &fmt));
    g_assert_cmpint (fmt, ==, 123);

}
/* gnc_date_monthformat_to_string
const char*
gnc_date_monthformat_to_string(GNCDateMonthFormat format)// C: 1  Local: 0:0:0
*/
static void
test_gnc_date_monthformat_to_string (void)
{
    g_assert_cmpstr (gnc_date_monthformat_to_string (GNCDATE_MONTH_NUMBER), ==, "number");
    g_assert_cmpstr (gnc_date_monthformat_to_string (GNCDATE_MONTH_ABBREV), ==, "abbrev");
    g_assert_cmpstr (gnc_date_monthformat_to_string (GNCDATE_MONTH_NAME), ==, "name");
    g_assert (gnc_date_monthformat_to_string (93) == NULL);
}
/* gnc_date_string_to_monthformat
gboolean
gnc_date_string_to_monthformat(const char *fmt_str, GNCDateMonthFormat *format)// C: 1  Local: 0:0:0
*/
static void
test_gnc_date_string_to_monthformat (void)
{
    GNCDateMonthFormat fmt = 123;
    g_assert (gnc_date_string_to_monthformat (NULL, &fmt));
    g_assert_cmpint (fmt, ==, 123);
    g_assert (!gnc_date_string_to_monthformat ("number", &fmt));
    g_assert_cmpint (fmt, ==, GNCDATE_MONTH_NUMBER);
    g_assert (!gnc_date_string_to_monthformat ("abbrev", &fmt));
    g_assert_cmpint (fmt, ==, GNCDATE_MONTH_ABBREV);
    g_assert (!gnc_date_string_to_monthformat ("name", &fmt));
    g_assert_cmpint (fmt, ==, GNCDATE_MONTH_NAME);
    fmt = 123;
    g_assert (gnc_date_string_to_monthformat ("", &fmt));
    g_assert_cmpint (fmt, ==, 123);
    g_assert (gnc_date_string_to_monthformat ("foo", &fmt));
    g_assert_cmpint (fmt, ==, 123);
}

static void
test_gnc_setlocale (int category, gchar *locale)
{
    gchar *suffixes[] = {"utf8", "UTF-8"};
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
    if (setlocale (category, locale) != NULL)
        return;

    for (i = 0; i < G_N_ELEMENTS (suffixes); i++)
    {
        gchar * modlocale = g_strdup_printf ("%s.%s", locale, suffixes[i]);
        gchar *localeval = setlocale (category, modlocale);
        g_free (modlocale);
        if (localeval != NULL)
            return;
    }
    g_fprintf (stderr, "There are some differences between distros in the way they name "
              "locales, and this can cause trouble with the locale-based "
              "formatting. If you get the assert in this function, run locale -a "
              "and make sure that en_US, en_GB, and fr_FR are installed and that "
              "if a suffix is needed it's in the suffixes array.");
    g_assert_not_reached ();
}
/* timespecCanonicalDayTime
time64
time64CanonicalDayTime(time64 t)// C: 12 in 5 SCM: 19 in 10 Local: 0:0:0
*/
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

static void
test_time64CanonicalDayTime (void)
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

    g_assert_cmpint (n0, ==, r0);
    g_assert_cmpint (na, ==, ra);
    g_assert_cmpint (nb, ==, rb);

}

/* gnc_date_get_last_mday
int gnc_date_get_last_mday (int month, int year)// C: 1  Local: 1:0:0
*/
static void
test_gnc_date_get_last_mday (void)
{
    g_assert_cmpint (gnc_date_get_last_mday (0, 1975), ==, 31);
    g_assert_cmpint (gnc_date_get_last_mday (0, 1980), ==, 31);
    g_assert_cmpint (gnc_date_get_last_mday (1, 1975), ==, 28);
    g_assert_cmpint (gnc_date_get_last_mday (1, 1980), ==, 29);
    g_assert_cmpint (gnc_date_get_last_mday (2, 1975), ==, 31);
    g_assert_cmpint (gnc_date_get_last_mday (2, 1980), ==, 31);
    g_assert_cmpint (gnc_date_get_last_mday (3, 1975), ==, 30);
    g_assert_cmpint (gnc_date_get_last_mday (3, 1980), ==, 30);
    g_assert_cmpint (gnc_date_get_last_mday (4, 1975), ==, 31);
    g_assert_cmpint (gnc_date_get_last_mday (4, 1980), ==, 31);
    g_assert_cmpint (gnc_date_get_last_mday (5, 1975), ==, 30);
    g_assert_cmpint (gnc_date_get_last_mday (5, 1980), ==, 30);
    g_assert_cmpint (gnc_date_get_last_mday (6, 1975), ==, 31);
    g_assert_cmpint (gnc_date_get_last_mday (6, 1980), ==, 31);
    g_assert_cmpint (gnc_date_get_last_mday (7, 1975), ==, 31);
    g_assert_cmpint (gnc_date_get_last_mday (7, 1980), ==, 31);
    g_assert_cmpint (gnc_date_get_last_mday (8, 1975), ==, 30);
    g_assert_cmpint (gnc_date_get_last_mday (8, 1980), ==, 30);
    g_assert_cmpint (gnc_date_get_last_mday (9, 1975), ==, 31);
    g_assert_cmpint (gnc_date_get_last_mday (9, 1980), ==, 31);
    g_assert_cmpint (gnc_date_get_last_mday (10, 1975), ==, 30);
    g_assert_cmpint (gnc_date_get_last_mday (10, 1980), ==, 30);
    g_assert_cmpint (gnc_date_get_last_mday (11, 1975), ==, 31);
    g_assert_cmpint (gnc_date_get_last_mday (11, 1980), ==, 31);
    g_assert_cmpint (gnc_date_get_last_mday (1, 2000), ==, 29);
    g_assert_cmpint (gnc_date_get_last_mday (1, 2400), ==, 28);
}
/* Getter, no testing needed.
QofDateFormat qof_date_format_get (void)// C: 5 in 3  Local: 0:0:0
*/
/* qof_date_format_set
set date format to one of US, UK, CE, ISO OR UTC
checks to make sure it's a legal value
param QofDateFormat: enumeration indicating preferred format
return void
Globals: dateFormat
void qof_date_format_set(QofDateFormat df)// C: 3 in 2  Local: 0:0:0
*/
static void
test_qof_date_format_set (void)
{
    gchar *msg = "[qof_date_format_set()] non-existent date format set attempted. Setting ISO default";
    gint loglevel = G_LOG_LEVEL_CRITICAL | G_LOG_FLAG_FATAL;
    gchar *logdomain = "qof.engine";
    TestErrorStruct check = {loglevel, logdomain, msg, 0};
    GLogFunc hdlr = g_log_set_default_handler ((GLogFunc)test_null_handler, &check);
    g_test_log_set_fatal_handler ((GTestLogFatalFunc)test_checked_handler, &check);
    qof_date_format_set ((QofDateFormat)((guint)DATE_FORMAT_LAST + 97));
    g_assert_cmpint (qof_date_format_get (), ==,  QOF_DATE_FORMAT_ISO);
    g_assert_cmpint (check.hits, ==,1);

    qof_date_format_set (QOF_DATE_FORMAT_UK);
    g_assert_cmpint (qof_date_format_get (), ==, QOF_DATE_FORMAT_UK);
    g_assert_cmpint (check.hits, ==,1);
    g_log_set_default_handler (hdlr, NULL);
}
/* qof_date_completion_set
set dateCompletion to one of QOF_DATE_COMPLETION_THISYEAR (for
completing the year to the current calendar year) or
QOF_DATE_COMPLETION_SLIDING (for using a sliding 12-month window). The
sliding window starts 'backmonth' months before the current month (0-11).
checks to make sure it's a legal value
param QofDateCompletion: indicates preferred completion method
param int: the number of months to go back in time (0-11)
return void
Globals: dateCompletion dateCompletionBackMonths
void qof_date_completion_set(QofDateCompletion dc, int backmonths)// C: 1  Local: 0:0:0
*/
/* static void
test_qof_date_completion_set (void)
{
}*/
/* qof_print_date_dmy_buff
size_t
qof_print_date_dmy_buff (char * buff, size_t len, int day, int month, int year)// C: 12 in 3  Local: 2:0:0
*/

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

static void
test_qof_print_date_dmy_buff (void)
{
    gchar buff[MAX_DATE_LENGTH + 1], t_buff[MAX_DATE_LENGTH + 1];
    gchar *locale = g_strdup (setlocale (LC_TIME, NULL));
    struct tm tm = { 0, 0, 0, 0, 0, 0, 0, 0, 0
#ifdef HAVE_STRUCT_TM_GMTOFF
        , 0, 0
#endif
    };

    qof_date_format_set (QOF_DATE_FORMAT_UK);
    memset ((gpointer)buff, 0, sizeof (buff));
    g_assert_cmpint (qof_print_date_dmy_buff (buff, MAX_DATE_LENGTH, 23, 11, 1974), ==, strlen (buff));
    g_assert_cmpstr (buff, ==, "23/11/1974");
    memset ((gpointer)buff, 0, sizeof (buff));
    g_assert_cmpint (qof_print_date_dmy_buff (buff, MAX_DATE_LENGTH, 2, 2, 1961), ==, strlen (buff));
    g_assert_cmpstr (buff, ==, "02/02/1961");
    memset ((gpointer)buff, 0, sizeof (buff));
    g_assert_cmpint (qof_print_date_dmy_buff (buff, MAX_DATE_LENGTH, 16, 6, 2045), ==, strlen (buff));
    g_assert_cmpstr (buff, ==, "16/06/2045");

    qof_date_format_set (QOF_DATE_FORMAT_CE);
    memset ((gpointer)buff, 0, sizeof (buff));
    g_assert_cmpint (qof_print_date_dmy_buff (buff, MAX_DATE_LENGTH, 23, 11, 1974), ==, strlen (buff));
    g_assert_cmpstr (buff, ==, "23.11.1974");
    memset ((gpointer)buff, 0, sizeof (buff));
    g_assert_cmpint (qof_print_date_dmy_buff (buff, MAX_DATE_LENGTH, 2, 2, 1961), ==, strlen (buff));
    g_assert_cmpstr (buff, ==, "02.02.1961");
    memset ((gpointer)buff, 0, sizeof (buff));
    g_assert_cmpint (qof_print_date_dmy_buff (buff, MAX_DATE_LENGTH, 16, 6, 2045), ==, strlen (buff));
    g_assert_cmpstr (buff, ==, "16.06.2045");

    qof_date_format_set (QOF_DATE_FORMAT_US);
    memset ((gpointer)buff, 0, sizeof (buff));
    g_assert_cmpint (qof_print_date_dmy_buff (buff, MAX_DATE_LENGTH, 23, 11, 1974), ==, strlen (buff));
    g_assert_cmpstr (buff, ==, "11/23/1974");
    memset ((gpointer)buff, 0, sizeof (buff));
    g_assert_cmpint (qof_print_date_dmy_buff (buff, MAX_DATE_LENGTH, 2, 2, 1961), ==, strlen (buff));
    g_assert_cmpstr (buff, ==, "02/02/1961");
    memset ((gpointer)buff, 0, sizeof (buff));
    g_assert_cmpint (qof_print_date_dmy_buff (buff, MAX_DATE_LENGTH, 16, 6, 2045), ==, strlen (buff));
    g_assert_cmpstr (buff, ==, "06/16/2045");

    qof_date_format_set (QOF_DATE_FORMAT_ISO);
    memset ((gpointer)buff, 0, sizeof (buff));
    g_assert_cmpint (qof_print_date_dmy_buff (buff, MAX_DATE_LENGTH, 23, 11, 1974), ==, strlen (buff));
    g_assert_cmpstr (buff, ==, "1974-11-23");
    memset ((gpointer)buff, 0, sizeof (buff));
    g_assert_cmpint (qof_print_date_dmy_buff (buff, MAX_DATE_LENGTH, 2, 2, 1961),
                     ==, strlen (buff));
    g_assert_cmpstr (buff, ==, "1961-02-02");
    memset ((gpointer)buff, 0, sizeof (buff));
    g_assert_cmpint (qof_print_date_dmy_buff (buff, MAX_DATE_LENGTH, 16, 6, 2045),
                     ==, strlen (buff));
    g_assert_cmpstr (buff, ==, "2045-06-16");

    qof_date_format_set (QOF_DATE_FORMAT_LOCALE);
    test_gnc_setlocale (LC_TIME, "en_US");
    tm_set_dmy (&tm, 1974, 11, 23);
    strftime(t_buff, MAX_DATE_LENGTH, GNC_D_FMT, &tm);
    memset ((gpointer)buff, 0, sizeof (buff));
    g_assert_cmpint (qof_print_date_dmy_buff (buff, MAX_DATE_LENGTH, tm.tm_mday,
                     tm.tm_mon + 1, tm.tm_year + 1900),
                     ==, strlen (buff));
    g_assert_cmpstr (buff, ==, t_buff);


    tm_set_dmy (&tm, 1961, 2, 2);
    strftime(t_buff, MAX_DATE_LENGTH, GNC_D_FMT, &tm);
    memset ((gpointer)buff, 0, sizeof (buff));
    g_assert_cmpint (qof_print_date_dmy_buff (buff, MAX_DATE_LENGTH, tm.tm_mday,
                     tm.tm_mon + 1, tm.tm_year + 1900),
                     ==, strlen (buff));
    g_assert_cmpstr (buff, ==, t_buff);
    memset ((gpointer)buff, 0, sizeof (buff));
    tm_set_dmy (&tm, 2045, 6, 16);
    strftime(t_buff, MAX_DATE_LENGTH, GNC_D_FMT, &tm);
    memset ((gpointer)buff, 0, sizeof (buff));
    g_assert_cmpint (qof_print_date_dmy_buff (buff, MAX_DATE_LENGTH, tm.tm_mday,
                     tm.tm_mon + 1, tm.tm_year + 1900),
                     ==, strlen (buff));
    g_assert_cmpstr (buff, ==, t_buff);

    test_gnc_setlocale (LC_TIME, "en_GB");
    tm_set_dmy (&tm, 1974, 11, 23);
    strftime(t_buff, MAX_DATE_LENGTH, GNC_D_FMT, &tm);
    memset ((gpointer)buff, 0, sizeof (buff));
    g_assert_cmpint (qof_print_date_dmy_buff (buff, MAX_DATE_LENGTH, tm.tm_mday,
                     tm.tm_mon + 1, tm.tm_year + 1900),
                     ==, strlen (buff));
    g_assert_cmpstr (buff, ==, t_buff);
    tm_set_dmy (&tm, 1961, 2, 2);
    strftime(t_buff, MAX_DATE_LENGTH, GNC_D_FMT, &tm);
    memset ((gpointer)buff, 0, sizeof (buff));
    g_assert_cmpint (qof_print_date_dmy_buff (buff, MAX_DATE_LENGTH, tm.tm_mday,
                     tm.tm_mon + 1, tm.tm_year + 1900),
                     ==, strlen (buff));
    g_assert_cmpstr (buff, ==, t_buff);
    memset ((gpointer)buff, 0, sizeof (buff));
    tm_set_dmy (&tm, 2045, 6, 16);
    strftime(t_buff, MAX_DATE_LENGTH, GNC_D_FMT, &tm);
    memset ((gpointer)buff, 0, MAX_DATE_LENGTH);
    g_assert_cmpint (qof_print_date_dmy_buff (buff, MAX_DATE_LENGTH, tm.tm_mday,
                     tm.tm_mon + 1, tm.tm_year + 1900),
                     ==, strlen (buff));
    g_assert_cmpstr (buff, ==, t_buff);

    test_gnc_setlocale (LC_TIME, "fr_FR");
    tm_set_dmy (&tm, 1974, 11, 23);
    strftime(t_buff, MAX_DATE_LENGTH, GNC_D_FMT, &tm);
    memset ((gpointer)buff, 0, sizeof (buff));
    g_assert_cmpint (qof_print_date_dmy_buff (buff, MAX_DATE_LENGTH, tm.tm_mday,
                     tm.tm_mon + 1, tm.tm_year + 1900),
                     ==, strlen (buff));
    g_assert_cmpstr (buff, ==, t_buff);
    tm_set_dmy (&tm, 1961, 2, 2);
    strftime(t_buff, MAX_DATE_LENGTH, GNC_D_FMT, &tm);
    memset ((gpointer)buff, 0, sizeof (buff));
    g_assert_cmpint (qof_print_date_dmy_buff (buff, MAX_DATE_LENGTH, tm.tm_mday,
                     tm.tm_mon + 1, tm.tm_year + 1900),
                     ==, strlen (buff));
    g_assert_cmpstr (buff, ==, t_buff);
    memset ((gpointer)buff, 0, sizeof (buff));
    tm_set_dmy (&tm, 2045, 6, 16);
    strftime(t_buff, MAX_DATE_LENGTH, GNC_D_FMT, &tm);
    memset ((gpointer)buff, 0, sizeof (buff));
    g_assert_cmpint (qof_print_date_dmy_buff (buff, MAX_DATE_LENGTH, tm.tm_mday,
                     tm.tm_mon + 1, tm.tm_year + 1900),
                     ==, strlen (buff));
    g_assert_cmpstr (buff, ==, t_buff);

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
        g_assert_cmpstr (datestr, ==, t_buff);                          \
    }


/* qof_print_date_buff
size_t
qof_print_date_buff (char * buff, size_t len, time64 t)// C: 3 in 1  Local: 2:0:0
*/
static void
test_qof_print_date_buff (void)
{
    gchar buff[MAX_DATE_LENGTH + 1], ans[MAX_DATE_LENGTH + 1];
    gchar *locale = g_strdup (setlocale (LC_TIME, NULL));

    struct tm tm1 = {0, 0, 12, 23, 10, 74};
    struct tm tm2 = {0, 0, 12, 2, 1, 61};
    struct tm tm3 = {0, 0, 12, 16, 5, 145};
    time64 time1 = gnc_mktime(&tm1);
    time64 time2 = gnc_mktime(&tm2);
    time64 time3 = gnc_mktime(&tm3);

    qof_date_format_set (QOF_DATE_FORMAT_UK);
    memset ((gpointer)buff, 0, sizeof (buff));
    g_assert_cmpint (qof_print_date_buff (buff, MAX_DATE_LENGTH, time1),
                     ==, strlen (buff));
    g_assert_cmpstr (buff, ==, "23/11/1974");
    memset ((gpointer)buff, 0, sizeof (buff));
    g_assert_cmpint (qof_print_date_buff (buff, MAX_DATE_LENGTH, time2),
                     ==, strlen (buff));
    g_assert_cmpstr (buff, ==, "02/02/1961");

    memset ((gpointer)buff, 0, sizeof (buff));
    g_assert_cmpint (qof_print_date_buff (buff, MAX_DATE_LENGTH, time3),
                     ==, strlen (buff));
    g_assert_cmpstr (buff, ==, "16/06/2045");

    qof_date_format_set (QOF_DATE_FORMAT_CE);
    memset ((gpointer)buff, 0, sizeof (buff));
    g_assert_cmpint (qof_print_date_buff (buff, MAX_DATE_LENGTH, time1),
                     ==, strlen (buff));
    g_assert_cmpstr (buff, ==, "23.11.1974");
    memset ((gpointer)buff, 0, sizeof (buff));
    g_assert_cmpint (qof_print_date_buff (buff, MAX_DATE_LENGTH, time2),
                     ==, strlen (buff));
    g_assert_cmpstr (buff, ==, "02.02.1961");

    memset ((gpointer)buff, 0, sizeof (buff));
    g_assert_cmpint (qof_print_date_buff (buff, MAX_DATE_LENGTH, time3),
                     ==, strlen (buff));
    g_assert_cmpstr (buff, ==, "16.06.2045");

    qof_date_format_set (QOF_DATE_FORMAT_US);
    memset ((gpointer)buff, 0, sizeof (buff));
    g_assert_cmpint (qof_print_date_buff (buff, MAX_DATE_LENGTH, time1),
                     ==, strlen (buff));
    g_assert_cmpstr (buff, ==, "11/23/1974");
    memset ((gpointer)buff, 0, sizeof (buff));
    g_assert_cmpint (qof_print_date_buff (buff, MAX_DATE_LENGTH, time2),
                     ==, strlen (buff));
    g_assert_cmpstr (buff, ==, "02/02/1961");

    memset ((gpointer)buff, 0, sizeof (buff));
    g_assert_cmpint (qof_print_date_buff (buff, MAX_DATE_LENGTH, time3),
                     ==, strlen (buff));
    g_assert_cmpstr (buff, ==, "06/16/2045");

    qof_date_format_set (QOF_DATE_FORMAT_ISO);
    memset ((gpointer)buff, 0, sizeof (buff));
    g_assert_cmpint (qof_print_date_buff (buff, MAX_DATE_LENGTH, time1),
                     ==, strlen (buff));
    g_assert_cmpstr (buff, ==, "1974-11-23");
    memset ((gpointer)buff, 0, sizeof (buff));
    g_assert_cmpint (qof_print_date_buff (buff, MAX_DATE_LENGTH, time2),
                     ==, strlen (buff));
    g_assert_cmpstr (buff, ==, "1961-02-02");

    memset ((gpointer)buff, 0, sizeof (buff));
    g_assert_cmpint (qof_print_date_buff (buff, MAX_DATE_LENGTH, time3),
                     ==, strlen (buff));
    g_assert_cmpstr (buff, ==, "2045-06-16");

    qof_date_format_set (QOF_DATE_FORMAT_LOCALE);
    test_gnc_setlocale (LC_TIME, "en_US");
    memset ((gpointer)buff, 0, sizeof (buff));
    g_assert_cmpint (qof_print_date_buff (buff, MAX_DATE_LENGTH, time1),
                     ==, strlen (buff));
    g_assert_cmpstr (buff, ==, "11/23/1974");
    memset ((gpointer)buff, 0, sizeof (buff));
    g_assert_cmpint (qof_print_date_buff (buff, MAX_DATE_LENGTH, time2),
                     ==, strlen (buff));
    g_assert_cmpstr (buff, ==, "02/02/1961");

    memset ((gpointer)buff, 0, sizeof (buff));
    g_assert_cmpint (qof_print_date_buff (buff, MAX_DATE_LENGTH, time3),
                     ==, strlen (buff));
    g_assert_cmpstr (buff, ==, "06/16/2045");

    test_gnc_setlocale (LC_TIME, "en_GB");
    memset ((gpointer)buff, 0, sizeof (buff));
    g_assert_cmpint (qof_print_date_buff (buff, MAX_DATE_LENGTH, time1),
                     ==, strlen (buff));
    strftime(ans, MAX_DATE_LENGTH, GNC_D_FMT, &tm1);
    g_assert_cmpstr (buff, ==, ans);
    memset ((gpointer)buff, 0, sizeof (buff));
    g_assert_cmpint (qof_print_date_buff (buff, MAX_DATE_LENGTH, time2),
                     ==, strlen (buff));
    strftime(ans, MAX_DATE_LENGTH, GNC_D_FMT, &tm2);
    g_assert_cmpstr (buff, ==, ans);
    memset ((gpointer)buff, 0, sizeof (buff));
    g_assert_cmpint (qof_print_date_buff (buff, MAX_DATE_LENGTH, time3),
                     ==, strlen (buff));
    strftime(ans, MAX_DATE_LENGTH, GNC_D_FMT, &tm3);
    g_assert_cmpstr (buff, ==, ans);

    test_gnc_setlocale (LC_TIME, "fr_FR");
    memset ((gpointer)buff, 0, sizeof (buff));
    g_assert_cmpint (qof_print_date_buff (buff, MAX_DATE_LENGTH, time1),
                     ==, strlen (buff));
    strftime(ans, MAX_DATE_LENGTH, GNC_D_FMT, &tm1);
    g_assert_cmpstr (buff, ==, ans);
    memset ((gpointer)buff, 0, sizeof (buff));
    g_assert_cmpint (qof_print_date_buff (buff, MAX_DATE_LENGTH, time2),
                     ==, strlen (buff));
    strftime(ans, MAX_DATE_LENGTH, GNC_D_FMT, &tm2);
    g_assert_cmpstr (buff, ==, ans);
    memset ((gpointer)buff, 0, sizeof (buff));
    g_assert_cmpint (qof_print_date_buff (buff, MAX_DATE_LENGTH, time3),
                     ==, strlen (buff));
    strftime(ans, MAX_DATE_LENGTH, GNC_D_FMT, &tm3);
    g_assert_cmpstr (buff, ==, ans);

    setlocale (LC_TIME, locale);
    g_free (locale);
}
/* qof_print_gdate
size_t
qof_print_gdate( char *buf, size_t len, const GDate *gd )// C: 6 in 5  Local: 0:0:0
*/
static void
test_qof_print_gdate (void)
{
    gchar buff[MAX_DATE_LENGTH + 1], t_buff[MAX_DATE_LENGTH + 1];
    gchar *locale = g_strdup (setlocale (LC_TIME, NULL));
    GDate *gd1 = g_date_new_dmy (23, 11, 1974);
    GDate *gd2 = g_date_new_dmy (2, 2, 1961);
    GDate *gd3 = g_date_new_dmy (16, 6, 2045);

    qof_date_format_set (QOF_DATE_FORMAT_UK);
    memset ((gpointer)buff, 0, sizeof (buff));
    g_assert_cmpint (qof_print_gdate (buff, MAX_DATE_LENGTH, gd1),
                     ==, strlen (buff));
    g_assert_cmpstr (buff, ==, "23/11/1974");
    memset ((gpointer)buff, 0, sizeof (buff));
    g_assert_cmpint (qof_print_gdate (buff, MAX_DATE_LENGTH, gd2),
                     ==, strlen (buff));
    g_assert_cmpstr (buff, ==, "02/02/1961");
    memset ((gpointer)buff, 0, sizeof (buff));
    g_assert_cmpint (qof_print_gdate (buff, MAX_DATE_LENGTH, gd3),
                     ==, strlen (buff));
    g_assert_cmpstr (buff, ==, "16/06/2045");

    qof_date_format_set (QOF_DATE_FORMAT_CE);
    memset ((gpointer)buff, 0, sizeof (buff));
    g_assert_cmpint (qof_print_gdate (buff, MAX_DATE_LENGTH, gd1),
                     ==, strlen (buff));
    g_assert_cmpstr (buff, ==, "23.11.1974");
    memset ((gpointer)buff, 0, sizeof (buff));
    g_assert_cmpint (qof_print_gdate (buff, MAX_DATE_LENGTH, gd2),
                     ==, strlen (buff));
    g_assert_cmpstr (buff, ==, "02.02.1961");
    memset ((gpointer)buff, 0, sizeof (buff));
    g_assert_cmpint (qof_print_gdate (buff, MAX_DATE_LENGTH, gd3),
                     ==, strlen (buff));
    g_assert_cmpstr (buff, ==, "16.06.2045");


    qof_date_format_set (QOF_DATE_FORMAT_US);
    memset ((gpointer)buff, 0, sizeof (buff));
    g_assert_cmpint (qof_print_gdate (buff, MAX_DATE_LENGTH, gd1),
                     ==, strlen (buff));
    g_assert_cmpstr (buff, ==, "11/23/1974");
    memset ((gpointer)buff, 0, sizeof (buff));
    g_assert_cmpint (qof_print_gdate (buff, MAX_DATE_LENGTH, gd2),
                     ==, strlen (buff));
    g_assert_cmpstr (buff, ==, "02/02/1961");
    memset ((gpointer)buff, 0, sizeof (buff));
    g_assert_cmpint (qof_print_gdate (buff, MAX_DATE_LENGTH, gd3),
                     ==, strlen (buff));
    g_assert_cmpstr (buff, ==, "06/16/2045");


    qof_date_format_set (QOF_DATE_FORMAT_ISO);
    memset ((gpointer)buff, 0, sizeof (buff));
    g_assert_cmpint (qof_print_gdate (buff, MAX_DATE_LENGTH, gd1),
                     ==, strlen (buff));
    g_assert_cmpstr (buff, ==, "1974-11-23");
    memset ((gpointer)buff, 0, sizeof (buff));
    g_assert_cmpint (qof_print_gdate (buff, MAX_DATE_LENGTH, gd2),
                     ==, strlen (buff));
    g_assert_cmpstr (buff, ==, "1961-02-02");
    memset ((gpointer)buff, 0, sizeof (buff));
    g_assert_cmpint (qof_print_gdate (buff, MAX_DATE_LENGTH, gd3),
                     ==, strlen (buff));
    g_assert_cmpstr (buff, ==, "2045-06-16");


    qof_date_format_set (QOF_DATE_FORMAT_LOCALE);
    test_gnc_setlocale (LC_TIME, "en_US");
    memset ((gpointer)buff, 0, sizeof (buff));
    g_assert_cmpint (qof_print_gdate (buff, MAX_DATE_LENGTH, gd1),
                     ==, strlen (buff));
    g_date_strftime (t_buff, MAX_DATE_LENGTH, GNC_D_FMT, gd1);
    g_assert_cmpstr (buff, ==, t_buff);
    memset ((gpointer)buff, 0, sizeof (buff));
    g_assert_cmpint (qof_print_gdate (buff, MAX_DATE_LENGTH, gd2),
                     ==, strlen (buff));
    g_date_strftime (t_buff, MAX_DATE_LENGTH, GNC_D_FMT, gd2);
    g_assert_cmpstr (buff, ==, t_buff);
    memset ((gpointer)buff, 0, sizeof (buff));
    g_assert_cmpint (qof_print_gdate (buff, MAX_DATE_LENGTH, gd3),
                     ==, strlen (buff));
    g_date_strftime (t_buff, MAX_DATE_LENGTH, GNC_D_FMT, gd3);
    g_assert_cmpstr (buff, ==, t_buff);

    test_gnc_setlocale (LC_TIME, "en_GB");
    memset ((gpointer)buff, 0, sizeof (buff));
    g_assert_cmpint (qof_print_gdate (buff, MAX_DATE_LENGTH, gd1),
                     ==, strlen (buff));
    g_date_strftime (t_buff, MAX_DATE_LENGTH, GNC_D_FMT, gd1);
    g_assert_cmpstr (buff, ==, t_buff);
    memset ((gpointer)buff, 0, sizeof (buff));
    g_assert_cmpint (qof_print_gdate (buff, MAX_DATE_LENGTH, gd2),
                     ==, strlen (buff));
    g_date_strftime (t_buff, MAX_DATE_LENGTH, GNC_D_FMT, gd2);
    g_assert_cmpstr (buff, ==, t_buff);
    memset ((gpointer)buff, 0, sizeof (buff));
    g_assert_cmpint (qof_print_gdate (buff, MAX_DATE_LENGTH, gd3),
                     ==, strlen (buff));
    g_date_strftime (t_buff, MAX_DATE_LENGTH, GNC_D_FMT, gd3);
    g_assert_cmpstr (buff, ==, t_buff);


    test_gnc_setlocale (LC_TIME, "fr_FR");
    memset ((gpointer)buff, 0, sizeof (buff));
    g_assert_cmpint (qof_print_gdate (buff, MAX_DATE_LENGTH, gd1),
                     ==, strlen (buff));
    g_date_strftime (t_buff, MAX_DATE_LENGTH, GNC_D_FMT, gd1);
    g_assert_cmpstr (buff, ==, t_buff);
    memset ((gpointer)buff, 0, sizeof (buff));
    g_assert_cmpint (qof_print_gdate (buff, MAX_DATE_LENGTH, gd2),
                     ==, strlen (buff));
    g_date_strftime (t_buff, MAX_DATE_LENGTH, GNC_D_FMT, gd2);
    g_assert_cmpstr (buff, ==, t_buff);
    memset ((gpointer)buff, 0, sizeof (buff));
    g_assert_cmpint (qof_print_gdate (buff, MAX_DATE_LENGTH, gd3),
                     ==, strlen (buff));
    g_date_strftime (t_buff, MAX_DATE_LENGTH, GNC_D_FMT, gd3);
    g_assert_cmpstr (buff, ==, t_buff);

    setlocale (LC_TIME, locale);
    g_free (locale);
    g_date_free (gd1);
    g_date_free (gd2);
    g_date_free (gd3);
}

#define test_assert_qof_print_date(time, datestr)  \
    {                                              \
        gchar *buf = qof_print_date (time);        \
        g_assert_cmpstr (buf, ==, datestr);        \
        g_free (buf);                              \
    }

#define test_assert_qof_print_date_outside_range(time, datestr)  \
    {                                              \
        gchar *buf = qof_print_date (time);        \
        g_assert_cmpstr (buf, ==, datestr);        \
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
static void
test_qof_print_date (void)
{
    gchar *locale = g_strdup (setlocale (LC_TIME, NULL));
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
/* floordiv
static int
floordiv(int a, int b)// Local: 1:0:0
*/
/* static void
test_floordiv (void)
{
}*/
/* qof_scan_date_internal
qof_scan_date just does passes this through, passing the pre-set QOF_DATE_FORMAT, so we test there rather than exposing this via Testfuncs.
qof_scan_date_internal (const char *buff, int *day, int *month, int *year,// Local: 3:0:0
*/
/* static void
test_qof_scan_date_internal (void)
{
} */
/* qof_scan_date
gboolean
qof_scan_date (const char *buff, int *day, int *month, int *year)// C: 7 in 3  Local: 0:0:0
*/
static void
test_qof_scan_date (void)
{
    gchar *locale = g_strdup (setlocale (LC_TIME, NULL));
    int day = 0, mo = 0, yr = 0;
    gint year, month;
    time64 now = gnc_time(NULL);
    gchar buff[MAX_DATE_LENGTH];
    struct tm tm = { 0, 0, 0, 0, 0, 0, 0, 0, 0
#ifndef G_OS_WIN32
        , 0, 0
#endif
    };
    gnc_localtime_r(&now, &tm);
    year = tm.tm_year + 1900;
    month = tm.tm_mon + 1;

    g_assert (!qof_scan_date (NULL, &day, &mo, &yr));
    g_assert_cmpint (day, ==, 0);
    g_assert_cmpint (mo, ==, 0);
    g_assert_cmpint (yr, ==, 0);

    qof_date_format_set (QOF_DATE_FORMAT_UTC);
    g_assert (qof_scan_date ("1974-11-23", &day, &mo, &yr));
    g_assert_cmpint (day, ==, 23);
    g_assert_cmpint (mo, ==, 11);
    g_assert_cmpint (yr, ==, 1974);

    g_assert (qof_scan_date ("1961-2-2", &day, &mo, &yr));
    g_assert_cmpint (day, ==, 2);
    g_assert_cmpint (mo, ==, 2);
    g_assert_cmpint (yr, ==, 1961);

    g_assert (qof_scan_date ("2045-6-16", &day, &mo, &yr));
    g_assert_cmpint (day, ==, 16);
    g_assert_cmpint (mo, ==, 6);
    g_assert_cmpint (yr, ==, 2045);

    qof_date_format_set (QOF_DATE_FORMAT_US);
    g_assert (qof_scan_date ("11/23/1974", &day, &mo, &yr));
    g_assert_cmpint (day, ==, 23);
    g_assert_cmpint (mo, ==, 11);
    g_assert_cmpint (yr, ==, 1974);

    g_assert (qof_scan_date ("2/2/1961", &day, &mo, &yr));
    g_assert_cmpint (day, ==, 2);
    g_assert_cmpint (mo, ==, 2);
    g_assert_cmpint (yr, ==, 1961);

    g_assert (qof_scan_date ("6/16/2045", &day, &mo, &yr));
    g_assert_cmpint (day, ==, 16);
    g_assert_cmpint (mo, ==, 6);
    g_assert_cmpint (yr, ==, 2045);

    g_assert (qof_scan_date ("11월23년1974", &day, &mo, &yr));
    g_assert_cmpint (day, ==, 23);
    g_assert_cmpint (mo, ==, 11);
    g_assert_cmpint (yr, ==, 1974);

    g_assert (qof_scan_date ("11月23年1974", &day, &mo, &yr));
    g_assert_cmpint (day, ==, 23);
    g_assert_cmpint (mo, ==, 11);
    g_assert_cmpint (yr, ==, 1974);

    qof_date_completion_set (QOF_DATE_COMPLETION_THISYEAR, 0);

    g_assert (qof_scan_date ("11-23", &day, &mo, &yr));
    g_assert_cmpint (day, ==, 23);
    g_assert_cmpint (mo, ==, 11);
    g_assert_cmpint (yr, ==, year);

    g_assert (qof_scan_date ("23-11", &day, &mo, &yr));
    g_assert_cmpint (day, ==, 23);
    g_assert_cmpint (mo, ==, 11);
    g_assert_cmpint (yr, ==, year);

    if (month < 10) /* Sliding window won't test well after October */
    {
        qof_date_completion_set (QOF_DATE_COMPLETION_SLIDING, month + 1);

        g_assert (qof_scan_date ("12-23", &day, &mo, &yr));
        g_assert_cmpint (day, ==, 23);
        g_assert_cmpint (mo, ==, 12);
        g_assert_cmpint (yr, ==, year - 1);

        qof_date_completion_set (QOF_DATE_COMPLETION_THISYEAR, 0);
    }

    qof_date_format_set (QOF_DATE_FORMAT_UK);
    g_assert (qof_scan_date ("23/11/1974", &day, &mo, &yr));
    g_assert_cmpint (day, ==, 23);
    g_assert_cmpint (mo, ==, 11);
    g_assert_cmpint (yr, ==, 1974);

    g_assert (qof_scan_date ("2/2/1961", &day, &mo, &yr));
    g_assert_cmpint (day, ==, 2);
    g_assert_cmpint (mo, ==, 2);
    g_assert_cmpint (yr, ==, 1961);

    g_assert (qof_scan_date ("16/6/2045", &day, &mo, &yr));
    g_assert_cmpint (day, ==, 16);
    g_assert_cmpint (mo, ==, 6);
    g_assert_cmpint (yr, ==, 2045);

    qof_date_format_set (QOF_DATE_FORMAT_LOCALE);
    test_gnc_setlocale (LC_TIME, "en_GB");
    tm_set_dmy (&tm, 1974, 11, 23);
    strftime (buff, MAX_DATE_LENGTH, GNC_D_FMT, &tm);
    g_assert (qof_scan_date (buff, &day, &mo, &yr));
    g_assert_cmpint (day, ==, tm.tm_mday);
    g_assert_cmpint (mo, ==, tm.tm_mon + 1);
    g_assert_cmpint (yr, ==, tm.tm_year + 1900);

    tm_set_dmy (&tm, 1961,2, 2);
    strftime (buff, MAX_DATE_LENGTH, GNC_D_FMT, &tm);
    g_assert (qof_scan_date (buff, &day, &mo, &yr));
    g_assert_cmpint (day, ==, tm.tm_mday);
    g_assert_cmpint (mo, ==, tm.tm_mon + 1);
    /* Some locale date formats result in a 2-digit year, which strptime
     * interprets as being in the current century.
     */
    g_assert_cmpint (yr % 100, ==, tm.tm_year % 100);

    tm_set_dmy (&tm, 2045, 6, 16);
    strftime (buff, MAX_DATE_LENGTH, GNC_D_FMT, &tm);
    g_assert (qof_scan_date (buff, &day, &mo, &yr));
    g_assert_cmpint (day, ==, tm.tm_mday);
    g_assert_cmpint (mo, ==, tm.tm_mon + 1);
    g_assert_cmpint (yr, ==, tm.tm_year + 1900);

    setlocale (LC_TIME, locale);
    g_free (locale);
}
/* dateSeparator
return date character
char dateSeparator (void)// C: 1  Local: 0:0:0
src/register/register-gnome/datecell-gnome.h
*/
/* static void
test_dateSeparator (void)
{
}*/
/* qof_time_format_from_utf8
gchar *
qof_time_format_from_utf8(const gchar *utf8_format)// C: 1  Local: 1:0:0
*/
/* static void
test_qof_time_format_from_utf8 (void)
{
}*/
/* qof_formatted_time_to_utf8
gchar *
qof_formatted_time_to_utf8(const gchar *locale_string)// C: 1  Local: 1:0:0
*/
/* static void
test_qof_formatted_time_to_utf8 (void)
{
}*/
/* qof_format_time
static gchar *
qof_format_time(const gchar *format, const struct tm *tm)// Local: 1:0:0
*/
/* static void
test_qof_format_time (void)
{
}*/
/* qof_strftime
gsize
qof_strftime(gchar *buf, gsize max, const gchar *format, const struct tm *tm)// C: 16 in 9  Local: 5:0:0
*/
/* static void
test_qof_strftime (void)
{
}*/
/* gnc_date_timestamp
gchar *
gnc_date_timestamp (void)// C: 2 in 2  Local: 0:0:0
*/
static void
test_gnc_date_timestamp (void)
{
    time64 now = gnc_time(NULL);
    gchar *timestr = gnc_date_timestamp ();
    struct tm tm0, tm1;
    gnc_localtime_r(&now, &tm0);
    g_assert (strptime (timestr, "%Y%m%d%H%M%S", &tm1));
    g_assert_cmpint (tm0.tm_year, ==, tm1.tm_year);
    g_assert_cmpint (tm0.tm_mon, ==, tm1.tm_mon);
    g_assert_cmpint (tm0.tm_mday, ==, tm1.tm_mday);
    g_assert_cmpint (tm0.tm_hour, ==, tm1.tm_hour);
    g_assert_cmpint (tm0.tm_min, ==, tm1.tm_min);
    g_assert_cmpint (tm0.tm_sec, ==, tm1.tm_sec);

    g_free (timestr);
}
/* gnc_iso8601_to_time64_gmt
time64
gnc_iso8601_to_time64_gmt(const char *str)// C: 6 in 3  Local: 0:0:0
*/
static gint
get_nanoseconds (GDateTime *gdt)
{
    return g_date_time_get_microsecond (gdt) * 1000;
}

static void
test_gnc_iso8601_to_time64_gmt (FixtureA *f, gconstpointer pData)
{
    time64 t = gnc_iso8601_to_time64_gmt (NULL);
    g_assert_cmpint (t, ==, INT64_MAX);

    t = gnc_iso8601_to_time64_gmt ("");
    g_assert_cmpint (t, ==, 0);

    t = gnc_iso8601_to_time64_gmt ("1989-03-27 13:43:27");
    g_assert_cmpint (t, ==, f->t1);

    t = gnc_iso8601_to_time64_gmt ("2020-11-07 06:21:19 -05");
    g_assert_cmpint (t, ==, f->t2);

    t = gnc_iso8601_to_time64_gmt ("2012-07-04 19:27:44.0+08:40");
    g_assert_cmpint (t, ==, f->t3);

    t = gnc_iso8601_to_time64_gmt ("1961-09-22 17:53:19 -05");
    g_assert_cmpint (t, ==, f->t4);

    t = gnc_iso8601_to_time64_gmt ("2061-01-25 23:21:19.0 -05:00");
    g_assert_cmpint (t, ==, f->t5);
}
/* gnc_time64_to_iso8601_buff
char *
gnc_time64_to_iso8601_buff (time64 t, char * buff)// C: 18 in 7  Local: 0:0:0
*/
static time64
g_date_time_to_time64 (GDateTime *gdt)
{
    time64 t;
    t = g_date_time_to_unix (gdt);
    return t;
}

#define ISO8601_SIZE MAX_DATE_LENGTH + 4
static gchar*
format_timestring (time64 t, TZOffset tz)
{
    static const unsigned tzlen = MAX_DATE_LENGTH - 26;
    char *fmt = "%Y-%m-%d %H:%M:%S";
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

static void
test_gnc_time64_to_iso8601_buff (FixtureA *f, gconstpointer pData)
{

    gchar buff[ISO8601_SIZE];
    gchar *time_str;
    time64 t = 0;
    gchar *end;

    memset (buff, 0, sizeof buff);

    end = gnc_time64_to_iso8601_buff (t, NULL);
    g_assert (end == NULL);

    end = gnc_time64_to_iso8601_buff (f->t0, buff);
    g_assert_cmpint (end - buff, ==, strlen (buff));
    time_str = format_timestring (f->t0, f->off_zulu);
    g_assert_cmpstr (buff, ==, time_str);
    g_free (time_str);

    end = gnc_time64_to_iso8601_buff (f->t1, buff);
    time_str = format_timestring (f->t1, f->off_zulu);
    g_assert_cmpstr (buff, ==, time_str);
    g_free (time_str);


    end = gnc_time64_to_iso8601_buff (f->t2, buff);
    time_str = format_timestring (f->t2, f->off_05w);
    g_assert_cmpstr (buff, ==, time_str);
    g_free (time_str);

    end = gnc_time64_to_iso8601_buff (f->t3, buff);
    time_str = format_timestring (f->t3, f->off_0840e);
    g_assert_cmpstr (buff, ==, time_str);
    g_free (time_str);

    end = gnc_time64_to_iso8601_buff (f->t4, buff);
    time_str = format_timestring (f->t4, f->off_05w);
    g_assert_cmpstr (buff, ==, time_str);
    g_free (time_str);

    end = gnc_time64_to_iso8601_buff (f->t5, buff);
    time_str = format_timestring (f->t5, f->off_05w);
    g_assert_cmpstr (buff, ==, time_str);
    g_free (time_str);
}

/* gnc_dmy2time64_internal
static time64
gnc_dmy2time64_internal (int day, int month, int year, gboolean start_of_day)// Local: 2:0:0
*/
/* static void
test_gnc_dmy2time64_internal (void)
{
}*/
/* gnc_dmy2time64
time64
gnc_dmy2time64 (int day, int month, int year)// C: 8 in 5  Local: 1:0:0
*/
static void
test_gnc_dmy2time64 (FixtureB *f, gconstpointer pData)
{
    gchar *msg1 = "[qof_dmy2time64()] Date computation error from Y-M-D 1257-7-2: Time value is outside the supported year range.";
    gint loglevel = G_LOG_LEVEL_WARNING | G_LOG_FLAG_FATAL;
    gchar *logdomain = "qof.engine";
    TestErrorStruct check = {loglevel, logdomain, msg1, 0};
    GLogFunc hdlr = g_log_set_default_handler ((GLogFunc)test_null_handler, &check);
    g_test_log_set_fatal_handler ((GTestLogFatalFunc)test_checked_handler, &check);
    for (int i = 0; i < sizeof(f->test)/sizeof(TimeMap); ++i)
    {
#ifdef HAVE_STRUCT_TM_GMTOFF
        struct tm tm = {0, 0, 0, f->test[i].day, f->test[i].mon - 1,
                        f->test[i].yr - 1900, 0, 0, -1, 0, NULL};
#else
        struct tm tm = {0, 0, 0, f->test[i].day, f->test[i].mon - 1,
                        f->test[i].yr - 1900, 0, 0, -1};
#endif
        time64 r_t = gnc_dmy2time64 (f->test[i].day, f->test[i].mon,
                                         f->test[i].yr);
        struct tm time1 = tm, time2 = tm;
        int offset = gnc_mktime(&time1) - gnc_timegm(&time2);
        if (f->test[i].secs == INT64_MAX)
            /* We use INT64_MAX as invalid time64.secs.
             * As we can't *add* to the max, we can ignore the tz offset in this case. */
            g_assert_cmpint (r_t, ==, INT64_MAX);
        else
            g_assert_cmpint (r_t, ==, f->test[i].secs - offset);
    }
    g_log_set_default_handler (hdlr, 0);
}
/* gnc_dmy2time64_end
time64
gnc_dmy2time64_end (int day, int month, int year)// C: 1  Local: 0:0:0
*/
static void
test_gnc_dmy2time64_end (FixtureB *f, gconstpointer pData)
{
    gchar *msg1 = "[qof_dmy2time64_end()] Date computation error from Y-M-D 1257-7-2: Time value is outside the supported year range.";
    gint loglevel = G_LOG_LEVEL_CRITICAL | G_LOG_FLAG_FATAL;
    gchar *logdomain = "qof.engine";
    TestErrorStruct check = {loglevel, logdomain, msg1, 0};
    GLogFunc hdlr = g_log_set_default_handler ((GLogFunc)test_null_handler, &check);
    g_test_log_set_fatal_handler ((GTestLogFatalFunc)test_checked_handler, &check);
    for (int i = 0; i < sizeof(f->test)/sizeof(TimeMap); ++i)
    {
#ifdef HAVE_STRUCT_TM_GMTOFF
        struct tm tm = {59, 59, 23, f->test[i].day, f->test[i].mon - 1,
                        f->test[i].yr - 1900, 0, 0, -1, 0, NULL};
#else
        struct tm tm = {59, 59, 23, f->test[i].day, f->test[i].mon - 1,
                        f->test[i].yr - 1900, 0, 0, -1};
#endif
        time64 r_t = gnc_dmy2time64_end (f->test[i].day, f->test[i].mon,
                                             f->test[i].yr);
        int offset = gnc_mktime(&tm) - gnc_timegm(&tm);
        if (f->test[i].secs == INT64_MAX)
            /* We use INT64_MAX as invalid time64.secs.
             * As we can't *add* to the max, we can ignore the tz offset in this case. */
            g_assert_cmpint (r_t, ==, INT64_MAX);
        else
            g_assert_cmpint (r_t, ==, f->test[i].secs - offset);
    }
    g_log_set_default_handler (hdlr, 0);
}

static GDateTime*
offset_adjust(GDateTime *gdt)
{
     Testfuncs *tf = gnc_date_load_funcs();
     GTimeZone *zone = tf->timezone_new_local();
     int interval = g_time_zone_find_interval(zone, G_TIME_TYPE_STANDARD,
					      g_date_time_to_unix(gdt));
     int offset = g_time_zone_get_offset(zone, interval) / 60;
     int off_hr = (offset / 60) + (offset % 60 ? (offset < 0 ? -1 : 1) : 0);
     int correction = off_hr < -10 ? -10 - off_hr : off_hr > 13 ? 13 - off_hr : 0;
     GDateTime* new_gdt = g_date_time_add_hours(gdt, correction);
     g_date_time_unref(gdt);
     g_slice_free(Testfuncs, tf);
     return new_gdt;
}

/*gnc_dmy2time64_neutral*/
static void
test_gnc_dmy2time64_neutral (FixtureB *f, gconstpointer pData)
{
    gchar *msg1 = "[qof_dmy2time64_neutral()] Date computation error from Y-M-D 1257-7-2: Time value is outside the supported year range.";
    gint loglevel = G_LOG_LEVEL_CRITICAL | G_LOG_FLAG_FATAL;
    gchar *logdomain = "qof.engine";
    TestErrorStruct check = {loglevel, logdomain, msg1, 0};
    GLogFunc hdlr = g_log_set_default_handler ((GLogFunc)test_null_handler, &check);
    struct tm check_tz;
    gnc_localtime_r(&(f->test[0].secs), &check_tz);
    /* gnc_dmy2time64_neutral returns the time64 for 10:59:00 Z
     * for timezones in the range -11 to +13. If the timezone being
     * tested is outside that range then the day of the month will be
     * different from the one in the test fixture and we skip the
     * test.
     */
    if (check_tz.tm_mday == f->test[0].day)
    {
         g_test_log_set_fatal_handler ((GTestLogFatalFunc)test_checked_handler, &check);
         for (int i = 0; i < sizeof(f->test)/sizeof(TimeMap); ++i)
         {
              time64 r_t = gnc_dmy2time64_neutral (f->test[i].day, f->test[i].mon,
                                                       f->test[i].yr);

              g_assert_cmpint (r_t, ==, f->test[i].secs);
         }
    }
    g_log_set_default_handler (hdlr, 0);
}

/* gnc_timezone
long int
gnc_timezone (const struct tm *tm)// C: 5 in 2  Local: 2:0:0
*/
/* static void
test_gnc_timezone (void)
{
}*/
/* time64_to_gdate
GDate time64_to_gdate (time64 t)// C: 5 in 4  Local: 0:0:0
*/
static void
test_time64_to_gdate (FixtureA *f, gconstpointer pData)
{
    GDate date1, date2;
    struct tm tm;

    g_date_clear (&date2, 1);

    date1 = time64_to_gdate (f->t0);
    gnc_localtime_r(&f->t0, &tm);
    g_date_set_dmy (&date2, tm.tm_mday, tm.tm_mon + 1, tm.tm_year + 1900);
    g_assert_cmpint (g_date_get_julian (&date1), ==,
                     g_date_get_julian (&date2));

    date1 = time64_to_gdate (f->t1);
    gnc_localtime_r(&f->t1, &tm);
    g_date_set_dmy (&date2, tm.tm_mday, tm.tm_mon + 1, tm.tm_year + 1900);
    g_assert_cmpint (g_date_get_julian (&date1), ==,
                     g_date_get_julian (&date2));

    date1 = time64_to_gdate (f->t2);
    gnc_localtime_r(&f->t2, &tm);
    g_date_set_dmy (&date2, tm.tm_mday, tm.tm_mon + 1, tm.tm_year + 1900);
    g_assert_cmpint (g_date_get_julian (&date1), ==,
                     g_date_get_julian (&date2));

    date1 = time64_to_gdate (f->t3);
    gnc_localtime_r(&f->t3, &tm);
    g_date_set_dmy (&date2, tm.tm_mday, tm.tm_mon + 1, tm.tm_year + 1900);
    g_assert_cmpint (g_date_get_julian (&date1), ==,
                     g_date_get_julian (&date2));

    date1 = time64_to_gdate (f->t4);
    gnc_localtime_r(&f->t4, &tm);
    g_date_set_dmy (&date2, tm.tm_mday, tm.tm_mon + 1, tm.tm_year + 1900);
    g_assert_cmpint (g_date_get_julian (&date1), ==,
                     g_date_get_julian (&date2));

    date1 = time64_to_gdate (f->t5);
    gnc_localtime_r(&f->t5, &tm);
    g_date_set_dmy (&date2, tm.tm_mday, tm.tm_mon + 1, tm.tm_year + 1900);
    g_assert_cmpint (g_date_get_julian (&date1), ==,
                     g_date_get_julian (&date2));
}

/* gdate_to_time64
time64 gdate_to_time64 (GDate d)// C: 7 in 6  Local: 0:0:0
*/
static void
test_gdate_to_time64 (FixtureB *f, gconstpointer pData)
{

    gchar *msg = "g_date_set_dmy: assertion 'g_date_valid_dmy (day, m, y)' failed";
    gint loglevel = G_LOG_LEVEL_CRITICAL | G_LOG_FLAG_FATAL;
    gchar *logdomain = G_LOG_DOMAIN;
    TestErrorStruct check = {loglevel, logdomain, msg, 0};
    GLogFunc hdlr = g_log_set_default_handler ((GLogFunc)test_null_handler, &check);
    struct tm check_tz;
    gnc_localtime_r(&(f->test[0].secs), &check_tz);
    /* gdate_to_time64 returns the time64 for 10:59:00 Z
     * for timezones in the range -11 to +13. If the timezone being
     * tested is outside that range then the day of the month will be
     * different from the one in the test fixture and we skip the
     * test.
     */
    if (check_tz.tm_mday == f->test[0].day)
    {
         g_test_log_set_fatal_handler ((GTestLogFatalFunc)test_checked_handler, &check);
         for (int i = 0; i < sizeof(f->test)/sizeof(TimeMap); ++i)
         {
              GDate gd, gd2;
              time64 r_t;
              g_date_clear(&gd, 1);
              g_date_clear(&gd2, 1);
              g_date_set_dmy(&gd, f->test[i].day, f->test[i].mon, f->test[i].yr);
              r_t = gdate_to_time64(gd);
              g_assert_cmpint (r_t, ==, f->test[i].secs);
              if (f->test[i].secs < INT64_MAX)
              {
                   gd2 = time64_to_gdate(r_t);
                   g_assert (g_date_compare (&gd2, &gd) == 0);
              }
         }
    }
    g_log_set_default_handler (hdlr, 0);
}
/* gnc_tm_get_day_start
static void
gnc_tm_get_day_start (struct tm *tm, time64 time_val)// Local: 3:0:0
*/
/* static void
test_gnc_tm_get_day_start (void)
{
}*/
/* gnc_tm_get_day_end
static void
gnc_tm_get_day_end (struct tm *tm, time64 time_val)// Local: 3:0:0
*/
/* static void
test_gnc_tm_get_day_end (void)
{
}*/
/* gnc_time64_get_day_start
time64
gnc_time64_get_day_start (time64 time_val)// C: 8 in 7  Local: 0:0:0
*/

static void
tm_day_begin(struct tm *tm)
{
    tm->tm_hour = 0;
    tm->tm_min = 0;
    tm->tm_sec = 0;
}

static void
test_gnc_time64_get_day_start (FixtureA *f, gconstpointer pData)
{
    struct tm tm;
    time64 t_time, r_time;

    gnc_localtime_r(&f->t0, &tm);
    tm_day_begin(&tm);
    t_time = gnc_mktime(&tm);
    r_time = gnc_time64_get_day_start (f->t0);
    g_assert_cmpint (t_time, ==, r_time);

    gnc_localtime_r(&f->t1, &tm);
    tm_day_begin(&tm);
    t_time = gnc_mktime(&tm);
    r_time = gnc_time64_get_day_start (f->t1);
    g_assert_cmpint (t_time, ==, r_time);

    gnc_localtime_r(&f->t2, &tm);
    tm_day_begin(&tm);
    t_time = gnc_mktime(&tm);
    r_time = gnc_time64_get_day_start (f->t2);
    g_assert_cmpint (t_time, ==, r_time);

    gnc_localtime_r(&f->t3, &tm);
    tm_day_begin(&tm);
    t_time = gnc_mktime(&tm);
    r_time = gnc_time64_get_day_start (f->t3);
    g_assert_cmpint (t_time, ==, r_time);

    gnc_localtime_r(&f->t4, &tm);
    tm_day_begin(&tm);
    t_time = gnc_mktime(&tm);
    r_time = gnc_time64_get_day_start (f->t4);
    g_assert_cmpint (t_time, ==, r_time);

    gnc_localtime_r(&f->t5, &tm);
    tm_day_begin(&tm);
    t_time = gnc_mktime(&tm);
    r_time = gnc_time64_get_day_start (f->t5);
    g_assert_cmpint (t_time, ==, r_time);

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

static void
test_gnc_time64_get_day_end (FixtureA *f, gconstpointer pData)
{
    struct tm tm;
    time64 t_time, r_time;

    gnc_localtime_r(&f->t0, &tm);
    tm_day_end(&tm);
    t_time = gnc_mktime(&tm);
    r_time = gnc_time64_get_day_end (f->t0);
    g_assert_cmpint (t_time, ==, r_time);

    gnc_localtime_r(&f->t1, &tm);
    tm_day_end(&tm);
    t_time = gnc_mktime(&tm);
    r_time = gnc_time64_get_day_end (f->t1);
    g_assert_cmpint (t_time, ==, r_time);

    gnc_localtime_r(&f->t2, &tm);
    tm_day_end(&tm);
    t_time = gnc_mktime(&tm);
    r_time = gnc_time64_get_day_end (f->t2);
    g_assert_cmpint (t_time, ==, r_time);

    gnc_localtime_r(&f->t3, &tm);
    tm_day_end(&tm);
    t_time = gnc_mktime(&tm);
    r_time = gnc_time64_get_day_end (f->t3);
    g_assert_cmpint (t_time, ==, r_time);

    gnc_localtime_r(&f->t4, &tm);
    tm_day_end(&tm);
    t_time = gnc_mktime(&tm);
    r_time = gnc_time64_get_day_end (f->t4);
    g_assert_cmpint (t_time, ==, r_time);

    gnc_localtime_r(&f->t5, &tm);
    tm_day_end(&tm);
    t_time = gnc_mktime(&tm);
    r_time = gnc_time64_get_day_end (f->t5);
    g_assert_cmpint (t_time, ==, r_time);

}
/* gnc_tm_get_today_start
void
gnc_tm_get_today_start (struct tm *tm)// C: 3 in 3  Local: 0:0:0
*/
/* static void
test_gnc_tm_get_today_start (void)
{
}*/
// Not Used
/* gnc_tm_get_today_end
void
gnc_tm_get_today_end (struct tm *tm)// Local: 0:0:0
*/
/* gnc_time64_get_today_start
time64
gnc_time64_get_today_start (void)// C: 7 in 4  Local: 0:0:0
*/
/* static void
test_gnc_time64_get_today_start (void)
{
}*/
/* gnc_time64_get_today_end
time64
gnc_time64_get_today_end (void)// C: 8 in 5  Local: 0:0:0
*/
/* static void
test_gnc_time64_get_today_end (void)
{
}*/
/* gnc_dow_abbrev
void
gnc_dow_abbrev(gchar *buf, int buf_len, int dow)// C: 4 in 2  Local: 0:0:0
*/
/* static void
test_gnc_dow_abbrev (void)
{
}*/
/* time64_boxed_copy_func
static gpointer
time64_boxed_copy_func( gpointer in_time64 )// Local: 0:1:0
*/
/* static void
test_time64_boxed_copy_func (void)
{
}*/
/* time64_boxed_free_func
static void
time64_boxed_free_func( gpointer in_time64 )// Local: 0:1:0
*/
/* static void
test_time64_boxed_free_func (void)
{
}*/
// Not Used
/* time64_get_type
GType
time64_get_type( void )// Local: 0:0:0
*/


void
test_suite_gnc_date (void)
{
    tz = g_time_zone_new_local();
    GNC_TEST_ADD_FUNC (suitename, "gnc localtime", test_gnc_localtime);
    GNC_TEST_ADD_FUNC (suitename, "gnc gmtime", test_gnc_gmtime);
    GNC_TEST_ADD_FUNC (suitename, "gnc mktime", test_gnc_mktime);
    GNC_TEST_ADD_FUNC (suitename, "gnc mktime normalization", test_gnc_mktime_normalization);
    GNC_TEST_ADD_FUNC (suitename, "gnc ctime", test_gnc_ctime);
    GNC_TEST_ADD_FUNC (suitename, "gnc time", test_gnc_time);

    GNC_TEST_ADD_FUNC (suitename, "gnc date dateformat to string", test_gnc_date_dateformat_to_string);
    GNC_TEST_ADD_FUNC (suitename, "gnc date string to dateformat", test_gnc_date_string_to_dateformat);
    GNC_TEST_ADD_FUNC (suitename, "gnc date monthformat to string", test_gnc_date_monthformat_to_string);
    GNC_TEST_ADD_FUNC (suitename, "gnc date string to monthformat", test_gnc_date_string_to_monthformat);
    GNC_TEST_ADD_FUNC (suitename, "time64CanonicalDayTime", test_time64CanonicalDayTime);
    GNC_TEST_ADD_FUNC (suitename, "date get last mday", test_gnc_date_get_last_mday);
    GNC_TEST_ADD_FUNC (suitename, "qof date format set", test_qof_date_format_set);
// GNC_TEST_ADD_FUNC (suitename, "qof date completion set", test_qof_date_completion_set);
    GNC_TEST_ADD_FUNC (suitename, "qof print date dmy buff", test_qof_print_date_dmy_buff);
    GNC_TEST_ADD_FUNC (suitename, "qof print date buff", test_qof_print_date_buff);
    GNC_TEST_ADD_FUNC (suitename, "qof print gdate", test_qof_print_gdate);
    GNC_TEST_ADD_FUNC (suitename, "qof print date", test_qof_print_date);
// GNC_TEST_ADD_FUNC (suitename, "floordiv", test_floordiv);
// GNC_TEST_ADD_FUNC (suitename, "qof scan date internal", test_qof_scan_date_internal);
    GNC_TEST_ADD_FUNC (suitename, "qof scan date", test_qof_scan_date);
// GNC_TEST_ADD_FUNC (suitename, "dateSeparator", test_dateSeparator);
// GNC_TEST_ADD_FUNC (suitename, "qof time format from utf8", test_qof_time_format_from_utf8);
// GNC_TEST_ADD_FUNC (suitename, "qof formatted time to utf8", test_qof_formatted_time_to_utf8);
// GNC_TEST_ADD_FUNC (suitename, "qof format time", test_qof_format_time);
// GNC_TEST_ADD_FUNC (suitename, "qof strftime", test_qof_strftime);
    GNC_TEST_ADD_FUNC (suitename, "gnc_date_timestamp", test_gnc_date_timestamp);
    GNC_TEST_ADD (suitename, "gnc iso8601 to time64 gmt", FixtureA, NULL, setup, test_gnc_iso8601_to_time64_gmt, NULL);
    GNC_TEST_ADD (suitename, "gnc time64 to iso8601 buff", FixtureA, NULL, setup, test_gnc_time64_to_iso8601_buff, NULL);
// GNC_TEST_ADD_FUNC (suitename, "gnc dmy2time64 internal", test_gnc_dmy2time64_internal);

    GNC_TEST_ADD (suitename, "gnc dmy2time64", FixtureB, NULL, setup_begin, test_gnc_dmy2time64, NULL);
    GNC_TEST_ADD (suitename, "gnc dmy2time64 end", FixtureB, NULL, setup_end, test_gnc_dmy2time64_end, NULL);
    GNC_TEST_ADD (suitename, "gnc dmy2time64 Neutral", FixtureB, NULL, setup_neutral, test_gnc_dmy2time64_neutral, NULL);
// GNC_TEST_ADD_FUNC (suitename, "gnc timezone", test_gnc_timezone);
    GNC_TEST_ADD (suitename, "time64 to gdate", FixtureA, NULL, setup, test_time64_to_gdate, NULL);
    GNC_TEST_ADD (suitename, "gdate to time64", FixtureB, NULL, setup_neutral, test_gdate_to_time64, NULL);
// GNC_TEST_ADD_FUNC (suitename, "gnc tm get day start", test_gnc_tm_get_day_start);
// GNC_TEST_ADD_FUNC (suitename, "gnc tm get day end", test_gnc_tm_get_day_end);
    GNC_TEST_ADD (suitename, "gnc time64 get day start", FixtureA, NULL, setup, test_gnc_time64_get_day_start, NULL);
    GNC_TEST_ADD (suitename, "gnc time64 get day end", FixtureA, NULL, setup, test_gnc_time64_get_day_end, NULL);
// GNC_TEST_ADD_FUNC (suitename, "gnc tm get today start", test_gnc_tm_get_today_start);
// GNC_TEST_ADD_FUNC (suitename, "gnc timet get today start", test_gnc_time64_get_today_start);
// GNC_TEST_ADD_FUNC (suitename, "gnc timet get today end", test_gnc_time64_get_today_end);
// GNC_TEST_ADD_FUNC (suitename, "gnc dow abbrev", test_gnc_dow_abbrev);
// GNC_TEST_ADD_FUNC (suitename, "time64 boxed copy func", test_time64_boxed_copy_func);
// GNC_TEST_ADD_FUNC (suitename, "time64 boxed free func", test_time64_boxed_free_func);
    g_time_zone_unref(tz);
}
