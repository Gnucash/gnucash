/********************************************************************
 * utest-gnc-date.c: GLib g_test test suite for gnc-date.c.	    *
 * Copyright 2012 John Ralls <jralls@ceridwen.us>		    *
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
#include <config.h>
#include <string.h>
#include <glib.h>
#include <unittest-support.h>
/* Add specific headers for this class */

#include "../gnc-date.h"
#include "../gnc-date-p.h"
#include <locale.h>
#include <glib/gprintf.h>
#ifndef HAVE_STRPTIME
#  include "strptime.h"
#endif

#ifdef HAVE_GLIB_2_38
#define _Q "'"
#else
#define _Q "`"
#endif

static const gchar *suitename = "/qof/gnc-date";
void test_suite_gnc_date ( void );

typedef struct
{
    GDateTime *(*new_local)(gint, gint, gint, gint, gint, gdouble);
    GDateTime *(*adjust_for_dst)(GDateTime *, GTimeZone *);
    GDateTime *(*new_from_unix_local)(time64);
    GDateTime *(*new_from_timeval_local)(GTimeVal *);
    GDateTime *(*new_now_local)(void);
    GDateTime *(*to_local)(GDateTime *);
} _GncDateTime;

static _GncDateTime gncdt;
extern void _gnc_date_time_init (_GncDateTime *);

/* gnc_localtime just creates a tm on the heap and calls
 * gnc_localtime_r with it, so this suffices to test both.
 */
static void
test_gnc_localtime (void)
{
    time64 secs[6] = {-43238956734LL, -1123692LL, 432761LL,
                      723349832LL, 887326459367LL,
                      1364160236LL // This is "Sunday 2013-03-24" (to verify the Sunday
                      // difference between g_date_time and tm->tm_wday)
                     };
    guint ind;
#ifdef __clang__
#define _func "struct tm *gnc_localtime_r(const time64 *, struct tm *)"
#else
#define _func "gnc_localtime_r"
#endif
    gchar *msg = _func ": assertion " _Q "gdt != NULL' failed";
#undef _func
    gint loglevel = G_LOG_LEVEL_CRITICAL | G_LOG_FLAG_FATAL;
    gchar *logdomain = "qof";
    TestErrorStruct check = {loglevel, logdomain, msg, 0};
    GLogFunc hdlr = g_log_set_default_handler ((GLogFunc)test_null_handler, &check);
    g_test_log_set_fatal_handler ((GTestLogFatalFunc)test_checked_handler, &check);

    for (ind = 0; ind < G_N_ELEMENTS (secs); ind++)
    {
        struct tm* time = gnc_localtime (&secs[ind]);
        GDateTime *gdt = gncdt.new_from_unix_local (secs[ind]);
        if (gdt == NULL)
        {
            g_assert (time == NULL);
            continue;
        }
        g_assert_cmpint (time->tm_year + 1900, ==, g_date_time_get_year (gdt));
        g_assert_cmpint (time->tm_mon + 1, ==, g_date_time_get_month (gdt));
        g_assert_cmpint (time->tm_mday, ==, g_date_time_get_day_of_month (gdt));
        g_assert_cmpint (time->tm_hour, ==, g_date_time_get_hour (gdt));
        g_assert_cmpint (time->tm_min, ==, g_date_time_get_minute (gdt));
        g_assert_cmpint (time->tm_sec, ==, g_date_time_get_second (gdt));
        // Watch out: struct tm has wday=0..6 with Sunday=0, but GDateTime has wday=1..7 with Sunday=7.
        g_assert_cmpint (time->tm_wday, ==, (g_date_time_get_day_of_week (gdt) % 7));
        g_assert_cmpint (time->tm_yday, ==, g_date_time_get_day_of_year (gdt));
        if (g_date_time_is_daylight_savings (gdt))
            g_assert_cmpint (time->tm_isdst, ==, 1);
        else
            g_assert_cmpint (time->tm_isdst, ==, 0);
#ifdef HAVE_STRUCT_TM_GMTOFF
        g_assert_cmpint (time->tm_gmtoff, ==,
                         g_date_time_get_utc_offset (gdt) / G_TIME_SPAN_SECOND);
#endif
        g_date_time_unref (gdt);
        gnc_tm_free (time);
    }
    g_assert_cmpint (check.hits, ==, 1);
    g_log_set_default_handler (hdlr, NULL);
}

static void
test_gnc_gmtime (void)
{
    time64 secs[6] = {-43238956734LL, -1123692LL, 432761LL,
                      723349832LL, 887326459367LL, 1175964426LL
                     };
    struct tm answers[6] =
    {
#ifdef HAVE_STRUCT_TM_GMTOFF
        { 6, 41, 2, 24, 9, -1301, 4, 297, 0, 0, NULL },
        { 48, 51, 23, 18, 11, 69, 4, 352, 0, 0, NULL },
        { 41, 12, 0, 6, 0, 70, 2, 6, 0, 0, NULL },
        { 32, 30, 2, 3, 11, 92, 4, 338, 0, 0, NULL },
        { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, NULL },
        { 6, 47, 16, 7, 3, 107, 6, 97, 0, 0, NULL },
#else
        { 6, 41, 2, 24, 9, -1301, 4, 297, 0 },
        { 48, 51, 23, 18, 11, 69, 4, 352, 0 },
        { 41, 12, 0, 6, 0, 70, 2, 6, 0 },
        { 32, 30, 2, 3, 11, 92, 4, 338, 0 },
        { 0, 0, 0, 0, 0, 0, 0, 0, 0 },
        { 6, 47, 16, 7, 3, 107, 6, 97, 0 },
#endif
    };
    guint ind;
#ifdef __clang__
#define _func "struct tm *gnc_gmtime(const time64 *)"
#else
#define _func "gnc_gmtime"
#endif
    gchar *msg = _func ": assertion " _Q "gdt != NULL' failed";
#undef _func
    gint loglevel = G_LOG_LEVEL_CRITICAL | G_LOG_FLAG_FATAL;
    gchar *logdomain = "qof";
    TestErrorStruct check = {loglevel, logdomain, msg, 0};
    GLogFunc hdlr = g_log_set_default_handler ((GLogFunc)test_null_handler, &check);
    g_test_log_set_fatal_handler ((GTestLogFatalFunc)test_checked_handler, &check);

    for (ind = 0; ind < G_N_ELEMENTS (secs); ind++)
    {
        struct tm* time = gnc_gmtime (&secs[ind]);
        GDateTime *gdt = g_date_time_new_from_unix_utc (secs[ind]);
        if (gdt == NULL)
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
        g_assert_cmpint (time->tm_isdst, ==, 0);
#ifdef HAVE_STRUCT_TM_GMTOFF
        g_assert_cmpint (time->tm_gmtoff, ==, 0);
#endif
        g_date_time_unref (gdt);
        gnc_tm_free (time);
    }
    g_assert_cmpint (check.hits, ==, 1);
    g_log_set_default_handler (hdlr, NULL);
}

static void
test_gnc_mktime (void)
{
    struct
    {
        time64 secs;
        gint wday;
        gint yday;
    } ans[5] =
    {
        { -43238956734LL, 4, 297 },
        { -1123692LL, 4, 352 },
        { 432761LL, 2, 6 },
        { 723349832LL, 4, 338 },
        { 1175964426LL, 6, 97 }
    };

    struct tm time[5] =
    {
#ifdef HAVE_STRUCT_TM_GMTOFF
        { 6, 41, 2, 24, 9, -1301, 0, 0, -1, 0, NULL },
        { 48, 51, 23, 18, 11, 69, 0, 0, -1, 0, NULL },
        { 41, 12, 0, 6, 0, 70, 0, 0, -1, 0, NULL },
        { 32, 30, 2, 3, 11, 92, 0, 0, -1, 0, NULL },
        { 6, 47, 16, 7, 3, 107, 0, 0, -1, 0, NULL },
#else
        { 6, 41, 2, 24, 9, -1301, 0, 0, -1 },
        { 48, 51, 23, 18, 11, 69, 0, 0, -1 },
        { 41, 12, 0, 6, 0, 70, 0, 0, -1 },
        { 32, 30, 2, 3, 11, 92, 0, 0, -1 },
        { 6, 47, 16, 7, 3, 107, 0, 0, -1 },
#endif
    };
    guint ind;

    for (ind = 0; ind < G_N_ELEMENTS (time); ind++)
    {
        time64 secs = gnc_mktime (&time[ind]);
        GDateTime *gdt = gncdt.new_local (time[ind].tm_year + 1900,
                                          time[ind].tm_mon + 1,
                                          time[ind].tm_mday,
                                          time[ind].tm_hour,
                                          time[ind].tm_min,
                                          (gdouble)time[ind].tm_sec);
        time64 offset = g_date_time_get_utc_offset (gdt) / G_TIME_SPAN_SECOND;
        g_assert_cmpint (secs, ==, ans[ind].secs - offset);
        g_assert_cmpint (time[ind].tm_wday, ==, ans[ind].wday);
        g_assert_cmpint (time[ind].tm_yday, ==, ans[ind].yday);
        if (g_date_time_is_daylight_savings (gdt))
            g_assert_cmpint (time[ind].tm_isdst, ==, 1);
        else
            g_assert_cmpint (time[ind].tm_isdst, ==, 0);

#ifdef HAVE_STRUCT_TM_GMTOFF
        g_assert_cmpint (time[ind].tm_gmtoff, ==, offset);
#endif
        g_date_time_unref (gdt);
    }
}

/* In addition to computing a time offset from a struct tm, mktime is
 * supposed to normalize struct tms with out-of-range values. This
 * second test exercises that facility in gnc_mktime.
 */
static void
test_gnc_mktime_normalization (void)
{
    struct answer
    {
        time64 secs;
        gint wday;
        gint yday;
    } ans = { 723349832LL, 4, 338 };

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
    for (ind = 0; ind < G_N_ELEMENTS (time); ind++)
    {
        time64 secs = gnc_mktime (&time[ind]);
        GDateTime *gdt = gncdt.new_local (time[ind].tm_year + 1900,
                                          time[ind].tm_mon + 1,
                                          time[ind].tm_mday,
                                          time[ind].tm_hour,
                                          time[ind].tm_min,
                                          (gdouble)time[ind].tm_sec);
        time64 offset = g_date_time_get_utc_offset (gdt) / G_TIME_SPAN_SECOND;
        g_assert_cmpfloat (time[ind].tm_sec, ==, normal_time.tm_sec);
        g_assert_cmpint (time[ind].tm_min, ==, normal_time.tm_min);
        g_assert_cmpint (time[ind].tm_hour, ==, normal_time.tm_hour);
        g_assert_cmpint (time[ind].tm_mday, ==, normal_time.tm_mday);
        g_assert_cmpint (time[ind].tm_mon, ==, normal_time.tm_mon);
        g_assert_cmpint (time[ind].tm_year, ==, normal_time.tm_year);
        g_assert_cmpint (secs, ==, ans.secs - offset);
        g_assert_cmpint (time[ind].tm_wday, ==, ans.wday);
        g_assert_cmpint (time[ind].tm_yday, ==, ans.yday);
        if (g_date_time_is_daylight_savings (gdt))
            g_assert_cmpint (time[ind].tm_isdst, ==, 1);
        else
            g_assert_cmpint (time[ind].tm_isdst, ==, 0);
#ifdef HAVE_STRUCT_TM_GMTOFF
        g_assert_cmpint (time[ind].tm_gmtoff, ==, offset);
#endif
        g_date_time_unref (gdt);
    }
}

static void
test_gnc_ctime (void)
{
    time64 secs[5] = {-43238956734LL, -1123692LL, 432761LL,
                      723349832LL, 1175964426LL
                     };
    guint ind;
    for (ind = 0; ind < G_N_ELEMENTS (secs); ind++)
    {
        GDateTime *gdt = gncdt.new_from_unix_local (secs[ind]);
        gchar* datestr = gnc_ctime (&secs[ind]);
        g_assert_cmpstr (datestr, ==,
                         g_date_time_format (gdt, "%a %b %e %H:%M:%S %Y"));
        g_date_time_unref (gdt);
        g_free (datestr);
    }
}

static void
test_gnc_time (void)
{
    time64 secs1, secs2;
    GDateTime *gdt;
    secs1 = gnc_time (NULL);
    secs1 = gnc_time (&secs2);
    gdt = gncdt.new_now_local ();
    g_assert_cmpint (secs1, ==, secs2);
    g_assert_cmpint (secs1, ==, g_date_time_to_unix (gdt));
    g_date_time_unref (gdt);
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

/* There are some differences between distros in the way they name
 * locales, and this can cause trouble with the locale-based
 * formatting. If you get the assert in this function, run locale -a
 * and make sure that en_US, en_GB, and fr_FR are installed and that
 * if a suffix is needed it's in the suffixes array.
 */
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
    g_assert_not_reached ();
}
/* timespec_normalize
static void
timespec_normalize(Timespec *t)// Local: 2:0:0
*/
static void
test_timespec_normalize (void)
{
    const int offset = 4396432;
    const int factor = 2;
    int base = 50;
    Timespec t = { base, factor * NANOS_PER_SECOND + offset };
    Testfuncs *tf = gnc_date_load_funcs ();

    tf->timespec_normalize (&t);
    g_assert_cmpint (t.tv_sec, ==, base + factor);
    g_assert_cmpint (t.tv_nsec, ==, offset);

    t.tv_sec = base;
    t.tv_nsec = - factor * NANOS_PER_SECOND - offset;
    tf->timespec_normalize (&t);
    g_assert_cmpint (t.tv_sec, ==, base - factor - 1);
    g_assert_cmpint (t.tv_nsec, ==, NANOS_PER_SECOND - offset);

    t.tv_sec = - base;
    t.tv_nsec = factor * NANOS_PER_SECOND + offset;
    tf->timespec_normalize (&t);
    g_assert_cmpint (t.tv_sec, ==, - base + factor + 1);
    g_assert_cmpint (t.tv_nsec, ==, - NANOS_PER_SECOND + offset);

    t.tv_sec = - base;
    t.tv_nsec = - factor * NANOS_PER_SECOND - offset;
    tf->timespec_normalize (&t);
    g_assert_cmpint (t.tv_sec, ==, - base - factor);
    g_assert_cmpint (t.tv_nsec, ==, - offset);

    g_slice_free (Testfuncs, tf);
}


/* timespec_equal
gboolean
timespec_equal (const Timespec *ta, const Timespec *tb)// C: 19 in 8  Local: 0:0:0
*/
static void
test_timespec_equal (void)
{
    const int sec_per_day = 24 * 3600;
    const int sec_per_mo = 30 * sec_per_day;
    const time64 sec_per_yr = 365 * sec_per_day;
    const int nsec1 = 439652, nsec2 = 132794892, nsec3 = 1132794892;
    const time64 secs1 = 23 * sec_per_yr + 5 * sec_per_mo + 11 * sec_per_day;
    const time64 secs2 = 21 * sec_per_yr + 11 * sec_per_mo + 19 * sec_per_day;
    const time64 secs3 = 72 * sec_per_yr + 2 * sec_per_mo + 26 * sec_per_day;
    Timespec ta = { secs1, nsec1 };
    Timespec tb = { secs2, nsec2 };
    Timespec tc = { secs1, nsec1 };
    Timespec td = { secs3, nsec1 };
    Timespec te = { secs1, nsec2 };
    Timespec tf = { secs2 - 1, nsec3 }; /* When normalized, equal to tb */

    g_assert (timespec_equal (&ta, &ta));
    g_assert (timespec_equal (&ta, &tc));
    g_assert (!timespec_equal (&ta, &tb));
    g_assert (!timespec_equal (&ta, &td));
    g_assert (!timespec_equal (&ta, &te));
    g_assert (timespec_equal (&tb, &tf));
}
/* timespec_cmp
gint
timespec_cmp(const Timespec *ta, const Timespec *tb)// C: 28 in 11  Local: 0:0:0
*/
static void
test_timespec_cmp (void)
{
    const int sec_per_day = 24 * 3600;
    const int sec_per_mo = 30 * sec_per_day;
    const time64 sec_per_yr = 365 * sec_per_day;
    const int nsec1 = 439652, nsec2 = 132794892, nsec3 = 1132794892;
    const time64 secs1 = 23 * sec_per_yr + 5 * sec_per_mo + 11 * sec_per_day;
    const time64 secs2 = 21 * sec_per_yr + 11 * sec_per_mo + 19 * sec_per_day;
    const time64 secs3 = 72 * sec_per_yr + 2 * sec_per_mo + 26 * sec_per_day;
    Timespec ta = { secs1, nsec1 };
    Timespec tb = { secs2, nsec2 };
    Timespec tc = { secs1, nsec1 };
    Timespec td = { secs3, nsec1 };
    Timespec te = { secs1, nsec2 };
    Timespec tf = { secs2 - 1, nsec3 }; /* When normalized, equal to tb */
    Timespec tg = { -secs2, nsec2 };
    Timespec th = { secs1, -nsec1 };

    g_assert_cmpint (timespec_cmp (&ta, &ta), ==, 0);
    g_assert_cmpint (timespec_cmp (&ta, &tc), ==, 0);
    g_assert_cmpint (timespec_cmp (&tf, &tb), ==, 0);
    g_assert_cmpint (timespec_cmp (&ta, &tb), ==, 1);
    g_assert_cmpint (timespec_cmp (&te, &ta), ==, 1);
    g_assert_cmpint (timespec_cmp (&td, &ta), ==, 1);
    g_assert_cmpint (timespec_cmp (&ta, &te), ==, -1);
    g_assert_cmpint (timespec_cmp (&ta, &tg), ==, 1);
    g_assert_cmpint (timespec_cmp (&th, &ta), ==, -1);

}
/* timespec_diff
Timespec
timespec_diff(const Timespec *ta, const Timespec *tb)// C: 4 in 1  Local: 0:0:0
*/
static void
test_timespec_diff (void)
{
    const gint sec_per_day = 24 * 3600;
    const gint sec_per_mo = 30 * sec_per_day;
    const time64 sec_per_yr = 365 * sec_per_day;
    const time64 nsec1 = 439652, nsec2 = 132794892, nsec3 = 1132794892;
    const time64 secs1 = 23 * sec_per_yr + 5 * sec_per_mo + 11 * sec_per_day;
    const time64 secs2 = 21 * sec_per_yr + 11 * sec_per_mo + 19 * sec_per_day;
    const time64 secs3 = 72 * sec_per_yr + 2 * sec_per_mo + 26 * sec_per_day;
    Timespec ta = { secs1, nsec1 };
    Timespec tb = { secs2, nsec2 };
    Timespec td = { secs3, nsec1 };
    Timespec te = { secs1, nsec2 };
    Timespec tf = { secs2 - 1, nsec3 }; /* When normalized, equal to tb */
    Timespec tg = { -secs2, nsec2 };
    Timespec th = { secs1, -nsec3 };

    Timespec tt = timespec_diff (&ta, &ta);

    g_assert_cmpint (tt.tv_sec, ==, 0);
    g_assert_cmpint (tt.tv_nsec, ==, 0);

    tt = timespec_diff (&ta, &tb);
    g_assert_cmpint (tt.tv_sec, ==, secs1 - secs2 - 1);
    g_assert_cmpint (tt.tv_nsec, ==, nsec1 - nsec2 + NANOS_PER_SECOND);

    tt = timespec_diff (&ta, &te);
    g_assert_cmpint (tt.tv_sec, ==, 0);
    g_assert_cmpint (tt.tv_nsec, ==, nsec1 - nsec2);

    tt = timespec_diff (&tb, &tf);
    g_assert_cmpint (tt.tv_sec, ==, 0);
    g_assert_cmpint (tt.tv_nsec, ==, 0);

    tt = timespec_diff (&tf, &th);
    if (sizeof (glong) > 4)
    {
        glong nsec_diff_norm = 2 * nsec3 - 2 * NANOS_PER_SECOND - NANOS_PER_SECOND;
        g_assert_cmpint (tt.tv_sec, ==, secs2 - secs1 + 2);
        g_assert_cmpint (tt.tv_nsec, ==,  nsec_diff_norm);
    }
    else
    {
        g_assert_cmpint (tt.tv_sec, ==, secs2 - secs1 - 3);
        g_assert_cmpint (tt.tv_nsec, <, 0); /* Overflow nanosecs */
    }
    tt = timespec_diff (&tg, &td);
    g_assert_cmpint (tt.tv_sec, ==, -secs2 - secs3 + 1);
    g_assert_cmpint (tt.tv_nsec, ==, nsec2 - nsec1 - NANOS_PER_SECOND);

}
/* timespec_abs
Timespec
timespec_abs(const Timespec *t)// C: 4 in 1  Local: 0:0:0
*/
static void
test_timespec_abs (void)
{
    const int sec_per_day = 24 * 3600;
    const int sec_per_mo = 30 * sec_per_day;
    const int sec_per_yr = 365 * sec_per_day;
    const int nsec1 = 439652, nsec2 = 132794892, nsec3 = 1132794892;
    const time64 secs1 = 23 * sec_per_yr + 5 * sec_per_mo + 11 * sec_per_day;
    const time64 secs2 = 21 * sec_per_yr + 11 * sec_per_mo + 19 * sec_per_day;
    Timespec ta = { secs1, nsec1 };
    Timespec tf = { secs2 - 1, nsec3 }; /* When normalized, equal to tb */
    Timespec tg = { -secs2, nsec2 };
    Timespec th = { secs1, -nsec1 };

    Timespec tt = timespec_abs (&ta);
    g_assert_cmpint (tt.tv_sec, ==, secs1);
    g_assert_cmpint (tt.tv_nsec, ==, nsec1);

    tt = timespec_abs (&tf);
    g_assert_cmpint (tt.tv_sec, ==, secs2);
    g_assert_cmpint (tt.tv_nsec, ==, nsec2);

    tt = timespec_abs (&tg);
    g_assert_cmpint (tt.tv_sec, ==, secs2 - 1);
    g_assert_cmpint (tt.tv_nsec, ==, NANOS_PER_SECOND - nsec2);

    tt = timespec_abs (&th);
    g_assert_cmpint (tt.tv_sec, ==, secs1 - 1);
    g_assert_cmpint (tt.tv_nsec, ==, NANOS_PER_SECOND - nsec1);

}
/* timespecCanonicalDayTime
Timespec
timespecCanonicalDayTime(Timespec t)// C: 12 in 5 SCM: 19 in 10 Local: 0:0:0
*/
static Timespec
compute_noon_of_day (Timespec *ts)
{
    GDateTime *g = gncdt.new_from_unix_local (ts->tv_sec);
    gint yr = g_date_time_get_year (g);
    gint mo = g_date_time_get_month (g);
    gint da = g_date_time_get_day_of_month (g);
    Timespec nt = {0, 0 };

    g_date_time_unref (g);
    g = gncdt.new_local (yr, mo, da, 12, 0, 0.0);
    nt.tv_sec = g_date_time_to_unix (g);
    g_date_time_unref (g);
    return nt;
}

static void
test_timespecCanonicalDayTime (void)
{
    const int sec_per_day = 24 * 3600;
    const int sec_per_mo = 30 * sec_per_day;
    const time64 sec_per_yr = 365 * sec_per_day;
    const time64 secs = 8 * 3600 + 43 * 60 + 11;
    const time64 secs1 = 23 * sec_per_yr + 5 * sec_per_mo + 11 * sec_per_day + 8 * 3600 + 43 * 60 + 11;
    const time64 secs2 = 21 * sec_per_yr + 11 * sec_per_mo + 19 * sec_per_day + 21 * 3600 + 9 * 60 + 48;
    const time64 secs3 = 72 * sec_per_yr + 2 * sec_per_mo + 26 * sec_per_day + 12 * 60;
    Timespec t0 = { secs, 0 };
    Timespec ta = { secs1, 0 };
    Timespec tb = { secs2, 0 };
    Timespec tc = { secs3, 0 };

    Timespec n0 = compute_noon_of_day (&t0);
    Timespec na = compute_noon_of_day (&ta);
    Timespec nb = compute_noon_of_day (&tb);
    Timespec nc = compute_noon_of_day (&tc);

    Timespec r0 = timespecCanonicalDayTime (t0);
    Timespec ra = timespecCanonicalDayTime (ta);
    Timespec rb = timespecCanonicalDayTime (tb);
    Timespec rc = timespecCanonicalDayTime (tc);

    g_assert_cmpint (n0.tv_sec, ==, r0.tv_sec);
    g_assert_cmpint (na.tv_sec, ==, ra.tv_sec);
    g_assert_cmpint (nb.tv_sec, ==, rb.tv_sec);
    g_assert_cmpint (nc.tv_sec, ==, rc.tv_sec);
}

/* gnc_date_get_last_mday
int gnc_date_get_last_mday (int month, int year)// C: 1  Local: 1:0:0
*/
static void
test_gnc_date_get_last_mday (void)
{
    g_assert_cmpint (gnc_date_get_last_mday (1, 1975), ==, 31);
    g_assert_cmpint (gnc_date_get_last_mday (1, 1980), ==, 31);
    g_assert_cmpint (gnc_date_get_last_mday (2, 1975), ==, 28);
    g_assert_cmpint (gnc_date_get_last_mday (2, 1980), ==, 29);
    g_assert_cmpint (gnc_date_get_last_mday (3, 1975), ==, 31);
    g_assert_cmpint (gnc_date_get_last_mday (3, 1980), ==, 31);
    g_assert_cmpint (gnc_date_get_last_mday (4, 1975), ==, 30);
    g_assert_cmpint (gnc_date_get_last_mday (4, 1980), ==, 30);
    g_assert_cmpint (gnc_date_get_last_mday (5, 1975), ==, 31);
    g_assert_cmpint (gnc_date_get_last_mday (5, 1980), ==, 31);
    g_assert_cmpint (gnc_date_get_last_mday (6, 1975), ==, 30);
    g_assert_cmpint (gnc_date_get_last_mday (6, 1980), ==, 30);
    g_assert_cmpint (gnc_date_get_last_mday (7, 1975), ==, 31);
    g_assert_cmpint (gnc_date_get_last_mday (7, 1980), ==, 31);
    g_assert_cmpint (gnc_date_get_last_mday (8, 1975), ==, 31);
    g_assert_cmpint (gnc_date_get_last_mday (8, 1980), ==, 31);
    g_assert_cmpint (gnc_date_get_last_mday (9, 1975), ==, 30);
    g_assert_cmpint (gnc_date_get_last_mday (9, 1980), ==, 30);
    g_assert_cmpint (gnc_date_get_last_mday (10, 1975), ==, 31);
    g_assert_cmpint (gnc_date_get_last_mday (10, 1980), ==, 31);
    g_assert_cmpint (gnc_date_get_last_mday (11, 1975), ==, 30);
    g_assert_cmpint (gnc_date_get_last_mday (11, 1980), ==, 30);
    g_assert_cmpint (gnc_date_get_last_mday (12, 1975), ==, 31);
    g_assert_cmpint (gnc_date_get_last_mday (12, 1980), ==, 31);
    g_assert_cmpint (gnc_date_get_last_mday (2, 2000), ==, 29);
    g_assert_cmpint (gnc_date_get_last_mday (2, 2400), ==, 28);
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
    gchar buff[MAX_DATE_LENGTH], t_buff[MAX_DATE_LENGTH];
    gchar *locale = g_strdup (setlocale (LC_TIME, NULL));
    struct tm tm = { 0, 0, 0, 0, 0, 0, 0, 0, 0
#ifdef HAVE_STRUCT_TM_GMTOFF
        , 0, 0
#endif
    };

    qof_date_format_set (QOF_DATE_FORMAT_UK);
    memset ((gpointer)buff, 0, sizeof (buff));
    g_assert_cmpint (qof_print_date_dmy_buff (buff, sizeof (buff), 23, 11, 1974), ==, strlen (buff));
    g_assert_cmpstr (buff, ==, "23/11/1974");
    memset ((gpointer)buff, 0, sizeof (buff));
    g_assert_cmpint (qof_print_date_dmy_buff (buff, sizeof (buff), 2, 2, 1961), ==, strlen (buff));
    g_assert_cmpstr (buff, ==, "02/02/1961");
    memset ((gpointer)buff, 0, sizeof (buff));
    g_assert_cmpint (qof_print_date_dmy_buff (buff, sizeof (buff), 16, 6, 2045), ==, strlen (buff));
    g_assert_cmpstr (buff, ==, "16/06/2045");

    qof_date_format_set (QOF_DATE_FORMAT_CE);
    memset ((gpointer)buff, 0, sizeof (buff));
    g_assert_cmpint (qof_print_date_dmy_buff (buff, sizeof (buff), 23, 11, 1974), ==, strlen (buff));
    g_assert_cmpstr (buff, ==, "23.11.1974");
    memset ((gpointer)buff, 0, sizeof (buff));
    g_assert_cmpint (qof_print_date_dmy_buff (buff, sizeof (buff), 2, 2, 1961), ==, strlen (buff));
    g_assert_cmpstr (buff, ==, "02.02.1961");
    memset ((gpointer)buff, 0, sizeof (buff));
    g_assert_cmpint (qof_print_date_dmy_buff (buff, sizeof (buff), 16, 6, 2045), ==, strlen (buff));
    g_assert_cmpstr (buff, ==, "16.06.2045");

    qof_date_format_set (QOF_DATE_FORMAT_US);
    memset ((gpointer)buff, 0, sizeof (buff));
    g_assert_cmpint (qof_print_date_dmy_buff (buff, sizeof (buff), 23, 11, 1974), ==, strlen (buff));
    g_assert_cmpstr (buff, ==, "11/23/1974");
    memset ((gpointer)buff, 0, sizeof (buff));
    g_assert_cmpint (qof_print_date_dmy_buff (buff, sizeof (buff), 2, 2, 1961), ==, strlen (buff));
    g_assert_cmpstr (buff, ==, "02/02/1961");
    memset ((gpointer)buff, 0, sizeof (buff));
    g_assert_cmpint (qof_print_date_dmy_buff (buff, sizeof (buff), 16, 6, 2045), ==, strlen (buff));
    g_assert_cmpstr (buff, ==, "06/16/2045");

    qof_date_format_set (QOF_DATE_FORMAT_ISO);
    memset ((gpointer)buff, 0, sizeof (buff));
    g_assert_cmpint (qof_print_date_dmy_buff (buff, sizeof (buff), 23, 11, 1974), ==, strlen (buff));
    g_assert_cmpstr (buff, ==, "1974-11-23");
    memset ((gpointer)buff, 0, sizeof (buff));
    g_assert_cmpint (qof_print_date_dmy_buff (buff, sizeof (buff), 2, 2, 1961),
                     ==, strlen (buff));
    g_assert_cmpstr (buff, ==, "1961-02-02");
    memset ((gpointer)buff, 0, sizeof (buff));
    g_assert_cmpint (qof_print_date_dmy_buff (buff, sizeof (buff), 16, 6, 2045),
                     ==, strlen (buff));
    g_assert_cmpstr (buff, ==, "2045-06-16");

    qof_date_format_set (QOF_DATE_FORMAT_LOCALE);
    test_gnc_setlocale (LC_TIME, "en_US");
    tm_set_dmy (&tm, 1974, 11, 23);
    strftime(t_buff, MAX_DATE_LENGTH, GNC_D_FMT, &tm);
    memset ((gpointer)buff, 0, sizeof (buff));
    g_assert_cmpint (qof_print_date_dmy_buff (buff, sizeof (buff), tm.tm_mday,
                     tm.tm_mon + 1, tm.tm_year + 1900),
                     ==, strlen (buff));
    g_assert_cmpstr (buff, ==, t_buff);


    tm_set_dmy (&tm, 1961, 2, 2);
    strftime(t_buff, MAX_DATE_LENGTH, GNC_D_FMT, &tm);
    memset ((gpointer)buff, 0, sizeof (buff));
    g_assert_cmpint (qof_print_date_dmy_buff (buff, sizeof (buff), tm.tm_mday,
                     tm.tm_mon + 1, tm.tm_year + 1900),
                     ==, strlen (buff));
    g_assert_cmpstr (buff, ==, t_buff);
    memset ((gpointer)buff, 0, sizeof (buff));
    tm_set_dmy (&tm, 2045, 6, 16);
    strftime(t_buff, MAX_DATE_LENGTH, GNC_D_FMT, &tm);
    memset ((gpointer)buff, 0, sizeof (buff));
    g_assert_cmpint (qof_print_date_dmy_buff (buff, sizeof (buff), tm.tm_mday,
                     tm.tm_mon + 1, tm.tm_year + 1900),
                     ==, strlen (buff));
    g_assert_cmpstr (buff, ==, t_buff);

    test_gnc_setlocale (LC_TIME, "en_GB");
    tm_set_dmy (&tm, 1974, 11, 23);
    strftime(t_buff, MAX_DATE_LENGTH, GNC_D_FMT, &tm);
    memset ((gpointer)buff, 0, sizeof (buff));
    g_assert_cmpint (qof_print_date_dmy_buff (buff, sizeof (buff), tm.tm_mday,
                     tm.tm_mon + 1, tm.tm_year + 1900),
                     ==, strlen (buff));
    g_assert_cmpstr (buff, ==, t_buff);
    tm_set_dmy (&tm, 1961, 2, 2);
    strftime(t_buff, MAX_DATE_LENGTH, GNC_D_FMT, &tm);
    memset ((gpointer)buff, 0, sizeof (buff));
    g_assert_cmpint (qof_print_date_dmy_buff (buff, sizeof (buff), tm.tm_mday,
                     tm.tm_mon + 1, tm.tm_year + 1900),
                     ==, strlen (buff));
    g_assert_cmpstr (buff, ==, t_buff);
    memset ((gpointer)buff, 0, sizeof (buff));
    tm_set_dmy (&tm, 2045, 6, 16);
    strftime(t_buff, MAX_DATE_LENGTH, GNC_D_FMT, &tm);
    memset ((gpointer)buff, 0, sizeof (buff));
    g_assert_cmpint (qof_print_date_dmy_buff (buff, sizeof (buff), tm.tm_mday,
                     tm.tm_mon + 1, tm.tm_year + 1900),
                     ==, strlen (buff));
    g_assert_cmpstr (buff, ==, t_buff);

    test_gnc_setlocale (LC_TIME, "fr_FR");
    tm_set_dmy (&tm, 1974, 11, 23);
    strftime(t_buff, MAX_DATE_LENGTH, GNC_D_FMT, &tm);
    memset ((gpointer)buff, 0, sizeof (buff));
    g_assert_cmpint (qof_print_date_dmy_buff (buff, sizeof (buff), tm.tm_mday,
                     tm.tm_mon + 1, tm.tm_year + 1900),
                     ==, strlen (buff));
    g_assert_cmpstr (buff, ==, t_buff);
    tm_set_dmy (&tm, 1961, 2, 2);
    strftime(t_buff, MAX_DATE_LENGTH, GNC_D_FMT, &tm);
    memset ((gpointer)buff, 0, sizeof (buff));
    g_assert_cmpint (qof_print_date_dmy_buff (buff, sizeof (buff), tm.tm_mday,
                     tm.tm_mon + 1, tm.tm_year + 1900),
                     ==, strlen (buff));
    g_assert_cmpstr (buff, ==, t_buff);
    memset ((gpointer)buff, 0, sizeof (buff));
    tm_set_dmy (&tm, 2045, 6, 16);
    strftime(t_buff, MAX_DATE_LENGTH, GNC_D_FMT, &tm);
    memset ((gpointer)buff, 0, sizeof (buff));
    g_assert_cmpint (qof_print_date_dmy_buff (buff, sizeof (buff), tm.tm_mday,
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
        gchar t_buff[MAX_DATE_LENGTH];                                  \
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
    gchar buff[MAX_DATE_LENGTH], t_buff[MAX_DATE_LENGTH];
    gchar *locale = g_strdup (setlocale (LC_TIME, NULL));
    GDateTime *gd1 = gncdt.new_local (1974, 11, 23, 12, 0, 0.0);
    GDateTime *gd2 = gncdt.new_local (1961, 2, 2, 12, 0, 0.0);
    GDateTime *gd3 = gncdt.new_local (2045, 6, 16, 12, 0, 0.0);

    time64 tm1 = g_date_time_to_unix (gd1);
    time64 tm2 = g_date_time_to_unix (gd2);
    time64 tm3 = g_date_time_to_unix (gd3);

    qof_date_format_set (QOF_DATE_FORMAT_UK);
    memset ((gpointer)buff, 0, sizeof (buff));
    g_assert_cmpint (qof_print_date_buff (buff, sizeof (buff), tm1),
                     ==, strlen (buff));
    g_assert_cmpstr (buff, ==, "23/11/1974");
    memset ((gpointer)buff, 0, sizeof (buff));
    g_assert_cmpint (qof_print_date_buff (buff, sizeof (buff), tm2),
                     ==, strlen (buff));
    g_assert_cmpstr (buff, ==, "02/02/1961");

    memset ((gpointer)buff, 0, sizeof (buff));
    g_assert_cmpint (qof_print_date_buff (buff, sizeof (buff), tm3),
                     ==, strlen (buff));
    g_assert_cmpstr (buff, ==, "16/06/2045");

    qof_date_format_set (QOF_DATE_FORMAT_CE);
    memset ((gpointer)buff, 0, sizeof (buff));
    g_assert_cmpint (qof_print_date_buff (buff, sizeof (buff), tm1),
                     ==, strlen (buff));
    g_assert_cmpstr (buff, ==, "23.11.1974");
    memset ((gpointer)buff, 0, sizeof (buff));
    g_assert_cmpint (qof_print_date_buff (buff, sizeof (buff), tm2),
                     ==, strlen (buff));
    g_assert_cmpstr (buff, ==, "02.02.1961");

    memset ((gpointer)buff, 0, sizeof (buff));
    g_assert_cmpint (qof_print_date_buff (buff, sizeof (buff), tm3),
                     ==, strlen (buff));
    g_assert_cmpstr (buff, ==, "16.06.2045");

    qof_date_format_set (QOF_DATE_FORMAT_US);
    memset ((gpointer)buff, 0, sizeof (buff));
    g_assert_cmpint (qof_print_date_buff (buff, sizeof (buff), tm1),
                     ==, strlen (buff));
    g_assert_cmpstr (buff, ==, "11/23/1974");
    memset ((gpointer)buff, 0, sizeof (buff));
    g_assert_cmpint (qof_print_date_buff (buff, sizeof (buff), tm2),
                     ==, strlen (buff));
    g_assert_cmpstr (buff, ==, "02/02/1961");

    memset ((gpointer)buff, 0, sizeof (buff));
    g_assert_cmpint (qof_print_date_buff (buff, sizeof (buff), tm3),
                     ==, strlen (buff));
    g_assert_cmpstr (buff, ==, "06/16/2045");

    qof_date_format_set (QOF_DATE_FORMAT_ISO);
    memset ((gpointer)buff, 0, sizeof (buff));
    g_assert_cmpint (qof_print_date_buff (buff, sizeof (buff), tm1),
                     ==, strlen (buff));
    g_assert_cmpstr (buff, ==, "1974-11-23");
    memset ((gpointer)buff, 0, sizeof (buff));
    g_assert_cmpint (qof_print_date_buff (buff, sizeof (buff), tm2),
                     ==, strlen (buff));
    g_assert_cmpstr (buff, ==, "1961-02-02");

    memset ((gpointer)buff, 0, sizeof (buff));
    g_assert_cmpint (qof_print_date_buff (buff, sizeof (buff), tm3),
                     ==, strlen (buff));
    g_assert_cmpstr (buff, ==, "2045-06-16");

    qof_date_format_set (QOF_DATE_FORMAT_LOCALE);
    test_gnc_setlocale (LC_TIME, "en_US");
    memset ((gpointer)buff, 0, sizeof (buff));
    g_assert_cmpint (qof_print_date_buff (buff, sizeof (buff), tm1),
                     ==, strlen (buff));
    g_assert_cmpstr (buff, ==, g_date_time_format (gd1, GNC_D_FMT));
    memset ((gpointer)buff, 0, sizeof (buff));
    g_assert_cmpint (qof_print_date_buff (buff, sizeof (buff), tm2),
                     ==, strlen (buff));
    g_assert_cmpstr (buff, ==, g_date_time_format (gd2, GNC_D_FMT));

    memset ((gpointer)buff, 0, sizeof (buff));
    g_assert_cmpint (qof_print_date_buff (buff, sizeof (buff), tm3),
                     ==, strlen (buff));
    g_assert_cmpstr (buff, ==, g_date_time_format (gd3, GNC_D_FMT));

    test_gnc_setlocale (LC_TIME, "en_GB");
    memset ((gpointer)buff, 0, sizeof (buff));
    g_assert_cmpint (qof_print_date_buff (buff, sizeof (buff), tm1),
                     ==, strlen (buff));
    g_assert_cmpstr (buff, ==, g_date_time_format (gd1, GNC_D_FMT));
    memset ((gpointer)buff, 0, sizeof (buff));
    g_assert_cmpint (qof_print_date_buff (buff, sizeof (buff), tm2),
                     ==, strlen (buff));
    g_assert_cmpstr (buff, ==, g_date_time_format (gd2, GNC_D_FMT));
    memset ((gpointer)buff, 0, sizeof (buff));
    g_assert_cmpint (qof_print_date_buff (buff, sizeof (buff), tm3),
                     ==, strlen (buff));
    g_assert_cmpstr (buff, ==, g_date_time_format (gd3, GNC_D_FMT));

    test_gnc_setlocale (LC_TIME, "fr_FR");
    memset ((gpointer)buff, 0, sizeof (buff));
    g_assert_cmpint (qof_print_date_buff (buff, sizeof (buff), tm1),
                     ==, strlen (buff));
    g_assert_cmpstr (buff, ==, g_date_time_format (gd1, GNC_D_FMT));
    memset ((gpointer)buff, 0, sizeof (buff));
    g_assert_cmpint (qof_print_date_buff (buff, sizeof (buff), tm2),
                     ==, strlen (buff));
    g_assert_cmpstr (buff, ==, g_date_time_format (gd2, GNC_D_FMT));
    memset ((gpointer)buff, 0, sizeof (buff));
    g_assert_cmpint (qof_print_date_buff (buff, sizeof (buff), tm3),
                     ==, strlen (buff));
    g_assert_cmpstr (buff, ==, g_date_time_format (gd3, GNC_D_FMT));

    setlocale (LC_TIME, locale);
    g_free (locale);
    g_date_time_unref (gd1);
    g_date_time_unref (gd2);
    g_date_time_unref (gd3);
}
/* qof_print_gdate
size_t
qof_print_gdate( char *buf, size_t len, const GDate *gd )// C: 6 in 5  Local: 0:0:0
*/
static void
test_qof_print_gdate (void)
{
    gchar buff[MAX_DATE_LENGTH], t_buff[MAX_DATE_LENGTH];
    gchar *locale = g_strdup (setlocale (LC_TIME, NULL));
    GDate *gd1 = g_date_new_dmy (23, 11, 1974);
    GDate *gd2 = g_date_new_dmy (2, 2, 1961);
    GDate *gd3 = g_date_new_dmy (16, 6, 2045);

    qof_date_format_set (QOF_DATE_FORMAT_UK);
    memset ((gpointer)buff, 0, sizeof (buff));
    g_assert_cmpint (qof_print_gdate (buff, sizeof (buff), gd1),
                     ==, strlen (buff));
    g_assert_cmpstr (buff, ==, "23/11/1974");
    memset ((gpointer)buff, 0, sizeof (buff));
    g_assert_cmpint (qof_print_gdate (buff, sizeof (buff), gd2),
                     ==, strlen (buff));
    g_assert_cmpstr (buff, ==, "02/02/1961");
    memset ((gpointer)buff, 0, sizeof (buff));
    g_assert_cmpint (qof_print_gdate (buff, sizeof (buff), gd3),
                     ==, strlen (buff));
    g_assert_cmpstr (buff, ==, "16/06/2045");

    qof_date_format_set (QOF_DATE_FORMAT_CE);
    memset ((gpointer)buff, 0, sizeof (buff));
    g_assert_cmpint (qof_print_gdate (buff, sizeof (buff), gd1),
                     ==, strlen (buff));
    g_assert_cmpstr (buff, ==, "23.11.1974");
    memset ((gpointer)buff, 0, sizeof (buff));
    g_assert_cmpint (qof_print_gdate (buff, sizeof (buff), gd2),
                     ==, strlen (buff));
    g_assert_cmpstr (buff, ==, "02.02.1961");
    memset ((gpointer)buff, 0, sizeof (buff));
    g_assert_cmpint (qof_print_gdate (buff, sizeof (buff), gd3),
                     ==, strlen (buff));
    g_assert_cmpstr (buff, ==, "16.06.2045");


    qof_date_format_set (QOF_DATE_FORMAT_US);
    memset ((gpointer)buff, 0, sizeof (buff));
    g_assert_cmpint (qof_print_gdate (buff, sizeof (buff), gd1),
                     ==, strlen (buff));
    g_assert_cmpstr (buff, ==, "11/23/1974");
    memset ((gpointer)buff, 0, sizeof (buff));
    g_assert_cmpint (qof_print_gdate (buff, sizeof (buff), gd2),
                     ==, strlen (buff));
    g_assert_cmpstr (buff, ==, "02/02/1961");
    memset ((gpointer)buff, 0, sizeof (buff));
    g_assert_cmpint (qof_print_gdate (buff, sizeof (buff), gd3),
                     ==, strlen (buff));
    g_assert_cmpstr (buff, ==, "06/16/2045");


    qof_date_format_set (QOF_DATE_FORMAT_ISO);
    memset ((gpointer)buff, 0, sizeof (buff));
    g_assert_cmpint (qof_print_gdate (buff, sizeof (buff), gd1),
                     ==, strlen (buff));
    g_assert_cmpstr (buff, ==, "1974-11-23");
    memset ((gpointer)buff, 0, sizeof (buff));
    g_assert_cmpint (qof_print_gdate (buff, sizeof (buff), gd2),
                     ==, strlen (buff));
    g_assert_cmpstr (buff, ==, "1961-02-02");
    memset ((gpointer)buff, 0, sizeof (buff));
    g_assert_cmpint (qof_print_gdate (buff, sizeof (buff), gd3),
                     ==, strlen (buff));
    g_assert_cmpstr (buff, ==, "2045-06-16");


    qof_date_format_set (QOF_DATE_FORMAT_LOCALE);
    test_gnc_setlocale (LC_TIME, "en_US");
    memset ((gpointer)buff, 0, sizeof (buff));
    g_assert_cmpint (qof_print_gdate (buff, sizeof (buff), gd1),
                     ==, strlen (buff));
    g_date_strftime (t_buff, MAX_DATE_LENGTH, GNC_D_FMT, gd1);
    g_assert_cmpstr (buff, ==, t_buff);
    memset ((gpointer)buff, 0, sizeof (buff));
    g_assert_cmpint (qof_print_gdate (buff, sizeof (buff), gd2),
                     ==, strlen (buff));
    g_date_strftime (t_buff, MAX_DATE_LENGTH, GNC_D_FMT, gd2);
    g_assert_cmpstr (buff, ==, t_buff);
    memset ((gpointer)buff, 0, sizeof (buff));
    g_assert_cmpint (qof_print_gdate (buff, sizeof (buff), gd3),
                     ==, strlen (buff));
    g_date_strftime (t_buff, MAX_DATE_LENGTH, GNC_D_FMT, gd3);
    g_assert_cmpstr (buff, ==, t_buff);

    test_gnc_setlocale (LC_TIME, "en_GB");
    memset ((gpointer)buff, 0, sizeof (buff));
    g_assert_cmpint (qof_print_gdate (buff, sizeof (buff), gd1),
                     ==, strlen (buff));
    g_date_strftime (t_buff, MAX_DATE_LENGTH, GNC_D_FMT, gd1);
    g_assert_cmpstr (buff, ==, t_buff);
    memset ((gpointer)buff, 0, sizeof (buff));
    g_assert_cmpint (qof_print_gdate (buff, sizeof (buff), gd2),
                     ==, strlen (buff));
    g_date_strftime (t_buff, MAX_DATE_LENGTH, GNC_D_FMT, gd2);
    g_assert_cmpstr (buff, ==, t_buff);
    memset ((gpointer)buff, 0, sizeof (buff));
    g_assert_cmpint (qof_print_gdate (buff, sizeof (buff), gd3),
                     ==, strlen (buff));
    g_date_strftime (t_buff, MAX_DATE_LENGTH, GNC_D_FMT, gd3);
    g_assert_cmpstr (buff, ==, t_buff);


    test_gnc_setlocale (LC_TIME, "fr_FR");
    memset ((gpointer)buff, 0, sizeof (buff));
    g_assert_cmpint (qof_print_gdate (buff, sizeof (buff), gd1),
                     ==, strlen (buff));
    g_date_strftime (t_buff, MAX_DATE_LENGTH, GNC_D_FMT, gd1);
    g_assert_cmpstr (buff, ==, t_buff);
    memset ((gpointer)buff, 0, sizeof (buff));
    g_assert_cmpint (qof_print_gdate (buff, sizeof (buff), gd2),
                     ==, strlen (buff));
    g_date_strftime (t_buff, MAX_DATE_LENGTH, GNC_D_FMT, gd2);
    g_assert_cmpstr (buff, ==, t_buff);
    memset ((gpointer)buff, 0, sizeof (buff));
    g_assert_cmpint (qof_print_gdate (buff, sizeof (buff), gd3),
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
	g_assert_cmpstr (buf, ==, datestr);	   \
        g_free (buf);                              \
    }

#define test_assert_qof_print_date_outside_range(time, datestr)  \
    {                                              \
        gchar *buf = qof_print_date (time);        \
	g_assert_cmpstr (buf, ==, datestr);	   \
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
    gchar *buff;
    GDateTime *gd1 = gncdt.new_local (1974, 11, 23, 12, 0, 0.0);
    GDateTime *gd2 = gncdt.new_local (1961, 2, 2, 12, 0, 0.0);
    GDateTime *gd3 = gncdt.new_local (2045, 6, 16, 12, 0, 0.0);

    time64 tm1 = g_date_time_to_unix (gd1);
    time64 tm2 = g_date_time_to_unix (gd2);
    time64 tm3 = g_date_time_to_unix (gd3);

    qof_date_format_set (QOF_DATE_FORMAT_UK);
    test_assert_qof_print_date (tm1, "23/11/1974");
    test_assert_qof_print_date_outside_range (tm2, "02/02/1961");
    test_assert_qof_print_date_outside_range (tm3, "16/06/2045");

    qof_date_format_set (QOF_DATE_FORMAT_CE);
    test_assert_qof_print_date (tm1, "23.11.1974");
    test_assert_qof_print_date_outside_range (tm2, "02.02.1961");
    test_assert_qof_print_date_outside_range (tm3, "16.06.2045");

    qof_date_format_set (QOF_DATE_FORMAT_US);
    test_assert_qof_print_date (tm1, "11/23/1974");
    test_assert_qof_print_date_outside_range (tm2, "02/02/1961");
    test_assert_qof_print_date_outside_range (tm3, "06/16/2045");

    qof_date_format_set (QOF_DATE_FORMAT_ISO);
    test_assert_qof_print_date (tm1, "1974-11-23");
    test_assert_qof_print_date_outside_range (tm2, "1961-02-02");
    test_assert_qof_print_date_outside_range (tm3, "2045-06-16");

    qof_date_format_set (QOF_DATE_FORMAT_LOCALE);
    test_gnc_setlocale (LC_TIME, "en_US");
    test_assert_qof_print_date (tm1,
                                g_date_time_format (gd1, GNC_D_FMT));
    test_assert_qof_print_date_outside_range (tm2,
            g_date_time_format (gd2, GNC_D_FMT));
    test_assert_qof_print_date_outside_range (tm3,
            g_date_time_format (gd3, GNC_D_FMT));

    test_gnc_setlocale (LC_TIME, "en_GB");
    test_assert_qof_print_date (tm1,
                                g_date_time_format (gd1, GNC_D_FMT));
    test_assert_qof_print_date_outside_range (tm2,
            g_date_time_format (gd2, GNC_D_FMT));
    test_assert_qof_print_date_outside_range (tm3,
            g_date_time_format (gd3, GNC_D_FMT));

    test_gnc_setlocale (LC_TIME, "fr_FR");
    test_assert_qof_print_date (tm1,
                                g_date_time_format (gd1, GNC_D_FMT));
    test_assert_qof_print_date_outside_range (tm2,
            g_date_time_format (gd2, GNC_D_FMT));
    test_assert_qof_print_date_outside_range (tm3,
            g_date_time_format (gd3, GNC_D_FMT));

    setlocale (LC_TIME, locale);
    g_free (locale);
    g_date_time_unref (gd1);
    g_date_time_unref (gd2);
    g_date_time_unref (gd3);
}
/* gnc_print_date
const char *
gnc_print_date (Timespec ts)// C: 11 in 9 SCM: 166 in 59 Local: 0:0:0
*/
static void
test_gnc_print_date (void)
{
    gchar t_buff[MAX_DATE_LENGTH];
    gchar *locale = g_strdup (setlocale (LC_TIME, NULL));
    GDate *gd1 = g_date_new_dmy (23, 11, 1974);
    GDate *gd2 = g_date_new_dmy (2, 2, 1961);
    GDate *gd3 = g_date_new_dmy (16, 6, 2045);
    Timespec tm1 = gdate_to_timespec (*gd1);
    Timespec tm2 = gdate_to_timespec (*gd2);
    Timespec tm3 = gdate_to_timespec (*gd3);


    qof_date_format_set (QOF_DATE_FORMAT_UK);
    g_assert_cmpstr (gnc_print_date (tm1), ==, "23/11/1974");
    g_assert_cmpstr (gnc_print_date (tm2), ==, "02/02/1961");
    g_assert_cmpstr (gnc_print_date (tm3), ==, "16/06/2045");

    qof_date_format_set (QOF_DATE_FORMAT_CE);
    g_assert_cmpstr (gnc_print_date (tm1), ==, "23.11.1974");
    g_assert_cmpstr (gnc_print_date (tm2), ==, "02.02.1961");
    g_assert_cmpstr (gnc_print_date (tm3), ==, "16.06.2045");

    qof_date_format_set (QOF_DATE_FORMAT_US);
    g_assert_cmpstr (gnc_print_date (tm1), ==, "11/23/1974");
    g_assert_cmpstr (gnc_print_date (tm2), ==, "02/02/1961");
    g_assert_cmpstr (gnc_print_date (tm3), ==, "06/16/2045");

    qof_date_format_set (QOF_DATE_FORMAT_ISO);
    g_assert_cmpstr (gnc_print_date (tm1), ==, "1974-11-23");
    g_assert_cmpstr (gnc_print_date (tm2), ==, "1961-02-02");
    g_assert_cmpstr (gnc_print_date (tm3), ==, "2045-06-16");

    qof_date_format_set (QOF_DATE_FORMAT_LOCALE);
    test_gnc_setlocale (LC_TIME, "en_US");
    g_date_strftime (t_buff, MAX_DATE_LENGTH, GNC_D_FMT, gd1);
    g_assert_cmpstr (gnc_print_date (tm1), ==, t_buff);
    g_date_strftime (t_buff, MAX_DATE_LENGTH, GNC_D_FMT, gd2);
    g_assert_cmpstr (gnc_print_date (tm2), ==, t_buff);
    g_date_strftime (t_buff, MAX_DATE_LENGTH, GNC_D_FMT, gd3);
    g_assert_cmpstr (gnc_print_date (tm3), ==, t_buff);

    test_gnc_setlocale (LC_TIME, "en_GB");
    g_date_strftime (t_buff, MAX_DATE_LENGTH, GNC_D_FMT, gd1);
    g_assert_cmpstr (gnc_print_date (tm1), ==, t_buff);
    g_date_strftime (t_buff, MAX_DATE_LENGTH, GNC_D_FMT, gd2);
    g_assert_cmpstr (gnc_print_date (tm2), ==, t_buff);
    g_date_strftime (t_buff, MAX_DATE_LENGTH, GNC_D_FMT, gd3);
    g_assert_cmpstr (gnc_print_date (tm3), ==, t_buff);

    test_gnc_setlocale (LC_TIME, "fr_FR");
    g_date_strftime (t_buff, MAX_DATE_LENGTH, GNC_D_FMT, gd1);
    g_assert_cmpstr (gnc_print_date (tm1), ==, t_buff);
    g_date_strftime (t_buff, MAX_DATE_LENGTH, GNC_D_FMT, gd2);
    g_assert_cmpstr (gnc_print_date (tm2), ==, t_buff);
    g_date_strftime (t_buff, MAX_DATE_LENGTH, GNC_D_FMT, gd3);
    g_assert_cmpstr (gnc_print_date (tm3), ==, t_buff);

    setlocale (LC_TIME, locale);
    g_free (locale);
    g_date_free (gd1);
    g_date_free (gd2);
    g_date_free (gd3);
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
    GDateTime *gdt = gncdt.new_now_local ();
    gint year = g_date_time_get_year (gdt);
    gint month = g_date_time_get_month (gdt);
    struct tm tm = { 0, 0, 0, 0, 0, 0, 0, 0, 0
#ifndef G_OS_WIN32
        , 0, 0
#endif
    };
    gchar buff[MAX_DATE_LENGTH];
    g_date_time_unref (gdt);

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
    GDateTime *gdt = gncdt.new_now_local ();
    gchar *timestr = gnc_date_timestamp ();
    struct tm tm;

    g_assert (strptime (timestr, "%Y%m%d%H%M%S", &tm));
    g_assert_cmpint (g_date_time_get_year (gdt), ==, tm.tm_year + 1900);
    g_assert_cmpint (g_date_time_get_month (gdt), ==, tm.tm_mon + 1);
    g_assert_cmpint (g_date_time_get_day_of_month (gdt), ==, tm.tm_mday);
    g_assert_cmpint (g_date_time_get_hour (gdt), ==, tm.tm_hour);
    g_assert_cmpint (g_date_time_get_minute (gdt), ==, tm.tm_min);
    g_assert_cmpint (g_date_time_get_second (gdt), ==, tm.tm_sec);

    g_date_time_unref (gdt);
    g_free (timestr);
}
/* gnc_iso8601_to_timespec_gmt
Timespec
gnc_iso8601_to_timespec_gmt(const char *str)// C: 6 in 3  Local: 0:0:0
*/
static gint
get_nanoseconds (GDateTime *gdt)
{
    return g_date_time_get_microsecond (gdt) * 1000;
}

static void
test_gnc_iso8601_to_timespec_gmt (void)
{
    GTimeZone *zulu = g_time_zone_new ("Z");
    GTimeZone *tz05 = g_time_zone_new ("-05");
    GTimeZone *tz0840 = g_time_zone_new ("+08:40");
    GDateTime *gdt1 = g_date_time_new (zulu, 1989, 3, 27, 13, 43, 27.345678);
    GDateTime *gdt2 = g_date_time_new (tz05, 2020, 11, 7, 6, 21, 19.0);
    GDateTime *gdt3 = g_date_time_new (tz0840, 2012, 7, 4, 19, 27, 44.0);
    GDateTime *gdt4 = g_date_time_new (tz05, 1961, 9, 22, 17, 53, 19.0);
    GDateTime *gdt5 = g_date_time_new (tz05, 2061, 1, 25, 23, 21, 19.0);

    Timespec t;

    t = gnc_iso8601_to_timespec_gmt (NULL);
    g_assert_cmpint (t.tv_sec, ==, 0);
    g_assert_cmpint (t.tv_nsec, ==, 0);

    t = gnc_iso8601_to_timespec_gmt ("");
    g_assert_cmpint (t.tv_sec, ==, 0);
    g_assert_cmpint (t.tv_nsec, ==, 0);

    t = gnc_iso8601_to_timespec_gmt ("1989-03-27 13:43:27.345678");
    g_assert_cmpint (t.tv_sec, ==, g_date_time_to_unix (gdt1));
    /* MinGW has some precision issues in the last microsecond digit */
#ifdef G_OS_WIN32
    g_assert_cmpint (t.tv_nsec - 2000, <=, get_nanoseconds (gdt1));
    g_assert_cmpint (t.tv_nsec + 2000, >=, get_nanoseconds (gdt1));
#else
    g_assert_cmpint (t.tv_nsec, ==, get_nanoseconds (gdt1));
#endif
    t = gnc_iso8601_to_timespec_gmt ("2020-11-7 06:21:19 -05");
    g_assert_cmpint (t.tv_sec, ==, g_date_time_to_unix (gdt2));
    g_assert_cmpint (t.tv_nsec, ==, get_nanoseconds (gdt2));

    t = gnc_iso8601_to_timespec_gmt ("2012-07-04 19:27:44.0+08:40");
    g_assert_cmpint (t.tv_sec, ==, g_date_time_to_unix (gdt3));
    g_assert_cmpint (t.tv_nsec, ==, get_nanoseconds (gdt3));

    t = gnc_iso8601_to_timespec_gmt ("1961-09-22 17:53:19 -05");
    g_assert_cmpint (t.tv_sec, ==, g_date_time_to_unix (gdt4));
    g_assert_cmpint (t.tv_nsec, ==, get_nanoseconds (gdt4));

    t = gnc_iso8601_to_timespec_gmt ("2061-01-25 23:21:19.0 -05:00");
    g_assert_cmpint (t.tv_sec, ==, g_date_time_to_unix (gdt5));
    g_assert_cmpint (t.tv_nsec, ==, get_nanoseconds (gdt5));

    g_date_time_unref (gdt1);
    g_date_time_unref (gdt2);
    g_date_time_unref (gdt3);
    g_date_time_unref (gdt4);
    g_date_time_unref (gdt5);
    g_time_zone_unref (zulu);
    g_time_zone_unref (tz05);
    g_time_zone_unref (tz0840);
}
/* gnc_timespec_to_iso8601_buff
char *
gnc_timespec_to_iso8601_buff (Timespec ts, char * buff)// C: 18 in 7  Local: 0:0:0
*/
#define ISO8601_SIZE 38
static Timespec
g_date_time_to_timespec (GDateTime *gdt)
{
    Timespec t;
    t.tv_sec = g_date_time_to_unix (gdt);
    t.tv_nsec = g_date_time_get_microsecond (gdt) * 1000;
    return t;
}

static gchar*
format_timestring (GDateTime *gdt)
{
    gchar *fmt = "%Y-%m-%d %H:%M";
    GDateTime *ngdt = gncdt.to_local (gdt);
    gchar *date_base = g_date_time_format (ngdt, fmt);
#ifdef G_OS_WIN32
    gchar *tz = g_date_time_format (ngdt, "%Z");
#else
    gchar *tz = g_date_time_format (ngdt, "%z");
#endif
    gchar *retval = g_strdup_printf ("%s:%02d.%06d %s", date_base,
                                     g_date_time_get_second (ngdt),
                                     g_date_time_get_microsecond (ngdt), tz);
    g_date_time_unref (ngdt);
    g_free (date_base);
    g_free (tz);
    return retval;
}

static void
test_gnc_timespec_to_iso8601_buff (void)
{
    GTimeZone *zulu = g_time_zone_new ("Z");
    GTimeZone *tz05 = g_time_zone_new ("-05");
    GTimeZone *tz0840 = g_time_zone_new ("+08:40");
    GDateTime *gdt0 = g_date_time_new_from_unix_utc (0);
    GDateTime *gdt1 = g_date_time_new (zulu, 1989, 3, 27, 13, 43, 27.345678);
    GDateTime *gdt2 = g_date_time_new (tz05, 2020, 11, 7, 6, 21, 19.0);
    GDateTime *gdt3 = g_date_time_new (tz0840, 2012, 7, 4, 19, 27, 44.0);
    GDateTime *gdt4 = g_date_time_new (tz05, 1961, 9, 22, 17, 53, 19.0);
    GDateTime *gdt5 = g_date_time_new (tz05, 2061, 1, 25, 23, 21, 19.0);

    gchar buff[ISO8601_SIZE];
    gchar *time_str;
    Timespec t = { 0, 0 };
    gchar *end;
    gchar *logdomain = "qof";
    guint loglevel = G_LOG_LEVEL_CRITICAL | G_LOG_FLAG_FATAL;
#ifdef __clang__
#define _func "gchar *gnc_timespec_to_iso8601_buff(Timespec, char *)"
#else
#define _func "gnc_timespec_to_iso8601_buff"
#endif
    gchar *msg = _func ": assertion " _Q "buff != NULL' failed";
#undef _func
    TestErrorStruct check = { loglevel, logdomain, msg, 0 };
    GLogFunc oldlogger = g_log_set_default_handler ((GLogFunc)test_null_handler,
                         &check);
    g_test_log_set_fatal_handler ((GTestLogFatalFunc)test_checked_handler, &check);

    memset (buff, 0, sizeof buff);

    end = gnc_timespec_to_iso8601_buff (t, NULL);
    g_assert (end == NULL);
    g_assert_cmpint (check.hits, ==, 1);

    g_log_set_default_handler (oldlogger, NULL);

    end = gnc_timespec_to_iso8601_buff (t, buff);
    g_assert_cmpint (end - buff, ==, strlen (buff));
    time_str = format_timestring (gdt0);
    g_assert_cmpstr (buff, ==, time_str);
    g_free (time_str);

    t = g_date_time_to_timespec (gdt1);
    end = gnc_timespec_to_iso8601_buff (t, buff);
    time_str = format_timestring (gdt1);
    g_assert_cmpstr (buff, ==, time_str);
    g_free (time_str);


    t = g_date_time_to_timespec (gdt2);
    end = gnc_timespec_to_iso8601_buff (t, buff);
    time_str = format_timestring (gdt2);
    g_assert_cmpstr (buff, ==, time_str);
    g_free (time_str);

    t = g_date_time_to_timespec (gdt3);
    end = gnc_timespec_to_iso8601_buff (t, buff);
    time_str = format_timestring (gdt3);
    g_assert_cmpstr (buff, ==, time_str);
    g_free (time_str);

    t = g_date_time_to_timespec (gdt4);
    end = gnc_timespec_to_iso8601_buff (t, buff);
    time_str = format_timestring (gdt4);
    g_assert_cmpstr (buff, ==, time_str);
    g_free (time_str);

    t = g_date_time_to_timespec (gdt5);
    end = gnc_timespec_to_iso8601_buff (t, buff);
    time_str = format_timestring (gdt5);
    g_assert_cmpstr (buff, ==, time_str);
    g_free (time_str);

    g_date_time_unref (gdt0);
    g_date_time_unref (gdt1);
    g_date_time_unref (gdt2);
    g_date_time_unref (gdt3);
    g_date_time_unref (gdt4);
    g_date_time_unref (gdt5);
    g_time_zone_unref (zulu);
    g_time_zone_unref (tz05);
    g_time_zone_unref (tz0840);
}
/* gnc_timespec2dmy
void
gnc_timespec2dmy (Timespec t, int *day, int *month, int *year)// C: 1  Local: 0:0:0
*/
static void
test_gnc_timespec2dmy (void)
{
    GTimeZone *zulu = g_time_zone_new ("Z");
    GTimeZone *tz05 = g_time_zone_new ("-05");
    GTimeZone *tz0840 = g_time_zone_new ("+08:40");
    GDateTime *gdt0 = g_date_time_new_from_unix_utc (0);
    GDateTime *gdt1 = g_date_time_new (zulu, 1989, 3, 27, 13, 43, 27.345678);
    GDateTime *gdt2 = g_date_time_new (tz05, 2020, 11, 7, 6, 21, 19.0);
    GDateTime *gdt3 = g_date_time_new (tz0840, 2012, 7, 4, 19, 27, 44.0);
    GDateTime *gdt4 = g_date_time_new (tz05, 1961, 9, 22, 17, 53, 19.0);
    GDateTime *gdt5 = g_date_time_new (tz05, 2061, 1, 25, 23, 21, 19.0);
    GDateTime *gdt_local;

    int day, r_day, mo, r_mo, yr, r_yr;
    Timespec t;

    t = g_date_time_to_timespec (gdt0);
    gnc_timespec2dmy (t, &r_day, &r_mo, &r_yr);
    gdt_local = gncdt.to_local (gdt0);
    g_date_time_get_ymd (gdt_local, &yr, &mo, &day);
    g_date_time_unref (gdt_local);
    g_assert_cmpint (r_day, ==, day);
    g_assert_cmpint (r_mo, ==, mo);
    g_assert_cmpint (r_yr, ==, yr);

    t = g_date_time_to_timespec (gdt1);
    gnc_timespec2dmy (t, &r_day, &r_mo, &r_yr);
    gdt_local = gncdt.to_local (gdt1);
    g_date_time_get_ymd (gdt_local, &yr, &mo, &day);
    g_date_time_unref (gdt_local);
    g_assert_cmpint (r_day, ==, day);
    g_assert_cmpint (r_mo, ==, mo);
    g_assert_cmpint (r_yr, ==, yr);

    t = g_date_time_to_timespec (gdt2);
    gnc_timespec2dmy (t, &r_day, &r_mo, &r_yr);
    gdt_local = gncdt.to_local (gdt2);
    g_date_time_get_ymd (gdt_local, &yr, &mo, &day);
    g_date_time_unref (gdt_local);
    g_assert_cmpint (r_day, ==, day);
    g_assert_cmpint (r_mo, ==, mo);
    g_assert_cmpint (r_yr, ==, yr);

    t = g_date_time_to_timespec (gdt3);
    gnc_timespec2dmy (t, &r_day, &r_mo, &r_yr);
    gdt_local = gncdt.to_local (gdt3);
    g_date_time_get_ymd (gdt_local, &yr, &mo, &day);
    g_date_time_unref (gdt_local);
    g_assert_cmpint (r_day, ==, day);
    g_assert_cmpint (r_mo, ==, mo);
    g_assert_cmpint (r_yr, ==, yr);

    t = g_date_time_to_timespec (gdt4);
    gnc_timespec2dmy (t, &r_day, &r_mo, &r_yr);
    gdt_local = gncdt.to_local (gdt4);
    g_date_time_get_ymd (gdt_local, &yr, &mo, &day);
    g_date_time_unref (gdt_local);
    g_assert_cmpint (r_day, ==, day);
    g_assert_cmpint (r_mo, ==, mo);
    g_assert_cmpint (r_yr, ==, yr);

    t = g_date_time_to_timespec (gdt5);
    gnc_timespec2dmy (t, &r_day, &r_mo, &r_yr);
    gdt_local = gncdt.to_local (gdt5);
    g_date_time_get_ymd (gdt_local, &yr, &mo, &day);
    g_date_time_unref (gdt_local);
    /* 2038 Bug */
    g_assert_cmpint (r_day, ==, day);
    g_assert_cmpint (r_mo, ==, mo);
    g_assert_cmpint (r_yr, ==, yr);

    g_date_time_unref (gdt0);
    g_date_time_unref (gdt1);
    g_date_time_unref (gdt2);
    g_date_time_unref (gdt3);
    g_date_time_unref (gdt4);
    g_date_time_unref (gdt5);
    g_time_zone_unref (zulu);
    g_time_zone_unref (tz05);
    g_time_zone_unref (tz0840);
}
/* gnc_dmy2timespec_internal
static Timespec
gnc_dmy2timespec_internal (int day, int month, int year, gboolean start_of_day)// Local: 2:0:0
*/
/* static void
test_gnc_dmy2timespec_internal (void)
{
}*/
/* gnc_dmy2timespec
Timespec
gnc_dmy2timespec (int day, int month, int year)// C: 8 in 5  Local: 1:0:0
*/
static void
test_gnc_dmy2timespec (void)
{
    GDateTime *gdt1 = gncdt.new_local (1999, 7, 21, 0, 0, 0);
    GDateTime *gdt2 = gncdt.new_local (1918, 3, 31, 0, 0, 0);
    GDateTime *gdt3 = gncdt.new_local (1918, 4, 1, 0, 0, 0);
    GDateTime *gdt4 = gncdt.new_local (2057, 11, 20, 0, 0, 0);

    gint day, mon, yr;
    Timespec t, r_t;

    t = g_date_time_to_timespec (gdt1);
    g_date_time_get_ymd (gdt1, &yr, &mon, &day);
    r_t = gnc_dmy2timespec (day, mon, yr);
    g_assert_cmpint (r_t.tv_sec, ==, t.tv_sec);
    g_assert_cmpint (r_t.tv_nsec, ==, t.tv_nsec);

    t = g_date_time_to_timespec (gdt2);
    g_date_time_get_ymd (gdt2, &yr, &mon, &day);
    r_t = gnc_dmy2timespec (day, mon, yr);
    g_assert_cmpint (r_t.tv_sec, ==, t.tv_sec);
    g_assert_cmpint (r_t.tv_nsec, ==, t.tv_nsec);

    t = g_date_time_to_timespec (gdt3);
    g_date_time_get_ymd (gdt3, &yr, &mon, &day);
    r_t = gnc_dmy2timespec (day, mon, yr);
    g_assert_cmpint (r_t.tv_sec, ==, t.tv_sec);
    g_assert_cmpint (r_t.tv_nsec, ==, t.tv_nsec);

    t = g_date_time_to_timespec (gdt4);
    g_date_time_get_ymd (gdt4, &yr, &mon, &day);
    r_t = gnc_dmy2timespec (day, mon, yr);
    g_assert_cmpint (r_t.tv_sec, ==, t.tv_sec);
    g_assert_cmpint (r_t.tv_nsec, ==, t.tv_nsec);

    g_date_time_unref (gdt1);
    g_date_time_unref (gdt2);
    g_date_time_unref (gdt3);
    g_date_time_unref (gdt4);
}
/* gnc_dmy2timespec_end
Timespec
gnc_dmy2timespec_end (int day, int month, int year)// C: 1  Local: 0:0:0
*/
static void
test_gnc_dmy2timespec_end (void)
{
    GDateTime *gdt1 = gncdt.new_local (1999, 7, 21,23,59, 59);
    GDateTime *gdt2 = gncdt.new_local (1918, 3, 30, 23, 59, 59);
    GDateTime *gdt3 = gncdt.new_local (1918, 3, 31, 23, 59, 59);
    GDateTime *gdt4 = gncdt.new_local (2057, 11, 20, 23, 59, 59);

    gint day, mon, yr;
    Timespec t, r_t;

    t = g_date_time_to_timespec (gdt1);
    g_date_time_get_ymd (gdt1, &yr, &mon, &day);
    r_t = gnc_dmy2timespec_end (day, mon, yr);
    g_assert_cmpint (r_t.tv_sec, ==, t.tv_sec);
    g_assert_cmpint (r_t.tv_nsec, ==, t.tv_nsec);

    t = g_date_time_to_timespec (gdt2);
    g_date_time_get_ymd (gdt2, &yr, &mon, &day);
    r_t = gnc_dmy2timespec_end (day, mon, yr);
    g_assert_cmpint (r_t.tv_sec, ==, t.tv_sec);
    g_assert_cmpint (r_t.tv_nsec, ==, t.tv_nsec);

    t = g_date_time_to_timespec (gdt3);
    g_date_time_get_ymd (gdt3, &yr, &mon, &day);
    r_t = gnc_dmy2timespec_end (day, mon, yr);
    g_assert_cmpint (r_t.tv_sec, ==, t.tv_sec);
    g_assert_cmpint (r_t.tv_nsec, ==, t.tv_nsec);

    t = g_date_time_to_timespec (gdt4);
    g_date_time_get_ymd (gdt4, &yr, &mon, &day);
    r_t = gnc_dmy2timespec_end (day, mon, yr);
    g_assert_cmpint (r_t.tv_sec, ==, t.tv_sec);
    g_assert_cmpint (r_t.tv_nsec, ==, t.tv_nsec);

    g_date_time_unref (gdt1);
    g_date_time_unref (gdt2);
    g_date_time_unref (gdt3);
    g_date_time_unref (gdt4);
}
/* gnc_timezone
long int
gnc_timezone (const struct tm *tm)// C: 5 in 2  Local: 2:0:0
*/
/* static void
test_gnc_timezone (void)
{
}*/
/* timespecFromtime64
void
timespecFromtime64( Timespec *ts, time64 t )// C: 22 in 11  Local: 0:0:0
*/
/* static void
test_timespecFromtime64 (void)
{
}*/
/* timespec_now
Timespec
timespec_now()// C: 2 in 2  Local: 0:0:0
*/
/* static void
test_timespec_now (void)
{
}*/
/* timespecTotime64
time64
timespecTotime64 (Timespec ts)// C: 10 in 6  Local: 1:0:0
*/
/* static void
test_timespecTotime64 (void)
{
}*/
/* timespec_to_gdate
GDate timespec_to_gdate (Timespec ts)// C: 5 in 4  Local: 0:0:0
*/
static void
test_timespec_to_gdate (void)
{
    GTimeZone *zulu = g_time_zone_new ("Z");
    GTimeZone *tz05 = g_time_zone_new ("-05");
    GTimeZone *tz0840 = g_time_zone_new ("+08:40");
    GDateTime *gdt0 = g_date_time_new_from_unix_utc (0);
    GDateTime *gdt1 = g_date_time_new (zulu, 1989, 3, 27, 13, 43, 27.345678);
    GDateTime *gdt2 = g_date_time_new (tz05, 2020, 11, 7, 6, 21, 19.0);
    GDateTime *gdt3 = g_date_time_new (tz0840, 2012, 7, 4, 19, 27, 44.0);
    GDateTime *gdt4 = g_date_time_new (tz05, 1961, 9, 22, 17, 53, 19.0);
    GDateTime *gdt5 = g_date_time_new (tz05, 2061, 1, 25, 23, 21, 19.0);
    GDateTime *gdt_local;

    gint day, mon, yr;
    GDate date1, date2;
    Timespec t;

    g_date_clear (&date2, 1);

    t = g_date_time_to_timespec (gdt0);
    date1 = timespec_to_gdate (t);
    gdt_local = gncdt.to_local (gdt0);
    g_date_time_get_ymd (gdt_local, &yr, &mon, &day);
    g_date_time_unref (gdt_local);
    g_date_set_dmy (&date2, day, mon, yr);
    g_assert_cmpint (g_date_get_julian (&date1), ==, g_date_get_julian (&date2));

    t = g_date_time_to_timespec (gdt1);
    date1 = timespec_to_gdate (t);
    gdt_local = gncdt.to_local (gdt1);
    g_date_time_get_ymd (gdt_local, &yr, &mon, &day);
    g_date_time_unref (gdt_local);
    g_date_set_dmy (&date2, day, mon, yr);
    g_assert_cmpint (g_date_get_julian (&date1), ==, g_date_get_julian (&date2));

    t = g_date_time_to_timespec (gdt2);
    date1 = timespec_to_gdate (t);
    gdt_local = gncdt.to_local (gdt2);
    g_date_time_get_ymd (gdt_local, &yr, &mon, &day);
    g_date_time_unref (gdt_local);
    g_date_set_dmy (&date2, day, mon, yr);
    g_assert_cmpint (g_date_get_julian (&date1), ==, g_date_get_julian (&date2));

    t = g_date_time_to_timespec (gdt3);
    date1 = timespec_to_gdate (t);
    gdt_local = gncdt.to_local (gdt3);
    g_date_time_get_ymd (gdt_local, &yr, &mon, &day);
    g_date_time_unref (gdt_local);
    g_date_set_dmy (&date2, day, mon, yr);
    g_assert_cmpint (g_date_get_julian (&date1), ==, g_date_get_julian (&date2));
    t = g_date_time_to_timespec (gdt4);
    date1 = timespec_to_gdate (t);
    gdt_local = gncdt.to_local (gdt4);
    g_date_time_get_ymd (gdt_local, &yr, &mon, &day);
    g_date_time_unref (gdt_local);
    g_date_set_dmy (&date2, day, mon, yr);
    g_assert_cmpint (g_date_get_julian (&date1), ==, g_date_get_julian (&date2));

    t = g_date_time_to_timespec (gdt5);
    date1 = timespec_to_gdate (t);
    gdt_local = gncdt.to_local (gdt5);
    g_date_time_get_ymd (gdt_local, &yr, &mon, &day);
    g_date_time_unref (gdt_local);
    g_date_set_dmy (&date2, day, mon, yr);
    g_assert_cmpint (g_date_get_julian (&date1),
                     ==, g_date_get_julian (&date2));

    g_date_time_unref (gdt0);
    g_date_time_unref (gdt1);
    g_date_time_unref (gdt2);
    g_date_time_unref (gdt3);
    g_date_time_unref (gdt4);
    g_date_time_unref (gdt5);
    g_time_zone_unref (zulu);
    g_time_zone_unref (tz05);
    g_time_zone_unref (tz0840);
}
/* gdate_to_timespec
Timespec gdate_to_timespec (GDate d)// C: 7 in 6  Local: 0:0:0
*/
static void
test_gdate_to_timespec (void)
{
    GDateTime *gdt1 = gncdt.new_local (1999, 7, 21, 0, 0, 0);
    GDateTime *gdt2 = gncdt.new_local (1918, 3, 31, 0, 0, 0);
    GDateTime *gdt3 = gncdt.new_local (1918, 4, 1, 0, 0, 0);
    GDateTime *gdt4 = gncdt.new_local (2057, 11, 20, 0, 0, 0);

    gint day, mon, yr;
    Timespec t, r_t;
    GDate gd;

    g_date_clear (&gd, 1);

    t = g_date_time_to_timespec (gdt1);
    g_date_time_get_ymd (gdt1, &yr, &mon, &day);
    g_date_set_dmy (&gd, day, mon, yr);
    r_t = gdate_to_timespec (gd);
    g_assert_cmpint (r_t.tv_sec, ==, t.tv_sec);
    g_assert_cmpint (r_t.tv_nsec, ==, t.tv_nsec);

    t = g_date_time_to_timespec (gdt2);
    g_date_time_get_ymd (gdt2, &yr, &mon, &day);
    g_date_set_dmy (&gd, day, mon, yr);
    r_t = gdate_to_timespec (gd);
    g_assert_cmpint (r_t.tv_sec, ==, t.tv_sec);
    g_assert_cmpint (r_t.tv_nsec, ==, t.tv_nsec);

    t = g_date_time_to_timespec (gdt3);
    g_date_time_get_ymd (gdt3, &yr, &mon, &day);
    g_date_set_dmy (&gd, day, mon, yr);
    r_t = gdate_to_timespec (gd);
    g_assert_cmpint (r_t.tv_sec, ==, t.tv_sec);
    g_assert_cmpint (r_t.tv_nsec, ==, t.tv_nsec);

    t = g_date_time_to_timespec (gdt4);
    g_date_time_get_ymd (gdt4, &yr, &mon, &day);
    g_date_set_dmy (&gd, day, mon, yr);
    r_t = gdate_to_timespec (gd);
    g_assert_cmpint (r_t.tv_sec, ==, t.tv_sec);
    g_assert_cmpint (r_t.tv_nsec, ==, t.tv_nsec);

    g_date_time_unref (gdt1);
    g_date_time_unref (gdt2);
    g_date_time_unref (gdt3);
    g_date_time_unref (gdt4);
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
test_gnc_time64_get_day_start (void)
{
    GTimeZone *zulu = g_time_zone_new ("Z");
    GTimeZone *tz05 = g_time_zone_new ("-05");
    GTimeZone *tz0840 = g_time_zone_new ("+08:40");
    GDateTime *gdt0 = g_date_time_new_from_unix_utc (0);
    GDateTime *gdt1 = g_date_time_new (zulu, 1989, 3, 27, 13, 43, 27.345678);
    GDateTime *gdt2 = g_date_time_new (tz05, 2020, 11, 7, 6, 21, 19.0);
    GDateTime *gdt3 = g_date_time_new (tz0840, 2012, 7, 4, 19, 27, 44.0);
    GDateTime *gdt4 = g_date_time_new (tz05, 1961, 9, 22, 17, 53, 19.0);
    GDateTime *gdt5 = g_date_time_new (tz05, 2061, 1, 25, 23, 21, 19.0);
    GDateTime *gdt_local, *gdt_day_begin;

    gint day, mon, yr;
    time64 time, t_time, r_time;

    gdt_local = gncdt.to_local (gdt0);
    time = g_date_time_to_unix (gdt0);
    g_date_time_get_ymd (gdt_local, &yr, &mon, &day);
    gdt_day_begin = gncdt.new_local (yr, mon, day, 0, 0, 0);
    t_time = g_date_time_to_unix (gdt_day_begin);
    r_time = gnc_time64_get_day_start (time);
    /* This will work in the half of the world where localtime is later than UTC */
    g_assert_cmpint (t_time, ==, r_time);

    gdt_local = gncdt.to_local (gdt1);
    time = g_date_time_to_unix (gdt1);
    g_date_time_get_ymd (gdt_local, &yr, &mon, &day);
    gdt_day_begin = gncdt.new_local (yr, mon, day, 0, 0, 0);
    t_time = g_date_time_to_unix (gdt_day_begin);
    r_time = gnc_time64_get_day_start (time);
    g_assert_cmpint (t_time, ==, r_time);

    gdt_local = gncdt.to_local (gdt2);
    time = g_date_time_to_unix (gdt2);
    g_date_time_get_ymd (gdt_local, &yr, &mon, &day);
    gdt_day_begin = gncdt.new_local (yr, mon, day, 0, 0, 0);
    t_time = g_date_time_to_unix (gdt_day_begin);
    r_time = gnc_time64_get_day_start (time);
    g_assert_cmpint (t_time, ==, r_time);

    gdt_local = gncdt.to_local (gdt3);
    time = g_date_time_to_unix (gdt3);
    g_date_time_get_ymd (gdt_local, &yr, &mon, &day);
    gdt_day_begin = gncdt.new_local (yr, mon, day, 0, 0, 0);
    t_time = g_date_time_to_unix (gdt_day_begin);
    r_time = gnc_time64_get_day_start (time);
    g_assert_cmpint (t_time, ==, r_time);

    gdt_local = gncdt.to_local (gdt4);
    time = g_date_time_to_unix (gdt4);
    g_date_time_get_ymd (gdt_local, &yr, &mon, &day);
    gdt_day_begin = gncdt.new_local (yr, mon, day, 0, 0, 0);
    t_time = g_date_time_to_unix (gdt_day_begin);
    r_time = gnc_time64_get_day_start (time);
    g_assert_cmpint (t_time, ==, r_time);

    gdt_local = gncdt.to_local (gdt5);
    time = g_date_time_to_unix (gdt5);
    g_date_time_get_ymd (gdt_local, &yr, &mon, &day);
    gdt_day_begin = gncdt.new_local (yr, mon, day, 0, 0, 0);
    t_time = g_date_time_to_unix (gdt_day_begin);
    r_time = gnc_time64_get_day_start (time);
    g_assert_cmpint (t_time, ==, r_time);

    g_date_time_unref (gdt0);
    g_date_time_unref (gdt1);
    g_date_time_unref (gdt2);
    g_date_time_unref (gdt3);
    g_date_time_unref (gdt4);
    g_date_time_unref (gdt5);
    g_time_zone_unref (zulu);
    g_time_zone_unref (tz05);
    g_time_zone_unref (tz0840);
}
/* gnc_time64_get_day_end
time64
gnc_time64_get_day_end (time64 time_val)// C: 12 in 8  Local: 0:0:0
*/
static void
test_gnc_time64_get_day_end (void)
{
    GTimeZone *zulu = g_time_zone_new ("Z");
    GTimeZone *tz05 = g_time_zone_new ("-05");
    GTimeZone *tz0840 = g_time_zone_new ("+08:40");
    GDateTime *gdt0 = g_date_time_new_from_unix_utc (0);
    GDateTime *gdt1 = g_date_time_new (zulu, 1989, 3, 27, 13, 43, 27.345678);
    GDateTime *gdt2 = g_date_time_new (tz05, 2020, 11, 7, 6, 21, 19.0);
    GDateTime *gdt3 = g_date_time_new (tz0840, 2012, 7, 4, 19, 27, 44.0);
    GDateTime *gdt4 = g_date_time_new (tz05, 1961, 9, 22, 17, 53, 19.0);
    GDateTime *gdt5 = g_date_time_new (tz05, 2061, 1, 25, 23, 21, 19.0);
    GDateTime *gdt_local, *gdt_day_end;

    gint day, mon, yr;
    time64 time, t_time, r_time;

    gdt_local = gncdt.to_local (gdt0);
    time = g_date_time_to_unix (gdt0);
    g_date_time_get_ymd (gdt_local, &yr, &mon, &day);
    gdt_day_end = gncdt.new_local (yr, mon, day, 23, 59, 59);
    t_time = g_date_time_to_unix (gdt_day_end);
    r_time = gnc_time64_get_day_end (time);
    g_assert_cmpint (t_time, ==, r_time);

    gdt_local = gncdt.to_local (gdt1);
    time = g_date_time_to_unix (gdt1);
    g_date_time_get_ymd (gdt_local, &yr, &mon, &day);
    gdt_day_end = gncdt.new_local (yr, mon, day, 23, 59, 59);
    t_time = g_date_time_to_unix (gdt_day_end);
    r_time = gnc_time64_get_day_end (time);
    g_assert_cmpint (t_time, ==, r_time);

    gdt_local = gncdt.to_local (gdt2);
    time = g_date_time_to_unix (gdt2);
    g_date_time_get_ymd (gdt_local, &yr, &mon, &day);
    gdt_day_end = gncdt.new_local (yr, mon, day, 23, 59, 59);
    t_time = g_date_time_to_unix (gdt_day_end);
    r_time = gnc_time64_get_day_end (time);
    g_assert_cmpint (t_time, ==, r_time);

    gdt_local = gncdt.to_local (gdt3);
    time = g_date_time_to_unix (gdt3);
    g_date_time_get_ymd (gdt_local, &yr, &mon, &day);
    gdt_day_end = gncdt.new_local (yr, mon, day, 23, 59, 59);
    t_time = g_date_time_to_unix (gdt_day_end);
    r_time = gnc_time64_get_day_end (time);
    g_assert_cmpint (t_time, ==, r_time);

    gdt_local = gncdt.to_local (gdt4);
    time = g_date_time_to_unix (gdt4);
    g_date_time_get_ymd (gdt_local, &yr, &mon, &day);
    gdt_day_end = gncdt.new_local (yr, mon, day, 23, 59, 59);
    t_time = g_date_time_to_unix (gdt_day_end);
    r_time = gnc_time64_get_day_end (time);
    g_assert_cmpint (t_time, ==, r_time);

    gdt_local = gncdt.to_local (gdt5);
    time = g_date_time_to_unix (gdt5);
    g_date_time_get_ymd (gdt_local, &yr, &mon, &day);
    gdt_day_end = gncdt.new_local (yr, mon, day, 23, 59, 59);
    t_time = g_date_time_to_unix (gdt_day_end);
    r_time = gnc_time64_get_day_end (time);
    g_assert_cmpint (t_time, ==, r_time);

    g_date_time_unref (gdt0);
    g_date_time_unref (gdt1);
    g_date_time_unref (gdt2);
    g_date_time_unref (gdt3);
    g_date_time_unref (gdt4);
    g_date_time_unref (gdt5);
    g_time_zone_unref (zulu);
    g_time_zone_unref (tz05);
    g_time_zone_unref (tz0840);
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
/* timespec_boxed_copy_func
static gpointer
timespec_boxed_copy_func( gpointer in_timespec )// Local: 0:1:0
*/
/* static void
test_timespec_boxed_copy_func (void)
{
}*/
/* timespec_boxed_free_func
static void
timespec_boxed_free_func( gpointer in_timespec )// Local: 0:1:0
*/
/* static void
test_timespec_boxed_free_func (void)
{
}*/
// Not Used
/* timespec_get_type
GType
timespec_get_type( void )// Local: 0:0:0
*/


void
test_suite_gnc_date (void)
{
    _gnc_date_time_init (&gncdt);


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
    GNC_TEST_ADD_FUNC (suitename, "timespec normalize", test_timespec_normalize);
    GNC_TEST_ADD_FUNC (suitename, "timespec equal", test_timespec_equal);
    GNC_TEST_ADD_FUNC (suitename, "timespec cmp", test_timespec_cmp);
    GNC_TEST_ADD_FUNC (suitename, "timespec diff", test_timespec_diff);
    GNC_TEST_ADD_FUNC (suitename, "timespec abs", test_timespec_abs);
    GNC_TEST_ADD_FUNC (suitename, "timespecCanonicalDayTime", test_timespecCanonicalDayTime);
    GNC_TEST_ADD_FUNC (suitename, "date get last mday", test_gnc_date_get_last_mday);
    GNC_TEST_ADD_FUNC (suitename, "qof date format set", test_qof_date_format_set);
// GNC_TEST_ADD_FUNC (suitename, "qof date completion set", test_qof_date_completion_set);
    GNC_TEST_ADD_FUNC (suitename, "qof print date dmy buff", test_qof_print_date_dmy_buff);
    GNC_TEST_ADD_FUNC (suitename, "qof print date buff", test_qof_print_date_buff);
    GNC_TEST_ADD_FUNC (suitename, "qof print gdate", test_qof_print_gdate);
    GNC_TEST_ADD_FUNC (suitename, "qof print date", test_qof_print_date);
    GNC_TEST_ADD_FUNC (suitename, "gnc print date", test_gnc_print_date);
// GNC_TEST_ADD_FUNC (suitename, "floordiv", test_floordiv);
// GNC_TEST_ADD_FUNC (suitename, "qof scan date internal", test_qof_scan_date_internal);
    GNC_TEST_ADD_FUNC (suitename, "qof scan date", test_qof_scan_date);
// GNC_TEST_ADD_FUNC (suitename, "dateSeparator", test_dateSeparator);
// GNC_TEST_ADD_FUNC (suitename, "qof time format from utf8", test_qof_time_format_from_utf8);
// GNC_TEST_ADD_FUNC (suitename, "qof formatted time to utf8", test_qof_formatted_time_to_utf8);
// GNC_TEST_ADD_FUNC (suitename, "qof format time", test_qof_format_time);
// GNC_TEST_ADD_FUNC (suitename, "qof strftime", test_qof_strftime);
    GNC_TEST_ADD_FUNC (suitename, "gnc_date_timestamp", test_gnc_date_timestamp);
    GNC_TEST_ADD_FUNC (suitename, "gnc iso8601 to timespec gmt", test_gnc_iso8601_to_timespec_gmt);
    GNC_TEST_ADD_FUNC (suitename, "gnc timespec to iso8601 buff", test_gnc_timespec_to_iso8601_buff);
    GNC_TEST_ADD_FUNC (suitename, "gnc timespec2dmy", test_gnc_timespec2dmy);
// GNC_TEST_ADD_FUNC (suitename, "gnc dmy2timespec internal", test_gnc_dmy2timespec_internal);
    GNC_TEST_ADD_FUNC (suitename, "gnc dmy2timespec", test_gnc_dmy2timespec);
    GNC_TEST_ADD_FUNC (suitename, "gnc dmy2timespec end", test_gnc_dmy2timespec_end);
// GNC_TEST_ADD_FUNC (suitename, "gnc timezone", test_gnc_timezone);
// GNC_TEST_ADD_FUNC (suitename, "timespecFromTime t", test_timespecFromtime64);
// GNC_TEST_ADD_FUNC (suitename, "timespec now", test_timespec_now);
// GNC_TEST_ADD_FUNC (suitename, "timespecToTime t", test_timespecTotime64);
    GNC_TEST_ADD_FUNC (suitename, "timespec to gdate", test_timespec_to_gdate);
    GNC_TEST_ADD_FUNC (suitename, "gdate to timespec", test_gdate_to_timespec);
// GNC_TEST_ADD_FUNC (suitename, "gnc tm get day start", test_gnc_tm_get_day_start);
// GNC_TEST_ADD_FUNC (suitename, "gnc tm get day end", test_gnc_tm_get_day_end);
    GNC_TEST_ADD_FUNC (suitename, "gnc time64 get day start", test_gnc_time64_get_day_start);
    GNC_TEST_ADD_FUNC (suitename, "gnc time64 get day end", test_gnc_time64_get_day_end);
// GNC_TEST_ADD_FUNC (suitename, "gnc tm get today start", test_gnc_tm_get_today_start);
// GNC_TEST_ADD_FUNC (suitename, "gnc timet get today start", test_gnc_time64_get_today_start);
// GNC_TEST_ADD_FUNC (suitename, "gnc timet get today end", test_gnc_time64_get_today_end);
// GNC_TEST_ADD_FUNC (suitename, "gnc dow abbrev", test_gnc_dow_abbrev);
// GNC_TEST_ADD_FUNC (suitename, "timespec boxed copy func", test_timespec_boxed_copy_func);
// GNC_TEST_ADD_FUNC (suitename, "timespec boxed free func", test_timespec_boxed_free_func);

}
