/********************************************************************\
 * gnc-date.c -- misc utility functions to handle date and time     *
 *         (to be renamed qofdate.c in libqof2)                     *
 *                                                                  *
 * Copyright (C) 1997 Robin D. Clark <rclark@cs.hmc.edu>            *
 * Copyright (C) 1998-2000, 2003 Linas Vepstas <linas@linas.org>    *
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

#define __EXTENSIONS__

#include "config.h"
#include <glib.h>
#include <glib/gprintf.h>
/* to be renamed qofdate.c */
#include <ctype.h>

#ifdef HAVE_LANGINFO_D_FMT
#  include <langinfo.h>
#endif

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#include <glib.h>
#include <glib/gi18n.h>

#include "gnc-date-p.h"
#include "qof.h"

#ifndef HAVE_STRPTIME
#include "strptime.h"
#endif
#ifndef HAVE_LOCALTIME_R
#include "localtime_r.h"
#endif
#include "platform.h"

#ifdef G_OS_WIN32
#  include <windows.h>
#endif

#ifdef HAVE_LANGINFO_D_FMT
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

const char *gnc_default_strftime_date_format =
#ifdef G_OS_WIN32
    /* The default date format for use with strftime in Win32. */
    N_("%B %#d, %Y")
#else
    /* The default date format for use with strftime in other OS. */
    /* Translators: call "man strftime" for possible values. */
    N_("%B %e, %Y")
#endif
    ;

/* This is now user configured through the gnome options system() */
static QofDateFormat dateFormat = QOF_DATE_FORMAT_LOCALE;
static QofDateFormat prevQofDateFormat = QOF_DATE_FORMAT_LOCALE;

static QofDateCompletion dateCompletion = QOF_DATE_COMPLETION_THISYEAR;
static int dateCompletionBackMonths = 6;

/* This static indicates the debugging module that this .o belongs to. */
static QofLogModule log_module = QOF_MOD_ENGINE;

/***********************************************************\
 * GLib's GTimeZone doesn't work with MSWindows, which in turn breaks
 * g_date_time_new_local, g_date_time_new_from_unix_local,
 * g_date_time_new_from_timeval_local, and gnc_g_date_time_to_local. The
 * following functions provide a work-around.
 */
static GTimeZone*
gnc_g_time_zone_new_local (void)
{
#ifndef G_OS_WIN32
    return g_time_zone_new_local();
#else
    TIME_ZONE_INFORMATION tzinfo;
    gint64 dst = GetTimeZoneInformation (&tzinfo);
    gint bias = tzinfo.Bias + tzinfo.StandardBias;
    gint hours = -bias / 60; // 60 minutes per hour
    gint minutes = (bias < 0 ? -bias : bias) % 60;
    gchar *tzstr = g_strdup_printf ("%+03d:%02d", hours, minutes);
    GTimeZone *tz = g_time_zone_new(tzstr);
    g_free (tzstr);
    return tz;
#endif
}

#ifdef G_OS_WIN32
static gboolean
win32_in_dst (GDateTime *date, TIME_ZONE_INFORMATION *tzinfo)
{
    guint year, month, day;
    SYSTEMTIME *std, *dlt;

    if (tzinfo == NULL || tzinfo->StandardDate.wMonth == 0)
      return FALSE;

    year = g_date_time_get_year (date);
    month = g_date_time_get_month (date);
    day = g_date_time_get_day_of_month (date);

    std = &(tzinfo->StandardDate);
    dlt = &(tzinfo->DaylightDate);

    if (std->wMonth < dlt->wMonth)
    {
         if ((month > dlt->wMonth || month < std->wMonth) ||
	     (month == dlt->wMonth && day > dlt->wDay) ||
	     (month == std->wMonth && day < std->wDay))
	     return TRUE;
    }
    else
    {
         if ((month > dlt->wMonth && month < std->wMonth) ||
	     (month == dlt->wMonth && day > dlt->wDay) ||
	     (month == std->wMonth && day < std->wDay))
	     return TRUE;
    }
    return FALSE;
}
#endif

static GTimeZone*
gnc_g_time_zone_adjust_for_dst (GTimeZone* tz, GDateTime *date)
{
#ifdef G_OS_WIN32
    TIME_ZONE_INFORMATION tzinfo;
    gint64 dst = GetTimeZoneInformation (&tzinfo);
    gint bias, hours, minutes;
    gchar *tzstr;
    g_return_val_if_fail (date != NULL, NULL);
    if (dst > 0 && win32_in_dst (date, &tzinfo))
    {
        g_time_zone_unref (tz);
	bias = tzinfo.Bias + tzinfo.DaylightBias;
	hours = -bias / 60; // 60 minutes per hour
	minutes = (bias < 0 ? -bias : bias) % 60;
	tzstr = g_strdup_printf ("%+03d:%02d", hours, minutes);
	tz = g_time_zone_new(tzstr);
    }
#endif
    return tz;
}

static GDateTime*
gnc_g_date_time_new_local (gint year, gint month, gint day, gint hour, gint minute, gdouble seconds)
{
#ifndef G_OS_WIN32
    return g_date_time_new_local (year, month, day, hour, minute, seconds);
#else
    GTimeZone *tz = gnc_g_time_zone_new_local();
    GDateTime *gdt = g_date_time_new (tz, year, month, day,
				      hour, minute, seconds);
    if (!gdt)
	return gdt;
    tz = gnc_g_time_zone_adjust_for_dst (tz, gdt);
    g_date_time_unref (gdt);
/* g_date_time_new truncates nanoseconds to microseconds. Sometimes in
 * converting (particularly when parsing from a string) the
 * nanoseconds will have lost 1/2 a femtosecond or so. Adding 1/2 a
 * nano second ensures that the truncation doesn't lose a micorsecond
 * in translation.
 */
    seconds += 5e-10;
    gdt =  g_date_time_new (tz, year, month, day, hour, minute, seconds);
    g_time_zone_unref (tz);
    return gdt;
#endif
}

static GDateTime*
gnc_g_date_time_adjust_for_dst (GDateTime *gdt, GTimeZone *tz)
{
    GDateTime *ngdt;
    g_return_val_if_fail (gdt != NULL, NULL);
    ngdt = g_date_time_to_timezone (gdt, tz);
    g_date_time_unref (gdt);
    tz = gnc_g_time_zone_adjust_for_dst (tz, ngdt);
    gdt = g_date_time_to_timezone (ngdt, tz);
    g_date_time_unref (ngdt);
    g_time_zone_unref (tz);
    return gdt;
}

GDateTime*
gnc_g_date_time_new_from_unix_local (time64 time)
{
#ifndef G_OS_WIN32
    return g_date_time_new_from_unix_local (time);
#else
    GTimeZone *tz = gnc_g_time_zone_new_local ();
    GDateTime *gdt = g_date_time_new_from_unix_utc (time);
    if (!gdt)
      return gdt;
    return gnc_g_date_time_adjust_for_dst (gdt, tz);
#endif
}

static GDateTime*
gnc_g_date_time_new_from_timeval_local (const GTimeVal* tv)
{
#ifndef G_OS_WIN32
    return g_date_time_new_from_timeval_local (tv);
#else
    GTimeZone *tz = gnc_g_time_zone_new_local ();
    GDateTime *gdt = g_date_time_new_from_timeval_utc (tv);
    if (!gdt)
	return gdt;
    return gnc_g_date_time_adjust_for_dst (gdt, tz);
#endif
}

static GDateTime*
gnc_g_date_time_new_now_local (void)
{
#ifndef G_OS_WIN32
    return g_date_time_new_now_local ();
#else
    GTimeZone *tz = gnc_g_time_zone_new_local ();
    GDateTime *gdt = g_date_time_new_now_local ();
    if (!gdt)
	return gdt;
    return gnc_g_date_time_adjust_for_dst (gdt, tz);
#endif
}

static GDateTime*
gnc_g_date_time_to_local (GDateTime* gdt)
{
#ifndef G_OS_WIN32
    return g_date_time_to_local (gdt);
#else
    GTimeZone *tz = gnc_g_time_zone_new_local ();
    return gnc_g_date_time_adjust_for_dst (g_date_time_to_utc (gdt), tz);
#endif
}

typedef struct
{
    GDateTime *(*new_local)(gint, gint, gint, gint, gint, gdouble);
    GDateTime *(*adjust_for_dst)(GDateTime *, GTimeZone *);
    GDateTime *(*new_from_unix_local)(time64);
    GDateTime *(*new_from_timeval_local)(const GTimeVal *);
    GDateTime *(*new_now_local)(void);
    GDateTime *(*to_local)(GDateTime *);
} _GncDateTime;

void _gnc_date_time_init(_GncDateTime*);
void
_gnc_date_time_init (_GncDateTime *gncdt)
{
    gncdt->new_local = gnc_g_date_time_new_local;
    gncdt->adjust_for_dst = gnc_g_date_time_adjust_for_dst;
    gncdt->new_from_unix_local = gnc_g_date_time_new_from_unix_local;
    gncdt->new_from_timeval_local = gnc_g_date_time_new_from_timeval_local;
    gncdt->new_now_local = gnc_g_date_time_new_now_local;
    gncdt->to_local = gnc_g_date_time_to_local;
}

/****************** Posix Replacement Functions ***************************/
void
gnc_tm_free (struct tm* time)
{
     g_slice_free1 (sizeof (struct tm), time);
}

#define MAX_TZ_SIZE
static void
gnc_g_date_time_fill_struct_tm (GDateTime *gdt, struct tm* time)
{
     memset (time, 0, sizeof (struct tm));
     g_date_time_get_ymd (gdt, &(time->tm_year), &(time->tm_mon), &(time->tm_mday));
     time->tm_sec = g_date_time_get_second (gdt);
     time->tm_min = g_date_time_get_minute (gdt);
     time->tm_hour = g_date_time_get_hour (gdt);
     // Watch out: struct tm has wday=0..6 with Sunday=0, but GDateTime has wday=1..7 with Sunday=7.
     time->tm_wday = g_date_time_get_day_of_week (gdt) % 7;
     time->tm_yday = g_date_time_get_day_of_year (gdt);
     time->tm_isdst = g_date_time_is_daylight_savings (gdt);
     time->tm_year -= 1900;
     --time->tm_mon;
}

struct tm*
gnc_localtime (const time64 *secs)
{
     struct tm *time = g_slice_alloc0 (sizeof (struct tm));
     if (gnc_localtime_r (secs, time) == NULL)
     {
	  gnc_tm_free (time);
	  return NULL;
     }
     return time;
}

/* Linux, Darwin, and MSWindows implementations of this function set the
 * globals timezone and daylight; BSD doesn't have those globals, and
 * Gnucash never uses them, so they're omitted from this
 * implementation. Bug 704185.
 */
struct tm*
gnc_localtime_r (const time64 *secs, struct tm* time)
{
     guint index = 0;
     GDateTime *gdt = gnc_g_date_time_new_from_unix_local (*secs);
     g_return_val_if_fail (gdt != NULL, NULL);

     gnc_g_date_time_fill_struct_tm (gdt, time);
     if (g_date_time_is_daylight_savings (gdt))
     {
	  index = 1;
          time->tm_isdst = 1;
     }

#ifdef HAVE_STRUCT_TM_GMTOFF
     time->tm_gmtoff = g_date_time_get_utc_offset (gdt) / G_TIME_SPAN_SECOND;
#endif

     g_date_time_unref (gdt);
     return time;
}

struct tm*
gnc_gmtime (const time64 *secs)
{
     struct tm *time;
     GDateTime *gdt = g_date_time_new_from_unix_utc (*secs);
     g_return_val_if_fail (gdt != NULL, NULL);
     time = g_slice_alloc0 (sizeof (struct tm));
     gnc_g_date_time_fill_struct_tm (gdt, time);
     g_date_time_unref (gdt);
     return time;
}

static void
normalize_time_component (gint *inner, gint *outer, guint divisor, gint base)
{
     while (*inner < base)
     {
	  --(*outer);
	  *inner += divisor;
     }
     while (*inner > divisor)
     {
	  ++(*outer);
	  *inner -= divisor;
     }
}

static gint
normalize_month (gint month)
{
     month = (month % 12 + 12) % 12;
     return month == 0 ? 12 : month;
}

static void
normalize_struct_tm (struct tm* time)
{
     gint year = time->tm_year + 1900;
     gint last_day;
     time64 secs;

     ++time->tm_mon;
     /* GDateTime doesn't protect itself against out-of range years,
      * so clamp year into GDateTime's range.
      */
     if (year < 0) year = -year;
     if (year > 9999) year %= 10000;

     normalize_time_component (&(time->tm_sec), &(time->tm_min), 60, 0);
     normalize_time_component (&(time->tm_min), &(time->tm_hour), 60, 0);
     normalize_time_component (&(time->tm_hour), &(time->tm_mday), 24, 0);
     normalize_time_component (&(time->tm_mon), &year, 12, 1);
     while (time->tm_mday < 1)
     {
	  last_day = gnc_date_get_last_mday (normalize_month (--time->tm_mon), year);
	  time->tm_mday += last_day;
	  normalize_time_component (&(time->tm_mon), &year, 12, 1);
     }
     last_day = gnc_date_get_last_mday (normalize_month (time->tm_mon), year);
     while (time->tm_mday > last_day)
     {
	  ++time->tm_mon;
	  time->tm_mday -= last_day;
	  normalize_time_component (&(time->tm_mon), &year, 12, 1);
	  last_day = gnc_date_get_last_mday (normalize_month (time->tm_mon), year);
     }
     time->tm_year = year - 1900;
}

time64
gnc_mktime (struct tm* time)
{
     GDateTime *gdt;
     time64 secs;
     normalize_struct_tm (time);
     gdt = gnc_g_date_time_new_local (time->tm_year + 1900, time->tm_mon,
				      time->tm_mday, time->tm_hour,
				      time->tm_min, (gdouble)(time->tm_sec));
     if (gdt == NULL)
     {
         g_warning("Invalid time passed to gnc_mktime");
         return -1;
     }
     time->tm_mon = time->tm_mon > 0 ? time->tm_mon - 1 : 11;
     // Watch out: struct tm has wday=0..6 with Sunday=0, but GDateTime has wday=1..7 with Sunday=7.
     time->tm_wday = g_date_time_get_day_of_week (gdt) % 7;
     time->tm_yday = g_date_time_get_day_of_year (gdt);
     time->tm_isdst = g_date_time_is_daylight_savings (gdt);

#ifdef HAVE_STRUCT_TM_GMTOFF
     time->tm_gmtoff = g_date_time_get_utc_offset (gdt) / G_TIME_SPAN_SECOND;
#endif

     secs = g_date_time_to_unix (gdt);
     g_date_time_unref (gdt);
     return secs;
}

time64
gnc_timegm (struct tm* time)
{
     GDateTime *gdt;
     time64 secs;
     normalize_struct_tm (time);
     gdt = g_date_time_new_utc (time->tm_year + 1900, time->tm_mon,
				time->tm_mday, time->tm_hour, time->tm_min,
				(gdouble)(time->tm_sec));
     time->tm_mon = time->tm_mon > 0 ? time->tm_mon - 1 : 11;
     // Watch out: struct tm has wday=0..6 with Sunday=0, but GDateTime has wday=1..7 with Sunday=7.
     time->tm_wday = g_date_time_get_day_of_week (gdt) % 7;
     time->tm_yday = g_date_time_get_day_of_year (gdt);
     time->tm_isdst = g_date_time_is_daylight_savings (gdt);

     secs = g_date_time_to_unix (gdt);
     g_date_time_unref (gdt);
     return secs;
}

gchar*
gnc_ctime (const time64 *secs)
{
     GDateTime *gdt = gnc_g_date_time_new_from_unix_local (*secs);
     gchar *string = g_date_time_format (gdt, "%a %b %e %H:%M:%S %Y");
     g_date_time_unref (gdt);
     return string;
}

time64
gnc_time (time64 *tbuf)
{
     GDateTime *gdt = gnc_g_date_time_new_now_local ();
     time64 secs = g_date_time_to_unix (gdt);
     g_date_time_unref (gdt);
     if (tbuf != NULL)
	  *tbuf = secs;
     return secs;
}

time64
gnc_time_utc (time64 *tbuf)
{
     GDateTime *gdt = g_date_time_new_now_utc ();
     time64 secs = g_date_time_to_unix (gdt);
     g_date_time_unref (gdt);
     if (tbuf != NULL)
	  *tbuf = secs;
     return secs;
}

gdouble
gnc_difftime (const time64 secs1, const time64 secs2)
{
     return (double)secs1 - (double)secs2;
}

/****************************************************************************/

GDateTime*
gnc_g_date_time_new_from_timespec_local (Timespec ts)
{
    GDateTime *gdt1 = gnc_g_date_time_new_from_unix_local (ts.tv_sec);
    double nsecs = ((double)ts.tv_nsec + 0.5)/ 1000000000.0L;
    GDateTime *gdt2 = g_date_time_add_seconds (gdt1, nsecs);
    g_date_time_unref (gdt1);
    g_assert (g_date_time_to_unix (gdt2) == ts.tv_sec + (nsecs >= 1.0 ? (gint64)nsecs : 0));
    return gdt2;
}

const char*
gnc_date_dateformat_to_string(QofDateFormat format)
{
    switch (format)
    {
    case QOF_DATE_FORMAT_US:
        return "us";
    case QOF_DATE_FORMAT_UK:
        return "uk";
    case QOF_DATE_FORMAT_CE:
        return "ce";
    case QOF_DATE_FORMAT_ISO:
        return "iso";
    case QOF_DATE_FORMAT_UTC:
        return "utc";
    case QOF_DATE_FORMAT_LOCALE:
        return "locale";
    case QOF_DATE_FORMAT_CUSTOM:
        return "custom";
    default:
        return NULL;
    }
}

gboolean
gnc_date_string_to_dateformat(const char* fmt_str, QofDateFormat *format)
{
    if (!fmt_str)
        return TRUE;

    if (!strcmp(fmt_str, "us"))
        *format = QOF_DATE_FORMAT_US;
    else if (!strcmp(fmt_str, "uk"))
        *format = QOF_DATE_FORMAT_UK;
    else if (!strcmp(fmt_str, "ce"))
        *format = QOF_DATE_FORMAT_CE;
    else if (!strcmp(fmt_str, "utc"))
        *format = QOF_DATE_FORMAT_UTC;
    else if (!strcmp(fmt_str, "iso"))
        *format = QOF_DATE_FORMAT_ISO;
    else if (!strcmp(fmt_str, "locale"))
        *format = QOF_DATE_FORMAT_LOCALE;
    else if (!strcmp(fmt_str, "custom"))
        *format = QOF_DATE_FORMAT_CUSTOM;
    else
        return TRUE;

    return FALSE;
}


const char*
gnc_date_monthformat_to_string(GNCDateMonthFormat format)
{
    switch (format)
    {
    case GNCDATE_MONTH_NUMBER:
        return "number";
    case GNCDATE_MONTH_ABBREV:
        return "abbrev";
    case GNCDATE_MONTH_NAME:
        return "name";
    default:
        return NULL;
    }
}

gboolean
gnc_date_string_to_monthformat(const char *fmt_str, GNCDateMonthFormat *format)
{
    if (!fmt_str)
        return TRUE;

    if (!strcmp(fmt_str, "number"))
        *format = GNCDATE_MONTH_NUMBER;
    else if (!strcmp(fmt_str, "abbrev"))
        *format = GNCDATE_MONTH_ABBREV;
    else if (!strcmp(fmt_str, "name"))
        *format = GNCDATE_MONTH_NAME;
    else
        return TRUE;

    return FALSE;
}

/********************************************************************\
\********************************************************************/

static void
timespec_normalize(Timespec *t)
{
    if (t->tv_nsec > NANOS_PER_SECOND)
    {
        t->tv_sec += (t->tv_nsec / NANOS_PER_SECOND);
        t->tv_nsec = t->tv_nsec % NANOS_PER_SECOND;
    }

    if (t->tv_nsec < - NANOS_PER_SECOND)
    {
        t->tv_sec += - (-t->tv_nsec / NANOS_PER_SECOND);
        t->tv_nsec = - (-t->tv_nsec % NANOS_PER_SECOND);
    }

    if (t->tv_sec > 0 && t->tv_nsec < 0)
    {
        t->tv_sec--;
        t->tv_nsec = NANOS_PER_SECOND + t->tv_nsec;
    }

    if (t->tv_sec < 0 && t->tv_nsec > 0)
    {
        t->tv_sec++;
        t->tv_nsec = - NANOS_PER_SECOND + t->tv_nsec;
    }
    return;
}


gboolean
timespec_equal (const Timespec *ta, const Timespec *tb)
{
    Timespec pta, ptb;

    if (ta == tb) return TRUE;
/* Copy and normalize the copies */
    pta = *ta;
    ptb = *tb;
    timespec_normalize (&pta);
    timespec_normalize (&ptb);

    if (pta.tv_sec != ptb.tv_sec) return FALSE;
    if (pta.tv_nsec != ptb.tv_nsec) return FALSE;
    return TRUE;
}

gint
timespec_cmp(const Timespec *ta, const Timespec *tb)
{
    Timespec pta, ptb;

    if (ta == tb) return 0;
/* Copy and normalize the copies */
    pta = *ta;
    ptb = *tb;
    timespec_normalize (&pta);
    timespec_normalize (&ptb);

    if (pta.tv_sec < ptb.tv_sec) return -1;
    if (pta.tv_sec > ptb.tv_sec) return 1;
    if (pta.tv_nsec < ptb.tv_nsec) return -1;
    if (pta.tv_nsec > ptb.tv_nsec) return 1;
    return 0;
}

Timespec
timespec_diff(const Timespec *ta, const Timespec *tb)
{
    Timespec retval;
    retval.tv_sec = ta->tv_sec - tb->tv_sec;
    retval.tv_nsec = ta->tv_nsec - tb->tv_nsec;
    timespec_normalize(&retval);
    return retval;
}

Timespec
timespec_abs(const Timespec *t)
{
    Timespec retval = *t;

    timespec_normalize(&retval);
    if (retval.tv_sec < 0)
    {
        retval.tv_sec = - retval.tv_sec;
        retval.tv_nsec = - retval.tv_nsec;
    }

    return retval;
}

/* Converts any time on a day to midday that day.

 * given a timepair contains any time on a certain day (local time)
 * converts it to be midday that day.
 */
Timespec
timespecCanonicalDayTime(Timespec t)
{
    struct tm tm;
    Timespec retval;
    time64 t_secs = t.tv_sec + (t.tv_nsec / NANOS_PER_SECOND);
    gnc_localtime_r(&t_secs, &tm);
    gnc_tm_set_day_middle(&tm);
    retval.tv_sec = gnc_mktime(&tm);
    retval.tv_nsec = 0;
    return retval;
}

/* NB: month is 1-12, year is 0001 - 9999. */
int gnc_date_get_last_mday (int month, int year)
{
    static int last_day_of_month[2][12] =
    {
        /* non leap */ {31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31},
        /*   leap   */ {31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31}
    };

    /* Is this a leap year? */
    if (year % 2000 == 0) return last_day_of_month[1][month-1];
    if (year % 400 == 0 ) return last_day_of_month[0][month-1];
    if (year % 4   == 0 ) return last_day_of_month[1][month-1];
    return last_day_of_month[0][month-1];
}

/* Return the set dateFormat.

return QofDateFormat: enumeration indicating preferred format

Global: dateFormat
*/
QofDateFormat qof_date_format_get (void)
{
    return dateFormat;
}

/* set date format

set date format to one of US, UK, CE, ISO OR UTC
checks to make sure it's a legal value

param QofDateFormat: enumeration indicating preferred format

return void

Globals: dateFormat
*/
void qof_date_format_set(QofDateFormat df)
{
    if (df >= DATE_FORMAT_FIRST && df <= DATE_FORMAT_LAST)
    {
        prevQofDateFormat = dateFormat;
        dateFormat = df;
    }
    else
    {
        /* hack alert - Use a neutral default. */
        PERR("non-existent date format set attempted. Setting ISO default");
        prevQofDateFormat = dateFormat;
        dateFormat = QOF_DATE_FORMAT_ISO;
    }

    return;
}

/* set date completion method

set dateCompletion to one of QOF_DATE_COMPLETION_THISYEAR (for
completing the year to the current calendar year) or
QOF_DATE_COMPLETION_SLIDING (for using a sliding 12-month window). The
sliding window starts 'backmonth' months before the current month (0-11).
checks to make sure it's a legal value

param QofDateCompletion: indicates preferred completion method
param int: the number of months to go back in time (0-11)

return void

Globals: dateCompletion dateCompletionBackMonths
*/
void qof_date_completion_set(QofDateCompletion dc, int backmonths)
{
    if (dc == QOF_DATE_COMPLETION_THISYEAR ||
            dc == QOF_DATE_COMPLETION_SLIDING)
    {
        dateCompletion = dc;
    }
    else
    {
        /* hack alert - Use a neutral default. */
        PERR("non-existent date completion set attempted. Setting current year completion as default");
        dateCompletion = QOF_DATE_COMPLETION_THISYEAR;
    }

    if (backmonths < 0)
    {
        backmonths = 0;
    }
    else if (backmonths > 11)
    {
        backmonths = 11;
    }
    dateCompletionBackMonths = backmonths;

    return;
}

/*
 qof_date_format_get_string
 get the date format string for the current format
 returns: string

 Globals: dateFormat
*/
const gchar *qof_date_format_get_string(QofDateFormat df)
{
    switch (df)
    {
    case QOF_DATE_FORMAT_US:
        return "%m/%d/%y";
    case QOF_DATE_FORMAT_UK:
        return "%d/%m/%y";
    case QOF_DATE_FORMAT_CE:
        return "%d.%m.%y";
    case QOF_DATE_FORMAT_UTC:
        return "%Y-%m-%dT%H:%M:%SZ";
    case QOF_DATE_FORMAT_ISO:
        return "%Y-%m-%d";
    case QOF_DATE_FORMAT_LOCALE:
    default:
        break;
    };
    return GNC_D_FMT;
}

/* get the date format string for the current format

get the date format string for the current format

param df Required date format.
return string

Globals: dateFormat
*/
const gchar *qof_date_text_format_get_string(QofDateFormat df)
{
    switch (df)
    {
    case QOF_DATE_FORMAT_US:
        return "%b %d, %y";
    case QOF_DATE_FORMAT_UK:
    case QOF_DATE_FORMAT_CE:
        return "%d %b, %y";
    case QOF_DATE_FORMAT_UTC:
        return "%Y-%m-%dT%H:%M:%SZ";
    case QOF_DATE_FORMAT_ISO:
        return "%Y-%b-%d";
    case QOF_DATE_FORMAT_LOCALE:
    default:
        break;
    };
    return GNC_D_FMT;
}

/* Convert day, month and year values to a date string

  Convert a date as day / month / year integers into a localized string
  representation

param   buff - pointer to previously allocated character array; its size
         must be at lease MAX_DATE_LENTH bytes.
param   day - value to be set with the day of the month as 1 ... 31
param   month - value to be set with the month of the year as 1 ... 12
param   year - value to be set with the year (4-digit)

return length of string created in buff.

Globals: global dateFormat value
*/
size_t
qof_print_date_dmy_buff (char * buff, size_t len, int day, int month, int year)
{
    int flen;
    if (!buff) return 0;

    /* Note that when printing year, we use %-4d in format string;
     * this causes a one, two or three-digit year to be left-adjusted
     * when printed (i.e. padded with blanks on the right).  This is
     * important while the user is editing the year, since erasing a
     * digit can temporarily cause a three-digit year, and having the
     * blank on the left is a real pain for the user.  So pad on the
     * right.
     */
    switch (dateFormat)
    {
    case QOF_DATE_FORMAT_UK:
        flen = g_snprintf (buff, len, "%02d/%02d/%-4d", day, month, year);
        break;
    case QOF_DATE_FORMAT_CE:
        flen = g_snprintf (buff, len, "%02d.%02d.%-4d", day, month, year);
        break;
    case QOF_DATE_FORMAT_LOCALE:
    {
        struct tm tm_str;
        time64 t;

        tm_str.tm_mday = day;
        tm_str.tm_mon = month - 1;    /* tm_mon = 0 through 11 */
        tm_str.tm_year = year - 1900; /* this is what the standard
	 says, it's not a Y2K thing */

        gnc_tm_set_day_start (&tm_str);
        t = gnc_mktime (&tm_str);
        gnc_localtime_r (&t, &tm_str);
        flen = qof_strftime (buff, len, GNC_D_FMT, &tm_str);
        if (flen != 0)
            break;
    }
    /* FALL THROUGH */
    case QOF_DATE_FORMAT_ISO:
    case QOF_DATE_FORMAT_UTC:
        flen = g_snprintf (buff, len, "%04d-%02d-%02d", year, month, day);
        break;
    case QOF_DATE_FORMAT_US:
    default:
        flen = g_snprintf (buff, len, "%02d/%02d/%-4d", month, day, year);
        break;
    }

    return flen;
}

size_t
qof_print_date_buff (char * buff, size_t len, time64 t)
{
    struct tm theTime;
    time64 bt = t;
    size_t actual;
    if (!buff) return 0 ;
    if (!gnc_localtime_r(&bt, &theTime))
	return 0;

    actual = qof_print_date_dmy_buff (buff, len,
                                    theTime.tm_mday,
                                    theTime.tm_mon + 1,
                                    theTime.tm_year + 1900);
    return actual;
}

size_t
qof_print_gdate( char *buf, size_t len, const GDate *gd )
{
    return qof_print_date_dmy_buff( buf, len,
                                    g_date_get_day(gd),
                                    g_date_get_month(gd),
                                    g_date_get_year(gd) );
}

char *
qof_print_date (time64 t)
{
    char buff[MAX_DATE_LENGTH];
    memset (buff, 0, sizeof (buff));
    qof_print_date_buff (buff, MAX_DATE_LENGTH, t);
    return g_strdup (buff);
}

const char *
gnc_print_date (Timespec ts)
{
    static char buff[MAX_DATE_LENGTH];
    time64 t;

    memset (buff, 0, sizeof (buff));
    t = ts.tv_sec + (time64)(ts.tv_nsec / 1000000000.0);

    qof_print_date_buff (buff, MAX_DATE_LENGTH, t);

    return buff;
}

/* ============================================================== */

/* return the greatest integer <= a/b; works for b > 0 and positive or
   negative a. */
static int
floordiv(int a, int b)
{
    if (a >= 0)
    {
        return a / b;
    }
    else
    {
        return - ((-a - 1) / b) - 1;
    }
}

/* Convert a string into  day, month and year integers

    Convert a string into  day / month / year integers according to
    the current dateFormat value.

    This function will always parse a single number as the day of
    the month, regardless of the ordering of the dateFormat value.
    Two numbers will always be parsed as the day and the month, in
    the same order that they appear in the dateFormat value.  Three
    numbers are parsed exactly as specified in the dateFormat field.

    Fully formatted UTC timestamp strings are converted separately.

param   buff - pointer to date string
param     day -  will store day of the month as 1 ... 31
param     month - will store month of the year as 1 ... 12
param     year - will store the year (4-digit)

return TRUE if date appeared to be valid.

 Globals: global dateFormat value
*/
static gboolean
qof_scan_date_internal (const char *buff, int *day, int *month, int *year,
                        QofDateFormat which_format)
{
    char *dupe, *tmp, *first_field, *second_field, *third_field;
    int iday, imonth, iyear;
    int now_day, now_month, now_year;
    struct tm *now, utc;
    time64 secs;

    if (!buff) return(FALSE);

    if (which_format == QOF_DATE_FORMAT_UTC)
    {
        if (strptime(buff, QOF_UTC_DATE_FORMAT, &utc)
	    || strptime (buff, "%Y-%m-%d", &utc))
        {
            *day = utc.tm_mday;
            *month = utc.tm_mon + 1;
            *year = utc.tm_year + 1900;
            return TRUE;
        }
        else
        {
            return FALSE;
        }
    }
    dupe = g_strdup (buff);

    tmp = dupe;
    first_field = NULL;
    second_field = NULL;
    third_field = NULL;

    /* Use strtok to find delimiters */
    if (tmp)
    {
        static char *delims = ".,-+/\\()년월年月 ";

        first_field = strtok (tmp, delims);
        if (first_field)
        {
            second_field = strtok (NULL, delims);
            if (second_field)
            {
                third_field = strtok (NULL, delims);
            }
        }
    }

    /* today's date */
    gnc_time (&secs);
    now = gnc_localtime (&secs);
    now_day = now->tm_mday;
    now_month = now->tm_mon + 1;
    now_year = now->tm_year + 1900;
    gnc_tm_free (now);

    /* set defaults: if day or month appear to be blank, use today's date */
    iday = now_day;
    imonth = now_month;
    iyear = -1;

    /* get numeric values */
    switch (which_format)
    {
    case QOF_DATE_FORMAT_LOCALE:
        if (buff[0] != '\0')
        {
            struct tm thetime;

            /* Parse time string. */
            memset(&thetime, -1, sizeof(struct tm));
            strptime (buff, GNC_D_FMT, &thetime);

            if (third_field)
            {
                /* Easy.  All three values were parsed. */
                iyear = thetime.tm_year + 1900;
                iday = thetime.tm_mday;
                imonth = thetime.tm_mon + 1;
            }
            else if (second_field)
            {
                /* Hard. Two values parsed.  Figure out the ordering. */
                if (thetime.tm_year == -1)
                {
                    /* %m-%d or %d-%m. Don't care. Already parsed correctly. */
                    iday = thetime.tm_mday;
                    imonth = thetime.tm_mon + 1;
                }
                else if (thetime.tm_mon != -1)
                {
                    /* Must be %Y-%m-%d. Reparse as %m-%d.*/
                    imonth = atoi(first_field);
                    iday = atoi(second_field);
                }
                else
                {
                    /* Must be %Y-%d-%m. Reparse as %d-%m. */
                    iday = atoi(first_field);
                    imonth = atoi(second_field);
                }
            }
            else if (first_field)
            {
                iday = atoi(first_field);
            }
        }
        break;
    case QOF_DATE_FORMAT_UK:
    case QOF_DATE_FORMAT_CE:
        if (third_field)
        {
            iday = atoi(first_field);
            imonth = atoi(second_field);
            iyear = atoi(third_field);
        }
        else if (second_field)
        {
            iday = atoi(first_field);
            imonth = atoi(second_field);
        }
        else if (first_field)
        {
            iday = atoi(first_field);
        }
        break;
    case QOF_DATE_FORMAT_ISO:
        if (third_field)
        {
            iyear = atoi(first_field);
            imonth = atoi(second_field);
            iday = atoi(third_field);
        }
        else if (second_field)
        {
            imonth = atoi(first_field);
            iday = atoi(second_field);
        }
        else if (first_field)
        {
            iday = atoi(first_field);
        }
        break;
    case QOF_DATE_FORMAT_US:
    default:
        if (third_field)
        {
            imonth = atoi(first_field);
            iday = atoi(second_field);
            iyear = atoi(third_field);
        }
        else if (second_field)
        {
            imonth = atoi(first_field);
            iday = atoi(second_field);
        }
        else if (first_field)
        {
            iday = atoi(first_field);
        }
        break;
    }

    g_free (dupe);

    if ((12 < imonth) || (31 < iday))
    {
        /*
         * Ack! Thppfft!  Someone just fed this routine a string in the
         * wrong date format.  This is known to happen if a register
         * window is open when changing the date format.  Try the
         * previous date format.  If that doesn't work, see if we can
         * exchange month and day. If that still doesn't work,
         * bail and give the caller what they asked for (garbage)
         * parsed in the new format.
         *
         * Note: This test cannot detect any format change that only
         * swaps month and day field, if the day is 12 or less.  This is
         * deemed acceptable given the obscurity of this bug.
         */
        if ((which_format != prevQofDateFormat) &&
                qof_scan_date_internal(buff, day, month, year, prevQofDateFormat))
        {
            return(TRUE);
        }
        if ((12 < imonth) && (12 >= iday))
        {
            int tmp = imonth;
            imonth = iday;
            iday = tmp;
        }
        else
        {
            return FALSE;
        }
    }

    /* if no year was entered, choose a year according to the
       dateCompletion preference. If it is
       QOF_DATE_COMPLETION_THISYEAR, use the current year, else if it
       is QOF_DATE_COMPLETION_SLIDING, use a sliding window that
       starts dateCompletionBackMonths before the current month.

       We go by whole months, rather than days, because presumably
       this is less confusing.
    */

    if (iyear == -1)
    {
        if (dateCompletion == QOF_DATE_COMPLETION_THISYEAR)
        {
            iyear = now_year;  /* use the current year */
        }
        else
        {
            iyear = now_year - floordiv(imonth - now_month +
                                        dateCompletionBackMonths, 12);
        }
    }

    /* If the year entered is smaller than 100, assume we mean the current
       century (and are not revising some roman emperor's books) */
    if (iyear < 100)
        iyear += ((int) ((now_year + 50 - iyear) / 100)) * 100;

    if (year) *year = iyear;
    if (month) *month = imonth;
    if (day) *day = iday;
    return(TRUE);
}

gboolean
qof_scan_date (const char *buff, int *day, int *month, int *year)
{
    return qof_scan_date_internal(buff, day, month, year, dateFormat);
}

/* Return the field separator for the current date format
return date character
*/
char dateSeparator (void)
{
    static char locale_separator = '\0';

    switch (dateFormat)
    {
    case QOF_DATE_FORMAT_CE:
        return '.';
    case QOF_DATE_FORMAT_ISO:
    case QOF_DATE_FORMAT_UTC:
        return '-';
    case QOF_DATE_FORMAT_US:
    case QOF_DATE_FORMAT_UK:
    default:
        return '/';
    case QOF_DATE_FORMAT_LOCALE:
        if (locale_separator != '\0')
            return locale_separator;
        else
        {
            /* Make a guess */
            gchar string[256];
            struct tm tm;
            time64 secs;
            gchar *s;

            secs = gnc_time (NULL);
            gnc_localtime_r(&secs, &tm);
            qof_strftime(string, sizeof(string), GNC_D_FMT, &tm);

            for (s = string; s != '\0'; s++)
                if (!isdigit(*s))
                    return (locale_separator = *s);
        }
        break;
    }

    return '\0';
}

/* The following functions have Win32 forms in qof-win32.c */
#ifndef G_OS_WIN32
gchar *
qof_time_format_from_utf8(const gchar *utf8_format)
{
    gchar *retval;
    GError *error = NULL;

    retval = g_locale_from_utf8(utf8_format, -1, NULL, NULL, &error);

    if (!retval)
    {
        g_warning("Could not convert format '%s' from UTF-8: %s", utf8_format,
                  error->message);
        g_error_free(error);
    }
    return retval;
}

gchar *
qof_formatted_time_to_utf8(const gchar *locale_string)
{
    gchar *retval;
    GError *error = NULL;

    retval = g_locale_to_utf8(locale_string, -1, NULL, NULL, &error);

    if (!retval)
    {
        g_warning("Could not convert '%s' to UTF-8: %s", locale_string,
                  error->message);
        g_error_free(error);
    }
    return retval;
}
#endif /* G_OS_WIN32 */

static gchar *
qof_format_time(const gchar *format, const struct tm *tm)
{
    gchar *locale_format, *tmpbuf, *retval;
    gsize tmplen, tmpbufsize;

    g_return_val_if_fail(format, 0);
    g_return_val_if_fail(tm, 0);

    locale_format = qof_time_format_from_utf8(format);
    if (!locale_format)
        return NULL;

    tmpbufsize = MAX(128, strlen(locale_format) * 2);
    while (TRUE)
    {
        tmpbuf = g_malloc(tmpbufsize);

        /* Set the first byte to something other than '\0', to be able to
         * recognize whether strftime actually failed or just returned "".
         */
        tmpbuf[0] = '\1';
        tmplen = strftime(tmpbuf, tmpbufsize, locale_format, tm);

        if (tmplen == 0 && tmpbuf[0] != '\0')
        {
            g_free(tmpbuf);
            tmpbufsize *= 2;

            if (tmpbufsize > 65536)
            {
                g_warning("Maximum buffer size for qof_format_time "
                          "exceeded: giving up");
                g_free(locale_format);

                return NULL;
            }
        }
        else
        {
            break;
        }
    }
    g_free(locale_format);

    retval = qof_formatted_time_to_utf8(tmpbuf);
    g_free(tmpbuf);

    return retval;
}

gsize
qof_strftime(gchar *buf, gsize max, const gchar *format, const struct tm *tm)
{
    gsize convlen, retval;
    gchar *convbuf;

    g_return_val_if_fail(buf, 0);
    g_return_val_if_fail(max > 0, 0);
    g_return_val_if_fail(format, 0);
    g_return_val_if_fail(tm, 0);

    convbuf = qof_format_time(format, tm);
    if (!convbuf)
    {
        buf[0] = '\0';
        return 0;
    }

    convlen = strlen(convbuf);

    if (max <= convlen)
    {
        /* Ensure only whole characters are copied into the buffer. */
        gchar *end = g_utf8_find_prev_char(convbuf, convbuf + max);
        g_assert(end != NULL);
        convlen = end - convbuf;

        /* Return 0 because the buffer isn't large enough. */
        retval = 0;
    }
    else
    {
        retval = convlen;
    }

    memcpy(buf, convbuf, convlen);
    buf[convlen] = '\0';
    g_free(convbuf);

    return retval;
}


/********************************************************************\
\********************************************************************/

gchar *
gnc_date_timestamp (void)
{
    GDateTime *gdt = gnc_g_date_time_new_now_local ();
    gchar *timestr = g_date_time_format (gdt, "%Y%m%d%H%M%S");
    g_date_time_unref (gdt);
    return timestr;
}

/********************************************************************\
 * iso 8601 datetimes should look like 1998-07-02 11:00:00.68-05
\********************************************************************/
/* Unfortunately, not all strptime or struct tm implementations
 * support timezones, so we have to do this with sscanf.
 */

#define ISO_DATE_FORMAT "%d-%d-%d %d:%d:%lf%s"
Timespec
gnc_iso8601_to_timespec_gmt(const char *str)
{
    Timespec time = { 0L, 0L };
    GDateTime *gdt;
    gint hour = 0, minute = 0, day = 0, month = 0, year = 0;
    gchar zone[12];
    gdouble second = 0.0;
    gint fields;

    memset (zone, 0, sizeof (zone));

    if (!str)
	return time;

    fields = sscanf (str, ISO_DATE_FORMAT, &year, &month,
			  &day, &hour, &minute, &second, zone);
    if (fields < 1)
	return time;
    else if (fields > 6 && strlen (zone) > 0) /* Date string included a timezone */
    {
	GTimeZone *tz = g_time_zone_new (zone);
        time64 secs;
	second += 5e-10;
	gdt = g_date_time_new (tz, year, month, day, hour, minute, second);
        secs = g_date_time_to_unix (gdt);
	g_time_zone_unref (tz);
    }
    else /* No zone info, assume UTC */
    {
	second += 5e-10;
	gdt = g_date_time_new_utc (year, month, day, hour, minute, second);
    }

    time.tv_sec = g_date_time_to_unix (gdt);
    time.tv_nsec = g_date_time_get_microsecond (gdt) * 1000;
    g_date_time_unref (gdt);
    return time;
}

/********************************************************************\
\********************************************************************/

char *
gnc_timespec_to_iso8601_buff (Timespec ts, char * buff)
{
    gchar *fmt1 = "%Y-%m-%d %H:%M", *fmt2 = "%s:%02d.%06d %s";
    GDateTime *gdt;
    gchar *time_base, *tz;

    g_return_val_if_fail (buff != NULL, NULL);
    gdt = gnc_g_date_time_new_from_timespec_local (ts);
    g_return_val_if_fail (gdt != NULL, NULL);
    time_base = g_date_time_format (gdt, fmt1);
#ifdef G_OS_WIN32
    tz = g_date_time_format (gdt, "%Z");
#else
    tz = g_date_time_format (gdt, "%z");
#endif
    snprintf (buff, MAX_DATE_LENGTH, fmt2, time_base,
	      g_date_time_get_second (gdt), g_date_time_get_microsecond (gdt),
	      tz);

    g_free (time_base);
    g_free (tz);
    g_date_time_unref (gdt);
    return buff + strlen (buff);

}

void
gnc_timespec2dmy (Timespec t, int *day, int *month, int *year)
{
    struct tm result;
    time64 t_secs = t.tv_sec + (t.tv_nsec / NANOS_PER_SECOND);
    gnc_localtime_r(&t_secs, &result);

    if (day) *day = result.tm_mday;
    if (month) *month = result.tm_mon + 1;
    if (year) *year = result.tm_year + 1900;
}

#define THIRTY_TWO_YEARS 0x3c30fc00LL

static Timespec
gnc_dmy2timespec_internal (int day, int month, int year, gboolean start_of_day)
{
    Timespec result;
    struct tm date;
    long long secs = 0;
    long long era = 0;

    date.tm_year = year - 1900;
    date.tm_mon = month - 1;
    date.tm_mday = day;

    if (start_of_day)
        gnc_tm_set_day_start(&date);
    else
        gnc_tm_set_day_end(&date);

    /* compute number of seconds */
    secs = gnc_mktime (&date);

    result.tv_sec = secs;
    result.tv_nsec = 0;

    return result;
}

Timespec
gnc_dmy2timespec (int day, int month, int year)
{
    return gnc_dmy2timespec_internal (day, month, year, TRUE);
}

Timespec
gnc_dmy2timespec_end (int day, int month, int year)
{
    return gnc_dmy2timespec_internal (day, month, year, FALSE);
}

/********************************************************************\
\********************************************************************/

long int
gnc_timezone (const struct tm *tm)
{
    g_return_val_if_fail (tm != NULL, 0);

#ifdef HAVE_STRUCT_TM_GMTOFF
    /* tm_gmtoff is seconds *east* of UTC and is
     * already adjusted for daylight savings time. */
    return -(tm->tm_gmtoff);
#else
    {
        long tz_seconds;
        /* timezone is seconds *west* of UTC and is
         * not adjusted for daylight savings time.
         * In Spring, we spring forward, wheee! */
# if COMPILER(MSVC)
        _get_timezone(&tz_seconds);
# else
        tz_seconds = timezone;
# endif
        return (long int)(tz_seconds - (tm->tm_isdst > 0 ? 3600 : 0));
    }
#endif
}


void
timespecFromTime64 ( Timespec *ts, time64 t )
{
    ts->tv_sec = t;
    ts->tv_nsec = 0;
}

Timespec
timespec_now()
{
    Timespec ts;
    ts.tv_sec = gnc_time(NULL);
    ts.tv_nsec = 0;
    return ts;
}

time64
timespecToTime64 (Timespec ts)
{
    return ts.tv_sec;
}

/* The GDate setter functions all in the end use g_date_set_time_t,
 * which in turn relies on localtime and is therefore subject to the
 * 2038 bug.
 */
GDate timespec_to_gdate (Timespec ts)
{
    GDate result;
    gint day, month, year;

    g_date_clear (&result, 1);
    gnc_timespec2dmy (ts, &day, &month, &year);
    g_date_set_dmy (&result, day, month, year);
    g_assert(g_date_valid (&result));

    return result;
}

GDate* gnc_g_date_new_today ()
{
     GDateTime *gdt = gnc_g_date_time_new_now_local ();
     gint day, month, year;
     GDate *result;

     g_date_time_get_ymd (gdt, &year, &month, &day);
     result = g_date_new_dmy (day, month, year);
     g_date_time_unref (gdt);
     g_assert(g_date_valid (result));

     return result;
}

Timespec gdate_to_timespec (GDate d)
{
    return gnc_dmy2timespec(g_date_get_day(&d),
                            g_date_get_month(&d),
                            g_date_get_year(&d));
}

static void
gnc_tm_get_day_start (struct tm *tm, time64 time_val)
{
    /* Get the equivalent time structure */
    if (!gnc_localtime_r(&time_val, tm))
	return;
    gnc_tm_set_day_start(tm);
}

static void
gnc_tm_get_day_end (struct tm *tm, time64 time_val)
{
    /* Get the equivalent time structure */
    if (!gnc_localtime_r(&time_val, tm))
	return;
    gnc_tm_set_day_end(tm);
}

time64
gnc_time64_get_day_start (time64 time_val)
{
    struct tm tm;
    time64 new_time;

    gnc_tm_get_day_start(&tm, time_val);
    new_time = gnc_mktime(&tm);
    return new_time;
}

time64
gnc_time64_get_day_end (time64 time_val)
{
    struct tm tm;
    time64 new_time;

    gnc_tm_get_day_end(&tm, time_val);
    new_time = gnc_mktime(&tm);
    return new_time;
}


/* ======================================================== */

void
gnc_tm_get_today_start (struct tm *tm)
{
    gnc_tm_get_day_start(tm, time(NULL));
}

void
gnc_tm_get_today_end (struct tm *tm)
{
    gnc_tm_get_day_end(tm, time(NULL));
}

time64
gnc_time64_get_today_start (void)
{
    struct tm tm;

    gnc_tm_get_day_start(&tm, time(NULL));
    return gnc_mktime(&tm);
}

time64
gnc_time64_get_today_end (void)
{
    struct tm tm;

    gnc_tm_get_day_end(&tm, time(NULL));
    return gnc_mktime(&tm);
}

void
gnc_dow_abbrev(gchar *buf, int buf_len, int dow)
{
    struct tm my_tm;
    int i;

    memset(buf, 0, buf_len);
    memset(&my_tm, 0, sizeof(struct tm));
    my_tm.tm_wday = dow;
    i = qof_strftime(buf, buf_len, "%a", &my_tm);
    buf[i] = 0;
}

/* *******************************************************************
 *  GValue handling
 ********************************************************************/
static gpointer
timespec_boxed_copy_func( gpointer in_timespec )
{
    Timespec* newvalue;

    newvalue = g_malloc( sizeof( Timespec ) );
    memcpy( newvalue, in_timespec, sizeof( Timespec ) );

    return newvalue;
}

static void
timespec_boxed_free_func( gpointer in_timespec )
{
    g_free( in_timespec );
}

GType
timespec_get_type( void )
{
    static GType type = 0;

    if ( type == 0 )
    {
        type = g_boxed_type_register_static( "timespec",
                                             timespec_boxed_copy_func,
                                             timespec_boxed_free_func );
    }

    return type;
}

Testfuncs*
gnc_date_load_funcs (void)
{
    Testfuncs *tf = g_slice_new (Testfuncs);
    tf->timespec_normalize = timespec_normalize;
    return tf;
}
