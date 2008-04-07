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

#include "gnc-date-p.h"
#include "qof.h"

#ifndef HAVE_STRPTIME
#include "strptime.h"
#endif
#ifndef HAVE_LOCALTIME_R
#include "localtime_r.h"
#endif

#define NANOS_PER_SECOND 1000000000

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

/* This is now user configured through the gnome options system() */
static QofDateFormat dateFormat = QOF_DATE_FORMAT_LOCALE;
static QofDateFormat prevQofDateFormat = QOF_DATE_FORMAT_LOCALE;

/* This static indicates the debugging module that this .o belongs to. */
static QofLogModule log_module = QOF_MOD_ENGINE;

/********************************************************************\
\********************************************************************/

const char*
gnc_date_dateformat_to_string(QofDateFormat format)
{
  switch (format) {
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
  switch (format) {
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
  if(t->tv_nsec > NANOS_PER_SECOND)
  {
    t->tv_sec+= (t->tv_nsec / NANOS_PER_SECOND);
    t->tv_nsec= t->tv_nsec % NANOS_PER_SECOND;
  }

  if(t->tv_nsec < - NANOS_PER_SECOND)
  {
    t->tv_sec+= - (-t->tv_nsec / NANOS_PER_SECOND);
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
  if(ta == tb) return TRUE;
  if(ta->tv_sec != tb->tv_sec) return FALSE;
  if(ta->tv_nsec != tb->tv_nsec) return FALSE;
  return TRUE;
}

gint
timespec_cmp(const Timespec *ta, const Timespec *tb)
{
  if(ta == tb) return 0;
  if(ta->tv_sec < tb->tv_sec) return -1;
  if(ta->tv_sec > tb->tv_sec) return 1;
  if(ta->tv_nsec < tb->tv_nsec) return -1;
  if(ta->tv_nsec > tb->tv_nsec) return 1;
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
  time_t t_secs = t.tv_sec + (t.tv_nsec / NANOS_PER_SECOND);
  localtime_r(&t_secs, &tm);
  gnc_tm_set_day_middle(&tm);
  retval.tv_sec = mktime(&tm);
  retval.tv_nsec = 0;
  return retval;
}

int gnc_date_my_last_mday (int month, int year)
{
  static int last_day_of_month[2][12] = {
  /* non leap */ {31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31},
  /*   leap   */ {31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31}
  };

  /* Is this a leap year? */
  if (year % 2000 == 0) return last_day_of_month[1][month-1];
  if (year % 400 == 0 ) return last_day_of_month[0][month-1];
  if (year % 4   == 0 ) return last_day_of_month[1][month-1];
  return last_day_of_month[0][month-1];
}

/* Retrieve the last numerical day of the month

 Retrieve the last numerical day for the month specified in the
 tm_year and tm_mon fields.

param  tm: the time value in question
return the last day of the month, integer.
*/
int date_get_last_mday(const struct tm *tm)
{
  return gnc_date_my_last_mday (tm->tm_mon+1, tm->tm_year+1900);
}

/* Determines if tm_mday is the last day of the month.

 Determines whether the tm_mday field contains the last day of the
 month as specified in the tm_year and tm_mon fields.
param  tm: the time value in question
return TRUE if tm_mday matches the last day of the month, else FALSE.
*/
gboolean date_is_last_mday(const struct tm *tm)
{
  return(tm->tm_mday == date_get_last_mday(tm));
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
  if(df >= DATE_FORMAT_FIRST && df <= DATE_FORMAT_LAST)
  {
    prevQofDateFormat = dateFormat;
    dateFormat = df;
  }
  else
  {    /* hack alert - Use a neutral default. */
    PERR("non-existent date format set attempted. Setting ISO default");
    prevQofDateFormat = dateFormat;
    dateFormat = QOF_DATE_FORMAT_ISO;
  }

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
  switch(df) {
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
    return GNC_D_FMT;
  };
}

/* get the date format string for the current format

get the date format string for the current format

param df Required date format.
return string

Globals: dateFormat
*/
const gchar *qof_date_text_format_get_string(QofDateFormat df)
{
  switch(df) {
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
    return GNC_D_FMT;
  };
}

/* Convert day, month and year values to a date string

  Convert a date as day / month / year integers into a localized string
  representation

param   buff - pointer to previously allocated character array; its size
         must be at lease MAX_DATE_LENTH bytes.
param   day - value to be set with the day of the month as 1 ... 31
param   month - value to be set with the month of the year as 1 ... 12
param   year - value to be set with the year (4-digit)

return void

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
  switch(dateFormat)
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
	time_t t;

        tm_str.tm_mday = day;
        tm_str.tm_mon = month - 1;    /* tm_mon = 0 through 11 */
        tm_str.tm_year = year - 1900; /* this is what the standard 
                                       * says, it's not a Y2K thing */

        gnc_tm_set_day_start (&tm_str);
	t = mktime (&tm_str);
	localtime_r (&t, &tm_str);
        flen = qof_strftime (buff, len, GNC_D_FMT, &tm_str);
       if (flen != 0)
         break;
      }
      /* FALLTHROUGH */
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
qof_print_date_buff (char * buff, size_t len, time_t t)
{
  struct tm theTime;

  if (!buff) return 0 ;

  localtime_r(&t, &theTime);

  return qof_print_date_dmy_buff (buff, len,
                   theTime.tm_mday, 
                   theTime.tm_mon + 1,
                   theTime.tm_year + 1900);
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
qof_print_date (time_t t)
{
   char buff[MAX_DATE_LENGTH];
   qof_print_date_buff (buff, MAX_DATE_LENGTH, t);
   return g_strdup (buff);
}

const char *
gnc_print_date (Timespec ts)
{
  static char buff[MAX_DATE_LENGTH];
  time_t t;

  t = ts.tv_sec + (ts.tv_nsec / 1000000000.0);

  qof_print_date_buff (buff, MAX_DATE_LENGTH, t);

  return buff;
}

/* ============================================================== */

size_t
qof_print_date_time_buff (char * buff, size_t len, time_t secs)
{
  int flen;
  int day, month, year, hour, min, sec;
  struct tm ltm, gtm;
  
  if (!buff) return 0;

  /* Note that when printing year, we use %-4d in format string;
   * this causes a one, two or three-digit year to be left-adjusted
   * when printed (i.e. padded with blanks on the right).  This is 
   * important while the user is editing the year, since erasing a 
   * digit can temporarily cause a three-digit year, and having the 
   * blank on the left is a real pain for the user.  So pad on the 
   * right.
   */
  ltm = *localtime (&secs);
  day = ltm.tm_mday;
  month = ltm.tm_mon +1;
  year = ltm.tm_year +1900;
  hour = ltm.tm_hour;
  min = ltm.tm_min;
  sec = ltm.tm_sec;
  
  switch(dateFormat)
  {
    case QOF_DATE_FORMAT_UK:
      flen = g_snprintf (buff, len, "%02d/%02d/%-4d %2d:%02d", day, month, year, hour, min);
      break;
    case QOF_DATE_FORMAT_CE:
      flen = g_snprintf (buff, len, "%02d.%02d.%-4d %2d:%02d", day, month, year, hour, min);
      break;
    case QOF_DATE_FORMAT_ISO:
      flen = g_snprintf (buff, len, "%04d-%02d-%02d %02d:%02d", year, month, day, hour, min);
      break;
	case QOF_DATE_FORMAT_UTC:
	{
		gtm = *gmtime (&secs);
		flen = qof_strftime (buff, len, QOF_UTC_DATE_FORMAT, &gtm);
		break;
	}
    case QOF_DATE_FORMAT_LOCALE:
      {
        flen = qof_strftime (buff, len, GNC_D_T_FMT, &ltm);
      }
      break;

    case QOF_DATE_FORMAT_US:
    default:
      flen = g_snprintf (buff, len, "%02d/%02d/%-4d %2d:%02d", month, day, year, hour, min);
      break;
  }
  return flen;
}

size_t 
qof_print_time_buff (char * buff, size_t len, time_t secs)
{
	int flen;
	struct tm ltm, gtm;
	
	if (!buff) return 0;
	if(dateFormat == QOF_DATE_FORMAT_UTC)
	{
		gtm = *gmtime (&secs);
		flen = qof_strftime(buff, len, QOF_UTC_DATE_FORMAT, &gtm);
		return flen;
	}
	ltm = *localtime (&secs);
	flen = qof_strftime (buff, len, GNC_T_FMT, &ltm);
	
	return flen;
}

/* ============================================================== */

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
   struct tm *now, utc;
   time_t secs;

   if (!buff) return(FALSE);

	if(which_format == QOF_DATE_FORMAT_UTC)
	{
		if(strptime(buff, QOF_UTC_DATE_FORMAT, &utc)) {
			*day = utc.tm_mday;
			*month = utc.tm_mon + 1;
			*year = utc.tm_year + 1900;
			return TRUE;
		}
		else { return FALSE; }
	}
   dupe = g_strdup (buff);

   tmp = dupe;
   first_field = NULL;
   second_field = NULL;
   third_field = NULL;

   /* Use strtok to find delimiters */
   if (tmp) {
     static char *delims = ".,-+/\\()년월年月 ";

      first_field = strtok (tmp, delims);
      if (first_field) {
         second_field = strtok (NULL, delims);
         if (second_field) {
            third_field = strtok (NULL, delims);
         }
      }
   }

   /* If any fields appear to be blank, use today's date */
   time (&secs);
   now = localtime (&secs);
   iday = now->tm_mday; 
   imonth = now->tm_mon+1;
   iyear = now->tm_year+1900;

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

         if (third_field) {
           /* Easy.  All three values were parsed. */
           iyear = thetime.tm_year + 1900;
           iday = thetime.tm_mday;
           imonth = thetime.tm_mon + 1;
         } else if (second_field) {
           /* Hard. Two values parsed.  Figure out the ordering. */
           if (thetime.tm_year == -1) {
             /* %m-%d or %d-%m. Don't care. Already parsed correctly. */
             iday = thetime.tm_mday;
             imonth = thetime.tm_mon + 1;
           } else if (thetime.tm_mon != -1) {
             /* Must be %Y-%m-%d. Reparse as %m-%d.*/
             imonth = atoi(first_field);
             iday = atoi(second_field);
           } else {
             /* Must be %Y-%d-%m. Reparse as %d-%m. */
             iday = atoi(first_field);
             imonth = atoi(second_field);
           }
         } else if (first_field) {
           iday = atoi(first_field);
         }
       }
       break;
     case QOF_DATE_FORMAT_UK:
     case QOF_DATE_FORMAT_CE:
       if (third_field) {
         iday = atoi(first_field);
         imonth = atoi(second_field);
         iyear = atoi(third_field);
       } else if (second_field) {
         iday = atoi(first_field);
         imonth = atoi(second_field);
       } else if (first_field) {
         iday = atoi(first_field);
       }
       break;
     case QOF_DATE_FORMAT_ISO:
       if (third_field) {
         iyear = atoi(first_field);
         imonth = atoi(second_field);
         iday = atoi(third_field);
       } else if (second_field) {
         imonth = atoi(first_field);
         iday = atoi(second_field);
       } else if (first_field) {
         iday = atoi(first_field);
       }
       break;
    case QOF_DATE_FORMAT_US:
    default:
       if (third_field) {
         imonth = atoi(first_field);
         iday = atoi(second_field);
         iyear = atoi(third_field);
       } else if (second_field) {
         imonth = atoi(first_field);
         iday = atoi(second_field);
       } else if (first_field) {
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
        int tmp = imonth; imonth = iday; iday = tmp;
     } 
	  else
	  {
        return FALSE;
	  }
   }

   /* If the year entered is smaller than 100, assume we mean the current
      century (and are not revising some roman emperor's books) */
   if (iyear < 100)
     iyear += ((int) ((now->tm_year+1950-iyear)/100)) * 100;

   if (year) *year=iyear;
   if (month) *month=imonth;
   if (day) *day=iday;
   return(TRUE);
}

gboolean
qof_scan_date (const char *buff, int *day, int *month, int *year)
{
  return qof_scan_date_internal(buff, day, month, year, dateFormat);
}

gboolean
qof_scan_date_secs (const char *buff, time_t *secs)
{
  gboolean rc;
  int day, month, year;
  
  rc = qof_scan_date_internal(buff, &day, &month, &year, dateFormat);
  if (secs) *secs = xaccDMYToSec (day, month, year);

  return rc;
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
      { /* Make a guess */
        unsigned char string[256];
        struct tm tm;
        time_t secs;
        unsigned char *s;

        secs = time(NULL);
        localtime_r(&secs, &tm);
        qof_strftime(string, sizeof(string), GNC_D_FMT, &tm);

        for (s = string; s != '\0'; s++)
          if (!isdigit(*s))
            return (locale_separator = *s);
      }
  }

  return '\0';
}


#ifndef G_OS_WIN32
gchar *
qof_time_format_from_utf8(const gchar *utf8_format)
{
    gchar *retval;
    GError *error = NULL;

    retval = g_locale_from_utf8(utf8_format, -1, NULL, NULL, &error);

    if (!retval) {
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

    if (!retval) {
        g_warning("Could not convert '%s' to UTF-8: %s", locale_string,
                  error->message);
        g_error_free(error);
    }
    return retval;
}
#endif /* G_OS_WIN32 */

gchar *
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
    while (TRUE) {
        tmpbuf = g_malloc(tmpbufsize);

        /* Set the first byte to something other than '\0', to be able to
         * recognize whether strftime actually failed or just returned "".
         */
        tmpbuf[0] = '\1';
        tmplen = strftime(tmpbuf, tmpbufsize, locale_format, tm);

        if (tmplen == 0 && tmpbuf[0] != '\0') {
            g_free(tmpbuf);
            tmpbufsize *= 2;

            if (tmpbufsize > 65536) {
                g_warning("Maximum buffer size for qof_format_time "
                          "exceeded: giving up");
                g_free(locale_format);

                return NULL;
            }
        } else {
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
    if (!convbuf) {
        buf[0] = '\0';
        return 0;
    }

    convlen = strlen(convbuf);

    if (max <= convlen) {
        /* Ensure only whole characters are copied into the buffer. */
        gchar *end = g_utf8_find_prev_char(convbuf, convbuf + max);
        g_assert(end != NULL);
        convlen = end - convbuf;

        /* Return 0 because the buffer isn't large enough. */
        retval = 0;
    } else {
        retval = convlen;
    }

    memcpy(buf, convbuf, convlen);
    buf[convlen] = '\0';
    g_free(convbuf);

    return retval;
}


/********************************************************************\
\********************************************************************/
                                                                                
/* Convert time in seconds to a textual.

The xaccDateUtilGetStamp() routine will take the given time in
seconds and return a buffer containing a textual for the date.

param thyme The time in seconds to convert.
return A pointer to the generated string.
The caller owns this buffer and must free it when done.
*/
char *
xaccDateUtilGetStamp (time_t thyme)
{
   struct tm *stm;
                                                                                
   stm = localtime (&thyme);
                                                                                
   return g_strdup_printf("%04d%02d%02d%02d%02d%02d",
      (stm->tm_year + 1900),
      (stm->tm_mon +1),
      stm->tm_mday,
      stm->tm_hour,
      stm->tm_min,
      stm->tm_sec
   );
}
                                                                                
                                                                                
/* Convert textual to time in seconds.

The xaccDateUtilGetStampNow() routine returns the current time in
seconds in textual format.

return A pointer to the generated string.

note The caller owns this buffer and must free it when done.
*/
char *
xaccDateUtilGetStampNow (void)
{
   time_t now;
   time (&now);
   return xaccDateUtilGetStamp (now);
}

/********************************************************************\
 * iso 8601 datetimes should look like 1998-07-02 11:00:00.68-05
\********************************************************************/
/* hack alert -- this routine returns incorrect values for 
 * dates before 1970 */

Timespec
gnc_iso8601_to_timespec_gmt(const char *str)
{
  char buf[4];
  gchar *dupe;
  Timespec ts;
  struct tm stm;
  long int nsec =0;

  ts.tv_sec=0;
  ts.tv_nsec=0;
  if (!str) return ts;
  dupe = g_strdup(str);
  stm.tm_year = atoi(str) - 1900;
  str = strchr (str, '-'); if (str) { str++; } else { return ts; }
  stm.tm_mon = atoi(str) - 1;
  str = strchr (str, '-'); if (str) { str++; } else { return ts; }
  stm.tm_mday = atoi(str);

  str = strchr (str, ' '); if (str) { str++; } else { return ts; }
  stm.tm_hour = atoi(str);
  str = strchr (str, ':'); if (str) { str++; } else { return ts; }
  stm.tm_min = atoi(str);
  str = strchr (str, ':'); if (str) { str++; } else { return ts; }
  stm.tm_sec = atoi (str);

  /* The decimal point, optionally present ... */
  /* hack alert -- this algo breaks if more than 9 decimal places present */
  if (strchr (str, '.')) 
  { 
     int decimals, i, multiplier=1000000000;
     str = strchr (str, '.') +1;
     decimals = strcspn (str, "+- ");
     for (i=0; i<decimals; i++) multiplier /= 10;
     nsec = atoi(str) * multiplier;
  }
  stm.tm_isdst = -1;

  /* Timezone format can be +hh or +hhmm or +hh.mm (or -) (or not present) */
  str += strcspn (str, "+-");
  if (*str)
  {
    buf[0] = str[0];
    buf[1] = str[1];
    buf[2] = str[2];
    buf[3] = 0;
    stm.tm_hour -= atoi(buf);

    str +=3;
    if ('.' == *str) str++;
    if (isdigit ((unsigned char)*str) && isdigit ((unsigned char)*(str+1)))
    {
      int cyn;
      /* copy sign from hour part */
      if ('+' == buf[0]) { cyn = -1; } else { cyn = +1; } 
      buf[0] = str[0];
      buf[1] = str[1];
      buf[2] = str[2];
      buf[3] = 0;
      stm.tm_min += cyn * atoi(buf);
    }
  }

  /* Note that mktime returns 'local seconds' which is the true time
   * minus the timezone offset.  We don't want to work with local 
   * seconds, since they swim around acording to daylight savings, etc. 
   * We want to work with universal time.  Thus, add an offset
   * to undo the damage that mktime causes.
   */
 {
    struct tm tmp_tm;
    struct tm tm;
    long int tz;
    int tz_hour;
    time_t secs;

    /* Use a temporary tm struct so the mktime call below
     * doesn't mess up stm. */
    tmp_tm = stm;
    tmp_tm.tm_isdst = -1;

    secs = mktime (&tmp_tm);

    if(secs < 0) 
    {
    /* Workaround buggy mktime implementations that get confused
       on the day daylight saving starts or ends. (OSX) */
      PWARN (" mktime failed to handle daylight saving: "
       "tm_hour=%d tm_year=%d tm_min=%d tm_sec=%d tm_isdst=%d for string=%s", 
        stm.tm_hour, stm.tm_year, stm.tm_min,
        stm.tm_sec, stm.tm_isdst, dupe ); 
      tmp_tm.tm_hour++;
      secs = mktime (&tmp_tm);
      if (secs < 0) 
      { 
      /* if, for some strange reason, first attempt didn't fix it,
         try reversing the workaround. */
        tmp_tm.tm_hour -= 2;
        secs = mktime (&tmp_tm);
      }
    /* CAS: Even correct implementations of mktime can return
       (time_t)(-1): From the libc info page: "If the specified
       broken-down time cannot be represented as a simple time,
       `mktime' returns a value of `(time_t)(-1)' and does not modify
       the contents of BROKENTIME."  This happens for dates after 2038
       when time_t is 32 bits.  In those cases, this code above is
       just noisy and has a slight risk of returning the incorrect
       time.
     */
      if (secs < 0) 
      {
        /* Seriously buggy mktime - give up.  */
        PERR (" unable to recover from buggy mktime ");
        g_free(dupe);
        return ts;
      }
    }

    /* The call to localtime is 'bogus', but it forces 'timezone' to
     * be set. Note that we must use the accurate date, since the
     * value of 'gnc_timezone' includes daylight savings corrections
     * for that date. */

    localtime_r (&secs, &tm);

    tz = gnc_timezone (&tmp_tm);

    tz_hour = tz / 3600;
    stm.tm_hour -= tz_hour;
    stm.tm_min -= (tz % 3600) / 60;
    stm.tm_isdst = tmp_tm.tm_isdst;
    ts.tv_sec = mktime (&stm);
    if(ts.tv_sec < 0) { 
      PWARN (" mktime failed to adjust calculated time:"
        " tm_hour=%d tm_year=%d tm_min=%d tm_sec=%d tm_isdst=%d", 
        stm.tm_hour, stm.tm_year, stm.tm_min,
        stm.tm_sec, stm.tm_isdst ); 
      /* Try and make some sense of the result. */
      ts.tv_sec = secs - tz;  
    }
    ts.tv_nsec = nsec;
  }
  g_free(dupe);
  return ts;
}

/********************************************************************\
\********************************************************************/

char * 
gnc_timespec_to_iso8601_buff (Timespec ts, char * buff)
{
  int len, tz_hour, tz_min;
  long int secs;
  char cyn;
  time_t tmp;
  struct tm parsed;

  tmp = ts.tv_sec;
  localtime_r(&tmp, &parsed);

  secs = gnc_timezone (&parsed);

  /* We also have to print the sign by hand, to work around a bug
   * in the glibc 2.1.3 printf (where %+02d fails to zero-pad).
   */
  cyn = '-';
  if (0>secs) { cyn = '+'; secs = -secs; }

  tz_hour = secs / 3600;
  tz_min = (secs % 3600) / 60;

  len = sprintf (buff, "%4d-%02d-%02d %02d:%02d:%02d.%06ld %c%02d%02d",
                 parsed.tm_year + 1900,
                 parsed.tm_mon + 1,
                 parsed.tm_mday,
                 parsed.tm_hour,
                 parsed.tm_min,
                 parsed.tm_sec,
                 ts.tv_nsec / 1000,
                 cyn,
                 tz_hour,
                 tz_min);

  /* Return pointer to end of string. */
  buff += len;
  return buff;
}

int
gnc_timespec_last_mday (Timespec t)
{
  struct tm result;
  time_t t_secs = t.tv_sec + (t.tv_nsec / NANOS_PER_SECOND);
  localtime_r(&t_secs, &result);
  return date_get_last_mday (&result);
}

void
gnc_timespec2dmy (Timespec t, int *day, int *month, int *year)
{
  struct tm result;
  time_t t_secs = t.tv_sec + (t.tv_nsec / NANOS_PER_SECOND);
  localtime_r(&t_secs, &result);

  if (day) *day = result.tm_mday;
  if (month) *month = result.tm_mon+1;
  if (year) *year = result.tm_year+1900;
}

/********************************************************************\
\********************************************************************/
/* hack alert -- this routine returns incorrect values for 
 * dates before 1970 */

time_t 
xaccDMYToSec (int day, int month, int year)
{
  struct tm stm;
  time_t secs;

  stm.tm_year = year - 1900;
  stm.tm_mon = month - 1;
  stm.tm_mday = day;
  gnc_tm_set_day_start(&stm);

  /* compute number of seconds */
  secs = mktime (&stm);

  return secs;
}


#define THIRTY_TWO_YEARS 0x3c30fc00LL

static Timespec
gnc_dmy2timespec_internal (int day, int month, int year, gboolean start_of_day)
{
  Timespec result;
  struct tm date;
  long long secs = 0;
  long long era = 0;

  year -= 1900;

  /* make a crude attempt to deal with dates outside the range of Dec
   * 1901 to Jan 2038. Note we screw up centennial leap years here so
   * hack alert */
  if ((2 > year) || (136 < year)) 
  {
    era = year / 32;
    year %= 32;
    if (0 > year) { year += 32; era -= 1; } 
  }

  date.tm_year = year;
  date.tm_mon = month - 1;
  date.tm_mday = day;

  if (start_of_day)
    gnc_tm_set_day_start(&date);
  else
    gnc_tm_set_day_end(&date);

  /* compute number of seconds */
  secs = mktime (&date);

  secs += era * THIRTY_TWO_YEARS;

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
  /* timezone is seconds *west* of UTC and is
   * not adjusted for daylight savings time.
   * In Spring, we spring forward, wheee! */
  return (long int)(timezone - (tm->tm_isdst > 0 ? 3600 : 0));
#endif
}


void
timespecFromTime_t( Timespec *ts, time_t t )
{
    ts->tv_sec = t;
    ts->tv_nsec = 0;
}

time_t 
timespecToTime_t (Timespec ts)
{
    return ts.tv_sec;
}

void
gnc_tm_get_day_start (struct tm *tm, time_t time_val)
{
  /* Get the equivalent time structure */
  tm = localtime_r(&time_val, tm);
  gnc_tm_set_day_start(tm);
}

void
gnc_tm_get_day_end (struct tm *tm, time_t time_val)
{
  /* Get the equivalent time structure */
  tm = localtime_r(&time_val, tm);
  gnc_tm_set_day_end(tm);
}

time_t
gnc_timet_get_day_start (time_t time_val)
{
  struct tm tm;

  gnc_tm_get_day_start(&tm, time_val);
  return mktime(&tm);
}

time_t
gnc_timet_get_day_end (time_t time_val)
{
  struct tm tm;

  gnc_tm_get_day_end(&tm, time_val);
  return mktime(&tm);
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

time_t
gnc_timet_get_today_start (void)
{
  struct tm tm;

  gnc_tm_get_day_start(&tm, time(NULL));
  return mktime(&tm);
}

time_t
gnc_timet_get_today_end (void)
{
  struct tm tm;

  gnc_tm_get_day_end(&tm, time(NULL));
  return mktime(&tm);
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
