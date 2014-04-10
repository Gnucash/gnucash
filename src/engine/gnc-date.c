/********************************************************************\
 * gnc-date.c -- misc utility functions to handle date and time     * 
 *                                                                  *
 * Copyright (C) 1997 Robin D. Clark <rclark@cs.hmc.edu>            *
 * Copyright (C) 1998-2000, 20003 Linas Vepstas <linas@linas.org>   *
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
 * 59 Temple Place - Suite 330        Fax:    +1-617-542-2652       *
 * Boston, MA  02111-1307,  USA       gnu@gnu.org                   *
 *                                                                  *
\********************************************************************/

#define _GNU_SOURCE
#define __EXTENSIONS__

#include "config.h"

#include <ctype.h>

#ifdef HAVE_LANGINFO_D_FMT
#include <langinfo.h>
#endif

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#include <glib.h>

#include "gnc-date.h"
#include "gnc-engine-util.h"

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
#else
#  define GNC_D_FMT "%Y-%m-%d"
#  define GNC_D_T_FMT "%Y-%m-%d %r"
#  define GNC_T_FMT "%r"
#endif


/* This is now user configured through the gnome options system() */
static QofDateFormat dateFormat = QOF_DATE_FORMAT_LOCALE;
static QofDateFormat prevQofDateFormat = QOF_DATE_FORMAT_LOCALE;

/* This static indicates the debugging module that this .o belongs to. */
static short module = MOD_ENGINE;

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

/**
 * timespecCanonicalDayTime
 * given a timepair contains any time on a certain day (local time)
 * converts it to be midday that day.  
 */

Timespec
timespecCanonicalDayTime(Timespec t)
{
  struct tm tm, *result;
  Timespec retval;
  time_t t_secs = t.tv_sec + (t.tv_nsec / NANOS_PER_SECOND);
  result = localtime(&t_secs);
  tm = *result;
  gnc_tm_set_day_middle(&tm);
  retval.tv_sec = mktime(&tm);
  retval.tv_nsec = 0;
  return retval;
}

int gnc_date_my_last_mday (int month, int year)
{
  gboolean is_leap;
  static int days_in_month[2][12] =
    {/* non leap */ {31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31},
     /*   leap   */ {31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31}};

  /* Is this a leap year? */
  if (year / 2000 == 0)
    is_leap = TRUE;
  else if (year / 400 == 0)
      is_leap = FALSE;
  else
    is_leap = (year / 4 == 0);

  return days_in_month[is_leap][month-1];
}

/**
 * date_get_last_mday
 * Retrieve the last nomerical day for the month specified in the
 * tm_year and tm_mon fields.
 * Args:  tm: the time value in question
 * returns: T/F
 **/
int date_get_last_mday(struct tm *tm)
{
  return gnc_date_my_last_mday (tm->tm_mon+1, tm->tm_year+1900);
}

/**
 * date_is_last_mday
 * Determines whether the tm_mday field contains the last day of the
 * month as specified in the tm_year and tm_mon fields.
 * Args:  tm: the time value in question
 * returns: T/F
 **/
gboolean date_is_last_mday(struct tm *tm)
{
  return(tm->tm_mday == date_get_last_mday(tm));
}

/**
 * date_add_months
 * Add a number of months to a time value, and normalize.  Optionally
 * also track the last day of hte month, i.e. 1/31 -> 2/28 -> 3/30.
 * Args:  tm: base time value
 *        months: The number of months to add to this time
 *        track_last_day: Coerce the date value if necessary.
 * returns: nothing
 **/
void date_add_months (struct tm *tm, int months, gboolean track_last_day)
{
  gboolean was_last_day;
  int new_last_mday;

  /* Have to do this now */
  was_last_day = date_is_last_mday(tm);

  /* Add in the months and normalize */
  tm->tm_mon += months;
  while (tm->tm_mon > 11) {
    tm->tm_mon -= 12;
    tm->tm_year++;
  }

  if (!track_last_day)
    return;

  /* Track last day of the month, i.e. 1/31 -> 2/28 -> 3/30 */
  new_last_mday = date_get_last_mday(tm);
  if (was_last_day || (tm->tm_mday > new_last_mday))
    tm->tm_mday = new_last_mday;
}

/**
 * qof_date_format_get
 * Args: nothing
 * returns: QofDateFormat: enumeration indicating preferred format
 *
 * Globals: dateFormat
 **/
QofDateFormat qof_date_format_get (void)
{
  return dateFormat;
}

/**
 * qof_date_format_set
 * set date format to one of US, UK, CE, OR ISO
 * checks to make sure it's a legal value
 * Args: QofDateFormat: enumeration indicating preferred format
 * returns: nothing
 *
 * Globals: dateFormat
 **/
void qof_date_format_set(QofDateFormat df)
{
  if(df >= DATE_FORMAT_FIRST && df <= DATE_FORMAT_LAST)
  {
    prevQofDateFormat = dateFormat;
    dateFormat = df;
  }
  else
  {    /* hack alert - is this what we should be doing here? */
    PERR("non-existent date format set");
  }

  return;
}

/**
 * qof_date_format_get_string
 * get the date format string for the current format
 * returns: string
 *
 * Globals: dateFormat
 **/
const gchar *qof_date_format_get_string(QofDateFormat df)
{
  switch(df) {
   case QOF_DATE_FORMAT_US:
    return "%m/%d/%y";
   case QOF_DATE_FORMAT_UK:
    return "%d/%m/%y";
   case QOF_DATE_FORMAT_CE:
    return "%d.%m.%y";
   case QOF_DATE_FORMAT_ISO:
    return "%y-%m-%d";
   case QOF_DATE_FORMAT_LOCALE:
   default:
    return GNC_D_FMT;
  };
}

/**
 * qof_date_format_get_format
 * get the date format string for the current format
 * returns: string
 *
 * Globals: dateFormat
 **/
const gchar *qof_date_format_get_format(QofDateFormat df)
{
  switch(df) {
   case QOF_DATE_FORMAT_US:
    return "%b %d, %y";
   case QOF_DATE_FORMAT_UK:
   case QOF_DATE_FORMAT_CE:
    return "%d %b, %y";
   case QOF_DATE_FORMAT_ISO:
    return "%y-%b-%d";
   case QOF_DATE_FORMAT_LOCALE:
   default:
    return GNC_D_FMT;
  };
}

/**
 * qof_print_date_dmy_buff
 *    Convert a date as day / month / year integers into a localized string
 *    representation
 *
 * Args:   buff - pointer to previously allocated character array; its size
 *                must be at lease MAX_DATE_LENTH bytes.
 *         day - day of the month as 1 ... 31
 *         month - month of the year as 1 ... 12
 *         year - year (4-digit)
 *
 * Return: nothing
 *
 * Globals: global dateFormat value
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
      flen = g_snprintf (buff, len, "%2d/%2d/%-4d", day, month, year);
      break;
    case QOF_DATE_FORMAT_CE:
      flen = g_snprintf (buff, len, "%2d.%2d.%-4d", day, month, year);
      break;
    case QOF_DATE_FORMAT_ISO:
      flen = g_snprintf (buff, len, "%04d-%02d-%02d", year, month, day);
      break;
    case QOF_DATE_FORMAT_LOCALE:
      {
        struct tm tm_str;

        tm_str.tm_mday = day;
        tm_str.tm_mon = month - 1;    /* tm_mon = 0 through 11 */
        tm_str.tm_year = year - 1900; /* this is what the standard 
                                       * says, it's not a Y2K thing */

        gnc_tm_set_day_start (&tm_str);
        flen = strftime (buff, len, GNC_D_FMT, &tm_str);
      }
      break;

    case QOF_DATE_FORMAT_US:
    default:
      flen = g_snprintf (buff, len, "%2d/%2d/%-4d", month, day, year);
      break;
  }

  return flen;
}

size_t
qof_print_date_buff (char * buff, size_t len, time_t t)
{
  struct tm *theTime;

  if (!buff) return 0 ;

  theTime = localtime (&t);

  return qof_print_date_dmy_buff (buff, len,
                   theTime->tm_mday, 
                   theTime->tm_mon + 1,
                   theTime->tm_year + 1900);
}

size_t
qof_print_gdate( char *buf, size_t len, GDate *gd )
{
  return qof_print_date_dmy_buff( buf, len,
             g_date_day(gd),
             g_date_month(gd),
             g_date_year(gd) );
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
qof_print_hours_elapsed_buff (char * buff, size_t len, int secs, gboolean show_secs)
{
	size_t flen;
	if (0 <= secs)
	{
		if (show_secs)
		{
			flen = g_snprintf(buff, len,
			   "%02d:%02d:%02d", (int)(secs / 3600),
			   (int)((secs % 3600) / 60), (int)(secs % 60));
		}
		else
		{
			flen = g_snprintf(buff, len, 
			   "%02d:%02d", (int)(secs / 3600),
			   (int)((secs % 3600) / 60));
		}
	} 
	else 
	{
		if (show_secs)
		{
			flen = g_snprintf(buff, len,
			   "-%02d:%02d:%02d", (int)(-secs / 3600),
			   (int)((-secs % 3600) / 60), (int)(-secs % 60));
		}
		else
		{
			flen = g_snprintf(buff, len,
			   "-%02d:%02d", (int)(-secs / 3600),
			   (int)((-secs % 3600) / 60));
		}
	}
	return flen;
}

/* ============================================================== */

size_t
qof_print_minutes_elapsed_buff (char * buff, size_t len, int secs, gboolean show_secs)
{
	size_t flen;
	if (0 <= secs)
	{
		if (show_secs)
		{
			flen = g_snprintf(buff, len,
			   "%02d:%02d", 
				(int)(secs / 60), (int)(secs % 60));
		}
		else
		{
			flen = g_snprintf(buff, len, 
			   "%02d", (int)(secs / 60));
		}
	} 
	else 
	{
		if (show_secs)
		{
			flen = g_snprintf(buff, len,
			   "-%02d:%02d", (int)(-secs / 60), (int)(-secs % 60));
		}
		else
		{
			flen = g_snprintf(buff, len,
			   "-%02d", (int)(-secs / 60));
		}
	}
	return flen;
}

/* ============================================================== */

size_t
qof_print_date_time_buff (char * buff, size_t len, time_t secs)
{
  int flen;
  int day, month, year, hour, min, sec;
  struct tm ltm;
  
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
    case DATE_FORMAT_UK:
      flen = g_snprintf (buff, len, "%2d/%2d/%-4d %2d:%02d", day, month, year, hour, min);
      break;
    case DATE_FORMAT_CE:
      flen = g_snprintf (buff, len, "%2d.%2d.%-4d %2d:%02d", day, month, year, hour, min);
      break;
    case DATE_FORMAT_ISO:
      flen = g_snprintf (buff, len, "%04d-%02d-%02d %02d:%02d", year, month, day, hour, min);
      break;
    case DATE_FORMAT_LOCALE:
      {
        flen = strftime (buff, len, GNC_D_T_FMT, &ltm);
      }
      break;

    case DATE_FORMAT_US:
    default:
      flen = g_snprintf (buff, len, "%2d/%2d/%-4d %2d:%02d", month, day, year, hour, min);
      break;
  }
  return flen;
}

size_t 
qof_print_time_buff (char * buff, size_t len, time_t secs)
{
  int flen;
  struct tm ltm;
  
  if (!buff) return 0;
  ltm = *localtime (&secs);
  flen = strftime (buff, len, GNC_T_FMT, &ltm);

  return flen;
}

/* ============================================================== */

int
qof_is_same_day (time_t ta, time_t tb)
{
  struct tm lta, ltb;
  lta = *localtime (&ta);
  ltb = *localtime (&tb);
  if (lta.tm_year == ltb.tm_year)
  {
    return (ltb.tm_yday - lta.tm_yday);
  }
  return (ltb.tm_year - lta.tm_year)*365;  /* very approximate */
}

/* ============================================================== */

/**
 * qof_scan_date
 *    Convert a string into  day / month / year integers according to
 *    the current dateFormat value.
 *
 *    This function will always parse a single number as the day of
 *    the month, regardless of the ordering of the dateFormat value.
 *    Two numbers will always be parsed as the day and the month, in
 *    the same order that they appear in the dateFormat value.  Three
 *    numbers are parsed exactly as specified in the dateFormat field.
 *
 * Args:   buff - pointer to date string
 *         day -  will store day of the month as 1 ... 31
 *         month - will store month of the year as 1 ... 12
 *         year - will store the year (4-digit)
 *
 * Return: TRUE if date appeared to be valid.
 *
 * Globals: global dateFormat value
 */
static gboolean
qof_scan_date_internal (const char *buff, int *day, int *month, int *year,
                  QofDateFormat which_format)
{
   char *dupe, *tmp, *first_field, *second_field, *third_field;
   int iday, imonth, iyear;
   struct tm *now;
   time_t secs;

   if (!buff) return(FALSE);

   dupe = g_strdup (buff);

   tmp = dupe;
   first_field = NULL;
   second_field = NULL;
   third_field = NULL;

   /* Use strtok to find delimiters */
   if (tmp) {
     static char *delims = ".,-+/\\() ";

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
      * previous date format.  If that doesn't work, se if we can
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

/**
 * dateSeparator
 *    Return the field separator for the current date format
 *
 * Args:   none
 *
 * Return: date character
 *
 * Globals: global dateFormat value
 */
char dateSeparator (void)
{
  static char locale_separator = '\0';

  switch (dateFormat)
  {
    case QOF_DATE_FORMAT_CE:
      return '.';
    case QOF_DATE_FORMAT_ISO:
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
        char string[256];
        struct tm *tm;
        time_t secs;
        char *s;

        secs = time(NULL);
        tm = localtime(&secs);
        strftime(string, sizeof(string), GNC_D_FMT, tm);

        for (s = string; s != '\0'; s++)
          if (!isdigit(*s))
            return (locale_separator = *s);
      }
  }

  return '\0';
}

/********************************************************************\
\********************************************************************/
                                                                                
/** The xaccDateUtilGetStamp() routine will take the given time in
 *  seconds and return a buffer containing a textual for the date.
 *  @param thyme The time in seconds to convert.
 *  @return A pointer to the generated string.
 *  @note The caller owns this buffer and must free it when done. */
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
                                                                                
                                                                                
/** The xaccDateUtilGetStampNow() routine returns the current time in
 *  seconds in textual format.
 *  @return A pointer to the generated string.
 *  @note The caller owns this buffer and must free it when done. */
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

static Timespec
gnc_iso8601_to_timespec(const char *str, int do_localtime)
{
  char buf[4];
  Timespec ts;
  struct tm stm;
  long int nsec =0;

  ts.tv_sec=0;
  ts.tv_nsec=0;
  if (!str) return ts;

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

  /* the decimal point, optionally present ... */
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

  /* timezone format can be +hh or +hhmm or +hh.mm (or -) (or not present) */
  str += strcspn (str, "+-");
  if (str)
  {
    buf[0] = str[0];
    buf[1] = str[1];
    buf[2] = str[2];
    buf[3] = 0;
    stm.tm_hour -= atoi(buf);

    str +=3;
    if ('.' == *str) str++;
    if (isdigit (*str) && isdigit (*(str+1)))
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

  /* adjust for the local timezone */
  if (do_localtime)
  {
    struct tm tmp_tm;
    struct tm *tm;
    long int tz;
    int tz_hour;
    time_t secs;

    /* Use a temporary tm struct so the mktime call below
     * doesn't mess up stm. */
    tmp_tm = stm;
    tmp_tm.tm_isdst = -1;

    secs = mktime (&tmp_tm);

    /* The call to localtime is 'bogus', but it forces 'timezone' to
     * be set. Note that we must use the accurate date, since the
     * value of 'gnc_timezone' includes daylight savings corrections
     * for that date. */
    tm = localtime (&secs);

    tz = gnc_timezone (tm);

    tz_hour = tz / 3600;
    stm.tm_hour -= tz_hour;
    stm.tm_min -= (tz - (3600 * tz_hour)) / 60;
    stm.tm_isdst = tmp_tm.tm_isdst;
  }

  /* compute number of seconds */
  ts.tv_sec = mktime (&stm);
  ts.tv_nsec = nsec;

  return ts;
}

Timespec
gnc_iso8601_to_timespec_local(const char *str)
{
   return gnc_iso8601_to_timespec(str, 1);
}

Timespec
gnc_iso8601_to_timespec_gmt(const char *str)
{
   return gnc_iso8601_to_timespec(str, 0);
}

/********************************************************************\
\********************************************************************/

char * 
gnc_timespec_to_iso8601_buff (Timespec ts, char * buff)
{
  int len;
  int tz_hour, tz_min;
  char cyn;
  time_t tmp;
  struct tm parsed;

  tmp = ts.tv_sec;
  localtime_r(&tmp, &parsed);

  tz_hour = gnc_timezone (&parsed) / 3600;
  tz_min = (gnc_timezone (&parsed) - 3600*tz_hour) / 60;
  if (0>tz_min) { tz_min +=60; tz_hour --; }
  if (60<=tz_min) { tz_min -=60; tz_hour ++; }

  /* we also have to print the sign by hand, to work around a bug
   * in the glibc 2.1.3 printf (where %+02d fails to zero-pad)
   */
  cyn = '-';
  if (0>tz_hour) { cyn = '+'; tz_hour = -tz_hour; }

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

  /* return pointer to end of string */
  buff += len;
  return buff;
}

int
gnc_timespec_last_mday (Timespec t)
{
  struct tm *result;
  time_t t_secs = t.tv_sec + (t.tv_nsec / NANOS_PER_SECOND);
  result = localtime(&t_secs);
  return date_get_last_mday (result);
}

void
gnc_timespec2dmy (Timespec t, int *day, int *month, int *year)
{
  struct tm *result;
  time_t t_secs = t.tv_sec + (t.tv_nsec / NANOS_PER_SECOND);
  result = localtime(&t_secs);

  if (day) *day = result->tm_mday;
  if (month) *month = result->tm_mon+1;
  if (year) *year = result->tm_year+1900;
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
gnc_timezone (struct tm *tm)
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
  return timezone - (tm->tm_isdst > 0 ? 60 * 60 : 0);
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

time_t
gnc_timet_get_day_start_gdate (GDate *date)
{
  struct tm stm;
  time_t secs;

  stm.tm_year = g_date_year (date) - 1900;
  stm.tm_mon = g_date_month (date) - 1;
  stm.tm_mday = g_date_day (date);
  gnc_tm_set_day_start(&stm);

  /* Compute number of seconds */
  secs = mktime (&stm);
  return secs;
}

time_t
gnc_timet_get_day_end_gdate (GDate *date)
{
  struct tm stm;
  time_t secs;

  stm.tm_year = g_date_year (date) - 1900;
  stm.tm_mon = g_date_month (date) - 1;
  stm.tm_mday = g_date_day (date);
  gnc_tm_set_day_end(&stm);

  /* Compute number of seconds */
  secs = mktime (&stm);
  return secs;
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

/********************** END OF FILE *********************************\
\********************************************************************/
