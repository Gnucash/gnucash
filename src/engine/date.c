/********************************************************************\
 * date.c -- utility functions to handle the date (adjusting, get   * 
 *           current date, etc.) for xacc (X-Accountant)            *
 * Copyright (C) 1997 Robin D. Clark                                *
 * Copyright (C) 1998, 1999, 2000 Linas Vepstas                     *
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
 *   Author: Rob Clark rclark@cs.hmc.edu                            *
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

#include "date.h"
#include "gnc-engine-util.h"

#define NANOS_PER_SECOND 1000000000

#ifdef HAVE_LANGINFO_D_FMT
#  define GNC_D_FMT (nl_langinfo (D_FMT))
#else
#  define GNC_D_FMT "%Y-%m-%d"
#endif

/* This is now user configured through the gnome options system() */
static DateFormat dateFormat = DATE_FORMAT_US;

/* This static indicates the debugging module that this .o belongs to. */
static short module = MOD_ENGINE;


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
  tm.tm_sec = 0;
  tm.tm_min = 0;
  tm.tm_hour = 12;
  tm.tm_isdst = -1;
  retval.tv_sec = mktime(&tm);
  retval.tv_nsec = 0;
  return retval;
}
    
/**
 * setDateFormat
 * set date format to one of US, UK, CE, OR ISO
 * checks to make sure it's a legal value
 * Args: DateFormat: enumeration indicating preferred format
 * returns: nothing
 *
 * Globals: dateFormat
 **/

void setDateFormat(DateFormat df)
{
  if(df >= DATE_FORMAT_FIRST && df <= DATE_FORMAT_LAST)
  {
    dateFormat = df;
  }
  else
  {    /* hack alert - is this what we should be doing here? */
    PERR("non-existent date format set");
  }

  return;
}

/**
 * printDate
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
void 
printDate (char * buff, int day, int month, int year)
{
  if (!buff) return;

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
    case DATE_FORMAT_UK:
      sprintf (buff, "%2d/%2d/%-4d", day, month, year);
      break;
    case DATE_FORMAT_CE:
      sprintf (buff, "%2d.%2d.%-4d", day, month, year);
      break;
    case DATE_FORMAT_ISO:
      sprintf (buff, "%04d-%02d-%02d", year, month, day);
      break;
    case DATE_FORMAT_LOCALE:
      {
        struct tm tm_str;

        tm_str.tm_mday = day;
        tm_str.tm_mon = month - 1;    /* tm_mon = 0 through 11 */
        tm_str.tm_year = year - 1900; /* this is what the standard 
                                       * says, it's not a Y2K thing */
        tm_str.tm_hour = 0;
        tm_str.tm_min = 0;
        tm_str.tm_sec = 0;
        tm_str.tm_isdst = -1;

        strftime (buff, MAX_DATE_LENGTH, GNC_D_FMT, &tm_str);
      }
      break;

    case DATE_FORMAT_US:
    default:
      sprintf (buff, "%2d/%2d/%-4d", month, day, year);
      break;
  }
}

void 
printDateSecs (char * buff, time_t t)
{
  struct tm *theTime;

  if (!buff) return;

  theTime = localtime (&t);

  printDate (buff, theTime->tm_mday, 
                   theTime->tm_mon + 1,
                   theTime->tm_year + 1900);
}

char * 
xaccPrintDateSecs (time_t t)
{
   char buff[100];
   printDateSecs (buff, t);
   return g_strdup (buff);
}

const char *
gnc_print_date (Timespec ts)
{
  static char buff[MAX_DATE_LENGTH];
  time_t t;

  t = ts.tv_sec + (ts.tv_nsec / 1000000000.0);

  printDateSecs (buff, t);

  return buff;
}

/**
 * scanDate
 *    Convert a string into  day / month / year integers according to
 *    the current dateFormat value.
 *
 * Args:   buff - pointer to date string
 *         day -  will store day of the month as 1 ... 31
 *         month - will store month of the year as 1 ... 12
 *         year - will store the year (4-digit)
 *
 * Return: nothing
 *
 * Globals: global dateFormat value
 */
void
scanDate (const char *buff, int *day, int *month, int *year)
{
   char *dupe, *tmp, *first_field, *second_field, *third_field;
   int iday, imonth, iyear;
   struct tm *now;
   time_t secs;

   if (!buff) return;

   dupe = g_strdup (buff);

   tmp = dupe;
   first_field = NULL;
   second_field = NULL;
   third_field = NULL;

   /* use strtok to find delimiters */
   if (tmp) {
      first_field = strtok (tmp, ".,-+/\\()");
      if (first_field) {
         second_field = strtok (NULL, ".,-+/\\()");
         if (second_field) {
            third_field = strtok (NULL, ".,-+/\\()");
         }
      }
   }

   /* if any fields appear blank, use today's date */
   time (&secs);
   now = localtime (&secs);
   iday = now->tm_mday; 
   imonth = now->tm_mon+1;
   iyear = now->tm_year+1900;

   /* get numeric values */
   switch (dateFormat)
   {
     case DATE_FORMAT_LOCALE:
       if (buff[0] != '\0')
       {
         struct tm thetime;

         strptime (buff, GNC_D_FMT, &thetime);

         iday = thetime.tm_mday;
         imonth = thetime.tm_mon + 1;
         iyear = thetime.tm_year + 1900;
       }
       break;
     case DATE_FORMAT_UK:
     case DATE_FORMAT_CE:
       if (first_field) iday = atoi (first_field);
       if (second_field) imonth = atoi (second_field);
       if (third_field) iyear = atoi (third_field);
       break;
     case DATE_FORMAT_ISO:
       if (first_field) iyear = atoi (first_field);
       if (second_field) imonth = atoi (second_field);
       if (third_field) iday = atoi (third_field);
       break;
     case DATE_FORMAT_US:
     default:
       if (first_field) imonth = atoi (first_field);
       if (second_field) iday = atoi (second_field);
       if (third_field) iyear = atoi (third_field);
       break;
   }

   g_free (dupe);

   /* if the year entered is smaller than 100, assume we mean the current
      century (and are not revising some roman emperor's books) */
   if (iyear < 100)
     iyear += ((int) ((now->tm_year+1950-iyear)/100)) * 100;

   if (year) *year=iyear;
   if (month) *month=imonth;
   if (day) *day=iday;
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
char dateSeparator ()
{
  static char locale_separator = '\0';

  switch (dateFormat)
  {
    case DATE_FORMAT_CE:
      return '.';
    case DATE_FORMAT_ISO:
      return '-';
    case DATE_FORMAT_US:
    case DATE_FORMAT_UK:
    default:
      return '/';
    case DATE_FORMAT_LOCALE:
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


  /* timezone format can be +hh or +hhmm or +hh.mm (or -) */
  str += strcspn (str, "+-");
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

  /* adjust for the local timezone */
  if (do_localtime)
  {
    time_t tz_hour = 0;
    localtime (&tz_hour);   /* bogus call, forces 'timezone' to be set */
    tz_hour = timezone/3600;
    stm.tm_hour -= tz_hour;
    stm.tm_min -= (timezone - 3600*tz_hour)/60;
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

  tz_hour = timezone/3600;
  tz_min = (timezone - 3600*tz_hour)/60;
  if (0>tz_min) { tz_min +=60; tz_hour --; }

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
  stm.tm_hour = 0;
  stm.tm_min = 0;
  stm.tm_sec = 0;
  stm.tm_isdst = -1;

  /* compute number of seconds */
  secs = mktime (&stm);

  return secs;
}

time_t
xaccScanDateS (const char *str)
{
  int month, day, year;

  scanDate (str, &day, &month, &year);

  return xaccDMYToSec (day,month,year);
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
  {
    date.tm_hour = 0;
    date.tm_min = 0;
    date.tm_sec = 0;
  }
  else
  {
    date.tm_hour = 23;
    date.tm_min = 59;
    date.tm_sec = 59;
  }

  date.tm_isdst = -1;

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

/********************** END OF FILE *********************************\
\********************************************************************/
