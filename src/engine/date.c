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

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#include "config.h"
#include "date.h"
#include "util.h"

/* This is now user configured through the gnome options system() */
static DateFormat dateFormat = DATE_FORMAT_US;

/* This static indicates the debugging module that this .o belongs to. */
static short module = MOD_ENGINE;


/********************************************************************\
\********************************************************************/


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
        tm_str.tm_mon = month - 1; /*tm_mon = 0 through 11 */
        tm_str.tm_year = year - 1900; /* this is what the standard 
                                       * says, it's not a Y2K thing
                                       */
        strftime(buff, MAX_DATE_LENGTH, "%x", &tm_str);
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
  
  theTime = localtime(&t);
  
  printDate (buff, theTime->tm_mday, 
                   theTime->tm_mon + 1,
                   theTime->tm_year + 1900);
}

char * 
xaccPrintDateSecs (time_t t)
{
   char buff[100];
   printDateSecs (buff, t);
   return strdup (buff);
}

const char *
gnc_print_date(Timespec ts)
{
  static char buff[MAX_DATE_LENGTH];
  time_t t;

  t = ts.tv_sec + (ts.tv_nsec / 1000000000.0);

  printDateSecs(buff, t);

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
 * Return: 0 if conversion was successful, 1 otherwise
 *
 * Globals: global dateFormat value
 */
void 
scanDate(const char *buff, int *day, int *month, int *year)
{
   char *dupe, *tmp, *first_field, *second_field, *third_field;
   int iday, imonth, iyear;
   time_t secs;
   struct tm *now;

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
   switch(dateFormat)
   {
#if 0 /* strptime broken in glibc <= 2.1.2 */
     case DATE_FORMAT_LOCALE:
       if (buff[0] != 0)
       {
         struct tm thetime;

         strptime(buff, "%x", &thetime);

         iday = thetime.tm_mday;
         imonth = thetime.tm_mon + 1;
         iyear = thetime.tm_year + 1900;
       }
       break;
#endif
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
char dateSeparator()
{
  char separator;

  switch(dateFormat)
  {
    case DATE_FORMAT_CE:
      separator='.';
      break;
    case DATE_FORMAT_ISO:
      separator='-';
      break;
    case DATE_FORMAT_US:
    case DATE_FORMAT_UK:
    default:
      separator='/';
      break;
  }

  return separator;
}

/********************************************************************\
\********************************************************************/

time_t 
xaccDMYToSec (int day, int month, int year)
{
   struct tm stm;
   time_t secs;

   stm.tm_year = year - 1900;
   stm.tm_mon = month - 1;
   stm.tm_mday = day;
   stm.tm_hour = 11;
   stm.tm_min = 0;
   stm.tm_sec = 0;

   /* compute number of seconds */
   secs = mktime (&stm);

   return (secs);
}

time_t
xaccScanDateS (const char *str)
{
   int month,day,year;
   scanDate (str, &day, &month, &year);
   return (xaccDMYToSec (day,month,year));
}

/* ================================================ */
/* february default is 28, and patched below */
static char days_in_month[12] = { 31,28,31,30,31,30,31,31,30,31,30,31 };

static void
xaccValidateDateInternal (struct tm *date, int recur)
{
   int day, month, year;

   /* avoid infinite recursion */
   if (1 < recur) return;

   day = date->tm_mday;
   month = date->tm_mon + 1;
   year = date->tm_year + 1900;

   /* adjust days in february for leap year */
   if (((year % 4 == 0) && (year % 100 != 0)) || (year % 400 == 0)) {
      days_in_month[1] = 29;
   } else {
      days_in_month[1] = 28;
   }

   /* the "% 12" business is because month might not be valid!*/

   while (day > days_in_month[(month+11) % 12]) {
      day -= days_in_month[(month+11) % 12];
      month++;
   }
   while (day < 1) {
      month--;
      day += days_in_month[(month+11) % 12];
   }
   while (month > 12) {
      month -= 12;
      year++;
   }
   while (month < 1) {
      month += 12;
      year--;
   }

   date->tm_mday = day;
   date->tm_mon = month - 1;
   date->tm_year = year - 1900;

   /* do it again, in case leap-year scrolling messed things up */
   xaccValidateDateInternal (date, ++recur);
}

void
xaccValidateDate (struct tm *date)
{
  xaccValidateDateInternal (date, 0);
}

/********************** END OF FILE *********************************\
\********************************************************************/
