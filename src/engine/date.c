/********************************************************************\
 * date.c -- utility functions to handle the date (adjusting, get   * 
 *           current date, etc.) for xacc (X-Accountant)            *
 * Copyright (C) 1997 Robin D. Clark                                *
 * Copyright (C) 1998 Linas Vepstas                                 *
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
 * along with this program; if not, write to the Free Software      *
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.        *
 *                                                                  *
 *   Author: Rob Clark                                              *
 * Internet: rclark@cs.hmc.edu                                      *
 *  Address: 609 8th Street                                         *
 *           Huntington Beach, CA 92648-4632                        *
 *                                                                  *
 *                                                                  * 
 * TODO: - for now, every year is leap year                         * 
 *                                                                  * 
\********************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#include "config.h"
#include "date.h"

/* hack alert -- this should be turned into user-configurable parameter .. */
DateFormat dateFormat = DATE_FORMAT_US;

/********************************************************************\
\********************************************************************/

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
   dupe = strdup (buff);
   tmp = dupe;
   first_field = 0x0;
   second_field = 0x0;
   third_field = 0x0;

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

   free (dupe);

   /* if the year entered is smaller than 100, assume we mean the current
      century (and are not revising some roman emperor's books) */
   if(iyear<100) {
     iyear += ((int) ((now->tm_year+1900)/100)) * 100;
   }

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

char *
xaccTransGetDateStr (Transaction *trans)
{
   char buf [MAX_DATE_LENGTH];
   time_t secs;
   struct tm *date;

   secs = xaccTransGetDate (trans);

   date = localtime (&secs);

   printDate(buf, date->tm_mday, date->tm_mon+1, date->tm_year +1900);
   return strdup (buf);
}

void
xaccTransSetDateStr (Transaction *trans, char *str)
{
   int day, month, year;

   /* hack alert -- the date string should be parsed for time values */
   /* cvs has some cool date parsing/guessing code .. maybe steal 
    * that code from there ...  */
   scanDate(str, &day, &month, &year);
   xaccTransSetDate (trans, day, month, year);
}

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


/********************** END OF FILE *********************************\
\********************************************************************/

      
/*
  Local Variables:
  tab-width: 2
  indent-tabs-mode: nil
  mode: c-mode
  c-indentation-style: gnu
  eval: (c-set-offset 'block-open '-)
  End:
*/
      
