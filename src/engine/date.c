/********************************************************************\
 * date.c -- utility functions to handle the date (adjusting, get   * 
 *           current date, etc.) for xacc (X-Accountant)            *
 * Copyright (C) 1997 Robin D. Clark                                *
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

#define False 0
#define True  1

/** PROTOTYPES ******************************************************/
static int    validateDate( Date *date );

/** GLOBALS *********************************************************/
/** DEFAULT FOR FEBRUARY IS 28 VICE 29 (and patched by year in validateDate() **/
static
char days[12] = { 31,28,31,30,31,30,31,31,30,31,30,31 };

static int been_here = 0;

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
void printDate (char * buff, int day, int month, int year)
{
  switch(dateFormat)
    {
    case DATE_FORMAT_UK:
      sprintf (buff, "%2d/%2d/%4d", day, month, year);
      break;
    case DATE_FORMAT_CE:
      sprintf (buff, "%2d.%2d.%4d", day, month, year);
      break;
    case DATE_FORMAT_ISO:
      sprintf (buff, "%04d-%02d-%02d", year, month, day);
      break;
    case DATE_FORMAT_US:
    default:
      sprintf (buff, "%2d/%2d/%4d", month, day, year);
      break;
    }
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
   if(iyear<100)
     iyear += ((int) ((now->tm_year+1900)/100)) * 100;

   *year=iyear;
   *month=imonth;
   *day=iday;
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
 * adjustDay                                                        *
 *   adds adj to the current day of the month... the resulting day  *
 *   of the month is a valid day of the month                       *
 *                                                                  * 
 * Args:   date - the date structure to edit                        *
 *         adj  - the amount to adjust the day of the month by      *
 * Return: none                                                     *
\********************************************************************/
void    
adjustDay( Date *date, int adj )
  {
  date->day += adj;
  validateDate(date);
  }

/********************************************************************\
 * adjustMonth                                                      *
 *   adds adj to the current month of the year.. the resulting      *
 *   month of the year is corrected to be in the range [1..12],     *
 *   incrementing/decrementing the year if necessary.               * 
 *                                                                  * 
 * Args:   date - the date structure to edit                        *
 *         adj  - the amount to adjust the day of the month by      *
 * Return: none                                                     *
\********************************************************************/
void    
adjustMonth( Date *date, int adj )
  {
  date->month += adj;
  validateDate(date);
  }

/********************************************************************\
 * validateDate                                                     *
 *   ensures that all the fields in date are in the valid range     *
 *   (ie month = [1..12], day is not less than 1, and not greater   *
 *   than last day of the month.).  If fields are not in the valid  * 
 *   range, they are adjusted.                                      * 
 *                                                                  * 
 * Args:   date - the date structure to edit                        *
 * Return: True if date was changed, otherwise False                *
\********************************************************************/
static int
validateDate( Date *date )
  {
  int valid = True;

  /* the "been here" flag prevents infinite recursion */
  if (1 == been_here) return valid;
  been_here = 1;

  /* adjust days in february for leap year */
  if ( ( ( date->year % 4 == 0 ) && ( date->year % 100 != 0 ) )
       || ( date->year % 400 == 0 ) )
    days[1] = 29;
  else 
    days[1] = 28;
  
  /* the "% 12" business is because month might not be valid!*/

  while( date->day > days[(date->month+11) % 12] )
    {
    valid = False;
    date->day -= days[(date->month+11) % 12];
    date->month++;
    }
  while( date->day < 1 )
    {
    valid = False;
    date->month--;
    date->day += days[(date->month+11) % 12];
    }
  while( date->month > 12 )
    {
    valid = False;
    date->month -= 12;
    date->year++;
    }
  while( date->month < 1 )
    {
    valid = False;
    date->month += 12;
    date->year--;
    }
  
  /* try again, in case a year change messed up leap year calcs. */
  validateDate (date);
  been_here = 0;

  return valid;
  }

/********************************************************************\
 * todaysDate                                                       *
 *   takes a (Date *) and fills it in with today's date             *
 *                                                                  * 
 * Args:   date - the struct to be filled in                        *
 * Return: today's date                                             *
\********************************************************************/
Date*   
todaysDate( Date *date )
  {
  time_t t;
  struct tm *theTime;
  
  time(&t);
  theTime = localtime(&t);
  
  date->day   = theTime->tm_mday;
  date->month = theTime->tm_mon + 1;
  date->year  = theTime->tm_year + 1900;
  
  return date;
  }

/********************************************************************\
 * daysInMonth                                                      *
 *   returns the number of days in month. The argument "year" is    *
 *   required to take into account leap years                       *
 *                                                                  * 
 * Args:   month - the current month                                *
 *         year  - the current year                                 *
 * Return: the number of days in month                              *
\********************************************************************/

int    daysInMonth( int month , int year )
  {
  /* adjust for leap year */
  if( month == 2 ) {  
    if ( ( ( 0 == year % 4 ) && ( 0 != year % 100 ) )
         || ( 0 == year % 400 ) ) 
      days[1] = 29;
    else 
      days[1] = 28;
  }
  return days[month-1];
  }

/********************************************************************\
 * datecmp                                                          *
 *   compares date1 and date2                                       *
 *   If you tink you need this function, you probably don't.        *
 *   You should probably use xaccTransOrder instead.  Some          *
 *   of the algorithms used here depend on the superior ordering    *
 *   that xaccTransOrder provides.                                  *
 *                                                                  * 
 * Args:   date1 - the first date to look at                        *
 *         date2 - the second date in the comparison                * 
 * Return: < 0 if date1<date2, == 0 if date1==date2, > 0 otherwise  *
\********************************************************************/
int
datecmp( Date *date1, Date *date2 )
  {
  if( date1 == NULL )
    return 0;
  else if( date2 == NULL )
    return 0;
  else
    {
#ifdef  OLD_DATE_COMPARISON_CODE
    /* to sort properly, must have 32 > days in month, and 500 > 12*32 */
    unsigned int d1 = date1->day + (32 * date1->month) + (500 * date1->year);
    unsigned int d2 = date2->day + (32 * date2->month) + (500 * date2->year);
    
    if( d1 < d2 )
      return -1;
    if( d1 == d2 )
      return 0;
    else
      return 1;
#endif /* OLD_DATE_COMPARISON_CODE */

    /* this date comparison code executes faster than multiply
     * code above, according to Dave Freese of USCG */
    if ( date1->year > date2->year ) return 1;
    if ( date1->year < date2->year ) return -1;
    if ( date1->month > date2->month ) return 1;
    if ( date1->month < date2->month ) return -1;
    if ( date1->day > date2->day ) return 1;
    if ( date1->day < date2->day ) return -1;
    return 0;
    }
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

   printDate(buf, date->tm_mday, date->tm_mon+1, (date->tm_year)%100);
   return strdup (buf);
}

void
xaccTransSetDateStr (Transaction *trans, char *str)
{
   Date d;

   /* hack alert -- the date string should be parsed for time values */
   scanDate(str, &(d.day), &(d.month), &(d.year));
   xaccTransSetDate (trans, d.day, d.month, d.year);
}

time_t 
xaccDateToSec (Date *date)
{
   struct tm stm;
   time_t secs;

   stm.tm_year = date->year - 1900;
   stm.tm_mon = date->month - 1;
   stm.tm_mday = date->day;
   stm.tm_hour = 11;
   stm.tm_min = 0;
   stm.tm_sec = 0;

   /* compute number of seconds */
   secs = mktime (&stm);

   return (secs);
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
      
