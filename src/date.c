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

#include <time.h>
#include "date.h"
#include "util.h"

#define False 0
#define True  1

/** PROTOTYPES ******************************************************/
int    validateDate( Date *date );

/** GLOBALS *********************************************************/
char days[12] = { 31,29,31,30,31,30,31,31,30,31,30,31 };

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
int
validateDate( Date *date )
  {
  int valid = True;
  
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
 *   returns the number of days in month                            *
 *                                                                  * 
 * Args:   month - the current month                                *
 * Return: the number of days in month                              *
\********************************************************************/
int
daysInMonth( int month )
  {
  return days[month-1];
  }

/********************************************************************\
 * datecmp                                                          *
 *   compares date1 and date2                                       *
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
    unsigned int d1 = date1->day + (31 * date1->month) + (365 * date1->year);
    unsigned int d2 = date2->day + (31 * date2->month) + (365 * date2->year);
    
    if( d1 < d2 )
      return -1;
    if( d1 == d2 )
      return 0;
    else
      return 1;
    }
  }
