
/* 
 * Major Hack Alert ---
 * this whole file & design needs to be replaced with
 * something that can handle seconds (actually, milliseconds
 * to keep the banks happy).
 *
 * There a lot of lint here and it needs major overhaul in general.
 */

/********************************************************************\
 * date.h -- utility functions to handle the date (adjusting, get   * 
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
\********************************************************************/

/********************************************************************\
 * adjustDay                                                        *
 *   increments/decrements the date structure by the indicated      *
 *   number of days.  The month and/or year is adjusted             *
 *   appropriately, as needed.                                      *
 *                                                                  * 
 * Args:   date - the date structure to edit                        *
 *         adj  - the number of days to change the date by.         *
 * Return: none                                                     *
\********************************************************************/

/********************************************************************\
 * adjustMonth                                                      *
 *   adds adj to the current month of the year.. the resulting      *
 *   month of the year is corrected to be in the range [1..12],     *
 *   incrementing/decrementing the year if necessary.               * 
 *                                                                  * 
 * Args:   date - the date structure to edit                        *
 *         adj  - the number of months to change the date by.       *
 * Return: none                                                     *
\********************************************************************/

/********************************************************************\
 * todaysDate                                                       *
 *   takes a (Date *) and fills it in with today's date             *
 *                                                                  * 
 * Args:   date - the struct to be filled in                        *
 * Return: today's date                                             *
\********************************************************************/

/********************************************************************\
 * daysInMonth                                                      *
 *   returns the number of days in month.  Leap years are handled   *
 *   correctly, based on the value of the argument "year".          *
 *                                                                  * 
 * Args:   month - a number [0..11] indicating the month            *
 *         year  - a number (e.g. 1997) indicating the year         *
\********************************************************************/

/********************************************************************\
 * datecmp                                                          *
 *   compares date1 and date2                                       *
 *                                                                  *
 *   Note: for ordering transactions, don't use this routine.       *
 *   You should probably use xaccTransOrder instead.  Some          *
 *   if the algorithms used by xacc depend on the superior ordering *
 *   that xaccTransOrder provides.                                  *
 *                                                                  * 
 * Args:   date1 - the first date to look at                        *
 *         date2 - the second date in the comparison                * 
 * Return: < 0 if date1<date2, == 0 if date1==date2, > 0 otherwise  *
\********************************************************************/

#ifndef __XACC_DATE_H__
#define __XACC_DATE_H__

#include "config.h"

typedef struct _date {
  int year;
  int month;
  int day;
} Date;

/** PROTOTYPES ******************************************************/
void   sprtDate (char * buff, int day, int month, int year);

void   adjustDay( Date *date, int adj );
void   adjustMonth( Date *date, int adj );
Date*  todaysDate( Date *date );

/** daysInMonth includes "year" parameter in order to accommodate leap year
**/
int    daysInMonth( int month , int year );
int    datecmp( Date *date1, Date *date2 );

#define DATE_SHORT 0
#define DATE_YEAR  1
#define DATE_FULL  2

int    sscandate( const char *in_string, Date *date, int flags);


/** GLOBALS *********************************************************/

#endif /* __XACC_DATE_H__ */
