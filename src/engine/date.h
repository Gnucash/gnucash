/********************************************************************\
 * date.h -- utility functions to handle the date (adjusting, get   * 
 *           current date, etc.) for GnuCash                        *
 * Copyright (C) 1997 Robin D. Clark (rclark@cs.hmc.edu)            *
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
\********************************************************************/

/* hack alert -- the scan and print routines should probably be moved
 * to somewhere else. The engine really isn't involved with things
 * like printing formats. This is needed mostly by the GUI and so on.
 * If a file-io thing needs date handling, it should do it itself,
 * instead of depending on the routines here. */

#ifndef __XACC_DATE_H__
#define __XACC_DATE_H__

#include <time.h>


/** Constants *******************************************************/

typedef enum
{
  DATE_FORMAT_US,       /* United states: mm/dd/yyyy */
  DATE_FORMAT_UK,       /* Britain: dd/mm/yyyy */
  DATE_FORMAT_CE,       /* Continental Europe: dd.mm.yyyy */
  DATE_FORMAT_ISO,      /* ISO: yyyy-mm-dd */
  DATE_FORMAT_LOCALE    /* Take from locale information */
} DateFormat;

#define DATE_FORMAT_FIRST DATE_FORMAT_US
#define DATE_FORMAT_LAST  DATE_FORMAT_LOCALE

/* the maximum length of a string created by the date printers */
#define MAX_DATE_LENGTH 11


/** Datatypes *******************************************************/

/* struct timespec64 is just like timespec except that we use a 64-bit
 * signed int to store the seconds.  This should adequately cover
 * dates in the distant future as well as the distant past, as long as
 * they're not more than a couple dozen times the age of the universe.
 * Note that both gcc and the IBM Toronto xlC compiler (aka CSet,
 * VisualAge, etc) correctly handle long long as a 64 bit quantity,
 * even on the 32-bit Intel x86 and PowerPC architectures.  I'm
 * assuming that all the other modern compilers are clean on this
 * issue too. */

struct timespec64
{
   long long int tv_sec;     
   long int tv_nsec;
};

typedef struct timespec64 Timespec;


/** Prototypes ******************************************************/

/* strict equality */
gboolean timespec_equal(const Timespec *ta, const Timespec *tb);
/* comparison:  if (ta < tb) -1; else if (ta > tb) 1; else 0; */
int      timespec_cmp(const Timespec *ta, const Timespec *tb);

/* difference between ta and tb, results are normalised
 * ie tv_sec and tv_nsec of the result have the same size
 * abs(result.tv_nsec) <= 1000000000
 */

Timespec timespec_diff(const Timespec *ta, const Timespec *tb);

/*
 * absolute value, also normalised
 */
Timespec timespec_abs(const Timespec *t);

void setDateFormat(DateFormat df);

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
void printDate (char * buff, int day, int month, int year);
void printDateSecs (char * buff, time_t secs);

char * xaccPrintDateSecs (time_t secs);
const char * gnc_print_date(Timespec ts);

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
void scanDate (const char *buff, int *day, int *month, int *year);

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
char dateSeparator(void);

/*
 * hack alert XXX FIXME -- these date routines return incorrect
 * values for dates before 1970.  Most of them are good only up 
 * till 2038.  This needs fixing ...
 */

time_t xaccDMYToSec (int day, int month, int year);
time_t xaccScanDateS (const char *buff);

/* Convert a day, month, and year to a Timespec */
Timespec gnc_dmy2timespec (int day, int month, int year);

/* Same as gnc_dmy2timespec, but last second of the day */
Timespec gnc_dmy2timespec_end (int day, int month, int year);

/* The gnc_iso8601_to_timespec_xxx() routines converts an ISO-8601 style 
 *    date/time string to Timespec.
 *    For example: 1998-07-17 11:00:00.68-05 
 *    is 680 milliseconds after 11 o'clock, central daylight time 
 *    The _gmt() routine returns the time in gmt. The _local() routine
 *    returns the local time.
 *
 * The gnc_timespec_to_iso8601_buff() routine prints a Timespec
 *    as an ISO-8601 style string.  The buffer must be long enough
 *    to contain the string.  The string is null-terminated. This
 *    routine returns a pointer to the null terminator (and can 
 *    thus be used in the 'stpcpy' metaphor of string concatenation).
 */
Timespec gnc_iso8601_to_timespec_local(const char *);
Timespec gnc_iso8601_to_timespec_gmt(const char *);
char * gnc_timespec_to_iso8601_buff (Timespec ts, char * buff);

#endif /* __XACC_DATE_H__ */
