/********************************************************************\
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
/** @addtogroup Date
    @{ */
/** @file date.h 
    @brief Date handling routines  
    *
    Utility functions to handle the date (adjusting, get 
    current date, etc.) 

    \warning HACK ALERT -- the scan and print routines should probably be moved
    to somewhere else. The engine really isn't involved with things
    like printing formats. This is needed mostly by the GUI and so on.
    If a file-io thing needs date handling, it should do it itself,
    instead of depending on the routines here. 
    *
    @author Copyright (C) 1997 Robin D. Clark
    @author Copyright (C) 1998,1999,2000 Linas Vepstas <linas@linas.org>
*/

#ifndef XACC_DATE_H
#define XACC_DATE_H

#include <glib.h>
#include <time.h>


/** Constants *******************************************************/

/** Enum for determining a date format */
typedef enum
{
  DATE_FORMAT_US,       /**< United states: mm/dd/yyyy */
  DATE_FORMAT_UK,       /**< Britain: dd/mm/yyyy */
  DATE_FORMAT_CE,       /**< Continental Europe: dd.mm.yyyy */
  DATE_FORMAT_ISO,      /**< ISO: yyyy-mm-dd */
  DATE_FORMAT_LOCALE,   /**< Take from locale information */
  DATE_FORMAT_CUSTOM    /**< Used by the check printing code */
} DateFormat;

#define DATE_FORMAT_FIRST DATE_FORMAT_US
#define DATE_FORMAT_LAST  DATE_FORMAT_LOCALE

/** The maximum length of a string created by the date printers */
#define MAX_DATE_LENGTH 11


/** Datatypes *******************************************************/

/** struct timespec64 is just like the unix 'struct timespec' except 
 * that we use a 64-bit
 * signed int to store the seconds.  This should adequately cover
 * dates in the distant future as well as the distant past, as long as
 * they're not more than a couple dozen times the age of the universe.
 * Note that both gcc and the IBM Toronto xlC compiler (aka CSet,
 * VisualAge, etc) correctly handle long long as a 64 bit quantity,
 * even on the 32-bit Intel x86 and PowerPC architectures.  I'm
 * assuming that all the other modern compilers are clean on this
 * issue too. */

#ifndef SWIG   /* swig 1.1p5 can't hack the long long type */
struct timespec64
{
   long long int tv_sec;     
   long int tv_nsec;
};
#endif /* SWIG */

/** The Timespec is just like the unix 'struct timespec' 
 * except that we use a 64-bit signed int to
 * store the seconds.  This should adequately cover dates in the
 * distant future as well as the distant past, as long as they're not
 * more than a couple dozen times the age of the universe.  Note that
 * both gcc and the IBM Toronto xlC compiler (aka CSet, VisualAge,
 * etc) correctly handle long long as a 64 bit quantity, even on the
 * 32-bit Intel x86 and PowerPC architectures.  I'm assuming that all
 * the other modern compilers are clean on this issue too. */
typedef struct timespec64 Timespec;


/** Prototypes ******************************************************/

/** @name Timespec functions */
/*@{*/
/** strict equality */
gboolean timespec_equal(const Timespec *ta, const Timespec *tb);

/** comparison:  if (ta < tb) -1; else if (ta > tb) 1; else 0; */
int      timespec_cmp(const Timespec *ta, const Timespec *tb);

/** difference between ta and tb, results are normalised
 * ie tv_sec and tv_nsec of the result have the same size
 * abs(result.tv_nsec) <= 1000000000 */
Timespec timespec_diff(const Timespec *ta, const Timespec *tb);

/** absolute value, also normalised */
Timespec timespec_abs(const Timespec *t);

/** convert a timepair on a certain day (localtime) to
 * the timepair representing midday on that day */
Timespec timespecCanonicalDayTime(Timespec t);

/** Turns a time_t into a Timespec */
void timespecFromTime_t( Timespec *ts, time_t t );

/** Turns a Timespec into a time_t */
time_t timespecToTime_t (Timespec ts);

/** Convert a day, month, and year to a Timespec */
Timespec gnc_dmy2timespec (int day, int month, int year);

/** Same as gnc_dmy2timespec, but last second of the day */
Timespec gnc_dmy2timespec_end (int day, int month, int year);

/** The gnc_iso8601_to_timespec_local() routine converts an ISO-8601 style 
 *    date/time string to Timespec.
 *    For example: 1998-07-17 11:00:00.68-05 
 *    is 680 milliseconds after 11 o'clock, central daylight time 
 *    \return The time in local time.*/
Timespec gnc_iso8601_to_timespec_local(const char *);

/** The gnc_iso8601_to_timespec_gmt() routine converts an ISO-8601 style 
 *    date/time string to Timespec.
 *    For example: 1998-07-17 11:00:00.68-05 
 *    is 680 milliseconds after 11 o'clock, central daylight time 
 *    \return The time in gmt. */
Timespec gnc_iso8601_to_timespec_gmt(const char *);

/** The gnc_timespec_to_iso8601_buff() routine prints a Timespec
* as an ISO-8601 style string.  The buffer must be long enough
* to contain the string.  The string is null-terminated. This
* routine returns a pointer to the null terminator (and can 
* thus be used in the 'stpcpy' metaphor of string concatenation).*/
char * gnc_timespec_to_iso8601_buff (Timespec ts, char * buff);

/** DOCUMENT ME! FIXME: Probably similar to xaccDMYToSec() this date
 * routine might return incorrect values for dates before 1970.  */
void gnc_timespec2dmy (Timespec ts, int *day, int *month, int *year);
/*@}*/


/** Add a number of months to a time value and normalize.  Optionally
 * also track the last day of the month, i.e. 1/31 -> 2/28 -> 3/30. */
void date_add_months (struct tm *tm, int months, gboolean track_last_day);

/** \warning hack alert XXX FIXME -- these date routines return incorrect
 * values for dates before 1970.  Most of them are good only up 
 * till 2038.  This needs fixing ... */
time_t xaccDMYToSec (int day, int month, int year);

/** The gnc_timezone function returns the number of seconds *west*
 * of UTC represented by the tm argument, adjusted for daylight
 * savings time.
 *
 * This function requires a tm argument returned by localtime or set
 * by mktime. This is a strange function! It requires that localtime
 * or mktime be called before use. Subsequent calls to localtime or
 * mktime *may* invalidate the result! The actual contents of tm *may*
 * be used for both timezone offset and daylight savings time, or only
 * daylight savings time! Timezone stuff under unix is not
 * standardized and is a big mess.
 */
long int gnc_timezone (struct tm *tm);


/** @name DateFormat functions */
/*@{*/
/** DOCUMENT ME! */
DateFormat getDateFormat(void);
/** DOCUMENT ME! */
void setDateFormat(DateFormat df);
/** DOCUMENT ME! */
const gchar *getDateFormatString(DateFormat df);
/** DOCUMENT ME! */
const gchar *getDateTextFormatString(DateFormat df);
/*@}*/

/** @name Date Printing/Scanning functions 
 *
 * \warning HACK ALERT -- the scan and print routines should probably
 * be moved to somewhere else. The engine really isn't involved with
 * things like printing formats. This is needed mostly by the GUI and
 * so on.  If a file-io thing needs date handling, it should do it
 * itself, instead of depending on the routines here.
 */
/*@{*/
/** printDate
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
 **/
void printDate (char * buff, int day, int month, int year);

/** convenience: calls through to printDate(). **/
void printDateSecs (char * buff, time_t secs);

/** Convenience; calls through to printDate(). **/
void printGDate( char *buf, GDate *gd );

/** DOCUMENT ME! */
char * xaccPrintDateSecs (time_t secs);

/** DOCUMENT ME! */
const char * gnc_print_date(Timespec ts);

/** The xaccDateUtilGetStamp() routine will take the given time in
 *  seconds and return a buffer containing a textual for the date.
 *  @param thyme The time in seconds to convert.
 *  @return A pointer to the generated string.
 *  @note The caller owns this buffer and must free it when done. */
char *xaccDateUtilGetStamp (time_t thyme);

/** dateSeparator
 *    Return the field separator for the current date format
 *
 * Args:   none
 *
 * Return: date character
 *
 * Globals: global dateFormat value
 */
char dateSeparator(void);

/** scanDate
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

/** \warning hack alert XXX FIXME -- these date routines return incorrect
 * values for dates before 1970.  Most of them are good only up 
 * till 2038.  This needs fixing ... */
time_t xaccScanDateS (const char *buff);
/*@}*/


/** @name Date Start/End Adjustment routines
 * Given a time value, adjust it to be the beginning or end of that day.
 */
/** @{ */

/** The gnc_tm_set_day_start() inline routine will set the appropriate
 *  fields in the struct tm to indicate the first second of that day.
 *  This routine assumes that the contents of the data structure is
 *  already in normalized form. */
static inline
void gnc_tm_set_day_start (struct tm *tm)
{
  /* First second of the day */
  tm->tm_hour = 0;
  tm->tm_min = 0;
  tm->tm_sec = 0;
  tm->tm_isdst = -1;
}

/** The gnc_tm_set_day_start() inline routine will set the appropriate
 *  fields in the struct tm to indicate noon of that day.  This
 *  routine assumes that the contents of the data structure is already
 *  in normalized form.*/
static inline
void gnc_tm_set_day_middle (struct tm *tm)
{
  /* First second of the day */
  tm->tm_hour = 12;
  tm->tm_min = 0;
  tm->tm_sec = 0;
  tm->tm_isdst = -1;
}

/** The gnc_tm_set_day_start() inline routine will set the appropriate
 *  fields in the struct tm to indicate the last second of that day.
 *  This routine assumes that the contents of the data structure is
 *  already in normalized form.*/
static inline
void gnc_tm_set_day_end (struct tm *tm)
{
  /* Last second of the day */
  tm->tm_hour = 23;
  tm->tm_min = 59;
  tm->tm_sec = 59;
  tm->tm_isdst = -1;
}

/** The gnc_tm_get_day_start() routine will convert the given time in
 *  seconds to the struct tm format, and then adjust it to the
 *  first second of that day. */
void   gnc_tm_get_day_start(struct tm *tm, time_t time_val);

/** The gnc_tm_get_day_end() routine will convert the given time in
 *  seconds to the struct tm format, and then adjust it to the
 *  last second of that day. */
void   gnc_tm_get_day_end(struct tm *tm, time_t time_val);

/** The gnc_timet_get_day_start() routine will take the given time in
 *  seconds and first it to the last second of that day. */
time_t gnc_timet_get_day_start(time_t time_val);

/** The gnc_timet_get_day_end() routine will take the given time in
 *  seconds and adjust it to the last second of that day. */
time_t gnc_timet_get_day_end(time_t time_val);

/** Get the numerical last date of the month. (28, 29, 30, 31) */
int date_get_last_mday(struct tm *tm);

/** Is the mday field the last day of the specified month.*/
gboolean date_is_last_mday(struct tm *tm);

/** DOCUMENT ME! Probably the same as date_get_last_mday() */
int gnc_date_my_last_mday (int month, int year);
/** DOCUMENT ME! Probably the same as date_get_last_mday() */
int gnc_timespec_last_mday (Timespec ts);
/*@}*/

/* ======================================================== */

/** @name Today's Date */
/*@{*/
/** The gnc_tm_get_today_start() routine takes a pointer to a struct
 *  tm and fills it in with the first second of the today. */
void   gnc_tm_get_today_start(struct tm *tm);

/** The gnc_tm_get_today_end() routine takes a pointer to a struct
 *  tm and fills it in with the last second of the today. */
void   gnc_tm_get_today_end(struct tm *tm);

/** The gnc_timet_get_today_start() routine returns a time_t value
 *  corresponding to the first second of today. */
time_t gnc_timet_get_today_start(void);

/** The gnc_timet_get_today_end() routine returns a time_t value
 *  corresponding to the last second of today. */
time_t gnc_timet_get_today_end(void);

/** The xaccDateUtilGetStampNow() routine returns the current time in
 *  seconds in textual format.
 *  @return A pointer to the generated string.
 *  @note The caller owns this buffer and must free it when done. */
char *xaccDateUtilGetStampNow (void);
/*@}*/

#endif /* XACC_DATE_H */
/** @} */
