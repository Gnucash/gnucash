/***************************************************************************
 *            gnc-date.h (to be renamed qofdate.h)
 *
 *  Copyright (C) 1997 Robin D. Clark <rclark@cs.hmc.edu>
 *  Copyright (C) 1998-2000, 2003 Linas Vepstas <linas@linas.org>
 *  Copyright  2005  Neil Williams <linux@codehelp.co.uk>
 ****************************************************************************/
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
 * 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652       *
 * Boston, MA  02110-1301,  USA       gnu@gnu.org                   *
\********************************************************************/
/** @addtogroup Date
    Utility functions to handle date and time (adjusting, getting
    the current date, printing the date and time, etc.)

    Overall, this file is quite a mess.  Note, however, that other
    applications, besides just GnuCash, use this file.  In particular,
    GnoTime (gttr.sourcefore.net) uses this file, and this file is
    formally a part of QOF (qof.sourceforge.net).

    An important note about time-keeping:  The general goal of any
    program that works with numeric time values SHOULD BE to always
    stores and use UNIVERSAL TIME internally.  Universal time is the
    'one true time' that is independent of one's location on planet
    Earth.  It is measured in seconds from midnight January 1, 1970
    in localtime-Grenwich (GMT).  If one wants to display the local
    time, then the display-print routine should make all final
    tweaks to print the local time.   The local time *must not* be
    kept as a numeric value anywhere in the program.   If one wants
    to parse a user's input string as if it were local time, then
    the output of the parse routine MUST BE universal time.
    A sane program must never ever store (to file or db) a time
    that is not Universal Time.  Break these rules, and you will
    rue the day...

    \warning HACK ALERT -- the scan and print routines should probably be moved
    to somewhere else. The engine really isn't involved with things
    like printing formats. This is needed mostly by the GUI and so on.
    If a file-io backend needs date handling, it should do it itself,
    instead of depending on the routines here.

	(to be renamed qofdate.h in libqof2.)

    @author Copyright (C) 1997 Robin D. Clark <rclark@cs.hmc.edu>
    @author Copyright (C) 1998-2001,2003 Linas Vepstas <linas@linas.org>
*/

/** @{
    @file gnc-date.h
    @brief Date and Time handling routines
*/

#ifndef GNC_DATE_H
#define GNC_DATE_H

#include <glib-object.h>
#include <time.h>

/** @name GValue
  @{
*/
GType timespec_get_type( void );
#define GNC_TYPE_TIMESPEC (timespec_get_type ())

/** @} */
/** The default date format for use with strftime. */
extern const char *gnc_default_strftime_date_format;

/** The maximum length of a string created by the date printers */
#define MAX_DATE_LENGTH 31

/** Constants *******************************************************/
/** \brief UTC date format string.

Timezone independent, date and time inclusive, as used in the QSF backend.
The T and Z characters are from xsd:dateTime format in coordinated universal time, UTC.
You can reproduce the string from the GNU/Linux command line using the date utility:
date -u +%Y-%m-%dT%H:M:SZ = 2004-12-12T23:39:11Z The datestring must be timezone independent
and include all specified fields. Remember to use gmtime() NOT localtime()!
*/

#define QOF_UTC_DATE_FORMAT     "%Y-%m-%dT%H:%M:%SZ"

/** Enum for determining a date format */
typedef enum
{
    QOF_DATE_FORMAT_US,       /**< United states: mm/dd/yyyy */
    QOF_DATE_FORMAT_UK,       /**< Britain: dd/mm/yyyy */
    QOF_DATE_FORMAT_CE,       /**< Continental Europe: dd.mm.yyyy */
    QOF_DATE_FORMAT_ISO,      /**< ISO: yyyy-mm-dd */
    QOF_DATE_FORMAT_UTC,      /**< UTC: 2004-12-12T23:39:11Z */
    QOF_DATE_FORMAT_LOCALE,   /**< Take from locale information */
    QOF_DATE_FORMAT_CUSTOM    /**< Used by the check printing code */
} QofDateFormat;

#define DATE_FORMAT_FIRST QOF_DATE_FORMAT_US
#define DATE_FORMAT_LAST  QOF_DATE_FORMAT_LOCALE

/** \deprecated qof_date_format_get_format has been replaced
by qof_date_text_format_get_string */
#define qof_date_format_get_format qof_date_text_format_get_string

/**
 * This is how to format the month, as a number, an abbreviated string,
 * or the full name.
 */
typedef enum
{
    GNCDATE_MONTH_NUMBER,
    GNCDATE_MONTH_ABBREV,
    GNCDATE_MONTH_NAME
} GNCDateMonthFormat;


/** \name String / DateFormat conversion. */
//@{

/** \brief The string->value versions return FALSE on success and TRUE on failure */
const gchar* gnc_date_dateformat_to_string(QofDateFormat format);

/** \brief Converts the date format to a printable string.

Note the reversed return values!
@return FALSE on success, TRUE on failure.
*/
gboolean gnc_date_string_to_dateformat(const gchar* format_string,
                                       QofDateFormat *format);

const gchar* gnc_date_monthformat_to_string(GNCDateMonthFormat format);

/** \brief Converts the month format to a printable string.

Note the reversed return values!
@return FALSE on success, TRUE on failure.
*/
gboolean gnc_date_string_to_monthformat(const gchar *format_string,
                                        GNCDateMonthFormat *format);
// @}

/* Datatypes *******************************************************/

/** \brief Use a 64-bit unsigned int timespec
 *
 * struct timespec64 is just like the unix 'struct timespec' except
 * that we use a 64-bit
 * unsigned int to store the seconds.  This should adequately cover
 * dates in the distant future as well as the distant past, as long as
 * they're not more than a couple dozen times the age of the universe.
 * Values of this type can range from -9,223,372,036,854,775,808 to
 * 9,223,372,036,854,775,807.
 */

#ifndef SWIG   /* swig 1.1p5 can't hack the long long type */
struct timespec64
{
    gint64 tv_sec;
    glong tv_nsec;
};
#endif /* SWIG */

/** The Timespec is just like the unix 'struct timespec'
 * except that we use a 64-bit unsigned int to
 * store the seconds.  This should adequately cover dates in the
 * distant future as well as the distant past, as long as they're not
 * more than a couple dozen times the age of the universe
 * Values of this type can range from -9,223,372,036,854,775,808 to
 * 9,223,372,036,854,775,807.
 */
typedef struct timespec64 Timespec;


/* Prototypes ******************************************************/

/** \name Timespec functions */
// @{
/** strict equality */
gboolean timespec_equal(const Timespec *ta, const Timespec *tb);

/** comparison:  if (ta < tb) -1; else if (ta > tb) 1; else 0; */
gint      timespec_cmp(const Timespec *ta, const Timespec *tb);

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
Timespec gnc_dmy2timespec (gint day, gint month, gint year);

/** Same as gnc_dmy2timespec, but last second of the day */
Timespec gnc_dmy2timespec_end (gint day, gint month, gint year);

/** The gnc_iso8601_to_timespec_gmt() routine converts an ISO-8601 style
 *    date/time string to Timespec.  Please note that ISO-8601 strings
 *    are a representation of Universal Time (UTC), and as such, they
 *    'store' UTC.  To make them human readable, they show timezone
 *    information along with a local-time string.  But fundamentally,
 *    they *are* UTC.  Thus, thir routine takes a UTC input, and
 *    returns a UTC output.
 *
 *    For example: 1998-07-17 11:00:00.68-0500
 *    is 680 milliseconds after 11 o'clock, central daylight time
 *    It is also 680 millisecs after 16:00:00 hours UTC.
 *    \return The universl time.
 *
 * XXX Caution: this routine does not handle strings that specify
 * times before January 1 1970.
 */
Timespec gnc_iso8601_to_timespec_gmt(const gchar *);

/** The gnc_timespec_to_iso8601_buff() routine takes the input
 *    UTC Timespec value and prints it as an ISO-8601 style string.
 *    The buffer must be long enough to contain the NULL-terminated
 *    string (32 characters + NUL).  This routine returns a pointer
 *    to the null terminator (and can thus be used in the 'stpcpy'
 *    metaphor of string concatenation).
 *
 *    Please note that ISO-8601 strings are a representation of
 *    Universal Time (UTC), and as such, they 'store' UTC.  To make them
 *    human readable, they show timezone information along with a
 *    local-time string.  But fundamentally, they *are* UTC.  Thus,
 *    this routine takes a UTC input, and returns a UTC output.
 *
 *    The string generated by this routine uses the local timezone
 *    on the machine on which it is executing to create the timestring.
 */
gchar * gnc_timespec_to_iso8601_buff (Timespec ts, gchar * buff);

/** DOCUMENT ME! FIXME: Probably similar to xaccDMYToSec() this date
 * routine might return incorrect values for dates before 1970.  */
void gnc_timespec2dmy (Timespec ts, gint *day, gint *month, gint *year);

/** \warning hack alert XXX FIXME -- these date routines return incorrect
 * values for dates before 1970.  Most of them are good only up
 * till 2038.  This needs fixing ...
 *
 * XXX  This routine should be modified to assume that the
 * the user wanted the time at noon, localtime.  The returned
 * time_t should be seconds (at GMT) of the local noon-time.
*/
time_t xaccDMYToSec (gint day, gint month, gint year);

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
glong gnc_timezone (const struct tm *tm);
// @}

/* ------------------------------------------------------------------------ */
/** \name QofDateFormat functions */
// @{
/** The qof_date_format_get routine returns the date format that
 *  the date printing will use when printing a date, and the scaning
 *  routines will assume when parsing a date.
 * @returns: the one of the enumerated date formats.
 */
QofDateFormat qof_date_format_get(void);

/**
 * The qof_date_format_set() routine sets date format to one of
 *    US, UK, CE, OR ISO.  Checks to make sure it's a legal value.
 *    Args: QofDateFormat: enumeration indicating preferred format
 */
void qof_date_format_set(QofDateFormat df);

/** This function returns a strftime formatting string for printing an
 *  all numeric date (e.g. 2005-09-14).  The string returned is based
 *  upon the location specified.
 *
 *  @param df The date style (us, uk, iso, etc) that should be provided.
 *
 *  @return A formatting string that will print a date in the
 *  requested style  */
const gchar *qof_date_format_get_string(QofDateFormat df);

/** This function returns a strftime formatting string for printing a
 *  date using words and numbers (e.g. 2005-September-14).  The string
 *  returned is based upon the location specified.
 *
 *  @param df The date style (us, uk, iso, etc) that should be provided.
 *
 *  @return A formatting string that will print a date in the
 *  requested style  */
const gchar *qof_date_text_format_get_string(QofDateFormat df);
// @}

/** dateSeparator
 *    Return the field separator for the current date format
 *
 * Args:   none
 *
 * Return: date character
 *
 * Globals: global dateFormat value
 */
gchar dateSeparator(void);

/** \name Date Printing/Scanning functions
 */
// @{
/**
 * \warning HACK ALERT -- the scan and print routines should probably
 * be moved to somewhere else. The engine really isn't involved with
 * things like printing formats. This is needed mostly by the GUI and
 * so on.  If a file-io thing needs date handling, it should do it
 * itself, instead of depending on the routines here.
 */

/** qof_format_time takes a format specification in UTF-8 and a broken-down time,
 *  tries to call strftime with a sufficiently large buffer and, if successful,
 *  return a newly allocated string in UTF-8 for the printing result.
 *
 *  @param format A format specification in UTF-8.
 *
 *  @param tm A broken-down time.
 *
 *  @return A newly allocated string on success, or NULL otherwise.
 */
gchar *qof_format_time(const gchar *format, const struct tm *tm);

/** qof_strftime calls qof_format_time to print a given time and afterwards tries
 *  to put the result into a buffer of fixed size.
 *
 *  @param buf A buffer.
 *
 *  @param max The size of buf in bytes.
 *
 *  @param format A format specification in UTF-8.
 *
 *  @param tm A broken-down time.
 *
 *  @return The number of characters written, not include the null byte, if the
 *  complete string, including the null byte, fits into the buffer.  Otherwise 0.
 */
gsize qof_strftime(gchar *buf, gsize max, const gchar *format,
                   const struct tm *tm);

/** qof_print_date_dmy_buff
 *    Convert a date as day / month / year integers into a localized string
 *    representation
 *
 * Args:   buff - pointer to previously allocated character array; its size
 *                must be at lease MAX_DATE_LENTH bytes.
 *         len - length of the buffer, in bytes.
 *         day - day of the month as 1 ... 31
 *         month - month of the year as 1 ... 12
 *         year - year (4-digit)
 *
 * Returns: number of characters printed
 *
 * Globals: global dateFormat value
 **/
size_t qof_print_date_dmy_buff (gchar * buff, size_t buflen, int day, int month, int year);

/** Convenience: calls through to qof_print_date_dmy_buff(). **/
size_t qof_print_date_buff (char * buff, size_t buflen, time_t secs);

/** Convenience; calls through to qof_print_date_dmy_buff(). **/
size_t qof_print_gdate(char *buf, size_t bufflen, const GDate *gd);

/** Convenience; calls through to qof_print_date_dmy_buff().
 *  Return: string, which should be freed when no longer needed.
 * **/
char * qof_print_date (time_t secs);

/** Convenience; calls through to qof_print_date_dmy_buff().
 *  Return: static global string.
 *  \warning This routine is not thread-safe, because it uses a single
 *      global buffer to store the return value.  Use qof_print_date_buff()
 *      or qof_print_date() instead.
 * **/
const char * gnc_print_date(Timespec ts);

/* ------------------------------------------------------------------ */
/* time printing utilities */

/** The qof_print_time_buff() routine prints only the hour-part of the date.
 *    Thus, if secs is  ...
 *    Returns the number of bytes printed.
 */

size_t qof_print_time_buff (char * buff, size_t len, time_t secs);
size_t qof_print_date_time_buff (char * buff, size_t len, time_t secs);

/* ------------------------------------------------------------------ */
/** The xaccDateUtilGetStamp() routine will take the given time in
 *  seconds and return a buffer containing a textual for the date.
 *  @param thyme The time in seconds to convert.
 *  @return A pointer to the generated string.
 *  @note The caller owns this buffer and must free it when done. */
char * xaccDateUtilGetStamp (time_t thyme);

/** qof_scan_date
 *    Convert a string into  day / month / year integers according to
 *    the current dateFormat value.
 *
 * Args:   buff - pointer to date string
 *         day -  will store day of the month as 1 ... 31
 *         month - will store month of the year as 1 ... 12
 *         year - will store the year (4-digit)
 *
 * Return: TRUE if the string seemed to be a valid date; else FALSE.
 *
 * Globals: uses global dateFormat value to assist in parsing.
 */
gboolean qof_scan_date (const char *buff, int *day, int *month, int *year);

/** as above, but returns seconds */
gboolean qof_scan_date_secs (const char *buff, time_t *secs);

// @}
/** \name Date Start/End Adjustment routines
 * Given a time value, adjust it to be the beginning or end of that day.
 */
// @{

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
 *  seconds and adjust it to the last second of that day. */
time_t gnc_timet_get_day_start(time_t time_val);

/** The gnc_timet_get_day_end() routine will take the given time in
 *  seconds and adjust it to the last second of that day. */
time_t gnc_timet_get_day_end(time_t time_val);

/** Get the numerical last date of the month. (28, 29, 30, 31) */
int date_get_last_mday(const struct tm *tm);

/** Is the mday field the last day of the specified month.*/
gboolean date_is_last_mday(const struct tm *tm);

/** \deprecated Use date_get_last_mday() */
int gnc_date_my_last_mday (int month, int year);
/** DOCUMENT ME! Probably the same as date_get_last_mday() */
int gnc_timespec_last_mday (Timespec ts);
// @}

/* ======================================================== */

/** \name Today's Date */
// @{
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
char * xaccDateUtilGetStampNow (void);

#define MIN_BUF_LEN 10
/**
 * Localized DOW abbreviation.
 * @param buf_len at least MIN_BUF_LEN
 * @param dow struct tm semantics: 0=sunday .. 6=saturday
 **/
void gnc_dow_abbrev(gchar *buf, int buf_len, int dow);

//@}
//@}
#endif /* GNC_DATE_H */
