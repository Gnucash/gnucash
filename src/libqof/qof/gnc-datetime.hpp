/********************************************************************\
 * gnc-datetime.cpp -- Date and Time classes for GnuCash            *
 *                                                                  *
 * Copyright 2015 John Ralls <jralls@ceridwen.us>                   *
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
 * 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652       *
 * Boston, MA  02110-1301,  USA       gnu@gnu.org                   *
 *                                                                  *
\********************************************************************/

#ifndef __GNC_DATETIME_HPP__
#define  __GNC_DATETIME_HPP__

#include <cstdint>
#include <memory>
#include <string>

typedef struct
{
    int year;  //1400-9999
    int month; //1-12
    int day; //1-31
} ymd;

class GncDateImpl;
class GncDateTimeImpl;
using time64 = int64_t;

class GncDate
{
public:/** Construct a GncDate representing the current day.
 */
    GncDate();;
/** Construct a GncDate representing the given year, month, and day in
 * the proleptic Gregorian calendar.
 *
 * Years are constrained to be from 1400 - 9999 CE inclusive. Dates
 * will be normalized if the day or month values are outside of the
 * normal ranges. e.g. 1994, -3, 47 will be normalized to 1993-10-17.
 *
 * @param year: The year in the Common Era.
 * @param month: The month, where 1 is January and 12 is December.
 * @param day: The day of the month, beginning with 1.
 * @exception std::invalid_argument if the calculated year is outside
 * of the constrained range.
 */
    GncDate(int year, int month, int day);
    GncDate(std::unique_ptr<GncDateImpl> impl);
    GncDate(GncDate&&);
    ~GncDate();
    GncDate& operator=(GncDate&&);
/** Set the date object to the computer clock's current day. */
    void today();
/** Get the year, month, and day from the date as a ymd.
    @return ymd struct
 */
    ymd year_month_day() const;
/** Format the GncDate into a std::string
 *  @param format: A cstr describing the way the date and time are
 *  presented. Code letters preceded with % stand in for arguments;
 *  most are the same as described in strftime(3), but there are a few
 *  differences. Consult the boost::date_time documentation.
 *  @return a std::string containing a representation of the date
 *  according to the format.
 */
    std::string format(const char* format);
/** Test that the Date has an implementation. */
    bool isnull (void) { return m_impl == nullptr; }

private:
    std::unique_ptr<GncDateImpl> m_impl;
};

/** GnuCash DateTime class
 *
 * Represents local time in the current timezone.
 * As with GncDate, the represented time is limited to the period
 * between 1400 and 9999 CE.
 *
 * Be careful when using times: A particular time is represented
 * differently depending on the timezone, which can shift the displayed
 * date. Accounting is generally not sensitive to the time of day, but
 * is sensitive to the recorded day. Since GncDates are not timezone
 * dependent they should be preferred for accounting entries.
 */

class GncDateTime
{
public:
/** Construct a GncDateTime representing the current time in the
 * current timezone.
 */
    GncDateTime();
/** Construct a GncDateTime in the current timezone representing the
 * timestamp as seconds from the POSIX epoch (1970-01-01T00:00:00UTC).
 * @param time: Seconds from the POSIX epoch.
 * @exception std::invalid_argument if the year is outside the constraints.
 */
    GncDateTime(const time64 time);
/** Construct a GncDateTime in the current timezone representing the
 * standard struct tm provided.
 * @param tm: A C-standard struct tm representing the date and
 * time. Note that the timezone and offset are ignored on those
 * systems which include them in struct tm.
 * @exception std::invalid_argument if the year is outside the constraints.
 */
    GncDateTime(const struct tm tm);
/** Construct a GncDateTime
 * @param str: A string representing the date and time in some
 * recognizable format. Note that if a timezone is not specified the
 * default is UTC, not the local one.
 * @exception std::invalid_argument if the year is outside the constraints.
 */
    GncDateTime(const std::string str);
    ~GncDateTime();
/** Set the GncDateTime to the date and time indicated in the computer's clock.
 */
    void now();
/** Cast the GncDateTime to a time64, seconds from the POSIX epoch. */
    explicit operator time64() const;
/** Cast the GncDateTime to a struct tm. Timezone field isn't filled.
 */
    explicit operator struct tm() const;
/** Obtain the UTC offset in seconds
 *  @return seconds difference between this local time and UTC. West
 *  is negative.
 */
    long offset()const;
/** Obtain a struct tm representing the time in UTC.
 * @return struct tm
 */
    struct tm utc_tm() const;
/** Obtain the date from the time, as a GncDate, in the current timezone.
 *  @return GncDate represented by the GncDateTime.
 */
    GncDate date() const;
/** Test if the GncDateTime has a member pointer. Testing only. */
    bool isnull (void) { return m_impl == nullptr; }
/** Format the GncDateTime into a std::string
 *  @param format: A cstr describing the way the date and time are
 *  presented. Code letters preceded with % stand in for arguments;
 *  most are the same as described in strftime(3), but there are a few
 *  differences. Consult the boost::date_time documentation.
 *  @return a std::string containing a representation of the date
 *  according to the format.
 */
    std::string format(const char* format) const;

private:
    std::unique_ptr<GncDateTimeImpl> m_impl;
};

#endif // __GNC_DATETIME_HPP__
