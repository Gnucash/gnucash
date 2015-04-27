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

class GncDateImpl;
class GncDateTimeImpl;
using time64 = int64_t;

class GncDate
{
public:
/** Construct a GncDate representing the current day.
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
    ~GncDate();
/** Set the date object to the computer clock's current day. */
    void today();
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
 * differently depending on the timezone, which can shif the displayed
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
    GncDateTime(const struct tm tm);
    ~GncDateTime();
    void now();
    explicit operator time64() const;
    explicit operator struct tm() const;
    long offset() const;
    bool isnull (void) { return m_impl == nullptr; }
    std::string format(const char* format) const;

private:
    std::unique_ptr<GncDateTimeImpl> m_impl;
};

#endif // __GNC_DATETIME_HPP__
