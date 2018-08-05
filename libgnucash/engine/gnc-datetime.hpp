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
#include <vector>

typedef struct
{
    int year;  //1400-9999
    int month; //1-12
    int day; //1-31
} ymd;

enum DayPart : int {
    start,  // 00:00
    neutral,  // 10:59
    end,  // 23:59
};

class GncDateTimeImpl;
class GncDateImpl;
class GncDate;
using time64 = int64_t;
constexpr const time64 MINTIME = -17987443200;
constexpr const time64 MAXTIME = 253402214400;

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
 * @param time Seconds from the POSIX epoch.
 * @exception std::invalid_argument if the year is outside the constraints.
 */
    GncDateTime(const time64 time);
/** Construct a GncDateTime in the current timezone representing the
 * standard struct tm provided.
 * @param tm A C-standard struct tm representing the date and
 * time. Note that the timezone and offset are ignored on those
 * systems which include them in struct tm.
 * @exception std::invalid_argument if the year is outside the constraints.
 */
    GncDateTime(const struct tm tm);
/** Construct a GncDateTime from a GncDate. As a GncDate doesn't contain time
 * information, the time will be set depending on the second parameter
 * to start of day, neutral or end of day.
 * @param date A GncDate representing a date.
 * @param part An optinoal DayPart indicating which time to use in the conversion.
 * This can be "DayPart::start" for start of day (00:00 local time),
 *             "DayPart::neutral" for a neutral time (10:59 UTC, chosen to have the
 *              least chance of date changes when crossing timezone borders),
 *             "DayPart::end" for end of day (23:59 local time).
 * If omitted part defaults to DayPart::neutral.
 * Note the different timezone used for DayPart::neutral compared to the other
 * two options!
 * @exception std::invalid_argument if the year is outside the constraints.
 */
    GncDateTime(const GncDate& date, DayPart part = DayPart::neutral);
/** Construct a GncDateTime
 * @param str A string representing the date and time in some
 * recognizable format. Note that if a timezone is not specified the
 * default is UTC, not the local one.
 * @exception std::invalid_argument if the year is outside the constraints.
 */
    GncDateTime(std::string str);
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
 *  @param format A cstr describing the way the date and time are
 *  presented. Code letters preceded with % stand in for arguments;
 *  most are the same as described in strftime(3), but there are a few
 *  differences. Consult the boost::date_time documentation.
 *  @return a std::string containing a representation of the date and time in
 *  the locale's time zone according to the format.
 */
    std::string format(const char* format) const;
/** Format the GncDateTime into a std::string in GMT
 *  @param format A cstr describing the way the date and time are
 *  presented. Code letters preceded with % stand in for arguments;
 *  most are the same as described in strftime(3), but there are a few
 *  differences. Consult the boost::date_time documentation.
 *  @return a std::string containing a representation of the date and time in
 *  GMT (timezone Z) according to the format.
 */
    std::string format_zulu(const char* format) const;

private:
    std::unique_ptr<GncDateTimeImpl> m_impl;
};

/** GnuCash DateFormat class
 *
 * A helper class to represent a date format understood
 * by the GncDate string/format constructor. Consumers
 * of this header file are not supposed to create
 * objects of this class themselves. Instead they
 * can get a list of the understood formats from the
 * GncDate::c_formats class variable and work with those.
 */

class GncDateFormat
{
public:
    /** Construct a GncDateFormat with a given format and corresponding
     * regular expression. This should only be used internally by the
     * GncDate implementation. Consumers should never construct a GncDateFormat
     * themselves!
     */
    GncDateFormat (const char* fmt, const char* re) :
    m_fmt(fmt), m_re(re) {}
    /** A string representing the format. */
    const std::string m_fmt;
private:
    /** Regular expression associated with the format string. This is to and
     * only be used internally by the gnc-datetime code.
     */
    const std::string m_re;

    friend class GncDateImpl;
};

/** GnuCash Date class
 *
 * The represented date is limited to the period
 * between 1400 and 9999 CE.
 */

class GncDate
{
    public:
        /** A vector with all the date formats supported by the string constructor.
         * The currently supported formats are:
         * "y-m-d" (including yyyymmdd)
         * "d-m-y" (including ddmmyyyy)
         * "m-d-y" (including mmddyyyy)
         * "d-m"   (including ddmm)
         * "m-d"   (including mmdd)
         *
         * Notes:
         * - while the format names are using a "-" as separator, the
         * regexes will accept any of "-/.' " and will also work for dates
         * without separators.
         * - the format strings are marked for translation so it is possible
         * to use a localized version of a format string using gettext. Example:
         * gettext(GncDate::c_formats[0])
         */
        static const std::vector<GncDateFormat> c_formats;
        /** Construct a GncDate representing the current day.
         */
        GncDate();
        /** Construct a GncDate representing the given year, month, and day in
         * the proleptic Gregorian calendar.
         *
         * Years are constrained to be from 1400 - 9999 CE inclusive. Dates
         * will be normalized if the day or month values are outside of the
         * normal ranges. e.g. 1994, -3, 47 will be normalized to 1993-10-17.
         *
         * @param year The year in the Common Era.
         * @param month The month, where 1 is January and 12 is December.
         * @param day The day of the month, beginning with 1.
         * @exception std::invalid_argument if the calculated year is outside
         * of the constrained range.
         */
        GncDate(int year, int month, int day);
        /** Construct a GncDate by parsing a string assumed to be in the format
         * passed in.
         *
         * The currently recognized formats are d-m-y, m-d-y, y-m-d, m-d, d-m.
         * Note while the format descriptions use "-" as separator any of
         * "-" (hyphen), "/" (slash), "'" (single quote), " " (space) or
         * "." will be accepted.
         *
         * @param str The string to be interpreted.
         * @param fmt The expected date format of the string passed in.
         * @exception std::invalid_argument if
         * - the string couldn't be parsed using the provided format
         * - any of the date components is outside of its limit
         *   (like month being 13, or day being 31 in February)
         * - fmt doesn't specify a year, yet a year was found in the string
         */
        GncDate(const std::string str, const std::string fmt);
        /** Construct a GncDate from a GncDateImpl.
         */
        GncDate(std::unique_ptr<GncDateImpl> impl);
        /** Copy constructor.
         */
        GncDate(const GncDate&);
        /** Move constructor.
         */
        GncDate(GncDate&&);
        /** Default destructor.
         */
        ~GncDate();
        /** Copy assignment operator.
         */
        GncDate& operator=(const GncDate&);
        /** Move assignment operator.
         */
        GncDate& operator=(GncDate&&);
        /** Set the date object to the computer clock's current day. */
        void today();
        /** Get the year, month, and day from the date as a ymd.
         *  @return ymd struct
         */
        ymd year_month_day() const;
        /** Format the GncDate into a std::string
         *  @param format A cstr describing the way the date and time are
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

    friend GncDateTime::GncDateTime(const GncDate&, DayPart);
    friend bool operator<(const GncDate&, const GncDate&);
    friend bool operator>(const GncDate&, const GncDate&);
    friend bool operator==(const GncDate&, const GncDate&);
    friend bool operator<=(const GncDate&, const GncDate&);
    friend bool operator>=(const GncDate&, const GncDate&);
    friend bool operator!=(const GncDate&, const GncDate&);
};

/**@{
 *  Standard comparison operators working on GncDate objects.
 */
bool operator<(const GncDate& a, const GncDate& b);
bool operator>(const GncDate& a, const GncDate& b);
bool operator==(const GncDate& a, const GncDate& b);
bool operator<=(const GncDate& a, const GncDate& b);
bool operator>=(const GncDate& a, const GncDate& b);
bool operator!=(const GncDate& a, const GncDate& b);
/**@}*/

#endif // __GNC_DATETIME_HPP__
