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

extern "C"
{
#include <config.h>
#include "platform.h"
}
#include <boost/date_time/gregorian/gregorian.hpp>
#include <boost/date_time/posix_time/posix_time.hpp>
#include <boost/date_time/local_time/local_time.hpp>
#include <boost/regex.hpp>
#include <libintl.h>
#include <map>
#include <memory>
#include <iostream>
#include <sstream>
#include <string>
#include <vector>
#include "gnc-timezone.hpp"
#include "gnc-datetime.hpp"

#define N_(string) string //So that xgettext will find it

using Date = boost::gregorian::date;
using Month = boost::gregorian::greg_month;
using PTime = boost::posix_time::ptime;
using LDT = boost::local_time::local_date_time;
using Duration = boost::posix_time::time_duration;
using LDTBase = boost::local_time::local_date_time_base<PTime, boost::date_time::time_zone_base<PTime, char>>;
using boost::date_time::not_a_date_time;
using time64 = int64_t;

static const TimeZoneProvider tzp;
// For converting to/from POSIX time.
static const PTime unix_epoch (Date(1970, boost::gregorian::Jan, 1),
        boost::posix_time::seconds(0));
static const TZ_Ptr utc_zone(new boost::local_time::posix_time_zone("UTC-0"));

/* To ensure things aren't overly screwed up by setting the nanosecond clock for boost::date_time. Don't do it, though, it doesn't get us anything and slows down the date/time library. */
#ifndef BOOST_DATE_TIME_HAS_NANOSECONDS
static constexpr auto ticks_per_second = INT64_C(1000000);
#else
static constexpr auto ticks_per_second = INT64_C(1000000000);
#endif

/* Vector of date formats understood by gnucash and corresponding regex
 * to parse each from an external source
 * Note: while the format names are using a "-" as separator, the
 * regexes will accept any of "-/.' " and will also work for dates
 * without separators.
 */
const std::vector<GncDateFormat> GncDate::c_formats ({
    GncDateFormat {
        N_("y-m-d"),
        "(?:"                                   // either y-m-d
        "(?<YEAR>[0-9]+)[-/.' ]+"
        "(?<MONTH>[0-9]+)[-/.' ]+"
        "(?<DAY>[0-9]+)"
        "|"                                     // or CCYYMMDD
        "(?<YEAR>[0-9]{4})"
        "(?<MONTH>[0-9]{2})"
        "(?<DAY>[0-9]{2})"
        ")"
    },
    GncDateFormat {
        N_("d-m-y"),
        "(?:"                                   // either d-m-y
        "(?<DAY>[0-9]+)[-/.' ]+"
        "(?<MONTH>[0-9]+)[-/.' ]+"
        "(?<YEAR>[0-9]+)"
        "|"                                     // or DDMMCCYY
        "(?<DAY>[0-9]{2})"
        "(?<MONTH>[0-9]{2})"
        "(?<YEAR>[0-9]{4})"
        ")"
    },
    GncDateFormat {
        N_("m-d-y"),
        "(?:"                                   // either m-d-y
        "(?<MONTH>[0-9]+)[-/.' ]+"
        "(?<DAY>[0-9]+)[-/.' ]+"
        "(?<YEAR>[0-9]+)"
        "|"                                     // or MMDDCCYY
        "(?<MONTH>[0-9]{2})"
        "(?<DAY>[0-9]{2})"
        "(?<YEAR>[0-9]{4})"
        ")"
    },
    // Note year is still checked for in the regexes below
    // This is to be able to raise an error if one is found for a yearless date format
    GncDateFormat {
        (N_("d-m")),
        "(?:"                                   // either d-m(-y)
        "(?<DAY>[0-9]+)[-/.' ]+"
        "(?<MONTH>[0-9]+)(?:[-/.' ]+"
        "(?<YEAR>[0-9]+))?"
        "|"                                     // or DDMM(CCYY)
        "(?<DAY>[0-9]{2})"
        "(?<MONTH>[0-9]{2})"
        "(?<YEAR>[0-9]+)?"
        ")"
    },
    GncDateFormat {
        (N_("m-d")),
        "(?:"                                   // either m-d(-y)
        "(?<MONTH>[0-9]+)[-/.' ]+"
        "(?<DAY>[0-9]+)(?:[-/.' ]+"
        "(?<YEAR>[0-9]+))?"
        "|"                                     // or MMDD(CCYY)
        "(?<MONTH>[0-9]{2})"
        "(?<DAY>[0-9]{2})"
        "(?<YEAR>[0-9]+)?"
        ")"
    }
});

/** Private implementation of GncDateTime. See the documentation for that class.
 */
static LDT
LDT_from_unix_local(const time64 time)
{
    try
    {
        PTime temp(unix_epoch.date(),
                   boost::posix_time::hours(time / 3600) +
                   boost::posix_time::seconds(time % 3600));
        auto tz = tzp.get(temp.date().year());
        return LDT(temp, tz);
    }
    catch(boost::gregorian::bad_year&)
    {
        throw(std::invalid_argument("Time value is outside the supported year range."));
    }
}

static LDT
LDT_from_struct_tm(const struct tm tm)
{
    Date tdate;
    Duration tdur;
    TZ_Ptr tz;

    try
    {
        tdate = boost::gregorian::date_from_tm(tm);
        tdur = boost::posix_time::time_duration(tm.tm_hour, tm.tm_min,
                                                 tm.tm_sec, 0);
        tz = tzp.get(tdate.year());
        LDT ldt(tdate, tdur, tz, LDTBase::EXCEPTION_ON_ERROR);
        return ldt;
    }
    catch(boost::gregorian::bad_year&)
    {
        throw(std::invalid_argument("Time value is outside the supported year range."));
    }
    catch(boost::local_time::time_label_invalid&)
    {
        throw(std::invalid_argument("Struct tm does not resolve to a valid time."));
    }
    catch(boost::local_time::ambiguous_result&)
    {
        /* We plunked down in the middle of a DST change. Try constructing the
         * LDT three hours later to get a valid result then back up those three
         * hours to have the time we want.
         */
        using boost::posix_time::hours;
        auto hour = tm.tm_hour;
        tdur += hours(3);
        LDT ldt(tdate, tdur, tz, LDTBase::NOT_DATE_TIME_ON_ERROR);
        if (ldt.is_special())
            throw(std::invalid_argument("Couldn't create a valid datetime."));
        ldt -= hours(3);
        return ldt;
    }
}

using TD = boost::posix_time::time_duration;

class GncDateTimeImpl
{
public:
    GncDateTimeImpl() : m_time(boost::local_time::local_sec_clock::local_time(tzp.get(boost::gregorian::day_clock::local_day().year()))) {}
    GncDateTimeImpl(const time64 time) : m_time(LDT_from_unix_local(time)) {}
    GncDateTimeImpl(const struct tm tm) : m_time(LDT_from_struct_tm(tm)) {}
    GncDateTimeImpl(const GncDateImpl& date, DayPart part = DayPart::neutral);
    GncDateTimeImpl(std::string str);
    GncDateTimeImpl(PTime&& pt) : m_time(pt, tzp.get(pt.date().year())) {}
    GncDateTimeImpl(LDT&& ldt) : m_time(ldt) {}

    operator time64() const;
    operator struct tm() const;
    void now() { m_time = boost::local_time::local_sec_clock::local_time(tzp.get(boost::gregorian::day_clock::local_day().year())); }
    long offset() const;
    struct tm utc_tm() const { return to_tm(m_time.utc_time()); }
    std::unique_ptr<GncDateImpl> date() const;
    std::string format(const char* format) const;
    std::string format_zulu(const char* format) const;
private:
    LDT m_time;
    static const TD time_of_day[3];
};

const TD GncDateTimeImpl::time_of_day[3] = {TD(0, 0, 0), TD(10, 59, 0), TD(23, 59, 59)};
/** Private implementation of GncDate. See the documentation for that class.
 */
class GncDateImpl
{
public:
    GncDateImpl(): m_greg(boost::gregorian::day_clock::local_day()) {}
    GncDateImpl(const int year, const int month, const int day) :
    m_greg(year, static_cast<Month>(month), day) {}
    GncDateImpl(Date d) : m_greg(d) {}
    GncDateImpl(const std::string str, const std::string fmt);

    void today() { m_greg = boost::gregorian::day_clock::local_day(); }
    ymd year_month_day() const;
    std::string format(const char* format) const;
    std::string format_zulu(const char* format) const;
private:
    Date m_greg;

    friend GncDateTimeImpl::GncDateTimeImpl(const GncDateImpl&, DayPart);
    friend bool operator<(const GncDateImpl&, const GncDateImpl&);
    friend bool operator>(const GncDateImpl&, const GncDateImpl&);
    friend bool operator==(const GncDateImpl&, const GncDateImpl&);
    friend bool operator<=(const GncDateImpl&, const GncDateImpl&);
    friend bool operator>=(const GncDateImpl&, const GncDateImpl&);
    friend bool operator!=(const GncDateImpl&, const GncDateImpl&);
};

/* Member function definitions for GncDateTimeImpl.
 */

GncDateTimeImpl::GncDateTimeImpl(const GncDateImpl& date, DayPart part) :
    m_time(date.m_greg, time_of_day[part], tzp.get(date.m_greg.year()),
                     LDT::NOT_DATE_TIME_ON_ERROR)
{
    using boost::posix_time::hours;
    try
    {
        if (part == DayPart::neutral)
        {
            auto offset = m_time.local_time() - m_time.utc_time();
            m_time = LDT(date.m_greg, time_of_day[part], utc_zone,
                         LDT::EXCEPTION_ON_ERROR);
            if (offset < hours(-10))
                m_time -= hours(offset.hours() + 10);
            if (offset > hours(13))
                m_time -= hours(offset.hours() - 11);
        }
    }
    catch(boost::gregorian::bad_year&)
    {
        throw(std::invalid_argument("Time value is outside the supported year range."));
    }
}

using PTZ = boost::local_time::posix_time_zone;

static TZ_Ptr
tz_from_string(std::string str)
{
    if (str.empty()) return utc_zone;
    std::string tzstr = "XXX" + str;
    if (tzstr.length() > 6 && tzstr[6] != ':') //6 for XXXsHH, s is + or -
        tzstr.insert(6, ":");
    if (tzstr.length() > 9 && tzstr[9] != ':') //9 for XXXsHH:MM
    {
        tzstr.insert(9, ":");
    }
    return TZ_Ptr(new PTZ(tzstr));
}

GncDateTimeImpl::GncDateTimeImpl(std::string str) :
    m_time(unix_epoch, utc_zone)
{
    if (str.empty()) return;
    TZ_Ptr tzptr;
    try
    {
        static const boost::regex delim_iso("^(\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}:\\d{2}(?:\\.\\d{0,9})?)\\s*([+-]\\d{2}(?::?\\d{2})?)?$");
        static const boost::regex non_delim("^(\\d{14}(?:\\.\\d{0,9})?)\\s*([+-]\\d{2}\\s*(:?\\d{2})?)?$");
        PTime pdt;
        boost::smatch sm;
        if (regex_match(str, sm, non_delim))
        {
            std::string time_str(sm[1]);
            time_str.insert(8, "T");
            pdt = boost::posix_time::from_iso_string(time_str);
        }
        else if (regex_match(str, sm, delim_iso))
        {
            pdt = boost::posix_time::time_from_string(sm[1]);
        }
        else
        {
            throw(std::invalid_argument("The date string was not formatted in a way that GncDateTime(std::string) knows how to parse."));
        }
        std::string tzstr("");
        if (sm[2].matched)
            tzstr += sm[2];
        tzptr = tz_from_string(tzstr);
        m_time = LDT(pdt.date(), pdt.time_of_day(), tzptr,
                         LDTBase::NOT_DATE_TIME_ON_ERROR);
    }
    catch(boost::gregorian::bad_year&)
    {
        throw(std::invalid_argument("The date string was outside of the supported year range."));
    }
    /* Bug 767824: A GLib bug in parsing the UTC timezone on Windows may have
     * created a bogus timezone of a random number of minutes. Since there are
     * no fractional-hour timezones around the prime meridian we can safely
     * check for this in files by resetting to UTC if there's a
     * less-than-an-hour offset.
     */
    auto offset = tzptr->base_utc_offset().seconds();
    if (offset != 0 && std::abs(offset) < 3600)
        m_time = m_time.local_time_in(utc_zone);
}

GncDateTimeImpl::operator time64() const
{
    auto duration = m_time.utc_time() - unix_epoch;
    auto secs = duration.ticks();
    secs /= ticks_per_second;
    return secs;
}

GncDateTimeImpl::operator struct tm() const
{
    struct tm time = to_tm(m_time);
#if HAVE_STRUCT_TM_GMTOFF
    time.tm_gmtoff = offset();
#endif
    return time;
}

long
GncDateTimeImpl::offset() const
{
    auto offset = m_time.local_time() - m_time.utc_time();
    return offset.total_seconds();
}

std::unique_ptr<GncDateImpl>
GncDateTimeImpl::date() const
{
    return std::unique_ptr<GncDateImpl>(new GncDateImpl(m_time.local_time().date()));
}

/* The 'O', 'E', and '-' format modifiers are not supported by
 * boost's output facets. Remove them.
 */
static inline std::string
normalize_format (const std::string& format)
{
    bool is_pct = false;
    std::string normalized;
    std::remove_copy_if(
        format.begin(), format.end(), back_inserter(normalized),
        [&is_pct](char e){
            bool r = (is_pct && (e == 'E' || e == 'O' || e == '-'));
            is_pct = e == '%';
            return r;
        });
    return normalized;
}

std::string
GncDateTimeImpl::format(const char* format) const
{
    using Facet = boost::local_time::local_time_facet;
    std::stringstream ss;
    //The stream destructor frees the facet, so it must be heap-allocated.
    auto output_facet(new Facet(normalize_format(format).c_str()));
    ss.imbue(std::locale(std::locale(), output_facet));
    ss << m_time;
    return ss.str();
}

std::string
GncDateTimeImpl::format_zulu(const char* format) const
{
    using Facet = boost::posix_time::time_facet;
    std::stringstream ss;
    //The stream destructor frees the facet, so it must be heap-allocated.
    auto output_facet(new Facet(normalize_format(format).c_str()));
    ss.imbue(std::locale(std::locale(), output_facet));
    ss << m_time.utc_time();
    return ss.str();
}

/* Member function definitions for GncDateImpl.
 */
GncDateImpl::GncDateImpl(const std::string str, const std::string fmt) :
    m_greg(boost::gregorian::day_clock::local_day()) /* Temporarily initialized to today, will be used and adjusted in the code below */
{
    auto iter = std::find_if(GncDate::c_formats.cbegin(), GncDate::c_formats.cend(),
                             [&fmt](const GncDateFormat& v){ return (v.m_fmt == fmt); } );
    if (iter == GncDate::c_formats.cend())
        throw std::invalid_argument(N_("Unknown date format specifier passed as argument."));

    boost::regex r(iter->m_re);
    boost::smatch what;
    if(!boost::regex_search(str, what, r))  // regex didn't find a match
        throw std::invalid_argument (N_("Value can't be parsed into a date using the selected date format."));

    // Bail out if a year was found with a yearless format specifier
    auto fmt_has_year = (fmt.find('y') != std::string::npos);
    if (!fmt_has_year && (what.length("YEAR") != 0))
        throw std::invalid_argument (N_("Value appears to contain a year while the selected format forbids this."));

    int year;
    if (fmt_has_year)
    {
        /* The input dates have a year, so use that one */
        year = std::stoi (what.str("YEAR"));

        /* We assume two-digit years to be in the range 1969 - 2068. */
        if (year < 69)
                year += 2000;
        else if (year < 100)
                year += 1900;
    }
    else /* The input dates have no year, so use current year */
        year = m_greg.year(); // Can use m_greg here as it was already initialized in the initializer list earlier

    m_greg = Date(year,
                  static_cast<Month>(std::stoi (what.str("MONTH"))),
                  std::stoi (what.str("DAY")));
}

ymd
GncDateImpl::year_month_day() const
{
    auto boost_ymd = m_greg.year_month_day();
    return {boost_ymd.year, boost_ymd.month.as_number(), boost_ymd.day};
}

std::string
GncDateImpl::format(const char* format) const
{
    using Facet = boost::gregorian::date_facet;
    std::stringstream ss;
    //The stream destructor frees the facet, so it must be heap-allocated.
    auto output_facet(new Facet(normalize_format(format).c_str()));
    ss.imbue(std::locale(std::locale(), output_facet));
    ss << m_greg;
    return ss.str();
}

bool operator<(const GncDateImpl& a, const GncDateImpl& b) { return a.m_greg < b.m_greg; }
bool operator>(const GncDateImpl& a, const GncDateImpl& b) { return a.m_greg > b.m_greg; }
bool operator==(const GncDateImpl& a, const GncDateImpl& b) { return a.m_greg == b.m_greg; }
bool operator<=(const GncDateImpl& a, const GncDateImpl& b) { return a.m_greg <= b.m_greg; }
bool operator>=(const GncDateImpl& a, const GncDateImpl& b) { return a.m_greg >= b.m_greg; }
bool operator!=(const GncDateImpl& a, const GncDateImpl& b) { return a.m_greg != b.m_greg; }

/* =================== Presentation-class Implementations ====================*/
/* GncDateTime */

GncDateTime::GncDateTime() : m_impl(new GncDateTimeImpl) {}
GncDateTime::GncDateTime(const time64 time) :
    m_impl(new GncDateTimeImpl(time)) {}
GncDateTime::GncDateTime(const struct tm tm) :
    m_impl(new GncDateTimeImpl(tm)) {}
GncDateTime::GncDateTime(std::string str) :
    m_impl(new GncDateTimeImpl(str)) {}
GncDateTime::~GncDateTime() = default;

GncDateTime::GncDateTime(const GncDate& date, DayPart part) :
    m_impl(new GncDateTimeImpl(*(date.m_impl), part)) {}

void
GncDateTime::now()
{
    m_impl->now();
}

GncDateTime::operator time64() const
{
    return m_impl->operator time64();
}

GncDateTime::operator struct tm() const
{
    return m_impl->operator struct tm();
}

long
GncDateTime::offset() const
{
    return m_impl->offset();
}

struct tm
GncDateTime::utc_tm() const
{
    return m_impl->utc_tm();
}

GncDate
GncDateTime::date() const
{
    return GncDate(m_impl->date());
}

std::string
GncDateTime::format(const char* format) const
{
    return m_impl->format(format);
}

std::string
GncDateTime::format_zulu(const char* format) const
{
    return m_impl->format_zulu(format);
}

/* GncDate */
GncDate::GncDate() : m_impl{new GncDateImpl} {}
GncDate::GncDate(int year, int month, int day) :
m_impl(new GncDateImpl(year, month, day)) {}
GncDate::GncDate(const std::string str, const std::string fmt) :
m_impl(new GncDateImpl(str, fmt)) {}
GncDate::GncDate(std::unique_ptr<GncDateImpl> impl) :
m_impl(std::move(impl)) {}
GncDate::GncDate(const GncDate& a) :
m_impl(new GncDateImpl(*a.m_impl)) {}
GncDate::GncDate(GncDate&&) = default;
GncDate::~GncDate() = default;

GncDate&
GncDate::operator=(const GncDate& a)
{
    m_impl.reset(new GncDateImpl(*a.m_impl));
    return *this;
}
GncDate&
GncDate::operator=(GncDate&&) = default;

void
GncDate::today()
{
    m_impl->today();
}

std::string
GncDate::format(const char* format)
{
    return m_impl->format(format);
}

ymd
GncDate::year_month_day() const
{
    return m_impl->year_month_day();
}

bool operator<(const GncDate& a, const GncDate& b) { return *(a.m_impl) < *(b.m_impl); }
bool operator>(const GncDate& a, const GncDate& b) { return *(a.m_impl) > *(b.m_impl); }
bool operator==(const GncDate& a, const GncDate& b) { return *(a.m_impl) == *(b.m_impl); }
bool operator<=(const GncDate& a, const GncDate& b) { return *(a.m_impl) <= *(b.m_impl); }
bool operator>=(const GncDate& a, const GncDate& b) { return *(a.m_impl) >= *(b.m_impl); }
bool operator!=(const GncDate& a, const GncDate& b) { return *(a.m_impl) != *(b.m_impl); }
