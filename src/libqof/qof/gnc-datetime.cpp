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
#include "config.h"
#include "platform.h"
}
#include <boost/date_time/gregorian/gregorian.hpp>
#include <memory>
#include "gnc-timezone.hpp"
#include "gnc-datetime.hpp"

using Date = boost::gregorian::date;
using Month = boost::gregorian::greg_month;
using PTime = boost::posix_time::ptime;
using LDT = boost::local_time::local_date_time;
using Duration = boost::posix_time::time_duration;
using LDTBase = boost::local_time::local_date_time_base<PTime, boost::date_time::time_zone_base<PTime, char>>;
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

/** Private implementation of GncDate. See the documentation for that class.
 */
class GncDateImpl
{
public:
    GncDateImpl(): m_greg(unix_epoch.date()) {}
    GncDateImpl(const int year, const int month, const int day) :
        m_greg(year, static_cast<Month>(month), day) {}
    GncDateImpl(Date d) : m_greg(d) {}

    void today() { m_greg = boost::gregorian::day_clock::local_day(); }
    ymd year_month_day() const;
    std::string format(const char* format) const;
private:
    Date m_greg;
};

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
    auto output_facet(new Facet(format));
    ss.imbue(std::locale(std::locale(), output_facet));
    ss << m_greg;
    return ss.str();
}

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
    catch(boost::gregorian::bad_year)
    {
        throw(std::invalid_argument("Time value is outside the supported year range."));
    }
}

static LDT
LDT_from_struct_tm(const struct tm tm)
{
    try
    {
        auto tdate = boost::gregorian::date_from_tm(tm);
        auto tdur = boost::posix_time::time_duration(tm.tm_hour, tm.tm_min,
                                                     tm.tm_sec, 0);
        auto tz = tzp.get(tdate.year());
        return LDT(PTime(tdate, tdur), tz);
    }
    catch(boost::gregorian::bad_year)
    {
        throw(std::invalid_argument("Time value is outside the supported year range."));
    }
}

class GncDateTimeImpl
{
public:
    GncDateTimeImpl() : m_time(unix_epoch, tzp.get(unix_epoch.date().year())) {}
    GncDateTimeImpl(const time64 time) : m_time(LDT_from_unix_local(time)) {}
    GncDateTimeImpl(const struct tm tm) : m_time(LDT_from_struct_tm(tm)) {}
    GncDateTimeImpl(const std::string str);
    GncDateTimeImpl(PTime&& pt) : m_time(pt, tzp.get(pt.date().year())) {}
    GncDateTimeImpl(LDT&& ldt) : m_time(ldt) {}

    operator time64() const;
    operator struct tm() const;
    void now() { m_time = boost::local_time::local_sec_clock::local_time(tzp.get(boost::gregorian::day_clock::local_day().year())); }
    long offset() const;
    struct tm utc_tm() const { return to_tm(m_time.utc_time()); }
    std::unique_ptr<GncDateImpl> date() const;
    std::string format(const char* format) const;
private:
    LDT m_time;
};

GncDateTimeImpl::GncDateTimeImpl(const std::string str) :
    m_time(unix_epoch, utc_zone)
{
    if (str.empty()) return;

    using std::string;
    using PTZ = boost::local_time::posix_time_zone;
    TZ_Ptr tzptr;
    auto tzpos = str.find_first_of("+-", str.find(":"));
    if (tzpos != str.npos)
    {
        string tzstr = "XXX" + str.substr(tzpos);
        if (tzstr.length() > 6 && tzstr[6] != ':') //6 for XXXsHH, s is + or -
            tzstr.insert(6, ":");
        if (tzstr.length() > 9 && tzstr[9] != ':') //9 for XXXsHH:MM
            tzstr.insert(9, ":");
        tzptr.reset(new PTZ(tzstr));
        if (str[tzpos - 1] == ' ') --tzpos;
    }
    else
    {
        tzptr = utc_zone;
    }
    try
    {
        auto pdt = boost::posix_time::time_from_string(str.substr(0, tzpos));
        m_time = LDT(pdt.date(), pdt.time_of_day(), tzptr,
                     LDTBase::NOT_DATE_TIME_ON_ERROR);
    }
    catch(boost::gregorian::bad_year)
    {
        throw(std::invalid_argument("The date string was outside of the supported year range."));
    }
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

std::string
GncDateTimeImpl::format(const char* format) const
{
    using Facet = boost::local_time::local_time_facet;
    std::stringstream ss;
    //The stream destructor frees the facet, so it must be heap-allocated.
    auto output_facet(new Facet(format));
    ss.imbue(std::locale(std::locale(), output_facet));
    ss << m_time;
    return ss.str();
}

/* =================== Presentation-class Implementations ====================*/
/* GncDate */
GncDate::GncDate() : m_impl{new GncDateImpl} {}
GncDate::GncDate(int year, int month, int day) :
    m_impl(new GncDateImpl(year, month, day)) {}
GncDate::GncDate(std::unique_ptr<GncDateImpl> impl) :
    m_impl(std::move(impl)) {}
GncDate::GncDate(GncDate&&) = default;
GncDate::~GncDate() = default;

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

/* GncDateTime */

GncDateTime::GncDateTime() : m_impl(new GncDateTimeImpl) {}
GncDateTime::GncDateTime(const time64 time) :
    m_impl(new GncDateTimeImpl(time)) {}
GncDateTime::GncDateTime(const struct tm tm) :
    m_impl(new GncDateTimeImpl(tm)) {}
GncDateTime::GncDateTime(const std::string str) :
    m_impl(new GncDateTimeImpl(str)) {}
GncDateTime::~GncDateTime() = default;

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
    return std::move(GncDate(m_impl->date()));
}

std::string
GncDateTime::format(const char* format) const
{
    return m_impl->format(format);
}
