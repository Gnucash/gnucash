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


/** Private implementation of GncDate. See the documentation for that class.
 */
class GncDateImpl
{
public:
    GncDateImpl(): m_greg(boost::gregorian::day_clock::local_day()) {}
    GncDateImpl(const int year, const int month, const int day) :
	m_greg(year, static_cast<Month>(month), day) {}
private:
    Date m_greg;
};

/** Private implementation of GncDateTime. See the documentation for that class.
 */
class GncDateTimeImpl
{
public:
    GncDateTimeImpl() : m_time(boost::local_time::local_sec_clock::local_time(tzp.get(boost::gregorian::day_clock::local_day().year()))) {}
    GncDateTimeImpl(const time64 time);
private:
    LDT m_time;
};

static LDT
LDT_from_unix_local(const time64 time)
{
    PTime temp(unix_epoch.date(),
	       boost::posix_time::hours(time / 3600) +
	       boost::posix_time::seconds(time % 3600));
    auto tz = tzp.get(temp.date().year());
    return LDT(temp, tz);
}

GncDateTimeImpl::GncDateTimeImpl(const time64 time) :
    m_time(LDT_from_unix_local(time)) {}

GncDate::GncDate() : m_impl{new GncDateImpl} {}
GncDate::GncDate(int year, int month, int day) :
    m_impl(new GncDateImpl(year, month, day)) {}
GncDate::~GncDate() = default;

GncDateTime::GncDateTime() : m_impl(new GncDateTimeImpl) {}
GncDateTime::GncDateTime(time64 time) : m_impl(new GncDateTimeImpl(time)) {}
GncDateTime::~GncDateTime() = default;
