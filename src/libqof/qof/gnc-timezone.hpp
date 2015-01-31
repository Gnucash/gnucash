/********************************************************************\
 * gnc-timezone.cpp - Retrieve timezone information from OS.        *
 * Copyright 2014 John Ralls <jralls@ceridwen.us>                   *
 * Based on work done with Arnel Borja for GLib's gtimezone in 2012.*
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

#ifndef __GNC_TIMEZONE_HPP__
#define __GNC_TIMEZONE_HPP__
extern "C"
{
#include <platform.h>
#if PLATFORM(WINDOWS)
#include <windows.h>
#endif
}

#define BOOST_ERROR_CODE_HEADER_ONLY
#include <boost/date_time/local_time/local_time.hpp>

namespace gnc
{
    namespace date
    {}
}// Move these later
using TZ = boost::local_time::time_zone;
using TZ_Ptr = boost::local_time::time_zone_ptr;
using TZ_Entry = std::pair<int, TZ_Ptr>;
using TZ_Vector = std::vector<TZ_Entry>;
using time_zone_names = boost::local_time::time_zone_names;

class TimeZoneProvider
{
public:
    // The default constructor provides the time zone for the current locale
    TimeZoneProvider() : TimeZoneProvider (static_cast<std::string>("")) {}
    TimeZoneProvider(const std::string& tzname); //create a provider for a specified TZ.
    TimeZoneProvider(const TimeZoneProvider&) = delete;
    TimeZoneProvider(const TimeZoneProvider&&) = delete;
    TimeZoneProvider operator=(const TimeZoneProvider&) = delete;
    TimeZoneProvider operator=(const TimeZoneProvider&&) = delete;
    TZ_Ptr get (int year) const noexcept;
private:
    TZ_Vector zone_vector;
#if PLATFORM(WINDOWS)
    void load_windows_dynamic_tz(HKEY, time_zone_names);
    void load_windows_classic_tz(HKEY, time_zone_names);
#endif
};

#endif //__GCN_TIMEZONE_HPP__
