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

#include "gnc-timezone.hpp"

#include <string>
#include <cstdint>
#include <ostream>
#include <boost/date_time/gregorian/gregorian.hpp>
using namespace gnc::date;

#if PLATFORM(WINDOWS)
/* libstdc++ to_string is broken on MinGW with no real interest in fixing it.
 * See https://gcc.gnu.org/bugzilla/show_bug.cgi?id=52015
 */
#if ! COMPILER(MINGW)
using std::to_string;
#else
template<typename T> inline std::string to_string(T num);

template<>
inline std::string
to_string<unsigned int>(unsigned int num)
{
    constexpr unsigned int numchars = sizeof num * 3 + 1;
    char buf [numchars] {};
    snprintf (buf, numchars, "%u", num);
    return std::string(buf);
}

template<>
inline std::string
to_string<int>(int num)
{
    constexpr unsigned int numchars = sizeof num * 3 + 1;
    char buf [numchars];
    snprintf (buf, numchars, "%d", num);
    return std::string(buf);
}
#endif

static std::string
windows_default_tzname (void)
{
  const char *subkey =
    "SYSTEM\\CurrentControlSet\\Control\\TimeZoneInformation";
  constexpr size_t keysize {128};
  HKEY key;
  char key_name[keysize] {};
  unsigned long tz_keysize = keysize;
  if (RegOpenKeyExA (HKEY_LOCAL_MACHINE, subkey, 0,
                     KEY_QUERY_VALUE, &key) == ERROR_SUCCESS)
    {
	if (RegQueryValueExA (key, "TimeZoneKeyName", nullptr, nullptr,
                                (LPBYTE)key_name, &tz_keysize) != ERROR_SUCCESS)
	{
	    memset (key_name, 0, tz_keysize);
        }
      RegCloseKey (key);
    }
  return std::string(key_name);
}

typedef   struct
{
  LONG Bias;
  LONG StandardBias;
  LONG DaylightBias;
  SYSTEMTIME StandardDate;
  SYSTEMTIME DaylightDate;
} RegTZI;

static time_zone_names
windows_tz_names (HKEY key)
{
    /* The weird sizeof arg is because C++ won't find a type's
     * element, just an object's.
     */
    constexpr auto s_size = sizeof (((TIME_ZONE_INFORMATION*)0)->StandardName);
    char std_name[s_size];
    unsigned long size = s_size;
    if (RegQueryValueExA (key, "Std", NULL, NULL,
			  (LPBYTE)&(std_name), &size) != ERROR_SUCCESS)
	throw std::invalid_argument ("Registry contains no standard name.");

    constexpr auto d_size = sizeof (((TIME_ZONE_INFORMATION*)0)->DaylightName);
    char dlt_name[d_size];
    size = d_size;
    if (RegQueryValueExA (key, "Dlt", NULL, NULL,
			  (LPBYTE)&(dlt_name), &size) != ERROR_SUCCESS)
	throw std::invalid_argument ("Registry contains no daylight name.");

    return time_zone_names (std_name, std_name, dlt_name, dlt_name);
}

static TZ_Ptr
zone_from_regtzi (const RegTZI& regtzi, time_zone_names names)
{
    using duration = boost::posix_time::time_duration;
    using ndate = boost::gregorian::nth_day_of_the_week_in_month;
    using nth_day_rule = boost::local_time::nth_day_of_the_week_in_month_dst_rule;
    using time_zone = boost::local_time::custom_time_zone;

    duration std_off (0, regtzi.StandardBias - regtzi.Bias, 0);
    duration dlt_off (0, regtzi.DaylightBias, 0);
    duration start_time (regtzi.StandardDate.wHour, regtzi.StandardDate.wMinute,
			 regtzi.StandardDate.wSecond);
    duration end_time (regtzi.DaylightDate.wHour, regtzi.DaylightDate.wMinute,
		       regtzi.DaylightDate.wSecond);
    boost::local_time::dst_adjustment_offsets offsets (dlt_off, start_time,
						       end_time);
    auto std_week_num = static_cast<boost::date_time::nth_kday_of_month<boost::gregorian::date>::week_num>(regtzi.StandardDate.wDay);
    auto dlt_week_num = static_cast<boost::date_time::nth_kday_of_month<boost::gregorian::date>::week_num>(regtzi.DaylightDate.wDay);
    ndate start (std_week_num, regtzi.StandardDate.wDayOfWeek,
		 regtzi.StandardDate.wMonth);
    ndate end(dlt_week_num, regtzi.DaylightDate.wDayOfWeek,
	      regtzi.DaylightDate.wMonth);
    boost::local_time::dst_calc_rule_ptr dates(new nth_day_rule (start, end));
    return TZ_Ptr(new time_zone(names, std_off, offsets, dates));
}

void
TimeZoneProvider::load_windows_dynamic_tz (HKEY key, time_zone_names names)
{
    DWORD first, last;

    try
    {
	unsigned long size = sizeof first;
	if (RegQueryValueExA (key, "FirstEntry", NULL, NULL,
			      (LPBYTE) &first, &size) != ERROR_SUCCESS)
	    throw std::invalid_argument ("No first entry.");

	size = sizeof last;
	if (RegQueryValueExA (key, "LastEntry", NULL, NULL,
			      (LPBYTE) &last, &size) != ERROR_SUCCESS)
	    throw std::invalid_argument ("No last entry.");

	TZ_Ptr tz {};
	for (unsigned int year = first; year <= last; year++)
	{
	    auto s = to_string(year);
	    auto ystr = s.c_str();
	    RegTZI regtzi {};
	    size = sizeof regtzi;
	    auto err_val = RegQueryValueExA (key, ystr, NULL, NULL,
					     (LPBYTE) &regtzi, &size);
	    if (err_val != ERROR_SUCCESS)
	    {
		break;
	    }
	    tz = zone_from_regtzi (regtzi, names);
	    if (year == first)
		zone_vector.push_back (std::make_pair(0, tz));
	    zone_vector.push_back (std::make_pair(year, tz));
	}
	zone_vector.push_back (std::make_pair(9999, tz));
   }
    catch (std::invalid_argument)
    {
	RegCloseKey (key);
	throw;
    }
    catch (std::bad_alloc)
    {
	RegCloseKey (key);
	throw;
    }
    RegCloseKey (key);
}

void
TimeZoneProvider::load_windows_classic_tz (HKEY key, time_zone_names names)
{
    RegTZI regtzi {};
    unsigned long size = sizeof regtzi;
    try
    {
	if (RegQueryValueExA (key, "TZI", NULL, NULL,
			      (LPBYTE) &regtzi, &size) == ERROR_SUCCESS)
	{
	    zone_vector.push_back(
		std::make_pair(0, zone_from_regtzi (regtzi, names)));
	}
    }
    catch (std::bad_alloc)
    {
	RegCloseKey (key);
	throw;
    }
    RegCloseKey (key);
}

TimeZoneProvider::TimeZoneProvider (const std::string& identifier) :
    zone_vector ()
{
    HKEY key;
    const std::string reg_key =
	"SOFTWARE\\Microsoft\\Windows NT\\CurrentVersion\\Time Zones\\";

    auto key_name = (identifier.empty() ? windows_default_tzname () :
		     identifier);

    if (key_name.empty())
	throw std::invalid_argument ("No identifier or default tzname.");

    std::string subkey = reg_key + key_name;
    if (RegOpenKeyExA (HKEY_LOCAL_MACHINE, subkey.c_str(), 0,
		       KEY_QUERY_VALUE, &key) != ERROR_SUCCESS)
	throw std::invalid_argument ("No TZ in registry named " + key_name);

    time_zone_names names {windows_tz_names (key)};
    RegCloseKey (key);

    std::string subkey_dynamic = subkey + "\\Dynamic DST";
    if (RegOpenKeyExA (HKEY_LOCAL_MACHINE, subkey_dynamic.c_str(), 0,
		       KEY_QUERY_VALUE, &key) == ERROR_SUCCESS)
	this->load_windows_dynamic_tz (key, names);
    else if (RegOpenKeyExA (HKEY_LOCAL_MACHINE, subkey.c_str(), 0,
			    KEY_QUERY_VALUE, &key) == ERROR_SUCCESS)
	this->load_windows_classic_tz (key, names);
    else
	throw std::invalid_argument ("No data for TZ " + key_name);
}
#else
using std::to_string;
TimeZoneProvider::TimeZoneProvider(const std::string& tzname)
{
}

#endif

TZ_Ptr
TimeZoneProvider::get(int year)
{
    auto iter = find_if(zone_vector.begin(), zone_vector.end(),
			[=](TZ_Entry e) { return e.first >= year; });
    if (iter == zone_vector.end())
	throw std::out_of_range ("Year " + to_string(year) +
				 " isn't covered by this time zone.");
    return iter->second;
}
