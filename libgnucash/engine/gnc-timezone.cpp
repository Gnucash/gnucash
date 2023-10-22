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
#include <iostream>
#include <algorithm>
#include <boost/date_time/gregorian/gregorian.hpp>
#if PLATFORM(WINDOWS)
//We'd prefer to use std::codecvt, but it's not supported by gcc until 5.0.
#include <boost/locale/encoding_utf.hpp>
#endif
#include "qoflog.h"
static const QofLogModule log_module = "gnc-timezone";

using namespace gnc::date;

using duration = boost::posix_time::time_duration;
using time_zone = boost::local_time::custom_time_zone;
using dst_offsets = boost::local_time::dst_adjustment_offsets;
using calc_rule_ptr = boost::local_time::dst_calc_rule_ptr;
using PTZ = boost::local_time::posix_time_zone;

const unsigned int TimeZoneProvider::min_year = 1400;
const unsigned int TimeZoneProvider::max_year = 9999;

template<typename T>
T*
endian_swap(T* t)
{
#if ! WORDS_BIGENDIAN
    auto memp = reinterpret_cast<unsigned char*>(t);
    std::reverse(memp, memp + sizeof(T));
#endif
    return t;
}

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

#define make_week_num(x)  static_cast<boost::date_time::nth_kday_of_month<boost::gregorian::date>::week_num>(x)

static TZ_Ptr
zone_from_regtzi (const RegTZI& regtzi, time_zone_names names)
{
    using ndate = boost::gregorian::nth_day_of_the_week_in_month;
    using nth_day_rule = boost::local_time::nth_day_of_the_week_in_month_dst_rule;
    /* Note that Windows runs its biases backwards from POSIX and
     * boost::date_time: It's the value added to the local time to get
     * GMT rather than the value added to GMT to get local time; for
     * the same reason the DaylightBias is negative as one generally
     * adds an hour less to the local time to get GMT. Biases are in
     * minutes.
     */
    duration std_off (0, regtzi.StandardBias - regtzi.Bias, 0);
    duration dlt_off (0, -regtzi.DaylightBias, 0);
    duration start_time (regtzi.StandardDate.wHour, regtzi.StandardDate.wMinute,
			 regtzi.StandardDate.wSecond);
    duration end_time (regtzi.DaylightDate.wHour, regtzi.DaylightDate.wMinute,
		       regtzi.DaylightDate.wSecond);
    dst_offsets offsets (dlt_off, start_time, end_time);
    auto std_week_num = make_week_num(regtzi.StandardDate.wDay);
    auto dlt_week_num = make_week_num(regtzi.DaylightDate.wDay);
    calc_rule_ptr dates;
    if (regtzi.StandardDate.wMonth != 0)
    {
	try
	{
	    ndate start (dlt_week_num, regtzi.DaylightDate.wDayOfWeek,
			 regtzi.DaylightDate.wMonth);
	    ndate end(std_week_num, regtzi.StandardDate.wDayOfWeek,
		      regtzi.StandardDate.wMonth);
	    dates.reset(new nth_day_rule (start, end));
	}
	catch (boost::gregorian::bad_month& err)
	{
	    PWARN("Caught Bad Month Exception. Daylight Bias: %ld  "
		  "Standard Month : %d  Daylight Month: %d",
		  regtzi.DaylightBias, regtzi.StandardDate.wMonth,
		  regtzi.DaylightDate.wMonth);
	}
    }
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
		m_zone_vector.push_back (std::make_pair(0, tz));
	    m_zone_vector.push_back (std::make_pair(year, tz));
	}
	m_zone_vector.push_back (std::make_pair(max_year, tz));
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
	    m_zone_vector.push_back(
		std::make_pair(max_year, zone_from_regtzi (regtzi, names)));
	}
    }
    catch (std::bad_alloc)
    {
	RegCloseKey (key);
	throw;
    }
    RegCloseKey (key);
}

void
TimeZoneProvider::load_windows_default_tz()
{
    TIME_ZONE_INFORMATION tzi {};
    GetTimeZoneInformation (&tzi);
    RegTZI regtzi { tzi.Bias, tzi.StandardBias, tzi.DaylightBias,
            tzi.StandardDate, tzi.DaylightDate };
    using boost::locale::conv::utf_to_utf;
    auto std_name = utf_to_utf<char>(tzi.StandardName,
				tzi.StandardName + sizeof(tzi.StandardName));
    auto dlt_name = utf_to_utf<char>(tzi.DaylightName,
				tzi.DaylightName + sizeof(tzi.DaylightName));
    time_zone_names names (std_name, std_name, dlt_name, dlt_name);
    m_zone_vector.push_back(std::make_pair(max_year, zone_from_regtzi(regtzi, names)));
}

TimeZoneProvider::TimeZoneProvider (const std::string& identifier) :
    m_zone_vector ()
{
    HKEY key;
    const std::string reg_key =
	"SOFTWARE\\Microsoft\\Windows NT\\CurrentVersion\\Time Zones\\";

    auto key_name = (identifier.empty() ? windows_default_tzname () :
		     identifier);

    if (key_name.empty())
    {
        load_windows_default_tz();
        return;
    }
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
#elif PLATFORM(POSIX)
using std::to_string;
#include <istream>
#include <cstdlib>

using boost::posix_time::ptime;
//To enable using Transition with different meanings for IANA files
//and for DSTRules.
namespace IANAParser
{
    struct TZHead
    {
	char magic[4];
	char version;
	uint8_t reserved[15];
	uint8_t ttisgmtcnt[4];
	uint8_t ttisstdcnt[4];
	uint8_t leapcnt[4];
	uint8_t timecnt[4];
	uint8_t typecnt[4];
	uint8_t charcnt[4];
    };

    struct TTInfo
    {
	int32_t gmtoff;
	uint8_t isdst;
	uint8_t abbrind;
    };

    struct TZInfo
    {
	TTInfo info;
	std::string name;
	bool isstd;
	bool isgmt;
    };

    struct Transition
    {
	int64_t timestamp;
	uint8_t index;
    };

    static std::unique_ptr<char[]>
    find_tz_file(const std::string& name)
    {
	std::ifstream ifs;
        auto tzname = name;
        if (tzname.empty())
            if (auto tzenv = std::getenv("TZ"))
                tzname = std::string(tzenv);
        //std::cout << "Testing tzname " << tzname << "\n";
        if (!tzname.empty())
        {
//POSIX specifies that that identifier should begin with ':', but we
//should be liberal. If it's there, it's not part of the filename.
	    if (tzname[0] == ':')
		tzname.erase(tzname.begin());
	    if (tzname[0] == '/') //Absolute filename
	    {
		ifs.open(tzname, std::ios::in|std::ios::binary|std::ios::ate);
	    }
	    else
	    {
		const char* tzdir_c = std::getenv("TZDIR");
		std::string tzdir = tzdir_c ? tzdir_c : "/usr/share/zoneinfo";
//Note that we're not checking the filename.
		ifs.open(std::move(tzdir + "/" + tzname),
			 std::ios::in|std::ios::binary|std::ios::ate);
	    }
	}

	if (! ifs.is_open())
	    throw std::invalid_argument("The timezone string failed to resolve to a valid filename");
	std::streampos filesize = ifs.tellg();
	std::unique_ptr<char[]>fileblock(new char[filesize]);
	ifs.seekg(0, std::ios::beg);
	ifs.read(fileblock.get(), filesize);
	ifs.close();
	return fileblock;
    }

    using TZInfoVec = std::vector<TZInfo>;
    using TZInfoIter = TZInfoVec::iterator;

    struct IANAParser
    {
	IANAParser(const std::string& name) : IANAParser(find_tz_file(name)) {}
	IANAParser(std::unique_ptr<char[]>);
	std::vector<Transition>transitions;
	TZInfoVec tzinfo;
	int last_year;
    };

    IANAParser::IANAParser(std::unique_ptr<char[]>fileblock)
    {
	unsigned int fb_index = 0;
	TZHead tzh = *reinterpret_cast<TZHead*>(&fileblock[fb_index]);
	static constexpr int ttinfo_size = 6; //struct TTInfo gets padded
	last_year = 2037; //Constrained by 32-bit time_t.
	int transition_size = 4; // length of a transition time in the file

	auto time_count = *(endian_swap(reinterpret_cast<uint32_t*>(tzh.timecnt)));
	auto type_count = *(endian_swap(reinterpret_cast<uint32_t*>(tzh.typecnt)));
	auto char_count = *(endian_swap(reinterpret_cast<uint32_t*>(tzh.charcnt)));
	auto isgmt_count = *(endian_swap(reinterpret_cast<uint32_t*>(tzh.ttisgmtcnt)));
	auto isstd_count = *(endian_swap(reinterpret_cast<uint32_t*>(tzh.ttisstdcnt)));
	auto leap_count = *(endian_swap(reinterpret_cast<uint32_t*>(tzh.leapcnt)));
	if ((tzh.version == '2' || tzh.version == '3'))
	{
	    fb_index = (sizeof(tzh) +
			(sizeof(uint32_t) + sizeof(uint8_t)) * time_count +
			ttinfo_size * type_count +
			sizeof(char) * char_count +
			sizeof(uint8_t) * isgmt_count +
			sizeof(uint8_t) * isstd_count +
			2 * sizeof(uint32_t) * leap_count);

	    //This might change at some point in the probably very
	    //distant future.
	    tzh = *reinterpret_cast<TZHead*>(&fileblock[fb_index]);
	    last_year = 2499;
	    time_count = *(endian_swap(reinterpret_cast<uint32_t*>(tzh.timecnt)));
	    type_count = *(endian_swap(reinterpret_cast<uint32_t*>(tzh.typecnt)));
	    char_count = *(endian_swap(reinterpret_cast<uint32_t*>(tzh.charcnt)));
	    transition_size = 8;
	}
	fb_index += sizeof(tzh);
	auto start_index = fb_index;
	auto info_index_zero = start_index + time_count * transition_size;
	for(uint32_t index = 0; index < time_count; ++index)
	{
	    fb_index = start_index + index * transition_size;
	    auto info_index = info_index_zero + index;
	    if (transition_size  == 4)
	    {
                int32_t transition_time;
                // Ensure correct alignment for ARM.
                memcpy(&transition_time,
                       endian_swap(reinterpret_cast<int32_t*>(&fileblock[fb_index])),
                       sizeof(int32_t));
                auto info = static_cast<uint8_t>(fileblock[info_index]);
                transitions.push_back({transition_time, info});
	    }
	    else
	    {
                int64_t transition_time;
                // Ensure correct alignment for ARM.
                memcpy(&transition_time,
                       endian_swap(reinterpret_cast<int64_t*>(&fileblock[fb_index])),
                       sizeof(int64_t));
                auto info = static_cast<uint8_t>(fileblock[info_index]);
                transitions.push_back({transition_time, info});
	    }
	}

	//Add in the tzinfo indexes consumed in the previous loop
	start_index = info_index_zero + time_count;
	auto abbrev = start_index + type_count * ttinfo_size;
	auto std_dist = abbrev + char_count;
	auto gmt_dist = std_dist + type_count;
	for(uint32_t index = 0; index < type_count; ++index)
	{
	    fb_index = start_index + index * ttinfo_size;
	    /* Use memcpy instead of static_cast to avoid memory alignment issues with chars */
	    TTInfo info{};
	    memcpy(&info, &fileblock[fb_index], ttinfo_size);
	    endian_swap(&info.gmtoff);
	    tzinfo.push_back(
		{info, &fileblock[abbrev + info.abbrind],
         (index < isstd_count ? fileblock[std_dist + index] != '\0' : true),
         (index < isgmt_count ? fileblock[gmt_dist + index] != '\0' : false)});
	}

    }
}

namespace DSTRule
{
    using gregorian_date = boost::gregorian::date;
    using IANAParser::TZInfoIter;
    using ndate = boost::gregorian::nth_day_of_the_week_in_month;
    using week_num =
	boost::date_time::nth_kday_of_month<boost::gregorian::date>::week_num;

    struct Transition
    {
	Transition() : month(1), dow(0), week(static_cast<week_num>(0)) {}
	Transition(gregorian_date date);
	bool operator==(const Transition& rhs) const noexcept;
	ndate get();
	boost::gregorian::greg_month month;
	boost::gregorian::greg_weekday dow;
	week_num week;
    };

    Transition::Transition(gregorian_date date) :
	month(date.month()), dow(date.day_of_week()),
	week(static_cast<week_num>((6 + date.day() - date.day_of_week()) / 7))
    {}

    bool
    Transition::operator==(const Transition& rhs) const noexcept
    {
	return (month == rhs.month && dow == rhs.dow && week == rhs.week);
    }

    ndate
    Transition::get()
    {
	return ndate(week, dow, month);
    }

    struct DSTRule
    {
	DSTRule();
	DSTRule(TZInfoIter info1, TZInfoIter info2,
		ptime date1, ptime date2);
	bool operator==(const DSTRule& rhs) const noexcept;
	bool operator!=(const DSTRule& rhs) const noexcept;
	Transition to_std;
	Transition to_dst;
	duration to_std_time;
	duration to_dst_time;
	TZInfoIter std_info;
	TZInfoIter dst_info;
    };

    DSTRule::DSTRule() : to_std(), to_dst(), to_std_time {}, to_dst_time {},
	std_info (), dst_info () {};

    DSTRule::DSTRule (TZInfoIter info1, TZInfoIter info2,
		      ptime date1, ptime date2) :
	to_std(date1.date()), to_dst(date2.date()),
	to_std_time(date1.time_of_day()), to_dst_time(date2.time_of_day()),
	std_info(info1), dst_info(info2)
    {
	if (info1->info.isdst == info2->info.isdst)
	    throw(std::invalid_argument("Both infos have the same dst value."));
	if (info1->info.isdst && !info2->info.isdst)
	{
	    std::swap(to_std, to_dst);
	    std::swap(to_std_time, to_dst_time);
	    std::swap(std_info, dst_info);
	}

        /* Documentation notwithstanding, the date-time rules are
         * looking for local time (wall clock to use the RFC 8538
         * definition) values.
         *
         * The TZ Info contains two fields, isstd and isgmt (renamed
         * to isut in newer versions of tzinfo). In theory if both are
         * 0 the transition times represent wall-clock times,
         * i.e. time stamps in the respective time zone's local time
         * at the moment of the transition. If isstd is 1 then the
         * representation is always in standard time instead of
         * daylight time; this is significant for dst->std
         * transitions. If isgmt/isut is one then isstd must also be
         * set and the transition time is in UTC.
         *
         * In practice it seems that the timestamps are always in UTC
         * so the isgmt/isut flag isn't meaningful. The times always
         * need to have the utc offset added to them to make the
         * transition occur at the right time; the isstd flag
         * determines whether that should be the standard offset or
         * the daylight offset for the daylight->standard transition.
         */

        to_dst_time += boost::posix_time::seconds(std_info->info.gmtoff);
        if (std_info->isstd) //if isstd always use standard time
            to_std_time += boost::posix_time::seconds(std_info->info.gmtoff);
        else
            to_std_time += boost::posix_time::seconds(dst_info->info.gmtoff);

    }

    bool
    DSTRule::operator==(const DSTRule& rhs) const noexcept
    {
	return (to_std == rhs.to_std &&
		to_dst == rhs.to_dst &&
		to_std_time == rhs.to_std_time &&
		to_dst_time == rhs.to_dst_time &&
		std_info == rhs.std_info &&
		dst_info == rhs.dst_info);
    }

    bool
    DSTRule::operator!=(const DSTRule& rhs) const noexcept
    {
	return ! operator==(rhs);
    }
}

static TZ_Entry
zone_no_dst(int year, IANAParser::TZInfoIter std_info)
{
    time_zone_names names(std_info->name, std_info->name, "", "");
    duration std_off(0, 0, std_info->info.gmtoff);
    dst_offsets offsets({0, 0, 0}, {0, 0, 0}, {0, 0, 0});
    boost::local_time::dst_calc_rule_ptr calc_rule(nullptr);
    TZ_Ptr tz(new time_zone(names, std_off, offsets, calc_rule));
    return std::make_pair(year, tz);
}

static TZ_Entry
zone_from_rule(int year, DSTRule::DSTRule rule)
{
    using boost::gregorian::partial_date;
    using boost::local_time::partial_date_dst_rule;
    using nth_day_rule =
	boost::local_time::nth_day_of_the_week_in_month_dst_rule;

    time_zone_names names(rule.std_info->name, rule.std_info->name,
			  rule.dst_info->name, rule.dst_info->name);
    duration std_off(0, 0, rule.std_info->info.gmtoff);
    duration dlt_off(0, 0,
		     rule.dst_info->info.gmtoff - rule.std_info->info.gmtoff);
    dst_offsets offsets(dlt_off, rule.to_dst_time, rule.to_std_time);
    calc_rule_ptr dates(new nth_day_rule(rule.to_dst.get(), rule.to_std.get()));
    TZ_Ptr tz(new time_zone(names, std_off, offsets, dates));
    return std::make_pair(year, tz);
}

void
TimeZoneProvider::parse_file(const std::string& tzname)
{
    IANAParser::IANAParser parser(tzname);
    using boost::posix_time::hours;
    const auto one_year = hours(366 * 24); //Might be a leap year.
    auto last_info = std::find_if(parser.tzinfo.begin(), parser.tzinfo.end(),
                                  [](IANAParser::TZInfo tz)
                                  {return !tz.info.isdst;});
    auto last_time = ptime();
    DSTRule::DSTRule last_rule;
    using boost::gregorian::date;
    using boost::posix_time::ptime;
    using boost::posix_time::time_duration;
    for (auto txi = parser.transitions.begin();
         txi != parser.transitions.end(); ++txi)
    {
        auto this_info = parser.tzinfo.begin() + txi->index;
//Can't use boost::posix_date::from_time_t() constructor because it
//silently casts the time_t to an int32_t.
        auto this_time = ptime(date(1970, 1, 1),
                               time_duration(txi->timestamp / 3600, 0,
                                             txi->timestamp % 3600));
        /* Note: The "get" function retrieves the last zone with a
         * year *earlier* than the requested year: Zone periods run
         * from the saved year to the beginning year of the next zone.
         */
        try
        {
            auto this_year = this_time.date().year();
            //Initial case
            if (last_time.is_not_a_date_time())
            {
                m_zone_vector.push_back(zone_no_dst(this_year - 1, last_info));
                m_zone_vector.push_back(zone_no_dst(this_year, this_info));
            }
            // No change in is_dst means a permanent zone change.
            else if (last_info->info.isdst == this_info->info.isdst)
            {
                m_zone_vector.push_back(zone_no_dst(this_year, this_info));
            }
            /* If there have been no transitions in at least a year
             * then we need to create a no-DST rule with last_info to
             * reflect the frozen timezone.
             */
            else if (this_time - last_time > one_year)
            {
                auto year = last_time.date().year();
                if (m_zone_vector.back().first == year)
                    year = year + 1; // no operator ++ or +=, sigh.
                m_zone_vector.push_back(zone_no_dst(year, last_info));
            }
            /* It's been less than a year, so it's probably a DST
             * cycle. This consumes two transitions so we want only
             * the return-to-standard-time one to make a DST rule.
             */
            else if (!this_info->info.isdst)
            {
                DSTRule::DSTRule new_rule(last_info, this_info,
                                          last_time, this_time);
                if (new_rule != last_rule)
                {
                    last_rule = new_rule;
                    auto year = last_time.date().year();
                    m_zone_vector.push_back(zone_from_rule(year, new_rule));
                }
            }
        }
        catch(const boost::gregorian::bad_year& err)
        {
            continue;
        }
        last_time = this_time;
        last_info = this_info;
    }
/* if the transitions end before the end of the zoneinfo coverage
 * period then the zone rescinded DST and we need a final no-dstzone.
 */
    if (last_time.is_not_a_date_time())
        m_zone_vector.push_back(zone_no_dst(max_year, last_info));
    else if (last_time.date().year() < parser.last_year)
        m_zone_vector.push_back(zone_no_dst(last_time.date().year(), last_info));
}

bool
TimeZoneProvider::construct(const std::string& tzname)
{
    try
    {
        parse_file(tzname);
    }
    catch(const std::invalid_argument& err)
    {
        try
        {
            TZ_Ptr zone(new PTZ(tzname));
            m_zone_vector.push_back(std::make_pair(max_year, zone));
        }
        catch(std::exception& err)
        {
            return false;
        }
    }
    return true;
}

TimeZoneProvider::TimeZoneProvider(const std::string& tzname) :  m_zone_vector {}
{
    if(construct(tzname))
        return;
    DEBUG("%s invalid, trying TZ environment variable.\n", tzname.c_str());
    const char* tz_env = getenv("TZ");
    if(tz_env && construct(tz_env))
        return;
    DEBUG("No valid $TZ, resorting to /etc/localtime.\n");
    try
    {
        parse_file("/etc/localtime");
    }
    catch(const std::invalid_argument& env)
    {
        DEBUG("/etc/localtime invalid, resorting to GMT.");
        TZ_Ptr zone(new PTZ("UTC0"));
        m_zone_vector.push_back(std::make_pair(max_year, zone));
    }
}
#endif


TZ_Ptr
TimeZoneProvider::get(int year) const noexcept
{
    if (m_zone_vector.empty())
        return TZ_Ptr(new PTZ("UTC0"));
    auto iter = find_if(m_zone_vector.rbegin(), m_zone_vector.rend(),
			[=](TZ_Entry e) { return e.first <= year; });
    if (iter == m_zone_vector.rend())
            return m_zone_vector.front().second;
    return iter->second;
}

void
TimeZoneProvider::dump() const noexcept
{
    for (const auto& zone : m_zone_vector)
	std::cout << zone.first << ": " << zone.second->to_posix_string() << "\n";
}
