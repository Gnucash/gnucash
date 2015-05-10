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
#include <istream>
#include <algorithm>
#include <boost/date_time/gregorian/gregorian.hpp>
using namespace gnc::date;

using duration = boost::posix_time::time_duration;
using time_zone = boost::local_time::custom_time_zone;
using dst_offsets = boost::local_time::dst_adjustment_offsets;
using calc_rule_ptr = boost::local_time::dst_calc_rule_ptr;

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

    duration std_off (0, regtzi.StandardBias - regtzi.Bias, 0);
    duration dlt_off (0, regtzi.DaylightBias, 0);
    duration start_time (regtzi.StandardDate.wHour, regtzi.StandardDate.wMinute,
			 regtzi.StandardDate.wSecond);
    duration end_time (regtzi.DaylightDate.wHour, regtzi.DaylightDate.wMinute,
		       regtzi.DaylightDate.wSecond);
    dst_offsets offsets (dlt_off, start_time, end_time);
    auto std_week_num = make_week_num(regtzi.StandardDate.wDay);
    auto dlt_week_num = make_week_num(regtzi.DaylightDate.wDay);
    ndate start (std_week_num, regtzi.StandardDate.wDayOfWeek,
		 regtzi.StandardDate.wMonth);
    ndate end(dlt_week_num, regtzi.DaylightDate.wDayOfWeek,
	      regtzi.DaylightDate.wMonth);
    calc_rule_ptr dates(new nth_day_rule (start, end));
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
#elif PLATFORM(POSIX)
using std::to_string;
#include <istream>
#include <cstdlib>

using boost::posix_time::ptime;;
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
	if (name.empty())
	{
	    ifs.open("/etc/localtime",
		     std::ios::in|std::ios::binary|std::ios::ate);
	}
	else
	{
	    std::string tzname = name;
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
	if ((tzh.version == '2' || tzh.version == '3') && sizeof(time_t) == sizeof(int64_t))
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
	    isgmt_count = *(endian_swap(reinterpret_cast<uint32_t*>(tzh.ttisgmtcnt)));
	    isstd_count = *(endian_swap(reinterpret_cast<uint32_t*>(tzh.ttisstdcnt)));
	    leap_count = *(endian_swap(reinterpret_cast<uint32_t*>(tzh.leapcnt)));
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
		transitions.push_back(
		    {*(endian_swap(reinterpret_cast<int32_t*>(&fileblock[fb_index]))),
			    static_cast<uint8_t>(fileblock[info_index])});
	    }
	    else
	    {
		transitions.push_back(
		    {*(endian_swap(reinterpret_cast<int64_t*>(&fileblock[fb_index]))),
			    static_cast<uint8_t>(fileblock[info_index])});
	    }
	}

	//Add in the tzinfo indexes consumed in the previous loop
	start_index = info_index_zero + time_count;
	//Can't use sizeof(TZInfo) because it's padded out to 8 bytes.
	static const size_t tzinfo_size = 6;
	auto abbrev = start_index + type_count * tzinfo_size;
	auto std_dist = abbrev + char_count;
	auto gmt_dist = std_dist + type_count;
	for(uint32_t index = 0; index < type_count; ++index)
	{
	    fb_index = start_index + index * tzinfo_size;
	    TTInfo info = *reinterpret_cast<TTInfo*>(&fileblock[fb_index]);
	    endian_swap(&info.gmtoff);
	    tzinfo.push_back(
		{info, &fileblock[abbrev + info.abbrind],
			fileblock[std_dist + index] != '\0',
			fileblock[gmt_dist + index] != '\0'});
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
	week(static_cast<week_num>((7 + date.day() - date.day_of_week()) / 7))
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
	if (dst_info->isgmt)
	    to_dst_time += boost::posix_time::seconds(dst_info->info.gmtoff);
	if (std_info->isgmt)
	    to_std_time += boost::posix_time::seconds(std_info->info.gmtoff);

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

TimeZoneProvider::TimeZoneProvider(const std::string& tzname) :  zone_vector {}
{
    IANAParser::IANAParser parser(tzname);
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
	try
	{
	    auto this_year = this_time.date().year();
	    //Initial case
	    if (last_time.is_not_a_date_time())
		zone_vector.push_back(zone_no_dst(this_year - 1, last_info));
	    //gap in transitions > 1 year, non-dst zone
	    //change. In the last case the exact date of the change will be
	    //wrong because boost::local_date::timezone isn't able to
	    //represent it. For GnuCash's purposes this isn't likely to be
	    //important as the last time this sort of transition happened
	    //was 1946, but we have to handle the case in order to parse
	    //the tz file.
	    else if (this_year - last_time.date().year() > 1 ||
		     last_info->info.isdst == this_info->info.isdst)
	    {
		zone_vector.push_back(zone_no_dst(this_year, last_info));
	    }

	    else
	    {
		DSTRule::DSTRule new_rule(last_info, this_info,
					  last_time, this_time);
		if (new_rule != last_rule)
		{
		    last_rule = new_rule;
		    zone_vector.push_back(zone_from_rule (this_time.date().year(),
							  new_rule));
		}
	    }
	}
	catch(boost::gregorian::bad_year err)
	{
	    continue;
	}
	last_time = this_time;
	last_info = this_info;
    }

    if (last_time.is_not_a_date_time() ||
	last_time.date().year() < parser.last_year)
	zone_vector.push_back(zone_no_dst(9999, last_info));
    else //Last DST rule forever after.
	zone_vector.push_back(zone_from_rule(9999, last_rule));
}
#endif


TZ_Ptr
TimeZoneProvider::get(int year) const noexcept
{
    auto iter = find_if(zone_vector.begin(), zone_vector.end(),
			[=](TZ_Entry e) { return e.first >= year; });
    if (iter == zone_vector.end())
	throw std::out_of_range ("Year " + to_string(year) +
				 " isn't covered by this time zone.");
    return iter->second;
}
