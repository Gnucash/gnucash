/********************************************************************\
 * gnc-timezone-p.hpp - Internal timezone structures and utilities  *
 * Copyright 2014 John Ralls <jralls@ceridwen.us>                   *
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
\********************************************************************/

#ifndef __GNC_TIMEZONE_P_HPP__
#define __GNC_TIMEZONE_P_HPP__

#include <config.h>
#include <platform.h>
#include <string>
#include <vector>
#include <memory>
#include <algorithm>
#include <cstdint>
#include <cstring>
#include <boost/date_time/local_time/local_time.hpp>
#include <boost/date_time/gregorian/gregorian.hpp>

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

namespace IANAParser
{
#pragma pack(push, 1)
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
#pragma pack(pop)

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

    using TZInfoVec = std::vector<TZInfo>;
    using TZInfoIter = TZInfoVec::iterator;

    struct IANAParser
    {
        IANAParser(const std::string& name);
        IANAParser(std::vector<char> fileblock);
        std::vector<Transition> transitions;
        TZInfoVec tzinfo;
        int last_year;
    };
}

namespace DSTRule
{
    using duration = boost::posix_time::time_duration;
    using gregorian_date = boost::gregorian::date;
    using IANAParser::TZInfoIter;
    using ndate = boost::gregorian::nth_day_of_the_week_in_month;
    using week_num =
        boost::date_time::nth_kday_of_month<boost::gregorian::date>::week_num;
    using ptime = boost::posix_time::ptime;

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
}

#endif
