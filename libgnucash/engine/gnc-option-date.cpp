/********************************************************************\
 * gnc-option-date.cpp -- Relative Dates for options                *
 * Copyright (C) 2020 John Ralls <jralls@ceridwen.us>               *
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
 * along with this program; if not, see                             *
 * <https://www.gnu.org/licenses/>                                  *
\********************************************************************/

#include "gnc-option-date.hpp"
#include <array>
#include "gnc-datetime.hpp"
#include <gnc-prefs.h>
#include <iostream>
#include <cassert>
#include <algorithm>

#include "gnc-accounting-period.h"

#define N_(string) string //So that xgettext will find it

enum RelativeDateType
{
    ABSOLUTE,
    LAST,
    NEXT,
    START,
    END
};

enum RelativeDateOffset
{
    NONE,
    WEEK,
    MONTH,
    QUARTER,
    THREE,
    SIX,
    YEAR
};

struct GncRelativeDate
{
    RelativeDatePeriod m_period;
    RelativeDateType m_type;
    RelativeDateOffset m_offset;
    const char* m_storage;
    const char* m_display;
    const char* m_description;
};


/* The fixed values and strings for date periods. Accessor functions will use
 * the RelativeDatePeriod as an index so any changes need to be reflected in the
 * RelativeDatePeriod enum class in gnc-option-date.hpp and vice-versa.
 *
 * The double curly braces are actually correct and required for a std::array
 * initializer list.
 */
static const std::array<GncRelativeDate, 31> reldates
{{
    {
        RelativeDatePeriod::TODAY,
        RelativeDateType::LAST,
        RelativeDateOffset::NONE,
        "today",
        N_("Today"),
        N_("The current date.")
    },
    {
        RelativeDatePeriod::ONE_WEEK_AGO,
        RelativeDateType::LAST,
        RelativeDateOffset::WEEK,
        "one-week-ago",
        N_("One Week Ago"),
        N_("One Week Ago.")
    },
    {
        RelativeDatePeriod::ONE_WEEK_AHEAD,
        RelativeDateType::NEXT,
        RelativeDateOffset::WEEK,
        "one-week-ahead",
        N_("One Week Ahead"),
        N_("One Week Ahead.")
    },
    {
        RelativeDatePeriod::ONE_MONTH_AGO,
        RelativeDateType::LAST,
        RelativeDateOffset::MONTH,
        "one-month-ago",
        N_("One Month Ago"),
        N_("One Month Ago.")
    },
    {
        RelativeDatePeriod::ONE_MONTH_AHEAD,
        RelativeDateType::NEXT,
        RelativeDateOffset::MONTH,
        "one-month-ahead",
        N_("One Month Ahead"),
        N_("One Month Ahead.")
    },
    {
        RelativeDatePeriod::THREE_MONTHS_AGO,
        RelativeDateType::LAST,
        RelativeDateOffset::THREE,
        "three-months-ago",
        N_("Three Months Ago"),
        N_("Three Months Ago.")
    },
    {
        RelativeDatePeriod::THREE_MONTHS_AHEAD,
        RelativeDateType::NEXT,
        RelativeDateOffset::THREE,
        "three-months-ahead",
        N_("Three Months Ahead"),
        N_("Three Months Ahead.")
    },
    {
        RelativeDatePeriod::SIX_MONTHS_AGO,
        RelativeDateType::LAST,
        RelativeDateOffset::SIX,
        "six-months-ago",
        N_("Six Months Ago"),
        N_("Six Months Ago.")
    },
    {
        RelativeDatePeriod::SIX_MONTHS_AHEAD,
        RelativeDateType::NEXT,
        RelativeDateOffset::SIX,
        "six-months-ahead",
        N_("Six Months Ahead"),
        N_("Six Months Ahead.")
    },
    {
        RelativeDatePeriod::ONE_YEAR_AGO,
        RelativeDateType::LAST,
        RelativeDateOffset::YEAR,
        "one-year-ago",
        N_("One Year Ago"),
        N_("One Year Ago.")
    },
    {
        RelativeDatePeriod::ONE_YEAR_AHEAD,
        RelativeDateType::NEXT,
        RelativeDateOffset::YEAR,
        "one-year-ahead",
        N_("One Year Ahead"),
        N_("One Year Ahead.")
    },
    {
        RelativeDatePeriod::START_THIS_MONTH,
        RelativeDateType::START,
        RelativeDateOffset::MONTH,
        "start-this-month",
        N_("Start of this month"),
        N_("First day of the current month.")
    },
    {
        RelativeDatePeriod::END_THIS_MONTH,
        RelativeDateType::END,
        RelativeDateOffset::MONTH,
        "end-this-month",
        N_("End of this month"),
        N_("Last day of the current month.")
    },
    {
        RelativeDatePeriod::START_PREV_MONTH,
        RelativeDateType::START,
        RelativeDateOffset::MONTH,
        "start-prev-month",
        N_("Start of previous month"),
        N_("First day of the previous month.")
    },
    {
        RelativeDatePeriod::END_PREV_MONTH,
        RelativeDateType::END,
        RelativeDateOffset::MONTH,
        "end-prev-month",
        N_("End of previous month"),
        N_("Last day of previous month.")
    },
    {
        RelativeDatePeriod::START_NEXT_MONTH,
        RelativeDateType::START,
        RelativeDateOffset::MONTH,
        "start-next-month",
        N_("Start of next month"),
        N_("First day of the next month.")
    },
    {
        RelativeDatePeriod::END_NEXT_MONTH,
        RelativeDateType::END,
        RelativeDateOffset::MONTH,
        "end-next-month",
        N_("End of next month"),
        N_("Last day of next month.")
    },
    {
        RelativeDatePeriod::START_CURRENT_QUARTER,
        RelativeDateType::START,
        RelativeDateOffset::QUARTER,
        "start-current-quarter",
        N_("Start of current quarter"),
        N_("First day of the current quarterly accounting period.")
    },
    {
        RelativeDatePeriod::END_CURRENT_QUARTER,
        RelativeDateType::END,
        RelativeDateOffset::QUARTER,
        "end-current-quarter",
        N_("End of current quarter"),
        N_("Last day of the current quarterly accounting period.")
    },
    {
        RelativeDatePeriod::START_PREV_QUARTER,
        RelativeDateType::START,
        RelativeDateOffset::QUARTER,
        "start-prev-quarter",
        N_("Start of previous quarter"),
        N_("First day of the previous quarterly accounting period.")
    },
    {
        RelativeDatePeriod::END_PREV_QUARTER,
        RelativeDateType::END,
        RelativeDateOffset::QUARTER,
        "end-prev-quarter",
        N_("End of previous quarter"),
        N_("Last day of previous quarterly accounting period.")
    },
    {
        RelativeDatePeriod::START_NEXT_QUARTER,
        RelativeDateType::START,
        RelativeDateOffset::QUARTER,
        "start-next-quarter",
        N_("Start of next quarter"),
        N_("First day of the next quarterly accounting period.")
    },
    {
        RelativeDatePeriod::END_NEXT_QUARTER,
        RelativeDateType::END,
        RelativeDateOffset::QUARTER,
        "end-next-quarter",
        N_("End of next quarter"),
        N_("Last day of next quarterly accounting period.")
    },
    {
        RelativeDatePeriod::START_CAL_YEAR,
        RelativeDateType::START,
        RelativeDateOffset::YEAR,
        "start-cal-year",
        N_("Start of this year"),
        N_("First day of the current calendar year.")
    },
    {
        RelativeDatePeriod::END_CAL_YEAR,
        RelativeDateType::END,
        RelativeDateOffset::YEAR,
        "end-cal-year",
        N_("End of this year"),
        N_("Last day of the current calendar year.")
    },
    {
        RelativeDatePeriod::START_PREV_YEAR,
        RelativeDateType::START,
        RelativeDateOffset::YEAR,
        "start-prev-year",
        N_("Start of previous year"),
        N_("First day of the previous calendar year.")
    },
    {
        RelativeDatePeriod::END_PREV_YEAR,
        RelativeDateType::END,
        RelativeDateOffset::YEAR,
        "end-prev-year",
        N_("End of previous year"),
        N_("Last day of the previous calendar year.")
    },
    {
        RelativeDatePeriod::START_NEXT_YEAR,
        RelativeDateType::START,
        RelativeDateOffset::YEAR,
        "start-next-year",
        N_("Start of next year"),
        N_("First day of the next calendar year.")
    },
    {
        RelativeDatePeriod::END_NEXT_YEAR,
        RelativeDateType::END,
        RelativeDateOffset::YEAR,
        "end-next-year",
        N_("End of next year"),
        N_("Last day of the next calendar year.")
    },
    {
        RelativeDatePeriod::START_ACCOUNTING_PERIOD,
        RelativeDateType::START,
        RelativeDateOffset::YEAR,
        "start-prev-fin-year",
        N_("Start of accounting period"),
        N_("First day of the accounting period, as set in the global preferences.")
    },
    {
        RelativeDatePeriod::END_ACCOUNTING_PERIOD,
        RelativeDateType::END,
        RelativeDateOffset::YEAR,
        "end-prev-fin-year",
        N_("End of accounting period"),
        N_("Last day of the accounting period, as set in the global preferences.")
    }
    }};

static const GncRelativeDate&
checked_reldate(RelativeDatePeriod per)
{
    assert (reldates[static_cast<int>(per)].m_period == per);
    return reldates[static_cast<int>(per)];
}

bool
gnc_relative_date_is_single(RelativeDatePeriod per)
{
    if (per == RelativeDatePeriod::ABSOLUTE)
        return false;
    auto reldate = checked_reldate(per);
    return reldate.m_type == RelativeDateType::LAST ||
        reldate.m_type == RelativeDateType::NEXT;
}

bool
gnc_relative_date_is_starting(RelativeDatePeriod per)
{
    if (per == RelativeDatePeriod::ABSOLUTE)
        return false;
    return checked_reldate(per).m_type == RelativeDateType::START;
}

bool
gnc_relative_date_is_ending(RelativeDatePeriod per)
{
    if (per == RelativeDatePeriod::ABSOLUTE)
        return false;
    return checked_reldate(per).m_type == RelativeDateType::END;
}

const char*
gnc_relative_date_storage_string(RelativeDatePeriod per)
{
    if (per == RelativeDatePeriod::ABSOLUTE)
        return nullptr;
    return checked_reldate(per).m_storage;
}

const char*
gnc_relative_date_display_string(RelativeDatePeriod per)
{
    if (per == RelativeDatePeriod::ABSOLUTE)
        return nullptr;
    return checked_reldate(per).m_display;
}
const char*
gnc_relative_date_description(RelativeDatePeriod per)
{
    if (per == RelativeDatePeriod::ABSOLUTE)
        return nullptr;
    return checked_reldate(per).m_description;
}

RelativeDatePeriod
gnc_relative_date_from_storage_string(const char* str)
{
    auto per = std::find_if(reldates.begin(), reldates.end(),
                         [str](auto rel) -> bool
                         {
                             return strcmp(str, rel.m_storage) == 0;
                         });
    return per != reldates.end() ? per->m_period : RelativeDatePeriod::ABSOLUTE;
}

static bool
reldate_is_prev(RelativeDatePeriod per)
{
    auto rdate{checked_reldate(per)};
    return per == RelativeDatePeriod::START_PREV_YEAR ||
        per == RelativeDatePeriod::END_PREV_YEAR ||
        per == RelativeDatePeriod::START_PREV_QUARTER ||
        per == RelativeDatePeriod::END_PREV_QUARTER ||
        per == RelativeDatePeriod::START_PREV_MONTH ||
        per == RelativeDatePeriod::END_PREV_MONTH ||
        rdate.m_type == LAST;
}

static bool
reldate_is_next(RelativeDatePeriod per)
{
    auto rdate{checked_reldate(per)};
    return per == RelativeDatePeriod::START_NEXT_YEAR ||
        per == RelativeDatePeriod::END_NEXT_YEAR ||
        per == RelativeDatePeriod::START_NEXT_QUARTER ||
        per == RelativeDatePeriod::END_NEXT_QUARTER ||
        per == RelativeDatePeriod::START_NEXT_MONTH ||
        per == RelativeDatePeriod::END_NEXT_MONTH ||
        rdate.m_type == NEXT;
}

static RelativeDateOffset
reldate_offset(RelativeDatePeriod per)
{
    return checked_reldate(per).m_offset;
}

static int
days_in_month(int month, int year)
{
    return gnc_date_get_last_mday(month, year + 1900);
}

static int
get_last_day_of_month(struct tm& now)
{
    /* Ensure that the month is between 0 and 11*/
    auto year_delta = (now.tm_mon / 12) + (now.tm_mon < 0 ? -1 : 0);
    return days_in_month(now.tm_mon - (12 * year_delta), now.tm_year + year_delta);
}

static void
set_last_day_in_month(struct tm& now)
{
    now.tm_mday = get_last_day_of_month(now);
}

static bool
is_last_day_in_month(struct tm& now)
{
    return now.tm_mday == get_last_day_of_month(now);
}

/* Normalize the modified struct tm computed in gnc_relative_date_to_time64
 * before setting the time and perhaps beginning/end of the month. Using the
 * gnc_date API would involve multiple conversions to and from struct tm.
*/
static void
normalize_reldate_tm(struct tm& now)
{
    auto delta = (now.tm_mon / 12) + (now.tm_mon < 0 ? -1 : 0);
    now.tm_mon -= 12 * delta;
    now.tm_year += delta;
  
    if (now.tm_mday < 1)
    {
        do
        {
            if (now.tm_mon-- == 0)
            {
                now.tm_mon = 11;
                now.tm_year--;
            }
            now.tm_mday += days_in_month(now.tm_mon, now.tm_year);
        } while (now.tm_mday < 1) ;
        return;
    }

    while (now.tm_mday > (delta = days_in_month(now.tm_mon, now.tm_year)))
    {
        if (now.tm_mon++ == 11)
        {
            now.tm_mon = 0;
            now.tm_year++;
        }
        now.tm_mday -= delta;
    }
}

static void
reldate_set_day_and_time(
        struct tm& now,
        RelativeDateType type,
        bool is_offset_quarter,
        struct tm& acct_per)
{
    if (type == RelativeDateType::START)
    {
        now.tm_mday = 1;
        if (is_offset_quarter)
        {
            set_last_day_in_month(now);
            if (!is_last_day_in_month(acct_per) && now.tm_mday > acct_per.tm_mday)
                now.tm_mday = acct_per.tm_mday;
        }
        gnc_tm_set_day_start(&now);
    }
    else if (type == RelativeDateType::END)
    {
        set_last_day_in_month(now);
        if (is_offset_quarter)
        {
            if (!is_last_day_in_month(acct_per) && now.tm_mday > acct_per.tm_mday)
                now.tm_mday = acct_per.tm_mday;
            --now.tm_mday;
        }
        gnc_tm_set_day_end(&now);
    }
    // Do nothing for LAST and NEXT.
};

time64
gnc_relative_date_to_time64(RelativeDatePeriod period, time64 now_t)
{
    if (period == RelativeDatePeriod::TODAY)
        return now_t;
    if (period == RelativeDatePeriod::START_ACCOUNTING_PERIOD)
        return gnc_accounting_period_fiscal_start();
    if (period == RelativeDatePeriod::END_ACCOUNTING_PERIOD)
        return gnc_accounting_period_fiscal_end();

    auto now{static_cast<tm>(GncDateTime(now_t))};
    struct tm acct_per =
        static_cast<tm>(GncDateTime(gnc_accounting_period_fiscal_start()));
    auto offset = reldate_offset(period);
    bool is_offset_quarter =
        offset == RelativeDateOffset::QUARTER && acct_per.tm_mday > 1;

    switch(offset)
    {
        case RelativeDateOffset::NONE:
            // Report on today so nothing to do
            break;
        case RelativeDateOffset::YEAR:
            if (reldate_is_prev(period))
                --now.tm_year;
            else if (reldate_is_next(period))
                ++now.tm_year;
            if (gnc_relative_date_is_starting(period))
                now.tm_mon = 0;
            else if (gnc_relative_date_is_ending(period))
                now.tm_mon = 11;
            break;
        case RelativeDateOffset::SIX:
            if (reldate_is_prev(period))
                now.tm_mon -= 6;
            else if (reldate_is_next(period))
                now.tm_mon += 6;
            break;
        case RelativeDateOffset::QUARTER:
        {
            auto delta = (12 + now.tm_mon - acct_per.tm_mon) % 3;
            if (is_offset_quarter)
            {
                if (delta == 0 && !is_last_day_in_month(now) && 
                    (is_last_day_in_month(acct_per) ||
                     now.tm_mday < acct_per.tm_mday))
                    delta = 3;
                if (gnc_relative_date_is_ending(period))
                    --delta;
            }
            now.tm_mon -= delta;
        }
            [[fallthrough]];
        case RelativeDateOffset::THREE:
            if (reldate_is_prev(period))
                now.tm_mon -= 3;
            else if (reldate_is_next(period))
                now.tm_mon += 3;
            if (gnc_relative_date_is_ending(period))
                now.tm_mon += 2;
            break;
        case RelativeDateOffset::MONTH:
            if (reldate_is_prev(period))
                --now.tm_mon;
            else if (reldate_is_next(period))
                ++now.tm_mon;
            break;
        case RelativeDateOffset::WEEK:
            if (reldate_is_prev(period))
                now.tm_mday -= 7;
            else if (reldate_is_next(period))
                now.tm_mday += 7;
    }
    reldate_set_day_and_time( now,
                              checked_reldate(period).m_type,
                              is_offset_quarter,
                              acct_per);
    normalize_reldate_tm(now);
    return static_cast<time64>(GncDateTime(now));
}

std::ostream&
operator<<(std::ostream& ostr, RelativeDatePeriod per)
{
    ostr << "'reldate . " << gnc_relative_date_display_string(per);
    return ostr;
}
