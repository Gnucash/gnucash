/********************************************************************\
 * gnc-option-date.hpp -- Relative dates for options                *
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
 * along with this program; if not, contact:                        *
 *                                                                  *
 * Free Software Foundation           Voice:  +1-617-542-5942       *
 * 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652       *
 * Boston, MA  02110-1301,  USA       gnu@gnu.org                   *
 *                                                                  *
\********************************************************************/

#ifndef GNC_OPTION_DATE_HPP_
#define GNC_OPTION_DATE_HPP_

extern "C"
{
#include <gnc-date.h>
}

#include <vector>
#include <iostream>
/**
 * Reporting periods relative to the current date.
 *
 * The original design allowed custom RelativeDatePeriods, but that facility is
 * unused so we'll go with compiled-in enums.
 */
enum class RelativeDatePeriod : int
{
    ABSOLUTE = -1,
    TODAY,
    ONE_WEEK_AGO,
    ONE_WEEK_AHEAD,
    ONE_MONTH_AGO,
    ONE_MONTH_AHEAD,
    THREE_MONTHS_AGO,
    THREE_MONTHS_AHEAD,
    SIX_MONTHS_AGO,
    SIX_MONTHS_AHEAD,
    ONE_YEAR_AGO,
    ONE_YEAR_AHEAD,
    START_THIS_MONTH,
    END_THIS_MONTH,
    START_PREV_MONTH,
    END_PREV_MONTH,
    START_NEXT_MONTH,
    END_NEXT_MONTH,
    START_CURRENT_QUARTER,
    END_CURRENT_QUARTER,
    START_PREV_QUARTER,
    END_PREV_QUARTER,
    START_NEXT_QUARTER,
    END_NEXT_QUARTER,
    START_CAL_YEAR,
    END_CAL_YEAR,
    START_PREV_YEAR,
    END_PREV_YEAR,
    START_NEXT_YEAR,
    END_NEXT_YEAR,
    START_ACCOUNTING_PERIOD,
    END_ACCOUNTING_PERIOD,
};

using RelativeDatePeriodVec = std::vector<RelativeDatePeriod>;

bool gnc_relative_date_is_single(RelativeDatePeriod);
bool gnc_relative_date_is_starting(RelativeDatePeriod);
bool gnc_relative_date_is_ending(RelativeDatePeriod);
const char* gnc_relative_date_storage_string(RelativeDatePeriod);
const char* gnc_relative_date_display_string(RelativeDatePeriod);
const char* gnc_relative_date_description(RelativeDatePeriod);
RelativeDatePeriod gnc_relative_date_from_storage_string(const char*);
time64 gnc_relative_date_to_time64(RelativeDatePeriod);
std::ostream& operator<<(std::ostream&, const RelativeDatePeriod);

#endif //GNC_OPTION_DATE_HPP_
