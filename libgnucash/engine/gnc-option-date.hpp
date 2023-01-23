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
/** @addtogroup Engine
    @{ */
/** @addtogroup Options
    @{ */
/** @file gnc-option-date.hpp
    @brief Relative date enumeration and manipulation functions.
    @author Copyright 2019-2021 John Ralls <jralls@ceridwen.us>
*/
#ifndef GNC_OPTION_DATE_HPP_
#define GNC_OPTION_DATE_HPP_

#include "gnc-date.h"

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

constexpr unsigned relative_date_periods =
    static_cast<unsigned>(RelativeDatePeriod::END_ACCOUNTING_PERIOD) + 2;

using RelativeDatePeriodVec = std::vector<RelativeDatePeriod>;

/**
 * Report whether the relative date represents a period offset to today's date
 * rather than the beginning or end of a date range. For example ONE_MONTH_AGO
 * will be made concrete as the same day as today in the previous month.
 *
 * @param period The Relative Date Period to check.
 * @return true if the date is stand-alone.
 */
bool gnc_relative_date_is_single(RelativeDatePeriod);

/**
 * Report whether the relative date represents the beginning of a date
 * range. For example START_LAST_MONTH is the beginning of a range.
 *
 * @param period The Relative Date Period to check.
 * @return true if the date is the beginning of a date range
 */
bool gnc_relative_date_is_starting(RelativeDatePeriod);

/**
 * Report whether the relative date represents the end of a date range. For
 * example END_LAST_MONTH is the end of a range.
 *
 * @param period The Relative Date Period to check.
 * @return true if the date is the end of a date range.
 */
bool gnc_relative_date_is_ending(RelativeDatePeriod);

/**
 * Provide the string representation of a relative date for persisting the
 * value. This string is not localizable.
 *
 * @param period The relative date period.
 * @return A constant string or nullptr if the period is ABSOLUTE. The string's
 * lifetime will be that of the Relative Date Period. It must not be freed and
 * should be copied if the period might be destroyed before the using code is
 * finished.
 */
const char* gnc_relative_date_storage_string(RelativeDatePeriod);

/**
 * Provide the string representation of a relative date for displaying
 * value to a user. This string is localizable.
 *
 * @param period The relative date period.
 * @return A constant string or nullptr if the period is ABSOLUTE. The string's
 * lifetime will be that of the Relative Date Period. It must not be freed and
 * should be copied if the period might be destroyed before the using code is
 * finished.
 */
const char* gnc_relative_date_display_string(RelativeDatePeriod);

/**
 * Provide the description of a relative date. This string is localizable.
 *
 * @param period The relative date period.
 * @return A constant string or nullptr if the period is ABSOLUTE. The string's
 * lifetime will be that of the Relative Date Period. It must not be freed and
 * should be copied if the period might be destroyed before the using code is
 * finished.
 */
const char* gnc_relative_date_description(RelativeDatePeriod);

/**
 * Convert a relative date storage string back to a RelativeDatePeriod value.
 *
 * @param A string representation obtained from
 * gnc_relative_date_storage_string.
 * @return A RelativeDatePeriod value.
 */
RelativeDatePeriod gnc_relative_date_from_storage_string(const char*);

/**
 * Convert a RelativeDatePeriod value to a concrete time64 by applying the value
 * to the current time.
 * For example if it is now 3:15:42 PM local time 3 June, calling this with a
 * period RelativeDatePeriod::ONE_WEEK_AHEAD will return a time64 representing
 * 3:15:42 PM local time 10 June of this year. Times for START periods are
 * changed to midnight local time and for END periods to 23:59:59 local time so
 * for example if the period is instead RelativeDatePeriod::START_THIS_MONTH the
 * time64 will represent 00:00:00 1 June and if it is
 * RelativeDatePeriod::END_THIS_MONTH the time64 will be for 23:59:59 30 June,
 * both in the current time zone.
 *
 * @param period The relative date period to use to calculate the concrete date.
 * @return a time64.
 */
time64 gnc_relative_date_to_time64(RelativeDatePeriod);

/**
 * Add the display string to the provided std::ostream.
 *
 * @param stream the std::ostream to which to write the period value
 * @param period the period value to write
 * @return A reference to stream so that the operator can be chained.
 */
std::ostream& operator<<(std::ostream&, const RelativeDatePeriod);

#endif //GNC_OPTION_DATE_HPP_

/** @}
    @} */
