/********************************************************************\
 * calendar-lib.h -- An Interface for all needed function that use  *
 *                    in GncCalendar widget, this interface         *
 *                    implimented in diffrent Calendar logic        *
 *                    and then just need add the calendar type      *
 *                    to global GncCalendarType                     *
 *                                                                  *
 * Copyright 2016 Amin Aghabiki <amin[dot]aghabeiki[at]gmail.com>   *
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

#ifndef GNUCASH_CALENDAR_LIB_H
#define GNUCASH_CALENDAR_LIB_H
#include "config.h"
#include <glib.h>

enum {
    MONTH_PREV,
    MONTH_CURRENT,
    MONTH_NEXT
};
///
typedef struct CalendarLib {

    /// month_length - A 2D array pointer that point to month lenght in
    /// target calendar, the 0 index of array is non-leap year and the 1
    /// index point to leap year month lenght in specefic Calendar
    /// all the 12 column point to each of 12 month in a year
    guint **month_length;

    /// days_in_months - a 2D array pointer that contain the sum of
    /// all days from start of a year till the target indexed month the
    /// columns are 12 cell and array has 2 row the 0 for non-leap yaer
    /// and 1 for leap year
    guint **days_in_months;

    /// calc_days - this function return the sum of all days from
    /// from 1/1/1 to the specefic date
    ///
    /// \param year , guint - a supported calendar date year part  , like 2016
    /// \param mm   , guint - a supported calendar date month part , like 1, should between 1-12
    /// \param dd   , guint - a supported calendar date day part   , like 1, should between 1-31 ( or month lenght)
    /// \return     , glong - the sum of days between 1/1/1 till passed date
    glong (*calc_days) (guint year, guint mm, guint dd);

    /// day_of_week - return the week day number for specefic date

    /// \param year - guint , supported calendar date year part , like 2016
    /// \param mm   - guint , supported calendar date month part, like 1 , should between 1-12
    /// \param dd   - guint , supported calendar date day part  , like 1 , should between 1-31 ( or month lenght)
    /// \return     - guint , a number between 0-6
    guint (*day_of_week) (guint year, guint mm, guint dd);

    /// \dates_difference - calculate the diffrent date between two passed date
    ///
    /// \param year1 , guint , a gregorian date year part , like 2016
    /// \param mm1   , guint , a gregorian date month part, like 1    , should be between 1-12
    /// \param dd1   , guint , a gregorian date day part  , like 1    , should be between 1-31 ( or month length)
    /// \param year2 , guint , a gregorian date year part , like 2016
    /// \param mm2   , guint , a gregorian date month part, like 1    , should be between 1-12
    /// \param dd2   , guint , a gregorian date day part  , like 1    , should be between 1-31 ( or month length)
    /// \return      , glong , sum of days between two passed dates
    glong (*dates_difference) (guint year1, guint mm1, guint dd1,
                               guint year2, guint mm2, guint dd2);

    /// weeks_in_year - calculate the number of week from the first of year based a year
    /// \param year   , guint , a gregorian date year part , like 2016
    /// \return       , guint , the week number
    guint (*weeks_in_year) (guint year);

    /// week_of_year - check the week number is currect in specefic date
    /// \param week  , guint   , the week number
    /// \param year  , guint   , a gregorian date year part
    /// \param mm    , guint   , a gregorian date month part
    /// \param dd    , guint   , a gregorian date day part
    /// \return      , gboolean, TRUE if the week number is correct in specefic date
    gboolean (*week_of_year) (guint *week, guint *year, guint mm, guint dd);

    /// year_to_days - calculate the sum of all days from start 1/1/1 to specefic year
    /// \param year  , guint , a gregorian date year part, like 2016
    /// \return      , glong , sum of days from 1/1/1 to the passed year
    glong (*year_to_days) (guint year);

    /// check_date  - check the date is currect or not
    /// \param year , guint    , a gregorian date year part , like 2016
    /// \param mm   , guint    , a gregorian date month part, like 1 , should be between 1-12
    /// \param dd   , guint    , a gregorian date day part  , like 1 , should be between 1-31 or month length
    /// \return     , gboolean , TRUE if the date is currect and FALSE if date is not currect
    gboolean (*check_date) (guint year, guint mm, guint dd);

    /// leap        - check if the passed year is leap or not
    /// \param year , guint    , a gregorian date year part , like 2016
    /// \return     , gboolean , TRUE if the passed year is leap and FALSE if the year is non-leap
    gboolean (*leap) (guint year);

    /// week_number - return the week number of specefic date
    /// \param year , guint , a gregorian date year part , like 2016
    /// \param mm   , guint , a gregorian date month part, like 1 , should be between 1-12
    /// \param dd   , guint , a gregorian date day part  , like 1 , should be between 1-31 or month length
    /// \return
    guint (*week_number) (guint year, guint mm, guint dd);

    /// compute_paint_days - campute the days that should be showen in a 6*7 rows
    ///                         calendar widget
    /// \param year        , a const guint , a gregorian date year part , like 2016
    /// \param month       , a const guint , a gregorian date month part, like 1 , should be between 1-12
    /// \param g_day       , a const guint , a gregorian date day part  , like 1 , should be between 1-31 or month lenght
    /// \param week_start  , gint          , the start week name, number, like 3 -> saturday
    /// \param day         , gint array    , output of the calculated date
    /// \param day_month   , gint array    , mark for prev current and next month
    void (*compute_paint_days) (const gint year, const gint month,
                                const gint g_day, gint week_start,
                                gint day[6][7], gint day_month[6][7]);

    /// init_day_name                        - initialize the target calendar day name
    /// \param default_abbreviated_dayname   , char array pointer , an 7 cell array that contain the day's name
    void (*init_day_name) (char *default_abbreviated_dayname[7]);

    /// init_month_name             - initialize the target calendar month name
    /// \param default_monthname    , a char array pointer , an 12 cell array that contain the month's name
    void (*init_month_name) (char *default_monthname[12]);

    /// paint_header_helper   - this function healp print header function in GncCalendar
    ///                          to display the selected month name in selected calendar
    /// \param g_yy          , const gint   , a gregorian date year part , like 2016
    /// \param g_mm          , const gint   , a gregorian date month part, like 1 , should be between 1-12
    /// \param g_dd          , const gint   , a gregorian date day part  , like 1 , should be between 1-31 or month length
    /// \param display_month , gint pointer , output- the month index that should be showen on header
    /// \return              , gchar pointer, output- the year that should be showen on header
    gchar *(*paint_header_helper) (const gint g_yy, const gint g_mm,
                                   const gint g_dd, gint *display_month);

    /// display_selected_day - return the current day index in target calendar
    /// \param g_yy          , const gint , a gregorian date year part , like 2016
    /// \param g_mm          , const gint , a gregorian date month part, like 1 , should be between 1-12
    /// \param g_dd          , const gint , a gregorian date day part  , like 1 , should be between 1-31 or month lenght
    /// \return              , the current or selected date index ( based 1-31 ) in target Calendar , like 1
    gint (*display_selected_day) (const gint g_yy, const gint g_mm, const gint g_dd);

    /// invalidate_display_day_to_gregorian - invalidate and convert back the display date to a gregorian date
    /// \param selected_year                , const gint  , a supported date year part , like 1395 ( jalalian date )
    /// \param selected_month               , const gint  , a supported date month part, like 1 , should be between 1-12
    /// \param selected_day                 , const gint  , a supported date day part  , like 1 , should be between 1-31 or month lenght
    /// \param gregorian_year               , gint pointer, output - converted gregorian date year part , like 2016
    /// \param gregorian_month              , gint pointer, output - converted gregorian date month part, like 1
    /// \param gregorian_day                , gint pointer, output - converted gregorian date day part  , like 1
    void
    (*invalidate_display_day_to_gregorian) (const gint selected_year, const gint selected_month,
                                            const gint selected_day, gint *gregorian_year,
                                            gint *gregorian_month, gint *gregorian_day);

} CalendarLib;

#endif //GNUCASH_CALENDAR_LIB_H
