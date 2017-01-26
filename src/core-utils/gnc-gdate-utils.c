/*
 * gnc-gdate-utils.c -- utility functions for manipulating
 *              GDate data structures from GLib
 * Copyright (C) 2005 David Hampton <hampton@employees.org>
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; either version 2 of
 * the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, contact:
 *
 * Free Software Foundation           Voice:  +1-617-542-5942
 * 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652
 * Boston, MA  02110-1301,  USA       gnu@gnu.org
 */

#include "config.h"
#include <glib.h>

#include "gnc-gdate-utils.h"
#include "gnc-jalali.h"

void
gnc_gdate_set_today (GDate* gd)
{
    GDate *today = gnc_g_date_new_today ();
    g_date_set_julian (gd, g_date_get_julian (today));
    g_date_free (today);
}

void
gnc_gdate_set_time64 (GDate* gd, time64 time)
{
    struct tm tm;
    gnc_localtime_r(&time, &tm);
    g_date_set_dmy (gd, tm.tm_mday, tm.tm_mon + 1, tm.tm_year + 1900);

}

gboolean
gnc_gdate_equal(gconstpointer gda, gconstpointer gdb)
{
    return (g_date_compare( (GDate*)gda, (GDate*)gdb ) == 0 ? TRUE : FALSE);
}

guint
gnc_gdate_hash( gconstpointer gd )
{
    gint val = (g_date_get_year( (GDate*)gd ) * 10000)
               + (g_date_get_month( (GDate*)gd ) * 100)
               + g_date_get_day( (GDate*)gd );
    return g_int_hash( &val );
}


time64
gnc_time64_get_day_start_gdate (const GDate *date)
{
    struct tm stm;
    time64 secs;

    /* First convert to a 'struct tm' */
    g_date_to_struct_tm (date, &stm);

    /* Then convert to number of seconds */
    secs = gnc_mktime (&stm);
    return secs;
}

time64
gnc_time64_get_day_end_gdate (const GDate *date)
{
    struct tm stm;
    time64 secs;

    /* First convert to a 'struct tm' */
    g_date_to_struct_tm(date, &stm);

    /* Force to th last second of the day */
    stm.tm_hour = 23;
    stm.tm_min = 59;
    stm.tm_sec = 59;
    stm.tm_isdst = -1;

    /* Then convert to number of seconds */
    secs = gnc_mktime (&stm);
    return secs;
}


void
gnc_gdate_set_month_start (GDate *date)
{

    gint g_tmp_y;
    gint g_tmp_m;
    gint g_tmp_d;


    g_tmp_d=-1;
    g_tmp_m=-1;
    g_tmp_y=-1;

    switch (gnc_calendar_type_get ())
    {
        case GNC_CALENDAR_TYPE_GREGORIAN:
            g_date_set_day(date, 1);
            break;
        case GNC_CALENDAR_TYPE_JALALI:
            gnc_beginning_jalalian_month(g_date_get_year (date),g_date_get_month (date)
                ,g_date_get_day (date),&g_tmp_y,&g_tmp_m,&g_tmp_d);
            if( g_tmp_d==-1 || g_tmp_m==-1 || g_tmp_y ==-1)
            {
                g_date_set_day(date, 1);
            } else{
              g_date_set_dmy (date, (GDateDay) g_tmp_d, (GDateMonth) g_tmp_m, (GDateYear) g_tmp_y);
            }

            break;
        case GNC_UNDEFINED_CALENDAR_TYPE:
          // todo LOG IT
          break;
    }


}


/** Convert a GDate to the last day of the month.  This routine has no
 *  knowledge of how many days are in a month, whether its a leap
 *  year, etc.  All that information is contained in the glib date
 *  functions.
 *
 *  @param date The GDate to modify.
 */
void
gnc_gdate_set_month_end (GDate *date)
{
    gint g_tmp_y;
    gint g_tmp_m;
    gint g_tmp_d;

    g_tmp_y=-1;
    g_tmp_m=-1;
    g_tmp_d=-1;
    switch (gnc_calendar_type_get ())
    {
        case GNC_CALENDAR_TYPE_GREGORIAN:
            /* First set the start of next month. */
            g_date_set_day(date, 1);
            g_date_add_months(date, 1);

            /* Then back up one day */
            g_date_subtract_days(date, 1);
            break;
        case GNC_CALENDAR_TYPE_JALALI:
            gnc_end_of_jalalian_month(g_date_get_year (date),g_date_get_month (date)
                ,g_date_get_day (date),&g_tmp_y,&g_tmp_m,&g_tmp_d);
            if( g_tmp_d==-1 || g_tmp_m==-1 || g_tmp_y ==-1)
            {
                g_date_set_day(date, 1);
            } else{
                g_date_set_dmy (date, (GDateDay) g_tmp_d, (GDateMonth) g_tmp_m, (GDateYear) g_tmp_y);
            }
            break;
        case GNC_UNDEFINED_CALENDAR_TYPE:
          // todo LOG it
          break;
    }

}


/** Convert a GDate to the first day of the prebvious month.  This
 *  routine has no knowledge of how many days are in a month, whether
 *  its a leap year, etc.  All that information is contained in the
 *  glib date functions.
 *
 *  @param date The GDate to modify.
 */
void
gnc_gdate_set_prev_month_start (GDate *date)
{
    g_date_set_day(date, 1);
    g_date_subtract_months(date, 1);
}


/** Convert a GDate to the last day of the prebvious month.  This
 *  routine has no knowledge of how many days are in a month, whether
 *  its a leap year, etc.  All that information is contained in the
 *  glib date functions.
 *
 *  @param date The GDate to modify.
 */
void
gnc_gdate_set_prev_month_end (GDate *date)
{
    /* This will correctly handle the varying month lengths */
    g_date_set_day(date, 1);
    g_date_subtract_days(date, 1);
}

/* ========== */

void
gnc_gdate_set_quarter_start (GDate *date)
{
    gint months;

    /* Set the date to the first day of the specified month. */
    g_date_set_day(date, 1);

    /* Back up 0-2 months */
    months = (g_date_get_month(date) - G_DATE_JANUARY) % 3;
    g_date_subtract_months(date, months);
}


void
gnc_gdate_set_quarter_end (GDate *date)
{
    gint months;

    /* Set the date to the first day of the specified month. */
    g_date_set_day(date, 1);

    /* Add 1-3 months to get the first day of the next quarter.*/
    months = (g_date_get_month(date) - G_DATE_JANUARY) % 3;
    g_date_add_months(date, 3 - months);

    /* Now back up one day */
    g_date_subtract_days(date, 1);
}


void
gnc_gdate_set_prev_quarter_start (GDate *date)
{
    gnc_gdate_set_quarter_start(date);
    g_date_subtract_months(date, 3);
}


void
gnc_gdate_set_prev_quarter_end (GDate *date)
{
    gnc_gdate_set_quarter_end(date);
    g_date_subtract_months(date, 3);
}

/* ========== */

void
gnc_gdate_set_year_start (GDate *date)
{
    gint g_tmp_y;
    gint g_tmp_m;
    gint g_tmp_d;

    g_tmp_y=-1;
    g_tmp_m=-1;
    g_tmp_d=-1;

    switch (gnc_calendar_type_get ())
    {
        case GNC_CALENDAR_TYPE_GREGORIAN:
            g_date_set_month(date, G_DATE_JANUARY);
            g_date_set_day(date, 1);
            break;
        case GNC_CALENDAR_TYPE_JALALI:
            gnc_beginning_jalalian_year(g_date_get_year (date),g_date_get_month (date)
                ,g_date_get_day (date),&g_tmp_y,&g_tmp_m,&g_tmp_d);
            if( g_tmp_y==-1|| g_tmp_m==-1|| g_tmp_d==-1)
            {
                g_date_set_month(date, G_DATE_JANUARY);
                g_date_set_day(date, 1);
            }
            else
            {
                g_date_set_dmy (date, (GDateDay) g_tmp_d, (GDateMonth) g_tmp_m, (GDateYear) g_tmp_y);
            }
            break;
        case GNC_UNDEFINED_CALENDAR_TYPE:
          // todo LOG IT
          break;
    }


}


void
gnc_gdate_set_year_end (GDate *date)
{
    gint g_tmp_y;
    gint g_tmp_m;
    gint g_tmp_d;
    g_tmp_y=-1;
    g_tmp_m=-1;
    g_tmp_d=-1;
    switch (gnc_calendar_type_get ())
    {
        case GNC_CALENDAR_TYPE_GREGORIAN:
            g_date_set_month(date, G_DATE_DECEMBER);
            g_date_set_day(date, 31);
        break;
        case GNC_CALENDAR_TYPE_JALALI:
            gnc_end_of_jalalian_year(g_date_get_year (date),g_date_get_month (date),g_date_get_day (date),&g_tmp_y,&g_tmp_m,&g_tmp_d);
            if(g_tmp_d==-1|| g_tmp_m==-1|| g_tmp_y==-1)
            {
                g_date_set_month(date, G_DATE_DECEMBER);
                g_date_set_day(date, 31);
            } else{
                g_date_set_dmy (date, (GDateDay) g_tmp_d, (GDateMonth) g_tmp_m, (GDateYear) g_tmp_y);
            }
            break;
        case GNC_UNDEFINED_CALENDAR_TYPE:
          // todo LOG IT
          break;


    }


}


void
gnc_gdate_set_prev_year_start (GDate *date)
{
    gnc_gdate_set_year_start(date);
    g_date_subtract_years(date, 1);
}


void
gnc_gdate_set_prev_year_end (GDate *date)
{
    gnc_gdate_set_year_end(date);
    g_date_subtract_years(date, 1);
}

/* ========== */

void
gnc_gdate_set_fiscal_year_start (GDate *date,
                                 const GDate *fy_end)
{
    GDate temp;
    gboolean new_fy;

    g_return_if_fail(date);
    g_return_if_fail(fy_end);

    /* Compute the FY end that occurred this CY */
    temp = *fy_end;
    g_date_set_year(&temp, g_date_get_year(date));

    /* Has it already passed? */
    new_fy = (g_date_compare(date, &temp) > 0);

    /* Set start date */
    *date = temp;
    g_date_add_days(date, 1);
    if (!new_fy)
        g_date_subtract_years(date, 1);
}

void
gnc_gdate_set_fiscal_year_end (GDate *date,
                               const GDate *fy_end)
{
    GDate temp;
    gboolean new_fy;

    g_return_if_fail(date);
    g_return_if_fail(fy_end);

    /* Compute the FY end that occurred this CY */
    temp = *fy_end;
    g_date_set_year(&temp, g_date_get_year(date));

    /* Has it already passed? */
    new_fy = (g_date_compare(date, &temp) > 0);

    /* Set end date */
    *date = temp;
    if (new_fy)
        g_date_add_years(date, 1);
}

void
gnc_gdate_set_prev_fiscal_year_start (GDate *date,
                                      const GDate *fy_end)
{
    g_return_if_fail(date);
    g_return_if_fail(fy_end);

    gnc_gdate_set_fiscal_year_start(date, fy_end);
    g_date_subtract_years(date, 1);
}

void
gnc_gdate_set_prev_fiscal_year_end (GDate *date,
                                    const GDate *fy_end)
{
    g_return_if_fail(date);
    g_return_if_fail(fy_end);

    gnc_gdate_set_fiscal_year_end(date, fy_end);
    g_date_subtract_years(date, 1);
}
void
gnc_gdate_next_month (GDate *date)
{
    gint g_ny;
    gint g_nm;
    gint g_nd;
    GncCalendarType calendarType;
    g_ny=-1;
    g_nm=-1;
    g_nd=-1;

    calendarType= gnc_calendar_type_get ();

    switch (calendarType)
    {
        case GNC_CALENDAR_TYPE_GREGORIAN:
            g_date_add_months (date, 1);
        break;
        case GNC_CALENDAR_TYPE_JALALI:
            gnc_next_jalalian_month(g_date_get_year (date),g_date_get_month (date)
                ,g_date_get_day (date),&g_ny,&g_nm,&g_nd);
            if( g_nd==-1 || g_nm==-1 || g_ny==-1)
            {
                g_date_add_months(date,1);
            } else
            {
                g_date_set_dmy (date, (GDateDay) g_nd, (GDateMonth) g_nm, (GDateYear) g_ny);
            }
        break;
        case GNC_UNDEFINED_CALENDAR_TYPE:
          // todo LOG IT
          break;

    }

}

void
gnc_gdate_prev_month (GDate *date)
{
    GncCalendarType calendarType;
    gint g_ny;
    gint g_nm;
    gint g_nd;


    calendarType= gnc_calendar_type_get ();
    g_ny=-1;
    g_nm=-1;
    g_nd=-1;

    switch (calendarType)
    {
        case GNC_CALENDAR_TYPE_GREGORIAN:
            g_date_subtract_months(date,1);
        break;
        case GNC_CALENDAR_TYPE_JALALI:
            gnc_prev_jalalian_month(g_date_get_year (date),g_date_get_month (date),
                g_date_get_day (date),&g_ny,&g_nm,&g_nd);
            if( g_nd==-1 || g_nm==-1 || g_ny==-1)
            {
                g_date_subtract_months(date,1);
            } else
            {
                g_date_set_dmy (date, (GDateDay) g_nd, (GDateMonth) g_nm, (GDateYear) g_ny);
            }
        break;
        case GNC_UNDEFINED_CALENDAR_TYPE:
          // todo LOG IT
          break;
    }

}



// used in Recurrence to handle Date Picker Events

void         gnc_date_subtract_days         (GDate       *date,
                                             guint        n_days)
{
    // maybe user use a masked calendar , so we need to be return to the Masked Calendar
    // and start calculation in the masked calendar and then return back again to Gregorian
    guint  masked_day;
    guint  masked_month;
    guint  masked_year;

    // the out_gregorian var used for get out put of calculation
    guint g_day;
    guint g_month;
    guint g_year;

    // the requested days should not be less then 0
    if(n_days<1)
        return;


    switch (gnc_calendar_type_get ())
    {
        case GNC_CALENDAR_TYPE_GREGORIAN:
            // nothing need be change
            g_date_subtract_days(date,n_days);
            break;
        case GNC_CALENDAR_TYPE_JALALI:
            // first revert gregorian to Jalalian
            gnc_gregorian_to_jalali((int *) &masked_year, (int *) &masked_month, (int *) &masked_day,
                                    g_date_get_year (date), g_date_get_month (date),g_date_get_day (date));

            if ( masked_year == -1 || masked_month  == -1 || masked_day == -1)
            {
                // its a problem in conversation , and TODO LOG IT
                g_date_subtract_days(date,n_days);
            } else
            {
                // ok we have the correct day so let do the rest thing

                // subtracts days from today
                masked_day-=n_days;

                // check if the current day is less then 0
                while (masked_day <=0)
                {

                    // we add last month to the current value to be day Positive

                    if( masked_month ==12 && gnc_jalali_is_leap_year(masked_year))
                    {
                        // if the year is leap last month has one more day
                        masked_day += (gnc_jalali_days_in_month(masked_month-1)+1);
                    } else
                    {
                        masked_day += gnc_jalali_days_in_month(masked_month-1);
                    }

                    // go back one month
                    masked_month--;

                    // if the month is less then 0 mean we should lock last year
                    if ( masked_month <= 0)
                    {
                        masked_month=1;
                        masked_year--;
                    }

                }


                // return the date to Gregorian
                gnc_jalali_to_gregorian((int *) &g_year, (int *) &g_month, (int *) &g_day, masked_year, masked_month, masked_day);

                // final check the value is correct

                if ( g_year !=-1 && g_month && -1 && g_day !=-1)
                {
                    // ok the value is correct , return the value in Gdate Param
                    g_date_set_dmy (date, (GDateDay) g_day, (GDateMonth) g_month, (GDateYear) g_year);

                } else
                {
                    // ok its look some thing is wrong I just call the general function and TODO LOG IT
                    g_date_subtract_days(date,n_days);
                }
            }// else
            break; // jalalian end

        default:
            // do the default function for unknown calendar type !
            g_date_subtract_days(date,n_days);
            break;

    }// switch case
}

void         gnc_date_add_days              (GDate       *date,
                                             guint        n_days)
{

    // maybe user use a masked calendar , so we need to be return to the Masked Calendar
    // and start calculation in the masked calendar and then return back again to Gregorian
    guint  masked_day;
    guint  masked_month;
    guint  masked_year;

    // the out_gregorian var used for get out put of calculation
    guint g_day;
    guint g_month;
    guint g_year;

    // the requested day should not be less then 0
    if(n_days<1)
        return;

    switch (gnc_calendar_type_get ())
    {
        case GNC_CALENDAR_TYPE_GREGORIAN:
            // nothing need be change
            g_date_add_days(date,n_days);
            break;
        case GNC_CALENDAR_TYPE_JALALI:
            // first revert gregorian to Jalalian
            gnc_gregorian_to_jalali((int *) &masked_year, (int *) &masked_month, (int *) &masked_day
                ,g_date_get_year (date),g_date_get_month (date),g_date_get_day (date));

            if ( masked_year == -1 || masked_month == -1 || masked_day == -1)
            {
                // its a problem in conversation , and TODO LOG IT

                g_date_add_days(date,n_days);
            } else
            {
                // ok we have the correct day so let do the rest thing

                // add days to today
                masked_day+=n_days;

                // check the day is in the month length
                // its a loop case maybe user input a large day numbers !
                while (masked_day > gnc_jalali_days_in_month(masked_month-1))
                {
                    // if its the last years in Jalalian , I need to check its a leap years too or not
                    if ( masked_month == 12 && gnc_jalali_is_leap_year(masked_year)) // check leap year !
                    {
                        // if the day is inside a extended Month length ( leap year ) the loop will be done
                        if ( masked_day <= (gnc_jalali_days_in_month(masked_month-1) +1 ) )
                            break;
                        else
                        {
                            // else calculate the real month and dedicate the day count of month
                            masked_day= masked_day- (gnc_jalali_days_in_month(masked_month-1)+1);
                            masked_month++;
                        }


                    } else{

                        // calculate the real month and dedicate the day count of month
                        masked_day= masked_day- gnc_jalali_days_in_month(masked_month-1);
                        masked_month++;

                    }
                    // if month is more then 12 we should be back to first month of year and
                    // add a new year !
                    if ( masked_month > 12 )
                    {
                        masked_month=1;
                        masked_year++;
                    }
                }


                // we have the added day to

                gnc_jalali_to_gregorian((int *) &g_year, (int *) &g_month, (int *) &g_day
                    , masked_year, masked_month, masked_day);

                // final check the value is currect

                if ( g_year !=-1 && g_month && -1 && g_day !=-1)
                {
                    // ok the value is correct , return the value in Gdate Param
                    g_date_set_dmy (date, (GDateDay) g_day, (GDateMonth) g_month, (GDateYear) g_year);

                } else
                {
                    // ok its look some thing is wrong I just call the general function and TODO LOG IT
                    g_date_add_days(date,n_days);
                }
            }// else
            break; // jalalian end

        default:
            // do the default function for unknown calendar type !
            g_date_add_days(date,n_days);
            break;

    }// switch case


}


void         gnc_date_set_day               (GDate       *date,
                                             GDateDay     day)
{

    // maybe user use a masked calendar , so we need to be return to the Masked Calendar
    // and start calculation in the masked calendar and then return back again to Gregorian
    guint  masked_day;
    guint  masked_month;
    guint  masked_year;

    // the out_gregorian var used for get out put of calculation
    guint g_day;
    guint g_month;
    guint g_year;
    // the requested day for set should not be less then 0
    if(day <1)
        return;
    switch (gnc_calendar_type_get ())
    {
        case GNC_CALENDAR_TYPE_GREGORIAN:
            // nothing need be change
            g_date_set_day(date,day);
            break;
        case GNC_CALENDAR_TYPE_JALALI:
            // first revert gregorian to Jalalian
            gnc_gregorian_to_jalali((int *) &masked_year, (int *) &masked_month, (int *) &masked_day,
                                    g_date_get_year(date), g_date_get_month(date), g_date_get_day(date));

            if ( masked_year ==-1 || masked_month ==-1 || masked_day == -1)
            {
                // its a problem in conversation , and TODO LOG IT

                g_date_set_day(date,day);
            } else
            {
                // ok we have the correct day so let do the rest thing

                // set the day in the field
                masked_day=day;

                // check the day is in the month length
                // its a loop case maybe user input a large day numbers !
                while (masked_day > gnc_jalali_days_in_month(masked_month-1))
                {
                    // if its the last years in Jalalian , I need to check its a leap years too or not
                    if ( masked_month == 12 && gnc_jalali_is_leap_year(masked_year)) // check leap year !
                    {
                        // if the day is inside a extended Month length ( leap year ) the loop will be done
                        if ( masked_day <= (gnc_jalali_days_in_month(masked_month-1) +1 ) )
                            break;
                        else
                        {
                            // else calculate the real month and dedicate the day count of month
                            masked_day= masked_day- (gnc_jalali_days_in_month(masked_month-1)+1);
                            masked_month++;
                        }


                    } else{

                        // calculate the real month and dedicate the day count of month
                        masked_day= masked_day- gnc_jalali_days_in_month(masked_month-1);
                        masked_month++;

                    }
                    // if month is more then 12 we should be back to first month of year and
                    // add a new year !
                    if ( masked_month > 12 )
                    {
                        masked_month=1;
                        masked_year++;
                    }
                }


                // we have the added day to

                gnc_jalali_to_gregorian((int *) &g_year, (int *) &g_month, (int *) &g_day, masked_year, masked_month, masked_day);

                // final check the value is correct

                if ( g_year !=-1 && g_month && g_day != -1)
                {
                    // ok the value is correct , return the value in GDate Param
                    g_date_set_dmy(date, (GDateDay) g_day, (GDateMonth) g_month, (GDateYear) g_year);

                } else
                {
                    // ok its look some thing is wrong I just call the general function and TODO LOG IT
                    g_date_set_day(date,day);
                }
            }// else
            break; // jalalian end

        default:
            // do the default function for unknown calendar type !
            g_date_set_day(date,day);
            break;

    }// switch case


}

GDateDay     gnc_date_get_day               (const GDate *date)
{

    // use for convert gregorian to masked date
    // the default value used for validation
    guint masked_day= (guint) -1;
    guint masked_month= (guint) -1;
    guint masked_year= (guint) -1;
    // this var use for sending output of function
    // the default value used for validation
    GDateDay  days_count=0;


    switch (gnc_calendar_type_get ())
    {
        case GNC_CALENDAR_TYPE_GREGORIAN:
            days_count=g_date_get_day(date);
            break;
        case GNC_CALENDAR_TYPE_JALALI:
            // first convert the date to Jalalian

            gnc_gregorian_to_jalali((int *) &masked_year, (int *) &masked_month, (int *) &masked_day
                    , g_date_get_year(date), g_date_get_month(date), g_date_get_day(date));

            // validate convert is correct
            if( masked_day == -1 || masked_month == -1 || masked_day ==-1)
            {
                // the conventions is wrong , so TODO LOG IT
                // and use default function
                days_count=g_date_get_day(date);
            }
            else
            {
                // set the day
                days_count= (GDateDay) masked_day;
            }

            break;
        default:
            days_count=g_date_get_day(date);

            break;

    }


    // return the value
    return days_count;

}

guint8       gnc_date_get_days_in_month     (GDateDay     day,
                                             GDateMonth   month,
                                             GDateYear    year)
{

    // used for calculation
    guint masked_day= (guint) -1;
    guint masked_month= (guint) -1;
    guint masked_year= (guint) -1;
    // used for output
    guint8 days_in_month=0;

    switch (gnc_calendar_type_get ())
    {
        case GNC_CALENDAR_TYPE_GREGORIAN:
            days_in_month=g_date_get_days_in_month(month,year);
            break;
        case GNC_CALENDAR_TYPE_JALALI:
            // first convert date to Jalalian
            gnc_gregorian_to_jalali((int *) &masked_year, (int *) &masked_month, (int *) &masked_day, year, month, day);
            // validate convert is correct
            if (masked_day == -1 || masked_month == -1 || masked_year == -1  )
            {
                // if conversation is incorrect use default function and TODO LOG IT
                days_in_month=g_date_get_days_in_month(month,year);
            }
            else
            {
                // get the month leng

                if (masked_month == 12 && gnc_jalali_is_leap_year(masked_year))
                {
                    days_in_month= (guint8) (gnc_jalali_days_in_month(masked_month - 1) + 1);
                }
                else
                {
                    days_in_month= (guint8) gnc_jalali_days_in_month(masked_month - 1);
                }
            }

            break; // jalali
        default:
            // for unknown calendar just use default function and TODO LOG IT
            days_in_month=g_date_get_days_in_month(month,year);
            break;

    }//switch


    // return days in month
    return days_in_month;

}

void         gnc_date_subtract_months       (GDate       *date,
                                             guint        n_months)
{
    // used for conversation from gregorian calendar to Jalalian calendar
    guint masked_day;
    guint masked_month;
    guint masked_year;
    // use for out put
    guint g_day;
    guint g_month;
    guint g_year;

    // calculation var
    guint added_tmp_month=0;
    // if n_month is less then 0 return ;
    if(n_months < 1)
        return;

    switch(gnc_calendar_type_get ())
    {
        case GNC_CALENDAR_TYPE_GREGORIAN:
            // for gregorian calendar use default calendar
            g_date_subtract_months(date,n_months);
            break;// end of Gregorian case
        case GNC_CALENDAR_TYPE_JALALI:
            // convert to Jalalian Date

            gnc_gregorian_to_jalali((int *) &masked_year, (int *) &masked_month, (int *) &masked_day
                    ,g_date_get_year(date),g_date_get_month(date),g_date_get_day(date));

            // validate conversation is correct
            if( masked_day ==-1 || masked_month  == -1 || masked_year == -1 )
            {
                // simply use default function and TODO LOG IT
                g_date_subtract_months(date,n_months);
            }
            else {
                // subtract month from current month
                masked_month-=n_months;
                // check the value is correct for month
                while( masked_month <= 0)
                {
                    masked_month++;
                    added_tmp_month++;
                }

                // remove the years
                masked_year -= ((added_tmp_month/12) + added_tmp_month*1);

                gnc_jalali_to_gregorian((int *) &g_year, (int *) &g_month, (int *) &g_day, masked_year, masked_month, masked_day);

                // validate the converted date
                if(g_day == -1 || g_month == -1  || g_year == -1 )
                {
                    // simple use default function and TODO LOG IT
                    g_date_subtract_months(date,n_months);
                }
                else
                {
                    // set the value
                    g_date_set_dmy(date, (GDateDay) g_day, (GDateMonth) g_month, (GDateYear) g_year);
                }

            }
            break;// end of Jalalian Case
        default:
            // for unknown calendar use default function and TODO LOG IT
            g_date_subtract_months(date,n_months);
            break;

    }

}


void         gnc_date_add_months            (GDate       *date,
                                             guint        n_months)
{
    // used for convert to masked calendar
    guint masked_day;
    guint masked_month;
    guint masked_year;
    // used for output
    guint g_day;
    guint g_month;
    guint g_year;

    // if n_month is less then 0 simply return
    if ( n_months <0)
        return ;


    if(!g_date_valid(date))
    {
        // something is wrong should TODO log it
        return ;
    }

    switch (gnc_calendar_type_get ())
    {
        case GNC_CALENDAR_TYPE_GREGORIAN:
            // use default function for gregorian
            g_date_add_months(date,n_months);
            break;// end of gregorian case
        case GNC_CALENDAR_TYPE_JALALI:
            // convert to Jalalian date

            gnc_gregorian_to_jalali((int *) &masked_year, (int *) &masked_month, (int *) &masked_day
                , g_date_get_year(date), g_date_get_month(date), g_date_get_day(date));

            // validate conversation
            if( masked_day == -1 || masked_month == -1 || masked_year == -1 )
            {
                // use default function and TODO LOG IT
                g_date_add_months(date,n_months);
            }// end of if conversation validate
            else
            {

                // add month to current month
                masked_month+=n_months;

                // check month value is correct
                while (masked_month > 12  )
                {
                    masked_year++;
                    masked_month-=12;

                }

                if(masked_month==0)
                    masked_month=1;

                // check if the day is in month range
                if( masked_day >
                        (masked_month == 12 && gnc_jalali_is_leap_year(masked_year) ?
                         gnc_jalali_days_in_month(masked_month-1)+1 :
                        gnc_jalali_days_in_month(masked_month-1)))
                {

                    // if day is out of the months days simply back to the last day

                    masked_day= (guint) (masked_month == 12 && gnc_jalali_is_leap_year(masked_year) ?
                                                    gnc_jalali_days_in_month(masked_month-1)+1 :
                                         gnc_jalali_days_in_month(masked_month-1));
                }

                // return date to gregorian
                gnc_jalali_to_gregorian((int *) &g_year, (int *) &g_month, (int *) &g_day
                    , masked_year, masked_month, masked_day);

                // validate conversation
                if ( g_day ==-1 || g_month == -1 || g_year == -1 )
                {

                    // use default function and TODO LOG IT
                    g_date_add_months(date,n_months);
                }
                else
                {
                   g_date_set_dmy(date, (GDateDay) g_day, (GDateMonth) g_month, (GDateYear) g_year);
                }

            }// else of conversation validate end
            break;// end of Jalalian Case
        default:
            // use default function for unknown Calendar type and TODO LOG IT
            g_date_add_months(date,n_months);
            break;// end of default case

    }// end of switch

}


gboolean     gnc_date_is_last_of_month      (const GDate *date)
{
    // used for convection to masked date ;
    guint masked_day= (guint) -1;
    guint masked_month= (guint) -1;
    guint masked_year= (guint) -1;
    // used for send output of function
    gboolean  output=FALSE;

    switch (gnc_calendar_type_get ())
    {
        case GNC_CALENDAR_TYPE_GREGORIAN:
            // for gregorian calendar use default function
            output=g_date_is_last_of_month(date);
            break; // end of Gregorian case
        case GNC_CALENDAR_TYPE_JALALI:
            gnc_gregorian_to_jalali((int *) &masked_year, (int *) &masked_month, (int *) &masked_day
                    , g_date_get_year(date), g_date_get_month(date), g_date_get_day(date));

            // validate conversation
            if( masked_day == -1 || masked_month == -1  || masked_year == -1 )
            {
                // simply use default function and TODO LOG IT
                output=g_date_is_last_of_month(date);
            }
            else
            {
                output= masked_day ==(
                        masked_month == 12 && gnc_jalali_is_leap_year(masked_year) ?
                        gnc_jalali_days_in_month(masked_month-1) + 1 :
                        gnc_jalali_days_in_month(masked_month-1)
                ) ;

            }
            break;// end of Case Jalalian
        default:
            output=g_date_is_last_of_month(date);
            break;// end of default


    }


    return output;

}

