/*
 * jalali.h
 * Copyright (C) 2010 Christian Stimming
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

#ifndef GNC_JALALI_H
#define GNC_JALALI_H

/** Convert gregorian year/month/day (arguments 4-6) to jalali
 * year/month/day (arguments 1-3) */
void gnc_gregorian_to_jalali(/* output */ int *j_y, int *j_m, int *j_d,
        /*  input */ int  g_y, int  g_m, int  g_d);

/** Convert jalali year/month/day (arguments 4-6) to gregorian
 * year/month/day (arguments 1-3) */
void gnc_jalali_to_gregorian(/* output */ int *g_y, int *g_m, int *g_d,
        /*  input */ int  j_y, int  j_m, int  j_d);

/** Returns the number of days in month in the Jalali calendar, which
 * is different from the Gregorian one. Argument month_index is 0 for
 * the first month. */
int gnc_jalali_days_in_month(int month_index);

/** Returns the month name of the Jalali calendar. Argument
 * month_index is 0 for the first month. */
const char* gnc_jalali_month_name(int month_index);


//// General function - added for GncCalendar and used in Jalalian Calendar feature
/// \author Amin Aghabeiki

// rest function added for GncCalendar and used in Jalalian Calendar feature

/// gnc_jalali_is_leap_year - check the passed year is a leap year or not
///
/// \example                - gnc_jalali_is_leap_year(1395) return 1 &
///                             gnc_jalali_is_leap_year(1396) return 0
///
/// \param year     (int)   - An Integer that point to a Jalali Year for example 1395, year >= 1
/// \return is_leap (int)   - 0 if the passed year is not leap and 1 if it is.
int gnc_jalali_is_leap_year(int year);

/// gnc_next_jalalian_month - return the next Jalalian month  in Gregorian Date
///
/// \details                - this function used in Gnc-Gdate-utils when user try to go to next month
///                              based on calendar route here,
///
///
/// \example                - 15/9/1395 is a Jalalian Date that equal by 5/12/2016
///                             if user need next month in Jalalian , gnc-Gdate-Utils call this
///                             function, and after calculation the next month Jalalian Date will
///                             be 15/10/1395 equal to 4/01/2017 in Gregorian Calendar
///
///
/// \param g_y              - Target year part in Gregorian       , g_y  >=1900
/// \param g_m              - Target month part in Gregorian      , g_m  >=1 && g_m <=12
/// \param g_d              - Target Day part in Gregorian        , g_d  >=1 && g_d <31 ( or month lenght)
/// used for output  *** if something was wrong in function the output set top -1 for date ( -1,-1,-1 )
/// \param g_ny             - Next Month, year part in Gregorian  , g_ny >=1900
/// \param g_nm             - Next Month, month part in Gregorian , g_nm >=1 && g_nm <=12
/// \param g_nd             - Next Month, day part in Gregorian   , g_nd >=1 && g_nd <=31 (or month length)
void gnc_next_jalalian_month(const int g_y,const int g_m,const int g_d,
                             int * g_ny, int *g_nm, int *g_nd);

/// gnc_prev_jalalian_month - return the previus Jalalian month  in Gregorian Date
///
/// \details                - this function used in Gnc-Gdate-utils when user try to go to previus month
///                              based on calendar route here,
///
///
/// \example                - 15/9/1395 is a Jalalian Date  that equal by 5/12/2016
///                             if user need previus month in Jalalian , gnc-Gdate-Utils call this
///                             function, and after calculation the previus month Jalalian Date will
///                             be 15/8/1395 equal to 5/11/2016 in Gregorian Calendar
///
///
/// \param g_y              - Target year part in Gregorian          , g_y  >=1900
/// \param g_m              - Target month part in Gregorian         , g_m  >=1 && g_m <=12
/// \param g_d              - Target Day part in Gregorian           , g_d  >=1 && g_d <31 ( or month lenght)
/// used for output  *** if something was wrong in function the output set top -1 for date ( -1,-1,-1 )
/// \param g_py             - previus Month, year part in Gregorian  , g_py >=1900
/// \param g_pm             - previus Month, month part in Gregorian , g_pm >=1 && g_pm <=12
/// \param g_pd             - previus Month, day part in Gregorian   , g_pd >=1 && g_pd <=31 (or month length)
void gnc_prev_jalalian_month(const int g_y,const int g_m,const int g_d,
                             int * g_py, int *g_pm, int *g_pd);


/// gnc_beginning_jalalian_month  - return the begining of the corrent month of passed date ,
///                                      calculation based Jalalian Calendar but
///                                      the return value is in Gregorian Calendar
///
/// \example                      - if the passed date will be : 5/12/2016 the first of month
///                                  in Jalalian Calculation is : 5/12/2016 is 15/09/1395 in
///                                  Jalalian, and begining of this month is 01/09/1395 and
///                                  this date in Gregorian Date will be : 21/11/2016
///
/// \param g_y                    - Target year part in Gregorian           , g_y  >=1900
/// \param g_m                    - Target month part in Gregorian          , g_m  >=1 && g_m <=12
/// \param g_d                    - Target Day part in Gregorian            , g_d  >=1 && g_d <31 ( or month lenght)
/// used for output  *** if something was wrong in function the output set top -1 for date ( -1,-1,-1 )
/// \param g_fy                   - begining Month, year part in Gregorian  , g_fy >=1900
/// \param g_fm                   - begining Month, month part in Gregorian , g_fm >=1 && g_fm <=12
/// \param g_fd                   - begining Month, day part in Gregorian   , g_fd >=1 && g_fd <=31 (or month length)
void gnc_beginning_jalalian_month(const int g_y,const int g_m,const int g_d,
                                 int * g_fy, int *g_fm, int *g_fd);

/// gnc_end_of_jalalian_month  - return the end of the corrent month of passed date ,
///                                      calculation based Jalalian Calendar but
///                                      the return value is in Gregorian Calendar
///
/// \example                   - if the passed date will be : 5/12/2016 the end of month
///                                  in Jalalian Calculation is : 5/12/2016 is 15/09/1395 in
///                                  Jalalian, and begining of this month is 30/09/1395 and
///                                  this date in Gregorian Date will be : 20/12/2016
///
/// \param g_y                 - Target year part in Gregorian         , g_y  >=1900
/// \param g_m                 - Target month part in Gregorian        , g_m  >=1 && g_m <=12
/// \param g_d                 - Target Day part in Gregorian          , g_d  >=1 && g_d <31 ( or month lenght)
/// used for output  *** if something was wrong in function the output set top -1 for date ( -1,-1,-1 )
/// \param g_ey                - End of Month, year part in Gregorian  , g_ey >=1900
/// \param g_em                - End of Month, month part in Gregorian , g_em >=1 && g_em <=12
/// \param g_ed                - End of Month, day part in Gregorian   , g_ed >=1 && g_ed <=31 (or month length)
/// \param g_ey
void gnc_end_of_jalalian_month(const int g_y,const int g_m,const int g_d,
                                 int * g_ey, int *g_em, int *g_ed);

/// gnc_beginning_jalalian_year  - return the begining of the corrent year of passed date ,
///                                      calculation based Jalalian Calendar but
///                                      the return value is in Gregorian Calendar
///
/// \example                      - if the passed date will be : 5/12/2016 the the begening of this year
///                                  in Jalalian Calculation is : 5/12/2016 is 15/09/1395 in
///                                  Jalalian, and begining of this year is 01/01/1395 and
///                                  this date in Gregorian Date will be : 20/03/2016
///
/// \param g_y                    - Target year part in Gregorian             , g_y  >=1900
/// \param g_m                    - Target month part in Gregorian            , g_m  >=1 && g_m <=12
/// \param g_d                    - Target Day part in Gregorian              , g_d  >=1 && g_d <31 ( or month lenght)
/// used for output  *** if something was wrong in function the output set top -1 for date ( -1,-1,-1 )
/// \param g_fy                   - begining of year, year part in Gregorian  , g_fy >=1900
/// \param g_fm                   - begining of year, month part in Gregorian , g_fm >=1 && g_fm <=12
/// \param g_fd                   - begining of year, day part in Gregorian   , g_fd >=1 && g_fd <=31 (or month length)
void gnc_beginning_jalalian_year(const int g_y,const int g_m,const int g_d,
                                int * g_fy, int *g_fm, int *g_fd);

/// gnc_beginning_jalalian_year  - return the End of the corrent year of passed date ,
///                                      calculation based Jalalian Calendar but
///                                      the return value is in Gregorian Calendar
///
/// \example                      - if the passed date will be : 5/12/2016 the the End of this year
///                                  in Jalalian Calculation is : 5/12/2016 is 15/09/1395 in
///                                  Jalalian, and end of this year is 30/12/1395 and
///                                  this date in Gregorian Date will be : 20/03/2017
///
/// \param g_y                    - Target year part in Gregorian        , g_y  >=1900
/// \param g_m                    - Target month part in Gregorian       , g_m  >=1 && g_m <=12
/// \param g_d                    - Target Day part in Gregorian         , g_d  >=1 && g_d <31 ( or month lenght)
/// used for output  *** if something was wrong in function the output set top -1 for date ( -1,-1,-1 )
/// \param g_ey                   - End of year, year part in Gregorian  , g_ey >=1900
/// \param g_em                   - End of year, month part in Gregorian , g_em >=1 && g_em <=12
/// \param g_ed                   - End of year, day part in Gregorian   , g_ed >=1 && g_ed <=31 (or month length)
void gnc_end_of_jalalian_year(const int g_y,const int g_m,const int g_d,
                                int * g_ey, int *g_em, int *g_ed);

// end of General Function
#endif
