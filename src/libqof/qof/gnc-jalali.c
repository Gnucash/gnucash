/* This file is part of:
 *    Jalali, a Gregorian to Jalali and inverse date convertor
 * Copyright (C) 2001  Roozbeh Pournader <roozbeh@sharif.edu>
 * Copyright (C) 2001  Mohammad Toossi <mohammad@bamdad.org>
 * Copyright (C) 2016  Amin Aghabeiki <amin.aghabeiki@gmail.com>
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You can receive a copy of GNU Lesser General Public License at the
 * World Wide Web address <http://www.gnu.org/licenses/lgpl.html>.
 *
 * For licensing issues, contact The FarsiWeb Project Group,
 * Computing Center, Sharif University of Technology,
 * PO Box 11365-8515, Tehran, Iran, or contact us the
 * email address <FWPG@sharif.edu>.
 */

/* Changes:
 *
 * 2016-nov-29:
 *      Add some general function that needed by GncCalendar GTK Object
 *      -- Amin Aghabeiki
 * 2005-Sep-06:
 *      General cleanup  --Behdad Esfahbod
 *
 * 2001-Sep-21:
 *	Fixed a bug with "30 Esfand" dates, reported by Mahmoud Ghandi
 *
 * 2001-Sep-20:
 *	First LGPL release, with both sides of conversions
 */


#include "gnc-jalali.h"


/* implementation */

#include <glib.h>

int g_days_in_month[12] = {31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31};
int j_days_in_month[12] = {31, 31, 31, 31, 31, 31, 30, 30, 30, 30, 30, 29};
const char *j_month_name[12] =
{
    "Farvardin", "Ordibehesht", "Khordad",
    "Tir", "Mordad", "Shahrivar",
    "Mehr", "Aban", "Azar",
    "Dey", "Bahman", "Esfand"
};

static gboolean  valid_date(const int j_y, const int j_m, const int j_d)
{
    return  j_y >0 && j_m <= 12 && j_m > 0 && j_d>0 &&( j_d <= j_days_in_month[j_m-1]
                                              || ( j_m == 12 ?j_d <= (j_days_in_month[j_m-1] +1):FALSE));
}

int gnc_jalali_days_in_month(int month_index)
{
    g_assert(month_index < 12);
    return j_days_in_month[month_index];
}

const char* gnc_jalali_month_name(int month_index)
{
    g_assert(month_index < 12);
    return j_month_name[month_index];
}

 int gnc_jalali_is_leap_year(int year) {

     int y;

     // if user input an invalid year, I return 0
     if (year <1)
         return 0;

    if (year > 0)
        y = year - 474;
    else
        y = 473;
    return (((((y % 2820) + 474) + 38) * 682) % 2816) < 682;

}

void gnc_next_jalalian_month(const int g_y,const int g_m,const int g_d,
                             int * g_ny, int *g_nm, int *g_nd)
{
    int j_tmp_y;
    int j_tmp_m;
    int j_tmp_d;


    if(g_y <1900 || g_m > 12 || g_m <=0 || g_d > 31 || g_d<=0 )
    {
        // if user input an Invalidate Date, Simple set the output to
        // an invalidate date and finish the function
        *g_nd=-1;
        *g_nm=-1;
        *g_ny=-1;
        return;
    }

    gnc_gregorian_to_jalali(&j_tmp_y,&j_tmp_m,&j_tmp_d,g_y,g_m,g_d);

    if( j_tmp_y ==-1 || j_tmp_m == -1 || j_tmp_d == -1 )
    {
        // TODO LOG IT
        // some thing in conversation happen wrongly so
        // set the output to an invalidate date and return it
        *g_nm=-1;
        *g_nd=-1;
        *g_ny=-1;
        return ;
    }

    j_tmp_m++;
    if(j_tmp_m >12)
    {
        j_tmp_y++;
        j_tmp_m=1;
    }

    while (!valid_date(j_tmp_y,j_tmp_m-1,j_tmp_d) && j_tmp_d>0)j_tmp_d--;


    if( j_tmp_d<=0)
    {
        *g_nd=-1;
        *g_nm=-1;
        *g_ny=-1;
    } else{
        gnc_jalali_to_gregorian(g_ny,g_nm,g_nd,j_tmp_y,j_tmp_m,j_tmp_d);

    }
}

void gnc_prev_jalalian_month(const int g_y,const int g_m,const int g_d,
                             int * g_py, int *g_pm, int *g_pd)
{
    int j_tmp_y;
    int j_tmp_m;
    int j_tmp_d;
    if(g_y <1900 || g_m > 12 || g_m <=0 || g_d > 31 || g_d<=0 )
    {
        // if user input an Invalidate Date, Simple set the output to
        // an invalidate date and finish the function
        *g_pd=-1;
        *g_pm=-1;
        *g_py=-1;
        return;
    }
    gnc_gregorian_to_jalali(&j_tmp_y,&j_tmp_m,&j_tmp_d,g_y,g_m,g_d);
    if( j_tmp_y ==-1 || j_tmp_m == -1 || j_tmp_d == -1 )
    {
        // TODO LOG IT
        // some thing in conversation happen wrongly so
        // set the output to an invalidate date and return it
        *g_pd=-1;
        *g_pm=-1;
        *g_py=-1;
        return ;
    }
    j_tmp_m--;
    if(j_tmp_m <=0)
    {
        j_tmp_y--;
        j_tmp_m=11;
    }

    while (!valid_date(j_tmp_y,j_tmp_m-1,j_tmp_d) && j_tmp_d>0)j_tmp_d--;


    if( j_tmp_d<=0)
    {
        *g_pd=-1;
        *g_pm=-1;
        *g_py=-1;
    } else{
        gnc_jalali_to_gregorian(g_py,g_pm,g_pd,j_tmp_y,j_tmp_m,j_tmp_d);
    }
}

void gnc_beginning_jalalian_month(const int g_y,const int g_m,const int g_d,
                                 int * g_fy, int *g_fm, int *g_fd)
{
    int j_tmp_y;
    int j_tmp_m;
    int j_tmp_d;

    if(g_y <1900 || g_m > 12 || g_m <=0 || g_d > 31 || g_d<=0 )
    {
        // if user input an Invalidate Date, Simple set the output to
        // an invalidate date and finish the function
        *g_fy=-1;
        *g_fm=-1;
        *g_fd=-1;
        return;
    }
    gnc_gregorian_to_jalali(&j_tmp_y,&j_tmp_m,&j_tmp_d,g_y,g_m,g_d);
    if( j_tmp_y ==-1 || j_tmp_m == -1 || j_tmp_d == -1 )
    {
        // TODO LOG IT
        // some thing in conversation happen wrongly so
        // set the output to an invalidate date and return it
        *g_fy=-1;
        *g_fm=-1;
        *g_fd=-1;
        return ;
    }
    j_tmp_d=1; // first day of month

    gnc_jalali_to_gregorian(g_fy,g_fm,g_fd,j_tmp_y,j_tmp_m,j_tmp_d);


}
void gnc_end_of_jalalian_month(const int g_y,const int g_m,const int g_d,
                               int * g_ey, int *g_em, int *g_ed)
{

    int j_tmp_y;
    int j_tmp_m;
    int j_tmp_d;
    if(g_y <1900 || g_m > 12 || g_m <=0 || g_d > 31 || g_d<=0 )
    {
        // if user input an Invalidate Date, Simple set the output to
        // an invalidate date and finish the function
        *g_ey=-1;
        *g_em=-1;
        *g_ed=-1;
        return;
    }
    gnc_gregorian_to_jalali(&j_tmp_y,&j_tmp_m,&j_tmp_d,g_y,g_m,g_d);
    if( j_tmp_y ==-1 || j_tmp_m == -1 || j_tmp_d == -1 )
    {
        // TODO LOG IT
        // some thing in conversation happen wrongly so
        // set the output to an invalidate date and return it
        *g_ey=-1;
        *g_em=-1;
        *g_ed=-1;
        return ;
    }

    j_tmp_d=gnc_jalali_days_in_month(j_tmp_m-1); // first day of month

    if(!valid_date(j_tmp_y,j_tmp_m,j_tmp_d))
    {

        *g_ed=-1;
        *g_em=-1;
        *g_ey=-1;
    } else
    {
        gnc_jalali_to_gregorian(g_ey,g_em,g_ed,j_tmp_y,j_tmp_m,j_tmp_d);
    }




}

void gnc_beginning_jalalian_year(const int g_y,const int g_m,const int g_d,
                                 int * g_fy, int *g_fm, int *g_fd)
{
    int j_tmp_y;
    int j_tmp_m;
    int j_tmp_d;
    if(g_y <1900 || g_m > 12 || g_m <=0 || g_d > 31 || g_d<=0 )
    {
        // if user input an Invalidate Date, Simple set the output to
        // an invalidate date and finish the function
        *g_fy=-1;
        *g_fm=-1;
        *g_fd=-1;
        return;
    }
    gnc_gregorian_to_jalali(&j_tmp_y,&j_tmp_m,&j_tmp_d,g_y,g_m,g_d);
    if( j_tmp_y ==-1 || j_tmp_m == -1 || j_tmp_d == -1 )
    {
        // TODO LOG IT
        // some thing in conversation happen wrongly so
        // set the output to an invalidate date and return it
        *g_fy=-1;
        *g_fm=-1;
        *g_fd=-1;
        return ;
    }
    j_tmp_m=1; // first month
    j_tmp_d=1; // first day of month

    gnc_jalali_to_gregorian(g_fy,g_fm,g_fd,j_tmp_y,j_tmp_m,j_tmp_d);
}
void gnc_end_of_jalalian_year(const int g_y,const int g_m,const int g_d,
                              int * g_ey, int *g_em, int *g_ed)
{
    int j_tmp_y;
    int j_tmp_m;
    int j_tmp_d;

    int y;

    gnc_gregorian_to_jalali(&j_tmp_y,&j_tmp_m,&j_tmp_d,g_y,g_m,g_d);

    j_tmp_m=12; // end of year
    j_tmp_d=gnc_jalali_days_in_month(j_tmp_m-1); // last day of month



    if (j_tmp_y > 0)
        y = j_tmp_y - 474;
    else
        y = 473;
     if((((((y % 2820) + 474) + 38) * 682) % 2816) < 682)
         j_tmp_d++;

    if(!valid_date(j_tmp_y,j_tmp_m,j_tmp_d))
    {

        *g_ed=-1;
        *g_em=-1;
        *g_ey=-1;
    } else
    {
        gnc_jalali_to_gregorian(g_ey,g_em,g_ed,j_tmp_y,j_tmp_m,j_tmp_d);
    }

}
void gnc_gregorian_to_jalali(int *j_y, int *j_m, int *j_d,
                             int  g_y, int  g_m, int  g_d)
{
    int gy, gm, gd;
    int jy, jm, jd;
    long g_day_no, j_day_no;
    int j_np;

    int i;

    gy = g_y - 1600;
    gm = g_m - 1;
    gd = g_d - 1;

    g_day_no = 365 * gy + (gy + 3) / 4 - (gy + 99) / 100 + (gy + 399) / 400;
    for (i = 0; i < gm; ++i)
        g_day_no += g_days_in_month[i];
    if (gm > 1 && ((gy % 4 == 0 && gy % 100 != 0) || (gy % 400 == 0)))
        /* leap and after Feb */
        ++g_day_no;
    g_day_no += gd;

    j_day_no = g_day_no - 79;

    j_np = j_day_no / 12053;
    j_day_no %= 12053;

    jy = 979 + 33 * j_np + 4 * (j_day_no / 1461);
    j_day_no %= 1461;

    if (j_day_no >= 366)
    {
        jy += (j_day_no - 1) / 365;
        j_day_no = (j_day_no - 1) % 365;
    }

    for (i = 0; i < 11 && j_day_no >= j_days_in_month[i]; ++i)
    {
        j_day_no -= j_days_in_month[i];
    }
    jm = i + 1;
    jd = j_day_no + 1;
    *j_y = jy;
    *j_m = jm;
    *j_d = jd;
}

void gnc_jalali_to_gregorian(int *g_y, int *g_m, int *g_d,
                             int  j_y, int  j_m, int  j_d)
{
    int gy, gm, gd;
    int jy, jm, jd;
    long g_day_no, j_day_no;
    int leap;

    int i;

    jy = j_y - 979;
    jm = j_m - 1;
    jd = j_d - 1;

    j_day_no = 365 * jy + (jy / 33) * 8 + (jy % 33 + 3) / 4;
    for (i = 0; i < jm; ++i)
        j_day_no += j_days_in_month[i];

    j_day_no += jd;

    g_day_no = j_day_no + 79;

    gy = 1600 + 400 * (g_day_no / 146097); /* 146097 = 365*400 + 400/4 - 400/100 + 400/400 */
    g_day_no = g_day_no % 146097;

    leap = 1;
    if (g_day_no >= 36525) /* 36525 = 365*100 + 100/4 */
    {
        g_day_no--;
        gy += 100 * (g_day_no / 36524); /* 36524 = 365*100 + 100/4 - 100/100 */
        g_day_no = g_day_no % 36524;

        if (g_day_no >= 365)
            g_day_no++;
        else
            leap = 0;
    }

    gy += 4 * (g_day_no / 1461); /* 1461 = 365*4 + 4/4 */
    g_day_no %= 1461;

    if (g_day_no >= 366)
    {
        leap = 0;

        g_day_no--;
        gy += g_day_no / 365;
        g_day_no = g_day_no % 365;
    }

    for (i = 0; g_day_no >= g_days_in_month[i] + (i == 1 && leap); i++)
        g_day_no -= g_days_in_month[i] + (i == 1 && leap);
    gm = i + 1;
    gd = g_day_no + 1;

    *g_y = gy;
    *g_m = gm;
    *g_d = gd;
}

#if 0
int
main(void)
{
    int y, m, d;
    time64 bin_time;
    struct tm br_time;

    gnc_time (&bin_time);
    gnc_localtime_r (&bin_time, &br_time);

    gregorian_to_jalali(&y, &m, &d,
                        1900 + br_time.tm_year,
                        1 + br_time.tm_mon,
                        br_time.tm_mday);

    printf("Current Jalali date: %d %s %d\n", d, j_month_name[m-1], y);

    return 0;
}
#endif
