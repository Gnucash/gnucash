/********************************************************************\
 * calendar-jalali-lib.c -- Jalalian Implimention of Calendar-lib   *
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

#include "calendar-lib.h"

#include "calendar-jalali-lib.h"

#include "gnc-jalali.h"

static glong calc_days (guint year, guint mm, guint dd)
{
  gboolean lp;


  if (year < 1) return (0L);
  if ((mm < 1) || (mm > 12)) return (0L);
  if ((dd < 1) || (dd > jalali_month_length[(lp = leap (year))][mm])) return (0L);

  return (year_to_days (--year) + jalali_days_in_months[lp][mm] + dd);
}

static guint day_of_week (guint year, guint mm, guint dd)
{
  glong days;

  days = calc_days (year, mm, dd) - 3; // this 3 days is different of start week in first of time
  if (days > 0L)
    {
      days--;
      days %= 7L;
      days++;
    }

  return ((guint) days);
}

static glong dates_difference (guint year1, guint mm1, guint dd1, guint year2, guint mm2, guint dd2)
{
  return (calc_days (year2, mm2, dd2) - calc_days (year1, mm1, dd1));
}

static guint weeks_in_year (guint year)
{
  return (guint) (52 + ((day_of_week (year, 1, 1) == 4) || (day_of_week (year, 12, 31) == 4)));
}

static gboolean week_of_year (guint *week, guint *year, guint mm, guint dd)
{
  if (check_date (*year, mm, dd))
    {
      *week = week_number (*year, mm, dd);
      if (*week == 0)
        *week = weeks_in_year (--(*year));
      else if (*week > weeks_in_year (*year))
        {
          *week = 1;
          (*year)++;
        }
      return TRUE;
    }
  return FALSE;
}

static glong year_to_days (guint year)
{
  gint index;
  glong sumDays;

  sumDays = 0;
  for (index = 1; index <= year; index++)
    {
      sumDays += jalali_days_in_months[leap ((guint) index)][13];
    }
  return sumDays;
}

static gboolean check_date (guint year, guint mm, guint dd)
{
  if (year < 1) return FALSE;
  if ((mm < 1) || (mm > 12)) return FALSE;
  if ((dd < 1) || (dd > jalali_month_length[leap (year)][mm])) return FALSE;
  return TRUE;
}

static gboolean leap (guint year)
{

  gint y;

  if (year > 0)
    y = year - 474;
  else
    y = 473;
  return (((((y % 2820) + 474) + 38) * 682) % 2816) < 682;

}

static guint week_number (guint year, guint mm, guint dd)
{

  guint first;

  first = day_of_week (year, 1, 1) - 1;
  return ((guint) ((dates_difference (year, 1, 1, year, mm, dd) + first) / 7L) +
          (first < 4));
}

static void compute_paint_days (const gint year, const gint month, const gint g_day, gint week_start,
                                gint day[6][7], gint day_month[6][7])
{

  gint clc_month;
  gint clc_year;
  gint clc_ndays_in_month;
  gint clc_ndays_in_prev_month;
  gint clc_first_day;
  gint clc_row;
  gint clc_col;
  gint clc_day;
  gint clc_dummy_day;

  gnc_gregorian_to_jalali (&clc_year, &clc_month, &clc_dummy_day, year, month + 1, g_day);

  clc_ndays_in_month = jalali_month_length[leap ((guint) clc_year)][clc_month];

  clc_first_day = day_of_week ((guint) clc_year, (guint) clc_month, 1);
  clc_first_day = (clc_first_day + 7 - week_start) % 7;

  /* Compute days of previous month */
  clc_month--;
  if (clc_month > 1)
    clc_ndays_in_prev_month = jalali_month_length[leap ((guint) clc_year)][clc_month];
  else
    clc_ndays_in_prev_month = jalali_month_length[leap ((guint) (clc_year - 1))][12];
  clc_day = clc_ndays_in_prev_month - clc_first_day + 1;
  clc_month++;

  clc_row = 0;
  if (clc_first_day > 0)
    {
      for (clc_col = 0; clc_col < clc_first_day; clc_col++)
        {
          day[clc_row][clc_col] = clc_day;
          day_month[clc_row][clc_col] = MONTH_PREV;
          clc_day++;
        }
    }

  /* Compute days of current month */
  clc_col = clc_first_day;
  for (clc_day = 1; clc_day <= clc_ndays_in_month; clc_day++)
    {
      day[clc_row][clc_col] = clc_day;
      day_month[clc_row][clc_col] = MONTH_CURRENT;

      clc_col++;
      if (clc_col == 7)
        {
          clc_row++;
          clc_col = 0;
        }
    }

  /* Compute days of next month */
  clc_day = 1;
  for (; clc_row <= 5; clc_row++)
    {
      for (; clc_col <= 6; clc_col++)
        {
          day[clc_row][clc_col] = clc_day;
          day_month[clc_row][clc_col] = MONTH_NEXT;
          clc_day++;
        }
      clc_col = 0;
    }
}

static void init_day_name (char *default_abbreviated_dayname[7])
{

  gint i;
  if (default_abbreviated_dayname[0])
    {
      for (i = 0; i < 7; i++)
        g_free (default_abbreviated_dayname[i]);
    }
  for (i = 0; i < 7; i++)
    {
      default_abbreviated_dayname[i] = g_strdup (jalali_day_name[i]);
    }
};
static void init_month_name (char *default_monthname[12])
{
  gint i;

  if (default_monthname[0])
    {
      for (i = 0; i < 12; i++)
        g_free (default_monthname[i]);
    }

  for (i = 0; i < 12; i++)
    {
      default_monthname[i] = g_strdup (jalali_month_name[i]);
    }
};

static gchar *paint_header_helper (const gint g_yy, const gint g_mm, const gint g_dd, gint *display_month)
{

  gint j_y;

  gint j_d;
  gnc_gregorian_to_jalali (&j_y, display_month, &j_d, g_yy, g_mm, g_dd);
  // for printing years in Jalalian date, I skip the formatting
  return g_strdup_printf ("%d", j_y);

}

static gint display_selected_day (const gint g_yy, const gint g_mm, const gint g_dd)
{

  gint j_y;
  gint j_m;
  gint j_d;

  gnc_gregorian_to_jalali (&j_y, &j_m, &j_d, g_yy, g_mm, g_dd);

  return j_d;
}

static void
invalidate_display_day_to_gregorian (const gint selected_year, const gint selected_month, const gint selected_day, gint *gregorian_year, gint *gregorian_month, gint *gregorian_day)
{

  gnc_jalali_to_gregorian (gregorian_year, gregorian_month, gregorian_day, selected_year, selected_month, selected_day);

}
CalendarLib *jalali_calendar_inti ()
{
  CalendarLib *calendar_obj = g_malloc (sizeof (CalendarLib));

  calendar_obj->month_length = (guint **) g_malloc (sizeof (guint *) * 2);
  calendar_obj->month_length[0] = (guint *) g_malloc (sizeof (guint *) * 13);
  calendar_obj->month_length[1] = (guint *) g_malloc (sizeof (guint *) * 13);
  calendar_obj->days_in_months = (guint **) g_malloc (sizeof (guint *) * 2);
  calendar_obj->days_in_months[0] = (guint *) g_malloc (sizeof (guint *) * 14);
  calendar_obj->days_in_months[1] = (guint *) g_malloc (sizeof (guint *) * 14);
  for (int i = 0; i < 2; i++)
    {
      for (int j = 0; j < 13; j++)
        {

          calendar_obj->month_length[i][j] = (guint) jalali_month_length[i][j];
          calendar_obj->days_in_months[i][j] = (guint) jalali_days_in_months[i][j];
        }

    }
  calendar_obj->days_in_months[0][13] = (guint) jalali_days_in_months[0][13];
  calendar_obj->days_in_months[1][13] = (guint) jalali_days_in_months[1][13];

  calendar_obj->calc_days = calc_days;

  calendar_obj->day_of_week = day_of_week;

  calendar_obj->dates_difference = dates_difference;

  calendar_obj->weeks_in_year = weeks_in_year;

  calendar_obj->week_of_year = week_of_year;

  calendar_obj->year_to_days = year_to_days;

  calendar_obj->check_date = check_date;

  calendar_obj->leap = leap;

  calendar_obj->week_number = week_number;

  calendar_obj->compute_paint_days = compute_paint_days;

  calendar_obj->init_day_name = init_day_name;

  calendar_obj->init_month_name = init_month_name;

  calendar_obj->paint_header_helper = paint_header_helper;

  calendar_obj->display_selected_day = display_selected_day;

  calendar_obj->invalidate_display_day_to_gregorian = invalidate_display_day_to_gregorian;

  return calendar_obj;
}

void jalali_calendar_destroy (CalendarLib *gregorian_obj)
{

  g_free (gregorian_obj->days_in_months[0]);
  g_free (gregorian_obj->days_in_months[1]);
  g_free (gregorian_obj->days_in_months);

  g_free (gregorian_obj->month_length[0]);
  g_free (gregorian_obj->month_length[1]);
  g_free (gregorian_obj->month_length);

  g_free (gregorian_obj);
}