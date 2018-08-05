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


#endif
