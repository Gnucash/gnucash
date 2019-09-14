/********************************************************************\
 * datecell.h -- Date entry/handling/display cell                   *
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

/** @addtogroup Cell Cell
 * @{
 * @file datecell.h
 * @struct DateCell
 * @brief The DateCell object implements a date handling cell.  It
 * is able to display dates in several formats, it is also
 * able to accept date input, and also supports a number of accelerator
 * keys.
 *
 * On output, this cell will display a date as mm/dd/yy
 * The actual date format is compile-time configurable.
 *
 * hack alert -- make the display format run-time configurable, and
 * appropriately internationalized.
 *
 * One input, this cell is able to parse dates that are entered.
 * It rejects any input that is not in the form of a date.  Input
 * also includes a number of accelerators:
 *
 *     '+':
 *     '=':    increment day
 *
 *     '_':
 *     '-':    decrement day
 *
 *     '}':
 *     ']':    increment month
 *
 *     '{':
 *     '[':    decrment month
 *
 *     'M':
 *     'm':    beginning of month
 *
 *     'H':
 *     'h':    end of month
 *
 *     'Y':
 *     'y':    beginning of year
 *
 *     'R':
 *     'r':    end of year
 *
 *     'T':
 *     't':    today
 *
 *
 * METHODS:
 *
 * HISTORY:
 * Copyright (c) 1998, 1999, 2000 Linas Vepstas <linas@linas.org>
 * Copyright (c) 2000 Dave Peticolas
 * Copyright (c) 2017 Aaron Laws
 */

#ifndef DATE_CELL_H
#define DATE_CELL_H

#include <time.h>

#include "basiccell.h"
#include "qof.h"


typedef struct
{
    BasicCell cell;
} DateCell;

/** installs a callback to handle date recording */
BasicCell * gnc_date_cell_new (void);

/** Accepts a numeric date and sets the contents of the date cell to that value.
 * The value day, month, year should follow the regular written conventions.
 * @param cell The DateCell to set
 * @param day The day of the month, 1..31
 * @param month The month of the year, 1..12
 * @param year The year, 4 digits, common era
  */
void        gnc_date_cell_set_value (DateCell *cell,
                                     int day, int mon, int year);
/** Sets the contents of the cell with seconds before or since the Unix epoch.
 * @param cell The DateCell to set
 * @param secs Seconds before or since the Unix epoch, 1 January 1970 CE.
 */
void        gnc_date_cell_set_value_secs (DateCell *cell, time64 secs);

/** Commits any pending changes to the value of the cell.  That is, it will take
 * the cells current string value, and parse it into a month-day-year value.
 *
 * Why is this needed? Basically, while the user is typing into the cell, we do
 * not even try to parse the date, lest we confuse things royally.  Normally,
 * when the user leaves this cell, and moves to another, we parse the date and
 * print it nicely and cleanly into the cell.  But it can happen that we want to
 * get the accurate contents of the date cell before we've left it, e.g. if the
 * user has clicked on the "commit" button.  This is the routine to call for
 * that.
*/
void        gnc_date_cell_commit (DateCell *cell);

/** Set a time64 to the value in the DateCell.
 * @param cell The DateCell
 * @param time A time64* to which the function will write the time.
 * @param warn Whether to warn of parse errors or silently change to a valid one.
 */
void        gnc_date_cell_get_date (DateCell *cell, time64 *time, gboolean warn);

/** @} */
#endif
