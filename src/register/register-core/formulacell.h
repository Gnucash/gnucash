/********************************************************************\
 * formulacell.h -- Formula entry/handling/display cell             *
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
 * 59 Temple Place - Suite 330        Fax:    +1-617-542-2652       *
 * Boston, MA  02111-1307,  USA       gnu@gnu.org                   *
 *                                                                  *
\********************************************************************/

/**
 * FILE:
 * formulacell.h
 *
 * FUNCTION:
 *
 * The FormulaCell object implements a cell which can contain a
 * mathematical formula involving some number of variables, defined by
 * the user.  Any standard math functions [*, /, +, -, ()] are
 * supported, and any non-numeric, non-syntactic characters or
 * character strings are interpreted as variables; these character
 * strings may be enclosed by square brackets ([...]) if they contain
 * spaces.
 *
 * The FormulaCell will check for formula validity [paren-matching,
 * mainly].
 *
 * The FormulaCell...
 *
 * DOES...
 * 
 * SHOULD DO...
 * . numeric-value detection
 * . paren-matching
 * . quick-fill on variables
 * . functions [special character strings which reference an external
 *   function
 * . scheme
 *
 * HISTORY:
 * Copyright (c) 2001 Joshua Sled <jsled@asynchronous.org>
 **/
 
#ifndef FORMULA_CELL_C
#define FORMULA_CELL_C

#include <time.h>

#include "basiccell.h"
#include "date.h"


typedef struct _FormulaCell
{
  BasicCell cell;
} FormulaCell;

/* installs a callback to handle date recording */
FormulaCell * xaccMallocFormulaCell (void);
void       xaccDestroyFormulaCell (FormulaCell *cell);

/* days are 1-31, mon is 1-12, year 1900 == 1900 */
void       xaccSetFormulaCellValue (FormulaCell *cell, int day, int mon, int year);  
void       xaccSetFormulaCellValueSecs (FormulaCell *cell, time_t secs);
void       xaccSetFormulaCellValueSecsL (FormulaCell *cell, long long secs);

void       xaccCommitFormulaCell (FormulaCell *cell);

void       xaccFormulaCellGetDate (FormulaCell *cell, Timespec *ts);

#endif /* __FORMULA_CELL_C__ */

/* --------------- end of file ---------------------- */

