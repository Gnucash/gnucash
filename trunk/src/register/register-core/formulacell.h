/********************************************************************\
 * formulacell.h -- Formula entry/display cell                      *
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

/**
 * FILE:
 * formulacell.h
 *
 * FUNCTION:
 *
 * The FormulaCell is a register-table cell which can contain a formula
 * involving numbers, formula markup and strings denoting either functions or
 * variables.
 *
 * Copyright (c) 2002 Joshua Sled <jsled@asynchronous.org>
 **/

#ifndef FORMULA_CELL_H
#define FORMULA_CELL_H

#include <time.h>

#include "gnc-ui-util.h"

#include "basiccell.h"
#include "qof.h"

typedef struct _FormulaCell
{
    BasicCell cell;

    /** The print-info for numeric values. **/
    GNCPrintAmountInfo print_info;

    /** The user-entered formula. **/
    char *formula;
} FormulaCell;

/* installs a callback to handle date recording */
BasicCell* gnc_formula_cell_new (void);

void gnc_formula_cell_set_value( FormulaCell *fc, const char *newVal );

#endif /* FORMULA_CELL_H */

/* --------------- end of file ---------------------- */

