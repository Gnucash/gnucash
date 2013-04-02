/********************************************************************\
 * pricecell.h -- price input/display cell                          *
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

/*
 * FILE:
 * pricecell.h
 *
 * FUNCTION:
 * The PriceCell object implements a cell handler that stores
 * a single double-precision value, and has the smarts to
 * display it as a price/amount as well as accepting monetary
 * or general numeric input.
 *
 * By default, the PriceCell is an input/output cell.
 *
 * On output, it will display negative values in red text.
 * hack alert -- the actual color (red) should be user configurable.
 *
 * HISTORY:
 * Copyright (c) 1998, 1999, 2000 Linas Vepstas
 * Copyright (c) 2000 Dave Peticolas <dave@krondo.com>
 * Copyright (c) 2001 Free Software Foundation
 */

#ifndef PRICE_CELL_H
#define PRICE_CELL_H

#include "basiccell.h"
#include "qof.h"
#include "gnc-ui-util.h"

typedef struct
{
    BasicCell cell;

    gnc_numeric amount;    /* the amount associated with this cell */

    int fraction;          /* fraction used for rounding, if 0 no rounding */

    gboolean blank_zero;   /* controls printing of zero values */

    GNCPrintAmountInfo print_info; /* amount printing context */

    gboolean need_to_parse; /* internal */
} PriceCell;

/* installs a callback to handle price recording */
BasicCell *  gnc_price_cell_new (void);

/* return the value of a price cell */
gnc_numeric  gnc_price_cell_get_value (PriceCell *cell);

/* updates amount, returns TRUE if string representation
 * actually changed */
gboolean     gnc_price_cell_set_value (PriceCell *cell, gnc_numeric amount);

/* Sets the fraction used for rounding. If 0, no rounding is performed. */
void         gnc_price_cell_set_fraction (PriceCell *cell, int fraction);

/* Sets the cell as blank, regardless of the blank_zero value */
void         gnc_price_cell_blank (PriceCell *cell);

/* determines whether 0 values are left blank or printed.
 * defaults to true. */
void         gnc_price_cell_set_blank_zero (PriceCell *cell,
        gboolean blank_zero);

/* set the printing context of the price cell */
void         gnc_price_cell_set_print_info (PriceCell *cell,
        GNCPrintAmountInfo print_info);

/* updates two cells; the deb cell if amt is negative, the credit cell
 * if amount is positive, and makes the other cell blank. */
void         gnc_price_cell_set_debt_credit_value (PriceCell *debit,
        PriceCell *credit,
        gnc_numeric amount);

#endif
