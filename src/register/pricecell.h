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
 * 59 Temple Place - Suite 330        Fax:    +1-617-542-2652       *
 * Boston, MA  02111-1307,  USA       gnu@gnu.org                   *
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
 * Copyright (c) 2000 Dave Peticolas
 * Copyright (c) 2001 Free Software Foundation
 */

#ifndef __PRICE_CELL_C__
#define __PRICE_CELL_C__

#include "basiccell.h"
#include "gnc-common.h"
#include "gnc-numeric.h"
#include "gnc-ui-util.h"

typedef struct _PriceCell
{
  BasicCell cell;

  gnc_numeric amount;    /* the amount associated with this cell */

  int fraction;          /* fraction used for rounding, if 0 no rounding */

  gboolean blank_zero;   /* controls printing of zero values */

  GNCPrintAmountInfo print_info; /* amount printing context */

  gboolean need_to_parse; /* internal */
} PriceCell;

/* installs a callback to handle price recording */
BasicCell *  xaccMallocPriceCell (void);

/* return the value of a price cell */
gnc_numeric  xaccGetPriceCellValue (PriceCell *cell);

/* updates amount, returns TRUE if string representation
 * actually changed */
gboolean     xaccSetPriceCellValue (PriceCell *cell, gnc_numeric amount);

/* Sets the fraction used for rounding. If 0, no rounding is performed. */
void         xaccSetPriceCellFraction (PriceCell *cell, int fraction);

/* Sets the cell as blank, regardless of the blank_zero value */
void         xaccSetPriceCellBlank (PriceCell *cell);

/* determines whether 0 values are left blank or printed.
 * defaults to true. */
void         xaccSetPriceCellBlankZero (PriceCell *cell, gboolean blank_zero);

/* set the printing context of the price cell */
void         xaccSetPriceCellPrintInfo (PriceCell *cell,
                                        GNCPrintAmountInfo print_info);

/* updates two cells; the deb cell if amt is negative, the credit cell
 * if amount is positive, and makes the other cell blank. */
void         xaccSetDebCredCellValue (PriceCell *debit,
                                      PriceCell *credit,
                                      gnc_numeric amount);

#endif /* __PRICE_CELL_C__ */

/* --------------- end of file ---------------------- */
