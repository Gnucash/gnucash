/*
 * FILE:
 * pricecell.h
 *
 * FUNCTION:
 * The PriceCell object implements a cell handler that
 * knows about storing and displaying a price or amount.
 *
 * By default, the PriceCell is an input/output cell.
 *
 * On input, this cell accepts only numeric characters
 * and numeric punctuation.  The punctuation accepted is *not*
 * currently internationalized.  Read the source for details.
 *
 * On output, it can display numeric values with two or three 
 * decimal places.  A planned enhancement would be to store 
 * formating data with an instance of this cell. This is *not*
 * currently done.
 *
 * hack alert -- implement the above formating & internationalization.
 *
 * On output, it will display negative values in red text.
 * hack alert -- the actual color (red) should be user configurable.
 *
 * The stored amount is stored as a double-precision floating point
 * variable. This should be sufficient precision to store trillions of
 * dollars with penny accuracy.
 *
 * HISTORY:
 * Copyright (c) 1998 Linas Vepstas
 */
/********************************************************************\
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
 * along with this program; if not, write to the Free Software      *
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.        *
\********************************************************************/

#ifndef __XACC_PRICE_CELL_C__
#define __XACC_PRICE_CELL_C__

#include "basiccell.h"

typedef struct _PriceCell {
   BasicCell cell;
   double amount;   /* the amount associated with this cell */
   short blank_zero;   /* controls printing of zero values */
   
} PriceCell;

/* installs a callback to handle price recording */
PriceCell *  xaccMallocPriceCell (void);
void         xaccInitPriceCell (PriceCell *);
void         xaccDestroyPriceCell (PriceCell *);

/* updates amount, string format is three decimal places */
void         xaccSetPriceCellValue (PriceCell *, double amount);

/* updates amount, string format is two decimal places */
void         xaccSetAmountCellValue (PriceCell *, double amount);

/* updates two cells; the deb cell if amt is negative,
 * the credit cell if amount is positive, and makes the other cell
 * blank. */
void         xaccSetDebCredCellValue (PriceCell *deb,
                                      PriceCell *cred,  double amount);

#endif /* __XACC_PRICE_CELL_C__ */

/* --------------- end of file ---------------------- */
