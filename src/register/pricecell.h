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
 * On input, this cell accepts only numeric characters
 * and numeric punctuation.  The punctuation accepted is *not*
 * currently internationalized.  Read the source for details.
 *
 * On output, it will display a numeric value using its current
 * format string.  The default format string prints two decimal 
 * places.  The format string can be set with the 
 * xaccSetPriceCellFormat() method.
 *
 * hack alert -- implement internationalization.
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
#include "gnc-common.h"

typedef struct _PriceCell
{
   BasicCell cell;

   double amount;          /* the amount associated with this cell */

   int precision;          /* precision of printed values */
   int min_trail_zeros;    /* minimum number of trailing zeros to print */

   gncBoolean blank_zero;  /* controls printing of zero values */
   gncBoolean monetary;    /* controls parsing of values */
} PriceCell;

/* installs a callback to handle price recording */
PriceCell *  xaccMallocPriceCell (void);
void         xaccInitPriceCell (PriceCell *);
void         xaccDestroyPriceCell (PriceCell *);

/* return the value of a price cell */
double       xaccGetPriceCellValue (PriceCell *cell);

/* updates amount, string format is three decimal places */
void         xaccSetPriceCellValue (PriceCell *cell, double amount);

/* sets the precision of the printed value. Defaults to 2 */
void         xaccSetPriceCellPrecision (PriceCell *cell, int precision);

/* Sets the mininum number of trailing decimal zeros that must
 * be printed. Defaults to 2. */
void         xaccSetPriceCellMinTrailZeros (PriceCell *cell, int);

/* determines whether 0 values are left blank or printed.
 * defaults to true. */
void         xaccSetPriceCellBlankZero (PriceCell *cell, gncBoolean);

/* The xaccSetPriceCellMonetary() sets a flag which determines
 *    how string amounts are parsed, either as monetary or
 *    non-monetary amounts. The default is monetary.
 */
void         xaccSetPriceCellMonetary (PriceCell *, gncBoolean);

/* updates two cells; the deb cell if amt is negative,
 * the credit cell if amount is positive, and makes the other cell
 * blank. */
void         xaccSetDebCredCellValue (PriceCell *deb,
                                      PriceCell *cred, double amount);

#endif /* __XACC_PRICE_CELL_C__ */

/* --------------- end of file ---------------------- */
