/*
 * FILE:
 * pricecell.c
 *
 * FUNCTION:
 * Implements the price cell
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

#include <ctype.h>
#include <string.h>

#include "util.h"

#include "basiccell.h"
#include "pricecell.h"

static void PriceSetValue (BasicCell *, const char *);

#define DECIMAL_PT  '.'

/* hack alert -- use color for cells as per old xacc */

#define SET(cell,str) { 			\
   if ((cell)->value) free ((cell)->value);	\
   (cell)->value = strdup (str);		\
}

/* ================================================ */
/* This callback only allows numbers with a single
 * decimal point in them */

static const char * 
PriceMV (BasicCell *_cell, 
         const char * oldval, 
         const char *change, 
         const char *newval)
{
   PriceCell *cell = (PriceCell *) _cell;

   /* accept the newval string if user action was delete, etc. */
   if (change) {
      /* if change is a decimal point, then count decimal points */
      if (DECIMAL_PT == change[0]) {
         int i, count=0;
         for (i=0; 0 != newval[i]; i++) {
            if (DECIMAL_PT == newval[i]) count ++;
         }
         if (1 < count) return NULL;
      } else {
         /* accept numeric, reject non-alpha edits */
         if (! (isdigit (change[0]))) return NULL;
      }
   }

   /* parse the float pt value  and store it */
   cell->amount = xaccParseUSAmount (newval);
   SET ((&(cell->cell)), newval);
   return newval; 
}

/* ================================================ */

PriceCell *
xaccMallocPriceCell (void)
{
   PriceCell *cell;
   cell = (PriceCell *) malloc (sizeof (PriceCell));
   xaccInitPriceCell (cell);
   return cell;
}

/* ================================================ */

void
xaccInitPriceCell (PriceCell *cell)
{
   xaccInitBasicCell( &(cell->cell));
   cell->amount = 0.0;

   SET ( &(cell->cell), "0.0");

   cell->cell.modify_verify = PriceMV;
   cell->cell.set_value = PriceSetValue;
}

/* ================================================ */

void
xaccDestroyPriceCell (PriceCell *cell)
{
   cell->amount = 0.0;
   xaccDestroyBasicCell ( &(cell->cell));
}

/* ================================================ */

void xaccSetPriceCellValue (PriceCell * cell, double amt)
{
   char buff[40];
   cell->amount = amt;
   sprintf (buff, "%.3f", amt);
   SET ( &(cell->cell), buff);
}

/* ================================================ */

void xaccSetAmountCellValue (PriceCell * cell, double amt)
{
   char buff[40];
   cell->amount = amt;
   sprintf (buff, "%.2f", amt);
   SET ( &(cell->cell), buff);
}

/* ================================================ */

void xaccSetDebCredCellValue (PriceCell * deb, 
                              PriceCell * cred, double amt)
{
   char buff[40];
   deb->amount = -amt;
   cred->amount = amt;

   if (0.0 <= amt) {
      sprintf (buff, "%.2f", amt);
      SET ( &(cred->cell), buff);
      SET ( &(deb->cell), "");
   } else {
      sprintf (buff, "%.2f", -amt);
      SET ( &(cred->cell), "");
      SET ( &(deb->cell), buff);
   }
}

/* ================================================ */

static void 
PriceSetValue (BasicCell *_cell, const char *str)
{
   char buff[40];
   PriceCell *cell = (PriceCell *) _cell;

   SET (_cell, str);

   cell->amount = xaccParseUSAmount (str);

   sprintf (buff, "%.2f", cell->amount);
   SET ( &(cell->cell), buff);
}

/* --------------- end of file ---------------------- */
