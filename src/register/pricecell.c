
#include <string.h>

#include "basiccell.h"
#include "pricecell.h"

#define DECIMAL_PT  '.'

/* ================================================ */
/* This callback only allows numbers with a single
 * decimal point in them */

static const char * 
PriceMV (struct _BasicCell *_cell, 
         const char * oldval, 
         const char *change, 
         const char *newval)
{
   if (change) {
      /* if change is a decimal point, then count decimal points */
      if (DECIMAL_PT == change[0]) {
         int i, count=0;
         for (i=0; 0 != newval[i]; i++) {
            if (DECIMAL_PT == newval[i]) count ++;
         }
         if (1 >= count) return newval;
         return NULL;

      } else {
         /* accept numeric, reject non-alpha edits */
         if (isdigit (change[0])) return newval;
         return NULL;
      }
   } else {
      /* accept the newval string if user action was delete, etc. */
      return newval; 
   }
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

   xaccSetBasicCellValue ( &(cell->cell), "0.0");

   cell->cell.modify_verify = PriceMV;
}

/* ================================================ */

void xaccSetPriceCellValue (PriceCell * cell, double amt)
{
   char buff[40];
   cell->amount = amt;
   sprintf (buff, "%.3f", amt);
   xaccSetBasicCellValue ( &(cell->cell), buff);
}

/* ================================================ */

void xaccSetAmountCellValue (PriceCell * cell, double amt)
{
   char buff[40];
   cell->amount = amt;
   sprintf (buff, "%.2f", amt);
   xaccSetBasicCellValue ( &(cell->cell), buff);
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
      xaccSetBasicCellValue ( &(cred->cell), buff);
      xaccSetBasicCellValue ( &(deb->cell), "");
   } else {
      sprintf (buff, "%.2f", -amt);
      xaccSetBasicCellValue ( &(cred->cell), "");
      xaccSetBasicCellValue ( &(deb->cell), buff);
   }
}

/* --------------- end of file ---------------------- */
