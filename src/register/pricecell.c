
#include <string.h>

#include "util.h"

#include "basiccell.h"
#include "pricecell.h"

static void PriceSetValue (struct _BasicCell *, const char *);
static void Set (BasicCell *, const char *);

#define DECIMAL_PT  '.'


#define SET(cell,str) { 			\
   if ((cell)->value) free ((cell)->value);	\
   (cell)->value = strdup (str);		\
}

/* ================================================ */
/* This callback only allows numbers with a single
 * decimal point in them */

static const char * 
PriceMV (struct _BasicCell *_cell, 
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
PriceSetValue (struct _BasicCell *_cell, const char *str)
{
   char buff[40];
   PriceCell *cell = (PriceCell *) _cell;

   SET (((BasicCell *)_cell), str);

   cell->amount = xaccParseUSAmount (str);

   sprintf (buff, "%.2f", cell->amount);
   SET ( &(cell->cell), buff);
}

/* --------------- end of file ---------------------- */
