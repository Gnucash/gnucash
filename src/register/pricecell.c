
#include <string.h>

#include "price.h"
#include "single.h"

#define DECIMAL_PT  '.'

/* ================================================ */
/* This callback only allows numbers with a single
 * decimal point in them */

static const char * 
PriceMV (const char * old, const char *change, const char *new)
{
   if (change) {
      /* if change is a decimal point, then count decimal points */
      if (DECIMAL_PT == change[0]) {
         int i, count=0;
         for (i=0; 0 != new[i]; i++) {
            if (DECIMAL_PT == new[i]) count ++;
         }
         if (1 >= count) return new;
         return NULL;

      } else {
         /* accept numeric, reject non-alpha edits */
         if (isdigit (change[0])) return new;
         return NULL;
      }
   } else {
      /* accept the new string if user action was delete, etc. */
      return new; 
   }
}

/* ================================================ */

SingleCell *
xaccMallocPriceCell (void)
{
   SingleCell *cell;
   cell = xaccMallocSingleCell();
   xaccInitPriceCell (cell);
   return cell;
}

/* ================================================ */

void
xaccInitPriceCell (SingleCell *cell)
{
  if (cell->value) free (cell->value);
  cell ->value = strdup ("0.0");

  cell ->modify_verify = PriceMV;
}

/* --------------- end of file ---------------------- */
