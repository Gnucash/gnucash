
#include <string.h>

#include "price.h"
#include "single.h"

static char *  PriceMV (char * input)
{
   return strdup (input);
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
