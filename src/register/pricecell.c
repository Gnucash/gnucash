
#include <string.h>

#include "price.h"
#include "single.h"

static char * 
PriceMV (char * old, char *change, char *new)
{
   printf (" price mv called %s %s %s \n", old, change, new);
   return strdup (new);
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
