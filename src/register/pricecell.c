
#include <string.h>

#include "price.h"
#include "single.h"

static const char * 
PriceMV (const char * old, const char *change, const char *new)
{
   printf (" price mv called old:%s change:%s new:%s \n", old, change, new);
   return new; 
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
