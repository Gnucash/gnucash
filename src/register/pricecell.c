
#include <string.h>

#include "price.h"
#include "single.h"

static char *  PriceMV (char * input)
{
   return strdup (input);
}

void
xaccInitPriceCell (SingleCell *cell)
{
  cell ->modify_verify = PriceMV;
}

/* --------------- end of file ---------------------- */
