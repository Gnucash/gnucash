
#ifndef __XACC_PRICE_C__
#define __XACC_PRICE_C__

#include "single.h"

typedef struct _PriceCell {
   SingleCell cell;
   double amount;
} PriceCell;

/* installs a callback to handle price recording */
PriceCell *  xaccMallocPriceCell (void);
void         xaccInitPriceCell (PriceCell *);

#endif /* __XACC_PRICE_C__ */

/* --------------- end of file ---------------------- */
