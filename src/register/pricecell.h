
#ifndef __XACC_PRICE_CELL_C__
#define __XACC_PRICE_CELL_C__

#include "basiccell.h"

typedef struct _PriceCell {
   BasicCell cell;
   double amount;
} PriceCell;

/* installs a callback to handle price recording */
PriceCell *  xaccMallocPriceCell (void);
void         xaccInitPriceCell (PriceCell *);

#endif /* __XACC_PRICE_CELL_C__ */

/* --------------- end of file ---------------------- */
