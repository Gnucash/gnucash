
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

/* updates amount, string format is three decimal places */
void         xaccSetPriceCellValue (PriceCell *, double amount);

/* updates amount, string format is two decimal places */
void         xaccSetAmountCellValue (PriceCell *, double amount);

/* updates two cells; the deb cell if amt is negative,
 * the credit cell if amount is positive, and makes the other cell
 * blank. */
void         xaccSetDebCredCellValue (PriceCell *deb,
                                      PriceCell *cred,  double amount);

#endif /* __XACC_PRICE_CELL_C__ */

/* --------------- end of file ---------------------- */
