
/*
 * register.h
 */

#ifndef __XACC_REGISTER_H__
#define __XACC_REGISTER_H__

#include "basiccell.h"
#include "combocell.h"
#include "datecell.h"
#include "quickfillcell.h"
#include "pricecell.h"
#include "table.h"
#include "recncell.h"
#include "textcell.h"

typedef struct _BasicRegister {
   Table         * table;
   CellBlock     * cursor;
   CellBlock     * header;
   DateCell      * dateCell;
   BasicCell     * numCell;
   ComboCell     * actionCell;
   ComboCell     * xfrmCell;
   QuickFillCell * descCell;
   BasicCell     * memoCell;
   BasicCell     * recnCell;
   PriceCell     * creditCell;
   PriceCell     * debitCell;
   PriceCell     * balanceCell;

} BasicRegister;

BasicRegister * xaccMallocBasicRegister (void);
void            xaccInitBasicRegister (BasicRegister *);

#endif __XACC_REGISTER_H__

/* ============ END OF FILE ===================== */
