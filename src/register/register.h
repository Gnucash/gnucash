
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

/* modified flags -- indicate how values have been modified */
#define MOD_NONE  0x000
#define MOD_DATE  0x001
#define MOD_NUM   0x002
#define MOD_DESC  0x004
#define MOD_RECN  0x008
#define MOD_AMNT  0x010
#define MOD_SHRS  0x020
#define MOD_PRIC  0x040
#define MOD_MEMO  0x080
#define MOD_ACTN  0x100
#define MOD_XFRM  0x200
#define MOD_XTO   0x400
#define MOD_NEW   0x800
#define MOD_ALL   0xfff

typedef struct _BasicRegister {
   Table         * table;
   CellBlock     * cursor;
   CellBlock     * header;
   DateCell      * dateCell;
   BasicCell     * numCell;
   QuickFillCell * descCell;
   BasicCell     * recnCell;
   PriceCell     * creditCell;
   PriceCell     * debitCell;
   PriceCell     * shrsCell;
   PriceCell     * priceCell;
   BasicCell     * memoCell;
   ComboCell     * actionCell;
   ComboCell     * xfrmCell;
   ComboCell     * xtoCell;

   PriceCell     * balanceCell;

} BasicRegister;

BasicRegister * xaccMallocBasicRegister (void);
void            xaccInitBasicRegister (BasicRegister *);

unsigned int    xaccGetChangeFlag (BasicRegister *);

#endif __XACC_REGISTER_H__

/* ============ END OF FILE ===================== */
