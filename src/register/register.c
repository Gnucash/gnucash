
/*
 * register.c
 */

#include "messages.h"
#include "register.h"

#define DATE_CELL_C  0
#define DATE_CELL_R  0

#define NUM_CELL_C  1
#define NUM_CELL_R  0

#define ACTN_CELL_C  1
#define ACTN_CELL_R  1

#define XFRM_CELL_C  2
#define XFRM_CELL_R  0

#define XTO_CELL_C  -1
#define XTO_CELL_R  -1

#define XTO_CELL_C  2
#define XTO_CELL_R  1

#define DESC_CELL_C  3
#define DESC_CELL_R  0

#define MEMO_CELL_C  3
#define MEMO_CELL_R  1

#define RECN_CELL_C  4
#define RECN_CELL_R  0

#define CRED_CELL_C  5
#define CRED_CELL_R  0

#define DEBT_CELL_C  6
#define DEBT_CELL_R  0

#define BALN_CELL_C  7
#define BALN_CELL_R  0

#define MAX_COLS 8

/* ============================================== */

BasicRegister * xaccMallocBasicRegister (void)
{
   BasicRegister * reg;
   reg = (BasicRegister *) malloc (sizeof (BasicRegister));
   xaccInitBasicRegister (reg);
   return reg;
}

/* ============================================== */

void xaccInitBasicRegister (BasicRegister *reg)
{
   Table * table;
   CellBlock *curs, *header;
   BasicCell *cell;

   /* define the header */

   header = xaccMallocCellBlock (1, MAX_COLS);
   reg->header = header;

   cell = xaccMallocTextCell();
   cell->width = 11;
   xaccAddCell (header, cell, 0, DATE_CELL_C);
   xaccSetBasicCellValue (cell, DATE_STR);
   
   cell = xaccMallocTextCell();
   cell->width = 7;
   xaccAddCell (header, cell, 0, NUM_CELL_C);
   xaccSetBasicCellValue (cell, NUM_STR);

   cell = xaccMallocTextCell();
   cell->width = 11;
   xaccAddCell (header, cell, XFRM_CELL_R, XFRM_CELL_C);
   xaccSetBasicCellValue (cell, XFRM_STR);
   
   cell = xaccMallocTextCell();
   cell->width = 29;
   xaccAddCell (header, cell, 0, DESC_CELL_C);
   xaccSetBasicCellValue (cell, DESC_STR);

   cell = xaccMallocTextCell();
   cell->width = 1;
   xaccAddCell (header, cell, 0, RECN_CELL_C);
   xaccSetBasicCellValue (cell, "R");

   cell = xaccMallocTextCell();
   cell->width = 9;
   xaccAddCell (header, cell, 0, CRED_CELL_C);
   xaccSetBasicCellValue (cell, CREDIT_STR);
   
   cell = xaccMallocTextCell();
   cell->width = 9;
   xaccAddCell (header, cell, 0, DEBT_CELL_C);
   xaccSetBasicCellValue (cell, DEBIT_STR);

   cell = xaccMallocTextCell();
   cell->width = 9;
   xaccAddCell (header, cell, 0, BALN_CELL_C);
   xaccSetBasicCellValue (cell, BALN_STR);

   
   /* --------------------------- */
   curs = xaccMallocCellBlock (2, MAX_COLS);
   reg->cursor = curs;
   
   reg->dateCell = xaccMallocDateCell();
   reg->dateCell->cell.width = 9;
   xaccAddCell (curs, &(reg->dateCell->cell), DATE_CELL_R, DATE_CELL_C);
   
   cell = xaccMallocTextCell();
   cell->width = 7;
   xaccAddCell (curs, cell, NUM_CELL_R, NUM_CELL_C);
   reg->numCell = cell;
   
   reg->actionCell = xaccMallocComboCell();
   reg->actionCell->cell.width = 7;
   xaccAddCell (curs, &(reg->actionCell->cell), ACTN_CELL_R, ACTN_CELL_C);
   
   reg->xfrmCell = xaccMallocComboCell();
   reg->xfrmCell->cell.width = 11;
   xaccAddCell (curs, &(reg->xfrmCell->cell), XFRM_CELL_R, XFRM_CELL_C);
   
   reg->descCell = xaccMallocQuickFillCell();
   reg->descCell->cell.width = 9;
   xaccAddCell (curs, &(reg->descCell->cell), DESC_CELL_R, DESC_CELL_C);
   
   cell = xaccMallocTextCell();
   cell->width = 9;
   xaccAddCell (curs, cell, MEMO_CELL_R, MEMO_CELL_C);
   reg->memoCell = cell;

   cell = xaccMallocRecnCell();
   cell->width = 1;
   xaccAddCell (curs, cell, RECN_CELL_R, RECN_CELL_C);
   reg->recnCell = cell;

   reg->creditCell = xaccMallocPriceCell();
   reg->creditCell->cell.width = 9;
   xaccAddCell (curs, &(reg->creditCell->cell), CRED_CELL_R, CRED_CELL_C);
   
   reg->debitCell = xaccMallocPriceCell();
   reg->debitCell->cell.width = 9;
   xaccAddCell (curs, &(reg->debitCell->cell), DEBT_CELL_R, DEBT_CELL_C);
   
   reg->balanceCell = xaccMallocPriceCell();
   reg->balanceCell->cell.width = 9;
   reg->balanceCell->cell.input_output = 0;
   xaccAddCell (curs, &(reg->balanceCell->cell), BALN_CELL_R, BALN_CELL_C);

   /* -------------------------------- */   
   /* define the traversal order */
   /* negative cells mean "traverse out of table" */
   xaccNextRight (curs, DATE_CELL_R, DATE_CELL_C,  NUM_CELL_R,  NUM_CELL_C);
   xaccNextRight (curs,  NUM_CELL_R,  NUM_CELL_C, XFRM_CELL_R, XFRM_CELL_C);
   xaccNextRight (curs, XFRM_CELL_R, XFRM_CELL_C, DESC_CELL_R, DESC_CELL_C);
   xaccNextRight (curs, DESC_CELL_R, DESC_CELL_C, CRED_CELL_R, CRED_CELL_C);
   xaccNextRight (curs, CRED_CELL_R, CRED_CELL_C, DEBT_CELL_R, DEBT_CELL_C);
   xaccNextRight (curs, DEBT_CELL_R, DEBT_CELL_C, ACTN_CELL_R, ACTN_CELL_C);
   xaccNextRight (curs, ACTN_CELL_R, ACTN_CELL_C, MEMO_CELL_R, MEMO_CELL_C);
   xaccNextRight (curs, MEMO_CELL_R, MEMO_CELL_C, -1-DATE_CELL_R, -1-DATE_CELL_C);


   /* -------------------------------- */   
   /* add menu items for the action cell */

   xaccAddComboCellMenuItem ( reg->actionCell, BUY_STR);
   xaccAddComboCellMenuItem ( reg->actionCell, SELL_STR);
   xaccAddComboCellMenuItem ( reg->actionCell, PRICE_STR);
   xaccAddComboCellMenuItem ( reg->actionCell, INT_STR);
   xaccAddComboCellMenuItem ( reg->actionCell, DIV_STR);
   xaccAddComboCellMenuItem ( reg->actionCell, LTCG_STR);
   xaccAddComboCellMenuItem ( reg->actionCell, STCG_STR);
   xaccAddComboCellMenuItem ( reg->actionCell, DIST_STR);
   xaccAddComboCellMenuItem ( reg->actionCell, SPLIT_STR);
   xaccAddComboCellMenuItem ( reg->actionCell, DEPOSIT_STR);
   xaccAddComboCellMenuItem ( reg->actionCell, WITHDRAW_STR);

   /* -------------------------------- */   
   table = xaccMallocTable ();
   table -> header = header;
   xaccSetCursor (table, curs);
   reg->table = table;
}

/* ============================================== */

unsigned int
xaccGetChangeFlag (BasicRegister *reg)
{

   unsigned int changed = 0;

   changed |= MOD_DATE && reg->dateCell->cell.changed;
   changed |= MOD_NUM  && reg->numCell->changed;
   changed |= MOD_DESC && reg->descCell->cell.changed;
   changed |= MOD_RECN && reg->recnCell->changed;
   changed |= MOD_AMNT && reg->creditCell->cell.changed;
   changed |= MOD_AMNT && reg->debitCell->cell.changed;
   /* changed |= MOD_SHRS && reg->xxxxxxCell->cell.changed; */
   /* changed |= MOD_PRIC && reg->xxxxxxCell->cell.changed; */
   changed |= MOD_MEMO && reg->memoCell->changed;
   changed |= MOD_ACTN && reg->actionCell->cell.changed;
   changed |= MOD_XFRM && reg->xfrmCell->cell.changed;
   /* changed |= MOD_XTO && reg->xtoCell->cell.changed; */

   return changed;
}

/* ============ END OF FILE ===================== */
