/*
 * FILE:
 * register.c
 *
 * FUNCTION:
 * Implements the register object.
 * Specifies the physical layout of the register cells.
 * See the header file for additional documentation.
 *
 * hack alert -- most of the code in this file should be 
 * replaced by a guile/scheme based config file.
 *
 * HISTORY:
 * Copyright (c) 1998 Linas Vepstas
 */

/********************************************************************\
 * This program is free software; you can redistribute it and/or    *
 * modify it under the terms of the GNU General Public License as   *
 * published by the Free Software Foundation; either version 2 of   *
 * the License, or (at your option) any later version.              *
 *                                                                  *
 * This program is distributed in the hope that it will be useful,  *
 * but WITHOUT ANY WARRANTY; without even the implied warranty of   *
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the    *
 * GNU General Public License for more details.                     *
 *                                                                  *
 * You should have received a copy of the GNU General Public License*
 * along with this program; if not, write to the Free Software      *
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.        *
\********************************************************************/

#include <stdlib.h>

#include "messages.h"
#include "register.h"
#include "table-allgui.h"

/* utility defines for cell configuration data */
#define DATE_CELL      0
#define NUM_CELL       1
#define ACTN_CELL      2
#define XFRM_CELL      3
#define XTO_CELL       4
#define DESC_CELL      5
#define MEMO_CELL      6
#define RECN_CELL      7
#define CRED_CELL      8
#define DEBT_CELL      9
#define BALN_CELL     10
#define SHRS_CELL     11
#define PRIC_CELL     12
#define VALU_CELL     13


/* utility defines for setting of cell values */
#define DATE_CELL_C   (reg->cols[DATE_CELL])
#define DATE_CELL_R   (reg->rows[DATE_CELL])
#define DATE_CELL_W   (reg->wids[DATE_CELL])

#define NUM_CELL_C   (reg->cols[NUM_CELL])
#define NUM_CELL_R   (reg->rows[NUM_CELL])
#define NUM_CELL_W   (reg->wids[NUM_CELL])

#define ACTN_CELL_C   (reg->cols[ACTN_CELL])
#define ACTN_CELL_R   (reg->rows[ACTN_CELL])
#define ACTN_CELL_W   (reg->wids[ACTN_CELL])

#define XFRM_CELL_C   (reg->cols[XFRM_CELL])
#define XFRM_CELL_R   (reg->rows[XFRM_CELL])
#define XFRM_CELL_W   (reg->wids[XFRM_CELL])

#define XTO_CELL_C   (reg->cols[XTO_CELL])
#define XTO_CELL_R   (reg->rows[XTO_CELL])
#define XTO_CELL_W   (reg->wids[XTO_CELL])

#define DESC_CELL_C   (reg->cols[DESC_CELL])
#define DESC_CELL_R   (reg->rows[DESC_CELL])
#define DESC_CELL_W   (reg->wids[DESC_CELL])

#define MEMO_CELL_C   (reg->cols[MEMO_CELL])
#define MEMO_CELL_R   (reg->rows[MEMO_CELL])
#define MEMO_CELL_W   (reg->wids[MEMO_CELL])

#define RECN_CELL_C   (reg->cols[RECN_CELL])
#define RECN_CELL_R   (reg->rows[RECN_CELL])
#define RECN_CELL_W   (reg->wids[RECN_CELL])

#define CRED_CELL_C   (reg->cols[CRED_CELL])
#define CRED_CELL_R   (reg->rows[CRED_CELL])
#define CRED_CELL_W   (reg->wids[CRED_CELL])

#define DEBT_CELL_C   (reg->cols[DEBT_CELL])
#define DEBT_CELL_R   (reg->rows[DEBT_CELL])
#define DEBT_CELL_W   (reg->wids[DEBT_CELL])

#define BALN_CELL_C   (reg->cols[BALN_CELL])
#define BALN_CELL_R   (reg->rows[BALN_CELL])
#define BALN_CELL_W   (reg->wids[BALN_CELL])

#define SHRS_CELL_C   (reg->cols[SHRS_CELL])
#define SHRS_CELL_R   (reg->rows[SHRS_CELL])
#define SHRS_CELL_W   (reg->wids[SHRS_CELL])

#define PRIC_CELL_C   (reg->cols[PRIC_CELL])
#define PRIC_CELL_R   (reg->rows[PRIC_CELL])
#define PRIC_CELL_W   (reg->wids[PRIC_CELL])

#define VALU_CELL_C   (reg->cols[VALU_CELL])
#define VALU_CELL_R   (reg->rows[VALU_CELL])
#define VALU_CELL_W   (reg->wids[VALU_CELL])


/* ============================================== */

#define SET(cell,col,row,width,label) {		\
   reg->cols[cell] = col;			\
   reg->rows[cell] = row;			\
   reg->wids[cell] = width;			\
   reg->labels[cell] = label;			\
}

/* ============================================== */

static void
configLayout (BasicRegister *reg)
{
   int type = reg->type;

   /* perform a bsic layout that's valid for most
    * of the ledgers; then customize with case 
    * statements. */
   reg->num_cols = 8;
   reg->num_header_rows = 1;
   SET (DATE_CELL,  0,  0, 11,  DATE_STR);
   SET (NUM_CELL,   1,  0,  7,  NUM_STR);
   SET (ACTN_CELL,  1,  1,  7,  NUM_STR);
   SET (XFRM_CELL,  2,  0, 14,  XFRM_STR);
   SET (XTO_CELL,   2,  1, 14,  XFTO_STR);
   SET (DESC_CELL,  3,  0, 29,  DESC_STR);
   SET (MEMO_CELL,  3,  1, 29,  DESC_STR);
   SET (RECN_CELL,  4,  0,  1,  "R");
   SET (DEBT_CELL,  5,  0, 12,  DEBIT_STR);
   SET (CRED_CELL,  6,  0, 12,  CREDIT_STR);
   SET (BALN_CELL,  7,  0, 12,  BALN_STR);
   SET (PRIC_CELL, -1, -1,  9,  PRICE_STR);
   SET (VALU_CELL, -1, -1, 10,  VALUE_STR);
   SET (SHRS_CELL, -1, -1, 10,  TOT_SHRS_STR);

   switch (type) {
      case BANK_REGISTER:
      case CASH_REGISTER:
      case ASSET_REGISTER:
      case CREDIT_REGISTER:
      case LIABILITY_REGISTER:
      case INCOME_REGISTER:
      case EXPENSE_REGISTER:
      case EQUITY_REGISTER:
         reg->num_cols = 8;
         reg->num_header_rows = 1;
         SET (XTO_CELL,  -1, -1, 14,  XFTO_STR);
         SET (PRIC_CELL, -1, -1,  9,  PRICE_STR);
         SET (VALU_CELL, -1, -1, 10,  VALUE_STR);
         SET (SHRS_CELL, -1, -1, 10,  TOT_SHRS_STR);
         break;

      case STOCK_REGISTER:
         reg->num_cols = 11;
         SET (XTO_CELL,  -1, -1, 14,  XFTO_STR);
         SET (PRIC_CELL,  7,  0,  9,  PRICE_STR);
         SET (VALU_CELL,  8,  0, 10,  VALUE_STR);
         SET (SHRS_CELL,  9,  0, 10,  TOT_SHRS_STR);
         SET (BALN_CELL, 10,  0, 12,  BALN_STR);
         break;

      default:
         break;
   }

   /* setup custom labels for the debit/credit columns */
   switch (type) {
      case BANK_REGISTER:
         reg->labels [DEBT_CELL] = PAYMENT_STR;
         reg->labels [CRED_CELL] = DEPOSIT_STR;
         break;
      case CASH_REGISTER:
         reg->labels [DEBT_CELL] = SPEND_STR;
         reg->labels [CRED_CELL] = RECEIVE_STR;
         break;
      case ASSET_REGISTER:
         reg->labels [DEBT_CELL] = DEPR_STR;
         reg->labels [CRED_CELL] = APPR_STR;
         break;
      case CREDIT_REGISTER:
         reg->labels [DEBT_CELL] = CHARGE_STR;
         reg->labels [CRED_CELL] = PAYMENT_STR;
         break;
      case LIABILITY_REGISTER:
         reg->labels [DEBT_CELL] = INCREASE_STR;
         reg->labels [CRED_CELL] = DECREASE_STR;
         break;
      case INCOME_REGISTER:
         reg->labels [DEBT_CELL] = INCOME_STR;
         reg->labels [CRED_CELL] = CHARGE_STR;
         break;
      case EXPENSE_REGISTER:
         reg->labels [DEBT_CELL] = REBATE_STR;
         reg->labels [CRED_CELL] = EXPENSE_STR;
         break;
      case EQUITY_REGISTER:
         reg->labels [DEBT_CELL] = SURPLUS_STR;
         reg->labels [CRED_CELL] = DEFICIT_STR;
         break;
      case STOCK_REGISTER:
         reg->labels [DEBT_CELL] = SOLD_STR;
         reg->labels [CRED_CELL] = BOUGHT_STR;
         break;
      default:
         break;
   }

}

/* ============================================== */
/* define the traversal order */
/* negative cells mean "traverse out of table" */

static void
configTraverse (BasicRegister *reg)
{
   int type = reg->type;
   CellBlock *curs = reg->cursor;

   switch (type) {
      case BANK_REGISTER:
      case CASH_REGISTER:
      case ASSET_REGISTER:
      case CREDIT_REGISTER:
      case LIABILITY_REGISTER:
      case INCOME_REGISTER:
      case EXPENSE_REGISTER:
      case EQUITY_REGISTER:
         xaccNextRight (curs, DATE_CELL_R, DATE_CELL_C,  NUM_CELL_R,  NUM_CELL_C);
         xaccNextRight (curs,  NUM_CELL_R,  NUM_CELL_C, XFRM_CELL_R, XFRM_CELL_C);
         xaccNextRight (curs, XFRM_CELL_R, XFRM_CELL_C, DESC_CELL_R, DESC_CELL_C);
         xaccNextRight (curs, DESC_CELL_R, DESC_CELL_C, DEBT_CELL_R, DEBT_CELL_C);
         xaccNextRight (curs, DEBT_CELL_R, DEBT_CELL_C, CRED_CELL_R, CRED_CELL_C);
         xaccNextRight (curs, CRED_CELL_R, CRED_CELL_C, ACTN_CELL_R, ACTN_CELL_C);
         xaccNextRight (curs, ACTN_CELL_R, ACTN_CELL_C, MEMO_CELL_R, MEMO_CELL_C);
         xaccNextRight (curs, MEMO_CELL_R, MEMO_CELL_C, -1-DATE_CELL_R, -1-DATE_CELL_C);
         break;

      case STOCK_REGISTER:
         xaccNextRight (curs, DATE_CELL_R, DATE_CELL_C,  NUM_CELL_R,  NUM_CELL_C);
         xaccNextRight (curs,  NUM_CELL_R,  NUM_CELL_C, XFRM_CELL_R, XFRM_CELL_C);
         xaccNextRight (curs, XFRM_CELL_R, XFRM_CELL_C, DESC_CELL_R, DESC_CELL_C);
         xaccNextRight (curs, DESC_CELL_R, DESC_CELL_C, DEBT_CELL_R, DEBT_CELL_C);
         xaccNextRight (curs, DEBT_CELL_R, DEBT_CELL_C, CRED_CELL_R, CRED_CELL_C);
         xaccNextRight (curs, CRED_CELL_R, CRED_CELL_C, PRIC_CELL_R, PRIC_CELL_C);
         xaccNextRight (curs, PRIC_CELL_R, PRIC_CELL_C, ACTN_CELL_R, ACTN_CELL_C);
         xaccNextRight (curs, ACTN_CELL_R, ACTN_CELL_C, MEMO_CELL_R, MEMO_CELL_C);
         xaccNextRight (curs, MEMO_CELL_R, MEMO_CELL_C, -1-DATE_CELL_R, -1-DATE_CELL_C);
         break;

      default:
         xaccNextRight (curs, DATE_CELL_R, DATE_CELL_C, -1-DATE_CELL_R, -1-DATE_CELL_C);

   }
}

/* ============================================== */

BasicRegister * xaccMallocBasicRegister (int type)
{
   BasicRegister * reg;
   reg = (BasicRegister *) malloc (sizeof (BasicRegister));
   xaccInitBasicRegister (reg, type);
   return reg;
}

/* ============================================== */

/* HDR is a utility to set up the header row */
#define HDR(NAME)						\
{								\
   BasicCell *hcell;						\
   hcell = xaccMallocTextCell();				\
   hcell->width = NAME##_CELL_W;				\
   if (1 == reg->num_header_rows) {				\
      xaccAddCell (header, hcell, 0, NAME##_CELL_C);		\
   } else {							\
      xaccAddCell (header, hcell, NAME##_CELL_R, NAME##_CELL_C);	\
   }								\
   xaccSetBasicCellValue (hcell, reg->labels[NAME##_CELL]);	\
}
   
/* BASIC & FANCY macros initialize cells in the register */

#define FANCY(CN,CT,CL) {					\
   reg->CN##Cell = xaccMalloc##CT##Cell();			\
   reg->CN##Cell->cell.width = CL##_CELL_W;			\
   xaccAddCell (curs, &(reg->CN##Cell->cell), CL##_CELL_R, CL##_CELL_C); \
}

#define BASIC(CN,CT,CL) {					\
   reg->CN##Cell = xaccMalloc##CT##Cell();			\
   reg->CN##Cell->width = CL##_CELL_W;				\
   xaccAddCell (curs, reg->CN##Cell, CL##_CELL_R, CL##_CELL_C);	\
}
   
/* ============================================== */

void xaccInitBasicRegister (BasicRegister *reg, int type)
{
   Table * table;
   CellBlock *curs, *header;
   int phys_r, phys_c;

   reg->user_hook = NULL;
   reg->destroy = NULL;
   reg->type = type;

   /* --------------------------- */
   configLayout (reg);

   /* --------------------------- */
   /* define the header */
   header = xaccMallocCellBlock (reg->num_header_rows, reg->num_cols);
   reg->header = header;

   HDR (DATE);
   HDR (NUM);
   HDR (XFRM);
   HDR (DESC);
   HDR (RECN);
   HDR (CRED);
   HDR (DEBT);
   HDR (BALN);
   HDR (PRIC);
   HDR (SHRS);
   HDR (VALU);
   
   /* --------------------------- */
   /* define the ledger cursor */
   curs = xaccMallocCellBlock (2, reg->num_cols);
   reg->cursor = curs;
   
   FANCY (date,    Date,      DATE);
   BASIC (num,     Text,      NUM);
   FANCY (action,  Combo,     ACTN);
   FANCY (xfrm,    Combo,     XFRM);
   FANCY (xto,     Combo,     XTO);
   FANCY (desc,    QuickFill, DESC);
   BASIC (memo,    Text,      MEMO);
   BASIC (recn,    Recn,      RECN);
   FANCY (credit,  Price,     CRED);
   FANCY (debit,   Price,     DEBT);
   FANCY (shrs,    Price,     SHRS);
   FANCY (price,   Price,     PRIC);
   FANCY (value,   Price,     VALU);

   FANCY (balance, Price,     BALN);
   reg->balanceCell->cell.input_output = 0;

   /* -------------------------------- */   
   /* define how traversal works */
   configTraverse (reg);

   /* -------------------------------- */   
   /* add menu items for the action cell */

   xaccAddComboCellMenuItem ( reg->actionCell, ATM_STR);
   xaccAddComboCellMenuItem ( reg->actionCell, TELLER_STR);
   xaccAddComboCellMenuItem ( reg->actionCell, CHECK_STR);
   xaccAddComboCellMenuItem ( reg->actionCell, POS_STR);
   xaccAddComboCellMenuItem ( reg->actionCell, ARU_STR);
   xaccAddComboCellMenuItem ( reg->actionCell, ONLINE_STR);
   xaccAddComboCellMenuItem ( reg->actionCell, ACH_STR);
   xaccAddComboCellMenuItem ( reg->actionCell, WIRE_STR);
   xaccAddComboCellMenuItem ( reg->actionCell, BUY_STR);
   xaccAddComboCellMenuItem ( reg->actionCell, SELL_STR);
   xaccAddComboCellMenuItem ( reg->actionCell, PRICE_STR);
   xaccAddComboCellMenuItem ( reg->actionCell, DIV_STR);
   xaccAddComboCellMenuItem ( reg->actionCell, INT_STR);
   xaccAddComboCellMenuItem ( reg->actionCell, LTCG_STR);
   xaccAddComboCellMenuItem ( reg->actionCell, STCG_STR);
   xaccAddComboCellMenuItem ( reg->actionCell, DIST_STR);
   xaccAddComboCellMenuItem ( reg->actionCell, SPLIT_STR);
   xaccAddComboCellMenuItem ( reg->actionCell, DEPOSIT_STR);
   xaccAddComboCellMenuItem ( reg->actionCell, WITHDRAW_STR);

   /* -------------------------------- */   
   table = xaccMallocTable ();
   phys_r = header->numRows + curs->numRows;
   phys_c = header->numCols;
   xaccSetTableSize (table, phys_r, phys_c, 2, 1);
   xaccSetCursor (table, header, 0, 0, 0, 0);
   xaccSetCursor (table, curs, header->numRows, 0, 1, 0);
   xaccMoveCursor (table, header->numRows, 0);

   reg->table = table;
}

/* ============================================== */

void 
xaccDestroyBasicRegister (BasicRegister *reg)
{
   /* give the user a chance to clean up */
   if (reg->destroy) {
      (*(reg->destroy)) (reg);
   }
   reg->destroy = NULL;
   reg->user_hook = NULL;

   xaccDestroyTable (reg->table);
   reg->table = NULL;

   xaccDestroyCellBlock (reg->header);
   xaccDestroyCellBlock (reg->cursor);
   reg->header = NULL;
   reg->cursor = NULL;

   xaccDestroyDateCell      (reg->dateCell);
   xaccDestroyBasicCell     (reg->numCell);
   xaccDestroyQuickFillCell (reg->descCell);
   xaccDestroyBasicCell     (reg->recnCell);
   xaccDestroyPriceCell     (reg->creditCell);
   xaccDestroyPriceCell     (reg->debitCell);
   xaccDestroyPriceCell     (reg->shrsCell);
   xaccDestroyPriceCell     (reg->priceCell);
   xaccDestroyPriceCell     (reg->valueCell);
   xaccDestroyBasicCell     (reg->memoCell);
   xaccDestroyComboCell     (reg->actionCell);
   xaccDestroyComboCell     (reg->xfrmCell);
   xaccDestroyComboCell     (reg->xtoCell);
   xaccDestroyPriceCell     (reg->balanceCell);

   reg->dateCell    = NULL;
   reg->numCell     = NULL;
   reg->descCell    = NULL;
   reg->recnCell    = NULL;
   reg->creditCell  = NULL;
   reg->debitCell   = NULL;
   reg->shrsCell    = NULL;
   reg->priceCell   = NULL;
   reg->valueCell   = NULL;
   reg->memoCell    = NULL;
   reg->actionCell  = NULL;
   reg->xfrmCell    = NULL;
   reg->xtoCell     = NULL;
   reg->balanceCell = NULL;

   /* free the memory itself */
   free (reg);
}

/* ============================================== */

unsigned int
xaccGetChangeFlag (BasicRegister *reg)
{

   unsigned int changed = 0;

   /* be careful to use bitwise ands and ors to assemble bit flag */
   changed |= MOD_DATE & reg->dateCell->cell.changed;
   changed |= MOD_NUM  & reg->numCell->changed;
   changed |= MOD_DESC & reg->descCell->cell.changed;
   changed |= MOD_RECN & reg->recnCell->changed;
   changed |= MOD_AMNT & reg->creditCell->cell.changed;
   changed |= MOD_AMNT & reg->debitCell->cell.changed;
   changed |= MOD_SHRS & reg->shrsCell->cell.changed; 
   changed |= MOD_PRIC & reg->priceCell->cell.changed;
   changed |= MOD_MEMO & reg->memoCell->changed;
   changed |= MOD_ACTN & reg->actionCell->cell.changed;
   changed |= MOD_XFRM & reg->xfrmCell->cell.changed;
   changed |= MOD_XTO  & reg->xtoCell->cell.changed; 

   return changed;
}

/* ============ END OF FILE ===================== */
