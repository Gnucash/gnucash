/*
 * FILE:
 * splitreg.c
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
#include "recncell.h"
#include "splitreg.h"
#include "table-allgui.h"
#include "textcell.h"

/* utility defines for cell configuration data */
#define DATE_CELL      0
#define NUM_CELL       1
#define DESC_CELL      2
#define RECN_CELL      3   
#define SHRS_CELL      4
#define BALN_CELL      5

#define ACTN_CELL      6
#define XFRM_CELL      7
#define XTO_CELL       8
#define MEMO_CELL      9
#define CRED_CELL      10
#define DEBT_CELL      11
#define PRIC_CELL      12
#define VALU_CELL      13


#define DATE_CELL_WIDTH    11
#define NUM_CELL_WIDTH      7
#define ACTN_CELL_WIDTH     7
#define XFRM_CELL_WIDTH    14
#define XTO_CELL_WIDTH     14
#define DESC_CELL_WIDTH    29
#define MEMO_CELL_WIDTH    29
#define RECN_CELL_WIDTH     1
#define DEBT_CELL_WIDTH    12
#define CRED_CELL_WIDTH    12
#define PRIC_CELL_WIDTH     9
#define VALU_CELL_WIDTH    10
#define SHRS_CELL_WIDTH    10
#define BALN_CELL_WIDTH    12


/* ============================================== */

#define LABEL(NAME,label)					\
{								\
   BasicCell *hcell;						\
   hcell = reg->header_label_cells[NAME##_CELL];		\
   xaccSetBasicCellValue (hcell, label);			\
}
   
/* ============================================== */

static void
configLabels (SplitRegister *reg)
{
   int type = (reg->type) & REG_TYPE_MASK;

   LABEL (DATE,  DATE_STR);
   LABEL (NUM,   NUM_STR);
   LABEL (ACTN,  NUM_STR);
   LABEL (XFRM,  XFRM_STR);
   LABEL (XTO,   XFTO_STR);
   LABEL (DESC,  DESC_STR);
   LABEL (MEMO,  DESC_STR);
   LABEL (RECN,  "R");
   LABEL (DEBT,  DEBIT_STR);
   LABEL (CRED,  CREDIT_STR);

   LABEL (PRIC,  PRICE_STR);
   LABEL (VALU,  VALUE_STR);
   LABEL (SHRS,  TOT_SHRS_STR);
   LABEL (BALN,  BALN_STR);


   /* setup custom labels for the debit/credit columns */
   switch (type) {
      case BANK_REGISTER:
         LABEL (DEBT,  PAYMENT_STR);
         LABEL (CRED,  DEPOSIT_STR);
         break;
      case CASH_REGISTER:
         LABEL (DEBT,  SPEND_STR);
         LABEL (CRED,  RECEIVE_STR);
         break;
      case ASSET_REGISTER:
         LABEL (DEBT,  DEPR_STR);
         LABEL (CRED,  APPR_STR);
         break;
      case CREDIT_REGISTER:
         LABEL (DEBT,  CHARGE_STR);
         LABEL (CRED,  PAYMENT_STR);
         break;
      case LIABILITY_REGISTER:
         LABEL (DEBT,  INCREASE_STR);
         LABEL (CRED,  DECREASE_STR);
         break;
      case INCOME_LEDGER:  
      case INCOME_REGISTER:
         LABEL (DEBT,  INCOME_STR);
         LABEL (CRED,  CHARGE_STR);
         break;
      case EXPENSE_REGISTER:
         LABEL (DEBT,  REBATE_STR);
         LABEL (CRED,  EXPENSE_STR);
         break;
      case GENERAL_LEDGER:  
      case EQUITY_REGISTER:
         LABEL (DEBT,  SURPLUS_STR);
         LABEL (CRED,  DEFICIT_STR);
         break;
      case STOCK_REGISTER:
      case PORTFOLIO:
         LABEL (DEBT,  SOLD_STR);
         LABEL (CRED,  BOUGHT_STR);
         break;
      default:
         break;
   }
}

/* ============================================== */

#define SET(NAME,row,col,handler)				\
{								\
   BasicCell *hcell;						\
   hcell = reg->header_label_cells[NAME##_CELL];		\
								\
   if ((0<=row) && (0<=col)) {					\
      curs->cells [row][col] = (handler);			\
      header->widths[col] = NAME##_CELL_WIDTH;			\
      if (hcell) {						\
         if (1 == reg->num_header_rows) {			\
            header->cells[0][col] = hcell;			\
         } else {						\
            header->cells[row][col] = hcell;			\
         }							\
      }								\
   }								\
}
   
/* BASIC & FANCY macros initialize cells in the register */

#define BASIC(NAME,CN,row,col) {			\
   SET (NAME, row, col, reg->CN##Cell);			\
}

#define FANCY(NAME,CN,row,col) {			\
   SET (NAME, row, col, &(reg->CN##Cell->cell));	\
}

/* ============================================== */

static void
configLayout (SplitRegister *reg)
{
   int type = (reg->type) & REG_TYPE_MASK;
   int style = (reg->type) & REG_STYLE_MASK;

   for (i=0; i<reg->num_cols; i++) {
      curs->cells[0][i] = reg->nullCell;
   }
   switch (style) {
      case REG_SINGLE_LINE:
         /* only the transaction cursor gets used */
         curs = reg->lead_cursor;
         FANCY (DATE,   date,     0,  0);
         BASIC (NUM,    num,      1,  0);
         FANCY (XFRM,   xfrm,     2,  0);
         FANCY (DESC,   desc,     3,  0);
         BASIC (RECN,   recn,     4,  0);
         FANCY (DEBT,   debit,    5,  0);
         FANCY (CRED,   credit,   6,  0);
         FANCY (PRIC,   price,    7,  0);
         FANCY (VALU,   value,    8,  0);
         FANCY (SHRS,   shrs,     9,  0);
         FANCY (BALN,   balance,  10, 0);

         switch (type) {
            case BANK_REGISTER:
            case CASH_REGISTER:
            case ASSET_REGISTER:
            case CREDIT_REGISTER:
            case LIABILITY_REGISTER:
            case INCOME_REGISTER:
            case EXPENSE_REGISTER:
            case EQUITY_REGISTER:
      
            case INCOME_LEDGER:     /* hack alert do xto cell too */
            case GENERAL_LEDGER:    /* hack alert do xto cell too */
               reg->num_cols = 8;
               FANCY (BALN,   balance,  7,  0);
               break;
      
            case STOCK_REGISTER:
            case PORTFOLIO:
               reg->num_cols = 11;
               break;
            default:
               break;
         }
         break;

      /* --------------------------------------------------------- */
      case REG_DOUBLE_LINE:
         /* only the transaction cursor gets used */
         curs = reg->lead_cursor;
         FANCY (DATE,   date,     0,  0);
         BASIC (NUM,    num,      1,  0);
         FANCY (DESC,   desc,     3,  0);
         BASIC (RECN,   recn,     4,  0);
         FANCY (DEBT,   debit,    5,  0);
         FANCY (CRED,   credit,   6,  0);
         FANCY (PRIC,   price,    7,  0);
         FANCY (VALU,   value,    8,  0);
         FANCY (SHRS,   shrs,     9,  0);
         FANCY (BALN,   balance,  10, 0);

         curs = reg->memo_cursor;
         FANCY (ACTN,   action,   1,  0);
         FANCY (XFRM,   xfrm,     2,  0);
         BASIC (MEMO,   memo,     3,  0);

         switch (type) {
            case BANK_REGISTER:
            case CASH_REGISTER:
            case ASSET_REGISTER:
            case CREDIT_REGISTER:
            case LIABILITY_REGISTER:
            case INCOME_REGISTER:
            case EXPENSE_REGISTER:
            case EQUITY_REGISTER:
      
            case INCOME_LEDGER:     /* hack alert do xto cell too */
            case GENERAL_LEDGER:    /* hack alert do xto cell too */
               reg->num_cols = 8;
               curs = reg->lead_cursor;
               FANCY (BALN,   balance,  7,  0);
               break;
      
            case STOCK_REGISTER:
            case PORTFOLIO:
               reg->num_cols = 11;
               break;
            default:
               break;
         }
         break;

      /* --------------------------------------------------------- */
      case REG_MULTI_LINE:
      case REG_SINGLE_DYNAMIC:
      case REG_DOUBLE_DYNAMIC:
         /* only the transaction cursor gets used */
         curs = reg->lead_cursor;
         FANCY (DATE,   date,     0,  0);
         BASIC (NUM,    num,      1,  0);
         FANCY (DESC,   desc,     3,  0);
         BASIC (RECN,   recn,     4,  0);
         FANCY (BALN,   balance,  10, 0);

         curs = reg->split_cursor;
         FANCY (ACTN,   action,   1,  0);
         FANCY (XFRM,   xfrm,     2,  0);
         BASIC (MEMO,   memo,     3,  0);
         FANCY (DEBT,   debit,    5,  0);
         FANCY (CRED,   credit,   6,  0);
         FANCY (PRIC,   price,    7,  0);
         FANCY (VALU,   value,    8,  0);
         FANCY (SHRS,   shrs,     9,  0);

         switch (type) {
            case BANK_REGISTER:
            case CASH_REGISTER:
            case ASSET_REGISTER:
            case CREDIT_REGISTER:
            case LIABILITY_REGISTER:
            case INCOME_REGISTER:
            case EXPENSE_REGISTER:
            case EQUITY_REGISTER:
      
            case INCOME_LEDGER:     /* hack alert do xto cell too */
            case GENERAL_LEDGER:    /* hack alert do xto cell too */
               reg->num_cols = 8;
               curs = reg->lead_cursor;
               FANCY (BALN,   balance,  7,  0);
               break;
      
            case STOCK_REGISTER:
            case PORTFOLIO:
               reg->num_cols = 11;
               break;
            default:
               break;
         }
         break;

      /* --------------------------------------------------------- */
      default:
         printf ("Internal Error!!!!! \n");
   }
}

/* ============================================== */
/* define the traversal order */
/* negative cells mean "traverse out of table" */
/* hack alert -- redesign so that we hop from one row to the next, if desired. */
/* hack alert -- if show_tamount or show_samount is set then don't traverse there */
/* hack alert -- fix show_txfrm also ... */

#define FIRST_RIGHT(r,c) {				\
   prev_r = r; prev_c = c;				\
}

#define NEXT_RIGHT(r,c) {				\
   xaccNextRight (curs, prev_r, prev_c, (r), (c));	\
   prev_r = r; prev_c = c;				\
}

static void
configTraverse (SplitRegister *reg)
{
#ifdef FUTURE 
   int prev_r, prev_c;
   CellBlock *curs = NULL;
   int type = (reg->type) & REG_TYPE_MASK;
   int show_tamount = (reg->type) & REG_SHOW_TAMOUNT;
   int show_samount = (reg->type) & REG_SHOW_SAMOUNT;
   int show_txfrm = (reg->type) & REG_SHOW_TXFRM;
   int double_line = (reg->type) & REG_DOUBLE_LINE;
   int multi_line = (reg->type) & REG_MULTI_LINE;

   switch (type) {
      case BANK_REGISTER:
      case CASH_REGISTER:
      case ASSET_REGISTER:
      case CREDIT_REGISTER:
      case LIABILITY_REGISTER:
      case INCOME_REGISTER:
      case EXPENSE_REGISTER:
      case EQUITY_REGISTER:
      case INCOME_LEDGER:    /* hack alert do xto cell too */
      case GENERAL_LEDGER:    /* hack alert do xto cell too */
         curs = reg->trans_cursor;
         FIRST_RIGHT (DATE_CELL_R, DATE_CELL_C);
         NEXT_RIGHT  (NUM_CELL_R,  NUM_CELL_C);
         if (show_txfrm) {
            NEXT_RIGHT (TXFRM_CELL_R, TXFRM_CELL_C);
         }
         NEXT_RIGHT  (DESC_CELL_R, DESC_CELL_C);
         if (show_tamount) {
            NEXT_RIGHT (TDEBT_CELL_R, TDEBT_CELL_C);
            NEXT_RIGHT (TCRED_CELL_R, TCRED_CELL_C);
         }

         /* if a multi-line display, hop down one line to the split cursor */
         if (!double_line && !multi_line) {
            NEXT_RIGHT (-1-DATE_CELL_R, -1-DATE_CELL_C);
         } else {
            NEXT_RIGHT (ACTN_CELL_R + curs->numRows, ACTN_CELL_C);
         }

         curs = reg->split_cursor;
         FIRST_RIGHT (ACTN_CELL_R, ACTN_CELL_C);
         NEXT_RIGHT (XFRM_CELL_R, XFRM_CELL_C);
         NEXT_RIGHT (MEMO_CELL_R, MEMO_CELL_C);
         if (show_samount) {
            NEXT_RIGHT (DEBT_CELL_R, DEBT_CELL_C);
            NEXT_RIGHT (CRED_CELL_R, CRED_CELL_C);
         }
         if (multi_line) {
            NEXT_RIGHT (ACTN_CELL_R + curs->numRows, ACTN_CELL_C);
         } else
         if (double_line) {
            /* if double-line, hop back one row */
            NEXT_RIGHT (-1-DATE_CELL_R + curs->numRows, -1-DATE_CELL_C);
         } else {
            /* normally, this statement should enver be reached */
            NEXT_RIGHT (-1-ACTN_CELL_R, -1-ACTN_CELL_C);
         }
         break;

      case STOCK_REGISTER:
      case PORTFOLIO:
         curs = reg->trans_cursor;
         FIRST_RIGHT (DATE_CELL_R, DATE_CELL_C);
         NEXT_RIGHT (NUM_CELL_R,  NUM_CELL_C);
         if (show_txfrm) {
            NEXT_RIGHT (TXFRM_CELL_R, TXFRM_CELL_C);
         }
         NEXT_RIGHT (DESC_CELL_R, DESC_CELL_C);
         if (show_tamount) {
            NEXT_RIGHT (TDEBT_CELL_R, TDEBT_CELL_C);
            NEXT_RIGHT (TCRED_CELL_R, TCRED_CELL_C);
            NEXT_RIGHT (TPRIC_CELL_R, TPRIC_CELL_C);
            NEXT_RIGHT (TVALU_CELL_R, TVALU_CELL_C);
         }
         /* if a multi-line display, hop down one line to the split cursor */
         if (!double_line && !multi_line) {
            NEXT_RIGHT (-1-DATE_CELL_R, -1-DATE_CELL_C);
         } else {
            NEXT_RIGHT (ACTN_CELL_R + curs->numRows, ACTN_CELL_C);
         }

         curs = reg->split_cursor;
         FIRST_RIGHT (ACTN_CELL_R, ACTN_CELL_C);
         NEXT_RIGHT (XFRM_CELL_R, XFRM_CELL_C);
         NEXT_RIGHT (MEMO_CELL_R, MEMO_CELL_C);
         if (show_samount) {
            NEXT_RIGHT (DEBT_CELL_R, DEBT_CELL_C);
            NEXT_RIGHT (CRED_CELL_R, CRED_CELL_C);
            NEXT_RIGHT (PRIC_CELL_R, PRIC_CELL_C);
            NEXT_RIGHT (VALU_CELL_R, VALU_CELL_C);
         }
         if (multi_line) {
            NEXT_RIGHT (ACTN_CELL_R + curs->numRows, ACTN_CELL_C);
         } else
         if (double_line) {
            /* if double-line, hop back one row */
            NEXT_RIGHT (-1-DATE_CELL_R + curs->numRows, -1-DATE_CELL_C);
         } else {
            NEXT_RIGHT (-1-ACTN_CELL_R, -1-ACTN_CELL_C);
         }
         break;

      default:
         FIRST_RIGHT (DATE_CELL_R, DATE_CELL_C);
         NEXT_RIGHT  (-1-DATE_CELL_R, -1-DATE_CELL_C);

   }
#endif
}

/* ============================================== */

SplitRegister * xaccMallocSplitRegister (int type)
{
   SplitRegister * reg;
   reg = (SplitRegister *) malloc (sizeof (SplitRegister));
   xaccInitSplitRegister (reg, type);
   return reg;
}

/* ============================================== */

static void
configCursors (SplitRegister *reg)
{
   CellBlock *curs;
   int i;

  /* 
   * The Null Cell is used to make sure that "empty"
   * cells stay empty.  This solves the problem of 
   * having the table be reformatted, the result of
   * which is that an empty cell has landed on a cell
   * that was previously non-empty.  We want to make 
   * sure that we erase those cell contents. The null
   * cells handles this for us.
   */

   reg -> nullCell -> input_output = XACC_CELL_ALLOW_NONE;
   xaccSetBasicCellValue (reg->nullCell, "");

   /* --------------------------- */
   /* set the color of the cells in the cursors */
   /* hack alert -- the actual color should depend on the 
    * type of register. */
   reg->lead_cursor->active_bg_color = 0xffdddd; /* pale red */
   reg->lead_cursor->passive_bg_color = 0xccccff; /* pale blue */

   reg->memo_cursor->active_bg_color = 0xffffdd; /* pale yellow */
   reg->memo_cursor->passive_bg_color = 0xffffff; /* white */

   reg->split_cursor->active_bg_color = 0xffffdd; /* pale yellow */
   reg->split_cursor->passive_bg_color = 0xffffff; /* white */

}

/* ============================================== */

#define HDR(NAME)						\
{								\
   BasicCell *hcell;						\
   hcell = xaccMallocTextCell();				\
   reg->header_label_cells[NAME##_CELL] = hcell;		\
}

#define NEW(CN,TYPE)						\
   reg->CN##Cell = xaccMalloc##TYPE##Cell();			\

void 
xaccInitSplitRegister (SplitRegister *reg, int type)
{
   Table * table;
   CellBlock *header;
   int phys_r, phys_c;

   reg->user_hook = NULL;
   reg->destroy = NULL;
   reg->type = type;

   /* --------------------------- */
   /* define the rows & columns where cells appear */
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
   HDR (PRIC);
   HDR (VALU);
   HDR (SHRS);
   HDR (BALN);
   
   /* --------------------------- */

   NEW (null,    Basic);
   NEW (date,    Date);
   NEW (num,     Text);
   NEW (desc,    QuickFill);
   NEW (recn,    Recn);
   NEW (shrs,    Price);
   NEW (balance, Price);

   NEW (xfrm,    Combo);
   NEW (xto,     Combo);
   NEW (action,  Combo);
   NEW (memo,    Text);
   NEW (credit,  Price);
   NEW (debit,   Price);
   NEW (price,   Price);
   NEW (value,   Price);


   /* the two cursors */
   reg->lead_cursor = xaccMallocCellBlock (1, reg->num_cols);
   reg->memo_cursor = xaccMallocCellBlock (1, reg->num_cols);
   reg->split_cursor = xaccMallocCellBlock (1, reg->num_cols);

   configCursors (reg);

   /* --------------------------- */
   /* do some misc cell config */

   /* balance cell does not accept input; its display only.  */
   /* however, we *do* want it to shadow the true cell contents when 
    * the cursor is repositioned.  Othewise, it will just display 
    * whatever previous bogus value it contained.
    */
   reg->balanceCell->cell.input_output = XACC_CELL_ALLOW_SHADOW;
   reg->shrsCell->cell.input_output = XACC_CELL_ALLOW_SHADOW;

   /* the debit/credit/value cells show blank if value is 0.00 */
   reg->debitCell->blank_zero = 1;
   reg->creditCell->blank_zero = 1;
   reg->valueCell->blank_zero = 1;

   /* ok, now make sure the initail value of 0.0 is blanked.
    * if this is not done, then various oddball situations 
    * will show the non-blanked values. 
    */
   xaccSetPriceCellValue (reg->debitCell, 0.0);
   xaccSetPriceCellValue (reg->creditCell, 0.0);
   xaccSetPriceCellValue (reg->valueCell, 0.0);

   /* use three decimal places to print share-related info.
    * The format is a printf-style format for a double.  */
   xaccSetPriceCellFormat (reg->shrsCell, "%.3f");
   xaccSetPriceCellFormat (reg->priceCell, "%.3f");

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
   phys_r = header->numRows;
   phys_r += reg->trans_cursor->numRows;
   phys_r += reg->split_cursor->numRows;
   phys_c = header->numCols;
   xaccSetTableSize (table, phys_r, phys_c, 3, 1);
   xaccSetCursor (table, header, 0, 0, 0, 0);

   /* hack alert -- document what call does, why we call it here, etc ??? ??? */
   xaccSetCursor (table, reg->trans_cursor, header->numRows, 0, 1, 0);
   xaccMoveCursor (table, header->numRows, 0);

   reg->table = table;
}

/* ============================================== */

void
xaccConfigSplitRegister (SplitRegister *reg, int newtype)
{
   if (!reg) return;

   reg->type = newtype;
   configLayout (reg);
   configTraverse (reg);
   configCursors (reg);
}

/* ============================================== */

void 
xaccDestroySplitRegister (SplitRegister *reg)
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
   xaccDestroyCellBlock (reg->lead_cursor);
   xaccDestroyCellBlock (reg->memo_cursor);
   xaccDestroyCellBlock (reg->split_cursor);
   reg->header = NULL;
   reg->lead_cursor = NULL;
   reg->memo_cursor = NULL;
   reg->split_cursor = NULL;

   xaccDestroyDateCell      (reg->dateCell);
   xaccDestroyBasicCell     (reg->numCell);
   xaccDestroyQuickFillCell (reg->descCell);
   xaccDestroyBasicCell     (reg->recnCell);
   xaccDestroyPriceCell     (reg->shrsCell);
   xaccDestroyPriceCell     (reg->balanceCell);

   xaccDestroyComboCell     (reg->actionCell);
   xaccDestroyComboCell     (reg->xfrmCell);
   xaccDestroyComboCell     (reg->xtoCell);
   xaccDestroyBasicCell     (reg->memoCell);
   xaccDestroyPriceCell     (reg->creditCell);
   xaccDestroyPriceCell     (reg->debitCell);
   xaccDestroyPriceCell     (reg->priceCell);
   xaccDestroyPriceCell     (reg->valueCell);

   reg->dateCell    = NULL;
   reg->numCell     = NULL;
   reg->descCell    = NULL;
   reg->recnCell    = NULL;
   reg->shrsCell    = NULL;
   reg->balanceCell = NULL;

   reg->actionCell  = NULL;
   reg->xfrmCell    = NULL;
   reg->xtoCell     = NULL;
   reg->memoCell    = NULL;
   reg->creditCell  = NULL;
   reg->debitCell   = NULL;
   reg->priceCell   = NULL;
   reg->valueCell   = NULL;

   /* free the memory itself */
   free (reg);
}

/* ============================================== */

unsigned int
xaccSplitRegisterGetChangeFlag (SplitRegister *reg)
{

   unsigned int changed = 0;

   /* be careful to use bitwise ands and ors to assemble bit flag */
   changed |= MOD_DATE & reg->dateCell->cell.changed;
   changed |= MOD_NUM  & reg->numCell->changed;
   changed |= MOD_DESC & reg->descCell->cell.changed;
   changed |= MOD_RECN & reg->recnCell->changed;

   changed |= MOD_ACTN & reg->actionCell->cell.changed;
   changed |= MOD_XFRM & reg->xfrmCell->cell.changed;
   changed |= MOD_XTO  & reg->xtoCell->cell.changed; 
   changed |= MOD_MEMO & reg->memoCell->changed;
   changed |= MOD_AMNT & reg->creditCell->cell.changed;
   changed |= MOD_AMNT & reg->debitCell->cell.changed;
   changed |= MOD_PRIC & reg->priceCell->cell.changed;
   changed |= MOD_VALU & reg->valueCell->cell.changed; 

   return changed;
}

/* ============ END OF FILE ===================== */
